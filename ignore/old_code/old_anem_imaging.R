# anem_imaging.R




#' Images wells across parallel boundaries
#'
#' @param wells Wells with columns Q, R, diam, x, y
#' @param bounds Bounds with columns bID, bound_type, m, b
#' @param num_levels (optional) Maximum number of iterations to image wells (and
#'   well images). Greater than or equal to 1. If not specified, then the number
#'   of levels is automatically estimated.
#' @param first_mirror If TRUE, create columns (path, orig_wID, transform,
#'   source_bound,max_miror_dist) for all wells. If FALSE, do not generate these
#'   columns
#' @return A data.frame containing original and mirrored wells, with the
#'   following columns: x, y, Q, diam, path, orig_wID, transform (boundary type),
#'   source_bound. Columns in wells that are not reproduced by this function are
#'   filled with \code{NA}.
#' @section Method: The original wells are labelled L0. These wells are mirrored
#'   across the boundaries, and each mirrored well (L1) has a source well (L0)
#'   and source boundary (the bound across which it was mirrored) The L1 wells
#'   then need to be mirrored across all boundaries except for the source
#'   boundary (which would replicate the actual well). The wells from level Lx
#'   are mirrored across the boundaries (except for the source boundary), and
#'   each mirror well (L+) has a source well (Lx) and source boundary (the bound
#'   across which it was mirrored).
#' @section Notes: The function requires that the wells are labeled with a
#'   column of identifiers, wID. If it is not present, the function generates
#'   them. The image well wID's are always generated automatically. not present.
#' @importFrom magrittr %>%
#' @keywords internal
#' @examples
#'
#' \dontrun{
#' wells <- define_wells(x=c(0,0.5),y=c(0,0.25),Q=c(0.5,-0.2),R=100,diam=c(1,1))
#' bounds <- data.frame(m=1,b=1,bound_type="NF",bID=1)
#' images <- mirror_well_parallel_bounds(wells,bounds)
#'
#' library(ggplot2)
#' ggplot() +
#'   geom_abline(data=bounds,aes(slope=m,intercept=b)) +
#'   geom_point(data=images,aes(x,y,color=well_image)) +
#'   coord_equal()
#'
#' well1 <- define_wells(x=50,y=50,Q=20,R=100,diam=1)
#' well2 <- define_wells(x=25,y=75,Q=20,R=100,diam=1)
#' wells <- define_wells(rbind(well1,well2))
#' bounds_df <- data.frame(bound_type=c("CH","NF","NF","NF"),m=c(Inf,0,Inf,0),b=c(0,0,100,100))
#' bounds <- define_bounds(bounds_df[bounds_df$m==Inf,])
#' mirror_well_parallel_bounds(well1,bounds)
#' images <- mirror_well_parallel_bounds(wells,bounds)
#' ggplot(images) +
#'   geom_segment(data=bounds,aes(x1,y1,xend=x2,yend=y2,color=bound_type)) +
#'   geom_point(aes(x,y,fill=Q,shape=well_image)) +
#'   scale_shape_manual(values=21:23) +
#'   coord_equal()
#' }
mirror_well_parallel_bounds <- function(wells,bounds,num_levels=NULL,first_mirror=TRUE) {

  is_sf <- max(grepl("sf",class(wells)))
  if (is_sf) {
    well_crs <- sf::st_crs(well)
    wells <- wells %>% sf::st_set_geometry(NULL)
  }

  # make sure boundaries are parallel
  if (length(unique(round(bounds$m,10)))>1) {
    stop("mirror_across_bounds can only mirror across parallel boundaries.")
  }

  num_levels <- NULL
  # estimate number of levels
  if (!is.null(num_levels)) {
    num_levels <- num_levels
  } else if (length(bounds$m)==1) {
    # if there is 1 boundary, we only need 1 image
    num_levels <- 1
    dist <- 0
  } else if (length(bounds$m)==2) {
    # if there are 2 boundaries, calculate the numer of image levels
    # check: make sure wells contain a radius of influence
    wCheck <- check_wells(wells,c("R","x","y"))
    max_radius <- max(wells$R)
    # get distance between boundaries
    if (abs(bounds$m[1]==Inf)) {
      dist <- abs(bounds$b[2] - bounds$b[1])
    } else if (bounds$m[1]==0) {
      dist <- abs(bounds$b[2] - bounds$b[1])
    } else { # find distance from x1=0, b1 to point on other line given by
      m <- bounds$m[1]
      b1 <- bounds$b[1]
      b2 <- bounds$b[2]
      x_i <- m * (b2-b1) / (m^2 + 1)
      y_i <- -1/m * x_i + b2
      dist <- sqrt((x_i-0)^2 + (y_i-b2)^2)
    }
    num_levels <- floor(max_radius / dist + 1)
  }

  if (num_levels>10) {
    message("Generage well images with ",num_levels," levels.")
  }

  # prepare bounds
  bounds_prep <- bounds %>%
    dplyr::bind_cols(bound_type_other=bounds[dim(bounds)[1]:1,]$bound_type)

  # prepare wells
  if (first_mirror) {
    wells <- wells %>%
      dplyr::mutate(orig_wID=dplyr::if_else(grepl("Actual",well_image),wID,as.integer(NA)))
  }
  wells_prep <- wells %>%
    tidyr::crossing(bounds_prep %>%
                      dplyr::select(m,b,tidyselect::starts_with("bound_type"))) %>%
    dplyr::bind_cols(get_nearest_point_on_line(.,.$m,.$b) %>%
                       dplyr::rename(xB=x,yB=y)) %>%
    dplyr::select(-m,-b) %>%
    tidyr::crossing(N=1:num_levels) %>%
    dplyr::mutate(x_diff=xB-x,
                  y_diff=yB-y,
                  D=floor(N/2)*2,
                  odd=N%%2)

  wells_new <- wells_prep %>%
    dplyr::mutate(orig_wID=pmin(wID,orig_wID),
                  x_new = x + sign(odd-0.5)*D*dist*sign(x_diff) + 2 * odd * x_diff,
                  y_new = y + sign(odd-0.5)*D*dist*sign(y_diff) + 2 * odd * y_diff,
                  wID=as.integer(dplyr::row_number()+max(wells$wID)),
                  well_image = gen_well_image_type(N,well_image,bound_type,bound_type_other)$well_image,
                  Q=NA)  %>%
    dplyr::select(wID,Q,R,diam,x=x_new,y=y_new,well_type,well_image,orig_wID)
  image_wells <- wells %>%
    dplyr::bind_rows(wells_new) %>%
    reconstruct_image_pumping()





  # if (!max(grepl("^wID$",names(wells)))) { # generate wID's if they are not present
  #   wells <- wells %>% dplyr::mutate(wID=dplyr::row_number())
  # }
  # if (!tibble::has_name(wells,"R")) {
  #   wells <- wells %>% dplyr::mutate(R= NA)
  # }

  # if (first_mirror) { # initialize wells
  #   wells <- wells %>% dplyr::mutate(orig_wID=wID,transform="none",source_bound="none",path="x",max_mirror_dist=0)
  # }

  if (is_sf) {
    image_wells <- image_wells %>% dplyr::mutate(X=x,Y=y) %>%
      sf::st_as_sf(coords=c("X","Y")) %>% sf::st_set_crs(sf::st_crs(well_crs))
  }
  return(image_wells)
}
