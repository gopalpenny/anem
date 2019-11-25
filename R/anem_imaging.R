# anema_imaging.R

#' Mirror a well across a boundary
#'
#' Mirror a well across a boundary, specified as constant head (CH) or no flow
#' (NF) and defined by the line \eqn{m x + b})
#'
#' @param well single well as a list (or sf object single well feature)
#'   containing coordinates (well$x, well$y), pumping rate (well$Q), and
#'   well_image ("Actual" or "Image (+Q)" or "Image (-Q)").
#'   which is positive for injection, negative for pumping. Should also have:
#'   diam, path, orig_wID.
#' @param boundary single line as a list (or sf object single line feature)
#'   containing slope (as boundary$m) and intercept (as boundary$b), as well as
#'   the boundary type (bound_type) as "CH" for constant head or "NF" for no flow
#' @param new_wID integer ID for the new well to be created
#' @return a row (same class as well input) containing the new imaged well and
#'   the following columns: x, y, Q, diam, path, orig_wID, transform
#'   (boundary type),
#'   source_bound
#' @importFrom magrittr %>%
#' @examples
#' well <- define_wells(wID=1,x=0,y=0,path=1,orig_wID=1)
#' boundary <- data.frame(bID=1,m=-1,b=1,bound_type="NF")
#' get_mirror_point(well, boundary, new_wID=2)
#'
#' well <- define_wells(wID=1,x=0,y=0,Q=0.5,diam=0.75,path=1,orig_wID=1,well_group="a")
#' boundary <- data.frame(bID=1,m=-1,b=1,bound_type="NF")
#' get_mirror_point(well, boundary, new_wID=2)
#'
#' well <- define_wells(wID=1,x=2,y=2,Q=0.5,diam=0.75,path=1,orig_wID=1,well_group="a")
#' boundary <- data.frame(bID=1,m=Inf,b=1,bound_type="NF")
#' get_mirror_point(well, boundary, new_wID=2) %>% select(x,y)
#'
#'
#' well <- define_wells(wID=1,x=2,y=2,Q=0.5,diam=0.75,path=1,orig_wID=1,well_group="a")
#' boundary <- data.frame(bID=1,m=0,b=1,bound_type="NF")
#' get_mirror_point(well, boundary, new_wID=2) %>% select(x,y)
get_mirror_point <- function(well, boundary, new_wID=NA) {
  check_wells(well,c("x","y","well_image"))
  is_sf <- max(grepl("sf",class(well)))
  if (is_sf) {
    well_crs <- sf::st_crs(well)
    well <- well %>% sf::st_set_geometry(NULL)
  }

  if (!tibble::has_name(well,"max_mirror_dist")) {
    well <- well %>% dplyr::mutate(max_mirror_dist= 0)
  }


  m <- boundary$m
  b <- boundary$b

  # get nearest point on boundary (use line perpendicular to slope of boundary)
  if (abs(m==Inf)) {
    xi <- b
    yi <- well$y
  } else if (m==0) {
    xi <- well$x
    yi <- b
  } else {
    bi <- with(well,y + 1/m * x) # intercept of perpendicular line
    xi <- with(well,m*(bi-b)/(m^2+1)) # x intersection
    yi <- -1/m * xi + bi # y intersection
  }

  # get new coordinates
  x1 <- with(well,2*xi - x)
  y1 <- with(well,2*yi - y)

  max_mirror_dist <- pmax(with(well,sqrt((xi-x)^2+(yi-y)^2)),well$max_mirror_dist)

  # set well_image field
  next_well_image <- set_pumping_sign(well$well_image,boundary$bound_type)$well_image

  # get pumping rate (depending on NF or CH)
  Q <- dplyr::case_when(
    boundary$bound_type=="NF"~well$Q,
    boundary$bound_type=="CH"~-well$Q,
    TRUE~Inf)


  # set path
  path <- paste(well$path," : ",well$wID," (",boundary$bID,"-",boundary$bound_type,")",sep="")

  # return well
  pt_df_props <- tibble::tibble(wID=new_wID,Q=Q,x=x1,y=y1,well_image=next_well_image,
                                orig_wID=well$orig_wID,transform=boundary$bound_type,source_bound=boundary$bID,path=path,
                                max_mirror_dist=max_mirror_dist)
  pt_df <- well[!(names(well) %in% names(pt_df_props))] %>% dplyr::bind_cols(pt_df_props)

  if (is_sf) {
    pt_df <- pt_df %>% dplyr::mutate(X=x,Y=y) %>%
      sf::st_as_sf(coords=c("X","Y")) %>% sf::st_set_crs(sf::st_crs(well_crs))
  }
  return(pt_df)
}

#' Get pumping sign
#'
#' @examples
#' get_pumping_sign("Actual")
#' get_pumping_sign("Image (+Q)")
#' get_pumping_sign("Image (-Q)")
get_pumping_sign <- function(well_image) {
  sign <- dplyr::case_when(
    grepl("Actual|\\+Q",well_image)~1,
    grepl("\\-Q",well_image)~-1,
    TRUE~Inf)
  return(sign)
}


#' Get pumping sign
#'
#' Get sign of pumping relative to pumping of Actual well (associated by orig_wID)
#' @examples
#' set_pumping_sign("Actual","NF")
#' set_pumping_sign("Actual","CH")
#' set_pumping_sign("Image (+Q)","NF")
#' set_pumping_sign("Image (+Q)","CH")
#' set_pumping_sign("Image (-Q)","NF")
#' set_pumping_sign("Image (-Q)","CH")
set_pumping_sign <- function(well_image,bound_type,type="text") {
  prev_sign <- dplyr::case_when(
    grepl("Actual|\\+Q",well_image)~1,
    grepl("\\-Q",well_image)~-1,
    TRUE~Inf)
  change_sign <- dplyr::case_when(
    bound_type=="NF"~1,
    bound_type=="CH"~-1,
    TRUE~Inf)
  new_sign <- prev_sign * change_sign
  text_sign <- dplyr::case_when(
    new_sign==1~"Image (+Q)",
    new_sign==-1~"Image (-Q)",
    TRUE~"Inf"
  )
  return(list(sign=new_sign,well_image=text_sign))
}


#' Recursively mirror wells across boundaries
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
#' @examples
#' wells <- define_wells(x=c(0,0.5),y=c(0,0.25),Q=c(0.5,-0.2),R=100,diam=c(1,1))
#' bounds <- data.frame(m=1,b=1,bound_type="CH",bID=1)
#' mirror_across_bounds(wells,bounds)
#'
#' well1 <- define_wells(x=50,y=50,Q=20,R=100,diam=1)
#' well2 <- define_wells(x=25,y=75,Q=20,R=100,diam=1)
#' wells <- define_wells(bind_rows(well1,well2))
#' bounds_df <- data.frame(bound_type=c("CH","NF","NF","NF"),m=c(Inf,0,Inf,0),b=c(0,0,100,100))
#' bounds <- define_bounds(bounds_df) %>% filter(m==Inf)
#' mirror_across_bounds(well1,bounds)
#' mirror_across_bounds(wells,bounds)
mirror_across_bounds <- function(wells,bounds,num_levels=NULL,first_mirror=TRUE) {
  # make sure boundaries are parallel
  if (length(unique(round(bounds$m,10)))>1) {
    stop("mirror_across_bounds can only mirror across parallel boundaries.")
  }


  # estimate number of levels
  if (!is.null(num_levels)) {
    num_levels <- num_levels
  } else if (length(bounds$m)==1) {
    # if there is 1 boundary, we only need 1 image
    num_levels <- 1
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

  if (!max(grepl("^wID$",names(wells)))) { # generate wID's if they are not present
    wells <- wells %>% dplyr::mutate(wID=dplyr::row_number())
  }
  if (!tibble::has_name(wells,"R")) {
    wells <- wells %>% dplyr::mutate(R= NA)
  }

  if (first_mirror) { # initialize wells
    wells <- wells %>% dplyr::mutate(orig_wID=wID,transform="none",source_bound="none",path="x",max_mirror_dist=0)
  }
  well_cols <- names(wells)

  wells_full <- list(wells)
  for (i in 1:num_levels) { # for each level, generate mirrored points in next level
    new_wells <- wells_full[[i]] %>% dplyr::filter(FALSE)
    for (j in 1:dim(wells_full[[i]])[1]) { # for each well in level i
      well <- wells_full[[i]][j,] # get well in level i

      for (k in 1:dim(bounds)[1]) { # for each boundary
        boundary <- bounds[k,] # get boundary
        if (well$source_bound!=boundary$bID) { # only take mirror for boundaries that are not the source boundary
          new_wID <- max(c(wells_full[[i]]$wID,new_wells$wID)) + 1 # get new well id
          new_point <- get_mirror_point(well,boundary,new_wID) # get new well
          new_wells <- new_wells %>% rbind(new_point) # add well to new_wells
        }
      }
    }
    new_wells[,well_cols[!(well_cols %in% names(new_wells))]] <- NA
    wells_full[[i+1]] <- new_wells
  }

  wells <- do.call(rbind,wells_full) %>%
    dplyr::mutate(level=as.integer(stringr::str_count(path,":")))
  return(wells)
}


#' Generate image wells across rectangular boundaries
#'
#' Mirror wells across 2 parallel bounds, then the two perpendicular bounds
#'
#' @param aquifer Aquifer object with bounds defined including columns for bID, bound_type, m, b
#' @param include_image_columns If FALSE, no new columns are added. If TRUE, columns for well
#'   imaging are included in the result.
#' @inheritParams mirror_across_bounds
#' @return A tibble containing original and mirrored wells, with the following columns:
#'   x, y, Q, diam, path, orig_wID, transform (boundary type), source_bound.
#'   Columns in wells that are not reproduced by this function are filled with \code{NA}.
#' @section Method:
#' The original wells are labelled L0. These wells are mirrored across a set of
#' parallel boundaries. Each mirrored well (L1) has a source well (L0) and source
#' boundary (the bound across which it was mirrored). The L1 wells then need to
#' be mirrored across the other parallel boundary. The wells from level Lx are mirrored
#' across the other boundary (ie, not the source boundary), and each mirror
#' well (L+) has a source well (Lx) and source boundary (the bound across
#' which it was mirrored). This is done for a first set of parallel boundaries,
#' then repeated for the other two perpendicular boundaries.
#' @section Notes:
#' The function requires that the wells are labeled with a column of identifiers, wID. If it is not
#' present, the function generates them. The image well wID's are always generated automatically.
#' not present.
#' @importFrom magrittr %>%
#' @export
#' @examples
#' well1 <- define_wells(x=50,y=50,Q=20,R=100,diam=1)
#' well2 <- define_wells(x=25,y=75,Q=20,R=100,diam=1)
#' wells <- define_wells(bind_rows(well1,well2))
#' bounds <- data.frame(bound_type=c("CH","NF","NF","NF"),m=c(Inf,0,Inf,0),b=c(0,0,100,100)) %>% define_bounds()
#' aquifer <- define_aquifer("unconfined",Ksat=1e-4,bounds=bounds)
#' image_wells <- generate_image_wells(wells,bounds)
#'
#' ggplot() +
#'   geom_segment(data=bounds,aes(x1,y1,xend=x2,yend=y2)) +
#'   geom_point(data=image_wells,aes(x,y,color=as.factor(orig_wID))) +
#'   scale_shape_manual(values=c(1,16)) +
#'   coord_equal()
generate_image_wells <- function(wells,aquifer,include_image_columns=FALSE) {
  if (max(grepl("aquifer",class(aquifer)))) {
    bounds <- aquifer$bounds
  } else {
    bounds <- aquifer
  }
  bcheck <- check_bounds(bounds)
  wells_column_names <- names(wells)

  # check for no images
  if (max(unique(wells$well_image)=="Image")==1) {
    stop("Stopping generate_image_wells. Won't generate image wells for wells already labeled as wells$well_image=\"Image\".")
  }

  bounds <- bounds %>% dplyr::mutate(bGroup=as.integer(as.factor(round(m,10))))

  if (!max(grepl("^wID$",names(wells)))) { # generate wID's if they are not present
    wells <- wells %>% dplyr::mutate(wID=dplyr::row_number())
  }

  image_wells1 <- mirror_across_bounds(wells,bounds %>% dplyr::filter(bGroup==1))
  image_wells2 <- mirror_across_bounds(image_wells1,bounds %>% dplyr::filter(bGroup==2),first_mirror=FALSE)

  # remove image wells with R greater than the distance to the boundaries
  image_wells <- image_wells2 %>% dplyr::mutate(dist=get_distance_to_bounds(.,bounds)) %>%
    dplyr::filter(!(dist>R & grepl("Image",well_image)))

  if (!include_image_columns) {
    image_wells <- image_wells %>% dplyr::select(dplyr::one_of(c(wells_column_names,"orig_wID")))
  }

  return(image_wells)
}

#' Reproduce pumping at image wells
#'
#' Reproduce pumping of actual wells at image wells
#' @param wells A data.frame object of wells, containing well_image column with
#' "Actual", "Image (+Q)", or "Image (-Q)" text.
#' @return A tibble containing wells where Image wells contain pumping rates similar to
#' generate_image_wells.
#' @examples
#' well1 <- define_wells(x=50,y=50,Q=5,R=100,diam=1)
#' well2 <- define_wells(x=25,y=75,Q=-2,R=100,diam=1)
#' wells <- define_wells(bind_rows(well1,well2))
#' bounds <- data.frame(bound_type=c("CH","NF","NF","NF"),m=c(Inf,0,Inf,0),b=c(0,0,100,100)) %>% define_bounds()
#' aquifer <- define_aquifer("unconfined",Ksat=1e-4,bounds=bounds)
#' image_wells <- generate_image_wells(wells,bounds)
#' image_wells_image_NA <- image_wells %>%
#'   dplyr::mutate(Q=dplyr::case_when(grepl("Image",well_image)~as.numeric(NA),TRUE~Q))
#' image_wells_reconstructed <- reconstruct_image_pumping(image_wells_image_NA)
#' identical(image_wells,image_wells_reconstructed)
reconstruct_image_pumping <- function(image_wells) {
  image_wells_reconstructed <- image_wells %>% dplyr::arrange(orig_wID,well_image) %>%
    tidyr::fill(Q) %>% dplyr::mutate(Q=Q*get_pumping_sign(well_image)) %>%
    dplyr::arrange(wID)
  return(image_wells_reconstructed)
}
