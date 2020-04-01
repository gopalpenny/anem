# anem_imaging.R

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
#' @keywords internal
#' @importFrom magrittr %>%
#' @examples
#'
#' \dontrun{
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
#' }
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
#' @keywords internal
#' @examples
#' \dontrun{
#' wells <- define_wells(x=c(0,0.5),y=c(0,0.25),Q=c(0.5,-0.2),R=100,diam=c(1,1))
#' bounds <- data.frame(m=1,b=1,bound_type="CH",bID=1)
#' mirror_across_bounds(wells,bounds)
#'
#' well1 <- define_wells(x=50,y=50,Q=20,R=100,diam=1)
#' well2 <- define_wells(x=25,y=75,Q=20,R=100,diam=1)
#' wells <- define_wells(rbind(well1,well2))
#' bounds_df <- data.frame(bound_type=c("CH","NF","NF","NF"),m=c(Inf,0,Inf,0),b=c(0,0,100,100))
#' bounds <- define_bounds(bounds_df[bounds_df$m==Inf,])
#' mirror_across_bounds(well1,bounds)
#' mirror_across_bounds(wells,bounds)
#'
#' bounds_df <- data.frame(bound_type=c("PB","NF","PB","NF"),m=c(Inf,0,Inf,0),b=c(0,0,100,100))
#' bounds <- define_bounds(bounds_df[bounds_df$m==Inf,])
#' mirror_across_bounds(well1,bounds)
#' }
mirror_across_bounds <- function(wells,bounds,num_levels=NULL,first_mirror=TRUE) {

  if (first_mirror) { # initialize wells
    wells <- wells %>% dplyr::mutate(orig_wID=wID,transform="none",source_bound="none",path="x",max_mirror_dist=0)
  }
  wells_full <- list(wells)

  bounds <- bounds %>% dplyr::filter(bound_type!="PB")

  # only do mirrors if 1 or more bounds exist (it's possible some are labelled "PB" for pervious boundary)
  if (nrow(bounds) > 0) {

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

    well_cols <- names(wells)

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
#' @return
#' A tibble containing original and mirrored wells, with the following columns:
#' x, y, Q, diam, path, orig_wID, transform (boundary type), source_bound.
#' For image wells that generated, \code{NA} values are used to fill fields
#' for columns that are not generated by this function.
#'
#' If \code{include_image_columns} is \code{TRUE}, the following additional columns
#' are included in the results:
#' \itemize{
#' \item \code{transform}: \code{bound_type} of boundary over which well was imaged
#' \item \code{source_bound}: \code{bID} of boundary over which well was imaged
#' \item \code{path}: complete history of imaging for a given image well, tracking back to the "Actual" well
#' \item \code{max_mirror_dist}: maximum mirror distance for a well and wells in its path (well-to-boundary distance)
#' \item \code{level}: number of images in path. "Actual" wells (level 0), first image (1), second image (2), etc.
#' \item \code{dist}: distance to nearest boundary (i.e., distance to the aquifer). The results are filtered for R > dist
#' }
#' @section Method:
#' The original wells are labelled L0. These wells are mirrored across a set of
#' parallel boundaries. Each mirrored well (L1) has a source well (L0) and source
#' boundary (the bound across which it was mirrored). The L1 wells then need to
#' be mirrored across the other parallel boundary. The wells from level Lx are mirrored
#' across the other boundary (ie, not the source boundary), and each mirror
#' well (L+) has a source well (Lx) and source boundary (the bound across
#' which it was mirrored). This is done for a first set of parallel boundaries,
#' then repeated for the other two perpendicular boundaries.
#' @details
#' The function requires that the wells are labeled with a column of identifiers, wID. If it is not
#' present, the function generates them. The image well wID's are always generated automatically.
#' not present.
#' @importFrom magrittr %>%
#' @export
#' @examples
#' well1 <- define_wells(x=50,y=50,Q=20,R=100,diam=1)
#' well2 <- define_wells(x=25,y=75,Q=20,R=100,diam=1)
#' wells <- define_wells(rbind(well1,well2))
#' bounds <- define_bounds(data.frame(bound_type=c("CH","NF","NF","NF"),
#'   m=c(Inf,0,Inf,0),b=c(0,0,100,100)))
#' aquifer <- define_aquifer("unconfined",Ksat=1e-4,bounds=bounds)
#' image_wells <- generate_image_wells(wells,bounds)
#'
#' bounds <- data.frame(bound_type=c("PB","PB","CH","NF"),m=c(Inf,0,Inf,0),b=c(0,0,100,100))
#' aquifer <- define_aquifer("unconfined",Ksat=1e-4,bounds=bounds)
#' image_wells <- generate_image_wells(well1,aquifer,include_image_columns=TRUE)
#'
#' library(ggplot2)
#' ggplot() +
#'   geom_segment(data=aquifer$bounds,aes(x1,y1,xend=x2,yend=y2)) +
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

  # GROUP IMAGING MIRRORS ALL WELLS AT THE SAME TIME. MIRROR ACROSS BOUNDS DOES MIRRORING RECURSIVELY.
  group_imaging<-FALSE # AVOID COMPLICATIONS OF USING GROUP_IMAGING. POTENTIALLY IMPLEMENT THIS LATER.
  if (group_imaging) {
    image_wells1 <- mirror_well_parallel_bounds(wells,bounds %>% dplyr::filter(bGroup==1))
    image_wells2 <- mirror_well_parallel_bounds(image_wells1,bounds %>% dplyr::filter(bGroup==2),first_mirror=FALSE)
  } else {
    bounds_group_1 <- bounds %>% dplyr::filter(bGroup==1)
    bounds_group_2 <- bounds %>% dplyr::filter(bGroup==2)
    image_wells1 <- mirror_across_bounds(wells,bounds_group_1)
    image_wells2 <- mirror_across_bounds(image_wells1,bounds_group_2,first_mirror=FALSE)
  }


  # remove image wells with R greater than the distance to the boundaries
  image_wells <- image_wells2 %>% dplyr::mutate(dist=get_distance_to_bounds(.,bounds)) %>%
    dplyr::filter(!(dist>R & grepl("Image",well_image)))

  get_distance_to_bounds(image_wells2,bounds)
  loc <- image_wells2
  dim(image_wells2)

  if (!include_image_columns) {
    image_wells <- image_wells %>% dplyr::select(dplyr::one_of(c(wells_column_names,"orig_wID")))
  }

  return(image_wells)
}



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




#' Reconstruct pumping at image wells
#'
#' Reconstruct pumping of actual wells at image wells
#' @param wells A data.frame object of wells, containing well_image column with
#' "Actual", "Image (+Q)", or "Image (-Q)" text.
#' @return A tibble containing wells where Image wells contain pumping rates similar to
#' generate_image_wells.
#' @export
#' @keywords internal
#' @examples
#' wells <- define_wells(wells_example)
#' aquifer <- aquifer_confined_example
#' image_wells <- generate_image_wells(wells,aquifer)
#' image_wells_image_NA <- image_wells
#' image_wells_image_NA$Q[grepl("Image",image_wells_image_NA$well_image)] <- NA
#' image_wells_reconstructed <- reconstruct_image_pumping(image_wells_image_NA)
#' identical(image_wells,image_wells_reconstructed)
reconstruct_image_pumping <- function(image_wells) {
  image_wells_reconstructed <- image_wells %>%
    dplyr::mutate(Q=dplyr::if_else(grepl("Image",well_image),as.numeric(NA),Q)) %>%
    dplyr::arrange(orig_wID,wID,well_image) %>%
    tidyr::fill(Q) %>% dplyr::mutate(Q=Q*get_pumping_sign(well_image)) %>%
    dplyr::arrange(wID)
  return(image_wells_reconstructed)
}


#' Get pumping sign
#'
#' @keywords internal
#' @examples
#' \dontrun{
#' get_pumping_sign("Actual")
#' get_pumping_sign("Image (+Q)")
#' get_pumping_sign("Image (-Q)")
#' }
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
#' @keywords internal
#' @examples
#' \dontrun{
#' set_pumping_sign("Actual","NF")
#' set_pumping_sign("Actual","CH")
#' set_pumping_sign("Image (+Q)","NF")
#' set_pumping_sign("Image (+Q)","CH")
#' set_pumping_sign("Image (-Q)","NF")
#' set_pumping_sign("Image (-Q)","CH")
#' }
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

#' Get pumping sign
#'
#' Get sign of pumping of image well relative to pumping of Actual well
#' (associated by orig_wID). Sign assumes the point is mirrored across parallel
#' boundaries of constant head and/or no-flow.
#' @param N Vector of levels, N
#' @param well_image Character vector of well_image ("Actual", "Image (+Q)", or "Image (-Q)")
#' @param bound_type1 Character vector of bound_type for first mirror boundary ("CH" or "NF")
#' @param bound_type2 Character vector of bound_type for second mirror boundary ("CH" or "NF")
#' @keywords internal
#' @examples
#'
#' \dontrun{
#' gen_well_image_type(N=rep(1:2,2),well_image=rep("Actual",4),
#'   bound_type1=c("NF","NF","CH","CH"),bound_type2=c("CH","CH","NF","NF"))
#' well_image <- "Actual"
#' bound_type1 <- "NF"
#' bound_type2 <- "NF"
#' gen_well_image_type(N,well_image,bound_type1,bound_type2)
#' gen_well_image_type(rep(1,4),"Actual","NF","NF")
#' gen_well_image_type(1:8,"Actual","NF","NF")
#' gen_well_image_type(1:8,"Actual","CH","NF")
#' gen_well_image_type(1:8,"Actual","NF","CH")
#' gen_well_image_type(1:8,"Image (+Q)","NF","CH")
#' gen_well_image_type(1:8,"Image (-Q)","NF","CH")
#' gen_well_image_type(1:8,"Actual","CH","CH")
#' }
gen_well_image_type <- function(N,well_image,bound_type1,bound_type2,type="text") {
  Q_sign <- rep(1,length(N))

  # bound_type1 <- "NF"
  # bound_type2 <- "CH"
  # adjust sign for first boundary
  Q_sign <- dplyr::case_when(
    bound_type1=="CH" ~ Q_sign * (-sign(((floor((N-1)/2)+1) %% 2)-0.5)), # change_sign = (sign(((N+1) %% 2)-0.5)),
    bound_type1=="NF" ~ Q_sign,
    TRUE~as.numeric(NA)
  )
  # adjust sign for second boundary
  Q_sign <- dplyr::case_when(
    bound_type2=="CH"~ Q_sign * (sign(((floor((N)/2)+1) %% 2)-0.5)), # change_sign = (sign(((N) %% 2)-0.5)),
    bound_type2=="NF"~ Q_sign,
    TRUE~as.numeric(NA)
  )

  # Q_sign <- cumprod(change_sign)
  prev_sign <- dplyr::case_when(
    grepl("Actual|\\+Q",well_image)~1,
    grepl("\\-Q",well_image)~-1,
    TRUE~Inf)
  new_sign <- prev_sign * Q_sign
  text_sign <- dplyr::case_when(
    new_sign==1~"Image (+Q)",
    new_sign==-1~"Image (-Q)",
    TRUE~"Inf"
  )
  return(list(sign=new_sign,well_image=text_sign))
}
