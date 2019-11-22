# anema_imaging.R

#' Mirror a well across a boundary
#'
#' Mirror a well across a boundary, specified as constant head (CH) or no flow
#' (NF) and defined by the line \eqn{m x + b})
#'
#' @param well single well as a list (or sf object single well feature)
#'   containing coordinates (well$x, well$y), and pumping rate (well$Q),
#'   which is positive for injection, negative for pumping. Should also have:
#'   diam, path, origin.
#' @param boundary single line as a list (or sf object single line feature)
#'   containing slope (as boundary$m) and intercept (as boundary$b), as well as
#'   the boundary type (bound_type) as "CH" for constant head or "NF" for no flow
#' @param new_wID integer ID for the new well to be created
#' @return a row (same class as well input) containing the new imaged well and
#'   the following columns: x, y, Q, diam, path, origin, transform
#'   (boundary type),
#'   source_bound
#' @importFrom magrittr %>%
#' @examples
#' well <- define_wells(wID=1,x=0,y=0,Q=0.5,diam=0.75,path=1,origin=1,well_group="a")
#' boundary <- data.frame(bID=1,m=-1,b=1,bound_type="NF")
#' get_mirror_point(well, boundary, new_wID=2)
#'
#' well <- define_wells(wID=1,x=2,y=2,Q=0.5,diam=0.75,path=1,origin=1,well_group="a")
#' boundary <- data.frame(bID=1,m=Inf,b=1,bound_type="NF")
#' get_mirror_point(well, boundary, new_wID=2) %>% select(x,y)
#'
#'
#' well <- define_wells(wID=1,x=2,y=2,Q=0.5,diam=0.75,path=1,origin=1,well_group="a")
#' boundary <- data.frame(bID=1,m=0,b=1,bound_type="NF")
#' get_mirror_point(well, boundary, new_wID=2) %>% select(x,y)
get_mirror_point <- function(well, boundary, new_wID=NA) {
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

  # get pumping rate (depending on NF or CH)
  Q <- dplyr::case_when(
    boundary$bound_type=="NF"~well$Q,
    boundary$bound_type=="CH"~-well$Q,
    TRUE~Inf)

  # set path
  path <- paste(well$path," : ",well$wID," (",boundary$bID,"-",boundary$bound_type,")",sep="")

  # return well
  pt_df_props <- tibble::tibble(wID=new_wID,Q=Q,x=x1,y=y1,
                                origin=well$origin,transform=boundary$bound_type,source_bound=boundary$bID,path=path,
                                max_mirror_dist=max_mirror_dist,well_image="Image")
  pt_df <- well[!(names(well) %in% names(pt_df_props))] %>% dplyr::bind_cols(pt_df_props)

  if (is_sf) {
    pt_df <- pt_df %>% dplyr::mutate(X=x,Y=y) %>%
      sf::st_as_sf(coords=c("X","Y")) %>% sf::st_set_crs(sf::st_crs(well_crs))
  }
  return(pt_df)
}

#' Recursively mirror wells across boundaries
#'
#' @param wells Wells with columns Q, diam, x, y
#' @param bounds Bounds with columns bID, bound_type, m, b
#' @param num_levels Maximum number of iterations to image wells (and well
#'   images). Greater than or equal to 1.
#' @param first_mirror If TRUE, create columns (path, origin, transform,
#'   source_bound,max_miror_dist) for all wells. If FALSE, do not generate these
#'   columns
#' @return A data.frame containing original and mirrored wells, with the
#'   following columns: x, y, Q, diam, path, origin, transform (boundary type),
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
#' wells <- define_wells(x=c(0,0.5),y=c(0,0.25),Q=c(0.5,-0.2),R=100,diam=c(0.75,0.8))
#' bounds <- data.frame(m=c(1,-1),b=c(0.5,1),bound_type=c("CH","NF"),bID=c(1,2))
#' mirror_across_bounds(wells,bounds,num_levels=1)
#' mirror_across_bounds(wells,bounds,num_levels=2)
#'
#' bounds_df <- data.frame(bound_type=c("CH","NF","NF","NF"),m=c(Inf,0,Inf,0),b=c(-1,-1,2,2))
#' bounds <- define_bounds(bounds_df)
mirror_across_bounds <- function(wells,bounds,first_mirror=TRUE) {
  # make sure boundaries are parallel
  if (length(unique(round(bounds$m,10)))>1) {
    stop("mirror_across_bounds can only mirror across parallel boundaries.")
  }


  # estimate number of levels
  if (length(bounds$m)==1) {
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
    wells <- wells %>% dplyr::mutate(origin=wID,transform="none",source_bound="none",path="x",max_mirror_dist=0)
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
    dplyr::mutate(level=as.factor(stringr::str_count(path,":")))
  return(wells)
}


#' Generate image wells across rectangular boundaries
#'
#' Mirror wells across 2 parallel bounds, then the two perpendicular bounds
#'
#' @param aquifer Aquifer object with bounds defined including columns for bID, bound_type, m, b
#' @inheritParams mirror_across_bounds
#' @return A tibble containing original and mirrored wells, with the following columns:
#'   x, y, Q, diam, path, origin, transform (boundary type), source_bound.
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
#' wells <- define_wells(x=c(5,0.5),y=c(2.5,0.25),Q=c(0.5,-0.2),diam=c(0.75,0.8))
#' bounds_df <- tibble(bound_type=c("CH","NF","NF","NF"),
#'                     m=c(0.8,-1.25,0.8,-1.25),b=c(0.3,10,-2.5,0.1),
#'                     bID=as.numeric(1:4))
#' bounds <- define_bounds(bounds_df)
#' image_wells <- generate_image_wells(wells,bounds)
#' ggplot() +
#'   geom_segment(data=bounds,aes(x1,y1,xend=x2,yend=y2)) +
#'   geom_point(data=image_wells,aes(x,y,color=as.factor(origin))) + #ylim(c(-3,5)) + xlim(c(-1,7)) +
#'   scale_shape_manual(values=c(1,16)) +
#'   coord_equal()
generate_image_wells <- function(wells,aquifer) {
  if (max(grepl("aquifer",class(aquifer)))) {
    bounds <- aquifer$bounds
  } else {
    bounds <- aquifer
  }
  bcheck <- check_bounds(bounds)

  bounds <- bounds %>% dplyr::mutate(bGroup=as.integer(as.factor(round(m,10))))

  if (!max(grepl("^wID$",names(wells)))) { # generate wID's if they are not present
    wells <- wells %>% dplyr::mutate(wID=dplyr::row_number())
  }

  image_wells1 <- mirror_across_bounds(wells,bounds %>% dplyr::filter(bGroup==1))
  image_wells <- mirror_across_bounds(image_wells1,bounds %>% dplyr::filter(bGroup==2),first_mirror=FALSE)
  image_wells <- define_wells(image_wells)
  return(image_wells)
}

#' Get distance between parallel lines
