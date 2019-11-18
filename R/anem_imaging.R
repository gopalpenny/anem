# anema_imaging.R

#' Mirror a point across a boundary
#'
#' Mirror a point across a boundary, specified as constant head (CH) or no flow
#' (NF) and defined by the line \eqn{m x + b})
#'
#' @param point single point as a list (or sf object single point feature)
#'   containing coordinates (point$x0, point$y0), and pumping rate (point$rate),
#'   which is positive for injection, negative for pumping. Should also have:
#'   diam, path, origin.
#' @param boundary single line as a list (or sf object single line feature)
#'   containing slope (as boundary$m) and intercept (as boundary$b), as well as
#'   the boundary type (bound_type) as "CH" for constant head or "NF" for no flow
#' @param new_pID integer ID for the new point to be created
#' @return a row (same class as point input) containing the new imaged point and
#'   the following columns: x0, y0, rate, diam, path, origin, transform
#'   (boundary type),
#'   source_bound
#' @importFrom magrittr %>%
#' @examples
#' point <- data.frame(pID=1,x0=0,y0=0,rate=0.5,diam=0.75,path=1,origin=1)
#' boundary <- data.frame(bID=1,m=-1,b=1,bound_type="NF")
#' get_mirror_point(point, boundary, new_pID=2)
get_mirror_point <- function(point, boundary, new_pID=NA) {

  if (!tibble::has_name(point,"max_mirror_dist")) {
    point <- point %>% dplyr::mutate(max_mirror_dist= 0)
  }
  if (!tibble::has_name(point,"roi")) {
    point <- point %>% dplyr::mutate(roi= NA)
  }
  # new_pID=5
  # get nearest point on boundary (perpendicular to slope of boundary)
  m <- boundary$m
  b <- boundary$b
  bi <- with(point,y0 + 1/m * x0) # intercept of perpendicular line
  xi <- with(point,m*(bi-b)/(m^2+1)) # x intersection
  yi <- -1/m * xi + bi # y intersection

  # get new coordinates
  x1 <- with(point,2*xi - x0)
  y1 <- with(point,2*yi - y0)

  max_mirror_dist <- pmax(with(point,sqrt((xi-x0)^2+(yi-y0)^2)),point$max_mirror_dist)

  # get pumping rate (depending on NF or CH)
  q <- dplyr::case_when(
    boundary$bound_type=="NF"~point$rate,
    boundary$bound_type=="CH"~-point$rate,
    TRUE~Inf)

  # set path
  path <- paste(point$path," : ",point$pID," (",boundary$bID,"-",boundary$bound_type,")",sep="")

  # return point
  pt_df <- tibble::tibble(pID=new_pID,rate=q,diam=point$diam,x0=x1,y0=y1,
                  origin=point$origin,transform=boundary$bound_type,source_bound=boundary$bID,path=path,
                  max_mirror_dist=max_mirror_dist,roi=point$roi)

  if (max(grepl("sf",class(point)))) {
    pt_df <- pt_df %>% dplyr::mutate(X=x1,Y=y1) %>%
      sf::st_as_sf(coords=c("X","Y")) %>% sf::st_set_crs(sf::st_crs(point))
  }
  return(pt_df)
}

#' Recursively mirror wells across boundaries
#'
#' @param wells Wells with columns rate, diam, x0, y0
#' @param bounds Bounds with columns bID, bound_type, m, b
#' @param num_levels Maximum number of iterations to image wells (and well images). Greater than or equal to 1.
#' @return A data.frame containing original and mirrored wells, with the following columns:
#'   x0, y0, rate, diam, path, origin, transform (boundary type), source_bound.
#'   Columns in wells that are not reproduced by this function are filled with \code{NA}.
#' @section Method:
#' The original wells are labelled L0. These wells are mirrored across the
#' boundaries, and each mirrored well (L1) has a source well (L0) and source
#' boundary (the bound across which it was mirrored) The L1 wells then need to
#' be mirrored across all boundaries except for the source boundary (which
#' would replicate the actual well). The wells from level Lx are mirrored
#' across the boundaries (except for the source boundary), and each mirror
#' well (L+) has a source well (Lx) and source boundary (the bound across
#' which it was mirrored).
#' @section Notes:
#' The function requires that the wells are labeled with a column of identifiers, pID. If it is not
#' present, the function generates them. The image well pID's are always generated automatically.
#' not present.
#' @importFrom magrittr %>%
#' @examples
#' wells <- data.frame(x0=c(0,0.5),y0=c(0,0.25),rate=c(0.5,-0.2),diam=c(0.75,0.8))
#' bounds <- data.frame(m=c(1,-1),b=c(0.5,1),bound_type=c("CH","NF"),bID=c(1,2))
#' mirror_across_bounds(wells,bounds,num_levels=1)
#' mirror_across_bounds(wells,bounds,num_levels=2)
mirror_across_bounds <- function(wells,bounds,num_levels,first_mirror=TRUE) {

  if (!max(grepl("^pID$",names(wells)))) { # generate pID's if they are not present
    wells <- wells %>% dplyr::mutate(pID=dplyr::row_number())
  }
  if (!tibble::has_name(wells,"roi")) {
    wells <- wells %>% dplyr::mutate(roi= NA)
  }

  if (first_mirror) { # initialize wells
    wells <- wells %>% dplyr::mutate(path=pID,origin=pID,transform="none",source_bound="none",path="x",max_mirror_dist=0)
  }
  well_cols <- names(wells)

  wells_full <- list(wells)
  for (i in 1:num_levels) { # for each level, generate mirrored points in next level
    new_wells <- wells_full[[i]] %>% dplyr::filter(FALSE)
    for (j in 1:dim(wells_full[[i]])[1]) { # for each well in level i
      point <- wells_full[[i]][j,] # get point in level i

      for (k in 1:dim(bounds)[1]) { # for each boundary
        boundary <- bounds[k,] # get boundary
        if (point$source_bound!=boundary$bID) { # only take mirror for boundaries that are not the source boundary
          new_pID <- max(c(wells_full[[i]]$pID,new_wells$pID)) + 1 # get new point id
          new_point <- get_mirror_point(point,boundary,new_pID) # get new point
          new_wells <- new_wells %>% rbind(new_point) # add point to new_wells
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


#' Mirror wells across rectangular boundaries
#'
#' Mirror wells across 2 parallel bounds, then the two perpendicular bounds
#'
#' @param bounds Bounds with columns bID, bGroup, bound_type, m, b
#' @inheritParams mirror_across_bounds
#' @return A data.frame containing original and mirrored wells, with the following columns:
#'   x0, y0, rate, diam, path, origin, transform (boundary type), source_bound.
#'   Columns in wells that are not reproduced by this function are filled with \code{NA}.
#' @section Method:
#' The original wells are labelled L0. These wells are mirrored across the
#' parallel boundaries from bGroup 1. Each mirrored well (L1) has a source well (L0) and source
#' boundary (the bound across which it was mirrored). The L1 wells then need to
#' be mirrored across the other boundary from bGroup 1. The wells from level Lx are mirrored
#' across the other (except for the source boundary), and each mirror
#' well (L+) has a source well (Lx) and source boundary (the bound across
#' which it was mirrored). This is done for the parallel boundaries in bGroup 1,
#' then repeated for the other two boundaries in bGroup 2.
#' @section Notes:
#' The function requires that the wells are labeled with a column of identifiers, pID. If it is not
#' present, the function generates them. The image well pID's are always generated automatically.
#' not present.
#' @importFrom magrittr %>%
#' @examples
#' wells <- data.frame(x0=c(5,0.5),y0=c(2.5,0.25),rate=c(0.5,-0.2),diam=c(0.75,0.8))
#' bounds <- tibble(bound_type=c("CH","NF","NF","NF"),
#'                  m=c(0.8,-1.25,0.8,-1.25),b=c(0.3,10,-2.5,0.1),
#'                  bID=as.numeric(1:4),bGroup=c(2,1,2,1))
#' image_wells <- generate_image_wells(wells,bounds,num_levels=1)
#' image_wells <- generate_image_wells(wells,bounds,num_levels=2) %>% mutate(original=level==0)
#' ggplot() +
#'   geom_abline(data=bounds,aes(slope=m,intercept=b)) +
#'   geom_point(data=image_wells,aes(x0,y0,color=as.factor(origin),shape=original)) + #ylim(c(-3,5)) + xlim(c(-1,7)) +
#'   scale_shape_manual(values=c(1,16)) +
#'   coord_equal()
generate_image_wells <- function(wells,bounds,num_levels) {
  if (!max(grepl("^pID$",names(wells)))) { # generate pID's if they are not present
    wells <- wells %>% dplyr::mutate(pID=dplyr::row_number())
  }

  image_wells1 <- mirror_across_bounds(wells,bounds %>% dplyr::filter(bGroup==1),num_levels=num_levels)
  image_wells <- mirror_across_bounds(image_wells1,bounds %>% dplyr::filter(bGroup==2),num_levels=num_levels,first_mirror=FALSE)

  return(image_wells)
}
