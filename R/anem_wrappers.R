# anem_wrappers.R # wrapper functions to perform high level analysis with


#' Get drawdown relationships
#'
#' Calculate Dii, Dij (or PHIii, PHIij) for groups of wells
#'
#' @param wells Wells data.frame or tibble
#' @param aquifer Aquifer object
#' @param group_column Name of column to identify groups of wells (no quotes)
#' @param weights_column Name of column to identify weights of well pumping (no
#'   quotes). Required -- create column equal to 1 if equal weights desired.
#' @return Returns a data.frame containing the drawdown due to unit pumping for
#'   all combinations of groups in the group_column. The variable, var, is given
#'   as D_i_j (confined aquifers), or PHI_i_j (unconfined aquifers), which
#'   represent the (weighted) average drawdown at wells i due to (weighted) unit
#'   pumping from wells j.
#' @export
#' @examples
#' # define aquifer
#' bounds_df <- data.frame(bound_type=c("CH","NF","NF","NF"),m=c(Inf,0,Inf,0),b=c(0,1000,1000,0))
#' aquifer_unconfined <- define_aquifer("unconfined",1e-3,bounds=bounds_df,h0=100)
#'
#' # define wells and well images
#' set.seed(30)
#' wells_df <- data.frame(x=runif(8,0,1000),y=runif(8,0,1000),diam=1) %>%
#'   mutate(R=1000,  # t = 1 year
#'          country=factor(y>500,levels=c(F,T),labels=c("A","B"))) %>%
#'   group_by(country) %>%
#'   mutate(weights=1,Q=1/n()) %>% group_by()
#' wells <- define_wells(wells_df) %>% generate_image_wells(aquifer_unconfined)
#'
#' get_drawdown_relationships(wells, aquifer_unconfined, group_column=country, weights_column=weights)
#'
#' ggplot() +
#'   geom_segment(data=aquifer_unconfined$bounds,aes(x1,y1,xend=x2,yend=y2,color=bound_type)) +
#'   geom_abline(slope=0,intercept=500,linetype="dashed") +
#'   geom_point(data=wells %>% filter(wID==orig_wID),aes(x,y,fill=country),shape=21) +
#'   coord_equal()
get_drawdown_relationships <- function(wells,aquifer,group_column,weights_column) {
  # enquo weights column
  # weights_column <- enquo(weights_column)
  # if (!is.null(weights_column)) {
  #   weights_column <- enquo(weights_column)
  # } else {
  #   wells <- wells %>% dplyr::mutate(get_pumping_relationships_weights=1)
  #   weights_column <- rlang::quo(get_pumping_relationships_weights)
  # }
  # print(weights_column)
  weights_column <- rlang::enquo(weights_column)
  group_column <- rlang::enquo(group_column)

  ## need to UNLIST
  group_df <- tidyr::expand(wells, loc_group=!! group_column,pump_group= !! group_column)
  output_df <- tibble::tibble()

  for (i in 1:dim(group_df)[1]) {
    output_df <- output_df %>% dplyr::bind_rows(
      get_single_drawdown_relationship(wells, aquifer, !! group_column, !! weights_column,
                                       loc_group=group_df$loc_group[i], pump_group=group_df$pump_group[i])
    )
  }

  return(output_df)
}

#' Get drawdown relationship
#'
#' Get drawdown relationship at locations locations due to unit pumping from wells. Include weights on location and wells.
#' @param loc_group Value in wells$group_column where drawdown should be measured
#' @param pump_group Value in wells$group_column which identifies wells
#' @inheritParams get_drawdown_relationships
#' @return Returns a list with two values:
#' pot, which is the potential differential due to unit pumping from the pumping wells in pump_group.
#'
#' var, which is given as D_i_j, or PHI_i_j -- the average drawdown at wells i due to wells j.
#'
#' The unit pumping at the pumping wells is weighted by values in weights_column for the pump_group.
#' The average drawdown is also weighted by values in weights_column, for the loc_group.
#' @examples
#' # define aquifer
#' bounds_df <- data.frame(bound_type=c("CH","NF","NF","NF"),m=c(Inf,0,Inf,0),b=c(0,1000,1000,0))
#' aquifer_unconfined <- define_aquifer("unconfined",1e-3,bounds=bounds_df,h0=100)
#'
#' # define wells and well images
#' set.seed(30)
#' wells_df <- data.frame(x=runif(8,0,1000),y=runif(8,0,1000),diam=1) %>%
#'   mutate(R=1000,  # t = 1 year
#'          country=factor(y>500,levels=c(F,T),labels=c("A","B"))) %>%
#'   group_by(country) %>%
#'   mutate(weights=1,Q=1/n()) %>% group_by()
#' wells <- define_wells(wells_df) %>% generate_image_wells(aquifer_unconfined)
#'
#' get_single_drawdown_relationship(wells, aquifer_unconfined, group_column=country, weights_column=weights,loc_group="S", pump_group="F")
#'
#' ggplot() +
#'   geom_segment(data=aquifer_unconfined$bounds,aes(x1,y1,xend=x2,yend=y2,color=bound_type)) +
#'   geom_abline(slope=0,intercept=500,linetype="dashed") +
#'   geom_point(data=wells %>% filter(wID==orig_wID),aes(x,y,fill=country),shape=21) +
#'   coord_equal()
get_single_drawdown_relationship <- function(wells, aquifer, group_column, weights_column, loc_group, pump_group) {
  is_sf <- max(grepl("sf",class(wells)))
  if (is_sf) {
    wells <- wells %>% sf::st_set_geometry(NULL)
  }
  wells <- wells %>% dplyr::group_by()
  group_column <- rlang::enquo(group_column)
  weights_column <- rlang::enquo(weights_column)

  # for debugging only:
  # weights_column <- rlang::quo(weights)
  # group_column <- rlang::quo(country)
  # aquifer <- aquifer_confined
  # loc_group <- "F"
  # pump_group <- "S"
  # end debugging code

  check_weights <- wells %>% dplyr::select(orig_wID,!! weights_column) %>% dplyr::distinct() %>%
    dplyr::group_by(orig_wID) %>%
    dplyr::summarize(n=dplyr::n())
  if (max(check_weights$n)>1) {
    stop("Weights of Image wells are different from the weights of the Actual wells. Try generating weights of wells before generating images.")
  }

  var_type <- ifelse(aquifer$aquifer_type=="confined","D","PHI")
  var_name <- ifelse(aquifer$aquifer_type=="confined","hydraulic head","discharge potential")

  loc <- wells %>% dplyr::filter(well_image=="Actual",!! group_column==loc_group)
  pump_wells <- wells %>% dplyr::filter(!! group_column==pump_group) %>%
    dplyr::mutate(Q=!! weights_column / sum((!! weights_column)[well_image=="Actual"])) %>%
    reconstruct_image_pumping()
  loc_potential <- loc %>% dplyr::mutate(potential=get_potential_differential(loc,pump_wells,aquifer = aquifer))

  weighted_potential <- loc_potential %>% dplyr::group_by() %>%
    dplyr::summarize(var=paste(var_type,loc_group,pump_group,sep="_"),
                     pot=weighted.mean(potential,!! weights_column),
                     description=paste("Weighted effect of pumping in group",pump_group,"on",var_name,"in group",loc_group))

  return(weighted_potential)
}


#' Map hydrodynamics
#'
#' This function is used for mapping hydrodynamics by generating gridded output of
#' hydraulic head and flow direction
#' @inheritParams get_drawdown_relationships
#' @param head_dim Vector containing number of rows and columns as \code{c(nrow,ncol)} for the grid of hydraulic head
#' @param flow_dim Vector containing number of rows and columns as \code{c(nrow,ncol)} for the grid of flow
#' @return
#' The function returns a list containing:
#' \itemize{
#'   \item{\code{head}: }{A data.frame of gridded hydraulic head}
#'   \item{\code{flow}: }{A data.frame of flow with gridded \code{x} and \code{y}, along with \code{dx} and
#'   \code{dy} from \code{get_flow_dir()}, and \code{x2} and \code{y2} which are specifically
#'   intended for plotting using \code{geom_segment()}-- they are normalized so that
#'   the maximum value is equal to the grid spacing.}
#' }
#' @export
#' @examples
#' # define aquifer
#' bounds_df <- data.frame(bound_type=c("CH","NF","NF","NF"),m=c(Inf,0,Inf,0),b=c(0,1000,1000,0))
#' aquifer_unconfined <- define_aquifer("unconfined",1e-3,bounds=bounds_df,h0=100)
#'
#' # define wells and well images
#' set.seed(30)
#' wells_df <- data.frame(x=runif(8,0,1000),y=runif(8,0,1000),diam=1) %>%
#'   mutate(R=1000,Q=-1/n())
#' wells <- define_wells(wells_df) %>% generate_image_wells(aquifer_unconfined)
#'
#' gridded <- get_gridded_hydrodynamics(wells,aquifer_unconfined,c(20,20),c(8,8))
#'
#' ggplot() +
#'   geom_raster(data=gridded$head,aes(x,y,fill=head_m)) +
#'   geom_segment(data=gridded$flow,aes(x,y,xend=x2,yend=y2),
#'                arrow = arrow(ends="last",type="closed",length=unit(1,"mm")),color="black") +
#'   geom_segment(data=aquifer_unconfined$bounds,aes(x1,y1,xend=x2,yend=y2,color=bound_type)) +
#'   geom_point(data=wells %>% filter(wID==orig_wID),aes(x,y),shape=21) +
#'   coord_equal()
get_gridded_hydrodynamics <- function(wells,aquifer,head_dim=c(20,20),flow_dim=c(10,10)) {

  grid_bounds <- get_quad_vertices(aquifer$bounds) %>% dplyr::filter(!is.na(x)) %>%
    dplyr::summarize(xmin=min(x),xmax=max(x),ymin=min(y),ymax=max(y))
  grid_pts_head <- with(grid_bounds,tidyr::crossing(x=seq(xmin,xmax,length.out=head_dim[1]),
                                             y=seq(ymin,ymax,length.out=head_dim[2])))
  heads <- get_hydraulic_head(grid_pts_head,wells,aquifer)
  head_df <- grid_pts_head %>% dplyr::bind_cols(head_m=heads)

  flow_x_seq <- with(grid_bounds,seq(xmin,xmax,length.out=flow_dim[1]))
  flow_y_seq <- with(grid_bounds,seq(ymin,ymax,length.out=flow_dim[2]))
  flow_grid_dx <- flow_x_seq[2] - flow_x_seq[1]
  flow_grid_dy <- flow_y_seq[2] - flow_y_seq[1]
  grid_spacing <- min(flow_grid_dx,flow_grid_dy)
  grid_pts_flowdir <- tidyr::crossing(x=flow_x_seq,y=flow_y_seq)
  flow_dir <- get_flowdir(grid_pts_flowdir,wells,aquifer)
  # the dx_norm and dy_norm are used to make the arrows visible in the plot that follows.
  flow_dir_df <- grid_pts_flowdir %>% dplyr::bind_cols(flow_dir) %>%
    dplyr::mutate(angle=atan(dy/dx),
           mag=sqrt(dx^2+dy^2),
           mag_max=max(mag),
           flow_scaling=grid_spacing/mag_max,
           mag_norm=mag*flow_scaling, # this scales the flow for the arrow plot
           dx_norm=cos(angle)*sign(dx)*mag_norm,
           dy_norm=sin(angle)*sign(dx)*mag_norm,
           x2=x+dx_norm,
           y2=y+dy_norm) %>%
    dplyr::select(x,y,dx,dy,x2,y2)
  return(list(head=head_df,flow=flow_dir_df))
}


