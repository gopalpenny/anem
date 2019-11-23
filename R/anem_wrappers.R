#' # anem_wrappers.R
#' # wrapper functions to perform high level analysis with anem
#'
#' library(tidyverse)
#'
#'
#'
#' #' Get pumping relationships
#' #'
#' #' Calculate Dii, Dij (or PHIii, PHIij) for groups of wells
#' #'
#' #' @param wells
#' #' @param aquifer
#' #' @export
#' #' @examples
#' #' # define aquifer
#' #' bounds_df <- data.frame(bound_type=c("CH","NF","NF","NF"),m=c(Inf,0,Inf,0),b=c(0,1000,1000,0))
#' #' aquifer_confined <- define_aquifer("unconfined",1e-3,bounds=bounds_df,h0=100)
#' #'
#' #' # define wells and well images
#' #' set.seed(30)
#' #' wells_df <- data.frame(x=runif(8,0,1000),y=runif(8,0,1000),diam=1) %>%
#' #'   dplyr::mutate(R=get_ROI(Ksat=aquifer_confined$Ksat,h=aquifer_confined$h0,t=3600*24*365,n=0.4,method="aravin-numerov"),  # t = 1 year
#' #'                 country=factor(y>500,levels=c(F,T),labels=c("A","B")))
#' #' wells <- define_wells(wells_df) %>% generate_image_wells(aquifer_confined,num_levels = 5)
#' #' ggplot() +
#' #'   geom_segment(data=aquifer_confined$bounds,aes(x1,y1,xend=x2,yend=y2,color=bound_type)) +
#' #'   geom_abline(slope=0,intercept=500,linetype="dashed") +
#' #'   geom_point(data=wells %>% filter(wID==origin),aes(x,y,fill=country),shape=21) +
#' #'   coord_equal()
# get_pumping_relationships <- function(wells,group_column,weights_column=NULL) {
#   if (!is.null(weights_column)) {
#     wells <- wells %>% dplyr::mutate(get_pumping_relationships_weights=1)
#     weights_column <- quo(get_pumping_relationships_weights)
#   }
#   group_column <- enquo(group_column)
#
#   ###########
#   group_column <- quo(country)
#   ###########
#
#   wells <- wells %>% group_by(!! group_column) %>% dplyr::mutate(Q=!! weights_column / sum(!! weights_column))
#
#   well_groups <- wells %>% select(!! group_column) %>% unique()
#   ## need to UNLIST
#   group_df <- tidyr::expand(wells, loc_group=!! group_column,pump_group= !! group_column)
#
#   for (i in 1:dim(group_df)[1]) {
#
#   }
#
#   for
#
#   # drop_column
# }

#' aquifer
#'
#' group_column <- quo(country)
#' pump_group <- "B"
#' aquifer <- aquifer_confined
get_single_drawdown_relationship <- function(wells, aquifer, group_column, loc_group, pump_group) {
  group_column <- enquo(group_column)
  loc <- wells %>% dplyr::filter(wID==origin,!! group_column==loc_group)
  pump_wells <- wells %>% dplyr::filter(wID==origin,!! group_column==pump_group)
  get_potential_differential(loc,pump_wells,aquifer = aquifer)
}
