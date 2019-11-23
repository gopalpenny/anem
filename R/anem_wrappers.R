#' # anem_wrappers.R # wrapper functions to perform high level analysis with
#' anem
#'
#' library(tidyverse)
#'
#'
#'
#' Get pumping relationships
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
#' aquifer_confined <- define_aquifer("unconfined",1e-3,bounds=bounds_df,h0=100)
#'
#' # define wells and well images
#' set.seed(30)
#' wells_df <- data.frame(x=runif(8,0,1000),y=runif(8,0,1000),diam=1) %>%
#'   dplyr::mutate(R=get_ROI(Ksat=aquifer_confined$Ksat,h=aquifer_confined$h0,t=3600*24*365,n=0.4,method="aravin-numerov"),  # t = 1 year
#'                 country=factor(y>500,levels=c(F,T),labels=c("A","B")),
#'                 weights=1)
#' wells <- define_wells(wells_df) %>% generate_image_wells(aquifer_confined)
#' get_pumping_relationships(wells,aquifer_confined,country,weights)
#' ggplot() +
#'   geom_segment(data=aquifer_confined$bounds,aes(x1,y1,xend=x2,yend=y2,color=bound_type)) +
#'   geom_abline(slope=0,intercept=500,linetype="dashed") +
#'   geom_point(data=wells %>% filter(wID==orig_wID),aes(x,y,fill=country),shape=21) +
#'   coord_equal()
get_pumping_relationships <- function(wells,aquifer,group_column,weights_column) {
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
#' @inheritParams get_pumping_relationships
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
#' aquifer_confined <- define_aquifer("unconfined",1e-3,bounds=bounds_df,h0=100)
#'
#' # define wells and well images
#' set.seed(30)
#' wells_df <- data.frame(x=runif(8,0,1000),y=runif(8,0,1000),diam=1) %>%
#'   dplyr::mutate(R=1000,  # t = 1 year
#'                 country=factor(y>500,levels=c(F,T),labels=c("A","B"))) %>%
#'   group_by(country) %>% mutate(weights=row_number()) %>% group_by()
#' wells <- define_wells(wells_df) %>% generate_image_wells(aquifer_confined)
#'
#' # Get drawdown relationship
#' get_single_drawdown_relationship(wells, aquifer_confined, group_column=country, weights_column=weights,loc_group="A", pump_group="B")
#'
#' ggplot() +
#'   geom_segment(data=aquifer_confined$bounds,aes(x1,y1,xend=x2,yend=y2,color=bound_type)) +
#'   geom_abline(slope=0,intercept=500,linetype="dashed") +
#'   geom_point(data=wells %>% filter(wID==orig_wID),aes(x,y,fill=country),shape=21) +
#'   coord_equal()
get_single_drawdown_relationship <- function(wells, aquifer, group_column, weights_column, loc_group, pump_group) {
  wells <- wells %>% dplyr::group_by()
  group_column <- rlang::enquo(group_column)
  weights_column <- rlang::enquo(weights_column)

  # weights_column <- rlang::quo(weights)
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
    dplyr::mutate(Q=!! weights_column / sum((!! weights_column)[well_image=="Actual"]))
  loc_potential <- loc %>% dplyr::mutate(potential=get_potential_differential(loc,pump_wells,aquifer = aquifer))
  weighted_potential <- loc_potential %>% dplyr::group_by() %>%
    dplyr::summarize(var=paste(var_type,loc_group,pump_group,sep="_"),
                     pot=weighted.mean(potential,!! weights_column),
                     description=paste("Weighted effect of pumping in group",pump_group,"on",var_name,"in group",loc_group))

  return(weighted_potential)
}
