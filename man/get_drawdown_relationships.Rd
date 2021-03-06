% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/anem_wrappers.R
\name{get_drawdown_relationships}
\alias{get_drawdown_relationships}
\title{Get drawdown relationships}
\usage{
get_drawdown_relationships(wells, aquifer, group_column, weights_column)
}
\arguments{
\item{wells}{Wells data.frame or tibble}

\item{aquifer}{Aquifer object}

\item{group_column}{Name of column to identify groups of wells (no quotes)}

\item{weights_column}{Name of column to identify weights of well pumping (no
quotes). Required -- create column equal to 1 if equal weights desired.}
}
\value{
Returns a data.frame containing the drawdown due to unit pumping for
  all combinations of groups in the group_column. The variable, var, is given
  as D_i_j (confined aquifers), or PHI_i_j (unconfined aquifers), which
  represent the (weighted) average drawdown at wells i due to (weighted) unit
  pumping from wells j.
}
\description{
Calculate Dii, Dij (or PHIii, PHIij) for groups of wells
}
\examples{
# define aquifer
bounds_df <- data.frame(bound_type=c("CH","NF","NF","NF"),m=c(Inf,0,Inf,0),b=c(0,1000,1000,0))
aquifer_unconfined <- define_aquifer("unconfined",1e-3,bounds=bounds_df,h0=100)

# define wells and well images
library(dplyr)
set.seed(30)
wells_df <- data.frame(x=runif(8,0,1000),y=runif(8,0,1000),diam=1,R=1000) \%>\%
  mutate(country=factor(y>500,levels=c(FALSE,TRUE),labels=c("A","B"))) \%>\%
  group_by(country) \%>\%
  mutate(weights=1,Q=1/n()) \%>\% group_by()
wells <- define_wells(wells_df) \%>\% generate_image_wells(aquifer_unconfined)

get_drawdown_relationships(wells, aquifer_unconfined, group_column=country, weights_column=weights)

library(ggplot2)
ggplot() +
  geom_segment(data=aquifer_unconfined$bounds,aes(x1,y1,xend=x2,yend=y2,color=bound_type)) +
  geom_abline(slope=0,intercept=500,linetype="dashed") +
  geom_point(data=wells \%>\% filter(wID==orig_wID),aes(x,y,fill=country),shape=21) +
  coord_equal()
}
