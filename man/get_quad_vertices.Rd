% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/anem_geoprocessing.R
\name{get_quad_vertices}
\alias{get_quad_vertices}
\title{Get vertices of a quadrangle}
\usage{
get_quad_vertices(bounds)
}
\arguments{
\item{bounds}{a data.frame containing 4 lines (rows) defined by bID and x1, y1, x2, y2, or m and b}
}
\value{
Returns a data.frame vertices for each of the 4 bounds. The bounds
are given an id, and there is one row in the output which identifies the opposide bound
(with which there is no intersection).
}
\description{
Get the 4 vertices corresponding to quadrangle of 4 lines
}
\examples{
\dontrun{
library(magrittr)
library(ggplot2)
library(dplyr)
bounds <- data.frame(x1=c(0,10,13,1),y1=c(0,10,9,-1),
  x2=c(10,13,1,0),y2=c(10,9,-1,0)) \%>\% mutate(bID=row_number())
quad_vertices <- get_quad_vertices(bounds)
ggplot() + geom_segment(data=bounds,aes(x1,y1,xend=x2,yend=y2)) +
  geom_point(data=quad_vertices,
    aes(x,y,shape=as.factor(bID),color=as.factor(bID)),size=4) +
  scale_shape_manual(values=1:4)

bounds <- data.frame(x1=c(0,1,2,1),y1=c(1,2,1,0),x2=c(1,2,1,0),
  y2=c(2,1,0,1)) \%>\% mutate(bID=row_number())
quad_vertices <- get_quad_vertices(bounds)
ggplot() + geom_segment(data=bounds,aes(x1,y1,xend=x2,yend=y2)) + coord_equal()
  geom_point(data=quad_vertices,
    aes(x,y,shape=as.factor(bID),color=as.factor(bID)),size=4) +
  scale_shape_manual(values=1:4)

bounds <- data.frame(bID=1:4,x1=c(0,0,10,10),y1=c(0,10,10,0),
  x2=c(0,10,10,0),y2=c(10,10,0,0))
quad_vertices <- get_quad_vertices(bounds)
ggplot() + geom_segment(data=bounds,aes(x1,y1,xend=x2,yend=y2)) +
  geom_point(data=quad_vertices,
    aes(x,y,shape=as.factor(bID),color=as.factor(bID)),size=4) +
  scale_shape_manual(values=1:4) +
  coord_equal()

bounds <-data.frame(bID=c(5, 6, 7, 8),
                    x1=c(468888, 572670, 483978, 468888),
                    x2=c(572670, 588746, 588746, 483978),
                    y1=c(4592114, 4630407, 4518117, 4592114),
                    y2=c(4630407, 4556624, 4556624, 4518117))
vertices <- get_quad_vertices(bounds)
}
}
\keyword{internal}
