% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/anem_imaging.R
\name{reconstruct_image_pumping}
\alias{reconstruct_image_pumping}
\title{Reconstruct pumping at image wells}
\usage{
reconstruct_image_pumping(image_wells)
}
\arguments{
\item{wells}{A data.frame object of wells, containing well_image column with
"Actual", "Image (+Q)", or "Image (-Q)" text.}
}
\value{
A tibble containing wells where Image wells contain pumping rates similar to
generate_image_wells.
}
\description{
Reconstruct pumping of actual wells at image wells
}
\examples{
wells <- define_wells(wells_example)
aquifer <- aquifer_confined_example
image_wells <- generate_image_wells(wells,aquifer)
image_wells_image_NA <- image_wells
image_wells_image_NA$Q[grepl("Image",image_wells_image_NA$well_image)] <- NA
image_wells_reconstructed <- reconstruct_image_pumping(image_wells_image_NA)
identical(image_wells,image_wells_reconstructed)
}
\keyword{internal}
