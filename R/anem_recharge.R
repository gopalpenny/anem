# anem_recharge.R

#' Recharge confined flow function
#'
#' Recharge confined flow function
#' @param loc Location for evaluating Vector location as c(x,y) or data.frame containing x, y columns
#' @param Aquifer Aquifer containing containing \code{h0} and \code{recharge$params} (see Details)
#' @details
#' Recharge parameters must contain \code{x0}, \code{y0}, \code{x_term}, and \code{y_term}. The last two are:
#' \itemize{
#' \item{\code{x_term}: - flow * cos(theta) * sign(dx) / (Ksat * z0)
#' \item{\code{y_term}: - flow * sin(theta) * sign(dy) / (Ksat * z0)
#' }
#' @return
#' Returns hydraulic head at the location(s)
#' @examples
#' aquifer <- define_aquifer(h0=0,recharge=list())
get_recharge_potential_confined_flow <- function(loc, aquifer) {

  params <- aquifer$recharge
  if (class(loc) == "numeric") {
    loc_class <- "num"
  } else if (any(grepl("data.frame",class(loc)))) {
    loc_class <- "df"
  } else {
    stop("loc must be numeric vector of length 2 or data.frame containin x, y columns.")
  }

  scenario_loc <- paste(params$scenario,loc,sep="_")

  head <- switch(
    scenario_loc,
    "cF_num"= h0 + params$x_term * (loc[1] - params$x0) + params$y_term * (loc[2] - params$y0),
    "cF_df" = h0 + params$x_term * (loc$x - params$x0) + params$y_term * (loc$y - params$y0)
    )

  if (is.null(head)) {
    stop("error processing head.")
  }
  return(head)
}

