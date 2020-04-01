# anem_printing.R


#' S3 Print method for class "aquifer"
#'
#' @param x Object of type aquifer
#' @param ... Additional, optional arguments to print S3 generic
#' @export
print.aquifer <- function(x, ...) {
  aquifer <- x
  aquifer_names <- names(aquifer)

  # Print meta information for standard colums:
  standard_columns <- c("aquifer_type","Ksat","h0")
  if (aquifer$aquifer_type == "confined") {
    standard_columns <- c(standard_columns,"z0")
  }
  if (any(names(aquifer)=="n")) {
    standard_columns <- c(standard_columns,"n")
  }
  for (i in 1:length(standard_columns)) {
    value <- aquifer[standard_columns[i]]
    print_value <- ifelse(is.null(value[[1]]),"--",value)
    cat(paste0("# ",standard_columns[i],": ",print_value,"\n"))
  }

  # Any additional named items?
  additional_names <- aquifer_names[!(aquifer_names %in% c(standard_columns,"bounds","recharge"))]
  if (length(additional_names) > 0) {
    cat("# Additional named objects:",paste(additional_names,collapse=", "),"\n")
  }

  # Print boundaries
  cat("# bounds: \n")
  if (!is.null(aquifer$bounds)) {
    print(aquifer$bounds)
  } else {
    cat("    No boundaries\n")
  }

  # Print recharge zones
  cat("# recharge (undisturbed water table): \n")
  if (!is.null(aquifer$recharge)) {
    print(aquifer$recharge)
  } else {
    cat("    No recharge zones\n")
  }
}
