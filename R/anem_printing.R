# anem_printing.R


#' S3 Print method for class "aquifer"
#'
#' @param aquifer Object of type aquifer
print.aquifer <- function(aquifer) {
  aquifer_names <- names(aquifer)

  # Print meta information for standard colums:
  standard_columns <- c("aquifer_type","Ksat","h0")
  if (aquifer$aquifer_type == "confined") {
    standard_columns <- c(standard_columns,"z0")
  }
  for (i in 1:length(standard_columns)) {
    value <- aquifer[standard_columns[i]]
    print_value <- ifelse(is.null(value[[1]]),"--",value)
    cat(paste0("# ",standard_columns[i],": ",print_value,"\n"))
  }

  # Print boundaries
  cat("# bounds: \n")
  if (!is.null(aquifer$bounds)) {
    print(aquifer$bounds)
  } else {
    cat("    No boundaries\n")
  }

  # Print recharge zones
  cat("# recharge: \n")
  if (!is.null(aquifer$recharge)) {
    print(aquifer$recharge)
  } else {
    cat("    No recharge zones\n")
  }

  # Any additional named items?
  additional_names <- aquifer_names[!(aquifer_names %in% c(standard_columns,"bounds","recharge"))]
  if (length(additional_names) > 0) {
    cat("# Additional, non-essential named objects:",paste(additional_names,collapse=", "))
  }
}
