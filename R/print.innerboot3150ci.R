#' Prints a innerboot3150ci object
#'
#' When given a innerboot3150ci object, this function ensures it prints correctly.
#'
#' @param x An object of class innerboot3150ci
#'
#' @return This function prints the output for a innerboot3150ci object.
#'
#' @export print.innerboot3150ci
#' @export

print.innerboot3150ci <- function(x) {
  cat("BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS\n")
  cat(paste0("Based on ", x$R, " bootstrap replicates\n\n"))
  print(x$ci_info)
  invisible(x$ci_info)
  cat("\nCalculations and Intervals on Original Scale\n")
  if("bca" %in% names(x)) {
    cat("Some BCa intervals may be unstable")
  }
}