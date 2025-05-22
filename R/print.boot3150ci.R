#' Prints a boot3150ci object
#'
#' When given a boot3150ci object, this function ensures it prints correctly.
#'
#' @param x An object of class boot3150ci
#'
#' @return This function prints the output for a boot3150ci object.
#'
#' @export print.boot3150ci
#' @export

print.boot3150ci <- function(x) {
  indices <- length(x)
  cat("BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS\n")
  cat(paste0("Based on ", x[[1]]$R, " bootstrap replicates\n\n"))
  if(indices > 1) {
    for(i in 1:indices) {
      cat(paste0("Info for stat", i, "\n"))
      print(x[[i]]$ci_info)
      invisible(x[[i]]$ci_info)
      cat("\n")
    }
  } else {
    print(x$ci_info)
    invisible(x$ci_info)
  }
  cat("Calculations and Intervals on Original Scale\n")
  if("bca" %in% (lapply(x,names) |> unlist() |> unique())) {
    cat("Some BCa intervals may be unstable")
  }
}