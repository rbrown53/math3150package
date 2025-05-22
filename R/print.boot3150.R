#' Prints a boot3150 object
#'
#' When given a boot3150 object, this function ensures it prints correctly.
#'
#' @param x An object of class boot3150
#'
#' @return This function prints the output for a boot3150 object.
#'
#' @export print.boot3150
#' @export

print.boot3150 <- function(x) {
  print(x$boot_info)
  invisible(x$boot_info)
}