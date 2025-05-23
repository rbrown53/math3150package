#' Outputs and restyles an object from the `boot.ci()` function from the boot 
#' package.
#'
#' This function relies on the \link[boot]{boot.ci} function. This function 
#' generates 5 different types of equi-tailed two-sided nonparametric 
#' confidence intervals. These are the first order normal approximation, the 
#' basic bootstrap interval, the studentized bootstrap interval, the bootstrap 
#' percentile interval, and the adjusted bootstrap percentile (BCa) interval. 
#' All or a subset of these intervals can be generated.
#'
#' @param boot_out An object of class "boot" or "boot3150" containing the output
#' of a bootstrap calculation.
#' @param conf A scalar or vector containing the confidence level(s) of the 
#' required interval(s).
#' @param type A vector of character strings representing the type of intervals
#' required. The value should be any subset of the values c("norm","basic",
#' "stud", "perc", "bca") or simply "all" which will compute all five types of 
#' intervals.
#' @param ... See more documentation in the \link[boot]{boot.ci} function.
#'
#' @return This function returns an object of class boot3150ci that works much
#' like an object of class bootci from the `boot` package.
#'
#' @examples
#' med <- function(x, i){median(x[i])}
#' sample_data <- rnorm(100)
#' boot_out <- bootstrapping(sample_data, med, 1000)
#' bootci_out <- boot_ci(boot_out)
#'
#' @import
#'   boot
#'   tibble
#'
#' @export

boot_ci <- function(boot_out, conf = 0.95, type = "all", ...) {
  indices <- nrow(boot_out$boot_info)
  all_cis <- list()
  for(index in 1:indices) {
    cis <- boot.ci(boot_out, conf, type, index = index, ...)
    ci_types <- names(cis)[! names(cis) %in% c("R", "t0", "call")]
    num_cis <- length(ci_types)
    ci_info <- tibble(interval_type = ci_types[1], 
                      conf_level = conf,
                      lower = cis[[4]][length(cis[[4]]) - 1],
                      upper = cis[[4]][length(cis[[4]])])
    if (num_cis > 1) {
      for(i in 5:(3 + num_cis)) {
        ci_info <- ci_info |>
          add_row(interval_type = ci_types[i - 3], 
                  conf_level = conf,
                  lower = cis[[i]][length(cis[[i]]) - 1],
                  upper = cis[[i]][length(cis[[i]])])
      }
    }
    cis$ci_info <- ci_info
    class(cis) <- "innerboot3150ci"
    all_cis[[paste0("stat", index)]] = cis
  }
  if(indices == 1) {
    return(cis)
  } else {
    class(all_cis) <- "boot3150ci"
    return(all_cis)
  }
}