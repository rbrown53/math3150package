#' Outputs and restyles an object from the `boot()` function from the boot 
#' package.
#'
#' This function relies on the \link[boot]{boot} function. It generates R 
#' bootstrap replicates of a statistic applied to data. Both parametric and 
#' nonparametric resampling are possible. For the nonparametric bootstrap, 
#' possible resampling methods are the ordinary bootstrap, the balanced 
#' bootstrap, antithetic resampling, and permutation. For nonparametric 
#' multi-sample problems stratified resampling is used: this is specified by 
#' including a vector of strata in the call to boot. Importance resampling 
#' weights may be specified.
#'
#' @param data The data as a vector, matrix or data frame. If it is a matrix or 
#' data frame then each row is considered as one multivariate observation.
#' @param statistic A function which when applied to data returns a vector 
#' containing the statistic(s) of interest. When sim = "parametric", the first 
#' argument to statistic must be the data. For each replicate a simulated 
#' dataset returned by ran.gen will be passed. In all other cases statistic 
#' must take at least two arguments. The first argument passed will always be 
#' the original data. The second will be a vector of indices, frequencies or 
#' weights which define the bootstrap sample. Further, if predictions are 
#' required, then a third argument is required which would be a vector of the 
#' random indices used to generate the bootstrap predictions. Any further 
#' arguments can be passed to statistic through the ... argument.
#' @param R The number of bootstrap replicates. Usually this will be a single 
#' positive integer. For importance resampling, some resamples may use one set 
#' of weights and others use a different set of weights. In this case R would 
#' be a vector of integers where each component gives the number of resamples 
#' from each of the rows of weights.
#' @param ... See more documentation in the \link[boot]{boot} function.
#'
#' @return This function returns an object of class boot3150 that works much
#' like an object of class boot from the `boot` package.
#'
#' @examples
#' med <- function(x, i){median(x[i])}
#' sample_data <- rnorm(100)
#' boot_out <- bootstrapping(sample_data, med, 1000)
#' boot_out
#'
#' @import
#'   boot
#'   broom
#'   dplyr
#'   tibble
#'
#' @export

bootstrapping <- function(data, statistic, R, ...) {
  boot_out <- boot::boot(data, statistic, R, ...)
  tidy_boot <- tidy(boot_out) |>
    rename(original_value = statistic)
  if ("term" %in% names(tidy_boot)) {
    tidy_boot <- tidy_boot |>
      mutate(sample_average = original_value + bias)
    tidy_boot <- tidy_boot[,c(1, 5, 2, 3, 4)]
  } else {
    tidy_boot <- tidy_boot |>
      mutate(sample_average = original_value + bias,
             statistic = paste0("stat", 1:nrow(tidy_boot)))
    tidy_boot <- tidy_boot[,c(5, 4, 1, 2, 3)]
  }
  boot_out$boot_info <- tidy_boot
  
  boot_out$samples <- as_tibble(boot_out$t, .name_repair = "minimal")
  ncols <- ncol(boot_out$t)
  colnames(boot_out$samples) <- paste0("stat", 1:ncols)
  
  class(boot_out) <- "boot3150"
  return(boot_out)
}