#' Computes many confidence intervals and returns how many capture the true mean
#'
#' This function reads in a given sample size, mean mu, standard deviation
#' sigma, confidence coefficient, confidence interval type ("z" or "t"),
#' and the number of simulated samples desired, and returns a count of how
#' many of the corresponding confidence intervals captured the true mean mu.
#'
#' @param n Sample size of each sample. 
#' @param mu The true population mean.
#' @param conf_level The confidence level. By default, this it 95 for 95% 
#' intervals.
#' @param ci_type The type of confidence interval to create. "z" for a 
#' z-interval and "t" for a t-interval.
#' @param n_ints The number of intervals to create. 
#' @param plot_graph Whether to return a plot or not. The default is TRUE. 
#'
#' @return This function returns a graph of all the intervals, if requested, and
#' the number of intervals that contained the true mean.
#'
#' @examples
#' ci_capture(10, 10, 5, 95, "t", 500)
#'
#' @import
#'   ggplot2
#'
#' @export

ci_capture <- function(n, mu, sigma, conf_level = 95, ci_type, n_ints,
                       plot_graph = TRUE) {
  library(ggplot2)
  dat <- matrix(rnorm(n * n_ints, mu, sigma), nrow = n)
  means <- apply(dat, 2, mean)
  sdevs <- apply(dat, 2, sd)
  if (n == 1) {
    sdevs <- rep(0, length(sdevs))
  }
  if (ci_type == "z") {
    crit <- qnorm(1 - (100 - cl) / 100 / 2)
  }
  if (ci_type == "t") {
    crit <- qt(1 - (100 - conf_level) / 100 / 2, n - 1)
  }
  ci.low <- means - crit * sdevs / sqrt(n)
  ci.upp <- means + crit * sdevs / sqrt(n)
  extremes <- c(min(ci.low), max(ci.upp))
  pdist <- max(abs(extremes - mu))
  ci_df <- tibble(
    extremes = c(mu - pdist - 1/mu, rep(mu + pdist + 1, n_ints - 1)), 
    samp = 1:n_ints
    )
  
  numin = sum((ci.low<mu)&(ci.upp>mu))
  if(plot_graph) {
    p <- ggplot(ci_df, aes(x = extremes, y = samp)) + 
      geom_blank() + geom_segment(x = ci.low, xend = ci.upp, 
                                  y = 1:n_ints, yend = 1:n_ints) +
      ggtitle(paste(numin, "Confidence Intervals Captured the True Mean"),
              subtitle = paste0("n = ", n, ", CI Used: ", ci_type,
                                ", Conf. Level = ", conf_level, "%")) +
      labs(x = "CI Bounds", y = "Sample Number") + 
      geom_vline(xintercept = mu, linetype = "dashed", col = "red") + 
      theme(axis.title = element_text(size = 14), 
            plot.title = element_text(size = 14, face = "bold"),
            plot.subtitle = element_text(size = 12, face = "bold"))
    print(p)
  }
  numin
}
