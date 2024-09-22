#' Performs either the Shapiro-Francia or the Shapiro-Wilk normality test
#'
#' When given a model, this will perfrom either the Shapiro-Francia or the 
#' Shapiro-Wilk normality test depending user request or based on sample size.
#' Both of these tests have a null hypothesis that the residuals are normally
#' distributed and an alternative that the residuals are not normally 
#' distributed.
#'
#' @param model A model of type lm or glm. 
#' @param resid_type The type of residuals. The default is "raw", but 
#' supports "jackknife", and "standard" as well.
#' @param test The type of test that should be performed. Options are "default",
#' "sf", and "sw". "sf" performs the Shapiro-Francia test, "sw" performs the
#' Shapiro-Wilk, and "defualt" performs the Shapiro-Francia is n > 30 or 
#' performs the Shapiro-Wilk if  n <= 30.
#' @param plot_graph Whether to return a plot or not. The default is TRUE. 
#'
#' @return This function returns a test statistic, and p-value and, if requested 
#' a plot of the residuals
#'
#' @examples
#' mod <- lm(mpg ~ disp, data = mtcars)
#' bf_test(mod)
#'
#' @import
#'   nortest
#'   ggplot2
#'
#' @export

norm_test <- function(model, resid_type = "raw", test = "default", 
                      plot_graph = TRUE) {
  library(nortest)
  library(ggplot2)
  if (resid_type == "raw") {
    resids <- residuals(model)
    ylabel <- "Raw Residuals"
  } else if (resid_type == "standard") {
    resids <- rstandard(model)
    ylabel <- "Standardized Residuals"
  } else if (resid_type == "jackknife") {
    resids <- rstudent(model)
    ylabel <- "Jackknife Residuals"
  } else if (resid_type == "pearson") {
    resids <- residuals(model, type = "pearson")
    ylabel <- "Pearson Residuals"
  }
  n <- length(resids)

  if(plot_graph) {
    p <- ggplot(data.frame(resids = resids), aes(sample = resids)) + 
      geom_qq(size = 2) + 
      geom_qq_line(linewidth = 1) +
      labs(x = "Theoretical Quantiles", y = ylabel) + 
      ggtitle(label = "Normal Q-Q Plot") + 
      theme(axis.title = element_text(size = 14),
            plot.title = element_text(size = 14, face = "bold"))
    print(p)
  }
  if (test == "default") {
    if (length(resids) > 30) {
      output <- sf.test(resids)
    } else {
      output <- shapiro.test(resids)
    }
  }
  if (test == "sf") {
    output <- sf.test(resids)
  }
  if (test == "sw") {
    output <- shapiro.test(resids)
  }
  output
}
