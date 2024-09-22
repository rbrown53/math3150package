#' Perform the Brown-Forsythe Test for Equality of Variance
#'
#' When given a model, this will break up into groups of equal size with a 
#' default of 2 perform a Brown Forsythe test. By default, the test will be 
#' performed with the jackknife residuals.
#'
#' @param model A model of type lm or glm. 
#' @param num_groups The number of groups to be used. The default is 2.
#' @param resid_type The type of residuals. The default is "jackknife", but 
#' supports "raw", "standard", and "pearson". 
#' @param plot_graph Whether to return a plot or not. The default is TRUE. 
#'
#' @return This function returns an ANOVA table and, if requested a plot of the 
#' residuals 
#'
#' @examples
#' mod <- lm(mpg ~ disp, data = mtcars)
#' bf_test(mod)
#'
#' @import
#'   ggplot2
#'
#' @export

bf_test <- function(model, num_groups = 2, resid_type = "jackknife",
                    plot_graph = TRUE) {
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
  fitted <- fitted.values(model)
  q <- seq(1 / num_groups, 0.99, by = 1 / num_groups)
  groups <- rep(0, length(fitted))
  groups[fitted <= quantile(fitted, q)[1]] <- 1
  groups[fitted >= quantile(fitted, q)[num_groups - 1]] <- num_groups
  for (i in 2:(num_groups - 1)) {
    groups[fitted > quantile(fitted, q)[i - 1] &
      fitted < quantile(fitted, q)[i]] <- i
  }
  groups <- as.factor(groups)
  d_f <- data.frame(resids = resids, groups = groups)
  
  if(plot_graph) {
  
    plot_graph <- ggplot(d_f, aes(fitted, resids)) + 
      geom_point(size = 2) +
      labs(x = "Fitted Values", y = ylabel, sizwe = 1.2) +
      ggtitle("Residual Plot") + 
      geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1.1) +
      theme(axis.title = element_text(size = 14),
            plot.title = element_text(size = 14, face = "bold")) +
      geom_vline(xintercept = quantile(fitted, q), linewidth = 0.8)
    print(plot_graph)
  }
  
  medians <- tapply(resids, groups, median)
  absdev <- abs(resids - medians[groups])
  cat("Brown-Forsythe Test Output Using", num_groups, "Groups\n\n")
  anova(aov(absdev ~ groups))
}
