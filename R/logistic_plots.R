#' Creates many diagnostic plots for logistic or multinomial models
#'
#' This function feeds into the `influence_plots()` function to create many 
#' diagnostic plots for a given logistic or multinomial model. These plots 
#' include residual plots, a leverage plot, a Cook's distance plot, a DfFits
#' plot, and DfBetas plots for the intercept and all slopes.
#'
#' @param model The model for which we would like these plots. This can be of
#' class "glm" with a binomial family or of class "multinom" from the `nnet` 
#' library.
#' @param missing_group Used for multinomial regression to indicate which group
#' is not being plotted. 
#'
#' @return This function returns plots. The user must press "enter" or "return"
#' to view subsequent plots.
#'
#' @examples
#' mod <- glm(vs ~ disp, data = mtcars, family = "binomial")
#' logistic_plots(mod)
#'
#' @import
#'   ggplot2
#'
#' @export

logistic_plots <- function(model) {
  if (class(model)[1] == "multinom") {
    modeldf <- model.frame(model)
    y <- as.factor(model.frame(model)[, 1])
    ylevels <- levels(y)
    for (i in ylevels[-1]) {
      print(paste("Plots for Model Without Group", i))
      if (dim(modeldf)[2] > 2) {
        influence_plots(
          glm(y[y != i] ~ ., data = modeldf[y != i, -1],family = binomial),
          missing_group = i
          )
      } else {
        influence_plots(
          glm(y[y != i] ~ modeldf[y != i, -1], family = binomial),
          missing_group = i
          )
      }
    }
  } else if (class(model)[1] == "glm") {
    influence_plots(model)
  }
}
