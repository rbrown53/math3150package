#' Creates residual and QQ plots
#'
#' This function creates a residual and a QQ plot for a given model.
#'
#' @param model The model for which we would like these plots. This can be of
#' class "lm" or "glm" with a binomial family.
#' @param resid_type The type of residuals that should be used in the plots. 
#' The options are "jackknife", "raw", "standard", and "pearson". Jackknife 
#' (externally studentized) residuals are used by default for "lm" objects.
#' Standardized Pearson residuals are used for "glm" objects regardless of what 
#' is entered here.
#' @param smoother Logical indicating whether a loess smoother should be added 
#' to the residual plot.
#' @param se Logical indicating whether the standard error bands should be added
#' to the smoother. 
#'
#' @return This function returns plots. The user must press "enter" or "return"
#' to view subsequent plots.
#'
#' @examples
#' mod <- lm(mpg ~ disp, data = mtcars)
#' residual_plots(mod)
#'
#' @import
#'   ggplot2
#'
#' @export

residual_plots <- function(model, resid_type = "jackknife", 
                           smoother = TRUE, se = T) {
  if (class(model)[1] == "lm") {
    if (resid_type == "raw") {
      resids <- residuals(model)
      ylabel <- "Raw Residuals"
    } else if (resid_type == "standard") {
      resids <- rstandard(model)
      ylabel <- "Standardized Residuals"
    } else if (resid_type == "jackknife") {
      resids <- rstudent(model)
      ylabel <- "Jackknife Residuals"
    } 
  } else if (class(model)[1] == "glm" && model$call[3] == "binomial()" ||
    class(model)[1] == "glm" && model$call[3] == '"binomial"()') {
    resids <- residuals(model, type = "pearson") / sqrt(1 - hatvalues(model))
    ylabel <- "Standardized Pearson Residuals"
  } else {
    return(print("Please enter either a linear or logistic model"))
  }

  # Residuals vs Fitted Values
  d_f <- data.frame(resids = resids, predictions = predict(model))

  graph <- ggplot(d_f, aes(x = predictions, y = resids)) +
    geom_point(size = 2) + #, aes(color = color_flag)) +
    labs(x = "Predicted Values", y = ylabel) +
    ggtitle(paste0(ylabel, " vs Predicted Values")) +
    theme(
      axis.title = element_text(size = 14),
      plot.title = element_text(size = 14, face = "bold")
    ) +
    geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.7)
  
  if(smoother == TRUE) {
    graph <- graph + geom_smooth(method = "loess", formula = "y ~ x", 
                                 linewidth = 0.6, alpha = 0.3, se = se)
  }
  
  print(graph)

  readline(prompt = "Residual Plot. Press [enter] to continue.")

  # QQ
  graph <- ggplot(d_f, aes(sample = resids)) +
    geom_qq_line() +
    geom_qq() +
    labs(x = "Theoretical Normal Quantiles", 
         y = paste0("Sample Quantiles (", ylabel, ")")) +
    ggtitle(paste0("QQ Plot for the ", ylabel)) +
    theme(
      axis.title = element_text(size = 14),
      plot.title = element_text(size = 14, face = "bold")
    )
  print(graph)
  
  invisible(readline(prompt = "QQ Plot. Press [enter] to end."))
}
