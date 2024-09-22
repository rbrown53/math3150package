#' Creates many diagnostic plots
#'
#' This function creates many diagnostic plots for a given model. These plots 
#' include residual plots, a leverage plot, a Cook's distance plot, a DfFits
#' plot, and DfBetas plots for the intercept and all slopes.
#'
#' @param model The model for which we would like these plots. This can be of
#' class "lm" or "glm" with a binomial family.
#' @param missing_group Used for multinomial regression to indicate which group
#' is not being plotted. 
#'
#' @return This function returns plots. The user must press "enter" or "return"
#' to view subsequent plots.
#'
#' @examples
#' mod <- lm(mpg ~ disp, data = mtcars)
#' influence_plots(mod)
#'
#' @import
#'   ggplot2
#'
#' @export

influence_plots <- function(model, missing_group = NULL) {
  # missing_group is used for multinomial regression
  # Find the sample size using degrees of freedom
  n <- sum(summary(model)$df[1:2])
  # Obtain the number of parameters (p = # predictors + 1 for intercept)
  p <- summary(model)$df[1] 
  if (class(model)[1] == "lm") {
    resids <- rstudent(model)
    residlab <- "Jackknife Residuals"
    cutoff <- qt(1 - .05 / 2 / n, n - p - 1)
    cutoff_low <- qt(1 - .05 / 2, n - p - 1)
    missinglab <- ""
  } else if (class(model)[1] == "glm" && model$call[3] == "binomial()" ||
    class(model)[1] == "glm" && model$call[3] == '"binomial"()') {
    resids <- residuals(model, type = "pearson") / sqrt(1 - hatvalues(model))
    residlab <- "Standardized Pearson Residuals"
    cutoff <- qnorm(1 - .05 / 2 / n)
    cutoff_low <- qnorm(1 - .05 / 2)
    if(!is.null(missing_group)) {
      missinglab <- paste0("\nWithout Group ", missing_group)
    } else {
      missinglab <- ""
    }
  } else {
    return(print("Please enter either an linear or logistic model"))
  }
  names(resids) <- rownames(model.frame(model))

  # Residuals
  index <- 1:n
  d_f <- data.frame(resids = resids, index = index)

  labelx_pos_rows <- which(resids > cutoff_low)
  labelx_neg_rows <- which(resids < -cutoff_low)
  labelx_pos <- rep("", n)
  labelx_pos[labelx_pos_rows] <- names(labelx_pos_rows)
  labelx_neg <- rep("", n)
  labelx_neg[labelx_neg_rows] <- names(labelx_neg_rows)
  color_flag <- labelx_pos != "" | labelx_neg != ""

  graph <- ggplot(d_f, aes(x = index, y = resids)) +
    geom_linerange(
      ymin = 0, ymax = resids,
      aes(color = color_flag)
    ) +
    geom_hline(yintercept = c(-cutoff, cutoff), linewidth = 0.7) +
    geom_hline(
      yintercept = c(-cutoff_low, cutoff_low),
      linewidth = 0.7, linetype = "dashed"
    ) +
    geom_text(label = labelx_pos, nudge_y = 0.2) +
    geom_text(label = labelx_neg, nudge_y = -0.2) +
    labs(x = "Index", y = residlab) +
    ggtitle(paste0("Plot of ", residlab, missinglab)) +
    theme(
      axis.title = element_text(size = 14),
      plot.title = element_text(size = 14, face = "bold")
    ) +
    theme(legend.position = "none") +
    scale_color_manual(values = c("black", "#F8766D"))
  print(graph)

  readline(prompt = "Plot of Residuals. Press [enter] to continue")

  # Residuals vs Fitted Values
  d_f <- data.frame(resids = resids, predictions = predict(model))

  graph <- ggplot(d_f, aes(x = predictions, y = resids)) +
    geom_point(size = 2, aes(color = color_flag)) +
    labs(x = "Predicted Values", y = residlab) +
    ggtitle(paste0(residlab, " vs Predicted Values", missinglab)) +
    theme(
      axis.title = element_text(size = 14),
      plot.title = element_text(size = 14, face = "bold")
    ) +
    geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.7) +
    geom_smooth(method = "loess", formula = y ~ x, 
                linewidth = 0.6, alpha = 0.3) +
    geom_text(label = labelx_pos, nudge_y = 0.2) +
    geom_text(label = labelx_neg, nudge_y = -0.2) +
    theme(legend.position = "none") +
    scale_color_manual(values = c("black", "#F8766D"))
  print(graph)

  readline(prompt = "Residual Plot. Press [enter] to continue")

  # Leverage Plot
  hatvals <- hatvalues(model)
  d_f <- data.frame(hatvals = hatvals, index = index)

  cutoff <- 2 * p / n
  labelx_rows <- which(hatvals > cutoff)
  labelx <- rep("", n)
  labelx[labelx_rows] <- names(labelx_rows)
  color_flag <- labelx != ""

  graph <- ggplot(d_f, aes(x = index, y = hatvals)) +
    geom_linerange(ymin = 0, ymax = hatvals, aes(color = color_flag)) +
    geom_hline(yintercept = cutoff, linewidth = 0.7) +
    geom_text(label = labelx, nudge_y = max(hatvals) * 0.02) +
    labs(x = "Index", y = "Leverage (Hat Value)") +
    ggtitle(paste0("Leverage Plot", missinglab)) +
    theme(
      axis.title = element_text(size = 14),
      plot.title = element_text(size = 14, face = "bold")
    ) +
    theme(legend.position = "none") +
    scale_color_manual(values = c("black", "#F8766D"))
  print(graph)

  readline(prompt = "Leverage Plot. Press [enter] to continue")

  # Cooks Distance
  cooks <- cooks.distance(model)
  d_f <- data.frame(cooks = cooks, index = index)

  cutoff <- 4 / n
  labelx_rows <- which(cooks > cutoff)
  labelx <- rep("", n)
  labelx[labelx_rows] <- names(labelx_rows)
  color_flag <- labelx != ""

  graph <- ggplot(d_f, aes(x = index, y = cooks)) +
    geom_linerange(ymin = 0, ymax = cooks, aes(color = color_flag)) +
    geom_hline(yintercept = cutoff, linewidth = 0.7) +
    geom_text(label = labelx, nudge_y = max(cooks) * 0.02) +
    labs(x = "Index", y = "Cook's Distance") +
    ggtitle(paste0("Cook's Distance Plot", missinglab)) +
    theme(
      axis.title = element_text(size = 14),
      plot.title = element_text(size = 14, face = "bold")
    ) +
    theme(legend.position = "none") +
    scale_color_manual(values = c("black", "#F8766D"))
  print(graph)

  readline(prompt = "Cook's Distance. Press [enter] to continue")

  # DfFits
  cutoff <- 2 * sqrt(p / n)
  df_fits <- dffits(model)
  d_f <- data.frame(df_fits = df_fits, index = index)

  labelx_pos_rows <- which(df_fits > cutoff)
  labelx_neg_rows <- which(df_fits < -cutoff)
  labelx_pos <- rep("", n)
  labelx_pos[labelx_pos_rows] <- names(labelx_pos_rows)
  labelx_neg <- rep("", n)
  labelx_neg[labelx_neg_rows] <- names(labelx_neg_rows)
  color_flag <- labelx_pos != "" | labelx_neg != ""

  graph <- ggplot(d_f, aes(x = index, y = df_fits)) +
    geom_linerange(
      ymin = 0, ymax = df_fits,
      aes(color = color_flag)
    ) +
    geom_hline(yintercept = c(-cutoff, cutoff), linewidth = 0.7) +
    geom_text(label = labelx_pos, nudge_y = max(abs(df_fits)) * 0.05) +
    geom_text(label = labelx_neg, nudge_y = -max(abs(df_fits)) * 0.05) +
    labs(x = "Index", y = "DfFits") +
    ggtitle(paste0("DfFits Plot", missinglab)) +
    theme(
      axis.title = element_text(size = 14),
      plot.title = element_text(size = 14, face = "bold")
    ) +
    theme(legend.position = "none") +
    scale_color_manual(values = c("black", "#F8766D"))
  print(graph)

  readline(prompt = "DfFits. Press [enter] to continue")

  # DfBetas
  cutoff <- 2 / sqrt(n)
  for (i in 1:p) {
    label <- toupper(names(model$coefficients)[i])
    if (i == 1) {
      label <- gsub("^.|.$", "", label) # Removes ( and ) from (INTERCEPT)
    }
    betas <- dfbetas(model)[, i]
    labelx_pos_rows <- which(betas > cutoff)
    labelx_neg_rows <- which(betas < -cutoff)
    labelx_pos <- rep("", n)
    labelx_pos[labelx_pos_rows] <- names(labelx_pos_rows)
    labelx_neg <- rep("", n)
    labelx_neg[labelx_neg_rows] <- names(labelx_neg_rows)
    color_flag <- labelx_pos != "" | labelx_neg != ""

    d_f <- data.frame(betas = betas, index = index)
    graph <- ggplot(d_f, aes(x = index, y = betas)) +
      geom_linerange(
        ymin = 0, ymax = betas,
        aes(color = color_flag)
      ) +
      geom_hline(yintercept = c(-cutoff, cutoff), linewidth = 0.7) +
      geom_text(label = labelx_pos, nudge_y = max(abs(betas)) * 0.05) +
      geom_text(label = labelx_neg, nudge_y = -max(abs(betas)) * 0.05) +
      labs(x = "Index", y = paste0("DfBetas (for ", label, ")")) +
      ggtitle(paste0("DfBetas Plot for ", label, missinglab)) +
      theme(
        axis.title = element_text(size = 14),
        plot.title = element_text(size = 14, face = "bold")
      ) +
      theme(legend.position = "none") +
      scale_color_manual(values = c("black", "#F8766D"))
    print(graph)

    readline(prompt = "DfBetas. Press [enter] to continue")
  }
}