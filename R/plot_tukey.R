#' Plots the Tukey HSD Confidence Intervals
#'
#' This function plots the Tukey HSD confidence intervals when given a tibble
#' outputted by the \link[math3150package]{tukeyhsd} function. If more than one 
#' variable, all will be plotted by default, but that can be adjusted with the 
#' `which` option.
#'
#' @param t The Tukey HSD tibble outputted by the 
#' \link[math3150package]{tukeyhsd} function.
#' @param which Indicates which variable for which the graph should be created. 
#' The default is "all", but a variable name used in the model can also be 
#' entered (in quotes).
#'
#' @return This function returns plots. The user must press "enter" or "return"
#' to view subsequent plots if there are any.
#'
#' @examples
#' data(warpbreaks)
#' fm1 <- aov(breaks ~ wool + tension, data = warpbreaks)
#' t <- tukeyhsd(fm1, "tension", ordered = TRUE)
#' plot_tukey(t)
#'
#' @import
#'   dplyr
#'   ggplot2
#'
#' @export

plot_tukey <- function(t, which = "all") {
  if(which == "all") {
    unique_terms <- unique(t$term)
  } else {
    unique_terms <- which
  }
  conf.level <- unique(t$conf)
  t <- mutate(t, 
              sig = case_when(
                adj.p.value > 0.1 ~ "",
                adj.p.value < 0.1 & adj.p.value > 0.05 ~ ".",
                adj.p.value < 0.05 & adj.p.value > 0.01 ~ "*",
                adj.p.value < 0.01 & adj.p.value > 0.001 ~ "**",
                adj.p.value < 0.001 ~ "***"
              )
  )
  for(term_index in unique_terms) {
    df <- t |> 
      filter(term == term_index) |>
      mutate(contrast = fct_rev(factor(contrast, levels = t$contrast)))
    lab <- paste0("Differences in mean levels of '", term_index, "'")
    p <- ggplot(df, aes(x = contrast, 
                        y = diff, 
                        color = contrast)) + 
      geom_hline(yintercept = 0, linetype = 2) + 
      geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                    size = 1, width = 0.2) + 
      geom_text(aes(label = sig), size = 7, vjust = 0.2) + 
      geom_point(size = 3) + 
      ylab(lab) + 
      xlab("") + 
      ggtitle(paste0("Tukey ", conf.level * 100, 
                     "% family-wise confidence level")) + 
      theme(plot.title = element_text(size = rel(2)), 
            axis.text.y = element_text(angle = 0, size = rel(1.5), 
                                       hjust = 0.5), 
            axis.text.x = element_text(size = rel(1.5)), 
            axis.title.x = element_text(size = rel(1.5))) +
      theme(legend.position = "none") +
      coord_flip() 
    
    if(length(unique_terms) > 1) {
      print(p)
      readline(prompt = paste0("Plot for levels of '", term_index, 
                               "'. Press [enter] to continue."))
    } else {
      return(p)
    }
  }
}