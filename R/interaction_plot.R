#' Two-way Interaction Plot with ggplot2
#'
#' This function is similar to \link[stats]{interaction.plot}, but plots in 
#' ggplot2 instead of base R. It plots the mean (or other summary) of the 
#' response for two-way combinations of factors, thereby illustrating possible
#' interactions.
#'
#' @param df The data frame that contains the response and factor variables.
#' @param response The name of the numeric variable giving the response. Can be 
#' entered with or without quotes.
#' @param x_factor The name of a factor whose levels will form the x axis. Can 
#' be entered with or without quotes.
#' @param group_factor The name of a factor whose levels will split up the other
#' variables. Can be entered with or without quotes.
#' @param linewidth The linewidth of the lines plotted. Enter 0 if the lines 
#' should be omitted. 
#' @param point_size The size of the points plotted. Enter 0 if the points 
#' should be omitted. 
#' @param ... Other parameters that can be passed to \link[ggplot2]{geom_point}.
#'
#' @return This function returns a two-way interaction plot as a ggplot object. 
#' All the ggplot add-on functions can be added onto the object returned for 
#' more customization. 
#'
#' @examples
#' data(warpbreaks)
#' interaction_plot(warpbreaks, breaks, wool, tension)
#' interaction_plot(warpbreaks, "breaks", "wool", "tension")
#'
#' @import
#'   dplyr
#'   rlang
#'   ggplot2
#'
#' @export

interaction_plot <- function(df, response, x_factor, group_factor,
                             linewidth = 1, point_size = 2, .f = mean, ...) {
  response <- rlang::ensym(response)
  x_factor <- rlang::ensym(x_factor)
  group_factor <- rlang::ensym(group_factor)
  p <- df |>
    group_by(!!x_factor, !!group_factor) |>
    summarize(avg = .f(!!response), .groups = "keep") |>
    rename_with(~paste0("Average of '", response, "' Variable"),
                .cols = avg) |>
    ggplot(aes(x = !!x_factor,
               y = .data[[paste0("Average of '", response, "' Variable")]],
               color = !!group_factor,
               group = !!group_factor,
               linetype = !!group_factor)) +
    geom_point(size = point_size, ...) +
    geom_line(linewidth = linewidth) +
    theme_bw() +
    ggtitle(paste0("Interaction Plot for '", response, "' Grouped by '",
                   x_factor, "' and '", group_factor, "'")) +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 14),
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 12),
          plot.title = element_text(size = 16, face = "bold"))
  return(p)
}