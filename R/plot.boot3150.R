#' Plots a boot3150 object
#'
#' When given a boot3150 object, this function plots the samples with both 
#' histograms and QQ plots.
#'
#' @param x An object of class boot3150
#'
#' @return This function plots the boot3150 object.
#' 
#' @import 
#'   ggplot2
#' @importFrom gridExtra grid.arrange
#'
#' @export plot.boot3150
#' @export

plot.boot3150 <- function(x) {
  plot_dat <- x$samples
  ncols <- ncol(plot_dat)
  for(i in 1:ncols) {
    p1 <- ggplot(plot_dat[,i], aes(x = .data[[paste0("stat", i)]])) +
      geom_histogram(color = "black", fill = "lightblue", bins = 30) +
      geom_vline(xintercept = x$boot_info[i, 3][[1]], color = "red",
                 linetype = "dashed")
    if("term" %in% names(x$boot_info)) {
      p1 <- p1 + labs(title = paste0("Histogram for '", 
                                     x$boot_info$term[i], "'"))
    } else {
      p1 <- p1 + labs(title = paste0("Histogram for stat", i))
    }
    p2 <- ggplot(plot_dat[,i], aes(sample = .data[[paste0("stat", i)]])) +
      geom_qq_line() +
      geom_qq()
    if("term" %in% names(x$boot_info)) {
      p2 <- p2 + labs(title = paste0("QQ Plot for '", 
                                     x$boot_info$term[i], "'"))
    } else {
      p2 <- p2 + labs(title = paste0("QQ Plot for stat", i))
    }
    gridExtra::grid.arrange(p1, p2, nrow = 1)
    if(ncols > 1) {
      if(i < ncols) {
        readline(prompt = paste0("Output for stat", i, 
                                 ". Press [enter] to continue."))
      } else {
        readline(prompt = paste0("Output for stat", i, 
                                 ". Press [enter] to end."))
      }
    }
  }
}