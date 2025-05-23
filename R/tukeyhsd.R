#' Compute Tukey Honest Significant Differences
#'
#' A tidy version of \link[stats]{TukeyHSD}. Create a set of confidence 
#' intervals on the differences between the means ofthe levels of a factor with
#' the specified family-wise probability of coverage. The intervals are based on
#' the Studentized range statistic, Tukey's 'Honest Significant Difference' 
#' method.
#'
#' @param model A fitted model object, usually an \link[stats]{aov} fit.
#' @param which A character vector listing terms in the fitted model for which 
#' the intervals should be calculated. Defaults to all the terms.
#' @param ordered value indicating if the levels of the factor should be 
#' ordered according to increasing average in the sample before taking 
#' differences. If ordered is true then the calculated differences in the means 
#' will all be positive. The significant differences will be those for which 
#' the lower interval end point is positive.
#' @param conf.level A numeric value between zero and one giving the family-wise
#' confidence level to use.
#' @param ... Optional additional arguments. None are used at present.
#'
#' @examples
#' data(warpbreaks)
#' summary(fm1 <- aov(breaks ~ wool + tension, data = warpbreaks))
#' tukeyhsd(fm1, "tension", ordered = TRUE)
#'
#' @import
#'   broom
#'   dplyr
#'
#' @export

tukeyhsd <- function(model, which, ordered = FALSE, conf.level = 0.95, ...) {
  t <- TukeyHSD(x = model, which = which, 
                ordered = ordered, conf.level = conf.level, ...)
  tidy_tukey <- broom::tidy(t) |>
    rename(diff = estimate) |>
    mutate(conf = conf.level) |>
    select(!null.value)
  tidy_tukey <- tidy_tukey[,c(1, 2, 7, 3:6)]
  tidy_tukey
}