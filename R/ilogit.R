#' Compute the Inverse Logit of a Value
#'
#' A function that computes the inverse logit (the inverse log odds) of a value.
#'
#' @param x A value for which that we want to find the inverse.
#'
#' @examples
#' data(mtcars)
#' mod <- glm(vs ~ mpg, data = mtcars, family = binomial())
#' ilogit(predict(mod, data.frame(mpg = 20)))
#'
#' @export

ilogit <- function(x) {
  exp(x)/(1 + exp(x))
}