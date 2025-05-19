#' Outputs estimates, standard errors, test statistic, and p-values for a
#' multinomial logistic regression object from the nnet library.
#'
#' When given an nnet multinom object, this function will compute the test
#' statistic and p-value for each term in the models and return a tibble with 
#' the coefficients, standard errors, test statistic, and p-values in a list 
#' with one list element per model.
#'
#' @param model An object of type "multinom" "nnet"
#'
#' @return This function returns a list of tibbles with the coefficient 
#' estimates, the standard errors, the test statistics, and the p-values for 
#' the models comparing each category to the baseline. The AIC and deviance are
#' also given as the last elements in the list. 
#'
#' @examples
#' mod <- mtcars |> 
#'   mutate(carb = factor(carb)) |>
#'   multinom(carb ~ mpg + disp + hp, data = _)
#' multinom_summary(mod)
#'
#' @import
#'   nnet
#'   tibble
#'
#' @export
#' 

multinom_summary <- function(model) {
  if (!inherits(model, "multinom")) {
    stop("The model should be a multinomial model from the 'nnet' library.")
  }
  
  model_summary <- summary(model)
  coefs <- model_summary$coefficients
  std_errs <- model_summary$standard.errors
  
  z_stats <- coefs / std_errs
  p_vals <- 2 * (1 - pnorm(abs(z_stats)))
  
  model_info_list <- vector("list", length = nrow(coefs))
  
  for (i in seq_len(nrow(coefs))) {
    info_matrix <- rbind(coefs[i,], std_errs[i,], z_stats[i,], p_vals[i,])
    rownames(info_matrix) <- c("estimate", "std.error", "z.stat", "p.value")
    info_df <- as_tibble(t(info_matrix), rownames = "term")
    model_info_list[[i]] <- info_df
  }
  
  response_categories <- colnames(model$fitted.values)
  non_baselines <- rownames(coefs)
  baseline <- setdiff(response_categories, non_baselines)
  
  # Append model stats
  model_info_list[[length(model_info_list) + 1]] <- model_summary$AIC
  model_info_list[[length(model_info_list) + 1]] <- model_summary$deviance
  
  names(model_info_list) <- c(
    paste(non_baselines, "vs", baseline, "(baseline)"), 
    "AIC", 
    "deviance"
  )
  
  model_info_list
}