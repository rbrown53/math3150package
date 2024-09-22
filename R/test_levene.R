#' Performs Levene's test many times to investigate standard deviations needed
#' to reject that the true SDs are equal
#'
#' This function performs Leven's test for the equality of variance in the ANOVA
#' setting many times for three randomly generated normal samples when given the
#' sample size and records how often the null is rejected, what the standard 
#' deviations for each group is when the null isrejected, the p-values when the 
#' null was rejected, and the ratio of largest SD to smallest SD for each time 
#' the null was rejected.
#'
#' @param n Sample size of each group.
#' @param num_sims The number of Levene's tests that should be performed.
#'
#' @return This function returns a list that contain the standard deviations for
#' each group when we reject the null hypothesis in Levene's test, the p-values 
#' for each time we rejected the null hypothesis, the number of samples, the
#' ratio of largest SD to smallest SD for each time we rejected the null, and
#' the number of times we rejected the null.
#'
#' @examples
#' test_levene(10, 1000)
#'
#' @import
#'   ggplot2
#'
#' @export


# Function used to investigate Levene's test in HW 4
test_levene <- function(n, num_sims) {
  rejects <- 0 # Initialize the the number of null rejections
  ratios <- c() # Initialize the ratios of (max sd)/(min sd) for the rejections
  p_vals <- c() # Initialize the p values for the rejections
  sds <- c() # Initialize the vector of sds for the rejections
  for (i in 1:num_sims) {
    # Randomly generate data from normal distribution for three groups
    x <- rnorm(n) 
    y <- rnorm(n)
    z <- rnorm(n)
    # Find the absolute deviations from the mean for each group
    devx <- abs(x - mean(x))
    devy <- abs(y - mean(y))
    devz <- abs(z - mean(z))
    # Define a factor variable for the group number
    groups <- as.factor(c(rep("1", n), rep("2", n), rep("3", n)))
    # Take the p-value for the Levene's test
    p_val <- anova(aov(c(devx, devy, devz) ~ groups))$"Pr(>F)"[1]
    # If we reject the null hypothesis, store the information
    if (p_val <= 0.01) { 
      # Store the p-value along with all other p-value when we 
      #   rejected the null hypothesis.
      p_vals <- c(p_vals, p_val)
      # Store the sds along with all other sds when we rejected H_0.
      sds_samp <- c(sd(x), sd(y), sd(z))
      sds <- rbind(sds, sds_samp)
      # Store the ratio of (max sd)/(min sd) for the sample along with all other
      #   ratios when we rejected H_0.
      ratios <- c(ratios, max(sds_samp) / min(sds_samp))
      # Count the number of times we rejected H_0.
      #   It should be around 1% of the samples.
      rejects <- rejects + 1
    }
  }
  # Save all information in a list and return the list.
  l <- list(sds = sds, p_vals = p_vals, sample = i,
            ratios = ratios, rejects = rejects)
  return(l)
}
