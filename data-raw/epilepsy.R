## code to prepare `epilepsy` dataset goes here
library(tidyverse) |> suppressPackageStartupMessages()
epilepsy <- read.csv("data-raw/epilepsy.txt") |>
  as_tibble()
usethis::use_data(epilepsy, overwrite = TRUE)
