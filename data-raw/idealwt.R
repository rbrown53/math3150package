## code to prepare `idealwt` dataset goes here
library(tidyverse) |> suppressPackageStartupMessages()
idealwt <- read.csv("data-raw/idealwt.txt") |>
  as_tibble() |>
  mutate(sex = factor(sex))
usethis::use_data(idealwt, overwrite = TRUE)
