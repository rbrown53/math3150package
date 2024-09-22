## code to prepare `abalone` dataset goes here
library(tidyverse) |> suppressPackageStartupMessages()
abalone <- read.csv("data-raw/abalone.csv") |>
  as_tibble() |>
  mutate(sex = factor(sex, levels = c("M", "F", "I")))
usethis::use_data(abalone, overwrite = TRUE)
