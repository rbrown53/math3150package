## code to prepare `EDGreliability` dataset goes here
library(tidyverse) |> suppressPackageStartupMessages()
EDGreliability <- read.csv("data-raw/EDGreliability.txt") |>
  as_tibble() |>
  mutate(plant = factor(plant))
usethis::use_data(EDGreliability, overwrite = TRUE)
