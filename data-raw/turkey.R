## code to prepare `turkey` dataset goes here
library(tidyverse) |> suppressPackageStartupMessages()
turkey <- read.csv("data-raw/turkey.txt") |>
  as_tibble() |>
  mutate(type = factor(type))
usethis::use_data(turkey, overwrite = TRUE)
