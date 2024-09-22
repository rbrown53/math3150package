## code to prepare `logging` dataset goes here
library(tidyverse) |> suppressPackageStartupMessages()
logging <- read.csv("data-raw/logging.txt") |>
  as_tibble() |>
  mutate(group = factor(group))
usethis::use_data(logging, overwrite = TRUE)
