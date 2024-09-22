## code to prepare `heartwine` dataset goes here
library(tidyverse) |> suppressPackageStartupMessages()
heartwine <- read.csv("data-raw/heartwine.txt") |>
  as_tibble()
usethis::use_data(heartwine, overwrite = TRUE)
