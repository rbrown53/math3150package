## code to prepare `sat` dataset goes here
library(tidyverse) |> suppressPackageStartupMessages()
sat <- read.csv("data-raw/sat.txt") |>
  as_tibble()
usethis::use_data(sat, overwrite = TRUE)
