## code to prepare `sucrase` dataset goes here
library(tidyverse) |> suppressPackageStartupMessages()
sucrase <- read.csv("data-raw/sucrase.txt") |>
  as_tibble()
usethis::use_data(sucrase, overwrite = TRUE)
