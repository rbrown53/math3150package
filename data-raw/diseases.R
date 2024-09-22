## code to prepare `diseases` dataset goes here
library(tidyverse) |> suppressPackageStartupMessages()
diseases <- read.csv("data-raw/diseases.txt") |>
  as_tibble()
usethis::use_data(diseases, overwrite = TRUE)
