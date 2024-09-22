## code to prepare `biomass` dataset goes here
library(tidyverse) |> suppressPackageStartupMessages()
biomass <- read.csv("data-raw/biomass.txt") |>
  as_tibble()
usethis::use_data(biomass, overwrite = TRUE)
