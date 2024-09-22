## code to prepare `pHdevices` dataset goes here
library(tidyverse) |> suppressPackageStartupMessages()
pHdevices <- read.csv("data-raw/pHdevices.txt") |>
  as_tibble() |>
  mutate(device = factor(device))
usethis::use_data(pHdevices, overwrite = TRUE)
