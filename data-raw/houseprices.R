## code to prepare `houseprices` dataset goes here
library(tidyverse) |> suppressPackageStartupMessages()
houseprices <- read.csv("data-raw/houseprices.txt") |>
  as_tibble()
usethis::use_data(houseprices, overwrite = TRUE)
