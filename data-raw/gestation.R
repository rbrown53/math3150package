## code to prepare `gestation` dataset goes here
library(tidyverse) |> suppressPackageStartupMessages()
gestation <- read.csv("data-raw/gestation.txt") |>
  as_tibble()
usethis::use_data(gestation, overwrite = TRUE)
