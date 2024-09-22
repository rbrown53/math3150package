## code to prepare `crime` dataset goes here
library(tidyverse) |> suppressPackageStartupMessages()
crime <- read.csv("data-raw/crime.csv") |>
  as_tibble()
usethis::use_data(crime, overwrite = TRUE)
