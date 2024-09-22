## code to prepare `satscores` dataset goes here
library(tidyverse) |> suppressPackageStartupMessages()
satscores <- read.csv("data-raw/satscores.txt") |>
  as_tibble()
usethis::use_data(satscores, overwrite = TRUE)
