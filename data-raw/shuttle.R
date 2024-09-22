## code to prepare `shuttle` dataset goes here
library(tidyverse) |> suppressPackageStartupMessages()
shuttle <- read.csv("data-raw/shuttle.txt") |>
  as_tibble()
usethis::use_data(shuttle, overwrite = TRUE)
