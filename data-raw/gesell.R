## code to prepare `gesell` dataset goes here
library(tidyverse) |> suppressPackageStartupMessages()
gesell <- read.csv("data-raw/gesell.txt") |>
  as_tibble()
usethis::use_data(gesell, overwrite = TRUE)
