## code to prepare `realtor` dataset goes here
library(tidyverse) |> suppressPackageStartupMessages()
realtor <- read.csv("data-raw/realtor.txt") |>
  as_tibble()
usethis::use_data(realtor, overwrite = TRUE)
