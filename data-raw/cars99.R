## code to prepare `cars99` dataset goes here
library(tidyverse) |> suppressPackageStartupMessages()
cars99 <- read.csv("data-raw/cars99.txt") |>
  as_tibble() |>
  select(!PageNum)
usethis::use_data(cars99, overwrite = TRUE)
