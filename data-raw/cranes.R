## code to prepare `cranes` dataset goes here
library(tidyverse) |> suppressPackageStartupMessages()
cranes <- read.csv("data-raw/cranes.txt") |>
  as_tibble()
usethis::use_data(cranes, overwrite = TRUE)
