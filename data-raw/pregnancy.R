## code to prepare `pregnancy` dataset goes here
library(tidyverse) |> suppressPackageStartupMessages()
pregnancy <- read.table("data-raw/pregnancy.txt", header = T) |>
  as_tibble()
usethis::use_data(pregnancy, overwrite = TRUE)
