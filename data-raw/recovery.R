## code to prepare `recovery` dataset goes here
library(tidyverse) |> suppressPackageStartupMessages()
recovery <- read.csv("data-raw/recovery.txt") |>
  as_tibble()
usethis::use_data(recovery, overwrite = TRUE)
