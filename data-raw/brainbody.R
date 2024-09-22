## code to prepare `brainbody` dataset goes here
library(tidyverse) |> suppressPackageStartupMessages()
brainbody <- read.csv("data-raw/brainbody.txt") |>
  as_tibble()
usethis::use_data(brainbody, overwrite = TRUE)
