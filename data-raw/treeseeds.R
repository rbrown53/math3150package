## code to prepare `treeseeds` dataset goes here
library(tidyverse) |> suppressPackageStartupMessages()
treeseeds <- read.csv("data-raw/treeseeds.txt") |>
  as_tibble()
usethis::use_data(treeseeds, overwrite = TRUE)
