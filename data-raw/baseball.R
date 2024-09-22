## code to prepare `baseball` dataset goes here
library(tidyverse) |> suppressPackageStartupMessages()
baseball <- read.table("data-raw/baseball.dat", header = T) |>
  as_tibble()
usethis::use_data(baseball, overwrite = TRUE)
