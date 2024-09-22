## code to prepare `pulse` dataset goes here
library(tidyverse) |> suppressPackageStartupMessages()
pulse <- read.csv("data-raw/pulse.csv") |>
  as_tibble() |>
  mutate(smoke = factor(smoke)) |>
  rename(subject = X)
usethis::use_data(pulse, overwrite = TRUE)
