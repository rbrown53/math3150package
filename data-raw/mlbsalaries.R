## code to prepare `mlbsalaries` dataset goes here
library(tidyverse) |> suppressPackageStartupMessages()
mlbsalaries <- read.csv("data-raw/mlbsalaries.txt") |>
  as_tibble() |>
  mutate(Position = factor(Position, 
                           levels = c("SP", "C", "1B", "2B", "3B", 
                                      "SS", "OF", "RP", "DH"))
         )
usethis::use_data(mlbsalaries, overwrite = TRUE)
