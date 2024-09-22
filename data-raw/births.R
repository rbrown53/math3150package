## code to prepare `births` dataset goes here
library(tidyverse) |> suppressPackageStartupMessages()
births <- read.csv("data-raw/births.txt") |>
  as_tibble() |>
  mutate(prenatal = factor(prenatal, 
                           levels = c("Adequate", "Inadequate", "Intensive")),
         type = factor(type)
         )
usethis::use_data(births, overwrite = TRUE)
