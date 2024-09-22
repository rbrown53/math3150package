## code to prepare `class_data_f2019` dataset goes here
library(tidyverse) |> suppressPackageStartupMessages()
class_data_f2019 <- read.csv("data-raw/class_data_f2019.csv") |>
  as_tibble() |>
  mutate(level = factor(level, levels = c("Freshman", "Sophomore", "Junior",
                                          "Senior", "Graduate")),
         sex = factor(sex, levels = c("F", "M")),
         ski = factor(ski, level = c("Ski", "Snowboard", "Neither")),
         penny = as_factor(penny))
usethis::use_data(class_data_f2019, overwrite = TRUE)
