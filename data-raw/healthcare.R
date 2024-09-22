## code to prepare `healthcare` dataset goes here
library(tidyverse) |> suppressPackageStartupMessages()
healthcare <- read.csv("data-raw/healthcare.txt")
usethis::use_data(healthcare, overwrite = TRUE)
