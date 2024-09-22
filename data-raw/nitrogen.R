## code to prepare `nitrogen` dataset goes here
nitrogen <- read.csv("data-raw/nitrogen.txt")
usethis::use_data(nitrogen, overwrite = TRUE)
