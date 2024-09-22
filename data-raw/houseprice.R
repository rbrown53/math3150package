## code to prepare `houseprice` dataset goes here
houseprice <- read.csv("data-raw/houseprice.txt") |>
  as_tibble()
usethis::use_data(houseprice, overwrite = TRUE)
