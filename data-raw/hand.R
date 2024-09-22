## code to prepare `hand` dataset goes here
hand <- read.csv("data-raw/hand.txt") |>
  as_tibble() |>
  mutate(sex = factor(sex))
usethis::use_data(hand, overwrite = TRUE)
