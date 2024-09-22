## code to prepare `moths` dataset goes here
library(tidyverse) |> suppressPackageStartupMessages()
moths <- read.csv("data-raw/moths.csv") |>
  as_tibble() |>
  mutate(Location = factor(Location, 
                           levels = c("Ground", "Lower", "Middle", "Top"))
         )
usethis::use_data(moths, overwrite = TRUE)
