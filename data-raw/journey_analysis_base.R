## code to prepare `journey_analysis_base` dataset goes here

journey_analysis_base <-read.csv(here::here("data-raw", "journey_analysis_base.csv"), stringsAsFactors = FALSE)
usethis::use_data(journey_analysis_base, overwrite = TRUE)
