## code to prepare `encounter_data` dataset goes here

#pp <- pipetree::get_portrpath()
#pp$profile <- "5y_hpc"
#cache <- pipetree::get_cache()
#encounter_prepr <- pipetree::load_merged_partitions("encounter_prepr", cache = cache)$encounter_prepr
library(magrittr)
library(dplyr)
encounter_file <- system.file(
  "extdata",
  "encounter_prepr.csv",
  package = "acs"
)

encounter_prepr <- read.csv(encounter_file, stringsAsFactors = FALSE)

encounter_data <- encounter_prepr %>%
  dplyr::filter(admit_dttm > as.POSIXct("2016-05-01") &
                  admit_dttm < as.POSIXct("2018-04-30") &
                  facility == 'Royal North Shore'
  )
#save(encounter_data,file="data/encounter_data.rda")
usethis::use_data(encounter_data, overwrite = TRUE)
