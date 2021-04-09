## code to prepare `diagnosis_data` dataset goes here

#pp <- pipetree::get_portrpath()
#pp$profile <- "5y_hpc"
#cache <- pipetree::get_cache()
#diagnosis <- pipetree::load_merged_partitions("diagosis_prepr", cache = cache)$diagosis_prepr
library(magrittr)
library(dplyr)

diagnosis_prepr <- read.csv(here::here("data-raw", "diagnosis_prepr.csv"), stringsAsFactors = FALSE, row.names=1)

## use the previously cleaned encounter data
load(file= "data/encounter_data.rda")

diagnosis_data <- diagnosis_prepr %>%
  dplyr::filter(encntr_key %in% unique(encounter_data$encntr_key))

usethis::use_data(diagnosis_data, overwrite = TRUE)
