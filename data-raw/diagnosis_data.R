## code to prepare `diagnosis_data` dataset goes here

#pp <- pipetree::get_portrpath()
#pp$profile <- "5y_hpc"
#cache <- pipetree::get_cache()
#diagnosis <- pipetree::load_merged_partitions("diagosis_prepr", cache = cache)$diagosis_prepr
library(magrittr)
library(dplyr)
library(portrpaths)
library(pipetree)
options(pipetree.config="D:/projects/SPEED-EXTRACT.pipeline/local.yaml")
pp <- pipetree::get_portrpath()

pp$profile <- "3m_hpc"
#### get the cache that we can use with drake ####
cache <- pipetree::get_cache()

diagnosis_prepr <- pipetree::load_merged_partitions(diagnosis_prepr,cache=cache)$diagnosis_prepr

#diagnosis_prepr <- read.csv(here::here("data-raw", "diagnosis_prepr.csv"), stringsAsFactors = FALSE, row.names=1)

## use the previously cleaned encounter data
data(encounter_data)

diagnosis_data <- diagnosis_prepr %>%
  dplyr::filter(encntr_key %in% unique(encounter_data$encntr_key))

usethis::use_data(diagnosis_data, overwrite = TRUE)
