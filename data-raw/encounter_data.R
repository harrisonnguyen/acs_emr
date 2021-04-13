## code to prepare `encounter_data` dataset goes here

#pp <- pipetree::get_portrpath()
#pp$profile <- "5y_hpc"
#cache <- pipetree::get_cache()
#encounter_prepr <- pipetree::load_merged_partitions("encounter_prepr", cache = cache)$encounter_prepr
library(magrittr)
library(dplyr)
library(portrpaths)
library(pipetree)
options(pipetree.config="D:/projects/SPEED-EXTRACT.pipeline/local.yaml")
pp <- pipetree::get_portrpath()

pp$profile <- "3m_hpc"
#### get the cache that we can use with drake ####
cache <- pipetree::get_cache()

encounter_prepr <- pipetree::load_merged_partitions(encounter_prepr,cache=cache)$encounter_prepr

#encounter_prepr <- read.csv(here::here("data-raw", "encounter_prepr.csv"), stringsAsFactors = FALSE)

encounter_data <- encounter_prepr %>%
  dplyr::filter(admit_dttm > as.POSIXct("2016-05-01") &
                  admit_dttm < as.POSIXct("2018-04-30") &
                  facility == 'Royal North Shore'
  )
#save(encounter_data,file="data/encounter_data.rda")
usethis::use_data(encounter_data, overwrite = TRUE)
