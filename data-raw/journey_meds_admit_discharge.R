## code to prepare `acs_journey_meds_admit_discharge` dataset goes here
library(portrpaths)
library(pipetree)
library(tidyr)
options(pipetree.config="D:/projects/SPEED-EXTRACT.pipeline/local.yaml")
pp <- pipetree::get_portrpath()

pp$profile <- "3m_hpc"
#### get the cache that we can use with drake ####
cache <- pipetree::get_cache()

journey_meds_admit_discharge <- pipetree::load_merged_partitions(journey_meds_admit_discharge,cache=cache)$journey_meds_admit_discharge
usethis::use_data(journey_meds_admit_discharge, overwrite = TRUE)
