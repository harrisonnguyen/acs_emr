## code to prepare `acs_discharge_letter_keys` dataset goes here
library(portrpaths)
library(pipetree)
library(tidyr)
options(pipetree.config="D:/projects/SPEED-EXTRACT.pipeline/local.yaml")
pp <- pipetree::get_portrpath()

pp$profile <- "3m_hpc"
#### get the cache that we can use with drake ####
cache <- pipetree::get_cache()
discharge_letter_keys <- pipetree::load_merged_partitions(discharge_letter_keys,cache=cache)$discharge_letter_keys

usethis::use_data(discharge_letter_keys, overwrite = TRUE)
