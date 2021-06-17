## code to prepare `acs_procedures` dataset goes here
library(portrpaths)
library(pipetree)
options(pipetree.config="D:/projects/SPEED-EXTRACT.pipeline/local.yaml")
pp <- pipetree::get_portrpath()

pp$profile <- "3m_hpc"
#### get the cache that we can use with drake ####
cache <- pipetree::get_cache()

procedures_grouped <- pipetree::load_merged_partitions(procedures_grouped,cache=cache)$procedures_grouped

usethis::use_data(procedures_grouped, overwrite = TRUE)
