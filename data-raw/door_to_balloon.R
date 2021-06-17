## code to prepare `acs_door_to_balloon` dataset goes here
library(portrpaths)
library(pipetree)
options(pipetree.config="D:/projects/SPEED-EXTRACT.pipeline/local.yaml")
pp <- pipetree::get_portrpath()

pp$profile <- "3m_hpc"
#### get the cache that we can use with drake ####
cache <- pipetree::get_cache()

door_to_balloon <- pipetree::load_merged_partitions(door_to_balloon,cache=cache)$door_to_balloon
usethis::use_data(door_to_balloon, overwrite = TRUE)





