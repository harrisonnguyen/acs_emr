## code to prepare `acs_door_to_balloon` dataset goes here
library(portrpaths)
library(pipetree)
options(pipetree.config="D:/projects/SPEED-EXTRACT.pipeline/local.yaml")
pp <- pipetree::get_portrpath()

pp$profile <- "3m_hpc"
#### get the cache that we can use with drake ####
cache <- pipetree::get_cache()

door_to_balloon_cache <- pipetree::load_merged_partitions(door_to_balloon,cache=cache)$door_to_balloon

data(journey_acs)
acs_door_to_balloon <- door_to_balloon_cache %>%
  #dplyr::filter(JOURNEY_KEY %in% journey_acs$JOURNEY_KEY) %>%
  dplyr::mutate(procedure_status = sapply(procedure_status,toString)) %>%
  dplyr::mutate_if(is.character, list(~dplyr::na_if(.,"")))


usethis::use_data(acs_door_to_balloon, overwrite = TRUE)
