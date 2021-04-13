## code to prepare `acs_procedures` dataset goes here
library(portrpaths)
library(pipetree)
options(pipetree.config="D:/projects/SPEED-EXTRACT.pipeline/local.yaml")
pp <- pipetree::get_portrpath()

pp$profile <- "3m_hpc"
#### get the cache that we can use with drake ####
cache <- pipetree::get_cache()

procedures_grouped_cache <- pipetree::load_merged_partitions(procedures_grouped,cache=cache)$procedures_grouped

data(journey_acs)

acs_procedures <- procedures_grouped_cache %>%
  dplyr::filter(encntr_key %in% journey_acs$encntr_key) %>%
  dplyr::mutate(procedure_name_groups = sapply(procedure_name_groups,toString),
                procedure_name_keywords = sapply(procedure_name_keywords,toString)) %>%
  dplyr::mutate_if(is.character, list(~dplyr::na_if(.,""))) %>%
  dplyr::mutate(
    cabg = stringr::str_detect(procedure_name,"(?i)coronary artery bypass")) %>%
  dplyr::filter(angiogram | PCI | cabg)


usethis::use_data(acs_procedures, overwrite = TRUE)
