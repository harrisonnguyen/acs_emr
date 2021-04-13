## code to prepare `journey_analysis_base` dataset goes here
library(portrpaths)
library(pipetree)

#journey_analysis_base <-read.csv(here::here("data-raw", "journey_analysis_base.csv"), stringsAsFactors = FALSE)

## this will change depending on machine
options(pipetree.config="D:/projects/SPEED-EXTRACT.pipeline/local.yaml")
pp <- pipetree::get_portrpath()

pp$profile <- "3m_hpc"
#### get the cache that we can use with drake ####
cache <- pipetree::get_cache()

journey_analysis_base_cache <- pipetree::load_merged_partitions(journey_analysis_base,cache=cache)$journey_analysis_base
journey_analysis_base <- journey_analysis_base_cache %>%
  dplyr::mutate(icd10_diagnosis_list = sapply(icd10_diagnosis_list,toString)) %>%
  dplyr::mutate_if(is.character, list(~dplyr::na_if(.,"")))

#journey_analysis_base <-read.csv(here::here("data-raw", "journey_analysis_base.csv"), stringsAsFactors = FALSE)
usethis::use_data(journey_analysis_base, overwrite = TRUE)

journey_acs <- journey_analysis_base %>%
  dplyr::filter(journey_above_normal_trop==TRUE)


usethis::use_data(journey_acs, overwrite = TRUE)
