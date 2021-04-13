## code to prepare `acs_journey_meds_admit_discharge` dataset goes here
library(portrpaths)
library(pipetree)
library(tidyr)
options(pipetree.config="D:/projects/SPEED-EXTRACT.pipeline/local.yaml")
pp <- pipetree::get_portrpath()

pp$profile <- "3m_hpc"
#### get the cache that we can use with drake ####
cache <- pipetree::get_cache()

journey_meds_admit_discharge_cache <- pipetree::load_merged_partitions(journey_meds_admit_discharge,cache=cache)$journey_meds_admit_discharge

data(journey_acs)
acs_journey_meds_admit_discharge <- journey_meds_admit_discharge_cache %>%
  dplyr::rename_all(toupper) %>%
  dplyr::filter(ENCNTR_KEY %in% journey_acs$encntr_key) %>%
  dplyr::select(JOURNEY_KEY,
         ENCNTR_KEY,
         MEDICATION_NAME, # Generic name (Maps trade names to generic name)
         MEDICATION = GROUPS,
         FORM_DTTM,
         ADMIT_MED,
         DISCHARGE_MED) %>%
  tidyr::unnest(MEDICATION, keep_empty = TRUE) %>%
  dplyr::distinct()

usethis::use_data(acs_journey_meds_admit_discharge, overwrite = TRUE)
