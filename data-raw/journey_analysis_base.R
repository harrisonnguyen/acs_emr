## code to prepare `journey_analysis_base` dataset goes here
library(portrpaths)
library(pipetree)
library(magrittr)

#journey_analysis_base <-read.csv(here::here("data-raw", "journey_analysis_base.csv"), stringsAsFactors = FALSE)

## this will change depending on machine
options(pipetree.config="D:/projects/SPEED-EXTRACT.pipeline/local.yaml")
pp <- pipetree::get_portrpath()

pp$profile <- "3m_hpc"
#### get the cache that we can use with drake ####
cache <- pipetree::get_cache()

journey_analysis_base <- pipetree::load_merged_partitions(journey_analysis_base,cache=cache)$journey_analysis_base
#journey_analysis_base <- journey_analysis_base_cache %>%
#  dplyr::mutate(icd10_diagnosis_list = sapply(icd10_diagnosis_list,toString)) %>%
#  dplyr::mutate_if(is.character, list(~dplyr::na_if(.,""))) %>%
#  dplyr::rename_all(toupper)

#journey_analysis_base <-read.csv(here::here("data-raw", "journey_analysis_base.csv"), stringsAsFactors = FALSE)
#usethis::use_data(journey_analysis_base, overwrite = TRUE)

usethis::use_data(journey_analysis_base, overwrite = TRUE)

NSLHD <- c("Royal North Shore", "Hornsby", "Ryde", "Manly", "Mona Vale")
CCLHD <- c("Gosford", "Woy Woy", "Wyong", "Long Jetty")

is.ACS <- function(.x) {
  any(.x %in% c("STEMI", "NSTEMI", "UNSTABLE_ANGINA", "UNSPECIFIED_ACS"),
      na.rm=T)
}

internal_transfers = c(
  # "Internal Ambulance / Transport",
  # "Retrieval",
  "Internal Bed / Wheelchair"
)

journey_acs <- journey_analysis_base %>%
  dplyr::rename_all(toupper) %>%
  dplyr::filter(JOURNEY_START > as.POSIXct("2017-04-01") & # filter the two years
           JOURNEY_START < as.POSIXct("2017-06-30")) %>%
  dplyr::filter(purrr::map_lgl(ICD10_DIAGNOSIS_LIST, is.ACS)) %>% # filter the ACS episodes to reduce unlist operations
  dplyr::group_by(JOURNEY_KEY) %>%
  dplyr::mutate(
    ICD10_ACS = dplyr::case_when(
      any(unlist(ICD10_DIAGNOSIS_LIST) %in% "STEMI") ~ "STEMI",
      any(unlist(ICD10_DIAGNOSIS_LIST) %in% "NSTEMI") ~ "NSTEMI",
      any(unlist(ICD10_DIAGNOSIS_LIST) %in% "UNSTABLE_ANGINA") ~ "Unstable angina", # I20.0 Unstable angina
      any(unlist(ICD10_DIAGNOSIS_LIST) %in% "UNSPECIFIED_ACS") ~ "Unspecified ACS", # I21.9 Unspecified
      TRUE ~ "None")
  ) %>%
  dplyr::filter(
    any(ENCLASS == "ED.Admitted" & FACILITY %in% NSLHD & !(ADMIT_MODE %in% internal_transfers), na.rm=T) |
      (dplyr::first(ENCLASS, order_by = ADMIT_DTTM) %in% c("ED.Only", "ED.Admitted") &
         (any(ENCLASS == "Transfer" & FACILITY == "Royal North Shore")))
  ) %>%
  dplyr::select(-ICD10_DIAGNOSIS_LIST) %>%
  dplyr::ungroup()

journey_acs %<>%
  dplyr::filter(ICD10_ACS %in% c("STEMI", "NSTEMI")) %>% # "STEMI", "NSTEMI", "UA", "UNSPEC"
  dplyr::select(PERSON_KEY, JOURNEY_KEY, ENCNTR_KEY, GENDER, AGE_AT_ADMIT, FACILITY,
         ADMIT_MODE, ADMIT_DTTM, DISCHARGE_DTTM, JOURNEY_START,JOURNEY_END, JOURNEY_FIRST_FACILITY,
         JOURNEY_FINAL_FACILITY, JOURNEY_DAYS, JOURNEY_SEP_MODE,ICD10_ACS) %>%
  dplyr::distinct()

usethis::use_data(journey_acs, overwrite = TRUE)
