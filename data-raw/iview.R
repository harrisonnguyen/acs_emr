## code to prepare `iview` dataset goes here
library(magrittr)
options(pipetree.config="D:/projects/SPEED-EXTRACT.pipeline/local.yaml")
pp <- pipetree::get_portrpath()

pp$profile <- "3m_hpc"
#### get the cache that we can use with drake ####
cache <- pipetree::get_cache()

iview_cache <- pipetree::load_merged_partitions(iview_prepr,cache=cache)$iview_prepr

iview <- iview_cache %>%
  dplyr::as_tibble() %>%
  dplyr::rename_all(toupper) %>%
  dplyr::ungroup()

# extract blood pressure observations
iview_bp <- iview %>%
  dplyr::select(ENCNTR_KEY, WORKING_VIEW_DISPLAY, EVENT_SET_DISPLAY, IVIEW_RESULT, IVIEW_RESULT_DTTM) %>%
  dplyr::mutate(across(where(is.factor), as.character)) %>%
  dplyr::filter(stringr::str_detect(EVENT_SET_DISPLAY, "Systolic|Diastolic")) %>%
  dplyr::distinct() %>%
  dplyr::mutate(IVIEW_RESULT =readr::parse_number(IVIEW_RESULT))

# extract glucose observations
iview_glu <- iview %>%
  dplyr::select(ENCNTR_KEY, WORKING_VIEW_DISPLAY, EVENT_SET_DISPLAY, IVIEW_RESULT, IVIEW_RESULT_DTTM) %>%
  dplyr::mutate(across(where(is.factor), as.character)) %>%
  dplyr::filter(stringr::str_detect(EVENT_SET_DISPLAY, "Blood Glucose Level Bedside")) %>%
  dplyr::distinct() %>%
  dplyr::mutate(IVIEW_RESULT = readr::parse_number(IVIEW_RESULT))


usethis::use_data(iview_bp, overwrite = TRUE)
usethis::use_data(iview_glu, overwrite = TRUE)
