## code to prepare `pathology` dataset goes here
library(magrittr)
options(pipetree.config="D:/projects/SPEED-EXTRACT.pipeline/local.yaml")
pp <- pipetree::get_portrpath()

pp$profile <- "3m_hpc"
#### get the cache that we can use with drake ####
cache <- pipetree::get_cache()

patho_fasting_cache <- pipetree::load_merged_partitions(patho_fasting,cache=cache)$patho_fasting

pathology <- patho_fasting_cache %>%
  dplyr::as_tibble() %>%
  dplyr::rename_all(toupper) %>%
  dplyr::ungroup() %>%
  dplyr::filter(
    stringr::str_detect(RESULT_DISPLAY, "Glucose \\(Rand") |
      RESULT_DISPLAY %in% c("HDL Cholesterol", "LDL Cholesterol", "Cholesterol",
                            "Triglycerides", "HbA1c (NGSP)")
  ) %>%
  dplyr::filter(RESULT_TAG != "Error") %>%
  dplyr::select(
    ENCNTR_KEY,
    RESULT_START_DTTM,
    RESULT_DISPLAY,
    RESULT_TAG,
    NORMAL_LOW,
    NORMAL_HIGH,
    FASTING_TEST
  ) %>%
  dplyr::mutate(
    across(where(is.factor), as.character),
    across(c(RESULT_TAG, NORMAL_LOW, NORMAL_HIGH), tidyr::extract_numeric)
  ) %>%
  dplyr::distinct()
usethis::use_data(pathology, overwrite = TRUE)
