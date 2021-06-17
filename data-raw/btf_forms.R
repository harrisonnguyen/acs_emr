## code to prepare `btf_forms` dataset goes here
library(magrittr)
#### BTF forms ####
options(pipetree.config="D:/projects/SPEED-EXTRACT.pipeline/local.yaml")
pp <- pipetree::get_portrpath()

pp$profile <- "3m_hpc"
#### get the cache that we can use with drake ####
cache <- pipetree::get_cache()

btf_forms_cache <- pipetree::load_merged_partitions(form_btf_vital,cache=cache)$form_btf_vital

btf_forms <- btf_forms_cache %>%
  dplyr::as_tibble() %>%
  dplyr::rename_all(toupper) %>%
  dplyr::ungroup() # 65,359,223 rows

# extract glucose observations
glucose_forms <- btf_forms %>%
  dplyr::filter(LABEL %in% "Blood Glucose Level, Bedside") %>%
  dplyr::select(
    ENCNTR_KEY,
    FORM_NAME,
    SECTION_NAME,
    CHARTED_VALUE,
    LABEL,
    FORM_MODIF_DTTM
  ) %>%
  dplyr::mutate(
    across(where(is.factor), as.character),
    CHARTED_VALUE = readr::parse_number(CHARTED_VALUE)) %>%
  dplyr::distinct()


# extract blood pressure obs.
bp_forms <- btf_forms %>%
  dplyr::filter(stringr::str_detect(LABEL, "Systolic|Diastolic")) %>%
  dplyr::select(
    ENCNTR_KEY,
    FORM_NAME,
    SECTION_NAME,
    CHARTED_VALUE,
    LABEL,
    FORM_MODIF_DTTM
  ) %>%
  dplyr::mutate(
    across(where(is.factor), as.character),
    CHARTED_VALUE = readr::parse_number(CHARTED_VALUE)) %>%
  dplyr::distinct()



usethis::use_data(glucose_forms, overwrite = TRUE)
usethis::use_data(bp_forms, overwrite = TRUE)
