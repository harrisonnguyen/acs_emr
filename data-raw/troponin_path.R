## code to prepare `troponin_path` dataset goes here
library(pipetree)
library(portrpaths)
library(magrittr)
#options(pipetree.config = "/media/harrison/ShortTerm/Users/HarrisonG/research/SPEED-EXTRACT.pipeline/local.yaml")
#pp <- pipetree::get_portrpath()
#pp$profile <- "5y_hpc"
#cache <- pipetree::get_cache()
options(pipetree.config = "D:/projects/SPEED-EXTRACT.pipeline/local.yaml")
pp <- pipetree::get_portrpath()
patho_categorized<- pipetree::load_merged_partitions("patho_categorized",cache=cache)$patho_categorized

troponin_path <- patho_categorized %>%
  dplyr::filter(pathology_groups == "cardiology") %>%
  dplyr::filter(stringr::str_detect(result_display, "(?i)Troponin")) %>%
  dplyr::rename_all(toupper) %>%
  dplyr::select(
    ENCNTR_KEY,
    CLINICAL_EVENT_KEY,
    ORDER_KEY,
    CLINSIG_DTTM,
    RESULT_DISPLAY,
    RESULT_END_DTTM,
    RESULT_DISCRETE_VALUE,
    NORMAL_HIGH)
usethis::use_data(troponin_path, overwrite = TRUE)
