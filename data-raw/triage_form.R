## code to prepare `triage_form` dataset goes here

library(pipetree)
library(portrpaths)
library(magrittr)
#options(pipetree.config = "/media/harrison/ShortTerm/Users/HarrisonG/research/SPEED-EXTRACT.pipeline/local.yaml")
#pp <- pipetree::get_portrpath()
#pp$profile <- "5y_hpc"
#cache <- pipetree::get_cache()
options(pipetree.config = "D:/projects/SPEED-EXTRACT.pipeline/local.yaml")
pp <- pipetree::get_portrpath()

form_triage_classified<- pipetree::load_merged_partitions("form_triage_classified",cache=cache)$form_triage_classified

triage_form <- form_triage_classified %>%
  dplyr::rename_all(toupper) %>%
  dplyr::mutate(
    ATAMI = stringr::str_detect(CHARTED_VALUE, stringr::regex("atami|etami|cath lab|cathlab", ignore_case = T))
  ) %>%
  dplyr::filter(ATAMI)


usethis::use_data(triage_form, overwrite = TRUE)
