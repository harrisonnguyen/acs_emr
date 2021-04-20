## code to prepare `encounter_orders` dataset goes here
library(pipetree)
library(portrpaths)
library(magrittr)
#options(pipetree.config = "/media/harrison/ShortTerm/Users/HarrisonG/research/SPEED-EXTRACT.pipeline/local.yaml")
#pp <- pipetree::get_portrpath()
#pp$profile <- "5y_hpc"
#cache <- pipetree::get_cache()
options(pipetree.config = "D:/projects/SPEED-EXTRACT.pipeline/local.yaml")
pp <- pipetree::get_portrpath()
encounter_order<- pipetree::load_merged_partitions("encounter_order_prepr",cache=cache)$encounter_order_prepr

#load(file.path(here::here('data-raw'),'encounter_order.rda'))

out <-encounter_order %>%
  dplyr::filter(stringr::str_detect(order_mnemonic,'(?i)Troponin|Troponin I"|Troponin T')) %>%
  dplyr::rename_with(toupper) %>%
  dplyr::select(-c("TEMPLATE_ORDER_KEY","CLINICAL_DISPLAY_LINE","DETAIL_DISPLAY_LINE","COMMUNICATION_TYPE","ORDERING_PHYSICIAN"))


troponin_encounter_order <- out

usethis::use_data(troponin_encounter_order, overwrite = TRUE)
