source("R/definitions.R")

extract_journeys <- function(journeys,
                             journey_start_begin = "2017-04-01",
                             journey_start_end = "2017-06-30"){
  #original was "2016-05-01" "2018-04-30"
  journey_start_begin <- as.POSIXct(journey_start_begin)
  journey_start_end <- as.POSIXct(journey_start_end)

  journeys %<>%
    dplyr::group_by(JOURNEY_KEY) %>%
    dplyr::filter(
      any(ENCLASS == "ED.Admitted" & FACILITY %in% NSLHD_facility() & !(ADMIT_MODE %in% internal_transfers()), na.rm=T))%>%
    dplyr::ungroup() %>%
    dplyr::filter(ADMIT_DTTM > journey_start_begin &
                    ADMIT_DTTM < journey_start_end)
}


extract_snomed_diagnosis <- function(diagnosis){
  STEMI_SNOMED <- "(?i)acute( inferior | anterior |\\s)st segment elevation|(ST elevation myocardial infarction)|(Acute STEMI)"

  snomed <- diagnosis %>%
    dplyr::filter(CONFIRMATION_STATUS == "Confirmed" & (CLASSIFICATION == "ED Medical" | CLASSIFICATION == "Medical" | CLASSIFICATION == "ED Nursing")) %>%
    dplyr::filter(N_SOURCE_VOCABULARY_DISP == "SNOMED CT")

  snomed_stemi <- snomed %>%
    dplyr::filter(stringr::str_detect(SOURCE_STRING_CAP, STEMI_SNOMED))
}


extract_troponin_order <- function(encounter_order){
  encounter_order %>%
    dplyr::filter(stringr::str_detect(ORDER_MNEMONIC,'(?i)Troponin|Troponin I"|Troponin T')) %>%
    dplyr::select(-c("TEMPLATE_ORDER_KEY","CLINICAL_DISPLAY_LINE","DETAIL_DISPLAY_LINE","COMMUNICATION_TYPE","ORDERING_PHYSICIAN"))
}
