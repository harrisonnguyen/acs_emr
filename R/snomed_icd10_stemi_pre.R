
extract_rns_encounters <- function(encounters,
                                   journey_start_begin = "2017-04-01",
                                   journey_start_end = "2017-06-30"){

  journey_start_begin <- as.POSIXct(journey_start_begin)
  journey_start_end <- as.POSIXct(journey_start_end)


  encounter_rns <- encounters %>%
    dplyr::filter(FACILITY == "Royal North Shore") %>%
    dplyr::filter(ADMIT_DTTM > journey_start_begin & # filter the two years
                    ADMIT_DTTM < journey_start_end)
}

extract_encounter_diagnosis <- function(encounters,diagnosis){
  diagnosis %>%
    dplyr::filter(ENCNTR_KEY %in% unique(encounters$ENCNTR_KEY))
}
