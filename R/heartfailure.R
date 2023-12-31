
#' Extracts heart failure cohort
#'
#' Extracts the journeys with ICD10 heart failure diagnosis
#' with `JOURNEY_START` between `journey_start_begin` and `journey_start_end`
#'
#' @seealso \code{\link{create_hf_snomed_encounters}} for selection of encounter cohort
#' based on SNOMED codes.
#' @param journey a df containing journeys
#' @param diagnosis a df containing diagnosis
#' @param journey_start_begin start date for the cohort
#' @param journey_start_end end date
#' @family heart failure
select_hf_cohort <- function(journey,diagnosis,
                             journey_start_begin = "2017-04-01",
                             journey_start_end = "2017-06-30"){
  NSLHD <- NSLHD_facility()
  internal_transfers = internal_transfers()
  icd10_list <- hf_icd_codes()$codes

  journey_start_begin <- as.POSIXct(journey_start_begin)
  journey_start_end <- as.POSIXct(journey_start_end)

  hf_diag <- diagnosis %>%
    dplyr::filter(DISPLAY_IDENTIFIER_CAP %in% icd10_list &
                    N_SOURCE_VOCABULARY_DISP == "ICD10-AM" & (DIAGNOSIS_IMPORTANCE == 'primary_diagnosis' |
                                                                DIAGNOSIS_IMPORTANCE == 'secondary_diagnosis')) %>%
    dplyr::select(ENCNTR_KEY,SOURCE_STRING_CAP,DISPLAY_IDENTIFIER_CAP,DIAGNOSIS_IMPORTANCE)

  journey_hf <- journey %>%
    dplyr::filter(JOURNEY_START > journey_start_begin & # filter the two years
                    JOURNEY_START < journey_start_end) %>%
    #dplyr::filter(
    #  any(ENCLASS == "ED.Admitted" & FACILITY %in% NSLHD & !(ADMIT_MODE %in% internal_transfers), na.rm=T) |
    #    (dplyr::first(ENCLASS, order_by = ADMIT_DTTM) %in% c("ED.Only", "ED.Admitted") &
    #       (any(ENCLASS == "Transfer" & FACILITY == "Royal North Shore")))
    #) %>%
    dplyr::filter(ENCNTR_KEY %in% hf_diag$ENCNTR_KEY) %>%
    dplyr::left_join(hf_diag,by='ENCNTR_KEY') %>%
    dplyr::select(PERSON_KEY, JOURNEY_KEY, ENCNTR_KEY, GENDER, AGE_AT_ADMIT, FACILITY,
                  ADMIT_MODE, ADMIT_DTTM, DISCHARGE_DTTM, JOURNEY_START,JOURNEY_END, JOURNEY_FIRST_FACILITY,
                  JOURNEY_FINAL_FACILITY, JOURNEY_DAYS, JOURNEY_SEP_MODE,SOURCE_STRING_CAP,DISPLAY_IDENTIFIER_CAP,DIAGNOSIS_IMPORTANCE,ENCLASS) %>%
    dplyr::distinct()
}

#' Extracts echocardiogram procedures
#'
#' Extracts echo procedures based on a string search for the specified cohort
#' @param journey a df containing journeys
#' @param procedures a df containing procedures
#' @family heart failure
extract_echo_procedures <- function(journeys,procedures){
  echos<- dplyr::filter(procedures, stringr::str_detect(PROCEDURE_NAME,"(?i)echocardiogram")) %>%
    dplyr::filter(ENCNTR_KEY %in% journeys$ENCNTR_KEY) %>%
    dplyr::select(ENCNTR_KEY,PROCEDURE_NAME,PROCEDURE_DTTM) %>%
    dplyr::left_join(
      dplyr::select(journeys,ADMIT_DTTM,ENCNTR_KEY),by="ENCNTR_KEY") %>%
    dplyr::mutate(PROCEDURE_ADMIT_TIME_DELTA = difftime(PROCEDURE_DTTM,ADMIT_DTTM))
}

#' Extracts SNOMED heart failure encounters
#'
#' Extracts the encounters with SNOMED heart failure diagnosis
#' with `JOURNEY_START` between `journey_start_begin` and `journey_start_end`
#'
#' @seealso \code{\link{select_hf_cohort}} for selection of journey cohort
#' based on ICD10 codes.
#'
#' @param journey a df containing journeys
#' @param diagnosis a df containing diagnosis
#' @param journey_start_begin start date for the cohort
#' @param journey_start_end end date
#' @family heart failure
create_hf_snomed_encounters <- function(encounters,
                                           diagnosis,
                                           journey_start_begin = "2017-04-01",
                                           journey_start_end = "2017-06-30"){

  journey_start_begin <- as.POSIXct(journey_start_begin)
  journey_start_end <- as.POSIXct(journey_start_end)

  encounters %<>% dplyr::filter(ADMIT_DTTM > journey_start_begin & # filter the two years
                                  ADMIT_DTTM < journey_start_end)

  hf_snomed <- "(?i)(heart|ventricular) failure"

  snomed_journeys <-  diagnosis %>%
    dplyr::filter(CONFIRMATION_STATUS == "Confirmed" & (CLASSIFICATION == "ED Medical" | CLASSIFICATION == "Medical" | CLASSIFICATION == "ED Nursing")) %>%
    dplyr::filter(N_SOURCE_VOCABULARY_DISP == "SNOMED CT") %>%
    dplyr::filter(stringr::str_detect(SOURCE_STRING_CAP, hf_snomed)) %>%
    dplyr::select(ENCNTR_KEY,SOURCE_STRING_CAP) %>%
    dplyr::inner_join(encounters, by ="ENCNTR_KEY")

}

#' Extracts ICD10 heart failure encounters
#'
#' Extracts the encounters with SNOMED heart failure diagnosis
#' with `JOURNEY_START` between `journey_start_begin` and `journey_start_end`
#'
#' @seealso \code{\link{select_hf_cohort}} for selection of journey cohort
#' based on ICD10 codes, \code{\link{create_hf_snomed_encounters}} for selection
#' based on SNOMED codes
#'
#' @param journey a df containing journeys
#' @param diagnosis a df containing diagnosis
#' @param journey_start_begin start date for the cohort
#' @param journey_start_end end date
#' @family heart failure
create_hf_icd10_encounters <- function(encounters,
                                        diagnosis,
                                        journey_start_begin = "2017-04-01",
                                        journey_start_end = "2017-06-30"){

  journey_start_begin <- as.POSIXct(journey_start_begin)
  journey_start_end <- as.POSIXct(journey_start_end)

  encounters %<>% dplyr::filter(ADMIT_DTTM > journey_start_begin & # filter the two years
                                  ADMIT_DTTM < journey_start_end)

  icd10_list <- hf_icd_codes()$codes

  diagnosis %>%
    dplyr::filter(DISPLAY_IDENTIFIER_CAP %in% icd10_list &
                    N_SOURCE_VOCABULARY_DISP == "ICD10-AM" & (DIAGNOSIS_IMPORTANCE == 'primary_diagnosis')) %>%
    dplyr::select(ENCNTR_KEY,SOURCE_STRING_CAP,DISPLAY_IDENTIFIER_CAP,DIAGNOSIS_IMPORTANCE) %>%
    dplyr::inner_join(encounters, by ="ENCNTR_KEY")

}

#' Creates discharge medication
#'
#' Summarises discharge medications for ACS cohort. In particular, summarises
#' `Diuretic`,`A2RB`,`Beta blockers` and `Beta blocker`
#' for each journey.
#'
#' It excludes journeys that meet the exclusion criteria, see \code{\link{exclusion_criteria_meds}}
#' and includes journeys with a discharge letter key.
#'
#' @param meds a df containing medication
#' @param journeys a df containing journeys
#' @param discharge_letter_keys a df containing keys of encounters with discharge letters
#' @return a df summarising the counts of the specified medication for each journey
#'
#' @seealso \code{\link{create_meds_discharge}} for acs medication
#'
#' @family heart failure
create_hf_meds_discharge <- function(journeys,meds,discharge_letter_keys){

  antiplatelets = c("P2Y12 receptor blocker", "Other antiplatelet")
  exclusions <- exclusion_criteria_meds()

  meds_sum <- meds %>%
    dplyr::group_by(JOURNEY_KEY) %>%
    dplyr::summarise(
      letter = any(ENCNTR_KEY %in% discharge_letter_keys),
      `Other diuretic` = sum(MEDICATION == "Other diuretic" & DISCHARGE_MED, na.rm = TRUE),
      `A2RB/ACE Inhibitor` = sum(MEDICATION == "A2RB/ACE Inhibitor" & DISCHARGE_MED, na.rm = TRUE),
      `Beta blocker` = sum(MEDICATION == "Beta blocker" & DISCHARGE_MED, na.rm = TRUE)
    ) %>%
    dplyr::ungroup()

  meds_letter <- journeys %>%
    dplyr::select(JOURNEY_KEY, JOURNEY_SEP_MODE) %>%
    dplyr::distinct() %>%
    dplyr::left_join(meds_sum, by = "JOURNEY_KEY") %>%
    dplyr::filter(letter)

  meds_discharge_sum <-  meds_letter %>%
    dplyr::filter(letter & !JOURNEY_SEP_MODE %in% exclusions)



  return(meds_discharge_sum)
}
