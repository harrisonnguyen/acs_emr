source("R/definitions.R")
#' Extracts the ACS cohort
#'
#' The cohort is defined as patients with a journey start date
#' between `journey_start_begin` and `journey_start_end`
#' and those with ICD10 STEMI/NSTEMI code.
#' It includes those who were admitted into ED OR patients who had transferred to RNSH
#' @param journey_analysis_base, a dataframe containing a list of encounters with their journey_keys
#' @param journey_start_begin a date time string
#' @param journey_start_end a date time string
#' @return a dataframe containing the ACS journeys
#' @family acs
select_acs_cohort <- function(journey_analysis_base,
                              journey_start_begin = "2017-04-01",
                              journey_start_end = "2017-06-30"){

  NSLHD <- NSLHD_facility()
  journey_start_begin <- as.POSIXct(journey_start_begin)
  journey_start_end <- as.POSIXct(journey_start_end)

  is.ACS <- function(.x) {
    any(.x %in% ACS_codes(),
        na.rm=T)
  }

  journey_acs <- journey_analysis_base %>%
    dplyr::filter(JOURNEY_START >journey_start_begin & # filter the two years
                    JOURNEY_START < journey_start_end) %>%
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
      any(ENCLASS == "ED.Admitted" & FACILITY %in% NSLHD & !(ADMIT_MODE %in% internal_transfers()), na.rm=T) |
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
}

#' Extracts ATAMI triage forms
#'
#' Extracts ATAMI triage forms. An ATAMI triage form is any form that contains
#' `atami`, `etami`,`cath lab`,`cathlab`
#' @param form_triag_classified a df containing triag forms
#' @return a dataframe that contains only ATAMI triag forms
#' @family journey
extract_triage_forms <- function(form_triage_classified){

  triage_form_prep <- form_triage_classified %>%
    dplyr::mutate(
      ATAMI = stringr::str_detect(CHARTED_VALUE,  stringr::regex("atami|etami|cath lab|cathlab", ignore_case = T))
    ) %>%
    dplyr::filter(ATAMI)
}

#' Extracts procedures from the given cohort
#'
#' Extracts procedures from the given cohort based on their encounter key
#' Include a separate column for the presence of a coronary artery bypass procedure
#' @param procedures_grouped a df containing procedures_grouped
#' @return a dataframe that contains only ATAMI triag forms
#' @family acs
extract_acs_procedures <- function(procedures_grouped,journey_acs){
  procedures_grouped %>%
    dplyr::filter(ENCNTR_KEY %in% journey_acs$ENCNTR_KEY) %>%
    dplyr::mutate(PROCEDURE_NAME_GROUPS = sapply(PROCEDURE_NAME_GROUPS,toString),
                  PROCEDURE_NAME_KEYWORDS = sapply(PROCEDURE_NAME_KEYWORDS,toString)) %>%
    dplyr::mutate_if(is.character, list(~dplyr::na_if(.,""))) %>%
    dplyr::mutate(
      CABG = stringr::str_detect(PROCEDURE_NAME,"(?i)coronary artery bypass")) %>%
    dplyr::filter(ANGIOGRAM | PCI | CABG) %>%
    dplyr::left_join(
      dplyr::select(journey_acs,JOURNEY_KEY,ENCNTR_KEY), by = c("ENCNTR_KEY")
    ) %>%
    dplyr::select(JOURNEY_KEY, PROCEDURE_DTTM, ANGIOGRAM,PCI,CABG)
}

#' Merges procedure table with McKesson data
#'
#' Merges procedure table with McKesson data particularly for PCI
#' procedures, ensuring consistency between visualation of
#' ICD10 procedures and McKesson
#'
#' @param procedures a df containing ICD10 procedures.
#' @param door_to_balloon as df containing doot to balloon times for McKesson
#'      PCI procedures
#' @return a df
#' @family acs
merge_mckesson_cerner_procedures <- function(procedures,door_to_balloon){

  pci_journeys <- procedures %>%
    dplyr::group_by(JOURNEY_KEY) %>%
    dplyr::summarise(HAD_PCI = any(PCI, na.rm = TRUE)) %>%
    dplyr::filter(HAD_PCI)

  # merge the door to balloon and ensure we
  missing_pci <- door_to_balloon %>%
    dplyr::mutate(pci=TRUE) %>%
    dplyr::select(JOURNEY_KEY,PROCEDURE.START.TIME,PCI) %>%
    dplyr::filter(!JOURNEY_KEY %in% procedures$JOURNEY_KEY | !JOURNEY_KEY %in% pci_journeys$JOURNEY_KEY) %>%
    dplyr::mutate(CABG = FALSE,ANGIOGRAM=FALSE) %>%
    dplyr::rename(PROCEDURE_DTTM = PROCEDURE.START.TIME) %>%
    dplyr::bind_rows(procedures)
}


#' Categorises the trajectories of journeys
#'
#' Categorises the trajectories of journeys and whether the journey was a transfer or not
#' Include a separate column for the presence of a coronary artery bypass procedure
#' @param journeys a df containing encounters and their journey keys
#' @return journeys dataframe with the columns, `TRANSFER_STATUS` and `TRAJECTORY`
#'
#' @family journey
create_trajectory <- function(journeys){
  NSLHD <- NSLHD_facility()

  CCLHD <- CCLHD_facility()

  trajectories<- journeys %>%
    dplyr::mutate(FACILITY = as.character(FACILITY)) %>%
    dplyr::group_by(JOURNEY_KEY) %>%
    dplyr::mutate(TRANSFER_STATUS = dplyr::case_when(
      JOURNEY_FIRST_FACILITY != "Royal North Shore" & any(FACILITY == "Royal North Shore", na.rm=T) ~ "Transfer to RNS",
      TRUE ~ "No Transfer"
    )) %>%
    dplyr::mutate(
      JOURNEY_FIRST_FACILITY,
      JOURNEY_FINAL_FACILITY,
      TRAJECTORY = dplyr::case_when(
        JOURNEY_FIRST_FACILITY %in% CCLHD & any(FACILITY == "Royal North Shore", na.rm=T) ~ "CCLHD - RNS",
        JOURNEY_FIRST_FACILITY == "Ryde" & any(FACILITY == "Royal North Shore", na.rm=T)  ~ "Ryde - RNS",
        JOURNEY_FIRST_FACILITY == "Manly" & any(FACILITY == "Royal North Shore", na.rm=T)  ~ "Manly - RNS",
        JOURNEY_FIRST_FACILITY == "Mona Vale" & any(FACILITY == "Royal North Shore", na.rm=T)  ~ "Mona Vale - RNS",
        JOURNEY_FIRST_FACILITY == "Hornsby" & any(FACILITY == "Royal North Shore", na.rm=T)  ~ "Hornsby - RNS",
        JOURNEY_FIRST_FACILITY == "Royal North Shore" & any(FACILITY == "Royal North Shore", na.rm=T)   ~ "RNS Only",
        JOURNEY_FIRST_FACILITY %in% NSLHD[-1] & !any(FACILITY == "Royal North Shore", na.rm=T) ~ "NSLHD (exc. RNS)",
        any(FACILITY %in% NSLHD[-1]) &
          any(stringr::str_detect(JOURNEY_FINAL_FACILITY, "(?i)private")|
                stringr::str_detect(JOURNEY_SEP_MODE, "(?i)private"))                 ~ "NSLHD - private",
        JOURNEY_FIRST_FACILITY %in% CCLHD & FACILITY %in% CCLHD  ~ "CCLHD",
      )
    ) %>%
    dplyr::ungroup()

  trajectories %<>% dplyr::select(JOURNEY_KEY, TRAJECTORY,TRANSFER_STATUS) %>%
    dplyr::distinct()

}

#' Defines the separation mode for journeys
#'
#' Defines the separation mode for journeys and groups separation modes
#' that are not of interest to "Other"
#' @param journeys a df containing encounters and their journey keys
#' @return a df containing, `SEPARATION_MODE`
#'
#' @family journey
create_separations <- function(journeys){

  separations_of_interest <- separation_modes()

  separations <- journeys %>%
    dplyr::select(JOURNEY_KEY, JOURNEY_SEP_MODE) %>%
    dplyr::distinct() %>%
    dplyr::transmute(
      JOURNEY_KEY,
      JOURNEY_SEP_MODE = forcats::fct_explicit_na(JOURNEY_SEP_MODE, na_level = "or missing"),
      SEPARATION_MODE = forcats::fct_other(JOURNEY_SEP_MODE, keep = separations_of_interest)
    )
}

#' Defines the presentation mode for journeys
#'
#' Defines the presentation mode for journeys and groups presentation modes
#' that are not of interest to "Other"
#' @param journeys a df containing encounters and their journey keys
#' @return a df containing, `PRESENTATION_MODE`
#'
#' @family journey
create_presentations <- function(journeys, form_triage_classified){

  triage_keys <- form_triage_classified %>%
    dplyr::right_join(dplyr::select(journeys, JOURNEY_KEY, ENCNTR_KEY, FACILITY),
                      by = "ENCNTR_KEY") %>%
    dplyr::mutate(
      ATAMI = stringr::str_detect(CHARTED_VALUE,  stringr::regex("atami|etami|cath lab|cathlab", ignore_case = T))
    ) %>%
    dplyr::filter(ATAMI) %>%
    dplyr::pull(JOURNEY_KEY)

  journeys %<>%
    dplyr::mutate(JRN.TRIAGE.AMBULANCE = JOURNEY_KEY %in% triage_keys &JOURNEY_FIRST_FACILITY == "Royal North Shore")

  presentations <- journeys %>%
    dplyr::filter(JOURNEY_START == ADMIT_DTTM) %>% # select the first encounter
    dplyr::transmute(
      JOURNEY_KEY,
      ADMIT_MODE,
      PRESENTATION_MODE = dplyr::case_when(
        stringr::str_detect(ADMIT_MODE, "Ambulance") & JRN.TRIAGE.AMBULANCE ~ "Ambulance (triage)",
        stringr::str_detect(ADMIT_MODE, "Ambulance") ~ "Ambulance",
        ADMIT_MODE %in% c("Private Car",
                          "No Transport (Walked In)",
                          "Community / Public Transport") ~ "Self-present",
        TRUE ~ "Other")
    ) %>%
    dplyr::distinct()
}

#' Creates categories for the types of interventions
#'
#' Patients are classified based on whether they have had
#' any of the following procedures: angiogram,pci,CABG
#' or whether they were transferred to a private hospital
#'
#' It is assumed that if a patient had a PCI, or CABG
#' they would have an angiogram
#' @param journeys a df containing encounters and their journey keys
#' @param procedures a df containing the relevant procedures for the cohort
#' @return a df containing, `INTERVENTION`
#'
#' @family journey
create_interventions <- function(journeys,procedures){

  interventions <-procedures %>%
    dplyr::full_join(dplyr::select(journeys,JOURNEY_KEY, JOURNEY_SEP_MODE)) %>%
    dplyr::group_by(JOURNEY_KEY) %>%
    dplyr::summarise(
      angiogram = any(ANGIOGRAM, na.rm = TRUE),
      pci = any(PCI, na.rm = TRUE),
      cabbage = any(CABG, na.rm = TRUE),
      private = any(JOURNEY_SEP_MODE == "private hospital", na.rm = TRUE)
    ) %>%
    dplyr::mutate(
      INTERVENTION = dplyr::case_when(
        pci ~ "Angio & PCI",
        cabbage ~ "Angio & CABG",
        !pci & !cabbage & private ~ "Private",
        angiogram ~ "Angio only",
        TRUE ~ "None"),
      INTERVENTION = forcats::fct_relevel(INTERVENTION, "None", after = Inf)
    ) #%>%
  #dplyr::select(JOURNEY_KEY,ENCNTR_KEY_I)

}

#' Creates discharge medication
#'
#' Summarises discharge medications for ACS cohort. In particular, summarises
#' `aspirin`,`P2Y12`,`Anticoagulant`,`A2RB`,`Beta blockers` and `Statins`
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
#' @seealso \code{\link{create_hf_meds_discharge}} for heart failure medication
#' @family acs
create_meds_discharge <- function(journeys,meds,discharge_letter_keys){

  antiplatelets = c("P2Y12 receptor blocker", "Other antiplatelet")
  exclusions <- exclusion_criteria_meds()

  meds_sum <- meds %>%
    dplyr::group_by(JOURNEY_KEY) %>%
    dplyr::summarise(
      letter = any(ENCNTR_KEY %in% discharge_letter_keys),
      Aspirin = sum(MEDICATION == "aspirin" & DISCHARGE_MED, na.rm = TRUE),
      `P2Y12 & others` = sum(MEDICATION %in% antiplatelets & DISCHARGE_MED, na.rm = TRUE),
      Anticoagulant = sum(MEDICATION == "warfarin/other anticoagulant" & DISCHARGE_MED, na.rm = TRUE),
      `A2RB/ACE Inhibitor` = sum(MEDICATION == "A2RB/ACE Inhibitor" & DISCHARGE_MED, na.rm = TRUE),
      `Beta blocker` = sum(MEDICATION == "Beta blocker" & DISCHARGE_MED, na.rm = TRUE),
      Statin = sum(MEDICATION == "Statin" & DISCHARGE_MED, na.rm = TRUE)
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

#' Creates SNOMED code column
#'
#' Extracts any STEMI or NSTEMI  related SNOMED diagnosis for the cohort journey
#'
#' @param journeys a df containing journeys
#' @param diagnosis a df containing diagnosis codes
#' @return the `journeys` df with additional logical columns `STEMI_SNOMED` and `NSTEMI_SNOMED`
#' describing whether that journeys had the relevant SNOMED code
#'
#' @family acs
create_stemi_code <- function(journeys, diagnosis){


  STEMI_SNOMED <- "(?i)acute( inferior | anterior |\\s)st segment elevation|(ST elevation myocardial infarction)|(Acute STEMI)"

  snomed_journeys <-  diagnosis %>%
    dplyr::filter(CONFIRMATION_STATUS == "Confirmed" &
                    (CLASSIFICATION == "ED Medical"
                    | CLASSIFICATION == "Medical" | CLASSIFICATION == "ED Nursing")) %>%
    dplyr::filter(N_SOURCE_VOCABULARY_DISP == "SNOMED CT") %>%
    dplyr::select(ENCNTR_KEY,SOURCE_STRING_CAP)

  stemi <-  snomed_journeys %>%
    dplyr::filter(stringr::str_detect(SOURCE_STRING_CAP, STEMI_SNOMED)) %>%
    dplyr::right_join(journeys, by=c("ENCNTR_KEY")) %>%
    dplyr::filter(!is.na(SOURCE_STRING_CAP))

  NSTEMI_SNOMED <- "NSTEMI|(Non-st)|(Non st)"
  snomed_nstemi <- snomed_journeys %>%
    dplyr::filter(stringr::str_detect(SOURCE_STRING_CAP, NSTEMI_SNOMED)) %>%
    dplyr::right_join(journeys, by=c("ENCNTR_KEY")) %>%
    dplyr::filter(!is.na(SOURCE_STRING_CAP))

  journeys %<>% dplyr::mutate(
    SNOMED_ACS =  dplyr::case_when(
      JOURNEY_KEY %in% stemi$JOURNEY_KEY ~ 'STEMI',
      JOURNEY_KEY %in% snomed_nstemi$JOURNEY_KEY ~ 'NSTEMI')) %>%
    dplyr::select(JOURNEY_KEY,SNOMED_ACS,ICD10_ACS) %>%
    dplyr::distinct()


}

#' Extracts cohort discharge keys
#'
#' @family journey
create_discharge_letter_keys <- function(journeys,discharge_letter_keys) {

  acs_discharge_letter_keys <- discharge_letter_keys %>%
    dplyr::filter(ENCNTR_KEY %in% journeys$ENCNTR_KEY) %>%
    dplyr::pull(ENCNTR_KEY)
}

#' Extracts door to balloon times
#'
#' Extracts door to balloon times,  bins the times
#' and assigns a category to each door to baloon measurement
#' @param journeys a df containing journeys
#' @param door_to_balloon a df containing balloon times
#' @param demographics a df containing demopgrahic information
#' of journeys such as `TRAJECTORY` and `PRESENTATION_MODE`
#'
#' @return the door_to_balloon df and additional columns `DTB_PRESENTATION_MODE`
#' and `DELTA_BINNED` for the cohort journeys
#'
#' @family acs
create_door_to_balloon <- function(journeys,door_to_balloon, demographics){

  breaks <- c(-Inf, 0, 30, 60, 90, 120,150,180,300, Inf)
  labels <- c("<0", "0-30 min", "30-60 min", "60-90 min","90-120 min", "120-150 min","150-180 min","180-300 min", "300+ min")

  door_to_balloon %<>%
    dplyr::filter(JOURNEY_KEY %in% journeys$JOURNEY_KEY) %>%
    dplyr::mutate(PROCEDURE_STATUS = sapply(PROCEDURE_STATUS,toString)) %>%
    dplyr::mutate_if(is.character, list(~dplyr::na_if(.,""))) %>%
    dplyr::mutate(DELTA_BINNED = cut(DELTA, breaks = breaks, labels = labels,
                                     right = FALSE))


  acs_door_to_balloon <- demographics %>%
    dplyr::select(JOURNEY_KEY, TRAJECTORY,PRESENTATION_MODE) %>%
    dplyr::left_join(door_to_balloon) %>%
    dplyr::mutate(DTB_PRESENTATION_MODE = dplyr::case_when(
      PRESENTATION_MODE == "Ambulance (triage)" ~ "RNS w/ Field Triage",
      TRAJECTORY == "RNS Only"  & PRESENTATION_MODE == 'Ambulance'~ "RNSH w/o Field Triage",
      TRAJECTORY == "RNS Only"  & PRESENTATION_MODE != 'Ambulance' ~ "RNSH Self-present + Other",
      TRUE ~ "Transfer"
    )) %>%
    dplyr::filter(!is.na(DELTA)) %>%
    dplyr::select(-c(TRAJECTORY,PRESENTATION_MODE))
}

#' Creates demographic information
#'
#' Creates columns such as `AGE`, `ADMIT_IN_BUSINESS_HOURS`, `LENGTH_OF_STAY`
#' for each journey
#'
#' @param journeys a df containing journeys
#' @param form_triage_classified df containing triage forms
#' @return a df containing journeys and the additional columns
#' @family journey
create_demographics <- function(journeys,form_triage_classified){

  los_breaks <- c(0, 0.2083333, 0.5416667, 1, 2, 5, 10, 30, Inf)
  los_labels <- c("0-4 hr", "5-12 hr", "13-24 hr", "1-2 d", "2-5 d", "5-10 d",
                  "10-30 d", ">30 d")


  AGE_breaks <- c(-Inf,18, 25, 45, 65, 80, Inf)
  AGE_labels <- c("<18", "19-24", "25-44", "45-64", "65-80", "80+")
  business_hours <- business_hours()

  weekdays <- business_hours$weekday

  journey_demo <- journeys %>%
    dplyr::group_by(JOURNEY_KEY) %>%
    dplyr::arrange(AGE_AT_ADMIT)%>%
    dplyr::slice(c(1)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(LENGTH_OF_STAY = cut(JOURNEY_DAYS, breaks = los_breaks, labels = los_labels,
                                       right = FALSE)) %>%
    dplyr::mutate(AGE_BINNED = cut(AGE_AT_ADMIT, breaks = AGE_breaks, labels = AGE_labels,
                                   right = FALSE)) %>%
    dplyr::mutate(WDAY_OF_JOURNEY_START = lubridate::wday(JOURNEY_START, label = TRUE)) %>%
    dplyr::mutate(ADMIT_IN_BUSINESS_HOURS = WDAY_OF_JOURNEY_START %in% weekdays &
                    lubridate::hour(JOURNEY_START) >= business_hours$start_time &
                    lubridate::hour(JOURNEY_START) <=business_hours$end_time)

  trajectory <- create_trajectory(journeys)
  presentation <- create_presentations(journeys,form_triage_classified)
  separation <- create_separations(journeys)

  journey_demo %<>%
    dplyr::left_join(trajectory, by = "JOURNEY_KEY") %>%
    dplyr::left_join(
      dplyr::select(presentation,JOURNEY_KEY, PRESENTATION_MODE), by = "JOURNEY_KEY") %>%
    dplyr::left_join(
      dplyr::select(separation,JOURNEY_KEY,SEPARATION_MODE), by = "JOURNEY_KEY")

}


#' Extracts medications relevant to the cohort journeys
#'
#' Extracts medications relevant to the cohort journeys
#' @param journey a df containin journeys
#' @param journey_meds_admit_discharge a df containing admission and discharge
#' medication
#' @return a df containing the medications for the cohort journeys
#' @family journey
create_meds_admit_discharge <- function(journey, journey_meds_admit_discharge){
  acs_journey_meds_admit_discharge <- journey_meds_admit_discharge %>%
    dplyr::filter(ENCNTR_KEY %in% journey$ENCNTR_KEY) %>%
    dplyr::select(JOURNEY_KEY,
                  ENCNTR_KEY,
                  MEDICATION_NAME, # Generic name (Maps trade names to generic name)
                  MEDICATION = GROUPS,
                  FORM_DTTM,
                  ADMIT_MED,
                  DISCHARGE_MED) %>%
    tidyr::unnest(MEDICATION, keep_empty = TRUE) %>%
    dplyr::distinct()
}

