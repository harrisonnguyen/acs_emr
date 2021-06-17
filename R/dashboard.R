#' Returns facilities in NSLHD
NSLHD_facility <- function(){
  NSLHD <- c("Royal North Shore", "Hornsby", "Ryde", "Manly", "Mona Vale")

  return(NSLHD)
}

#' Returns facility in CCLHD
CCLHD_facility <- function(){
  CCLHD <- c("Gosford", "Woy Woy", "Wyong")

  return(CCLHD)
}

create_trajectory <- function(df){
  NSLHD <- NSLHD_facility()

  CCLHD <- CCLHD_facility()

  trajectories<- df %>%
    dplyr::mutate(FACILITY = as.character(FACILITY)) %>%
    dplyr::group_by(JOURNEY_KEY) %>%
    dplyr::mutate(TRANSFER_STATUS = dplyr::case_when(
      JOURNEY_FIRST_FACILITY != "Royal North Shore" & any(FACILITY == "Royal North Shore", na.rm=T) ~ "Transfer to RNS",
      #any(FACILITY != JOURNEY_FIRST_FACILITY, na.rm=T) ~ "Transfer to Other",
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

create_separations <- function(journeys){

  separations_of_interest <- c("home",
                               "death",
                               "private hospital"
  )

  separations <- journeys %>%
    dplyr::select(JOURNEY_KEY, JOURNEY_SEP_MODE) %>%
    dplyr::distinct() %>%
    dplyr::transmute(
      JOURNEY_KEY,
      JOURNEY_SEP_MODE = forcats::fct_explicit_na(JOURNEY_SEP_MODE, na_level = "or missing"),
      SEPARATION_MODE = forcats::fct_other(JOURNEY_SEP_MODE, keep = separations_of_interest)
    )

}

create_presentations <- function(journey, triage_form){

  triage_keys <- triage_form %>%
    dplyr::right_join(dplyr::select(journey, JOURNEY_KEY, ENCNTR_KEY, FACILITY),
               by = "ENCNTR_KEY") %>%
    dplyr::mutate(
      ATAMI = stringr::str_detect(CHARTED_VALUE,  stringr::regex("atami|etami|cath lab|cathlab", ignore_case = T))
    ) %>%
    dplyr::filter(ATAMI) %>%
    dplyr::pull(JOURNEY_KEY)

  journey %<>%
    dplyr::mutate(JRN.TRIAGE.AMBULANCE = JOURNEY_KEY %in% triage_keys &JOURNEY_FIRST_FACILITY == "Royal North Shore")

  presentations <- journey %>%
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
#' Patients are classified based on whether they have had
#' any of the following procedures: angiogram,pci,CABG
#' or whether they were transferred to a private hospital
#'
#' It is assumed that if a patient had a PCI, or CABG
#' they would have an angiogram
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

create_meds_discharge <- function(meds,journeys,discharge_letter_keys){

  antiplatelets = c("P2Y12 receptor blocker", "Other antiplatelet")
  exclusions <- c("death", "dama", "private hospital")

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

create_stemi_code <- function(journeys, diagnosis){


  STEMI_SNOMED <- "(?i)acute( inferior | anterior |\\s)st segment elevation|(ST elevation myocardial infarction)|(Acute STEMI)"

  snomed_journeys <-  diagnosis %>%
    dplyr::filter(confirmation_status == "Confirmed" & (classification == "ED Medical" | classification == "Medical" | classification == "ED Nursing")) %>%
    dplyr::filter(n_source_vocabulary_disp == "SNOMED CT") %>%
    dplyr::select(encntr_key,source_string_cap)

    stemi <-  snomed_journeys %>%
      dplyr::filter(stringr::str_detect(source_string_cap, STEMI_SNOMED)) %>%
      dplyr::right_join(journeys, by=c("encntr_key" = "ENCNTR_KEY")) %>%
      dplyr::filter(!is.na(source_string_cap))

  NSTEMI_SNOMED <- "NSTEMI|(Non-st)|(Non st)"
  snomed_nstemi <- snomed_journeys %>%
    dplyr::filter(stringr::str_detect(source_string_cap, NSTEMI_SNOMED)) %>%
    dplyr::right_join(journeys, by=c("encntr_key" = "ENCNTR_KEY")) %>%
    dplyr::filter(!is.na(source_string_cap))

  journeys %<>% dplyr::mutate(
                SNOMED_ACS =  dplyr::case_when(
                                  JOURNEY_KEY %in% stemi$JOURNEY_KEY ~ 'STEMI',
                                  JOURNEY_KEY %in% snomed_nstemi$JOURNEY_KEY ~ 'NSTEMI')) %>%
    dplyr::select(JOURNEY_KEY,SNOMED_ACS,ICD10_ACS) %>%
    dplyr::distinct()


}

create_discharge_letter_keys <- function(journey,discharge_letter_keys) {

  acs_discharge_letter_keys <- discharge_letter_keys %>%
    dplyr::rename_all(toupper) %>%
    dplyr::filter(ENCNTR_KEY %in% journey$ENCNTR_KEY) %>%
    dplyr::pull(ENCNTR_KEY)
}

create_door_to_balloon <- function(journey,door_to_balloon, trajectories, presentations){

  breaks <- c(-Inf, 0, 30, 60, 90, 120,150,180,300, Inf)
  labels <- c("<0", "0-30 min", "30-60 min", "60-90 min","90-120 min", "120-150 min","150-180 min","180-300 min", "300+ min")

  door_to_balloon %<>%
    dplyr::filter(JOURNEY_KEY %in% journey$JOURNEY_KEY) %>%
    dplyr::mutate(procedure_status = sapply(procedure_status,toString)) %>%
    dplyr::mutate_if(is.character, list(~dplyr::na_if(.,""))) %>%
    dplyr::mutate(DELTA_BINNED = cut(delta, breaks = breaks, labels = labels,
                                   right = FALSE))


  acs_door_to_balloon <- trajectories %>%
    dplyr::select(JOURNEY_KEY, TRAJECTORY) %>%
    dplyr::left_join(dplyr::select(presentations, JOURNEY_KEY, PRESENTATION_MODE)) %>%
    dplyr::left_join(door_to_balloon) %>%
    dplyr::mutate(DTB_PRESENTATION_MODE = dplyr::case_when(
      PRESENTATION_MODE == "Ambulance (triage)" ~ "RNS w/ Field Triage",
      TRAJECTORY == "RNS Only"  & PRESENTATION_MODE == 'Ambulance'~ "RNSH w/o Field Triage",
      TRAJECTORY == "RNS Only"  & PRESENTATION_MODE != 'Ambulance' ~ "RNSH Self-present + Other",
      TRUE ~ "Transfer"
    )) %>%
    dplyr::filter(!is.na(delta)) %>%
    dplyr::select(-c(TRAJECTORY,PRESENTATION_MODE))
}


#' Filters the procedures table
#' for relevant ACS procedures: angiogram, pci and CABG
#' Procedures are extracted based on a regex search
#' and assigns a boolean column for each procedure for each encounter
#'
#' Secondly, the PCI boolean column is corrobated with PCI mckesson
#' procedures
#' @export
create_procedures <- function(journey, procedures_grouped,door_to_balloon){
  acs_procedures <- procedures_grouped %>%
  dplyr::filter(encntr_key %in% journey$ENCNTR_KEY) %>%
  dplyr::mutate(procedure_name_groups = sapply(procedure_name_groups,toString),
                procedure_name_keywords = sapply(procedure_name_keywords,toString)) %>%
  dplyr::mutate_if(is.character, list(~dplyr::na_if(.,""))) %>%
  dplyr::mutate(
    cabg = stringr::str_detect(procedure_name,"(?i)coronary artery bypass")) %>%
  dplyr::filter(angiogram | PCI | cabg) %>%
  dplyr::rename_all(toupper) %>%
  dplyr::left_join(
    dplyr::select(journey,JOURNEY_KEY,ENCNTR_KEY), by = c("ENCNTR_KEY")
  ) %>%
  dplyr::select(JOURNEY_KEY, PROCEDURE_DTTM, ANGIOGRAM,PCI,CABG)

  pci_journeys <- acs_procedures %>%
    dplyr::group_by(JOURNEY_KEY) %>%
    dplyr::summarise(HAD_PCI = any(PCI, na.rm = TRUE)) %>%
    dplyr::filter(HAD_PCI)
  # merge the door to balloon and ensure we
  missing_pci <- door_to_balloon %>%
    dplyr::mutate(pci=TRUE) %>%
    dplyr::select(JOURNEY_KEY,Procedure.start.time,pci) %>%
    dplyr::filter(!JOURNEY_KEY %in% acs_procedures$JOURNEY_KEY | !JOURNEY_KEY %in% pci_journeys$JOURNEY_KEY) %>%
    dplyr::mutate(CABG = FALSE,ANGIOGRAM=FALSE) %>%
    dplyr::rename(PROCEDURE_DTTM = Procedure.start.time,PCI = pci) %>%
    dplyr::bind_rows(acs_procedures)


}


create_procedures_second <- function(journeys, procedures_grouped,door_to_balloon){

  acs_procedures <- procedures_grouped %>%
    dplyr::filter(encntr_key %in% journeys$ENCNTR_KEY) %>%
    dplyr::mutate(procedure_name_groups = sapply(procedure_name_groups,toString),
                  procedure_name_keywords = sapply(procedure_name_keywords,toString)) %>%
    dplyr::mutate_if(is.character, list(~dplyr::na_if(.,""))) %>%
    dplyr::mutate(
      cabg = stringr::str_detect(procedure_name,"(?i)coronary artery bypass")) %>%
    dplyr::filter(angiogram | PCI | cabg) %>%
    dplyr::rename_all(toupper) %>%
    dplyr::left_join(
      dplyr::select(journeys,JOURNEY_KEY,ENCNTR_KEY), by = c("ENCNTR_KEY")
    ) %>%
    dplyr::select(JOURNEY_KEY, PROCEDURE_DTTM, ANGIOGRAM,PCI,CABG)

  stemi <- journeys %>%
    dplyr::filter(ICD10_ACS == "STEMI")

  interventions <-acs_procedures %>%
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
    ) %>%
    dplyr::mutate(PCI_MCKESSON = JOURNEY_KEY %in% door_to_balloon$JOURNEY_KEY) %>%
    dplyr::filter(JOURNEY_KEY %in% stemi$JOURNEY_KEY)
}


create_meds_admit_discharge <- function(journey, journey_meds_admit_discharge){
  acs_journey_meds_admit_discharge <- journey_meds_admit_discharge %>%
    dplyr::rename_all(toupper) %>%
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

create_demographics <- function(journey){

  los_breaks <- c(0, 0.2083333, 0.5416667, 1, 2, 5, 10, 30, Inf)
  los_labels <- c("0-4 hr", "5-12 hr", "13-24 hr", "1-2 d", "2-5 d", "5-10 d",
                  "10-30 d", ">30 d")


  AGE_breaks <- c(-Inf,18, 25, 45, 65, 80, Inf)
  AGE_labels <- c("<18", "19-24", "25-44", "45-64", "65-80", "80+")

  weekdays <- c('Mon','Tue','Wed','Thu','Fri')
  journey_demo <- journey %>%
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
                    lubridate::hour(JOURNEY_START) >= 8 &
                    lubridate::hour(JOURNEY_START) <=18)

}

create_glucose <- function(encounters,btf_forms_glu,iview_glu,pathology){
  d0<- btf_forms_glu %>%
    dplyr::filter(ENCNTR_KEY %in% encounters$ENCNTR_KEY) %>%
    dplyr::select(ENCNTR_KEY, LABEL, GLUCOSE = CHARTED_VALUE,
           GLUCOSE_DTTM = FORM_MODIF_DTTM)
  d1<-iview_glu %>%
    dplyr::filter(ENCNTR_KEY %in% encounters$ENCNTR_KEY) %>%
    dplyr::select(ENCNTR_KEY, LABEL = EVENT_SET_DISPLAY, GLUCOSE = IVIEW_RESULT,
           GLUCOSE_DTTM = IVIEW_RESULT_DTTM)
  d2<- pathology %>%
    dplyr::filter(ENCNTR_KEY %in% encounters$ENCNTR_KEY) %>%
    dplyr::filter(stringr::str_detect(RESULT_DISPLAY, "Glucose \\(Rand")) %>%
    dplyr::select(ENCNTR_KEY, LABEL = RESULT_DISPLAY, GLUCOSE = RESULT_TAG,
           GLUCOSE_DTTM = RESULT_START_DTTM, FASTING_TEST)

  glucose_results <- dplyr::bind_rows(d0, d1, d2) %>%
    dplyr::right_join(
      dplyr::select(journey_acs,ENCNTR_KEY, ADMIT_DTTM)) %>%
    dplyr::mutate(LABEL = dplyr::recode(LABEL,
                                        `Glucose (Rand)` = "Glucose (Random)",
                                        `Glucose (Random)` = "Glucose (Random)",
                                        `Blood Glucose Level, Bedside` = "Glucose (Bedside)",
                                        `Blood Glucose Level Bedside` = "Glucose (Bedside)"
    )) %>%
    dplyr::distinct() %>%
    dplyr::arrange(ENCNTR_KEY, GLUCOSE_DTTM) %>%
    dplyr::mutate(
      NEXT_DAY = as.integer(as.Date(GLUCOSE_DTTM) - as.Date(ADMIT_DTTM)) %in% c(1:2) & lubridate::am(GLUCOSE_DTTM)
    )
}

create_hba1c_test <- function(pathology,encounters){
  hba1c_results <- pathology %>%
    dplyr::filter(ENCNTR_KEY %in% encounters$ENCNTR_KEY) %>%
    dplyr::filter(RESULT_DISPLAY == "HbA1c (NGSP)") %>%
    dplyr::select(ENCNTR_KEY, HBA1C = RESULT_TAG, HBA1C_DTTM = RESULT_START_DTTM) %>%
    dplyr::arrange(ENCNTR_KEY, HBA1C_DTTM)
}

htn_bp_when <- function(Diastolic, Systolic, BP_DTTM) {

  dplyr::case_when(
    sum(Diastolic > 90 | Systolic > 140, na.rm=T) > 2 ~ "Positive",
    length(BP_DTTM) < 3 ~ "Missing", # if less than 2 BP measurements
    TRUE ~ "Negative"
  )
}

create_blood_pressure <- function(btf_forms_bp,iview_bp,encounters){
  d0<- btf_forms_bp %>%
    dplyr::filter(ENCNTR_KEY %in% encounters$ENCNTR_KEY) %>%
    dplyr::select(ENCNTR_KEY, LABEL, BP = CHARTED_VALUE, BP_DTTM = FORM_MODIF_DTTM)
  d1<-iview_bp %>%
    dplyr::filter(ENCNTR_KEY %in% encounters$ENCNTR_KEY) %>%
    dplyr::select(ENCNTR_KEY, LABEL = EVENT_SET_DISPLAY, BP = IVIEW_RESULT,
           BP_DTTM = IVIEW_RESULT_DTTM)

  d3<- dplyr::bind_rows(d0, d1) %>%
    dplyr::mutate(
      LABEL = dplyr::case_when(
        stringr::str_detect(LABEL, "Systolic") ~ "Systolic",
        stringr::str_detect(LABEL, "Diastolic") ~ "Diastolic")
    ) %>%
    dplyr::group_by(ENCNTR_KEY, LABEL, BP_DTTM) %>%
    dplyr::summarise(BP = mean(BP, na.rm=T)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(ENCNTR_KEY, BP_DTTM) %>%
    tidyr::spread(LABEL, BP) %>%
    dplyr::group_by(ENCNTR_KEY) %>%
    dplyr::summarise(
      htn_bp = htn_bp_when(Diastolic, Systolic, BP_DTTM)
    ) %>%
    dplyr::left_join(
      dplyr::select(journey_acs,JOURNEY_KEY,ENCNTR_KEY,ADMIT_DTTM), by = "ENCNTR_KEY"
    ) %>%
    dplyr::group_by(JOURNEY_KEY) %>%
    dplyr::arrange(ADMIT_DTTM) %>%
    dplyr::slice(c(1)) %>%
    dplyr::ungroup()
}

create_lipids <- function(pathology,encounters){
  lipid_tests <- c(
    "Cholesterol",
    "LDL Cholesterol",
    "HDL Cholesterol",
    "Triglycerides"
  )
  pathology_lipids <- pathology %>%
    dplyr::filter(ENCNTR_KEY %in% encounters$ENCNTR_KEY) %>%
    dplyr::filter(RESULT_TAG != "Error") %>%
    dplyr::filter(RESULT_DISPLAY %in% lipid_tests) %>%
    dplyr::mutate_at(dplyr::vars(RESULT_TAG, NORMAL_LOW, NORMAL_HIGH), tidyr::extract_numeric) %>%
    dplyr::select(
      ENCNTR_KEY,
      RESULT_START_DTTM,
      RESULT_DISPLAY,
      RESULT_TAG,
      NORMAL_LOW,
      NORMAL_HIGH
    ) %>%
    dplyr::mutate(
      NORMAL_LOW = tidyr::replace_na(NORMAL_LOW, 0),
      NORMAL_HIGH = dplyr::if_else(is.na(NORMAL_HIGH), Inf, NORMAL_HIGH)
    ) %>%
    dplyr::group_by(ENCNTR_KEY, RESULT_DISPLAY) %>%
    dplyr::filter(RESULT_START_DTTM == min(RESULT_START_DTTM, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-c(NORMAL_LOW,NORMAL_HIGH)) %>%
    dplyr::group_by(ENCNTR_KEY, RESULT_DISPLAY, RESULT_START_DTTM) %>%
    tidyr::spread(RESULT_DISPLAY, RESULT_TAG)
}

# get the table wh
get_first_encounter <- function(df,journey_acs,name=NULL,bins=NULL){
  df %<>% dplyr::left_join(
    dplyr::select(journey_acs,JOURNEY_KEY,ENCNTR_KEY,ADMIT_DTTM), by = "ENCNTR_KEY"
  ) %>%
  dplyr::group_by(JOURNEY_KEY) %>%
  dplyr::arrange(ADMIT_DTTM,RESULT_START_DTTM) %>%
  dplyr::slice(c(1)) %>%
  dplyr::ungroup() %>%
  dplyr::select(ENCNTR_KEY,JOURNEY_KEY, .data[[name]],RESULT_START_DTTM)

  if(!is.null(name) & !is.null(bins)){
    df %<>%dplyr::mutate("{name}_BIN" := cut(as.numeric(.data[[name]]),
                                             breaks=bins$breaks,
                                             include.lowest = TRUE,
                                             right = FALSE,
                                             labels=bins$tags))
  }
  return(df)

}
library(magrittr)

# load the data
data(journey_acs)
data(triage_form_prep)
data(discharge_letter_keys)
data(door_to_balloon)
data(procedures_grouped)
data(journey_meds_admit_discharge)
data(troponin_encounter_order)
data(troponin_path)
data(diagnosis_data)
data(triage_form)


output_dir <- here::here("output_hf")

acs_demographics <- create_demographics(journey_acs)
trajectories <- create_trajectory(journey_acs)
presentations <- create_presentations(journey_acs,triage_form)

acs_door_to_balloon <- create_door_to_balloon(journey_acs,door_to_balloon,trajectories,presentations)

acs_procedures <- create_procedures(journey_acs,procedures_grouped,acs_door_to_balloon)


separations <- create_separations(journey_acs)
interventions <- create_interventions(journey_acs,acs_procedures)

diagnosis_acs <- create_stemi_code(journey_acs,diagnosis_data)

acs_demographics %<>%
  dplyr::left_join(trajectories, by = "JOURNEY_KEY") %>%
  dplyr::left_join(
    dplyr::select(presentations,JOURNEY_KEY, PRESENTATION_MODE), by = "JOURNEY_KEY") %>%
  dplyr::left_join(
    dplyr::select(separations,JOURNEY_KEY,SEPARATION_MODE), by = "JOURNEY_KEY") %>%
  dplyr::left_join(interventions, by = "JOURNEY_KEY") %>%
  dplyr::left_join(
    dplyr::select(diagnosis_acs,JOURNEY_KEY,SNOMED_ACS), by = 'JOURNEY_KEY')


acs_discharge_letter_keys <- create_discharge_letter_keys(journey_acs,discharge_letter_keys)




acs_journey_meds_admit_discharge <- create_meds_admit_discharge(journey_acs,journey_meds_admit_discharge)


meds_discharge_sum <- create_meds_discharge(acs_journey_meds_admit_discharge,
                                            journey_acs,
                                            acs_discharge_letter_keys)




write.csv(meds_discharge_sum,
          file.path(output_dir,"acs_medication_sum.csv"),
          row.names=FALSE)
write.csv(acs_journey_meds_admit_discharge,
          file.path(output_dir,"acs_journey_meds_admit_discharge.csv"),
          row.names=FALSE)

write.csv(acs_discharge_letter_keys,
          file.path(output_dir,"acs_discharge_letter_keys.csv"),
          row.names=FALSE)


write.csv(acs_demographics,
          file.path(output_dir,"demographics.csv"),
          row.names=FALSE)

journey_acs_reduced <- journey_acs %>%
  dplyr::select(PERSON_KEY,JOURNEY_KEY,ENCNTR_KEY, FACILITY,ADMIT_MODE, ADMIT_DTTM, DISCHARGE_DTTM,)

write.csv(journey_acs_reduced,
          file.path(output_dir,"journey_acs.csv"),
          row.names=FALSE)

write.csv(acs_procedures,
          file.path(output_dir,"acs_procedures.csv"),
          row.names=FALSE)

write.csv(acs_door_to_balloon,
          file.path(output_dir,"acs_door_to_balloon.csv"),
          row.names=FALSE)


# glucose
data(glucose_forms)
data(iview_glu)
data(pathology)

glucose <- create_glucose(journey_acs,glucose_forms,iview_glu,pathology)
write.csv(glucose,
          file.path(output_dir,"glucose.csv"),
          row.names=FALSE)

hba1c_results <- create_hba1c_test(pathology,journey_acs)


hba1c_results_first <- hba1c_results %>%
  dplyr::rename(RESULT_START_DTTM = HBA1C_DTTM) %>%
            get_first_encounter(journey_acs,
                                "HBA1C",
                                  bins = list(
                                      breaks =  c(0,6.5,Inf),
                                      tags = c("<6.5 (Normal)","6.5+ (Elevated)")
                                    ))
write.csv(hba1c_results,
          file.path(output_dir,"hba1c_results.csv"),
          row.names=FALSE)
write.csv(hba1c_results_first,
          file.path(output_dir,"hba1c_results_first.csv"),
          row.names=FALSE)



## blood pressure
data(bp_forms)
data(iview_bp)

bp_results<- create_blood_pressure(bp_forms,iview_bp,journey_acs)


write.csv(bp_results,
          file.path(output_dir,"bp_results.csv"),
          row.names=FALSE)

## lipid results
lipid_results<-create_lipids(pathology,journey_acs)
cholesterol <- lipid_results %>%
  dplyr::filter(!is.na(Cholesterol)) %>%
  get_first_encounter(journey_acs,
                      "Cholesterol",
                      bins = list(
                        breaks =  c(0,6.2,Inf),
                        tags = c("<6.2 (Normal)","6.2+ (Elevated)")
                      ))
triglycerides  <- lipid_results %>%
  dplyr::filter(!is.na(Triglycerides)) %>%
  get_first_encounter(journey_acs,
                      "Triglycerides",
                      bins = list(
                        breaks =  c(0,2.6,Inf),
                        tags = c("<6.2 (Normal)","2.6+ (Elevated)")
                      ))
hdl  <- lipid_results %>%
  dplyr::filter(!is.na(`HDL Cholesterol`)) %>%
  get_first_encounter(journey_acs,
                      "HDL Cholesterol",
                      bins = list(
                        breaks =  c(0,1.0,Inf),
                        tags = c("<1.0 (Abnormal)","1.0+ (Normal)")
                      ))
ldl  <- lipid_results %>%
  dplyr::filter(!is.na(`LDL Cholesterol`)) %>%
  get_first_encounter(journey_acs,
                      "LDL Cholesterol",
                      bins = list(
                        breaks =  c(0,4.1,Inf),
                        tags = c("<4.1 (Normal)","4.1+ (Elevated)")
                      ))

write.csv(lipid_results,
          file.path(output_dir,"lipid_results.csv"),
          row.names=FALSE)
write.csv(cholesterol,
          file.path(output_dir,"cholesterol.csv"),
          row.names=FALSE)
write.csv(hdl,
          file.path(output_dir,"hdl.csv"),
          row.names=FALSE)
write.csv(triglycerides,
          file.path(output_dir,"triglycerides.csv"),
          row.names=FALSE)
write.csv(ldl,
          file.path(output_dir,"ldl.csv"),
          row.names=FALSE)


## TROPONIN STUFFF
data(troponin_encounter_order)
data(troponin_path)

troponin_encounter_acs <- troponin_encounter_order %>%
  dplyr::filter(ENCNTR_KEY %in% journey_acs$ENCNTR_KEY) %>%
  dplyr::filter(ORDER_STATUS == "Completed")

write.csv(troponin_encounter_acs,
          file.path(output_dir,"troponin_encounter_acs.csv"),
          row.names=FALSE)

troponin_encounter_count <- journey_acs %>%
  dplyr::left_join(troponin_encounter_acs, by = c("ENCNTR_KEY" = "ENCNTR_KEY")) %>%
  dplyr::group_by(JOURNEY_KEY) %>%
  dplyr::summarise(TROPONIN_ORDER_COUNT = sum(!is.na(ORDER_MNEMONIC))) %>%
  dplyr::ungroup()

write.csv(troponin_encounter_count,
          file.path(output_dir,"troponin_encounter_count.csv"),
          row.names=FALSE)



troponin_result <- troponin_path %>%
  dplyr::filter(ENCNTR_KEY %in% journey_acs$ENCNTR_KEY)

write.csv(troponin_result,
          file.path(output_dir,"troponin_result.csv"),
          row.names=FALSE)

troponin_result_count <- journey_acs %>%
  dplyr::left_join(troponin_result, by = c("ENCNTR_KEY" = "ENCNTR_KEY")) %>%
  dplyr::group_by(JOURNEY_KEY) %>%
  dplyr::summarise(TROPONIN_RESULT_COUNT = sum(!is.na(RESULT_DISCRETE_VALUE))) %>%
  dplyr::ungroup()

write.csv(troponin_result_count,
          file.path(output_dir,"troponin_result_count.csv"),
          row.names=FALSE)
