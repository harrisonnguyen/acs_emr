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
    dplyr::transmute(
      JOURNEY_FIRST_FACILITY,
      JOURNEY_FINAL_FACILITY,
      TRAJECTORY = dplyr::case_when(
        JOURNEY_FIRST_FACILITY %in% CCLHD & any(FACILITY == "Royal North Shore", na.rm=T)      ~ "CCLHD - RNS",
        JOURNEY_FIRST_FACILITY %in% NSLHD[-1] & any(FACILITY == "Royal North Shore", na.rm=T)  ~ "NSLHD - RNS",
        any(FACILITY == NSLHD[1])                                                              ~ "Royal North Shore",
        JOURNEY_FIRST_FACILITY %in% NSLHD[-1] & !any(FACILITY == "Royal North Shore", na.rm=T) ~ "NSLHD (exc. RNS)",
        any(FACILITY %in% NSLHD[-1]) &
          any(stringr::str_detect(JOURNEY_FINAL_FACILITY, "(?i)private")|
                stringr::str_detect(JOURNEY_SEP_MODE, "(?i)private"))                 ~ "NSLHD - private",
      JOURNEY_FIRST_FACILITY %in% CCLHD & FACILITY %in% CCLHD  ~ "CCLHD",
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::distinct()

  trajectories %<>% dplyr::select(JOURNEY_KEY, TRAJECTORY) %>%
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
    dplyr::pull(JOURNEY_KEY)

  journey %<>%
    dplyr::mutate(JRN.TRIAGE.AMBULANCE = JOURNEY_KEY %in% triage_keys &
                    JOURNEY_FIRST_FACILITY == "Royal North Shore")

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

create_interventions <- function(journeys,procedures){

  interventions <-procedures %>%
    dplyr::full_join(dplyr::select(journeys, ENCNTR_KEY, JOURNEY_KEY, JOURNEY_SEP_MODE,)) %>%
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

  journey_sep_mode <- journeys %>%
    dplyr::select(JOURNEY_KEY, JOURNEY_SEP_MODE) %>%
    dplyr::distinct()

  meds_discharge_sum <-  journey_sep_mode %>%
    dplyr::left_join(meds_sum, by = "JOURNEY_KEY") %>%
    dplyr::filter(letter & !JOURNEY_SEP_MODE %in% exclusions)



  return(meds_discharge_sum)
}

create_stemi_code <- function(journeys, diagnosis){


  STEMI_SNOMED <- "(?i)acute( inferior | anterior |\\s)st segment elevation|(ST elevation myocardial infarction)|(Acute STEMI)"

  snomed_journeys <-  diagnosis %>%
    dplyr::filter(confirmation_status == "Confirmed" & (classification == "ED Medical" | classification == "Medical" | classification == "ED Nursing")) %>%
    dplyr::filter(n_source_vocabulary_disp == "SNOMED CT") %>%
    dplyr::filter(stringr::str_detect(source_string_cap, STEMI_SNOMED)) %>%
    dplyr::select(encntr_key,source_string_cap) %>%
    dplyr::right_join(journeys, by=c("encntr_key" = "ENCNTR_KEY")) %>%
    dplyr::filter(!is.na(source_string_cap))

  journeys %<>% dplyr::mutate(
    STEMI = stringr::str_detect(ICD10_DIAGNOSIS_LIST,"[^N]STEMI") | stringr::str_starts(ICD10_DIAGNOSIS_LIST,"STEMI"),
    SNOMED_STEMI = JOURNEY_KEY %in% snomed_journeys$JOURNEY_KEY)
}



data(journey_acs)

data(triage_form)

trajectories <- create_trajectory(journey_acs)

write.csv(trajectories,
          file.path(here::here("output"),"trajectories.csv"),
          row.names=FALSE)

presentations <- create_presentations(journey_acs,triage_form)

write.csv(presentations,
          file.path(here::here("output"),"presentations.csv"),
          row.names=FALSE)

separations <- create_separations(journey_acs)

write.csv(separations,
          file.path(here::here("output"),"separations.csv"),
          row.names=FALSE)


data(acs_procedures)
interventions <- create_interventions(journey_acs,acs_procedures)

write.csv(interventions,
          file.path(here::here("output"),"interventions.csv"),
          row.names=FALSE)

data(acs_journey_meds_admit_discharge)

data(acs_discharge_letter_keys)

data(acs_procedures)

data(acs_door_to_balloon)

data(troponin_encounter_order)

meds_discharge <- create_meds_discharge(acs_journey_meds_admit_discharge,
                                        journey_acs,
                                        acs_discharge_letter_keys)


write.csv(meds_sum,
          file.path(here::here("output"),"acs_medication_sum.csv"),
          row.names=FALSE)
write.csv(acs_journey_meds_admit_discharge,
          file.path(here::here("output"),"acs_journey_meds_admit_discharge.csv"),
          row.names=FALSE)

write.csv(acs_discharge_letter_keys,
          file.path(here::here("output"),"acs_discharge_letter_keys.csv"),
          row.names=FALSE)

los_breaks <- c(0, 0.2083333, 0.5416667, 1, 2, 5, 10, 30, Inf)
los_labels <- c("0-4 hr", "5-12 hr", "13-24 hr", "1-2 d", "2-5 d", "5-10 d",
                "10-30 d", ">30 d")

journey_acs %<>%
  dplyr::mutate(LENGTH_OF_STAY = cut(JOURNEY_DAYS, breaks = los_breaks, labels = los_labels,
                                     right = FALSE))

data(diagnosis_data)

journey_acs <- create_stemi_code(journey_acs,diagnosis_data)

write.csv(journey_acs,
          file.path(here::here("output"),"journey_acs.csv"),
          row.names=FALSE)

write.csv(acs_procedures,
          file.path(here::here("output"),"acs_procedures.csv"),
          row.names=FALSE)

write.csv(acs_door_to_balloon,
          file.path(here::here("output"),"acs_door_to_balloon.csv"),
          row.names=FALSE)

troponin_encounter_acs <- troponin_encounter_order %>%
  dplyr::filter(ENCNTR_KEY %in% journey_acs$encntr_key) %>%
  dplyr::filter(ORDER_STATUS == "Completed")

write.csv(troponin_encounter_acs,
          file.path(here::here("output"),"troponin_encounter_acs.csv"),
          row.names=FALSE)

troponin_encounter_count <- journey_acs %>%
  dplyr::left_join(troponin_encounter_acs, by = c("encntr_key" = "ENCNTR_KEY")) %>%
  dplyr::group_by(JOURNEY_KEY) %>%
  dplyr::summarise(TROPONIN_ORDER_COUNT = sum(!is.na(ORDER_MNEMONIC))) %>%
  dplyr::ungroup()

write.csv(troponin_encounter_count,
          file.path(here::here("output"),"troponin_encounter_count.csv"),
          row.names=FALSE)

data(troponin_path)

troponin_result <- troponin_path %>%
  dplyr::filter(ENCNTR_KEY %in% journey_acs$encntr_key)

write.csv(troponin_result,
          file.path(here::here("output"),"troponin_result.csv"),
          row.names=FALSE)

troponin_result_count <- journey_acs %>%
  dplyr::left_join(troponin_result, by = c("encntr_key" = "ENCNTR_KEY")) %>%
  dplyr::group_by(JOURNEY_KEY) %>%
  dplyr::summarise(TROPONIN_RESULT_COUNT = sum(!is.na(RESULT_DISCRETE_VALUE))) %>%
  dplyr::ungroup()

write.csv(troponin_result_count,
          file.path(here::here("output"),"troponin_result_count.csv"),
          row.names=FALSE)
