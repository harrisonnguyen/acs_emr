
get_data <- function(){
  load(journey_acs)
  load(acs_procedures)



}

data(acs_journey_meds_admit_discharge)

data(acs_discharge_letter_keys)

data(journey_acs)
data(acs_procedures)

data(acs_door_to_balloon)

data(troponin_encounter_order)

antiplatelets = c("P2Y12 receptor blocker", "Other antiplatelet")
exclusions <- c("death", "dama", "private hospital")

meds_sum <- acs_journey_meds_admit_discharge %>%
  dplyr::group_by(JOURNEY_KEY) %>%
  dplyr::summarise(
    letter = any(ENCNTR_KEY %in% acs_discharge_letter_keys),
    Aspirin = sum(MEDICATION == "aspirin" & DISCHARGE_MED, na.rm = TRUE),
    `P2Y12 & others` = sum(MEDICATION %in% antiplatelets & DISCHARGE_MED, na.rm = TRUE),
    Anticoagulant = sum(MEDICATION == "warfarin/other anticoagulant" & DISCHARGE_MED, na.rm = TRUE),
    `A2RB/ACE Inhibitor` = sum(MEDICATION == "A2RB/ACE Inhibitor" & DISCHARGE_MED, na.rm = TRUE),
    `Beta blocker` = sum(MEDICATION == "Beta blocker" & DISCHARGE_MED, na.rm = TRUE),
    Statin = sum(MEDICATION == "Statin" & DISCHARGE_MED, na.rm = TRUE)
  ) %>%
  dplyr::ungroup()

write.csv(meds_sum,
          file.path(here::here("output"),"acs_medication_sum.csv"),
          row.names=FALSE)
write.csv(acs_journey_meds_admit_discharge,
          file.path(here::here("output"),"acs_journey_meds_admit_discharge.csv"),
          row.names=FALSE)

write.csv(acs_discharge_letter_keys,
          file.path(here::here("output"),"acs_discharge_letter_keys.csv"),
          row.names=FALSE)

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
