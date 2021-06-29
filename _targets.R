library(targets)
#source("R/dashboard.R")
library(tibble)
library(tarchetypes)
source("R/utils.R")
source("R/dashboard.R")
source("R/heartfailure.R")
source("R/snomed_icd10_stemi_pre.R")
source("R/validating_acs_pre.R")
tar_option_set(packages = c("magrittr", "tidyverse"))

## dataframe of tables we want to write to csv
input_files <- tibble::tibble(
  name =c('journey_analysis_base',
          'discharge_letter_keys',
          'journey_meds_admit_discharge',
          'diagnosis_prepr',
          'encounter_prepr',
          'procedures_grouped',
          'door_to_balloon',
          'patho_fasting',
          'form_triage_classified',
          'form_btf_vital',
          'iview_prepr',
          'patient_prepr',
          'encounter_order_prepr'),

  data_source = rlang::syms(name),
)

read<-tar_map(
  values = input_files,
  names = name,
  unlist=FALSE,
  tar_target(input, load_table(name))
)

list(
  read,
  tar_target(acs_journey, select_acs_cohort(input_journey_analysis_base)),
  tar_target(acs_journey_demo_prep, create_demographics(acs_journey,input_form_triage_classified)),
  tar_target(acs_journey_snomed,
             acs_journey %>%
               create_stemi_code(input_diagnosis_prepr)),
  tar_target(
    door_to_ballon,
    acs_journey %>%
    create_door_to_balloon(
      input_door_to_balloon,acs_journey_demo_prep)),
  tar_target(acs_procedure, extract_acs_procedures(input_procedures_grouped,acs_journey)),
  tar_target(acs_intervention,
             acs_journey %>%
             create_interventions(
               merge_mckesson_cerner_procedures(acs_procedure,door_to_ballon))),
 tar_target(acs_journey_demo,
            acs_journey_demo_prep %>%
              dplyr::left_join(acs_intervention, by = "JOURNEY_KEY") %>%
              dplyr::left_join(
                dplyr::select(acs_journey_snomed,JOURNEY_KEY,SNOMED_ACS),
                by = 'JOURNEY_KEY') %>%
              dplyr::left_join(
                dplyr::select(input_patient_prepr,PERSON_KEY,CURRENT_POSTCODE),
                by = 'PERSON_KEY')),
 tar_target(acs_discharge_letter_keys,
            acs_journey %>%
              create_discharge_letter_keys(input_discharge_letter_keys)),
 tar_target(acs_journey_meds_admit_discharge,
            acs_journey %>%
              create_meds_admit_discharge(input_journey_meds_admit_discharge)),
 tar_target(acs_meds_discharge_sum,
            acs_journey %>%
              create_meds_discharge(acs_journey_meds_admit_discharge,acs_discharge_letter_keys)),

 ## targets for snomed_icd10 analysios
 tar_target(encounter_rns,
            input_encounter_prepr %>%
              extract_rns_encounters()),
 tar_target(diagnosis_rns,
            encounter_rns %>%
              extract_encounter_diagnosis(input_diagnosis_prepr)),

 ## targets for validating acs analysis
 tar_target(val_journeys,
            input_journey_analysis_base %>%
              extract_journeys()),
 tar_target(stemi_snomed,
            input_diagnosis_prepr %>%
              extract_snomed_diagnosis()),
 tar_target(troponin_encounter_order,
            input_encounter_order_prepr %>%
              extract_troponin_order()),
 ## begining of heart failure stuff
 tar_target(hf_journey, select_hf_cohort(input_journey_analysis_base,input_diagnosis_prepr)),
 tar_target(hf_journey_demo_prep,
            hf_journey %>%
            create_demographics(input_form_triage_classified) %>%
              dplyr::left_join(
                dplyr::select(input_patient_prepr,PERSON_KEY,CURRENT_POSTCODE),
                by = 'PERSON_KEY')),
 tar_target(hf_discharge_letter_keys,
            hf_journey %>%
              create_discharge_letter_keys(input_discharge_letter_keys)),
 tar_target(hf_journey_meds_admit_discharge,
            hf_journey %>%
              create_meds_admit_discharge(input_journey_meds_admit_discharge)),
 tar_target(hf_meds_discharge_sum,
            hf_journey %>%
              create_hf_meds_discharge(hf_journey_meds_admit_discharge,hf_discharge_letter_keys)),
 tar_target(hf_echo_procedures,
            hf_journey %>%
              extract_echo_procedures(input_procedures_grouped)),
 tar_target(hf_snomed_encounters,
            input_encounter_prepr %>%
              create_hf_snomed_encounters(input_diagnosis_prepr)),
 tar_target(hf_icd10_encounters,
            input_encounter_prepr %>%
              create_hf_icd10_encounters(input_diagnosis_prepr)),
  tar_render(validating_acs_report, "vignettes/validating_acs.Rmd"),
 tar_render(snomed_icd10_stemi_report, "vignettes/snomed_icd10_stemi.Rmd"),
 tar_render(heartfailure_prelim_report, "vignettes/heartfailure_prelim.Rmd"),
 tar_render(acs_procedure_report, "vignettes/acs_procedures.Rmd")
)
