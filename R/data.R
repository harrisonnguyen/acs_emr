#' Encounters for the years between 2016-2018 at RNS.
#'
#' A dataset containing encounters.
#'
"encounter_data"


#' Diagnosis for the encounters given by `encounter_data`
#'
#' A dataset containing diagnosises
#'
"diagnosis_data"


#' Contains journeys as defined by ACS for `encounter_data`
#'
#'
#'
"journey_analysis_base"

#' Contains journeys according to ACS rule 2
#'
#'
#' **Relative to absolute value:**
#'
#' This rule is simpler than **ACS Rule 1**, and simply involves comparing
# 'each `result_discrete_value` with a large number, chosen to be `1000`.
#'
#' If a result exceeds that, flag its row with `abs_high_thresh_flag <- TRUE`,
#'
#' and flag all rows in that journey with `group_pass_abs_high_thresh <- TRUE`
#' "journey_acs"
"journey_acs"

#' Contains angiograms,pci, and cabg for acs cohort
"acs_procedures"

#' Contains door to balloon time for select patients
"acs_door_to_baloon"

#' Contain troponin orders based on `encounter_order`
#'
#'
#'
"troponin_encounter_order"
