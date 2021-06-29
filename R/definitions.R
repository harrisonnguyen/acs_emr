#' Returns the definition of NSLHD facilities
#' @return a vector of strings
NSLHD_facility<- function(){
  return(c("Royal North Shore", "Hornsby", "Ryde", "Manly", "Mona Vale"))
}

#' Returns the definition of CCLHD facilities
#' @return a vector of strings
CCLHD_facility<- function(){
  return(c("Gosford", "Woy Woy", "Wyong", "Long Jetty"))
}

#' Definition of internal transfer
#' @return a vector of strings
internal_transfers <- function(){
  return(c("Internal Bed / Wheelchair"
  ))
}

#' Definition for ACS names
#' @return a vector of strings
ACS_codes <- function(){
  return(c("STEMI", "NSTEMI", "UNSTABLE_ANGINA", "UNSPECIFIED_ACS"))
}

#' Definition for separation modes of interest
separation_modes <- function(){
  return(c("home",
           "death",
           "private hospital"))
}

#' Definition for an admission during business hours
business_hours <- function(){
  business <- list(
    weekday = c('Mon','Tue','Wed','Thu','Fri'),
    start_time = 8,
    end_time = 18
  )
  return(business)
}


#' Definition the exclusion criteria for medication
exclusion_criteria_meds <- function(){

  return(c("death", "dama", "private hospital"))
}

#' Definition for heart failure ICD10 codes
hf_icd_codes <- function(){

  hf_codes <- list(
    codes = c("I50.0","I50.1","I50.9","U82.2","I11.0","I13.0","I13.2"),
    names = c("Heart failure", "Left venctricular failure, unspecified",
              "Heart failure, unspecified","Chronic heart failure","Hypertensive heart disease with heart failure",
              "Hypertensive heart and chronic kidney disease with heart failure and stage 1 through stage 4 chronic kidney disease, or unspecified chronic kidney disease",
              "Hypertensive heart and chronic kidney disease with heart failure and with stage 5 chronic kidney disease, or end stage renal disease")
  )
  return(hf_codes)
}
