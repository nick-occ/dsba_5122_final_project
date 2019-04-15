library(dplyr)

getOpioidPrescribers <- function(data, state) {
  
    if (state == "All") {
      result <- data %>%
        filter(opioid_drug_flag == "Y" | long_acting_opioid_drug_flag == "Y") %>%
        group_by(nppes_provider_state, generic_name) %>%
        summarise(sumPrescribers = sum(number_of_prescribers)) %>%
        arrange(desc(sumPrescribers)) %>%
        na.omit()
      
    } else {
      result <- data %>%
        filter((opioid_drug_flag == "Y" | long_acting_opioid_drug_flag == "Y") &
                 nppes_provider_state == state) %>%
        group_by(nppes_provider_state, generic_name) %>%
        summarise(sumPrescribers = sum(number_of_prescribers)) %>%
        arrange(desc(sumPrescribers)) %>%
        na.omit()
    }
    result
}

getOpioidData <- function(data, state, variable) {
  
  if (state == "All") {
    result <- data %>%
      filter(opioid_drug_flag == "Y" | long_acting_opioid_drug_flag == "Y") %>%
      group_by(nppes_provider_state, generic_name) %>%
      summarise(sumResult = sum(number_of_prescribers)) %>%
      arrange(desc(sumPrescribers)) %>%
      na.omit()
    
  } else {
    result <- data %>%
      filter((opioid_drug_flag == "Y" | long_acting_opioid_drug_flag == "Y") &
               nppes_provider_state == state) %>%
      group_by(nppes_provider_state, generic_name) %>%
      summarise(sumPrescribers = sum(number_of_prescribers)) %>%
      arrange(desc(sumPrescribers)) %>%
      na.omit()
  }
  result
}
  
