library(dplyr)

getOpioidPrescribers <- function(data, state) {
  
    if (state == "All") {
      result <- data %>%
        filter(opioid_drug_flag == "Y" | long_acting_opioid_drug_flag == "Y") %>%
        group_by(nppes_provider_state, drug_name) %>%
        summarise(sumPrescribers = sum(number_of_prescribers)) %>%
        arrange(desc(sumPrescribers)) %>%
        na.omit()
      
    } else {
      result <- data %>%
        filter((opioid_drug_flag == "Y" | long_acting_opioid_drug_flag == "Y") &
                 nppes_provider_state == state) %>%
        group_by(nppes_provider_state, drug_name) %>%
        summarise(sumPrescribers = sum(number_of_prescribers)) %>%
        arrange(desc(sumPrescribers)) %>%
        na.omit()
    }
    result
}

getOpioidClaims <- function(data, state) {
  
  if (state == "All") {
    result <- data %>%
      filter(opioid_drug_flag == "Y" | long_acting_opioid_drug_flag == "Y") %>%
      group_by(nppes_provider_state, drug_name) %>%
      summarise(sumClaim = sum(total_claim_count)) %>%
      arrange(desc(sumClaim)) %>%
      na.omit()
    
  } else {
    result <- data %>%
      filter((opioid_drug_flag == "Y" | long_acting_opioid_drug_flag == "Y") &
               nppes_provider_state == state) %>%
      group_by(nppes_provider_state, drug_name) %>%
      summarise(sumClaim = sum(total_claim_count)) %>%
      arrange(desc(sumClaim)) %>%
      na.omit()
  }
  result
}
  
