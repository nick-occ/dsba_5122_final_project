library(dplyr)

state_data = readxl::read_xlsx('./data/PartD_Prescriber_PUF_Drug_St_16_Cleaned.xlsx')
national_data = readxl::read_xlsx('./data/PartD_Prescriber_PUF_Drug_Ntl_16_Cleaned.xlsx')

getOpioidPrescribers <- function(state) {
  
    if (state == "All") {
      result <- national_data %>%
        filter(opioid_drug_flag == "Y" | long_acting_opioid_drug_flag == "Y") %>%
        group_by(drug_name) %>%
        summarise(sumPrescribers = sum(number_of_prescribers)) %>%
        arrange(desc(sumPrescribers)) %>%
        na.omit()
      
    } else {
      result <- state_data %>%
        filter((opioid_drug_flag == "Y" | long_acting_opioid_drug_flag == "Y") &
                 nppes_provider_state == state) %>%
        group_by(nppes_provider_state, drug_name) %>%
        summarise(sumPrescribers = sum(number_of_prescribers)) %>%
        arrange(desc(sumPrescribers)) %>%
        na.omit()
    }
    result
}

getOpioidClaims <- function(state) {
  
  if (state == "All") {
    result <- national_data %>%
      filter(opioid_drug_flag == "Y" | long_acting_opioid_drug_flag == "Y") %>%
      group_by(drug_name) %>%
      summarise(sumClaim = sum(total_claim_count)) %>%
      arrange(desc(sumClaim)) %>%
      na.omit()
    
  } else {
    result <- state_data %>%
      filter((opioid_drug_flag == "Y" | long_acting_opioid_drug_flag == "Y") &
               nppes_provider_state == state) %>%
      group_by(nppes_provider_state, drug_name) %>%
      summarise(sumClaim = sum(total_claim_count)) %>%
      arrange(desc(sumClaim)) %>%
      na.omit()
  }
  result
}

getOpioidCost <- function(state) {
  
  if (state == "All") {
    result <- national_data %>%
      filter(opioid_drug_flag == "Y" | long_acting_opioid_drug_flag == "Y") %>%
      group_by(drug_name) %>%
      summarise(sumCost = sum(total_drug_cost)) %>%
      arrange(desc(sumCost)) %>%
      na.omit()
    
  } else {
    result <- state_data %>%
      filter((opioid_drug_flag == "Y" | long_acting_opioid_drug_flag == "Y") &
               nppes_provider_state == state) %>%
      group_by(nppes_provider_state, drug_name) %>%
      summarise(sumCost = sum(total_drug_cost)) %>%
      arrange(desc(sumCost)) %>%
      na.omit()
  }
  result
}
  
