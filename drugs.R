library(dplyr)

state_data = readxl::read_xlsx('./data/PartD_Prescriber_PUF_Drug_St_16_Cleaned.xlsx')
national_data = readxl::read_xlsx('./data/PartD_Prescriber_PUF_Drug_Ntl_16_Cleaned.xlsx')
population = readxl::read_xlsx('./data/census_state_population.xlsx')

state_pop_op <-
  inner_join(state_data, population, by=c("nppes_provider_state" = "State")) %>%
  filter(opioid_drug_flag == "Y" | long_acting_opioid_drug_flag == "Y") %>%
  mutate(
    number_of_prescribers_pc = (number_of_prescribers/Y2016) * 100000,
    total_claim_count_pc = (total_claim_count/Y2016) * 100000,
    total_drug_cost_pc = (total_drug_cost/Y2016) * 100000
  ) %>%
  select(
    nppes_provider_state, 
    drug_name,
    number_of_prescribers_pc,
    total_claim_count_pc,
    total_drug_cost_pc
  )

national_pop = population %>%
  filter(State == "United States") %>%
  select(Y2016)

national_pop_op <- national_data %>%
  filter(opioid_drug_flag == "Y" | long_acting_opioid_drug_flag == "Y") %>%
  mutate(
    number_of_prescribers_pc = (number_of_prescribers/as.integer(national_pop)) * 100000,
    total_claim_count_pc = (total_claim_count/as.integer(national_pop)) * 100000,
    total_drug_cost_pc = (total_drug_cost/as.integer(national_pop)) * 100000
  ) %>%
  select(
    drug_name,
    number_of_prescribers_pc,
    total_claim_count_pc,
    total_drug_cost_pc
  )

getOpioidPrescribers <- function(state) {
  
    if (state == "All") {
      result <- national_pop_op %>%
        mutate(number_of_prescribers_pc = round(number_of_prescribers_pc,2)) %>%
        select(drug_name, number_of_prescribers_pc) %>%
        arrange(desc(number_of_prescribers_pc)) %>%
        na.omit()
      
    } else {
      result <- state_pop_op %>%
        mutate(number_of_prescribers_pc = round(number_of_prescribers_pc,2)) %>%
        select(nppes_provider_state, drug_name, number_of_prescribers_pc) %>%
        filter(nppes_provider_state == state) %>%
        arrange(desc(number_of_prescribers_pc)) %>%
        na.omit()
    }
    result
}

getOpioidClaims <- function(state) {
  
  if (state == "All") {
    result <- national_pop_op %>%
      mutate(total_claim_count_pc = round(total_claim_count_pc,2)) %>%
      select(drug_name, total_claim_count_pc) %>%
      arrange(desc(total_claim_count_pc)) %>%
      na.omit()
    
  } else {
    result <- state_pop_op %>%
      mutate(total_claim_count_pc = round(total_claim_count_pc,2)) %>%
      select(nppes_provider_state, drug_name, total_claim_count_pc) %>%
      filter(nppes_provider_state == state) %>%
      arrange(desc(total_claim_count_pc)) %>%
      na.omit()
  }
  result
}

getOpioidCost <- function(state) {
  
  if (state == "All") {
    result <- national_pop_op %>%
      mutate(total_drug_cost_pc = round(total_drug_cost_pc,2)) %>%
      select(drug_name, total_drug_cost_pc) %>%
      arrange(desc(total_drug_cost_pc)) %>%
      na.omit()
    
  } else {
    result <- state_pop_op %>%
      select(nppes_provider_state, drug_name, total_drug_cost_pc) %>%
      mutate(total_drug_cost_pc = round(total_drug_cost_pc,2)) %>%
      filter(nppes_provider_state == state) %>%
      arrange(desc(total_drug_cost_pc)) %>%
      na.omit()
  }
  result
}
  
