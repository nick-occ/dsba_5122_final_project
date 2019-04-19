library(dplyr)
library(sf)
library(ggplot2)
library(lazyeval)

#external files
state_data = readxl::read_xlsx('./data/PartD_Prescriber_PUF_Drug_St_16_Cleaned.xlsx')
national_data = readxl::read_xlsx('./data/PartD_Prescriber_PUF_Drug_Ntl_16_Cleaned.xlsx')
population = readxl::read_xlsx('./data/census_state_population.xlsx')

getStateOp <- function(col_name, year) {
  state_pop_op <-
    inner_join(state_data, population, by=c("nppes_provider_state" = "State")) %>%
    filter(opioid_drug_flag == "Y" | long_acting_opioid_drug_flag == "Y") %>%
    mutate(
      pc=(!!col_name/!!year) * 100000
    )
  
  state_pop_op
}

getStateOpPresc <- function() {
  state_pop_op <-
    inner_join(state_data, population, by=c("nppes_provider_state" = "State")) %>%
    filter(opioid_drug_flag == "Y" | long_acting_opioid_drug_flag == "Y") %>%
    mutate(
      number_of_prescribers_pc = (number_of_prescribers/Y2016) * 100000,
    ) %>%
    select(
      nppes_provider_state, 
      number_of_prescribers_pc
    ) %>%
    na.omit() %>%
    mutate(
      STATE_NAME = nppes_provider_state,
      PRESCRIBERS = round(number_of_prescribers_pc,2)
    ) %>%
    select(STATE_NAME, PRESCRIBERS) %>%
    group_by(STATE_NAME) %>%
    summarise(PRESCRIBERS = sum(PRESCRIBERS)) %>%
    arrange(desc(PRESCRIBERS))
  
  state_pop_op
}


getStateOpClaim <- function() {
  state_pop_op <-
    inner_join(state_data, population, by=c("nppes_provider_state" = "State")) %>%
    filter(opioid_drug_flag == "Y" | long_acting_opioid_drug_flag == "Y") %>%
    mutate(
      total_claim_count_pc = (total_claim_count/Y2016) * 100000,
    ) %>%
    select(
      nppes_provider_state, 
      total_claim_count_pc
    ) %>%
    na.omit() %>%
    mutate(
      STATE_NAME = nppes_provider_state,
      CLAIMS = round(total_claim_count_pc,2)
    ) %>%
    select(STATE_NAME, CLAIMS) %>%
    group_by(STATE_NAME) %>%
    summarise(CLAIMS = sum(CLAIMS)) %>%
    arrange(desc(CLAIMS))
  
  state_pop_op
}

getStateOpCost <- function() {
  state_pop_op <-
    inner_join(state_data, population, by=c("nppes_provider_state" = "State")) %>%
    filter(opioid_drug_flag == "Y" | long_acting_opioid_drug_flag == "Y") %>%
    mutate(
      total_drug_cost_pc = (total_drug_cost/Y2016) * 100000
    ) %>%
    select(
      nppes_provider_state, 
      total_drug_cost_pc
    ) %>%
    na.omit() %>%
    mutate(
      STATE_NAME = nppes_provider_state,
      COST = round(total_drug_cost_pc)
    ) %>%
    select(STATE_NAME, COST) %>%
    group_by(STATE_NAME) %>%
    summarise(COST = sum(COST)) %>%
    arrange(desc(COST))
  
  state_pop_op
}

