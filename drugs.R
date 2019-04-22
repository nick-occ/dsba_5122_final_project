library(dplyr)
library(reshape2)

#external files
state_data = readxl::read_xlsx('./data/PartD_Prescriber_PUF_Drug_St_16_Cleaned.xlsx')
national_data = readxl::read_xlsx('./data/PartD_Prescriber_PUF_Drug_Ntl_16_Cleaned.xlsx')
population = readxl::read_xlsx('./data/census_state_population.xlsx')
opioids_race_data = readxl::read_xlsx('./data/opioid_death_by_race_cleaned.xlsx')
opioids_age_data = readxl::read_xlsx('./data/opioid_overdose_death_by_age_group.xlsx')
death_by_opioids = readxl::read_xlsx('./data/death_by_opioid.xlsx')
  

headstate_pop_op <-
  inner_join(state_data, population, by=c("nppes_provider_state" = "State")) %>%
  mutate(
    number_of_prescribers = (number_of_prescribers/Y2016) * 100000,
    total_claim_count = (total_claim_count/Y2016) * 100000,
    total_drug_cost = (total_drug_cost/Y2016) * 100000
  ) %>%
  select(
    nppes_provider_state, 
    drug_name,
    number_of_prescribers,
    total_claim_count,
    total_drug_cost
  )

national_pop = population %>%
  filter(State == "United States") %>%
  select(Y2016)

national_pop_op <- national_data %>%
  mutate(
    number_of_prescribersc = (number_of_prescribers/as.integer(national_pop)) * 100000,
    total_claim_count = (total_claim_count/as.integer(national_pop)) * 100000,
    total_drug_cost = (total_drug_cost/as.integer(national_pop)) * 100000
  ) %>%
  select(
    drug_name,
    number_of_prescribers,
    total_claim_count,
    total_drug_cost
  )

getRaceData <- function(inYear, state = "All") {
  result <- opioids_race_data %>%
    filter(year == as.character(inYear))
  
  if (state != "All") {
    result <- result %>%
      filter(STATE_NAME == state)
  }
  
  result
}

getAgeData <- function(inYear, state="United States") {
  result <- opioids_age_data %>%
    filter(year == as.character(inYear))
  
  if (state != "United States") {
    result <- result %>%
      filter(STATE_NAME == state)
  }
  
  result
}

getOpioidDeathData <- function(inYear, state="United States") {
  result <- death_by_opioids %>%
    filter(year == as.character(inYear))
  
  if (state != "United States") {
    result <- result %>%
      filter(STATE_NAME == state)
  }
  
  result
}

getOpioidData <- function(state, variable, rounding) {
  result <- getDataSource(state) %>%
    mutate(pc = round(!!variable,rounding)) %>%
    arrange(desc(pc))
  
  if (state != "All") {
    result <- result %>%
      filter(nppes_provider_state == state) %>%
      select(nppes_provider_state, drug_name, pc)
  } else {
    result <- result %>%
      select(drug_name, pc)
  }
  
  result <- result %>% 
    na.omit()
  
  result
}

getDataSource <- function(state) {
  if (state == "All") {
    ds = national_pop_op
  } else {
    ds = state_pop_op
  }
  
  ds
}

getWordCloudData <- function(state, variable, charLength) {
  result <- getDataSource(state) %>%
    mutate(
      freq = round(!!variable),
      word = substr(drug_name,1,charLength)
    ) 
  
  if (state != "All") {
    result <- result %>%
      filter(nppes_provider_state == state)
  }
  
  result <- result %>%
    select(word, freq) %>%
    arrange(desc(freq)) %>%
    na.omit()
  
  result
}

getStateOp <- function(col_name, year, new_col) {
  VALUE <- quo_name(new_col)
  
  state_pop_op <-
    inner_join(state_data, population, by=c("nppes_provider_state" = "State")) %>%
    mutate(
      pc=(!!col_name/!!year) * 100000
    ) %>%
    select(
      nppes_provider_state, 
      pc
    ) %>%
    na.omit() %>%
    mutate(
      STATE_NAME = nppes_provider_state,
      VALUE = round(pc,2)
    ) %>%
    select(STATE_NAME, VALUE) %>%
    group_by(STATE_NAME) %>%
    summarise(VALUE = sum(VALUE)) %>%
    arrange(desc(VALUE))
  
  state_pop_op
  
}

