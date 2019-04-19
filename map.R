library(dplyr)
library(sf)
library(ggplot2)

#external files
state_data = readxl::read_xlsx('./data/PartD_Prescriber_PUF_Drug_St_16_Cleaned.xlsx')
national_data = readxl::read_xlsx('./data/PartD_Prescriber_PUF_Drug_Ntl_16_Cleaned.xlsx')
population = readxl::read_xlsx('./data/census_state_population.xlsx')

getStateOp <- function(col_name, year, new_col) {
  VALUE <- quo_name(new_col)
  
  state_pop_op <-
    inner_join(state_data, population, by=c("nppes_provider_state" = "State")) %>%
    filter(opioid_drug_flag == "Y" | long_acting_opioid_drug_flag == "Y") %>%
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
