library(dplyr)
library(reshape2)
library(scales)
library(plotly)
library(memoise)
library(sf)

# external files
state_data = readxl::read_xlsx('./data/PartD_Prescriber_PUF_Drug_St_16_Cleaned.xlsx')
national_data = readxl::read_xlsx('./data/PartD_Prescriber_PUF_Drug_Ntl_16_Cleaned.xlsx')
population = readxl::read_xlsx('./data/census_state_population.xlsx')
opioids_race_data = readxl::read_xlsx('./data/opioid_death_by_race_cleaned.xlsx')
opioids_age_data = readxl::read_xlsx('./data/opioid_overdose_death_by_age_group.xlsx')
death_by_opioids = readxl::read_xlsx('./data/death_by_opioid.xlsx')
prescriber_rates = readxl::read_xlsx('./data/opioid_prescriber_rates.xlsx')
county<- st_read("shp/counties_4326.shp")

# HELPER FUNCTIONS

# get population for year and state that is passed
getPopulation <- function(popYear, state) {
  
  result <- population %>%
    filter(State == state) %>%
    select(!!popYear)
  
  as.integer(result)
}

# return either state or national data
getDataSource <- function(state) {
  if (state == "All") {
    ds = national_pop_op
  } else {
    ds = state_pop_op
  }
  
  ds
}

# END HELPER FUNCTIONS

# DRUG DATA FUNCTIONS

# get opioid data by state and variable that are passed in
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

# get data for word cloud
getWordCloudData <- memoise(function(state, variable, charLength) {
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
})

# get state data for column that is passed for the given year
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

state_pop_op <-
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
    number_of_prescribers = (number_of_prescribers/as.integer(national_pop)) * 100000,
    total_claim_count = (total_claim_count/as.integer(national_pop)) * 100000,
    total_drug_cost = (total_drug_cost/as.integer(national_pop)) * 100000
  ) %>%
  select(
    drug_name,
    number_of_prescribers,
    total_claim_count,
    total_drug_cost
  )

# END DRUG DATA FUNCTIONS

# DEATH DATA FUNCTIONS

# get race data for year and state that is passed
getRaceData <- function(inYear, state = "All") {
  
  result <- opioids_race_data %>%
    filter(year == as.character(inYear))
  
  result <- result %>%
    rowwise()%>%
    mutate(pop = getPopulation(paste("Y",inYear,sep=""), STATE_NAME))
    
  
  if (state != "All") {
    result <- result %>%
      filter(STATE_NAME == state)
  }
  
  # get result per 100,000 people based on population
  result <- result %>%
    mutate(
      total = ((total/pop) * 100000),
      black_non_hispanic = ((black_non_hispanic/pop) * 100000),
      hispanic = ((hispanic/pop) * 100000),
      white_non_hispanic = ((white_non_hispanic/pop) * 100000),
      unknown = ((unknown/pop) * 100000),
    )
  
  result
}

# get age data for year and state that is passed
getAgeData <- function(inYear, state="United States") {
  result <- opioids_age_data %>%
    filter(year == as.character(inYear))
  
  result <- result %>%
    rowwise()%>%
    mutate(pop = getPopulation(paste("Y",inYear,sep=""), STATE_NAME))
  
  if (state != "United States") {
    result <- result %>%
      filter(STATE_NAME == state)
  }
  # get result per 100,000 people based on population
  result <- result %>%
    mutate(
      total = ((total/pop) * 100000),
      age_0_24 = (age_0_24/pop) * 100000,
      age_25_34 = (age_25_34/pop) * 100000,
      age_35_44 = (age_35_44/pop) * 100000,
      age_45_54 = (age_45_54/pop) * 100000,
      age_55_over = (age_55_over/pop) * 100000
    )
  
  result
}

# get death data for year and state that is passed
getOpioidDeathData <- function(inYear, state="United States") {
  result <- death_by_opioids %>%
    filter(year == as.character(inYear))
  
  result <- result %>%
    rowwise()%>%
    mutate(pop = getPopulation(paste("Y",inYear,sep=""), STATE_NAME))
  
  if (state != "United States") {
    result <- result %>%
      filter(STATE_NAME == state)
  }
  
  # get result per 100,000 people based on population
  result <- result %>%
    mutate(
      total = ((total/pop) * 100000),
      nat_semisynthetic = ((nat_semisynthetic/pop) * 100000),
      synthetic_non_methadone = ((synthetic_non_methadone/pop) * 100000),
      methadone = ((methadone/pop) * 100000),
      heroin = ((heroin/pop) * 100000)
    )
  
  result
}

# get death data by state include multiplier for weighted value
getDeathDataByState <- memoise(function(data, multiplier, state="United States") {
  result <- data %>%
    filter(STATE_NAME == state & year >= 2010)
  
  result <- result %>%
    rowwise()%>%
    mutate(pop = getPopulation(paste("Y",year,sep=""), state))

  result <- result %>%
    mutate(
      deaths = ((total/pop) * multiplier),
    ) %>%
    select(STATE_NAME, year, deaths)
  
  result
})

# END DEATH DATA FUNCTIONS

# PRESCRIPTION RATE FUNCTIONS

# get prescription rate data for a year passed
getPresRateData <- function(inYear) {
  result <-prescriber_rates %>%
    filter(year == as.character(inYear))
  
  result
}

# get prescription rate data by state passed
getPresRateDataByState <- memoise(function(state) {
  result <-prescriber_rates %>%
    filter(STATE_NAME == state) %>%
    select(STATE_NAME,year,prescriber_rate)

  result
})



getPresRateCountyData <- memoise(function(state) {
  select_county <- county %>%
    filter(STATE_NAME == state)
  
  p <- ggplot() + 
    geom_sf(data=select_county, aes(label=NAME,fill=X2010_2015_)) + theme_bw() + 
    ggtitle(paste('Opioid Prescription Amounts By County Between 2010 to 2015 in', state)) +
    labs(fill = "Change") + 
    theme(plot.title = element_text(hjust = 0.5))
  
  p
  
})

# END PRESCRIPTION RATE FUNCTIONS

# ANALYSIS FUNCTIONS

# merge prescription rates with death data by state that is passed
getPresRateDeathData <- memoise(function(state) {
  m <- merge(getPresRateDataByState(state),getDeathDataByState(opioids_race_data,100000,state))
  m
})

# get data for radar plot analysis
getRadarDeathData <- memoise(function(inYear, state) {
  o <- getOpioidDeathData(inYear, state) %>%
    filter(STATE_NAME == state) %>%
    select(-total,-pop)
  
  o
  
  a <- getAgeData(inYear, state) %>%
    filter(STATE_NAME == state) %>%
    select(-total,-pop, -unknown)
  
  a
  
  r <- getRaceData(inYear, state) %>%
    filter(STATE_NAME == state) %>%
    select(-total,-pop, -unknown)
  
  r
  
  m <- merge(o,a)
  
  m
  
  n <- merge(m,r)
  
  n <- n %>%
    select(-year) %>%
    group_by( STATE_NAME )
  
  melted <- melt(n)
  
  melted
  
})

# create radar plot from data that is passed in
plotly_radar <- function(inR, inTheta, inMax) {
  p <- plot_ly(
    type = 'scatterpolar',
    r = inR,
    theta = inTheta,
    fill = 'toself'
  ) %>%
    layout(
      polar = list(
        radialaxis = list(
          visible = T,
          range = c(0,inMax)
        )
      ),
      showlegend = F
    )
  
  p
}

getPrescriberRateUSAvg <- function() {
  result <- prescriber_rates %>%
    group_by(year) %>%
    summarise(prescriber_rate = mean(prescriber_rate)) %>%
    select(year,prescriber_rate)
  
  result
}

getDeathUSAvg <- function() {
  result <- getDeathDataByState(opioids_age_data,100000) %>%
    select(year, deaths)
  
  result
}

# END ANALYSIS FUNCTIONS