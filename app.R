# final project

#libraries
library(shiny)
library(dplyr)
library(DT)
library(ggplot2)
library(RColorBrewer)
library(wordcloud2)
library(sf)
library(plotly)

#external source
source(file = 'drugs.R')
source(file = 'map.R')

#external file
states = readxl::read_xlsx('./data/us_state_coords.xlsx')

us <- st_read("shp/states_4326.shp")

#ui portion of shiny app
ui <- navbarPage("Opioid Research",
   tabPanel("Drug Data",
    sidebarLayout(
      sidebarPanel(
        conditionalPanel(
          condition = "input.drugTab == 'Word Cloud' | input.drugTab == 'Data'",
            selectInput("states", "States", choices=c("All",states$State))  
          ),
        radioButtons("variable", "Show by:", c(
          "Number of Prescribers" = "number_of_prescribers_pc",
          "Number of Claims" = "total_claim_count_pc",
          "Drug Cost" = "total_drug_cost_pc"
        ))
      ),
      mainPanel(
        tabsetPanel(id="drugTab",
          tabPanel("Word Cloud",
                   wordcloud2Output("plot"),
                   tags$b(tags$caption("* Some words were trimmed to fit into plot."))
                   ),
          tabPanel("Data",
                   DTOutput("results")
                   ),
          tabPanel("Map",
                   plotlyOutput("drugmap")
                   ),
        tags$b(tags$caption("* Values shown are per 100,000 people"))
      )
    )
  )
   )
)

# server portion of shiny app
server <- function(input, output) {
  opioidPres <- reactive({
    getOpioidPrescribers(input$states)
  })
  
  opioidClaims <- reactive({
    getOpioidClaims(input$states)
  })
  
  opioidCost <- reactive({
    getOpioidCost(input$states)
  })
  
  getVariable <- reactive({
    input$variable
  })
  
  getState <- reactive({
    input$states
  })
  
  getWordCloudPresc <- reactive({
    wcOpioidPrescribers(input$states)
  })
  
  getWordCloudClaims <- reactive({
    wcOpioidClaims(input$states)
  })
  
  getWordCloudCost <- reactive({
    wcOpioidCost(input$states)
  })
  
  output$plot <- renderWordcloud2({
    
    if(getVariable() == "number_of_prescribers_pc") {
      wordcloud2(getWordCloudPresc(), size=.2, gridSize=-30)
    } else if (getVariable() == "total_drug_cost_pc") {
      wordcloud2(getWordCloudCost(), size=.2, gridSize=-30)
    } else {
      wordcloud2(getWordCloudClaims(), size=.2, gridSize=-30)
    }
    
  })
  
  output$results <- renderDT({
    if(getVariable() == "number_of_prescribers_pc") {
      
      if (getState() == "All") {
        colnames = c("Drug Name", "Total Prescribers")
      } else {
        colnames = c("State", "Drug Name", "Total Prescribers")
      }
      
      datatable(
        opioidPres(), 
        options = list(
          lengthMenu = c(10, 30, 50), 
          pageLength = 10
        ),
        colnames=colnames
      )
    } else if (getVariable() == "total_drug_cost_pc") {
      
      if (getState() == "All") {
        colnames = c("Drug Name", "Total Drug Cost")
      } else {
        colnames = c("State", "Drug Name", "Total Drug Cost")
      }
      
      datatable(
        opioidCost(), 
        options = list(
          lengthMenu = c(10, 30, 50), 
          pageLength = 10
        ),
        colnames=colnames
      )
    } else {
      
      if (getState() == "All") {
        colnames = c("Drug Name", "Total Claims")
      } else {
        colnames = c("State", "Drug Name", "Total Claims")
      }
      
      datatable(
        opioidClaims(), 
        options = list(
          lengthMenu = c(10, 30, 50), 
          pageLength = 10
        ),
        colnames=colnames
      )
    }
  })
  
  # map output
  
  output$drugmap <- renderPlotly({
    g <- list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      showlakes = TRUE,
      lakecolor = toRGB('white')
    )
  
    if(getVariable() == "number_of_prescribers_pc") {
      state_map <- merge(us,getStateOpPresc())
      state_map$hover <- with(state_map, paste(STATE_NAME))
  
      p <-plot_geo(state_map, locationmode = 'USA-states') %>%
        add_trace(
          z = ~PRESCRIBERS, text = ~hover, locations = ~STATE_ABBR,
          color = ~PRESCRIBERS, colors = 'Reds'
        ) %>%
        layout(
          title = '2016 Prescribers By State',
          geo = g
        )
    } else if (getVariable() == "total_drug_cost_pc") {
      state_map <- merge(us,getStateOpCost())
      state_map$hover <- with(state_map, paste(STATE_NAME))
      
      p <-plot_geo(state_map, locationmode = 'USA-states') %>%
        add_trace(
          z = ~COST, text = ~hover, locations = ~STATE_ABBR,
          color = ~COST, colors = 'Reds'
        ) %>%
        layout(
          title = '2016 Cost By State',
          geo = g
        )
    } else {
      state_map <- merge(us,getStateOpClaim())
      state_map$hover <- with(state_map, paste(STATE_NAME))
      
      p <-plot_geo(state_map, locationmode = 'USA-states') %>%
        add_trace(
          z = ~CLAIMS, text = ~hover, locations = ~STATE_ABBR,
          color = ~CLAIMS, colors = 'Reds'
        ) %>%
        layout(
          title = '2016 Claims By State',
          geo = g
        )
    }
  })
}

# run shiny app
shinyApp(ui = ui, server = server)

