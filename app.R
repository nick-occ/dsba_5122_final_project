# final project

#libraries
library(shiny)
library(DT)
library(ggplot2)
library(RColorBrewer)
library(wordcloud2)
library(sf)
library(plotly)

#external source
source(file = 'drugs.R')

#external file

us <- st_read("shp/states_4326.shp")

variableChoiceName <- c(
  "Number of Prescribers",
  "Number of Claims",
  "Drug Cost"
)

variableChoiceValue <- c(
  "number_of_prescribers",
  "total_claim_count",
  "total_drug_cost"
)


#ui portion of shiny app
ui <- navbarPage("Opioid Research",
   tabPanel("Drug Data",
    sidebarLayout(
      sidebarPanel(
        conditionalPanel(
          condition = "input.drugTab == 'Word Cloud' | input.drugTab == 'Data'",
            selectInput("states", "States", choices=c("All",state.name))  
          ),
        radioButtons("variable",
                     "Show by:",
                     choiceNames = variableChoiceName,
                     choiceValues = variableChoiceValue
                       )
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
  
  getVariable <- reactive({
    input$variable
  })
  
  getVariableName <- reactive({
    variableChoiceName[match(getVariable(),variableChoiceValue)]
  })
  
  getState <- reactive({
    input$states
  })
  
  getOpioid <- reactive({
    getOpioidData(getState(), sym(getVariable()), 2)
  })
  
  getWordCloud  <- reactive({
    getWordCloudData(getState(), sym(getVariable()), 25)
  })
  
  output$plot <- renderWordcloud2({
    wordcloud2(getWordCloud(), size=.2, gridSize=-30)
  })
  
  output$results <- renderDT({
    
    if (getState() == "All") {
      colnames = c("Drug Name", getVariableName())
    } else {
      colnames = c("State", "Drug Name", getVariableName())
    }
    
    datatable(
      getOpioid(), 
      options = list(
        lengthMenu = c(10, 30, 50), 
        pageLength = 10
      ),
      colnames=colnames
    )
  })
  
  # map output
  
  output$drugmap <- renderPlotly({
    g <- list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      showlakes = TRUE,
      lakecolor = toRGB('white')
    )
    
    state_map <- merge(us,getStateOp(sym(getVariable()),sym('Y2016'),sym('VALUE')))
    state_map$hover <- with(state_map, paste(STATE_NAME))
    
    title <- paste("2016 ", getVariableName(), ' By State')
    
    p <-plot_geo(state_map, locationmode = 'USA-states') %>%
      add_trace(
        z = ~VALUE, text = ~hover, locations = ~STATE_ABBR,
        color = ~VALUE, colors = 'Reds'
      ) %>%
      layout(
        title = title,
        geo = g
      )
  })
}

# run shiny app
shinyApp(ui = ui, server = server)

