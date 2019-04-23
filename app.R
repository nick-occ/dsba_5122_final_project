# final project

#libraries
library(shiny)
library(DT)
library(ggplot2)
library(RColorBrewer)
library(wordcloud2)
library(sf)
library(plotly)
library(reshape2)
library(shinythemes)

#external source
source(file = 'drugs.R')

#external file

us <- st_read("shp/states_4326.shp")

g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)

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


deathChoiceName <- c(
  "Race",
  "Age",
  "Type of Opioid"
)

deathChoiceValue <- c(
  "opioids_race_data",
  "opioids_age_data",
  "death_by_opioids"
)

#ui portion of shiny app
ui <- 
  
    navbarPage("Opioid Research",theme = shinytheme("slate"),
     tabPanel("Drug Data",
      sidebarLayout(
        sidebarPanel(
          tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "app.css")
          ),
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
                       wordcloud2Output("plot", width="100%", height="80vh"),
                       tags$b(tags$caption("* Some words were trimmed to fit into plot."))
                       ),
              tabPanel("Data",
                       DTOutput("results", height = "80vh")
                       ),
              tabPanel("Map",
                       textOutput("drugmap_header"),
                       plotlyOutput("drugmap", height = "80vh")
                       ),
            tags$b(tags$caption("* Values shown are per 100,000 people"))
          )
        
      )
     )
    ),
    tabPanel("Death Data",
      sidebarLayout(
        sidebarPanel(
          sliderInput("deathyear", "Year", min=1999, max=2015,value=1999,sep = ""),
          radioButtons("deathchoice",
                       "Show by:",
                       choiceNames = deathChoiceName,
                       choiceValues = deathChoiceValue
          )
        ),
        mainPanel(
          plotlyOutput("deathmap"),
          plotlyOutput("deathby")
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
  
  getDeathChoice <- reactive({
    input$deathchoice
  })
  
  getDeathChoiceName <- reactive({
    deathChoiceName[match(getDeathChoice(),deathChoiceValue)]
  })
  
  getState <- reactive({
    input$states
  })
  
  getOpioid <- reactive({
    getOpioidData(getState(), sym(getVariable()), 2)
  })
  
  
  getDeathYear <- reactive({
    input$deathyear
  })
  
  getDeath <- reactive({
    if (getDeathChoiceName() == "Race") {
      getRaceData(getDeathYear())  
    } else if(getDeathChoiceName() == "Age") {
      getAgeData(getDeathYear())  
    } else {
      getOpioidDeathData(getDeathYear())
    }
  })
  
  getWordCloud  <- reactive({
    getWordCloudData(getState(), sym(getVariable()), 20)
  })
  
  output$plot <- renderWordcloud2({
    wordcloud2(getWordCloud(), size=.5, gridSize=20, color="random-light", backgroundColor = "grey",minRotation = -pi/6, maxRotation = -pi/6)
  })
  
  plotRace <- function(data, title_location) {
    g <- ggplot(data,aes(reorder(variable,-value),value, fill=as.vector(unique(variable)))) + 
      geom_bar(stat="identity") + 
      ggtitle(paste("Opioid Deaths in", title_location, "During", getDeathYear())) + 
      xlab("Race") +
      ylab("Deaths") +
      theme(legend.position="none")
    
    g
  }
  
  plotDeathBy <- function(data, title_location, xlabel, ylabel, title_by) {
    g <- ggplot(data,aes(reorder(variable,-value),value, fill=as.vector(unique(variable)))) + 
      geom_bar(stat="identity") + 
      ggtitle(paste("Opioid Deaths in", title_location, "During", getDeathYear(), "By", title_by)) + 
      xlab(xlabel) +
      ylab(ylabel) +
      theme(legend.position="none")
    
    g
  }
  
  output$results <- renderDT({
    
    if (getState() == "All") {
      colnames = c("Drug Name", getVariableName())
    } else {
      colnames = c("State", "Drug Name", getVariableName())
    }
    
    datatable(
      getOpioid(), 
      options = list(
        lengthMenu = c(15, 30, 45), 
        pageLength = 15,
        initComplete = JS("
        function(settings, json) {
          $(this.api().table().header()).css({
            'background-color': '#f00',
            'color': '#fff'
          });
          $(this.api().table().body()).css({
            'background-color': '#f00',
            'color': '#fff'
          });
        }")
      ),
      colnames=colnames
    )
  })
  
  # map output
  
  output$drugmap_header <- renderText({
    paste("2016", getVariableName(), ' By State')
  })
  
  output$drugmap <- renderPlotly({
    
    state_map <- merge(us,getStateOp(sym(getVariable()),sym('Y2016'),sym('VALUE')))
    state_map$hover <- with(state_map, paste(STATE_NAME))
    
    p <-plot_geo(state_map, locationmode = 'USA-states') %>%
      add_trace(
        z = ~VALUE, text = ~hover, locations = ~STATE_ABBR,
        color = ~VALUE, colors = 'Reds'
      ) %>%
      layout(
        geo = g
      )
  })
  
  output$deathmap <- renderPlotly({
    deathmap <- merge(us,getDeath())
    deathmap$hover <- with(deathmap, paste(STATE_NAME))
    deathmap$click <- with(deathmap, paste(STATE_ABBR))
    
    deathplot <- plot_geo(deathmap, locationmode = 'USA-states', source="deathplot") %>%
      add_trace(
        z = ~total, text = ~hover, locations = ~STATE_ABBR,
        color = ~total, colors = 'Reds', key=~STATE_NAME
      ) %>%
      layout(
        geo = g
      )
  })

  output$deathby <- renderPlotly({

    s <- event_data("plotly_hover", source = "deathplot")
    
    us_data <- 
      getDeath() %>%
      select(-STATE_NAME, -total)
    
    if (length(s) > 0) {
      if (getDeathChoiceName() == "Race") {
        state_data <- 
          getRaceData(getDeathYear(),s[["key"]])
      } else if (getDeathChoiceName() == "Age") {
        state_data <- 
          getAgeData(getDeathYear(),s[["key"]])
      } else {
        state_data <- 
          getOpioidDeathData(getDeathYear(),s[["key"]])
      }
      
      state_data <- state_data %>%
        select(-total)
        
      state_data <- melt(state_data, c("STATE_NAME", "year"))
      
      plotDeathBy(state_data,s[['key']],"Race", "Death", getDeathChoiceName())
      
    } else {
      us_data <- melt(us_data, c("year"))
      
      us_data <- us_data %>%
        group_by(year, variable) %>%
        summarise(value = sum(value))
      
      plotDeathBy(us_data,"the US","Age Group", "Death", getDeathChoiceName())
    }
    
  })
}

# run shiny app
shinyApp(ui = ui, server = server)

