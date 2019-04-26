# libraries
library(shiny)
library(DT)
library(ggplot2)
library(RColorBrewer)
library(wordcloud2)
library(sf)
library(plotly)
library(reshape2)
library(shinythemes)

# external source
source(file = 'drugs.R')

# constants
START_YEAR <- 2010
END_YEAR <- 2015
ANIMATE_INTERVAL <- 3000

# shapefile of the United States
us <- st_read("shp/states_4326.shp")

# plotly map properties
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)

# choices for radio button in drug data section
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

# choices for radio button in death data section
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

# ui portion of shiny app
ui <- 
    # navbar menu 
    navbarPage("Opioid Research",theme = shinytheme("slate"),
     # opioid drug tab
     tabPanel("Opioid Drug Data",
      sidebarLayout(
        sidebarPanel(
          h3("Input"),
          # show state input for only word cloud and data, not map
          conditionalPanel(
            condition = "input.drugTab == 'Word Cloud' | input.drugTab == 'Data'",
              selectInput("states", "States", choices=c("All",state.name))  
            ),
          radioButtons("variable",
                       "Show by:",
                       choiceNames = variableChoiceName,
                       choiceValues = variableChoiceValue
                         ),
          # show download only for the data tab
          conditionalPanel(
            condition = "input.drugTab == 'Data'",
            downloadButton("downloadData", "Download")
          ),
          h3("Description"),
          p("This section focuses on looking into Medicare Part D Prescriber data to see what the most common drugs classified as opioids are being
            prescribed.  The user has the ability to select by different variables from the dataset to see how the drug distribution changes.  This
            allows the user to examine which are the most common drugs used and if these variable could aid in their research towards what factors
            contribute to opioid addiction."),
          br(),
          p("The word cloud gives the user a high level view of what the most common opioids for the state and variable selected.
            The data view gives the user a complete list of the drugs ranked and the user can also click the Download button to export a CSV.
            The map view will allow the user to see how different states compare based on the variable selected from the radio button.
            "),
          h3("References"),
          tags$a(href="https://www.cms.gov/Research-Statistics-Data-and-Systems/Statistics-Trends-and-Reports/Medicare-Provider-Charge-Data/PartD2016.html",
            "Medicare Part D Prescriber Data"
            )
        ), 
        mainPanel(
            tags$head(
              tags$link(rel = "stylesheet", type = "text/css", href = "app.css")
            ),
            textOutput("drug_header"),
            tabsetPanel(id="drugTab",
              tabPanel("Word Cloud",
                       textOutput("drugwc_header"),
                       tags$br(),
                       wordcloud2Output("plot", width="100%", height="75vh"),
                       tags$b(tags$caption("* Some words were trimmed to fit into plot."))
                       ),
              tabPanel("Data",
                       textOutput("drugdata_header"),
                       DTOutput("results", height = "75vh")
                       ),
              tabPanel("Map",
                       textOutput("drugmap_header"),
                       tags$br(),
                       plotlyOutput("drugmap", height = "75vh")
                       ),
            tags$b(tags$caption("* Values shown are per 100,000 people"))
          )
        
      )
     )
    ),
    # end opioid drug tab
    # opioid death data
    tabPanel("Death Data",
      sidebarLayout(
        sidebarPanel(
          sliderInput(
            "deathyear", 
            "Year", 
            min=START_YEAR, 
            max=END_YEAR, 
            value=START_YEAR,
            sep = "", 
            animate = animationOptions(interval = ANIMATE_INTERVAL, loop = TRUE)
            ),
          radioButtons("deathchoice",
                       "Show by:",
                       choiceNames = deathChoiceName,
                       choiceValues = deathChoiceValue
          )
        ),
        mainPanel(
          textOutput("death_header"),
          plotlyOutput("deathmap"),
          plotlyOutput("deathby")
        )
      )
    ),
    # end opioid death data
    # prescriber rate data
    tabPanel("Prescriber Rates",
     sidebarLayout(
       sidebarPanel(
         sliderInput(
           "presrateyear", 
           "Year", 
           min=START_YEAR,
           max=END_YEAR,
           value=START_YEAR,
           sep = "",
           animate = animationOptions(interval = ANIMATE_INTERVAL, loop = TRUE)
           )
       ),
       mainPanel(
         textOutput("presrate_header"),
         plotlyOutput("presratemap")
       )
    )
  ),
  # end prescriber rate data
  # analysis data
  tabPanel("Analysis",
   sidebarLayout(
     sidebarPanel(
       tags$head(
         tags$link(rel = "stylesheet", type = "text/css", href = "app.css")
       ),
       selectInput("states_analysis", "States", choices=c(state.name)),
       tags$hr(),
       sliderInput(
         "year_analysis", 
         "Year", 
         min=START_YEAR,
         max=END_YEAR,
         value=START_YEAR,
         sep = "",
         animate = animationOptions(interval = ANIMATE_INTERVAL, loop = TRUE)
         )
     ),
     mainPanel(
      textOutput("presratedeath_header"),
      plotlyOutput("presrate_death"),
      textOutput("radar_header"),
      plotlyOutput("radar_death")
     )
   )
  )
  # end prescriber rate data
)

# server portion of shiny app
server <- function(input, output) {
  
  # DRUG REACTIVES
  
  # drug data reactives for variable choice and choice name
  getVariable <- reactive({
    input$variable
  })
  
  getVariableName <- reactive({
    variableChoiceName[match(getVariable(),variableChoiceValue)]
  })
  
  # drug data reactives for state input
  getState <- reactive({
    input$states
  })
  
  # reactive to get opioid data for grid
  getOpioid <- reactive({
    getOpioidData(getState(), sym(getVariable()), 2)
  })
  
  # reactive to get opioid data for word cloud
  getWordCloud  <- reactive({
    getWordCloudData(getState(), sym(getVariable()), 20)
  })
  
  # END DRUG REACTIVES
  
  
  # DEATH REACTIVES
  
  # death data reactives for variable choice and choice name
  getDeathChoice <- reactive({
    input$deathchoice
  })
  
  getDeathChoiceName <- reactive({
    deathChoiceName[match(getDeathChoice(),deathChoiceValue)]
  })
  
  # death data reactive for year
  getDeathYear <- reactive({
    input$deathyear
  })
  
  # reactive to get death data
  getDeath <- reactive({
    if (getDeathChoiceName() == "Race") {
      getRaceData(getDeathYear())  
    } else if(getDeathChoiceName() == "Age") {
      getAgeData(getDeathYear())  
    } else {
      getOpioidDeathData(getDeathYear())
    }
  })
  
  # END DEATH REACTIVES
  
  
  # PRESCRIPTION RATE REACTIVES
  
  # reactive to get year from slider input
  getPresRateYear <- reactive({
    input$presrateyear
  })
  
  # reactive to get prescription rate data
  getPresRate <- reactive({
    getPresRateData(getPresRateYear())
  })
  
  # END PRESCRIPTION RATE REACTIVES
  
  
  # ANALYSIS REACTIVES
  
  # analysis reactive for state input
  getStateAnalysis <- reactive({
    input$states_analysis
  })
  
  # analysis reactive for year input
  getYearAnalysis <- reactive({
    input$year_analysis
  })
  
  # analysis reactive to prescription rate vs deaths
  getPresRateDeath <- reactive({
    getPresRateDeathData(getStateAnalysis())
  })
  
  
  # analysis reactive to get data for radar plot
  getRadarDeath <- reactive({
    getRadarDeathData(getYearAnalysis(),getStateAnalysis())
  })
  
  # END ANALYSIS REACTIVES

  
  # drug data output
  
  output$plot <- renderWordcloud2({
    wordcloud2(getWordCloud(), size=.5, gridSize=20, color="random-light", backgroundColor = "grey",minRotation = -pi/6, maxRotation = -pi/6)
  })
  
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
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(getVariableName(),"_",getState(),".csv", sep = "")
    },
    content = function(file) {
      write.csv(getOpioid(), file, row.names = FALSE)
    }
  )
  
  # map output
  
  output$drug_header <- renderText({
    paste("2016 Medicare Part D Prescriber Data")
  })
  
  output$drugwc_header <- renderText({
    paste("2016 Word Cloud of Most Common Opioids by", getVariableName())
  })
  
  output$drugdata_header <- renderText({
    paste("2016 Most Common Opioids by", getVariableName())
  })
  
  output$drugmap_header <- renderText({
    paste("2016 Map by", getVariableName())
  })
  
  output$death_header <- renderText({
    paste("Opioid Deaths", "in", getDeathYear(), "by", getDeathChoiceName())
  })
  
  output$presrate_header <- renderText({
    paste("Prescriber Rates in", getPresRateYear())
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
      select(-STATE_NAME, -total, -pop)
    
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
        select(-total, -pop)
        
      state_data <- melt(state_data, c("STATE_NAME", "year"))
      
      plotDeathBy(state_data,s[['key']],getDeathChoiceName(), "Death", getDeathChoiceName())
      
    } else {
      us_data <- melt(us_data, c("year"))
      
      us_data <- us_data %>%
        group_by(year, variable) %>%
        summarise(value = sum(value))
      
      plotDeathBy(us_data,"the US",getDeathChoiceName(), "Death", getDeathChoiceName())
    }
    
  })
  
  output$presratemap <- renderPlotly({
    presRate <- getPresRate()
    
    # state_map <- merge(us,getStateOp(sym(getVariable()),sym('Y2016'),sym('VALUE')))
    presRate$hover <- with(presRate, paste(STATE_NAME))
    
    p <-plot_geo(presRate, locationmode = 'USA-states') %>%
      add_trace(
        z = ~prescriber_rate, text = ~hover, locations = ~STATE_ABBR,
        color = ~prescriber_rate, colors = 'Reds'
      ) %>%
      layout(
        geo = g
      )
  })
  
  output$presratedeath_header <- renderText({
    paste("Opioid Prescriber Rate and Death Analysis for ", getStateAnalysis())
  })
  
  output$presrate_death <- renderPlotly({
    ggplot(getPresRateDeath(),aes(year,prescriber_rate)) + 
      geom_smooth() + 
      geom_point(aes(color=deaths,size=deaths)) + 
      xlab("Year") + 
      ylab("Prescription Rate")
  })
  
  output$radar_header <- renderText({
    paste("Opioid Death Variables for", getStateAnalysis(), "in", getYearAnalysis())
  })
  
  output$radar_death <- renderPlotly({
    d <- getRadarDeath()
    
    variable <- as.vector(d$variable)
    value <- as.vector(d$value)
    
    plotly_radar(value,variable, max(d$value))
  })
}

# run shiny app
shinyApp(ui = ui, server = server)

