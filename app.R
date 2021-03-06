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
library(memoise)

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
            allows the user to examine which are the most common drugs used and if these variables could aid in their research towards what factors
            contribute to opioid addiction."),
          br(),
          p("The word cloud gives the user a high level view of what the most common opioids are for the state and variable selected.
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
            tags$b(tags$footer("* Values shown are per 100,000 people"))
          )
        
      )
     )
    ),
    # end opioid drug tab
    # opioid death data
    tabPanel("Death Data",
      sidebarLayout(
        sidebarPanel(
          h3("Input"),
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
          ),
          # show download only for the data tab
          conditionalPanel(
            condition = "input.deathTab == 'Data'",
            downloadButton("downloadDeathData", "Download")
          ),
          h3("Description"),
          p("This section focuses on looking into how many opioid related deaths occured in the United States.
            The user can see data for different years and they have the ability to animate through the years by clicking the play button 
            to see how patterns change over time.  The bottom plot will show the distribution of deaths by different variables such as race, 
            age and type of opioid.  The user can hover over a state to see the exact number of deaths and the bottom plot will also change to
            show the death distribtion for the state currently hovered over.  This visualization will help the user focus on where a high amount
            of deaths occur, the patterns over time and if there is any correlation with a persons age or race and the classification of opioid that was the
            cause of death.
            "),
          h3("References"),
          tags$a(href="https://public.opendatasoft.com/explore/dataset/opioid-overdose-deaths-by-race-and-ethnicity/export/",
                 "OpenDataSoft - Opioid Overdose Deaths by Race/Ethnicity"
          ),
          br(),
          tags$a(href="https://www.kff.org/other/state-indicator/opioid-overdose-deaths-by-age-group/?currentTimeframe=0&sortModel=%7B%22colId%22:%22Location%22,%22sort%22:%22asc%22%7D",
                 "Henry J Kaiser - Family Foundation - Opioid Overdose Deaths by Age Group"
          ),
          br(),
          tags$a(href="https://www.kff.org/other/state-indicator/opioid-overdose-deaths-by-type-of-opioid/?currentTimeframe=2&sortModel=%7B%22colId%22:%22Location%22,%22sort%22:%22asc%22%7D",
                 "Henry J Kaiser - Family Foundation - Opioid Overdose Deaths by Type of Opioid"
          )
        ),
        mainPanel(
          textOutput("death_header"),
          tabsetPanel(id="deathTab",
            tabPanel("Map",
              plotlyOutput("deathmap", height = "40vh"),
              plotlyOutput("deathby", height = "40vh")
            ),
            tabPanel("Data",
             DTOutput("deathresults", height = "50vh")
            ),
            tags$b(tags$footer("* Values shown are per 100,000 people"))
          )
        )
      )
    ),
    # end opioid death data
    # prescriber rate data
    tabPanel("Prescriber Rates",
     sidebarLayout(
       sidebarPanel(
         h3("Input"),
         sliderInput(
           "presrateyear", 
           "Year", 
           min=START_YEAR,
           max=END_YEAR,
           value=START_YEAR,
           sep = "",
           animate = animationOptions(interval = ANIMATE_INTERVAL, loop = TRUE)
           ),
           # show download only for the data tab
           conditionalPanel(
             condition = "input.presRateTab == 'Data'",
             downloadButton("downloadPresRateData", "Download")
           ),
         h3("Description"),
         p("This section focuses on looking into prescription rates in the United States.
            The user can see data for different years and they have the ability to animate through the years by clicking the play button 
           to see how patterns change over time.  The user can hover on a state to see the prescriber rate for that state.  When the user clicks the
          the map it will generate a county view of the state that shows if the prescription amounts have increased, decreased or stabilized between 
          2010 and 2015.  These plots will help the user focus on where high prescriber rates occur to visualize patterns over time and also get a detailed view
          at the county level.
           "),
         h3("References"),
         tags$a(href="https://www.cdc.gov/drugoverdose/maps/rxrate-maps.html",
                "CDC - Centers for Disease Control - U.S. Opioid Prescribing Rate Maps"
         ),
         br(),
         tags$a(href="https://data.world/associatedpress/opioid-prescriptions-2010-2015",
                "data.world - Opioid Prescriptions, 2010 + 2015"
         )
       ),
       mainPanel(
         textOutput("presrate_header"),
         tabsetPanel(id="presRateTab",
            tabPanel("Map",
               plotlyOutput("presratemap", height = "40vh"),
               tags$b(tags$caption("* Values shown are per 100 US residents")),
               plotOutput("prescounty", height = "40vh")          
            ),
            tabPanel("Data",
               DTOutput("presrateresults", height = "75vh"),
               tags$b(tags$caption("* Values shown are per 100 US residents"))
            )
         )
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
       h3("Input"),
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
         ),
       h3("Description"),
       p("The analysis sections focuses on what the findings are based on the data from the previous sections and help the users draw conclusions from the data.
        The first visualization allows a user to select a state and compare prescription rates with the number of opioid related deaths over time.  We included the
        national average as a reference to see the level of impact of the selected state over time.
        The idea is that reasearchers can use the previous sections to help guide there focus to certain states and then use the analysis tab to see
        the relationships between the different dataset.
        The second visualization is a radar chart that takes all the variables related to deaths and shows how they compare to one another.
        This not only allows the researcher to find patterns about which race, age groups and type of opioids are high and where to focus research,
        but also how the different variable change over time.
         "),
       h3("References"),
       tags$a(href="https://public.opendatasoft.com/explore/dataset/opioid-overdose-deaths-by-race-and-ethnicity/export/",
              "OpenDataSoft - Opioid Overdose Deaths by Race/Ethnicity"
       ),
       br(),
       tags$a(href="https://www.kff.org/other/state-indicator/opioid-overdose-deaths-by-age-group/?currentTimeframe=0&sortModel=%7B%22colId%22:%22Location%22,%22sort%22:%22asc%22%7D",
              "Henry J Kaiser - Family Foundation - Opioid Overdose Deaths by Age Group"
       ),
       br(),
       tags$a(href="https://www.kff.org/other/state-indicator/opioid-overdose-deaths-by-type-of-opioid/?currentTimeframe=2&sortModel=%7B%22colId%22:%22Location%22,%22sort%22:%22asc%22%7D",
              "Henry J Kaiser - Family Foundation - Opioid Overdose Deaths by Type of Opioid"
       ),
       br(),
       tags$a(href="https://www.cdc.gov/drugoverdose/maps/rxrate-maps.html",
              "CDC - Centers for Disease Control - U.S. Opioid Prescribing Rate Maps"
       )
     ),
     mainPanel(
      textOutput("presratedeath_header"),
      plotlyOutput("presrate_death", height = "40vh"),
      textOutput("radar_header"),
      plotlyOutput("radar_death", height = "40vh"),
      tags$b(tags$caption("* Prescription rates shown are per 100 US residents")),
      br(),
      tags$b(tags$caption("* Deaths shown are per 100,000 people")),tags$b(tags$caption("* Values shown are per 100 US residents"))
     )
   )
  ),
  # end prescriber rate data
  tabPanel("About",
    h2("Project Information",style="text-align:center"),
    tags$article("This was a research project for DSBA-5122 at UNC Charlotte.  
      For the project we wanted to explore data related to opioids, 
      in an effort to better understand and get more insight into the opioid epidemic. 
      Our domain problem is one for a researcher wanting to explore the connection between 
      prescriber rates of opioid prescriptions and opioid related deaths both in the country
      as a whole and drilling down to the state level. The first part we wanted to look at was data on prescribers. 
      This data would allow the researcher to see the distribution of opioids
      across the US and also find the most commonly prescribed opioids. 
      The second part of the data involved finding information about deaths that occur from opioid overdoses
      in the United States. This would also allow the researcher to drill down to the state level. 
      Another level of detail that we felt would be an important task for the researcher is to categorize 
      these deaths into different types of groups such as race and age. 
      This would add another level of detail and help identify groups that are suffering from opioid addiction, 
      which would then allow researchers to provide information and where attention needs to be focused the most to 
      combat the opioid epidemic.", style="padding:2%"),
    h2("Get the Code at Github",style="text-align:center"),
    div(
      p("All our code is open source, please feel free run our code and use as is or develop new features:"),
            a(href="https://github.com/nick-occ/dsba_5122_final_project", "nick-occ/dsba_5122_final_project")
      , style='text-align:center'
    ),
    br(),
    h2("Ideas for New Development",style="text-align:center"),
    div(
      p("To review the features we implemented and see new features we would like to add in a future release visit:"),
      a(href="https://github.com/nick-occ/dsba_5122_final_project/projects/1",
        "https://github.com/nick-occ/dsba_5122_final_project/projects/1")
      , style='text-align:center'
    ),
    br(),
    h2("Final Report",style="text-align:center"),
    div(
      p("Our final report is available for review at:"),
      a(href="https://bookdown.org/nocchipi/dsba-5122-final_report/",
        "https://bookdown.org/nocchipi/dsba-5122-final_report/")
      , style='text-align:center'
    ),
    br(),
    div(
      h2("Created By:"),
      h5("- Nicholas Occhipinti"),
      h5("- Karyn Cook"),
      h5("- Ziyin Liu")
      ,style="text-align:center")
  )
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

  
  # DRUG DATA OUTPUT
  
  # drug data heading
  output$drug_header <- renderText({
    paste("2016 Medicare Part D Prescriber Data")
  })
  
  # word cloud header
  output$drugwc_header <- renderText({
    paste("2016 Word Cloud of Most Common Opioids by", getVariableName())
  })
  
  # word cloud
  output$plot <- renderWordcloud2({
    wordcloud2(getWordCloud(), size=.5, gridSize=20, color="random-light", backgroundColor = "grey",minRotation = -pi/6, maxRotation = -pi/6)
  })
  
  # data grid header
  output$drugdata_header <- renderText({
    paste("2016 Most Common Opioids by", getVariableName())
  })
  
  # data grid
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
          })
        }")
      ),
      colnames=colnames
    )
  })
  
  # download button for data grid
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(getVariableName(),"_",getState(),".csv", sep = "")
    },
    content = function(file) {
      write.csv(getOpioid(), file, row.names = FALSE)
    }
  )
  
  # map header
  output$drugmap_header <- renderText({
    paste("2016 Map by", getVariableName())
  })
  
  # map of drugs by different prescriber variables
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
  
  # END DRUG DATA OUTPUT
  
  
  # DEATH DATA OUTPUT
  
  # death data header
  output$death_header <- renderText({
    paste("Opioid Deaths", "in", getDeathYear(), "by", getDeathChoiceName())
  })
  
  # map showing total deaths in the US
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
        geo = g,
        title = paste("Opioid Deaths in the US", "During", getDeathYear())
      )
  })

  # event driven plot when user hovers over map
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
  
  output$deathresults <- renderDT({
    datatable(
      getDeath() %>%
        arrange(desc(total)), 
      options = list(
        lengthMenu = c(15, 30, 45), 
        pageLength = 15,
        initComplete = JS("
                          function(settings, json) {
                          $(this.api().table().header()).css({
                          'background-color': '#f00',
                          'color': '#fff'
                          })
                          }")
      )
    )
  })
  
  # download button for data grid
  output$downloadDeathData <- downloadHandler(
    filename = function() {
      paste("opioid_deaths_by_",getDeathChoiceName(),"_",getDeathYear(),".csv", sep = "")
    },
    content = function(file) {
      write.csv(getDeath() %>%
                  arrange(desc(total)), file, row.names = FALSE)
    }
  )
  
  # function to generate bar chart when user hovers over map
  plotDeathBy <- function(data, title_location, xlabel, ylabel, title_by) {
    g <- ggplot(data,aes(reorder(variable,-value),value, fill=as.vector(unique(variable)))) + 
      geom_bar(stat="identity") + 
      ggtitle(paste("Opioid Deaths in", title_location, "During", getDeathYear(), "By", title_by)) + 
      xlab(xlabel) +
      ylab(ylabel) +
      theme(legend.position="none")
    
    g
  }
  
  # END DEATH DATA OUTPUT
  
  # PRESCRIPTION RATE OUTPUT
  
  # prescription rate header
  output$presrate_header <- renderText({
    paste("Prescriber Rates in", getPresRateYear())
  })
  
  # prescription rate map
  output$presratemap <- renderPlotly({
    presRate <- getPresRate()
    
    presRate$hover <- with(presRate, paste(STATE_NAME))
    
    p <-plot_geo(presRate, locationmode = 'USA-states', source="presrateplot") %>%
      add_trace(
        z = ~prescriber_rate, text = ~hover, locations = ~STATE_ABBR,
        color = ~prescriber_rate, colors = 'Reds', key=~STATE_NAME
      ) %>%
      layout(
        geo = g,
        title = paste("Opioid Prescription Rate in the US", "During", getPresRateYear())
      )
  })
  
  output$downloadPresRateData <- downloadHandler(
    filename = function() {
      paste("prescription_rate_by_state_",getPresRateYear(),".csv", sep = "")
    },
    content = function(file) {
      write.csv(getPresRate() %>%
                  arrange(desc(prescriber_rate)), file, row.names = FALSE)
    }
  )
  
  output$prescounty <- renderPlot({
      s <- event_data("plotly_click", source = "presrateplot")
      
      if (length(s) > 0) {
        getPresRateCountyData(s[['key']])
      }
  })
  
  output$presrateresults <- renderDT({
    datatable(
      getPresRate() %>%
        arrange(desc(prescriber_rate)), 
      options = list(
        lengthMenu = c(15, 30, 45), 
        pageLength = 15,
        initComplete = JS("
                          function(settings, json) {
                          $(this.api().table().header()).css({
                          'background-color': '#f00',
                          'color': '#fff'
                          })
                          }")
      )
        )
  })
  
  # END PRESCRIPTION RATE OUTPUT
  
  # ANALYSIS OUTPUT
  
  # header for pres. rate vs death plot
  output$presratedeath_header <- renderText({
    paste("Opioid Prescriber Rate and Death Analysis for ", getStateAnalysis())
  })
  
  # plot for pres. rate vs death plot
  output$presrate_death <- renderPlotly({
    g <- ggplot(getPresRateDeath(),aes(year,prescriber_rate)) + 
      geom_smooth() + 
      geom_point(aes(color=deaths,size=deaths)) + 
      xlab("Year") + 
      ylab("Prescription Rate") + 
      scale_color_gradient2(low="#a3d4ff", high="#1f4364")
    
    us_avg <- merge(getPrescriberRateUSAvg(),getDeathUSAvg())
    
    max_year <- us_avg %>%
      filter(year == as.numeric(max(us_avg$year))-1) %>%
      select(year, prescriber_rate)
    
    g <- g + 
      geom_line(data=us_avg, aes(year,prescriber_rate), color="red") +
      geom_text(data=max_year, aes((year),(prescriber_rate + 5), label="National Average"), color="red") +
      geom_point(data=us_avg, aes(size=deaths),color="red")
    
    
    g
    
  })
  
  # header for radar plot
  output$radar_header <- renderText({
    paste("Opioid Death Variables for", getStateAnalysis(), "in", getYearAnalysis())
  })
  
  # radar plot for death variables
  output$radar_death <- renderPlotly({
    d <- getRadarDeath()
    
    variable <- as.vector(d$variable)
    value <- as.vector(d$value)
    
    plotly_radar(value,variable, max(d$value))
  })
  # END ANALYSIS OUTPUT  
}

# run shiny app
shinyApp(ui = ui, server = server)

