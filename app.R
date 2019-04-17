#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(ggplot2)
library(RColorBrewer)

source(file = 'drugs.R')

states = readxl::read_xlsx('./data/us_state_coords.xlsx')

# Define UI for application that draws a histogram
ui <- navbarPage("Opioid Research",
   tabPanel("Drug Data",
    sidebarLayout(
      sidebarPanel(
        selectInput("states", "States", choices=c("All",states$State)),
        radioButtons("variable", "Show by:", c(
          "Number of Prescribers" = "number_of_prescribers_pc",
          "Number of Claims" = "total_claim_count_pc",
          "Drug Cost" = "total_drug_cost_pc"
        ))
      ),
      mainPanel(
        plotOutput("plot", width = "100%", height = "400px"),
        DT::dataTableOutput("results"),
        br(),
        tags$caption("* Values shown are per 100,000 people")
      )
    )
  )
)

# Define server logic required to draw a histogram
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
  
  output$results <- DT::renderDataTable({
    if(getVariable() == "number_of_prescribers_pc") {
      
      if (getState() == "All") {
        colnames = c("Drug Name", "Total Prescribers")
      } else {
        colnames = c("State", "Drug Name", "Total Prescribers")
      }
      
      DT::datatable(
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
      
      DT::datatable(
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
      
      DT::datatable(
        opioidClaims(), 
        options = list(
          lengthMenu = c(10, 30, 50), 
          pageLength = 10
        ),
        colnames=colnames
      )
    }
  })
  
  # output$plot <- renderPlot({
  #   ggplot(data=opioidPres(),aes(x=reorder(generic_name,-sumPrescribers), y=sumPrescribers)) + 
  #     geom_bar(stat="identity") + 
  #     coord_flip()
  # })
  
  
    output$plot <- renderPlot({
      
      if(getVariable() == "number_of_prescribers_pc") {
        wordcloud::wordcloud(words = opioidPres()$drug_name, 
                             freq = opioidPres()$number_of_prescribers_pc, 
                             min.freq = 1000,
                             colors=brewer.pal(8, "Dark2"))
      } else if (getVariable() == "total_drug_cost_pc") {
        wordcloud::wordcloud(words = opioidCost()$drug_name, 
                             freq = opioidCost()$total_drug_cost_pc, 
                             min.freq = 1000,
                             colors=brewer.pal(8, "Dark2"))
      } else {
        wordcloud::wordcloud(words = opioidClaims()$drug_name, 
                             freq = opioidClaims()$total_claim_count_pc, 
                             min.freq = 1000,
                             colors=brewer.pal(8, "Dark2"))
      }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

