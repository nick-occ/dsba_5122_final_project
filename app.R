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

data = readxl::read_xlsx('./data/PartD_Prescriber_PUF_Drug_St_16_Cleaned.xlsx')

states = readxl::read_xlsx('./data/us_state_coords.xlsx')

# Define UI for application that draws a histogram
ui <- navbarPage("Opioid Research",
   tabPanel("Drug Data",
    sidebarLayout(
      sidebarPanel(
        selectInput("states", "States", choices=c("All", states$State)),
        radioButtons("variable", "Show by:", c(
          "Number of Prescribers" = "number_of_prescribers",
          "Number of Claims" = "total_claim_count"
        ))
      ),
      mainPanel(
        textOutput("test"),
        plotOutput("plot", width = "100%", height = "400px"),
        DT::dataTableOutput("results") 
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  opioidPres <- reactive({
    getOpioidPrescribers(data, input$states)
  })
  
  opioidClaims <- reactive({
    getOpioidClaims(data, input$states)
  })
  
  getVariable <- reactive({
    input$variable
  })

  output$test <- renderText({
    getVariable()
  })
  
  output$results <- DT::renderDataTable({
    if(getVariable() == "number_of_prescribers") {
      DT::datatable(
        opioidPres(), 
        options = list(
          lengthMenu = c(10, 30, 50), 
          pageLength = 10
          )
        )
    } else {
      DT::datatable(
        opioidClaims(), 
        options = list(
          lengthMenu = c(10, 30, 50), 
          pageLength = 10
        )
      )
    }
  })
  
  # output$plot <- renderPlot({
  #   ggplot(data=opioidPres(),aes(x=reorder(generic_name,-sumPrescribers), y=sumPrescribers)) + 
  #     geom_bar(stat="identity") + 
  #     coord_flip()
  # })
  
  
    output$plot <- renderPlot({
      
      if(getVariable() == "number_of_prescribers") {
        wordcloud::wordcloud(words = opioidPres()$drug_name, 
                             freq = opioidPres()$sumPrescribers, 
                             min.freq = 1000,
                             colors=brewer.pal(8, "Dark2"))
      } else {
        wordcloud::wordcloud(words = opioidClaims()$drug_name, 
                             freq = opioidClaims()$sumClaim, 
                             min.freq = 1000,
                             max.freq = 10000,
                             colors=brewer.pal(8, "Dark2"))
      }
    })
  
  
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

