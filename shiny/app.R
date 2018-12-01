library(shiny)
library(flexdashboard)
library(tidyverse)
library(plotly)
library(shiny)
library(shinyWidgets)
source("../utils.R")

colectomies = read.csv(file = '../procedure10.csv') %>% 
  select(-starts_with("flg_"), -starts_with("e_")) %>% 
  select_if(unlist(map(., is_mostly_intact), use.names = FALSE)) %>% 
  prettify_names(.)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   titlePanel("Model Making"),
   
   sidebarLayout(
     sidebarPanel(
       # Delay all updates to charts until user is ready
       # submitButton(text = "Regress!")
       
       # Covariates for patient information states
       pickerInput(
         inputId = "patientStates", 
         label = "Possible patient states", 
         choices = c("Uninsured", "Asian", "Latino"), 
         options = list(
           `actions-box` = TRUE, 
           size = 12,
           `selected-text-format` = "count > 3"
         ), 
         multiple = TRUE
       ),
       
       # Covariates for disease states
       pickerInput(
         inputId = "diseaseStates", 
         label = "Possible disease states", 
         choices = colnames(colectomies), 
         options = list(
           `actions-box` = TRUE, 
           size = 10,
           `selected-text-format` = "count > 3"
         ), 
         multiple = TRUE
       )
     ),
     
     mainPanel(
       textOutput("allCovariates")
     )
  )
) 

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$allCovariates = renderText({
    c(input$diseaseStates, input$patientStates)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

