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

ui <- fluidPage(
   
   titlePanel("Model Making"),
   
   sidebarLayout(
     sidebarPanel(
       # Delay all updates to charts until user is ready
       # submitButton(text = "Regress!")
       
       # Covariates for patient information states
       pickerInput(
         inputId = "patientStates", 
         label = "Possible patient variables", 
         choices = patient_states_util, 
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
         label = "Possible disease variables", 
         choices = disease_states_util, 
         options = list(
           `actions-box` = TRUE, 
           size = 10,
           `selected-text-format` = "count > 3"
         ), 
         multiple = TRUE
       ),
       
       # Covariates for surgery states
       pickerInput(
         inputId = "surgStates", 
         label = "Possible surgery variables", 
         choices = surgery_states_util, 
         options = list(
           `actions-box` = TRUE, 
           size = 10,
           `selected-text-format` = "count > 3"
         ), 
         multiple = TRUE
       ),
       
       # Covariates for laboratory states
       pickerInput(
         inputId = "labStates", 
         label = "Possible disease  variables", 
         choices = lab_states_util, 
         options = list(
           `actions-box` = TRUE, 
           size = 10,
           `selected-text-format` = "count > 3"
         ), 
         multiple = TRUE
       ),
       
       # Button to proceed with regression
       actionBttn("regress", "Regress!")
       
     ),
     
     mainPanel(
       fluidRow(
         textOutput("allCovariates")
       ),
       fluidRow(
         "poop"
       )
     )
  )
) 

server <- function(input, output) {
  output$allCovariates = renderText({
    c(input$patientStates,
      input$surgStates,
      input$labStates,
      input$diseaseStates
      )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

