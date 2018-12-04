library(broom)
library(caret)
library(plotly)
library(shiny)
library(shinyWidgets)
library(tidyverse)

source("./utils.R")

colectomies = read.csv(file = './sample_colectomies.csv') 

ui = fluidPage(
  
  titlePanel("Colectomy Success Model Tool"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      helpText("You are in charge of creating a new model for predicting post-colectomy infections! 
                Choose a few variables below and see how your model turns out. You may see how 
                statistically significant your coefficients are as well as your model's 
               predictive ability."),
      
      
      # Picker for patient variables
      pickerInput(
        inputId = "patientStates", 
        label = "Possible patient variables", 
        choices = patient_states_util, 
        options = list(
          `actions-box` = TRUE, 
          size = 12,
          `selected-text-format` = "count > 3"
        ), 
        multiple = TRUE),
      
      # Picker for disease variables
      pickerInput(
        inputId = "diseaseStates", 
        label = "Possible disease variables", 
        choices = disease_states_util, 
        options = list(
          `actions-box` = TRUE,
          size = 10,
          `selected-text-format` = "count > 3"
        ), 
        multiple = TRUE),
      
      # Picker for surgery variables
      pickerInput(
        inputId = "surgStates", 
        label = "Possible surgery variables", 
        choices = surgery_states_util, 
        options = list(
          `actions-box` = TRUE, 
          size = 10,
          `selected-text-format` = "count > 3"
        ), 
        multiple = TRUE),
      
      # Picker for laboratory variables
      pickerInput(
        inputId = "labStates", 
        label = "Possible laboratory variables", 
        choices = lab_states_util, 
        options = list(
          `actions-box` = TRUE, 
          size = 10,
          `selected-text-format` = "count > 3"
        ), 
        multiple = TRUE
      ),
      
      actionBttn("submit", "Regress!")
      
      
      ),
    
    
    mainPanel(
      tabsetPanel(
        tabPanel("Coefficient Magnitudes", plotlyOutput("coeffsGraph")),
        tabPanel("Statistical Significance", plotlyOutput("statsGraph"))
      )
      
      )
    )
  )
    
server <- function(input, output) {

  # Compile all the covariates into a formula for the regression
  fmla = reactive({
    as.formula(
      paste("any_ssi ~ ", paste(c(input$patientStates,
                                           input$surgStates,
                                           input$labStates,
                                           input$diseaseStates), collapse = "+"))
    )
  })
  
  # Create a model from the formula
  log.model = reactive({
    glm(fmla(), data = colectomies, family = binomial())
  })
  
  
  # Act upon a user pressing the Regress button
  observeEvent(input$submit, {
    
    output$coeffsGraph = renderPlotly({
      tidy(log.model()) %>%
        plot_ly(
          x = ~term,
          y = ~estimate, 
          type = "bar",
          text = mapCoeffsToText(.$estimate),
          marker = list(
            color = mapCoeffsToColor(.$estimate)
          ) 
        ) %>% 
        layout(xaxis = list(title = "Coefficient Term", tickangle = -45),
               yaxis = list(title = "Coefficient Magnitude"),
               margin = list(b = 100))
    })
    
    output$statsGraph = renderPlotly({
      tidy(log.model()) %>% 
        mutate(OR = exp(estimate),
               low.bound = exp(estimate - 1.96 * std.error),
               high.bound = exp(estimate + 1.96 * std.error)) %>% 
        plot_ly(
          x = ~term,
          y = ~OR,
          type = "scatter",
          mode = "markers",
          color = ~term,
          error_y = ~list(array = c(.$low.bound, .$high.bound))
        ) %>% 
        layout(xaxis = list(title = "Coefficient Term", tickangle = -45),
               yaxis = list(title = "Adjusted Odds Ratio"),
               margin = list(b = 100))
    })
  
    })
  }

# Run the application 
shinyApp(ui = ui, server = server)

