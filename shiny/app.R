library(shiny)
library(broom)
library(flexdashboard)
library(tidyverse)
library(plotly)
library(shiny)
library(shinyWidgets)
source("../utils.R")

colectomies = read.csv(file = '../procedure10.csv')  %>% 
  select(-starts_with("flg_"), -starts_with("e_")) %>% 
  select_if(unlist(map(., is_mostly_intact), use.names = FALSE)) %>% 
  prettify_names(.) %>% 
  mutate(any_ssi =  (postop_ssi_super + postop_ssi_deep + postop_ssi_organspace) >= 1)

ui <- fillPage(
  # This row contains the graphs to be made for the regression
  fillRow(
    fillCol(plotOutput("coeffsGraph"), height = "100%", width = "100%"),
    fillCol(plotOutput("CIGraph"), height = "100%", width = "100%"),
    flex = c(3, 2),
    height = "50%"
    ),
    
  # This row contains controls for picking the regression covariates
  fillRow(
    # Button to proceed with regression
    fillCol(
      titlePanel("Colectomy Infection Prediction"),
      helpText("Think you can predict what will cause an infection? Create a model and then create your regression to see how it fares!"),
      actionButton("submit", "Regress!"), width = "100%"),
    
    # Covariates for patient and disease information
    fillCol(
      
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
      
      pickerInput(
        inputId = "diseaseStates", 
        label = "Possible disease variables", 
        choices = disease_states_util, 
        options = list(
          `actions-box` = TRUE,
          size = 10,
          `selected-text-format` = "count > 3"
          ), 
        multiple = TRUE)
      ),
     
    # Covariates for surgery and lab information
     fillCol(
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
         )),
    flex = c(1, 1, 1),
    height = "30%"
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
    
    output$coeffsGraph = renderPlot({
      broom::tidy(log.model()) %>% 
        ggplot(data = ., aes(x = term, y = estimate)) +
        geom_bar(stat = "identity") +
        labs(
          title = "Magnitude of logistic regression coefficients",
          x = "Covariate",
          y = "Coefficient estimate"
        )
    })
    
    output$CIGraph = renderPlot({
      broom::tidy(log.model()) %>% 
        ggplot(data = ., aes(x = term, y = estimate)) +
        geom_bar(stat = "identity")
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

