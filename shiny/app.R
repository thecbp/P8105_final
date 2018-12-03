library(broom)
library(caret)
library(plotly)
library(shiny)
library(shinyWidgets)
library(tidyverse)

source("../utils.R")

colectomies = read.csv(file = '../procedure10.csv')  %>% 
  select(-starts_with("flg_"), -starts_with("e_")) %>% 
  select_if(unlist(map(., is_mostly_intact), use.names = FALSE)) %>% 
  prettify_names(.) %>% 
  mutate(any_ssi = factor((postop_ssi_super + postop_ssi_deep + postop_ssi_organspace) >= 1))

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
        tabPanel("Coefficient Magnitudes", plotOutput("coeffsGraph")),
        tabPanel("Statistical Significance", plotOutput("statsGraph"))
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
    
    output$coeffsGraph = renderPlot({
      tidy(log.model()) %>% 
        mutate(OR = exp(estimate)) %>% 
        ggplot(data = ., aes(x = term, y = estimate, fill = term)) +
        geom_bar(stat = "identity") +
        labs(
          x = "Covariate",
          y = "Coefficient estimate"
        )
      })
    
    output$statsGraph = renderPlot({
      tidy(log.model()) %>% 
        mutate(OR = exp(estimate),
               low.bound = exp(estimate - 1.96 * std.error),
               high.bound = exp(estimate + 1.96 * std.error)) %>% 
        ggplot(data = ., aes(x = term, y = OR, color = term)) +
        geom_point() +
        geom_errorbar(aes(ymin = low.bound, ymax = high.bound)) +
        geom_hline(yintercept = 1, alpha = 0.3, color = "red") +
        labs(
          x = "Covariate",
          y = "Odds ratio estimate"
        )
    })
    })
  }

# Run the application 
shinyApp(ui = ui, server = server)

