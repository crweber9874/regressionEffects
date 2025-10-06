library(shiny)
library(weberUtilties)
library(MASS)

# Load the sample data
data(satisfaction_data, package = "weberUtilties")

ui <- fluidPage(
  titlePanel("Ordered Regression Models Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Model Configuration"),
      
      selectInput("model_type", 
                  "Select Model Type:",
                  choices = c("Ordered Logit" = "logit", 
                              "Ordered Probit" = "probit"),
                  selected = "logit"),
      
      checkboxGroupInput("predictors",
                         "Select Predictors:",
                         choices = c("Income" = "income",
                                     "Education" = "education",
                                     "Age" = "age",
                                     "Experience" = "experience"),
                         selected = c("income", "education")),
      
      actionButton("fit_model", "Fit Model", class = "btn-primary"),
      
      hr(),
      
      h4("Prediction"),
      numericInput("pred_income", "Income:", value = 50, min = 0, max = 150),
      numericInput("pred_education", "Education (years):", value = 16, min = 12, max = 20),
      numericInput("pred_age", "Age:", value = 35, min = 22, max = 65),
      numericInput("pred_experience", "Experience (years):", value = 10, min = 0, max = 40),
      
      actionButton("predict", "Predict", class = "btn-success")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Model Summary",
                 h4("Fitted Model Summary"),
                 verbatimTextOutput("model_summary"),
                 h4("Coefficient Table"),
                 tableOutput("coef_table")),
        
        tabPanel("Predictions",
                 h4("Predicted Probabilities"),
                 tableOutput("pred_probs"),
                 h4("Predicted Class"),
                 verbatimTextOutput("pred_class")),
        
        tabPanel("Data",
                 h4("Sample Data"),
                 dataTableOutput("data_table"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive value to store fitted model
  fitted_model <- reactiveVal(NULL)
  
  # Fit model when button is clicked
  observeEvent(input$fit_model, {
    req(input$predictors)
    
    # Build formula
    predictors_str <- paste(input$predictors, collapse = " + ")
    formula_str <- paste("satisfaction ~", predictors_str)
    formula_obj <- as.formula(formula_str)
    
    # Fit model based on selection
    if (input$model_type == "logit") {
      model <- fit_ordered_logit(formula_obj, data = satisfaction_data)
    } else {
      model <- fit_ordered_probit(formula_obj, data = satisfaction_data)
    }
    
    fitted_model(model)
  })
  
  # Display model summary
  output$model_summary <- renderPrint({
    req(fitted_model())
    summary(fitted_model())
  })
  
  # Display coefficient table
  output$coef_table <- renderTable({
    req(fitted_model())
    summary_obj <- summary_ordered(fitted_model())
    summary_obj$coefficients
  }, rownames = TRUE, digits = 4)
  
  # Make predictions
  prediction_results <- eventReactive(input$predict, {
    req(fitted_model())
    
    # Create new data for prediction
    new_data <- data.frame(
      income = input$pred_income,
      education = input$pred_education,
      age = input$pred_age,
      experience = input$pred_experience
    )
    
    # Get predictions
    pred_class <- predict_ordered(fitted_model(), newdata = new_data, type = "class")
    pred_probs <- predict_ordered(fitted_model(), newdata = new_data, type = "probs")
    
    list(class = pred_class, probs = pred_probs)
  })
  
  # Display predicted probabilities
  output$pred_probs <- renderTable({
    req(prediction_results())
    probs <- prediction_results()$probs
    data.frame(
      Category = colnames(probs),
      Probability = as.numeric(probs)
    )
  }, digits = 4)
  
  # Display predicted class
  output$pred_class <- renderPrint({
    req(prediction_results())
    cat("Predicted Satisfaction Level:", as.character(prediction_results()$class), "\n")
  })
  
  # Display data table
  output$data_table <- renderDataTable({
    satisfaction_data
  }, options = list(pageLength = 10))
}

shinyApp(ui = ui, server = server)
