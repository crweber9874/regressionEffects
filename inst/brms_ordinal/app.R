# inst/shinyapp/app.R
library(shiny)
library(shinythemes)
library(DT)
library(brms)
library(ggplot2)
library(dplyr)

# UI Definition
ui <- fluidPage(
  theme = shinytheme("flatly"),

  titlePanel("BRMS Ordinal Regression Analysis"),

  sidebarLayout(
    sidebarPanel(
      width = 3,

      # Step 1: Data Selection
      h4("1. Select Data"),
      selectInput("dataset",
                  "Choose a dataset from workspace:",
                  choices = NULL),
      actionButton("refresh_data", "Refresh Data List"),
      hr(),

      # Step 2: Variable Selection
      h4("2. Select Variables"),

      # Outcome variable (must be ordered factor)
      selectInput("outcome",
                  "Outcome Variable (Ordered):",
                  choices = NULL),

      # Show outcome levels
      verbatimTextOutput("outcome_levels"),

      # Predictor variables (checkboxes)
      h5("Main Effects:"),
      uiOutput("predictor_checklist"),

      # Interaction terms
      h5("Two-Way Interactions (Optional):"),
      uiOutput("interaction_ui"),

      # Random effects (optional)
      h5("Random Effects (Optional):"),
      selectInput("random_var",
                  "Random Intercept Variable:",
                  choices = c("None" = ""),
                  multiple = FALSE),

      hr(),

      # Step 3: Model Settings
      h4("3. Model Settings"),

      # Ordinal family options
      selectInput("link",
                  "Link Function:",
                  choices = c("logit", "probit", "cloglog"),
                  selected = "logit"),

      selectInput("threshold",
                  "Threshold Type:",
                  choices = c("flexible", "equidistant"),
                  selected = "flexible"),

      # MCMC Settings
      numericInput("chains", "Number of Chains:",
                   value = 4, min = 1, max = 10),

      numericInput("iter", "Iterations per Chain:",
                   value = 2000, min = 500, max = 10000, step = 500),

      numericInput("warmup", "Warmup Iterations:",
                   value = 1000, min = 100, max = 5000, step = 100),

      numericInput("cores", "Number of Cores:",
                   value = parallel::detectCores() - 1,
                   min = 1,
                   max = parallel::detectCores()),

      # Prior options
      checkboxInput("use_priors", "Set Custom Priors", FALSE),

      conditionalPanel(
        condition = "input.use_priors",
        numericInput("prior_sd", "Prior SD for Coefficients:",
                     value = 10, min = 0.1, max = 100)
      ),

      hr(),

      # Run button
      actionButton("run_model", "Run Ordinal Model",
                   class = "btn-primary btn-block"),

      # Download options
      conditionalPanel(
        condition = "output.model_complete",
        hr(),
        downloadButton("download_model", "Download Model (RDS)"),
        br(),
        br(),
        downloadButton("download_results", "Download Results (CSV)")
      )
    ),

    mainPanel(
      width = 9,

      tabsetPanel(
        id = "main_tabs",

        # Data Preview Tab
        tabPanel("Data Preview",
                 h4("Dataset Summary"),
                 verbatimTextOutput("data_summary"),
                 h4("Data Preview (First 100 rows)"),
                 DT::dataTableOutput("data_preview")
        ),

        # Model Formula Tab
        tabPanel("Model Specification",
                 h4("Model Formula"),
                 wellPanel(
                   verbatimTextOutput("formula_display")
                 ),
                 h4("Model Details"),
                 verbatimTextOutput("model_spec"),
                 h4("Prior Specification"),
                 verbatimTextOutput("prior_spec")
        ),

        # Results Tab
        tabPanel("Model Summary",
                 conditionalPanel(
                   condition = "output.model_complete == false",
                   h4("No model has been run yet"),
                   p("Please select your data and variables, then click 'Run Ordinal Model'")
                 ),
                 conditionalPanel(
                   condition = "output.model_complete",
                   h4("Model Summary"),
                   verbatimTextOutput("model_summary"),

                   h4("Model Convergence Diagnostics"),
                   verbatimTextOutput("convergence_summary"),

                   h4("WAIC and LOO"),
                   verbatimTextOutput("model_criteria")
                 )
        ),

        # Coefficients Tab
        tabPanel("Coefficients",
                 conditionalPanel(
                   condition = "output.model_complete",
                   h4("Fixed Effects"),
                   DT::dataTableOutput("coef_table"),

                   h4("Threshold Parameters"),
                   DT::dataTableOutput("threshold_table"),

                   h4("Coefficient Plot"),
                   plotOutput("coef_plot", height = "600px")
                 )
        ),

        # Diagnostics Tab
        tabPanel("Diagnostics",
                 conditionalPanel(
                   condition = "output.model_complete",
                   h4("Trace Plots"),
                   plotOutput("trace_plot", height = "800px"),

                   h4("Posterior Predictive Check"),
                   plotOutput("pp_check", height = "400px")
                 )
        ),

        # Predictions Tab
        tabPanel("Predictions",
                 conditionalPanel(
                   condition = "output.model_complete",
                   h4("Predicted Probabilities"),
                   p("Set predictor values for prediction:"),
                   uiOutput("prediction_inputs"),
                   br(),
                   actionButton("generate_predictions", "Generate Predictions",
                                class = "btn-info"),
                   hr(),
                   plotOutput("prediction_plot", height = "500px"),
                   br(),
                   DT::dataTableOutput("prediction_table")
                 )
        ),

        # Marginal Effects Tab
        tabPanel("Marginal Effects",
                 conditionalPanel(
                   condition = "output.model_complete",
                   h4("Marginal Effects Analysis"),
                   selectInput("me_focal", "Focal Variable:", choices = NULL),
                   selectInput("me_moderator", "Moderator (Optional):",
                               choices = c("None" = "")),
                   br(),
                   actionButton("compute_marginal", "Compute Marginal Effects",
                                class = "btn-info"),
                   hr(),
                   plotOutput("marginal_plot", height = "600px"),
                   br(),
                   verbatimTextOutput("marginal_summary")
                 )
        )
      )
    )
  )
)

# Server Logic
server <- function(input, output, session) {

  # Reactive values to store data and model
  values <- reactiveValues(
    data = NULL,
    model = NULL,
    model_complete = FALSE,
    predictions = NULL,
    marginal_effects = NULL
  )

  # Get list of data frames from global environment
  get_data_list <- function() {
    objs <- ls(envir = .GlobalEnv)
    data_objs <- c()

    for (obj in objs) {
      if (is.data.frame(get(obj, envir = .GlobalEnv))) {
        data_objs <- c(data_objs, obj)
      }
    }

    if (length(data_objs) == 0) {
      return(c("No data frames found" = ""))
    }

    return(data_objs)
  }

  # Initialize and refresh data list
  observe({
    updateSelectInput(session, "dataset",
                      choices = get_data_list())
  })

  observeEvent(input$refresh_data, {
    updateSelectInput(session, "dataset",
                      choices = get_data_list())
    showNotification("Data list refreshed", type = "info", duration = 2)
  })

  # Load selected dataset
  observeEvent(input$dataset, {
    req(input$dataset)
    if (input$dataset != "") {
      values$data <- get(input$dataset, envir = .GlobalEnv)

      # Find ordered factors for outcome
      ordered_vars <- names(values$data)[sapply(values$data, is.ordered)]

      # Also include regular factors and numeric variables that could be converted
      factor_vars <- names(values$data)[sapply(values$data, is.factor)]
      numeric_vars <- names(values$data)[sapply(values$data, function(x) {
        is.numeric(x) && length(unique(x)) <= 10  # Reasonable for ordinal
      })]

      outcome_choices <- unique(c(ordered_vars, factor_vars, numeric_vars))

      if (length(outcome_choices) == 0) {
        outcome_choices <- names(values$data)
      }

      updateSelectInput(session, "outcome",
                        choices = outcome_choices)

      # Update predictor variables (all variables)
      var_choices <- names(values$data)

      updateSelectInput(session, "random_var",
                        choices = c("None" = "", var_choices))
    }
  })

  # Display outcome levels
  output$outcome_levels <- renderPrint({
    req(input$outcome, values$data)

    outcome_var <- values$data[[input$outcome]]

    if (is.ordered(outcome_var)) {
      cat("Ordered factor with levels:\n")
      cat(paste(levels(outcome_var), collapse = " < "))
    } else if (is.factor(outcome_var)) {
      cat("Factor (will be converted to ordered) with levels:\n")
      cat(paste(levels(outcome_var), collapse = ", "))
    } else if (is.numeric(outcome_var)) {
      cat("Numeric variable with values:\n")
      cat(paste(sort(unique(outcome_var)), collapse = ", "))
      cat("\nWill be converted to ordered factor")
    } else {
      cat("Variable type:", class(outcome_var))
    }
  })

  # Create predictor checklist UI
  output$predictor_checklist <- renderUI({
    req(values$data, input$outcome)

    var_names <- names(values$data)
    var_names <- var_names[var_names != input$outcome]

    checkboxGroupInput("predictors",
                       label = NULL,
                       choices = var_names,
                       selected = NULL)
  })

  # Create interaction UI
  output$interaction_ui <- renderUI({
    req(input$predictors)

    if (length(input$predictors) >= 2) {
      # Generate all possible 2-way interactions
      interactions <- combn(input$predictors, 2,
                            function(x) paste(x, collapse = " : "))

      checkboxGroupInput("interactions",
                         label = NULL,
                         choices = interactions,
                         selected = NULL)
    } else {
      p("Select at least 2 predictors to enable interactions")
    }
  })

  # Generate formula
  get_formula <- reactive({
    req(input$outcome, input$predictors)

    # Base formula with main effects
    pred_terms <- paste(input$predictors, collapse = " + ")

    # Add interactions if specified
    if (!is.null(input$interactions) && length(input$interactions) > 0) {
      int_terms <- paste(input$interactions, collapse = " + ")
      pred_terms <- paste(pred_terms, "+", int_terms)
    }

    formula_str <- paste(input$outcome, "~", pred_terms)

    # Add random effects if specified
    if (!is.null(input$random_var) && input$random_var != "") {
      formula_str <- paste(formula_str, "+ (1 |", input$random_var, ")")
    }

    return(formula_str)
  })

  # Display formula
  output$formula_display <- renderPrint({
    cat(get_formula())
  })

  # Display model specification
  output$model_spec <- renderPrint({
    req(input$outcome, input$predictors)

    cat("Ordinal Regression Specification:\n")
    cat("================================\n")
    cat("Family: cumulative(link = '", input$link, "', threshold = '",
        input$threshold, "')\n", sep = "")
    cat("Chains:", input$chains, "\n")
    cat("Iterations:", input$iter, "\n")
    cat("Warmup:", input$warmup, "\n")
    cat("Cores:", input$cores, "\n")

    if (!is.null(input$random_var) && input$random_var != "") {
      cat("\nRandom Effects:\n")
      cat("Random Intercepts for:", input$random_var, "\n")
    }

    cat("\nNumber of predictors:", length(input$predictors), "\n")
    if (!is.null(input$interactions)) {
      cat("Number of interactions:", length(input$interactions), "\n")
    }
  })

  # Display prior specification
  output$prior_spec <- renderPrint({
    if (input$use_priors) {
      cat("Custom Priors:\n")
      cat("==============\n")
      cat("Coefficients: Normal(0,", input$prior_sd, ")\n")
      cat("Intercepts: Student-t(3, 0, 2.5)\n")
    } else {
      cat("Using default BRMS priors\n")
      cat("Run get_prior() on the model for details")
    }
  })

  # Data preview
  output$data_preview <- DT::renderDataTable({
    req(values$data)
    DT::datatable(head(values$data, 100),
                  options = list(scrollX = TRUE, pageLength = 10))
  })

  # Data summary
  output$data_summary <- renderPrint({
    req(values$data)
    cat("Dataset:", input$dataset, "\n")
    cat("Dimensions:", nrow(values$data), "rows x", ncol(values$data), "columns\n")
    cat("Complete cases:", sum(complete.cases(values$data)), "\n\n")
    str(values$data, list.len = 10)
  })

  # Run BRMS ordinal model
  observeEvent(input$run_model, {
    req(input$outcome, input$predictors, values$data)

    withProgress(message = 'Running Ordinal Regression...', value = 0, {

      incProgress(0.1, detail = "Preparing data...")

      # Prepare the data - ensure outcome is ordered
      model_data <- values$data

      if (!is.ordered(model_data[[input$outcome]])) {
        if (is.factor(model_data[[input$outcome]])) {
          model_data[[input$outcome]] <- as.ordered(model_data[[input$outcome]])
        } else if (is.numeric(model_data[[input$outcome]])) {
          model_data[[input$outcome]] <- as.ordered(model_data[[input$outcome]])
        }

        showNotification(paste("Converted", input$outcome, "to ordered factor"),
                         type = "warning", duration = 5)
      }

      # Prepare formula
      formula_obj <- as.formula(get_formula())

      incProgress(0.2, detail = "Setting up priors...")

      # Set priors if requested
      if (input$use_priors) {
        prior_spec <- c(
          prior(normal(0, input$prior_sd), class = b),
          prior(student_t(3, 0, 2.5), class = Intercept)
        )
      } else {
        prior_spec <- NULL
      }

      incProgress(0.3, detail = "Compiling Stan model...")

      # Run model with error handling
      tryCatch({
        values$model <- brm(
          formula = formula_obj,
          data = model_data,
          family = cumulative(link = input$link, threshold = input$threshold),
          prior = prior_spec,
          chains = input$chains,
          iter = input$iter,
          warmup = input$warmup,
          cores = input$cores,
          seed = 123,
          silent = 2,
          refresh = 0,
          backend = "cmdstanr"  # Use cmdstanr if available, otherwise falls back to rstan
        )

        values$model_complete <- TRUE

        incProgress(1, detail = "Model complete!")

        showNotification("Ordinal model completed successfully!",
                         type = "success",
                         duration = 5)

        # Switch to results tab
        updateTabsetPanel(session, "main_tabs", selected = "Model Summary")

        # Update marginal effects choices
        updateSelectInput(session, "me_focal",
                          choices = input$predictors)
        updateSelectInput(session, "me_moderator",
                          choices = c("None" = "", input$predictors))

      }, error = function(e) {
        showNotification(paste("Error running model:", e$message),
                         type = "error",
                         duration = NULL)
        values$model_complete <- FALSE
      })
    })
  })

  # Model complete flag for conditional panels
  output$model_complete <- reactive({
    values$model_complete
  })
  outputOptions(output, "model_complete", suspendWhenHidden = FALSE)

  # Model summary
  output$model_summary <- renderPrint({
    req(values$model)
    summary(values$model)
  })

  # Convergence diagnostics
  output$convergence_summary <- renderPrint({
    req(values$model)

    cat("Convergence Diagnostics:\n")
    cat("========================\n\n")

    # Get Rhat values
    rhat_vals <- rhat(values$model)
    cat("R-hat Summary:\n")
    cat("Max R-hat:", max(rhat_vals, na.rm = TRUE), "\n")

    problematic <- sum(rhat_vals > 1.01, na.rm = TRUE)
    if (problematic > 0) {
      cat("WARNING:", problematic, "parameters have R-hat > 1.01\n")
    } else {
      cat("All R-hat values < 1.01 (good convergence)\n")
    }

    # Get ESS values
    ess_vals <- ess_bulk(values$model)
    cat("\nEffective Sample Size (bulk):\n")
    cat("Min ESS:", min(ess_vals, na.rm = TRUE), "\n")

    if (min(ess_vals, na.rm = TRUE) < 400) {
      cat("WARNING: Some parameters have ESS < 400\n")
    } else {
      cat("All ESS values adequate\n")
    }
  })

  # Model criteria
  output$model_criteria <- renderPrint({
    req(values$model)

    cat("Model Information Criteria:\n")
    cat("===========================\n\n")

    tryCatch({
      waic_result <- waic(values$model)
      cat("WAIC:\n")
      print(waic_result)
      cat("\n")
    }, error = function(e) {
      cat("WAIC calculation failed\n")
    })

    tryCatch({
      loo_result <- loo(values$model)
      cat("LOO:\n")
      print(loo_result)
    }, error = function(e) {
      cat("LOO calculation failed\n")
    })
  })

  # Coefficient table
  output$coef_table <- DT::renderDataTable({
    req(values$model)

    # Extract fixed effects
    fixed_ef <- as.data.frame(fixef(values$model))
    fixed_ef$Parameter <- rownames(fixed_ef)
    fixed_ef <- fixed_ef[, c("Parameter", "Estimate", "Est.Error",
                             "Q2.5", "Q97.5")]

    # Add significance indicator
    fixed_ef$Significant <- ifelse(
      (fixed_ef$Q2.5 > 0 & fixed_ef$Q97.5 > 0) |
        (fixed_ef$Q2.5 < 0 & fixed_ef$Q97.5 < 0),
      "Yes", "No"
    )

    DT::datatable(fixed_ef,
                  options = list(pageLength = 20),
                  rownames = FALSE) %>%
      DT::formatRound(columns = c("Estimate", "Est.Error", "Q2.5", "Q97.5"),
                      digits = 3) %>%
      DT::formatStyle("Significant",
                      backgroundColor = styleEqual("Yes", "lightgreen"))
  })

  # Threshold table
  output$threshold_table <- DT::renderDataTable({
    req(values$model)

    # Extract thresholds (Intercepts for ordinal models)
    model_summary <- summary(values$model)

    # Find Intercept parameters
    all_params <- as.data.frame(model_summary$fixed)
    threshold_params <- all_params[grep("Intercept", rownames(all_params)), ]

    if (nrow(threshold_params) > 0) {
      threshold_df <- data.frame(
        Threshold = rownames(threshold_params),
        Estimate = threshold_params$Estimate,
        Est.Error = threshold_params$Est.Error,
        Lower_95CI = threshold_params$`l-95% CI`,
        Upper_95CI = threshold_params$`u-95% CI`
      )

      DT::datatable(threshold_df,
                    options = list(pageLength = 10),
                    rownames = FALSE) %>%
        DT::formatRound(columns = 2:5, digits = 3)
    } else {
      data.frame(Message = "No threshold parameters found")
    }
  })

  # Coefficient plot
  output$coef_plot <- renderPlot({
    req(values$model)
    mcmc_plot(values$model,
              type = "intervals",
              prob = 0.5,
              prob_outer = 0.95,
              point_est = "median")
  })

  # Trace plots
  output$trace_plot <- renderPlot({
    req(values$model)
    mcmc_plot(values$model, type = "trace")
  })

  # Posterior predictive check
  output$pp_check <- renderPlot({
    req(values$model)
    pp_check(values$model, ndraws = 100, type = "bars")
  })

  # Create prediction inputs UI
  output$prediction_inputs <- renderUI({
    req(values$model, input$predictors)

    input_list <- list()

    for (var in input$predictors) {
      var_data <- values$data[[var]]

      if (is.numeric(var_data)) {
        input_list[[var]] <- numericInput(
          paste0("pred_", var),
          paste(var, ":"),
          value = mean(var_data, na.rm = TRUE),
          min = min(var_data, na.rm = TRUE),
          max = max(var_data, na.rm = TRUE)
        )
      } else if (is.factor(var_data) || is.character(var_data)) {
        if (is.character(var_data)) var_data <- as.factor(var_data)
        input_list[[var]] <- selectInput(
          paste0("pred_", var),
          paste(var, ":"),
          choices = levels(var_data),
          selected = levels(var_data)[1]
        )
      }
    }

    do.call(tagList, input_list)
  })

  # Generate predictions
  observeEvent(input$generate_predictions, {
    req(values$model)

    # Create new data for prediction
    new_data <- data.frame(row.names = 1)

    for (var in input$predictors) {
      input_val <- input[[paste0("pred_", var)]]

      if (is.numeric(values$data[[var]])) {
        new_data[[var]] <- as.numeric(input_val)
      } else {
        new_data[[var]] <- input_val
      }
    }

    # Add random effect if needed
    if (!is.null(input$random_var) && input$random_var != "") {
      # Use average/reference level for random effect
      new_data[[input$random_var]] <- NA
    }

    # Generate predictions
    values$predictions <- predict(values$model,
                                  newdata = new_data,
                                  probs = c(0.025, 0.975),
                                  summary = TRUE)

    showNotification("Predictions generated!", type = "success", duration = 3)
  })

  # Prediction plot
  output$prediction_plot <- renderPlot({
    req(values$predictions)

    # Get category probabilities
    prob_cols <- grep("^P\\(", colnames(values$predictions), value = TRUE)

    if (length(prob_cols) > 0) {
      prob_data <- data.frame(
        Category = gsub("P\\(|\\)", "", prob_cols),
        Probability = as.numeric(values$predictions[1, prob_cols])
      )

      ggplot(prob_data, aes(x = Category, y = Probability)) +
        geom_bar(stat = "identity", fill = "steelblue", alpha = 0.7) +
        theme_minimal() +
        labs(title = "Predicted Probabilities for Each Category",
             x = "Category",
             y = "Probability") +
        ylim(0, 1) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
  })

  # Prediction table
  output$prediction_table <- DT::renderDataTable({
    req(values$predictions)

    pred_df <- as.data.frame(values$predictions)
    pred_df <- round(pred_df, 4)

    DT::datatable(pred_df,
                  options = list(scrollX = TRUE, pageLength = 20))
  })

  # Compute marginal effects
  observeEvent(input$compute_marginal, {
    req(values$model, input$me_focal)

    withProgress(message = 'Computing marginal effects...', {

      # Create conditions for marginal effects
      conditions <- list()

      # Set focal variable to its range
      focal_data <- values$data[[input$me_focal]]
      if (is.numeric(focal_data)) {
        focal_values <- seq(min(focal_data, na.rm = TRUE),
                            max(focal_data, na.rm = TRUE),
                            length.out = 20)
      } else {
        focal_values <- unique(focal_data)
      }

      conditions[[input$me_focal]] <- focal_values

      # Add moderator if specified
      if (!is.null(input$me_moderator) && input$me_moderator != "") {
        mod_data <- values$data[[input$me_moderator]]
        if (is.numeric(mod_data)) {
          conditions[[input$me_moderator]] <- quantile(mod_data,
                                                       c(0.25, 0.5, 0.75),
                                                       na.rm = TRUE)
        } else {
          conditions[[input$me_moderator]] <- unique(mod_data)
        }
      }

      # Compute marginal effects using marginaleffects if available
      tryCatch({
        if (requireNamespace("marginaleffects", quietly = TRUE)) {
          values$marginal_effects <- marginaleffects::predictions(
            values$model,
            newdata = marginaleffects::datagrid(!!!conditions)
          )
        } else {
          showNotification("Install 'marginaleffects' package for this feature",
                           type = "warning")
        }
      }, error = function(e) {
        showNotification(paste("Error computing marginal effects:", e$message),
                         type = "error")
      })
    })
  })

  # Marginal effects plot
  output$marginal_plot <- renderPlot({
    req(values$marginal_effects)

    if (requireNamespace("marginaleffects", quietly = TRUE)) {
      marginaleffects::plot_predictions(values$model,
                                        condition = input$me_focal)
    }
  })

  # Marginal effects summary
  output$marginal_summary <- renderPrint({
    req(values$marginal_effects)
    print(summary(values$marginal_effects))
  })

  # Download model
  output$download_model <- downloadHandler(
    filename = function() {
      paste0("brms_ordinal_model_", Sys.Date(), ".rds")
    },
    content = function(file) {
      saveRDS(values$model, file)
    }
  )

  # Download results
  output$download_results <- downloadHandler(
    filename = function() {
      paste0("brms_ordinal_results_", Sys.Date(), ".csv")
    },
    content = function(file) {
      if (!is.null(values$model)) {
        fixed_ef <- as.data.frame(fixef(values$model))
        fixed_ef$Parameter <- rownames(fixed_ef)
        write.csv(fixed_ef, file, row.names = FALSE)
      }
    }
  )
}

# Run the app
shinyApp(ui = ui, server = server)
