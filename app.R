# ------------------------------------------------------------------------------

# ------------ SmartRegress: Interactive Regression Learning App ---------------

# ------------------------------------------------------------------------------

# --- Libraries ---

library(shiny)
library(bslib)
library(plotly)
library(bsicons)
library(reshape2)
library(car)

# --- Custom Theme & Styling Definitions ---

theme <- bs_theme(
  version = 5,
  primary = "#F4F1FA",
  bg = "#F4F1FA",
  fg = "#2A2438",
  base_font = font_google("Poppins")
)

# Custom CSS for high contrast: dark sidebar with light text, light cards with dark text

custom_css <- HTML("
  /* Sidebar styling: Dark Purple background with light text */
  .sidebar {
    background-color: #1E1035 !important;
    color: #E2D9F3 !important;
  }
  .sidebar h4, .sidebar label, .sidebar .control-label, .sidebar h5 {
    color: #FFFFFF !important;
  }
  .sidebar select, .sidebar .form-control {
    background-color: #2D1A4D !important;
    color: #FFFFFF !important;
    border: 1px solid #512DA8 !important;
  }
  .sidebar .irs-line, .sidebar .irs-bar {
    background: #673AB7 !important;
  }
  .sidebar .irs-single, .sidebar .irs-from, .sidebar .irs-to {
    background: #A5D6A7 !important;
    color: #000000 !important;
  }
  
  /* Main Panel Cards: White/Light cards with dark text */
  .card {
    background-color: #FFFFFF !important;
    color: #2A2438 !important;
    border: 1px solid #E0D6F0 !important;
    border-radius: 10px !important;
    box-shadow: 0 4px 12px rgba(30, 16, 53, 0.05) !important;
  }
  .card-header {
    background-color: #1E1035 !important;
    color: #F8F5FC !important;
    font-weight: 600 !important;
    border-bottom: 1px solid #E0D6F0 !important;
  }
  
  /* Value Boxes Styling */
  .bslib-value-box {
    border-radius: 10px !important;
  }
  
  /* Code / Verbatim Outputs */
  pre, code, .shiny-bound-output {
    color: #1E1035 !important;
    background-color: #F8F5FC !important;
    border: 1px solid #E0D6F0 !important;
  }
")

# --- UI ---

ui <- page_sidebar(
  title = div(bs_icon("graph-up-arrow"), 
              " SmartRegress: Interactive Regression Learning App", 
              style = "background-color: #120E1E; 
              color: #FFFFFF !important; 
              font-weight: bold;
              width: 100%; display: block;"),
  theme = theme,
  bg = "#1E1035",
  
  # Inject Custom Visual Styling
  tags$head(tags$style(custom_css)),
  
  sidebar = sidebar(
    width = 330,
    h4("App Controls"),
    
    selectInput(
      "scenario",
      "Regression Scenario",
      choices = c(
        "Good Fit",
        "Underfitting",
        "Overfitting",
        "Multicollinearity",
        "Heteroscedasticity",
        "Non-linearity",
        "Outliers",
        "Influential Observations"
      )
    ),
    
    selectInput(
      "diagnostic",
      "Diagnostic Plot",
      choices = c(
        "Scatter Plot",
        "Residual vs Fitted",
        "Q-Q Plot",
        "Scale-Location",
        "Cook's Distance",
        "Residual Histogram",
        "Correlation Matrix",
        "VIF Analysis"
      )
    ),
    
    hr(style = "border-color: #512DA8;"),
    h5("Sample Parameters"),
    sliderInput("obs_count", "Sample Size (N)", 
                min = 50, 
                max = 500, 
                value = 200, 
                step = 25),
    
    hr(style = "border-color: #512DA8;"),
    uiOutput("scenarioDescription")
  ),
  
  # Top Metrics Summary Row
  layout_column_wrap(
    width = 1/3,
    fill = FALSE,
    value_box(
      title = "R-Squared",
      value = textOutput("vb_r2"),
      showcase = bs_icon("speedometer"),
      theme = value_box_theme(bg = "#512DA8", 
                              fg = "#FFFFFF")
    ),
    value_box(
      title = "Adjusted R-Squared",
      value = textOutput("vb_adj_r2"),
      showcase = bs_icon("check2-circle"),
      theme = value_box_theme(bg = "#673AB7", 
                              fg = "#FFFFFF")
    ),
    value_box(
      title = "Residual Std Error",
      value = textOutput("vb_rse"),
      showcase = bs_icon("activity"),
      theme = value_box_theme(bg = "#3F51B5", 
                              fg = "#FFFFFF")
    )
  ),
  
  br(),
  
  # Main Diagnostic & Output Grid
  layout_column_wrap(
    width = 1/2,
    
    # --- Card 1: Dynamic Diagnostics Plot ---
    card(
      full_screen = TRUE,
      card_header(div(bs_icon("graph-up"), " Diagnostics Explorer")),
      card_body(uiOutput("diagnosticOutput"))
    ),
    
    # --- Card 2: Theory & Equations ---
    card(
      full_screen = TRUE,
      card_header(div(bs_icon("book"), " Theory & Equation")),
      card_body(
        withMathJax(),
        uiOutput("theory"),
        hr(),
        h6("Estimated Equation", style = "color: #512DA8; font-weight: bold;"),
        uiOutput("latex_equation")
      )
    ),
    
    # --- Card 3: Model Statistics & Summary ---
    card(
      full_screen = TRUE,
      card_header(div(bs_icon("calculator"), " Model Statistics")),
      card_body(
        accordion(
          accordion_panel(
            "Regression Summary Output",
            verbatimTextOutput("summary")
          ),
          accordion_panel(
            "ANOVA Table",
            tableOutput("anova")
          ),
          accordion_panel(
            "Coefficients Table",
            tableOutput("coef")
          )
        )
      )
    ),
    
    # --- Card 4: Assumption Dashboard & Actions ---
    card(
      full_screen = TRUE,
      card_header(div(bs_icon("shield-check"), " Assumption Diagnostics")),
      card_body(
        tableOutput("assumptionTable"),
        hr(),
        uiOutput("assumptionExplanation"),
        br(),
        h6("Actionable Guidance", style = "color: #512DA8; font-weight: bold;"),
        verbatimTextOutput("recommendation")
      )
    )
  )
)

# --- Server ---

server <- function(input, output, session) {
  
  # 1. Dataset Generation Reactive
  regressionData <- reactive({
    set.seed(123)
    n <- input$obs_count
    
    x1 <- rnorm(n)
    x2 <- rnorm(n)
    x3 <- rnorm(n)
    scenario <- input$scenario
    
    if (scenario == "Good Fit") {
      y <- 5 + 2*x1 - 1.5*x2 + rnorm(n, 0, 1)
      return(data.frame(y, x1, x2, x3))
      
    } else if (scenario == "Underfitting") {
      y <- x1^2 + rnorm(n)
      return(data.frame(y, x1, x2, x3))
      
    } else if (scenario == "Overfitting") {
      x4 <- rnorm(n); x5 <- rnorm(n); x6 <- rnorm(n)
      y <- 4 + 2*x1 - x2 + rnorm(n)
      return(data.frame(y, x1, x2, x3, x4, x5, x6))
      
    } else if (scenario == "Multicollinearity") {
      x2 <- x1 * 0.98 + rnorm(n, 0, 0.05)
      y <- 5 + 2*x1 - x2 + rnorm(n)
      return(data.frame(y, x1, x2, x3))
      
    } else if (scenario == "Heteroscedasticity") {
      error <- rnorm(n, 0, abs(x1) * 2)
      y <- 5 + 2*x1 + error
      return(data.frame(y, x1, x2, x3))
      
    } else if (scenario == "Non-linearity") {
      y <- 5 + 2*(x1^2) - x2 + rnorm(n)
      return(data.frame(y, x1, x2, x3))
      
    } else if (scenario == "Outliers") {
      y <- 5 + 2*x1 + rnorm(n)
      y[c(10, 30, 50)] <- y[c(10, 30, 50)] + 15
      return(data.frame(y, x1, x2, x3))
      
    } else { # Influential Observations
      y <- 5 + 2*x1 + rnorm(n)
      x1[n] <- 8
      y[n] <- 25
      return(data.frame(y, x1, x2, x3))
    }
  })
  
  # 2. Model Fitting Reactive
  regressionModel <- reactive({
    data <- regressionData()
    if (input$scenario == "Overfitting") {
      lm(y ~ ., data = data)
    } else {
      lm(y ~ x1 + x2 + x3, data = data)
    }
  })
  
  # 3. Top Metrics Value Boxes
  output$vb_r2 <- renderText({
    s <- summary(regressionModel())
    paste0(round(s$r.squared * 100, 1), "%")
  })
  
  output$vb_adj_r2 <- renderText({
    s <- summary(regressionModel())
    paste0(round(s$adj.r.squared * 100, 1), "%")
  })
  
  output$vb_rse <- renderText({
    s <- summary(regressionModel())
    round(s$sigma, 3)
  })
  
  # 4. Scenario Descriptions & Explanations
  output$scenarioDescription <- renderUI({
    HTML("<span style='color: #E2D9F3; 
         font-size: 0.9em;'>Select a scenario above to dynamically simulate dataset characteristics.</span>")
  })
  
  output$theory <- renderUI({
    switch(
      input$scenario,
      "Good Fit" = HTML("<h5 style='color: #512DA8;'>Good Fit</h5><p>A true linear structure exists. Residuals exhibit spherical distribution around zero.</p>"),
      "Underfitting" = HTML("<h5 style='color: #512DA8;'>Underfitting</h5><p>The specified model lacks structural complexity (e.g., polynomial terms) to capture the true underlying distribution.</p>"),
      "Overfitting" = HTML("<h5 style='color: #512DA8;'>Overfitting</h5><p>Extraneous noise variables inflate model complexity without improving true explanatory power.</p>"),
      "Multicollinearity" = HTML("<h5 style='color: #512DA8;'>Multicollinearity</h5><p>High collinearity between predictors inflates coefficient standard errors, leading to model instability.</p>"),
      "Heteroscedasticity" = HTML("<h5 style='color: #512DA8;'>Heteroscedasticity</h5><p>Residual variance expands across fitted values, violating the constant variance assumption $Var(\\epsilon_i) = \\sigma^2$.</p>"),
      "Non-linearity" = HTML("<h5 style='color: #512DA8;'>Non-linearity</h5><p>Curvature present in the functional form distorts linear coefficient estimation.</p>"),
      "Outliers" = HTML("<h5 style='color: #512DA8;'>Outliers</h5><p>Extreme $Y$-space deviations distort global error variance bounds.</p>"),
      HTML("<h5 style='color: #512DA8;'>Influential Observations</h5><p>High leverage points combined with high residual values alter regression parameters dramatically.</p>")
    )
  })
  
  output$latex_equation <- renderUI({
    cf <- round(coef(regressionModel()), 3)
    eq_str <- paste0("$$\\hat{Y} = ", cf[1])
    for (i in 2:length(cf)) {
      var_name <- names(cf)[i]
      val <- cf[i]
      sign <- if (val >= 0) "+" else "-"
      eq_str <- paste0(eq_str, " ", sign, " ", 
                       abs(val), " \\cdot ", var_name)
    }
    eq_str <- paste0(eq_str, "$$")
    withMathJax(HTML(eq_str))
  })
  
  # 5. Dynamic Diagnostic Output Router
  output$diagnosticOutput <- renderUI({
    if (input$diagnostic == "VIF Analysis") {
      tableOutput("vifTable")
    } else if (input$diagnostic == "Correlation Matrix") {
      plotlyOutput("corrPlot", height = 380)
    } else {
      plotlyOutput("mainPlot", height = 380)
    }
  })
  
  # 6. Interactive Plotly Diagnostic Charts
  output$mainPlot <- renderPlotly({
    data <- regressionData()
    model <- regressionModel()
    fitted_vals <- predict(model)
    resids <- residuals(model)
    diag_type <- input$diagnostic
    
    if (diag_type == "Scatter Plot") {
      plot_ly(data, x = ~x1, y = ~y, type = "scatter", mode = "markers",
              marker = list(color = "#512DA8", size = 7, 
                            opacity = 0.7), name = "Observed") |>
        add_lines(x = ~x1, y = fitted_vals, 
                  line = list(color = "#D81B60", width = 2.5), 
                  name = "Fitted Line")
      
    } else if (diag_type == "Residual vs Fitted") {
      plot_ly(x = fitted_vals, y = resids, type = "scatter", mode = "markers",
              marker = list(color = "#512DA8", size = 7, 
                            opacity = 0.7), name = "Residuals") |>
        add_lines(x = range(fitted_vals), y = c(0, 0), 
                  line = list(color = "red", dash = "dash"), 
                  name = "Zero Bound")
      
    } else if (diag_type == "Q-Q Plot") {
      qq <- qqnorm(resids, plot.it = FALSE)
      plot_ly(x = qq$x, y = qq$y, type = "scatter", mode = "markers",
              marker = list(color = "#512DA8", size = 7), name = "Quantiles") |>
        add_lines(x = qq$x, y = qq$x, 
                  line = list(color = "red"), 
                  name = "Normal Target")
      
    } else if (diag_type == "Scale-Location") {
      sqrt_abs_res <- sqrt(abs(rstandard(model)))
      plot_ly(x = fitted_vals, y = sqrt_abs_res, type = "scatter", mode = "markers",
              marker = list(color = "#512DA8", size = 7))
      
    } else if (diag_type == "Cook's Distance") {
      cooks <- cooks.distance(model)
      cutoff <- 4 / nrow(data)
      plot_ly(x = seq_along(cooks), y = cooks, 
              type = "bar", 
              marker = list(color = "#512DA8")) |>
        add_lines(x = c(0, length(cooks)), 
                  y = c(cutoff, cutoff), 
                  line = list(color = "red", 
                              dash = "dash"), 
                  name = "4/N Limit")
      
    } else if (diag_type == "Residual Histogram") {
      plot_ly(x = resids, type = "histogram", 
              marker = list(color = "#512DA8", 
                            opacity = 0.7))
    }
  })
  
  output$corrPlot <- renderPlotly({
    corr <- cor(regressionData())
    corr_df <- reshape2::melt(corr)
    plot_ly(corr_df, x = ~Var1, 
            y = ~Var2, 
            z = ~value, 
            type = "heatmap", 
            colors = "Purples")
  })
  
  output$vifTable <- renderTable({
    model <- regressionModel()
    vif_vals <- car::vif(model)
    data.frame(Variable = names(vif_vals), VIF = round(vif_vals, 2))
  })
  
  # 7. Model Statistics, Tables & Recommendations
  output$summary <- renderPrint({ summary(regressionModel()) })
  output$anova <- renderTable({ as.data.frame(anova(regressionModel())) }, 
                              rownames = TRUE)
  output$coef <- renderTable({ as.data.frame(round(summary(regressionModel())$coefficients, 4)) }, 
                             rownames = TRUE)
  
  output$assumptionTable <- renderTable({
    scenario <- input$scenario
    status <- data.frame(
      Assumption = c("Linearity", "Normality", "Homoscedasticity", "Independence", "Multicollinearity"),
      Status = c("Pass", "Pass", "Pass", "Pass", "Pass")
    )
    if (scenario == "Non-linearity") status$Status[1] <- "Violated"
    if (scenario == "Heteroscedasticity") status$Status[3] <- "Violated"
    if (scenario == "Multicollinearity") status$Status[5] <- "Warning"
    if (scenario == "Outliers") status$Status[2] <- "Warning"
    status
  })
  
  output$assumptionExplanation <- renderUI({
    switch(
      input$scenario,
      "Good Fit" = HTML("<p style='color: #2E7D32; font-weight: bold;'>All regression assumptions are satisfied.</p>"),
      "Underfitting" = HTML("<p style='color: #C62828;'>Structural bias detected due to missing non-linear terms.</p>"),
      "Overfitting" = HTML("<p style='color: #EF6C00;'>High variance detected due to irrelevant predictors.</p>"),
      "Multicollinearity" = HTML("<p style='color: #EF6C00;'>Predictor collinearity inflates standard errors.</p>"),
      "Heteroscedasticity" = HTML("<p style='color: #C62828;'>Non-constant error variance present across predictions.</p>"),
      "Non-linearity" = HTML("<p style='color: #C62828;'>Residual curvature indicates linear model mis-specification.</p>"),
      "Outliers" = HTML("<p style='color: #EF6C00;'>Outliers distorting overall error variance.</p>"),
      HTML("<p style='color: #C62828;'>High leverage observation significantly shifting regression slope.</p>")
    )
  })
  
  output$recommendation <- renderPrint({
    switch(
      input$scenario,
      "Good Fit" = cat("Recommendation:\nModel meets classical assumptions. Proceed with OLS inference."),
      "Underfitting" = cat("Recommendation:\nAdd non-linear predictors, interactions, or polynomial features."),
      "Overfitting" = cat("Recommendation:\nApply Lasso/Ridge regularization or remove non-significant variables."),
      "Multicollinearity" = cat("Recommendation:\nRemove highly correlated variables or perform Principal Component Analysis (PCA)."),
      "Heteroscedasticity" = cat("Recommendation:\nApply Box-Cox transformation or use Weighted Least Squares (WLS)."),
      "Non-linearity" = cat("Recommendation:\nFit polynomial regression or generalized additive models (GAM)."),
      "Outliers" = cat("Recommendation:\nInvestigate data quality or fit Huber robust regression."),
      cat("Recommendation:\nReview leverage points (Cook's distance > 4/N) and assess sensitivity.")
    )
  })
}

# --- Launch Application ---

shinyApp(ui, server)

