
# app.R
# ------------------------------------------------------------------------------
# Pooled Testing for Infectious Diseases
# - Single-file Shiny app (no external images or assets required)
# - Two workflows:
#     (1) Pick a disease + age group -> uses a built-in probability table
#     (2) Enter your own infection probability
# - Monte Carlo simulation estimates expected # of tests for each pool size
# ------------------------------------------------------------------------------

# --- Packages -----------------------------------------------------------------
library(shiny)
library(ggplot2)
library(bslib)   # theming only
library(DT)      # interactive tables

# --- Data: infection probabilities by disease and age group -------------------
infection_data <- data.frame(
  Disease = c("Influenza", "Influenza", "Influenza", "Influenza",
              "Norovirus", "Norovirus", "Norovirus", "Norovirus",
              "Adenovirus", "Adenovirus", "Adenovirus", "Adenovirus",
              "Rhinovirus/Enterovirus", "Rhinovirus/Enterovirus",
              "Rhinovirus/Enterovirus", "Rhinovirus/Enterovirus",
              "RSV", "RSV", "RSV", "RSV"),
  Age_Group = c("<5", "<=17", "18-64", ">=65",
                "<5", "<=17", "18-64", ">=65",
                "<5", "<=17", "18-64", ">=65",
                "<5", "<=17", "18-64", ">=65",
                "<5", "<=17", "18-64", ">=65"),
  Infection_Probability = c(0.093, 0.093, 0.088, 0.039,
                            0.0144, 0.0015, 0.0011, 0.0021,
                            0.082, 0.05, 0.02, 0.015,
                            0.16, 0.11, 0.07, 0.03,
                            0.15, 0.06, 0.02, 0.07)
)

# --- UI -----------------------------------------------------------------------
ui <- navbarPage(
  # App title (image removed)
  title = div(
    style = "display:flex;align-items:center;",
    h1(
      "Pooled Testing for Infectious Diseases",
      style = "color:#000;font-weight:bold;padding:10px;border-radius:5px;
               font-size:32px;margin-right:2px"
    )
  ),
  
  # Light theme with purple primary color
  theme = bs_theme(
    bg = "#FFFFFF",
    fg = "#000000",
    primary = "#800080",
    secondary = "#800080",
    base_font = font_google("Lora"),
    heading_font = font_google("Roboto Slab")
  ),
  
  # ----------------------- Tab 1: Project Overview ----------------------------
  tabPanel(
    "Project Overview",
    fluidPage(
      # Info panels
      div(
        style = "background-color:#F3E5F5;color:#800080;padding:32px;border-radius:5px;
                 box-shadow:0 4px 8px rgba(0,0,0,0.1);",
        h3("Welcome to our app!", style = "color:#800080;"),
        p("This app streamlines pooled testing by simulating how many total tests
           are needed when you group samples before individual retesting.",
          style = "font-size:16px;")
      ),
      br(),
      div(
        style = "background-color:#F3E5F5;color:#800080;padding:32px;border-radius:5px;
                 box-shadow:0 4px 8px rgba(0,0,0,0.1);",
        h3("Methods", style = "color:#800080;"),
        p("We use Monte Carlo simulations across possible pool sizes. For each pool size,
           we estimate the expected number of group tests plus follow-up individual tests.",
          style = "font-size:16px;")
      ),
      br(),
      div(
        style = "background-color:#F3E5F5;color:#800080;padding:32px;border-radius:5px;
                 box-shadow:0 4px 8px rgba(0,0,0,0.1);",
        h3("How to use the app?", style = "color:#800080;"),
        p("Use the tabs to either (1) choose a disease & age group, or (2) enter your
           own infection probability. Provide a sample size N, click Generate Results,
           and explore the visualization and table. Use Restart to clear inputs.",
          style = "font-size:16px;")
      ),
      
      # Small style helpers for the disease buttons (icons removed)
      tags$head(tags$style(HTML("
        .btn-custom { width: 150px; text-align:center; }
        .col-custom { padding: 0 5px; }
      "))),
      
      br(),
      h3("Explore Infectious Diseases",
         style = "color:#800080;margin-bottom:15px"),
      # Five buttons (previously had icons; now buttons only)
      fluidRow(
        column(2, div(actionButton("influenza_btn", "Influenza",
                                   class = "btn btn-primary btn-custom")),
               class="col-custom"),
        column(2, div(actionButton("norovirus_btn", "Norovirus",
                                   class = "btn btn-primary btn-custom")),
               class="col-custom"),
        column(2, div(actionButton("adenovirus_btn", "Adenovirus",
                                   class = "btn btn-primary btn-custom")),
               class="col-custom"),
        column(2, div(actionButton("rhinovirus_btn", "Rhinovirus",
                                   class = "btn btn-primary btn-custom")),
               class="col-custom"),
        column(2, div(actionButton("rsv_btn", "RSV",
                                   class = "btn btn-primary btn-custom")),
               class="col-custom")
      ),
      br()
    )
  ),
  
  # -------------------- Tab 2: Know your disease type -------------------------
  tabPanel(
    "Know your disease type",
    sidebarLayout(
      sidebarPanel(
        h3("Inputs", style = "color:#800080;"),
        numericInput("N", "Total number of samples (N > 1):", 0, min = 2),
        selectInput("disease", "Select Disease Type:",
                    choices = unique(infection_data$Disease)),
        selectInput("age_group", "Select Age Group:",
                    choices = unique(infection_data$Age_Group)),
        br(),
        actionButton("submit_disease", "Generate Results",
                     class = "btn btn-primary"),
        actionButton("restart_disease", "Restart",
                     class = "btn btn-secondary"),
        br(), br(),
        p("Note: Infection probability is matched from the table based on your selections."),
        p("Please be patient while the results load ⏳.", style = "color:gray;")
      ),
      mainPanel(
        h3("Results", style = "color:#800080;"),
        tabsetPanel(
          tabPanel(
            title = "Visualization",
            br(),
            h5("Explanation of the Plot", style = "color:#800080;"),
            p("Expected tests vs. pool size. The red vertical line marks the
               optimal pool size; the blue horizontal line marks the expected tests at that optimum."),
            textOutput("matched_probability"),
            plotOutput("Linechart_disease", height = "500px", width = "800px")
          ),
          tabPanel(
            title = "Optimal Pool Size",
            br(),
            h5("Optimal Pool Size:", style = "color:#800080;"),
            textOutput("optimal_pool_size_disease")
          ),
          tabPanel(
            title = "Data Table",
            br(),
            h5("Explanation of the Data", style = "color:#800080;"),
            p("Expected number of tests for each candidate pool size."),
            DTOutput("Table_disease")
          )
        )
      )
    )
  ),
  
  # ---------------- Tab 3: Know your infection probability --------------------
  tabPanel(
    "Know your infection probability",
    sidebarLayout(
      sidebarPanel(
        h3("Inputs", style = "color:#800080;"),
        numericInput("N_prob", "Total number of samples (N > 1):", 0, min = 2),
        numericInput("p_prob", "Infection Probability (0 < p ≤ 0.5):",
                     0, min = 0, max = 0.5),
        br(),
        actionButton("submit_prob", "Generate Results",
                     class = "btn btn-primary"),
        actionButton("restart_prob", "Restart",
                     class = "btn btn-secondary"),
        br(), br(),
        p("Note: Infection probability must be between 0 and 0.5."),
        p("Please be patient while the results load ⏳.", style = "color:gray;")
      ),
      mainPanel(
        h3("Results", style = "color:#800080;"),
        tabsetPanel(
          tabPanel(
            title = "Visualization",
            br(),
            h5("Explanation of the Plot", style = "color:#800080;"),
            p("Efficiency across pool sizes for your specified probability.
               Red = optimal pool size; Blue = expected tests at optimum."),
            plotOutput("Linechart_prob", height = "500px", width = "800px")
          ),
          tabPanel(
            title = "Optimal Pool Size",
            br(),
            h5("Optimal Pool Size:", style = "color:#800080;"),
            textOutput("optimal_pool_size_prob")
          ),
          tabPanel(
            title = "Data Table",
            br(),
            h5("Explanation of the Data", style = "color:#800080;"),
            p("Expected number of tests by pool size under your input probability."),
            DTOutput("Table_prob")
          )
        )
      )
    )
  )
)

# --- Server -------------------------------------------------------------------
server <- function(input, output, session) {
  
  # Monte Carlo simulator:
  # For each pool size n in 1..ceil(sqrt(N)), compute expected group tests + retests
  mc_pooled_testing <- function(num_sim, N, p) {
    max_n <- ceiling(sqrt(N))
    result <- data.frame(pool_size = 1:max_n, expected_tests = NA_real_)
    result$expected_tests[1] <- N  # pool size = 1 is just individual testing
    
    for (n in 2:max_n) {
      total_tests <- replicate(num_sim, {
        sim_sample <- rbinom(N, 1, p)                 # 1 = infected, 0 = clean
        num_groups <- ceiling(N / n)
        groups <- split(sim_sample, rep(1:num_groups, each = n, length.out = N))
        
        group_tests <- num_groups                      # one test per group
        followup_tests <- sum(sapply(groups, function(g) if (sum(g) > 0) length(g) else 0))
        group_tests + followup_tests
      })
      result$expected_tests[n] <- mean(total_tests)
    }
    result
  }
  
  # -------------------------- Disease info modals ------------------------------
  # (Icons removed; simple text modals remain)
  observeEvent(input$influenza_btn, {
    showModal(modalDialog(
      title = "Influenza",
      p("Influenza (flu) is a respiratory illness with symptoms such as fever,
         cough, sore throat, and fatigue. It can cause severe complications."),
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$norovirus_btn, {
    showModal(modalDialog(
      title = "Norovirus",
      p("Norovirus is highly contagious and causes vomiting and diarrhea.
         Transmission occurs via contact, food/water, and surfaces."),
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$adenovirus_btn, {
    showModal(modalDialog(
      title = "Adenovirus",
      p("Adenovirus can cause cold-like illness, conjunctivitis, bronchitis,
         pneumonia, and gastroenteritis."),
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$rhinovirus_btn, {
    showModal(modalDialog(
      title = "Rhinovirus",
      p("Rhinovirus is a major cause of the common cold: cough, sore throat,
         sneezing, runny/stuffy nose."),
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$rsv_btn, {
    showModal(modalDialog(
      title = "RSV",
      p("RSV can cause severe respiratory infections, especially in infants
         and older adults."),
      easyClose = TRUE
    ))
  })
  
  # ---------------------- Workflow 1: Disease + age group ----------------------
  observeEvent(input$submit_disease, {
    # Validate N
    if (input$N <= 1) {
      output$matched_probability <- renderText("⚠️ Please set N > 1.")
      output$Linechart_disease <- renderPlot(NULL)
      output$optimal_pool_size_disease <- renderText(NULL)
      output$Table_disease <- renderDT(NULL)
      return()
    }
    
    # Look up probability from the table
    matched_row <- infection_data[
      infection_data$Disease == input$disease &
        infection_data$Age_Group == input$age_group, ]
    
    if (nrow(matched_row) == 0) {
      output$matched_probability <- renderText(
        "⚠️ No matching data for the selected disease & age group.")
      output$Linechart_disease <- renderPlot(NULL)
      output$optimal_pool_size_disease <- renderText(NULL)
      output$Table_disease <- renderDT(NULL)
      return()
    }
    
    p <- matched_row$Infection_Probability
    
    output$matched_probability <- renderText({
      paste("Matched Infection Probability:", p)
    })
    
    # Run simulation
    result <- mc_pooled_testing(num_sim = 100, N = input$N, p = p)
    optimal_row <- result[which.min(result$expected_tests), ]
    
    output$optimal_pool_size_disease <- renderText({
      paste0("Optimal pool size: ", ceiling(optimal_row$pool_size),
             ", with expected tests: ", ceiling(optimal_row$expected_tests))
    })
    
    output$Linechart_disease <- renderPlot({
      ggplot(result, aes(x = pool_size, y = expected_tests)) +
        geom_line(color = "#800080") +
        geom_point(color = "#800080") +
        geom_vline(xintercept = optimal_row$pool_size, linetype = "dashed",
                   color = "red", size = 1) +
        geom_hline(yintercept = optimal_row$expected_tests, linetype = "dashed",
                   color = "blue", size = 1) +
        labs(title = "Expected Tests vs Pool Size",
             x = "Pool Size", y = "Expected Tests") +
        theme_minimal(base_size = 14)
    })
    
    output$Table_disease <- renderDT({
      datatable(
        result,
        options = list(pageLength = 10, autoWidth = TRUE, searchHighlight = TRUE),
        caption = "Expected tests for each pool size."
      )
    })
  })
  
  # Reset workflow 1
  observeEvent(input$restart_disease, {
    updateNumericInput(session, "N", value = 0)
    updateSelectInput(session, "disease", selected = unique(infection_data$Disease)[1])
    updateSelectInput(session, "age_group", selected = unique(infection_data$Age_Group)[1])
    output$matched_probability <- renderText(NULL)
    output$Linechart_disease <- renderPlot(NULL)
    output$optimal_pool_size_disease <- renderText(NULL)
    output$Table_disease <- renderDT(NULL)
  })
  
  # ---------------------- Workflow 2: Custom probability -----------------------
  observeEvent(input$submit_prob, {
    # Validate inputs
    if (input$p_prob <= 0 || input$p_prob > 0.5 || input$N_prob <= 1) {
      output$optimal_pool_size_prob <- renderText(NULL)
      output$Linechart_prob <- renderPlot(NULL)
      output$Table_prob <- renderDT(NULL)
      showNotification("Please provide valid inputs: N > 1 and 0 < p ≤ 0.5.",
                       type = "warning")
      return()
    }
    
    # Run simulation
    result <- mc_pooled_testing(num_sim = 100, N = input$N_prob, p = input$p_prob)
    optimal_row <- result[which.min(result$expected_tests), ]
    
    output$optimal_pool_size_prob <- renderText({
      paste0("Optimal pool size: ", ceiling(optimal_row$pool_size),
             ", with expected tests: ", ceiling(optimal_row$expected_tests))
    })
    
    output$Linechart_prob <- renderPlot({
      ggplot(result, aes(x = pool_size, y = expected_tests)) +
        geom_line(color = "#800080") +
        geom_point(color = "#800080") +
        geom_vline(xintercept = optimal_row$pool_size, linetype = "dashed",
                   color = "red", size = 1) +
        geom_hline(yintercept = optimal_row$expected_tests, linetype = "dashed",
                   color = "blue", size = 1) +
        labs(title = "Expected Tests vs Pool Size",
             x = "Pool Size", y = "Expected Tests") +
        theme_minimal(base_size = 14)
    })
    
    output$Table_prob <- renderDT({
      datatable(
        result,
        options = list(pageLength = 10, autoWidth = TRUE, searchHighlight = TRUE),
        caption = "Expected tests for each pool size based on your probability."
      )
    })
  })
  
  # Reset workflow 2
  observeEvent(input$restart_prob, {
    updateNumericInput(session, "N_prob", value = 0)
    updateNumericInput(session, "p_prob", value = 0)
    output$Linechart_prob <- renderPlot(NULL)
    output$optimal_pool_size_prob <- renderText(NULL)
    output$Table_prob <- renderDT(NULL)
  })
}

# --- App launcher --------------------------------------------------------------
shinyApp(ui = ui, server = server)
