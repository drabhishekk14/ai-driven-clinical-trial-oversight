# ============================================================
# RShiny App: Sponsor Behaviour Explorer
# ============================================================

library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(readr)
library(scales)
library(DT)
library(here)

# ------------------------------------------------------------
# Load dataset
# ------------------------------------------------------------

df <- read_csv(
  here("data","interim","train_with_sponsor_history_TEMP_with_sponsor_name.csv"),
  show_col_types = FALSE
)

df <- df %>% 
  filter(!is.na(lead_sponsor_name))

# ------------------------------------------------------------
# Sponsor summary dataset for governance map
# ------------------------------------------------------------

sponsor_summary <- df %>%
  group_by(lead_sponsor_name) %>%
  summarise(
    total_trials = n(),
    delay_rate = mean(delayed_reporting, na.rm = TRUE),
    avg_lag = mean(reporting_lag_days, na.rm = TRUE),
    .groups = "drop"
  )

# ------------------------------------------------------------
# UI
# ------------------------------------------------------------

ui <- dashboardPage(
  
  dashboardHeader(title = "Clinical Trial Reporting Behaviour"),
  
  dashboardSidebar(
    
    selectizeInput(
      "sponsor",
      "Select Lead Sponsor",
      choices = NULL,
      multiple = TRUE,
      options = list(
        placeholder = 'Search sponsor name'
      )
    )
    
  ),
  
  dashboardBody(
    
    fluidRow(
      valueBoxOutput("total_trials"),
      valueBoxOutput("delay_rate"),
      valueBoxOutput("avg_lag")
    ),
    
    fluidRow(
      box(width = 12, plotOutput("lag_distribution"))
    ),
    
    fluidRow(
      box(width = 12, plotOutput("delay_bar"))
    ),
    
    fluidRow(
      box(width = 12, plotOutput("governance_map"))
    ),
    
    fluidRow(
      box(width = 12, DTOutput("trial_table"))
    )
    
  )
)

# ------------------------------------------------------------
# SERVER
# ------------------------------------------------------------

server <- function(input, output, session) {
  
  # Load sponsor names into selectize
  updateSelectizeInput(
    session,
    "sponsor",
    choices = sort(unique(df$lead_sponsor_name)),
    server = TRUE
  )
  
  # ----------------------------------------------------------
  # Filtered dataset
  # ----------------------------------------------------------
  
  filtered_data <- reactive({
    
    if (is.null(input$sponsor) || length(input$sponsor) == 0) {
      return(df)
    }
    
    df %>%
      filter(lead_sponsor_name %in% input$sponsor)
    
  })
  
  # ----------------------------------------------------------
  # Metrics
  # ----------------------------------------------------------
  
  output$total_trials <- renderValueBox({
    
    valueBox(
      value = nrow(filtered_data()),
      subtitle = "Total Trials",
      color = "blue"
    )
    
  })
  
  output$delay_rate <- renderValueBox({
    
    delay_rate <- mean(filtered_data()$delayed_reporting, na.rm = TRUE)
    
    valueBox(
      value = percent(delay_rate),
      subtitle = "Delay Rate",
      color = "red"
    )
    
  })
  
  output$avg_lag <- renderValueBox({
    
    avg_lag <- mean(filtered_data()$reporting_lag_days, na.rm = TRUE)
    
    valueBox(
      value = round(avg_lag,1),
      subtitle = "Average Reporting Lag (Days)",
      color = "green"
    )
    
  })
  
  # ----------------------------------------------------------
  # Reporting lag distribution
  # ----------------------------------------------------------
  
  output$lag_distribution <- renderPlot({
    
    ggplot(filtered_data(),
           aes(x = reporting_lag_days)) +
      geom_histogram(
        bins = 40,
        fill = "steelblue",
        color = "white"
      ) +
      labs(
        title = "Reporting Lag Distribution",
        x = "Reporting Lag (Days)",
        y = "Number of Trials"
      ) +
      theme_minimal()
    
  })
  
  # ----------------------------------------------------------
  # Delay vs On-time plot
  # ----------------------------------------------------------
  
  output$delay_bar <- renderPlot({
    
    filtered_data() %>%
      ggplot(aes(x = factor(delayed_reporting))) +
      geom_bar(
        aes(y = after_stat(prop), group = 1),
        fill = "darkorange"
      ) +
      scale_y_continuous(labels = percent_format()) +
      labs(
        title = "Delayed vs On-Time Reporting",
        x = "Delayed Reporting (1 = Delayed)",
        y = "Proportion"
      ) +
      theme_minimal()
    
  })
  
  # ----------------------------------------------------------
  # Governance risk map
  # ----------------------------------------------------------
  
  output$governance_map <- renderPlot({
    
    ggplot(sponsor_summary,
           aes(x = total_trials,
               y = delay_rate,
               size = avg_lag)) +
      
      geom_point(alpha = 0.6, color = "darkred") +
      
      scale_y_continuous(labels = percent_format()) +
      
      labs(
        title = "Sponsor Governance Risk Landscape",
        x = "Number of Trials Conducted",
        y = "Delay Rate",
        size = "Average Lag (Days)"
      ) +
      
      theme_minimal()
    
  })
  
  # ----------------------------------------------------------
  # Table
  # ----------------------------------------------------------
  
  output$trial_table <- renderDT({
    
    filtered_data() %>%
      select(
        nct_id,
        lead_sponsor_name,
        reporting_lag_days,
        delayed_reporting
      )
    
  })
  
}

# ------------------------------------------------------------
# Run App
# ------------------------------------------------------------

shinyApp(ui, server)