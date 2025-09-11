# ========================================
# REUSABLE UI COMPONENTS
# ========================================
# File: R/ui_components.R
# Purpose: Modular UI functions to keep app.R clean

# ========================================
# TAB CREATION FUNCTIONS
# ========================================

create_fetcher_tab <- function() {
  tabItem(tabName = "fetcher",
          # Remove the data source selection box since it's now in sidebar
          fluidRow(
            create_indicator_selection_box(),
            create_country_selection_box()
          ),
          
          fluidRow(
            create_fetch_data_box()
          )
  )
}

create_metadata_tab <- function() {
  tabItem(tabName = "metadata",
          fluidRow(
            box(
              title = "Indicator Metadata Browser", status = "primary", solidHeader = TRUE, width = 12,
              p("Browse available indicators and their definitions."),
              withSpinner(DT::dataTableOutput("metadata_table"), type = 6)
            )
          )
  )
}

create_results_tab <- function() {
  tabItem(tabName = "results",
          fluidRow(
            box(
              title = "Fetched Data Preview", status = "success", solidHeader = TRUE, width = 12,
              withSpinner(DT::dataTableOutput("results_table"), type = 6)
            )
          ),
          
          fluidRow(
            box(
              title = "Data Summary", status = "info", solidHeader = TRUE, width = 6,
              verbatimTextOutput("data_summary")
            ),
            
            box(
              title = "Download Options", status = "warning", solidHeader = TRUE, width = 6,
              downloadButton("download_csv", "Download CSV", class = "btn-primary"),
              br(), br(),
              downloadButton("download_rds", "Download RDS", class = "btn-info")
            )
          )
  )
}

create_processing_tab <- function() {
  tabItem(tabName = "processing",
          fluidRow(
            box(
              title = "Data Cleaning & Processing", status = "warning", solidHeader = TRUE, width = 12,
              
              fluidRow(
                column(6,
                       h4("Clean Fetched Data"),
                       conditionalPanel(
                         condition = "output.has_data",
                         div(
                           p("Transform raw survey data into standardized format with common indicator IDs."),
                           actionButton("clean_data", "Clean & Process Data",
                                        class = "btn-warning btn-lg",
                                        icon = icon("magic"))
                         )
                       ),
                       conditionalPanel(
                         condition = "!output.has_data",
                         div(class = "alert alert-info",
                             icon("info-circle"),
                             " Please fetch data first using the Data Fetcher tab.")
                       )
                ),
                column(6,
                       h4("Processing Status"),
                       uiOutput("cleaning_status")
                )
              )
            )
          ),
          
          fluidRow(
            box(
              title = "Cleaned Data Preview", status = "success", solidHeader = TRUE, width = 12,
              DT::dataTableOutput("cleaned_data_table")
            )
          ),
          
          fluidRow(
            box(
              title = "Download Cleaned Data", status = "success", solidHeader = TRUE, width = 12,
              downloadButton("download_cleaned_csv", "Download Cleaned CSV", class = "btn-success")
            )
          )
  )
}

create_help_tab <- function() {
  tabItem(tabName = "help",
          fluidRow(
            box(
              title = "About This Application", status = "info", solidHeader = TRUE, width = 12,
              
              h4("Purpose"),
              p("This application provides an easy-to-use interface for fetching survey data from multiple international sources:"),
              
              tags$ul(
                tags$li(strong("DHS:"), " Demographic & Health Surveys from 90+ countries"),
                tags$li(strong("MICS:"), " Multiple Indicator Cluster Surveys (UNICEF)"),
                tags$li(strong("UNWPP:"), " UN World Population Prospects")
              ),
              
              h4("Getting Started"),
              tags$ol(
                tags$li("Choose your data source (DHS, MICS, or UNWPP)"),
                tags$li("Select indicators using favorites or browse all available"),
                tags$li("Choose countries"),
                tags$li("Click 'Fetch Data' and wait for results"),
                tags$li("View, analyze, and download your data")
              ),
              
              hr(),
              
              h4("Important Notes"),
              div(class = "alert alert-warning",
                  icon("exclamation-triangle"),
                  " ", strong("ANC1 Indicator:"), " For ANC1 coverage, always use RH_ANCP_W_SKP (ANC1 from skilled provider) rather than RH_ANCN_W_N01 (exactly 1 visit). This ensures data quality and comparability across surveys."),
              
              div(class = "alert alert-success",
                  icon("check-circle"),
                  " ", strong("Côte d'Ivoire:"), " This country is fully supported across all data sources using ISO code 'CI'.")
            )
          )
  )
}

# ========================================
# BOX CREATION FUNCTIONS
# ========================================

create_indicator_selection_box <- function() {
  box(
    title = "Indicator Selection", status = "info", solidHeader = TRUE, width = 6,
    
    conditionalPanel(
      condition = "input.data_source == 'dhs'",
      div(
        h5(icon("star"), "Quick Select Favorites"),
        div(class = "alert alert-info", style = "margin-bottom: 10px; padding: 8px;",
            icon("info-circle"),
            " ", strong("ANC1 Note:"), " We use RH_ANCP_W_SKP (ANC1 from skilled provider) as the standard ANC1 indicator."),
        
        create_dhs_favorite_buttons(),
        hr()
      )
    ),
    
    conditionalPanel(
      condition = "input.data_source == 'mics'",
      div(
        h5(icon("star"), "Quick Select MICS Favorites"),
        create_mics_favorite_buttons(),
        hr()
      )
    ),
    
    withSpinner(uiOutput("indicator_selector"), type = 6)
  )
}

create_country_selection_box <- function() {
  box(
    title = "Country Selection", status = "info", solidHeader = TRUE, width = 6,
    
    withSpinner(uiOutput("country_selector"), type = 6),
    
    conditionalPanel(
      condition = "input.data_source == 'dhs'",
      radioButtons("breakdown", "Geographic Level:",
                   choices = list("National" = "national", "Subnational" = "subnational"),
                   selected = "national")
    ),
    
    conditionalPanel(
      condition = "input.data_source == 'unwpp'",
      div(
        h5("Date Range:"),
        fluidRow(
          column(6, numericInput("start_year", "Start Year:", value = 2020, min = 2000, max = 2025)),
          column(6, numericInput("end_year", "End Year:", value = 2025, min = 2000, max = 2030))
        )
      )
    )
  )
}

create_fetch_data_box <- function() {
  box(
    title = "Fetch Data", status = "success", solidHeader = TRUE, width = 12,
    
    div(style = "text-align: center;",
        actionButton("fetch_data", "Fetch Data",
                     class = "btn-success btn-lg",
                     icon = icon("play"),
                     style = "margin-bottom: 15px;")),
    
    div(id = "fetch_status",
        uiOutput("status_message"))
  )
}

# ========================================
# FAVORITE BUTTON COMPONENTS
# ========================================

create_dhs_favorite_buttons <- function() {
  div(style = "margin-bottom: 15px;",
      div(class = "btn-group-toggle", `data-toggle` = "buttons",
          actionButton("select_maternal",
                       HTML('ANC & Maternal <span id="maternal_count" class="badge badge-light">0</span>'),
                       class = "btn btn-outline-primary btn-sm",
                       style = "margin: 2px;"),
          actionButton("select_vaccines",
                       HTML('Vaccinations <span id="vaccine_count" class="badge badge-light">0</span>'),
                       class = "btn btn-outline-primary btn-sm",
                       style = "margin: 2px;"),
          actionButton("select_iptp",
                       HTML('IPTp <span id="iptp_count" class="badge badge-light">0</span>'),
                       class = "btn btn-outline-primary btn-sm",
                       style = "margin: 2px;"),
          actionButton("select_mortality",
                       HTML('Mortality <span id="mortality_count" class="badge badge-light">0</span>'),
                       class = "btn btn-outline-primary btn-sm",
                       style = "margin: 2px;")
      ),
      br(),
      div(style = "margin-top: 10px;",
          actionButton("select_all_favorites",
                       HTML('All Favorites <span id="all_count" class="badge badge-warning">0</span>'),
                       class = "btn btn-warning btn-sm",
                       style = "margin: 2px;"),
          actionButton("clear_selection",
                       HTML('Clear All'),
                       class = "btn btn-outline-secondary btn-sm",
                       style = "margin: 2px;")
      )
  )
}

create_mics_favorite_buttons <- function() {
  div(style = "margin-bottom: 15px;",
      actionButton("select_mics_maternal", "Maternal Health",
                   class = "btn-sm btn-outline-info", style = "margin: 2px;"),
      actionButton("select_mics_vaccines", "Child Immunization",
                   class = "btn-sm btn-outline-info", style = "margin: 2px;"),
      actionButton("select_mics_mortality", "Child Mortality",
                   class = "btn-sm btn-outline-info", style = "margin: 2px;")
  )
}

# ========================================
# HEADER AND SIDEBAR COMPONENTS
# ========================================

create_app_header <- function() {
  dashboardHeader(
    title = HTML('<span style="font-weight: 600; letter-spacing: 0.5px;">
                <i class="fa fa-chart-line" style="margin-right: 8px; color: white;"></i>
                FASTR Survey Data Fetcher
                </span>'),
    titleWidth = 350
  )
}


create_app_sidebar <- function() {
  dashboardSidebar(
    # Data source selection at top of sidebar
    div(style = "padding: 15px; border-bottom: 1px solid rgba(255,255,255,0.2); margin-bottom: 15px;",
        h3("Get data from:", 
           style = "color: white; margin-bottom: 15px; font-weight: bold;"),
        radioButtons("data_source", NULL,
                     choices = list(
                       "DHS - Demographic & Health Surveys" = "dhs", 
                       "MICS - Multiple Indicator Cluster" = "mics",
                       "UN World Population Prospects" = "unwpp"
                     ),
                     selected = "dhs")
    ),
    
    sidebarMenu(
      menuItem("Browse Metadata", tabName = "metadata", icon = icon("table")),
      menuItem("Fetch Data", tabName = "fetcher", icon = icon("download")),
      menuItem("Results", tabName = "results", icon = icon("chart-line")),
      menuItem("Clean & Process", tabName = "processing", icon = icon("magic")),
      menuItem("Help & Info", tabName = "help", icon = icon("question-circle"))
    )
  )
}