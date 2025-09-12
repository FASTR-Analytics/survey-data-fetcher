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
          # Step 1: Cleaning Configuration
          fluidRow(
            box(
              title = "Step 1: Configure Cleaning Rules", status = "info", solidHeader = TRUE, width = 12,
              
              conditionalPanel(
                condition = "output.has_data",
                div(
                  p("Review and configure how your raw data should be cleaned and standardized."),
                  
                  fluidRow(
                    column(8,
                           h5("Indicator Mapping & Filtering"),
                           div(class = "alert alert-info", style = "margin-bottom: 15px;",
                               icon("info-circle"),
                               " Configure how each fetched indicator should be mapped to common IDs and what filters to apply."),
                           
                           DT::dataTableOutput("cleaning_config_table")
                    ),
                    column(4,
                           h5("Configuration Actions"),
                           
                           div(style = "margin-bottom: 15px;",
                               actionButton("load_default_config", "📋 Load Default Settings",
                                           class = "btn-outline-primary btn-block",
                                           style = "margin-bottom: 8px;"),
                               actionButton("reset_config", "🔄 Reset All",
                                           class = "btn-outline-secondary btn-block",
                                           style = "margin-bottom: 8px;"),
                               actionButton("save_config", "💾 Save Config",
                                           class = "btn-outline-success btn-block")
                           ),
                           
                           hr(),
                           
                           h6("Quick Filters"),
                           div(style = "margin-bottom: 10px;",
                               checkboxInput("filter_totals_only", "Keep totals only (no age/sex breakdown)", value = TRUE),
                               checkboxInput("filter_preferred", "Keep preferred estimates only", value = TRUE),
                               checkboxInput("filter_median_variant", "Keep median variant only (UNWPP)", value = TRUE)
                           ),
                           
                           div(class = "alert alert-warning", style = "font-size: 12px;",
                               strong("Note: "), "Changes will be applied when you click 'Apply Configuration' below.")
                    )
                  ),
                  
                  div(style = "text-align: center; margin-top: 20px;",
                      actionButton("apply_config", "✅ Apply Configuration",
                                  class = "btn-success btn-lg",
                                  icon = icon("check"),
                                  style = "margin-right: 15px;"),
                      actionButton("preview_config", "👁️ Preview Changes",
                                  class = "btn-info btn-lg",
                                  icon = icon("eye"))
                  )
                )
              ),
              
              conditionalPanel(
                condition = "!output.has_data",
                div(class = "alert alert-info",
                    icon("info-circle"),
                    " Please fetch data first using the Data Fetcher tab.")
              )
            )
          ),
          
          # Step 2: Traditional Processing (kept for backward compatibility)
          fluidRow(
            box(
              title = "Step 2: Quick Clean (Legacy)", status = "warning", solidHeader = TRUE, width = 12, collapsed = TRUE,
              
              fluidRow(
                column(6,
                       h4("Use Default Cleaning"),
                       conditionalPanel(
                         condition = "output.has_data",
                         div(
                           p("Apply default cleaning rules without configuration (legacy method)."),
                           actionButton("clean_data", "Quick Clean with Defaults",
                                        class = "btn-warning",
                                        icon = icon("magic"))
                         )
                       )
                ),
                column(6,
                       h4("Processing Status"),
                       uiOutput("cleaning_status")
                )
              )
            )
          ),
          
          # Results
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
      menuItem("Visualizations", tabName = "visualize", icon = icon("chart-bar")),
      menuItem("Help & Info", tabName = "help", icon = icon("question-circle"))
    )
  )
}

# ========================================
# VISUALIZATION TAB FUNCTION
# ========================================
# Purpose: Provides UI components for time series and multi-indicator visualizations of cleaned survey data

create_visualization_tab <- function() {
  tabItem(tabName = "visualize",
          fluidRow(
            box(
              title = "Time Series Visualization", status = "primary", solidHeader = TRUE, width = 12,
              
              conditionalPanel(
                condition = "output.has_cleaned_data",
                fluidRow(
                  column(4,
                         h5("Visualization Options"),
                         div(class = "alert alert-info", style = "margin-bottom: 15px;",
                             icon("info-circle"),
                             " Select your options below, then click 'Generate Plot' to create your visualization."),
                         
                         selectInput("plot_indicator", "Select Indicator:",
                                    choices = NULL),
                         
                         selectInput("plot_countries", "Select Geographic Areas:",
                                    choices = NULL,
                                    multiple = TRUE),
                         
                         radioButtons("plot_type", "Chart Type:",
                                     choices = list(
                                       "Line Chart" = "line",
                                       "Point Chart" = "point", 
                                       "Lines + Points" = "both"
                                     ),
                                     selected = "both"),
                         
                         checkboxInput("show_trend", "Add Trend Line", value = FALSE),
                         
                         br(),
                         actionButton("generate_plot", "📊 Generate Plot",
                                     class = "btn-primary btn-block btn-lg",
                                     style = "font-weight: bold;")
                  ),
                  
                  column(8,
                         h5("Time Series Chart"),
                         div(id = "plot-placeholder", class = "well text-center", style = "height: 480px; line-height: 480px; background: #f9f9f9; border: 2px dashed #ddd; margin-bottom: 20px;",
                             icon("chart-line", style = "font-size: 48px; color: #ccc; margin-right: 15px;"),
                             span("Select options and click 'Generate Plot' to create your time series visualization", 
                                  style = "color: #999; font-size: 16px; vertical-align: middle;")
                         ),
                         withSpinner(plotlyOutput("time_series_plot", height = "500px"), type = 6)
                  )
                )
              ),
              
              conditionalPanel(
                condition = "!output.has_cleaned_data",
                div(class = "alert alert-info",
                    icon("info-circle"),
                    " Please clean your data first in the 'Clean & Process' tab to enable visualizations.")
              )
            )
          ),
          
          fluidRow(
            box(
              title = "Multi-Indicator Comparison", status = "info", solidHeader = TRUE, width = 12,
              
              conditionalPanel(
                condition = "output.has_cleaned_data",
                fluidRow(
                  column(4,
                         h5("Comparison Options"),
                         div(class = "alert alert-info", style = "margin-bottom: 15px;",
                             icon("info-circle"),
                             " Choose one geographic area and multiple indicators to compare them over time."),
                         
                         selectInput("comparison_country", "Select Geographic Area:",
                                    choices = NULL),
                         
                         selectInput("comparison_indicators", "Select Indicators:",
                                    choices = NULL,
                                    multiple = TRUE),
                         
                         radioButtons("comparison_scale", "Y-axis Scale:",
                                     choices = list(
                                       "Free Scale (Separate Charts)" = "free",
                                       "Fixed Scale (One Chart)" = "fixed"
                                     ),
                                     selected = "free"),
                         
                         br(),
                         actionButton("generate_comparison", "📈 Generate Comparison",
                                     class = "btn-info btn-block btn-lg",
                                     style = "font-weight: bold;")
                  ),
                  
                  column(8,
                         h5("Multi-Indicator Chart"),
                         div(id = "comparison-placeholder", class = "well text-center", style = "height: 480px; line-height: 480px; background: #f9f9f9; border: 2px dashed #ddd; margin-bottom: 20px;",
                             icon("chart-bar", style = "font-size: 48px; color: #ccc; margin-right: 15px;"),
                             span("Select options and click 'Generate Comparison' to create your multi-indicator chart", 
                                  style = "color: #999; font-size: 16px; vertical-align: middle;")
                         ),
                         withSpinner(plotlyOutput("comparison_plot", height = "500px"), type = 6)
                  )
                )
              )
            )
          )
  )
}

