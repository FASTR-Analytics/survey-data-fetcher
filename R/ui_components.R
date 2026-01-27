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
          # NEW: Data Cart Management
          fluidRow(
            box(
              title = "Session Data Cart", status = "primary", solidHeader = TRUE, width = 12, collapsible = TRUE,
              div(class = "alert alert-info",
                  icon("info-circle"),
                  " Your data cart allows you to accumulate multiple datasets in a single session. Fetch data from different sources or indicators, add them to your cart, and combine them for visualization."),

              fluidRow(
                column(9,
                       DT::dataTableOutput("cart_table")
                ),
                column(3,
                       h5("Cart Actions"),
                       actionButton("remove_selected_from_cart", "Remove Selected",
                                  class = "btn-warning btn-block",
                                  icon = icon("trash")),
                       br(),
                       actionButton("clear_cart", "Clear All",
                                  class = "btn-danger btn-block",
                                  icon = icon("trash-alt")),
                       br(),
                       div(class = "alert alert-secondary", style = "margin-top: 10px;",
                           uiOutput("cart_summary"))
                )
              )
            )
          ),

          # Current/Latest Fetch Preview
          fluidRow(
            box(
              title = "Latest Fetch Preview", status = "success", solidHeader = TRUE, width = 12, collapsible = TRUE, collapsed = FALSE,
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
              downloadButton("download_csv", "Download Latest CSV", class = "btn-primary"),
              br(), br(),
              downloadButton("download_rds", "Download Latest RDS", class = "btn-info"),
              br(), br(),
              downloadButton("download_cart_csv", "Download All Cart Data (CSV)", class = "btn-success")
            )
          )
  )
}

create_processing_tab <- function() {
  tabItem(tabName = "processing",
          # Clean Data
          fluidRow(
            box(
              title = "Clean & Process Data", status = "success", solidHeader = TRUE, width = 12,

              fluidRow(
                column(6,
                       h4("Clean Latest Fetch"),
                       conditionalPanel(
                         condition = "output.has_data",
                         div(
                           p("Apply standard cleaning and standardization to your latest fetched data."),
                           div(style = "margin-bottom: 15px;",
                               checkboxInput("apply_fastr_standardization",
                                           "Apply FASTR name standardization",
                                           value = TRUE,
                                           width = "100%"),
                               div(class = "alert alert-info", style = "margin-top: 10px; padding: 8px; font-size: 12px;",
                                   icon("info-circle", style = "margin-right: 5px;"),
                                   "Standardizes country and province names for FASTR Analytics Platform compatibility")
                           ),
                           actionButton("clean_data", "Clean Latest Data",
                                        class = "btn-success btn-lg",
                                        icon = icon("magic")),
                           br(), br(),
                           actionButton("clean_cart_data", "Clean All Cart Data",
                                        class = "btn-primary btn-lg",
                                        icon = icon("shopping-cart"),
                                        style = "margin-top: 10px;"),
                           div(class = "alert alert-warning", style = "margin-top: 10px; padding: 8px; font-size: 12px;",
                               icon("info-circle", style = "margin-right: 5px;"),
                               "Clean All Cart Data will clean and combine ALL datasets in your cart, preserving both national and subnational data.")
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
                       uiOutput("cleaning_status"),
                       uiOutput("cart_cleaning_status")
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
              title = "Documentation", status = "primary", solidHeader = TRUE, width = 12,
              div(style = "text-align: center; padding: 20px;",
                  tags$a(href = "https://fastr-analytics.github.io/survey-data-fetcher/",
                         target = "_blank",
                         class = "btn btn-primary btn-lg",
                         icon("book"), " View Full Documentation")
              )
            )
          ),
          fluidRow(
            box(
              title = "About This Application", status = "info", solidHeader = TRUE, width = 12,

              h4("Purpose"),
              p("This application provides an easy-to-use interface for fetching survey data from multiple international sources using real-time API connections:"),

              tags$ul(
                tags$li(strong("DHS:"), " Demographic & Health Surveys from 90+ countries via the DHS Program API"),
                tags$li(strong("UNICEF:"), " Multiple data sources including MICS surveys, WHO/UNICEF immunization estimates (WUENIC), and UN IGME mortality estimates via SDMX web services"),
                tags$li(strong("UNWPP:"), " UN World Population Prospects via UN Data API")
              ),

              h4("Application Tabs"),
              tags$ul(
                tags$li(strong("Browse Metadata:"), " View available indicators and their definitions"),
                tags$li(strong("Fetch Data:"), " Select data source, indicators, and countries to fetch via API"),
                tags$li(strong("Results:"), " Preview fetched data and manage your data cart"),
                tags$li(strong("Clean & Process:"), " Apply FASTR standardization to harmonize data"),
                tags$li(strong("Manual Entry:"), " Enter survey values manually with enforced standard indicator codes"),
                tags$li(strong("Data Review:"), " Compare fetched data against existing database with visual charts"),
                tags$li(strong("Database Integration:"), " Validate names, check duplicates, and push to GitHub")
              ),

              h4("Workflow"),
              tags$ol(
                tags$li("Choose your data source (DHS, UNICEF, or UNWPP) from the sidebar"),
                tags$li("Select indicators using quick favorites or browse all available options"),
                tags$li("Choose your countries of interest"),
                tags$li("Click 'Fetch Data' and wait for the API to retrieve your data"),
                tags$li("Clean and process your data using the built-in tools"),
                tags$li("Use Data Review to compare against existing database"),
                tags$li("Go to Database Integration to validate, check duplicates, and push to GitHub")
              ),

              p(em("Alternatively, use Manual Entry to add individual data points from reports or other sources.")),

              h4("Data Processing"),
              p("The cleaning process harmonizes different data formats across sources and prepares the data for integration into the FASTR Analytics Platform. Admin area names are standardized using DHIS2 reference data.")
            )
          ),
          fluidRow(
            box(
              title = "License", status = "warning", solidHeader = TRUE, width = 12,
              p(strong("Copyright (c) 2025"), " The World Bank, Global Financing Facility for Women, Children and Adolescents (GFF), Frequent Assessments and System Tools for Resilience (FASTR) Initiative"),
              p("All rights reserved."),
              p("This software is made publicly available for transparency and reference purposes only. Viewing and reviewing the source code is permitted. Copying, modification, distribution, or use requires express written permission.")
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

    # Indicator Mode Toggle
    div(style = "margin-bottom: 15px;",
        radioButtons("indicator_mode", "Selection Mode:",
                    choices = list(
                      "Favorites (Quick Select)" = "favorites",
                      "Browse All (Advanced)" = "browse"
                    ),
                    selected = "favorites",
                    inline = TRUE)
    ),

    # Favorites Mode - DHS
    conditionalPanel(
      condition = "input.data_source == 'dhs' && input.indicator_mode == 'favorites'",
      div(
        h5(icon("star"), "Quick Select Favorites"),
        div(class = "alert alert-info", style = "margin-bottom: 10px; padding: 8px;",
            icon("info-circle"),
            " ", strong("ANC1 Note:"), " We use RH_ANCP_W_SKP (ANC1 from skilled provider) as the standard ANC1 indicator."),

        create_dhs_favorite_buttons(),
        hr()
      )
    ),
    
    # Favorites Mode - UNICEF
    conditionalPanel(
      condition = "input.data_source == 'unicef' && input.indicator_mode == 'favorites'",
      div(
        h5(icon("star"), "Quick Select UNICEF Favorites"),
        create_unicef_favorite_buttons(),
        hr()
      )
    ),

    # Favorites Mode - UNWPP
    conditionalPanel(
      condition = "input.data_source == 'unwpp' && input.indicator_mode == 'favorites'",
      div(
        h5(icon("star"), "Quick Select UNWPP Favorites"),
        create_unwpp_favorite_buttons(),
        hr()
      )
    ),

    # Browse Mode - All sources
    conditionalPanel(
      condition = "input.indicator_mode == 'browse'",
      div(
        h5(icon("search"), "Browse All Indicators"),
        div(class = "alert alert-warning", style = "margin-bottom: 10px; padding: 8px;",
            icon("exclamation-triangle"),
            " Browse mode shows ALL available indicators. Use search to filter."),
        hr()
      )
    ),

    # Indicator Picker (shown in both modes)
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

    # NEW: Add to Cart option
    div(style = "margin-bottom: 15px;",
        checkboxInput("add_to_cart", "Add to session cart (accumulate datasets)", value = TRUE),
        div(class = "alert alert-info", style = "padding: 8px; font-size: 12px;",
            icon("info-circle"),
            " When checked, fetched data will be added to your cart. Uncheck to replace previous data.")
    ),

    div(style = "text-align: center;",
        actionButton("fetch_data", "Fetch Data",
                     class = "btn-success btn-lg",
                     icon = icon("download"),
                     style = "margin-bottom: 15px;")),

    # Progress indicator
    div(id = "progress_container", style = "margin-bottom: 15px;",
        conditionalPanel(
          condition = "input.fetch_data > 0",
          div(
            div(class = "progress", style = "height: 8px; margin-bottom: 10px;",
                div(id = "fetch_progress", class = "progress-bar",
                    style = "width: 0%; transition: width 0.3s ease;")
            ),
            div(id = "progress_text", style = "font-size: 14px; color: #6c757d; text-align: center;",
                "Ready to fetch data...")
          )
        )
    ),

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

create_unicef_favorite_buttons <- function() {
  div(style = "margin-bottom: 15px;",
      actionButton("select_unicef_maternal", "Maternal Health",
                   class = "btn-sm btn-outline-info", style = "margin: 2px;"),
      actionButton("select_unicef_vaccines", "Child Immunization",
                   class = "btn-sm btn-outline-info", style = "margin: 2px;"),
      actionButton("select_unicef_mortality", "Child Mortality",
                   class = "btn-sm btn-outline-info", style = "margin: 2px;")
  )
}

create_unwpp_favorite_buttons <- function() {
  div(style = "margin-bottom: 15px;",
      div(class = "btn-group-toggle", `data-toggle` = "buttons",
          actionButton("select_unwpp_health",
                       HTML('Health & Mortality <span id="unwpp_health_count" class="badge badge-light">0</span>'),
                       class = "btn btn-outline-primary btn-sm",
                       style = "margin: 2px;"),
          actionButton("select_unwpp_demographics",
                       HTML('Demographics <span id="unwpp_demo_count" class="badge badge-light">0</span>'),
                       class = "btn btn-outline-primary btn-sm",
                       style = "margin: 2px;"),
          actionButton("select_unwpp_social",
                       HTML('Social Structure <span id="unwpp_social_count" class="badge badge-light">0</span>'),
                       class = "btn btn-outline-primary btn-sm",
                       style = "margin: 2px;")
      ),
      br(),
      div(style = "margin-top: 10px;",
          actionButton("select_unwpp_favorites",
                       HTML('Current Favorites <span id="unwpp_fav_count" class="badge badge-warning">0</span>'),
                       class = "btn btn-warning btn-sm",
                       style = "margin: 2px;"),
          actionButton("clear_unwpp_selection",
                       HTML('Clear All'),
                       class = "btn btn-outline-secondary btn-sm",
                       style = "margin: 2px;")
      )
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
                       "UNICEF SDMX API" = "unicef",
                       "UN World Population Prospects" = "unwpp"
                     ),
                     selected = "dhs")
    ),
    
    sidebarMenu(
      menuItem("Browse Metadata", tabName = "metadata", icon = icon("table")),
      menuItem("Fetch Data", tabName = "fetcher", icon = icon("download")),
      menuItem("Results", tabName = "results", icon = icon("chart-line")),
      menuItem("Clean & Process", tabName = "processing", icon = icon("magic")),
      menuItem("Manual Entry", tabName = "manual_entry", icon = icon("keyboard")),
      menuItem("Database Explorer", tabName = "db_explorer", icon = icon("database")),
      menuItem("Data Review", tabName = "data_review", icon = icon("search")),
      menuItem("Database Integration", tabName = "integration", icon = icon("cloud-upload-alt")),
      menuItem("Help & Info", tabName = "help", icon = icon("question-circle"))
    )
  )
}

# ========================================
# DATABASE INTEGRATION TAB
# ========================================

create_integration_tab <- function() {
  tabItem(tabName = "integration",
          # Introduction and GitHub Status
          fluidRow(
            box(
              title = "Database Integration (GitHub Sync)", status = "primary", solidHeader = TRUE, width = 12,
              div(class = "alert alert-info",
                  icon("github"),
                  " This tab syncs with GitHub: pulls the latest database, validates your data, then pushes updates back. ",
                  "Follow the steps below: pull from GitHub, validate names, check duplicates, then push to GitHub."
              ),
              fluidRow(
                column(6,
                       h5("Step 0: Pull Latest from GitHub"),
                       p("Pull the current database from GitHub before making changes."),
                       actionButton("pull_from_github", "Pull Latest from GitHub",
                                   class = "btn-primary btn-lg",
                                   icon = icon("cloud-download-alt")),
                       br(), br(),
                       uiOutput("github_pull_status")
                ),
                column(6,
                       h5("GitHub Repository"),
                       div(class = "well",
                           p(strong("Repo:"), " FASTR-Analytics/modules"),
                           p(strong("Branch:"), " main"),
                           p(strong("Survey DB:"), " survey_data_unified.csv"),
                           p(strong("Population DB:"), " population_estimates_only.csv")
                       )
                )
              ),
              conditionalPanel(
                condition = "!output.has_cleaned_data",
                div(class = "alert alert-warning",
                    icon("exclamation-triangle"),
                    " Please clean your data first in the 'Clean & Process' tab before integration.")
              )
            )
          ),

          # Step 1: Name Validation
          fluidRow(
            box(
              title = "Step 1: Validate Admin Area Names", status = "warning", solidHeader = TRUE, width = 12, collapsible = TRUE,

              conditionalPanel(
                condition = "output.has_cleaned_data",
                fluidRow(
                  column(4,
                         p("Check if admin area names in your data match the existing database."),
                         actionButton("validate_names", "Validate Admin Area Names",
                                     class = "btn-warning btn-lg",
                                     icon = icon("check-circle")),
                         br(), br(),
                         uiOutput("validation_status")
                  ),
                  column(8,
                         h5("Unmatched Admin Areas"),
                         p("For each unmatched area, select the correct name from the database or choose 'IGNORE' to skip those records."),
                         uiOutput("unmatched_areas_ui"),
                         conditionalPanel(
                           condition = "output.has_unmatched_areas",
                           hr(),
                           actionButton("apply_corrections", "Apply Corrections & Continue",
                                       class = "btn-success btn-lg",
                                       icon = icon("check"))
                         )
                  )
                )
              )
            )
          ),

          # Step 2: Duplicate Detection
          fluidRow(
            box(
              title = "Step 2: Check for Duplicates", status = "info", solidHeader = TRUE, width = 12, collapsible = TRUE,

              conditionalPanel(
                condition = "output.has_cleaned_data",
                fluidRow(
                  column(12,
                         fluidRow(
                           column(4,
                                  actionButton("check_duplicates", "Check for Duplicates",
                                              class = "btn-info btn-lg",
                                              icon = icon("clone"))
                           ),
                           column(8,
                                  uiOutput("duplicate_status_summary")
                           )
                         )
                  )
                ),

                # Records with Different Values section
                conditionalPanel(
                  condition = "output.has_different_values",
                  hr(),
                  fluidRow(
                    column(8,
                           h5(icon("exclamation-triangle", style = "color: #f39c12;"),
                              " Records with Different Values"),
                           div(class = "alert alert-warning", style = "padding: 10px; margin-bottom: 10px;",
                               "These records exist in the database but have ", strong("different values"),
                               " than your fetched data. Use the buttons in each row to choose whether to ",
                               strong("Replace"), " with new values or ", strong("Keep"), " existing values.")
                    ),
                    column(4,
                           h5("Bulk Action"),
                           selectInput("duplicate_action_choice", "Apply to all rows:",
                                      choices = c(
                                        "Keep existing values" = "keep_existing",
                                        "Replace with new values" = "replace"
                                      ),
                                      selected = "keep_existing"),
                           actionButton("apply_duplicate_action", "Apply to All",
                                       class = "btn-warning btn-block",
                                       icon = icon("check"))
                    )
                  ),
                  DT::dataTableOutput("different_values_table")
                ),

                # Records with Same Values (collapsed by default)
                conditionalPanel(
                  condition = "output.has_same_values",
                  hr(),
                  h5(icon("check-circle", style = "color: #27ae60;"), " Records with Same Values (no action needed)"),
                  p(style = "font-size: 12px; color: #666;",
                    "These records already exist in the database with the same values - they will be skipped."),
                  DT::dataTableOutput("same_values_table")
                ),

                # New Records section
                conditionalPanel(
                  condition = "output.has_new_records",
                  hr(),
                  h5(icon("plus-circle", style = "color: #3498db;"), " New Records to Add"),
                  p(style = "font-size: 12px; color: #666;",
                    "These records don't exist in the database and will be added."),
                  DT::dataTableOutput("new_records_preview")
                )
              )
            )
          ),

          # Step 3: Append and Push to GitHub
          fluidRow(
            box(
              title = "Step 3: Append & Push to GitHub", status = "success", solidHeader = TRUE, width = 12, collapsible = TRUE,

              conditionalPanel(
                condition = "output.has_cleaned_data",
                fluidRow(
                  column(6,
                         h4("Final Summary"),
                         verbatimTextOutput("integration_summary"),
                         hr(),
                         checkboxInput("push_to_github", "Push changes to GitHub", value = TRUE),
                         div(class = "alert alert-info", style = "padding: 8px; font-size: 12px;",
                             icon("info-circle"),
                             " Requires GITHUB_TOKEN in .Renviron file (HF Spaces: set as secret)"),
                         br(),
                         textAreaInput("commit_notes", "Commit Notes (optional):",
                                      placeholder = "Add any notes about this update...",
                                      rows = 2, width = "100%"),
                         p(style = "font-size: 11px; color: #666;",
                           "Auto-includes: countries, indicators, record counts, source, timestamp"),
                         br(),
                         actionButton("append_to_database", "Append & Push to GitHub",
                                     class = "btn-success btn-lg",
                                     icon = icon("cloud-upload-alt")),
                         br(), br(),
                         uiOutput("append_status")
                  ),
                  column(6,
                         h4("Commit Message Preview"),
                         uiOutput("commit_preview"),
                         hr(),
                         div(class = "well",
                             p(strong("Target Repo:"), " FASTR-Analytics/modules"),
                             p(strong("Files to update:")),
                             tags$ul(
                               tags$li("survey_data_unified.csv"),
                               tags$li("population_estimates_only.csv")
                             )
                         ),
                         uiOutput("github_token_status")
                  )
                )
              )
            )
          )
  )
}

# ========================================
# DATABASE EXPLORER TAB
# ========================================

create_database_explorer_tab <- function() {
  tabItem(tabName = "db_explorer",
          fluidRow(
            box(
              title = "Database Explorer", status = "primary", solidHeader = TRUE, width = 12,
              div(class = "alert alert-info",
                  icon("database"),
                  " Browse the GitHub database directly. Filter by country and indicator to see what data is available.")
            )
          ),

          # Load Database Section
          fluidRow(
            box(
              title = "Load Database", status = "warning", solidHeader = TRUE, width = 12,
              fluidRow(
                column(3,
                       actionButton("explorer_load_db", "Load Database from GitHub",
                                   class = "btn-warning btn-lg",
                                   icon = icon("cloud-download-alt"))
                ),
                column(9,
                       uiOutput("explorer_db_status")
                )
              )
            )
          ),

          fluidRow(
            # Filters
            box(
              title = "Filters", status = "info", solidHeader = TRUE, width = 4,

              selectInput("explorer_iso3", "Country (ISO3):",
                         choices = c("Loading..." = ""),
                         selected = NULL),

              conditionalPanel(
                condition = "input.explorer_iso3 != '' && input.explorer_iso3 != null",
                selectInput("explorer_indicator", "Indicator:",
                           choices = c("All indicators" = ""),
                           selected = NULL,
                           multiple = FALSE),

                selectInput("explorer_region", "Region:",
                           choices = c("All regions" = ""),
                           selected = NULL,
                           multiple = TRUE),

                selectInput("explorer_source", "Source:",
                           choices = c("All sources" = ""),
                           selected = NULL,
                           multiple = TRUE)
              ),

              hr(),
              actionButton("explorer_apply_filter", "Apply Filters",
                          class = "btn-primary btn-block",
                          icon = icon("filter")),
              br(),
              downloadButton("explorer_download", "Download Filtered Data",
                            class = "btn-success btn-block")
            ),

            # Results
            box(
              title = "Database Contents", status = "success", solidHeader = TRUE, width = 8,

              uiOutput("explorer_summary"),

              hr(),

              withSpinner(DT::dataTableOutput("explorer_table"), type = 6)
            )
          ),

          # Time series plot
          fluidRow(
            box(
              title = "Time Series View", status = "info", solidHeader = TRUE, width = 12, collapsible = TRUE,
              withSpinner(plotlyOutput("explorer_plot", height = "350px"), type = 6)
            )
          )
  )
}

# ========================================
# DATA REVIEW TAB (DB vs NEW COMPARISON)
# ========================================

create_data_review_tab <- function() {
  tabItem(tabName = "data_review",
          fluidRow(
            box(
              title = "Data Coverage Review", status = "primary", solidHeader = TRUE, width = 12,
              div(class = "alert alert-info",
                  icon("info-circle"),
                  " Review data coverage by country and region. Compare what's in the database (teal) with newly fetched data (red).")
            )
          ),

          # Database Loading Section
          fluidRow(
            box(
              title = "Database Status", status = "warning", solidHeader = TRUE, width = 12,
              fluidRow(
                column(4,
                       actionButton("load_review_database", "Load Database from GitHub",
                                   class = "btn-warning btn-lg",
                                   icon = icon("cloud-download-alt"))
                ),
                column(8,
                       uiOutput("review_db_status"),
                       uiOutput("review_duplicate_summary")
                )
              )
            )
          ),

          fluidRow(
            # Controls
            box(
              title = "Selection", status = "info", solidHeader = TRUE, width = 4,

              h5("Step 1: Select Country"),
              selectInput("review_country", "Country:",
                         choices = NULL,
                         selected = NULL),

              conditionalPanel(
                condition = "input.review_country != '' && input.review_country != null",
                h5("Step 2: Select Region(s)"),
                checkboxInput("review_include_national", "Include National-level data", value = TRUE),
                selectInput("review_regions", "Regions:",
                           choices = NULL,
                           selected = NULL,
                           multiple = TRUE)
              ),

              conditionalPanel(
                condition = "input.review_country != '' && input.review_country != null",
                h5("Step 3: Select Indicator"),
                selectInput("review_indicator", "Indicator:",
                           choices = NULL,
                           selected = NULL)
              ),

              hr(),
              actionButton("generate_review_plot", "Generate Coverage Chart",
                          class = "btn-primary btn-block btn-lg",
                          icon = icon("chart-line"))
            ),

            # Chart and Tables
            box(
              title = "Coverage Comparison", status = "success", solidHeader = TRUE, width = 8,

              # Legend - Database sources
              div(style = "margin-bottom: 10px; padding: 10px; background: #f8f9fa; border-radius: 4px;",
                  div(style = "margin-bottom: 8px;",
                      strong("Database Sources: "),
                      span(style = "display: inline-block; width: 14px; height: 14px; background: #0f706d; margin-left: 10px; margin-right: 3px; border-radius: 2px; vertical-align: middle;"),
                      span("DHS", style = "margin-right: 12px; font-size: 12px;"),
                      span(style = "display: inline-block; width: 14px; height: 14px; background: #3498db; margin-right: 3px; border-radius: 2px; vertical-align: middle;"),
                      span("MICS", style = "margin-right: 12px; font-size: 12px;"),
                      span(style = "display: inline-block; width: 14px; height: 14px; background: #9b59b6; margin-right: 3px; border-radius: 2px; vertical-align: middle;"),
                      span("WUENIC", style = "margin-right: 12px; font-size: 12px;"),
                      span(style = "display: inline-block; width: 14px; height: 14px; background: #f39c12; margin-right: 3px; border-radius: 2px; vertical-align: middle;"),
                      span("UNWPP", style = "margin-right: 12px; font-size: 12px;"),
                      span(style = "display: inline-block; width: 14px; height: 14px; background: #1abc9c; margin-right: 3px; border-radius: 2px; vertical-align: middle;"),
                      span("WHO", style = "font-size: 12px;")
                  ),
                  div(
                      strong("New Fetched: "),
                      span(style = "display: inline-block; width: 14px; height: 14px; background: #e74c3c; margin-left: 10px; margin-right: 3px; border-radius: 2px; vertical-align: middle; border: 2px dashed #c0392b;"),
                      span("Red dashed lines", style = "font-size: 12px;")
                  )
              ),

              withSpinner(plotlyOutput("review_coverage_plot", height = "400px"), type = 6),

              hr(),

              fluidRow(
                column(6,
                       h5("Database Records"),
                       DT::dataTableOutput("review_db_table")
                ),
                column(6,
                       h5("New Fetched Records"),
                       DT::dataTableOutput("review_new_table")
                )
              )
            )
          )
  )
}

# ========================================
# MANUAL ENTRY TAB
# ========================================

create_manual_entry_tab <- function() {
  tabItem(tabName = "manual_entry",
          fluidRow(
            box(
              title = "Manual Survey Data Entry", status = "primary", solidHeader = TRUE, width = 12,
              div(class = "alert alert-info",
                  icon("keyboard"),
                  " Use this form to manually enter survey data. Indicator codes are enforced from the standard list. ",
                  "Entries follow the same workflow: cleaned_data -> validation -> duplicate check -> database.")
            )
          ),

          fluidRow(
            # Entry Form
            box(
              title = "Data Entry Form", status = "warning", solidHeader = TRUE, width = 6,

              h4("Geographic Information"),
              fluidRow(
                column(6,
                       selectInput("manual_country", "Country:",
                                  choices = NULL,
                                  selected = NULL)
                ),
                column(6,
                       selectInput("manual_region", "Region/Province:",
                                  choices = c("NATIONAL" = "NATIONAL"),
                                  selected = "NATIONAL")
                )
              ),

              h4("Temporal Information"),
              numericInput("manual_year", "Year:",
                          value = as.integer(format(Sys.Date(), "%Y")),
                          min = 1990, max = 2035, step = 1, width = "200px"),

              h4("Indicator Information"),
              fluidRow(
                column(6,
                       selectInput("manual_indicator_category", "Indicator Category:",
                                  choices = c("Select category..." = "",
                                             "ANC & Maternal Health" = "ANC & Maternal Health",
                                             "Immunization" = "Immunization",
                                             "Child Health & Nutrition" = "Child Health & Nutrition",
                                             "Mortality Rates" = "Mortality Rates",
                                             "Family Planning" = "Family Planning",
                                             "Malaria" = "Malaria",
                                             "HIV/AIDS" = "HIV/AIDS",
                                             "Population & Demographics" = "Population & Demographics"),
                                  selected = "")
                ),
                column(6,
                       selectInput("manual_indicator", "Indicator:",
                                  choices = c("Select category first..." = ""),
                                  selected = "")
                )
              ),
              uiOutput("manual_indicator_type_display"),

              h4("Value"),
              numericInput("manual_value", "Survey Value:",
                          value = NA, min = 0, step = 0.01, width = "200px"),
              uiOutput("manual_value_guidance"),

              h4("Source Information"),
              fluidRow(
                column(6,
                       selectInput("manual_source", "Source:",
                                  choices = c("DHS National" = "DHS National",
                                             "DHS Sub-national" = "DHS Sub-national",
                                             "MICS" = "MICS",
                                             "WUENIC" = "WUENIC",
                                             "UNWPP" = "UNWPP",
                                             "WHO" = "WHO",
                                             "Admin" = "Admin",
                                             "Other" = "Other"),
                                  selected = "Other")
                ),
                column(6,
                       selectInput("manual_survey_type", "Survey Type:",
                                  choices = c("household" = "household",
                                             "modeled" = "modeled",
                                             "admin" = "admin",
                                             "other" = "other"),
                                  selected = "household")
                )
              ),
              textInput("manual_source_detail", "Source Detail:",
                       placeholder = "e.g., DHS 2023, MICS 2022, Admin report Q3",
                       width = "100%"),

              hr(),

              fluidRow(
                column(6,
                       actionButton("add_manual_entry", "Add to Staging",
                                   class = "btn-success btn-block",
                                   icon = icon("plus"))
                ),
                column(6,
                       actionButton("clear_manual_form", "Clear Form",
                                   class = "btn-outline-secondary btn-block",
                                   icon = icon("eraser"))
                )
              ),

              br(),
              uiOutput("manual_entry_status")
            ),

            # Staging Area
            box(
              title = "Staging Area", status = "info", solidHeader = TRUE, width = 6,

              h4("Current Entry Preview"),
              verbatimTextOutput("manual_entry_preview"),

              hr(),

              h4("Staged Entries"),
              div(class = "alert alert-warning", style = "padding: 8px; font-size: 12px;",
                  icon("exclamation-triangle"),
                  " Staged entries are NOT yet in cleaned_data. Click 'Commit All' to add them."),

              DT::dataTableOutput("staged_entries_table"),

              br(),

              fluidRow(
                column(6,
                       actionButton("remove_staged_entry", "Remove Selected",
                                   class = "btn-warning btn-block",
                                   icon = icon("minus"))
                ),
                column(6,
                       actionButton("commit_staged_entries", "Commit All to Cleaned Data",
                                   class = "btn-success btn-block",
                                   icon = icon("check"))
                )
              ),

              br(),
              uiOutput("staged_entries_summary")
            )
          )
  )
}

