# ========================================
# FASTR Survey Data Fetcher - Main App
# ========================================
# Purpose: Modular Shiny app with external CSS and function files

# Load required libraries
library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)
library(rdhs)
library(rsdmx)
library(httr)
library(jsonlite)
library(countrycode)
library(data.table)
library(plotly)
library(shinyWidgets)
library(RCurl)
library(shinycssloaders)
library(shinyBS)
library(stringr)
library(shinyjs)

# Load environment variables
readRenviron("~/Desktop/modules/.Renviron")

# Source modular function files
source("R/data_functions.R")
source("R/cleaning_functions.R")

# ========================================
# UI DEFINITION
# ========================================

ui <- dashboardPage(
  dashboardHeader(
    title = HTML('<span style="font-weight: 600; letter-spacing: 0.5px;">
                <i class="fa fa-chart-line" style="margin-right: 8px; color: #66bb6a;"></i>
                FASTR Survey Data Fetcher
                </span>'),
    titleWidth = 350
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data Fetcher", tabName = "fetcher", icon = icon("download")),
      menuItem("Metadata Browser", tabName = "metadata", icon = icon("table")),
      menuItem("Results", tabName = "results", icon = icon("chart-line")),
      menuItem("Clean & Process", tabName = "processing", icon = icon("magic")),
      menuItem("Help & Info", tabName = "help", icon = icon("question-circle"))
    )
  ),
  
  dashboardBody(
    useShinyjs(),
    
    # External CSS file reference (clean!)
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    
    tabItems(
      tabItem(tabName = "fetcher",
              fluidRow(
                box(
                  title = "Select Data Source", status = "primary", solidHeader = TRUE, width = 12,
                  radioButtons("data_source", "Choose Data Source:",
                               choices = list(
                                 "DHS (Demographic & Health Surveys)" = "dhs",
                                 "MICS (Multiple Indicator Cluster Surveys)" = "mics",
                                 "UNWPP (UN World Population Prospects)" = "unwpp"
                               ),
                               selected = "dhs", inline = TRUE)
                )
              ),
              
              fluidRow(
                box(
                  title = "Indicator Selection", status = "info", solidHeader = TRUE, width = 6,
                  
                  conditionalPanel(
                    condition = "input.data_source == 'dhs'",
                    div(
                      h5(icon("star"), "Quick Select Favorites"),
                      div(class = "alert alert-info", style = "margin-bottom: 10px; padding: 8px;",
                          icon("info-circle"),
                          " ", strong("ANC1 Note:"), " We use RH_ANCP_W_SKP (ANC1 from skilled provider) as the standard ANC1 indicator."),
                      
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
                      ),
                      hr()
                    )
                  ),
                  
                  conditionalPanel(
                    condition = "input.data_source == 'mics'",
                    div(
                      h5(icon("star"), "Quick Select MICS Favorites"),
                      div(style = "margin-bottom: 15px;",
                          actionButton("select_mics_maternal", "Maternal Health",
                                       class = "btn-sm btn-outline-info", style = "margin: 2px;"),
                          actionButton("select_mics_vaccines", "Child Immunization",
                                       class = "btn-sm btn-outline-info", style = "margin: 2px;"),
                          actionButton("select_mics_mortality", "Child Mortality",
                                       class = "btn-sm btn-outline-info", style = "margin: 2px;")
                      ),
                      hr()
                    )
                  ),
                  
                  withSpinner(uiOutput("indicator_selector"), type = 6)
                ),
                
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
              ),
              
              fluidRow(
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
              )
      ),
      
      tabItem(tabName = "metadata",
              fluidRow(
                box(
                  title = "Indicator Metadata Browser", status = "primary", solidHeader = TRUE, width = 12,
                  p("Browse available indicators and their definitions."),
                  withSpinner(DT::dataTableOutput("metadata_table"), type = 6)
                )
              )
      ),
      
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
      ),
      
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
      ),
      
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
    )
  )
)

# ========================================
# SERVER LOGIC
# ========================================

server <- function(input, output, session) {
  
  values <- reactiveValues(
    metadata = data.frame(),
    countries = data.frame(),
    fetched_data = data.frame(),
    cleaned_data = data.frame()
  )
  
  observe({
    metadata <- switch(input$data_source,
                       "dhs" = fetch_dhs_metadata(),
                       "mics" = fetch_mics_metadata(),
                       "unwpp" = prepare_unwpp_metadata())
    values$metadata <- metadata
    
    countries <- switch(input$data_source,
                        "dhs" = fetch_dhs_countries(),
                        "mics" = fetch_mics_countries(),
                        "unwpp" = fetch_unwpp_countries())
    values$countries <- countries
  })
  
  output$indicator_selector <- renderUI({
    req(values$metadata)
    
    if(nrow(values$metadata) == 0) {
      return(div(class = "alert alert-warning",
                 icon("exclamation-triangle"),
                 " No indicators available. Please check your connection."))
    }
    
    choices <- setNames(values$metadata$IndicatorId, values$metadata$display_label)
    
    if("is_favorite" %in% names(values$metadata)) {
      favorite_ids <- values$metadata$IndicatorId[values$metadata$is_favorite]
      names(choices)[values$metadata$IndicatorId %in% favorite_ids] <-
        paste("[FAVORITE]", names(choices)[values$metadata$IndicatorId %in% favorite_ids])
    }
    
    tagList(
      div(id = "selection_counter", style = "margin-bottom: 10px;",
          textOutput("indicator_count")),
      
      pickerInput("indicators", "Select Indicators:",
                  choices = choices,
                  multiple = TRUE,
                  options = pickerOptions(
                    actionsBox = TRUE,
                    selectAllText = "Select All",
                    deselectAllText = "Deselect All",
                    noneSelectedText = "Choose indicators...",
                    selectedTextFormat = "count > 2",
                    liveSearch = TRUE,
                    size = 10,
                    showTick = TRUE,
                    style = "btn-outline-secondary"
                  ))
    )
  })
  
  output$indicator_count <- renderText({
    selected_count <- length(input$indicators %||% 0)
    total_count <- nrow(values$metadata)
    
    if(selected_count == 0) {
      "No indicators selected"
    } else {
      paste("Selected:", selected_count, "/", total_count, "indicators")
    }
  })
  
  output$country_selector <- renderUI({
    req(values$countries)
    
    if(nrow(values$countries) == 0) {
      return(div(class = "alert alert-warning",
                 icon("exclamation-triangle"),
                 " No countries available. Please check your connection."))
    }
    
    choices <- setNames(values$countries$country_code, values$countries$country_display)
    default_selection <- c("CI", "GH", "NG")
    default_selection <- default_selection[default_selection %in% values$countries$country_code]
    
    pickerInput("countries", "Select Countries:",
                choices = choices,
                selected = default_selection,
                multiple = TRUE,
                options = pickerOptions(
                  actionsBox = TRUE,
                  selectAllText = "Select All",
                  deselectAllText = "Deselect All",
                  liveSearch = TRUE,
                  size = 10
                ))
  })
  
  # Enhanced server-side observers with visual feedback
  observeEvent(input$select_maternal, {
    maternal_ids <- get_dhs_favorites()[["ANC and Maternal"]]
    current_selection <- input$indicators %||% character(0)
    
    if(all(maternal_ids %in% current_selection)) {
      new_selection <- setdiff(current_selection, maternal_ids)
    } else {
      new_selection <- union(current_selection, maternal_ids)
    }
    
    updatePickerInput(session, "indicators", selected = new_selection)
    
    shinyjs::runjs(sprintf("
      $('#maternal_count').text('%d');
      $('#select_maternal').toggleClass('btn-primary', %s).toggleClass('btn-outline-primary', %s);
    ", length(maternal_ids), 
                           all(maternal_ids %in% new_selection),
                           !all(maternal_ids %in% new_selection)))
  })
  
  observeEvent(input$select_vaccines, {
    vaccine_ids <- get_dhs_favorites()[["Child Vaccinations"]]
    current_selection <- input$indicators %||% character(0)
    
    if(all(vaccine_ids %in% current_selection)) {
      new_selection <- setdiff(current_selection, vaccine_ids)
    } else {
      new_selection <- union(current_selection, vaccine_ids)
    }
    
    updatePickerInput(session, "indicators", selected = new_selection)
    
    shinyjs::runjs(sprintf("
      $('#vaccine_count').text('%d');
      $('#select_vaccines').toggleClass('btn-primary', %s).toggleClass('btn-outline-primary', %s);
    ", length(vaccine_ids), 
                           all(vaccine_ids %in% new_selection),
                           !all(vaccine_ids %in% new_selection)))
  })
  
  observeEvent(input$select_iptp, {
    iptp_ids <- get_dhs_favorites()[["IPTp (Malaria Prevention)"]]
    current_selection <- input$indicators %||% character(0)
    
    if(all(iptp_ids %in% current_selection)) {
      new_selection <- setdiff(current_selection, iptp_ids)
    } else {
      new_selection <- union(current_selection, iptp_ids)
    }
    
    updatePickerInput(session, "indicators", selected = new_selection)
    
    shinyjs::runjs(sprintf("
      $('#iptp_count').text('%d');
      $('#select_iptp').toggleClass('btn-primary', %s).toggleClass('btn-outline-primary', %s);
    ", length(iptp_ids), 
                           all(iptp_ids %in% new_selection),
                           !all(iptp_ids %in% new_selection)))
  })
  
  observeEvent(input$select_mortality, {
    mortality_ids <- get_dhs_favorites()[["Mortality and Fertility"]]
    current_selection <- input$indicators %||% character(0)
    
    if(all(mortality_ids %in% current_selection)) {
      new_selection <- setdiff(current_selection, mortality_ids)
    } else {
      new_selection <- union(current_selection, mortality_ids)
    }
    
    updatePickerInput(session, "indicators", selected = new_selection)
    
    shinyjs::runjs(sprintf("
      $('#mortality_count').text('%d');
      $('#select_mortality').toggleClass('btn-primary', %s).toggleClass('btn-outline-primary', %s);
    ", length(mortality_ids), 
                           all(mortality_ids %in% new_selection),
                           !all(mortality_ids %in% new_selection)))
  })
  
  observeEvent(input$select_all_favorites, {
    all_favorites <- unlist(get_dhs_favorites(), use.names = FALSE)
    current_selection <- input$indicators %||% character(0)
    
    if(all(all_favorites %in% current_selection)) {
      new_selection <- setdiff(current_selection, all_favorites)
    } else {
      new_selection <- union(current_selection, all_favorites)
    }
    
    updatePickerInput(session, "indicators", selected = new_selection)
    
    shinyjs::runjs(sprintf("
      $('#all_count').text('%d');
      $('#select_all_favorites').toggleClass('btn-warning', %s).toggleClass('btn-outline-warning', %s);
    ", length(all_favorites), 
                           all(all_favorites %in% new_selection),
                           !all(all_favorites %in% new_selection)))
  })
  
  observeEvent(input$clear_selection, {
    updatePickerInput(session, "indicators", selected = character(0))
    
    shinyjs::runjs("
      $('.btn-primary').removeClass('btn-primary').addClass('btn-outline-primary');
      $('.btn-warning').removeClass('btn-warning').addClass('btn-outline-warning');
      $('.badge').text('0');
    ")
  })
  
  observeEvent(input$indicators, {
    req(values$metadata)
    current_selection <- input$indicators %||% character(0)
    
    favorites_list <- get_dhs_favorites()
    
    maternal_selected <- length(intersect(current_selection, favorites_list[["ANC and Maternal"]]))
    vaccine_selected <- length(intersect(current_selection, favorites_list[["Child Vaccinations"]]))
    iptp_selected <- length(intersect(current_selection, favorites_list[["IPTp (Malaria Prevention)"]]))
    mortality_selected <- length(intersect(current_selection, favorites_list[["Mortality and Fertility"]]))
    all_selected <- length(intersect(current_selection, unlist(favorites_list, use.names = FALSE)))
    
    shinyjs::runjs(sprintf("
      $('#maternal_count').text('%d');
      $('#vaccine_count').text('%d');
      $('#iptp_count').text('%d');
      $('#mortality_count').text('%d');
      $('#all_count').text('%d');
      
      $('#select_maternal').toggleClass('btn-primary', %s).toggleClass('btn-outline-primary', %s);
      $('#select_vaccines').toggleClass('btn-primary', %s).toggleClass('btn-outline-primary', %s);
      $('#select_iptp').toggleClass('btn-primary', %s).toggleClass('btn-outline-primary', %s);
      $('#select_mortality').toggleClass('btn-primary', %s).toggleClass('btn-outline-primary', %s);
      $('#select_all_favorites').toggleClass('btn-warning', %s).toggleClass('btn-outline-warning', %s);
    ", 
                           maternal_selected, vaccine_selected, iptp_selected, mortality_selected, all_selected,
                           maternal_selected == length(favorites_list[["ANC and Maternal"]]) && maternal_selected > 0,
                           !(maternal_selected == length(favorites_list[["ANC and Maternal"]]) && maternal_selected > 0),
                           vaccine_selected == length(favorites_list[["Child Vaccinations"]]) && vaccine_selected > 0,
                           !(vaccine_selected == length(favorites_list[["Child Vaccinations"]]) && vaccine_selected > 0),
                           iptp_selected == length(favorites_list[["IPTp (Malaria Prevention)"]]) && iptp_selected > 0,
                           !(iptp_selected == length(favorites_list[["IPTp (Malaria Prevention)"]]) && iptp_selected > 0),
                           mortality_selected == length(favorites_list[["Mortality and Fertility"]]) && mortality_selected > 0,
                           !(mortality_selected == length(favorites_list[["Mortality and Fertility"]]) && mortality_selected > 0),
                           all_selected == length(unlist(favorites_list, use.names = FALSE)) && all_selected > 0,
                           !(all_selected == length(unlist(favorites_list, use.names = FALSE)) && all_selected > 0)))
  }, ignoreNULL = FALSE, ignoreInit = TRUE)
  
  observeEvent(input$select_mics_maternal, {
    mics_maternal <- c("MNCH_ANC1", "MNCH_ANC4", "MNCH_INSTDEL", "MNCH_PNCMOM")
    updatePickerInput(session, "indicators", selected = mics_maternal)
  })
  
  observeEvent(input$select_mics_vaccines, {
    mics_vaccines <- c("IM_BCG", "IM_DTP1", "IM_DTP3")
    updatePickerInput(session, "indicators", selected = mics_vaccines)
  })
  
  observeEvent(input$select_mics_mortality, {
    mics_mortality <- c("CME_MRM0", "CME_MRY0T4")
    updatePickerInput(session, "indicators", selected = mics_mortality)
  })
  
  output$metadata_table <- DT::renderDataTable({
    req(values$metadata)
    
    if(nrow(values$metadata) == 0) {
      return(data.frame(Message = "No metadata available"))
    }
    
    display_data <- values$metadata %>%
      mutate(
        Favorite = ifelse(is_favorite %in% TRUE, "YES", ""),
        `Indicator ID` = IndicatorId,
        `Label` = Label,
        `Definition` = full_definition,
        Source = source
      ) %>%
      select(Favorite, `Indicator ID`, `Label`, `Definition`, Source)
    
    DT::datatable(
      display_data,
      options = list(scrollX = TRUE, pageLength = 25, processing = TRUE, server = TRUE),
      rownames = FALSE,
      filter = "top"
    )
  }, server = TRUE)
  
  observeEvent(input$fetch_data, {
    req(input$indicators, input$countries)
    
    if(length(input$indicators) == 0) {
      showNotification("Please select at least one indicator", type = "warning")
      return()
    }
    
    if(length(input$countries) == 0) {
      showNotification("Please select at least one country", type = "warning")
      return()
    }
    
    output$status_message <- renderUI({
      div(class = "alert alert-info",
          icon("spinner", class = "fa-spin"),
          " Fetching data... Please wait.")
    })
    
    tryCatch({
      if(input$data_source == "dhs") {
        data <- fetch_dhs_data(input$indicators, input$countries, input$breakdown)
      } else if(input$data_source == "mics") {
        data <- fetch_mics_data(input$indicators, input$countries)
      } else if(input$data_source == "unwpp") {
        data <- fetch_unwpp_data(input$indicators, input$countries, input$start_year, input$end_year)
      }
      
      values$fetched_data <- data
      
      if(nrow(data) > 0) {
        output$status_message <- renderUI({
          div(class = "alert alert-success",
              icon("check"),
              paste(" Successfully fetched", nrow(data), "records from", toupper(input$data_source), "!"))
        })
        
        showNotification(paste("Successfully fetched", nrow(data), "records!"), type = "message", duration = 5)
      } else {
        output$status_message <- renderUI({
          div(class = "alert alert-warning",
              icon("exclamation-triangle"),
              " No data returned. Try different selections.")
        })
      }
      
    }, error = function(e) {
      output$status_message <- renderUI({
        div(class = "alert alert-danger",
            icon("times"),
            paste(" Error:", e$message))
      })
      
      showNotification(paste("Error:", e$message), type = "error", duration = 10)
    })
  })
  
  output$results_table <- DT::renderDataTable({
    req(values$fetched_data)
    
    if(nrow(values$fetched_data) == 0) {
      return(data.frame(Message = "No data available. Please fetch data first."))
    }
    
    values$fetched_data
  }, options = list(scrollX = TRUE, pageLength = 10), rownames = FALSE)
  
  output$data_summary <- renderText({
    req(values$fetched_data)
    if(nrow(values$fetched_data) > 0) {
      paste(
        "Total Records:", nrow(values$fetched_data), "\n",
        "Columns:", ncol(values$fetched_data), "\n",
        "Data Source:", toupper(input$data_source), "\n",
        "Countries:", length(input$countries), "\n",
        "Indicators:", length(input$indicators)
      )
    } else {
      "No data available"
    }
  })
  
  output$download_csv <- downloadHandler(
    filename = function() {
      paste0("survey_data_", input$data_source, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(values$fetched_data, file, row.names = FALSE)
    }
  )
  
  output$download_rds <- downloadHandler(
    filename = function() {
      paste0("survey_data_", input$data_source, "_", Sys.Date(), ".rds")
    },
    content = function(file) {
      saveRDS(values$fetched_data, file)
    }
  )
  
  output$has_data <- reactive({
    nrow(values$fetched_data) > 0
  })
  outputOptions(output, "has_data", suspendWhenHidden = FALSE)
  
  observeEvent(input$clean_data, {
    req(values$fetched_data)
    
    if(nrow(values$fetched_data) == 0) {
      showNotification("No data to clean. Please fetch data first.", type = "warning")
      return()
    }
    
    output$cleaning_status <- renderUI({
      div(class = "alert alert-info",
          icon("spinner", class = "fa-spin"),
          " Cleaning data... Please wait.")
    })
    
    tryCatch({
      if(input$data_source == "dhs") {
        cleaned <- clean_dhs_data(values$fetched_data)
      } else if(input$data_source == "mics") {
        cleaned <- clean_mics_data(values$fetched_data, input$countries)
      } else {
        cleaned <- clean_unwpp_data(values$fetched_data)
      }
      
      values$cleaned_data <- cleaned
      
      if(nrow(cleaned) > 0) {
        output$cleaning_status <- renderUI({
          div(class = "alert alert-success",
              icon("check"),
              paste("Successfully cleaned", nrow(cleaned), "records!"))
        })
        
        showNotification(paste("Cleaned", nrow(cleaned), "records!"), type = "message")
      } else {
        output$cleaning_status <- renderUI({
          div(class = "alert alert-warning",
              icon("exclamation-triangle"),
              " No data remained after cleaning.")
        })
      }
      
    }, error = function(e) {
      output$cleaning_status <- renderUI({
        div(class = "alert alert-danger",
            icon("times"),
            paste("Cleaning error:", e$message))
      })
      
      showNotification(paste("Cleaning error:", e$message), type = "error")
    })
  })
  
  output$cleaned_data_table <- DT::renderDataTable({
    req(values$cleaned_data)
    
    if(nrow(values$cleaned_data) == 0) {
      return(data.frame(Message = "No cleaned data available. Please clean data first."))
    }
    
    values$cleaned_data
  }, options = list(scrollX = TRUE, pageLength = 10), rownames = FALSE)
  
  output$download_cleaned_csv <- downloadHandler(
    filename = function() {
      paste0("cleaned_survey_data_", input$data_source, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(values$cleaned_data, file, row.names = FALSE)
    }
  )
}

# ========================================
# RUN APP
# ========================================

shinyApp(ui = ui, server = server)
