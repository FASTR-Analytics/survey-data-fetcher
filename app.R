# ========================================
# FASTR Survey Data Fetcher - Ultra Clean Main App
# ========================================
# Purpose: Modular Shiny app with external CSS and modular UI components

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
library(readxl)  # For WUENIC MICS data parser

# Load environment variables
readRenviron(".Renviron")

# Source all modular files
source("R/data_functions.R")
source("R/cleaning_functions.R")
source("R/ui_components.R")

# ========================================
# UI DEFINITION (ULTRA CLEAN)
# ========================================

ui <- dashboardPage(
  skin = "black",
  create_app_header(),
  create_app_sidebar(),
  
  dashboardBody(
    useShinyjs(),
    
    # External CSS and JS file references (clean!)
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
      tags$script(src = "progress.js")
    ),
    
    # All tabs now come from ui_components.R
    tabItems(
      create_fetcher_tab(),
      create_metadata_tab(),
      create_results_tab(),
      create_processing_tab(),
      create_visualization_tab(), 
      create_help_tab()
    )
  )
)

# ========================================
# SERVER LOGIC (UNCHANGED)
# ========================================

server <- function(input, output, session) {
  
  values <- reactiveValues(
    metadata = data.frame(),
    countries = data.frame(),
    fetched_data = data.frame(),
    cleaned_data = data.frame()
  )

  # Initialize indicator lookup table for better plotting labels
  observe({
    if(!exists("indicator_lookup", envir = .GlobalEnv)) {
      tryCatch({
        message("Building indicator lookup table...")
        lookup <- get_all_indicators_lookup()
        assign("indicator_lookup", lookup, envir = .GlobalEnv)
        message("Indicator lookup table created with ", nrow(lookup), " indicators")
      }, error = function(e) {
        message("Could not build indicator lookup table: ", e$message)
      })
    }
  })
  
  observe({
    req(input$data_source)  # Add this line to require the input exists

    metadata <- switch(input$data_source,
                       "dhs" = fetch_dhs_metadata(),
                       "mics" = fetch_mics_metadata(),
                       "mics_wuenic" = data.frame(),  # No metadata - using direct checkbox selection
                       "unwpp" = fetch_unwpp_metadata())

    # Debug: Check metadata right after fetching
    message("Data source changed to: ", input$data_source)
    message("Metadata fetched - rows: ", nrow(metadata))

    values$metadata <- metadata
    
    countries <- switch(input$data_source,
                        "dhs" = fetch_dhs_countries(),
                        "mics" = fetch_mics_countries(),
                        "mics_wuenic" = fetch_mics_wuenic_countries(),  # Countries from WUENIC Excel file
                        "unwpp" = fetch_unwpp_countries())
    values$countries <- countries
  })
  
  output$indicator_selector <- renderUI({
    # Skip indicator selector for MICS WUENIC (uses checkboxes instead)
    if(input$data_source == "mics_wuenic") {
      return(NULL)
    }

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
    # Skip for MICS WUENIC (uses checkboxes instead)
    if(input$data_source == "mics_wuenic") {
      return("")
    }

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
  
  pickerInput("countries", "Select Countries:",
              choices = choices,
              selected = character(0),  # ← Empty selection
              multiple = TRUE,
              options = pickerOptions(
                actionsBox = TRUE,
                selectAllText = "Select All",
                deselectAllText = "Deselect All",
                liveSearch = TRUE,
                size = 10,
                noneSelectedText = "Choose countries..."
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

  # ========================================
  # MICS VACCINE CHECKBOX HANDLERS (for mics_wuenic)
  # ========================================

  # Select All Vaccines button
  observeEvent(input$select_all_vaccines, {
    all_vaccines <- c("CH_VACC_C_BCG", "CH_VACC_C_PT1", "CH_VACC_C_PT2", "CH_VACC_C_PT3",
                      "CH_VACS_C_OP1", "CH_VACC_C_OP2", "CH_VACC_C_OP3",
                      "CH_VACC_C_MSL", "CH_VACC_C_MS2", "CH_VACC_C_PC3",
                      "CH_VACC_C_RTC", "CH_VACC_C_HB3", "CH_VACC_C_HBB",
                      "CH_VACC_C_HI3", "CH_VACC_C_YF", "CH_VACC_C_FUL")
    updateCheckboxGroupInput(session, "mics_vaccines", selected = all_vaccines)
  })

  # Clear Vaccines button
  observeEvent(input$clear_vaccines, {
    updateCheckboxGroupInput(session, "mics_vaccines", selected = character(0))
  })

  # ========================================
  # UNWPP FAVORITE BUTTON HANDLERS
  # ========================================

  observeEvent(input$select_unwpp_health, {
    unwpp_health <- c("22", "24", "61", "62")  # IMR, U5MR, Life Expectancy, Adult Mortality
    current_selection <- input$indicators %||% character(0)

    if(all(unwpp_health %in% current_selection)) {
      new_selection <- setdiff(current_selection, unwpp_health)
    } else {
      new_selection <- union(current_selection, unwpp_health)
    }

    updatePickerInput(session, "indicators", selected = new_selection)
  })

  observeEvent(input$select_unwpp_demographics, {
    unwpp_demo <- c("49", "19", "67", "72")  # Total Pop, TFR, Median Age, Sex Ratio
    current_selection <- input$indicators %||% character(0)

    if(all(unwpp_demo %in% current_selection)) {
      new_selection <- setdiff(current_selection, unwpp_demo)
    } else {
      new_selection <- union(current_selection, unwpp_demo)
    }

    updatePickerInput(session, "indicators", selected = new_selection)
  })

  observeEvent(input$select_unwpp_social, {
    unwpp_social <- c("83", "84", "86")  # Child Dependency, Old Dependency, Total Dependency
    current_selection <- input$indicators %||% character(0)

    if(all(unwpp_social %in% current_selection)) {
      new_selection <- setdiff(current_selection, unwpp_social)
    } else {
      new_selection <- union(current_selection, unwpp_social)
    }

    updatePickerInput(session, "indicators", selected = new_selection)
  })

  observeEvent(input$select_unwpp_favorites, {
    unwpp_favorites <- c("2", "22", "24", "41", "46", "47", "49", "55")  # Current 8 favorites
    current_selection <- input$indicators %||% character(0)

    if(all(unwpp_favorites %in% current_selection)) {
      new_selection <- setdiff(current_selection, unwpp_favorites)
    } else {
      new_selection <- union(current_selection, unwpp_favorites)
    }

    updatePickerInput(session, "indicators", selected = new_selection)
  })

  observeEvent(input$clear_unwpp_selection, {
    updatePickerInput(session, "indicators", selected = character(0))
  })
  
  output$metadata_table <- DT::renderDataTable({
    req(values$metadata)

    # Debug: Check what's in metadata
    message("Metadata table rendering - rows: ", nrow(values$metadata))
    message("Data source: ", if("source" %in% names(values$metadata)) unique(values$metadata$source) else "unknown")

    if(nrow(values$metadata) == 0) {
      return(data.frame(Message = "No metadata available"))
    }
    
    # Create clean display with standardized column names
    display_data <- values$metadata %>%
      mutate(
        Favorite = ifelse(is_favorite %in% TRUE, "YES", ""),
        `Indicator ID` = IndicatorId,
        `Display Label` = if("display_label" %in% names(.)) display_label else NA,
        `Label` = Label,
        `Definition` = if("description" %in% names(.)) description else if("full_definition" %in% names(.)) full_definition else NA,
        Source = source
      )

    # Add sourceUrl if available (for UNWPP)
    if("sourceUrl" %in% names(values$metadata)) {
      display_data <- display_data %>% mutate(`Source URL` = sourceUrl)
    }

    # Add rich DHS metadata fields if available
    if("Category" %in% names(values$metadata)) {
      display_data <- display_data %>% mutate(Category = Category)
    }
    if("Subcategory" %in% names(values$metadata)) {
      display_data <- display_data %>% mutate(Subcategory = Subcategory)
    }
    if("Demographic Group" %in% names(values$metadata)) {
      display_data <- display_data %>% mutate(`Demographic Group` = `Demographic Group`)
    }
    if("Measurement Type" %in% names(values$metadata)) {
      display_data <- display_data %>% mutate(`Measurement Type` = `Measurement Type`)
    }
    if("Denominator" %in% names(values$metadata)) {
      display_data <- display_data %>% mutate(Denominator = Denominator)
    }

    # Build column list dynamically based on available fields
    key_cols <- c("Favorite", "Indicator ID", "Display Label", "Label", "Definition")

    # Add optional fields if they exist
    if("Category" %in% names(values$metadata)) key_cols <- c(key_cols, "Category")
    if("Subcategory" %in% names(values$metadata)) key_cols <- c(key_cols, "Subcategory")
    if("Demographic Group" %in% names(values$metadata)) key_cols <- c(key_cols, "Demographic Group")
    if("Measurement Type" %in% names(values$metadata)) key_cols <- c(key_cols, "Measurement Type")
    if("Denominator" %in% names(values$metadata)) key_cols <- c(key_cols, "Denominator")

    key_cols <- c(key_cols, "Source")
    if("sourceUrl" %in% names(values$metadata)) key_cols <- c(key_cols, "Source URL")

    display_data <- display_data %>% select(all_of(key_cols[key_cols %in% names(.)]))

    # Debug: Check final display data
    message("Final display_data rows: ", nrow(display_data))
    message("Final display_data columns: ", paste(names(display_data), collapse = ", "))

    DT::datatable(
      display_data,
      options = list(
        scrollX = TRUE,
        scrollY = "400px",
        pageLength = 25,
        processing = TRUE,
        server = TRUE,
        columnDefs = list(
          list(width = '80px', targets = which(names(display_data) == "Favorite") - 1),
          list(width = '120px', targets = which(names(display_data) == "Indicator ID") - 1),
          list(width = '180px', targets = which(names(display_data) == "Display Label") - 1),
          list(width = '200px', targets = which(names(display_data) == "Label") - 1),
          list(width = '300px', targets = which(names(display_data) == "Definition") - 1),
          list(width = '140px', targets = which(names(display_data) == "Category") - 1),
          list(width = '140px', targets = which(names(display_data) == "Subcategory") - 1),
          list(width = '140px', targets = which(names(display_data) == "Demographic Group") - 1),
          list(width = '120px', targets = which(names(display_data) == "Measurement Type") - 1),
          list(width = '150px', targets = which(names(display_data) == "Denominator") - 1),
          list(width = '100px', targets = which(names(display_data) == "Source") - 1),
          list(width = '200px', targets = which(names(display_data) == "Source URL") - 1),
          list(width = '120px', targets = '_all')  # Default for any additional columns
        ) %>% purrr::discard(~ length(.x$targets) == 0 || any(is.na(.x$targets))),
        autoWidth = FALSE,
        dom = 'Bfrtip'
      ),
      rownames = FALSE,
      filter = "top"
    )
  }, server = TRUE)
  
  observeEvent(input$fetch_data, {
    req(input$countries)

    # For MICS WUENIC, check mics_vaccines instead of indicators
    if(input$data_source == "mics_wuenic") {
      req(input$mics_vaccines)
      if(length(input$mics_vaccines) == 0) {
        showNotification("Please select at least one vaccine", type = "warning")
        return()
      }
    } else {
      req(input$indicators)
      if(length(input$indicators) == 0) {
        showNotification("Please select at least one indicator", type = "warning")
        return()
      }
    }

    if(length(input$countries) == 0) {
      showNotification("Please select at least one country", type = "warning")
      return()
    }

    # Initialize progress
    session$sendCustomMessage("updateProgress", list(percent = 10, text = "Initializing fetch..."))

    # Disable the fetch button
    session$sendCustomMessage("disableButton", "fetch_data")

    output$status_message <- renderUI({
      div(class = "alert alert-info",
          icon("spinner", class = "fa-spin"),
          " Fetching data... Please wait.")
    })

    # Add a small delay to show progress initialization
    Sys.sleep(0.5)
    session$sendCustomMessage("updateProgress", list(percent = 25, text = "Connecting to data source..."))

    tryCatch({
      if(input$data_source == "dhs") {
        session$sendCustomMessage("updateProgress", list(percent = 50, text = "Fetching DHS data..."))
        data <- fetch_dhs_data(input$indicators, input$countries, input$breakdown)
      } else if(input$data_source == "mics") {
        session$sendCustomMessage("updateProgress", list(percent = 50, text = "Fetching UNICEF SDMX data..."))
        data <- fetch_mics_data(input$indicators, input$countries)
      } else if(input$data_source == "mics_wuenic") {
        session$sendCustomMessage("updateProgress", list(percent = 50, text = "Fetching MICS data from WUENIC database..."))
        # Use mics_vaccines checkboxes instead of indicators picker
        selected_vaccines <- input$mics_vaccines %||% character(0)
        data <- fetch_wuenic_mics_data(
          indicators = selected_vaccines,
          countries = input$countries,
          evidence_type = "Record or Recall",
          source_filter = "MICS",
          use_latest_only = FALSE  # Get full time series
        )
      } else if(input$data_source == "unwpp") {
        session$sendCustomMessage("updateProgress", list(percent = 50, text = "Fetching UNWPP data..."))
        data <- fetch_unwpp_data(input$indicators, input$countries, input$start_year, input$end_year)
      }

      session$sendCustomMessage("updateProgress", list(percent = 85, text = "Processing retrieved data..."))
      values$fetched_data <- data
      
      if(nrow(data) > 0) {
        session$sendCustomMessage("updateProgress", list(percent = 100, text = paste("Successfully fetched", nrow(data), "records!")))

        output$status_message <- renderUI({
          div(class = "alert alert-success",
              icon("check"),
              paste(" Successfully fetched", nrow(data), "records from", toupper(input$data_source), "!"))
        })

        showNotification(paste("Successfully fetched", nrow(data), "records!"), type = "message", duration = 5)
      } else {
        session$sendCustomMessage("updateProgress", list(percent = 100, text = "No data returned"))

        output$status_message <- renderUI({
          div(class = "alert alert-warning",
              icon("exclamation-triangle"),
              " No data returned. Try different selections.")
        })
      }

      # Re-enable the fetch button
      session$sendCustomMessage("enableButton", "fetch_data")

    }, error = function(e) {
      session$sendCustomMessage("updateProgress", list(percent = 0, text = "Fetch failed"))

      output$status_message <- renderUI({
        div(class = "alert alert-danger",
            icon("times"),
            paste(" Error:", e$message))
      })

      # Re-enable the fetch button
      session$sendCustomMessage("enableButton", "fetch_data")

      showNotification(paste("Error:", e$message), type = "error", duration = 10)
    })
  })
  
  output$results_table <- DT::renderDataTable({
    req(values$fetched_data)

    if(nrow(values$fetched_data) == 0) {
      return(data.frame(Message = "No data available. Please fetch data first."))
    }

    values$fetched_data
  }, options = list(
    scrollX = TRUE,
    scrollY = "400px",
    pageLength = 10,
    autoWidth = FALSE,
    columnDefs = list(
      list(width = '80px', targets = '_all')  # Set minimum width for all columns
    ),
    dom = 'Bfrtip'
  ), rownames = FALSE)
  
  output$data_summary <- renderText({
    req(values$fetched_data)
    if(nrow(values$fetched_data) > 0) {
      # Get proper data source label
      source_label <- switch(input$data_source,
                            "dhs" = "DHS",
                            "mics" = "UNICEF SDMX API",
                            "mics_wuenic" = "WUENIC",
                            "unwpp" = "UNWPP",
                            toupper(input$data_source))

      # Get indicator count (different for MICS WUENIC which uses checkboxes)
      indicator_count <- if(input$data_source == "mics_wuenic") {
        length(input$mics_vaccines %||% character(0))
      } else {
        length(input$indicators %||% character(0))
      }

      paste(
        "Total Records:", nrow(values$fetched_data), "\n",
        "Columns:", ncol(values$fetched_data), "\n",
        "Data Source:", source_label, "\n",
        "Countries:", length(input$countries), "\n",
        "Indicators:", indicator_count
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
  
  # ========================================
  # CLEANING CONFIGURATION INTERFACE
  # ========================================
  
  # ========================================
  # DATA CLEANING - SIMPLIFIED DEFAULT ONLY  
  # ========================================
  
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
      # Use the simplified dispatcher with FASTR standardization option
      cleaned <- clean_survey_data(values$fetched_data, input$data_source,
                                   selected_countries = NULL,
                                   apply_fastr_standardization = input$apply_fastr_standardization)
      
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
  }, options = list(
    scrollX = TRUE,
    scrollY = "400px",
    pageLength = 10,
    autoWidth = FALSE,
    columnDefs = list(
      list(width = '120px', targets = c(0, 1)),  # admin_area columns
      list(width = '80px', targets = c(2, 5, 6)), # year, indicator_type, survey_value
      list(width = '150px', targets = c(3, 4)),  # indicator_id, indicator_common_id
      list(width = '100px', targets = c(7, 8, 9)), # source, source_detail, survey_type
      list(width = '120px', targets = c(10, 11, 12)) # country_name, iso2_code, iso3_code
    ),
    dom = 'Bfrtip'
  ), rownames = FALSE)
  
  output$download_cleaned_csv <- downloadHandler(
    filename = function() {
      paste0("cleaned_survey_data_", input$data_source, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(values$cleaned_data, file, row.names = FALSE)
    }
  )
  
  # Update plot selectors when cleaned data changes
  observe({
    req(values$cleaned_data)
    
    if(nrow(values$cleaned_data) > 0) {
      # Get unique indicators with readable names
      indicator_choices <- values$cleaned_data %>%
        select(indicator_id, indicator_common_id) %>%
        distinct() %>%
        {setNames(.$indicator_id, .$indicator_common_id)}
      
      # Create geographic area choices that handle both national and subnational data
      geo_areas <- values$cleaned_data %>%
        mutate(
          # Create display name for geographic areas using country_name for better readability
          geo_display = if_else(
            admin_area_2 == "NATIONAL" | is.na(admin_area_2),
            country_name,  # Use country_name instead of admin_area_1
            paste(country_name, "-", admin_area_2)  # Use country_name for subnational too
          ),
          # Create unique identifier for filtering
          geo_id = if_else(
            admin_area_2 == "NATIONAL" | is.na(admin_area_2),
            admin_area_1,
            paste(admin_area_1, "||", admin_area_2)
          )
        ) %>%
        select(geo_id, geo_display) %>%
        distinct() %>%
        arrange(geo_display)
      
      geo_choices <- setNames(geo_areas$geo_id, geo_areas$geo_display)
      
      # Update indicator selector with readable names
      updateSelectInput(session, "plot_indicator",
                        choices = indicator_choices)
      
      # Update country selectors with geographic areas
      updateSelectInput(session, "plot_countries",
                        choices = geo_choices,
                        selected = geo_areas$geo_id[1:min(3, length(geo_areas$geo_id))])
      
      updateSelectInput(session, "comparison_country",
                        choices = geo_choices)
      
      # Update comparison indicators with readable names too
      updateSelectInput(session, "comparison_indicators",
                        choices = indicator_choices,
                        selected = names(indicator_choices)[1:min(3, length(indicator_choices))])
    }
  })
  
  # Check if cleaned data exists
  output$has_cleaned_data <- reactive({
    nrow(values$cleaned_data) > 0
  })
  outputOptions(output, "has_cleaned_data", suspendWhenHidden = FALSE)
  
  # Create filtering function that handles both national and subnational data
  filter_data <- function(data, selected_geos) {
    filtered_data <- data.frame()
    
    # Debug: Print available data structure
    cat("=== DEBUG: Available data structure ===\n")
    cat("Unique admin_area_1 values:", paste(unique(data$admin_area_1), collapse = ", "), "\n")
    cat("Unique admin_area_2 values:", paste(unique(data$admin_area_2), collapse = ", "), "\n")
    cat("Selected geo_ids:", paste(selected_geos, collapse = ", "), "\n")
    
    for(geo_id in selected_geos) {
      cat("\n--- Processing geo_id:", geo_id, "---\n")
      
      if(grepl("\\|\\|", geo_id)) {
        # Subnational data - split geo_id to get admin_area_1 and admin_area_2
        parts <- strsplit(geo_id, "\\|\\|")[[1]]
        admin1 <- parts[1]
        admin2 <- parts[2]
        cat("Looking for admin1:", admin1, "admin2:", admin2, "\n")
        
        # Check if this combination exists
        matching_rows <- data %>%
          filter(.data$admin_area_1 == admin1 & .data$admin_area_2 == admin2)
        cat("Found", nrow(matching_rows), "matching rows\n")
        
        # Also try trimmed comparison in case of whitespace issues
        if(nrow(matching_rows) == 0) {
          matching_rows <- data %>%
            filter(str_trim(.data$admin_area_1) == str_trim(admin1) & 
                   str_trim(.data$admin_area_2) == str_trim(admin2))
          cat("Found", nrow(matching_rows), "matching rows after trimming\n")
        }
        
        subset_data <- matching_rows
      } else {
        # National data - just match admin_area_1
        cat("Looking for national data for:", geo_id, "\n")
        subset_data <- data %>%
          filter(.data$admin_area_1 == geo_id & (.data$admin_area_2 == "NATIONAL" | is.na(.data$admin_area_2)))
        cat("Found", nrow(subset_data), "national rows\n")
      }
      filtered_data <- bind_rows(filtered_data, subset_data)
    }
    
    cat("=== Final filtered data has", nrow(filtered_data), "rows ===\n")
    return(filtered_data)
  }
  
  # Helper function to shorten long indicator names for better plot display
  shorten_indicator_name <- function(name, max_chars = 50) {
    if(is.null(name) || is.na(name) || nchar(name) <= max_chars) {
      return(name)
    }

    # Try to break at natural points (commas, dashes, parentheses)
    if(grepl("[,-]", name)) {
      parts <- strsplit(name, "[,-]")[[1]]
      shortened <- trimws(parts[1])
      if(nchar(shortened) <= max_chars) {
        return(shortened)
      }
    }

    # If still too long, truncate and add ellipsis
    paste0(substr(name, 1, max_chars - 3), "...")
  }

  # Generate time series plot
  observeEvent(input$generate_plot, {
    req(input$plot_indicator, input$plot_countries, values$cleaned_data)
    
    # First filter by indicator, then by geography
    indicator_data <- values$cleaned_data %>%
      filter(.data$indicator_id == input$plot_indicator)
    
    plot_data <- filter_data(indicator_data, input$plot_countries) %>%
      mutate(
        # Create display name for legend using country_name
        geo_label = if_else(
          admin_area_2 == "NATIONAL" | is.na(admin_area_2),
          country_name,  # Use country_name instead of admin_area_1
          paste(country_name, "-", admin_area_2)  # Use country_name for subnational too
        )
      ) %>%
      arrange(.data$year)
    
    if(nrow(plot_data) == 0) {
      output$time_series_plot <- renderPlotly({
        plotly::plot_ly() %>%
          add_text(x = 0.5, y = 0.5, text = "No data available for selected filters",
                   showlegend = FALSE) %>%
          layout(xaxis = list(visible = FALSE), yaxis = list(visible = FALSE))
      })
      return()
    }
    
    # Hide placeholder and show plot
    shinyjs::hide("plot-placeholder")
    
    output$time_series_plot <- renderPlotly({
      # Use lines+markers for optimal time series visualization
      plot_mode <- "lines+markers"

      # Define FASTR theme colors
      fastr_colors <- c("#0f706d", "#1a8b86", "#2c3e50", "#7f8c8d", "#e74c3c", "#f39c12", "#3498db", "#9b59b6", "#2ecc71", "#e67e22")

      # Enhance plot data with better labels from lookup table
      enhanced_plot_data <- plot_data %>%
        mutate(
          indicator_display_label = sapply(indicator_id, function(id) {
            tryCatch({
              label <- get_indicator_label(id)
              if(label == id && "indicator_common_id" %in% names(plot_data) &&
                 !is.na(indicator_common_id) && indicator_common_id != "") {
                return(indicator_common_id)
              }
              return(label)
            }, error = function(e) {
              # Fallback to original ID if lookup fails
              return(id)
            })
          }),
          # Create shortened version for legend
          indicator_short_label = sapply(indicator_display_label, function(name) {
            shorten_indicator_name(name, max_chars = 40)
          })
        )

      # Check if indicators are percentage types for y-axis formatting
      is_percentage <- any(enhanced_plot_data$indicator_type == "percent", na.rm = TRUE)
      y_axis_config <- if(is_percentage) {
        list(
          title = list(text = "Value (%)", font = list(color = "#2c3e50")),
          tickformat = ".1%",
          gridcolor = "#dee2e6",
          linecolor = "#dee2e6"
        )
      } else {
        list(
          title = list(text = "Value", font = list(color = "#2c3e50")),
          gridcolor = "#dee2e6",
          linecolor = "#dee2e6"
        )
      }

      p <- plot_ly(enhanced_plot_data, x = ~year, y = ~survey_value, color = ~geo_label,
                   type = "scatter",
                   mode = plot_mode,
                   line = list(width = 3),
                   marker = list(size = 8),
                   colors = fastr_colors) %>%
        layout(
          title = list(
            text = shorten_indicator_name(unique(enhanced_plot_data$indicator_display_label)[1], max_chars = 60),
            font = list(color = "#2c3e50", size = 16)
          ),
          xaxis = list(
            title = list(text = "Year", font = list(color = "#2c3e50")),
            gridcolor = "#dee2e6",
            linecolor = "#dee2e6"
          ),
          yaxis = y_axis_config,
          plot_bgcolor = "#ffffff",
          paper_bgcolor = "#ffffff",
          font = list(color = "#2c3e50"),
          hovermode = "x unified"
        )
      
      # Add trend lines if requested
      if(input$show_trend) {
        for(geo_label in unique(plot_data$geo_label)) {
          geo_data <- plot_data %>% filter(.data$geo_label == geo_label)
          if(nrow(geo_data) > 1) {
            trend_model <- lm(survey_value ~ year, data = geo_data)
            trend_line <- data.frame(
              year = range(geo_data$year),
              survey_value = predict(trend_model, newdata = data.frame(year = range(geo_data$year)))
            )
            
            p <- p %>% add_lines(
              data = trend_line,
              x = ~year, y = ~survey_value,
              name = paste(geo_label, "trend"),
              line = list(dash = "dash", width = 2),
              showlegend = FALSE
            )
          }
        }
      }
      
      p
    })
  })
  
  # Generate comparison plot
  observeEvent(input$generate_comparison, {
    req(input$comparison_country, input$comparison_indicators, values$cleaned_data)
    
    # Filter data using the same logic as the time series plot
    indicators_data <- values$cleaned_data %>%
      filter(.data$indicator_id %in% input$comparison_indicators)
    
    plot_data <- filter_data(indicators_data, list(input$comparison_country)) %>%
      arrange(.data$year)

    # Enhance plot data with better labels from lookup table
    if(nrow(plot_data) > 0) {
      plot_data <- plot_data %>%
        mutate(
          indicator_display_label = sapply(indicator_id, function(id) {
            tryCatch({
              label <- get_indicator_label(id)
              if(label == id && "indicator_common_id" %in% names(plot_data) &&
                 !is.na(indicator_common_id) && indicator_common_id != "") {
                return(indicator_common_id)
              }
              return(label)
            }, error = function(e) {
              # Fallback to original ID if lookup fails
              return(id)
            })
          }),
          # Create shortened version for legend (shorter for comparison plots)
          indicator_short_label = sapply(indicator_display_label, function(name) {
            shorten_indicator_name(name, max_chars = 30)
          })
        )
    }

    if(nrow(plot_data) == 0) {
      output$comparison_plot <- renderPlotly({
        plotly::plot_ly() %>%
          add_text(x = 0.5, y = 0.5, text = "No data available for selected filters",
                   showlegend = FALSE) %>%
          layout(xaxis = list(visible = FALSE), yaxis = list(visible = FALSE))
      })
      return()
    }
    
    # Hide placeholder and show comparison plot  
    shinyjs::hide("comparison-placeholder")
    
    output$comparison_plot <- renderPlotly({
      if(input$comparison_scale == "free") {
        # Use subplots for free scale
        plot_list <- list()
        for(i in seq_along(input$comparison_indicators)) {
          indicator_data <- plot_data %>% filter(.data$indicator_id == input$comparison_indicators[i])
          indicator_display <- unique(indicator_data$indicator_short_label)[1]

          # Define FASTR theme colors
          fastr_colors <- c("#0f706d", "#1a8b86", "#2c3e50", "#7f8c8d", "#e74c3c", "#f39c12", "#3498db", "#9b59b6", "#2ecc71", "#e67e22")

          # Check if this indicator is a percentage type for y-axis formatting
          is_percentage <- any(indicator_data$indicator_type == "percent", na.rm = TRUE)
          y_axis_config <- if(is_percentage) {
            list(
              title = list(text = "Value (%)", font = list(color = "#2c3e50")),
              tickformat = ".1%",
              gridcolor = "#dee2e6",
              linecolor = "#dee2e6"
            )
          } else {
            list(
              title = list(text = "Value", font = list(color = "#2c3e50")),
              gridcolor = "#dee2e6",
              linecolor = "#dee2e6"
            )
          }

          p <- plot_ly(indicator_data, x = ~year, y = ~survey_value,
                       type = "scatter", mode = "lines+markers",
                       name = if(!is.na(indicator_display)) indicator_display else input$comparison_indicators[i],
                       line = list(width = 3, color = fastr_colors[((i-1) %% length(fastr_colors)) + 1]),
                       marker = list(size = 8, color = fastr_colors[((i-1) %% length(fastr_colors)) + 1])) %>%
            layout(
              title = list(
                text = if(!is.na(indicator_display)) indicator_display else input$comparison_indicators[i],
                font = list(color = "#2c3e50", size = 14)
              ),
              xaxis = list(
                title = list(
                  text = if(i == length(input$comparison_indicators)) "Year" else "",
                  font = list(color = "#2c3e50")
                ),
                gridcolor = "#dee2e6",
                linecolor = "#dee2e6"
              ),
              yaxis = y_axis_config,
              plot_bgcolor = "#ffffff",
              paper_bgcolor = "#ffffff",
              font = list(color = "#2c3e50")
            )
          plot_list[[i]] <- p
        }
        
        subplot(plot_list, nrows = length(input$comparison_indicators), 
                shareX = TRUE, titleY = TRUE)
        
      } else {
        # Fixed scale - single plot with indicator_common_id in legend
        # Define FASTR theme colors
        fastr_colors <- c("#0f706d", "#1a8b86", "#2c3e50", "#7f8c8d", "#e74c3c", "#f39c12", "#3498db", "#9b59b6", "#2ecc71", "#e67e22")

        # Check if indicators are percentage types for y-axis formatting
        is_percentage <- any(plot_data$indicator_type == "percent", na.rm = TRUE)
        y_axis_config <- if(is_percentage) {
          list(
            title = list(text = "Value (%)", font = list(color = "#2c3e50")),
            tickformat = ".1%",
            gridcolor = "#dee2e6",
            linecolor = "#dee2e6"
          )
        } else {
          list(
            title = list(text = "Value", font = list(color = "#2c3e50")),
            gridcolor = "#dee2e6",
            linecolor = "#dee2e6"
          )
        }

        plot_ly(plot_data, x = ~year, y = ~survey_value, color = ~indicator_short_label,
                type = "scatter", mode = "lines+markers",
                line = list(width = 3), marker = list(size = 8),
                colors = fastr_colors) %>%
          layout(
            title = list(
              text = paste("Multi-Indicator Comparison:", unique(plot_data$country_name)[1]),
              font = list(color = "#2c3e50", size = 16)
            ),
            xaxis = list(
              title = list(text = "Year", font = list(color = "#2c3e50")),
              gridcolor = "#dee2e6",
              linecolor = "#dee2e6"
            ),
            yaxis = y_axis_config,
            plot_bgcolor = "#ffffff",
            paper_bgcolor = "#ffffff",
            font = list(color = "#2c3e50"),
            hovermode = "x unified",
            legend = list(
              font = list(color = "#2c3e50")
            )
          )
      }
    })
  })
}

# ========================================
# RUN APP
# ========================================

shinyApp(ui = ui, server = server)
