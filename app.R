# ========================================
# FASTR Survey Data Fetcher - Ultra Clean Main App
# ========================================
# Purpose: Modular Shiny app with external CSS and modular UI components

# Load required libraries
library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)
library(rlang)  # For %||% operator
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
library(readxl)  # For WUENIC data parser

# Load environment variables
readRenviron(".Renviron")

# Source all modular files
source("R/indicator_mappings.R")
source("R/data_functions.R")
source("R/cleaning_functions.R")
source("R/ui_components.R")
source("R/integration_functions.R")

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
      create_manual_entry_tab(),
      create_database_explorer_tab(),
      create_data_review_tab(),
      create_integration_tab(),
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
    # Legacy single dataset storage (for backward compatibility)
    fetched_data = data.frame(),
    cleaned_data = data.frame(),
    # NEW: Collection-based storage for multi-fetch sessions
    fetch_collection = list(),      # List of fetched datasets
    cleaned_collection = list(),    # List of cleaned datasets
    next_dataset_id = 1,            # Auto-increment ID for datasets
    # Database integration
    survey_db = NULL,               # Existing survey database
    pop_db = NULL,                  # Existing population database
    validation_result = NULL,       # Name validation results
    validated_data = NULL,          # Data after name corrections
    duplicate_analysis = NULL,      # Duplicate detection results
    # Manual entry
    staged_manual_entries = data.frame(),  # Staging area for manual entries
    available_countries = NULL,            # Countries from backbone files
    country_regions = list()               # Regions per country from backbone
  )

  # Initialize indicator lookup table lazily (built on first use, not at startup)
  # This avoids slow API calls blocking app startup on HF Spaces
  
  observe({
    req(input$data_source)  # Add this line to require the input exists

    metadata <- switch(input$data_source,
                       "dhs" = fetch_dhs_metadata(),
                       "unicef" = fetch_unicef_metadata(),
                       "unwpp" = fetch_unwpp_metadata())

    # Debug: Check metadata right after fetching
    message("Data source changed to: ", input$data_source)
    message("Metadata fetched - rows: ", nrow(metadata))

    values$metadata <- metadata
    
    countries <- switch(input$data_source,
                        "dhs" = fetch_dhs_countries(),
                        "unicef" = fetch_unicef_countries(),
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

    # Determine which indicators to show based on mode
    indicator_mode <- input$indicator_mode %||% "favorites"

    if(indicator_mode == "favorites" && "is_favorite" %in% names(values$metadata)) {
      # Filter to show only favorites
      filtered_metadata <- values$metadata %>% filter(is_favorite == TRUE)

      if(nrow(filtered_metadata) == 0) {
        # No favorites defined, show all with warning
        filtered_metadata <- values$metadata
        message("No favorites defined for this data source, showing all indicators")
      }
    } else {
      # Browse mode - show all indicators
      filtered_metadata <- values$metadata
    }

    choices <- setNames(filtered_metadata$IndicatorId, filtered_metadata$display_label)

    # Mark favorites in browse mode
    if(indicator_mode == "browse" && "is_favorite" %in% names(filtered_metadata)) {
      favorite_ids <- filtered_metadata$IndicatorId[filtered_metadata$is_favorite %in% TRUE]
      names(choices)[filtered_metadata$IndicatorId %in% favorite_ids] <-
        paste("[⭐]", names(choices)[filtered_metadata$IndicatorId %in% favorite_ids])
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
    req(values$metadata)
    selected_count <- length(input$indicators %||% 0)

    # Calculate total based on current mode
    indicator_mode <- input$indicator_mode %||% "favorites"

    if(indicator_mode == "favorites" && "is_favorite" %in% names(values$metadata)) {
      total_count <- sum(values$metadata$is_favorite == TRUE, na.rm = TRUE)
      if(total_count == 0) total_count <- nrow(values$metadata)  # Fallback if no favorites
    } else {
      total_count <- nrow(values$metadata)
    }

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
  
  observeEvent(input$select_unicef_maternal, {
    unicef_maternal <- c("MNCH_ANC1", "MNCH_ANC4", "MNCH_INSTDEL", "MNCH_PNCMOM")
    updatePickerInput(session, "indicators", selected = unicef_maternal)
  })

  observeEvent(input$select_unicef_vaccines, {
    unicef_vaccines <- c("IM_BCG", "IM_DTP1", "IM_DTP3")
    updatePickerInput(session, "indicators", selected = unicef_vaccines)
  })

  observeEvent(input$select_unicef_mortality, {
    unicef_mortality <- c("CME_MRM0", "CME_MRY0T4")
    updatePickerInput(session, "indicators", selected = unicef_mortality)
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
    req(input$countries, input$indicators)

    if(length(input$indicators) == 0) {
      showNotification("Please select at least one indicator", type = "warning")
      return()
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
      } else if(input$data_source == "unicef") {
        session$sendCustomMessage("updateProgress", list(percent = 50, text = "Fetching UNICEF SDMX data..."))
        data <- fetch_unicef_data(input$indicators, input$countries, input$start_year, input$end_year)
      } else if(input$data_source == "unwpp") {
        session$sendCustomMessage("updateProgress", list(percent = 50, text = "Fetching UNWPP data..."))
        data <- fetch_unwpp_data(input$indicators, input$countries, input$start_year, input$end_year)
      }

      session$sendCustomMessage("updateProgress", list(percent = 85, text = "Processing retrieved data..."))

      # Always update legacy storage for backward compatibility
      values$fetched_data <- data

      # Add to cart if requested
      if(!is.null(input$add_to_cart) && input$add_to_cart == TRUE && nrow(data) > 0) {
        # Generate dataset label
        dataset_label <- generate_dataset_label(
          source = input$data_source,
          indicators = input$indicators,
          countries = input$countries
        )

        # Add to collection
        values$fetch_collection[[as.character(values$next_dataset_id)]] <- list(
          id = values$next_dataset_id,
          label = dataset_label,
          source = input$data_source,
          timestamp = Sys.time(),
          indicators = input$indicators,
          countries = input$countries,
          n_records = nrow(data),
          data = data
        )

        values$next_dataset_id <- values$next_dataset_id + 1

        message("Added dataset to cart: ", dataset_label, " (ID: ", values$next_dataset_id - 1, ")")
      }

      if(nrow(data) > 0) {
        session$sendCustomMessage("updateProgress", list(percent = 100, text = paste("Successfully fetched", nrow(data), "records!")))

        # Update status message to show cart status if applicable
        cart_message <- if(!is.null(input$add_to_cart) && input$add_to_cart == TRUE) {
          paste0(" Added to cart (", length(values$fetch_collection), " datasets total).")
        } else {
          ""
        }

        output$status_message <- renderUI({
          div(class = "alert alert-success",
              icon("check"),
              paste0(" Successfully fetched ", nrow(data), " records from ", toupper(input$data_source), "!", cart_message))
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
                            "unicef" = "UNICEF SDMX API",
                            "unwpp" = "UNWPP",
                            toupper(input$data_source))

      paste(
        "Total Records:", nrow(values$fetched_data), "\n",
        "Columns:", ncol(values$fetched_data), "\n",
        "Data Source:", source_label, "\n",
        "Countries:", length(input$countries), "\n",
        "Indicators:", length(input$indicators %||% character(0))
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
  # CART MANAGEMENT
  # ========================================

  # Render cart table
  output$cart_table <- DT::renderDataTable({
    if(length(values$fetch_collection) == 0) {
      return(data.frame(Message = "Cart is empty. Add data by checking 'Add to cart' before fetching."))
    }

    # Convert collection to data frame
    cart_df <- data.frame(
      ID = sapply(values$fetch_collection, function(x) x$id),
      Label = sapply(values$fetch_collection, function(x) x$label),
      Source = sapply(values$fetch_collection, function(x) toupper(x$source)),
      Records = sapply(values$fetch_collection, function(x) x$n_records),
      Timestamp = sapply(values$fetch_collection, function(x) {
        format(x$timestamp, "%Y-%m-%d %H:%M")
      }),
      stringsAsFactors = FALSE
    )

    DT::datatable(
      cart_df,
      selection = "multiple",
      options = list(
        pageLength = 10,
        dom = 't',
        ordering = TRUE
      ),
      rownames = FALSE
    )
  })

  # Cart summary
  output$cart_summary <- renderUI({
    total_datasets <- length(values$fetch_collection)
    total_records <- sum(sapply(values$fetch_collection, function(x) x$n_records))

    HTML(paste0(
      "<strong>", total_datasets, "</strong> dataset", if(total_datasets != 1) "s" else "", "<br>",
      "<strong>", format(total_records, big.mark = ","), "</strong> total records"
    ))
  })

  # Remove selected from cart
  observeEvent(input$remove_selected_from_cart, {
    selected_rows <- input$cart_table_rows_selected

    if(is.null(selected_rows) || length(selected_rows) == 0) {
      showNotification("Please select datasets to remove", type = "warning")
      return()
    }

    # Get IDs of datasets in the table (sorted by how they appear)
    cart_ids <- names(values$fetch_collection)
    ids_to_remove <- cart_ids[selected_rows]

    # Remove from collection
    values$fetch_collection[ids_to_remove] <- NULL

    showNotification(paste("Removed", length(ids_to_remove), "dataset(s) from cart"),
                    type = "message")
  })

  # Clear all cart
  observeEvent(input$clear_cart, {
    if(length(values$fetch_collection) == 0) {
      showNotification("Cart is already empty", type = "warning")
      return()
    }

    values$fetch_collection <- list()
    values$next_dataset_id <- 1
    showNotification("Cart cleared", type = "message")
  })

  # Download all cart data as CSV
  output$download_cart_csv <- downloadHandler(
    filename = function() {
      paste0("cart_all_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      if(length(values$fetch_collection) == 0) {
        # Write empty file with message
        write.csv(data.frame(Message = "Cart is empty"), file, row.names = FALSE)
        return()
      }

      # Combine all datasets from cart
      all_data <- lapply(values$fetch_collection, function(dataset) {
        dataset$data %>%
          mutate(
            cart_dataset_id = dataset$id,
            cart_dataset_label = dataset$label,
            cart_dataset_source = toupper(dataset$source),
            cart_timestamp = format(dataset$timestamp, "%Y-%m-%d %H:%M:%S")
          )
      })

      combined_data <- bind_rows(all_data)
      write.csv(combined_data, file, row.names = FALSE)
    }
  )

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

  # Clean all cart data
  observeEvent(input$clean_cart_data, {
    if(length(values$fetch_collection) == 0) {
      showNotification("Cart is empty. Please add data to cart first.", type = "warning")
      return()
    }

    output$cart_cleaning_status <- renderUI({
      div(class = "alert alert-info",
          icon("spinner", class = "fa-spin"),
          paste(" Cleaning", length(values$fetch_collection), "dataset(s) from cart... Please wait."))
    })

    tryCatch({
      # Clean each dataset in the cart
      cleaned_datasets <- lapply(values$fetch_collection, function(dataset) {
        clean_survey_data(dataset$data, dataset$source,
                         selected_countries = NULL,
                         apply_fastr_standardization = input$apply_fastr_standardization)
      })

      # Filter out empty cleaned datasets
      non_empty <- cleaned_datasets[sapply(cleaned_datasets, nrow) > 0]

      if(length(non_empty) == 0) {
        output$cart_cleaning_status <- renderUI({
          div(class = "alert alert-warning",
              icon("exclamation-triangle"),
              " No data remained after cleaning all cart datasets.")
        })
        return()
      }

      # Combine all cleaned datasets
      combined_cleaned <- bind_rows(non_empty)

      # Store in cleaned_data for visualization
      values$cleaned_data <- combined_cleaned

      # Report summary
      total_raw_records <- sum(sapply(values$fetch_collection, function(x) x$n_records))
      national_count <- sum(combined_cleaned$admin_area_2 == "NATIONAL", na.rm = TRUE)
      subnational_count <- sum(combined_cleaned$admin_area_2 != "NATIONAL", na.rm = TRUE)

      output$cart_cleaning_status <- renderUI({
        div(class = "alert alert-success",
            icon("check"),
            HTML(paste0(
              "Successfully cleaned ", length(non_empty), " dataset(s)!<br>",
              "<strong>", nrow(combined_cleaned), "</strong> total records ",
              "(", total_raw_records, " raw → ", nrow(combined_cleaned), " cleaned)<br>",
              "<strong>", national_count, "</strong> national + ",
              "<strong>", subnational_count, "</strong> subnational"
            )))
      })

      showNotification(paste("Cleaned", nrow(combined_cleaned), "records from", length(non_empty), "datasets!"),
                      type = "message")

    }, error = function(e) {
      output$cart_cleaning_status <- renderUI({
        div(class = "alert alert-danger",
            icon("times"),
            paste("Cart cleaning error:", e$message))
      })

      showNotification(paste("Cart cleaning error:", e$message), type = "error")
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

  # Check if cleaned data exists
  output$has_cleaned_data <- reactive({
    nrow(values$cleaned_data) > 0
  })
  outputOptions(output, "has_cleaned_data", suspendWhenHidden = FALSE)

  # ========================================
  # DATABASE INTEGRATION - SERVER LOGIC
  # ========================================

  # Check for GitHub token
  github_token <- reactive({
    token <- Sys.getenv("GITHUB_TOKEN")
    if (token == "") token <- NULL
    return(token)
  })

  # GitHub token status
  output$github_token_status <- renderUI({
    if (!is.null(github_token()) && github_token() != "") {
      div(class = "alert alert-success", style = "padding: 8px;",
          icon("check-circle"),
          " GitHub token found - ready to push")
    } else {
      div(class = "alert alert-danger", style = "padding: 8px;",
          icon("times-circle"),
          " GitHub token not found - set GITHUB_TOKEN in .Renviron or HF secrets")
    }
  })

  # Pull from GitHub button
  observeEvent(input$pull_from_github, {
    output$github_pull_status <- renderUI({
      div(class = "alert alert-info",
          icon("spinner", class = "fa-spin"),
          " Pulling latest data from GitHub...")
    })

    # Pull survey database
    values$survey_db <- load_survey_database(use_github = TRUE)
    values$pop_db <- load_population_database(use_github = TRUE)

    if (!is.null(values$survey_db) && !is.null(values$pop_db)) {
      output$github_pull_status <- renderUI({
        div(class = "alert alert-success",
            icon("check"),
            HTML(paste0(
              " Successfully pulled from GitHub!<br>",
              "Survey records: ", nrow(values$survey_db), "<br>",
              "Population records: ", nrow(values$pop_db)
            ))
        )
      })
      showNotification(
        paste("Pulled", nrow(values$survey_db), "survey and",
              nrow(values$pop_db), "population records from GitHub"),
        type = "message"
      )
    } else {
      output$github_pull_status <- renderUI({
        div(class = "alert alert-warning",
            icon("exclamation-triangle"),
            " Could not pull from GitHub. Check your connection.")
      })
    }
  })

  # Validate admin area names
  observeEvent(input$validate_names, {
    req(values$cleaned_data)

    # Load fresh databases
    values$survey_db <- load_survey_database()
    values$pop_db <- load_population_database()

    # Validate admin areas
    validation_result <- validate_admin_areas(values$cleaned_data, values$survey_db)

    values$validation_result <- validation_result

    if (validation_result$all_matched) {
      values$validated_data <- values$cleaned_data
      showNotification("All admin area names match the database!", type = "message")
    } else {
      showNotification(
        paste("Found", nrow(validation_result$unmatched), "unmatched admin area(s). Select corrections below."),
        type = "warning"
      )
    }
  })

  # Validation status UI
  output$validation_status <- renderUI({
    if (is.null(values$validation_result)) {
      return(div(class = "alert alert-secondary",
                 "Click 'Validate Admin Area Names' to check your data."))
    }

    if (values$validation_result$all_matched) {
      return(div(class = "alert alert-success",
                 icon("check"),
                 " All admin area names match! Proceed to duplicate check."))
    } else {
      return(div(class = "alert alert-warning",
                 icon("exclamation-triangle"),
                 paste(" Found", nrow(values$validation_result$unmatched),
                       "unmatched area(s). Select corrections on the right.")))
    }
  })

  # Unmatched areas UI with dropdowns
  output$unmatched_areas_ui <- renderUI({
    if (is.null(values$validation_result) || values$validation_result$all_matched) {
      return(NULL)
    }

    unmatched <- values$validation_result$unmatched

    dropdown_list <- lapply(1:nrow(unmatched), function(i) {
      row <- unmatched[i, ]
      input_id <- paste0("area_select_", i)

      div(
        style = "margin-bottom: 15px; padding: 12px; background-color: #f8f9fa; border-radius: 5px; border-left: 4px solid #ffc107;",
        h5(style = "margin-top: 0;", paste0(row$country_name, " - ", row$admin_area_2)),
        p(style = "margin-bottom: 8px;", strong("Years:"), row$years, " | ", strong("Records:"), row$n_records),
        selectInput(
          inputId = input_id,
          label = "Select: ADD AS NEW, correct name, or IGNORE:",
          choices = row$db_options[[1]],
          selected = "ADD AS NEW",
          width = "100%"
        )
      )
    })

    do.call(tagList, dropdown_list)
  })

  # Check if there are unmatched areas
  output$has_unmatched_areas <- reactive({
    !is.null(values$validation_result) &&
    !values$validation_result$all_matched &&
    !is.null(values$validation_result$unmatched) &&
    nrow(values$validation_result$unmatched) > 0
  })
  outputOptions(output, "has_unmatched_areas", suspendWhenHidden = FALSE)

  # Apply corrections
  observeEvent(input$apply_corrections, {
    req(values$validation_result, values$cleaned_data)

    unmatched <- values$validation_result$unmatched

    corrections <- data.frame(
      iso3_code = unmatched$iso3_code,
      original_name = unmatched$admin_area_2,
      selected_name = sapply(1:nrow(unmatched), function(i) {
        input[[paste0("area_select_", i)]]
      }),
      stringsAsFactors = FALSE
    )

    # Apply corrections
    values$validated_data <- apply_name_corrections(values$cleaned_data, corrections)

    # Count corrections vs ignores
    n_corrected <- sum(corrections$selected_name != "IGNORE")
    n_ignored <- sum(corrections$selected_name == "IGNORE")

    showNotification(
      paste("Applied", n_corrected, "correction(s) and ignored", n_ignored, "area(s)."),
      type = "message"
    )

    # Clear validation state so UI updates
    values$validation_result <- list(all_matched = TRUE, unmatched = NULL)
  })

  # Check for duplicates
  observeEvent(input$check_duplicates, {
    # Use validated_data if available, otherwise cleaned_data
    data_to_check <- if (!is.null(values$validated_data) && nrow(values$validated_data) > 0) {
      values$validated_data
    } else if (!is.null(values$cleaned_data) && nrow(values$cleaned_data) > 0) {
      values$cleaned_data
    } else {
      showNotification("No data available to check. Please clean data first.", type = "warning")
      return(NULL)
    }

    # Load fresh databases
    values$survey_db <- load_survey_database()
    values$pop_db <- load_population_database()

    # Combine databases for duplicate check
    survey_typed <- if (!is.null(values$survey_db)) {
      values$survey_db %>% dplyr::mutate(iso3_code = as.character(iso3_code))
    } else {
      data.frame()
    }

    pop_typed <- if (!is.null(values$pop_db)) {
      values$pop_db %>% dplyr::mutate(iso3_code = as.character(iso3_code))
    } else {
      data.frame()
    }

    existing_combined <- dplyr::bind_rows(survey_typed, pop_typed)

    values$duplicate_analysis <- detect_duplicates(data_to_check, existing_combined)

    showNotification(
      paste("Found", nrow(values$duplicate_analysis$duplicates), "duplicate(s) and",
            nrow(values$duplicate_analysis$new_records), "new record(s)."),
      type = "message"
    )
  })

  # Duplicate summary
  output$duplicate_summary <- renderPrint({
    if (is.null(values$duplicate_analysis)) {
      cat("Click 'Check for Duplicates' to analyze your data.\n")
      return()
    }

    cat("=== DUPLICATE CHECK RESULTS ===\n\n")
    cat("Duplicate records:", nrow(values$duplicate_analysis$duplicates), "\n")
    cat("New records to add:", nrow(values$duplicate_analysis$new_records), "\n")
  })

  # Duplicates table
  output$duplicates_table <- DT::renderDataTable({
    if (is.null(values$duplicate_analysis) || nrow(values$duplicate_analysis$duplicates) == 0) {
      return(data.frame(Message = "No duplicates found."))
    }

    values$duplicate_analysis$duplicates %>%
      dplyr::select(admin_area_1, admin_area_2, year, indicator_common_id,
                   new_value = survey_value, existing_value, pct_diff, action)
  }, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE,
     editable = list(target = "cell", disable = list(columns = c(0:6))))

  # Handle duplicate action edits
  observeEvent(input$duplicates_table_cell_edit, {
    info <- input$duplicates_table_cell_edit
    values$duplicate_analysis$duplicates[info$row, "action"] <- info$value
  })

  # Output to show/hide bulk action buttons

  output$has_duplicates <- reactive({
    !is.null(values$duplicate_analysis) &&
    !is.null(values$duplicate_analysis$duplicates) &&
    nrow(values$duplicate_analysis$duplicates) > 0
  })
  outputOptions(output, "has_duplicates", suspendWhenHidden = FALSE)

  # Output: has records with different values (pct_diff != 0)
  output$has_different_values <- reactive({
    if (is.null(values$duplicate_analysis) || is.null(values$duplicate_analysis$duplicates)) {
      return(FALSE)
    }
    dups <- values$duplicate_analysis$duplicates
    nrow(dups) > 0 && any(!is.na(dups$pct_diff) & abs(dups$pct_diff) > 0.01)
  })
  outputOptions(output, "has_different_values", suspendWhenHidden = FALSE)

  # Output: has records with same values (pct_diff == 0 or very close)
  output$has_same_values <- reactive({
    if (is.null(values$duplicate_analysis) || is.null(values$duplicate_analysis$duplicates)) {
      return(FALSE)
    }
    dups <- values$duplicate_analysis$duplicates
    nrow(dups) > 0 && any(is.na(dups$pct_diff) | abs(dups$pct_diff) <= 0.01)
  })
  outputOptions(output, "has_same_values", suspendWhenHidden = FALSE)

  # Output: has new records to add
  output$has_new_records <- reactive({
    !is.null(values$duplicate_analysis) &&
    !is.null(values$duplicate_analysis$new_records) &&
    nrow(values$duplicate_analysis$new_records) > 0
  })
  outputOptions(output, "has_new_records", suspendWhenHidden = FALSE)

  # Duplicate status summary UI
  output$duplicate_status_summary <- renderUI({
    if (is.null(values$duplicate_analysis)) {
      return(div(class = "text-muted", "Click 'Check for Duplicates' to analyze your data."))
    }

    dups <- values$duplicate_analysis$duplicates
    new_recs <- values$duplicate_analysis$new_records

    # Count different vs same values
    n_different <- if (nrow(dups) > 0) {
      sum(!is.na(dups$pct_diff) & abs(dups$pct_diff) > 0.01)
    } else 0

    n_same <- if (nrow(dups) > 0) {
      sum(is.na(dups$pct_diff) | abs(dups$pct_diff) <= 0.01)
    } else 0

    n_new <- nrow(new_recs)

    div(
      div(class = "alert alert-success", style = "padding: 10px; margin-bottom: 5px;",
          icon("check-circle"),
          HTML(paste0(
            " Duplicate check complete: ",
            "<strong>", n_different, "</strong> with different values, ",
            "<strong>", n_same, "</strong> with same values, ",
            "<strong>", n_new, "</strong> new records"
          ))
      )
    )
  })

  # Table: records with different values (pct_diff != 0) - with action buttons
 output$different_values_table <- DT::renderDataTable({
    if (is.null(values$duplicate_analysis) || nrow(values$duplicate_analysis$duplicates) == 0) {
      return(data.frame(Message = "No records with different values."))
    }

    dups <- values$duplicate_analysis$duplicates %>%
      dplyr::filter(!is.na(pct_diff) & abs(pct_diff) > 0.01)

    if (nrow(dups) == 0) {
      return(data.frame(Message = "No records with different values."))
    }

    # Create action buttons for each row
    dups <- dups %>%
      dplyr::mutate(
        pct_diff = round(pct_diff, 2),
        row_id = row_number(),
        # Show current action as styled badge + toggle button
        Action = sapply(row_id, function(i) {
          current_action <- dups$action[i]
          if (current_action == "replace") {
            paste0(
              '<span class="label label-warning" style="font-size: 11px;">Replace</span> ',
              '<button class="btn btn-xs btn-default action-toggle" data-row="', i, '" data-action="keep_existing" ',
              'onclick="Shiny.setInputValue(\'toggle_dup_action\', {row: ', i, ', action: \'keep_existing\', nonce: Math.random()})">',
              '<i class="fa fa-undo"></i> Keep Instead</button>'
            )
          } else {
            paste0(
              '<span class="label label-info" style="font-size: 11px;">Keep Existing</span> ',
              '<button class="btn btn-xs btn-warning action-toggle" data-row="', i, '" data-action="replace" ',
              'onclick="Shiny.setInputValue(\'toggle_dup_action\', {row: ', i, ', action: \'replace\', nonce: Math.random()})">',
              '<i class="fa fa-refresh"></i> Replace</button>'
            )
          }
        })
      ) %>%
      dplyr::select(admin_area_1, admin_area_2, year, indicator_common_id,
                   new_value = survey_value, existing_value, pct_diff, Action)

    dups
  }, options = list(
    pageLength = 10,
    scrollX = TRUE
  ), rownames = FALSE, escape = FALSE, selection = "none")

  # Handle per-row action toggle button clicks
 observeEvent(input$toggle_dup_action, {
    info <- input$toggle_dup_action
    req(info$row, info$action)

    # Get the filtered data to map row index
    dups_filtered <- values$duplicate_analysis$duplicates %>%
      dplyr::filter(!is.na(pct_diff) & abs(pct_diff) > 0.01)

    if (info$row <= nrow(dups_filtered)) {
      # Find matching row in original duplicates by composite_key
      target_key <- dups_filtered$composite_key[info$row]
      match_idx <- which(values$duplicate_analysis$duplicates$composite_key == target_key)

      if (length(match_idx) > 0) {
        values$duplicate_analysis$duplicates[match_idx, "action"] <- info$action
        # Table will re-render automatically due to reactive dependency
      }
    }
  })

  # Table: records with same values
  output$same_values_table <- DT::renderDataTable({
    if (is.null(values$duplicate_analysis) || nrow(values$duplicate_analysis$duplicates) == 0) {
      return(data.frame(Message = "No records with same values."))
    }

    same <- values$duplicate_analysis$duplicates %>%
      dplyr::filter(is.na(pct_diff) | abs(pct_diff) <= 0.01) %>%
      dplyr::select(admin_area_1, admin_area_2, year, indicator_common_id, survey_value, source)

    if (nrow(same) == 0) {
      return(data.frame(Message = "No records with same values."))
    }

    same
  }, options = list(pageLength = 5, scrollX = TRUE), rownames = FALSE)

  # Apply duplicate action from dropdown
  observeEvent(input$apply_duplicate_action, {
    if (is.null(values$duplicate_analysis) || nrow(values$duplicate_analysis$duplicates) == 0) {
      showNotification("No duplicates to update", type = "warning")
      return()
    }

    # Filter to only records with different values
    diff_indices <- which(!is.na(values$duplicate_analysis$duplicates$pct_diff) &
                          abs(values$duplicate_analysis$duplicates$pct_diff) > 0.01)

    if (length(diff_indices) == 0) {
      showNotification("No records with different values to update", type = "warning")
      return()
    }

    action <- input$duplicate_action_choice
    values$duplicate_analysis$duplicates$action[diff_indices] <- action

    action_text <- if (action == "replace") "REPLACE with new values" else "KEEP existing values"
    showNotification(
      paste("Set", length(diff_indices), "records to", action_text),
      type = "message"
    )
  })

  # Bulk action: Replace all with new values
  observeEvent(input$replace_all_duplicates, {
    if (is.null(values$duplicate_analysis) || nrow(values$duplicate_analysis$duplicates) == 0) {
      showNotification("No duplicates to update", type = "warning")
      return()
    }

    values$duplicate_analysis$duplicates$action <- "replace"
    showNotification(
      paste("Set", nrow(values$duplicate_analysis$duplicates), "records to REPLACE with new values"),
      type = "message"
    )
  })

  # Bulk action: Keep all existing values
  observeEvent(input$keep_all_existing, {
    if (is.null(values$duplicate_analysis) || nrow(values$duplicate_analysis$duplicates) == 0) {
      showNotification("No duplicates to update", type = "warning")
      return()
    }

    values$duplicate_analysis$duplicates$action <- "keep_existing"
    showNotification(
      paste("Set", nrow(values$duplicate_analysis$duplicates), "records to KEEP existing values"),
      type = "message"
    )
  })

  # New records preview
  output$new_records_preview <- DT::renderDataTable({
    if (is.null(values$duplicate_analysis) || nrow(values$duplicate_analysis$new_records) == 0) {
      return(data.frame(Message = "No new records to add."))
    }

    values$duplicate_analysis$new_records %>%
      dplyr::select(admin_area_1, admin_area_2, year, indicator_common_id, survey_value, source) %>%
      head(50)
  }, options = list(pageLength = 5, scrollX = TRUE), rownames = FALSE)

  # Integration summary
  output$integration_summary <- renderPrint({
    if (is.null(values$duplicate_analysis)) {
      cat("Please complete the validation and duplicate check steps first.\n")
      return()
    }

    cat("=== READY TO APPEND ===\n\n")
    cat("New records to add:", nrow(values$duplicate_analysis$new_records), "\n")

    if (nrow(values$duplicate_analysis$duplicates) > 0) {
      cat("\nDuplicate actions:\n")
      print(table(values$duplicate_analysis$duplicates$action))
    }

    cat("\nClick 'Append to Database' to finalize.\n")
  })

  # Commit message preview
  output$commit_preview <- renderUI({
    if (is.null(values$duplicate_analysis) || nrow(values$duplicate_analysis$new_records) == 0) {
      return(div(class = "well", style = "background: #f5f5f5; font-size: 12px;",
                 "Commit preview will appear after duplicate check..."))
    }

    # Generate preview
    preview_msg <- generate_commit_message(values$duplicate_analysis$new_records, input$commit_notes)

    div(class = "well", style = "background: #f5f5f5; font-family: monospace; font-size: 11px; white-space: pre-wrap;",
        preview_msg)
  })

  # Append to database and push to GitHub
  observeEvent(input$append_to_database, {
    req(values$duplicate_analysis)

    if (nrow(values$duplicate_analysis$new_records) == 0 &&
        !any(values$duplicate_analysis$duplicates$action != "keep_existing")) {
      showNotification("No records to add.", type = "warning")
      return()
    }

    # Check for GitHub token if pushing
    if (input$push_to_github && (is.null(github_token()) || github_token() == "")) {
      showNotification(
        "GitHub token not found. Set GITHUB_TOKEN in .Renviron or HF Spaces secrets.",
        type = "error",
        duration = 10
      )
      return()
    }

    output$append_status <- renderUI({
      div(class = "alert alert-info",
          icon("spinner", class = "fa-spin"),
          if(input$push_to_github) " Appending and pushing to GitHub..." else " Appending to local database...")
    })

    result <- append_to_databases(
      new_records = values$duplicate_analysis$new_records,
      duplicates = values$duplicate_analysis$duplicates,
      survey_db = values$survey_db,
      pop_db = values$pop_db,
      create_backup = FALSE,  # Skip local backup for cloud deployment
      push_to_github = input$push_to_github,
      github_token = github_token(),
      commit_notes = input$commit_notes
    )

    if (result$success) {
      # Update local copies
      values$survey_db <- result$updated_survey_db
      values$pop_db <- result$updated_pop_db

      output$append_status <- renderUI({
        div(class = "alert alert-success",
            icon("check-circle"),
            HTML(paste0(
              "<strong>Success!</strong><br>",
              "Survey records added: ", result$survey_added, "<br>",
              "Population records added: ", result$pop_added, "<br>",
              "Total survey database: ", result$survey_total, " records<br>",
              "Total population database: ", result$pop_total, " records",
              if(input$push_to_github) "<br><strong>Changes pushed to GitHub!</strong>" else ""
            ))
        )
      })

      showNotification(result$message, type = "message", duration = 10)

      # Clear the analysis to prevent accidental re-append
      values$duplicate_analysis <- NULL
      values$validated_data <- NULL

    } else {
      output$append_status <- renderUI({
        div(class = "alert alert-danger",
            icon("times-circle"),
            paste("Error:", result$message))
      })

      showNotification(result$message, type = "error")
    }
  })

  # ========================================
  # DATABASE EXPLORER TAB - SERVER LOGIC
  # ========================================

  # Reactive value to store the full explorer database
  explorer_db <- reactiveVal(NULL)
  explorer_filtered <- reactiveVal(NULL)

  # Load database for explorer (no cleaned data requirement)
  observeEvent(input$explorer_load_db, {
    showNotification("Loading database from GitHub...", type = "message", duration = 3)

    tryCatch({
      # Load both databases
      survey_db <- load_survey_database(use_github = TRUE)
      pop_db <- load_population_database(use_github = TRUE)

      # Combine them
      if(!is.null(survey_db) && nrow(survey_db) > 0) {
        combined <- survey_db
        if(!is.null(pop_db) && nrow(pop_db) > 0) {
          # Add pop_db rows that aren't already in survey_db
          combined <- bind_rows(combined, pop_db)
        }
        explorer_db(combined)

        # Update country dropdown - show names, sorted alphabetically
        country_names <- sort(unique(combined$country_name[!is.na(combined$country_name) & combined$country_name != ""]))
        updateSelectInput(session, "explorer_country",
                         choices = c("Select a country" = "", country_names),
                         selected = "")

        showNotification(
          paste("Loaded", nrow(combined), "records from", length(country_names), "countries"),
          type = "message", duration = 5)
      } else {
        showNotification("Could not load database - check connection", type = "warning")
      }
    }, error = function(e) {
      showNotification(paste("Error loading database:", e$message), type = "error")
    })
  })

  # Database status output
  output$explorer_db_status <- renderUI({
    db <- explorer_db()
    if(is.null(db) || nrow(db) == 0) {
      div(class = "alert alert-info", style = "margin: 0;",
          icon("info-circle"),
          " Click 'Load Database' to fetch data from GitHub")
    } else {
      n_countries <- length(unique(db$iso3_code))
      n_indicators <- length(unique(db$indicator_common_id))
      div(class = "alert alert-success", style = "margin: 0;",
          icon("check-circle"),
          paste(" Database loaded:", nrow(db), "records |",
                n_countries, "countries |",
                n_indicators, "indicators"))
    }
  })

  # Update indicator dropdown when country changes
  observeEvent(input$explorer_country, {
    req(input$explorer_country, input$explorer_country != "")
    db <- explorer_db()
    req(db)

    # Filter to selected country
    country_data <- db %>% filter(country_name == input$explorer_country)

    # Get unique indicators
    indicators <- sort(unique(country_data$indicator_common_id[!is.na(country_data$indicator_common_id)]))
    updateSelectInput(session, "explorer_indicator",
                     choices = c("All indicators" = "", indicators),
                     selected = "")

    # Get unique regions
    regions <- sort(unique(country_data$admin_area_1[!is.na(country_data$admin_area_1)]))
    updateSelectInput(session, "explorer_region",
                     choices = c("All regions" = "", regions),
                     selected = NULL)

    # Get unique sources
    sources <- sort(unique(country_data$source[!is.na(country_data$source)]))
    updateSelectInput(session, "explorer_source",
                     choices = c("All sources" = "", sources),
                     selected = NULL)
  })

  # Apply filters and update table
  observeEvent(input$explorer_apply_filter, {
    db <- explorer_db()
    req(db)
    req(input$explorer_country, input$explorer_country != "")

    # Start with country filter
    filtered <- db %>% filter(country_name == input$explorer_country)

    # Apply indicator filter if selected
    if(!is.null(input$explorer_indicator) && input$explorer_indicator != "") {
      filtered <- filtered %>% filter(indicator_common_id == input$explorer_indicator)
    }

    # Apply region filter if selected
    if(!is.null(input$explorer_region) && length(input$explorer_region) > 0 && input$explorer_region[1] != "") {
      filtered <- filtered %>% filter(admin_area_1 %in% input$explorer_region)
    }

    # Apply source filter if selected
    if(!is.null(input$explorer_source) && length(input$explorer_source) > 0 && input$explorer_source[1] != "") {
      filtered <- filtered %>% filter(source %in% input$explorer_source)
    }

    # Sort by year descending
    filtered <- filtered %>% arrange(desc(year), admin_area_1)

    explorer_filtered(filtered)
  })

  # Summary output
  output$explorer_summary <- renderUI({
    filtered <- explorer_filtered()
    if(is.null(filtered) || nrow(filtered) == 0) {
      div(class = "alert alert-secondary",
          icon("filter"),
          " Select a country and click 'Apply Filters' to view data")
    } else {
      n_records <- nrow(filtered)
      n_regions <- length(unique(filtered$admin_area_1))
      n_indicators <- length(unique(filtered$indicator_common_id))
      years <- range(filtered$year, na.rm = TRUE)

      div(class = "alert alert-info",
          icon("chart-bar"),
          paste(" Showing", n_records, "records |",
                n_regions, "regions |",
                n_indicators, "indicators |",
                "Years:", years[1], "-", years[2]))
    }
  })

  # Data table output
  output$explorer_table <- DT::renderDataTable({
    filtered <- explorer_filtered()
    req(filtered, nrow(filtered) > 0)

    # Prepare display data - rename survey_value to value for cleaner display
    display_data <- filtered %>%
      mutate(value = survey_value) %>%
      select(country_name, admin_area_1, admin_area_2,
             indicator_common_id, year, value, source)

    DT::datatable(
      display_data,
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        order = list(list(4, 'desc'))  # Sort by year descending
      ),
      rownames = FALSE
    )
  })

  # Time series plot
  output$explorer_plot <- renderPlotly({
    filtered <- explorer_filtered()

    # Return empty plot if no data
    if(is.null(filtered) || nrow(filtered) == 0) {
      return(
        plot_ly() %>%
          layout(
            title = "Select a country and apply filters to view data",
            xaxis = list(visible = FALSE),
            yaxis = list(visible = FALSE)
          )
      )
    }

    # Prepare plot data with error handling
    tryCatch({
      # Handle both survey_value and value column names
      if("survey_value" %in% names(filtered)) {
        filtered$plot_value <- filtered$survey_value
      } else if("value" %in% names(filtered)) {
        filtered$plot_value <- filtered$value
      } else {
        return(plot_ly() %>% layout(title = "No value column found in data"))
      }

      plot_data <- filtered %>%
        mutate(
          value = suppressWarnings(as.numeric(as.character(plot_value))),
          year = suppressWarnings(as.numeric(as.character(year))),
          region = as.character(admin_area_1)
        ) %>%
        filter(!is.na(value), !is.na(year)) %>%
        arrange(region, year)

      if(nrow(plot_data) == 0) {
        return(
          plot_ly() %>%
            layout(title = "No numeric data available to plot")
        )
      }

      # Get indicator for title
      indicator <- as.character(plot_data$indicator_common_id[1])

      # Simple scatter plot with lines
      plot_ly(
        data = plot_data,
        x = ~year,
        y = ~value,
        color = ~region,
        type = 'scatter',
        mode = 'lines+markers',
        marker = list(size = 8),
        colors = "Set2"
      ) %>%
        layout(
          title = paste("Time Series:", indicator),
          xaxis = list(title = "Year", dtick = 1),
          yaxis = list(title = "Value"),
          legend = list(orientation = "h", y = -0.15),
          hovermode = "x unified"
        )
    }, error = function(e) {
      plot_ly() %>%
        layout(title = paste("Plot error:", e$message))
    })
  })

  # Download handler
  output$explorer_download <- downloadHandler(
    filename = function() {
      # Sanitize country name for filename
      country <- gsub("[^a-zA-Z0-9]", "_", input$explorer_country)
      indicator <- if(!is.null(input$explorer_indicator) && input$explorer_indicator != "") {
        paste0("_", input$explorer_indicator)
      } else {
        ""
      }
      paste0("database_", country, indicator, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      filtered <- explorer_filtered()
      if(!is.null(filtered) && nrow(filtered) > 0) {
        write.csv(filtered, file, row.names = FALSE)
      }
    }
  )

  # ========================================
  # DATA REVIEW TAB - SERVER LOGIC
  # ========================================

  # Database status display
  output$review_db_status <- renderUI({
    has_cleaned <- !is.null(values$cleaned_data) && nrow(values$cleaned_data) > 0
    has_db <- !is.null(values$survey_db) && nrow(values$survey_db) > 0

    if(has_db) {
      n_records <- nrow(values$survey_db)
      countries <- unique(values$survey_db$iso3_code)
      div(class = "alert alert-success", style = "margin: 0;",
          icon("check-circle"),
          paste(" Database loaded:", n_records, "records for", paste(countries, collapse = ", ")))
    } else if(has_cleaned) {
      countries <- unique(values$cleaned_data$iso3_code)
      countries <- countries[!is.na(countries) & countries != ""]
      div(class = "alert alert-info", style = "margin: 0;",
          icon("info-circle"),
          paste(" Ready to load database for:", paste(countries, collapse = ", "),
                "- Click 'Load Database' to fetch existing records"))
    } else {
      div(class = "alert alert-warning", style = "margin: 0;",
          icon("exclamation-triangle"),
          " First fetch and clean data, then load database to compare existing vs new records.")
    }
  })

  # Duplicate summary - shows how many fetched records already exist in database
  output$review_duplicate_summary <- renderUI({
    has_cleaned <- !is.null(values$cleaned_data) && nrow(values$cleaned_data) > 0
    has_db <- !is.null(values$survey_db) && nrow(values$survey_db) > 0

    if(!has_cleaned || !has_db) {
      return(NULL)
    }

    # Create composite keys for comparison
    cleaned_keys <- values$cleaned_data %>%
      mutate(key = paste(admin_area_1, admin_area_2, year, indicator_common_id, sep = "|||")) %>%
      pull(key)

    db_keys <- values$survey_db %>%
      mutate(key = paste(admin_area_1, admin_area_2, year, indicator_common_id, sep = "|||")) %>%
      pull(key)

    # Count duplicates
    n_total <- length(cleaned_keys)
    n_duplicates <- sum(cleaned_keys %in% db_keys)
    n_new <- n_total - n_duplicates

    if(n_duplicates > 0) {
      div(class = "alert alert-info", style = "margin-top: 10px; margin-bottom: 0;",
          icon("clone"),
          HTML(paste0(" Your fetched data: ", n_total, " records total - <strong>",
                      n_duplicates, "</strong> already in database, <strong>",
                      n_new, "</strong> are new")))
    } else {
      div(class = "alert alert-success", style = "margin-top: 10px; margin-bottom: 0;",
          icon("plus-circle"),
          paste(" All", n_total, "fetched records are NEW (not in database)"))
    }
  })

  # Load database button handler - filters to countries in cleaned_data
  observeEvent(input$load_review_database, {
    # Get ISO3 codes from cleaned_data to filter
    filter_iso3 <- NULL
    if(!is.null(values$cleaned_data) && nrow(values$cleaned_data) > 0) {
      filter_iso3 <- unique(values$cleaned_data$iso3_code)
      filter_iso3 <- filter_iso3[!is.na(filter_iso3) & filter_iso3 != ""]
    }

    if(is.null(filter_iso3) || length(filter_iso3) == 0) {
      showNotification("No cleaned data found - please fetch and clean data first, then load database to compare",
                      type = "warning", duration = 5)
      return()
    }

    showNotification(
      paste("Loading database for:", paste(filter_iso3, collapse = ", ")),
      type = "message", duration = 3)

    tryCatch({
      # Load full database first
      full_survey_db <- load_survey_database(use_github = TRUE)
      full_pop_db <- load_population_database(use_github = TRUE)

      # Filter to relevant countries by ISO3
      if(!is.null(full_survey_db) && nrow(full_survey_db) > 0) {
        values$survey_db <- full_survey_db %>%
          filter(iso3_code %in% filter_iso3)

        if(!is.null(full_pop_db) && nrow(full_pop_db) > 0) {
          values$pop_db <- full_pop_db %>%
            filter(iso3_code %in% filter_iso3)
        }

        showNotification(
          paste("Loaded", nrow(values$survey_db), "database records for",
                paste(filter_iso3, collapse = ", ")),
          type = "message", duration = 5)
      } else {
        showNotification("Could not load database - check connection", type = "warning")
      }
    }, error = function(e) {
      showNotification(paste("Error loading database:", e$message), type = "error")
    })
  })

  # Populate country selector from both survey_db and cleaned_data
  observe({
    countries_from_db <- if(!is.null(values$survey_db) && nrow(values$survey_db) > 0) {
      unique(values$survey_db$country_name)
    } else {
      character(0)
    }

    countries_from_new <- if(!is.null(values$cleaned_data) && nrow(values$cleaned_data) > 0) {
      unique(values$cleaned_data$country_name)
    } else {
      character(0)
    }

    all_countries <- sort(unique(c(countries_from_db, countries_from_new)))

    updateSelectInput(session, "review_country",
                     choices = c("Select a country" = "", all_countries),
                     selected = "")
  })

  # Populate region selector based on selected country
  observeEvent(input$review_country, {
    req(input$review_country, input$review_country != "")

    # Get regions from database
    regions_from_db <- if(!is.null(values$survey_db) && nrow(values$survey_db) > 0) {
      values$survey_db %>%
        filter(country_name == input$review_country,
               admin_area_2 != "NATIONAL") %>%
        pull(admin_area_2) %>%
        unique()
    } else {
      character(0)
    }

    # Get regions from new data
    regions_from_new <- if(!is.null(values$cleaned_data) && nrow(values$cleaned_data) > 0) {
      values$cleaned_data %>%
        filter(country_name == input$review_country,
               admin_area_2 != "NATIONAL") %>%
        pull(admin_area_2) %>%
        unique()
    } else {
      character(0)
    }

    all_regions <- sort(unique(c(regions_from_db, regions_from_new)))

    updateSelectInput(session, "review_regions",
                     choices = all_regions,
                     selected = if(length(all_regions) > 0) all_regions[1:min(3, length(all_regions))] else NULL)
  })

  # Populate indicator selector based on country selection
  observe({
    req(input$review_country, input$review_country != "")

    # Get indicators from database
    indicators_db <- if(!is.null(values$survey_db) && nrow(values$survey_db) > 0) {
      values$survey_db %>%
        filter(country_name == input$review_country) %>%
        pull(indicator_common_id) %>%
        unique()
    } else {
      character(0)
    }

    # Get indicators from new data
    indicators_new <- if(!is.null(values$cleaned_data) && nrow(values$cleaned_data) > 0) {
      values$cleaned_data %>%
        filter(country_name == input$review_country) %>%
        pull(indicator_common_id) %>%
        unique()
    } else {
      character(0)
    }

    all_indicators <- sort(unique(c(indicators_db, indicators_new)))

    updateSelectInput(session, "review_indicator",
                     choices = c("Select an indicator" = "", all_indicators),
                     selected = if(length(all_indicators) > 0) all_indicators[1] else "")
  })

  # Generate coverage comparison plot
  observeEvent(input$generate_review_plot, {
    req(input$review_country, input$review_indicator,
        input$review_country != "", input$review_indicator != "")

    # Build region filter
    selected_regions <- if(isTRUE(input$review_include_national)) {
      c("NATIONAL", input$review_regions)
    } else {
      input$review_regions
    }

    # Filter database data
    db_data <- if(!is.null(values$survey_db) && nrow(values$survey_db) > 0) {
      values$survey_db %>%
        filter(country_name == input$review_country,
               admin_area_2 %in% selected_regions,
               indicator_common_id == input$review_indicator) %>%
        mutate(data_source = "Database")
    } else {
      data.frame()
    }

    # Filter new data
    new_data <- if(!is.null(values$cleaned_data) && nrow(values$cleaned_data) > 0) {
      values$cleaned_data %>%
        filter(country_name == input$review_country,
               admin_area_2 %in% selected_regions,
               indicator_common_id == input$review_indicator) %>%
        mutate(data_source = "New")
    } else {
      data.frame()
    }

    # Generate overlay plot
    output$review_coverage_plot <- renderPlotly({
      if(nrow(db_data) == 0 && nrow(new_data) == 0) {
        return(plotly::plot_ly() %>%
                 add_annotations(x = 0.5, y = 0.5,
                                text = "No data available for this selection",
                                showarrow = FALSE, xref = "paper", yref = "paper",
                                font = list(size = 16, color = "#999")) %>%
                 layout(xaxis = list(visible = FALSE),
                       yaxis = list(visible = FALSE)))
      }

      # Define colors for different database sources
      source_colors <- list(
        "DHS National" = "#0f706d",
        "DHS Sub-national" = "#14967d",
        "MICS" = "#3498db",
        "WUENIC" = "#9b59b6",
        "UNWPP" = "#f39c12",
        "WHO" = "#1abc9c",
        "UN IGME" = "#e67e22",
        "Admin" = "#7f8c8d"
      )
      default_db_color <- "#34495e"

      # Create plot
      p <- plot_ly() %>%
        layout(
          title = list(text = paste(input$review_indicator, "-", input$review_country),
                      font = list(size = 16)),
          xaxis = list(title = "Year"),
          yaxis = list(title = "Value"),
          legend = list(orientation = "h", y = -0.2, xanchor = "center", x = 0.5),
          hovermode = "x unified"
        )

      # Add database traces - grouped by source AND region
      # Use different styles for national vs subnational:
      # - National: solid line, circle markers, full opacity
      # - Subnational: dashed line, triangle markers, lighter color
      if(nrow(db_data) > 0) {
        # Get unique source types
        db_sources <- unique(db_data$source)

        # Track subnational region index for varying colors slightly
        subnational_regions <- setdiff(unique(db_data$admin_area_2), "NATIONAL")
        region_color_offset <- setNames(seq_along(subnational_regions) * 0.15, subnational_regions)

        for(src in db_sources) {
          src_color <- if(src %in% names(source_colors)) source_colors[[src]] else default_db_color

          for(region in unique(db_data$admin_area_2)) {
            src_region_data <- db_data %>%
              filter(source == src, admin_area_2 == region)

            if(nrow(src_region_data) > 0) {
              # Simplify source name for legend
              src_short <- gsub(" National| Sub-national", "", src)

              # Different styling for national vs subnational
              is_national <- region == "NATIONAL"

              if(is_national) {
                label <- paste0(src_short, " (National)")
                line_style <- list(color = src_color, width = 3)
                marker_style <- list(color = src_color, size = 10, symbol = "circle")
              } else {
                label <- paste0(src_short, ": ", region)
                # Use dashed line and triangle markers for subnational
                line_style <- list(color = src_color, width = 2, dash = "dot")
                marker_style <- list(color = src_color, size = 8, symbol = "triangle-up",
                                    line = list(color = "white", width = 1))
              }

              p <- p %>% add_trace(
                data = src_region_data,
                x = ~year, y = ~survey_value,
                type = "scatter", mode = "lines+markers",
                name = label,
                line = line_style,
                marker = marker_style,
                legendgroup = src,
                hovertemplate = paste0(
                  "<b>", src, "</b><br>",
                  "Region: ", region, "<br>",
                  "Year: %{x}<br>",
                  "Value: %{y:.3f}<extra></extra>"
                )
              )
            }
          }
        }
      }

      # Add new/fetched data traces - in red/orange tones
      # National: solid red, diamond markers
      # Subnational: dashed orange, square markers
      if(nrow(new_data) > 0) {
        new_sources <- unique(new_data$source)

        for(src in new_sources) {
          for(region in unique(new_data$admin_area_2)) {
            src_region_data <- new_data %>%
              filter(source == src, admin_area_2 == region)

            if(nrow(src_region_data) > 0) {
              src_short <- gsub(" National| Sub-national", "", src)
              is_national <- region == "NATIONAL"

              if(is_national) {
                label <- paste0("NEW ", src_short, " (National)")
                line_style <- list(color = "#e74c3c", width = 3, dash = "dash")
                marker_style <- list(color = "#e74c3c", size = 11, symbol = "diamond")
              } else {
                label <- paste0("NEW ", src_short, ": ", region)
                # Use orange and squares for subnational new data
                line_style <- list(color = "#e67e22", width = 2, dash = "dashdot")
                marker_style <- list(color = "#e67e22", size = 9, symbol = "square",
                                    line = list(color = "white", width = 1))
              }

              p <- p %>% add_trace(
                data = src_region_data,
                x = ~year, y = ~survey_value,
                type = "scatter", mode = "lines+markers",
                name = label,
                line = line_style,
                marker = marker_style,
                legendgroup = "NEW",
                hovertemplate = paste0(
                  "<b>NEW: ", src, "</b><br>",
                  "Region: ", region, "<br>",
                  "Year: %{x}<br>",
                  "Value: %{y:.3f}<extra></extra>"
                )
              )
            }
          }
        }
      }

      p
    })

    # Render summary tables
    output$review_db_table <- DT::renderDataTable({
      if(nrow(db_data) == 0) {
        return(data.frame(Message = "No database records for this selection"))
      }
      db_data %>%
        select(admin_area_2, year, survey_value, source) %>%
        arrange(admin_area_2, year)
    }, options = list(pageLength = 5, scrollX = TRUE, dom = 'tip'), rownames = FALSE)

    output$review_new_table <- DT::renderDataTable({
      if(nrow(new_data) == 0) {
        return(data.frame(Message = "No new records for this selection"))
      }
      new_data %>%
        select(admin_area_2, year, survey_value, source) %>%
        arrange(admin_area_2, year)
    }, options = list(pageLength = 5, scrollX = TRUE, dom = 'tip'), rownames = FALSE)
  })

  # ========================================
  # MANUAL ENTRY TAB - SERVER LOGIC
  # ========================================

  # Load countries and regions from backbone files on startup
  observe({
    backbone_files <- list.files("assets", pattern = "_backbone\\.csv$", full.names = TRUE)

    # Mapping from backbone filename to proper country display name
    country_name_map <- c(
      "afghanistan" = "Afghanistan",
      "bangladesh1" = "Bangladesh (DGHS)",
      "bangladesh2" = "Bangladesh (DGFP)",
      "cameroon" = "Cameroon",
      "drc" = "République Démocratique du Congo",
      "ethiopia" = "Ethiopia",
      "ghana" = "Ghana",
      "guinea" = "Guinée",
      "guineaold" = "Guinea (Old)",
      "haiti" = "Haiti",
      "kenya" = "Kenya",
      "liberia" = "Liberia",
      "malawi" = "Malawi",
      "mali" = "Mali",
      "nigeria" = "Nigeria",
      "senegal" = "Sénégal",
      "sierraleone" = "Sierra Leone",
      "somalia" = "Somalia",
      "somaliland" = "Somaliland"
    )

    countries <- c()
    regions <- list()

    for(file in backbone_files) {
      tryCatch({
        data <- read.csv(file, stringsAsFactors = FALSE)
        # Extract country key from file (e.g., "senegal_backbone.csv" -> "senegal")
        file_key <- gsub("_backbone\\.csv$", "", basename(file))
        # Map to proper country name, fallback to title case if not in mapping
        country_key <- if(file_key %in% names(country_name_map)) {
          country_name_map[[file_key]]
        } else {
          tools::toTitleCase(file_key)
        }

        # Get unique admin_area_2 values (regions)
        if("admin_area_2" %in% names(data)) {
          region_list <- unique(data$admin_area_2[!is.na(data$admin_area_2) & data$admin_area_2 != ""])
          regions[[country_key]] <- sort(region_list)
          countries <- c(countries, country_key)
        }
      }, error = function(e) {
        message("Could not load backbone file: ", file, " - ", e$message)
      })
    }

    values$available_countries <- sort(unique(countries))
    values$country_regions <- regions

    # Update country selector
    updateSelectInput(session, "manual_country",
                     choices = c("Select a country" = "", values$available_countries))
  })

  # Update indicator dropdown based on selected category
  observeEvent(input$manual_indicator_category, {
    category <- input$manual_indicator_category

    if (is.null(category) || category == "") {
      updateSelectInput(session, "manual_indicator",
                       choices = c("Select category first..." = ""),
                       selected = "")
    } else {
      # Get indicators for this category
      indicators <- get_indicators_by_category(category)
      indicator_choices <- setNames(indicators, indicators)
      updateSelectInput(session, "manual_indicator",
                       choices = c("Select an indicator" = "", indicator_choices),
                       selected = "")
    }
  }, ignoreNULL = FALSE)

  # Cascading region selector based on country
  observeEvent(input$manual_country, {
    req(input$manual_country, input$manual_country != "")

    if(input$manual_country %in% names(values$country_regions)) {
      region_list <- values$country_regions[[input$manual_country]]
      regions <- c("NATIONAL" = "NATIONAL",
                   setNames(region_list, region_list))
    } else {
      regions <- c("NATIONAL" = "NATIONAL")
    }

    updateSelectInput(session, "manual_region",
                     choices = regions,
                     selected = "NATIONAL")
  })

  # Display indicator type
  output$manual_indicator_type_display <- renderUI({
    req(input$manual_indicator, input$manual_indicator != "")
    indicator_type <- get_indicator_type(input$manual_indicator)
    div(class = "alert alert-secondary", style = "padding: 8px; margin-top: 5px;",
        strong("Indicator Type: "), indicator_type)
  })

  # Value guidance based on indicator type
  output$manual_value_guidance <- renderUI({
    req(input$manual_indicator, input$manual_indicator != "")
    indicator_type <- get_indicator_type(input$manual_indicator)

    guidance <- switch(indicator_type,
      "percent" = HTML("<div class='alert alert-info' style='padding: 8px; font-size: 12px;'>
                        <strong>Percentage:</strong> Enter as decimal (0.0 to 1.0). Example: 85% = 0.85</div>"),
      "rate" = HTML("<div class='alert alert-info' style='padding: 8px; font-size: 12px;'>
                     <strong>Rate:</strong> Enter the rate value directly (e.g., deaths per 1,000)</div>"),
      "population_estimate" = HTML("<div class='alert alert-info' style='padding: 8px; font-size: 12px;'>
                                    <strong>Population:</strong> Enter as thousands (e.g., 1500 for 1.5 million)</div>"),
      HTML("<div class='alert alert-secondary' style='padding: 8px; font-size: 12px;'>
            Enter value as appropriate for this indicator</div>")
    )

    guidance
  })

  # Entry preview
  output$manual_entry_preview <- renderPrint({
    if(is.null(input$manual_country) || input$manual_country == "" ||
       is.null(input$manual_indicator) || input$manual_indicator == "") {
      cat("Fill in the form to see preview...")
      return()
    }

    # Get country codes
    iso2 <- countrycode::countrycode(input$manual_country, "country.name", "iso2c", warn = FALSE)
    iso3 <- countrycode::countrycode(input$manual_country, "country.name", "iso3c", warn = FALSE)

    cat("=== Entry Preview ===\n\n")
    cat("admin_area_1:", input$manual_country, "\n")
    cat("admin_area_2:", input$manual_region, "\n")
    cat("year:", input$manual_year, "\n")
    cat("indicator_id:", input$manual_indicator, "\n")
    cat("indicator_common_id:", input$manual_indicator, "\n")
    cat("indicator_type:", get_indicator_type(input$manual_indicator), "\n")
    cat("survey_value:", input$manual_value, "\n")
    cat("source:", input$manual_source, "\n")
    cat("source_detail:", input$manual_source_detail, "\n")
    cat("survey_type:", input$manual_survey_type, "\n")
    cat("country_name:", input$manual_country, "\n")
    cat("iso2_code:", iso2, "\n")
    cat("iso3_code:", iso3, "\n")
  })

  # Add entry to staging
  observeEvent(input$add_manual_entry, {
    # Validate required fields
    if(is.null(input$manual_country) || input$manual_country == "") {
      showNotification("Please select a country", type = "error")
      return()
    }
    if(is.null(input$manual_indicator) || input$manual_indicator == "") {
      showNotification("Please select an indicator", type = "error")
      return()
    }
    if(is.na(input$manual_value)) {
      showNotification("Please enter a value", type = "error")
      return()
    }

    # Validate percentage values
    indicator_type <- get_indicator_type(input$manual_indicator)
    if(indicator_type == "percent" && (input$manual_value < 0 || input$manual_value > 1)) {
      showNotification("Percentage values must be between 0 and 1 (e.g., 0.85 for 85%)",
                      type = "error", duration = 5)
      return()
    }

    # Get country codes
    iso2 <- countrycode::countrycode(input$manual_country, "country.name", "iso2c", warn = FALSE)
    iso3 <- countrycode::countrycode(input$manual_country, "country.name", "iso3c", warn = FALSE)

    # Create new entry
    new_entry <- data.frame(
      admin_area_1 = input$manual_country,
      admin_area_2 = input$manual_region,
      year = as.integer(input$manual_year),
      indicator_id = input$manual_indicator,
      indicator_common_id = input$manual_indicator,
      indicator_type = indicator_type,
      survey_value = as.numeric(input$manual_value),
      source = input$manual_source,
      source_detail = input$manual_source_detail,
      survey_type = input$manual_survey_type,
      country_name = input$manual_country,
      iso2_code = ifelse(is.na(iso2), "", iso2),
      iso3_code = ifelse(is.na(iso3), "", iso3),
      stringsAsFactors = FALSE
    )

    # Check for duplicate in staging
    if(!is.null(values$staged_manual_entries) && nrow(values$staged_manual_entries) > 0) {
      duplicate_check <- values$staged_manual_entries %>%
        filter(admin_area_1 == new_entry$admin_area_1,
               admin_area_2 == new_entry$admin_area_2,
               year == new_entry$year,
               indicator_common_id == new_entry$indicator_common_id)

      if(nrow(duplicate_check) > 0) {
        showNotification("This entry already exists in staging", type = "warning")
        return()
      }
    }

    # Add to staging
    if(is.null(values$staged_manual_entries) || nrow(values$staged_manual_entries) == 0) {
      values$staged_manual_entries <- new_entry
    } else {
      values$staged_manual_entries <- bind_rows(values$staged_manual_entries, new_entry)
    }

    showNotification("Entry added to staging", type = "message")

    output$manual_entry_status <- renderUI({
      div(class = "alert alert-success", style = "padding: 8px;",
          icon("check"),
          paste(" Entry added. Total staged:", nrow(values$staged_manual_entries)))
    })
  })

  # Staged entries table
  output$staged_entries_table <- DT::renderDataTable({
    if(is.null(values$staged_manual_entries) || nrow(values$staged_manual_entries) == 0) {
      return(data.frame(Message = "No staged entries yet"))
    }

    values$staged_manual_entries %>%
      select(admin_area_1, admin_area_2, year, indicator_common_id, survey_value, source)
  }, options = list(pageLength = 5, scrollX = TRUE, dom = 'tip'),
     selection = "multiple",
     rownames = FALSE)

  # Staged entries summary
  output$staged_entries_summary <- renderUI({
    n <- if(!is.null(values$staged_manual_entries)) nrow(values$staged_manual_entries) else 0
    div(class = "alert alert-info", style = "padding: 8px;",
        strong("Total staged entries: "), n)
  })

  # Remove staged entry
  observeEvent(input$remove_staged_entry, {
    selected_rows <- input$staged_entries_table_rows_selected

    if(is.null(selected_rows) || length(selected_rows) == 0) {
      showNotification("Please select entries to remove", type = "warning")
      return()
    }

    values$staged_manual_entries <- values$staged_manual_entries[-selected_rows, ]
    showNotification(paste("Removed", length(selected_rows), "entry(ies)"), type = "message")
  })

  # Commit staged entries to cleaned_data
  observeEvent(input$commit_staged_entries, {
    if(is.null(values$staged_manual_entries) || nrow(values$staged_manual_entries) == 0) {
      showNotification("No staged entries to commit", type = "warning")
      return()
    }

    n_added <- nrow(values$staged_manual_entries)

    # Add all staged entries to cleaned_data
    if(is.null(values$cleaned_data) || nrow(values$cleaned_data) == 0) {
      values$cleaned_data <- values$staged_manual_entries
    } else {
      values$cleaned_data <- bind_rows(values$cleaned_data, values$staged_manual_entries)
    }

    # Clear staging
    values$staged_manual_entries <- data.frame()

    showNotification(paste("Committed", n_added, "entries to cleaned_data"), type = "message")

    output$manual_entry_status <- renderUI({
      div(class = "alert alert-success", style = "padding: 8px;",
          icon("check"),
          paste(" Committed", n_added, "entries! Total cleaned_data:", nrow(values$cleaned_data)))
    })
  })

  # Clear manual entry form
  observeEvent(input$clear_manual_form, {
    updateSelectInput(session, "manual_country", selected = "")
    updateSelectInput(session, "manual_region", selected = "NATIONAL")
    updateNumericInput(session, "manual_year", value = as.integer(format(Sys.Date(), "%Y")))
    updateSelectInput(session, "manual_indicator", selected = "")
    updateNumericInput(session, "manual_value", value = NA)
    updateSelectInput(session, "manual_source", selected = "Other")
    updateTextInput(session, "manual_source_detail", value = "")
    updateSelectInput(session, "manual_survey_type", selected = "household")

    output$manual_entry_status <- renderUI({ NULL })
  })
}

# ========================================
# RUN APP
# ========================================

shinyApp(ui = ui, server = server)
