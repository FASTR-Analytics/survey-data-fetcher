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
    
    # External CSS file reference (clean!)
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    
    # All tabs now come from ui_components.R
    tabItems(
      create_fetcher_tab(),
      create_metadata_tab(),
      create_results_tab(),
      create_processing_tab(),
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
  
  observe({
    req(input$data_source)  # Add this line to require the input exists
    
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
