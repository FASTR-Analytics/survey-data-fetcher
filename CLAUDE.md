# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is a Shiny web application for fetching and processing survey data from multiple international sources (DHS, MICS, UN World Population Prospects). It provides an interactive interface for selecting indicators, countries, fetching data, and exporting results in various formats.

## Development Commands

### Running the Application
```r
# From R console or RStudio
shiny::runApp()
```

### Installing Dependencies
```r
install.packages(c(
  "shiny", "shinydashboard", "DT", "dplyr", "rdhs", 
  "rsdmx", "httr", "jsonlite", "countrycode", "data.table",
  "plotly", "shinyWidgets", "RCurl", "shinycssloaders", 
  "shinyBS", "stringr", "shinyjs"
))
```

### Environment Setup
- Create a `.Renviron` file in the project root with API credentials:
```
DHS_API_KEY=your_dhs_api_key_here
```

## Code Architecture

### Modular Structure
The application follows a modular architecture with clear separation of concerns:

- **app.R**: Main application entry point containing UI definition and server logic
- **R/data_functions.R**: API integration functions for fetching data from DHS, MICS, and UNWPP sources
- **R/cleaning_functions.R**: Data processing and standardization functions
- **R/ui_components.R**: Reusable UI component functions to keep app.R clean
- **www/custom.css**: Application styling

### Key Components

#### Data Sources Integration
- **DHS (Demographic & Health Surveys)**: Uses rdhs package with API key authentication
- **MICS (Multiple Indicator Cluster Surveys)**: Custom API integration with UNICEF data
- **UNWPP (UN World Population Prospects)**: Integration with UN statistical APIs

#### Reactive Architecture
- `values` reactive values object stores: metadata, countries, fetched_data, cleaned_data
- Server logic uses `observe()` and `observeEvent()` for data source switching and user interactions
- UI components are dynamically rendered based on selected data sources

#### Favorite Indicators System
- Pre-configured indicator groups in `get_dhs_favorites()` function
- Interactive buttons for quick selection of common indicator combinations
- Visual feedback with button state changes and counters

### File Organization Pattern
- Functions are prefixed by purpose: `fetch_*`, `clean_*`, `create_*`
- UI components return complete UI elements rather than fragments
- Data functions handle both metadata and actual data fetching
- Cleaning functions standardize data across different source formats

### State Management
- Single `values` reactiveValues object manages all application state
- Data flows: metadata → country selection → data fetching → cleaning → visualization
- Error handling with user-friendly notifications and status messages

## Data Processing Pipeline

1. **Metadata Loading**: Fetch available indicators and countries based on selected source
2. **User Selection**: Interactive selection of indicators and countries with favorites system
3. **Data Fetching**: API calls to retrieve actual survey data
4. **Data Cleaning**: Standardization and validation of fetched data
5. **Export/Visualization**: Download options and interactive plotting capabilities

## API Integration Notes

- All API functions include error handling and user feedback
- DHS requires API key authentication stored in .Renviron
- MICS and UNWPP use different authentication patterns
- Functions are designed to handle rate limiting and connection issues gracefully