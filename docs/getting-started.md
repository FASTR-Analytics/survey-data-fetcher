# Getting Started

## Prerequisites

### R Packages

The app requires the following R packages:

```r
# Core Shiny packages
install.packages(c("shiny", "shinydashboard", "shinyWidgets",
                   "shinycssloaders", "shinyBS", "shinyjs"))

# Data manipulation
install.packages(c("dplyr", "tidyr", "readr", "data.table", "stringr"))

# API and web
install.packages(c("httr", "jsonlite", "RCurl", "rsdmx"))

# Visualization
install.packages(c("plotly", "DT"))

# Other utilities
install.packages(c("countrycode", "readxl", "stringdist", "base64enc"))

# DHS data
install.packages("rdhs")
```

## Running Locally

### Option 1: RStudio

1. Open the `survey_data_fetcher.Rproj` file in RStudio
2. Open `app.R`
3. Click "Run App" or press `Ctrl+Shift+Enter`

### Option 2: Command Line

```bash
cd /path/to/survey_data_fetcher
R -e "shiny::runApp()"
```

### Option 3: With specific port

```r
shiny::runApp(host = "0.0.0.0", port = 3838)
```

## Accessing on Hugging Face

The app is deployed at:

**[https://huggingface.co/spaces/CIJBoulange/survey-data-fetcher](https://huggingface.co/spaces/CIJBoulange/survey-data-fetcher)**

No installation required - just open in your browser.

## Environment Variables

For GitHub integration, set up a `.Renviron` file:

```bash
# .Renviron
GITHUB_TOKEN=ghp_your_personal_access_token_here
```

Or on Hugging Face Spaces, add it as a secret in Settings.

## First Steps

1. **Select a data source** from the sidebar (DHS, UNICEF, or UNWPP)
2. **Choose indicators** using Quick Favorites or Browse All
3. **Select countries** of interest
4. **Click "Fetch Data"** to retrieve from the API
5. **Clean the data** in the "Clean & Process" tab
6. **Integrate with database** in the "Database Integration" tab
