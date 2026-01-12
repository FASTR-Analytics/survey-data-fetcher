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

Set up a `.Renviron` file with your API keys:

```bash
# .Renviron

# Required for UNWPP data fetching
UNWPP_TOKEN=your_unwpp_api_token_here

# Required for GitHub database integration
GITHUB_TOKEN=ghp_your_personal_access_token_here
```

On Hugging Face Spaces, add these as secrets in Settings.

### Getting a UNWPP Token

1. Go to [population.un.org/dataportal](https://population.un.org/dataportal/)
2. Register for an account
3. Request API access to get your token

## Next Steps

Once set up, see [Fetching Data](fetching-data.md) for a complete workflow example.
