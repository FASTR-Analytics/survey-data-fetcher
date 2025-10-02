---
title: Survey Data Fetcher
emoji: 📊
colorFrom: blue
colorTo: purple
sdk: docker
app_port: 3838
---

# FASTR Survey Data Fetcher

A Shiny web application for fetching and processing survey data from multiple international sources.

## Features

- **Multiple Data Sources**: DHS, MICS, and UN World Population Prospects
- **Interactive Interface**: Clean, responsive design with favorite indicator shortcuts
- **Data Processing**: Built-in cleaning and standardization functions
- **Export Options**: Download data in CSV or RDS formats
- **Real-time Status**: Progress indicators and error handling

## Data Sources

- **DHS**: Demographic & Health Surveys from 90+ countries
- **MICS**: Multiple Indicator Cluster Surveys (UNICEF)
- **UNWPP**: UN World Population Prospects

## Installation

1. Clone this repository
2. Install required R packages:
```r
install.packages(c(
  "shiny", "shinydashboard", "DT", "dplyr", "rdhs", 
  "rsdmx", "httr", "jsonlite", "countrycode", "data.table",
  "plotly", "shinyWidgets", "RCurl", "shinycssloaders", 
  "shinyBS", "stringr", "shinyjs"
))
```

3. Create a `.Renviron` file with your API credentials:
```
DHS_API_KEY=your_dhs_api_key_here
# Add other API keys as needed
```

4. Run the app:
```r
shiny::runApp()
```

## Project Structure

```
survey_data_fetcher/
├── app.R                 # Main application file
├── R/
│   ├── ui_components.R   # Modular UI functions
│   ├── data_functions.R  # Data fetching functions
│   └── cleaning_functions.R # Data processing functions
├── www/
│   └── custom.css        # Application styling
└── .Renviron            # API credentials (not in git)
```

## Usage

1. **Select Data Source**: Choose between DHS, MICS, or UNWPP
2. **Choose Indicators**: Use favorites shortcuts or browse all available indicators
3. **Select Countries**: Pick countries of interest
4. **Fetch Data**: Click to retrieve data with real-time progress
5. **Process & Export**: Clean data and download in preferred format

## Key Features

### Favorite Indicators
Quick-select buttons for common indicator groups:
- ANC & Maternal Health
- Child Vaccinations  
- IPTp (Malaria Prevention)
- Mortality & Fertility

### Smart Country Selection
- Live search and filtering
- Regional groupings
- Support for both national and subnational data (DHS)

### Data Quality
- Standardized indicator IDs across sources
- Built-in data validation
- Comprehensive error handling

## Contributing

This is a FASTR Analytics internal tool. For issues or feature requests, please contact the development team.

## License

Internal use only - FASTR Analytics

---

**Note**: This application requires valid API credentials for data sources. Contact your administrator for access.