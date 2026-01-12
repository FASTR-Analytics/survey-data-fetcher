---
title: Survey Data Fetcher
emoji: 📊
colorFrom: blue
colorTo: purple
sdk: docker
app_port: 3838
---

# FASTR Survey Data Fetcher

A Shiny web application for fetching, cleaning, and integrating health survey data from multiple international sources into the FASTR Analytics Platform.

**[📖 Full Documentation](https://fastr-analytics.github.io/survey-data-fetcher/)** | **[🚀 Live App](https://huggingface.co/spaces/CIJBoulange/survey-data-fetcher)**

## Features

- **Multi-Source Data Fetching**: Connect to DHS, UNICEF (MICS/WUENIC), and UN World Population Prospects APIs
- **Data Cleaning & Standardization**: Automatically harmonize data for FASTR compatibility
- **GitHub Integration**: Pull the latest database, validate new data, and push updates directly to GitHub
- **Collaborative Workflow**: Multiple users can contribute to the unified survey database
- **Visualizations**: Interactive time series and multi-indicator comparison charts

## Data Sources

| Source | Description | Geographic Level |
|--------|-------------|------------------|
| **DHS** | Demographic & Health Surveys from 90+ countries | National + Subnational |
| **UNICEF** | MICS surveys, WUENIC immunization estimates, UN IGME mortality | National |
| **UNWPP** | UN World Population Prospects | National |

## Quick Start

### Run on Hugging Face (No Installation)

Visit: **https://huggingface.co/spaces/CIJBoulange/survey-data-fetcher**

### Run Locally

```bash
# Clone
git clone https://github.com/FASTR-Analytics/survey-data-fetcher.git
cd survey-data-fetcher

# Install R packages
R -e "install.packages(c('shiny', 'shinydashboard', 'DT', 'dplyr', 'rdhs',
      'rsdmx', 'httr', 'jsonlite', 'countrycode', 'plotly', 'shinyWidgets',
      'shinycssloaders', 'stringdist', 'base64enc'))"

# Run
R -e "shiny::runApp()"
```

## Workflow

```
1. Fetch Data     →  Select source, indicators, countries
2. Clean Data     →  Apply FASTR standardization
3. Validate       →  Check admin area names against database
4. Check Dupes    →  Identify existing records
5. Push to GitHub →  Append to unified database
```

## Database Integration

The app syncs with the FASTR-Analytics/modules repository:

| Database | Contents |
|----------|----------|
| `survey_data_unified.csv` | Survey indicators (anc1, penta1, bcg, u5mr, etc.) |
| `population_estimates_only.csv` | Population estimates (poptot, livebirth, etc.) |

### Environment Variables

Set these in your `.Renviron` file (local) or as Hugging Face Spaces secrets:

| Variable | Purpose | Required For |
|----------|---------|--------------|
| `UNWPP_TOKEN` | UN Population API access | Fetching UNWPP data |
| `GITHUB_TOKEN` | GitHub repo write access | Pushing to database |

**Local (.Renviron):**
```
UNWPP_TOKEN=your_unwpp_token_here
GITHUB_TOKEN=ghp_your_github_token_here
```

**Hugging Face:** Add as secrets in Space Settings.

## Project Structure

```
survey_data_fetcher/
├── app.R                    # Main Shiny application
├── Dockerfile               # Docker configuration
├── mkdocs.yml              # Documentation config
├── R/
│   ├── ui_components.R      # Modular UI functions
│   ├── data_functions.R     # API fetching
│   ├── cleaning_functions.R # Data standardization
│   ├── indicator_mappings.R # ID mappings
│   └── integration_functions.R # GitHub sync
├── docs/                    # MkDocs documentation
├── www/                     # CSS and JS assets
└── assets/                  # Geographic reference data
```

## Documentation

Full documentation available at: **https://fastr-analytics.github.io/survey-data-fetcher/**

- [Getting Started](https://fastr-analytics.github.io/survey-data-fetcher/getting-started/)
- [Data Sources](https://fastr-analytics.github.io/survey-data-fetcher/data-sources/)
- [Database Integration](https://fastr-analytics.github.io/survey-data-fetcher/database-integration/)
- [Indicator Reference](https://fastr-analytics.github.io/survey-data-fetcher/indicators/)
- [Deployment Guide](https://fastr-analytics.github.io/survey-data-fetcher/deployment/)

## Contributing

This is a FASTR Analytics tool. For issues or feature requests, please contact the development team or open an issue on GitHub.

## License

Internal use - FASTR Analytics
