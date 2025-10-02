# Use Rocker Shiny image (comes with Shiny pre-installed)
FROM rocker/shiny:4.3.1

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libsodium-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    libudunits2-dev \
    && rm -rf /var/lib/apt/lists/*

# Use RStudio Package Manager for faster binary installations
RUN R -e "options(repos = c(RSPM = 'https://packagemanager.rstudio.com/all/__linux__/jammy/latest', CRAN = 'https://cloud.r-project.org/')); \
    install.packages(c('shinydashboard', 'DT', 'dplyr', 'httr', 'jsonlite', 'countrycode', 'data.table', 'plotly', 'shinyWidgets', 'RCurl', 'shinycssloaders', 'shinyBS', 'stringr', 'shinyjs', 'readxl', 'rsdmx', 'rdhs', 'rlang'))"

# Create app directory
RUN mkdir -p /app

# Copy app files
COPY app.R /app/
COPY R/ /app/R/
COPY www/ /app/www/
COPY assets/ /app/assets/

# Set working directory
WORKDIR /app

# Make port 3838 available
EXPOSE 3838

# Run app
CMD ["R", "-e", "shiny::runApp(host='0.0.0.0', port=3838)"]
