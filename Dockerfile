# Use official R base image which supports ARM/Apple Silicon
FROM r-base:latest

# Install system dependencies for R packages
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    libudunits2-dev \
    && rm -rf /var/lib/apt/lists/*

# Install R packages with error checking
RUN R -e "options(warn=2); \
    pkgs <- c('shiny', 'shinydashboard', 'DT', 'dplyr', 'rdhs', \
              'rsdmx', 'httr', 'jsonlite', 'countrycode', 'data.table', \
              'plotly', 'shinyWidgets', 'RCurl', 'shinycssloaders', \
              'shinyBS', 'stringr', 'shinyjs', 'readxl'); \
    install.packages(pkgs, repos='https://cloud.r-project.org/', dependencies=TRUE); \
    if (!all(pkgs %in% installed.packages()[,'Package'])) { \
        stop('Failed to install all required packages'); \
    }"

# Create app directory
RUN mkdir -p /app

# Copy app files
COPY app.R /app/
COPY R/ /app/R/
COPY www/ /app/www/
COPY assets/ /app/assets/

# Set working directory
WORKDIR /app

# Expose port
EXPOSE 3838

# Run the app
CMD ["R", "-e", "shiny::runApp(host='0.0.0.0', port=3838)"]
