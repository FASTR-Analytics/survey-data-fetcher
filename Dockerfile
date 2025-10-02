# Use rocker/shiny base image which has optimized R and system dependencies
FROM rocker/shiny:latest

# Install additional system dependencies
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    libudunits2-dev \
    libfontconfig1-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    && rm -rf /var/lib/apt/lists/*

# Install R packages
RUN install2.r --error --skipinstalled \
    shinydashboard \
    DT \
    dplyr \
    rdhs \
    rsdmx \
    httr \
    jsonlite \
    countrycode \
    data.table \
    plotly \
    shinyWidgets \
    RCurl \
    shinycssloaders \
    shinyBS \
    stringr \
    shinyjs \
    readxl

# Remove default shiny app
RUN rm -rf /srv/shiny-server/*

# Copy app files to shiny server directory
COPY app.R /srv/shiny-server/
COPY R/ /srv/shiny-server/R/
COPY www/ /srv/shiny-server/www/
COPY assets/ /srv/shiny-server/assets/

# Set permissions
RUN chown -R shiny:shiny /srv/shiny-server

# Expose port
EXPOSE 3838

# Run shiny server
CMD ["/init"]
