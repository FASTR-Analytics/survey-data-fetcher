# Use R base image
FROM r-base:4.3.1

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    libsodium-dev \
    git \
    && rm -rf /var/lib/apt/lists/*

# Install R packages (combining to reduce layers)
RUN R -e "install.packages(c('shiny', 'shinydashboard', 'DT', 'dplyr', 'httr', 'jsonlite', 'countrycode', 'data.table', 'plotly', 'shinyWidgets', 'RCurl', 'shinycssloaders', 'shinyBS', 'stringr', 'shinyjs', 'readxl', 'rsdmx'), repos='https://cloud.r-project.org/', dependencies=TRUE)"

# Install rdhs separately (requires libsodium-dev)
RUN R -e "install.packages('rdhs', repos='https://cloud.r-project.org/', dependencies=TRUE)"

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
