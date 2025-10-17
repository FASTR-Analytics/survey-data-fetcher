# Use rocker/shiny-verse which includes tidyverse packages already installed
FROM rocker/shiny-verse:4.3.1

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
    libgit2-dev \
    git \
    && rm -rf /var/lib/apt/lists/*

# Set CRAN repo to use binary packages (faster installation)
RUN echo "options(repos = c(CRAN = 'https://packagemanager.posit.co/cran/__linux__/jammy/latest'), download.file.method = 'libcurl')" >> /usr/local/lib/R/etc/Rprofile.site

# Install R packages in groups to improve caching and error handling
# shinydashboard, DT, dplyr, httr, jsonlite are commonly available as binaries
RUN R -e "install.packages(c('shinydashboard', 'DT', 'httr', 'jsonlite', 'countrycode', 'data.table'), Ncpus = 2)"

# Install plotting and UI packages
RUN R -e "install.packages(c('plotly', 'shinyWidgets', 'shinycssloaders', 'shinyBS', 'shinyjs'), Ncpus = 2)"

# Install data/file handling packages
RUN R -e "install.packages(c('RCurl', 'stringr', 'readxl', 'rsdmx'), Ncpus = 2)"

# Install rdhs separately with explicit validation and error output
RUN R -e "install.packages('rdhs', Ncpus = 2)" && \
    R -e "if (!require('rdhs', quietly = TRUE)) { message('rdhs installation check failed'); q(status = 1) }"

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
