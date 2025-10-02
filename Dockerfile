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
    && rm -rf /var/lib/apt/lists/*

# Install R packages one by one to identify failures
RUN R -e "install.packages('shiny', repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('shinydashboard', repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('DT', repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('dplyr', repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('rdhs', repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('rsdmx', repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('httr', repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('jsonlite', repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('countrycode', repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('data.table', repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('plotly', repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('shinyWidgets', repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('RCurl', repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('shinycssloaders', repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('shinyBS', repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('stringr', repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('shinyjs', repos='https://cloud.r-project.org/')"
RUN R -e "install.packages('readxl', repos='https://cloud.r-project.org/')"

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
