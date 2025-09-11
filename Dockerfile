# Use Posit's (RStudio) Shiny image that supports ARM64
FROM rstudio/r-base:4.3-focal

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

# Install packages in smaller batches to avoid timeouts
RUN R -e "install.packages(c('shiny', 'shinydashboard', 'DT'), repos='https://cloud.r-project.org/')"
RUN R -e "install.packages(c('dplyr', 'httr', 'jsonlite'), repos='https://cloud.r-project.org/')"
RUN R -e "install.packages(c('plotly', 'data.table', 'stringr'), repos='https://cloud.r-project.org/')"
RUN R -e "install.packages(c('shinyWidgets', 'shinyjs', 'shinycssloaders'), repos='https://cloud.r-project.org/')"
RUN R -e "install.packages(c('countrycode', 'RCurl', 'shinyBS'), repos='https://cloud.r-project.org/')"

# Install the more complex packages last
RUN R -e "install.packages(c('rdhs', 'rsdmx'), repos='https://cloud.r-project.org/')"

WORKDIR /app
COPY . /app/
EXPOSE 3838
CMD ["R", "-e", "shiny::runApp(host='0.0.0.0', port=3838)"]
