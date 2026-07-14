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
RUN R -e "install.packages(c('RCurl', 'stringr', 'readxl', 'rsdmx', 'stringdist', 'base64enc'), Ncpus = 2)"

# Install rdhs separately with explicit validation and error output
RUN R -e "install.packages('rdhs', Ncpus = 2)" && \
    R -e "if (!require('rdhs', quietly = TRUE)) { message('rdhs installation check failed'); q(status = 1) }"

# HuggingFace Spaces runs the container as UID 1000, NOT root. Without a matching user
# and a writable HOME, R cannot write its temp/cache and the app never binds — the Space
# then sits in APP_STARTING forever with no output. Create the user HF expects.
RUN useradd -m -u 1000 user || true
ENV HOME=/home/user \
    PATH=/home/user/.local/bin:$PATH \
    R_LIBS_USER=/home/user/R \
    TMPDIR=/tmp
RUN mkdir -p $HOME/app $HOME/R && chown -R user:user $HOME

# Copy app files as the runtime user so they are readable/writable at UID 1000
COPY --chown=user:user app.R   $HOME/app/
COPY --chown=user:user R/      $HOME/app/R/
COPY --chown=user:user www/    $HOME/app/www/
COPY --chown=user:user assets/ $HOME/app/assets/

USER user
WORKDIR $HOME/app

# Make port 3838 available (must match `app_port` in README.md front-matter)
EXPOSE 3838

# Run app. Unbuffered + explicit appDir so failures surface in the container log
# instead of the Space hanging silently on "Starting...".
CMD ["R", "--no-save", "--no-restore", "-e", \
     "options(shiny.host='0.0.0.0', shiny.port=3838); \
      message('>>> starting shiny from ', getwd()); \
      shiny::runApp('.', host='0.0.0.0', port=3838, launch.browser=FALSE)"]
