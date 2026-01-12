# Deployment

## Hugging Face Spaces

The app is deployed as a Docker-based Hugging Face Space.

### Live URL

**[https://huggingface.co/spaces/CIJBoulange/survey-data-fetcher](https://huggingface.co/spaces/CIJBoulange/survey-data-fetcher)**

---

## Dockerfile Configuration

The app uses `rocker/shiny-verse` as the base image:

```dockerfile
# Base image with R and tidyverse
FROM rocker/shiny-verse:4.3.1

# System dependencies
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libsodium-dev \
    libgit2-dev \
    git

# R packages - Core
RUN R -e "install.packages(c('shinydashboard', 'DT', 'httr',
          'jsonlite', 'countrycode', 'data.table'))"

# R packages - UI
RUN R -e "install.packages(c('plotly', 'shinyWidgets',
          'shinycssloaders', 'shinyBS', 'shinyjs'))"

# R packages - Data handling
RUN R -e "install.packages(c('RCurl', 'stringr', 'readxl',
          'rsdmx', 'stringdist', 'base64enc'))"

# R packages - DHS
RUN R -e "install.packages('rdhs')"

# Copy app files
COPY app.R /app/
COPY R/ /app/R/
COPY www/ /app/www/
COPY assets/ /app/assets/

WORKDIR /app
EXPOSE 3838

CMD ["R", "-e", "shiny::runApp(host='0.0.0.0', port=3838)"]
```

---

## Setting Up a New Space

### 1. Create the Space

1. Go to [huggingface.co/new-space](https://huggingface.co/new-space)
2. Choose:
   - **Space name**: `survey-data-fetcher`
   - **SDK**: Docker
   - **Visibility**: Public or Private

### 2. Push Code

```bash
# Clone the space
git clone https://huggingface.co/spaces/YOUR_USERNAME/survey-data-fetcher

# Copy files
cp -r /path/to/survey_data_fetcher/* survey-data-fetcher/

# Push
cd survey-data-fetcher
git add .
git commit -m "Initial deployment"
git push
```

### 3. Add Secrets

1. Go to Space **Settings**
2. Find **"Variables and secrets"**
3. Add these secrets:

| Secret Name | Value | Purpose |
|-------------|-------|---------|
| `UNWPP_TOKEN` | Your UN Data Portal token | Fetch UNWPP population data |
| `GITHUB_TOKEN` | Your GitHub PAT | Push data to GitHub repo |

---

## Updating the Deployment

### From Local

```bash
cd /path/to/survey_data_fetcher
git add .
git commit -m "Update description"
git push huggingface main
```

### Remote Configuration

You should have two remotes:

```bash
git remote -v
# origin     https://github.com/FASTR-Analytics/survey-data-fetcher.git
# huggingface https://huggingface.co/spaces/CIJBoulange/survey-data-fetcher
```

---

## Monitoring

### Build Logs

- Go to your Space page
- Click on **"Building"** or check the **Logs** tab
- Watch for errors during package installation

### Common Issues

#### Package installation fails

Check the Dockerfile has all required packages. Add missing packages:

```dockerfile
RUN R -e "install.packages(c('package_name'))"
```

#### App crashes on startup

Check the logs for missing libraries. The error will look like:

```
Error in library(package_name) : there is no package called 'package_name'
```

**Solution:** Add the missing package to the Dockerfile and rebuild.

#### API fetch fails

- **DHS**: No token required, but rate limits may apply
- **UNICEF**: No token required
- **UNWPP**: Requires `UNWPP_TOKEN` secret

#### GitHub push fails

Verify your `GITHUB_TOKEN` secret:

1. Token has `repo` or `contents:write` permission
2. Token has access to the target repository
3. Secret name is exactly `GITHUB_TOKEN`
4. Token hasn't expired

#### Data not appearing after push

- Check the commit was successful on GitHub
- Verify you pushed to the correct branch (`main`)
- Ensure the data passed validation (no unmatched admin areas)

---

## Local Development

### Run with Docker

```bash
cd survey_data_fetcher
docker build -t survey-fetcher .
docker run -p 3838:3838 survey-fetcher
```

Open: http://localhost:3838

### Run without Docker

```bash
cd survey_data_fetcher
R -e "shiny::runApp(host='0.0.0.0', port=3838)"
```

---

## File Structure

```
survey_data_fetcher/
├── app.R                    # Main Shiny app
├── Dockerfile               # Docker configuration
├── R/
│   ├── cleaning_functions.R # Data cleaning
│   ├── data_functions.R     # API fetching
│   ├── indicator_mappings.R # ID mappings
│   ├── integration_functions.R # GitHub sync
│   └── ui_components.R      # UI modules
├── www/
│   ├── custom.css           # Styling
│   └── progress.js          # Progress indicators
├── assets/                  # Geographic data
├── docs/                    # This documentation
└── mkdocs.yml              # MkDocs config
```
