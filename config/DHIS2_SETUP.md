# DHIS2 Integration Setup Guide

This guide explains how to set up and use the DHIS2 reference dataset builder to fetch standardized admin area names from DHIS2 instances.

## Overview

The DHIS2 reference builder fetches organization unit hierarchies from DHIS2 and creates CSV reference files for standardizing country and province names in your survey data.

## Quick Start

### 1. Set Up Credentials

**Option A: Environment Variables (Recommended)**

Add to your `.Renviron` file:

```bash
# For Basic Authentication
DHIS2_USERNAME=your_username
DHIS2_PASSWORD=your_password

# For Personal Access Token (PAT) - Recommended
DHIS2_PAT=d2pat_yourTokenHere

# For multiple instances, use instance-specific variables
DHIS2_SENEGAL_PAT=d2pat_senegalToken
DHIS2_NIGERIA_PAT=d2pat_nigeriaToken
```

Restart R after editing `.Renviron`.

**Option B: Direct Parameters (For Testing)**

Pass credentials directly to functions (not recommended for production).

### 2. Test with DHIS2 Demo Instance

```r
# Load the builder script
source("R/dhis2_reference_builder.R")

# Fetch from DHIS2 demo server (public access)
build_all_dhis2_references(
  base_url = "https://play.dhis2.org/dev",
  username = "admin",
  password = "district",
  levels = 1:3,
  output_dir = "assets"
)
```

This will create three CSV files in the `assets/` directory:
- `dhis2_countries.csv`
- `dhis2_provinces.csv`
- `dhis2_districts.csv`

### 3. Connect to Your DHIS2 Instance

```r
# Using Personal Access Token (recommended)
build_all_dhis2_references(
  base_url = "https://your-dhis2-instance.org",
  use_pat = TRUE,
  pat_token = Sys.getenv("DHIS2_PAT"),
  levels = 1:3
)
```

### 4. Multiple Instances (Advanced)

If you work with multiple country DHIS2 instances:

1. Configure `config/dhis2_instances.yaml`:

```yaml
instances:
  senegal:
    url: "https://dhis2.sante.gouv.sn"
    use_pat: true
    pat_token: "${DHIS2_SENEGAL_PAT}"
    enabled: true

  nigeria:
    url: "https://dhis2.ng"
    use_pat: true
    pat_token: "${DHIS2_NIGERIA_PAT}"
    enabled: true
```

2. Run the multi-instance builder:

```r
library(yaml)

# Load configuration
config <- read_yaml("config/dhis2_instances.yaml")
instances <- config$instances[sapply(config$instances, function(x) x$enabled)]

# Build from all instances
build_multi_instance_references(
  instances = instances,
  levels = 1:3,
  output_dir = "assets"
)
```

## Authentication Methods

### Personal Access Tokens (PAT) - Recommended

PAT is more secure than Basic Authentication. To create a PAT:

1. Log into your DHIS2 instance
2. Go to your profile → "Personal access tokens"
3. Click "Generate new token"
4. Choose "Server/script context"
5. Set constraints:
   - **Expiry time**: 90 days or more
   - **HTTP methods**: GET (read-only access)
   - **IP addresses**: Optional, your server IP if known
6. Copy the token (shows only once!)
7. Add to `.Renviron`: `DHIS2_PAT=d2pat_yourTokenHere`

### Basic Authentication

Less secure, but simpler for testing:

```r
build_all_dhis2_references(
  base_url = "https://your-instance.org",
  username = "your_username",
  password = "your_password"
)
```

## Understanding Organization Unit Levels

DHIS2 organizes administrative areas hierarchically:

- **Level 1** - Country/National (e.g., "Senegal", "Nigeria")
- **Level 2** - Province/Region (e.g., "Dakar Region", "Lagos State")
- **Level 3** - District/Department (e.g., "Dakar District")
- **Level 4** - Facilities/Sub-districts (e.g., "Dakar Central Hospital")

Check your instance's level structure:

```r
source("R/dhis2_reference_builder.R")

auth <- dhis2_auth("username", "password")
levels <- fetch_dhis2_org_unit_levels("https://your-instance.org", auth)
print(levels)
```

## Output Files

Reference CSV files have these columns:

### Countries (`dhis2_countries.csv`)
- `dhis2_id` - DHIS2 internal ID
- `dhis2_name` - Official name in DHIS2
- `dhis2_display_name` - Display name
- `country_code` - ISO country code (if available)
- `level` - Organization unit level (1)

### Provinces (`dhis2_provinces.csv`)
- `dhis2_id` - DHIS2 internal ID
- `dhis2_name` - Province name in DHIS2
- `dhis2_display_name` - Display name
- `country_id` - Parent country DHIS2 ID
- `country_name` - Parent country name
- `province_code` - Province code (if available)
- `level` - Organization unit level (2)

### Districts (`dhis2_districts.csv`)
- `dhis2_id` - DHIS2 internal ID
- `dhis2_name` - District name in DHIS2
- `dhis2_display_name` - Display name
- `district_code` - District code (if available)
- `province_id` - Parent province DHIS2 ID
- `province_name` - Parent province name
- `country_id` - Parent country DHIS2 ID
- `country_name` - Parent country name
- `level` - Organization unit level (3)

## Updating the Cleaning Functions

Once you have DHIS2 reference files, update your cleaning functions to use them:

```r
# Load DHIS2 reference data
dhis2_countries <- read.csv("assets/dhis2_countries.csv", stringsAsFactors = FALSE)
dhis2_provinces <- read.csv("assets/dhis2_provinces.csv", stringsAsFactors = FALSE)

# Create name mappings from DHIS2 data
get_country_name_mappings_from_dhis2 <- function() {
  # Create mapping from survey names to DHIS2 names
  # This replaces the hardcoded mappings
  # Add your custom logic here based on your survey data
}
```

## Maintenance

### How Often to Update

DHIS2 organization units change infrequently. Update reference files:
- **Quarterly** - For active projects
- **Annually** - For stable configurations
- **On-demand** - When you know admin boundaries changed

### Automation

Add to your workflow:

```r
# Script: update_dhis2_references.R
# Run via cron job or scheduled task

source("R/dhis2_reference_builder.R")

# Update from all configured instances
build_all_dhis2_references(
  base_url = Sys.getenv("DHIS2_URL"),
  use_pat = TRUE,
  pat_token = Sys.getenv("DHIS2_PAT"),
  levels = 1:3
)

# Commit updated files to git
system("git add assets/dhis2_*.csv")
system("git commit -m 'Update DHIS2 reference data'")
```

## Troubleshooting

### Error: Authentication failed

- Check credentials are correct
- Verify PAT token hasn't expired
- Ensure your IP isn't blocked by DHIS2 firewall

### Error: No organization units returned

- Check the instance URL is correct
- Verify you have permission to read org units
- Try fetching level metadata first to see what's available

### Error: Connection timeout

- Check network connectivity
- Verify DHIS2 instance is accessible
- Try increasing timeout in httr::GET()

### Names don't match survey data

- DHIS2 names might use different spelling/accents
- Add fuzzy matching or synonym tables
- Document known variations in your cleaning functions

## Security Best Practices

1. **Never commit credentials to git**
   - Add `.Renviron` and `dhis2_instances.yaml` to `.gitignore`
   - Use environment variables for all sensitive data

2. **Use Personal Access Tokens**
   - More secure than username/password
   - Can set expiration and IP restrictions
   - Easier to revoke if compromised

3. **Limit API permissions**
   - Create DHIS2 user with read-only access
   - Restrict to metadata only, not patient data

4. **Rotate tokens regularly**
   - Set expiration dates on PAT tokens
   - Update tokens quarterly

## Support

For DHIS2 API documentation:
- https://docs.dhis2.org/en/develop/using-the-api/dhis-core-version-239/
- https://community.dhis2.org/

For issues with this integration:
- Check PROGRESS.md for known issues
- Contact your DHIS2 administrator for instance-specific questions
