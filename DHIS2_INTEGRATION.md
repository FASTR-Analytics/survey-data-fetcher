# DHIS2 Integration for Admin Area Name Standardization

## Overview

This integration provides tools to fetch organization unit hierarchies from DHIS2 and use them as the "source of truth" for standardizing admin area names across survey data sources (DHS, MICS, UNWPP).

## Problem Statement

Previously, country and province names were standardized using hardcoded mappings in `R/cleaning_functions.R`:

```r
# Old approach - hardcoded mappings
"Senegal" → "Sénégal"
"Dakar" → "DRS Dakar"
```

**Challenges:**
- Manual maintenance required
- Inconsistencies between survey data and DHIS2
- Difficult to scale across multiple countries

## Solution

Fetch organization units directly from DHIS2 API and build reference datasets:

```r
# New approach - DHIS2 as source of truth
DHIS2 API → CSV reference files → Automated name standardization
```

## Files Created

### Core Scripts
- `R/dhis2_reference_builder.R` - Main API integration and reference builder
- `test_dhis2_demo.R` - Test script for DHIS2 demo instance
- `diagnose_dhis2.R` - Diagnostic tool for troubleshooting

### Configuration
- `config/dhis2_instances_template.yaml` - Multi-instance configuration template
- `.Renviron.template` - Environment variables template for credentials

### Documentation
- `config/DHIS2_SETUP.md` - Detailed setup and usage guide
- `.gitignore` - Updated to protect credentials

## Quick Start

### 1. Set Up Credentials

```bash
# Copy template
cp .Renviron.template .Renviron

# Edit and add your credentials
# For Personal Access Token (recommended):
DHIS2_PAT=d2pat_yourActualTokenHere

# Or for Basic Auth:
DHIS2_USERNAME=your_username
DHIS2_PASSWORD=your_password
```

### 2. Fetch Reference Data

```r
# Load the builder
source("R/dhis2_reference_builder.R")

# Fetch from your DHIS2 instance
build_all_dhis2_references(
  base_url = "https://your-dhis2-instance.org",
  use_pat = TRUE,
  pat_token = Sys.getenv("DHIS2_PAT"),
  levels = 1:3  # Country, Province, District
)
```

This creates CSV files in `assets/`:
- `dhis2_countries.csv`
- `dhis2_provinces.csv`
- `dhis2_districts.csv`

### 3. Use in Cleaning Functions

The cleaning functions automatically load DHIS2 reference data if available:

```r
# In R/cleaning_functions.R
load_dhis2_country_reference()   # Loads assets/dhis2_countries.csv
load_dhis2_province_reference()  # Loads assets/dhis2_provinces.csv
```

Current implementation maintains backward compatibility:
- If DHIS2 reference files exist, they're loaded
- Falls back to hardcoded mappings if files don't exist
- No breaking changes to existing functionality

## Architecture

### Data Flow

```
┌─────────────────────┐
│  DHIS2 Instance(s)  │
│  (API)              │
└──────────┬──────────┘
           │
           │ fetch_dhis2_org_units()
           ▼
┌─────────────────────┐
│  Reference Builder  │
│  (R Script)         │
└──────────┬──────────┘
           │
           │ export_reference_csv()
           ▼
┌─────────────────────┐
│  CSV Reference Files│
│  (assets/*.csv)     │
└──────────┬──────────┘
           │
           │ load_dhis2_*_reference()
           ▼
┌─────────────────────┐
│  Cleaning Functions │
│  (name std.)        │
└─────────────────────┘
```

### Key Functions

**`R/dhis2_reference_builder.R`:**
- `fetch_dhis2_org_units()` - Query DHIS2 API for org units
- `build_country_reference()` - Build level 1 (country) reference
- `build_province_reference()` - Build level 2 (province) reference
- `build_district_reference()` - Build level 3 (district) reference
- `build_facility_reference()` - Build level 4 (facility) reference
- `build_all_dhis2_references()` - Orchestrate full build process
- `build_multi_instance_references()` - Handle multiple DHIS2 instances

**`R/cleaning_functions.R` (updated):**
- `load_dhis2_country_reference()` - Load country reference CSV
- `load_dhis2_province_reference()` - Load province reference CSV
- `get_country_name_mappings()` - Enhanced to try DHIS2 first

## Multiple DHIS2 Instances

For organizations working across multiple countries with separate DHIS2 instances:

1. **Configure instances** in `config/dhis2_instances.yaml`:

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

2. **Fetch from all instances**:

```r
library(yaml)

config <- read_yaml("config/dhis2_instances.yaml")
active_instances <- config$instances[sapply(config$instances, function(x) x$enabled)]

build_multi_instance_references(
  instances = active_instances,
  levels = 1:3,
  output_dir = "assets"
)
```

This creates:
- Instance-specific files: `dhis2_senegal_countries.csv`, `dhis2_nigeria_countries.csv`
- Combined file: `dhis2_combined_countries.csv`

## Security & Best Practices

### Authentication

**Personal Access Tokens (Recommended)**

- More secure than username/password
- Can set expiration dates and IP restrictions
- Easier to revoke if compromised

**Creating a PAT:**
1. Log into DHIS2 → Profile → Personal Access Tokens
2. Generate new token (Server/script context)
3. Set constraints (expiry, IP, HTTP methods: GET only)
4. Copy token (shown only once!)
5. Add to `.Renviron`: `DHIS2_PAT=d2pat_token`

### Credential Management

✅ **DO:**
- Use environment variables for credentials
- Add `.Renviron` and `config/dhis2_instances.yaml` to `.gitignore`
- Use PAT tokens instead of passwords
- Rotate tokens regularly (quarterly)
- Create read-only DHIS2 users for API access

❌ **DON'T:**
- Commit credentials to git
- Share `.Renviron` or config files with credentials
- Use admin accounts for API access
- Store credentials in code

## Maintenance

### Update Frequency

DHIS2 organization units change infrequently:

- **Monthly** - For active development
- **Quarterly** - For production systems
- **Annually** - For stable configurations
- **On-demand** - When boundaries change

### Automation

Schedule reference updates:

```r
# Script: scheduled_dhis2_update.R
source("R/dhis2_reference_builder.R")

build_all_dhis2_references(
  base_url = Sys.getenv("DHIS2_URL"),
  use_pat = TRUE,
  pat_token = Sys.getenv("DHIS2_PAT"),
  levels = 1:3
)

# Commit updated files
system("git add assets/dhis2_*.csv")
system("git commit -m 'Update DHIS2 reference data'")
```

Add to cron (Linux/Mac) or Task Scheduler (Windows).

## Troubleshooting

### Demo Instance Returns No Data

The DHIS2 demo instance (play.dhis2.org/dev) may not have organization units configured. This is expected - the code is designed for production DHIS2 instances with actual country/province data.

**Solution:** Use your organization's DHIS2 instance instead.

### Authentication Errors

```
Error: DHIS2 API request failed with status 401
```

**Causes:**
- Invalid credentials
- Expired PAT token
- IP restrictions on token

**Solutions:**
- Verify credentials in `.Renviron`
- Generate new PAT token
- Check token constraints

### No Organization Units Returned

```
Warning: No organization units returned from DHIS2 API
```

**Causes:**
- User lacks permission to read org units
- Org units not configured in DHIS2
- Wrong instance URL

**Solutions:**
- Check user permissions in DHIS2
- Verify instance URL
- Run diagnostic: `Rscript diagnose_dhis2.R`

### Connection Timeouts

**Causes:**
- Network connectivity issues
- Firewall blocking API access
- DHIS2 server down

**Solutions:**
- Check network connection
- Test URL in browser
- Contact DHIS2 administrator

## Diagnostic Tools

```r
# Run diagnostic to check DHIS2 connectivity
Rscript diagnose_dhis2.R
```

This checks:
- API connectivity
- Authentication
- Organization unit levels available
- Sample org units

## Next Steps

1. **Get DHIS2 Credentials**
   - Contact your DHIS2 administrator
   - Request API access (read-only sufficient)
   - Generate Personal Access Token

2. **Fetch Reference Data**
   - Set up `.Renviron` with credentials
   - Run `build_all_dhis2_references()`
   - Review generated CSV files

3. **Integrate with Cleaning**
   - Customize `get_country_name_mappings()` to use DHIS2 data
   - Add fuzzy matching for name variations
   - Test with real survey data

4. **Deploy**
   - Add reference CSVs to git (data only, not credentials!)
   - Update deployment workflow
   - Document update schedule

## Resources

- **DHIS2 API Documentation:** https://docs.dhis2.org/en/develop/using-the-api/
- **DHIS2 Community:** https://community.dhis2.org/
- **Setup Guide:** See `config/DHIS2_SETUP.md`

## Support

For issues with this integration:
- Check `config/DHIS2_SETUP.md` for detailed instructions
- Run `diagnose_dhis2.R` to troubleshoot connectivity
- Review `PROGRESS.md` for known issues

For DHIS2-specific questions:
- Contact your DHIS2 administrator
- Visit DHIS2 Community forums
