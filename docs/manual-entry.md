# Manual Data Entry

The Manual Entry tab allows you to add survey data manually when API fetching is not available or when you need to enter data from reports, publications, or other sources.

## Why Manual Entry?

- **No API Available**: Some data sources don't provide API access
- **Report Data**: Enter values from PDF reports or publications
- **Admin Data**: Add administrative data from health ministries
- **Quick Additions**: Add individual data points without full fetch workflow

## How It Works

### Step 1: Select Indicator Category

First, choose the category of indicator you want to enter:

| Category | Example Indicators |
|----------|-------------------|
| **ANC & Maternal Health** | anc1, anc4, delivery, pnc1 |
| **Immunization** | bcg, penta1, penta3, measles1, polio3 |
| **Child Health & Nutrition** | stunting, wasting, underweight |
| **Mortality Rates** | imr, nmr, u5mr, mmr |
| **Family Planning** | contraceptive_modern, unmet_need |
| **Malaria** | itn_access, itn_use |
| **HIV/AIDS** | hiv_test, hiv_knowledge |
| **Population & Demographics** | poptot, livebirth, tfr |

### Step 2: Select Specific Indicator

After choosing a category, the indicator dropdown filters to show only relevant indicators with standardized codes.

### Step 3: Enter Geographic Information

- **Country**: Select from available countries (loaded from backbone files)
- **Region/Province**: Choose specific region or "NATIONAL" for country-level data

### Step 4: Enter Value

Enter the survey value. The form provides guidance based on indicator type:

- **Percentages**: Enter as decimal (0.0 to 1.0). Example: 85% = 0.85
- **Rates**: Enter the rate value directly (e.g., deaths per 1,000)
- **Population**: Enter as thousands (e.g., 1500 for 1.5 million)

### Step 5: Source Information

- **Source**: Select the data source (DHS, MICS, WUENIC, Admin, Other)
- **Survey Type**: household, modeled, admin, or other
- **Source Detail**: Add specifics like "DHS 2023" or "Ministry Report Q3"

## Staging Area

Before committing to the database workflow:

1. Click **"Add to Staging"** to preview your entry
2. Review all 13 fields in the staging area
3. Add multiple entries if needed
4. Click **"Commit All to Cleaned Data"** when ready

## Database Schema

Manual entries follow the same 13-column schema as fetched data:

| Column | Description |
|--------|-------------|
| admin_area_1 | Country name |
| admin_area_2 | Region/province or "NATIONAL" |
| year | Survey year |
| indicator_id | Same as common_id for manual entry |
| indicator_common_id | Standardized indicator code |
| indicator_type | percent, rate, or population_estimate |
| survey_value | The numeric value |
| source | Data source (DHS, MICS, etc.) |
| source_detail | Specific source info |
| survey_type | household, modeled, admin, other |
| country_name | Same as admin_area_1 |
| iso2_code | Auto-generated |
| iso3_code | Auto-generated |

## After Manual Entry

Once committed, manually entered data flows through the same workflow as fetched data:

1. Goes to **cleaned_data**
2. Available in **Data Review** tab for comparison
3. Proceeds through **Database Integration** for validation and duplicate checking
4. Can be pushed to GitHub

## Tips

- Use consistent source naming (e.g., always "Admin" not "Administrative")
- Include year in source_detail for clarity
- Review entries in staging before committing
- Use Data Review tab to verify entries against existing data
