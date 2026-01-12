# Cleaning & Processing

## Overview

The cleaning process standardizes data from different sources into a unified format compatible with the FASTR Analytics Platform.

## Cleaning Steps

### 1. Column Standardization

All data is transformed to a consistent column structure:

| Column | Description | Example |
|--------|-------------|---------|
| `admin_area_1` | Country name | "Nigeria" |
| `admin_area_2` | Province/region or "NATIONAL" | "Lagos" or "NATIONAL" |
| `year` | Integer year | 2023 |
| `indicator_id` | Original indicator code | "RH_ANCP_W_SKP" |
| `indicator_common_id` | Standardized ID | "anc1" |
| `indicator_type` | Type of measure | "percent", "rate", "number" |
| `survey_value` | Numeric value | 0.85 |
| `source` | Data source | "DHS", "MICS", "UNWPP" |
| `source_detail` | Specific source ID | "NG2018DHS" |
| `survey_type` | Survey category | "household", "modeled" |
| `country_name` | Full country name | "Nigeria" |
| `iso2_code` | ISO2 country code | "NG" |
| `iso3_code` | ISO3 country code | "NGA" |

### 2. Percentage Normalization

All percentage values are converted to **0-1 decimal scale**:

- Input: `85%` or `85`
- Output: `0.85`

This ensures consistency across sources that may report percentages differently.

### 3. Indicator ID Mapping

Original indicator IDs are mapped to standardized `indicator_common_id` values:

```
DHS:    RH_ANCP_W_SKP  → anc1
UNICEF: MNCH_ANC1      → anc1
MICS:   MNCH_ANC1      → anc1
```

See [Indicator Reference](indicators.md) for complete mappings.

---

## FASTR Name Standardization

When **"Apply FASTR name standardization"** is checked, country and province names are harmonized to match the FASTR Analytics Platform naming conventions.

### Country Name Mappings

| Original | Standardized |
|----------|--------------|
| Guinea | Guinée |
| Cote d'Ivoire | Côte d'Ivoire |
| Democratic Republic of the Congo | DRC |

### Province Name Mappings (Examples)

**Nigeria:**
```
Abia → ab Abia State
Lagos → la Lagos State
```

**Senegal:**
```
Dakar → DRS Dakar
Thiès → DRS Thiès
```

**Guinea:**
```
Conakry → DSV Conakry
Boké → DPS Boké
```

These mappings ensure compatibility with DHIS2 naming conventions used in the FASTR platform.

---

## How to Clean Data

### Clean Latest Fetch

1. Go to **"Clean & Process"** tab
2. Ensure **"Apply FASTR name standardization"** is checked
3. Click **"Clean Latest Data"**
4. View results in "Cleaned Data Preview"

### Clean All Cart Data

1. Accumulate multiple datasets in your cart
2. Go to **"Clean & Process"** tab
3. Click **"Clean All Cart Data"**
4. All datasets are cleaned and combined

---

## Output Format

After cleaning, data is ready for:

- **Visualization** in the app
- **Download** as CSV
- **Integration** into the unified database

Example cleaned row:

```csv
admin_area_1,admin_area_2,year,indicator_id,indicator_common_id,indicator_type,survey_value,source,source_detail,survey_type,country_name,iso2_code,iso3_code
Nigeria,la Lagos State,2018,RH_ANCP_W_SKP,anc1,percent,0.92,DHS Sub-national,NG2018DHS,household,Nigeria,NG,NGA
```
