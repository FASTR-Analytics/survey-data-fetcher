# Cleaning & Processing

## Overview

The cleaning process standardizes data from different sources into a unified format compatible with the FASTR Analytics Platform.

---

## Data Filtering Rules

The cleaning functions apply several filters to ensure consistent, comparable data.

### Sex Disaggregation

**We keep totals only, not sex-disaggregated values.**

- UNICEF data: Filters to `SEX = "_T"` (total) or `NA`
- UNWPP data: Filters to `sex = "Both sexes"` for population and mortality indicators
- DHS data: Uses `IsPreferred = 1` which typically returns the combined value

This means if an indicator has separate Male/Female values, we take the combined "Both sexes" or "Total" value.

### Preferred Values (DHS)

DHS data includes multiple breakdowns (by age, education, wealth, etc.). We filter to `IsPreferred = 1` to get the standard reported value rather than disaggregated subgroups.

### Variant Selection (UNWPP)

UNWPP provides multiple projection variants. We use `variant = "Median"` for all indicators to get the central estimate.

### Age Filters (UNWPP)

- Women of reproductive age: `ageLabel = "15-49"`
- Under-1 population: `ageLabel = "0"`
- Under-5 population: `ageLabel = "0-4"` (or sum of 0,1,2,3,4)

---

## Indicators Outside the Predefined List

The app has predefined mappings for common indicators (anc1, penta1, bcg, etc.). But you can fetch **any indicator** from the data sources.

### What Happens to Unmapped Indicators

When you select an indicator that isn't in our predefined list:

1. The app tries to **auto-generate** a meaningful `indicator_common_id` based on the indicator label
2. It looks for patterns like "DTP", "measles", "ANC", "mortality" etc.
3. If no pattern matches, it creates an ID from the original indicator code (e.g., `ch_vacc_c_xyz` → `ch_vacc_c_xyz`)

### Example Auto-Generated IDs

| Original Label | Auto-Generated ID |
|----------------|-------------------|
| "DTP3 coverage" | `penta3` |
| "Measles 2nd dose" | `measles2` |
| "Some new indicator XYZ" | `some_new_indicator_xyz` |

### Adding New Indicator Mappings

If you regularly use an indicator that isn't mapped, contact the FASTR team to add it to the predefined mapping list. This ensures consistent naming across all users.

---

## Column Standardization

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

---

## Percentage Normalization

All percentage values are converted to **0-1 decimal scale**:

- Input: `85%` or `85`
- Output: `0.85`

This ensures consistency across sources that may report percentages differently.

**Indicators treated as percentages:**

- Coverage indicators: anc1, anc4, delivery, pnc1, bcg, penta1-3, measles1-2, polio1-3, rotavirus, pcv, iptp1-3, fp, etc.

**Indicators kept as rates (per 1,000):**

- Mortality rates: imr, nmr, u5mr, mmr
- Demographic rates: crude_birth_rate, total_fertility_rate

**Indicators kept as counts:**

- Population estimates: poptot, livebirth, womenrepage, totu1pop, totu5pop

---

## Indicator ID Mapping

Original indicator IDs are mapped to standardized `indicator_common_id` values:

```
DHS:    RH_ANCP_W_SKP  → anc1
UNICEF: MNCH_ANC1      → anc1
WUENIC: DTP3           → penta3
UNWPP:  22             → imr
```

See [Indicator Reference](indicators.md) for complete mappings.

---

## FASTR Name Standardization

When **"Apply FASTR name standardization"** is checked, country and province names are harmonized to match DHIS2 naming conventions used in FASTR.

### Country Name Mappings

| Original | Standardized |
|----------|--------------|
| Guinea | Guinée |
| Nigeria | ng Federal Government |
| Senegal | Sénégal |
| Cameroon | Ministere de la Sante Publique |
| DRC | République Démocratique du Congo |

### Province Name Mappings (Examples)

**Nigeria:**
```
Abia → ab Abia State
Lagos → la Lagos State
FCT Abuja → fc Federal Capital Territory
```

**Senegal:**
```
Dakar → DRS Dakar
Thiès → DRS Thies
```

**Guinea:**
```
Conakry → DSV Conakry
Boké → IRS Boké
```

These mappings ensure compatibility with DHIS2 organisation unit names.

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
admin_area_1,admin_area_2,year,indicator_common_id,indicator_type,survey_value,source
ng Federal Government,la Lagos State,2018,anc1,percent,0.92,DHS Sub-national
```
