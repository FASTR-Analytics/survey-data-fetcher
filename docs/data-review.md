# Data Review

The Data Review tab provides visual comparison between your fetched/entered data and the existing database, helping you identify what's new and what already exists.

## Purpose

- **Visual Comparison**: See database vs. new data overlaid on the same chart
- **Duplicate Identification**: Quickly spot records that already exist
- **Quality Check**: Verify new data aligns with historical trends
- **Regional Analysis**: Compare national and subnational data

## How to Use

### Step 1: Load the Database

Click **"Load Database from GitHub"** to fetch the current database for comparison.

!!! note
    The database is filtered to only include countries present in your cleaned data.

### Step 2: Select Filters

- **Country**: Choose the country to review
- **Regions**: Select specific regions or check "Include NATIONAL" for country-level data
- **Indicator**: Select the indicator to visualize

### Step 3: Generate Plot

Click **"Generate Plot"** to create the comparison chart.

## Reading the Chart

### Database Data (Existing)

| Level | Line Style | Marker |
|-------|------------|--------|
| **National** | Solid line (thick) | Circle |
| **Subnational** | Dotted line (thin) | Triangle |

Colors indicate source:
- **DHS**: Teal (#0f706d)
- **MICS**: Blue (#3498db)
- **WUENIC**: Purple (#9b59b6)
- **UNWPP**: Orange (#f39c12)
- **WHO**: Green (#1abc9c)

### New/Fetched Data

| Level | Line Style | Marker | Color |
|-------|------------|--------|-------|
| **National** | Dashed | Diamond | Red |
| **Subnational** | Dash-dot | Square | Orange |

## Duplicate Summary

Below the filters, a summary shows:

- **X already in database**: Records that match existing entries
- **Y are new**: Records not found in database

This helps you understand how much of your fetched data is new vs. duplicate.

## Summary Tables

Two tables show the breakdown:

### Database Records Table
Shows existing records for your selection, including:
- Region, Year, Value, Source

### New Data Table
Shows your fetched/entered data for comparison

## Workflow Integration

The Data Review tab fits into the workflow between cleaning and database integration:

```
Fetch/Enter → Clean → **Review** → Validate → Deduplicate → Push
```

Use this step to:

1. **Verify data quality** before integration
2. **Identify gaps** in existing coverage
3. **Spot anomalies** in new data
4. **Plan updates** - decide which records to replace

## Tips

- Review data before running duplicate check
- Use to verify manual entries match expected patterns
- Compare multiple regions to spot inconsistencies
- National data (solid lines) should generally be visible above subnational (dotted lines)
