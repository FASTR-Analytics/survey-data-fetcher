# Database Integration

The Database Integration feature allows you to append cleaned data to the unified survey database stored on GitHub.

## GitHub Repository

- **Repository**: `FASTR-Analytics/modules`
- **Branch**: `main`
- **Files**:
  - `survey_data_unified.csv` - Survey indicators
  - `population_estimates_only.csv` - Population estimates

## Workflow Overview

```
┌─────────────────┐
│ 1. Pull from    │
│    GitHub       │
└────────┬────────┘
         ▼
┌─────────────────┐
│ 2. Validate     │
│    Names        │
└────────┬────────┘
         ▼
┌─────────────────┐
│ 3. Check        │
│    Duplicates   │
└────────┬────────┘
         ▼
┌─────────────────┐
│ 4. Append &     │
│    Push         │
└─────────────────┘
```

---

## Step 1: Pull from GitHub

Click **"Pull Latest from GitHub"** to fetch the current database.

This ensures you're working with the most recent version, especially important for collaborative workflows.

**Success message:**
```
Successfully pulled from GitHub!
Survey records: 19,419
Population records: 1,318
```

---

## Step 2: Validate Admin Area Names

Click **"Validate Admin Area Names"** to check if your data's geographic areas match the existing database.

### If all names match:
```
✓ All admin area names match! Proceed to duplicate check.
```

### If there are unmatched names:

For each unmatched area, you'll see a dropdown to:

- **Select the correct name** from the database
- **Choose "IGNORE"** to skip those records

Example:
```
Nigeria - Lagos State
Years: 2018, 2019 | Records: 24
Select correct name or IGNORE: [Dropdown: IGNORE, la Lagos State, ...]
```

After making selections, click **"Apply Corrections & Continue"**.

---

## Step 3: Check for Duplicates

Click **"Check for Duplicates"** to identify records that already exist in the database.

A record is considered a duplicate if it matches on:

- `admin_area_1` (country)
- `admin_area_2` (province)
- `year`
- `indicator_common_id`

### Results:

- **Duplicate Records**: Shows existing vs. new values
- **New Records**: Records that will be added

For duplicates, you can choose:

- `keep_existing` - Don't update (default)
- `replace_with_new` - Update to new value

---

## Step 4: Append & Push to GitHub

### Commit Message

The app auto-generates detailed commit messages:

```
Add data: NGA, GIN, SEN (156 records)

Countries: NGA, GIN, SEN
Indicators: anc1, penta1, penta3, bcg, measles1
Years: 2018-2023
Source: DHS
Records: 156

Notes: Updated with latest DHS 2023 data

Timestamp: 2026-01-12 16:45 UTC
```

You can add custom notes in the **"Commit Notes"** field.

### Push

1. Ensure **"Push changes to GitHub"** is checked
2. Click **"Append & Push to GitHub"**
3. Wait for confirmation

**Success message:**
```
✓ Success!
Survey records added: 145
Population records added: 11
Total survey database: 19,564 records
Total population database: 1,329 records
Changes pushed to GitHub!
```

---

## Setting Up GitHub Token

### Create a Personal Access Token

1. Go to [github.com/settings/tokens](https://github.com/settings/tokens?type=beta)
2. Click **"Generate new token"**
3. Configure:
   - **Name**: `FASTR Survey Fetcher`
   - **Repository access**: Select `FASTR-Analytics/modules`
   - **Permissions**: Contents → Read and write
4. Click **"Generate token"**
5. Copy the token (you won't see it again!)

### Add Token to Environment

**Local (.Renviron file):**
```
GITHUB_TOKEN=ghp_your_token_here
```

**Hugging Face Spaces:**
1. Go to Space Settings
2. Find "Variables and secrets"
3. Add secret: `GITHUB_TOKEN` = your token

---

## Data Routing

Records are automatically routed to the correct file based on `indicator_common_id`:

| File | Indicators |
|------|------------|
| `survey_data_unified.csv` | anc1, penta1, bcg, measles1, u5mr, mcpr, etc. |
| `population_estimates_only.csv` | poptot, popu5, livebirth, womenrepage, totu1pop, totu5pop, popgrowth |
