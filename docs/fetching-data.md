# Fetching Data

## Example: Adding Nigeria DHS Data to FASTR

You need vaccination coverage data from Nigeria's latest DHS survey, cleaned and formatted for the FASTR analytics platform.

**Workflow:**

1. Open the app and select **DHS** from the sidebar
2. Click **Vaccinations** in Quick Favorites to select BCG, Penta1-3, Measles, etc.
3. Select **Nigeria** from the country dropdown
4. Set Geographic Level to **Subnational** (to get state-level data)
5. Click **Fetch Data**
6. Go to **Clean & Process** tab → click **Clean Data**
7. Go to **Database Integration** tab:
    - Click **Pull from GitHub** to get the latest database
    - Click **Validate Names** to check state names match
    - Click **Check Duplicates** to see if records already exist
    - Click **Append & Push** to add new records to GitHub

The data is now standardized with FASTR column names (`admin_area_1`, `indicator_common_id`, etc.) and pushed to the shared database.

---

## Step-by-Step Guide

### 1. Select Data Source

Choose your data source from the sidebar:

- **DHS** - Demographic & Health Surveys (household survey data)
- **UNICEF** - MICS, WUENIC, mortality estimates
- **UNWPP** - UN World Population Prospects

### 2. Select Indicators

#### Quick Favorites Mode (Recommended)

Click category buttons to quickly select common indicators:

**DHS Favorites:**

- `ANC & Maternal` - ANC1, ANC4, delivery, postnatal care
- `Vaccinations` - BCG, Penta1/2/3, Polio, Measles, etc.
- `IPTp` - Malaria prevention in pregnancy
- `Mortality` - IMR, U5MR, NMR

**UNICEF Favorites:**

- `Maternal Health` - ANC visits, skilled birth attendance
- `Child Immunization` - WUENIC coverage estimates
- `Child Mortality` - UN IGME estimates

**UNWPP Favorites:**

- `Health & Mortality` - IMR, U5MR, life expectancy
- `Demographics` - Population, birth rate, fertility
- `Social Structure` - Women reproductive age, growth rate

#### Browse Mode (Advanced)

Switch to "Browse All" to see every available indicator. Use the search box to filter.

!!! tip "ANC1 Note"
    For DHS data, we use `RH_ANCP_W_SKP` (ANC1 from skilled provider) as the standard ANC1 indicator.

### 3. Select Countries

- Use the country dropdown to select one or more countries
- For DHS, choose Geographic Level: **National** or **Subnational**
- For UNWPP, set the **Start Year** and **End Year**

### 4. Fetch Data

Click the **"Fetch Data"** button. The app will:

1. Connect to the API
2. Request data for your selections
3. Download and parse the response
4. Display results in the Results tab

### 5. View Results

Navigate to the **Results** tab to see:

- **Session Data Cart** - All datasets fetched this session
- **Latest Fetch Preview** - Most recent data
- **Data Summary** - Row counts, columns, indicators

---

## Using the Data Cart

The data cart lets you accumulate multiple datasets in a single session.

### Add to Cart

- Check **"Add to session cart"** before fetching
- Each fetch adds to your collection

### Manage Cart

- View all items in the cart table
- **Remove Selected** - Delete specific items
- **Clear All** - Start fresh

### Combine Data

When you click **"Clean All Cart Data"**, the app will:

1. Clean each dataset individually
2. Combine them into one unified dataset
3. Preserve both national and subnational data

---

## Download Options

### Raw Data

- **Download Latest CSV** - Most recent fetch (uncleaned)
- **Download Latest RDS** - R data format
- **Download All Cart Data (CSV)** - Combined cart data

### Cleaned Data

After cleaning, download from the **Clean & Process** tab:

- **Download Cleaned CSV** - Standardized, ready for integration
