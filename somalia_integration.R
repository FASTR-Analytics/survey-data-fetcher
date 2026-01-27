# =============================================================================
# Somalia National Survey Data Integration
# Last edit: 2025 Jan 27
#
# Purpose:
#   1. Update existing 2020 SHDS data with more precise decimal values
#   2. Append new national survey records (MICS 2000, 2006, Other NS 2002, WUENIC 2000-2009)
# =============================================================================

library(dplyr)
library(tidyr)

# =============================================================================
# CONFIGURATION
# =============================================================================

SURVEY_DB_FILE <- "../survey_data_unified.csv"
COUNTRY_NAME <- "Federal Govt of Somalia"

# =============================================================================
# LOAD EXISTING DATABASE
# =============================================================================

message("\n=== LOADING SURVEY DATABASE ===")
survey_db <- read.csv(SURVEY_DB_FILE, stringsAsFactors = FALSE)
message("Loaded: ", nrow(survey_db), " records")

# =============================================================================
# 1. UPDATE EXISTING 2020 SHDS RECORDS WITH MORE PRECISE VALUES
# =============================================================================

message("\n=== UPDATING 2020 SHDS RECORDS ===")

# Show current values
current_shds <- survey_db %>%
  filter(admin_area_1 == COUNTRY_NAME,
         year == 2020,
         source == "DHS National",
         indicator_common_id %in% c("anc1", "anc4", "delivery", "pnc1"))

message("Current 2020 SHDS records:")
print(current_shds %>% select(year, indicator_common_id, survey_value, source))

# Update with precise values from SHDS 2019 (published as 2020 survey)
# Original file values: ANC1=31.1%, ANC4=24.4%, Delivery=20.7%, PNC1=10.5%
survey_db <- survey_db %>%
  mutate(
    survey_value = case_when(
      admin_area_1 == COUNTRY_NAME & year == 2020 & source == "DHS National" & indicator_common_id == "anc1" ~ 0.311,
      admin_area_1 == COUNTRY_NAME & year == 2020 & source == "DHS National" & indicator_common_id == "anc4" ~ 0.244,
      admin_area_1 == COUNTRY_NAME & year == 2020 & source == "DHS National" & indicator_common_id == "delivery" ~ 0.207,
      admin_area_1 == COUNTRY_NAME & year == 2020 & source == "DHS National" & indicator_common_id == "pnc1" ~ 0.105,
      TRUE ~ survey_value
    ),
    # Also update source_detail to reflect the actual survey name
    source_detail = case_when(
      admin_area_1 == COUNTRY_NAME & year == 2020 & source == "DHS National" &
        indicator_common_id %in% c("anc1", "anc4", "delivery", "pnc1") ~ "Somali Health and Demographic Survey 2020",
      TRUE ~ source_detail
    )
  )

message("\nUpdated 2020 SHDS records:")
print(survey_db %>%
        filter(admin_area_1 == COUNTRY_NAME, year == 2020, source == "DHS National",
               indicator_common_id %in% c("anc1", "anc4", "delivery", "pnc1")) %>%
        select(year, indicator_common_id, survey_value, source, source_detail))

# =============================================================================
# 2. CREATE NEW RECORDS TO APPEND
# =============================================================================

message("\n=== CREATING NEW RECORDS ===")

# --- MICS Survey Data ---
mics_surveys <- data.frame(
  admin_area_1 = COUNTRY_NAME,
  admin_area_2 = "NATIONAL",
  year = c(2000, 2006, 2006, 2002, 2006),
  indicator_id = c("MICS_ANC1", "MICS_ANC1", "MICS_ANC4", "MICS_DELIVERY", "MICS_DELIVERY"),
  indicator_common_id = c("anc1", "anc1", "anc4", "delivery", "delivery"),
  indicator_type = "percent",
  survey_value = c(0.323, 0.261, 0.063, 0.088, 0.094),  # converted to decimals
  source = c("MICS", "MICS", "MICS", "Other NS", "MICS"),
  source_detail = c(
    "Multiple Indicator Cluster Survey 2000",
    "Multiple Indicator Cluster Survey 2006",
    "Multiple Indicator Cluster Survey 2006",
    "Other National Survey 2002",
    "Multiple Indicator Cluster Survey 2006"
  ),
  survey_type = "household",
  country_name = "Somalia",
  iso2_code = "SO",
  iso3_code = "SOM",
  stringsAsFactors = FALSE
)

# --- WUENIC Data 2000-2009 ---
wuenic_years <- 2000:2009

# BCG values from file
bcg_values <- c(69, 70, 60, 65, 50, 50, 40, 38, 36, 29) / 100

# Penta1 values from file
penta1_values <- c(57, 57, 60, 65, 50, 50, 40, 59, 40, 52) / 100

# Penta3 values from file
penta3_values <- c(33, 33, 40, 40, 30, 35, 26, 40, 31, 42) / 100

wuenic_bcg <- data.frame(
  admin_area_1 = COUNTRY_NAME,
  admin_area_2 = "NATIONAL",
  year = wuenic_years,
  indicator_id = "IM_BCG",
  indicator_common_id = "bcg",
  indicator_type = "percent",
  survey_value = bcg_values,
  source = "WUENIC",
  source_detail = "WHO/UNICEF estimates of national immunization coverage",
  survey_type = "modeled",
  country_name = "Somalia",
  iso2_code = "SO",
  iso3_code = "SOM",
  stringsAsFactors = FALSE
)

wuenic_penta1 <- data.frame(
  admin_area_1 = COUNTRY_NAME,
  admin_area_2 = "NATIONAL",
  year = wuenic_years,
  indicator_id = "IM_DTP1",
  indicator_common_id = "penta1",
  indicator_type = "percent",
  survey_value = penta1_values,
  source = "WUENIC",
  source_detail = "WHO/UNICEF estimates of national immunization coverage",
  survey_type = "modeled",
  country_name = "Somalia",
  iso2_code = "SO",
  iso3_code = "SOM",
  stringsAsFactors = FALSE
)

wuenic_penta3 <- data.frame(
  admin_area_1 = COUNTRY_NAME,
  admin_area_2 = "NATIONAL",
  year = wuenic_years,
  indicator_id = "IM_DTP3",
  indicator_common_id = "penta3",
  indicator_type = "percent",
  survey_value = penta3_values,
  source = "WUENIC",
  source_detail = "WHO/UNICEF estimates of national immunization coverage",
  survey_type = "modeled",
  country_name = "Somalia",
  iso2_code = "SO",
  iso3_code = "SOM",
  stringsAsFactors = FALSE
)

# Combine all new records
new_records <- bind_rows(mics_surveys, wuenic_bcg, wuenic_penta1, wuenic_penta3)

message("New records to append: ", nrow(new_records))
message("\n--- Summary of new records ---")
print(new_records %>%
        group_by(indicator_common_id, source) %>%
        summarise(min_year = min(year), max_year = max(year), n = n(), .groups = "drop"))

message("\n--- New MICS/Other survey records ---")
print(mics_surveys %>% select(year, indicator_common_id, survey_value, source, source_detail))

# =============================================================================
# 3. CHECK FOR DUPLICATES BEFORE APPENDING
# =============================================================================

message("\n=== CHECKING FOR DUPLICATES ===")

# Create keys for comparison
new_keys <- paste(new_records$admin_area_1, new_records$admin_area_2,
                  new_records$year, new_records$indicator_common_id, sep = "|")

existing_keys <- paste(survey_db$admin_area_1, survey_db$admin_area_2,
                       survey_db$year, survey_db$indicator_common_id, sep = "|")

duplicates <- new_keys[new_keys %in% existing_keys]

if(length(duplicates) > 0) {
  message("WARNING: Found ", length(duplicates), " potential duplicates:")
  print(duplicates)
  message("\nRemoving duplicates from new records...")
  new_records <- new_records[!new_keys %in% existing_keys, ]
  message("Records after removing duplicates: ", nrow(new_records))
} else {
  message("No duplicates found - all records are new")
}

# =============================================================================
# 4. APPEND NEW RECORDS AND SAVE
# =============================================================================

message("\n=== APPENDING AND SAVING ===")

# Append new records
updated_db <- bind_rows(survey_db, new_records)

message("Original DB rows: ", nrow(survey_db))
message("New records added: ", nrow(new_records))
message("Updated DB rows: ", nrow(updated_db))

# Save
write.csv(updated_db, SURVEY_DB_FILE, row.names = FALSE)
message("\nSaved to: ", SURVEY_DB_FILE)

# =============================================================================
# 5. VERIFICATION
# =============================================================================

message("\n=== VERIFICATION ===")

# Reload and verify
verify_db <- read.csv(SURVEY_DB_FILE, stringsAsFactors = FALSE)

somalia_data <- verify_db %>%
  filter(admin_area_1 == COUNTRY_NAME) %>%
  arrange(indicator_common_id, year)

message("\nAll Somalia national survey data after update:")
print(somalia_data %>%
        select(year, indicator_common_id, survey_value, source, source_detail) %>%
        arrange(indicator_common_id, year))

message("\n=== DONE ===")
