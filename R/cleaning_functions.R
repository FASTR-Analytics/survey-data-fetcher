# ========================================
# DATA CLEANING FUNCTIONS
# ========================================
# File: R/cleaning_functions.R
# Purpose: Functions for cleaning and standardizing data from different sources

# ========================================
# SIMPLIFIED CLEANING FUNCTIONS - DEFAULT ONLY
# ========================================

# ========================================
# INDICATOR MAPPING
# ========================================

get_indicator_mapping <- function() {
  data.frame(
    original_id = c(
      "rh_ancp_w_skp",
      "rh_ancn_w_n4p",
      "rh_delp_c_dhf",
      "rh_pcmt_w_dy2",
      "rh_ancc_w_irn",
      
      "ch_vacc_c_bcg",
      "ch_vacc_c_pt1",
      "ch_vacc_c_pt2",
      "ch_vacc_c_pt3",
      
      "ch_vacc_c_msl",
      "ch_vacc_c_ms2",
      "ch_vacc_c_rt1",
      "ch_vacc_c_rt2",
      "ch_vacc_c_op1",
      "ch_vacc_c_op2",
      "ch_vacc_c_op3",
      
      "ml_iptp_w_spf", "ml_iptp_w_2sp", "ml_iptp_w_3sp",
      "cm_ecmt_c_imr", "cm_ecmr_c_nnr", "cm_pnmr_c_nsb",
      "fe_frty_w_npg", "fe_frtr_w_cbr", "fe_frtr_w_tfr",
      "fp_srcm_w_tot",
      "cn_miac_c_mmn", "cn_miac_c_dwm", "cn_miac_c_vas", "ch_diat_c_orz"
    ),
    common_id = c(
      "anc1", "anc4", "delivery", "pnc1", "iron_anc",
      "bcg", "penta1", "penta2", "penta3", "measles1", "measles2",
      "rotavirus1", "rotavirus2",
      "polio1", "polio2", "polio3",
      "iptp1", "iptp2", "iptp3",
      "imr", "nmr", "stillbirth",
      "womenrepage", "crude_birth_rate", "total_fertility_rate",
      "fp",
      "micronutrient", "deworming", "vitamina", "ors_zinc"
    ),
    stringsAsFactors = FALSE
  )
}

# ========================================
# DHS DATA CLEANING
# ========================================

clean_dhs_data <- function(df, apply_fastr_standardization = TRUE) {
  if(nrow(df) == 0) return(data.frame())
  
  mapping <- get_indicator_mapping()
  
  # Define which indicators are percentages (should be converted to decimals)
  percentage_indicators <- c(
    "anc1", "anc4", "delivery", "pnc1", "iron_anc",
    "bcg", "penta1", "penta2", "penta3", "measles1", "measles2",
    "rotavirus1", "rotavirus2", "polio1", "polio2", "polio3",
    "iptp1", "iptp2", "iptp3", "fp",
    "deworming", "vitamina", "ors_zinc", "micronutrient"
  )
  
  # Define rate indicators
  rate_indicators <- c("imr", "nmr", "crude_birth_rate", "total_fertility_rate")
  
  # Define number indicators
  number_indicators <- c("stillbirth")
  
  # Define survey count indicators
  survey_count_indicators <- c("women_repro_age")
  
  # Check if this is TRULY subnational data (geographic regions, not just disaggregated by age/sex/etc.)
  is_subnational <- "CharacteristicCategory" %in% names(df) &&
    any(!is.na(df$CharacteristicCategory) &
          tolower(df$CharacteristicCategory) %in% c("region", "province", "state", "administrative region"))
  
  # Alternative check if CharacteristicCategory not available
  if(!is_subnational && "CharacteristicLabel" %in% names(df)) {
    # Only treat as subnational if CharacteristicLabel contains actual region names
    # (not age groups like "0-4", sex like "Male", education like "Primary", etc.)
    char_labels <- unique(df$CharacteristicLabel[!is.na(df$CharacteristicLabel)])
    
    # Geographic regions typically don't contain numbers or common demographic terms
    geographic_patterns <- c("region", "province", "state", "district", "ashanti", "central", "northern",
                             "southern", "eastern", "western", "volta", "brong", "upper")
    non_geographic_patterns <- c("^[0-9]", "total", "male", "female", "primary", "secondary",
                                 "urban", "rural", "poorest", "richest", "lowest", "highest")
    
    has_geographic <- any(grepl(paste(geographic_patterns, collapse = "|"), char_labels, ignore.case = TRUE))
    has_non_geographic <- any(grepl(paste(non_geographic_patterns, collapse = "|"), char_labels, ignore.case = TRUE))
    
    # Only treat as subnational if it looks like geographic regions
    is_subnational <- has_geographic && !has_non_geographic
  }
  
  message("DHS data type detected: ", ifelse(is_subnational, "Subnational (Geographic)", "National (or non-geographic breakdown)"))
  
  cleaned_data <- df %>%
    filter(!is.na(Value), IsPreferred == 1) %>%
    # For truly subnational data, filter for meaningful geographic regions
    {if(is_subnational) {
      if("CharacteristicCategory" %in% names(.)) {
        filter(., !is.na(CharacteristicCategory),
               tolower(CharacteristicCategory) %in% c("region", "province", "state", "administrative region"))
      } else {
        filter(., !is.na(CharacteristicLabel), CharacteristicLabel != "Total")
      }
    } else .} %>%
    mutate(indicator_id = tolower(IndicatorId)) %>%
    left_join(mapping, by = c("indicator_id" = "original_id")) %>%
    filter(!is.na(common_id)) %>%
    mutate(
      # Convert percentages to decimals
      cleaned_value = ifelse(common_id %in% percentage_indicators, Value / 100, Value),
      # Classify indicator types
      indicator_type = case_when(
        common_id %in% percentage_indicators ~ "percent",
        common_id %in% rate_indicators ~ "rate",
        common_id %in% number_indicators ~ "number",
        common_id %in% survey_count_indicators ~ "survey_count",
        TRUE ~ "other"
      ),
      # Set admin_area_2 based on data type
      admin_area_2 = if(is_subnational) {
        # Clean up subnational area names (remove dots and extra spaces)
        str_trim(str_remove(CharacteristicLabel, "^\\.*\\s*"))
      } else {
        "NATIONAL"  # For national data, always use NATIONAL regardless of other breakdowns
      }
    ) %>%
    transmute(
      admin_area_1 = CountryName,
      admin_area_2 = admin_area_2,
      year = as.integer(SurveyYear),
      indicator_id = as.character(IndicatorId),           # Ensure string type
      indicator_common_id = as.character(common_id),      # Ensure string type
      indicator_type = indicator_type,
      survey_value = cleaned_value,
      source = if(is_subnational) "DHS Sub-national" else "DHS National",
      source_detail = as.character(SurveyId),
      survey_type = "household",
      country_name = CountryName,
      iso2_code = countrycode(CountryName, "country.name", "iso2c", warn = FALSE),
      iso3_code = countrycode(CountryName, "country.name", "iso3c", warn = FALSE)
    )

  # Apply FASTR name standardization
  cleaned_data <- apply_fastr_name_standardization(cleaned_data, apply_standardization = apply_fastr_standardization)

  return(cleaned_data)
}

# ========================================
# MICS DATA CLEANING
# ========================================

clean_mics_data <- function(df, selected_countries = NULL, apply_fastr_standardization = TRUE) {
  if(nrow(df) == 0) return(data.frame())

  # Debug: Check what columns we actually have
  message("MICS columns available: ", paste(names(df), collapse = ", "))

  # Debug: Check what countries are actually in the data
  if("REF_AREA" %in% names(df)) {
    available_countries <- unique(df$REF_AREA)
    message("Countries available in MICS data: ", paste(sort(available_countries), collapse = ", "))
  }

  # Define percentage indicators (coverage indicators that should be converted to decimals)
  percentage_indicators <- c("anc1", "anc4", "delivery", "pnc1", "bcg", "penta1", "penta3",
                             "hepb", "hib3", "ors", "ors_zinc", "fp")

  # Define rate indicators (mortality rates that stay as-is)
  rate_indicators <- c("imr", "nmr", "mmr")

  # Define population estimate indicators
  population_indicators <- c("popgrowth", "poptot", "popu5", "womenrepage", "totu1pop", "totu5pop", "livebirth")

  # Country filtering is now done at fetch time, so just process the data
  cleaned_data <- df %>%
    filter(SEX %in% c("_T", NA)) %>%  # Keep totals only
    filter(!is.na(OBS_VALUE)) %>%
    mutate(
      year = as.integer(TIME_PERIOD),
      # Simplified value calculation
      value = case_when(
        !is.na(UNIT_MULTIPLIER) & UNIT_MULTIPLIER == "3" ~ as.numeric(OBS_VALUE) * 1000,
        TRUE ~ as.numeric(OBS_VALUE)
      ),
      admin_area_1_iso = REF_AREA,
      indicator_id = INDICATOR,
      indicator_common_id = case_when(
        # Mortality
        indicator_id == "CME_MRM0" ~ "nmr",          # Neonatal mortality rate
        indicator_id == "CME_MRY0T4" ~ "imr",        # Infant mortality rate (0-4 years)
        
        # Population estimates
        indicator_id == "DM_POP_GRT" ~ "popgrowth",  # Population growth rate
        indicator_id == "DM_POP_TOT" ~ "poptot",     # Total population
        indicator_id == "DM_POP_U5" ~ "popu5",       # Under-5 population
        
        # Immunization
        indicator_id == "IM_BCG" ~ "bcg",            # BCG vaccine
        indicator_id == "IM_HEPBB" ~ "hepb",         # Hepatitis B
        indicator_id == "IM_HPV" ~ "hpv",            # HPV vaccine
        indicator_id == "IM_HIB3" ~ "hib3",          # Hib3
        indicator_id == "IM_DTP1" ~ "penta1",        # DTP1 (Penta1)
        indicator_id == "IM_DTP3" ~ "penta3",        # DTP3 (Penta3)
        
        # Maternal and newborn care
        indicator_id == "MNCH_ANC1" ~ "anc1",        # ANC1
        indicator_id == "MNCH_ANC4" ~ "anc4",        # ANC4+
        indicator_id == "MNCH_INSTDEL" ~ "delivery", # Institutional delivery
        indicator_id == "MNCH_PNCMOM" ~ "pnc1",      # PNC (mother)
        indicator_id == "MNCH_IPTP" ~ "iptp3",       # IPTp3+ doses
        indicator_id == "MNCH_MMR" ~ "mmr",          # Maternal mortality ratio
        
        # Diarrhoea treatment
        indicator_id == "MNCH_ORS" ~ "ors",          # ORS only
        indicator_id == "MNCH_ORSZINC" ~ "ors_zinc",  # ORS and Zinc
        
        # Family planning and nutrition
        indicator_id == "MNCH_DEMAND_FP" ~ "fp",     # Family planning demand satisfied
        
        # Fallback
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(value), !is.na(indicator_common_id)) %>%
    mutate(
      # Convert country codes to names, with special handling for Côte d'Ivoire
      admin_area_1 = case_when(
        admin_area_1_iso == "CIV" ~ "Côte d'Ivoire",
        admin_area_1_iso == "COT" ~ "Côte d'Ivoire",  # Alternative code
        admin_area_1_iso == "384" ~ "Côte d'Ivoire",  # Numeric code
        admin_area_1_iso == "COD" ~ "Democratic Republic of the Congo",
        TRUE ~ countrycode(admin_area_1_iso, "iso3c", "country.name", warn = FALSE)
      ),
      # Convert percentages to decimals
      survey_value = case_when(
        indicator_common_id %in% percentage_indicators ~ value / 100,
        TRUE ~ value
      ),
      # Classify indicator types
      indicator_type = case_when(
        indicator_common_id %in% percentage_indicators ~ "percent",
        indicator_common_id %in% rate_indicators ~ "rate",
        indicator_common_id %in% population_indicators ~ "population_estimate",
        TRUE ~ "other"
      )
    ) %>%
    # Remove rows where country name conversion failed (resulted in NA)
    filter(!is.na(admin_area_1)) %>%
    transmute(
      admin_area_1 = admin_area_1,
      admin_area_2 = "NATIONAL",
      year = year,
      indicator_id = as.character(indicator_id),          # Ensure string type
      indicator_common_id = as.character(indicator_common_id), # Ensure string type
      indicator_type = indicator_type,
      survey_value = survey_value,
      source = "MICS",
      source_detail = as.character(DATA_SOURCE),
      survey_type = "household",
      country_name = admin_area_1,
      iso2_code = admin_area_1_iso,  # MICS REF_AREA provides ISO2 codes
      iso3_code = countrycode(admin_area_1_iso, "iso2c", "iso3c", warn = FALSE)
    )

  # Apply FASTR name standardization
  cleaned_data <- apply_fastr_name_standardization(cleaned_data, apply_standardization = apply_fastr_standardization)

  message("MICS cleaning completed. Final records: ", nrow(cleaned_data))
  return(cleaned_data)
}

# ========================================
# UNWPP DATA CLEANING
# ========================================

clean_unwpp_data <- function(df, apply_fastr_standardization = TRUE) {
  if (nrow(df) == 0) return(data.frame())
  
  # Base filtering like your legacy code
  base <- df %>%
    filter(
      variant == "Median",
      !is.na(value)
    )
  
  message("Base records after median variant filter: ", nrow(base))
  
  # Crude birth rate - no sex filter needed
  crudebr <- base %>%
    filter(indicatorDisplayName == "Crude birth rate (births per 1,000 population)") %>%
    mutate(
      admin_area_1 = case_when(
        location == "Côte d'Ivoire" ~ "Côte d'Ivoire",
        location == "Democratic Republic of the Congo" ~ "Democratic Republic of the Congo", 
        TRUE ~ location
      ),
      admin_area_2 = "NATIONAL",
      year = as.integer(timeLabel),
      indicator_id = indicatorDisplayName,
      indicator_common_id = "crudebr",
      indicator_type = "rate",
      survey_value = as.numeric(value),
      source = "UNWPP",
      source_detail = "UNWPP - Crude birth rate",
      survey_type = "modeled",
      country_name = admin_area_1,
      iso2_code = iso2,
      iso3_code = countrycode(iso2, "iso2c", "iso3c", warn = FALSE)
    )
  
  message("Crude birth rate records: ", nrow(crudebr))
  
  # Total population (all ages) - requires "Both sexes"
  poptot <- base %>%
    filter(
      indicatorDisplayName == "Total population by sex",
      sex == "Both sexes"
    ) %>%
    mutate(
      admin_area_1 = case_when(
        location == "Côte d'Ivoire" ~ "Côte d'Ivoire",
        location == "Democratic Republic of the Congo" ~ "Democratic Republic of the Congo",
        TRUE ~ location
      ),
      admin_area_2 = "NATIONAL", 
      year = as.integer(timeLabel),
      indicator_id = indicatorDisplayName,
      indicator_common_id = "poptot",
      indicator_type = "population_estimate",
      survey_value = as.numeric(value),
      source = "UNWPP",
      source_detail = "UNWPP - Total population",
      survey_type = "modeled",
      country_name = admin_area_1,
      iso2_code = iso2,
      iso3_code = countrycode(iso2, "iso2c", "iso3c", warn = FALSE)
    )
  
  message("Total population records: ", nrow(poptot))
  
  # Female population 15–49 - requires Female sex and 15-49 age
  womenrepage <- base %>%
    filter(
      indicatorDisplayName == "Female population of reproductive age (15-49 years)",
      sex %in% c("Female", NA),
      ageLabel == "15-49"
    ) %>%
    mutate(
      admin_area_1 = case_when(
        location == "Côte d'Ivoire" ~ "Côte d'Ivoire", 
        location == "Democratic Republic of the Congo" ~ "Democratic Republic of the Congo",
        TRUE ~ location
      ),
      admin_area_2 = "NATIONAL",
      year = as.integer(timeLabel),
      indicator_id = indicatorDisplayName,
      indicator_common_id = "womenrepage", 
      indicator_type = "population_estimate",
      survey_value = as.numeric(value),
      source = "UNWPP",
      source_detail = "UNWPP - Female reproductive age population",
      survey_type = "modeled",
      country_name = admin_area_1,
      iso2_code = iso2,
      iso3_code = countrycode(iso2, "iso2c", "iso3c", warn = FALSE)
    )
  
  message("Women reproductive age records: ", nrow(womenrepage))
  
  # Infant mortality rate - requires "Both sexes"
  imr <- base %>%
    filter(
      indicatorDisplayName == "Infant mortality rate (IMR)",
      sex == "Both sexes"
    ) %>%
    mutate(
      admin_area_1 = case_when(
        location == "Côte d'Ivoire" ~ "Côte d'Ivoire",
        location == "Democratic Republic of the Congo" ~ "Democratic Republic of the Congo",
        TRUE ~ location
      ),
      admin_area_2 = "NATIONAL",
      year = as.integer(timeLabel),
      indicator_id = indicatorDisplayName,
      indicator_common_id = "imr",
      indicator_type = "rate",
      survey_value = as.numeric(value),
      source = "UNWPP", 
      source_detail = "UNWPP - Infant mortality rate",
      survey_type = "modeled",
      country_name = admin_area_1,
      iso2_code = iso2,
      iso3_code = countrycode(iso2, "iso2c", "iso3c", warn = FALSE)
    )
  
  message("Infant mortality rate records: ", nrow(imr))
  
  # Under-five mortality rate - requires "Both sexes"
  u5mr <- base %>%
    filter(
      indicatorDisplayName == "Under-five mortality rate (U5MR)",
      sex == "Both sexes" 
    ) %>%
    mutate(
      admin_area_1 = case_when(
        location == "Côte d'Ivoire" ~ "Côte d'Ivoire",
        location == "Democratic Republic of the Congo" ~ "Democratic Republic of the Congo",
        TRUE ~ location
      ),
      admin_area_2 = "NATIONAL",
      year = as.integer(timeLabel),
      indicator_id = indicatorDisplayName,
      indicator_common_id = "u5mr", 
      indicator_type = "rate",
      survey_value = as.numeric(value),
      source = "UNWPP",
      source_detail = "UNWPP - Under-five mortality rate",
      survey_type = "modeled",
      country_name = admin_area_1,
      iso2_code = iso2,
      iso3_code = countrycode(iso2, "iso2c", "iso3c", warn = FALSE)
    )
  
  message("Under-five mortality rate records: ", nrow(u5mr))
  
  # mCPR (modern contraceptive prevalence) - requires specific category
  mcpr <- base %>%
    filter(
      indicatorDisplayName == "CP Modern",
      category == "All women"
    ) %>%
    mutate(
      admin_area_1 = case_when(
        location == "Côte d'Ivoire" ~ "Côte d'Ivoire",
        location == "Democratic Republic of the Congo" ~ "Democratic Republic of the Congo",
        TRUE ~ location
      ),
      admin_area_2 = "NATIONAL",
      year = as.integer(timeLabel),
      indicator_id = indicatorDisplayName,
      indicator_common_id = "mcpr",
      indicator_type = "percent",
      survey_value = as.numeric(value) / 100,  # Convert to decimal
      source = "UNWPP",
      source_detail = "UNWPP - Modern contraceptive prevalence",
      survey_type = "modeled",
      country_name = admin_area_1,
      iso2_code = iso2,
      iso3_code = countrycode(iso2, "iso2c", "iso3c", warn = FALSE)
    )
  
  message("Modern contraceptive prevalence records: ", nrow(mcpr))
  
  # Total number of live births
  livebirth <- base %>%
    filter(indicatorDisplayName == "Total number of live births by sex") %>%
    mutate(
      admin_area_1 = case_when(
        location == "Côte d'Ivoire" ~ "Côte d'Ivoire",
        location == "Democratic Republic of the Congo" ~ "Democratic Republic of the Congo",
        TRUE ~ location
      ),
      admin_area_2 = "NATIONAL",
      year = as.integer(timeLabel),
      indicator_id = indicatorDisplayName,
      indicator_common_id = "livebirth",
      indicator_type = "population_estimate", 
      survey_value = as.numeric(value),
      source = "UNWPP",
      source_detail = "UNWPP - Live births",
      survey_type = "modeled",
      country_name = admin_area_1,
      iso2_code = iso2,
      iso3_code = countrycode(iso2, "iso2c", "iso3c", warn = FALSE)
    )
  
  message("Live birth records: ", nrow(livebirth))
  
  # U1: age = 0 (calculated from age-disaggregated data)
  totu1pop_data <- base %>%
    filter(
      indicatorDisplayName == "Annual population by 1-year age groups and by sex",
      ageLabel == "0",
      sex == "Both sexes"
    )
  
  if(nrow(totu1pop_data) > 0) {
    totu1pop <- totu1pop_data %>%
      group_by(location, iso2, timeLabel) %>%
      summarise(value = sum(value), .groups = "drop") %>%
      mutate(
        admin_area_1 = case_when(
          location == "Côte d'Ivoire" ~ "Côte d'Ivoire",
          location == "Democratic Republic of the Congo" ~ "Democratic Republic of the Congo", 
          TRUE ~ location
        ),
        admin_area_2 = "NATIONAL",
        year = as.integer(timeLabel),
        indicator_id = "Annual population age 0",
        indicator_common_id = "totu1pop",
        indicator_type = "population_estimate",
        survey_value = as.numeric(value),
        source = "UNWPP",
        source_detail = "UNWPP - Under-1 population",
        survey_type = "modeled",
        country_name = admin_area_1,
        iso2_code = iso2,
        iso3_code = countrycode(iso2, "iso2c", "iso3c", warn = FALSE)
      )
  } else {
    totu1pop <- data.frame()
  }
  
  message("Under-1 population records: ", nrow(totu1pop))
  
  # U5: ages 0–4 (calculated from age-disaggregated data)
  totu5pop_data <- base %>%
    filter(
      indicatorDisplayName == "Annual population by 1-year age groups and by sex",
      ageLabel %in% as.character(0:4),
      sex == "Both sexes"
    )
  
  if(nrow(totu5pop_data) > 0) {
    totu5pop <- totu5pop_data %>%
      group_by(location, iso2, timeLabel) %>%
      summarise(value = sum(value), .groups = "drop") %>%
      mutate(
        admin_area_1 = case_when(
          location == "Côte d'Ivoire" ~ "Côte d'Ivoire",
          location == "Democratic Republic of the Congo" ~ "Democratic Republic of the Congo",
          TRUE ~ location
        ),
        admin_area_2 = "NATIONAL", 
        year = as.integer(timeLabel),
        indicator_id = "Annual population age 0-4",
        indicator_common_id = "totu5pop",
        indicator_type = "population_estimate",
        survey_value = as.numeric(value),
        source = "UNWPP",
        source_detail = "UNWPP - Under-5 population",
        survey_type = "modeled",
        country_name = admin_area_1,
        iso2_code = iso2,
        iso3_code = countrycode(iso2, "iso2c", "iso3c", warn = FALSE)
      )
  } else {
    totu5pop <- data.frame()
  }
  
  message("Under-5 population records: ", nrow(totu5pop))

  # Life Expectancy at Birth - Both sexes
  lifeexp <- base %>%
    filter(
      indicatorDisplayName == "Life expectancy at birth (years)",
      sex == "Both sexes"
    ) %>%
    mutate(
      admin_area_1 = case_when(
        location == "Côte d'Ivoire" ~ "Côte d'Ivoire",
        location == "Democratic Republic of the Congo" ~ "Democratic Republic of the Congo",
        TRUE ~ location
      ),
      admin_area_2 = "NATIONAL",
      year = as.integer(timeLabel),
      indicator_id = indicatorDisplayName,
      indicator_common_id = "lifeexp",
      indicator_type = "rate",
      survey_value = as.numeric(value),
      source = "UNWPP",
      source_detail = "UNWPP - Life expectancy at birth",
      survey_type = "modeled",
      country_name = admin_area_1,
      iso2_code = iso2,
      iso3_code = countrycode(iso2, "iso2c", "iso3c", warn = FALSE)
    )

  message("Life expectancy records: ", nrow(lifeexp))

  # Total Fertility Rate - Female only
  tfr <- base %>%
    filter(indicatorDisplayName == "Total fertility (children per woman)") %>%
    mutate(
      admin_area_1 = case_when(
        location == "Côte d'Ivoire" ~ "Côte d'Ivoire",
        location == "Democratic Republic of the Congo" ~ "Democratic Republic of the Congo",
        TRUE ~ location
      ),
      admin_area_2 = "NATIONAL",
      year = as.integer(timeLabel),
      indicator_id = indicatorDisplayName,
      indicator_common_id = "tfr",
      indicator_type = "rate",
      survey_value = as.numeric(value),
      source = "UNWPP",
      source_detail = "UNWPP - Total fertility rate",
      survey_type = "modeled",
      country_name = admin_area_1,
      iso2_code = iso2,
      iso3_code = countrycode(iso2, "iso2c", "iso3c", warn = FALSE)
    )

  message("Total fertility rate records: ", nrow(tfr))

  # Median Age of Population - Both sexes
  medianage <- base %>%
    filter(
      indicatorDisplayName == "Median age of the population (years)",
      sex == "Both sexes"
    ) %>%
    mutate(
      admin_area_1 = case_when(
        location == "Côte d'Ivoire" ~ "Côte d'Ivoire",
        location == "Democratic Republic of the Congo" ~ "Democratic Republic of the Congo",
        TRUE ~ location
      ),
      admin_area_2 = "NATIONAL",
      year = as.integer(timeLabel),
      indicator_id = indicatorDisplayName,
      indicator_common_id = "medianage",
      indicator_type = "other",
      survey_value = as.numeric(value),
      source = "UNWPP",
      source_detail = "UNWPP - Median age of population",
      survey_type = "modeled",
      country_name = admin_area_1,
      iso2_code = iso2,
      iso3_code = countrycode(iso2, "iso2c", "iso3c", warn = FALSE)
    )

  message("Median age records: ", nrow(medianage))

  # Adult Mortality (15-50) - Both sexes
  adultmort <- base %>%
    filter(
      indicatorDisplayName == "Probability of dying between age 15 and age 50 (per 1,000)",
      sex == "Both sexes"
    ) %>%
    mutate(
      admin_area_1 = case_when(
        location == "Côte d'Ivoire" ~ "Côte d'Ivoire",
        location == "Democratic Republic of the Congo" ~ "Democratic Republic of the Congo",
        TRUE ~ location
      ),
      admin_area_2 = "NATIONAL",
      year = as.integer(timeLabel),
      indicator_id = indicatorDisplayName,
      indicator_common_id = "adultmort",
      indicator_type = "rate",
      survey_value = as.numeric(value),
      source = "UNWPP",
      source_detail = "UNWPP - Adult mortality rate (15-50)",
      survey_type = "modeled",
      country_name = admin_area_1,
      iso2_code = iso2,
      iso3_code = countrycode(iso2, "iso2c", "iso3c", warn = FALSE)
    )

  message("Adult mortality records: ", nrow(adultmort))

  # Child Dependency Ratio - Both sexes
  childdep <- base %>%
    filter(
      indicatorDisplayName == "Child dependency ratio (per 100 persons of working age)",
      sex == "Both sexes"
    ) %>%
    mutate(
      admin_area_1 = case_when(
        location == "Côte d'Ivoire" ~ "Côte d'Ivoire",
        location == "Democratic Republic of the Congo" ~ "Democratic Republic of the Congo",
        TRUE ~ location
      ),
      admin_area_2 = "NATIONAL",
      year = as.integer(timeLabel),
      indicator_id = indicatorDisplayName,
      indicator_common_id = "childdep",
      indicator_type = "rate",
      survey_value = as.numeric(value),
      source = "UNWPP",
      source_detail = "UNWPP - Child dependency ratio",
      survey_type = "modeled",
      country_name = admin_area_1,
      iso2_code = iso2,
      iso3_code = countrycode(iso2, "iso2c", "iso3c", warn = FALSE)
    )

  message("Child dependency records: ", nrow(childdep))

  # Old-Age Dependency Ratio - Both sexes
  olddep <- base %>%
    filter(
      indicatorDisplayName == "Old-age dependency ratio (per 100 persons of working age)",
      sex == "Both sexes"
    ) %>%
    mutate(
      admin_area_1 = case_when(
        location == "Côte d'Ivoire" ~ "Côte d'Ivoire",
        location == "Democratic Republic of the Congo" ~ "Democratic Republic of the Congo",
        TRUE ~ location
      ),
      admin_area_2 = "NATIONAL",
      year = as.integer(timeLabel),
      indicator_id = indicatorDisplayName,
      indicator_common_id = "olddep",
      indicator_type = "rate",
      survey_value = as.numeric(value),
      source = "UNWPP",
      source_detail = "UNWPP - Old-age dependency ratio",
      survey_type = "modeled",
      country_name = admin_area_1,
      iso2_code = iso2,
      iso3_code = countrycode(iso2, "iso2c", "iso3c", warn = FALSE)
    )

  message("Old-age dependency records: ", nrow(olddep))

  # Total Dependency Ratio - Both sexes
  totdep <- base %>%
    filter(
      indicatorDisplayName == "Total dependency ratio (per 100 persons of working age)",
      sex == "Both sexes"
    ) %>%
    mutate(
      admin_area_1 = case_when(
        location == "Côte d'Ivoire" ~ "Côte d'Ivoire",
        location == "Democratic Republic of the Congo" ~ "Democratic Republic of the Congo",
        TRUE ~ location
      ),
      admin_area_2 = "NATIONAL",
      year = as.integer(timeLabel),
      indicator_id = indicatorDisplayName,
      indicator_common_id = "totdep",
      indicator_type = "rate",
      survey_value = as.numeric(value),
      source = "UNWPP",
      source_detail = "UNWPP - Total dependency ratio",
      survey_type = "modeled",
      country_name = admin_area_1,
      iso2_code = iso2,
      iso3_code = countrycode(iso2, "iso2c", "iso3c", warn = FALSE)
    )

  message("Total dependency records: ", nrow(totdep))

  # Sex Ratio of Total Population
  sexratio <- base %>%
    filter(indicatorDisplayName == "Sex ratio of the total population (males per 100 females)") %>%
    mutate(
      admin_area_1 = case_when(
        location == "Côte d'Ivoire" ~ "Côte d'Ivoire",
        location == "Democratic Republic of the Congo" ~ "Democratic Republic of the Congo",
        TRUE ~ location
      ),
      admin_area_2 = "NATIONAL",
      year = as.integer(timeLabel),
      indicator_id = indicatorDisplayName,
      indicator_common_id = "sexratio",
      indicator_type = "rate",
      survey_value = as.numeric(value),
      source = "UNWPP",
      source_detail = "UNWPP - Sex ratio of total population",
      survey_type = "modeled",
      country_name = admin_area_1,
      iso2_code = iso2,
      iso3_code = countrycode(iso2, "iso2c", "iso3c", warn = FALSE)
    )

  message("Sex ratio records: ", nrow(sexratio))

  # Combine all indicators and select standard columns
  all_indicators <- list(crudebr, poptot, womenrepage, imr, u5mr, mcpr, livebirth, totu1pop, totu5pop,
                        lifeexp, tfr, medianage, adultmort, childdep, olddep, totdep, sexratio)
  non_empty <- all_indicators[sapply(all_indicators, nrow) > 0]
  
  if(length(non_empty) > 0) {
    final_cleaned <- bind_rows(non_empty) %>%
      select(
        admin_area_1, admin_area_2, year,
        indicator_id, indicator_common_id, indicator_type,
        survey_value, source, source_detail, survey_type,
        country_name, iso2_code, iso3_code
      )
  } else {
    final_cleaned <- data.frame()
  }
  
  # Apply FASTR name standardization
  final_cleaned <- apply_fastr_name_standardization(final_cleaned, apply_standardization = apply_fastr_standardization)

  message("UNWPP cleaning completed. Final records: ", nrow(final_cleaned))
  message("Available indicators: ", paste(unique(final_cleaned$indicator_common_id), collapse = ", "))

  return(final_cleaned)
}

# ========================================
# MAIN CLEANING DISPATCHER - SIMPLIFIED
# ========================================

clean_survey_data <- function(raw_data, data_source, selected_countries = NULL, apply_fastr_standardization = TRUE) {
  if(data_source == "dhs") {
    return(clean_dhs_data(raw_data, apply_fastr_standardization = apply_fastr_standardization))
  } else if(data_source == "mics") {
    return(clean_mics_data(raw_data, selected_countries, apply_fastr_standardization = apply_fastr_standardization))
  } else if(data_source == "unwpp") {
    return(clean_unwpp_data(raw_data, apply_fastr_standardization = apply_fastr_standardization))
  } else {
    message("Unknown data source: ", data_source)
    return(data.frame())
  }
}

# ========================================
# FASTR NAME STANDARDIZATION FUNCTIONS
# ========================================

get_country_name_mappings <- function() {
  c(
    "Guinea" = "Guinée",
    "Nigeria" = "ng Federal Government",
    "Somalia" = "Federal Govt of Somalia",
    "Ethiopia" = "Federal Ministry Of Health",
    "Côte d'Ivoire" = "Cote d Ivoire",
    "SierraLeone" = "Sierra Leone",
    "Cameroon" = "Ministere de la Sante Publique",
    "DRC" = "République Démocratique du Congo",
    "Senegal" = "Sénégal"
  )
}

get_province_name_mappings <- function() {
  list(
    # Senegal provinces (after country name is fixed to "Sénégal")
    "Sénégal" = c(
      "Dakar" = "DRS Dakar",
      "Kaolack" = "DRS Kaolack",
      "Matam" = "DRS Matam",
      "Fatick" = "DRS Fatick",
      "Ziguinchor" = "DRS Ziguinchor",
      "Saint Louis" = "DRS Saint-Louis",
      "Sédhiou" = "DRS Sedhiou",
      "Diourbel" = "DRS Diourbel",
      "Tambacounda" = "DRS Tambacounda",
      "Kolda" = "DRS Kolda",
      "Louga" = "DRS Louga",
      "Thiès" = "DRS Thies",
      "Kédougou" = "DRS Kedougou",
      "Kaffrine" = "DRS Kaffrine"
    ),

    # Nigeria provinces (after country name is fixed to "ng Federal Government")
    "ng Federal Government" = c(
      "Abia"        = "ab Abia State",
      "Adamawa"     = "ad Adamawa State",
      "Akwa Ibom"   = "ak Akwa-Ibom State",
      "Anambra"     = "an Anambra state",
      "Bauchi"      = "ba Bauchi State",
      "Benue"       = "be Benue State",
      "Borno"       = "bo Borno State",
      "Bayelsa"     = "by Bayelsa State",
      "Cross River" = "cr Cross River State",
      "Delta"       = "de Delta State",
      "Ebonyi"      = "eb Ebonyi State",
      "Edo"         = "ed Edo State",
      "Ekiti"       = "ek Ekiti State",
      "Enugu"       = "en Enugu State",
      "FCT Abuja"   = "fc Federal Capital Territory",
      "Gombe"       = "go Gombe State",
      "Imo"         = "im Imo State",
      "Jigawa"      = "ji Jigawa State",
      "Kaduna"      = "kd Kaduna State",
      "Kebbi"       = "ke Kebbi State",
      "Kano"        = "kn Kano State",
      "Kogi"        = "ko Kogi State",
      "Katsina"     = "kt Katsina State",
      "Kwara"       = "kw Kwara State",
      "Lagos"       = "la Lagos State",
      "Nasarawa"    = "na Nasarawa State",
      "Niger"       = "ni Niger State",
      "Ogun"        = "og Ogun State",
      "Ondo"        = "on Ondo State",
      "Osun"        = "os Osun State",
      "Oyo"         = "oy Oyo State",
      "Plateau"     = "pl Plateau State",
      "Rivers"      = "ri Rivers State",
      "Sokoto"      = "so Sokoto State",
      "Taraba"      = "ta Taraba State",
      "Yobe"        = "yo Yobe State",
      "Zamfara"     = "za Zamfara State",
      "North Central" = "North Central Zone",
      "North East" =  "North East Zone",
      "North West" = "North West Zone",
      "South East" = "South East Zone",
      "South South" = "South South Zone",
      "South West" = "South West Zone"
    ),

    # Guinea provinces (after country name is fixed to "Guinée")
    "Guinée" = c(
      "Conakry"     = "DSV Conakry",
      "Faranah"     = "IRS Faranah",
      "Kankan"      = "IRS Kankan",
      "Kindia"      = "IRS Kindia",
      "Mamou"       = "IRS Mamou",
      "Boké"        = "IRS Boké",
      "Labé"        = "IRS Labé",
      "N'Zérékoré"  = "IRS Nzérékoré"
    ),

    # Liberia provinces
    "Liberia" = c(
      "River Cess" = "Rivercess",
      "Montserrado incl. Monrovia" = "Montserrado"
    ),

    # Afghanistan provinces
    "Afghanistan" = c(
      "Sar-E-Pul" = "Sar-e-Pul",
      "Herat" = "Hirat",
      "Daykundi" = "Dykundi",
      "Kunarha" = "Kunar"
    ),

    # Cameroon regions (after country name is fixed to "Ministere de la Sante Publique")
    "Ministere de la Sante Publique" = c(
      "Extrême-Nord" = "Region Extreme-Nord",
      "Centre" = "Region Centre",
      "Ouest" = "Region Ouest",
      "Littoral" = "Region Littoral",
      "Est" = "Region Est",
      "Adamaoua" = "Region Adamaoua",
      "Nord Ouest" = "Region Nord-Ouest",
      "Sud Ouest" = "Region Sud-Ouest",
      "Nord" = "Region Nord",
      "Sud" = "Region Sud"
    ),

    # Haiti regions
    "Haiti" = c(
      "North" = "Nord",
      "Rest of West" = "Ouest",
      "South" = "Sud",
      "South-East" = "Sud-Est",
      "Region Centre" = "Centre",
      "North-East" = "Nord-Est",
      "North-West" = "Nord-Ouest"
    ),

    # DRC provinces (after country name is fixed to "République Démocratique du Congo")
    "République Démocratique du Congo" = c(
      "Kinshasa" = "kn Kinshasa Province",
      "Bas-Congo" = "kc Kongo Central Province",
      "Equateur (>= 2015)" = "eq Equateur Province",
      "Nord-Kivu" = "nk Nord Kivu Province",
      "Maniema" = "mn Maniema Province",
      "Sud-Kivu" = "sk Sud Kivu Province",
      "Kasai-Oriental (>= 2015)" = "ke Kasai Oriental Province",
      "Kwango" = "kg Kwango Province",
      "Kwilu" = "kl Kwilu Province",
      "Mai-Ndombe" = "md Maindombe Province",
      "Mongala" = "mg Mongala Province",
      "Nord-Ubangi" = "nu Nord Ubangi Province",
      "Sud-Ubangi" = "su Sud Ubangi Province",
      "Tshuapa" = "tu Tshuapa Province",
      "Bas-Uele" = "bu Bas Uele Province",
      "Haut-Uele" = "hu Haut Uele Province",
      "Ituri" = "it Ituri Province",
      "Tshopo" = "tp Tshopo Province",
      "Haut-Katanga" = "hk Haut Katanga Province",
      "Haut-Lomani" = "hl Haut Lomami Province",
      "Lualaba" = "ll Lualaba Province",
      "Tanganyka" = "tn Tanganyika Province",
      "Lomani" = "lm Lomami Province",
      "Sankuru" = "sn Sankuru Province",
      "Kasai" = "ks Kasai Province",
      "Kasai-Central" = "kr Kasai Central Province"
    ),

    # Sierra Leone districts (after country name is fixed to "Sierra Leone")
    "Sierra Leone" = c(
      "Bo" = "Bo District",
      "Bombali" = "Bombali District",
      "Bonthe" = "Bonthe District",
      "Falaba" = "Falaba District",
      "Kailahun" = "Kailahun District",
      "Kambia" = "Kambia District",
      "Karene" = "Karene District",
      "Kenema" = "Kenema District",
      "Koinadugu" = "Koinadugu District",
      "Kono" = "Kono District",
      "Moyamba" = "Moyamba District",
      "Port Loko" = "Port Loko District",
      "Pujehun" = "Pujehun District",
      "Tonkolili" = "Tonkolili District",
      "Western Rural" = "Western Area Rural District",
      "Western Urban" = "Western Area Urban District"
    )
  )
}

apply_fastr_name_standardization <- function(data, apply_standardization = TRUE) {
  if (!apply_standardization || nrow(data) == 0) {
    return(data)
  }

  # Track changes for user feedback
  country_changes <- 0
  province_changes <- 0

  # STEP 1: Fix country names FIRST
  if("admin_area_1" %in% names(data)) {
    country_fixes <- get_country_name_mappings()
    before_countries <- unique(data$admin_area_1)
    data$admin_area_1 <- dplyr::recode(data$admin_area_1, !!!country_fixes)
    after_countries <- unique(data$admin_area_1)
    country_changes <- length(setdiff(before_countries, after_countries))

    if(country_changes > 0) {
      changed_countries <- setdiff(before_countries, after_countries)
      message("FASTR Standardization: Fixed ", country_changes, " country name variants: ",
              paste(changed_countries, collapse = ", "))
    }
  }

  # STEP 2: Fix province names by country (after country names are corrected)
  if("admin_area_2" %in% names(data)) {
    province_fixes <- get_province_name_mappings()
    total_province_changes <- 0

    # Get unique countries in the dataset
    countries_in_data <- unique(data$admin_area_1)

    for(country in countries_in_data) {

      # Skip NATIONAL entries
      country_data <- data[data$admin_area_1 == country & data$admin_area_2 != "NATIONAL", ]

      if(nrow(country_data) > 0 && country %in% names(province_fixes)) {

        # Get the province fixes for this specific country
        country_province_fixes <- province_fixes[[country]]

        # Apply fixes only to this country's data
        before_provinces <- unique(country_data$admin_area_2)

        # Apply the fixes
        data$admin_area_2[data$admin_area_1 == country] <-
          dplyr::recode(data$admin_area_2[data$admin_area_1 == country], !!!country_province_fixes)

        # Count changes for this country
        after_provinces <- unique(data$admin_area_2[data$admin_area_1 == country & data$admin_area_2 != "NATIONAL"])
        country_province_changes <- length(setdiff(before_provinces, after_provinces))

        if(country_province_changes > 0) {
          changed_provinces <- setdiff(before_provinces, after_provinces)
          message("FASTR Standardization: Fixed ", country_province_changes, " province names in ", country, ": ",
                  paste(changed_provinces, collapse = ", "))
        }

        total_province_changes <- total_province_changes + country_province_changes
      }
    }

    province_changes <- total_province_changes
  }

  if(country_changes > 0 || province_changes > 0) {
    message("FASTR Standardization completed: ", country_changes, " countries, ", province_changes, " provinces standardized")
  }

  return(data)
}
