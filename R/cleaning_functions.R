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

clean_dhs_data <- function(df) {
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
  
  df %>%
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
      survey_type = "household"
    )
}

# ========================================
# MICS DATA CLEANING
# ========================================

clean_mics_data <- function(df, selected_countries = NULL) {
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
      survey_type = "household"
    )
  
  message("MICS cleaning completed. Final records: ", nrow(cleaned_data))
  return(cleaned_data)
}

# ========================================
# UNWPP DATA CLEANING
# ========================================

clean_unwpp_data <- function(df) {
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
      survey_type = "modeled"
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
      survey_type = "modeled"
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
      survey_type = "modeled"
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
      survey_type = "modeled"
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
      survey_type = "modeled"
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
      survey_type = "modeled"
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
      survey_type = "modeled"
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
        survey_type = "modeled"
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
        survey_type = "modeled"
      )
  } else {
    totu5pop <- data.frame()
  }
  
  message("Under-5 population records: ", nrow(totu5pop))
  
  # Combine all indicators and select standard columns
  all_indicators <- list(crudebr, poptot, womenrepage, imr, u5mr, mcpr, livebirth, totu1pop, totu5pop)
  non_empty <- all_indicators[sapply(all_indicators, nrow) > 0]
  
  if(length(non_empty) > 0) {
    final_cleaned <- bind_rows(non_empty) %>%
      select(
        admin_area_1, admin_area_2, year,
        indicator_id, indicator_common_id, indicator_type,
        survey_value, source, source_detail, survey_type
      )
  } else {
    final_cleaned <- data.frame()
  }
  
  message("UNWPP cleaning completed. Final records: ", nrow(final_cleaned))
  message("Available indicators: ", paste(unique(final_cleaned$indicator_common_id), collapse = ", "))
  
  return(final_cleaned)
}

# ========================================
# MAIN CLEANING DISPATCHER - SIMPLIFIED
# ========================================

clean_survey_data <- function(raw_data, data_source, selected_countries = NULL) {
  if(data_source == "dhs") {
    return(clean_dhs_data(raw_data))
  } else if(data_source == "mics") {
    return(clean_mics_data(raw_data, selected_countries))
  } else if(data_source == "unwpp") {
    return(clean_unwpp_data(raw_data))
  } else {
    message("Unknown data source: ", data_source)
    return(data.frame())
  }
}
