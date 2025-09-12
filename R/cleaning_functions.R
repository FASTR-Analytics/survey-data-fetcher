# ========================================
# DATA CLEANING FUNCTIONS
# ========================================
# File: R/cleaning_functions.R
# Purpose: Functions for cleaning and standardizing data from different sources

# ========================================
# CLEANING CONFIGURATION FUNCTIONS
# ========================================

create_cleaning_config <- function(raw_data, data_source) {
  if(nrow(raw_data) == 0) return(data.frame())
  
  # Get unique indicators from raw data
  unique_indicators <- if(data_source == "dhs") {
    # Check what columns actually exist in DHS data
    if("IndicatorCode" %in% names(raw_data)) {
      raw_data %>%
        select(indicator_id = IndicatorId, indicator_name = IndicatorCode) %>%
        distinct()
    } else if("Indicator" %in% names(raw_data)) {
      raw_data %>%
        select(indicator_id = IndicatorId, indicator_name = Indicator) %>%
        distinct()
    } else {
      # Fallback: use IndicatorId for both
      raw_data %>%
        select(indicator_id = IndicatorId) %>%
        mutate(indicator_name = indicator_id) %>%
        distinct()
    }
  } else if(data_source == "mics") {
    # Check what columns actually exist in MICS data
    if("INDICATOR" %in% names(raw_data)) {
      raw_data %>%
        select(indicator_id = INDICATOR, indicator_name = INDICATOR) %>%
        distinct()
    } else if("indicator" %in% names(raw_data)) {
      raw_data %>%
        select(indicator_id = indicator, indicator_name = indicator) %>%
        distinct()
    } else {
      # Debug: show what columns are actually available
      message("MICS columns available: ", paste(names(raw_data), collapse = ", "))
      # Try to find any column that might contain indicator info
      possible_cols <- names(raw_data)[grepl("indicator|INDICATOR|IND", names(raw_data), ignore.case = TRUE)]
      if(length(possible_cols) > 0) {
        first_col <- possible_cols[1]
        raw_data %>%
          select(indicator_id = !!sym(first_col)) %>%
          mutate(indicator_name = indicator_id) %>%
          distinct()
      } else {
        # Ultimate fallback - create empty config
        data.frame(indicator_id = character(), indicator_name = character())
      }
    }
  } else if(data_source == "unwpp") {
    # Check what columns actually exist in UNWPP data
    if("indicator" %in% names(raw_data) && "indicatorDisplayName" %in% names(raw_data)) {
      raw_data %>%
        select(indicator_id = indicator, indicator_name = indicatorDisplayName) %>%
        distinct()
    } else if("indicator" %in% names(raw_data)) {
      raw_data %>%
        select(indicator_id = indicator) %>%
        mutate(indicator_name = indicator_id) %>%
        distinct()
    } else {
      # Debug: show what columns are actually available
      message("UNWPP columns available: ", paste(names(raw_data), collapse = ", "))
      # Ultimate fallback - create empty config
      data.frame(indicator_id = character(), indicator_name = character())
    }
  }
  
  # Create configuration table
  config <- unique_indicators %>%
    mutate(
      data_source = data_source,
      indicator_common_id = "", # User will fill this
      indicator_type = "auto", # Will be auto-detected
      apply_filter_totals = TRUE,
      apply_filter_preferred = data_source %in% c("dhs", "mics"),
      apply_filter_median = data_source == "unwpp",
      include_indicator = TRUE,
      notes = ""
    )
  
  return(config)
}

get_default_cleaning_config <- function(raw_data, data_source) {
  # Create base config
  config <- create_cleaning_config(raw_data, data_source)
  
  if(nrow(config) == 0) return(config)
  
  # Apply default mappings from existing functions
  if(data_source == "dhs") {
    mapping <- get_indicator_mapping()
    config <- config %>%
      left_join(mapping, by = c("indicator_id" = "original_id")) %>%
      mutate(
        indicator_common_id = ifelse(!is.na(common_id), common_id, ""),
        include_indicator = !is.na(common_id)
      ) %>%
      select(-common_id)
  } else if(data_source == "mics") {
    # Add MICS default mappings
    config <- config %>%
      mutate(
        indicator_common_id = case_when(
          indicator_id == "CME_MRM0" ~ "nmr",
          indicator_id == "CME_MRY0T4" ~ "imr",
          indicator_id == "IM_BCG" ~ "bcg",
          indicator_id == "IM_DTP1" ~ "penta1",
          indicator_id == "IM_DTP3" ~ "penta3",
          indicator_id == "MNCH_ANC1" ~ "anc1",
          indicator_id == "MNCH_ANC4" ~ "anc4",
          indicator_id == "MNCH_INSTDEL" ~ "delivery",
          indicator_id == "MNCH_PNCMOM" ~ "pnc1",
          indicator_id == "MNCH_ORSZINC" ~ "ors_zinc",
          TRUE ~ ""
        ),
        include_indicator = indicator_common_id != ""
      )
  } else if(data_source == "unwpp") {
    # Add UNWPP default mappings
    config <- config %>%
      mutate(
        indicator_common_id = case_when(
          grepl("Total population", indicator_name) ~ "poptot",
          grepl("Life expectancy at birth", indicator_name) ~ "life_expectancy",
          grepl("Infant mortality rate", indicator_name) ~ "imr",
          grepl("Under-five mortality rate", indicator_name) ~ "u5mr",
          grepl("Total fertility rate", indicator_name) ~ "tfr",
          grepl("Crude birth rate", indicator_name) ~ "crude_birth_rate",
          grepl("Maternal mortality ratio", indicator_name) ~ "mmr",
          TRUE ~ ""
        ),
        include_indicator = indicator_common_id != ""
      )
  }
  
  # Auto-detect indicator types
  config <- config %>%
    mutate(
      indicator_type = case_when(
        indicator_common_id %in% c("anc1", "anc4", "delivery", "pnc1", "bcg", "penta1", "penta3", "ors_zinc") ~ "percent",
        indicator_common_id %in% c("imr", "nmr", "u5mr", "mmr", "crude_birth_rate") ~ "rate",
        indicator_common_id %in% c("poptot", "life_expectancy", "tfr") ~ "index",
        TRUE ~ "auto"
      )
    )
  
  return(config)
}

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
  
  # Convert selected countries from ISO2 to ISO3 for filtering
  if(!is.null(selected_countries)) {
    iso3_countries <- countrycode(selected_countries, "iso2c", "iso3c", warn = FALSE)
    iso3_countries <- iso3_countries[!is.na(iso3_countries)]
    message("Selected countries converted to ISO3: ", paste(selected_countries, "->", iso3_countries, collapse = ", "))
    
    # Also check for alternative codes for Côte d'Ivoire
    if("CI" %in% selected_countries) {
      # Add common alternative codes for Côte d'Ivoire
      alternative_codes <- c("CIV", "COT", "384")  # Different possible codes
      iso3_countries <- c(iso3_countries, alternative_codes)
      message("Added alternative codes for Côte d'Ivoire: ", paste(alternative_codes, collapse = ", "))
    }
  }
  
  cleaned_data <- df %>%
    filter(SEX %in% c("_T", NA)) %>%  # Keep totals only
    filter(!is.na(OBS_VALUE)) %>%
    # Debug: show counts before filtering
    {message("Records before country filtering: ", nrow(.)); .} %>%
    # Filter by selected countries if specified
    {if(!is.null(selected_countries) && length(iso3_countries) > 0) {
      result <- filter(., REF_AREA %in% iso3_countries)
      message("Records after filtering for ", paste(iso3_countries, collapse = ", "), ": ", nrow(result))
      if(nrow(result) == 0) {
        message("WARNING: No records found for selected countries!")
        message("Available countries in data: ", paste(sort(unique(df$REF_AREA)), collapse = ", "))
      }
      result
    } else .} %>%
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
    transmute(
      admin_area_1 = admin_area_1,
      admin_area_2 = "NATIONAL",
      year = as.integer(timeLabel),
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
  
  # Use "Median" variant only for projections
  if ("variant" %in% names(df)) {
    df <- df %>% filter(variant == "Median")
  }
  
  cleaned <- df %>%
    # Filter for valid observations and totals only (no age/sex breakdowns)
    filter(!is.na(value)) %>%
    # Filter for totals only - no age or sex disaggregation
    filter(
      # Keep records with no sex column or sex is total
      (!("sex" %in% names(.)) | is.na(sex) | sex == "_T"),
      # Keep records with no age columns or age is total/all ages
      (!("age" %in% names(.)) | is.na(age)),
      (!("ageStart" %in% names(.)) | is.na(ageStart)),
      (!("ageEnd" %in% names(.)) | is.na(ageEnd))
    ) %>%
    mutate(
      # Handle complex indicators with sub-indicators
      indicator_simple = case_when(
        # Population indicators
        grepl("Population by single age", indicatorDisplayName) ~ "Population by single age",
        grepl("Population by 5-year age groups and sex", indicatorDisplayName) ~ "Population by 5-year age groups",
        grepl("Total population", indicatorDisplayName) ~ "Total population",
        grepl("Life expectancy at birth", indicatorDisplayName) ~ "Life expectancy at birth",
        grepl("Infant mortality rate", indicatorDisplayName) ~ "Infant mortality rate",
        grepl("Under-five mortality rate", indicatorDisplayName) ~ "Under-five mortality rate",
        grepl("Total fertility rate", indicatorDisplayName) ~ "Total fertility rate",
        grepl("Crude birth rate", indicatorDisplayName) ~ "Crude birth rate",
        grepl("Crude death rate", indicatorDisplayName) ~ "Crude death rate",
        grepl("Net migration", indicatorDisplayName) ~ "Net migration",
        grepl("Population growth rate", indicatorDisplayName) ~ "Population growth rate",
        grepl("Population density", indicatorDisplayName) ~ "Population density",
        # Maternal indicators
        grepl("Maternal mortality ratio", indicatorDisplayName) ~ "Maternal mortality ratio",
        # Sex ratio
        grepl("Sex ratio at birth", indicatorDisplayName) ~ "Sex ratio at birth",
        TRUE ~ indicatorDisplayName
      ),
      
      # Map to common indicator IDs
      indicator_common_id = case_when(
        indicator_simple == "Total population" ~ "poptot",
        indicator_simple == "Population by single age" ~ "pop_by_age",
        indicator_simple == "Population by 5-year age groups" ~ "pop_by_5year_age",
        indicator_simple == "Life expectancy at birth" ~ "life_expectancy",
        indicator_simple == "Infant mortality rate" ~ "imr",
        indicator_simple == "Under-five mortality rate" ~ "u5mr",
        indicator_simple == "Total fertility rate" ~ "tfr",
        indicator_simple == "Crude birth rate" ~ "crude_birth_rate",
        indicator_simple == "Crude death rate" ~ "crude_death_rate",
        indicator_simple == "Net migration" ~ "net_migration",
        indicator_simple == "Population growth rate" ~ "pop_growth_rate",
        indicator_simple == "Population density" ~ "pop_density",
        indicator_simple == "Maternal mortality ratio" ~ "mmr",
        indicator_simple == "Sex ratio at birth" ~ "sex_ratio_birth",
        TRUE ~ paste0("other_", as.character(indicator))
      ),
      
      # Classify indicator types
      indicator_type = case_when(
        indicator_common_id %in% c("poptot", "pop_by_age", "pop_by_5year_age") ~ "population_estimate",
        indicator_common_id %in% c("imr", "u5mr", "crude_birth_rate", "crude_death_rate", "mmr") ~ "rate",
        indicator_common_id %in% c("tfr", "life_expectancy", "net_migration", "pop_growth_rate", "pop_density", "sex_ratio_birth") ~ "index",
        TRUE ~ "other"
      ),
      
      # Handle country names
      admin_area_1 = case_when(
        location == "Côte d'Ivoire" ~ "Côte d'Ivoire",
        location == "Democratic Republic of the Congo" ~ "Democratic Republic of the Congo",
        TRUE ~ location
      ),
      
      # Create age group info if available (check what columns actually exist)
      age_info = if("ageStart" %in% names(df) && "ageEnd" %in% names(df)) {
        case_when(
          !is.na(ageStart) & !is.na(ageEnd) ~ paste0(ageStart, "-", ageEnd),
          TRUE ~ "Total"
        )
      } else if("age" %in% names(df)) {
        case_when(
          !is.na(age) ~ as.character(age),
          TRUE ~ "Total"
        )
      } else {
        "Total"
      },
      
      # Add sub-indicator info (for age groups, etc.)
      sub_indicator = case_when(
        age_info != "Total" ~ age_info,
        "sex" %in% names(df) & !is.na(sex) & sex != "_T" ~ case_when(
          sex == "M" ~ "Male",
          sex == "F" ~ "Female",
          TRUE ~ sex
        ),
        TRUE ~ "Total"
      )
    ) %>%
    transmute(
      admin_area_1 = admin_area_1,
      admin_area_2 = "NATIONAL",
      year = as.integer(timeLabel),
      indicator_id = as.character(indicator),
      indicator_common_id = as.character(indicator_common_id),
      indicator_type = indicator_type,
      survey_value = as.numeric(value),
      source = "UNWPP",
      source_detail = paste0("UNWPP - ", indicator_simple),
      survey_type = "modeled",
      age_group = age_info,
      sex_group = if("sex" %in% names(df)) {
        case_when(
          !is.na(sex) & sex != "_T" ~ case_when(
            sex == "M" ~ "Male",
            sex == "F" ~ "Female",
            TRUE ~ sex
          ),
          TRUE ~ "Total"
        )
      } else {
        "Total"
      },
      sub_indicator = sub_indicator
    ) %>%
    filter(!is.na(survey_value))
  
  message("UNWPP cleaning completed. Final records: ", nrow(cleaned))
  message("Available indicators: ", paste(unique(cleaned$indicator_common_id), collapse = ", "))
  
  return(cleaned)
}

# ========================================
# DYNAMIC CLEANING FUNCTION
# ========================================

clean_data_with_config <- function(raw_data, config, data_source) {
  if(nrow(raw_data) == 0 || nrow(config) == 0) return(data.frame())
  
  # Filter config for included indicators only
  config <- config %>% filter(include_indicator == TRUE)
  
  if(nrow(config) == 0) {
    message("No indicators selected for cleaning")
    return(data.frame())
  }
  
  # Apply source-specific pre-processing
  if(data_source == "dhs") {
    processed <- raw_data %>%
      filter(IndicatorId %in% config$indicator_id)
      
    # Apply filters based on config
    if(any(config$apply_filter_preferred)) {
      processed <- processed %>% filter(IsPreferred == 1)
    }
    
    # Detect and handle subnational data
    is_subnational <- "CharacteristicCategory" %in% names(processed) &&
      any(!is.na(processed$CharacteristicCategory) &
            tolower(processed$CharacteristicCategory) %in% c("region", "province", "state", "administrative region"))
    
    if(is_subnational) {
      processed <- processed %>%
        filter(!is.na(CharacteristicCategory),
               tolower(CharacteristicCategory) %in% c("region", "province", "state", "administrative region"))
    }
    
  } else if(data_source == "mics") {
    # Use flexible column reference for MICS
    indicator_col <- if("INDICATOR" %in% names(raw_data)) "INDICATOR" else "indicator"
    processed <- raw_data %>%
      filter(!!sym(indicator_col) %in% config$indicator_id)
      
    # Apply totals filter
    if(any(config$apply_filter_totals)) {
      processed <- processed %>% filter(SEX %in% c("_T", NA))
    }
    
  } else if(data_source == "unwpp") {
    processed <- raw_data %>%
      filter(indicator %in% config$indicator_id)
      
    # Apply variant filter
    if(any(config$apply_filter_median) && "variant" %in% names(processed)) {
      processed <- processed %>% filter(variant == "Median")
    }
    
    # Apply totals filter
    if(any(config$apply_filter_totals)) {
      processed <- processed %>%
        filter(
          (!("sex" %in% names(.)) | is.na(sex) | sex == "_T"),
          (!("age" %in% names(.)) | is.na(age)),
          (!("ageStart" %in% names(.)) | is.na(ageStart)),
          (!("ageEnd" %in% names(.)) | is.na(ageEnd))
        )
    }
  }
  
  # Create indicator mapping from config
  indicator_mapping <- config %>%
    select(indicator_id, indicator_common_id, indicator_type)
  
  # Apply common cleaning logic
  if(data_source == "dhs") {
    cleaned <- processed %>%
      filter(!is.na(Value)) %>%
      mutate(indicator_id = tolower(IndicatorId)) %>%
      left_join(indicator_mapping, by = "indicator_id") %>%
      filter(!is.na(indicator_common_id)) %>%
      mutate(
        # Convert based on indicator type
        cleaned_value = case_when(
          indicator_type == "percent" ~ Value / 100,
          TRUE ~ Value
        ),
        # Set admin area
        admin_area_2 = if(exists("is_subnational") && is_subnational) {
          str_trim(str_remove(CharacteristicLabel, "^\\.\\s*"))
        } else {
          "NATIONAL"
        }
      ) %>%
      transmute(
        admin_area_1 = CountryName,
        admin_area_2 = admin_area_2,
        year = as.integer(SurveyYear),
        indicator_id = as.character(IndicatorId),
        indicator_common_id = as.character(indicator_common_id),
        indicator_type = indicator_type,
        survey_value = cleaned_value,
        source = if(exists("is_subnational") && is_subnational) "DHS Sub-national" else "DHS National",
        source_detail = as.character(SurveyId),
        survey_type = "household"
      )
      
  } else if(data_source == "mics") {
    # Use the same flexible column reference
    indicator_col <- if("INDICATOR" %in% names(processed)) "INDICATOR" else "indicator"
    
    cleaned <- processed %>%
      filter(!is.na(OBS_VALUE)) %>%
      left_join(indicator_mapping, by = setNames("indicator_id", indicator_col)) %>%
      filter(!is.na(indicator_common_id)) %>%
      mutate(
        # Handle unit multipliers
        value = case_when(
          !is.na(UNIT_MULTIPLIER) & UNIT_MULTIPLIER == "3" ~ as.numeric(OBS_VALUE) * 1000,
          TRUE ~ as.numeric(OBS_VALUE)
        ),
        # Convert based on indicator type  
        cleaned_value = case_when(
          indicator_type == "percent" ~ value / 100,
          TRUE ~ value
        ),
        # Handle country names
        admin_area_1 = case_when(
          REF_AREA == "CIV" ~ "Côte d'Ivoire",
          REF_AREA == "COD" ~ "Democratic Republic of the Congo",
          TRUE ~ countrycode(REF_AREA, "iso3c", "country.name", warn = FALSE)
        )
      ) %>%
      transmute(
        admin_area_1 = admin_area_1,
        admin_area_2 = "NATIONAL",
        year = as.integer(TIME_PERIOD),
        indicator_id = as.character(!!sym(indicator_col)),
        indicator_common_id = as.character(indicator_common_id),
        indicator_type = indicator_type,
        survey_value = cleaned_value,
        source = "MICS",
        source_detail = as.character(DATA_SOURCE),
        survey_type = "household"
      )
      
  } else if(data_source == "unwpp") {
    cleaned <- processed %>%
      filter(!is.na(value)) %>%
      left_join(indicator_mapping, by = c("indicator" = "indicator_id")) %>%
      filter(!is.na(indicator_common_id)) %>%
      mutate(
        # Handle country names
        admin_area_1 = case_when(
          location == "Côte d'Ivoire" ~ "Côte d'Ivoire",
          location == "Democratic Republic of the Congo" ~ "Democratic Republic of the Congo",
          TRUE ~ location
        )
      ) %>%
      transmute(
        admin_area_1 = admin_area_1,
        admin_area_2 = "NATIONAL",
        year = as.integer(timeLabel),
        indicator_id = as.character(indicator),
        indicator_common_id = as.character(indicator_common_id),
        indicator_type = indicator_type,
        survey_value = as.numeric(value),
        source = "UNWPP",
        source_detail = paste0("UNWPP - ", indicator_common_id),
        survey_type = "modeled"
      )
  }
  
  message("Dynamic cleaning completed. Final records: ", nrow(cleaned))
  message("Indicators processed: ", paste(unique(cleaned$indicator_common_id), collapse = ", "))
  
  return(cleaned)
}
