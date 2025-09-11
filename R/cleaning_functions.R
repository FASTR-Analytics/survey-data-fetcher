# ========================================
# DATA CLEANING FUNCTIONS
# ========================================
# File: R/cleaning_functions.R
# Purpose: Functions for cleaning and standardizing data from different sources

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
      year = as.integer(year),
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
  if(nrow(df) == 0) return(data.frame())
  
  # For UNWPP, do proper cleaning with indicator mapping
  cleaned <- df %>%
    mutate(
      # Convert indicator IDs to strings for consistency
      indicator_id_str = as.character(indicator_id),
      # Map UNWPP indicators to common IDs
      indicator_common_id = case_when(
        indicator_id_str == "2" ~ "mcpr",
        indicator_id_str == "22" ~ "imr",
        indicator_id_str == "24" ~ "u5mr",
        indicator_id_str == "41" ~ "womenrepage",
        indicator_id_str == "46" ~ "totu5pop",
        indicator_id_str == "47" ~ "totu1pop",
        indicator_id_str == "49" ~ "poptot",
        indicator_id_str == "55" ~ "crudebr",
        TRUE ~ "unknown"
      ),
      # Classify indicator types properly
      indicator_type = case_when(
        indicator_common_id %in% c("mcpr") ~ "percent",
        indicator_common_id %in% c("imr", "u5mr", "crudebr") ~ "rate",
        indicator_common_id %in% c("womenrepage", "totu5pop", "totu1pop", "poptot") ~ "population_estimate",
        TRUE ~ "other"
      ),
      # Convert mcpr from percentage to decimal if needed
      survey_value_clean = case_when(
        indicator_common_id == "mcpr" & value > 1 ~ value / 100,
        TRUE ~ value
      ),
      # Convert country codes to names, with special handling for Côte d'Ivoire
      admin_area_1 = case_when(
        country == "CI" ~ "Côte d'Ivoire",
        country == "CD" ~ "Democratic Republic of the Congo",
        TRUE ~ countrycode(country, "iso2c", "country.name", warn = FALSE)
      )
    ) %>%
    transmute(
      admin_area_1 = admin_area_1,
      admin_area_2 = "NATIONAL",
      year = as.integer(year),
      indicator_id = indicator_id_str,  # String for consistency
      indicator_common_id = indicator_common_id,  # String for consistency
      indicator_type = indicator_type,
      survey_value = survey_value_clean,
      source = "UNWPP",
      source_detail = "UNWPP",
      survey_type = "modeled"
    ) %>%
    filter(!is.na(indicator_common_id), indicator_common_id != "unknown")
  
  message("UNWPP cleaning completed. Final records: ", nrow(cleaned))
  return(cleaned)
}
