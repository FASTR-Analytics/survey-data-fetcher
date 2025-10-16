# ========================================
# DATA FETCHING AND METADATA FUNCTIONS
# ========================================
# File: R/data_functions.R
# Purpose: All functions for fetching data from DHS, MICS, and UNWPP APIs

# ========================================
# DATASET LABELING HELPER
# ========================================

#' Generate a human-readable label for a dataset
#' @param source Data source ("dhs", "mics", "unwpp")
#' @param indicators Vector of indicator IDs
#' @param countries Vector of country codes
#' @return A concise label string
generate_dataset_label <- function(source, indicators, countries) {
  # Source label
  source_label <- switch(source,
                        "dhs" = "DHS",
                        "mics" = "MICS",
                        "unwpp" = "UNWPP",
                        toupper(source))

  # Indicator summary
  n_indicators <- length(indicators)
  indicator_summary <- if(n_indicators == 1) {
    # Try to get a short readable name
    indicators[1]
  } else {
    paste0(n_indicators, " indicators")
  }

  # Country summary
  n_countries <- length(countries)
  country_summary <- if(n_countries == 1) {
    countries[1]
  } else if(n_countries <= 3) {
    paste(countries, collapse = ", ")
  } else {
    paste0(n_countries, " countries")
  }

  # Combine into label
  label <- paste0(source_label, " | ", indicator_summary, " | ", country_summary)

  # Truncate if too long
  if(nchar(label) > 80) {
    label <- paste0(substr(label, 1, 77), "...")
  }

  return(label)
}

# ========================================
# DHS FAVORITE INDICATORS CONFIGURATION
# ========================================

get_dhs_favorites <- function() {
  list(
    "ANC and Maternal" = c(
      "RH_ANCP_W_SKP",  # ANC1 from skilled provider *** PREFERRED ANC1 ***
      "RH_ANCN_W_N4P",  # ANC4+
      "RH_DELP_C_DHF",  # Institutional delivery
      "RH_PCMN_W_MOT",  # PNC_mother
      "RH_ANCC_W_IRN"   # Iron supplementation during ANC
    ),
    "IPTp (Malaria Prevention)" = c(
      "ML_IPTP_W_SPF",  # IPTp1
      "ML_IPTP_W_2SP",  # IPTp2+
      "ML_IPTP_W_3SP"   # IPTp3+
    ),
    "Family Planning" = c(
      "FP_SRCM_W_TOT"   # mCPR
    ),
    "Mortality and Fertility" = c(
      "CM_PNMR_C_NSB",  # Stillbirth rate
      "CM_ECMT_C_IMR",  # IMR
      "CM_ECMR_C_NNR",  # NMR
      "FE_FRTY_W_NPG",  # Women of reproductive age
      "FE_FRTR_W_CBR",  # Crude birth rate
      "FE_FRTR_W_TFR"   # Total fertility rate
    ),
    "Child Vaccinations" = c(
      "CH_VACC_C_BCG",  # BCG
      "CH_VACC_C_PT1",  # Penta1
      "CH_VACC_C_PT2",  # Penta2
      "CH_VACC_C_PT3",  # Penta3
      "CH_VACS_C_OP1",  # Polio1
      "CH_VACC_C_OP2",  # Polio2
      "CH_VACC_C_OP3",  # Polio3
      "CH_VACC_C_MSL",  # Measles1
      "CH_VACC_C_MS2",  # Measles2
      "CH_VACC_C_RT1",  # Rotavirus 1
      "CH_VACC_C_RT2"   # Rotavirus 2
    ),
    "Child Health and Nutrition" = c(
      "CN_MIAC_C_MMN",  # Micronutrient powder (MNP)
      "CN_MIAC_C_DWM",  # Deworming
      "CN_MIAC_C_VAS",  # Vitamin A
      "CH_DIAT_C_ORZ"   # ORS and Zinc
    )
  )
}

# ========================================
# COUNTRY FETCHING FUNCTIONS
# ========================================

fetch_dhs_countries <- function() {
  tryCatch({
    url <- "https://api.dhsprogram.com/rest/dhs/countries?f=json"
    response <- GET(url)
    data_parsed <- fromJSON(content(response, as = "text"), flatten = TRUE)
    countries_df <- data_parsed$Data
    
    countries_df %>%
      filter(!is.na(ISO2_CountryCode), ISO2_CountryCode != "",
             !is.na(DHS_CountryCode), DHS_CountryCode != "") %>%
      mutate(
        country_display = paste0(CountryName, " (", ISO2_CountryCode, ")"),
        source = "DHS",
        continent = countrycode(ISO2_CountryCode, "iso2c", "continent", warn = FALSE),
        region = countrycode(ISO2_CountryCode, "iso2c", "region", warn = FALSE)
      ) %>%
      select(
        country_code = ISO2_CountryCode,    # For user interface
        dhs_code = DHS_CountryCode,         # For API calls
        country_name = CountryName,
        country_display,
        continent,
        region,
        source
      ) %>%
      arrange(country_name)
  }, error = function(e) {
    message("Error fetching DHS countries: ", e$message)
    return(get_fallback_countries("DHS"))
  })
}

get_fallback_countries <- function(source) {
  base_countries <- data.frame(
    country_code = c("AF", "BD", "BJ", "BF", "BI", "KH", "CM", "CF", "TD", "KM",
                     "CD", "CI", "DJ", "EG", "ER", "ET", "GM", "GH", "GN", "GW",
                     "HT", "IN", "ID", "KE", "LS", "LR", "MG", "MW", "ML", "MR",
                     "MZ", "MM", "NP", "NE", "NG", "PK", "RW", "ST", "SN", "SL",
                     "SO", "SS", "LK", "TZ", "TG", "UG", "YE", "ZM", "ZW"),
    stringsAsFactors = FALSE
  )
  
  base_countries %>%
    mutate(
      country_name = countrycode(country_code, "iso2c", "country.name", warn = FALSE),
      country_display = paste0(country_name, " (", country_code, ")"),
      continent = countrycode(country_code, "iso2c", "continent", warn = FALSE),
      region = countrycode(country_code, "iso2c", "region", warn = FALSE),
      source = source
    ) %>%
    filter(!is.na(country_name)) %>%
    arrange(country_name)
}

fetch_mics_countries <- function() {
  tryCatch({
    # Try to fetch from UNICEF SDMX API first
    ref_area_codelist <- readSDMX("https://sdmx.data.unicef.org/ws/public/sdmxapi/rest/codelist/UNICEF/CL_UNICEF_REF_AREA/1.0")
    countries_df <- as.data.frame(ref_area_codelist)
    
    countries_df %>%
      filter(nchar(id) == 3) %>%  # ISO3 codes are 3 characters
      mutate(
        country_code = countrycode(id, "iso3c", "iso2c", warn = FALSE),
        country_name = label.en,
        country_display = paste0(label.en, " (", country_code, ")"),
        source = "UNICEF",
        continent = countrycode(id, "iso3c", "continent", warn = FALSE),
        region = countrycode(id, "iso3c", "region", warn = FALSE)
      ) %>%
      filter(!is.na(country_code)) %>%
      select(country_code, country_name, country_display, continent, region, source) %>%
      arrange(country_name)
    
  }, error = function(e) {
    message("MICS API unavailable, using curated MICS countries list")
    
    # Curated list of countries that actually have MICS data, ensuring CI is included
    mics_countries <- data.frame(
      country_code = c("AF", "BD", "BJ", "BF", "BI", "KH", "CM", "CF", "TD", "KM",
                       "CD", "CI", "DJ", "EG", "ER", "ET", "GM", "GH", "GN", "GW",
                       "HT", "KE", "LS", "LR", "MG", "MW", "ML", "MR", "MZ", "NP",
                       "NE", "NG", "RW", "ST", "SN", "SL", "SO", "SS", "LK", "TZ",
                       "TG", "UG", "ZM", "ZW", "AL", "BA", "MD", "ME", "MK", "RS"),
      stringsAsFactors = FALSE
    ) %>%
      mutate(
        country_name = case_when(
          country_code == "CI" ~ "Côte d'Ivoire",  # Explicit handling for CI
          country_code == "CD" ~ "Democratic Republic of the Congo",
          TRUE ~ countrycode(country_code, "iso2c", "country.name", warn = FALSE)
        ),
        country_display = paste0(country_name, " (", country_code, ")"),
        source = "UNICEF",
        continent = countrycode(country_code, "iso2c", "continent", warn = FALSE),
        region = countrycode(country_code, "iso2c", "region", warn = FALSE)
      ) %>%
      filter(!is.na(country_name)) %>%
      arrange(country_name)
    
    return(mics_countries)
  })
}

fetch_mics_wuenic_countries <- function() {
  tryCatch({
    # Read WUENIC file to get countries with actual MICS data
    wuenic_file <- "assets/survey-data_wuenic2024rev.xlsx"

    if (!file.exists(wuenic_file)) {
      stop("WUENIC file not found at: ", wuenic_file)
    }

    # Read the data
    survey_data <- read_excel(wuenic_file, sheet = "Data", skip = 1)

    # Filter to MICS only and get unique countries
    mics_countries <- survey_data %>%
      filter(grepl("MICS|Multiple Indicator Cluster", surveyNameEnglish, ignore.case = TRUE)) %>%
      select(ISO3) %>%
      distinct() %>%
      mutate(
        country_code = countrycode(ISO3, "iso3c", "iso2c", warn = FALSE),
        country_name = countrycode(ISO3, "iso3c", "country.name", warn = FALSE),
        country_display = paste0(country_name, " (", country_code, ")"),
        source = "WUENIC",
        continent = countrycode(ISO3, "iso3c", "continent", warn = FALSE),
        region = countrycode(ISO3, "iso3c", "region", warn = FALSE)
      ) %>%
      filter(!is.na(country_code), !is.na(country_name)) %>%
      select(country_code, country_name, country_display, continent, region, source) %>%
      arrange(country_name)

    return(mics_countries)

  }, error = function(e) {
    message("Could not read WUENIC file: ", e$message)
    # Return empty data frame if file not found
    return(data.frame(
      country_code = character(0),
      country_name = character(0),
      country_display = character(0),
      continent = character(0),
      region = character(0),
      source = character(0)
    ))
  })
}

fetch_unwpp_countries <- function() {
  get_fallback_countries("UNWPP")
}

# ========================================
# METADATA FETCHING FUNCTIONS
# ========================================

fetch_dhs_metadata <- function() {
  tryCatch({
    url <- "https://api.dhsprogram.com/rest/dhs/indicators?f=json"
    response <- GET(url)
    data_parsed <- fromJSON(content(response, as = "text"), flatten = TRUE)
    indicators_df <- data_parsed$Data

    favorites <- unlist(get_dhs_favorites(), use.names = FALSE)

    indicators_df %>%
      mutate(
        display_label = paste0(Label, " (", IndicatorId, ")"),
        is_favorite = IndicatorId %in% favorites,
        source = "DHS",
        full_definition = ifelse(is.na(Definition) | Definition == "", Label, Definition),
        Category = Level1,
        Subcategory = Level2,
        `Demographic Group` = Level3,
        `Measurement Type` = MeasurementType,
        Denominator = Denominator
      ) %>%
      select(IndicatorId, display_label, Label, full_definition, Category, Subcategory, `Demographic Group`,
             `Measurement Type`, Denominator, is_favorite, source) %>%
      arrange(desc(is_favorite), Category, Subcategory, display_label)
  }, error = function(e) {
    message("Error fetching DHS metadata: ", e$message)
    return(data.frame())
  })
}

fetch_mics_metadata <- function() {
  tryCatch({
    # Fetch indicator metadata from UNICEF SDMX API
    codelist_url <- "https://sdmx.data.unicef.org/ws/public/sdmxapi/rest/codelist/UNICEF/CL_UNICEF_INDICATOR"
    codelist <- readSDMX(codelist_url)
    indicators_df <- as.data.frame(codelist)

    # Debug: Check what columns we actually have
    message("MICS API columns available: ", paste(names(indicators_df), collapse = ", "))
    message("Number of indicators from API: ", nrow(indicators_df))

    # Define favorite indicators (our current core set)
    favorite_indicators <- c("CME_MRM0", "CME_MRY0T4", "IM_BCG", "IM_DTP1", "IM_DTP3",
                            "MNCH_ANC1", "MNCH_ANC4", "MNCH_INSTDEL", "MNCH_PNCMOM", "MNCH_ORSZINC")

    # Filter for MICS-relevant indicators based on common MICS survey topics
    mics_relevant_prefixes <- c(
      "CME_",     # Child Mortality
      "IM_",      # Immunization
      "MNCH_",    # Maternal, Newborn and Child Health
      "NT_",      # Nutrition
      "CP_",      # Child Protection
      "ECD_",     # Early Childhood Development
      "WS_",      # Water and Sanitation
      "ED_",      # Education
      "DV_",      # Disability
      "TC_"       # Birth Registration and other topics
    )

    # Check available column names and adapt accordingly
    id_col <- if("id" %in% names(indicators_df)) "id" else if("ID" %in% names(indicators_df)) "ID" else names(indicators_df)[1]
    name_col <- if("name.en" %in% names(indicators_df)) "name.en" else if("Name" %in% names(indicators_df)) "Name" else names(indicators_df)[2]

    # Filter and process the indicators data
    result_df <- indicators_df %>%
      filter(grepl(paste(mics_relevant_prefixes, collapse = "|"), !!sym(id_col))) %>%
      mutate(
        IndicatorId = !!sym(id_col),
        Label = !!sym(name_col),
        display_label = paste0(!!sym(name_col), " (", !!sym(id_col), ")"),
        description = if("description.en" %in% names(.)) description.en else !!sym(name_col),
        Category = if("parent" %in% names(.)) parent else "General",
        is_favorite = !!sym(id_col) %in% favorite_indicators,
        source = "UNICEF"
      ) %>%
      select(IndicatorId, display_label, Label, description, Category, is_favorite, source) %>%
      arrange(desc(is_favorite), Category, Label)

    message("Processed indicators: ", nrow(result_df))
    return(result_df)

  }, error = function(e) {
    message("Error fetching MICS metadata from API, using fallback data: ", e$message)

    # Fallback to hardcoded core indicators if API fails
    data.frame(
      IndicatorId = c("CME_MRM0", "CME_MRY0T4", "IM_BCG", "IM_DTP1", "IM_DTP3",
                      "MNCH_ANC1", "MNCH_ANC4", "MNCH_INSTDEL", "MNCH_PNCMOM", "MNCH_ORSZINC"),
      display_label = c("Neonatal mortality rate (CME_MRM0)", "Infant mortality rate (CME_MRY0T4)",
                        "BCG vaccination (IM_BCG)", "Penta1 vaccination (IM_DTP1)", "Penta3 vaccination (IM_DTP3)",
                        "ANC1 coverage (MNCH_ANC1)", "ANC4+ coverage (MNCH_ANC4)",
                        "Institutional delivery (MNCH_INSTDEL)", "PNC for mother (MNCH_PNCMOM)",
                        "ORS and Zinc treatment (MNCH_ORSZINC)"),
      Label = c("Neonatal mortality rate", "Infant mortality rate", "BCG vaccination coverage",
                "DTP1 vaccination coverage", "DTP3 vaccination coverage", "ANC1 coverage",
                "ANC4+ coverage", "Institutional delivery rate", "PNC coverage for mother",
                "ORS and Zinc treatment"),
      description = c("Neonatal mortality rate (deaths per 1,000 live births)",
                      "Infant mortality rate (deaths per 1,000 live births)",
                      "BCG vaccination coverage among children 12-23 months",
                      "DTP1 vaccination coverage among children 12-23 months",
                      "DTP3 vaccination coverage among children 12-23 months",
                      "ANC1 coverage (at least 1 visit)", "ANC4+ coverage (at least 4 visits)",
                      "Institutional delivery rate", "PNC coverage for mother within 2 days",
                      "ORS and Zinc treatment for diarrhea"),
      Category = c("Child Mortality", "Child Mortality", "Immunization", "Immunization", "Immunization",
                   "Maternal Health", "Maternal Health", "Maternal Health", "Maternal Health", "Child Health"),
      is_favorite = TRUE,
      source = "UNICEF",
      stringsAsFactors = FALSE
    )
  })
}

fetch_unwpp_metadata <- function() {
  tryCatch({
    # Fetch indicator metadata from UN Population Division API
    api_url <- "https://population.un.org/dataportalapi/api/v1/indicators/"
    response <- GET(api_url)
    data_parsed <- fromJSON(content(response, as = "text"), flatten = TRUE)
    indicators_df <- data_parsed$data

    # Define favorite indicators (our current core set - first 8 only)
    favorite_indicators <- c("2", "22", "24", "41", "46", "47", "49", "55")

    # Process the indicators data
    indicators_df %>%
      mutate(
        IndicatorId = as.character(id),
        display_label = paste0(shortName, " (", id, ")"),
        Label = name,
        description = description,
        Category = topicName,
        Subcategory = sourceName,
        `Measurement Type` = valueType,
        sourceUrl = paste0("https://population.un.org/wpp/Download/Standard/", topicName, "/"),
        is_favorite = as.character(id) %in% favorite_indicators,
        source = "UNWPP"
      ) %>%
      select(IndicatorId, display_label, Label, description, Category, Subcategory, `Measurement Type`,
             sourceUrl, is_favorite, source) %>%
      arrange(desc(is_favorite), Category, Label)

  }, error = function(e) {
    message("Error fetching UNWPP metadata from API, using fallback data: ", e$message)

    # Fallback to core indicators if API fails
    data.frame(
      IndicatorId = as.character(c(2, 22, 24, 41, 46, 47, 49, 55, 61, 19, 67, 62, 83, 84, 86, 72)),
      display_label = c("Modern CPR (2)", "IMR (22)", "U5MR (24)", "Women 15-49 (41)",
                        "U5 Pop (46)", "U1 Pop (47)", "Total Pop (49)", "CBR (55)",
                        "Life Expectancy (61)", "TFR (19)", "Median Age (67)", "Adult Mortality (62)",
                        "Child Dependency (83)", "Old Dependency (84)", "Total Dependency (86)", "Sex Ratio (72)"),
      Label = c("Modern contraceptive prevalence rate", "Infant mortality rate",
                "Under-five mortality rate", "Female population 15-49 years",
                "Total population under 5", "Total population under 1",
                "Total population", "Crude birth rate",
                "Life expectancy at birth", "Total fertility rate",
                "Median age of population", "Adult mortality rate (15-50)",
                "Child dependency ratio", "Old-age dependency ratio",
                "Total dependency ratio", "Sex ratio of total population"),
      description = c(
        "Modern contraceptive prevalence rate among women of reproductive age",
        "Infant mortality rate (deaths per 1,000 live births)",
        "Under-five mortality rate (deaths per 1,000 live births)",
        "Female population of reproductive age (15-49 years)",
        "Total population under 5 years of age",
        "Total population under 1 year of age (age 0)",
        "Total population by sex",
        "Crude birth rate (births per 1,000 population)",
        "Average number of years of life expected by a hypothetical cohort",
        "Total fertility rate (children per woman)",
        "Age that divides the population in two equal parts",
        "Probability of dying between fifteen and fiftieth birthdays",
        "Ratio of population under working age to working-age population",
        "Ratio of elderly population to working-age population",
        "Combined child and elderly dependency ratio",
        "Number of males per 100 females in total population"
      ),
      Category = c("Family Planning", "Mortality", "Mortality", "Population", "Population", "Population",
                   "Population", "Fertility", "Mortality", "Fertility", "Population", "Mortality",
                   "Population", "Population", "Population", "Population"),
      Subcategory = "World Population Prospects",
      `Measurement Type` = c("percent", "rate", "rate", "number", "number", "number", "number",
                            "rate", "number", "rate", "number", "rate", "ratio", "ratio", "ratio", "ratio"),
      sourceUrl = "https://population.un.org/wpp/Download/Standard/Population/",
      is_favorite = c(rep(TRUE, 8), rep(FALSE, 8)), # First 8 are favorites, last 8 are not
      source = "UNWPP",
      stringsAsFactors = FALSE
    )
  })
}

# ========================================
# INDICATOR LOOKUP TABLE
# ========================================

# Global indicator lookup table to preserve labels for plotting
get_all_indicators_lookup <- function() {
  # Fetch metadata from all sources
  dhs_metadata <- tryCatch(fetch_dhs_metadata(), error = function(e) data.frame())
  mics_metadata <- tryCatch(fetch_mics_metadata(), error = function(e) data.frame())
  unwpp_metadata <- tryCatch(fetch_unwpp_metadata(), error = function(e) data.frame())

  # Combine all metadata into a lookup table
  all_metadata <- bind_rows(dhs_metadata, mics_metadata, unwpp_metadata)

  # Create a simple lookup table with essential fields
  # Handle different column names across data sources
  lookup_table <- all_metadata %>%
    mutate(
      indicator_id = IndicatorId,
      indicator_label = Label,
      indicator_common_id = IndicatorId,  # Use IndicatorId as fallback
      source = source,
      category = if("Category" %in% names(all_metadata)) Category else "General"
    ) %>%
    select(indicator_id, indicator_label, indicator_common_id, source, category) %>%
    distinct()

  return(lookup_table)
}

# Function to get label for a specific indicator
get_indicator_label <- function(indicator_id, source = NULL) {
  if(is.null(indicator_id) || length(indicator_id) == 0) return(indicator_id)

  # Try to get from stored lookup table first
  if(exists("indicator_lookup", envir = .GlobalEnv)) {
    lookup <- get("indicator_lookup", envir = .GlobalEnv)
  } else {
    # Create lookup table if it doesn't exist
    lookup <- get_all_indicators_lookup()
    assign("indicator_lookup", lookup, envir = .GlobalEnv)
  }

  # Filter by source if provided
  if(!is.null(source)) {
    lookup <- lookup %>% filter(tolower(source) == tolower(!!source))
  }

  # Look up the label
  result <- lookup %>%
    filter(indicator_id == !!indicator_id) %>%
    pull(indicator_label)

  # Return original id if no match found
  if(length(result) == 0) return(indicator_id)
  return(result[1])
}

# ========================================
# DATA FETCHING FUNCTIONS
# ========================================

fetch_dhs_data <- function(indicators, countries, breakdown = "national") {
  all_data <- list()
  
  # Get fresh country mapping from API to ensure we have DHS codes
  tryCatch({
    url <- "https://api.dhsprogram.com/rest/dhs/countries?f=json"
    response <- GET(url)
    data_parsed <- fromJSON(content(response, as = "text"), flatten = TRUE)
    country_mapping <- data_parsed$Data %>%
      select(ISO2_CountryCode, DHS_CountryCode) %>%
      filter(!is.na(ISO2_CountryCode), !is.na(DHS_CountryCode))
    
  }, error = function(e) {
    message("Error getting country mapping: ", e$message)
    return(data.frame())
  })
  
  # Convert selected ISO2 codes to DHS codes
  selected_mapping <- country_mapping %>%
    filter(ISO2_CountryCode %in% countries)
  
  message("Country conversion mapping:")
  for(i in 1:nrow(selected_mapping)) {
    message("  ", selected_mapping$ISO2_CountryCode[i], " -> ", selected_mapping$DHS_CountryCode[i])
  }
  
  # Fetch data for each country
  for(i in 1:nrow(selected_mapping)) {
    iso_country <- selected_mapping$ISO2_CountryCode[i]
    dhs_country <- selected_mapping$DHS_CountryCode[i]
    
    message("Fetching data for ", iso_country, " using DHS code: ", dhs_country)
    
    tryCatch({
      data <- dhs_data(
        indicatorIds = indicators,
        countryIds = dhs_country,  # Use DHS code
        breakdown = breakdown,
        f = "json"
      )
      
      if(nrow(data) > 0) {
        # Data type conversions
        if("LevelRank" %in% names(data)) {
          data$LevelRank <- as.character(data$LevelRank)
        }
        if("IsPreferred" %in% names(data)) {
          data$IsPreferred <- as.logical(data$IsPreferred)
        }
        if("Value" %in% names(data)) {
          data$Value <- as.numeric(data$Value)
        }
        if("SurveyYear" %in% names(data)) {
          data$SurveyYear <- as.integer(data$SurveyYear)
        }
        
        data$country_iso <- iso_country
        all_data[[iso_country]] <- data
        message("Successfully fetched ", nrow(data), " records for ", iso_country)
      } else {
        message("No data returned for ", iso_country, " (DHS code: ", dhs_country, ")")
      }
    }, error = function(e) {
      message("Error fetching DHS data for ", iso_country, " (DHS code: ", dhs_country, "): ", e$message)
    })
  }
  
  if(length(all_data) > 0) {
    return(bind_rows(all_data))
  } else {
    return(data.frame())
  }
}

fetch_mics_data <- function(indicators, countries = NULL) {
  tryCatch({
    # Build country filter for SDMX URL
    if(!is.null(countries) && length(countries) > 0) {
      # Convert ISO2 to ISO3 for MICS SDMX API
      iso3_countries <- countrycode(countries, "iso2c", "iso3c", warn = FALSE)
      iso3_countries <- iso3_countries[!is.na(iso3_countries)]

      # Handle special cases like Côte d'Ivoire
      if("CI" %in% countries) {
        iso3_countries <- c(iso3_countries, "CIV")
      }

      country_filter <- paste(iso3_countries, collapse = "+")
      message("Filtering UNICEF data for countries: ", paste(countries, collapse = ", "))
      message("Using ISO3 codes: ", country_filter)
    } else {
      country_filter <- "."
      message("Fetching UNICEF data for all countries")
    }

    mics_url <- paste0(
      "https://sdmx.data.unicef.org/ws/public/sdmxapi/rest/data/UNICEF,GLOBAL_DATAFLOW,1.0/",
      country_filter, ".",
      paste(indicators, collapse = "+"),
      "..?startPeriod=2010&endPeriod=2024"
    )

    message("Fetching UNICEF data from: ", mics_url)
    mics_sdmx <- readSDMX(mics_url)
    mics_data <- as.data.frame(mics_sdmx)

    message("Successfully fetched ", nrow(mics_data), " UNICEF records for selected countries")
    return(mics_data)
    
  }, error = function(e) {
    message("Error fetching UNICEF data: ", e$message)
    # Return informative error data frame
    data.frame(
      Message = paste("UNICEF API Error:", e$message, "- Please try again or use DHS/UNWPP"),
      Error_Details = as.character(e$message),
      URL_Attempted = mics_url,
      stringsAsFactors = FALSE
    )
  })
}

# ========================================
# WUENIC MICS DATA FETCHER
# ========================================
# Parse WUENIC survey database Excel file for MICS immunization data
# This provides actual MICS survey data with full time series

fetch_wuenic_mics_data <- function(
    wuenic_file = "assets/survey-data_wuenic2024rev.xlsx",
    indicators = NULL,
    countries = NULL,
    evidence_type = "Record or Recall",
    source_filter = "MICS",
    use_latest_only = FALSE
) {

  if (!file.exists(wuenic_file)) {
    stop("WUENIC file not found at: ", wuenic_file)
  }

  message("Reading WUENIC survey database...")
  survey_data <- read_excel(wuenic_file, sheet = "Data", skip = 1)
  message("Loaded ", nrow(survey_data), " total survey records")

  # Filter by survey source
  if (source_filter == "MICS") {
    survey_data <- survey_data %>%
      filter(grepl("MICS|Multiple Indicator Cluster", surveyNameEnglish, ignore.case = TRUE))
    message("Filtered to ", nrow(survey_data), " MICS records")
  } else if (source_filter == "DHS") {
    survey_data <- survey_data %>%
      filter(grepl("DHS|Demographic and Health", surveyNameEnglish, ignore.case = TRUE))
    message("Filtered to ", nrow(survey_data), " DHS records")
  }

  # Filter by evidence type
  if (!is.null(evidence_type)) {
    survey_data <- survey_data %>%
      filter(grepl(evidence_type, evidence, ignore.case = FALSE))
    message("Filtered to ", nrow(survey_data), " records with evidence type: ", evidence_type)
  }

  # Map WUENIC vaccine codes to DHS-style indicator codes
  vaccine_mapping <- list(
    "BCG" = "CH_VACC_C_BCG",
    "DTP1" = "CH_VACC_C_PT1", "DTP2" = "CH_VACC_C_PT2", "DTP3" = "CH_VACC_C_PT3",
    "HEPB1" = "CH_VACC_C_HB1", "HEPB2" = "CH_VACC_C_HB2", "HEPB3" = "CH_VACC_C_HB3", "HEPBB" = "CH_VACC_C_HBB",
    "HIB1" = "CH_VACC_C_HI1", "HIB2" = "CH_VACC_C_HI2", "HIB3" = "CH_VACC_C_HI3",
    "POL0" = "CH_VACC_C_OP0", "POL1" = "CH_VACS_C_OP1", "POL2" = "CH_VACC_C_OP2", "POL3" = "CH_VACC_C_OP3",
    "IPV1" = "CH_VACC_C_IP1", "IPV2" = "CH_VACC_C_IP2",
    "MCV1" = "CH_VACC_C_MSL", "MCV2" = "CH_VACC_C_MS2",
    "RCV1" = "CH_VACC_C_MSL", "RCV2" = "CH_VACC_C_MS2",
    "PCV1" = "CH_VACC_C_PC1", "PCV2" = "CH_VACC_C_PC2", "PCV3" = "CH_VACC_C_PC3",
    "ROTA1" = "CH_VACC_C_RT1", "ROTA2" = "CH_VACC_C_RT2", "ROTAC" = "CH_VACC_C_RTC",
    "YFV" = "CH_VACC_C_YF",
    "FULL" = "CH_VACC_C_FUL"
  )

  # Filter by indicators
  if (!is.null(indicators)) {
    reverse_map <- setNames(names(vaccine_mapping), unlist(vaccine_mapping))
    wuenic_vaccines <- unique(unname(reverse_map[indicators]))
    wuenic_vaccines <- wuenic_vaccines[!is.na(wuenic_vaccines)]

    if (length(wuenic_vaccines) > 0) {
      survey_data <- survey_data %>%
        filter(vaccine %in% wuenic_vaccines)
      message("Filtered to ", length(wuenic_vaccines), " vaccine types: ", paste(wuenic_vaccines, collapse = ", "))
    }
  }

  # Filter by countries (convert ISO2 to ISO3 if needed)
  if (!is.null(countries) && length(countries) > 0) {
    # Check if countries are ISO2 (2 characters) or ISO3 (3 characters)
    if (all(nchar(countries) == 2)) {
      # Convert ISO2 to ISO3
      iso3_countries <- countrycode(countries, "iso2c", "iso3c", warn = FALSE)
      iso3_countries <- iso3_countries[!is.na(iso3_countries)]
      message("Converting ISO2 to ISO3: ", paste(countries, "->", iso3_countries, collapse = ", "))
      countries <- iso3_countries
    }

    survey_data <- survey_data %>%
      filter(ISO3 %in% countries)
    message("Filtered to ", length(countries), " countries: ", paste(countries, collapse = ", "))
  }

  # DEDUPLICATION: One record per country/vaccine/year
  survey_data <- survey_data %>%
    mutate(
      evidence_priority = case_when(
        evidence == "Record or Recall" ~ 1,
        grepl("^Record or Recall<", evidence) ~ 2,
        evidence == "Record" ~ 3,
        evidence == "Recall" ~ 4,
        TRUE ~ 5
      ),
      age_score = case_when(
        ageVaccination == "0-35 m" ~ 0,
        ageVaccination == "0-23 m" ~ 1,
        ageVaccination == "0-12 m" ~ 2,
        TRUE ~ 3
      ),
      validity_priority = ifelse(validity == "CRUDE", 1, 2)
    ) %>%
    group_by(ISO3, vaccine, cohortYear) %>%
    arrange(evidence_priority, age_score, validity_priority, desc(coverage)) %>%
    slice(1) %>%
    ungroup() %>%
    select(-evidence_priority, -age_score, -validity_priority)

  message("After deduplication: ", nrow(survey_data), " records")

  if (use_latest_only) {
    survey_data <- survey_data %>%
      group_by(ISO3, vaccine) %>%
      filter(cohortYear == max(cohortYear, na.rm = TRUE)) %>%
      ungroup()
    message("Selected latest year only: ", nrow(survey_data), " records")
  }

  # Transform to match app structure
  formatted_data <- survey_data %>%
    mutate(
      IndicatorId = sapply(vaccine, function(v) {
        ind <- vaccine_mapping[[v]]
        if (is.null(ind)) NA_character_ else ind
      }),
      Indicator = vaccine,
      SurveyYear = as.integer(cohortYear),
      CountryName = countrycode(ISO3, "iso3c", "country.name", warn = FALSE),
      DHS_CountryCode = ISO3,
      ISO2_CountryCode = countrycode(ISO3, "iso3c", "iso2c", warn = FALSE),
      Value = as.numeric(coverage),
      SurveyId = surveyNameProduction,
      SurveyType = ifelse(grepl("MICS", surveyNameEnglish), "MICS", "Other"),
      DataSource = source_filter,
      Evidence = evidence,
      Validity = validity,
      SampleSize = as.numeric(denominator),
      CardsSeenPct = as.numeric(cardsSeen),
      CharacteristicLabel = "National",
      CharacteristicCategory = "Total",
      CollectionStart = collectBegin,
      CollectionEnd = collectEnd,
      IsPreferred = TRUE
    ) %>%
    select(IndicatorId, Indicator, Value, SurveyYear, CountryName, DHS_CountryCode,
           ISO2_CountryCode, SurveyId, SurveyType, CharacteristicLabel,
           CharacteristicCategory, IsPreferred, DataSource, Evidence, Validity,
           SampleSize, CardsSeenPct, CollectionStart, CollectionEnd) %>%
    filter(!is.na(Value), !is.na(IndicatorId))

  message("Final dataset: ", nrow(formatted_data), " records across ",
          n_distinct(formatted_data$ISO2_CountryCode), " countries")

  return(formatted_data)
}

fetch_unwpp_data <- function(indicators, countries, start_year = 2020, end_year = 2025) {
  tryCatch({
    all_data <- list()
    counter <- 1
    
    # Get token like your working code
    unwpp_token <- Sys.getenv("UNWPP_TOKEN")
    if(unwpp_token == "") {
      message("UNWPP_TOKEN not found in .Renviron")
      return(data.frame())
    }
    
    headers <- c("Authorization" = unwpp_token)
    
    message("Fetching UNWPP data for ", length(countries), " countries, ", 
            length(indicators), " indicators, years ", start_year, "-", end_year)
    
    # First, get location IDs like in your working code
    message("Getting location list...")
    locations_df <- NULL
    tryCatch({
      base_url <- "https://population.un.org/dataportalapi/api/v1/locations/?pageSize=100&pageNumber="
      all_pages <- list()
      for(page in 1:3) {
        url <- paste0(base_url, page)
        res <- RCurl::getURL(url, .opts = list(httpheader = headers, followlocation = TRUE))
        parsed <- jsonlite::fromJSON(res, flatten = TRUE)
        if(!is.null(parsed$data)) {
          all_pages[[page]] <- parsed$data
        }
      }
      locations_df <- bind_rows(all_pages)
    }, error = function(e) {
      message("Could not fetch location list: ", e$message)
    })
    
    if(is.null(locations_df)) {
      message("No location data available")
      return(data.frame())
    }
    
    # Map country codes to location IDs
    target_locations <- locations_df %>%
      filter(iso2 %in% countries) %>%
      select(id, iso2, iso3, name)
    
    if(nrow(target_locations) == 0) {
      message("No matching locations found for countries: ", paste(countries, collapse = ", "))
      return(data.frame())
    }
    
    # Helper function exactly like your working code  
    fetch_wpp_data <- function(location_id, indicator_id, start_year, end_year) {
      url <- paste0(
        "https://population.un.org/dataportalapi/api/v1/data/indicators/", indicator_id,
        "/locations/", location_id,
        "/start/", start_year, "/end/", end_year, "?pagingInHeader=true&format=json"
      )
      
      response <- tryCatch({
        RCurl::getURL(url, .opts = list(httpheader = headers, followlocation = TRUE))
      }, error = function(e) return(NULL))
      
      if(is.null(response) || nchar(response) < 10) return(NULL)
      
      parsed <- tryCatch({
        jsonlite::fromJSON(response, flatten = TRUE)
      }, error = function(e) return(NULL))
      
      if(!is.null(parsed) && is.data.frame(parsed)) {
        return(parsed)
      } else {
        return(NULL)
      }
    }
    
    # Main loop like your working code - use location IDs
    for(indicator in indicators) {
      message("Fetching indicator ", indicator)
      
      for(i in 1:nrow(target_locations)) {
        loc <- target_locations[i, ]
        message("  → ", loc$name, " (", loc$iso2, ")")
        
        dat <- fetch_wpp_data(loc$id, indicator, start_year, end_year)
        
        if(!is.null(dat) && nrow(dat) > 0) {
          df <- as.data.frame(dat)
          df$indicator_id <- indicator
          df$country <- loc$iso2  # Use ISO2 code for consistency
          df$country_name <- loc$name
          all_data[[counter]] <- df
          counter <- counter + 1
        } else {
          message("    No data returned.")
        }
      }
    }
    
    if(length(all_data) > 0) {
      result <- bind_rows(all_data)
      message("Successfully fetched ", nrow(result), " UNWPP records")
      return(result)
    } else {
      message("No UNWPP data available")
      return(data.frame())
    }
    
  }, error = function(e) {
    message("Error in UNWPP data fetching: ", e$message)
    return(data.frame())
  })
}
