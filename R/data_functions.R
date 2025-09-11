# ========================================
# DATA FETCHING AND METADATA FUNCTIONS
# ========================================
# File: R/data_functions.R
# Purpose: All functions for fetching data from DHS, MICS, and UNWPP APIs

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
        source = "MICS",
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
        source = "MICS",
        continent = countrycode(country_code, "iso2c", "continent", warn = FALSE),
        region = countrycode(country_code, "iso2c", "region", warn = FALSE)
      ) %>%
      filter(!is.na(country_name)) %>%
      arrange(country_name)
    
    return(mics_countries)
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
        display_label = paste0(ShortName, " (", IndicatorId, ")"),
        is_favorite = IndicatorId %in% favorites,
        source = "DHS",
        full_definition = ifelse(is.na(Definition) | Definition == "", Label, Definition)
      ) %>%
      select(IndicatorId, display_label, Label, full_definition, is_favorite, source) %>%
      arrange(desc(is_favorite), display_label)
  }, error = function(e) {
    message("Error fetching DHS metadata: ", e$message)
    return(data.frame())
  })
}

fetch_mics_metadata <- function() {
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
    full_definition = c("Neonatal mortality rate (deaths per 1,000 live births)",
                        "Infant mortality rate (deaths per 1,000 live births)",
                        "BCG vaccination coverage among children 12-23 months",
                        "DTP1 vaccination coverage among children 12-23 months",
                        "DTP3 vaccination coverage among children 12-23 months",
                        "ANC1 coverage (at least 1 visit)", "ANC4+ coverage (at least 4 visits)",
                        "Institutional delivery rate", "PNC coverage for mother within 2 days",
                        "ORS and Zinc treatment for diarrhea"),
    is_favorite = TRUE,
    source = "MICS",
    stringsAsFactors = FALSE
  )
}

prepare_unwpp_metadata <- function() {
  data.frame(
    IndicatorId = as.character(c(2, 22, 24, 41, 46, 47, 49, 55)),
    display_label = c("Modern CPR (2)", "IMR (22)", "U5MR (24)", "Women 15-49 (41)",
                      "U5 Pop (46)", "U1 Pop (47)", "Total Pop (49)", "CBR (55)"),
    Label = c("Modern contraceptive prevalence rate", "Infant mortality rate",
              "Under-five mortality rate", "Female population 15-49 years",
              "Total population under 5", "Total population under 1",
              "Total population", "Crude birth rate"),
    full_definition = c(
      "Modern contraceptive prevalence rate among women of reproductive age",
      "Infant mortality rate (deaths per 1,000 live births)",
      "Under-five mortality rate (deaths per 1,000 live births)",
      "Female population of reproductive age (15-49 years)",
      "Total population under 5 years of age",
      "Total population under 1 year of age (age 0)",
      "Total population by sex",
      "Crude birth rate (births per 1,000 population)"
    ),
    is_favorite = TRUE,
    source = "UNWPP",
    stringsAsFactors = FALSE
  )
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
    # MICS SDMX doesn't support country filtering in URL, so fetch all and filter later
    mics_url <- paste0(
      "https://sdmx.data.unicef.org/ws/public/sdmxapi/rest/data/UNICEF,GLOBAL_DATAFLOW,1.0/.",
      paste(indicators, collapse = "+"),
      "..?startPeriod=2010&endPeriod=2024"
    )
    
    message("Fetching MICS data from: ", mics_url)
    mics_sdmx <- readSDMX(mics_url)
    mics_data <- as.data.frame(mics_sdmx)
    
    message("Successfully fetched ", nrow(mics_data), " MICS records (will filter by country during cleaning)")
    return(mics_data)
    
  }, error = function(e) {
    message("Error fetching MICS data: ", e$message)
    # Return informative error data frame
    data.frame(
      Message = paste("MICS API Error:", e$message, "- Please try again or use DHS/UNWPP"),
      Error_Details = as.character(e$message),
      URL_Attempted = mics_url,
      stringsAsFactors = FALSE
    )
  })
}

fetch_unwpp_data <- function(indicators, countries, start_year = 2020, end_year = 2025) {
  # For demo purposes, return simulated data
  # In production, this would connect to actual UNWPP API
  data.frame(
    indicator_id = rep(indicators, each = length(countries)),
    country = rep(countries, length(indicators)),
    year = 2023,
    value = runif(length(indicators) * length(countries), 10, 100),
    stringsAsFactors = FALSE
  )
}
