# ========================================
# Indicator Mappings and Auto-Labeling
# ========================================
# Purpose: Central place for indicator label management
# - Curated mappings for favorite indicators
# - Pattern-based auto-generation rules
# - Fallback generation for unknown indicators

library(stringr)

# ========================================
# CURATED FAVORITE INDICATOR LABELS
# ========================================
# These are manually curated for known/favorite indicators
# Format: indicator_id -> common_id

FAVORITE_DHS_LABELS <- list(
  # ANC and Maternal
  "RH_ANCP_W_SKP" = "anc1",
  "RH_ANCN_W_N4P" = "anc4",
  "RH_DELA_C_SKP" = "delivery",
  "RH_PNCP_W_2D" = "pnc1",
  "RH_ANCP_W_IRN" = "iron_anc",

  # Immunization
  "CH_VACC_C_BAS" = "fully_immunized",
  "CH_VACC_C_NON" = "no_immunization",
  "CH_VACC_C_BCG" = "bcg",
  # Penta/DTP - both code variants (DP = DTP, PT = Pentavalent)
  "CH_VACC_C_DP1" = "penta1",
  "CH_VACC_C_DP2" = "penta2",
  "CH_VACC_C_DP3" = "penta3",
  "CH_VACC_C_PT1" = "penta1",
  "CH_VACC_C_PT2" = "penta2",
  "CH_VACC_C_PT3" = "penta3",
  # Measles - both code variants
  "CH_VACC_C_MSL" = "measles1",
  "CH_VACC_C_MSL2" = "measles2",
  "CH_VACC_C_MS2" = "measles2",
  # Rotavirus - both code variants (ROT and RT)
  "CH_VACC_C_ROT1" = "rota1",
  "CH_VACC_C_ROT2" = "rota2",
  "CH_VACC_C_RT1" = "rota1",
  "CH_VACC_C_RT2" = "rota2",
  # Polio - both code variants (VACS and VACC)
  "CH_VACS_C_OP1" = "polio1",
  "CH_VACS_C_OP2" = "polio2",
  "CH_VACS_C_OP3" = "polio3",
  "CH_VACC_C_OP1" = "polio1",
  "CH_VACC_C_OP2" = "polio2",
  "CH_VACC_C_OP3" = "polio3",

  # Child Health
  "ML_FEVT_C_ADV" = "fever_treatment",
  "ML_ARIS_C_ADV" = "ari_treatment",
  "ML_DIAT_C_ORT" = "diarrhea_ort",
  "CH_NUT_C_WH2" = "wasting",
  "CH_NUT_C_HA2" = "stunting",
  "CH_NUT_C_WA2" = "underweight",

  # Family Planning
  "FP_CUSA_W_MOD" = "contraceptive_modern",
  "FP_CUSA_W_ANY" = "contraceptive_any",
  "FP_NADA_W_UNT" = "unmet_need",

  # HIV
  "HV_TEST_W_TST" = "hiv_test_women",
  "HV_KNWA_W_KNW" = "hiv_knowledge",

  # Malaria
  "ML_NETP_H_MPR" = "itn_access",
  "ML_NETP_H_MII" = "itn_use"
)

FAVORITE_UNICEF_LABELS <- list(
  # UNICEF SDMX indicator mappings
  "CME_MRM0" = "nmr",
  "CME_MRY0T4" = "imr",
  "CME_MRY0" = "u5mr",
  "IM_MCV1" = "measles1",
  "IM_MCV2" = "measles2",
  "MNCH_ANC4" = "anc4",
  "MNCH_SAB" = "delivery",
  "NT_CF_BF_1HR" = "early_breastfeeding",
  "NT_BF_EXBF" = "exclusive_breastfeeding"
)

FAVORITE_WUENIC_LABELS <- list(
  # WUENIC vaccine indicators
  "BCG" = "bcg",
  "DTP1" = "penta1",
  "DTP2" = "penta2",
  "DTP3" = "penta3",
  "MCV1" = "measles1",
  "MCV2" = "measles2",
  "POL1" = "polio1",
  "POL2" = "polio2",
  "POL3" = "polio3",
  "HepB3" = "hepb3",
  "Hib3" = "hib3",
  "PCV3" = "pcv3",
  "RCV1" = "rubella1",
  "RotaC" = "rotavirus_complete",
  "YFV" = "yellow_fever"
)

# UN World Population Prospects (Data Portal API) indicator ids.
#
# CORRECTED 2026 Jul 14 against the live API
# (GET https://population.un.org/dataportalapi/api/v1/indicators/ — 86 indicators).
# 13 of the previous 15 entries pointed at the WRONG indicator, and four of them
# (13, 14, 15, 16) pointed at ids that DO NOT EXIST. Nothing had written bad rows,
# because clean_unwpp_data() filters by its own (correct) ids and never consults this
# table — but it is the reference anyone reads, and it was lying. Examples of what the
# old ids actually were:
#     68 -> "Fertility rates by age of mother"   (was labelled imr)
#     47 -> "Population by 1-year age groups"    (was labelled crudebr)
#     58 -> "Sex ratio at birth"                 (was labelled u5mr)
#     52 -> "Natural change of population"       (was labelled womenrepage)
#     80 -> "Age specific mortality rate"        (was labelled livebirth)
#      1 -> "Contraceptive prevalence: ANY method" (was labelled mcpr — that is id 2)
#
# The official name is on every line. Verify against the API before changing any of them.
FAVORITE_UNWPP_LABELS <- list(
  "49" = "poptot",                # Total population by sex
  "41" = "womenrepage",           # Female population of reproductive age (15-49 years)
  "55" = "crudebr",               # Crude birth rate
  "57" = "livebirth",             # Total births by sex
  "19" = "total_fertility_rate",  # Total fertility rate
  "22" = "imr",                   # Infant mortality rate (IMR)
  "24" = "u5mr",                  # Under-five mortality rate (U5MR)
  "61" = "lifeexp",               # Life expectancy at birth
  "62" = "adultmort",             # Probability of dying 15-50 (35q15)
  "67" = "medianage",             # Median age of population
  "72" = "sexratio",              # Sex ratio of the total population
  "83" = "childdep",              # Child dependency ratio
  "84" = "olddep",                # Old-age dependency ratio
  "86" = "totdep",                # Total dependency ratio

  # Contraceptive prevalence: 1 and 2 are DIFFERENT indicators. mCPR is the MODERN one.
  "2"  = "mcpr",                  # Contraceptive prevalence: Any MODERN method
  "1"  = "contraceptive_any"      # Contraceptive prevalence: Any method

  # NOT listed here on purpose: ids 46 and 47 ("Population by 5-year / 1-year age groups
  # and sex"). They are raw age-structure inputs from which clean_unwpp_data() DERIVES
  # both totu1pop and totu5pop by filtering ages. One id cannot map to one common_id, so
  # a 1:1 label here would be wrong. They are still fetched — see the favourites list in
  # app.R: c("2","22","24","41","46","47","49","55").
)

# ========================================
# PATTERN-BASED AUTO-GENERATION RULES
# ========================================
# These patterns help auto-generate labels for unknown indicators
# Format: list(pattern = regex, label = common_id)

AUTO_LABEL_PATTERNS <- list(
  # Immunization patterns
  list(pattern = "(?i)DTP3|DPT3|Penta.*3", label = "penta3"),
  list(pattern = "(?i)DTP2|DPT2|Penta.*2", label = "penta2"),
  list(pattern = "(?i)DTP1|DPT1|Penta.*1", label = "penta1"),
  list(pattern = "(?i)Measles.*1st|MCV1", label = "measles1"),
  list(pattern = "(?i)Measles.*2nd|MCV2", label = "measles2"),
  list(pattern = "(?i)BCG", label = "bcg"),
  list(pattern = "(?i)Polio.*1|POL1", label = "polio1"),
  list(pattern = "(?i)Polio.*2|POL2", label = "polio2"),
  list(pattern = "(?i)Polio.*3|POL3", label = "polio3"),
  list(pattern = "(?i)Rotavirus", label = "rotavirus"),
  list(pattern = "(?i)Pneumococcal|PCV", label = "pcv"),
  list(pattern = "(?i)Hepatitis.*B|HepB", label = "hepb"),

  # Maternal Health patterns
  list(pattern = "(?i)ANC.*4.*visit|Antenatal.*4", label = "anc4"),
  list(pattern = "(?i)ANC.*1.*visit|Antenatal.*1st", label = "anc1"),
  list(pattern = "(?i)Skilled.*delivery|Delivery.*skilled", label = "delivery"),
  list(pattern = "(?i)Postnatal.*care|PNC", label = "pnc"),
  list(pattern = "(?i)Iron.*supplement|Iron.*ANC", label = "iron_anc"),

  # Child Health patterns
  list(pattern = "(?i)Under.?five.*mortality|U5MR", label = "u5mr"),
  list(pattern = "(?i)Infant.*mortality|IMR", label = "imr"),
  list(pattern = "(?i)Neonatal.*mortality|NMR", label = "nmr"),
  list(pattern = "(?i)Stunting|Height.*age", label = "stunting"),
  list(pattern = "(?i)Wasting|Weight.*height", label = "wasting"),
  list(pattern = "(?i)Underweight|Weight.*age", label = "underweight"),
  list(pattern = "(?i)Exclusive.*breastfeeding", label = "exclusive_breastfeeding"),
  list(pattern = "(?i)Early.*breastfeeding", label = "early_breastfeeding"),

  # Family Planning patterns
  list(pattern = "(?i)Modern.*contracepti|Contracepti.*modern", label = "contraceptive_modern"),
  list(pattern = "(?i)Contracepti.*prevalence|CPR", label = "contraceptive_any"),
  list(pattern = "(?i)Unmet.*need", label = "unmet_need"),

  # Disease patterns
  list(pattern = "(?i)Fever.*treatment", label = "fever_treatment"),
  list(pattern = "(?i)Diarr?h?ea.*ORT|ORT.*diarr?h?ea", label = "diarrhea_ort"),
  list(pattern = "(?i)ARI.*treatment|Pneumonia.*treatment", label = "ari_treatment"),
  list(pattern = "(?i)Malaria.*ITN|ITN.*use", label = "itn_use"),
  list(pattern = "(?i)HIV.*test", label = "hiv_test"),

  # Population patterns
  list(pattern = "(?i)Total.*population", label = "poptot"),
  list(pattern = "(?i)Life.*expectancy", label = "lifeexp"),
  list(pattern = "(?i)Total.*fertility.*rate|TFR", label = "total_fertility_rate"),
  list(pattern = "(?i)Crude.*birth.*rate", label = "crudebr"),
  list(pattern = "(?i)Live.*births?", label = "livebirth"),
  list(pattern = "(?i)Median.*age", label = "medianage")
)

# ========================================
# MAIN AUTO-GENERATION FUNCTION
# ========================================

#' Get or generate a common indicator ID
#'
#' @param indicator_id Original indicator ID from source
#' @param indicator_label Human-readable label
#' @param source Data source (DHS, UNICEF, WUENIC, UNWPP)
#' @return Standardized common indicator ID
#'
get_or_generate_common_id <- function(indicator_id, indicator_label = NULL, source = "unknown") {

  # Handle NULL or empty input
  if (is.null(indicator_id) || indicator_id == "" || is.na(indicator_id)) {
    if (!is.null(indicator_label) && indicator_label != "" && !is.na(indicator_label)) {
      indicator_id <- indicator_label
    } else {
      return("unknown")
    }
  }

  # Step 1: Check if it's in curated favorites
  favorite_lookup <- switch(tolower(source),
    "dhs" = FAVORITE_DHS_LABELS,
    "mics" = FAVORITE_UNICEF_LABELS,  # Keep "mics" for backwards compatibility
    "unicef" = FAVORITE_UNICEF_LABELS,
    "wuenic" = FAVORITE_WUENIC_LABELS,
    "unwpp" = FAVORITE_UNWPP_LABELS,
    list()  # Default empty list
  )

  if (indicator_id %in% names(favorite_lookup)) {
    return(favorite_lookup[[indicator_id]])
  }

  # Step 2: Try pattern matching on the label
  if (!is.null(indicator_label) && indicator_label != "" && !is.na(indicator_label)) {
    for (pattern_rule in AUTO_LABEL_PATTERNS) {
      if (grepl(pattern_rule$pattern, indicator_label, perl = TRUE)) {
        return(pattern_rule$label)
      }
    }
  }

  # Step 3: Fallback - generate from label or ID
  label_to_use <- if (!is.null(indicator_label) && indicator_label != "" && !is.na(indicator_label)) {
    indicator_label
  } else {
    indicator_id
  }

  generated_label <- generate_indicator_common_id(label_to_use, source)
  return(generated_label)
}

#' Generate a standardized indicator ID from a label
#'
#' @param label Raw indicator label
#' @param source Data source prefix
#' @return Generated common indicator ID
#'
generate_indicator_common_id <- function(label, source = "unknown") {

  if (is.null(label) || label == "" || is.na(label)) {
    return(paste0(tolower(source), "_unknown"))
  }

  # Clean the label
  auto_label <- label %>%
    # Remove special characters
    str_remove_all("[\\(\\)%\\[\\]{}]") %>%
    # Remove colons and commas
    str_replace_all("[,:]", "") %>%
    # Replace multiple spaces with single space
    str_squish() %>%
    # Replace spaces with underscores
    str_replace_all("\\s+", "_") %>%
    # Convert to lowercase
    str_to_lower() %>%
    # Remove any remaining special chars except underscore
    str_replace_all("[^a-z0-9_]", "") %>%
    # Limit length
    str_sub(1, 50) %>%
    # Remove trailing/leading underscores
    str_remove("^_+|_+$")

  # Add source prefix if not already present
  if (!grepl(paste0("^", tolower(source)), auto_label)) {
    auto_label <- paste0(tolower(source), "_", auto_label)
  }

  return(auto_label)
}

#' Batch process multiple indicators
#'
#' @param indicator_df Data frame with indicator_id, indicator_label, source columns
#' @return Data frame with added indicator_common_id column
#'
add_common_ids <- function(indicator_df) {

  if (!"indicator_id" %in% names(indicator_df)) {
    stop("indicator_df must have 'indicator_id' column")
  }

  # Set defaults for missing columns
  if (!"indicator_label" %in% names(indicator_df)) {
    indicator_df$indicator_label <- indicator_df$indicator_id
  }

  if (!"source" %in% names(indicator_df)) {
    indicator_df$source <- "unknown"
  }

  # Generate common IDs
  indicator_df$indicator_common_id <- mapply(
    get_or_generate_common_id,
    indicator_df$indicator_id,
    indicator_df$indicator_label,
    indicator_df$source,
    SIMPLIFY = TRUE,
    USE.NAMES = FALSE
  )

  return(indicator_df)
}

# ========================================
# INDICATOR CATEGORIES
# ========================================
# Categories for organizing indicators in manual entry

INDICATOR_CATEGORIES <- list(
  "ANC & Maternal Health" = c(
    "anc1", "anc4", "delivery", "pnc1", "iron_anc"
  ),
  "Immunization" = c(
    "bcg", "penta1", "penta2", "penta3", "measles1", "measles2",
    "rota1", "rota2", "rotavirus_complete",
    "polio1", "polio2", "polio3",
    "fully_immunized", "no_immunization",
    "hepb3", "pcv3", "hib3", "yellow_fever", "rubella1"
  ),
  "Child Health & Nutrition" = c(
    "stunting", "wasting", "underweight",
    "exclusive_breastfeeding", "early_breastfeeding",
    "fever_treatment", "ari_treatment", "diarrhea_ort"
  ),
  "Mortality Rates" = c(
    "imr", "nmr", "u5mr", "mmr"
  ),
  "Family Planning" = c(
    "contraceptive_modern", "contraceptive_any", "unmet_need", "mcpr"
  ),
  "Malaria" = c(
    "itn_access", "itn_use", "iptp1", "iptp2", "iptp3"
  ),
  "HIV/AIDS" = c(
    "hiv_test", "hiv_test_women", "hiv_knowledge"
  ),
  "Population & Demographics" = c(
    "poptot", "popu5", "totu1pop", "totu5pop",
    "livebirth", "womenrepage", "popgrowth",
    "total_fertility_rate", "crudebr", "lifeexp", "adultmort",
    "childdep", "olddep", "totdep", "sexratio", "medianage"
  )
)

#' Get indicator categories for dropdown
#' @return Named vector of category names
#' @export
get_indicator_categories <- function() {
  cats <- names(INDICATOR_CATEGORIES)
  setNames(cats, cats)
}

#' Get indicators by category
#' @param category Category name
#' @return Vector of indicator common IDs in that category
#' @export
get_indicators_by_category <- function(category) {
  if (is.null(category) || category == "" || category == "All") {
    # Return all indicators
    return(sort(unique(unlist(INDICATOR_CATEGORIES))))
  }

  if (category %in% names(INDICATOR_CATEGORIES)) {
    return(INDICATOR_CATEGORIES[[category]])
  }

  return(character(0))
}

# ========================================
# MANUAL ENTRY HELPER FUNCTIONS
# ========================================

#' Get all valid indicator common IDs for manual entry dropdown
#'
#' @return Named vector suitable for selectInput choices
#' @export
get_all_valid_indicator_ids <- function() {
  # Collect unique common_ids from all sources
  all_common_ids <- unique(c(
    unlist(FAVORITE_DHS_LABELS, use.names = FALSE),
    unlist(FAVORITE_UNICEF_LABELS, use.names = FALSE),
    unlist(FAVORITE_WUENIC_LABELS, use.names = FALSE),
    unlist(FAVORITE_UNWPP_LABELS, use.names = FALSE)
  ))

  # Remove duplicates and sort
  all_common_ids <- sort(unique(all_common_ids))

  # Create named vector for selectInput (value = value for simplicity)
  choices <- setNames(all_common_ids, all_common_ids)

  return(choices)
}

#' Get indicator type based on common_id
#'
#' @param common_id The indicator common ID
#' @return Character string: "percent", "rate", "population_estimate", or "other"
#' @export
get_indicator_type <- function(common_id) {
  percentage_indicators <- c(
    "anc1", "anc4", "delivery", "pnc1", "iron_anc",
    "bcg", "penta1", "penta2", "penta3", "measles1", "measles2",
    "rota1", "rota2", "rotavirus_complete", "polio1", "polio2", "polio3",
    "fully_immunized", "no_immunization", "iptp1", "iptp2", "iptp3",
    "mcpr", "hepb3", "pcv3", "hib3", "yellow_fever", "rubella1",
    "contraceptive_modern", "contraceptive_any", "unmet_need",
    "itn_access", "itn_use", "hiv_test", "hiv_test_women", "hiv_knowledge",
    "fever_treatment", "ari_treatment", "diarrhea_ort",
    "exclusive_breastfeeding", "early_breastfeeding",
    "stunting", "wasting", "underweight"
  )

  rate_indicators <- c(
    "imr", "nmr", "u5mr", "mmr", "total_fertility_rate", "crudebr",
    "lifeexp", "adultmort", "childdep", "olddep", "totdep", "sexratio", "medianage"
  )

  population_indicators <- c(
    "poptot", "popu5", "totu1pop", "totu5pop",
    "livebirth", "womenrepage", "popgrowth"
  )

  if (common_id %in% percentage_indicators) return("percent")
  if (common_id %in% rate_indicators) return("rate")
  if (common_id %in% population_indicators) return("population_estimate")
  return("other")
}
