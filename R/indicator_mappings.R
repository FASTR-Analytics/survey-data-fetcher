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
  "CH_VACC_C_BAS" = "basic_immunization",
  "CH_VACC_C_NON" = "no_immunization",
  "CH_VACC_C_BCG" = "bcg",
  "CH_VACC_C_DP1" = "penta1",
  "CH_VACC_C_DP2" = "penta2",
  "CH_VACC_C_DP3" = "penta3",
  "CH_VACC_C_MSL" = "measles1",
  "CH_VACC_C_MSL2" = "measles2",
  "CH_VACC_C_ROT1" = "rotavirus1",
  "CH_VACC_C_ROT2" = "rotavirus2",
  "CH_VACS_C_OP1" = "polio1",
  "CH_VACS_C_OP2" = "polio2",
  "CH_VACS_C_OP3" = "polio3",

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

FAVORITE_UNWPP_LABELS <- list(
  # UN World Population Prospects indicators
  "49" = "poptot",
  "68" = "imr",
  "80" = "livebirth",
  "19" = "tfr",
  "58" = "u5mr",
  "52" = "womenrepage",
  "47" = "crudebr",
  "69" = "lifeexp",
  "1" = "mcpr",
  "16" = "medianage",
  "70" = "adultmort",
  "12" = "childdep",
  "13" = "olddep",
  "14" = "totdep",
  "15" = "sexratio"
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
  list(pattern = "(?i)Total.*fertility.*rate|TFR", label = "tfr"),
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
