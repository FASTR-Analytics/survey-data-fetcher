# ============================================================
# Uganda integration (CLEAN REPLACE) into the unified DBs
# Last edit: 2026 Jun 04
# Removes ALL existing admin_area_1=="Uganda" rows (old un-harmonised load)
# from both DBs, then adds the backbone-harmonised DHS + UNWPP load:
#   - admin_area_1 = "MOH - Uganda", 15 sub-regions, national for all surveys
#   - POP_INDICATORS -> population_estimates_only.csv; rest -> survey_data_unified.csv
# Bases off pristine origin CSVs (/tmp/base_*.csv) and preserves per-file quoting
# (survey = write.csv style, pop = readr::write_csv style). DRY_RUN gated.
# ============================================================
setwd("/Users/claireboulange/Desktop/modules/06_survey_data_fetcher")
suppressMessages({ library(dplyr); library(readr); library(stringr); library(countrycode) })
source("R/indicator_mappings.R"); source("R/cleaning_functions.R"); source("R/integration_functions.R")

DRY_RUN <- as.logical(Sys.getenv("DRY_RUN", "TRUE"))
SURVEY_PATH <- "/Users/claireboulange/Desktop/modules/survey_data_unified.csv"
POP_PATH    <- "/Users/claireboulange/Desktop/modules/population_estimates_only.csv"

# --- New harmonised Uganda data ---
dhs <- as_tibble(readRDS("uganda_dhs_cleaned.rds"))
unwpp <- clean_unwpp_data(as_tibble(readRDS("uganda_unwpp_raw.rds")), apply_fastr_standardization = TRUE)
new <- bind_rows(dhs, unwpp)
stopifnot(all(new$admin_area_1 == "MOH - Uganda"))
# guard: no duplicate keys in the new data (source-aware — DHS & UNWPP crudebr/imr
# legitimately coexist for the same area/year, distinguished by source)
stopifnot(nrow(new %>% count(admin_area_1,admin_area_2,year,indicator_common_id,source) %>% filter(n>1)) == 0)

new_pop    <- new %>% filter(indicator_common_id %in% POP_INDICATORS)
new_survey <- new %>% filter(!indicator_common_id %in% POP_INDICATORS)
cat("new Uganda -> survey:", nrow(new_survey), " pop:", nrow(new_pop), "\n")

# --- Base = pristine origin (has Mozambique, correct quoting) ---
base_sv <- read.csv("/tmp/base_survey.csv", stringsAsFactors=FALSE, check.names=FALSE)
base_pp <- read_csv("/tmp/base_pop.csv", show_col_types=FALSE)

# Replace ONLY the sources we regenerate (DHS + UNWPP). Preserve other existing
# Uganda sources (WUENIC, MICS, etc.) — they can't be re-fetched — and recode
# their admin_area_1 to "MOH - Uganda" for consistency with the backbone.
REGEN <- c("DHS National", "DHS Sub-national", "UNWPP")
uga_sv  <- base_sv$admin_area_1 %in% c("Uganda","MOH - Uganda")
uga_pp  <- base_pp$admin_area_1 %in% c("Uganda","MOH - Uganda")
drop_sv <- uga_sv & base_sv$source %in% REGEN
drop_pp <- uga_pp & base_pp$source %in% REGEN
cat("removing regenerated Uganda -> survey:", sum(drop_sv), " pop:", sum(drop_pp), "\n")
cat("preserving other Uganda sources -> survey:", sum(uga_sv & !drop_sv),
    " (", paste(unique(base_sv$source[uga_sv & !drop_sv]), collapse=","), ")",
    " | pop:", sum(uga_pp & !drop_pp),
    " (", paste(unique(base_pp$source[uga_pp & !drop_pp]), collapse=","), ")\n")

# recode preserved Uganda rows to MOH - Uganda
base_sv$admin_area_1[uga_sv] <- "MOH - Uganda"; base_sv$country_name[uga_sv] <- "MOH - Uganda"
base_pp$admin_area_1[uga_pp] <- "MOH - Uganda"; base_pp$country_name[uga_pp] <- "MOH - Uganda"

out_sv <- rbind(base_sv[!drop_sv, ], as.data.frame(new_survey)[, names(base_sv)])
out_pp <- bind_rows(base_pp[!drop_pp, ], new_pop[, names(base_pp)])

cat("\n=== RESULT ===\n")
cat("survey:", nrow(base_sv), "->", nrow(out_sv), "\n")
cat("pop:   ", nrow(base_pp), "->", nrow(out_pp), "\n")
cat("Uganda survey by source:\n"); print(table(out_sv$source[out_sv$admin_area_1=="MOH - Uganda"]))
cat("Uganda pop by indicator:\n"); print(table(out_pp$indicator_common_id[out_pp$admin_area_1=="MOH - Uganda"]))

if (!DRY_RUN) {
  write.csv(out_sv, SURVEY_PATH, row.names=FALSE, na="")
  write_csv(out_pp, POP_PATH)
  message("\nWrote both DBs (working tree).")
} else {
  # write to tmp so we can diff
  write.csv(out_sv, "/tmp/out_survey.csv", row.names=FALSE, na="")
  write_csv(out_pp, "/tmp/out_pop.csv")
  message("\nDRY RUN — wrote /tmp/out_*.csv for diff inspection.")
}
