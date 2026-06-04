# ============================================================
# Mozambique UNWPP integration
# Last edit: 2026 Jun 04
# Cleans UNWPP estimates (moz_unwpp_raw.rds) and routes them:
#   - population indicators -> population_estimates_only.csv
#   - other indicators (imr, u5mr, mcpr, lifeexp) -> survey_data_unified.csv
# Backs up both files, source-aware dedup, append. DRY_RUN gated.
# ============================================================

setwd("/Users/claireboulange/Desktop/modules/06_survey_data_fetcher")
suppressMessages({ library(dplyr); library(readr); library(stringr); library(countrycode) })
source("R/indicator_mappings.R"); source("R/cleaning_functions.R"); source("R/integration_functions.R")

SURVEY_PATH <- "/Users/claireboulange/Desktop/modules/survey_data_unified.csv"
POP_PATH    <- "/Users/claireboulange/Desktop/modules/population_estimates_only.csv"
DRY_RUN <- as.logical(Sys.getenv("DRY_RUN", "TRUE"))

raw <- as_tibble(readRDS("moz_unwpp_raw.rds"))
cleaned <- clean_unwpp_data(raw, apply_fastr_standardization = TRUE)
message("Cleaned UNWPP rows: ", nrow(cleaned))
cat("\nindicator_common_id breakdown:\n"); print(table(cleaned$indicator_common_id))
cat("admin_area_1:", paste(unique(cleaned$admin_area_1), collapse=", "), "\n")
cat("year range:", paste(range(cleaned$year, na.rm=TRUE), collapse="-"), "\n")

# --- Route ---
pop_new    <- cleaned %>% filter(indicator_common_id %in% POP_INDICATORS)
survey_new <- cleaned %>% filter(!indicator_common_id %in% POP_INDICATORS)
message("Routed -> population: ", nrow(pop_new), " | survey: ", nrow(survey_new))

key <- function(d) paste(d$admin_area_1, d$admin_area_2, d$year, d$indicator_common_id, d$source, sep="||")
stamp <- format(Sys.time(), "%Y%m%d-%H%M%S")

integrate_one <- function(db_path, new) {
  if (nrow(new) == 0) return(invisible())
  db <- read_csv(db_path, show_col_types = FALSE)
  new <- new %>% mutate(year = as.integer(year)) %>% select(all_of(names(db)))
  dup <- key(new) %in% key(db)
  add <- new[!dup, ]
  updated <- bind_rows(db, add)
  cat("\n[", basename(db_path), "] before:", nrow(db),
      " dups:", sum(dup), " add:", nrow(add), " after:", nrow(updated), "\n")
  if (!DRY_RUN) {
    write_csv(db, paste0(db_path, ".bak-", stamp))
    write_csv(updated, db_path)
    message("  wrote ", db_path)
  }
}

integrate_one(POP_PATH, pop_new)
integrate_one(SURVEY_PATH, survey_new)

if (DRY_RUN) message("\nDRY RUN — no files written. Set DRY_RUN=FALSE to commit.")
