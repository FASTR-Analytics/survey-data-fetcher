# ============================================================
# Mozambique DHS integration into survey_data_unified.csv
# Last edit: 2026 Jun 04
# Appends cleaned DHS estimates (moz_dhs_cleaned.rds) to the unified DB.
#   1. Timestamped backup of the DB
#   2. Recode existing Mozambique WUENIC rows -> "MOÇAMBIQUE" (DHIS2 name)
#   3. Source-aware duplicate check (keeps DHS alongside WUENIC)
#   4. Append new DHS rows and write back
# ============================================================

setwd("/Users/claireboulange/Desktop/modules/06_survey_data_fetcher")
suppressMessages({ library(dplyr); library(readr) })

DB_PATH <- "/Users/claireboulange/Desktop/modules/survey_data_unified.csv"
DRY_RUN <- as.logical(Sys.getenv("DRY_RUN", "TRUE"))   # default safe: no write

db <- read_csv(DB_PATH, show_col_types = FALSE)
new <- as_tibble(readRDS("moz_dhs_cleaned.rds"))
message("DB rows: ", nrow(db), " | new DHS rows: ", nrow(new))

# Align column types/order to the DB
new <- new %>% mutate(year = as.integer(year)) %>% select(all_of(names(db)))

# --- 1. Backup ---
stamp <- format(Sys.time(), "%Y%m%d-%H%M%S")
backup_path <- paste0(DB_PATH, ".bak-", stamp)
if (!DRY_RUN) { write_csv(db, backup_path); message("Backup written: ", backup_path) }

# --- 2. Recode existing Mozambique WUENIC rows to DHIS2 country name ---
n_wuenic <- sum(db$admin_area_1 == "Mozambique", na.rm = TRUE)
db <- db %>% mutate(
  admin_area_1 = if_else(admin_area_1 == "Mozambique", "MOÇAMBIQUE", admin_area_1),
  country_name = if_else(country_name == "Mozambique", "MOÇAMBIQUE", country_name)
)
message("Recoded existing 'Mozambique' rows -> 'MOÇAMBIQUE': ", n_wuenic)

# --- 3. Source-aware duplicate check (a row is a dup only if source also matches) ---
key <- function(d) paste(d$admin_area_1, d$admin_area_2, d$year, d$indicator_common_id, d$source, sep = "||")
dup_mask <- key(new) %in% key(db)
message("New rows that already exist (same key+source): ", sum(dup_mask))
new_to_add <- new[!dup_mask, ]

# Report what coexists with WUENIC (informational, not a conflict)
overlap_wuenic <- new_to_add %>%
  filter(admin_area_2 == "NATIONAL", indicator_common_id %in% c("bcg","penta1","penta3")) %>%
  nrow()
message("DHS national bcg/penta1/penta3 rows that will coexist with WUENIC: ", overlap_wuenic)

# --- 4. Append + write ---
updated <- bind_rows(db, new_to_add)
cat("\n=== RESULT ===\n")
cat("Rows to add:", nrow(new_to_add), "\n")
cat("DB before:", nrow(db), " -> after:", nrow(updated), "\n")
cat("Mozambique (MOÇAMBIQUE) rows after:", sum(updated$admin_area_1 == "MOÇAMBIQUE"), "\n")
cat("  by source:\n"); print(table(updated$source[updated$admin_area_1 == "MOÇAMBIQUE"]))

if (!DRY_RUN) {
  write_csv(updated, DB_PATH)
  message("\nWROTE ", DB_PATH, " (", nrow(updated), " rows). Backup: ", backup_path)
} else {
  message("\nDRY RUN — no files written. Set DRY_RUN=FALSE to commit.")
}
