# ============================================================
# Fix: move misrouted Mozambique DHS pop-indicators
# Last edit: 2026 Jun 04
# moz_dhs_integrate.R appended ALL DHS rows to survey_data_unified.csv,
# but POP_INDICATORS (crudebr, womenrepage) belong in
# population_estimates_only.csv (matches Niger/Madagascar/CI convention).
# This moves MOÇAMBIQUE DHS crudebr/womenrepage rows survey -> pop.
# ============================================================
setwd("/Users/claireboulange/Desktop/modules/06_survey_data_fetcher")
suppressMessages({ library(dplyr); library(readr) })

SURVEY_PATH <- "/Users/claireboulange/Desktop/modules/survey_data_unified.csv"
POP_PATH    <- "/Users/claireboulange/Desktop/modules/population_estimates_only.csv"
DRY_RUN <- as.logical(Sys.getenv("DRY_RUN", "TRUE"))
POP_INDICATORS <- c("poptot","popu5","totu1pop","totu5pop","livebirth","womenrepage","popgrowth","crudebr")

sv <- read_csv(SURVEY_PATH, show_col_types = FALSE)
pp <- read_csv(POP_PATH, show_col_types = FALSE)

move_mask <- sv$admin_area_1 == "MOÇAMBIQUE" &
             sv$indicator_common_id %in% POP_INDICATORS &
             grepl("DHS", sv$source)
to_move <- sv[move_mask, ]
cat("Rows to move survey -> pop:", nrow(to_move), "\n")
print(to_move %>% count(indicator_common_id, source))

# source-aware dedup against pop DB (should be 0 — UNWPP only there so far)
key <- function(d) paste(d$admin_area_1,d$admin_area_2,d$year,d$indicator_common_id,d$source,sep="||")
dup <- key(to_move) %in% key(pp)
cat("already-in-pop duplicates:", sum(dup), "\n")
to_move <- to_move[!dup, ] %>% select(all_of(names(pp)))

sv_new <- sv[!move_mask, ]
pp_new <- bind_rows(pp, to_move)

cat("\n=== RESULT ===\n")
cat("survey:", nrow(sv), "->", nrow(sv_new), " (", nrow(sv)-nrow(sv_new), "removed)\n")
cat("pop:   ", nrow(pp), "->", nrow(pp_new), " (", nrow(pp_new)-nrow(pp), "added)\n")
cat("survey DB MOÇAMBIQUE pop-indicators remaining (should be 0):",
    sum(sv_new$admin_area_1=="MOÇAMBIQUE" & sv_new$indicator_common_id %in% POP_INDICATORS), "\n")

if (!DRY_RUN) {
  stamp <- format(Sys.time(), "%Y%m%d-%H%M%S")
  write_csv(sv, paste0(SURVEY_PATH, ".bak-", stamp))
  write_csv(pp, paste0(POP_PATH, ".bak-", stamp))
  write_csv(sv_new, SURVEY_PATH)
  write_csv(pp_new, POP_PATH)
  message("Wrote both DBs. Backups stamped ", stamp)
} else {
  message("DRY RUN — no files written.")
}
