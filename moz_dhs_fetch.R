# ============================================================
# Mozambique DHS survey fetch (headless replica of the Shiny app)
# Last edit: 2026 Jun 04
# Fetches DHS estimates (favorites) for Mozambique via the DHS REST API
# (national + subnational breakdowns, the way the app does), cleans with
# clean_dhs_data(), applies FASTR name standardization, validates province
# names against assets/mozambique_backbone.csv, and writes moz_dhs_cleaned.rds.
# Does NOT modify the unified database.
# ============================================================

setwd("/Users/claireboulange/Desktop/modules/06_survey_data_fetcher")
suppressMessages({
  library(httr); library(jsonlite); library(dplyr); library(stringr); library(countrycode)
})

source("R/indicator_mappings.R")
source("R/data_functions.R")
source("R/cleaning_functions.R")
source("R/integration_functions.R")

indicators <- unlist(get_dhs_favorites(), use.names = FALSE)
message("Fetching ", length(indicators), " DHS favorite indicators for Mozambique (MZ)")

# --- Fetch one breakdown from the DHS REST API (paginated, chunked) ---
fetch_dhs_rest <- function(inds, country = "MZ", breakdown = "national") {
  base <- "https://api.dhsprogram.com/rest/dhs/data"
  all_rows <- list()
  chunks <- split(inds, ceiling(seq_along(inds) / 8))
  for (ci in seq_along(chunks)) {
    ind_str <- paste(chunks[[ci]], collapse = ",")
    page <- 1
    repeat {
      url <- sprintf("%s?countryIds=%s&indicatorIds=%s&breakdown=%s&f=json&perpage=1000&page=%d",
                     base, country, ind_str, breakdown, page)
      r <- GET(url, timeout(120))
      if (status_code(r) != 200) { message("  HTTP ", status_code(r)); break }
      j <- fromJSON(content(r, as = "text", encoding = "UTF-8"), flatten = TRUE)
      d <- j$Data
      if (is.null(d) || length(d) == 0 || nrow(d) == 0) break
      all_rows[[length(all_rows) + 1]] <- d %>% mutate(across(everything(), as.character))
      total_pages <- if (!is.null(j$TotalPages)) as.integer(j$TotalPages) else 1
      if (page >= total_pages) break
      page <- page + 1
    }
  }
  if (length(all_rows) == 0) return(data.frame())
  out <- bind_rows(all_rows)
  out$Value <- as.numeric(out$Value)
  out$IsPreferred <- as.numeric(out$IsPreferred)
  out$SurveyYear <- as.integer(out$SurveyYear)
  out
}

nat <- fetch_dhs_rest(indicators, breakdown = "national")
sub <- fetch_dhs_rest(indicators, breakdown = "subnational")
message("Fetched: national=", nrow(nat), " subnational=", nrow(sub))
raw <- bind_rows(nat, sub)
raw$Label <- raw$Indicator   # cleaner reads `Label`; DHS REST calls it `Indicator`
saveRDS(raw, "moz_dhs_raw.rds")

# --- Clean + FASTR standardization (applies the new MOÇAMBIQUE name maps) ---
cleaned <- clean_dhs_data(raw, apply_fastr_standardization = TRUE)
message("Cleaned rows: ", nrow(cleaned))

# --- Validate subnational names against the Mozambique backbone ---
backbone <- get_backbone_areas("MOZ")
prov <- cleaned %>% filter(admin_area_2 != "NATIONAL") %>% pull(admin_area_2) %>% unique()
unmatched <- setdiff(prov, backbone)
cat("\n=== Province match vs backbone ===\n")
cat("Provinces in cleaned data:", length(prov), "\n")
cat("Matched to backbone:", length(intersect(prov, backbone)), "\n")
cat("UNMATCHED:", if (length(unmatched)) paste(unmatched, collapse=", ") else "(none)", "\n")

cat("\n=== Summary ===\n")
cat("admin_area_1 values:", paste(unique(cleaned$admin_area_1), collapse=", "), "\n")
cat("country_name values:", paste(unique(cleaned$country_name), collapse=", "), "\n")
cat("iso3:", paste(unique(cleaned$iso3_code), collapse=", "), "\n")
cat("years:", paste(sort(unique(cleaned$year)), collapse=", "), "\n")
cat("source values:", paste(unique(cleaned$source), collapse=", "), "\n")
cat("n indicators:", length(unique(cleaned$indicator_common_id)), "\n")
cat("national rows:", sum(cleaned$admin_area_2=="NATIONAL"),
    " subnational rows:", sum(cleaned$admin_area_2!="NATIONAL"), "\n")

saveRDS(cleaned, "moz_dhs_cleaned.rds")
message("\nWrote moz_dhs_cleaned.rds (", nrow(cleaned), " rows). No DB changes made.")
