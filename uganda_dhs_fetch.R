# ============================================================
# Uganda DHS survey fetch (headless replica of the Shiny app)
# Last edit: 2026 Jun 04
# Fetches DHS favorites for Uganda (national + subnational) via the DHS REST
# API, cleans with clean_dhs_data() (which now drops non-backbone sub-regions),
# validates against assets/uganda_backbone.csv, prints a year x region matrix.
# Writes uganda_dhs_cleaned.rds. No DB changes.
# ============================================================
setwd("/Users/claireboulange/Desktop/modules/06_survey_data_fetcher")
suppressMessages({ library(httr); library(jsonlite); library(dplyr); library(stringr); library(countrycode) })
source("R/indicator_mappings.R"); source("R/data_functions.R")
source("R/cleaning_functions.R"); source("R/integration_functions.R")

indicators <- unlist(get_dhs_favorites(), use.names = FALSE)

fetch_dhs_rest <- function(inds, country = "UG", breakdown = "national") {
  base <- "https://api.dhsprogram.com/rest/dhs/data"; all_rows <- list()
  chunks <- split(inds, ceiling(seq_along(inds) / 8))
  for (ci in seq_along(chunks)) {
    ind_str <- paste(chunks[[ci]], collapse = ","); page <- 1
    repeat {
      url <- sprintf("%s?countryIds=%s&indicatorIds=%s&breakdown=%s&f=json&perpage=1000&page=%d",
                     base, country, ind_str, breakdown, page)
      r <- GET(url, timeout(120)); if (status_code(r) != 200) break
      j <- fromJSON(content(r, as = "text", encoding = "UTF-8"), flatten = TRUE)
      d <- j$Data; if (is.null(d) || length(d) == 0 || nrow(d) == 0) break
      all_rows[[length(all_rows)+1]] <- d %>% mutate(across(everything(), as.character))
      tp <- if (!is.null(j$TotalPages)) as.integer(j$TotalPages) else 1
      if (page >= tp) break; page <- page + 1
    }
  }
  if (length(all_rows) == 0) return(data.frame())
  out <- bind_rows(all_rows)
  out$Value <- as.numeric(out$Value); out$IsPreferred <- as.numeric(out$IsPreferred)
  out$SurveyYear <- as.integer(out$SurveyYear); out
}

nat <- fetch_dhs_rest(indicators, breakdown = "national")
sub <- fetch_dhs_rest(indicators, breakdown = "subnational")
message("Fetched national=", nrow(nat), " subnational=", nrow(sub))
raw <- bind_rows(nat, sub); raw$Label <- raw$Indicator
saveRDS(raw, "uganda_dhs_raw.rds")

cleaned <- clean_dhs_data(raw, apply_fastr_standardization = TRUE)
message("Cleaned rows (pre survey-dedup): ", nrow(cleaned))

# Survey-priority dedup: 2011 has BOTH UG2011DHS and UG2011AIS. The DB keeps one
# value per (area, year, indicator); prefer DHS > AIS > MIS. Keeps DHS-2011 where
# it overlaps AIS-2011, and AIS-2011 rows DHS doesn't cover.
cleaned <- cleaned %>%
  mutate(.pri = case_when(grepl("DHS$", source_detail) ~ 1L,
                          grepl("AIS$", source_detail) ~ 2L,
                          grepl("MIS$", source_detail) ~ 3L, TRUE ~ 4L)) %>%
  arrange(.pri) %>%
  distinct(admin_area_1, admin_area_2, year, indicator_common_id, .keep_all = TRUE) %>%
  select(-.pri)
message("Cleaned rows (post survey-dedup): ", nrow(cleaned))
# hard check: zero true duplicates on the natural key
dups <- cleaned %>% count(admin_area_1, admin_area_2, year, indicator_common_id) %>% filter(n > 1)
message("Remaining duplicate keys (must be 0): ", nrow(dups))

backbone <- get_backbone_areas("UGA")
prov <- cleaned %>% filter(admin_area_2 != "NATIONAL") %>% pull(admin_area_2) %>% unique()
cat("\n=== Province match vs backbone (15 sub-regions) ===\n")
cat("kept provinces:", length(prov), "| matched:", length(intersect(prov, backbone)),
    "| UNMATCHED:", paste(setdiff(prov, backbone), collapse=", "), "\n")
cat("backbone regions NOT covered by any survey:",
    paste(setdiff(backbone, prov), collapse=", "), "\n")

cat("\n=== admin_area_1 / iso ===\n")
print(cleaned %>% distinct(admin_area_1, country_name, iso2_code, iso3_code))

cat("\n=== YEAR x REGION matrix (n indicators per cell) ===\n")
mat <- cleaned %>% filter(admin_area_2 != "NATIONAL") %>%
  count(year, admin_area_2) %>% tidyr::pivot_wider(names_from = year, values_from = n, values_fill = 0)
print(as.data.frame(mat), row.names = FALSE)

cat("\n=== National rows per survey year ===\n")
print(cleaned %>% filter(admin_area_2 == "NATIONAL") %>% count(year))

saveRDS(cleaned, "uganda_dhs_cleaned.rds")
message("\nWrote uganda_dhs_cleaned.rds (", nrow(cleaned), " rows). No DB changes.")
