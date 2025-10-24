# Quick diagnostic script to check DHIS2 API
library(httr)
library(jsonlite)

# DHIS2 demo instance
base_url <- "https://play.dhis2.org/dev"
username <- "admin"
password <- "district"

message("Checking DHIS2 API: ", base_url)

# Test 1: Basic API connection
message("\n1. Testing API connection...")
response <- GET(paste0(base_url, "/api/me"),
               authenticate(username, password),
               accept_json())
message("Status: ", status_code(response))
if (status_code(response) == 200) {
  user_info <- content(response, "parsed")
  message("✓ Connected as: ", user_info$name)
}

# Test 2: Check org unit levels
message("\n2. Checking organization unit levels...")
response <- GET(paste0(base_url, "/api/organisationUnitLevels"),
               authenticate(username, password),
               accept_json())
if (status_code(response) == 200) {
  levels_data <- content(response, "parsed")
  if (length(levels_data$organisationUnitLevels) > 0) {
    message("Found ", length(levels_data$organisationUnitLevels), " levels:")
    for (level in levels_data$organisationUnitLevels) {
      message("  Level ", level$level, ": ", level$name)
    }
  }
}

# Test 3: Get a sample of org units (no level filter)
message("\n3. Fetching sample organization units...")
response <- GET(paste0(base_url, "/api/organisationUnits"),
               authenticate(username, password),
               query = list(
                 fields = "id,name,level",
                 pageSize = 10
               ),
               accept_json())
message("Status: ", status_code(response))
if (status_code(response) == 200) {
  org_data <- content(response, "parsed")
  if (!is.null(org_data$organisationUnits)) {
    message("Found ", length(org_data$organisationUnits), " org units (sample):")
    for (ou in head(org_data$organisationUnits, 5)) {
      message("  - ", ou$name, " (level ", ou$level %||% "N/A", ")")
    }
    message("\nPager info:")
    message("  Total: ", org_data$pager$total %||% "N/A")
    message("  Page: ", org_data$pager$page %||% "N/A")
  }
}

# Test 4: Try specific level
message("\n4. Trying to fetch level 1 org units...")
response <- GET(paste0(base_url, "/api/organisationUnits"),
               authenticate(username, password),
               query = list(
                 filter = "level:eq:1",
                 fields = "id,name,level",
                 paging = "false"
               ),
               accept_json())
message("Status: ", status_code(response))
if (status_code(response) == 200) {
  org_data <- content(response, "parsed")
  message("Org units at level 1: ", length(org_data$organisationUnits %||% list()))
}
