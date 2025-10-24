# ========================================
# TEST SCRIPT: DHIS2 Demo Instance
# ========================================
# This script tests the DHIS2 reference builder with the public demo instance
# No credentials needed - uses public demo server

library(httr)
library(jsonlite)
library(dplyr)
library(purrr)

# Load the DHIS2 reference builder functions
source("R/dhis2_reference_builder.R")

message("========================================")
message("TESTING DHIS2 REFERENCE BUILDER")
message("========================================")
message("Using DHIS2 demo instance (no credentials needed)")
message("")

# Test with DHIS2 demo/training instance
# This is publicly accessible with username: admin, password: district
test_result <- tryCatch({

  build_all_dhis2_references(
    base_url = "https://play.dhis2.org/dev",
    username = "admin",
    password = "district",
    levels = 1:3,
    output_dir = "assets",
    instance_name = "demo"
  )

  message("\n✓ SUCCESS! Test completed")
  message("\nGenerated files:")
  message("  - assets/dhis2_demo_countries.csv")
  message("  - assets/dhis2_demo_provinces.csv")
  message("  - assets/dhis2_demo_districts.csv")
  message("\nYou can now inspect these files to see the DHIS2 data structure.")

  TRUE

}, error = function(e) {
  message("\n✗ ERROR: Test failed")
  message("Error message: ", e$message)
  message("\nPossible causes:")
  message("  - Network connectivity issues")
  message("  - DHIS2 demo server is down")
  message("  - Required packages not installed")
  FALSE
})

# Quick preview of results if successful
if (test_result && file.exists("assets/dhis2_demo_countries.csv")) {
  message("\n========================================")
  message("PREVIEW OF RESULTS")
  message("========================================")

  countries <- read.csv("assets/dhis2_demo_countries.csv", stringsAsFactors = FALSE)
  message("\nCountries found: ", nrow(countries))
  if (nrow(countries) > 0) {
    message("Sample countries:")
    print(head(countries, 5))
  }

  if (file.exists("assets/dhis2_demo_provinces.csv")) {
    provinces <- read.csv("assets/dhis2_demo_provinces.csv", stringsAsFactors = FALSE)
    message("\nProvinces/regions found: ", nrow(provinces))
    if (nrow(provinces) > 0) {
      message("Sample provinces:")
      print(head(provinces, 5))
    }
  }

  if (file.exists("assets/dhis2_demo_districts.csv")) {
    districts <- read.csv("assets/dhis2_demo_districts.csv", stringsAsFactors = FALSE)
    message("\nDistricts found: ", nrow(districts))
  }
}

message("\n========================================")
message("Next steps:")
message("========================================")
message("1. Review the generated CSV files in assets/")
message("2. Set up your own DHIS2 credentials in .Renviron")
message("3. Run build_all_dhis2_references() with your instance")
message("4. See config/DHIS2_SETUP.md for detailed instructions")
