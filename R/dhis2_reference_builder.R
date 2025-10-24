# ========================================
# DHIS2 REFERENCE DATASET BUILDER
# ========================================
# Purpose: Fetch organization units from DHIS2 API and build reference datasets
#          for standardizing admin area names across survey data sources
#
# Usage:
#   1. Set DHIS2 credentials in .Renviron or environment
#   2. Run build_all_dhis2_references() to fetch and save reference files
#   3. Reference CSVs will be saved in assets/ directory
#   4. Re-run periodically (monthly/quarterly) to keep data fresh

library(httr)
library(jsonlite)
library(dplyr)
library(purrr)

# ========================================
# AUTHENTICATION
# ========================================

#' Create DHIS2 API authentication headers
#' @param username DHIS2 username
#' @param password DHIS2 password
#' @param use_pat Use Personal Access Token instead of Basic Auth
#' @param pat_token Personal Access Token (if use_pat = TRUE)
#' @return httr configuration with authentication headers
dhis2_auth <- function(username, password, use_pat = FALSE, pat_token = NULL) {
  if (use_pat && !is.null(pat_token)) {
    # Personal Access Token authentication (recommended)
    add_headers(Authorization = paste("ApiToken", pat_token))
  } else {
    # Basic authentication
    authenticate(username, password)
  }
}

# ========================================
# CORE API FUNCTIONS
# ========================================

#' Fetch organization units from DHIS2 API
#' @param base_url DHIS2 instance base URL (e.g., "https://play.dhis2.org/dev")
#' @param level Organization unit level (1=country, 2=province, 3=district, 4=facility)
#' @param auth_config Authentication configuration from dhis2_auth()
#' @param fields Comma-separated fields to retrieve
#' @return Data frame of organization units
fetch_dhis2_org_units <- function(base_url,
                                   level = NULL,
                                   auth_config,
                                   fields = "id,name,displayName,code,shortName,level,parent[id,name,level]") {

  message("Fetching org units from DHIS2: ", base_url)
  if (!is.null(level)) {
    message("  Level: ", level)
  }

  # Build API endpoint
  endpoint <- paste0(base_url, "/api/organisationUnits")

  # Build query parameters
  query_params <- list(
    fields = fields,
    paging = "false"  # Get all results
  )

  # Add level filter if specified
  if (!is.null(level)) {
    query_params$filter <- paste0("level:eq:", level)
  }

  # Make API request
  tryCatch({
    response <- GET(
      url = endpoint,
      auth_config,
      query = query_params,
      accept_json()
    )

    # Check response status
    if (status_code(response) != 200) {
      stop("DHIS2 API request failed with status ", status_code(response),
           "\nResponse: ", content(response, "text"))
    }

    # Parse JSON response
    content_parsed <- content(response, "parsed")

    if (is.null(content_parsed$organisationUnits) || length(content_parsed$organisationUnits) == 0) {
      warning("No organization units returned from DHIS2 API")
      return(data.frame())
    }

    # Convert to data frame
    org_units <- content_parsed$organisationUnits %>%
      map_dfr(function(ou) {
        tibble(
          id = ou$id %||% NA_character_,
          name = ou$name %||% NA_character_,
          display_name = ou$displayName %||% NA_character_,
          short_name = ou$shortName %||% NA_character_,
          code = ou$code %||% NA_character_,
          level = ou$level %||% NA_integer_,
          parent_id = ou$parent$id %||% NA_character_,
          parent_name = ou$parent$name %||% NA_character_,
          parent_level = ou$parent$level %||% NA_integer_
        )
      })

    message("  Retrieved ", nrow(org_units), " organization units")
    return(org_units)

  }, error = function(e) {
    stop("Error fetching DHIS2 org units: ", e$message)
  })
}

#' Fetch organization unit levels metadata
#' @param base_url DHIS2 instance base URL
#' @param auth_config Authentication configuration
#' @return Data frame of organization unit levels
fetch_dhis2_org_unit_levels <- function(base_url, auth_config) {

  message("Fetching org unit levels from DHIS2: ", base_url)

  endpoint <- paste0(base_url, "/api/organisationUnitLevels")

  tryCatch({
    response <- GET(
      url = endpoint,
      auth_config,
      query = list(fields = "id,name,level", paging = "false"),
      accept_json()
    )

    if (status_code(response) != 200) {
      stop("Failed to fetch org unit levels: ", status_code(response))
    }

    content_parsed <- content(response, "parsed")

    levels <- content_parsed$organisationUnitLevels %>%
      map_dfr(~ tibble(
        id = .x$id %||% NA_character_,
        name = .x$name %||% NA_character_,
        level = .x$level %||% NA_integer_
      ))

    message("  Retrieved ", nrow(levels), " org unit levels")
    return(levels)

  }, error = function(e) {
    warning("Could not fetch org unit levels: ", e$message)
    return(data.frame())
  })
}

# ========================================
# REFERENCE DATASET BUILDERS
# ========================================

#' Build country reference dataset (level 1)
#' @param base_url DHIS2 instance base URL
#' @param auth_config Authentication configuration
#' @return Data frame with country names
build_country_reference <- function(base_url, auth_config) {

  message("\n=== Building Country Reference (Level 1) ===")

  org_units <- fetch_dhis2_org_units(
    base_url = base_url,
    level = 1,
    auth_config = auth_config,
    fields = "id,name,displayName,shortName,code"
  )

  if (nrow(org_units) == 0) {
    warning("No level 1 org units found")
    return(data.frame())
  }

  # Create standardized country reference
  country_ref <- org_units %>%
    mutate(
      dhis2_id = id,
      dhis2_name = name,
      dhis2_display_name = display_name,
      country_code = code,
      level = 1
    ) %>%
    select(dhis2_id, dhis2_name, dhis2_display_name, country_code, level) %>%
    arrange(dhis2_name)

  message("  Built reference for ", nrow(country_ref), " countries")
  return(country_ref)
}

#' Build province/region reference dataset (level 2)
#' @param base_url DHIS2 instance base URL
#' @param auth_config Authentication configuration
#' @return Data frame with province names and parent countries
build_province_reference <- function(base_url, auth_config) {

  message("\n=== Building Province/Region Reference (Level 2) ===")

  org_units <- fetch_dhis2_org_units(
    base_url = base_url,
    level = 2,
    auth_config = auth_config
  )

  if (nrow(org_units) == 0) {
    warning("No level 2 org units found")
    return(data.frame())
  }

  # Create standardized province reference
  province_ref <- org_units %>%
    mutate(
      dhis2_id = id,
      dhis2_name = name,
      dhis2_display_name = display_name,
      country_id = parent_id,
      country_name = parent_name,
      province_code = code,
      level = 2
    ) %>%
    select(dhis2_id, dhis2_name, dhis2_display_name, country_id,
           country_name, province_code, level) %>%
    arrange(country_name, dhis2_name)

  message("  Built reference for ", nrow(province_ref), " provinces/regions")
  return(province_ref)
}

#' Build district reference dataset (level 3)
#' @param base_url DHIS2 instance base URL
#' @param auth_config Authentication configuration
#' @return Data frame with district names and parent hierarchies
build_district_reference <- function(base_url, auth_config) {

  message("\n=== Building District Reference (Level 3) ===")

  org_units <- fetch_dhis2_org_units(
    base_url = base_url,
    level = 3,
    auth_config = auth_config
  )

  if (nrow(org_units) == 0) {
    warning("No level 3 org units found")
    return(data.frame())
  }

  # Get level 2 data to complete hierarchy
  level2 <- fetch_dhis2_org_units(base_url, 2, auth_config,
                                   fields = "id,name,parent[id,name]")

  # Join to get full hierarchy (district -> province -> country)
  district_ref <- org_units %>%
    left_join(
      level2 %>% select(province_id = id, province_name = name,
                       country_id = parent_id, country_name = parent_name),
      by = c("parent_id" = "province_id")
    ) %>%
    mutate(
      dhis2_id = id,
      dhis2_name = name,
      dhis2_display_name = display_name,
      district_code = code,
      level = 3
    ) %>%
    select(dhis2_id, dhis2_name, dhis2_display_name, district_code,
           province_id = parent_id, province_name = parent_name,
           country_id, country_name, level) %>%
    arrange(country_name, province_name, dhis2_name)

  message("  Built reference for ", nrow(district_ref), " districts")
  return(district_ref)
}

#' Build facility reference dataset (level 4)
#' @param base_url DHIS2 instance base URL
#' @param auth_config Authentication configuration
#' @return Data frame with facility names and locations
build_facility_reference <- function(base_url, auth_config) {

  message("\n=== Building Facility Reference (Level 4) ===")

  org_units <- fetch_dhis2_org_units(
    base_url = base_url,
    level = 4,
    auth_config = auth_config,
    fields = "id,name,displayName,code,shortName,level,coordinates,parent[id,name,parent[id,name,parent[id,name]]]"
  )

  if (nrow(org_units) == 0) {
    warning("No level 4 org units found")
    return(data.frame())
  }

  # Extract hierarchy from nested parent structure
  facility_ref <- org_units %>%
    mutate(
      dhis2_id = id,
      dhis2_name = name,
      dhis2_display_name = display_name,
      facility_code = code,
      district_id = parent_id,
      district_name = parent_name,
      level = 4
    ) %>%
    select(dhis2_id, dhis2_name, dhis2_display_name, facility_code,
           district_id, district_name, level) %>%
    arrange(district_name, dhis2_name)

  message("  Built reference for ", nrow(facility_ref), " facilities")
  return(facility_ref)
}

# ========================================
# EXPORT FUNCTIONS
# ========================================

#' Export reference dataset to CSV
#' @param data Data frame to export
#' @param filename Output filename (without path)
#' @param output_dir Output directory (default: "assets")
export_reference_csv <- function(data, filename, output_dir = "assets") {

  # Create output directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  filepath <- file.path(output_dir, filename)

  write.csv(data, filepath, row.names = FALSE, fileEncoding = "UTF-8")
  message("  Exported: ", filepath, " (", nrow(data), " rows)")
}

# ========================================
# MAIN ORCHESTRATION FUNCTIONS
# ========================================

#' Build all DHIS2 reference datasets from a single instance
#' @param base_url DHIS2 instance base URL
#' @param username DHIS2 username (or set DHIS2_USERNAME env var)
#' @param password DHIS2 password (or set DHIS2_PASSWORD env var)
#' @param use_pat Use Personal Access Token instead of Basic Auth
#' @param pat_token Personal Access Token (or set DHIS2_PAT env var)
#' @param levels Which levels to fetch (default: 1:3)
#' @param output_dir Output directory for CSV files
#' @param instance_name Optional name to prefix output files
#' @return List of reference data frames
build_all_dhis2_references <- function(base_url,
                                        username = Sys.getenv("DHIS2_USERNAME"),
                                        password = Sys.getenv("DHIS2_PASSWORD"),
                                        use_pat = FALSE,
                                        pat_token = Sys.getenv("DHIS2_PAT"),
                                        levels = 1:3,
                                        output_dir = "assets",
                                        instance_name = NULL) {

  message("\n========================================")
  message("DHIS2 REFERENCE DATASET BUILDER")
  message("========================================")
  message("Instance: ", base_url)
  message("Levels to fetch: ", paste(levels, collapse = ", "))

  # Validate credentials
  if (!use_pat && (username == "" || password == "")) {
    stop("Please provide DHIS2 credentials via parameters or environment variables:\n",
         "  DHIS2_USERNAME and DHIS2_PASSWORD\n",
         "  Or use PAT: DHIS2_PAT")
  }

  if (use_pat && pat_token == "") {
    stop("Please provide DHIS2_PAT environment variable for token authentication")
  }

  # Create authentication
  auth_config <- dhis2_auth(username, password, use_pat, pat_token)

  # Optional: Fetch and display org unit level definitions
  tryCatch({
    levels_info <- fetch_dhis2_org_unit_levels(base_url, auth_config)
    if (nrow(levels_info) > 0) {
      message("\nOrganization Unit Levels:")
      for (i in 1:nrow(levels_info)) {
        message("  Level ", levels_info$level[i], ": ", levels_info$name[i])
      }
    }
  }, error = function(e) {
    message("Note: Could not fetch level definitions, continuing...")
  })

  # Build reference datasets
  references <- list()

  if (1 %in% levels) {
    references$countries <- build_country_reference(base_url, auth_config)
    if (nrow(references$countries) > 0) {
      filename <- if (!is.null(instance_name)) {
        paste0("dhis2_", instance_name, "_countries.csv")
      } else {
        "dhis2_countries.csv"
      }
      export_reference_csv(references$countries, filename, output_dir)
    }
  }

  if (2 %in% levels) {
    references$provinces <- build_province_reference(base_url, auth_config)
    if (nrow(references$provinces) > 0) {
      filename <- if (!is.null(instance_name)) {
        paste0("dhis2_", instance_name, "_provinces.csv")
      } else {
        "dhis2_provinces.csv"
      }
      export_reference_csv(references$provinces, filename, output_dir)
    }
  }

  if (3 %in% levels) {
    references$districts <- build_district_reference(base_url, auth_config)
    if (nrow(references$districts) > 0) {
      filename <- if (!is.null(instance_name)) {
        paste0("dhis2_", instance_name, "_districts.csv")
      } else {
        "dhis2_districts.csv"
      }
      export_reference_csv(references$districts, filename, output_dir)
    }
  }

  if (4 %in% levels) {
    references$facilities <- build_facility_reference(base_url, auth_config)
    if (nrow(references$facilities) > 0) {
      filename <- if (!is.null(instance_name)) {
        paste0("dhis2_", instance_name, "_facilities.csv")
      } else {
        "dhis2_facilities.csv"
      }
      export_reference_csv(references$facilities, filename, output_dir)
    }
  }

  message("\n========================================")
  message("REFERENCE DATASET BUILD COMPLETE")
  message("========================================")
  message("Total datasets built: ", length(references))
  message("Output directory: ", output_dir)

  return(invisible(references))
}

#' Build references from multiple DHIS2 instances
#' @param instances List of instance configurations (see config/dhis2_instances_template.yaml)
#' @param levels Which levels to fetch
#' @param output_dir Output directory
#' @return Combined reference datasets
build_multi_instance_references <- function(instances, levels = 1:3, output_dir = "assets") {

  message("\n========================================")
  message("MULTI-INSTANCE DHIS2 REFERENCE BUILDER")
  message("========================================")
  message("Processing ", length(instances), " DHIS2 instances")

  all_references <- list()

  for (inst_name in names(instances)) {
    inst_config <- instances[[inst_name]]

    message("\n--- Processing instance: ", inst_name, " ---")

    refs <- build_all_dhis2_references(
      base_url = inst_config$url,
      username = inst_config$username %||% Sys.getenv(paste0("DHIS2_", toupper(inst_name), "_USERNAME")),
      password = inst_config$password %||% Sys.getenv(paste0("DHIS2_", toupper(inst_name), "_PASSWORD")),
      use_pat = inst_config$use_pat %||% FALSE,
      pat_token = inst_config$pat_token %||% Sys.getenv(paste0("DHIS2_", toupper(inst_name), "_PAT")),
      levels = levels,
      output_dir = output_dir,
      instance_name = inst_name
    )

    all_references[[inst_name]] <- refs
  }

  # Combine all references
  combined <- list()

  if (1 %in% levels) {
    combined$countries <- bind_rows(lapply(all_references, function(x) x$countries))
    if (nrow(combined$countries) > 0) {
      export_reference_csv(combined$countries, "dhis2_combined_countries.csv", output_dir)
    }
  }

  if (2 %in% levels) {
    combined$provinces <- bind_rows(lapply(all_references, function(x) x$provinces))
    if (nrow(combined$provinces) > 0) {
      export_reference_csv(combined$provinces, "dhis2_combined_provinces.csv", output_dir)
    }
  }

  if (3 %in% levels) {
    combined$districts <- bind_rows(lapply(all_references, function(x) x$districts))
    if (nrow(combined$districts) > 0) {
      export_reference_csv(combined$districts, "dhis2_combined_districts.csv", output_dir)
    }
  }

  message("\n========================================")
  message("MULTI-INSTANCE BUILD COMPLETE")
  message("========================================")

  return(invisible(list(
    individual = all_references,
    combined = combined
  )))
}

# ========================================
# EXAMPLE USAGE
# ========================================

# Example 1: Single instance (DHIS2 demo server)
# build_all_dhis2_references(
#   base_url = "https://play.dhis2.org/dev",
#   username = "admin",
#   password = "district",
#   levels = 1:3
# )

# Example 2: Using Personal Access Token
# build_all_dhis2_references(
#   base_url = "https://dhis2.example.org",
#   use_pat = TRUE,
#   pat_token = "d2pat_yourTokenHere",
#   levels = 1:4
# )

# Example 3: Multiple instances from config
# instances <- list(
#   senegal = list(
#     url = "https://dhis2.sante.gouv.sn",
#     username = Sys.getenv("DHIS2_SENEGAL_USERNAME"),
#     password = Sys.getenv("DHIS2_SENEGAL_PASSWORD")
#   ),
#   nigeria = list(
#     url = "https://dhis2.ng",
#     use_pat = TRUE,
#     pat_token = Sys.getenv("DHIS2_NIGERIA_PAT")
#   )
# )
# build_multi_instance_references(instances, levels = 1:3)
