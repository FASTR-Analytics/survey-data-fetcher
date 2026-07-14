# ========================================
# DATABASE INTEGRATION FUNCTIONS
# ========================================
# File: R/integration_functions.R
# Purpose: Functions for validating and integrating survey data into the unified database
# Supports GitHub-based collaborative workflow

library(stringdist)
library(httr)
library(base64enc)

# ========================================
# CONFIGURATION
# ========================================

# GitHub repository settings
GITHUB_OWNER <- "FASTR-Analytics"
GITHUB_REPO <- "modules"
GITHUB_BRANCH <- "main"

# Raw GitHub URLs for database files
SURVEY_DB_URL <- "https://raw.githubusercontent.com/FASTR-Analytics/modules/refs/heads/main/survey_data_unified.csv"
POP_DB_URL <- "https://raw.githubusercontent.com/FASTR-Analytics/modules/refs/heads/main/population_estimates_only.csv"

# Local paths (for fallback/temp storage)
SURVEY_DB_PATH <- "/Users/claireboulange/Desktop/modules/survey_data_unified.csv"
POP_DB_PATH <- "/Users/claireboulange/Desktop/modules/population_estimates_only.csv"

# Population estimate indicators (for splitting databases)
POP_INDICATORS <- c("poptot", "popu5", "totu1pop", "totu5pop", "livebirth",
                    "womenrepage", "popgrowth", "crudebr")

# ========================================
# GITHUB FUNCTIONS
# ========================================

#' Pull database from GitHub
#' @param url Raw GitHub URL for the CSV file
#' @return data.frame or NULL if fetch fails
pull_database_from_github <- function(url) {
  tryCatch({
    message("Pulling database from: ", url)
    response <- httr::GET(url)

    if (httr::status_code(response) == 200) {
      content <- httr::content(response, as = "text", encoding = "UTF-8")
      data <- readr::read_csv(content, show_col_types = FALSE)
      message("Successfully pulled ", nrow(data), " records from GitHub")
      return(data)
    } else {
      warning("Failed to fetch from GitHub: HTTP ", httr::status_code(response))
      return(NULL)
    }
  }, error = function(e) {
    warning("Error pulling from GitHub: ", e$message)
    return(NULL)
  })
}

#' Create a feature branch from main on GitHub
#' @param branch_name Name for the new branch
#' @param github_token GitHub personal access token
#' @return list with success status and branch SHA
create_github_branch <- function(branch_name, github_token) {
  # Get main branch SHA
  ref_url <- paste0("https://api.github.com/repos/", GITHUB_OWNER, "/", GITHUB_REPO,
                    "/git/ref/heads/", GITHUB_BRANCH)
  ref_response <- httr::GET(ref_url, httr::add_headers(
    Authorization = paste("Bearer", github_token),
    Accept = "application/vnd.github.v3+json"
  ))

  if (httr::status_code(ref_response) != 200) {
    return(list(success = FALSE, message = "Could not get main branch SHA"))
  }

  main_sha <- httr::content(ref_response)$object$sha

  # Create new branch
  create_url <- paste0("https://api.github.com/repos/", GITHUB_OWNER, "/", GITHUB_REPO, "/git/refs")
  create_response <- httr::POST(create_url,
    httr::add_headers(
      Authorization = paste("Bearer", github_token),
      Accept = "application/vnd.github.v3+json"
    ),
    body = list(ref = paste0("refs/heads/", branch_name), sha = main_sha),
    encode = "json"
  )

  if (httr::status_code(create_response) %in% c(200, 201)) {
    return(list(success = TRUE, sha = main_sha))
  } else {
    error_content <- httr::content(create_response)
    return(list(success = FALSE, message = paste("Branch creation failed:", error_content$message)))
  }
}

#' Push a file to a specific branch on GitHub
#' @param data data.frame to push
#' @param file_path Path in the repo (e.g., "survey_data_unified.csv")
#' @param commit_message Commit message
#' @param github_token GitHub personal access token
#' @param branch Branch to push to
#' @return list with success status and message
push_database_to_github <- function(data, file_path, commit_message, github_token, branch = GITHUB_BRANCH) {
  if (is.null(github_token) || github_token == "") {
    return(list(success = FALSE, message = "GitHub token not provided"))
  }

  tryCatch({
    # Convert data to CSV string
    csv_content <- readr::format_csv(data)
    csv_base64 <- base64enc::base64encode(charToRaw(csv_content))

    # Get current file SHA from the target branch
    api_url <- paste0("https://api.github.com/repos/", GITHUB_OWNER, "/", GITHUB_REPO,
                      "/contents/", file_path, "?ref=", branch)

    get_response <- httr::GET(
      api_url,
      httr::add_headers(
        Authorization = paste("Bearer", github_token),
        Accept = "application/vnd.github.v3+json"
      )
    )

    sha <- NULL
    if (httr::status_code(get_response) == 200) {
      file_info <- httr::content(get_response)
      sha <- file_info$sha
    }

    # Prepare request body
    body <- list(
      message = commit_message,
      content = csv_base64,
      branch = branch
    )

    if (!is.null(sha)) {
      body$sha <- sha
    }

    # Push to GitHub
    put_url <- paste0("https://api.github.com/repos/", GITHUB_OWNER, "/", GITHUB_REPO,
                      "/contents/", file_path)
    put_response <- httr::PUT(
      put_url,
      httr::add_headers(
        Authorization = paste("Bearer", github_token),
        Accept = "application/vnd.github.v3+json"
      ),
      body = body,
      encode = "json"
    )

    if (httr::status_code(put_response) %in% c(200, 201)) {
      return(list(success = TRUE, message = paste("Successfully pushed", file_path, "to branch", branch)))
    } else {
      error_content <- httr::content(put_response)
      return(list(success = FALSE, message = paste("GitHub push failed:", error_content$message)))
    }

  }, error = function(e) {
    return(list(success = FALSE, message = paste("Error pushing to GitHub:", e$message)))
  })
}

#' Create a pull request on GitHub
#' @param branch_name Source branch
#' @param title PR title
#' @param body PR body/description
#' @param github_token GitHub personal access token
#' @return list with success, message, and pr_url
create_github_pr <- function(branch_name, title, body, github_token) {
  pr_url <- paste0("https://api.github.com/repos/", GITHUB_OWNER, "/", GITHUB_REPO, "/pulls")
  pr_response <- httr::POST(pr_url,
    httr::add_headers(
      Authorization = paste("Bearer", github_token),
      Accept = "application/vnd.github.v3+json"
    ),
    body = list(
      title = title,
      body = body,
      head = branch_name,
      base = GITHUB_BRANCH
    ),
    encode = "json"
  )

  if (httr::status_code(pr_response) %in% c(200, 201)) {
    pr_info <- httr::content(pr_response)
    return(list(success = TRUE, message = "PR created", pr_url = pr_info$html_url, pr_number = pr_info$number))
  } else {
    error_content <- httr::content(pr_response)
    return(list(success = FALSE, message = paste("PR creation failed:", error_content$message)))
  }
}

# ========================================
# DATABASE LOADING FUNCTIONS
# ========================================

#' Standardize country names in database to match FASTR conventions
#' @param data Data frame with country_name and/or admin_area_1 columns
#' @return Data frame with standardized country names
standardize_db_country_names <- function(data) {
  if (is.null(data) || nrow(data) == 0) return(data)

  # Mapping from database variations to standard FASTR names
  db_country_fixes <- c(
    "Congo Democratic Republic" = "République Démocratique du Congo",
    "Democratic Republic of the Congo" = "République Démocratique du Congo",
    "Cote d'Ivoire" = "Côte d'Ivoire",
    "Cote d Ivoire" = "Côte d'Ivoire"
  )

  if ("country_name" %in% names(data)) {
    data$country_name <- dplyr::recode(data$country_name, !!!db_country_fixes)
  }
  if ("admin_area_1" %in% names(data)) {
    data$admin_area_1 <- dplyr::recode(data$admin_area_1, !!!db_country_fixes)
  }

  return(data)
}

#' Load the survey database (from GitHub first, fallback to local)
#' @param use_github Whether to pull from GitHub (default TRUE)
#' @return data.frame of survey data or NULL if not available
load_survey_database <- function(use_github = TRUE) {
  data <- NULL

  if (use_github) {
    data <- pull_database_from_github(SURVEY_DB_URL)
  }

  # Fallback to local file
  if (is.null(data) && file.exists(SURVEY_DB_PATH)) {
    data <- readr::read_csv(SURVEY_DB_PATH, show_col_types = FALSE)
  }

  # Standardize country names before returning
  return(standardize_db_country_names(data))
}

#' Load the population database (from GitHub first, fallback to local)
#' @param use_github Whether to pull from GitHub (default TRUE)
#' @return data.frame of population data or NULL if not available
load_population_database <- function(use_github = TRUE) {
  data <- NULL

  if (use_github) {
    data <- pull_database_from_github(POP_DB_URL)
  }

  # Fallback to local file
  if (is.null(data) && file.exists(POP_DB_PATH)) {
    data <- readr::read_csv(POP_DB_PATH, show_col_types = FALSE)
  }

  # Standardize country names before returning
  return(standardize_db_country_names(data))
}

# ========================================
# NAME VALIDATION FUNCTIONS
# ========================================

#' Fuzzy match admin area names against existing database
#' @param new_names Vector of new admin area names to check
#' @param existing_names Vector of existing admin area names in database
#' @param threshold Similarity threshold for fuzzy matching (default 0.85)
#' @return data.frame with matching results
fuzzy_match_names <- function(new_names, existing_names, threshold = 0.85) {
  results <- data.frame(
    new_name = character(),
    best_match = character(),
    similarity = numeric(),
    match_type = character(),
    stringsAsFactors = FALSE
  )

  for (new_name in unique(new_names)) {
    if (new_name == "NATIONAL") {
      results <- rbind(results, data.frame(
        new_name = new_name,
        best_match = "NATIONAL",
        similarity = 1.0,
        match_type = "exact"
      ))
      next
    }

    # Check for exact match
    if (new_name %in% existing_names) {
      results <- rbind(results, data.frame(
        new_name = new_name,
        best_match = new_name,
        similarity = 1.0,
        match_type = "exact"
      ))
    } else {
      # Find best fuzzy match
      distances <- stringdist::stringsim(new_name, existing_names, method = "jw")
      best_idx <- which.max(distances)
      best_sim <- distances[best_idx]

      if (best_sim >= threshold) {
        results <- rbind(results, data.frame(
          new_name = new_name,
          best_match = existing_names[best_idx],
          similarity = best_sim,
          match_type = "fuzzy"
        ))
      } else {
        results <- rbind(results, data.frame(
          new_name = new_name,
          best_match = NA,
          similarity = best_sim,
          match_type = "new_area"
        ))
      }
    }
  }

  return(results)
}

#' Load DHIS2 backbone admin_area_2 names for a given ISO3 code
#' @param iso3 ISO3 country code
#' @return character vector of valid admin_area_2 names, or NULL if no backbone
get_backbone_areas <- function(iso3) {
  # Map ISO3 codes to backbone filenames
  iso3_to_file <- c(
    "AFG" = "afghanistan", "BGD" = "bangladesh1", "CMR" = "cameroon",
    "TCD" = "chad", "COD" = "drc", "ETH" = "ethiopia", "GHA" = "ghana",
    "GIN" = "guinea", "HTI" = "haiti", "KEN" = "kenya", "LBR" = "liberia",
    "MDG" = "madagascar", "MWI" = "malawi", "MLI" = "mali",
    "MRT" = "mauritania", "NGA" = "nigeria", "CAF" = "rca",
    "SEN" = "senegal", "SLE" = "sierraleone", "SOM" = "somalia",
    "NER" = "niger", "CIV" = "ci", "ZMB" = "zambia",
    "MOZ" = "mozambique", "UGA" = "uganda", "ZWE" = "zimbabwe"
  )

  slug <- iso3_to_file[iso3]
  if (is.na(slug) || is.null(slug)) return(NULL)

  backbone_path <- file.path("assets", paste0(slug, "_backbone.csv"))
  if (!file.exists(backbone_path)) return(NULL)

  tryCatch({
    bb <- read.csv(backbone_path, stringsAsFactors = FALSE)
    if ("admin_area_2" %in% names(bb)) {
      areas <- unique(bb$admin_area_2[!is.na(bb$admin_area_2)])
      return(areas)
    }
    NULL
  }, error = function(e) NULL)
}

#' Validate admin area names in new data against existing database AND DHIS2 backbone
#' @param new_data data.frame of new survey data
#' @param survey_db data.frame of existing survey database
#' @return list with validation results per country
validate_admin_areas <- function(new_data, survey_db) {
  if (is.null(new_data) || nrow(new_data) == 0) {
    return(list(all_matched = TRUE, unmatched = NULL))
  }

  countries_in_new <- unique(new_data$iso3_code)
  unmatched_list <- list()

  for (iso_code in countries_in_new) {
    # Get existing admin areas for this country from database
    existing_areas <- character(0)
    if (!is.null(survey_db) && nrow(survey_db) > 0) {
      existing_areas <- survey_db %>%
        dplyr::filter(iso3_code == iso_code) %>%
        dplyr::pull(admin_area_2) %>%
        unique()
    }

    # Also get valid areas from DHIS2 backbone
    backbone_areas <- get_backbone_areas(iso_code)
    known_areas <- unique(c(existing_areas, backbone_areas))

    # Get admin areas from new data
    new_areas_summary <- new_data %>%
      dplyr::filter(iso3_code == iso_code) %>%
      dplyr::group_by(iso3_code, country_name, admin_area_2) %>%
      dplyr::summarize(
        years = paste(sort(unique(year)), collapse = ", "),
        n_records = dplyr::n(),
        .groups = "drop"
      )

    # Find which ones are NOT in database OR backbone
    unmatched <- new_areas_summary %>%
      dplyr::filter(!admin_area_2 %in% known_areas)

    if (nrow(unmatched) > 0) {
      dropdown_options <- sort(unique(c(existing_areas, backbone_areas)))
      unmatched$db_options <- list(c("ADD AS NEW", "IGNORE", dropdown_options))
      unmatched_list[[iso_code]] <- unmatched
    }
  }

  if (length(unmatched_list) > 0) {
    unmatched_list <- lapply(unmatched_list, function(df) {
      df %>% dplyr::mutate(iso3_code = as.character(iso3_code))
    })
    unmatched_combined <- dplyr::bind_rows(unmatched_list)
    return(list(all_matched = FALSE, unmatched = unmatched_combined))
  }

  return(list(all_matched = TRUE, unmatched = NULL))
}

#' Apply name corrections to data
#' @param data data.frame of survey data
#' @param corrections data.frame with columns: iso3_code, original_name, selected_name
#' @return data.frame with corrected admin_area_2 names
apply_name_corrections <- function(data, corrections) {
  if (is.null(corrections) || nrow(corrections) == 0) {
    return(data)
  }

  corrected_data <- data

  # Separate IGNORE vs ADD AS NEW vs actual corrections
  to_ignore <- corrections %>% dplyr::filter(selected_name == "IGNORE")
  to_add_new <- corrections %>% dplyr::filter(selected_name == "ADD AS NEW")
  to_correct <- corrections %>% dplyr::filter(!selected_name %in% c("IGNORE", "ADD AS NEW"))

  # Apply corrections
  if (nrow(to_correct) > 0) {
    for (i in 1:nrow(to_correct)) {
      mask <- corrected_data$iso3_code == to_correct$iso3_code[i] &
              corrected_data$admin_area_2 == to_correct$original_name[i]
      corrected_data$admin_area_2[mask] <- to_correct$selected_name[i]
    }
  }

  # Remove ignored rows
  if (nrow(to_ignore) > 0) {
    for (i in 1:nrow(to_ignore)) {
      corrected_data <- corrected_data %>%
        dplyr::filter(!(iso3_code == to_ignore$iso3_code[i] &
                       admin_area_2 == to_ignore$original_name[i]))
    }
  }

  return(corrected_data)
}

# ========================================
# DUPLICATE DETECTION FUNCTIONS
# ========================================

#' Detect duplicate records between new data and existing database
#' @param new_data data.frame of new survey data
#' @param existing_data data.frame of existing database
#' @return list with duplicates and new_records data frames
detect_duplicates <- function(new_data, existing_data) {
  if (is.null(new_data) || nrow(new_data) == 0) {
    return(list(duplicates = data.frame(), new_records = data.frame()))
  }

  # The composite key MUST include `source`. Different sources are designed to coexist for the
  # same area/year/indicator (DHS + WUENIC penta3, DHS + UNWPP crudebr, MICS + UNWPP poptot).
  # Without `source` in the key: (a) a new WUENIC row is mis-flagged as a duplicate of the
  # existing DHS row, (b) the left_join below fans out one new record across every source's row,
  # and (c) an "update" anti_joins away EVERY source at that key. 353 keys / 706 rows in the
  # current DBs hold 2+ sources and were exposed to exactly that. Fixed 2026 Jul 14.
  if (is.null(existing_data) || nrow(existing_data) == 0) {
    # No existing data - all records are new
    new_data$composite_key <- paste(new_data$admin_area_1, new_data$admin_area_2,
                                     new_data$year, new_data$indicator_common_id,
                                     new_data$source, sep = "|||")
    return(list(duplicates = data.frame(), new_records = new_data))
  }

  # Create composite keys
  new_data <- new_data %>%
    dplyr::mutate(composite_key = paste(admin_area_1, admin_area_2, year, indicator_common_id, source, sep = "|||"))

  existing_data <- existing_data %>%
    dplyr::mutate(composite_key = paste(admin_area_1, admin_area_2, year, indicator_common_id, source, sep = "|||"))

  # Find duplicates
  duplicates <- new_data %>%
    dplyr::filter(composite_key %in% existing_data$composite_key) %>%
    dplyr::left_join(
      existing_data %>% dplyr::select(composite_key, existing_value = survey_value) %>%
        dplyr::distinct(composite_key, .keep_all = TRUE),
      by = "composite_key"
    ) %>%
    dplyr::mutate(
      value_diff = survey_value - existing_value,
      pct_diff = ifelse(existing_value != 0, (value_diff / existing_value) * 100, NA),
      action = "keep_existing"  # default action
    )

  # Find truly new records
  new_records <- new_data %>%
    dplyr::filter(!composite_key %in% existing_data$composite_key)

  return(list(
    duplicates = duplicates,
    new_records = new_records
  ))
}

# ========================================
# DATABASE APPEND FUNCTIONS
# ========================================

#' Generate a detailed commit message from the data being added
#' @param new_records data.frame of new records
#' @param user_notes Optional user-provided notes
#' @return Character string with formatted commit message
generate_commit_message <- function(new_records, user_notes = NULL) {
  if (is.null(new_records) || nrow(new_records) == 0) {
    return("Update survey database")
  }

  # Extract summary info
  countries <- unique(new_records$iso3_code)
  indicators <- unique(new_records$indicator_common_id)
  sources <- unique(new_records$source)
  years <- range(new_records$year, na.rm = TRUE)

  # Build title line
  country_str <- if (length(countries) <= 3) {
    paste(countries, collapse = ", ")
  } else {
    paste0(paste(countries[1:3], collapse = ", "), " +", length(countries) - 3, " more")
  }

  title <- paste0("Add data: ", country_str, " (", nrow(new_records), " records)")

  # Build body
  body_parts <- c()

  # Countries
  body_parts <- c(body_parts, paste0("Countries: ", paste(countries, collapse = ", ")))

  # Indicators
  indicator_str <- if (length(indicators) <= 5) {
    paste(indicators, collapse = ", ")
  } else {
    paste0(paste(indicators[1:5], collapse = ", "), " +", length(indicators) - 5, " more")
  }
  body_parts <- c(body_parts, paste0("Indicators: ", indicator_str))

  # Years
  if (years[1] == years[2]) {
    body_parts <- c(body_parts, paste0("Year: ", years[1]))
  } else {
    body_parts <- c(body_parts, paste0("Years: ", years[1], "-", years[2]))
  }

  # Source
  body_parts <- c(body_parts, paste0("Source: ", paste(sources, collapse = ", ")))

  # Record count
  body_parts <- c(body_parts, paste0("Records: ", nrow(new_records)))

  # User notes
  if (!is.null(user_notes) && nchar(trimws(user_notes)) > 0) {
    body_parts <- c(body_parts, "", paste0("Notes: ", user_notes))
  }

  # Timestamp
  body_parts <- c(body_parts, "", paste0("Timestamp: ", format(Sys.time(), "%Y-%m-%d %H:%M UTC")))

  # Combine
  commit_msg <- paste0(title, "\n\n", paste(body_parts, collapse = "\n"))

  return(commit_msg)
}

#' Append validated data to databases and push to GitHub
#' @param new_records data.frame of new records to add
#' @param duplicates data.frame of duplicates with actions
#' @param survey_db data.frame of existing survey database
#' @param pop_db data.frame of existing population database
#' @param create_backup logical, whether to create local backup files
#' @param push_to_github logical, whether to push changes to GitHub
#' @param github_token GitHub personal access token (required for push)
#' @param commit_notes Optional user notes for commit message
#' @return list with success status and message
append_to_databases <- function(new_records, duplicates, survey_db, pop_db,
                                 create_backup = TRUE, push_to_github = TRUE,
                                 github_token = NULL, commit_notes = NULL) {
  tryCatch({
    # Create local backups if requested
    if (create_backup) {
      backup_suffix <- format(Sys.time(), "_%Y%m%d_%H%M%S")
      if (!is.null(survey_db) && nrow(survey_db) > 0) {
        tryCatch({
          write.csv(survey_db, paste0(SURVEY_DB_PATH, backup_suffix, ".backup"),
                    row.names = FALSE, na = "")
        }, error = function(e) {
          message("Could not create local backup (may be on cloud): ", e$message)
        })
      }
      if (!is.null(pop_db) && nrow(pop_db) > 0) {
        tryCatch({
          write.csv(pop_db, paste0(POP_DB_PATH, backup_suffix, ".backup"),
                    row.names = FALSE, na = "")
        }, error = function(e) {
          message("Could not create local backup (may be on cloud): ", e$message)
        })
      }
    }

    # Process duplicates - handle replacements
    records_to_add <- new_records

    if (!is.null(duplicates) && nrow(duplicates) > 0) {
      updates <- duplicates %>%
        dplyr::filter(action != "keep_existing")

      if (nrow(updates) > 0) {
        # Remove old versions from databases.
        # `source` MUST be in the key — without it, updating a DHS value also deletes the
        # WUENIC/UNWPP/MICS row for the same area/year/indicator, which the DB is designed to
        # keep alongside it. See the note in detect_duplicates(). Fixed 2026 Jul 14.
        REPLACE_KEY <- c("admin_area_1", "admin_area_2", "year", "indicator_common_id", "source")

        keys_to_remove <- updates %>%
          dplyr::select(dplyr::all_of(REPLACE_KEY))

        if (!is.null(survey_db) && nrow(survey_db) > 0) {
          survey_db <- survey_db %>%
            dplyr::anti_join(keys_to_remove, by = REPLACE_KEY)
        }

        if (!is.null(pop_db) && nrow(pop_db) > 0) {
          pop_db <- pop_db %>%
            dplyr::anti_join(keys_to_remove, by = REPLACE_KEY)
        }

        # Add updated records to the records to add
        updates_clean <- updates %>%
          dplyr::select(-composite_key, -existing_value, -value_diff, -pct_diff, -action) %>%
          dplyr::mutate(iso3_code = as.character(iso3_code))

        records_to_add <- dplyr::bind_rows(records_to_add, updates_clean)
      }
    }

    # Remove composite_key if it exists
    if ("composite_key" %in% names(records_to_add)) {
      records_to_add <- records_to_add %>% dplyr::select(-composite_key)
    }

    # Split new records into survey vs population
    new_survey_data <- records_to_add %>%
      dplyr::filter(!indicator_common_id %in% POP_INDICATORS) %>%
      dplyr::mutate(iso3_code = as.character(iso3_code))

    new_pop_data <- records_to_add %>%
      dplyr::filter(indicator_common_id %in% POP_INDICATORS) %>%
      dplyr::mutate(iso3_code = as.character(iso3_code))

    # Ensure type consistency in existing databases
    if (!is.null(survey_db) && nrow(survey_db) > 0) {
      survey_db <- survey_db %>% dplyr::mutate(iso3_code = as.character(iso3_code))
    } else {
      survey_db <- data.frame()
    }

    if (!is.null(pop_db) && nrow(pop_db) > 0) {
      pop_db <- pop_db %>% dplyr::mutate(iso3_code = as.character(iso3_code))
    } else {
      pop_db <- data.frame()
    }

    # Append to databases
    updated_survey_db <- dplyr::bind_rows(survey_db, new_survey_data)
    updated_pop_db <- dplyr::bind_rows(pop_db, new_pop_data)

    # Write to local files (if possible)
    # BOTH databases use write.csv(row.names=FALSE, na="") — fully quoted. Do NOT use
    # readr::write_csv here: minimal quoting rewrites every line of a 26k-row file and
    # buries the real change in a spurious diff, and readr writes a literal "NA" for
    # missing values unless na="" is passed. Unified 2026 Jul 14.
    tryCatch({
      write.csv(updated_survey_db, SURVEY_DB_PATH, row.names = FALSE, na = "")
      write.csv(updated_pop_db,    POP_DB_PATH,    row.names = FALSE, na = "")
    }, error = function(e) {
      message("Could not write to local files (may be on cloud): ", e$message)
    })

    # Push directly to main on GitHub
    github_results <- list(survey = NULL, pop = NULL)

    if (push_to_github) {
      if (is.null(github_token) || github_token == "") {
        return(list(
          success = FALSE,
          message = "GitHub token is required to push changes. Please set GITHUB_TOKEN in your environment or .Renviron file."
        ))
      }

      # Generate detailed commit message
      commit_msg <- generate_commit_message(records_to_add, commit_notes)

      # Push files directly to main
      has_survey_updates <- exists("updates") && nrow(updates) > 0
      if (nrow(new_survey_data) > 0 || has_survey_updates) {
        github_results$survey <- push_database_to_github(
          data = updated_survey_db,
          file_path = "survey_data_unified.csv",
          commit_message = commit_msg,
          github_token = github_token
        )
      }

      if (nrow(new_pop_data) > 0) {
        github_results$pop <- push_database_to_github(
          data = updated_pop_db,
          file_path = "population_estimates_only.csv",
          commit_message = commit_msg,
          github_token = github_token
        )
      }

      # Check for push errors
      push_errors <- c()
      if (!is.null(github_results$survey) && !github_results$survey$success) {
        push_errors <- c(push_errors, paste("Survey:", github_results$survey$message))
      }
      if (!is.null(github_results$pop) && !github_results$pop$success) {
        push_errors <- c(push_errors, paste("Population:", github_results$pop$message))
      }

      if (length(push_errors) > 0) {
        return(list(
          success = FALSE,
          message = paste("GitHub push failed:", paste(push_errors, collapse = "; ")),
          survey_added = nrow(new_survey_data),
          pop_added = nrow(new_pop_data)
        ))
      }
    }

    return(list(
      success = TRUE,
      message = paste0("Successfully added ", nrow(new_survey_data), " survey records and ",
                      nrow(new_pop_data), " population records.",
                      if(push_to_github) " Changes pushed to GitHub!" else ""),
      survey_added = nrow(new_survey_data),
      pop_added = nrow(new_pop_data),
      survey_total = nrow(updated_survey_db),
      pop_total = nrow(updated_pop_db),
      updated_survey_db = updated_survey_db,
      updated_pop_db = updated_pop_db,
      pushed_to_github = push_to_github
    ))

  }, error = function(e) {
    return(list(
      success = FALSE,
      message = paste("Error during append:", e$message)
    ))
  })
}
