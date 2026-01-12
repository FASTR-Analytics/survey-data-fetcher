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
                    "womenrepage", "popgrowth")

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

#' Push database to GitHub
#' @param data data.frame to push
#' @param file_path Path in the repo (e.g., "survey_data_unified.csv")
#' @param commit_message Commit message
#' @param github_token GitHub personal access token
#' @return list with success status and message
push_database_to_github <- function(data, file_path, commit_message, github_token) {
  if (is.null(github_token) || github_token == "") {
    return(list(success = FALSE, message = "GitHub token not provided"))
  }

  tryCatch({
    # Convert data to CSV string
    csv_content <- readr::format_csv(data)
    csv_base64 <- base64enc::base64encode(charToRaw(csv_content))

    # Get current file SHA (required for updates)
    api_url <- paste0("https://api.github.com/repos/", GITHUB_OWNER, "/", GITHUB_REPO,
                      "/contents/", file_path, "?ref=", GITHUB_BRANCH)

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
      branch = GITHUB_BRANCH
    )

    if (!is.null(sha)) {
      body$sha <- sha
    }

    # Push to GitHub
    put_response <- httr::PUT(
      api_url,
      httr::add_headers(
        Authorization = paste("Bearer", github_token),
        Accept = "application/vnd.github.v3+json"
      ),
      body = body,
      encode = "json"
    )

    if (httr::status_code(put_response) %in% c(200, 201)) {
      return(list(success = TRUE, message = paste("Successfully pushed", file_path, "to GitHub")))
    } else {
      error_content <- httr::content(put_response)
      return(list(success = FALSE, message = paste("GitHub push failed:", error_content$message)))
    }

  }, error = function(e) {
    return(list(success = FALSE, message = paste("Error pushing to GitHub:", e$message)))
  })
}

# ========================================
# DATABASE LOADING FUNCTIONS
# ========================================

#' Load the survey database (from GitHub first, fallback to local)
#' @param use_github Whether to pull from GitHub (default TRUE)
#' @return data.frame of survey data or NULL if not available
load_survey_database <- function(use_github = TRUE) {
  if (use_github) {
    data <- pull_database_from_github(SURVEY_DB_URL)
    if (!is.null(data)) {
      return(data)
    }
  }

  # Fallback to local file
  if (file.exists(SURVEY_DB_PATH)) {
    return(readr::read_csv(SURVEY_DB_PATH, show_col_types = FALSE))
  }
  return(NULL)
}

#' Load the population database (from GitHub first, fallback to local)
#' @param use_github Whether to pull from GitHub (default TRUE)
#' @return data.frame of population data or NULL if not available
load_population_database <- function(use_github = TRUE) {
  if (use_github) {
    data <- pull_database_from_github(POP_DB_URL)
    if (!is.null(data)) {
      return(data)
    }
  }

  # Fallback to local file
  if (file.exists(POP_DB_PATH)) {
    return(readr::read_csv(POP_DB_PATH, show_col_types = FALSE))
  }
  return(NULL)
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

#' Validate admin area names in new data against existing database
#' @param new_data data.frame of new survey data
#' @param survey_db data.frame of existing survey database
#' @return list with validation results per country
validate_admin_areas <- function(new_data, survey_db) {
  if (is.null(new_data) || nrow(new_data) == 0) {
    return(list(all_matched = TRUE, unmatched = NULL))
  }

  if (is.null(survey_db) || nrow(survey_db) == 0) {
    # No existing database - all areas are "new"
    return(list(
      all_matched = FALSE,
      unmatched = new_data %>%
        dplyr::select(iso3_code, country_name, admin_area_2) %>%
        dplyr::distinct() %>%
        dplyr::mutate(
          db_options = list(c("IGNORE")),
          years = "all",
          n_records = nrow(new_data)
        )
    ))
  }

  countries_in_new <- unique(new_data$iso3_code)
  unmatched_list <- list()

  for (iso_code in countries_in_new) {
    # Get existing admin areas for this country
    existing_areas <- survey_db %>%
      dplyr::filter(iso3_code == iso_code) %>%
      dplyr::pull(admin_area_2) %>%
      unique()

    # Get admin areas from new data
    new_areas_summary <- new_data %>%
      dplyr::filter(iso3_code == iso_code) %>%
      dplyr::group_by(iso3_code, country_name, admin_area_2) %>%
      dplyr::summarize(
        years = paste(sort(unique(year)), collapse = ", "),
        n_records = dplyr::n(),
        .groups = "drop"
      )

    # Find which ones are NOT in database
    unmatched <- new_areas_summary %>%
      dplyr::filter(!admin_area_2 %in% existing_areas)

    if (nrow(unmatched) > 0) {
      unmatched$db_options <- list(c("IGNORE", sort(existing_areas)))
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

  # Separate IGNORE vs actual corrections
  to_ignore <- corrections %>% dplyr::filter(selected_name == "IGNORE")
  to_correct <- corrections %>% dplyr::filter(selected_name != "IGNORE")

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

  if (is.null(existing_data) || nrow(existing_data) == 0) {
    # No existing data - all records are new
    new_data$composite_key <- paste(new_data$admin_area_1, new_data$admin_area_2,
                                     new_data$year, new_data$indicator_common_id, sep = "|||")
    return(list(duplicates = data.frame(), new_records = new_data))
  }

  # Create composite keys
  new_data <- new_data %>%
    dplyr::mutate(composite_key = paste(admin_area_1, admin_area_2, year, indicator_common_id, sep = "|||"))

  existing_data <- existing_data %>%
    dplyr::mutate(composite_key = paste(admin_area_1, admin_area_2, year, indicator_common_id, sep = "|||"))

  # Find duplicates
  duplicates <- new_data %>%
    dplyr::filter(composite_key %in% existing_data$composite_key) %>%
    dplyr::left_join(
      existing_data %>% dplyr::select(composite_key, existing_value = survey_value),
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

#' Append validated data to databases and push to GitHub
#' @param new_records data.frame of new records to add
#' @param duplicates data.frame of duplicates with actions
#' @param survey_db data.frame of existing survey database
#' @param pop_db data.frame of existing population database
#' @param create_backup logical, whether to create local backup files
#' @param push_to_github logical, whether to push changes to GitHub
#' @param github_token GitHub personal access token (required for push)
#' @return list with success status and message
append_to_databases <- function(new_records, duplicates, survey_db, pop_db,
                                 create_backup = TRUE, push_to_github = TRUE,
                                 github_token = NULL) {
  tryCatch({
    # Create local backups if requested
    if (create_backup) {
      backup_suffix <- format(Sys.time(), "_%Y%m%d_%H%M%S")
      if (!is.null(survey_db) && nrow(survey_db) > 0) {
        tryCatch({
          readr::write_csv(survey_db, paste0(SURVEY_DB_PATH, backup_suffix, ".backup"))
        }, error = function(e) {
          message("Could not create local backup (may be on cloud): ", e$message)
        })
      }
      if (!is.null(pop_db) && nrow(pop_db) > 0) {
        tryCatch({
          readr::write_csv(pop_db, paste0(POP_DB_PATH, backup_suffix, ".backup"))
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
        # Remove old versions from databases
        keys_to_remove <- updates %>%
          dplyr::select(admin_area_1, admin_area_2, year, indicator_common_id)

        if (!is.null(survey_db) && nrow(survey_db) > 0) {
          survey_db <- survey_db %>%
            dplyr::anti_join(keys_to_remove, by = c("admin_area_1", "admin_area_2", "year", "indicator_common_id"))
        }

        if (!is.null(pop_db) && nrow(pop_db) > 0) {
          pop_db <- pop_db %>%
            dplyr::anti_join(keys_to_remove, by = c("admin_area_1", "admin_area_2", "year", "indicator_common_id"))
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
    tryCatch({
      readr::write_csv(updated_survey_db, SURVEY_DB_PATH)
      readr::write_csv(updated_pop_db, POP_DB_PATH)
    }, error = function(e) {
      message("Could not write to local files (may be on cloud): ", e$message)
    })

    # Push to GitHub if requested
    github_results <- list(survey = NULL, pop = NULL)

    if (push_to_github) {
      if (is.null(github_token) || github_token == "") {
        return(list(
          success = FALSE,
          message = "GitHub token is required to push changes. Please set GITHUB_TOKEN in your environment or .Renviron file."
        ))
      }

      # Generate commit message
      timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M")
      commit_msg <- paste0("Update survey data: +", nrow(new_survey_data), " survey, +",
                           nrow(new_pop_data), " population records (", timestamp, ")")

      # Push survey database
      if (nrow(new_survey_data) > 0 || nrow(updates) > 0) {
        github_results$survey <- push_database_to_github(
          data = updated_survey_db,
          file_path = "survey_data_unified.csv",
          commit_message = commit_msg,
          github_token = github_token
        )
      }

      # Push population database
      if (nrow(new_pop_data) > 0) {
        github_results$pop <- push_database_to_github(
          data = updated_pop_db,
          file_path = "population_estimates_only.csv",
          commit_message = commit_msg,
          github_token = github_token
        )
      }

      # Check for GitHub push errors
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
      message = paste("Successfully added", nrow(new_survey_data), "survey records and",
                     nrow(new_pop_data), "population records.",
                     if(push_to_github) " Changes pushed to GitHub." else ""),
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
