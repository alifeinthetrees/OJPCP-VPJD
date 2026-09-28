# ==============================================================================
# 01_gbif_institution_acquire.R
# VPJD / Oxford-Japan Plant Conservation Partnership
#
# Purpose:
#   Registry-driven acquisition of Japanese occurrence records from the
#   institutional GBIF datasets represented in the legacy VPJD system.
#
# Modes:
#   check_gbif_institution()         Check one dataset; no download.
#   acquire_gbif_institution()       Acquire one dataset.
#   acquire_all_gbif_institutions() Acquire all active READY datasets.
#
# IMPORTANT:
# - Sourcing this script DOES NOT submit GBIF downloads.
# - Batch acquisition begins only when acquire_all_gbif_institutions() is called.
# - Query = datasetKey + country=JP.
# - Raw GBIF ZIP archives are immutable source data.
# - Existing successful local snapshots are skipped by default.
# - Batch progress is checkpointed after every institution.
# - Individual failures do not terminate the batch.
# - No cleaning, WCVP reconciliation, FOJ Stars or GHI processing occurs here.
#
# Version: 0.3.0
# ==============================================================================

VPJD_INSTITUTION_ACQUIRE_VERSION <- "0.3.0"

# Packages ---------------------------------------------------------------------

required_packages <- c(
  "here", "rgbif", "readr", "dplyr", "stringr", "tibble", "cli"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0L) {
  stop(
    "Required package(s) not installed: ",
    paste(missing_packages, collapse = ", "),
    "\nInstall with: install.packages(c(",
    paste0('"', missing_packages, '"', collapse = ", "),
    "))"
  )
}

suppressPackageStartupMessages({
  library(here)
  library(rgbif)
  library(readr)
  library(dplyr)
  library(stringr)
  library(tibble)
  library(cli)
})

# Configuration ----------------------------------------------------------------

registry_path <- here::here(
  "data", "raw", "external", "legacy_gbif",
  "gbif_institution_registry.csv"
)

institution_root <- here::here(
  "data", "raw", "GBIF", "institutions"
)

batch_metadata_dir <- file.path(
  institution_root,
  "_batch_metadata"
)

country_code <- "JP"
download_format <- "SIMPLE_CSV"

dir.create(
  institution_root,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  batch_metadata_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

# Helpers ----------------------------------------------------------------------

sanitize_directory_name <- function(x) {
  x <- stringr::str_squish(x)
  x <- stringr::str_replace_all(x, "[^A-Za-z0-9]+", "_")
  x <- stringr::str_replace_all(x, "_+", "_")
  x <- stringr::str_replace_all(x, "^_|_$", "")
  
  if (
    length(x) != 1L ||
    is.na(x) ||
    x == ""
  ) {
    stop("Could not derive a valid institution directory name.")
  }
  
  x
}

validate_dataset_key <- function(x) {
  pattern <- paste0(
    "^[0-9a-fA-F]{8}-",
    "[0-9a-fA-F]{4}-",
    "[0-9a-fA-F]{4}-",
    "[0-9a-fA-F]{4}-",
    "[0-9a-fA-F]{12}$"
  )
  
  if (
    length(x) != 1L ||
    is.na(x) ||
    !grepl(pattern, x)
  ) {
    stop("Invalid GBIF datasetKey: ", x)
  }
  
  invisible(TRUE)
}

validate_credentials <- function() {
  credentials <- c(
    GBIF_USER = Sys.getenv("GBIF_USER"),
    GBIF_PWD = Sys.getenv("GBIF_PWD"),
    GBIF_EMAIL = Sys.getenv("GBIF_EMAIL")
  )
  
  missing <- names(credentials)[credentials == ""]
  
  if (length(missing) > 0L) {
    stop(
      "Missing GBIF credential(s) in .Renviron: ",
      paste(missing, collapse = ", ")
    )
  }
  
  invisible(credentials)
}

format_count <- function(x) {
  if (
    length(x) == 0L ||
    is.na(x)
  ) {
    return("NA")
  }
  
  format(
    x,
    big.mark = ",",
    scientific = FALSE,
    trim = TRUE
  )
}

format_bytes <- function(bytes) {
  if (
    length(bytes) == 0L ||
    is.na(bytes)
  ) {
    return("NA")
  }
  
  units <- c("B", "KB", "MB", "GB", "TB")
  value <- as.numeric(bytes)
  i <- 1L
  
  while (
    value >= 1024 &&
    i < length(units)
  ) {
    value <- value / 1024
    i <- i + 1L
  }
  
  paste0(
    format(
      round(value, 2),
      nsmall = 2,
      trim = TRUE
    ),
    " ",
    units[[i]]
  )
}

timestamp_id <- function() {
  format(
    Sys.time(),
    "%Y%m%d_%H%M%S"
  )
}

institution_paths <- function(title) {
  slug <- sanitize_directory_name(title)
  
  institution_dir <- file.path(
    institution_root,
    slug
  )
  
  list(
    slug = slug,
    institution_dir = institution_dir,
    download_dir = file.path(
      institution_dir,
      "downloads"
    ),
    metadata_dir = file.path(
      institution_dir,
      "metadata"
    )
  )
}

# Load and validate registry ---------------------------------------------------

if (!file.exists(registry_path)) {
  stop(
    "Institutional GBIF registry not found:\n",
    registry_path,
    "\nRun 00_build_legacy_gbif_registry.R first."
  )
}

institution_registry <- readr::read_csv(
  registry_path,
  show_col_types = FALSE
)

required_registry_columns <- c(
  "registry_id",
  "dataset_title",
  "dataset_key",
  "active",
  "country_filter",
  "registry_status"
)

missing_registry_columns <- setdiff(
  required_registry_columns,
  names(institution_registry)
)

if (length(missing_registry_columns) > 0L) {
  stop(
    "Institution registry is missing required column(s): ",
    paste(missing_registry_columns, collapse = ", ")
  )
}

if (anyDuplicated(institution_registry$dataset_key) > 0L) {
  stop("Institution registry contains duplicated datasetKeys.")
}

if (any(is.na(institution_registry$dataset_key))) {
  stop("Institution registry contains missing datasetKeys.")
}

invisible(
  lapply(
    institution_registry$dataset_key,
    validate_dataset_key
  )
)

active_registry <- institution_registry |>
  dplyr::filter(
    .data$active,
    .data$registry_status == "READY"
  )

# Registry lookup --------------------------------------------------------------

get_registered_institution <- function(
    registry_id = NULL,
    dataset_key = NULL
) {
  if (
    is.null(registry_id) &&
    is.null(dataset_key)
  ) {
    stop("Supply either registry_id or dataset_key.")
  }
  
  if (
    !is.null(registry_id) &&
    !is.null(dataset_key)
  ) {
    stop("Supply registry_id OR dataset_key, not both.")
  }
  
  if (!is.null(registry_id)) {
    lookup_registry_id <- registry_id
    
    result <- institution_registry |>
      dplyr::filter(
        .data$registry_id == .env$lookup_registry_id
      )
  } else {
    lookup_dataset_key <- dataset_key
    
    result <- institution_registry |>
      dplyr::filter(
        .data$dataset_key == .env$lookup_dataset_key
      )
  }
  
  if (nrow(result) == 0L) {
    stop("No matching institutional dataset found in registry.")
  }
  
  if (nrow(result) > 1L) {
    stop("Registry lookup returned more than one dataset.")
  }
  
  result
}

# Existing snapshot detection --------------------------------------------------

get_existing_snapshot <- function(title) {
  paths <- institution_paths(title)
  
  if (!dir.exists(paths$metadata_dir)) {
    return(NULL)
  }
  
  metadata_files <- list.files(
    paths$metadata_dir,
    pattern = "^gbif_download_.*\\.csv$",
    full.names = TRUE
  )
  
  if (length(metadata_files) == 0L) {
    return(NULL)
  }
  
  metadata_rows <- lapply(
    metadata_files,
    function(path) {
      tryCatch(
        readr::read_csv(
          path,
          show_col_types = FALSE
        ),
        error = function(e) NULL
      )
    }
  )
  
  metadata_rows <- metadata_rows[
    !vapply(metadata_rows, is.null, logical(1))
  ]
  
  if (length(metadata_rows) == 0L) {
    return(NULL)
  }
  
  combined <- dplyr::bind_rows(
    metadata_rows
  )
  
  if (
    !"status" %in% names(combined) ||
    !"raw_archive" %in% names(combined)
  ) {
    return(NULL)
  }
  
  successful <- combined |>
    dplyr::filter(
      .data$status == "SUCCEEDED",
      !is.na(.data$raw_archive)
    )
  
  if (nrow(successful) == 0L) {
    return(NULL)
  }
  
  archive_exists <- file.exists(
    successful$raw_archive
  )
  
  successful <- successful[
    archive_exists,
    ,
    drop = FALSE
  ]
  
  if (nrow(successful) == 0L) {
    return(NULL)
  }
  
  if ("completed_at" %in% names(successful)) {
    successful <- successful |>
      dplyr::arrange(
        dplyr::desc(.data$completed_at)
      )
  }
  
  successful[1, , drop = FALSE]
}

# Current-count check ----------------------------------------------------------

check_gbif_institution <- function(
    registry_id = NULL,
    dataset_key = NULL,
    quiet = FALSE
) {
  row <- get_registered_institution(
    registry_id = registry_id,
    dataset_key = dataset_key
  )
  
  key <- row$dataset_key[[1]]
  title <- row$dataset_title[[1]]
  
  validate_dataset_key(key)
  
  if (!quiet) {
    cli::cli_h2("GBIF institutional check")
    cli::cli_text("{.strong Dataset:} {title}")
    cli::cli_text("{.strong datasetKey:} {key}")
    
    progress_id <- cli::cli_progress_bar(
      "Checking GBIF",
      total = 2,
      format = paste0(
        "{cli::pb_bar} {cli::pb_percent} | ",
        "{cli::pb_current}/{cli::pb_total} | ",
        "{cli::pb_status} | {cli::pb_elapsed}"
      )
    )
    
    cli::cli_progress_update(
      id = progress_id,
      set = 0,
      status = "Querying total dataset records"
    )
  }
  
  all_records <- rgbif::occ_count(
    datasetKey = key
  )
  
  if (!quiet) {
    cli::cli_progress_update(
      id = progress_id,
      set = 1,
      status = paste0(
        "Total records: ",
        format_count(all_records),
        " | querying Japan"
      )
    )
  }
  
  japan_records <- rgbif::occ_count(
    datasetKey = key,
    country = country_code
  )
  
  if (!quiet) {
    cli::cli_progress_update(
      id = progress_id,
      set = 2,
      status = paste0(
        "Japan records: ",
        format_count(japan_records)
      )
    )
    
    cli::cli_progress_done(
      id = progress_id
    )
    
    cli::cli_alert_success(
      "GBIF check complete."
    )
    
    cli::cli_text(
      "{.strong All records:} {format_count(all_records)}"
    )
    
    cli::cli_text(
      "{.strong Japan records:} {format_count(japan_records)}"
    )
  }
  
  tibble::tibble(
    checked_at = format(
      Sys.time(),
      "%Y-%m-%d %H:%M:%S"
    ),
    registry_id = row$registry_id[[1]],
    dataset_title = title,
    dataset_key = key,
    all_records = as.numeric(all_records),
    japan_records = as.numeric(japan_records),
    country_filter = country_code
  )
}

# GBIF processing monitor ------------------------------------------------------

wait_for_gbif_download <- function(
    download_key,
    poll_seconds = 30,
    quiet = FALSE
) {
  if (!quiet) {
    spinner_id <- cli::cli_progress_bar(
      format = paste0(
        "{cli::pb_spin} GBIF processing | ",
        "{cli::pb_status} | {cli::pb_elapsed}"
      ),
      total = NA
    )
  }
  
  repeat {
    meta <- tryCatch(
      rgbif::occ_download_meta(
        download_key
      ),
      error = function(e) NULL
    )
    
    if (is.null(meta)) {
      if (!quiet) {
        cli::cli_progress_update(
          id = spinner_id,
          status = "Waiting for GBIF status"
        )
      }
      
      Sys.sleep(
        poll_seconds
      )
      
      next
    }
    
    status <- if (
      !is.null(meta$status)
    ) {
      as.character(
        meta$status
      )
    } else {
      "UNKNOWN"
    }
    
    records <- if (
      !is.null(meta$totalRecords)
    ) {
      suppressWarnings(
        as.numeric(
          meta$totalRecords
        )
      )
    } else {
      NA_real_
    }
    
    if (!quiet) {
      status_text <- if (
        !is.na(records)
      ) {
        paste0(
          status,
          " | ",
          format_count(records),
          " records"
        )
      } else {
        status
      }
      
      cli::cli_progress_update(
        id = spinner_id,
        status = status_text
      )
    }
    
    if (status == "SUCCEEDED") {
      if (!quiet) {
        cli::cli_progress_done(
          id = spinner_id
        )
        
        cli::cli_alert_success(
          "GBIF processing succeeded."
        )
      }
      
      return(meta)
    }
    
    if (
      status %in% c(
        "FAILED",
        "CANCELLED",
        "KILLED"
      )
    ) {
      if (!quiet) {
        cli::cli_progress_done(
          id = spinner_id
        )
      }
      
      stop(
        "GBIF download ended with status: ",
        status
      )
    }
    
    Sys.sleep(
      poll_seconds
    )
  }
}

# Acquire one institution ------------------------------------------------------

acquire_gbif_institution <- function(
    registry_id = NULL,
    dataset_key = NULL,
    poll_seconds = 30,
    skip_existing = TRUE,
    quiet = FALSE
) {
  row <- get_registered_institution(
    registry_id = registry_id,
    dataset_key = dataset_key
  )
  
  if (!isTRUE(row$active[[1]])) {
    stop(
      "Registry entry is not active: ",
      row$registry_id[[1]]
    )
  }
  
  if (
    !identical(
      row$registry_status[[1]],
      "READY"
    )
  ) {
    stop(
      "Registry entry is not READY: ",
      row$registry_id[[1]],
      " [",
      row$registry_status[[1]],
      "]"
    )
  }
  
  key <- row$dataset_key[[1]]
  title <- row$dataset_title[[1]]
  
  validate_dataset_key(key)
  
  paths <- institution_paths(
    title
  )
  
  dir.create(
    paths$download_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  dir.create(
    paths$metadata_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  if (skip_existing) {
    existing <- get_existing_snapshot(
      title
    )
    
    if (!is.null(existing)) {
      if (!quiet) {
        cli::cli_alert_info(
          "Existing successful snapshot found; skipping download."
        )
      }
      
      return(
        tibble::tibble(
          registry_id = row$registry_id[[1]],
          dataset_title = title,
          dataset_key = key,
          result = "SKIPPED_EXISTING",
          download_key = if (
            "download_key" %in% names(existing)
          ) {
            as.character(
              existing$download_key[[1]]
            )
          } else {
            NA_character_
          },
          doi = if (
            "doi" %in% names(existing)
          ) {
            as.character(
              existing$doi[[1]]
            )
          } else {
            NA_character_
          },
          downloaded_records = if (
            "downloaded_records" %in% names(existing)
          ) {
            as.numeric(
              existing$downloaded_records[[1]]
            )
          } else {
            NA_real_
          },
          archive = as.character(
            existing$raw_archive[[1]]
          ),
          error_message = NA_character_
        )
      )
    }
  }
  
  credentials <- validate_credentials()
  
  if (!quiet) {
    cli::cli_h1(
      "VPJD institutional GBIF acquisition"
    )
    
    cli::cli_text(
      "{.strong Dataset:} {title}"
    )
    
    cli::cli_text(
      "{.strong datasetKey:} {key}"
    )
  }
  
  counts <- check_gbif_institution(
    dataset_key = key,
    quiet = TRUE
  )
  
  dataset_all <- counts$all_records[[1]]
  dataset_japan <- counts$japan_records[[1]]
  
  if (
    is.na(dataset_japan) ||
    dataset_japan == 0
  ) {
    if (!quiet) {
      cli::cli_alert_warning(
        "No Japanese records. No download submitted."
      )
    }
    
    return(
      tibble::tibble(
        registry_id = row$registry_id[[1]],
        dataset_title = title,
        dataset_key = key,
        result = "ZERO_RECORDS",
        download_key = NA_character_,
        doi = NA_character_,
        downloaded_records = 0,
        archive = NA_character_,
        error_message = NA_character_
      )
    )
  }
  
  if (!quiet) {
    cli::cli_alert_info(
      "{format_count(dataset_japan)} Japanese records currently reported."
    )
    
    cli::cli_alert_info(
      "Submitting GBIF download..."
    )
  }
  
  download_request <- rgbif::occ_download(
    rgbif::pred(
      "datasetKey",
      key
    ),
    rgbif::pred(
      "country",
      country_code
    ),
    format = download_format,
    user = credentials[["GBIF_USER"]],
    pwd = credentials[["GBIF_PWD"]],
    email = credentials[["GBIF_EMAIL"]]
  )
  
  download_key <- as.character(
    download_request
  )
  
  if (
    length(download_key) != 1L ||
    is.na(download_key) ||
    download_key == ""
  ) {
    stop(
      "GBIF request was submitted but a valid download key was not returned."
    )
  }
  
  submission_path <- file.path(
    paths$metadata_dir,
    paste0(
      "gbif_submission_",
      download_key,
      ".csv"
    )
  )
  
  submission_metadata <- tibble::tibble(
    vpjd_acquisition_version =
      VPJD_INSTITUTION_ACQUIRE_VERSION,
    submitted_at = format(
      Sys.time(),
      "%Y-%m-%d %H:%M:%S"
    ),
    registry_id = row$registry_id[[1]],
    dataset_title = title,
    dataset_key = key,
    country_filter = country_code,
    download_format = download_format,
    gbif_records_all_at_submission =
      as.numeric(dataset_all),
    gbif_records_japan_at_submission =
      as.numeric(dataset_japan),
    download_key = download_key
  )
  
  readr::write_csv(
    submission_metadata,
    submission_path
  )
  
  download_metadata <- wait_for_gbif_download(
    download_key = download_key,
    poll_seconds = poll_seconds,
    quiet = quiet
  )
  
  if (!quiet) {
    cli::cli_alert_info(
      "Retrieving raw GBIF ZIP..."
    )
  }
  
  download_result <- rgbif::occ_download_get(
    download_key,
    path = paths$download_dir,
    overwrite = FALSE
  )
  
  zip_path <- as.character(
    download_result
  )
  
  if (
    length(zip_path) != 1L ||
    !file.exists(zip_path)
  ) {
    stop(
      "GBIF download completed but ZIP archive could not be confirmed."
    )
  }
  
  archive_bytes <- file.info(
    zip_path
  )$size
  
  doi <- if (
    !is.null(download_metadata$doi)
  ) {
    as.character(
      download_metadata$doi
    )
  } else {
    NA_character_
  }
  
  downloaded_records <- if (
    !is.null(download_metadata$totalRecords)
  ) {
    as.numeric(
      download_metadata$totalRecords
    )
  } else {
    NA_real_
  }
  
  status <- if (
    !is.null(download_metadata$status)
  ) {
    as.character(
      download_metadata$status
    )
  } else {
    NA_character_
  }
  
  created <- if (
    !is.null(download_metadata$created)
  ) {
    as.character(
      download_metadata$created
    )
  } else {
    NA_character_
  }
  
  modified <- if (
    !is.null(download_metadata$modified)
  ) {
    as.character(
      download_metadata$modified
    )
  } else {
    NA_character_
  }
  
  completion_path <- file.path(
    paths$metadata_dir,
    paste0(
      "gbif_download_",
      download_key,
      ".csv"
    )
  )
  
  completion_metadata <- tibble::tibble(
    vpjd_acquisition_version =
      VPJD_INSTITUTION_ACQUIRE_VERSION,
    completed_at = format(
      Sys.time(),
      "%Y-%m-%d %H:%M:%S"
    ),
    registry_id = row$registry_id[[1]],
    dataset_title = title,
    dataset_key = key,
    country_filter = country_code,
    download_format = download_format,
    download_key = download_key,
    doi = doi,
    status = status,
    downloaded_records = downloaded_records,
    gbif_created = created,
    gbif_modified = modified,
    raw_archive = normalizePath(
      zip_path,
      winslash = "/",
      mustWork = FALSE
    ),
    archive_bytes = archive_bytes
  )
  
  readr::write_csv(
    completion_metadata,
    completion_path
  )
  
  if (!quiet) {
    cli::cli_alert_success(
      "{title} complete."
    )
    
    cli::cli_text(
      "{.strong Download key:} {download_key}"
    )
    
    cli::cli_text(
      "{.strong DOI:} {doi}"
    )
    
    cli::cli_text(
      "{.strong Records:} {format_count(downloaded_records)}"
    )
    
    cli::cli_text(
      "{.strong Archive:} {format_bytes(archive_bytes)}"
    )
  }
  
  tibble::tibble(
    registry_id = row$registry_id[[1]],
    dataset_title = title,
    dataset_key = key,
    result = "SUCCEEDED",
    download_key = download_key,
    doi = doi,
    downloaded_records = downloaded_records,
    archive = normalizePath(
      zip_path,
      winslash = "/",
      mustWork = FALSE
    ),
    error_message = NA_character_
  )
}

# Batch acquisition ------------------------------------------------------------

acquire_all_gbif_institutions <- function(
    poll_seconds = 30,
    skip_existing = TRUE,
    resume = TRUE
) {
  validate_credentials()
  
  batch_registry <- active_registry |>
    dplyr::arrange(
      .data$dataset_title
    )
  
  n_total <- nrow(
    batch_registry
  )
  
  if (n_total == 0L) {
    stop("No active READY institutional datasets found.")
  }
  
  batch_id <- timestamp_id()
  
  current_log_path <- file.path(
    batch_metadata_dir,
    "institution_acquisition_current.csv"
  )
  
  batch_log_path <- file.path(
    batch_metadata_dir,
    paste0(
      "institution_acquisition_",
      batch_id,
      ".csv"
    )
  )
  
  manifest_path <- file.path(
    batch_metadata_dir,
    paste0(
      "institution_acquisition_manifest_",
      batch_id,
      ".csv"
    )
  )
  
  previous_log <- NULL
  
  if (
    resume &&
    file.exists(current_log_path)
  ) {
    previous_log <- tryCatch(
      readr::read_csv(
        current_log_path,
        show_col_types = FALSE
      ),
      error = function(e) NULL
    )
  }
  
  completed_keys <- character()
  
  if (
    !is.null(previous_log) &&
    nrow(previous_log) > 0L &&
    all(
      c(
        "dataset_key",
        "result"
      ) %in% names(previous_log)
    )
  ) {
    completed_keys <- previous_log |>
      dplyr::filter(
        .data$result %in% c(
          "SUCCEEDED",
          "SKIPPED_EXISTING",
          "ZERO_RECORDS"
        )
      ) |>
      dplyr::pull(
        .data$dataset_key
      ) |>
      unique()
  }
  
  cli::cli_h1(
    "VPJD institutional GBIF batch acquisition"
  )
  
  cli::cli_text(
    "{.strong Registered datasets:} {n_total}"
  )
  
  cli::cli_text(
    "{.strong Country filter:} {country_code}"
  )
  
  cli::cli_text(
    "{.strong Existing successful snapshots:} ",
    if (skip_existing) "skip" else "download again"
  )
  
  cli::cli_text(
    "{.strong Resume previous progress:} ",
    if (resume) "yes" else "no"
  )
  
  cli::cli_alert_warning(
    "This command will submit GBIF downloads for datasets requiring acquisition."
  )
  
  progress_id <- cli::cli_progress_bar(
    "Institutional datasets",
    total = n_total,
    format = paste0(
      "{cli::pb_bar} {cli::pb_percent} | ",
      "{cli::pb_current}/{cli::pb_total} | ",
      "{cli::pb_status} | {cli::pb_elapsed}"
    ),
    clear = FALSE
  )
  
  results <- vector(
    "list",
    n_total
  )
  
  for (i in seq_len(n_total)) {
    row <- batch_registry[
      i,
      ,
      drop = FALSE
    ]
    
    key <- row$dataset_key[[1]]
    title <- row$dataset_title[[1]]
    
    cli::cli_progress_update(
      id = progress_id,
      set = i - 1L,
      status = paste0(
        "[",
        i,
        "/",
        n_total,
        "] ",
        title
      )
    )
    
    if (
      key %in% completed_keys
    ) {
      previous_row <- previous_log |>
        dplyr::filter(
          .data$dataset_key == .env$key
        ) |>
        dplyr::slice_tail(
          n = 1
        )
      
      results[[i]] <- tibble::tibble(
        batch_id = batch_id,
        sequence = i,
        attempted_at = format(
          Sys.time(),
          "%Y-%m-%d %H:%M:%S"
        ),
        registry_id = row$registry_id[[1]],
        dataset_title = title,
        dataset_key = key,
        result = "SKIPPED_RESUME",
        download_key = if (
          "download_key" %in% names(previous_row)
        ) {
          as.character(
            previous_row$download_key[[1]]
          )
        } else {
          NA_character_
        },
        doi = if (
          "doi" %in% names(previous_row)
        ) {
          as.character(
            previous_row$doi[[1]]
          )
        } else {
          NA_character_
        },
        downloaded_records = if (
          "downloaded_records" %in% names(previous_row)
        ) {
          as.numeric(
            previous_row$downloaded_records[[1]]
          )
        } else {
          NA_real_
        },
        archive = if (
          "archive" %in% names(previous_row)
        ) {
          as.character(
            previous_row$archive[[1]]
          )
        } else {
          NA_character_
        },
        error_message = NA_character_
      )
    } else {
      result <- tryCatch(
        {
          acquisition <- acquire_gbif_institution(
            dataset_key = key,
            poll_seconds = poll_seconds,
            skip_existing = skip_existing,
            quiet = TRUE
          )
          
          tibble::tibble(
            batch_id = batch_id,
            sequence = i,
            attempted_at = format(
              Sys.time(),
              "%Y-%m-%d %H:%M:%S"
            ),
            registry_id =
              acquisition$registry_id[[1]],
            dataset_title =
              acquisition$dataset_title[[1]],
            dataset_key =
              acquisition$dataset_key[[1]],
            result =
              acquisition$result[[1]],
            download_key =
              acquisition$download_key[[1]],
            doi =
              acquisition$doi[[1]],
            downloaded_records =
              acquisition$downloaded_records[[1]],
            archive =
              acquisition$archive[[1]],
            error_message =
              acquisition$error_message[[1]]
          )
        },
        error = function(e) {
          tibble::tibble(
            batch_id = batch_id,
            sequence = i,
            attempted_at = format(
              Sys.time(),
              "%Y-%m-%d %H:%M:%S"
            ),
            registry_id =
              row$registry_id[[1]],
            dataset_title = title,
            dataset_key = key,
            result = "ERROR",
            download_key = NA_character_,
            doi = NA_character_,
            downloaded_records = NA_real_,
            archive = NA_character_,
            error_message = conditionMessage(e)
          )
        }
      )
      
      results[[i]] <- result
    }
    
    current_results <- dplyr::bind_rows(
      results[
        !vapply(
          results,
          is.null,
          logical(1)
        )
      ]
    )
    
    readr::write_csv(
      current_results,
      current_log_path
    )
    
    readr::write_csv(
      current_results,
      batch_log_path
    )
    
    latest_result <- results[[i]]$result[[1]]
    
    cli::cli_progress_update(
      id = progress_id,
      set = i,
      status = paste0(
        title,
        " | ",
        latest_result
      )
    )
  }
  
  cli::cli_progress_done(
    id = progress_id
  )
  
  manifest <- dplyr::bind_rows(
    results
  )
  
  readr::write_csv(
    manifest,
    manifest_path
  )
  
  n_succeeded <- sum(
    manifest$result == "SUCCEEDED",
    na.rm = TRUE
  )
  
  n_existing <- sum(
    manifest$result == "SKIPPED_EXISTING",
    na.rm = TRUE
  )
  
  n_resumed <- sum(
    manifest$result == "SKIPPED_RESUME",
    na.rm = TRUE
  )
  
  n_zero <- sum(
    manifest$result == "ZERO_RECORDS",
    na.rm = TRUE
  )
  
  n_error <- sum(
    manifest$result == "ERROR",
    na.rm = TRUE
  )
  
  total_downloaded_records <- sum(
    manifest$downloaded_records[
      manifest$result %in% c(
        "SUCCEEDED",
        "SKIPPED_EXISTING",
        "SKIPPED_RESUME"
      )
    ],
    na.rm = TRUE
  )
  
  cli::cli_h2(
    "Institutional acquisition batch complete"
  )
  
  cli::cli_alert_success(
    "All {n_total} registered institutional datasets were processed."
  )
  
  cli::cli_text(
    "{.strong Newly downloaded:} {n_succeeded}"
  )
  
  cli::cli_text(
    "{.strong Existing snapshots retained:} {n_existing}"
  )
  
  cli::cli_text(
    "{.strong Resumed/skipped:} {n_resumed}"
  )
  
  cli::cli_text(
    "{.strong Zero JP records:} {n_zero}"
  )
  
  if (n_error > 0L) {
    cli::cli_alert_danger(
      "{n_error} dataset{?s} ended with an error."
    )
  } else {
    cli::cli_alert_success(
      "No dataset errors."
    )
  }
  
  cli::cli_text(
    "{.strong Records represented in successful snapshots:} ",
    "{format_count(total_downloaded_records)}"
  )
  
  cli::cli_text(
    "{.strong Batch manifest:} {manifest_path}"
  )
  
  cli::cli_text(
    "{.strong Recovery log:} {current_log_path}"
  )
  
  invisible(
    manifest
  )
}

# Module summary ---------------------------------------------------------------

cli::cli_h1(
  "VPJD institutional GBIF acquisition module"
)

cli::cli_alert_success(
  "Registry loaded and validated."
)

cli::cli_text(
  "{.strong Registry entries:} {nrow(institution_registry)}"
)

cli::cli_text(
  "{.strong Active READY datasets:} {nrow(active_registry)}"
)

cli::cli_alert_info(
  "No GBIF downloads have been submitted."
)

cli::cli_text(
  "Check one dataset:"
)

cli::cli_code(
  'check_gbif_institution(dataset_key = "dc692432-d85c-46cc-82bc-2fb1ff38decc")'
)

cli::cli_text(
  "Acquire one dataset:"
)

cli::cli_code(
  'acquire_gbif_institution(dataset_key = "dc692432-d85c-46cc-82bc-2fb1ff38decc")'
)

cli::cli_text(
  "Acquire all active READY institutional datasets:"
)

cli::cli_code(
  "acquire_all_gbif_institutions()"
)

cli::cli_text(
  "01_gbif_institution_acquire.R v{VPJD_INSTITUTION_ACQUIRE_VERSION} loaded."
)