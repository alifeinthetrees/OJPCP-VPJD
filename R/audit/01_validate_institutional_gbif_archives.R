# ==============================================================================
# 01_validate_institutional_gbif_archives.R
# VPJD / Oxford-Japan Plant Conservation Partnership
#
# Purpose:
#   Validate and inventory the completed institutional GBIF acquisition.
#
# Outputs:
#   1. Institutional dataset manifest
#   2. Archive-level inventory
#   3. Multiple-snapshot audit
#   4. Validation summary
#
# IMPORTANT:
# - READ-ONLY with respect to raw GBIF archives.
# - Does not extract, modify, rename, delete or download raw data.
# - Does not clean or merge occurrence records.
# - Does not perform WCVP reconciliation.
#
# Version: 0.1.0
# ==============================================================================

VPJD_ARCHIVE_AUDIT_VERSION <- "0.1.0"

# Packages ---------------------------------------------------------------------

required_packages <- c(
  "here", "readr", "dplyr", "stringr", "tibble", "purrr", "cli"
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
  library(readr)
  library(dplyr)
  library(stringr)
  library(tibble)
  library(purrr)
  library(cli)
})

# Paths ------------------------------------------------------------------------

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

output_dir <- here::here(
  "outputs", "tables", "GBIF",
  "institutional_archive_validation"
)

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

# Helpers ----------------------------------------------------------------------

sanitize_directory_name <- function(x) {
  x <- stringr::str_squish(x)
  x <- stringr::str_replace_all(
    x,
    "[^A-Za-z0-9]+",
    "_"
  )
  x <- stringr::str_replace_all(
    x,
    "_+",
    "_"
  )
  x <- stringr::str_replace_all(
    x,
    "^_|_$",
    ""
  )
  x
}

format_count <- function(x) {
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
  
  units <- c(
    "B", "KB", "MB", "GB", "TB"
  )
  
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

safe_read_csv <- function(path) {
  tryCatch(
    readr::read_csv(
      path,
      show_col_types = FALSE
    ),
    error = function(e) NULL
  )
}

extract_download_key <- function(path) {
  filename <- basename(path)
  
  stringr::str_remove(
    filename,
    "\\.zip$"
  )
}

# Load registry ----------------------------------------------------------------

if (!file.exists(registry_path)) {
  stop(
    "Institution registry not found:\n",
    registry_path
  )
}

registry <- readr::read_csv(
  registry_path,
  show_col_types = FALSE
)

required_columns <- c(
  "registry_id",
  "dataset_title",
  "dataset_key",
  "active",
  "registry_status"
)

missing_columns <- setdiff(
  required_columns,
  names(registry)
)

if (length(missing_columns) > 0L) {
  stop(
    "Registry missing required columns: ",
    paste(
      missing_columns,
      collapse = ", "
    )
  )
}

registry <- registry |>
  dplyr::filter(
    .data$active,
    .data$registry_status == "READY"
  )

cli::cli_h1(
  "VPJD institutional GBIF archive validation"
)

cli::cli_text(
  "{.strong Registry datasets:} {nrow(registry)}"
)

# Read latest batch manifest ----------------------------------------------------

batch_files <- list.files(
  batch_metadata_dir,
  pattern = "^institution_acquisition_manifest_.*\\.csv$",
  full.names = TRUE
)

if (length(batch_files) == 0L) {
  stop(
    "No completed institutional acquisition manifest found."
  )
}

batch_info <- file.info(
  batch_files
)

latest_batch_path <- rownames(
  batch_info
)[
  which.max(
    batch_info$mtime
  )
]

batch_manifest <- readr::read_csv(
  latest_batch_path,
  show_col_types = FALSE
)

cli::cli_text(
  "{.strong Batch manifest:} {basename(latest_batch_path)}"
)

cli::cli_text(
  "{.strong Batch rows:} {nrow(batch_manifest)}"
)

# Discover archives ------------------------------------------------------------

archive_inventory <- list()

progress_id <- cli::cli_progress_bar(
  "Validating institutional archives",
  total = nrow(registry),
  format = paste0(
    "{cli::pb_bar} {cli::pb_percent} | ",
    "{cli::pb_current}/{cli::pb_total} | ",
    "{cli::pb_status} | {cli::pb_elapsed}"
  ),
  clear = FALSE
)

for (i in seq_len(nrow(registry))) {
  
  row <- registry[
    i,
    ,
    drop = FALSE
  ]
  
  title <- row$dataset_title[[1]]
  dataset_key <- row$dataset_key[[1]]
  
  cli::cli_progress_update(
    id = progress_id,
    set = i - 1L,
    status = title
  )
  
  institution_slug <- sanitize_directory_name(
    title
  )
  
  institution_dir <- file.path(
    institution_root,
    institution_slug
  )
  
  download_dir <- file.path(
    institution_dir,
    "downloads"
  )
  
  metadata_dir <- file.path(
    institution_dir,
    "metadata"
  )
  
  zip_files <- if (
    dir.exists(download_dir)
  ) {
    list.files(
      download_dir,
      pattern = "\\.zip$",
      full.names = TRUE,
      ignore.case = TRUE
    )
  } else {
    character()
  }
  
  if (length(zip_files) == 0L) {
    
    archive_inventory[[length(archive_inventory) + 1L]] <-
      tibble::tibble(
        registry_id = row$registry_id[[1]],
        dataset_title = title,
        dataset_key = dataset_key,
        institution_directory = institution_dir,
        archive_path = NA_character_,
        archive_filename = NA_character_,
        download_key = NA_character_,
        archive_bytes = NA_real_,
        archive_modified = NA_character_,
        zip_valid = NA,
        files_inside_zip = NA_integer_,
        occurrence_file = NA_character_,
        occurrence_file_bytes = NA_real_,
        expected_records = NA_real_,
        doi = NA_character_,
        gbif_status = NA_character_,
        completion_metadata_found = FALSE
      )
    
    cli::cli_progress_update(
      id = progress_id,
      set = i
    )
    
    next
  }
  
  for (zip_path in zip_files) {
    
    download_key <- extract_download_key(
      zip_path
    )
    
    zip_details <- tryCatch(
      utils::unzip(
        zip_path,
        list = TRUE
      ),
      error = function(e) NULL
    )
    
    zip_valid <- !is.null(
      zip_details
    )
    
    files_inside_zip <- if (
      zip_valid
    ) {
      nrow(
        zip_details
      )
    } else {
      NA_integer_
    }
    
    occurrence_file <- NA_character_
    occurrence_file_bytes <- NA_real_
    
    if (
      zip_valid &&
      nrow(zip_details) > 0L
    ) {
      candidate_files <- zip_details |>
        dplyr::filter(
          stringr::str_detect(
            .data$Name,
            "\\.(csv|txt)$"
          )
        ) |>
        dplyr::arrange(
          dplyr::desc(
            .data$Length
          )
        )
      
      if (
        nrow(candidate_files) > 0L
      ) {
        occurrence_file <-
          candidate_files$Name[[1]]
        
        occurrence_file_bytes <-
          candidate_files$Length[[1]]
      }
    }
    
    completion_metadata_path <- file.path(
      metadata_dir,
      paste0(
        "gbif_download_",
        download_key,
        ".csv"
      )
    )
    
    completion_metadata_found <- file.exists(
      completion_metadata_path
    )
    
    completion_metadata <- if (
      completion_metadata_found
    ) {
      safe_read_csv(
        completion_metadata_path
      )
    } else {
      NULL
    }
    
    expected_records <- NA_real_
    doi <- NA_character_
    gbif_status <- NA_character_
    
    if (
      !is.null(completion_metadata) &&
      nrow(completion_metadata) > 0L
    ) {
      if (
        "downloaded_records" %in%
        names(completion_metadata)
      ) {
        expected_records <- suppressWarnings(
          as.numeric(
            completion_metadata$
              downloaded_records[[1]]
          )
        )
      }
      
      if (
        "doi" %in%
        names(completion_metadata)
      ) {
        doi <- as.character(
          completion_metadata$
            doi[[1]]
        )
      }
      
      if (
        "status" %in%
        names(completion_metadata)
      ) {
        gbif_status <- as.character(
          completion_metadata$
            status[[1]]
        )
      }
    }
    
    archive_inventory[[length(archive_inventory) + 1L]] <-
      tibble::tibble(
        registry_id = row$registry_id[[1]],
        dataset_title = title,
        dataset_key = dataset_key,
        institution_directory = institution_dir,
        archive_path = normalizePath(
          zip_path,
          winslash = "/",
          mustWork = FALSE
        ),
        archive_filename = basename(
          zip_path
        ),
        download_key = download_key,
        archive_bytes = file.info(
          zip_path
        )$size,
        archive_modified = format(
          file.info(zip_path)$mtime,
          "%Y-%m-%d %H:%M:%S"
        ),
        zip_valid = zip_valid,
        files_inside_zip =
          files_inside_zip,
        occurrence_file =
          occurrence_file,
        occurrence_file_bytes =
          occurrence_file_bytes,
        expected_records =
          expected_records,
        doi = doi,
        gbif_status =
          gbif_status,
        completion_metadata_found =
          completion_metadata_found
      )
  }
  
  cli::cli_progress_update(
    id = progress_id,
    set = i
  )
}

cli::cli_progress_done(
  id = progress_id
)

archive_inventory <- dplyr::bind_rows(
  archive_inventory
)

# Dataset-level summary ---------------------------------------------------------

dataset_manifest <- archive_inventory |>
  dplyr::group_by(
    .data$registry_id,
    .data$dataset_title,
    .data$dataset_key
  ) |>
  dplyr::summarise(
    archive_count = sum(
      !is.na(.data$archive_path)
    ),
    valid_archive_count = sum(
      .data$zip_valid %in% TRUE,
      na.rm = TRUE
    ),
    metadata_count = sum(
      .data$completion_metadata_found,
      na.rm = TRUE
    ),
    latest_archive_modified = if (
      all(
        is.na(
          .data$archive_modified
        )
      )
    ) {
      NA_character_
    } else {
      max(
        .data$archive_modified,
        na.rm = TRUE
      )
    },
    .groups = "drop"
  )

# Add batch outcome ------------------------------------------------------------

batch_outcome <- batch_manifest |>
  dplyr::select(
    .data$dataset_key,
    batch_result = .data$result,
    batch_download_key =
      .data$download_key,
    batch_doi =
      .data$doi,
    batch_records =
      .data$downloaded_records
  )

dataset_manifest <- dataset_manifest |>
  dplyr::left_join(
    batch_outcome,
    by = "dataset_key"
  ) |>
  dplyr::mutate(
    validation_status = dplyr::case_when(
      .data$batch_result ==
        "ZERO_RECORDS" &
        .data$archive_count == 0L ~
        "VALID_ZERO_RECORDS",
      
      .data$batch_result %in%
        c(
          "SUCCEEDED",
          "SKIPPED_EXISTING",
          "SKIPPED_RESUME"
        ) &
        .data$archive_count >= 1L &
        .data$valid_archive_count >= 1L ~
        "VALID_ARCHIVE",
      
      .data$archive_count > 1L &
        .data$valid_archive_count >= 1L ~
        "MULTIPLE_SNAPSHOTS",
      
      .data$archive_count == 0L &
        .data$batch_result !=
        "ZERO_RECORDS" ~
        "MISSING_ARCHIVE",
      
      .data$archive_count > 0L &
        .data$valid_archive_count == 0L ~
        "INVALID_ARCHIVE",
      
      TRUE ~
        "REVIEW"
    )
  )

# Multiple snapshot audit -------------------------------------------------------

multiple_snapshots <- archive_inventory |>
  dplyr::filter(
    !is.na(
      .data$archive_path
    )
  ) |>
  dplyr::group_by(
    .data$registry_id,
    .data$dataset_title,
    .data$dataset_key
  ) |>
  dplyr::filter(
    dplyr::n() > 1L
  ) |>
  dplyr::ungroup() |>
  dplyr::arrange(
    .data$dataset_title,
    .data$archive_modified
  )

# Validation summary -----------------------------------------------------------

validation_summary <- tibble::tibble(
  metric = c(
    "registry_datasets",
    "batch_manifest_rows",
    "successful_batch_datasets",
    "zero_record_datasets",
    "datasets_with_archives",
    "datasets_without_archives",
    "total_zip_archives",
    "valid_zip_archives",
    "invalid_zip_archives",
    "archives_with_completion_metadata",
    "datasets_with_multiple_snapshots",
    "batch_records_represented",
    "archive_bytes_total"
  ),
  value = c(
    nrow(registry),
    nrow(batch_manifest),
    sum(
      batch_manifest$result ==
        "SUCCEEDED",
      na.rm = TRUE
    ),
    sum(
      batch_manifest$result ==
        "ZERO_RECORDS",
      na.rm = TRUE
    ),
    sum(
      dataset_manifest$
        archive_count > 0L,
      na.rm = TRUE
    ),
    sum(
      dataset_manifest$
        archive_count == 0L,
      na.rm = TRUE
    ),
    sum(
      !is.na(
        archive_inventory$
          archive_path
      )
    ),
    sum(
      archive_inventory$
        zip_valid %in% TRUE,
      na.rm = TRUE
    ),
    sum(
      archive_inventory$
        zip_valid %in% FALSE,
      na.rm = TRUE
    ),
    sum(
      archive_inventory$
        completion_metadata_found,
      na.rm = TRUE
    ),
    dplyr::n_distinct(
      multiple_snapshots$
        dataset_key
    ),
    sum(
      batch_manifest$
        downloaded_records,
      na.rm = TRUE
    ),
    sum(
      archive_inventory$
        archive_bytes,
      na.rm = TRUE
    )
  )
)

# Write outputs ----------------------------------------------------------------

archive_inventory_path <- file.path(
  output_dir,
  "institutional_gbif_archive_inventory.csv"
)

dataset_manifest_path <- file.path(
  output_dir,
  "institutional_gbif_dataset_manifest.csv"
)

multiple_snapshots_path <- file.path(
  output_dir,
  "institutional_gbif_multiple_snapshots.csv"
)

validation_summary_path <- file.path(
  output_dir,
  "institutional_gbif_validation_summary.csv"
)

readr::write_csv(
  archive_inventory,
  archive_inventory_path
)

readr::write_csv(
  dataset_manifest,
  dataset_manifest_path
)

readr::write_csv(
  multiple_snapshots,
  multiple_snapshots_path
)

readr::write_csv(
  validation_summary,
  validation_summary_path
)

# Console report ---------------------------------------------------------------

cli::cli_h2(
  "Institutional GBIF archive validation complete"
)

cli::cli_text(
  "{.strong Registry datasets:} {nrow(registry)}"
)

cli::cli_text(
  "{.strong Successful batch datasets:} ",
  "{sum(batch_manifest$result == 'SUCCEEDED', na.rm = TRUE)}"
)

cli::cli_text(
  "{.strong Zero-record datasets:} ",
  "{sum(batch_manifest$result == 'ZERO_RECORDS', na.rm = TRUE)}"
)

cli::cli_text(
  "{.strong Datasets with ZIP archives:} ",
  "{sum(dataset_manifest$archive_count > 0)}"
)

cli::cli_text(
  "{.strong Total ZIP archives found:} ",
  "{sum(!is.na(archive_inventory$archive_path))}"
)

cli::cli_text(
  "{.strong Valid ZIP archives:} ",
  "{sum(archive_inventory$zip_valid %in% TRUE, na.rm = TRUE)}"
)

cli::cli_text(
  "{.strong Invalid ZIP archives:} ",
  "{sum(archive_inventory$zip_valid %in% FALSE, na.rm = TRUE)}"
)

cli::cli_text(
  "{.strong Datasets with multiple snapshots:} ",
  "{dplyr::n_distinct(multiple_snapshots$dataset_key)}"
)

cli::cli_text(
  "{.strong Batch records represented:} ",
  "{format_count(sum(batch_manifest$downloaded_records, na.rm = TRUE))}"
)

cli::cli_text(
  "{.strong Total archive storage:} ",
  "{format_bytes(sum(archive_inventory$archive_bytes, na.rm = TRUE))}"
)

validation_problem_count <- sum(
  dataset_manifest$validation_status %in%
    c(
      "MISSING_ARCHIVE",
      "INVALID_ARCHIVE",
      "REVIEW"
    ),
  na.rm = TRUE
)

if (validation_problem_count == 0L) {
  cli::cli_alert_success(
    "No missing, invalid or unresolved institutional archives detected."
  )
} else {
  cli::cli_alert_warning(
    "{validation_problem_count} dataset{?s} require validation review."
  )
}

cli::cli_text(
  "{.strong Dataset manifest:} {dataset_manifest_path}"
)

cli::cli_text(
  "{.strong Archive inventory:} {archive_inventory_path}"
)

cli::cli_text(
  "{.strong Multiple snapshots:} {multiple_snapshots_path}"
)

cli::cli_text(
  "{.strong Validation summary:} {validation_summary_path}"
)

cli::cli_text(
  "01_validate_institutional_gbif_archives.R v{VPJD_ARCHIVE_AUDIT_VERSION} complete."
)