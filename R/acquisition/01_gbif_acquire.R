# ==============================================================================
# 01_gbif_acquire.R
# VPJD / Oxford-Japan Plant Conservation Partnership
#
# Stage 1: GBIF occurrence acquisition
#
# Purpose:
#   Discover and acquire vascular-plant occurrence records reported from Japan
#   in GBIF.
#
# Architecture:
#   GBIF -> immutable raw download -> cleaning/QC -> WCVP reconciliation
#
# Scientific rules:
#   1. Acquisition is occurrence-first.
#   2. GEOJAPAN/FOJ do not define the GBIF query taxon universe.
#   3. GBIF taxonomy is used only to delimit vascular plants for acquisition.
#   4. WCVP is the downstream VPJD taxonomic authority.
#   5. No Star ratings are assigned during acquisition.
#   6. No occurrence records are cleaned or deduplicated during acquisition.
#   7. The original GBIF ZIP archive is retained unchanged.
#   8. GBIF download metadata, DOI and query provenance are retained.
#
# Version: 0.4.2
# ==============================================================================

suppressPackageStartupMessages({
  library(rgbif)
  library(readr)
  library(dplyr)
  library(tibble)
  library(here)
})

VPJD_GBIF_ACQUIRE_VERSION <- "0.4.2"

# ------------------------------------------------------------------------------
# Paths
# ------------------------------------------------------------------------------

gbif_raw_dir <- here::here("data", "raw", "GBIF")
gbif_download_dir <- here::here("data", "raw", "GBIF", "downloads")
gbif_metadata_dir <- here::here("data", "raw", "GBIF", "metadata")
gbif_audit_dir <- here::here("outputs", "tables", "GBIF", "acquisition")

for (path in c(
  gbif_raw_dir,
  gbif_download_dir,
  gbif_metadata_dir,
  gbif_audit_dir
)) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
}

# ------------------------------------------------------------------------------
# Historical benchmark
#
# Thesis:
#   4,324,875 raw GBIF occurrence records.
#
# This is retained only as a historical benchmark. Contemporary GBIF holdings
# are expected to differ substantially.
# ------------------------------------------------------------------------------

THESIS_GBIF_RAW_RECORDS <- 4324875L

# ------------------------------------------------------------------------------
# Utility
# ------------------------------------------------------------------------------

`%||%` <- function(x, y) {
  if (
    is.null(x) ||
    length(x) == 0L ||
    all(is.na(x)) ||
    identical(x, "")
  ) {
    y
  } else {
    x
  }
}

# ------------------------------------------------------------------------------
# GBIF credentials
#
# Required in .Renviron:
#
#   GBIF_USER=
#   GBIF_PWD=
#   GBIF_EMAIL=
#
# Credentials must never be written directly into project scripts.
# ------------------------------------------------------------------------------

get_gbif_credentials <- function() {
  credentials <- list(
    user = Sys.getenv("GBIF_USER"),
    pwd = Sys.getenv("GBIF_PWD"),
    email = Sys.getenv("GBIF_EMAIL")
  )
  
  missing_credentials <- names(credentials)[
    vapply(credentials, function(x) !nzchar(x), logical(1))
  ]
  
  if (length(missing_credentials) > 0L) {
    stop(
      "GBIF credentials missing from .Renviron: ",
      paste(missing_credentials, collapse = ", "),
      ". Add them and restart R."
    )
  }
  
  credentials
}

# ------------------------------------------------------------------------------
# Safely extract GBIF download key
# ------------------------------------------------------------------------------

safe_download_key <- function(x) {
  if (is.list(x) && !is.null(x$key)) {
    return(as.character(x$key)[1])
  }
  
  if (is.character(x) && length(x) >= 1L) {
    return(as.character(x)[1])
  }
  
  stop("Could not extract GBIF download key from occ_download() result.")
}

# ------------------------------------------------------------------------------
# Resolve vascular plants in GBIF
#
# Tracheophyta is used only as the broad GBIF acquisition concept.
# WCVP remains the downstream VPJD taxonomic authority.
# ------------------------------------------------------------------------------

resolve_gbif_vascular_plant_key <- function() {
  message("Resolving Tracheophyta against GBIF taxonomy...")
  
  match <- rgbif::name_backbone(
    name = "Tracheophyta",
    kingdom = "Plantae"
  )
  
  if (
    is.null(match$usageKey) ||
    length(match$usageKey) == 0L ||
    is.na(match$usageKey)
  ) {
    stop("GBIF did not return a usable taxon key for Tracheophyta.")
  }
  
  result <- tibble(
    query_name = "Tracheophyta",
    gbif_usage_key = as.integer(match$usageKey),
    gbif_scientific_name = as.character(match$scientificName),
    gbif_rank = as.character(match$rank),
    gbif_status = as.character(match$status),
    gbif_confidence = as.integer(match$confidence)
  )
  
  message(
    "Resolved: ",
    result$gbif_scientific_name,
    " [GBIF key ",
    result$gbif_usage_key,
    "]"
  )
  
  result
}

# ------------------------------------------------------------------------------
# Count occurrences
#
# occ_search(limit = 0) requests no occurrence records. Only GBIF's metadata
# count is returned.
#
# IMPORTANT:
#   occ_search() uses camelCase GBIF parameter names:
#
#   taxonKey
#   hasCoordinate
#   occurrenceStatus
#
# occurrenceStatus defaults to PRESENT in occ_search(), so NULL must be supplied
# explicitly when occurrence status is intended to be unrestricted.
# ------------------------------------------------------------------------------

count_gbif_occurrences <- function(...) {
  result <- rgbif::occ_search(
    ...,
    limit = 0
  )
  
  count <- result$meta$count
  
  if (
    is.null(count) ||
    length(count) == 0L ||
    is.na(count)
  ) {
    stop("GBIF did not return an occurrence count.")
  }
  
  as.numeric(count)
}

# ------------------------------------------------------------------------------
# Audit intended acquisition
#
# Four counts are made:
#
#   1. Vascular plants + Japan + coordinates + PRESENT
#   2. Vascular plants + Japan + PRESENT, coordinates unrestricted
#   3. Vascular plants + Japan + coordinates, occurrence status unrestricted
#   4. All vascular-plant records assigned to Japan
#
# This function does NOT submit a GBIF download.
# ------------------------------------------------------------------------------

audit_gbif_japan_harvest <- function() {
  message("")
  message("==============================================================")
  message("AUDITING GBIF JAPAN VASCULAR-PLANT HARVEST")
  message("==============================================================")
  
  vascular_concept <- resolve_gbif_vascular_plant_key()
  vascular_key <- vascular_concept$gbif_usage_key[[1]]
  
  message("Counting georeferenced PRESENT records...")
  
  production_count <- count_gbif_occurrences(
    taxonKey = vascular_key,
    country = "JP",
    hasCoordinate = TRUE,
    occurrenceStatus = "PRESENT"
  )
  
  message("Counting PRESENT records, coordinates unrestricted...")
  
  present_count <- count_gbif_occurrences(
    taxonKey = vascular_key,
    country = "JP",
    occurrenceStatus = "PRESENT"
  )
  
  message("Counting georeferenced records, occurrence status unrestricted...")
  
  coordinate_count <- count_gbif_occurrences(
    taxonKey = vascular_key,
    country = "JP",
    hasCoordinate = TRUE,
    occurrenceStatus = NULL
  )
  
  message("Counting all Japan vascular-plant records...")
  
  unrestricted_count <- count_gbif_occurrences(
    taxonKey = vascular_key,
    country = "JP",
    occurrenceStatus = NULL
  )
  
  counts <- tibble(
    query = c(
      "production_candidate",
      "present_any_georeferencing",
      "georeferenced_any_occurrence_status",
      "all_japan_vascular_records"
    ),
    country = "JP",
    taxon_key = vascular_key,
    has_coordinate = c(
      TRUE,
      NA,
      TRUE,
      NA
    ),
    occurrence_status = c(
      "PRESENT",
      "PRESENT",
      NA,
      NA
    ),
    occurrence_records = c(
      production_count,
      present_count,
      coordinate_count,
      unrestricted_count
    )
  )
  
  comparison <- tibble(
    metric = c(
      "thesis_GBIF_raw_records",
      "current_production_candidate",
      "absolute_difference",
      "current_percentage_of_thesis"
    ),
    value = c(
      THESIS_GBIF_RAW_RECORDS,
      production_count,
      production_count - THESIS_GBIF_RAW_RECORDS,
      100 * production_count / THESIS_GBIF_RAW_RECORDS
    )
  )
  
  readr::write_csv(
    vascular_concept,
    file.path(
      gbif_audit_dir,
      "gbif_vascular_plant_concept.csv"
    )
  )
  
  readr::write_csv(
    counts,
    file.path(
      gbif_audit_dir,
      "gbif_japan_acquisition_counts.csv"
    )
  )
  
  readr::write_csv(
    comparison,
    file.path(
      gbif_audit_dir,
      "gbif_japan_thesis_comparison.csv"
    )
  )
  
  message("")
  message(
    "Candidate production records: ",
    format(
      production_count,
      big.mark = ",",
      scientific = FALSE
    )
  )
  
  message(
    "Historical thesis benchmark: ",
    format(
      THESIS_GBIF_RAW_RECORDS,
      big.mark = ",",
      scientific = FALSE
    )
  )
  
  message("Audit complete. No download submitted.")
  
  invisible(
    list(
      vascular_concept = vascular_concept,
      acquisition_counts = counts,
      thesis_comparison = comparison
    )
  )
}

# ------------------------------------------------------------------------------
# Build production download predicate
#
# Default candidate:
#
#   taxonKey         = Tracheophyta
#   country          = JP
#   hasCoordinate    = TRUE
#   occurrenceStatus = PRESENT
#
# These remain configurable until the audit has established which restrictions
# should be applied to the production harvest.
#
# We deliberately do NOT filter at acquisition for:
#
#   coordinate uncertainty
#   GBIF geospatial issues
#   basisOfRecord
#   establishmentMeans
#   cultivated records
#   dates
#   dataset/publisher
#   duplicate records
#   GBIF taxonomic status
#
# Those belong to downstream QC/reconciliation.
# ------------------------------------------------------------------------------

build_gbif_japan_predicate <- function(
    vascular_key = NULL,
    require_coordinates = TRUE,
    require_present = TRUE) {
  
  if (is.null(vascular_key)) {
    vascular_key <- resolve_gbif_vascular_plant_key()$
      gbif_usage_key[[1]]
  }
  
  predicates <- list(
    rgbif::pred(
      "taxonKey",
      vascular_key
    ),
    rgbif::pred(
      "country",
      "JP"
    )
  )
  
  if (isTRUE(require_coordinates)) {
    predicates <- append(
      predicates,
      list(
        rgbif::pred(
          "hasCoordinate",
          TRUE
        )
      )
    )
  }
  
  if (isTRUE(require_present)) {
    predicates <- append(
      predicates,
      list(
        rgbif::pred(
          "occurrenceStatus",
          "PRESENT"
        )
      )
    )
  }
  
  do.call(
    rgbif::pred_and,
    predicates
  )
}

# ------------------------------------------------------------------------------
# Save GBIF metadata and DOI
# ------------------------------------------------------------------------------

save_gbif_download_metadata <- function(
    download_key,
    vascular_concept = NULL) {
  
  if (is.null(vascular_concept)) {
    vascular_concept <- resolve_gbif_vascular_plant_key()
  }
  
  metadata <- rgbif::occ_download_meta(
    download_key
  )
  
  metadata_rds <- file.path(
    gbif_metadata_dir,
    paste0(
      "gbif_metadata_",
      download_key,
      ".rds"
    )
  )
  
  saveRDS(
    metadata,
    metadata_rds
  )
  
  metadata_txt <- file.path(
    gbif_metadata_dir,
    paste0(
      "gbif_download_",
      download_key,
      ".txt"
    )
  )
  
  doi <- metadata$doi %||% NA_character_
  status <- metadata$status %||% NA_character_
  
  record_count <-
    metadata$totalRecords %||%
    metadata$total_records %||%
    NA
  
  lines <- c(
    paste(
      "VPJD GBIF acquisition module:",
      VPJD_GBIF_ACQUIRE_VERSION
    ),
    paste(
      "Metadata recorded:",
      format(
        Sys.time(),
        "%Y-%m-%d %H:%M:%S %Z"
      )
    ),
    paste(
      "Download key:",
      download_key
    ),
    paste(
      "DOI:",
      doi
    ),
    paste(
      "Status:",
      status
    ),
    paste(
      "Reported records:",
      record_count
    ),
    paste(
      "Country:",
      "JP"
    ),
    paste(
      "Taxonomic scope:",
      vascular_concept$gbif_scientific_name[[1]]
    ),
    paste(
      "GBIF taxon key:",
      vascular_concept$gbif_usage_key[[1]]
    ),
    paste(
      "Download format:",
      "SIMPLE_CSV"
    ),
    paste(
      "VPJD taxonomic authority:",
      "WCVP downstream"
    )
  )
  
  writeLines(
    lines,
    metadata_txt
  )
  
  message(
    "Saved GBIF metadata: ",
    metadata_rds
  )
  
  message(
    "Saved GBIF provenance: ",
    metadata_txt
  )
  
  invisible(
    list(
      metadata = metadata,
      metadata_rds = metadata_rds,
      metadata_txt = metadata_txt
    )
  )
}

# ------------------------------------------------------------------------------
# Submit production GBIF download
#
# IMPORTANT:
#   Calling this function creates a real GBIF download request.
#
# It submits the request and records the download key.
# It does NOT automatically wait for, retrieve, unpack or clean the data.
# ------------------------------------------------------------------------------

submit_gbif_japan_download <- function(
    require_coordinates = TRUE,
    require_present = TRUE) {
  
  credentials <- get_gbif_credentials()
  
  vascular_concept <- resolve_gbif_vascular_plant_key()
  vascular_key <- vascular_concept$gbif_usage_key[[1]]
  
  predicate <- build_gbif_japan_predicate(
    vascular_key = vascular_key,
    require_coordinates = require_coordinates,
    require_present = require_present
  )
  
  message("")
  message("==============================================================")
  message("SUBMITTING PRODUCTION GBIF DOWNLOAD")
  message("==============================================================")
  
  message(
    "Taxon: ",
    vascular_concept$gbif_scientific_name[[1]]
  )
  
  message(
    "GBIF taxon key: ",
    vascular_key
  )
  
  message(
    "Country: JP"
  )
  
  message(
    "Coordinates required: ",
    require_coordinates
  )
  
  message(
    "Occurrence status PRESENT required: ",
    require_present
  )
  
  message(
    "Format: SIMPLE_CSV"
  )
  
  result <- rgbif::occ_download(
    predicate,
    format = "SIMPLE_CSV",
    user = credentials$user,
    pwd = credentials$pwd,
    email = credentials$email
  )
  
  download_key <- safe_download_key(
    result
  )
  
  submission <- tibble(
    module_version =
      VPJD_GBIF_ACQUIRE_VERSION,
    submitted_at =
      format(
        Sys.time(),
        "%Y-%m-%d %H:%M:%S %Z"
      ),
    gbif_download_key =
      download_key,
    country =
      "JP",
    vascular_taxon_key =
      vascular_key,
    vascular_taxon_name =
      vascular_concept$gbif_scientific_name[[1]],
    has_coordinate =
      require_coordinates,
    occurrence_status_present =
      require_present,
    download_format =
      "SIMPLE_CSV"
  )
  
  submission_file <- file.path(
    gbif_metadata_dir,
    paste0(
      "gbif_submission_",
      download_key,
      ".csv"
    )
  )
  
  readr::write_csv(
    submission,
    submission_file
  )
  
  message("")
  message(
    "GBIF download submitted successfully."
  )
  
  message(
    "Download key: ",
    download_key
  )
  
  message(
    "Submission record: ",
    submission_file
  )
  
  invisible(
    download_key
  )
}

# ------------------------------------------------------------------------------
# Check download status
# ------------------------------------------------------------------------------

check_gbif_download <- function(
    download_key
) {
  
  if (
    missing(download_key) ||
    is.null(download_key) ||
    !nzchar(download_key)
  ) {
    stop(
      "A GBIF download key must be supplied."
    )
  }
  
  metadata <- rgbif::occ_download_meta(
    download_key
  )
  
  print(
    metadata
  )
  
  invisible(
    metadata
  )
}

# ------------------------------------------------------------------------------
# Wait for GBIF
#
# Does not retrieve, unzip or import occurrence data.
# ------------------------------------------------------------------------------

wait_for_gbif_download <- function(
    download_key,
    status_ping = 30) {
  
  if (
    missing(download_key) ||
    is.null(download_key) ||
    !nzchar(download_key)
  ) {
    stop(
      "A GBIF download key must be supplied."
    )
  }
  
  message(
    "Waiting for GBIF download: ",
    download_key
  )
  
  rgbif::occ_download_wait(
    download_key,
    status_ping = status_ping
  )
  
  metadata <- rgbif::occ_download_meta(
    download_key
  )
  
  save_gbif_download_metadata(
    download_key
  )
  
  invisible(
    metadata
  )
}

# ------------------------------------------------------------------------------
# Retrieve completed immutable raw archive
#
# The ZIP is retained as the immutable raw GBIF source.
#
# It is NOT:
#   unzipped
#   imported
#   cleaned
#   taxonomically reconciled
# ------------------------------------------------------------------------------

retrieve_gbif_download <- function(
    download_key,
    overwrite = FALSE) {
  
  if (
    missing(download_key) ||
    is.null(download_key) ||
    !nzchar(download_key)
  ) {
    stop(
      "A GBIF download key must be supplied."
    )
  }
  
  metadata <- rgbif::occ_download_meta(
    download_key
  )
  
  status <- as.character(
    metadata$status %||% NA_character_
  )[1]
  
  message(
    "GBIF download status: ",
    status
  )
  
  if (!identical(status, "SUCCEEDED")) {
    stop(
      "GBIF download is not ready. Current status: ",
      status
    )
  }
  
  message(
    "Retrieving immutable raw GBIF archive..."
  )
  
  downloaded <- rgbif::occ_download_get(
    download_key,
    path = gbif_download_dir,
    overwrite = overwrite
  )
  
  save_gbif_download_metadata(
    download_key
  )
  
  message("")
  message(
    "Raw GBIF archive retrieved."
  )
  
  message(
    "Raw directory: ",
    gbif_download_dir
  )
  
  message(
    "The archive has NOT been modified or unpacked."
  )
  
  invisible(
    downloaded
  )
}

# ------------------------------------------------------------------------------
# Convenience function: wait + retrieve
# ------------------------------------------------------------------------------

wait_and_retrieve_gbif_download <- function(
    download_key,
    status_ping = 30,
    overwrite = FALSE) {
  
  wait_for_gbif_download(
    download_key = download_key,
    status_ping = status_ping
  )
  
  retrieve_gbif_download(
    download_key = download_key,
    overwrite = overwrite
  )
}

# ------------------------------------------------------------------------------
# Audit completed GBIF download
# ------------------------------------------------------------------------------

audit_completed_gbif_download <- function(
    download_key
) {
  
  metadata <- rgbif::occ_download_meta(
    download_key
  )
  
  vascular_concept <- resolve_gbif_vascular_plant_key()
  
  record_count <-
    metadata$totalRecords %||%
    metadata$total_records %||%
    NA
  
  audit <- tibble(
    module_version =
      VPJD_GBIF_ACQUIRE_VERSION,
    gbif_download_key =
      download_key,
    doi =
      as.character(
        metadata$doi %||% NA_character_
      ),
    status =
      as.character(
        metadata$status %||% NA_character_
      ),
    reported_records =
      as.numeric(
        record_count
      ),
    thesis_GBIF_records =
      THESIS_GBIF_RAW_RECORDS,
    difference_from_thesis =
      as.numeric(record_count) -
      THESIS_GBIF_RAW_RECORDS,
    country =
      "JP",
    taxon =
      vascular_concept$gbif_scientific_name[[1]],
    taxon_key =
      vascular_concept$gbif_usage_key[[1]],
    format =
      "SIMPLE_CSV"
  )
  
  audit_file <- file.path(
    gbif_audit_dir,
    paste0(
      "gbif_completed_download_audit_",
      download_key,
      ".csv"
    )
  )
  
  readr::write_csv(
    audit,
    audit_file
  )
  
  message(
    "Completed-download audit written: ",
    audit_file
  )
  
  invisible(
    audit
  )
}

# ------------------------------------------------------------------------------
# Module status
# ------------------------------------------------------------------------------

message("")

message(
  "01_gbif_acquire.R v",
  VPJD_GBIF_ACQUIRE_VERSION,
  " loaded."
)

message(
  "Occurrence-first GBIF acquisition module ready."
)

message(
  "No GBIF download has been submitted automatically."
)