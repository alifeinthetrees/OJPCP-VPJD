# ==============================================================================
# GBIF_Herbarium_of_Shinshu_University.R
# VPJD / Oxford-Japan Plant Conservation Partnership
# Institutional GBIF acquisition module
#
# Institution/dataset:
#   Herbarium of Shinshu University
#
# GBIF datasetKey:
#   dc692432-d85c-46cc-82bc-2fb1ff38decc
#
# Purpose:
#   Submit a reproducible GBIF download request for Japanese occurrence records
#   published through this dataset.
#
# Design:
# - Dataset identity is fixed by persistent GBIF datasetKey.
# - Japan filter is applied at GBIF download stage.
# - Raw GBIF download is retained unchanged.
# - Each execution creates a new GBIF download with its own DOI/download key.
# - Submission and completion metadata are retained for audit/repeat harvesting.
# - No occurrence cleaning occurs here.
# - No WCVP reconciliation occurs here.
# - No FOJ Star matching occurs here.
# - No binomial truncation occurs here.
#
# This module is intended to be rerun periodically as the publisher updates
# records in GBIF.
#
# Version: 0.1.1
# ==============================================================================

VPJD_INSTITUTION_GBIF_VERSION <- "0.1.1"

# Packages ---------------------------------------------------------------------

required_packages <- c("here", "rgbif", "readr")

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0L) {
  stop(
    "Required package(s) not installed in project renv: ",
    paste(missing_packages, collapse = ", "),
    "\nInstall with:\nrenv::install(c(",
    paste(sprintf('"%s"', missing_packages), collapse = ", "),
    "))"
  )
}

suppressPackageStartupMessages({
  library(here)
  library(rgbif)
  library(readr)
})

# Dataset configuration --------------------------------------------------------

dataset_key <- "dc692432-d85c-46cc-82bc-2fb1ff38decc"
dataset_title <- "Herbarium of Shinshu University"
country_code <- "JP"
download_format <- "SIMPLE_CSV"

# Paths ------------------------------------------------------------------------

download_dir <- here::here(
  "data", "raw", "GBIF", "institutions",
  "Herbarium_of_Shinshu_University", "downloads"
)

metadata_dir <- here::here(
  "data", "raw", "GBIF", "institutions",
  "Herbarium_of_Shinshu_University", "metadata"
)

dir.create(
  download_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  metadata_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

# GBIF credentials -------------------------------------------------------------

gbif_user <- Sys.getenv("GBIF_USER")
gbif_pwd <- Sys.getenv("GBIF_PWD")
gbif_email <- Sys.getenv("GBIF_EMAIL")

if (
  gbif_user == "" ||
  gbif_pwd == "" ||
  gbif_email == ""
) {
  stop(
    "GBIF credentials not set. ",
    "Check .Renviron and restart R."
  )
}

# Current GBIF dataset status --------------------------------------------------

message("")
message("==============================================================")
message("VPJD INSTITUTIONAL GBIF ACQUISITION")
message("==============================================================")
message("Module version: ", VPJD_INSTITUTION_GBIF_VERSION)
message("Dataset: ", dataset_title)
message("Dataset key: ", dataset_key)
message("Country filter: ", country_code)
message("")

message("Checking current GBIF record counts...")

dataset_all <- rgbif::occ_count(
  datasetKey = dataset_key
)

dataset_japan <- rgbif::occ_count(
  datasetKey = dataset_key,
  country = country_code
)

message(
  "All records currently reported by GBIF: ",
  format(
    dataset_all,
    big.mark = ",",
    scientific = FALSE
  )
)

message(
  "Japanese records currently reported by GBIF: ",
  format(
    dataset_japan,
    big.mark = ",",
    scientific = FALSE
  )
)

if (dataset_japan == 0) {
  stop(
    "GBIF currently reports no Japanese records ",
    "for this dataset."
  )
}

# Submit GBIF download ---------------------------------------------------------

message("")
message("Submitting GBIF download request...")

download_request <- rgbif::occ_download(
  rgbif::pred("datasetKey", dataset_key),
  rgbif::pred("country", country_code),
  format = download_format,
  user = gbif_user,
  pwd = gbif_pwd,
  email = gbif_email
)

download_key <- as.character(download_request)

message("GBIF download key: ", download_key)

# Submission metadata ---------------------------------------------------------

submission_metadata <- data.frame(
  module_version = VPJD_INSTITUTION_GBIF_VERSION,
  dataset_title = dataset_title,
  dataset_key = dataset_key,
  country = country_code,
  download_format = download_format,
  gbif_records_all_at_submission = dataset_all,
  gbif_records_japan_at_submission = dataset_japan,
  download_key = download_key,
  submitted_utc = format(
    Sys.time(),
    tz = "UTC",
    format = "%Y-%m-%dT%H:%M:%SZ"
  ),
  stringsAsFactors = FALSE
)

submission_path <- file.path(
  metadata_dir,
  paste0(
    "gbif_submission_",
    download_key,
    ".csv"
  )
)

readr::write_csv(
  submission_metadata,
  submission_path
)

message(
  "Submission metadata: ",
  submission_path
)

# Wait for GBIF ---------------------------------------------------------------

message("")
message("Waiting for GBIF to prepare download...")

rgbif::occ_download_wait(
  download_request,
  status_ping = 30
)

# Retrieve completed download -------------------------------------------------

message("")
message("Retrieving GBIF download...")

download_result <- rgbif::occ_download_get(
  download_request,
  path = download_dir,
  overwrite = FALSE
)

# rgbif returns the downloaded archive path as an atomic character value.
zip_path <- as.character(download_result)

if (
  length(zip_path) != 1L ||
  !file.exists(zip_path)
) {
  stop(
    "GBIF download completed but ZIP archive ",
    "could not be confirmed."
  )
}

message("Downloaded archive: ", zip_path)

# Retrieve GBIF download metadata ---------------------------------------------

download_metadata <- rgbif::occ_download_meta(
  download_key
)

download_doi <- if (
  !is.null(download_metadata$doi)
) {
  as.character(download_metadata$doi)
} else {
  NA_character_
}

download_status <- if (
  !is.null(download_metadata$status)
) {
  as.character(download_metadata$status)
} else {
  NA_character_
}

download_total_records <- if (
  !is.null(download_metadata$totalRecords)
) {
  as.numeric(download_metadata$totalRecords)
} else {
  NA_real_
}

# Completion metadata ---------------------------------------------------------

completion_metadata <- data.frame(
  module_version = VPJD_INSTITUTION_GBIF_VERSION,
  dataset_title = dataset_title,
  dataset_key = dataset_key,
  country = country_code,
  download_format = download_format,
  download_key = download_key,
  doi = download_doi,
  status = download_status,
  total_records = download_total_records,
  zip_path = normalizePath(
    zip_path,
    winslash = "/",
    mustWork = TRUE
  ),
  zip_size_bytes = file.info(zip_path)$size,
  retrieved_utc = format(
    Sys.time(),
    tz = "UTC",
    format = "%Y-%m-%dT%H:%M:%SZ"
  ),
  stringsAsFactors = FALSE
)

completion_path <- file.path(
  metadata_dir,
  paste0(
    "gbif_download_",
    download_key,
    ".csv"
  )
)

readr::write_csv(
  completion_metadata,
  completion_path
)

# Final report ----------------------------------------------------------------

message("")
message("==============================================================")
message("INSTITUTIONAL GBIF ACQUISITION COMPLETE")
message("==============================================================")
message("Dataset: ", dataset_title)
message("Dataset key: ", dataset_key)
message("Country: ", country_code)
message("Download key: ", download_key)

if (!is.na(download_doi)) {
  message("DOI: ", download_doi)
}

if (!is.na(download_total_records)) {
  message(
    "Downloaded records: ",
    format(
      download_total_records,
      big.mark = ",",
      scientific = FALSE
    )
  )
}

message("Raw archive: ", zip_path)
message("Submission metadata: ", submission_path)
message("Completion metadata: ", completion_path)
message("")
message("Raw download has not been modified.")
message("No WCVP reconciliation has been performed.")
message("No Star ratings have been assigned.")
message("")
message(
  "GBIF_Herbarium_of_Shinshu_University.R v",
  VPJD_INSTITUTION_GBIF_VERSION,
  " complete."
)
