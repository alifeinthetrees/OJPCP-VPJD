# ==============================================================================
# 03_clean_occurrences.R
# VPJD / Oxford-Japan Plant Conservation Partnership
# Stage 2: GBIF occurrence import and initial quality control
#
# GBIF download: 0005066-260916113435855
# DOI: 10.15468/dl.hyuu23
# Records: 3,284,719
# Format: SIMPLE_CSV
# Scope: Tracheophyta, country JP, hasCoordinate TRUE,
#        occurrenceStatus unrestricted
#
# Rules:
# - Raw GBIF ZIP is never modified.
# - All 50 original GBIF fields are retained.
# - No WCVP reconciliation or Star assignment occurs here.
# - Taxonomically unresolved records are retained.
# - Duplicate candidates are flagged, not removed.
# - Occurrence status and coordinate uncertainty are flagged, not excluded.
# - Only structurally unusable spatial records are quarantined.
#
# Version: 0.1.2
# ==============================================================================

VPJD_CLEAN_OCCURRENCES_VERSION <- "0.1.2"

# Packages ---------------------------------------------------------------------

required_packages <- c("here", "DBI", "duckdb", "readr", "dplyr", "tidyr")
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
  library(DBI)
  library(duckdb)
  library(readr)
  library(dplyr)
  library(tidyr)
})

# Configuration ----------------------------------------------------------------

gbif_download_key <- "0005066-260916113435855"
gbif_doi <- "10.15468/dl.hyuu23"
expected_records <- 3284719L
current_year <- as.integer(format(Sys.Date(), "%Y"))

raw_zip <- here::here(
  "data", "raw", "GBIF", "downloads",
  paste0(gbif_download_key, ".zip")
)

import_dir <- here::here(
  "data", "interim", "occurrences", "GBIF", gbif_download_key
)

audit_dir <- here::here(
  "outputs", "tables", "GBIF", "cleaning"
)

db_path <- here::here(
  "data", "interim", "occurrences", "vpjd_occurrences.duckdb"
)

dir.create(import_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(audit_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(db_path), recursive = TRUE, showWarnings = FALSE)

# Validate archive -------------------------------------------------------------

if (!file.exists(raw_zip)) {
  stop("Raw GBIF ZIP not found:\n", raw_zip)
}

zip_contents <- utils::unzip(raw_zip, list = TRUE)
expected_csv <- paste0(gbif_download_key, ".csv")

if (!(expected_csv %in% zip_contents$Name)) {
  stop("Expected GBIF file not found inside ZIP: ", expected_csv)
}

message("")
message("==============================================================")
message("VPJD GBIF OCCURRENCE CLEANING")
message("==============================================================")
message("Module version: ", VPJD_CLEAN_OCCURRENCES_VERSION)
message("GBIF download: ", gbif_download_key)
message("GBIF DOI: ", gbif_doi)
message("Expected records: ", format(expected_records, big.mark = ","))
message(
  "Compressed archive: ",
  format(file.info(raw_zip)$size, big.mark = ",", scientific = FALSE),
  " bytes"
)
message(
  "Uncompressed GBIF table: ",
  format(
    zip_contents$Length[zip_contents$Name == expected_csv],
    big.mark = ",",
    scientific = FALSE
  ),
  " bytes"
)

# Extract working copy ---------------------------------------------------------

working_csv <- file.path(import_dir, expected_csv)

if (!file.exists(working_csv)) {
  message("")
  message("Extracting working copy of GBIF table...")
  utils::unzip(
    zipfile = raw_zip,
    files = expected_csv,
    exdir = import_dir,
    overwrite = FALSE
  )
  message("Working copy extracted: ", working_csv)
} else {
  message("")
  message("Working GBIF table already exists: ", working_csv)
}

if (!file.exists(working_csv)) {
  stop("GBIF working table could not be extracted.")
}

# Validate schema --------------------------------------------------------------

message("")
message("Inspecting GBIF schema...")

gbif_header <- readLines(working_csv, n = 1, warn = FALSE)
gbif_columns <- strsplit(gbif_header, "\t", fixed = TRUE)[[1]]

required_columns <- c(
  "gbifID", "datasetKey", "occurrenceID", "kingdom", "phylum", "class",
  "order", "family", "genus", "species", "infraspecificEpithet", "taxonRank",
  "scientificName", "verbatimScientificName", "countryCode", "locality",
  "stateProvince", "occurrenceStatus", "publishingOrgKey", "decimalLatitude",
  "decimalLongitude", "coordinateUncertaintyInMeters", "eventDate", "year",
  "taxonKey", "speciesKey", "basisOfRecord", "institutionCode",
  "collectionCode", "catalogNumber", "recordNumber", "identifiedBy",
  "recordedBy", "establishmentMeans", "issue"
)

missing_columns <- setdiff(required_columns, gbif_columns)

if (length(missing_columns) > 0L) {
  stop(
    "Required GBIF column(s) missing: ",
    paste(missing_columns, collapse = ", ")
  )
}

message("GBIF columns detected: ", length(gbif_columns))

if (length(gbif_columns) != 50L) {
  warning(
    "Expected 50 GBIF fields but detected ",
    length(gbif_columns),
    "."
  )
}

# DuckDB -----------------------------------------------------------------------

message("")
message("Opening DuckDB database: ", db_path)

con <- DBI::dbConnect(
  duckdb::duckdb(),
  dbdir = db_path
)

on.exit(
  DBI::dbDisconnect(con, shutdown = TRUE),
  add = TRUE
)

sql_path <- function(x) {
  gsub(
    "'",
    "''",
    normalizePath(x, winslash = "/", mustWork = TRUE),
    fixed = TRUE
  )
}

csv_sql_path <- sql_path(working_csv)

# Import -----------------------------------------------------------------------

message("")
message("Importing GBIF table into DuckDB...")
message("Literal quotation marks will be preserved as field content.")

DBI::dbExecute(con, "DROP TABLE IF EXISTS gbif_raw")

import_sql <- sprintf(
  "
  CREATE TABLE gbif_raw AS
  SELECT *
  FROM read_csv(
    '%s',
    delim = '\\t',
    header = TRUE,
    all_varchar = TRUE,
    quote = '',
    escape = '',
    nullstr = '',
    ignore_errors = FALSE
  )
  ",
  csv_sql_path
)

DBI::dbExecute(con, import_sql)

raw_count <- DBI::dbGetQuery(
  con,
  "SELECT COUNT(*) AS n FROM gbif_raw"
)$n[[1]]

message(
  "Imported records: ",
  format(raw_count, big.mark = ",", scientific = FALSE)
)

if (as.numeric(raw_count) != as.numeric(expected_records)) {
  stop(
    "Imported record count does not match GBIF metadata. Expected ",
    format(expected_records, big.mark = ","),
    "; imported ",
    format(raw_count, big.mark = ","),
    "."
  )
}

message(
  "Record count validated against GBIF metadata: ",
  format(expected_records, big.mark = ",")
)

# Initial QC -------------------------------------------------------------------

message("")
message("Building occurrence QC table...")

DBI::dbExecute(con, "DROP TABLE IF EXISTS occurrences_qc")

qc_sql <- sprintf(
  "
CREATE TABLE occurrences_qc AS
WITH typed AS (
  SELECT
    row_number() OVER () AS vpjd_row_id,
    *,
    TRY_CAST(NULLIF(decimalLatitude, '') AS DOUBLE)
      AS vpjd_decimalLatitude,
    TRY_CAST(NULLIF(decimalLongitude, '') AS DOUBLE)
      AS vpjd_decimalLongitude,
    TRY_CAST(NULLIF(coordinateUncertaintyInMeters, '') AS DOUBLE)
      AS vpjd_coordinateUncertaintyInMeters,
    TRY_CAST(NULLIF(year, '') AS INTEGER)
      AS vpjd_year
  FROM gbif_raw
),
flags AS (
  SELECT
    *,
    (vpjd_decimalLatitude IS NULL)
      AS qc_missing_latitude,
    (vpjd_decimalLongitude IS NULL)
      AS qc_missing_longitude,
    (
      vpjd_decimalLatitude IS NOT NULL
      AND (vpjd_decimalLatitude < -90 OR vpjd_decimalLatitude > 90)
    ) AS qc_invalid_latitude,
    (
      vpjd_decimalLongitude IS NOT NULL
      AND (vpjd_decimalLongitude < -180 OR vpjd_decimalLongitude > 180)
    ) AS qc_invalid_longitude,
    (
      vpjd_decimalLatitude = 0
      AND vpjd_decimalLongitude = 0
    ) AS qc_zero_zero,
    (
      NULLIF(TRIM(gbifID), '') IS NULL
    ) AS qc_missing_gbifID,
    (
      NULLIF(TRIM(scientificName), '') IS NULL
    ) AS qc_missing_scientificName,
    (
      NULLIF(TRIM(taxonKey), '') IS NULL
    ) AS qc_missing_taxonKey,
    (
      NULLIF(TRIM(speciesKey), '') IS NULL
    ) AS qc_missing_speciesKey,
    (
      NULLIF(TRIM(species), '') IS NULL
    ) AS qc_not_resolved_to_species,
    (
      UPPER(COALESCE(NULLIF(TRIM(countryCode), ''), '')) <> 'JP'
    ) AS qc_country_not_JP,
    (
      UPPER(COALESCE(NULLIF(TRIM(occurrenceStatus), ''), '')) <> 'PRESENT'
    ) AS qc_not_present,
    (
      vpjd_coordinateUncertaintyInMeters IS NOT NULL
      AND vpjd_coordinateUncertaintyInMeters > 1000
    ) AS qc_uncertainty_gt_1km,
    (
      vpjd_coordinateUncertaintyInMeters IS NOT NULL
      AND vpjd_coordinateUncertaintyInMeters > 10000
    ) AS qc_uncertainty_gt_10km,
    (
      vpjd_coordinateUncertaintyInMeters IS NOT NULL
      AND vpjd_coordinateUncertaintyInMeters > 100000
    ) AS qc_uncertainty_gt_100km,
    (
      vpjd_year IS NOT NULL
      AND (vpjd_year < 1600 OR vpjd_year > %d)
    ) AS qc_year_out_of_range,
    (
      UPPER(COALESCE(issue, '')) LIKE '%%ZERO_COORDINATE%%'
    ) AS qc_gbif_zero_coordinate_issue,
    (
      UPPER(COALESCE(issue, '')) LIKE '%%COUNTRY_COORDINATE_MISMATCH%%'
    ) AS qc_gbif_country_coordinate_mismatch,
    (
      UPPER(COALESCE(issue, '')) LIKE '%%COORDINATE_INVALID%%'
    ) AS qc_gbif_coordinate_invalid,
    (
      UPPER(COALESCE(issue, '')) LIKE '%%COORDINATE_OUT_OF_RANGE%%'
    ) AS qc_gbif_coordinate_out_of_range
  FROM typed
)
SELECT *
FROM flags
",
current_year
)

DBI::dbExecute(con, qc_sql)

qc_count <- DBI::dbGetQuery(
  con,
  "SELECT COUNT(*) AS n FROM occurrences_qc"
)$n[[1]]

if (as.numeric(qc_count) != as.numeric(raw_count)) {
  stop(
    "QC table record count differs from raw import: ",
    format(qc_count, big.mark = ","),
    " vs ",
    format(raw_count, big.mark = ","),
    "."
  )
}

message(
  "QC table records validated: ",
  format(qc_count, big.mark = ",")
)

# Duplicate diagnostics --------------------------------------------------------

message("Building duplicate diagnostics...")

DBI::dbExecute(con, "DROP TABLE IF EXISTS duplicate_gbifID")
DBI::dbExecute(
  con,
  "
  CREATE TABLE duplicate_gbifID AS
  SELECT gbifID, COUNT(*) AS n_records
  FROM occurrences_qc
  WHERE NULLIF(TRIM(gbifID), '') IS NOT NULL
  GROUP BY gbifID
  HAVING COUNT(*) > 1
  "
)

DBI::dbExecute(con, "DROP TABLE IF EXISTS duplicate_occurrenceID")
DBI::dbExecute(
  con,
  "
  CREATE TABLE duplicate_occurrenceID AS
  SELECT occurrenceID, COUNT(*) AS n_records
  FROM occurrences_qc
  WHERE NULLIF(TRIM(occurrenceID), '') IS NOT NULL
  GROUP BY occurrenceID
  HAVING COUNT(*) > 1
  "
)

DBI::dbExecute(con, "DROP TABLE IF EXISTS duplicate_name_coordinates")
DBI::dbExecute(
  con,
  "
  CREATE TABLE duplicate_name_coordinates AS
  SELECT
    scientificName,
    vpjd_decimalLatitude AS decimalLatitude,
    vpjd_decimalLongitude AS decimalLongitude,
    COUNT(*) AS n_records
  FROM occurrences_qc
  WHERE
    NULLIF(TRIM(scientificName), '') IS NOT NULL
    AND vpjd_decimalLatitude IS NOT NULL
    AND vpjd_decimalLongitude IS NOT NULL
  GROUP BY
    scientificName,
    vpjd_decimalLatitude,
    vpjd_decimalLongitude
  HAVING COUNT(*) > 1
  "
)

# Add duplicate flags ----------------------------------------------------------

message("Adding duplicate flags...")

DBI::dbExecute(con, "DROP TABLE IF EXISTS occurrences_flagged")
DBI::dbExecute(
  con,
  "
  CREATE TABLE occurrences_flagged AS
  SELECT
    q.*,
    (g.gbifID IS NOT NULL)
      AS qc_duplicate_gbifID,
    (o.occurrenceID IS NOT NULL)
      AS qc_duplicate_occurrenceID,
    (n.scientificName IS NOT NULL)
      AS qc_duplicate_name_coordinates
  FROM occurrences_qc q
  LEFT JOIN duplicate_gbifID g
    ON q.gbifID = g.gbifID
  LEFT JOIN duplicate_occurrenceID o
    ON q.occurrenceID = o.occurrenceID
  LEFT JOIN duplicate_name_coordinates n
    ON q.scientificName = n.scientificName
   AND q.vpjd_decimalLatitude = n.decimalLatitude
   AND q.vpjd_decimalLongitude = n.decimalLongitude
  "
)

# Structural quarantine --------------------------------------------------------

message("Classifying structurally unusable spatial records...")

DBI::dbExecute(con, "DROP TABLE IF EXISTS occurrences_classified")
DBI::dbExecute(
  con,
  "
  CREATE TABLE occurrences_classified AS
  SELECT
    *,
    (
      qc_missing_latitude
      OR qc_missing_longitude
      OR qc_invalid_latitude
      OR qc_invalid_longitude
      OR qc_zero_zero
    ) AS qc_quarantine,
    CASE
      WHEN qc_missing_latitude THEN 'missing_latitude'
      WHEN qc_missing_longitude THEN 'missing_longitude'
      WHEN qc_invalid_latitude THEN 'invalid_latitude'
      WHEN qc_invalid_longitude THEN 'invalid_longitude'
      WHEN qc_zero_zero THEN 'zero_zero_coordinates'
      ELSE 'retain'
    END AS qc_primary_action
  FROM occurrences_flagged
  "
)

DBI::dbExecute(con, "DROP TABLE IF EXISTS occurrences_clean")
DBI::dbExecute(
  con,
  "
  CREATE TABLE occurrences_clean AS
  SELECT *
  FROM occurrences_classified
  WHERE qc_quarantine = FALSE
  "
)

DBI::dbExecute(con, "DROP TABLE IF EXISTS occurrences_quarantine")
DBI::dbExecute(
  con,
  "
  CREATE TABLE occurrences_quarantine AS
  SELECT *
  FROM occurrences_classified
  WHERE qc_quarantine = TRUE
  "
)

# QC summaries -----------------------------------------------------------------

message("")
message("Generating QC summaries...")

qc_summary <- DBI::dbGetQuery(
  con,
  "
  SELECT
    COUNT(*) AS total_records,
    SUM(CAST(qc_quarantine AS INTEGER)) AS quarantined_records,
    SUM(CAST(NOT qc_quarantine AS INTEGER)) AS retained_records,
    SUM(CAST(qc_missing_latitude AS INTEGER)) AS missing_latitude,
    SUM(CAST(qc_missing_longitude AS INTEGER)) AS missing_longitude,
    SUM(CAST(qc_invalid_latitude AS INTEGER)) AS invalid_latitude,
    SUM(CAST(qc_invalid_longitude AS INTEGER)) AS invalid_longitude,
    SUM(CAST(qc_zero_zero AS INTEGER)) AS zero_zero_coordinates,
    SUM(CAST(qc_missing_gbifID AS INTEGER)) AS missing_gbifID,
    SUM(CAST(qc_missing_scientificName AS INTEGER)) AS missing_scientificName,
    SUM(CAST(qc_missing_taxonKey AS INTEGER)) AS missing_taxonKey,
    SUM(CAST(qc_missing_speciesKey AS INTEGER)) AS missing_speciesKey,
    SUM(CAST(qc_not_resolved_to_species AS INTEGER))
      AS not_resolved_to_species,
    SUM(CAST(qc_country_not_JP AS INTEGER)) AS country_not_JP,
    SUM(CAST(qc_not_present AS INTEGER))
      AS occurrence_status_not_present,
    SUM(CAST(qc_uncertainty_gt_1km AS INTEGER)) AS uncertainty_gt_1km,
    SUM(CAST(qc_uncertainty_gt_10km AS INTEGER)) AS uncertainty_gt_10km,
    SUM(CAST(qc_uncertainty_gt_100km AS INTEGER)) AS uncertainty_gt_100km,
    SUM(CAST(qc_year_out_of_range AS INTEGER)) AS year_out_of_range,
    SUM(CAST(qc_gbif_zero_coordinate_issue AS INTEGER))
      AS gbif_zero_coordinate_issue,
    SUM(CAST(qc_gbif_country_coordinate_mismatch AS INTEGER))
      AS gbif_country_coordinate_mismatch,
    SUM(CAST(qc_gbif_coordinate_invalid AS INTEGER))
      AS gbif_coordinate_invalid,
    SUM(CAST(qc_gbif_coordinate_out_of_range AS INTEGER))
      AS gbif_coordinate_out_of_range,
    SUM(CAST(qc_duplicate_gbifID AS INTEGER))
      AS records_duplicate_gbifID,
    SUM(CAST(qc_duplicate_occurrenceID AS INTEGER))
      AS records_duplicate_occurrenceID,
    SUM(CAST(qc_duplicate_name_coordinates AS INTEGER))
      AS records_duplicate_name_coordinates
  FROM occurrences_classified
  "
)

qc_summary_long <- qc_summary %>%
  tidyr::pivot_longer(
    cols = everything(),
    names_to = "metric",
    values_to = "value"
  )

taxon_rank_summary <- DBI::dbGetQuery(
  con,
  "
  SELECT
    COALESCE(NULLIF(TRIM(taxonRank), ''), '[missing]') AS taxonRank,
    COUNT(*) AS records
  FROM occurrences_classified
  GROUP BY 1
  ORDER BY records DESC
  "
)

occurrence_status_summary <- DBI::dbGetQuery(
  con,
  "
  SELECT
    COALESCE(
      NULLIF(TRIM(occurrenceStatus), ''),
      '[missing]'
    ) AS occurrenceStatus,
    COUNT(*) AS records
  FROM occurrences_classified
  GROUP BY 1
  ORDER BY records DESC
  "
)

basis_summary <- DBI::dbGetQuery(
  con,
  "
  SELECT
    COALESCE(
      NULLIF(TRIM(basisOfRecord), ''),
      '[missing]'
    ) AS basisOfRecord,
    COUNT(*) AS records
  FROM occurrences_classified
  GROUP BY 1
  ORDER BY records DESC
  "
)

establishment_summary <- DBI::dbGetQuery(
  con,
  "
  SELECT
    COALESCE(
      NULLIF(TRIM(establishmentMeans), ''),
      '[missing]'
    ) AS establishmentMeans,
    COUNT(*) AS records
  FROM occurrences_classified
  GROUP BY 1
  ORDER BY records DESC
  "
)

dataset_summary <- DBI::dbGetQuery(
  con,
  "
  SELECT
    datasetKey,
    publishingOrgKey,
    COUNT(*) AS records
  FROM occurrences_classified
  GROUP BY datasetKey, publishingOrgKey
  ORDER BY records DESC
  "
)

duplicate_summary <- DBI::dbGetQuery(
  con,
  "
  SELECT
    (SELECT COUNT(*) FROM duplicate_gbifID)
      AS duplicated_gbifID_groups,
    (SELECT COUNT(*) FROM duplicate_occurrenceID)
      AS duplicated_occurrenceID_groups,
    (SELECT COUNT(*) FROM duplicate_name_coordinates)
      AS duplicated_name_coordinate_groups
  "
)

# Write audit outputs ----------------------------------------------------------

readr::write_csv(
  qc_summary_long,
  file.path(audit_dir, "gbif_occurrence_qc_summary.csv")
)

readr::write_csv(
  taxon_rank_summary,
  file.path(audit_dir, "gbif_taxon_rank_summary.csv")
)

readr::write_csv(
  occurrence_status_summary,
  file.path(audit_dir, "gbif_occurrence_status_summary.csv")
)

readr::write_csv(
  basis_summary,
  file.path(audit_dir, "gbif_basis_of_record_summary.csv")
)

readr::write_csv(
  establishment_summary,
  file.path(audit_dir, "gbif_establishment_means_summary.csv")
)

readr::write_csv(
  dataset_summary,
  file.path(audit_dir, "gbif_dataset_contribution_summary.csv")
)

readr::write_csv(
  duplicate_summary,
  file.path(audit_dir, "gbif_duplicate_summary.csv")
)

# Processing metadata ----------------------------------------------------------

DBI::dbExecute(con, "DROP TABLE IF EXISTS vpjd_import_metadata")

metadata_sql <- sprintf(
  "
  CREATE TABLE vpjd_import_metadata AS
  SELECT
    '%s' AS cleaning_module_version,
    '%s' AS gbif_download_key,
    '%s' AS gbif_doi,
    %s AS expected_records,
    %s AS imported_records,
    %s AS processing_year
  ",
  VPJD_CLEAN_OCCURRENCES_VERSION,
  gbif_download_key,
  gbif_doi,
  expected_records,
  raw_count,
  current_year
)

DBI::dbExecute(con, metadata_sql)

# Final validation -------------------------------------------------------------

final_counts <- DBI::dbGetQuery(
  con,
  "
  SELECT
    (SELECT COUNT(*) FROM gbif_raw) AS raw_records,
    (SELECT COUNT(*) FROM occurrences_clean) AS retained_records,
    (SELECT COUNT(*) FROM occurrences_quarantine) AS quarantined_records
  "
)

if (
  as.numeric(final_counts$retained_records[[1]]) +
  as.numeric(final_counts$quarantined_records[[1]]) !=
  as.numeric(final_counts$raw_records[[1]])
) {
  stop(
    "QC classification validation failed: retained + quarantined ",
    "does not equal raw record count."
  )
}

message("")
message("==============================================================")
message("INITIAL OCCURRENCE QC COMPLETE")
message("==============================================================")
message(
  "Raw records: ",
  format(final_counts$raw_records[[1]], big.mark = ",", scientific = FALSE)
)
message(
  "Retained after structural spatial QC: ",
  format(final_counts$retained_records[[1]], big.mark = ",", scientific = FALSE)
)
message(
  "Quarantined: ",
  format(final_counts$quarantined_records[[1]], big.mark = ",", scientific = FALSE)
)
message("")
message("DuckDB database: ", db_path)
message("QC summaries: ", audit_dir)
message("")
message("Taxonomic reconciliation has NOT been performed.")
message("FOJ Star ratings have NOT been assigned.")
message("Duplicate candidates have NOT been removed.")
message("")
message(
  "03_clean_occurrences.R v",
  VPJD_CLEAN_OCCURRENCES_VERSION,
  " complete."
)