# ==============================================================================
# 02_stage_institutional_occurrences.R
# VPJD / Oxford-Japan Plant Conservation Partnership
#
# Purpose:
#   Stage validated institutional GBIF downloads in DuckDB and quantify their
#   overlap with the broad national Japan GBIF harvest.
#
# National reference tables:
#   gbif_raw           = original national GBIF harvest
#   occurrences_clean  = records retained after structural spatial QC
#
# IMPORTANT:
# - Raw GBIF archives remain immutable.
# - gbif_raw and existing occurrence tables are not modified.
# - Institutional overlap is assessed primarily using persistent GBIF gbifID.
# - No occurrence cleaning.
# - No WCVP reconciliation.
# - No Star assignment.
#
# Version: 0.1.1
# ==============================================================================

VPJD_INST_STAGE_VERSION <- "0.1.1"

required_packages <- c("here", "DBI", "duckdb", "readr", "dplyr", "cli")

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0L) {
  stop(
    "Required package(s) not installed: ",
    paste(missing_packages, collapse = ", ")
  )
}

suppressPackageStartupMessages({
  library(here)
  library(DBI)
  library(duckdb)
  library(readr)
  library(dplyr)
  library(cli)
})

# Paths ------------------------------------------------------------------------

archive_inventory_path <- here::here(
  "outputs", "tables", "GBIF",
  "institutional_archive_validation",
  "institutional_gbif_archive_inventory.csv"
)

duckdb_path <- here::here(
  "data", "interim", "occurrences",
  "vpjd_occurrences.duckdb"
)

stage_root <- here::here(
  "data", "interim", "occurrences",
  "GBIF", "institutions"
)

output_dir <- here::here(
  "outputs", "tables", "GBIF",
  "institutional_integration"
)

dir.create(stage_root, recursive = TRUE, showWarnings = FALSE)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(archive_inventory_path)) {
  stop(
    "Validated institutional archive inventory not found:\n",
    archive_inventory_path
  )
}

if (!file.exists(duckdb_path)) {
  stop(
    "VPJD occurrence DuckDB not found:\n",
    duckdb_path
  )
}

# Validated archive inventory --------------------------------------------------

archive_inventory <- readr::read_csv(
  archive_inventory_path,
  show_col_types = FALSE
) |>
  dplyr::filter(
    .data$zip_valid %in% TRUE,
    !is.na(.data$archive_path)
  )

if (nrow(archive_inventory) != 93L) {
  stop(
    "Expected 93 validated institutional archives; found ",
    nrow(archive_inventory),
    "."
  )
}

cli::cli_h1("VPJD institutional occurrence staging")
cli::cli_text("{.strong Validated archives:} {nrow(archive_inventory)}")

# DuckDB connection ------------------------------------------------------------

con <- DBI::dbConnect(
  duckdb::duckdb(),
  dbdir = duckdb_path,
  read_only = FALSE
)

on.exit(
  DBI::dbDisconnect(con, shutdown = TRUE),
  add = TRUE
)

existing_tables <- DBI::dbListTables(con)

required_tables <- c(
  "gbif_raw",
  "occurrences_clean"
)

missing_tables <- setdiff(
  required_tables,
  existing_tables
)

if (length(missing_tables) > 0L) {
  stop(
    "Required VPJD DuckDB table(s) not found: ",
    paste(missing_tables, collapse = ", ")
  )
}

national_raw_n <- DBI::dbGetQuery(
  con,
  "SELECT COUNT(*) AS n FROM gbif_raw"
)$n[[1]]

national_clean_n <- DBI::dbGetQuery(
  con,
  "SELECT COUNT(*) AS n FROM occurrences_clean"
)$n[[1]]

cli::cli_text(
  "{.strong National gbif_raw records:} ",
  "{format(national_raw_n, big.mark = ',')}"
)

cli::cli_text(
  "{.strong National occurrences_clean records:} ",
  "{format(national_clean_n, big.mark = ',')}"
)

# Remove only tables owned by this staging module ------------------------------

DBI::dbExecute(
  con,
  "DROP TABLE IF EXISTS institutional_occurrence_summary"
)

DBI::dbExecute(
  con,
  "DROP TABLE IF EXISTS institutional_occurrence_overlap"
)

DBI::dbExecute(
  con,
  "DROP TABLE IF EXISTS institutional_occurrences_raw"
)

# Stage institutional archives -------------------------------------------------

progress_id <- cli::cli_progress_bar(
  "Staging institutional archives",
  total = nrow(archive_inventory),
  format = paste0(
    "{cli::pb_bar} {cli::pb_percent} | ",
    "{cli::pb_current}/{cli::pb_total} | ",
    "{cli::pb_status} | {cli::pb_elapsed}"
  ),
  clear = FALSE
)

staging_log <- vector(
  "list",
  nrow(archive_inventory)
)

for (i in seq_len(nrow(archive_inventory))) {
  
  x <- archive_inventory[i, ]
  
  dataset_title <- x$dataset_title[[1]]
  dataset_key <- x$dataset_key[[1]]
  download_key <- x$download_key[[1]]
  zip_path <- x$archive_path[[1]]
  occurrence_file <- x$occurrence_file[[1]]
  
  cli::cli_progress_update(
    id = progress_id,
    set = i - 1L,
    status = dataset_title
  )
  
  if (is.na(occurrence_file) || !nzchar(occurrence_file)) {
    stop(
      "No occurrence file recorded for dataset: ",
      dataset_title
    )
  }
  
  extract_dir <- file.path(
    stage_root,
    download_key
  )
  
  dir.create(
    extract_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  extracted_path <- file.path(
    extract_dir,
    basename(occurrence_file)
  )
  
  if (!file.exists(extracted_path)) {
    utils::unzip(
      zipfile = zip_path,
      files = occurrence_file,
      exdir = extract_dir,
      overwrite = FALSE
    )
  }
  
  if (!file.exists(extracted_path)) {
    stop(
      "Could not confirm extracted occurrence file:\n",
      extracted_path
    )
  }
  
  sql_path <- gsub(
    "'",
    "''",
    normalizePath(
      extracted_path,
      winslash = "/",
      mustWork = TRUE
    )
  )
  
  sql_dataset_title <- gsub(
    "'",
    "''",
    dataset_title
  )
  
  sql_dataset_key <- gsub(
    "'",
    "''",
    dataset_key
  )
  
  sql_download_key <- gsub(
    "'",
    "''",
    download_key
  )
  
  temp_table <- paste0(
    "inst_stage_",
    i
  )
  
  import_sql <- paste0(
    "CREATE OR REPLACE TEMP TABLE ",
    temp_table,
    " AS ",
    "SELECT *, ",
    "'",
    sql_dataset_title,
    "'::VARCHAR AS vpjd_source_dataset_title, ",
    "'",
    sql_dataset_key,
    "'::VARCHAR AS vpjd_source_dataset_key, ",
    "'",
    sql_download_key,
    "'::VARCHAR AS vpjd_source_download_key ",
    "FROM read_csv(",
    "'",
    sql_path,
    "', ",
    "delim = '\\t', ",
    "header = TRUE, ",
    "all_varchar = TRUE, ",
    "quote = '', ",
    "escape = '', ",
    "nullstr = '', ",
    "ignore_errors = FALSE",
    ")"
  )
  
  DBI::dbExecute(
    con,
    import_sql
  )
  
  staged_n <- DBI::dbGetQuery(
    con,
    paste0(
      "SELECT COUNT(*) AS n FROM ",
      temp_table
    )
  )$n[[1]]
  
  if (i == 1L) {
    
    DBI::dbExecute(
      con,
      paste0(
        "CREATE TABLE institutional_occurrences_raw AS ",
        "SELECT * FROM ",
        temp_table
      )
    )
    
  } else {
    
    DBI::dbExecute(
      con,
      paste0(
        "INSERT INTO institutional_occurrences_raw ",
        "BY NAME SELECT * FROM ",
        temp_table
      )
    )
  }
  
  DBI::dbExecute(
    con,
    paste0(
      "DROP TABLE ",
      temp_table
    )
  )
  
  staging_log[[i]] <- tibble::tibble(
    dataset_title = dataset_title,
    dataset_key = dataset_key,
    download_key = download_key,
    staged_records = staged_n
  )
  
  cli::cli_progress_update(
    id = progress_id,
    set = i
  )
}

cli::cli_progress_done(
  id = progress_id
)

staging_log <- dplyr::bind_rows(
  staging_log
)

# Validate institutional staging -----------------------------------------------

institutional_total <- DBI::dbGetQuery(
  con,
  "
  SELECT COUNT(*) AS n
  FROM institutional_occurrences_raw
  "
)$n[[1]]

institutional_unique_gbif <- DBI::dbGetQuery(
  con,
  "
  SELECT COUNT(DISTINCT gbifID) AS n
  FROM institutional_occurrences_raw
  WHERE gbifID IS NOT NULL
    AND TRIM(gbifID) <> ''
  "
)$n[[1]]

institutional_missing_gbif <- DBI::dbGetQuery(
  con,
  "
  SELECT COUNT(*) AS n
  FROM institutional_occurrences_raw
  WHERE gbifID IS NULL
     OR TRIM(gbifID) = ''
  "
)$n[[1]]

repeated_gbif_summary <- DBI::dbGetQuery(
  con,
  "
  SELECT
    COUNT(*) AS gbif_ids_represented_multiple_times,
    SUM(n) AS rows_with_repeated_gbif_id
  FROM (
    SELECT
      gbifID,
      COUNT(*) AS n
    FROM institutional_occurrences_raw
    WHERE gbifID IS NOT NULL
      AND TRIM(gbifID) <> ''
    GROUP BY gbifID
    HAVING COUNT(*) > 1
  ) x
  "
)

# National raw overlap ----------------------------------------------------------

DBI::dbExecute(
  con,
  "
  CREATE TABLE institutional_occurrence_overlap AS
  SELECT
    i.*,
    CASE
      WHEN r.gbifID IS NOT NULL THEN TRUE
      ELSE FALSE
    END AS in_national_raw,
    CASE
      WHEN c.gbifID IS NOT NULL THEN TRUE
      ELSE FALSE
    END AS in_national_clean
  FROM institutional_occurrences_raw i
  LEFT JOIN (
    SELECT DISTINCT gbifID
    FROM gbif_raw
    WHERE gbifID IS NOT NULL
      AND TRIM(gbifID) <> ''
  ) r
    ON i.gbifID = r.gbifID
  LEFT JOIN (
    SELECT DISTINCT gbifID
    FROM occurrences_clean
    WHERE gbifID IS NOT NULL
      AND TRIM(gbifID) <> ''
  ) c
    ON i.gbifID = c.gbifID
  "
)

# Dataset-level summary ---------------------------------------------------------

DBI::dbExecute(
  con,
  "
  CREATE TABLE institutional_occurrence_summary AS
  SELECT
    vpjd_source_dataset_key AS dataset_key,
    vpjd_source_dataset_title AS dataset_title,
    vpjd_source_download_key AS download_key,
    COUNT(*) AS institutional_rows,
    COUNT(DISTINCT gbifID) AS unique_gbif_ids,
    SUM(
      CASE WHEN in_national_raw
      THEN 1 ELSE 0 END
    ) AS rows_in_national_raw,
    SUM(
      CASE WHEN NOT in_national_raw
      THEN 1 ELSE 0 END
    ) AS rows_not_in_national_raw,
    SUM(
      CASE WHEN in_national_clean
      THEN 1 ELSE 0 END
    ) AS rows_in_national_clean,
    SUM(
      CASE
        WHEN NOT in_national_raw
          AND decimalLatitude IS NOT NULL
          AND TRIM(decimalLatitude) <> ''
          AND decimalLongitude IS NOT NULL
          AND TRIM(decimalLongitude) <> ''
        THEN 1 ELSE 0
      END
    ) AS additional_with_coordinates,
    SUM(
      CASE
        WHEN NOT in_national_raw
          AND (
            decimalLatitude IS NULL
            OR TRIM(decimalLatitude) = ''
            OR decimalLongitude IS NULL
            OR TRIM(decimalLongitude) = ''
          )
        THEN 1 ELSE 0
      END
    ) AS additional_without_coordinates
  FROM institutional_occurrence_overlap
  GROUP BY
    vpjd_source_dataset_key,
    vpjd_source_dataset_title,
    vpjd_source_download_key
  ORDER BY
    rows_not_in_national_raw DESC
  "
)

# Overall overlap ---------------------------------------------------------------

overlap_summary <- DBI::dbGetQuery(
  con,
  "
  SELECT
    COUNT(*) AS institutional_rows,
    SUM(
      CASE WHEN in_national_raw
      THEN 1 ELSE 0 END
    ) AS rows_in_national_raw,
    SUM(
      CASE WHEN NOT in_national_raw
      THEN 1 ELSE 0 END
    ) AS rows_not_in_national_raw,
    COUNT(
      DISTINCT CASE
        WHEN in_national_raw
        THEN gbifID
      END
    ) AS unique_ids_in_national_raw,
    COUNT(
      DISTINCT CASE
        WHEN NOT in_national_raw
        THEN gbifID
      END
    ) AS unique_ids_not_in_national_raw,
    COUNT(
      DISTINCT CASE
        WHEN in_national_clean
        THEN gbifID
      END
    ) AS unique_ids_in_national_clean
  FROM institutional_occurrence_overlap
  "
)

# Diagnose institutional records absent from national harvest ------------------

additional_diagnostics <- DBI::dbGetQuery(
  con,
  "
  SELECT
    COUNT(*) AS additional_rows,
    COUNT(DISTINCT gbifID) AS additional_unique_gbif_ids,
    SUM(
      CASE
        WHEN decimalLatitude IS NULL
          OR TRIM(decimalLatitude) = ''
          OR decimalLongitude IS NULL
          OR TRIM(decimalLongitude) = ''
        THEN 1 ELSE 0
      END
    ) AS missing_coordinates,
    SUM(
      CASE
        WHEN decimalLatitude IS NOT NULL
          AND TRIM(decimalLatitude) <> ''
          AND decimalLongitude IS NOT NULL
          AND TRIM(decimalLongitude) <> ''
        THEN 1 ELSE 0
      END
    ) AS has_coordinates,
    SUM(
      CASE WHEN occurrenceStatus = 'PRESENT'
      THEN 1 ELSE 0 END
    ) AS occurrence_present,
    SUM(
      CASE WHEN occurrenceStatus = 'ABSENT'
      THEN 1 ELSE 0 END
    ) AS occurrence_absent,
    SUM(
      CASE
        WHEN scientificName IS NULL
          OR TRIM(scientificName) = ''
        THEN 1 ELSE 0
      END
    ) AS missing_scientific_name,
    SUM(
      CASE
        WHEN taxonKey IS NULL
          OR TRIM(taxonKey) = ''
        THEN 1 ELSE 0
      END
    ) AS missing_taxon_key
  FROM institutional_occurrence_overlap
  WHERE NOT in_national_raw
  "
)

# Coordinate/status diagnostic for additional records --------------------------

additional_breakdown <- DBI::dbGetQuery(
  con,
  "
  SELECT
    CASE
      WHEN decimalLatitude IS NULL
        OR TRIM(decimalLatitude) = ''
        OR decimalLongitude IS NULL
        OR TRIM(decimalLongitude) = ''
      THEN 'NO_COORDINATES'
      ELSE 'HAS_COORDINATES'
    END AS coordinate_status,
    COALESCE(
      NULLIF(TRIM(occurrenceStatus), ''),
      'MISSING'
    ) AS occurrence_status,
    COUNT(*) AS records
  FROM institutional_occurrence_overlap
  WHERE NOT in_national_raw
  GROUP BY 1, 2
  ORDER BY records DESC
  "
)

# Export -----------------------------------------------------------------------

dataset_summary <- DBI::dbGetQuery(
  con,
  "
  SELECT *
  FROM institutional_occurrence_summary
  "
)

overall_summary <- tibble::tibble(
  metric = c(
    "national_gbif_raw_records",
    "national_occurrences_clean_records",
    "institutional_rows_staged",
    "institutional_unique_gbif_ids",
    "institutional_rows_missing_gbif_id",
    "gbif_ids_represented_multiple_times_in_institutional_data",
    "rows_with_repeated_institutional_gbif_id",
    "institutional_rows_in_national_raw",
    "institutional_rows_not_in_national_raw",
    "unique_institutional_ids_in_national_raw",
    "unique_institutional_ids_not_in_national_raw",
    "unique_institutional_ids_in_national_clean"
  ),
  value = c(
    national_raw_n,
    national_clean_n,
    institutional_total,
    institutional_unique_gbif,
    institutional_missing_gbif,
    repeated_gbif_summary$gbif_ids_represented_multiple_times[[1]],
    repeated_gbif_summary$rows_with_repeated_gbif_id[[1]],
    overlap_summary$rows_in_national_raw[[1]],
    overlap_summary$rows_not_in_national_raw[[1]],
    overlap_summary$unique_ids_in_national_raw[[1]],
    overlap_summary$unique_ids_not_in_national_raw[[1]],
    overlap_summary$unique_ids_in_national_clean[[1]]
  )
)

readr::write_csv(
  staging_log,
  file.path(
    output_dir,
    "institutional_staging_log.csv"
  )
)

readr::write_csv(
  dataset_summary,
  file.path(
    output_dir,
    "institutional_dataset_overlap_summary.csv"
  )
)

readr::write_csv(
  overall_summary,
  file.path(
    output_dir,
    "institutional_national_overlap_summary.csv"
  )
)

readr::write_csv(
  additional_diagnostics,
  file.path(
    output_dir,
    "institutional_additional_record_diagnostics.csv"
  )
)

readr::write_csv(
  additional_breakdown,
  file.path(
    output_dir,
    "institutional_additional_coordinate_status.csv"
  )
)

# Console summary ---------------------------------------------------------------

fmt <- function(x) {
  format(
    x,
    big.mark = ",",
    scientific = FALSE,
    trim = TRUE
  )
}

cli::cli_h2("Institutional occurrence staging complete")

cli::cli_text(
  "{.strong Institutional archives staged:} {nrow(staging_log)}"
)

cli::cli_text(
  "{.strong Institutional rows staged:} {fmt(institutional_total)}"
)

cli::cli_text(
  "{.strong Unique institutional GBIF IDs:} {fmt(institutional_unique_gbif)}"
)

cli::cli_text(
  "{.strong Institutional rows missing GBIF ID:} {fmt(institutional_missing_gbif)}"
)

cli::cli_text(
  "{.strong National gbif_raw records:} {fmt(national_raw_n)}"
)

cli::cli_text(
  "{.strong Institutional rows already in national harvest:} ",
  "{fmt(overlap_summary$rows_in_national_raw[[1]])}"
)

cli::cli_text(
  "{.strong Institutional rows outside national harvest:} ",
  "{fmt(overlap_summary$rows_not_in_national_raw[[1]])}"
)

cli::cli_text(
  "{.strong Unique institutional GBIF IDs outside national harvest:} ",
  "{fmt(overlap_summary$unique_ids_not_in_national_raw[[1]])}"
)

cli::cli_text(
  "{.strong Additional rows with coordinates:} ",
  "{fmt(additional_diagnostics$has_coordinates[[1]])}"
)

cli::cli_text(
  "{.strong Additional rows without coordinates:} ",
  "{fmt(additional_diagnostics$missing_coordinates[[1]])}"
)

cli::cli_text(
  "{.strong Unique institutional GBIF IDs retained in national clean data:} ",
  "{fmt(overlap_summary$unique_ids_in_national_clean[[1]])}"
)

cli::cli_alert_success(
  "Existing national occurrence tables were not modified."
)

cli::cli_text(
  "02_stage_institutional_occurrences.R v{VPJD_INST_STAGE_VERSION} complete."
)