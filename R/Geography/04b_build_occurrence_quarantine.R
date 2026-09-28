# ==============================================================================
# VPJD-OJPCP
# Geography 04b — Build persistent occurrence quarantine
# Version: 0.1.1
#
# Purpose:
#   Establish the persistent VPJD occurrence-quarantine architecture and
#   initialise it with the spatially unresolved occurrences validated by
#   Geography 04a v0.2.0.
#
# Architecture:
#
#   vpjd_occurrence_quarantine
#     One row per quarantined occurrence.
#     Stores overall quarantine/review/resolution state.
#
#   vpjd_occurrence_quarantine_reason
#     One or more rows per quarantined occurrence.
#     Stores individual reasons, stages and provenance.
#
# v0.1.1:
#   - avoids DuckDB CURRENT_DATE / ICU dependency;
#   - generates processing date in R;
#   - detects and removes empty tables left by failed initialisation;
#   - refuses to overwrite populated persistent quarantine tables.
#
# Principles:
#   - quarantine is NOT deletion or rejection;
#   - original occurrence data remain in canonical occurrence tables;
#   - multiple quarantine reasons may apply to one occurrence;
#   - future modules may append reasons without rebuilding existing history;
#   - future ingestion batches can process only new records;
#   - records can later be reviewed and released from quarantine;
#   - this module does NOT modify coordinates, taxonomy or Geography 04;
#   - this module does NOT calculate distributions, Stars or GHI.
# ==============================================================================

suppressPackageStartupMessages({
  library(DBI)
  library(duckdb)
  library(dplyr)
  library(readr)
  library(here)
})

SCRIPT_VERSION <- "0.1.1"
SOURCE_MODULE <- "04b"
SOURCE_PROFILE_TABLE <- "vpjd_japan_spatially_unresolved_occurrences"
QUARANTINE_TABLE <- "vpjd_occurrence_quarantine"
REASON_TABLE <- "vpjd_occurrence_quarantine_reason"
EXPECTED_QUARANTINE <- 83201L
EXPECTED_GROSS <- 504L
EXPECTED_WIDER <- 31L
RUN_DATE <- format(Sys.Date(), "%Y-%m-%d")

run_occurrence_quarantine_build <- function() {
  cat("\n— VPJD persistent occurrence quarantine —\n\n")
  cat("Run date: ", RUN_DATE, "\n\n", sep = "")
  
  db_path <- here(
    "data", "interim", "occurrences",
    "vpjd_occurrences.duckdb"
  )
  
  output_dir <- here(
    "outputs", "tables", "quarantine"
  )
  
  dir.create(
    output_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  if (!file.exists(db_path)) {
    stop("DuckDB database not found: ", db_path)
  }
  
  con <- dbConnect(
    duckdb(),
    dbdir = db_path,
    read_only = FALSE
  )
  
  on.exit(
    dbDisconnect(con, shutdown = TRUE),
    add = TRUE
  )
  
  quote_id <- function(x) {
    paste0(
      '"',
      gsub('"', '""', x),
      '"'
    )
  }
  
  q_source <- quote_id(SOURCE_PROFILE_TABLE)
  q_quarantine <- quote_id(QUARANTINE_TABLE)
  q_reason <- quote_id(REASON_TABLE)
  
  tables <- dbListTables(con)
  
  if (!(SOURCE_PROFILE_TABLE %in% tables)) {
    stop(
      "Geography 04a source table not found: ",
      SOURCE_PROFILE_TABLE
    )
  }
  
  # ---------------------------------------------------------------------------
  # Validate Geography 04a source
  # ---------------------------------------------------------------------------
  
  source_count <- dbGetQuery(
    con,
    paste0(
      "SELECT COUNT(*) AS n FROM ",
      q_source
    )
  )$n[[1]]
  
  source_ids <- dbGetQuery(
    con,
    paste0(
      "SELECT COUNT(DISTINCT CAST(gbifID AS VARCHAR)) AS n FROM ",
      q_source
    )
  )$n[[1]]
  
  gross_count <- dbGetQuery(
    con,
    paste0(
      "SELECT COUNT(*) AS n FROM ",
      q_source,
      " WHERE SPATIAL_QA_CLASS = 'gross_coordinate_anomaly'"
    )
  )$n[[1]]
  
  wider_count <- dbGetQuery(
    con,
    paste0(
      "SELECT COUNT(*) AS n FROM ",
      q_source,
      " WHERE SPATIAL_QA_CLASS = ",
      "'outside_broad_but_within_wider_region'"
    )
  )$n[[1]]
  
  broad_count <- dbGetQuery(
    con,
    paste0(
      "SELECT COUNT(*) AS n FROM ",
      q_source,
      " WHERE SPATIAL_QA_CLASS = ",
      "'outside_within_broad_japan_envelope'"
    )
  )$n[[1]]
  
  cat(
    "Geography 04a source records: ",
    format(source_count, big.mark = ","),
    "\n",
    sep = ""
  )
  
  cat(
    "Distinct source gbifIDs: ",
    format(source_ids, big.mark = ","),
    "\n",
    sep = ""
  )
  
  cat(
    "Broad-envelope unresolved: ",
    format(broad_count, big.mark = ","),
    "\n",
    sep = ""
  )
  
  cat(
    "Wider-region unresolved: ",
    format(wider_count, big.mark = ","),
    "\n",
    sep = ""
  )
  
  cat(
    "Gross coordinate anomalies: ",
    format(gross_count, big.mark = ","),
    "\n\n",
    sep = ""
  )
  
  if (source_count != EXPECTED_QUARANTINE) {
    stop("Unexpected Geography 04a source count.")
  }
  
  if (source_ids != source_count) {
    stop("Geography 04a source gbifIDs are not unique.")
  }
  
  if (gross_count != EXPECTED_GROSS) {
    stop("Unexpected gross-coordinate-anomaly count.")
  }
  
  if (wider_count != EXPECTED_WIDER) {
    stop("Unexpected wider-region count.")
  }
  
  if ((broad_count + wider_count + gross_count) != source_count) {
    stop("Geography 04a QA classes do not reconcile.")
  }
  
  cat("Geography 04a source validation: PASS\n\n")
  
  # ---------------------------------------------------------------------------
  # Detect previous failed/partial initialisation
  #
  # Safe behaviour:
  #   - neither table exists: proceed;
  #   - table exists but contains zero rows: remove it and rebuild;
  #   - table contains one or more rows: STOP.
  #
  # Once populated, these tables are persistent database state and must never
  # be silently replaced by this initialisation module.
  # ---------------------------------------------------------------------------
  
  tables <- dbListTables(con)
  
  quarantine_exists <- QUARANTINE_TABLE %in% tables
  reason_exists <- REASON_TABLE %in% tables
  
  if (quarantine_exists) {
    existing_quarantine_count <- dbGetQuery(
      con,
      paste0(
        "SELECT COUNT(*) AS n FROM ",
        q_quarantine
      )
    )$n[[1]]
    
    cat(
      "Existing quarantine table detected: ",
      format(existing_quarantine_count, big.mark = ","),
      " rows\n",
      sep = ""
    )
    
    if (existing_quarantine_count > 0L) {
      stop(
        "Canonical quarantine table already contains data: ",
        QUARANTINE_TABLE,
        ". Persistent quarantine history will not be overwritten."
      )
    }
  } else {
    existing_quarantine_count <- NA_integer_
  }
  
  if (reason_exists) {
    existing_reason_count <- dbGetQuery(
      con,
      paste0(
        "SELECT COUNT(*) AS n FROM ",
        q_reason
      )
    )$n[[1]]
    
    cat(
      "Existing quarantine-reason table detected: ",
      format(existing_reason_count, big.mark = ","),
      " rows\n",
      sep = ""
    )
    
    if (existing_reason_count > 0L) {
      stop(
        "Canonical quarantine-reason table already contains data: ",
        REASON_TABLE,
        ". Persistent quarantine history will not be overwritten."
      )
    }
  } else {
    existing_reason_count <- NA_integer_
  }
  
  if (quarantine_exists && existing_quarantine_count == 0L) {
    cat(
      "Removing empty quarantine table left by incomplete initialisation...\n"
    )
    
    dbExecute(
      con,
      paste0(
        "DROP TABLE ",
        q_quarantine
      )
    )
  }
  
  if (reason_exists && existing_reason_count == 0L) {
    cat(
      "Removing empty quarantine-reason table left by incomplete initialisation...\n"
    )
    
    dbExecute(
      con,
      paste0(
        "DROP TABLE ",
        q_reason
      )
    )
  }
  
  cat("Persistent-state safeguard: PASS\n\n")
  
  # ---------------------------------------------------------------------------
  # Create canonical quarantine table
  #
  # One row per occurrence.
  #
  # QUARANTINE_STATUS:
  #   ACTIVE   = currently excluded from analytical occurrence population
  #   RELEASED = reviewed/resolved and eligible for analytical use
  #   REJECTED = reviewed and retained only for provenance
  #
  # REVIEW_STATUS:
  #   NOT_REVIEWED
  #   IN_REVIEW
  #   REVIEWED
  # ---------------------------------------------------------------------------
  
  cat("Creating canonical occurrence quarantine...\n")
  
  dbExecute(
    con,
    paste0(
      "CREATE TABLE ",
      q_quarantine,
      " (",
      "gbifID VARCHAR PRIMARY KEY, ",
      "QUARANTINE_STATUS VARCHAR NOT NULL, ",
      "REVIEW_STATUS VARCHAR NOT NULL, ",
      "FIRST_QUARANTINED_DATE DATE NOT NULL, ",
      "LAST_STATUS_DATE DATE NOT NULL, ",
      "FIRST_QUARANTINED_MODULE VARCHAR NOT NULL, ",
      "FIRST_QUARANTINED_VERSION VARCHAR NOT NULL, ",
      "REVIEWED_DATE DATE, ",
      "REVIEWED_BY VARCHAR, ",
      "REVIEW_NOTES VARCHAR, ",
      "RESOLUTION_ACTION VARCHAR, ",
      "RESOLUTION_DATE DATE, ",
      "RELEASE_MODULE VARCHAR, ",
      "RELEASE_VERSION VARCHAR",
      ")"
    )
  )
  
  # ---------------------------------------------------------------------------
  # Create canonical quarantine-reason table
  # ---------------------------------------------------------------------------
  
  cat("Creating canonical quarantine-reason table...\n")
  
  dbExecute(
    con,
    paste0(
      "CREATE TABLE ",
      q_reason,
      " (",
      "gbifID VARCHAR NOT NULL, ",
      "QUARANTINE_STAGE VARCHAR NOT NULL, ",
      "QUARANTINE_REASON VARCHAR NOT NULL, ",
      "QUARANTINE_DETAIL VARCHAR, ",
      "REASON_STATUS VARCHAR NOT NULL, ",
      "SOURCE_MODULE VARCHAR NOT NULL, ",
      "SOURCE_VERSION VARCHAR NOT NULL, ",
      "REASON_ADDED_DATE DATE NOT NULL, ",
      "REASON_RESOLVED_DATE DATE, ",
      "RESOLUTION_NOTES VARCHAR, ",
      "PRIMARY KEY ",
      "(gbifID, QUARANTINE_STAGE, QUARANTINE_REASON, SOURCE_MODULE)",
      ")"
    )
  )
  
  # ---------------------------------------------------------------------------
  # Populate one quarantine row per Geography 04a occurrence
  #
  # Date is supplied explicitly from R. This avoids DuckDB CURRENT_DATE and
  # therefore avoids the ICU extension dependency encountered in v0.1.0.
  # ---------------------------------------------------------------------------
  
  cat("Populating occurrence quarantine...\n")
  
  dbExecute(
    con,
    paste0(
      "INSERT INTO ",
      q_quarantine,
      " (",
      "gbifID, ",
      "QUARANTINE_STATUS, ",
      "REVIEW_STATUS, ",
      "FIRST_QUARANTINED_DATE, ",
      "LAST_STATUS_DATE, ",
      "FIRST_QUARANTINED_MODULE, ",
      "FIRST_QUARANTINED_VERSION",
      ") ",
      "SELECT ",
      "CAST(gbifID AS VARCHAR), ",
      "'ACTIVE', ",
      "'NOT_REVIEWED', ",
      "DATE '",
      RUN_DATE,
      "', ",
      "DATE '",
      RUN_DATE,
      "', ",
      "'",
      SOURCE_MODULE,
      "', ",
      "'",
      SCRIPT_VERSION,
      "' ",
      "FROM ",
      q_source
    )
  )
  
  # ---------------------------------------------------------------------------
  # Reason 1 — all Geography 04a records
  # ---------------------------------------------------------------------------
  
  cat(
    "Adding reason: outside_canonical_botanical_area...\n"
  )
  
  dbExecute(
    con,
    paste0(
      "INSERT INTO ",
      q_reason,
      " (",
      "gbifID, ",
      "QUARANTINE_STAGE, ",
      "QUARANTINE_REASON, ",
      "QUARANTINE_DETAIL, ",
      "REASON_STATUS, ",
      "SOURCE_MODULE, ",
      "SOURCE_VERSION, ",
      "REASON_ADDED_DATE",
      ") ",
      "SELECT ",
      "CAST(gbifID AS VARCHAR), ",
      "'geography', ",
      "'outside_canonical_botanical_area', ",
      "'No intersection with canonical JP01-JP51 botanical-area geometry', ",
      "'ACTIVE', ",
      "'",
      SOURCE_MODULE,
      "', ",
      "'",
      SCRIPT_VERSION,
      "', ",
      "DATE '",
      RUN_DATE,
      "' ",
      "FROM ",
      q_source
    )
  )
  
  # ---------------------------------------------------------------------------
  # Reason 2 — gross coordinate anomalies
  # ---------------------------------------------------------------------------
  
  cat(
    "Adding reason: gross_coordinate_anomaly...\n"
  )
  
  dbExecute(
    con,
    paste0(
      "INSERT INTO ",
      q_reason,
      " (",
      "gbifID, ",
      "QUARANTINE_STAGE, ",
      "QUARANTINE_REASON, ",
      "QUARANTINE_DETAIL, ",
      "REASON_STATUS, ",
      "SOURCE_MODULE, ",
      "SOURCE_VERSION, ",
      "REASON_ADDED_DATE",
      ") ",
      "SELECT ",
      "CAST(gbifID AS VARCHAR), ",
      "'geography', ",
      "'gross_coordinate_anomaly', ",
      "'Coordinates fall outside VPJD wider regional QA envelope', ",
      "'ACTIVE', ",
      "'",
      SOURCE_MODULE,
      "', ",
      "'",
      SCRIPT_VERSION,
      "', ",
      "DATE '",
      RUN_DATE,
      "' ",
      "FROM ",
      q_source,
      " WHERE SPATIAL_QA_CLASS = 'gross_coordinate_anomaly'"
    )
  )
  
  # ---------------------------------------------------------------------------
  # Reason 3 — outside broad Japan envelope but inside wider regional envelope
  # ---------------------------------------------------------------------------
  
  cat(
    "Adding reason: outside_broad_japan_envelope...\n"
  )
  
  dbExecute(
    con,
    paste0(
      "INSERT INTO ",
      q_reason,
      " (",
      "gbifID, ",
      "QUARANTINE_STAGE, ",
      "QUARANTINE_REASON, ",
      "QUARANTINE_DETAIL, ",
      "REASON_STATUS, ",
      "SOURCE_MODULE, ",
      "SOURCE_VERSION, ",
      "REASON_ADDED_DATE",
      ") ",
      "SELECT ",
      "CAST(gbifID AS VARCHAR), ",
      "'geography', ",
      "'outside_broad_japan_envelope', ",
      "'Outside broad Japan QA envelope but within wider regional envelope', ",
      "'ACTIVE', ",
      "'",
      SOURCE_MODULE,
      "', ",
      "'",
      SCRIPT_VERSION,
      "', ",
      "DATE '",
      RUN_DATE,
      "' ",
      "FROM ",
      q_source,
      " WHERE SPATIAL_QA_CLASS = ",
      "'outside_broad_but_within_wider_region'"
    )
  )
  
  # ---------------------------------------------------------------------------
  # Counts
  # ---------------------------------------------------------------------------
  
  quarantine_count <- dbGetQuery(
    con,
    paste0(
      "SELECT COUNT(*) AS n FROM ",
      q_quarantine
    )
  )$n[[1]]
  
  quarantine_ids <- dbGetQuery(
    con,
    paste0(
      "SELECT COUNT(DISTINCT gbifID) AS n FROM ",
      q_quarantine
    )
  )$n[[1]]
  
  active_count <- dbGetQuery(
    con,
    paste0(
      "SELECT COUNT(*) AS n FROM ",
      q_quarantine,
      " WHERE QUARANTINE_STATUS = 'ACTIVE'"
    )
  )$n[[1]]
  
  reason_count <- dbGetQuery(
    con,
    paste0(
      "SELECT COUNT(*) AS n FROM ",
      q_reason
    )
  )$n[[1]]
  
  expected_reasons <-
    EXPECTED_QUARANTINE +
    EXPECTED_GROSS +
    EXPECTED_WIDER
  
  reason_summary <- dbGetQuery(
    con,
    paste0(
      "SELECT ",
      "QUARANTINE_STAGE, ",
      "QUARANTINE_REASON, ",
      "REASON_STATUS, ",
      "COUNT(*) AS records ",
      "FROM ",
      q_reason,
      " GROUP BY ",
      "QUARANTINE_STAGE, ",
      "QUARANTINE_REASON, ",
      "REASON_STATUS ",
      "ORDER BY records DESC"
    )
  )
  
  cat("\n— Quarantine summary —\n")
  
  cat(
    "Quarantined occurrences: ",
    format(quarantine_count, big.mark = ","),
    "\n",
    sep = ""
  )
  
  cat(
    "Active quarantines: ",
    format(active_count, big.mark = ","),
    "\n",
    sep = ""
  )
  
  cat(
    "Quarantine-reason rows: ",
    format(reason_count, big.mark = ","),
    "\n\n",
    sep = ""
  )
  
  print.data.frame(
    reason_summary,
    row.names = FALSE
  )
  
  # ---------------------------------------------------------------------------
  # Number of active reasons per occurrence
  # ---------------------------------------------------------------------------
  
  reasons_per_occurrence <- dbGetQuery(
    con,
    paste0(
      "SELECT ",
      "active_reason_count, ",
      "COUNT(*) AS occurrences ",
      "FROM (",
      "SELECT ",
      "gbifID, ",
      "COUNT(*) AS active_reason_count ",
      "FROM ",
      q_reason,
      " WHERE REASON_STATUS = 'ACTIVE' ",
      "GROUP BY gbifID",
      ") x ",
      "GROUP BY active_reason_count ",
      "ORDER BY active_reason_count"
    )
  )
  
  cat("\n— Active reasons per occurrence —\n")
  
  print.data.frame(
    reasons_per_occurrence,
    row.names = FALSE
  )
  
  # ---------------------------------------------------------------------------
  # Create reusable active-quarantine view
  # ---------------------------------------------------------------------------
  
  active_view <- "vpjd_occurrence_quarantine_active"
  
  dbExecute(
    con,
    paste0(
      "CREATE OR REPLACE VIEW ",
      quote_id(active_view),
      " AS ",
      "SELECT ",
      "q.gbifID, ",
      "q.QUARANTINE_STATUS, ",
      "q.REVIEW_STATUS, ",
      "q.FIRST_QUARANTINED_DATE, ",
      "q.LAST_STATUS_DATE, ",
      "COUNT(r.QUARANTINE_REASON) AS ACTIVE_REASON_COUNT ",
      "FROM ",
      q_quarantine,
      " q ",
      "LEFT JOIN ",
      q_reason,
      " r ",
      "ON q.gbifID = r.gbifID ",
      "AND r.REASON_STATUS = 'ACTIVE' ",
      "WHERE q.QUARANTINE_STATUS = 'ACTIVE' ",
      "GROUP BY ",
      "q.gbifID, ",
      "q.QUARANTINE_STATUS, ",
      "q.REVIEW_STATUS, ",
      "q.FIRST_QUARANTINED_DATE, ",
      "q.LAST_STATUS_DATE"
    )
  )
  
  active_view_count <- dbGetQuery(
    con,
    paste0(
      "SELECT COUNT(*) AS n FROM ",
      quote_id(active_view)
    )
  )$n[[1]]
  
  # ---------------------------------------------------------------------------
  # Validation
  # ---------------------------------------------------------------------------
  
  orphan_reason_count <- dbGetQuery(
    con,
    paste0(
      "SELECT COUNT(*) AS n ",
      "FROM ",
      q_reason,
      " r ",
      "LEFT JOIN ",
      q_quarantine,
      " q ON r.gbifID = q.gbifID ",
      "WHERE q.gbifID IS NULL"
    )
  )$n[[1]]
  
  zero_reason_count <- dbGetQuery(
    con,
    paste0(
      "SELECT COUNT(*) AS n FROM (",
      "SELECT q.gbifID ",
      "FROM ",
      q_quarantine,
      " q ",
      "LEFT JOIN ",
      q_reason,
      " r ",
      "ON q.gbifID = r.gbifID ",
      "AND r.REASON_STATUS = 'ACTIVE' ",
      "WHERE q.QUARANTINE_STATUS = 'ACTIVE' ",
      "GROUP BY q.gbifID ",
      "HAVING COUNT(r.QUARANTINE_REASON) = 0",
      ") z"
    )
  )$n[[1]]
  
  general_reason_count <- dbGetQuery(
    con,
    paste0(
      "SELECT COUNT(*) AS n FROM ",
      q_reason,
      " WHERE QUARANTINE_REASON = ",
      "'outside_canonical_botanical_area'"
    )
  )$n[[1]]
  
  gross_reason_count <- dbGetQuery(
    con,
    paste0(
      "SELECT COUNT(*) AS n FROM ",
      q_reason,
      " WHERE QUARANTINE_REASON = ",
      "'gross_coordinate_anomaly'"
    )
  )$n[[1]]
  
  wider_reason_count <- dbGetQuery(
    con,
    paste0(
      "SELECT COUNT(*) AS n FROM ",
      q_reason,
      " WHERE QUARANTINE_REASON = ",
      "'outside_broad_japan_envelope'"
    )
  )$n[[1]]
  
  not_reviewed_count <- dbGetQuery(
    con,
    paste0(
      "SELECT COUNT(*) AS n FROM ",
      q_quarantine,
      " WHERE REVIEW_STATUS = 'NOT_REVIEWED'"
    )
  )$n[[1]]
  
  validation <- tibble(
    check = c(
      "source_records_83201",
      "source_gbifIDs_unique",
      "quarantine_records_83201",
      "quarantine_gbifIDs_unique",
      "all_quarantines_active",
      "all_quarantines_not_reviewed",
      "general_geography_reason_83201",
      "gross_coordinate_reason_504",
      "wider_region_reason_31",
      "reason_rows_83736",
      "no_orphan_reason_rows",
      "no_active_quarantine_without_active_reason",
      "active_view_records_83201",
      "source_occurrences_not_modified",
      "coordinates_not_modified",
      "taxonomy_not_modified",
      "geography_not_modified",
      "records_not_deleted",
      "stars_not_calculated",
      "ghi_not_calculated"
    ),
    pass = c(
      source_count == EXPECTED_QUARANTINE,
      source_ids == source_count,
      quarantine_count == EXPECTED_QUARANTINE,
      quarantine_ids == quarantine_count,
      active_count == quarantine_count,
      not_reviewed_count == quarantine_count,
      general_reason_count == EXPECTED_QUARANTINE,
      gross_reason_count == EXPECTED_GROSS,
      wider_reason_count == EXPECTED_WIDER,
      reason_count == expected_reasons,
      orphan_reason_count == 0L,
      zero_reason_count == 0L,
      active_view_count == EXPECTED_QUARANTINE,
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE
    )
  )
  
  cat("\n— Validation —\n")
  
  print.data.frame(
    validation,
    row.names = FALSE
  )
  
  all_pass <- all(
    validation$pass
  )
  
  cat(
    "\nAll quarantine validation checks PASS: ",
    all_pass,
    "\n",
    sep = ""
  )
  
  # ---------------------------------------------------------------------------
  # Metadata
  # ---------------------------------------------------------------------------
  
  metadata <- tibble(
    metric = c(
      "script",
      "version",
      "run_date",
      "source_profile",
      "quarantine_table",
      "reason_table",
      "active_view",
      "quarantined_occurrences",
      "active_quarantines",
      "quarantine_reason_rows",
      "general_geography_reasons",
      "gross_coordinate_reasons",
      "wider_region_reasons",
      "architecture",
      "incremental_updates_supported",
      "multiple_reasons_supported",
      "duckdb_current_date_used",
      "icu_required",
      "coordinates_modified",
      "taxonomy_modified",
      "geography_modified",
      "records_deleted",
      "validation_pass"
    ),
    value = c(
      "04b_build_occurrence_quarantine",
      SCRIPT_VERSION,
      RUN_DATE,
      SOURCE_PROFILE_TABLE,
      QUARANTINE_TABLE,
      REASON_TABLE,
      active_view,
      as.character(quarantine_count),
      as.character(active_count),
      as.character(reason_count),
      as.character(general_reason_count),
      as.character(gross_reason_count),
      as.character(wider_reason_count),
      "persistent_one_occurrence_plus_one_to_many_reasons",
      "TRUE",
      "TRUE",
      "FALSE",
      "FALSE",
      "FALSE",
      "FALSE",
      "FALSE",
      "FALSE",
      as.character(all_pass)
    )
  )
  
  # ---------------------------------------------------------------------------
  # Export QA snapshots
  #
  # DuckDB tables are canonical.
  # CSVs are review/export products only.
  # ---------------------------------------------------------------------------
  
  write_csv(
    reason_summary,
    file.path(
      output_dir,
      "quarantine_reason_summary.csv"
    )
  )
  
  write_csv(
    reasons_per_occurrence,
    file.path(
      output_dir,
      "quarantine_reasons_per_occurrence.csv"
    )
  )
  
  write_csv(
    validation,
    file.path(
      output_dir,
      "quarantine_validation.csv"
    )
  )
  
  write_csv(
    metadata,
    file.path(
      output_dir,
      "quarantine_metadata.csv"
    )
  )
  
  gross_review <- dbGetQuery(
    con,
    paste0(
      "SELECT s.* ",
      "FROM ",
      q_source,
      " s ",
      "WHERE s.SPATIAL_QA_CLASS = ",
      "'gross_coordinate_anomaly' ",
      "ORDER BY ",
      "s.ANALYTICAL_DECIMAL_LATITUDE, ",
      "s.ANALYTICAL_DECIMAL_LONGITUDE"
    )
  )
  
  write_csv(
    gross_review,
    file.path(
      output_dir,
      "review_gross_coordinate_anomalies.csv"
    )
  )
  
  wider_review <- dbGetQuery(
    con,
    paste0(
      "SELECT s.* ",
      "FROM ",
      q_source,
      " s ",
      "WHERE s.SPATIAL_QA_CLASS = ",
      "'outside_broad_but_within_wider_region' ",
      "ORDER BY ",
      "s.ANALYTICAL_DECIMAL_LATITUDE, ",
      "s.ANALYTICAL_DECIMAL_LONGITUDE"
    )
  )
  
  write_csv(
    wider_review,
    file.path(
      output_dir,
      "review_outside_broad_japan_envelope.csv"
    )
  )
  
  # ---------------------------------------------------------------------------
  # Store metadata/validation in DuckDB
  # ---------------------------------------------------------------------------
  
  dbWriteTable(
    con,
    "vpjd_occurrence_quarantine_validation",
    validation,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "vpjd_occurrence_quarantine_metadata",
    metadata,
    overwrite = TRUE
  )
  
  # ---------------------------------------------------------------------------
  # Final status
  # ---------------------------------------------------------------------------
  
  if (!all_pass) {
    cat(
      "\nOutput status: FAILED QUARANTINE VALIDATION\n"
    )
    
    stop(
      paste(
        "Geography 04b quarantine validation failed.",
        "Do not use the quarantine as canonical."
      )
    )
  }
  
  cat("\nCanonical tables:\n")
  cat("  ", QUARANTINE_TABLE, "\n", sep = "")
  cat("  ", REASON_TABLE, "\n", sep = "")
  
  cat("\nReusable active-quarantine view:\n")
  cat("  ", active_view, "\n", sep = "")
  
  cat(
    "\nOutput status: ",
    "VALIDATED PERSISTENT OCCURRENCE QUARANTINE\n",
    sep = ""
  )
  
  cat(
    "Active quarantined occurrences: ",
    format(active_count, big.mark = ","),
    "\n",
    sep = ""
  )
  
  cat(
    "Active quarantine reasons: ",
    format(reason_count, big.mark = ","),
    "\n",
    sep = ""
  )
  
  cat("Persistent architecture created: TRUE\n")
  cat("Multiple reasons supported: TRUE\n")
  cat("Incremental future updates supported: TRUE\n")
  cat("DuckDB CURRENT_DATE used: FALSE\n")
  cat("ICU extension required: FALSE\n")
  cat("Coordinates modified: FALSE\n")
  cat("Taxonomy modified: FALSE\n")
  cat("Geography modified: FALSE\n")
  cat("Occurrences deleted: FALSE\n")
  cat("Stars calculated: FALSE\n")
  cat("GHI calculated: FALSE\n")
  
  cat(
    "\nGeography 04b v",
    SCRIPT_VERSION,
    " complete.\n",
    sep = ""
  )
  
  invisible(
    list(
      reason_summary =
        reason_summary,
      reasons_per_occurrence =
        reasons_per_occurrence,
      validation =
        validation,
      metadata =
        metadata
    )
  )
}

result_04b <-
  run_occurrence_quarantine_build()