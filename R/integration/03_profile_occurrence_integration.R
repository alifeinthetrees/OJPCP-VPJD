# ==============================================================================
# VPJD-OJPCP
# 03_profile_occurrence_integration.R
# Version: 0.1.0
#
# Purpose:
#   Profile the two occurrence streams before constructing the integrated VPJD
#   analytical occurrence layer:
#
#   1. National GBIF PRESENT occurrences.
#   2. Gazetteer-recovered institutional occurrences from Integration 02.
#
# This module is diagnostic only.
#
# It does NOT:
#   - append recovered records to the national occurrence table;
#   - modify coordinates;
#   - remove duplicates;
#   - discard unresolved taxa;
#   - make new taxonomic assignments;
#   - modify frozen upstream tables.
#
# Required frozen inputs:
#   Cleaning 03 v0.1.2
#   Integration 01 v0.1.0
#   Integration 02 v0.1.4
#   Taxonomy 04p v0.1.1
# ==============================================================================

suppressPackageStartupMessages({
  library(DBI)
  library(duckdb)
  library(dplyr)
  library(tibble)
  library(readr)
  library(here)
})

SCRIPT_VERSION <- "0.1.0"
EXPECTED_NATIONAL_PRESENT <- 3099799L
EXPECTED_RECOVERED <- 13290L
EXPECTED_COMBINED_PRE_DEDUP <- 3113089L

get_metadata_value <- function(con, table_name, metric_name) {
  fields <- dbListFields(con, table_name)
  
  if (all(c("metric", "value") %in% fields)) {
    sql <- paste0(
      "SELECT CAST(value AS VARCHAR) AS value FROM ",
      dbQuoteIdentifier(con, table_name),
      " WHERE CAST(metric AS VARCHAR) = ?"
    )
    result <- dbGetQuery(con, sql, params = list(metric_name))
    
    if (nrow(result) != 1) {
      stop(
        "Expected exactly one '", metric_name,
        "' row in ", table_name,
        "; found ", nrow(result), "."
      )
    }
    
    return(as.character(result$value[[1]]))
  }
  
  if (metric_name %in% fields) {
    metadata <- dbReadTable(con, table_name)
    
    if (nrow(metadata) != 1) {
      stop(
        "Expected exactly one row in ",
        table_name, "."
      )
    }
    
    return(as.character(metadata[[metric_name]][[1]]))
  }
  
  stop(
    "Metadata item '", metric_name,
    "' not found in ", table_name, "."
  )
}

run_integration_profile <- function() {
  
  cat("\n— VPJD occurrence integration profile —\n\n")
  
  db_path <- here(
    "data", "interim", "occurrences",
    "vpjd_occurrences.duckdb"
  )
  
  if (!file.exists(db_path)) {
    stop("DuckDB not found: ", db_path)
  }
  
  con <- dbConnect(
    duckdb::duckdb(),
    dbdir = db_path,
    read_only = FALSE
  )
  
  on.exit(
    dbDisconnect(con, shutdown = TRUE),
    add = TRUE
  )
  
  required_tables <- c(
    "occurrences_clean",
    "recovered_institutional_occurrences",
    "recovered_institutional_occurrences_taxonomised",
    "recovered_institutional_occurrence_metadata",
    "recovered_institutional_taxon_integration_metadata",
    "occurrence_wcvp_consolidation_metadata"
  )
  
  missing_tables <- setdiff(
    required_tables,
    dbListTables(con)
  )
  
  if (length(missing_tables) > 0) {
    stop(
      "Required tables missing: ",
      paste(missing_tables, collapse = ", ")
    )
  }
  
  # ---------------------------------------------------------------------------
  # Validate frozen upstream versions
  # ---------------------------------------------------------------------------
  
  integration_01_version <- get_metadata_value(
    con,
    "recovered_institutional_occurrence_metadata",
    "script_version"
  )
  
  integration_02_version <- get_metadata_value(
    con,
    "recovered_institutional_taxon_integration_metadata",
    "script_version"
  )
  
  taxonomy_04p_version <- get_metadata_value(
    con,
    "occurrence_wcvp_consolidation_metadata",
    "script_version"
  )
  
  if (integration_01_version != "0.1.0") {
    stop(
      "Expected Integration 01 v0.1.0; detected ",
      integration_01_version, "."
    )
  }
  
  if (integration_02_version != "0.1.4") {
    stop(
      "Expected Integration 02 v0.1.4; detected ",
      integration_02_version, "."
    )
  }
  
  if (taxonomy_04p_version != "0.1.1") {
    stop(
      "Expected taxonomy 04p v0.1.1; detected ",
      taxonomy_04p_version, "."
    )
  }
  
  cat(
    "Integration 01 version: ",
    integration_01_version, "\n", sep = ""
  )
  
  cat(
    "Integration 02 version: ",
    integration_02_version, "\n", sep = ""
  )
  
  cat(
    "Taxonomy 04p version: ",
    taxonomy_04p_version, "\n\n", sep = ""
  )
  
  # ---------------------------------------------------------------------------
  # Identify PRESENT national occurrence population
  # ---------------------------------------------------------------------------
  
  clean_fields <- dbListFields(
    con,
    "occurrences_clean"
  )
  
  if (!"occurrenceStatus" %in% clean_fields) {
    stop(
      "occurrences_clean lacks occurrenceStatus. Available fields: ",
      paste(clean_fields, collapse = ", ")
    )
  }
  
  national_n <- dbGetQuery(
    con,
    "
    SELECT COUNT(*) AS n
    FROM occurrences_clean
    WHERE UPPER(TRIM(CAST(occurrenceStatus AS VARCHAR))) = 'PRESENT'
    "
  )$n[[1]]
  
  if (national_n != EXPECTED_NATIONAL_PRESENT) {
    stop(
      "Unexpected national PRESENT count: ",
      national_n,
      "; expected ",
      EXPECTED_NATIONAL_PRESENT, "."
    )
  }
  
  recovered_n <- dbGetQuery(
    con,
    "
    SELECT COUNT(*) AS n
    FROM recovered_institutional_occurrences_taxonomised
    "
  )$n[[1]]
  
  if (recovered_n != EXPECTED_RECOVERED) {
    stop(
      "Unexpected recovered count: ",
      recovered_n,
      "; expected ",
      EXPECTED_RECOVERED, "."
    )
  }
  
  combined_n <- national_n + recovered_n
  
  if (combined_n != EXPECTED_COMBINED_PRE_DEDUP) {
    stop(
      "Unexpected pre-deduplication combined count: ",
      combined_n, "."
    )
  }
  
  cat(
    "National PRESENT occurrences: ",
    format(national_n, big.mark = ","),
    "\n", sep = ""
  )
  
  cat(
    "Recovered institutional occurrences: ",
    format(recovered_n, big.mark = ","),
    "\n", sep = ""
  )
  
  cat(
    "Combined before duplicate assessment: ",
    format(combined_n, big.mark = ","),
    "\n\n", sep = ""
  )
  
  # ---------------------------------------------------------------------------
  # Field compatibility
  # ---------------------------------------------------------------------------
  
  recovered_fields <- dbListFields(
    con,
    "recovered_institutional_occurrences_taxonomised"
  )
  
  common_fields <- intersect(
    clean_fields,
    recovered_fields
  )
  
  national_only_fields <- setdiff(
    clean_fields,
    recovered_fields
  )
  
  recovered_only_fields <- setdiff(
    recovered_fields,
    clean_fields
  )
  
  field_summary <- tibble(
    metric = c(
      "national_fields",
      "recovered_fields",
      "common_fields",
      "national_only_fields",
      "recovered_only_fields"
    ),
    value = c(
      length(clean_fields),
      length(recovered_fields),
      length(common_fields),
      length(national_only_fields),
      length(recovered_only_fields)
    )
  )
  
  cat("— Field compatibility —\n\n")
  print(field_summary, n = Inf)
  
  cat("\nCommon fields:\n")
  cat(paste(common_fields, collapse = ", "), "\n\n")
  
  cat("Recovered-only fields:\n")
  cat(paste(recovered_only_fields, collapse = ", "), "\n\n")
  
  # ---------------------------------------------------------------------------
  # Recovered taxonomy profile
  # ---------------------------------------------------------------------------
  
  recovered_taxonomy <- dbGetQuery(
    con,
    "
    SELECT
      RECONCILIATION_STATUS,
      COUNT(*) AS occurrence_records,
      COUNT(DISTINCT FINAL_WCVP_ID) AS distinct_wcvp_ids
    FROM recovered_institutional_occurrences_taxonomised
    GROUP BY RECONCILIATION_STATUS
    ORDER BY occurrence_records DESC
    "
  ) %>%
    as_tibble()
  
  cat("— Recovered taxonomic status —\n\n")
  print(recovered_taxonomy, n = Inf)
  
  # ---------------------------------------------------------------------------
  # Coordinate/provenance field discovery
  # ---------------------------------------------------------------------------
  
  coordinate_candidates <- c(
    "decimalLatitude",
    "decimalLongitude",
    "coordinateUncertaintyInMeters",
    "analytical_decimalLatitude",
    "analytical_decimalLongitude",
    "analytical_coordinateUncertaintyInMeters",
    "ANALYTICAL_DECIMAL_LATITUDE",
    "ANALYTICAL_DECIMAL_LONGITUDE",
    "ANALYTICAL_COORDINATE_UNCERTAINTY_METERS"
  )
  
  coordinate_fields <- intersect(
    coordinate_candidates,
    recovered_fields
  )
  
  cat("\nRecovered coordinate-related fields detected:\n")
  cat(
    ifelse(
      length(coordinate_fields) == 0,
      "None detected",
      paste(coordinate_fields, collapse = ", ")
    ),
    "\n\n"
  )
  
  # ---------------------------------------------------------------------------
  # gbifID overlap
  # Integration 01 established zero overlap with national raw; confirm against
  # the actual national PRESENT analytical population used here.
  # ---------------------------------------------------------------------------
  
  if (
    !"gbifID" %in% clean_fields ||
    !"gbifID" %in% recovered_fields
  ) {
    stop(
      "gbifID must be present in both occurrence streams."
    )
  }
  
  gbif_overlap <- dbGetQuery(
    con,
    "
    SELECT COUNT(*) AS n
    FROM (
      SELECT DISTINCT CAST(r.gbifID AS VARCHAR) AS gbifID
      FROM recovered_institutional_occurrences_taxonomised r
      INNER JOIN occurrences_clean n
        ON CAST(r.gbifID AS VARCHAR) = CAST(n.gbifID AS VARCHAR)
      WHERE UPPER(TRIM(CAST(n.occurrenceStatus AS VARCHAR))) = 'PRESENT'
    ) x
    "
  )$n[[1]]
  
  cat(
    "Recovered gbifIDs already in national PRESENT layer: ",
    format(gbif_overlap, big.mark = ","),
    "\n\n", sep = ""
  )
  
  # ---------------------------------------------------------------------------
  # Determine fields available for cross-source duplicate diagnostics
  # ---------------------------------------------------------------------------
  
  duplicate_candidate_fields <- c(
    "occurrenceID",
    "catalogNumber",
    "recordNumber",
    "institutionCode",
    "collectionCode",
    "datasetKey",
    "scientificName",
    "eventDate",
    "year",
    "month",
    "day"
  )
  
  duplicate_fields <- intersect(
    duplicate_candidate_fields,
    common_fields
  )
  
  duplicate_field_profile <- tibble(
    field = duplicate_candidate_fields,
    available_in_both =
      duplicate_candidate_fields %in%
      duplicate_fields
  )
  
  cat("— Potential cross-source duplicate fields —\n\n")
  print(duplicate_field_profile, n = Inf)
  
  # ---------------------------------------------------------------------------
  # Exact occurrenceID overlap where occurrenceID exists in both streams
  # ---------------------------------------------------------------------------
  
  occurrence_id_overlap <- NA_integer_
  
  if ("occurrenceID" %in% duplicate_fields) {
    
    occurrence_id_overlap <- dbGetQuery(
      con,
      "
      SELECT COUNT(*) AS n
      FROM (
        SELECT DISTINCT CAST(r.occurrenceID AS VARCHAR) AS occurrenceID
        FROM recovered_institutional_occurrences_taxonomised r
        INNER JOIN occurrences_clean n
          ON CAST(r.occurrenceID AS VARCHAR) =
             CAST(n.occurrenceID AS VARCHAR)
        WHERE
          UPPER(TRIM(CAST(n.occurrenceStatus AS VARCHAR))) = 'PRESENT'
          AND r.occurrenceID IS NOT NULL
          AND TRIM(CAST(r.occurrenceID AS VARCHAR)) <> ''
          AND n.occurrenceID IS NOT NULL
          AND TRIM(CAST(n.occurrenceID AS VARCHAR)) <> ''
      ) x
      "
    )$n[[1]]
    
    cat(
      "\nExact nonblank occurrenceID overlap: ",
      format(occurrence_id_overlap, big.mark = ","),
      "\n", sep = ""
    )
    
  } else {
    
    cat(
      "\noccurrenceID not available in both streams; ",
      "exact occurrenceID overlap not calculated.\n"
    )
  }
  
  # ---------------------------------------------------------------------------
  # Exact catalog/institution overlap diagnostic
  # ---------------------------------------------------------------------------
  
  catalog_overlap <- NA_integer_
  
  catalog_required <- c(
    "institutionCode",
    "catalogNumber"
  )
  
  if (
    all(
      catalog_required %in%
      duplicate_fields
    )
  ) {
    
    catalog_overlap <- dbGetQuery(
      con,
      "
      SELECT COUNT(*) AS n
      FROM (
        SELECT DISTINCT
          UPPER(TRIM(CAST(r.institutionCode AS VARCHAR))) AS institutionCode,
          TRIM(CAST(r.catalogNumber AS VARCHAR)) AS catalogNumber
        FROM recovered_institutional_occurrences_taxonomised r
        INNER JOIN occurrences_clean n
          ON UPPER(TRIM(CAST(r.institutionCode AS VARCHAR))) =
             UPPER(TRIM(CAST(n.institutionCode AS VARCHAR)))
         AND TRIM(CAST(r.catalogNumber AS VARCHAR)) =
             TRIM(CAST(n.catalogNumber AS VARCHAR))
        WHERE
          UPPER(TRIM(CAST(n.occurrenceStatus AS VARCHAR))) = 'PRESENT'
          AND r.institutionCode IS NOT NULL
          AND TRIM(CAST(r.institutionCode AS VARCHAR)) <> ''
          AND r.catalogNumber IS NOT NULL
          AND TRIM(CAST(r.catalogNumber AS VARCHAR)) <> ''
          AND n.institutionCode IS NOT NULL
          AND TRIM(CAST(n.institutionCode AS VARCHAR)) <> ''
          AND n.catalogNumber IS NOT NULL
          AND TRIM(CAST(n.catalogNumber AS VARCHAR)) <> ''
      ) x
      "
    )$n[[1]]
    
    cat(
      "Exact institutionCode + catalogNumber overlap: ",
      format(catalog_overlap, big.mark = ","),
      "\n", sep = ""
    )
    
  } else {
    
    cat(
      "institutionCode + catalogNumber not available in both streams; ",
      "diagnostic not calculated.\n"
    )
  }
  
  # ---------------------------------------------------------------------------
  # Analytical duplicate risk classification
  # ---------------------------------------------------------------------------
  
  duplicate_risk <- case_when(
    gbif_overlap > 0 ~
      "confirmed_gbif_id_overlap",
    !is.na(occurrence_id_overlap) &&
      occurrence_id_overlap > 0 ~
      "possible_occurrence_id_overlap",
    !is.na(catalog_overlap) &&
      catalog_overlap > 0 ~
      "possible_catalog_overlap",
    TRUE ~
      "no_exact_cross_source_overlap_detected"
  )
  
  duplicate_summary <- tibble(
    diagnostic = c(
      "gbif_id_overlap",
      "occurrence_id_overlap",
      "institution_catalog_overlap"
    ),
    value = c(
      as.character(gbif_overlap),
      ifelse(
        is.na(occurrence_id_overlap),
        "not_calculated",
        as.character(occurrence_id_overlap)
      ),
      ifelse(
        is.na(catalog_overlap),
        "not_calculated",
        as.character(catalog_overlap)
      )
    )
  )
  
  cat("\n— Exact cross-source duplicate diagnostics —\n\n")
  print(duplicate_summary, n = Inf)
  
  cat(
    "\nDuplicate-risk classification: ",
    duplicate_risk, "\n", sep = ""
  )
  
  # ---------------------------------------------------------------------------
  # Metadata
  # ---------------------------------------------------------------------------
  
  metadata <- tibble(
    metric = c(
      "script_version",
      "input_integration_01_version",
      "input_integration_02_version",
      "input_taxonomy_04p_version",
      "national_present_records",
      "recovered_institutional_records",
      "combined_pre_dedup_records",
      "national_field_count",
      "recovered_field_count",
      "common_field_count",
      "gbif_id_overlap",
      "occurrence_id_overlap",
      "institution_catalog_overlap",
      "duplicate_risk_classification",
      "records_merged",
      "records_removed",
      "coordinates_modified",
      "taxonomy_assignments_made",
      "upstream_tables_modified"
    ),
    value = c(
      SCRIPT_VERSION,
      integration_01_version,
      integration_02_version,
      taxonomy_04p_version,
      as.character(national_n),
      as.character(recovered_n),
      as.character(combined_n),
      as.character(length(clean_fields)),
      as.character(length(recovered_fields)),
      as.character(length(common_fields)),
      as.character(gbif_overlap),
      ifelse(
        is.na(occurrence_id_overlap),
        "not_calculated",
        as.character(occurrence_id_overlap)
      ),
      ifelse(
        is.na(catalog_overlap),
        "not_calculated",
        as.character(catalog_overlap)
      ),
      duplicate_risk,
      "0",
      "0",
      "FALSE",
      "FALSE",
      "FALSE"
    )
  )
  
  # ---------------------------------------------------------------------------
  # Persist diagnostic tables only
  # ---------------------------------------------------------------------------
  
  dbWriteTable(
    con,
    "occurrence_integration_field_profile",
    field_summary,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "occurrence_integration_duplicate_field_profile",
    duplicate_field_profile,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "occurrence_integration_duplicate_summary",
    duplicate_summary,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "occurrence_integration_profile_metadata",
    metadata,
    overwrite = TRUE
  )
  
  out_dir <- here(
    "outputs",
    "tables",
    "integration",
    "occurrence_integration_profile"
  )
  
  dir.create(
    out_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  write_csv(
    field_summary,
    file.path(
      out_dir,
      "field_summary.csv"
    )
  )
  
  write_csv(
    duplicate_field_profile,
    file.path(
      out_dir,
      "duplicate_field_profile.csv"
    )
  )
  
  write_csv(
    duplicate_summary,
    file.path(
      out_dir,
      "duplicate_summary.csv"
    )
  )
  
  write_csv(
    recovered_taxonomy,
    file.path(
      out_dir,
      "recovered_taxonomy_summary.csv"
    )
  )
  
  write_csv(
    metadata,
    file.path(
      out_dir,
      "metadata.csv"
    )
  )
  
  # ---------------------------------------------------------------------------
  # Final report
  # ---------------------------------------------------------------------------
  
  cat("\n— Accounting —\n")
  cat(
    "National PRESENT records: ",
    format(national_n, big.mark = ","),
    "\n", sep = ""
  )
  cat(
    "Recovered institutional records: ",
    format(recovered_n, big.mark = ","),
    "\n", sep = ""
  )
  cat(
    "Combined pre-deduplication records: ",
    format(combined_n, big.mark = ","),
    "\n", sep = ""
  )
  
  cat("\n— Safety —\n")
  cat("Records merged: 0\n")
  cat("Records removed: 0\n")
  cat("Coordinates modified: FALSE\n")
  cat("Taxonomic assignments made: FALSE\n")
  cat("Frozen upstream tables modified: FALSE\n")
  cat("Output status: DIAGNOSTIC / PRE-INTEGRATION ONLY\n")
  
  cat(
    "\n03_profile_occurrence_integration.R v",
    SCRIPT_VERSION,
    " complete.\n",
    sep = ""
  )
  
  invisible(
    list(
      field_summary = field_summary,
      duplicate_field_profile =
        duplicate_field_profile,
      duplicate_summary =
        duplicate_summary,
      recovered_taxonomy =
        recovered_taxonomy,
      metadata = metadata
    )
  )
}

integration_03_profile <-
  run_integration_profile()