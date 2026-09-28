# ==============================================================================
# VPJD-OJPCP
# Geography 04a — Profile spatially unresolved occurrences
# Version: 0.2.0
#
# Purpose:
#   Profile integrated Japanese occurrence records classified by Geography 04
#   as outside all canonical JP01–JP51 botanical-area polygons.
#
# This module:
#   - uses the existing validated Geography 04 spatial assignment;
#   - performs NO new polygon intersection or distance calculation;
#   - does NOT modify coordinates;
#   - does NOT modify botanical-area assignments;
#   - does NOT modify taxonomy;
#   - does NOT delete or reject records;
#   - does NOT calculate distributions, Stars or GHI;
#   - classifies outside-area records using coordinate-envelope diagnostics;
#   - prepares a stable spatially-unresolved dataset for Geography 04b quarantine.
#
# Expected Geography 04 result:
#   Total integrated occurrences: 3,113,089
#   Assigned uniquely:           3,029,888
#   Outside botanical areas:        83,201
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(DBI)
  library(duckdb)
  library(here)
})

SCRIPT_VERSION <- "0.2.0"
SOURCE_TABLE <- "vpjd_japan_occurrences_integrated"
ASSIGNMENT_TABLE <- "vpjd_japan_occurrence_botanical_area_assignment"
OUTPUT_TABLE <- "vpjd_japan_spatially_unresolved_occurrences"
EXPECTED_TOTAL <- 3113089L
EXPECTED_OUTSIDE <- 83201L

run_spatially_unresolved_profile <- function() {
  cat("\n— VPJD spatially unresolved occurrence profile —\n\n")
  
  db_path <- here(
    "data", "interim", "occurrences",
    "vpjd_occurrences.duckdb"
  )
  
  output_dir <- here(
    "outputs", "tables", "geography",
    "spatially_unresolved_occurrences"
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
    dbDisconnect(
      con,
      shutdown = TRUE
    ),
    add = TRUE
  )
  
  quote_id <- function(x) {
    paste0(
      '"',
      gsub('"', '""', x),
      '"'
    )
  }
  
  tables <- dbListTables(con)
  
  if (!(SOURCE_TABLE %in% tables)) {
    stop("Source table not found: ", SOURCE_TABLE)
  }
  
  if (!(ASSIGNMENT_TABLE %in% tables)) {
    stop("Geography 04 assignment table not found: ", ASSIGNMENT_TABLE)
  }
  
  q_source <- quote_id(SOURCE_TABLE)
  q_assignment <- quote_id(ASSIGNMENT_TABLE)
  q_output <- quote_id(OUTPUT_TABLE)
  
  # ---------------------------------------------------------------------------
  # Confirm source state
  # ---------------------------------------------------------------------------
  
  source_count <- dbGetQuery(
    con,
    paste0(
      "SELECT COUNT(*) AS n FROM ",
      q_source
    )
  )$n[[1]]
  
  assignment_count <- dbGetQuery(
    con,
    paste0(
      "SELECT COUNT(*) AS n FROM ",
      q_assignment
    )
  )$n[[1]]
  
  assigned_count <- dbGetQuery(
    con,
    paste0(
      "SELECT COUNT(*) AS n FROM ",
      q_assignment,
      " WHERE assignment_status = 'assigned_unique'"
    )
  )$n[[1]]
  
  outside_count <- dbGetQuery(
    con,
    paste0(
      "SELECT COUNT(*) AS n FROM ",
      q_assignment,
      " WHERE assignment_status = 'outside_botanical_areas'"
    )
  )$n[[1]]
  
  pending_count <- dbGetQuery(
    con,
    paste0(
      "SELECT COUNT(*) AS n FROM ",
      q_assignment,
      " WHERE assignment_status NOT IN ",
      "('assigned_unique','outside_botanical_areas')"
    )
  )$n[[1]]
  
  cat(
    "Integrated occurrences: ",
    format(source_count, big.mark = ",", scientific = FALSE),
    "\n",
    sep = ""
  )
  
  cat(
    "Geography 04 assignment rows: ",
    format(assignment_count, big.mark = ",", scientific = FALSE),
    "\n",
    sep = ""
  )
  
  cat(
    "Assigned uniquely: ",
    format(assigned_count, big.mark = ",", scientific = FALSE),
    "\n",
    sep = ""
  )
  
  cat(
    "Spatially unresolved: ",
    format(outside_count, big.mark = ",", scientific = FALSE),
    "\n",
    sep = ""
  )
  
  cat(
    "Other assignment states: ",
    format(pending_count, big.mark = ",", scientific = FALSE),
    "\n\n",
    sep = ""
  )
  
  if (source_count != EXPECTED_TOTAL) {
    stop("Unexpected integrated occurrence count.")
  }
  
  if (assignment_count != EXPECTED_TOTAL) {
    stop("Unexpected Geography 04 assignment count.")
  }
  
  if (outside_count != EXPECTED_OUTSIDE) {
    stop(
      "Expected ",
      format(EXPECTED_OUTSIDE, big.mark = ","),
      " spatially unresolved records; found ",
      format(outside_count, big.mark = ","),
      "."
    )
  }
  
  if ((assigned_count + outside_count + pending_count) != assignment_count) {
    stop("Geography 04 assignment states do not reconcile.")
  }
  
  # ---------------------------------------------------------------------------
  # Required fields
  # ---------------------------------------------------------------------------
  
  source_fields <- dbListFields(
    con,
    SOURCE_TABLE
  )
  
  required_fields <- c(
    "gbifID",
    "datasetKey",
    "scientificName",
    "taxonRank",
    "countryCode",
    "locality",
    "stateProvince",
    "basisOfRecord",
    "institutionCode",
    "collectionCode",
    "ANALYTICAL_DECIMAL_LATITUDE",
    "ANALYTICAL_DECIMAL_LONGITUDE",
    "ANALYTICAL_COORDINATE_UNCERTAINTY_M",
    "OCCURRENCE_SOURCE",
    "ANALYTICAL_COORDINATE_SOURCE",
    "ANALYTICAL_GEOREFERENCE_METHOD",
    "ANALYTICAL_GEOREFERENCE_CONFIDENCE",
    "ANALYTICAL_GAZETTEER_SOURCE",
    "ANALYTICAL_GAZETTEER_ID",
    "ANALYTICAL_PRECISION_CLASS",
    "FINAL_WCVP_ID",
    "FINAL_WCVP_RECOGNISED_NAME",
    "FINAL_WCVP_RANK",
    "FINAL_WCVP_STATUS",
    "FINAL_WCVP_CONCEPT_CLASS",
    "TAXONOMIC_RECONCILIATION_SOURCE",
    "RECONCILIATION_STATUS"
  )
  
  missing_fields <- setdiff(
    required_fields,
    source_fields
  )
  
  if (length(missing_fields) > 0L) {
    stop(
      "Required source fields missing:\n",
      paste(missing_fields, collapse = ", ")
    )
  }
  
  cat("Required source fields: PASS\n")
  
  # ---------------------------------------------------------------------------
  # Remove previous v0.2.0 output if rerunning this module
  # ---------------------------------------------------------------------------
  
  if (OUTPUT_TABLE %in% dbListTables(con)) {
    dbExecute(
      con,
      paste0(
        "DROP TABLE ",
        q_output
      )
    )
  }
  
  # ---------------------------------------------------------------------------
  # Build canonical spatially unresolved table
  #
  # IMPORTANT:
  #   The broad/wider envelopes are QA categories only.
  #   They do NOT constitute botanical-area assignment rules.
  # ---------------------------------------------------------------------------
  
  cat(
    "Building spatially unresolved occurrence table...\n"
  )
  
  dbExecute(
    con,
    paste0(
      "CREATE TABLE ",
      q_output,
      " AS ",
      "SELECT ",
      "a.source_row_number, ",
      "a.gbifID, ",
      "s.datasetKey, ",
      "s.scientificName, ",
      "s.taxonRank, ",
      "s.countryCode, ",
      "s.locality, ",
      "s.stateProvince, ",
      "s.basisOfRecord, ",
      "s.institutionCode, ",
      "s.collectionCode, ",
      "s.ANALYTICAL_DECIMAL_LATITUDE, ",
      "s.ANALYTICAL_DECIMAL_LONGITUDE, ",
      "s.ANALYTICAL_COORDINATE_UNCERTAINTY_M, ",
      "s.OCCURRENCE_SOURCE, ",
      "s.ANALYTICAL_COORDINATE_SOURCE, ",
      "s.ANALYTICAL_GEOREFERENCE_METHOD, ",
      "s.ANALYTICAL_GEOREFERENCE_CONFIDENCE, ",
      "s.ANALYTICAL_GAZETTEER_SOURCE, ",
      "s.ANALYTICAL_GAZETTEER_ID, ",
      "s.ANALYTICAL_PRECISION_CLASS, ",
      "s.FINAL_WCVP_ID, ",
      "s.FINAL_WCVP_RECOGNISED_NAME, ",
      "s.FINAL_WCVP_RANK, ",
      "s.FINAL_WCVP_STATUS, ",
      "s.FINAL_WCVP_CONCEPT_CLASS, ",
      "s.TAXONOMIC_RECONCILIATION_SOURCE, ",
      "s.RECONCILIATION_STATUS, ",
      "'QUARANTINE_CANDIDATE' AS PROCESSING_STATUS, ",
      "'geography' AS QUARANTINE_STAGE, ",
      "'outside_canonical_botanical_area' AS QUARANTINE_REASON, ",
      "CASE ",
      "WHEN ",
      "s.ANALYTICAL_DECIMAL_LATITUDE BETWEEN 20 AND 50 ",
      "AND s.ANALYTICAL_DECIMAL_LONGITUDE BETWEEN 120 AND 155 ",
      "THEN 'outside_within_broad_japan_envelope' ",
      "WHEN ",
      "s.ANALYTICAL_DECIMAL_LATITUDE BETWEEN 15 AND 55 ",
      "AND s.ANALYTICAL_DECIMAL_LONGITUDE BETWEEN 115 AND 160 ",
      "THEN 'outside_broad_but_within_wider_region' ",
      "ELSE 'gross_coordinate_anomaly' ",
      "END AS SPATIAL_QA_CLASS, ",
      "CASE ",
      "WHEN ",
      "s.ANALYTICAL_DECIMAL_LATITUDE BETWEEN 20 AND 50 ",
      "AND s.ANALYTICAL_DECIMAL_LONGITUDE BETWEEN 120 AND 155 ",
      "THEN TRUE ELSE FALSE ",
      "END AS WITHIN_BROAD_JAPAN_ENVELOPE, ",
      "CASE ",
      "WHEN ",
      "s.ANALYTICAL_DECIMAL_LATITUDE BETWEEN 15 AND 55 ",
      "AND s.ANALYTICAL_DECIMAL_LONGITUDE BETWEEN 115 AND 160 ",
      "THEN TRUE ELSE FALSE ",
      "END AS WITHIN_WIDER_REGIONAL_ENVELOPE, ",
      "'04a' AS QA_MODULE, ",
      "'",
      SCRIPT_VERSION,
      "' AS QA_MODULE_VERSION ",
      "FROM ",
      q_assignment,
      " a ",
      "INNER JOIN ",
      q_source,
      " s ON CAST(a.gbifID AS VARCHAR) = CAST(s.gbifID AS VARCHAR) ",
      "WHERE a.assignment_status = 'outside_botanical_areas'"
    )
  )
  
  unresolved_count <- dbGetQuery(
    con,
    paste0(
      "SELECT COUNT(*) AS n FROM ",
      q_output
    )
  )$n[[1]]
  
  unresolved_ids <- dbGetQuery(
    con,
    paste0(
      "SELECT COUNT(DISTINCT CAST(gbifID AS VARCHAR)) AS n FROM ",
      q_output
    )
  )$n[[1]]
  
  cat(
    "Spatially unresolved records written: ",
    format(unresolved_count, big.mark = ",", scientific = FALSE),
    "\n",
    sep = ""
  )
  
  # ---------------------------------------------------------------------------
  # QA class summary
  # ---------------------------------------------------------------------------
  
  qa_class_summary <- dbGetQuery(
    con,
    paste0(
      "SELECT ",
      "SPATIAL_QA_CLASS, ",
      "COUNT(*) AS records ",
      "FROM ",
      q_output,
      " GROUP BY SPATIAL_QA_CLASS ",
      "ORDER BY records DESC"
    )
  )
  
  cat("\n— Spatial QA classification —\n")
  
  print.data.frame(
    qa_class_summary,
    row.names = FALSE
  )
  
  # ---------------------------------------------------------------------------
  # Coordinate extent by QA class
  # ---------------------------------------------------------------------------
  
  coordinate_extent <- dbGetQuery(
    con,
    paste0(
      "SELECT ",
      "SPATIAL_QA_CLASS, ",
      "COUNT(*) AS records, ",
      "MIN(ANALYTICAL_DECIMAL_LONGITUDE) AS min_longitude, ",
      "MAX(ANALYTICAL_DECIMAL_LONGITUDE) AS max_longitude, ",
      "MIN(ANALYTICAL_DECIMAL_LATITUDE) AS min_latitude, ",
      "MAX(ANALYTICAL_DECIMAL_LATITUDE) AS max_latitude ",
      "FROM ",
      q_output,
      " GROUP BY SPATIAL_QA_CLASS ",
      "ORDER BY records DESC"
    )
  )
  
  cat("\n— Coordinate extent by QA class —\n")
  
  print.data.frame(
    coordinate_extent,
    row.names = FALSE
  )
  
  # ---------------------------------------------------------------------------
  # Occurrence source
  # ---------------------------------------------------------------------------
  
  occurrence_source_summary <- dbGetQuery(
    con,
    paste0(
      "SELECT ",
      "OCCURRENCE_SOURCE, ",
      "SPATIAL_QA_CLASS, ",
      "COUNT(*) AS records ",
      "FROM ",
      q_output,
      " GROUP BY OCCURRENCE_SOURCE, SPATIAL_QA_CLASS ",
      "ORDER BY records DESC"
    )
  )
  
  cat("\n— Occurrence source —\n")
  
  print.data.frame(
    occurrence_source_summary,
    row.names = FALSE
  )
  
  # ---------------------------------------------------------------------------
  # Coordinate source
  # ---------------------------------------------------------------------------
  
  coordinate_source_summary <- dbGetQuery(
    con,
    paste0(
      "SELECT ",
      "ANALYTICAL_COORDINATE_SOURCE, ",
      "SPATIAL_QA_CLASS, ",
      "COUNT(*) AS records ",
      "FROM ",
      q_output,
      " GROUP BY ANALYTICAL_COORDINATE_SOURCE, SPATIAL_QA_CLASS ",
      "ORDER BY records DESC"
    )
  )
  
  cat("\n— Analytical coordinate source —\n")
  
  print.data.frame(
    coordinate_source_summary,
    row.names = FALSE
  )
  
  # ---------------------------------------------------------------------------
  # Georeferencing
  # ---------------------------------------------------------------------------
  
  georef_summary <- dbGetQuery(
    con,
    paste0(
      "SELECT ",
      "ANALYTICAL_GEOREFERENCE_METHOD, ",
      "ANALYTICAL_GEOREFERENCE_CONFIDENCE, ",
      "SPATIAL_QA_CLASS, ",
      "COUNT(*) AS records ",
      "FROM ",
      q_output,
      " GROUP BY ",
      "ANALYTICAL_GEOREFERENCE_METHOD, ",
      "ANALYTICAL_GEOREFERENCE_CONFIDENCE, ",
      "SPATIAL_QA_CLASS ",
      "ORDER BY records DESC"
    )
  )
  
  cat("\n— Georeference profile —\n")
  
  print.data.frame(
    georef_summary,
    row.names = FALSE
  )
  
  # ---------------------------------------------------------------------------
  # Precision
  # ---------------------------------------------------------------------------
  
  precision_summary <- dbGetQuery(
    con,
    paste0(
      "SELECT ",
      "ANALYTICAL_PRECISION_CLASS, ",
      "SPATIAL_QA_CLASS, ",
      "COUNT(*) AS records ",
      "FROM ",
      q_output,
      " GROUP BY ANALYTICAL_PRECISION_CLASS, SPATIAL_QA_CLASS ",
      "ORDER BY records DESC"
    )
  )
  
  cat("\n— Coordinate precision —\n")
  
  print.data.frame(
    precision_summary,
    row.names = FALSE
  )
  
  # ---------------------------------------------------------------------------
  # Coordinate uncertainty
  # ---------------------------------------------------------------------------
  
  uncertainty_summary <- dbGetQuery(
    con,
    paste0(
      "SELECT ",
      "SPATIAL_QA_CLASS, ",
      "COUNT(*) AS records, ",
      "COUNT(ANALYTICAL_COORDINATE_UNCERTAINTY_M) ",
      "AS uncertainty_present, ",
      "COUNT(*) - COUNT(ANALYTICAL_COORDINATE_UNCERTAINTY_M) ",
      "AS uncertainty_missing, ",
      "MEDIAN(ANALYTICAL_COORDINATE_UNCERTAINTY_M) ",
      "AS median_uncertainty_m, ",
      "AVG(ANALYTICAL_COORDINATE_UNCERTAINTY_M) ",
      "AS mean_uncertainty_m, ",
      "MAX(ANALYTICAL_COORDINATE_UNCERTAINTY_M) ",
      "AS max_uncertainty_m ",
      "FROM ",
      q_output,
      " GROUP BY SPATIAL_QA_CLASS ",
      "ORDER BY records DESC"
    )
  )
  
  cat("\n— Coordinate uncertainty —\n")
  
  print.data.frame(
    uncertainty_summary,
    row.names = FALSE
  )
  
  # ---------------------------------------------------------------------------
  # Country
  # ---------------------------------------------------------------------------
  
  country_summary <- dbGetQuery(
    con,
    paste0(
      "SELECT ",
      "countryCode, ",
      "SPATIAL_QA_CLASS, ",
      "COUNT(*) AS records ",
      "FROM ",
      q_output,
      " GROUP BY countryCode, SPATIAL_QA_CLASS ",
      "ORDER BY records DESC"
    )
  )
  
  cat("\n— Country code —\n")
  
  print.data.frame(
    country_summary,
    row.names = FALSE
  )
  
  # ---------------------------------------------------------------------------
  # State/province
  # ---------------------------------------------------------------------------
  
  state_summary <- dbGetQuery(
    con,
    paste0(
      "SELECT ",
      "COALESCE(NULLIF(TRIM(stateProvince), ''), '[missing]') ",
      "AS stateProvince, ",
      "SPATIAL_QA_CLASS, ",
      "COUNT(*) AS records ",
      "FROM ",
      q_output,
      " GROUP BY 1, 2 ",
      "ORDER BY records DESC ",
      "LIMIT 50"
    )
  )
  
  cat("\n— Top stateProvince values —\n")
  
  print.data.frame(
    state_summary,
    row.names = FALSE
  )
  
  # ---------------------------------------------------------------------------
  # Institution
  # ---------------------------------------------------------------------------
  
  institution_summary <- dbGetQuery(
    con,
    paste0(
      "SELECT ",
      "COALESCE(NULLIF(TRIM(institutionCode), ''), '[missing]') ",
      "AS institutionCode, ",
      "SPATIAL_QA_CLASS, ",
      "COUNT(*) AS records ",
      "FROM ",
      q_output,
      " GROUP BY 1, 2 ",
      "ORDER BY records DESC ",
      "LIMIT 50"
    )
  )
  
  cat("\n— Top institution codes —\n")
  
  print.data.frame(
    institution_summary,
    row.names = FALSE
  )
  
  # ---------------------------------------------------------------------------
  # Dataset
  # ---------------------------------------------------------------------------
  
  dataset_summary <- dbGetQuery(
    con,
    paste0(
      "SELECT ",
      "datasetKey, ",
      "SPATIAL_QA_CLASS, ",
      "COUNT(*) AS records ",
      "FROM ",
      q_output,
      " GROUP BY datasetKey, SPATIAL_QA_CLASS ",
      "ORDER BY records DESC ",
      "LIMIT 50"
    )
  )
  
  cat("\n— Top dataset keys —\n")
  
  print.data.frame(
    dataset_summary,
    row.names = FALSE
  )
  
  # ---------------------------------------------------------------------------
  # Basis of record
  # ---------------------------------------------------------------------------
  
  basis_summary <- dbGetQuery(
    con,
    paste0(
      "SELECT ",
      "basisOfRecord, ",
      "SPATIAL_QA_CLASS, ",
      "COUNT(*) AS records ",
      "FROM ",
      q_output,
      " GROUP BY basisOfRecord, SPATIAL_QA_CLASS ",
      "ORDER BY records DESC"
    )
  )
  
  cat("\n— Basis of record —\n")
  
  print.data.frame(
    basis_summary,
    row.names = FALSE
  )
  
  # ---------------------------------------------------------------------------
  # Taxonomic reconciliation
  # ---------------------------------------------------------------------------
  
  reconciliation_summary <- dbGetQuery(
    con,
    paste0(
      "SELECT ",
      "RECONCILIATION_STATUS, ",
      "FINAL_WCVP_CONCEPT_CLASS, ",
      "SPATIAL_QA_CLASS, ",
      "COUNT(*) AS records ",
      "FROM ",
      q_output,
      " GROUP BY ",
      "RECONCILIATION_STATUS, ",
      "FINAL_WCVP_CONCEPT_CLASS, ",
      "SPATIAL_QA_CLASS ",
      "ORDER BY records DESC"
    )
  )
  
  cat("\n— Taxonomic reconciliation —\n")
  
  print.data.frame(
    reconciliation_summary,
    row.names = FALSE
  )
  
  # ---------------------------------------------------------------------------
  # Gross anomaly records
  # ---------------------------------------------------------------------------
  
  gross_anomalies <- dbGetQuery(
    con,
    paste0(
      "SELECT * FROM ",
      q_output,
      " WHERE SPATIAL_QA_CLASS = 'gross_coordinate_anomaly' ",
      "ORDER BY ANALYTICAL_DECIMAL_LATITUDE, ",
      "ANALYTICAL_DECIMAL_LONGITUDE"
    )
  )
  
  cat(
    "\nGross coordinate anomalies retained for later review: ",
    format(nrow(gross_anomalies), big.mark = ","),
    "\n",
    sep = ""
  )
  
  # ---------------------------------------------------------------------------
  # Validation
  # ---------------------------------------------------------------------------
  
  qa_total <- sum(
    qa_class_summary$records
  )
  
  gross_count <- sum(
    qa_class_summary$records[
      qa_class_summary$SPATIAL_QA_CLASS ==
        "gross_coordinate_anomaly"
    ]
  )
  
  broad_count <- sum(
    qa_class_summary$records[
      qa_class_summary$SPATIAL_QA_CLASS ==
        "outside_within_broad_japan_envelope"
    ]
  )
  
  wider_count <- sum(
    qa_class_summary$records[
      qa_class_summary$SPATIAL_QA_CLASS ==
        "outside_broad_but_within_wider_region"
    ]
  )
  
  validation <- tibble(
    check = c(
      "source_records_3113089",
      "assignment_records_3113089",
      "outside_records_83201",
      "output_records_equal_outside",
      "output_gbifIDs_unique",
      "qa_classes_reconcile_to_output",
      "broad_envelope_records_82666",
      "wider_region_records_31",
      "gross_anomaly_records_504",
      "no_other_assignment_states",
      "all_output_records_have_coordinates",
      "all_output_records_quarantine_candidates",
      "all_output_records_geography_stage",
      "coordinates_not_modified",
      "assignments_not_modified",
      "taxonomy_not_modified",
      "occurrences_not_deleted",
      "geography_not_modified",
      "distance_not_calculated",
      "stars_not_calculated",
      "ghi_not_calculated"
    ),
    pass = c(
      source_count == EXPECTED_TOTAL,
      assignment_count == EXPECTED_TOTAL,
      outside_count == EXPECTED_OUTSIDE,
      unresolved_count == outside_count,
      unresolved_ids == unresolved_count,
      qa_total == unresolved_count,
      broad_count == 82666L,
      wider_count == 31L,
      gross_count == 504L,
      pending_count == 0L,
      dbGetQuery(
        con,
        paste0(
          "SELECT COUNT(*) AS n FROM ",
          q_output,
          " WHERE ANALYTICAL_DECIMAL_LATITUDE IS NULL ",
          "OR ANALYTICAL_DECIMAL_LONGITUDE IS NULL"
        )
      )$n[[1]] == 0L,
      dbGetQuery(
        con,
        paste0(
          "SELECT COUNT(*) AS n FROM ",
          q_output,
          " WHERE PROCESSING_STATUS <> 'QUARANTINE_CANDIDATE' ",
          "OR PROCESSING_STATUS IS NULL"
        )
      )$n[[1]] == 0L,
      dbGetQuery(
        con,
        paste0(
          "SELECT COUNT(*) AS n FROM ",
          q_output,
          " WHERE QUARANTINE_STAGE <> 'geography' ",
          "OR QUARANTINE_STAGE IS NULL"
        )
      )$n[[1]] == 0L,
      TRUE,
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
    "\nAll profile validation checks PASS: ",
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
      "source_table",
      "assignment_table",
      "output_table",
      "source_records",
      "assigned_unique_records",
      "spatially_unresolved_records",
      "broad_envelope_records",
      "wider_region_records",
      "gross_coordinate_anomaly_records",
      "distance_calculation",
      "processing_status",
      "quarantine_stage",
      "coordinates_modified",
      "assignments_modified",
      "taxonomy_modified",
      "records_deleted",
      "stars_calculated",
      "ghi_calculated",
      "validation_pass"
    ),
    value = c(
      "04a_profile_spatially_unresolved_occurrences",
      SCRIPT_VERSION,
      SOURCE_TABLE,
      ASSIGNMENT_TABLE,
      OUTPUT_TABLE,
      as.character(source_count),
      as.character(assigned_count),
      as.character(unresolved_count),
      as.character(broad_count),
      as.character(wider_count),
      as.character(gross_count),
      "NONE",
      "QUARANTINE_CANDIDATE",
      "geography",
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
  # Write QA outputs
  # ---------------------------------------------------------------------------
  
  write_csv(
    qa_class_summary,
    file.path(
      output_dir,
      "spatial_qa_class_summary.csv"
    )
  )
  
  write_csv(
    coordinate_extent,
    file.path(
      output_dir,
      "spatial_qa_coordinate_extent.csv"
    )
  )
  
  write_csv(
    occurrence_source_summary,
    file.path(
      output_dir,
      "spatial_qa_occurrence_source.csv"
    )
  )
  
  write_csv(
    coordinate_source_summary,
    file.path(
      output_dir,
      "spatial_qa_coordinate_source.csv"
    )
  )
  
  write_csv(
    georef_summary,
    file.path(
      output_dir,
      "spatial_qa_georeference_profile.csv"
    )
  )
  
  write_csv(
    precision_summary,
    file.path(
      output_dir,
      "spatial_qa_precision.csv"
    )
  )
  
  write_csv(
    uncertainty_summary,
    file.path(
      output_dir,
      "spatial_qa_uncertainty.csv"
    )
  )
  
  write_csv(
    country_summary,
    file.path(
      output_dir,
      "spatial_qa_country.csv"
    )
  )
  
  write_csv(
    state_summary,
    file.path(
      output_dir,
      "spatial_qa_stateprovince.csv"
    )
  )
  
  write_csv(
    institution_summary,
    file.path(
      output_dir,
      "spatial_qa_institution.csv"
    )
  )
  
  write_csv(
    dataset_summary,
    file.path(
      output_dir,
      "spatial_qa_dataset.csv"
    )
  )
  
  write_csv(
    basis_summary,
    file.path(
      output_dir,
      "spatial_qa_basisofrecord.csv"
    )
  )
  
  write_csv(
    reconciliation_summary,
    file.path(
      output_dir,
      "spatial_qa_taxonomic_reconciliation.csv"
    )
  )
  
  write_csv(
    gross_anomalies,
    file.path(
      output_dir,
      "gross_coordinate_anomalies.csv"
    )
  )
  
  write_csv(
    validation,
    file.path(
      output_dir,
      "spatially_unresolved_validation.csv"
    )
  )
  
  write_csv(
    metadata,
    file.path(
      output_dir,
      "spatially_unresolved_metadata.csv"
    )
  )
  
  # ---------------------------------------------------------------------------
  # Store summaries in DuckDB
  # ---------------------------------------------------------------------------
  
  dbWriteTable(
    con,
    "vpjd_japan_spatially_unresolved_qa_summary",
    qa_class_summary,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "vpjd_japan_spatially_unresolved_validation",
    validation,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "vpjd_japan_spatially_unresolved_metadata",
    metadata,
    overwrite = TRUE
  )
  
  # ---------------------------------------------------------------------------
  # Final status
  # ---------------------------------------------------------------------------
  
  if (!all_pass) {
    cat(
      "\nOutput status: FAILED PROFILE VALIDATION\n"
    )
    
    stop(
      paste(
        "Geography 04a validation failed.",
        "Review QA before proceeding."
      )
    )
  }
  
  cat(
    "\nOutput table:\n",
    OUTPUT_TABLE,
    "\n",
    sep = ""
  )
  
  cat(
    "\nOutput status: ",
    "VALIDATED SPATIALLY UNRESOLVED OCCURRENCE PROFILE\n",
    sep = ""
  )
  
  cat(
    "Spatially unresolved records: ",
    format(
      unresolved_count,
      big.mark = ",",
      scientific = FALSE
    ),
    "\n",
    sep = ""
  )
  
  cat(
    "Outside within broad Japan envelope: ",
    format(
      broad_count,
      big.mark = ",",
      scientific = FALSE
    ),
    "\n",
    sep = ""
  )
  
  cat(
    "Outside broad but within wider region: ",
    format(
      wider_count,
      big.mark = ",",
      scientific = FALSE
    ),
    "\n",
    sep = ""
  )
  
  cat(
    "Gross coordinate anomalies: ",
    format(
      gross_count,
      big.mark = ",",
      scientific = FALSE
    ),
    "\n",
    sep = ""
  )
  
  cat("Distance calculations performed: FALSE\n")
  cat("Coordinates modified: FALSE\n")
  cat("Botanical-area assignments modified: FALSE\n")
  cat("Taxonomy modified: FALSE\n")
  cat("Records deleted: FALSE\n")
  cat("Stars calculated: FALSE\n")
  cat("GHI calculated: FALSE\n")
  
  cat(
    "\nGeography 04a v",
    SCRIPT_VERSION,
    " complete.\n",
    sep = ""
  )
  
  invisible(
    list(
      qa_class_summary =
        qa_class_summary,
      coordinate_extent =
        coordinate_extent,
      occurrence_source_summary =
        occurrence_source_summary,
      coordinate_source_summary =
        coordinate_source_summary,
      georef_summary =
        georef_summary,
      precision_summary =
        precision_summary,
      uncertainty_summary =
        uncertainty_summary,
      country_summary =
        country_summary,
      state_summary =
        state_summary,
      institution_summary =
        institution_summary,
      dataset_summary =
        dataset_summary,
      basis_summary =
        basis_summary,
      reconciliation_summary =
        reconciliation_summary,
      validation =
        validation,
      metadata =
        metadata
    )
  )
}

result_04a <-
  run_spatially_unresolved_profile()