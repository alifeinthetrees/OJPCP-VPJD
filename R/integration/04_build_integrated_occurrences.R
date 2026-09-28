# ==============================================================================
# VPJD-OJPCP
# 04_build_integrated_occurrences.R
# Version: 0.1.0
#
# Purpose:
#   Construct the canonical integrated VPJD Japan occurrence layer from:
#     1. National GBIF PRESENT occurrences.
#     2. Gazetteer-recovered institutional occurrences.
#
# Design:
#   - One row per source occurrence.
#   - Preserve gbifID and source provenance.
#   - National records use original GBIF coordinates.
#   - Recovered records use validated analytical gazetteer coordinates.
#   - Recovered taxonomic decisions come from Integration 02.
#   - National taxonomy comes from frozen 04p.
#   - Unresolved records are retained.
#   - No speculative cross-source deduplication.
#   - No new taxonomic assignments.
#
# Required frozen inputs:
#   Cleaning 03 v0.1.2
#   Taxonomy 04p v0.1.1
#   Integration 01 v0.1.0
#   Integration 02 v0.1.4
#   Integration 03 v0.1.0
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
EXPECTED_TOTAL <- 3113089L

get_metadata_value <- function(con, table_name, metric_name) {
  fields <- dbListFields(con, table_name)
  
  if (all(c("metric", "value") %in% fields)) {
    sql <- paste0(
      "SELECT CAST(value AS VARCHAR) AS value FROM ",
      dbQuoteIdentifier(con, table_name),
      " WHERE CAST(metric AS VARCHAR) = ?"
    )
    x <- dbGetQuery(con, sql, params = list(metric_name))
    
    if (nrow(x) != 1) {
      stop(
        "Expected one '", metric_name,
        "' row in ", table_name,
        "; found ", nrow(x), "."
      )
    }
    
    return(as.character(x$value[[1]]))
  }
  
  if (metric_name %in% fields) {
    x <- dbReadTable(con, table_name)
    
    if (nrow(x) != 1) {
      stop("Expected one row in ", table_name, ".")
    }
    
    return(as.character(x[[metric_name]][[1]]))
  }
  
  stop(
    "Metadata item '", metric_name,
    "' not found in ", table_name, "."
  )
}

run_integrated_occurrence_build <- function() {
  
  cat("\n— VPJD integrated occurrence build —\n\n")
  
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
    "occurrence_wcvp_consolidated",
    "occurrence_wcvp_consolidation_metadata",
    "recovered_institutional_occurrences_taxonomised",
    "recovered_institutional_occurrence_metadata",
    "recovered_institutional_taxon_integration_metadata",
    "occurrence_integration_profile_metadata"
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
  
  integration_03_version <- get_metadata_value(
    con,
    "occurrence_integration_profile_metadata",
    "script_version"
  )
  
  taxonomy_version <- get_metadata_value(
    con,
    "occurrence_wcvp_consolidation_metadata",
    "script_version"
  )
  
  if (integration_01_version != "0.1.0") {
    stop("Expected Integration 01 v0.1.0.")
  }
  
  if (integration_02_version != "0.1.4") {
    stop("Expected Integration 02 v0.1.4.")
  }
  
  if (integration_03_version != "0.1.0") {
    stop("Expected Integration 03 v0.1.0.")
  }
  
  if (taxonomy_version != "0.1.1") {
    stop("Expected taxonomy 04p v0.1.1.")
  }
  
  cat(
    "Integration 01: ", integration_01_version, "\n",
    "Integration 02: ", integration_02_version, "\n",
    "Integration 03: ", integration_03_version, "\n",
    "Taxonomy 04p: ", taxonomy_version, "\n\n",
    sep = ""
  )
  
  # ---------------------------------------------------------------------------
  # Validate required fields
  # ---------------------------------------------------------------------------
  
  national_fields <- dbListFields(
    con,
    "occurrences_clean"
  )
  
  recovered_fields <- dbListFields(
    con,
    "recovered_institutional_occurrences_taxonomised"
  )
  
  taxonomy_fields <- dbListFields(
    con,
    "occurrence_wcvp_consolidated"
  )
  
  required_national <- c(
    "gbifID",
    "occurrenceStatus",
    "scientificName",
    "taxonRank",
    "countryCode",
    "locality",
    "stateProvince",
    "decimalLatitude",
    "decimalLongitude",
    "coordinateUncertaintyInMeters",
    "basisOfRecord",
    "institutionCode",
    "collectionCode"
  )
  
  required_recovered <- c(
    "gbifID",
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
    "RECONCILIATION_METHOD",
    "RECONCILIATION_STATUS"
  )
  
  required_taxonomy <- c(
    "taxonKey",
    "scientificName",
    "final_wcvp_accepted_plant_name_id",
    "final_wcvp_taxon_name",
    "final_wcvp_taxon_rank",
    "final_wcvp_taxon_status",
    "vpjd_taxonomic_status"
  )
  
  missing_national <- setdiff(
    required_national,
    national_fields
  )
  
  missing_recovered <- setdiff(
    required_recovered,
    recovered_fields
  )
  
  missing_taxonomy <- setdiff(
    required_taxonomy,
    taxonomy_fields
  )
  
  if (length(missing_national) > 0) {
    stop(
      "National layer missing: ",
      paste(missing_national, collapse = ", ")
    )
  }
  
  if (length(missing_recovered) > 0) {
    stop(
      "Recovered layer missing: ",
      paste(missing_recovered, collapse = ", ")
    )
  }
  
  if (length(missing_taxonomy) > 0) {
    stop(
      "04p taxonomy missing: ",
      paste(missing_taxonomy, collapse = ", ")
    )
  }
  
  cat("Required-field validation: PASS\n\n")
  
  # ---------------------------------------------------------------------------
  # National PRESENT population
  # ---------------------------------------------------------------------------
  
  national_n <- dbGetQuery(
    con,
    "
    SELECT COUNT(*) AS n
    FROM occurrences_clean
    WHERE UPPER(TRIM(CAST(occurrenceStatus AS VARCHAR))) = 'PRESENT'
    "
  )$n[[1]]
  
  recovered_n <- dbGetQuery(
    con,
    "
    SELECT COUNT(*) AS n
    FROM recovered_institutional_occurrences_taxonomised
    "
  )$n[[1]]
  
  if (national_n != EXPECTED_NATIONAL_PRESENT) {
    stop(
      "Unexpected national PRESENT count: ",
      national_n, "."
    )
  }
  
  if (recovered_n != EXPECTED_RECOVERED) {
    stop(
      "Unexpected recovered count: ",
      recovered_n, "."
    )
  }
  
  # ---------------------------------------------------------------------------
  # Determine national taxonomy join behaviour before constructing table
  #
  # Frozen 04p is keyed primarily by GBIF taxonKey. Confirm uniqueness so the
  # join cannot multiply occurrence rows.
  # ---------------------------------------------------------------------------
  
  taxonomy_key_profile <- dbGetQuery(
    con,
    "
    SELECT
      COUNT(*) AS source_concepts,
      COUNT(DISTINCT CAST(taxonKey AS VARCHAR)) AS distinct_taxon_keys
    FROM occurrence_wcvp_consolidated
    WHERE taxonKey IS NOT NULL
    "
  )
  
  duplicate_taxon_keys <- dbGetQuery(
    con,
    "
    SELECT COUNT(*) AS n
    FROM (
      SELECT CAST(taxonKey AS VARCHAR) AS taxonKey
      FROM occurrence_wcvp_consolidated
      WHERE taxonKey IS NOT NULL
      GROUP BY CAST(taxonKey AS VARCHAR)
      HAVING COUNT(*) > 1
    ) x
    "
  )$n[[1]]
  
  cat("— National taxonomy join profile —\n\n")
  print(as_tibble(taxonomy_key_profile), n = Inf)
  
  cat(
    "Duplicate taxonKeys in frozen 04p: ",
    format(duplicate_taxon_keys, big.mark = ","),
    "\n\n", sep = ""
  )
  
  if (duplicate_taxon_keys > 0) {
    stop(
      "04p taxonKey is not unique. ",
      "Integration halted before constructing the occurrence layer."
    )
  }
  
  # ---------------------------------------------------------------------------
  # Create national analytical stream
  #
  # National GBIF coordinates remain the analytical coordinates.
  # ---------------------------------------------------------------------------
  
  dbExecute(
    con,
    "
    CREATE OR REPLACE TEMP TABLE integration04_national AS
    SELECT
      CAST(n.gbifID AS VARCHAR) AS gbifID,
      CAST(n.datasetKey AS VARCHAR) AS datasetKey,
      CAST(n.scientificName AS VARCHAR) AS scientificName,
      CAST(n.taxonRank AS VARCHAR) AS taxonRank,
      CAST(n.countryCode AS VARCHAR) AS countryCode,
      CAST(n.locality AS VARCHAR) AS locality,
      CAST(n.stateProvince AS VARCHAR) AS stateProvince,
      CAST(n.basisOfRecord AS VARCHAR) AS basisOfRecord,
      CAST(n.institutionCode AS VARCHAR) AS institutionCode,
      CAST(n.collectionCode AS VARCHAR) AS collectionCode,

      CAST(n.decimalLatitude AS DOUBLE) AS ANALYTICAL_DECIMAL_LATITUDE,
      CAST(n.decimalLongitude AS DOUBLE) AS ANALYTICAL_DECIMAL_LONGITUDE,
      CAST(n.coordinateUncertaintyInMeters AS DOUBLE)
        AS ANALYTICAL_COORDINATE_UNCERTAINTY_M,

      'national_gbif' AS OCCURRENCE_SOURCE,
      'gbif_original_coordinates' AS ANALYTICAL_COORDINATE_SOURCE,
      'GBIF supplied coordinate' AS ANALYTICAL_GEOREFERENCE_METHOD,
      NULL AS ANALYTICAL_GEOREFERENCE_CONFIDENCE,
      NULL AS ANALYTICAL_GAZETTEER_SOURCE,
      NULL AS ANALYTICAL_GAZETTEER_ID,
      NULL AS ANALYTICAL_PRECISION_CLASS,

      CAST(t.final_wcvp_accepted_plant_name_id AS VARCHAR)
        AS FINAL_WCVP_ID,
      CAST(t.final_wcvp_taxon_name AS VARCHAR)
        AS FINAL_WCVP_RECOGNISED_NAME,
      CAST(t.final_wcvp_taxon_rank AS VARCHAR)
        AS FINAL_WCVP_RANK,
      CAST(t.final_wcvp_taxon_status AS VARCHAR)
        AS FINAL_WCVP_STATUS,
      CAST(t.vpjd_taxonomic_status AS VARCHAR)
        AS FINAL_WCVP_CONCEPT_CLASS,

      'frozen_04p' AS TAXONOMIC_RECONCILIATION_SOURCE,
      CASE
        WHEN t.final_wcvp_accepted_plant_name_id IS NULL
          THEN 'unresolved'
        ELSE CAST(t.vpjd_taxonomic_status AS VARCHAR)
      END AS RECONCILIATION_STATUS

    FROM occurrences_clean n
    LEFT JOIN occurrence_wcvp_consolidated t
      ON CAST(n.taxonKey AS VARCHAR) =
         CAST(t.taxonKey AS VARCHAR)

    WHERE
      UPPER(TRIM(CAST(n.occurrenceStatus AS VARCHAR))) = 'PRESENT'
    "
  )
  
  national_integrated_n <- dbGetQuery(
    con,
    "
    SELECT COUNT(*) AS n
    FROM integration04_national
    "
  )$n[[1]]
  
  if (national_integrated_n != EXPECTED_NATIONAL_PRESENT) {
    stop(
      "National analytical stream changed row count: ",
      national_integrated_n, "."
    )
  }
  
  # ---------------------------------------------------------------------------
  # Create recovered analytical stream
  # ---------------------------------------------------------------------------
  
  dbExecute(
    con,
    "
    CREATE OR REPLACE TEMP TABLE integration04_recovered AS
    SELECT
      CAST(gbifID AS VARCHAR) AS gbifID,
      CAST(datasetKey AS VARCHAR) AS datasetKey,
      CAST(scientificName AS VARCHAR) AS scientificName,
      CAST(taxonRank AS VARCHAR) AS taxonRank,
      CAST(countryCode AS VARCHAR) AS countryCode,
      CAST(locality AS VARCHAR) AS locality,
      CAST(stateProvince AS VARCHAR) AS stateProvince,
      CAST(basisOfRecord AS VARCHAR) AS basisOfRecord,
      CAST(institutionCode AS VARCHAR) AS institutionCode,
      CAST(collectionCode AS VARCHAR) AS collectionCode,

      CAST(ANALYTICAL_DECIMAL_LATITUDE AS DOUBLE)
        AS ANALYTICAL_DECIMAL_LATITUDE,
      CAST(ANALYTICAL_DECIMAL_LONGITUDE AS DOUBLE)
        AS ANALYTICAL_DECIMAL_LONGITUDE,
      CAST(ANALYTICAL_COORDINATE_UNCERTAINTY_M AS DOUBLE)
        AS ANALYTICAL_COORDINATE_UNCERTAINTY_M,

      'recovered_institutional' AS OCCURRENCE_SOURCE,
      CAST(ANALYTICAL_COORDINATE_SOURCE AS VARCHAR)
        AS ANALYTICAL_COORDINATE_SOURCE,
      CAST(ANALYTICAL_GEOREFERENCE_METHOD AS VARCHAR)
        AS ANALYTICAL_GEOREFERENCE_METHOD,
      CAST(ANALYTICAL_GEOREFERENCE_CONFIDENCE AS VARCHAR)
        AS ANALYTICAL_GEOREFERENCE_CONFIDENCE,
      CAST(ANALYTICAL_GAZETTEER_SOURCE AS VARCHAR)
        AS ANALYTICAL_GAZETTEER_SOURCE,
      CAST(ANALYTICAL_GAZETTEER_ID AS VARCHAR)
        AS ANALYTICAL_GAZETTEER_ID,
      CAST(ANALYTICAL_PRECISION_CLASS AS VARCHAR)
        AS ANALYTICAL_PRECISION_CLASS,

      CAST(FINAL_WCVP_ID AS VARCHAR)
        AS FINAL_WCVP_ID,
      CAST(FINAL_WCVP_RECOGNISED_NAME AS VARCHAR)
        AS FINAL_WCVP_RECOGNISED_NAME,
      CAST(FINAL_WCVP_RANK AS VARCHAR)
        AS FINAL_WCVP_RANK,
      CAST(FINAL_WCVP_STATUS AS VARCHAR)
        AS FINAL_WCVP_STATUS,
      CAST(FINAL_WCVP_CONCEPT_CLASS AS VARCHAR)
        AS FINAL_WCVP_CONCEPT_CLASS,

      CAST(RECONCILIATION_METHOD AS VARCHAR)
        AS TAXONOMIC_RECONCILIATION_SOURCE,
      CAST(RECONCILIATION_STATUS AS VARCHAR)
        AS RECONCILIATION_STATUS

    FROM recovered_institutional_occurrences_taxonomised
    "
  )
  
  recovered_integrated_n <- dbGetQuery(
    con,
    "
    SELECT COUNT(*) AS n
    FROM integration04_recovered
    "
  )$n[[1]]
  
  if (recovered_integrated_n != EXPECTED_RECOVERED) {
    stop(
      "Recovered analytical stream changed row count: ",
      recovered_integrated_n, "."
    )
  }
  
  # ---------------------------------------------------------------------------
  # Construct canonical integrated occurrence table
  # ---------------------------------------------------------------------------
  
  dbExecute(
    con,
    "
    CREATE OR REPLACE TABLE vpjd_japan_occurrences_integrated AS
    SELECT * FROM integration04_national
    UNION ALL
    SELECT * FROM integration04_recovered
    "
  )
  
  total_n <- dbGetQuery(
    con,
    "
    SELECT COUNT(*) AS n
    FROM vpjd_japan_occurrences_integrated
    "
  )$n[[1]]
  
  distinct_gbif_n <- dbGetQuery(
    con,
    "
    SELECT COUNT(DISTINCT gbifID) AS n
    FROM vpjd_japan_occurrences_integrated
    "
  )$n[[1]]
  
  if (total_n != EXPECTED_TOTAL) {
    stop(
      "Integrated table contains ",
      total_n,
      " rows; expected ",
      EXPECTED_TOTAL, "."
    )
  }
  
  if (distinct_gbif_n != EXPECTED_TOTAL) {
    stop(
      "Integrated table contains duplicate gbifID values: ",
      total_n - distinct_gbif_n, "."
    )
  }
  
  # ---------------------------------------------------------------------------
  # Validate coordinates
  # ---------------------------------------------------------------------------
  
  coordinate_qa <- dbGetQuery(
    con,
    "
    SELECT
      OCCURRENCE_SOURCE,
      COUNT(*) AS occurrence_records,
      SUM(
        CASE
          WHEN ANALYTICAL_DECIMAL_LATITUDE IS NULL
            OR ANALYTICAL_DECIMAL_LONGITUDE IS NULL
          THEN 1 ELSE 0
        END
      ) AS missing_analytical_coordinates,
      SUM(
        CASE
          WHEN ANALYTICAL_DECIMAL_LATITUDE < -90
            OR ANALYTICAL_DECIMAL_LATITUDE > 90
            OR ANALYTICAL_DECIMAL_LONGITUDE < -180
            OR ANALYTICAL_DECIMAL_LONGITUDE > 180
          THEN 1 ELSE 0
        END
      ) AS invalid_analytical_coordinates
    FROM vpjd_japan_occurrences_integrated
    GROUP BY OCCURRENCE_SOURCE
    ORDER BY OCCURRENCE_SOURCE
    "
  ) %>%
    as_tibble()
  
  if (
    sum(coordinate_qa$missing_analytical_coordinates) > 0 ||
    sum(coordinate_qa$invalid_analytical_coordinates) > 0
  ) {
    stop(
      "Integrated analytical coordinate QA failed."
    )
  }
  
  # ---------------------------------------------------------------------------
  # Taxonomic profile
  # ---------------------------------------------------------------------------
  
  taxonomy_summary <- dbGetQuery(
    con,
    "
    SELECT
      OCCURRENCE_SOURCE,
      RECONCILIATION_STATUS,
      COUNT(*) AS occurrence_records,
      COUNT(DISTINCT FINAL_WCVP_ID) AS distinct_wcvp_ids
    FROM vpjd_japan_occurrences_integrated
    GROUP BY
      OCCURRENCE_SOURCE,
      RECONCILIATION_STATUS
    ORDER BY
      OCCURRENCE_SOURCE,
      occurrence_records DESC
    "
  ) %>%
    as_tibble()
  
  source_summary <- dbGetQuery(
    con,
    "
    SELECT
      OCCURRENCE_SOURCE,
      COUNT(*) AS occurrence_records,
      COUNT(DISTINCT gbifID) AS distinct_gbif_ids,
      COUNT(DISTINCT FINAL_WCVP_ID) AS distinct_wcvp_ids
    FROM vpjd_japan_occurrences_integrated
    GROUP BY OCCURRENCE_SOURCE
    ORDER BY OCCURRENCE_SOURCE
    "
  ) %>%
    as_tibble()
  
  overall_taxonomy <- dbGetQuery(
    con,
    "
    SELECT
      COUNT(*) AS occurrence_records,
      COUNT(DISTINCT gbifID) AS distinct_gbif_ids,
      SUM(
        CASE WHEN FINAL_WCVP_ID IS NOT NULL
        THEN 1 ELSE 0 END
      ) AS taxonomically_resolved_records,
      SUM(
        CASE WHEN FINAL_WCVP_ID IS NULL
        THEN 1 ELSE 0 END
      ) AS taxonomically_unresolved_records,
      COUNT(DISTINCT FINAL_WCVP_ID) AS distinct_wcvp_ids
    FROM vpjd_japan_occurrences_integrated
    "
  ) %>%
    as_tibble()
  
  # ---------------------------------------------------------------------------
  # Provenance profile
  # ---------------------------------------------------------------------------
  
  provenance_summary <- dbGetQuery(
    con,
    "
    SELECT
      OCCURRENCE_SOURCE,
      ANALYTICAL_COORDINATE_SOURCE,
      COUNT(*) AS occurrence_records
    FROM vpjd_japan_occurrences_integrated
    GROUP BY
      OCCURRENCE_SOURCE,
      ANALYTICAL_COORDINATE_SOURCE
    ORDER BY occurrence_records DESC
    "
  ) %>%
    as_tibble()
  
  # ---------------------------------------------------------------------------
  # Metadata
  # ---------------------------------------------------------------------------
  
  metadata <- tibble(
    metric = c(
      "script_version",
      "input_integration_01_version",
      "input_integration_02_version",
      "input_integration_03_version",
      "input_taxonomy_04p_version",
      "national_present_records",
      "recovered_institutional_records",
      "integrated_occurrence_records",
      "distinct_integrated_gbif_ids",
      "records_removed_as_duplicates",
      "speculative_deduplication_performed",
      "new_taxonomic_assignments_made",
      "source_coordinates_overwritten",
      "upstream_tables_modified"
    ),
    value = c(
      SCRIPT_VERSION,
      integration_01_version,
      integration_02_version,
      integration_03_version,
      taxonomy_version,
      as.character(national_n),
      as.character(recovered_n),
      as.character(total_n),
      as.character(distinct_gbif_n),
      "0",
      "FALSE",
      "FALSE",
      "FALSE",
      "FALSE"
    )
  )
  
  # ---------------------------------------------------------------------------
  # Persist summaries
  # ---------------------------------------------------------------------------
  
  dbWriteTable(
    con,
    "vpjd_japan_occurrences_integrated_source_summary",
    source_summary,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "vpjd_japan_occurrences_integrated_taxonomy_summary",
    taxonomy_summary,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "vpjd_japan_occurrences_integrated_coordinate_qa",
    coordinate_qa,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "vpjd_japan_occurrences_integrated_metadata",
    metadata,
    overwrite = TRUE
  )
  
  out_dir <- here(
    "outputs",
    "tables",
    "integration",
    "integrated_occurrences"
  )
  
  dir.create(
    out_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  write_csv(
    source_summary,
    file.path(out_dir, "source_summary.csv")
  )
  
  write_csv(
    taxonomy_summary,
    file.path(out_dir, "taxonomy_summary.csv")
  )
  
  write_csv(
    coordinate_qa,
    file.path(out_dir, "coordinate_qa.csv")
  )
  
  write_csv(
    overall_taxonomy,
    file.path(out_dir, "overall_taxonomy.csv")
  )
  
  write_csv(
    provenance_summary,
    file.path(out_dir, "coordinate_provenance.csv")
  )
  
  write_csv(
    metadata,
    file.path(out_dir, "metadata.csv")
  )
  
  # ---------------------------------------------------------------------------
  # Console report
  # ---------------------------------------------------------------------------
  
  cat("— Source summary —\n\n")
  print(source_summary, n = Inf)
  
  cat("\n— Coordinate QA —\n\n")
  print(coordinate_qa, n = Inf)
  
  cat("\n— Taxonomic profile —\n\n")
  print(taxonomy_summary, n = Inf)
  
  cat("\n— Overall taxonomy —\n\n")
  print(overall_taxonomy, n = Inf)
  
  cat("\n— Coordinate provenance —\n\n")
  print(provenance_summary, n = Inf)
  
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
    "Integrated records: ",
    format(total_n, big.mark = ","),
    "\n", sep = ""
  )
  cat(
    "Distinct integrated gbifIDs: ",
    format(distinct_gbif_n, big.mark = ","),
    "\n", sep = ""
  )
  
  cat("\n— Safety —\n")
  cat("Speculative deduplication performed: FALSE\n")
  cat("Records removed as duplicates: 0\n")
  cat("New taxonomic assignments made: FALSE\n")
  cat("Source coordinates overwritten: FALSE\n")
  cat("Frozen upstream tables modified: FALSE\n")
  cat("Output status: INTEGRATED ANALYTICAL OCCURRENCE LAYER\n")
  
  cat(
    "\n04_build_integrated_occurrences.R v",
    SCRIPT_VERSION,
    " complete.\n",
    sep = ""
  )
  
  invisible(
    list(
      source_summary = source_summary,
      coordinate_qa = coordinate_qa,
      taxonomy_summary = taxonomy_summary,
      overall_taxonomy = overall_taxonomy,
      provenance_summary = provenance_summary,
      metadata = metadata
    )
  )
}

integration_04 <-
  run_integrated_occurrence_build()