# ==============================================================================
# VPJD-OJPCP
# Distribution 01 — Build contemporary Japanese taxon-area distribution
# Version: 0.1.1
#
# Purpose:
#   Derive the contemporary occurrence-based Japanese distribution of
#   WCVP-linked vascular-plant concepts across canonical JP01–JP51 botanical
#   areas.
#
# Source:
#   vpjd_japan_occurrences_analytical
#
# Canonical output:
#   vpjd_japan_taxon_area_distribution
#
# Grain:
#   One row per FINAL_WCVP_ID x botanical area.
#
# v0.1.1:
#   - distinguishes missing taxon metadata from genuinely conflicting metadata;
#   - NULL metadata do not constitute a taxonomic conflict;
#   - profiles WCVP IDs with incomplete final metadata;
#   - canonical metadata are propagated from the unique available non-null
#     value only after non-null consistency has been validated.
#
# Principles:
#   - occurrence-first;
#   - contemporary Japanese distribution is derived from analytical records;
#   - historical Nakamura/Key data do NOT define membership;
#   - FINAL_WCVP_ID is required for taxon-area aggregation;
#   - unresolved records remain preserved upstream but are not forced into
#     WCVP taxon distributions;
#   - no inference of native/alien/cultivated status;
#   - no Stars or GHI;
#   - no modification of occurrences, taxonomy, geography or quarantine.
# ==============================================================================

suppressPackageStartupMessages({
  library(DBI)
  library(duckdb)
  library(dplyr)
  library(readr)
  library(here)
})

SCRIPT_VERSION <- "0.1.1"
SOURCE_VIEW <- "vpjd_japan_occurrences_analytical"
OUTPUT_TABLE <- "vpjd_japan_taxon_area_distribution"
EXPECTED_ANALYTICAL <- 3029888L
EXPECTED_AREAS <- 51L
RUN_DATE <- format(Sys.Date(), "%Y-%m-%d")

run_japan_taxon_area_distribution <- function() {
  cat("\n— VPJD contemporary Japanese taxon-area distribution —\n\n")
  cat("Run date: ", RUN_DATE, "\n\n", sep = "")
  
  db_path <- here(
    "data", "interim", "occurrences",
    "vpjd_occurrences.duckdb"
  )
  
  output_dir <- here(
    "outputs", "tables", "distribution",
    "japan_taxon_area"
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
    paste0('"', gsub('"', '""', x), '"')
  }
  
  q_source <- quote_id(SOURCE_VIEW)
  q_output <- quote_id(OUTPUT_TABLE)
  
  # ---------------------------------------------------------------------------
  # Source validation
  # ---------------------------------------------------------------------------
  
  objects <- dbGetQuery(
    con,
    "SELECT table_name FROM information_schema.tables"
  )$table_name
  
  if (!(SOURCE_VIEW %in% objects)) {
    stop("Canonical analytical occurrence view not found: ", SOURCE_VIEW)
  }
  
  source_fields <- dbListFields(
    con,
    SOURCE_VIEW
  )
  
  required_fields <- c(
    "gbifID",
    "scientificName",
    "FINAL_WCVP_ID",
    "FINAL_WCVP_RECOGNISED_NAME",
    "FINAL_WCVP_RANK",
    "FINAL_WCVP_STATUS",
    "FINAL_WCVP_CONCEPT_CLASS",
    "RECONCILIATION_STATUS",
    "OCCURRENCE_SOURCE",
    "ANALYTICAL_BOTANICAL_AREA_ID",
    "ANALYTICAL_BOTANICAL_AREA_NAME"
  )
  
  missing_fields <- setdiff(
    required_fields,
    source_fields
  )
  
  if (length(missing_fields) > 0L) {
    stop(
      "Required analytical fields missing:\n",
      paste(missing_fields, collapse = ", ")
    )
  }
  
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
  
  source_areas <- dbGetQuery(
    con,
    paste0(
      "SELECT COUNT(DISTINCT ANALYTICAL_BOTANICAL_AREA_ID) AS n FROM ",
      q_source
    )
  )$n[[1]]
  
  cat(
    "Analytical occurrence records: ",
    format(source_count, big.mark = ","),
    "\n",
    sep = ""
  )
  
  cat(
    "Distinct analytical gbifIDs: ",
    format(source_ids, big.mark = ","),
    "\n",
    sep = ""
  )
  
  cat(
    "Botanical areas represented: ",
    source_areas,
    "\n\n",
    sep = ""
  )
  
  if (source_count != EXPECTED_ANALYTICAL) {
    stop("Unexpected analytical occurrence count.")
  }
  
  if (source_ids != source_count) {
    stop("Analytical occurrence gbifIDs are not unique.")
  }
  
  if (source_areas != EXPECTED_AREAS) {
    stop(
      "Analytical occurrence population does not represent all 51 areas."
    )
  }
  
  cat("Source validation: PASS\n")
  
  # ---------------------------------------------------------------------------
  # Taxonomic eligibility
  #
  # FINAL_WCVP_ID is the operative criterion for WCVP taxon-area aggregation.
  # Records without FINAL_WCVP_ID remain preserved upstream.
  # ---------------------------------------------------------------------------
  
  eligible_count <- dbGetQuery(
    con,
    paste0(
      "SELECT COUNT(*) AS n FROM ",
      q_source,
      " WHERE FINAL_WCVP_ID IS NOT NULL"
    )
  )$n[[1]]
  
  ineligible_count <- source_count - eligible_count
  
  eligible_taxa <- dbGetQuery(
    con,
    paste0(
      "SELECT COUNT(DISTINCT FINAL_WCVP_ID) AS n FROM ",
      q_source,
      " WHERE FINAL_WCVP_ID IS NOT NULL"
    )
  )$n[[1]]
  
  ineligible_names <- dbGetQuery(
    con,
    paste0(
      "SELECT COUNT(DISTINCT scientificName) AS n FROM ",
      q_source,
      " WHERE FINAL_WCVP_ID IS NULL ",
      "AND scientificName IS NOT NULL"
    )
  )$n[[1]]
  
  eligibility_summary <- tibble(
    population = c(
      "analytical_occurrences",
      "WCVP_linked_occurrences",
      "without_FINAL_WCVP_ID"
    ),
    records = c(
      source_count,
      eligible_count,
      ineligible_count
    ),
    percent = round(
      100 * c(
        source_count,
        eligible_count,
        ineligible_count
      ) / source_count,
      4
    )
  )
  
  cat("\n— Taxonomic eligibility —\n")
  
  print.data.frame(
    eligibility_summary,
    row.names = FALSE
  )
  
  cat(
    "\nDistinct FINAL_WCVP_IDs represented: ",
    format(eligible_taxa, big.mark = ","),
    "\n",
    sep = ""
  )
  
  cat(
    "Distinct unresolved scientificName values: ",
    format(ineligible_names, big.mark = ","),
    "\n",
    sep = ""
  )
  
  # ---------------------------------------------------------------------------
  # WCVP-linked concept profile
  # ---------------------------------------------------------------------------
  
  concept_profile <- dbGetQuery(
    con,
    paste0(
      "SELECT ",
      "FINAL_WCVP_CONCEPT_CLASS, ",
      "FINAL_WCVP_STATUS, ",
      "FINAL_WCVP_RANK, ",
      "COUNT(*) AS occurrence_records, ",
      "COUNT(DISTINCT FINAL_WCVP_ID) AS distinct_WCVP_IDs ",
      "FROM ",
      q_source,
      " WHERE FINAL_WCVP_ID IS NOT NULL ",
      "GROUP BY ",
      "FINAL_WCVP_CONCEPT_CLASS, ",
      "FINAL_WCVP_STATUS, ",
      "FINAL_WCVP_RANK ",
      "ORDER BY occurrence_records DESC"
    )
  )
  
  cat("\n— WCVP-linked concept profile —\n")
  
  print.data.frame(
    concept_profile,
    row.names = FALSE
  )
  
  reconciliation_profile <- dbGetQuery(
    con,
    paste0(
      "SELECT ",
      "RECONCILIATION_STATUS, ",
      "FINAL_WCVP_CONCEPT_CLASS, ",
      "COUNT(*) AS occurrence_records, ",
      "COUNT(DISTINCT FINAL_WCVP_ID) AS distinct_WCVP_IDs ",
      "FROM ",
      q_source,
      " WHERE FINAL_WCVP_ID IS NOT NULL ",
      "GROUP BY ",
      "RECONCILIATION_STATUS, ",
      "FINAL_WCVP_CONCEPT_CLASS ",
      "ORDER BY occurrence_records DESC"
    )
  )
  
  cat("\n— WCVP-linked reconciliation profile —\n")
  
  print.data.frame(
    reconciliation_profile,
    row.names = FALSE
  )
  
  # ---------------------------------------------------------------------------
  # Metadata completeness profile
  #
  # Missing metadata are NOT treated as conflicting metadata.
  # ---------------------------------------------------------------------------
  
  metadata_completeness <- dbGetQuery(
    con,
    paste0(
      "SELECT ",
      "FINAL_WCVP_ID, ",
      "COUNT(*) AS occurrence_records, ",
      "SUM(CASE WHEN FINAL_WCVP_RECOGNISED_NAME IS NULL ",
      "THEN 1 ELSE 0 END) AS missing_name_records, ",
      "SUM(CASE WHEN FINAL_WCVP_RANK IS NULL ",
      "THEN 1 ELSE 0 END) AS missing_rank_records, ",
      "SUM(CASE WHEN FINAL_WCVP_STATUS IS NULL ",
      "THEN 1 ELSE 0 END) AS missing_status_records, ",
      "SUM(CASE WHEN FINAL_WCVP_CONCEPT_CLASS IS NULL ",
      "THEN 1 ELSE 0 END) AS missing_class_records ",
      "FROM ",
      q_source,
      " WHERE FINAL_WCVP_ID IS NOT NULL ",
      "GROUP BY FINAL_WCVP_ID ",
      "HAVING ",
      "SUM(CASE WHEN FINAL_WCVP_RECOGNISED_NAME IS NULL ",
      "THEN 1 ELSE 0 END) > 0 ",
      "OR SUM(CASE WHEN FINAL_WCVP_RANK IS NULL ",
      "THEN 1 ELSE 0 END) > 0 ",
      "OR SUM(CASE WHEN FINAL_WCVP_STATUS IS NULL ",
      "THEN 1 ELSE 0 END) > 0 ",
      "OR SUM(CASE WHEN FINAL_WCVP_CONCEPT_CLASS IS NULL ",
      "THEN 1 ELSE 0 END) > 0 ",
      "ORDER BY occurrence_records DESC"
    )
  )
  
  incomplete_taxa <- nrow(metadata_completeness)
  
  incomplete_records <- dbGetQuery(
    con,
    paste0(
      "SELECT COUNT(*) AS n FROM ",
      q_source,
      " WHERE FINAL_WCVP_ID IS NOT NULL ",
      "AND (",
      "FINAL_WCVP_RECOGNISED_NAME IS NULL ",
      "OR FINAL_WCVP_RANK IS NULL ",
      "OR FINAL_WCVP_STATUS IS NULL ",
      "OR FINAL_WCVP_CONCEPT_CLASS IS NULL",
      ")"
    )
  )$n[[1]]
  
  cat("\n— Final WCVP metadata completeness —\n")
  
  cat(
    "WCVP IDs with one or more incomplete metadata records: ",
    format(incomplete_taxa, big.mark = ","),
    "\n",
    sep = ""
  )
  
  cat(
    "Occurrence records with incomplete final WCVP metadata: ",
    format(incomplete_records, big.mark = ","),
    "\n",
    sep = ""
  )
  
  if (incomplete_taxa > 0L) {
    print.data.frame(
      metadata_completeness,
      row.names = FALSE
    )
  }
  
  # ---------------------------------------------------------------------------
  # Genuine metadata consistency validation
  #
  # NULL is ignored here. A conflict exists only when a FINAL_WCVP_ID has
  # more than one distinct NON-NULL value for a canonical metadata attribute.
  # ---------------------------------------------------------------------------
  
  metadata_consistency <- dbGetQuery(
    con,
    paste0(
      "SELECT ",
      "FINAL_WCVP_ID, ",
      "COUNT(*) AS occurrence_records, ",
      "COUNT(DISTINCT FINAL_WCVP_RECOGNISED_NAME) AS n_names, ",
      "COUNT(DISTINCT FINAL_WCVP_RANK) AS n_ranks, ",
      "COUNT(DISTINCT FINAL_WCVP_STATUS) AS n_statuses, ",
      "COUNT(DISTINCT FINAL_WCVP_CONCEPT_CLASS) AS n_classes ",
      "FROM ",
      q_source,
      " WHERE FINAL_WCVP_ID IS NOT NULL ",
      "GROUP BY FINAL_WCVP_ID"
    )
  )
  
  genuine_conflicts <- metadata_consistency %>%
    filter(
      n_names > 1 |
        n_ranks > 1 |
        n_statuses > 1 |
        n_classes > 1
    )
  
  genuine_conflict_count <- nrow(genuine_conflicts)
  
  cat(
    "\nWCVP IDs with genuinely conflicting non-null metadata: ",
    genuine_conflict_count,
    "\n",
    sep = ""
  )
  
  if (genuine_conflict_count > 0L) {
    print.data.frame(
      genuine_conflicts,
      row.names = FALSE
    )
    
    stop(
      "Genuine FINAL_WCVP_ID metadata conflicts detected. ",
      "Do not build taxon-area distribution until reviewed."
    )
  }
  
  cat("Non-null WCVP concept metadata consistency: PASS\n")
  
  # ---------------------------------------------------------------------------
  # Identify IDs for which NO canonical metadata are available
  #
  # A WCVP ID may have some occurrence rows with NULL metadata and still be
  # usable if another row supplies the unique canonical metadata.
  # ---------------------------------------------------------------------------
  
  metadata_availability <- dbGetQuery(
    con,
    paste0(
      "SELECT ",
      "FINAL_WCVP_ID, ",
      "COUNT(DISTINCT FINAL_WCVP_RECOGNISED_NAME) AS n_names, ",
      "COUNT(DISTINCT FINAL_WCVP_RANK) AS n_ranks, ",
      "COUNT(DISTINCT FINAL_WCVP_STATUS) AS n_statuses, ",
      "COUNT(DISTINCT FINAL_WCVP_CONCEPT_CLASS) AS n_classes ",
      "FROM ",
      q_source,
      " WHERE FINAL_WCVP_ID IS NOT NULL ",
      "GROUP BY FINAL_WCVP_ID"
    )
  )
  
  metadata_unavailable <- metadata_availability %>%
    filter(
      n_names == 0 |
        n_ranks == 0 |
        n_statuses == 0 |
        n_classes == 0
    )
  
  metadata_unavailable_count <- nrow(metadata_unavailable)
  
  cat(
    "WCVP IDs lacking any available value for one or more metadata fields: ",
    metadata_unavailable_count,
    "\n",
    sep = ""
  )
  
  if (metadata_unavailable_count > 0L) {
    print.data.frame(
      metadata_unavailable,
      row.names = FALSE
    )
  }
  
  # ---------------------------------------------------------------------------
  # Existing output safeguard
  #
  # This is a reproducible derived table, so it may be rebuilt.
  # ---------------------------------------------------------------------------
  
  if (OUTPUT_TABLE %in% dbListTables(con)) {
    cat(
      "\nExisting derived taxon-area table detected; rebuilding...\n"
    )
    
    dbExecute(
      con,
      paste0(
        "DROP TABLE ",
        q_output
      )
    )
  }
  
  # ---------------------------------------------------------------------------
  # Build canonical long-format distribution
  #
  # One row per FINAL_WCVP_ID x botanical area.
  #
  # MIN() is safe here because:
  #   1. genuine non-null conflicts have already been excluded;
  #   2. DuckDB MIN() ignores NULL values;
  #   3. therefore the unique available canonical value is propagated.
  # ---------------------------------------------------------------------------
  
  cat("\nBuilding canonical taxon-area distribution...\n")
  
  dbExecute(
    con,
    paste0(
      "CREATE TABLE ",
      q_output,
      " AS ",
      "SELECT ",
      "FINAL_WCVP_ID, ",
      "MIN(FINAL_WCVP_RECOGNISED_NAME) ",
      "AS FINAL_WCVP_RECOGNISED_NAME, ",
      "MIN(FINAL_WCVP_RANK) AS FINAL_WCVP_RANK, ",
      "MIN(FINAL_WCVP_STATUS) AS FINAL_WCVP_STATUS, ",
      "MIN(FINAL_WCVP_CONCEPT_CLASS) AS FINAL_WCVP_CONCEPT_CLASS, ",
      "ANALYTICAL_BOTANICAL_AREA_ID AS BOTANICAL_AREA_ID, ",
      "MIN(ANALYTICAL_BOTANICAL_AREA_NAME) AS BOTANICAL_AREA_NAME, ",
      "COUNT(*) AS OCCURRENCE_RECORDS, ",
      "COUNT(DISTINCT CAST(gbifID AS VARCHAR)) AS DISTINCT_GBIF_IDS, ",
      "SUM(CASE WHEN OCCURRENCE_SOURCE = 'national_gbif' ",
      "THEN 1 ELSE 0 END) AS NATIONAL_GBIF_RECORDS, ",
      "SUM(CASE WHEN OCCURRENCE_SOURCE = 'recovered_institutional' ",
      "THEN 1 ELSE 0 END) AS RECOVERED_INSTITUTIONAL_RECORDS, ",
      "SUM(CASE WHEN ",
      "FINAL_WCVP_RECOGNISED_NAME IS NULL ",
      "OR FINAL_WCVP_RANK IS NULL ",
      "OR FINAL_WCVP_STATUS IS NULL ",
      "OR FINAL_WCVP_CONCEPT_CLASS IS NULL ",
      "THEN 1 ELSE 0 END) AS INCOMPLETE_METADATA_RECORDS, ",
      "1 AS PRESENCE, ",
      "'01' AS DISTRIBUTION_MODULE, ",
      "'",
      SCRIPT_VERSION,
      "' AS DISTRIBUTION_VERSION ",
      "FROM ",
      q_source,
      " WHERE FINAL_WCVP_ID IS NOT NULL ",
      "GROUP BY ",
      "FINAL_WCVP_ID, ",
      "ANALYTICAL_BOTANICAL_AREA_ID"
    )
  )
  
  # ---------------------------------------------------------------------------
  # Validate canonical table
  # ---------------------------------------------------------------------------
  
  distribution_rows <- dbGetQuery(
    con,
    paste0(
      "SELECT COUNT(*) AS n FROM ",
      q_output
    )
  )$n[[1]]
  
  distribution_taxa <- dbGetQuery(
    con,
    paste0(
      "SELECT COUNT(DISTINCT FINAL_WCVP_ID) AS n FROM ",
      q_output
    )
  )$n[[1]]
  
  distribution_areas <- dbGetQuery(
    con,
    paste0(
      "SELECT COUNT(DISTINCT BOTANICAL_AREA_ID) AS n FROM ",
      q_output
    )
  )$n[[1]]
  
  distribution_occurrences <- dbGetQuery(
    con,
    paste0(
      "SELECT SUM(OCCURRENCE_RECORDS) AS n FROM ",
      q_output
    )
  )$n[[1]]
  
  distribution_gbif_ids <- dbGetQuery(
    con,
    paste0(
      "SELECT SUM(DISTINCT_GBIF_IDS) AS n FROM ",
      q_output
    )
  )$n[[1]]
  
  duplicate_pairs <- dbGetQuery(
    con,
    paste0(
      "SELECT COUNT(*) AS n FROM (",
      "SELECT FINAL_WCVP_ID, BOTANICAL_AREA_ID, COUNT(*) AS n ",
      "FROM ",
      q_output,
      " GROUP BY FINAL_WCVP_ID, BOTANICAL_AREA_ID ",
      "HAVING COUNT(*) > 1",
      ") x"
    )
  )$n[[1]]
  
  invalid_area_count <- dbGetQuery(
    con,
    paste0(
      "SELECT COUNT(*) AS n FROM ",
      q_output,
      " WHERE BOTANICAL_AREA_ID IS NULL ",
      "OR BOTANICAL_AREA_ID NOT BETWEEN 'JP01' AND 'JP51'"
    )
  )$n[[1]]
  
  invalid_presence_count <- dbGetQuery(
    con,
    paste0(
      "SELECT COUNT(*) AS n FROM ",
      q_output,
      " WHERE PRESENCE <> 1 OR PRESENCE IS NULL"
    )
  )$n[[1]]
  
  source_balance_error <- dbGetQuery(
    con,
    paste0(
      "SELECT COUNT(*) AS n FROM ",
      q_output,
      " WHERE ",
      "NATIONAL_GBIF_RECORDS + RECOVERED_INSTITUTIONAL_RECORDS ",
      "<> OCCURRENCE_RECORDS"
    )
  )$n[[1]]
  
  output_missing_metadata <- dbGetQuery(
    con,
    paste0(
      "SELECT COUNT(*) AS n FROM ",
      q_output,
      " WHERE ",
      "FINAL_WCVP_RECOGNISED_NAME IS NULL ",
      "OR FINAL_WCVP_RANK IS NULL ",
      "OR FINAL_WCVP_STATUS IS NULL ",
      "OR FINAL_WCVP_CONCEPT_CLASS IS NULL"
    )
  )$n[[1]]
  
  propagated_metadata_records <- dbGetQuery(
    con,
    paste0(
      "SELECT SUM(INCOMPLETE_METADATA_RECORDS) AS n FROM ",
      q_output
    )
  )$n[[1]]
  
  cat("\n— Canonical distribution —\n")
  
  cat(
    "Taxon-area rows: ",
    format(distribution_rows, big.mark = ","),
    "\n",
    sep = ""
  )
  
  cat(
    "Distinct WCVP IDs: ",
    format(distribution_taxa, big.mark = ","),
    "\n",
    sep = ""
  )
  
  cat(
    "Botanical areas represented: ",
    distribution_areas,
    "\n",
    sep = ""
  )
  
  cat(
    "Occurrence evidence represented: ",
    format(distribution_occurrences, big.mark = ","),
    "\n",
    sep = ""
  )
  
  cat(
    "Source occurrence records with incomplete metadata represented: ",
    format(propagated_metadata_records, big.mark = ","),
    "\n",
    sep = ""
  )
  
  cat(
    "Taxon-area rows still lacking canonical metadata: ",
    format(output_missing_metadata, big.mark = ","),
    "\n",
    sep = ""
  )
  
  # ---------------------------------------------------------------------------
  # Taxon summary
  # ---------------------------------------------------------------------------
  
  taxon_summary <- dbGetQuery(
    con,
    paste0(
      "SELECT ",
      "FINAL_WCVP_ID, ",
      "MIN(FINAL_WCVP_RECOGNISED_NAME) ",
      "AS FINAL_WCVP_RECOGNISED_NAME, ",
      "MIN(FINAL_WCVP_RANK) AS FINAL_WCVP_RANK, ",
      "MIN(FINAL_WCVP_STATUS) AS FINAL_WCVP_STATUS, ",
      "MIN(FINAL_WCVP_CONCEPT_CLASS) AS FINAL_WCVP_CONCEPT_CLASS, ",
      "COUNT(*) AS BOTANICAL_AREAS_PRESENT, ",
      "SUM(OCCURRENCE_RECORDS) AS OCCURRENCE_RECORDS, ",
      "SUM(NATIONAL_GBIF_RECORDS) AS NATIONAL_GBIF_RECORDS, ",
      "SUM(RECOVERED_INSTITUTIONAL_RECORDS) ",
      "AS RECOVERED_INSTITUTIONAL_RECORDS, ",
      "SUM(INCOMPLETE_METADATA_RECORDS) ",
      "AS INCOMPLETE_METADATA_RECORDS ",
      "FROM ",
      q_output,
      " GROUP BY FINAL_WCVP_ID ",
      "ORDER BY BOTANICAL_AREAS_PRESENT DESC, ",
      "OCCURRENCE_RECORDS DESC"
    )
  )
  
  # ---------------------------------------------------------------------------
  # Area summary
  # ---------------------------------------------------------------------------
  
  area_summary <- dbGetQuery(
    con,
    paste0(
      "SELECT ",
      "BOTANICAL_AREA_ID, ",
      "MIN(BOTANICAL_AREA_NAME) AS BOTANICAL_AREA_NAME, ",
      "COUNT(DISTINCT FINAL_WCVP_ID) AS WCVP_TAXA_PRESENT, ",
      "SUM(OCCURRENCE_RECORDS) AS WCVP_LINKED_OCCURRENCE_RECORDS, ",
      "SUM(NATIONAL_GBIF_RECORDS) AS NATIONAL_GBIF_RECORDS, ",
      "SUM(RECOVERED_INSTITUTIONAL_RECORDS) ",
      "AS RECOVERED_INSTITUTIONAL_RECORDS ",
      "FROM ",
      q_output,
      " GROUP BY BOTANICAL_AREA_ID ",
      "ORDER BY BOTANICAL_AREA_ID"
    )
  )
  
  cat("\n— WCVP-linked distribution by botanical area —\n")
  
  print.data.frame(
    area_summary,
    row.names = FALSE
  )
  
  # ---------------------------------------------------------------------------
  # Distribution breadth
  # ---------------------------------------------------------------------------
  
  breadth_summary <- dbGetQuery(
    con,
    paste0(
      "SELECT ",
      "BOTANICAL_AREAS_PRESENT, ",
      "COUNT(*) AS WCVP_TAXA ",
      "FROM (",
      "SELECT FINAL_WCVP_ID, COUNT(*) AS BOTANICAL_AREAS_PRESENT ",
      "FROM ",
      q_output,
      " GROUP BY FINAL_WCVP_ID",
      ") x ",
      "GROUP BY BOTANICAL_AREAS_PRESENT ",
      "ORDER BY BOTANICAL_AREAS_PRESENT"
    )
  )
  
  cat("\n— Number of botanical areas occupied per WCVP concept —\n")
  
  print.data.frame(
    breadth_summary,
    row.names = FALSE
  )
  
  # ---------------------------------------------------------------------------
  # Evidence profile
  #
  # No evidence threshold is imposed here.
  # ---------------------------------------------------------------------------
  
  evidence_summary <- dbGetQuery(
    con,
    paste0(
      "SELECT ",
      "CASE ",
      "WHEN OCCURRENCE_RECORDS = 1 THEN '1_record' ",
      "WHEN OCCURRENCE_RECORDS = 2 THEN '2_records' ",
      "WHEN OCCURRENCE_RECORDS BETWEEN 3 AND 5 THEN '3_5_records' ",
      "WHEN OCCURRENCE_RECORDS BETWEEN 6 AND 10 THEN '6_10_records' ",
      "ELSE 'gt_10_records' ",
      "END AS EVIDENCE_CLASS, ",
      "COUNT(*) AS TAXON_AREA_PRESENCES ",
      "FROM ",
      q_output,
      " GROUP BY 1 ",
      "ORDER BY ",
      "CASE EVIDENCE_CLASS ",
      "WHEN '1_record' THEN 1 ",
      "WHEN '2_records' THEN 2 ",
      "WHEN '3_5_records' THEN 3 ",
      "WHEN '6_10_records' THEN 4 ",
      "ELSE 5 END"
    )
  )
  
  cat("\n— Taxon-area occurrence evidence —\n")
  
  print.data.frame(
    evidence_summary,
    row.names = FALSE
  )
  
  # ---------------------------------------------------------------------------
  # Validation
  # ---------------------------------------------------------------------------
  
  validation <- tibble(
    check = c(
      "analytical_records_3029888",
      "analytical_gbifIDs_unique",
      "all_51_source_areas_represented",
      "WCVP_linked_plus_unlinked_equal_source",
      "no_genuine_non_null_WCVP_metadata_conflicts",
      "distribution_contains_all_eligible_WCVP_IDs",
      "all_51_distribution_areas_represented",
      "distribution_occurrences_equal_WCVP_linked_source",
      "distribution_distinct_gbifIDs_equal_WCVP_linked_source",
      "no_duplicate_taxon_area_pairs",
      "no_invalid_botanical_area_ids",
      "all_presence_values_equal_1",
      "occurrence_source_counts_reconcile",
      "incomplete_metadata_record_count_reconciles",
      "historical_key_not_used",
      "native_status_not_inferred",
      "alien_status_not_inferred",
      "coordinates_not_modified",
      "taxonomy_not_modified",
      "geography_not_modified",
      "quarantine_not_modified",
      "occurrences_not_deleted",
      "stars_not_calculated",
      "ghi_not_calculated"
    ),
    pass = c(
      source_count == EXPECTED_ANALYTICAL,
      source_ids == source_count,
      source_areas == EXPECTED_AREAS,
      (eligible_count + ineligible_count) == source_count,
      genuine_conflict_count == 0L,
      distribution_taxa == eligible_taxa,
      distribution_areas == EXPECTED_AREAS,
      distribution_occurrences == eligible_count,
      distribution_gbif_ids == eligible_count,
      duplicate_pairs == 0L,
      invalid_area_count == 0L,
      invalid_presence_count == 0L,
      source_balance_error == 0L,
      propagated_metadata_records == incomplete_records,
      TRUE,
      TRUE,
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
    "\nAll taxon-area distribution validation checks PASS: ",
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
      "source_view",
      "output_table",
      "source_analytical_occurrences",
      "WCVP_linked_occurrences",
      "occurrences_without_FINAL_WCVP_ID",
      "distinct_WCVP_IDs",
      "WCVP_IDs_with_incomplete_source_metadata",
      "source_records_with_incomplete_WCVP_metadata",
      "genuine_WCVP_metadata_conflicts",
      "taxon_area_rows_with_missing_canonical_metadata",
      "taxon_area_rows",
      "botanical_areas",
      "canonical_format",
      "taxonomic_eligibility_rule",
      "metadata_conflict_rule",
      "metadata_propagation_rule",
      "presence_rule",
      "historical_key_used",
      "native_status_inferred",
      "alien_status_inferred",
      "stars_calculated",
      "ghi_calculated",
      "validation_pass"
    ),
    value = c(
      "01_build_japan_taxon_area_distribution",
      SCRIPT_VERSION,
      RUN_DATE,
      SOURCE_VIEW,
      OUTPUT_TABLE,
      as.character(source_count),
      as.character(eligible_count),
      as.character(ineligible_count),
      as.character(distribution_taxa),
      as.character(incomplete_taxa),
      as.character(incomplete_records),
      as.character(genuine_conflict_count),
      as.character(output_missing_metadata),
      as.character(distribution_rows),
      as.character(distribution_areas),
      "long_one_row_per_WCVP_ID_x_botanical_area",
      "FINAL_WCVP_ID_not_null",
      "more_than_one_distinct_non_null_value",
      "unique_available_non_null_value_after_consistency_validation",
      "one_or_more_analytical_occurrence_records",
      "FALSE",
      "FALSE",
      "FALSE",
      "FALSE",
      "FALSE",
      as.character(all_pass)
    )
  )
  
  # ---------------------------------------------------------------------------
  # Export QA and derived summaries
  # ---------------------------------------------------------------------------
  
  write_csv(
    eligibility_summary,
    file.path(
      output_dir,
      "taxonomic_eligibility_summary.csv"
    )
  )
  
  write_csv(
    concept_profile,
    file.path(
      output_dir,
      "WCVP_concept_profile.csv"
    )
  )
  
  write_csv(
    reconciliation_profile,
    file.path(
      output_dir,
      "WCVP_reconciliation_profile.csv"
    )
  )
  
  write_csv(
    metadata_completeness,
    file.path(
      output_dir,
      "WCVP_metadata_completeness.csv"
    )
  )
  
  write_csv(
    metadata_consistency,
    file.path(
      output_dir,
      "WCVP_metadata_consistency.csv"
    )
  )
  
  write_csv(
    metadata_unavailable,
    file.path(
      output_dir,
      "WCVP_metadata_unavailable.csv"
    )
  )
  
  write_csv(
    area_summary,
    file.path(
      output_dir,
      "taxon_area_summary_by_botanical_area.csv"
    )
  )
  
  write_csv(
    breadth_summary,
    file.path(
      output_dir,
      "taxon_distribution_breadth_summary.csv"
    )
  )
  
  write_csv(
    evidence_summary,
    file.path(
      output_dir,
      "taxon_area_evidence_summary.csv"
    )
  )
  
  write_csv(
    validation,
    file.path(
      output_dir,
      "taxon_area_distribution_validation.csv"
    )
  )
  
  write_csv(
    metadata,
    file.path(
      output_dir,
      "taxon_area_distribution_metadata.csv"
    )
  )
  
  # ---------------------------------------------------------------------------
  # Store derived summaries and QA in DuckDB
  # ---------------------------------------------------------------------------
  
  dbWriteTable(
    con,
    "vpjd_japan_taxon_distribution_summary",
    taxon_summary,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "vpjd_japan_taxon_area_distribution_validation",
    validation,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "vpjd_japan_taxon_area_distribution_metadata",
    metadata,
    overwrite = TRUE
  )
  
  # ---------------------------------------------------------------------------
  # Final status
  # ---------------------------------------------------------------------------
  
  if (!all_pass) {
    cat(
      "\nOutput status: FAILED TAXON-AREA DISTRIBUTION VALIDATION\n"
    )
    
    stop(
      paste(
        "Distribution 01 validation failed.",
        "Do not use the taxon-area table downstream."
      )
    )
  }
  
  cat("\nCanonical distribution table:\n")
  cat("  ", OUTPUT_TABLE, "\n", sep = "")
  
  cat(
    "\nOutput status: ",
    "VALIDATED CONTEMPORARY JAPANESE TAXON-AREA DISTRIBUTION\n",
    sep = ""
  )
  
  cat(
    "Analytical occurrences: ",
    format(source_count, big.mark = ","),
    "\n",
    sep = ""
  )
  
  cat(
    "WCVP-linked occurrence evidence: ",
    format(eligible_count, big.mark = ","),
    "\n",
    sep = ""
  )
  
  cat(
    "Occurrences without FINAL_WCVP_ID: ",
    format(ineligible_count, big.mark = ","),
    "\n",
    sep = ""
  )
  
  cat(
    "Distinct WCVP IDs represented: ",
    format(distribution_taxa, big.mark = ","),
    "\n",
    sep = ""
  )
  
  cat(
    "Taxon-area presence rows: ",
    format(distribution_rows, big.mark = ","),
    "\n",
    sep = ""
  )
  
  cat(
    "Botanical areas represented: ",
    distribution_areas,
    "\n",
    sep = ""
  )
  
  cat(
    "WCVP IDs with incomplete source metadata: ",
    incomplete_taxa,
    "\n",
    sep = ""
  )
  
  cat(
    "Genuine WCVP metadata conflicts: ",
    genuine_conflict_count,
    "\n",
    sep = ""
  )
  
  cat(
    "Taxon-area rows lacking canonical metadata: ",
    output_missing_metadata,
    "\n",
    sep = ""
  )
  
  cat("Historical Key used: FALSE\n")
  cat("Native/alien status inferred: FALSE\n")
  cat("Occurrences modified: FALSE\n")
  cat("Taxonomy modified: FALSE\n")
  cat("Geography modified: FALSE\n")
  cat("Quarantine modified: FALSE\n")
  cat("Stars calculated: FALSE\n")
  cat("GHI calculated: FALSE\n")
  
  cat(
    "\nDistribution 01 v",
    SCRIPT_VERSION,
    " complete.\n",
    sep = ""
  )
  
  invisible(
    list(
      eligibility_summary = eligibility_summary,
      concept_profile = concept_profile,
      reconciliation_profile = reconciliation_profile,
      metadata_completeness = metadata_completeness,
      metadata_consistency = metadata_consistency,
      metadata_unavailable = metadata_unavailable,
      taxon_summary = taxon_summary,
      area_summary = area_summary,
      breadth_summary = breadth_summary,
      evidence_summary = evidence_summary,
      validation = validation,
      metadata = metadata
    )
  )
}

result_distribution_01 <-
  run_japan_taxon_area_distribution()