# ==============================================================================
# VPJD-OJPCP
# Distribution 02 — Build contemporary Japanese taxon distribution matrix
# Version: 0.1.0
#
# Purpose:
#   Convert the validated long-format Japanese taxon-area distribution into
#   the canonical contemporary taxon-level distribution dataset.
#
# Source:
#   vpjd_japan_taxon_area_distribution
#
# Canonical outputs:
#   vpjd_japan_taxon_distribution
#   vpjd_japan_taxon_area_matrix
#
# Grain:
#   One row per FINAL_WCVP_ID.
#
# Outputs include:
#   - canonical WCVP metadata;
#   - number/list of occupied botanical areas;
#   - occurrence evidence totals;
#   - national GBIF / recovered institutional evidence;
#   - JP01_PRESENT ... JP51_PRESENT.
#
# This module does NOT:
#   - use the historical Nakamura/Key distribution;
#   - infer districts;
#   - infer endemicity;
#   - infer native/alien/cultivated status;
#   - impose an occurrence-evidence threshold;
#   - calculate Stars or GHI;
#   - modify occurrence, taxonomy, geography or quarantine data.
# ==============================================================================

suppressPackageStartupMessages({
  library(DBI)
  library(duckdb)
  library(dplyr)
  library(readr)
  library(here)
})

SCRIPT_VERSION <- "0.1.0"
SOURCE_TABLE <- "vpjd_japan_taxon_area_distribution"
OUTPUT_TABLE <- "vpjd_japan_taxon_distribution"
MATRIX_TABLE <- "vpjd_japan_taxon_area_matrix"
EXPECTED_TAXA <- 11484L
EXPECTED_TAXON_AREA_ROWS <- 128876L
EXPECTED_AREAS <- 51L
EXPECTED_OCCURRENCES <- 2851736L
RUN_DATE <- format(Sys.Date(), "%Y-%m-%d")

run_japan_taxon_distribution_matrix <- function() {
  cat("\n— VPJD contemporary Japanese taxon distribution matrix —\n\n")
  cat("Run date: ", RUN_DATE, "\n\n", sep = "")
  
  db_path <- here(
    "data", "interim", "occurrences",
    "vpjd_occurrences.duckdb"
  )
  
  output_dir <- here(
    "outputs", "tables", "distribution",
    "japan_taxon_distribution"
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
  
  q_source <- quote_id(SOURCE_TABLE)
  q_output <- quote_id(OUTPUT_TABLE)
  q_matrix <- quote_id(MATRIX_TABLE)
  
  # ---------------------------------------------------------------------------
  # Source validation
  # ---------------------------------------------------------------------------
  
  if (!(SOURCE_TABLE %in% dbListTables(con))) {
    stop(
      "Validated Distribution 01 table not found: ",
      SOURCE_TABLE
    )
  }
  
  source_fields <- dbListFields(
    con,
    SOURCE_TABLE
  )
  
  required_fields <- c(
    "FINAL_WCVP_ID",
    "FINAL_WCVP_RECOGNISED_NAME",
    "FINAL_WCVP_RANK",
    "FINAL_WCVP_STATUS",
    "FINAL_WCVP_CONCEPT_CLASS",
    "BOTANICAL_AREA_ID",
    "BOTANICAL_AREA_NAME",
    "OCCURRENCE_RECORDS",
    "DISTINCT_GBIF_IDS",
    "NATIONAL_GBIF_RECORDS",
    "RECOVERED_INSTITUTIONAL_RECORDS",
    "PRESENCE"
  )
  
  missing_fields <- setdiff(
    required_fields,
    source_fields
  )
  
  if (length(missing_fields) > 0L) {
    stop(
      "Required Distribution 01 fields missing:\n",
      paste(missing_fields, collapse = ", ")
    )
  }
  
  source_rows <- dbGetQuery(
    con,
    paste0(
      "SELECT COUNT(*) AS n FROM ",
      q_source
    )
  )$n[[1]]
  
  source_taxa <- dbGetQuery(
    con,
    paste0(
      "SELECT COUNT(DISTINCT FINAL_WCVP_ID) AS n FROM ",
      q_source
    )
  )$n[[1]]
  
  source_areas <- dbGetQuery(
    con,
    paste0(
      "SELECT COUNT(DISTINCT BOTANICAL_AREA_ID) AS n FROM ",
      q_source
    )
  )$n[[1]]
  
  source_occurrences <- dbGetQuery(
    con,
    paste0(
      "SELECT SUM(OCCURRENCE_RECORDS) AS n FROM ",
      q_source
    )
  )$n[[1]]
  
  source_gbif_ids <- dbGetQuery(
    con,
    paste0(
      "SELECT SUM(DISTINCT_GBIF_IDS) AS n FROM ",
      q_source
    )
  )$n[[1]]
  
  duplicate_pairs <- dbGetQuery(
    con,
    paste0(
      "SELECT COUNT(*) AS n FROM (",
      "SELECT FINAL_WCVP_ID, BOTANICAL_AREA_ID, COUNT(*) AS n ",
      "FROM ",
      q_source,
      " GROUP BY FINAL_WCVP_ID, BOTANICAL_AREA_ID ",
      "HAVING COUNT(*) > 1",
      ") x"
    )
  )$n[[1]]
  
  cat("Distribution 01 taxon-area rows: ",
      format(source_rows, big.mark = ","), "\n", sep = "")
  cat("Distinct WCVP IDs: ",
      format(source_taxa, big.mark = ","), "\n", sep = "")
  cat("Botanical areas represented: ",
      source_areas, "\n", sep = "")
  cat("Occurrence evidence represented: ",
      format(source_occurrences, big.mark = ","), "\n\n", sep = "")
  
  if (source_rows != EXPECTED_TAXON_AREA_ROWS) {
    stop("Unexpected Distribution 01 taxon-area row count.")
  }
  
  if (source_taxa != EXPECTED_TAXA) {
    stop("Unexpected Distribution 01 WCVP taxon count.")
  }
  
  if (source_areas != EXPECTED_AREAS) {
    stop("Distribution 01 does not represent all 51 botanical areas.")
  }
  
  if (source_occurrences != EXPECTED_OCCURRENCES) {
    stop("Unexpected Distribution 01 occurrence evidence count.")
  }
  
  if (source_gbif_ids != EXPECTED_OCCURRENCES) {
    stop("Distribution 01 gbifID evidence does not reconcile.")
  }
  
  if (duplicate_pairs != 0L) {
    stop("Duplicate WCVP ID x botanical-area pairs detected.")
  }
  
  cat("Distribution 01 source validation: PASS\n")
  
  # ---------------------------------------------------------------------------
  # Metadata consistency
  # ---------------------------------------------------------------------------
  
  metadata_conflicts <- dbGetQuery(
    con,
    paste0(
      "SELECT COUNT(*) AS n FROM (",
      "SELECT FINAL_WCVP_ID ",
      "FROM ",
      q_source,
      " GROUP BY FINAL_WCVP_ID ",
      "HAVING ",
      "COUNT(DISTINCT FINAL_WCVP_RECOGNISED_NAME) > 1 ",
      "OR COUNT(DISTINCT FINAL_WCVP_RANK) > 1 ",
      "OR COUNT(DISTINCT FINAL_WCVP_STATUS) > 1 ",
      "OR COUNT(DISTINCT FINAL_WCVP_CONCEPT_CLASS) > 1",
      ") x"
    )
  )$n[[1]]
  
  missing_metadata_taxa <- dbGetQuery(
    con,
    paste0(
      "SELECT COUNT(*) AS n FROM (",
      "SELECT FINAL_WCVP_ID ",
      "FROM ",
      q_source,
      " GROUP BY FINAL_WCVP_ID ",
      "HAVING ",
      "COUNT(FINAL_WCVP_RECOGNISED_NAME) = 0 ",
      "OR COUNT(FINAL_WCVP_RANK) = 0 ",
      "OR COUNT(FINAL_WCVP_STATUS) = 0 ",
      "OR COUNT(FINAL_WCVP_CONCEPT_CLASS) = 0",
      ") x"
    )
  )$n[[1]]
  
  cat("\n— Taxon metadata QA —\n")
  cat("WCVP IDs with conflicting metadata: ",
      metadata_conflicts, "\n", sep = "")
  cat("WCVP IDs lacking canonical metadata: ",
      missing_metadata_taxa, "\n", sep = "")
  
  if (metadata_conflicts != 0L) {
    stop("Conflicting taxon metadata detected.")
  }
  
  if (missing_metadata_taxa != 0L) {
    stop("One or more WCVP IDs lack canonical metadata.")
  }
  
  cat("Taxon metadata validation: PASS\n")
  
  # ---------------------------------------------------------------------------
  # Build JP01–JP51 SQL expressions
  # ---------------------------------------------------------------------------
  
  area_ids <- sprintf(
    "JP%02d",
    1:51
  )
  
  presence_sql <- paste(
    vapply(
      area_ids,
      function(area) {
        paste0(
          "MAX(CASE WHEN BOTANICAL_AREA_ID = '",
          area,
          "' THEN 1 ELSE 0 END) AS ",
          area,
          "_PRESENT"
        )
      },
      character(1)
    ),
    collapse = ", "
  )
  
  # ---------------------------------------------------------------------------
  # Rebuild derived outputs
  # ---------------------------------------------------------------------------
  
  existing_tables <- dbListTables(con)
  
  if (OUTPUT_TABLE %in% existing_tables) {
    cat(
      "\nExisting derived taxon distribution table detected; rebuilding...\n"
    )
    
    dbExecute(
      con,
      paste0(
        "DROP TABLE ",
        q_output
      )
    )
  }
  
  if (MATRIX_TABLE %in% existing_tables) {
    cat(
      "Existing derived taxon-area matrix detected; rebuilding...\n"
    )
    
    dbExecute(
      con,
      paste0(
        "DROP TABLE ",
        q_matrix
      )
    )
  }
  
  # ---------------------------------------------------------------------------
  # Canonical taxon distribution
  # ---------------------------------------------------------------------------
  
  cat("\nBuilding canonical taxon distribution...\n")
  
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
      "COUNT(*) AS BOTANICAL_AREAS_PRESENT, ",
      "STRING_AGG(BOTANICAL_AREA_ID, ',' ORDER BY BOTANICAL_AREA_ID) ",
      "AS BOTANICAL_AREA_IDS, ",
      "SUM(OCCURRENCE_RECORDS) AS OCCURRENCE_RECORDS, ",
      "SUM(NATIONAL_GBIF_RECORDS) AS NATIONAL_GBIF_RECORDS, ",
      "SUM(RECOVERED_INSTITUTIONAL_RECORDS) ",
      "AS RECOVERED_INSTITUTIONAL_RECORDS, ",
      "MIN(OCCURRENCE_RECORDS) AS MIN_RECORDS_PER_PRESENT_AREA, ",
      "MAX(OCCURRENCE_RECORDS) AS MAX_RECORDS_PER_PRESENT_AREA, ",
      "SUM(CASE WHEN OCCURRENCE_RECORDS = 1 THEN 1 ELSE 0 END) ",
      "AS SINGLE_RECORD_AREA_PRESENCES, ",
      presence_sql,
      ", '02' AS DISTRIBUTION_MODULE, ",
      "'",
      SCRIPT_VERSION,
      "' AS DISTRIBUTION_VERSION ",
      "FROM ",
      q_source,
      " GROUP BY FINAL_WCVP_ID"
    )
  )
  
  # ---------------------------------------------------------------------------
  # Matrix-only derived table
  #
  # Kept separately because it is useful for later numerical analysis while
  # the canonical taxon distribution retains names/evidence/provenance.
  # ---------------------------------------------------------------------------
  
  cat("Building taxon-area presence matrix...\n")
  
  matrix_fields <- paste(
    paste0(area_ids, "_PRESENT"),
    collapse = ", "
  )
  
  dbExecute(
    con,
    paste0(
      "CREATE TABLE ",
      q_matrix,
      " AS ",
      "SELECT ",
      "FINAL_WCVP_ID, ",
      matrix_fields,
      " FROM ",
      q_output
    )
  )
  
  # ---------------------------------------------------------------------------
  # Validate canonical taxon table
  # ---------------------------------------------------------------------------
  
  output_taxa <- dbGetQuery(
    con,
    paste0(
      "SELECT COUNT(*) AS n FROM ",
      q_output
    )
  )$n[[1]]
  
  output_ids <- dbGetQuery(
    con,
    paste0(
      "SELECT COUNT(DISTINCT FINAL_WCVP_ID) AS n FROM ",
      q_output
    )
  )$n[[1]]
  
  output_occurrences <- dbGetQuery(
    con,
    paste0(
      "SELECT SUM(OCCURRENCE_RECORDS) AS n FROM ",
      q_output
    )
  )$n[[1]]
  
  output_area_sum <- dbGetQuery(
    con,
    paste0(
      "SELECT SUM(BOTANICAL_AREAS_PRESENT) AS n FROM ",
      q_output
    )
  )$n[[1]]
  
  invalid_area_counts <- dbGetQuery(
    con,
    paste0(
      "SELECT COUNT(*) AS n FROM ",
      q_output,
      " WHERE BOTANICAL_AREAS_PRESENT < 1 ",
      "OR BOTANICAL_AREAS_PRESENT > 51"
    )
  )$n[[1]]
  
  missing_output_metadata <- dbGetQuery(
    con,
    paste0(
      "SELECT COUNT(*) AS n FROM ",
      q_output,
      " WHERE FINAL_WCVP_RECOGNISED_NAME IS NULL ",
      "OR FINAL_WCVP_RANK IS NULL ",
      "OR FINAL_WCVP_STATUS IS NULL ",
      "OR FINAL_WCVP_CONCEPT_CLASS IS NULL"
    )
  )$n[[1]]
  
  invalid_presence_values <- dbGetQuery(
    con,
    paste0(
      "SELECT COUNT(*) AS n FROM ",
      q_output,
      " WHERE ",
      paste(
        paste0(
          area_ids,
          "_PRESENT NOT IN (0,1) OR ",
          area_ids,
          "_PRESENT IS NULL"
        ),
        collapse = " OR "
      )
    )
  )$n[[1]]
  
  presence_sum_expression <- paste(
    paste0(area_ids, "_PRESENT"),
    collapse = " + "
  )
  
  area_count_mismatches <- dbGetQuery(
    con,
    paste0(
      "SELECT COUNT(*) AS n FROM ",
      q_output,
      " WHERE BOTANICAL_AREAS_PRESENT <> (",
      presence_sum_expression,
      ")"
    )
  )$n[[1]]
  
  matrix_rows <- dbGetQuery(
    con,
    paste0(
      "SELECT COUNT(*) AS n FROM ",
      q_matrix
    )
  )$n[[1]]
  
  matrix_ids <- dbGetQuery(
    con,
    paste0(
      "SELECT COUNT(DISTINCT FINAL_WCVP_ID) AS n FROM ",
      q_matrix
    )
  )$n[[1]]
  
  cat("\n— Canonical taxon distribution —\n")
  cat("Taxa: ",
      format(output_taxa, big.mark = ","), "\n", sep = "")
  cat("Taxon-area presences represented: ",
      format(output_area_sum, big.mark = ","), "\n", sep = "")
  cat("Occurrence evidence represented: ",
      format(output_occurrences, big.mark = ","), "\n", sep = "")
  cat("Presence matrix rows: ",
      format(matrix_rows, big.mark = ","), "\n", sep = "")
  
  # ---------------------------------------------------------------------------
  # Distribution breadth profile
  # ---------------------------------------------------------------------------
  
  breadth_summary <- dbGetQuery(
    con,
    paste0(
      "SELECT ",
      "BOTANICAL_AREAS_PRESENT, ",
      "COUNT(*) AS WCVP_TAXA ",
      "FROM ",
      q_output,
      " GROUP BY BOTANICAL_AREAS_PRESENT ",
      "ORDER BY BOTANICAL_AREAS_PRESENT"
    )
  )
  
  cat("\n— Distribution breadth —\n")
  
  print.data.frame(
    breadth_summary,
    row.names = FALSE
  )
  
  # ---------------------------------------------------------------------------
  # Rank/concept profile
  # ---------------------------------------------------------------------------
  
  taxonomic_profile <- dbGetQuery(
    con,
    paste0(
      "SELECT ",
      "FINAL_WCVP_CONCEPT_CLASS, ",
      "FINAL_WCVP_STATUS, ",
      "FINAL_WCVP_RANK, ",
      "COUNT(*) AS WCVP_TAXA, ",
      "SUM(OCCURRENCE_RECORDS) AS OCCURRENCE_RECORDS ",
      "FROM ",
      q_output,
      " GROUP BY ",
      "FINAL_WCVP_CONCEPT_CLASS, ",
      "FINAL_WCVP_STATUS, ",
      "FINAL_WCVP_RANK ",
      "ORDER BY WCVP_TAXA DESC"
    )
  )
  
  cat("\n— Taxonomic profile —\n")
  
  print.data.frame(
    taxonomic_profile,
    row.names = FALSE
  )
  
  # ---------------------------------------------------------------------------
  # Evidence profile
  #
  # Descriptive only; no evidence threshold is imposed.
  # ---------------------------------------------------------------------------
  
  evidence_profile <- dbGetQuery(
    con,
    paste0(
      "SELECT ",
      "CASE ",
      "WHEN OCCURRENCE_RECORDS = 1 THEN '1_record' ",
      "WHEN OCCURRENCE_RECORDS BETWEEN 2 AND 5 THEN '2_5_records' ",
      "WHEN OCCURRENCE_RECORDS BETWEEN 6 AND 10 THEN '6_10_records' ",
      "WHEN OCCURRENCE_RECORDS BETWEEN 11 AND 50 THEN '11_50_records' ",
      "WHEN OCCURRENCE_RECORDS BETWEEN 51 AND 100 THEN '51_100_records' ",
      "ELSE 'gt_100_records' ",
      "END AS TOTAL_EVIDENCE_CLASS, ",
      "COUNT(*) AS WCVP_TAXA ",
      "FROM ",
      q_output,
      " GROUP BY 1 ",
      "ORDER BY ",
      "CASE TOTAL_EVIDENCE_CLASS ",
      "WHEN '1_record' THEN 1 ",
      "WHEN '2_5_records' THEN 2 ",
      "WHEN '6_10_records' THEN 3 ",
      "WHEN '11_50_records' THEN 4 ",
      "WHEN '51_100_records' THEN 5 ",
      "ELSE 6 END"
    )
  )
  
  cat("\n— Total occurrence evidence per WCVP concept —\n")
  
  print.data.frame(
    evidence_profile,
    row.names = FALSE
  )
  
  # ---------------------------------------------------------------------------
  # Single-area taxa
  # ---------------------------------------------------------------------------
  
  single_area_summary <- dbGetQuery(
    con,
    paste0(
      "SELECT ",
      "BOTANICAL_AREA_IDS AS BOTANICAL_AREA_ID, ",
      "COUNT(*) AS WCVP_TAXA ",
      "FROM ",
      q_output,
      " WHERE BOTANICAL_AREAS_PRESENT = 1 ",
      "GROUP BY BOTANICAL_AREA_IDS ",
      "ORDER BY BOTANICAL_AREA_IDS"
    )
  )
  
  cat("\n— WCVP concepts currently recorded from one botanical area —\n")
  
  print.data.frame(
    single_area_summary,
    row.names = FALSE
  )
  
  # ---------------------------------------------------------------------------
  # Validation
  # ---------------------------------------------------------------------------
  
  validation <- tibble(
    check = c(
      "source_taxon_area_rows_128876",
      "source_WCVP_taxa_11484",
      "source_botanical_areas_51",
      "source_occurrences_2851736",
      "source_gbifID_evidence_2851736",
      "no_duplicate_source_taxon_area_pairs",
      "no_taxon_metadata_conflicts",
      "no_taxa_lacking_canonical_metadata",
      "output_taxa_11484",
      "output_WCVP_IDs_unique",
      "output_occurrences_2851736",
      "output_area_sum_128876",
      "all_taxa_present_in_1_to_51_areas",
      "no_missing_output_taxon_metadata",
      "all_matrix_values_binary",
      "area_counts_equal_matrix_row_sums",
      "matrix_rows_11484",
      "matrix_WCVP_IDs_unique",
      "historical_key_not_used",
      "districts_not_inferred",
      "endemicity_not_inferred",
      "native_status_not_inferred",
      "alien_status_not_inferred",
      "evidence_threshold_not_imposed",
      "occurrences_not_modified",
      "taxonomy_not_modified",
      "geography_not_modified",
      "quarantine_not_modified",
      "stars_not_calculated",
      "ghi_not_calculated"
    ),
    pass = c(
      source_rows == EXPECTED_TAXON_AREA_ROWS,
      source_taxa == EXPECTED_TAXA,
      source_areas == EXPECTED_AREAS,
      source_occurrences == EXPECTED_OCCURRENCES,
      source_gbif_ids == EXPECTED_OCCURRENCES,
      duplicate_pairs == 0L,
      metadata_conflicts == 0L,
      missing_metadata_taxa == 0L,
      output_taxa == EXPECTED_TAXA,
      output_ids == output_taxa,
      output_occurrences == EXPECTED_OCCURRENCES,
      output_area_sum == EXPECTED_TAXON_AREA_ROWS,
      invalid_area_counts == 0L,
      missing_output_metadata == 0L,
      invalid_presence_values == 0L,
      area_count_mismatches == 0L,
      matrix_rows == EXPECTED_TAXA,
      matrix_ids == matrix_rows,
      TRUE,
      TRUE,
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
    "\nAll taxon distribution matrix validation checks PASS: ",
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
      "source_table",
      "canonical_taxon_table",
      "matrix_table",
      "source_taxon_area_rows",
      "taxa",
      "botanical_areas",
      "occurrence_evidence",
      "canonical_grain",
      "matrix_columns",
      "presence_definition",
      "historical_key_used",
      "districts_inferred",
      "endemicity_inferred",
      "native_status_inferred",
      "alien_status_inferred",
      "evidence_threshold_imposed",
      "stars_calculated",
      "ghi_calculated",
      "validation_pass"
    ),
    value = c(
      "02_build_japan_taxon_distribution_matrix",
      SCRIPT_VERSION,
      RUN_DATE,
      SOURCE_TABLE,
      OUTPUT_TABLE,
      MATRIX_TABLE,
      as.character(source_rows),
      as.character(output_taxa),
      as.character(EXPECTED_AREAS),
      as.character(output_occurrences),
      "one_row_per_FINAL_WCVP_ID",
      "JP01_PRESENT_to_JP51_PRESENT",
      "one_or_more_validated_taxon_area_occurrence_records",
      "FALSE",
      "FALSE",
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
  # QA exports
  # ---------------------------------------------------------------------------
  
  write_csv(
    breadth_summary,
    file.path(
      output_dir,
      "taxon_distribution_breadth.csv"
    )
  )
  
  write_csv(
    taxonomic_profile,
    file.path(
      output_dir,
      "taxon_distribution_taxonomic_profile.csv"
    )
  )
  
  write_csv(
    evidence_profile,
    file.path(
      output_dir,
      "taxon_distribution_evidence_profile.csv"
    )
  )
  
  write_csv(
    single_area_summary,
    file.path(
      output_dir,
      "single_area_taxa_summary.csv"
    )
  )
  
  write_csv(
    validation,
    file.path(
      output_dir,
      "taxon_distribution_validation.csv"
    )
  )
  
  write_csv(
    metadata,
    file.path(
      output_dir,
      "taxon_distribution_metadata.csv"
    )
  )
  
  dbWriteTable(
    con,
    "vpjd_japan_taxon_distribution_validation",
    validation,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "vpjd_japan_taxon_distribution_metadata",
    metadata,
    overwrite = TRUE
  )
  
  # ---------------------------------------------------------------------------
  # Final status
  # ---------------------------------------------------------------------------
  
  if (!all_pass) {
    cat(
      "\nOutput status: FAILED TAXON DISTRIBUTION MATRIX VALIDATION\n"
    )
    
    stop(
      paste(
        "Distribution 02 validation failed.",
        "Do not use the taxon distribution downstream."
      )
    )
  }
  
  cat("\nCanonical taxon distribution table:\n")
  cat("  ", OUTPUT_TABLE, "\n", sep = "")
  
  cat("\nCanonical presence matrix:\n")
  cat("  ", MATRIX_TABLE, "\n", sep = "")
  
  cat(
    "\nOutput status: ",
    "VALIDATED CONTEMPORARY JAPANESE TAXON DISTRIBUTION MATRIX\n",
    sep = ""
  )
  
  cat(
    "WCVP concepts: ",
    format(output_taxa, big.mark = ","),
    "\n",
    sep = ""
  )
  
  cat(
    "Taxon-area presences: ",
    format(output_area_sum, big.mark = ","),
    "\n",
    sep = ""
  )
  
  cat(
    "Occurrence evidence: ",
    format(output_occurrences, big.mark = ","),
    "\n",
    sep = ""
  )
  
  cat(
    "Botanical-area presence fields: ",
    EXPECTED_AREAS,
    "\n",
    sep = ""
  )
  
  cat("Historical Key used: FALSE\n")
  cat("Districts inferred: FALSE\n")
  cat("Endemicity inferred: FALSE\n")
  cat("Native/alien status inferred: FALSE\n")
  cat("Evidence threshold imposed: FALSE\n")
  cat("Occurrences modified: FALSE\n")
  cat("Taxonomy modified: FALSE\n")
  cat("Geography modified: FALSE\n")
  cat("Quarantine modified: FALSE\n")
  cat("Stars calculated: FALSE\n")
  cat("GHI calculated: FALSE\n")
  
  cat(
    "\nDistribution 02 v",
    SCRIPT_VERSION,
    " complete.\n",
    sep = ""
  )
  
  invisible(
    list(
      breadth_summary = breadth_summary,
      taxonomic_profile = taxonomic_profile,
      evidence_profile = evidence_profile,
      single_area_summary = single_area_summary,
      validation = validation,
      metadata = metadata
    )
  )
}

result_distribution_02 <-
  run_japan_taxon_distribution_matrix()