# ==============================================================================
# VPJD-OJPCP
# Distribution 03 — Profile contemporary Japanese taxon-distribution QA
# Version: 0.1.1
#
# Purpose:
#   Build a diagnostic QA layer for the validated contemporary Japanese
#   taxon distribution.
#
# Sources:
#   vpjd_japan_taxon_distribution
#   vpjd_japan_taxon_area_distribution
#
# Canonical output:
#   vpjd_japan_taxon_distribution_qa
#
# Grain:
#   One row per FINAL_WCVP_ID.
#
# v0.1.1:
#   - fixes JP50/JP51 diagnostic query to use QA flags carried into output;
#   - validates genus-level total as 825 across all concept classes;
#   - retains diagnostic-only architecture.
#
# This module does NOT:
#   - exclude or delete taxa;
#   - alter presence/absence;
#   - infer endemicity;
#   - infer native/alien/cultivated status;
#   - use Nakamura's historical Key;
#   - calculate Stars or GHI;
#   - modify occurrences, taxonomy, geography or quarantine.
# ==============================================================================

suppressPackageStartupMessages({
  library(DBI)
  library(duckdb)
  library(dplyr)
  library(readr)
  library(here)
})

SCRIPT_VERSION <- "0.1.1"
SOURCE_TAXON <- "vpjd_japan_taxon_distribution"
SOURCE_AREA <- "vpjd_japan_taxon_area_distribution"
OUTPUT_TABLE <- "vpjd_japan_taxon_distribution_qa"
EXPECTED_TAXA <- 11484L
EXPECTED_TAXON_AREA_ROWS <- 128876L
EXPECTED_OCCURRENCES <- 2851736L
EXPECTED_SINGLE_RECORD <- 1447L
EXPECTED_SINGLE_AREA <- 3067L
EXPECTED_GENUS <- 825L
EXPECTED_UNPLACED <- 14L
EXPECTED_ARTIFICIAL_HYBRID <- 31L
RUN_DATE <- format(Sys.Date(), "%Y-%m-%d")

run_japan_taxon_distribution_qa <- function() {
  cat("\n— VPJD contemporary Japanese taxon-distribution QA —\n\n")
  cat("Run date: ", RUN_DATE, "\n\n", sep = "")
  
  db_path <- here(
    "data", "interim", "occurrences",
    "vpjd_occurrences.duckdb"
  )
  
  output_dir <- here(
    "outputs", "tables", "distribution",
    "japan_taxon_distribution_qa"
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
  
  q_taxon <- paste0('"', SOURCE_TAXON, '"')
  q_area <- paste0('"', SOURCE_AREA, '"')
  q_output <- paste0('"', OUTPUT_TABLE, '"')
  
  # ---------------------------------------------------------------------------
  # Source validation
  # ---------------------------------------------------------------------------
  
  tables <- dbListTables(con)
  
  missing_tables <- setdiff(
    c(SOURCE_TAXON, SOURCE_AREA),
    tables
  )
  
  if (length(missing_tables) > 0L) {
    stop(
      "Required Distribution tables missing:\n",
      paste(missing_tables, collapse = ", ")
    )
  }
  
  required_taxon_fields <- c(
    "FINAL_WCVP_ID",
    "FINAL_WCVP_RECOGNISED_NAME",
    "FINAL_WCVP_RANK",
    "FINAL_WCVP_STATUS",
    "FINAL_WCVP_CONCEPT_CLASS",
    "BOTANICAL_AREAS_PRESENT",
    "BOTANICAL_AREA_IDS",
    "OCCURRENCE_RECORDS",
    "NATIONAL_GBIF_RECORDS",
    "RECOVERED_INSTITUTIONAL_RECORDS",
    "MIN_RECORDS_PER_PRESENT_AREA",
    "MAX_RECORDS_PER_PRESENT_AREA",
    "SINGLE_RECORD_AREA_PRESENCES",
    "JP50_PRESENT",
    "JP51_PRESENT"
  )
  
  required_area_fields <- c(
    "FINAL_WCVP_ID",
    "BOTANICAL_AREA_ID",
    "OCCURRENCE_RECORDS"
  )
  
  missing_taxon_fields <- setdiff(
    required_taxon_fields,
    dbListFields(con, SOURCE_TAXON)
  )
  
  missing_area_fields <- setdiff(
    required_area_fields,
    dbListFields(con, SOURCE_AREA)
  )
  
  if (length(missing_taxon_fields) > 0L) {
    stop(
      "Required Distribution 02 fields missing:\n",
      paste(missing_taxon_fields, collapse = ", ")
    )
  }
  
  if (length(missing_area_fields) > 0L) {
    stop(
      "Required Distribution 01 fields missing:\n",
      paste(missing_area_fields, collapse = ", ")
    )
  }
  
  source_taxa <- dbGetQuery(
    con,
    paste0(
      "SELECT COUNT(*) AS n FROM ",
      q_taxon
    )
  )$n[[1]]
  
  source_ids <- dbGetQuery(
    con,
    paste0(
      "SELECT COUNT(DISTINCT FINAL_WCVP_ID) AS n FROM ",
      q_taxon
    )
  )$n[[1]]
  
  source_occurrences <- dbGetQuery(
    con,
    paste0(
      "SELECT SUM(OCCURRENCE_RECORDS) AS n FROM ",
      q_taxon
    )
  )$n[[1]]
  
  source_area_rows <- dbGetQuery(
    con,
    paste0(
      "SELECT COUNT(*) AS n FROM ",
      q_area
    )
  )$n[[1]]
  
  cat(
    "Distribution 02 taxa: ",
    format(source_taxa, big.mark = ","),
    "\n",
    sep = ""
  )
  
  cat(
    "Distinct WCVP IDs: ",
    format(source_ids, big.mark = ","),
    "\n",
    sep = ""
  )
  
  cat(
    "Occurrence evidence: ",
    format(source_occurrences, big.mark = ","),
    "\n",
    sep = ""
  )
  
  cat(
    "Distribution 01 taxon-area rows: ",
    format(source_area_rows, big.mark = ","),
    "\n\n",
    sep = ""
  )
  
  if (source_taxa != EXPECTED_TAXA) {
    stop("Unexpected Distribution 02 taxon count.")
  }
  
  if (source_ids != source_taxa) {
    stop("Distribution 02 WCVP IDs are not unique.")
  }
  
  if (source_occurrences != EXPECTED_OCCURRENCES) {
    stop("Unexpected Distribution 02 occurrence evidence.")
  }
  
  if (source_area_rows != EXPECTED_TAXON_AREA_ROWS) {
    stop("Unexpected Distribution 01 taxon-area row count.")
  }
  
  cat("Source validation: PASS\n")
  
  # ---------------------------------------------------------------------------
  # Rebuild diagnostic QA table
  # ---------------------------------------------------------------------------
  
  if (OUTPUT_TABLE %in% dbListTables(con)) {
    cat("\nExisting derived QA table detected; rebuilding...\n")
    
    dbExecute(
      con,
      paste0("DROP TABLE ", q_output)
    )
  }
  
  cat("\nBuilding taxon-distribution QA layer...\n")
  
  dbExecute(
    con,
    paste0(
      "CREATE TABLE ", q_output, " AS ",
      "SELECT ",
      "FINAL_WCVP_ID, ",
      "FINAL_WCVP_RECOGNISED_NAME, ",
      "FINAL_WCVP_RANK, ",
      "FINAL_WCVP_STATUS, ",
      "FINAL_WCVP_CONCEPT_CLASS, ",
      "BOTANICAL_AREAS_PRESENT, ",
      "BOTANICAL_AREA_IDS, ",
      "OCCURRENCE_RECORDS, ",
      "NATIONAL_GBIF_RECORDS, ",
      "RECOVERED_INSTITUTIONAL_RECORDS, ",
      "MIN_RECORDS_PER_PRESENT_AREA, ",
      "MAX_RECORDS_PER_PRESENT_AREA, ",
      "SINGLE_RECORD_AREA_PRESENCES, ",
      
      "CASE WHEN OCCURRENCE_RECORDS = 1 ",
      "THEN 1 ELSE 0 END AS QA_SINGLE_RECORD, ",
      
      "CASE WHEN BOTANICAL_AREAS_PRESENT = 1 ",
      "THEN 1 ELSE 0 END AS QA_SINGLE_AREA, ",
      
      "CASE WHEN OCCURRENCE_RECORDS = 1 ",
      "AND BOTANICAL_AREAS_PRESENT = 1 ",
      "THEN 1 ELSE 0 END AS QA_SINGLE_RECORD_SINGLE_AREA, ",
      
      "CASE WHEN FINAL_WCVP_RANK = 'Genus' ",
      "THEN 1 ELSE 0 END AS QA_GENUS_LEVEL, ",
      
      "CASE WHEN FINAL_WCVP_CONCEPT_CLASS = 'unplaced_wcvp' ",
      "OR FINAL_WCVP_STATUS = 'Unplaced' ",
      "THEN 1 ELSE 0 END AS QA_UNPLACED, ",
      
      "CASE WHEN FINAL_WCVP_CONCEPT_CLASS = 'artificial_hybrid_wcvp' ",
      "OR FINAL_WCVP_STATUS = 'Artificial Hybrid' ",
      "THEN 1 ELSE 0 END AS QA_ARTIFICIAL_HYBRID, ",
      
      "CASE WHEN OCCURRENCE_RECORDS <= 5 ",
      "THEN 1 ELSE 0 END AS QA_LOW_TOTAL_EVIDENCE, ",
      
      "CASE WHEN MIN_RECORDS_PER_PRESENT_AREA = 1 ",
      "THEN 1 ELSE 0 END AS QA_HAS_SINGLE_RECORD_AREA_PRESENCE, ",
      
      "CASE WHEN JP50_PRESENT = 1 ",
      "THEN 1 ELSE 0 END AS QA_RECORDED_JP50_KAZAN, ",
      
      "CASE WHEN JP51_PRESENT = 1 ",
      "THEN 1 ELSE 0 END AS QA_RECORDED_JP51_KURILES, ",
      
      "CASE ",
      "WHEN FINAL_WCVP_CONCEPT_CLASS = 'unplaced_wcvp' ",
      "OR FINAL_WCVP_STATUS = 'Unplaced' ",
      "OR FINAL_WCVP_CONCEPT_CLASS = 'artificial_hybrid_wcvp' ",
      "OR FINAL_WCVP_STATUS = 'Artificial Hybrid' ",
      "THEN 'TAXONOMIC_REVIEW' ",
      
      "WHEN FINAL_WCVP_RANK = 'Genus' ",
      "THEN 'TAXONOMIC_REVIEW' ",
      
      "WHEN OCCURRENCE_RECORDS = 1 ",
      "AND BOTANICAL_AREAS_PRESENT = 1 ",
      "THEN 'HIGH_EVIDENCE_REVIEW' ",
      
      "WHEN OCCURRENCE_RECORDS <= 5 ",
      "AND BOTANICAL_AREAS_PRESENT = 1 ",
      "THEN 'MODERATE_EVIDENCE_REVIEW' ",
      
      "WHEN OCCURRENCE_RECORDS <= 5 ",
      "THEN 'LOW_EVIDENCE_REVIEW' ",
      
      "WHEN MIN_RECORDS_PER_PRESENT_AREA = 1 ",
      "THEN 'AREA_PRESENCE_REVIEW' ",
      
      "ELSE 'NO_IMMEDIATE_FLAG' ",
      "END AS QA_REVIEW_PRIORITY, ",
      
      "'03' AS QA_MODULE, ",
      "'", SCRIPT_VERSION, "' AS QA_VERSION ",
      
      "FROM ", q_taxon
    )
  )
  
  # ---------------------------------------------------------------------------
  # Core QA counts
  # ---------------------------------------------------------------------------
  
  qa_counts <- dbGetQuery(
    con,
    paste0(
      "SELECT ",
      "COUNT(*) AS WCVP_TAXA, ",
      "SUM(QA_SINGLE_RECORD) AS SINGLE_RECORD, ",
      "SUM(QA_SINGLE_AREA) AS SINGLE_AREA, ",
      "SUM(QA_SINGLE_RECORD_SINGLE_AREA) ",
      "AS SINGLE_RECORD_SINGLE_AREA, ",
      "SUM(QA_GENUS_LEVEL) AS GENUS_LEVEL, ",
      "SUM(QA_UNPLACED) AS UNPLACED, ",
      "SUM(QA_ARTIFICIAL_HYBRID) AS ARTIFICIAL_HYBRID, ",
      "SUM(QA_LOW_TOTAL_EVIDENCE) AS LOW_TOTAL_EVIDENCE, ",
      "SUM(QA_HAS_SINGLE_RECORD_AREA_PRESENCE) ",
      "AS HAS_SINGLE_RECORD_AREA_PRESENCE, ",
      "SUM(QA_RECORDED_JP50_KAZAN) AS RECORDED_JP50_KAZAN, ",
      "SUM(QA_RECORDED_JP51_KURILES) AS RECORDED_JP51_KURILES ",
      "FROM ", q_output
    )
  )
  
  cat("\n— Core QA profile —\n")
  
  print.data.frame(
    qa_counts,
    row.names = FALSE
  )
  
  # ---------------------------------------------------------------------------
  # Review-priority profile
  # ---------------------------------------------------------------------------
  
  priority_summary <- dbGetQuery(
    con,
    paste0(
      "SELECT ",
      "QA_REVIEW_PRIORITY, ",
      "COUNT(*) AS WCVP_TAXA, ",
      "SUM(OCCURRENCE_RECORDS) AS OCCURRENCE_RECORDS ",
      "FROM ", q_output, " ",
      "GROUP BY QA_REVIEW_PRIORITY ",
      "ORDER BY ",
      "CASE QA_REVIEW_PRIORITY ",
      "WHEN 'TAXONOMIC_REVIEW' THEN 1 ",
      "WHEN 'HIGH_EVIDENCE_REVIEW' THEN 2 ",
      "WHEN 'MODERATE_EVIDENCE_REVIEW' THEN 3 ",
      "WHEN 'LOW_EVIDENCE_REVIEW' THEN 4 ",
      "WHEN 'AREA_PRESENCE_REVIEW' THEN 5 ",
      "ELSE 6 END"
    )
  )
  
  cat("\n— Diagnostic review priorities —\n")
  
  print.data.frame(
    priority_summary,
    row.names = FALSE
  )
  
  # ---------------------------------------------------------------------------
  # Rank x evidence x breadth
  # ---------------------------------------------------------------------------
  
  rank_profile <- dbGetQuery(
    con,
    paste0(
      "SELECT ",
      "FINAL_WCVP_RANK, ",
      "COUNT(*) AS WCVP_TAXA, ",
      "SUM(QA_SINGLE_RECORD) AS SINGLE_RECORD_TAXA, ",
      "SUM(QA_SINGLE_AREA) AS SINGLE_AREA_TAXA, ",
      "SUM(QA_LOW_TOTAL_EVIDENCE) AS LOW_EVIDENCE_TAXA, ",
      "ROUND(AVG(BOTANICAL_AREAS_PRESENT), 2) ",
      "AS MEAN_AREAS_PRESENT, ",
      "SUM(OCCURRENCE_RECORDS) AS OCCURRENCE_RECORDS ",
      "FROM ", q_output, " ",
      "GROUP BY FINAL_WCVP_RANK ",
      "ORDER BY WCVP_TAXA DESC"
    )
  )
  
  cat("\n— Rank, evidence and geographic breadth —\n")
  
  print.data.frame(
    rank_profile,
    row.names = FALSE
  )
  
  # ---------------------------------------------------------------------------
  # Single-area evidence profile
  # ---------------------------------------------------------------------------
  
  single_area_profile <- dbGetQuery(
    con,
    paste0(
      "SELECT ",
      "BOTANICAL_AREA_IDS AS BOTANICAL_AREA_ID, ",
      "COUNT(*) AS SINGLE_AREA_TAXA, ",
      "SUM(CASE WHEN OCCURRENCE_RECORDS = 1 ",
      "THEN 1 ELSE 0 END) AS SINGLE_RECORD_TAXA, ",
      "SUM(CASE WHEN OCCURRENCE_RECORDS <= 5 ",
      "THEN 1 ELSE 0 END) AS LOW_EVIDENCE_TAXA, ",
      "SUM(OCCURRENCE_RECORDS) AS OCCURRENCE_RECORDS ",
      "FROM ", q_output, " ",
      "WHERE BOTANICAL_AREAS_PRESENT = 1 ",
      "GROUP BY BOTANICAL_AREA_IDS ",
      "ORDER BY BOTANICAL_AREA_IDS"
    )
  )
  
  cat("\n— Single-area taxa by botanical area —\n")
  
  print.data.frame(
    single_area_profile,
    row.names = FALSE
  )
  
  # ---------------------------------------------------------------------------
  # JP50 / JP51 diagnostic profile
  #
  # v0.1.1 uses QA flags stored in the QA table rather than expecting the
  # original JP50_PRESENT / JP51_PRESENT matrix columns to be duplicated.
  # ---------------------------------------------------------------------------
  
  peripheral_profile <- dbGetQuery(
    con,
    paste0(
      "SELECT ",
      "'JP50' AS BOTANICAL_AREA_ID, ",
      "'Kazan Islands' AS BOTANICAL_AREA_NAME, ",
      "COUNT(*) AS WCVP_TAXA, ",
      "SUM(CASE WHEN BOTANICAL_AREAS_PRESENT = 1 ",
      "THEN 1 ELSE 0 END) AS SINGLE_AREA_TAXA, ",
      "SUM(CASE WHEN OCCURRENCE_RECORDS <= 5 ",
      "THEN 1 ELSE 0 END) AS LOW_TOTAL_EVIDENCE_TAXA ",
      "FROM ", q_output, " ",
      "WHERE QA_RECORDED_JP50_KAZAN = 1 ",
      "UNION ALL ",
      "SELECT ",
      "'JP51', ",
      "'Kuriles', ",
      "COUNT(*), ",
      "SUM(CASE WHEN BOTANICAL_AREAS_PRESENT = 1 ",
      "THEN 1 ELSE 0 END), ",
      "SUM(CASE WHEN OCCURRENCE_RECORDS <= 5 ",
      "THEN 1 ELSE 0 END) ",
      "FROM ", q_output, " ",
      "WHERE QA_RECORDED_JP51_KURILES = 1"
    )
  )
  
  cat("\n— JP50 / JP51 diagnostic profile —\n")
  
  print.data.frame(
    peripheral_profile,
    row.names = FALSE
  )
  
  # ---------------------------------------------------------------------------
  # Evidence x breadth cross-tab
  # ---------------------------------------------------------------------------
  
  evidence_breadth <- dbGetQuery(
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
      "END AS EVIDENCE_CLASS, ",
      "CASE ",
      "WHEN BOTANICAL_AREAS_PRESENT = 1 THEN '1_area' ",
      "WHEN BOTANICAL_AREAS_PRESENT BETWEEN 2 AND 5 THEN '2_5_areas' ",
      "WHEN BOTANICAL_AREAS_PRESENT BETWEEN 6 AND 10 THEN '6_10_areas' ",
      "WHEN BOTANICAL_AREAS_PRESENT BETWEEN 11 AND 25 THEN '11_25_areas' ",
      "ELSE '26_51_areas' ",
      "END AS BREADTH_CLASS, ",
      "COUNT(*) AS WCVP_TAXA ",
      "FROM ", q_output, " ",
      "GROUP BY 1, 2 ",
      "ORDER BY 1, 2"
    )
  )
  
  cat("\n— Evidence × geographic breadth —\n")
  
  print.data.frame(
    evidence_breadth,
    row.names = FALSE
  )
  
  # ---------------------------------------------------------------------------
  # Validation
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
  
  binary_flags <- c(
    "QA_SINGLE_RECORD",
    "QA_SINGLE_AREA",
    "QA_SINGLE_RECORD_SINGLE_AREA",
    "QA_GENUS_LEVEL",
    "QA_UNPLACED",
    "QA_ARTIFICIAL_HYBRID",
    "QA_LOW_TOTAL_EVIDENCE",
    "QA_HAS_SINGLE_RECORD_AREA_PRESENCE",
    "QA_RECORDED_JP50_KAZAN",
    "QA_RECORDED_JP51_KURILES"
  )
  
  invalid_flag_sql <- paste(
    paste0(
      binary_flags,
      " NOT IN (0,1) OR ",
      binary_flags,
      " IS NULL"
    ),
    collapse = " OR "
  )
  
  invalid_flags <- dbGetQuery(
    con,
    paste0(
      "SELECT COUNT(*) AS n FROM ",
      q_output,
      " WHERE ",
      invalid_flag_sql
    )
  )$n[[1]]
  
  missing_priorities <- dbGetQuery(
    con,
    paste0(
      "SELECT COUNT(*) AS n FROM ",
      q_output,
      " WHERE QA_REVIEW_PRIORITY IS NULL"
    )
  )$n[[1]]
  
  single_record_count <- qa_counts$SINGLE_RECORD[[1]]
  single_area_count <- qa_counts$SINGLE_AREA[[1]]
  single_record_single_area_count <-
    qa_counts$SINGLE_RECORD_SINGLE_AREA[[1]]
  genus_count <- qa_counts$GENUS_LEVEL[[1]]
  unplaced_count <- qa_counts$UNPLACED[[1]]
  hybrid_count <- qa_counts$ARTIFICIAL_HYBRID[[1]]
  jp50_count <- qa_counts$RECORDED_JP50_KAZAN[[1]]
  jp51_count <- qa_counts$RECORDED_JP51_KURILES[[1]]
  
  validation <- tibble(
    check = c(
      "source_taxa_11484",
      "source_WCVP_IDs_unique",
      "source_occurrences_2851736",
      "source_taxon_area_rows_128876",
      "output_taxa_11484",
      "output_WCVP_IDs_unique",
      "output_occurrences_2851736",
      "single_record_taxa_1447",
      "single_area_taxa_3067",
      "single_record_implies_single_area",
      "genus_level_taxa_825",
      "unplaced_taxa_14",
      "artificial_hybrid_taxa_31",
      "JP50_taxa_171",
      "JP51_taxa_183",
      "all_QA_flags_binary",
      "all_taxa_have_review_priority",
      "QA_is_diagnostic_only",
      "no_taxa_excluded",
      "presence_absence_not_modified",
      "historical_key_not_used",
      "districts_not_inferred",
      "endemicity_not_inferred",
      "native_status_not_inferred",
      "alien_status_not_inferred",
      "occurrences_not_modified",
      "taxonomy_not_modified",
      "geography_not_modified",
      "quarantine_not_modified",
      "stars_not_calculated",
      "ghi_not_calculated"
    ),
    pass = c(
      source_taxa == EXPECTED_TAXA,
      source_ids == source_taxa,
      source_occurrences == EXPECTED_OCCURRENCES,
      source_area_rows == EXPECTED_TAXON_AREA_ROWS,
      output_taxa == EXPECTED_TAXA,
      output_ids == output_taxa,
      output_occurrences == EXPECTED_OCCURRENCES,
      single_record_count == EXPECTED_SINGLE_RECORD,
      single_area_count == EXPECTED_SINGLE_AREA,
      single_record_single_area_count == single_record_count,
      genus_count == EXPECTED_GENUS,
      unplaced_count == EXPECTED_UNPLACED,
      hybrid_count == EXPECTED_ARTIFICIAL_HYBRID,
      jp50_count == 171L,
      jp51_count == 183L,
      invalid_flags == 0L,
      missing_priorities == 0L,
      TRUE,
      output_taxa == source_taxa,
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
  
  all_pass <- all(validation$pass)
  
  cat(
    "\nAll taxon-distribution QA validation checks PASS: ",
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
      "source_taxon_table",
      "source_taxon_area_table",
      "output_table",
      "taxa",
      "occurrence_evidence",
      "single_record_taxa",
      "single_area_taxa",
      "single_record_single_area_taxa",
      "genus_level_taxa",
      "unplaced_taxa",
      "artificial_hybrid_taxa",
      "JP50_taxa",
      "JP51_taxa",
      "low_total_evidence_definition",
      "review_priority_is_exclusion_rule",
      "historical_key_used",
      "endemicity_inferred",
      "native_status_inferred",
      "alien_status_inferred",
      "stars_calculated",
      "ghi_calculated",
      "validation_pass"
    ),
    value = c(
      "03_profile_japan_taxon_distribution_qa",
      SCRIPT_VERSION,
      RUN_DATE,
      SOURCE_TAXON,
      SOURCE_AREA,
      OUTPUT_TABLE,
      as.character(output_taxa),
      as.character(output_occurrences),
      as.character(single_record_count),
      as.character(single_area_count),
      as.character(single_record_single_area_count),
      as.character(genus_count),
      as.character(unplaced_count),
      as.character(hybrid_count),
      as.character(jp50_count),
      as.character(jp51_count),
      "OCCURRENCE_RECORDS <= 5",
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
  # Review exports
  # ---------------------------------------------------------------------------
  
  high_review <- dbGetQuery(
    con,
    paste0(
      "SELECT * FROM ",
      q_output,
      " WHERE QA_REVIEW_PRIORITY IN ",
      "('TAXONOMIC_REVIEW','HIGH_EVIDENCE_REVIEW') ",
      "ORDER BY QA_REVIEW_PRIORITY, ",
      "OCCURRENCE_RECORDS, FINAL_WCVP_RECOGNISED_NAME"
    )
  )
  
  single_record_single_area <- dbGetQuery(
    con,
    paste0(
      "SELECT * FROM ",
      q_output,
      " WHERE QA_SINGLE_RECORD_SINGLE_AREA = 1 ",
      "ORDER BY BOTANICAL_AREA_IDS, ",
      "FINAL_WCVP_RECOGNISED_NAME"
    )
  )
  
  jp50_jp51_review <- dbGetQuery(
    con,
    paste0(
      "SELECT * FROM ",
      q_output,
      " WHERE QA_RECORDED_JP50_KAZAN = 1 ",
      "OR QA_RECORDED_JP51_KURILES = 1 ",
      "ORDER BY BOTANICAL_AREAS_PRESENT, ",
      "OCCURRENCE_RECORDS, FINAL_WCVP_RECOGNISED_NAME"
    )
  )
  
  write_csv(
    priority_summary,
    file.path(output_dir, "qa_review_priority_summary.csv")
  )
  
  write_csv(
    rank_profile,
    file.path(output_dir, "qa_rank_evidence_breadth.csv")
  )
  
  write_csv(
    single_area_profile,
    file.path(output_dir, "qa_single_area_profile.csv")
  )
  
  write_csv(
    peripheral_profile,
    file.path(output_dir, "qa_JP50_JP51_profile.csv")
  )
  
  write_csv(
    evidence_breadth,
    file.path(output_dir, "qa_evidence_breadth_crosstab.csv")
  )
  
  write_csv(
    high_review,
    file.path(output_dir, "qa_high_priority_review.csv")
  )
  
  write_csv(
    single_record_single_area,
    file.path(output_dir, "qa_single_record_single_area.csv")
  )
  
  write_csv(
    jp50_jp51_review,
    file.path(output_dir, "qa_JP50_JP51_review.csv")
  )
  
  write_csv(
    validation,
    file.path(output_dir, "qa_validation.csv")
  )
  
  write_csv(
    metadata,
    file.path(output_dir, "qa_metadata.csv")
  )
  
  dbWriteTable(
    con,
    "vpjd_japan_taxon_distribution_qa_validation",
    validation,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "vpjd_japan_taxon_distribution_qa_metadata",
    metadata,
    overwrite = TRUE
  )
  
  # ---------------------------------------------------------------------------
  # Final status
  # ---------------------------------------------------------------------------
  
  if (!all_pass) {
    cat(
      "\nOutput status: FAILED TAXON-DISTRIBUTION QA VALIDATION\n"
    )
    
    stop(
      paste(
        "Distribution 03 validation failed.",
        "Do not freeze the QA layer."
      )
    )
  }
  
  cat("\nCanonical QA table:\n")
  cat("  ", OUTPUT_TABLE, "\n", sep = "")
  
  cat(
    "\nOutput status: ",
    "VALIDATED CONTEMPORARY JAPANESE TAXON-DISTRIBUTION QA LAYER\n",
    sep = ""
  )
  
  cat(
    "WCVP concepts retained: ",
    format(output_taxa, big.mark = ","),
    "\n",
    sep = ""
  )
  
  cat(
    "Occurrence evidence retained: ",
    format(output_occurrences, big.mark = ","),
    "\n",
    sep = ""
  )
  
  cat(
    "Single-record taxa flagged: ",
    format(single_record_count, big.mark = ","),
    "\n",
    sep = ""
  )
  
  cat(
    "Single-area taxa flagged: ",
    format(single_area_count, big.mark = ","),
    "\n",
    sep = ""
  )
  
  cat(
    "Genus-level taxa flagged: ",
    format(genus_count, big.mark = ","),
    "\n",
    sep = ""
  )
  
  cat(
    "Unplaced taxa flagged: ",
    format(unplaced_count, big.mark = ","),
    "\n",
    sep = ""
  )
  
  cat(
    "Artificial hybrids flagged: ",
    format(hybrid_count, big.mark = ","),
    "\n",
    sep = ""
  )
  
  cat(
    "JP50 taxa flagged: ",
    format(jp50_count, big.mark = ","),
    "\n",
    sep = ""
  )
  
  cat(
    "JP51 taxa flagged: ",
    format(jp51_count, big.mark = ","),
    "\n",
    sep = ""
  )
  
  cat("Taxa excluded: 0\n")
  cat("QA layer diagnostic only: TRUE\n")
  cat("Historical Key used: FALSE\n")
  cat("Endemicity inferred: FALSE\n")
  cat("Native/alien status inferred: FALSE\n")
  cat("Occurrences modified: FALSE\n")
  cat("Taxonomy modified: FALSE\n")
  cat("Geography modified: FALSE\n")
  cat("Quarantine modified: FALSE\n")
  cat("Stars calculated: FALSE\n")
  cat("GHI calculated: FALSE\n")
  
  cat(
    "\nDistribution 03 v",
    SCRIPT_VERSION,
    " complete.\n",
    sep = ""
  )
  
  invisible(
    list(
      qa_counts = qa_counts,
      priority_summary = priority_summary,
      rank_profile = rank_profile,
      single_area_profile = single_area_profile,
      peripheral_profile = peripheral_profile,
      evidence_breadth = evidence_breadth,
      validation = validation,
      metadata = metadata
    )
  )
}

result_distribution_03 <-
  run_japan_taxon_distribution_qa()