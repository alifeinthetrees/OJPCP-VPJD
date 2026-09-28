# ==============================================================================
# 04c_occurrence_wcvp_residual_diagnostics.R
# VPJD / Oxford-Japan Plant Conservation Partnership
#
# Purpose:
#   Diagnose and prioritise taxonomic concepts remaining unresolved after
#   occurrence-first WCVP reconciliation stages 04a and 04b.
#
# Inputs:
#   occurrence_wcvp_reconciliation_recovered
#   occurrence_wcvp_recovery
#   wcvp_occurrence_match_index
#
# Outputs:
#   occurrence_wcvp_residual_diagnostics
#   occurrence_wcvp_residual_metadata
#   CSV audit tables under:
#   outputs/tables/taxonomy/occurrence_wcvp_residual_diagnostics/
#
# Principles:
# - Diagnostic only.
# - No new taxonomic assignments.
# - No fuzzy matching.
# - No modification of 04a or 04b outputs.
# - Preserve higher-rank records as a distinct residual class.
# - Prioritise residuals partly by their occurrence-record representation.
#
# Version: 0.1.0
# ==============================================================================

VPJD_WCVP_RESIDUAL_VERSION <- "0.1.0"

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

run_occurrence_wcvp_residual_diagnostics <- function() {
  
  duckdb_path <- here::here(
    "data", "interim", "occurrences",
    "vpjd_occurrences.duckdb"
  )
  
  output_dir <- here::here(
    "outputs", "tables", "taxonomy",
    "occurrence_wcvp_residual_diagnostics"
  )
  
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  if (!file.exists(duckdb_path)) {
    stop("VPJD occurrence DuckDB not found:\n", duckdb_path)
  }
  
  con <- DBI::dbConnect(
    duckdb::duckdb(),
    dbdir = duckdb_path,
    read_only = FALSE
  )
  
  on.exit(
    DBI::dbDisconnect(con, shutdown = TRUE),
    add = TRUE
  )
  
  required_tables <- c(
    "occurrence_wcvp_reconciliation_recovered",
    "occurrence_wcvp_recovery",
    "wcvp_occurrence_match_index"
  )
  
  missing_tables <- setdiff(
    required_tables,
    DBI::dbListTables(con)
  )
  
  if (length(missing_tables) > 0L) {
    stop(
      "Required DuckDB table(s) missing: ",
      paste(missing_tables, collapse = ", ")
    )
  }
  
  cli::cli_h1(
    "VPJD occurrence-first WCVP residual diagnostics"
  )
  
  # ---------------------------------------------------------------------------
  # Validate schemas
  # ---------------------------------------------------------------------------
  
  required_recon_fields <- c(
    "vpjd_taxon_universe_id",
    "taxonKey",
    "speciesKey",
    "family",
    "genus",
    "species",
    "infraspecificEpithet",
    "taxonRank",
    "scientificName",
    "total_records",
    "present_records",
    "absent_records",
    "n_datasets",
    "earliest_year",
    "latest_year",
    "wcvp_candidate_count",
    "wcvp_plant_name_id",
    "wcvp_taxon_name",
    "wcvp_taxon_authors",
    "wcvp_taxon_rank",
    "wcvp_taxon_status",
    "wcvp_accepted_plant_name_id",
    "wcvp_scientific_name",
    "wcvp_match_status",
    "final_wcvp_accepted_plant_name_id",
    "final_resolution_stage",
    "recovery_status",
    "recovery_stage",
    "recovery_matched_wcvp_plant_name_id",
    "best_stage_matched_name_count",
    "best_stage_accepted_concept_count"
  )
  
  actual_recon_fields <- DBI::dbListFields(
    con,
    "occurrence_wcvp_reconciliation_recovered"
  )
  
  missing_recon_fields <- setdiff(
    required_recon_fields,
    actual_recon_fields
  )
  
  if (length(missing_recon_fields) > 0L) {
    stop(
      "occurrence_wcvp_reconciliation_recovered missing field(s): ",
      paste(missing_recon_fields, collapse = ", ")
    )
  }
  
  # ---------------------------------------------------------------------------
  # Remove only 04c-owned tables
  # ---------------------------------------------------------------------------
  
  DBI::dbExecute(
    con,
    "DROP TABLE IF EXISTS occurrence_wcvp_residual_metadata"
  )
  
  DBI::dbExecute(
    con,
    "DROP TABLE IF EXISTS occurrence_wcvp_residual_diagnostics"
  )
  
  # ---------------------------------------------------------------------------
  # Build residual diagnostic table
  # ---------------------------------------------------------------------------
  
  DBI::dbExecute(
    con,
    "
    CREATE TABLE occurrence_wcvp_residual_diagnostics AS

    SELECT
      vpjd_taxon_universe_id,
      taxonKey,
      speciesKey,
      family,
      genus,
      species,
      infraspecificEpithet,
      taxonRank,
      scientificName,

      total_records,
      present_records,
      absent_records,
      n_datasets,
      earliest_year,
      latest_year,

      wcvp_match_status,
      wcvp_candidate_count,
      wcvp_plant_name_id,
      wcvp_taxon_name,
      wcvp_taxon_authors,
      wcvp_taxon_rank,
      wcvp_taxon_status,
      wcvp_accepted_plant_name_id,
      wcvp_scientific_name,

      recovery_status,
      recovery_stage,
      recovery_matched_wcvp_plant_name_id,
      best_stage_matched_name_count,
      best_stage_accepted_concept_count,

      CASE
        WHEN UPPER(COALESCE(taxonRank, '')) IN (
          'SPECIES',
          'SUBSPECIES',
          'VARIETY',
          'FORM'
        )
        THEN 'species_or_infraspecific'
        ELSE 'higher_rank_or_unranked'
      END AS residual_resolution_class,

      CASE
        WHEN wcvp_match_status = 'exact_nonaccepted'
        THEN 'exact_nonaccepted_without_accepted_resolution'

        WHEN recovery_status = 'ambiguous'
        THEN 'deterministic_recovery_ambiguous'

        WHEN wcvp_match_status = 'ambiguous_exact'
        THEN 'initial_exact_match_ambiguous'

        WHEN wcvp_match_status = 'unmatched'
         AND UPPER(COALESCE(taxonRank, '')) IN (
           'SPECIES',
           'SUBSPECIES',
           'VARIETY',
           'FORM'
         )
        THEN 'species_or_infraspecific_unmatched'

        WHEN UPPER(COALESCE(taxonRank, '')) NOT IN (
          'SPECIES',
          'SUBSPECIES',
          'VARIETY',
          'FORM'
        )
        THEN 'higher_rank_or_unranked'

        ELSE 'other_unresolved'
      END AS residual_reason,

      CASE
        WHEN present_records >= 10000 THEN 'A_10000_plus'
        WHEN present_records >= 1000  THEN 'B_1000_9999'
        WHEN present_records >= 100   THEN 'C_100_999'
        WHEN present_records >= 10    THEN 'D_10_99'
        ELSE 'E_1_9'
      END AS occurrence_priority_band

    FROM occurrence_wcvp_reconciliation_recovered

    WHERE final_wcvp_accepted_plant_name_id IS NULL
    "
  )
  
  # ---------------------------------------------------------------------------
  # Core validation
  # ---------------------------------------------------------------------------
  
  validation <- DBI::dbGetQuery(
    con,
    "
    SELECT
      COUNT(*) AS residual_concepts,
      SUM(total_records) AS total_records_represented,
      SUM(present_records) AS present_records_represented,
      SUM(absent_records) AS absent_records_represented,
      COUNT(DISTINCT taxonKey) AS distinct_gbif_taxon_keys
    FROM occurrence_wcvp_residual_diagnostics
    "
  )
  
  if (validation$residual_concepts[[1]] != 2348) {
    warning(
      "Residual concept count is ",
      validation$residual_concepts[[1]],
      "; expected 2,348 from validated 04b output."
    )
  }
  
  # ---------------------------------------------------------------------------
  # Diagnostic summaries
  # ---------------------------------------------------------------------------
  
  residual_by_reason <- DBI::dbGetQuery(
    con,
    "
    SELECT
      residual_reason,
      COUNT(*) AS concepts,
      SUM(present_records) AS present_records,
      SUM(total_records) AS total_records
    FROM occurrence_wcvp_residual_diagnostics
    GROUP BY residual_reason
    ORDER BY concepts DESC
    "
  )
  
  residual_by_rank <- DBI::dbGetQuery(
    con,
    "
    SELECT
      taxonRank,
      COUNT(*) AS concepts,
      SUM(present_records) AS present_records,
      SUM(total_records) AS total_records
    FROM occurrence_wcvp_residual_diagnostics
    GROUP BY taxonRank
    ORDER BY concepts DESC
    "
  )
  
  residual_by_priority <- DBI::dbGetQuery(
    con,
    "
    SELECT
      occurrence_priority_band,
      COUNT(*) AS concepts,
      SUM(present_records) AS present_records,
      SUM(total_records) AS total_records
    FROM occurrence_wcvp_residual_diagnostics
    GROUP BY occurrence_priority_band
    ORDER BY occurrence_priority_band
    "
  )
  
  species_residual_summary <- DBI::dbGetQuery(
    con,
    "
    SELECT
      wcvp_match_status,
      recovery_status,
      COUNT(*) AS concepts,
      SUM(present_records) AS present_records
    FROM occurrence_wcvp_residual_diagnostics
    WHERE residual_resolution_class =
          'species_or_infraspecific'
    GROUP BY
      wcvp_match_status,
      recovery_status
    ORDER BY concepts DESC
    "
  )
  
  # ---------------------------------------------------------------------------
  # Most consequential unresolved concepts
  # ---------------------------------------------------------------------------
  
  top_residuals <- DBI::dbGetQuery(
    con,
    "
    SELECT
      vpjd_taxon_universe_id,
      taxonKey,
      speciesKey,
      scientificName,
      taxonRank,
      family,
      genus,
      species,
      infraspecificEpithet,
      present_records,
      absent_records,
      total_records,
      n_datasets,
      earliest_year,
      latest_year,
      wcvp_match_status,
      wcvp_candidate_count,
      recovery_status,
      recovery_stage,
      best_stage_matched_name_count,
      best_stage_accepted_concept_count,
      residual_reason,
      occurrence_priority_band
    FROM occurrence_wcvp_residual_diagnostics
    ORDER BY
      present_records DESC,
      total_records DESC,
      scientificName
    LIMIT 100
    "
  )
  
  # ---------------------------------------------------------------------------
  # 73 exact nonaccepted concepts
  # ---------------------------------------------------------------------------
  
  exact_nonaccepted_residuals <- DBI::dbGetQuery(
    con,
    "
    SELECT *
    FROM occurrence_wcvp_residual_diagnostics
    WHERE wcvp_match_status = 'exact_nonaccepted'
    ORDER BY
      present_records DESC,
      scientificName
    "
  )
  
  # ---------------------------------------------------------------------------
  # Ambiguous residuals
  # ---------------------------------------------------------------------------
  
  ambiguous_residuals <- DBI::dbGetQuery(
    con,
    "
    SELECT *
    FROM occurrence_wcvp_residual_diagnostics
    WHERE recovery_status = 'ambiguous'
       OR wcvp_match_status = 'ambiguous_exact'
    ORDER BY
      present_records DESC,
      scientificName
    "
  )
  
  # ---------------------------------------------------------------------------
  # Remaining unmatched species/infraspecific concepts
  # ---------------------------------------------------------------------------
  
  unmatched_terminal_residuals <- DBI::dbGetQuery(
    con,
    "
    SELECT *
    FROM occurrence_wcvp_residual_diagnostics
    WHERE residual_resolution_class =
          'species_or_infraspecific'
      AND wcvp_match_status = 'unmatched'
      AND COALESCE(recovery_status, '') <> 'ambiguous'
    ORDER BY
      present_records DESC,
      scientificName
    "
  )
  
  # ---------------------------------------------------------------------------
  # Higher-rank residuals
  # ---------------------------------------------------------------------------
  
  higher_rank_residuals <- DBI::dbGetQuery(
    con,
    "
    SELECT *
    FROM occurrence_wcvp_residual_diagnostics
    WHERE residual_resolution_class =
          'higher_rank_or_unranked'
    ORDER BY
      present_records DESC,
      scientificName
    "
  )
  
  # ---------------------------------------------------------------------------
  # Metadata
  # ---------------------------------------------------------------------------
  
  DBI::dbExecute(
    con,
    "
    CREATE TABLE occurrence_wcvp_residual_metadata AS
    SELECT
      CURRENT_TIMESTAMP AS created_at,
      '0.1.0'::VARCHAR AS script_version,
      COUNT(*) AS residual_concepts,
      SUM(total_records) AS total_records_represented,
      SUM(present_records) AS present_records_represented,
      SUM(absent_records) AS absent_records_represented
    FROM occurrence_wcvp_residual_diagnostics
    "
  )
  
  # ---------------------------------------------------------------------------
  # Export
  # ---------------------------------------------------------------------------
  
  residual_table <- DBI::dbGetQuery(
    con,
    "
    SELECT *
    FROM occurrence_wcvp_residual_diagnostics
    ORDER BY
      present_records DESC,
      total_records DESC,
      scientificName
    "
  )
  
  readr::write_csv(
    residual_table,
    file.path(
      output_dir,
      "occurrence_wcvp_residual_diagnostics.csv"
    )
  )
  
  readr::write_csv(
    residual_by_reason,
    file.path(
      output_dir,
      "residual_by_reason.csv"
    )
  )
  
  readr::write_csv(
    residual_by_rank,
    file.path(
      output_dir,
      "residual_by_rank.csv"
    )
  )
  
  readr::write_csv(
    residual_by_priority,
    file.path(
      output_dir,
      "residual_by_occurrence_priority.csv"
    )
  )
  
  readr::write_csv(
    species_residual_summary,
    file.path(
      output_dir,
      "species_infraspecific_residual_summary.csv"
    )
  )
  
  readr::write_csv(
    top_residuals,
    file.path(
      output_dir,
      "top_100_residual_concepts.csv"
    )
  )
  
  readr::write_csv(
    exact_nonaccepted_residuals,
    file.path(
      output_dir,
      "exact_nonaccepted_residuals.csv"
    )
  )
  
  readr::write_csv(
    ambiguous_residuals,
    file.path(
      output_dir,
      "ambiguous_residuals.csv"
    )
  )
  
  readr::write_csv(
    unmatched_terminal_residuals,
    file.path(
      output_dir,
      "unmatched_species_infraspecific_residuals.csv"
    )
  )
  
  readr::write_csv(
    higher_rank_residuals,
    file.path(
      output_dir,
      "higher_rank_residuals.csv"
    )
  )
  
  # ---------------------------------------------------------------------------
  # Console
  # ---------------------------------------------------------------------------
  
  fmt <- function(x) {
    format(
      x,
      big.mark = ",",
      scientific = FALSE,
      trim = TRUE
    )
  }
  
  cli::cli_h2(
    "WCVP residual diagnostics complete"
  )
  
  cli::cli_text(
    "{.strong Residual concepts:} ",
    "{fmt(validation$residual_concepts[[1]])}"
  )
  
  cli::cli_text(
    "{.strong PRESENT occurrence records represented:} ",
    "{fmt(validation$present_records_represented[[1]])}"
  )
  
  cli::cli_text(
    "{.strong Total occurrence records represented:} ",
    "{fmt(validation$total_records_represented[[1]])}"
  )
  
  cli::cli_text(
    "{.strong Distinct GBIF taxonKeys represented:} ",
    "{fmt(validation$distinct_gbif_taxon_keys[[1]])}"
  )
  
  cli::cli_alert_success(
    "No WCVP assignments were changed."
  )
  
  cli::cli_text(
    "04c_occurrence_wcvp_residual_diagnostics.R ",
    "v{VPJD_WCVP_RESIDUAL_VERSION} complete."
  )
  
  invisible(
    list(
      validation = validation,
      residual_by_reason = residual_by_reason,
      residual_by_rank = residual_by_rank,
      residual_by_priority = residual_by_priority,
      species_residual_summary = species_residual_summary,
      top_residuals = top_residuals
    )
  )
}

wcvp_residual_results <-
  run_occurrence_wcvp_residual_diagnostics()

residual_validation <-
  wcvp_residual_results$validation

residual_by_reason <-
  wcvp_residual_results$residual_by_reason

residual_by_rank <-
  wcvp_residual_results$residual_by_rank

residual_by_priority <-
  wcvp_residual_results$residual_by_priority

species_residual_summary <-
  wcvp_residual_results$species_residual_summary

top_residuals <-
  wcvp_residual_results$top_residuals