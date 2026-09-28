# ==============================================================================
# 04l_occurrence_wcvp_hybrid_recovery.R
# VPJD / Oxford-Japan Plant Conservation Partnership
#
# Purpose:
#   Promote ONLY secure hybrid resolutions established by the validated/frozen
#   04k hybrid diagnostic.
#
# Input:
#   occurrence_wcvp_reconciliation_alternative_combination
#   occurrence_wcvp_hybrid_diagnostics
#
# Recovery rule:
#   hybrid_diagnostic_class == 'secure_wcvp_hybrid_resolution'
#
# Safeguards:
#   - no new matching;
#   - no recalculation of 04k diagnostic logic;
#   - no fuzzy matching;
#   - no edit distance;
#   - no parental inference;
#   - no Artificial Hybrid recovery;
#   - no review-class recovery;
#   - no ambiguous recovery;
#   - no no-evidence recovery;
#   - upstream tables remain unchanged.
#
# Version: 0.1.0
# ==============================================================================

VPJD_WCVP_HYBRID_RECOVERY_VERSION <- "0.1.0"
required_packages <- c("here","DBI","duckdb","readr","dplyr","cli")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0L) {
  stop("Required package(s) not installed: ", paste(missing_packages, collapse = ", "))
}
suppressPackageStartupMessages({
  library(here)
  library(DBI)
  library(duckdb)
  library(readr)
  library(dplyr)
  library(cli)
})

run_occurrence_wcvp_hybrid_recovery <- function() {
  duckdb_path <- here::here(
    "data","interim","occurrences","vpjd_occurrences.duckdb"
  )
  output_dir <- here::here(
    "outputs","tables","taxonomy","occurrence_wcvp_hybrid_recovery"
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
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  
  required_tables <- c(
    "occurrence_wcvp_reconciliation_alternative_combination",
    "occurrence_wcvp_hybrid_diagnostics"
  )
  missing_tables <- setdiff(required_tables, DBI::dbListTables(con))
  if (length(missing_tables) > 0L) {
    stop(
      "Required DuckDB table(s) missing: ",
      paste(missing_tables, collapse = ", ")
    )
  }
  
  fmt <- function(x) {
    format(x, big.mark = ",", scientific = FALSE, trim = TRUE)
  }
  
  cli::cli_h1("VPJD WCVP secure hybrid recovery")
  
  # ---------------------------------------------------------------------------
  # Baseline
  # ---------------------------------------------------------------------------
  
  baseline <- DBI::dbGetQuery(
    con,
    "
    SELECT
      COUNT(*) AS total_concepts,
      SUM(CASE
        WHEN alternative_final_wcvp_accepted_plant_name_id IS NOT NULL
        THEN 1 ELSE 0
      END) AS resolved_before_04l,
      COUNT(
        DISTINCT alternative_final_wcvp_accepted_plant_name_id
      ) AS distinct_accepted_before_04l,
      SUM(CASE
        WHEN alternative_final_wcvp_accepted_plant_name_id IS NULL
        THEN 1 ELSE 0
      END) AS unresolved_before_04l
    FROM occurrence_wcvp_reconciliation_alternative_combination
    "
  )
  
  cli::cli_text(
    "{.strong Resolved before 04l:} ",
    "{fmt(baseline$resolved_before_04l[[1]])}"
  )
  cli::cli_text(
    "{.strong Distinct accepted concepts before 04l:} ",
    "{fmt(baseline$distinct_accepted_before_04l[[1]])}"
  )
  cli::cli_text(
    "{.strong Unresolved before 04l:} ",
    "{fmt(baseline$unresolved_before_04l[[1]])}"
  )
  
  # ---------------------------------------------------------------------------
  # Validate 04k version and diagnostic population
  # ---------------------------------------------------------------------------
  
  if (!"occurrence_wcvp_hybrid_diagnostic_metadata" %in% DBI::dbListTables(con)) {
    stop(
      "04k diagnostic metadata table is missing. ",
      "Run validated/frozen 04k v0.1.1 before 04l."
    )
  }
  
  diag_meta <- DBI::dbGetQuery(
    con,
    "
    SELECT *
    FROM occurrence_wcvp_hybrid_diagnostic_metadata
    ORDER BY created_at DESC
    LIMIT 1
    "
  )
  
  if (
    nrow(diag_meta) != 1L ||
    is.na(diag_meta$script_version[[1]]) ||
    diag_meta$script_version[[1]] != "0.1.1"
  ) {
    stop(
      "04l requires occurrence_wcvp_hybrid_diagnostics ",
      "from validated/frozen 04k v0.1.1."
    )
  }
  
  diagnostic_check <- DBI::dbGetQuery(
    con,
    "
    SELECT
      COUNT(*) AS total_hybrid_concepts,
      SUM(CASE
        WHEN hybrid_diagnostic_class =
             'secure_wcvp_hybrid_resolution'
        THEN 1 ELSE 0
      END) AS secure_concepts,
      SUM(CASE
        WHEN hybrid_diagnostic_class =
             'secure_wcvp_hybrid_resolution'
        THEN present_records ELSE 0
      END) AS secure_present_records,
      SUM(CASE
        WHEN hybrid_diagnostic_class =
             'review_changed_accepted_concept'
        THEN 1 ELSE 0
      END) AS review_concepts,
      SUM(CASE
        WHEN hybrid_diagnostic_class =
             'wcvp_artificial_hybrid_treatment'
        THEN 1 ELSE 0
      END) AS artificial_hybrid_concepts,
      SUM(CASE
        WHEN hybrid_diagnostic_class =
             'multiple_wcvp_hybrid_concepts'
        THEN 1 ELSE 0
      END) AS ambiguous_concepts,
      SUM(CASE
        WHEN hybrid_diagnostic_class =
             'no_wcvp_hybrid_evidence'
        THEN 1 ELSE 0
      END) AS no_evidence_concepts
    FROM occurrence_wcvp_hybrid_diagnostics
    "
  )
  
  if (
    diagnostic_check$secure_concepts[[1]] != 35L ||
    diagnostic_check$secure_present_records[[1]] != 1364L
  ) {
    stop(
      "04k secure population differs from the validated/frozen audit. ",
      "Expected 35 concepts / 1,364 PRESENT records; found ",
      diagnostic_check$secure_concepts[[1]], " concepts / ",
      diagnostic_check$secure_present_records[[1]], " PRESENT records."
    )
  }
  
  cli::cli_text(
    "{.strong Frozen 04k secure population:} ",
    "{fmt(diagnostic_check$secure_concepts[[1]])} concepts / ",
    "{fmt(diagnostic_check$secure_present_records[[1]])} PRESENT records"
  )
  
  # ---------------------------------------------------------------------------
  # Remove 04l-owned products only
  # ---------------------------------------------------------------------------
  
  for (tbl in c(
    "occurrence_wcvp_hybrid_recovery_metadata",
    "occurrence_wcvp_reconciliation_hybrid",
    "occurrence_wcvp_hybrid_recovery"
  )) {
    DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", tbl))
  }
  
  # ---------------------------------------------------------------------------
  # Recovery table
  #
  # IMPORTANT:
  #   We do not rematch anything here. We simply consume the accepted WCVP ID
  #   already established by the frozen 04k diagnostic.
  # ---------------------------------------------------------------------------
  
  DBI::dbExecute(
    con,
    "
    CREATE TABLE occurrence_wcvp_hybrid_recovery AS
    SELECT
      d.vpjd_taxon_universe_id,
      d.taxonKey,
      d.speciesKey,
      d.family,
      d.genus,
      d.species,
      d.taxonRank,
      d.scientificName,
      d.total_records,
      d.present_records,
      d.reconstructed_hybrid_name,
      d.reconstructed_unmarked_name,
      d.gbif_species_name,
      d.candidate_accepted_concept_id
        AS recovered_wcvp_accepted_plant_name_id,
      d.target_wcvp_taxon_name
        AS recovered_wcvp_taxon_name,
      d.target_wcvp_taxon_authors
        AS recovered_wcvp_taxon_authors,
      d.target_wcvp_taxon_rank
        AS recovered_wcvp_taxon_rank,
      d.target_wcvp_taxon_status
        AS recovered_wcvp_taxon_status,
      d.target_wcvp_family
        AS recovered_wcvp_family,
      d.supporting_rules,
      d.matched_query_names,
      '04l_secure_hybrid_recovery'::VARCHAR
        AS recovery_stage,
      d.hybrid_diagnostic_class
    FROM occurrence_wcvp_hybrid_diagnostics d
    WHERE d.hybrid_diagnostic_class =
          'secure_wcvp_hybrid_resolution'
      AND d.accepted_concept_count = 1
      AND d.candidate_accepted_concept_id IS NOT NULL
      AND UPPER(COALESCE(d.target_wcvp_taxon_status, '')) = 'ACCEPTED'
    "
  )
  
  # ---------------------------------------------------------------------------
  # Recovery safety checks
  # ---------------------------------------------------------------------------
  
  recovery_check <- DBI::dbGetQuery(
    con,
    "
    SELECT
      COUNT(*) AS recovered_concepts,
      SUM(present_records) AS recovered_present_records,
      COUNT(
        DISTINCT recovered_wcvp_accepted_plant_name_id
      ) AS distinct_recovered_accepted_concepts,
      SUM(CASE
        WHEN recovered_wcvp_accepted_plant_name_id IS NULL
        THEN 1 ELSE 0
      END) AS missing_accepted_id,
      SUM(CASE
        WHEN UPPER(COALESCE(recovered_wcvp_taxon_status, '')) <> 'ACCEPTED'
        THEN 1 ELSE 0
      END) AS nonaccepted_targets
    FROM occurrence_wcvp_hybrid_recovery
    "
  )
  
  if (
    recovery_check$recovered_concepts[[1]] != 35L ||
    recovery_check$recovered_present_records[[1]] != 1364L
  ) {
    stop(
      "04l recovery population failed validation. Expected ",
      "35 concepts / 1,364 PRESENT records."
    )
  }
  
  if (
    recovery_check$missing_accepted_id[[1]] != 0L ||
    recovery_check$nonaccepted_targets[[1]] != 0L
  ) {
    stop(
      "04l safety check failed: recovery contains missing IDs ",
      "or non-Accepted WCVP targets."
    )
  }
  
  # ---------------------------------------------------------------------------
  # Consolidate with secure pre-04l baseline
  #
  # Preserve every upstream column and add explicit 04l final fields.
  # ---------------------------------------------------------------------------
  
  DBI::dbExecute(
    con,
    "
    CREATE TABLE occurrence_wcvp_reconciliation_hybrid AS
    SELECT
      b.*,
      CASE
        WHEN b.alternative_final_wcvp_accepted_plant_name_id IS NOT NULL
        THEN b.alternative_final_wcvp_accepted_plant_name_id
        ELSE r.recovered_wcvp_accepted_plant_name_id
      END AS hybrid_final_wcvp_accepted_plant_name_id,

      CASE
        WHEN b.alternative_final_wcvp_accepted_plant_name_id IS NOT NULL
        THEN COALESCE(
          b.alternative_final_resolution_stage,
          'pre_04l_resolved'
        )
        WHEN r.recovered_wcvp_accepted_plant_name_id IS NOT NULL
        THEN '04l_secure_hybrid_recovery'
        ELSE NULL
      END AS hybrid_final_resolution_stage,

      CASE
        WHEN r.recovered_wcvp_accepted_plant_name_id IS NOT NULL
        THEN r.recovered_wcvp_taxon_name
        ELSE NULL
      END AS hybrid_recovered_wcvp_taxon_name,

      CASE
        WHEN r.recovered_wcvp_accepted_plant_name_id IS NOT NULL
        THEN r.recovered_wcvp_taxon_authors
        ELSE NULL
      END AS hybrid_recovered_wcvp_taxon_authors,

      CASE
        WHEN r.recovered_wcvp_accepted_plant_name_id IS NOT NULL
        THEN r.recovered_wcvp_taxon_rank
        ELSE NULL
      END AS hybrid_recovered_wcvp_taxon_rank,

      CASE
        WHEN r.recovered_wcvp_accepted_plant_name_id IS NOT NULL
        THEN r.recovered_wcvp_family
        ELSE NULL
      END AS hybrid_recovered_wcvp_family

    FROM occurrence_wcvp_reconciliation_alternative_combination b

    LEFT JOIN occurrence_wcvp_hybrid_recovery r
      ON b.vpjd_taxon_universe_id =
         r.vpjd_taxon_universe_id
    "
  )
  
  # ---------------------------------------------------------------------------
  # Final validation
  # ---------------------------------------------------------------------------
  
  final_summary <- DBI::dbGetQuery(
    con,
    "
    SELECT
      COUNT(*) AS total_concepts,
      SUM(CASE
        WHEN hybrid_final_wcvp_accepted_plant_name_id IS NOT NULL
        THEN 1 ELSE 0
      END) AS resolved_after_04l,
      COUNT(
        DISTINCT hybrid_final_wcvp_accepted_plant_name_id
      ) AS distinct_accepted_after_04l,
      SUM(CASE
        WHEN hybrid_final_wcvp_accepted_plant_name_id IS NULL
        THEN 1 ELSE 0
      END) AS unresolved_after_04l
    FROM occurrence_wcvp_reconciliation_hybrid
    "
  )
  
  expected_resolved <-
    baseline$resolved_before_04l[[1]] +
    recovery_check$recovered_concepts[[1]]
  
  expected_unresolved <-
    baseline$unresolved_before_04l[[1]] -
    recovery_check$recovered_concepts[[1]]
  
  if (
    final_summary$total_concepts[[1]] !=
    baseline$total_concepts[[1]]
  ) {
    stop("04l row-count safety check failed.")
  }
  
  if (
    final_summary$resolved_after_04l[[1]] !=
    expected_resolved
  ) {
    stop(
      "04l resolved-count safety check failed. Expected ",
      expected_resolved, ", found ",
      final_summary$resolved_after_04l[[1]], "."
    )
  }
  
  if (
    final_summary$unresolved_after_04l[[1]] !=
    expected_unresolved
  ) {
    stop(
      "04l unresolved-count safety check failed. Expected ",
      expected_unresolved, ", found ",
      final_summary$unresolved_after_04l[[1]], "."
    )
  }
  
  # ---------------------------------------------------------------------------
  # Explicit exclusion check
  #
  # None of the review/artificial/ambiguous/no-evidence 04k concepts may have
  # acquired an 04l recovery.
  # ---------------------------------------------------------------------------
  
  exclusion_check <- DBI::dbGetQuery(
    con,
    "
    SELECT
      COUNT(*) AS incorrectly_recovered
    FROM occurrence_wcvp_hybrid_diagnostics d
    INNER JOIN occurrence_wcvp_hybrid_recovery r
      ON d.vpjd_taxon_universe_id =
         r.vpjd_taxon_universe_id
    WHERE d.hybrid_diagnostic_class <>
          'secure_wcvp_hybrid_resolution'
    "
  )
  
  if (exclusion_check$incorrectly_recovered[[1]] != 0L) {
    stop(
      "04l exclusion safety check failed: non-secure hybrid ",
      "concepts were recovered."
    )
  }
  
  # ---------------------------------------------------------------------------
  # Audit tables
  # ---------------------------------------------------------------------------
  
  recovered_audit <- DBI::dbGetQuery(
    con,
    "
    SELECT
      vpjd_taxon_universe_id,
      taxonKey,
      scientificName,
      family,
      genus,
      species,
      present_records,
      reconstructed_hybrid_name,
      gbif_species_name,
      recovered_wcvp_accepted_plant_name_id,
      recovered_wcvp_taxon_name,
      recovered_wcvp_taxon_authors,
      recovered_wcvp_taxon_rank,
      recovered_wcvp_family,
      supporting_rules
    FROM occurrence_wcvp_hybrid_recovery
    ORDER BY present_records DESC, scientificName
    "
  )
  
  remaining_hybrid_summary <- DBI::dbGetQuery(
    con,
    "
    SELECT
      d.hybrid_diagnostic_class,
      COUNT(*) AS concepts,
      SUM(d.present_records) AS present_records
    FROM occurrence_wcvp_hybrid_diagnostics d
    LEFT JOIN occurrence_wcvp_hybrid_recovery r
      ON d.vpjd_taxon_universe_id =
         r.vpjd_taxon_universe_id
    WHERE r.vpjd_taxon_universe_id IS NULL
    GROUP BY d.hybrid_diagnostic_class
    ORDER BY present_records DESC
    "
  )
  
  remaining_unresolved <- DBI::dbGetQuery(
    con,
    "
    SELECT
      taxonRank,
      COUNT(*) AS concepts,
      SUM(present_records) AS present_records
    FROM occurrence_wcvp_reconciliation_hybrid
    WHERE hybrid_final_wcvp_accepted_plant_name_id IS NULL
    GROUP BY taxonRank
    ORDER BY present_records DESC
    "
  )
  
  # ---------------------------------------------------------------------------
  # Metadata
  # ---------------------------------------------------------------------------
  
  DBI::dbExecute(
    con,
    paste0(
      "
      CREATE TABLE occurrence_wcvp_hybrid_recovery_metadata AS
      SELECT
        CURRENT_TIMESTAMP AS created_at,
        '", VPJD_WCVP_HYBRID_RECOVERY_VERSION,
      "'::VARCHAR AS script_version,
        '0.1.1'::VARCHAR AS source_04k_version,
        ",
      baseline$resolved_before_04l[[1]],
      "::BIGINT AS resolved_before_04l,
        ",
      recovery_check$recovered_concepts[[1]],
      "::BIGINT AS recovered_concepts,
        ",
      recovery_check$recovered_present_records[[1]],
      "::BIGINT AS recovered_present_records,
        ",
      recovery_check$distinct_recovered_accepted_concepts[[1]],
      "::BIGINT AS distinct_recovered_accepted_concepts,
        ",
      final_summary$resolved_after_04l[[1]],
      "::BIGINT AS resolved_after_04l,
        ",
      final_summary$distinct_accepted_after_04l[[1]],
      "::BIGINT AS distinct_accepted_after_04l,
        ",
      final_summary$unresolved_after_04l[[1]],
      "::BIGINT AS unresolved_after_04l
      "
    )
  )
  
  # ---------------------------------------------------------------------------
  # Exports
  # ---------------------------------------------------------------------------
  
  exports <- list(
    hybrid_recovery = recovered_audit,
    hybrid_recovery_summary = recovery_check,
    hybrid_final_summary = final_summary,
    hybrid_remaining_diagnostic_classes = remaining_hybrid_summary,
    hybrid_remaining_unresolved_by_rank = remaining_unresolved
  )
  
  for (nm in names(exports)) {
    readr::write_csv(
      exports[[nm]],
      file.path(output_dir, paste0(nm, ".csv"))
    )
  }
  
  # ---------------------------------------------------------------------------
  # Console
  # ---------------------------------------------------------------------------
  
  cli::cli_h2("Secure hybrid recovery complete")
  cli::cli_text(
    "{.strong Recovered hybrid concepts:} ",
    "{fmt(recovery_check$recovered_concepts[[1]])}"
  )
  cli::cli_text(
    "{.strong PRESENT records represented:} ",
    "{fmt(recovery_check$recovered_present_records[[1]])}"
  )
  cli::cli_text(
    "{.strong Distinct accepted WCVP concepts in recovery:} ",
    "{fmt(recovery_check$distinct_recovered_accepted_concepts[[1]])}"
  )
  cli::cli_text(
    "{.strong Total resolved after 04l:} ",
    "{fmt(final_summary$resolved_after_04l[[1]])}"
  )
  cli::cli_text(
    "{.strong Distinct accepted concepts after 04l:} ",
    "{fmt(final_summary$distinct_accepted_after_04l[[1]])}"
  )
  cli::cli_text(
    "{.strong Remaining unresolved:} ",
    "{fmt(final_summary$unresolved_after_04l[[1]])}"
  )
  cli::cli_alert_success(
    "Only frozen 04k secure_wcvp_hybrid_resolution concepts were promoted."
  )
  cli::cli_alert_success(
    "Review, Artificial Hybrid, ambiguous and no-evidence concepts remain unresolved."
  )
  cli::cli_alert_success(
    "No new taxonomic matching was performed in 04l."
  )
  cli::cli_text(
    "04l_occurrence_wcvp_hybrid_recovery.R ",
    "v{VPJD_WCVP_HYBRID_RECOVERY_VERSION} complete."
  )
  
  invisible(list(
    baseline = baseline,
    diagnostic_check = diagnostic_check,
    recovery_summary = recovery_check,
    final_summary = final_summary,
    recovered_audit = recovered_audit,
    remaining_hybrid_summary = remaining_hybrid_summary,
    remaining_unresolved = remaining_unresolved
  ))
}

wcvp_hybrid_recovery_results <-
  run_occurrence_wcvp_hybrid_recovery()

hybrid_recovery_baseline <-
  wcvp_hybrid_recovery_results$baseline

hybrid_recovery_diagnostic_check <-
  wcvp_hybrid_recovery_results$diagnostic_check

hybrid_recovery_summary <-
  wcvp_hybrid_recovery_results$recovery_summary

hybrid_recovery_final_summary <-
  wcvp_hybrid_recovery_results$final_summary

hybrid_recovered_audit <-
  wcvp_hybrid_recovery_results$recovered_audit

hybrid_remaining_diagnostic_classes <-
  wcvp_hybrid_recovery_results$remaining_hybrid_summary

hybrid_remaining_unresolved_by_rank <-
  wcvp_hybrid_recovery_results$remaining_unresolved