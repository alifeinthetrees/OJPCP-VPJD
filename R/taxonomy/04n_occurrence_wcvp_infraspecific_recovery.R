# ==============================================================================
# 04n_occurrence_wcvp_infraspecific_recovery.R
# VPJD / Oxford-Japan Plant Conservation Partnership
#
# Purpose:
#   Promote ONLY the secure infraspecific WCVP resolutions identified by the
#   frozen 04m v0.1.1 diagnostic.
#
# Secure classes promoted:
#   1. secure_same_infraspecific_concept
#   2. secure_synonym_to_gbif_parent_species
#
# Explicitly NOT promoted:
#   - review_changed_accepted_concept
#   - multiple_strong_wcvp_concepts
#   - family_epithet_evidence_only
#   - no_wcvp_infraspecific_evidence
#   - any other diagnostic class
#
# IMPORTANT:
#   This module performs NO new matching.
#   It consumes the frozen 04m diagnostic classifications only.
#   No fuzzy matching.
#   No edit-distance matching.
#   No new synonym inference.
#   No family/epithet recovery.
#   Upstream reconciliation tables remain unchanged.
#
# Expected recovery:
#   247 concepts
#   21,444 PRESENT records
#
# Expected resulting baseline:
#   21,165 total concepts
#   19,682 resolved concepts
#   1,483 unresolved concepts
#
# Distinct accepted WCVP concepts are calculated, NOT hard-coded.
#
# Version: 0.1.0
# ==============================================================================

VPJD_WCVP_INFRA_RECOVERY_VERSION <- "0.1.0"
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

run_occurrence_wcvp_infraspecific_recovery <- function() {
  duckdb_path <- here::here(
    "data","interim","occurrences","vpjd_occurrences.duckdb"
  )
  output_dir <- here::here(
    "outputs","tables","taxonomy",
    "occurrence_wcvp_infraspecific_recovery"
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
  
  fmt <- function(x) {
    format(x, big.mark = ",", scientific = FALSE, trim = TRUE)
  }
  
  cli::cli_h1("VPJD deterministic WCVP infraspecific recovery")
  
  # ---------------------------------------------------------------------------
  # Required inputs
  # ---------------------------------------------------------------------------
  
  required_tables <- c(
    "occurrence_wcvp_reconciliation_hybrid",
    "occurrence_wcvp_infraspecific_diagnostics",
    "occurrence_wcvp_infraspecific_diagnostic_metadata"
  )
  
  missing_tables <- setdiff(required_tables, DBI::dbListTables(con))
  
  if (length(missing_tables) > 0L) {
    stop(
      "Required DuckDB table(s) missing: ",
      paste(missing_tables, collapse = ", ")
    )
  }
  
  # ---------------------------------------------------------------------------
  # Validate frozen 04m diagnostic
  # ---------------------------------------------------------------------------
  
  diagnostic_metadata <- DBI::dbGetQuery(
    con,
    "
    SELECT *
    FROM occurrence_wcvp_infraspecific_diagnostic_metadata
    "
  )
  
  if (nrow(diagnostic_metadata) != 1L) {
    stop(
      "Expected exactly one row in ",
      "occurrence_wcvp_infraspecific_diagnostic_metadata."
    )
  }
  
  if (
    is.na(diagnostic_metadata$script_version[[1]]) ||
    diagnostic_metadata$script_version[[1]] != "0.1.1"
  ) {
    stop(
      "04n requires frozen 04m diagnostic version 0.1.1. Found: ",
      diagnostic_metadata$script_version[[1]]
    )
  }
  
  cli::cli_text(
    "{.strong Frozen 04m diagnostic version:} ",
    "{diagnostic_metadata$script_version[[1]]}"
  )
  
  # ---------------------------------------------------------------------------
  # Validate secure 04l baseline
  # ---------------------------------------------------------------------------
  
  baseline <- DBI::dbGetQuery(
    con,
    "
    SELECT
      COUNT(*) AS total_concepts,
      SUM(
        CASE
          WHEN hybrid_final_wcvp_accepted_plant_name_id IS NOT NULL
          THEN 1 ELSE 0
        END
      ) AS resolved_before_04n,
      COUNT(
        DISTINCT hybrid_final_wcvp_accepted_plant_name_id
      ) AS distinct_accepted_before_04n,
      SUM(
        CASE
          WHEN hybrid_final_wcvp_accepted_plant_name_id IS NULL
          THEN 1 ELSE 0
        END
      ) AS unresolved_before_04n
    FROM occurrence_wcvp_reconciliation_hybrid
    "
  )
  
  if (
    baseline$total_concepts[[1]] != 21165L ||
    baseline$resolved_before_04n[[1]] != 19435L ||
    baseline$distinct_accepted_before_04n[[1]] != 12019L ||
    baseline$unresolved_before_04n[[1]] != 1730L
  ) {
    stop(
      "04l baseline differs from validated/frozen state. Expected ",
      "21,165 total / 19,435 resolved / 12,019 distinct accepted / ",
      "1,730 unresolved."
    )
  }
  
  cli::cli_text(
    "{.strong Secure resolved baseline:} ",
    "{fmt(baseline$resolved_before_04n[[1]])}"
  )
  cli::cli_text(
    "{.strong Distinct accepted baseline:} ",
    "{fmt(baseline$distinct_accepted_before_04n[[1]])}"
  )
  cli::cli_text(
    "{.strong Unresolved baseline:} ",
    "{fmt(baseline$unresolved_before_04n[[1]])}"
  )
  
  # ---------------------------------------------------------------------------
  # Validate frozen secure 04m population
  # ---------------------------------------------------------------------------
  
  secure_check <- DBI::dbGetQuery(
    con,
    "
    SELECT
      infra_diagnostic_class,
      COUNT(*) AS concepts,
      SUM(present_records) AS present_records
    FROM occurrence_wcvp_infraspecific_diagnostics
    WHERE infra_diagnostic_class IN (
      'secure_same_infraspecific_concept',
      'secure_synonym_to_gbif_parent_species'
    )
    GROUP BY infra_diagnostic_class
    ORDER BY infra_diagnostic_class
    "
  )
  
  secure_total <- DBI::dbGetQuery(
    con,
    "
    SELECT
      COUNT(*) AS concepts,
      SUM(present_records) AS present_records
    FROM occurrence_wcvp_infraspecific_diagnostics
    WHERE infra_diagnostic_class IN (
      'secure_same_infraspecific_concept',
      'secure_synonym_to_gbif_parent_species'
    )
    "
  )
  
  if (
    secure_total$concepts[[1]] != 247L ||
    secure_total$present_records[[1]] != 21444L
  ) {
    stop(
      "Frozen 04m secure population differs from expected state. Expected ",
      "247 concepts / 21,444 PRESENT records."
    )
  }
  
  if (
    !all(
      c(
        "secure_same_infraspecific_concept",
        "secure_synonym_to_gbif_parent_species"
      ) %in% secure_check$infra_diagnostic_class
    )
  ) {
    stop("One or more expected secure 04m classes are absent.")
  }
  
  same_concept_check <- secure_check[
    secure_check$infra_diagnostic_class ==
      "secure_same_infraspecific_concept",
    ,
    drop = FALSE
  ]
  
  parent_species_check <- secure_check[
    secure_check$infra_diagnostic_class ==
      "secure_synonym_to_gbif_parent_species",
    ,
    drop = FALSE
  ]
  
  if (
    nrow(same_concept_check) != 1L ||
    same_concept_check$concepts[[1]] != 126L ||
    same_concept_check$present_records[[1]] != 14189L
  ) {
    stop(
      "Frozen 04m secure_same_infraspecific_concept population differs ",
      "from expected 126 concepts / 14,189 PRESENT records."
    )
  }
  
  if (
    nrow(parent_species_check) != 1L ||
    parent_species_check$concepts[[1]] != 121L ||
    parent_species_check$present_records[[1]] != 7255L
  ) {
    stop(
      "Frozen 04m secure_synonym_to_gbif_parent_species population differs ",
      "from expected 121 concepts / 7,255 PRESENT records."
    )
  }
  
  cli::cli_text(
    "{.strong Secure 04m concepts to recover:} ",
    "{fmt(secure_total$concepts[[1]])}"
  )
  cli::cli_text(
    "{.strong PRESENT records represented:} ",
    "{fmt(secure_total$present_records[[1]])}"
  )
  
  # ---------------------------------------------------------------------------
  # Remove only 04n-owned products
  # ---------------------------------------------------------------------------
  
  for (tbl in c(
    "occurrence_wcvp_infraspecific_recovery_metadata",
    "occurrence_wcvp_reconciliation_infraspecific",
    "occurrence_wcvp_infraspecific_recovery"
  )) {
    DBI::dbExecute(
      con,
      paste0("DROP TABLE IF EXISTS ", tbl)
    )
  }
  
  # ---------------------------------------------------------------------------
  # Create deterministic recovery table
  #
  # No matching occurs here. We simply promote the unique accepted WCVP target
  # already established by frozen 04m.
  # ---------------------------------------------------------------------------
  
  DBI::dbExecute(
    con,
    "
    CREATE TABLE occurrence_wcvp_infraspecific_recovery AS
    SELECT
      vpjd_taxon_universe_id,
      taxonKey,
      speciesKey,
      family,
      genus,
      species,
      taxonRank,
      scientificName,
      total_records,
      present_records,
      infra_diagnostic_class,
      strong_candidate_accepted_id
        AS recovered_wcvp_accepted_plant_name_id,
      target_wcvp_taxon_name
        AS recovered_wcvp_taxon_name,
      target_wcvp_taxon_authors
        AS recovered_wcvp_taxon_authors,
      target_wcvp_taxon_rank
        AS recovered_wcvp_taxon_rank,
      target_wcvp_taxon_status
        AS recovered_wcvp_taxon_status,
      target_wcvp_family
        AS recovered_wcvp_family,
      strong_supporting_rules
        AS recovery_supporting_rules,
      CASE
        WHEN infra_diagnostic_class =
          'secure_same_infraspecific_concept'
        THEN '04n_secure_same_infraspecific_concept'
        WHEN infra_diagnostic_class =
          'secure_synonym_to_gbif_parent_species'
        THEN '04n_secure_synonym_to_gbif_parent_species'
        ELSE NULL
      END AS recovery_stage
    FROM occurrence_wcvp_infraspecific_diagnostics
    WHERE infra_diagnostic_class IN (
      'secure_same_infraspecific_concept',
      'secure_synonym_to_gbif_parent_species'
    )
    "
  )
  
  # ---------------------------------------------------------------------------
  # Recovery safety checks
  # ---------------------------------------------------------------------------
  
  recovery_summary <- DBI::dbGetQuery(
    con,
    "
    SELECT
      infra_diagnostic_class,
      COUNT(*) AS recovered_concepts,
      SUM(present_records) AS recovered_present_records,
      COUNT(
        DISTINCT recovered_wcvp_accepted_plant_name_id
      ) AS distinct_recovered_accepted_ids
    FROM occurrence_wcvp_infraspecific_recovery
    GROUP BY infra_diagnostic_class
    ORDER BY infra_diagnostic_class
    "
  )
  
  recovery_total <- DBI::dbGetQuery(
    con,
    "
    SELECT
      COUNT(*) AS recovered_concepts,
      SUM(present_records) AS recovered_present_records,
      COUNT(
        DISTINCT recovered_wcvp_accepted_plant_name_id
      ) AS distinct_recovered_accepted_ids,
      SUM(
        CASE
          WHEN recovered_wcvp_accepted_plant_name_id IS NULL
          THEN 1 ELSE 0
        END
      ) AS missing_accepted_ids,
      SUM(
        CASE
          WHEN UPPER(
            COALESCE(recovered_wcvp_taxon_status, '')
          ) <> 'ACCEPTED'
          THEN 1 ELSE 0
        END
      ) AS nonaccepted_targets,
      COUNT(*) -
      COUNT(DISTINCT vpjd_taxon_universe_id)
        AS duplicate_source_concepts
    FROM occurrence_wcvp_infraspecific_recovery
    "
  )
  
  if (
    recovery_total$recovered_concepts[[1]] != 247L ||
    recovery_total$recovered_present_records[[1]] != 21444L ||
    recovery_total$missing_accepted_ids[[1]] != 0L ||
    recovery_total$nonaccepted_targets[[1]] != 0L ||
    recovery_total$duplicate_source_concepts[[1]] != 0L
  ) {
    stop(
      "04n recovery safety check failed. Expected 247 concepts / ",
      "21,444 PRESENT records / zero missing accepted IDs / zero ",
      "non-Accepted targets / zero duplicate source concepts."
    )
  }
  
  # ---------------------------------------------------------------------------
  # Build cumulative reconciliation table
  #
  # Preserve all 04l columns and add 04n final fields.
  # ---------------------------------------------------------------------------
  
  DBI::dbExecute(
    con,
    "
    CREATE TABLE occurrence_wcvp_reconciliation_infraspecific AS
    SELECT
      b.*,
      r.infra_diagnostic_class
        AS infraspecific_recovery_class,
      r.recovery_supporting_rules
        AS infraspecific_recovery_supporting_rules,
      r.recovered_wcvp_accepted_plant_name_id
        AS infraspecific_recovered_wcvp_accepted_plant_name_id,
      r.recovered_wcvp_taxon_name
        AS infraspecific_recovered_wcvp_taxon_name,
      r.recovered_wcvp_taxon_authors
        AS infraspecific_recovered_wcvp_taxon_authors,
      r.recovered_wcvp_taxon_rank
        AS infraspecific_recovered_wcvp_taxon_rank,
      r.recovered_wcvp_taxon_status
        AS infraspecific_recovered_wcvp_taxon_status,
      r.recovered_wcvp_family
        AS infraspecific_recovered_wcvp_family,
      COALESCE(
        b.hybrid_final_wcvp_accepted_plant_name_id,
        r.recovered_wcvp_accepted_plant_name_id
      ) AS infraspecific_final_wcvp_accepted_plant_name_id,
      CASE
        WHEN b.hybrid_final_wcvp_accepted_plant_name_id IS NOT NULL
        THEN b.hybrid_final_resolution_stage
        WHEN r.recovered_wcvp_accepted_plant_name_id IS NOT NULL
        THEN r.recovery_stage
        ELSE NULL
      END AS infraspecific_final_resolution_stage
    FROM occurrence_wcvp_reconciliation_hybrid b
    LEFT JOIN occurrence_wcvp_infraspecific_recovery r
      ON b.vpjd_taxon_universe_id =
         r.vpjd_taxon_universe_id
    "
  )
  
  # ---------------------------------------------------------------------------
  # Validate cumulative result
  # ---------------------------------------------------------------------------
  
  final_summary <- DBI::dbGetQuery(
    con,
    "
    SELECT
      COUNT(*) AS total_concepts,
      SUM(
        CASE
          WHEN infraspecific_final_wcvp_accepted_plant_name_id IS NOT NULL
          THEN 1 ELSE 0
        END
      ) AS resolved_after_04n,
      COUNT(
        DISTINCT infraspecific_final_wcvp_accepted_plant_name_id
      ) AS distinct_accepted_after_04n,
      SUM(
        CASE
          WHEN infraspecific_final_wcvp_accepted_plant_name_id IS NULL
          THEN 1 ELSE 0
        END
      ) AS unresolved_after_04n
    FROM occurrence_wcvp_reconciliation_infraspecific
    "
  )
  
  if (
    final_summary$total_concepts[[1]] !=
    baseline$total_concepts[[1]]
  ) {
    stop("04n changed the total reconciliation row count.")
  }
  
  if (
    final_summary$resolved_after_04n[[1]] != 19682L ||
    final_summary$unresolved_after_04n[[1]] != 1483L
  ) {
    stop(
      "04n cumulative counts differ from expected state. Expected ",
      "19,682 resolved / 1,483 unresolved."
    )
  }
  
  if (
    final_summary$resolved_after_04n[[1]] -
    baseline$resolved_before_04n[[1]] != 247L
  ) {
    stop("04n did not add exactly 247 resolved concepts.")
  }
  
  # ---------------------------------------------------------------------------
  # Ensure no non-secure 04m classes were promoted
  # ---------------------------------------------------------------------------
  
  nonsecure_recovery_check <- DBI::dbGetQuery(
    con,
    "
    SELECT
      COUNT(*) AS nonsecure_recovered
    FROM occurrence_wcvp_infraspecific_recovery
    WHERE infra_diagnostic_class NOT IN (
      'secure_same_infraspecific_concept',
      'secure_synonym_to_gbif_parent_species'
    )
    "
  )
  
  if (nonsecure_recovery_check$nonsecure_recovered[[1]] != 0L) {
    stop("04n incorrectly promoted one or more non-secure 04m classes.")
  }
  
  # ---------------------------------------------------------------------------
  # Remaining unresolved summaries
  # ---------------------------------------------------------------------------
  
  remaining_unresolved_by_rank <- DBI::dbGetQuery(
    con,
    "
    SELECT
      taxonRank,
      COUNT(*) AS unresolved_concepts,
      SUM(present_records) AS present_records
    FROM occurrence_wcvp_reconciliation_infraspecific
    WHERE infraspecific_final_wcvp_accepted_plant_name_id IS NULL
    GROUP BY taxonRank
    ORDER BY present_records DESC
    "
  )
  
  remaining_04m_classes <- DBI::dbGetQuery(
    con,
    "
    SELECT
      d.infra_diagnostic_class,
      COUNT(*) AS unresolved_concepts,
      SUM(d.present_records) AS present_records
    FROM occurrence_wcvp_infraspecific_diagnostics d
    INNER JOIN occurrence_wcvp_reconciliation_infraspecific f
      ON d.vpjd_taxon_universe_id =
         f.vpjd_taxon_universe_id
    WHERE f.infraspecific_final_wcvp_accepted_plant_name_id IS NULL
    GROUP BY d.infra_diagnostic_class
    ORDER BY present_records DESC
    "
  )
  
  # ---------------------------------------------------------------------------
  # Recovery audit
  # ---------------------------------------------------------------------------
  
  recovered_audit <- DBI::dbGetQuery(
    con,
    "
    SELECT
      vpjd_taxon_universe_id,
      taxonKey,
      speciesKey,
      family,
      genus,
      species,
      taxonRank,
      scientificName,
      present_records,
      infra_diagnostic_class,
      recovered_wcvp_accepted_plant_name_id,
      recovered_wcvp_taxon_name,
      recovered_wcvp_taxon_authors,
      recovered_wcvp_taxon_rank,
      recovered_wcvp_taxon_status,
      recovered_wcvp_family,
      recovery_supporting_rules,
      recovery_stage
    FROM occurrence_wcvp_infraspecific_recovery
    ORDER BY present_records DESC, scientificName
    "
  )
  
  # ---------------------------------------------------------------------------
  # Metadata
  # ---------------------------------------------------------------------------
  
  DBI::dbExecute(
    con,
    paste0(
      "
      CREATE TABLE occurrence_wcvp_infraspecific_recovery_metadata AS
      SELECT
        CURRENT_TIMESTAMP AS created_at,
        '",
      VPJD_WCVP_INFRA_RECOVERY_VERSION,
      "'::VARCHAR AS script_version,
        '0.1.1'::VARCHAR AS required_04m_version,
        ",
      baseline$resolved_before_04n[[1]],
      "::BIGINT AS resolved_before_04n,
        ",
      baseline$distinct_accepted_before_04n[[1]],
      "::BIGINT AS distinct_accepted_before_04n,
        ",
      baseline$unresolved_before_04n[[1]],
      "::BIGINT AS unresolved_before_04n,
        ",
      recovery_total$recovered_concepts[[1]],
      "::BIGINT AS recovered_concepts,
        ",
      recovery_total$recovered_present_records[[1]],
      "::BIGINT AS recovered_present_records,
        ",
      final_summary$resolved_after_04n[[1]],
      "::BIGINT AS resolved_after_04n,
        ",
      final_summary$distinct_accepted_after_04n[[1]],
      "::BIGINT AS distinct_accepted_after_04n,
        ",
      final_summary$unresolved_after_04n[[1]],
      "::BIGINT AS unresolved_after_04n
      "
    )
  )
  
  # ---------------------------------------------------------------------------
  # Exports
  # ---------------------------------------------------------------------------
  
  readr::write_csv(
    recovery_summary,
    file.path(output_dir, "infraspecific_recovery_summary.csv")
  )
  
  readr::write_csv(
    final_summary,
    file.path(output_dir, "infraspecific_recovery_final_summary.csv")
  )
  
  readr::write_csv(
    recovered_audit,
    file.path(output_dir, "infraspecific_recovered_audit.csv")
  )
  
  readr::write_csv(
    remaining_04m_classes,
    file.path(output_dir, "infraspecific_remaining_04m_classes.csv")
  )
  
  readr::write_csv(
    remaining_unresolved_by_rank,
    file.path(output_dir, "infraspecific_remaining_unresolved_by_rank.csv")
  )
  
  # ---------------------------------------------------------------------------
  # Console
  # ---------------------------------------------------------------------------
  
  cli::cli_h2("Deterministic infraspecific recovery complete")
  
  cli::cli_text(
    "{.strong Recovered concepts:} ",
    "{fmt(recovery_total$recovered_concepts[[1]])}"
  )
  
  cli::cli_text(
    "{.strong PRESENT records represented:} ",
    "{fmt(recovery_total$recovered_present_records[[1]])}"
  )
  
  cli::cli_text(
    "{.strong Resolved concepts after 04n:} ",
    "{fmt(final_summary$resolved_after_04n[[1]])}"
  )
  
  cli::cli_text(
    "{.strong Distinct accepted WCVP concepts after 04n:} ",
    "{fmt(final_summary$distinct_accepted_after_04n[[1]])}"
  )
  
  cli::cli_text(
    "{.strong Unresolved concepts after 04n:} ",
    "{fmt(final_summary$unresolved_after_04n[[1]])}"
  )
  
  cli::cli_alert_success(
    "04n promoted only the two secure 04m v0.1.1 diagnostic classes."
  )
  
  cli::cli_alert_success(
    "04l, 04m and all earlier reconciliation tables remain unchanged."
  )
  
  cli::cli_text(
    "04n_occurrence_wcvp_infraspecific_recovery.R ",
    "v{VPJD_WCVP_INFRA_RECOVERY_VERSION} complete."
  )
  
  invisible(
    list(
      baseline = baseline,
      secure_check = secure_check,
      recovery_summary = recovery_summary,
      recovery_total = recovery_total,
      final_summary = final_summary,
      recovered_audit = recovered_audit,
      remaining_04m_classes = remaining_04m_classes,
      remaining_unresolved_by_rank = remaining_unresolved_by_rank
    )
  )
}

wcvp_infra_recovery_results <-
  run_occurrence_wcvp_infraspecific_recovery()

infra_recovery_baseline <-
  wcvp_infra_recovery_results$baseline

infra_recovery_secure_check <-
  wcvp_infra_recovery_results$secure_check

infra_recovery_summary <-
  wcvp_infra_recovery_results$recovery_summary

infra_recovery_total <-
  wcvp_infra_recovery_results$recovery_total

infra_recovery_final_summary <-
  wcvp_infra_recovery_results$final_summary

infra_recovered_audit <-
  wcvp_infra_recovery_results$recovered_audit

infra_remaining_04m_classes <-
  wcvp_infra_recovery_results$remaining_04m_classes

infra_remaining_unresolved_by_rank <-
  wcvp_infra_recovery_results$remaining_unresolved_by_rank