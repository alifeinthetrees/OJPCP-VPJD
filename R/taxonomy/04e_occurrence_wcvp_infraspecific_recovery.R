# ==============================================================================
# 04e_occurrence_wcvp_infraspecific_recovery.R
# VPJD / Oxford-Japan Plant Conservation Partnership
#
# Purpose:
#   Deterministically recover unresolved infraspecific GBIF concepts by
#   constraining WCVP searches to the accepted parent species identified in 04d.
#
# Strategy:
#   E1 - Identify WCVP infraspecific names within the known accepted parent.
#   E2 - Extract and normalise WCVP infraspecific epithets.
#   E3 - Match GBIF epithet + rank within the accepted parent-species concept.
#   E4 - Accept only one unique resulting accepted WCVP concept.
#
# Principles:
# - Preserve all 04a/04b/04d accepted resolutions unchanged.
# - No fuzzy matching.
# - No global epithet-only matching.
# - No automatic parent-species collapse.
# - Ambiguous candidates remain unresolved.
#
# Version: 0.1.1
# ==============================================================================

VPJD_WCVP_INFRA_VERSION <- "0.1.1"

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

run_occurrence_wcvp_infraspecific_recovery <- function() {
  
  duckdb_path <- here::here(
    "data", "interim", "occurrences",
    "vpjd_occurrences.duckdb"
  )
  
  output_dir <- here::here(
    "outputs", "tables", "taxonomy",
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
  
  on.exit(
    DBI::dbDisconnect(con, shutdown = TRUE),
    add = TRUE
  )
  
  required_tables <- c(
    "occurrence_wcvp_reconciliation_structured",
    "occurrence_wcvp_structured_recovery",
    "wcvp_occurrence_match_index",
    "wcvp_occurrence_accepted_lookup"
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
  
  cli::cli_h1("VPJD constrained infraspecific WCVP recovery")
  
  # ---------------------------------------------------------------------------
  # Validate schemas
  # ---------------------------------------------------------------------------
  
  required_structured_fields <- c(
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
    "structured_status",
    "structured_accepted_species_id"
  )
  
  actual_structured_fields <- DBI::dbListFields(
    con,
    "occurrence_wcvp_structured_recovery"
  )
  
  missing_structured_fields <- setdiff(
    required_structured_fields,
    actual_structured_fields
  )
  
  if (length(missing_structured_fields) > 0L) {
    stop(
      "occurrence_wcvp_structured_recovery missing field(s): ",
      paste(missing_structured_fields, collapse = ", ")
    )
  }
  
  required_recon_fields <- c(
    "vpjd_taxon_universe_id",
    "structured_final_wcvp_accepted_plant_name_id",
    "structured_final_resolution_stage"
  )
  
  actual_recon_fields <- DBI::dbListFields(
    con,
    "occurrence_wcvp_reconciliation_structured"
  )
  
  missing_recon_fields <- setdiff(
    required_recon_fields,
    actual_recon_fields
  )
  
  if (length(missing_recon_fields) > 0L) {
    stop(
      "occurrence_wcvp_reconciliation_structured missing field(s): ",
      paste(missing_recon_fields, collapse = ", ")
    )
  }
  
  # ---------------------------------------------------------------------------
  # Baseline
  # ---------------------------------------------------------------------------
  
  baseline <- DBI::dbGetQuery(
    con,
    "
    SELECT
      COUNT(*) AS total_concepts,
      SUM(CASE
        WHEN structured_final_wcvp_accepted_plant_name_id IS NOT NULL
        THEN 1 ELSE 0
      END) AS resolved_before_04e,
      COUNT(DISTINCT structured_final_wcvp_accepted_plant_name_id)
        AS distinct_accepted_before_04e,
      SUM(CASE
        WHEN structured_final_wcvp_accepted_plant_name_id IS NULL
        THEN 1 ELSE 0
      END) AS unresolved_before_04e
    FROM occurrence_wcvp_reconciliation_structured
    "
  )
  
  fmt <- function(x) {
    format(x, big.mark = ",", scientific = FALSE, trim = TRUE)
  }
  
  cli::cli_text(
    "{.strong Resolved before 04e:} ",
    "{fmt(baseline$resolved_before_04e[[1]])}"
  )
  
  cli::cli_text(
    "{.strong Unresolved before 04e:} ",
    "{fmt(baseline$unresolved_before_04e[[1]])}"
  )
  
  # ---------------------------------------------------------------------------
  # Remove only 04e-owned persistent objects
  # ---------------------------------------------------------------------------
  
  DBI::dbExecute(
    con,
    "DROP TABLE IF EXISTS occurrence_wcvp_infraspecific_metadata"
  )
  
  DBI::dbExecute(
    con,
    "DROP TABLE IF EXISTS occurrence_wcvp_reconciliation_infraspecific"
  )
  
  DBI::dbExecute(
    con,
    "DROP TABLE IF EXISTS occurrence_wcvp_infraspecific_recovery"
  )
  
  DBI::dbExecute(
    con,
    "DROP TABLE IF EXISTS occurrence_wcvp_infraspecific_candidates"
  )
  
  # ---------------------------------------------------------------------------
  # Input
  # ---------------------------------------------------------------------------
  
  DBI::dbExecute(
    con,
    "
    CREATE OR REPLACE TEMP TABLE infra_input AS
    SELECT
      s.*,
      LOWER(
        TRIM(
          REGEXP_REPLACE(
            REPLACE(COALESCE(s.infraspecificEpithet, ''), '×', 'x'),
            '\\s+',
            ' ',
            'g'
          )
        )
      ) AS norm_infra_epithet
    FROM occurrence_wcvp_structured_recovery s
    INNER JOIN occurrence_wcvp_reconciliation_structured r
      ON s.vpjd_taxon_universe_id = r.vpjd_taxon_universe_id
    WHERE r.structured_final_wcvp_accepted_plant_name_id IS NULL
      AND s.structured_status = 'parent_species_identified_genus_context'
      AND s.structured_accepted_species_id IS NOT NULL
      AND UPPER(COALESCE(s.taxonRank, '')) IN (
        'SUBSPECIES',
        'VARIETY',
        'FORM'
      )
      AND COALESCE(TRIM(s.infraspecificEpithet), '') <> ''
    "
  )
  
  input_summary <- DBI::dbGetQuery(
    con,
    "
    SELECT
      COUNT(*) AS concepts,
      SUM(present_records) AS present_records
    FROM infra_input
    "
  )
  
  cli::cli_text(
    "{.strong Infraspecific concepts entering 04e:} ",
    "{fmt(input_summary$concepts[[1]])}"
  )
  
  # ---------------------------------------------------------------------------
  # Load WCVP
  # ---------------------------------------------------------------------------
  
  if (!requireNamespace("rWCVPdata", quietly = TRUE)) {
    stop("Package rWCVPdata is required.")
  }
  
  wcvp_names <- rWCVPdata::wcvp_names
  
  required_raw_wcvp_fields <- c(
    "plant_name_id",
    "taxon_name",
    "taxon_rank",
    "taxon_status",
    "accepted_plant_name_id"
  )
  
  missing_raw_wcvp_fields <- setdiff(
    required_raw_wcvp_fields,
    names(wcvp_names)
  )
  
  if (length(missing_raw_wcvp_fields) > 0L) {
    stop(
      "rWCVPdata::wcvp_names missing field(s): ",
      paste(missing_raw_wcvp_fields, collapse = ", ")
    )
  }
  
  wcvp_parent_source <- wcvp_names[
    ,
    required_raw_wcvp_fields,
    drop = FALSE
  ]
  
  DBI::dbWriteTable(
    con,
    "wcvp_04e_parent_source",
    wcvp_parent_source,
    temporary = TRUE,
    overwrite = TRUE
  )
  
  rm(wcvp_parent_source, wcvp_names)
  gc()
  
  # ---------------------------------------------------------------------------
  # Normalise WCVP IDs and names.
  #
  # TRY_CAST is deliberate: WCVP IDs may arrive through DBI as VARCHAR.
  # From this point onward all IDs used by 04e are BIGINT.
  # ---------------------------------------------------------------------------
  
  DBI::dbExecute(
    con,
    "
    CREATE OR REPLACE TEMP TABLE wcvp_04e_names AS
    SELECT
      TRY_CAST(plant_name_id AS BIGINT) AS plant_name_id,
      taxon_name,
      UPPER(COALESCE(taxon_rank, '')) AS taxon_rank,
      taxon_status,
      TRY_CAST(accepted_plant_name_id AS BIGINT)
        AS accepted_plant_name_id,
      LOWER(
        TRIM(
          REGEXP_REPLACE(
            REPLACE(COALESCE(taxon_name, ''), '×', 'x'),
            '\\s+',
            ' ',
            'g'
          )
        )
      ) AS norm_taxon_name,
      LOWER(
        TRIM(
          REGEXP_REPLACE(
            REPLACE(
              COALESCE(
                REGEXP_EXTRACT(
                  COALESCE(taxon_name, ''),
                  '^([^ ]+\\s+[^ ]+)',
                  1
                ),
                ''
              ),
              '×',
              'x'
            ),
            '\\s+',
            ' ',
            'g'
          )
        )
      ) AS norm_parent_binomial
    FROM wcvp_04e_parent_source
    WHERE UPPER(COALESCE(taxon_rank, '')) IN (
      'SPECIES',
      'SUBSPECIES',
      'VARIETY',
      'FORM'
    )
    "
  )
  
  # ---------------------------------------------------------------------------
  # Accepted species lookup
  # ---------------------------------------------------------------------------
  
  DBI::dbExecute(
    con,
    "
    CREATE OR REPLACE TEMP TABLE wcvp_04e_species_lookup AS
    SELECT
      norm_taxon_name,
      COUNT(DISTINCT accepted_plant_name_id)
        AS accepted_species_count,
      MIN(accepted_plant_name_id)
        AS accepted_species_id
    FROM wcvp_04e_names
    WHERE taxon_rank = 'SPECIES'
      AND accepted_plant_name_id IS NOT NULL
    GROUP BY norm_taxon_name
    "
  )
  
  # ---------------------------------------------------------------------------
  # Infra index
  # ---------------------------------------------------------------------------
  
  DBI::dbExecute(
    con,
    "
    CREATE OR REPLACE TEMP TABLE wcvp_04e_infra_index AS
    SELECT
      n.plant_name_id AS wcvp_plant_name_id,
      n.taxon_name AS wcvp_taxon_name,
      n.taxon_rank AS wcvp_taxon_rank,
      n.taxon_status AS wcvp_taxon_status,
      n.accepted_plant_name_id AS wcvp_accepted_plant_name_id,
      s.accepted_species_id AS parent_accepted_species_id,
      LOWER(
        TRIM(
          CASE
            WHEN n.taxon_rank = 'SUBSPECIES'
            THEN REGEXP_EXTRACT(
              n.taxon_name,
              '(?:subsp\\.|ssp\\.)\\s+([^ ]+)',
              1
            )
            WHEN n.taxon_rank = 'VARIETY'
            THEN REGEXP_EXTRACT(
              n.taxon_name,
              '(?:var\\.)\\s+([^ ]+)',
              1
            )
            WHEN n.taxon_rank = 'FORM'
            THEN REGEXP_EXTRACT(
              n.taxon_name,
              '(?:f\\.|forma)\\s+([^ ]+)',
              1
            )
            ELSE ''
          END
        )
      ) AS norm_infra_epithet
    FROM wcvp_04e_names n
    INNER JOIN wcvp_04e_species_lookup s
      ON n.norm_parent_binomial = s.norm_taxon_name
    WHERE n.taxon_rank IN (
      'SUBSPECIES',
      'VARIETY',
      'FORM'
    )
      AND s.accepted_species_count = 1
      AND n.accepted_plant_name_id IS NOT NULL
    "
  )
  
  # ---------------------------------------------------------------------------
  # Candidate matches
  # ---------------------------------------------------------------------------
  
  DBI::dbExecute(
    con,
    "
    CREATE TABLE occurrence_wcvp_infraspecific_candidates AS
    SELECT DISTINCT
      g.vpjd_taxon_universe_id,
      g.taxonKey,
      g.speciesKey,
      g.family,
      g.genus,
      g.species,
      g.infraspecificEpithet,
      g.taxonRank,
      g.scientificName,
      g.total_records,
      g.present_records,
      TRY_CAST(g.structured_accepted_species_id AS BIGINT)
        AS structured_accepted_species_id,
      w.wcvp_plant_name_id,
      w.wcvp_taxon_name,
      w.wcvp_taxon_rank,
      w.wcvp_taxon_status,
      w.wcvp_accepted_plant_name_id,
      w.parent_accepted_species_id
    FROM infra_input g
    INNER JOIN wcvp_04e_infra_index w
      ON TRY_CAST(g.structured_accepted_species_id AS BIGINT) =
         w.parent_accepted_species_id
     AND g.norm_infra_epithet = w.norm_infra_epithet
     AND UPPER(g.taxonRank) = w.wcvp_taxon_rank
    WHERE g.norm_infra_epithet <> ''
    "
  )
  
  # ---------------------------------------------------------------------------
  # Candidate counts
  # ---------------------------------------------------------------------------
  
  DBI::dbExecute(
    con,
    "
    CREATE OR REPLACE TEMP TABLE infra_candidate_counts AS
    SELECT
      vpjd_taxon_universe_id,
      COUNT(DISTINCT wcvp_plant_name_id)
        AS matched_wcvp_name_count,
      COUNT(DISTINCT wcvp_accepted_plant_name_id)
        AS accepted_infra_concept_count,
      MIN(wcvp_plant_name_id)
        AS matched_wcvp_plant_name_id,
      MIN(wcvp_accepted_plant_name_id)
        AS accepted_infra_concept_id
    FROM occurrence_wcvp_infraspecific_candidates
    GROUP BY vpjd_taxon_universe_id
    "
  )
  
  # ---------------------------------------------------------------------------
  # Recovery decision
  # ---------------------------------------------------------------------------
  
  DBI::dbExecute(
    con,
    "
    CREATE TABLE occurrence_wcvp_infraspecific_recovery AS
    SELECT
      g.vpjd_taxon_universe_id,
      g.taxonKey,
      g.speciesKey,
      g.family,
      g.genus,
      g.species,
      g.infraspecificEpithet,
      g.taxonRank,
      g.scientificName,
      g.total_records,
      g.present_records,
      TRY_CAST(g.structured_accepted_species_id AS BIGINT)
        AS structured_accepted_species_id,
      COALESCE(c.matched_wcvp_name_count, 0)
        AS matched_wcvp_name_count,
      COALESCE(c.accepted_infra_concept_count, 0)
        AS accepted_infra_concept_count,
      CASE
        WHEN c.accepted_infra_concept_count = 1
        THEN 'recovered'
        WHEN c.accepted_infra_concept_count > 1
        THEN 'ambiguous'
        ELSE 'unmatched'
      END AS infra_recovery_status,
      CASE
        WHEN c.accepted_infra_concept_count = 1
        THEN c.matched_wcvp_plant_name_id
        ELSE CAST(NULL AS BIGINT)
      END AS infra_matched_wcvp_plant_name_id,
      CASE
        WHEN c.accepted_infra_concept_count = 1
        THEN c.accepted_infra_concept_id
        ELSE CAST(NULL AS BIGINT)
      END AS infra_accepted_wcvp_plant_name_id
    FROM infra_input g
    LEFT JOIN infra_candidate_counts c
      ON g.vpjd_taxon_universe_id =
         c.vpjd_taxon_universe_id
    "
  )
  
  # ---------------------------------------------------------------------------
  # Consolidate
  # ---------------------------------------------------------------------------
  
  DBI::dbExecute(
    con,
    "
    CREATE TABLE occurrence_wcvp_reconciliation_infraspecific AS
    SELECT
      r.*,
      CASE
        WHEN r.structured_final_wcvp_accepted_plant_name_id IS NOT NULL
        THEN TRY_CAST(
          r.structured_final_wcvp_accepted_plant_name_id AS BIGINT
        )
        WHEN e.infra_recovery_status = 'recovered'
        THEN e.infra_accepted_wcvp_plant_name_id
        ELSE CAST(NULL AS BIGINT)
      END AS infra_final_wcvp_accepted_plant_name_id,
      CASE
        WHEN r.structured_final_wcvp_accepted_plant_name_id IS NOT NULL
        THEN r.structured_final_resolution_stage
        WHEN e.infra_recovery_status = 'recovered'
        THEN '04e'
        ELSE NULL
      END AS infra_final_resolution_stage,
      e.infra_recovery_status,
      e.infra_matched_wcvp_plant_name_id,
      e.infra_accepted_wcvp_plant_name_id
    FROM occurrence_wcvp_reconciliation_structured r
    LEFT JOIN occurrence_wcvp_infraspecific_recovery e
      ON r.vpjd_taxon_universe_id =
         e.vpjd_taxon_universe_id
    "
  )
  
  # ---------------------------------------------------------------------------
  # Summaries
  # ---------------------------------------------------------------------------
  
  recovery_summary <- DBI::dbGetQuery(
    con,
    "
    SELECT
      infra_recovery_status,
      COUNT(*) AS concepts,
      SUM(present_records) AS present_records
    FROM occurrence_wcvp_infraspecific_recovery
    GROUP BY infra_recovery_status
    ORDER BY concepts DESC
    "
  )
  
  recovery_by_rank <- DBI::dbGetQuery(
    con,
    "
    SELECT
      taxonRank,
      infra_recovery_status,
      COUNT(*) AS concepts,
      SUM(present_records) AS present_records
    FROM occurrence_wcvp_infraspecific_recovery
    GROUP BY taxonRank, infra_recovery_status
    ORDER BY taxonRank, infra_recovery_status
    "
  )
  
  final_summary <- DBI::dbGetQuery(
    con,
    "
    SELECT
      COUNT(*) AS total_concepts,
      SUM(CASE
        WHEN infra_final_wcvp_accepted_plant_name_id IS NOT NULL
        THEN 1 ELSE 0
      END) AS resolved_concepts,
      SUM(CASE
        WHEN infra_final_resolution_stage = '04a'
        THEN 1 ELSE 0
      END) AS resolved_in_04a,
      SUM(CASE
        WHEN infra_final_resolution_stage = '04b'
        THEN 1 ELSE 0
      END) AS resolved_in_04b,
      SUM(CASE
        WHEN infra_final_resolution_stage = '04d'
        THEN 1 ELSE 0
      END) AS resolved_in_04d,
      SUM(CASE
        WHEN infra_final_resolution_stage = '04e'
        THEN 1 ELSE 0
      END) AS resolved_in_04e,
      COUNT(DISTINCT infra_final_wcvp_accepted_plant_name_id)
        AS distinct_accepted_wcvp_concepts,
      SUM(CASE
        WHEN infra_final_wcvp_accepted_plant_name_id IS NULL
        THEN 1 ELSE 0
      END) AS unresolved_concepts
    FROM occurrence_wcvp_reconciliation_infraspecific
    "
  )
  
  recovered_examples <- DBI::dbGetQuery(
    con,
    "
    SELECT
      e.vpjd_taxon_universe_id,
      e.taxonKey,
      e.scientificName,
      e.taxonRank,
      e.family,
      e.genus,
      e.species,
      e.infraspecificEpithet,
      e.present_records,
      e.structured_accepted_species_id,
      e.infra_accepted_wcvp_plant_name_id,
      a.accepted_wcvp_taxon_name,
      a.accepted_wcvp_taxon_authors,
      a.accepted_wcvp_taxon_rank,
      a.accepted_wcvp_scientific_name
    FROM occurrence_wcvp_infraspecific_recovery e
    LEFT JOIN wcvp_occurrence_accepted_lookup a
      ON e.infra_accepted_wcvp_plant_name_id =
         TRY_CAST(a.accepted_wcvp_plant_name_id AS BIGINT)
    WHERE e.infra_recovery_status = 'recovered'
    ORDER BY e.present_records DESC, e.scientificName
    LIMIT 100
    "
  )
  
  remaining_summary <- DBI::dbGetQuery(
    con,
    "
    SELECT
      taxonRank,
      COUNT(*) AS concepts,
      SUM(present_records) AS present_records
    FROM occurrence_wcvp_reconciliation_infraspecific
    WHERE infra_final_wcvp_accepted_plant_name_id IS NULL
    GROUP BY taxonRank
    ORDER BY concepts DESC
    "
  )
  
  # ---------------------------------------------------------------------------
  # Metadata
  # ---------------------------------------------------------------------------
  
  DBI::dbExecute(
    con,
    "
    CREATE TABLE occurrence_wcvp_infraspecific_metadata AS
    SELECT
      CURRENT_TIMESTAMP AS created_at,
      '0.1.1'::VARCHAR AS script_version,
      COUNT(*) AS input_concepts,
      SUM(CASE
        WHEN infra_recovery_status = 'recovered'
        THEN 1 ELSE 0
      END) AS recovered_concepts,
      SUM(CASE
        WHEN infra_recovery_status = 'ambiguous'
        THEN 1 ELSE 0
      END) AS ambiguous_concepts,
      SUM(CASE
        WHEN infra_recovery_status = 'unmatched'
        THEN 1 ELSE 0
      END) AS unmatched_concepts
    FROM occurrence_wcvp_infraspecific_recovery
    "
  )
  
  # ---------------------------------------------------------------------------
  # Export
  # ---------------------------------------------------------------------------
  
  full_recovery <- DBI::dbGetQuery(
    con,
    "
    SELECT *
    FROM occurrence_wcvp_infraspecific_recovery
    ORDER BY present_records DESC, scientificName
    "
  )
  
  readr::write_csv(
    full_recovery,
    file.path(
      output_dir,
      "occurrence_wcvp_infraspecific_recovery.csv"
    )
  )
  
  readr::write_csv(
    recovery_summary,
    file.path(
      output_dir,
      "infraspecific_recovery_summary.csv"
    )
  )
  
  readr::write_csv(
    recovery_by_rank,
    file.path(
      output_dir,
      "infraspecific_recovery_by_rank.csv"
    )
  )
  
  readr::write_csv(
    final_summary,
    file.path(
      output_dir,
      "infraspecific_final_summary.csv"
    )
  )
  
  readr::write_csv(
    recovered_examples,
    file.path(
      output_dir,
      "infraspecific_recovered_examples.csv"
    )
  )
  
  readr::write_csv(
    remaining_summary,
    file.path(
      output_dir,
      "remaining_unresolved_by_rank.csv"
    )
  )
  
  # ---------------------------------------------------------------------------
  # Console
  # ---------------------------------------------------------------------------
  
  cli::cli_h2(
    "Constrained infraspecific WCVP recovery complete"
  )
  
  recovered_n <- recovery_summary |>
    dplyr::filter(infra_recovery_status == "recovered") |>
    dplyr::pull(concepts)
  
  if (length(recovered_n) == 0L) {
    recovered_n <- 0L
  }
  
  cli::cli_text(
    "{.strong 04e input concepts:} ",
    "{fmt(input_summary$concepts[[1]])}"
  )
  
  cli::cli_text(
    "{.strong Deterministically recovered in 04e:} ",
    "{fmt(recovered_n)}"
  )
  
  cli::cli_text(
    "{.strong Total resolved concepts:} ",
    "{fmt(final_summary$resolved_concepts[[1]])}"
  )
  
  cli::cli_text(
    "{.strong Distinct accepted WCVP concepts:} ",
    "{fmt(final_summary$distinct_accepted_wcvp_concepts[[1]])}"
  )
  
  cli::cli_text(
    "{.strong Remaining unresolved concepts:} ",
    "{fmt(final_summary$unresolved_concepts[[1]])}"
  )
  
  cli::cli_alert_success(
    "Earlier accepted resolutions were preserved unchanged."
  )
  
  cli::cli_alert_success(
    "No unresolved infraspecific concept was collapsed automatically to its parent species."
  )
  
  cli::cli_text(
    "04e_occurrence_wcvp_infraspecific_recovery.R ",
    "v{VPJD_WCVP_INFRA_VERSION} complete."
  )
  
  invisible(
    list(
      baseline = baseline,
      input_summary = input_summary,
      recovery_summary = recovery_summary,
      recovery_by_rank = recovery_by_rank,
      final_summary = final_summary,
      recovered_examples = recovered_examples,
      remaining_summary = remaining_summary
    )
  )
}

wcvp_infra_results <-
  run_occurrence_wcvp_infraspecific_recovery()

infra_input_summary <-
  wcvp_infra_results$input_summary

infra_recovery_summary <-
  wcvp_infra_results$recovery_summary

infra_recovery_by_rank <-
  wcvp_infra_results$recovery_by_rank

infra_final_summary <-
  wcvp_infra_results$final_summary

infra_recovered_examples <-
  wcvp_infra_results$recovered_examples

infra_remaining_summary <-
  wcvp_infra_results$remaining_summary
