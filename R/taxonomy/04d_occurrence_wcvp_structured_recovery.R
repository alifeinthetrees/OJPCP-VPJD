# ==============================================================================
# 04d_occurrence_wcvp_structured_recovery.R
# VPJD / Oxford-Japan Plant Conservation Partnership
#
# Purpose:
#   Apply a second deterministic recovery stage to species/infraspecific
#   concepts remaining unresolved after 04a + 04b.
#
# Strategy:
#   S1 - Resolve the GBIF species field exactly against WCVP.
#   S2 - Use genus/family context to resolve otherwise ambiguous species matches.
#   S3 - For unresolved infraspecific concepts, identify the accepted WCVP
#        parent species without falsely treating this as an infraspecific match.
#
# Outputs:
#   occurrence_wcvp_structured_recovery
#   occurrence_wcvp_reconciliation_structured
#   occurrence_wcvp_structured_recovery_metadata
#
# Principles:
# - Preserve all 04a/04b accepted resolutions unchanged.
# - Exact deterministic matching only.
# - No fuzzy matching.
# - No automatic collapse of an unresolved infraspecific taxon to species.
# - Parent-species relationships are explicitly labelled as fallback evidence.
# - Ambiguity remains unresolved.
#
# Version: 0.1.0
# ==============================================================================

VPJD_WCVP_STRUCTURED_VERSION <- "0.1.0"

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

run_occurrence_wcvp_structured_recovery <- function() {
  
  duckdb_path <- here::here(
    "data", "interim", "occurrences",
    "vpjd_occurrences.duckdb"
  )
  
  output_dir <- here::here(
    "outputs", "tables", "taxonomy",
    "occurrence_wcvp_structured_recovery"
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
    "occurrence_wcvp_residual_diagnostics",
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
  
  cli::cli_h1(
    "VPJD occurrence-first WCVP structured recovery"
  )
  
  # ---------------------------------------------------------------------------
  # Validate known schemas
  # ---------------------------------------------------------------------------
  
  required_residual_fields <- c(
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
    "wcvp_match_status",
    "recovery_status",
    "residual_resolution_class"
  )
  
  actual_residual_fields <- DBI::dbListFields(
    con,
    "occurrence_wcvp_residual_diagnostics"
  )
  
  missing_residual_fields <- setdiff(
    required_residual_fields,
    actual_residual_fields
  )
  
  if (length(missing_residual_fields) > 0L) {
    stop(
      "occurrence_wcvp_residual_diagnostics missing field(s): ",
      paste(missing_residual_fields, collapse = ", ")
    )
  }
  
  required_wcvp_fields <- c(
    "wcvp_plant_name_id",
    "wcvp_taxon_name",
    "wcvp_taxon_authors",
    "wcvp_taxon_rank",
    "wcvp_taxon_status",
    "wcvp_accepted_plant_name_id",
    "wcvp_scientific_name",
    "wcvp_match_name"
  )
  
  actual_wcvp_fields <- DBI::dbListFields(
    con,
    "wcvp_occurrence_match_index"
  )
  
  missing_wcvp_fields <- setdiff(
    required_wcvp_fields,
    actual_wcvp_fields
  )
  
  if (length(missing_wcvp_fields) > 0L) {
    stop(
      "wcvp_occurrence_match_index missing field(s): ",
      paste(missing_wcvp_fields, collapse = ", ")
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
      SUM(
        CASE
          WHEN final_wcvp_accepted_plant_name_id IS NOT NULL
          THEN 1 ELSE 0
        END
      ) AS resolved_before_04d,
      COUNT(
        DISTINCT final_wcvp_accepted_plant_name_id
      ) AS distinct_accepted_before_04d,
      SUM(
        CASE
          WHEN final_wcvp_accepted_plant_name_id IS NULL
          THEN 1 ELSE 0
        END
      ) AS unresolved_before_04d
    FROM occurrence_wcvp_reconciliation_recovered
    "
  )
  
  fmt <- function(x) {
    format(
      x,
      big.mark = ",",
      scientific = FALSE,
      trim = TRUE
    )
  }
  
  cli::cli_text(
    "{.strong Resolved before 04d:} ",
    "{fmt(baseline$resolved_before_04d[[1]])}"
  )
  
  cli::cli_text(
    "{.strong Unresolved before 04d:} ",
    "{fmt(baseline$unresolved_before_04d[[1]])}"
  )
  
  # ---------------------------------------------------------------------------
  # Remove only 04d-owned objects
  # ---------------------------------------------------------------------------
  
  DBI::dbExecute(
    con,
    "DROP TABLE IF EXISTS occurrence_wcvp_structured_recovery_metadata"
  )
  
  DBI::dbExecute(
    con,
    "DROP TABLE IF EXISTS occurrence_wcvp_reconciliation_structured"
  )
  
  DBI::dbExecute(
    con,
    "DROP TABLE IF EXISTS occurrence_wcvp_structured_recovery"
  )
  
  DBI::dbExecute(
    con,
    "DROP TABLE IF EXISTS occurrence_wcvp_structured_candidates"
  )
  
  # ---------------------------------------------------------------------------
  # Normalize WCVP names
  # ---------------------------------------------------------------------------
  
  DBI::dbExecute(
    con,
    "
    CREATE OR REPLACE TEMP TABLE wcvp_structured_index AS
    SELECT
      wcvp_plant_name_id,
      wcvp_taxon_name,
      wcvp_taxon_authors,
      wcvp_taxon_rank,
      wcvp_taxon_status,
      wcvp_accepted_plant_name_id,
      wcvp_scientific_name,

      LOWER(
        TRIM(
          REGEXP_REPLACE(
            REPLACE(
              COALESCE(wcvp_taxon_name, ''),
              '×',
              'x'
            ),
            '\\s+',
            ' ',
            'g'
          )
        )
      ) AS norm_taxon_name,

      LOWER(
        SPLIT_PART(
          TRIM(COALESCE(wcvp_taxon_name, '')),
          ' ',
          1
        )
      ) AS norm_genus

    FROM wcvp_occurrence_match_index
    "
  )
  
  # ---------------------------------------------------------------------------
  # Normalize unresolved terminal concepts
  # ---------------------------------------------------------------------------
  
  DBI::dbExecute(
    con,
    "
    CREATE OR REPLACE TEMP TABLE structured_input AS
    SELECT
      *,

      LOWER(
        TRIM(
          REGEXP_REPLACE(
            REPLACE(
              COALESCE(species, ''),
              '×',
              'x'
            ),
            '\\s+',
            ' ',
            'g'
          )
        )
      ) AS norm_species,

      LOWER(
        TRIM(
          REGEXP_REPLACE(
            REPLACE(
              COALESCE(genus, ''),
              '×',
              'x'
            ),
            '\\s+',
            ' ',
            'g'
          )
        )
      ) AS norm_genus

    FROM occurrence_wcvp_residual_diagnostics

    WHERE residual_resolution_class =
          'species_or_infraspecific'
    "
  )
  
  # ---------------------------------------------------------------------------
  # Candidate parent/species matches
  #
  # We deliberately match the GBIF species field to WCVP Species only.
  # ---------------------------------------------------------------------------
  
  DBI::dbExecute(
    con,
    "
    CREATE TABLE occurrence_wcvp_structured_candidates AS
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

      w.wcvp_plant_name_id,
      w.wcvp_taxon_name,
      w.wcvp_taxon_authors,
      w.wcvp_taxon_rank,
      w.wcvp_taxon_status,
      w.wcvp_accepted_plant_name_id,
      w.wcvp_scientific_name,

      CASE
        WHEN g.norm_genus = w.norm_genus
        THEN TRUE
        ELSE FALSE
      END AS genus_context_agrees

    FROM structured_input g

    INNER JOIN wcvp_structured_index w
      ON g.norm_species = w.norm_taxon_name

    WHERE g.norm_species <> ''
      AND UPPER(COALESCE(w.wcvp_taxon_rank, '')) = 'SPECIES'
    "
  )
  
  # ---------------------------------------------------------------------------
  # Summarise candidate structure
  # ---------------------------------------------------------------------------
  
  DBI::dbExecute(
    con,
    "
    CREATE OR REPLACE TEMP TABLE structured_candidate_counts AS
    SELECT
      vpjd_taxon_universe_id,

      COUNT(
        DISTINCT wcvp_plant_name_id
      ) AS matched_wcvp_name_count,

      COUNT(
        DISTINCT wcvp_accepted_plant_name_id
      ) AS accepted_species_count,

      COUNT(
        DISTINCT CASE
          WHEN genus_context_agrees
          THEN wcvp_accepted_plant_name_id
          ELSE NULL
        END
      ) AS genus_agreeing_accepted_species_count,

      MIN(
        CASE
          WHEN genus_context_agrees
          THEN wcvp_plant_name_id
          ELSE NULL
        END
      ) AS genus_agreeing_wcvp_plant_name_id,

      MIN(
        CASE
          WHEN genus_context_agrees
          THEN wcvp_accepted_plant_name_id
          ELSE NULL
        END
      ) AS genus_agreeing_accepted_species_id,

      MIN(wcvp_plant_name_id)
        AS any_wcvp_plant_name_id,

      MIN(wcvp_accepted_plant_name_id)
        AS any_accepted_species_id

    FROM occurrence_wcvp_structured_candidates

    GROUP BY vpjd_taxon_universe_id
    "
  )
  
  # ---------------------------------------------------------------------------
  # Structured result
  #
  # Important:
  # A species-rank residual may be genuinely recovered if its GBIF species
  # field points uniquely to an accepted WCVP species.
  #
  # An infraspecific residual is NOT treated as fully resolved merely because
  # its parent species can be identified.
  # ---------------------------------------------------------------------------
  
  DBI::dbExecute(
    con,
    "
    CREATE TABLE occurrence_wcvp_structured_recovery AS
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
      g.wcvp_match_status,
      g.recovery_status,

      COALESCE(
        c.matched_wcvp_name_count,
        0
      ) AS matched_wcvp_name_count,

      COALESCE(
        c.accepted_species_count,
        0
      ) AS accepted_species_count,

      COALESCE(
        c.genus_agreeing_accepted_species_count,
        0
      ) AS genus_agreeing_accepted_species_count,

      CASE
        WHEN UPPER(g.taxonRank) = 'SPECIES'
         AND c.genus_agreeing_accepted_species_count = 1
        THEN 'species_recovered_genus_context'

        WHEN UPPER(g.taxonRank) = 'SPECIES'
         AND c.accepted_species_count = 1
        THEN 'species_recovered_unique'

        WHEN UPPER(g.taxonRank) = 'SPECIES'
         AND c.accepted_species_count > 1
        THEN 'species_ambiguous'

        WHEN UPPER(g.taxonRank) IN (
          'SUBSPECIES',
          'VARIETY',
          'FORM'
        )
         AND c.genus_agreeing_accepted_species_count = 1
        THEN 'parent_species_identified_genus_context'

        WHEN UPPER(g.taxonRank) IN (
          'SUBSPECIES',
          'VARIETY',
          'FORM'
        )
         AND c.accepted_species_count = 1
        THEN 'parent_species_identified'

        WHEN UPPER(g.taxonRank) IN (
          'SUBSPECIES',
          'VARIETY',
          'FORM'
        )
         AND c.accepted_species_count > 1
        THEN 'parent_species_ambiguous'

        ELSE 'no_structured_match'
      END AS structured_status,

      CASE
        WHEN c.genus_agreeing_accepted_species_count = 1
        THEN c.genus_agreeing_wcvp_plant_name_id

        WHEN c.accepted_species_count = 1
        THEN c.any_wcvp_plant_name_id

        ELSE NULL
      END AS structured_matched_wcvp_plant_name_id,

      CASE
        WHEN c.genus_agreeing_accepted_species_count = 1
        THEN c.genus_agreeing_accepted_species_id

        WHEN c.accepted_species_count = 1
        THEN c.any_accepted_species_id

        ELSE NULL
      END AS structured_accepted_species_id

    FROM structured_input g

    LEFT JOIN structured_candidate_counts c
      ON g.vpjd_taxon_universe_id =
         c.vpjd_taxon_universe_id
    "
  )
  
  # ---------------------------------------------------------------------------
  # Consolidate with 04a + 04b
  #
  # Only species-level structured recoveries become accepted taxon resolutions.
  # Parent-species identification for unresolved infraspecific concepts remains
  # supplementary evidence and does NOT populate the final accepted taxon ID.
  # ---------------------------------------------------------------------------
  
  DBI::dbExecute(
    con,
    "
    CREATE TABLE occurrence_wcvp_reconciliation_structured AS
    SELECT
      r.*,

      CASE
        WHEN r.final_wcvp_accepted_plant_name_id IS NOT NULL
        THEN r.final_wcvp_accepted_plant_name_id

        WHEN s.structured_status IN (
          'species_recovered_genus_context',
          'species_recovered_unique'
        )
        THEN s.structured_accepted_species_id

        ELSE NULL
      END AS structured_final_wcvp_accepted_plant_name_id,

      CASE
        WHEN r.final_wcvp_accepted_plant_name_id IS NOT NULL
        THEN r.final_resolution_stage

        WHEN s.structured_status IN (
          'species_recovered_genus_context',
          'species_recovered_unique'
        )
        THEN '04d'

        ELSE NULL
      END AS structured_final_resolution_stage,

      s.structured_status,
      s.structured_matched_wcvp_plant_name_id,
      s.structured_accepted_species_id

    FROM occurrence_wcvp_reconciliation_recovered r

    LEFT JOIN occurrence_wcvp_structured_recovery s
      ON r.vpjd_taxon_universe_id =
         s.vpjd_taxon_universe_id
    "
  )
  
  # ---------------------------------------------------------------------------
  # Summaries
  # ---------------------------------------------------------------------------
  
  structured_summary <- DBI::dbGetQuery(
    con,
    "
    SELECT
      structured_status,
      COUNT(*) AS concepts,
      SUM(present_records) AS present_records
    FROM occurrence_wcvp_structured_recovery
    GROUP BY structured_status
    ORDER BY concepts DESC
    "
  )
  
  final_summary <- DBI::dbGetQuery(
    con,
    "
    SELECT
      COUNT(*) AS total_concepts,

      SUM(
        CASE
          WHEN structured_final_wcvp_accepted_plant_name_id IS NOT NULL
          THEN 1 ELSE 0
        END
      ) AS resolved_concepts,

      SUM(
        CASE
          WHEN structured_final_resolution_stage = '04a'
          THEN 1 ELSE 0
        END
      ) AS resolved_in_04a,

      SUM(
        CASE
          WHEN structured_final_resolution_stage = '04b'
          THEN 1 ELSE 0
        END
      ) AS resolved_in_04b,

      SUM(
        CASE
          WHEN structured_final_resolution_stage = '04d'
          THEN 1 ELSE 0
        END
      ) AS resolved_in_04d,

      COUNT(
        DISTINCT structured_final_wcvp_accepted_plant_name_id
      ) AS distinct_accepted_wcvp_concepts,

      SUM(
        CASE
          WHEN structured_final_wcvp_accepted_plant_name_id IS NULL
          THEN 1 ELSE 0
        END
      ) AS unresolved_concepts

    FROM occurrence_wcvp_reconciliation_structured
    "
  )
  
  parent_summary <- DBI::dbGetQuery(
    con,
    "
    SELECT
      structured_status,
      COUNT(*) AS concepts,
      SUM(present_records) AS present_records
    FROM occurrence_wcvp_structured_recovery
    WHERE structured_status LIKE 'parent_species_%'
    GROUP BY structured_status
    ORDER BY concepts DESC
    "
  )
  
  # ---------------------------------------------------------------------------
  # High-frequency residuals after 04d
  # ---------------------------------------------------------------------------
  
  remaining_residuals <- DBI::dbGetQuery(
    con,
    "
    SELECT
      r.vpjd_taxon_universe_id,
      r.taxonKey,
      r.speciesKey,
      r.scientificName,
      r.taxonRank,
      r.family,
      r.genus,
      r.species,
      r.infraspecificEpithet,
      r.present_records,
      r.total_records,
      r.wcvp_match_status,
      s.structured_status,
      s.structured_accepted_species_id
    FROM occurrence_wcvp_residual_diagnostics r

    LEFT JOIN occurrence_wcvp_structured_recovery s
      ON r.vpjd_taxon_universe_id =
         s.vpjd_taxon_universe_id

    LEFT JOIN occurrence_wcvp_reconciliation_structured f
      ON r.vpjd_taxon_universe_id =
         f.vpjd_taxon_universe_id

    WHERE f.structured_final_wcvp_accepted_plant_name_id IS NULL

    ORDER BY
      r.present_records DESC,
      r.scientificName
    "
  )
  
  # ---------------------------------------------------------------------------
  # Accepted species details
  # ---------------------------------------------------------------------------
  
  structured_species_details <- DBI::dbGetQuery(
    con,
    "
    SELECT
      s.*,
      a.accepted_wcvp_taxon_name,
      a.accepted_wcvp_taxon_authors,
      a.accepted_wcvp_taxon_rank,
      a.accepted_wcvp_scientific_name

    FROM occurrence_wcvp_structured_recovery s

    LEFT JOIN wcvp_occurrence_accepted_lookup a
      ON s.structured_accepted_species_id =
         a.accepted_wcvp_plant_name_id

    ORDER BY
      s.present_records DESC,
      s.scientificName
    "
  )
  
  # ---------------------------------------------------------------------------
  # Metadata
  # ---------------------------------------------------------------------------
  
  DBI::dbExecute(
    con,
    "
    CREATE TABLE occurrence_wcvp_structured_recovery_metadata AS
    SELECT
      CURRENT_TIMESTAMP AS created_at,
      '0.1.0'::VARCHAR AS script_version,
      COUNT(*) AS input_terminal_residuals,

      SUM(
        CASE
          WHEN structured_status IN (
            'species_recovered_genus_context',
            'species_recovered_unique'
          )
          THEN 1 ELSE 0
        END
      ) AS species_recovered,

      SUM(
        CASE
          WHEN structured_status LIKE 'parent_species_identified%'
          THEN 1 ELSE 0
        END
      ) AS parent_species_identified

    FROM occurrence_wcvp_structured_recovery
    "
  )
  
  # ---------------------------------------------------------------------------
  # Export
  # ---------------------------------------------------------------------------
  
  readr::write_csv(
    structured_species_details,
    file.path(
      output_dir,
      "occurrence_wcvp_structured_recovery.csv"
    )
  )
  
  readr::write_csv(
    structured_summary,
    file.path(
      output_dir,
      "structured_recovery_summary.csv"
    )
  )
  
  readr::write_csv(
    final_summary,
    file.path(
      output_dir,
      "structured_final_summary.csv"
    )
  )
  
  readr::write_csv(
    parent_summary,
    file.path(
      output_dir,
      "infraspecific_parent_species_summary.csv"
    )
  )
  
  readr::write_csv(
    remaining_residuals,
    file.path(
      output_dir,
      "remaining_residuals_after_04d.csv"
    )
  )
  
  # ---------------------------------------------------------------------------
  # Console
  # ---------------------------------------------------------------------------
  
  cli::cli_h2(
    "Structured WCVP recovery complete"
  )
  
  cli::cli_text(
    "{.strong Terminal residual concepts examined:} ",
    "{fmt(nrow(structured_species_details))}"
  )
  
  cli::cli_text(
    "{.strong Resolved concepts after 04d:} ",
    "{fmt(final_summary$resolved_concepts[[1]])}"
  )
  
  cli::cli_text(
    "{.strong Newly resolved in 04d:} ",
    "{fmt(final_summary$resolved_in_04d[[1]])}"
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
    "04a and 04b accepted resolutions were preserved unchanged."
  )
  
  cli::cli_alert_success(
    "Infraspecific parent-species matches were not treated as accepted-taxon resolutions."
  )
  
  cli::cli_text(
    "04d_occurrence_wcvp_structured_recovery.R ",
    "v{VPJD_WCVP_STRUCTURED_VERSION} complete."
  )
  
  invisible(
    list(
      baseline = baseline,
      structured_summary = structured_summary,
      final_summary = final_summary,
      parent_summary = parent_summary,
      remaining_residuals = remaining_residuals
    )
  )
}

wcvp_structured_results <-
  run_occurrence_wcvp_structured_recovery()

structured_summary <-
  wcvp_structured_results$structured_summary

structured_final_summary <-
  wcvp_structured_results$final_summary

parent_summary <-
  wcvp_structured_results$parent_summary

remaining_residuals <-
  wcvp_structured_results$remaining_residuals