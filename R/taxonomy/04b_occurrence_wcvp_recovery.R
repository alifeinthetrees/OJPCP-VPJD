# ==============================================================================
# 04b_occurrence_wcvp_recovery.R
# VPJD / Oxford-Japan Plant Conservation Partnership
#
# Purpose:
#   Deterministically recover unresolved species/infraspecific GBIF taxon
#   concepts following the initial occurrence-first WCVP reconciliation.
#
# Inputs:
#   occurrence_wcvp_reconciliation
#   wcvp_occurrence_match_index
#   wcvp_occurrence_accepted_lookup
#
# Outputs:
#   occurrence_wcvp_recovery
#   occurrence_wcvp_reconciliation_recovered
#   occurrence_wcvp_recovery_metadata
#
# Principles:
# - Preserve successful 04a resolutions unchanged.
# - Recover only unresolved species/infraspecific concepts.
# - Exact deterministic matching only.
# - No fuzzy matching.
# - No authoritative binomial truncation.
# - Ambiguous matches remain unresolved.
# - Higher-rank concepts remain untouched.
#
# Version: 0.1.2
# ==============================================================================

VPJD_WCVP_RECOVERY_VERSION <- "0.1.2"

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

run_occurrence_wcvp_recovery <- function() {
  
  duckdb_path <- here::here(
    "data", "interim", "occurrences",
    "vpjd_occurrences.duckdb"
  )
  
  output_dir <- here::here(
    "outputs", "tables", "taxonomy",
    "occurrence_wcvp_recovery"
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
    "occurrence_wcvp_reconciliation",
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
    "VPJD occurrence-first WCVP deterministic recovery"
  )
  
  # ---------------------------------------------------------------------------
  # Validate actual schemas
  # ---------------------------------------------------------------------------
  
  expected_schemas <- list(
    occurrence_wcvp_reconciliation = c(
      "vpjd_taxon_universe_id",
      "taxonKey",
      "speciesKey",
      "family",
      "genus",
      "species",
      "infraspecificEpithet",
      "taxonRank",
      "scientificName",
      "wcvp_plant_name_id",
      "wcvp_accepted_plant_name_id",
      "wcvp_match_status"
    ),
    wcvp_occurrence_match_index = c(
      "wcvp_plant_name_id",
      "wcvp_taxon_name",
      "wcvp_taxon_authors",
      "wcvp_taxon_rank",
      "wcvp_taxon_status",
      "wcvp_accepted_plant_name_id",
      "wcvp_scientific_name",
      "wcvp_match_name"
    ),
    wcvp_occurrence_accepted_lookup = c(
      "accepted_wcvp_plant_name_id",
      "accepted_wcvp_taxon_name",
      "accepted_wcvp_taxon_authors",
      "accepted_wcvp_taxon_rank",
      "accepted_wcvp_scientific_name"
    )
  )
  
  for (tbl in names(expected_schemas)) {
    actual <- DBI::dbListFields(con, tbl)
    missing <- setdiff(expected_schemas[[tbl]], actual)
    
    if (length(missing) > 0L) {
      stop(
        tbl,
        " missing field(s): ",
        paste(missing, collapse = ", ")
      )
    }
  }
  
  # ---------------------------------------------------------------------------
  # 04a baseline
  # ---------------------------------------------------------------------------
  
  baseline <- DBI::dbGetQuery(
    con,
    "
    SELECT
      COUNT(*) AS total_concepts,
      SUM(
        CASE
          WHEN wcvp_accepted_plant_name_id IS NOT NULL
          THEN 1 ELSE 0
        END
      ) AS resolved_concepts,
      COUNT(
        DISTINCT wcvp_accepted_plant_name_id
      ) AS distinct_accepted_concepts,
      SUM(
        CASE
          WHEN wcvp_accepted_plant_name_id IS NULL
          THEN 1 ELSE 0
        END
      ) AS unresolved_concepts
    FROM occurrence_wcvp_reconciliation
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
    "{.strong 04a total concepts:} ",
    "{fmt(baseline$total_concepts[[1]])}"
  )
  
  cli::cli_text(
    "{.strong 04a resolved concepts:} ",
    "{fmt(baseline$resolved_concepts[[1]])}"
  )
  
  cli::cli_text(
    "{.strong 04a distinct accepted concepts:} ",
    "{fmt(baseline$distinct_accepted_concepts[[1]])}"
  )
  
  cli::cli_text(
    "{.strong 04a unresolved concepts:} ",
    "{fmt(baseline$unresolved_concepts[[1]])}"
  )
  
  # ---------------------------------------------------------------------------
  # Remove only 04b-owned objects
  # ---------------------------------------------------------------------------
  
  DBI::dbExecute(
    con,
    "DROP TABLE IF EXISTS occurrence_wcvp_recovery_metadata"
  )
  
  DBI::dbExecute(
    con,
    "DROP TABLE IF EXISTS occurrence_wcvp_reconciliation_recovered"
  )
  
  DBI::dbExecute(
    con,
    "DROP TABLE IF EXISTS occurrence_wcvp_recovery"
  )
  
  DBI::dbExecute(
    con,
    "DROP TABLE IF EXISTS occurrence_wcvp_recovery_candidates"
  )
  
  DBI::dbExecute(
    con,
    "DROP TABLE IF EXISTS occurrence_wcvp_unresolved_input"
  )
  
  # ---------------------------------------------------------------------------
  # Recovery population
  # ---------------------------------------------------------------------------
  
  DBI::dbExecute(
    con,
    "
    CREATE TABLE occurrence_wcvp_unresolved_input AS
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
      wcvp_match_status AS original_wcvp_match_status,
      wcvp_plant_name_id AS original_wcvp_plant_name_id,
      wcvp_accepted_plant_name_id
        AS original_wcvp_accepted_plant_name_id
    FROM occurrence_wcvp_reconciliation
    WHERE wcvp_accepted_plant_name_id IS NULL
      AND UPPER(COALESCE(taxonRank, '')) IN (
        'SPECIES',
        'SUBSPECIES',
        'VARIETY',
        'FORM'
      )
    "
  )
  
  unresolved_input_n <- DBI::dbGetQuery(
    con,
    "
    SELECT COUNT(*) AS n
    FROM occurrence_wcvp_unresolved_input
    "
  )$n[[1]]
  
  cli::cli_text(
    "{.strong Species/infraspecific concepts entering recovery:} ",
    "{fmt(unresolved_input_n)}"
  )
  
  # ---------------------------------------------------------------------------
  # Normalize WCVP matching fields
  # ---------------------------------------------------------------------------
  
  DBI::dbExecute(
    con,
    "
    CREATE OR REPLACE TEMP TABLE wcvp_recovery_index AS
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
              COALESCE(wcvp_match_name, ''),
              '×',
              'x'
            ),
            '\\s+',
            ' ',
            'g'
          )
        )
      ) AS norm_full_name,

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
      ) AS norm_taxon_name

    FROM wcvp_occurrence_match_index
    "
  )
  
  # ---------------------------------------------------------------------------
  # Normalize unresolved GBIF fields
  # ---------------------------------------------------------------------------
  
  DBI::dbExecute(
    con,
    "
    CREATE OR REPLACE TEMP TABLE gbif_recovery_input AS
    SELECT
      *,

      LOWER(
        TRIM(
          REGEXP_REPLACE(
            REPLACE(
              COALESCE(scientificName, ''),
              '×',
              'x'
            ),
            '\\s+',
            ' ',
            'g'
          )
        )
      ) AS norm_scientific_name,

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
      ) AS norm_species_name

    FROM occurrence_wcvp_unresolved_input
    "
  )
  
  # ---------------------------------------------------------------------------
  # Deterministic recovery candidates
  #
  # R1:
  #   GBIF scientificName == complete WCVP name including authorship.
  #
  # R2:
  #   GBIF scientificName == WCVP taxon_name, with compatible rank.
  #
  # R3:
  #   GBIF species field == WCVP taxon_name, with compatible rank.
  #
  # R3 is particularly useful where GBIF scientificName contains authorship
  # or formatting that prevented the original exact match.
  # ---------------------------------------------------------------------------
  
  DBI::dbExecute(
    con,
    "
    CREATE TABLE occurrence_wcvp_recovery_candidates AS

    SELECT
      g.vpjd_taxon_universe_id,
      'R1_FULL_NAME_EXACT'::VARCHAR AS recovery_stage,
      1 AS stage_priority,
      w.wcvp_plant_name_id,
      w.wcvp_accepted_plant_name_id,
      w.wcvp_taxon_name,
      w.wcvp_taxon_authors,
      w.wcvp_taxon_rank,
      w.wcvp_taxon_status,
      w.wcvp_scientific_name

    FROM gbif_recovery_input g

    INNER JOIN wcvp_recovery_index w
      ON g.norm_scientific_name =
         w.norm_full_name

    WHERE g.norm_scientific_name <> ''

    UNION ALL

    SELECT
      g.vpjd_taxon_universe_id,
      'R2_TAXON_NAME_EXACT'::VARCHAR,
      2,
      w.wcvp_plant_name_id,
      w.wcvp_accepted_plant_name_id,
      w.wcvp_taxon_name,
      w.wcvp_taxon_authors,
      w.wcvp_taxon_rank,
      w.wcvp_taxon_status,
      w.wcvp_scientific_name

    FROM gbif_recovery_input g

    INNER JOIN wcvp_recovery_index w
      ON g.norm_scientific_name =
         w.norm_taxon_name

    WHERE g.norm_scientific_name <> ''
      AND UPPER(COALESCE(g.taxonRank, '')) =
          UPPER(COALESCE(w.wcvp_taxon_rank, ''))

    UNION ALL

    SELECT
      g.vpjd_taxon_universe_id,
      'R3_SPECIES_FIELD_EXACT'::VARCHAR,
      3,
      w.wcvp_plant_name_id,
      w.wcvp_accepted_plant_name_id,
      w.wcvp_taxon_name,
      w.wcvp_taxon_authors,
      w.wcvp_taxon_rank,
      w.wcvp_taxon_status,
      w.wcvp_scientific_name

    FROM gbif_recovery_input g

    INNER JOIN wcvp_recovery_index w
      ON g.norm_species_name =
         w.norm_taxon_name

    WHERE g.norm_species_name <> ''
      AND UPPER(COALESCE(g.taxonRank, '')) =
          UPPER(COALESCE(w.wcvp_taxon_rank, ''))
    "
  )
  
  # ---------------------------------------------------------------------------
  # Select highest-priority stage with candidates
  # ---------------------------------------------------------------------------
  
  DBI::dbExecute(
    con,
    "
    CREATE OR REPLACE TEMP TABLE recovery_best_stage AS
    SELECT
      vpjd_taxon_universe_id,
      MIN(stage_priority) AS best_stage_priority
    FROM occurrence_wcvp_recovery_candidates
    GROUP BY vpjd_taxon_universe_id
    "
  )
  
  DBI::dbExecute(
    con,
    "
    CREATE OR REPLACE TEMP TABLE recovery_best_candidates AS
    SELECT DISTINCT
      c.*
    FROM occurrence_wcvp_recovery_candidates c
    INNER JOIN recovery_best_stage b
      ON c.vpjd_taxon_universe_id =
         b.vpjd_taxon_universe_id
     AND c.stage_priority =
         b.best_stage_priority
    "
  )
  
  # ---------------------------------------------------------------------------
  # Count distinct accepted concepts at best stage
  # ---------------------------------------------------------------------------
  
  DBI::dbExecute(
    con,
    "
    CREATE OR REPLACE TEMP TABLE recovery_best_counts AS
    SELECT
      vpjd_taxon_universe_id,
      MIN(recovery_stage) AS recovery_stage,
      MIN(stage_priority) AS stage_priority,

      COUNT(
        DISTINCT wcvp_plant_name_id
      ) AS matched_name_count,

      COUNT(
        DISTINCT wcvp_accepted_plant_name_id
      ) AS accepted_concept_count,

      MIN(wcvp_plant_name_id)
        AS matched_wcvp_plant_name_id,

      MIN(wcvp_accepted_plant_name_id)
        AS recovered_wcvp_accepted_plant_name_id

    FROM recovery_best_candidates

    GROUP BY vpjd_taxon_universe_id
    "
  )
  
  # ---------------------------------------------------------------------------
  # Recovery result
  # ---------------------------------------------------------------------------
  
  DBI::dbExecute(
    con,
    "
    CREATE TABLE occurrence_wcvp_recovery AS
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
      g.original_wcvp_match_status,

      CASE
        WHEN b.accepted_concept_count = 1
        THEN 'recovered'
        WHEN b.accepted_concept_count > 1
        THEN 'ambiguous'
        ELSE 'unmatched'
      END AS recovery_status,

      b.recovery_stage,

      CASE
        WHEN b.accepted_concept_count = 1
        THEN b.matched_wcvp_plant_name_id
        ELSE NULL
      END AS recovery_matched_wcvp_plant_name_id,

      CASE
        WHEN b.accepted_concept_count = 1
        THEN b.recovered_wcvp_accepted_plant_name_id
        ELSE NULL
      END AS recovered_wcvp_accepted_plant_name_id,

      COALESCE(
        b.matched_name_count,
        0
      ) AS best_stage_matched_name_count,

      COALESCE(
        b.accepted_concept_count,
        0
      ) AS best_stage_accepted_concept_count

    FROM gbif_recovery_input g

    LEFT JOIN recovery_best_counts b
      ON g.vpjd_taxon_universe_id =
         b.vpjd_taxon_universe_id
    "
  )
  
  # ---------------------------------------------------------------------------
  # Consolidate 04a + 04b
  # ---------------------------------------------------------------------------
  
  DBI::dbExecute(
    con,
    "
    CREATE TABLE occurrence_wcvp_reconciliation_recovered AS
    SELECT
      r.*,

      CASE
        WHEN r.wcvp_accepted_plant_name_id IS NOT NULL
        THEN r.wcvp_accepted_plant_name_id

        WHEN rec.recovery_status = 'recovered'
        THEN rec.recovered_wcvp_accepted_plant_name_id

        ELSE NULL
      END AS final_wcvp_accepted_plant_name_id,

      CASE
        WHEN r.wcvp_accepted_plant_name_id IS NOT NULL
        THEN '04a'

        WHEN rec.recovery_status = 'recovered'
        THEN '04b'

        ELSE NULL
      END AS final_resolution_stage,

      rec.recovery_status,
      rec.recovery_stage,
      rec.recovery_matched_wcvp_plant_name_id,
      rec.best_stage_matched_name_count,
      rec.best_stage_accepted_concept_count

    FROM occurrence_wcvp_reconciliation r

    LEFT JOIN occurrence_wcvp_recovery rec
      ON r.vpjd_taxon_universe_id =
         rec.vpjd_taxon_universe_id
    "
  )
  
  # ---------------------------------------------------------------------------
  # Attach accepted-name details for recovered concepts
  # ---------------------------------------------------------------------------
  
  DBI::dbExecute(
    con,
    "
    CREATE OR REPLACE TEMP TABLE recovered_accepted_details AS
    SELECT
      r.vpjd_taxon_universe_id,
      a.accepted_wcvp_plant_name_id,
      a.accepted_wcvp_taxon_name,
      a.accepted_wcvp_taxon_authors,
      a.accepted_wcvp_taxon_rank,
      a.accepted_wcvp_scientific_name

    FROM occurrence_wcvp_recovery r

    INNER JOIN wcvp_occurrence_accepted_lookup a
      ON r.recovered_wcvp_accepted_plant_name_id =
         a.accepted_wcvp_plant_name_id

    WHERE r.recovery_status = 'recovered'
    "
  )
  
  # ---------------------------------------------------------------------------
  # Summaries
  # ---------------------------------------------------------------------------
  
  recovery_summary <- DBI::dbGetQuery(
    con,
    "
    SELECT
      recovery_status,
      COUNT(*) AS concepts
    FROM occurrence_wcvp_recovery
    GROUP BY recovery_status
    ORDER BY concepts DESC
    "
  )
  
  stage_summary <- DBI::dbGetQuery(
    con,
    "
    SELECT
      recovery_stage,
      COUNT(*) AS concepts
    FROM occurrence_wcvp_recovery
    WHERE recovery_status = 'recovered'
    GROUP BY recovery_stage
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
          WHEN final_wcvp_accepted_plant_name_id IS NOT NULL
          THEN 1 ELSE 0
        END
      ) AS resolved_concepts,

      SUM(
        CASE
          WHEN final_resolution_stage = '04a'
          THEN 1 ELSE 0
        END
      ) AS resolved_in_04a,

      SUM(
        CASE
          WHEN final_resolution_stage = '04b'
          THEN 1 ELSE 0
        END
      ) AS recovered_in_04b,

      COUNT(
        DISTINCT final_wcvp_accepted_plant_name_id
      ) AS distinct_accepted_wcvp_concepts,

      SUM(
        CASE
          WHEN final_wcvp_accepted_plant_name_id IS NULL
          THEN 1 ELSE 0
        END
      ) AS unresolved_concepts

    FROM occurrence_wcvp_reconciliation_recovered
    "
  )
  
  # ---------------------------------------------------------------------------
  # Diagnose 04a rows that matched WCVP but lacked accepted ID
  # ---------------------------------------------------------------------------
  
  exact_but_unresolved <- DBI::dbGetQuery(
    con,
    "
    SELECT
      wcvp_match_status,
      COUNT(*) AS concepts
    FROM occurrence_wcvp_reconciliation
    WHERE wcvp_accepted_plant_name_id IS NULL
      AND wcvp_match_status IN (
        'exact_accepted',
        'exact_nonaccepted'
      )
    GROUP BY wcvp_match_status
    ORDER BY concepts DESC
    "
  )
  
  unresolved_by_original_status <- DBI::dbGetQuery(
    con,
    "
    SELECT
      wcvp_match_status,
      COUNT(*) AS concepts
    FROM occurrence_wcvp_reconciliation_recovered
    WHERE final_wcvp_accepted_plant_name_id IS NULL
    GROUP BY wcvp_match_status
    ORDER BY concepts DESC
    "
  )
  
  unresolved_by_rank <- DBI::dbGetQuery(
    con,
    "
    SELECT
      taxonRank,
      COUNT(*) AS concepts
    FROM occurrence_wcvp_reconciliation_recovered
    WHERE final_wcvp_accepted_plant_name_id IS NULL
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
    CREATE TABLE occurrence_wcvp_recovery_metadata AS
    SELECT
      CURRENT_TIMESTAMP AS created_at,
      '0.1.2'::VARCHAR AS script_version,

      COUNT(*) AS recovery_input_concepts,

      SUM(
        CASE
          WHEN recovery_status = 'recovered'
          THEN 1 ELSE 0
        END
      ) AS recovered_concepts,

      SUM(
        CASE
          WHEN recovery_status = 'ambiguous'
          THEN 1 ELSE 0
        END
      ) AS ambiguous_concepts,

      SUM(
        CASE
          WHEN recovery_status = 'unmatched'
          THEN 1 ELSE 0
        END
      ) AS unmatched_concepts

    FROM occurrence_wcvp_recovery
    "
  )
  
  # ---------------------------------------------------------------------------
  # Export
  # ---------------------------------------------------------------------------
  
  recovery_table <- DBI::dbGetQuery(
    con,
    "
    SELECT
      r.*,
      a.accepted_wcvp_taxon_name,
      a.accepted_wcvp_taxon_authors,
      a.accepted_wcvp_taxon_rank,
      a.accepted_wcvp_scientific_name

    FROM occurrence_wcvp_recovery r

    LEFT JOIN recovered_accepted_details a
      ON r.vpjd_taxon_universe_id =
         a.vpjd_taxon_universe_id

    ORDER BY
      r.recovery_status,
      r.scientificName,
      r.vpjd_taxon_universe_id
    "
  )
  
  readr::write_csv(
    recovery_table,
    file.path(
      output_dir,
      "occurrence_wcvp_recovery.csv"
    )
  )
  
  readr::write_csv(
    recovery_summary,
    file.path(
      output_dir,
      "occurrence_wcvp_recovery_summary.csv"
    )
  )
  
  readr::write_csv(
    stage_summary,
    file.path(
      output_dir,
      "occurrence_wcvp_recovery_stage_summary.csv"
    )
  )
  
  readr::write_csv(
    final_summary,
    file.path(
      output_dir,
      "occurrence_wcvp_reconciliation_recovered_summary.csv"
    )
  )
  
  readr::write_csv(
    exact_but_unresolved,
    file.path(
      output_dir,
      "occurrence_wcvp_04a_exact_but_unresolved.csv"
    )
  )
  
  readr::write_csv(
    unresolved_by_original_status,
    file.path(
      output_dir,
      "occurrence_wcvp_remaining_unresolved_by_04a_status.csv"
    )
  )
  
  readr::write_csv(
    unresolved_by_rank,
    file.path(
      output_dir,
      "occurrence_wcvp_remaining_unresolved_by_rank.csv"
    )
  )
  
  # ---------------------------------------------------------------------------
  # Console
  # ---------------------------------------------------------------------------
  
  get_status_n <- function(status) {
    
    x <- recovery_summary |>
      dplyr::filter(
        .data$recovery_status == status
      ) |>
      dplyr::pull(.data$concepts)
    
    if (length(x) == 0L) {
      return(0)
    }
    
    x[[1]]
  }
  
  recovered_n <- get_status_n("recovered")
  ambiguous_n <- get_status_n("ambiguous")
  unmatched_n <- get_status_n("unmatched")
  
  cli::cli_h2(
    "Occurrence-first WCVP deterministic recovery complete"
  )
  
  cli::cli_text(
    "{.strong Recovery input concepts:} ",
    "{fmt(unresolved_input_n)}"
  )
  
  cli::cli_text(
    "{.strong Deterministically recovered:} ",
    "{fmt(recovered_n)}"
  )
  
  cli::cli_text(
    "{.strong Ambiguous after recovery:} ",
    "{fmt(ambiguous_n)}"
  )
  
  cli::cli_text(
    "{.strong Unmatched after recovery:} ",
    "{fmt(unmatched_n)}"
  )
  
  cli::cli_text(
    "{.strong Total WCVP-resolved concepts after 04a + 04b:} ",
    "{fmt(final_summary$resolved_concepts[[1]])}"
  )
  
  cli::cli_text(
    "{.strong Distinct accepted WCVP concepts represented:} ",
    "{fmt(final_summary$distinct_accepted_wcvp_concepts[[1]])}"
  )
  
  cli::cli_text(
    "{.strong Remaining unresolved concepts overall:} ",
    "{fmt(final_summary$unresolved_concepts[[1]])}"
  )
  
  cli::cli_alert_success(
    "Existing 04a reconciliation and occurrence tables were not modified."
  )
  
  cli::cli_text(
    "04b_occurrence_wcvp_recovery.R v{VPJD_WCVP_RECOVERY_VERSION} complete."
  )
  
  invisible(
    list(
      baseline = baseline,
      recovery_summary = recovery_summary,
      stage_summary = stage_summary,
      final_summary = final_summary,
      exact_but_unresolved = exact_but_unresolved,
      unresolved_by_original_status =
        unresolved_by_original_status,
      unresolved_by_rank =
        unresolved_by_rank
    )
  )
}

wcvp_recovery_results <- run_occurrence_wcvp_recovery()

recovery_summary <-
  wcvp_recovery_results$recovery_summary

stage_summary <-
  wcvp_recovery_results$stage_summary

final_summary <-
  wcvp_recovery_results$final_summary

exact_but_unresolved <-
  wcvp_recovery_results$exact_but_unresolved

unresolved_by_original_status <-
  wcvp_recovery_results$unresolved_by_original_status

unresolved_by_rank <-
  wcvp_recovery_results$unresolved_by_rank