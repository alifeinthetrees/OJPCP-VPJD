# ==============================================================================
# 04f_occurrence_wcvp_species_recovery.R
# VPJD / Oxford-Japan Plant Conservation Partnership
#
# Purpose:
#   Deterministic recovery of species-rank concepts remaining unresolved after
#   04a + 04b + 04d + 04e.
#
# Strategy:
#   F0 - Exact normalised full scientificName, including authorship.
#   F1 - Exact normalised scientific-name binomial.
#   F2 - Exact normalised GBIF species field.
#
# Resolution rules within the selected stage:
#   1. If all matched WCVP names resolve to exactly one accepted concept:
#      recover that concept.
#   2. If multiple accepted concepts occur, but exactly one matched WCVP
#      nomenclatural record has taxon_status == "Accepted":
#      recover the accepted concept represented by that Accepted record.
#   3. Otherwise remain ambiguous.
#
# Important:
# - Starts from validated 04e reconciliation.
# - Previous 04f outputs are recalculated, not inherited.
# - Later stages are attempted only when the preceding stage has zero candidates.
# - Genus/family context is diagnostic only.
# - No fuzzy matching.
# - No edit-distance matching.
# - No arbitrary homonym selection.
#
# Version: 0.1.3
# ==============================================================================

VPJD_WCVP_SPECIES_VERSION <- "0.1.3"

required_packages <- c(
  "here", "DBI", "duckdb", "readr", "dplyr", "cli", "rWCVPdata"
)

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

run_occurrence_wcvp_species_recovery <- function() {
  
  duckdb_path <- here::here(
    "data", "interim", "occurrences",
    "vpjd_occurrences.duckdb"
  )
  
  output_dir <- here::here(
    "outputs", "tables", "taxonomy",
    "occurrence_wcvp_species_recovery"
  )
  
  dir.create(
    output_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
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
    "occurrence_wcvp_reconciliation_infraspecific",
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
    "VPJD accepted-status species-level WCVP recovery"
  )
  
  # ---------------------------------------------------------------------------
  # Validate secure 04e input
  # ---------------------------------------------------------------------------
  
  required_recon_fields <- c(
    "vpjd_taxon_universe_id",
    "taxonKey",
    "speciesKey",
    "family",
    "genus",
    "species",
    "taxonRank",
    "scientificName",
    "total_records",
    "present_records",
    "infra_final_wcvp_accepted_plant_name_id",
    "infra_final_resolution_stage"
  )
  
  actual_recon_fields <- DBI::dbListFields(
    con,
    "occurrence_wcvp_reconciliation_infraspecific"
  )
  
  missing_recon_fields <- setdiff(
    required_recon_fields,
    actual_recon_fields
  )
  
  if (length(missing_recon_fields) > 0L) {
    stop(
      "04e reconciliation missing field(s): ",
      paste(missing_recon_fields, collapse = ", ")
    )
  }
  
  # ---------------------------------------------------------------------------
  # Secure 04e baseline
  # ---------------------------------------------------------------------------
  
  baseline <- DBI::dbGetQuery(
    con,
    "
    SELECT
      COUNT(*) AS total_concepts,
      SUM(CASE
        WHEN infra_final_wcvp_accepted_plant_name_id IS NOT NULL
        THEN 1 ELSE 0
      END) AS resolved_before_04f,
      COUNT(DISTINCT infra_final_wcvp_accepted_plant_name_id)
        AS distinct_accepted_before_04f,
      SUM(CASE
        WHEN infra_final_wcvp_accepted_plant_name_id IS NULL
        THEN 1 ELSE 0
      END) AS unresolved_before_04f
    FROM occurrence_wcvp_reconciliation_infraspecific
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
    "{.strong Secure 04e resolved baseline:} ",
    "{fmt(baseline$resolved_before_04f[[1]])}"
  )
  
  cli::cli_text(
    "{.strong Secure 04e unresolved baseline:} ",
    "{fmt(baseline$unresolved_before_04f[[1]])}"
  )
  
  # ---------------------------------------------------------------------------
  # Remove previous 04f products
  # ---------------------------------------------------------------------------
  
  for (tbl in c(
    "occurrence_wcvp_species_recovery_metadata",
    "occurrence_wcvp_reconciliation_species",
    "occurrence_wcvp_species_recovery",
    "occurrence_wcvp_species_candidates"
  )) {
    DBI::dbExecute(
      con,
      paste0("DROP TABLE IF EXISTS ", tbl)
    )
  }
  
  # ---------------------------------------------------------------------------
  # Species-only unresolved input
  # ---------------------------------------------------------------------------
  
  DBI::dbExecute(
    con,
    "
    CREATE OR REPLACE TEMP TABLE species_input AS
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

      LOWER(
        TRIM(
          REGEXP_REPLACE(
            REGEXP_REPLACE(
              REPLACE(
                REPLACE(
                  REPLACE(
                    COALESCE(scientificName, ''),
                    '×', 'x'
                  ),
                  ',', ''
                ),
                '.', ''
              ),
              '\\s+', ' ', 'g'
            ),
            '\\s*([()&])\\s*', '\\1', 'g'
          )
        )
      ) AS norm_full_name,

      LOWER(
        TRIM(
          REGEXP_REPLACE(
            REPLACE(
              COALESCE(
                REGEXP_EXTRACT(
                  COALESCE(scientificName, ''),
                  '^([^ ]+\\s+[^ ]+)',
                  1
                ),
                ''
              ),
              '×', 'x'
            ),
            '\\s+', ' ', 'g'
          )
        )
      ) AS norm_binomial,

      LOWER(
        TRIM(
          REGEXP_REPLACE(
            REPLACE(COALESCE(species, ''), '×', 'x'),
            '\\s+', ' ', 'g'
          )
        )
      ) AS norm_species,

      LOWER(
        TRIM(
          REGEXP_REPLACE(
            REPLACE(COALESCE(genus, ''), '×', 'x'),
            '\\s+', ' ', 'g'
          )
        )
      ) AS norm_genus,

      LOWER(
        TRIM(
          REGEXP_REPLACE(
            REPLACE(COALESCE(family, ''), '×', 'x'),
            '\\s+', ' ', 'g'
          )
        )
      ) AS norm_family

    FROM occurrence_wcvp_reconciliation_infraspecific

    WHERE infra_final_wcvp_accepted_plant_name_id IS NULL
      AND UPPER(COALESCE(taxonRank, '')) = 'SPECIES'
    "
  )
  
  input_summary <- DBI::dbGetQuery(
    con,
    "
    SELECT
      COUNT(*) AS concepts,
      SUM(present_records) AS present_records
    FROM species_input
    "
  )
  
  cli::cli_text(
    "{.strong Species concepts entering 04f:} ",
    "{fmt(input_summary$concepts[[1]])}"
  )
  
  # ---------------------------------------------------------------------------
  # Load WCVP species names
  # ---------------------------------------------------------------------------
  
  wcvp_names <- rWCVPdata::wcvp_names
  
  required_wcvp_fields <- c(
    "plant_name_id",
    "taxon_name",
    "taxon_authors",
    "taxon_rank",
    "taxon_status",
    "accepted_plant_name_id",
    "family"
  )
  
  missing_wcvp_fields <- setdiff(
    required_wcvp_fields,
    names(wcvp_names)
  )
  
  if (length(missing_wcvp_fields) > 0L) {
    stop(
      "rWCVPdata::wcvp_names missing field(s): ",
      paste(missing_wcvp_fields, collapse = ", ")
    )
  }
  
  wcvp_species_source <- wcvp_names[
    ,
    required_wcvp_fields,
    drop = FALSE
  ]
  
  DBI::dbWriteTable(
    con,
    "wcvp_04f_species_source",
    wcvp_species_source,
    temporary = TRUE,
    overwrite = TRUE
  )
  
  rm(wcvp_species_source, wcvp_names)
  gc()
  
  # ---------------------------------------------------------------------------
  # WCVP species nomenclatural index
  # ---------------------------------------------------------------------------
  
  DBI::dbExecute(
    con,
    "
    CREATE OR REPLACE TEMP TABLE wcvp_04f_species_index AS
    SELECT
      TRY_CAST(plant_name_id AS BIGINT)
        AS wcvp_plant_name_id,
      taxon_name
        AS wcvp_taxon_name,
      taxon_authors
        AS wcvp_taxon_authors,
      UPPER(COALESCE(taxon_rank, ''))
        AS wcvp_taxon_rank,
      taxon_status
        AS wcvp_taxon_status,
      TRY_CAST(accepted_plant_name_id AS BIGINT)
        AS wcvp_accepted_plant_name_id,
      family
        AS wcvp_family,

      LOWER(
        TRIM(
          REGEXP_REPLACE(
            REGEXP_REPLACE(
              REPLACE(
                REPLACE(
                  REPLACE(
                    TRIM(
                      COALESCE(taxon_name, '') ||
                      CASE
                        WHEN COALESCE(taxon_authors, '') <> ''
                        THEN ' ' || taxon_authors
                        ELSE ''
                      END
                    ),
                    '×', 'x'
                  ),
                  ',', ''
                ),
                '.', ''
              ),
              '\\s+', ' ', 'g'
            ),
            '\\s*([()&])\\s*', '\\1', 'g'
          )
        )
      ) AS norm_full_name,

      LOWER(
        TRIM(
          REGEXP_REPLACE(
            REPLACE(COALESCE(taxon_name, ''), '×', 'x'),
            '\\s+', ' ', 'g'
          )
        )
      ) AS norm_taxon_name,

      LOWER(
        SPLIT_PART(
          TRIM(COALESCE(taxon_name, '')),
          ' ',
          1
        )
      ) AS norm_wcvp_genus,

      LOWER(
        TRIM(COALESCE(family, ''))
      ) AS norm_wcvp_family

    FROM wcvp_04f_species_source

    WHERE UPPER(COALESCE(taxon_rank, '')) = 'SPECIES'
      AND TRY_CAST(accepted_plant_name_id AS BIGINT) IS NOT NULL
    "
  )
  
  # ---------------------------------------------------------------------------
  # F0 candidates
  # ---------------------------------------------------------------------------
  
  DBI::dbExecute(
    con,
    "
    CREATE OR REPLACE TEMP TABLE species_f0_candidates AS
    SELECT DISTINCT
      g.vpjd_taxon_universe_id,
      g.taxonKey,
      g.speciesKey,
      g.family,
      g.genus,
      g.species,
      g.scientificName,
      g.present_records,
      'F0_FULL_NAME_AUTHORSHIP'::VARCHAR AS recovery_stage,
      0::INTEGER AS stage_priority,
      w.wcvp_plant_name_id,
      w.wcvp_taxon_name,
      w.wcvp_taxon_authors,
      w.wcvp_taxon_status,
      w.wcvp_accepted_plant_name_id,
      w.wcvp_family,

      CASE
        WHEN g.norm_genus <> ''
         AND g.norm_genus = w.norm_wcvp_genus
        THEN TRUE ELSE FALSE
      END AS genus_context_agrees,

      CASE
        WHEN g.norm_family <> ''
         AND w.norm_wcvp_family <> ''
         AND g.norm_family = w.norm_wcvp_family
        THEN TRUE ELSE FALSE
      END AS family_context_agrees

    FROM species_input g

    INNER JOIN wcvp_04f_species_index w
      ON g.norm_full_name = w.norm_full_name

    WHERE g.norm_full_name <> ''
    "
  )
  
  # ---------------------------------------------------------------------------
  # Candidate-set summary helper
  #
  # accepted_status_count counts nomenclatural records explicitly marked
  # Accepted, not the number of accepted concepts represented by all synonyms.
  # ---------------------------------------------------------------------------
  
  make_counts <- function(candidate_table, output_table) {
    
    sql <- sprintf(
      "
      CREATE OR REPLACE TEMP TABLE %s AS
      SELECT
        vpjd_taxon_universe_id,

        COUNT(DISTINCT wcvp_plant_name_id)
          AS matched_wcvp_name_count,

        COUNT(DISTINCT wcvp_accepted_plant_name_id)
          AS accepted_concept_count,

        MIN(wcvp_accepted_plant_name_id)
          AS unique_concept_id,

        COUNT(DISTINCT CASE
          WHEN UPPER(TRIM(COALESCE(wcvp_taxon_status, ''))) = 'ACCEPTED'
          THEN wcvp_plant_name_id
          ELSE NULL
        END) AS accepted_status_count,

        MIN(CASE
          WHEN UPPER(TRIM(COALESCE(wcvp_taxon_status, ''))) = 'ACCEPTED'
          THEN wcvp_accepted_plant_name_id
          ELSE NULL
        END) AS accepted_status_concept_id,

        MIN(CASE
          WHEN UPPER(TRIM(COALESCE(wcvp_taxon_status, ''))) = 'ACCEPTED'
          THEN wcvp_plant_name_id
          ELSE NULL
        END) AS accepted_status_plant_name_id

      FROM %s

      GROUP BY vpjd_taxon_universe_id
      ",
      output_table,
      candidate_table
    )
    
    DBI::dbExecute(con, sql)
  }
  
  make_counts(
    "species_f0_candidates",
    "species_f0_counts"
  )
  
  # ---------------------------------------------------------------------------
  # F1 candidates - only where F0 had zero candidates
  # ---------------------------------------------------------------------------
  
  DBI::dbExecute(
    con,
    "
    CREATE OR REPLACE TEMP TABLE species_f1_candidates AS
    SELECT DISTINCT
      g.vpjd_taxon_universe_id,
      g.taxonKey,
      g.speciesKey,
      g.family,
      g.genus,
      g.species,
      g.scientificName,
      g.present_records,
      'F1_SCIENTIFIC_BINOMIAL'::VARCHAR AS recovery_stage,
      1::INTEGER AS stage_priority,
      w.wcvp_plant_name_id,
      w.wcvp_taxon_name,
      w.wcvp_taxon_authors,
      w.wcvp_taxon_status,
      w.wcvp_accepted_plant_name_id,
      w.wcvp_family,

      CASE
        WHEN g.norm_genus <> ''
         AND g.norm_genus = w.norm_wcvp_genus
        THEN TRUE ELSE FALSE
      END AS genus_context_agrees,

      CASE
        WHEN g.norm_family <> ''
         AND w.norm_wcvp_family <> ''
         AND g.norm_family = w.norm_wcvp_family
        THEN TRUE ELSE FALSE
      END AS family_context_agrees

    FROM species_input g

    LEFT JOIN species_f0_counts f0
      ON g.vpjd_taxon_universe_id =
         f0.vpjd_taxon_universe_id

    INNER JOIN wcvp_04f_species_index w
      ON g.norm_binomial = w.norm_taxon_name

    WHERE g.norm_binomial <> ''
      AND f0.vpjd_taxon_universe_id IS NULL
    "
  )
  
  make_counts(
    "species_f1_candidates",
    "species_f1_counts"
  )
  
  # ---------------------------------------------------------------------------
  # F2 candidates - only where F0 and F1 had zero candidates
  # ---------------------------------------------------------------------------
  
  DBI::dbExecute(
    con,
    "
    CREATE OR REPLACE TEMP TABLE species_f2_candidates AS
    SELECT DISTINCT
      g.vpjd_taxon_universe_id,
      g.taxonKey,
      g.speciesKey,
      g.family,
      g.genus,
      g.species,
      g.scientificName,
      g.present_records,
      'F2_SPECIES_FIELD'::VARCHAR AS recovery_stage,
      2::INTEGER AS stage_priority,
      w.wcvp_plant_name_id,
      w.wcvp_taxon_name,
      w.wcvp_taxon_authors,
      w.wcvp_taxon_status,
      w.wcvp_accepted_plant_name_id,
      w.wcvp_family,

      CASE
        WHEN g.norm_genus <> ''
         AND g.norm_genus = w.norm_wcvp_genus
        THEN TRUE ELSE FALSE
      END AS genus_context_agrees,

      CASE
        WHEN g.norm_family <> ''
         AND w.norm_wcvp_family <> ''
         AND g.norm_family = w.norm_wcvp_family
        THEN TRUE ELSE FALSE
      END AS family_context_agrees

    FROM species_input g

    LEFT JOIN species_f0_counts f0
      ON g.vpjd_taxon_universe_id =
         f0.vpjd_taxon_universe_id

    LEFT JOIN species_f1_counts f1
      ON g.vpjd_taxon_universe_id =
         f1.vpjd_taxon_universe_id

    INNER JOIN wcvp_04f_species_index w
      ON g.norm_species = w.norm_taxon_name

    WHERE g.norm_species <> ''
      AND f0.vpjd_taxon_universe_id IS NULL
      AND f1.vpjd_taxon_universe_id IS NULL
    "
  )
  
  make_counts(
    "species_f2_candidates",
    "species_f2_counts"
  )
  
  # ---------------------------------------------------------------------------
  # Persistent candidate table
  # ---------------------------------------------------------------------------
  
  DBI::dbExecute(
    con,
    "
    CREATE TABLE occurrence_wcvp_species_candidates AS
    SELECT * FROM species_f0_candidates
    UNION ALL
    SELECT * FROM species_f1_candidates
    UNION ALL
    SELECT * FROM species_f2_candidates
    "
  )
  
  # ---------------------------------------------------------------------------
  # Selected-stage evidence
  # ---------------------------------------------------------------------------
  
  DBI::dbExecute(
    con,
    "
    CREATE OR REPLACE TEMP TABLE species_selected_evidence AS
    SELECT
      g.vpjd_taxon_universe_id,

      CASE
        WHEN f0.vpjd_taxon_universe_id IS NOT NULL
        THEN 'F0_FULL_NAME_AUTHORSHIP'
        WHEN f1.vpjd_taxon_universe_id IS NOT NULL
        THEN 'F1_SCIENTIFIC_BINOMIAL'
        WHEN f2.vpjd_taxon_universe_id IS NOT NULL
        THEN 'F2_SPECIES_FIELD'
        ELSE NULL
      END AS recovery_stage,

      COALESCE(
        f0.matched_wcvp_name_count,
        f1.matched_wcvp_name_count,
        f2.matched_wcvp_name_count,
        0
      ) AS matched_wcvp_name_count,

      COALESCE(
        f0.accepted_concept_count,
        f1.accepted_concept_count,
        f2.accepted_concept_count,
        0
      ) AS accepted_concept_count,

      COALESCE(
        f0.accepted_status_count,
        f1.accepted_status_count,
        f2.accepted_status_count,
        0
      ) AS accepted_status_count,

      COALESCE(
        f0.unique_concept_id,
        f1.unique_concept_id,
        f2.unique_concept_id
      ) AS unique_concept_id,

      COALESCE(
        f0.accepted_status_concept_id,
        f1.accepted_status_concept_id,
        f2.accepted_status_concept_id
      ) AS accepted_status_concept_id,

      COALESCE(
        f0.accepted_status_plant_name_id,
        f1.accepted_status_plant_name_id,
        f2.accepted_status_plant_name_id
      ) AS accepted_status_plant_name_id

    FROM species_input g

    LEFT JOIN species_f0_counts f0
      ON g.vpjd_taxon_universe_id =
         f0.vpjd_taxon_universe_id

    LEFT JOIN species_f1_counts f1
      ON g.vpjd_taxon_universe_id =
         f1.vpjd_taxon_universe_id

    LEFT JOIN species_f2_counts f2
      ON g.vpjd_taxon_universe_id =
         f2.vpjd_taxon_universe_id
    "
  )
  
  # ---------------------------------------------------------------------------
  # Strict recovery decision
  # ---------------------------------------------------------------------------
  
  DBI::dbExecute(
    con,
    "
    CREATE TABLE occurrence_wcvp_species_recovery AS
    SELECT
      g.vpjd_taxon_universe_id,
      g.taxonKey,
      g.speciesKey,
      g.family,
      g.genus,
      g.species,
      g.taxonRank,
      g.scientificName,
      g.total_records,
      g.present_records,
      e.recovery_stage,
      e.matched_wcvp_name_count,
      e.accepted_concept_count,
      e.accepted_status_count,
      e.accepted_status_plant_name_id,

      CASE
        WHEN e.recovery_stage IS NULL
        THEN 'unmatched'

        WHEN e.accepted_concept_count = 1
        THEN CASE e.recovery_stage
          WHEN 'F0_FULL_NAME_AUTHORSHIP'
          THEN 'recovered_unique_f0'
          WHEN 'F1_SCIENTIFIC_BINOMIAL'
          THEN 'recovered_unique_f1'
          WHEN 'F2_SPECIES_FIELD'
          THEN 'recovered_unique_f2'
        END

        WHEN e.accepted_concept_count > 1
         AND e.accepted_status_count = 1
        THEN CASE e.recovery_stage
          WHEN 'F0_FULL_NAME_AUTHORSHIP'
          THEN 'recovered_accepted_status_f0'
          WHEN 'F1_SCIENTIFIC_BINOMIAL'
          THEN 'recovered_accepted_status_f1'
          WHEN 'F2_SPECIES_FIELD'
          THEN 'recovered_accepted_status_f2'
        END

        ELSE CASE e.recovery_stage
          WHEN 'F0_FULL_NAME_AUTHORSHIP'
          THEN 'ambiguous_f0'
          WHEN 'F1_SCIENTIFIC_BINOMIAL'
          THEN 'ambiguous_f1'
          WHEN 'F2_SPECIES_FIELD'
          THEN 'ambiguous_f2'
        END
      END AS species_recovery_status,

      CASE
        WHEN e.accepted_concept_count = 1
        THEN e.unique_concept_id

        WHEN e.accepted_concept_count > 1
         AND e.accepted_status_count = 1
        THEN e.accepted_status_concept_id

        ELSE CAST(NULL AS BIGINT)
      END AS species_accepted_wcvp_plant_name_id

    FROM species_input g

    LEFT JOIN species_selected_evidence e
      ON g.vpjd_taxon_universe_id =
         e.vpjd_taxon_universe_id
    "
  )
  
  # ---------------------------------------------------------------------------
  # Consolidate from secure 04e baseline
  # ---------------------------------------------------------------------------
  
  DBI::dbExecute(
    con,
    "
    CREATE TABLE occurrence_wcvp_reconciliation_species AS
    SELECT
      r.*,

      CASE
        WHEN r.infra_final_wcvp_accepted_plant_name_id IS NOT NULL
        THEN TRY_CAST(
          r.infra_final_wcvp_accepted_plant_name_id AS BIGINT
        )

        WHEN f.species_accepted_wcvp_plant_name_id IS NOT NULL
        THEN f.species_accepted_wcvp_plant_name_id

        ELSE CAST(NULL AS BIGINT)
      END AS species_final_wcvp_accepted_plant_name_id,

      CASE
        WHEN r.infra_final_wcvp_accepted_plant_name_id IS NOT NULL
        THEN r.infra_final_resolution_stage

        WHEN f.species_accepted_wcvp_plant_name_id IS NOT NULL
        THEN '04f'

        ELSE NULL
      END AS species_final_resolution_stage,

      f.species_recovery_status,
      f.recovery_stage AS species_recovery_stage,
      f.matched_wcvp_name_count
        AS species_matched_wcvp_name_count,
      f.accepted_concept_count
        AS species_accepted_concept_count,
      f.accepted_status_count
        AS species_accepted_status_count,
      f.accepted_status_plant_name_id
        AS species_accepted_status_plant_name_id,
      f.species_accepted_wcvp_plant_name_id

    FROM occurrence_wcvp_reconciliation_infraspecific r

    LEFT JOIN occurrence_wcvp_species_recovery f
      ON r.vpjd_taxon_universe_id =
         f.vpjd_taxon_universe_id
    "
  )
  
  # ---------------------------------------------------------------------------
  # Summaries
  # ---------------------------------------------------------------------------
  
  recovery_summary <- DBI::dbGetQuery(
    con,
    "
    SELECT
      species_recovery_status,
      COUNT(*) AS concepts,
      SUM(present_records) AS present_records
    FROM occurrence_wcvp_species_recovery
    GROUP BY species_recovery_status
    ORDER BY concepts DESC
    "
  )
  
  stage_summary <- DBI::dbGetQuery(
    con,
    "
    SELECT
      recovery_stage,
      species_recovery_status,
      COUNT(*) AS concepts,
      SUM(present_records) AS present_records
    FROM occurrence_wcvp_species_recovery
    GROUP BY recovery_stage, species_recovery_status
    ORDER BY recovery_stage, species_recovery_status
    "
  )
  
  final_summary <- DBI::dbGetQuery(
    con,
    "
    SELECT
      COUNT(*) AS total_concepts,

      SUM(CASE
        WHEN species_final_wcvp_accepted_plant_name_id IS NOT NULL
        THEN 1 ELSE 0
      END) AS resolved_concepts,

      SUM(CASE
        WHEN species_final_resolution_stage = '04a'
        THEN 1 ELSE 0
      END) AS resolved_in_04a,

      SUM(CASE
        WHEN species_final_resolution_stage = '04b'
        THEN 1 ELSE 0
      END) AS resolved_in_04b,

      SUM(CASE
        WHEN species_final_resolution_stage = '04d'
        THEN 1 ELSE 0
      END) AS resolved_in_04d,

      SUM(CASE
        WHEN species_final_resolution_stage = '04e'
        THEN 1 ELSE 0
      END) AS resolved_in_04e,

      SUM(CASE
        WHEN species_final_resolution_stage = '04f'
        THEN 1 ELSE 0
      END) AS resolved_in_04f,

      COUNT(
        DISTINCT species_final_wcvp_accepted_plant_name_id
      ) AS distinct_accepted_wcvp_concepts,

      SUM(CASE
        WHEN species_final_wcvp_accepted_plant_name_id IS NULL
        THEN 1 ELSE 0
      END) AS unresolved_concepts

    FROM occurrence_wcvp_reconciliation_species
    "
  )
  
  # ---------------------------------------------------------------------------
  # Accepted-status recoveries specifically
  # ---------------------------------------------------------------------------
  
  accepted_status_recoveries <- DBI::dbGetQuery(
    con,
    "
    SELECT
      f.vpjd_taxon_universe_id,
      f.taxonKey,
      f.speciesKey,
      f.scientificName,
      f.family,
      f.genus,
      f.species,
      f.present_records,
      f.recovery_stage,
      f.species_recovery_status,
      f.accepted_status_plant_name_id,
      f.species_accepted_wcvp_plant_name_id,

      a.accepted_wcvp_taxon_name,
      a.accepted_wcvp_taxon_authors,
      a.accepted_wcvp_taxon_rank,
      a.accepted_wcvp_scientific_name

    FROM occurrence_wcvp_species_recovery f

    LEFT JOIN wcvp_occurrence_accepted_lookup a
      ON f.species_accepted_wcvp_plant_name_id =
         TRY_CAST(
           a.accepted_wcvp_plant_name_id AS BIGINT
         )

    WHERE f.species_recovery_status LIKE
      'recovered_accepted_status_%'

    ORDER BY
      f.present_records DESC,
      f.scientificName
    "
  )
  
  # ---------------------------------------------------------------------------
  # All recovered concepts
  # ---------------------------------------------------------------------------
  
  recovered_examples <- DBI::dbGetQuery(
    con,
    "
    SELECT
      f.vpjd_taxon_universe_id,
      f.taxonKey,
      f.speciesKey,
      f.scientificName,
      f.family,
      f.genus,
      f.species,
      f.present_records,
      f.recovery_stage,
      f.species_recovery_status,
      f.accepted_concept_count,
      f.accepted_status_count,
      f.species_accepted_wcvp_plant_name_id,

      a.accepted_wcvp_taxon_name,
      a.accepted_wcvp_taxon_authors,
      a.accepted_wcvp_taxon_rank,
      a.accepted_wcvp_scientific_name

    FROM occurrence_wcvp_species_recovery f

    LEFT JOIN wcvp_occurrence_accepted_lookup a
      ON f.species_accepted_wcvp_plant_name_id =
         TRY_CAST(
           a.accepted_wcvp_plant_name_id AS BIGINT
         )

    WHERE f.species_accepted_wcvp_plant_name_id IS NOT NULL

    ORDER BY
      f.present_records DESC,
      f.scientificName
    "
  )
  
  # ---------------------------------------------------------------------------
  # Remaining ambiguous species
  # ---------------------------------------------------------------------------
  
  ambiguous_summary <- DBI::dbGetQuery(
    con,
    "
    SELECT
      vpjd_taxon_universe_id,
      taxonKey,
      speciesKey,
      scientificName,
      family,
      genus,
      species,
      present_records,
      recovery_stage,
      matched_wcvp_name_count,
      accepted_concept_count,
      accepted_status_count,
      species_recovery_status

    FROM occurrence_wcvp_species_recovery

    WHERE species_recovery_status LIKE 'ambiguous_%'

    ORDER BY
      present_records DESC,
      scientificName
    "
  )
  
  # ---------------------------------------------------------------------------
  # Candidate records for remaining ambiguous species
  # ---------------------------------------------------------------------------
  
  ambiguous_candidates <- DBI::dbGetQuery(
    con,
    "
    SELECT
      r.vpjd_taxon_universe_id,
      r.taxonKey,
      r.speciesKey,
      r.scientificName,
      r.family AS gbif_family,
      r.genus AS gbif_genus,
      r.species AS gbif_species,
      r.present_records,
      r.species_recovery_status,
      r.accepted_status_count,

      c.recovery_stage,
      c.wcvp_plant_name_id,
      c.wcvp_taxon_name,
      c.wcvp_taxon_authors,
      c.wcvp_taxon_status,
      c.wcvp_accepted_plant_name_id,
      c.wcvp_family,
      c.genus_context_agrees,
      c.family_context_agrees,

      a.accepted_wcvp_taxon_name,
      a.accepted_wcvp_taxon_authors,
      a.accepted_wcvp_taxon_rank,
      a.accepted_wcvp_scientific_name

    FROM occurrence_wcvp_species_recovery r

    INNER JOIN occurrence_wcvp_species_candidates c
      ON r.vpjd_taxon_universe_id =
         c.vpjd_taxon_universe_id
     AND r.recovery_stage =
         c.recovery_stage

    LEFT JOIN wcvp_occurrence_accepted_lookup a
      ON c.wcvp_accepted_plant_name_id =
         TRY_CAST(
           a.accepted_wcvp_plant_name_id AS BIGINT
         )

    WHERE r.species_recovery_status LIKE 'ambiguous_%'

    ORDER BY
      r.present_records DESC,
      r.scientificName,
      c.wcvp_accepted_plant_name_id,
      c.wcvp_plant_name_id
    "
  )
  
  # ---------------------------------------------------------------------------
  # Unmatched species
  # ---------------------------------------------------------------------------
  
  unmatched_species <- DBI::dbGetQuery(
    con,
    "
    SELECT
      vpjd_taxon_universe_id,
      taxonKey,
      speciesKey,
      scientificName,
      family,
      genus,
      species,
      present_records,
      species_recovery_status

    FROM occurrence_wcvp_species_recovery

    WHERE species_recovery_status = 'unmatched'

    ORDER BY
      present_records DESC,
      scientificName
    "
  )
  
  # ---------------------------------------------------------------------------
  # Overall residual by rank
  # ---------------------------------------------------------------------------
  
  remaining_by_rank <- DBI::dbGetQuery(
    con,
    "
    SELECT
      taxonRank,
      COUNT(*) AS concepts,
      SUM(present_records) AS present_records

    FROM occurrence_wcvp_reconciliation_species

    WHERE species_final_wcvp_accepted_plant_name_id IS NULL

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
    CREATE TABLE occurrence_wcvp_species_recovery_metadata AS
    SELECT
      CURRENT_TIMESTAMP AS created_at,
      '0.1.3'::VARCHAR AS script_version,
      COUNT(*) AS input_species_concepts,

      SUM(CASE
        WHEN species_accepted_wcvp_plant_name_id IS NOT NULL
        THEN 1 ELSE 0
      END) AS recovered_species_concepts,

      SUM(CASE
        WHEN species_recovery_status LIKE
          'recovered_accepted_status_%'
        THEN 1 ELSE 0
      END) AS recovered_by_accepted_status,

      SUM(CASE
        WHEN species_recovery_status LIKE 'ambiguous_%'
        THEN 1 ELSE 0
      END) AS ambiguous_species_concepts,

      SUM(CASE
        WHEN species_recovery_status = 'unmatched'
        THEN 1 ELSE 0
      END) AS unmatched_species_concepts

    FROM occurrence_wcvp_species_recovery
    "
  )
  
  # ---------------------------------------------------------------------------
  # Export
  # ---------------------------------------------------------------------------
  
  full_recovery <- DBI::dbGetQuery(
    con,
    "
    SELECT *
    FROM occurrence_wcvp_species_recovery
    ORDER BY present_records DESC, scientificName
    "
  )
  
  exports <- list(
    occurrence_wcvp_species_recovery = full_recovery,
    species_recovery_summary = recovery_summary,
    species_recovery_stage_summary = stage_summary,
    species_final_summary = final_summary,
    species_accepted_status_recoveries = accepted_status_recoveries,
    species_recovered_examples = recovered_examples,
    species_ambiguous_summary = ambiguous_summary,
    species_ambiguous_candidates = ambiguous_candidates,
    species_unmatched = unmatched_species,
    remaining_unresolved_by_rank = remaining_by_rank
  )
  
  for (nm in names(exports)) {
    readr::write_csv(
      exports[[nm]],
      file.path(
        output_dir,
        paste0(nm, ".csv")
      )
    )
  }
  
  # ---------------------------------------------------------------------------
  # Console
  # ---------------------------------------------------------------------------
  
  cli::cli_h2(
    "Accepted-status species-level WCVP recovery complete"
  )
  
  recovered_n <- sum(
    recovery_summary$concepts[
      grepl(
        "^recovered_",
        recovery_summary$species_recovery_status
      )
    ],
    na.rm = TRUE
  )
  
  accepted_status_n <- sum(
    recovery_summary$concepts[
      grepl(
        "^recovered_accepted_status_",
        recovery_summary$species_recovery_status
      )
    ],
    na.rm = TRUE
  )
  
  ambiguous_n <- sum(
    recovery_summary$concepts[
      grepl(
        "^ambiguous_",
        recovery_summary$species_recovery_status
      )
    ],
    na.rm = TRUE
  )
  
  cli::cli_text(
    "{.strong 04f input species concepts:} ",
    "{fmt(input_summary$concepts[[1]])}"
  )
  
  cli::cli_text(
    "{.strong Total recovered in 04f:} ",
    "{fmt(recovered_n)}"
  )
  
  cli::cli_text(
    "{.strong Recovered by unique Accepted status:} ",
    "{fmt(accepted_status_n)}"
  )
  
  cli::cli_text(
    "{.strong Remaining ambiguous species:} ",
    "{fmt(ambiguous_n)}"
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
    "Resolution was recalculated from the secure 04e baseline."
  )
  
  cli::cli_alert_success(
    "Unique Accepted-status selection was used only within exact-name candidate sets."
  )
  
  cli::cli_alert_success(
    "Genus/family context remained diagnostic only."
  )
  
  cli::cli_alert_success(
    "No fuzzy or edit-distance matching was used."
  )
  
  cli::cli_text(
    "04f_occurrence_wcvp_species_recovery.R ",
    "v{VPJD_WCVP_SPECIES_VERSION} complete."
  )
  
  invisible(
    list(
      baseline = baseline,
      input_summary = input_summary,
      recovery_summary = recovery_summary,
      stage_summary = stage_summary,
      final_summary = final_summary,
      accepted_status_recoveries = accepted_status_recoveries,
      recovered_examples = recovered_examples,
      ambiguous_summary = ambiguous_summary,
      ambiguous_candidates = ambiguous_candidates,
      unmatched_species = unmatched_species,
      remaining_by_rank = remaining_by_rank
    )
  )
}

wcvp_species_results <-
  run_occurrence_wcvp_species_recovery()

species_input_summary <-
  wcvp_species_results$input_summary

species_recovery_summary <-
  wcvp_species_results$recovery_summary

species_stage_summary <-
  wcvp_species_results$stage_summary

species_final_summary <-
  wcvp_species_results$final_summary

species_accepted_status_recoveries <-
  wcvp_species_results$accepted_status_recoveries

species_recovered_examples <-
  wcvp_species_results$recovered_examples

species_ambiguous <-
  wcvp_species_results$ambiguous_summary

species_ambiguous_candidates <-
  wcvp_species_results$ambiguous_candidates

species_unmatched <-
  wcvp_species_results$unmatched_species

species_remaining_by_rank <-
  wcvp_species_results$remaining_by_rank