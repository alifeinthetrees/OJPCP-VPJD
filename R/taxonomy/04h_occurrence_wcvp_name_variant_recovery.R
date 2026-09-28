# ==============================================================================
# 04h_occurrence_wcvp_name_variant_recovery.R
# VPJD / Oxford-Japan Plant Conservation Partnership
#
# Purpose:
#   Conservative recovery of residual species concepts using tightly controlled
#   orthographic variants within the SAME genus.
#
# Input:
#   Frozen 04f reconciliation + validated 04g v0.2.0 diagnostics.
#
# Recovery rules:
#   H1  remove hyphen from epithet
#   H2  ae -> e
#   H3  ou -> oua
#   H4  oe -> e
#   H5  terminal ii -> i
#   H6  terminal i -> ii
#   H7  terminal yi -> i
#
# Safeguards:
#   - same genus required;
#   - transformed epithet must differ from original;
#   - exact transformed epithet match to WCVP;
#   - exactly one accepted WCVP concept across ALL matching rules;
#   - multiple rules may support a recovery only if they converge on the same
#     accepted WCVP concept;
#   - no fuzzy matching;
#   - no edit distance;
#   - no cross-genus matching;
#   - no family/genus tie-breaking;
#   - no arbitrary candidate selection;
#   - original and transformed names retained for audit;
#   - 04f and 04g are never modified.
#
# Version: 0.1.1
# ==============================================================================

VPJD_WCVP_VARIANT_VERSION <- "0.1.1"

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

run_occurrence_wcvp_name_variant_recovery <- function() {
  
  duckdb_path <- here::here(
    "data", "interim", "occurrences",
    "vpjd_occurrences.duckdb"
  )
  
  output_dir <- here::here(
    "outputs", "tables", "taxonomy",
    "occurrence_wcvp_name_variant_recovery"
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
    "occurrence_wcvp_reconciliation_species",
    "occurrence_wcvp_residual_diagnostics",
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
  
  fmt <- function(x) {
    format(
      x,
      big.mark = ",",
      scientific = FALSE,
      trim = TRUE
    )
  }
  
  cli::cli_h1(
    "VPJD hardened WCVP name-variant recovery"
  )
  
  # ---------------------------------------------------------------------------
  # Validate frozen 04f
  # ---------------------------------------------------------------------------
  
  required_04f_fields <- c(
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
    "species_final_wcvp_accepted_plant_name_id",
    "species_final_resolution_stage"
  )
  
  actual_04f_fields <- DBI::dbListFields(
    con,
    "occurrence_wcvp_reconciliation_species"
  )
  
  missing_04f_fields <- setdiff(
    required_04f_fields,
    actual_04f_fields
  )
  
  if (length(missing_04f_fields) > 0L) {
    stop(
      "04f reconciliation missing field(s): ",
      paste(missing_04f_fields, collapse = ", ")
    )
  }
  
  # ---------------------------------------------------------------------------
  # Secure 04f baseline
  # ---------------------------------------------------------------------------
  
  baseline <- DBI::dbGetQuery(
    con,
    "
    SELECT
      COUNT(*) AS total_concepts,

      SUM(CASE
        WHEN species_final_wcvp_accepted_plant_name_id IS NOT NULL
        THEN 1 ELSE 0
      END) AS resolved_before_04h,

      COUNT(
        DISTINCT species_final_wcvp_accepted_plant_name_id
      ) AS distinct_accepted_before_04h,

      SUM(CASE
        WHEN species_final_wcvp_accepted_plant_name_id IS NULL
        THEN 1 ELSE 0
      END) AS unresolved_before_04h

    FROM occurrence_wcvp_reconciliation_species
    "
  )
  
  cli::cli_text(
    "{.strong Secure resolved baseline:} ",
    "{fmt(baseline$resolved_before_04h[[1]])}"
  )
  
  cli::cli_text(
    "{.strong Secure distinct accepted concepts:} ",
    "{fmt(baseline$distinct_accepted_before_04h[[1]])}"
  )
  
  cli::cli_text(
    "{.strong Secure unresolved baseline:} ",
    "{fmt(baseline$unresolved_before_04h[[1]])}"
  )
  
  # ---------------------------------------------------------------------------
  # Remove 04h-owned products only
  # ---------------------------------------------------------------------------
  
  for (tbl in c(
    "occurrence_wcvp_name_variant_metadata",
    "occurrence_wcvp_reconciliation_name_variant",
    "occurrence_wcvp_name_variant_recovery",
    "occurrence_wcvp_name_variant_candidates"
  )) {
    DBI::dbExecute(
      con,
      paste0("DROP TABLE IF EXISTS ", tbl)
    )
  }
  
  # ---------------------------------------------------------------------------
  # Genuine residual species from 04g
  # ---------------------------------------------------------------------------
  
  DBI::dbExecute(
    con,
    "
    CREATE OR REPLACE TEMP TABLE variant_input AS

    SELECT
      r.vpjd_taxon_universe_id,
      r.taxonKey,
      r.speciesKey,
      r.family,
      r.genus,
      r.species,
      r.taxonRank,
      r.scientificName,
      r.total_records,
      r.present_records,
      d.residual_diagnostic_class,

      LOWER(
        TRIM(
          REGEXP_REPLACE(
            REPLACE(
              COALESCE(r.genus, ''),
              '×', ''
            ),
            '[^[:alpha:]-]', '',
            'g'
          )
        )
      ) AS norm_genus,

      LOWER(
        TRIM(
          REGEXP_REPLACE(
            REPLACE(
              COALESCE(
                SPLIT_PART(
                  r.species,
                  ' ',
                  2
                ),
                ''
              ),
              '×', ''
            ),
            '[^[:alpha:]-]', '',
            'g'
          )
        )
      ) AS norm_epithet

    FROM occurrence_wcvp_reconciliation_species r

    INNER JOIN occurrence_wcvp_residual_diagnostics d
      ON r.vpjd_taxon_universe_id =
         d.vpjd_taxon_universe_id

    WHERE r.species_final_wcvp_accepted_plant_name_id IS NULL
      AND UPPER(COALESCE(r.taxonRank, '')) = 'SPECIES'
    "
  )
  
  input_summary <- DBI::dbGetQuery(
    con,
    "
    SELECT
      COUNT(*) AS concepts,
      SUM(present_records) AS present_records
    FROM variant_input
    "
  )
  
  cli::cli_text(
    "{.strong Residual species entering 04h:} ",
    "{fmt(input_summary$concepts[[1]])}"
  )
  
  cli::cli_text(
    "{.strong PRESENT records represented:} ",
    "{fmt(input_summary$present_records[[1]])}"
  )
  
  # ---------------------------------------------------------------------------
  # Load WCVP
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
  
  wcvp_source <- wcvp_names[
    ,
    required_wcvp_fields,
    drop = FALSE
  ]
  
  DBI::dbWriteTable(
    con,
    "wcvp_04h_source",
    wcvp_source,
    temporary = TRUE,
    overwrite = TRUE
  )
  
  rm(wcvp_source, wcvp_names)
  gc()
  
  # ---------------------------------------------------------------------------
  # WCVP species index
  # ---------------------------------------------------------------------------
  
  DBI::dbExecute(
    con,
    "
    CREATE OR REPLACE TEMP TABLE wcvp_04h_species_index AS

    SELECT
      TRY_CAST(plant_name_id AS BIGINT)
        AS wcvp_plant_name_id,

      taxon_name
        AS wcvp_taxon_name,

      taxon_authors
        AS wcvp_taxon_authors,

      taxon_status
        AS wcvp_taxon_status,

      TRY_CAST(accepted_plant_name_id AS BIGINT)
        AS wcvp_accepted_plant_name_id,

      family
        AS wcvp_family,

      LOWER(
        TRIM(
          REGEXP_REPLACE(
            REPLACE(
              SPLIT_PART(
                COALESCE(taxon_name, ''),
                ' ',
                1
              ),
              '×', ''
            ),
            '[^[:alpha:]-]', '',
            'g'
          )
        )
      ) AS norm_wcvp_genus,

      LOWER(
        TRIM(
          REGEXP_REPLACE(
            REPLACE(
              SPLIT_PART(
                COALESCE(taxon_name, ''),
                ' ',
                2
              ),
              '×', ''
            ),
            '[^[:alpha:]-]', '',
            'g'
          )
        )
      ) AS norm_wcvp_epithet

    FROM wcvp_04h_source

    WHERE UPPER(COALESCE(taxon_rank, '')) = 'SPECIES'
      AND TRY_CAST(
        accepted_plant_name_id AS BIGINT
      ) IS NOT NULL
    "
  )
  
  # ---------------------------------------------------------------------------
  # Controlled variant generation
  #
  # Each rule changes only the explicitly defined sequence.
  # Original names are NOT included as recovery candidates.
  # ---------------------------------------------------------------------------
  
  DBI::dbExecute(
    con,
    "
    CREATE OR REPLACE TEMP TABLE gbif_name_variants AS

    SELECT
      vpjd_taxon_universe_id,
      'H1_REMOVE_HYPHEN'::VARCHAR
        AS variant_rule,
      norm_genus AS original_genus,
      norm_epithet AS original_epithet,
      norm_genus AS variant_genus,
      REPLACE(
        norm_epithet,
        '-',
        ''
      ) AS variant_epithet

    FROM variant_input

    WHERE norm_genus <> ''
      AND norm_epithet LIKE '%-%'

    UNION ALL

    SELECT
      vpjd_taxon_universe_id,
      'H2_AE_TO_E',
      norm_genus,
      norm_epithet,
      norm_genus,
      REPLACE(
        norm_epithet,
        'ae',
        'e'
      )

    FROM variant_input

    WHERE norm_genus <> ''
      AND norm_epithet LIKE '%ae%'

    UNION ALL

    SELECT
      vpjd_taxon_universe_id,
      'H3_OU_TO_OUA',
      norm_genus,
      norm_epithet,
      norm_genus,
      REPLACE(
        norm_epithet,
        'ou',
        'oua'
      )

    FROM variant_input

    WHERE norm_genus <> ''
      AND norm_epithet LIKE '%ou%'

    UNION ALL

    SELECT
      vpjd_taxon_universe_id,
      'H4_OE_TO_E',
      norm_genus,
      norm_epithet,
      norm_genus,
      REPLACE(
        norm_epithet,
        'oe',
        'e'
      )

    FROM variant_input

    WHERE norm_genus <> ''
      AND norm_epithet LIKE '%oe%'

    UNION ALL

    SELECT
      vpjd_taxon_universe_id,
      'H5_TERMINAL_II_TO_I',
      norm_genus,
      norm_epithet,
      norm_genus,
      REGEXP_REPLACE(
        norm_epithet,
        'ii$',
        'i'
      )

    FROM variant_input

    WHERE norm_genus <> ''
      AND norm_epithet LIKE '%ii'

    UNION ALL

    SELECT
      vpjd_taxon_universe_id,
      'H6_TERMINAL_I_TO_II',
      norm_genus,
      norm_epithet,
      norm_genus,
      REGEXP_REPLACE(
        norm_epithet,
        'i$',
        'ii'
      )

    FROM variant_input

    WHERE norm_genus <> ''
      AND norm_epithet LIKE '%i'
      AND norm_epithet NOT LIKE '%ii'

    UNION ALL

    SELECT
      vpjd_taxon_universe_id,
      'H7_TERMINAL_YI_TO_I',
      norm_genus,
      norm_epithet,
      norm_genus,
      REGEXP_REPLACE(
        norm_epithet,
        'yi$',
        'i'
      )

    FROM variant_input

    WHERE norm_genus <> ''
      AND norm_epithet LIKE '%yi'
    "
  )
  
  # ---------------------------------------------------------------------------
  # Remove no-op transformations
  # ---------------------------------------------------------------------------
  
  DBI::dbExecute(
    con,
    "
    CREATE OR REPLACE TEMP TABLE gbif_name_variants_valid AS

    SELECT DISTINCT
      *

    FROM gbif_name_variants

    WHERE original_genus = variant_genus
      AND original_epithet <> variant_epithet
      AND variant_genus <> ''
      AND variant_epithet <> ''
    "
  )
  
  # ---------------------------------------------------------------------------
  # Exact transformed-name matching within SAME genus
  # ---------------------------------------------------------------------------
  
  DBI::dbExecute(
    con,
    "
    CREATE TABLE occurrence_wcvp_name_variant_candidates AS

    SELECT DISTINCT
      g.vpjd_taxon_universe_id,
      g.variant_rule,

      g.original_genus,
      g.original_epithet,

      g.variant_genus,
      g.variant_epithet,

      g.original_genus || ' ' ||
        g.original_epithet
        AS original_normalised_binomial,

      g.variant_genus || ' ' ||
        g.variant_epithet
        AS transformed_normalised_binomial,

      w.wcvp_plant_name_id,
      w.wcvp_taxon_name,
      w.wcvp_taxon_authors,
      w.wcvp_taxon_status,
      w.wcvp_accepted_plant_name_id,
      w.wcvp_family

    FROM gbif_name_variants_valid g

    INNER JOIN wcvp_04h_species_index w
      ON g.variant_genus =
         w.norm_wcvp_genus

     AND g.variant_epithet =
         w.norm_wcvp_epithet
    "
  )
  
  # ---------------------------------------------------------------------------
  # Candidate summary per residual concept
  # ---------------------------------------------------------------------------
  
  DBI::dbExecute(
    con,
    "
    CREATE OR REPLACE TEMP TABLE variant_candidate_counts AS

    SELECT
      vpjd_taxon_universe_id,

      COUNT(
        DISTINCT wcvp_accepted_plant_name_id
      ) AS accepted_concept_count,

      MIN(
        wcvp_accepted_plant_name_id
      ) AS unique_accepted_concept_id,

      COUNT(
        DISTINCT variant_rule
      ) AS matching_rule_count,

      STRING_AGG(
        DISTINCT variant_rule,
        '; '
        ORDER BY variant_rule
      ) AS matching_rules,

      COUNT(
        DISTINCT transformed_normalised_binomial
      ) AS transformed_name_count,

      STRING_AGG(
        DISTINCT transformed_normalised_binomial,
        '; '
        ORDER BY transformed_normalised_binomial
      ) AS transformed_names

    FROM occurrence_wcvp_name_variant_candidates

    GROUP BY vpjd_taxon_universe_id
    "
  )
  
  # ---------------------------------------------------------------------------
  # Recovery decision
  #
  # Multiple rules are allowed only if ALL converge on one accepted concept.
  # ---------------------------------------------------------------------------
  
  DBI::dbExecute(
    con,
    "
    CREATE TABLE occurrence_wcvp_name_variant_recovery AS

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
      g.residual_diagnostic_class,

      g.norm_genus || ' ' ||
        g.norm_epithet
        AS original_normalised_binomial,

      COALESCE(
        c.accepted_concept_count,
        0
      ) AS accepted_concept_count,

      COALESCE(
        c.matching_rule_count,
        0
      ) AS matching_rule_count,

      COALESCE(
        c.transformed_name_count,
        0
      ) AS transformed_name_count,

      c.matching_rules,
      c.transformed_names,

      CASE
        WHEN c.accepted_concept_count = 1
        THEN 'recovered_unique_controlled_variant'

        WHEN c.accepted_concept_count > 1
        THEN 'ambiguous_controlled_variant'

        ELSE 'no_controlled_variant'
      END AS variant_recovery_status,

      CASE
        WHEN c.accepted_concept_count = 1
        THEN c.unique_accepted_concept_id

        ELSE CAST(NULL AS BIGINT)
      END AS variant_accepted_wcvp_plant_name_id

    FROM variant_input g

    LEFT JOIN variant_candidate_counts c
      ON g.vpjd_taxon_universe_id =
         c.vpjd_taxon_universe_id
    "
  )
  
  # ---------------------------------------------------------------------------
  # Safety audit: no accepted recovery may cross genus at matching stage
  # ---------------------------------------------------------------------------
  
  cross_genus_count <- DBI::dbGetQuery(
    con,
    "
    SELECT COUNT(*) AS n

    FROM occurrence_wcvp_name_variant_candidates

    WHERE original_genus <> variant_genus
    "
  )$n[[1]]
  
  if (cross_genus_count != 0) {
    stop(
      "Safety check failed: cross-genus 04h candidate detected."
    )
  }
  
  # ---------------------------------------------------------------------------
  # Safety audit: conflicting transformation routes
  # ---------------------------------------------------------------------------
  
  conflicting_routes <- DBI::dbGetQuery(
    con,
    "
    SELECT
      vpjd_taxon_universe_id,
      COUNT(
        DISTINCT wcvp_accepted_plant_name_id
      ) AS accepted_concepts

    FROM occurrence_wcvp_name_variant_candidates

    GROUP BY vpjd_taxon_universe_id

    HAVING COUNT(
      DISTINCT wcvp_accepted_plant_name_id
    ) > 1
    "
  )
  
  # ---------------------------------------------------------------------------
  # Consolidate with secure 04f baseline
  # ---------------------------------------------------------------------------
  
  DBI::dbExecute(
    con,
    "
    CREATE TABLE occurrence_wcvp_reconciliation_name_variant AS

    SELECT
      r.*,

      CASE
        WHEN r.species_final_wcvp_accepted_plant_name_id IS NOT NULL
        THEN TRY_CAST(
          r.species_final_wcvp_accepted_plant_name_id AS BIGINT
        )

        WHEN h.variant_accepted_wcvp_plant_name_id IS NOT NULL
        THEN h.variant_accepted_wcvp_plant_name_id

        ELSE CAST(NULL AS BIGINT)
      END AS name_variant_final_wcvp_accepted_plant_name_id,

      CASE
        WHEN r.species_final_wcvp_accepted_plant_name_id IS NOT NULL
        THEN r.species_final_resolution_stage

        WHEN h.variant_accepted_wcvp_plant_name_id IS NOT NULL
        THEN '04h'

        ELSE NULL
      END AS name_variant_final_resolution_stage,

      h.variant_recovery_status,
      h.matching_rules,
      h.transformed_names,
      h.variant_accepted_wcvp_plant_name_id

    FROM occurrence_wcvp_reconciliation_species r

    LEFT JOIN occurrence_wcvp_name_variant_recovery h
      ON r.vpjd_taxon_universe_id =
         h.vpjd_taxon_universe_id
    "
  )
  
  # ---------------------------------------------------------------------------
  # Recovery summary
  # ---------------------------------------------------------------------------
  
  recovery_summary <- DBI::dbGetQuery(
    con,
    "
    SELECT
      variant_recovery_status,
      COUNT(*) AS concepts,
      SUM(present_records) AS present_records

    FROM occurrence_wcvp_name_variant_recovery

    GROUP BY variant_recovery_status

    ORDER BY present_records DESC
    "
  )
  
  # ---------------------------------------------------------------------------
  # Rule summary
  # ---------------------------------------------------------------------------
  
  rule_summary <- DBI::dbGetQuery(
    con,
    "
    SELECT
      variant_rule,

      COUNT(
        DISTINCT vpjd_taxon_universe_id
      ) AS concepts,

      COUNT(*) AS candidate_rows,

      COUNT(
        DISTINCT wcvp_accepted_plant_name_id
      ) AS accepted_concepts

    FROM occurrence_wcvp_name_variant_candidates

    GROUP BY variant_rule

    ORDER BY
      concepts DESC,
      variant_rule
    "
  )
  
  # ---------------------------------------------------------------------------
  # Final summary
  # ---------------------------------------------------------------------------
  
  final_summary <- DBI::dbGetQuery(
    con,
    "
    SELECT
      COUNT(*) AS total_concepts,

      SUM(CASE
        WHEN name_variant_final_wcvp_accepted_plant_name_id
             IS NOT NULL
        THEN 1 ELSE 0
      END) AS resolved_concepts,

      SUM(CASE
        WHEN name_variant_final_resolution_stage = '04h'
        THEN 1 ELSE 0
      END) AS resolved_in_04h,

      COUNT(
        DISTINCT name_variant_final_wcvp_accepted_plant_name_id
      ) AS distinct_accepted_wcvp_concepts,

      SUM(CASE
        WHEN name_variant_final_wcvp_accepted_plant_name_id
             IS NULL
        THEN 1 ELSE 0
      END) AS unresolved_concepts

    FROM occurrence_wcvp_reconciliation_name_variant
    "
  )
  
  # ---------------------------------------------------------------------------
  # Detailed recovered audit
  # ---------------------------------------------------------------------------
  
  recovered_audit <- DBI::dbGetQuery(
    con,
    "
    SELECT
      h.vpjd_taxon_universe_id,
      h.taxonKey,
      h.scientificName,
      h.family,
      h.genus,
      h.species,
      h.present_records,

      h.original_normalised_binomial,
      h.matching_rules,
      h.transformed_names,

      c.variant_rule,
      c.original_epithet,
      c.variant_epithet,
      c.transformed_normalised_binomial,

      c.wcvp_plant_name_id,
      c.wcvp_taxon_name,
      c.wcvp_taxon_authors,
      c.wcvp_taxon_status,
      c.wcvp_accepted_plant_name_id,

      a.accepted_wcvp_taxon_name,
      a.accepted_wcvp_taxon_authors,
      a.accepted_wcvp_taxon_rank,
      a.accepted_wcvp_scientific_name

    FROM occurrence_wcvp_name_variant_recovery h

    INNER JOIN occurrence_wcvp_name_variant_candidates c
      ON h.vpjd_taxon_universe_id =
         c.vpjd_taxon_universe_id

    LEFT JOIN wcvp_occurrence_accepted_lookup a
      ON c.wcvp_accepted_plant_name_id =
         TRY_CAST(
           a.accepted_wcvp_plant_name_id AS BIGINT
         )

    WHERE h.variant_accepted_wcvp_plant_name_id IS NOT NULL

    ORDER BY
      h.present_records DESC,
      h.scientificName,
      c.variant_rule
    "
  )
  
  # ---------------------------------------------------------------------------
  # Remaining residual species
  # ---------------------------------------------------------------------------
  
  remaining_species <- DBI::dbGetQuery(
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
      residual_diagnostic_class,
      variant_recovery_status,
      accepted_concept_count,
      matching_rules,
      transformed_names

    FROM occurrence_wcvp_name_variant_recovery

    WHERE variant_accepted_wcvp_plant_name_id IS NULL

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
    CREATE TABLE occurrence_wcvp_name_variant_metadata AS

    SELECT
      CURRENT_TIMESTAMP AS created_at,
      '0.1.1'::VARCHAR AS script_version,

      COUNT(*) AS input_species_concepts,
      SUM(present_records) AS input_present_records,

      SUM(CASE
        WHEN variant_accepted_wcvp_plant_name_id IS NOT NULL
        THEN 1 ELSE 0
      END) AS recovered_species_concepts,

      SUM(CASE
        WHEN variant_accepted_wcvp_plant_name_id IS NOT NULL
        THEN present_records ELSE 0
      END) AS recovered_present_records,

      SUM(CASE
        WHEN variant_recovery_status =
             'ambiguous_controlled_variant'
        THEN 1 ELSE 0
      END) AS ambiguous_species_concepts

    FROM occurrence_wcvp_name_variant_recovery
    "
  )
  
  # ---------------------------------------------------------------------------
  # Exports
  # ---------------------------------------------------------------------------
  
  full_recovery <- DBI::dbGetQuery(
    con,
    "
    SELECT *
    FROM occurrence_wcvp_name_variant_recovery
    ORDER BY present_records DESC, scientificName
    "
  )
  
  exports <- list(
    occurrence_wcvp_name_variant_recovery =
      full_recovery,
    
    name_variant_recovery_summary =
      recovery_summary,
    
    name_variant_rule_summary =
      rule_summary,
    
    name_variant_recovered_audit =
      recovered_audit,
    
    name_variant_conflicting_routes =
      conflicting_routes,
    
    name_variant_remaining_species =
      remaining_species,
    
    name_variant_final_summary =
      final_summary
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
  
  recovered_records <- sum(
    recovery_summary$present_records[
      recovery_summary$variant_recovery_status ==
        "recovered_unique_controlled_variant"
    ],
    na.rm = TRUE
  )
  
  cli::cli_h2(
    "Hardened WCVP name-variant recovery complete"
  )
  
  cli::cli_text(
    "{.strong Input residual species concepts:} ",
    "{fmt(input_summary$concepts[[1]])}"
  )
  
  cli::cli_text(
    "{.strong Input PRESENT records:} ",
    "{fmt(input_summary$present_records[[1]])}"
  )
  
  cli::cli_text(
    "{.strong Recovered in 04h:} ",
    "{fmt(final_summary$resolved_in_04h[[1]])}"
  )
  
  cli::cli_text(
    "{.strong PRESENT records represented by recoveries:} ",
    "{fmt(recovered_records)}"
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
  
  cli::cli_text(
    "{.strong Conflicting transformation routes:} ",
    "{fmt(nrow(conflicting_routes))}"
  )
  
  cli::cli_alert_success(
    "Frozen 04f and validated 04g tables were not modified."
  )
  
  cli::cli_alert_success(
    "All candidate matching remained within the same genus."
  )
  
  cli::cli_alert_success(
    "No fuzzy or edit-distance matching was used."
  )
  
  cli::cli_text(
    "04h_occurrence_wcvp_name_variant_recovery.R ",
    "v{VPJD_WCVP_VARIANT_VERSION} complete."
  )
  
  invisible(
    list(
      baseline = baseline,
      input_summary = input_summary,
      recovery_summary = recovery_summary,
      rule_summary = rule_summary,
      final_summary = final_summary,
      recovered_audit = recovered_audit,
      conflicting_routes = conflicting_routes,
      remaining_species = remaining_species
    )
  )
}

wcvp_name_variant_results <-
  run_occurrence_wcvp_name_variant_recovery()

name_variant_input_summary <-
  wcvp_name_variant_results$input_summary

name_variant_recovery_summary <-
  wcvp_name_variant_results$recovery_summary

name_variant_rule_summary <-
  wcvp_name_variant_results$rule_summary

name_variant_final_summary <-
  wcvp_name_variant_results$final_summary

name_variant_recovered_audit <-
  wcvp_name_variant_results$recovered_audit

name_variant_conflicting_routes <-
  wcvp_name_variant_results$conflicting_routes

name_variant_remaining_species <-
  wcvp_name_variant_results$remaining_species