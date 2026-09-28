# ==============================================================================
# 04j_occurrence_wcvp_nomenclatural_diagnostics.R
# VPJD / Oxford-Japan Plant Conservation Partnership
#
# Purpose:
#   Diagnostic investigation of unresolved species names after 04i.
#
# Targets:
#   1. GBIF cross-genus alternative combinations not matched exactly in 04i.
#   2. GBIF same-genus/different-name combinations.
#
# Diagnostic routes:
#   J1 - exact WCVP match to GBIF species field
#   J2 - controlled adjectival gender-ending variants:
#        -us / -a / -um
#   J3 - controlled terminal -is / -e variants
#
# Safeguards:
#   - DIAGNOSTIC ONLY: no accepted-WCVP assignments;
#   - no fuzzy matching;
#   - no edit distance;
#   - no unrestricted epithet search;
#   - genus is fixed to the GBIF species-field genus;
#   - transformations alter terminal grammatical endings only;
#   - candidate convergence is reported, not interpreted as resolution;
#   - validated/frozen 04h and 04i products are not modified.
#
# Version: 0.1.0
# ==============================================================================

VPJD_WCVP_NOM_DIAG_VERSION <- "0.1.0"

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

run_occurrence_wcvp_nomenclatural_diagnostics <- function() {
  
  duckdb_path <- here::here(
    "data", "interim", "occurrences",
    "vpjd_occurrences.duckdb"
  )
  
  output_dir <- here::here(
    "outputs", "tables", "taxonomy",
    "occurrence_wcvp_nomenclatural_diagnostics"
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
    "occurrence_wcvp_reconciliation_alternative_combination",
    "occurrence_wcvp_alternative_combination_recovery",
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
    "VPJD WCVP nomenclatural diagnostics"
  )
  
  # ---------------------------------------------------------------------------
  # Secure 04i baseline
  # ---------------------------------------------------------------------------
  
  baseline <- DBI::dbGetQuery(
    con,
    "
    SELECT
      COUNT(*) AS total_concepts,

      SUM(CASE
        WHEN alternative_final_wcvp_accepted_plant_name_id IS NOT NULL
        THEN 1 ELSE 0
      END) AS resolved_before_04j,

      COUNT(
        DISTINCT alternative_final_wcvp_accepted_plant_name_id
      ) AS distinct_accepted_before_04j,

      SUM(CASE
        WHEN alternative_final_wcvp_accepted_plant_name_id IS NULL
        THEN 1 ELSE 0
      END) AS unresolved_before_04j

    FROM occurrence_wcvp_reconciliation_alternative_combination
    "
  )
  
  cli::cli_text(
    "{.strong Secure resolved baseline:} ",
    "{fmt(baseline$resolved_before_04j[[1]])}"
  )
  
  cli::cli_text(
    "{.strong Secure distinct accepted concepts:} ",
    "{fmt(baseline$distinct_accepted_before_04j[[1]])}"
  )
  
  cli::cli_text(
    "{.strong Secure unresolved baseline:} ",
    "{fmt(baseline$unresolved_before_04j[[1]])}"
  )
  
  # ---------------------------------------------------------------------------
  # Remove 04j-owned products only
  # ---------------------------------------------------------------------------
  
  for (tbl in c(
    "occurrence_wcvp_nomenclatural_diagnostic_metadata",
    "occurrence_wcvp_nomenclatural_diagnostics",
    "occurrence_wcvp_nomenclatural_candidates"
  )) {
    DBI::dbExecute(
      con,
      paste0("DROP TABLE IF EXISTS ", tbl)
    )
  }
  
  # ---------------------------------------------------------------------------
  # Diagnostic input
  #
  # Only unresolved species for which GBIF species differs from the original
  # scientific-name combination.
  # ---------------------------------------------------------------------------
  
  DBI::dbExecute(
    con,
    "
    CREATE OR REPLACE TEMP TABLE nom_input AS

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

      i.scientific_binomial,
      i.gbif_species_binomial,
      i.scientific_genus,
      i.alternative_genus,
      i.alternative_input_class,

      LOWER(
        SPLIT_PART(
          COALESCE(i.gbif_species_binomial, ''),
          ' ',
          1
        )
      ) AS candidate_genus,

      LOWER(
        SPLIT_PART(
          COALESCE(i.gbif_species_binomial, ''),
          ' ',
          2
        )
      ) AS candidate_epithet

    FROM occurrence_wcvp_reconciliation_alternative_combination r

    INNER JOIN occurrence_wcvp_alternative_combination_recovery i
      ON r.vpjd_taxon_universe_id =
         i.vpjd_taxon_universe_id

    WHERE r.alternative_final_wcvp_accepted_plant_name_id IS NULL
      AND UPPER(COALESCE(r.taxonRank, '')) = 'SPECIES'
      AND i.alternative_input_class IN (
        'alternative_genus_combination',
        'same_genus_different_name'
      )
    "
  )
  
  input_summary <- DBI::dbGetQuery(
    con,
    "
    SELECT
      alternative_input_class,
      COUNT(*) AS concepts,
      SUM(present_records) AS present_records

    FROM nom_input

    GROUP BY alternative_input_class

    ORDER BY present_records DESC
    "
  )
  
  cli::cli_text(
    "{.strong Diagnostic concepts:} ",
    "{fmt(sum(input_summary$concepts))}"
  )
  
  cli::cli_text(
    "{.strong PRESENT records represented:} ",
    "{fmt(sum(input_summary$present_records))}"
  )
  
  # ---------------------------------------------------------------------------
  # Load WCVP species nomenclatural records
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
    "wcvp_04j_source",
    wcvp_source,
    temporary = TRUE,
    overwrite = TRUE
  )
  
  rm(wcvp_source, wcvp_names)
  gc()
  
  DBI::dbExecute(
    con,
    "
    CREATE OR REPLACE TEMP TABLE wcvp_04j_species_index AS

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
            SPLIT_PART(
              COALESCE(taxon_name, ''),
              ' ',
              1
            ),
            '[^[:alpha:]-]',
            '',
            'g'
          )
        )
      ) AS norm_genus,

      LOWER(
        TRIM(
          REGEXP_REPLACE(
            SPLIT_PART(
              COALESCE(taxon_name, ''),
              ' ',
              2
            ),
            '[^[:alpha:]-]',
            '',
            'g'
          )
        )
      ) AS norm_epithet

    FROM wcvp_04j_source

    WHERE UPPER(COALESCE(taxon_rank, '')) = 'SPECIES'
      AND TRY_CAST(
        accepted_plant_name_id AS BIGINT
      ) IS NOT NULL
    "
  )
  
  # ---------------------------------------------------------------------------
  # Generate controlled candidate names
  #
  # J1: unchanged GBIF species-field binomial
  #
  # J2: grammatical -us / -a / -um alternatives.
  #     Example:
  #       decurrentialatum -> decurrentialata
  #
  # J3: terminal -is / -e alternative.
  #
  # No other spelling changes are generated.
  # ---------------------------------------------------------------------------
  
  DBI::dbExecute(
    con,
    "
    CREATE OR REPLACE TEMP TABLE nom_variants AS

    SELECT
      vpjd_taxon_universe_id,
      'J1_EXACT_GBIF_SPECIES'::VARCHAR AS diagnostic_rule,
      candidate_genus,
      candidate_epithet AS original_epithet,
      candidate_epithet AS candidate_variant_epithet

    FROM nom_input

    WHERE candidate_genus <> ''
      AND candidate_epithet <> ''

    UNION ALL

    SELECT
      vpjd_taxon_universe_id,
      'J2_US_TO_A',
      candidate_genus,
      candidate_epithet,
      REGEXP_REPLACE(
        candidate_epithet,
        'us$',
        'a'
      )

    FROM nom_input

    WHERE candidate_genus <> ''
      AND candidate_epithet LIKE '%us'

    UNION ALL

    SELECT
      vpjd_taxon_universe_id,
      'J2_US_TO_UM',
      candidate_genus,
      candidate_epithet,
      REGEXP_REPLACE(
        candidate_epithet,
        'us$',
        'um'
      )

    FROM nom_input

    WHERE candidate_genus <> ''
      AND candidate_epithet LIKE '%us'

    UNION ALL

    SELECT
      vpjd_taxon_universe_id,
      'J2_A_TO_US',
      candidate_genus,
      candidate_epithet,
      REGEXP_REPLACE(
        candidate_epithet,
        'a$',
        'us'
      )

    FROM nom_input

    WHERE candidate_genus <> ''
      AND candidate_epithet LIKE '%a'

    UNION ALL

    SELECT
      vpjd_taxon_universe_id,
      'J2_A_TO_UM',
      candidate_genus,
      candidate_epithet,
      REGEXP_REPLACE(
        candidate_epithet,
        'a$',
        'um'
      )

    FROM nom_input

    WHERE candidate_genus <> ''
      AND candidate_epithet LIKE '%a'

    UNION ALL

    SELECT
      vpjd_taxon_universe_id,
      'J2_UM_TO_US',
      candidate_genus,
      candidate_epithet,
      REGEXP_REPLACE(
        candidate_epithet,
        'um$',
        'us'
      )

    FROM nom_input

    WHERE candidate_genus <> ''
      AND candidate_epithet LIKE '%um'

    UNION ALL

    SELECT
      vpjd_taxon_universe_id,
      'J2_UM_TO_A',
      candidate_genus,
      candidate_epithet,
      REGEXP_REPLACE(
        candidate_epithet,
        'um$',
        'a'
      )

    FROM nom_input

    WHERE candidate_genus <> ''
      AND candidate_epithet LIKE '%um'

    UNION ALL

    SELECT
      vpjd_taxon_universe_id,
      'J3_IS_TO_E',
      candidate_genus,
      candidate_epithet,
      REGEXP_REPLACE(
        candidate_epithet,
        'is$',
        'e'
      )

    FROM nom_input

    WHERE candidate_genus <> ''
      AND candidate_epithet LIKE '%is'

    UNION ALL

    SELECT
      vpjd_taxon_universe_id,
      'J3_E_TO_IS',
      candidate_genus,
      candidate_epithet,
      REGEXP_REPLACE(
        candidate_epithet,
        'e$',
        'is'
      )

    FROM nom_input

    WHERE candidate_genus <> ''
      AND candidate_epithet LIKE '%e'
    "
  )
  
  # ---------------------------------------------------------------------------
  # Remove no-op / duplicate generated variants
  # ---------------------------------------------------------------------------
  
  DBI::dbExecute(
    con,
    "
    CREATE OR REPLACE TEMP TABLE nom_variants_valid AS

    SELECT DISTINCT
      vpjd_taxon_universe_id,
      diagnostic_rule,
      candidate_genus,
      original_epithet,
      candidate_variant_epithet,

      candidate_genus || ' ' ||
        candidate_variant_epithet
        AS candidate_binomial

    FROM nom_variants

    WHERE candidate_genus <> ''
      AND candidate_variant_epithet <> ''
    "
  )
  
  # ---------------------------------------------------------------------------
  # Exact matching of controlled candidate names against WCVP
  # ---------------------------------------------------------------------------
  
  DBI::dbExecute(
    con,
    "
    CREATE TABLE occurrence_wcvp_nomenclatural_candidates AS

    SELECT DISTINCT
      v.vpjd_taxon_universe_id,
      v.diagnostic_rule,
      v.candidate_genus,
      v.original_epithet,
      v.candidate_variant_epithet,
      v.candidate_binomial,

      w.wcvp_plant_name_id,
      w.wcvp_taxon_name,
      w.wcvp_taxon_authors,
      w.wcvp_taxon_status,
      w.wcvp_accepted_plant_name_id,
      w.wcvp_family

    FROM nom_variants_valid v

    INNER JOIN wcvp_04j_species_index w
      ON v.candidate_genus =
         w.norm_genus

     AND v.candidate_variant_epithet =
         w.norm_epithet
    "
  )
  
  # ---------------------------------------------------------------------------
  # Candidate convergence per GBIF concept
  # ---------------------------------------------------------------------------
  
  DBI::dbExecute(
    con,
    "
    CREATE OR REPLACE TEMP TABLE nom_candidate_summary AS

    SELECT
      vpjd_taxon_universe_id,

      COUNT(*) AS candidate_rows,

      COUNT(
        DISTINCT wcvp_plant_name_id
      ) AS nomenclatural_name_count,

      COUNT(
        DISTINCT wcvp_accepted_plant_name_id
      ) AS accepted_concept_count,

      MIN(
        wcvp_accepted_plant_name_id
      ) AS candidate_accepted_concept_id,

      COUNT(
        DISTINCT diagnostic_rule
      ) AS supporting_rule_count,

      STRING_AGG(
        DISTINCT diagnostic_rule,
        '; '
        ORDER BY diagnostic_rule
      ) AS supporting_rules,

      STRING_AGG(
        DISTINCT candidate_binomial,
        '; '
        ORDER BY candidate_binomial
      ) AS candidate_binomials

    FROM occurrence_wcvp_nomenclatural_candidates

    GROUP BY vpjd_taxon_universe_id
    "
  )
  
  # ---------------------------------------------------------------------------
  # Diagnostic classifications
  #
  # IMPORTANT: no accepted concept is assigned to the VPJD reconciliation.
  # candidate_accepted_concept_id is evidence only.
  # ---------------------------------------------------------------------------
  
  DBI::dbExecute(
    con,
    "
    CREATE TABLE occurrence_wcvp_nomenclatural_diagnostics AS

    SELECT
      i.vpjd_taxon_universe_id,
      i.taxonKey,
      i.speciesKey,
      i.family,
      i.genus,
      i.species,
      i.taxonRank,
      i.scientificName,
      i.total_records,
      i.present_records,

      i.scientific_binomial,
      i.gbif_species_binomial,
      i.alternative_input_class,

      COALESCE(
        c.candidate_rows,
        0
      ) AS candidate_rows,

      COALESCE(
        c.nomenclatural_name_count,
        0
      ) AS nomenclatural_name_count,

      COALESCE(
        c.accepted_concept_count,
        0
      ) AS accepted_concept_count,

      c.candidate_accepted_concept_id,
      c.supporting_rule_count,
      c.supporting_rules,
      c.candidate_binomials,

      CASE
        WHEN c.accepted_concept_count IS NULL
          OR c.accepted_concept_count = 0
        THEN 'no_wcvp_evidence'

        WHEN c.accepted_concept_count = 1
          AND c.supporting_rules LIKE '%J1_EXACT_GBIF_SPECIES%'
        THEN 'unique_exact_gbif_species_evidence'

        WHEN c.accepted_concept_count = 1
          AND c.supporting_rules LIKE '%J2_%'
        THEN 'unique_gender_variant_evidence'

        WHEN c.accepted_concept_count = 1
          AND c.supporting_rules LIKE '%J3_%'
        THEN 'unique_is_e_variant_evidence'

        WHEN c.accepted_concept_count = 1
        THEN 'unique_controlled_nomenclatural_evidence'

        ELSE 'multiple_wcvp_concepts'
      END AS nomenclatural_diagnostic_class

    FROM nom_input i

    LEFT JOIN nom_candidate_summary c
      ON i.vpjd_taxon_universe_id =
         c.vpjd_taxon_universe_id
    "
  )
  
  # ---------------------------------------------------------------------------
  # Safety check: 04i baseline must remain unchanged
  # ---------------------------------------------------------------------------
  
  baseline_after <- DBI::dbGetQuery(
    con,
    "
    SELECT
      SUM(CASE
        WHEN alternative_final_wcvp_accepted_plant_name_id IS NOT NULL
        THEN 1 ELSE 0
      END) AS resolved_after_diagnostics,

      COUNT(
        DISTINCT alternative_final_wcvp_accepted_plant_name_id
      ) AS distinct_accepted_after_diagnostics

    FROM occurrence_wcvp_reconciliation_alternative_combination
    "
  )
  
  if (
    baseline_after$resolved_after_diagnostics[[1]] !=
    baseline$resolved_before_04j[[1]] ||
    baseline_after$distinct_accepted_after_diagnostics[[1]] !=
    baseline$distinct_accepted_before_04j[[1]]
  ) {
    stop(
      "Safety check failed: 04j changed the secure 04i baseline."
    )
  }
  
  # ---------------------------------------------------------------------------
  # Summary
  # ---------------------------------------------------------------------------
  
  diagnostic_summary <- DBI::dbGetQuery(
    con,
    "
    SELECT
      nomenclatural_diagnostic_class,
      COUNT(*) AS concepts,
      SUM(present_records) AS present_records

    FROM occurrence_wcvp_nomenclatural_diagnostics

    GROUP BY nomenclatural_diagnostic_class

    ORDER BY present_records DESC
    "
  )
  
  class_by_input <- DBI::dbGetQuery(
    con,
    "
    SELECT
      alternative_input_class,
      nomenclatural_diagnostic_class,
      COUNT(*) AS concepts,
      SUM(present_records) AS present_records

    FROM occurrence_wcvp_nomenclatural_diagnostics

    GROUP BY
      alternative_input_class,
      nomenclatural_diagnostic_class

    ORDER BY
      alternative_input_class,
      present_records DESC
    "
  )
  
  # ---------------------------------------------------------------------------
  # Candidate detail
  # ---------------------------------------------------------------------------
  
  candidate_detail <- DBI::dbGetQuery(
    con,
    "
    SELECT
      d.vpjd_taxon_universe_id,
      d.taxonKey,
      d.scientificName,
      d.family,
      d.genus,
      d.species,
      d.present_records,
      d.scientific_binomial,
      d.gbif_species_binomial,
      d.alternative_input_class,
      d.nomenclatural_diagnostic_class,

      c.diagnostic_rule,
      c.original_epithet,
      c.candidate_variant_epithet,
      c.candidate_binomial,

      c.wcvp_plant_name_id,
      c.wcvp_taxon_name,
      c.wcvp_taxon_authors,
      c.wcvp_taxon_status,
      c.wcvp_accepted_plant_name_id,

      a.accepted_wcvp_taxon_name,
      a.accepted_wcvp_taxon_authors,
      a.accepted_wcvp_taxon_rank,
      a.accepted_wcvp_scientific_name

    FROM occurrence_wcvp_nomenclatural_diagnostics d

    INNER JOIN occurrence_wcvp_nomenclatural_candidates c
      ON d.vpjd_taxon_universe_id =
         c.vpjd_taxon_universe_id

    LEFT JOIN wcvp_occurrence_accepted_lookup a
      ON c.wcvp_accepted_plant_name_id =
         TRY_CAST(
           a.accepted_wcvp_plant_name_id AS BIGINT
         )

    ORDER BY
      d.present_records DESC,
      d.scientificName,
      c.diagnostic_rule,
      c.wcvp_taxon_name
    "
  )
  
  # ---------------------------------------------------------------------------
  # Unique evidence concepts
  # ---------------------------------------------------------------------------
  
  unique_evidence <- DBI::dbGetQuery(
    con,
    "
    SELECT
      d.vpjd_taxon_universe_id,
      d.taxonKey,
      d.scientificName,
      d.family,
      d.genus,
      d.species,
      d.present_records,
      d.scientific_binomial,
      d.gbif_species_binomial,
      d.alternative_input_class,
      d.nomenclatural_diagnostic_class,
      d.supporting_rules,
      d.candidate_binomials,
      d.candidate_accepted_concept_id,

      a.accepted_wcvp_taxon_name,
      a.accepted_wcvp_taxon_authors,
      a.accepted_wcvp_taxon_rank,
      a.accepted_wcvp_scientific_name

    FROM occurrence_wcvp_nomenclatural_diagnostics d

    LEFT JOIN wcvp_occurrence_accepted_lookup a
      ON d.candidate_accepted_concept_id =
         TRY_CAST(
           a.accepted_wcvp_plant_name_id AS BIGINT
         )

    WHERE d.accepted_concept_count = 1

    ORDER BY
      d.present_records DESC,
      d.scientificName
    "
  )
  
  # ---------------------------------------------------------------------------
  # No-evidence priority list
  # ---------------------------------------------------------------------------
  
  no_evidence <- DBI::dbGetQuery(
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
      scientific_binomial,
      gbif_species_binomial,
      alternative_input_class

    FROM occurrence_wcvp_nomenclatural_diagnostics

    WHERE nomenclatural_diagnostic_class =
          'no_wcvp_evidence'

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
    CREATE TABLE occurrence_wcvp_nomenclatural_diagnostic_metadata AS

    SELECT
      CURRENT_TIMESTAMP AS created_at,
      '0.1.0'::VARCHAR AS script_version,

      COUNT(*) AS diagnostic_concepts,
      SUM(present_records) AS diagnostic_present_records,

      SUM(CASE
        WHEN accepted_concept_count = 1
        THEN 1 ELSE 0
      END) AS unique_evidence_concepts,

      SUM(CASE
        WHEN accepted_concept_count = 1
        THEN present_records ELSE 0
      END) AS unique_evidence_present_records,

      SUM(CASE
        WHEN nomenclatural_diagnostic_class =
             'no_wcvp_evidence'
        THEN 1 ELSE 0
      END) AS no_evidence_concepts

    FROM occurrence_wcvp_nomenclatural_diagnostics
    "
  )
  
  # ---------------------------------------------------------------------------
  # Exports
  # ---------------------------------------------------------------------------
  
  exports <- list(
    nomenclatural_input_summary =
      input_summary,
    
    nomenclatural_diagnostic_summary =
      diagnostic_summary,
    
    nomenclatural_class_by_input =
      class_by_input,
    
    nomenclatural_unique_evidence =
      unique_evidence,
    
    nomenclatural_candidate_detail =
      candidate_detail,
    
    nomenclatural_no_evidence =
      no_evidence
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
  
  unique_n <- sum(
    diagnostic_summary$concepts[
      diagnostic_summary$nomenclatural_diagnostic_class !=
        "no_wcvp_evidence" &
        diagnostic_summary$nomenclatural_diagnostic_class !=
        "multiple_wcvp_concepts"
    ],
    na.rm = TRUE
  )
  
  unique_records <- sum(
    diagnostic_summary$present_records[
      diagnostic_summary$nomenclatural_diagnostic_class !=
        "no_wcvp_evidence" &
        diagnostic_summary$nomenclatural_diagnostic_class !=
        "multiple_wcvp_concepts"
    ],
    na.rm = TRUE
  )
  
  cli::cli_h2(
    "WCVP nomenclatural diagnostics complete"
  )
  
  cli::cli_text(
    "{.strong Diagnostic concepts:} ",
    "{fmt(sum(input_summary$concepts))}"
  )
  
  cli::cli_text(
    "{.strong PRESENT records represented:} ",
    "{fmt(sum(input_summary$present_records))}"
  )
  
  cli::cli_text(
    "{.strong Concepts with unique WCVP evidence:} ",
    "{fmt(unique_n)}"
  )
  
  cli::cli_text(
    "{.strong PRESENT records represented by unique evidence:} ",
    "{fmt(unique_records)}"
  )
  
  cli::cli_text(
    "{.strong Secure resolved baseline remains:} ",
    "{fmt(baseline$resolved_before_04j[[1]])}"
  )
  
  cli::cli_alert_success(
    "04j is diagnostic only: no accepted-WCVP assignments were made."
  )
  
  cli::cli_alert_success(
    "Validated/frozen 04i and earlier tables were not modified."
  )
  
  cli::cli_alert_success(
    "No fuzzy or edit-distance matching was used."
  )
  
  cli::cli_text(
    "04j_occurrence_wcvp_nomenclatural_diagnostics.R ",
    "v{VPJD_WCVP_NOM_DIAG_VERSION} complete."
  )
  
  invisible(
    list(
      baseline = baseline,
      input_summary = input_summary,
      diagnostic_summary = diagnostic_summary,
      class_by_input = class_by_input,
      unique_evidence = unique_evidence,
      candidate_detail = candidate_detail,
      no_evidence = no_evidence
    )
  )
}

wcvp_nomenclatural_results <-
  run_occurrence_wcvp_nomenclatural_diagnostics()

nomenclatural_input_summary <-
  wcvp_nomenclatural_results$input_summary

nomenclatural_diagnostic_summary <-
  wcvp_nomenclatural_results$diagnostic_summary

nomenclatural_class_by_input <-
  wcvp_nomenclatural_results$class_by_input

nomenclatural_unique_evidence <-
  wcvp_nomenclatural_results$unique_evidence

nomenclatural_candidate_detail <-
  wcvp_nomenclatural_results$candidate_detail

nomenclatural_no_evidence <-
  wcvp_nomenclatural_results$no_evidence