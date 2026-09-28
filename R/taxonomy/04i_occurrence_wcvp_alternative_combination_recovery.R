# ==============================================================================
# 04i_occurrence_wcvp_alternative_combination_recovery.R
# VPJD / Oxford-Japan Plant Conservation Partnership
#
# Purpose:
#   Recover residual species concepts where GBIF provides an alternative
#   species combination and WCVP recognises that exact alternative name.
#
# Input:
#   Validated/frozen 04h v0.1.1 reconciliation.
#
# Recovery principle:
#   - target unresolved species only;
#   - compare the GBIF scientificName binomial with the GBIF species field;
#   - alternative species field must be a genuine binomial;
#   - alternative genus must differ from the scientificName genus;
#   - alternative binomial must exactly match a WCVP species nomenclatural name;
#   - all WCVP matches must converge on exactly one accepted WCVP concept;
#   - accepted concept may legitimately reside in another genus;
#   - no epithet-only matching;
#   - no fuzzy matching;
#   - no edit distance;
#   - no arbitrary candidate selection.
#
# Version: 0.1.0
# ==============================================================================

VPJD_WCVP_ALT_COMB_VERSION <- "0.1.0"

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

run_occurrence_wcvp_alternative_combination_recovery <- function() {
  
  duckdb_path <- here::here(
    "data", "interim", "occurrences",
    "vpjd_occurrences.duckdb"
  )
  
  output_dir <- here::here(
    "outputs", "tables", "taxonomy",
    "occurrence_wcvp_alternative_combination_recovery"
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
    "occurrence_wcvp_reconciliation_name_variant",
    "occurrence_wcvp_name_variant_recovery",
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
    "VPJD WCVP alternative-combination recovery"
  )
  
  # ---------------------------------------------------------------------------
  # Validate 04h input schema
  # ---------------------------------------------------------------------------
  
  required_04h_fields <- c(
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
    "name_variant_final_wcvp_accepted_plant_name_id",
    "name_variant_final_resolution_stage"
  )
  
  actual_04h_fields <- DBI::dbListFields(
    con,
    "occurrence_wcvp_reconciliation_name_variant"
  )
  
  missing_04h_fields <- setdiff(
    required_04h_fields,
    actual_04h_fields
  )
  
  if (length(missing_04h_fields) > 0L) {
    stop(
      "04h reconciliation missing field(s): ",
      paste(missing_04h_fields, collapse = ", ")
    )
  }
  
  # ---------------------------------------------------------------------------
  # Secure 04h baseline
  # ---------------------------------------------------------------------------
  
  baseline <- DBI::dbGetQuery(
    con,
    "
    SELECT
      COUNT(*) AS total_concepts,

      SUM(CASE
        WHEN name_variant_final_wcvp_accepted_plant_name_id IS NOT NULL
        THEN 1 ELSE 0
      END) AS resolved_before_04i,

      COUNT(
        DISTINCT name_variant_final_wcvp_accepted_plant_name_id
      ) AS distinct_accepted_before_04i,

      SUM(CASE
        WHEN name_variant_final_wcvp_accepted_plant_name_id IS NULL
        THEN 1 ELSE 0
      END) AS unresolved_before_04i

    FROM occurrence_wcvp_reconciliation_name_variant
    "
  )
  
  cli::cli_text(
    "{.strong Secure resolved baseline:} ",
    "{fmt(baseline$resolved_before_04i[[1]])}"
  )
  
  cli::cli_text(
    "{.strong Secure distinct accepted concepts:} ",
    "{fmt(baseline$distinct_accepted_before_04i[[1]])}"
  )
  
  cli::cli_text(
    "{.strong Secure unresolved baseline:} ",
    "{fmt(baseline$unresolved_before_04i[[1]])}"
  )
  
  # ---------------------------------------------------------------------------
  # Remove 04i-owned tables only
  # ---------------------------------------------------------------------------
  
  for (tbl in c(
    "occurrence_wcvp_alternative_combination_metadata",
    "occurrence_wcvp_reconciliation_alternative_combination",
    "occurrence_wcvp_alternative_combination_recovery",
    "occurrence_wcvp_alternative_combination_candidates"
  )) {
    DBI::dbExecute(
      con,
      paste0("DROP TABLE IF EXISTS ", tbl)
    )
  }
  
  # ---------------------------------------------------------------------------
  # Residual species after 04h
  #
  # scientificName = GBIF supplied taxon label
  # species        = GBIF species field
  #
  # The species field becomes candidate evidence only where it is a true
  # binomial and differs from the scientificName binomial.
  # ---------------------------------------------------------------------------
  
  DBI::dbExecute(
    con,
    "
    CREATE OR REPLACE TEMP TABLE alt_input_raw AS

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
            COALESCE(
              REGEXP_EXTRACT(
                COALESCE(scientificName, ''),
                '^([^ ]+\\s+[^ ]+)',
                1
              ),
              ''
            ),
            '\\s+', ' ', 'g'
          )
        )
      ) AS scientific_binomial,

      LOWER(
        TRIM(
          REGEXP_REPLACE(
            COALESCE(species, ''),
            '\\s+', ' ', 'g'
          )
        )
      ) AS gbif_species_binomial

    FROM occurrence_wcvp_reconciliation_name_variant

    WHERE name_variant_final_wcvp_accepted_plant_name_id IS NULL
      AND UPPER(COALESCE(taxonRank, '')) = 'SPECIES'
    "
  )
  
  # ---------------------------------------------------------------------------
  # Parse candidate combinations
  # ---------------------------------------------------------------------------
  
  DBI::dbExecute(
    con,
    "
    CREATE OR REPLACE TEMP TABLE alt_input_features AS

    SELECT
      *,

      SPLIT_PART(
        scientific_binomial,
        ' ',
        1
      ) AS scientific_genus,

      SPLIT_PART(
        scientific_binomial,
        ' ',
        2
      ) AS scientific_epithet,

      SPLIT_PART(
        gbif_species_binomial,
        ' ',
        1
      ) AS alternative_genus,

      SPLIT_PART(
        gbif_species_binomial,
        ' ',
        2
      ) AS alternative_epithet,

      ARRAY_LENGTH(
        STRING_SPLIT(
          gbif_species_binomial,
          ' '
        )
      ) AS alternative_token_count

    FROM alt_input_raw
    "
  )
  
  # ---------------------------------------------------------------------------
  # Classify whether GBIF supplies a usable alternative combination
  # ---------------------------------------------------------------------------
  
  DBI::dbExecute(
    con,
    "
    CREATE OR REPLACE TEMP TABLE alt_input AS

    SELECT
      *,

      CASE
        WHEN gbif_species_binomial = ''
        THEN 'missing_species_field'

        WHEN alternative_token_count <> 2
        THEN 'species_field_not_binomial'

        WHEN alternative_genus = ''
          OR alternative_epithet = ''
        THEN 'species_field_not_binomial'

        WHEN alternative_genus = scientific_genus
          AND gbif_species_binomial = scientific_binomial
        THEN 'same_combination'

        WHEN alternative_genus = scientific_genus
        THEN 'same_genus_different_name'

        ELSE 'alternative_genus_combination'
      END AS alternative_input_class

    FROM alt_input_features
    "
  )
  
  input_summary <- DBI::dbGetQuery(
    con,
    "
    SELECT
      alternative_input_class,
      COUNT(*) AS concepts,
      SUM(present_records) AS present_records

    FROM alt_input

    GROUP BY alternative_input_class

    ORDER BY present_records DESC
    "
  )
  
  alternative_summary <- DBI::dbGetQuery(
    con,
    "
    SELECT
      COUNT(*) AS concepts,
      SUM(present_records) AS present_records

    FROM alt_input

    WHERE alternative_input_class =
          'alternative_genus_combination'
    "
  )
  
  cli::cli_text(
    "{.strong Cross-genus GBIF alternative combinations:} ",
    "{fmt(alternative_summary$concepts[[1]])}"
  )
  
  cli::cli_text(
    "{.strong PRESENT records represented:} ",
    "{fmt(alternative_summary$present_records[[1]])}"
  )
  
  # ---------------------------------------------------------------------------
  # Load WCVP nomenclatural species table
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
    "wcvp_04i_source",
    wcvp_source,
    temporary = TRUE,
    overwrite = TRUE
  )
  
  rm(wcvp_source, wcvp_names)
  gc()
  
  # ---------------------------------------------------------------------------
  # Exact WCVP species-name index
  # ---------------------------------------------------------------------------
  
  DBI::dbExecute(
    con,
    "
    CREATE OR REPLACE TEMP TABLE wcvp_04i_species_index AS

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
            COALESCE(taxon_name, ''),
            '\\s+', ' ', 'g'
          )
        )
      ) AS norm_wcvp_taxon_name

    FROM wcvp_04i_source

    WHERE UPPER(COALESCE(taxon_rank, '')) = 'SPECIES'
      AND TRY_CAST(
        accepted_plant_name_id AS BIGINT
      ) IS NOT NULL
    "
  )
  
  # ---------------------------------------------------------------------------
  # I1 - exact GBIF alternative-combination match
  # ---------------------------------------------------------------------------
  
  DBI::dbExecute(
    con,
    "
    CREATE TABLE occurrence_wcvp_alternative_combination_candidates AS

    SELECT DISTINCT
      g.vpjd_taxon_universe_id,

      'I1_GBIF_SPECIES_EXACT'::VARCHAR
        AS recovery_rule,

      g.scientific_binomial
        AS original_binomial,

      g.gbif_species_binomial
        AS alternative_binomial,

      g.scientific_genus
        AS original_genus,

      g.alternative_genus,

      w.wcvp_plant_name_id,
      w.wcvp_taxon_name,
      w.wcvp_taxon_authors,
      w.wcvp_taxon_status,
      w.wcvp_accepted_plant_name_id,
      w.wcvp_family

    FROM alt_input g

    INNER JOIN wcvp_04i_species_index w
      ON g.gbif_species_binomial =
         w.norm_wcvp_taxon_name

    WHERE g.alternative_input_class =
          'alternative_genus_combination'
    "
  )
  
  # ---------------------------------------------------------------------------
  # Candidate counts
  # ---------------------------------------------------------------------------
  
  DBI::dbExecute(
    con,
    "
    CREATE OR REPLACE TEMP TABLE alt_candidate_counts AS

    SELECT
      vpjd_taxon_universe_id,

      COUNT(
        DISTINCT wcvp_plant_name_id
      ) AS nomenclatural_candidate_count,

      COUNT(
        DISTINCT wcvp_accepted_plant_name_id
      ) AS accepted_concept_count,

      MIN(
        wcvp_accepted_plant_name_id
      ) AS unique_accepted_concept_id,

      SUM(
        CASE
          WHEN LOWER(COALESCE(wcvp_taxon_status, '')) =
               'accepted'
          THEN 1
          ELSE 0
        END
      ) AS accepted_status_name_count

    FROM occurrence_wcvp_alternative_combination_candidates

    GROUP BY vpjd_taxon_universe_id
    "
  )
  
  # ---------------------------------------------------------------------------
  # Recovery decisions
  # ---------------------------------------------------------------------------
  
  DBI::dbExecute(
    con,
    "
    CREATE TABLE occurrence_wcvp_alternative_combination_recovery AS

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

      g.scientific_binomial,
      g.gbif_species_binomial,
      g.scientific_genus,
      g.alternative_genus,
      g.alternative_input_class,

      COALESCE(
        c.nomenclatural_candidate_count,
        0
      ) AS nomenclatural_candidate_count,

      COALESCE(
        c.accepted_concept_count,
        0
      ) AS accepted_concept_count,

      COALESCE(
        c.accepted_status_name_count,
        0
      ) AS accepted_status_name_count,

      CASE
        WHEN g.alternative_input_class <>
             'alternative_genus_combination'
        THEN 'not_alternative_combination'

        WHEN c.accepted_concept_count = 1
        THEN 'recovered_unique_wcvp_concept'

        WHEN c.accepted_concept_count > 1
        THEN 'ambiguous_wcvp_concepts'

        ELSE 'alternative_combination_not_in_wcvp'
      END AS alternative_recovery_status,

      CASE
        WHEN c.accepted_concept_count = 1
        THEN c.unique_accepted_concept_id

        ELSE CAST(NULL AS BIGINT)
      END AS alternative_accepted_wcvp_plant_name_id

    FROM alt_input g

    LEFT JOIN alt_candidate_counts c
      ON g.vpjd_taxon_universe_id =
         c.vpjd_taxon_universe_id
    "
  )
  
  # ---------------------------------------------------------------------------
  # Safety check:
  # no recovery without an actual cross-genus alternative combination
  # ---------------------------------------------------------------------------
  
  invalid_recovery_count <- DBI::dbGetQuery(
    con,
    "
    SELECT COUNT(*) AS n

    FROM occurrence_wcvp_alternative_combination_recovery

    WHERE alternative_accepted_wcvp_plant_name_id IS NOT NULL
      AND alternative_input_class <>
          'alternative_genus_combination'
    "
  )$n[[1]]
  
  if (invalid_recovery_count != 0) {
    stop(
      "Safety check failed: 04i recovery without ",
      "a cross-genus GBIF alternative combination."
    )
  }
  
  # ---------------------------------------------------------------------------
  # Consolidate with validated/frozen 04h
  # ---------------------------------------------------------------------------
  
  DBI::dbExecute(
    con,
    "
    CREATE TABLE occurrence_wcvp_reconciliation_alternative_combination AS

    SELECT
      r.*,

      CASE
        WHEN r.name_variant_final_wcvp_accepted_plant_name_id
             IS NOT NULL
        THEN TRY_CAST(
          r.name_variant_final_wcvp_accepted_plant_name_id
          AS BIGINT
        )

        WHEN i.alternative_accepted_wcvp_plant_name_id
             IS NOT NULL
        THEN i.alternative_accepted_wcvp_plant_name_id

        ELSE CAST(NULL AS BIGINT)
      END AS alternative_final_wcvp_accepted_plant_name_id,

      CASE
        WHEN r.name_variant_final_wcvp_accepted_plant_name_id
             IS NOT NULL
        THEN r.name_variant_final_resolution_stage

        WHEN i.alternative_accepted_wcvp_plant_name_id
             IS NOT NULL
        THEN '04i'

        ELSE NULL
      END AS alternative_final_resolution_stage,

      i.alternative_recovery_status,
      i.gbif_species_binomial
        AS alternative_combination_used,

      i.alternative_accepted_wcvp_plant_name_id

    FROM occurrence_wcvp_reconciliation_name_variant r

    LEFT JOIN occurrence_wcvp_alternative_combination_recovery i
      ON r.vpjd_taxon_universe_id =
         i.vpjd_taxon_universe_id
    "
  )
  
  # ---------------------------------------------------------------------------
  # Summaries
  # ---------------------------------------------------------------------------
  
  recovery_summary <- DBI::dbGetQuery(
    con,
    "
    SELECT
      alternative_recovery_status,
      COUNT(*) AS concepts,
      SUM(present_records) AS present_records

    FROM occurrence_wcvp_alternative_combination_recovery

    GROUP BY alternative_recovery_status

    ORDER BY present_records DESC
    "
  )
  
  final_summary <- DBI::dbGetQuery(
    con,
    "
    SELECT
      COUNT(*) AS total_concepts,

      SUM(CASE
        WHEN alternative_final_wcvp_accepted_plant_name_id
             IS NOT NULL
        THEN 1 ELSE 0
      END) AS resolved_concepts,

      SUM(CASE
        WHEN alternative_final_resolution_stage = '04i'
        THEN 1 ELSE 0
      END) AS resolved_in_04i,

      COUNT(
        DISTINCT alternative_final_wcvp_accepted_plant_name_id
      ) AS distinct_accepted_wcvp_concepts,

      SUM(CASE
        WHEN alternative_final_wcvp_accepted_plant_name_id
             IS NULL
        THEN 1 ELSE 0
      END) AS unresolved_concepts

    FROM occurrence_wcvp_reconciliation_alternative_combination
    "
  )
  
  # ---------------------------------------------------------------------------
  # Detailed audit of proposed recoveries
  # ---------------------------------------------------------------------------
  
  recovered_audit <- DBI::dbGetQuery(
    con,
    "
    SELECT
      i.vpjd_taxon_universe_id,
      i.taxonKey,
      i.scientificName,
      i.family,
      i.genus,
      i.species,
      i.present_records,

      i.scientific_binomial,
      i.gbif_species_binomial,

      c.wcvp_plant_name_id,
      c.wcvp_taxon_name,
      c.wcvp_taxon_authors,
      c.wcvp_taxon_status,
      c.wcvp_accepted_plant_name_id,

      a.accepted_wcvp_taxon_name,
      a.accepted_wcvp_taxon_authors,
      a.accepted_wcvp_taxon_rank,
      a.accepted_wcvp_scientific_name

    FROM occurrence_wcvp_alternative_combination_recovery i

    INNER JOIN occurrence_wcvp_alternative_combination_candidates c
      ON i.vpjd_taxon_universe_id =
         c.vpjd_taxon_universe_id

    LEFT JOIN wcvp_occurrence_accepted_lookup a
      ON c.wcvp_accepted_plant_name_id =
         TRY_CAST(
           a.accepted_wcvp_plant_name_id AS BIGINT
         )

    WHERE i.alternative_accepted_wcvp_plant_name_id
          IS NOT NULL

    ORDER BY
      i.present_records DESC,
      i.scientificName
    "
  )
  
  # ---------------------------------------------------------------------------
  # Alternative combinations not recognised exactly by WCVP
  # ---------------------------------------------------------------------------
  
  unmatched_alternatives <- DBI::dbGetQuery(
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
      alternative_recovery_status

    FROM occurrence_wcvp_alternative_combination_recovery

    WHERE alternative_input_class =
          'alternative_genus_combination'

      AND alternative_accepted_wcvp_plant_name_id
          IS NULL

    ORDER BY
      present_records DESC,
      scientificName
    "
  )
  
  # ---------------------------------------------------------------------------
  # Remaining unresolved species after proposed 04i
  # ---------------------------------------------------------------------------
  
  remaining_species <- DBI::dbGetQuery(
    con,
    "
    SELECT
      r.vpjd_taxon_universe_id,
      r.taxonKey,
      r.scientificName,
      r.family,
      r.genus,
      r.species,
      r.present_records,

      i.alternative_input_class,
      i.alternative_recovery_status,
      i.gbif_species_binomial

    FROM occurrence_wcvp_reconciliation_alternative_combination r

    LEFT JOIN occurrence_wcvp_alternative_combination_recovery i
      ON r.vpjd_taxon_universe_id =
         i.vpjd_taxon_universe_id

    WHERE r.alternative_final_wcvp_accepted_plant_name_id
          IS NULL

      AND UPPER(COALESCE(r.taxonRank, '')) = 'SPECIES'

    ORDER BY
      r.present_records DESC,
      r.scientificName
    "
  )
  
  # ---------------------------------------------------------------------------
  # Metadata
  # ---------------------------------------------------------------------------
  
  DBI::dbExecute(
    con,
    "
    CREATE TABLE occurrence_wcvp_alternative_combination_metadata AS

    SELECT
      CURRENT_TIMESTAMP AS created_at,
      '0.1.0'::VARCHAR AS script_version,

      COUNT(*) AS input_residual_species,

      SUM(present_records)
        AS input_present_records,

      SUM(CASE
        WHEN alternative_input_class =
             'alternative_genus_combination'
        THEN 1 ELSE 0
      END) AS alternative_combination_concepts,

      SUM(CASE
        WHEN alternative_accepted_wcvp_plant_name_id
             IS NOT NULL
        THEN 1 ELSE 0
      END) AS recovered_concepts,

      SUM(CASE
        WHEN alternative_accepted_wcvp_plant_name_id
             IS NOT NULL
        THEN present_records ELSE 0
      END) AS recovered_present_records

    FROM occurrence_wcvp_alternative_combination_recovery
    "
  )
  
  # ---------------------------------------------------------------------------
  # Exports
  # ---------------------------------------------------------------------------
  
  full_recovery <- DBI::dbGetQuery(
    con,
    "
    SELECT *
    FROM occurrence_wcvp_alternative_combination_recovery
    ORDER BY present_records DESC, scientificName
    "
  )
  
  exports <- list(
    alternative_combination_input_summary =
      input_summary,
    
    alternative_combination_recovery =
      full_recovery,
    
    alternative_combination_recovery_summary =
      recovery_summary,
    
    alternative_combination_recovered_audit =
      recovered_audit,
    
    alternative_combination_unmatched =
      unmatched_alternatives,
    
    alternative_combination_remaining_species =
      remaining_species,
    
    alternative_combination_final_summary =
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
      recovery_summary$alternative_recovery_status ==
        "recovered_unique_wcvp_concept"
    ],
    na.rm = TRUE
  )
  
  cli::cli_h2(
    "WCVP alternative-combination recovery complete"
  )
  
  cli::cli_text(
    "{.strong Cross-genus alternative combinations:} ",
    "{fmt(alternative_summary$concepts[[1]])}"
  )
  
  cli::cli_text(
    "{.strong Recovered in 04i:} ",
    "{fmt(final_summary$resolved_in_04i[[1]])}"
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
  
  cli::cli_alert_success(
    "Validated/frozen 04h tables were not modified."
  )
  
  cli::cli_alert_success(
    "No epithet-only, fuzzy or edit-distance matching was used."
  )
  
  cli::cli_text(
    "04i_occurrence_wcvp_alternative_combination_recovery.R ",
    "v{VPJD_WCVP_ALT_COMB_VERSION} complete."
  )
  
  invisible(
    list(
      baseline = baseline,
      input_summary = input_summary,
      alternative_summary = alternative_summary,
      recovery_summary = recovery_summary,
      final_summary = final_summary,
      recovered_audit = recovered_audit,
      unmatched_alternatives = unmatched_alternatives,
      remaining_species = remaining_species
    )
  )
}

wcvp_alt_combination_results <-
  run_occurrence_wcvp_alternative_combination_recovery()

alt_combination_input_summary <-
  wcvp_alt_combination_results$input_summary

alt_combination_summary <-
  wcvp_alt_combination_results$alternative_summary

alt_combination_recovery_summary <-
  wcvp_alt_combination_results$recovery_summary

alt_combination_final_summary <-
  wcvp_alt_combination_results$final_summary

alt_combination_recovered_audit <-
  wcvp_alt_combination_results$recovered_audit

alt_combination_unmatched <-
  wcvp_alt_combination_results$unmatched_alternatives

alt_combination_remaining_species <-
  wcvp_alt_combination_results$remaining_species