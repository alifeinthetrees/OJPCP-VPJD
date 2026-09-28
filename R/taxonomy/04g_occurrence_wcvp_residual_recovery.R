# ==============================================================================
# 04g_occurrence_wcvp_residual_recovery.R
# VPJD / Oxford-Japan Plant Conservation Partnership
#
# Purpose:
#   Diagnose unresolved species concepts remaining after validated/frozen 04f
#   and prepare conservative candidate evidence for subsequent recovery.
#
# Important:
#   - Starts from frozen 04f.
#   - Does NOT inherit 04g v0.1.0 assignments.
#   - Makes NO new accepted-WCVP assignments.
#   - Separates genus-only / pseudo-species concepts.
#   - Preserves exact-name ambiguity.
#   - Diagnoses possible alternative combinations and controlled name variants.
#   - No fuzzy matching.
#   - No edit-distance matching.
#   - No arbitrary candidate selection.
#
# Version: 0.2.0
# ==============================================================================

VPJD_WCVP_RESIDUAL_VERSION <- "0.2.0"

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

run_occurrence_wcvp_residual_recovery <- function() {
  
  duckdb_path <- here::here(
    "data", "interim", "occurrences",
    "vpjd_occurrences.duckdb"
  )
  
  output_dir <- here::here(
    "outputs", "tables", "taxonomy",
    "occurrence_wcvp_residual_recovery"
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
    "VPJD residual species diagnostic recovery"
  )
  
  fmt <- function(x) {
    format(
      x,
      big.mark = ",",
      scientific = FALSE,
      trim = TRUE
    )
  }
  
  # ---------------------------------------------------------------------------
  # Validate frozen 04f
  # ---------------------------------------------------------------------------
  
  required_fields <- c(
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
    "species_final_resolution_stage",
    "species_recovery_status"
  )
  
  actual_fields <- DBI::dbListFields(
    con,
    "occurrence_wcvp_reconciliation_species"
  )
  
  missing_fields <- setdiff(
    required_fields,
    actual_fields
  )
  
  if (length(missing_fields) > 0L) {
    stop(
      "04f reconciliation missing field(s): ",
      paste(missing_fields, collapse = ", ")
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
      END) AS resolved_before_04g,

      COUNT(
        DISTINCT species_final_wcvp_accepted_plant_name_id
      ) AS distinct_accepted_before_04g,

      SUM(CASE
        WHEN species_final_wcvp_accepted_plant_name_id IS NULL
        THEN 1 ELSE 0
      END) AS unresolved_before_04g

    FROM occurrence_wcvp_reconciliation_species
    "
  )
  
  cli::cli_text(
    "{.strong Frozen 04f resolved baseline:} ",
    "{fmt(baseline$resolved_before_04g[[1]])}"
  )
  
  cli::cli_text(
    "{.strong Frozen 04f distinct accepted concepts:} ",
    "{fmt(baseline$distinct_accepted_before_04g[[1]])}"
  )
  
  cli::cli_text(
    "{.strong Frozen 04f unresolved baseline:} ",
    "{fmt(baseline$unresolved_before_04g[[1]])}"
  )
  
  # ---------------------------------------------------------------------------
  # Remove previous 04g-owned tables
  # ---------------------------------------------------------------------------
  
  for (tbl in c(
    "occurrence_wcvp_residual_diagnostic_metadata",
    "occurrence_wcvp_residual_diagnostics",
    "occurrence_wcvp_residual_candidates",
    "occurrence_wcvp_residual_recovery",
    "occurrence_wcvp_reconciliation_residual"
  )) {
    DBI::dbExecute(
      con,
      paste0("DROP TABLE IF EXISTS ", tbl)
    )
  }
  
  # ---------------------------------------------------------------------------
  # All unresolved species from frozen 04f
  # ---------------------------------------------------------------------------
  
  DBI::dbExecute(
    con,
    "
    CREATE OR REPLACE TEMP TABLE residual_species_all AS

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
      species_recovery_status,

      LOWER(
        TRIM(
          REGEXP_REPLACE(
            REPLACE(
              COALESCE(genus, ''),
              '×', 'x'
            ),
            '\\s+', ' ', 'g'
          )
        )
      ) AS norm_genus,

      LOWER(
        TRIM(
          REGEXP_REPLACE(
            REPLACE(
              COALESCE(species, ''),
              '×', 'x'
            ),
            '\\s+', ' ', 'g'
          )
        )
      ) AS norm_species,

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
      ) AS norm_scientific_binomial

    FROM occurrence_wcvp_reconciliation_species

    WHERE species_final_wcvp_accepted_plant_name_id IS NULL
      AND UPPER(COALESCE(taxonRank, '')) = 'SPECIES'
    "
  )
  
  all_input_summary <- DBI::dbGetQuery(
    con,
    "
    SELECT
      COUNT(*) AS concepts,
      SUM(present_records) AS present_records
    FROM residual_species_all
    "
  )
  
  # ---------------------------------------------------------------------------
  # Detect pseudo-species / genus-only concepts
  #
  # A species concept is treated as genus-only when the GBIF species field:
  #   - is blank, OR
  #   - normalises exactly to the genus, OR
  #   - contains only one token.
  #
  # These are NOT accepted as species-level resolutions.
  # ---------------------------------------------------------------------------
  
  DBI::dbExecute(
    con,
    "
    CREATE OR REPLACE TEMP TABLE residual_species_classified AS

    SELECT
      *,

      CASE
        WHEN norm_species = ''
        THEN TRUE

        WHEN norm_genus <> ''
         AND norm_species = norm_genus
        THEN TRUE

        WHEN ARRAY_LENGTH(
          STRING_SPLIT(
            TRIM(COALESCE(species, '')),
            ' '
          )
        ) < 2
        THEN TRUE

        ELSE FALSE
      END AS genus_only_species_field

    FROM residual_species_all
    "
  )
  
  pseudo_species <- DBI::dbGetQuery(
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

      'higher_rank_misclassified_as_species'
        AS residual_class

    FROM residual_species_classified

    WHERE genus_only_species_field = TRUE

    ORDER BY
      present_records DESC,
      scientificName
    "
  )
  
  # ---------------------------------------------------------------------------
  # Genuine residual species
  # ---------------------------------------------------------------------------
  
  DBI::dbExecute(
    con,
    "
    CREATE OR REPLACE TEMP TABLE residual_species_input AS

    SELECT *
    FROM residual_species_classified

    WHERE genus_only_species_field = FALSE
    "
  )
  
  input_summary <- DBI::dbGetQuery(
    con,
    "
    SELECT
      COUNT(*) AS concepts,
      SUM(present_records) AS present_records
    FROM residual_species_input
    "
  )
  
  cli::cli_text(
    "{.strong All unresolved species-rank concepts:} ",
    "{fmt(all_input_summary$concepts[[1]])}"
  )
  
  cli::cli_text(
    "{.strong Genus-only pseudo-species:} ",
    "{fmt(nrow(pseudo_species))}"
  )
  
  cli::cli_text(
    "{.strong Genuine residual species concepts:} ",
    "{fmt(input_summary$concepts[[1]])}"
  )
  
  cli::cli_text(
    "{.strong PRESENT records in genuine residual species:} ",
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
    "wcvp_04g_source",
    wcvp_source,
    temporary = TRUE,
    overwrite = TRUE
  )
  
  rm(wcvp_source, wcvp_names)
  gc()
  
  # ---------------------------------------------------------------------------
  # WCVP species-name index
  # ---------------------------------------------------------------------------
  
  DBI::dbExecute(
    con,
    "
    CREATE OR REPLACE TEMP TABLE wcvp_04g_species_index AS

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
            REPLACE(
              COALESCE(taxon_name, ''),
              '×', 'x'
            ),
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
        SPLIT_PART(
          TRIM(COALESCE(taxon_name, '')),
          ' ',
          2
        )
      ) AS norm_wcvp_epithet

    FROM wcvp_04g_source

    WHERE UPPER(COALESCE(taxon_rank, '')) = 'SPECIES'
      AND TRY_CAST(
        accepted_plant_name_id AS BIGINT
      ) IS NOT NULL
    "
  )
  
  # ---------------------------------------------------------------------------
  # Extract GBIF epithet and hybrid diagnostics
  # ---------------------------------------------------------------------------
  
  DBI::dbExecute(
    con,
    "
    CREATE OR REPLACE TEMP TABLE residual_species_features AS

    SELECT
      *,

      LOWER(
        SPLIT_PART(
          norm_species,
          ' ',
          2
        )
      ) AS gbif_epithet,

      CASE
        WHEN scientificName LIKE '%×%'
          OR scientificName LIKE '% x %'
          OR species LIKE '%×%'
          OR species LIKE '% x %'
        THEN TRUE
        ELSE FALSE
      END AS has_hybrid_marker,

      LOWER(
        TRIM(
          REGEXP_REPLACE(
            REPLACE(
              REPLACE(
                COALESCE(species, ''),
                '×', ''
              ),
              ' x ', ' '
            ),
            '\\s+', ' ', 'g'
          )
        )
      ) AS norm_species_without_hybrid

    FROM residual_species_input
    "
  )
  
  # ---------------------------------------------------------------------------
  # D1 - reproduce exact-name evidence from 04f for diagnostics
  # ---------------------------------------------------------------------------
  
  DBI::dbExecute(
    con,
    "
    CREATE OR REPLACE TEMP TABLE diagnostic_exact_candidates AS

    SELECT DISTINCT
      g.vpjd_taxon_universe_id,
      'D1_EXACT_BINOMIAL'::VARCHAR
        AS diagnostic_stage,

      w.wcvp_plant_name_id,
      w.wcvp_taxon_name,
      w.wcvp_taxon_authors,
      w.wcvp_taxon_status,
      w.wcvp_accepted_plant_name_id,
      w.wcvp_family

    FROM residual_species_features g

    INNER JOIN wcvp_04g_species_index w
      ON g.norm_scientific_binomial =
         w.norm_taxon_name

    WHERE g.norm_scientific_binomial <> ''
    "
  )
  
  # ---------------------------------------------------------------------------
  # Identify exact-name ambiguous concepts
  # ---------------------------------------------------------------------------
  
  DBI::dbExecute(
    con,
    "
    CREATE OR REPLACE TEMP TABLE diagnostic_exact_counts AS

    SELECT
      vpjd_taxon_universe_id,

      COUNT(DISTINCT wcvp_plant_name_id)
        AS exact_name_count,

      COUNT(DISTINCT wcvp_accepted_plant_name_id)
        AS exact_accepted_concept_count

    FROM diagnostic_exact_candidates

    GROUP BY vpjd_taxon_universe_id
    "
  )
  
  # ---------------------------------------------------------------------------
  # D2 - same epithet, different genus
  #
  # Diagnostic only. This identifies potential alternative combinations.
  # It does NOT resolve them.
  # ---------------------------------------------------------------------------
  
  DBI::dbExecute(
    con,
    "
    CREATE OR REPLACE TEMP TABLE diagnostic_epithet_candidates AS

    SELECT DISTINCT
      g.vpjd_taxon_universe_id,
      'D2_SAME_EPITHET_DIFFERENT_GENUS'::VARCHAR
        AS diagnostic_stage,

      w.wcvp_plant_name_id,
      w.wcvp_taxon_name,
      w.wcvp_taxon_authors,
      w.wcvp_taxon_status,
      w.wcvp_accepted_plant_name_id,
      w.wcvp_family

    FROM residual_species_features g

    LEFT JOIN diagnostic_exact_counts e
      ON g.vpjd_taxon_universe_id =
         e.vpjd_taxon_universe_id

    INNER JOIN wcvp_04g_species_index w
      ON g.gbif_epithet =
         w.norm_wcvp_epithet

    WHERE g.gbif_epithet <> ''
      AND COALESCE(
        e.exact_name_count,
        0
      ) = 0

      AND g.norm_genus <>
          w.norm_wcvp_genus
    "
  )
  
  # ---------------------------------------------------------------------------
  # D3 - hybrid-marker normalisation
  #
  # Diagnostic only.
  # ---------------------------------------------------------------------------
  
  DBI::dbExecute(
    con,
    "
    CREATE OR REPLACE TEMP TABLE diagnostic_hybrid_candidates AS

    SELECT DISTINCT
      g.vpjd_taxon_universe_id,
      'D3_HYBRID_MARKER_NORMALISED'::VARCHAR
        AS diagnostic_stage,

      w.wcvp_plant_name_id,
      w.wcvp_taxon_name,
      w.wcvp_taxon_authors,
      w.wcvp_taxon_status,
      w.wcvp_accepted_plant_name_id,
      w.wcvp_family

    FROM residual_species_features g

    LEFT JOIN diagnostic_exact_counts e
      ON g.vpjd_taxon_universe_id =
         e.vpjd_taxon_universe_id

    INNER JOIN wcvp_04g_species_index w
      ON g.norm_species_without_hybrid =
         w.norm_taxon_name

    WHERE g.has_hybrid_marker = TRUE

      AND COALESCE(
        e.exact_name_count,
        0
      ) = 0

      AND g.norm_species_without_hybrid <> ''
    "
  )
  
  # ---------------------------------------------------------------------------
  # Persistent candidate evidence
  # ---------------------------------------------------------------------------
  
  DBI::dbExecute(
    con,
    "
    CREATE TABLE occurrence_wcvp_residual_candidates AS

    SELECT *
    FROM diagnostic_exact_candidates

    UNION ALL

    SELECT *
    FROM diagnostic_epithet_candidates

    UNION ALL

    SELECT *
    FROM diagnostic_hybrid_candidates
    "
  )
  
  # ---------------------------------------------------------------------------
  # Candidate counts by diagnostic route
  # ---------------------------------------------------------------------------
  
  candidate_summary <- DBI::dbGetQuery(
    con,
    "
    SELECT
      diagnostic_stage,

      COUNT(
        DISTINCT vpjd_taxon_universe_id
      ) AS concepts_with_candidates,

      COUNT(*) AS candidate_rows,

      COUNT(
        DISTINCT wcvp_accepted_plant_name_id
      ) AS accepted_concepts_represented

    FROM occurrence_wcvp_residual_candidates

    GROUP BY diagnostic_stage

    ORDER BY diagnostic_stage
    "
  )
  
  # ---------------------------------------------------------------------------
  # Candidate counts per residual concept
  # ---------------------------------------------------------------------------
  
  DBI::dbExecute(
    con,
    "
    CREATE OR REPLACE TEMP TABLE diagnostic_candidate_counts AS

    SELECT
      vpjd_taxon_universe_id,

      COUNT(DISTINCT CASE
        WHEN diagnostic_stage = 'D1_EXACT_BINOMIAL'
        THEN wcvp_accepted_plant_name_id
        ELSE NULL
      END) AS d1_accepted_concepts,

      COUNT(DISTINCT CASE
        WHEN diagnostic_stage =
          'D2_SAME_EPITHET_DIFFERENT_GENUS'
        THEN wcvp_accepted_plant_name_id
        ELSE NULL
      END) AS d2_accepted_concepts,

      COUNT(DISTINCT CASE
        WHEN diagnostic_stage =
          'D3_HYBRID_MARKER_NORMALISED'
        THEN wcvp_accepted_plant_name_id
        ELSE NULL
      END) AS d3_accepted_concepts

    FROM occurrence_wcvp_residual_candidates

    GROUP BY vpjd_taxon_universe_id
    "
  )
  
  # ---------------------------------------------------------------------------
  # Classify residual concepts
  # ---------------------------------------------------------------------------
  
  DBI::dbExecute(
    con,
    "
    CREATE TABLE occurrence_wcvp_residual_diagnostics AS

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
      g.has_hybrid_marker,

      COALESCE(
        c.d1_accepted_concepts,
        0
      ) AS d1_accepted_concepts,

      COALESCE(
        c.d2_accepted_concepts,
        0
      ) AS d2_accepted_concepts,

      COALESCE(
        c.d3_accepted_concepts,
        0
      ) AS d3_accepted_concepts,

      CASE
        WHEN COALESCE(
          c.d1_accepted_concepts,
          0
        ) > 1
        THEN 'exact_name_ambiguous'

        WHEN COALESCE(
          c.d2_accepted_concepts,
          0
        ) = 1
        THEN 'single_alternative_genus_candidate'

        WHEN COALESCE(
          c.d2_accepted_concepts,
          0
        ) > 1
        THEN 'multiple_alternative_genus_candidates'

        WHEN COALESCE(
          c.d3_accepted_concepts,
          0
        ) = 1
        THEN 'single_hybrid_normalisation_candidate'

        WHEN COALESCE(
          c.d3_accepted_concepts,
          0
        ) > 1
        THEN 'multiple_hybrid_normalisation_candidates'

        ELSE 'no_controlled_candidate'
      END AS residual_diagnostic_class

    FROM residual_species_features g

    LEFT JOIN diagnostic_candidate_counts c
      ON g.vpjd_taxon_universe_id =
         c.vpjd_taxon_universe_id
    "
  )
  
  # ---------------------------------------------------------------------------
  # Diagnostic summary
  # ---------------------------------------------------------------------------
  
  diagnostic_summary <- DBI::dbGetQuery(
    con,
    "
    SELECT
      residual_diagnostic_class,
      COUNT(*) AS concepts,
      SUM(present_records) AS present_records

    FROM occurrence_wcvp_residual_diagnostics

    GROUP BY residual_diagnostic_class

    ORDER BY
      present_records DESC,
      concepts DESC
    "
  )
  
  # ---------------------------------------------------------------------------
  # Single alternative-genus candidates
  # ---------------------------------------------------------------------------
  
  single_alt_genus <- DBI::dbGetQuery(
    con,
    "
    SELECT
      d.vpjd_taxon_universe_id,
      d.taxonKey,
      d.scientificName,
      d.family AS gbif_family,
      d.genus AS gbif_genus,
      d.species AS gbif_species,
      d.present_records,

      c.wcvp_plant_name_id,
      c.wcvp_taxon_name,
      c.wcvp_taxon_authors,
      c.wcvp_taxon_status,
      c.wcvp_accepted_plant_name_id,
      c.wcvp_family,

      a.accepted_wcvp_taxon_name,
      a.accepted_wcvp_taxon_authors,
      a.accepted_wcvp_taxon_rank,
      a.accepted_wcvp_scientific_name

    FROM occurrence_wcvp_residual_diagnostics d

    INNER JOIN occurrence_wcvp_residual_candidates c
      ON d.vpjd_taxon_universe_id =
         c.vpjd_taxon_universe_id

    LEFT JOIN wcvp_occurrence_accepted_lookup a
      ON c.wcvp_accepted_plant_name_id =
         TRY_CAST(
           a.accepted_wcvp_plant_name_id AS BIGINT
         )

    WHERE d.residual_diagnostic_class =
          'single_alternative_genus_candidate'

      AND c.diagnostic_stage =
          'D2_SAME_EPITHET_DIFFERENT_GENUS'

    ORDER BY
      d.present_records DESC,
      d.scientificName
    "
  )
  
  # ---------------------------------------------------------------------------
  # Hybrid candidates
  # ---------------------------------------------------------------------------
  
  hybrid_candidates <- DBI::dbGetQuery(
    con,
    "
    SELECT
      d.vpjd_taxon_universe_id,
      d.taxonKey,
      d.scientificName,
      d.family AS gbif_family,
      d.genus AS gbif_genus,
      d.species AS gbif_species,
      d.present_records,
      d.residual_diagnostic_class,

      c.wcvp_plant_name_id,
      c.wcvp_taxon_name,
      c.wcvp_taxon_authors,
      c.wcvp_taxon_status,
      c.wcvp_accepted_plant_name_id,

      a.accepted_wcvp_taxon_name,
      a.accepted_wcvp_taxon_authors,
      a.accepted_wcvp_taxon_rank,
      a.accepted_wcvp_scientific_name

    FROM occurrence_wcvp_residual_diagnostics d

    INNER JOIN occurrence_wcvp_residual_candidates c
      ON d.vpjd_taxon_universe_id =
         c.vpjd_taxon_universe_id

    LEFT JOIN wcvp_occurrence_accepted_lookup a
      ON c.wcvp_accepted_plant_name_id =
         TRY_CAST(
           a.accepted_wcvp_plant_name_id AS BIGINT
         )

    WHERE c.diagnostic_stage =
          'D3_HYBRID_MARKER_NORMALISED'

    ORDER BY
      d.present_records DESC,
      d.scientificName
    "
  )
  
  # ---------------------------------------------------------------------------
  # Highest-priority unresolved concepts
  # ---------------------------------------------------------------------------
  
  priority_residuals <- DBI::dbGetQuery(
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
      d1_accepted_concepts,
      d2_accepted_concepts,
      d3_accepted_concepts

    FROM occurrence_wcvp_residual_diagnostics

    ORDER BY
      present_records DESC,
      scientificName
    "
  )
  
  # ---------------------------------------------------------------------------
  # Preserve frozen 04f as authoritative reconciliation
  #
  # 04g v0.2.0 makes no new accepted assignments.
  # ---------------------------------------------------------------------------
  
  DBI::dbExecute(
    con,
    "
    CREATE TABLE occurrence_wcvp_reconciliation_residual AS

    SELECT
      r.*,

      TRY_CAST(
        r.species_final_wcvp_accepted_plant_name_id AS BIGINT
      ) AS residual_final_wcvp_accepted_plant_name_id,

      r.species_final_resolution_stage
        AS residual_final_resolution_stage,

      CASE
        WHEN r.species_final_wcvp_accepted_plant_name_id
             IS NOT NULL
        THEN 'resolved_before_04g'

        WHEN UPPER(COALESCE(r.taxonRank, '')) <> 'SPECIES'
        THEN 'not_targeted_in_04g'

        WHEN p.vpjd_taxon_universe_id IS NOT NULL
        THEN 'higher_rank_misclassified_as_species'

        ELSE d.residual_diagnostic_class
      END AS residual_04g_status

    FROM occurrence_wcvp_reconciliation_species r

    LEFT JOIN (
      SELECT
        vpjd_taxon_universe_id
      FROM residual_species_classified
      WHERE genus_only_species_field = TRUE
    ) p
      ON r.vpjd_taxon_universe_id =
         p.vpjd_taxon_universe_id

    LEFT JOIN occurrence_wcvp_residual_diagnostics d
      ON r.vpjd_taxon_universe_id =
         d.vpjd_taxon_universe_id
    "
  )
  
  # ---------------------------------------------------------------------------
  # Final baseline check
  # ---------------------------------------------------------------------------
  
  final_summary <- DBI::dbGetQuery(
    con,
    "
    SELECT
      COUNT(*) AS total_concepts,

      SUM(CASE
        WHEN residual_final_wcvp_accepted_plant_name_id IS NOT NULL
        THEN 1 ELSE 0
      END) AS resolved_concepts,

      COUNT(
        DISTINCT residual_final_wcvp_accepted_plant_name_id
      ) AS distinct_accepted_wcvp_concepts,

      SUM(CASE
        WHEN residual_final_wcvp_accepted_plant_name_id IS NULL
        THEN 1 ELSE 0
      END) AS unresolved_concepts

    FROM occurrence_wcvp_reconciliation_residual
    "
  )
  
  # ---------------------------------------------------------------------------
  # Safety check: 04g v0.2.0 must not change frozen 04f totals
  # ---------------------------------------------------------------------------
  
  if (
    final_summary$resolved_concepts[[1]] !=
    baseline$resolved_before_04g[[1]]
  ) {
    stop(
      "Safety check failed: 04g v0.2.0 changed the number ",
      "of resolved concepts."
    )
  }
  
  if (
    final_summary$distinct_accepted_wcvp_concepts[[1]] !=
    baseline$distinct_accepted_before_04g[[1]]
  ) {
    stop(
      "Safety check failed: 04g v0.2.0 changed the number ",
      "of distinct accepted WCVP concepts."
    )
  }
  
  # ---------------------------------------------------------------------------
  # Metadata
  # ---------------------------------------------------------------------------
  
  DBI::dbExecute(
    con,
    "
    CREATE TABLE occurrence_wcvp_residual_diagnostic_metadata AS

    SELECT
      CURRENT_TIMESTAMP AS created_at,
      '0.2.0'::VARCHAR AS script_version,

      (SELECT COUNT(*)
       FROM residual_species_all)
        AS unresolved_species_rank_concepts,

      (SELECT SUM(present_records)
       FROM residual_species_all)
        AS unresolved_species_rank_present_records,

      (SELECT COUNT(*)
       FROM residual_species_classified
       WHERE genus_only_species_field = TRUE)
        AS genus_only_pseudo_species,

      (SELECT SUM(present_records)
       FROM residual_species_classified
       WHERE genus_only_species_field = TRUE)
        AS genus_only_present_records,

      (SELECT COUNT(*)
       FROM residual_species_input)
        AS genuine_residual_species,

      (SELECT SUM(present_records)
       FROM residual_species_input)
        AS genuine_residual_present_records
    "
  )
  
  # ---------------------------------------------------------------------------
  # Export
  # ---------------------------------------------------------------------------
  
  exports <- list(
    residual_pseudo_species = pseudo_species,
    residual_candidate_summary = candidate_summary,
    residual_diagnostic_summary = diagnostic_summary,
    residual_single_alternative_genus_candidates = single_alt_genus,
    residual_hybrid_candidates = hybrid_candidates,
    residual_priority_species = priority_residuals,
    residual_final_summary = final_summary
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
    "Residual species diagnostics complete"
  )
  
  cli::cli_text(
    "{.strong Unresolved species-rank concepts from 04f:} ",
    "{fmt(all_input_summary$concepts[[1]])}"
  )
  
  cli::cli_text(
    "{.strong PRESENT records represented:} ",
    "{fmt(all_input_summary$present_records[[1]])}"
  )
  
  cli::cli_text(
    "{.strong Genus-only pseudo-species:} ",
    "{fmt(nrow(pseudo_species))}"
  )
  
  cli::cli_text(
    "{.strong Genuine residual species concepts:} ",
    "{fmt(input_summary$concepts[[1]])}"
  )
  
  cli::cli_text(
    "{.strong Genuine residual PRESENT records:} ",
    "{fmt(input_summary$present_records[[1]])}"
  )
  
  cli::cli_text(
    "{.strong Resolved concepts remain:} ",
    "{fmt(final_summary$resolved_concepts[[1]])}"
  )
  
  cli::cli_text(
    "{.strong Distinct accepted WCVP concepts remain:} ",
    "{fmt(final_summary$distinct_accepted_wcvp_concepts[[1]])}"
  )
  
  cli::cli_text(
    "{.strong Total unresolved concepts remain:} ",
    "{fmt(final_summary$unresolved_concepts[[1]])}"
  )
  
  cli::cli_alert_success(
    "04g v0.2.0 made no new taxonomic assignments."
  )
  
  cli::cli_alert_success(
    "The 04f accepted-taxon baseline was preserved exactly."
  )
  
  cli::cli_alert_success(
    "Candidate transformations are diagnostic only."
  )
  
  cli::cli_alert_success(
    "No fuzzy or edit-distance matching was used."
  )
  
  cli::cli_text(
    "04g_occurrence_wcvp_residual_recovery.R ",
    "v{VPJD_WCVP_RESIDUAL_VERSION} complete."
  )
  
  invisible(
    list(
      baseline = baseline,
      all_input_summary = all_input_summary,
      input_summary = input_summary,
      pseudo_species = pseudo_species,
      candidate_summary = candidate_summary,
      diagnostic_summary = diagnostic_summary,
      single_alt_genus = single_alt_genus,
      hybrid_candidates = hybrid_candidates,
      priority_residuals = priority_residuals,
      final_summary = final_summary
    )
  )
}

wcvp_residual_results <-
  run_occurrence_wcvp_residual_recovery()

residual_all_input_summary <-
  wcvp_residual_results$all_input_summary

residual_input_summary <-
  wcvp_residual_results$input_summary

residual_pseudo_species <-
  wcvp_residual_results$pseudo_species

residual_candidate_summary <-
  wcvp_residual_results$candidate_summary

residual_diagnostic_summary <-
  wcvp_residual_results$diagnostic_summary

residual_single_alt_genus <-
  wcvp_residual_results$single_alt_genus

residual_hybrid_candidates <-
  wcvp_residual_results$hybrid_candidates

residual_priority_species <-
  wcvp_residual_results$priority_residuals

residual_final_summary <-
  wcvp_residual_results$final_summary