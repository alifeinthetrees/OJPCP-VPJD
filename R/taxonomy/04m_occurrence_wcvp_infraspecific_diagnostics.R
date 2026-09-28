# ==============================================================================
# 04m_occurrence_wcvp_infraspecific_diagnostics.R
# VPJD / Oxford-Japan Plant Conservation Partnership
#
# Purpose:
#   Harden the residual infraspecific WCVP diagnostic.
#
#   Distinguish conservative deterministic WCVP outcomes from nomenclatural
#   evidence that redirects the source concept to a materially different
#   accepted taxon.
#
# Targets:
#   Unresolved SUBSPECIES, VARIETY and FORM concepts after frozen 04l.
#
# IMPORTANT:
#   DIAGNOSTIC ONLY.
#   No assignments are made.
#   No fuzzy matching.
#   No edit-distance matching.
#   No epithet-only recovery.
#   No family-level evidence is considered secure.
#   04l and all upstream reconciliation tables remain unchanged.
#
# Version: 0.1.1
# ==============================================================================

VPJD_WCVP_INFRA_DIAG_VERSION <- "0.1.1"

required_packages <- c(
  "here","DBI","duckdb","readr","dplyr","cli","rWCVPdata"
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

run_occurrence_wcvp_infraspecific_diagnostics <- function() {
  
  duckdb_path <- here::here(
    "data","interim","occurrences","vpjd_occurrences.duckdb"
  )
  
  output_dir <- here::here(
    "outputs","tables","taxonomy",
    "occurrence_wcvp_infraspecific_diagnostics"
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
    "occurrence_wcvp_reconciliation_hybrid",
    "occurrence_wcvp_hybrid_recovery_metadata"
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
    "VPJD WCVP hardened residual infraspecific diagnostics"
  )
  
  # ---------------------------------------------------------------------------
  # Validate frozen 04l baseline
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
      ) AS resolved_before_04m,

      COUNT(
        DISTINCT hybrid_final_wcvp_accepted_plant_name_id
      ) AS distinct_accepted_before_04m,

      SUM(
        CASE
          WHEN hybrid_final_wcvp_accepted_plant_name_id IS NULL
          THEN 1 ELSE 0
        END
      ) AS unresolved_before_04m

    FROM occurrence_wcvp_reconciliation_hybrid
    "
  )
  
  if (
    baseline$total_concepts[[1]] != 21165L ||
    baseline$resolved_before_04m[[1]] != 19435L ||
    baseline$distinct_accepted_before_04m[[1]] != 12019L ||
    baseline$unresolved_before_04m[[1]] != 1730L
  ) {
    stop(
      "04l baseline differs from validated/frozen state. Expected ",
      "21,165 total / 19,435 resolved / 12,019 distinct accepted / ",
      "1,730 unresolved."
    )
  }
  
  cli::cli_text(
    "{.strong Secure resolved baseline:} ",
    "{fmt(baseline$resolved_before_04m[[1]])}"
  )
  
  cli::cli_text(
    "{.strong Distinct accepted concepts:} ",
    "{fmt(baseline$distinct_accepted_before_04m[[1]])}"
  )
  
  cli::cli_text(
    "{.strong Unresolved baseline:} ",
    "{fmt(baseline$unresolved_before_04m[[1]])}"
  )
  
  # ---------------------------------------------------------------------------
  # Remove 04m-owned persistent products only
  # ---------------------------------------------------------------------------
  
  for (tbl in c(
    "occurrence_wcvp_infraspecific_diagnostic_metadata",
    "occurrence_wcvp_infraspecific_diagnostics",
    "occurrence_wcvp_infraspecific_candidates"
  )) {
    DBI::dbExecute(
      con,
      paste0("DROP TABLE IF EXISTS ", tbl)
    )
  }
  
  # ---------------------------------------------------------------------------
  # Residual infraspecific input
  # ---------------------------------------------------------------------------
  
  DBI::dbExecute(
    con,
    "
    CREATE OR REPLACE TEMP TABLE infra_input AS

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
            COALESCE(scientificName, ''),
            '\\s+',
            ' ',
            'g'
          )
        )
      ) AS norm_scientific_name,

      LOWER(
        TRIM(
          REGEXP_REPLACE(
            COALESCE(species, ''),
            '\\s+',
            ' ',
            'g'
          )
        )
      ) AS norm_species,

      LOWER(
        REGEXP_EXTRACT(
          COALESCE(scientificName, ''),
          '^([^ ]+)',
          1
        )
      ) AS parsed_genus,

      LOWER(
        REGEXP_EXTRACT(
          COALESCE(scientificName, ''),
          '^[^ ]+[[:space:]]+([^ ]+)',
          1
        )
      ) AS parsed_species_epithet,

      LOWER(
        REGEXP_EXTRACT(
          COALESCE(scientificName, ''),
          '(?:subsp\\.|ssp\\.|var\\.|f\\.)[[:space:]]+([^ ]+)',
          1
        )
      ) AS parsed_infra_epithet

    FROM occurrence_wcvp_reconciliation_hybrid

    WHERE hybrid_final_wcvp_accepted_plant_name_id IS NULL

      AND UPPER(
        COALESCE(taxonRank, '')
      ) IN (
        'SUBSPECIES',
        'VARIETY',
        'FORM'
      )
    "
  )
  
  input_summary <- DBI::dbGetQuery(
    con,
    "
    SELECT
      taxonRank,
      COUNT(*) AS concepts,
      SUM(present_records) AS present_records

    FROM infra_input

    GROUP BY taxonRank

    ORDER BY present_records DESC
    "
  )
  
  input_total <- DBI::dbGetQuery(
    con,
    "
    SELECT
      COUNT(*) AS concepts,
      SUM(present_records) AS present_records,

      SUM(
        CASE
          WHEN parsed_infra_epithet IS NULL
            OR parsed_infra_epithet = ''
          THEN 1 ELSE 0
        END
      ) AS infra_epithet_parse_failures

    FROM infra_input
    "
  )
  
  if (
    input_total$concepts[[1]] != 1018L ||
    input_total$present_records[[1]] != 105744L ||
    input_total$infra_epithet_parse_failures[[1]] != 0L
  ) {
    stop(
      "04m input differs from validated v0.1.0 diagnostic. Expected ",
      "1,018 concepts / 105,744 PRESENT records / 0 parse failures."
    )
  }
  
  cli::cli_text(
    "{.strong Residual infraspecific concepts:} ",
    "{fmt(input_total$concepts[[1]])}"
  )
  
  cli::cli_text(
    "{.strong PRESENT records represented:} ",
    "{fmt(input_total$present_records[[1]])}"
  )
  
  # ---------------------------------------------------------------------------
  # Complete WCVP source
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
  
  DBI::dbWriteTable(
    con,
    "wcvp_04m_source",
    wcvp_names[
      ,
      required_wcvp_fields,
      drop = FALSE
    ],
    temporary = TRUE,
    overwrite = TRUE
  )
  
  rm(wcvp_names)
  gc()
  
  # ---------------------------------------------------------------------------
  # Complete WCVP ID lookup
  # ---------------------------------------------------------------------------
  
  DBI::dbExecute(
    con,
    "
    CREATE OR REPLACE TEMP TABLE wcvp_04m_id_lookup AS

    SELECT
      TRY_CAST(
        plant_name_id AS BIGINT
      ) AS plant_name_id,

      taxon_name,
      taxon_authors,
      taxon_rank,
      taxon_status,

      TRY_CAST(
        accepted_plant_name_id AS BIGINT
      ) AS accepted_plant_name_id,

      family,

      LOWER(
        REGEXP_EXTRACT(
          COALESCE(taxon_name, ''),
          '^([^ ]+)',
          1
        )
      ) AS parsed_genus,

      LOWER(
        REGEXP_EXTRACT(
          COALESCE(taxon_name, ''),
          '^[^ ]+[[:space:]]+([^ ]+)',
          1
        )
      ) AS parsed_species_epithet,

      LOWER(
        REGEXP_EXTRACT(
          COALESCE(taxon_name, ''),
          '(?:subsp\\.|ssp\\.|var\\.|f\\.)[[:space:]]+([^ ]+)',
          1
        )
      ) AS parsed_infra_epithet

    FROM wcvp_04m_source

    WHERE TRY_CAST(
      plant_name_id AS BIGINT
    ) IS NOT NULL
    "
  )
  
  # ---------------------------------------------------------------------------
  # WCVP infraspecific nomenclatural index
  # ---------------------------------------------------------------------------
  
  DBI::dbExecute(
    con,
    "
    CREATE OR REPLACE TEMP TABLE wcvp_04m_infra_index AS

    SELECT
      plant_name_id AS wcvp_plant_name_id,
      taxon_name AS wcvp_taxon_name,
      taxon_authors AS wcvp_taxon_authors,
      taxon_rank AS wcvp_taxon_rank,
      taxon_status AS wcvp_taxon_status,
      accepted_plant_name_id AS wcvp_accepted_plant_name_id,
      family AS wcvp_family,

      LOWER(
        TRIM(
          REGEXP_REPLACE(
            COALESCE(taxon_name, ''),
            '\\s+',
            ' ',
            'g'
          )
        )
      ) AS norm_wcvp_name,

      parsed_genus AS wcvp_genus,
      parsed_species_epithet AS wcvp_species_epithet,
      parsed_infra_epithet AS wcvp_infra_epithet

    FROM wcvp_04m_id_lookup

    WHERE UPPER(
      COALESCE(taxon_rank, '')
    ) IN (
      'SUBSPECIES',
      'VARIETY',
      'FORM'
    )

      AND accepted_plant_name_id IS NOT NULL
    "
  )
  
  # ---------------------------------------------------------------------------
  # Candidate evidence
  #
  # M1 = exact normalized full nomenclatural name
  # M2 = same genus + species epithet + infra epithet, any infra rank
  # M3 = GBIF species concept + same infra epithet
  # M4 = same family + infra epithet (weak diagnostic only)
  # ---------------------------------------------------------------------------
  
  DBI::dbExecute(
    con,
    "
    CREATE TABLE occurrence_wcvp_infraspecific_candidates AS

    SELECT DISTINCT
      i.vpjd_taxon_universe_id,
      'M1_EXACT_FULL_NAME'::VARCHAR AS diagnostic_rule,
      w.wcvp_plant_name_id,
      w.wcvp_taxon_name,
      w.wcvp_taxon_authors,
      w.wcvp_taxon_rank,
      w.wcvp_taxon_status,
      w.wcvp_accepted_plant_name_id,
      w.wcvp_family

    FROM infra_input i

    INNER JOIN wcvp_04m_infra_index w
      ON i.norm_scientific_name =
         w.norm_wcvp_name

    UNION ALL

    SELECT DISTINCT
      i.vpjd_taxon_universe_id,
      'M2_SAME_GENUS_SPECIES_INFRA_ANY_RANK',
      w.wcvp_plant_name_id,
      w.wcvp_taxon_name,
      w.wcvp_taxon_authors,
      w.wcvp_taxon_rank,
      w.wcvp_taxon_status,
      w.wcvp_accepted_plant_name_id,
      w.wcvp_family

    FROM infra_input i

    INNER JOIN wcvp_04m_infra_index w
      ON i.parsed_genus =
         w.wcvp_genus

     AND i.parsed_species_epithet =
         w.wcvp_species_epithet

     AND i.parsed_infra_epithet =
         w.wcvp_infra_epithet

    WHERE i.parsed_infra_epithet IS NOT NULL
      AND i.parsed_infra_epithet <> ''

    UNION ALL

    SELECT DISTINCT
      i.vpjd_taxon_universe_id,
      'M3_GBIF_PARENT_PLUS_INFRA',
      w.wcvp_plant_name_id,
      w.wcvp_taxon_name,
      w.wcvp_taxon_authors,
      w.wcvp_taxon_rank,
      w.wcvp_taxon_status,
      w.wcvp_accepted_plant_name_id,
      w.wcvp_family

    FROM infra_input i

    INNER JOIN wcvp_04m_infra_index w
      ON i.norm_species =
         w.wcvp_genus || ' ' ||
         w.wcvp_species_epithet

     AND i.parsed_infra_epithet =
         w.wcvp_infra_epithet

    WHERE i.parsed_infra_epithet IS NOT NULL
      AND i.parsed_infra_epithet <> ''

    UNION ALL

    SELECT DISTINCT
      i.vpjd_taxon_universe_id,
      'M4_FAMILY_INFRA_EPITHET_DIAGNOSTIC',
      w.wcvp_plant_name_id,
      w.wcvp_taxon_name,
      w.wcvp_taxon_authors,
      w.wcvp_taxon_rank,
      w.wcvp_taxon_status,
      w.wcvp_accepted_plant_name_id,
      w.wcvp_family

    FROM infra_input i

    INNER JOIN wcvp_04m_infra_index w
      ON LOWER(
           COALESCE(i.family, '')
         ) =
         LOWER(
           COALESCE(w.wcvp_family, '')
         )

     AND i.parsed_infra_epithet =
         w.wcvp_infra_epithet

    WHERE i.parsed_infra_epithet IS NOT NULL
      AND i.parsed_infra_epithet <> ''
    "
  )
  
  # ---------------------------------------------------------------------------
  # Strong candidate summary
  #
  # M4 is deliberately excluded from strong evidence.
  # ---------------------------------------------------------------------------
  
  DBI::dbExecute(
    con,
    "
    CREATE OR REPLACE TEMP TABLE infra_strong_summary AS

    SELECT
      vpjd_taxon_universe_id,

      COUNT(*) AS strong_candidate_rows,

      COUNT(
        DISTINCT wcvp_plant_name_id
      ) AS strong_nomenclatural_name_count,

      COUNT(
        DISTINCT wcvp_accepted_plant_name_id
      ) AS strong_accepted_concept_count,

      MIN(
        wcvp_accepted_plant_name_id
      ) AS strong_candidate_accepted_id,

      STRING_AGG(
        DISTINCT diagnostic_rule,
        '; ' ORDER BY diagnostic_rule
      ) AS strong_supporting_rules,

      SUM(
        CASE
          WHEN diagnostic_rule =
               'M1_EXACT_FULL_NAME'
          THEN 1 ELSE 0
        END
      ) AS m1_rows,

      SUM(
        CASE
          WHEN diagnostic_rule =
               'M2_SAME_GENUS_SPECIES_INFRA_ANY_RANK'
          THEN 1 ELSE 0
        END
      ) AS m2_rows,

      SUM(
        CASE
          WHEN diagnostic_rule =
               'M3_GBIF_PARENT_PLUS_INFRA'
          THEN 1 ELSE 0
        END
      ) AS m3_rows

    FROM occurrence_wcvp_infraspecific_candidates

    WHERE diagnostic_rule <>
          'M4_FAMILY_INFRA_EPITHET_DIAGNOSTIC'

    GROUP BY vpjd_taxon_universe_id
    "
  )
  
  # ---------------------------------------------------------------------------
  # Weak M4 summary
  # ---------------------------------------------------------------------------
  
  DBI::dbExecute(
    con,
    "
    CREATE OR REPLACE TEMP TABLE infra_weak_summary AS

    SELECT
      vpjd_taxon_universe_id,

      COUNT(*) AS m4_rows,

      COUNT(
        DISTINCT wcvp_accepted_plant_name_id
      ) AS m4_accepted_concept_count

    FROM occurrence_wcvp_infraspecific_candidates

    WHERE diagnostic_rule =
          'M4_FAMILY_INFRA_EPITHET_DIAGNOSTIC'

    GROUP BY vpjd_taxon_universe_id
    "
  )
  
  # ---------------------------------------------------------------------------
  # Unique target lookup
  # ---------------------------------------------------------------------------
  
  DBI::dbExecute(
    con,
    "
    CREATE OR REPLACE TEMP TABLE infra_unique_target AS

    SELECT
      s.vpjd_taxon_universe_id,
      s.strong_candidate_accepted_id,

      t.taxon_name AS target_wcvp_taxon_name,
      t.taxon_authors AS target_wcvp_taxon_authors,
      t.taxon_rank AS target_wcvp_taxon_rank,
      t.taxon_status AS target_wcvp_taxon_status,
      t.family AS target_wcvp_family,

      t.parsed_genus AS target_genus,
      t.parsed_species_epithet
        AS target_species_epithet,

      t.parsed_infra_epithet
        AS target_infra_epithet

    FROM infra_strong_summary s

    LEFT JOIN wcvp_04m_id_lookup t
      ON s.strong_accepted_concept_count = 1

     AND s.strong_candidate_accepted_id =
         t.plant_name_id
    "
  )
  
  # ---------------------------------------------------------------------------
  # Hardened concept comparison
  #
  # Conservative outcomes:
  #
  # A. accepted target remains the same trinomial concept:
  #      genus + species epithet + infra epithet unchanged
  #
  # B. accepted target is the GBIF species concept itself:
  #      the infraspecific name has been synonymised into the same parent
  #      species already represented by GBIF.
  #
  # Anything else with a unique WCVP target is retained for review.
  # ---------------------------------------------------------------------------
  
  DBI::dbExecute(
    con,
    "
    CREATE TABLE occurrence_wcvp_infraspecific_diagnostics AS

    SELECT
      i.*,

      COALESCE(
        s.strong_candidate_rows,
        0
      ) AS strong_candidate_rows,

      COALESCE(
        s.strong_nomenclatural_name_count,
        0
      ) AS strong_nomenclatural_name_count,

      COALESCE(
        s.strong_accepted_concept_count,
        0
      ) AS strong_accepted_concept_count,

      s.strong_candidate_accepted_id,
      s.strong_supporting_rules,

      COALESCE(
        s.m1_rows,
        0
      ) AS m1_rows,

      COALESCE(
        s.m2_rows,
        0
      ) AS m2_rows,

      COALESCE(
        s.m3_rows,
        0
      ) AS m3_rows,

      COALESCE(
        w.m4_rows,
        0
      ) AS m4_rows,

      COALESCE(
        w.m4_accepted_concept_count,
        0
      ) AS m4_accepted_concept_count,

      u.target_wcvp_taxon_name,
      u.target_wcvp_taxon_authors,
      u.target_wcvp_taxon_rank,
      u.target_wcvp_taxon_status,
      u.target_wcvp_family,

      u.target_genus,
      u.target_species_epithet,
      u.target_infra_epithet,

      CASE
        WHEN s.strong_accepted_concept_count = 1
         AND u.target_genus =
             i.parsed_genus
         AND u.target_species_epithet =
             i.parsed_species_epithet
         AND u.target_infra_epithet =
             i.parsed_infra_epithet
        THEN TRUE
        ELSE FALSE
      END AS same_trinomial_concept,

      CASE
        WHEN s.strong_accepted_concept_count = 1
         AND LOWER(
               COALESCE(
                 u.target_wcvp_taxon_name,
                 ''
               )
             ) =
             i.norm_species
        THEN TRUE
        ELSE FALSE
      END AS target_equals_gbif_species,

      CASE

        WHEN i.parsed_infra_epithet IS NULL
          OR i.parsed_infra_epithet = ''
        THEN 'parse_failure'

        WHEN COALESCE(
               s.strong_accepted_concept_count,
               0
             ) > 1
        THEN 'multiple_strong_wcvp_concepts'

        WHEN s.strong_accepted_concept_count = 1
          AND u.target_wcvp_taxon_name IS NULL
        THEN 'wcvp_target_record_missing'

        WHEN s.strong_accepted_concept_count = 1
          AND UPPER(
                COALESCE(
                  u.target_wcvp_taxon_status,
                  ''
                )
              ) <> 'ACCEPTED'
        THEN 'review_nonstandard_wcvp_target'

        WHEN s.strong_accepted_concept_count = 1
          AND u.target_genus =
              i.parsed_genus
          AND u.target_species_epithet =
              i.parsed_species_epithet
          AND u.target_infra_epithet =
              i.parsed_infra_epithet
        THEN 'secure_same_infraspecific_concept'

        WHEN s.strong_accepted_concept_count = 1
          AND LOWER(
                COALESCE(
                  u.target_wcvp_taxon_name,
                  ''
                )
              ) =
              i.norm_species
        THEN 'secure_synonym_to_gbif_parent_species'

        WHEN s.strong_accepted_concept_count = 1
        THEN 'review_changed_accepted_concept'

        WHEN COALESCE(
               s.strong_accepted_concept_count,
               0
             ) = 0
          AND COALESCE(
                w.m4_rows,
                0
              ) > 0
        THEN 'family_epithet_evidence_only'

        ELSE 'no_wcvp_infraspecific_evidence'

      END AS infra_diagnostic_class

    FROM infra_input i

    LEFT JOIN infra_strong_summary s
      ON i.vpjd_taxon_universe_id =
         s.vpjd_taxon_universe_id

    LEFT JOIN infra_weak_summary w
      ON i.vpjd_taxon_universe_id =
         w.vpjd_taxon_universe_id

    LEFT JOIN infra_unique_target u
      ON i.vpjd_taxon_universe_id =
         u.vpjd_taxon_universe_id
    "
  )
  
  # ---------------------------------------------------------------------------
  # Summaries
  # ---------------------------------------------------------------------------
  
  diagnostic_summary <- DBI::dbGetQuery(
    con,
    "
    SELECT
      infra_diagnostic_class,
      COUNT(*) AS concepts,
      SUM(present_records) AS present_records

    FROM occurrence_wcvp_infraspecific_diagnostics

    GROUP BY infra_diagnostic_class

    ORDER BY present_records DESC
    "
  )
  
  rank_class_summary <- DBI::dbGetQuery(
    con,
    "
    SELECT
      taxonRank,
      infra_diagnostic_class,
      COUNT(*) AS concepts,
      SUM(present_records) AS present_records

    FROM occurrence_wcvp_infraspecific_diagnostics

    GROUP BY
      taxonRank,
      infra_diagnostic_class

    ORDER BY
      taxonRank,
      present_records DESC
    "
  )
  
  rule_summary <- DBI::dbGetQuery(
    con,
    "
    SELECT
      diagnostic_rule,

      COUNT(
        DISTINCT vpjd_taxon_universe_id
      ) AS concepts,

      COUNT(*) AS candidate_rows,

      COUNT(
        DISTINCT wcvp_accepted_plant_name_id
      ) AS accepted_concepts

    FROM occurrence_wcvp_infraspecific_candidates

    GROUP BY diagnostic_rule

    ORDER BY concepts DESC
    "
  )
  
  secure_evidence <- DBI::dbGetQuery(
    con,
    "
    SELECT *

    FROM occurrence_wcvp_infraspecific_diagnostics

    WHERE infra_diagnostic_class IN (
      'secure_same_infraspecific_concept',
      'secure_synonym_to_gbif_parent_species'
    )

    ORDER BY
      present_records DESC,
      scientificName
    "
  )
  
  review_evidence <- DBI::dbGetQuery(
    con,
    "
    SELECT *

    FROM occurrence_wcvp_infraspecific_diagnostics

    WHERE infra_diagnostic_class IN (
      'review_changed_accepted_concept',
      'review_nonstandard_wcvp_target',
      'wcvp_target_record_missing'
    )

    ORDER BY
      present_records DESC,
      scientificName
    "
  )
  
  ambiguous_evidence <- DBI::dbGetQuery(
    con,
    "
    SELECT *

    FROM occurrence_wcvp_infraspecific_diagnostics

    WHERE infra_diagnostic_class =
          'multiple_strong_wcvp_concepts'

    ORDER BY
      present_records DESC,
      scientificName
    "
  )
  
  weak_evidence <- DBI::dbGetQuery(
    con,
    "
    SELECT *

    FROM occurrence_wcvp_infraspecific_diagnostics

    WHERE infra_diagnostic_class =
          'family_epithet_evidence_only'

    ORDER BY
      present_records DESC,
      scientificName
    "
  )
  
  no_evidence <- DBI::dbGetQuery(
    con,
    "
    SELECT *

    FROM occurrence_wcvp_infraspecific_diagnostics

    WHERE infra_diagnostic_class IN (
      'no_wcvp_infraspecific_evidence',
      'parse_failure'
    )

    ORDER BY
      present_records DESC,
      scientificName
    "
  )
  
  # ---------------------------------------------------------------------------
  # Candidate audit for unique targets
  # ---------------------------------------------------------------------------
  
  unique_candidate_detail <- DBI::dbGetQuery(
    con,
    "
    SELECT
      d.vpjd_taxon_universe_id,
      d.taxonKey,
      d.scientificName,
      d.family,
      d.species,
      d.taxonRank,
      d.present_records,

      d.parsed_genus,
      d.parsed_species_epithet,
      d.parsed_infra_epithet,

      d.infra_diagnostic_class,

      d.strong_candidate_accepted_id,

      d.target_wcvp_taxon_name,
      d.target_wcvp_taxon_authors,
      d.target_wcvp_taxon_rank,
      d.target_wcvp_taxon_status,

      d.same_trinomial_concept,
      d.target_equals_gbif_species,

      c.diagnostic_rule,
      c.wcvp_plant_name_id,
      c.wcvp_taxon_name,
      c.wcvp_taxon_authors,
      c.wcvp_taxon_rank,
      c.wcvp_taxon_status,
      c.wcvp_accepted_plant_name_id

    FROM occurrence_wcvp_infraspecific_diagnostics d

    INNER JOIN occurrence_wcvp_infraspecific_candidates c
      ON d.vpjd_taxon_universe_id =
         c.vpjd_taxon_universe_id

    WHERE d.strong_accepted_concept_count = 1

      AND c.diagnostic_rule <>
          'M4_FAMILY_INFRA_EPITHET_DIAGNOSTIC'

    ORDER BY
      d.present_records DESC,
      d.scientificName,
      c.diagnostic_rule,
      c.wcvp_taxon_name
    "
  )
  
  # ---------------------------------------------------------------------------
  # Baseline safety check
  # ---------------------------------------------------------------------------
  
  baseline_after <- DBI::dbGetQuery(
    con,
    "
    SELECT
      COUNT(*) AS total_concepts,

      SUM(
        CASE
          WHEN hybrid_final_wcvp_accepted_plant_name_id IS NOT NULL
          THEN 1 ELSE 0
        END
      ) AS resolved_after,

      COUNT(
        DISTINCT hybrid_final_wcvp_accepted_plant_name_id
      ) AS distinct_accepted_after,

      SUM(
        CASE
          WHEN hybrid_final_wcvp_accepted_plant_name_id IS NULL
          THEN 1 ELSE 0
        END
      ) AS unresolved_after

    FROM occurrence_wcvp_reconciliation_hybrid
    "
  )
  
  if (
    baseline_after$total_concepts[[1]] !=
    baseline$total_concepts[[1]] ||
    
    baseline_after$resolved_after[[1]] !=
    baseline$resolved_before_04m[[1]] ||
    
    baseline_after$distinct_accepted_after[[1]] !=
    baseline$distinct_accepted_before_04m[[1]] ||
    
    baseline_after$unresolved_after[[1]] !=
    baseline$unresolved_before_04m[[1]]
  ) {
    stop(
      "04m safety check failed: secure reconciliation baseline changed."
    )
  }
  
  # ---------------------------------------------------------------------------
  # Metadata
  # ---------------------------------------------------------------------------
  
  DBI::dbExecute(
    con,
    paste0(
      "
      CREATE TABLE occurrence_wcvp_infraspecific_diagnostic_metadata AS

      SELECT
        CURRENT_TIMESTAMP AS created_at,

        '",
      VPJD_WCVP_INFRA_DIAG_VERSION,
      "'::VARCHAR AS script_version,

        ",
      baseline$resolved_before_04m[[1]],
      "::BIGINT AS resolved_baseline,

        ",
      baseline$distinct_accepted_before_04m[[1]],
      "::BIGINT AS distinct_accepted_baseline,

        ",
      baseline$unresolved_before_04m[[1]],
      "::BIGINT AS unresolved_baseline,

        COUNT(*) AS target_concepts,

        SUM(
          present_records
        ) AS target_present_records

      FROM occurrence_wcvp_infraspecific_diagnostics
      "
    )
  )
  
  # ---------------------------------------------------------------------------
  # Exports
  # ---------------------------------------------------------------------------
  
  full_diagnostics <- DBI::dbGetQuery(
    con,
    "
    SELECT *

    FROM occurrence_wcvp_infraspecific_diagnostics

    ORDER BY
      present_records DESC,
      scientificName
    "
  )
  
  exports <- list(
    infraspecific_diagnostics =
      full_diagnostics,
    
    infraspecific_diagnostic_summary =
      diagnostic_summary,
    
    infraspecific_rank_class_summary =
      rank_class_summary,
    
    infraspecific_rule_summary =
      rule_summary,
    
    infraspecific_secure_evidence =
      secure_evidence,
    
    infraspecific_review_evidence =
      review_evidence,
    
    infraspecific_ambiguous_evidence =
      ambiguous_evidence,
    
    infraspecific_weak_evidence =
      weak_evidence,
    
    infraspecific_no_evidence =
      no_evidence,
    
    infraspecific_unique_candidate_detail =
      unique_candidate_detail
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
    "Hardened residual infraspecific diagnostics complete"
  )
  
  cli::cli_text(
    "{.strong Target concepts:} ",
    "{fmt(input_total$concepts[[1]])}"
  )
  
  cli::cli_text(
    "{.strong PRESENT records represented:} ",
    "{fmt(input_total$present_records[[1]])}"
  )
  
  cli::cli_text(
    "{.strong Secure concepts:} ",
    "{fmt(nrow(secure_evidence))}"
  )
  
  cli::cli_text(
    "{.strong Review concepts:} ",
    "{fmt(nrow(review_evidence))}"
  )
  
  cli::cli_text(
    "{.strong Ambiguous concepts:} ",
    "{fmt(nrow(ambiguous_evidence))}"
  )
  
  cli::cli_text(
    "{.strong Weak family/epithet-only concepts:} ",
    "{fmt(nrow(weak_evidence))}"
  )
  
  cli::cli_text(
    "{.strong No-evidence/parse-failure concepts:} ",
    "{fmt(nrow(no_evidence))}"
  )
  
  cli::cli_alert_success(
    "04m v0.1.1 is diagnostic only: no WCVP assignments were made."
  )
  
  cli::cli_alert_success(
    "04l and all upstream reconciliation tables remain unchanged."
  )
  
  cli::cli_text(
    "04m_occurrence_wcvp_infraspecific_diagnostics.R ",
    "v{VPJD_WCVP_INFRA_DIAG_VERSION} complete."
  )
  
  invisible(
    list(
      baseline =
        baseline,
      
      input_summary =
        input_summary,
      
      input_total =
        input_total,
      
      diagnostic_summary =
        diagnostic_summary,
      
      rank_class_summary =
        rank_class_summary,
      
      rule_summary =
        rule_summary,
      
      secure_evidence =
        secure_evidence,
      
      review_evidence =
        review_evidence,
      
      ambiguous_evidence =
        ambiguous_evidence,
      
      weak_evidence =
        weak_evidence,
      
      no_evidence =
        no_evidence,
      
      unique_candidate_detail =
        unique_candidate_detail
    )
  )
}

wcvp_infra_diag_results <-
  run_occurrence_wcvp_infraspecific_diagnostics()

infra_input_summary <-
  wcvp_infra_diag_results$input_summary

infra_input_total <-
  wcvp_infra_diag_results$input_total

infra_diagnostic_summary <-
  wcvp_infra_diag_results$diagnostic_summary

infra_rank_class_summary <-
  wcvp_infra_diag_results$rank_class_summary

infra_rule_summary <-
  wcvp_infra_diag_results$rule_summary

infra_secure_evidence <-
  wcvp_infra_diag_results$secure_evidence

infra_review_evidence <-
  wcvp_infra_diag_results$review_evidence

infra_ambiguous_evidence <-
  wcvp_infra_diag_results$ambiguous_evidence

infra_weak_evidence <-
  wcvp_infra_diag_results$weak_evidence

infra_no_evidence <-
  wcvp_infra_diag_results$no_evidence

infra_unique_candidate_detail <-
  wcvp_infra_diag_results$unique_candidate_detail