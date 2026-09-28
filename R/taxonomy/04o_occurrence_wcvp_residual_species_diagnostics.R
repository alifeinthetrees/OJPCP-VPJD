# ==============================================================================
# 04o_occurrence_wcvp_residual_species_diagnostics.R
# VPJD / Oxford-Japan Plant Conservation Partnership
#
# Purpose:
#   Hardened diagnostic of the 351 residual unresolved SPECIES concepts
#   remaining after 04n.
#
# Evidence hierarchy:
#   O1 = exact GBIF species field == WCVP taxon_name
#        Primary nomenclatural evidence.
#
#   O2 = same genus + species epithet
#        Supporting diagnostic evidence ONLY.
#        Never sufficient for automatic recovery.
#
#   O3 = same family + species epithet
#        Weak diagnostic evidence ONLY.
#
#   O4 = same species epithet globally
#        Weak diagnostic evidence ONLY.
#
#   O5 = normalized authorship agreement within O1 exact-name candidates
#        Diagnostic discriminator for homonyms/nomenclatural ambiguity.
#
# IMPORTANT:
#   DIAGNOSTIC ONLY.
#   No assignments are made.
#   No fuzzy matching.
#   No edit-distance matching.
#   No new spelling transformations.
#   O2/O3/O4 are never treated as secure recovery evidence.
#   WCVP Unplaced / Artificial Hybrid / other non-Accepted targets are not
#   treated as secure.
#   04n and all upstream reconciliation tables remain unchanged.
#
# Expected input:
#   351 unresolved SPECIES concepts
#   18,253 PRESENT records
#
# Version: 0.1.1
# ==============================================================================

VPJD_WCVP_SPECIES_DIAG_VERSION <- "0.1.1"

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

run_occurrence_wcvp_residual_species_diagnostics <- function() {
  
  duckdb_path <- here::here(
    "data","interim","occurrences","vpjd_occurrences.duckdb"
  )
  
  output_dir <- here::here(
    "outputs","tables","taxonomy",
    "occurrence_wcvp_residual_species_diagnostics"
  )
  
  dir.create(
    output_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  if (!file.exists(duckdb_path)) {
    stop(
      "VPJD occurrence DuckDB not found:\n",
      duckdb_path
    )
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
  
  fmt <- function(x) {
    format(
      x,
      big.mark = ",",
      scientific = FALSE,
      trim = TRUE
    )
  }
  
  cli::cli_h1(
    "VPJD hardened WCVP residual species diagnostics"
  )
  
  # ---------------------------------------------------------------------------
  # Required frozen 04n input
  # ---------------------------------------------------------------------------
  
  required_tables <- c(
    "occurrence_wcvp_reconciliation_infraspecific",
    "occurrence_wcvp_infraspecific_recovery_metadata"
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
  
  metadata_04n <- DBI::dbGetQuery(
    con,
    "
    SELECT *
    FROM occurrence_wcvp_infraspecific_recovery_metadata
    "
  )
  
  if (nrow(metadata_04n) != 1L) {
    stop(
      "Expected exactly one row in 04n recovery metadata."
    )
  }
  
  if (
    is.na(metadata_04n$script_version[[1]]) ||
    metadata_04n$script_version[[1]] != "0.1.0"
  ) {
    stop(
      "04o requires frozen 04n version 0.1.0. Found: ",
      metadata_04n$script_version[[1]]
    )
  }
  
  # ---------------------------------------------------------------------------
  # Validate authoritative 04n baseline
  # ---------------------------------------------------------------------------
  
  baseline <- DBI::dbGetQuery(
    con,
    "
    SELECT
      COUNT(*) AS total_concepts,
      SUM(
        CASE
          WHEN infraspecific_final_wcvp_accepted_plant_name_id
               IS NOT NULL
          THEN 1 ELSE 0
        END
      ) AS resolved_before_04o,
      COUNT(
        DISTINCT
        infraspecific_final_wcvp_accepted_plant_name_id
      ) AS distinct_accepted_before_04o,
      SUM(
        CASE
          WHEN infraspecific_final_wcvp_accepted_plant_name_id
               IS NULL
          THEN 1 ELSE 0
        END
      ) AS unresolved_before_04o
    FROM occurrence_wcvp_reconciliation_infraspecific
    "
  )
  
  if (
    baseline$total_concepts[[1]] != 21165L ||
    baseline$resolved_before_04o[[1]] != 19682L ||
    baseline$distinct_accepted_before_04o[[1]] != 12053L ||
    baseline$unresolved_before_04o[[1]] != 1483L
  ) {
    stop(
      "04n baseline differs from validated/frozen state. Expected ",
      "21,165 total / 19,682 resolved / 12,053 distinct accepted / ",
      "1,483 unresolved."
    )
  }
  
  cli::cli_text(
    "{.strong Resolved baseline:} ",
    "{fmt(baseline$resolved_before_04o[[1]])}"
  )
  
  cli::cli_text(
    "{.strong Distinct accepted baseline:} ",
    "{fmt(baseline$distinct_accepted_before_04o[[1]])}"
  )
  
  cli::cli_text(
    "{.strong Unresolved baseline:} ",
    "{fmt(baseline$unresolved_before_04o[[1]])}"
  )
  
  # ---------------------------------------------------------------------------
  # Remove only 04o-owned persistent products
  # ---------------------------------------------------------------------------
  
  for (tbl in c(
    "occurrence_wcvp_residual_species_diagnostic_metadata",
    "occurrence_wcvp_residual_species_diagnostics",
    "occurrence_wcvp_residual_species_candidates"
  )) {
    DBI::dbExecute(
      con,
      paste0(
        "DROP TABLE IF EXISTS ",
        tbl
      )
    )
  }
  
  # ---------------------------------------------------------------------------
  # Residual species input
  #
  # GBIF species is preferred for the binomial because scientificName commonly
  # contains authorship.
  #
  # scientificName is separately normalized for authorship diagnostics.
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
          COALESCE(species, scientificName, ''),
          '^([^ ]+)',
          1
        )
      ) AS parsed_genus,

      LOWER(
        REGEXP_EXTRACT(
          COALESCE(species, scientificName, ''),
          '^[^ ]+[[:space:]]+([^ ]+)',
          1
        )
      ) AS parsed_species_epithet,

      TRIM(
        REGEXP_REPLACE(
          COALESCE(scientificName, ''),
          '^[^ ]+[[:space:]]+[^ ]+[[:space:]]*',
          ''
        )
      ) AS source_authorship_raw

    FROM occurrence_wcvp_reconciliation_infraspecific

    WHERE
      infraspecific_final_wcvp_accepted_plant_name_id IS NULL
      AND UPPER(COALESCE(taxonRank, '')) = 'SPECIES'
    "
  )
  
  # ---------------------------------------------------------------------------
  # Explicit pseudo-species / malformed-binomial classification
  #
  # Important:
  #   A valid species concept requires a usable genus and species epithet.
  #   Historical GBIF values such as "Acer spec" are deliberately isolated.
  # ---------------------------------------------------------------------------
  
  DBI::dbExecute(
    con,
    "
    CREATE OR REPLACE TEMP TABLE species_input_classified AS

    SELECT
      *,

      CASE
        WHEN parsed_genus IS NULL
          OR parsed_genus = ''
          OR parsed_species_epithet IS NULL
          OR parsed_species_epithet = ''
        THEN TRUE

        WHEN LOWER(parsed_species_epithet) IN (
          'sp',
          'sp.',
          'spec',
          'spec.',
          'spp',
          'spp.',
          'indet',
          'indet.',
          'unknown',
          'unidentified',
          '×',
          'x'
        )
        THEN TRUE

        ELSE FALSE
      END AS pseudo_species_flag,

      LOWER(
        REGEXP_REPLACE(
          REGEXP_REPLACE(
            REGEXP_REPLACE(
              COALESCE(source_authorship_raw, ''),
              '[0-9]{4}',
              '',
              'g'
            ),
            '[^[:alnum:]]+',
            '',
            'g'
          ),
          '[[:space:]]+',
          '',
          'g'
        )
      ) AS norm_source_authorship

    FROM species_input
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
          WHEN pseudo_species_flag
          THEN 1 ELSE 0
        END
      ) AS pseudo_species_concepts,
      SUM(
        CASE
          WHEN pseudo_species_flag
          THEN present_records ELSE 0
        END
      ) AS pseudo_species_present_records
    FROM species_input_classified
    "
  )
  
  if (
    input_total$concepts[[1]] != 351L ||
    input_total$present_records[[1]] != 18253L
  ) {
    stop(
      "04o input differs from expected frozen 04n residual. Expected ",
      "351 SPECIES concepts / 18,253 PRESENT records."
    )
  }
  
  cli::cli_text(
    "{.strong Residual species concepts:} ",
    "{fmt(input_total$concepts[[1]])}"
  )
  
  cli::cli_text(
    "{.strong PRESENT records represented:} ",
    "{fmt(input_total$present_records[[1]])}"
  )
  
  cli::cli_text(
    "{.strong Pseudo-species / malformed concepts:} ",
    "{fmt(input_total$pseudo_species_concepts[[1]])}"
  )
  
  # ---------------------------------------------------------------------------
  # Complete WCVP nomenclatural source
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
      paste(
        missing_wcvp_fields,
        collapse = ", "
      )
    )
  }
  
  DBI::dbWriteTable(
    con,
    "wcvp_04o_source",
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
  # WCVP lookup
  #
  # accepted_concept_id:
  #   Accepted records may be self-referential or may require plant_name_id.
  #   COALESCE therefore protects both representations.
  # ---------------------------------------------------------------------------
  
  DBI::dbExecute(
    con,
    "
    CREATE OR REPLACE TEMP TABLE wcvp_04o_lookup AS

    SELECT
      TRY_CAST(plant_name_id AS BIGINT)
        AS plant_name_id,

      taxon_name,
      taxon_authors,
      taxon_rank,
      taxon_status,

      TRY_CAST(
        accepted_plant_name_id AS BIGINT
      ) AS accepted_plant_name_id,

      CASE
        WHEN UPPER(COALESCE(taxon_status, '')) = 'ACCEPTED'
        THEN COALESCE(
          TRY_CAST(accepted_plant_name_id AS BIGINT),
          TRY_CAST(plant_name_id AS BIGINT)
        )
        ELSE TRY_CAST(
          accepted_plant_name_id AS BIGINT
        )
      END AS accepted_concept_id,

      family,

      LOWER(
        TRIM(
          REGEXP_REPLACE(
            COALESCE(taxon_name, ''),
            '\\s+',
            ' ',
            'g'
          )
        )
      ) AS norm_taxon_name,

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
        REGEXP_REPLACE(
          REGEXP_REPLACE(
            REGEXP_REPLACE(
              COALESCE(taxon_authors, ''),
              '[0-9]{4}',
              '',
              'g'
            ),
            '[^[:alnum:]]+',
            '',
            'g'
          ),
          '[[:space:]]+',
          '',
          'g'
        )
      ) AS norm_taxon_authors

    FROM wcvp_04o_source

    WHERE
      TRY_CAST(plant_name_id AS BIGINT)
      IS NOT NULL
    "
  )
  
  # ---------------------------------------------------------------------------
  # Candidate evidence
  #
  # O1 = exact species/binomial name
  # O2 = same genus + epithet, diagnostic only
  # O3 = same family + epithet, diagnostic only
  # O4 = same epithet globally, diagnostic only
  #
  # O5 is calculated later as authorship agreement among O1 candidates.
  # ---------------------------------------------------------------------------
  
  DBI::dbExecute(
    con,
    "
    CREATE TABLE occurrence_wcvp_residual_species_candidates AS

    SELECT DISTINCT
      i.vpjd_taxon_universe_id,
      'O1_EXACT_SPECIES_NAME'::VARCHAR
        AS diagnostic_rule,
      w.plant_name_id
        AS wcvp_plant_name_id,
      w.taxon_name
        AS wcvp_taxon_name,
      w.taxon_authors
        AS wcvp_taxon_authors,
      w.taxon_rank
        AS wcvp_taxon_rank,
      w.taxon_status
        AS wcvp_taxon_status,
      w.accepted_plant_name_id
        AS wcvp_accepted_plant_name_id,
      w.accepted_concept_id
        AS wcvp_accepted_concept_id,
      w.family
        AS wcvp_family,
      w.norm_taxon_authors
        AS norm_wcvp_authorship,
      CASE
        WHEN i.norm_source_authorship <> ''
         AND w.norm_taxon_authors <> ''
         AND i.norm_source_authorship =
             w.norm_taxon_authors
        THEN TRUE
        ELSE FALSE
      END AS authorship_exact

    FROM species_input_classified i

    INNER JOIN wcvp_04o_lookup w
      ON i.norm_species =
         w.norm_taxon_name

    WHERE
      NOT i.pseudo_species_flag

    UNION ALL

    SELECT DISTINCT
      i.vpjd_taxon_universe_id,
      'O2_SAME_GENUS_EPITHET',
      w.plant_name_id,
      w.taxon_name,
      w.taxon_authors,
      w.taxon_rank,
      w.taxon_status,
      w.accepted_plant_name_id,
      w.accepted_concept_id,
      w.family,
      w.norm_taxon_authors,
      CASE
        WHEN i.norm_source_authorship <> ''
         AND w.norm_taxon_authors <> ''
         AND i.norm_source_authorship =
             w.norm_taxon_authors
        THEN TRUE
        ELSE FALSE
      END

    FROM species_input_classified i

    INNER JOIN wcvp_04o_lookup w
      ON i.parsed_genus =
         w.parsed_genus
     AND i.parsed_species_epithet =
         w.parsed_species_epithet

    WHERE
      NOT i.pseudo_species_flag
      AND i.parsed_species_epithet <> ''

    UNION ALL

    SELECT DISTINCT
      i.vpjd_taxon_universe_id,
      'O3_SAME_FAMILY_EPITHET_DIAGNOSTIC',
      w.plant_name_id,
      w.taxon_name,
      w.taxon_authors,
      w.taxon_rank,
      w.taxon_status,
      w.accepted_plant_name_id,
      w.accepted_concept_id,
      w.family,
      w.norm_taxon_authors,
      FALSE

    FROM species_input_classified i

    INNER JOIN wcvp_04o_lookup w
      ON LOWER(COALESCE(i.family, '')) =
         LOWER(COALESCE(w.family, ''))
     AND i.parsed_species_epithet =
         w.parsed_species_epithet

    WHERE
      NOT i.pseudo_species_flag
      AND i.parsed_species_epithet <> ''

    UNION ALL

    SELECT DISTINCT
      i.vpjd_taxon_universe_id,
      'O4_SAME_EPITHET_ANY_GENUS_DIAGNOSTIC',
      w.plant_name_id,
      w.taxon_name,
      w.taxon_authors,
      w.taxon_rank,
      w.taxon_status,
      w.accepted_plant_name_id,
      w.accepted_concept_id,
      w.family,
      w.norm_taxon_authors,
      FALSE

    FROM species_input_classified i

    INNER JOIN wcvp_04o_lookup w
      ON i.parsed_species_epithet =
         w.parsed_species_epithet

    WHERE
      NOT i.pseudo_species_flag
      AND i.parsed_species_epithet <> ''
    "
  )
  
  # ---------------------------------------------------------------------------
  # O1 exact-name summary
  #
  # This is now the ONLY primary nomenclatural evidence.
  # ---------------------------------------------------------------------------
  
  DBI::dbExecute(
    con,
    "
    CREATE OR REPLACE TEMP TABLE species_o1_summary AS

    SELECT
      vpjd_taxon_universe_id,

      COUNT(*) AS o1_candidate_rows,

      COUNT(
        DISTINCT wcvp_plant_name_id
      ) AS o1_nomenclatural_name_count,

      COUNT(
        DISTINCT wcvp_accepted_concept_id
      ) AS o1_accepted_concept_count,

      MIN(
        wcvp_accepted_concept_id
      ) AS o1_candidate_accepted_id,

      SUM(
        CASE
          WHEN authorship_exact
          THEN 1 ELSE 0
        END
      ) AS o1_authorship_exact_rows,

      COUNT(
        DISTINCT CASE
          WHEN authorship_exact
          THEN wcvp_plant_name_id
        END
      ) AS o1_authorship_exact_names,

      COUNT(
        DISTINCT CASE
          WHEN authorship_exact
          THEN wcvp_accepted_concept_id
        END
      ) AS o1_authorship_accepted_concepts,

      MIN(
        CASE
          WHEN authorship_exact
          THEN wcvp_accepted_concept_id
          ELSE NULL
        END
      ) AS o1_authorship_candidate_accepted_id

    FROM occurrence_wcvp_residual_species_candidates

    WHERE
      diagnostic_rule =
      'O1_EXACT_SPECIES_NAME'

    GROUP BY
      vpjd_taxon_universe_id
    "
  )
  
  # ---------------------------------------------------------------------------
  # Supporting evidence summary
  # ---------------------------------------------------------------------------
  
  DBI::dbExecute(
    con,
    "
    CREATE OR REPLACE TEMP TABLE species_support_summary AS

    SELECT
      vpjd_taxon_universe_id,

      SUM(
        CASE
          WHEN diagnostic_rule =
            'O2_SAME_GENUS_EPITHET'
          THEN 1 ELSE 0
        END
      ) AS o2_rows,

      COUNT(
        DISTINCT CASE
          WHEN diagnostic_rule =
            'O2_SAME_GENUS_EPITHET'
          THEN wcvp_accepted_concept_id
        END
      ) AS o2_accepted_concepts,

      SUM(
        CASE
          WHEN diagnostic_rule =
            'O3_SAME_FAMILY_EPITHET_DIAGNOSTIC'
          THEN 1 ELSE 0
        END
      ) AS o3_rows,

      COUNT(
        DISTINCT CASE
          WHEN diagnostic_rule =
            'O3_SAME_FAMILY_EPITHET_DIAGNOSTIC'
          THEN wcvp_accepted_concept_id
        END
      ) AS o3_accepted_concepts,

      SUM(
        CASE
          WHEN diagnostic_rule =
            'O4_SAME_EPITHET_ANY_GENUS_DIAGNOSTIC'
          THEN 1 ELSE 0
        END
      ) AS o4_rows,

      COUNT(
        DISTINCT CASE
          WHEN diagnostic_rule =
            'O4_SAME_EPITHET_ANY_GENUS_DIAGNOSTIC'
          THEN wcvp_accepted_concept_id
        END
      ) AS o4_accepted_concepts

    FROM occurrence_wcvp_residual_species_candidates

    WHERE
      diagnostic_rule IN (
        'O2_SAME_GENUS_EPITHET',
        'O3_SAME_FAMILY_EPITHET_DIAGNOSTIC',
        'O4_SAME_EPITHET_ANY_GENUS_DIAGNOSTIC'
      )

    GROUP BY
      vpjd_taxon_universe_id
    "
  )
  
  # ---------------------------------------------------------------------------
  # Select deterministic candidate ID
  #
  # Priority:
  #   1. unique authorship-matched accepted concept within O1;
  #   2. otherwise unique accepted concept across all O1 records.
  #
  # This does NOT mean it will be recovered. Target status is checked below.
  # ---------------------------------------------------------------------------
  
  DBI::dbExecute(
    con,
    "
    CREATE OR REPLACE TEMP TABLE species_selected_target AS

    SELECT
      vpjd_taxon_universe_id,

      CASE
        WHEN o1_authorship_accepted_concepts = 1
        THEN o1_authorship_candidate_accepted_id

        WHEN o1_accepted_concept_count = 1
        THEN o1_candidate_accepted_id

        ELSE NULL
      END AS selected_accepted_id,

      CASE
        WHEN o1_authorship_accepted_concepts = 1
        THEN 'O1_EXACT_NAME_PLUS_AUTHORSHIP'

        WHEN o1_accepted_concept_count = 1
        THEN 'O1_EXACT_NAME_UNIQUE_ACCEPTED_CONCEPT'

        ELSE NULL
      END AS selected_evidence_route

    FROM species_o1_summary
    "
  )
  
  # ---------------------------------------------------------------------------
  # Accepted target lookup
  # ---------------------------------------------------------------------------
  
  DBI::dbExecute(
    con,
    "
    CREATE OR REPLACE TEMP TABLE species_target_lookup AS

    SELECT
      s.vpjd_taxon_universe_id,
      s.selected_accepted_id,
      s.selected_evidence_route,

      w.taxon_name
        AS target_wcvp_taxon_name,

      w.taxon_authors
        AS target_wcvp_taxon_authors,

      w.taxon_rank
        AS target_wcvp_taxon_rank,

      w.taxon_status
        AS target_wcvp_taxon_status,

      w.family
        AS target_wcvp_family,

      w.parsed_genus
        AS target_genus,

      w.parsed_species_epithet
        AS target_species_epithet

    FROM species_selected_target s

    LEFT JOIN wcvp_04o_lookup w
      ON s.selected_accepted_id =
         w.plant_name_id
    "
  )
  
  # ---------------------------------------------------------------------------
  # Hardened diagnostic classification
  # ---------------------------------------------------------------------------
  
  DBI::dbExecute(
    con,
    "
    CREATE TABLE occurrence_wcvp_residual_species_diagnostics AS

    SELECT
      i.*,

      COALESCE(o.o1_candidate_rows, 0)
        AS o1_candidate_rows,

      COALESCE(o.o1_nomenclatural_name_count, 0)
        AS o1_nomenclatural_name_count,

      COALESCE(o.o1_accepted_concept_count, 0)
        AS o1_accepted_concept_count,

      COALESCE(o.o1_authorship_exact_rows, 0)
        AS o1_authorship_exact_rows,

      COALESCE(o.o1_authorship_exact_names, 0)
        AS o1_authorship_exact_names,

      COALESCE(o.o1_authorship_accepted_concepts, 0)
        AS o1_authorship_accepted_concepts,

      COALESCE(s.o2_rows, 0)
        AS o2_rows,

      COALESCE(s.o2_accepted_concepts, 0)
        AS o2_accepted_concepts,

      COALESCE(s.o3_rows, 0)
        AS o3_rows,

      COALESCE(s.o3_accepted_concepts, 0)
        AS o3_accepted_concepts,

      COALESCE(s.o4_rows, 0)
        AS o4_rows,

      COALESCE(s.o4_accepted_concepts, 0)
        AS o4_accepted_concepts,

      t.selected_accepted_id,
      t.selected_evidence_route,

      t.target_wcvp_taxon_name,
      t.target_wcvp_taxon_authors,
      t.target_wcvp_taxon_rank,
      t.target_wcvp_taxon_status,
      t.target_wcvp_family,
      t.target_genus,
      t.target_species_epithet,

      CASE
        WHEN t.selected_accepted_id IS NOT NULL
         AND t.target_genus = i.parsed_genus
         AND t.target_species_epithet =
             i.parsed_species_epithet
        THEN TRUE
        ELSE FALSE
      END AS target_preserves_genus_epithet,

      CASE

        WHEN i.pseudo_species_flag
        THEN 'pseudo_species_or_placeholder'

        WHEN o.o1_authorship_accepted_concepts > 1
        THEN 'multiple_author_mapped_wcvp_concepts'

        WHEN o.o1_authorship_accepted_concepts = 0
         AND o.o1_accepted_concept_count > 1
        THEN 'multiple_exact_name_wcvp_concepts'

        WHEN t.selected_accepted_id IS NOT NULL
         AND t.target_wcvp_taxon_name IS NULL
        THEN 'wcvp_target_record_missing'

        WHEN t.selected_accepted_id IS NOT NULL
         AND UPPER(
           COALESCE(
             t.target_wcvp_taxon_status,
             ''
           )
         ) <> 'ACCEPTED'
        THEN 'review_nonstandard_wcvp_target'

        WHEN t.selected_accepted_id IS NOT NULL
         AND UPPER(
           COALESCE(
             t.target_wcvp_taxon_status,
             ''
           )
         ) = 'ACCEPTED'
         AND t.target_genus = i.parsed_genus
         AND t.target_species_epithet =
             i.parsed_species_epithet
        THEN 'secure_exact_species_concept'

        WHEN t.selected_accepted_id IS NOT NULL
         AND UPPER(
           COALESCE(
             t.target_wcvp_taxon_status,
             ''
           )
         ) = 'ACCEPTED'
        THEN 'review_exact_name_changed_accepted_concept'

        WHEN COALESCE(o.o1_candidate_rows, 0) > 0
        THEN 'exact_name_evidence_unresolved'

        WHEN COALESCE(s.o2_rows, 0) > 0
        THEN 'same_genus_epithet_evidence_only'

        WHEN COALESCE(s.o3_rows, 0) > 0
        THEN 'family_epithet_evidence_only'

        WHEN COALESCE(s.o4_rows, 0) > 0
        THEN 'cross_genus_epithet_evidence_only'

        ELSE 'no_wcvp_species_evidence'

      END AS species_diagnostic_class

    FROM species_input_classified i

    LEFT JOIN species_o1_summary o
      ON i.vpjd_taxon_universe_id =
         o.vpjd_taxon_universe_id

    LEFT JOIN species_support_summary s
      ON i.vpjd_taxon_universe_id =
         s.vpjd_taxon_universe_id

    LEFT JOIN species_target_lookup t
      ON i.vpjd_taxon_universe_id =
         t.vpjd_taxon_universe_id
    "
  )
  
  # ---------------------------------------------------------------------------
  # Diagnostic summaries
  # ---------------------------------------------------------------------------
  
  diagnostic_summary <- DBI::dbGetQuery(
    con,
    "
    SELECT
      species_diagnostic_class,
      COUNT(*) AS concepts,
      SUM(present_records) AS present_records
    FROM occurrence_wcvp_residual_species_diagnostics
    GROUP BY species_diagnostic_class
    ORDER BY present_records DESC
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
        DISTINCT wcvp_accepted_concept_id
      ) AS accepted_concepts
    FROM occurrence_wcvp_residual_species_candidates
    GROUP BY diagnostic_rule
    ORDER BY concepts DESC
    "
  )
  
  # ---------------------------------------------------------------------------
  # Evidence populations
  # ---------------------------------------------------------------------------
  
  secure_evidence <- DBI::dbGetQuery(
    con,
    "
    SELECT *
    FROM occurrence_wcvp_residual_species_diagnostics
    WHERE species_diagnostic_class =
      'secure_exact_species_concept'
    ORDER BY present_records DESC, scientificName
    "
  )
  
  review_evidence <- DBI::dbGetQuery(
    con,
    "
    SELECT *
    FROM occurrence_wcvp_residual_species_diagnostics
    WHERE species_diagnostic_class IN (
      'review_exact_name_changed_accepted_concept',
      'review_nonstandard_wcvp_target',
      'wcvp_target_record_missing',
      'exact_name_evidence_unresolved'
    )
    ORDER BY present_records DESC, scientificName
    "
  )
  
  ambiguous_evidence <- DBI::dbGetQuery(
    con,
    "
    SELECT *
    FROM occurrence_wcvp_residual_species_diagnostics
    WHERE species_diagnostic_class IN (
      'multiple_author_mapped_wcvp_concepts',
      'multiple_exact_name_wcvp_concepts'
    )
    ORDER BY present_records DESC, scientificName
    "
  )
  
  weak_evidence <- DBI::dbGetQuery(
    con,
    "
    SELECT *
    FROM occurrence_wcvp_residual_species_diagnostics
    WHERE species_diagnostic_class IN (
      'same_genus_epithet_evidence_only',
      'family_epithet_evidence_only',
      'cross_genus_epithet_evidence_only'
    )
    ORDER BY present_records DESC, scientificName
    "
  )
  
  pseudo_species <- DBI::dbGetQuery(
    con,
    "
    SELECT *
    FROM occurrence_wcvp_residual_species_diagnostics
    WHERE species_diagnostic_class =
      'pseudo_species_or_placeholder'
    ORDER BY present_records DESC, scientificName
    "
  )
  
  no_evidence <- DBI::dbGetQuery(
    con,
    "
    SELECT *
    FROM occurrence_wcvp_residual_species_diagnostics
    WHERE species_diagnostic_class =
      'no_wcvp_species_evidence'
    ORDER BY present_records DESC, scientificName
    "
  )
  
  # ---------------------------------------------------------------------------
  # O1 exact-name candidate audit
  # ---------------------------------------------------------------------------
  
  exact_candidate_detail <- DBI::dbGetQuery(
    con,
    "
    SELECT
      d.vpjd_taxon_universe_id,
      d.taxonKey,
      d.speciesKey,
      d.family,
      d.species,
      d.scientificName,
      d.source_authorship_raw,
      d.norm_source_authorship,
      d.present_records,
      d.species_diagnostic_class,
      d.selected_accepted_id,
      d.selected_evidence_route,
      d.target_wcvp_taxon_name,
      d.target_wcvp_taxon_authors,
      d.target_wcvp_taxon_status,

      c.wcvp_plant_name_id,
      c.wcvp_taxon_name,
      c.wcvp_taxon_authors,
      c.wcvp_taxon_rank,
      c.wcvp_taxon_status,
      c.wcvp_accepted_plant_name_id,
      c.wcvp_accepted_concept_id,
      c.authorship_exact

    FROM occurrence_wcvp_residual_species_diagnostics d

    INNER JOIN occurrence_wcvp_residual_species_candidates c
      ON d.vpjd_taxon_universe_id =
         c.vpjd_taxon_universe_id

    WHERE
      c.diagnostic_rule =
      'O1_EXACT_SPECIES_NAME'

    ORDER BY
      d.present_records DESC,
      d.scientificName,
      c.authorship_exact DESC,
      c.wcvp_taxon_name,
      c.wcvp_taxon_authors
    "
  )
  
  # ---------------------------------------------------------------------------
  # Safety check: 04n baseline unchanged
  # ---------------------------------------------------------------------------
  
  baseline_after <- DBI::dbGetQuery(
    con,
    "
    SELECT
      COUNT(*) AS total_concepts,
      SUM(
        CASE
          WHEN infraspecific_final_wcvp_accepted_plant_name_id
               IS NOT NULL
          THEN 1 ELSE 0
        END
      ) AS resolved_after,
      COUNT(
        DISTINCT
        infraspecific_final_wcvp_accepted_plant_name_id
      ) AS distinct_accepted_after,
      SUM(
        CASE
          WHEN infraspecific_final_wcvp_accepted_plant_name_id
               IS NULL
          THEN 1 ELSE 0
        END
      ) AS unresolved_after
    FROM occurrence_wcvp_reconciliation_infraspecific
    "
  )
  
  if (
    baseline_after$total_concepts[[1]] !=
    baseline$total_concepts[[1]] ||
    
    baseline_after$resolved_after[[1]] !=
    baseline$resolved_before_04o[[1]] ||
    
    baseline_after$distinct_accepted_after[[1]] !=
    baseline$distinct_accepted_before_04o[[1]] ||
    
    baseline_after$unresolved_after[[1]] !=
    baseline$unresolved_before_04o[[1]]
  ) {
    stop(
      "04o safety check failed: ",
      "04n reconciliation baseline changed."
    )
  }
  
  # ---------------------------------------------------------------------------
  # Metadata
  # ---------------------------------------------------------------------------
  
  DBI::dbExecute(
    con,
    paste0(
      "
      CREATE TABLE
        occurrence_wcvp_residual_species_diagnostic_metadata
      AS
      SELECT
        CURRENT_TIMESTAMP AS created_at,
        '",
      VPJD_WCVP_SPECIES_DIAG_VERSION,
      "'::VARCHAR AS script_version,
        '0.1.0'::VARCHAR AS required_04n_version,
        ",
      baseline$resolved_before_04o[[1]],
      "::BIGINT AS resolved_baseline,
        ",
      baseline$distinct_accepted_before_04o[[1]],
      "::BIGINT AS distinct_accepted_baseline,
        ",
      baseline$unresolved_before_04o[[1]],
      "::BIGINT AS unresolved_baseline,
        COUNT(*) AS target_concepts,
        SUM(present_records) AS target_present_records
      FROM occurrence_wcvp_residual_species_diagnostics
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
    FROM occurrence_wcvp_residual_species_diagnostics
    ORDER BY present_records DESC, scientificName
    "
  )
  
  exports <- list(
    residual_species_diagnostics =
      full_diagnostics,
    
    residual_species_diagnostic_summary =
      diagnostic_summary,
    
    residual_species_rule_summary =
      rule_summary,
    
    residual_species_secure_evidence =
      secure_evidence,
    
    residual_species_review_evidence =
      review_evidence,
    
    residual_species_ambiguous_evidence =
      ambiguous_evidence,
    
    residual_species_weak_evidence =
      weak_evidence,
    
    residual_species_pseudo_species =
      pseudo_species,
    
    residual_species_no_evidence =
      no_evidence,
    
    residual_species_exact_candidate_detail =
      exact_candidate_detail
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
    "Hardened residual species diagnostics complete"
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
    "{.strong Pseudo-species / malformed concepts:} ",
    "{fmt(input_total$pseudo_species_concepts[[1]])}"
  )
  
  cli::cli_text(
    "{.strong Pseudo-species PRESENT records:} ",
    "{fmt(input_total$pseudo_species_present_records[[1]])}"
  )
  
  cli::cli_text(
    "{.strong Secure diagnostic concepts:} ",
    "{fmt(nrow(secure_evidence))}"
  )
  
  cli::cli_text(
    "{.strong Review concepts:} ",
    "{fmt(nrow(review_evidence))}"
  )
  
  cli::cli_text(
    "{.strong Ambiguous exact-name concepts:} ",
    "{fmt(nrow(ambiguous_evidence))}"
  )
  
  cli::cli_alert_success(
    "04o is diagnostic only: no WCVP assignments were made."
  )
  
  cli::cli_alert_success(
    "04n and all upstream reconciliation tables remain unchanged."
  )
  
  cli::cli_text(
    "04o_occurrence_wcvp_residual_species_diagnostics.R ",
    "v{VPJD_WCVP_SPECIES_DIAG_VERSION} complete."
  )
  
  invisible(
    list(
      baseline = baseline,
      input_total = input_total,
      diagnostic_summary = diagnostic_summary,
      rule_summary = rule_summary,
      secure_evidence = secure_evidence,
      review_evidence = review_evidence,
      ambiguous_evidence = ambiguous_evidence,
      weak_evidence = weak_evidence,
      pseudo_species = pseudo_species,
      no_evidence = no_evidence,
      exact_candidate_detail = exact_candidate_detail
    )
  )
}

wcvp_species_diag_results <-
  run_occurrence_wcvp_residual_species_diagnostics()

species_diag_baseline <-
  wcvp_species_diag_results$baseline

species_diag_input_total <-
  wcvp_species_diag_results$input_total

species_diagnostic_summary <-
  wcvp_species_diag_results$diagnostic_summary

species_rule_summary <-
  wcvp_species_diag_results$rule_summary

species_secure_evidence <-
  wcvp_species_diag_results$secure_evidence

species_review_evidence <-
  wcvp_species_diag_results$review_evidence

species_ambiguous_evidence <-
  wcvp_species_diag_results$ambiguous_evidence

species_weak_evidence <-
  wcvp_species_diag_results$weak_evidence

species_pseudo_species <-
  wcvp_species_diag_results$pseudo_species

species_no_evidence <-
  wcvp_species_diag_results$no_evidence

species_exact_candidate_detail <-
  wcvp_species_diag_results$exact_candidate_detail