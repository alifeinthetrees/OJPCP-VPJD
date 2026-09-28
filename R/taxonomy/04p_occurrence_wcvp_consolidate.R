# ==============================================================================
# 04p_occurrence_wcvp_consolidate.R
# VPJD / Oxford-Japan Plant Conservation Partnership
#
# Purpose:
#   Build the authoritative consolidated occurrence-derived taxonomic dataset
#   following deterministic WCVP reconciliation.
#
# Final WCVP target classes:
#   accepted_wcvp
#     WCVP taxon_status = Accepted
#
#   artificial_hybrid_wcvp
#     WCVP taxon_status = Artificial Hybrid
#     Retained as a recognised WCVP concept, but explicitly distinguished from
#     ordinary Accepted taxa.
#
#   unplaced_wcvp
#     WCVP taxon_status = Unplaced
#     Nomenclaturally linked, but NOT included in the recognised VPJD taxon
#     universe pending review.
#
#   unresolved
#     No deterministic final WCVP ID.
#
# Recognised VPJD taxon universe:
#   Accepted + Artificial Hybrid
#
# IMPORTANT:
#   No new taxonomic matching or assignment occurs here.
#   No fuzzy matching.
#   No edit-distance matching.
#   No spelling transformations.
#   Unplaced concepts are retained for audit but excluded from the recognised
#   downstream taxon universe.
#
# Version: 0.1.1
# ==============================================================================

VPJD_WCVP_CONSOLIDATE_VERSION <- "0.1.1"

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

run_occurrence_wcvp_consolidation <- function() {
  
  duckdb_path <- here::here(
    "data","interim","occurrences","vpjd_occurrences.duckdb"
  )
  
  output_dir <- here::here(
    "outputs","tables","taxonomy",
    "occurrence_wcvp_consolidated"
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
    "VPJD consolidated occurrence-derived WCVP taxonomy"
  )
  
  # ---------------------------------------------------------------------------
  # Required frozen inputs
  # ---------------------------------------------------------------------------
  
  required_tables <- c(
    "occurrence_wcvp_reconciliation_infraspecific",
    "occurrence_wcvp_infraspecific_recovery_metadata",
    "occurrence_wcvp_residual_species_diagnostics",
    "occurrence_wcvp_residual_species_diagnostic_metadata"
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
  
  metadata_04o <- DBI::dbGetQuery(
    con,
    "
    SELECT *
    FROM occurrence_wcvp_residual_species_diagnostic_metadata
    "
  )
  
  if (nrow(metadata_04n) != 1L) {
    stop(
      "Expected exactly one row in 04n metadata."
    )
  }
  
  if (nrow(metadata_04o) != 1L) {
    stop(
      "Expected exactly one row in 04o metadata."
    )
  }
  
  if (
    is.na(metadata_04n$script_version[[1]]) ||
    metadata_04n$script_version[[1]] != "0.1.0"
  ) {
    stop(
      "04p requires frozen 04n v0.1.0. Found: ",
      metadata_04n$script_version[[1]]
    )
  }
  
  if (
    is.na(metadata_04o$script_version[[1]]) ||
    metadata_04o$script_version[[1]] != "0.1.1"
  ) {
    stop(
      "04p requires frozen 04o v0.1.1. Found: ",
      metadata_04o$script_version[[1]]
    )
  }
  
  # ---------------------------------------------------------------------------
  # Validate frozen 04n baseline
  #
  # NOTE:
  #   12,053 is now described as distinct FINAL WCVP IDs, not distinct
  #   Accepted taxa. Their WCVP statuses are classified below.
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
      ) AS linked_concepts,

      COUNT(
        DISTINCT
        infraspecific_final_wcvp_accepted_plant_name_id
      ) AS distinct_final_wcvp_ids,

      SUM(
        CASE
          WHEN infraspecific_final_wcvp_accepted_plant_name_id
               IS NULL
          THEN 1 ELSE 0
        END
      ) AS unresolved_concepts

    FROM occurrence_wcvp_reconciliation_infraspecific
    "
  )
  
  if (
    baseline$total_concepts[[1]] != 21165L ||
    baseline$linked_concepts[[1]] != 19682L ||
    baseline$distinct_final_wcvp_ids[[1]] != 12053L ||
    baseline$unresolved_concepts[[1]] != 1483L
  ) {
    stop(
      "Frozen 04n baseline differs from expected state. Expected ",
      "21,165 total / 19,682 linked / 12,053 distinct final WCVP IDs / ",
      "1,483 unresolved."
    )
  }
  
  cli::cli_text(
    "{.strong Total occurrence-derived concepts:} ",
    "{fmt(baseline$total_concepts[[1]])}"
  )
  
  cli::cli_text(
    "{.strong Concepts linked to final WCVP IDs:} ",
    "{fmt(baseline$linked_concepts[[1]])}"
  )
  
  cli::cli_text(
    "{.strong Distinct final WCVP IDs:} ",
    "{fmt(baseline$distinct_final_wcvp_ids[[1]])}"
  )
  
  cli::cli_text(
    "{.strong Concepts without final WCVP IDs:} ",
    "{fmt(baseline$unresolved_concepts[[1]])}"
  )
  
  # ---------------------------------------------------------------------------
  # Remove only 04p-owned products
  # ---------------------------------------------------------------------------
  
  for (tbl in c(
    "occurrence_wcvp_consolidation_metadata",
    "occurrence_wcvp_recognised_taxa",
    "occurrence_wcvp_accepted_taxa",
    "occurrence_wcvp_artificial_hybrid_taxa",
    "occurrence_wcvp_unplaced_taxa",
    "occurrence_wcvp_unresolved_taxa",
    "occurrence_wcvp_consolidated"
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
  # Complete WCVP lookup
  # ---------------------------------------------------------------------------
  
  wcvp_names <- rWCVPdata::wcvp_names
  
  required_wcvp_fields <- c(
    "plant_name_id",
    "taxon_name",
    "taxon_authors",
    "taxon_rank",
    "taxon_status",
    "accepted_plant_name_id",
    "family",
    "genus",
    "species"
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
    "wcvp_04p_source",
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
  
  DBI::dbExecute(
    con,
    "
    CREATE OR REPLACE TEMP TABLE wcvp_04p_lookup AS

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
      genus,
      species

    FROM wcvp_04p_source

    WHERE
      TRY_CAST(
        plant_name_id AS BIGINT
      ) IS NOT NULL
    "
  )
  
  # ---------------------------------------------------------------------------
  # Consolidated source-concept table
  # ---------------------------------------------------------------------------
  
  DBI::dbExecute(
    con,
    "
    CREATE TABLE occurrence_wcvp_consolidated AS

    SELECT
      r.*,

      w.taxon_name
        AS final_wcvp_taxon_name,

      w.taxon_authors
        AS final_wcvp_taxon_authors,

      w.taxon_rank
        AS final_wcvp_taxon_rank,

      w.taxon_status
        AS final_wcvp_taxon_status,

      w.family
        AS final_wcvp_family,

      w.genus
        AS final_wcvp_genus,

      w.species
        AS final_wcvp_species,

      CASE

        WHEN
          r.infraspecific_final_wcvp_accepted_plant_name_id
          IS NULL
        THEN 'unresolved'

        WHEN
          UPPER(
            COALESCE(
              w.taxon_status,
              ''
            )
          ) = 'ACCEPTED'
        THEN 'accepted_wcvp'

        WHEN
          UPPER(
            COALESCE(
              w.taxon_status,
              ''
            )
          ) = 'ARTIFICIAL HYBRID'
        THEN 'artificial_hybrid_wcvp'

        WHEN
          UPPER(
            COALESCE(
              w.taxon_status,
              ''
            )
          ) = 'UNPLACED'
        THEN 'unplaced_wcvp'

        WHEN
          w.plant_name_id IS NULL
        THEN 'linked_wcvp_id_missing_lookup'

        ELSE 'other_wcvp_status'

      END AS vpjd_taxonomic_status,

      CASE
        WHEN
          UPPER(
            COALESCE(
              w.taxon_status,
              ''
            )
          ) IN (
            'ACCEPTED',
            'ARTIFICIAL HYBRID'
          )
        THEN TRUE
        ELSE FALSE
      END AS vpjd_recognised_taxon,

      CASE
        WHEN
          UPPER(
            COALESCE(
              w.taxon_status,
              ''
            )
          ) = 'ACCEPTED'
        THEN TRUE
        ELSE FALSE
      END AS vpjd_accepted_taxon,

      CASE
        WHEN
          UPPER(
            COALESCE(
              w.taxon_status,
              ''
            )
          ) = 'ARTIFICIAL HYBRID'
        THEN TRUE
        ELSE FALSE
      END AS vpjd_artificial_hybrid,

      CASE
        WHEN
          UPPER(
            COALESCE(
              w.taxon_status,
              ''
            )
          ) = 'UNPLACED'
        THEN TRUE
        ELSE FALSE
      END AS vpjd_unplaced_taxon,

      s.species_diagnostic_class
        AS residual_species_diagnostic_class,

      s.pseudo_species_flag
        AS residual_species_pseudo_species_flag,

      s.selected_evidence_route
        AS residual_species_selected_evidence_route,

      s.o1_candidate_rows
        AS residual_species_o1_candidate_rows,

      s.o1_accepted_concept_count
        AS residual_species_o1_accepted_concept_count

    FROM occurrence_wcvp_reconciliation_infraspecific r

    LEFT JOIN wcvp_04p_lookup w
      ON
        r.infraspecific_final_wcvp_accepted_plant_name_id =
        w.plant_name_id

    LEFT JOIN occurrence_wcvp_residual_species_diagnostics s
      ON
        r.vpjd_taxon_universe_id =
        s.vpjd_taxon_universe_id
    "
  )
  
  # ---------------------------------------------------------------------------
  # Integrity: every source concept must occur exactly once
  # ---------------------------------------------------------------------------
  
  source_integrity <- DBI::dbGetQuery(
    con,
    "
    SELECT
      COUNT(*) AS total_concepts,

      COUNT(
        DISTINCT vpjd_taxon_universe_id
      ) AS distinct_source_concepts,

      SUM(
        CASE
          WHEN
            infraspecific_final_wcvp_accepted_plant_name_id
            IS NOT NULL
          THEN 1 ELSE 0
        END
      ) AS linked_concepts,

      COUNT(
        DISTINCT
        infraspecific_final_wcvp_accepted_plant_name_id
      ) AS distinct_final_wcvp_ids,

      SUM(
        CASE
          WHEN
            infraspecific_final_wcvp_accepted_plant_name_id
            IS NULL
          THEN 1 ELSE 0
        END
      ) AS unresolved_concepts

    FROM occurrence_wcvp_consolidated
    "
  )
  
  if (
    source_integrity$total_concepts[[1]] != 21165L ||
    source_integrity$distinct_source_concepts[[1]] != 21165L ||
    source_integrity$linked_concepts[[1]] != 19682L ||
    source_integrity$distinct_final_wcvp_ids[[1]] != 12053L ||
    source_integrity$unresolved_concepts[[1]] != 1483L
  ) {
    stop(
      "04p source-concept integrity check failed."
    )
  }
  
  # ---------------------------------------------------------------------------
  # Status summary
  # ---------------------------------------------------------------------------
  
  taxonomic_status_summary <- DBI::dbGetQuery(
    con,
    "
    SELECT
      vpjd_taxonomic_status,
      COUNT(*) AS source_concepts,
      COUNT(
        DISTINCT
        infraspecific_final_wcvp_accepted_plant_name_id
      ) AS distinct_wcvp_ids,
      SUM(
        COALESCE(
          present_records,
          0
        )
      ) AS present_records

    FROM occurrence_wcvp_consolidated

    GROUP BY
      vpjd_taxonomic_status

    ORDER BY
      source_concepts DESC
    "
  )
  
  # ---------------------------------------------------------------------------
  # Guard against unexpected WCVP states
  # ---------------------------------------------------------------------------
  
  unexpected_statuses <- DBI::dbGetQuery(
    con,
    "
    SELECT
      vpjd_taxonomic_status,
      final_wcvp_taxon_status,
      COUNT(*) AS source_concepts,
      COUNT(
        DISTINCT
        infraspecific_final_wcvp_accepted_plant_name_id
      ) AS distinct_wcvp_ids
    FROM occurrence_wcvp_consolidated
    WHERE
      vpjd_taxonomic_status IN (
        'linked_wcvp_id_missing_lookup',
        'other_wcvp_status'
      )
    GROUP BY
      vpjd_taxonomic_status,
      final_wcvp_taxon_status
    "
  )
  
  if (nrow(unexpected_statuses) > 0L) {
    print(unexpected_statuses)
    
    stop(
      "04p encountered one or more unexpected final WCVP states. ",
      "No downstream taxon universe has been frozen."
    )
  }
  
  # ---------------------------------------------------------------------------
  # Recognised downstream taxon universe
  #
  # Accepted + Artificial Hybrid only.
  # ---------------------------------------------------------------------------
  
  DBI::dbExecute(
    con,
    "
    CREATE TABLE occurrence_wcvp_recognised_taxa AS

    SELECT
      infraspecific_final_wcvp_accepted_plant_name_id
        AS wcvp_plant_name_id,

      MAX(final_wcvp_taxon_name)
        AS wcvp_taxon_name,

      MAX(final_wcvp_taxon_authors)
        AS wcvp_taxon_authors,

      MAX(final_wcvp_taxon_rank)
        AS wcvp_taxon_rank,

      MAX(final_wcvp_taxon_status)
        AS wcvp_taxon_status,

      MAX(final_wcvp_family)
        AS wcvp_family,

      MAX(final_wcvp_genus)
        AS wcvp_genus,

      MAX(final_wcvp_species)
        AS wcvp_species,

      MAX(vpjd_taxonomic_status)
        AS vpjd_taxonomic_status,

      COUNT(*) AS source_gbif_concepts,

      SUM(
        COALESCE(
          total_records,
          0
        )
      ) AS source_total_records,

      SUM(
        COALESCE(
          present_records,
          0
        )
      ) AS source_present_records

    FROM occurrence_wcvp_consolidated

    WHERE
      vpjd_recognised_taxon

    GROUP BY
      infraspecific_final_wcvp_accepted_plant_name_id
    "
  )
  
  # ---------------------------------------------------------------------------
  # Accepted-only table
  # ---------------------------------------------------------------------------
  
  DBI::dbExecute(
    con,
    "
    CREATE TABLE occurrence_wcvp_accepted_taxa AS

    SELECT *
    FROM occurrence_wcvp_recognised_taxa

    WHERE
      UPPER(
        COALESCE(
          wcvp_taxon_status,
          ''
        )
      ) = 'ACCEPTED'
    "
  )
  
  # ---------------------------------------------------------------------------
  # Artificial-hybrid table
  # ---------------------------------------------------------------------------
  
  DBI::dbExecute(
    con,
    "
    CREATE TABLE occurrence_wcvp_artificial_hybrid_taxa AS

    SELECT *
    FROM occurrence_wcvp_recognised_taxa

    WHERE
      UPPER(
        COALESCE(
          wcvp_taxon_status,
          ''
        )
      ) = 'ARTIFICIAL HYBRID'
    "
  )
  
  # ---------------------------------------------------------------------------
  # Unplaced review table
  # ---------------------------------------------------------------------------
  
  DBI::dbExecute(
    con,
    "
    CREATE TABLE occurrence_wcvp_unplaced_taxa AS

    SELECT *
    FROM occurrence_wcvp_consolidated

    WHERE
      vpjd_unplaced_taxon
    "
  )
  
  # ---------------------------------------------------------------------------
  # Fully unresolved source concepts
  # ---------------------------------------------------------------------------
  
  DBI::dbExecute(
    con,
    "
    CREATE TABLE occurrence_wcvp_unresolved_taxa AS

    SELECT *
    FROM occurrence_wcvp_consolidated

    WHERE
      vpjd_taxonomic_status =
      'unresolved'
    "
  )
  
  # ---------------------------------------------------------------------------
  # Downstream counts
  # ---------------------------------------------------------------------------
  
  universe_summary <- DBI::dbGetQuery(
    con,
    "
    SELECT

      (
        SELECT COUNT(*)
        FROM occurrence_wcvp_recognised_taxa
      ) AS recognised_wcvp_taxa,

      (
        SELECT COUNT(*)
        FROM occurrence_wcvp_accepted_taxa
      ) AS accepted_wcvp_taxa,

      (
        SELECT COUNT(*)
        FROM occurrence_wcvp_artificial_hybrid_taxa
      ) AS artificial_hybrid_wcvp_taxa,

      (
        SELECT COUNT(
          DISTINCT
          infraspecific_final_wcvp_accepted_plant_name_id
        )
        FROM occurrence_wcvp_unplaced_taxa
      ) AS unplaced_wcvp_taxa,

      (
        SELECT COUNT(*)
        FROM occurrence_wcvp_unplaced_taxa
      ) AS unplaced_source_concepts,

      (
        SELECT COUNT(*)
        FROM occurrence_wcvp_unresolved_taxa
      ) AS unresolved_source_concepts
    "
  )
  
  # ---------------------------------------------------------------------------
  # Validate observed diagnostic populations
  #
  # These counts were established by the diagnostic following v0.1.0.
  # ---------------------------------------------------------------------------
  
  observed_nonaccepted <- DBI::dbGetQuery(
    con,
    "
    SELECT
      final_wcvp_taxon_status,
      COUNT(*) AS source_concepts,
      COUNT(
        DISTINCT
        infraspecific_final_wcvp_accepted_plant_name_id
      ) AS distinct_wcvp_ids,
      SUM(
        COALESCE(
          present_records,
          0
        )
      ) AS present_records

    FROM occurrence_wcvp_consolidated

    WHERE
      final_wcvp_taxon_status IN (
        'Artificial Hybrid',
        'Unplaced'
      )

    GROUP BY
      final_wcvp_taxon_status

    ORDER BY
      final_wcvp_taxon_status
    "
  )
  
  artificial_check <- observed_nonaccepted[
    observed_nonaccepted$final_wcvp_taxon_status ==
      "Artificial Hybrid",
    ,
    drop = FALSE
  ]
  
  unplaced_check <- observed_nonaccepted[
    observed_nonaccepted$final_wcvp_taxon_status ==
      "Unplaced",
    ,
    drop = FALSE
  ]
  
  if (
    nrow(artificial_check) != 1L ||
    artificial_check$source_concepts[[1]] != 35L ||
    artificial_check$distinct_wcvp_ids[[1]] != 31L ||
    artificial_check$present_records[[1]] != 304L
  ) {
    stop(
      "Artificial Hybrid population differs from validated diagnostic: ",
      "expected 35 source concepts / 31 IDs / 304 PRESENT records."
    )
  }
  
  if (
    nrow(unplaced_check) != 1L ||
    unplaced_check$source_concepts[[1]] != 18L ||
    unplaced_check$distinct_wcvp_ids[[1]] != 16L ||
    unplaced_check$present_records[[1]] != 568L
  ) {
    stop(
      "Unplaced population differs from validated diagnostic: ",
      "expected 18 source concepts / 16 IDs / 568 PRESENT records."
    )
  }
  
  # ---------------------------------------------------------------------------
  # Internal arithmetic checks
  # ---------------------------------------------------------------------------
  
  if (
    universe_summary$accepted_wcvp_taxa[[1]] +
    universe_summary$artificial_hybrid_wcvp_taxa[[1]] !=
    universe_summary$recognised_wcvp_taxa[[1]]
  ) {
    stop(
      "Recognised taxon-universe arithmetic check failed."
    )
  }
  
  if (
    universe_summary$recognised_wcvp_taxa[[1]] +
    universe_summary$unplaced_wcvp_taxa[[1]] !=
    baseline$distinct_final_wcvp_ids[[1]]
  ) {
    stop(
      "Final WCVP ID arithmetic check failed."
    )
  }
  
  # ---------------------------------------------------------------------------
  # Rank summaries
  # ---------------------------------------------------------------------------
  
  recognised_rank_summary <- DBI::dbGetQuery(
    con,
    "
    SELECT
      COALESCE(
        wcvp_taxon_rank,
        '<MISSING>'
      ) AS wcvp_taxon_rank,

      COUNT(*) AS recognised_taxa,

      SUM(
        CASE
          WHEN
            UPPER(
              COALESCE(
                wcvp_taxon_status,
                ''
              )
            ) = 'ACCEPTED'
          THEN 1 ELSE 0
        END
      ) AS accepted_taxa,

      SUM(
        CASE
          WHEN
            UPPER(
              COALESCE(
                wcvp_taxon_status,
                ''
              )
            ) = 'ARTIFICIAL HYBRID'
          THEN 1 ELSE 0
        END
      ) AS artificial_hybrid_taxa,

      SUM(source_gbif_concepts)
        AS source_gbif_concepts,

      SUM(source_present_records)
        AS source_present_records

    FROM occurrence_wcvp_recognised_taxa

    GROUP BY
      wcvp_taxon_rank

    ORDER BY
      recognised_taxa DESC
    "
  )
  
  unresolved_rank_summary <- DBI::dbGetQuery(
    con,
    "
    SELECT
      COALESCE(
        taxonRank,
        '<MISSING>'
      ) AS taxonRank,

      COUNT(*) AS source_concepts,

      SUM(
        COALESCE(
          present_records,
          0
        )
      ) AS present_records

    FROM occurrence_wcvp_unresolved_taxa

    GROUP BY
      taxonRank

    ORDER BY
      source_concepts DESC,
      present_records DESC
    "
  )
  
  unplaced_rank_summary <- DBI::dbGetQuery(
    con,
    "
    SELECT
      COALESCE(
        final_wcvp_taxon_rank,
        '<MISSING>'
      ) AS wcvp_taxon_rank,

      COUNT(*) AS source_concepts,

      COUNT(
        DISTINCT
        infraspecific_final_wcvp_accepted_plant_name_id
      ) AS distinct_wcvp_ids,

      SUM(
        COALESCE(
          present_records,
          0
        )
      ) AS present_records

    FROM occurrence_wcvp_unplaced_taxa

    GROUP BY
      final_wcvp_taxon_rank

    ORDER BY
      source_concepts DESC
    "
  )
  
  residual_species_summary <- DBI::dbGetQuery(
    con,
    "
    SELECT
      COALESCE(
        residual_species_diagnostic_class,
        '<NOT_SPECIES_DIAGNOSTIC>'
      ) AS residual_species_diagnostic_class,

      COUNT(*) AS source_concepts,

      SUM(
        COALESCE(
          present_records,
          0
        )
      ) AS present_records

    FROM occurrence_wcvp_unresolved_taxa

    GROUP BY
      residual_species_diagnostic_class

    ORDER BY
      source_concepts DESC,
      present_records DESC
    "
  )
  
  # ---------------------------------------------------------------------------
  # Unplaced audit
  # ---------------------------------------------------------------------------
  
  unplaced_audit <- DBI::dbGetQuery(
    con,
    "
    SELECT
      vpjd_taxon_universe_id,
      taxonKey,
      speciesKey,
      family,
      genus,
      species,
      taxonRank,
      scientificName,
      present_records,

      infraspecific_final_wcvp_accepted_plant_name_id
        AS final_wcvp_id,

      final_wcvp_taxon_name,
      final_wcvp_taxon_authors,
      final_wcvp_taxon_rank,
      final_wcvp_taxon_status,
      infraspecific_final_resolution_stage

    FROM occurrence_wcvp_unplaced_taxa

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
    paste0(
      "
      CREATE TABLE occurrence_wcvp_consolidation_metadata AS

      SELECT
        CURRENT_TIMESTAMP AS created_at,

        '",
      VPJD_WCVP_CONSOLIDATE_VERSION,
      "'::VARCHAR AS script_version,

        '0.1.0'::VARCHAR AS required_04n_version,
        '0.1.1'::VARCHAR AS required_04o_version,

        ",
      baseline$total_concepts[[1]],
      "::BIGINT AS total_source_concepts,

        ",
      baseline$linked_concepts[[1]],
      "::BIGINT AS source_concepts_with_final_wcvp_id,

        ",
      baseline$distinct_final_wcvp_ids[[1]],
      "::BIGINT AS distinct_final_wcvp_ids,

        ",
      universe_summary$recognised_wcvp_taxa[[1]],
      "::BIGINT AS recognised_wcvp_taxa,

        ",
      universe_summary$accepted_wcvp_taxa[[1]],
      "::BIGINT AS accepted_wcvp_taxa,

        ",
      universe_summary$artificial_hybrid_wcvp_taxa[[1]],
      "::BIGINT AS artificial_hybrid_wcvp_taxa,

        ",
      universe_summary$unplaced_wcvp_taxa[[1]],
      "::BIGINT AS unplaced_wcvp_taxa,

        ",
      universe_summary$unplaced_source_concepts[[1]],
      "::BIGINT AS unplaced_source_concepts,

        ",
      universe_summary$unresolved_source_concepts[[1]],
      "::BIGINT AS unresolved_source_concepts
      "
    )
  )
  
  # ---------------------------------------------------------------------------
  # Exports
  # ---------------------------------------------------------------------------
  
  consolidated_export <- DBI::dbGetQuery(
    con,
    "
    SELECT *
    FROM occurrence_wcvp_consolidated

    ORDER BY
      CASE vpjd_taxonomic_status
        WHEN 'accepted_wcvp' THEN 1
        WHEN 'artificial_hybrid_wcvp' THEN 2
        WHEN 'unplaced_wcvp' THEN 3
        WHEN 'unresolved' THEN 4
        ELSE 5
      END,
      final_wcvp_family,
      final_wcvp_taxon_name,
      scientificName
    "
  )
  
  recognised_export <- DBI::dbGetQuery(
    con,
    "
    SELECT *
    FROM occurrence_wcvp_recognised_taxa
    ORDER BY
      wcvp_family,
      wcvp_taxon_name
    "
  )
  
  accepted_export <- DBI::dbGetQuery(
    con,
    "
    SELECT *
    FROM occurrence_wcvp_accepted_taxa
    ORDER BY
      wcvp_family,
      wcvp_taxon_name
    "
  )
  
  artificial_hybrid_export <- DBI::dbGetQuery(
    con,
    "
    SELECT *
    FROM occurrence_wcvp_artificial_hybrid_taxa
    ORDER BY
      wcvp_family,
      wcvp_taxon_name
    "
  )
  
  unresolved_export <- DBI::dbGetQuery(
    con,
    "
    SELECT *
    FROM occurrence_wcvp_unresolved_taxa
    ORDER BY
      present_records DESC,
      family,
      scientificName
    "
  )
  
  exports <- list(
    occurrence_wcvp_consolidated =
      consolidated_export,
    
    occurrence_wcvp_recognised_taxa =
      recognised_export,
    
    occurrence_wcvp_accepted_taxa =
      accepted_export,
    
    occurrence_wcvp_artificial_hybrid_taxa =
      artificial_hybrid_export,
    
    occurrence_wcvp_unplaced_taxa =
      unplaced_audit,
    
    occurrence_wcvp_unresolved_taxa =
      unresolved_export,
    
    taxonomic_status_summary =
      taxonomic_status_summary,
    
    taxon_universe_summary =
      universe_summary,
    
    recognised_rank_summary =
      recognised_rank_summary,
    
    unresolved_rank_summary =
      unresolved_rank_summary,
    
    unplaced_rank_summary =
      unplaced_rank_summary,
    
    residual_species_summary =
      residual_species_summary
  )
  
  for (nm in names(exports)) {
    readr::write_csv(
      exports[[nm]],
      file.path(
        output_dir,
        paste0(
          nm,
          ".csv"
        )
      )
    )
  }
  
  # ---------------------------------------------------------------------------
  # Console
  # ---------------------------------------------------------------------------
  
  cli::cli_h2(
    "04p consolidation complete"
  )
  
  cli::cli_text(
    "{.strong Source concepts:} ",
    "{fmt(source_integrity$total_concepts[[1]])}"
  )
  
  cli::cli_text(
    "{.strong Source concepts linked to WCVP IDs:} ",
    "{fmt(source_integrity$linked_concepts[[1]])}"
  )
  
  cli::cli_text(
    "{.strong Distinct final WCVP IDs:} ",
    "{fmt(source_integrity$distinct_final_wcvp_ids[[1]])}"
  )
  
  cli::cli_text(
    "{.strong Recognised WCVP taxa:} ",
    "{fmt(universe_summary$recognised_wcvp_taxa[[1]])}"
  )
  
  cli::cli_text(
    "{.strong Accepted WCVP taxa:} ",
    "{fmt(universe_summary$accepted_wcvp_taxa[[1]])}"
  )
  
  cli::cli_text(
    "{.strong Artificial Hybrid WCVP taxa:} ",
    "{fmt(universe_summary$artificial_hybrid_wcvp_taxa[[1]])}"
  )
  
  cli::cli_text(
    "{.strong Unplaced WCVP taxa:} ",
    "{fmt(universe_summary$unplaced_wcvp_taxa[[1]])}"
  )
  
  cli::cli_text(
    "{.strong Unplaced source concepts:} ",
    "{fmt(universe_summary$unplaced_source_concepts[[1]])}"
  )
  
  cli::cli_text(
    "{.strong Fully unresolved source concepts:} ",
    "{fmt(universe_summary$unresolved_source_concepts[[1]])}"
  )
  
  cli::cli_alert_success(
    "Accepted and Artificial Hybrid WCVP concepts form the recognised taxon universe."
  )
  
  cli::cli_alert_success(
    "Unplaced WCVP concepts have been retained separately for review."
  )
  
  cli::cli_alert_success(
    "No new taxonomic assignments were made."
  )
  
  cli::cli_text(
    "04p_occurrence_wcvp_consolidate.R ",
    "v{VPJD_WCVP_CONSOLIDATE_VERSION} complete."
  )
  
  invisible(
    list(
      baseline = baseline,
      source_integrity = source_integrity,
      taxonomic_status_summary = taxonomic_status_summary,
      universe_summary = universe_summary,
      observed_nonaccepted = observed_nonaccepted,
      recognised_rank_summary = recognised_rank_summary,
      unresolved_rank_summary = unresolved_rank_summary,
      unplaced_rank_summary = unplaced_rank_summary,
      residual_species_summary = residual_species_summary,
      unplaced_audit = unplaced_audit
    )
  )
}

wcvp_consolidation_results <-
  run_occurrence_wcvp_consolidation()

consolidation_baseline <-
  wcvp_consolidation_results$baseline

consolidated_check <-
  wcvp_consolidation_results$source_integrity

taxonomic_status_summary <-
  wcvp_consolidation_results$taxonomic_status_summary

taxon_universe_summary <-
  wcvp_consolidation_results$universe_summary

nonaccepted_status_summary <-
  wcvp_consolidation_results$observed_nonaccepted

recognised_rank_summary <-
  wcvp_consolidation_results$recognised_rank_summary

unresolved_rank_summary <-
  wcvp_consolidation_results$unresolved_rank_summary

unplaced_rank_summary <-
  wcvp_consolidation_results$unplaced_rank_summary

residual_species_summary <-
  wcvp_consolidation_results$residual_species_summary

unplaced_audit <-
  wcvp_consolidation_results$unplaced_audit