# ==============================================================================
# 04a_occurrence_wcvp_reconcile.R
# VPJD / Oxford-Japan Plant Conservation Partnership
# Stage 4a: Initial WCVP reconciliation of occurrence-derived taxon universe
#
# Input:
#   data/interim/occurrences/vpjd_occurrences.duckdb
#   table: occurrence_taxon_universe
#
# Taxonomic authority:
#   rWCVPdata::wcvp_names
#
# Purpose:
#   Reconcile GBIF taxon concepts represented by PRESENT occurrence records
#   from Japan against WCVP.
#
# Principles:
# - Occurrence-derived GBIF concepts are the input.
# - GBIF scientificName is preserved unchanged.
# - WCVP is the taxonomic authority for reconciliation.
# - Exact scientific-name matching only in this stage.
# - Accepted WCVP names remain accepted.
# - WCVP synonyms resolve to their accepted WCVP concept.
# - Higher-rank GBIF concepts are retained separately.
# - Unmatched and ambiguous names are retained for subsequent recovery.
# - No fuzzy matching occurs here.
# - No taxon concepts are discarded.
# - No Star ratings are assigned here.
#
# Version: 0.1.1
# ==============================================================================

VPJD_OCCURRENCE_WCVP_VERSION <- "0.1.1"

# Packages ---------------------------------------------------------------------

required_packages <- c("here", "DBI", "duckdb", "readr", "dplyr", "rWCVPdata")

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0L) {
  stop(
    "Required package(s) not installed in project renv: ",
    paste(missing_packages, collapse = ", "),
    "\nInstall with:\nrenv::install(c(",
    paste(sprintf('"%s"', missing_packages), collapse = ", "),
    "))"
  )
}

suppressPackageStartupMessages({
  library(here)
  library(DBI)
  library(duckdb)
  library(readr)
  library(dplyr)
})

# Paths ------------------------------------------------------------------------

db_path <- here::here(
  "data", "interim", "occurrences", "vpjd_occurrences.duckdb"
)

output_dir <- here::here(
  "data", "interim", "taxonomy"
)

audit_dir <- here::here(
  "outputs", "tables", "taxonomy", "occurrence_wcvp_reconciliation"
)

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(audit_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(db_path)) {
  stop("VPJD occurrence DuckDB database not found:\n", db_path)
}

# Main -------------------------------------------------------------------------

reconcile_occurrence_taxa_wcvp <- function() {
  
  message("")
  message("==============================================================")
  message("VPJD OCCURRENCE-FIRST WCVP RECONCILIATION")
  message("==============================================================")
  message("Module version: ", VPJD_OCCURRENCE_WCVP_VERSION)
  message("Occurrence database: ", db_path)
  
  con <- DBI::dbConnect(
    duckdb::duckdb(),
    dbdir = db_path
  )
  
  on.exit({
    if (DBI::dbIsValid(con)) {
      DBI::dbDisconnect(con, shutdown = TRUE)
    }
  }, add = TRUE)
  
  # Validate input -------------------------------------------------------------
  
  tables <- DBI::dbListTables(con)
  
  if (!"occurrence_taxon_universe" %in% tables) {
    stop(
      "Required table 'occurrence_taxon_universe' not found.\n",
      "Run 03a_build_occurrence_taxon_universe.R first."
    )
  }
  
  input_summary <- DBI::dbGetQuery(
    con,
    "
    SELECT
      COUNT(*) AS taxon_concepts,
      COUNT(DISTINCT vpjd_taxon_universe_id) AS unique_ids,
      SUM(present_records) AS present_records,
      SUM(
        CAST(vpjd_resolution_class = 'higher_rank' AS INTEGER)
      ) AS higher_rank_concepts
    FROM occurrence_taxon_universe
    "
  )
  
  if (
    input_summary$taxon_concepts[[1]] !=
    input_summary$unique_ids[[1]]
  ) {
    stop("Occurrence taxon universe IDs are not unique.")
  }
  
  message("")
  message(
    "Occurrence-derived concepts: ",
    format(
      input_summary$taxon_concepts[[1]],
      big.mark = ",",
      scientific = FALSE
    )
  )
  
  message(
    "PRESENT occurrence records represented: ",
    format(
      input_summary$present_records[[1]],
      big.mark = ",",
      scientific = FALSE
    )
  )
  
  message(
    "Higher-rank concepts retained separately: ",
    format(
      input_summary$higher_rank_concepts[[1]],
      big.mark = ",",
      scientific = FALSE
    )
  )
  
  # Load WCVP -----------------------------------------------------------------
  
  message("")
  message("Loading WCVP names backbone...")
  
  wcvp <- rWCVPdata::wcvp_names
  
  required_wcvp_columns <- c(
    "plant_name_id",
    "taxon_name",
    "taxon_authors",
    "taxon_rank",
    "taxon_status",
    "accepted_plant_name_id"
  )
  
  missing_wcvp_columns <- setdiff(
    required_wcvp_columns,
    names(wcvp)
  )
  
  if (length(missing_wcvp_columns) > 0L) {
    stop(
      "Expected WCVP column(s) missing: ",
      paste(missing_wcvp_columns, collapse = ", ")
    )
  }
  
  message(
    "WCVP rows loaded: ",
    format(
      nrow(wcvp),
      big.mark = ",",
      scientific = FALSE
    )
  )
  
  # Prepare WCVP exact-name index ---------------------------------------------
  
  message("Preparing WCVP exact-name index...")
  
  wcvp_index <- wcvp |>
    dplyr::transmute(
      wcvp_plant_name_id =
        as.character(plant_name_id),
      wcvp_taxon_name =
        as.character(taxon_name),
      wcvp_taxon_authors =
        as.character(taxon_authors),
      wcvp_taxon_rank =
        as.character(taxon_rank),
      wcvp_taxon_status =
        as.character(taxon_status),
      wcvp_accepted_plant_name_id =
        as.character(accepted_plant_name_id),
      wcvp_scientific_name = trimws(
        paste(
          as.character(taxon_name),
          dplyr::coalesce(
            as.character(taxon_authors),
            ""
          )
        )
      ),
      wcvp_match_name = trimws(
        paste(
          as.character(taxon_name),
          dplyr::coalesce(
            as.character(taxon_authors),
            ""
          )
        )
      )
    )
  
  # Accepted WCVP lookup -------------------------------------------------------
  
  accepted_lookup <- wcvp |>
    dplyr::filter(
      tolower(as.character(taxon_status)) == "accepted"
    ) |>
    dplyr::transmute(
      accepted_wcvp_plant_name_id =
        as.character(plant_name_id),
      accepted_wcvp_taxon_name =
        as.character(taxon_name),
      accepted_wcvp_taxon_authors =
        as.character(taxon_authors),
      accepted_wcvp_taxon_rank =
        as.character(taxon_rank),
      accepted_wcvp_scientific_name = trimws(
        paste(
          as.character(taxon_name),
          dplyr::coalesce(
            as.character(taxon_authors),
            ""
          )
        )
      )
    )
  
  # Register WCVP in DuckDB ----------------------------------------------------
  
  message("Registering WCVP index in DuckDB...")
  
  DBI::dbWriteTable(
    con,
    "wcvp_occurrence_match_index",
    wcvp_index,
    overwrite = TRUE
  )
  
  DBI::dbWriteTable(
    con,
    "wcvp_occurrence_accepted_lookup",
    accepted_lookup,
    overwrite = TRUE
  )
  
  # Exact-name candidates ------------------------------------------------------
  
  message("Matching GBIF scientific names exactly against WCVP...")
  
  DBI::dbExecute(
    con,
    "DROP TABLE IF EXISTS occurrence_wcvp_exact_candidates"
  )
  
  DBI::dbExecute(
    con,
    "
    CREATE TABLE occurrence_wcvp_exact_candidates AS
    SELECT
      u.vpjd_taxon_universe_id,
      w.wcvp_plant_name_id,
      w.wcvp_taxon_name,
      w.wcvp_taxon_authors,
      w.wcvp_taxon_rank,
      w.wcvp_taxon_status,
      w.wcvp_accepted_plant_name_id,
      w.wcvp_scientific_name
    FROM occurrence_taxon_universe u
    INNER JOIN wcvp_occurrence_match_index w
      ON TRIM(u.scientificName) = w.wcvp_match_name
    "
  )
  
  # Candidate counts -----------------------------------------------------------
  
  DBI::dbExecute(
    con,
    "DROP TABLE IF EXISTS occurrence_wcvp_candidate_counts"
  )
  
  DBI::dbExecute(
    con,
    "
    CREATE TABLE occurrence_wcvp_candidate_counts AS
    SELECT
      vpjd_taxon_universe_id,
      COUNT(*) AS wcvp_candidate_count
    FROM occurrence_wcvp_exact_candidates
    GROUP BY vpjd_taxon_universe_id
    "
  )
  
  # Unique exact matches -------------------------------------------------------
  
  DBI::dbExecute(
    con,
    "DROP TABLE IF EXISTS occurrence_wcvp_unique_exact"
  )
  
  DBI::dbExecute(
    con,
    "
    CREATE TABLE occurrence_wcvp_unique_exact AS
    SELECT c.*
    FROM occurrence_wcvp_exact_candidates c
    INNER JOIN occurrence_wcvp_candidate_counts n
      ON c.vpjd_taxon_universe_id =
         n.vpjd_taxon_universe_id
    WHERE n.wcvp_candidate_count = 1
    "
  )
  
  # Initial reconciliation -----------------------------------------------------
  
  message("Building reconciliation table...")
  
  DBI::dbExecute(
    con,
    "DROP TABLE IF EXISTS occurrence_wcvp_reconciliation"
  )
  
  DBI::dbExecute(
    con,
    "
    CREATE TABLE occurrence_wcvp_reconciliation AS
    SELECT
      u.*,
      COALESCE(
        n.wcvp_candidate_count,
        0
      ) AS wcvp_candidate_count,
      m.wcvp_plant_name_id,
      m.wcvp_taxon_name,
      m.wcvp_taxon_authors,
      m.wcvp_taxon_rank,
      m.wcvp_taxon_status,
      m.wcvp_accepted_plant_name_id,
      m.wcvp_scientific_name,
      CASE
        WHEN u.vpjd_resolution_class = 'higher_rank'
          THEN 'higher_rank'
        WHEN COALESCE(n.wcvp_candidate_count, 0) = 0
          THEN 'unmatched'
        WHEN COALESCE(n.wcvp_candidate_count, 0) > 1
          THEN 'ambiguous_exact'
        WHEN LOWER(COALESCE(m.wcvp_taxon_status, '')) = 'accepted'
          THEN 'exact_accepted'
        ELSE 'exact_nonaccepted'
      END AS wcvp_match_status
    FROM occurrence_taxon_universe u
    LEFT JOIN occurrence_wcvp_candidate_counts n
      ON u.vpjd_taxon_universe_id =
         n.vpjd_taxon_universe_id
    LEFT JOIN occurrence_wcvp_unique_exact m
      ON u.vpjd_taxon_universe_id =
         m.vpjd_taxon_universe_id
    "
  )
  
  # Resolve to accepted WCVP concepts -----------------------------------------
  
  message("Resolving matched WCVP names to accepted concepts...")
  
  DBI::dbExecute(
    con,
    "DROP TABLE IF EXISTS occurrence_wcvp_resolved"
  )
  
  DBI::dbExecute(
    con,
    "
    CREATE TABLE occurrence_wcvp_resolved AS
    SELECT
      r.*,
      CASE
        WHEN r.wcvp_match_status IN (
          'exact_accepted',
          'exact_nonaccepted'
        )
        THEN r.wcvp_accepted_plant_name_id
        ELSE NULL
      END AS resolved_wcvp_plant_name_id
    FROM occurrence_wcvp_reconciliation r
    "
  )
  
  DBI::dbExecute(
    con,
    "DROP TABLE IF EXISTS occurrence_wcvp_resolved_final"
  )
  
  DBI::dbExecute(
    con,
    "
    CREATE TABLE occurrence_wcvp_resolved_final AS
    SELECT
      r.*,
      a.accepted_wcvp_taxon_name,
      a.accepted_wcvp_taxon_authors,
      a.accepted_wcvp_taxon_rank,
      a.accepted_wcvp_scientific_name
    FROM occurrence_wcvp_resolved r
    LEFT JOIN wcvp_occurrence_accepted_lookup a
      ON r.resolved_wcvp_plant_name_id =
         a.accepted_wcvp_plant_name_id
    "
  )
  
  # Summaries ------------------------------------------------------------------
  
  match_summary <- DBI::dbGetQuery(
    con,
    "
    SELECT
      wcvp_match_status,
      COUNT(*) AS taxon_concepts,
      SUM(present_records) AS present_records
    FROM occurrence_wcvp_resolved_final
    GROUP BY wcvp_match_status
    ORDER BY taxon_concepts DESC
    "
  )
  
  taxonomic_status_summary <- DBI::dbGetQuery(
    con,
    "
    SELECT
      COALESCE(
        NULLIF(TRIM(wcvp_taxon_status), ''),
        '[unmatched]'
      ) AS wcvp_taxon_status,
      COUNT(*) AS taxon_concepts,
      SUM(present_records) AS present_records
    FROM occurrence_wcvp_resolved_final
    GROUP BY 1
    ORDER BY taxon_concepts DESC
    "
  )
  
  resolution_summary <- DBI::dbGetQuery(
    con,
    "
    SELECT
      COUNT(*) AS total_concepts,
      SUM(CAST(
        resolved_wcvp_plant_name_id IS NOT NULL
        AS INTEGER
      )) AS resolved_concepts,
      SUM(CAST(
        wcvp_match_status = 'unmatched'
        AS INTEGER
      )) AS unmatched_concepts,
      SUM(CAST(
        wcvp_match_status = 'ambiguous_exact'
        AS INTEGER
      )) AS ambiguous_concepts,
      SUM(CAST(
        wcvp_match_status = 'higher_rank'
        AS INTEGER
      )) AS higher_rank_concepts,
      COUNT(
        DISTINCT resolved_wcvp_plant_name_id
      ) AS distinct_accepted_wcvp_concepts
    FROM occurrence_wcvp_resolved_final
    "
  )
  
  # Review tables --------------------------------------------------------------
  
  unresolved <- DBI::dbGetQuery(
    con,
    "
    SELECT *
    FROM occurrence_wcvp_resolved_final
    WHERE wcvp_match_status IN (
      'unmatched',
      'ambiguous_exact'
    )
    ORDER BY present_records DESC, scientificName
    "
  )
  
  ambiguous_candidates <- DBI::dbGetQuery(
    con,
    "
    SELECT
      u.scientificName AS gbif_scientific_name,
      u.taxonRank AS gbif_taxon_rank,
      u.family AS gbif_family,
      u.genus AS gbif_genus,
      u.present_records,
      c.*
    FROM occurrence_wcvp_exact_candidates c
    INNER JOIN occurrence_taxon_universe u
      ON c.vpjd_taxon_universe_id =
         u.vpjd_taxon_universe_id
    INNER JOIN occurrence_wcvp_candidate_counts n
      ON c.vpjd_taxon_universe_id =
         n.vpjd_taxon_universe_id
    WHERE n.wcvp_candidate_count > 1
    ORDER BY
      u.present_records DESC,
      u.scientificName,
      c.wcvp_plant_name_id
    "
  )
  
  # Export ---------------------------------------------------------------------
  
  message("")
  message("Exporting reconciliation and audit tables...")
  
  reconciliation <- DBI::dbGetQuery(
    con,
    "
    SELECT *
    FROM occurrence_wcvp_resolved_final
    ORDER BY vpjd_taxon_universe_id
    "
  )
  
  readr::write_csv(
    reconciliation,
    file.path(
      output_dir,
      "vpjd_occurrence_wcvp_reconciliation.csv"
    )
  )
  
  readr::write_csv(
    match_summary,
    file.path(
      audit_dir,
      "occurrence_wcvp_match_summary.csv"
    )
  )
  
  readr::write_csv(
    taxonomic_status_summary,
    file.path(
      audit_dir,
      "occurrence_wcvp_taxonomic_status_summary.csv"
    )
  )
  
  readr::write_csv(
    resolution_summary,
    file.path(
      audit_dir,
      "occurrence_wcvp_resolution_summary.csv"
    )
  )
  
  readr::write_csv(
    unresolved,
    file.path(
      audit_dir,
      "occurrence_wcvp_unresolved_review.csv"
    )
  )
  
  readr::write_csv(
    ambiguous_candidates,
    file.path(
      audit_dir,
      "occurrence_wcvp_ambiguous_candidates.csv"
    )
  )
  
  # Metadata -------------------------------------------------------------------
  
  DBI::dbExecute(
    con,
    "DROP TABLE IF EXISTS occurrence_wcvp_metadata"
  )
  
  metadata_sql <- sprintf(
    "
    CREATE TABLE occurrence_wcvp_metadata AS
    SELECT
      '%s' AS module_version,
      '%s' AS source_table,
      %s AS source_taxon_concepts,
      %s AS source_present_records,
      %s AS resolved_concepts,
      %s AS distinct_accepted_wcvp_concepts,
      %s AS unmatched_concepts,
      %s AS ambiguous_concepts,
      %s AS higher_rank_concepts
    ",
    VPJD_OCCURRENCE_WCVP_VERSION,
    "occurrence_taxon_universe",
    resolution_summary$total_concepts[[1]],
    input_summary$present_records[[1]],
    resolution_summary$resolved_concepts[[1]],
    resolution_summary$distinct_accepted_wcvp_concepts[[1]],
    resolution_summary$unmatched_concepts[[1]],
    resolution_summary$ambiguous_concepts[[1]],
    resolution_summary$higher_rank_concepts[[1]]
  )
  
  DBI::dbExecute(con, metadata_sql)
  
  # Validation -----------------------------------------------------------------
  
  validation <- DBI::dbGetQuery(
    con,
    "
    SELECT
      COUNT(*) AS rows,
      COUNT(DISTINCT vpjd_taxon_universe_id) AS unique_ids,
      SUM(present_records) AS present_records
    FROM occurrence_wcvp_resolved_final
    "
  )
  
  if (
    validation$rows[[1]] !=
    input_summary$taxon_concepts[[1]]
  ) {
    stop(
      "Reconciliation changed the number of occurrence taxon concepts."
    )
  }
  
  if (
    validation$rows[[1]] !=
    validation$unique_ids[[1]]
  ) {
    stop(
      "Reconciliation contains duplicate vpjd_taxon_universe_id values."
    )
  }
  
  if (
    validation$present_records[[1]] !=
    input_summary$present_records[[1]]
  ) {
    stop(
      "Reconciliation changed the number of represented PRESENT records."
    )
  }
  
  # Final report ---------------------------------------------------------------
  
  message("")
  message("==============================================================")
  message("INITIAL WCVP RECONCILIATION COMPLETE")
  message("==============================================================")
  
  message(
    "Input GBIF taxon concepts: ",
    format(
      resolution_summary$total_concepts[[1]],
      big.mark = ",",
      scientific = FALSE
    )
  )
  
  message(
    "Resolved WCVP concepts: ",
    format(
      resolution_summary$resolved_concepts[[1]],
      big.mark = ",",
      scientific = FALSE
    )
  )
  
  message(
    "Distinct accepted WCVP concepts represented: ",
    format(
      resolution_summary$distinct_accepted_wcvp_concepts[[1]],
      big.mark = ",",
      scientific = FALSE
    )
  )
  
  message(
    "Unmatched concepts: ",
    format(
      resolution_summary$unmatched_concepts[[1]],
      big.mark = ",",
      scientific = FALSE
    )
  )
  
  message(
    "Ambiguous exact matches: ",
    format(
      resolution_summary$ambiguous_concepts[[1]],
      big.mark = ",",
      scientific = FALSE
    )
  )
  
  message(
    "Higher-rank concepts retained: ",
    format(
      resolution_summary$higher_rank_concepts[[1]],
      big.mark = ",",
      scientific = FALSE
    )
  )
  
  message("")
  message("Match status:")
  
  for (i in seq_len(nrow(match_summary))) {
    message(
      "  ",
      match_summary$wcvp_match_status[[i]],
      ": ",
      format(
        match_summary$taxon_concepts[[i]],
        big.mark = ",",
        scientific = FALSE
      ),
      " concepts"
    )
  }
  
  message("")
  message(
    "Reconciliation: ",
    file.path(
      output_dir,
      "vpjd_occurrence_wcvp_reconciliation.csv"
    )
  )
  
  message("Audit tables: ", audit_dir)
  message("")
  message("No fuzzy matching has been performed.")
  message("No occurrence-derived taxon concepts have been discarded.")
  message("")
  message(
    "04a_occurrence_wcvp_reconcile.R v",
    VPJD_OCCURRENCE_WCVP_VERSION,
    " complete."
  )
  
  invisible(list(
    resolution_summary = resolution_summary,
    match_summary = match_summary,
    taxonomic_status_summary = taxonomic_status_summary
  ))
}

reconcile_occurrence_taxa_wcvp()