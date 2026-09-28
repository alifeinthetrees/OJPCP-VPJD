# ==============================================================================
# 03a_build_occurrence_taxon_universe.R
# VPJD / Oxford-Japan Plant Conservation Partnership
# Stage 3a: Build the occurrence-derived taxon universe
#
# Input:
#   data/interim/occurrences/vpjd_occurrences.duckdb
#   table: occurrences_clean
#
# Purpose:
#   Derive the distinct taxonomic universe represented by spatially usable
#   GBIF occurrence records from Japan, before WCVP reconciliation.
#
# Principles:
# - The taxon universe is occurrence-first.
# - PRESENT records define evidence of occurrence in Japan.
# - ABSENT records are retained as supporting information but do not establish
#   inclusion in the observed Japanese flora.
# - GBIF scientificName is preserved exactly.
# - verbatimScientificName is preserved separately.
# - Species and infraspecific ranks are retained.
# - Higher-rank records are retained for diagnosis/review.
# - No WCVP reconciliation occurs here.
# - No FOJ Star ratings are assigned here.
# - No binomial truncation occurs here.
#
# Version: 0.1.1
# ==============================================================================

VPJD_OCCURRENCE_TAXON_UNIVERSE_VERSION <- "0.1.1"

# Packages ---------------------------------------------------------------------

required_packages <- c("here", "DBI", "duckdb", "readr")
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
})

# Paths ------------------------------------------------------------------------

db_path <- here::here(
  "data", "interim", "occurrences", "vpjd_occurrences.duckdb"
)

output_dir <- here::here(
  "data", "interim", "taxonomy"
)

audit_dir <- here::here(
  "outputs", "tables", "taxonomy", "occurrence_taxon_universe"
)

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(audit_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(db_path)) {
  stop("VPJD occurrence DuckDB database not found:\n", db_path)
}

# Main -------------------------------------------------------------------------

build_occurrence_taxon_universe <- function() {
  
  message("")
  message("==============================================================")
  message("VPJD OCCURRENCE-DERIVED TAXON UNIVERSE")
  message("==============================================================")
  message("Module version: ", VPJD_OCCURRENCE_TAXON_UNIVERSE_VERSION)
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
  
  tables <- DBI::dbListTables(con)
  
  if (!"occurrences_clean" %in% tables) {
    stop("Required table 'occurrences_clean' not found.")
  }
  
  # Validate occurrence input --------------------------------------------------
  
  input_summary <- DBI::dbGetQuery(
    con,
    "
    SELECT
      COUNT(*) AS spatially_usable_records,
      SUM(CAST(
        UPPER(COALESCE(NULLIF(TRIM(occurrenceStatus), ''), '')) = 'PRESENT'
        AS INTEGER
      )) AS present_records,
      SUM(CAST(
        UPPER(COALESCE(NULLIF(TRIM(occurrenceStatus), ''), '')) = 'ABSENT'
        AS INTEGER
      )) AS absent_records
    FROM occurrences_clean
    "
  )
  
  message("")
  message(
    "Spatially usable records: ",
    format(
      input_summary$spatially_usable_records[[1]],
      big.mark = ",",
      scientific = FALSE
    )
  )
  message(
    "PRESENT records: ",
    format(
      input_summary$present_records[[1]],
      big.mark = ",",
      scientific = FALSE
    )
  )
  message(
    "ABSENT records: ",
    format(
      input_summary$absent_records[[1]],
      big.mark = ",",
      scientific = FALSE
    )
  )
  
  if (
    as.numeric(input_summary$present_records[[1]]) +
    as.numeric(input_summary$absent_records[[1]]) !=
    as.numeric(input_summary$spatially_usable_records[[1]])
  ) {
    warning(
      "Occurrence statuses other than PRESENT/ABSENT are represented in ",
      "occurrences_clean."
    )
  }
  
  # Build GBIF taxon concepts --------------------------------------------------
  
  message("")
  message("Building distinct GBIF taxon-concept table...")
  
  DBI::dbExecute(
    con,
    "DROP TABLE IF EXISTS occurrence_taxon_concepts"
  )
  
  DBI::dbExecute(
    con,
    "
    CREATE TABLE occurrence_taxon_concepts AS
    SELECT
      taxonKey,
      speciesKey,
      kingdom,
      phylum,
      class,
      \"order\" AS taxonomicOrder,
      family,
      genus,
      species,
      infraspecificEpithet,
      taxonRank,
      scientificName,
      COUNT(*) AS total_records,
      SUM(CAST(
        UPPER(COALESCE(NULLIF(TRIM(occurrenceStatus), ''), '')) = 'PRESENT'
        AS INTEGER
      )) AS present_records,
      SUM(CAST(
        UPPER(COALESCE(NULLIF(TRIM(occurrenceStatus), ''), '')) = 'ABSENT'
        AS INTEGER
      )) AS absent_records,
      COUNT(DISTINCT datasetKey) AS n_datasets,
      MIN(vpjd_year) AS earliest_year,
      MAX(vpjd_year) AS latest_year
    FROM occurrences_clean
    GROUP BY
      taxonKey,
      speciesKey,
      kingdom,
      phylum,
      class,
      \"order\",
      family,
      genus,
      species,
      infraspecificEpithet,
      taxonRank,
      scientificName
    "
  )
  
  concept_count <- DBI::dbGetQuery(
    con,
    "SELECT COUNT(*) AS n FROM occurrence_taxon_concepts"
  )$n[[1]]
  
  message(
    "Distinct GBIF taxon concepts: ",
    format(concept_count, big.mark = ",", scientific = FALSE)
  )
  
  # Build PRESENT taxon universe -----------------------------------------------
  
  message("Building PRESENT occurrence-derived taxon universe...")
  
  DBI::dbExecute(
    con,
    "DROP TABLE IF EXISTS occurrence_taxon_universe"
  )
  
  DBI::dbExecute(
    con,
    "
    CREATE TABLE occurrence_taxon_universe AS
    SELECT
      row_number() OVER (
        ORDER BY
          CASE
            WHEN TRY_CAST(taxonKey AS BIGINT) IS NULL THEN 1
            ELSE 0
          END,
          TRY_CAST(taxonKey AS BIGINT),
          scientificName
      ) AS vpjd_taxon_universe_id,
      *,
      CASE
        WHEN UPPER(COALESCE(taxonRank, '')) IN (
          'SPECIES', 'SUBSPECIES', 'VARIETY', 'FORM'
        )
        THEN TRUE
        ELSE FALSE
      END AS taxonomically_resolved_for_vpjd,
      CASE
        WHEN UPPER(COALESCE(taxonRank, '')) = 'SPECIES'
          THEN 'species'
        WHEN UPPER(COALESCE(taxonRank, '')) IN (
          'SUBSPECIES', 'VARIETY', 'FORM'
        )
          THEN 'infraspecific'
        ELSE 'higher_rank'
      END AS vpjd_resolution_class
    FROM occurrence_taxon_concepts
    WHERE present_records > 0
    "
  )
  
  universe_count <- DBI::dbGetQuery(
    con,
    "SELECT COUNT(*) AS n FROM occurrence_taxon_universe"
  )$n[[1]]
  
  # Summaries ------------------------------------------------------------------
  
  resolution_summary <- DBI::dbGetQuery(
    con,
    "
    SELECT
      vpjd_resolution_class,
      COUNT(*) AS taxa,
      SUM(present_records) AS present_records
    FROM occurrence_taxon_universe
    GROUP BY vpjd_resolution_class
    ORDER BY taxa DESC
    "
  )
  
  rank_summary <- DBI::dbGetQuery(
    con,
    "
    SELECT
      COALESCE(NULLIF(TRIM(taxonRank), ''), '[missing]') AS taxonRank,
      COUNT(*) AS taxa,
      SUM(present_records) AS present_records
    FROM occurrence_taxon_universe
    GROUP BY 1
    ORDER BY taxa DESC
    "
  )
  
  name_summary <- DBI::dbGetQuery(
    con,
    "
    SELECT
      COUNT(*) AS total_taxon_concepts,
      SUM(CAST(
        NULLIF(TRIM(scientificName), '') IS NULL
        AS INTEGER
      )) AS missing_scientific_name,
      SUM(CAST(
        NULLIF(TRIM(taxonKey), '') IS NULL
        AS INTEGER
      )) AS missing_taxon_key,
      SUM(CAST(
        NULLIF(TRIM(speciesKey), '') IS NULL
        AS INTEGER
      )) AS missing_species_key,
      COUNT(DISTINCT scientificName) AS distinct_scientific_names,
      COUNT(DISTINCT taxonKey) AS distinct_taxon_keys,
      COUNT(DISTINCT speciesKey) AS distinct_species_keys
    FROM occurrence_taxon_universe
    "
  )
  
  # Verbatim names -------------------------------------------------------------
  
  message("Summarising verbatim taxonomic determinations...")
  
  DBI::dbExecute(
    con,
    "DROP TABLE IF EXISTS occurrence_verbatim_names"
  )
  
  DBI::dbExecute(
    con,
    "
    CREATE TABLE occurrence_verbatim_names AS
    SELECT
      taxonKey,
      scientificName,
      verbatimScientificName,
      COUNT(*) AS records
    FROM occurrences_clean
    WHERE
      UPPER(COALESCE(NULLIF(TRIM(occurrenceStatus), ''), '')) = 'PRESENT'
      AND NULLIF(TRIM(verbatimScientificName), '') IS NOT NULL
    GROUP BY
      taxonKey,
      scientificName,
      verbatimScientificName
    "
  )
  
  verbatim_summary <- DBI::dbGetQuery(
    con,
    "
    SELECT
      COUNT(*) AS taxon_verbatim_combinations,
      COUNT(DISTINCT verbatimScientificName) AS distinct_verbatim_names
    FROM occurrence_verbatim_names
    "
  )
  
  # Dataset representation -----------------------------------------------------
  
  dataset_taxon_summary <- DBI::dbGetQuery(
    con,
    "
    SELECT
      datasetKey,
      COUNT(*) AS present_records,
      COUNT(DISTINCT taxonKey) AS distinct_taxon_keys
    FROM occurrences_clean
    WHERE
      UPPER(COALESCE(NULLIF(TRIM(occurrenceStatus), ''), '')) = 'PRESENT'
    GROUP BY datasetKey
    ORDER BY present_records DESC
    "
  )
  
  # Higher-rank review ---------------------------------------------------------
  
  higher_rank_taxa <- DBI::dbGetQuery(
    con,
    "
    SELECT
      vpjd_taxon_universe_id,
      taxonKey,
      speciesKey,
      taxonRank,
      scientificName,
      family,
      genus,
      present_records,
      absent_records,
      n_datasets,
      earliest_year,
      latest_year
    FROM occurrence_taxon_universe
    WHERE vpjd_resolution_class = 'higher_rank'
    ORDER BY present_records DESC, scientificName
    "
  )
  
  # Export ---------------------------------------------------------------------
  
  message("")
  message("Exporting taxon universe and audit tables...")
  
  taxon_universe <- DBI::dbGetQuery(
    con,
    "
    SELECT *
    FROM occurrence_taxon_universe
    ORDER BY vpjd_taxon_universe_id
    "
  )
  
  readr::write_csv(
    taxon_universe,
    file.path(output_dir, "vpjd_occurrence_taxon_universe.csv")
  )
  
  readr::write_csv(
    resolution_summary,
    file.path(audit_dir, "occurrence_taxon_resolution_summary.csv")
  )
  
  readr::write_csv(
    rank_summary,
    file.path(audit_dir, "occurrence_taxon_rank_summary.csv")
  )
  
  readr::write_csv(
    name_summary,
    file.path(audit_dir, "occurrence_taxon_name_summary.csv")
  )
  
  readr::write_csv(
    verbatim_summary,
    file.path(audit_dir, "occurrence_verbatim_name_summary.csv")
  )
  
  readr::write_csv(
    dataset_taxon_summary,
    file.path(audit_dir, "occurrence_dataset_taxon_summary.csv")
  )
  
  readr::write_csv(
    higher_rank_taxa,
    file.path(audit_dir, "occurrence_higher_rank_taxa_review.csv")
  )
  
  # Metadata -------------------------------------------------------------------
  
  DBI::dbExecute(
    con,
    "DROP TABLE IF EXISTS occurrence_taxon_universe_metadata"
  )
  
  metadata_sql <- sprintf(
    "
    CREATE TABLE occurrence_taxon_universe_metadata AS
    SELECT
      '%s' AS module_version,
      '%s' AS source_table,
      %s AS source_spatial_records,
      %s AS source_present_records,
      %s AS source_absent_records,
      %s AS distinct_taxon_concepts,
      %s AS present_taxon_universe
    ",
    VPJD_OCCURRENCE_TAXON_UNIVERSE_VERSION,
    "occurrences_clean",
    input_summary$spatially_usable_records[[1]],
    input_summary$present_records[[1]],
    input_summary$absent_records[[1]],
    concept_count,
    universe_count
  )
  
  DBI::dbExecute(con, metadata_sql)
  
  # Validation -----------------------------------------------------------------
  
  validation <- DBI::dbGetQuery(
    con,
    "
    SELECT
      COUNT(*) AS taxa,
      COUNT(DISTINCT vpjd_taxon_universe_id) AS unique_vpjd_ids,
      SUM(CAST(
        present_records <= 0
        AS INTEGER
      )) AS taxa_without_present_records
    FROM occurrence_taxon_universe
    "
  )
  
  if (validation$taxa[[1]] != validation$unique_vpjd_ids[[1]]) {
    stop("vpjd_taxon_universe_id is not unique.")
  }
  
  if (validation$taxa_without_present_records[[1]] != 0) {
    stop(
      "Occurrence taxon universe contains taxa without PRESENT records."
    )
  }
  
  # Final output ---------------------------------------------------------------
  
  message("")
  message("==============================================================")
  message("OCCURRENCE TAXON UNIVERSE COMPLETE")
  message("==============================================================")
  message(
    "Distinct GBIF concepts (PRESENT + ABSENT): ",
    format(concept_count, big.mark = ",", scientific = FALSE)
  )
  message(
    "PRESENT occurrence-derived taxon universe: ",
    format(universe_count, big.mark = ",", scientific = FALSE)
  )
  message("")
  message("Resolution:")
  
  for (i in seq_len(nrow(resolution_summary))) {
    message(
      "  ",
      resolution_summary$vpjd_resolution_class[[i]],
      ": ",
      format(
        resolution_summary$taxa[[i]],
        big.mark = ",",
        scientific = FALSE
      ),
      " taxa"
    )
  }
  
  message("")
  message(
    "Distinct scientific names: ",
    format(
      name_summary$distinct_scientific_names[[1]],
      big.mark = ",",
      scientific = FALSE
    )
  )
  message(
    "Distinct GBIF taxonKeys: ",
    format(
      name_summary$distinct_taxon_keys[[1]],
      big.mark = ",",
      scientific = FALSE
    )
  )
  message("")
  message(
    "Taxon universe: ",
    file.path(output_dir, "vpjd_occurrence_taxon_universe.csv")
  )
  message("Audit tables: ", audit_dir)
  message("")
  message("No WCVP reconciliation has been performed.")
  message("No taxon concepts have been discarded.")
  message("")
  message(
    "03a_build_occurrence_taxon_universe.R v",
    VPJD_OCCURRENCE_TAXON_UNIVERSE_VERSION,
    " complete."
  )
  
  invisible(list(
    concept_count = concept_count,
    universe_count = universe_count,
    resolution_summary = resolution_summary,
    rank_summary = rank_summary,
    name_summary = name_summary
  ))
}

build_occurrence_taxon_universe()