# ==============================================================================
# 03_diagnose_additional_georeferenced.R
# VPJD / Oxford-Japan Plant Conservation Partnership
#
# Purpose:
#   Diagnose institutional GBIF records that:
#     1. were not present in the broad national GBIF harvest; and
#     2. nevertheless contain decimal coordinates.
#
# Questions:
# - Are these records classified outside Tracheophyta by GBIF?
# - What taxonomic ranks/classes/phyla do they represent?
# - Are they predominantly particular institutional datasets?
# - Are coordinates structurally usable?
# - What GBIF issues occur?
# - Which records are plausible candidates for later VPJD inclusion?
#
# IMPORTANT:
# - Diagnostic only.
# - Existing DuckDB tables are not modified.
# - No records are accepted/rejected here.
# - No WCVP reconciliation.
# - No Star assignment.
#
# Version: 0.1.0
# ==============================================================================

VPJD_INST_DIAG_VERSION <- "0.1.0"

required_packages <- c("here", "DBI", "duckdb", "readr", "dplyr", "cli")

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

# Paths ------------------------------------------------------------------------

duckdb_path <- here::here(
  "data", "interim", "occurrences",
  "vpjd_occurrences.duckdb"
)

output_dir <- here::here(
  "outputs", "tables", "GBIF",
  "institutional_integration",
  "additional_georeferenced_diagnostics"
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

# Connect ----------------------------------------------------------------------

con <- DBI::dbConnect(
  duckdb::duckdb(),
  dbdir = duckdb_path,
  read_only = TRUE
)

on.exit(
  DBI::dbDisconnect(con, shutdown = TRUE),
  add = TRUE
)

required_tables <- c(
  "gbif_raw",
  "institutional_occurrence_overlap"
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
  "VPJD additional institutional georeferenced diagnostic"
)

# Core population ---------------------------------------------------------------

core_where <- "
  NOT in_national_raw
  AND decimalLatitude IS NOT NULL
  AND TRIM(decimalLatitude) <> ''
  AND decimalLongitude IS NOT NULL
  AND TRIM(decimalLongitude) <> ''
"

core_n <- DBI::dbGetQuery(
  con,
  paste0(
    "SELECT COUNT(*) AS n
     FROM institutional_occurrence_overlap
     WHERE ",
    core_where
  )
)$n[[1]]

if (core_n != 39513L) {
  cli::cli_alert_warning(
    "Expected 39,513 additional georeferenced rows; found {format(core_n, big.mark = ',')}."
  )
}

cli::cli_text(
  "{.strong Additional georeferenced institutional records:} ",
  "{format(core_n, big.mark = ',')}"
)

# Overall diagnostic ------------------------------------------------------------

overall <- DBI::dbGetQuery(
  con,
  paste0(
    "
    SELECT
      COUNT(*) AS records,
      COUNT(DISTINCT gbifID) AS unique_gbif_ids,

      SUM(
        CASE WHEN occurrenceStatus = 'PRESENT'
        THEN 1 ELSE 0 END
      ) AS present_records,

      SUM(
        CASE WHEN occurrenceStatus = 'ABSENT'
        THEN 1 ELSE 0 END
      ) AS absent_records,

      SUM(
        CASE WHEN countryCode = 'JP'
        THEN 1 ELSE 0 END
      ) AS country_jp,

      SUM(
        CASE
          WHEN countryCode IS NULL
            OR TRIM(countryCode) = ''
          THEN 1 ELSE 0
        END
      ) AS country_missing,

      SUM(
        CASE
          WHEN UPPER(TRIM(phylum)) = 'TRACHEOPHYTA'
          THEN 1 ELSE 0
        END
      ) AS phylum_tracheophyta,

      SUM(
        CASE
          WHEN phylum IS NULL
            OR TRIM(phylum) = ''
          THEN 1 ELSE 0
        END
      ) AS phylum_missing,

      SUM(
        CASE
          WHEN UPPER(TRIM(phylum)) <> 'TRACHEOPHYTA'
          THEN 1 ELSE 0
        END
      ) AS phylum_other,

      SUM(
        CASE
          WHEN scientificName IS NULL
            OR TRIM(scientificName) = ''
          THEN 1 ELSE 0
        END
      ) AS scientific_name_missing,

      SUM(
        CASE
          WHEN taxonKey IS NULL
            OR TRIM(taxonKey) = ''
          THEN 1 ELSE 0
        END
      ) AS taxon_key_missing,

      SUM(
        CASE
          WHEN speciesKey IS NULL
            OR TRIM(speciesKey) = ''
          THEN 1 ELSE 0
        END
      ) AS species_key_missing

    FROM institutional_occurrence_overlap
    WHERE ",
    core_where
  )
)

# Coordinate validity -----------------------------------------------------------

coordinate_qc <- DBI::dbGetQuery(
  con,
  paste0(
    "
    WITH x AS (
      SELECT
        TRY_CAST(decimalLatitude AS DOUBLE) AS latitude,
        TRY_CAST(decimalLongitude AS DOUBLE) AS longitude
      FROM institutional_occurrence_overlap
      WHERE ",
    core_where,
    "
    )
    SELECT
      COUNT(*) AS records,

      SUM(
        CASE
          WHEN latitude IS NULL
            OR longitude IS NULL
          THEN 1 ELSE 0
        END
      ) AS non_numeric_coordinates,

      SUM(
        CASE
          WHEN latitude < -90
            OR latitude > 90
            OR longitude < -180
            OR longitude > 180
          THEN 1 ELSE 0
        END
      ) AS coordinates_out_of_range,

      SUM(
        CASE
          WHEN latitude = 0
            AND longitude = 0
          THEN 1 ELSE 0
        END
      ) AS zero_zero_coordinates,

      SUM(
        CASE
          WHEN latitude BETWEEN -90 AND 90
            AND longitude BETWEEN -180 AND 180
            AND NOT (
              latitude = 0
              AND longitude = 0
            )
          THEN 1 ELSE 0
        END
      ) AS structurally_usable_coordinates

    FROM x
    "
  )
)

# Phylum -----------------------------------------------------------------------

phylum_summary <- DBI::dbGetQuery(
  con,
  paste0(
    "
    SELECT
      COALESCE(NULLIF(TRIM(phylum), ''), 'MISSING') AS phylum,
      COUNT(*) AS records,
      COUNT(DISTINCT taxonKey) AS taxon_keys
    FROM institutional_occurrence_overlap
    WHERE ",
    core_where,
    "
    GROUP BY 1
    ORDER BY records DESC
    "
  )
)

# Class ------------------------------------------------------------------------

class_summary <- DBI::dbGetQuery(
  con,
  paste0(
    "
    SELECT
      COALESCE(NULLIF(TRIM(class), ''), 'MISSING') AS class,
      COUNT(*) AS records,
      COUNT(DISTINCT taxonKey) AS taxon_keys
    FROM institutional_occurrence_overlap
    WHERE ",
    core_where,
    "
    GROUP BY 1
    ORDER BY records DESC
    "
  )
)

# Taxonomic rank ---------------------------------------------------------------

rank_summary <- DBI::dbGetQuery(
  con,
  paste0(
    "
    SELECT
      COALESCE(NULLIF(TRIM(taxonRank), ''), 'MISSING') AS taxon_rank,
      COUNT(*) AS records,
      COUNT(DISTINCT taxonKey) AS taxon_keys
    FROM institutional_occurrence_overlap
    WHERE ",
    core_where,
    "
    GROUP BY 1
    ORDER BY records DESC
    "
  )
)

# Country code -----------------------------------------------------------------

country_summary <- DBI::dbGetQuery(
  con,
  paste0(
    "
    SELECT
      COALESCE(NULLIF(TRIM(countryCode), ''), 'MISSING') AS country_code,
      COUNT(*) AS records
    FROM institutional_occurrence_overlap
    WHERE ",
    core_where,
    "
    GROUP BY 1
    ORDER BY records DESC
    "
  )
)

# Occurrence status ------------------------------------------------------------

occurrence_status_summary <- DBI::dbGetQuery(
  con,
  paste0(
    "
    SELECT
      COALESCE(
        NULLIF(TRIM(occurrenceStatus), ''),
        'MISSING'
      ) AS occurrence_status,
      COUNT(*) AS records
    FROM institutional_occurrence_overlap
    WHERE ",
    core_where,
    "
    GROUP BY 1
    ORDER BY records DESC
    "
  )
)

# Basis of record --------------------------------------------------------------

basis_summary <- DBI::dbGetQuery(
  con,
  paste0(
    "
    SELECT
      COALESCE(
        NULLIF(TRIM(basisOfRecord), ''),
        'MISSING'
      ) AS basis_of_record,
      COUNT(*) AS records
    FROM institutional_occurrence_overlap
    WHERE ",
    core_where,
    "
    GROUP BY 1
    ORDER BY records DESC
    "
  )
)

# Institutional contribution ---------------------------------------------------

dataset_summary <- DBI::dbGetQuery(
  con,
  paste0(
    "
    SELECT
      vpjd_source_dataset_title AS dataset_title,
      vpjd_source_dataset_key AS dataset_key,
      COUNT(*) AS records,
      COUNT(DISTINCT taxonKey) AS taxon_keys,
      SUM(
        CASE
          WHEN UPPER(TRIM(phylum)) = 'TRACHEOPHYTA'
          THEN 1 ELSE 0
        END
      ) AS tracheophyta_records,
      SUM(
        CASE
          WHEN phylum IS NULL
            OR TRIM(phylum) = ''
            OR UPPER(TRIM(phylum)) <> 'TRACHEOPHYTA'
          THEN 1 ELSE 0
        END
      ) AS non_tracheophyta_or_missing
    FROM institutional_occurrence_overlap
    WHERE ",
    core_where,
    "
    GROUP BY 1, 2
    ORDER BY records DESC
    "
  )
)

# GBIF issues ------------------------------------------------------------------

issue_summary <- DBI::dbGetQuery(
  con,
  paste0(
    "
    SELECT
      COALESCE(NULLIF(TRIM(issue), ''), 'NONE') AS issue,
      COUNT(*) AS records
    FROM institutional_occurrence_overlap
    WHERE ",
    core_where,
    "
    GROUP BY 1
    ORDER BY records DESC
    "
  )
)

# Potential Tracheophyta records ------------------------------------------------
#
# These are NOT automatically accepted into VPJD.
# They are simply records whose current downloaded GBIF classification says
# Tracheophyta despite being absent from the broad national harvest.

tracheophyta_candidates <- DBI::dbGetQuery(
  con,
  paste0(
    "
    SELECT
      gbifID,
      occurrenceID,
      scientificName,
      verbatimScientificName,
      taxonRank,
      kingdom,
      phylum,
      class,
      \"order\",
      family,
      genus,
      species,
      taxonKey,
      speciesKey,
      occurrenceStatus,
      basisOfRecord,
      countryCode,
      stateProvince,
      locality,
      decimalLatitude,
      decimalLongitude,
      coordinateUncertaintyInMeters,
      year,
      issue,
      vpjd_source_dataset_title,
      vpjd_source_dataset_key,
      vpjd_source_download_key
    FROM institutional_occurrence_overlap
    WHERE ",
    core_where,
    "
      AND UPPER(TRIM(phylum)) = 'TRACHEOPHYTA'
    ORDER BY
      vpjd_source_dataset_title,
      scientificName,
      gbifID
    "
  )
)

# Non-Tracheophyta/missing-phylum records --------------------------------------

non_tracheophyta <- DBI::dbGetQuery(
  con,
  paste0(
    "
    SELECT
      gbifID,
      scientificName,
      taxonRank,
      kingdom,
      phylum,
      class,
      family,
      taxonKey,
      occurrenceStatus,
      decimalLatitude,
      decimalLongitude,
      issue,
      vpjd_source_dataset_title,
      vpjd_source_dataset_key
    FROM institutional_occurrence_overlap
    WHERE ",
    core_where,
    "
      AND (
        phylum IS NULL
        OR TRIM(phylum) = ''
        OR UPPER(TRIM(phylum)) <> 'TRACHEOPHYTA'
      )
    ORDER BY
      phylum,
      class,
      scientificName,
      gbifID
    "
  )
)

# Cross-tab: phylum × rank ------------------------------------------------------

phylum_rank_summary <- DBI::dbGetQuery(
  con,
  paste0(
    "
    SELECT
      COALESCE(NULLIF(TRIM(phylum), ''), 'MISSING') AS phylum,
      COALESCE(NULLIF(TRIM(taxonRank), ''), 'MISSING') AS taxon_rank,
      COUNT(*) AS records
    FROM institutional_occurrence_overlap
    WHERE ",
    core_where,
    "
    GROUP BY 1, 2
    ORDER BY records DESC
    "
  )
)

# Export -----------------------------------------------------------------------

readr::write_csv(
  overall,
  file.path(output_dir, "additional_georeferenced_overall.csv")
)

readr::write_csv(
  coordinate_qc,
  file.path(output_dir, "additional_georeferenced_coordinate_qc.csv")
)

readr::write_csv(
  phylum_summary,
  file.path(output_dir, "additional_georeferenced_phylum.csv")
)

readr::write_csv(
  class_summary,
  file.path(output_dir, "additional_georeferenced_class.csv")
)

readr::write_csv(
  rank_summary,
  file.path(output_dir, "additional_georeferenced_taxon_rank.csv")
)

readr::write_csv(
  country_summary,
  file.path(output_dir, "additional_georeferenced_country.csv")
)

readr::write_csv(
  occurrence_status_summary,
  file.path(output_dir, "additional_georeferenced_occurrence_status.csv")
)

readr::write_csv(
  basis_summary,
  file.path(output_dir, "additional_georeferenced_basis_of_record.csv")
)

readr::write_csv(
  dataset_summary,
  file.path(output_dir, "additional_georeferenced_datasets.csv")
)

readr::write_csv(
  issue_summary,
  file.path(output_dir, "additional_georeferenced_gbif_issues.csv")
)

readr::write_csv(
  phylum_rank_summary,
  file.path(output_dir, "additional_georeferenced_phylum_rank.csv")
)

readr::write_csv(
  tracheophyta_candidates,
  file.path(output_dir, "additional_georeferenced_tracheophyta_candidates.csv")
)

readr::write_csv(
  non_tracheophyta,
  file.path(output_dir, "additional_georeferenced_non_tracheophyta.csv")
)

# Console report ---------------------------------------------------------------

fmt <- function(x) {
  format(
    x,
    big.mark = ",",
    scientific = FALSE,
    trim = TRUE
  )
}

cli::cli_h2("Additional georeferenced diagnostic complete")

cli::cli_text(
  "{.strong Records examined:} {fmt(overall$records[[1]])}"
)

cli::cli_text(
  "{.strong Unique GBIF IDs:} {fmt(overall$unique_gbif_ids[[1]])}"
)

cli::cli_text(
  "{.strong PRESENT records:} {fmt(overall$present_records[[1]])}"
)

cli::cli_text(
  "{.strong ABSENT records:} {fmt(overall$absent_records[[1]])}"
)

cli::cli_text(
  "{.strong countryCode = JP:} {fmt(overall$country_jp[[1]])}"
)

cli::cli_text(
  "{.strong Tracheophyta records:} {fmt(overall$phylum_tracheophyta[[1]])}"
)

cli::cli_text(
  "{.strong Missing phylum:} {fmt(overall$phylum_missing[[1]])}"
)

cli::cli_text(
  "{.strong Other phyla:} {fmt(overall$phylum_other[[1]])}"
)

cli::cli_text(
  "{.strong Structurally usable coordinates:} ",
  "{fmt(coordinate_qc$structurally_usable_coordinates[[1]])}"
)

cli::cli_text(
  "{.strong Invalid/out-of-range coordinates:} ",
  "{fmt(coordinate_qc$coordinates_out_of_range[[1]])}"
)

cli::cli_text(
  "{.strong (0,0) coordinates:} ",
  "{fmt(coordinate_qc$zero_zero_coordinates[[1]])}"
)

cli::cli_text(
  "{.strong Tracheophyta candidates requiring explanation:} ",
  "{fmt(nrow(tracheophyta_candidates))}"
)

cli::cli_alert_success(
  "No existing occurrence tables were modified."
)

cli::cli_text(
  "03_diagnose_additional_georeferenced.R v{VPJD_INST_DIAG_VERSION} complete."
)