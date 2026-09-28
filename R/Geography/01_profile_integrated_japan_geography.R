# ==============================================================================
# VPJD-OJPCP
# 01_profile_integrated_japan_geography.R
# Version: 0.1.0
#
# Purpose:
#   Profile geographic information in the frozen integrated VPJD Japan
#   occurrence layer before assigning records to botanical geographic areas.
#
# This module is diagnostic only.
#
# It does NOT:
#   - assign botanical prefectures;
#   - modify coordinates;
#   - interpret stateProvince values;
#   - spatially join records to polygons;
#   - remove records;
#   - modify frozen upstream tables.
#
# Required frozen inputs:
#   Integration 04 v0.1.0
# ==============================================================================

suppressPackageStartupMessages({
  library(DBI)
  library(duckdb)
  library(dplyr)
  library(tibble)
  library(readr)
  library(here)
})

SCRIPT_VERSION <- "0.1.0"
EXPECTED_INTEGRATION_04_VERSION <- "0.1.0"
EXPECTED_TOTAL <- 3113089L
EXPECTED_NATIONAL <- 3099799L
EXPECTED_RECOVERED <- 13290L

get_metadata_value <- function(con, table_name, metric_name) {
  fields <- dbListFields(con, table_name)
  if (all(c("metric", "value") %in% fields)) {
    sql <- paste0(
      "SELECT CAST(value AS VARCHAR) AS value FROM ",
      dbQuoteIdentifier(con, table_name),
      " WHERE CAST(metric AS VARCHAR) = ?"
    )
    x <- dbGetQuery(con, sql, params = list(metric_name))
    if (nrow(x) != 1) {
      stop(
        "Expected one '", metric_name,
        "' row in ", table_name,
        "; found ", nrow(x), "."
      )
    }
    return(as.character(x$value[[1]]))
  }
  if (metric_name %in% fields) {
    x <- dbReadTable(con, table_name)
    if (nrow(x) != 1) {
      stop("Expected one row in ", table_name, ".")
    }
    return(as.character(x[[metric_name]][[1]]))
  }
  stop(
    "Metadata item '", metric_name,
    "' not found in ", table_name, "."
  )
}

run_geography_profile <- function() {
  cat("\n— VPJD integrated Japan geography profile —\n\n")
  
  db_path <- here(
    "data", "interim", "occurrences",
    "vpjd_occurrences.duckdb"
  )
  
  if (!file.exists(db_path)) {
    stop("DuckDB not found: ", db_path)
  }
  
  con <- dbConnect(
    duckdb::duckdb(),
    dbdir = db_path,
    read_only = FALSE
  )
  
  on.exit(
    dbDisconnect(con, shutdown = TRUE),
    add = TRUE
  )
  
  required_tables <- c(
    "vpjd_japan_occurrences_integrated",
    "vpjd_japan_occurrences_integrated_metadata"
  )
  
  missing_tables <- setdiff(
    required_tables,
    dbListTables(con)
  )
  
  if (length(missing_tables) > 0) {
    stop(
      "Required tables missing: ",
      paste(missing_tables, collapse = ", ")
    )
  }
  
  # ---------------------------------------------------------------------------
  # Validate frozen Integration 04
  # ---------------------------------------------------------------------------
  
  integration_04_version <- get_metadata_value(
    con,
    "vpjd_japan_occurrences_integrated_metadata",
    "script_version"
  )
  
  if (integration_04_version != EXPECTED_INTEGRATION_04_VERSION) {
    stop(
      "Expected Integration 04 v",
      EXPECTED_INTEGRATION_04_VERSION,
      "; detected ",
      integration_04_version, "."
    )
  }
  
  fields <- dbListFields(
    con,
    "vpjd_japan_occurrences_integrated"
  )
  
  required_fields <- c(
    "gbifID",
    "countryCode",
    "stateProvince",
    "locality",
    "ANALYTICAL_DECIMAL_LATITUDE",
    "ANALYTICAL_DECIMAL_LONGITUDE",
    "ANALYTICAL_COORDINATE_UNCERTAINTY_M",
    "OCCURRENCE_SOURCE",
    "ANALYTICAL_COORDINATE_SOURCE",
    "FINAL_WCVP_ID"
  )
  
  missing_fields <- setdiff(
    required_fields,
    fields
  )
  
  if (length(missing_fields) > 0) {
    stop(
      "Integrated occurrence layer missing: ",
      paste(missing_fields, collapse = ", ")
    )
  }
  
  cat(
    "Integration 04 version: ",
    integration_04_version, "\n"
  )
  
  cat("Required-field validation: PASS\n\n")
  
  # ---------------------------------------------------------------------------
  # Basic accounting
  # ---------------------------------------------------------------------------
  
  accounting <- dbGetQuery(
    con,
    "
    SELECT
      COUNT(*) AS total_records,
      COUNT(DISTINCT gbifID) AS distinct_gbif_ids,
      SUM(
        CASE WHEN OCCURRENCE_SOURCE = 'national_gbif'
        THEN 1 ELSE 0 END
      ) AS national_records,
      SUM(
        CASE WHEN OCCURRENCE_SOURCE = 'recovered_institutional'
        THEN 1 ELSE 0 END
      ) AS recovered_records
    FROM vpjd_japan_occurrences_integrated
    "
  ) %>%
    as_tibble()
  
  if (accounting$total_records[[1]] != EXPECTED_TOTAL) {
    stop(
      "Unexpected integrated record count: ",
      accounting$total_records[[1]], "."
    )
  }
  
  if (accounting$distinct_gbif_ids[[1]] != EXPECTED_TOTAL) {
    stop("Integrated gbifID uniqueness check failed.")
  }
  
  if (accounting$national_records[[1]] != EXPECTED_NATIONAL) {
    stop("Unexpected national record count.")
  }
  
  if (accounting$recovered_records[[1]] != EXPECTED_RECOVERED) {
    stop("Unexpected recovered record count.")
  }
  
  # ---------------------------------------------------------------------------
  # Geographic-field completeness
  # ---------------------------------------------------------------------------
  
  geographic_completeness <- dbGetQuery(
    con,
    "
    SELECT
      OCCURRENCE_SOURCE,
      COUNT(*) AS occurrence_records,

      SUM(
        CASE
          WHEN stateProvince IS NOT NULL
           AND TRIM(CAST(stateProvince AS VARCHAR)) <> ''
          THEN 1 ELSE 0
        END
      ) AS stateProvince_present,

      SUM(
        CASE
          WHEN locality IS NOT NULL
           AND TRIM(CAST(locality AS VARCHAR)) <> ''
          THEN 1 ELSE 0
        END
      ) AS locality_present,

      SUM(
        CASE
          WHEN ANALYTICAL_DECIMAL_LATITUDE IS NOT NULL
           AND ANALYTICAL_DECIMAL_LONGITUDE IS NOT NULL
          THEN 1 ELSE 0
        END
      ) AS coordinates_present,

      SUM(
        CASE
          WHEN ANALYTICAL_COORDINATE_UNCERTAINTY_M IS NOT NULL
          THEN 1 ELSE 0
        END
      ) AS uncertainty_present

    FROM vpjd_japan_occurrences_integrated
    GROUP BY OCCURRENCE_SOURCE
    ORDER BY OCCURRENCE_SOURCE
    "
  ) %>%
    as_tibble() %>%
    mutate(
      pct_stateProvince =
        round(100 * stateProvince_present / occurrence_records, 2),
      pct_locality =
        round(100 * locality_present / occurrence_records, 2),
      pct_coordinates =
        round(100 * coordinates_present / occurrence_records, 2),
      pct_uncertainty =
        round(100 * uncertainty_present / occurrence_records, 2)
    )
  
  # ---------------------------------------------------------------------------
  # countryCode profile
  # ---------------------------------------------------------------------------
  
  country_profile <- dbGetQuery(
    con,
    "
    SELECT
      COALESCE(
        NULLIF(TRIM(CAST(countryCode AS VARCHAR)), ''),
        '[missing]'
      ) AS countryCode,
      COUNT(*) AS occurrence_records
    FROM vpjd_japan_occurrences_integrated
    GROUP BY 1
    ORDER BY occurrence_records DESC
    "
  ) %>%
    as_tibble()
  
  # ---------------------------------------------------------------------------
  # stateProvince profile
  # ---------------------------------------------------------------------------
  
  state_profile <- dbGetQuery(
    con,
    "
    SELECT
      COALESCE(
        NULLIF(TRIM(CAST(stateProvince AS VARCHAR)), ''),
        '[missing]'
      ) AS stateProvince,
      OCCURRENCE_SOURCE,
      COUNT(*) AS occurrence_records,
      COUNT(DISTINCT FINAL_WCVP_ID) AS distinct_wcvp_ids
    FROM vpjd_japan_occurrences_integrated
    GROUP BY 1, 2
    ORDER BY occurrence_records DESC
    "
  ) %>%
    as_tibble()
  
  state_totals <- dbGetQuery(
    con,
    "
    SELECT
      COALESCE(
        NULLIF(TRIM(CAST(stateProvince AS VARCHAR)), ''),
        '[missing]'
      ) AS stateProvince,
      COUNT(*) AS occurrence_records,
      COUNT(DISTINCT FINAL_WCVP_ID) AS distinct_wcvp_ids
    FROM vpjd_japan_occurrences_integrated
    GROUP BY 1
    ORDER BY occurrence_records DESC
    "
  ) %>%
    as_tibble()
  
  # ---------------------------------------------------------------------------
  # Coordinate ranges
  # ---------------------------------------------------------------------------
  
  coordinate_range <- dbGetQuery(
    con,
    "
    SELECT
      OCCURRENCE_SOURCE,
      MIN(ANALYTICAL_DECIMAL_LATITUDE) AS min_latitude,
      MAX(ANALYTICAL_DECIMAL_LATITUDE) AS max_latitude,
      MIN(ANALYTICAL_DECIMAL_LONGITUDE) AS min_longitude,
      MAX(ANALYTICAL_DECIMAL_LONGITUDE) AS max_longitude
    FROM vpjd_japan_occurrences_integrated
    GROUP BY OCCURRENCE_SOURCE
    ORDER BY OCCURRENCE_SOURCE
    "
  ) %>%
    as_tibble()
  
  # ---------------------------------------------------------------------------
  # Coordinate uncertainty profile
  # ---------------------------------------------------------------------------
  
  uncertainty_profile <- dbGetQuery(
    con,
    "
    SELECT
      OCCURRENCE_SOURCE,
      COUNT(*) AS occurrence_records,

      SUM(
        CASE
          WHEN ANALYTICAL_COORDINATE_UNCERTAINTY_M IS NULL
          THEN 1 ELSE 0
        END
      ) AS uncertainty_missing,

      SUM(
        CASE
          WHEN ANALYTICAL_COORDINATE_UNCERTAINTY_M = 0
          THEN 1 ELSE 0
        END
      ) AS uncertainty_zero,

      SUM(
        CASE
          WHEN ANALYTICAL_COORDINATE_UNCERTAINTY_M > 0
           AND ANALYTICAL_COORDINATE_UNCERTAINTY_M <= 1000
          THEN 1 ELSE 0
        END
      ) AS uncertainty_gt0_le1km,

      SUM(
        CASE
          WHEN ANALYTICAL_COORDINATE_UNCERTAINTY_M > 1000
           AND ANALYTICAL_COORDINATE_UNCERTAINTY_M <= 10000
          THEN 1 ELSE 0
        END
      ) AS uncertainty_gt1_le10km,

      SUM(
        CASE
          WHEN ANALYTICAL_COORDINATE_UNCERTAINTY_M > 10000
           AND ANALYTICAL_COORDINATE_UNCERTAINTY_M <= 100000
          THEN 1 ELSE 0
        END
      ) AS uncertainty_gt10_le100km,

      SUM(
        CASE
          WHEN ANALYTICAL_COORDINATE_UNCERTAINTY_M > 100000
          THEN 1 ELSE 0
        END
      ) AS uncertainty_gt100km

    FROM vpjd_japan_occurrences_integrated
    GROUP BY OCCURRENCE_SOURCE
    ORDER BY OCCURRENCE_SOURCE
    "
  ) %>%
    as_tibble()
  
  # ---------------------------------------------------------------------------
  # Broad coordinate-envelope diagnostics
  #
  # Deliberately broad. This is NOT botanical-prefecture assignment and is
  # not intended to define Japan's final spatial boundary.
  # ---------------------------------------------------------------------------
  
  envelope_profile <- dbGetQuery(
    con,
    "
    SELECT
      OCCURRENCE_SOURCE,
      COUNT(*) AS occurrence_records,

      SUM(
        CASE
          WHEN ANALYTICAL_DECIMAL_LATITUDE BETWEEN 20 AND 50
           AND ANALYTICAL_DECIMAL_LONGITUDE BETWEEN 120 AND 155
          THEN 1 ELSE 0
        END
      ) AS within_broad_japan_envelope,

      SUM(
        CASE
          WHEN NOT (
            ANALYTICAL_DECIMAL_LATITUDE BETWEEN 20 AND 50
            AND ANALYTICAL_DECIMAL_LONGITUDE BETWEEN 120 AND 155
          )
          THEN 1 ELSE 0
        END
      ) AS outside_broad_japan_envelope

    FROM vpjd_japan_occurrences_integrated
    GROUP BY OCCURRENCE_SOURCE
    ORDER BY OCCURRENCE_SOURCE
    "
  ) %>%
    as_tibble()
  
  # ---------------------------------------------------------------------------
  # Distinct stateProvince values by source
  # ---------------------------------------------------------------------------
  
  state_distinct <- dbGetQuery(
    con,
    "
    SELECT
      OCCURRENCE_SOURCE,
      COUNT(
        DISTINCT NULLIF(
          TRIM(CAST(stateProvince AS VARCHAR)),
          ''
        )
      ) AS distinct_stateProvince_values
    FROM vpjd_japan_occurrences_integrated
    GROUP BY OCCURRENCE_SOURCE
    ORDER BY OCCURRENCE_SOURCE
    "
  ) %>%
    as_tibble()
  
  # ---------------------------------------------------------------------------
  # Most common locality values
  # ---------------------------------------------------------------------------
  
  locality_profile <- dbGetQuery(
    con,
    "
    SELECT
      OCCURRENCE_SOURCE,
      TRIM(CAST(locality AS VARCHAR)) AS locality,
      COUNT(*) AS occurrence_records
    FROM vpjd_japan_occurrences_integrated
    WHERE locality IS NOT NULL
      AND TRIM(CAST(locality AS VARCHAR)) <> ''
    GROUP BY 1, 2
    ORDER BY occurrence_records DESC
    LIMIT 100
    "
  ) %>%
    as_tibble()
  
  # ---------------------------------------------------------------------------
  # Metadata
  # ---------------------------------------------------------------------------
  
  metadata <- tibble(
    metric = c(
      "script_version",
      "input_integration_04_version",
      "integrated_occurrence_records",
      "distinct_gbif_ids",
      "national_records",
      "recovered_records",
      "botanical_areas_assigned",
      "coordinates_modified",
      "records_removed",
      "taxonomy_modified",
      "upstream_tables_modified"
    ),
    value = c(
      SCRIPT_VERSION,
      integration_04_version,
      as.character(accounting$total_records[[1]]),
      as.character(accounting$distinct_gbif_ids[[1]]),
      as.character(accounting$national_records[[1]]),
      as.character(accounting$recovered_records[[1]]),
      "FALSE",
      "FALSE",
      "0",
      "FALSE",
      "FALSE"
    )
  )
  
  # ---------------------------------------------------------------------------
  # Persist diagnostics
  # ---------------------------------------------------------------------------
  
  dbWriteTable(
    con,
    "japan_geography_completeness",
    geographic_completeness,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "japan_geography_state_profile",
    state_profile,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "japan_geography_state_totals",
    state_totals,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "japan_geography_coordinate_range",
    coordinate_range,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "japan_geography_uncertainty_profile",
    uncertainty_profile,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "japan_geography_envelope_profile",
    envelope_profile,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "japan_geography_profile_metadata",
    metadata,
    overwrite = TRUE
  )
  
  out_dir <- here(
    "outputs",
    "tables",
    "geography",
    "integrated_japan_profile"
  )
  
  dir.create(
    out_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  write_csv(
    geographic_completeness,
    file.path(out_dir, "geographic_completeness.csv")
  )
  
  write_csv(
    country_profile,
    file.path(out_dir, "country_profile.csv")
  )
  
  write_csv(
    state_profile,
    file.path(out_dir, "stateProvince_by_source.csv")
  )
  
  write_csv(
    state_totals,
    file.path(out_dir, "stateProvince_totals.csv")
  )
  
  write_csv(
    state_distinct,
    file.path(out_dir, "stateProvince_distinct_counts.csv")
  )
  
  write_csv(
    coordinate_range,
    file.path(out_dir, "coordinate_range.csv")
  )
  
  write_csv(
    uncertainty_profile,
    file.path(out_dir, "coordinate_uncertainty.csv")
  )
  
  write_csv(
    envelope_profile,
    file.path(out_dir, "broad_coordinate_envelope.csv")
  )
  
  write_csv(
    locality_profile,
    file.path(out_dir, "top_100_localities.csv")
  )
  
  write_csv(
    metadata,
    file.path(out_dir, "metadata.csv")
  )
  
  # ---------------------------------------------------------------------------
  # Console report
  # ---------------------------------------------------------------------------
  
  cat("— Accounting —\n\n")
  print(accounting, n = Inf)
  
  cat("\n— Geographic-field completeness —\n\n")
  print(geographic_completeness, n = Inf)
  
  cat("\n— countryCode profile —\n\n")
  print(country_profile, n = Inf)
  
  cat("\n— Distinct stateProvince values —\n\n")
  print(state_distinct, n = Inf)
  
  cat("\n— Largest stateProvince values —\n\n")
  print(head(state_totals, 60), n = 60)
  
  cat("\n— Coordinate ranges —\n\n")
  print(coordinate_range, n = Inf)
  
  cat("\n— Coordinate uncertainty —\n\n")
  print(uncertainty_profile, n = Inf)
  
  cat("\n— Broad Japan coordinate envelope —\n\n")
  print(envelope_profile, n = Inf)
  
  cat("\n— Most frequent locality values —\n\n")
  print(head(locality_profile, 40), n = 40)
  
  cat("\n— Safety —\n")
  cat("Botanical areas assigned: FALSE\n")
  cat("Coordinates modified: FALSE\n")
  cat("Records removed: 0\n")
  cat("Taxonomy modified: FALSE\n")
  cat("Frozen upstream tables modified: FALSE\n")
  cat("Output status: GEOGRAPHIC DIAGNOSTIC ONLY\n")
  
  cat(
    "\n01_profile_integrated_japan_geography.R v",
    SCRIPT_VERSION,
    " complete.\n",
    sep = ""
  )
  
  invisible(
    list(
      accounting = accounting,
      geographic_completeness = geographic_completeness,
      country_profile = country_profile,
      state_profile = state_profile,
      state_totals = state_totals,
      state_distinct = state_distinct,
      coordinate_range = coordinate_range,
      uncertainty_profile = uncertainty_profile,
      envelope_profile = envelope_profile,
      locality_profile = locality_profile,
      metadata = metadata
    )
  )
}

geography_01 <-
  run_geography_profile()