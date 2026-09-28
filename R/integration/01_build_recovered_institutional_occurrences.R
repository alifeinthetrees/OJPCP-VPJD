# ==============================================================================
# VPJD-OJPCP
# 01_build_recovered_institutional_occurrences.R
# Version: 0.1.0
#
# Purpose:
#   Build the canonical recovered institutional occurrence layer from the
#   georeferenced institutional records validated by frozen georeferencing
#   module 03h v0.1.0.
#
# Scientific principles:
#   - Only 03h QA-passed / analytically eligible records are admitted.
#   - Original GBIF fields are preserved.
#   - Original coordinate fields are never overwritten.
#   - Gazetteer-derived coordinates are stored in explicit analytical fields.
#   - Coordinate uncertainty and full georeferencing provenance are retained.
#   - GBIF-ID overlap with the national GBIF acquisition is diagnosed.
#   - No records are merged into occurrences_clean in this module.
#   - No taxonomy is inferred or assigned here.
#   - No upstream tables are modified.
# ==============================================================================

suppressPackageStartupMessages({
  library(DBI)
  library(duckdb)
  library(dplyr)
  library(stringr)
  library(readr)
  library(tibble)
  library(here)
})

VPJD_INTEGRATION_VERSION <- "0.1.0"

run_recovered_institutional_integration <- function() {
  cat("\n— VPJD recovered institutional occurrence integration —\n\n")
  
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
  
  tables <- dbListTables(con)
  
  required_tables <- c(
    "institutional_georeferencing_qa_occurrences",
    "institutional_georeferencing_qa_metadata",
    "gbif_raw"
  )
  
  missing_tables <- setdiff(required_tables, tables)
  
  if (length(missing_tables) > 0) {
    stop(
      "Required tables missing: ",
      paste(missing_tables, collapse = ", ")
    )
  }
  
  # ---------------------------------------------------------------------------
  # Validate frozen georeferencing input
  # ---------------------------------------------------------------------------
  
  metadata_03h <- dbReadTable(
    con,
    "institutional_georeferencing_qa_metadata"
  ) %>%
    as_tibble()
  
  version_03h <- metadata_03h %>%
    filter(metric == "script_version") %>%
    pull(value)
  
  if (
    length(version_03h) != 1 ||
    version_03h != "0.1.0"
  ) {
    stop(
      "Integration module expects frozen georeferencing 03h v0.1.0; detected: ",
      paste(version_03h, collapse = ", ")
    )
  }
  
  qa_occurrences <- dbReadTable(
    con,
    "institutional_georeferencing_qa_occurrences"
  ) %>%
    as_tibble()
  
  expected_records <- 13290L
  
  if (nrow(qa_occurrences) != expected_records) {
    stop(
      "Unexpected 03h occurrence count: ",
      nrow(qa_occurrences),
      "; expected ",
      expected_records
    )
  }
  
  if (!"gbifID" %in% names(qa_occurrences)) {
    stop("03h occurrence table lacks gbifID.")
  }
  
  if (
    n_distinct(qa_occurrences$gbifID) !=
    expected_records
  ) {
    stop(
      "03h occurrence table does not contain exactly ",
      expected_records,
      " distinct gbifID values."
    )
  }
  
  required_qa_fields <- c(
    "GEOREF_PROPOSED_LATITUDE",
    "GEOREF_PROPOSED_LONGITUDE",
    "GEOREF_COORDINATE_UNCERTAINTY_M",
    "GEOREF_PROPOSAL_METHOD",
    "GEOREF_PROPOSAL_CONFIDENCE",
    "GEOREF_COORDINATE_INTERPRETATION",
    "GEOREF_UNCERTAINTY_METHOD",
    "GEOREF_PROVENANCE",
    "GEOREF_QA_STATUS",
    "GEOREF_ANALYTICAL_STATUS",
    "GEOREF_PRECISION_CLASS",
    "GAZETTEER_SOURCE",
    "GAZETTEER_ID"
  )
  
  missing_qa_fields <- setdiff(
    required_qa_fields,
    names(qa_occurrences)
  )
  
  if (length(missing_qa_fields) > 0) {
    stop(
      "Required 03h fields missing: ",
      paste(missing_qa_fields, collapse = ", ")
    )
  }
  
  eligible <- qa_occurrences %>%
    filter(
      GEOREF_QA_STATUS == "qa_pass",
      GEOREF_ANALYTICAL_STATUS ==
        "eligible_georeferenced_occurrence"
    )
  
  if (nrow(eligible) != expected_records) {
    stop(
      "Not all 13,290 frozen 03h records remain analytically eligible."
    )
  }
  
  cat(
    "Frozen georeferencing 03h version: ",
    version_03h,
    "\n",
    sep = ""
  )
  
  cat(
    "QA-passed institutional records: ",
    format(nrow(eligible), big.mark = ","),
    "\n\n",
    sep = ""
  )
  
  # ---------------------------------------------------------------------------
  # Confirm original records remain coordinate-less
  # ---------------------------------------------------------------------------
  
  source_lat_present <- FALSE
  source_lon_present <- FALSE
  
  if ("decimalLatitude" %in% names(eligible)) {
    source_lat_present <- any(
      !is.na(
        suppressWarnings(
          as.numeric(eligible$decimalLatitude)
        )
      )
    )
  }
  
  if ("decimalLongitude" %in% names(eligible)) {
    source_lon_present <- any(
      !is.na(
        suppressWarnings(
          as.numeric(eligible$decimalLongitude)
        )
      )
    )
  }
  
  if (source_lat_present || source_lon_present) {
    stop(
      "At least one recovered source record now contains an original ",
      "decimalLatitude/decimalLongitude value. Integration halted."
    )
  }
  
  # ---------------------------------------------------------------------------
  # Construct canonical recovered occurrence layer
  #
  # Original GBIF coordinate fields remain untouched.
  # ANALYTICAL_* fields are the approved gazetteer-derived spatial values.
  # ---------------------------------------------------------------------------
  
  recovered <- eligible %>%
    mutate(
      ANALYTICAL_DECIMAL_LATITUDE =
        as.numeric(GEOREF_PROPOSED_LATITUDE),
      
      ANALYTICAL_DECIMAL_LONGITUDE =
        as.numeric(GEOREF_PROPOSED_LONGITUDE),
      
      ANALYTICAL_COORDINATE_UNCERTAINTY_M =
        as.numeric(GEOREF_COORDINATE_UNCERTAINTY_M),
      
      ANALYTICAL_COORDINATE_SOURCE =
        "VPJD gazetteer-derived georeference",
      
      ANALYTICAL_COORDINATE_STATUS =
        "recovered_from_locality",
      
      ANALYTICAL_GEOREFERENCE_METHOD =
        GEOREF_PROPOSAL_METHOD,
      
      ANALYTICAL_GEOREFERENCE_CONFIDENCE =
        GEOREF_PROPOSAL_CONFIDENCE,
      
      ANALYTICAL_GEOREFERENCE_INTERPRETATION =
        GEOREF_COORDINATE_INTERPRETATION,
      
      ANALYTICAL_GEOREFERENCE_UNCERTAINTY_METHOD =
        GEOREF_UNCERTAINTY_METHOD,
      
      ANALYTICAL_GEOREFERENCE_PROVENANCE =
        GEOREF_PROVENANCE,
      
      ANALYTICAL_GAZETTEER_SOURCE =
        GAZETTEER_SOURCE,
      
      ANALYTICAL_GAZETTEER_ID =
        as.character(GAZETTEER_ID),
      
      ANALYTICAL_PRECISION_CLASS =
        GEOREF_PRECISION_CLASS,
      
      INTEGRATION_SOURCE =
        "institutional_coordinate_recovery",
      
      INTEGRATION_VERSION =
        VPJD_INTEGRATION_VERSION
    )
  
  # ---------------------------------------------------------------------------
  # Validate analytical coordinates
  # ---------------------------------------------------------------------------
  
  invalid_analytical_coordinates <- recovered %>%
    filter(
      is.na(ANALYTICAL_DECIMAL_LATITUDE) |
        is.na(ANALYTICAL_DECIMAL_LONGITUDE) |
        ANALYTICAL_DECIMAL_LATITUDE < -90 |
        ANALYTICAL_DECIMAL_LATITUDE > 90 |
        ANALYTICAL_DECIMAL_LONGITUDE < -180 |
        ANALYTICAL_DECIMAL_LONGITUDE > 180
    )
  
  if (nrow(invalid_analytical_coordinates) > 0) {
    stop(
      "Invalid analytical coordinates detected in ",
      nrow(invalid_analytical_coordinates),
      " recovered records."
    )
  }
  
  invalid_uncertainty <- recovered %>%
    filter(
      is.na(ANALYTICAL_COORDINATE_UNCERTAINTY_M) |
        ANALYTICAL_COORDINATE_UNCERTAINTY_M < 0
    )
  
  if (nrow(invalid_uncertainty) > 0) {
    stop(
      "Invalid analytical coordinate uncertainty detected in ",
      nrow(invalid_uncertainty),
      " recovered records."
    )
  }
  
  # ---------------------------------------------------------------------------
  # National GBIF overlap
  #
  # Use gbifID only. This is a source-record identity test, not a biological
  # duplicate test.
  # ---------------------------------------------------------------------------
  
  national_ids <- dbGetQuery(
    con,
    "
    SELECT DISTINCT CAST(gbifID AS VARCHAR) AS gbifID
    FROM gbif_raw
    WHERE gbifID IS NOT NULL
    "
  ) %>%
    as_tibble()
  
  recovered <- recovered %>%
    mutate(
      gbifID =
        as.character(gbifID)
    ) %>%
    left_join(
      national_ids %>%
        mutate(
          NATIONAL_GBIF_ID_PRESENT = TRUE
        ),
      by = "gbifID"
    ) %>%
    mutate(
      NATIONAL_GBIF_ID_PRESENT =
        coalesce(
          NATIONAL_GBIF_ID_PRESENT,
          FALSE
        ),
      
      NATIONAL_GBIF_ID_STATUS =
        if_else(
          NATIONAL_GBIF_ID_PRESENT,
          "already_in_national_gbif_raw",
          "new_to_national_gbif_raw"
        )
    )
  
  overlap_summary <- recovered %>%
    count(
      NATIONAL_GBIF_ID_STATUS,
      name = "records"
    ) %>%
    mutate(
      pct_records =
        100 * records / sum(records)
    ) %>%
    arrange(desc(records))
  
  new_to_national <- recovered %>%
    filter(
      !NATIONAL_GBIF_ID_PRESENT
    )
  
  already_national <- recovered %>%
    filter(
      NATIONAL_GBIF_ID_PRESENT
    )
  
  # ---------------------------------------------------------------------------
  # Additional identity diagnostics
  # ---------------------------------------------------------------------------
  
  occurrence_id_summary <- tibble(
    metric = c(
      "recovered_records",
      "distinct_gbif_ids",
      "records_new_to_national_gbif_raw",
      "records_already_in_national_gbif_raw"
    ),
    value = c(
      nrow(recovered),
      n_distinct(recovered$gbifID),
      nrow(new_to_national),
      nrow(already_national)
    )
  )
  
  if ("occurrenceID" %in% names(recovered)) {
    occurrence_id_values <- recovered %>%
      mutate(
        OCCURRENCE_ID_NORMALISED =
          str_trim(
            as.character(occurrenceID)
          ),
        OCCURRENCE_ID_NORMALISED =
          na_if(
            OCCURRENCE_ID_NORMALISED,
            ""
          )
      )
    
    occurrence_id_diagnostics <-
      occurrence_id_values %>%
      summarise(
        records = n(),
        populated_occurrence_ids =
          sum(
            !is.na(OCCURRENCE_ID_NORMALISED)
          ),
        distinct_occurrence_ids =
          n_distinct(
            OCCURRENCE_ID_NORMALISED,
            na.rm = TRUE
          ),
        duplicate_occurrence_id_records =
          sum(
            duplicated(
              OCCURRENCE_ID_NORMALISED
            ) &
              !is.na(
                OCCURRENCE_ID_NORMALISED
              )
          )
      )
  } else {
    occurrence_id_diagnostics <- tibble(
      records = nrow(recovered),
      populated_occurrence_ids = NA_integer_,
      distinct_occurrence_ids = NA_integer_,
      duplicate_occurrence_id_records = NA_integer_
    )
  }
  
  # ---------------------------------------------------------------------------
  # Precision summary
  # ---------------------------------------------------------------------------
  
  precision_summary <- recovered %>%
    count(
      ANALYTICAL_PRECISION_CLASS,
      name = "records"
    ) %>%
    mutate(
      pct_records =
        100 * records / sum(records)
    ) %>%
    arrange(
      factor(
        ANALYTICAL_PRECISION_CLASS,
        levels = c(
          "point_or_zero_extent",
          "<=1_km",
          ">1-5_km",
          ">5-10_km",
          ">10-25_km",
          ">25-50_km",
          ">50_km",
          "uncertainty_missing"
        )
      )
    )
  
  # ---------------------------------------------------------------------------
  # Gazetteer/provenance summary
  # ---------------------------------------------------------------------------
  
  provenance_summary <- recovered %>%
    count(
      SEMANTIC_CLASS,
      ANALYTICAL_GAZETTEER_SOURCE,
      ANALYTICAL_GEOREFERENCE_CONFIDENCE,
      name = "records"
    ) %>%
    mutate(
      pct_records =
        100 * records / sum(records)
    ) %>%
    arrange(desc(records))
  
  # ---------------------------------------------------------------------------
  # Taxonomic field availability diagnostic
  #
  # No taxonomic decisions are made here.
  # ---------------------------------------------------------------------------
  
  possible_taxon_fields <- c(
    "scientificName",
    "acceptedScientificName",
    "taxonKey",
    "acceptedTaxonKey",
    "speciesKey",
    "genusKey",
    "familyKey",
    "taxonRank",
    "kingdom",
    "phylum",
    "class",
    "order",
    "family",
    "genus",
    "species"
  )
  
  taxon_fields_present <- intersect(
    possible_taxon_fields,
    names(recovered)
  )
  
  if (length(taxon_fields_present) > 0) {
    taxon_field_summary <- tibble(
      field = taxon_fields_present,
      populated_records = vapply(
        taxon_fields_present,
        function(x) {
          value <- as.character(
            recovered[[x]]
          )
          
          sum(
            !is.na(value) &
              str_trim(value) != ""
          )
        },
        integer(1)
      )
    ) %>%
      mutate(
        pct_populated =
          100 *
          populated_records /
          nrow(recovered)
      )
  } else {
    taxon_field_summary <- tibble(
      field = character(),
      populated_records = integer(),
      pct_populated = double()
    )
  }
  
  # ---------------------------------------------------------------------------
  # Final guards
  # ---------------------------------------------------------------------------
  
  if (nrow(recovered) != expected_records) {
    stop(
      "Recovered occurrence accounting failed."
    )
  }
  
  if (
    n_distinct(recovered$gbifID) !=
    expected_records
  ) {
    stop(
      "Recovered occurrence gbifID accounting failed."
    )
  }
  
  if (
    nrow(new_to_national) +
    nrow(already_national) !=
    expected_records
  ) {
    stop(
      "National-overlap accounting failed."
    )
  }
  
  # ---------------------------------------------------------------------------
  # Metadata
  # ---------------------------------------------------------------------------
  
  metadata <- tibble(
    metric = c(
      "script_version",
      "input_georeferencing_03h_version",
      "recovered_records",
      "distinct_gbif_ids",
      "new_to_national_gbif_raw",
      "already_in_national_gbif_raw",
      "invalid_analytical_coordinates",
      "invalid_analytical_uncertainty",
      "source_coordinates_overwritten",
      "taxonomy_assignments_made",
      "records_merged_into_occurrences_clean",
      "upstream_tables_modified"
    ),
    value = c(
      VPJD_INTEGRATION_VERSION,
      version_03h,
      as.character(nrow(recovered)),
      as.character(
        n_distinct(recovered$gbifID)
      ),
      as.character(
        nrow(new_to_national)
      ),
      as.character(
        nrow(already_national)
      ),
      as.character(
        nrow(
          invalid_analytical_coordinates
        )
      ),
      as.character(
        nrow(
          invalid_uncertainty
        )
      ),
      "0",
      "0",
      "0",
      "FALSE"
    )
  )
  
  # ---------------------------------------------------------------------------
  # Persist NEW integration tables only
  # ---------------------------------------------------------------------------
  
  dbWriteTable(
    con,
    "recovered_institutional_occurrences",
    recovered,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "recovered_institutional_occurrences_new_to_national",
    new_to_national,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "recovered_institutional_occurrences_already_national",
    already_national,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "recovered_institutional_occurrence_overlap_summary",
    overlap_summary,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "recovered_institutional_occurrence_metadata",
    metadata,
    overwrite = TRUE
  )
  
  # ---------------------------------------------------------------------------
  # CSV outputs
  # ---------------------------------------------------------------------------
  
  out_dir <- here(
    "outputs",
    "tables",
    "integration",
    "recovered_institutional_occurrences"
  )
  
  dir.create(
    out_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  write_csv(
    overlap_summary,
    file.path(
      out_dir,
      "national_gbif_overlap_summary.csv"
    )
  )
  
  write_csv(
    occurrence_id_summary,
    file.path(
      out_dir,
      "occurrence_identity_summary.csv"
    )
  )
  
  write_csv(
    occurrence_id_diagnostics,
    file.path(
      out_dir,
      "occurrence_id_diagnostics.csv"
    )
  )
  
  write_csv(
    precision_summary,
    file.path(
      out_dir,
      "precision_summary.csv"
    )
  )
  
  write_csv(
    provenance_summary,
    file.path(
      out_dir,
      "provenance_summary.csv"
    )
  )
  
  write_csv(
    taxon_field_summary,
    file.path(
      out_dir,
      "taxon_field_availability.csv"
    )
  )
  
  write_csv(
    metadata,
    file.path(
      out_dir,
      "metadata.csv"
    )
  )
  
  # ---------------------------------------------------------------------------
  # Console report
  # ---------------------------------------------------------------------------
  
  cat(
    "\n— 01 recovered institutional occurrence summary —\n\n"
  )
  
  print(
    occurrence_id_summary,
    n = Inf
  )
  
  cat(
    "\n— National GBIF-ID overlap —\n\n"
  )
  
  print(
    overlap_summary,
    n = Inf
  )
  
  cat(
    "\n— Occurrence-ID diagnostics —\n\n"
  )
  
  print(
    occurrence_id_diagnostics
  )
  
  cat(
    "\n— Spatial-precision summary —\n\n"
  )
  
  print(
    precision_summary,
    n = Inf
  )
  
  cat(
    "\n— Georeferencing provenance summary —\n\n"
  )
  
  print(
    provenance_summary,
    n = Inf
  )
  
  cat(
    "\n— Taxonomic field availability —\n\n"
  )
  
  if (nrow(taxon_field_summary) == 0) {
    cat(
      "No standard taxonomic fields detected.\n"
    )
  } else {
    print(
      taxon_field_summary,
      n = Inf
    )
  }
  
  cat(
    "\n— Accounting —\n"
  )
  
  cat(
    "Recovered records: ",
    format(
      nrow(recovered),
      big.mark = ","
    ),
    " / ",
    format(
      expected_records,
      big.mark = ","
    ),
    "\n",
    sep = ""
  )
  
  cat(
    "Distinct GBIF IDs: ",
    format(
      n_distinct(recovered$gbifID),
      big.mark = ","
    ),
    " / ",
    format(
      expected_records,
      big.mark = ","
    ),
    "\n",
    sep = ""
  )
  
  cat(
    "New to national GBIF raw: ",
    format(
      nrow(new_to_national),
      big.mark = ","
    ),
    "\n",
    sep = ""
  )
  
  cat(
    "Already in national GBIF raw: ",
    format(
      nrow(already_national),
      big.mark = ","
    ),
    "\n",
    sep = ""
  )
  
  cat(
    "\n— Safety —\n"
  )
  
  cat(
    "Original GBIF coordinate fields overwritten: 0\n"
  )
  
  cat(
    "Taxonomic assignments made: 0\n"
  )
  
  cat(
    "Records merged into occurrences_clean: 0\n"
  )
  
  cat(
    "Frozen upstream tables modified: FALSE\n"
  )
  
  cat(
    "Output status: CANONICAL RECOVERED LAYER ONLY\n"
  )
  
  cat(
    "\n01_build_recovered_institutional_occurrences.R v",
    VPJD_INTEGRATION_VERSION,
    " complete.\n",
    sep = ""
  )
  
  invisible(
    list(
      recovered = recovered,
      new_to_national = new_to_national,
      already_national = already_national,
      overlap_summary = overlap_summary,
      occurrence_id_summary = occurrence_id_summary,
      occurrence_id_diagnostics = occurrence_id_diagnostics,
      precision_summary = precision_summary,
      provenance_summary = provenance_summary,
      taxon_field_summary = taxon_field_summary,
      metadata = metadata
    )
  )
}

integration_01 <-
  run_recovered_institutional_integration()

integration_01_recovered <-
  integration_01$recovered

integration_01_new_to_national <-
  integration_01$new_to_national

integration_01_already_national <-
  integration_01$already_national

integration_01_overlap_summary <-
  integration_01$overlap_summary

integration_01_taxon_fields <-
  integration_01$taxon_field_summary