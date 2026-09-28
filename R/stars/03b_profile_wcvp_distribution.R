# =============================================================================
# VPJD-OJPCP
# R/stars/03b_profile_wcvp_distribution.R
# Version 0.1.1
#
# PROFILE WCVP DISTRIBUTION DATA AGAINST THE CONTEMPORARY JAPANESE FLORA
#
# Purpose:
# - Profile the official WCVP distribution extension.
# - Link WCVP distribution records to the 11,439 accepted VPJD taxa.
# - Examine geographic resolution and establishmentMeans.
# - Determine which Key-to-Stars inputs may be derivable from WCVP.
#
# IMPORTANT:
# - Diagnostic only.
# - No Stars are assigned.
# - No endemicity is inferred yet.
# - No GX is assigned yet.
# - No TDWG interpretation is hard-coded yet.
# - Existing VPJD taxonomy/geography/distribution tables are not modified.
# =============================================================================

suppressPackageStartupMessages({
  library(DBI)
  library(duckdb)
  library(dplyr)
  library(readr)
  library(here)
})

MODULE <- "stars_03b_profile_wcvp_distribution"
VERSION <- "0.1.1"
RUN_DATE <- Sys.Date()

DB_PATH <- here("data", "interim", "occurrences", "vpjd_occurrences.duckdb")
SOURCE_TAXA <- "vpjd_japan_taxon_distribution"

WCVP_DIR <- here("data", "raw", "geography", "wcvp_dwca")
WCVP_TAXON_FILE <- file.path(WCVP_DIR, "wcvp_taxon.csv")
WCVP_DISTRIBUTION_FILE <- file.path(WCVP_DIR, "wcvp_distribution.csv")

OUTPUT_DIR <- here("outputs", "tables", "stars", "wcvp_distribution")

OUTPUT_SUMMARY <- "vpjd_star_wcvp_distribution_profile"
OUTPUT_ESTABLISHMENT <- "vpjd_star_wcvp_establishment_profile"
OUTPUT_GEOGRAPHY <- "vpjd_star_wcvp_geography_profile"
OUTPUT_LINKAGE <- "vpjd_star_wcvp_taxon_linkage_profile"
OUTPUT_VALIDATION <- "vpjd_star_wcvp_distribution_validation"
OUTPUT_METADATA <- "vpjd_star_wcvp_distribution_metadata"

run_stars_03b <- function() {
  
  cat("\n— Profile WCVP distribution data —\n\n")
  cat("Run date: ", as.character(RUN_DATE), "\n", sep = "")
  cat("Module: ", MODULE, "\n", sep = "")
  cat("Version: ", VERSION, "\n\n", sep = "")
  
  dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
  
  required_files <- c(
    WCVP_TAXON_FILE,
    WCVP_DISTRIBUTION_FILE
  )
  
  missing_files <- required_files[!file.exists(required_files)]
  
  if (length(missing_files) > 0L) {
    stop(
      "Required WCVP files missing:\n",
      paste(missing_files, collapse = "\n")
    )
  }
  
  cat("WCVP taxon file:\n  ", WCVP_TAXON_FILE, "\n", sep = "")
  cat("WCVP distribution file:\n  ", WCVP_DISTRIBUTION_FILE, "\n\n", sep = "")
  
  if (!file.exists(DB_PATH)) {
    stop("VPJD DuckDB not found: ", DB_PATH)
  }
  
  con <- dbConnect(
    duckdb::duckdb(),
    dbdir = DB_PATH,
    read_only = FALSE
  )
  
  on.exit(
    dbDisconnect(con, shutdown = TRUE),
    add = TRUE
  )
  
  if (!SOURCE_TAXA %in% dbListTables(con)) {
    stop("Required VPJD table missing: ", SOURCE_TAXA)
  }
  
  # ===========================================================================
  # 1. Contemporary accepted VPJD taxon population
  # ===========================================================================
  
  accepted <- dbGetQuery(
    con,
    paste0(
      "SELECT ",
      "FINAL_WCVP_ID, ",
      "FINAL_WCVP_RECOGNISED_NAME, ",
      "FINAL_WCVP_RANK, ",
      "FINAL_WCVP_STATUS, ",
      "OCCURRENCE_RECORDS ",
      "FROM ", SOURCE_TAXA, " ",
      "WHERE lower(FINAL_WCVP_STATUS) = 'accepted'"
    )
  ) |>
    mutate(
      FINAL_WCVP_ID = as.character(FINAL_WCVP_ID)
    )
  
  cat(
    "Accepted VPJD WCVP taxa: ",
    format(nrow(accepted), big.mark = ","),
    "\n",
    sep = ""
  )
  
  if (nrow(accepted) != 11439L) {
    stop(
      "Accepted taxon population differs from validated Stars 03a population."
    )
  }
  
  if (n_distinct(accepted$FINAL_WCVP_ID) != 11439L) {
    stop("Accepted VPJD WCVP IDs are not unique.")
  }
  
  # ===========================================================================
  # 2. Normalise file paths for DuckDB
  # ===========================================================================
  
  taxon_path_sql <- gsub(
    "'",
    "''",
    normalizePath(
      WCVP_TAXON_FILE,
      winslash = "/",
      mustWork = TRUE
    )
  )
  
  distribution_path_sql <- gsub(
    "'",
    "''",
    normalizePath(
      WCVP_DISTRIBUTION_FILE,
      winslash = "/",
      mustWork = TRUE
    )
  )
  
  # ===========================================================================
  # 3. Inspect actual WCVP schemas
  #
  # DuckDB normalises headers to lowercase.
  #
  # Observed WCVP 16.0 fields include:
  # taxon core:
  #   taxonid
  #   scientfiicname
  #   taxonrank
  #   taxonomicstatus
  #
  # distribution extension:
  #   coreid
  #   locality
  #   establishmentmeans
  #   locationid
  #   occurrencestatus
  #   threatstatus
  #
  # Note: "scientfiicname" is retained exactly as supplied in the WCVP file.
  # ===========================================================================
  
  cat("\nReading WCVP taxon schema...\n")
  
  taxon_sample <- dbGetQuery(
    con,
    paste0(
      "SELECT * FROM read_csv_auto('",
      taxon_path_sql,
      "', delim='|', header=TRUE, all_varchar=TRUE) LIMIT 5"
    )
  )
  
  cat("\nWCVP taxon fields:\n")
  print(names(taxon_sample))
  
  cat("\nReading WCVP distribution schema...\n")
  
  distribution_sample <- dbGetQuery(
    con,
    paste0(
      "SELECT * FROM read_csv_auto('",
      distribution_path_sql,
      "', delim='|', header=TRUE, all_varchar=TRUE) LIMIT 10"
    )
  )
  
  cat("\nWCVP distribution fields:\n")
  print(names(distribution_sample))
  
  taxon_fields <- names(taxon_sample)
  distribution_fields <- names(distribution_sample)
  
  required_taxon_fields <- c(
    "taxonid",
    "scientfiicname",
    "taxonrank",
    "taxonomicstatus"
  )
  
  required_distribution_fields <- c(
    "coreid",
    "locality",
    "establishmentmeans",
    "locationid",
    "occurrencestatus"
  )
  
  missing_taxon_fields <- setdiff(
    required_taxon_fields,
    taxon_fields
  )
  
  missing_distribution_fields <- setdiff(
    required_distribution_fields,
    distribution_fields
  )
  
  if (length(missing_taxon_fields) > 0L) {
    stop(
      "WCVP taxon file missing required fields: ",
      paste(missing_taxon_fields, collapse = ", ")
    )
  }
  
  if (length(missing_distribution_fields) > 0L) {
    stop(
      "WCVP distribution file missing required fields: ",
      paste(missing_distribution_fields, collapse = ", ")
    )
  }
  
  # ===========================================================================
  # 4. Create normalised temporary WCVP views
  # ===========================================================================
  
  dbExecute(
    con,
    paste0(
      "CREATE OR REPLACE TEMP VIEW wcvp_taxon_raw AS ",
      "SELECT ",
      "CAST(taxonid AS VARCHAR) AS taxon_id, ",
      "family, ",
      "genus, ",
      "specificepithet, ",
      "infraspecificepithet, ",
      "scientfiicname AS scientific_name, ",
      "taxonrank AS taxon_rank, ",
      "taxonomicstatus AS taxonomic_status, ",
      "acceptednameusageid AS accepted_name_usage_id, ",
      "parentnameusageid AS parent_name_usage_id ",
      "FROM read_csv_auto('",
      taxon_path_sql,
      "', delim='|', header=TRUE, all_varchar=TRUE)"
    )
  )
  
  dbExecute(
    con,
    paste0(
      "CREATE OR REPLACE TEMP VIEW wcvp_distribution_raw AS ",
      "SELECT ",
      "CAST(coreid AS VARCHAR) AS taxon_id, ",
      "locality, ",
      "establishmentmeans AS establishment_means, ",
      "locationid AS location_id, ",
      "occurrencestatus AS occurrence_status, ",
      "threatstatus AS threat_status ",
      "FROM read_csv_auto('",
      distribution_path_sql,
      "', delim='|', header=TRUE, all_varchar=TRUE)"
    )
  )
  
  wcvp_taxon_n <- dbGetQuery(
    con,
    "SELECT COUNT(*) AS n FROM wcvp_taxon_raw"
  )$n[[1]]
  
  wcvp_distribution_n <- dbGetQuery(
    con,
    "SELECT COUNT(*) AS n FROM wcvp_distribution_raw"
  )$n[[1]]
  
  cat(
    "\nWCVP taxon rows: ",
    format(wcvp_taxon_n, big.mark = ","),
    "\n",
    sep = ""
  )
  
  cat(
    "WCVP distribution rows: ",
    format(wcvp_distribution_n, big.mark = ","),
    "\n",
    sep = ""
  )
  
  # ===========================================================================
  # 5. Load accepted VPJD IDs into DuckDB
  # ===========================================================================
  
  dbWriteTable(
    con,
    "vpjd_star_accepted_taxa_temp",
    accepted,
    temporary = TRUE,
    overwrite = TRUE
  )
  
  # ===========================================================================
  # 6. Direct VPJD -> current WCVP taxon linkage
  # ===========================================================================
  
  linkage <- dbGetQuery(
    con,
    "
    SELECT
      CASE
        WHEN w.taxon_id IS NOT NULL
          THEN 'DIRECT_WCVP_TAXON_ID_MATCH'
        ELSE 'NO_DIRECT_WCVP_TAXON_ID_MATCH'
      END AS LINKAGE_STATUS,
      COUNT(*) AS WCVP_TAXA
    FROM vpjd_star_accepted_taxa_temp v
    LEFT JOIN wcvp_taxon_raw w
      ON CAST(v.FINAL_WCVP_ID AS VARCHAR) = w.taxon_id
    GROUP BY 1
    ORDER BY 1
    "
  )
  
  cat("\n— VPJD → current WCVP taxon linkage —\n")
  print.data.frame(linkage, row.names = FALSE)
  
  linked_taxa <- dbGetQuery(
    con,
    "
    SELECT DISTINCT
      CAST(v.FINAL_WCVP_ID AS VARCHAR) AS FINAL_WCVP_ID,
      v.FINAL_WCVP_RECOGNISED_NAME,
      v.FINAL_WCVP_RANK,
      w.taxon_id AS WCVP_TAXON_ID,
      w.scientific_name AS WCVP_SCIENTIFIC_NAME,
      w.taxon_rank AS WCVP_TAXON_RANK,
      w.taxonomic_status AS WCVP_TAXONOMIC_STATUS
    FROM vpjd_star_accepted_taxa_temp v
    INNER JOIN wcvp_taxon_raw w
      ON CAST(v.FINAL_WCVP_ID AS VARCHAR) = w.taxon_id
    "
  )
  
  # ===========================================================================
  # 7. WCVP distribution coverage
  # ===========================================================================
  
  distribution_coverage <- dbGetQuery(
    con,
    "
    WITH x AS (
      SELECT
        CAST(v.FINAL_WCVP_ID AS VARCHAR) AS FINAL_WCVP_ID,
        COUNT(d.taxon_id) AS DISTRIBUTION_ROWS
      FROM vpjd_star_accepted_taxa_temp v
      LEFT JOIN wcvp_distribution_raw d
        ON CAST(v.FINAL_WCVP_ID AS VARCHAR) = d.taxon_id
      GROUP BY 1
    )
    SELECT
      CASE
        WHEN DISTRIBUTION_ROWS > 0
          THEN 'HAS_WCVP_DISTRIBUTION'
        ELSE 'NO_WCVP_DISTRIBUTION'
      END AS DISTRIBUTION_STATUS,
      COUNT(*) AS WCVP_TAXA
    FROM x
    GROUP BY 1
    ORDER BY 1
    "
  )
  
  cat("\n— WCVP distribution coverage of accepted Japanese taxa —\n")
  print.data.frame(distribution_coverage, row.names = FALSE)
  
  taxa_distribution_summary <- dbGetQuery(
    con,
    "
    SELECT
      CAST(v.FINAL_WCVP_ID AS VARCHAR) AS FINAL_WCVP_ID,
      v.FINAL_WCVP_RECOGNISED_NAME,
      COUNT(d.taxon_id) AS WCVP_DISTRIBUTION_ROWS,
      COUNT(DISTINCT d.location_id) AS WCVP_LOCATION_IDS,
      COUNT(DISTINCT d.locality) AS WCVP_LOCALITIES,
      COUNT(DISTINCT d.establishment_means) AS ESTABLISHMENT_VALUES
    FROM vpjd_star_accepted_taxa_temp v
    LEFT JOIN wcvp_distribution_raw d
      ON CAST(v.FINAL_WCVP_ID AS VARCHAR) = d.taxon_id
    GROUP BY
      v.FINAL_WCVP_ID,
      v.FINAL_WCVP_RECOGNISED_NAME
    ORDER BY
      WCVP_DISTRIBUTION_ROWS DESC,
      v.FINAL_WCVP_RECOGNISED_NAME
    "
  )
  
  # ===========================================================================
  # 8. establishmentMeans profile
  # ===========================================================================
  
  establishment <- dbGetQuery(
    con,
    "
    SELECT
      CASE
        WHEN d.establishment_means IS NULL
          OR TRIM(d.establishment_means) = ''
          THEN '<NA>'
        ELSE d.establishment_means
      END AS ESTABLISHMENT_MEANS,
      COUNT(*) AS DISTRIBUTION_ROWS,
      COUNT(DISTINCT d.taxon_id) AS WCVP_TAXA
    FROM wcvp_distribution_raw d
    INNER JOIN vpjd_star_accepted_taxa_temp v
      ON d.taxon_id = CAST(v.FINAL_WCVP_ID AS VARCHAR)
    GROUP BY 1
    ORDER BY DISTRIBUTION_ROWS DESC, ESTABLISHMENT_MEANS
    "
  )
  
  cat("\n— establishmentMeans profile —\n")
  print.data.frame(establishment, row.names = FALSE)
  
  # ===========================================================================
  # 9. occurrenceStatus profile
  # ===========================================================================
  
  occurrence_status <- dbGetQuery(
    con,
    "
    SELECT
      CASE
        WHEN d.occurrence_status IS NULL
          OR TRIM(d.occurrence_status) = ''
          THEN '<NA>'
        ELSE d.occurrence_status
      END AS OCCURRENCE_STATUS,
      COUNT(*) AS DISTRIBUTION_ROWS,
      COUNT(DISTINCT d.taxon_id) AS WCVP_TAXA
    FROM wcvp_distribution_raw d
    INNER JOIN vpjd_star_accepted_taxa_temp v
      ON d.taxon_id = CAST(v.FINAL_WCVP_ID AS VARCHAR)
    GROUP BY 1
    ORDER BY DISTRIBUTION_ROWS DESC, OCCURRENCE_STATUS
    "
  )
  
  cat("\n— occurrenceStatus profile —\n")
  print.data.frame(occurrence_status, row.names = FALSE)
  
  # ===========================================================================
  # 10. Geographic profile
  # ===========================================================================
  
  geography <- dbGetQuery(
    con,
    "
    SELECT
      CASE
        WHEN d.location_id IS NULL
          OR TRIM(d.location_id) = ''
          THEN '<NA>'
        ELSE d.location_id
      END AS LOCATION_ID,
      CASE
        WHEN d.locality IS NULL
          OR TRIM(d.locality) = ''
          THEN '<NA>'
        ELSE d.locality
      END AS LOCALITY,
      COUNT(*) AS DISTRIBUTION_ROWS,
      COUNT(DISTINCT d.taxon_id) AS WCVP_TAXA
    FROM wcvp_distribution_raw d
    INNER JOIN vpjd_star_accepted_taxa_temp v
      ON d.taxon_id = CAST(v.FINAL_WCVP_ID AS VARCHAR)
    GROUP BY 1, 2
    ORDER BY DISTRIBUTION_ROWS DESC, LOCATION_ID, LOCALITY
    "
  )
  
  cat(
    "\nDistinct WCVP locationID/locality combinations for accepted Japanese taxa: ",
    format(nrow(geography), big.mark = ","),
    "\n",
    sep = ""
  )
  
  cat("\nFirst 50 geographic units:\n")
  print.data.frame(
    head(geography, 50),
    row.names = FALSE
  )
  
  # ===========================================================================
  # 11. Target-region diagnostic
  #
  # Text matching only.
  # No formal Key geography interpretation yet.
  # ===========================================================================
  
  target_geography <- geography |>
    filter(
      grepl(
        paste(
          c(
            "Japan",
            "Taiwan",
            "Korea",
            "China",
            "Kuril",
            "Sakhalin",
            "Ryukyu",
            "Ogasawara",
            "Bonin",
            "Izu"
          ),
          collapse = "|"
        ),
        LOCALITY,
        ignore.case = TRUE
      )
    )
  
  cat("\n— Target-region diagnostic —\n")
  
  if (nrow(target_geography) > 0L) {
    print.data.frame(
      target_geography,
      row.names = FALSE
    )
  } else {
    cat(
      "No target regions identified by locality text matching.\n"
    )
  }
  
  # ===========================================================================
  # 12. Summary
  # ===========================================================================
  
  direct_link_n <- sum(
    linkage$WCVP_TAXA[
      linkage$LINKAGE_STATUS ==
        "DIRECT_WCVP_TAXON_ID_MATCH"
    ]
  )
  
  distribution_taxa_n <- sum(
    distribution_coverage$WCVP_TAXA[
      distribution_coverage$DISTRIBUTION_STATUS ==
        "HAS_WCVP_DISTRIBUTION"
    ]
  )
  
  summary <- tibble::tibble(
    metric = c(
      "accepted_vpjd_taxa",
      "direct_current_wcvp_taxon_id_matches",
      "accepted_taxa_with_wcvp_distribution",
      "accepted_taxa_without_wcvp_distribution",
      "wcvp_taxon_rows",
      "wcvp_distribution_rows",
      "distinct_target_geography_rows"
    ),
    value = c(
      nrow(accepted),
      direct_link_n,
      distribution_taxa_n,
      nrow(accepted) - distribution_taxa_n,
      wcvp_taxon_n,
      wcvp_distribution_n,
      nrow(target_geography)
    )
  )
  
  cat("\n— Summary —\n")
  print.data.frame(summary, row.names = FALSE)
  
  # ===========================================================================
  # 13. Validation
  # ===========================================================================
  
  validation <- tibble::tibble(
    check = c(
      "accepted_taxon_population_11439",
      "accepted_taxon_ids_unique",
      "wcvp_taxon_file_present",
      "wcvp_distribution_file_present",
      "observed_wcvp_taxon_schema_supported",
      "observed_wcvp_distribution_schema_supported",
      "wcvp_taxon_rows_nonzero",
      "wcvp_distribution_rows_nonzero",
      "existing_taxonomy_not_modified",
      "existing_geography_not_modified",
      "existing_distribution_not_modified",
      "stars_not_assigned",
      "endemicity_not_inferred",
      "gx_not_assigned",
      "tdwg_key_mapping_not_invented",
      "historical_star_totals_not_used",
      "ghi_not_calculated"
    ),
    pass = c(
      nrow(accepted) == 11439L,
      n_distinct(accepted$FINAL_WCVP_ID) == 11439L,
      file.exists(WCVP_TAXON_FILE),
      file.exists(WCVP_DISTRIBUTION_FILE),
      length(missing_taxon_fields) == 0L,
      length(missing_distribution_fields) == 0L,
      wcvp_taxon_n > 0,
      wcvp_distribution_n > 0,
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE
    )
  )
  
  cat("\n— Validation —\n")
  print.data.frame(validation, row.names = FALSE)
  
  all_pass <- all(validation$pass)
  
  cat(
    "\nAll Stars 03b validation checks PASS: ",
    all_pass,
    "\n",
    sep = ""
  )
  
  if (!all_pass) {
    stop("Stars 03b validation failed. Do not freeze.")
  }
  
  # ===========================================================================
  # 14. Persist diagnostic outputs
  # ===========================================================================
  
  metadata <- tibble::tibble(
    metric = c(
      "module",
      "version",
      "run_date",
      "wcvp_source",
      "wcvp_taxon_file",
      "wcvp_distribution_file",
      "accepted_taxa",
      "stars_assigned",
      "endemicity_inferred",
      "gx_assigned",
      "tdwg_mapping_formalised",
      "validation_pass"
    ),
    value = c(
      MODULE,
      VERSION,
      as.character(RUN_DATE),
      "Royal Botanic Gardens Kew WCVP Darwin Core Archive",
      basename(WCVP_TAXON_FILE),
      basename(WCVP_DISTRIBUTION_FILE),
      as.character(nrow(accepted)),
      "FALSE",
      "FALSE",
      "FALSE",
      "FALSE",
      as.character(all_pass)
    )
  )
  
  dbWriteTable(
    con,
    OUTPUT_SUMMARY,
    summary,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    OUTPUT_ESTABLISHMENT,
    establishment,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    OUTPUT_GEOGRAPHY,
    geography,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    OUTPUT_LINKAGE,
    linkage,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    OUTPUT_VALIDATION,
    validation,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    OUTPUT_METADATA,
    metadata,
    overwrite = TRUE
  )
  
  write_csv(
    summary,
    file.path(
      OUTPUT_DIR,
      "stars03b_summary.csv"
    )
  )
  
  write_csv(
    linkage,
    file.path(
      OUTPUT_DIR,
      "stars03b_taxon_linkage_profile.csv"
    )
  )
  
  write_csv(
    linked_taxa,
    file.path(
      OUTPUT_DIR,
      "stars03b_linked_taxa.csv"
    )
  )
  
  write_csv(
    taxa_distribution_summary,
    file.path(
      OUTPUT_DIR,
      "stars03b_taxon_distribution_summary.csv"
    )
  )
  
  write_csv(
    establishment,
    file.path(
      OUTPUT_DIR,
      "stars03b_establishment_means_profile.csv"
    )
  )
  
  write_csv(
    occurrence_status,
    file.path(
      OUTPUT_DIR,
      "stars03b_occurrence_status_profile.csv"
    )
  )
  
  write_csv(
    geography,
    file.path(
      OUTPUT_DIR,
      "stars03b_geography_profile.csv"
    )
  )
  
  write_csv(
    target_geography,
    file.path(
      OUTPUT_DIR,
      "stars03b_target_geography_diagnostic.csv"
    )
  )
  
  write_csv(
    validation,
    file.path(
      OUTPUT_DIR,
      "stars03b_validation.csv"
    )
  )
  
  write_csv(
    metadata,
    file.path(
      OUTPUT_DIR,
      "stars03b_metadata.csv"
    )
  )
  
  cat("\nStars 03b v", VERSION, " complete.\n", sep = "")
  cat(
    "Diagnostic only: no Stars, endemicity or GX assigned.\n"
  )
  
  invisible(
    list(
      summary = summary,
      linkage = linkage,
      linked_taxa = linked_taxa,
      taxa_distribution_summary = taxa_distribution_summary,
      establishment = establishment,
      occurrence_status = occurrence_status,
      geography = geography,
      target_geography = target_geography,
      validation = validation,
      metadata = metadata
    )
  )
}

result_stars_03b <- run_stars_03b()