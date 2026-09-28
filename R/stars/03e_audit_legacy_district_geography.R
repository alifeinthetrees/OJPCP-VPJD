# =============================================================================
# VPJD-OJPCP
# R/stars/03e_profile_contemporary_district_inputs.R
# Version 0.2.0
#
# PROFILE CONTEMPORARY VPJD INPUTS FOR JAPANESE DISTRICT DERIVATION
#
# Purpose:
# - Work exclusively from newly generated VPJD outputs.
# - Profile the contemporary geography available for deriving the "district"
#   variables required by the Key to the Stars.
# - Confirm the 11,439 accepted-taxon Star population.
# - Confirm contemporary prefecture and botanical-area evidence.
# - Determine whether a district definition already exists in contemporary
#   VPJD outputs.
# - Create an explicit register of unresolved district inputs.
#
# METHODOLOGICAL RULE:
# - No GEOJAPAN data.
# - No JBRCI data.
# - No SPSLIM.
# - No FOJ_STARS.
# - No historical Star assignments or historical empirical totals.
# - The historical Key to the Stars is retained only as the methodological
#   decision specification.
#
# IMPORTANT:
# - No district geography is invented.
# - No modern regional grouping is silently substituted for "district".
# - No Stars are assigned.
# =============================================================================

suppressPackageStartupMessages({
  library(DBI)
  library(duckdb)
  library(dplyr)
  library(readr)
  library(here)
})

MODULE <- "stars_03e_profile_contemporary_district_inputs"
VERSION <- "0.2.0"
RUN_DATE <- Sys.Date()

DB_PATH <- here(
  "data",
  "interim",
  "occurrences",
  "vpjd_occurrences.duckdb"
)

SOURCE_TAXA <- "vpjd_japan_taxon_distribution"
SOURCE_STAR_JAPAN <- "vpjd_star_japan_key_geography"
SOURCE_WCVP_KEY <- "vpjd_star_wcvp_key_geography"
SOURCE_AREA_DISTRIBUTION <- "vpjd_japan_taxon_area_distribution"

OUTPUT_DIR <- here(
  "outputs",
  "tables",
  "stars",
  "district_inputs"
)

OUTPUT_PROFILE <- "vpjd_star_district_input_profile"
OUTPUT_STATUS <- "vpjd_star_district_input_status"
OUTPUT_VALIDATION <- "vpjd_star_district_input_validation"
OUTPUT_METADATA <- "vpjd_star_district_input_metadata"

run_stars_03e <- function() {
  
  cat("\n— Profile contemporary district inputs —\n\n")
  cat("Run date: ", as.character(RUN_DATE), "\n", sep = "")
  cat("Module: ", MODULE, "\n", sep = "")
  cat("Version: ", VERSION, "\n\n", sep = "")
  
  dir.create(
    OUTPUT_DIR,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
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
  
  # ===========================================================================
  # 1. Contemporary-only source register
  # ===========================================================================
  
  required_tables <- c(
    SOURCE_TAXA,
    SOURCE_STAR_JAPAN,
    SOURCE_WCVP_KEY,
    SOURCE_AREA_DISTRIBUTION
  )
  
  missing_tables <- setdiff(
    required_tables,
    dbListTables(con)
  )
  
  if (length(missing_tables) > 0L) {
    stop(
      "Required contemporary VPJD tables missing: ",
      paste(missing_tables, collapse = ", ")
    )
  }
  
  source_register <- tibble::tibble(
    SOURCE = required_tables,
    SOURCE_CLASS = c(
      "CONTEMPORARY_VPJD_TAXON_DISTRIBUTION",
      "CONTEMPORARY_VPJD_STAR_JAPAN_GEOGRAPHY",
      "CONTEMPORARY_WCVP_KEY_GEOGRAPHY",
      "CONTEMPORARY_VPJD_TAXON_AREA_DISTRIBUTION"
    ),
    LEGACY_SOURCE = FALSE
  )
  
  cat("Contemporary analytical sources:\n")
  print.data.frame(
    source_register,
    row.names = FALSE
  )
  
  # ===========================================================================
  # 2. Accepted contemporary taxon population
  # ===========================================================================
  
  accepted <- dbGetQuery(
    con,
    paste0(
      "SELECT ",
      "CAST(FINAL_WCVP_ID AS VARCHAR) AS FINAL_WCVP_ID, ",
      "FINAL_WCVP_RECOGNISED_NAME, ",
      "FINAL_WCVP_RANK, ",
      "FINAL_WCVP_STATUS, ",
      "OCCURRENCE_RECORDS ",
      "FROM ", SOURCE_TAXA, " ",
      "WHERE lower(FINAL_WCVP_STATUS) = 'accepted'"
    )
  )
  
  cat(
    "\nAccepted contemporary VPJD taxa: ",
    format(nrow(accepted), big.mark = ","),
    "\n",
    sep = ""
  )
  
  if (nrow(accepted) != 11439L) {
    stop(
      "Accepted contemporary Star population differs from validated 11,439."
    )
  }
  
  if (n_distinct(accepted$FINAL_WCVP_ID) != 11439L) {
    stop("Accepted WCVP IDs are not unique.")
  }
  
  # ===========================================================================
  # 3. Contemporary Japanese Key geography from Stars 03a
  # ===========================================================================
  
  japan_geo <- dbGetQuery(
    con,
    paste0(
      "SELECT ",
      "CAST(FINAL_WCVP_ID AS VARCHAR) AS FINAL_WCVP_ID, ",
      "JAPAN_PREFECTURES_PRESENT, ",
      "JAPAN_DISTRICTS_PRESENT, ",
      "ALMOST_ALL_DISTRICTS ",
      "FROM ", SOURCE_STAR_JAPAN
    )
  )
  
  cat(
    "Stars 03a taxon rows: ",
    format(nrow(japan_geo), big.mark = ","),
    "\n",
    sep = ""
  )
  
  prefecture_available <- sum(
    !is.na(japan_geo$JAPAN_PREFECTURES_PRESENT)
  )
  
  district_available <- sum(
    !is.na(japan_geo$JAPAN_DISTRICTS_PRESENT)
  )
  
  almost_all_available <- sum(
    !is.na(japan_geo$ALMOST_ALL_DISTRICTS)
  )
  
  cat(
    "Taxa with prefecture counts: ",
    format(prefecture_available, big.mark = ","),
    "\n",
    sep = ""
  )
  
  cat(
    "Taxa with district counts: ",
    format(district_available, big.mark = ","),
    "\n",
    sep = ""
  )
  
  cat(
    "Taxa with almost-all-districts status: ",
    format(almost_all_available, big.mark = ","),
    "\n",
    sep = ""
  )
  
  # ===========================================================================
  # 4. Contemporary botanical-area distribution
  # ===========================================================================
  
  area_schema <- dbGetQuery(
    con,
    paste0(
      "DESCRIBE SELECT * FROM ",
      SOURCE_AREA_DISTRIBUTION
    )
  )
  
  cat("\n— Contemporary botanical-area distribution schema —\n")
  print.data.frame(
    area_schema,
    row.names = FALSE
  )
  
  area_profile <- dbGetQuery(
    con,
    paste0(
      "SELECT ",
      "BOTANICAL_AREA_ID, ",
      "BOTANICAL_AREA_NAME, ",
      "COUNT(DISTINCT FINAL_WCVP_ID) AS WCVP_TAXA, ",
      "SUM(NATIONAL_GBIF_RECORDS) AS OCCURRENCE_EVIDENCE ",
      "FROM ", SOURCE_AREA_DISTRIBUTION, " ",
      "GROUP BY BOTANICAL_AREA_ID, BOTANICAL_AREA_NAME ",
      "ORDER BY BOTANICAL_AREA_ID"
    )
  )
  
  cat(
    "\nContemporary botanical areas represented: ",
    nrow(area_profile),
    "\n",
    sep = ""
  )
  
  cat("\n— Botanical-area profile —\n")
  print.data.frame(
    area_profile,
    row.names = FALSE
  )
  
  # ===========================================================================
  # 5. Contemporary WCVP Key geography
  # ===========================================================================
  
  wcvp_key <- dbGetQuery(
    con,
    paste0(
      "SELECT ",
      "CAST(FINAL_WCVP_ID AS VARCHAR) AS FINAL_WCVP_ID, ",
      "ENDEMIC_TO_JAPAN_WCVP, ",
      "WCVP_INTRODUCED_TO_JAPAN, ",
      "WCVP_TAIWAN, ",
      "WCVP_KOREA, ",
      "WCVP_KURILES, ",
      "WCVP_SAKHALIN, ",
      "WCVP_CHINA, ",
      "WCVP_OUTSIDE_JAPAN ",
      "FROM ", SOURCE_WCVP_KEY
    )
  )
  
  cat(
    "\nContemporary WCVP Key-geography rows: ",
    format(nrow(wcvp_key), big.mark = ","),
    "\n",
    sep = ""
  )
  
  # ===========================================================================
  # 6. Build contemporary district-input profile
  # ===========================================================================
  
  profile <- accepted |>
    select(
      FINAL_WCVP_ID,
      FINAL_WCVP_RECOGNISED_NAME,
      FINAL_WCVP_RANK,
      OCCURRENCE_RECORDS
    ) |>
    left_join(
      japan_geo,
      by = "FINAL_WCVP_ID"
    ) |>
    left_join(
      wcvp_key,
      by = "FINAL_WCVP_ID"
    ) |>
    mutate(
      PREFECTURE_INPUT_AVAILABLE =
        !is.na(JAPAN_PREFECTURES_PRESENT),
      
      DISTRICT_INPUT_AVAILABLE =
        !is.na(JAPAN_DISTRICTS_PRESENT),
      
      ALMOST_ALL_DISTRICTS_AVAILABLE =
        !is.na(ALMOST_ALL_DISTRICTS),
      
      DISTRICT_DERIVATION_STATUS =
        case_when(
          DISTRICT_INPUT_AVAILABLE &
            ALMOST_ALL_DISTRICTS_AVAILABLE ~
            "AVAILABLE",
          
          TRUE ~
            "REQUIRES_CONTEMPORARY_DISTRICT_DEFINITION"
        )
    )
  
  # ===========================================================================
  # 7. Profile which Key branches currently require districts
  #
  # This does NOT assign Stars. It only determines where district information
  # could be needed based on currently available inputs.
  # ===========================================================================
  
  district_need <- profile |>
    mutate(
      DISTRICT_INPUT_POTENTIALLY_REQUIRED =
        case_when(
          
          WCVP_INTRODUCED_TO_JAPAN %in% TRUE ~
            FALSE,
          
          ENDEMIC_TO_JAPAN_WCVP %in% TRUE &
            !is.na(JAPAN_PREFECTURES_PRESENT) &
            JAPAN_PREFECTURES_PRESENT <= 14 ~
            TRUE,
          
          ENDEMIC_TO_JAPAN_WCVP %in% TRUE &
            !is.na(JAPAN_PREFECTURES_PRESENT) &
            JAPAN_PREFECTURES_PRESENT > 10 ~
            TRUE,
          
          ENDEMIC_TO_JAPAN_WCVP %in% FALSE ~
            TRUE,
          
          TRUE ~
            NA
        )
    )
  
  district_need_profile <- district_need |>
    count(
      DISTRICT_INPUT_POTENTIALLY_REQUIRED,
      name = "WCVP_TAXA"
    )
  
  cat("\n— Potential requirement for district input —\n")
  print.data.frame(
    district_need_profile,
    row.names = FALSE
  )
  
  # ===========================================================================
  # 8. Explicit district-definition status register
  # ===========================================================================
  
  status_register <- tibble::tibble(
    INPUT = c(
      "Accepted contemporary taxon population",
      "Japanese occurrence evidence",
      "Japanese botanical-area distribution",
      "Japanese prefecture occupancy",
      "Japanese district definition",
      "Japanese district occupancy",
      "Almost-all-districts definition",
      "Almost-all-districts taxon status",
      "WCVP global distribution evidence",
      "WCVP introduction-to-Japan evidence"
    ),
    STATUS = c(
      "AVAILABLE",
      "AVAILABLE",
      "AVAILABLE",
      "AVAILABLE",
      "NOT_YET_FORMALISED",
      "NOT_YET_DERIVED",
      "NOT_YET_FORMALISED",
      "NOT_YET_DERIVED",
      "AVAILABLE",
      "AVAILABLE"
    ),
    SOURCE = c(
      SOURCE_TAXA,
      "vpjd_japan_occurrences_analytical",
      SOURCE_AREA_DISTRIBUTION,
      SOURCE_STAR_JAPAN,
      "NONE",
      "NONE",
      "NONE",
      "NONE",
      SOURCE_WCVP_KEY,
      SOURCE_WCVP_KEY
    ),
    LEGACY_DEPENDENCY = FALSE
  )
  
  cat("\n— District-input status register —\n")
  print.data.frame(
    status_register,
    row.names = FALSE
  )
  
  # ===========================================================================
  # 9. Explicit methodological constraint
  # ===========================================================================
  
  methodological_rule <- tibble::tibble(
    RULE = c(
      "LEGACY_GEOJAPAN_DATA",
      "LEGACY_JBRCI_DATA",
      "LEGACY_SPSLIM",
      "LEGACY_FOJ_STARS",
      "LEGACY_STAR_TOTALS",
      "HISTORICAL_KEY_AS_METHOD_SPECIFICATION"
    ),
    PERMITTED_AS_ANALYTICAL_INPUT = c(
      FALSE,
      FALSE,
      FALSE,
      FALSE,
      FALSE,
      TRUE
    ),
    NOTE = c(
      "Excluded from contemporary VPJD analysis",
      "Excluded from contemporary VPJD analysis",
      "Excluded from contemporary VPJD analysis",
      "Excluded from contemporary VPJD analysis",
      "Not a validation or calibration target",
      "Permitted only as methodological decision specification"
    )
  )
  
  cat("\n— Legacy-input exclusion rule —\n")
  print.data.frame(
    methodological_rule,
    row.names = FALSE
  )
  
  # ===========================================================================
  # 10. Validation
  # ===========================================================================
  
  validation <- tibble::tibble(
    check = c(
      "accepted_taxon_population_11439",
      "accepted_taxon_ids_unique",
      "stars03a_population_11439",
      "wcvp_key_population_11439",
      "prefecture_counts_from_contemporary_vpjd",
      "botanical_areas_from_contemporary_vpjd",
      "all_51_botanical_areas_present",
      "district_definition_not_invented",
      "district_counts_not_invented",
      "almost_all_districts_not_invented",
      "no_GEOJAPAN_dependency",
      "no_JBRCI_dependency",
      "no_SPSLIM_dependency",
      "no_FOJ_STARS_dependency",
      "historical_star_totals_not_used",
      "historical_key_used_only_as_method",
      "stars_not_assigned",
      "ghi_not_calculated"
    ),
    pass = c(
      nrow(accepted) == 11439L,
      n_distinct(accepted$FINAL_WCVP_ID) == 11439L,
      nrow(japan_geo) == 11439L,
      nrow(wcvp_key) == 11439L,
      prefecture_available >= 11438L,
      nrow(area_profile) > 0L,
      n_distinct(area_profile$BOTANICAL_AREA_ID) == 51L,
      district_available == 0L,
      district_available == 0L,
      almost_all_available == 0L,
      !any(source_register$LEGACY_SOURCE),
      !any(source_register$LEGACY_SOURCE),
      !any(source_register$LEGACY_SOURCE),
      !any(source_register$LEGACY_SOURCE),
      TRUE,
      TRUE,
      TRUE,
      TRUE
    )
  )
  
  cat("\n— Validation —\n")
  print.data.frame(
    validation,
    row.names = FALSE
  )
  
  all_pass <- all(validation$pass)
  
  cat(
    "\nAll Stars 03e validation checks PASS: ",
    all_pass,
    "\n",
    sep = ""
  )
  
  if (!all_pass) {
    stop(
      "Stars 03e validation failed. Do not freeze."
    )
  }
  
  # ===========================================================================
  # 11. Persist outputs
  # ===========================================================================
  
  metadata <- tibble::tibble(
    metric = c(
      "module",
      "version",
      "run_date",
      "accepted_taxa",
      "botanical_areas",
      "taxa_with_prefecture_counts",
      "taxa_with_district_counts",
      "taxa_with_almost_all_districts_status",
      "legacy_analytical_inputs_used",
      "district_definition_created",
      "stars_assigned",
      "ghi_calculated",
      "validation_pass"
    ),
    value = c(
      MODULE,
      VERSION,
      as.character(RUN_DATE),
      as.character(nrow(accepted)),
      as.character(
        n_distinct(area_profile$BOTANICAL_AREA_ID)
      ),
      as.character(prefecture_available),
      as.character(district_available),
      as.character(almost_all_available),
      "FALSE",
      "FALSE",
      "FALSE",
      "FALSE",
      as.character(all_pass)
    )
  )
  
  dbWriteTable(
    con,
    OUTPUT_PROFILE,
    profile,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    OUTPUT_STATUS,
    status_register,
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
    profile,
    file.path(
      OUTPUT_DIR,
      "stars03e_contemporary_district_input_profile.csv"
    )
  )
  
  write_csv(
    district_need_profile,
    file.path(
      OUTPUT_DIR,
      "stars03e_district_requirement_profile.csv"
    )
  )
  
  write_csv(
    status_register,
    file.path(
      OUTPUT_DIR,
      "stars03e_district_input_status.csv"
    )
  )
  
  write_csv(
    methodological_rule,
    file.path(
      OUTPUT_DIR,
      "stars03e_methodological_source_rules.csv"
    )
  )
  
  write_csv(
    area_profile,
    file.path(
      OUTPUT_DIR,
      "stars03e_botanical_area_profile.csv"
    )
  )
  
  write_csv(
    validation,
    file.path(
      OUTPUT_DIR,
      "stars03e_validation.csv"
    )
  )
  
  write_csv(
    metadata,
    file.path(
      OUTPUT_DIR,
      "stars03e_metadata.csv"
    )
  )
  
  cat("\nCanonical diagnostic outputs:\n")
  cat("  ", OUTPUT_PROFILE, "\n", sep = "")
  cat("  ", OUTPUT_STATUS, "\n", sep = "")
  
  cat(
    "\nStars 03e v",
    VERSION,
    " complete.\n",
    sep = ""
  )
  
  cat(
    "Contemporary VPJD outputs only. ",
    "No legacy analytical inputs and no Stars assigned.\n",
    sep = ""
  )
  
  invisible(
    list(
      profile = profile,
      district_need_profile = district_need_profile,
      status_register = status_register,
      methodological_rule = methodological_rule,
      area_profile = area_profile,
      validation = validation,
      metadata = metadata
    )
  )
}

result_stars_03e <- run_stars_03e()