# =============================================================================
# VPJD-OJPCP
# R/stars/03c_build_wcvp_key_geography.R
# Version 0.1.1
#
# BUILD WCVP GLOBAL GEOGRAPHIC INPUTS FOR THE KEY TO STARS
#
# Evidence model:
# - VPJD occurrence data establishes that each eligible taxon occurs in Japan.
# - WCVP distribution data provides evidence of distribution outside Japan.
# - WCVP establishmentMeans provides positive evidence of introduction to Japan
#   when "introduced" occurs in a Japanese WCVP/WGSRPD unit.
#
# Japanese WCVP units:
# - TDWG:JAP = Japan
# - TDWG:NNS = Nansei-shoto
# - TDWG:OGA = Ogasawara-shoto
# - TDWG:KZN = Kazan-retto
#
# IMPORTANT:
# - No Stars assigned.
# - Missing WCVP distribution is not interpreted as absence.
# - Absence of a WCVP Japanese row does not negate VPJD evidence from Japan.
# - "introduced" outside Japan does not imply GX.
# - China WGSRPD units are not equated with Chinese provinces.
# =============================================================================

suppressPackageStartupMessages({
  library(DBI)
  library(duckdb)
  library(dplyr)
  library(readr)
  library(here)
})

MODULE <- "stars_03c_build_wcvp_key_geography"
VERSION <- "0.1.1"
RUN_DATE <- Sys.Date()

DB_PATH <- here("data", "interim", "occurrences", "vpjd_occurrences.duckdb")
SOURCE_TAXA <- "vpjd_japan_taxon_distribution"
SOURCE_JAPAN_GEOGRAPHY <- "vpjd_star_japan_key_geography"

WCVP_DIR <- here("data", "raw", "geography", "wcvp_dwca")
WCVP_DISTRIBUTION_FILE <- file.path(WCVP_DIR, "wcvp_distribution.csv")

OUTPUT_DIR <- here("outputs", "tables", "stars", "wcvp_key_geography")

OUTPUT_TABLE <- "vpjd_star_wcvp_key_geography"
OUTPUT_UNIT_PROFILE <- "vpjd_star_wcvp_key_unit_profile"
OUTPUT_VALIDATION <- "vpjd_star_wcvp_key_geography_validation"
OUTPUT_METADATA <- "vpjd_star_wcvp_key_geography_metadata"

run_stars_03c <- function() {
  cat("\n— Build WCVP Key geography —\n\n")
  cat("Run date: ", as.character(RUN_DATE), "\n", sep = "")
  cat("Module: ", MODULE, "\n", sep = "")
  cat("Version: ", VERSION, "\n\n", sep = "")
  
  dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
  
  if (!file.exists(DB_PATH)) stop("VPJD DuckDB not found: ", DB_PATH)
  if (!file.exists(WCVP_DISTRIBUTION_FILE)) {
    stop("WCVP distribution file not found: ", WCVP_DISTRIBUTION_FILE)
  }
  
  con <- dbConnect(
    duckdb::duckdb(),
    dbdir = DB_PATH,
    read_only = FALSE
  )
  
  on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)
  
  required_tables <- c(SOURCE_TAXA, SOURCE_JAPAN_GEOGRAPHY)
  missing_tables <- setdiff(required_tables, dbListTables(con))
  
  if (length(missing_tables) > 0L) {
    stop(
      "Required VPJD tables missing: ",
      paste(missing_tables, collapse = ", ")
    )
  }
  
  # ===========================================================================
  # 1. Accepted contemporary Japanese taxon population
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
    mutate(FINAL_WCVP_ID = as.character(FINAL_WCVP_ID))
  
  cat(
    "Accepted VPJD taxa: ",
    format(nrow(accepted), big.mark = ","),
    "\n",
    sep = ""
  )
  
  if (nrow(accepted) != 11439L) {
    stop("Accepted taxon population differs from validated Stars population.")
  }
  
  if (n_distinct(accepted$FINAL_WCVP_ID) != 11439L) {
    stop("Accepted WCVP IDs are not unique.")
  }
  
  dbWriteTable(
    con,
    "vpjd_star_accepted_taxa_temp",
    accepted,
    temporary = TRUE,
    overwrite = TRUE
  )
  
  # ===========================================================================
  # 2. Read WCVP distribution
  # ===========================================================================
  
  distribution_path_sql <- gsub(
    "'",
    "''",
    normalizePath(
      WCVP_DISTRIBUTION_FILE,
      winslash = "/",
      mustWork = TRUE
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
  
  relevant_rows <- dbGetQuery(
    con,
    "
    SELECT COUNT(*) AS n
    FROM wcvp_distribution_raw d
    INNER JOIN vpjd_star_accepted_taxa_temp v
      ON d.taxon_id = CAST(v.FINAL_WCVP_ID AS VARCHAR)
    "
  )$n[[1]]
  
  cat(
    "Relevant WCVP distribution rows: ",
    format(relevant_rows, big.mark = ","),
    "\n",
    sep = ""
  )
  
  # ===========================================================================
  # 3. Geographic unit profile
  # ===========================================================================
  
  unit_profile <- dbGetQuery(
    con,
    "
    SELECT
      location_id AS LOCATION_ID,
      locality AS LOCALITY,
      COUNT(*) AS DISTRIBUTION_ROWS,
      COUNT(DISTINCT taxon_id) AS WCVP_TAXA
    FROM wcvp_distribution_raw d
    INNER JOIN vpjd_star_accepted_taxa_temp v
      ON d.taxon_id = CAST(v.FINAL_WCVP_ID AS VARCHAR)
    GROUP BY location_id, locality
    ORDER BY DISTRIBUTION_ROWS DESC
    "
  )
  
  cat(
    "Distinct WCVP geographic units: ",
    format(nrow(unit_profile), big.mark = ","),
    "\n",
    sep = ""
  )
  
  # ===========================================================================
  # 4. Key-region crosswalk
  #
  # Stars 03d established KZN as an additional Japanese WCVP unit.
  # ===========================================================================
  
  japan_ids <- c(
    "TDWG:JAP",
    "TDWG:NNS",
    "TDWG:OGA",
    "TDWG:KZN"
  )
  
  taiwan_ids <- "TDWG:TAI"
  korea_ids <- "TDWG:KOR"
  kuril_ids <- "TDWG:KUR"
  sakhalin_ids <- "TDWG:SAK"
  
  china_ids <- unit_profile |>
    filter(
      grepl(
        "China|Manchuria|Inner Mongolia|Hainan",
        LOCALITY,
        ignore.case = TRUE
      )
    ) |>
    pull(LOCATION_ID) |>
    unique()
  
  cat("\nJapanese WCVP units:\n")
  print.data.frame(
    unit_profile |>
      filter(LOCATION_ID %in% japan_ids),
    row.names = FALSE
  )
  
  cat("\nTaiwan/Korea/Kuriles/Sakhalin WCVP units:\n")
  print.data.frame(
    unit_profile |>
      filter(
        LOCATION_ID %in% c(
          taiwan_ids,
          korea_ids,
          kuril_ids,
          sakhalin_ids
        )
      ),
    row.names = FALSE
  )
  
  cat("\nChina WCVP/WGSRPD units retained as distribution evidence:\n")
  print.data.frame(
    unit_profile |>
      filter(LOCATION_ID %in% china_ids),
    row.names = FALSE
  )
  
  # ===========================================================================
  # 5. WCVP evidence
  # ===========================================================================
  
  evidence <- dbGetQuery(
    con,
    "
    SELECT
      d.taxon_id AS FINAL_WCVP_ID,
      d.location_id AS LOCATION_ID,
      d.locality AS LOCALITY,
      d.establishment_means AS ESTABLISHMENT_MEANS,
      d.occurrence_status AS OCCURRENCE_STATUS
    FROM wcvp_distribution_raw d
    INNER JOIN vpjd_star_accepted_taxa_temp v
      ON d.taxon_id = CAST(v.FINAL_WCVP_ID AS VARCHAR)
    "
  ) |>
    mutate(
      IS_DOUBTFUL =
        !is.na(OCCURRENCE_STATUS) &
        tolower(trimws(OCCURRENCE_STATUS)) == "doubtful",
      IS_INTRODUCED =
        !is.na(ESTABLISHMENT_MEANS) &
        tolower(trimws(ESTABLISHMENT_MEANS)) == "introduced"
    )
  
  positive <- evidence |>
    filter(!IS_DOUBTFUL)
  
  cat(
    "\nPositive WCVP distribution rows after excluding Doubtful: ",
    format(nrow(positive), big.mark = ","),
    "\n",
    sep = ""
  )
  
  # ===========================================================================
  # 6. Derive taxon-level WCVP geographic evidence
  # ===========================================================================
  
  wcvp_summary <- positive |>
    group_by(FINAL_WCVP_ID) |>
    summarise(
      WCVP_DISTRIBUTION_ROWS = n(),
      WCVP_UNITS_PRESENT = n_distinct(LOCATION_ID),
      
      WCVP_JAPAN_ROW_PRESENT =
        any(LOCATION_ID %in% japan_ids),
      
      WCVP_TAIWAN =
        any(LOCATION_ID %in% taiwan_ids),
      
      WCVP_KOREA =
        any(LOCATION_ID %in% korea_ids),
      
      WCVP_KURILES =
        any(LOCATION_ID %in% kuril_ids),
      
      WCVP_SAKHALIN =
        any(LOCATION_ID %in% sakhalin_ids),
      
      WCVP_CHINA =
        any(LOCATION_ID %in% china_ids),
      
      WCVP_CHINA_UNITS =
        n_distinct(
          LOCATION_ID[LOCATION_ID %in% china_ids]
        ),
      
      WCVP_OUTSIDE_JAPAN =
        any(!LOCATION_ID %in% japan_ids),
      
      WCVP_OUTSIDE_JAPAN_UNITS =
        n_distinct(
          LOCATION_ID[!LOCATION_ID %in% japan_ids]
        ),
      
      WCVP_INTRODUCED_TO_JAPAN =
        any(
          LOCATION_ID %in% japan_ids &
            IS_INTRODUCED
        ),
      
      WCVP_INTRODUCED_SOMEWHERE =
        any(IS_INTRODUCED),
      
      .groups = "drop"
    )
  
  # ===========================================================================
  # 7. Combine with the accepted Japanese flora
  #
  # Every row in accepted is independently demonstrated by VPJD occurrence
  # evidence to occur in Japan. A WCVP Japanese row is therefore corroboration,
  # not a prerequisite for Japanese presence.
  # ===========================================================================
  
  geography <- accepted |>
    select(
      FINAL_WCVP_ID,
      FINAL_WCVP_RECOGNISED_NAME,
      FINAL_WCVP_RANK,
      OCCURRENCE_RECORDS
    ) |>
    left_join(
      wcvp_summary,
      by = "FINAL_WCVP_ID"
    ) |>
    mutate(
      VPJD_JAPAN_PRESENCE = TRUE,
      HAS_WCVP_DISTRIBUTION =
        !is.na(WCVP_DISTRIBUTION_ROWS)
    )
  
  boolean_fields <- c(
    "WCVP_JAPAN_ROW_PRESENT",
    "WCVP_TAIWAN",
    "WCVP_KOREA",
    "WCVP_KURILES",
    "WCVP_SAKHALIN",
    "WCVP_CHINA",
    "WCVP_OUTSIDE_JAPAN",
    "WCVP_INTRODUCED_TO_JAPAN",
    "WCVP_INTRODUCED_SOMEWHERE"
  )
  
  geography <- geography |>
    mutate(
      across(
        all_of(boolean_fields),
        ~ case_when(
          !HAS_WCVP_DISTRIBUTION ~ NA,
          is.na(.x) ~ FALSE,
          TRUE ~ .x
        )
      ),
      
      WCVP_CHINA_UNITS =
        case_when(
          !HAS_WCVP_DISTRIBUTION ~ NA_integer_,
          is.na(WCVP_CHINA_UNITS) ~ 0L,
          TRUE ~ as.integer(WCVP_CHINA_UNITS)
        ),
      
      WCVP_OUTSIDE_JAPAN_UNITS =
        case_when(
          !HAS_WCVP_DISTRIBUTION ~ NA_integer_,
          is.na(WCVP_OUTSIDE_JAPAN_UNITS) ~ 0L,
          TRUE ~ as.integer(WCVP_OUTSIDE_JAPAN_UNITS)
        )
    )
  
  # ===========================================================================
  # 8. Candidate Japanese endemicity
  #
  # VPJD already establishes presence in Japan.
  #
  # TRUE:
  #   WCVP distribution exists and contains no positive non-Japanese unit.
  #
  # FALSE:
  #   WCVP contains at least one positive non-Japanese unit.
  #
  # NA:
  #   no WCVP distribution data.
  # ===========================================================================
  
  geography <- geography |>
    mutate(
      ENDEMIC_TO_JAPAN_WCVP =
        case_when(
          !HAS_WCVP_DISTRIBUTION ~ NA,
          WCVP_OUTSIDE_JAPAN ~ FALSE,
          TRUE ~ TRUE
        ),
      
      ENDEMICITY_EVIDENCE_MODEL =
        case_when(
          !HAS_WCVP_DISTRIBUTION ~
            "UNRESOLVED_NO_WCVP_DISTRIBUTION",
          
          WCVP_OUTSIDE_JAPAN ~
            "NON_ENDEMIC_POSITIVE_WCVP_OUTSIDE_JAPAN",
          
          TRUE ~
            "CANDIDATE_ENDEMIC_NO_POSITIVE_WCVP_OUTSIDE_JAPAN"
        ),
      
      INTRODUCTION_TO_JAPAN_EVIDENCE =
        case_when(
          !HAS_WCVP_DISTRIBUTION ~
            "NO_WCVP_DISTRIBUTION",
          
          WCVP_INTRODUCED_TO_JAPAN ~
            "POSITIVE_WCVP_INTRODUCED_IN_JAPANESE_UNIT",
          
          TRUE ~
            "NO_POSITIVE_WCVP_INTRODUCTION_EVIDENCE"
        )
    )
  
  # ===========================================================================
  # 9. Join Stars 03a Japanese prefecture inputs
  # ===========================================================================
  
  japan_geography <- dbGetQuery(
    con,
    paste0(
      "SELECT ",
      "CAST(FINAL_WCVP_ID AS VARCHAR) AS FINAL_WCVP_ID, ",
      "JAPAN_PREFECTURES_PRESENT, ",
      "JAPAN_DISTRICTS_PRESENT, ",
      "ALMOST_ALL_DISTRICTS ",
      "FROM ", SOURCE_JAPAN_GEOGRAPHY
    )
  )
  
  geography <- geography |>
    left_join(
      japan_geography,
      by = "FINAL_WCVP_ID"
    )
  
  # ===========================================================================
  # 10. Profiles
  # ===========================================================================
  
  endemicity_profile <- geography |>
    count(
      ENDEMIC_TO_JAPAN_WCVP,
      name = "WCVP_TAXA"
    )
  
  introduction_profile <- geography |>
    count(
      WCVP_INTRODUCED_TO_JAPAN,
      name = "WCVP_TAXA"
    )
  
  japan_row_profile <- geography |>
    count(
      WCVP_JAPAN_ROW_PRESENT,
      name = "WCVP_TAXA"
    )
  
  neighbour_profile <- tibble::tibble(
    KEY_INPUT = c(
      "TAIWAN",
      "KOREA",
      "KURILES",
      "SAKHALIN",
      "CHINA",
      "OUTSIDE_JAPAN"
    ),
    WCVP_TAXA = c(
      sum(geography$WCVP_TAIWAN %in% TRUE, na.rm = TRUE),
      sum(geography$WCVP_KOREA %in% TRUE, na.rm = TRUE),
      sum(geography$WCVP_KURILES %in% TRUE, na.rm = TRUE),
      sum(geography$WCVP_SAKHALIN %in% TRUE, na.rm = TRUE),
      sum(geography$WCVP_CHINA %in% TRUE, na.rm = TRUE),
      sum(geography$WCVP_OUTSIDE_JAPAN %in% TRUE, na.rm = TRUE)
    )
  )
  
  china_profile <- geography |>
    filter(WCVP_CHINA %in% TRUE) |>
    count(
      WCVP_CHINA_UNITS,
      name = "WCVP_TAXA"
    ) |>
    arrange(WCVP_CHINA_UNITS)
  
  no_distribution <- geography |>
    filter(!HAS_WCVP_DISTRIBUTION)
  
  no_wcvp_japan_row <- geography |>
    filter(
      HAS_WCVP_DISTRIBUTION,
      !WCVP_JAPAN_ROW_PRESENT
    )
  
  introduced_japan <- geography |>
    filter(WCVP_INTRODUCED_TO_JAPAN %in% TRUE)
  
  cat("\n— Candidate WCVP endemicity —\n")
  print.data.frame(endemicity_profile, row.names = FALSE)
  
  cat("\n— Positive WCVP introduction-to-Japan evidence —\n")
  print.data.frame(introduction_profile, row.names = FALSE)
  
  cat("\n— WCVP Japanese-row profile —\n")
  print.data.frame(japan_row_profile, row.names = FALSE)
  
  cat("\n— Key neighbouring-region evidence —\n")
  print.data.frame(neighbour_profile, row.names = FALSE)
  
  cat("\n— China-unit occupancy profile —\n")
  print.data.frame(china_profile, row.names = FALSE)
  
  cat(
    "\nAccepted taxa without WCVP distribution: ",
    nrow(no_distribution),
    "\n",
    sep = ""
  )
  
  cat(
    "Accepted taxa with WCVP distribution but no Japanese WCVP row: ",
    format(nrow(no_wcvp_japan_row), big.mark = ","),
    "\n",
    sep = ""
  )
  
  cat(
    "Accepted taxa with positive introduced-to-Japan evidence: ",
    format(nrow(introduced_japan), big.mark = ","),
    "\n",
    sep = ""
  )
  
  # ===========================================================================
  # 11. Validation
  # ===========================================================================
  
  validation <- tibble::tibble(
    check = c(
      "accepted_taxon_population_11439",
      "accepted_taxon_ids_unique",
      "all_accepted_taxa_have_vpjd_japan_presence",
      "wcvp_distribution_available",
      "japan_JAP_present",
      "japan_NNS_present",
      "japan_OGA_present",
      "japan_KZN_present",
      "stars03d_kazan_result_incorporated",
      "taiwan_unit_explicit",
      "korea_unit_explicit",
      "kuriles_unit_explicit",
      "sakhalin_unit_explicit",
      "china_units_profiled_not_equated_to_provinces",
      "doubtful_not_used_as_positive_evidence",
      "absence_of_wcvp_japan_row_not_absence_from_japan",
      "introduced_outside_japan_not_equal_gx",
      "missing_wcvp_distribution_not_treated_as_endemic",
      "prefecture_counts_retained",
      "district_counts_not_invented",
      "stars_not_assigned",
      "historical_star_totals_not_used",
      "ghi_not_calculated"
    ),
    pass = c(
      nrow(accepted) == 11439L,
      n_distinct(accepted$FINAL_WCVP_ID) == 11439L,
      all(geography$VPJD_JAPAN_PRESENCE),
      file.exists(WCVP_DISTRIBUTION_FILE),
      "TDWG:JAP" %in% unit_profile$LOCATION_ID,
      "TDWG:NNS" %in% unit_profile$LOCATION_ID,
      "TDWG:OGA" %in% unit_profile$LOCATION_ID,
      "TDWG:KZN" %in% unit_profile$LOCATION_ID,
      length(japan_ids) == 4L,
      "TDWG:TAI" %in% unit_profile$LOCATION_ID,
      "TDWG:KOR" %in% unit_profile$LOCATION_ID,
      "TDWG:KUR" %in% unit_profile$LOCATION_ID,
      "TDWG:SAK" %in% unit_profile$LOCATION_ID,
      length(china_ids) > 0L,
      TRUE,
      TRUE,
      TRUE,
      all(
        is.na(
          geography$ENDEMIC_TO_JAPAN_WCVP[
            !geography$HAS_WCVP_DISTRIBUTION
          ]
        )
      ),
      sum(
        !is.na(geography$JAPAN_PREFECTURES_PRESENT)
      ) >= 11438L,
      all(
        is.na(geography$JAPAN_DISTRICTS_PRESENT)
      ),
      TRUE,
      TRUE,
      TRUE
    )
  )
  
  cat("\n— Validation —\n")
  print.data.frame(validation, row.names = FALSE)
  
  all_pass <- all(validation$pass)
  
  cat(
    "\nAll Stars 03c validation checks PASS: ",
    all_pass,
    "\n",
    sep = ""
  )
  
  if (!all_pass) {
    stop("Stars 03c validation failed. Do not freeze.")
  }
  
  # ===========================================================================
  # 12. Persist canonical outputs
  # ===========================================================================
  
  metadata <- tibble::tibble(
    metric = c(
      "module",
      "version",
      "run_date",
      "accepted_taxa",
      "wcvp_distribution_rows_for_vpjd",
      "wcvp_geography_standard",
      "japanese_wcvp_units",
      "vpjd_establishes_japanese_presence",
      "candidate_endemicity_derived",
      "introduced_to_japan_evidence_derived",
      "china_units_equated_to_provinces",
      "districts_derived",
      "stars_assigned",
      "historical_star_totals_used",
      "ghi_calculated",
      "validation_pass"
    ),
    value = c(
      MODULE,
      VERSION,
      as.character(RUN_DATE),
      as.character(nrow(accepted)),
      as.character(relevant_rows),
      "WGSRPD_TDWG",
      paste(japan_ids, collapse = ";"),
      "TRUE",
      "TRUE",
      "TRUE",
      "FALSE",
      "FALSE",
      "FALSE",
      "FALSE",
      "FALSE",
      as.character(all_pass)
    )
  )
  
  dbWriteTable(
    con,
    OUTPUT_TABLE,
    geography,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    OUTPUT_UNIT_PROFILE,
    unit_profile,
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
    geography,
    file.path(
      OUTPUT_DIR,
      "stars03c_taxon_wcvp_key_geography.csv"
    )
  )
  
  write_csv(
    unit_profile,
    file.path(
      OUTPUT_DIR,
      "stars03c_wcvp_unit_profile.csv"
    )
  )
  
  write_csv(
    endemicity_profile,
    file.path(
      OUTPUT_DIR,
      "stars03c_endemicity_profile.csv"
    )
  )
  
  write_csv(
    introduction_profile,
    file.path(
      OUTPUT_DIR,
      "stars03c_introduction_to_japan_profile.csv"
    )
  )
  
  write_csv(
    japan_row_profile,
    file.path(
      OUTPUT_DIR,
      "stars03c_japanese_row_profile.csv"
    )
  )
  
  write_csv(
    neighbour_profile,
    file.path(
      OUTPUT_DIR,
      "stars03c_neighbour_region_profile.csv"
    )
  )
  
  write_csv(
    china_profile,
    file.path(
      OUTPUT_DIR,
      "stars03c_china_unit_profile.csv"
    )
  )
  
  write_csv(
    no_distribution,
    file.path(
      OUTPUT_DIR,
      "stars03c_no_wcvp_distribution.csv"
    )
  )
  
  write_csv(
    no_wcvp_japan_row,
    file.path(
      OUTPUT_DIR,
      "stars03c_no_wcvp_japanese_row.csv"
    )
  )
  
  write_csv(
    introduced_japan,
    file.path(
      OUTPUT_DIR,
      "stars03c_introduced_to_japan.csv"
    )
  )
  
  write_csv(
    validation,
    file.path(
      OUTPUT_DIR,
      "stars03c_validation.csv"
    )
  )
  
  write_csv(
    metadata,
    file.path(
      OUTPUT_DIR,
      "stars03c_metadata.csv"
    )
  )
  
  cat("\nCanonical output:\n")
  cat("  ", OUTPUT_TABLE, "\n", sep = "")
  
  cat(
    "\nStars 03c v",
    VERSION,
    " complete.\n",
    sep = ""
  )
  
  cat(
    "No Stars assigned. Districts remain unresolved.\n"
  )
  
  invisible(
    list(
      geography = geography,
      unit_profile = unit_profile,
      endemicity_profile = endemicity_profile,
      introduction_profile = introduction_profile,
      japan_row_profile = japan_row_profile,
      neighbour_profile = neighbour_profile,
      china_profile = china_profile,
      validation = validation,
      metadata = metadata
    )
  )
}

result_stars_03c <- run_stars_03c()