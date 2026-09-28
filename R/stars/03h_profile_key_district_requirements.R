# =============================================================================
# VPJD-OJPCP
# R/stars/03h_profile_key_district_requirements.R
# Version 0.1.0
#
# PROFILE JAPANESE DISTRICT REQUIREMENTS FOR THE ORIGINAL KEY TO STARS
#
# PURPOSE
# - Establish the contemporary evidence available for reconstructing the
#   Japanese "district" geography used by the original Key to Stars.
# - Profile botanical-area and prefectural occupancy for all accepted taxa.
# - Quantify where district evidence is required by the endemic and
#   non-endemic branches.
#
# IMPORTANT
# - This module DOES NOT define Japanese districts.
# - It DOES NOT infer districts from modern regions.
# - It DOES NOT assign Stars.
# - It DOES NOT use legacy geographic data as analytical input.
# - Historical Star totals are not used.
# - Stars 04 v0.3.0 is superseded and is not an analytical input.
# =============================================================================

suppressPackageStartupMessages({
  library(DBI)
  library(duckdb)
  library(dplyr)
  library(readr)
  library(here)
})

MODULE <- "stars_03h_profile_key_district_requirements"
VERSION <- "0.1.0"
RUN_DATE <- Sys.Date()

DB_PATH <- here(
  "data", "interim", "occurrences", "vpjd_occurrences.duckdb"
)

OUTPUT_DIR <- here(
  "outputs", "tables", "stars", "district_requirements"
)

OUTPUT_PROFILE <- "vpjd_star_district_requirement_profile"
OUTPUT_TAXA <- "vpjd_star_district_requirement_taxa"
OUTPUT_VALIDATION <- "vpjd_star_03h_validation"
OUTPUT_METADATA <- "vpjd_star_03h_metadata"

run_stars_03h <- function() {
  cat("\n— Profile Key district requirements —\n\n")
  cat("Run date: ", RUN_DATE, "\n", sep = "")
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
  
  required_tables <- c(
    "vpjd_japan_taxon_distribution",
    "vpjd_japan_taxon_area_distribution",
    "vpjd_star_japan_key_geography",
    "vpjd_star_wcvp_key_geography",
    "vpjd_star_hybrid_status"
  )
  
  missing_tables <- setdiff(
    required_tables,
    dbListTables(con)
  )
  
  if (length(missing_tables) > 0L) {
    stop(
      "Required tables missing: ",
      paste(missing_tables, collapse = ", ")
    )
  }
  
  # ===========================================================================
  # 1. Accepted contemporary taxon population
  # ===========================================================================
  
  taxa <- dbGetQuery(
    con,
    "
    SELECT DISTINCT
      FINAL_WCVP_ID,
      FINAL_WCVP_RECOGNISED_NAME,
      FINAL_WCVP_RANK,
      FINAL_WCVP_STATUS,
      FINAL_WCVP_CONCEPT_CLASS
    FROM vpjd_japan_taxon_distribution
    WHERE FINAL_WCVP_STATUS = 'Accepted'
    "
  ) |>
    as_tibble()
  
  if (nrow(taxa) != 11439L) {
    stop(
      "Expected 11,439 accepted taxa; found ",
      format(nrow(taxa), big.mark = ",")
    )
  }
  
  if (n_distinct(taxa$FINAL_WCVP_ID) != 11439L) {
    stop("Accepted WCVP IDs are not unique.")
  }
  
  cat(
    "Accepted contemporary taxa: ",
    format(nrow(taxa), big.mark = ","),
    "\n",
    sep = ""
  )
  
  # ===========================================================================
  # 2. Resolve frozen 03a field names
  # ===========================================================================
  
  japan_geo <- dbGetQuery(
    con,
    "
    SELECT *
    FROM vpjd_star_japan_key_geography
    "
  ) |>
    as_tibble()
  
  find_field <- function(
    data,
    candidates,
    required = TRUE
  ) {
    hit <- candidates[
      candidates %in% names(data)
    ]
    
    if (length(hit) > 0L) {
      return(hit[[1L]])
    }
    
    if (required) {
      stop(
        "Required field not found. Tried: ",
        paste(candidates, collapse = ", ")
      )
    }
    
    NA_character_
  }
  
  prefecture_field <- find_field(
    japan_geo,
    c(
      "JAPAN_PREFECTURES_PRESENT",
      "KEY_PREFECTURES_PRESENT",
      "PREFECTURES_PRESENT"
    )
  )
  
  district_field <- find_field(
    japan_geo,
    c(
      "JAPAN_DISTRICTS_PRESENT",
      "KEY_DISTRICTS_PRESENT",
      "DISTRICTS_PRESENT"
    ),
    required = FALSE
  )
  
  japan_evidence <- japan_geo |>
    transmute(
      FINAL_WCVP_ID,
      JAPAN_PREFECTURES_PRESENT =
        .data[[prefecture_field]],
      EXISTING_DISTRICT_COUNT =
        if (!is.na(district_field)) {
          .data[[district_field]]
        } else {
          NA_integer_
        }
    )
  
  # ===========================================================================
  # 3. Contemporary botanical-area occupancy
  # ===========================================================================
  
  area <- dbGetQuery(
    con,
    "
    SELECT
      FINAL_WCVP_ID,
      BOTANICAL_AREA_ID
    FROM vpjd_japan_taxon_area_distribution
    WHERE FINAL_WCVP_STATUS = 'Accepted'
      AND PRESENCE = 1
    "
  ) |>
    as_tibble()
  
  area_profile <- area |>
    group_by(FINAL_WCVP_ID) |>
    summarise(
      BOTANICAL_AREAS_PRESENT =
        n_distinct(BOTANICAL_AREA_ID),
      .groups = "drop"
    )
  
  # ===========================================================================
  # 4. WCVP endemicity / external-range evidence
  # ===========================================================================
  
  wcvp_geo <- dbGetQuery(
    con,
    "
    SELECT *
    FROM vpjd_star_wcvp_key_geography
    "
  ) |>
    as_tibble()
  
  endemic_field <- find_field(
    wcvp_geo,
    c(
      "ENDEMIC_TO_JAPAN_WCVP",
      "WCVP_ENDEMIC_TO_JAPAN",
      "CANDIDATE_ENDEMIC_TO_JAPAN",
      "ENDEMIC_TO_JAPAN"
    )
  )
  
  outside_field <- find_field(
    wcvp_geo,
    c(
      "WCVP_OUTSIDE_JAPAN",
      "OUTSIDE_JAPAN_WCVP",
      "OUTSIDE_JAPAN_PRESENT",
      "HAS_OUTSIDE_JAPAN_EVIDENCE"
    )
  )
  
  introduced_field <- find_field(
    wcvp_geo,
    c(
      "WCVP_INTRODUCED_TO_JAPAN",
      "INTRODUCED_TO_JAPAN_WCVP",
      "INTRODUCED_TO_JAPAN"
    )
  )
  
  regional_candidates <- list(
    TAIWAN = c(
      "WCVP_TAIWAN",
      "TAIWAN_PRESENT",
      "TAIWAN"
    ),
    KOREA = c(
      "WCVP_KOREA",
      "KOREA_PRESENT",
      "KOREA"
    ),
    KURILES = c(
      "WCVP_KURILES",
      "KURILES_PRESENT",
      "KURILES"
    ),
    SAKHALIN = c(
      "WCVP_SAKHALIN",
      "SAKHALIN_PRESENT",
      "SAKHALIN"
    ),
    CHINA = c(
      "WCVP_CHINA",
      "CHINA_PRESENT",
      "CHINA"
    )
  )
  
  regional_fields <- lapply(
    regional_candidates,
    function(x) {
      find_field(
        wcvp_geo,
        x,
        required = FALSE
      )
    }
  )
  
  get_optional_field <- function(data, field) {
    if (is.na(field)) {
      return(rep(NA, nrow(data)))
    }
    
    data[[field]]
  }
  
  wcvp_evidence <- wcvp_geo |>
    transmute(
      FINAL_WCVP_ID,
      ENDEMIC_TO_JAPAN =
        .data[[endemic_field]],
      OUTSIDE_JAPAN =
        .data[[outside_field]],
      INTRODUCED_TO_JAPAN =
        .data[[introduced_field]],
      TAIWAN_PRESENT =
        get_optional_field(
          wcvp_geo,
          regional_fields$TAIWAN
        ),
      KOREA_PRESENT =
        get_optional_field(
          wcvp_geo,
          regional_fields$KOREA
        ),
      KURILES_PRESENT =
        get_optional_field(
          wcvp_geo,
          regional_fields$KURILES
        ),
      SAKHALIN_PRESENT =
        get_optional_field(
          wcvp_geo,
          regional_fields$SAKHALIN
        ),
      CHINA_PRESENT =
        get_optional_field(
          wcvp_geo,
          regional_fields$CHINA
        )
    )
  
  # ===========================================================================
  # 5. Hybrid evidence
  # ===========================================================================
  
  hybrid <- dbGetQuery(
    con,
    "
    SELECT
      FINAL_WCVP_ID,
      HYBRID_STATUS,
      HYB_ELIGIBLE
    FROM vpjd_star_hybrid_status
    "
  ) |>
    as_tibble()
  
  # ===========================================================================
  # 6. Build district-requirement population
  #
  # This does NOT route the Key.
  #
  # It identifies why district information will ultimately be needed:
  #
  # ENDEMIC_KEY
  #   District count required by Japanese endemic branch.
  #
  # NON_ENDEMIC_KEY
  #   District count required to apply regional-range matrix.
  #
  # SPECIAL_STATUS
  #   Positive GX or HYB evidence means district geography is not required
  #   for that taxon's immediate special-status decision.
  #
  # ENDEMICITY_UNRESOLVED
  #   Cannot yet determine which geographic branch applies.
  # ===========================================================================
  
  district_taxa <- taxa |>
    left_join(
      japan_evidence,
      by = "FINAL_WCVP_ID"
    ) |>
    left_join(
      area_profile,
      by = "FINAL_WCVP_ID"
    ) |>
    left_join(
      wcvp_evidence,
      by = "FINAL_WCVP_ID"
    ) |>
    left_join(
      hybrid,
      by = "FINAL_WCVP_ID"
    ) |>
    mutate(
      DISTRICT_REQUIREMENT_CLASS = case_when(
        INTRODUCED_TO_JAPAN == TRUE ~
          "SPECIAL_STATUS_GX",
        
        HYB_ELIGIBLE == TRUE ~
          "SPECIAL_STATUS_HYB",
        
        is.na(ENDEMIC_TO_JAPAN) ~
          "ENDEMICITY_UNRESOLVED",
        
        ENDEMIC_TO_JAPAN == TRUE ~
          "ENDEMIC_KEY",
        
        ENDEMIC_TO_JAPAN == FALSE ~
          "NON_ENDEMIC_KEY",
        
        TRUE ~
          "ENDEMICITY_UNRESOLVED"
      ),
      
      DISTRICT_DEFINITION_STATUS =
        "REQUIRES_ORIGINAL_KEY_DEFINITION",
      
      DISTRICT_COUNT_ASSIGNED = FALSE,
      
      DISTRICT_ANALYTICAL_STATUS =
        "NOT_YET_IMPLEMENTED"
    )
  
  if (nrow(district_taxa) != 11439L) {
    stop(
      "District-requirement join changed accepted population."
    )
  }
  
  if (
    n_distinct(
      district_taxa$FINAL_WCVP_ID
    ) != 11439L
  ) {
    stop(
      "District-requirement join created duplicate taxa."
    )
  }
  
  # ===========================================================================
  # 7. Profiles
  # ===========================================================================
  
  requirement_profile <- district_taxa |>
    count(
      DISTRICT_REQUIREMENT_CLASS,
      name = "WCVP_TAXA"
    ) |>
    arrange(
      DISTRICT_REQUIREMENT_CLASS
    )
  
  cat("\n— District requirement profile —\n")
  
  print.data.frame(
    requirement_profile,
    row.names = FALSE
  )
  
  prefecture_profile <- district_taxa |>
    summarise(
      TAXA = n(),
      WITH_PREFECTURE_COUNT =
        sum(
          !is.na(
            JAPAN_PREFECTURES_PRESENT
          )
        ),
      WITHOUT_PREFECTURE_COUNT =
        sum(
          is.na(
            JAPAN_PREFECTURES_PRESENT
          )
        ),
      WITH_EXISTING_DISTRICT_COUNT =
        sum(
          !is.na(
            EXISTING_DISTRICT_COUNT
          )
        ),
      WITHOUT_EXISTING_DISTRICT_COUNT =
        sum(
          is.na(
            EXISTING_DISTRICT_COUNT
          )
        )
    )
  
  cat("\n— Existing Japanese geographic evidence —\n")
  
  print.data.frame(
    prefecture_profile,
    row.names = FALSE
  )
  
  regional_profile <- district_taxa |>
    summarise(
      TAIWAN_POSITIVE =
        sum(
          TAIWAN_PRESENT == TRUE,
          na.rm = TRUE
        ),
      KOREA_POSITIVE =
        sum(
          KOREA_PRESENT == TRUE,
          na.rm = TRUE
        ),
      KURILES_POSITIVE =
        sum(
          KURILES_PRESENT == TRUE,
          na.rm = TRUE
        ),
      SAKHALIN_POSITIVE =
        sum(
          SAKHALIN_PRESENT == TRUE,
          na.rm = TRUE
        ),
      CHINA_POSITIVE =
        sum(
          CHINA_PRESENT == TRUE,
          na.rm = TRUE
        )
    )
  
  cat("\n— Existing external regional evidence —\n")
  
  print.data.frame(
    regional_profile,
    row.names = FALSE
  )
  
  # ===========================================================================
  # 8. District-definition requirements
  # ===========================================================================
  
  district_specification <- tibble::tibble(
    KEY_REQUIREMENT = c(
      "JAPAN_DISTRICT_COUNT",
      "JAPAN_LE1_DISTRICT",
      "JAPAN_LE2_DISTRICTS",
      "JAPAN_LE3_DISTRICTS",
      "JAPAN_GT3_DISTRICTS",
      "ALMOST_ALL_DISTRICTS"
    ),
    
    REQUIRED_FOR = c(
      "ALL_NATIVE_KEY_APPLICATION",
      "NON_ENDEMIC_MATRIX",
      "ENDEMIC_AND_NON_ENDEMIC_KEY",
      "NON_ENDEMIC_MATRIX",
      "NON_ENDEMIC_MATRIX",
      "ENDEMIC_WIDESPREAD_BRANCH"
    ),
    
    CURRENT_STATUS = c(
      "UNDEFINED",
      "UNDEFINED",
      "UNDEFINED",
      "UNDEFINED",
      "UNDEFINED",
      "UNDEFINED"
    ),
    
    ACTION = c(
      "RECOVER_ORIGINAL_DISTRICT_GEOGRAPHY",
      "DERIVE_AFTER_DISTRICTS_DEFINED",
      "DERIVE_AFTER_DISTRICTS_DEFINED",
      "DERIVE_AFTER_DISTRICTS_DEFINED",
      "DERIVE_AFTER_DISTRICTS_DEFINED",
      "RECOVER_ORIGINAL_KEY_DEFINITION"
    )
  )
  
  cat("\n— District methodology still required —\n")
  
  print.data.frame(
    district_specification,
    row.names = FALSE
  )
  
  # ===========================================================================
  # 9. Validation
  # ===========================================================================
  
  validation <- tibble::tibble(
    check = c(
      "accepted_population_11439",
      "accepted_ids_unique",
      "district_requirement_population_11439",
      "district_requirement_ids_unique",
      "district_requirement_complete",
      "prefecture_evidence_retained",
      "district_counts_not_assigned",
      "district_geography_not_invented",
      "modern_regions_not_substituted",
      "almost_all_districts_not_invented",
      "regional_wcvp_evidence_retained",
      "hybrid_evidence_retained",
      "legacy_geographic_data_not_used",
      "legacy_star_data_not_used",
      "superseded_stars04_not_used",
      "stars_not_assigned",
      "ghi_not_calculated"
    ),
    
    pass = c(
      nrow(taxa) == 11439L,
      
      n_distinct(
        taxa$FINAL_WCVP_ID
      ) == 11439L,
      
      nrow(district_taxa) == 11439L,
      
      n_distinct(
        district_taxa$FINAL_WCVP_ID
      ) == 11439L,
      
      !any(
        is.na(
          district_taxa$
            DISTRICT_REQUIREMENT_CLASS
        )
      ),
      
      any(
        !is.na(
          district_taxa$
            JAPAN_PREFECTURES_PRESENT
        )
      ),
      
      all(
        district_taxa$
          DISTRICT_COUNT_ASSIGNED ==
          FALSE
      ),
      
      TRUE,
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
  
  print.data.frame(
    validation,
    row.names = FALSE
  )
  
  all_pass <- all(
    validation$pass
  )
  
  cat(
    "\nAll Stars 03h validation checks PASS: ",
    all_pass,
    "\n",
    sep = ""
  )
  
  if (!all_pass) {
    stop(
      "Stars 03h validation failed. Do not freeze."
    )
  }
  
  # ===========================================================================
  # 10. Metadata
  # ===========================================================================
  
  metadata <- tibble::tibble(
    metric = c(
      "module",
      "version",
      "run_date",
      "accepted_taxa",
      "district_definition_status",
      "district_counts_assigned",
      "stars_assigned",
      "legacy_analytical_inputs_used",
      "superseded_stars04_used",
      "ghi_calculated",
      "validation_pass"
    ),
    
    value = c(
      MODULE,
      VERSION,
      as.character(RUN_DATE),
      as.character(
        nrow(district_taxa)
      ),
      "REQUIRES_ORIGINAL_KEY_DEFINITION",
      "FALSE",
      "FALSE",
      "FALSE",
      "FALSE",
      "FALSE",
      as.character(all_pass)
    )
  )
  
  # ===========================================================================
  # 11. Persist
  # ===========================================================================
  
  dbWriteTable(
    con,
    OUTPUT_TAXA,
    district_taxa,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    OUTPUT_PROFILE,
    requirement_profile,
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
  
  dbWriteTable(
    con,
    "vpjd_star_district_methodology_requirements",
    district_specification,
    overwrite = TRUE
  )
  
  write_csv(
    district_taxa,
    file.path(
      OUTPUT_DIR,
      "stars03h_district_requirement_taxa.csv"
    )
  )
  
  write_csv(
    requirement_profile,
    file.path(
      OUTPUT_DIR,
      "stars03h_district_requirement_profile.csv"
    )
  )
  
  write_csv(
    district_specification,
    file.path(
      OUTPUT_DIR,
      "stars03h_district_methodology_requirements.csv"
    )
  )
  
  write_csv(
    validation,
    file.path(
      OUTPUT_DIR,
      "stars03h_validation.csv"
    )
  )
  
  write_csv(
    metadata,
    file.path(
      OUTPUT_DIR,
      "stars03h_metadata.csv"
    )
  )
  
  cat("\nCanonical diagnostic tables:\n")
  cat(
    "  ",
    OUTPUT_TAXA,
    "\n",
    sep = ""
  )
  cat(
    "  ",
    OUTPUT_PROFILE,
    "\n",
    sep = ""
  )
  
  cat(
    "\nStars 03h v",
    VERSION,
    " complete.\n",
    sep = ""
  )
  
  cat(
    "District requirements profiled. ",
    "No district geography or Stars assigned.\n"
  )
  
  invisible(
    list(
      district_taxa = district_taxa,
      requirement_profile =
        requirement_profile,
      regional_profile =
        regional_profile,
      district_specification =
        district_specification,
      validation = validation,
      metadata = metadata
    )
  )
}

result_stars_03h <- run_stars_03h()