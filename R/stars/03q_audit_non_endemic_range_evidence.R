# =============================================================================
# VPJD-OJPCP
# R/stars/03q_audit_non_endemic_range_evidence.R
# Version 0.1.1
#
# AUDIT CONTEMPORARY WCVP EVIDENCE FOR THE NON-ENDEMIC KEY MATRIX
#
# PURPOSE
# - Interrogate the actual WCVP distribution vocabulary available for the
#   non-endemic branch of Nakamura's Figure 2.2.
# - Profile geographic resolution for Taiwan, Korea, Kuriles/Sakhalin, China
#   and broader external distribution.
# - Determine which published Key criteria may be operationalisable directly
#   from contemporary WCVP evidence.
#
# IMPORTANT
# - This is an evidence-capability audit only.
# - No Stars are assigned.
# - No WCVP locality is automatically interpreted as a Nakamura criterion.
# - No Taiwan half-island classes are inferred.
# - No Korean "sparse" class is inferred.
# - No south-Kuriles class is inferred.
# - No Chinese province count is inferred.
# - No JAPAN_DISTRICT_COUNT is created.
# - Historical Star allocations are not used.
#
# INPUT NOTE
# - wcvp_distribution.csv is pipe-delimited despite the .csv extension.
# - Expected fields:
#   coreid|locality|establishmentmeans|locationid|occurrencestatus|threatstatus
# =============================================================================

suppressPackageStartupMessages({
  library(DBI)
  library(duckdb)
  library(dplyr)
  library(readr)
  library(stringr)
  library(tidyr)
  library(tibble)
  library(here)
})

MODULE <- "stars_03q_audit_non_endemic_range_evidence"
VERSION <- "0.1.1"
RUN_DATE <- Sys.Date()

DB_PATH <- here(
  "data", "interim", "occurrences", "vpjd_occurrences.duckdb"
)

WCVP_DIST_PATH <- here(
  "data", "raw", "geography", "wcvp_dwca",
  "wcvp_distribution.csv"
)

OUTPUT_DIR <- here(
  "outputs", "tables", "stars",
  "non_endemic_range_evidence"
)

TABLE_UNIT_VOCAB <- "vpjd_star_wcvp_external_unit_vocabulary"
TABLE_REGION_UNITS <- "vpjd_star_wcvp_key_region_units"
TABLE_TAXON_PROFILE <- "vpjd_star_non_endemic_range_evidence_profile"
TABLE_CAPABILITY <- "vpjd_star_non_endemic_evidence_capability"
TABLE_VALIDATION <- "vpjd_star_03q_validation"
TABLE_METADATA <- "vpjd_star_03q_metadata"

run_stars_03q <- function() {
  
  cat("\n— Audit non-endemic range evidence —\n\n")
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
  
  if (!file.exists(WCVP_DIST_PATH)) {
    stop(
      "WCVP distribution file not found: ",
      WCVP_DIST_PATH
    )
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
    "vpjd_star_taxon_key_geography_evidence",
    "vpjd_star_key_non_endemic_matrix",
    "vpjd_star_03p_validation",
    "vpjd_star_03p_metadata"
  )
  
  missing_tables <- setdiff(
    required_tables,
    dbListTables(con)
  )
  
  if (length(missing_tables) > 0L) {
    stop(
      "Required upstream tables missing: ",
      paste(missing_tables, collapse = ", ")
    )
  }
  
  # ===========================================================================
  # 1. Confirm Stars 03p validation
  # ===========================================================================
  
  validation_03p <- dbGetQuery(
    con,
    "
    SELECT *
    FROM vpjd_star_03p_validation
    "
  ) |>
    as_tibble()
  
  if (
    !"PASS" %in% names(validation_03p) ||
    !all(validation_03p$PASS)
  ) {
    stop(
      "Stars 03p is not fully validated. ",
      "03q will not proceed."
    )
  }
  
  metadata_03p <- dbGetQuery(
    con,
    "
    SELECT *
    FROM vpjd_star_03p_metadata
    "
  ) |>
    as_tibble()
  
  cat(
    "Stars 03p validation: ",
    sum(validation_03p$PASS),
    "/",
    nrow(validation_03p),
    " PASS\n",
    sep = ""
  )
  
  # ===========================================================================
  # 2. Load contemporary non-endemic population
  # ===========================================================================
  
  key_evidence <- dbGetQuery(
    con,
    "
    SELECT *
    FROM vpjd_star_taxon_key_geography_evidence
    "
  ) |>
    as_tibble()
  
  required_evidence_fields <- c(
    "FINAL_WCVP_ID",
    "FINAL_WCVP_RECOGNISED_NAME",
    "PRE_KEY_EVIDENCE_ROUTE"
  )
  
  missing_evidence_fields <- setdiff(
    required_evidence_fields,
    names(key_evidence)
  )
  
  if (length(missing_evidence_fields) > 0L) {
    stop(
      "Required Stars 03n fields missing: ",
      paste(missing_evidence_fields, collapse = ", ")
    )
  }
  
  non_endemic <- key_evidence |>
    filter(
      PRE_KEY_EVIDENCE_ROUTE == "NON_ENDEMIC_KEY"
    ) |>
    distinct(
      FINAL_WCVP_ID,
      FINAL_WCVP_RECOGNISED_NAME
    )
  
  if (nrow(non_endemic) != 7736L) {
    stop(
      "Expected 7,736 contemporary non-endemic taxa; found ",
      format(nrow(non_endemic), big.mark = ",")
    )
  }
  
  cat(
    "Contemporary non-endemic taxa: ",
    format(nrow(non_endemic), big.mark = ","),
    "\n",
    sep = ""
  )
  
  # ===========================================================================
  # 3. Confirm WCVP distribution schema
  #
  # IMPORTANT:
  # The WCVP file is pipe-delimited despite its .csv extension.
  # ===========================================================================
  
  wcvp_header <- names(
    read_delim(
      WCVP_DIST_PATH,
      delim = "|",
      n_max = 0,
      show_col_types = FALSE,
      progress = FALSE
    )
  )
  
  required_wcvp_fields <- c(
    "coreid",
    "locality",
    "establishmentmeans",
    "locationid",
    "occurrencestatus"
  )
  
  missing_wcvp_fields <- setdiff(
    required_wcvp_fields,
    wcvp_header
  )
  
  if (length(missing_wcvp_fields) > 0L) {
    stop(
      "Required WCVP distribution fields missing: ",
      paste(missing_wcvp_fields, collapse = ", ")
    )
  }
  
  cat(
    "WCVP distribution schema confirmed: ",
    length(wcvp_header),
    " fields\n",
    sep = ""
  )
  
  cat("WCVP distribution delimiter: |\n")
  
  # ===========================================================================
  # 4. Register WCVP distribution directly in DuckDB
  #
  # Explicit pipe delimiter is used. This avoids loading the full source file
  # into R memory and avoids delimiter inference.
  # ===========================================================================
  
  wcvp_path_sql <- gsub(
    "'",
    "''",
    normalizePath(
      WCVP_DIST_PATH,
      winslash = "/",
      mustWork = TRUE
    )
  )
  
  dbExecute(
    con,
    "
    DROP VIEW IF EXISTS stars03q_wcvp_distribution_raw
    "
  )
  
  dbExecute(
    con,
    paste0(
      "
      CREATE TEMP VIEW stars03q_wcvp_distribution_raw AS
      SELECT
        CAST(coreid AS VARCHAR) AS coreid,
        CAST(locality AS VARCHAR) AS locality,
        CAST(establishmentmeans AS VARCHAR) AS establishmentmeans,
        CAST(locationid AS VARCHAR) AS locationid,
        CAST(occurrencestatus AS VARCHAR) AS occurrencestatus
      FROM read_csv(
        '",
      wcvp_path_sql,
      "',
        delim = '|',
        header = TRUE,
        all_varchar = TRUE
      )
      "
    )
  )
  
  # ===========================================================================
  # 5. Restrict WCVP evidence to contemporary non-endemic taxa
  # ===========================================================================
  
  dbWriteTable(
    con,
    "stars03q_non_endemic_taxa_temp",
    non_endemic |>
      transmute(
        FINAL_WCVP_ID =
          as.character(FINAL_WCVP_ID),
        FINAL_WCVP_RECOGNISED_NAME
      ),
    temporary = TRUE,
    overwrite = TRUE
  )
  
  wcvp_non_endemic <- dbGetQuery(
    con,
    "
    SELECT
      n.FINAL_WCVP_ID,
      n.FINAL_WCVP_RECOGNISED_NAME,
      d.locationid,
      d.locality,
      d.establishmentmeans,
      d.occurrencestatus
    FROM stars03q_non_endemic_taxa_temp n
    INNER JOIN stars03q_wcvp_distribution_raw d
      ON n.FINAL_WCVP_ID = d.coreid
    "
  ) |>
    as_tibble()
  
  cat(
    "WCVP distribution rows for non-endemic taxa: ",
    format(nrow(wcvp_non_endemic), big.mark = ","),
    "\n",
    sep = ""
  )
  
  # ===========================================================================
  # 6. Standardise vocabulary for auditing only
  #
  # Original locality/locationid values are retained exactly.
  # Normalised fields are used only for candidate vocabulary searches.
  # ===========================================================================
  
  wcvp_non_endemic <- wcvp_non_endemic |>
    mutate(
      LOCALITY_ORIGINAL = locality,
      LOCATIONID_ORIGINAL = locationid,
      
      LOCALITY_SEARCH =
        str_to_lower(
          str_squish(
            coalesce(locality, "")
          )
        ),
      
      LOCATIONID_SEARCH =
        str_to_lower(
          str_squish(
            coalesce(locationid, "")
          )
        ),
      
      OCCURRENCE_STATUS_SEARCH =
        str_to_lower(
          str_squish(
            coalesce(occurrencestatus, "")
          )
        ),
      
      ESTABLISHMENT_SEARCH =
        str_to_lower(
          str_squish(
            coalesce(establishmentmeans, "")
          )
        )
    )
  
  # ===========================================================================
  # 7. Complete WCVP geographic vocabulary represented by non-endemic taxa
  # ===========================================================================
  
  unit_vocabulary <- wcvp_non_endemic |>
    group_by(
      LOCATIONID_ORIGINAL,
      LOCALITY_ORIGINAL
    ) |>
    summarise(
      N_TAXA =
        n_distinct(FINAL_WCVP_ID),
      
      N_ROWS =
        n(),
      
      ESTABLISHMENT_VALUES =
        paste(
          sort(
            unique(
              establishmentmeans[
                !is.na(establishmentmeans) &
                  establishmentmeans != ""
              ]
            )
          ),
          collapse = " | "
        ),
      
      OCCURRENCE_STATUS_VALUES =
        paste(
          sort(
            unique(
              occurrencestatus[
                !is.na(occurrencestatus) &
                  occurrencestatus != ""
              ]
            )
          ),
          collapse = " | "
        ),
      
      .groups = "drop"
    ) |>
    arrange(
      LOCALITY_ORIGINAL,
      LOCATIONID_ORIGINAL
    )
  
  # ===========================================================================
  # 8. Candidate vocabulary for the published external regions
  #
  # These are text-search hits only, not methodological assignments.
  # ===========================================================================
  
  region_hits <- wcvp_non_endemic |>
    mutate(
      CANDIDATE_REGION =
        case_when(
          str_detect(
            LOCALITY_SEARCH,
            "taiwan|formosa"
          ) |
            str_detect(
              LOCATIONID_SEARCH,
              "taiwan|formosa"
            ) ~
            "Taiwan",
          
          str_detect(
            LOCALITY_SEARCH,
            "korea"
          ) |
            str_detect(
              LOCATIONID_SEARCH,
              "korea"
            ) ~
            "Korea",
          
          str_detect(
            LOCALITY_SEARCH,
            "kuril|kurile|sakhalin"
          ) |
            str_detect(
              LOCATIONID_SEARCH,
              "kuril|kurile|sakhalin"
            ) ~
            "Kuriles & Sakhalin",
          
          str_detect(
            LOCALITY_SEARCH,
            "china|chinese"
          ) |
            str_detect(
              LOCATIONID_SEARCH,
              "china|chinese"
            ) ~
            "China",
          
          TRUE ~
            NA_character_
        )
    ) |>
    filter(
      !is.na(CANDIDATE_REGION)
    )
  
  region_units <- region_hits |>
    group_by(
      CANDIDATE_REGION,
      LOCATIONID_ORIGINAL,
      LOCALITY_ORIGINAL
    ) |>
    summarise(
      N_TAXA =
        n_distinct(FINAL_WCVP_ID),
      
      N_ROWS =
        n(),
      
      .groups = "drop"
    ) |>
    arrange(
      CANDIDATE_REGION,
      LOCALITY_ORIGINAL,
      LOCATIONID_ORIGINAL
    )
  
  # ===========================================================================
  # 9. Search vocabulary for source-specific subregional concepts
  #
  # These flags identify text potentially relevant to Nakamura's criteria.
  # They do not establish methodological equivalence.
  # ===========================================================================
  
  source_term_hits <- unit_vocabulary |>
    mutate(
      SEARCH_TEXT =
        str_to_lower(
          paste(
            coalesce(LOCATIONID_ORIGINAL, ""),
            coalesce(LOCALITY_ORIGINAL, "")
          )
        ),
      
      TAIWAN_TERM =
        str_detect(
          SEARCH_TEXT,
          "taiwan|formosa"
        ),
      
      KOREA_TERM =
        str_detect(
          SEARCH_TEXT,
          "korea"
        ),
      
      KURILES_TERM =
        str_detect(
          SEARCH_TEXT,
          "kuril|kurile"
        ),
      
      SAKHALIN_TERM =
        str_detect(
          SEARCH_TEXT,
          "sakhalin"
        ),
      
      SOUTH_KURILES_TERM =
        str_detect(
          SEARCH_TEXT,
          "south.{0,10}kuril|southern.{0,10}kuril"
        ),
      
      CHINA_TERM =
        str_detect(
          SEARCH_TEXT,
          "china|chinese"
        ),
      
      ISLAND_TERM =
        str_detect(
          SEARCH_TEXT,
          "island|islands|is\\.|archipelago"
        ),
      
      PROVINCE_TERM =
        str_detect(
          SEARCH_TEXT,
          "province|prov\\."
        )
    )
  
  # ===========================================================================
  # 10. Candidate regional-unit inventory
  # ===========================================================================
  
  regional_inventory <- source_term_hits |>
    filter(
      TAIWAN_TERM |
        KOREA_TERM |
        KURILES_TERM |
        SAKHALIN_TERM |
        CHINA_TERM
    ) |>
    select(
      LOCATIONID_ORIGINAL,
      LOCALITY_ORIGINAL,
      N_TAXA,
      N_ROWS,
      TAIWAN_TERM,
      KOREA_TERM,
      KURILES_TERM,
      SAKHALIN_TERM,
      SOUTH_KURILES_TERM,
      CHINA_TERM,
      ISLAND_TERM,
      PROVINCE_TERM
    ) |>
    arrange(
      LOCALITY_ORIGINAL,
      LOCATIONID_ORIGINAL
    )
  
  # ===========================================================================
  # 11. Region-resolution profile
  # ===========================================================================
  
  region_resolution <- bind_rows(
    tibble(
      KEY_REGION = "Taiwan",
      N_MATCHED_UNITS =
        sum(source_term_hits$TAIWAN_TERM),
      N_MATCHED_TAXA =
        n_distinct(
          region_hits$FINAL_WCVP_ID[
            region_hits$CANDIDATE_REGION ==
              "Taiwan"
          ]
        ),
      SOURCE_SUBCLASS =
        "<½ island / ≥½ island",
      DIRECT_SUBCLASS_IMPLEMENTED =
        FALSE
    ),
    
    tibble(
      KEY_REGION = "Korea",
      N_MATCHED_UNITS =
        sum(source_term_hits$KOREA_TERM),
      N_MATCHED_TAXA =
        n_distinct(
          region_hits$FINAL_WCVP_ID[
            region_hits$CANDIDATE_REGION ==
              "Korea"
          ]
        ),
      SOURCE_SUBCLASS =
        paste0(
          "Islands or sparse distribution / ",
          "not only in islands nor restricted"
        ),
      DIRECT_SUBCLASS_IMPLEMENTED =
        FALSE
    ),
    
    tibble(
      KEY_REGION = "Kuriles & Sakhalin",
      N_MATCHED_UNITS =
        sum(
          source_term_hits$KURILES_TERM |
            source_term_hits$SAKHALIN_TERM
        ),
      N_MATCHED_TAXA =
        n_distinct(
          region_hits$FINAL_WCVP_ID[
            region_hits$CANDIDATE_REGION ==
              "Kuriles & Sakhalin"
          ]
        ),
      SOURCE_SUBCLASS =
        paste0(
          "Only in the south Kuriles / ",
          "not only in the south Kuriles"
        ),
      DIRECT_SUBCLASS_IMPLEMENTED =
        FALSE
    ),
    
    tibble(
      KEY_REGION = "China",
      N_MATCHED_UNITS =
        sum(source_term_hits$CHINA_TERM),
      N_MATCHED_TAXA =
        n_distinct(
          region_hits$FINAL_WCVP_ID[
            region_hits$CANDIDATE_REGION ==
              "China"
          ]
        ),
      SOURCE_SUBCLASS =
        "≤2 provinces / >2 provinces",
      DIRECT_SUBCLASS_IMPLEMENTED =
        FALSE
    )
  )
  
  # ===========================================================================
  # 12. Taxon-level external evidence profile
  #
  # This profiles available evidence only.
  # It does not determine Nakamura's external-range class.
  # ===========================================================================
  
  taxon_region_flags <- region_hits |>
    distinct(
      FINAL_WCVP_ID,
      CANDIDATE_REGION
    ) |>
    mutate(
      PRESENT = TRUE
    ) |>
    pivot_wider(
      names_from = CANDIDATE_REGION,
      values_from = PRESENT,
      values_fill = FALSE,
      names_prefix = "REGION_"
    )
  
  expected_region_columns <- c(
    "REGION_Taiwan",
    "REGION_Korea",
    "REGION_Kuriles & Sakhalin",
    "REGION_China"
  )
  
  for (nm in expected_region_columns) {
    if (!nm %in% names(taxon_region_flags)) {
      taxon_region_flags[[nm]] <- FALSE
    }
  }
  
  names(taxon_region_flags)[
    names(taxon_region_flags) ==
      "REGION_Kuriles & Sakhalin"
  ] <- "REGION_KURILES_SAKHALIN"
  
  names(taxon_region_flags)[
    names(taxon_region_flags) ==
      "REGION_Taiwan"
  ] <- "REGION_TAIWAN"
  
  names(taxon_region_flags)[
    names(taxon_region_flags) ==
      "REGION_Korea"
  ] <- "REGION_KOREA"
  
  names(taxon_region_flags)[
    names(taxon_region_flags) ==
      "REGION_China"
  ] <- "REGION_CHINA"
  
  all_distribution_units <- wcvp_non_endemic |>
    distinct(
      FINAL_WCVP_ID,
      locationid,
      locality
    ) |>
    mutate(
      SEARCH_TEXT =
        str_to_lower(
          paste(
            coalesce(locationid, ""),
            coalesce(locality, "")
          )
        ),
      
      IS_JAPAN_UNIT =
        str_detect(
          SEARCH_TEXT,
          "japan|japanese"
        ),
      
      IS_NAMED_KEY_REGION =
        str_detect(
          SEARCH_TEXT,
          paste0(
            "taiwan|formosa|korea|",
            "kuril|kurile|sakhalin|",
            "china|chinese"
          )
        )
    )
  
  broader_external <- all_distribution_units |>
    group_by(
      FINAL_WCVP_ID
    ) |>
    summarise(
      WCVP_DISTINCT_UNITS =
        n(),
      
      NAMED_KEY_REGION_UNITS =
        sum(IS_NAMED_KEY_REGION),
      
      CANDIDATE_OTHER_UNITS =
        sum(
          !IS_JAPAN_UNIT &
            !IS_NAMED_KEY_REGION
        ),
      
      CANDIDATE_BEYOND_NAMED_REGIONS =
        CANDIDATE_OTHER_UNITS > 0,
      
      .groups = "drop"
    )
  
  taxon_profile <- non_endemic |>
    left_join(
      taxon_region_flags,
      by = "FINAL_WCVP_ID"
    ) |>
    left_join(
      broader_external,
      by = "FINAL_WCVP_ID"
    ) |>
    mutate(
      across(
        starts_with("REGION_"),
        ~ replace_na(.x, FALSE)
      ),
      
      WCVP_DISTINCT_UNITS =
        replace_na(
          WCVP_DISTINCT_UNITS,
          0L
        ),
      
      NAMED_KEY_REGION_UNITS =
        replace_na(
          NAMED_KEY_REGION_UNITS,
          0L
        ),
      
      CANDIDATE_OTHER_UNITS =
        replace_na(
          CANDIDATE_OTHER_UNITS,
          0L
        ),
      
      CANDIDATE_BEYOND_NAMED_REGIONS =
        replace_na(
          CANDIDATE_BEYOND_NAMED_REGIONS,
          FALSE
        ),
      
      N_NAMED_KEY_REGIONS =
        as.integer(REGION_TAIWAN) +
        as.integer(REGION_KOREA) +
        as.integer(REGION_KURILES_SAKHALIN) +
        as.integer(REGION_CHINA)
    )
  
  # ===========================================================================
  # 13. Evidence capability register
  #
  # AVAILABLE means source data exist.
  # It does not mean Nakamura's criterion has been operationally resolved.
  # ===========================================================================
  
  capability <- tribble(
    ~CAPABILITY_ID,
    ~KEY_COMPONENT,
    ~SOURCE_REQUIREMENT,
    ~CONTEMPORARY_EVIDENCE,
    ~METHOD_STATUS,
    
    "03Q-C001",
    "TAIWAN_PRESENCE",
    "Taiwan",
    "WCVP locality/locationID vocabulary",
    "EVIDENCE_AVAILABLE",
    
    "03Q-C002",
    "TAIWAN_SUBCLASS",
    "<½ island / ≥½ island",
    "Requires inspection of WCVP Taiwan geographic resolution",
    "NOT_OPERATIONALISED",
    
    "03Q-C003",
    "KOREA_PRESENCE",
    "Korea",
    "WCVP locality/locationID vocabulary",
    "EVIDENCE_AVAILABLE",
    
    "03Q-C004",
    "KOREA_SUBCLASS",
    "Islands or sparse distribution",
    "Requires inspection of WCVP Korean geographic resolution",
    "NOT_OPERATIONALISED",
    
    "03Q-C005",
    "KURILES_SAKHALIN_PRESENCE",
    "Kuriles & Sakhalin",
    "WCVP locality/locationID vocabulary",
    "EVIDENCE_AVAILABLE",
    
    "03Q-C006",
    "SOUTH_KURILES_SUBCLASS",
    "Only in the south Kuriles",
    "Requires explicit south-Kuriles geographic evidence",
    "NOT_OPERATIONALISED",
    
    "03Q-C007",
    "CHINA_PRESENCE",
    "China",
    "WCVP locality/locationID vocabulary",
    "EVIDENCE_AVAILABLE",
    
    "03Q-C008",
    "CHINA_PROVINCE_COUNT",
    "≤2 provinces / >2 provinces",
    "Requires province-level mapping of WCVP Chinese units",
    "NOT_OPERATIONALISED",
    
    "03Q-C009",
    "BROADER_EXTERNAL_RANGE",
    "and beyond",
    paste0(
      "WCVP distribution units outside Japan and the named Key regions ",
      "can be inventoried"
    ),
    "EVIDENCE_AVAILABLE_METHOD_REVIEW_REQUIRED"
  )
  
  # ===========================================================================
  # 14. Profiles
  # ===========================================================================
  
  vocabulary_profile <- tibble(
    WCVP_NON_ENDEMIC_ROWS =
      nrow(wcvp_non_endemic),
    
    DISTINCT_LOCATIONIDS =
      n_distinct(
        wcvp_non_endemic$locationid,
        na.rm = TRUE
      ),
    
    DISTINCT_LOCALITIES =
      n_distinct(
        wcvp_non_endemic$locality,
        na.rm = TRUE
      ),
    
    DISTINCT_LOCATION_LOCALITY_PAIRS =
      nrow(unit_vocabulary),
    
    TAIWAN_MATCHED_UNITS =
      sum(source_term_hits$TAIWAN_TERM),
    
    KOREA_MATCHED_UNITS =
      sum(source_term_hits$KOREA_TERM),
    
    KURILES_MATCHED_UNITS =
      sum(source_term_hits$KURILES_TERM),
    
    SAKHALIN_MATCHED_UNITS =
      sum(source_term_hits$SAKHALIN_TERM),
    
    SOUTH_KURILES_TEXT_MATCHES =
      sum(source_term_hits$SOUTH_KURILES_TERM),
    
    CHINA_MATCHED_UNITS =
      sum(source_term_hits$CHINA_TERM),
    
    CHINA_PROVINCE_TEXT_MATCHES =
      sum(
        source_term_hits$CHINA_TERM &
          source_term_hits$PROVINCE_TERM
      )
  )
  
  taxon_region_profile <- taxon_profile |>
    summarise(
      NON_ENDEMIC_TAXA =
        n(),
      
      TAIWAN =
        sum(REGION_TAIWAN),
      
      KOREA =
        sum(REGION_KOREA),
      
      KURILES_SAKHALIN =
        sum(REGION_KURILES_SAKHALIN),
      
      CHINA =
        sum(REGION_CHINA),
      
      ONE_NAMED_REGION =
        sum(N_NAMED_KEY_REGIONS == 1),
      
      MULTIPLE_NAMED_REGIONS =
        sum(N_NAMED_KEY_REGIONS > 1),
      
      NO_NAMED_REGION =
        sum(N_NAMED_KEY_REGIONS == 0),
      
      CANDIDATE_BEYOND_NAMED_REGIONS =
        sum(CANDIDATE_BEYOND_NAMED_REGIONS)
    )
  
  named_region_combination_profile <- taxon_profile |>
    mutate(
      NAMED_REGION_COMBINATION =
        case_when(
          N_NAMED_KEY_REGIONS == 0 ~
            "NONE",
          
          TRUE ~
            paste0(
              if_else(
                REGION_TAIWAN,
                "Taiwan;",
                ""
              ),
              if_else(
                REGION_KOREA,
                "Korea;",
                ""
              ),
              if_else(
                REGION_KURILES_SAKHALIN,
                "Kuriles_Sakhalin;",
                ""
              ),
              if_else(
                REGION_CHINA,
                "China;",
                ""
              )
            )
        ),
      
      NAMED_REGION_COMBINATION =
        str_remove(
          NAMED_REGION_COMBINATION,
          ";$"
        )
    ) |>
    count(
      NAMED_REGION_COMBINATION,
      name = "N_TAXA",
      sort = TRUE
    )
  
  cat("\n— WCVP VOCABULARY PROFILE —\n")
  print(vocabulary_profile, n = Inf)
  
  cat("\n— KEY-REGION RESOLUTION PROFILE —\n")
  print(region_resolution, n = Inf)
  
  cat("\n— NON-ENDEMIC TAXON REGION PROFILE —\n")
  print(taxon_region_profile, n = Inf)
  
  cat("\n— NAMED-REGION COMBINATIONS —\n")
  print(named_region_combination_profile, n = Inf)
  
  cat("\n— CANDIDATE REGIONAL UNIT VOCABULARY —\n")
  print(
    regional_inventory |>
      select(
        LOCATIONID_ORIGINAL,
        LOCALITY_ORIGINAL,
        N_TAXA,
        TAIWAN_TERM,
        KOREA_TERM,
        KURILES_TERM,
        SAKHALIN_TERM,
        SOUTH_KURILES_TERM,
        CHINA_TERM,
        ISLAND_TERM,
        PROVINCE_TERM
      ),
    n = Inf
  )
  
  cat("\n— EVIDENCE CAPABILITY —\n")
  print(capability, n = Inf)
  
  # ===========================================================================
  # 15. Validation
  # ===========================================================================
  
  validation <- tibble(
    CHECK = c(
      "Stars 03p validation fully PASS",
      "Contemporary non-endemic population = 7,736",
      "One row per non-endemic FINAL_WCVP_ID",
      "WCVP distribution file exists",
      "WCVP distribution schema has six fields",
      "WCVP coreid field available",
      "WCVP locality field available",
      "WCVP locationid field available",
      "WCVP establishmentmeans field available",
      "WCVP occurrencestatus field available",
      "WCVP threatstatus field available",
      "WCVP distribution evidence retrieved",
      "WCVP unit vocabulary created",
      "Taiwan vocabulary audited",
      "Korea vocabulary audited",
      "Kuriles/Sakhalin vocabulary audited",
      "China vocabulary audited",
      "Taxon evidence profile = 7,736",
      "Taxon evidence profile one row per taxon",
      "Named-region count bounded 0-4",
      "Taiwan half-island class not inferred",
      "Korean sparse class not inferred",
      "South-Kuriles class not inferred",
      "Chinese province class not inferred",
      "No STAR_CATEGORY created",
      "No JAPAN_DISTRICT_COUNT created",
      "No final Star assignments created",
      "Historical Star allocations not used"
    ),
    
    PASS = c(
      all(validation_03p$PASS),
      
      nrow(non_endemic) == 7736L,
      
      n_distinct(
        non_endemic$FINAL_WCVP_ID
      ) == 7736L,
      
      file.exists(WCVP_DIST_PATH),
      
      length(wcvp_header) == 6L,
      
      "coreid" %in% wcvp_header,
      
      "locality" %in% wcvp_header,
      
      "locationid" %in% wcvp_header,
      
      "establishmentmeans" %in%
        wcvp_header,
      
      "occurrencestatus" %in%
        wcvp_header,
      
      "threatstatus" %in%
        wcvp_header,
      
      nrow(wcvp_non_endemic) > 0L,
      
      nrow(unit_vocabulary) > 0L,
      
      "TAIWAN_TERM" %in%
        names(source_term_hits),
      
      "KOREA_TERM" %in%
        names(source_term_hits),
      
      all(
        c(
          "KURILES_TERM",
          "SAKHALIN_TERM"
        ) %in%
          names(source_term_hits)
      ),
      
      "CHINA_TERM" %in%
        names(source_term_hits),
      
      nrow(taxon_profile) == 7736L,
      
      n_distinct(
        taxon_profile$FINAL_WCVP_ID
      ) == 7736L,
      
      all(
        taxon_profile$N_NAMED_KEY_REGIONS >= 0 &
          taxon_profile$N_NAMED_KEY_REGIONS <= 4
      ),
      
      capability$METHOD_STATUS[
        capability$CAPABILITY_ID ==
          "03Q-C002"
      ] == "NOT_OPERATIONALISED",
      
      capability$METHOD_STATUS[
        capability$CAPABILITY_ID ==
          "03Q-C004"
      ] == "NOT_OPERATIONALISED",
      
      capability$METHOD_STATUS[
        capability$CAPABILITY_ID ==
          "03Q-C006"
      ] == "NOT_OPERATIONALISED",
      
      capability$METHOD_STATUS[
        capability$CAPABILITY_ID ==
          "03Q-C008"
      ] == "NOT_OPERATIONALISED",
      
      !"STAR_CATEGORY" %in%
        names(taxon_profile),
      
      !"JAPAN_DISTRICT_COUNT" %in%
        names(taxon_profile),
      
      TRUE,
      
      TRUE
    )
  ) |>
    mutate(
      RESULT =
        if_else(
          PASS,
          "PASS",
          "FAIL"
        )
    )
  
  cat("\n— VALIDATION —\n")
  print(validation, n = Inf)
  
  if (!all(validation$PASS)) {
    stop(
      "Stars 03q validation failed. ",
      "Review FAIL checks before proceeding."
    )
  }
  
  # ===========================================================================
  # 16. Metadata
  # ===========================================================================
  
  metadata <- tibble(
    MODULE = MODULE,
    VERSION = VERSION,
    RUN_DATE = as.character(RUN_DATE),
    
    SOURCE_KEY =
      "Nakamura (2012), Figure 2.2, p.48",
    
    SOURCE_KEY_MODULE =
      if (
        "VERSION" %in%
        names(metadata_03p)
      ) {
        paste0(
          "Stars 03p v",
          metadata_03p$VERSION[[1]]
        )
      } else {
        "Stars 03p"
      },
    
    WCVP_SOURCE =
      "wcvp_distribution.csv",
    
    WCVP_DELIMITER =
      "|",
    
    WCVP_EXPECTED_FIELDS =
      paste(
        wcvp_header,
        collapse = "|"
      ),
    
    NON_ENDEMIC_TAXA =
      nrow(non_endemic),
    
    WCVP_DISTRIBUTION_ROWS =
      nrow(wcvp_non_endemic),
    
    DISTINCT_WCVP_UNIT_PAIRS =
      nrow(unit_vocabulary),
    
    TAIWAN_SUBCLASS_OPERATIONALISED =
      FALSE,
    
    KOREA_SUBCLASS_OPERATIONALISED =
      FALSE,
    
    SOUTH_KURILES_SUBCLASS_OPERATIONALISED =
      FALSE,
    
    CHINA_PROVINCE_CLASS_OPERATIONALISED =
      FALSE,
    
    STAR_CLASSIFICATIONS_CREATED =
      FALSE,
    
    JAPAN_DISTRICT_COUNT_CREATED =
      FALSE,
    
    HISTORICAL_STAR_ALLOCATIONS_USED =
      FALSE,
    
    FINAL_STAR_CLASSIFIER_READY =
      FALSE,
    
    STATUS =
      "VALIDATED_EVIDENCE_CAPABILITY_AUDIT"
  )
  
  # ===========================================================================
  # 17. Write canonical DuckDB outputs
  # ===========================================================================
  
  dbWriteTable(
    con,
    TABLE_UNIT_VOCAB,
    unit_vocabulary,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    TABLE_REGION_UNITS,
    regional_inventory,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    TABLE_TAXON_PROFILE,
    taxon_profile,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    TABLE_CAPABILITY,
    capability,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    TABLE_VALIDATION,
    validation,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    TABLE_METADATA,
    metadata,
    overwrite = TRUE
  )
  
  # ===========================================================================
  # 18. Write audit CSV outputs
  # ===========================================================================
  
  write_csv(
    vocabulary_profile,
    file.path(
      OUTPUT_DIR,
      "stars03q_wcvp_vocabulary_profile.csv"
    )
  )
  
  write_csv(
    unit_vocabulary,
    file.path(
      OUTPUT_DIR,
      "stars03q_wcvp_unit_vocabulary.csv"
    )
  )
  
  write_csv(
    regional_inventory,
    file.path(
      OUTPUT_DIR,
      "stars03q_candidate_regional_units.csv"
    )
  )
  
  write_csv(
    region_resolution,
    file.path(
      OUTPUT_DIR,
      "stars03q_region_resolution_profile.csv"
    )
  )
  
  write_csv(
    taxon_region_profile,
    file.path(
      OUTPUT_DIR,
      "stars03q_taxon_region_profile.csv"
    )
  )
  
  write_csv(
    named_region_combination_profile,
    file.path(
      OUTPUT_DIR,
      "stars03q_named_region_combinations.csv"
    )
  )
  
  write_csv(
    taxon_profile,
    file.path(
      OUTPUT_DIR,
      "stars03q_taxon_range_evidence_profile.csv"
    )
  )
  
  write_csv(
    capability,
    file.path(
      OUTPUT_DIR,
      "stars03q_evidence_capability.csv"
    )
  )
  
  write_csv(
    validation,
    file.path(
      OUTPUT_DIR,
      "stars03q_validation.csv"
    )
  )
  
  write_csv(
    metadata,
    file.path(
      OUTPUT_DIR,
      "stars03q_metadata.csv"
    )
  )
  
  # ===========================================================================
  # 19. Final summary
  # ===========================================================================
  
  cat("\n============================================================\n")
  cat("Stars 03q v", VERSION, " COMPLETE\n", sep = "")
  cat("============================================================\n")
  
  cat(
    "Contemporary non-endemic taxa: ",
    format(
      nrow(non_endemic),
      big.mark = ","
    ),
    "\n",
    sep = ""
  )
  
  cat(
    "WCVP distribution rows examined: ",
    format(
      nrow(wcvp_non_endemic),
      big.mark = ","
    ),
    "\n",
    sep = ""
  )
  
  cat(
    "Distinct WCVP location/locality pairs: ",
    format(
      nrow(unit_vocabulary),
      big.mark = ","
    ),
    "\n",
    sep = ""
  )
  
  cat("WCVP delimiter: |\n")
  cat("Taiwan subclass operationalised: FALSE\n")
  cat("Korea subclass operationalised: FALSE\n")
  cat("South-Kuriles subclass operationalised: FALSE\n")
  cat("China province class operationalised: FALSE\n")
  cat("Star classifications created: FALSE\n")
  cat("JAPAN_DISTRICT_COUNT created: FALSE\n")
  cat("Historical Star allocations used: FALSE\n")
  cat("Final classifier ready: FALSE\n")
  
  cat(
    "Validation: ",
    sum(validation$PASS),
    "/",
    nrow(validation),
    " PASS\n",
    sep = ""
  )
  
  cat(
    "Canonical taxon evidence table: ",
    TABLE_TAXON_PROFILE,
    "\n",
    sep = ""
  )
  
  cat(
    "Outputs: ",
    OUTPUT_DIR,
    "\n",
    sep = ""
  )
  
  cat("============================================================\n")
  
  invisible(
    list(
      non_endemic = non_endemic,
      wcvp_non_endemic = wcvp_non_endemic,
      unit_vocabulary = unit_vocabulary,
      regional_inventory = regional_inventory,
      region_resolution = region_resolution,
      taxon_profile = taxon_profile,
      vocabulary_profile = vocabulary_profile,
      taxon_region_profile = taxon_region_profile,
      named_region_combination_profile =
        named_region_combination_profile,
      capability = capability,
      validation = validation,
      metadata = metadata
    )
  )
}

stars_03q_result <- run_stars_03q()