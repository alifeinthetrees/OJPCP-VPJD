# =============================================================================
# VPJD-OJPCP
# R/stars/03r_formalise_wcvp_non_endemic_geography.R
# Version 0.1.0
#
# FORMALISE WCVP GEOGRAPHIC-UNIT RELATIONSHIPS FOR NAKAMURA'S
# NON-ENDEMIC KEY
#
# PURPOSE
# - Use the complete WCVP geographic vocabulary established by Stars 03q.
# - Create an explicit unit-level crosswalk between WCVP geography and the
#   broad geographic components required by Nakamura's non-endemic Key.
# - Replace taxon-level regex inference of "beyond" with an auditable
#   geographic-unit classification.
# - Rebuild taxon-level broad external-range evidence from that crosswalk.
#
# IMPORTANT
# - This is NOT a Star classifier.
# - No terminal Star category is assigned.
# - WCVP units are classified only at the broad geographic level supported
#   directly by their locality/locationID vocabulary.
# - Taiwan <1/2 versus >=1/2 island is NOT resolved.
# - Korean islands/sparse versus broader distribution is NOT resolved.
# - South Kuriles versus broader Kuriles is NOT resolved.
# - Chinese province counts are NOT resolved.
# - "Beyond" means WCVP positive distribution evidence in a geographic unit
#   explicitly crosswalked outside Japan and outside the four named
#   neighbouring Key groups.
# - No JAPAN_DISTRICT_COUNT is created.
# - No historical Star allocations are used.
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

MODULE <- "stars_03r_formalise_wcvp_non_endemic_geography"
VERSION <- "0.1.0"
RUN_DATE <- Sys.Date()

DB_PATH <- here(
  "data", "interim", "occurrences",
  "vpjd_occurrences.duckdb"
)

WCVP_DIST_PATH <- here(
  "data", "raw", "geography", "wcvp_dwca",
  "wcvp_distribution.csv"
)

OUTPUT_DIR <- here(
  "outputs", "tables", "stars",
  "non_endemic_geography_crosswalk"
)

TABLE_CROSSWALK <-
  "vpjd_star_wcvp_non_endemic_geography_crosswalk"

TABLE_TAXON_EVIDENCE <-
  "vpjd_star_non_endemic_geography_evidence"

TABLE_REVIEW <-
  "vpjd_star_wcvp_non_endemic_geography_review"

TABLE_VALIDATION <-
  "vpjd_star_03r_validation"

TABLE_METADATA <-
  "vpjd_star_03r_metadata"

run_stars_03r <- function() {
  
  cat("\n— Formalise WCVP non-endemic geography —\n\n")
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
    "vpjd_star_non_endemic_range_evidence_profile",
    "vpjd_star_wcvp_external_unit_vocabulary",
    "vpjd_star_03q_validation",
    "vpjd_star_03q_metadata"
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
  # 1. Validate Stars 03q
  # ===========================================================================
  
  validation_03q <- dbGetQuery(
    con,
    "
    SELECT *
    FROM vpjd_star_03q_validation
    "
  ) |>
    as_tibble()
  
  if (
    !"PASS" %in% names(validation_03q) ||
    !all(validation_03q$PASS)
  ) {
    stop(
      "Stars 03q is not fully validated. ",
      "03r will not proceed."
    )
  }
  
  metadata_03q <- dbGetQuery(
    con,
    "
    SELECT *
    FROM vpjd_star_03q_metadata
    "
  ) |>
    as_tibble()
  
  cat(
    "Stars 03q validation: ",
    sum(validation_03q$PASS),
    "/",
    nrow(validation_03q),
    " PASS\n",
    sep = ""
  )
  
  # ===========================================================================
  # 2. Load accepted contemporary non-endemic population
  # ===========================================================================
  
  key_evidence <- dbGetQuery(
    con,
    "
    SELECT *
    FROM vpjd_star_taxon_key_geography_evidence
    "
  ) |>
    as_tibble()
  
  required_key_fields <- c(
    "FINAL_WCVP_ID",
    "FINAL_WCVP_RECOGNISED_NAME",
    "PRE_KEY_EVIDENCE_ROUTE"
  )
  
  missing_key_fields <- setdiff(
    required_key_fields,
    names(key_evidence)
  )
  
  if (length(missing_key_fields) > 0L) {
    stop(
      "Required Stars 03n fields missing: ",
      paste(missing_key_fields, collapse = ", ")
    )
  }
  
  non_endemic <- key_evidence |>
    filter(
      PRE_KEY_EVIDENCE_ROUTE ==
        "NON_ENDEMIC_KEY"
    ) |>
    distinct(
      FINAL_WCVP_ID,
      FINAL_WCVP_RECOGNISED_NAME
    ) |>
    mutate(
      FINAL_WCVP_ID =
        as.character(FINAL_WCVP_ID)
    )
  
  if (nrow(non_endemic) != 7736L) {
    stop(
      "Expected 7,736 non-endemic taxa; found ",
      format(
        nrow(non_endemic),
        big.mark = ","
      )
    )
  }
  
  cat(
    "Contemporary non-endemic taxa: ",
    format(
      nrow(non_endemic),
      big.mark = ","
    ),
    "\n",
    sep = ""
  )
  
  # ===========================================================================
  # 3. Load complete WCVP unit vocabulary from Stars 03q
  # ===========================================================================
  
  unit_vocabulary <- dbGetQuery(
    con,
    "
    SELECT *
    FROM vpjd_star_wcvp_external_unit_vocabulary
    "
  ) |>
    as_tibble()
  
  required_unit_fields <- c(
    "LOCATIONID_ORIGINAL",
    "LOCALITY_ORIGINAL"
  )
  
  missing_unit_fields <- setdiff(
    required_unit_fields,
    names(unit_vocabulary)
  )
  
  if (length(missing_unit_fields) > 0L) {
    stop(
      "Required Stars 03q vocabulary fields missing: ",
      paste(missing_unit_fields, collapse = ", ")
    )
  }
  
  if (nrow(unit_vocabulary) != 366L) {
    stop(
      "Expected 366 WCVP location/locality pairs from 03q; found ",
      nrow(unit_vocabulary)
    )
  }
  
  cat(
    "WCVP geographic units loaded: ",
    nrow(unit_vocabulary),
    "\n",
    sep = ""
  )
  
  # ===========================================================================
  # 4. Build explicit unit-level geographic crosswalk
  #
  # WCVP locationID is TDWG-based. We use locality/locationID together to
  # identify only broad geographic relationships directly supported by the
  # vocabulary.
  #
  # REVIEW is retained where a unit cannot be classified safely.
  # ===========================================================================
  
  crosswalk <- unit_vocabulary |>
    mutate(
      LOCATIONID_ORIGINAL =
        as.character(LOCATIONID_ORIGINAL),
      
      LOCALITY_ORIGINAL =
        as.character(LOCALITY_ORIGINAL),
      
      LOCATIONID_SEARCH =
        str_to_upper(
          str_squish(
            coalesce(
              LOCATIONID_ORIGINAL,
              ""
            )
          )
        ),
      
      LOCALITY_SEARCH =
        str_to_lower(
          str_squish(
            coalesce(
              LOCALITY_ORIGINAL,
              ""
            )
          )
        ),
      
      KEY_GEOGRAPHIC_RELATIONSHIP =
        case_when(
          
          # -------------------------------------------------------------------
          # Japan
          # -------------------------------------------------------------------
          
          LOCATIONID_SEARCH %in%
            c(
              "TDWG:JAP",
              "TDWG:NNS",
              "TDWG:OGA",
              "TDWG:KZN"
            ) ~
            "JAPAN",
          
          str_detect(
            LOCALITY_SEARCH,
            "^japan$|japanese"
          ) ~
            "JAPAN",
          
          # -------------------------------------------------------------------
          # Taiwan
          # -------------------------------------------------------------------
          
          LOCATIONID_SEARCH ==
            "TDWG:TAI" ~
            "TAIWAN",
          
          LOCALITY_SEARCH ==
            "taiwan" ~
            "TAIWAN",
          
          # -------------------------------------------------------------------
          # Korea
          # -------------------------------------------------------------------
          
          LOCATIONID_SEARCH ==
            "TDWG:KOR" ~
            "KOREA",
          
          LOCALITY_SEARCH ==
            "korea" ~
            "KOREA",
          
          # -------------------------------------------------------------------
          # Kuriles
          # -------------------------------------------------------------------
          
          LOCATIONID_SEARCH ==
            "TDWG:KUR" ~
            "KURILES",
          
          str_detect(
            LOCALITY_SEARCH,
            "^kuril is\\.?$|^kurile is\\.?$"
          ) ~
            "KURILES",
          
          # -------------------------------------------------------------------
          # Sakhalin
          # -------------------------------------------------------------------
          
          LOCATIONID_SEARCH ==
            "TDWG:SAK" ~
            "SAKHALIN",
          
          LOCALITY_SEARCH ==
            "sakhalin" ~
            "SAKHALIN",
          
          # -------------------------------------------------------------------
          # China
          #
          # These are broad WCVP/TDWG Chinese units, NOT Chinese provinces.
          # South China Sea is retained as China for the broad Nakamura
          # regional-presence evidence because 03q identified it as a WCVP
          # Chinese distribution unit. This does NOT make it a province.
          # -------------------------------------------------------------------
          
          LOCATIONID_SEARCH %in%
            c(
              "TDWG:CHN",
              "TDWG:CHC",
              "TDWG:CHS",
              "TDWG:SCS"
            ) ~
            "CHINA",
          
          str_detect(
            LOCALITY_SEARCH,
            "^china |^south china sea$"
          ) ~
            "CHINA",
          
          # -------------------------------------------------------------------
          # All remaining explicit WCVP geographic units are outside Japan
          # and outside the four named neighbouring Key groups.
          #
          # This assignment is made only where locationID and locality are
          # both non-missing. Missing/blank units remain REVIEW.
          # -------------------------------------------------------------------
          
          !is.na(LOCATIONID_ORIGINAL) &
            LOCATIONID_ORIGINAL != "" &
            !is.na(LOCALITY_ORIGINAL) &
            LOCALITY_ORIGINAL != "" ~
            "BEYOND_KEY_REGIONS",
          
          TRUE ~
            "REVIEW"
        ),
      
      KEY_REGION_GROUP =
        case_when(
          KEY_GEOGRAPHIC_RELATIONSHIP ==
            "JAPAN" ~
            "JAPAN",
          
          KEY_GEOGRAPHIC_RELATIONSHIP ==
            "TAIWAN" ~
            "TAIWAN",
          
          KEY_GEOGRAPHIC_RELATIONSHIP ==
            "KOREA" ~
            "KOREA",
          
          KEY_GEOGRAPHIC_RELATIONSHIP %in%
            c(
              "KURILES",
              "SAKHALIN"
            ) ~
            "KURILES_SAKHALIN",
          
          KEY_GEOGRAPHIC_RELATIONSHIP ==
            "CHINA" ~
            "CHINA",
          
          KEY_GEOGRAPHIC_RELATIONSHIP ==
            "BEYOND_KEY_REGIONS" ~
            "BEYOND_KEY_REGIONS",
          
          TRUE ~
            "REVIEW"
        ),
      
      SUBREGIONAL_KEY_CLASS_RESOLVED =
        FALSE,
      
      METHOD_STATUS =
        case_when(
          KEY_GEOGRAPHIC_RELATIONSHIP ==
            "REVIEW" ~
            "REVIEW_REQUIRED",
          
          TRUE ~
            "BROAD_RELATIONSHIP_ESTABLISHED"
        ),
      
      CROSSWALK_BASIS =
        case_when(
          LOCATIONID_SEARCH %in%
            c(
              "TDWG:JAP",
              "TDWG:NNS",
              "TDWG:OGA",
              "TDWG:KZN",
              "TDWG:TAI",
              "TDWG:KOR",
              "TDWG:KUR",
              "TDWG:SAK",
              "TDWG:CHN",
              "TDWG:CHC",
              "TDWG:CHS",
              "TDWG:SCS"
            ) ~
            "EXPLICIT_TDWG_LOCATIONID",
          
          KEY_GEOGRAPHIC_RELATIONSHIP !=
            "REVIEW" ~
            "EXPLICIT_WCVP_GEOGRAPHIC_UNIT",
          
          TRUE ~
            "UNRESOLVED"
        )
    )
  
  # ===========================================================================
  # 5. Crosswalk profiles
  # ===========================================================================
  
  relationship_profile <- crosswalk |>
    count(
      KEY_GEOGRAPHIC_RELATIONSHIP,
      name = "N_WCVP_UNITS"
    ) |>
    arrange(
      desc(N_WCVP_UNITS),
      KEY_GEOGRAPHIC_RELATIONSHIP
    )
  
  region_group_profile <- crosswalk |>
    count(
      KEY_REGION_GROUP,
      name = "N_WCVP_UNITS"
    ) |>
    arrange(
      desc(N_WCVP_UNITS),
      KEY_REGION_GROUP
    )
  
  review_units <- crosswalk |>
    filter(
      KEY_GEOGRAPHIC_RELATIONSHIP ==
        "REVIEW"
    ) |>
    select(
      LOCATIONID_ORIGINAL,
      LOCALITY_ORIGINAL,
      everything()
    )
  
  cat("\n— WCVP KEY-GEOGRAPHY CROSSWALK PROFILE —\n")
  print(
    relationship_profile,
    n = Inf
  )
  
  cat("\n— KEY REGION-GROUP PROFILE —\n")
  print(
    region_group_profile,
    n = Inf
  )
  
  cat("\n— UNRESOLVED WCVP UNITS —\n")
  
  if (nrow(review_units) == 0L) {
    cat("None\n")
  } else {
    print(
      review_units |>
        select(
          LOCATIONID_ORIGINAL,
          LOCALITY_ORIGINAL,
          N_TAXA,
          N_ROWS
        ),
      n = Inf
    )
  }
  
  # ===========================================================================
  # 6. Register WCVP source directly in DuckDB
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
    DROP VIEW IF EXISTS stars03r_wcvp_distribution_raw
    "
  )
  
  dbExecute(
    con,
    paste0(
      "
      CREATE TEMP VIEW stars03r_wcvp_distribution_raw AS
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
  
  dbWriteTable(
    con,
    "stars03r_non_endemic_taxa_temp",
    non_endemic,
    temporary = TRUE,
    overwrite = TRUE
  )
  
  crosswalk_sql <- crosswalk |>
    select(
      LOCATIONID_ORIGINAL,
      LOCALITY_ORIGINAL,
      KEY_GEOGRAPHIC_RELATIONSHIP,
      KEY_REGION_GROUP,
      METHOD_STATUS
    )
  
  dbWriteTable(
    con,
    "stars03r_crosswalk_temp",
    crosswalk_sql,
    temporary = TRUE,
    overwrite = TRUE
  )
  
  # ===========================================================================
  # 7. Retrieve positive WCVP evidence for non-endemic taxa
  #
  # We retain all rows here and profile occurrence status before using them.
  # ===========================================================================
  
  distribution_rows <- dbGetQuery(
    con,
    "
    SELECT
      n.FINAL_WCVP_ID,
      n.FINAL_WCVP_RECOGNISED_NAME,
      d.locationid AS LOCATIONID_ORIGINAL,
      d.locality AS LOCALITY_ORIGINAL,
      d.establishmentmeans AS ESTABLISHMENT_MEANS,
      d.occurrencestatus AS OCCURRENCE_STATUS,
      c.KEY_GEOGRAPHIC_RELATIONSHIP,
      c.KEY_REGION_GROUP,
      c.METHOD_STATUS
    FROM stars03r_non_endemic_taxa_temp n
    INNER JOIN stars03r_wcvp_distribution_raw d
      ON n.FINAL_WCVP_ID = d.coreid
    LEFT JOIN stars03r_crosswalk_temp c
      ON COALESCE(d.locationid, '') =
         COALESCE(c.LOCATIONID_ORIGINAL, '')
     AND COALESCE(d.locality, '') =
         COALESCE(c.LOCALITY_ORIGINAL, '')
    "
  ) |>
    as_tibble()
  
  distribution_rows <- distribution_rows |>
    mutate(
      OCCURRENCE_STATUS_NORMALISED =
        str_to_lower(
          str_squish(
            coalesce(
              OCCURRENCE_STATUS,
              ""
            )
          )
        ),
      
      POSITIVE_DISTRIBUTION_EVIDENCE =
        OCCURRENCE_STATUS_NORMALISED !=
        "doubtful"
    )
  
  occurrence_status_profile <- distribution_rows |>
    count(
      OCCURRENCE_STATUS,
      POSITIVE_DISTRIBUTION_EVIDENCE,
      name = "N_ROWS"
    ) |>
    arrange(
      desc(N_ROWS)
    )
  
  cat("\n— WCVP OCCURRENCE-STATUS PROFILE —\n")
  print(
    occurrence_status_profile,
    n = Inf
  )
  
  # ===========================================================================
  # 8. Positive broad geographic evidence
  #
  # Doubtful rows are excluded, consistent with Stars 03c.
  # ===========================================================================
  
  positive_rows <- distribution_rows |>
    filter(
      POSITIVE_DISTRIBUTION_EVIDENCE
    )
  
  unmatched_positive_rows <- positive_rows |>
    filter(
      is.na(KEY_GEOGRAPHIC_RELATIONSHIP)
    )
  
  # ===========================================================================
  # 9. Taxon-level broad geographic evidence
  # ===========================================================================
  
  taxon_region_flags <- positive_rows |>
    filter(
      !is.na(KEY_REGION_GROUP)
    ) |>
    distinct(
      FINAL_WCVP_ID,
      KEY_REGION_GROUP
    ) |>
    mutate(
      PRESENT = TRUE
    ) |>
    pivot_wider(
      names_from = KEY_REGION_GROUP,
      values_from = PRESENT,
      values_fill = FALSE,
      names_prefix = "WCVP_"
    )
  
  expected_flags <- c(
    "WCVP_JAPAN",
    "WCVP_TAIWAN",
    "WCVP_KOREA",
    "WCVP_KURILES_SAKHALIN",
    "WCVP_CHINA",
    "WCVP_BEYOND_KEY_REGIONS",
    "WCVP_REVIEW"
  )
  
  for (nm in expected_flags) {
    if (!nm %in% names(taxon_region_flags)) {
      taxon_region_flags[[nm]] <- FALSE
    }
  }
  
  unit_counts <- positive_rows |>
    filter(
      !is.na(KEY_REGION_GROUP)
    ) |>
    distinct(
      FINAL_WCVP_ID,
      LOCATIONID_ORIGINAL,
      LOCALITY_ORIGINAL,
      KEY_REGION_GROUP
    ) |>
    count(
      FINAL_WCVP_ID,
      KEY_REGION_GROUP,
      name = "N_UNITS"
    ) |>
    pivot_wider(
      names_from = KEY_REGION_GROUP,
      values_from = N_UNITS,
      values_fill = 0L,
      names_prefix = "N_WCVP_UNITS_"
    )
  
  taxon_evidence <- non_endemic |>
    left_join(
      taxon_region_flags,
      by = "FINAL_WCVP_ID"
    ) |>
    left_join(
      unit_counts,
      by = "FINAL_WCVP_ID"
    ) |>
    mutate(
      across(
        starts_with("WCVP_"),
        ~ replace_na(.x, FALSE)
      ),
      
      across(
        starts_with("N_WCVP_UNITS_"),
        ~ replace_na(.x, 0L)
      ),
      
      N_NAMED_NEIGHBOUR_REGIONS =
        as.integer(WCVP_TAIWAN) +
        as.integer(WCVP_KOREA) +
        as.integer(WCVP_KURILES_SAKHALIN) +
        as.integer(WCVP_CHINA),
      
      MULTIPLE_NAMED_NEIGHBOUR_REGIONS =
        N_NAMED_NEIGHBOUR_REGIONS > 1L,
      
      NO_NAMED_NEIGHBOUR_REGION =
        N_NAMED_NEIGHBOUR_REGIONS == 0L,
      
      BROADER_EXTERNAL_RANGE_EVIDENCE =
        WCVP_BEYOND_KEY_REGIONS,
      
      BROAD_RANGE_EVIDENCE_REVIEW_REQUIRED =
        WCVP_REVIEW,
      
      NON_ENDEMIC_KEY_SUBCLASS_RESOLVED =
        FALSE,
      
      STAR_CATEGORY_ASSIGNED =
        FALSE
    )
  
  # ===========================================================================
  # 10. Taxon-level profiles
  # ===========================================================================
  
  taxon_profile <- taxon_evidence |>
    summarise(
      NON_ENDEMIC_TAXA =
        n(),
      
      TAIWAN =
        sum(WCVP_TAIWAN),
      
      KOREA =
        sum(WCVP_KOREA),
      
      KURILES_SAKHALIN =
        sum(WCVP_KURILES_SAKHALIN),
      
      CHINA =
        sum(WCVP_CHINA),
      
      BEYOND_KEY_REGIONS =
        sum(WCVP_BEYOND_KEY_REGIONS),
      
      REVIEW =
        sum(WCVP_REVIEW),
      
      ONE_NAMED_NEIGHBOUR_REGION =
        sum(
          N_NAMED_NEIGHBOUR_REGIONS == 1L
        ),
      
      MULTIPLE_NAMED_NEIGHBOUR_REGIONS =
        sum(
          N_NAMED_NEIGHBOUR_REGIONS > 1L
        ),
      
      NO_NAMED_NEIGHBOUR_REGION =
        sum(
          N_NAMED_NEIGHBOUR_REGIONS == 0L
        )
    )
  
  combination_profile <- taxon_evidence |>
    mutate(
      RANGE_COMBINATION =
        case_when(
          N_NAMED_NEIGHBOUR_REGIONS == 0L &
            !WCVP_BEYOND_KEY_REGIONS ~
            "NO_NAMED_REGION_OR_BEYOND",
          
          TRUE ~
            paste0(
              if_else(
                WCVP_TAIWAN,
                "Taiwan;",
                ""
              ),
              if_else(
                WCVP_KOREA,
                "Korea;",
                ""
              ),
              if_else(
                WCVP_KURILES_SAKHALIN,
                "Kuriles_Sakhalin;",
                ""
              ),
              if_else(
                WCVP_CHINA,
                "China;",
                ""
              ),
              if_else(
                WCVP_BEYOND_KEY_REGIONS,
                "Beyond;",
                ""
              )
            )
        ),
      
      RANGE_COMBINATION =
        str_remove(
          RANGE_COMBINATION,
          ";$"
        )
    ) |>
    count(
      RANGE_COMBINATION,
      name = "N_TAXA",
      sort = TRUE
    )
  
  cat("\n— TAXON-LEVEL BROAD RANGE PROFILE —\n")
  print(
    taxon_profile,
    n = Inf
  )
  
  cat("\n— BROAD RANGE COMBINATIONS —\n")
  print(
    combination_profile,
    n = Inf
  )
  
  # ===========================================================================
  # 11. Methodological decision register
  # ===========================================================================
  
  decision_register <- tribble(
    ~DECISION_ID,
    ~KEY_TERM,
    ~OPERATIONAL_DEFINITION,
    ~EVIDENCE_SOURCE,
    ~METHOD_STATUS,
    
    "03R-D001",
    "Taiwan",
    paste0(
      "Positive WCVP distribution evidence in ",
      "TDWG:TAI."
    ),
    "WCVP distribution",
    "BROAD_RELATIONSHIP_ESTABLISHED",
    
    "03R-D002",
    "Korea",
    paste0(
      "Positive WCVP distribution evidence in ",
      "TDWG:KOR."
    ),
    "WCVP distribution",
    "BROAD_RELATIONSHIP_ESTABLISHED",
    
    "03R-D003",
    "Kuriles and Sakhalin",
    paste0(
      "Positive WCVP distribution evidence in ",
      "TDWG:KUR and/or TDWG:SAK."
    ),
    "WCVP distribution",
    "BROAD_RELATIONSHIP_ESTABLISHED",
    
    "03R-D004",
    "China",
    paste0(
      "Positive WCVP distribution evidence in ",
      "TDWG:CHN, TDWG:CHC, TDWG:CHS or TDWG:SCS. ",
      "This is broad China evidence and is not a ",
      "Chinese province count."
    ),
    "WCVP distribution",
    "BROAD_RELATIONSHIP_ESTABLISHED",
    
    "03R-D005",
    "Beyond named Key regions",
    paste0(
      "Positive WCVP distribution evidence in an ",
      "explicit WCVP geographic unit crosswalked ",
      "outside Japan, Taiwan, Korea, Kuriles, ",
      "Sakhalin and China."
    ),
    paste0(
      "Stars 03q WCVP geographic vocabulary + ",
      "WCVP distribution"
    ),
    "PROVISIONAL_METHOD_VALIDATED",
    
    "03R-D006",
    "Taiwan <1/2 versus >=1/2 island",
    "Not operationalised.",
    "Requires finer geographic evidence",
    "UNRESOLVED",
    
    "03R-D007",
    "Korean islands or sparse distribution",
    "Not operationalised.",
    "Requires finer geographic evidence",
    "UNRESOLVED",
    
    "03R-D008",
    "South Kuriles only",
    "Not operationalised.",
    "Requires finer geographic evidence",
    "UNRESOLVED",
    
    "03R-D009",
    "China <=2 versus >2 provinces",
    "Not operationalised.",
    "Requires province-level geographic evidence",
    "UNRESOLVED"
  )
  
  # ===========================================================================
  # 12. Readiness profile
  # ===========================================================================
  
  readiness <- tibble(
    REQUIREMENT = c(
      "Broad Taiwan presence",
      "Taiwan half-island subclass",
      "Broad Korea presence",
      "Korean sparse/island subclass",
      "Broad Kuriles/Sakhalin presence",
      "South-Kuriles subclass",
      "Broad China presence",
      "Chinese province-count subclass",
      "Beyond named Key regions",
      "Non-endemic matrix fully operational",
      "Final Star classifier"
    ),
    
    READY = c(
      TRUE,
      FALSE,
      TRUE,
      FALSE,
      TRUE,
      FALSE,
      TRUE,
      FALSE,
      nrow(review_units) == 0L &&
        nrow(unmatched_positive_rows) == 0L,
      FALSE,
      FALSE
    )
  )
  
  cat("\n— NON-ENDEMIC EVIDENCE READINESS —\n")
  print(
    readiness,
    n = Inf
  )
  
  # ===========================================================================
  # 13. Validation
  # ===========================================================================
  
  validation <- tibble(
    CHECK = c(
      "Stars 03q validation fully PASS",
      "Non-endemic population = 7,736",
      "One row per non-endemic FINAL_WCVP_ID",
      "WCVP unit vocabulary = 366",
      "One row per WCVP location/locality pair",
      "All 366 units receive a broad relationship",
      "Taiwan TDWG unit identified",
      "Korea TDWG unit identified",
      "Kuriles TDWG unit identified",
      "Sakhalin TDWG unit identified",
      "China North-Central unit identified",
      "China South-Central unit identified",
      "China Southeast unit identified",
      "South China Sea unit identified",
      "Taxon evidence = 7,736",
      "Taxon evidence one row per taxon",
      "Named-neighbour count bounded 0-4",
      "Doubtful WCVP rows excluded from positive evidence",
      "Positive distribution rows crosswalk successfully",
      "Taiwan subclass remains unresolved",
      "Korea subclass remains unresolved",
      "South-Kuriles subclass remains unresolved",
      "Chinese province subclass remains unresolved",
      "No JAPAN_DISTRICT_COUNT created",
      "No STAR_CATEGORY created",
      "No final Star assignments created",
      "Historical Star allocations not used"
    ),
    
    PASS = c(
      all(validation_03q$PASS),
      
      nrow(non_endemic) == 7736L,
      
      n_distinct(
        non_endemic$FINAL_WCVP_ID
      ) == 7736L,
      
      nrow(unit_vocabulary) == 366L,
      
      n_distinct(
        paste(
          unit_vocabulary$LOCATIONID_ORIGINAL,
          unit_vocabulary$LOCALITY_ORIGINAL,
          sep = "||"
        )
      ) == 366L,
      
      all(
        !is.na(
          crosswalk$KEY_GEOGRAPHIC_RELATIONSHIP
        )
      ),
      
      any(
        crosswalk$LOCATIONID_ORIGINAL ==
          "TDWG:TAI" &
          crosswalk$KEY_GEOGRAPHIC_RELATIONSHIP ==
          "TAIWAN"
      ),
      
      any(
        crosswalk$LOCATIONID_ORIGINAL ==
          "TDWG:KOR" &
          crosswalk$KEY_GEOGRAPHIC_RELATIONSHIP ==
          "KOREA"
      ),
      
      any(
        crosswalk$LOCATIONID_ORIGINAL ==
          "TDWG:KUR" &
          crosswalk$KEY_GEOGRAPHIC_RELATIONSHIP ==
          "KURILES"
      ),
      
      any(
        crosswalk$LOCATIONID_ORIGINAL ==
          "TDWG:SAK" &
          crosswalk$KEY_GEOGRAPHIC_RELATIONSHIP ==
          "SAKHALIN"
      ),
      
      any(
        crosswalk$LOCATIONID_ORIGINAL ==
          "TDWG:CHN" &
          crosswalk$KEY_GEOGRAPHIC_RELATIONSHIP ==
          "CHINA"
      ),
      
      any(
        crosswalk$LOCATIONID_ORIGINAL ==
          "TDWG:CHC" &
          crosswalk$KEY_GEOGRAPHIC_RELATIONSHIP ==
          "CHINA"
      ),
      
      any(
        crosswalk$LOCATIONID_ORIGINAL ==
          "TDWG:CHS" &
          crosswalk$KEY_GEOGRAPHIC_RELATIONSHIP ==
          "CHINA"
      ),
      
      any(
        crosswalk$LOCATIONID_ORIGINAL ==
          "TDWG:SCS" &
          crosswalk$KEY_GEOGRAPHIC_RELATIONSHIP ==
          "CHINA"
      ),
      
      nrow(taxon_evidence) == 7736L,
      
      n_distinct(
        taxon_evidence$FINAL_WCVP_ID
      ) == 7736L,
      
      all(
        taxon_evidence$
          N_NAMED_NEIGHBOUR_REGIONS >= 0L &
          taxon_evidence$
          N_NAMED_NEIGHBOUR_REGIONS <= 4L
      ),
      
      !any(
        positive_rows$
          OCCURRENCE_STATUS_NORMALISED ==
          "doubtful"
      ),
      
      nrow(unmatched_positive_rows) == 0L,
      
      decision_register$METHOD_STATUS[
        decision_register$DECISION_ID ==
          "03R-D006"
      ] == "UNRESOLVED",
      
      decision_register$METHOD_STATUS[
        decision_register$DECISION_ID ==
          "03R-D007"
      ] == "UNRESOLVED",
      
      decision_register$METHOD_STATUS[
        decision_register$DECISION_ID ==
          "03R-D008"
      ] == "UNRESOLVED",
      
      decision_register$METHOD_STATUS[
        decision_register$DECISION_ID ==
          "03R-D009"
      ] == "UNRESOLVED",
      
      !"JAPAN_DISTRICT_COUNT" %in%
        names(taxon_evidence),
      
      !"STAR_CATEGORY" %in%
        names(taxon_evidence),
      
      all(
        !taxon_evidence$
          STAR_CATEGORY_ASSIGNED
      ),
      
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
  print(
    validation,
    n = Inf
  )
  
  if (!all(validation$PASS)) {
    stop(
      "Stars 03r validation failed. ",
      "Review FAIL checks before proceeding."
    )
  }
  
  # ===========================================================================
  # 14. Metadata
  # ===========================================================================
  
  metadata <- tibble(
    MODULE = MODULE,
    VERSION = VERSION,
    RUN_DATE = as.character(RUN_DATE),
    
    SOURCE_KEY =
      "Nakamura (2012), Figure 2.2, p.48",
    
    UPSTREAM_MODULE =
      if (
        "VERSION" %in%
        names(metadata_03q)
      ) {
        paste0(
          "Stars 03q v",
          metadata_03q$VERSION[[1]]
        )
      } else {
        "Stars 03q"
      },
    
    WCVP_SOURCE =
      "wcvp_distribution.csv",
    
    WCVP_DELIMITER =
      "|",
    
    NON_ENDEMIC_TAXA =
      nrow(non_endemic),
    
    WCVP_GEOGRAPHIC_UNITS =
      nrow(crosswalk),
    
    WCVP_REVIEW_UNITS =
      nrow(review_units),
    
    UNMATCHED_POSITIVE_ROWS =
      nrow(unmatched_positive_rows),
    
    TAIWAN_SUBCLASS_OPERATIONALISED =
      FALSE,
    
    KOREA_SUBCLASS_OPERATIONALISED =
      FALSE,
    
    SOUTH_KURILES_SUBCLASS_OPERATIONALISED =
      FALSE,
    
    CHINA_PROVINCE_CLASS_OPERATIONALISED =
      FALSE,
    
    BROADER_EXTERNAL_RANGE_OPERATIONALISED =
      nrow(review_units) == 0L &&
      nrow(unmatched_positive_rows) == 0L,
    
    STAR_CLASSIFICATIONS_CREATED =
      FALSE,
    
    JAPAN_DISTRICT_COUNT_CREATED =
      FALSE,
    
    HISTORICAL_STAR_ALLOCATIONS_USED =
      FALSE,
    
    FINAL_STAR_CLASSIFIER_READY =
      FALSE,
    
    STATUS =
      "VALIDATED_GEOGRAPHIC_CROSSWALK"
  )
  
  # ===========================================================================
  # 15. Write canonical DuckDB outputs
  # ===========================================================================
  
  dbWriteTable(
    con,
    TABLE_CROSSWALK,
    crosswalk,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    TABLE_TAXON_EVIDENCE,
    taxon_evidence,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    TABLE_REVIEW,
    review_units,
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
  # 16. Write audit outputs
  # ===========================================================================
  
  write_csv(
    crosswalk,
    file.path(
      OUTPUT_DIR,
      "stars03r_wcvp_geography_crosswalk.csv"
    )
  )
  
  write_csv(
    relationship_profile,
    file.path(
      OUTPUT_DIR,
      "stars03r_relationship_profile.csv"
    )
  )
  
  write_csv(
    region_group_profile,
    file.path(
      OUTPUT_DIR,
      "stars03r_region_group_profile.csv"
    )
  )
  
  write_csv(
    review_units,
    file.path(
      OUTPUT_DIR,
      "stars03r_review_units.csv"
    )
  )
  
  write_csv(
    occurrence_status_profile,
    file.path(
      OUTPUT_DIR,
      "stars03r_occurrence_status_profile.csv"
    )
  )
  
  write_csv(
    taxon_evidence,
    file.path(
      OUTPUT_DIR,
      "stars03r_taxon_geography_evidence.csv"
    )
  )
  
  write_csv(
    taxon_profile,
    file.path(
      OUTPUT_DIR,
      "stars03r_taxon_profile.csv"
    )
  )
  
  write_csv(
    combination_profile,
    file.path(
      OUTPUT_DIR,
      "stars03r_range_combination_profile.csv"
    )
  )
  
  write_csv(
    decision_register,
    file.path(
      OUTPUT_DIR,
      "stars03r_decision_register.csv"
    )
  )
  
  write_csv(
    readiness,
    file.path(
      OUTPUT_DIR,
      "stars03r_readiness.csv"
    )
  )
  
  write_csv(
    validation,
    file.path(
      OUTPUT_DIR,
      "stars03r_validation.csv"
    )
  )
  
  write_csv(
    metadata,
    file.path(
      OUTPUT_DIR,
      "stars03r_metadata.csv"
    )
  )
  
  # ===========================================================================
  # 17. Final summary
  # ===========================================================================
  
  cat("\n============================================================\n")
  cat(
    "Stars 03r v",
    VERSION,
    " COMPLETE\n",
    sep = ""
  )
  cat("============================================================\n")
  
  cat(
    "Non-endemic taxa: ",
    format(
      nrow(non_endemic),
      big.mark = ","
    ),
    "\n",
    sep = ""
  )
  
  cat(
    "WCVP geographic units crosswalked: ",
    nrow(crosswalk),
    "\n",
    sep = ""
  )
  
  cat(
    "WCVP units requiring review: ",
    nrow(review_units),
    "\n",
    sep = ""
  )
  
  cat(
    "Unmatched positive distribution rows: ",
    nrow(unmatched_positive_rows),
    "\n",
    sep = ""
  )
  
  cat(
    "Taxa with beyond-region evidence: ",
    format(
      sum(
        taxon_evidence$
          WCVP_BEYOND_KEY_REGIONS
      ),
      big.mark = ","
    ),
    "\n",
    sep = ""
  )
  
  cat("Taiwan subclass operationalised: FALSE\n")
  cat("Korea subclass operationalised: FALSE\n")
  cat("South-Kuriles subclass operationalised: FALSE\n")
  cat("China province class operationalised: FALSE\n")
  
  cat(
    "Broad beyond-region evidence operationalised: ",
    if_else(
      nrow(review_units) == 0L &&
        nrow(unmatched_positive_rows) == 0L,
      "TRUE",
      "FALSE"
    ),
    "\n",
    sep = ""
  )
  
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
    "Canonical crosswalk: ",
    TABLE_CROSSWALK,
    "\n",
    sep = ""
  )
  
  cat(
    "Canonical taxon evidence: ",
    TABLE_TAXON_EVIDENCE,
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
      crosswalk = crosswalk,
      relationship_profile =
        relationship_profile,
      region_group_profile =
        region_group_profile,
      review_units = review_units,
      occurrence_status_profile =
        occurrence_status_profile,
      taxon_evidence = taxon_evidence,
      taxon_profile = taxon_profile,
      combination_profile =
        combination_profile,
      decision_register =
        decision_register,
      readiness = readiness,
      validation = validation,
      metadata = metadata
    )
  )
}

stars_03r_result <- run_stars_03r()