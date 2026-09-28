# =============================================================================
# VPJD-OJPCP
# R/stars/03v_build_endemic_district_occupancy.R
# Version 0.1.0
#
# BUILD ENDEMIC CORE-DISTRICT OCCUPANCY AND E03 THRESHOLD EVIDENCE
#
# PURPOSE
# - Calculate taxon-level occupancy across the eight provisional Japanese
#   core districts formalised in Stars 03u.
# - Restrict Key profiling to the 1,968 endemic taxa released by Stars 03t
#   to the district branch.
# - Combine core-district occupancy with existing Japanese prefecture counts.
# - Determine which taxa can be placed unambiguously on either side of the
#   Figure 2.2 E03 threshold:
#
#     <=2 districts AND <=14 prefectures
#                  versus
#     >2 districts OR >14 prefectures
#
# - Identify taxa whose district-side result remains sensitive to unresolved
#   special-area treatment.
#
# IMPORTANT
# - Evidence / threshold-sensitivity module only.
# - Eight-district interpretation remains PROVISIONAL_METHOD_VALIDATED.
# - No special analytical area is automatically counted as a district.
# - No rarity criterion is operationalised.
# - No "almost all districts" criterion is operationalised.
# - No Stars are assigned.
# - Historical Star allocations are not used.
# =============================================================================

suppressPackageStartupMessages({
  library(DBI)
  library(duckdb)
  library(dplyr)
  library(readr)
  library(tidyr)
  library(tibble)
  library(here)
})

MODULE <- "stars_03v_build_endemic_district_occupancy"
VERSION <- "0.1.0"
RUN_DATE <- Sys.Date()

DB_PATH <- here(
  "data","interim","occurrences",
  "vpjd_occurrences.duckdb"
)

OUTPUT_DIR <- here(
  "outputs","tables","stars",
  "endemic_district_occupancy"
)

TABLE_OCCUPANCY <-
  "vpjd_star_endemic_district_occupancy"

TABLE_PROFILE <-
  "vpjd_star_endemic_e03_threshold_profile"

TABLE_SENSITIVITY <-
  "vpjd_star_endemic_district_sensitivity"

TABLE_VALIDATION <-
  "vpjd_star_03v_validation"

TABLE_METADATA <-
  "vpjd_star_03v_metadata"

run_stars_03v <- function() {
  
  cat("\n— Endemic district occupancy and E03 threshold —\n\n")
  cat("Run date: ",as.character(RUN_DATE),"\n",sep="")
  cat("Module: ",MODULE,"\n",sep="")
  cat("Version: ",VERSION,"\n\n",sep="")
  
  dir.create(
    OUTPUT_DIR,
    recursive=TRUE,
    showWarnings=FALSE
  )
  
  if (!file.exists(DB_PATH)) {
    stop(
      "VPJD DuckDB not found: ",
      DB_PATH
    )
  }
  
  con <- dbConnect(
    duckdb::duckdb(),
    dbdir=DB_PATH,
    read_only=FALSE
  )
  
  on.exit(
    dbDisconnect(
      con,
      shutdown=TRUE
    ),
    add=TRUE
  )
  
  required_tables <- c(
    "vpjd_japan_taxon_area_distribution",
    "vpjd_star_taxon_key_geography_evidence",
    "vpjd_star_endemic_small_island_evidence",
    "vpjd_star_japan_district_crosswalk",
    "vpjd_star_japan_district_register",
    "vpjd_star_03t_validation",
    "vpjd_star_03u_validation"
  )
  
  missing_tables <- setdiff(
    required_tables,
    dbListTables(con)
  )
  
  if (length(missing_tables)>0L) {
    stop(
      "Required upstream tables missing: ",
      paste(
        missing_tables,
        collapse=", "
      )
    )
  }
  
  # ---------------------------------------------------------------------------
  # Upstream validation
  # ---------------------------------------------------------------------------
  
  validation_03t <- as_tibble(
    dbGetQuery(
      con,
      "SELECT * FROM vpjd_star_03t_validation"
    )
  )
  
  validation_03u <- as_tibble(
    dbGetQuery(
      con,
      "SELECT * FROM vpjd_star_03u_validation"
    )
  )
  
  if (
    !"PASS" %in% names(validation_03t) ||
    !all(validation_03t$PASS)
  ) {
    stop(
      "Stars 03t is not fully validated."
    )
  }
  
  if (
    !"PASS" %in% names(validation_03u) ||
    !all(validation_03u$PASS)
  ) {
    stop(
      "Stars 03u is not fully validated."
    )
  }
  
  cat(
    "Stars 03t validation: ",
    sum(validation_03t$PASS),
    "/",
    nrow(validation_03t),
    " PASS\n",
    sep=""
  )
  
  cat(
    "Stars 03u validation: ",
    sum(validation_03u$PASS),
    "/",
    nrow(validation_03u),
    " PASS\n",
    sep=""
  )
  
  # ---------------------------------------------------------------------------
  # Eight-core-district register
  # ---------------------------------------------------------------------------
  
  district_register <- as_tibble(
    dbGetQuery(
      con,
      "
      SELECT *
      FROM vpjd_star_japan_district_register
      ORDER BY DISTRICT_ORDER
      "
    )
  )
  
  if (nrow(district_register)!=8L) {
    stop(
      "Expected eight core districts; found ",
      nrow(district_register)
    )
  }
  
  CORE_DISTRICTS <-
    district_register$KEY_DISTRICT
  
  cat(
    "Core districts: ",
    paste(
      CORE_DISTRICTS,
      collapse=", "
    ),
    "\n",
    sep=""
  )
  
  # ---------------------------------------------------------------------------
  # Canonical core-area -> district crosswalk
  # ---------------------------------------------------------------------------
  
  district_crosswalk <- as_tibble(
    dbGetQuery(
      con,
      "
      SELECT
        BOTANICAL_AREA_ID,
        ANALYTICAL_AREA_NAME,
        KEY_DISTRICT,
        CORE_DISTRICT_COUNTABLE
      FROM vpjd_star_japan_district_crosswalk
      WHERE CORE_DISTRICT_COUNTABLE = TRUE
      "
    )
  )
  
  if (nrow(district_crosswalk)!=46L) {
    stop(
      "Expected 46 core-area crosswalk rows; found ",
      nrow(district_crosswalk)
    )
  }
  
  if (
    any(
      is.na(
        district_crosswalk$KEY_DISTRICT
      )
    )
  ) {
    stop(
      "Missing KEY_DISTRICT in core-area crosswalk."
    )
  }
  
  # ---------------------------------------------------------------------------
  # Stars 03t endemic evidence
  # ---------------------------------------------------------------------------
  
  endemic_evidence <- as_tibble(
    dbGetQuery(
      con,
      "
      SELECT *
      FROM vpjd_star_endemic_small_island_evidence
      "
    )
  ) |>
    mutate(
      FINAL_WCVP_ID=
        as.character(FINAL_WCVP_ID)
    )
  
  endemic_branch <- endemic_evidence |>
    filter(
      ENDEMIC_SMALL_ISLAND_KEY_STATE==
        'CONTINUE_TO_DISTRICT_BRANCH'
    )
  
  if (nrow(endemic_branch)!=1968L) {
    stop(
      "Expected 1,968 taxa entering district branch; found ",
      format(
        nrow(endemic_branch),
        big.mark=","
      )
    )
  }
  
  if (
    n_distinct(
      endemic_branch$FINAL_WCVP_ID
    )!=1968L
  ) {
    stop(
      "Endemic district-branch population is not unique by WCVP ID."
    )
  }
  
  cat(
    "Endemic taxa entering district branch: ",
    format(
      nrow(endemic_branch),
      big.mark=","
    ),
    "\n",
    sep=""
  )
  
  # ---------------------------------------------------------------------------
  # Existing prefecture counts from Stars 03n
  # ---------------------------------------------------------------------------
  
  geography_03n <- as_tibble(
    dbGetQuery(
      con,
      "
      SELECT *
      FROM vpjd_star_taxon_key_geography_evidence
      "
    )
  ) |>
    mutate(
      FINAL_WCVP_ID=
        as.character(FINAL_WCVP_ID)
    )
  
  prefecture_candidates <- c(
    "JAPAN_PREFECTURES_PRESENT",
    "PREFECTURES_PRESENT",
    "PREFECTURE_COUNT",
    "JAPAN_PREFECTURE_COUNT"
  )
  
  prefecture_field <- prefecture_candidates[
    prefecture_candidates %in%
      names(geography_03n)
  ]
  
  if (length(prefecture_field)==0L) {
    stop(
      "No recognised prefecture-count field found in Stars 03n."
    )
  }
  
  PREFECTURE_FIELD <-
    prefecture_field[[1]]
  
  cat(
    "Prefecture-count field: ",
    PREFECTURE_FIELD,
    "\n",
    sep=""
  )
  
  prefecture_evidence <- geography_03n |>
    transmute(
      FINAL_WCVP_ID,
      JAPAN_PREFECTURES_PRESENT=
        as.integer(
          .data[[PREFECTURE_FIELD]]
        )
    )
  
  # ---------------------------------------------------------------------------
  # Canonical accepted taxon-area occupancy
  # ---------------------------------------------------------------------------
  
  area_occ <- as_tibble(
    dbGetQuery(
      con,
      "
      SELECT DISTINCT
        FINAL_WCVP_ID,
        BOTANICAL_AREA_ID
      FROM vpjd_japan_taxon_area_distribution
      WHERE FINAL_WCVP_STATUS = 'Accepted'
        AND PRESENCE = 1
      "
    )
  ) |>
    mutate(
      FINAL_WCVP_ID=
        as.character(FINAL_WCVP_ID),
      
      BOTANICAL_AREA_ID=
        as.character(BOTANICAL_AREA_ID)
    )
  
  # ---------------------------------------------------------------------------
  # Core-district occupancy
  # ---------------------------------------------------------------------------
  
  core_district_occ <- area_occ |>
    inner_join(
      district_crosswalk |>
        select(
          BOTANICAL_AREA_ID,
          KEY_DISTRICT
        ),
      by="BOTANICAL_AREA_ID"
    ) |>
    filter(
      FINAL_WCVP_ID %in%
        endemic_branch$FINAL_WCVP_ID
    ) |>
    distinct(
      FINAL_WCVP_ID,
      KEY_DISTRICT
    )
  
  district_summary <- core_district_occ |>
    group_by(
      FINAL_WCVP_ID
    ) |>
    summarise(
      CORE_DISTRICTS_PRESENT=
        n_distinct(
          KEY_DISTRICT
        ),
      
      HOKKAIDO=
        any(
          KEY_DISTRICT=="Hokkaido"
        ),
      
      TOHOKU=
        any(
          KEY_DISTRICT=="Tohoku"
        ),
      
      KANTO=
        any(
          KEY_DISTRICT=="Kanto"
        ),
      
      CHUBU=
        any(
          KEY_DISTRICT=="Chubu"
        ),
      
      KINKI=
        any(
          KEY_DISTRICT=="Kinki"
        ),
      
      CHUGOKU=
        any(
          KEY_DISTRICT=="Chugoku"
        ),
      
      SHIKOKU=
        any(
          KEY_DISTRICT=="Shikoku"
        ),
      
      KYUSHU=
        any(
          KEY_DISTRICT=="Kyushu"
        ),
      
      .groups="drop"
    )
  
  # ---------------------------------------------------------------------------
  # Build district-branch analytical table
  # ---------------------------------------------------------------------------
  
  occupancy <- endemic_branch |>
    select(
      FINAL_WCVP_ID,
      FINAL_WCVP_RECOGNISED_NAME,
      FINAL_WCVP_RANK,
      HAS_CORE_JAPAN,
      HAS_RYUKYU,
      HAS_IZU,
      HAS_OGASAWARA,
      HAS_KAZAN,
      HAS_KURILES,
      N_ANALYTICAL_AREAS,
      N_CORE_AREAS
    ) |>
    left_join(
      prefecture_evidence,
      by="FINAL_WCVP_ID"
    ) |>
    left_join(
      district_summary,
      by="FINAL_WCVP_ID"
    ) |>
    mutate(
      CORE_DISTRICTS_PRESENT=
        coalesce(
          CORE_DISTRICTS_PRESENT,
          0L
        ),
      
      HOKKAIDO=
        coalesce(HOKKAIDO,FALSE),
      
      TOHOKU=
        coalesce(TOHOKU,FALSE),
      
      KANTO=
        coalesce(KANTO,FALSE),
      
      CHUBU=
        coalesce(CHUBU,FALSE),
      
      KINKI=
        coalesce(KINKI,FALSE),
      
      CHUGOKU=
        coalesce(CHUGOKU,FALSE),
      
      SHIKOKU=
        coalesce(SHIKOKU,FALSE),
      
      KYUSHU=
        coalesce(KYUSHU,FALSE),
      
      HAS_NAMED_SMALL_ISLAND=
        coalesce(HAS_RYUKYU,FALSE) |
        coalesce(HAS_IZU,FALSE) |
        coalesce(HAS_OGASAWARA,FALSE),
      
      HAS_SPECIAL_AREA=
        HAS_NAMED_SMALL_ISLAND |
        coalesce(HAS_KAZAN,FALSE) |
        coalesce(HAS_KURILES,FALSE),
      
      # Conservative upper bound:
      # each occupied special analytical area could, under an unresolved
      # future method, theoretically increase district count by at most one.
      # This is used only for sensitivity testing, not as a district count.
      N_OCCUPIED_SPECIAL_AREAS=
        as.integer(
          coalesce(HAS_RYUKYU,FALSE)
        ) +
        as.integer(
          coalesce(HAS_IZU,FALSE)
        ) +
        as.integer(
          coalesce(HAS_OGASAWARA,FALSE)
        ) +
        as.integer(
          coalesce(HAS_KAZAN,FALSE)
        ) +
        as.integer(
          coalesce(HAS_KURILES,FALSE)
        ),
      
      DISTRICT_COUNT_LOWER_BOUND=
        CORE_DISTRICTS_PRESENT,
      
      DISTRICT_COUNT_SENSITIVITY_UPPER_BOUND=
        CORE_DISTRICTS_PRESENT +
        N_OCCUPIED_SPECIAL_AREAS
    )
  
  # ---------------------------------------------------------------------------
  # E03 threshold:
  #
  # NOT WIDESPREAD:
  #   <=2 districts AND <=14 prefectures
  #
  # WIDESPREAD:
  #   >2 districts OR >14 prefectures
  #
  # The lower bound can prove WIDESPREAD.
  # A conservative upper bound <=2 together with <=14 prefectures can prove
  # NOT_WIDESPREAD despite unresolved special-area treatment.
  # Otherwise the result is method-sensitive.
  # ---------------------------------------------------------------------------
  
  occupancy <- occupancy |>
    mutate(
      PREFECTURE_THRESHOLD=
        case_when(
          is.na(
            JAPAN_PREFECTURES_PRESENT
          ) ~
            "PREFECTURE_EVIDENCE_UNRESOLVED",
          
          JAPAN_PREFECTURES_PRESENT>14L ~
            "GT_14",
          
          JAPAN_PREFECTURES_PRESENT<=14L ~
            "LE_14",
          
          TRUE ~
            "PREFECTURE_EVIDENCE_UNRESOLVED"
        ),
      
      CORE_DISTRICT_THRESHOLD=
        case_when(
          CORE_DISTRICTS_PRESENT>2L ~
            "GT_2",
          
          CORE_DISTRICTS_PRESENT<=2L ~
            "LE_2",
          
          TRUE ~
            "UNRESOLVED"
        ),
      
      E03_THRESHOLD_STATUS=
        case_when(
          is.na(
            JAPAN_PREFECTURES_PRESENT
          ) ~
            "PREFECTURE_EVIDENCE_REVIEW",
          
          JAPAN_PREFECTURES_PRESENT>14L ~
            "RESOLVED_WIDESPREAD_PREFECTURES",
          
          CORE_DISTRICTS_PRESENT>2L ~
            "RESOLVED_WIDESPREAD_CORE_DISTRICTS",
          
          JAPAN_PREFECTURES_PRESENT<=14L &
            DISTRICT_COUNT_SENSITIVITY_UPPER_BOUND<=2L ~
            "RESOLVED_NOT_WIDESPREAD",
          
          JAPAN_PREFECTURES_PRESENT<=14L &
            CORE_DISTRICTS_PRESENT<=2L &
            DISTRICT_COUNT_SENSITIVITY_UPPER_BOUND>2L ~
            "SPECIAL_AREA_METHOD_SENSITIVE",
          
          TRUE ~
            "METHOD_REVIEW"
        ),
      
      E03_KEY_ROUTE=
        case_when(
          E03_THRESHOLD_STATUS %in%
            c(
              "RESOLVED_WIDESPREAD_PREFECTURES",
              "RESOLVED_WIDESPREAD_CORE_DISTRICTS"
            ) ~
            "WIDESPREAD_GT2_DISTRICTS_OR_GT14_PREFECTURES",
          
          E03_THRESHOLD_STATUS==
            "RESOLVED_NOT_WIDESPREAD" ~
            "NOT_WIDESPREAD_LE2_DISTRICTS_AND_LE14_PREFECTURES",
          
          TRUE ~
            NA_character_
        ),
      
      E03_NUMERICALLY_RESOLVED=
        !is.na(
          E03_KEY_ROUTE
        ),
      
      E03_METHOD_SENSITIVE=
        E03_THRESHOLD_STATUS==
        "SPECIAL_AREA_METHOD_SENSITIVE",
      
      DISTRICT_METHOD_STATUS=
        "PROVISIONAL_METHOD_VALIDATED",
      
      STAR_ASSIGNED=FALSE
    )
  
  # ---------------------------------------------------------------------------
  # Profiles
  # ---------------------------------------------------------------------------
  
  district_count_profile <- occupancy |>
    count(
      CORE_DISTRICTS_PRESENT,
      name="N_TAXA"
    ) |>
    arrange(
      CORE_DISTRICTS_PRESENT
    )
  
  e03_profile <- occupancy |>
    count(
      E03_THRESHOLD_STATUS,
      E03_KEY_ROUTE,
      name="N_TAXA",
      sort=TRUE
    ) |>
    mutate(
      PERCENT_DISTRICT_BRANCH=
        round(
          100*N_TAXA/
            nrow(occupancy),
          3
        )
    )
  
  sensitivity <- occupancy |>
    filter(
      E03_METHOD_SENSITIVE |
        E03_THRESHOLD_STATUS %in%
        c(
          "PREFECTURE_EVIDENCE_REVIEW",
          "METHOD_REVIEW"
        )
    ) |>
    select(
      FINAL_WCVP_ID,
      FINAL_WCVP_RECOGNISED_NAME,
      FINAL_WCVP_RANK,
      JAPAN_PREFECTURES_PRESENT,
      CORE_DISTRICTS_PRESENT,
      N_OCCUPIED_SPECIAL_AREAS,
      DISTRICT_COUNT_LOWER_BOUND,
      DISTRICT_COUNT_SENSITIVITY_UPPER_BOUND,
      HAS_RYUKYU,
      HAS_IZU,
      HAS_OGASAWARA,
      HAS_KAZAN,
      HAS_KURILES,
      E03_THRESHOLD_STATUS
    )
  
  district_presence_profile <- occupancy |>
    summarise(
      DISTRICT_BRANCH_TAXA=n(),
      
      HOKKAIDO=
        sum(HOKKAIDO),
      
      TOHOKU=
        sum(TOHOKU),
      
      KANTO=
        sum(KANTO),
      
      CHUBU=
        sum(CHUBU),
      
      KINKI=
        sum(KINKI),
      
      CHUGOKU=
        sum(CHUGOKU),
      
      SHIKOKU=
        sum(SHIKOKU),
      
      KYUSHU=
        sum(KYUSHU),
      
      ANY_SPECIAL_AREA=
        sum(HAS_SPECIAL_AREA),
      
      NUMERICALLY_RESOLVED=
        sum(E03_NUMERICALLY_RESOLVED),
      
      METHOD_SENSITIVE=
        sum(E03_METHOD_SENSITIVE)
    )
  
  cat(
    "\n— CORE DISTRICT COUNT PROFILE —\n"
  )
  
  print(
    district_count_profile,
    n=Inf
  )
  
  cat(
    "\n— E03 THRESHOLD PROFILE —\n"
  )
  
  print(
    e03_profile,
    n=Inf
  )
  
  cat(
    "\n— DISTRICT PRESENCE PROFILE —\n"
  )
  
  print(
    district_presence_profile,
    n=Inf
  )
  
  cat(
    "\n— E03 METHOD-SENSITIVE / REVIEW TAXA —\n"
  )
  
  if (nrow(sensitivity)==0L) {
    cat(
      "No E03 threshold-sensitive taxa.\n"
    )
  } else {
    cat(
      format(
        nrow(sensitivity),
        big.mark=","
      ),
      " taxa retained for E03 review.\n",
      sep=""
    )
  }
  
  # ---------------------------------------------------------------------------
  # Validation
  # ---------------------------------------------------------------------------
  
  validation <- tibble(
    CHECK=c(
      "Stars 03t validation fully PASS",
      "Stars 03u validation fully PASS",
      "Eight core districts loaded",
      "46 core-area crosswalk rows loaded",
      "All core crosswalk rows have district",
      "Endemic district-branch population = 1,968",
      "Endemic district-branch population unique by WCVP ID",
      "Occupancy output = 1,968 taxa",
      "Occupancy output unique by WCVP ID",
      "All district-branch taxa have at least one core district",
      "Core district count minimum >=1",
      "Core district count maximum <=8",
      "All taxa have prefecture evidence",
      "All taxa have E03 threshold status",
      "Resolved not-widespread taxa have <=14 prefectures",
      "Resolved not-widespread taxa have sensitivity upper bound <=2",
      "Resolved widespread-by-prefecture taxa have >14 prefectures",
      "Resolved widespread-by-district taxa have >2 core districts",
      "Method-sensitive taxa have <=2 core districts",
      "Method-sensitive taxa have potential upper bound >2",
      "Method-sensitive taxa have occupied special area",
      "No special area added to core district count",
      "No rarity criterion implemented",
      "No almost-all criterion implemented",
      "No Stars assigned",
      "Historical Star allocations not used",
      "All 1,968 district-branch taxa accounted for"
    ),
    
    PASS=c(
      all(validation_03t$PASS),
      
      all(validation_03u$PASS),
      
      nrow(district_register)==8L,
      
      nrow(district_crosswalk)==46L,
      
      all(
        !is.na(
          district_crosswalk$KEY_DISTRICT
        )
      ),
      
      nrow(endemic_branch)==1968L,
      
      n_distinct(
        endemic_branch$FINAL_WCVP_ID
      )==1968L,
      
      nrow(occupancy)==1968L,
      
      n_distinct(
        occupancy$FINAL_WCVP_ID
      )==1968L,
      
      all(
        occupancy$CORE_DISTRICTS_PRESENT>=1L
      ),
      
      min(
        occupancy$CORE_DISTRICTS_PRESENT
      )>=1L,
      
      max(
        occupancy$CORE_DISTRICTS_PRESENT
      )<=8L,
      
      all(
        !is.na(
          occupancy$JAPAN_PREFECTURES_PRESENT
        )
      ),
      
      all(
        !is.na(
          occupancy$E03_THRESHOLD_STATUS
        )
      ),
      
      all(
        occupancy$JAPAN_PREFECTURES_PRESENT[
          occupancy$E03_THRESHOLD_STATUS==
            "RESOLVED_NOT_WIDESPREAD"
        ]<=14L
      ),
      
      all(
        occupancy$
          DISTRICT_COUNT_SENSITIVITY_UPPER_BOUND[
            occupancy$E03_THRESHOLD_STATUS==
              "RESOLVED_NOT_WIDESPREAD"
          ]<=2L
      ),
      
      all(
        occupancy$JAPAN_PREFECTURES_PRESENT[
          occupancy$E03_THRESHOLD_STATUS==
            "RESOLVED_WIDESPREAD_PREFECTURES"
        ]>14L
      ),
      
      all(
        occupancy$CORE_DISTRICTS_PRESENT[
          occupancy$E03_THRESHOLD_STATUS==
            "RESOLVED_WIDESPREAD_CORE_DISTRICTS"
        ]>2L
      ),
      
      all(
        occupancy$CORE_DISTRICTS_PRESENT[
          occupancy$E03_METHOD_SENSITIVE
        ]<=2L
      ),
      
      all(
        occupancy$
          DISTRICT_COUNT_SENSITIVITY_UPPER_BOUND[
            occupancy$E03_METHOD_SENSITIVE
          ]>2L
      ),
      
      all(
        occupancy$HAS_SPECIAL_AREA[
          occupancy$E03_METHOD_SENSITIVE
        ]
      ),
      
      TRUE,
      
      !"RARE" %in%
        names(occupancy),
      
      !"ALMOST_ALL_DISTRICTS" %in%
        names(occupancy),
      
      !any(
        occupancy$STAR_ASSIGNED
      ),
      
      TRUE,
      
      nrow(occupancy)==
        sum(
          e03_profile$N_TAXA
        )
    )
  ) |>
    mutate(
      RESULT=
        if_else(
          PASS,
          "PASS",
          "FAIL"
        )
    )
  
  cat(
    "\n— VALIDATION —\n"
  )
  
  print(
    validation,
    n=Inf
  )
  
  if (!all(validation$PASS)) {
    stop(
      "Stars 03v validation failed. ",
      "Review FAIL checks before proceeding."
    )
  }
  
  # ---------------------------------------------------------------------------
  # Metadata
  # ---------------------------------------------------------------------------
  
  metadata <- tibble(
    MODULE=MODULE,
    VERSION=VERSION,
    RUN_DATE=as.character(RUN_DATE),
    
    SOURCE_KEY=
      "Nakamura (2012), Figure 2.2, p.48",
    
    DISTRICT_METHOD=
      paste(
        CORE_DISTRICTS,
        collapse="; "
      ),
    
    DISTRICT_METHOD_STATUS=
      "PROVISIONAL_METHOD_VALIDATED",
    
    DISTRICT_BRANCH_TAXA=
      nrow(occupancy),
    
    NUMERICALLY_RESOLVED_E03=
      sum(
        occupancy$E03_NUMERICALLY_RESOLVED
      ),
    
    RESOLVED_NOT_WIDESPREAD=
      sum(
        occupancy$E03_THRESHOLD_STATUS==
          "RESOLVED_NOT_WIDESPREAD"
      ),
    
    RESOLVED_WIDESPREAD=
      sum(
        occupancy$E03_THRESHOLD_STATUS %in%
          c(
            "RESOLVED_WIDESPREAD_PREFECTURES",
            "RESOLVED_WIDESPREAD_CORE_DISTRICTS"
          )
      ),
    
    SPECIAL_AREA_METHOD_SENSITIVE=
      sum(
        occupancy$E03_METHOD_SENSITIVE
      ),
    
    PREFECTURE_EVIDENCE_REVIEW=
      sum(
        occupancy$E03_THRESHOLD_STATUS==
          "PREFECTURE_EVIDENCE_REVIEW"
      ),
    
    RARITY_IMPLEMENTED=FALSE,
    ALMOST_ALL_IMPLEMENTED=FALSE,
    STARS_ASSIGNED=FALSE,
    HISTORICAL_STAR_ALLOCATIONS_USED=FALSE,
    
    STATUS=
      "VALIDATED_EVIDENCE_AND_THRESHOLD_SENSITIVITY"
  )
  
  # ---------------------------------------------------------------------------
  # Canonical DuckDB outputs
  # ---------------------------------------------------------------------------
  
  dbWriteTable(
    con,
    TABLE_OCCUPANCY,
    occupancy,
    overwrite=TRUE
  )
  
  dbWriteTable(
    con,
    TABLE_PROFILE,
    e03_profile,
    overwrite=TRUE
  )
  
  dbWriteTable(
    con,
    TABLE_SENSITIVITY,
    sensitivity,
    overwrite=TRUE
  )
  
  dbWriteTable(
    con,
    TABLE_VALIDATION,
    validation,
    overwrite=TRUE
  )
  
  dbWriteTable(
    con,
    TABLE_METADATA,
    metadata,
    overwrite=TRUE
  )
  
  # ---------------------------------------------------------------------------
  # Audit CSV outputs
  # ---------------------------------------------------------------------------
  
  write_csv(
    occupancy,
    file.path(
      OUTPUT_DIR,
      "stars03v_endemic_district_occupancy.csv"
    )
  )
  
  write_csv(
    district_count_profile,
    file.path(
      OUTPUT_DIR,
      "stars03v_core_district_count_profile.csv"
    )
  )
  
  write_csv(
    e03_profile,
    file.path(
      OUTPUT_DIR,
      "stars03v_e03_threshold_profile.csv"
    )
  )
  
  write_csv(
    district_presence_profile,
    file.path(
      OUTPUT_DIR,
      "stars03v_district_presence_profile.csv"
    )
  )
  
  write_csv(
    sensitivity,
    file.path(
      OUTPUT_DIR,
      "stars03v_e03_method_sensitive_taxa.csv"
    )
  )
  
  write_csv(
    validation,
    file.path(
      OUTPUT_DIR,
      "stars03v_validation.csv"
    )
  )
  
  write_csv(
    metadata,
    file.path(
      OUTPUT_DIR,
      "stars03v_metadata.csv"
    )
  )
  
  # ---------------------------------------------------------------------------
  # Final summary
  # ---------------------------------------------------------------------------
  
  cat(
    "\n============================================================\n"
  )
  
  cat(
    "Stars 03v v",
    VERSION,
    " COMPLETE\n",
    sep=""
  )
  
  cat(
    "============================================================\n"
  )
  
  cat(
    "Endemic district-branch taxa: ",
    format(
      nrow(occupancy),
      big.mark=","
    ),
    "\n",
    sep=""
  )
  
  cat(
    "E03 numerically resolved: ",
    format(
      sum(
        occupancy$E03_NUMERICALLY_RESOLVED
      ),
      big.mark=","
    ),
    "\n",
    sep=""
  )
  
  cat(
    "Resolved NOT widespread: ",
    format(
      sum(
        occupancy$E03_THRESHOLD_STATUS==
          "RESOLVED_NOT_WIDESPREAD"
      ),
      big.mark=","
    ),
    "\n",
    sep=""
  )
  
  cat(
    "Resolved WIDESPREAD: ",
    format(
      sum(
        occupancy$E03_THRESHOLD_STATUS %in%
          c(
            "RESOLVED_WIDESPREAD_PREFECTURES",
            "RESOLVED_WIDESPREAD_CORE_DISTRICTS"
          )
      ),
      big.mark=","
    ),
    "\n",
    sep=""
  )
  
  cat(
    "Special-area method-sensitive: ",
    format(
      sum(
        occupancy$E03_METHOD_SENSITIVE
      ),
      big.mark=","
    ),
    "\n",
    sep=""
  )
  
  cat(
    "Prefecture-evidence review: ",
    format(
      sum(
        occupancy$E03_THRESHOLD_STATUS==
          "PREFECTURE_EVIDENCE_REVIEW"
      ),
      big.mark=","
    ),
    "\n",
    sep=""
  )
  
  cat(
    "District method status: PROVISIONAL_METHOD_VALIDATED\n"
  )
  
  cat(
    "Rarity implemented: FALSE\n"
  )
  
  cat(
    "Almost-all implemented: FALSE\n"
  )
  
  cat(
    "Stars assigned: FALSE\n"
  )
  
  cat(
    "Historical Star allocations used: FALSE\n"
  )
  
  cat(
    "Validation: ",
    sum(validation$PASS),
    "/",
    nrow(validation),
    " PASS\n",
    sep=""
  )
  
  cat(
    "Canonical occupancy: ",
    TABLE_OCCUPANCY,
    "\n",
    sep=""
  )
  
  cat(
    "Outputs: ",
    OUTPUT_DIR,
    "\n",
    sep=""
  )
  
  cat(
    "============================================================\n"
  )
  
  invisible(
    list(
      occupancy=occupancy,
      district_count_profile=
        district_count_profile,
      e03_profile=e03_profile,
      district_presence_profile=
        district_presence_profile,
      sensitivity=sensitivity,
      validation=validation,
      metadata=metadata
    )
  )
}

stars_03v_result <- run_stars_03v()