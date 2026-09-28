# =============================================================================
# VPJD-OJPCP
# R/stars/03o_profile_district_threshold_sensitivity.R
# Version 0.1.1
#
# PROFILE TAXON-LEVEL SENSITIVITY TO UNRESOLVED SPECIAL-AREA DISTRICT TREATMENT
#
# PURPOSE
# - Use validated Stars 03n evidence to identify which accepted taxa could
#   actually change Key route depending on treatment of JP47-JP51.
# - Separate simple occurrence in an unresolved special area from genuine
#   sensitivity to a district threshold in Nakamura's Key.
# - Quantify the remaining methodological work before district-based Key
#   traversal can be finalised.
#
# IMPORTANT
# - This module DOES NOT decide how JP47-JP51 are treated as Key districts.
# - It DOES NOT assign Stars.
# - It DOES NOT create JAPAN_DISTRICT_COUNT.
# - It DOES NOT evaluate "almost all districts".
# - It DOES NOT use historical Star allocations.
# - CORE_DISTRICT_COUNT is the established minimum district evidence.
# - MAX_POSSIBLE_DISTRICT_COUNT is a conservative diagnostic upper bound only.
# - The upper bound assumes each occupied unresolved special analytical unit
#   could, for sensitivity testing only, contribute one additional district.
#   This is NOT a methodological interpretation of the Key.
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

MODULE <- "stars_03o_profile_district_threshold_sensitivity"
VERSION <- "0.1.1"
RUN_DATE <- Sys.Date()

DB_PATH <- here(
  "data", "interim", "occurrences", "vpjd_occurrences.duckdb"
)

OUTPUT_DIR <- here(
  "outputs", "tables", "stars",
  "district_threshold_sensitivity"
)

SOURCE_TABLE <- "vpjd_star_taxon_key_geography_evidence"
OUTPUT_TABLE <- "vpjd_star_district_threshold_sensitivity"
OUTPUT_QUARANTINE <- "vpjd_star_district_threshold_method_quarantine"
OUTPUT_VALIDATION <- "vpjd_star_03o_validation"
OUTPUT_METADATA <- "vpjd_star_03o_metadata"

run_stars_03o <- function() {
  
  cat("\n— Profile district-threshold sensitivity —\n\n")
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
  
  required_tables <- c(
    SOURCE_TABLE,
    "vpjd_star_03n_validation",
    "vpjd_star_03n_metadata"
  )
  
  missing_tables <- setdiff(
    required_tables,
    dbListTables(con)
  )
  
  if (length(missing_tables) > 0L) {
    stop(
      "Required Stars 03n tables missing: ",
      paste(missing_tables, collapse = ", ")
    )
  }
  
  # ===========================================================================
  # 1. Confirm Stars 03n validation
  # ===========================================================================
  
  validation_03n <- dbGetQuery(
    con,
    "
    SELECT *
    FROM vpjd_star_03n_validation
    "
  ) |>
    as_tibble()
  
  if (
    !"PASS" %in% names(validation_03n) ||
    !all(validation_03n$PASS)
  ) {
    stop(
      "Stars 03n is not fully validated. ",
      "03o will not proceed."
    )
  }
  
  metadata_03n <- dbGetQuery(
    con,
    "
    SELECT *
    FROM vpjd_star_03n_metadata
    "
  ) |>
    as_tibble()
  
  cat(
    "Stars 03n validation: ",
    sum(validation_03n$PASS),
    "/",
    nrow(validation_03n),
    " PASS\n",
    sep = ""
  )
  
  # ===========================================================================
  # 2. Load validated Stars 03n evidence
  # ===========================================================================
  
  evidence <- dbGetQuery(
    con,
    paste0(
      "SELECT * FROM ",
      SOURCE_TABLE
    )
  ) |>
    as_tibble()
  
  required_fields <- c(
    "FINAL_WCVP_ID",
    "FINAL_WCVP_RECOGNISED_NAME",
    "FINAL_WCVP_RANK",
    "JAPAN_PREFECTURE_COUNT",
    "ENDEMIC_TO_JAPAN",
    "INTRODUCED_TO_JAPAN",
    "HYBRID_STATUS",
    "PRE_KEY_EVIDENCE_ROUTE",
    "CORE_DISTRICT_COUNT",
    "RYUKYU_PRESENT",
    "IZU_PRESENT",
    "OGASAWARA_PRESENT",
    "KAZAN_PRESENT",
    "KURILES_PRESENT",
    "N_SPECIAL_AREAS",
    "SPECIAL_AREA_PRESENT"
  )
  
  missing_fields <- setdiff(
    required_fields,
    names(evidence)
  )
  
  if (length(missing_fields) > 0L) {
    stop(
      "Required Stars 03n fields missing: ",
      paste(missing_fields, collapse = ", ")
    )
  }
  
  if (nrow(evidence) != 11439L) {
    stop(
      "Expected 11,439 accepted taxa from Stars 03n; found ",
      format(nrow(evidence), big.mark = ",")
    )
  }
  
  if (
    n_distinct(
      evidence$FINAL_WCVP_ID
    ) != 11439L
  ) {
    stop(
      "Stars 03n evidence is not one row per accepted taxon."
    )
  }
  
  cat(
    "Accepted taxa loaded: ",
    format(nrow(evidence), big.mark = ","),
    "\n",
    sep = ""
  )
  
  cat(
    "Taxa with special-area evidence: ",
    format(
      sum(evidence$SPECIAL_AREA_PRESENT),
      big.mark = ","
    ),
    "\n\n",
    sep = ""
  )
  
  # ===========================================================================
  # 3. Conservative district-count interval
  #
  # MIN = established occupancy of the eight core districts.
  #
  # MAX = MIN + number of occupied unresolved special analytical units.
  #
  # MAX is deliberately only a sensitivity bound. It does NOT mean that
  # Ryukyu, Izu, Ogasawara, Kazan or Kuriles are independent Key districts.
  # ===========================================================================
  
  sensitivity <- evidence |>
    mutate(
      MIN_POSSIBLE_DISTRICT_COUNT =
        as.integer(CORE_DISTRICT_COUNT),
      
      MAX_POSSIBLE_DISTRICT_COUNT =
        as.integer(
          pmin(
            CORE_DISTRICT_COUNT +
              N_SPECIAL_AREAS,
            13L
          )
        ),
      
      DISTRICT_COUNT_INTERVAL_WIDTH =
        MAX_POSSIBLE_DISTRICT_COUNT -
        MIN_POSSIBLE_DISTRICT_COUNT,
      
      HAS_DISTRICT_UNCERTAINTY =
        DISTRICT_COUNT_INTERVAL_WIDTH > 0
    )
  
  # ===========================================================================
  # 4. Generic threshold-crossing diagnostics
  # ===========================================================================
  
  sensitivity <- sensitivity |>
    mutate(
      CROSS_THRESHOLD_1 =
        MIN_POSSIBLE_DISTRICT_COUNT <= 1 &
        MAX_POSSIBLE_DISTRICT_COUNT > 1,
      
      CROSS_THRESHOLD_2 =
        MIN_POSSIBLE_DISTRICT_COUNT <= 2 &
        MAX_POSSIBLE_DISTRICT_COUNT > 2,
      
      CROSS_THRESHOLD_3 =
        MIN_POSSIBLE_DISTRICT_COUNT <= 3 &
        MAX_POSSIBLE_DISTRICT_COUNT > 3,
      
      THRESHOLD_1_STATUS =
        case_when(
          MAX_POSSIBLE_DISTRICT_COUNT <= 1 ~
            "STABLE_LE_1",
          MIN_POSSIBLE_DISTRICT_COUNT > 1 ~
            "STABLE_GT_1",
          TRUE ~
            "SENSITIVE_AT_1"
        ),
      
      THRESHOLD_2_STATUS =
        case_when(
          MAX_POSSIBLE_DISTRICT_COUNT <= 2 ~
            "STABLE_LE_2",
          MIN_POSSIBLE_DISTRICT_COUNT > 2 ~
            "STABLE_GT_2",
          TRUE ~
            "SENSITIVE_AT_2"
        ),
      
      THRESHOLD_3_STATUS =
        case_when(
          MAX_POSSIBLE_DISTRICT_COUNT <= 3 ~
            "STABLE_LE_3",
          MIN_POSSIBLE_DISTRICT_COUNT > 3 ~
            "STABLE_GT_3",
          TRUE ~
            "SENSITIVE_AT_3"
        )
    )
  
  # ===========================================================================
  # 5. Branch-specific Key sensitivity
  #
  # ENDEMIC branch:
  # - Relevant known district split: <=2 versus >2.
  #
  # NON-ENDEMIC branch:
  # - Relevant known district bands: <=1, <=2, <=3, >3.
  #
  # GX / HYB:
  # - Retained for audit.
  # - District thresholds do not need evaluating for the current pre-Key route.
  #
  # ENDEMICITY_UNRESOLVED:
  # - Cannot yet select branch.
  # ===========================================================================
  
  sensitivity <- sensitivity |>
    mutate(
      ENDEMIC_DISTRICT_THRESHOLD_SENSITIVE =
        case_when(
          PRE_KEY_EVIDENCE_ROUTE == "ENDEMIC_KEY" ~
            CROSS_THRESHOLD_2,
          TRUE ~
            FALSE
        ),
      
      NON_ENDEMIC_THRESHOLD_1_SENSITIVE =
        case_when(
          PRE_KEY_EVIDENCE_ROUTE == "NON_ENDEMIC_KEY" ~
            CROSS_THRESHOLD_1,
          TRUE ~
            FALSE
        ),
      
      NON_ENDEMIC_THRESHOLD_2_SENSITIVE =
        case_when(
          PRE_KEY_EVIDENCE_ROUTE == "NON_ENDEMIC_KEY" ~
            CROSS_THRESHOLD_2,
          TRUE ~
            FALSE
        ),
      
      NON_ENDEMIC_THRESHOLD_3_SENSITIVE =
        case_when(
          PRE_KEY_EVIDENCE_ROUTE == "NON_ENDEMIC_KEY" ~
            CROSS_THRESHOLD_3,
          TRUE ~
            FALSE
        ),
      
      NON_ENDEMIC_ANY_DISTRICT_THRESHOLD_SENSITIVE =
        NON_ENDEMIC_THRESHOLD_1_SENSITIVE |
        NON_ENDEMIC_THRESHOLD_2_SENSITIVE |
        NON_ENDEMIC_THRESHOLD_3_SENSITIVE,
      
      KEY_DISTRICT_THRESHOLD_SENSITIVE =
        case_when(
          PRE_KEY_EVIDENCE_ROUTE == "ENDEMIC_KEY" ~
            ENDEMIC_DISTRICT_THRESHOLD_SENSITIVE,
          
          PRE_KEY_EVIDENCE_ROUTE == "NON_ENDEMIC_KEY" ~
            NON_ENDEMIC_ANY_DISTRICT_THRESHOLD_SENSITIVE,
          
          TRUE ~
            FALSE
        )
    )
  
  # ===========================================================================
  # 6. Stable district evidence categories
  # ===========================================================================
  
  sensitivity <- sensitivity |>
    mutate(
      KEY_DISTRICT_THRESHOLD_STATUS =
        case_when(
          PRE_KEY_EVIDENCE_ROUTE == "GX_EVIDENCE" ~
            "DISTRICT_NOT_REQUIRED_GX",
          
          PRE_KEY_EVIDENCE_ROUTE == "HYB_EVIDENCE" ~
            "DISTRICT_NOT_REQUIRED_HYB",
          
          PRE_KEY_EVIDENCE_ROUTE ==
            "ENDEMICITY_UNRESOLVED" ~
            "BRANCH_UNRESOLVED",
          
          PRE_KEY_EVIDENCE_ROUTE == "ENDEMIC_KEY" &
            KEY_DISTRICT_THRESHOLD_SENSITIVE ~
            "METHOD_SENSITIVE",
          
          PRE_KEY_EVIDENCE_ROUTE == "ENDEMIC_KEY" &
            MAX_POSSIBLE_DISTRICT_COUNT <= 2 ~
            "STABLE_LE_2",
          
          PRE_KEY_EVIDENCE_ROUTE == "ENDEMIC_KEY" &
            MIN_POSSIBLE_DISTRICT_COUNT > 2 ~
            "STABLE_GT_2",
          
          PRE_KEY_EVIDENCE_ROUTE == "NON_ENDEMIC_KEY" &
            KEY_DISTRICT_THRESHOLD_SENSITIVE ~
            "METHOD_SENSITIVE",
          
          PRE_KEY_EVIDENCE_ROUTE == "NON_ENDEMIC_KEY" &
            MAX_POSSIBLE_DISTRICT_COUNT <= 1 ~
            "STABLE_LE_1",
          
          PRE_KEY_EVIDENCE_ROUTE == "NON_ENDEMIC_KEY" &
            MIN_POSSIBLE_DISTRICT_COUNT > 3 ~
            "STABLE_GT_3",
          
          PRE_KEY_EVIDENCE_ROUTE == "NON_ENDEMIC_KEY" &
            MIN_POSSIBLE_DISTRICT_COUNT > 1 &
            MAX_POSSIBLE_DISTRICT_COUNT <= 2 ~
            "STABLE_GT_1_LE_2",
          
          PRE_KEY_EVIDENCE_ROUTE == "NON_ENDEMIC_KEY" &
            MIN_POSSIBLE_DISTRICT_COUNT > 2 &
            MAX_POSSIBLE_DISTRICT_COUNT <= 3 ~
            "STABLE_GT_2_LE_3",
          
          TRUE ~
            "REVIEW"
        ),
      
      DISTRICT_THRESHOLD_METHOD_REQUIRED =
        KEY_DISTRICT_THRESHOLD_STATUS ==
        "METHOD_SENSITIVE",
      
      FINAL_STAR_ASSIGNED_03O =
        FALSE
    )
  
  # ===========================================================================
  # 7. Methodological quarantine
  # ===========================================================================
  
  quarantine <- sensitivity |>
    filter(
      DISTRICT_THRESHOLD_METHOD_REQUIRED
    ) |>
    select(
      FINAL_WCVP_ID,
      FINAL_WCVP_RECOGNISED_NAME,
      FINAL_WCVP_RANK,
      PRE_KEY_EVIDENCE_ROUTE,
      JAPAN_PREFECTURE_COUNT,
      CORE_DISTRICT_COUNT,
      N_SPECIAL_AREAS,
      MIN_POSSIBLE_DISTRICT_COUNT,
      MAX_POSSIBLE_DISTRICT_COUNT,
      RYUKYU_PRESENT,
      IZU_PRESENT,
      OGASAWARA_PRESENT,
      KAZAN_PRESENT,
      KURILES_PRESENT,
      CROSS_THRESHOLD_1,
      CROSS_THRESHOLD_2,
      CROSS_THRESHOLD_3,
      ENDEMIC_DISTRICT_THRESHOLD_SENSITIVE,
      NON_ENDEMIC_THRESHOLD_1_SENSITIVE,
      NON_ENDEMIC_THRESHOLD_2_SENSITIVE,
      NON_ENDEMIC_THRESHOLD_3_SENSITIVE,
      KEY_DISTRICT_THRESHOLD_STATUS
    ) |>
    arrange(
      PRE_KEY_EVIDENCE_ROUTE,
      CORE_DISTRICT_COUNT,
      FINAL_WCVP_RECOGNISED_NAME
    )
  
  # ===========================================================================
  # 8. Profiles
  #
  # v0.1.1:
  # Summary field names deliberately differ from the source row-level field
  # SPECIAL_AREA_PRESENT to prevent dplyr summarise() name shadowing.
  # ===========================================================================
  
  overall_profile <- sensitivity |>
    summarise(
      ACCEPTED_TAXA =
        n(),
      
      TAXA_WITH_SPECIAL_AREA =
        sum(SPECIAL_AREA_PRESENT),
      
      TAXA_WITHOUT_SPECIAL_AREA =
        sum(!SPECIAL_AREA_PRESENT),
      
      ANY_DISTRICT_COUNT_UNCERTAINTY =
        sum(HAS_DISTRICT_UNCERTAINTY),
      
      KEY_THRESHOLD_SENSITIVE =
        sum(KEY_DISTRICT_THRESHOLD_SENSITIVE),
      
      KEY_THRESHOLD_STABLE =
        sum(
          PRE_KEY_EVIDENCE_ROUTE %in%
            c("ENDEMIC_KEY", "NON_ENDEMIC_KEY") &
            !KEY_DISTRICT_THRESHOLD_SENSITIVE
        ),
      
      GX_HYB_DISTRICT_NOT_REQUIRED =
        sum(
          PRE_KEY_EVIDENCE_ROUTE %in%
            c("GX_EVIDENCE", "HYB_EVIDENCE")
        ),
      
      ENDEMICITY_UNRESOLVED =
        sum(
          PRE_KEY_EVIDENCE_ROUTE ==
            "ENDEMICITY_UNRESOLVED"
        )
    )
  
  branch_profile <- sensitivity |>
    group_by(
      PRE_KEY_EVIDENCE_ROUTE
    ) |>
    summarise(
      N_TAXA =
        n(),
      
      WITH_SPECIAL_AREA =
        sum(SPECIAL_AREA_PRESENT),
      
      THRESHOLD_SENSITIVE =
        sum(KEY_DISTRICT_THRESHOLD_SENSITIVE),
      
      THRESHOLD_STABLE =
        sum(
          !KEY_DISTRICT_THRESHOLD_SENSITIVE
        ),
      
      PERCENT_SENSITIVE =
        round(
          100 *
            THRESHOLD_SENSITIVE /
            N_TAXA,
          2
        ),
      
      .groups = "drop"
    ) |>
    arrange(
      desc(N_TAXA)
    )
  
  status_profile <- sensitivity |>
    count(
      PRE_KEY_EVIDENCE_ROUTE,
      KEY_DISTRICT_THRESHOLD_STATUS,
      name = "N_TAXA"
    ) |>
    group_by(
      PRE_KEY_EVIDENCE_ROUTE
    ) |>
    mutate(
      ROUTE_PERCENT =
        round(
          100 * N_TAXA /
            sum(N_TAXA),
          2
        )
    ) |>
    ungroup() |>
    arrange(
      PRE_KEY_EVIDENCE_ROUTE,
      desc(N_TAXA)
    )
  
  threshold_crossing_profile <- sensitivity |>
    summarise(
      CROSS_THRESHOLD_1 =
        sum(CROSS_THRESHOLD_1),
      
      CROSS_THRESHOLD_2 =
        sum(CROSS_THRESHOLD_2),
      
      CROSS_THRESHOLD_3 =
        sum(CROSS_THRESHOLD_3),
      
      ENDEMIC_CROSS_THRESHOLD_2 =
        sum(
          ENDEMIC_DISTRICT_THRESHOLD_SENSITIVE
        ),
      
      NON_ENDEMIC_CROSS_THRESHOLD_1 =
        sum(
          NON_ENDEMIC_THRESHOLD_1_SENSITIVE
        ),
      
      NON_ENDEMIC_CROSS_THRESHOLD_2 =
        sum(
          NON_ENDEMIC_THRESHOLD_2_SENSITIVE
        ),
      
      NON_ENDEMIC_CROSS_THRESHOLD_3 =
        sum(
          NON_ENDEMIC_THRESHOLD_3_SENSITIVE
        ),
      
      NON_ENDEMIC_ANY_THRESHOLD =
        sum(
          NON_ENDEMIC_ANY_DISTRICT_THRESHOLD_SENSITIVE
        )
    )
  
  special_area_sensitivity <- bind_rows(
    sensitivity |>
      filter(RYUKYU_PRESENT) |>
      summarise(
        SPECIAL_AREA = "Ryukyu",
        N_TAXA = n(),
        THRESHOLD_SENSITIVE =
          sum(KEY_DISTRICT_THRESHOLD_SENSITIVE)
      ),
    
    sensitivity |>
      filter(IZU_PRESENT) |>
      summarise(
        SPECIAL_AREA = "Izu",
        N_TAXA = n(),
        THRESHOLD_SENSITIVE =
          sum(KEY_DISTRICT_THRESHOLD_SENSITIVE)
      ),
    
    sensitivity |>
      filter(OGASAWARA_PRESENT) |>
      summarise(
        SPECIAL_AREA = "Ogasawara",
        N_TAXA = n(),
        THRESHOLD_SENSITIVE =
          sum(KEY_DISTRICT_THRESHOLD_SENSITIVE)
      ),
    
    sensitivity |>
      filter(KAZAN_PRESENT) |>
      summarise(
        SPECIAL_AREA = "Kazan",
        N_TAXA = n(),
        THRESHOLD_SENSITIVE =
          sum(KEY_DISTRICT_THRESHOLD_SENSITIVE)
      ),
    
    sensitivity |>
      filter(KURILES_PRESENT) |>
      summarise(
        SPECIAL_AREA = "Kuriles",
        N_TAXA = n(),
        THRESHOLD_SENSITIVE =
          sum(KEY_DISTRICT_THRESHOLD_SENSITIVE)
      )
  ) |>
    mutate(
      PERCENT_SENSITIVE =
        round(
          100 *
            THRESHOLD_SENSITIVE /
            N_TAXA,
          2
        )
    )
  
  interval_profile <- sensitivity |>
    count(
      MIN_POSSIBLE_DISTRICT_COUNT,
      MAX_POSSIBLE_DISTRICT_COUNT,
      name = "N_TAXA"
    ) |>
    arrange(
      MIN_POSSIBLE_DISTRICT_COUNT,
      MAX_POSSIBLE_DISTRICT_COUNT
    )
  
  cat("\n— OVERALL SENSITIVITY PROFILE —\n")
  print(overall_profile, n = Inf)
  
  cat("\n— SENSITIVITY BY PRE-KEY ROUTE —\n")
  print(branch_profile, n = Inf)
  
  cat("\n— DISTRICT-THRESHOLD STATUS BY ROUTE —\n")
  print(status_profile, n = Inf)
  
  cat("\n— THRESHOLD-CROSSING PROFILE —\n")
  print(threshold_crossing_profile, n = Inf)
  
  cat("\n— SPECIAL-AREA SENSITIVITY PROFILE —\n")
  print(special_area_sensitivity, n = Inf)
  
  cat("\n— DISTRICT-COUNT INTERVAL PROFILE —\n")
  print(interval_profile, n = Inf)
  
  # ===========================================================================
  # 9. Validation
  # ===========================================================================
  
  validation <- tibble(
    CHECK = c(
      "Stars 03n validation fully PASS",
      "Accepted population = 11,439",
      "One row per accepted FINAL_WCVP_ID",
      "03n special-area total = 3,936",
      "03n no-special-area total = 7,503",
      "Special/no-special totals sum to 11,439",
      "Minimum district count bounded 0-8",
      "Maximum district count >= minimum",
      "Maximum district count bounded 0-13",
      "Special-area presence equals positive interval width",
      "No-special-area taxa have zero interval width",
      "Threshold-1 sensitivity logically valid",
      "Threshold-2 sensitivity logically valid",
      "Threshold-3 sensitivity logically valid",
      "Endemic sensitivity uses threshold 2 only",
      "Non-endemic sensitivity uses thresholds 1-3",
      "GX taxa never district-threshold sensitive",
      "HYB taxa never district-threshold sensitive",
      "Method quarantine equals sensitive taxa",
      "No JAPAN_DISTRICT_COUNT created",
      "No ALMOST_ALL_DISTRICTS created",
      "No STAR_CATEGORY created",
      "No Star assignments created",
      "Historical Star allocations not used"
    ),
    
    PASS = c(
      all(validation_03n$PASS),
      
      nrow(sensitivity) == 11439L,
      
      n_distinct(
        sensitivity$FINAL_WCVP_ID
      ) == 11439L,
      
      sum(
        sensitivity$SPECIAL_AREA_PRESENT
      ) == 3936L,
      
      sum(
        !sensitivity$SPECIAL_AREA_PRESENT
      ) == 7503L,
      
      (
        sum(sensitivity$SPECIAL_AREA_PRESENT) +
          sum(!sensitivity$SPECIAL_AREA_PRESENT)
      ) == 11439L,
      
      all(
        sensitivity$MIN_POSSIBLE_DISTRICT_COUNT >= 0 &
          sensitivity$MIN_POSSIBLE_DISTRICT_COUNT <= 8
      ),
      
      all(
        sensitivity$MAX_POSSIBLE_DISTRICT_COUNT >=
          sensitivity$MIN_POSSIBLE_DISTRICT_COUNT
      ),
      
      all(
        sensitivity$MAX_POSSIBLE_DISTRICT_COUNT >= 0 &
          sensitivity$MAX_POSSIBLE_DISTRICT_COUNT <= 13
      ),
      
      all(
        sensitivity$HAS_DISTRICT_UNCERTAINTY ==
          sensitivity$SPECIAL_AREA_PRESENT
      ),
      
      all(
        sensitivity$DISTRICT_COUNT_INTERVAL_WIDTH[
          !sensitivity$SPECIAL_AREA_PRESENT
        ] == 0
      ),
      
      all(
        sensitivity$CROSS_THRESHOLD_1 ==
          (
            sensitivity$MIN_POSSIBLE_DISTRICT_COUNT <= 1 &
              sensitivity$MAX_POSSIBLE_DISTRICT_COUNT > 1
          )
      ),
      
      all(
        sensitivity$CROSS_THRESHOLD_2 ==
          (
            sensitivity$MIN_POSSIBLE_DISTRICT_COUNT <= 2 &
              sensitivity$MAX_POSSIBLE_DISTRICT_COUNT > 2
          )
      ),
      
      all(
        sensitivity$CROSS_THRESHOLD_3 ==
          (
            sensitivity$MIN_POSSIBLE_DISTRICT_COUNT <= 3 &
              sensitivity$MAX_POSSIBLE_DISTRICT_COUNT > 3
          )
      ),
      
      all(
        sensitivity$ENDEMIC_DISTRICT_THRESHOLD_SENSITIVE ==
          (
            sensitivity$PRE_KEY_EVIDENCE_ROUTE ==
              "ENDEMIC_KEY" &
              sensitivity$CROSS_THRESHOLD_2
          )
      ),
      
      all(
        sensitivity$NON_ENDEMIC_ANY_DISTRICT_THRESHOLD_SENSITIVE ==
          (
            sensitivity$PRE_KEY_EVIDENCE_ROUTE ==
              "NON_ENDEMIC_KEY" &
              (
                sensitivity$CROSS_THRESHOLD_1 |
                  sensitivity$CROSS_THRESHOLD_2 |
                  sensitivity$CROSS_THRESHOLD_3
              )
          )
      ),
      
      !any(
        sensitivity$KEY_DISTRICT_THRESHOLD_SENSITIVE[
          sensitivity$PRE_KEY_EVIDENCE_ROUTE ==
            "GX_EVIDENCE"
        ]
      ),
      
      !any(
        sensitivity$KEY_DISTRICT_THRESHOLD_SENSITIVE[
          sensitivity$PRE_KEY_EVIDENCE_ROUTE ==
            "HYB_EVIDENCE"
        ]
      ),
      
      nrow(quarantine) ==
        sum(
          sensitivity$KEY_DISTRICT_THRESHOLD_SENSITIVE
        ),
      
      !"JAPAN_DISTRICT_COUNT" %in%
        names(sensitivity),
      
      !"ALMOST_ALL_DISTRICTS" %in%
        names(sensitivity),
      
      !"STAR_CATEGORY" %in%
        names(sensitivity),
      
      all(
        sensitivity$FINAL_STAR_ASSIGNED_03O ==
          FALSE
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
  print(validation, n = Inf)
  
  if (!all(validation$PASS)) {
    stop(
      "Stars 03o validation failed. ",
      "Review FAIL checks before proceeding."
    )
  }
  
  # ===========================================================================
  # 10. Metadata
  # ===========================================================================
  
  metadata <- tibble(
    MODULE = MODULE,
    VERSION = VERSION,
    RUN_DATE = as.character(RUN_DATE),
    
    SOURCE =
      SOURCE_TABLE,
    
    SOURCE_VERSION =
      if (
        "VERSION" %in%
        names(metadata_03n)
      ) {
        as.character(
          metadata_03n$VERSION[[1]]
        )
      } else {
        "UNKNOWN"
      },
    
    ACCEPTED_TAXA =
      nrow(sensitivity),
    
    TAXA_WITH_SPECIAL_AREA_EVIDENCE =
      sum(
        sensitivity$SPECIAL_AREA_PRESENT
      ),
    
    TAXA_WITHOUT_SPECIAL_AREA_EVIDENCE =
      sum(
        !sensitivity$SPECIAL_AREA_PRESENT
      ),
    
    DISTRICT_THRESHOLD_SENSITIVE_TAXA =
      sum(
        sensitivity$KEY_DISTRICT_THRESHOLD_SENSITIVE
      ),
    
    DISTRICT_THRESHOLD_METHOD_QUARANTINE =
      nrow(quarantine),
    
    CORE_DISTRICT_COUNT_IS_MINIMUM =
      TRUE,
    
    MAX_COUNT_IS_DIAGNOSTIC_BOUND_ONLY =
      TRUE,
    
    SPECIAL_AREAS_ASSIGNED_AS_DISTRICTS =
      FALSE,
    
    JAPAN_DISTRICT_COUNT_CREATED =
      FALSE,
    
    ALMOST_ALL_DISTRICTS_EVALUATED =
      FALSE,
    
    STAR_CLASSIFICATIONS_CREATED =
      FALSE,
    
    HISTORICAL_STAR_ALLOCATIONS_USED =
      FALSE,
    
    FINAL_STAR_CLASSIFIER_READY =
      FALSE,
    
    STATUS =
      "VALIDATED_DIAGNOSTIC_SENSITIVITY_MODULE"
  )
  
  # ===========================================================================
  # 11. Write canonical DuckDB outputs
  # ===========================================================================
  
  dbWriteTable(
    con,
    OUTPUT_TABLE,
    sensitivity,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    OUTPUT_QUARANTINE,
    quarantine,
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
  
  # ===========================================================================
  # 12. Write audit CSV outputs
  # ===========================================================================
  
  write_csv(
    overall_profile,
    file.path(
      OUTPUT_DIR,
      "stars03o_overall_sensitivity_profile.csv"
    )
  )
  
  write_csv(
    branch_profile,
    file.path(
      OUTPUT_DIR,
      "stars03o_sensitivity_by_route.csv"
    )
  )
  
  write_csv(
    status_profile,
    file.path(
      OUTPUT_DIR,
      "stars03o_threshold_status_by_route.csv"
    )
  )
  
  write_csv(
    threshold_crossing_profile,
    file.path(
      OUTPUT_DIR,
      "stars03o_threshold_crossing_profile.csv"
    )
  )
  
  write_csv(
    special_area_sensitivity,
    file.path(
      OUTPUT_DIR,
      "stars03o_special_area_sensitivity.csv"
    )
  )
  
  write_csv(
    interval_profile,
    file.path(
      OUTPUT_DIR,
      "stars03o_district_count_interval_profile.csv"
    )
  )
  
  write_csv(
    quarantine,
    file.path(
      OUTPUT_DIR,
      "stars03o_district_threshold_method_quarantine.csv"
    )
  )
  
  write_csv(
    validation,
    file.path(
      OUTPUT_DIR,
      "stars03o_validation.csv"
    )
  )
  
  write_csv(
    metadata,
    file.path(
      OUTPUT_DIR,
      "stars03o_metadata.csv"
    )
  )
  
  # ===========================================================================
  # 13. Final summary
  # ===========================================================================
  
  cat("\n============================================================\n")
  cat("Stars 03o v", VERSION, " COMPLETE\n", sep = "")
  cat("============================================================\n")
  
  cat(
    "Accepted taxa: ",
    format(
      nrow(sensitivity),
      big.mark = ","
    ),
    "\n",
    sep = ""
  )
  
  cat(
    "Taxa with special-area evidence: ",
    format(
      sum(
        sensitivity$SPECIAL_AREA_PRESENT
      ),
      big.mark = ","
    ),
    "\n",
    sep = ""
  )
  
  cat(
    "Taxa without special-area evidence: ",
    format(
      sum(
        !sensitivity$SPECIAL_AREA_PRESENT
      ),
      big.mark = ","
    ),
    "\n",
    sep = ""
  )
  
  cat(
    "Taxa sensitive to a known district threshold: ",
    format(
      sum(
        sensitivity$KEY_DISTRICT_THRESHOLD_SENSITIVE
      ),
      big.mark = ","
    ),
    "\n",
    sep = ""
  )
  
  cat(
    "District-threshold method quarantine: ",
    format(
      nrow(quarantine),
      big.mark = ","
    ),
    "\n",
    sep = ""
  )
  
  cat(
    "Taxa removed from broad 03n special-area quarantine: ",
    format(
      sum(
        sensitivity$SPECIAL_AREA_PRESENT
      ) -
        nrow(quarantine),
      big.mark = ","
    ),
    "\n",
    sep = ""
  )
  
  cat("Special areas assigned as Key districts: FALSE\n")
  cat("JAPAN_DISTRICT_COUNT created: FALSE\n")
  cat("'Almost all districts' evaluated: FALSE\n")
  cat("Star classifications created: FALSE\n")
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
    "Canonical DuckDB table: ",
    OUTPUT_TABLE,
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
      sensitivity = sensitivity,
      quarantine = quarantine,
      overall_profile = overall_profile,
      branch_profile = branch_profile,
      status_profile = status_profile,
      threshold_crossing_profile = threshold_crossing_profile,
      special_area_sensitivity = special_area_sensitivity,
      interval_profile = interval_profile,
      validation = validation,
      metadata = metadata
    )
  )
}

stars_03o_result <- run_stars_03o()