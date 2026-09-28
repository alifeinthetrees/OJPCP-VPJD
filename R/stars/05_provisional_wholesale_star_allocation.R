# =============================================================================
# VPJD-OJPCP
# R/stars/05_provisional_wholesale_star_allocation.R
# Version 0.1.4
#
# FIRST PROVISIONAL WHOLESALE STAR ALLOCATION
#
# PURPOSE
# - Allocate a provisional Star across all 11,439 accepted contemporary taxa.
# - Stress-test the current reconstructed Key.
# - Retain unresolved taxa rather than force unsupported classifications.
# - Establish a benchmark against which subsequent improvements can be measured.
#
# STAR CATEGORIES
# BK  Black
# GD  Gold
# BU  Blue
# GN  Green
# GX  Introduced to Japan
# HYB Hybrid
#
# PRECEDENCE
# 1. GX: positive introduced-to-Japan evidence
# 2. HYB: validated accepted hybrid-name rule
# 3. Geographic Key
#
# GEOGRAPHIC INPUTS
# - Endemic BK/GD candidates: Stars 03w
# - Broad non-endemic GN: Stars 04b
# - Restricted matrix invariant GN: Stars 04a
#
# IMPORTANT
# - Accepted WCVP taxa only.
# - Historical Star allocations are not analytical inputs.
# - Stars 03z classifications are not used.
# - No unresolved taxon is forced into a Star.
# - PROVISIONAL_STAR is a benchmark, not the final publication field.
# =============================================================================

suppressPackageStartupMessages({
  library(DBI)
  library(duckdb)
  library(dplyr)
  library(readr)
  library(tibble)
  library(here)
})

MODULE <- "stars_05_provisional_wholesale_star_allocation"
VERSION <- "0.1.4"
RUN_DATE <- Sys.Date()

DB_PATH <- here(
  "data","interim","occurrences",
  "vpjd_occurrences.duckdb"
)

OUTPUT_DIR <- here(
  "outputs","tables","stars",
  "provisional_wholesale_allocation"
)

TABLE_ALLOCATION <- "vpjd_star_provisional_wholesale_allocation"
TABLE_QUARANTINE <- "vpjd_star_provisional_wholesale_quarantine"
TABLE_PROFILE <- "vpjd_star_provisional_wholesale_profile"
TABLE_METHOD <- "vpjd_star_05_method_register"
TABLE_VALIDATION <- "vpjd_star_05_validation"
TABLE_METADATA <- "vpjd_star_05_metadata"

run_stars_05 <- function() {
  
  cat("\n— Provisional wholesale Star allocation —\n\n")
  cat("Run date: ",as.character(RUN_DATE),"\n",sep="")
  cat("Module: ",MODULE,"\n",sep="")
  cat("Version: ",VERSION,"\n\n",sep="")
  
  dir.create(
    OUTPUT_DIR,
    recursive=TRUE,
    showWarnings=FALSE
  )
  
  if (!file.exists(DB_PATH)) {
    stop("VPJD DuckDB not found: ",DB_PATH)
  }
  
  con <- dbConnect(
    duckdb::duckdb(),
    dbdir=DB_PATH,
    read_only=FALSE
  )
  
  on.exit(
    dbDisconnect(con,shutdown=TRUE),
    add=TRUE
  )
  
  # ===========================================================================
  # 1. REQUIRED TABLES
  # ===========================================================================
  
  required_tables <- c(
    "vpjd_japan_taxon_distribution",
    "vpjd_star_wcvp_key_geography",
    "vpjd_star_hybrid_status",
    "vpjd_star_endemic_key_traversal",
    "vpjd_star_non_endemic_matrix_possibilities",
    "vpjd_star_non_endemic_broad_range_gn_audit",
    "vpjd_star_04a_validation",
    "vpjd_star_04b_validation"
  )
  
  missing_tables <- setdiff(
    required_tables,
    dbListTables(con)
  )
  
  if (length(missing_tables)>0L) {
    stop(
      "Required upstream tables missing: ",
      paste(missing_tables,collapse=", ")
    )
  }
  
  # ===========================================================================
  # 2. VALIDATE 04a / 04b
  # ===========================================================================
  
  validation_04a <- as_tibble(
    dbGetQuery(
      con,
      "SELECT * FROM vpjd_star_04a_validation"
    )
  )
  
  validation_04b <- as_tibble(
    dbGetQuery(
      con,
      "SELECT * FROM vpjd_star_04b_validation"
    )
  )
  
  if (!"PASS" %in% names(validation_04a)) {
    stop("Stars 04a validation table has no PASS field.")
  }
  
  if (!"PASS" %in% names(validation_04b)) {
    stop("Stars 04b validation table has no PASS field.")
  }
  
  if (!all(validation_04a$PASS,na.rm=FALSE)) {
    stop("Stars 04a validation is not fully PASS.")
  }
  
  if (!all(validation_04b$PASS,na.rm=FALSE)) {
    stop("Stars 04b validation is not fully PASS.")
  }
  
  cat(
    "Stars 04a validation: ",
    sum(validation_04a$PASS,na.rm=TRUE),"/",
    nrow(validation_04a)," PASS\n",
    sep=""
  )
  
  cat(
    "Stars 04b validation: ",
    sum(validation_04b$PASS,na.rm=TRUE),"/",
    nrow(validation_04b)," PASS\n\n",
    sep=""
  )
  
  # ===========================================================================
  # 3. ACCEPTED POPULATION
  # ===========================================================================
  
  accepted <- as_tibble(
    dbGetQuery(
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
    )
  )
  
  if (nrow(accepted)!=11439L) {
    stop(
      "Expected 11,439 accepted taxa; found ",
      nrow(accepted)
    )
  }
  
  if (n_distinct(accepted$FINAL_WCVP_ID)!=11439L) {
    stop("Accepted population is not unique by WCVP ID.")
  }
  
  cat(
    "Accepted contemporary population: ",
    format(nrow(accepted),big.mark=","),
    "\n",
    sep=""
  )
  
  # ===========================================================================
  # 4. GX EVIDENCE
  # ===========================================================================
  
  gx_raw <- as_tibble(
    dbGetQuery(
      con,
      "SELECT * FROM vpjd_star_wcvp_key_geography"
    )
  )
  
  required_gx_fields <- c(
    "FINAL_WCVP_ID",
    "WCVP_INTRODUCED_TO_JAPAN"
  )
  
  missing_gx_fields <- setdiff(
    required_gx_fields,
    names(gx_raw)
  )
  
  if (length(missing_gx_fields)>0L) {
    stop(
      "Required 03c fields missing: ",
      paste(missing_gx_fields,collapse=", "),
      "\nAvailable fields: ",
      paste(names(gx_raw),collapse=", ")
    )
  }
  
  gx <- gx_raw |>
    select(
      FINAL_WCVP_ID,
      WCVP_INTRODUCED_TO_JAPAN
    ) |>
    distinct(
      FINAL_WCVP_ID,
      .keep_all=TRUE
    )
  
  if (nrow(gx)!=11439L) {
    stop(
      "Expected 11,439 03c evidence rows; found ",
      nrow(gx)
    )
  }
  
  cat(
    "03c introduction evidence: ",
    format(nrow(gx),big.mark=","),
    " taxa\n",
    sep=""
  )
  
  # ===========================================================================
  # 5. HYB EVIDENCE
  # ===========================================================================
  
  hybrid_raw <- as_tibble(
    dbGetQuery(
      con,
      "SELECT * FROM vpjd_star_hybrid_status"
    )
  )
  
  required_hybrid_fields <- c(
    "FINAL_WCVP_ID",
    "HYBRID_STATUS"
  )
  
  missing_hybrid_fields <- setdiff(
    required_hybrid_fields,
    names(hybrid_raw)
  )
  
  if (length(missing_hybrid_fields)>0L) {
    stop(
      "Required 03g fields missing: ",
      paste(missing_hybrid_fields,collapse=", "),
      "\nAvailable fields: ",
      paste(names(hybrid_raw),collapse=", ")
    )
  }
  
  hybrid <- hybrid_raw |>
    select(
      FINAL_WCVP_ID,
      HYBRID_STATUS
    ) |>
    distinct(
      FINAL_WCVP_ID,
      .keep_all=TRUE
    )
  
  if (nrow(hybrid)!=11439L) {
    stop(
      "Expected 11,439 hybrid evidence rows; found ",
      nrow(hybrid)
    )
  }
  
  cat(
    "03g hybrid evidence: ",
    format(nrow(hybrid),big.mark=","),
    " taxa\n",
    sep=""
  )
  
  # ===========================================================================
  # 6. ENDEMIC KEY
  # ===========================================================================
  
  endemic_raw <- as_tibble(
    dbGetQuery(
      con,
      "SELECT * FROM vpjd_star_endemic_key_traversal"
    )
  )
  
  required_endemic_fields <- c(
    "FINAL_WCVP_ID",
    "CANDIDATE_STAR"
  )
  
  missing_endemic_fields <- setdiff(
    required_endemic_fields,
    names(endemic_raw)
  )
  
  if (length(missing_endemic_fields)>0L) {
    stop(
      "Required 03w fields missing: ",
      paste(missing_endemic_fields,collapse=", "),
      "\nAvailable fields: ",
      paste(names(endemic_raw),collapse=", ")
    )
  }
  
  if (nrow(endemic_raw)!=2141L) {
    stop(
      "Expected 2,141 endemic-Key taxa; found ",
      nrow(endemic_raw)
    )
  }
  
  if (n_distinct(endemic_raw$FINAL_WCVP_ID)!=2141L) {
    stop("03w endemic population is not unique.")
  }
  
  endemic <- endemic_raw |>
    transmute(
      FINAL_WCVP_ID,
      ENDEMIC_CANDIDATE_STAR=CANDIDATE_STAR
    )
  
  cat(
    "03w endemic Key population: ",
    format(nrow(endemic),big.mark=","),
    " taxa\n",
    sep=""
  )
  
  endemic_detail_candidates <- c(
    "STOP_REASON",
    "REQUIRED_NEXT_EVIDENCE",
    "UNRESOLVED_REASON",
    "KEY_STOP_REASON",
    "ROUTE_STATUS",
    "TRAVERSAL_STATUS",
    "RESOLUTION_STATUS"
  )
  
  endemic_detail_field <- intersect(
    endemic_detail_candidates,
    names(endemic_raw)
  )
  
  if (length(endemic_detail_field)>0L) {
    
    chosen_endemic_detail <- endemic_detail_field[1]
    
    endemic_reason <- endemic_raw |>
      select(
        FINAL_WCVP_ID,
        all_of(chosen_endemic_detail)
      )
    
    names(endemic_reason)[2] <- "ENDEMIC_ROUTE_DETAIL"
    
    endemic_reason <- endemic_reason |>
      distinct(
        FINAL_WCVP_ID,
        .keep_all=TRUE
      )
    
    cat(
      "03w unresolved-detail field: ",
      chosen_endemic_detail,
      "\n",
      sep=""
    )
    
  } else {
    
    endemic_reason <- tibble(
      FINAL_WCVP_ID=endemic$FINAL_WCVP_ID,
      ENDEMIC_ROUTE_DETAIL=NA_character_
    )
    
    cat(
      "03w unresolved-detail field: none detected\n"
    )
  }
  
  # ===========================================================================
  # 7. RESTRICTED NON-ENDEMIC MATRIX
  # ===========================================================================
  
  matrix_raw <- as_tibble(
    dbGetQuery(
      con,
      "
      SELECT *
      FROM vpjd_star_non_endemic_matrix_possibilities
      "
    )
  )
  
  required_matrix_fields <- c(
    "FINAL_WCVP_ID",
    "MATRIX_REGION",
    "POSSIBLE_STARS",
    "N_POSSIBLE_STARS",
    "MATRIX_STAR_INVARIANT",
    "CANDIDATE_STAR",
    "REQUIRED_NEXT_EVIDENCE"
  )
  
  missing_matrix_fields <- setdiff(
    required_matrix_fields,
    names(matrix_raw)
  )
  
  if (length(missing_matrix_fields)>0L) {
    stop(
      "Required 04a fields missing: ",
      paste(missing_matrix_fields,collapse=", "),
      "\nAvailable fields: ",
      paste(names(matrix_raw),collapse=", ")
    )
  }
  
  if (nrow(matrix_raw)!=764L) {
    stop(
      "Expected 764 matrix taxa; found ",
      nrow(matrix_raw)
    )
  }
  
  if (n_distinct(matrix_raw$FINAL_WCVP_ID)!=764L) {
    stop("04a matrix population is not unique.")
  }
  
  matrix <- matrix_raw |>
    transmute(
      FINAL_WCVP_ID,
      MATRIX_REGION,
      MATRIX_POSSIBLE_STARS=POSSIBLE_STARS,
      MATRIX_N_POSSIBLE_STARS=N_POSSIBLE_STARS,
      MATRIX_STAR_INVARIANT,
      MATRIX_CANDIDATE_STAR=CANDIDATE_STAR,
      MATRIX_REQUIRED_NEXT_EVIDENCE=REQUIRED_NEXT_EVIDENCE
    )
  
  cat(
    "04a restricted matrix population: ",
    format(nrow(matrix),big.mark=","),
    " taxa\n",
    sep=""
  )
  
  # ===========================================================================
  # 8. BROAD NON-ENDEMIC GN
  # ===========================================================================
  
  broad_raw <- as_tibble(
    dbGetQuery(
      con,
      "
      SELECT *
      FROM vpjd_star_non_endemic_broad_range_gn_audit
      "
    )
  )
  
  required_broad_fields <- c(
    "FINAL_WCVP_ID",
    "BROAD_GN_METHOD_STATE",
    "CANDIDATE_STAR"
  )
  
  missing_broad_fields <- setdiff(
    required_broad_fields,
    names(broad_raw)
  )
  
  if (length(missing_broad_fields)>0L) {
    stop(
      "Required 04b fields missing: ",
      paste(missing_broad_fields,collapse=", "),
      "\nAvailable fields: ",
      paste(names(broad_raw),collapse=", ")
    )
  }
  
  if (nrow(broad_raw)!=7736L) {
    stop(
      "Expected 7,736 non-endemic taxa; found ",
      nrow(broad_raw)
    )
  }
  
  if (n_distinct(broad_raw$FINAL_WCVP_ID)!=7736L) {
    stop("04b non-endemic population is not unique.")
  }
  
  broad <- broad_raw |>
    transmute(
      FINAL_WCVP_ID,
      BROAD_GN_METHOD_STATE,
      BROAD_CANDIDATE_STAR=CANDIDATE_STAR
    )
  
  cat(
    "04b non-endemic population: ",
    format(nrow(broad),big.mark=","),
    " taxa\n\n",
    sep=""
  )
  
  # ===========================================================================
  # 9. BUILD WHOLE-POPULATION TABLE
  # ===========================================================================
  
  allocation <- accepted |>
    left_join(
      gx,
      by="FINAL_WCVP_ID"
    ) |>
    left_join(
      hybrid,
      by="FINAL_WCVP_ID"
    ) |>
    left_join(
      endemic,
      by="FINAL_WCVP_ID"
    ) |>
    left_join(
      endemic_reason,
      by="FINAL_WCVP_ID"
    ) |>
    left_join(
      matrix,
      by="FINAL_WCVP_ID"
    ) |>
    left_join(
      broad,
      by="FINAL_WCVP_ID"
    )
  
  # ===========================================================================
  # 10. EVIDENCE FLAGS
  #
  # Explicit TRUE/FALSE values are used here. Taxa outside a particular
  # upstream route are FALSE rather than NA.
  # ===========================================================================
  
  allocation <- allocation |>
    mutate(
      GX_POSITIVE=
        coalesce(
          WCVP_INTRODUCED_TO_JAPAN %in% TRUE,
          FALSE
        ),
      
      HYB_POSITIVE=
        coalesce(
          HYBRID_STATUS=="HYBRID",
          FALSE
        ),
      
      ENDEMIC_KEY_TAXON=
        FINAL_WCVP_ID %in%
        endemic$FINAL_WCVP_ID,
      
      MATRIX_NON_ENDEMIC=
        FINAL_WCVP_ID %in%
        matrix$FINAL_WCVP_ID,
      
      BROAD_NON_ENDEMIC_GN=
        coalesce(
          BROAD_GN_METHOD_STATE=="SOURCE_ROUTE_TO_GN" &
            BROAD_CANDIDATE_STAR=="GN",
          FALSE
        ),
      
      MATRIX_INVARIANT=
        coalesce(
          MATRIX_STAR_INVARIANT %in% TRUE,
          FALSE
        )
    )
  
  # ===========================================================================
  # 11. GEOGRAPHIC KEY RESULT
  # ===========================================================================
  
  allocation <- allocation |>
    mutate(
      GEOGRAPHIC_CANDIDATE_STAR=
        case_when(
          !is.na(ENDEMIC_CANDIDATE_STAR) ~
            ENDEMIC_CANDIDATE_STAR,
          
          BROAD_NON_ENDEMIC_GN ~
            "GN",
          
          MATRIX_INVARIANT &
            !is.na(MATRIX_CANDIDATE_STAR) ~
            MATRIX_CANDIDATE_STAR,
          
          TRUE ~
            NA_character_
        ),
      
      GEOGRAPHIC_SOURCE=
        case_when(
          !is.na(ENDEMIC_CANDIDATE_STAR) ~
            "STARS_03W_ENDEMIC_KEY",
          
          BROAD_NON_ENDEMIC_GN ~
            "STARS_04B_BROAD_NON_ENDEMIC",
          
          MATRIX_INVARIANT &
            !is.na(MATRIX_CANDIDATE_STAR) ~
            "STARS_04A_NON_ENDEMIC_MATRIX",
          
          MATRIX_NON_ENDEMIC ~
            "STARS_04A_MATRIX_UNRESOLVED",
          
          ENDEMIC_KEY_TAXON ~
            "STARS_03W_ENDEMIC_UNRESOLVED",
          
          TRUE ~
            "KEY_ROUTE_UNRESOLVED"
        )
    )
  
  # ===========================================================================
  # 12. PROVISIONAL STAR ALLOCATION
  # ===========================================================================
  
  allocation <- allocation |>
    mutate(
      PROVISIONAL_STAR=
        case_when(
          GX_POSITIVE ~
            "GX",
          
          !GX_POSITIVE &
            HYB_POSITIVE ~
            "HYB",
          
          !is.na(GEOGRAPHIC_CANDIDATE_STAR) ~
            GEOGRAPHIC_CANDIDATE_STAR,
          
          TRUE ~
            NA_character_
        ),
      
      ALLOCATION_SOURCE=
        case_when(
          GX_POSITIVE ~
            "CONTEMPORARY_GX_CROSSWALK",
          
          !GX_POSITIVE &
            HYB_POSITIVE ~
            "CONTEMPORARY_HYB_EXTENSION",
          
          !GX_POSITIVE &
            !HYB_POSITIVE &
            GEOGRAPHIC_SOURCE=="STARS_03W_ENDEMIC_KEY" ~
            "NAKAMURA_KEY_ENDEMIC",
          
          !GX_POSITIVE &
            !HYB_POSITIVE &
            GEOGRAPHIC_SOURCE=="STARS_04B_BROAD_NON_ENDEMIC" ~
            "NAKAMURA_KEY_NON_ENDEMIC_BROAD",
          
          !GX_POSITIVE &
            !HYB_POSITIVE &
            GEOGRAPHIC_SOURCE=="STARS_04A_NON_ENDEMIC_MATRIX" ~
            "NAKAMURA_KEY_NON_ENDEMIC_MATRIX",
          
          TRUE ~
            "UNRESOLVED"
        )
    )
  
  # ===========================================================================
  # 13. UNRESOLVED TAXA
  # ===========================================================================
  
  allocation <- allocation |>
    mutate(
      UNRESOLVED_REASON=
        case_when(
          !is.na(PROVISIONAL_STAR) ~
            NA_character_,
          
          MATRIX_NON_ENDEMIC &
            !MATRIX_INVARIANT ~
            coalesce(
              MATRIX_REQUIRED_NEXT_EVIDENCE,
              "NON_ENDEMIC_MATRIX_UNRESOLVED"
            ),
          
          ENDEMIC_KEY_TAXON &
            !is.na(ENDEMIC_ROUTE_DETAIL) ~
            paste0(
              "ENDEMIC_KEY: ",
              ENDEMIC_ROUTE_DETAIL
            ),
          
          ENDEMIC_KEY_TAXON ~
            "ENDEMIC_KEY_UNRESOLVED",
          
          TRUE ~
            "KEY_ROUTE_UNRESOLVED"
        ),
      
      POSSIBLE_STARS=
        case_when(
          !is.na(PROVISIONAL_STAR) ~
            PROVISIONAL_STAR,
          
          MATRIX_NON_ENDEMIC ~
            MATRIX_POSSIBLE_STARS,
          
          TRUE ~
            NA_character_
        ),
      
      IS_ALLOCATED=
        !is.na(PROVISIONAL_STAR),
      
      IS_UNRESOLVED=
        is.na(PROVISIONAL_STAR),
      
      DEFINITIVE_STAR=
        NA_character_,
      
      DEFINITIVE_STATUS=
        "NOT_YET_FINAL_CLASSIFIER"
    )
  
  if (nrow(allocation)!=11439L) {
    stop("Wholesale output does not contain 11,439 taxa.")
  }
  
  if (n_distinct(allocation$FINAL_WCVP_ID)!=11439L) {
    stop("Wholesale output is not unique by WCVP ID.")
  }
  
  # ===========================================================================
  # 14. QUARANTINE
  # ===========================================================================
  
  quarantine <- allocation |>
    filter(
      IS_UNRESOLVED
    ) |>
    select(
      FINAL_WCVP_ID,
      FINAL_WCVP_RECOGNISED_NAME,
      FINAL_WCVP_RANK,
      UNRESOLVED_REASON,
      POSSIBLE_STARS,
      MATRIX_REGION,
      MATRIX_POSSIBLE_STARS,
      MATRIX_REQUIRED_NEXT_EVIDENCE,
      ENDEMIC_ROUTE_DETAIL
    )
  
  # ===========================================================================
  # 15. STAR COUNTS
  # ===========================================================================
  
  n_bk <- sum(
    allocation$PROVISIONAL_STAR=="BK",
    na.rm=TRUE
  )
  
  n_gd <- sum(
    allocation$PROVISIONAL_STAR=="GD",
    na.rm=TRUE
  )
  
  n_bu <- sum(
    allocation$PROVISIONAL_STAR=="BU",
    na.rm=TRUE
  )
  
  n_gn <- sum(
    allocation$PROVISIONAL_STAR=="GN",
    na.rm=TRUE
  )
  
  n_gx <- sum(
    allocation$PROVISIONAL_STAR=="GX",
    na.rm=TRUE
  )
  
  n_hyb <- sum(
    allocation$PROVISIONAL_STAR=="HYB",
    na.rm=TRUE
  )
  
  n_allocated <- sum(
    allocation$IS_ALLOCATED,
    na.rm=TRUE
  )
  
  n_unresolved <- sum(
    allocation$IS_UNRESOLVED,
    na.rm=TRUE
  )
  
  gx_positive_n <- sum(
    allocation$GX_POSITIVE,
    na.rm=TRUE
  )
  
  hyb_positive_n <- sum(
    allocation$HYB_POSITIVE,
    na.rm=TRUE
  )
  
  gx_hyb_overlap <- sum(
    allocation$GX_POSITIVE &
      allocation$HYB_POSITIVE,
    na.rm=TRUE
  )
  
  endemic_candidate_n <- sum(
    !is.na(
      allocation$ENDEMIC_CANDIDATE_STAR
    ),
    na.rm=TRUE
  )
  
  matrix_invariant_n <- sum(
    allocation$MATRIX_INVARIANT,
    na.rm=TRUE
  )
  
  broad_gn_n <- sum(
    allocation$BROAD_NON_ENDEMIC_GN,
    na.rm=TRUE
  )
  
  # ===========================================================================
  # 16. CLEAR STAR RATING ALLOCATION TABLE
  # ===========================================================================
  
  star_allocation_table <- tibble(
    STAR=c(
      "BK",
      "GD",
      "BU",
      "GN",
      "GX",
      "HYB",
      "UNRESOLVED"
    ),
    
    RATING=c(
      "Black",
      "Gold",
      "Blue",
      "Green",
      "Introduced",
      "Hybrid",
      "Unresolved"
    ),
    
    N_TAXA=c(
      n_bk,
      n_gd,
      n_bu,
      n_gn,
      n_gx,
      n_hyb,
      n_unresolved
    )
  ) |>
    mutate(
      PERCENT_ACCEPTED=
        round(
          100*N_TAXA/nrow(allocation),
          3
        )
    )
  
  star_allocation_total <- tibble(
    STAR="TOTAL",
    RATING="Accepted taxa",
    N_TAXA=nrow(allocation),
    PERCENT_ACCEPTED=100
  )
  
  star_allocation_display <- bind_rows(
    star_allocation_table,
    star_allocation_total
  )
  
  # ===========================================================================
  # 17. ADDITIONAL PROFILES
  # ===========================================================================
  
  source_profile <- allocation |>
    count(
      ALLOCATION_SOURCE,
      PROVISIONAL_STAR,
      name="N_TAXA",
      sort=TRUE
    )
  
  unresolved_profile <- quarantine |>
    count(
      UNRESOLVED_REASON,
      POSSIBLE_STARS,
      name="N_TAXA",
      sort=TRUE
    )
  
  overlap_profile <- allocation |>
    count(
      GX_POSITIVE,
      HYB_POSITIVE,
      name="N_TAXA",
      sort=TRUE
    )
  
  rank_profile <- allocation |>
    mutate(
      STAR_DISPLAY=
        coalesce(
          PROVISIONAL_STAR,
          "UNRESOLVED"
        )
    ) |>
    count(
      FINAL_WCVP_RANK,
      STAR_DISPLAY,
      name="N_TAXA",
      sort=TRUE
    )
  
  # ===========================================================================
  # 18. METHOD REGISTER
  # ===========================================================================
  
  method_register <- tribble(
    ~METHOD_ID,
    ~COMPONENT,
    ~OPERATIONAL_RULE,
    ~STATUS,
    
    "S05-01",
    "Population",
    "Exactly 11,439 accepted contemporary WCVP taxa.",
    "VALIDATED",
    
    "S05-02",
    "GX",
    paste0(
      "Positive WCVP introduced-to-Japan evidence produces provisional GX. ",
      "This is the contemporary operational crosswalk to the source ",
      "cultivated-species branch."
    ),
    "CONTEMPORARY_CROSSWALK",
    
    "S05-03",
    "HYB",
    paste0(
      "Accepted taxa satisfying the validated Genus × species rule produce ",
      "provisional HYB."
    ),
    "CONTEMPORARY_EXTENSION",
    
    "S05-04",
    "Precedence",
    "GX > HYB > geographic Key.",
    "PROVISIONAL_BENCHMARK_RULE",
    
    "S05-05",
    "Endemic Key",
    "Use only Stars 03w candidate BK/GD terminals.",
    "VALIDATED_INPUT",
    
    "S05-06",
    "Broad non-endemic",
    "Use only Stars 04b SOURCE_ROUTE_TO_GN candidate GN.",
    "VALIDATED_INPUT",
    
    "S05-07",
    "Restricted matrix",
    "Use only Stars 04a matrix-invariant candidate Stars.",
    "VALIDATED_INPUT",
    
    "S05-08",
    "Unresolved taxa",
    "Retain as NA; do not force an allocation.",
    "VALIDATED",
    
    "S05-09",
    "Stars 03z",
    "Stars 03z classifications are not used.",
    "SUPERSEDED",
    
    "S05-10",
    "Historical allocations",
    "Historical Star allocations are not analytical inputs.",
    "PROHIBITED",
    
    "S05-11",
    "Benchmark",
    "PROVISIONAL_STAR is not the definitive publication Star field.",
    "PROVISIONAL"
  )
  
  # ===========================================================================
  # 19. VALIDATION
  # ===========================================================================
  
  valid_stars <- c(
    "BK","GD","BU","GN","GX","HYB"
  )
  
  validation <- tribble(
    ~CHECK, ~PASS,
    
    "Stars 04a validation fully PASS",
    all(validation_04a$PASS,na.rm=FALSE),
    
    "Stars 04b validation fully PASS",
    all(validation_04b$PASS,na.rm=FALSE),
    
    "Accepted population = 11,439",
    nrow(allocation)==11439L,
    
    "Accepted population unique by WCVP ID",
    n_distinct(allocation$FINAL_WCVP_ID)==11439L,
    
    "All population records are Accepted",
    all(allocation$FINAL_WCVP_STATUS=="Accepted"),
    
    "03c evidence population = 11,439",
    nrow(gx)==11439L,
    
    "03g evidence population = 11,439",
    nrow(hybrid)==11439L,
    
    "03w endemic population = 2,141",
    nrow(endemic)==2141L,
    
    "04a matrix population = 764",
    nrow(matrix)==764L,
    
    "04b non-endemic population = 7,736",
    nrow(broad)==7736L,
    
    "GX positive evidence = 1,120",
    gx_positive_n==1120L,
    
    "HYB positive evidence = 456",
    hyb_positive_n==456L,
    
    "GX/HYB evidence overlap = 16",
    gx_hyb_overlap==16L,
    
    "GX precedence leaves 440 HYB",
    n_hyb==440L,
    
    "Endemic candidate terminals = 1,329",
    endemic_candidate_n==1329L,
    
    "Broad non-endemic GN evidence = 6,972",
    broad_gn_n==6972L,
    
    "Matrix-invariant candidate terminals = 403",
    matrix_invariant_n==403L,
    
    "Evidence flags contain no NA values",
    !anyNA(
      allocation |>
        select(
          GX_POSITIVE,
          HYB_POSITIVE,
          ENDEMIC_KEY_TAXON,
          MATRIX_NON_ENDEMIC,
          BROAD_NON_ENDEMIC_GN,
          MATRIX_INVARIANT
        )
    ),
    
    "Provisional Stars restricted to valid categories",
    all(
      allocation$PROVISIONAL_STAR[
        !is.na(allocation$PROVISIONAL_STAR)
      ] %in% valid_stars
    ),
    
    "Every allocated taxon has a provisional Star",
    all(
      !is.na(
        allocation$PROVISIONAL_STAR[
          allocation$IS_ALLOCATED
        ]
      )
    ),
    
    "Every unresolved taxon has no provisional Star",
    all(
      is.na(
        allocation$PROVISIONAL_STAR[
          allocation$IS_UNRESOLVED
        ]
      )
    ),
    
    "Quarantine equals unresolved population",
    nrow(quarantine)==n_unresolved,
    
    "Allocated plus unresolved = 11,439",
    n_allocated+n_unresolved==11439L,
    
    "Star totals plus unresolved = 11,439",
    n_bk+n_gd+n_bu+n_gn+n_gx+n_hyb+n_unresolved==
      11439L,
    
    "Star allocation table totals 11,439",
    sum(star_allocation_table$N_TAXA)==11439L,
    
    "No definitive Stars assigned",
    all(is.na(allocation$DEFINITIVE_STAR))
  ) |>
    mutate(
      PASS=coalesce(PASS,FALSE),
      RESULT=if_else(
        PASS,
        "PASS",
        "FAIL"
      )
    )
  
  # ===========================================================================
  # 20. DISPLAY STAR ALLOCATION
  # ===========================================================================
  
  cat(
    "\n============================================================\n"
  )
  
  cat(
    "PROVISIONAL STAR RATING ALLOCATION\n"
  )
  
  cat(
    "============================================================\n\n"
  )
  
  print(
    star_allocation_display,
    n=Inf
  )
  
  cat(
    "\nAllocated taxa: ",
    format(n_allocated,big.mark=","),
    " / ",
    format(nrow(allocation),big.mark=","),
    " (",
    round(
      100*n_allocated/nrow(allocation),
      2
    ),
    "%)\n",
    sep=""
  )
  
  cat(
    "Unresolved taxa: ",
    format(n_unresolved,big.mark=","),
    " / ",
    format(nrow(allocation),big.mark=","),
    " (",
    round(
      100*n_unresolved/nrow(allocation),
      2
    ),
    "%)\n",
    sep=""
  )
  
  cat(
    "\n— ALLOCATION SOURCE PROFILE —\n"
  )
  
  print(
    source_profile,
    n=Inf
  )
  
  cat(
    "\n— GX / HYB EVIDENCE OVERLAP —\n"
  )
  
  print(
    overlap_profile,
    n=Inf
  )
  
  cat(
    "\n— UNRESOLVED PROFILE —\n"
  )
  
  if (nrow(unresolved_profile)>0L) {
    
    print(
      unresolved_profile,
      n=Inf
    )
    
  } else {
    
    cat(
      "No unresolved taxa.\n"
    )
  }
  
  cat(
    "\n— VALIDATION —\n"
  )
  
  print(
    validation,
    n=Inf
  )
  
  if (!all(validation$PASS)) {
    stop(
      "Stars 05 validation failed. ",
      "Canonical Stars 05 tables have NOT been written."
    )
  }
  
  # ===========================================================================
  # 21. METADATA
  # ===========================================================================
  
  metadata <- tibble(
    MODULE=MODULE,
    VERSION=VERSION,
    RUN_DATE=as.character(RUN_DATE),
    
    POPULATION=
      nrow(allocation),
    
    BK=
      n_bk,
    
    GD=
      n_gd,
    
    BU=
      n_bu,
    
    GN=
      n_gn,
    
    GX=
      n_gx,
    
    HYB=
      n_hyb,
    
    ALLOCATED=
      n_allocated,
    
    UNRESOLVED=
      n_unresolved,
    
    PERCENT_ALLOCATED=
      round(
        100*n_allocated/nrow(allocation),
        3
      ),
    
    GX_HYB_OVERLAP=
      gx_hyb_overlap,
    
    ENDEMIC_CANDIDATE_TERMINALS=
      endemic_candidate_n,
    
    MATRIX_INVARIANT_TERMINALS=
      matrix_invariant_n,
    
    BROAD_NON_ENDEMIC_GN=
      broad_gn_n,
    
    HISTORICAL_ALLOCATIONS_USED=
      FALSE,
    
    STARS_03Z_CLASSIFICATIONS_USED=
      FALSE,
    
    DEFINITIVE_CLASSIFIER=
      FALSE,
    
    STATUS=
      "VALIDATED_PROVISIONAL_WHOLESALE_BENCHMARK"
  )
  
  # ===========================================================================
  # 22. WRITE CANONICAL TABLES
  # ===========================================================================
  
  dbWriteTable(
    con,
    TABLE_ALLOCATION,
    allocation,
    overwrite=TRUE
  )
  
  dbWriteTable(
    con,
    TABLE_QUARANTINE,
    quarantine,
    overwrite=TRUE
  )
  
  dbWriteTable(
    con,
    TABLE_PROFILE,
    star_allocation_table,
    overwrite=TRUE
  )
  
  dbWriteTable(
    con,
    TABLE_METHOD,
    method_register,
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
  
  # ===========================================================================
  # 23. WRITE CSV OUTPUTS
  # ===========================================================================
  
  write_csv(
    allocation,
    file.path(
      OUTPUT_DIR,
      "stars05_provisional_wholesale_allocation.csv"
    )
  )
  
  write_csv(
    quarantine,
    file.path(
      OUTPUT_DIR,
      "stars05_provisional_wholesale_quarantine.csv"
    )
  )
  
  write_csv(
    star_allocation_display,
    file.path(
      OUTPUT_DIR,
      "stars05_star_rating_allocation.csv"
    )
  )
  
  write_csv(
    source_profile,
    file.path(
      OUTPUT_DIR,
      "stars05_allocation_source_profile.csv"
    )
  )
  
  write_csv(
    unresolved_profile,
    file.path(
      OUTPUT_DIR,
      "stars05_unresolved_profile.csv"
    )
  )
  
  write_csv(
    overlap_profile,
    file.path(
      OUTPUT_DIR,
      "stars05_gx_hyb_overlap.csv"
    )
  )
  
  write_csv(
    rank_profile,
    file.path(
      OUTPUT_DIR,
      "stars05_rank_profile.csv"
    )
  )
  
  write_csv(
    method_register,
    file.path(
      OUTPUT_DIR,
      "stars05_method_register.csv"
    )
  )
  
  write_csv(
    validation,
    file.path(
      OUTPUT_DIR,
      "stars05_validation.csv"
    )
  )
  
  write_csv(
    metadata,
    file.path(
      OUTPUT_DIR,
      "stars05_metadata.csv"
    )
  )
  
  # ===========================================================================
  # 24. FINAL SUMMARY
  # ===========================================================================
  
  cat(
    "\n============================================================\n"
  )
  
  cat(
    "Stars 05 v",VERSION," COMPLETE\n",
    sep=""
  )
  
  cat(
    "============================================================\n\n"
  )
  
  cat(
    "FINAL PROVISIONAL BENCHMARK:\n\n"
  )
  
  print(
    star_allocation_display,
    n=Inf
  )
  
  cat(
    "\nAllocated: ",
    format(n_allocated,big.mark=","),
    " (",
    round(
      100*n_allocated/nrow(allocation),
      2
    ),
    "%)\n",
    sep=""
  )
  
  cat(
    "Unresolved: ",
    format(n_unresolved,big.mark=","),
    " (",
    round(
      100*n_unresolved/nrow(allocation),
      2
    ),
    "%)\n",
    sep=""
  )
  
  cat(
    "GX/HYB evidence overlap: ",
    format(gx_hyb_overlap,big.mark=","),
    "\n",
    sep=""
  )
  
  cat(
    "Endemic candidate terminals before precedence: ",
    format(endemic_candidate_n,big.mark=","),
    "\n",
    sep=""
  )
  
  cat(
    "Matrix-invariant terminals before precedence: ",
    format(matrix_invariant_n,big.mark=","),
    "\n",
    sep=""
  )
  
  cat(
    "Broad non-endemic GN before precedence: ",
    format(broad_gn_n,big.mark=","),
    "\n",
    sep=""
  )
  
  cat(
    "Historical allocations used: FALSE\n"
  )
  
  cat(
    "Stars 03z classifications used: FALSE\n"
  )
  
  cat(
    "Definitive classifier: FALSE\n"
  )
  
  cat(
    "Validation: ",
    sum(validation$PASS),"/",
    nrow(validation)," PASS\n",
    sep=""
  )
  
  cat(
    "Canonical allocation: ",
    TABLE_ALLOCATION,
    "\n",
    sep=""
  )
  
  cat(
    "Canonical quarantine: ",
    TABLE_QUARANTINE,
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
      allocation=allocation,
      quarantine=quarantine,
      star_allocation=star_allocation_display,
      source_profile=source_profile,
      unresolved_profile=unresolved_profile,
      overlap_profile=overlap_profile,
      rank_profile=rank_profile,
      method_register=method_register,
      validation=validation,
      metadata=metadata
    )
  )
}

stars_05_result <- run_stars_05()