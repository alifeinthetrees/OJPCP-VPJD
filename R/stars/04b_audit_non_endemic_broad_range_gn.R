# =============================================================================
# VPJD-OJPCP
# R/stars/04b_audit_non_endemic_broad_range_gn.R
# Version 0.1.2
#
# AUDIT NON-ENDEMIC BROAD-RANGE GN ELIGIBILITY
#
# PURPOSE
# - Audit the 7,736 taxa entering the non-endemic branch.
# - Distinguish restricted single-region matrix taxa from broad-range taxa.
# - Do NOT import Stars 03z classifications.
# - Independently cross-check the 764 Stars 04a matrix taxa.
# - Retain candidate GN only where the N02/N02A source route supports it.
#
# SOURCE
# Nakamura (2012), Figure 2.2, p.48
#
# N02:
# "Apart from Japan, only in one of Taiwan, Korea, OR the Kuriles &
#  Sakhalin, use the Table"
#
# N02A:
# "Not only in one of Taiwan or Korea OR the Kuriles & Sakhalin,
#  and beyond = GREEN."
#
# IMPORTANT
# - China occurs explicitly in the source matrix and is therefore retained
#   as a matrix region when it is the sole external region.
# - One named matrix region + beyond evidence is broad range, not matrix.
# - Two or more named matrix regions are broad range.
# - No definitive Stars are assigned here.
# - Historical Star allocations are not used.
# =============================================================================

suppressPackageStartupMessages({
  library(DBI)
  library(duckdb)
  library(dplyr)
  library(readr)
  library(tibble)
  library(here)
})

MODULE <- "stars_04b_audit_non_endemic_broad_range_gn"
VERSION <- "0.1.2"
RUN_DATE <- Sys.Date()

DB_PATH <- here(
  "data","interim","occurrences",
  "vpjd_occurrences.duckdb"
)

OUTPUT_DIR <- here(
  "outputs","tables","stars",
  "non_endemic_broad_range_gn_audit"
)

TABLE_AUDIT <- "vpjd_star_non_endemic_broad_range_gn_audit"
TABLE_METHOD <- "vpjd_star_non_endemic_broad_range_method_register"
TABLE_PROFILE <- "vpjd_star_non_endemic_broad_range_gn_profile"
TABLE_VALIDATION <- "vpjd_star_04b_validation"
TABLE_METADATA <- "vpjd_star_04b_metadata"

run_stars_04b <- function() {
  
  cat("\n— Audit non-endemic broad-range GN eligibility —\n\n")
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
  
  required_tables <- c(
    "vpjd_star_non_endemic_key_logic",
    "vpjd_star_non_endemic_geography_evidence",
    "vpjd_star_non_endemic_matrix_possibilities",
    "vpjd_star_03p_validation",
    "vpjd_star_03r_validation",
    "vpjd_star_03y_validation",
    "vpjd_star_04a_validation"
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
  
  get_validation <- function(table_name) {
    as_tibble(
      dbGetQuery(
        con,
        paste0("SELECT * FROM ",table_name)
      )
    )
  }
  
  validation_03p <- get_validation(
    "vpjd_star_03p_validation"
  )
  
  validation_03r <- get_validation(
    "vpjd_star_03r_validation"
  )
  
  validation_03y <- get_validation(
    "vpjd_star_03y_validation"
  )
  
  validation_04a <- get_validation(
    "vpjd_star_04a_validation"
  )
  
  if (!all(validation_03p$PASS)) {
    stop("Stars 03p is not fully validated.")
  }
  
  if (!all(validation_03r$PASS)) {
    stop("Stars 03r is not fully validated.")
  }
  
  if (!all(validation_03y$PASS)) {
    stop("Stars 03y is not fully validated.")
  }
  
  if (!all(validation_04a$PASS)) {
    stop("Stars 04a is not fully validated.")
  }
  
  cat(
    "Stars 03p validation: ",
    sum(validation_03p$PASS),"/",
    nrow(validation_03p)," PASS\n",
    sep=""
  )
  
  cat(
    "Stars 03r validation: ",
    sum(validation_03r$PASS),"/",
    nrow(validation_03r)," PASS\n",
    sep=""
  )
  
  cat(
    "Stars 03y validation: ",
    sum(validation_03y$PASS),"/",
    nrow(validation_03y)," PASS\n",
    sep=""
  )
  
  cat(
    "Stars 04a validation: ",
    sum(validation_04a$PASS),"/",
    nrow(validation_04a)," PASS\n\n",
    sep=""
  )
  
  # ---------------------------------------------------------------------------
  # Load validated upstream evidence
  # ---------------------------------------------------------------------------
  
  logic <- as_tibble(
    dbGetQuery(
      con,
      "SELECT * FROM vpjd_star_non_endemic_key_logic"
    )
  )
  
  evidence <- as_tibble(
    dbGetQuery(
      con,
      "SELECT * FROM vpjd_star_non_endemic_geography_evidence"
    )
  )
  
  matrix_04a <- as_tibble(
    dbGetQuery(
      con,
      "SELECT * FROM vpjd_star_non_endemic_matrix_possibilities"
    )
  )
  
  required_logic_fields <- c(
    "FINAL_WCVP_ID",
    "FINAL_WCVP_RECOGNISED_NAME",
    "NON_ENDEMIC_LOGIC_CLASS"
  )
  
  missing_logic_fields <- setdiff(
    required_logic_fields,
    names(logic)
  )
  
  if (length(missing_logic_fields)>0L) {
    stop(
      "Required 03y fields missing: ",
      paste(missing_logic_fields,collapse=", "),
      "\nAvailable fields: ",
      paste(names(logic),collapse=", ")
    )
  }
  
  required_evidence_fields <- c(
    "FINAL_WCVP_ID",
    "WCVP_TAIWAN",
    "WCVP_KOREA",
    "WCVP_KURILES_SAKHALIN",
    "WCVP_CHINA",
    "WCVP_BEYOND_KEY_REGIONS",
    "N_NAMED_NEIGHBOUR_REGIONS",
    "MULTIPLE_NAMED_NEIGHBOUR_REGIONS",
    "NO_NAMED_NEIGHBOUR_REGION",
    "BROADER_EXTERNAL_RANGE_EVIDENCE",
    "BROAD_RANGE_EVIDENCE_REVIEW_REQUIRED"
  )
  
  missing_evidence_fields <- setdiff(
    required_evidence_fields,
    names(evidence)
  )
  
  if (length(missing_evidence_fields)>0L) {
    stop(
      "Required 03r evidence fields missing: ",
      paste(missing_evidence_fields,collapse=", "),
      "\nAvailable fields: ",
      paste(names(evidence),collapse=", ")
    )
  }
  
  if (nrow(logic)!=7736L) {
    stop(
      "Expected 7,736 non-endemic taxa; found ",
      nrow(logic)
    )
  }
  
  if (n_distinct(logic$FINAL_WCVP_ID)!=7736L) {
    stop("03y non-endemic population is not unique.")
  }
  
  if (nrow(evidence)!=7736L) {
    stop(
      "Expected 7,736 rows of 03r evidence; found ",
      nrow(evidence)
    )
  }
  
  if (n_distinct(evidence$FINAL_WCVP_ID)!=7736L) {
    stop("03r non-endemic evidence is not unique.")
  }
  
  if (nrow(matrix_04a)!=764L) {
    stop(
      "Expected 764 Stars 04a matrix taxa; found ",
      nrow(matrix_04a)
    )
  }
  
  if (n_distinct(matrix_04a$FINAL_WCVP_ID)!=764L) {
    stop("Stars 04a matrix population is not unique.")
  }
  
  # ---------------------------------------------------------------------------
  # Build complete non-endemic audit population
  # ---------------------------------------------------------------------------
  
  evidence_selected <- evidence |>
    select(
      all_of(required_evidence_fields)
    )
  
  audit <- logic |>
    select(
      FINAL_WCVP_ID,
      FINAL_WCVP_RECOGNISED_NAME,
      NON_ENDEMIC_LOGIC_CLASS
    ) |>
    left_join(
      evidence_selected,
      by="FINAL_WCVP_ID"
    ) |>
    mutate(
      NAMED_REGION_COUNT=
        as.integer(WCVP_TAIWAN %in% TRUE) +
        as.integer(WCVP_KOREA %in% TRUE) +
        as.integer(WCVP_KURILES_SAKHALIN %in% TRUE) +
        as.integer(WCVP_CHINA %in% TRUE),
      
      HAS_BEYOND_EVIDENCE=
        WCVP_BEYOND_KEY_REGIONS %in% TRUE,
      
      HAS_MULTIPLE_NAMED_REGIONS=
        NAMED_REGION_COUNT>=2L,
      
      SINGLE_NAMED_REGION=
        NAMED_REGION_COUNT==1L,
      
      NO_NAMED_REGION=
        NAMED_REGION_COUNT==0L,
      
      RESTRICTED_SINGLE_REGION=
        SINGLE_NAMED_REGION &
        !HAS_BEYOND_EVIDENCE,
      
      SINGLE_REGION_PLUS_BEYOND=
        SINGLE_NAMED_REGION &
        HAS_BEYOND_EVIDENCE,
      
      IS_04A_MATRIX_TAXON=
        FINAL_WCVP_ID %in%
        matrix_04a$FINAL_WCVP_ID
    )
  
  # ---------------------------------------------------------------------------
  # Source-route audit
  #
  # Restricted matrix:
  # exactly one named matrix region + no beyond evidence.
  #
  # Broad GN:
  # beyond evidence OR >=2 named matrix regions.
  # ---------------------------------------------------------------------------
  
  audit <- audit |>
    mutate(
      SOURCE_RESTRICTION_STATE=
        case_when(
          BROAD_RANGE_EVIDENCE_REVIEW_REQUIRED %in% TRUE ~
            "EVIDENCE_REVIEW",
          
          HAS_BEYOND_EVIDENCE ~
            "NOT_ONLY_ONE_MATRIX_REGION_BEYOND",
          
          HAS_MULTIPLE_NAMED_REGIONS ~
            "NOT_ONLY_ONE_MATRIX_REGION_MULTIPLE",
          
          RESTRICTED_SINGLE_REGION ~
            "ONLY_ONE_MATRIX_REGION",
          
          NO_NAMED_REGION ~
            "NO_MATRIX_REGION_WITHOUT_BEYOND",
          
          TRUE ~
            "EVIDENCE_REVIEW"
        ),
      
      BROAD_GN_METHOD_STATE=
        case_when(
          SOURCE_RESTRICTION_STATE==
            "NOT_ONLY_ONE_MATRIX_REGION_BEYOND" ~
            "SOURCE_ROUTE_TO_GN",
          
          SOURCE_RESTRICTION_STATE==
            "NOT_ONLY_ONE_MATRIX_REGION_MULTIPLE" ~
            "SOURCE_ROUTE_TO_GN",
          
          SOURCE_RESTRICTION_STATE==
            "ONLY_ONE_MATRIX_REGION" ~
            "CONTINUE_TO_MATRIX",
          
          TRUE ~
            "EVIDENCE_REVIEW"
        ),
      
      CANDIDATE_STAR=
        if_else(
          BROAD_GN_METHOD_STATE=="SOURCE_ROUTE_TO_GN",
          "GN",
          NA_character_
        ),
      
      NEXT_KEY_NODE=
        case_when(
          BROAD_GN_METHOD_STATE=="CONTINUE_TO_MATRIX" ~
            "NON_ENDEMIC_MATRIX",
          
          BROAD_GN_METHOD_STATE=="EVIDENCE_REVIEW" ~
            "BROAD_EXTERNAL_RANGE_EVIDENCE",
          
          TRUE ~
            NA_character_
        ),
      
      DEFINITIVE_STAR=
        NA_character_,
      
      STAR_ASSIGNED=
        FALSE
    )
  
  # ---------------------------------------------------------------------------
  # Method register
  # ---------------------------------------------------------------------------
  
  method_register <- tribble(
    ~METHOD_ID,
    ~KEY_COMPONENT,
    ~SOURCE_TEXT_OR_STRUCTURE,
    ~OPERATIONAL_DEFINITION,
    ~METHOD_STATUS,
    
    "BRGN01",
    "Only one matrix region",
    "Figure 2.2 N02 plus explicit non-endemic matrix",
    paste0(
      "Positive external evidence is confined to exactly one of the ",
      "matrix regions Taiwan, Korea, Kuriles/Sakhalin or China, with ",
      "no validated evidence beyond those regions. Continue to matrix."
    ),
    "METHOD_VALIDATED",
    
    "BRGN02",
    "One matrix region plus beyond",
    "Figure 2.2 N02/N02A contrast",
    paste0(
      "Positive external evidence occurs in exactly one named matrix ",
      "region plus validated evidence beyond the matrix regions. ",
      "The taxon follows the broad GREEN branch."
    ),
    "METHOD_VALIDATED",
    
    "BRGN03",
    "Multiple matrix regions",
    "Figure 2.2 N02/N02A contrast",
    paste0(
      "Positive external evidence occurs in two or more matrix regions. ",
      "The taxon is not confined to only one matrix region and follows ",
      "the broad GREEN branch."
    ),
    "METHOD_VALIDATED",
    
    "BRGN04",
    "Beyond matrix regions",
    "Figure 2.2 N02A: and beyond = GREEN",
    paste0(
      "Validated positive WCVP evidence occurs outside Japan, Taiwan, ",
      "Korea, Kuriles, Sakhalin and China. The taxon follows the ",
      "broad GREEN branch."
    ),
    "METHOD_VALIDATED",
    
    "BRGN05",
    "China treatment",
    "China appears explicitly as two rows in Figure 2.2 matrix",
    paste0(
      "China-only taxa remain matrix taxa. China is not automatically ",
      "interpreted as beyond."
    ),
    "METHOD_VALIDATED",
    
    "BRGN06",
    "Stars 03z status",
    "Stars 03z v0.1.0",
    paste0(
      "Stars 03z is superseded as a classification dependency. ",
      "Its candidate Star assignments are not imported."
    ),
    "SUPERSEDED_CLASSIFIER",
    
    "BRGN07",
    "Historical allocations",
    "Project reproducibility rule",
    paste0(
      "Historical Star allocations do not determine contemporary ",
      "routing or Star category."
    ),
    "PROHIBITED"
  )
  
  # ---------------------------------------------------------------------------
  # Profiles
  # ---------------------------------------------------------------------------
  
  route_profile <- audit |>
    count(
      SOURCE_RESTRICTION_STATE,
      BROAD_GN_METHOD_STATE,
      CANDIDATE_STAR,
      NEXT_KEY_NODE,
      name="N_TAXA",
      sort=TRUE
    ) |>
    mutate(
      PERCENT_NON_ENDEMIC=
        round(
          100*N_TAXA/nrow(audit),
          3
        )
    )
  
  evidence_profile <- audit |>
    count(
      HAS_BEYOND_EVIDENCE,
      NAMED_REGION_COUNT,
      BROAD_GN_METHOD_STATE,
      name="N_TAXA",
      sort=TRUE
    )
  
  single_region_profile <- audit |>
    filter(
      SINGLE_NAMED_REGION
    ) |>
    count(
      HAS_BEYOND_EVIDENCE,
      RESTRICTED_SINGLE_REGION,
      BROAD_GN_METHOD_STATE,
      name="N_TAXA",
      sort=TRUE
    )
  
  candidate_profile <- audit |>
    count(
      BROAD_GN_METHOD_STATE,
      CANDIDATE_STAR,
      name="N_TAXA",
      sort=TRUE
    )
  
  matrix_comparison <- audit |>
    summarise(
      NON_ENDEMIC_TAXA=n(),
      
      SOURCE_ROUTE_TO_GN=
        sum(
          BROAD_GN_METHOD_STATE=="SOURCE_ROUTE_TO_GN"
        ),
      
      CONTINUE_TO_MATRIX=
        sum(
          BROAD_GN_METHOD_STATE=="CONTINUE_TO_MATRIX"
        ),
      
      EVIDENCE_REVIEW=
        sum(
          BROAD_GN_METHOD_STATE=="EVIDENCE_REVIEW"
        ),
      
      STARS_04A_MATRIX_TAXA=
        sum(
          IS_04A_MATRIX_TAXON
        ),
      
      MATRIX_MEMBERSHIP_MATCH=
        sum(
          (
            BROAD_GN_METHOD_STATE=="CONTINUE_TO_MATRIX"
          ) ==
            IS_04A_MATRIX_TAXON
        )
    )
  
  cat("\n— BROAD-RANGE SOURCE ROUTE PROFILE —\n")
  print(route_profile,n=Inf)
  
  cat("\n— EXTERNAL EVIDENCE PROFILE —\n")
  print(evidence_profile,n=Inf)
  
  cat("\n— SINGLE-NAMED-REGION PROFILE —\n")
  print(single_region_profile,n=Inf)
  
  cat("\n— BROAD-GN CANDIDATE PROFILE —\n")
  print(candidate_profile,n=Inf)
  
  cat("\n— MATRIX CROSS-CHECK —\n")
  print(matrix_comparison,n=Inf)
  
  # ---------------------------------------------------------------------------
  # Validation components
  # ---------------------------------------------------------------------------
  
  gn_n <- sum(
    audit$BROAD_GN_METHOD_STATE=="SOURCE_ROUTE_TO_GN"
  )
  
  matrix_n <- sum(
    audit$BROAD_GN_METHOD_STATE=="CONTINUE_TO_MATRIX"
  )
  
  review_n <- sum(
    audit$BROAD_GN_METHOD_STATE=="EVIDENCE_REVIEW"
  )
  
  matrix_rows <- audit |>
    filter(
      BROAD_GN_METHOD_STATE=="CONTINUE_TO_MATRIX"
    )
  
  broad_rows <- audit |>
    filter(
      BROAD_GN_METHOD_STATE=="SOURCE_ROUTE_TO_GN"
    )
  
  restricted_rows <- audit |>
    filter(
      RESTRICTED_SINGLE_REGION
    )
  
  single_plus_beyond_rows <- audit |>
    filter(
      SINGLE_REGION_PLUS_BEYOND
    )
  
  china_only_restricted_rows <- audit |>
    filter(
      WCVP_CHINA %in% TRUE,
      NAMED_REGION_COUNT==1L,
      !HAS_BEYOND_EVIDENCE
    )
  
  # ---------------------------------------------------------------------------
  # Validation
  # ---------------------------------------------------------------------------
  
  validation <- tribble(
    ~CHECK, ~PASS,
    
    "Stars 03p validation fully PASS",
    all(validation_03p$PASS),
    
    "Stars 03r validation fully PASS",
    all(validation_03r$PASS),
    
    "Stars 03y validation fully PASS",
    all(validation_03y$PASS),
    
    "Stars 04a validation fully PASS",
    all(validation_04a$PASS),
    
    "Non-endemic population = 7,736",
    nrow(audit)==7736L,
    
    "Non-endemic population unique by WCVP ID",
    n_distinct(audit$FINAL_WCVP_ID)==7736L,
    
    "03r evidence population = 7,736",
    nrow(evidence)==7736L,
    
    "03r evidence unique by WCVP ID",
    n_distinct(evidence$FINAL_WCVP_ID)==7736L,
    
    "Stars 04a matrix population = 764",
    nrow(matrix_04a)==764L,
    
    "Stars 04a matrix population unique by WCVP ID",
    n_distinct(matrix_04a$FINAL_WCVP_ID)==764L,
    
    "All taxa have source restriction state",
    all(!is.na(audit$SOURCE_RESTRICTION_STATE)),
    
    "All taxa have broad-GN method state",
    all(!is.na(audit$BROAD_GN_METHOD_STATE)),
    
    "Beyond evidence cannot route to restricted matrix",
    !any(
      matrix_rows$HAS_BEYOND_EVIDENCE
    ),
    
    "Multiple named regions cannot route to restricted matrix",
    !any(
      matrix_rows$HAS_MULTIPLE_NAMED_REGIONS
    ),
    
    "Matrix-routed taxa have exactly one named region",
    all(
      matrix_rows$NAMED_REGION_COUNT==1L
    ),
    
    "Matrix-routed taxa have no beyond evidence",
    all(
      !matrix_rows$HAS_BEYOND_EVIDENCE
    ),
    
    "All restricted single-region taxa route to matrix",
    all(
      restricted_rows$BROAD_GN_METHOD_STATE==
        "CONTINUE_TO_MATRIX"
    ),
    
    "Single-region plus beyond taxa route to broad GN",
    all(
      single_plus_beyond_rows$BROAD_GN_METHOD_STATE==
        "SOURCE_ROUTE_TO_GN"
    ),
    
    "China-only restricted taxa route to matrix",
    all(
      china_only_restricted_rows$BROAD_GN_METHOD_STATE==
        "CONTINUE_TO_MATRIX"
    ),
    
    "Broad-GN candidate Star limited to GN",
    all(
      broad_rows$CANDIDATE_STAR=="GN"
    ),
    
    "Matrix taxa have no broad-GN candidate Star",
    all(
      is.na(matrix_rows$CANDIDATE_STAR)
    ),
    
    "Broad-GN taxa have no next Key node",
    all(
      is.na(broad_rows$NEXT_KEY_NODE)
    ),
    
    "Matrix taxa continue to non-endemic matrix",
    all(
      matrix_rows$NEXT_KEY_NODE=="NON_ENDEMIC_MATRIX"
    ),
    
    "04a matrix membership matches audit matrix route",
    matrix_comparison$MATRIX_MEMBERSHIP_MATCH==7736L,
    
    "Broad GN population = 6,972",
    gn_n==6972L,
    
    "Restricted matrix population = 764",
    matrix_n==764L,
    
    "Evidence review population = 0",
    review_n==0L,
    
    "No BK assigned",
    !any(audit$CANDIDATE_STAR=="BK",na.rm=TRUE),
    
    "No GD assigned",
    !any(audit$CANDIDATE_STAR=="GD",na.rm=TRUE),
    
    "No BU assigned",
    !any(audit$CANDIDATE_STAR=="BU",na.rm=TRUE),
    
    "No GX assigned",
    !any(audit$CANDIDATE_STAR=="GX",na.rm=TRUE),
    
    "No HYB assigned",
    !any(audit$CANDIDATE_STAR=="HYB",na.rm=TRUE),
    
    "No definitive Stars assigned",
    all(is.na(audit$DEFINITIVE_STAR)) &&
      !any(audit$STAR_ASSIGNED),
    
    "All 7,736 taxa accounted for",
    gn_n+matrix_n+review_n==7736L
  ) |>
    mutate(
      RESULT=
        if_else(
          PASS,
          "PASS",
          "FAIL"
        )
    )
  
  cat("\n— VALIDATION —\n")
  print(validation,n=Inf)
  
  if (!all(validation$PASS)) {
    stop(
      "Stars 04b validation failed. ",
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
    
    NON_ENDEMIC_TAXA=
      nrow(audit),
    
    BROAD_GN_CANDIDATES=
      gn_n,
    
    CONTINUE_TO_MATRIX=
      matrix_n,
    
    EVIDENCE_REVIEW=
      review_n,
    
    STARS_04A_MATRIX_TAXA=
      nrow(matrix_04a),
    
    STARS_03Z_CLASSIFICATION_IMPORTED=
      FALSE,
    
    CHINA_AUTOMATICALLY_TREATED_AS_BEYOND=
      FALSE,
    
    SINGLE_REGION_PLUS_BEYOND_IS_MATRIX=
      FALSE,
    
    DEFINITIVE_STARS_ASSIGNED=
      FALSE,
    
    HISTORICAL_STAR_ALLOCATIONS_USED=
      FALSE,
    
    STATUS=
      "VALIDATED_NON_ENDEMIC_BROAD_RANGE_GN_AUDIT"
  )
  
  # ---------------------------------------------------------------------------
  # Canonical database outputs
  # ---------------------------------------------------------------------------
  
  dbWriteTable(
    con,
    TABLE_AUDIT,
    audit,
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
    TABLE_PROFILE,
    route_profile,
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
  # CSV audit outputs
  # ---------------------------------------------------------------------------
  
  write_csv(
    audit,
    file.path(
      OUTPUT_DIR,
      "stars04b_non_endemic_broad_range_gn_audit.csv"
    )
  )
  
  write_csv(
    method_register,
    file.path(
      OUTPUT_DIR,
      "stars04b_method_register.csv"
    )
  )
  
  write_csv(
    route_profile,
    file.path(
      OUTPUT_DIR,
      "stars04b_route_profile.csv"
    )
  )
  
  write_csv(
    evidence_profile,
    file.path(
      OUTPUT_DIR,
      "stars04b_evidence_profile.csv"
    )
  )
  
  write_csv(
    single_region_profile,
    file.path(
      OUTPUT_DIR,
      "stars04b_single_named_region_profile.csv"
    )
  )
  
  write_csv(
    candidate_profile,
    file.path(
      OUTPUT_DIR,
      "stars04b_candidate_profile.csv"
    )
  )
  
  write_csv(
    matrix_comparison,
    file.path(
      OUTPUT_DIR,
      "stars04b_matrix_crosscheck.csv"
    )
  )
  
  write_csv(
    validation,
    file.path(
      OUTPUT_DIR,
      "stars04b_validation.csv"
    )
  )
  
  write_csv(
    metadata,
    file.path(
      OUTPUT_DIR,
      "stars04b_metadata.csv"
    )
  )
  
  # ---------------------------------------------------------------------------
  # Final summary
  # ---------------------------------------------------------------------------
  
  cat(
    "\n============================================================\n"
  )
  
  cat(
    "Stars 04b v",VERSION," COMPLETE\n",
    sep=""
  )
  
  cat(
    "============================================================\n"
  )
  
  cat(
    "Non-endemic taxa audited: ",
    format(nrow(audit),big.mark=","),
    "\n",
    sep=""
  )
  
  cat(
    "Broad-range candidate GN: ",
    format(gn_n,big.mark=","),
    "\n",
    sep=""
  )
  
  cat(
    "Continue to literal matrix: ",
    format(matrix_n,big.mark=","),
    "\n",
    sep=""
  )
  
  cat(
    "Evidence review: ",
    format(review_n,big.mark=","),
    "\n",
    sep=""
  )
  
  cat(
    "04a matrix membership matched: ",
    matrix_comparison$MATRIX_MEMBERSHIP_MATCH,
    "/7,736\n",
    sep=""
  )
  
  cat(
    "03z classification imported: FALSE\n"
  )
  
  cat(
    "China automatically treated as beyond: FALSE\n"
  )
  
  cat(
    "Definitive Stars assigned: FALSE\n"
  )
  
  cat(
    "Historical Star allocations used: FALSE\n"
  )
  
  cat(
    "Validation: ",
    sum(validation$PASS),"/",
    nrow(validation)," PASS\n",
    sep=""
  )
  
  cat(
    "Canonical audit: ",
    TABLE_AUDIT,
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
      audit=audit,
      method_register=method_register,
      route_profile=route_profile,
      evidence_profile=evidence_profile,
      single_region_profile=single_region_profile,
      candidate_profile=candidate_profile,
      matrix_comparison=matrix_comparison,
      validation=validation,
      metadata=metadata
    )
  )
}

stars_04b_result <- run_stars_04b()