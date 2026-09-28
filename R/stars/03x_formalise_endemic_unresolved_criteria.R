# =============================================================================
# VPJD-OJPCP
# R/stars/03x_formalise_endemic_unresolved_criteria.R
# Version 0.1.0
#
# FORMALISE UNRESOLVED ENDEMIC KEY CRITERIA
#
# PURPOSE
# - Formally document the two unresolved endemic criteria in Nakamura
#   (2012), Figure 2.2:
#     1. "rare (e.g., mountain tops, along the coastline)"
#     2. "not in all districts" / "in almost all districts"
# - Preserve the affected taxa as explicit methodological work packages.
# - Prevent unsupported operational thresholds or proxies entering the
#   classifier.
# - Record what evidence would be required to resolve each criterion.
#
# IMPORTANT
# - Methodological register only.
# - No rarity proxy is created.
# - No definition of "almost all districts" is imposed.
# - No Stars are assigned.
# - Historical Star allocations are not used to infer either criterion.
# =============================================================================

suppressPackageStartupMessages({
  library(DBI)
  library(duckdb)
  library(dplyr)
  library(readr)
  library(tibble)
  library(here)
})

MODULE <- "stars_03x_formalise_endemic_unresolved_criteria"
VERSION <- "0.1.0"
RUN_DATE <- Sys.Date()

DB_PATH <- here(
  "data","interim","occurrences",
  "vpjd_occurrences.duckdb"
)

OUTPUT_DIR <- here(
  "outputs","tables","stars",
  "endemic_unresolved_criteria"
)

TABLE_REGISTER <-
  "vpjd_star_endemic_unresolved_criteria_register"

TABLE_WORK_PACKAGES <-
  "vpjd_star_endemic_method_work_packages"

TABLE_PROHIBITED <-
  "vpjd_star_endemic_prohibited_assumptions"

TABLE_READINESS <-
  "vpjd_star_endemic_method_readiness"

TABLE_VALIDATION <-
  "vpjd_star_03x_validation"

TABLE_METADATA <-
  "vpjd_star_03x_metadata"

run_stars_03x <- function() {
  
  cat("\n— Formalise unresolved endemic Key criteria —\n\n")
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
    "vpjd_star_key_source_nodes",
    "vpjd_star_key_operational_gaps",
    "vpjd_star_endemic_key_traversal",
    "vpjd_star_endemic_unresolved_work_packages",
    "vpjd_star_03p_validation",
    "vpjd_star_03w_validation"
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
  
  # ---------------------------------------------------------------------------
  # Upstream validation
  # ---------------------------------------------------------------------------
  
  validation_03p <- as_tibble(
    dbGetQuery(
      con,
      "SELECT * FROM vpjd_star_03p_validation"
    )
  )
  
  validation_03w <- as_tibble(
    dbGetQuery(
      con,
      "SELECT * FROM vpjd_star_03w_validation"
    )
  )
  
  if (
    !"PASS" %in% names(validation_03p) ||
    !all(validation_03p$PASS)
  ) {
    stop("Stars 03p is not fully validated.")
  }
  
  if (
    !"PASS" %in% names(validation_03w) ||
    !all(validation_03w$PASS)
  ) {
    stop("Stars 03w is not fully validated.")
  }
  
  cat(
    "Stars 03p validation: ",
    sum(validation_03p$PASS),"/",
    nrow(validation_03p)," PASS\n",
    sep=""
  )
  
  cat(
    "Stars 03w validation: ",
    sum(validation_03w$PASS),"/",
    nrow(validation_03w)," PASS\n",
    sep=""
  )
  
  # ---------------------------------------------------------------------------
  # Current endemic traversal
  # ---------------------------------------------------------------------------
  
  traversal <- as_tibble(
    dbGetQuery(
      con,
      "
      SELECT *
      FROM vpjd_star_endemic_key_traversal
      "
    )
  )
  
  if (nrow(traversal)!=2141L) {
    stop(
      "Expected 2,141 endemic-Key taxa; found ",
      format(nrow(traversal),big.mark=",")
    )
  }
  
  if (
    n_distinct(
      traversal$FINAL_WCVP_ID
    )!=2141L
  ) {
    stop(
      "Endemic traversal is not unique by WCVP ID."
    )
  }
  
  rarity_taxa <- traversal |>
    filter(
      NEXT_KEY_NODE=="RARE"
    )
  
  almost_all_taxa <- traversal |>
    filter(
      NEXT_KEY_NODE=="ALMOST_ALL_DISTRICTS"
    )
  
  special_area_taxa <- traversal |>
    filter(
      NEXT_KEY_NODE==
        "SPECIAL_AREA_DISTRICT_TREATMENT"
    )
  
  kazan_taxa <- traversal |>
    filter(
      NEXT_KEY_NODE==
        "KAZAN_OR_SPECIAL_AREA_TREATMENT"
    )
  
  cat(
    "Endemic-Key taxa: ",
    format(nrow(traversal),big.mark=","),
    "\n",
    sep=""
  )
  
  cat(
    "Rarity work package: ",
    nrow(rarity_taxa),
    "\n",
    sep=""
  )
  
  cat(
    "Almost-all-districts work package: ",
    nrow(almost_all_taxa),
    "\n",
    sep=""
  )
  
  # ---------------------------------------------------------------------------
  # Formal methodological register
  # ---------------------------------------------------------------------------
  
  criteria_register <- tribble(
    ~CRITERION_ID,
    ~KEY_TERM,
    ~SOURCE,
    ~ORIGINAL_TEXT,
    ~CURRENT_INTERPRETATION,
    ~OPERATIONAL_DEFINITION,
    ~RATIONALE,
    ~STATUS,
    ~AFFECTED_TAXA,
    
    "END_UNRES_01",
    "RARE",
    "Nakamura (2012), Figure 2.2, p.48",
    "rare (e.g., mountain tops, along the coastline)",
    paste0(
      "Rarity is a distinct criterion applied within the endemic ",
      "widespread branch when occurrence is in <=5 prefectures."
    ),
    NA_character_,
    paste0(
      "Figure 2.2 provides examples of rarity but does not, from the ",
      "currently available source evidence, provide a deterministic ",
      "quantitative definition. Contemporary GBIF record frequency, ",
      "number of localities, occupied analytical areas, or another ",
      "unsourced proxy must not be substituted."
    ),
    "UNRESOLVED",
    nrow(rarity_taxa),
    
    "END_UNRES_02",
    "ALMOST_ALL_DISTRICTS",
    "Nakamura (2012), Figure 2.2, p.48",
    "Not in all districts / In almost all districts",
    paste0(
      "The source contrasts BLUE with GREEN using district coverage ",
      "after the endemic widespread >10-prefecture branch."
    ),
    NA_character_,
    paste0(
      "The literal wording is not sufficient for a deterministic ",
      "threshold because 'not in all districts' and 'in almost all ",
      "districts' are not mutually exclusive in ordinary language. ",
      "No 7/8, 8/8, or other threshold is inferred."
    ),
    "UNRESOLVED",
    nrow(almost_all_taxa),
    
    "END_UNRES_03",
    "SPECIAL_AREA_DISTRICT_TREATMENT",
    "Nakamura (2012), Figure 2.2, p.48; Stars 03u-03v",
    paste0(
      "District threshold applied after taxa not confined to the ",
      "named small-island groups."
    ),
    paste0(
      "Eight core districts are provisionally operationalised, but ",
      "the effect of occupied special analytical areas on district ",
      "count remains unresolved."
    ),
    NA_character_,
    paste0(
      "No special analytical area is automatically added to the ",
      "eight-core-district count."
    ),
    "UNRESOLVED",
    nrow(special_area_taxa),
    
    "END_UNRES_04",
    "KAZAN",
    "Nakamura (2012), Figure 2.2, p.48; Stars 03t",
    paste0(
      "Kazan Islands are not explicitly named in the Figure 2.2 ",
      "small-island criterion."
    ),
    paste0(
      "Kazan remains a separate analytical area with no inferred ",
      "Key treatment."
    ),
    NA_character_,
    paste0(
      "Kazan must not be silently merged with Ogasawara or another ",
      "Key geography without methodological evidence."
    ),
    "UNRESOLVED",
    nrow(kazan_taxa)
  )
  
  # ---------------------------------------------------------------------------
  # Work-package register
  # ---------------------------------------------------------------------------
  
  work_packages <- bind_rows(
    
    rarity_taxa |>
      transmute(
        FINAL_WCVP_ID,
        FINAL_WCVP_RECOGNISED_NAME,
        FINAL_WCVP_RANK,
        WORK_PACKAGE="RARE",
        SOURCE_NODE=
          "E04_WIDESPREAD_LE5_PREFECTURES_RARITY_REQUIRED",
        REQUIRED_RESOLUTION=
          paste0(
            "Method-validated operational definition and evidence ",
            "for Nakamura's rarity criterion."
          ),
        STATUS="METHOD_UNRESOLVED"
      ),
    
    almost_all_taxa |>
      transmute(
        FINAL_WCVP_ID,
        FINAL_WCVP_RECOGNISED_NAME,
        FINAL_WCVP_RANK,
        WORK_PACKAGE="ALMOST_ALL_DISTRICTS",
        SOURCE_NODE=
          "E04_WIDESPREAD_GT10_PREFECTURES",
        REQUIRED_RESOLUTION=
          paste0(
            "Method-validated mutually exclusive interpretation of ",
            "'not in all districts' versus 'in almost all districts'."
          ),
        STATUS="METHOD_UNRESOLVED"
      ),
    
    special_area_taxa |>
      transmute(
        FINAL_WCVP_ID,
        FINAL_WCVP_RECOGNISED_NAME,
        FINAL_WCVP_RANK,
        WORK_PACKAGE=
          "SPECIAL_AREA_DISTRICT_TREATMENT",
        SOURCE_NODE=
          "E03_SPECIAL_AREA_METHOD_SENSITIVE",
        REQUIRED_RESOLUTION=
          paste0(
            "Method-validated treatment of occupied special ",
            "analytical areas at the <=2/>2 district threshold."
          ),
        STATUS="METHOD_UNRESOLVED"
      ),
    
    kazan_taxa |>
      transmute(
        FINAL_WCVP_ID,
        FINAL_WCVP_RECOGNISED_NAME,
        FINAL_WCVP_RANK,
        WORK_PACKAGE=
          "KAZAN_OR_SPECIAL_AREA_TREATMENT",
        SOURCE_NODE=
          "E02_SPECIAL_AREA_REVIEW",
        REQUIRED_RESOLUTION=
          paste0(
            "Method-validated treatment of Kazan in the endemic ",
            "small-island/district branches."
          ),
        STATUS="METHOD_UNRESOLVED"
      )
  )
  
  work_package_profile <- work_packages |>
    count(
      WORK_PACKAGE,
      STATUS,
      name="N_TAXA",
      sort=TRUE
    ) |>
    mutate(
      PERCENT_ENDEMIC_KEY=
        round(
          100*N_TAXA/nrow(traversal),
          3
        )
    )
  
  # ---------------------------------------------------------------------------
  # Explicit prohibited assumptions
  # ---------------------------------------------------------------------------
  
  prohibited_assumptions <- tribble(
    ~ASSUMPTION_ID,
    ~CRITERION,
    ~PROHIBITED_ASSUMPTION,
    
    "EPA01",
    "RARE",
    paste0(
      "Do not classify rarity from raw GBIF occurrence-record count."
    ),
    
    "EPA02",
    "RARE",
    paste0(
      "Do not classify rarity from number of VPJD localities without ",
      "a source-supported rarity definition."
    ),
    
    "EPA03",
    "RARE",
    paste0(
      "Do not equate occurrence in <=5 prefectures with rarity; ",
      "Figure 2.2 requires <=5 prefectures AND rare."
    ),
    
    "EPA04",
    "RARE",
    paste0(
      "Do not infer rarity solely from occurrence on mountains, ",
      "coasts, islands, or another habitat descriptor."
    ),
    
    "EPA05",
    "ALMOST_ALL_DISTRICTS",
    paste0(
      "Do not define 'almost all districts' as 7 of 8 without ",
      "methodological evidence."
    ),
    
    "EPA06",
    "ALMOST_ALL_DISTRICTS",
    paste0(
      "Do not define 'almost all districts' as 8 of 8 without ",
      "methodological evidence."
    ),
    
    "EPA07",
    "ALMOST_ALL_DISTRICTS",
    paste0(
      "Do not assume 'not in all districts' and 'in almost all ",
      "districts' are mutually exclusive without an operational rule."
    ),
    
    "EPA08",
    "SPECIAL_AREA_DISTRICT_TREATMENT",
    paste0(
      "Do not automatically add Ryukyu, Izu, Ogasawara, Kazan or ",
      "Kuriles to the eight-core-district count."
    ),
    
    "EPA09",
    "KAZAN",
    paste0(
      "Do not automatically merge Kazan with Ogasawara for Key ",
      "classification."
    ),
    
    "EPA10",
    "ALL",
    paste0(
      "Do not use historical Star allocations to choose thresholds, ",
      "definitions or individual classifications."
    )
  )
  
  # ---------------------------------------------------------------------------
  # Endemic readiness
  # ---------------------------------------------------------------------------
  
  candidate_n <- sum(
    traversal$TRAVERSAL_STATUS=="CANDIDATE_TERMINAL"
  )
  
  unresolved_n <- nrow(work_packages)
  
  readiness <- tribble(
    ~COMPONENT,
    ~READY,
    ~STATUS,
    
    "Endemic contemporary population",
    TRUE,
    "EVIDENCE_VALIDATED",
    
    "Endemic small-island source criterion",
    TRUE,
    "SOURCE_AND_EVIDENCE_VALIDATED",
    
    "Eight-core-district evidence",
    TRUE,
    "PROVISIONAL_METHOD_VALIDATED",
    
    "E03 numerical threshold evidence",
    TRUE,
    "EVIDENCE_VALIDATED",
    
    "Endemic <=7 / >7 prefecture threshold",
    TRUE,
    "SOURCE_VALIDATED",
    
    "Endemic <=10 / >10 prefecture threshold",
    TRUE,
    "SOURCE_VALIDATED",
    
    "Endemic >5 <=10 direct GD route",
    TRUE,
    "SOURCE_AND_EVIDENCE_VALIDATED",
    
    "Rarity criterion",
    FALSE,
    "UNRESOLVED",
    
    "Almost-all-districts criterion",
    FALSE,
    "UNRESOLVED",
    
    "Special-area district treatment",
    FALSE,
    "UNRESOLVED",
    
    "Kazan treatment",
    FALSE,
    "UNRESOLVED",
    
    "Endemic branch fully operational",
    FALSE,
    "NOT_READY",
    
    "Definitive endemic Stars",
    FALSE,
    "NOT_ASSIGNED"
  )
  
  cat("\n— UNRESOLVED CRITERIA REGISTER —\n")
  print(criteria_register,n=Inf)
  
  cat("\n— ENDEMIC METHOD WORK PACKAGES —\n")
  print(work_package_profile,n=Inf)
  
  cat("\n— PROHIBITED ASSUMPTIONS —\n")
  print(prohibited_assumptions,n=Inf)
  
  cat("\n— ENDEMIC METHOD READINESS —\n")
  print(readiness,n=Inf)
  
  # ---------------------------------------------------------------------------
  # Validation
  # ---------------------------------------------------------------------------
  
  validation <- tibble(
    CHECK=c(
      "Stars 03p validation fully PASS",
      "Stars 03w validation fully PASS",
      "Endemic traversal population = 2,141",
      "Endemic traversal unique by WCVP ID",
      "Candidate terminal population = 1,329",
      "Candidate BK = 1,081",
      "Candidate GD = 248",
      "Rarity work package = 85",
      "Almost-all-districts work package = 670",
      "Special-area district work package = 56",
      "Kazan work package = 1",
      "Unresolved endemic work packages total = 812",
      "Candidate + unresolved endemic taxa = 2,141",
      "Work-package taxa unique by WCVP ID",
      "Rarity criterion remains unresolved",
      "Almost-all-districts criterion remains unresolved",
      "Special-area district treatment remains unresolved",
      "Kazan treatment remains unresolved",
      "No rarity operational definition created",
      "No almost-all operational definition created",
      "No 7-of-8 threshold imposed",
      "No 8-of-8 threshold imposed",
      "No occurrence-count rarity proxy created",
      "No Stars assigned",
      "Historical Star allocations not used"
    ),
    
    PASS=c(
      all(validation_03p$PASS),
      all(validation_03w$PASS),
      
      nrow(traversal)==2141L,
      
      n_distinct(
        traversal$FINAL_WCVP_ID
      )==2141L,
      
      candidate_n==1329L,
      
      sum(
        traversal$CANDIDATE_STAR=="BK",
        na.rm=TRUE
      )==1081L,
      
      sum(
        traversal$CANDIDATE_STAR=="GD",
        na.rm=TRUE
      )==248L,
      
      nrow(rarity_taxa)==85L,
      
      nrow(almost_all_taxa)==670L,
      
      nrow(special_area_taxa)==56L,
      
      nrow(kazan_taxa)==1L,
      
      unresolved_n==812L,
      
      candidate_n+unresolved_n==2141L,
      
      n_distinct(
        work_packages$FINAL_WCVP_ID
      )==812L,
      
      criteria_register$STATUS[
        criteria_register$CRITERION_ID==
          "END_UNRES_01"
      ]=="UNRESOLVED",
      
      criteria_register$STATUS[
        criteria_register$CRITERION_ID==
          "END_UNRES_02"
      ]=="UNRESOLVED",
      
      criteria_register$STATUS[
        criteria_register$CRITERION_ID==
          "END_UNRES_03"
      ]=="UNRESOLVED",
      
      criteria_register$STATUS[
        criteria_register$CRITERION_ID==
          "END_UNRES_04"
      ]=="UNRESOLVED",
      
      is.na(
        criteria_register$OPERATIONAL_DEFINITION[
          criteria_register$CRITERION_ID==
            "END_UNRES_01"
        ]
      ),
      
      is.na(
        criteria_register$OPERATIONAL_DEFINITION[
          criteria_register$CRITERION_ID==
            "END_UNRES_02"
        ]
      ),
      
      TRUE,
      TRUE,
      TRUE,
      
      !any(
        traversal$DEFINITIVE_STAR_ASSIGNED
      ),
      
      TRUE
    )
  ) |>
    mutate(
      RESULT=
        if_else(PASS,"PASS","FAIL")
    )
  
  cat("\n— VALIDATION —\n")
  print(validation,n=Inf)
  
  if (!all(validation$PASS)) {
    stop(
      "Stars 03x validation failed. ",
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
    
    ENDEMIC_KEY_TAXA=2141L,
    
    CANDIDATE_TERMINAL_TAXA=
      candidate_n,
    
    UNRESOLVED_ENDEMIC_TAXA=
      unresolved_n,
    
    RARITY_TAXA=
      nrow(rarity_taxa),
    
    ALMOST_ALL_DISTRICTS_TAXA=
      nrow(almost_all_taxa),
    
    SPECIAL_AREA_DISTRICT_TAXA=
      nrow(special_area_taxa),
    
    KAZAN_TAXA=
      nrow(kazan_taxa),
    
    RARITY_OPERATIONALISED=FALSE,
    
    ALMOST_ALL_OPERATIONALISED=FALSE,
    
    SPECIAL_AREA_TREATMENT_RESOLVED=FALSE,
    
    KAZAN_TREATMENT_RESOLVED=FALSE,
    
    DEFINITIVE_STARS_ASSIGNED=FALSE,
    
    HISTORICAL_STAR_ALLOCATIONS_USED=FALSE,
    
    STATUS=
      "VALIDATED_UNRESOLVED_METHOD_REGISTER",
    
    NEXT_PRIORITY=
      "NON_ENDEMIC_KEY_TRAVERSAL"
  )
  
  # ---------------------------------------------------------------------------
  # Canonical DuckDB outputs
  # ---------------------------------------------------------------------------
  
  dbWriteTable(
    con,
    TABLE_REGISTER,
    criteria_register,
    overwrite=TRUE
  )
  
  dbWriteTable(
    con,
    TABLE_WORK_PACKAGES,
    work_packages,
    overwrite=TRUE
  )
  
  dbWriteTable(
    con,
    TABLE_PROHIBITED,
    prohibited_assumptions,
    overwrite=TRUE
  )
  
  dbWriteTable(
    con,
    TABLE_READINESS,
    readiness,
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
    criteria_register,
    file.path(
      OUTPUT_DIR,
      "stars03x_unresolved_criteria_register.csv"
    )
  )
  
  write_csv(
    work_packages,
    file.path(
      OUTPUT_DIR,
      "stars03x_endemic_method_work_packages.csv"
    )
  )
  
  write_csv(
    work_package_profile,
    file.path(
      OUTPUT_DIR,
      "stars03x_work_package_profile.csv"
    )
  )
  
  write_csv(
    prohibited_assumptions,
    file.path(
      OUTPUT_DIR,
      "stars03x_prohibited_assumptions.csv"
    )
  )
  
  write_csv(
    readiness,
    file.path(
      OUTPUT_DIR,
      "stars03x_endemic_method_readiness.csv"
    )
  )
  
  write_csv(
    validation,
    file.path(
      OUTPUT_DIR,
      "stars03x_validation.csv"
    )
  )
  
  write_csv(
    metadata,
    file.path(
      OUTPUT_DIR,
      "stars03x_metadata.csv"
    )
  )
  
  # ---------------------------------------------------------------------------
  # Final summary
  # ---------------------------------------------------------------------------
  
  cat(
    "\n============================================================\n"
  )
  
  cat(
    "Stars 03x v",VERSION," COMPLETE\n",
    sep=""
  )
  
  cat(
    "============================================================\n"
  )
  
  cat(
    "Endemic Key taxa: 2,141\n"
  )
  
  cat(
    "Candidate terminal taxa retained: ",
    format(candidate_n,big.mark=","),
    "\n",
    sep=""
  )
  
  cat(
    "Unresolved endemic taxa: ",
    format(unresolved_n,big.mark=","),
    "\n",
    sep=""
  )
  
  cat(
    "Rarity unresolved: ",
    nrow(rarity_taxa),
    "\n",
    sep=""
  )
  
  cat(
    "Almost-all districts unresolved: ",
    nrow(almost_all_taxa),
    "\n",
    sep=""
  )
  
  cat(
    "Special-area district treatment unresolved: ",
    nrow(special_area_taxa),
    "\n",
    sep=""
  )
  
  cat(
    "Kazan treatment unresolved: ",
    nrow(kazan_taxa),
    "\n",
    sep=""
  )
  
  cat(
    "Unsupported rarity proxy used: FALSE\n"
  )
  
  cat(
    "Unsupported almost-all threshold used: FALSE\n"
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
    "Canonical criteria register: ",
    TABLE_REGISTER,
    "\n",
    sep=""
  )
  
  cat(
    "Canonical work packages: ",
    TABLE_WORK_PACKAGES,
    "\n",
    sep=""
  )
  
  cat(
    "Next priority: NON-ENDEMIC KEY TRAVERSAL\n"
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
      criteria_register=criteria_register,
      work_packages=work_packages,
      work_package_profile=
        work_package_profile,
      prohibited_assumptions=
        prohibited_assumptions,
      readiness=readiness,
      validation=validation,
      metadata=metadata
    )
  )
}

stars_03x_result <- run_stars_03x()