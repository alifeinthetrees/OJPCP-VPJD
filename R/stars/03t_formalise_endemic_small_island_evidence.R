# =============================================================================
# VPJD-OJPCP
# R/stars/03t_formalise_endemic_small_island_evidence.R
# Version 0.1.0
#
# FORMALISE ENDEMIC SMALL-ISLAND EVIDENCE
#
# PURPOSE
# - Build explicit taxon-level evidence for the endemic small-island branch
#   of Nakamura (2012), Figure 2.2.
# - Use the canonical contemporary taxon-area distribution table directly.
# - Determine whether endemic taxa are confined to the explicitly named
#   small-island groups: Ryukyu, Izu and Ogasawara.
# - Keep Kazan and Kuriles treatment explicitly unresolved.
#
# IMPORTANT
# - Evidence module only.
# - No Stars assigned.
# - No district counts created.
# - No treatment of Kazan or Kuriles inferred.
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

MODULE <- "stars_03t_formalise_endemic_small_island_evidence"
VERSION <- "0.1.0"
RUN_DATE <- Sys.Date()

DB_PATH <- here(
  "data","interim","occurrences",
  "vpjd_occurrences.duckdb"
)

OUTPUT_DIR <- here(
  "outputs","tables","stars",
  "endemic_small_island_evidence"
)

TABLE_EVIDENCE <-
  "vpjd_star_endemic_small_island_evidence"

TABLE_PROFILE <-
  "vpjd_star_endemic_small_island_profile"

TABLE_VALIDATION <-
  "vpjd_star_03t_validation"

TABLE_METADATA <-
  "vpjd_star_03t_metadata"

run_stars_03t <- function() {
  
  cat("\n— Endemic small-island evidence —\n\n")
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
    "vpjd_japan_taxon_distribution",
    "vpjd_japan_taxon_area_distribution",
    "vpjd_star_wcvp_key_geography",
    "vpjd_star_hybrid_status",
    "vpjd_star_03p_validation",
    "vpjd_star_03s_validation"
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
  
  validation_03s <- as_tibble(
    dbGetQuery(
      con,
      "SELECT * FROM vpjd_star_03s_validation"
    )
  )
  
  if (
    !"PASS" %in% names(validation_03p) ||
    !all(validation_03p$PASS)
  ) {
    stop("Stars 03p is not fully validated.")
  }
  
  if (
    !"PASS" %in% names(validation_03s) ||
    !all(validation_03s$PASS)
  ) {
    stop("Stars 03s is not fully validated.")
  }
  
  cat(
    "Stars 03p validation: ",
    sum(validation_03p$PASS),"/",
    nrow(validation_03p)," PASS\n",
    sep=""
  )
  
  cat(
    "Stars 03s validation: ",
    sum(validation_03s$PASS),"/",
    nrow(validation_03s)," PASS\n",
    sep=""
  )
  
  # ---------------------------------------------------------------------------
  # Accepted contemporary population
  # ---------------------------------------------------------------------------
  
  accepted <- as_tibble(
    dbGetQuery(
      con,
      "
      SELECT DISTINCT
        FINAL_WCVP_ID,
        FINAL_WCVP_RECOGNISED_NAME,
        FINAL_WCVP_RANK
      FROM vpjd_japan_taxon_distribution
      WHERE FINAL_WCVP_STATUS = 'Accepted'
      "
    )
  ) |>
    mutate(
      FINAL_WCVP_ID=
        as.character(FINAL_WCVP_ID)
    )
  
  if (nrow(accepted)!=11439L) {
    stop(
      "Expected 11,439 accepted taxa; found ",
      format(nrow(accepted),big.mark=",")
    )
  }
  
  if (
    n_distinct(
      accepted$FINAL_WCVP_ID
    )!=11439L
  ) {
    stop(
      "Accepted population is not one row per FINAL_WCVP_ID."
    )
  }
  
  cat(
    "Accepted contemporary population: ",
    format(nrow(accepted),big.mark=","),
    "\n",
    sep=""
  )
  
  # ---------------------------------------------------------------------------
  # Endemicity / introduction evidence
  # ---------------------------------------------------------------------------
  
  wcvp <- as_tibble(
    dbGetQuery(
      con,
      "
      SELECT
        FINAL_WCVP_ID,
        ENDEMIC_TO_JAPAN_WCVP,
        WCVP_INTRODUCED_TO_JAPAN
      FROM vpjd_star_wcvp_key_geography
      "
    )
  ) |>
    mutate(
      FINAL_WCVP_ID=
        as.character(FINAL_WCVP_ID)
    )
  
  # ---------------------------------------------------------------------------
  # HYB evidence
  # ---------------------------------------------------------------------------
  
  hybrid <- as_tibble(
    dbGetQuery(
      con,
      "
      SELECT
        FINAL_WCVP_ID,
        HYBRID_STATUS
      FROM vpjd_star_hybrid_status
      "
    )
  ) |>
    mutate(
      FINAL_WCVP_ID=
        as.character(FINAL_WCVP_ID)
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
  
  cat(
    "Accepted taxon-area evidence rows: ",
    format(nrow(area_occ),big.mark=","),
    "\n",
    sep=""
  )
  
  # ---------------------------------------------------------------------------
  # Canonical area sets
  # ---------------------------------------------------------------------------
  
  CORE_AREA_IDS <-
    sprintf(
      "JP%02d",
      1:46
    )
  
  NAMED_SMALL_ISLAND_IDS <- c(
    "JP47", # Ryukyu
    "JP48", # Izu
    "JP49"  # Ogasawara
  )
  
  KAZAN_ID <- "JP50"
  KURILES_ID <- "JP51"
  
  EXPECTED_AREA_IDS <- c(
    CORE_AREA_IDS,
    NAMED_SMALL_ISLAND_IDS,
    KAZAN_ID,
    KURILES_ID
  )
  
  unexpected_area_ids <- setdiff(
    unique(area_occ$BOTANICAL_AREA_ID),
    EXPECTED_AREA_IDS
  )
  
  if (length(unexpected_area_ids)>0L) {
    stop(
      "Unexpected BOTANICAL_AREA_ID values: ",
      paste(
        unexpected_area_ids,
        collapse=", "
      )
    )
  }
  
  # ---------------------------------------------------------------------------
  # Taxon-level area evidence
  # ---------------------------------------------------------------------------
  
  area_evidence <- area_occ |>
    group_by(
      FINAL_WCVP_ID
    ) |>
    summarise(
      N_ANALYTICAL_AREAS=
        n_distinct(
          BOTANICAL_AREA_ID
        ),
      
      N_CORE_AREAS=
        n_distinct(
          BOTANICAL_AREA_ID[
            BOTANICAL_AREA_ID %in%
              CORE_AREA_IDS
          ]
        ),
      
      N_NAMED_SMALL_ISLAND_AREAS=
        n_distinct(
          BOTANICAL_AREA_ID[
            BOTANICAL_AREA_ID %in%
              NAMED_SMALL_ISLAND_IDS
          ]
        ),
      
      HAS_CORE_JAPAN=
        any(
          BOTANICAL_AREA_ID %in%
            CORE_AREA_IDS
        ),
      
      HAS_RYUKYU=
        any(
          BOTANICAL_AREA_ID=="JP47"
        ),
      
      HAS_IZU=
        any(
          BOTANICAL_AREA_ID=="JP48"
        ),
      
      HAS_OGASAWARA=
        any(
          BOTANICAL_AREA_ID=="JP49"
        ),
      
      HAS_KAZAN=
        any(
          BOTANICAL_AREA_ID=="JP50"
        ),
      
      HAS_KURILES=
        any(
          BOTANICAL_AREA_ID=="JP51"
        ),
      
      .groups="drop"
    )
  
  # ---------------------------------------------------------------------------
  # Join all accepted taxa
  # ---------------------------------------------------------------------------
  
  evidence <- accepted |>
    left_join(
      wcvp,
      by="FINAL_WCVP_ID"
    ) |>
    left_join(
      hybrid,
      by="FINAL_WCVP_ID"
    ) |>
    left_join(
      area_evidence,
      by="FINAL_WCVP_ID"
    ) |>
    mutate(
      INTRODUCED_POSITIVE=
        WCVP_INTRODUCED_TO_JAPAN %in% TRUE,
      
      HYBRID_POSITIVE=
        HYBRID_STATUS=="HYBRID",
      
      ENDEMIC_STATUS=
        case_when(
          ENDEMIC_TO_JAPAN_WCVP %in% TRUE ~
            "ENDEMIC",
          
          ENDEMIC_TO_JAPAN_WCVP %in% FALSE ~
            "NON_ENDEMIC",
          
          TRUE ~
            "UNRESOLVED"
        ),
      
      HAS_AREA_EVIDENCE=
        !is.na(
          N_ANALYTICAL_AREAS
        ),
      
      HAS_NAMED_SMALL_ISLAND=
        coalesce(HAS_RYUKYU,FALSE) |
        coalesce(HAS_IZU,FALSE) |
        coalesce(HAS_OGASAWARA,FALSE),
      
      HAS_UNRESOLVED_SPECIAL_AREA=
        coalesce(HAS_KAZAN,FALSE) |
        coalesce(HAS_KURILES,FALSE)
    )
  
  # ---------------------------------------------------------------------------
  # Source-faithful small-island evidence state
  #
  # TRUE:
  #   Taxon has evidence in >=1 of Ryukyu/Izu/Ogasawara,
  #   no core JP01-JP46 evidence,
  #   no Kazan evidence,
  #   no Kuriles evidence.
  #
  # FALSE:
  #   Any core JP01-JP46 evidence proves that the taxon is not confined
  #   to the named small-island groups.
  #
  # UNRESOLVED_SPECIAL_AREA:
  #   No core evidence, but Kazan and/or Kuriles are involved.
  #
  # NO_NAMED_SMALL_ISLAND:
  #   No evidence in Ryukyu/Izu/Ogasawara.
  #
  # NO_AREA_EVIDENCE:
  #   No canonical area evidence available.
  # ---------------------------------------------------------------------------
  
  evidence <- evidence |>
    mutate(
      SMALL_ISLAND_ROUTE_STATUS=
        case_when(
          !HAS_AREA_EVIDENCE ~
            "NO_AREA_EVIDENCE",
          
          coalesce(
            HAS_CORE_JAPAN,
            FALSE
          ) ~
            "NOT_EXCLUSIVE_CORE_PRESENT",
          
          !coalesce(
            HAS_CORE_JAPAN,
            FALSE
          ) &
            HAS_NAMED_SMALL_ISLAND &
            !HAS_UNRESOLVED_SPECIAL_AREA ~
            "EXCLUSIVE_NAMED_SMALL_ISLANDS",
          
          !coalesce(
            HAS_CORE_JAPAN,
            FALSE
          ) &
            HAS_UNRESOLVED_SPECIAL_AREA ~
            "UNRESOLVED_SPECIAL_AREA",
          
          !HAS_NAMED_SMALL_ISLAND ~
            "NO_NAMED_SMALL_ISLAND",
          
          TRUE ~
            "METHOD_REVIEW"
        ),
      
      NAMED_SMALL_ISLAND_EXCLUSIVE=
        case_when(
          SMALL_ISLAND_ROUTE_STATUS==
            "EXCLUSIVE_NAMED_SMALL_ISLANDS" ~
            TRUE,
          
          SMALL_ISLAND_ROUTE_STATUS==
            "NOT_EXCLUSIVE_CORE_PRESENT" ~
            FALSE,
          
          SMALL_ISLAND_ROUTE_STATUS==
            "NO_NAMED_SMALL_ISLAND" ~
            FALSE,
          
          TRUE ~
            NA
        ),
      
      SPECIAL_AREA_REVIEW_REASON=
        case_when(
          SMALL_ISLAND_ROUTE_STATUS !=
            "UNRESOLVED_SPECIAL_AREA" ~
            NA_character_,
          
          coalesce(HAS_KAZAN,FALSE) &
            coalesce(HAS_KURILES,FALSE) ~
            "KAZAN_AND_KURILES_TREATMENT_UNRESOLVED",
          
          coalesce(HAS_KAZAN,FALSE) ~
            "KAZAN_TREATMENT_UNRESOLVED",
          
          coalesce(HAS_KURILES,FALSE) ~
            "KURILES_TREATMENT_UNRESOLVED",
          
          TRUE ~
            "SPECIAL_AREA_TREATMENT_UNRESOLVED"
        )
    )
  
  # ---------------------------------------------------------------------------
  # Endemic Key subset after existing GX/HYB precedence
  # ---------------------------------------------------------------------------
  
  evidence <- evidence |>
    mutate(
      ELIGIBLE_ENDEMIC_KEY=
        !INTRODUCED_POSITIVE &
        !HYBRID_POSITIVE &
        ENDEMIC_STATUS=="ENDEMIC",
      
      ENDEMIC_SMALL_ISLAND_KEY_STATE=
        case_when(
          !ELIGIBLE_ENDEMIC_KEY ~
            "NOT_IN_ENDEMIC_KEY_SUBSET",
          
          SMALL_ISLAND_ROUTE_STATUS==
            "EXCLUSIVE_NAMED_SMALL_ISLANDS" ~
            "SOURCE_CRITERION_SATISFIED",
          
          SMALL_ISLAND_ROUTE_STATUS==
            "NOT_EXCLUSIVE_CORE_PRESENT" ~
            "CONTINUE_TO_DISTRICT_BRANCH",
          
          SMALL_ISLAND_ROUTE_STATUS==
            "NO_NAMED_SMALL_ISLAND" ~
            "CONTINUE_TO_DISTRICT_BRANCH",
          
          SMALL_ISLAND_ROUTE_STATUS==
            "UNRESOLVED_SPECIAL_AREA" ~
            "SPECIAL_AREA_METHOD_REVIEW",
          
          SMALL_ISLAND_ROUTE_STATUS==
            "NO_AREA_EVIDENCE" ~
            "AREA_EVIDENCE_REVIEW",
          
          TRUE ~
            "METHOD_REVIEW"
        ),
      
      SOURCE_TERMINAL_AVAILABLE=
        ELIGIBLE_ENDEMIC_KEY &
        ENDEMIC_SMALL_ISLAND_KEY_STATE==
        "SOURCE_CRITERION_SATISFIED",
      
      SOURCE_TERMINAL_CATEGORY=
        case_when(
          SOURCE_TERMINAL_AVAILABLE ~
            "BK",
          
          TRUE ~
            NA_character_
        )
    )
  
  # ---------------------------------------------------------------------------
  # Profiles
  # ---------------------------------------------------------------------------
  
  endemic_subset <- evidence |>
    filter(
      ELIGIBLE_ENDEMIC_KEY
    )
  
  if (nrow(endemic_subset)!=2141L) {
    stop(
      "Expected 2,141 eligible endemic-Key taxa; found ",
      format(
        nrow(endemic_subset),
        big.mark=","
      )
    )
  }
  
  profile <- endemic_subset |>
    count(
      ENDEMIC_SMALL_ISLAND_KEY_STATE,
      name="N_TAXA",
      sort=TRUE
    ) |>
    mutate(
      PERCENT_ENDEMIC_KEY=
        round(
          100*N_TAXA/
            nrow(endemic_subset),
          3
        )
    )
  
  special_profile <- endemic_subset |>
    filter(
      ENDEMIC_SMALL_ISLAND_KEY_STATE==
        "SPECIAL_AREA_METHOD_REVIEW"
    ) |>
    count(
      SPECIAL_AREA_REVIEW_REASON,
      name="N_TAXA",
      sort=TRUE
    )
  
  island_profile <- endemic_subset |>
    summarise(
      ENDEMIC_KEY_TAXA=n(),
      
      RYUKYU=
        sum(
          HAS_RYUKYU,
          na.rm=TRUE
        ),
      
      IZU=
        sum(
          HAS_IZU,
          na.rm=TRUE
        ),
      
      OGASAWARA=
        sum(
          HAS_OGASAWARA,
          na.rm=TRUE
        ),
      
      KAZAN=
        sum(
          HAS_KAZAN,
          na.rm=TRUE
        ),
      
      KURILES=
        sum(
          HAS_KURILES,
          na.rm=TRUE
        ),
      
      CORE_JAPAN=
        sum(
          HAS_CORE_JAPAN,
          na.rm=TRUE
        ),
      
      EXCLUSIVE_NAMED_SMALL_ISLANDS=
        sum(
          SOURCE_TERMINAL_AVAILABLE,
          na.rm=TRUE
        )
    )
  
  cat(
    "\n— ENDEMIC SMALL-ISLAND KEY PROFILE —\n"
  )
  
  print(
    profile,
    n=Inf
  )
  
  cat(
    "\n— ENDEMIC AREA EVIDENCE —\n"
  )
  
  print(
    island_profile,
    n=Inf
  )
  
  cat(
    "\n— SPECIAL-AREA METHOD REVIEW —\n"
  )
  
  if (nrow(special_profile)==0L) {
    cat(
      "No endemic taxa require special-area review.\n"
    )
  } else {
    print(
      special_profile,
      n=Inf
    )
  }
  
  # ---------------------------------------------------------------------------
  # Validation
  # ---------------------------------------------------------------------------
  
  validation <- tibble(
    CHECK=c(
      "Stars 03p validation fully PASS",
      "Stars 03s validation fully PASS",
      "Accepted population = 11,439",
      "Accepted population one row per WCVP ID",
      "Area IDs limited to canonical JP01-JP51",
      "All accepted area evidence uses non-missing area IDs",
      "Evidence output = 11,439 taxa",
      "Evidence output one row per WCVP ID",
      "Eligible endemic-Key population = 2,141",
      "All endemic-Key taxa have small-island Key state",
      "Exclusive taxa have named small-island evidence",
      "Exclusive taxa have no core-area evidence",
      "Exclusive taxa have no Kazan evidence",
      "Exclusive taxa have no Kuriles evidence",
      "Core-area taxa cannot be small-island exclusive",
      "Special-area review taxa not assigned exclusive TRUE",
      "Source terminal limited to BK",
      "Source-terminal BK limited to eligible endemic taxa",
      "No Stars assigned",
      "No Japanese district count created",
      "No Kazan treatment inferred",
      "No Kuriles treatment inferred",
      "Historical Star allocations not used"
    ),
    
    PASS=c(
      all(validation_03p$PASS),
      
      all(validation_03s$PASS),
      
      nrow(accepted)==11439L,
      
      n_distinct(
        accepted$FINAL_WCVP_ID
      )==11439L,
      
      length(
        unexpected_area_ids
      )==0L,
      
      !any(
        is.na(
          area_occ$BOTANICAL_AREA_ID
        )
      ),
      
      nrow(evidence)==11439L,
      
      n_distinct(
        evidence$FINAL_WCVP_ID
      )==11439L,
      
      nrow(endemic_subset)==2141L,
      
      all(
        !is.na(
          endemic_subset$
            ENDEMIC_SMALL_ISLAND_KEY_STATE
        )
      ),
      
      all(
        evidence$HAS_NAMED_SMALL_ISLAND[
          evidence$SOURCE_TERMINAL_AVAILABLE
        ]
      ),
      
      !any(
        evidence$HAS_CORE_JAPAN[
          evidence$SOURCE_TERMINAL_AVAILABLE
        ],
        na.rm=TRUE
      ),
      
      !any(
        evidence$HAS_KAZAN[
          evidence$SOURCE_TERMINAL_AVAILABLE
        ],
        na.rm=TRUE
      ),
      
      !any(
        evidence$HAS_KURILES[
          evidence$SOURCE_TERMINAL_AVAILABLE
        ],
        na.rm=TRUE
      ),
      
      !any(
        evidence$HAS_CORE_JAPAN &
          evidence$NAMED_SMALL_ISLAND_EXCLUSIVE %in%
          TRUE,
        na.rm=TRUE
      ),
      
      !any(
        evidence$
          SMALL_ISLAND_ROUTE_STATUS==
          "UNRESOLVED_SPECIAL_AREA" &
          evidence$
          NAMED_SMALL_ISLAND_EXCLUSIVE %in%
          TRUE,
        na.rm=TRUE
      ),
      
      all(
        na.omit(
          unique(
            evidence$
              SOURCE_TERMINAL_CATEGORY
          )
        )=="BK"
      ),
      
      all(
        evidence$
          ELIGIBLE_ENDEMIC_KEY[
            evidence$
              SOURCE_TERMINAL_AVAILABLE
          ]
      ),
      
      TRUE,
      
      !"JAPAN_DISTRICT_COUNT" %in%
        names(evidence),
      
      all(
        evidence$
          SMALL_ISLAND_ROUTE_STATUS[
            coalesce(
              evidence$HAS_KAZAN,
              FALSE
            ) &
              !coalesce(
                evidence$HAS_CORE_JAPAN,
                FALSE
              )
          ] %in%
          c(
            "UNRESOLVED_SPECIAL_AREA"
          )
      ),
      
      all(
        evidence$
          SMALL_ISLAND_ROUTE_STATUS[
            coalesce(
              evidence$HAS_KURILES,
              FALSE
            ) &
              !coalesce(
                evidence$HAS_CORE_JAPAN,
                FALSE
              )
          ] %in%
          c(
            "UNRESOLVED_SPECIAL_AREA"
          )
      ),
      
      TRUE
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
      "Stars 03t validation failed. ",
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
    
    ACCEPTED_TAXA=
      nrow(evidence),
    
    ELIGIBLE_ENDEMIC_KEY_TAXA=
      nrow(endemic_subset),
    
    SOURCE_TERMINAL_BK_AVAILABLE=
      sum(
        evidence$
          SOURCE_TERMINAL_AVAILABLE,
        na.rm=TRUE
      ),
    
    CONTINUE_TO_DISTRICT=
      sum(
        evidence$
          ENDEMIC_SMALL_ISLAND_KEY_STATE==
          "CONTINUE_TO_DISTRICT_BRANCH",
        na.rm=TRUE
      ),
    
    SPECIAL_AREA_METHOD_REVIEW=
      sum(
        evidence$
          ENDEMIC_SMALL_ISLAND_KEY_STATE==
          "SPECIAL_AREA_METHOD_REVIEW",
        na.rm=TRUE
      ),
    
    AREA_EVIDENCE_REVIEW=
      sum(
        evidence$
          ENDEMIC_SMALL_ISLAND_KEY_STATE==
          "AREA_EVIDENCE_REVIEW",
        na.rm=TRUE
      ),
    
    STARS_ASSIGNED=FALSE,
    JAPAN_DISTRICT_COUNT_CREATED=FALSE,
    KAZAN_TREATMENT_INFERRED=FALSE,
    KURILES_TREATMENT_INFERRED=FALSE,
    HISTORICAL_STAR_ALLOCATIONS_USED=FALSE,
    
    STATUS=
      "VALIDATED_EVIDENCE_MODULE"
  )
  
  # ---------------------------------------------------------------------------
  # Canonical outputs
  # ---------------------------------------------------------------------------
  
  dbWriteTable(
    con,
    TABLE_EVIDENCE,
    evidence,
    overwrite=TRUE
  )
  
  dbWriteTable(
    con,
    TABLE_PROFILE,
    profile,
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
    evidence,
    file.path(
      OUTPUT_DIR,
      "stars03t_endemic_small_island_evidence.csv"
    )
  )
  
  write_csv(
    profile,
    file.path(
      OUTPUT_DIR,
      "stars03t_endemic_small_island_profile.csv"
    )
  )
  
  write_csv(
    island_profile,
    file.path(
      OUTPUT_DIR,
      "stars03t_endemic_area_profile.csv"
    )
  )
  
  write_csv(
    special_profile,
    file.path(
      OUTPUT_DIR,
      "stars03t_special_area_review.csv"
    )
  )
  
  write_csv(
    validation,
    file.path(
      OUTPUT_DIR,
      "stars03t_validation.csv"
    )
  )
  
  write_csv(
    metadata,
    file.path(
      OUTPUT_DIR,
      "stars03t_metadata.csv"
    )
  )
  
  # ---------------------------------------------------------------------------
  # Final summary
  # ---------------------------------------------------------------------------
  
  cat(
    "\n============================================================\n"
  )
  
  cat(
    "Stars 03t v",
    VERSION,
    " COMPLETE\n",
    sep=""
  )
  
  cat(
    "============================================================\n"
  )
  
  cat(
    "Accepted taxa: ",
    format(
      nrow(evidence),
      big.mark=","
    ),
    "\n",
    sep=""
  )
  
  cat(
    "Eligible endemic-Key taxa: ",
    format(
      nrow(endemic_subset),
      big.mark=","
    ),
    "\n",
    sep=""
  )
  
  cat(
    "Source-terminal BK available: ",
    format(
      sum(
        evidence$
          SOURCE_TERMINAL_AVAILABLE,
        na.rm=TRUE
      ),
      big.mark=","
    ),
    "\n",
    sep=""
  )
  
  cat(
    "Continue to district branch: ",
    format(
      sum(
        evidence$
          ENDEMIC_SMALL_ISLAND_KEY_STATE==
          "CONTINUE_TO_DISTRICT_BRANCH",
        na.rm=TRUE
      ),
      big.mark=","
    ),
    "\n",
    sep=""
  )
  
  cat(
    "Special-area method review: ",
    format(
      sum(
        evidence$
          ENDEMIC_SMALL_ISLAND_KEY_STATE==
          "SPECIAL_AREA_METHOD_REVIEW",
        na.rm=TRUE
      ),
      big.mark=","
    ),
    "\n",
    sep=""
  )
  
  cat(
    "Area-evidence review: ",
    format(
      sum(
        evidence$
          ENDEMIC_SMALL_ISLAND_KEY_STATE==
          "AREA_EVIDENCE_REVIEW",
        na.rm=TRUE
      ),
      big.mark=","
    ),
    "\n",
    sep=""
  )
  
  cat(
    "Stars assigned: FALSE\n"
  )
  
  cat(
    "JAPAN_DISTRICT_COUNT created: FALSE\n"
  )
  
  cat(
    "Kazan treatment inferred: FALSE\n"
  )
  
  cat(
    "Kuriles treatment inferred: FALSE\n"
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
    "Canonical evidence: ",
    TABLE_EVIDENCE,
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
      evidence=evidence,
      endemic_subset=endemic_subset,
      profile=profile,
      island_profile=island_profile,
      special_profile=special_profile,
      validation=validation,
      metadata=metadata
    )
  )
}

stars_03t_result <- run_stars_03t()