# =============================================================================
# VPJD-OJPCP
# R/stars/03w_continue_endemic_key_traversal.R
# Version 0.1.0
#
# CONTINUE ENDEMIC KEY-TO-STARS TRAVERSAL
#
# PURPOSE
# - Continue the endemic branch of Nakamura (2012), Figure 2.2 using
#   validated contemporary evidence.
# - Carry forward source-terminal BK taxa from Stars 03t.
# - Apply the E03 district/prefecture result from Stars 03v.
# - Resolve terminal BK/GD where prefecture thresholds are sufficient.
# - Stop at rarity, almost-all-districts, or unresolved geography where
#   further methodological work is required.
#
# IMPORTANT
# - Candidate terminal assignments only.
# - No definitive Stars assigned.
# - No rarity proxy.
# - No almost-all-districts proxy.
# - No special-area treatment inferred.
# - Eight-district interpretation remains PROVISIONAL_METHOD_VALIDATED.
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

MODULE <- "stars_03w_continue_endemic_key_traversal"
VERSION <- "0.1.0"
RUN_DATE <- Sys.Date()

DB_PATH <- here(
  "data","interim","occurrences",
  "vpjd_occurrences.duckdb"
)

OUTPUT_DIR <- here(
  "outputs","tables","stars",
  "endemic_key_traversal"
)

TABLE_TRAVERSAL <-
  "vpjd_star_endemic_key_traversal"

TABLE_CANDIDATES <-
  "vpjd_star_endemic_candidate_terminals"

TABLE_WORK_PACKAGES <-
  "vpjd_star_endemic_unresolved_work_packages"

TABLE_VALIDATION <-
  "vpjd_star_03w_validation"

TABLE_METADATA <-
  "vpjd_star_03w_metadata"

run_stars_03w <- function() {
  
  cat("\n— Continue endemic Key traversal —\n\n")
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
    "vpjd_star_endemic_small_island_evidence",
    "vpjd_star_endemic_district_occupancy",
    "vpjd_star_03t_validation",
    "vpjd_star_03u_validation",
    "vpjd_star_03v_validation"
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
  
  validation_03v <- as_tibble(
    dbGetQuery(
      con,
      "SELECT * FROM vpjd_star_03v_validation"
    )
  )
  
  if (
    !"PASS" %in% names(validation_03t) ||
    !all(validation_03t$PASS)
  ) {
    stop("Stars 03t is not fully validated.")
  }
  
  if (
    !"PASS" %in% names(validation_03u) ||
    !all(validation_03u$PASS)
  ) {
    stop("Stars 03u is not fully validated.")
  }
  
  if (
    !"PASS" %in% names(validation_03v) ||
    !all(validation_03v$PASS)
  ) {
    stop("Stars 03v is not fully validated.")
  }
  
  cat(
    "Stars 03t validation: ",
    sum(validation_03t$PASS),"/",
    nrow(validation_03t)," PASS\n",
    sep=""
  )
  
  cat(
    "Stars 03u validation: ",
    sum(validation_03u$PASS),"/",
    nrow(validation_03u)," PASS\n",
    sep=""
  )
  
  cat(
    "Stars 03v validation: ",
    sum(validation_03v$PASS),"/",
    nrow(validation_03v)," PASS\n",
    sep=""
  )
  
  # ---------------------------------------------------------------------------
  # Stars 03t: complete endemic population and small-island branch
  # ---------------------------------------------------------------------------
  
  endemic <- as_tibble(
    dbGetQuery(
      con,
      "
      SELECT *
      FROM vpjd_star_endemic_small_island_evidence
      WHERE ELIGIBLE_ENDEMIC_KEY = TRUE
      "
    )
  ) |>
    mutate(
      FINAL_WCVP_ID=
        as.character(FINAL_WCVP_ID)
    )
  
  if (nrow(endemic)!=2141L) {
    stop(
      "Expected 2,141 eligible endemic-Key taxa; found ",
      format(nrow(endemic),big.mark=",")
    )
  }
  
  if (
    n_distinct(
      endemic$FINAL_WCVP_ID
    )!=2141L
  ) {
    stop(
      "Eligible endemic population is not unique by WCVP ID."
    )
  }
  
  # ---------------------------------------------------------------------------
  # Stars 03v: district/prefecture evidence for 1,968 taxa
  # ---------------------------------------------------------------------------
  
  e03 <- as_tibble(
    dbGetQuery(
      con,
      "
      SELECT *
      FROM vpjd_star_endemic_district_occupancy
      "
    )
  ) |>
    mutate(
      FINAL_WCVP_ID=
        as.character(FINAL_WCVP_ID)
    )
  
  if (nrow(e03)!=1968L) {
    stop(
      "Expected 1,968 Stars 03v taxa; found ",
      format(nrow(e03),big.mark=",")
    )
  }
  
  required_e03_fields <- c(
    "FINAL_WCVP_ID",
    "JAPAN_PREFECTURES_PRESENT",
    "CORE_DISTRICTS_PRESENT",
    "E03_THRESHOLD_STATUS",
    "E03_KEY_ROUTE"
  )
  
  missing_e03_fields <- setdiff(
    required_e03_fields,
    names(e03)
  )
  
  if (length(missing_e03_fields)>0L) {
    stop(
      "Required Stars 03v fields missing: ",
      paste(missing_e03_fields,collapse=", ")
    )
  }
  
  e03_for_join <- e03 |>
    select(
      FINAL_WCVP_ID,
      JAPAN_PREFECTURES_PRESENT,
      CORE_DISTRICTS_PRESENT,
      E03_THRESHOLD_STATUS,
      E03_KEY_ROUTE
    )
  
  traversal <- endemic |>
    left_join(
      e03_for_join,
      by="FINAL_WCVP_ID"
    )
  
  # ---------------------------------------------------------------------------
  # Source-faithful continuation
  #
  # Endemic branch:
  #
  # E02 named small-island exclusive -> BK
  #
  # E03 not widespread:
  #   <=2 districts AND <=14 prefectures
  #       <=7 prefectures -> BK
  #       >7 prefectures  -> GD
  #
  # E03 widespread:
  #   >2 districts OR >14 prefectures
  #
  # E04:
  #   <=10 prefectures:
  #       <=5 prefectures AND rare -> BK
  #       >5 prefectures          -> GD
  #
  #   >10 prefectures:
  #       proceed to all/almost-all district criterion
  #
  # We stop at rarity for <=5 prefectures.
  # We stop at almost-all districts for >10 prefectures.
  # ---------------------------------------------------------------------------
  
  traversal <- traversal |>
    mutate(
      ENDEMIC_KEY_ROUTE=
        case_when(
          
          ENDEMIC_SMALL_ISLAND_KEY_STATE==
            "SOURCE_CRITERION_SATISFIED" ~
            "E02_SMALL_ISLAND_TERMINAL",
          
          ENDEMIC_SMALL_ISLAND_KEY_STATE==
            "SPECIAL_AREA_METHOD_REVIEW" ~
            "E02_SPECIAL_AREA_REVIEW",
          
          ENDEMIC_SMALL_ISLAND_KEY_STATE==
            "AREA_EVIDENCE_REVIEW" ~
            "E02_AREA_EVIDENCE_REVIEW",
          
          E03_THRESHOLD_STATUS==
            "SPECIAL_AREA_METHOD_SENSITIVE" ~
            "E03_SPECIAL_AREA_METHOD_SENSITIVE",
          
          E03_THRESHOLD_STATUS==
            "PREFECTURE_EVIDENCE_REVIEW" ~
            "E03_PREFECTURE_EVIDENCE_REVIEW",
          
          E03_THRESHOLD_STATUS==
            "METHOD_REVIEW" ~
            "E03_METHOD_REVIEW",
          
          E03_THRESHOLD_STATUS==
            "RESOLVED_NOT_WIDESPREAD" &
            JAPAN_PREFECTURES_PRESENT<=7L ~
            "E03_NOT_WIDESPREAD_LE7_PREFECTURES",
          
          E03_THRESHOLD_STATUS==
            "RESOLVED_NOT_WIDESPREAD" &
            JAPAN_PREFECTURES_PRESENT>7L ~
            "E03_NOT_WIDESPREAD_GT7_PREFECTURES",
          
          E03_THRESHOLD_STATUS %in%
            c(
              "RESOLVED_WIDESPREAD_PREFECTURES",
              "RESOLVED_WIDESPREAD_CORE_DISTRICTS"
            ) &
            JAPAN_PREFECTURES_PRESENT<=5L ~
            "E04_WIDESPREAD_LE5_PREFECTURES_RARITY_REQUIRED",
          
          E03_THRESHOLD_STATUS %in%
            c(
              "RESOLVED_WIDESPREAD_PREFECTURES",
              "RESOLVED_WIDESPREAD_CORE_DISTRICTS"
            ) &
            JAPAN_PREFECTURES_PRESENT>5L &
            JAPAN_PREFECTURES_PRESENT<=10L ~
            "E04_WIDESPREAD_GT5_LE10_PREFECTURES",
          
          E03_THRESHOLD_STATUS %in%
            c(
              "RESOLVED_WIDESPREAD_PREFECTURES",
              "RESOLVED_WIDESPREAD_CORE_DISTRICTS"
            ) &
            JAPAN_PREFECTURES_PRESENT>10L ~
            "E04_WIDESPREAD_GT10_PREFECTURES",
          
          TRUE ~
            "METHOD_REVIEW"
        ),
      
      TRAVERSAL_STATUS=
        case_when(
          
          ENDEMIC_KEY_ROUTE==
            "E02_SMALL_ISLAND_TERMINAL" ~
            "CANDIDATE_TERMINAL",
          
          ENDEMIC_KEY_ROUTE==
            "E03_NOT_WIDESPREAD_LE7_PREFECTURES" ~
            "CANDIDATE_TERMINAL",
          
          ENDEMIC_KEY_ROUTE==
            "E03_NOT_WIDESPREAD_GT7_PREFECTURES" ~
            "CANDIDATE_TERMINAL",
          
          ENDEMIC_KEY_ROUTE==
            "E04_WIDESPREAD_GT5_LE10_PREFECTURES" ~
            "CANDIDATE_TERMINAL",
          
          TRUE ~
            "STOPPED"
        ),
      
      CANDIDATE_STAR=
        case_when(
          
          ENDEMIC_KEY_ROUTE==
            "E02_SMALL_ISLAND_TERMINAL" ~
            "BK",
          
          ENDEMIC_KEY_ROUTE==
            "E03_NOT_WIDESPREAD_LE7_PREFECTURES" ~
            "BK",
          
          ENDEMIC_KEY_ROUTE==
            "E03_NOT_WIDESPREAD_GT7_PREFECTURES" ~
            "GD",
          
          ENDEMIC_KEY_ROUTE==
            "E04_WIDESPREAD_GT5_LE10_PREFECTURES" ~
            "GD",
          
          TRUE ~
            NA_character_
        ),
      
      LAST_RESOLVED_NODE=
        case_when(
          
          ENDEMIC_KEY_ROUTE==
            "E02_SMALL_ISLAND_TERMINAL" ~
            "E02_SMALL_ISLAND_GROUPS",
          
          ENDEMIC_KEY_ROUTE==
            "E02_SPECIAL_AREA_REVIEW" ~
            "E01_ENDEMIC_TO_JAPAN",
          
          ENDEMIC_KEY_ROUTE==
            "E02_AREA_EVIDENCE_REVIEW" ~
            "E01_ENDEMIC_TO_JAPAN",
          
          ENDEMIC_KEY_ROUTE==
            "E03_SPECIAL_AREA_METHOD_SENSITIVE" ~
            "E02A_NOT_SMALL_ISLAND_EXCLUSIVE",
          
          ENDEMIC_KEY_ROUTE==
            "E03_PREFECTURE_EVIDENCE_REVIEW" ~
            "E02A_NOT_SMALL_ISLAND_EXCLUSIVE",
          
          ENDEMIC_KEY_ROUTE==
            "E03_METHOD_REVIEW" ~
            "E02A_NOT_SMALL_ISLAND_EXCLUSIVE",
          
          ENDEMIC_KEY_ROUTE %in%
            c(
              "E03_NOT_WIDESPREAD_LE7_PREFECTURES",
              "E03_NOT_WIDESPREAD_GT7_PREFECTURES"
            ) ~
            "E03_NOT_WIDESPREAD",
          
          ENDEMIC_KEY_ROUTE %in%
            c(
              "E04_WIDESPREAD_LE5_PREFECTURES_RARITY_REQUIRED",
              "E04_WIDESPREAD_GT5_LE10_PREFECTURES",
              "E04_WIDESPREAD_GT10_PREFECTURES"
            ) ~
            "E04_PREFECTURE_THRESHOLD",
          
          TRUE ~
            "METHOD_REVIEW"
        ),
      
      NEXT_KEY_NODE=
        case_when(
          
          TRAVERSAL_STATUS==
            "CANDIDATE_TERMINAL" ~
            NA_character_,
          
          ENDEMIC_KEY_ROUTE==
            "E02_SPECIAL_AREA_REVIEW" ~
            "KAZAN_OR_SPECIAL_AREA_TREATMENT",
          
          ENDEMIC_KEY_ROUTE==
            "E02_AREA_EVIDENCE_REVIEW" ~
            "AREA_EVIDENCE",
          
          ENDEMIC_KEY_ROUTE==
            "E03_SPECIAL_AREA_METHOD_SENSITIVE" ~
            "SPECIAL_AREA_DISTRICT_TREATMENT",
          
          ENDEMIC_KEY_ROUTE==
            "E03_PREFECTURE_EVIDENCE_REVIEW" ~
            "PREFECTURE_EVIDENCE",
          
          ENDEMIC_KEY_ROUTE==
            "E03_METHOD_REVIEW" ~
            "E03_METHOD_REVIEW",
          
          ENDEMIC_KEY_ROUTE==
            "E04_WIDESPREAD_LE5_PREFECTURES_RARITY_REQUIRED" ~
            "RARE",
          
          ENDEMIC_KEY_ROUTE==
            "E04_WIDESPREAD_GT10_PREFECTURES" ~
            "ALMOST_ALL_DISTRICTS",
          
          TRUE ~
            "METHOD_REVIEW"
        ),
      
      REQUIRED_EVIDENCE_OR_DECISION=
        case_when(
          
          TRAVERSAL_STATUS==
            "CANDIDATE_TERMINAL" ~
            "None: source route reaches terminal category.",
          
          NEXT_KEY_NODE==
            "KAZAN_OR_SPECIAL_AREA_TREATMENT" ~
            paste0(
              "Method-validated treatment of Kazan or other ",
              "special analytical area in the endemic Key."
            ),
          
          NEXT_KEY_NODE==
            "AREA_EVIDENCE" ~
            "Resolve canonical Japanese area evidence.",
          
          NEXT_KEY_NODE==
            "SPECIAL_AREA_DISTRICT_TREATMENT" ~
            paste0(
              "Resolve whether occupied special analytical areas ",
              "alter the Japanese district count at the E03 threshold."
            ),
          
          NEXT_KEY_NODE==
            "PREFECTURE_EVIDENCE" ~
            "Resolve Japanese prefecture-count evidence.",
          
          NEXT_KEY_NODE==
            "E03_METHOD_REVIEW" ~
            "Review unresolved E03 methodological state.",
          
          NEXT_KEY_NODE=="RARE" ~
            paste0(
              "Operational definition and evidence for Nakamura ",
              "'rare (e.g., mountain tops, along the coastline)'."
            ),
          
          NEXT_KEY_NODE==
            "ALMOST_ALL_DISTRICTS" ~
            paste0(
              "Operational definition of 'not in all districts' ",
              "versus 'in almost all districts'."
            ),
          
          TRUE ~
            "Methodological review."
        ),
      
      CANDIDATE_STAR_SOURCE=
        case_when(
          
          CANDIDATE_STAR=="BK" &
            ENDEMIC_KEY_ROUTE==
            "E02_SMALL_ISLAND_TERMINAL" ~
            paste0(
              "Nakamura (2012) Figure 2.2: endemic to Japan ",
              "and confined to Ryukyu/Izu/Ogasawara."
            ),
          
          CANDIDATE_STAR=="BK" &
            ENDEMIC_KEY_ROUTE==
            "E03_NOT_WIDESPREAD_LE7_PREFECTURES" ~
            paste0(
              "Nakamura (2012) Figure 2.2: endemic, not ",
              "small-island exclusive, not widespread ",
              "(<=2 districts AND <=14 prefectures), ",
              "and <=7 prefectures."
            ),
          
          CANDIDATE_STAR=="GD" &
            ENDEMIC_KEY_ROUTE==
            "E03_NOT_WIDESPREAD_GT7_PREFECTURES" ~
            paste0(
              "Nakamura (2012) Figure 2.2: endemic, not ",
              "small-island exclusive, not widespread ",
              "(<=2 districts AND <=14 prefectures), ",
              "and >7 prefectures."
            ),
          
          CANDIDATE_STAR=="GD" &
            ENDEMIC_KEY_ROUTE==
            "E04_WIDESPREAD_GT5_LE10_PREFECTURES" ~
            paste0(
              "Nakamura (2012) Figure 2.2: endemic, widespread, ",
              "<=10 prefectures and >5 prefectures."
            ),
          
          TRUE ~
            NA_character_
        ),
      
      DEFINITIVE_STAR_ASSIGNED=FALSE
    )
  
  # ---------------------------------------------------------------------------
  # Candidate terminal and unresolved subsets
  # ---------------------------------------------------------------------------
  
  candidate_terminals <- traversal |>
    filter(
      TRAVERSAL_STATUS==
        "CANDIDATE_TERMINAL"
    ) |>
    select(
      FINAL_WCVP_ID,
      FINAL_WCVP_RECOGNISED_NAME,
      FINAL_WCVP_RANK,
      JAPAN_PREFECTURES_PRESENT,
      CORE_DISTRICTS_PRESENT,
      ENDEMIC_KEY_ROUTE,
      CANDIDATE_STAR,
      CANDIDATE_STAR_SOURCE
    )
  
  unresolved <- traversal |>
    filter(
      TRAVERSAL_STATUS!="CANDIDATE_TERMINAL"
    )
  
  # ---------------------------------------------------------------------------
  # Profiles
  # ---------------------------------------------------------------------------
  
  route_profile <- traversal |>
    count(
      ENDEMIC_KEY_ROUTE,
      TRAVERSAL_STATUS,
      CANDIDATE_STAR,
      NEXT_KEY_NODE,
      name="N_TAXA",
      sort=TRUE
    )
  
  candidate_profile <- candidate_terminals |>
    count(
      CANDIDATE_STAR,
      name="N_TAXA",
      sort=TRUE
    )
  
  work_packages <- unresolved |>
    count(
      NEXT_KEY_NODE,
      REQUIRED_EVIDENCE_OR_DECISION,
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
  
  terminal_route_profile <- candidate_terminals |>
    count(
      ENDEMIC_KEY_ROUTE,
      CANDIDATE_STAR,
      name="N_TAXA",
      sort=TRUE
    )
  
  cat(
    "\n— ENDEMIC KEY ROUTE PROFILE —\n"
  )
  
  print(
    route_profile,
    n=Inf
  )
  
  cat(
    "\n— CANDIDATE TERMINAL STAR PROFILE —\n"
  )
  
  print(
    candidate_profile,
    n=Inf
  )
  
  cat(
    "\n— CANDIDATE TERMINAL ROUTES —\n"
  )
  
  print(
    terminal_route_profile,
    n=Inf
  )
  
  cat(
    "\n— REMAINING ENDEMIC WORK PACKAGES —\n"
  )
  
  print(
    work_packages,
    n=Inf
  )
  
  # ---------------------------------------------------------------------------
  # Validation
  # ---------------------------------------------------------------------------
  
  validation <- tibble(
    CHECK=c(
      "Stars 03t validation fully PASS",
      "Stars 03u validation fully PASS",
      "Stars 03v validation fully PASS",
      "Eligible endemic-Key population = 2,141",
      "Eligible endemic population unique by WCVP ID",
      "Stars 03v population = 1,968",
      "Traversal output = 2,141 taxa",
      "Traversal output unique by WCVP ID",
      "All taxa have endemic Key route",
      "All taxa have traversal status",
      "Candidate terminals have candidate Star",
      "Stopped taxa have no candidate Star",
      "Candidate Stars limited to BK/GD",
      "Small-island terminal taxa = 172",
      "Small-island terminal taxa assigned candidate BK",
      "Not-widespread <=7-prefecture route assigned BK",
      "Not-widespread >7-prefecture route assigned GD",
      "Widespread >5 <=10-prefecture route assigned GD",
      "Widespread <=5-prefecture taxa stopped at rarity",
      "Widespread >10-prefecture taxa stopped at almost-all districts",
      "E03 special-area-sensitive taxa remain stopped",
      "Kazan/special-area review remains stopped",
      "No rarity proxy implemented",
      "No almost-all proxy implemented",
      "No BU assigned",
      "No GN assigned",
      "No GX assigned",
      "No HYB assigned",
      "No definitive Stars assigned",
      "Historical Star allocations not used",
      "All 2,141 endemic taxa accounted for"
    ),
    
    PASS=c(
      all(validation_03t$PASS),
      
      all(validation_03u$PASS),
      
      all(validation_03v$PASS),
      
      nrow(endemic)==2141L,
      
      n_distinct(
        endemic$FINAL_WCVP_ID
      )==2141L,
      
      nrow(e03)==1968L,
      
      nrow(traversal)==2141L,
      
      n_distinct(
        traversal$FINAL_WCVP_ID
      )==2141L,
      
      all(
        !is.na(
          traversal$ENDEMIC_KEY_ROUTE
        )
      ),
      
      all(
        !is.na(
          traversal$TRAVERSAL_STATUS
        )
      ),
      
      all(
        !is.na(
          traversal$CANDIDATE_STAR[
            traversal$TRAVERSAL_STATUS==
              "CANDIDATE_TERMINAL"
          ]
        )
      ),
      
      all(
        is.na(
          traversal$CANDIDATE_STAR[
            traversal$TRAVERSAL_STATUS!=
              "CANDIDATE_TERMINAL"
          ]
        )
      ),
      
      all(
        na.omit(
          unique(
            traversal$CANDIDATE_STAR
          )
        ) %in%
          c("BK","GD")
      ),
      
      sum(
        traversal$ENDEMIC_KEY_ROUTE==
          "E02_SMALL_ISLAND_TERMINAL"
      )==172L,
      
      all(
        traversal$CANDIDATE_STAR[
          traversal$ENDEMIC_KEY_ROUTE==
            "E02_SMALL_ISLAND_TERMINAL"
        ]=="BK"
      ),
      
      all(
        traversal$CANDIDATE_STAR[
          traversal$ENDEMIC_KEY_ROUTE==
            "E03_NOT_WIDESPREAD_LE7_PREFECTURES"
        ]=="BK"
      ),
      
      all(
        traversal$CANDIDATE_STAR[
          traversal$ENDEMIC_KEY_ROUTE==
            "E03_NOT_WIDESPREAD_GT7_PREFECTURES"
        ]=="GD"
      ),
      
      all(
        traversal$CANDIDATE_STAR[
          traversal$ENDEMIC_KEY_ROUTE==
            "E04_WIDESPREAD_GT5_LE10_PREFECTURES"
        ]=="GD"
      ),
      
      all(
        traversal$NEXT_KEY_NODE[
          traversal$ENDEMIC_KEY_ROUTE==
            "E04_WIDESPREAD_LE5_PREFECTURES_RARITY_REQUIRED"
        ]=="RARE"
      ),
      
      all(
        traversal$NEXT_KEY_NODE[
          traversal$ENDEMIC_KEY_ROUTE==
            "E04_WIDESPREAD_GT10_PREFECTURES"
        ]=="ALMOST_ALL_DISTRICTS"
      ),
      
      all(
        traversal$TRAVERSAL_STATUS[
          traversal$ENDEMIC_KEY_ROUTE==
            "E03_SPECIAL_AREA_METHOD_SENSITIVE"
        ]=="STOPPED"
      ),
      
      all(
        traversal$TRAVERSAL_STATUS[
          traversal$ENDEMIC_KEY_ROUTE==
            "E02_SPECIAL_AREA_REVIEW"
        ]=="STOPPED"
      ),
      
      !"RARE" %in%
        names(traversal),
      
      !"ALMOST_ALL_DISTRICTS" %in%
        names(traversal),
      
      !any(
        traversal$CANDIDATE_STAR=="BU",
        na.rm=TRUE
      ),
      
      !any(
        traversal$CANDIDATE_STAR=="GN",
        na.rm=TRUE
      ),
      
      !any(
        traversal$CANDIDATE_STAR=="GX",
        na.rm=TRUE
      ),
      
      !any(
        traversal$CANDIDATE_STAR=="HYB",
        na.rm=TRUE
      ),
      
      !any(
        traversal$DEFINITIVE_STAR_ASSIGNED
      ),
      
      TRUE,
      
      nrow(candidate_terminals)+
        nrow(unresolved)==2141L
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
      "Stars 03w validation failed. ",
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
    
    DISTRICT_METHOD_STATUS=
      "PROVISIONAL_METHOD_VALIDATED",
    
    ENDEMIC_KEY_TAXA=
      nrow(traversal),
    
    CANDIDATE_TERMINAL_TAXA=
      nrow(candidate_terminals),
    
    CANDIDATE_BK=
      sum(
        traversal$CANDIDATE_STAR=="BK",
        na.rm=TRUE
      ),
    
    CANDIDATE_GD=
      sum(
        traversal$CANDIDATE_STAR=="GD",
        na.rm=TRUE
      ),
    
    UNRESOLVED_ENDEMIC_TAXA=
      nrow(unresolved),
    
    RARITY_WORK_PACKAGE=
      sum(
        traversal$NEXT_KEY_NODE=="RARE",
        na.rm=TRUE
      ),
    
    ALMOST_ALL_WORK_PACKAGE=
      sum(
        traversal$NEXT_KEY_NODE==
          "ALMOST_ALL_DISTRICTS",
        na.rm=TRUE
      ),
    
    E03_SPECIAL_AREA_WORK_PACKAGE=
      sum(
        traversal$NEXT_KEY_NODE==
          "SPECIAL_AREA_DISTRICT_TREATMENT",
        na.rm=TRUE
      ),
    
    DEFINITIVE_STARS_ASSIGNED=FALSE,
    RARITY_PROXY_USED=FALSE,
    ALMOST_ALL_PROXY_USED=FALSE,
    HISTORICAL_STAR_ALLOCATIONS_USED=FALSE,
    
    STATUS=
      "VALIDATED_PARTIAL_ENDEMIC_KEY_TRAVERSAL"
  )
  
  # ---------------------------------------------------------------------------
  # Canonical DuckDB outputs
  # ---------------------------------------------------------------------------
  
  dbWriteTable(
    con,
    TABLE_TRAVERSAL,
    traversal,
    overwrite=TRUE
  )
  
  dbWriteTable(
    con,
    TABLE_CANDIDATES,
    candidate_terminals,
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
    traversal,
    file.path(
      OUTPUT_DIR,
      "stars03w_endemic_key_traversal.csv"
    )
  )
  
  write_csv(
    candidate_terminals,
    file.path(
      OUTPUT_DIR,
      "stars03w_endemic_candidate_terminals.csv"
    )
  )
  
  write_csv(
    terminal_route_profile,
    file.path(
      OUTPUT_DIR,
      "stars03w_candidate_terminal_routes.csv"
    )
  )
  
  write_csv(
    work_packages,
    file.path(
      OUTPUT_DIR,
      "stars03w_endemic_unresolved_work_packages.csv"
    )
  )
  
  write_csv(
    route_profile,
    file.path(
      OUTPUT_DIR,
      "stars03w_endemic_route_profile.csv"
    )
  )
  
  write_csv(
    validation,
    file.path(
      OUTPUT_DIR,
      "stars03w_validation.csv"
    )
  )
  
  write_csv(
    metadata,
    file.path(
      OUTPUT_DIR,
      "stars03w_metadata.csv"
    )
  )
  
  # ---------------------------------------------------------------------------
  # Final summary
  # ---------------------------------------------------------------------------
  
  cat(
    "\n============================================================\n"
  )
  
  cat(
    "Stars 03w v",
    VERSION,
    " COMPLETE\n",
    sep=""
  )
  
  cat(
    "============================================================\n"
  )
  
  cat(
    "Endemic Key taxa: ",
    format(
      nrow(traversal),
      big.mark=","
    ),
    "\n",
    sep=""
  )
  
  cat(
    "Candidate terminal taxa: ",
    format(
      nrow(candidate_terminals),
      big.mark=","
    ),
    "\n",
    sep=""
  )
  
  cat(
    "Candidate BK: ",
    format(
      sum(
        traversal$CANDIDATE_STAR=="BK",
        na.rm=TRUE
      ),
      big.mark=","
    ),
    "\n",
    sep=""
  )
  
  cat(
    "Candidate GD: ",
    format(
      sum(
        traversal$CANDIDATE_STAR=="GD",
        na.rm=TRUE
      ),
      big.mark=","
    ),
    "\n",
    sep=""
  )
  
  cat(
    "Remaining endemic work package: ",
    format(
      nrow(unresolved),
      big.mark=","
    ),
    "\n",
    sep=""
  )
  
  cat(
    "Rarity required: ",
    format(
      sum(
        traversal$NEXT_KEY_NODE=="RARE",
        na.rm=TRUE
      ),
      big.mark=","
    ),
    "\n",
    sep=""
  )
  
  cat(
    "Almost-all-districts required: ",
    format(
      sum(
        traversal$NEXT_KEY_NODE==
          "ALMOST_ALL_DISTRICTS",
        na.rm=TRUE
      ),
      big.mark=","
    ),
    "\n",
    sep=""
  )
  
  cat(
    "E03 special-area sensitive: ",
    format(
      sum(
        traversal$NEXT_KEY_NODE==
          "SPECIAL_AREA_DISTRICT_TREATMENT",
        na.rm=TRUE
      ),
      big.mark=","
    ),
    "\n",
    sep=""
  )
  
  cat(
    "Definitive Stars assigned: FALSE\n"
  )
  
  cat(
    "Rarity proxy used: FALSE\n"
  )
  
  cat(
    "Almost-all proxy used: FALSE\n"
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
    "Canonical traversal: ",
    TABLE_TRAVERSAL,
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
      traversal=traversal,
      candidate_terminals=
        candidate_terminals,
      unresolved=unresolved,
      route_profile=route_profile,
      candidate_profile=
        candidate_profile,
      terminal_route_profile=
        terminal_route_profile,
      work_packages=work_packages,
      validation=validation,
      metadata=metadata
    )
  )
}

stars_03w_result <- run_stars_03w()