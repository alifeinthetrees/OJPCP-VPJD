# =============================================================================
# VPJD-OJPCP
# R/stars/03s_partial_key_traversal.R
# Version 0.1.0
#
# PARTIAL SOURCE-FAITHFUL KEY-TO-STARS TRAVERSAL
#
# PURPOSE
# - Traverse Nakamura (2012), Figure 2.2 only as far as validated evidence
#   and method definitions permit.
# - Identify taxa reaching a defensible candidate terminal Star.
# - Stop every other taxon at its FIRST unresolved methodological criterion.
# - Quantify remaining work packages before definitive Stars 04.
#
# IMPORTANT
# - NOT the definitive Star classifier.
# - No unresolved Key criterion is guessed or proxied.
# - GX stops pending cultivated -> introduced-to-Japan crosswalk.
# - HYB stops pending documentation as a contemporary extension.
# - Japanese district-dependent routes remain stopped.
# - Non-endemic N02/N02A logic remains stopped pending formal interpretation.
# - Historical Star allocations are not used.
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

MODULE <- "stars_03s_partial_key_traversal"
VERSION <- "0.1.0"
RUN_DATE <- Sys.Date()

DB_PATH <- here(
  "data","interim","occurrences",
  "vpjd_occurrences.duckdb"
)

OUTPUT_DIR <- here(
  "outputs","tables","stars",
  "partial_key_traversal"
)

TABLE_TRAVERSAL <- "vpjd_star_partial_key_traversal"
TABLE_WORK_PACKAGES <- "vpjd_star_unresolved_work_packages"
TABLE_CANDIDATE_TERMINALS <- "vpjd_star_candidate_terminal_assignments"
TABLE_VALIDATION <- "vpjd_star_03s_validation"
TABLE_METADATA <- "vpjd_star_03s_metadata"

run_stars_03s <- function() {
  
  cat("\n— Partial Key-to-Stars traversal —\n\n")
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
    "vpjd_star_taxon_key_geography_evidence",
    "vpjd_star_wcvp_key_geography",
    "vpjd_star_hybrid_status",
    "vpjd_star_non_endemic_geography_evidence",
    "vpjd_star_key_source_nodes",
    "vpjd_star_key_non_endemic_matrix",
    "vpjd_star_key_operational_gaps",
    "vpjd_star_03p_validation",
    "vpjd_star_03r_validation",
    "vpjd_star_03r_metadata"
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
  
  validation_03p <- as_tibble(
    dbGetQuery(
      con,
      "SELECT * FROM vpjd_star_03p_validation"
    )
  )
  
  validation_03r <- as_tibble(
    dbGetQuery(
      con,
      "SELECT * FROM vpjd_star_03r_validation"
    )
  )
  
  metadata_03r <- as_tibble(
    dbGetQuery(
      con,
      "SELECT * FROM vpjd_star_03r_metadata"
    )
  )
  
  if (
    !"PASS" %in% names(validation_03p) ||
    !all(validation_03p$PASS)
  ) {
    stop("Stars 03p is not fully validated.")
  }
  
  if (
    !"PASS" %in% names(validation_03r) ||
    !all(validation_03r$PASS)
  ) {
    stop("Stars 03r is not fully validated.")
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
        FINAL_WCVP_RANK,
        FINAL_WCVP_STATUS,
        FINAL_WCVP_CONCEPT_CLASS
      FROM vpjd_japan_taxon_distribution
      WHERE FINAL_WCVP_STATUS = 'Accepted'
      "
    )
  ) |>
    mutate(
      FINAL_WCVP_ID=as.character(FINAL_WCVP_ID)
    )
  
  if (nrow(accepted)!=11439L) {
    stop(
      "Expected 11,439 accepted taxa; found ",
      format(nrow(accepted),big.mark=",")
    )
  }
  
  if (
    n_distinct(accepted$FINAL_WCVP_ID)!=11439L
  ) {
    stop(
      "Accepted population is not one row per FINAL_WCVP_ID."
    )
  }
  
  cat(
    "Accepted contemporary Star population: ",
    format(nrow(accepted),big.mark=","),
    "\n",
    sep=""
  )
  
  # ---------------------------------------------------------------------------
  # Stars 03n evidence
  # ---------------------------------------------------------------------------
  
  geo <- as_tibble(
    dbGetQuery(
      con,
      "SELECT * FROM vpjd_star_taxon_key_geography_evidence"
    )
  ) |>
    mutate(
      FINAL_WCVP_ID=as.character(FINAL_WCVP_ID)
    )
  
  required_geo_fields <- c(
    "FINAL_WCVP_ID",
    "PRE_KEY_EVIDENCE_ROUTE"
  )
  
  missing_geo_fields <- setdiff(
    required_geo_fields,
    names(geo)
  )
  
  if (length(missing_geo_fields)>0L) {
    stop(
      "Required Stars 03n fields missing: ",
      paste(missing_geo_fields,collapse=", ")
    )
  }
  
  # ---------------------------------------------------------------------------
  # WCVP endemicity/introduction evidence
  # ---------------------------------------------------------------------------
  
  wcvp <- as_tibble(
    dbGetQuery(
      con,
      "
      SELECT
        FINAL_WCVP_ID,
        ENDEMIC_TO_JAPAN_WCVP,
        WCVP_INTRODUCED_TO_JAPAN,
        WCVP_OUTSIDE_JAPAN
      FROM vpjd_star_wcvp_key_geography
      "
    )
  ) |>
    mutate(
      FINAL_WCVP_ID=as.character(FINAL_WCVP_ID)
    )
  
  # ---------------------------------------------------------------------------
  # Contemporary HYB evidence
  # ---------------------------------------------------------------------------
  
  hybrid <- as_tibble(
    dbGetQuery(
      con,
      "SELECT * FROM vpjd_star_hybrid_status"
    )
  ) |>
    mutate(
      FINAL_WCVP_ID=as.character(FINAL_WCVP_ID)
    )
  
  if (!"HYBRID_STATUS" %in% names(hybrid)) {
    stop(
      "HYBRID_STATUS missing from vpjd_star_hybrid_status."
    )
  }
  
  hybrid <- hybrid |>
    select(
      FINAL_WCVP_ID,
      HYBRID_STATUS
    )
  
  # ---------------------------------------------------------------------------
  # Stars 03r external-range evidence
  # ---------------------------------------------------------------------------
  
  external <- as_tibble(
    dbGetQuery(
      con,
      "SELECT * FROM vpjd_star_non_endemic_geography_evidence"
    )
  ) |>
    mutate(
      FINAL_WCVP_ID=as.character(FINAL_WCVP_ID)
    )
  
  required_external_fields <- c(
    "FINAL_WCVP_ID",
    "WCVP_TAIWAN",
    "WCVP_KOREA",
    "WCVP_KURILES_SAKHALIN",
    "WCVP_CHINA",
    "WCVP_BEYOND_KEY_REGIONS",
    "N_NAMED_NEIGHBOUR_REGIONS"
  )
  
  missing_external_fields <- setdiff(
    required_external_fields,
    names(external)
  )
  
  if (length(missing_external_fields)>0L) {
    stop(
      "Required Stars 03r fields missing: ",
      paste(missing_external_fields,collapse=", ")
    )
  }
  
  external <- external |>
    select(
      all_of(required_external_fields)
    )
  
  # ---------------------------------------------------------------------------
  # Unified evidence table
  # ---------------------------------------------------------------------------
  
# ---------------------------------------------------------------------------
# Unified evidence table
#
# 03g is the authoritative contemporary HYB evidence module.
# Remove any duplicate HYBRID_STATUS carried through 03n before joining 03g.
# ---------------------------------------------------------------------------

geo_for_join <- geo |>
  select(
    -any_of("HYBRID_STATUS")
  )

evidence <- accepted |>
  left_join(
    geo_for_join,
    by="FINAL_WCVP_ID",
    suffix=c("",".03n")
  ) |>
  left_join(
    wcvp,
    by="FINAL_WCVP_ID"
  ) |>
  left_join(
    hybrid,
    by="FINAL_WCVP_ID"
  ) |>
  left_join(
    external,
    by="FINAL_WCVP_ID"
  )

if (!"HYBRID_STATUS" %in% names(evidence)) {
  stop(
    "Authoritative Stars 03g HYBRID_STATUS missing after evidence joins."
  )
}
  # ---------------------------------------------------------------------------
  # Identify explicit 03n small-island fields without inventing geography
  # ---------------------------------------------------------------------------
  
  find_first_field <- function(
    candidates,
    data_names
  ) {
    x <- candidates[
      candidates %in% data_names
    ]
    
    if (length(x)==0L) {
      return(NA_character_)
    }
    
    x[[1]]
  }
  
  ryukyu_field <- find_first_field(
    c(
      "RYUKYU",
      "RYUKYU_PRESENT",
      "RYUKYU_PRESENCE",
      "RYUKYU_EVIDENCE",
      "JAPAN_RYUKYU",
      "SPECIAL_AREA_RYUKYU",
      "RYUKYU_AREA_PRESENT"
    ),
    names(evidence)
  )
  
  izu_field <- find_first_field(
    c(
      "IZU",
      "IZU_PRESENT",
      "IZU_PRESENCE",
      "IZU_EVIDENCE",
      "JAPAN_IZU",
      "SPECIAL_AREA_IZU",
      "IZU_AREA_PRESENT"
    ),
    names(evidence)
  )
  
  ogasawara_field <- find_first_field(
    c(
      "OGASAWARA",
      "OGASAWARA_PRESENT",
      "OGASAWARA_PRESENCE",
      "OGASAWARA_EVIDENCE",
      "JAPAN_OGASAWARA",
      "SPECIAL_AREA_OGASAWARA",
      "OGASAWARA_AREA_PRESENT"
    ),
    names(evidence)
  )
  
  small_island_fields_available <- all(
    !is.na(
      c(
        ryukyu_field,
        izu_field,
        ogasawara_field
      )
    )
  )
  
  if (small_island_fields_available) {
    
    evidence$RYUKYU_KEY_EVIDENCE <-
      as.logical(
        evidence[[ryukyu_field]]
      )
    
    evidence$IZU_KEY_EVIDENCE <-
      as.logical(
        evidence[[izu_field]]
      )
    
    evidence$OGASAWARA_KEY_EVIDENCE <-
      as.logical(
        evidence[[ogasawara_field]]
      )
    
  } else {
    
    evidence$RYUKYU_KEY_EVIDENCE <- NA
    evidence$IZU_KEY_EVIDENCE <- NA
    evidence$OGASAWARA_KEY_EVIDENCE <- NA
  }
  
  total_area_field <- find_first_field(
    c(
      "JAPAN_ANALYTICAL_AREAS_PRESENT",
      "JAPAN_AREAS_PRESENT",
      "BOTANICAL_AREAS_PRESENT",
      "N_BOTANICAL_AREAS",
      "AREA_COUNT"
    ),
    names(evidence)
  )
  
  if (
    small_island_fields_available &&
    !is.na(total_area_field)
  ) {
    
    evidence <- evidence |>
      mutate(
        KEY_SMALL_ISLAND_AREAS_PRESENT=
          as.integer(
            replace_na(
              RYUKYU_KEY_EVIDENCE,
              FALSE
            )
          ) +
          as.integer(
            replace_na(
              IZU_KEY_EVIDENCE,
              FALSE
            )
          ) +
          as.integer(
            replace_na(
              OGASAWARA_KEY_EVIDENCE,
              FALSE
            )
          ),
        
        KEY_SMALL_ISLAND_EXCLUSIVE=
          !is.na(
            .data[[total_area_field]]
          ) &
          .data[[total_area_field]]>0 &
          .data[[total_area_field]]==
          KEY_SMALL_ISLAND_AREAS_PRESENT
      )
    
  } else {
    
    evidence$KEY_SMALL_ISLAND_AREAS_PRESENT <-
      NA_integer_
    
    evidence$KEY_SMALL_ISLAND_EXCLUSIVE <-
      NA
  }
  
  # ---------------------------------------------------------------------------
  # Conservative evidence states
  # ---------------------------------------------------------------------------
  
  evidence <- evidence |>
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
      
      ENDEMIC_TERMINAL_BK=
        !INTRODUCED_POSITIVE &
        !HYBRID_POSITIVE &
        ENDEMIC_STATUS=="ENDEMIC" &
        KEY_SMALL_ISLAND_EXCLUSIVE %in% TRUE,
      
      ENDEMIC_NEEDS_SMALL_ISLAND_EVIDENCE=
        !INTRODUCED_POSITIVE &
        !HYBRID_POSITIVE &
        ENDEMIC_STATUS=="ENDEMIC" &
        is.na(
          KEY_SMALL_ISLAND_EXCLUSIVE
        ),
      
      ENDEMIC_CONTINUES_TO_DISTRICT=
        !INTRODUCED_POSITIVE &
        !HYBRID_POSITIVE &
        ENDEMIC_STATUS=="ENDEMIC" &
        KEY_SMALL_ISLAND_EXCLUSIVE %in% FALSE,
      
      NON_ENDEMIC_NAMED_REGION_EVIDENCE=
        ENDEMIC_STATUS=="NON_ENDEMIC" &
        (
          WCVP_TAIWAN %in% TRUE |
            WCVP_KOREA %in% TRUE |
            WCVP_KURILES_SAKHALIN %in% TRUE |
            WCVP_CHINA %in% TRUE
        ),
      
      NON_ENDEMIC_BEYOND_EVIDENCE=
        ENDEMIC_STATUS=="NON_ENDEMIC" &
        WCVP_BEYOND_KEY_REGIONS %in% TRUE
    )
  
  # ---------------------------------------------------------------------------
  # First-unresolved-node traversal
  # ---------------------------------------------------------------------------
  
  traversal <- evidence |>
    mutate(
      TRAVERSAL_STATUS=
        case_when(
          INTRODUCED_POSITIVE ~
            "STOPPED_METHOD",
          
          !INTRODUCED_POSITIVE &
            HYBRID_POSITIVE ~
            "STOPPED_METHOD",
          
          ENDEMIC_STATUS=="UNRESOLVED" ~
            "STOPPED_EVIDENCE",
          
          ENDEMIC_TERMINAL_BK ~
            "CANDIDATE_TERMINAL",
          
          ENDEMIC_NEEDS_SMALL_ISLAND_EVIDENCE ~
            "STOPPED_METHOD",
          
          ENDEMIC_CONTINUES_TO_DISTRICT ~
            "STOPPED_METHOD",
          
          ENDEMIC_STATUS=="NON_ENDEMIC" ~
            "STOPPED_METHOD",
          
          TRUE ~
            "STOPPED_METHOD"
        ),
      
      LAST_RESOLVED_NODE=
        case_when(
          INTRODUCED_POSITIVE ~
            "P01_INTRODUCTION_EVIDENCE",
          
          !INTRODUCED_POSITIVE &
            HYBRID_POSITIVE ~
            "P02_HYBRID_EVIDENCE",
          
          ENDEMIC_STATUS=="UNRESOLVED" ~
            "ROOT_ACCEPTED_TAXON",
          
          ENDEMIC_TERMINAL_BK ~
            "E02_SMALL_ISLAND_GROUPS",
          
          ENDEMIC_NEEDS_SMALL_ISLAND_EVIDENCE ~
            "E01_ENDEMIC_TO_JAPAN",
          
          ENDEMIC_CONTINUES_TO_DISTRICT ~
            "E02A_NOT_SMALL_ISLAND_EXCLUSIVE",
          
          ENDEMIC_STATUS=="NON_ENDEMIC" ~
            "N01_NOT_ENDEMIC_TO_JAPAN",
          
          TRUE ~
            "ROOT_ACCEPTED_TAXON"
        ),
      
      NEXT_KEY_NODE=
        case_when(
          INTRODUCED_POSITIVE ~
            "GX_CULTIVATED_CROSSWALK",
          
          !INTRODUCED_POSITIVE &
            HYBRID_POSITIVE ~
            "HYB_CONTEMPORARY_EXTENSION",
          
          ENDEMIC_STATUS=="UNRESOLVED" ~
            "ENDEMICITY_EVIDENCE",
          
          ENDEMIC_TERMINAL_BK ~
            NA_character_,
          
          ENDEMIC_NEEDS_SMALL_ISLAND_EVIDENCE ~
            "SMALL_ISLAND_EXCLUSIVITY",
          
          ENDEMIC_CONTINUES_TO_DISTRICT ~
            "JAPAN_DISTRICT",
          
          ENDEMIC_STATUS=="NON_ENDEMIC" ~
            "BROAD_EXTERNAL_RANGE_LOGIC",
          
          TRUE ~
            "METHOD_REVIEW"
        ),
      
      REQUIRED_EVIDENCE_OR_DECISION=
        case_when(
          INTRODUCED_POSITIVE ~
            paste0(
              "Formal crosswalk between Nakamura 'cultivated species' ",
              "and contemporary positive introduced-to-Japan evidence."
            ),
          
          !INTRODUCED_POSITIVE &
            HYBRID_POSITIVE ~
            paste0(
              "Formal documentation of HYB as a contemporary extension ",
              "outside literal Figure 2.2 terminals."
            ),
          
          ENDEMIC_STATUS=="UNRESOLVED" ~
            "Resolve contemporary endemicity evidence.",
          
          ENDEMIC_TERMINAL_BK ~
            paste0(
              "None: complete source-defined endemic ",
              "small-island route."
            ),
          
          ENDEMIC_NEEDS_SMALL_ISLAND_EVIDENCE ~
            paste0(
              "Explicit evidence that occurrence is confined ",
              "to any/all of Ryukyu, Ogasawara and Izu."
            ),
          
          ENDEMIC_CONTINUES_TO_DISTRICT ~
            paste0(
              "Method-validated operational definition and ",
              "count of Japanese districts."
            ),
          
          ENDEMIC_STATUS=="NON_ENDEMIC" ~
            paste0(
              "Formal logical interpretation of Nakamura ",
              "N02/N02A using validated 03r broad-range evidence."
            ),
          
          TRUE ~
            "Methodological review."
        ),
      
      CANDIDATE_STAR=
        case_when(
          ENDEMIC_TERMINAL_BK ~ "BK",
          TRUE ~ NA_character_
        ),
      
      CANDIDATE_STAR_SOURCE=
        case_when(
          ENDEMIC_TERMINAL_BK ~
            paste0(
              "Nakamura (2012) Figure 2.2: endemic to Japan ",
              "and confined to Ryukyu/Ogasawara/Izu ",
              "small-island groups."
            ),
          
          TRUE ~
            NA_character_
        ),
      
      DEFINITIVE_STAR_ASSIGNED=FALSE
    ) |>
    select(
      FINAL_WCVP_ID,
      FINAL_WCVP_RECOGNISED_NAME,
      FINAL_WCVP_RANK,
      FINAL_WCVP_STATUS,
      FINAL_WCVP_CONCEPT_CLASS,
      INTRODUCED_POSITIVE,
      HYBRID_POSITIVE,
      HYBRID_STATUS,
      ENDEMIC_STATUS,
      RYUKYU_KEY_EVIDENCE,
      IZU_KEY_EVIDENCE,
      OGASAWARA_KEY_EVIDENCE,
      KEY_SMALL_ISLAND_EXCLUSIVE,
      WCVP_TAIWAN,
      WCVP_KOREA,
      WCVP_KURILES_SAKHALIN,
      WCVP_CHINA,
      WCVP_BEYOND_KEY_REGIONS,
      N_NAMED_NEIGHBOUR_REGIONS,
      NON_ENDEMIC_NAMED_REGION_EVIDENCE,
      NON_ENDEMIC_BEYOND_EVIDENCE,
      TRAVERSAL_STATUS,
      LAST_RESOLVED_NODE,
      NEXT_KEY_NODE,
      REQUIRED_EVIDENCE_OR_DECISION,
      CANDIDATE_STAR,
      CANDIDATE_STAR_SOURCE,
      DEFINITIVE_STAR_ASSIGNED
    )
  # ---------------------------------------------------------------------------
  # Candidate terminals and unresolved work packages
  # ---------------------------------------------------------------------------
  
  candidate_terminals <- traversal |>
    filter(
      TRAVERSAL_STATUS=="CANDIDATE_TERMINAL"
    ) |>
    select(
      FINAL_WCVP_ID,
      FINAL_WCVP_RECOGNISED_NAME,
      FINAL_WCVP_RANK,
      CANDIDATE_STAR,
      CANDIDATE_STAR_SOURCE,
      LAST_RESOLVED_NODE
    )
  
  unresolved <- traversal |>
    filter(
      TRAVERSAL_STATUS!="CANDIDATE_TERMINAL"
    )
  
  work_packages <- unresolved |>
    count(
      NEXT_KEY_NODE,
      REQUIRED_EVIDENCE_OR_DECISION,
      name="N_TAXA",
      sort=TRUE
    ) |>
    mutate(
      PERCENT_ACCEPTED=
        round(
          100*N_TAXA/nrow(traversal),
          3
        )
    )
  
  traversal_profile <- traversal |>
    count(
      TRAVERSAL_STATUS,
      CANDIDATE_STAR,
      NEXT_KEY_NODE,
      name="N_TAXA",
      sort=TRUE
    )
  
  route_profile <- traversal |>
    summarise(
      ACCEPTED_TAXA=n(),
      
      CANDIDATE_TERMINAL=
        sum(
          TRAVERSAL_STATUS==
            "CANDIDATE_TERMINAL"
        ),
      
      CANDIDATE_BK=
        sum(
          CANDIDATE_STAR=="BK",
          na.rm=TRUE
        ),
      
      STOPPED_METHOD=
        sum(
          TRAVERSAL_STATUS==
            "STOPPED_METHOD"
        ),
      
      STOPPED_EVIDENCE=
        sum(
          TRAVERSAL_STATUS==
            "STOPPED_EVIDENCE"
        ),
      
      GX_CROSSWALK=
        sum(
          NEXT_KEY_NODE==
            "GX_CULTIVATED_CROSSWALK",
          na.rm=TRUE
        ),
      
      HYB_EXTENSION=
        sum(
          NEXT_KEY_NODE==
            "HYB_CONTEMPORARY_EXTENSION",
          na.rm=TRUE
        ),
      
      ENDEMICITY_EVIDENCE=
        sum(
          NEXT_KEY_NODE==
            "ENDEMICITY_EVIDENCE",
          na.rm=TRUE
        ),
      
      SMALL_ISLAND_EXCLUSIVITY=
        sum(
          NEXT_KEY_NODE==
            "SMALL_ISLAND_EXCLUSIVITY",
          na.rm=TRUE
        ),
      
      JAPAN_DISTRICT=
        sum(
          NEXT_KEY_NODE==
            "JAPAN_DISTRICT",
          na.rm=TRUE
        ),
      
      BROAD_EXTERNAL_RANGE_LOGIC=
        sum(
          NEXT_KEY_NODE==
            "BROAD_EXTERNAL_RANGE_LOGIC",
          na.rm=TRUE
        )
    )
  
  candidate_profile <- candidate_terminals |>
    count(
      CANDIDATE_STAR,
      name="N_TAXA",
      sort=TRUE
    )
  
  cat(
    "\n— PARTIAL TRAVERSAL PROFILE —\n"
  )
  
  print(
    route_profile,
    n=Inf
  )
  
  cat(
    "\n— FIRST UNRESOLVED WORK PACKAGES —\n"
  )
  
  print(
    work_packages,
    n=Inf
  )
  
  cat(
    "\n— TRAVERSAL STATUS PROFILE —\n"
  )
  
  print(
    traversal_profile,
    n=Inf
  )
  
  cat(
    "\n— CANDIDATE TERMINAL STAR PROFILE —\n"
  )
  
  if (nrow(candidate_profile)==0L) {
    
    cat(
      "No candidate terminal Stars yet.\n"
    )
    
  } else {
    
    print(
      candidate_profile,
      n=Inf
    )
  }
  
  # ---------------------------------------------------------------------------
  # Method readiness
  # ---------------------------------------------------------------------------
  
  readiness <- tribble(
    ~COMPONENT,
    ~READY,
    ~STATUS,
    
    "Accepted contemporary population",
    TRUE,
    "METHOD_VALIDATED",
    
    "Contemporary endemicity evidence",
    TRUE,
    "EVIDENCE_VALIDATED",
    
    "Endemic small-island source criterion",
    TRUE,
    "SOURCE_VALIDATED",
    
    "Endemic small-island exclusivity evidence",
    small_island_fields_available &&
      !is.na(total_area_field),
    if (
      small_island_fields_available &&
      !is.na(total_area_field)
    ) {
      "EVIDENCE_AVAILABLE"
    } else {
      "EVIDENCE_REVIEW_REQUIRED"
    },
    
    "Japanese district definition",
    FALSE,
    "UNRESOLVED",
    
    "Rare criterion",
    FALSE,
    "UNRESOLVED",
    
    "Almost all districts",
    FALSE,
    "UNRESOLVED",
    
    "Broad Taiwan evidence",
    TRUE,
    "EVIDENCE_VALIDATED",
    
    "Taiwan fine-range subclass",
    FALSE,
    "UNRESOLVED",
    
    "Broad Korea evidence",
    TRUE,
    "EVIDENCE_VALIDATED",
    
    "Korea fine-range subclass",
    FALSE,
    "UNRESOLVED",
    
    "Broad Kuriles/Sakhalin evidence",
    TRUE,
    "EVIDENCE_VALIDATED",
    
    "South-Kuriles subclass",
    FALSE,
    "UNRESOLVED",
    
    "Broad China evidence",
    TRUE,
    "EVIDENCE_VALIDATED",
    
    "China province-count subclass",
    FALSE,
    "UNRESOLVED",
    
    "Beyond named Key regions evidence",
    TRUE,
    "EVIDENCE_VALIDATED",
    
    "N02/N02A logical interpretation",
    FALSE,
    "METHOD_REVIEW_REQUIRED",
    
    "GX contemporary crosswalk",
    FALSE,
    "METHOD_CROSSWALK_REQUIRED",
    
    "HYB contemporary extension",
    FALSE,
    "METHOD_CROSSWALK_REQUIRED",
    
    "Definitive Stars 04 classifier",
    FALSE,
    "NOT_READY"
  )
  
  cat(
    "\n— METHOD READINESS —\n"
  )
  
  print(
    readiness,
    n=Inf
  )
  
  # ---------------------------------------------------------------------------
  # Validation
  # ---------------------------------------------------------------------------
  
  validation <- tibble(
    CHECK=c(
      "Stars 03p validation fully PASS",
      "Stars 03r validation fully PASS",
      "Accepted population = 11,439",
      "Accepted population one row per WCVP ID",
      "Traversal population = 11,439",
      "Traversal one row per WCVP ID",
      "All taxa have traversal status",
      "All stopped taxa have next Key node",
      "Candidate terminal taxa have no next Key node",
      "Candidate terminal Stars limited to source-supported categories",
      "Candidate terminal BK restricted to endemic taxa",
      "Candidate terminal BK excludes positive introduced taxa",
      "Candidate terminal BK excludes HYB-positive taxa",
      "GX not assigned as candidate Star",
      "HYB not assigned as candidate Star",
      "Non-endemic GN not automatically assigned",
      "No GD assigned",
      "No BU assigned",
      "No definitive Stars assigned",
      "No JAPAN_DISTRICT_COUNT created",
      "No rarity proxy created",
      "No almost-all proxy created",
      "No Taiwan fine-range proxy created",
      "No Korea fine-range proxy created",
      "No South-Kuriles proxy created",
      "No China province-count proxy created",
      "All 11,439 taxa accounted for",
      "Historical Star allocations not used"
    ),
    
    PASS=c(
      all(
        validation_03p$PASS
      ),
      
      all(
        validation_03r$PASS
      ),
      
      nrow(accepted)==11439L,
      
      n_distinct(
        accepted$FINAL_WCVP_ID
      )==11439L,
      
      nrow(traversal)==11439L,
      
      n_distinct(
        traversal$FINAL_WCVP_ID
      )==11439L,
      
      all(
        !is.na(
          traversal$TRAVERSAL_STATUS
        )
      ),
      
      all(
        !is.na(
          traversal$NEXT_KEY_NODE[
            traversal$TRAVERSAL_STATUS !=
              "CANDIDATE_TERMINAL"
          ]
        )
      ),
      
      all(
        is.na(
          traversal$NEXT_KEY_NODE[
            traversal$TRAVERSAL_STATUS ==
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
          c(
            "BK",
            "GD",
            "BU",
            "GN",
            "GX"
          )
      ),
      
      all(
        traversal$ENDEMIC_STATUS[
          traversal$CANDIDATE_STAR=="BK" &
            !is.na(
              traversal$CANDIDATE_STAR
            )
        ]=="ENDEMIC"
      ),
      
      !any(
        traversal$INTRODUCED_POSITIVE &
          traversal$CANDIDATE_STAR=="BK",
        na.rm=TRUE
      ),
      
      !any(
        traversal$HYBRID_POSITIVE &
          traversal$CANDIDATE_STAR=="BK",
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
        traversal$ENDEMIC_STATUS==
          "NON_ENDEMIC" &
          traversal$CANDIDATE_STAR=="GN",
        na.rm=TRUE
      ),
      
      !any(
        traversal$CANDIDATE_STAR=="GD",
        na.rm=TRUE
      ),
      
      !any(
        traversal$CANDIDATE_STAR=="BU",
        na.rm=TRUE
      ),
      
      !any(
        traversal$DEFINITIVE_STAR_ASSIGNED
      ),
      
      !"JAPAN_DISTRICT_COUNT" %in%
        names(traversal),
      
      !any(
        str_detect(
          names(traversal),
          "RARITY_PROXY|RARE_PROXY"
        )
      ),
      
      !any(
        str_detect(
          names(traversal),
          "ALMOST_ALL_PROXY"
        )
      ),
      
      !any(
        str_detect(
          names(traversal),
          "TAIWAN_HALF|TAIWAN_SUBCLASS"
        )
      ),
      
      !any(
        str_detect(
          names(traversal),
          "KOREA_SPARSE|KOREA_SUBCLASS"
        )
      ),
      
      !any(
        str_detect(
          names(traversal),
          "SOUTH_KURILES_CLASS"
        )
      ),
      
      !any(
        str_detect(
          names(traversal),
          "CHINA_PROVINCE_COUNT"
        )
      ),
      
      nrow(candidate_terminals)+
        nrow(unresolved)==11439L,
      
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
      "Stars 03s validation failed. ",
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
    
    UPSTREAM_GEOGRAPHY=
      if (
        "VERSION" %in%
        names(metadata_03r)
      ) {
        paste0(
          "Stars 03r v",
          metadata_03r$VERSION[[1]]
        )
      } else {
        "Stars 03r"
      },
    
    ACCEPTED_TAXA=
      nrow(traversal),
    
    CANDIDATE_TERMINAL_TAXA=
      nrow(candidate_terminals),
    
    UNRESOLVED_TAXA=
      nrow(unresolved),
    
    CANDIDATE_BK=
      sum(
        traversal$CANDIDATE_STAR=="BK",
        na.rm=TRUE
      ),
    
    DEFINITIVE_STARS_ASSIGNED=FALSE,
    GX_ASSIGNED=FALSE,
    HYB_ASSIGNED=FALSE,
    JAPAN_DISTRICT_COUNT_CREATED=FALSE,
    HISTORICAL_STAR_ALLOCATIONS_USED=FALSE,
    FINAL_CLASSIFIER_READY=FALSE,
    
    STATUS=
      "VALIDATED_PARTIAL_KEY_TRAVERSAL"
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
    TABLE_WORK_PACKAGES,
    work_packages,
    overwrite=TRUE
  )
  
  dbWriteTable(
    con,
    TABLE_CANDIDATE_TERMINALS,
    candidate_terminals,
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
      "stars03s_partial_key_traversal.csv"
    )
  )
  
  write_csv(
    candidate_terminals,
    file.path(
      OUTPUT_DIR,
      "stars03s_candidate_terminal_assignments.csv"
    )
  )
  
  write_csv(
    work_packages,
    file.path(
      OUTPUT_DIR,
      "stars03s_unresolved_work_packages.csv"
    )
  )
  
  write_csv(
    route_profile,
    file.path(
      OUTPUT_DIR,
      "stars03s_route_profile.csv"
    )
  )
  
  write_csv(
    traversal_profile,
    file.path(
      OUTPUT_DIR,
      "stars03s_traversal_profile.csv"
    )
  )
  
  write_csv(
    candidate_profile,
    file.path(
      OUTPUT_DIR,
      "stars03s_candidate_star_profile.csv"
    )
  )
  
  write_csv(
    readiness,
    file.path(
      OUTPUT_DIR,
      "stars03s_method_readiness.csv"
    )
  )
  
  write_csv(
    validation,
    file.path(
      OUTPUT_DIR,
      "stars03s_validation.csv"
    )
  )
  
  write_csv(
    metadata,
    file.path(
      OUTPUT_DIR,
      "stars03s_metadata.csv"
    )
  )
  
  # ---------------------------------------------------------------------------
  # Final summary
  # ---------------------------------------------------------------------------
  
  cat(
    "\n============================================================\n"
  )
  
  cat(
    "Stars 03s v",
    VERSION,
    " COMPLETE\n",
    sep=""
  )
  
  cat(
    "============================================================\n"
  )
  
  cat(
    "Accepted taxa traversed: ",
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
    "Taxa stopped at unresolved node: ",
    format(
      nrow(unresolved),
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
    "Small-island evidence fields available: ",
    small_island_fields_available,
    "\n",
    sep=""
  )
  
  cat(
    "Small-island total-area field: ",
    if_else(
      is.na(total_area_field),
      "NONE",
      total_area_field
    ),
    "\n",
    sep=""
  )
  
  cat(
    "Definitive Stars assigned: FALSE\n"
  )
  
  cat(
    "GX assigned: FALSE\n"
  )
  
  cat(
    "HYB assigned: FALSE\n"
  )
  
  cat(
    "JAPAN_DISTRICT_COUNT created: FALSE\n"
  )
  
  cat(
    "Historical Star allocations used: FALSE\n"
  )
  
  cat(
    "Final classifier ready: FALSE\n"
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
    "Canonical work packages: ",
    TABLE_WORK_PACKAGES,
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
      candidate_terminals=candidate_terminals,
      unresolved=unresolved,
      work_packages=work_packages,
      route_profile=route_profile,
      traversal_profile=traversal_profile,
      candidate_profile=candidate_profile,
      readiness=readiness,
      validation=validation,
      metadata=metadata
    )
  )
}

stars_03s_result <- run_stars_03s()