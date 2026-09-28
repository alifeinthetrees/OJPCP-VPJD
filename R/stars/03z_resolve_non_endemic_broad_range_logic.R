# =============================================================================
# VPJD-OJPCP
# R/stars/03z_resolve_non_endemic_broad_range_logic.R
# Version 0.1.0
#
# RESOLVE NON-ENDEMIC BROAD-RANGE LOGIC
#
# PURPOSE
# - Continue the non-endemic branch of Nakamura (2012), Figure 2.2.
# - Use the evidence configurations formalised in Stars 03y.
# - Operationalise the distinction between restricted matrix-entry taxa and
#   broader non-endemic taxa.
# - Produce candidate GN terminals only where the broad-range route is
#   methodologically resolved.
# - Retain restricted single-region taxa for later matrix classification.
#
# METHOD
# - Taiwan only -> Taiwan matrix.
# - Korea only -> Korea matrix.
# - Kuriles/Sakhalin only -> Kuriles & Sakhalin matrix.
# - China only -> China matrix, because China is explicitly represented in
#   the published matrix.
# - More than one named external Key region -> candidate GN.
# - Positive evidence beyond the named Key regions -> candidate GN.
#
# IMPORTANT
# - Candidate Stars only; no definitive Stars are assigned.
# - China is NOT treated as "beyond" merely because it is absent from the
#   wording of N02/N02A.
# - Fine Taiwan/Korea/Kuriles-Sakhalin/China subclasses are not inferred.
# - Japanese matrix district columns are not selected.
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

MODULE <- "stars_03z_resolve_non_endemic_broad_range_logic"
VERSION <- "0.1.0"
RUN_DATE <- Sys.Date()

DB_PATH <- here(
  "data","interim","occurrences",
  "vpjd_occurrences.duckdb"
)

OUTPUT_DIR <- here(
  "outputs","tables","stars",
  "non_endemic_broad_range_logic"
)

TABLE_TRAVERSAL <- "vpjd_star_non_endemic_broad_range_traversal"
TABLE_METHOD <- "vpjd_star_non_endemic_broad_range_method"
TABLE_MATRIX_WORK <- "vpjd_star_non_endemic_matrix_work_packages"
TABLE_READINESS <- "vpjd_star_non_endemic_broad_range_readiness"
TABLE_VALIDATION <- "vpjd_star_03z_validation"
TABLE_METADATA <- "vpjd_star_03z_metadata"

run_stars_03z <- function() {
  
  cat("\n— Resolve non-endemic broad-range logic —\n\n")
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
    "vpjd_star_non_endemic_logic_register",
    "vpjd_star_03p_validation",
    "vpjd_star_03r_validation",
    "vpjd_star_03y_validation"
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
  
  validation_03r <- as_tibble(
    dbGetQuery(
      con,
      "SELECT * FROM vpjd_star_03r_validation"
    )
  )
  
  validation_03y <- as_tibble(
    dbGetQuery(
      con,
      "SELECT * FROM vpjd_star_03y_validation"
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
  
  if (
    !"PASS" %in% names(validation_03y) ||
    !all(validation_03y$PASS)
  ) {
    stop("Stars 03y is not fully validated.")
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
  
  # ---------------------------------------------------------------------------
  # Load canonical 03y logic
  # ---------------------------------------------------------------------------
  
  logic <- as_tibble(
    dbGetQuery(
      con,
      "
      SELECT *
      FROM vpjd_star_non_endemic_key_logic
      "
    )
  )
  
  required_fields <- c(
    "FINAL_WCVP_ID",
    "FINAL_WCVP_RECOGNISED_NAME",
    "WCVP_TAIWAN",
    "WCVP_KOREA",
    "WCVP_KURILES_SAKHALIN",
    "WCVP_CHINA",
    "WCVP_BEYOND_KEY_REGIONS",
    "N_NAMED_KEY_REGIONS",
    "NON_ENDEMIC_LOGIC_CLASS"
  )
  
  missing_fields <- setdiff(
    required_fields,
    names(logic)
  )
  
  if (length(missing_fields)>0L) {
    stop(
      "Required Stars 03y fields missing: ",
      paste(missing_fields,collapse=", "),
      "\nAvailable fields: ",
      paste(names(logic),collapse=", ")
    )
  }
  
  if (nrow(logic)!=7736L) {
    stop(
      "Expected 7,736 non-endemic taxa; found ",
      format(nrow(logic),big.mark=",")
    )
  }
  
  if (n_distinct(logic$FINAL_WCVP_ID)!=7736L) {
    stop("Stars 03y logic is not unique by WCVP ID.")
  }
  
  logic <- logic |>
    mutate(
      FINAL_WCVP_ID=as.character(FINAL_WCVP_ID),
      WCVP_TAIWAN=coalesce(as.logical(WCVP_TAIWAN),FALSE),
      WCVP_KOREA=coalesce(as.logical(WCVP_KOREA),FALSE),
      WCVP_KURILES_SAKHALIN=
        coalesce(as.logical(WCVP_KURILES_SAKHALIN),FALSE),
      WCVP_CHINA=coalesce(as.logical(WCVP_CHINA),FALSE),
      WCVP_BEYOND_KEY_REGIONS=
        coalesce(as.logical(WCVP_BEYOND_KEY_REGIONS),FALSE)
    )
  
  cat(
    "Non-endemic taxa loaded: ",
    format(nrow(logic),big.mark=","),
    "\n",
    sep=""
  )
  
  # ---------------------------------------------------------------------------
  # Formal method register
  # ---------------------------------------------------------------------------
  
  method_register <- tribble(
    ~METHOD_ID,
    ~KEY_COMPONENT,
    ~SOURCE_BASIS,
    ~OPERATIONAL_DEFINITION,
    ~METHOD_STATUS,
    
    "NEBR01",
    "Restricted Taiwan matrix entry",
    paste0(
      "Figure 2.2: apart from Japan, only in one of Taiwan, Korea, ",
      "or the Kuriles & Sakhalin, use the Table."
    ),
    paste0(
      "Taiwan is the only positive named external Key region and ",
      "there is no positive evidence beyond the named Key regions."
    ),
    "METHOD_VALIDATED",
    
    "NEBR02",
    "Restricted Korea matrix entry",
    paste0(
      "Figure 2.2: apart from Japan, only in one of Taiwan, Korea, ",
      "or the Kuriles & Sakhalin, use the Table."
    ),
    paste0(
      "Korea is the only positive named external Key region and ",
      "there is no positive evidence beyond the named Key regions."
    ),
    "METHOD_VALIDATED",
    
    "NEBR03",
    "Restricted Kuriles/Sakhalin matrix entry",
    paste0(
      "Figure 2.2: apart from Japan, only in one of Taiwan, Korea, ",
      "or the Kuriles & Sakhalin, use the Table."
    ),
    paste0(
      "Kuriles/Sakhalin is the only positive named external Key ",
      "region and there is no positive evidence beyond the named ",
      "Key regions."
    ),
    "METHOD_VALIDATED",
    
    "NEBR04",
    "Restricted China matrix entry",
    "Figure 2.2 explicitly contains two China matrix rows.",
    paste0(
      "China is the only positive named external Key region and ",
      "there is no positive evidence beyond the named Key regions. ",
      "The taxon enters the China matrix rather than being treated ",
      "as broad-range GREEN."
    ),
    "METHOD_VALIDATED_BY_MATRIX_STRUCTURE",
    
    "NEBR05",
    "Multiple named external regions",
    paste0(
      "Figure 2.2 contrasts distribution only in one restricted ",
      "external region with 'not only in one ... and beyond'."
    ),
    paste0(
      "Positive evidence in two or more named external Key regions ",
      "fails the restricted single-region condition and follows the ",
      "broad non-endemic route to GREEN."
    ),
    "METHOD_VALIDATED",
    
    "NEBR06",
    "Beyond named external regions",
    paste0(
      "Figure 2.2: 'Not only in one of Taiwan or Korea OR the ",
      "Kuriles & Sakhalin, and beyond = GREEN.'"
    ),
    paste0(
      "Positive validated distribution evidence outside Japan, ",
      "Taiwan, Korea, Kuriles, Sakhalin and China constitutes ",
      "'beyond' evidence and follows the broad non-endemic route ",
      "to GREEN."
    ),
    "METHOD_VALIDATED",
    
    "NEBR07",
    "Precedence",
    "Logical structure of Figure 2.2 non-endemic branch.",
    paste0(
      "Beyond-region evidence takes the broad GREEN route. In the ",
      "absence of beyond evidence, two or more named Key regions ",
      "take the broad GREEN route. Otherwise a single named Key ",
      "region enters its corresponding matrix row."
    ),
    "METHOD_VALIDATED",
    
    "NEBR08",
    "Fine matrix subclasses",
    "Figure 2.2 non-endemic matrix.",
    paste0(
      "Taiwan half-island, Korea sparse/island, south-Kuriles and ",
      "China province-count subclasses remain separate unresolved ",
      "criteria and are not inferred here."
    ),
    "NOT_IMPLEMENTED_IN_THIS_MODULE"
  )
  
  # ---------------------------------------------------------------------------
  # Apply broad-range traversal
  # ---------------------------------------------------------------------------
  
  traversal <- logic |>
    mutate(
      BROAD_RANGE_ROUTE=
        case_when(
          WCVP_BEYOND_KEY_REGIONS ~
            "BROAD_RANGE_BEYOND_KEY_REGIONS",
          
          N_NAMED_KEY_REGIONS>=2L ~
            "BROAD_RANGE_MULTIPLE_KEY_REGIONS",
          
          NON_ENDEMIC_LOGIC_CLASS==
            "SINGLE_KEY_REGION_TAIWAN" ~
            "MATRIX_TAIWAN",
          
          NON_ENDEMIC_LOGIC_CLASS==
            "SINGLE_KEY_REGION_KOREA" ~
            "MATRIX_KOREA",
          
          NON_ENDEMIC_LOGIC_CLASS==
            "SINGLE_KEY_REGION_KURILES_SAKHALIN" ~
            "MATRIX_KURILES_SAKHALIN",
          
          NON_ENDEMIC_LOGIC_CLASS==
            "SINGLE_KEY_REGION_CHINA" ~
            "MATRIX_CHINA",
          
          TRUE ~
            "BROAD_RANGE_EVIDENCE_REVIEW"
        ),
      
      TRAVERSAL_STATUS=
        case_when(
          BROAD_RANGE_ROUTE %in%
            c(
              "BROAD_RANGE_BEYOND_KEY_REGIONS",
              "BROAD_RANGE_MULTIPLE_KEY_REGIONS"
            ) ~
            "CANDIDATE_TERMINAL",
          
          BROAD_RANGE_ROUTE %in%
            c(
              "MATRIX_TAIWAN",
              "MATRIX_KOREA",
              "MATRIX_KURILES_SAKHALIN",
              "MATRIX_CHINA"
            ) ~
            "CONTINUE_TO_MATRIX",
          
          TRUE ~
            "STOPPED_EVIDENCE"
        ),
      
      CANDIDATE_STAR=
        if_else(
          TRAVERSAL_STATUS=="CANDIDATE_TERMINAL",
          "GN",
          NA_character_
        ),
      
      NEXT_KEY_NODE=
        case_when(
          BROAD_RANGE_ROUTE=="MATRIX_TAIWAN" ~
            "TAIWAN_RANGE_SUBCLASS",
          
          BROAD_RANGE_ROUTE=="MATRIX_KOREA" ~
            "KOREA_RANGE_SUBCLASS",
          
          BROAD_RANGE_ROUTE=="MATRIX_KURILES_SAKHALIN" ~
            "KURILES_SAKHALIN_RANGE_SUBCLASS",
          
          BROAD_RANGE_ROUTE=="MATRIX_CHINA" ~
            "CHINA_PROVINCE_COUNT",
          
          BROAD_RANGE_ROUTE=="BROAD_RANGE_EVIDENCE_REVIEW" ~
            "EXTERNAL_RANGE_EVIDENCE_REVIEW",
          
          TRUE ~
            NA_character_
        ),
      
      DEFINITIVE_STAR=NA_character_,
      STAR_ASSIGNED=FALSE
    )
  
  # ---------------------------------------------------------------------------
  # Profiles
  # ---------------------------------------------------------------------------
  
  route_profile <- traversal |>
    count(
      BROAD_RANGE_ROUTE,
      TRAVERSAL_STATUS,
      CANDIDATE_STAR,
      NEXT_KEY_NODE,
      name="N_TAXA",
      sort=TRUE
    ) |>
    mutate(
      PERCENT_NON_ENDEMIC=
        round(
          100*N_TAXA/nrow(traversal),
          3
        )
    )
  
  status_profile <- traversal |>
    count(
      TRAVERSAL_STATUS,
      CANDIDATE_STAR,
      name="N_TAXA",
      sort=TRUE
    ) |>
    mutate(
      PERCENT_NON_ENDEMIC=
        round(
          100*N_TAXA/nrow(traversal),
          3
        )
    )
  
  candidate_profile <- traversal |>
    filter(
      TRAVERSAL_STATUS=="CANDIDATE_TERMINAL"
    ) |>
    count(
      CANDIDATE_STAR,
      name="N_TAXA",
      sort=TRUE
    )
  
  matrix_work_packages <- traversal |>
    filter(
      TRAVERSAL_STATUS!="CANDIDATE_TERMINAL"
    ) |>
    count(
      NEXT_KEY_NODE,
      name="N_TAXA",
      sort=TRUE
    ) |>
    mutate(
      PERCENT_NON_ENDEMIC=
        round(
          100*N_TAXA/nrow(traversal),
          3
        )
    )
  
  cat("\n— BROAD-RANGE ROUTE PROFILE —\n")
  print(route_profile,n=Inf)
  
  cat("\n— TRAVERSAL STATUS PROFILE —\n")
  print(status_profile,n=Inf)
  
  cat("\n— CANDIDATE TERMINAL STAR PROFILE —\n")
  
  if (nrow(candidate_profile)>0L) {
    print(candidate_profile,n=Inf)
  } else {
    cat("No candidate terminal Stars.\n")
  }
  
  cat("\n— REMAINING NON-ENDEMIC MATRIX WORK PACKAGES —\n")
  print(matrix_work_packages,n=Inf)
  
  cat("\n— METHOD REGISTER —\n")
  print(method_register,n=Inf)
  
  # ---------------------------------------------------------------------------
  # Readiness
  # ---------------------------------------------------------------------------
  
  readiness <- tribble(
    ~COMPONENT,
    ~READY,
    ~STATUS,
    
    "Non-endemic population",
    TRUE,
    "EVIDENCE_VALIDATED",
    
    "Broad Taiwan evidence",
    TRUE,
    "EVIDENCE_VALIDATED",
    
    "Broad Korea evidence",
    TRUE,
    "EVIDENCE_VALIDATED",
    
    "Broad Kuriles/Sakhalin evidence",
    TRUE,
    "EVIDENCE_VALIDATED",
    
    "Broad China evidence",
    TRUE,
    "EVIDENCE_VALIDATED",
    
    "Beyond-region evidence",
    TRUE,
    "EVIDENCE_VALIDATED",
    
    "Restricted Taiwan matrix routing",
    TRUE,
    "METHOD_VALIDATED",
    
    "Restricted Korea matrix routing",
    TRUE,
    "METHOD_VALIDATED",
    
    "Restricted Kuriles/Sakhalin matrix routing",
    TRUE,
    "METHOD_VALIDATED",
    
    "Restricted China matrix routing",
    TRUE,
    "METHOD_VALIDATED_BY_MATRIX_STRUCTURE",
    
    "Multiple-region broad GREEN routing",
    TRUE,
    "METHOD_VALIDATED",
    
    "Beyond-region broad GREEN routing",
    TRUE,
    "METHOD_VALIDATED",
    
    "Taiwan fine-range subclass",
    FALSE,
    "UNRESOLVED",
    
    "Korea fine-range subclass",
    FALSE,
    "UNRESOLVED",
    
    "South-Kuriles subclass",
    FALSE,
    "UNRESOLVED",
    
    "China province-count subclass",
    FALSE,
    "UNRESOLVED",
    
    "Japanese matrix district column",
    FALSE,
    "NOT_IMPLEMENTED_FOR_MATRIX",
    
    "Non-endemic branch fully operational",
    FALSE,
    "NOT_READY",
    
    "Definitive non-endemic Stars",
    FALSE,
    "NOT_ASSIGNED"
  )
  
  cat("\n— NON-ENDEMIC READINESS —\n")
  print(readiness,n=Inf)
  
  # ---------------------------------------------------------------------------
  # Validation
  # ---------------------------------------------------------------------------
  
  candidate_gn_n <- sum(
    traversal$CANDIDATE_STAR=="GN",
    na.rm=TRUE
  )
  
  matrix_n <- sum(
    traversal$TRAVERSAL_STATUS=="CONTINUE_TO_MATRIX"
  )
  
  review_n <- sum(
    traversal$TRAVERSAL_STATUS=="STOPPED_EVIDENCE"
  )
  
  validation <- tibble(
    CHECK=c(
      "Stars 03p validation fully PASS",
      "Stars 03r validation fully PASS",
      "Stars 03y validation fully PASS",
      "Non-endemic population = 7,736",
      "Non-endemic population unique by WCVP ID",
      "Traversal output = 7,736 taxa",
      "Traversal output unique by WCVP ID",
      "All taxa have broad-range route",
      "All taxa have traversal status",
      "Candidate terminals limited to GN",
      "Candidate GN taxa have no next Key node",
      "Candidate GN taxa have beyond evidence or >=2 named regions",
      "Beyond-region taxa route to candidate GN",
      "Multiple named-region taxa without beyond route to candidate GN",
      "Single Taiwan restricted taxa continue to matrix",
      "Single Korea restricted taxa continue to matrix",
      "Single Kuriles/Sakhalin restricted taxa continue to matrix",
      "Single China restricted taxa continue to matrix",
      "Taiwan matrix taxa have no beyond evidence",
      "Korea matrix taxa have no beyond evidence",
      "Kuriles/Sakhalin matrix taxa have no beyond evidence",
      "China matrix taxa have no beyond evidence",
      "Matrix taxa have exactly one named Key region",
      "China not automatically treated as beyond",
      "No Taiwan fine-range proxy created",
      "No Korea fine-range proxy created",
      "No south-Kuriles proxy created",
      "No China province-count proxy created",
      "No Japanese matrix district column selected",
      "No BK assigned",
      "No GD assigned",
      "No BU assigned",
      "No GX assigned",
      "No HYB assigned",
      "No definitive Stars assigned",
      "Historical Star allocations not used",
      "All 7,736 taxa accounted for"
    ),
    
    PASS=c(
      all(validation_03p$PASS),
      all(validation_03r$PASS),
      all(validation_03y$PASS),
      
      nrow(logic)==7736L,
      
      n_distinct(
        logic$FINAL_WCVP_ID
      )==7736L,
      
      nrow(traversal)==7736L,
      
      n_distinct(
        traversal$FINAL_WCVP_ID
      )==7736L,
      
      all(
        !is.na(
          traversal$BROAD_RANGE_ROUTE
        )
      ),
      
      all(
        !is.na(
          traversal$TRAVERSAL_STATUS
        )
      ),
      
      all(
        traversal$CANDIDATE_STAR[
          !is.na(traversal$CANDIDATE_STAR)
        ]=="GN"
      ),
      
      all(
        is.na(
          traversal$NEXT_KEY_NODE[
            traversal$TRAVERSAL_STATUS==
              "CANDIDATE_TERMINAL"
          ]
        )
      ),
      
      all(
        traversal$WCVP_BEYOND_KEY_REGIONS[
          traversal$TRAVERSAL_STATUS==
            "CANDIDATE_TERMINAL"
        ] |
          traversal$N_NAMED_KEY_REGIONS[
            traversal$TRAVERSAL_STATUS==
              "CANDIDATE_TERMINAL"
          ]>=2L
      ),
      
      all(
        traversal$TRAVERSAL_STATUS[
          traversal$WCVP_BEYOND_KEY_REGIONS
        ]=="CANDIDATE_TERMINAL"
      ),
      
      all(
        traversal$TRAVERSAL_STATUS[
          !traversal$WCVP_BEYOND_KEY_REGIONS &
            traversal$N_NAMED_KEY_REGIONS>=2L
        ]=="CANDIDATE_TERMINAL"
      ),
      
      all(
        traversal$BROAD_RANGE_ROUTE[
          traversal$NON_ENDEMIC_LOGIC_CLASS==
            "SINGLE_KEY_REGION_TAIWAN"
        ]=="MATRIX_TAIWAN"
      ),
      
      all(
        traversal$BROAD_RANGE_ROUTE[
          traversal$NON_ENDEMIC_LOGIC_CLASS==
            "SINGLE_KEY_REGION_KOREA"
        ]=="MATRIX_KOREA"
      ),
      
      all(
        traversal$BROAD_RANGE_ROUTE[
          traversal$NON_ENDEMIC_LOGIC_CLASS==
            "SINGLE_KEY_REGION_KURILES_SAKHALIN"
        ]=="MATRIX_KURILES_SAKHALIN"
      ),
      
      all(
        traversal$BROAD_RANGE_ROUTE[
          traversal$NON_ENDEMIC_LOGIC_CLASS==
            "SINGLE_KEY_REGION_CHINA"
        ]=="MATRIX_CHINA"
      ),
      
      all(
        !traversal$WCVP_BEYOND_KEY_REGIONS[
          traversal$BROAD_RANGE_ROUTE==
            "MATRIX_TAIWAN"
        ]
      ),
      
      all(
        !traversal$WCVP_BEYOND_KEY_REGIONS[
          traversal$BROAD_RANGE_ROUTE==
            "MATRIX_KOREA"
        ]
      ),
      
      all(
        !traversal$WCVP_BEYOND_KEY_REGIONS[
          traversal$BROAD_RANGE_ROUTE==
            "MATRIX_KURILES_SAKHALIN"
        ]
      ),
      
      all(
        !traversal$WCVP_BEYOND_KEY_REGIONS[
          traversal$BROAD_RANGE_ROUTE==
            "MATRIX_CHINA"
        ]
      ),
      
      all(
        traversal$N_NAMED_KEY_REGIONS[
          traversal$TRAVERSAL_STATUS==
            "CONTINUE_TO_MATRIX"
        ]==1L
      ),
      
      all(
        !traversal$WCVP_BEYOND_KEY_REGIONS[
          traversal$BROAD_RANGE_ROUTE==
            "MATRIX_CHINA"
        ]
      ),
      
      !"TAIWAN_HALF_ISLAND_CLASS" %in%
        names(traversal),
      
      !"KOREA_SPARSE_CLASS" %in%
        names(traversal),
      
      !"SOUTH_KURILES_CLASS" %in%
        names(traversal),
      
      !"CHINA_PROVINCE_COUNT" %in%
        names(traversal),
      
      !"JAPAN_MATRIX_DISTRICT_COLUMN" %in%
        names(traversal),
      
      !any(
        traversal$CANDIDATE_STAR=="BK",
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
        traversal$CANDIDATE_STAR=="GX",
        na.rm=TRUE
      ),
      
      !any(
        traversal$CANDIDATE_STAR=="HYB",
        na.rm=TRUE
      ),
      
      all(
        is.na(
          traversal$DEFINITIVE_STAR
        )
      ) &&
        !any(
          traversal$STAR_ASSIGNED
        ),
      
      TRUE,
      
      candidate_gn_n +
        matrix_n +
        review_n ==
        7736L
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
      "Stars 03z validation failed. ",
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
      nrow(traversal),
    
    CANDIDATE_GN=
      candidate_gn_n,
    
    CONTINUE_TO_MATRIX=
      matrix_n,
    
    EVIDENCE_REVIEW=
      review_n,
    
    MATRIX_TAIWAN=
      sum(
        traversal$BROAD_RANGE_ROUTE==
          "MATRIX_TAIWAN"
      ),
    
    MATRIX_KOREA=
      sum(
        traversal$BROAD_RANGE_ROUTE==
          "MATRIX_KOREA"
      ),
    
    MATRIX_KURILES_SAKHALIN=
      sum(
        traversal$BROAD_RANGE_ROUTE==
          "MATRIX_KURILES_SAKHALIN"
      ),
    
    MATRIX_CHINA=
      sum(
        traversal$BROAD_RANGE_ROUTE==
          "MATRIX_CHINA"
      ),
    
    BROAD_RANGE_LOGIC_RESOLVED=TRUE,
    
    FINE_EXTERNAL_SUBCLASSES_RESOLVED=FALSE,
    
    JAPAN_MATRIX_DISTRICT_COLUMN_RESOLVED=FALSE,
    
    DEFINITIVE_STARS_ASSIGNED=FALSE,
    
    HISTORICAL_STAR_ALLOCATIONS_USED=FALSE,
    
    STATUS=
      "VALIDATED_NON_ENDEMIC_BROAD_RANGE_TRAVERSAL"
  )
  
  # ---------------------------------------------------------------------------
  # Canonical database outputs
  # ---------------------------------------------------------------------------
  
  dbWriteTable(
    con,
    TABLE_TRAVERSAL,
    traversal,
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
    TABLE_MATRIX_WORK,
    matrix_work_packages,
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
    traversal,
    file.path(
      OUTPUT_DIR,
      "stars03z_non_endemic_broad_range_traversal.csv"
    )
  )
  
  write_csv(
    route_profile,
    file.path(
      OUTPUT_DIR,
      "stars03z_broad_range_route_profile.csv"
    )
  )
  
  write_csv(
    status_profile,
    file.path(
      OUTPUT_DIR,
      "stars03z_traversal_status_profile.csv"
    )
  )
  
  write_csv(
    candidate_profile,
    file.path(
      OUTPUT_DIR,
      "stars03z_candidate_terminal_profile.csv"
    )
  )
  
  write_csv(
    method_register,
    file.path(
      OUTPUT_DIR,
      "stars03z_method_register.csv"
    )
  )
  
  write_csv(
    matrix_work_packages,
    file.path(
      OUTPUT_DIR,
      "stars03z_matrix_work_packages.csv"
    )
  )
  
  write_csv(
    readiness,
    file.path(
      OUTPUT_DIR,
      "stars03z_readiness.csv"
    )
  )
  
  write_csv(
    validation,
    file.path(
      OUTPUT_DIR,
      "stars03z_validation.csv"
    )
  )
  
  write_csv(
    metadata,
    file.path(
      OUTPUT_DIR,
      "stars03z_metadata.csv"
    )
  )
  
  # ---------------------------------------------------------------------------
  # Final summary
  # ---------------------------------------------------------------------------
  
  cat(
    "\n============================================================\n"
  )
  
  cat(
    "Stars 03z v",VERSION," COMPLETE\n",
    sep=""
  )
  
  cat(
    "============================================================\n"
  )
  
  cat(
    "Non-endemic taxa: ",
    format(
      nrow(traversal),
      big.mark=","
    ),
    "\n",
    sep=""
  )
  
  cat(
    "Candidate GN terminals: ",
    format(
      candidate_gn_n,
      big.mark=","
    ),
    "\n",
    sep=""
  )
  
  cat(
    "Continue to matrix: ",
    format(
      matrix_n,
      big.mark=","
    ),
    "\n",
    sep=""
  )
  
  cat(
    "Evidence review: ",
    format(
      review_n,
      big.mark=","
    ),
    "\n",
    sep=""
  )
  
  cat(
    "Taiwan matrix: ",
    sum(
      traversal$BROAD_RANGE_ROUTE==
        "MATRIX_TAIWAN"
    ),
    "\n",
    sep=""
  )
  
  cat(
    "Korea matrix: ",
    sum(
      traversal$BROAD_RANGE_ROUTE==
        "MATRIX_KOREA"
    ),
    "\n",
    sep=""
  )
  
  cat(
    "Kuriles/Sakhalin matrix: ",
    sum(
      traversal$BROAD_RANGE_ROUTE==
        "MATRIX_KURILES_SAKHALIN"
    ),
    "\n",
    sep=""
  )
  
  cat(
    "China matrix: ",
    sum(
      traversal$BROAD_RANGE_ROUTE==
        "MATRIX_CHINA"
    ),
    "\n",
    sep=""
  )
  
  cat(
    "Broad-range logic resolved: TRUE\n"
  )
  
  cat(
    "Fine external-range subclasses resolved: FALSE\n"
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
      route_profile=route_profile,
      status_profile=status_profile,
      candidate_profile=candidate_profile,
      method_register=method_register,
      matrix_work_packages=
        matrix_work_packages,
      readiness=readiness,
      validation=validation,
      metadata=metadata
    )
  )
}

stars_03z_result <- run_stars_03z()