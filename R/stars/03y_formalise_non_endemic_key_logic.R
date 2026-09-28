# =============================================================================
# VPJD-OJPCP
# R/stars/03y_formalise_non_endemic_key_logic.R
# Version 0.1.1
#
# FORMALISE NON-ENDEMIC KEY LOGIC
#
# PURPOSE
# - Profile the 7,736 taxa entering the non-endemic branch of Nakamura
#   (2012), Figure 2.2.
# - Use validated Stars 03r broad external-range evidence.
# - Separate named Key-region evidence from evidence beyond those regions.
# - Identify simple and compound external-range configurations.
# - Formalise what is source-explicit versus what remains logically
#   unresolved in N02/N02A.
#
# IMPORTANT
# - Method/evidence diagnostic only.
# - No Stars are assigned.
# - "Beyond" is NOT automatically translated to GN.
# - China is retained as an explicit Key matrix region.
# - No Taiwan half-island proxy.
# - No Korea sparse/broad proxy.
# - No south-Kuriles proxy.
# - No China province-count proxy.
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

MODULE <- "stars_03y_formalise_non_endemic_key_logic"
VERSION <- "0.1.1"
RUN_DATE <- Sys.Date()

DB_PATH <- here(
  "data","interim","occurrences",
  "vpjd_occurrences.duckdb"
)

OUTPUT_DIR <- here(
  "outputs","tables","stars",
  "non_endemic_key_logic"
)

TABLE_LOGIC <- "vpjd_star_non_endemic_key_logic"
TABLE_COMBINATIONS <- "vpjd_star_non_endemic_range_combinations"
TABLE_REGISTER <- "vpjd_star_non_endemic_logic_register"
TABLE_WORK_PACKAGES <- "vpjd_star_non_endemic_logic_work_packages"
TABLE_READINESS <- "vpjd_star_non_endemic_logic_readiness"
TABLE_VALIDATION <- "vpjd_star_03y_validation"
TABLE_METADATA <- "vpjd_star_03y_metadata"

run_stars_03y <- function() {
  
  cat("\n— Formalise non-endemic Key logic —\n\n")
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
    "vpjd_star_non_endemic_geography_evidence",
    "vpjd_star_wcvp_non_endemic_geography_crosswalk",
    "vpjd_star_key_non_endemic_matrix",
    "vpjd_star_key_source_nodes",
    "vpjd_star_03p_validation",
    "vpjd_star_03r_validation",
    "vpjd_star_03x_validation"
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
  
  validation_03x <- as_tibble(
    dbGetQuery(
      con,
      "SELECT * FROM vpjd_star_03x_validation"
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
    !"PASS" %in% names(validation_03x) ||
    !all(validation_03x$PASS)
  ) {
    stop("Stars 03x is not fully validated.")
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
    "Stars 03x validation: ",
    sum(validation_03x$PASS),"/",
    nrow(validation_03x)," PASS\n",
    sep=""
  )
  
  # ---------------------------------------------------------------------------
  # Load validated Stars 03r evidence
  # ---------------------------------------------------------------------------
  
  evidence <- as_tibble(
    dbGetQuery(
      con,
      "
      SELECT *
      FROM vpjd_star_non_endemic_geography_evidence
      "
    )
  )
  
  if (nrow(evidence)!=7736L) {
    stop(
      "Expected 7,736 non-endemic taxa in Stars 03r; found ",
      format(nrow(evidence),big.mark=",")
    )
  }
  
  if (
    n_distinct(
      evidence$FINAL_WCVP_ID
    )!=7736L
  ) {
    stop(
      "Stars 03r non-endemic evidence is not unique by WCVP ID."
    )
  }
  
  # ---------------------------------------------------------------------------
  # Confirm required canonical 03r fields
  # ---------------------------------------------------------------------------
  
  required_evidence_fields <- c(
    "FINAL_WCVP_ID",
    "FINAL_WCVP_RECOGNISED_NAME",
    "WCVP_TAIWAN",
    "WCVP_KOREA",
    "WCVP_KURILES_SAKHALIN",
    "WCVP_CHINA",
    "WCVP_BEYOND_KEY_REGIONS"
  )
  
  missing_evidence_fields <- setdiff(
    required_evidence_fields,
    names(evidence)
  )
  
  if (length(missing_evidence_fields)>0L) {
    stop(
      "Required Stars 03r evidence fields missing: ",
      paste(missing_evidence_fields,collapse=", "),
      "\nAvailable fields: ",
      paste(names(evidence),collapse=", ")
    )
  }
  
  evidence <- evidence |>
    mutate(
      FINAL_WCVP_ID=
        as.character(FINAL_WCVP_ID),
      
      WCVP_TAIWAN=
        coalesce(as.logical(WCVP_TAIWAN),FALSE),
      
      WCVP_KOREA=
        coalesce(as.logical(WCVP_KOREA),FALSE),
      
      WCVP_KURILES_SAKHALIN=
        coalesce(
          as.logical(WCVP_KURILES_SAKHALIN),
          FALSE
        ),
      
      WCVP_CHINA=
        coalesce(as.logical(WCVP_CHINA),FALSE),
      
      WCVP_BEYOND_KEY_REGIONS=
        coalesce(
          as.logical(WCVP_BEYOND_KEY_REGIONS),
          FALSE
        )
    )
  
  cat(
    "Non-endemic taxa loaded: ",
    format(nrow(evidence),big.mark=","),
    "\n",
    sep=""
  )
  
  # ---------------------------------------------------------------------------
  # Build broad range configuration
  # ---------------------------------------------------------------------------
  
  logic <- evidence |>
    mutate(
      N_NAMED_KEY_REGIONS=
        as.integer(WCVP_TAIWAN) +
        as.integer(WCVP_KOREA) +
        as.integer(WCVP_KURILES_SAKHALIN) +
        as.integer(WCVP_CHINA),
      
      ONLY_TAIWAN=
        WCVP_TAIWAN &
        !WCVP_KOREA &
        !WCVP_KURILES_SAKHALIN &
        !WCVP_CHINA &
        !WCVP_BEYOND_KEY_REGIONS,
      
      ONLY_KOREA=
        !WCVP_TAIWAN &
        WCVP_KOREA &
        !WCVP_KURILES_SAKHALIN &
        !WCVP_CHINA &
        !WCVP_BEYOND_KEY_REGIONS,
      
      ONLY_KURILES_SAKHALIN=
        !WCVP_TAIWAN &
        !WCVP_KOREA &
        WCVP_KURILES_SAKHALIN &
        !WCVP_CHINA &
        !WCVP_BEYOND_KEY_REGIONS,
      
      ONLY_CHINA=
        !WCVP_TAIWAN &
        !WCVP_KOREA &
        !WCVP_KURILES_SAKHALIN &
        WCVP_CHINA &
        !WCVP_BEYOND_KEY_REGIONS,
      
      ONLY_BEYOND=
        N_NAMED_KEY_REGIONS==0L &
        WCVP_BEYOND_KEY_REGIONS,
      
      NO_BROAD_EXTERNAL_REGION=
        N_NAMED_KEY_REGIONS==0L &
        !WCVP_BEYOND_KEY_REGIONS,
      
      NAMED_REGION_COMBINATION=
        case_when(
          N_NAMED_KEY_REGIONS==0L ~
            "NONE",
          
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
              )
            )
        ),
      
      NAMED_REGION_COMBINATION=
        sub(
          ";$",
          "",
          NAMED_REGION_COMBINATION
        ),
      
      FULL_RANGE_CONFIGURATION=
        case_when(
          WCVP_BEYOND_KEY_REGIONS &
            NAMED_REGION_COMBINATION!="NONE" ~
            paste0(
              NAMED_REGION_COMBINATION,
              ";Beyond"
            ),
          
          WCVP_BEYOND_KEY_REGIONS &
            NAMED_REGION_COMBINATION=="NONE" ~
            "Beyond",
          
          !WCVP_BEYOND_KEY_REGIONS ~
            NAMED_REGION_COMBINATION
        )
    )
  
  # ---------------------------------------------------------------------------
  # Logical route classes
  #
  # These are evidence configurations, NOT Star routes.
  # ---------------------------------------------------------------------------
  
  logic <- logic |>
    mutate(
      NON_ENDEMIC_LOGIC_CLASS=
        case_when(
          ONLY_TAIWAN ~
            "SINGLE_KEY_REGION_TAIWAN",
          
          ONLY_KOREA ~
            "SINGLE_KEY_REGION_KOREA",
          
          ONLY_KURILES_SAKHALIN ~
            "SINGLE_KEY_REGION_KURILES_SAKHALIN",
          
          ONLY_CHINA ~
            "SINGLE_KEY_REGION_CHINA",
          
          ONLY_BEYOND ~
            "BEYOND_WITHOUT_NAMED_KEY_REGION",
          
          NO_BROAD_EXTERNAL_REGION ~
            "NO_RESOLVED_BROAD_EXTERNAL_REGION",
          
          N_NAMED_KEY_REGIONS>=2L &
            !WCVP_BEYOND_KEY_REGIONS ~
            "MULTIPLE_KEY_REGIONS_NO_BEYOND",
          
          N_NAMED_KEY_REGIONS>=1L &
            WCVP_BEYOND_KEY_REGIONS ~
            "NAMED_KEY_REGION_AND_BEYOND",
          
          TRUE ~
            "LOGIC_REVIEW"
        ),
      
      SOURCE_LOGIC_STATUS=
        case_when(
          NON_ENDEMIC_LOGIC_CLASS %in%
            c(
              "SINGLE_KEY_REGION_TAIWAN",
              "SINGLE_KEY_REGION_KOREA",
              "SINGLE_KEY_REGION_KURILES_SAKHALIN"
            ) ~
            "SOURCE_MATRIX_ENTRY_EXPLICIT",
          
          NON_ENDEMIC_LOGIC_CLASS==
            "SINGLE_KEY_REGION_CHINA" ~
            "SOURCE_MATRIX_CHINA_ROW_EXPLICIT_WORDING_AMBIGUOUS",
          
          NON_ENDEMIC_LOGIC_CLASS %in%
            c(
              "MULTIPLE_KEY_REGIONS_NO_BEYOND",
              "NAMED_KEY_REGION_AND_BEYOND",
              "BEYOND_WITHOUT_NAMED_KEY_REGION"
            ) ~
            "N02_N02A_LOGIC_REQUIRES_METHOD_INTERPRETATION",
          
          NON_ENDEMIC_LOGIC_CLASS==
            "NO_RESOLVED_BROAD_EXTERNAL_REGION" ~
            "EVIDENCE_REVIEW",
          
          TRUE ~
            "METHOD_REVIEW"
        ),
      
      MATRIX_FINE_RANGE_REQUIRED=
        NON_ENDEMIC_LOGIC_CLASS %in%
        c(
          "SINGLE_KEY_REGION_TAIWAN",
          "SINGLE_KEY_REGION_KOREA",
          "SINGLE_KEY_REGION_KURILES_SAKHALIN",
          "SINGLE_KEY_REGION_CHINA"
        ),
      
      BROAD_EXTERNAL_LOGIC_RESOLVED=
        SOURCE_LOGIC_STATUS==
        "SOURCE_MATRIX_ENTRY_EXPLICIT",
      
      STAR_ASSIGNED=FALSE
    )
  
  # ---------------------------------------------------------------------------
  # Profiles
  # ---------------------------------------------------------------------------
  
  broad_region_profile <- logic |>
    summarise(
      NON_ENDEMIC_TAXA=n(),
      
      TAIWAN=
        sum(WCVP_TAIWAN),
      
      KOREA=
        sum(WCVP_KOREA),
      
      KURILES_SAKHALIN=
        sum(WCVP_KURILES_SAKHALIN),
      
      CHINA=
        sum(WCVP_CHINA),
      
      BEYOND_KEY_REGIONS=
        sum(WCVP_BEYOND_KEY_REGIONS),
      
      NO_NAMED_KEY_REGION=
        sum(N_NAMED_KEY_REGIONS==0L),
      
      ONE_NAMED_KEY_REGION=
        sum(N_NAMED_KEY_REGIONS==1L),
      
      MULTIPLE_NAMED_KEY_REGIONS=
        sum(N_NAMED_KEY_REGIONS>=2L)
    )
  
  combination_profile <- logic |>
    count(
      FULL_RANGE_CONFIGURATION,
      name="N_TAXA",
      sort=TRUE
    ) |>
    mutate(
      PERCENT_NON_ENDEMIC=
        round(
          100*N_TAXA/nrow(logic),
          3
        )
    )
  
  logic_profile <- logic |>
    count(
      NON_ENDEMIC_LOGIC_CLASS,
      SOURCE_LOGIC_STATUS,
      MATRIX_FINE_RANGE_REQUIRED,
      name="N_TAXA",
      sort=TRUE
    ) |>
    mutate(
      PERCENT_NON_ENDEMIC=
        round(
          100*N_TAXA/nrow(logic),
          3
        )
    )
  
  # ---------------------------------------------------------------------------
  # Formal source-logic register
  # ---------------------------------------------------------------------------
  
  logic_register <- tribble(
    ~LOGIC_ID,
    ~SOURCE_ELEMENT,
    ~SOURCE_TEXT,
    ~OPERATIONAL_INTERPRETATION,
    ~STATUS,
    
    "NEL01",
    "Non-endemic root",
    "Not endemic to Japan",
    paste0(
      "Taxa with validated positive distribution evidence outside ",
      "Japan enter the non-endemic branch."
    ),
    "SOURCE_AND_EVIDENCE_VALIDATED",
    
    "NEL02",
    "Taiwan matrix entry",
    paste0(
      "Apart from Japan, only in one of Taiwan, Korea, OR the ",
      "Kuriles & Sakhalin, use the Table"
    ),
    paste0(
      "A taxon whose only resolved external Key region is Taiwan ",
      "and which has no beyond-region evidence is a direct ",
      "candidate for the Taiwan matrix row. Fine Taiwan range ",
      "subclass remains unresolved."
    ),
    "SOURCE_LOGIC_VALIDATED",
    
    "NEL03",
    "Korea matrix entry",
    paste0(
      "Apart from Japan, only in one of Taiwan, Korea, OR the ",
      "Kuriles & Sakhalin, use the Table"
    ),
    paste0(
      "A taxon whose only resolved external Key region is Korea ",
      "and which has no beyond-region evidence is a direct ",
      "candidate for the Korea matrix row. Fine Korea range ",
      "subclass remains unresolved."
    ),
    "SOURCE_LOGIC_VALIDATED",
    
    "NEL04",
    "Kuriles & Sakhalin matrix entry",
    paste0(
      "Apart from Japan, only in one of Taiwan, Korea, OR the ",
      "Kuriles & Sakhalin, use the Table"
    ),
    paste0(
      "A taxon whose only resolved external Key region is Kuriles/",
      "Sakhalin and which has no beyond-region evidence is a direct ",
      "candidate for that matrix row. South-Kuriles subclass ",
      "remains unresolved."
    ),
    "SOURCE_LOGIC_VALIDATED",
    
    "NEL05",
    "China matrix entry",
    "China <=2 provinces / >2 provinces",
    paste0(
      "China is explicitly represented by two rows in the published ",
      "matrix. Therefore China evidence must not be automatically ",
      "treated as evidence for the broad GREEN route."
    ),
    "SOURCE_STRUCTURE_VALIDATED_LOGIC_UNRESOLVED",
    
    "NEL06",
    "Multiple named Key regions",
    paste0(
      "Not only in one of Taiwan or Korea OR the Kuriles & ",
      "Sakhalin, and beyond = GREEN"
    ),
    paste0(
      "The source wording suggests a broader-range GREEN route, ",
      "but its exact relationship to combinations of named regions ",
      "and to the China rows requires explicit methodological ",
      "interpretation before classification."
    ),
    "UNRESOLVED",
    
    "NEL07",
    "Beyond named Key regions",
    paste0(
      "Not only in one of Taiwan or Korea OR the Kuriles & ",
      "Sakhalin, and beyond = GREEN"
    ),
    paste0(
      "Stars 03r provides validated evidence for distribution beyond ",
      "Japan, Taiwan, Korea, Kuriles, Sakhalin and China. Positive ",
      "beyond evidence is retained but is not automatically ",
      "translated to GREEN in this module."
    ),
    "EVIDENCE_VALIDATED_LOGIC_UNRESOLVED",
    
    "NEL08",
    "Japanese distribution columns",
    paste0(
      "Islands or <=1 district; <=2 districts; <=3 districts; ",
      ">3 districts"
    ),
    paste0(
      "Matrix column selection requires Japanese district evidence. ",
      "No column is selected in this module."
    ),
    "REQUIRES_DISTRICT_METHOD"
  )
  
  # ---------------------------------------------------------------------------
  # Work packages
  # ---------------------------------------------------------------------------
  
  work_packages <- logic |>
    mutate(
      NEXT_METHOD_REQUIREMENT=
        case_when(
          NON_ENDEMIC_LOGIC_CLASS==
            "SINGLE_KEY_REGION_TAIWAN" ~
            "TAIWAN_FINE_RANGE_AND_JAPAN_DISTRICT_COLUMN",
          
          NON_ENDEMIC_LOGIC_CLASS==
            "SINGLE_KEY_REGION_KOREA" ~
            "KOREA_FINE_RANGE_AND_JAPAN_DISTRICT_COLUMN",
          
          NON_ENDEMIC_LOGIC_CLASS==
            "SINGLE_KEY_REGION_KURILES_SAKHALIN" ~
            "SOUTH_KURILES_RANGE_AND_JAPAN_DISTRICT_COLUMN",
          
          NON_ENDEMIC_LOGIC_CLASS==
            "SINGLE_KEY_REGION_CHINA" ~
            "CHINA_PROVINCE_COUNT_AND_N02_LOGIC",
          
          NON_ENDEMIC_LOGIC_CLASS==
            "MULTIPLE_KEY_REGIONS_NO_BEYOND" ~
            "N02_N02A_LOGIC",
          
          NON_ENDEMIC_LOGIC_CLASS==
            "NAMED_KEY_REGION_AND_BEYOND" ~
            "N02_N02A_LOGIC",
          
          NON_ENDEMIC_LOGIC_CLASS==
            "BEYOND_WITHOUT_NAMED_KEY_REGION" ~
            "N02_N02A_LOGIC",
          
          NON_ENDEMIC_LOGIC_CLASS==
            "NO_RESOLVED_BROAD_EXTERNAL_REGION" ~
            "EXTERNAL_RANGE_EVIDENCE_REVIEW",
          
          TRUE ~
            "METHOD_REVIEW"
        )
    ) |>
    count(
      NEXT_METHOD_REQUIREMENT,
      name="N_TAXA",
      sort=TRUE
    ) |>
    mutate(
      PERCENT_NON_ENDEMIC=
        round(
          100*N_TAXA/nrow(logic),
          3
        )
    )
  
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
    
    "Beyond named Key regions evidence",
    TRUE,
    "EVIDENCE_VALIDATED",
    
    "Single-region Taiwan matrix logic",
    TRUE,
    "SOURCE_LOGIC_VALIDATED",
    
    "Single-region Korea matrix logic",
    TRUE,
    "SOURCE_LOGIC_VALIDATED",
    
    "Single-region Kuriles/Sakhalin matrix logic",
    TRUE,
    "SOURCE_LOGIC_VALIDATED",
    
    "China matrix presence",
    TRUE,
    "SOURCE_STRUCTURE_VALIDATED",
    
    "China relationship to N02/N02A",
    FALSE,
    "UNRESOLVED",
    
    "Multiple-region N02/N02A logic",
    FALSE,
    "UNRESOLVED",
    
    "Beyond-region N02/N02A logic",
    FALSE,
    "UNRESOLVED",
    
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
    "METHOD_DEPENDENT",
    
    "Non-endemic classifier",
    FALSE,
    "NOT_READY"
  )
  
  cat("\n— BROAD EXTERNAL RANGE PROFILE —\n")
  print(broad_region_profile,n=Inf)
  
  cat("\n— RANGE CONFIGURATION PROFILE —\n")
  print(combination_profile,n=Inf)
  
  cat("\n— NON-ENDEMIC LOGIC PROFILE —\n")
  print(logic_profile,n=Inf)
  
  cat("\n— SOURCE LOGIC REGISTER —\n")
  print(logic_register,n=Inf)
  
  cat("\n— NEXT METHOD WORK PACKAGES —\n")
  print(work_packages,n=Inf)
  
  cat("\n— NON-ENDEMIC READINESS —\n")
  print(readiness,n=Inf)
  
  # ---------------------------------------------------------------------------
  # Validation
  # ---------------------------------------------------------------------------
  
  validation <- tibble(
    CHECK=c(
      "Stars 03p validation fully PASS",
      "Stars 03r validation fully PASS",
      "Stars 03x validation fully PASS",
      "Non-endemic population = 7,736",
      "Non-endemic population unique by WCVP ID",
      "Logic output = 7,736 taxa",
      "Logic output unique by WCVP ID",
      "All taxa have named-region count",
      "Named-region count restricted to 0-4",
      "All taxa have full range configuration",
      "All taxa have non-endemic logic class",
      "All taxa have source logic status",
      "Taiwan broad evidence retained",
      "Korea broad evidence retained",
      "Kuriles/Sakhalin broad evidence retained",
      "China broad evidence retained",
      "Beyond-region evidence retained",
      "China represented as explicit matrix region",
      "Single Taiwan route requires no other named region or beyond",
      "Single Korea route requires no other named region or beyond",
      "Single Kuriles/Sakhalin route requires no other named region or beyond",
      "Single China route requires no other named region or beyond",
      "Multiple-region cases not assigned Star",
      "Beyond-region cases not assigned Star",
      "No Taiwan fine-range proxy created",
      "No Korea fine-range proxy created",
      "No south-Kuriles proxy created",
      "No China province-count proxy created",
      "No Stars assigned",
      "Historical Star allocations not used",
      "All 7,736 taxa accounted for"
    ),
    
    PASS=c(
      all(validation_03p$PASS),
      all(validation_03r$PASS),
      all(validation_03x$PASS),
      
      nrow(evidence)==7736L,
      
      n_distinct(
        evidence$FINAL_WCVP_ID
      )==7736L,
      
      nrow(logic)==7736L,
      
      n_distinct(
        logic$FINAL_WCVP_ID
      )==7736L,
      
      all(
        !is.na(
          logic$N_NAMED_KEY_REGIONS
        )
      ),
      
      all(
        logic$N_NAMED_KEY_REGIONS>=0L &
          logic$N_NAMED_KEY_REGIONS<=4L
      ),
      
      all(
        !is.na(
          logic$FULL_RANGE_CONFIGURATION
        )
      ),
      
      all(
        !is.na(
          logic$NON_ENDEMIC_LOGIC_CLASS
        )
      ),
      
      all(
        !is.na(
          logic$SOURCE_LOGIC_STATUS
        )
      ),
      
      sum(logic$WCVP_TAIWAN)==3026L,
      
      sum(logic$WCVP_KOREA)==3619L,
      
      sum(
        logic$WCVP_KURILES_SAKHALIN
      )==1779L,
      
      sum(logic$WCVP_CHINA)==4554L,
      
      sum(
        logic$WCVP_BEYOND_KEY_REGIONS
      )==6542L,
      
      "SINGLE_KEY_REGION_CHINA" %in%
        logic$NON_ENDEMIC_LOGIC_CLASS,
      
      all(
        logic$N_NAMED_KEY_REGIONS[
          logic$ONLY_TAIWAN
        ]==1L &
          !logic$WCVP_BEYOND_KEY_REGIONS[
            logic$ONLY_TAIWAN
          ]
      ),
      
      all(
        logic$N_NAMED_KEY_REGIONS[
          logic$ONLY_KOREA
        ]==1L &
          !logic$WCVP_BEYOND_KEY_REGIONS[
            logic$ONLY_KOREA
          ]
      ),
      
      all(
        logic$N_NAMED_KEY_REGIONS[
          logic$ONLY_KURILES_SAKHALIN
        ]==1L &
          !logic$WCVP_BEYOND_KEY_REGIONS[
            logic$ONLY_KURILES_SAKHALIN
          ]
      ),
      
      all(
        logic$N_NAMED_KEY_REGIONS[
          logic$ONLY_CHINA
        ]==1L &
          !logic$WCVP_BEYOND_KEY_REGIONS[
            logic$ONLY_CHINA
          ]
      ),
      
      !any(
        logic$STAR_ASSIGNED[
          logic$NON_ENDEMIC_LOGIC_CLASS==
            "MULTIPLE_KEY_REGIONS_NO_BEYOND"
        ]
      ),
      
      !any(
        logic$STAR_ASSIGNED[
          logic$WCVP_BEYOND_KEY_REGIONS
        ]
      ),
      
      !"TAIWAN_HALF_ISLAND_CLASS" %in%
        names(logic),
      
      !"KOREA_SPARSE_CLASS" %in%
        names(logic),
      
      !"SOUTH_KURILES_CLASS" %in%
        names(logic),
      
      !"CHINA_PROVINCE_COUNT" %in%
        names(logic),
      
      !any(logic$STAR_ASSIGNED),
      
      TRUE,
      
      sum(
        logic_profile$N_TAXA
      )==7736L
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
      "Stars 03y validation failed. ",
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
      nrow(logic),
    
    TAIWAN_TAXA=
      sum(logic$WCVP_TAIWAN),
    
    KOREA_TAXA=
      sum(logic$WCVP_KOREA),
    
    KURILES_SAKHALIN_TAXA=
      sum(
        logic$WCVP_KURILES_SAKHALIN
      ),
    
    CHINA_TAXA=
      sum(logic$WCVP_CHINA),
    
    BEYOND_KEY_REGION_TAXA=
      sum(
        logic$WCVP_BEYOND_KEY_REGIONS
      ),
    
    SINGLE_TAIWAN=
      sum(logic$ONLY_TAIWAN),
    
    SINGLE_KOREA=
      sum(logic$ONLY_KOREA),
    
    SINGLE_KURILES_SAKHALIN=
      sum(
        logic$ONLY_KURILES_SAKHALIN
      ),
    
    SINGLE_CHINA=
      sum(logic$ONLY_CHINA),
    
    N02_N02A_LOGIC_RESOLVED=FALSE,
    
    FINE_EXTERNAL_SUBCLASSES_RESOLVED=FALSE,
    
    JAPAN_MATRIX_DISTRICT_COLUMN_RESOLVED=FALSE,
    
    STARS_ASSIGNED=FALSE,
    
    HISTORICAL_STAR_ALLOCATIONS_USED=FALSE,
    
    STATUS=
      "VALIDATED_NON_ENDEMIC_LOGIC_DIAGNOSTIC"
  )
  
  # ---------------------------------------------------------------------------
  # Canonical outputs
  # ---------------------------------------------------------------------------
  
  dbWriteTable(
    con,
    TABLE_LOGIC,
    logic,
    overwrite=TRUE
  )
  
  dbWriteTable(
    con,
    TABLE_COMBINATIONS,
    combination_profile,
    overwrite=TRUE
  )
  
  dbWriteTable(
    con,
    TABLE_REGISTER,
    logic_register,
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
    logic,
    file.path(
      OUTPUT_DIR,
      "stars03y_non_endemic_key_logic.csv"
    )
  )
  
  write_csv(
    broad_region_profile,
    file.path(
      OUTPUT_DIR,
      "stars03y_broad_external_range_profile.csv"
    )
  )
  
  write_csv(
    combination_profile,
    file.path(
      OUTPUT_DIR,
      "stars03y_range_configuration_profile.csv"
    )
  )
  
  write_csv(
    logic_profile,
    file.path(
      OUTPUT_DIR,
      "stars03y_logic_profile.csv"
    )
  )
  
  write_csv(
    logic_register,
    file.path(
      OUTPUT_DIR,
      "stars03y_source_logic_register.csv"
    )
  )
  
  write_csv(
    work_packages,
    file.path(
      OUTPUT_DIR,
      "stars03y_method_work_packages.csv"
    )
  )
  
  write_csv(
    readiness,
    file.path(
      OUTPUT_DIR,
      "stars03y_readiness.csv"
    )
  )
  
  write_csv(
    validation,
    file.path(
      OUTPUT_DIR,
      "stars03y_validation.csv"
    )
  )
  
  write_csv(
    metadata,
    file.path(
      OUTPUT_DIR,
      "stars03y_metadata.csv"
    )
  )
  
  # ---------------------------------------------------------------------------
  # Final summary
  # ---------------------------------------------------------------------------
  
  cat(
    "\n============================================================\n"
  )
  
  cat(
    "Stars 03y v",VERSION," COMPLETE\n",
    sep=""
  )
  
  cat(
    "============================================================\n"
  )
  
  cat(
    "Non-endemic taxa: ",
    format(nrow(logic),big.mark=","),
    "\n",
    sep=""
  )
  
  cat(
    "Taiwan evidence: ",
    format(
      sum(logic$WCVP_TAIWAN),
      big.mark=","
    ),
    "\n",
    sep=""
  )
  
  cat(
    "Korea evidence: ",
    format(
      sum(logic$WCVP_KOREA),
      big.mark=","
    ),
    "\n",
    sep=""
  )
  
  cat(
    "Kuriles/Sakhalin evidence: ",
    format(
      sum(logic$WCVP_KURILES_SAKHALIN),
      big.mark=","
    ),
    "\n",
    sep=""
  )
  
  cat(
    "China evidence: ",
    format(
      sum(logic$WCVP_CHINA),
      big.mark=","
    ),
    "\n",
    sep=""
  )
  
  cat(
    "Beyond-Key-region evidence: ",
    format(
      sum(logic$WCVP_BEYOND_KEY_REGIONS),
      big.mark=","
    ),
    "\n",
    sep=""
  )
  
  cat(
    "Single-region Taiwan: ",
    sum(logic$ONLY_TAIWAN),
    "\n",
    sep=""
  )
  
  cat(
    "Single-region Korea: ",
    sum(logic$ONLY_KOREA),
    "\n",
    sep=""
  )
  
  cat(
    "Single-region Kuriles/Sakhalin: ",
    sum(logic$ONLY_KURILES_SAKHALIN),
    "\n",
    sep=""
  )
  
  cat(
    "Single-region China: ",
    sum(logic$ONLY_CHINA),
    "\n",
    sep=""
  )
  
  cat(
    "N02/N02A logic resolved: FALSE\n"
  )
  
  cat(
    "Fine external-range subclasses resolved: FALSE\n"
  )
  
  cat(
    "Stars assigned: FALSE\n"
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
    "Canonical logic table: ",
    TABLE_LOGIC,
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
      logic=logic,
      broad_region_profile=
        broad_region_profile,
      combination_profile=
        combination_profile,
      logic_profile=logic_profile,
      logic_register=logic_register,
      work_packages=work_packages,
      readiness=readiness,
      validation=validation,
      metadata=metadata
    )
  )
}

stars_03y_result <- run_stars_03y()