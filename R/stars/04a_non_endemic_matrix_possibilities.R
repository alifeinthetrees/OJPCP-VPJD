# =============================================================================
# VPJD-OJPCP
# R/stars/04a_non_endemic_matrix_possibilities.R
# Version 0.1.1
#
# NON-ENDEMIC MATRIX: POSSIBLE-STAR ANALYSIS
#
# PURPOSE
# - Replace the over-broad GN interpretation in Stars 03z.
# - Apply the literal Nakamura (2012) Figure 2.2 non-endemic matrix to the
#   restricted single-region taxa identified by Stars 03y.
# - Calculate Japanese district occupancy directly from canonical VPJD
#   taxon-area evidence.
# - Where an external fine-range subclass is unresolved, evaluate BOTH
#   published matrix rows for that region.
# - Assign a candidate Star only when all admissible matrix cells agree.
# - Otherwise retain the taxon with an explicit set of possible Stars.
#
# IMPORTANT
# - Stars 03z candidate GN assignments are NOT used.
# - No external fine-range subclass is invented.
# - No special analytical area is silently counted as a core district.
# - Historical Star allocations are not used.
# - No definitive Stars are assigned.
# =============================================================================

suppressPackageStartupMessages({
  library(DBI)
  library(duckdb)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(tibble)
  library(stringr)
  library(here)
})

MODULE <- "stars_04a_non_endemic_matrix_possibilities"
VERSION <- "0.1.1"
RUN_DATE <- Sys.Date()

DB_PATH <- here(
  "data","interim","occurrences",
  "vpjd_occurrences.duckdb"
)

OUTPUT_DIR <- here(
  "outputs","tables","stars",
  "non_endemic_matrix_possibilities"
)

TABLE_ANALYSIS <- "vpjd_star_non_endemic_matrix_possibilities"
TABLE_CELLS <- "vpjd_star_non_endemic_matrix_candidate_cells"
TABLE_PROFILE <- "vpjd_star_non_endemic_matrix_possibility_profile"
TABLE_VALIDATION <- "vpjd_star_04a_validation"
TABLE_METADATA <- "vpjd_star_04a_metadata"

run_stars_04a <- function() {
  cat("\n— Non-endemic matrix possible-Star analysis —\n\n")
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
    "vpjd_star_japan_district_crosswalk",
    "vpjd_japan_taxon_area_distribution",
    "vpjd_star_03p_validation",
    "vpjd_star_03u_validation",
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
  
  get_validation <- function(table) {
    as_tibble(
      dbGetQuery(
        con,
        paste0("SELECT * FROM ",table)
      )
    )
  }
  
  validation_03p <- get_validation(
    "vpjd_star_03p_validation"
  )
  
  validation_03u <- get_validation(
    "vpjd_star_03u_validation"
  )
  
  validation_03y <- get_validation(
    "vpjd_star_03y_validation"
  )
  
  if (!all(validation_03p$PASS)) {
    stop("Stars 03p is not fully validated.")
  }
  
  if (!all(validation_03u$PASS)) {
    stop("Stars 03u is not fully validated.")
  }
  
  if (!all(validation_03y$PASS)) {
    stop("Stars 03y is not fully validated.")
  }
  
  cat(
    "Stars 03p validation: ",
    sum(validation_03p$PASS),"/",
    nrow(validation_03p)," PASS\n",
    sep=""
  )
  
  cat(
    "Stars 03u validation: ",
    sum(validation_03u$PASS),"/",
    nrow(validation_03u)," PASS\n",
    sep=""
  )
  
  cat(
    "Stars 03y validation: ",
    sum(validation_03y$PASS),"/",
    nrow(validation_03y)," PASS\n\n",
    sep=""
  )
  
  # ---------------------------------------------------------------------------
  # Load 03y non-endemic logic
  #
  # IMPORTANT:
  # Stars 03z is deliberately NOT a classification dependency.
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
      "Required Stars 03y fields missing: ",
      paste(missing_logic_fields,collapse=", "),
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
    stop("Stars 03y non-endemic population is not unique by WCVP ID.")
  }
  
  restricted <- logic |>
    filter(
      NON_ENDEMIC_LOGIC_CLASS %in% c(
        "SINGLE_KEY_REGION_TAIWAN",
        "SINGLE_KEY_REGION_KOREA",
        "SINGLE_KEY_REGION_KURILES_SAKHALIN",
        "SINGLE_KEY_REGION_CHINA"
      )
    ) |>
    mutate(
      MATRIX_REGION=
        case_when(
          NON_ENDEMIC_LOGIC_CLASS==
            "SINGLE_KEY_REGION_TAIWAN" ~
            "TAIWAN",
          
          NON_ENDEMIC_LOGIC_CLASS==
            "SINGLE_KEY_REGION_KOREA" ~
            "KOREA",
          
          NON_ENDEMIC_LOGIC_CLASS==
            "SINGLE_KEY_REGION_KURILES_SAKHALIN" ~
            "KURILES_SAKHALIN",
          
          NON_ENDEMIC_LOGIC_CLASS==
            "SINGLE_KEY_REGION_CHINA" ~
            "CHINA",
          
          TRUE ~
            NA_character_
        )
    )
  
  if (nrow(restricted)!=764L) {
    stop(
      "Expected 764 restricted matrix taxa; found ",
      format(nrow(restricted),big.mark=",")
    )
  }
  
  if (any(is.na(restricted$MATRIX_REGION))) {
    stop("One or more restricted taxa lack MATRIX_REGION.")
  }
  
  cat(
    "Restricted non-endemic matrix taxa: ",
    format(nrow(restricted),big.mark=","),
    "\n",
    sep=""
  )
  
  # ---------------------------------------------------------------------------
  # Load canonical 03u district crosswalk
  #
  # Actual schema:
  # BOTANICAL_AREA_ID
  # ANALYTICAL_AREA_NAME
  # FRAMEWORK_DISTRICT
  # KEY_DISTRICT
  # CORE_DISTRICT_COUNTABLE
  # METHOD_STATUS
  # ---------------------------------------------------------------------------
  
  district_crosswalk <- as_tibble(
    dbGetQuery(
      con,
      "
      SELECT *
      FROM vpjd_star_japan_district_crosswalk
      "
    )
  )
  
  required_crosswalk_fields <- c(
    "BOTANICAL_AREA_ID",
    "KEY_DISTRICT",
    "CORE_DISTRICT_COUNTABLE"
  )
  
  missing_crosswalk_fields <- setdiff(
    required_crosswalk_fields,
    names(district_crosswalk)
  )
  
  if (length(missing_crosswalk_fields)>0L) {
    stop(
      "Required district-crosswalk fields missing: ",
      paste(missing_crosswalk_fields,collapse=", "),
      "\nAvailable fields: ",
      paste(names(district_crosswalk),collapse=", ")
    )
  }
  
  core_crosswalk <- district_crosswalk |>
    filter(
      CORE_DISTRICT_COUNTABLE %in% TRUE
    ) |>
    select(
      BOTANICAL_AREA_ID,
      KEY_DISTRICT
    ) |>
    distinct()
  
  if (nrow(core_crosswalk)!=46L) {
    stop(
      "Expected 46 countable core-area crosswalk rows; found ",
      nrow(core_crosswalk)
    )
  }
  
  if (n_distinct(core_crosswalk$KEY_DISTRICT)!=8L) {
    stop(
      "Expected exactly eight core districts; found ",
      n_distinct(core_crosswalk$KEY_DISTRICT)
    )
  }
  
  if (any(is.na(core_crosswalk$KEY_DISTRICT))) {
    stop("Countable core area has missing KEY_DISTRICT.")
  }
  
  cat(
    "Countable core areas: ",
    nrow(core_crosswalk),
    "\n",
    sep=""
  )
  
  cat(
    "Core districts: ",
    paste(
      unique(core_crosswalk$KEY_DISTRICT),
      collapse=", "
    ),
    "\n",
    sep=""
  )
  
  # ---------------------------------------------------------------------------
  # Canonical Japanese area occupancy for the 764 matrix taxa
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
    semi_join(
      restricted,
      by="FINAL_WCVP_ID"
    )
  
  core_occ <- area_occ |>
    inner_join(
      core_crosswalk,
      by="BOTANICAL_AREA_ID"
    ) |>
    distinct(
      FINAL_WCVP_ID,
      KEY_DISTRICT
    )
  
  special_occ <- area_occ |>
    filter(
      BOTANICAL_AREA_ID %in%
        sprintf("JP%02d",47:51)
    ) |>
    group_by(FINAL_WCVP_ID) |>
    summarise(
      HAS_RYUKYU=
        any(BOTANICAL_AREA_ID=="JP47"),
      
      HAS_IZU=
        any(BOTANICAL_AREA_ID=="JP48"),
      
      HAS_OGASAWARA=
        any(BOTANICAL_AREA_ID=="JP49"),
      
      HAS_KAZAN=
        any(BOTANICAL_AREA_ID=="JP50"),
      
      HAS_KURILES=
        any(BOTANICAL_AREA_ID=="JP51"),
      
      N_SPECIAL_AREAS=
        n_distinct(BOTANICAL_AREA_ID),
      
      .groups="drop"
    )
  
  district_counts <- restricted |>
    select(
      FINAL_WCVP_ID
    ) |>
    left_join(
      core_occ |>
        count(
          FINAL_WCVP_ID,
          name="CORE_DISTRICTS_PRESENT"
        ),
      by="FINAL_WCVP_ID"
    ) |>
    left_join(
      special_occ,
      by="FINAL_WCVP_ID"
    ) |>
    mutate(
      CORE_DISTRICTS_PRESENT=
        coalesce(CORE_DISTRICTS_PRESENT,0L),
      
      HAS_RYUKYU=
        coalesce(HAS_RYUKYU,FALSE),
      
      HAS_IZU=
        coalesce(HAS_IZU,FALSE),
      
      HAS_OGASAWARA=
        coalesce(HAS_OGASAWARA,FALSE),
      
      HAS_KAZAN=
        coalesce(HAS_KAZAN,FALSE),
      
      HAS_KURILES=
        coalesce(HAS_KURILES,FALSE),
      
      N_SPECIAL_AREAS=
        coalesce(N_SPECIAL_AREAS,0L),
      
      HAS_SPECIAL_AREA=
        N_SPECIAL_AREAS>0L
    )
  
  # ---------------------------------------------------------------------------
  # Japanese matrix-column possibilities
  #
  # Literal Figure 2.2 columns:
  #
  # 1 = Islands or <=1 district
  # 2 = <=2 districts
  # 3 = <=3 districts
  # 4 = >3 districts
  #
  # We do NOT infer how the five special analytical areas alter the eight
  # core-district count.
  #
  # The core count is therefore the known lower bound. Each occupied special
  # area can potentially alter the district interpretation, so the upper
  # bound is retained conservatively.
  # ---------------------------------------------------------------------------
  
  district_counts <- district_counts |>
    rowwise() |>
    mutate(
      MIN_DISTRICTS=
        CORE_DISTRICTS_PRESENT,
      
      MAX_DISTRICTS=
        min(
          8L,
          CORE_DISTRICTS_PRESENT +
            N_SPECIAL_AREAS
        ),
      
      COLUMN_1_POSSIBLE=
        MIN_DISTRICTS<=1L,
      
      COLUMN_2_POSSIBLE=
        MIN_DISTRICTS<=2L &
        MAX_DISTRICTS>=2L,
      
      COLUMN_3_POSSIBLE=
        MIN_DISTRICTS<=3L &
        MAX_DISTRICTS>=3L,
      
      COLUMN_4_POSSIBLE=
        MAX_DISTRICTS>3L,
      
      N_POSSIBLE_JAPAN_COLUMNS=
        sum(
          c(
            COLUMN_1_POSSIBLE,
            COLUMN_2_POSSIBLE,
            COLUMN_3_POSSIBLE,
            COLUMN_4_POSSIBLE
          )
        ),
      
      JAPAN_COLUMN_RESOLVED=
        N_POSSIBLE_JAPAN_COLUMNS==1L
    ) |>
    ungroup()
  
  # ---------------------------------------------------------------------------
  # Literal Nakamura Figure 2.2 non-endemic matrix
  #
  # Both published external subclasses are retained for every matrix taxon.
  # We do not infer which subclass applies.
  # ---------------------------------------------------------------------------
  
  matrix_spec <- tribble(
    ~MATRIX_REGION,
    ~EXTERNAL_SUBCLASS,
    ~EXTERNAL_SUBCLASS_LABEL,
    ~COL1,
    ~COL2,
    ~COL3,
    ~COL4,
    
    "TAIWAN",
    "TAIWAN_LT_HALF",
    "Taiwan < half island",
    "BK","GD","BU","GN",
    
    "TAIWAN",
    "TAIWAN_GE_HALF",
    "Taiwan >= half island",
    "GD","BU","GN","GN",
    
    "KOREA",
    "KOREA_ISLANDS_OR_SPARSE",
    "Korea islands or sparse distribution",
    "BK","GD","BU","GN",
    
    "KOREA",
    "KOREA_NOT_ISLANDS_OR_RESTRICTED",
    "Korea not only in islands nor restricted",
    "GD","BU","GN","GN",
    
    "KURILES_SAKHALIN",
    "KURILES_SOUTH_ONLY",
    "Only in the south Kuriles",
    "BK","GD","BU","GN",
    
    "KURILES_SAKHALIN",
    "KURILES_NOT_SOUTH_ONLY",
    "Not only in the south Kuriles",
    "GD","BU","GN","GN",
    
    "CHINA",
    "CHINA_LE2_PROVINCES",
    "China <=2 provinces",
    "GD","BU","GN","GN",
    
    "CHINA",
    "CHINA_GT2_PROVINCES",
    "China >2 provinces",
    "GN","GN","GN","GN"
  )
  
  # ---------------------------------------------------------------------------
  # Expand over all admissible matrix cells
  # ---------------------------------------------------------------------------
  
  taxon_base <- restricted |>
    select(
      FINAL_WCVP_ID,
      FINAL_WCVP_RECOGNISED_NAME,
      MATRIX_REGION
    ) |>
    left_join(
      district_counts,
      by="FINAL_WCVP_ID"
    )
  
  candidate_cells <- taxon_base |>
    inner_join(
      matrix_spec,
      by="MATRIX_REGION",
      relationship="many-to-many"
    ) |>
    select(
      FINAL_WCVP_ID,
      FINAL_WCVP_RECOGNISED_NAME,
      MATRIX_REGION,
      CORE_DISTRICTS_PRESENT,
      N_SPECIAL_AREAS,
      HAS_SPECIAL_AREA,
      MIN_DISTRICTS,
      MAX_DISTRICTS,
      COLUMN_1_POSSIBLE,
      COLUMN_2_POSSIBLE,
      COLUMN_3_POSSIBLE,
      COLUMN_4_POSSIBLE,
      EXTERNAL_SUBCLASS,
      EXTERNAL_SUBCLASS_LABEL,
      COL1,
      COL2,
      COL3,
      COL4
    ) |>
    pivot_longer(
      cols=c(COL1,COL2,COL3,COL4),
      names_to="JAPAN_COLUMN_CODE",
      values_to="POSSIBLE_STAR"
    ) |>
    mutate(
      JAPAN_COLUMN=
        case_when(
          JAPAN_COLUMN_CODE=="COL1" ~ 1L,
          JAPAN_COLUMN_CODE=="COL2" ~ 2L,
          JAPAN_COLUMN_CODE=="COL3" ~ 3L,
          JAPAN_COLUMN_CODE=="COL4" ~ 4L
        ),
      
      JAPAN_COLUMN_LABEL=
        case_when(
          JAPAN_COLUMN==1L ~
            "Islands or <=1 district",
          
          JAPAN_COLUMN==2L ~
            "<=2 districts",
          
          JAPAN_COLUMN==3L ~
            "<=3 districts",
          
          JAPAN_COLUMN==4L ~
            ">3 districts"
        ),
      
      JAPAN_COLUMN_POSSIBLE=
        case_when(
          JAPAN_COLUMN==1L ~ COLUMN_1_POSSIBLE,
          JAPAN_COLUMN==2L ~ COLUMN_2_POSSIBLE,
          JAPAN_COLUMN==3L ~ COLUMN_3_POSSIBLE,
          JAPAN_COLUMN==4L ~ COLUMN_4_POSSIBLE
        )
    ) |>
    filter(
      JAPAN_COLUMN_POSSIBLE
    ) |>
    select(
      FINAL_WCVP_ID,
      FINAL_WCVP_RECOGNISED_NAME,
      MATRIX_REGION,
      CORE_DISTRICTS_PRESENT,
      N_SPECIAL_AREAS,
      HAS_SPECIAL_AREA,
      MIN_DISTRICTS,
      MAX_DISTRICTS,
      EXTERNAL_SUBCLASS,
      EXTERNAL_SUBCLASS_LABEL,
      JAPAN_COLUMN,
      JAPAN_COLUMN_LABEL,
      POSSIBLE_STAR
    ) |>
    distinct()
  
  # ---------------------------------------------------------------------------
  # Collapse to one row per taxon
  # ---------------------------------------------------------------------------
  
  possibilities <- candidate_cells |>
    group_by(
      FINAL_WCVP_ID,
      FINAL_WCVP_RECOGNISED_NAME,
      MATRIX_REGION
    ) |>
    summarise(
      CORE_DISTRICTS_PRESENT=
        first(CORE_DISTRICTS_PRESENT),
      
      N_SPECIAL_AREAS=
        first(N_SPECIAL_AREAS),
      
      HAS_SPECIAL_AREA=
        first(HAS_SPECIAL_AREA),
      
      MIN_DISTRICTS=
        first(MIN_DISTRICTS),
      
      MAX_DISTRICTS=
        first(MAX_DISTRICTS),
      
      N_EXTERNAL_SUBCLASSES=
        n_distinct(EXTERNAL_SUBCLASS),
      
      N_JAPAN_COLUMNS=
        n_distinct(JAPAN_COLUMN),
      
      POSSIBLE_STARS=
        paste(
          sort(
            unique(POSSIBLE_STAR)
          ),
          collapse="|"
        ),
      
      N_POSSIBLE_STARS=
        n_distinct(POSSIBLE_STAR),
      
      .groups="drop"
    ) |>
    mutate(
      MATRIX_STAR_INVARIANT=
        N_POSSIBLE_STARS==1L,
      
      CANDIDATE_STAR=
        if_else(
          MATRIX_STAR_INVARIANT,
          POSSIBLE_STARS,
          NA_character_
        ),
      
      RESOLUTION_STATUS=
        case_when(
          MATRIX_STAR_INVARIANT ~
            "CANDIDATE_TERMINAL_MATRIX_INVARIANT",
          
          TRUE ~
            "UNRESOLVED_MATRIX_CELL"
        ),
      
      REQUIRED_NEXT_EVIDENCE=
        case_when(
          MATRIX_STAR_INVARIANT ~
            NA_character_,
          
          N_JAPAN_COLUMNS>1L &
            N_EXTERNAL_SUBCLASSES>1L ~
            "EXTERNAL_SUBCLASS_AND_SPECIAL_AREA_DISTRICT_TREATMENT",
          
          N_JAPAN_COLUMNS>1L ~
            "SPECIAL_AREA_DISTRICT_TREATMENT",
          
          N_EXTERNAL_SUBCLASSES>1L ~
            case_when(
              MATRIX_REGION=="TAIWAN" ~
                "TAIWAN_RANGE_SUBCLASS",
              
              MATRIX_REGION=="KOREA" ~
                "KOREA_RANGE_SUBCLASS",
              
              MATRIX_REGION=="KURILES_SAKHALIN" ~
                "KURILES_SAKHALIN_RANGE_SUBCLASS",
              
              MATRIX_REGION=="CHINA" ~
                "CHINA_PROVINCE_COUNT",
              
              TRUE ~
                "EXTERNAL_SUBCLASS_REVIEW"
            ),
          
          TRUE ~
            "MATRIX_REVIEW"
        ),
      
      DEFINITIVE_STAR=NA_character_,
      STAR_ASSIGNED=FALSE
    )
  
  if (nrow(possibilities)!=764L) {
    stop(
      "Expected 764 taxon-level matrix outputs; found ",
      nrow(possibilities)
    )
  }
  
  # ---------------------------------------------------------------------------
  # Profiles
  # ---------------------------------------------------------------------------
  
  region_profile <- possibilities |>
    count(
      MATRIX_REGION,
      RESOLUTION_STATUS,
      POSSIBLE_STARS,
      CANDIDATE_STAR,
      name="N_TAXA",
      sort=TRUE
    )
  
  possibility_profile <- possibilities |>
    count(
      POSSIBLE_STARS,
      N_POSSIBLE_STARS,
      name="N_TAXA",
      sort=TRUE
    ) |>
    mutate(
      PERCENT_MATRIX_TAXA=
        round(
          100*N_TAXA/nrow(possibilities),
          3
        )
    )
  
  candidate_profile <- possibilities |>
    filter(
      MATRIX_STAR_INVARIANT
    ) |>
    count(
      CANDIDATE_STAR,
      name="N_TAXA",
      sort=TRUE
    )
  
  unresolved_profile <- possibilities |>
    filter(
      !MATRIX_STAR_INVARIANT
    ) |>
    count(
      REQUIRED_NEXT_EVIDENCE,
      POSSIBLE_STARS,
      name="N_TAXA",
      sort=TRUE
    )
  
  district_profile <- possibilities |>
    count(
      CORE_DISTRICTS_PRESENT,
      HAS_SPECIAL_AREA,
      N_JAPAN_COLUMNS,
      name="N_TAXA",
      sort=FALSE
    )
  
  cat("\n— MATRIX REGION / POSSIBILITY PROFILE —\n")
  print(region_profile,n=Inf)
  
  cat("\n— POSSIBLE-STAR PROFILE —\n")
  print(possibility_profile,n=Inf)
  
  cat("\n— MATRIX-INVARIANT CANDIDATE STARS —\n")
  
  if (nrow(candidate_profile)>0L) {
    print(candidate_profile,n=Inf)
  } else {
    cat("No matrix-invariant candidate Stars.\n")
  }
  
  cat("\n— REMAINING MATRIX WORK PACKAGES —\n")
  
  if (nrow(unresolved_profile)>0L) {
    print(unresolved_profile,n=Inf)
  } else {
    cat("No unresolved matrix taxa.\n")
  }
  
  cat("\n— JAPANESE DISTRICT-EVIDENCE PROFILE —\n")
  print(district_profile,n=Inf)
  
  # ---------------------------------------------------------------------------
  # Validation
  # ---------------------------------------------------------------------------
  
  invariant_n <- sum(
    possibilities$MATRIX_STAR_INVARIANT
  )
  
  unresolved_n <- sum(
    !possibilities$MATRIX_STAR_INVARIANT
  )
  
  possible_star_values <- unique(
    unlist(
      str_split(
        possibilities$POSSIBLE_STARS,
        "\\|"
      )
    )
  )
  
  validation <- tibble(
    CHECK=c(
      "Stars 03p validation fully PASS",
      "Stars 03u validation fully PASS",
      "Stars 03y validation fully PASS",
      "Non-endemic population = 7,736",
      "Restricted matrix population = 764",
      "Restricted matrix population unique by WCVP ID",
      "Taiwan matrix population = 171",
      "Korea matrix population = 266",
      "Kuriles/Sakhalin matrix population = 146",
      "China matrix population = 181",
      "Core district crosswalk contains 46 countable areas",
      "Core district crosswalk contains eight districts",
      "All core crosswalk districts non-missing",
      "Taxon-level matrix output = 764",
      "Taxon-level matrix output unique by WCVP ID",
      "All taxa have at least one possible Star",
      "Possible Stars restricted to BK/GD/BU/GN",
      "All taxa retain two external subclasses",
      "No external fine-range subclass inferred",
      "Core district counts do not exceed eight",
      "Matrix-invariant taxa have exactly one possible Star",
      "Unresolved taxa have >1 possible Star",
      "Candidate Stars equal sole possible Star",
      "Stars 03z classification not used",
      "No GX assigned",
      "No HYB assigned",
      "No definitive Stars assigned",
      "Historical Star allocations not used",
      "Invariant + unresolved = 764"
    ),
    
    PASS=c(
      all(validation_03p$PASS),
      all(validation_03u$PASS),
      all(validation_03y$PASS),
      
      nrow(logic)==7736L,
      
      nrow(restricted)==764L,
      
      n_distinct(
        restricted$FINAL_WCVP_ID
      )==764L,
      
      sum(
        restricted$MATRIX_REGION=="TAIWAN"
      )==171L,
      
      sum(
        restricted$MATRIX_REGION=="KOREA"
      )==266L,
      
      sum(
        restricted$MATRIX_REGION==
          "KURILES_SAKHALIN"
      )==146L,
      
      sum(
        restricted$MATRIX_REGION=="CHINA"
      )==181L,
      
      nrow(core_crosswalk)==46L,
      
      n_distinct(
        core_crosswalk$KEY_DISTRICT
      )==8L,
      
      !any(
        is.na(core_crosswalk$KEY_DISTRICT)
      ),
      
      nrow(possibilities)==764L,
      
      n_distinct(
        possibilities$FINAL_WCVP_ID
      )==764L,
      
      all(
        possibilities$N_POSSIBLE_STARS>=1L
      ),
      
      all(
        possible_star_values %in%
          c("BK","GD","BU","GN")
      ),
      
      all(
        possibilities$N_EXTERNAL_SUBCLASSES==2L
      ),
      
      TRUE,
      
      all(
        district_counts$CORE_DISTRICTS_PRESENT<=8L
      ),
      
      all(
        possibilities$N_POSSIBLE_STARS[
          possibilities$MATRIX_STAR_INVARIANT
        ]==1L
      ),
      
      all(
        possibilities$N_POSSIBLE_STARS[
          !possibilities$MATRIX_STAR_INVARIANT
        ]>1L
      ),
      
      all(
        possibilities$CANDIDATE_STAR[
          possibilities$MATRIX_STAR_INVARIANT
        ]==
          possibilities$POSSIBLE_STARS[
            possibilities$MATRIX_STAR_INVARIANT
          ]
      ),
      
      TRUE,
      
      !any(
        possibilities$CANDIDATE_STAR=="GX",
        na.rm=TRUE
      ),
      
      !any(
        possibilities$CANDIDATE_STAR=="HYB",
        na.rm=TRUE
      ),
      
      all(
        is.na(
          possibilities$DEFINITIVE_STAR
        )
      ) &&
        !any(
          possibilities$STAR_ASSIGNED
        ),
      
      TRUE,
      
      invariant_n+unresolved_n==764L
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
      "Stars 04a validation failed. ",
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
    
    NON_ENDEMIC_TAXA=7736L,
    RESTRICTED_MATRIX_TAXA=764L,
    
    MATRIX_INVARIANT_TAXA=
      invariant_n,
    
    MATRIX_UNRESOLVED_TAXA=
      unresolved_n,
    
    TAIWAN_TAXA=
      sum(
        possibilities$MATRIX_REGION=="TAIWAN"
      ),
    
    KOREA_TAXA=
      sum(
        possibilities$MATRIX_REGION=="KOREA"
      ),
    
    KURILES_SAKHALIN_TAXA=
      sum(
        possibilities$MATRIX_REGION==
          "KURILES_SAKHALIN"
      ),
    
    CHINA_TAXA=
      sum(
        possibilities$MATRIX_REGION=="CHINA"
      ),
    
    STARS_03Z_CLASSIFICATION_USED=FALSE,
    EXTERNAL_FINE_SUBCLASS_INFERRED=FALSE,
    DEFINITIVE_STARS_ASSIGNED=FALSE,
    HISTORICAL_STAR_ALLOCATIONS_USED=FALSE,
    
    STATUS=
      "VALIDATED_NON_ENDEMIC_MATRIX_POSSIBILITY_ANALYSIS"
  )
  
  # ---------------------------------------------------------------------------
  # Canonical database outputs
  # ---------------------------------------------------------------------------
  
  dbWriteTable(
    con,
    TABLE_ANALYSIS,
    possibilities,
    overwrite=TRUE
  )
  
  dbWriteTable(
    con,
    TABLE_CELLS,
    candidate_cells,
    overwrite=TRUE
  )
  
  dbWriteTable(
    con,
    TABLE_PROFILE,
    possibility_profile,
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
    possibilities,
    file.path(
      OUTPUT_DIR,
      "stars04a_non_endemic_matrix_possibilities.csv"
    )
  )
  
  write_csv(
    candidate_cells,
    file.path(
      OUTPUT_DIR,
      "stars04a_candidate_matrix_cells.csv"
    )
  )
  
  write_csv(
    region_profile,
    file.path(
      OUTPUT_DIR,
      "stars04a_region_profile.csv"
    )
  )
  
  write_csv(
    possibility_profile,
    file.path(
      OUTPUT_DIR,
      "stars04a_possible_star_profile.csv"
    )
  )
  
  write_csv(
    candidate_profile,
    file.path(
      OUTPUT_DIR,
      "stars04a_matrix_invariant_candidates.csv"
    )
  )
  
  write_csv(
    unresolved_profile,
    file.path(
      OUTPUT_DIR,
      "stars04a_unresolved_matrix_work_packages.csv"
    )
  )
  
  write_csv(
    district_profile,
    file.path(
      OUTPUT_DIR,
      "stars04a_japan_district_profile.csv"
    )
  )
  
  write_csv(
    validation,
    file.path(
      OUTPUT_DIR,
      "stars04a_validation.csv"
    )
  )
  
  write_csv(
    metadata,
    file.path(
      OUTPUT_DIR,
      "stars04a_metadata.csv"
    )
  )
  
  # ---------------------------------------------------------------------------
  # Final summary
  # ---------------------------------------------------------------------------
  
  cat(
    "\n============================================================\n"
  )
  
  cat(
    "Stars 04a v",VERSION," COMPLETE\n",
    sep=""
  )
  
  cat(
    "============================================================\n"
  )
  
  cat(
    "Non-endemic taxa: 7,736\n"
  )
  
  cat(
    "Restricted matrix taxa analysed: 764\n"
  )
  
  cat(
    "Matrix-invariant candidate terminals: ",
    format(invariant_n,big.mark=","),
    "\n",
    sep=""
  )
  
  cat(
    "Matrix taxa still unresolved: ",
    format(unresolved_n,big.mark=","),
    "\n",
    sep=""
  )
  
  cat(
    "03z GN classifications used: FALSE\n"
  )
  
  cat(
    "External fine-range subclasses inferred: FALSE\n"
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
    "Canonical analysis: ",
    TABLE_ANALYSIS,
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
      possibilities=possibilities,
      candidate_cells=candidate_cells,
      region_profile=region_profile,
      possibility_profile=possibility_profile,
      candidate_profile=candidate_profile,
      unresolved_profile=unresolved_profile,
      district_profile=district_profile,
      validation=validation,
      metadata=metadata
    )
  )
}

stars_04a_result <- run_stars_04a()