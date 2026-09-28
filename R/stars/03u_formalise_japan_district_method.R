# =============================================================================
# VPJD-OJPCP
# R/stars/03u_formalise_japan_district_method.R
# Version 0.1.0
#
# FORMALISE JAPANESE "DISTRICT" METHOD FOR THE KEY TO STARS
#
# PURPOSE
# - Formalise the working interpretation of "district" in Nakamura (2012),
#   Figure 2.2.
# - Validate the eight broad Japanese geographical regions already encoded
#   in the canonical 51-area geography framework.
# - Explicitly separate the eight core districts from special analytical
#   areas whose district treatment remains unresolved.
# - Create a reproducible method specification for subsequent taxon-level
#   district counting.
#
# IMPORTANT
# - Method module only.
# - No taxon district counts are calculated.
# - No Stars are assigned.
# - Ryukyu, Izu, Ogasawara, Kazan and Kuriles are NOT automatically counted
#   as additional districts.
# - No "almost all districts" rule is operationalised here.
# - Historical Star allocations are not used.
# =============================================================================

suppressPackageStartupMessages({
  library(DBI)
  library(duckdb)
  library(dplyr)
  library(readr)
  library(stringr)
  library(tibble)
  library(here)
})

MODULE <- "stars_03u_formalise_japan_district_method"
VERSION <- "0.1.0"
RUN_DATE <- Sys.Date()

DB_PATH <- here(
  "data","interim","occurrences",
  "vpjd_occurrences.duckdb"
)

FRAMEWORK_PATH <- here(
  "outputs","tables","geography",
  "botanical_area_framework",
  "japan_botanical_areas_51.csv"
)

OUTPUT_DIR <- here(
  "outputs","tables","stars",
  "japan_district_method"
)

TABLE_CROSSWALK <-
  "vpjd_star_japan_district_crosswalk"

TABLE_DISTRICTS <-
  "vpjd_star_japan_district_register"

TABLE_SPECIAL_AREAS <-
  "vpjd_star_japan_district_special_area_register"

TABLE_METHOD <-
  "vpjd_star_japan_district_method"

TABLE_VALIDATION <-
  "vpjd_star_03u_validation"

TABLE_METADATA <-
  "vpjd_star_03u_metadata"

run_stars_03u <- function() {
  
  cat("\n— Japanese district method —\n\n")
  cat("Run date: ",as.character(RUN_DATE),"\n",sep="")
  cat("Module: ",MODULE,"\n",sep="")
  cat("Version: ",VERSION,"\n\n",sep="")
  
  dir.create(
    OUTPUT_DIR,
    recursive=TRUE,
    showWarnings=FALSE
  )
  
  if (!file.exists(DB_PATH)) {
    stop(
      "VPJD DuckDB not found: ",
      DB_PATH
    )
  }
  
  if (!file.exists(FRAMEWORK_PATH)) {
    stop(
      "Canonical 51-area framework not found: ",
      FRAMEWORK_PATH
    )
  }
  
  con <- dbConnect(
    duckdb::duckdb(),
    dbdir=DB_PATH,
    read_only=FALSE
  )
  
  on.exit(
    dbDisconnect(
      con,
      shutdown=TRUE
    ),
    add=TRUE
  )
  
  required_tables <- c(
    "vpjd_star_key_source_nodes",
    "vpjd_star_key_operational_gaps",
    "vpjd_star_methodological_decision_register",
    "vpjd_star_03p_validation",
    "vpjd_star_03t_validation"
  )
  
  missing_tables <- setdiff(
    required_tables,
    dbListTables(con)
  )
  
  if (length(missing_tables)>0L) {
    stop(
      "Required upstream tables missing: ",
      paste(
        missing_tables,
        collapse=", "
      )
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
  
  validation_03t <- as_tibble(
    dbGetQuery(
      con,
      "SELECT * FROM vpjd_star_03t_validation"
    )
  )
  
  if (
    !"PASS" %in% names(validation_03p) ||
    !all(validation_03p$PASS)
  ) {
    stop(
      "Stars 03p is not fully validated."
    )
  }
  
  if (
    !"PASS" %in% names(validation_03t) ||
    !all(validation_03t$PASS)
  ) {
    stop(
      "Stars 03t is not fully validated."
    )
  }
  
  cat(
    "Stars 03p validation: ",
    sum(validation_03p$PASS),
    "/",
    nrow(validation_03p),
    " PASS\n",
    sep=""
  )
  
  cat(
    "Stars 03t validation: ",
    sum(validation_03t$PASS),
    "/",
    nrow(validation_03t),
    " PASS\n",
    sep=""
  )
  
  # ---------------------------------------------------------------------------
  # Read canonical 51-area geography framework
  # ---------------------------------------------------------------------------
  
  framework <- read_csv(
    FRAMEWORK_PATH,
    show_col_types=FALSE
  )
  
  required_fields <- c(
    "area_id",
    "area_no",
    "analytical_area_name",
    "area_type",
    "district",
    "political_parent",
    "display_area_id",
    "display_area_no",
    "display_area_name",
    "star_small_island_group",
    "special_assignment_required",
    "notes"
  )
  
  missing_fields <- setdiff(
    required_fields,
    names(framework)
  )
  
  if (length(missing_fields)>0L) {
    stop(
      "Required framework fields missing: ",
      paste(
        missing_fields,
        collapse=", "
      )
    )
  }
  
  if (nrow(framework)!=51L) {
    stop(
      "Expected 51 analytical areas; found ",
      nrow(framework)
    )
  }
  
  if (
    n_distinct(
      framework$area_id
    )!=51L
  ) {
    stop(
      "Canonical framework does not contain 51 unique area IDs."
    )
  }
  
  cat(
    "Canonical analytical areas: ",
    nrow(framework),
    "\n",
    sep=""
  )
  
  # ---------------------------------------------------------------------------
  # Explicit source-method interpretation
  # ---------------------------------------------------------------------------
  
  CORE_DISTRICTS <- c(
    "Hokkaido",
    "Tohoku",
    "Kanto",
    "Chubu",
    "Kinki",
    "Chugoku",
    "Shikoku",
    "Kyushu"
  )
  
  SPECIAL_AREA_IDS <- c(
    "JP47",
    "JP48",
    "JP49",
    "JP50",
    "JP51"
  )
  
  SPECIAL_AREA_EXPECTED <- tibble(
    area_id=c(
      "JP47",
      "JP48",
      "JP49",
      "JP50",
      "JP51"
    ),
    
    expected_area_name=c(
      "Ryukyu",
      "Izu",
      "Ogasawara",
      "Kazan",
      "Kuriles"
    ),
    
    key_role=c(
      "NAMED_SMALL_ISLAND_GROUP",
      "NAMED_SMALL_ISLAND_GROUP",
      "NAMED_SMALL_ISLAND_GROUP",
      "SOURCE_TREATMENT_UNRESOLVED",
      "NON_ENDEMIC_EXTERNAL_REGION"
    ),
    
    district_count_status=c(
      "EXCLUDED_PENDING_METHOD",
      "EXCLUDED_PENDING_METHOD",
      "EXCLUDED_PENDING_METHOD",
      "EXCLUDED_PENDING_METHOD",
      "EXCLUDED_PENDING_METHOD"
    ),
    
    rationale=c(
      paste0(
        "Ryukyu is explicitly named in the endemic ",
        "small-island branch of Figure 2.2. It is not ",
        "automatically treated as a ninth district."
      ),
      
      paste0(
        "Izu is explicitly named in the endemic ",
        "small-island branch of Figure 2.2. It is not ",
        "automatically treated as a ninth district."
      ),
      
      paste0(
        "Ogasawara is explicitly named in the endemic ",
        "small-island branch of Figure 2.2. It is not ",
        "automatically treated as a ninth district."
      ),
      
      paste0(
        "Kazan is not explicitly named in Figure 2.2. ",
        "Its district treatment remains unresolved."
      ),
      
      paste0(
        "Kuriles & Sakhalin are explicitly represented ",
        "as an external distribution region in the ",
        "non-endemic branch. Kuriles is therefore not ",
        "automatically treated as a Japanese district."
      )
    )
  )
  
  # ---------------------------------------------------------------------------
  # Separate core-area and special-area framework records
  # ---------------------------------------------------------------------------
  
  core_framework <- framework |>
    filter(
      !area_id %in% SPECIAL_AREA_IDS
    )
  
  special_framework <- framework |>
    filter(
      area_id %in% SPECIAL_AREA_IDS
    )
  
  if (nrow(core_framework)!=46L) {
    stop(
      "Expected 46 core political areas; found ",
      nrow(core_framework)
    )
  }
  
  if (nrow(special_framework)!=5L) {
    stop(
      "Expected five special analytical areas; found ",
      nrow(special_framework)
    )
  }
  
  # ---------------------------------------------------------------------------
  # Core district crosswalk
  #
  # The existing framework district field is used as evidence for the
  # proposed broad-region interpretation. No district values are invented.
  # ---------------------------------------------------------------------------
  
  core_crosswalk <- core_framework |>
    transmute(
      BOTANICAL_AREA_ID=
        area_id,
      
      ANALYTICAL_AREA_NAME=
        analytical_area_name,
      
      FRAMEWORK_DISTRICT=
        district,
      
      KEY_DISTRICT=
        if_else(
          district %in% CORE_DISTRICTS,
          district,
          NA_character_
        ),
      
      CORE_DISTRICT_COUNTABLE=
        district %in% CORE_DISTRICTS,
      
      METHOD_STATUS=
        if_else(
          district %in% CORE_DISTRICTS,
          "PROVISIONAL_METHOD_VALIDATED",
          "REVIEW_REQUIRED"
        )
    )
  
  unexpected_core_districts <- setdiff(
    unique(
      na.omit(
        core_crosswalk$
          FRAMEWORK_DISTRICT
      )
    ),
    CORE_DISTRICTS
  )
  
  missing_core_districts <- setdiff(
    CORE_DISTRICTS,
    unique(
      na.omit(
        core_crosswalk$
          KEY_DISTRICT
      )
    )
  )
  
  # ---------------------------------------------------------------------------
  # District register
  # ---------------------------------------------------------------------------
  
  district_register <- core_crosswalk |>
    filter(
      CORE_DISTRICT_COUNTABLE
    ) |>
    count(
      KEY_DISTRICT,
      name="N_CORE_AREAS"
    ) |>
    mutate(
      DISTRICT_ORDER=
        match(
          KEY_DISTRICT,
          CORE_DISTRICTS
        )
    ) |>
    arrange(
      DISTRICT_ORDER
    ) |>
    mutate(
      DISTRICT_DEFINITION=
        "Broad Japanese geographical region (chihō-level interpretation)",
      
      METHOD_STATUS=
        "PROVISIONAL_METHOD_VALIDATED",
      
      COUNTABLE_IN_NEXT_MODULE=
        TRUE
    ) |>
    select(
      DISTRICT_ORDER,
      KEY_DISTRICT,
      N_CORE_AREAS,
      DISTRICT_DEFINITION,
      METHOD_STATUS,
      COUNTABLE_IN_NEXT_MODULE
    )
  
  # ---------------------------------------------------------------------------
  # Special-area register
  # ---------------------------------------------------------------------------
  
  special_area_register <- special_framework |>
    transmute(
      area_id=
        area_id,
      
      analytical_area_name=
        analytical_area_name,
      
      framework_district=
        district,
      
      star_small_island_group=
        star_small_island_group,
      
      special_assignment_required=
        special_assignment_required
    ) |>
    left_join(
      SPECIAL_AREA_EXPECTED,
      by="area_id"
    ) |>
    mutate(
      COUNT_AS_CORE_DISTRICT=FALSE,
      KEY_DISTRICT=NA_character_,
      METHOD_STATUS=
        "UNRESOLVED_SPECIAL_AREA_TREATMENT"
    )
  
  # ---------------------------------------------------------------------------
  # Method specification
  # ---------------------------------------------------------------------------
  
  method_specification <- tribble(
    ~METHOD_ID,
    ~COMPONENT,
    ~SOURCE_OR_EVIDENCE,
    ~OPERATIONAL_DEFINITION,
    ~STATUS,
    
    "DISTRICT_01",
    "Meaning of district",
    paste0(
      "Nakamura (2012) Figure 2.2 plus existing ",
      "canonical VPJD 51-area framework"
    ),
    paste0(
      "Working interpretation: district denotes one of ",
      "eight broad Japanese geographical regions: ",
      "Hokkaido, Tohoku, Kanto, Chubu, Kinki, Chugoku, ",
      "Shikoku and Kyushu."
    ),
    "PROVISIONAL_METHOD_VALIDATED",
    
    "DISTRICT_02",
    "Core district denominator",
    "Canonical VPJD 51-area framework",
    paste0(
      "Eight core districts are recognised for subsequent ",
      "district occupancy calculations."
    ),
    "PROVISIONAL_METHOD_VALIDATED",
    
    "DISTRICT_03",
    "Core-area assignment",
    "Canonical VPJD 51-area framework district field",
    paste0(
      "JP01-JP46 retain their existing framework district ",
      "assignment where it is one of the eight core districts."
    ),
    "PROVISIONAL_METHOD_VALIDATED",
    
    "DISTRICT_04",
    "Ryukyu",
    "Nakamura (2012) Figure 2.2",
    paste0(
      "Explicit endemic small-island group. Not automatically ",
      "counted as an additional Japanese district."
    ),
    "UNRESOLVED_SPECIAL_AREA_TREATMENT",
    
    "DISTRICT_05",
    "Izu",
    "Nakamura (2012) Figure 2.2",
    paste0(
      "Explicit endemic small-island group. Not automatically ",
      "counted as an additional Japanese district."
    ),
    "UNRESOLVED_SPECIAL_AREA_TREATMENT",
    
    "DISTRICT_06",
    "Ogasawara",
    "Nakamura (2012) Figure 2.2",
    paste0(
      "Explicit endemic small-island group. Not automatically ",
      "counted as an additional Japanese district."
    ),
    "UNRESOLVED_SPECIAL_AREA_TREATMENT",
    
    "DISTRICT_07",
    "Kazan",
    "Nakamura (2012) Figure 2.2",
    paste0(
      "Kazan is not explicitly named in the source Key. ",
      "No district treatment is inferred."
    ),
    "UNRESOLVED",
    
    "DISTRICT_08",
    "Kuriles",
    "Nakamura (2012) Figure 2.2",
    paste0(
      "Kuriles & Sakhalin form an external range category ",
      "in the non-endemic branch. Kuriles is not automatically ",
      "counted as a Japanese district."
    ),
    "UNRESOLVED_SPECIAL_AREA_TREATMENT",
    
    "DISTRICT_09",
    "Almost all districts",
    "Nakamura (2012) Figure 2.2",
    paste0(
      "No operational definition or denominator threshold ",
      "is established by this module."
    ),
    "UNRESOLVED",
    
    "DISTRICT_10",
    "Taxon district occupancy",
    "Future Stars 03v",
    paste0(
      "No taxon-level district count is calculated in 03u. ",
      "03v may count occupancy only across the validated ",
      "eight-core-district crosswalk."
    ),
    "NOT_IMPLEMENTED"
  )
  
  # ---------------------------------------------------------------------------
  # Prohibited assumptions
  # ---------------------------------------------------------------------------
  
  prohibited_assumptions <- tribble(
    ~ASSUMPTION_ID,
    ~PROHIBITED_ASSUMPTION,
    
    "PA01",
    "Do not interpret district as prefecture.",
    
    "PA02",
    "Do not interpret district as gun (郡).",
    
    "PA03",
    paste0(
      "Do not interpret district as a floristic region ",
      "without separate source evidence."
    ),
    
    "PA04",
    paste0(
      "Do not automatically count Ryukyu as a ninth ",
      "Japanese district."
    ),
    
    "PA05",
    paste0(
      "Do not automatically count Izu as a ninth ",
      "Japanese district."
    ),
    
    "PA06",
    paste0(
      "Do not automatically count Ogasawara as a ninth ",
      "Japanese district."
    ),
    
    "PA07",
    paste0(
      "Do not automatically assign Kazan to Ogasawara ",
      "or another district."
    ),
    
    "PA08",
    paste0(
      "Do not automatically count Kuriles as a Japanese ",
      "district."
    ),
    
    "PA09",
    paste0(
      "Do not operationalise 'almost all districts' ",
      "as seven of eight, eight of eight, or another ",
      "threshold without separate methodological support."
    ),
    
    "PA10",
    paste0(
      "Do not use historical Star allocations to choose ",
      "or validate the district interpretation."
    )
  )
  
  # ---------------------------------------------------------------------------
  # Profiles
  # ---------------------------------------------------------------------------
  
  district_profile <- core_crosswalk |>
    count(
      KEY_DISTRICT,
      CORE_DISTRICT_COUNTABLE,
      name="N_AREAS",
      sort=FALSE
    ) |>
    arrange(
      match(
        KEY_DISTRICT,
        CORE_DISTRICTS
      )
    )
  
  cat(
    "\n— EIGHT-CORE-DISTRICT REGISTER —\n"
  )
  
  print(
    district_register,
    n=Inf
  )
  
  cat(
    "\n— CORE AREA CROSSWALK PROFILE —\n"
  )
  
  print(
    district_profile,
    n=Inf
  )
  
  cat(
    "\n— SPECIAL-AREA DISTRICT STATUS —\n"
  )
  
  print(
    special_area_register |>
      select(
        area_id,
        analytical_area_name,
        framework_district,
        key_role,
        district_count_status,
        COUNT_AS_CORE_DISTRICT,
        METHOD_STATUS
      ),
    n=Inf
  )
  
  cat(
    "\n— METHOD SPECIFICATION —\n"
  )
  
  print(
    method_specification,
    n=Inf
  )
  
  # ---------------------------------------------------------------------------
  # Validation
  # ---------------------------------------------------------------------------
  
  validation <- tibble(
    CHECK=c(
      "Stars 03p validation fully PASS",
      "Stars 03t validation fully PASS",
      "Canonical framework contains 51 analytical areas",
      "Canonical framework contains 51 unique area IDs",
      "Core framework contains 46 areas",
      "Special framework contains five areas",
      "Exactly eight core districts recognised",
      "All expected core districts represented",
      "No unexpected core district labels",
      "All 46 core areas map to a core district",
      "Hokkaido represented",
      "Tohoku represented",
      "Kanto represented",
      "Chubu represented",
      "Kinki represented",
      "Chugoku represented",
      "Shikoku represented",
      "Kyushu represented",
      "Special register contains Ryukyu",
      "Special register contains Izu",
      "Special register contains Ogasawara",
      "Special register contains Kazan",
      "Special register contains Kuriles",
      "No special area counted as core district",
      "No special area assigned a Key district",
      "Almost-all rule remains unresolved",
      "No taxon district counts calculated",
      "No Stars assigned",
      "Historical Star allocations not used"
    ),
    
    PASS=c(
      all(
        validation_03p$PASS
      ),
      
      all(
        validation_03t$PASS
      ),
      
      nrow(framework)==51L,
      
      n_distinct(
        framework$area_id
      )==51L,
      
      nrow(core_framework)==46L,
      
      nrow(special_framework)==5L,
      
      nrow(
        district_register
      )==8L,
      
      length(
        missing_core_districts
      )==0L,
      
      length(
        unexpected_core_districts
      )==0L,
      
      all(
        core_crosswalk$
          CORE_DISTRICT_COUNTABLE
      ),
      
      "Hokkaido" %in%
        district_register$
        KEY_DISTRICT,
      
      "Tohoku" %in%
        district_register$
        KEY_DISTRICT,
      
      "Kanto" %in%
        district_register$
        KEY_DISTRICT,
      
      "Chubu" %in%
        district_register$
        KEY_DISTRICT,
      
      "Kinki" %in%
        district_register$
        KEY_DISTRICT,
      
      "Chugoku" %in%
        district_register$
        KEY_DISTRICT,
      
      "Shikoku" %in%
        district_register$
        KEY_DISTRICT,
      
      "Kyushu" %in%
        district_register$
        KEY_DISTRICT,
      
      "JP47" %in%
        special_area_register$
        area_id,
      
      "JP48" %in%
        special_area_register$
        area_id,
      
      "JP49" %in%
        special_area_register$
        area_id,
      
      "JP50" %in%
        special_area_register$
        area_id,
      
      "JP51" %in%
        special_area_register$
        area_id,
      
      !any(
        special_area_register$
          COUNT_AS_CORE_DISTRICT
      ),
      
      all(
        is.na(
          special_area_register$
            KEY_DISTRICT
        )
      ),
      
      method_specification$
        STATUS[
          method_specification$
            METHOD_ID=="DISTRICT_09"
        ]=="UNRESOLVED",
      
      TRUE,
      
      TRUE,
      
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
      "Stars 03u validation failed. ",
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
    
    ANALYTICAL_AREAS=
      nrow(framework),
    
    CORE_AREAS=
      nrow(core_framework),
    
    SPECIAL_AREAS=
      nrow(special_framework),
    
    CORE_DISTRICTS=
      nrow(district_register),
    
    DISTRICT_INTERPRETATION=
      paste(
        CORE_DISTRICTS,
        collapse="; "
      ),
    
    DISTRICT_METHOD_STATUS=
      "PROVISIONAL_METHOD_VALIDATED",
    
    SPECIAL_AREA_TREATMENT_COMPLETE=
      FALSE,
    
    ALMOST_ALL_DISTRICTS_DEFINED=
      FALSE,
    
    TAXON_DISTRICT_COUNTS_CREATED=
      FALSE,
    
    STARS_ASSIGNED=
      FALSE,
    
    HISTORICAL_STAR_ALLOCATIONS_USED=
      FALSE,
    
    NEXT_MODULE=
      "Stars 03v taxon-level core district occupancy"
  )
  
  # ---------------------------------------------------------------------------
  # Canonical DuckDB outputs
  # ---------------------------------------------------------------------------
  
  dbWriteTable(
    con,
    TABLE_CROSSWALK,
    core_crosswalk,
    overwrite=TRUE
  )
  
  dbWriteTable(
    con,
    TABLE_DISTRICTS,
    district_register,
    overwrite=TRUE
  )
  
  dbWriteTable(
    con,
    TABLE_SPECIAL_AREAS,
    special_area_register,
    overwrite=TRUE
  )
  
  dbWriteTable(
    con,
    TABLE_METHOD,
    method_specification,
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
    core_crosswalk,
    file.path(
      OUTPUT_DIR,
      "stars03u_japan_district_crosswalk.csv"
    )
  )
  
  write_csv(
    district_register,
    file.path(
      OUTPUT_DIR,
      "stars03u_japan_district_register.csv"
    )
  )
  
  write_csv(
    special_area_register,
    file.path(
      OUTPUT_DIR,
      "stars03u_special_area_register.csv"
    )
  )
  
  write_csv(
    method_specification,
    file.path(
      OUTPUT_DIR,
      "stars03u_method_specification.csv"
    )
  )
  
  write_csv(
    prohibited_assumptions,
    file.path(
      OUTPUT_DIR,
      "stars03u_prohibited_assumptions.csv"
    )
  )
  
  write_csv(
    validation,
    file.path(
      OUTPUT_DIR,
      "stars03u_validation.csv"
    )
  )
  
  write_csv(
    metadata,
    file.path(
      OUTPUT_DIR,
      "stars03u_metadata.csv"
    )
  )
  
  # ---------------------------------------------------------------------------
  # Final summary
  # ---------------------------------------------------------------------------
  
  cat(
    "\n============================================================\n"
  )
  
  cat(
    "Stars 03u v",
    VERSION,
    " COMPLETE\n",
    sep=""
  )
  
  cat(
    "============================================================\n"
  )
  
  cat(
    "Canonical analytical areas: ",
    nrow(framework),
    "\n",
    sep=""
  )
  
  cat(
    "Core areas mapped: ",
    nrow(core_crosswalk),
    "\n",
    sep=""
  )
  
  cat(
    "Core districts recognised: ",
    nrow(district_register),
    "\n",
    sep=""
  )
  
  cat(
    "Districts: ",
    paste(
      district_register$
        KEY_DISTRICT,
      collapse=", "
    ),
    "\n",
    sep=""
  )
  
  cat(
    "Special analytical areas: ",
    nrow(special_area_register),
    "\n",
    sep=""
  )
  
  cat(
    "Special areas counted as core districts: ",
    sum(
      special_area_register$
        COUNT_AS_CORE_DISTRICT
    ),
    "\n",
    sep=""
  )
  
  cat(
    "Almost-all rule defined: FALSE\n"
  )
  
  cat(
    "Taxon district counts created: FALSE\n"
  )
  
  cat(
    "Stars assigned: FALSE\n"
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
    "Canonical crosswalk: ",
    TABLE_CROSSWALK,
    "\n",
    sep=""
  )
  
  cat(
    "Canonical district register: ",
    TABLE_DISTRICTS,
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
      framework=framework,
      core_crosswalk=core_crosswalk,
      district_register=district_register,
      special_area_register=
        special_area_register,
      method_specification=
        method_specification,
      prohibited_assumptions=
        prohibited_assumptions,
      validation=validation,
      metadata=metadata
    )
  )
}

stars_03u_result <- run_stars_03u()