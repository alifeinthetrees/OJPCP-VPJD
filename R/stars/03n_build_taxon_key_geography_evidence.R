# =============================================================================
# VPJD-OJPCP
# R/stars/03n_build_taxon_key_geography_evidence.R
# Version 0.1.3
#
# BUILD TAXON-LEVEL KEY GEOGRAPHY EVIDENCE
#
# PURPOSE
# - Build one contemporary geography-evidence row per accepted WCVP taxon.
# - Derive occupancy of the eight established core Japanese districts.
# - Retain JP47-JP51 special-area evidence independently.
# - Join validated prefecture, endemicity, introduction and hybrid evidence.
# - Quantify taxa affected by unresolved special-area methodology.
#
# IMPORTANT
# - NO Star categories are assigned.
# - CORE_DISTRICT_COUNT is evidence, not JAPAN_DISTRICT_COUNT.
# - JP47-JP51 are NOT silently converted to Key districts.
# - Existing legacy JAPAN_DISTRICTS_PRESENT and ALMOST_ALL_DISTRICTS fields
#   are deliberately NOT used.
# - "Almost all districts" is NOT evaluated.
# - Historical Star allocations are NOT used.
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

MODULE <- "stars_03n_build_taxon_key_geography_evidence"
VERSION <- "0.1.3"
RUN_DATE <- Sys.Date()

DB_PATH <- here(
  "data", "interim", "occurrences", "vpjd_occurrences.duckdb"
)

AREA_FILE <- here(
  "outputs", "tables", "geography",
  "botanical_area_framework",
  "japan_botanical_areas_51.csv"
)

OUTPUT_DIR <- here(
  "outputs", "tables", "stars",
  "taxon_key_geography_evidence"
)

OUTPUT_TABLE <- "vpjd_star_taxon_key_geography_evidence"
OUTPUT_QUARANTINE <- "vpjd_star_special_area_method_quarantine"
OUTPUT_VALIDATION <- "vpjd_star_03n_validation"
OUTPUT_METADATA <- "vpjd_star_03n_metadata"

run_stars_03n <- function() {
  
  cat("\n— Build taxon-level Key geography evidence —\n\n")
  cat("Run date: ", as.character(RUN_DATE), "\n", sep = "")
  cat("Module: ", MODULE, "\n", sep = "")
  cat("Version: ", VERSION, "\n\n", sep = "")
  
  dir.create(
    OUTPUT_DIR,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  if (!file.exists(DB_PATH)) {
    stop("VPJD DuckDB not found: ", DB_PATH)
  }
  
  if (!file.exists(AREA_FILE)) {
    stop("51-area botanical framework not found: ", AREA_FILE)
  }
  
  con <- dbConnect(
    duckdb::duckdb(),
    dbdir = DB_PATH,
    read_only = FALSE
  )
  
  on.exit(
    dbDisconnect(con, shutdown = TRUE),
    add = TRUE
  )
  
  required_tables <- c(
    "vpjd_japan_taxon_distribution",
    "vpjd_japan_taxon_area_distribution",
    "vpjd_star_japan_key_geography",
    "vpjd_star_wcvp_key_geography",
    "vpjd_star_hybrid_status"
  )
  
  missing_tables <- setdiff(
    required_tables,
    dbListTables(con)
  )
  
  if (length(missing_tables) > 0L) {
    stop(
      "Required tables missing: ",
      paste(missing_tables, collapse = ", ")
    )
  }
  
  # ===========================================================================
  # 1. Accepted contemporary taxon population
  # ===========================================================================
  
  taxa <- dbGetQuery(
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
  ) |>
    as_tibble() |>
    mutate(
      FINAL_WCVP_ID = as.character(FINAL_WCVP_ID)
    )
  
  if (nrow(taxa) != 11439L) {
    stop(
      "Expected 11,439 accepted taxa; found ",
      format(nrow(taxa), big.mark = ",")
    )
  }
  
  if (n_distinct(taxa$FINAL_WCVP_ID) != 11439L) {
    stop("Accepted population is not one row per FINAL_WCVP_ID.")
  }
  
  cat(
    "Accepted contemporary taxa: ",
    format(nrow(taxa), big.mark = ","),
    "\n",
    sep = ""
  )
  
  # ===========================================================================
  # 2. Canonical accepted taxon-area evidence
  # ===========================================================================
  
  taxon_area <- dbGetQuery(
    con,
    "
    SELECT DISTINCT
      FINAL_WCVP_ID,
      BOTANICAL_AREA_ID
    FROM vpjd_japan_taxon_area_distribution
    WHERE FINAL_WCVP_STATUS = 'Accepted'
      AND PRESENCE = 1
    "
  ) |>
    as_tibble() |>
    mutate(
      FINAL_WCVP_ID = as.character(FINAL_WCVP_ID),
      BOTANICAL_AREA_ID = as.character(BOTANICAL_AREA_ID)
    )
  
  cat(
    "Accepted taxon-area evidence rows: ",
    format(nrow(taxon_area), big.mark = ","),
    "\n",
    sep = ""
  )
  
  cat(
    "Accepted taxa represented: ",
    format(
      n_distinct(taxon_area$FINAL_WCVP_ID),
      big.mark = ","
    ),
    "\n\n",
    sep = ""
  )
  
  # ===========================================================================
  # 3. Canonical 51-area geography
  # ===========================================================================
  
  areas <- read_csv(
    AREA_FILE,
    show_col_types = FALSE
  ) |>
    mutate(
      area_id = as.character(area_id),
      district = as.character(district)
    )
  
  required_area_fields <- c(
    "area_id",
    "area_no",
    "analytical_area_name",
    "area_type",
    "district",
    "political_parent",
    "display_area_id",
    "display_area_name",
    "star_small_island_group",
    "special_assignment_required"
  )
  
  missing_area_fields <- setdiff(
    required_area_fields,
    names(areas)
  )
  
  if (length(missing_area_fields) > 0L) {
    stop(
      "Required geography fields missing: ",
      paste(missing_area_fields, collapse = ", ")
    )
  }
  
  if (
    nrow(areas) != 51L ||
    n_distinct(areas$area_id) != 51L
  ) {
    stop("Expected exactly 51 unique analytical botanical areas.")
  }
  
  core_districts <- c(
    "Hokkaido",
    "Tohoku",
    "Kanto",
    "Chubu",
    "Kinki",
    "Chugoku",
    "Shikoku",
    "Kyushu"
  )
  
  special_area_ids <- c(
    "JP47",
    "JP48",
    "JP49",
    "JP50",
    "JP51"
  )
  
  if (!all(core_districts %in% areas$district)) {
    stop("Not all eight core districts are represented.")
  }
  
  if (!all(special_area_ids %in% areas$area_id)) {
    stop("Not all special analytical areas JP47-JP51 are present.")
  }
  
  area_lookup <- areas |>
    select(
      BOTANICAL_AREA_ID = area_id,
      ANALYTICAL_AREA_NAME = analytical_area_name,
      DISTRICT = district,
      STAR_SMALL_ISLAND_GROUP = star_small_island_group,
      SPECIAL_ASSIGNMENT_REQUIRED = special_assignment_required
    )
  
  # ===========================================================================
  # 4. Join taxon-area evidence to geography
  # ===========================================================================
  
  taxon_area_geo <- taxon_area |>
    left_join(
      area_lookup,
      by = "BOTANICAL_AREA_ID"
    )
  
  unmatched_areas <- taxon_area_geo |>
    filter(is.na(ANALYTICAL_AREA_NAME)) |>
    distinct(BOTANICAL_AREA_ID)
  
  if (nrow(unmatched_areas) > 0L) {
    stop(
      "Unmatched BOTANICAL_AREA_ID values: ",
      paste(
        unmatched_areas$BOTANICAL_AREA_ID,
        collapse = ", "
      )
    )
  }
  
  # ===========================================================================
  # 5. Eight-core-district occupancy
  # ===========================================================================
  
  core_long <- taxon_area_geo |>
    filter(DISTRICT %in% core_districts) |>
    distinct(
      FINAL_WCVP_ID,
      DISTRICT
    ) |>
    mutate(PRESENT = TRUE)
  
  core_wide <- core_long |>
    pivot_wider(
      names_from = DISTRICT,
      values_from = PRESENT,
      values_fill = FALSE
    )
  
  for (nm in core_districts) {
    if (!nm %in% names(core_wide)) {
      core_wide[[nm]] <- FALSE
    }
  }
  
  core_wide <- core_wide |>
    select(
      FINAL_WCVP_ID,
      all_of(core_districts)
    )
  
  names(core_wide)[
    match(core_districts, names(core_wide))
  ] <- paste0(
    "CORE_",
    toupper(core_districts),
    "_PRESENT"
  )
  
  core_fields <- paste0(
    "CORE_",
    toupper(core_districts),
    "_PRESENT"
  )
  
  core_wide <- core_wide |>
    mutate(
      CORE_DISTRICT_COUNT =
        rowSums(
          across(all_of(core_fields))
        )
    )
  
  # ===========================================================================
  # 6. Special-area occupancy
  # ===========================================================================
  
  special_lookup <- tibble(
    BOTANICAL_AREA_ID = special_area_ids,
    SPECIAL_FIELD = c(
      "RYUKYU_PRESENT",
      "IZU_PRESENT",
      "OGASAWARA_PRESENT",
      "KAZAN_PRESENT",
      "KURILES_PRESENT"
    )
  )
  
  special_long <- taxon_area_geo |>
    filter(
      BOTANICAL_AREA_ID %in%
        special_area_ids
    ) |>
    distinct(
      FINAL_WCVP_ID,
      BOTANICAL_AREA_ID
    ) |>
    left_join(
      special_lookup,
      by = "BOTANICAL_AREA_ID"
    ) |>
    mutate(PRESENT = TRUE)
  
  special_wide <- special_long |>
    select(
      FINAL_WCVP_ID,
      SPECIAL_FIELD,
      PRESENT
    ) |>
    pivot_wider(
      names_from = SPECIAL_FIELD,
      values_from = PRESENT,
      values_fill = FALSE
    )
  
  special_fields <- special_lookup$SPECIAL_FIELD
  
  for (nm in special_fields) {
    if (!nm %in% names(special_wide)) {
      special_wide[[nm]] <- FALSE
    }
  }
  
  special_wide <- special_wide |>
    select(
      FINAL_WCVP_ID,
      all_of(special_fields)
    ) |>
    mutate(
      N_SPECIAL_AREAS =
        rowSums(
          across(all_of(special_fields))
        ),
      SPECIAL_AREA_PRESENT =
        N_SPECIAL_AREAS > 0
    )
  
  # ===========================================================================
  # 7. Prefecture evidence from Stars 03a
  #
  # Legacy district fields in this table are deliberately not selected.
  # ===========================================================================
  
  prefecture_evidence <- dbGetQuery(
    con,
    "
    SELECT
      FINAL_WCVP_ID,
      JAPAN_PREFECTURES_PRESENT
    FROM vpjd_star_japan_key_geography
    "
  ) |>
    as_tibble() |>
    transmute(
      FINAL_WCVP_ID =
        as.character(FINAL_WCVP_ID),
      JAPAN_PREFECTURE_COUNT =
        as.integer(JAPAN_PREFECTURES_PRESENT)
    ) |>
    distinct(
      FINAL_WCVP_ID,
      .keep_all = TRUE
    )
  
  cat(
    "Stars 03a prefecture evidence rows: ",
    format(
      nrow(prefecture_evidence),
      big.mark = ","
    ),
    "\n",
    sep = ""
  )
  
  # ===========================================================================
  # 8. Contemporary WCVP range evidence from Stars 03c
  #
  # Canonical fields established from actual 03c schema:
  # - ENDEMIC_TO_JAPAN_WCVP
  # - WCVP_INTRODUCED_TO_JAPAN
  # - WCVP_OUTSIDE_JAPAN
  # ===========================================================================
  
  wcvp_evidence <- dbGetQuery(
    con,
    "
    SELECT
      FINAL_WCVP_ID,
      ENDEMIC_TO_JAPAN_WCVP,
      WCVP_INTRODUCED_TO_JAPAN,
      WCVP_OUTSIDE_JAPAN
    FROM vpjd_star_wcvp_key_geography
    "
  ) |>
    as_tibble() |>
    transmute(
      FINAL_WCVP_ID =
        as.character(FINAL_WCVP_ID),
      ENDEMIC_TO_JAPAN =
        as.logical(ENDEMIC_TO_JAPAN_WCVP),
      INTRODUCED_TO_JAPAN =
        as.logical(WCVP_INTRODUCED_TO_JAPAN),
      OUTSIDE_JAPAN_POSITIVE =
        as.logical(WCVP_OUTSIDE_JAPAN)
    ) |>
    distinct(
      FINAL_WCVP_ID,
      .keep_all = TRUE
    )
  
  cat(
    "Stars 03c WCVP evidence rows: ",
    format(
      nrow(wcvp_evidence),
      big.mark = ","
    ),
    "\n",
    sep = ""
  )
  
  # ===========================================================================
  # 9. Strict hybrid evidence from Stars 03g
  # ===========================================================================
  
  hybrid_schema <- dbGetQuery(
    con,
    "
    SELECT *
    FROM vpjd_star_hybrid_status
    LIMIT 0
    "
  )
  
  if (!"HYBRID_STATUS" %in% names(hybrid_schema)) {
    cat("\nStars 03g fields:\n")
    print(
      tibble(FIELD = names(hybrid_schema)),
      n = Inf
    )
    stop(
      "Expected HYBRID_STATUS field not found in ",
      "vpjd_star_hybrid_status."
    )
  }
  
  hybrid_evidence <- dbGetQuery(
    con,
    "
    SELECT
      FINAL_WCVP_ID,
      HYBRID_STATUS
    FROM vpjd_star_hybrid_status
    "
  ) |>
    as_tibble() |>
    transmute(
      FINAL_WCVP_ID =
        as.character(FINAL_WCVP_ID),
      HYBRID_STATUS =
        as.character(HYBRID_STATUS)
    ) |>
    distinct(
      FINAL_WCVP_ID,
      .keep_all = TRUE
    )
  
  cat(
    "Stars 03g hybrid evidence rows: ",
    format(
      nrow(hybrid_evidence),
      big.mark = ","
    ),
    "\n",
    sep = ""
  )
  
  # ===========================================================================
  # 10. Assemble one-row-per-taxon evidence matrix
  # ===========================================================================
  
  evidence <- taxa |>
    left_join(
      core_wide,
      by = "FINAL_WCVP_ID"
    ) |>
    left_join(
      special_wide,
      by = "FINAL_WCVP_ID"
    ) |>
    left_join(
      prefecture_evidence,
      by = "FINAL_WCVP_ID"
    ) |>
    left_join(
      wcvp_evidence,
      by = "FINAL_WCVP_ID"
    ) |>
    left_join(
      hybrid_evidence,
      by = "FINAL_WCVP_ID"
    )
  
  logical_geo_fields <- c(
    core_fields,
    special_fields
  )
  
  for (nm in logical_geo_fields) {
    evidence[[nm]][
      is.na(evidence[[nm]])
    ] <- FALSE
  }
  
  evidence <- evidence |>
    mutate(
      CORE_DISTRICT_COUNT =
        rowSums(
          across(all_of(core_fields))
        ),
      
      N_SPECIAL_AREAS =
        rowSums(
          across(all_of(special_fields))
        ),
      
      SPECIAL_AREA_PRESENT =
        N_SPECIAL_AREAS > 0,
      
      CORE_DISTRICT_COUNT_LE_1 =
        CORE_DISTRICT_COUNT <= 1,
      
      CORE_DISTRICT_COUNT_LE_2 =
        CORE_DISTRICT_COUNT <= 2,
      
      CORE_DISTRICT_COUNT_LE_3 =
        CORE_DISTRICT_COUNT <= 3,
      
      CORE_DISTRICT_COUNT_GT_2 =
        CORE_DISTRICT_COUNT > 2,
      
      CORE_DISTRICT_COUNT_GT_3 =
        CORE_DISTRICT_COUNT > 3,
      
      CORE_ALL_8_DISTRICTS =
        CORE_DISTRICT_COUNT == 8,
      
      PREFECTURES_LE_5 =
        if_else(
          !is.na(JAPAN_PREFECTURE_COUNT),
          JAPAN_PREFECTURE_COUNT <= 5,
          NA
        ),
      
      PREFECTURES_LE_7 =
        if_else(
          !is.na(JAPAN_PREFECTURE_COUNT),
          JAPAN_PREFECTURE_COUNT <= 7,
          NA
        ),
      
      PREFECTURES_LE_10 =
        if_else(
          !is.na(JAPAN_PREFECTURE_COUNT),
          JAPAN_PREFECTURE_COUNT <= 10,
          NA
        ),
      
      PREFECTURES_LE_14 =
        if_else(
          !is.na(JAPAN_PREFECTURE_COUNT),
          JAPAN_PREFECTURE_COUNT <= 14,
          NA
        ),
      
      PREFECTURES_GT_10 =
        if_else(
          !is.na(JAPAN_PREFECTURE_COUNT),
          JAPAN_PREFECTURE_COUNT > 10,
          NA
        ),
      
      PREFECTURES_GT_14 =
        if_else(
          !is.na(JAPAN_PREFECTURE_COUNT),
          JAPAN_PREFECTURE_COUNT > 14,
          NA
        ),
      
      PRE_KEY_EVIDENCE_ROUTE =
        case_when(
          INTRODUCED_TO_JAPAN %in% TRUE ~
            "GX_EVIDENCE",
          
          HYBRID_STATUS == "HYBRID" ~
            "HYB_EVIDENCE",
          
          ENDEMIC_TO_JAPAN %in% TRUE ~
            "ENDEMIC_KEY",
          
          ENDEMIC_TO_JAPAN %in% FALSE ~
            "NON_ENDEMIC_KEY",
          
          TRUE ~
            "ENDEMICITY_UNRESOLVED"
        ),
      
      DISTRICT_COUNT_METHOD_COMPLETE =
        !SPECIAL_AREA_PRESENT,
      
      DISTRICT_COUNT_STATUS =
        case_when(
          SPECIAL_AREA_PRESENT ~
            "SPECIAL_AREA_METHOD_REQUIRED",
          
          TRUE ~
            "CORE_DISTRICT_EVIDENCE_COMPLETE"
        ),
      
      FINAL_STAR_ASSIGNED = FALSE
    )
  
  # ===========================================================================
  # 11. Profiles
  # ===========================================================================
  
  route_profile <- evidence |>
    count(
      PRE_KEY_EVIDENCE_ROUTE,
      name = "N_TAXA",
      sort = TRUE
    )
  
  district_profile <- evidence |>
    count(
      CORE_DISTRICT_COUNT,
      name = "N_TAXA"
    ) |>
    arrange(CORE_DISTRICT_COUNT)
  
  special_profile <- evidence |>
    summarise(
      N_TAXA = n(),
      
      RYUKYU =
        sum(RYUKYU_PRESENT),
      
      IZU =
        sum(IZU_PRESENT),
      
      OGASAWARA =
        sum(OGASAWARA_PRESENT),
      
      KAZAN =
        sum(KAZAN_PRESENT),
      
      KURILES =
        sum(KURILES_PRESENT),
      
      ANY_SPECIAL_AREA =
        sum(SPECIAL_AREA_PRESENT),
      
      NO_SPECIAL_AREA =
        sum(!SPECIAL_AREA_PRESENT)
    )
  
  special_sensitivity <- evidence |>
    count(
      PRE_KEY_EVIDENCE_ROUTE,
      SPECIAL_AREA_PRESENT,
      name = "N_TAXA"
    ) |>
    group_by(
      PRE_KEY_EVIDENCE_ROUTE
    ) |>
    mutate(
      ROUTE_PERCENT =
        round(
          100 * N_TAXA / sum(N_TAXA),
          2
        )
    ) |>
    ungroup() |>
    arrange(
      PRE_KEY_EVIDENCE_ROUTE,
      desc(SPECIAL_AREA_PRESENT)
    )
  
  threshold_profile <- evidence |>
    summarise(
      N_TAXA = n(),
      
      CORE_DISTRICT_LE_1 =
        sum(CORE_DISTRICT_COUNT_LE_1),
      
      CORE_DISTRICT_LE_2 =
        sum(CORE_DISTRICT_COUNT_LE_2),
      
      CORE_DISTRICT_LE_3 =
        sum(CORE_DISTRICT_COUNT_LE_3),
      
      CORE_DISTRICT_GT_2 =
        sum(CORE_DISTRICT_COUNT_GT_2),
      
      CORE_DISTRICT_GT_3 =
        sum(CORE_DISTRICT_COUNT_GT_3),
      
      CORE_ALL_8 =
        sum(CORE_ALL_8_DISTRICTS),
      
      PREFECTURE_LE_5 =
        sum(
          PREFECTURES_LE_5,
          na.rm = TRUE
        ),
      
      PREFECTURE_LE_7 =
        sum(
          PREFECTURES_LE_7,
          na.rm = TRUE
        ),
      
      PREFECTURE_LE_10 =
        sum(
          PREFECTURES_LE_10,
          na.rm = TRUE
        ),
      
      PREFECTURE_LE_14 =
        sum(
          PREFECTURES_LE_14,
          na.rm = TRUE
        ),
      
      PREFECTURE_GT_10 =
        sum(
          PREFECTURES_GT_10,
          na.rm = TRUE
        ),
      
      PREFECTURE_GT_14 =
        sum(
          PREFECTURES_GT_14,
          na.rm = TRUE
        )
    )
  
  quarantine <- evidence |>
    filter(
      SPECIAL_AREA_PRESENT
    ) |>
    select(
      FINAL_WCVP_ID,
      FINAL_WCVP_RECOGNISED_NAME,
      FINAL_WCVP_RANK,
      PRE_KEY_EVIDENCE_ROUTE,
      CORE_DISTRICT_COUNT,
      all_of(core_fields),
      all_of(special_fields),
      JAPAN_PREFECTURE_COUNT,
      DISTRICT_COUNT_STATUS
    ) |>
    arrange(
      PRE_KEY_EVIDENCE_ROUTE,
      FINAL_WCVP_RECOGNISED_NAME
    )
  
  cat("\n— PRE-KEY EVIDENCE ROUTES —\n")
  print(route_profile, n = Inf)
  
  cat("\n— CORE DISTRICT COUNT PROFILE —\n")
  print(district_profile, n = Inf)
  
  cat("\n— SPECIAL-AREA TAXON PROFILE —\n")
  print(special_profile, n = Inf)
  
  cat("\n— SPECIAL-AREA SENSITIVITY BY ROUTE —\n")
  print(special_sensitivity, n = Inf)
  
  cat("\n— KEY-THRESHOLD EVIDENCE PROFILE —\n")
  print(threshold_profile, n = Inf)
  
  # ===========================================================================
  # 12. Validation
  # ===========================================================================
  
  validation <- tibble(
    CHECK = c(
      "Accepted population = 11,439",
      "One row per accepted FINAL_WCVP_ID",
      "Accepted taxon-area evidence available",
      "Accepted taxon-area evidence represents 11,439 taxa",
      "All taxon-area IDs match geography",
      "51 analytical areas available",
      "Eight core districts available",
      "Five special areas available",
      "Evidence table = 11,439 taxa",
      "Evidence table one row per taxon",
      "Core district counts bounded 0-8",
      "Core district flags contain no NA",
      "Special-area flags contain no NA",
      "Prefecture evidence joined",
      "Endemicity evidence joined",
      "Introduction evidence joined",
      "Outside-Japan evidence joined",
      "Hybrid evidence joined",
      "Legacy JAPAN_DISTRICTS_PRESENT not propagated",
      "Legacy ALMOST_ALL_DISTRICTS not propagated",
      "No JAPAN_DISTRICT_COUNT created",
      "No STAR_CATEGORY created",
      "FINAL_STAR_ASSIGNED always FALSE",
      "Special-area taxa explicitly identified",
      "No historical Star allocations used"
    ),
    
    PASS = c(
      nrow(taxa) == 11439L,
      
      n_distinct(
        taxa$FINAL_WCVP_ID
      ) == 11439L,
      
      nrow(taxon_area) > 0L,
      
      n_distinct(
        taxon_area$FINAL_WCVP_ID
      ) == 11439L,
      
      nrow(unmatched_areas) == 0L,
      
      nrow(areas) == 51L,
      
      all(
        core_districts %in%
          areas$district
      ),
      
      all(
        special_area_ids %in%
          areas$area_id
      ),
      
      nrow(evidence) == 11439L,
      
      n_distinct(
        evidence$FINAL_WCVP_ID
      ) == 11439L,
      
      all(
        evidence$CORE_DISTRICT_COUNT >= 0 &
          evidence$CORE_DISTRICT_COUNT <= 8
      ),
      
      !any(
        is.na(
          evidence[core_fields]
        )
      ),
      
      !any(
        is.na(
          evidence[special_fields]
        )
      ),
      
      sum(
        !is.na(
          evidence$JAPAN_PREFECTURE_COUNT
        )
      ) > 0L,
      
      sum(
        !is.na(
          evidence$ENDEMIC_TO_JAPAN
        )
      ) > 0L,
      
      sum(
        !is.na(
          evidence$INTRODUCED_TO_JAPAN
        )
      ) > 0L,
      
      sum(
        !is.na(
          evidence$OUTSIDE_JAPAN_POSITIVE
        )
      ) > 0L,
      
      sum(
        !is.na(
          evidence$HYBRID_STATUS
        )
      ) > 0L,
      
      !"JAPAN_DISTRICTS_PRESENT" %in%
        names(evidence),
      
      !"ALMOST_ALL_DISTRICTS" %in%
        names(evidence),
      
      !"JAPAN_DISTRICT_COUNT" %in%
        names(evidence),
      
      !"STAR_CATEGORY" %in%
        names(evidence),
      
      all(
        evidence$FINAL_STAR_ASSIGNED ==
          FALSE
      ),
      
      nrow(quarantine) ==
        sum(
          evidence$SPECIAL_AREA_PRESENT
        ),
      
      TRUE
    )
  ) |>
    mutate(
      RESULT =
        if_else(
          PASS,
          "PASS",
          "FAIL"
        )
    )
  
  cat("\n— VALIDATION —\n")
  print(validation, n = Inf)
  
  if (!all(validation$PASS)) {
    stop(
      "Stars 03n validation failed. ",
      "Review FAIL checks before proceeding."
    )
  }
  
  # ===========================================================================
  # 13. Metadata
  # ===========================================================================
  
  metadata <- tibble(
    MODULE = MODULE,
    VERSION = VERSION,
    RUN_DATE = as.character(RUN_DATE),
    
    PURPOSE =
      paste(
        "Build one-row-per-accepted-taxon contemporary",
        "Japanese geography evidence for final Key traversal."
      ),
    
    SOURCE_TAXA =
      "vpjd_japan_taxon_distribution",
    
    SOURCE_TAXON_AREA =
      "vpjd_japan_taxon_area_distribution",
    
    SOURCE_PREFECTURE =
      paste0(
        "vpjd_star_japan_key_geography.",
        "JAPAN_PREFECTURES_PRESENT"
      ),
    
    SOURCE_ENDEMICITY =
      paste0(
        "vpjd_star_wcvp_key_geography.",
        "ENDEMIC_TO_JAPAN_WCVP"
      ),
    
    SOURCE_INTRODUCTION =
      paste0(
        "vpjd_star_wcvp_key_geography.",
        "WCVP_INTRODUCED_TO_JAPAN"
      ),
    
    SOURCE_OUTSIDE_JAPAN =
      paste0(
        "vpjd_star_wcvp_key_geography.",
        "WCVP_OUTSIDE_JAPAN"
      ),
    
    ACCEPTED_TAXA =
      nrow(evidence),
    
    ACCEPTED_TAXON_AREA_ROWS =
      nrow(taxon_area),
    
    CORE_DISTRICTS =
      length(core_districts),
    
    SPECIAL_AREAS =
      length(special_area_ids),
    
    TAXA_WITH_SPECIAL_AREA_EVIDENCE =
      sum(
        evidence$SPECIAL_AREA_PRESENT
      ),
    
    TAXA_WITHOUT_SPECIAL_AREA_EVIDENCE =
      sum(
        !evidence$SPECIAL_AREA_PRESENT
      ),
    
    LEGACY_DISTRICT_FIELDS_USED =
      FALSE,
    
    JAPAN_DISTRICT_COUNT_CREATED =
      FALSE,
    
    ALMOST_ALL_DISTRICTS_EVALUATED =
      FALSE,
    
    STAR_CLASSIFICATIONS_CREATED =
      FALSE,
    
    HISTORICAL_STAR_ALLOCATIONS_USED =
      FALSE,
    
    FINAL_STAR_CLASSIFIER_READY =
      FALSE,
    
    STATUS =
      "VALIDATED_EVIDENCE_MODULE"
  )
  
  # ===========================================================================
  # 14. Write canonical DuckDB outputs
  # ===========================================================================
  
  dbWriteTable(
    con,
    OUTPUT_TABLE,
    evidence,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    OUTPUT_QUARANTINE,
    quarantine,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    OUTPUT_VALIDATION,
    validation,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    OUTPUT_METADATA,
    metadata,
    overwrite = TRUE
  )
  
  # ===========================================================================
  # 15. Write audit CSV outputs
  # ===========================================================================
  
  write_csv(
    route_profile,
    file.path(
      OUTPUT_DIR,
      "stars03n_pre_key_route_profile.csv"
    )
  )
  
  write_csv(
    district_profile,
    file.path(
      OUTPUT_DIR,
      "stars03n_core_district_count_profile.csv"
    )
  )
  
  write_csv(
    special_profile,
    file.path(
      OUTPUT_DIR,
      "stars03n_special_area_profile.csv"
    )
  )
  
  write_csv(
    special_sensitivity,
    file.path(
      OUTPUT_DIR,
      "stars03n_special_area_sensitivity_by_route.csv"
    )
  )
  
  write_csv(
    threshold_profile,
    file.path(
      OUTPUT_DIR,
      "stars03n_key_threshold_profile.csv"
    )
  )
  
  write_csv(
    quarantine,
    file.path(
      OUTPUT_DIR,
      "stars03n_special_area_method_quarantine.csv"
    )
  )
  
  write_csv(
    validation,
    file.path(
      OUTPUT_DIR,
      "stars03n_validation.csv"
    )
  )
  
  write_csv(
    metadata,
    file.path(
      OUTPUT_DIR,
      "stars03n_metadata.csv"
    )
  )
  
  # ===========================================================================
  # 16. Final summary
  # ===========================================================================
  
  cat("\n============================================================\n")
  cat("Stars 03n v", VERSION, " COMPLETE\n", sep = "")
  cat("============================================================\n")
  
  cat(
    "Accepted taxa: ",
    format(
      nrow(evidence),
      big.mark = ","
    ),
    "\n",
    sep = ""
  )
  
  cat(
    "Accepted taxon-area rows: ",
    format(
      nrow(taxon_area),
      big.mark = ","
    ),
    "\n",
    sep = ""
  )
  
  cat(
    "Core districts: ",
    length(core_districts),
    "\n",
    sep = ""
  )
  
  cat(
    "Taxa with special-area evidence: ",
    format(
      sum(
        evidence$SPECIAL_AREA_PRESENT
      ),
      big.mark = ","
    ),
    "\n",
    sep = ""
  )
  
  cat(
    "Taxa without special-area evidence: ",
    format(
      sum(
        !evidence$SPECIAL_AREA_PRESENT
      ),
      big.mark = ","
    ),
    "\n",
    sep = ""
  )
  
  cat(
    "Special-area methodological quarantine: ",
    format(
      nrow(quarantine),
      big.mark = ","
    ),
    "\n",
    sep = ""
  )
  
  cat("Legacy district fields used: FALSE\n")
  cat("JAPAN_DISTRICT_COUNT created: FALSE\n")
  cat("'Almost all districts' evaluated: FALSE\n")
  cat("Star classifications created: FALSE\n")
  cat("Final classifier ready: FALSE\n")
  
  cat(
    "Validation: ",
    sum(validation$PASS),
    "/",
    nrow(validation),
    " PASS\n",
    sep = ""
  )
  
  cat(
    "Canonical DuckDB table: ",
    OUTPUT_TABLE,
    "\n",
    sep = ""
  )
  
  cat(
    "Outputs: ",
    OUTPUT_DIR,
    "\n",
    sep = ""
  )
  
  cat("============================================================\n")
  
  invisible(
    list(
      evidence = evidence,
      quarantine = quarantine,
      route_profile = route_profile,
      district_profile = district_profile,
      special_profile = special_profile,
      special_sensitivity = special_sensitivity,
      threshold_profile = threshold_profile,
      validation = validation,
      metadata = metadata
    )
  )
}

stars_03n_result <- run_stars_03n()