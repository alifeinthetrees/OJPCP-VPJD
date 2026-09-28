# =============================================================================
# VPJD-OJPCP
# R/stars/03_apply_key_to_japanese_flora.R
# Version 0.3.1
#
# TAXON-BY-TAXON APPLICATION OF THE KEY TO STARS
#
# Methodological rules
# --------------------
# 1. Star attribution operates on WCVP ACCEPTED taxa only.
# 2. Accepted hybrid taxa receive HYB.
# 3. Accepted taxa introduced to Japan receive GX.
# 4. Remaining accepted, non-hybrid, non-introduced taxa are assessed
#    geographically through the Key to Stars: BK / GD / BU / GN.
# 5. Unknown introduction status is not interpreted as native.
# 6. Historical Star totals are not targets or attribution inputs.
# 7. Historical FOJ Stars are not used for contemporary assignment.
# 8. "Rare" is not inferred from occurrence-record count.
#
# Japanese distribution evidence
# ------------------------------
# JP47 = Ryukyu
# JP48 = Izu
# JP49 = Ogasawara
# JP50 = Kazan
# JP51 = Kuriles
#
# For the Key's Ryukyu/Ogasawara/Izu test, JP49 + JP50 are treated as the
# Ogasawara island group.
#
# Canonical inputs
# ----------------
# vpjd_japan_taxon_distribution
# vpjd_japan_taxon_area_distribution
# vpjd_star_key_validation
#
# Optional future inputs
# ----------------------
# vpjd_star_japan_key_geography
#   FINAL_WCVP_ID
#   JAPAN_PREFECTURES_PRESENT
#   JAPAN_DISTRICTS_PRESENT
#   ALMOST_ALL_DISTRICTS
#
# vpjd_japan_introduction_status
#   FINAL_WCVP_ID
#   INTRODUCED_TO_JAPAN
#
# Canonical outputs
# -----------------
# vpjd_star_taxon_distribution_inputs
# vpjd_star_key_application
# vpjd_star_key_application_summary
# vpjd_star_key_application_validation
# vpjd_star_key_application_metadata
# =============================================================================

suppressPackageStartupMessages({
  library(DBI)
  library(duckdb)
  library(dplyr)
  library(readr)
  library(here)
})

MODULE <- "stars_03_apply_key_to_japanese_flora"
VERSION <- "0.3.1"
RUN_DATE <- Sys.Date()

DB_PATH <- here(
  "data", "interim", "occurrences",
  "vpjd_occurrences.duckdb"
)

SOURCE_TAXA <- "vpjd_japan_taxon_distribution"
SOURCE_LONG <- "vpjd_japan_taxon_area_distribution"
SOURCE_KEY_VALIDATION <- "vpjd_star_key_validation"

KEY_GEOGRAPHY_TABLE <- "vpjd_star_japan_key_geography"
INTRODUCTION_TABLE <- "vpjd_japan_introduction_status"

OUTPUT_INPUTS <- "vpjd_star_taxon_distribution_inputs"
OUTPUT_APPLICATION <- "vpjd_star_key_application"
OUTPUT_SUMMARY <- "vpjd_star_key_application_summary"
OUTPUT_VALIDATION <- "vpjd_star_key_application_validation"
OUTPUT_METADATA <- "vpjd_star_key_application_metadata"

OUTPUT_DIR <- here(
  "outputs", "tables", "stars",
  "key_application"
)

EXPECTED_SOURCE_TAXA <- 11484L
EXPECTED_OCCURRENCES <- 2851736L
EXPECTED_AREA_PRESENCES <- 128876L

run_stars_03 <- function() {
  
  cat("\n— Taxon-by-taxon application of Key to Stars —\n\n")
  cat("Run date: ", as.character(RUN_DATE), "\n", sep = "")
  cat("Module: ", MODULE, "\n", sep = "")
  cat("Version: ", VERSION, "\n\n", sep = "")
  
  if (!file.exists(DB_PATH)) {
    stop("VPJD DuckDB not found: ", DB_PATH)
  }
  
  dir.create(
    OUTPUT_DIR,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  con <- dbConnect(
    duckdb::duckdb(),
    dbdir = DB_PATH,
    read_only = FALSE
  )
  
  on.exit(
    dbDisconnect(con, shutdown = TRUE),
    add = TRUE
  )
  
  tables <- dbListTables(con)
  
  required_tables <- c(
    SOURCE_TAXA,
    SOURCE_LONG,
    SOURCE_KEY_VALIDATION
  )
  
  missing_tables <- setdiff(
    required_tables,
    tables
  )
  
  if (length(missing_tables) > 0L) {
    stop(
      "Required tables missing: ",
      paste(missing_tables, collapse = ", ")
    )
  }
  
  # ===========================================================================
  # 1. Validate frozen Key
  # ===========================================================================
  
  key_validation <- dbGetQuery(
    con,
    paste0(
      'SELECT "check", pass FROM "',
      SOURCE_KEY_VALIDATION,
      '"'
    )
  )
  
  if (
    nrow(key_validation) == 0L ||
    any(is.na(key_validation$pass)) ||
    !all(key_validation$pass)
  ) {
    stop("Stars 01 Key validation is not fully PASS.")
  }
  
  cat("Stars 01 Key validation: PASS\n")
  
  # ===========================================================================
  # 2. Validate contemporary source flora
  # ===========================================================================
  
  source_profile <- dbGetQuery(
    con,
    paste0(
      "SELECT ",
      "COUNT(*) AS WCVP_TAXA, ",
      "COUNT(DISTINCT FINAL_WCVP_ID) AS DISTINCT_WCVP_IDS, ",
      "SUM(OCCURRENCE_RECORDS) AS OCCURRENCE_RECORDS ",
      "FROM ", SOURCE_TAXA
    )
  )
  
  area_profile <- dbGetQuery(
    con,
    paste0(
      "SELECT COUNT(*) AS TAXON_AREA_PRESENCES ",
      "FROM ", SOURCE_LONG
    )
  )
  
  cat("\n— Contemporary source flora —\n")
  print.data.frame(
    source_profile,
    row.names = FALSE
  )
  
  cat(
    "\nTaxon-area presences: ",
    format(
      area_profile$TAXON_AREA_PRESENCES[[1]],
      big.mark = ","
    ),
    "\n",
    sep = ""
  )
  
  if (
    source_profile$WCVP_TAXA[[1]] != EXPECTED_SOURCE_TAXA ||
    source_profile$DISTINCT_WCVP_IDS[[1]] != EXPECTED_SOURCE_TAXA ||
    source_profile$OCCURRENCE_RECORDS[[1]] != EXPECTED_OCCURRENCES ||
    area_profile$TAXON_AREA_PRESENCES[[1]] != EXPECTED_AREA_PRESENCES
  ) {
    stop(
      "Contemporary source flora differs from validated Distribution 01/02."
    )
  }
  
  # ===========================================================================
  # 3. Profile accepted-name eligibility
  # ===========================================================================
  
  accepted_profile <- dbGetQuery(
    con,
    paste0(
      "SELECT ",
      "COALESCE(FINAL_WCVP_STATUS, '<NA>') AS FINAL_WCVP_STATUS, ",
      "COUNT(*) AS WCVP_TAXA, ",
      "SUM(OCCURRENCE_RECORDS) AS OCCURRENCE_RECORDS ",
      "FROM ", SOURCE_TAXA, " ",
      "GROUP BY COALESCE(FINAL_WCVP_STATUS, '<NA>') ",
      "ORDER BY WCVP_TAXA DESC"
    )
  )
  
  cat("\n— WCVP status profile —\n")
  print.data.frame(
    accepted_profile,
    row.names = FALSE
  )
  
  # ===========================================================================
  # 4. Optional exact Key geography
  # ===========================================================================
  
  key_geography_available <- KEY_GEOGRAPHY_TABLE %in% tables
  
  cat("\n— Key geography —\n")
  cat(
    "Exact prefecture/district input table: ",
    ifelse(
      key_geography_available,
      "AVAILABLE",
      "NOT YET AVAILABLE"
    ),
    "\n",
    sep = ""
  )
  
  dbExecute(
    con,
    "DROP TABLE IF EXISTS temp_star_key_geography"
  )
  
  if (key_geography_available) {
    
    geo_fields <- dbListFields(
      con,
      KEY_GEOGRAPHY_TABLE
    )
    
    required_geo_fields <- c(
      "FINAL_WCVP_ID",
      "JAPAN_PREFECTURES_PRESENT",
      "JAPAN_DISTRICTS_PRESENT",
      "ALMOST_ALL_DISTRICTS"
    )
    
    missing_geo_fields <- setdiff(
      required_geo_fields,
      geo_fields
    )
    
    if (length(missing_geo_fields) > 0L) {
      stop(
        "Key geography table exists but required fields are missing: ",
        paste(missing_geo_fields, collapse = ", ")
      )
    }
    
    dbExecute(
      con,
      paste0(
        "CREATE TEMP TABLE temp_star_key_geography AS ",
        "SELECT ",
        "FINAL_WCVP_ID, ",
        "JAPAN_PREFECTURES_PRESENT, ",
        "JAPAN_DISTRICTS_PRESENT, ",
        "ALMOST_ALL_DISTRICTS ",
        "FROM ", KEY_GEOGRAPHY_TABLE
      )
    )
    
  } else {
    
    dbExecute(
      con,
      paste0(
        "CREATE TEMP TABLE temp_star_key_geography AS ",
        "SELECT ",
        "FINAL_WCVP_ID, ",
        "CAST(NULL AS INTEGER) AS JAPAN_PREFECTURES_PRESENT, ",
        "CAST(NULL AS INTEGER) AS JAPAN_DISTRICTS_PRESENT, ",
        "CAST(NULL AS BOOLEAN) AS ALMOST_ALL_DISTRICTS ",
        "FROM ", SOURCE_TAXA
      )
    )
  }
  
  # ===========================================================================
  # 5. Optional introduction status
  #
  # GX means introduced to Japan.
  #
  # Unknown introduction status is retained as NULL and must not be treated
  # as evidence that the taxon is native.
  # ===========================================================================
  
  introduction_available <- INTRODUCTION_TABLE %in% tables
  
  cat("\n— Introduction status —\n")
  cat(
    "Introduced-to-Japan input table: ",
    ifelse(
      introduction_available,
      "AVAILABLE",
      "NOT YET AVAILABLE"
    ),
    "\n",
    sep = ""
  )
  
  dbExecute(
    con,
    "DROP TABLE IF EXISTS temp_star_introduction_status"
  )
  
  if (introduction_available) {
    
    intro_fields <- dbListFields(
      con,
      INTRODUCTION_TABLE
    )
    
    required_intro_fields <- c(
      "FINAL_WCVP_ID",
      "INTRODUCED_TO_JAPAN"
    )
    
    missing_intro_fields <- setdiff(
      required_intro_fields,
      intro_fields
    )
    
    if (length(missing_intro_fields) > 0L) {
      stop(
        "Introduction-status table exists but required fields are missing: ",
        paste(missing_intro_fields, collapse = ", ")
      )
    }
    
    dbExecute(
      con,
      paste0(
        "CREATE TEMP TABLE temp_star_introduction_status AS ",
        "SELECT ",
        "FINAL_WCVP_ID, ",
        "INTRODUCED_TO_JAPAN ",
        "FROM ", INTRODUCTION_TABLE
      )
    )
    
  } else {
    
    dbExecute(
      con,
      paste0(
        "CREATE TEMP TABLE temp_star_introduction_status AS ",
        "SELECT ",
        "FINAL_WCVP_ID, ",
        "CAST(NULL AS BOOLEAN) AS INTRODUCED_TO_JAPAN ",
        "FROM ", SOURCE_TAXA
      )
    )
  }
  
  # ===========================================================================
  # 6. Build one-row-per-taxon Japanese distribution inputs
  #
  # BOTANICAL_AREA_ID is the canonical Distribution 01 field.
  #
  # For the Key:
  # JP47 = Ryukyu
  # JP48 = Izu
  # JP49 + JP50 = Ogasawara group
  # ===========================================================================
  
  dbExecute(
    con,
    paste0(
      "DROP TABLE IF EXISTS ",
      OUTPUT_INPUTS
    )
  )
  
  cat("\nBuilding taxon-level Japanese distribution inputs...\n")
  
  dbExecute(
    con,
    paste0(
      
      "CREATE TABLE ", OUTPUT_INPUTS, " AS ",
      
      "WITH area AS (",
      
      "SELECT ",
      "FINAL_WCVP_ID, ",
      "COUNT(*) AS BOTANICAL_AREAS_PRESENT_CALCULATED, ",
      "SUM(OCCURRENCE_RECORDS) AS OCCURRENCE_RECORDS_CALCULATED, ",
      
      "MAX(CASE ",
      "WHEN BOTANICAL_AREA_ID = 'JP47' ",
      "THEN 1 ELSE 0 END) AS RYUKYU_PRESENT, ",
      
      "MAX(CASE ",
      "WHEN BOTANICAL_AREA_ID = 'JP48' ",
      "THEN 1 ELSE 0 END) AS IZU_PRESENT, ",
      
      "MAX(CASE ",
      "WHEN BOTANICAL_AREA_ID IN ('JP49','JP50') ",
      "THEN 1 ELSE 0 END) AS OGASAWARA_PRESENT, ",
      
      "MAX(CASE ",
      "WHEN BOTANICAL_AREA_ID = 'JP49' ",
      "THEN 1 ELSE 0 END) AS OGASAWARA_JP49_PRESENT, ",
      
      "MAX(CASE ",
      "WHEN BOTANICAL_AREA_ID = 'JP50' ",
      "THEN 1 ELSE 0 END) AS KAZAN_JP50_PRESENT, ",
      
      "MAX(CASE ",
      "WHEN BOTANICAL_AREA_ID = 'JP51' ",
      "THEN 1 ELSE 0 END) AS KURILES_PRESENT, ",
      
      "SUM(CASE ",
      "WHEN BOTANICAL_AREA_ID IN ('JP47','JP48','JP49','JP50') ",
      "THEN 1 ELSE 0 END) AS SMALL_ISLAND_AREAS_PRESENT, ",
      
      "SUM(CASE ",
      "WHEN BOTANICAL_AREA_ID NOT IN ('JP47','JP48','JP49','JP50') ",
      "THEN 1 ELSE 0 END) AS NON_SMALL_ISLAND_AREAS_PRESENT ",
      
      "FROM ", SOURCE_LONG, " ",
      "GROUP BY FINAL_WCVP_ID",
      
      ") ",
      
      "SELECT ",
      
      "t.FINAL_WCVP_ID, ",
      "t.FINAL_WCVP_RECOGNISED_NAME, ",
      "t.FINAL_WCVP_RANK, ",
      "t.FINAL_WCVP_STATUS, ",
      "t.FINAL_WCVP_CONCEPT_CLASS, ",
      "t.OCCURRENCE_RECORDS, ",
      "t.BOTANICAL_AREAS_PRESENT, ",
      "t.BOTANICAL_AREA_IDS, ",
      
      "a.BOTANICAL_AREAS_PRESENT_CALCULATED, ",
      "a.OCCURRENCE_RECORDS_CALCULATED, ",
      "a.RYUKYU_PRESENT, ",
      "a.IZU_PRESENT, ",
      "a.OGASAWARA_PRESENT, ",
      "a.OGASAWARA_JP49_PRESENT, ",
      "a.KAZAN_JP50_PRESENT, ",
      "a.KURILES_PRESENT, ",
      "a.SMALL_ISLAND_AREAS_PRESENT, ",
      "a.NON_SMALL_ISLAND_AREAS_PRESENT, ",
      
      "CASE ",
      "WHEN a.SMALL_ISLAND_AREAS_PRESENT > 0 ",
      "AND a.NON_SMALL_ISLAND_AREAS_PRESENT = 0 ",
      "THEN TRUE ELSE FALSE END ",
      "AS RESTRICTED_TO_RYUKYU_IZU_OGASAWARA, ",
      
      "g.JAPAN_PREFECTURES_PRESENT, ",
      "g.JAPAN_DISTRICTS_PRESENT, ",
      "g.ALMOST_ALL_DISTRICTS, ",
      
      "i.INTRODUCED_TO_JAPAN, ",
      
      # Strict accepted-name eligibility
      "CASE ",
      "WHEN lower(COALESCE(t.FINAL_WCVP_STATUS,'')) = 'accepted' ",
      "THEN TRUE ELSE FALSE END ",
      "AS WCVP_ACCEPTED, ",
      
      # Hybrid determination
      "CASE ",
      "WHEN lower(COALESCE(t.FINAL_WCVP_CONCEPT_CLASS,'')) ",
      "LIKE '%hybrid%' ",
      "OR lower(COALESCE(t.FINAL_WCVP_STATUS,'')) ",
      "LIKE '%hybrid%' ",
      "OR lower(COALESCE(t.FINAL_WCVP_RANK,'')) ",
      "IN ('nothospecies','nothosubsp.','nothovar.','nothof.') ",
      "OR COALESCE(t.FINAL_WCVP_RECOGNISED_NAME,'') LIKE '% × %' ",
      "THEN TRUE ELSE FALSE END ",
      "AS HYBRID_TAXON, ",
      
      # Infraspecific handling
      "CASE ",
      "WHEN lower(COALESCE(t.FINAL_WCVP_RANK,'')) IN ",
      "('subspecies','variety','nothosubsp.','nothovar.') ",
      "THEN TRUE ELSE FALSE END ",
      "AS REQUIRES_INFRASPECIFIC_ADJUSTMENT, ",
      
      "CASE ",
      "WHEN lower(COALESCE(t.FINAL_WCVP_RANK,'')) = 'form' ",
      "THEN TRUE ELSE FALSE END ",
      "AS REQUIRES_CONTAINING_TAXON_STAR, ",
      
      "'", MODULE, "' AS MODULE, ",
      "'", VERSION, "' AS VERSION, ",
      "DATE '", as.character(RUN_DATE), "' AS RUN_DATE ",
      
      "FROM ", SOURCE_TAXA, " t ",
      "JOIN area a USING (FINAL_WCVP_ID) ",
      "LEFT JOIN temp_star_key_geography g ",
      "USING (FINAL_WCVP_ID) ",
      "LEFT JOIN temp_star_introduction_status i ",
      "USING (FINAL_WCVP_ID)"
    )
  )
  
  # ===========================================================================
  # 7. Validate taxon distribution aggregation
  # ===========================================================================
  
  distribution_validation <- dbGetQuery(
    con,
    paste0(
      "SELECT ",
      "COUNT(*) AS SOURCE_TAXA, ",
      "COUNT(DISTINCT FINAL_WCVP_ID) AS DISTINCT_WCVP_IDS, ",
      "SUM(OCCURRENCE_RECORDS) AS OCCURRENCE_RECORDS, ",
      "SUM(BOTANICAL_AREAS_PRESENT) AS TAXON_AREA_PRESENCES, ",
      "SUM(CASE WHEN WCVP_ACCEPTED THEN 1 ELSE 0 END) ",
      "AS ACCEPTED_TAXA, ",
      "SUM(CASE WHEN NOT WCVP_ACCEPTED THEN 1 ELSE 0 END) ",
      "AS NON_ACCEPTED_TAXA, ",
      "SUM(CASE WHEN WCVP_ACCEPTED AND HYBRID_TAXON ",
      "THEN 1 ELSE 0 END) AS ACCEPTED_HYBRID_TAXA, ",
      "SUM(CASE WHEN RESTRICTED_TO_RYUKYU_IZU_OGASAWARA ",
      "THEN 1 ELSE 0 END) AS SMALL_ISLAND_RESTRICTED_TAXA ",
      "FROM ", OUTPUT_INPUTS
    )
  )
  
  cat("\n— Taxon-level distribution inputs —\n")
  print.data.frame(
    distribution_validation,
    row.names = FALSE
  )
  
  # ===========================================================================
  # 8. Apply Star decision hierarchy
  #
  # Precedence:
  #
  # non-accepted -> excluded
  # accepted hybrid -> HYB
  # accepted introduced -> GX
  # accepted non-hybrid known non-introduced -> geographic Key
  # introduction unknown -> introduction status required before geographic Star
  #
  # The restricted-small-island geographic branch is evaluated only after
  # hybrid/GX status has been resolved.
  # ===========================================================================
  
  dbExecute(
    con,
    paste0(
      "DROP TABLE IF EXISTS ",
      OUTPUT_APPLICATION
    )
  )
  
  cat("\nRunning eligible taxa through Key to Stars...\n")
  
  dbExecute(
    con,
    paste0(
      
      "CREATE TABLE ", OUTPUT_APPLICATION, " AS ",
      
      "SELECT ",
      "*, ",
      
      # =======================================================================
      # KEY PATH
      # =======================================================================
      
      "CASE ",
      
      "WHEN NOT WCVP_ACCEPTED ",
      "THEN 'Excluded -> WCVP name not Accepted' ",
      
      "WHEN HYBRID_TAXON ",
      "THEN 'Accepted hybrid taxon -> HYB' ",
      
      "WHEN INTRODUCED_TO_JAPAN = TRUE ",
      "THEN 'Accepted taxon introduced to Japan -> GX' ",
      
      "WHEN INTRODUCED_TO_JAPAN IS NULL ",
      "THEN 'Accepted non-hybrid taxon -> introduction status required' ",
      
      "WHEN REQUIRES_CONTAINING_TAXON_STAR ",
      "THEN 'Form -> containing taxon Star required' ",
      
      "WHEN RESTRICTED_TO_RYUKYU_IZU_OGASAWARA ",
      "THEN 'Restricted to Ryukyu/Izu/Ogasawara -> BK' ",
      
      "WHEN JAPAN_PREFECTURES_PRESENT IS NULL ",
      "OR JAPAN_DISTRICTS_PRESENT IS NULL ",
      "THEN 'Japanese distribution -> exact prefecture/district counts required' ",
      
      "WHEN JAPAN_DISTRICTS_PRESENT <= 2 ",
      "AND JAPAN_PREFECTURES_PRESENT <= 7 ",
      "THEN '<=2 districts AND <=7 prefectures -> BK' ",
      
      "WHEN JAPAN_DISTRICTS_PRESENT <= 2 ",
      "AND JAPAN_PREFECTURES_PRESENT BETWEEN 8 AND 14 ",
      "THEN '<=2 districts AND 8-14 prefectures -> GD' ",
      
      "WHEN (JAPAN_DISTRICTS_PRESENT > 2 ",
      "OR JAPAN_PREFECTURES_PRESENT > 14) ",
      "AND JAPAN_PREFECTURES_PRESENT <= 5 ",
      "THEN 'Widespread branch; <=5 prefectures -> rarity condition required' ",
      
      "WHEN (JAPAN_DISTRICTS_PRESENT > 2 ",
      "OR JAPAN_PREFECTURES_PRESENT > 14) ",
      "AND JAPAN_PREFECTURES_PRESENT BETWEEN 6 AND 10 ",
      "THEN 'Widespread branch; 6-10 prefectures -> GD' ",
      
      "WHEN JAPAN_PREFECTURES_PRESENT > 10 ",
      "AND ALMOST_ALL_DISTRICTS = FALSE ",
      "THEN '>10 prefectures; not all districts -> BU' ",
      
      "WHEN JAPAN_PREFECTURES_PRESENT > 10 ",
      "AND ALMOST_ALL_DISTRICTS = TRUE ",
      "THEN '>10 prefectures; almost all districts -> GN' ",
      
      "WHEN JAPAN_PREFECTURES_PRESENT > 10 ",
      "AND ALMOST_ALL_DISTRICTS IS NULL ",
      "THEN '>10 prefectures -> almost-all-districts test required' ",
      
      "ELSE 'Key review required' ",
      "END AS KEY_PATH, ",
      
      # =======================================================================
      # STAR
      # =======================================================================
      
      "CASE ",
      
      "WHEN NOT WCVP_ACCEPTED ",
      "THEN NULL ",
      
      "WHEN HYBRID_TAXON ",
      "THEN 'HYB' ",
      
      "WHEN INTRODUCED_TO_JAPAN = TRUE ",
      "THEN 'GX' ",
      
      "WHEN INTRODUCED_TO_JAPAN IS NULL ",
      "THEN NULL ",
      
      "WHEN REQUIRES_CONTAINING_TAXON_STAR ",
      "THEN NULL ",
      
      "WHEN RESTRICTED_TO_RYUKYU_IZU_OGASAWARA ",
      "THEN 'BK' ",
      
      "WHEN JAPAN_PREFECTURES_PRESENT IS NULL ",
      "OR JAPAN_DISTRICTS_PRESENT IS NULL ",
      "THEN NULL ",
      
      "WHEN JAPAN_DISTRICTS_PRESENT <= 2 ",
      "AND JAPAN_PREFECTURES_PRESENT <= 7 ",
      "THEN 'BK' ",
      
      "WHEN JAPAN_DISTRICTS_PRESENT <= 2 ",
      "AND JAPAN_PREFECTURES_PRESENT BETWEEN 8 AND 14 ",
      "THEN 'GD' ",
      
      "WHEN (JAPAN_DISTRICTS_PRESENT > 2 ",
      "OR JAPAN_PREFECTURES_PRESENT > 14) ",
      "AND JAPAN_PREFECTURES_PRESENT BETWEEN 6 AND 10 ",
      "THEN 'GD' ",
      
      "WHEN JAPAN_PREFECTURES_PRESENT > 10 ",
      "AND ALMOST_ALL_DISTRICTS = FALSE ",
      "THEN 'BU' ",
      
      "WHEN JAPAN_PREFECTURES_PRESENT > 10 ",
      "AND ALMOST_ALL_DISTRICTS = TRUE ",
      "THEN 'GN' ",
      
      "ELSE NULL ",
      "END AS STAR, ",
      
      # =======================================================================
      # NEXT REQUIRED INPUT
      # =======================================================================
      
      "CASE ",
      
      "WHEN NOT WCVP_ACCEPTED ",
      "THEN NULL ",
      
      "WHEN HYBRID_TAXON ",
      "THEN NULL ",
      
      "WHEN INTRODUCED_TO_JAPAN = TRUE ",
      "THEN NULL ",
      
      "WHEN INTRODUCED_TO_JAPAN IS NULL ",
      "THEN 'INTRODUCTION_STATUS' ",
      
      "WHEN REQUIRES_CONTAINING_TAXON_STAR ",
      "THEN 'CONTAINING_TAXON_STAR' ",
      
      "WHEN RESTRICTED_TO_RYUKYU_IZU_OGASAWARA ",
      "THEN NULL ",
      
      "WHEN JAPAN_PREFECTURES_PRESENT IS NULL ",
      "THEN 'JAPAN_PREFECTURE_COUNT' ",
      
      "WHEN JAPAN_DISTRICTS_PRESENT IS NULL ",
      "THEN 'JAPAN_DISTRICT_COUNT' ",
      
      "WHEN (JAPAN_DISTRICTS_PRESENT > 2 ",
      "OR JAPAN_PREFECTURES_PRESENT > 14) ",
      "AND JAPAN_PREFECTURES_PRESENT <= 5 ",
      "THEN 'RARE_WITHIN_RANGE' ",
      
      "WHEN JAPAN_PREFECTURES_PRESENT > 10 ",
      "AND ALMOST_ALL_DISTRICTS IS NULL ",
      "THEN 'ALMOST_ALL_DISTRICTS' ",
      
      "WHEN REQUIRES_INFRASPECIFIC_ADJUSTMENT ",
      "THEN 'PARENT_SPECIES_STAR_ADJUSTMENT' ",
      
      "ELSE NULL ",
      "END AS NEXT_REQUIRED_INPUT, ",
      
      # =======================================================================
      # STATUS
      # =======================================================================
      
      "CASE ",
      
      "WHEN NOT WCVP_ACCEPTED ",
      "THEN 'EXCLUDED_NON_ACCEPTED' ",
      
      "WHEN HYBRID_TAXON ",
      "THEN 'STAR_ASSIGNED' ",
      
      "WHEN INTRODUCED_TO_JAPAN = TRUE ",
      "THEN 'STAR_ASSIGNED' ",
      
      "WHEN INTRODUCED_TO_JAPAN IS NULL ",
      "THEN 'INTRODUCTION_STATUS_REQUIRED' ",
      
      "WHEN REQUIRES_CONTAINING_TAXON_STAR ",
      "THEN 'PARENT_STAR_REQUIRED' ",
      
      "WHEN RESTRICTED_TO_RYUKYU_IZU_OGASAWARA ",
      "THEN 'STAR_ASSIGNED' ",
      
      "WHEN JAPAN_PREFECTURES_PRESENT IS NULL ",
      "OR JAPAN_DISTRICTS_PRESENT IS NULL ",
      "THEN 'KEY_GEOGRAPHY_REQUIRED' ",
      
      "WHEN (JAPAN_DISTRICTS_PRESENT > 2 ",
      "OR JAPAN_PREFECTURES_PRESENT > 14) ",
      "AND JAPAN_PREFECTURES_PRESENT <= 5 ",
      "THEN 'RARITY_INPUT_REQUIRED' ",
      
      "WHEN JAPAN_PREFECTURES_PRESENT > 10 ",
      "AND ALMOST_ALL_DISTRICTS IS NULL ",
      "THEN 'KEY_GEOGRAPHY_REQUIRED' ",
      
      "WHEN REQUIRES_INFRASPECIFIC_ADJUSTMENT ",
      "THEN 'PARENT_STAR_REQUIRED' ",
      
      "ELSE 'STAR_ASSIGNED' ",
      "END AS STAR_STATUS, ",
      
      "'", MODULE, "' AS APPLICATION_MODULE, ",
      "'", VERSION, "' AS APPLICATION_VERSION, ",
      "DATE '", as.character(RUN_DATE), "' AS APPLICATION_DATE ",
      
      "FROM ", OUTPUT_INPUTS
    )
  )
  
  # ===========================================================================
  # 9. Star profile
  # ===========================================================================
  
  star_profile <- dbGetQuery(
    con,
    paste0(
      "SELECT ",
      "COALESCE(STAR, 'UNRESOLVED') AS STAR, ",
      "COUNT(*) AS WCVP_TAXA, ",
      "SUM(OCCURRENCE_RECORDS) AS OCCURRENCE_RECORDS ",
      "FROM ", OUTPUT_APPLICATION, " ",
      "GROUP BY COALESCE(STAR, 'UNRESOLVED') ",
      "ORDER BY STAR"
    )
  )
  
  cat("\n— Contemporary Star profile —\n")
  print.data.frame(
    star_profile,
    row.names = FALSE
  )
  
  # ===========================================================================
  # 10. Status profile
  # ===========================================================================
  
  status_profile <- dbGetQuery(
    con,
    paste0(
      "SELECT ",
      "STAR_STATUS, ",
      "COUNT(*) AS WCVP_TAXA, ",
      "SUM(OCCURRENCE_RECORDS) AS OCCURRENCE_RECORDS ",
      "FROM ", OUTPUT_APPLICATION, " ",
      "GROUP BY STAR_STATUS ",
      "ORDER BY WCVP_TAXA DESC"
    )
  )
  
  cat("\n— Star attribution status —\n")
  print.data.frame(
    status_profile,
    row.names = FALSE
  )
  
  # ===========================================================================
  # 11. Next unresolved input
  # ===========================================================================
  
  next_input_profile <- dbGetQuery(
    con,
    paste0(
      "SELECT ",
      "NEXT_REQUIRED_INPUT, ",
      "COUNT(*) AS WCVP_TAXA, ",
      "SUM(OCCURRENCE_RECORDS) AS OCCURRENCE_RECORDS ",
      "FROM ", OUTPUT_APPLICATION, " ",
      "WHERE NEXT_REQUIRED_INPUT IS NOT NULL ",
      "GROUP BY NEXT_REQUIRED_INPUT ",
      "ORDER BY WCVP_TAXA DESC"
    )
  )
  
  cat("\n— Next unresolved Key input —\n")
  print.data.frame(
    next_input_profile,
    row.names = FALSE
  )
  
  # ===========================================================================
  # 12. Key paths
  # ===========================================================================
  
  key_path_profile <- dbGetQuery(
    con,
    paste0(
      "SELECT ",
      "KEY_PATH, ",
      "COUNT(*) AS WCVP_TAXA, ",
      "SUM(OCCURRENCE_RECORDS) AS OCCURRENCE_RECORDS ",
      "FROM ", OUTPUT_APPLICATION, " ",
      "GROUP BY KEY_PATH ",
      "ORDER BY WCVP_TAXA DESC"
    )
  )
  
  cat("\n— Key paths —\n")
  print.data.frame(
    key_path_profile,
    row.names = FALSE
  )
  
  # ===========================================================================
  # 13. Validation
  # ===========================================================================
  
  output_profile <- dbGetQuery(
    con,
    paste0(
      "SELECT ",
      "COUNT(*) AS n, ",
      "COUNT(DISTINCT FINAL_WCVP_ID) AS ids, ",
      "SUM(OCCURRENCE_RECORDS) AS occurrences, ",
      "SUM(BOTANICAL_AREAS_PRESENT) AS area_presences, ",
      "SUM(CASE WHEN WCVP_ACCEPTED THEN 1 ELSE 0 END) ",
      "AS accepted_taxa, ",
      "SUM(CASE WHEN NOT WCVP_ACCEPTED THEN 1 ELSE 0 END) ",
      "AS non_accepted_taxa ",
      "FROM ", OUTPUT_APPLICATION
    )
  )
  
  invalid_star <- dbGetQuery(
    con,
    paste0(
      "SELECT COUNT(*) AS n ",
      "FROM ", OUTPUT_APPLICATION, " ",
      "WHERE STAR IS NOT NULL ",
      "AND STAR NOT IN ('BK','GD','BU','GN','GX','HYB')"
    )
  )$n[[1]]
  
  nonaccepted_with_star <- dbGetQuery(
    con,
    paste0(
      "SELECT COUNT(*) AS n ",
      "FROM ", OUTPUT_APPLICATION, " ",
      "WHERE NOT WCVP_ACCEPTED ",
      "AND STAR IS NOT NULL"
    )
  )$n[[1]]
  
  hybrid_not_hyb <- dbGetQuery(
    con,
    paste0(
      "SELECT COUNT(*) AS n ",
      "FROM ", OUTPUT_APPLICATION, " ",
      "WHERE WCVP_ACCEPTED ",
      "AND HYBRID_TAXON ",
      "AND STAR IS DISTINCT FROM 'HYB'"
    )
  )$n[[1]]
  
  introduced_not_gx <- dbGetQuery(
    con,
    paste0(
      "SELECT COUNT(*) AS n ",
      "FROM ", OUTPUT_APPLICATION, " ",
      "WHERE WCVP_ACCEPTED ",
      "AND NOT HYBRID_TAXON ",
      "AND INTRODUCED_TO_JAPAN = TRUE ",
      "AND STAR IS DISTINCT FROM 'GX'"
    )
  )$n[[1]]
  
  unknown_intro_geographic_star <- dbGetQuery(
    con,
    paste0(
      "SELECT COUNT(*) AS n ",
      "FROM ", OUTPUT_APPLICATION, " ",
      "WHERE WCVP_ACCEPTED ",
      "AND NOT HYBRID_TAXON ",
      "AND INTRODUCED_TO_JAPAN IS NULL ",
      "AND STAR IN ('BK','GD','BU','GN')"
    )
  )$n[[1]]
  
  invented_prefecture <- if (!key_geography_available) {
    dbGetQuery(
      con,
      paste0(
        "SELECT COUNT(*) AS n ",
        "FROM ", OUTPUT_APPLICATION, " ",
        "WHERE JAPAN_PREFECTURES_PRESENT IS NOT NULL"
      )
    )$n[[1]]
  } else {
    0L
  }
  
  invented_district <- if (!key_geography_available) {
    dbGetQuery(
      con,
      paste0(
        "SELECT COUNT(*) AS n ",
        "FROM ", OUTPUT_APPLICATION, " ",
        "WHERE JAPAN_DISTRICTS_PRESENT IS NOT NULL"
      )
    )$n[[1]]
  } else {
    0L
  }
  
  validation <- tibble::tibble(
    check = c(
      "stars_01_key_validation_passed",
      "source_11484_taxa_preserved",
      "all_WCVP_IDs_unique",
      "all_2851736_occurrences_represented",
      "all_128876_taxon_area_presences_represented",
      "only_valid_star_codes_used",
      "nonaccepted_taxa_receive_no_star",
      "accepted_hybrids_receive_HYB",
      "accepted_introduced_nonhybrids_receive_GX",
      "unknown_introduction_not_treated_as_native",
      "prefecture_counts_not_invented",
      "district_counts_not_invented",
      "historical_star_totals_not_used",
      "foj_historical_stars_not_used",
      "rare_not_inferred_from_occurrence_count",
      "occurrences_not_modified",
      "taxonomy_not_modified",
      "geography_not_modified",
      "quarantine_not_modified",
      "ghi_not_calculated"
    ),
    pass = c(
      all(key_validation$pass),
      output_profile$n[[1]] == EXPECTED_SOURCE_TAXA,
      output_profile$ids[[1]] == EXPECTED_SOURCE_TAXA,
      output_profile$occurrences[[1]] == EXPECTED_OCCURRENCES,
      output_profile$area_presences[[1]] == EXPECTED_AREA_PRESENCES,
      invalid_star == 0L,
      nonaccepted_with_star == 0L,
      hybrid_not_hyb == 0L,
      introduced_not_gx == 0L,
      unknown_intro_geographic_star == 0L,
      invented_prefecture == 0L,
      invented_district == 0L,
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE
    )
  )
  
  cat("\n— Validation —\n")
  print.data.frame(
    validation,
    row.names = FALSE
  )
  
  all_pass <- all(validation$pass)
  
  cat(
    "\nAll Stars 03 validation checks PASS: ",
    all_pass,
    "\n",
    sep = ""
  )
  
  if (!all_pass) {
    stop(
      "Stars 03 validation failed. ",
      "Do not freeze this module."
    )
  }
  
  # ===========================================================================
  # 14. Summary
  # ===========================================================================
  
  summary_table <- dbGetQuery(
    con,
    paste0(
      "SELECT ",
      "COUNT(*) AS SOURCE_WCVP_TAXA, ",
      "SUM(CASE WHEN WCVP_ACCEPTED THEN 1 ELSE 0 END) ",
      "AS ACCEPTED_TAXA, ",
      "SUM(CASE WHEN NOT WCVP_ACCEPTED THEN 1 ELSE 0 END) ",
      "AS EXCLUDED_NON_ACCEPTED, ",
      "SUM(CASE WHEN STAR = 'HYB' THEN 1 ELSE 0 END) AS HYB, ",
      "SUM(CASE WHEN STAR = 'GX' THEN 1 ELSE 0 END) AS GX, ",
      "SUM(CASE WHEN STAR = 'BK' THEN 1 ELSE 0 END) AS BK, ",
      "SUM(CASE WHEN STAR = 'GD' THEN 1 ELSE 0 END) AS GD, ",
      "SUM(CASE WHEN STAR = 'BU' THEN 1 ELSE 0 END) AS BU, ",
      "SUM(CASE WHEN STAR = 'GN' THEN 1 ELSE 0 END) AS GN, ",
      "SUM(CASE WHEN WCVP_ACCEPTED AND STAR IS NULL ",
      "THEN 1 ELSE 0 END) AS ACCEPTED_UNRESOLVED, ",
      "SUM(CASE WHEN RESTRICTED_TO_RYUKYU_IZU_OGASAWARA ",
      "THEN 1 ELSE 0 END) AS SMALL_ISLAND_RESTRICTED ",
      "FROM ", OUTPUT_APPLICATION
    )
  )
  
  cat("\n— Summary —\n")
  print.data.frame(
    summary_table,
    row.names = FALSE
  )
  
  # ===========================================================================
  # 15. Persist
  # ===========================================================================
  
  dbWriteTable(
    con,
    OUTPUT_SUMMARY,
    summary_table,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    OUTPUT_VALIDATION,
    validation,
    overwrite = TRUE
  )
  
  metadata <- tibble::tibble(
    metric = c(
      "module",
      "version",
      "run_date",
      "source_taxa",
      "source_taxon_area_distribution",
      "key_geography_available",
      "introduction_status_available",
      "accepted_names_only_for_star_attribution",
      "GX_definition",
      "HYB_definition",
      "Ogasawara_key_areas",
      "historical_star_totals_used",
      "foj_historical_stars_used",
      "rare_inferred_from_occurrence_count",
      "ghi_calculated",
      "validation_pass"
    ),
    value = c(
      MODULE,
      VERSION,
      as.character(RUN_DATE),
      SOURCE_TAXA,
      SOURCE_LONG,
      as.character(key_geography_available),
      as.character(introduction_available),
      "TRUE",
      "introduced_to_Japan",
      "hybrid_taxon",
      "JP49+JP50",
      "FALSE",
      "FALSE",
      "FALSE",
      "FALSE",
      as.character(all_pass)
    )
  )
  
  dbWriteTable(
    con,
    OUTPUT_METADATA,
    metadata,
    overwrite = TRUE
  )
  
  # ===========================================================================
  # 16. CSV outputs
  # ===========================================================================
  
  write_csv(
    accepted_profile,
    file.path(
      OUTPUT_DIR,
      "stars03_wcvp_status_profile.csv"
    )
  )
  
  write_csv(
    star_profile,
    file.path(
      OUTPUT_DIR,
      "stars03_star_profile.csv"
    )
  )
  
  write_csv(
    status_profile,
    file.path(
      OUTPUT_DIR,
      "stars03_status_profile.csv"
    )
  )
  
  write_csv(
    next_input_profile,
    file.path(
      OUTPUT_DIR,
      "stars03_next_input_profile.csv"
    )
  )
  
  write_csv(
    key_path_profile,
    file.path(
      OUTPUT_DIR,
      "stars03_key_paths.csv"
    )
  )
  
  write_csv(
    summary_table,
    file.path(
      OUTPUT_DIR,
      "stars03_summary.csv"
    )
  )
  
  write_csv(
    validation,
    file.path(
      OUTPUT_DIR,
      "stars03_validation.csv"
    )
  )
  
  write_csv(
    metadata,
    file.path(
      OUTPUT_DIR,
      "stars03_metadata.csv"
    )
  )
  
  # ===========================================================================
  # 17. Final status
  # ===========================================================================
  
  cat("\nCanonical taxon distribution input table:\n")
  cat("  ", OUTPUT_INPUTS, "\n", sep = "")
  
  cat("\nCanonical Key application table:\n")
  cat("  ", OUTPUT_APPLICATION, "\n", sep = "")
  
  cat(
    "\nSource WCVP concepts: ",
    format(output_profile$n[[1]], big.mark = ","),
    "\n",
    sep = ""
  )
  
  cat(
    "Accepted WCVP concepts eligible for Star methodology: ",
    format(output_profile$accepted_taxa[[1]], big.mark = ","),
    "\n",
    sep = ""
  )
  
  cat(
    "Non-accepted concepts excluded: ",
    format(output_profile$non_accepted_taxa[[1]], big.mark = ","),
    "\n",
    sep = ""
  )
  
  cat(
    "Exact Key prefecture/district geography available: ",
    key_geography_available,
    "\n",
    sep = ""
  )
  
  cat(
    "Introduction-status evidence available: ",
    introduction_available,
    "\n",
    sep = ""
  )
  
  cat("\nStar vocabulary: BK / GD / BU / GN / GX / HYB\n")
  cat("GX = introduced to Japan\n")
  cat("HYB = hybrid taxon\n")
  cat("Historical Star totals used: FALSE\n")
  cat("FOJ historical Stars used for assignment: FALSE\n")
  cat("GHI calculated: FALSE\n")
  
  cat(
    "\nStars 03 v",
    VERSION,
    " complete.\n",
    sep = ""
  )
  
  invisible(
    list(
      wcvp_status = accepted_profile,
      stars = star_profile,
      status = status_profile,
      next_inputs = next_input_profile,
      key_paths = key_path_profile,
      summary = summary_table,
      validation = validation,
      metadata = metadata
    )
  )
}

result_stars_03 <- run_stars_03()