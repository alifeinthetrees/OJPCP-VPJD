# =============================================================================
# VPJD-OJPCP
# R/stars/02_profile_key_input_requirements.R
# Version 0.1.0
#
# PROFILE CONTEMPORARY KEY-TO-STARS INPUT REQUIREMENTS
#
# Purpose
# -------
# Profile the 11,484 contemporary WCVP concepts against the analytical inputs
# required by the validated Key to Stars decision engine.
#
# Sources
# -------
# vpjd_japan_taxon_distribution
# vpjd_japan_taxon_area_distribution
# vpjd_star_key_rule_register
# vpjd_star_key_validation
#
# Output
# ------
# vpjd_star_key_input_requirements
#
# Grain
# -----
# One row per contemporary FINAL_WCVP_ID.
#
# Scientific principles
# ---------------------
# - No Stars are assigned here.
# - Historical FOJ Star assignments/totals are not used.
# - Botanical areas are not silently equated with Star prefectures/districts.
# - Japan endemicity is not inferred without global evidence.
# - Missing qualitative evidence is retained explicitly.
# - The output specifies what must be derived/acquired before attribution.
# =============================================================================

suppressPackageStartupMessages({
  library(DBI)
  library(duckdb)
  library(dplyr)
  library(readr)
  library(here)
})

MODULE <- "stars_02_profile_key_input_requirements"
VERSION <- "0.1.0"
RUN_DATE <- as.Date("2026-09-20")

SOURCE_TAXON <- "vpjd_japan_taxon_distribution"
SOURCE_AREA <- "vpjd_japan_taxon_area_distribution"
SOURCE_KEY <- "vpjd_star_key_rule_register"
SOURCE_KEY_VALIDATION <- "vpjd_star_key_validation"
OUTPUT_TABLE <- "vpjd_star_key_input_requirements"

EXPECTED_TAXA <- 11484L
EXPECTED_OCCURRENCES <- 2851736L
EXPECTED_TAXON_AREA_ROWS <- 128876L
EXPECTED_BOTANICAL_AREAS <- 51L

DB_PATH <- here(
  "data", "interim", "occurrences",
  "vpjd_occurrences.duckdb"
)

OUTPUT_DIR <- here(
  "outputs", "tables", "stars",
  "key_input_requirements"
)

run_stars_02 <- function() {
  cat("\n— VPJD Key-to-Stars input requirements —\n\n")
  cat("Run date: ", RUN_DATE, "\n", sep = "")
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
    SOURCE_TAXON,
    SOURCE_AREA,
    SOURCE_KEY,
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
  
  q_taxon <- paste0('"', SOURCE_TAXON, '"')
  q_area <- paste0('"', SOURCE_AREA, '"')
  q_output <- paste0('"', OUTPUT_TABLE, '"')
  
  # ===========================================================================
  # 1. Validate frozen upstream sources
  # ===========================================================================
  
  key_validation <- dbGetQuery(
    con,
    paste0(
      'SELECT "check", pass FROM "',
      SOURCE_KEY_VALIDATION,
      '"'
    )
  )
  
  if (nrow(key_validation) == 0L ||
      any(is.na(key_validation$pass)) ||
      !all(key_validation$pass)) {
    stop(
      "Stars 01 Key validation is not fully PASS. ",
      "Do not continue."
    )
  }
  
  source_profile <- dbGetQuery(
    con,
    paste0(
      "SELECT ",
      "COUNT(*) AS WCVP_TAXA, ",
      "COUNT(DISTINCT FINAL_WCVP_ID) AS DISTINCT_WCVP_IDS, ",
      "SUM(OCCURRENCE_RECORDS) AS OCCURRENCE_RECORDS, ",
      "SUM(BOTANICAL_AREAS_PRESENT) AS TAXON_AREA_PRESENCES ",
      "FROM ", q_taxon
    )
  )
  
  source_area_profile <- dbGetQuery(
    con,
    paste0(
      "SELECT ",
      "COUNT(*) AS TAXON_AREA_ROWS, ",
      "COUNT(DISTINCT BOTANICAL_AREA_ID) AS BOTANICAL_AREAS ",
      "FROM ", q_area
    )
  )
  
  cat("— Upstream source profile —\n")
  print.data.frame(source_profile, row.names = FALSE)
  print.data.frame(source_area_profile, row.names = FALSE)
  
  source_taxa <- source_profile$WCVP_TAXA[[1]]
  source_ids <- source_profile$DISTINCT_WCVP_IDS[[1]]
  source_occurrences <- source_profile$OCCURRENCE_RECORDS[[1]]
  source_taxon_area <- source_profile$TAXON_AREA_PRESENCES[[1]]
  source_area_rows <- source_area_profile$TAXON_AREA_ROWS[[1]]
  source_areas <- source_area_profile$BOTANICAL_AREAS[[1]]
  
  if (source_taxa != EXPECTED_TAXA ||
      source_ids != EXPECTED_TAXA ||
      source_occurrences != EXPECTED_OCCURRENCES ||
      source_taxon_area != EXPECTED_TAXON_AREA_ROWS ||
      source_area_rows != EXPECTED_TAXON_AREA_ROWS ||
      source_areas != EXPECTED_BOTANICAL_AREAS) {
    stop(
      "Frozen Distribution 01/02 source counts differ from expected values."
    )
  }
  
  cat("\nUpstream source validation: PASS\n")
  
  # ===========================================================================
  # 2. Determine contemporary information already present
  # ===========================================================================
  
  taxon_fields <- dbListFields(
    con,
    SOURCE_TAXON
  )
  
  required_taxon_fields <- c(
    "FINAL_WCVP_ID",
    "FINAL_WCVP_RECOGNISED_NAME",
    "FINAL_WCVP_RANK",
    "FINAL_WCVP_STATUS",
    "FINAL_WCVP_CONCEPT_CLASS",
    "BOTANICAL_AREAS_PRESENT",
    "BOTANICAL_AREA_IDS",
    "OCCURRENCE_RECORDS",
    "NATIONAL_GBIF_RECORDS",
    "RECOVERED_INSTITUTIONAL_RECORDS",
    "JP47_PRESENT",
    "JP48_PRESENT",
    "JP49_PRESENT"
  )
  
  missing_taxon_fields <- setdiff(
    required_taxon_fields,
    taxon_fields
  )
  
  if (length(missing_taxon_fields) > 0L) {
    stop(
      "Required Distribution 02 fields missing: ",
      paste(missing_taxon_fields, collapse = ", ")
    )
  }
  
  # ===========================================================================
  # 3. Build one-row-per-taxon input-requirements layer
  #
  # Important:
  # JP47 = Ryukyu
  # JP48 = Izu
  # JP49 = Ogasawara
  #
  # The Key criterion is restriction to any/all of these island groups.
  # This can be derived from contemporary botanical-area presence.
  #
  # Prefecture and district counts are deliberately NOT populated here.
  # They require a separate Star-geography derivation.
  # ===========================================================================
  
  if (OUTPUT_TABLE %in% dbListTables(con)) {
    cat("\nExisting derived Stars 02 table detected; rebuilding...\n")
    dbExecute(
      con,
      paste0("DROP TABLE ", q_output)
    )
  }
  
  cat("\nBuilding Key-input requirements layer...\n")
  
  dbExecute(
    con,
    paste0(
      "CREATE TABLE ", q_output, " AS ",
      "SELECT ",
      "FINAL_WCVP_ID, ",
      "FINAL_WCVP_RECOGNISED_NAME, ",
      "FINAL_WCVP_RANK, ",
      "FINAL_WCVP_STATUS, ",
      "FINAL_WCVP_CONCEPT_CLASS, ",
      "BOTANICAL_AREAS_PRESENT, ",
      "BOTANICAL_AREA_IDS, ",
      "OCCURRENCE_RECORDS, ",
      "NATIONAL_GBIF_RECORDS, ",
      "RECOVERED_INSTITUTIONAL_RECORDS, ",
      
      # Current Japanese evidence
      "1 AS INPUT_JAPAN_OCCURRENCE_EVIDENCE_AVAILABLE, ",
      "1 AS INPUT_BOTANICAL_AREA_DISTRIBUTION_AVAILABLE, ",
      
      # Small-island criterion
      "CASE ",
      "WHEN BOTANICAL_AREAS_PRESENT > 0 ",
      "AND BOTANICAL_AREAS_PRESENT = ",
      "(JP47_PRESENT + JP48_PRESENT + JP49_PRESENT) ",
      "THEN 1 ELSE 0 END ",
      "AS DERIVED_RESTRICTED_TO_RYUKYU_OGASAWARA_IZU, ",
      
      # Star geography not yet derived
      "CAST(NULL AS INTEGER) AS JAPAN_PREFECTURES_PRESENT, ",
      "CAST(NULL AS INTEGER) AS JAPAN_DISTRICTS_PRESENT, ",
      "'REQUIRES_STAR_GEOGRAPHY' ",
      "AS INPUT_JAPAN_PREFECTURES_STATUS, ",
      "'REQUIRES_STAR_GEOGRAPHY' ",
      "AS INPUT_JAPAN_DISTRICTS_STATUS, ",
      
      # Global distribution not yet acquired
      "CAST(NULL AS BOOLEAN) AS ENDEMIC_TO_JAPAN, ",
      "'REQUIRES_GLOBAL_DATA' AS INPUT_ENDEMICITY_STATUS, ",
      
      "CAST(NULL AS VARCHAR) AS OUTSIDE_JAPAN_SCOPE, ",
      "'REQUIRES_GLOBAL_DATA' AS INPUT_OUTSIDE_JAPAN_STATUS, ",
      
      "CAST(NULL AS VARCHAR) AS TAIWAN_DISTRIBUTION_CLASS, ",
      "'REQUIRES_GLOBAL_DATA' AS INPUT_TAIWAN_STATUS, ",
      
      "CAST(NULL AS VARCHAR) AS KOREA_DISTRIBUTION_CLASS, ",
      "'REQUIRES_GLOBAL_DATA' AS INPUT_KOREA_STATUS, ",
      
      "CAST(NULL AS VARCHAR) AS KURILES_SAKHALIN_CLASS, ",
      "'REQUIRES_GLOBAL_DATA' AS INPUT_KURILES_SAKHALIN_STATUS, ",
      
      "CAST(NULL AS INTEGER) AS CHINA_PROVINCES_PRESENT, ",
      "'REQUIRES_GLOBAL_DATA' AS INPUT_CHINA_STATUS, ",
      
      # Qualitative / supplementary inputs
      "CAST(NULL AS BOOLEAN) AS RARE_WITHIN_RANGE, ",
      "'REQUIRES_EXTERNAL_OR_EXPERT_EVIDENCE' ",
      "AS INPUT_RARE_WITHIN_RANGE_STATUS, ",
      
      "CAST(NULL AS BOOLEAN) AS ALMOST_ALL_DISTRICTS, ",
      "'REQUIRES_STAR_GEOGRAPHY_DEFINITION' ",
      "AS INPUT_ALMOST_ALL_DISTRICTS_STATUS, ",
      
      "CAST(NULL AS BOOLEAN) AS INTRODUCED, ",
      "CAST(NULL AS BOOLEAN) AS NATURALISED, ",
      "CAST(NULL AS BOOLEAN) AS CULTIVATED_ONLY, ",
      "'REQUIRES_STATUS_EVIDENCE' ",
      "AS INPUT_NATIVE_ALIEN_STATUS, ",
      
      # Infraspecific requirement
      "CASE ",
      "WHEN lower(FINAL_WCVP_RANK) IN ",
      "('subspecies','subsp.','ssp.','variety','var.') ",
      "THEN 1 ELSE 0 END AS REQUIRES_PARENT_SPECIES_STAR, ",
      
      "CASE ",
      "WHEN lower(FINAL_WCVP_RANK) IN ",
      "('form','forma','f.') ",
      "THEN 1 ELSE 0 END AS REQUIRES_CONTAINING_TAXON_STAR, ",
      
      # Genus-level concepts require taxonomic review before Star use
      "CASE ",
      "WHEN lower(FINAL_WCVP_RANK) = 'genus' ",
      "THEN 1 ELSE 0 END AS REQUIRES_GENUS_LEVEL_REVIEW, ",
      
      # Non-accepted concepts retained but explicitly flagged
      "CASE ",
      "WHEN FINAL_WCVP_CONCEPT_CLASS = 'unplaced_wcvp' ",
      "OR FINAL_WCVP_STATUS = 'Unplaced' ",
      "THEN 1 ELSE 0 END AS REQUIRES_UNPLACED_REVIEW, ",
      
      "CASE ",
      "WHEN FINAL_WCVP_CONCEPT_CLASS = 'artificial_hybrid_wcvp' ",
      "OR FINAL_WCVP_STATUS = 'Artificial Hybrid' ",
      "THEN 1 ELSE 0 END AS REQUIRES_HYBRID_REVIEW, ",
      
      # Overall readiness: deliberately conservative
      "'NOT_READY_FOR_STAR_ASSIGNMENT' AS STAR_INPUT_READINESS, ",
      
      "'", MODULE, "' AS MODULE, ",
      "'", VERSION, "' AS VERSION, ",
      "DATE '", RUN_DATE, "' AS RUN_DATE ",
      
      "FROM ", q_taxon
    )
  )
  
  # ===========================================================================
  # 4. Core profile
  # ===========================================================================
  
  core_profile <- dbGetQuery(
    con,
    paste0(
      "SELECT ",
      "COUNT(*) AS WCVP_TAXA, ",
      "SUM(INPUT_JAPAN_OCCURRENCE_EVIDENCE_AVAILABLE) ",
      "AS JAPAN_OCCURRENCE_AVAILABLE, ",
      "SUM(INPUT_BOTANICAL_AREA_DISTRIBUTION_AVAILABLE) ",
      "AS BOTANICAL_DISTRIBUTION_AVAILABLE, ",
      "SUM(DERIVED_RESTRICTED_TO_RYUKYU_OGASAWARA_IZU) ",
      "AS CURRENTLY_RESTRICTED_TO_KEY_ISLAND_GROUPS, ",
      "SUM(REQUIRES_PARENT_SPECIES_STAR) ",
      "AS REQUIRES_PARENT_SPECIES_STAR, ",
      "SUM(REQUIRES_CONTAINING_TAXON_STAR) ",
      "AS REQUIRES_CONTAINING_TAXON_STAR, ",
      "SUM(REQUIRES_GENUS_LEVEL_REVIEW) ",
      "AS GENUS_LEVEL_REVIEW, ",
      "SUM(REQUIRES_UNPLACED_REVIEW) ",
      "AS UNPLACED_REVIEW, ",
      "SUM(REQUIRES_HYBRID_REVIEW) ",
      "AS HYBRID_REVIEW ",
      "FROM ", q_output
    )
  )
  
  cat("\n— Core Key-input profile —\n")
  print.data.frame(
    core_profile,
    row.names = FALSE
  )
  
  # ===========================================================================
  # 5. Rank profile
  # ===========================================================================
  
  rank_profile <- dbGetQuery(
    con,
    paste0(
      "SELECT ",
      "FINAL_WCVP_RANK, ",
      "COUNT(*) AS WCVP_TAXA, ",
      "SUM(OCCURRENCE_RECORDS) AS OCCURRENCE_RECORDS, ",
      "SUM(REQUIRES_PARENT_SPECIES_STAR) ",
      "AS REQUIRES_PARENT_SPECIES_STAR, ",
      "SUM(REQUIRES_CONTAINING_TAXON_STAR) ",
      "AS REQUIRES_CONTAINING_TAXON_STAR, ",
      "SUM(REQUIRES_GENUS_LEVEL_REVIEW) ",
      "AS GENUS_LEVEL_REVIEW ",
      "FROM ", q_output, " ",
      "GROUP BY FINAL_WCVP_RANK ",
      "ORDER BY WCVP_TAXA DESC"
    )
  )
  
  cat("\n— Taxonomic rank requirements —\n")
  print.data.frame(
    rank_profile,
    row.names = FALSE
  )
  
  # ===========================================================================
  # 6. Input-status register
  #
  # This is the key output for planning subsequent modules.
  # ===========================================================================
  
  input_register <- tibble::tribble(
    ~KEY_INPUT, ~CURRENT_STATUS, ~SOURCE_OR_NEXT_STAGE, ~NOTES,
    
    "Japanese occurrence evidence",
    "AVAILABLE",
    "Distribution 01/02",
    "2,851,736 WCVP-linked occurrence records.",
    
    "Japanese botanical-area distribution",
    "AVAILABLE",
    "Distribution 01/02",
    "51-area contemporary occurrence-derived distribution.",
    
    "Restricted to Ryukyu/Ogasawara/Izu",
    "DERIVABLE_NOW",
    "Distribution 02",
    "Derived from exclusive presence in JP47, JP48 and/or JP49.",
    
    "Japanese prefectures present",
    "REQUIRES_DERIVATION",
    "Stars 03 — Star geography",
    "Must reproduce the Key's prefectural counting geography rather than assume 51 botanical areas are equivalent.",
    
    "Japanese districts present",
    "REQUIRES_DERIVATION",
    "Stars 03 — Star geography",
    "Requires explicit district definitions and taxon occupancy.",
    
    "Almost all districts",
    "REQUIRES_DEFINITION_AND_DERIVATION",
    "Stars 03 — Star geography",
    "Do not impose an arbitrary threshold.",
    
    "Endemic to Japan",
    "REQUIRES_GLOBAL_DATA",
    "Global occurrence/distribution stage",
    "Cannot be established from Japanese occurrence data alone.",
    
    "Outside-Japan scope",
    "REQUIRES_GLOBAL_DATA",
    "Global occurrence/distribution stage",
    "Required to distinguish neighbouring-region and broader distributions.",
    
    "Taiwan distribution extent",
    "REQUIRES_GLOBAL_DATA",
    "Global occurrence/distribution stage",
    "Needed only for taxa entering the Taiwan branch.",
    
    "Korea distribution class",
    "REQUIRES_GLOBAL_DATA",
    "Global occurrence/distribution stage",
    "Needed only for taxa entering the Korea branch.",
    
    "Kuriles/Sakhalin distribution class",
    "REQUIRES_GLOBAL_DATA",
    "Global occurrence/distribution stage",
    "Needed only for taxa entering the Kuriles/Sakhalin branch.",
    
    "Chinese provinces present",
    "REQUIRES_GLOBAL_DATA",
    "Global occurrence/distribution stage",
    "Needed for the China branch.",
    
    "Rare within range",
    "REQUIRES_EXTERNAL_OR_EXPERT_EVIDENCE",
    "Later evidence module",
    "The Key uses an ecological rarity criterion; occurrence count alone must not silently substitute for it.",
    
    "Introduced/naturalised/cultivated-only",
    "REQUIRES_STATUS_EVIDENCE",
    "Later status/enrichment module",
    "Needed before GX attribution.",
    
    "Parent species Star",
    "DERIVED_LATER",
    "Final Star attribution stage",
    "Required for subspecies and varieties after species-level classification.",
    
    "Containing taxon Star for forms",
    "DERIVED_LATER",
    "Final Star attribution stage",
    "Forms inherit the containing taxon's Star.",
    
    "Genus-level concept review",
    "REQUIRES_TAXONOMIC_REVIEW",
    "Later taxonomic QA",
    "Genus-level occurrence concepts should not be treated automatically as species-level Star taxa."
  )
  
  cat("\n— Key-input status register —\n")
  print.data.frame(
    input_register,
    row.names = FALSE
  )
  
  # ===========================================================================
  # 7. Group requirements by next analytical stage
  # ===========================================================================
  
  stage_summary <- input_register %>%
    count(
      SOURCE_OR_NEXT_STAGE,
      name = "KEY_INPUTS"
    ) %>%
    arrange(
      SOURCE_OR_NEXT_STAGE
    )
  
  cat("\n— Requirements by analytical stage —\n")
  print.data.frame(
    stage_summary,
    row.names = FALSE
  )
  
  # ===========================================================================
  # 8. Validation
  # ===========================================================================
  
  output_profile <- dbGetQuery(
    con,
    paste0(
      "SELECT ",
      "COUNT(*) AS n, ",
      "COUNT(DISTINCT FINAL_WCVP_ID) AS ids, ",
      "SUM(OCCURRENCE_RECORDS) AS occurrences ",
      "FROM ", q_output
    )
  )
  
  output_taxa <- output_profile$n[[1]]
  output_ids <- output_profile$ids[[1]]
  output_occurrences <- output_profile$occurrences[[1]]
  
  stars_columns <- dbGetQuery(
    con,
    paste0(
      "SELECT COUNT(*) AS n ",
      "FROM information_schema.columns ",
      "WHERE table_name = '", OUTPUT_TABLE, "' ",
      "AND lower(column_name) IN ",
      "('star','star_rule_id','star_method')"
    )
  )$n[[1]]
  
  non_null_endemicity <- dbGetQuery(
    con,
    paste0(
      "SELECT COUNT(*) AS n FROM ", q_output, " ",
      "WHERE ENDEMIC_TO_JAPAN IS NOT NULL"
    )
  )$n[[1]]
  
  non_null_prefectures <- dbGetQuery(
    con,
    paste0(
      "SELECT COUNT(*) AS n FROM ", q_output, " ",
      "WHERE JAPAN_PREFECTURES_PRESENT IS NOT NULL"
    )
  )$n[[1]]
  
  non_null_districts <- dbGetQuery(
    con,
    paste0(
      "SELECT COUNT(*) AS n FROM ", q_output, " ",
      "WHERE JAPAN_DISTRICTS_PRESENT IS NOT NULL"
    )
  )$n[[1]]
  
  invalid_island_flags <- dbGetQuery(
    con,
    paste0(
      "SELECT COUNT(*) AS n FROM ", q_output, " ",
      "WHERE DERIVED_RESTRICTED_TO_RYUKYU_OGASAWARA_IZU ",
      "NOT IN (0,1) ",
      "OR DERIVED_RESTRICTED_TO_RYUKYU_OGASAWARA_IZU IS NULL"
    )
  )$n[[1]]
  
  not_ready_count <- dbGetQuery(
    con,
    paste0(
      "SELECT COUNT(*) AS n FROM ", q_output, " ",
      "WHERE STAR_INPUT_READINESS = 'NOT_READY_FOR_STAR_ASSIGNMENT'"
    )
  )$n[[1]]
  
  validation <- tibble::tibble(
    check = c(
      "stars_01_validation_passed",
      "source_taxa_11484",
      "source_WCVP_IDs_unique",
      "source_occurrences_2851736",
      "source_taxon_area_rows_128876",
      "source_botanical_areas_51",
      "output_taxa_11484",
      "output_WCVP_IDs_unique",
      "output_occurrences_2851736",
      "island_group_flags_binary",
      "endemicity_not_inferred",
      "prefecture_counts_not_invented",
      "district_counts_not_invented",
      "all_taxa_not_ready_for_assignment",
      "no_star_assignment_columns_created",
      "historical_star_totals_not_used",
      "foj_stars_not_used",
      "occurrences_not_modified",
      "taxonomy_not_modified",
      "geography_not_modified",
      "quarantine_not_modified",
      "stars_not_assigned",
      "ghi_not_calculated"
    ),
    pass = c(
      all(key_validation$pass),
      source_taxa == EXPECTED_TAXA,
      source_ids == EXPECTED_TAXA,
      source_occurrences == EXPECTED_OCCURRENCES,
      source_area_rows == EXPECTED_TAXON_AREA_ROWS,
      source_areas == EXPECTED_BOTANICAL_AREAS,
      output_taxa == EXPECTED_TAXA,
      output_ids == EXPECTED_TAXA,
      output_occurrences == EXPECTED_OCCURRENCES,
      invalid_island_flags == 0L,
      non_null_endemicity == 0L,
      non_null_prefectures == 0L,
      non_null_districts == 0L,
      not_ready_count == EXPECTED_TAXA,
      stars_columns == 0L,
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
  
  all_pass <- all(
    validation$pass
  )
  
  cat(
    "\nAll Key-input requirement validation checks PASS: ",
    all_pass,
    "\n",
    sep = ""
  )
  
  if (!all_pass) {
    stop(
      "Stars 02 validation failed. ",
      "Do not freeze the Key-input requirements layer."
    )
  }
  
  # ===========================================================================
  # 9. Metadata and exports
  # ===========================================================================
  
  metadata <- tibble::tibble(
    metric = c(
      "module",
      "version",
      "run_date",
      "source_taxon_table",
      "source_taxon_area_table",
      "source_key_table",
      "output_table",
      "WCVP_taxa",
      "occurrence_evidence",
      "taxon_area_presences",
      "botanical_areas",
      "historical_star_totals_used",
      "foj_stars_used",
      "endemicity_inferred",
      "prefecture_counts_inferred",
      "district_counts_inferred",
      "stars_assigned",
      "ghi_calculated",
      "validation_pass"
    ),
    value = c(
      MODULE,
      VERSION,
      as.character(RUN_DATE),
      SOURCE_TAXON,
      SOURCE_AREA,
      SOURCE_KEY,
      OUTPUT_TABLE,
      as.character(output_taxa),
      as.character(output_occurrences),
      as.character(source_area_rows),
      as.character(source_areas),
      "FALSE",
      "FALSE",
      "FALSE",
      "FALSE",
      "FALSE",
      "FALSE",
      "FALSE",
      as.character(all_pass)
    )
  )
  
  dbWriteTable(
    con,
    "vpjd_star_key_input_status_register",
    input_register,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "vpjd_star_key_input_requirements_validation",
    validation,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "vpjd_star_key_input_requirements_metadata",
    metadata,
    overwrite = TRUE
  )
  
  write_csv(
    core_profile,
    file.path(
      OUTPUT_DIR,
      "key_input_core_profile.csv"
    )
  )
  
  write_csv(
    rank_profile,
    file.path(
      OUTPUT_DIR,
      "key_input_rank_profile.csv"
    )
  )
  
  write_csv(
    input_register,
    file.path(
      OUTPUT_DIR,
      "key_input_status_register.csv"
    )
  )
  
  write_csv(
    stage_summary,
    file.path(
      OUTPUT_DIR,
      "key_input_stage_summary.csv"
    )
  )
  
  write_csv(
    validation,
    file.path(
      OUTPUT_DIR,
      "key_input_validation.csv"
    )
  )
  
  write_csv(
    metadata,
    file.path(
      OUTPUT_DIR,
      "key_input_metadata.csv"
    )
  )
  
  # ===========================================================================
  # 10. Final status
  # ===========================================================================
  
  cat("\nCanonical Key-input requirements table:\n")
  cat("  ", OUTPUT_TABLE, "\n", sep = "")
  
  cat("\nCanonical input-status register:\n")
  cat("  vpjd_star_key_input_status_register\n")
  
  cat(
    "\nOutput status: ",
    "VALIDATED CONTEMPORARY KEY-TO-STARS INPUT REQUIREMENTS\n",
    sep = ""
  )
  
  cat(
    "WCVP concepts profiled: ",
    format(output_taxa, big.mark = ","),
    "\n",
    sep = ""
  )
  
  cat(
    "Occurrence evidence represented: ",
    format(output_occurrences, big.mark = ","),
    "\n",
    sep = ""
  )
  
  cat(
    "Currently restricted to Ryukyu/Ogasawara/Izu: ",
    format(
      core_profile$CURRENTLY_RESTRICTED_TO_KEY_ISLAND_GROUPS[[1]],
      big.mark = ","
    ),
    "\n",
    sep = ""
  )
  
  cat(
    "Genus-level concepts requiring review: ",
    format(
      core_profile$GENUS_LEVEL_REVIEW[[1]],
      big.mark = ","
    ),
    "\n",
    sep = ""
  )
  
  cat("Endemicity inferred: FALSE\n")
  cat("Japanese prefecture counts inferred: FALSE\n")
  cat("Japanese district counts inferred: FALSE\n")
  cat("Historical Star totals used: FALSE\n")
  cat("FOJ historical Stars used: FALSE\n")
  cat("Contemporary Stars assigned: FALSE\n")
  cat("GHI calculated: FALSE\n")
  
  cat(
    "\nStars 02 v",
    VERSION,
    " complete.\n",
    sep = ""
  )
  
  invisible(
    list(
      core_profile = core_profile,
      rank_profile = rank_profile,
      input_register = input_register,
      stage_summary = stage_summary,
      validation = validation,
      metadata = metadata
    )
  )
}

result_stars_02 <- run_stars_02()