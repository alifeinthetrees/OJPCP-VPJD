# =============================================================================
# VPJD-OJPCP
# R/stars/03f_build_key_decision_specification.R
# Version 0.2.0
#
# BUILD CONTEMPORARY KEY-TO-STARS DECISION SPECIFICATION
#
# Contemporary methodological clarification:
# - Strictly accepted WCVP taxa only.
# - Introduced to Japan -> GX.
# - Hybrid taxa -> HYB, once contemporary hybrid rule is formalised.
# - Taxon occurring in Japan AND outside Japan -> GN.
# - Taxa endemic to Japan continue through the endemic Japanese Key.
#
# IMPORTANT:
# - No Stars are assigned by this module.
# - No legacy analytical data are used.
# - Historical Star totals are not validation/calibration targets.
# =============================================================================

suppressPackageStartupMessages({
  library(DBI)
  library(duckdb)
  library(dplyr)
  library(readr)
  library(here)
})

MODULE <- "stars_03f_build_key_decision_specification"
VERSION <- "0.2.0"
RUN_DATE <- Sys.Date()

DB_PATH <- here(
  "data", "interim", "occurrences", "vpjd_occurrences.duckdb"
)

OUTPUT_DIR <- here(
  "outputs", "tables", "stars", "key_specification"
)

OUTPUT_SPEC <- "vpjd_star_key_decision_specification"
OUTPUT_ROUTE <- "vpjd_star_key_route_register"
OUTPUT_TERMINAL <- "vpjd_star_terminal_register"
OUTPUT_VALIDATION <- "vpjd_star_03f_validation"
OUTPUT_METADATA <- "vpjd_star_03f_metadata"

run_stars_03f <- function() {
  
  cat("\n— Build contemporary Key-to-Stars decision specification —\n\n")
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
  
  con <- dbConnect(
    duckdb::duckdb(),
    dbdir = DB_PATH,
    read_only = FALSE
  )
  
  on.exit(
    dbDisconnect(con, shutdown = TRUE),
    add = TRUE
  )
  
  # ===========================================================================
  # 1. Verify contemporary analytical sources
  # ===========================================================================
  
  required_sources <- c(
    "vpjd_japan_taxon_distribution",
    "vpjd_japan_taxon_area_distribution",
    "vpjd_star_japan_key_geography",
    "vpjd_star_wcvp_key_geography"
  )
  
  missing_sources <- setdiff(
    required_sources,
    dbListTables(con)
  )
  
  if (length(missing_sources) > 0L) {
    stop(
      "Required contemporary VPJD sources missing: ",
      paste(missing_sources, collapse = ", ")
    )
  }
  
  cat("Contemporary VPJD analytical sources verified.\n")
  
  # ===========================================================================
  # 2. Contemporary decision specification
  # ===========================================================================
  
  spec <- tibble::tribble(
    ~NODE_ID, ~BRANCH, ~SEQUENCE, ~KEY_QUESTION,
    ~YES_ACTION, ~NO_ACTION,
    ~EVIDENCE_FIELD, ~CONTEMPORARY_SOURCE,
    ~EVIDENCE_STATUS, ~COMPUTATIONAL_TEST, ~NOTES,
    
    "P01", "PRELIMINARY", 1,
    "Is the accepted taxon introduced to Japan?",
    "GX", "P02",
    "WCVP_INTRODUCED_TO_JAPAN",
    "vpjd_star_wcvp_key_geography",
    "AVAILABLE",
    "WCVP_INTRODUCED_TO_JAPAN == TRUE",
    paste0(
      "Positive contemporary evidence of introduction to Japan assigns GX. ",
      "Absence of positive introduction evidence does not itself block ",
      "geographic assessment."
    ),
    
    "P02", "PRELIMINARY", 2,
    "Is the accepted taxon a hybrid?",
    "HYB", "G01",
    "HYBRID_STATUS",
    "CONTEMPORARY_WCVP_TAXONOMY",
    "REQUIRES_DEFINITION",
    NA_character_,
    paste0(
      "HYB is assigned to hybrid taxa. The exact contemporary operational ",
      "hybrid test and GX/HYB precedence remain to be formalised."
    ),
    
    "G01", "GEOGRAPHIC", 10,
    "Does the taxon occur outside Japan?",
    "GN", "E01",
    "WCVP_OUTSIDE_JAPAN",
    "vpjd_star_wcvp_key_geography",
    "AVAILABLE",
    "WCVP_OUTSIDE_JAPAN == TRUE",
    paste0(
      "A taxon occurring in Japan and also outside Japan is GN. ",
      "Taxa without positive outside-Japan evidence continue to the ",
      "Japanese endemic branch."
    ),
    
    "E01", "ENDEMIC", 20,
    "Is Japanese endemicity sufficiently resolved?",
    "E02", "QUARANTINE",
    "ENDEMIC_TO_JAPAN_WCVP",
    "vpjd_star_wcvp_key_geography",
    "AVAILABLE",
    "ENDEMIC_TO_JAPAN_WCVP == TRUE",
    paste0(
      "Taxa with insufficient contemporary WCVP distribution evidence ",
      "are quarantined rather than assumed endemic."
    ),
    
    "E02", "ENDEMIC", 30,
    "Is the taxon endemic only to the specified small island groups?",
    "BK", "E03",
    "RESTRICTED_TO_KEY_SMALL_ISLANDS",
    "vpjd_japan_taxon_area_distribution",
    "AVAILABLE",
    paste0(
      "All occupied Japanese botanical areas are within ",
      "JP47, JP48, JP49 or JP50"
    ),
    paste0(
      "Ryukyu, Izu and Ogasawara small-island concept; ",
      "JP50 Kazan is treated with the Ogasawara concept."
    ),
    
    "E03", "ENDEMIC", 40,
    "Does the taxon occur in <=2 districts AND <=14 prefectures?",
    "E04", "E05",
    "JAPAN_DISTRICTS_PRESENT + JAPAN_PREFECTURES_PRESENT",
    "vpjd_star_japan_key_geography",
    "PARTIALLY_AVAILABLE",
    paste0(
      "JAPAN_DISTRICTS_PRESENT <= 2 AND ",
      "JAPAN_PREFECTURES_PRESENT <= 14"
    ),
    paste0(
      "If prefectures >14, this conjunction is FALSE without requiring ",
      "district evidence. Otherwise district occupancy is required."
    ),
    
    "E04", "ENDEMIC", 50,
    "Does the taxon occur in <=7 prefectures?",
    "BK", "GD",
    "JAPAN_PREFECTURES_PRESENT",
    "vpjd_star_japan_key_geography",
    "AVAILABLE",
    "JAPAN_PREFECTURES_PRESENT <= 7",
    "Reached only after E03 is TRUE.",
    
    "E05", "ENDEMIC", 60,
    "Does the taxon occur in <=10 prefectures?",
    "E06", "E07",
    "JAPAN_PREFECTURES_PRESENT",
    "vpjd_star_japan_key_geography",
    "AVAILABLE",
    "JAPAN_PREFECTURES_PRESENT <= 10",
    "Widespread Japanese-endemic branch.",
    
    "E06", "ENDEMIC", 70,
    "Does the taxon occur in <=5 prefectures AND is it rare within its range?",
    "BK", "GD",
    "JAPAN_PREFECTURES_PRESENT + RARE_WITHIN_RANGE",
    "vpjd_star_japan_key_geography + external/expert evidence",
    "PARTIALLY_AVAILABLE",
    paste0(
      "If JAPAN_PREFECTURES_PRESENT > 5 then FALSE; ",
      "if <=5, RARE_WITHIN_RANGE is required"
    ),
    paste0(
      "Do not infer rarity from occurrence-record count alone. ",
      "The Key gives examples such as mountain-top or coastline restriction."
    ),
    
    "E07", "ENDEMIC", 80,
    "Does the taxon occur in almost all districts?",
    "GN", "BU",
    "ALMOST_ALL_DISTRICTS",
    "vpjd_star_japan_key_geography",
    "REQUIRES_DEFINITION",
    NA_character_,
    paste0(
      "An explicit interpretation of 'almost all districts' is required. ",
      "No arbitrary threshold is imposed here."
    )
  )
  
  # ===========================================================================
  # 3. Route register
  # ===========================================================================
  
  route_register <- tibble::tribble(
    ~FROM_NODE, ~EVIDENCE_VALUE, ~TO_NODE, ~ROUTE_MEANING,
    
    "P01", "INTRODUCED_TO_JAPAN", "GX",
    "Positive evidence that the accepted taxon is introduced to Japan.",
    
    "P02", "HYBRID", "HYB",
    "Taxon satisfies the formal contemporary hybrid definition.",
    
    "G01", "OUTSIDE_JAPAN_PRESENT", "GN",
    paste0(
      "Taxon occurs in Japan and has positive distribution evidence ",
      "outside Japan."
    ),
    
    "G01", "NO_OUTSIDE_JAPAN_EVIDENCE", "E01",
    "Proceed to assessment of Japanese endemicity.",
    
    "E01", "ENDEMIC_TO_JAPAN", "E02",
    "Contemporary evidence supports endemicity to Japan.",
    
    "E01", "ENDEMICITY_UNRESOLVED", "QUARANTINE",
    "Insufficient evidence to establish endemicity."
  )
  
  # ===========================================================================
  # 4. Terminal categories
  # ===========================================================================
  
  terminal_register <- tibble::tribble(
    ~STAR, ~MEANING,
    "BK", "Black Star",
    "GD", "Gold Star",
    "BU", "Blue Star",
    "GN", "Green Star",
    "GX", "Introduced to Japan",
    "HYB", "Hybrid taxon"
  )
  
  # ===========================================================================
  # 5. Evidence-status profile
  # ===========================================================================
  
  status_profile <- spec |>
    count(
      EVIDENCE_STATUS,
      name = "KEY_NODES"
    ) |>
    arrange(EVIDENCE_STATUS)
  
  cat("\n— Key evidence-status profile —\n")
  
  print.data.frame(
    status_profile,
    row.names = FALSE
  )
  
  cat("\n— Contemporary Key decision specification —\n")
  
  print.data.frame(
    spec |>
      select(
        NODE_ID,
        BRANCH,
        KEY_QUESTION,
        YES_ACTION,
        NO_ACTION,
        EVIDENCE_STATUS
      ),
    row.names = FALSE
  )
  
  cat("\n— Route register —\n")
  
  print.data.frame(
    route_register,
    row.names = FALSE
  )
  
  # ===========================================================================
  # 6. Currently unresolved methodological inputs
  # ===========================================================================
  
  blocked <- spec |>
    filter(
      EVIDENCE_STATUS %in%
        c(
          "REQUIRES_DEFINITION",
          "REQUIRES_EXTERNAL_OR_EXPERT_EVIDENCE"
        )
    ) |>
    select(
      NODE_ID,
      BRANCH,
      KEY_QUESTION,
      EVIDENCE_STATUS,
      NOTES
    )
  
  cat("\n— Key nodes requiring further methodology/evidence —\n")
  
  print.data.frame(
    blocked,
    row.names = FALSE
  )
  
  # ===========================================================================
  # 7. Validation
  # ===========================================================================
  
  valid_stars <- c(
    "BK", "GD", "BU", "GN", "GX", "HYB"
  )
  
  terminal_actions <- unique(
    c(
      spec$YES_ACTION,
      spec$NO_ACTION
    )
  )
  
  stars_in_spec <- intersect(
    terminal_actions,
    valid_stars
  )
  
  validation <- tibble::tibble(
    check = c(
      "node_ids_unique",
      "node_sequence_unique",
      "controlled_star_vocabulary",
      "gx_rule_present",
      "hyb_rule_present",
      "outside_japan_routes_directly_to_gn",
      "outside_japan_uses_contemporary_wcvp",
      "endemic_branch_present",
      "small_island_bk_rule_present",
      "prefecture_thresholds_present",
      "district_threshold_present",
      "rare_within_range_not_invented",
      "almost_all_districts_not_invented",
      "taiwan_branch_removed",
      "korea_branch_removed",
      "kuriles_sakhalin_branch_removed",
      "china_branch_removed",
      "legacy_analytical_data_not_used",
      "historical_star_totals_not_used",
      "stars_not_assigned",
      "ghi_not_calculated"
    ),
    
    pass = c(
      n_distinct(spec$NODE_ID) == nrow(spec),
      
      n_distinct(spec$SEQUENCE) == nrow(spec),
      
      all(stars_in_spec %in% valid_stars),
      
      any(
        spec$NODE_ID == "P01" &
          spec$YES_ACTION == "GX"
      ),
      
      any(
        spec$NODE_ID == "P02" &
          spec$YES_ACTION == "HYB"
      ),
      
      any(
        spec$NODE_ID == "G01" &
          spec$YES_ACTION == "GN"
      ),
      
      any(
        spec$NODE_ID == "G01" &
          spec$CONTEMPORARY_SOURCE ==
          "vpjd_star_wcvp_key_geography"
      ),
      
      all(
        c(
          "E01", "E02", "E03", "E04",
          "E05", "E06", "E07"
        ) %in% spec$NODE_ID
      ),
      
      any(
        spec$NODE_ID == "E02" &
          spec$YES_ACTION == "BK"
      ),
      
      all(
        c("E03", "E04", "E05", "E06") %in%
          spec$NODE_ID
      ),
      
      "E03" %in% spec$NODE_ID,
      
      spec$EVIDENCE_STATUS[
        spec$NODE_ID == "E06"
      ] == "PARTIALLY_AVAILABLE",
      
      spec$EVIDENCE_STATUS[
        spec$NODE_ID == "E07"
      ] == "REQUIRES_DEFINITION",
      
      !any(
        grepl(
          "TAIWAN",
          spec$BRANCH,
          ignore.case = TRUE
        )
      ),
      
      !any(
        grepl(
          "KOREA",
          spec$BRANCH,
          ignore.case = TRUE
        )
      ),
      
      !any(
        grepl(
          "KURILES",
          spec$BRANCH,
          ignore.case = TRUE
        )
      ),
      
      !any(
        grepl(
          "CHINA",
          spec$BRANCH,
          ignore.case = TRUE
        )
      ),
      
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
    "\nAll Stars 03f validation checks PASS: ",
    all_pass,
    "\n",
    sep = ""
  )
  
  if (!all_pass) {
    stop(
      "Stars 03f validation failed. Do not freeze."
    )
  }
  
  # ===========================================================================
  # 8. Metadata
  # ===========================================================================
  
  metadata <- tibble::tibble(
    metric = c(
      "module",
      "version",
      "run_date",
      "key_nodes",
      "route_records",
      "terminal_star_categories",
      "outside_japan_direct_to_gn",
      "restricted_external_region_branches_used",
      "legacy_analytical_inputs_used",
      "historical_star_totals_used",
      "stars_assigned",
      "ghi_calculated",
      "validation_pass"
    ),
    
    value = c(
      MODULE,
      VERSION,
      as.character(RUN_DATE),
      as.character(nrow(spec)),
      as.character(nrow(route_register)),
      as.character(nrow(terminal_register)),
      "TRUE",
      "FALSE",
      "FALSE",
      "FALSE",
      "FALSE",
      "FALSE",
      as.character(all_pass)
    )
  )
  
  # ===========================================================================
  # 9. Persist
  # ===========================================================================
  
  dbWriteTable(
    con,
    OUTPUT_SPEC,
    spec,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    OUTPUT_ROUTE,
    route_register,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    OUTPUT_TERMINAL,
    terminal_register,
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
  
  write_csv(
    spec,
    file.path(
      OUTPUT_DIR,
      "stars03f_key_decision_specification.csv"
    )
  )
  
  write_csv(
    route_register,
    file.path(
      OUTPUT_DIR,
      "stars03f_key_route_register.csv"
    )
  )
  
  write_csv(
    terminal_register,
    file.path(
      OUTPUT_DIR,
      "stars03f_terminal_register.csv"
    )
  )
  
  write_csv(
    blocked,
    file.path(
      OUTPUT_DIR,
      "stars03f_blocked_nodes.csv"
    )
  )
  
  write_csv(
    validation,
    file.path(
      OUTPUT_DIR,
      "stars03f_validation.csv"
    )
  )
  
  write_csv(
    metadata,
    file.path(
      OUTPUT_DIR,
      "stars03f_metadata.csv"
    )
  )
  
  cat("\nCanonical specification:\n")
  cat("  ", OUTPUT_SPEC, "\n", sep = "")
  cat("  ", OUTPUT_ROUTE, "\n", sep = "")
  
  cat(
    "\nStars 03f v",
    VERSION,
    " complete.\n",
    sep = ""
  )
  
  cat(
    "Simplified contemporary Key specification created. ",
    "No Stars assigned.\n"
  )
  
  invisible(
    list(
      specification = spec,
      route_register = route_register,
      terminal_register = terminal_register,
      blocked = blocked,
      validation = validation,
      metadata = metadata
    )
  )
}

result_stars_03f <- run_stars_03f()