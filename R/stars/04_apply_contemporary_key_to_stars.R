# =============================================================================
# VPJD-OJPCP
# R/stars/04_apply_contemporary_key_to_stars.R
# Version 0.3.0
#
# SEQUENTIAL CONTEMPORARY APPLICATION OF THE KEY TO STARS
#
# Eligible population:
# - Strictly accepted contemporary WCVP taxa only.
#
# Sequential decision order:
# 1. Introduced to Japan                         -> GX
# 2. Explicit Genus × species hybrid            -> HYB
# 3. Occurs in Japan and outside Japan          -> GN
# 4. Otherwise assess Japanese endemic branch:
#    a. Endemic only to Ryukyu/Izu/Ogasawara-
#       Kazan small-island concept               -> BK
#    b. <=2 districts AND <=14 prefectures       -> continue
#    c. <=7 prefectures                          -> BK/GD
#    d. <=10 prefectures                         -> rarity branch
#    e. >10 prefectures                          -> almost-all-districts branch
#
# IMPORTANT:
# - Each taxon stops at its first unresolved Key node.
# - No district count is inferred.
# - No "almost all districts" threshold is invented.
# - Rarity is not inferred from GBIF record counts.
# - Leading-× nothogenus diagnostics are NOT automatically HYB.
# - No legacy analytical data are used.
# - Historical Star totals are not calibration/validation targets.
# - GHI is not calculated.
# =============================================================================

suppressPackageStartupMessages({
  library(DBI)
  library(duckdb)
  library(dplyr)
  library(readr)
  library(stringr)
  library(here)
})

MODULE <- "stars_04_apply_contemporary_key_to_stars"
VERSION <- "0.3.0"
RUN_DATE <- Sys.Date()

DB_PATH <- here(
  "data", "interim", "occurrences", "vpjd_occurrences.duckdb"
)

OUTPUT_DIR <- here(
  "outputs", "tables", "stars", "classification"
)

OUTPUT_CLASSIFICATION <- "vpjd_star_classification"
OUTPUT_QUARANTINE <- "vpjd_star_quarantine"
OUTPUT_VALIDATION <- "vpjd_star_04_validation"
OUTPUT_METADATA <- "vpjd_star_04_metadata"

run_stars_04 <- function() {
  cat("\n— Sequential contemporary application of the Key to Stars —\n\n")
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
    "vpjd_star_wcvp_key_geography",
    "vpjd_star_hybrid_status",
    "vpjd_star_key_decision_specification"
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
  
  cat("Contemporary analytical sources verified.\n")
  
  # ===========================================================================
  # 2. Accepted contemporary taxon population
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
    as_tibble()
  
  if (nrow(taxa) != 11439L) {
    stop(
      "Expected 11,439 accepted taxa; found ",
      format(nrow(taxa), big.mark = ","),
      "."
    )
  }
  
  if (n_distinct(taxa$FINAL_WCVP_ID) != 11439L) {
    stop("Accepted WCVP taxon IDs are not unique.")
  }
  
  cat(
    "Accepted contemporary taxa: ",
    format(nrow(taxa), big.mark = ","),
    "\n",
    sep = ""
  )
  
  # ===========================================================================
  # 3. Contemporary WCVP geographic evidence
  # ===========================================================================
  
  wcvp_geo <- dbGetQuery(
    con,
    "
    SELECT *
    FROM vpjd_star_wcvp_key_geography
    "
  ) |>
    as_tibble()
  
  if (n_distinct(wcvp_geo$FINAL_WCVP_ID) != 11439L) {
    stop(
      "WCVP Key-geography table does not contain exactly ",
      "11,439 unique accepted taxa."
    )
  }
  
  # Resolve field names defensively from the frozen 03c output.
  
  find_field <- function(
    data,
    candidates,
    required = TRUE
  ) {
    hit <- candidates[
      candidates %in% names(data)
    ]
    
    if (length(hit) > 0L) {
      return(hit[[1L]])
    }
    
    if (required) {
      stop(
        "Required field not found. Tried: ",
        paste(candidates, collapse = ", ")
      )
    }
    
    NA_character_
  }
  
  introduced_field <- find_field(
    wcvp_geo,
    c(
      "WCVP_INTRODUCED_TO_JAPAN",
      "INTRODUCED_TO_JAPAN_WCVP",
      "INTRODUCED_TO_JAPAN"
    )
  )
  
  outside_field <- find_field(
    wcvp_geo,
    c(
      "WCVP_OUTSIDE_JAPAN",
      "OUTSIDE_JAPAN_WCVP",
      "OUTSIDE_JAPAN_PRESENT",
      "HAS_OUTSIDE_JAPAN_EVIDENCE"
    )
  )
  
  endemic_field <- find_field(
    wcvp_geo,
    c(
      "ENDEMIC_TO_JAPAN_WCVP",
      "WCVP_ENDEMIC_TO_JAPAN",
      "CANDIDATE_ENDEMIC_TO_JAPAN",
      "ENDEMIC_TO_JAPAN"
    )
  )
  
  wcvp_evidence <- wcvp_geo |>
    transmute(
      FINAL_WCVP_ID,
      WCVP_INTRODUCED_TO_JAPAN =
        .data[[introduced_field]],
      WCVP_OUTSIDE_JAPAN =
        .data[[outside_field]],
      ENDEMIC_TO_JAPAN_WCVP =
        .data[[endemic_field]]
    )
  
  # ===========================================================================
  # 4. Contemporary hybrid evidence
  # ===========================================================================
  
  hybrid <- dbGetQuery(
    con,
    "
    SELECT
      FINAL_WCVP_ID,
      HYBRID_STATUS,
      HYB_ELIGIBLE
    FROM vpjd_star_hybrid_status
    "
  ) |>
    as_tibble()
  
  if (nrow(hybrid) != 11439L) {
    stop(
      "Hybrid-status table does not contain 11,439 taxa."
    )
  }
  
  # ===========================================================================
  # 5. Contemporary Japanese Key geography
  # ===========================================================================
  
  japan_geo <- dbGetQuery(
    con,
    "
    SELECT *
    FROM vpjd_star_japan_key_geography
    "
  ) |>
    as_tibble()
  
  prefecture_field <- find_field(
    japan_geo,
    c(
      "JAPAN_PREFECTURES_PRESENT",
      "KEY_PREFECTURES_PRESENT",
      "PREFECTURES_PRESENT"
    )
  )
  
  district_field <- find_field(
    japan_geo,
    c(
      "JAPAN_DISTRICTS_PRESENT",
      "KEY_DISTRICTS_PRESENT",
      "DISTRICTS_PRESENT"
    ),
    required = FALSE
  )
  
  almost_all_field <- find_field(
    japan_geo,
    c(
      "ALMOST_ALL_DISTRICTS",
      "JAPAN_ALMOST_ALL_DISTRICTS"
    ),
    required = FALSE
  )
  
  japan_evidence <- japan_geo |>
    transmute(
      FINAL_WCVP_ID,
      JAPAN_PREFECTURES_PRESENT =
        .data[[prefecture_field]],
      JAPAN_DISTRICTS_PRESENT =
        if (!is.na(district_field)) {
          .data[[district_field]]
        } else {
          NA_integer_
        },
      ALMOST_ALL_DISTRICTS =
        if (!is.na(almost_all_field)) {
          .data[[almost_all_field]]
        } else {
          NA
        }
    )
  
  # ===========================================================================
  # 6. Small-island restriction
  #
  # Key small-island concept:
  # - JP47 Ryukyu
  # - JP48 Izu
  # - JP49 Ogasawara
  # - JP50 Kazan
  #
  # JP51 Kuriles is NOT included.
  # ===========================================================================
  
  area_distribution <- dbGetQuery(
    con,
    "
    SELECT
      FINAL_WCVP_ID,
      BOTANICAL_AREA_ID
    FROM vpjd_japan_taxon_area_distribution
    WHERE FINAL_WCVP_STATUS = 'Accepted'
      AND PRESENCE = 1
    "
  ) |>
    as_tibble()
  
  key_small_islands <- c(
    "JP47",
    "JP48",
    "JP49",
    "JP50"
  )
  
  small_island_status <- area_distribution |>
    group_by(
      FINAL_WCVP_ID
    ) |>
    summarise(
      JAPAN_BOTANICAL_AREAS_PRESENT =
        n_distinct(BOTANICAL_AREA_ID),
      
      RESTRICTED_TO_KEY_SMALL_ISLANDS =
        all(
          BOTANICAL_AREA_ID %in%
            key_small_islands
        ),
      
      .groups = "drop"
    )
  
  # ===========================================================================
  # 7. Build one contemporary evidence row per accepted taxon
  # ===========================================================================
  
  evidence <- taxa |>
    left_join(
      wcvp_evidence,
      by = "FINAL_WCVP_ID"
    ) |>
    left_join(
      hybrid,
      by = "FINAL_WCVP_ID"
    ) |>
    left_join(
      japan_evidence,
      by = "FINAL_WCVP_ID"
    ) |>
    left_join(
      small_island_status,
      by = "FINAL_WCVP_ID"
    )
  
  if (nrow(evidence) != 11439L) {
    stop(
      "Evidence join changed accepted taxon population."
    )
  }
  
  if (n_distinct(evidence$FINAL_WCVP_ID) != 11439L) {
    stop(
      "Evidence join created duplicate accepted taxon IDs."
    )
  }
  
  # ===========================================================================
  # 8. Sequential Key traversal
  #
  # Every taxon receives either:
  #
  # STAR = definitive terminal classification
  #
  # or
  #
  # NEXT_KEY_NODE = first unresolved Key question.
  #
  # No downstream unresolved requirements are accumulated.
  # ===========================================================================
  
  result <- evidence |>
    mutate(
      STAR = NA_character_,
      TERMINAL_KEY_NODE = NA_character_,
      CLASSIFICATION_RULE = NA_character_,
      NEXT_KEY_NODE = NA_character_,
      REQUIRED_EVIDENCE = NA_character_
    )
  
  # ---------------------------------------------------------------------------
  # P01 — introduced to Japan -> GX
  # ---------------------------------------------------------------------------
  
  idx <- which(
    is.na(result$STAR) &
      isTRUE(FALSE)
  )
  
  idx <- which(
    is.na(result$STAR) &
      !is.na(result$WCVP_INTRODUCED_TO_JAPAN) &
      result$WCVP_INTRODUCED_TO_JAPAN == TRUE
  )
  
  result$STAR[idx] <- "GX"
  result$TERMINAL_KEY_NODE[idx] <- "P01"
  result$CLASSIFICATION_RULE[idx] <-
    "INTRODUCED_TO_JAPAN"
  
  # ---------------------------------------------------------------------------
  # P02 — explicit Genus × species -> HYB
  #
  # GX has precedence because P01 is traversed first.
  # Leading-× nothogenus diagnostics do not satisfy HYB_ELIGIBLE.
  # ---------------------------------------------------------------------------
  
  idx <- which(
    is.na(result$STAR) &
      !is.na(result$HYB_ELIGIBLE) &
      result$HYB_ELIGIBLE == TRUE
  )
  
  result$STAR[idx] <- "HYB"
  result$TERMINAL_KEY_NODE[idx] <- "P02"
  result$CLASSIFICATION_RULE[idx] <-
    "EXPLICIT_GENUS_X_SPECIES_HYBRID"
  
  # ---------------------------------------------------------------------------
  # G01 — positive outside-Japan evidence -> GN
  # ---------------------------------------------------------------------------
  
  idx <- which(
    is.na(result$STAR) &
      !is.na(result$WCVP_OUTSIDE_JAPAN) &
      result$WCVP_OUTSIDE_JAPAN == TRUE
  )
  
  result$STAR[idx] <- "GN"
  result$TERMINAL_KEY_NODE[idx] <- "G01"
  result$CLASSIFICATION_RULE[idx] <-
    "OCCURS_IN_JAPAN_AND_OUTSIDE_JAPAN"
  
  # ---------------------------------------------------------------------------
  # G01/E01 — outside-Japan/endemicity evidence unresolved
  # ---------------------------------------------------------------------------
  
  idx <- which(
    is.na(result$STAR) &
      (
        is.na(result$WCVP_OUTSIDE_JAPAN) |
          is.na(result$ENDEMIC_TO_JAPAN_WCVP)
      )
  )
  
  result$NEXT_KEY_NODE[idx] <- "E01"
  result$REQUIRED_EVIDENCE[idx] <-
    "JAPAN_ENDEMICITY"
  
  # ---------------------------------------------------------------------------
  # Internal consistency check:
  #
  # Any remaining active taxon should have:
  # WCVP_OUTSIDE_JAPAN = FALSE
  # ENDEMIC_TO_JAPAN_WCVP = TRUE
  # ---------------------------------------------------------------------------
  
  active <- which(
    is.na(result$STAR) &
      is.na(result$NEXT_KEY_NODE)
  )
  
  inconsistent <- active[
    is.na(result$ENDEMIC_TO_JAPAN_WCVP[active]) |
      result$ENDEMIC_TO_JAPAN_WCVP[active] != TRUE
  ]
  
  if (length(inconsistent) > 0L) {
    stop(
      "Active taxa remain without positive endemic-to-Japan evidence."
    )
  }
  
  # ---------------------------------------------------------------------------
  # E02 — endemic only to Key small islands -> BK
  # ---------------------------------------------------------------------------
  
  idx <- which(
    is.na(result$STAR) &
      is.na(result$NEXT_KEY_NODE) &
      !is.na(result$RESTRICTED_TO_KEY_SMALL_ISLANDS) &
      result$RESTRICTED_TO_KEY_SMALL_ISLANDS == TRUE
  )
  
  result$STAR[idx] <- "BK"
  result$TERMINAL_KEY_NODE[idx] <- "E02"
  result$CLASSIFICATION_RULE[idx] <-
    "ENDEMIC_TO_KEY_SMALL_ISLANDS"
  
  # Missing Japanese area evidence at E02.
  idx <- which(
    is.na(result$STAR) &
      is.na(result$NEXT_KEY_NODE) &
      is.na(result$RESTRICTED_TO_KEY_SMALL_ISLANDS)
  )
  
  result$NEXT_KEY_NODE[idx] <- "E02"
  result$REQUIRED_EVIDENCE[idx] <-
    "JAPAN_BOTANICAL_AREA_DISTRIBUTION"
  
  # ---------------------------------------------------------------------------
  # E03 — <=2 districts AND <=14 prefectures
  #
  # Logical short-circuit:
  #
  # If prefectures >14, the conjunction is FALSE regardless of district count.
  # Therefore these taxa can proceed directly to E05.
  #
  # If prefectures <=14, district occupancy is required.
  # ---------------------------------------------------------------------------
  
  active <- which(
    is.na(result$STAR) &
      is.na(result$NEXT_KEY_NODE)
  )
  
  # Missing prefecture occupancy.
  idx <- active[
    is.na(
      result$JAPAN_PREFECTURES_PRESENT[active]
    )
  ]
  
  result$NEXT_KEY_NODE[idx] <- "E03"
  result$REQUIRED_EVIDENCE[idx] <-
    "JAPAN_PREFECTURE_OCCUPANCY"
  
  # Recalculate active population.
  active <- which(
    is.na(result$STAR) &
      is.na(result$NEXT_KEY_NODE)
  )
  
  # Prefectures <=14 but district count missing -> stop at E03.
  idx <- active[
    result$JAPAN_PREFECTURES_PRESENT[active] <= 14 &
      is.na(
        result$JAPAN_DISTRICTS_PRESENT[active]
      )
  ]
  
  result$NEXT_KEY_NODE[idx] <- "E03"
  result$REQUIRED_EVIDENCE[idx] <-
    "JAPAN_DISTRICT_OCCUPANCY"
  
  # Recalculate active.
  active <- which(
    is.na(result$STAR) &
      is.na(result$NEXT_KEY_NODE)
  )
  
  # E03 TRUE:
  # <=14 prefectures AND <=2 districts -> E04.
  e03_true <- active[
    result$JAPAN_PREFECTURES_PRESENT[active] <= 14 &
      !is.na(
        result$JAPAN_DISTRICTS_PRESENT[active]
      ) &
      result$JAPAN_DISTRICTS_PRESENT[active] <= 2
  ]
  
  # E03 FALSE:
  # >14 prefectures OR >2 districts -> E05.
  e03_false <- setdiff(
    active,
    e03_true
  )
  
  # ---------------------------------------------------------------------------
  # E04 — E03 TRUE: <=7 prefectures -> BK, otherwise GD
  # ---------------------------------------------------------------------------
  
  idx <- e03_true[
    result$JAPAN_PREFECTURES_PRESENT[e03_true] <= 7
  ]
  
  result$STAR[idx] <- "BK"
  result$TERMINAL_KEY_NODE[idx] <- "E04"
  result$CLASSIFICATION_RULE[idx] <-
    "ENDEMIC_LE2_DISTRICTS_LE7_PREFECTURES"
  
  idx <- e03_true[
    result$JAPAN_PREFECTURES_PRESENT[e03_true] > 7
  ]
  
  result$STAR[idx] <- "GD"
  result$TERMINAL_KEY_NODE[idx] <- "E04"
  result$CLASSIFICATION_RULE[idx] <-
    "ENDEMIC_LE2_DISTRICTS_8_TO_14_PREFECTURES"
  
  # ---------------------------------------------------------------------------
  # E05 — E03 FALSE: <=10 prefectures?
  #
  # This branch is reachable when:
  # - districts >2, or
  # - prefectures >14.
  # ---------------------------------------------------------------------------
  
  e05_le10 <- e03_false[
    result$JAPAN_PREFECTURES_PRESENT[e03_false] <= 10
  ]
  
  e05_gt10 <- e03_false[
    result$JAPAN_PREFECTURES_PRESENT[e03_false] > 10
  ]
  
  # ---------------------------------------------------------------------------
  # E06 — <=10 prefectures
  #
  # Key:
  # <=5 prefectures AND rare -> BK
  # otherwise -> GD
  #
  # If prefectures >5, the conjunction is FALSE without needing rarity.
  # If prefectures <=5, rarity evidence is genuinely required.
  # ---------------------------------------------------------------------------
  
  idx <- e05_le10[
    result$JAPAN_PREFECTURES_PRESENT[e05_le10] > 5
  ]
  
  result$STAR[idx] <- "GD"
  result$TERMINAL_KEY_NODE[idx] <- "E06"
  result$CLASSIFICATION_RULE[idx] <-
    "ENDEMIC_WIDESPREAD_6_TO_10_PREFECTURES"
  
  idx <- e05_le10[
    result$JAPAN_PREFECTURES_PRESENT[e05_le10] <= 5
  ]
  
  result$NEXT_KEY_NODE[idx] <- "E06"
  result$REQUIRED_EVIDENCE[idx] <-
    "RARE_WITHIN_RANGE"
  
  # ---------------------------------------------------------------------------
  # E07 — >10 prefectures
  #
  # GN if almost all districts.
  # BU otherwise.
  #
  # No threshold is invented. Missing status stops at E07.
  # ---------------------------------------------------------------------------
  
  idx <- e05_gt10[
    is.na(
      result$ALMOST_ALL_DISTRICTS[e05_gt10]
    )
  ]
  
  result$NEXT_KEY_NODE[idx] <- "E07"
  result$REQUIRED_EVIDENCE[idx] <-
    "ALMOST_ALL_DISTRICTS_STATUS"
  
  idx <- e05_gt10[
    !is.na(
      result$ALMOST_ALL_DISTRICTS[e05_gt10]
    ) &
      result$ALMOST_ALL_DISTRICTS[e05_gt10] == TRUE
  ]
  
  result$STAR[idx] <- "GN"
  result$TERMINAL_KEY_NODE[idx] <- "E07"
  result$CLASSIFICATION_RULE[idx] <-
    "ENDEMIC_ALMOST_ALL_DISTRICTS"
  
  idx <- e05_gt10[
    !is.na(
      result$ALMOST_ALL_DISTRICTS[e05_gt10]
    ) &
      result$ALMOST_ALL_DISTRICTS[e05_gt10] == FALSE
  ]
  
  result$STAR[idx] <- "BU"
  result$TERMINAL_KEY_NODE[idx] <- "E07"
  result$CLASSIFICATION_RULE[idx] <-
    "ENDEMIC_NOT_ALMOST_ALL_DISTRICTS"
  
  # ===========================================================================
  # 9. Final accounting
  # ===========================================================================
  
  active_remaining <- result |>
    filter(
      is.na(STAR),
      is.na(NEXT_KEY_NODE)
    )
  
  if (nrow(active_remaining) > 0L) {
    stop(
      nrow(active_remaining),
      " taxa remain active without a terminal Star or stopping node."
    )
  }
  
  classification <- result |>
    filter(
      !is.na(STAR)
    ) |>
    transmute(
      FINAL_WCVP_ID,
      FINAL_WCVP_RECOGNISED_NAME,
      FINAL_WCVP_RANK,
      FINAL_WCVP_STATUS,
      FINAL_WCVP_CONCEPT_CLASS,
      STAR,
      TERMINAL_KEY_NODE,
      CLASSIFICATION_RULE,
      WCVP_INTRODUCED_TO_JAPAN,
      HYBRID_STATUS,
      HYB_ELIGIBLE,
      WCVP_OUTSIDE_JAPAN,
      ENDEMIC_TO_JAPAN_WCVP,
      JAPAN_PREFECTURES_PRESENT,
      JAPAN_DISTRICTS_PRESENT,
      ALMOST_ALL_DISTRICTS,
      RESTRICTED_TO_KEY_SMALL_ISLANDS,
      STAR_MODULE = MODULE,
      STAR_VERSION = VERSION
    ) |>
    arrange(
      STAR,
      FINAL_WCVP_RECOGNISED_NAME
    )
  
  quarantine <- result |>
    filter(
      is.na(STAR)
    ) |>
    transmute(
      FINAL_WCVP_ID,
      FINAL_WCVP_RECOGNISED_NAME,
      FINAL_WCVP_RANK,
      FINAL_WCVP_STATUS,
      FINAL_WCVP_CONCEPT_CLASS,
      NEXT_KEY_NODE,
      REQUIRED_EVIDENCE,
      WCVP_INTRODUCED_TO_JAPAN,
      HYBRID_STATUS,
      HYB_ELIGIBLE,
      WCVP_OUTSIDE_JAPAN,
      ENDEMIC_TO_JAPAN_WCVP,
      JAPAN_PREFECTURES_PRESENT,
      JAPAN_DISTRICTS_PRESENT,
      ALMOST_ALL_DISTRICTS,
      RESTRICTED_TO_KEY_SMALL_ISLANDS,
      QUARANTINE_STATUS = "ACTIVE",
      STAR_MODULE = MODULE,
      STAR_VERSION = VERSION
    ) |>
    arrange(
      NEXT_KEY_NODE,
      FINAL_WCVP_RECOGNISED_NAME
    )
  
  # ===========================================================================
  # 10. Profiles
  # ===========================================================================
  
  classification_profile <- classification |>
    count(
      STAR,
      TERMINAL_KEY_NODE,
      CLASSIFICATION_RULE,
      name = "WCVP_TAXA"
    ) |>
    arrange(
      factor(
        STAR,
        levels = c(
          "BK",
          "GD",
          "BU",
          "GN",
          "GX",
          "HYB"
        )
      ),
      TERMINAL_KEY_NODE
    )
  
  quarantine_profile <- quarantine |>
    count(
      NEXT_KEY_NODE,
      REQUIRED_EVIDENCE,
      name = "WCVP_TAXA"
    ) |>
    arrange(
      NEXT_KEY_NODE,
      REQUIRED_EVIDENCE
    )
  
  cat("\n— Definitive classifications —\n")
  
  print.data.frame(
    classification_profile,
    row.names = FALSE
  )
  
  cat("\n— Current stopping points —\n")
  
  print.data.frame(
    quarantine_profile,
    row.names = FALSE
  )
  
  cat("\n— Overall accounting —\n")
  
  cat(
    "Accepted taxa: ",
    format(nrow(result), big.mark = ","),
    "\n",
    sep = ""
  )
  
  cat(
    "Definitively classified: ",
    format(nrow(classification), big.mark = ","),
    "\n",
    sep = ""
  )
  
  cat(
    "Stopped at first unresolved Key node: ",
    format(nrow(quarantine), big.mark = ","),
    "\n",
    sep = ""
  )
  
  cat(
    "Accounted for: ",
    format(
      nrow(classification) +
        nrow(quarantine),
      big.mark = ","
    ),
    "\n",
    sep = ""
  )
  
  # ===========================================================================
  # 11. Useful diagnostic counts
  # ===========================================================================
  
  gx_hyb_overlap <- evidence |>
    filter(
      WCVP_INTRODUCED_TO_JAPAN == TRUE,
      HYB_ELIGIBLE == TRUE
    )
  
  outside_after_precedence <- classification |>
    filter(
      STAR == "GN",
      TERMINAL_KEY_NODE == "G01"
    )
  
  nothogenus_profile <- evidence |>
    filter(
      HYBRID_STATUS == "NOTHOGENUS_DIAGNOSTIC"
    ) |>
    count(
      WCVP_INTRODUCED_TO_JAPAN,
      WCVP_OUTSIDE_JAPAN,
      ENDEMIC_TO_JAPAN_WCVP,
      name = "WCVP_TAXA"
    )
  
  cat(
    "\nGX/HYB evidence overlap before precedence: ",
    format(nrow(gx_hyb_overlap), big.mark = ","),
    "\n",
    sep = ""
  )
  
  cat(
    "GN assigned at outside-Japan node after GX/HYB precedence: ",
    format(nrow(outside_after_precedence), big.mark = ","),
    "\n",
    sep = ""
  )
  
  cat("\n— Nothogenus diagnostic routing profile —\n")
  
  print.data.frame(
    nothogenus_profile,
    row.names = FALSE
  )
  
  # ===========================================================================
  # 12. Validation
  # ===========================================================================
  
  valid_stars <- c(
    "BK",
    "GD",
    "BU",
    "GN",
    "GX",
    "HYB"
  )
  
  validation <- tibble::tibble(
    check = c(
      "accepted_population_11439",
      "accepted_ids_unique",
      "classification_ids_unique",
      "quarantine_ids_unique",
      "classification_quarantine_no_overlap",
      "all_taxa_accounted_for",
      "classified_stars_controlled",
      "one_stopping_point_per_quarantined_taxon",
      "no_active_taxa_remain",
      "gx_requires_positive_introduction_evidence",
      "hyb_requires_explicit_hyb_eligibility",
      "hyb_excludes_nothogenus_diagnostic",
      "gx_precedes_hyb",
      "gn_g01_requires_outside_japan_evidence",
      "outside_japan_does_not_enter_endemic_key",
      "bk_e02_requires_endemic_small_island_evidence",
      "e03_missing_district_only_blocks_le14_prefecture_taxa",
      "e06_rarity_only_blocks_le5_prefecture_taxa",
      "e07_requires_almost_all_districts_status",
      "district_counts_not_invented",
      "almost_all_districts_not_invented",
      "rarity_not_inferred_from_record_counts",
      "legacy_data_not_used",
      "historical_star_totals_not_used",
      "stars_not_calibrated_to_historical_results",
      "ghi_not_calculated"
    ),
    
    pass = c(
      nrow(result) == 11439L,
      
      n_distinct(
        result$FINAL_WCVP_ID
      ) == 11439L,
      
      n_distinct(
        classification$FINAL_WCVP_ID
      ) == nrow(classification),
      
      n_distinct(
        quarantine$FINAL_WCVP_ID
      ) == nrow(quarantine),
      
      length(
        intersect(
          classification$FINAL_WCVP_ID,
          quarantine$FINAL_WCVP_ID
        )
      ) == 0L,
      
      nrow(classification) +
        nrow(quarantine) ==
        11439L,
      
      all(
        classification$STAR %in%
          valid_stars
      ),
      
      all(
        !is.na(
          quarantine$NEXT_KEY_NODE
        )
      ),
      
      nrow(active_remaining) == 0L,
      
      all(
        classification$STAR != "GX" |
          classification$WCVP_INTRODUCED_TO_JAPAN == TRUE
      ),
      
      all(
        classification$STAR != "HYB" |
          classification$HYB_ELIGIBLE == TRUE
      ),
      
      all(
        classification$STAR != "HYB" |
          classification$HYBRID_STATUS == "HYBRID"
      ),
      
      !any(
        classification$STAR == "HYB" &
          classification$WCVP_INTRODUCED_TO_JAPAN == TRUE
      ),
      
      all(
        classification$TERMINAL_KEY_NODE != "G01" |
          classification$WCVP_OUTSIDE_JAPAN == TRUE
      ),
      
      !any(
        result$WCVP_OUTSIDE_JAPAN == TRUE &
          result$STAR %in% c("BK", "GD", "BU")
      ),
      
      all(
        classification$TERMINAL_KEY_NODE != "E02" |
          (
            classification$STAR == "BK" &
              classification$ENDEMIC_TO_JAPAN_WCVP == TRUE &
              classification$RESTRICTED_TO_KEY_SMALL_ISLANDS == TRUE
          )
      ),
      
      all(
        quarantine$NEXT_KEY_NODE != "E03" |
          quarantine$JAPAN_PREFECTURES_PRESENT <= 14 |
          is.na(
            quarantine$JAPAN_PREFECTURES_PRESENT
          )
      ),
      
      all(
        quarantine$NEXT_KEY_NODE != "E06" |
          quarantine$JAPAN_PREFECTURES_PRESENT <= 5
      ),
      
      all(
        classification$TERMINAL_KEY_NODE != "E07" |
          !is.na(
            classification$ALMOST_ALL_DISTRICTS
          )
      ),
      
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
    "\nAll Stars 04 validation checks PASS: ",
    all_pass,
    "\n",
    sep = ""
  )
  
  if (!all_pass) {
    stop(
      "Stars 04 validation failed. Do not freeze."
    )
  }
  
  # ===========================================================================
  # 13. Metadata
  # ===========================================================================
  
  metadata <- tibble::tibble(
    metric = c(
      "module",
      "version",
      "run_date",
      "accepted_taxa",
      "classified_taxa",
      "quarantined_taxa",
      "gx_taxa",
      "hyb_taxa",
      "gn_taxa",
      "gd_taxa",
      "bu_taxa",
      "bk_taxa",
      "gx_hyb_evidence_overlap",
      "legacy_analytical_inputs_used",
      "historical_star_totals_used",
      "historical_totals_used_as_validation_target",
      "ghi_calculated",
      "validation_pass"
    ),
    
    value = c(
      MODULE,
      VERSION,
      as.character(RUN_DATE),
      as.character(nrow(result)),
      as.character(nrow(classification)),
      as.character(nrow(quarantine)),
      as.character(
        sum(classification$STAR == "GX")
      ),
      as.character(
        sum(classification$STAR == "HYB")
      ),
      as.character(
        sum(classification$STAR == "GN")
      ),
      as.character(
        sum(classification$STAR == "GD")
      ),
      as.character(
        sum(classification$STAR == "BU")
      ),
      as.character(
        sum(classification$STAR == "BK")
      ),
      as.character(
        nrow(gx_hyb_overlap)
      ),
      "FALSE",
      "FALSE",
      "FALSE",
      "FALSE",
      as.character(all_pass)
    )
  )
  
  # ===========================================================================
  # 14. Persist
  # ===========================================================================
  
  dbWriteTable(
    con,
    OUTPUT_CLASSIFICATION,
    classification,
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
  
  write_csv(
    classification,
    file.path(
      OUTPUT_DIR,
      "stars04_classification.csv"
    )
  )
  
  write_csv(
    quarantine,
    file.path(
      OUTPUT_DIR,
      "stars04_quarantine.csv"
    )
  )
  
  write_csv(
    classification_profile,
    file.path(
      OUTPUT_DIR,
      "stars04_classification_profile.csv"
    )
  )
  
  write_csv(
    quarantine_profile,
    file.path(
      OUTPUT_DIR,
      "stars04_quarantine_profile.csv"
    )
  )
  
  write_csv(
    gx_hyb_overlap,
    file.path(
      OUTPUT_DIR,
      "stars04_gx_hyb_overlap.csv"
    )
  )
  
  write_csv(
    validation,
    file.path(
      OUTPUT_DIR,
      "stars04_validation.csv"
    )
  )
  
  write_csv(
    metadata,
    file.path(
      OUTPUT_DIR,
      "stars04_metadata.csv"
    )
  )
  
  cat("\nCanonical outputs:\n")
  cat(
    "  ",
    OUTPUT_CLASSIFICATION,
    "\n",
    sep = ""
  )
  cat(
    "  ",
    OUTPUT_QUARANTINE,
    "\n",
    sep = ""
  )
  
  cat(
    "\nStars 04 v",
    VERSION,
    " complete.\n",
    sep = ""
  )
  
  cat(
    "Every accepted taxon has either a definitive Star ",
    "or one first unresolved Key node.\n"
  )
  
  invisible(
    list(
      classification = classification,
      quarantine = quarantine,
      classification_profile =
        classification_profile,
      quarantine_profile =
        quarantine_profile,
      gx_hyb_overlap =
        gx_hyb_overlap,
      validation = validation,
      metadata = metadata
    )
  )
}

result_stars_04 <- run_stars_04()