# =============================================================================
# VPJD-OJPCP
# R/stars/03i_build_methodological_decision_register.R
# Version 0.1.0
#
# BUILD METHODOLOGICAL DECISION REGISTER FOR THE KEY TO STARS
#
# PURPOSE
# - Create a permanent, auditable register of methodological decisions required
#   to implement the published Key to Stars.
# - Distinguish source wording, interpretation, operational definition,
#   evidence source and implementation status.
# - Prevent unresolved methodological assumptions entering the classifier.
# - Support reproducibility, publication and future reruns of the VPJD.
#
# STATUS VOCABULARY
# - UNRESOLVED:
#     Original criterion identified but operational definition not established.
# - PROVISIONAL:
#     Proposed interpretation exists but is not yet sufficiently supported.
# - METHOD_VALIDATED:
#     Operational definition is supported and suitable for implementation.
# - FROZEN:
#     Method-validated definition has been implemented and validated.
#
# IMPORTANT
# - This module DOES NOT assign Stars.
# - It DOES NOT assign Japanese districts.
# - It DOES NOT resolve ambiguous Key terminology.
# - It DOES NOT use historical Star totals as calibration targets.
# - It DOES NOT use legacy analytical datasets.
# - Figure 2.2 is the primary Key specification currently available.
# =============================================================================

suppressPackageStartupMessages({
  library(DBI)
  library(duckdb)
  library(dplyr)
  library(readr)
  library(here)
  library(tibble)
})

MODULE <- "stars_03i_build_methodological_decision_register"
VERSION <- "0.1.0"
RUN_DATE <- Sys.Date()

DB_PATH <- here("data", "interim", "occurrences", "vpjd_occurrences.duckdb")
OUTPUT_DIR <- here("outputs", "tables", "stars", "methodology")

OUTPUT_REGISTER <- "vpjd_star_methodological_decision_register"
OUTPUT_STATUS <- "vpjd_star_methodological_status"
OUTPUT_VALIDATION <- "vpjd_star_03i_validation"
OUTPUT_METADATA <- "vpjd_star_03i_metadata"

run_stars_03i <- function() {
  cat("\n— Build methodological decision register —\n\n")
  cat("Run date: ", as.character(RUN_DATE), "\n", sep = "")
  cat("Module: ", MODULE, "\n", sep = "")
  cat("Version: ", VERSION, "\n\n", sep = "")
  
  dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
  
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
  
  required_tables <- c(
    "vpjd_star_district_requirement_taxa",
    "vpjd_star_district_requirement_profile"
  )
  
  missing_tables <- setdiff(required_tables, dbListTables(con))
  
  if (length(missing_tables) > 0L) {
    stop(
      "Required Stars 03h tables missing: ",
      paste(missing_tables, collapse = ", ")
    )
  }
  
  # ===========================================================================
  # 1. Confirm contemporary population inherited from 03h
  # ===========================================================================
  
  taxa <- dbGetQuery(
    con,
    "
    SELECT *
    FROM vpjd_star_district_requirement_taxa
    "
  ) |>
    as_tibble()
  
  if (nrow(taxa) != 11439L) {
    stop(
      "Expected 11,439 accepted taxa; found ",
      format(nrow(taxa), big.mark = ",")
    )
  }
  
  if (n_distinct(taxa$FINAL_WCVP_ID) != 11439L) {
    stop("Accepted WCVP IDs are not unique.")
  }
  
  population <- taxa |>
    count(
      DISTRICT_REQUIREMENT_CLASS,
      name = "WCVP_TAXA"
    )
  
  get_count <- function(class) {
    x <- population |>
      filter(DISTRICT_REQUIREMENT_CLASS == class) |>
      pull(WCVP_TAXA)
    
    if (length(x) == 0L) {
      return(0L)
    }
    
    as.integer(x[[1L]])
  }
  
  n_endemic <- get_count("ENDEMIC_KEY")
  n_nonendemic <- get_count("NON_ENDEMIC_KEY")
  n_gx <- get_count("SPECIAL_STATUS_GX")
  n_hyb <- get_count("SPECIAL_STATUS_HYB")
  n_unresolved <- get_count("ENDEMICITY_UNRESOLVED")
  
  cat("Accepted contemporary taxa: ", format(nrow(taxa), big.mark = ","), "\n", sep = "")
  cat("Endemic Key population: ", format(n_endemic, big.mark = ","), "\n", sep = "")
  cat("Non-endemic Key population: ", format(n_nonendemic, big.mark = ","), "\n", sep = "")
  cat("GX evidence population: ", format(n_gx, big.mark = ","), "\n", sep = "")
  cat("HYB population after GX precedence: ", format(n_hyb, big.mark = ","), "\n", sep = "")
  cat("Endemicity unresolved: ", format(n_unresolved, big.mark = ","), "\n", sep = "")
  
  # ===========================================================================
  # 2. Methodological decision register
  #
  # ORIGINAL_WORDING is deliberately conservative.
  #
  # Where exact wording from Figure 2.2 has not yet been transcribed and
  # verified, we record only the identified criterion rather than manufacture
  # a quotation.
  #
  # No unresolved criterion may be used as a definitive classifier rule.
  # ===========================================================================
  
  register <- tribble(
    ~DECISION_ID, ~KEY_BRANCH, ~KEY_TERM, ~SOURCE, ~ORIGINAL_WORDING, ~INTERPRETATION, ~OPERATIONAL_DEFINITION, ~EVIDENCE_SOURCE, ~RATIONALE, ~STATUS, ~IMPLEMENTED_IN,
    
    "KTS-POP-001",
    "PRE_KEY",
    "accepted taxon",
    "Contemporary VPJD methodology",
    NA_character_,
    "Only accepted contemporary WCVP taxa enter the principal Key population.",
    "FINAL_WCVP_STATUS == 'Accepted'",
    "VPJD taxonomy reconciliation / WCVP",
    "Maintains one contemporary accepted taxon concept per analytical unit.",
    "METHOD_VALIDATED",
    "Taxonomy 04p / Stars 03a onward",
    
    "KTS-POP-002",
    "PRE_KEY",
    "introduced to Japan",
    "Figure 2.2 / contemporary evidence model",
    "Cultivated",
    "Positive evidence of introduction to Japan identifies GX candidates.",
    "Positive WCVP establishment evidence for introduction in Japanese geographic units.",
    "WCVP distribution",
    "GX requires positive evidence; absence of positive introduction evidence does not itself demonstrate native status.",
    "METHOD_VALIDATED",
    "Stars 03c",
    
    "KTS-POP-003",
    "PRE_KEY",
    "hybrid",
    "Contemporary VPJD methodological definition",
    NA_character_,
    "HYB is restricted to accepted names explicitly containing the multiplication sign between genus and specific epithet.",
    "Accepted scientific name matches Genus × species construction.",
    "WCVP accepted scientific name",
    "Provides a deterministic and auditable hybrid definition without inferring hybrid status from broader indicators.",
    "METHOD_VALIDATED",
    "Stars 03g v0.1.1",
    
    "KTS-END-001",
    "ENDEMIC",
    "endemic to Japan",
    "Figure 2.2 / contemporary evidence model",
    "Endemic to Japan",
    "Taxon is observed in Japan and has no positive WCVP distribution evidence outside Japan.",
    "Contemporary Japanese occurrence evidence AND WCVP distribution exists AND no positive non-Japanese WCVP distribution unit.",
    "VPJD occurrences + WCVP distribution",
    "Separates the endemic and non-endemic branches using independently reproducible contemporary evidence.",
    "METHOD_VALIDATED",
    "Stars 03c v0.1.1",
    
    "KTS-END-002",
    "ENDEMIC",
    "small island groups",
    "Figure 2.2",
    "Ryukyu / Ogasawara / Izu",
    "Japanese endemic taxon restricted to the small-island groups identified by the Key.",
    "To be expressed using the contemporary botanical-area framework without changing the geographic meaning of the Key.",
    "VPJD botanical geography",
    "Required for the direct endemic BK branch.",
    "PROVISIONAL",
    "Not yet frozen",
    
    "KTS-END-003",
    "ENDEMIC",
    "district",
    "Figure 2.2",
    "district",
    NA_character_,
    NA_character_,
    "Methodological source still required",
    "The Key explicitly uses district occupancy independently of prefecture occupancy. District membership must not be invented.",
    "UNRESOLVED",
    "Not implemented",
    
    "KTS-END-004",
    "ENDEMIC",
    "two districts or fewer",
    "Figure 2.2",
    "<=2 districts",
    "Requires a validated definition of district before it can be calculated.",
    NA_character_,
    "Dependent on KTS-END-003",
    "Used with the <=14 prefecture criterion in the endemic branch.",
    "UNRESOLVED",
    "Not implemented",
    
    "KTS-END-005",
    "ENDEMIC",
    "fourteen prefectures or fewer",
    "Figure 2.2",
    "<=14 prefectures",
    "Count Japanese prefectural units according to the Key-compatible contemporary prefecture framework.",
    "JAPAN_PREFECTURES_PRESENT <= 14",
    "VPJD occurrence-derived prefecture occupancy",
    "Prefecture occupancy is already independently calculated from contemporary occurrences.",
    "METHOD_VALIDATED",
    "Stars 03a v0.1.2",
    
    "KTS-END-006",
    "ENDEMIC",
    "seven prefectures or fewer",
    "Figure 2.2",
    "<=7 prefectures",
    "Use contemporary occurrence-derived prefecture occupancy.",
    "JAPAN_PREFECTURES_PRESENT <= 7",
    "VPJD occurrence-derived prefecture occupancy",
    "Separates BK and GD within the restricted endemic branch.",
    "METHOD_VALIDATED",
    "Stars 03a v0.1.2",
    
    "KTS-END-007",
    "ENDEMIC",
    "ten prefectures or fewer",
    "Figure 2.2",
    "<=10 prefectures",
    "Use contemporary occurrence-derived prefecture occupancy.",
    "JAPAN_PREFECTURES_PRESENT <= 10",
    "VPJD occurrence-derived prefecture occupancy",
    "Routes the widespread endemic branch.",
    "METHOD_VALIDATED",
    "Stars 03a v0.1.2",
    
    "KTS-END-008",
    "ENDEMIC",
    "five prefectures or fewer",
    "Figure 2.2",
    "<=5 prefectures",
    "Use contemporary occurrence-derived prefecture occupancy.",
    "JAPAN_PREFECTURES_PRESENT <= 5",
    "VPJD occurrence-derived prefecture occupancy",
    "Forms one component of the rarity-dependent endemic decision.",
    "METHOD_VALIDATED",
    "Stars 03a v0.1.2",
    
    "KTS-END-009",
    "ENDEMIC",
    "rare",
    "Figure 2.2",
    "rare",
    NA_character_,
    NA_character_,
    "Methodological source still required",
    "Occurrence-record count must not be substituted for rarity without methodological justification.",
    "UNRESOLVED",
    "Not implemented",
    
    "KTS-END-010",
    "ENDEMIC",
    "almost all districts",
    "Figure 2.2",
    "almost all districts",
    NA_character_,
    NA_character_,
    "Methodological source still required",
    "No numerical threshold should be invented for 'almost all'.",
    "UNRESOLVED",
    "Not implemented",
    
    "KTS-NON-001",
    "NON_ENDEMIC",
    "Taiwan range",
    "Figure 2.2",
    "Taiwan",
    "The Key distinguishes restricted distribution in Taiwan rather than treating all outside-Japan occurrence as equivalent.",
    NA_character_,
    "WCVP distribution plus additional defensible geographic evidence if required",
    "Exact Taiwan subcategories must be established from the figure before implementation.",
    "UNRESOLVED",
    "Not implemented",
    
    "KTS-NON-002",
    "NON_ENDEMIC",
    "Korea range",
    "Figure 2.2",
    "Korea",
    "The Key distinguishes restricted distribution in Korea.",
    NA_character_,
    "WCVP distribution plus additional defensible geographic evidence if required",
    "Exact Korean range categories must be established before implementation.",
    "UNRESOLVED",
    "Not implemented",
    
    "KTS-NON-003",
    "NON_ENDEMIC",
    "Kuriles and Sakhalin range",
    "Figure 2.2",
    "Kuriles / Sakhalin",
    "The Key distinguishes northern adjacent range structure.",
    NA_character_,
    "WCVP distribution plus additional defensible geographic evidence if required",
    "Exact northern range categories must be established before implementation.",
    "UNRESOLVED",
    "Not implemented",
    
    "KTS-NON-004",
    "NON_ENDEMIC",
    "China range",
    "Figure 2.2",
    "China",
    "The Key distinguishes Chinese range extent.",
    NA_character_,
    "WCVP distribution plus additional defensible geographic evidence if required",
    "Exact Chinese range categories must be established before implementation.",
    "UNRESOLVED",
    "Not implemented",
    
    "KTS-NON-005",
    "NON_ENDEMIC",
    "broader external distribution",
    "Figure 2.2",
    NA_character_,
    "A taxon occurring outside Japan must not automatically be classified GN.",
    NA_character_,
    "WCVP distribution plus Key-compatible regional evidence",
    "The regional branch must be traversed before a terminal Star is assigned.",
    "UNRESOLVED",
    "Not implemented",
    
    "KTS-NON-006",
    "NON_ENDEMIC",
    "one district or fewer",
    "Figure 2.2",
    "<=1 district",
    "Japanese occupancy class used by the non-endemic matrix.",
    NA_character_,
    "Dependent on KTS-END-003",
    "Cannot be implemented until district geography is method-validated.",
    "UNRESOLVED",
    "Not implemented",
    
    "KTS-NON-007",
    "NON_ENDEMIC",
    "two districts or fewer",
    "Figure 2.2",
    "<=2 districts",
    "Japanese occupancy class used by the non-endemic matrix.",
    NA_character_,
    "Dependent on KTS-END-003",
    "Cannot be implemented until district geography is method-validated.",
    "UNRESOLVED",
    "Not implemented",
    
    "KTS-NON-008",
    "NON_ENDEMIC",
    "three districts or fewer",
    "Figure 2.2",
    "<=3 districts",
    "Japanese occupancy class used by the non-endemic matrix.",
    NA_character_,
    "Dependent on KTS-END-003",
    "Cannot be implemented until district geography is method-validated.",
    "UNRESOLVED",
    "Not implemented",
    
    "KTS-NON-009",
    "NON_ENDEMIC",
    "more than three districts",
    "Figure 2.2",
    ">3 districts",
    "Japanese occupancy class used by the non-endemic matrix.",
    NA_character_,
    "Dependent on KTS-END-003",
    "Cannot be implemented until district geography is method-validated.",
    "UNRESOLVED",
    "Not implemented"
  )
  
  # ===========================================================================
  # 3. Dependency register
  # ===========================================================================
  
  dependencies <- tribble(
    ~DECISION_ID, ~DEPENDS_ON, ~DEPENDENCY_TYPE,
    
    "KTS-END-004", "KTS-END-003", "METHOD",
    "KTS-END-010", "KTS-END-003", "METHOD",
    "KTS-NON-006", "KTS-END-003", "METHOD",
    "KTS-NON-007", "KTS-END-003", "METHOD",
    "KTS-NON-008", "KTS-END-003", "METHOD",
    "KTS-NON-009", "KTS-END-003", "METHOD",
    
    "KTS-NON-001", "KTS-END-003", "GEOGRAPHIC_APPLICATION",
    "KTS-NON-002", "KTS-END-003", "GEOGRAPHIC_APPLICATION",
    "KTS-NON-003", "KTS-END-003", "GEOGRAPHIC_APPLICATION",
    "KTS-NON-004", "KTS-END-003", "GEOGRAPHIC_APPLICATION"
  )
  
  # ===========================================================================
  # 4. Prohibited assumptions
  # ===========================================================================
  
  prohibited <- tribble(
    ~ASSUMPTION_ID, ~ASSUMPTION, ~REASON,
    
    "KTS-PROHIBIT-001",
    "Any positive distribution outside Japan automatically equals GN.",
    "This bypasses the regional non-endemic structure of the published Key.",
    
    "KTS-PROHIBIT-002",
    "Modern Japanese administrative regions may be substituted for Key districts.",
    "The meaning of district has not yet been method-validated.",
    
    "KTS-PROHIBIT-003",
    "The 51 VPJD botanical areas are equivalent to Key districts.",
    "Botanical areas are contemporary evidence units and have not been demonstrated to be the districts intended by the Key.",
    
    "KTS-PROHIBIT-004",
    "Japanese prefectures are equivalent to Key districts.",
    "The Key uses prefectures and districts as separate variables.",
    
    "KTS-PROHIBIT-005",
    "GBIF occurrence frequency is equivalent to rarity.",
    "Sampling intensity and database representation cannot be assumed to represent biological rarity.",
    
    "KTS-PROHIBIT-006",
    "Historical Star totals may be used to tune contemporary thresholds.",
    "Historical totals are diagnostic context, not analytical targets.",
    
    "KTS-PROHIBIT-007",
    "An unresolved Key criterion may be replaced by analyst judgement during classification.",
    "Unresolved criteria must remain explicit and auditable until method-validated."
  )
  
  # ===========================================================================
  # 5. Methodological status summary
  # ===========================================================================
  
  status_summary <- register |>
    count(
      STATUS,
      name = "DECISIONS"
    ) |>
    arrange(
      factor(
        STATUS,
        levels = c(
          "UNRESOLVED",
          "PROVISIONAL",
          "METHOD_VALIDATED",
          "FROZEN"
        )
      )
    )
  
  branch_summary <- register |>
    count(
      KEY_BRANCH,
      STATUS,
      name = "DECISIONS"
    ) |>
    arrange(
      KEY_BRANCH,
      STATUS
    )
  
  cat("\n— Methodological status —\n")
  print.data.frame(status_summary, row.names = FALSE)
  
  cat("\n— Status by Key branch —\n")
  print.data.frame(branch_summary, row.names = FALSE)
  
  cat("\n— Unresolved methodological decisions —\n")
  
  unresolved <- register |>
    filter(
      STATUS %in% c(
        "UNRESOLVED",
        "PROVISIONAL"
      )
    ) |>
    select(
      DECISION_ID,
      KEY_BRANCH,
      KEY_TERM,
      STATUS
    )
  
  print.data.frame(
    unresolved,
    row.names = FALSE
  )
  
  # ===========================================================================
  # 6. Readiness gate
  #
  # The classifier must NOT be rebuilt while unresolved methodological
  # decisions remain in geographic branches required for BK/GD/BU/GN.
  # ===========================================================================
  
  geographic_branches <- c(
    "ENDEMIC",
    "NON_ENDEMIC"
  )
  
  blocking <- register |>
    filter(
      KEY_BRANCH %in% geographic_branches,
      STATUS %in% c(
        "UNRESOLVED",
        "PROVISIONAL"
      )
    )
  
  classifier_ready <- nrow(blocking) == 0L
  
  readiness <- tibble(
    CHECK = c(
      "ACCEPTED_POPULATION_ESTABLISHED",
      "ENDEMIC_POPULATION_ESTABLISHED",
      "NON_ENDEMIC_POPULATION_ESTABLISHED",
      "DISTRICT_METHOD_RESOLVED",
      "RARITY_METHOD_RESOLVED",
      "ALMOST_ALL_DISTRICTS_RESOLVED",
      "NON_ENDEMIC_MATRIX_RESOLVED",
      "FINAL_STAR_CLASSIFIER_READY"
    ),
    VALUE = c(
      nrow(taxa) == 11439L,
      n_endemic == 2141L,
      n_nonendemic == 7736L,
      register$STATUS[register$DECISION_ID == "KTS-END-003"] %in%
        c("METHOD_VALIDATED", "FROZEN"),
      register$STATUS[register$DECISION_ID == "KTS-END-009"] %in%
        c("METHOD_VALIDATED", "FROZEN"),
      register$STATUS[register$DECISION_ID == "KTS-END-010"] %in%
        c("METHOD_VALIDATED", "FROZEN"),
      all(
        register$STATUS[
          register$DECISION_ID %in%
            c(
              "KTS-NON-001",
              "KTS-NON-002",
              "KTS-NON-003",
              "KTS-NON-004",
              "KTS-NON-005",
              "KTS-NON-006",
              "KTS-NON-007",
              "KTS-NON-008",
              "KTS-NON-009"
            )
        ] %in% c("METHOD_VALIDATED", "FROZEN")
      ),
      classifier_ready
    )
  )
  
  cat("\n— Classifier readiness —\n")
  print.data.frame(readiness, row.names = FALSE)
  
  # ===========================================================================
  # 7. Validation
  # ===========================================================================
  
  allowed_status <- c(
    "UNRESOLVED",
    "PROVISIONAL",
    "METHOD_VALIDATED",
    "FROZEN"
  )
  
  validation <- tibble(
    CHECK = c(
      "accepted_population_11439",
      "accepted_ids_unique",
      "endemic_population_2141",
      "nonendemic_population_7736",
      "gx_population_1120",
      "hyb_population_440",
      "endemicity_unresolved_2",
      "decision_ids_unique",
      "decision_status_vocabulary_valid",
      "district_explicitly_unresolved",
      "rarity_explicitly_unresolved",
      "almost_all_districts_explicitly_unresolved",
      "nonendemic_regional_method_not_assumed",
      "outside_japan_gn_shortcut_prohibited",
      "modern_region_shortcut_prohibited",
      "occurrence_rarity_shortcut_prohibited",
      "historical_calibration_prohibited",
      "unresolved_judgement_prohibited",
      "classifier_correctly_blocked",
      "stars_not_assigned",
      "legacy_analytical_data_not_used",
      "historical_totals_not_used_as_targets",
      "ghi_not_calculated"
    ),
    PASS = c(
      nrow(taxa) == 11439L,
      n_distinct(taxa$FINAL_WCVP_ID) == 11439L,
      n_endemic == 2141L,
      n_nonendemic == 7736L,
      n_gx == 1120L,
      n_hyb == 440L,
      n_unresolved == 2L,
      n_distinct(register$DECISION_ID) == nrow(register),
      all(register$STATUS %in% allowed_status),
      register$STATUS[register$DECISION_ID == "KTS-END-003"] == "UNRESOLVED",
      register$STATUS[register$DECISION_ID == "KTS-END-009"] == "UNRESOLVED",
      register$STATUS[register$DECISION_ID == "KTS-END-010"] == "UNRESOLVED",
      all(
        register$STATUS[
          register$KEY_BRANCH == "NON_ENDEMIC"
        ] != "FROZEN"
      ),
      "KTS-PROHIBIT-001" %in% prohibited$ASSUMPTION_ID,
      "KTS-PROHIBIT-002" %in% prohibited$ASSUMPTION_ID,
      "KTS-PROHIBIT-005" %in% prohibited$ASSUMPTION_ID,
      "KTS-PROHIBIT-006" %in% prohibited$ASSUMPTION_ID,
      "KTS-PROHIBIT-007" %in% prohibited$ASSUMPTION_ID,
      classifier_ready == FALSE,
      TRUE,
      TRUE,
      TRUE,
      TRUE
    )
  )
  
  cat("\n— Validation —\n")
  print.data.frame(validation, row.names = FALSE)
  
  all_pass <- all(validation$PASS)
  
  cat(
    "\nAll Stars 03i validation checks PASS: ",
    all_pass,
    "\n",
    sep = ""
  )
  
  if (!all_pass) {
    stop(
      "Stars 03i validation failed. Do not freeze."
    )
  }
  
  # ===========================================================================
  # 8. Metadata
  # ===========================================================================
  
  metadata <- tibble(
    METRIC = c(
      "module",
      "version",
      "run_date",
      "accepted_taxa",
      "endemic_key_taxa",
      "nonendemic_key_taxa",
      "gx_taxa",
      "hyb_taxa",
      "endemicity_unresolved",
      "methodological_decisions",
      "unresolved_or_provisional_decisions",
      "classifier_ready",
      "stars_assigned",
      "legacy_analytical_inputs_used",
      "historical_star_totals_used_as_targets",
      "validation_pass"
    ),
    VALUE = c(
      MODULE,
      VERSION,
      as.character(RUN_DATE),
      as.character(nrow(taxa)),
      as.character(n_endemic),
      as.character(n_nonendemic),
      as.character(n_gx),
      as.character(n_hyb),
      as.character(n_unresolved),
      as.character(nrow(register)),
      as.character(nrow(blocking)),
      as.character(classifier_ready),
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
    OUTPUT_REGISTER,
    register,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "vpjd_star_methodological_dependencies",
    dependencies,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "vpjd_star_prohibited_methodological_assumptions",
    prohibited,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    OUTPUT_STATUS,
    readiness,
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
    register,
    file.path(
      OUTPUT_DIR,
      "stars03i_methodological_decision_register.csv"
    )
  )
  
  write_csv(
    dependencies,
    file.path(
      OUTPUT_DIR,
      "stars03i_methodological_dependencies.csv"
    )
  )
  
  write_csv(
    prohibited,
    file.path(
      OUTPUT_DIR,
      "stars03i_prohibited_methodological_assumptions.csv"
    )
  )
  
  write_csv(
    readiness,
    file.path(
      OUTPUT_DIR,
      "stars03i_classifier_readiness.csv"
    )
  )
  
  write_csv(
    validation,
    file.path(
      OUTPUT_DIR,
      "stars03i_validation.csv"
    )
  )
  
  write_csv(
    metadata,
    file.path(
      OUTPUT_DIR,
      "stars03i_metadata.csv"
    )
  )
  
  cat("\nCanonical methodological register:\n")
  cat("  ", OUTPUT_REGISTER, "\n", sep = "")
  
  cat("\nStars 03i v", VERSION, " complete.\n", sep = "")
  
  cat(
    "Methodological decision register established.\n",
    "Unresolved Key criteria remain explicit.\n",
    "Final Star classifier remains blocked until method validation.\n",
    sep = ""
  )
  
  invisible(
    list(
      register = register,
      dependencies = dependencies,
      prohibited = prohibited,
      readiness = readiness,
      validation = validation,
      metadata = metadata
    )
  )
}

result_stars_03i <- run_stars_03i()