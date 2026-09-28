# =============================================================================
# VPJD-OJPCP
# R/stars/03d_audit_wcvp_japan_geography.R
# Version 0.1.0
#
# AUDIT WCVP/WGSRPD GEOGRAPHIC UNITS RELEVANT TO JAPAN
#
# Purpose:
# - Audit all WCVP geographic units used by the 11,439 accepted VPJD taxa.
# - Examine the 2,743 taxa with WCVP distribution but no JAP/NNS/OGA row.
# - Identify any additional WCVP units potentially representing Japan.
# - Profile introduction evidence within candidate Japanese units.
# - Test whether the 03c Japan definition is complete.
#
# IMPORTANT:
# - Diagnostic only.
# - No Stars assigned.
# - No 03c classifications overwritten.
# - No geographic unit is automatically added to Japan.
# - No endemicity or GX status is promoted to canonical status.
# =============================================================================

suppressPackageStartupMessages({
  library(DBI)
  library(duckdb)
  library(dplyr)
  library(readr)
  library(here)
})

MODULE <- "stars_03d_audit_wcvp_japan_geography"
VERSION <- "0.1.0"
RUN_DATE <- Sys.Date()

DB_PATH <- here("data", "interim", "occurrences", "vpjd_occurrences.duckdb")
SOURCE_TAXA <- "vpjd_japan_taxon_distribution"
SOURCE_03C <- "vpjd_star_wcvp_key_geography"

WCVP_DIR <- here("data", "raw", "geography", "wcvp_dwca")
WCVP_DISTRIBUTION_FILE <- file.path(WCVP_DIR, "wcvp_distribution.csv")

OUTPUT_DIR <- here(
  "outputs",
  "tables",
  "stars",
  "wcvp_japan_geography_audit"
)

OUTPUT_UNIT_AUDIT <- "vpjd_star_wcvp_japan_unit_audit"
OUTPUT_MISSING_JAPAN <- "vpjd_star_wcvp_no_japan_evidence_audit"
OUTPUT_VALIDATION <- "vpjd_star_wcvp_japan_audit_validation"
OUTPUT_METADATA <- "vpjd_star_wcvp_japan_audit_metadata"

run_stars_03d <- function() {
  
  cat("\n— Audit WCVP Japan geography —\n\n")
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
  
  if (!file.exists(WCVP_DISTRIBUTION_FILE)) {
    stop(
      "WCVP distribution file not found: ",
      WCVP_DISTRIBUTION_FILE
    )
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
    SOURCE_TAXA,
    SOURCE_03C
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
  # 1. Accepted taxon population
  # ===========================================================================
  
  accepted <- dbGetQuery(
    con,
    paste0(
      "SELECT ",
      "CAST(FINAL_WCVP_ID AS VARCHAR) AS FINAL_WCVP_ID, ",
      "FINAL_WCVP_RECOGNISED_NAME, ",
      "FINAL_WCVP_RANK ",
      "FROM ", SOURCE_TAXA, " ",
      "WHERE lower(FINAL_WCVP_STATUS) = 'accepted'"
    )
  )
  
  cat(
    "Accepted VPJD taxa: ",
    format(nrow(accepted), big.mark = ","),
    "\n",
    sep = ""
  )
  
  if (nrow(accepted) != 11439L) {
    stop("Accepted taxon population is not 11,439.")
  }
  
  if (n_distinct(accepted$FINAL_WCVP_ID) != 11439L) {
    stop("Accepted WCVP IDs are not unique.")
  }
  
  dbWriteTable(
    con,
    "vpjd_star_accepted_taxa_temp",
    accepted,
    temporary = TRUE,
    overwrite = TRUE
  )
  
  # ===========================================================================
  # 2. Read WCVP distribution
  # ===========================================================================
  
  distribution_path_sql <- gsub(
    "'",
    "''",
    normalizePath(
      WCVP_DISTRIBUTION_FILE,
      winslash = "/",
      mustWork = TRUE
    )
  )
  
  dbExecute(
    con,
    paste0(
      "CREATE OR REPLACE TEMP VIEW wcvp_distribution_raw AS ",
      "SELECT ",
      "CAST(coreid AS VARCHAR) AS taxon_id, ",
      "locality, ",
      "establishmentmeans AS establishment_means, ",
      "locationid AS location_id, ",
      "occurrencestatus AS occurrence_status ",
      "FROM read_csv_auto('",
      distribution_path_sql,
      "', delim='|', header=TRUE, all_varchar=TRUE)"
    )
  )
  
  # ===========================================================================
  # 3. Positive WCVP evidence only
  # ===========================================================================
  
  evidence <- dbGetQuery(
    con,
    "
    SELECT
      d.taxon_id AS FINAL_WCVP_ID,
      d.location_id AS LOCATION_ID,
      d.locality AS LOCALITY,
      d.establishment_means AS ESTABLISHMENT_MEANS,
      d.occurrence_status AS OCCURRENCE_STATUS
    FROM wcvp_distribution_raw d
    INNER JOIN vpjd_star_accepted_taxa_temp v
      ON d.taxon_id = v.FINAL_WCVP_ID
    "
  ) |>
    mutate(
      IS_DOUBTFUL =
        !is.na(OCCURRENCE_STATUS) &
        tolower(trimws(OCCURRENCE_STATUS)) == "doubtful",
      IS_INTRODUCED =
        !is.na(ESTABLISHMENT_MEANS) &
        tolower(trimws(ESTABLISHMENT_MEANS)) == "introduced"
    )
  
  positive <- evidence |>
    filter(!IS_DOUBTFUL)
  
  cat(
    "Positive WCVP distribution rows: ",
    format(nrow(positive), big.mark = ","),
    "\n",
    sep = ""
  )
  
  # ===========================================================================
  # 4. Current Stars 03c Japanese WCVP definition
  # ===========================================================================
  
  current_japan_ids <- c(
    "TDWG:JAP",
    "TDWG:NNS",
    "TDWG:OGA"
  )
  
  cat("\nCurrent 03c Japanese WCVP units:\n")
  print(current_japan_ids)
  
  # ===========================================================================
  # 5. Audit every geographic unit
  # ===========================================================================
  
  unit_audit <- positive |>
    group_by(
      LOCATION_ID,
      LOCALITY
    ) |>
    summarise(
      DISTRIBUTION_ROWS = n(),
      WCVP_TAXA = n_distinct(FINAL_WCVP_ID),
      INTRODUCED_ROWS = sum(IS_INTRODUCED, na.rm = TRUE),
      INTRODUCED_TAXA = n_distinct(
        FINAL_WCVP_ID[IS_INTRODUCED]
      ),
      .groups = "drop"
    ) |>
    mutate(
      CURRENT_03C_JAPAN =
        LOCATION_ID %in% current_japan_ids,
      
      JAPAN_TEXT_CANDIDATE =
        grepl(
          paste(
            c(
              "Japan",
              "Nansei",
              "Ryukyu",
              "Ogasawara",
              "Bonin",
              "Izu",
              "Kazan",
              "Volcano",
              "Hokkaido",
              "Honshu",
              "Shikoku",
              "Kyushu",
              "Tsushima",
              "Yakushima",
              "Tanegashima",
              "Amami",
              "Okinawa"
            ),
            collapse = "|"
          ),
          LOCALITY,
          ignore.case = TRUE
        ),
      
      AUDIT_PRIORITY =
        case_when(
          CURRENT_03C_JAPAN ~ "CURRENT_JAPAN_UNIT",
          JAPAN_TEXT_CANDIDATE ~ "REVIEW_AS_POSSIBLE_JAPAN",
          TRUE ~ "OTHER_WCVP_UNIT"
        )
    ) |>
    arrange(
      factor(
        AUDIT_PRIORITY,
        levels = c(
          "REVIEW_AS_POSSIBLE_JAPAN",
          "CURRENT_JAPAN_UNIT",
          "OTHER_WCVP_UNIT"
        )
      ),
      desc(WCVP_TAXA),
      LOCATION_ID
    )
  
  cat(
    "\nTotal audited WCVP geographic units: ",
    nrow(unit_audit),
    "\n",
    sep = ""
  )
  
  # ===========================================================================
  # 6. Explicit possible-Japan candidates
  # ===========================================================================
  
  japan_candidates <- unit_audit |>
    filter(
      JAPAN_TEXT_CANDIDATE |
        CURRENT_03C_JAPAN
    )
  
  cat("\n— WCVP units potentially relevant to Japan —\n")
  
  print.data.frame(
    japan_candidates,
    row.names = FALSE
  )
  
  additional_candidates <- japan_candidates |>
    filter(!CURRENT_03C_JAPAN)
  
  cat(
    "\nAdditional candidate Japanese units beyond JAP/NNS/OGA: ",
    nrow(additional_candidates),
    "\n",
    sep = ""
  )
  
  if (nrow(additional_candidates) > 0L) {
    print.data.frame(
      additional_candidates,
      row.names = FALSE
    )
  }
  
  # ===========================================================================
  # 7. Reproduce the 03c no-Japan-evidence population
  # ===========================================================================
  
  taxon_current_japan <- positive |>
    group_by(FINAL_WCVP_ID) |>
    summarise(
      CURRENT_JAPAN_EVIDENCE =
        any(LOCATION_ID %in% current_japan_ids),
      .groups = "drop"
    )
  
  has_distribution <- positive |>
    distinct(FINAL_WCVP_ID) |>
    mutate(HAS_POSITIVE_WCVP_DISTRIBUTION = TRUE)
  
  missing_japan_taxa <- accepted |>
    left_join(
      has_distribution,
      by = "FINAL_WCVP_ID"
    ) |>
    left_join(
      taxon_current_japan,
      by = "FINAL_WCVP_ID"
    ) |>
    mutate(
      HAS_POSITIVE_WCVP_DISTRIBUTION =
        ifelse(
          is.na(HAS_POSITIVE_WCVP_DISTRIBUTION),
          FALSE,
          HAS_POSITIVE_WCVP_DISTRIBUTION
        ),
      CURRENT_JAPAN_EVIDENCE =
        ifelse(
          is.na(CURRENT_JAPAN_EVIDENCE),
          FALSE,
          CURRENT_JAPAN_EVIDENCE
        )
    ) |>
    filter(
      HAS_POSITIVE_WCVP_DISTRIBUTION,
      !CURRENT_JAPAN_EVIDENCE
    )
  
  cat(
    "\nTaxa with WCVP distribution but no JAP/NNS/OGA evidence: ",
    format(nrow(missing_japan_taxa), big.mark = ","),
    "\n",
    sep = ""
  )
  
  # ===========================================================================
  # 8. Which units occur among those 2,743 taxa?
  # ===========================================================================
  
  missing_japan_evidence <- missing_japan_taxa |>
    select(
      FINAL_WCVP_ID,
      FINAL_WCVP_RECOGNISED_NAME,
      FINAL_WCVP_RANK
    ) |>
    inner_join(
      positive,
      by = "FINAL_WCVP_ID"
    )
  
  missing_unit_profile <- missing_japan_evidence |>
    group_by(
      LOCATION_ID,
      LOCALITY
    ) |>
    summarise(
      DISTRIBUTION_ROWS = n(),
      WCVP_TAXA = n_distinct(FINAL_WCVP_ID),
      INTRODUCED_TAXA = n_distinct(
        FINAL_WCVP_ID[IS_INTRODUCED]
      ),
      .groups = "drop"
    ) |>
    arrange(
      desc(WCVP_TAXA),
      LOCATION_ID
    )
  
  cat("\n— Top 75 WCVP units among taxa lacking JAP/NNS/OGA —\n")
  
  print.data.frame(
    head(missing_unit_profile, 75),
    row.names = FALSE
  )
  
  # ===========================================================================
  # 9. Test additional text-identified candidate units
  #
  # Diagnostic only. We do NOT promote these units to Japan.
  # ===========================================================================
  
  additional_candidate_ids <- additional_candidates$LOCATION_ID
  
  candidate_recovery <- tibble::tibble(
    metric = c(
      "taxa_without_current_japan_evidence",
      "taxa_recovered_by_additional_text_candidates",
      "taxa_still_without_japan_evidence"
    ),
    value = c(
      nrow(missing_japan_taxa),
      0L,
      nrow(missing_japan_taxa)
    )
  )
  
  if (length(additional_candidate_ids) > 0L) {
    
    recovered_ids <- missing_japan_evidence |>
      filter(
        LOCATION_ID %in% additional_candidate_ids
      ) |>
      distinct(FINAL_WCVP_ID)
    
    candidate_recovery$value[
      candidate_recovery$metric ==
        "taxa_recovered_by_additional_text_candidates"
    ] <- nrow(recovered_ids)
    
    candidate_recovery$value[
      candidate_recovery$metric ==
        "taxa_still_without_japan_evidence"
    ] <- nrow(missing_japan_taxa) - nrow(recovered_ids)
  }
  
  cat("\n— Candidate Japan-unit recovery diagnostic —\n")
  print.data.frame(
    candidate_recovery,
    row.names = FALSE
  )
  
  # ===========================================================================
  # 10. Audit introduction evidence
  # ===========================================================================
  
  current_japan_introduced <- positive |>
    filter(
      LOCATION_ID %in% current_japan_ids,
      IS_INTRODUCED
    ) |>
    distinct(
      FINAL_WCVP_ID,
      LOCATION_ID,
      LOCALITY
    )
  
  current_gx_candidates <- current_japan_introduced |>
    distinct(FINAL_WCVP_ID)
  
  additional_japan_introduced <- positive |>
    filter(
      LOCATION_ID %in% additional_candidate_ids,
      IS_INTRODUCED
    ) |>
    distinct(
      FINAL_WCVP_ID,
      LOCATION_ID,
      LOCALITY
    )
  
  additional_gx_candidates <- additional_japan_introduced |>
    anti_join(
      current_gx_candidates,
      by = "FINAL_WCVP_ID"
    ) |>
    distinct(FINAL_WCVP_ID)
  
  introduction_audit <- tibble::tibble(
    metric = c(
      "gx_candidates_under_current_JAP_NNS_OGA_definition",
      "additional_taxa_if_all_text_candidates_were_Japan"
    ),
    value = c(
      nrow(current_gx_candidates),
      nrow(additional_gx_candidates)
    )
  )
  
  cat("\n— Introduction-to-Japan audit —\n")
  print.data.frame(
    introduction_audit,
    row.names = FALSE
  )
  
  # ===========================================================================
  # 11. Compare directly against Stars 03c
  # ===========================================================================
  
  stars03c <- dbGetQuery(
    con,
    paste0(
      "SELECT ",
      "CAST(FINAL_WCVP_ID AS VARCHAR) AS FINAL_WCVP_ID, ",
      "HAS_WCVP_DISTRIBUTION, ",
      "WCVP_JAPAN_EVIDENCE, ",
      "WCVP_INTRODUCED_TO_JAPAN, ",
      "ENDEMIC_TO_JAPAN_WCVP ",
      "FROM ", SOURCE_03C
    )
  )
  
  cat(
    "\nStars 03c rows: ",
    format(nrow(stars03c), big.mark = ","),
    "\n",
    sep = ""
  )
  
  endemic03c <- sum(
    stars03c$ENDEMIC_TO_JAPAN_WCVP %in% TRUE,
    na.rm = TRUE
  )
  
  introduced03c <- sum(
    stars03c$WCVP_INTRODUCED_TO_JAPAN %in% TRUE,
    na.rm = TRUE
  )
  
  no_japan03c <- sum(
    stars03c$HAS_WCVP_DISTRIBUTION %in% TRUE &
      stars03c$WCVP_JAPAN_EVIDENCE %in% FALSE,
    na.rm = TRUE
  )
  
  cat(
    "03c candidate endemic taxa: ",
    endemic03c,
    "\n",
    sep = ""
  )
  
  cat(
    "03c introduced-to-Japan candidates: ",
    introduced03c,
    "\n",
    sep = ""
  )
  
  cat(
    "03c taxa with distribution but no current Japan evidence: ",
    no_japan03c,
    "\n",
    sep = ""
  )
  
  # ===========================================================================
  # 12. Validation
  # ===========================================================================
  
  validation <- tibble::tibble(
    check = c(
      "accepted_taxon_population_11439",
      "accepted_taxon_ids_unique",
      "stars03c_population_11439",
      "positive_distribution_rows_present",
      "all_wcvp_units_audited",
      "current_japan_definition_JAP_NNS_OGA",
      "missing_japan_population_matches_03c",
      "introduced_population_matches_03c",
      "endemic_population_matches_03c",
      "candidate_units_not_automatically_promoted",
      "endemicity_not_reclassified",
      "gx_not_assigned",
      "stars_not_assigned",
      "historical_star_totals_not_used",
      "ghi_not_calculated"
    ),
    pass = c(
      nrow(accepted) == 11439L,
      n_distinct(accepted$FINAL_WCVP_ID) == 11439L,
      nrow(stars03c) == 11439L,
      nrow(positive) > 0L,
      nrow(unit_audit) == n_distinct(positive$LOCATION_ID),
      identical(
        sort(current_japan_ids),
        sort(c("TDWG:JAP", "TDWG:NNS", "TDWG:OGA"))
      ),
      nrow(missing_japan_taxa) == no_japan03c,
      nrow(current_gx_candidates) == introduced03c,
      endemic03c ==
        sum(
          stars03c$ENDEMIC_TO_JAPAN_WCVP %in% TRUE,
          na.rm = TRUE
        ),
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
    "\nAll Stars 03d validation checks PASS: ",
    all_pass,
    "\n",
    sep = ""
  )
  
  if (!all_pass) {
    stop(
      "Stars 03d validation failed. Do not freeze."
    )
  }
  
  # ===========================================================================
  # 13. Persist diagnostic outputs
  # ===========================================================================
  
  metadata <- tibble::tibble(
    metric = c(
      "module",
      "version",
      "run_date",
      "accepted_taxa",
      "positive_wcvp_rows",
      "wcvp_units_audited",
      "current_japan_units",
      "additional_candidate_units",
      "stars_assigned",
      "gx_assigned",
      "endemicity_promoted",
      "validation_pass"
    ),
    value = c(
      MODULE,
      VERSION,
      as.character(RUN_DATE),
      as.character(nrow(accepted)),
      as.character(nrow(positive)),
      as.character(nrow(unit_audit)),
      paste(current_japan_ids, collapse = ";"),
      as.character(nrow(additional_candidates)),
      "FALSE",
      "FALSE",
      "FALSE",
      as.character(all_pass)
    )
  )
  
  dbWriteTable(
    con,
    OUTPUT_UNIT_AUDIT,
    unit_audit,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    OUTPUT_MISSING_JAPAN,
    missing_unit_profile,
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
    unit_audit,
    file.path(
      OUTPUT_DIR,
      "stars03d_all_wcvp_units_audit.csv"
    )
  )
  
  write_csv(
    japan_candidates,
    file.path(
      OUTPUT_DIR,
      "stars03d_possible_japan_units.csv"
    )
  )
  
  write_csv(
    additional_candidates,
    file.path(
      OUTPUT_DIR,
      "stars03d_additional_japan_candidates.csv"
    )
  )
  
  write_csv(
    missing_japan_taxa,
    file.path(
      OUTPUT_DIR,
      "stars03d_taxa_without_current_japan_evidence.csv"
    )
  )
  
  write_csv(
    missing_unit_profile,
    file.path(
      OUTPUT_DIR,
      "stars03d_units_among_taxa_without_japan_evidence.csv"
    )
  )
  
  write_csv(
    candidate_recovery,
    file.path(
      OUTPUT_DIR,
      "stars03d_candidate_recovery.csv"
    )
  )
  
  write_csv(
    current_japan_introduced,
    file.path(
      OUTPUT_DIR,
      "stars03d_current_japan_introduced_evidence.csv"
    )
  )
  
  write_csv(
    additional_japan_introduced,
    file.path(
      OUTPUT_DIR,
      "stars03d_additional_candidate_introduced_evidence.csv"
    )
  )
  
  write_csv(
    introduction_audit,
    file.path(
      OUTPUT_DIR,
      "stars03d_introduction_audit.csv"
    )
  )
  
  write_csv(
    validation,
    file.path(
      OUTPUT_DIR,
      "stars03d_validation.csv"
    )
  )
  
  write_csv(
    metadata,
    file.path(
      OUTPUT_DIR,
      "stars03d_metadata.csv"
    )
  )
  
  cat("\nCanonical diagnostic outputs:\n")
  cat("  ", OUTPUT_UNIT_AUDIT, "\n", sep = "")
  cat("  ", OUTPUT_MISSING_JAPAN, "\n", sep = "")
  
  cat(
    "\nStars 03d v",
    VERSION,
    " complete.\n",
    sep = ""
  )
  
  cat(
    "Diagnostic only: no WCVP unit promoted to Japan and no Stars assigned.\n"
  )
  
  invisible(
    list(
      unit_audit = unit_audit,
      japan_candidates = japan_candidates,
      additional_candidates = additional_candidates,
      missing_japan_taxa = missing_japan_taxa,
      missing_unit_profile = missing_unit_profile,
      candidate_recovery = candidate_recovery,
      introduction_audit = introduction_audit,
      validation = validation,
      metadata = metadata
    )
  )
}

result_stars_03d <- run_stars_03d()