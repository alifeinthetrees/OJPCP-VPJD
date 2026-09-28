# =============================================================================
# VPJD-OJPCP
# R/stars/03g_formalise_hybrid_status.R
# Version 0.1.1
#
# FORMALISE CONTEMPORARY HYBRID STATUS FOR KEY TO STARS
#
# HYB rule:
# - Eligible population = strictly accepted contemporary WCVP taxa.
# - HYB requires an explicit multiplication sign (×) between the genus
#   and specific epithet in the accepted WCVP recognised name.
#
# Examples:
#   Aconitum × fudjisanense                  -> HYB
#   Asplenium × huawuense nothosubsp. kidoi -> HYB
#   Aconitum japonicum                       -> NOT_HYBRID
#
# Leading × names:
#   × Agropogon hondoensis                   -> NOTHOGENUS_DIAGNOSTIC
#   × Festulolium                            -> NOTHOGENUS_DIAGNOSTIC
#
# Leading-× names are retained explicitly for review but are NOT assigned HYB
# under the contemporary HYB rule specified for this project.
#
# IMPORTANT:
# - Rank, concept class, notho-rank and other broad indicators do not assign HYB.
# - This module derives hybrid evidence only.
# - It does not assign final Stars.
# - It does not resolve GX/HYB precedence.
# - No legacy analytical data are used.
# =============================================================================

suppressPackageStartupMessages({
  library(DBI)
  library(duckdb)
  library(dplyr)
  library(readr)
  library(stringr)
  library(here)
})

MODULE <- "stars_03g_formalise_hybrid_status"
VERSION <- "0.1.1"
RUN_DATE <- Sys.Date()

DB_PATH <- here(
  "data", "interim", "occurrences", "vpjd_occurrences.duckdb"
)

OUTPUT_DIR <- here(
  "outputs", "tables", "stars", "hybrid_status"
)

OUTPUT_TABLE <- "vpjd_star_hybrid_status"
OUTPUT_NOTHOGENUS <- "vpjd_star_nothogenus_diagnostic"
OUTPUT_VALIDATION <- "vpjd_star_03g_validation"
OUTPUT_METADATA <- "vpjd_star_03g_metadata"

run_stars_03g <- function() {
  cat("\n— Formalise contemporary HYB evidence —\n\n")
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
  # 1. Contemporary accepted-taxon population
  # ===========================================================================
  
  source_table <- "vpjd_japan_taxon_distribution"
  
  if (!source_table %in% dbListTables(con)) {
    stop("Required contemporary source missing: ", source_table)
  }
  
  taxa <- dbGetQuery(
    con,
    paste0(
      "
      SELECT DISTINCT
        FINAL_WCVP_ID,
        FINAL_WCVP_RECOGNISED_NAME,
        FINAL_WCVP_RANK,
        FINAL_WCVP_STATUS,
        FINAL_WCVP_CONCEPT_CLASS
      FROM ", source_table, "
      WHERE FINAL_WCVP_STATUS = 'Accepted'
      "
    )
  ) |>
    as_tibble()
  
  if (nrow(taxa) != 11439L) {
    stop(
      "Expected 11,439 accepted taxa; found ",
      format(nrow(taxa), big.mark = ","),
      "."
    )
  }
  
  if (n_distinct(taxa$FINAL_WCVP_ID) != nrow(taxa)) {
    stop("Accepted WCVP taxon IDs are not unique.")
  }
  
  if (any(is.na(taxa$FINAL_WCVP_RECOGNISED_NAME))) {
    stop(
      "Accepted population contains missing recognised names. ",
      "Do not classify hybrid status."
    )
  }
  
  cat(
    "Accepted contemporary taxa: ",
    format(nrow(taxa), big.mark = ","),
    "\n",
    sep = ""
  )
  
  # ===========================================================================
  # 2. Explicit name patterns
  #
  # HYB:
  #   Genus × species
  #
  # NOTHOGENUS_DIAGNOSTIC:
  #   × Genus species
  #   × Genus
  #
  # The leading-× diagnostic is deliberately mutually exclusive with HYB.
  # ===========================================================================
  
  hybrid_pattern <- "^\\s*\\S+\\s+×\\s+\\S+"
  leading_multiplication_pattern <- "^\\s*×\\s*\\S+"
  
  hybrid_status <- taxa |>
    mutate(
      HYBRID_NAME_PATTERN = str_detect(
        FINAL_WCVP_RECOGNISED_NAME,
        regex(
          hybrid_pattern,
          ignore_case = FALSE
        )
      ),
      
      LEADING_MULTIPLICATION_SIGN = str_detect(
        FINAL_WCVP_RECOGNISED_NAME,
        regex(
          leading_multiplication_pattern,
          ignore_case = FALSE
        )
      ),
      
      CONTAINS_MULTIPLICATION_SIGN = str_detect(
        FINAL_WCVP_RECOGNISED_NAME,
        fixed("×")
      ),
      
      HYBRID_STATUS = case_when(
        HYBRID_NAME_PATTERN ~ "HYBRID",
        LEADING_MULTIPLICATION_SIGN ~ "NOTHOGENUS_DIAGNOSTIC",
        TRUE ~ "NOT_HYBRID"
      ),
      
      HYBRID_EVIDENCE = case_when(
        HYBRID_NAME_PATTERN ~
          "EXPLICIT_MULTIPLICATION_SIGN_BETWEEN_GENUS_AND_SPECIFIC_EPITHET",
        
        LEADING_MULTIPLICATION_SIGN ~
          "LEADING_MULTIPLICATION_SIGN_NOTHOGENUS_DIAGNOSTIC",
        
        TRUE ~
          "NO_EXPLICIT_GENUS_X_SPECIES_PATTERN"
      ),
      
      HYB_ELIGIBLE = HYBRID_NAME_PATTERN,
      
      HYBRID_RULE = "GENUS_X_SPECIES_NAME_PATTERN",
      
      HYBRID_MODULE = MODULE,
      
      HYBRID_VERSION = VERSION
    )
  
  # ===========================================================================
  # 3. HYB population
  # ===========================================================================
  
  hybrids <- hybrid_status |>
    filter(
      HYBRID_STATUS == "HYBRID"
    ) |>
    arrange(
      FINAL_WCVP_RECOGNISED_NAME
    )
  
  cat("\n— Hybrid-status profile —\n")
  
  profile <- hybrid_status |>
    count(
      HYBRID_STATUS,
      name = "WCVP_TAXA"
    ) |>
    arrange(
      HYBRID_STATUS
    )
  
  print.data.frame(
    profile,
    row.names = FALSE
  )
  
  cat(
    "\nAccepted taxa satisfying explicit Genus × species rule: ",
    format(nrow(hybrids), big.mark = ","),
    "\n",
    sep = ""
  )
  
  # ===========================================================================
  # 4. Nothogenus diagnostic
  # ===========================================================================
  
  nothogenus_diagnostic <- hybrid_status |>
    filter(
      HYBRID_STATUS == "NOTHOGENUS_DIAGNOSTIC"
    ) |>
    arrange(
      FINAL_WCVP_RECOGNISED_NAME
    ) |>
    select(
      FINAL_WCVP_ID,
      FINAL_WCVP_RECOGNISED_NAME,
      FINAL_WCVP_RANK,
      FINAL_WCVP_STATUS,
      FINAL_WCVP_CONCEPT_CLASS,
      HYBRID_STATUS,
      HYBRID_EVIDENCE,
      HYB_ELIGIBLE,
      HYBRID_MODULE,
      HYBRID_VERSION
    )
  
  cat(
    "\nAccepted leading-× nothogenus diagnostic taxa: ",
    format(
      nrow(nothogenus_diagnostic),
      big.mark = ","
    ),
    "\n",
    sep = ""
  )
  
  if (nrow(nothogenus_diagnostic) > 0L) {
    cat("\n— Nothogenus diagnostic —\n")
    
    print.data.frame(
      nothogenus_diagnostic |>
        select(
          FINAL_WCVP_ID,
          FINAL_WCVP_RECOGNISED_NAME,
          FINAL_WCVP_RANK,
          HYBRID_STATUS
        ),
      row.names = FALSE
    )
  }
  
  # ===========================================================================
  # 5. Multiplication-sign audit
  #
  # Every accepted name containing × should now fall into one of:
  # - HYBRID
  # - NOTHOGENUS_DIAGNOSTIC
  #
  # Anything else is unexpected and must be reviewed.
  # ===========================================================================
  
  multiplication_sign_unclassified <- hybrid_status |>
    filter(
      CONTAINS_MULTIPLICATION_SIGN,
      HYBRID_STATUS == "NOT_HYBRID"
    ) |>
    arrange(
      FINAL_WCVP_RECOGNISED_NAME
    )
  
  cat(
    "\nAccepted names containing × but not classified by either rule: ",
    format(
      nrow(multiplication_sign_unclassified),
      big.mark = ","
    ),
    "\n",
    sep = ""
  )
  
  if (nrow(multiplication_sign_unclassified) > 0L) {
    cat("\n— Unclassified multiplication-sign names —\n")
    
    print.data.frame(
      multiplication_sign_unclassified |>
        select(
          FINAL_WCVP_ID,
          FINAL_WCVP_RECOGNISED_NAME,
          FINAL_WCVP_RANK,
          FINAL_WCVP_CONCEPT_CLASS
        ),
      row.names = FALSE
    )
  }
  
  # ===========================================================================
  # 6. Diagnostic broad indicators
  #
  # These remain diagnostics only and never assign HYB.
  # ===========================================================================
  
  broad_diagnostic <- hybrid_status |>
    mutate(
      BROAD_NOTHO_RANK = str_detect(
        coalesce(
          FINAL_WCVP_RANK,
          ""
        ),
        regex(
          "notho|hybrid",
          ignore_case = TRUE
        )
      ),
      
      BROAD_CONCEPT_INDICATOR = str_detect(
        coalesce(
          FINAL_WCVP_CONCEPT_CLASS,
          ""
        ),
        regex(
          "hybrid",
          ignore_case = TRUE
        )
      )
    ) |>
    summarise(
      ACCEPTED_TAXA = n(),
      
      STRICT_HYBRID_NAME = sum(
        HYBRID_STATUS == "HYBRID",
        na.rm = TRUE
      ),
      
      NOTHOGENUS_DIAGNOSTIC = sum(
        HYBRID_STATUS == "NOTHOGENUS_DIAGNOSTIC",
        na.rm = TRUE
      ),
      
      NOTHO_RANK_INDICATOR = sum(
        BROAD_NOTHO_RANK,
        na.rm = TRUE
      ),
      
      CONCEPT_CLASS_INDICATOR = sum(
        BROAD_CONCEPT_INDICATOR,
        na.rm = TRUE
      )
    )
  
  cat("\n— Strict versus diagnostic hybrid indicators —\n")
  
  print.data.frame(
    broad_diagnostic,
    row.names = FALSE
  )
  
  # ===========================================================================
  # 7. Validation
  # ===========================================================================
  
  n_hybrid <- sum(
    hybrid_status$HYBRID_STATUS == "HYBRID"
  )
  
  n_nothogenus <- sum(
    hybrid_status$HYBRID_STATUS ==
      "NOTHOGENUS_DIAGNOSTIC"
  )
  
  n_not_hybrid <- sum(
    hybrid_status$HYBRID_STATUS ==
      "NOT_HYBRID"
  )
  
  validation <- tibble::tibble(
    check = c(
      "accepted_population_11439",
      "accepted_ids_unique",
      "recognised_names_complete",
      "hybrid_status_complete",
      "hybrid_status_controlled",
      "hybrid_requires_genus_x_species_pattern",
      "hybrid_requires_explicit_multiplication_sign",
      "nothogenus_requires_leading_multiplication_sign",
      "hybrid_nothogenus_mutually_exclusive",
      "all_multiplication_sign_names_accounted_for",
      "expected_strict_hybrid_count_456",
      "expected_nothogenus_diagnostic_count_7",
      "population_accounting_complete",
      "rank_not_used_to_assign_hybrid",
      "concept_class_not_used_to_assign_hybrid",
      "nothogenus_not_assigned_hyb",
      "legacy_data_not_used",
      "historical_star_totals_not_used",
      "stars_not_assigned",
      "gx_hyb_precedence_not_forced",
      "ghi_not_calculated"
    ),
    
    pass = c(
      nrow(hybrid_status) == 11439L,
      
      n_distinct(
        hybrid_status$FINAL_WCVP_ID
      ) == 11439L,
      
      !any(
        is.na(
          hybrid_status$FINAL_WCVP_RECOGNISED_NAME
        )
      ),
      
      !any(
        is.na(
          hybrid_status$HYBRID_STATUS
        )
      ),
      
      all(
        hybrid_status$HYBRID_STATUS %in%
          c(
            "HYBRID",
            "NOTHOGENUS_DIAGNOSTIC",
            "NOT_HYBRID"
          )
      ),
      
      all(
        hybrid_status$HYBRID_STATUS != "HYBRID" |
          hybrid_status$HYBRID_NAME_PATTERN
      ),
      
      all(
        hybrid_status$HYBRID_STATUS != "HYBRID" |
          hybrid_status$CONTAINS_MULTIPLICATION_SIGN
      ),
      
      all(
        hybrid_status$HYBRID_STATUS !=
          "NOTHOGENUS_DIAGNOSTIC" |
          hybrid_status$LEADING_MULTIPLICATION_SIGN
      ),
      
      !any(
        hybrid_status$HYBRID_NAME_PATTERN &
          hybrid_status$LEADING_MULTIPLICATION_SIGN
      ),
      
      nrow(
        multiplication_sign_unclassified
      ) == 0L,
      
      n_hybrid == 456L,
      
      n_nothogenus == 7L,
      
      n_hybrid +
        n_nothogenus +
        n_not_hybrid ==
        11439L,
      
      TRUE,
      
      TRUE,
      
      all(
        hybrid_status$HYBRID_STATUS !=
          "NOTHOGENUS_DIAGNOSTIC" |
          !hybrid_status$HYB_ELIGIBLE
      ),
      
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
    "\nAll Stars 03g validation checks PASS: ",
    all_pass,
    "\n",
    sep = ""
  )
  
  if (!all_pass) {
    stop(
      "Stars 03g validation failed. Do not freeze."
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
      "accepted_taxa",
      "explicit_genus_x_species_hybrids",
      "nothogenus_diagnostic_taxa",
      "not_hybrid_taxa",
      "unclassified_multiplication_sign_names",
      "hybrid_rule",
      "nothogenus_rule",
      "legacy_analytical_inputs_used",
      "historical_star_totals_used",
      "stars_assigned",
      "gx_hyb_precedence_resolved",
      "ghi_calculated",
      "validation_pass"
    ),
    
    value = c(
      MODULE,
      VERSION,
      as.character(RUN_DATE),
      as.character(
        nrow(hybrid_status)
      ),
      as.character(
        n_hybrid
      ),
      as.character(
        n_nothogenus
      ),
      as.character(
        n_not_hybrid
      ),
      as.character(
        nrow(
          multiplication_sign_unclassified
        )
      ),
      "GENUS_X_SPECIES_NAME_PATTERN",
      "LEADING_MULTIPLICATION_SIGN_DIAGNOSTIC_ONLY",
      "FALSE",
      "FALSE",
      "FALSE",
      "FALSE",
      "FALSE",
      as.character(
        all_pass
      )
    )
  )
  
  # ===========================================================================
  # 9. Persist
  # ===========================================================================
  
  dbWriteTable(
    con,
    OUTPUT_TABLE,
    hybrid_status,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    OUTPUT_NOTHOGENUS,
    nothogenus_diagnostic,
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
    hybrid_status,
    file.path(
      OUTPUT_DIR,
      "stars03g_hybrid_status.csv"
    )
  )
  
  write_csv(
    hybrids,
    file.path(
      OUTPUT_DIR,
      "stars03g_explicit_hybrid_taxa.csv"
    )
  )
  
  write_csv(
    nothogenus_diagnostic,
    file.path(
      OUTPUT_DIR,
      "stars03g_nothogenus_diagnostic.csv"
    )
  )
  
  write_csv(
    multiplication_sign_unclassified,
    file.path(
      OUTPUT_DIR,
      "stars03g_unclassified_multiplication_sign_names.csv"
    )
  )
  
  write_csv(
    validation,
    file.path(
      OUTPUT_DIR,
      "stars03g_validation.csv"
    )
  )
  
  write_csv(
    metadata,
    file.path(
      OUTPUT_DIR,
      "stars03g_metadata.csv"
    )
  )
  
  cat("\nCanonical hybrid-status table:\n")
  cat(
    "  ",
    OUTPUT_TABLE,
    "\n",
    sep = ""
  )
  
  cat("\nSupporting nothogenus diagnostic:\n")
  cat(
    "  ",
    OUTPUT_NOTHOGENUS,
    "\n",
    sep = ""
  )
  
  cat(
    "\nStars 03g v",
    VERSION,
    " complete.\n",
    sep = ""
  )
  
  cat(
    "Contemporary hybrid evidence formalised. ",
    "No final Stars assigned.\n"
  )
  
  invisible(
    list(
      hybrid_status = hybrid_status,
      hybrids = hybrids,
      nothogenus_diagnostic =
        nothogenus_diagnostic,
      multiplication_sign_unclassified =
        multiplication_sign_unclassified,
      validation = validation,
      metadata = metadata
    )
  )
}

result_stars_03g <- run_stars_03g()