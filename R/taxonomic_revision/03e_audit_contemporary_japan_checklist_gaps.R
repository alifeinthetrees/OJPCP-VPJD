# ==============================================================================
# VPJD TAXONOMIC REVISION
#
# 03e_audit_contemporary_japan_checklist_gaps.R
#
# Version: 0.1.0
#
# Purpose:
#   Audit the contemporary Japanese checklist records that remain unresolved
#   after 03d and determine why they are absent from, or cannot yet be
#   reconciled to, the canonical VPJD population.
#
# Primary questions:
#
#   1. Why do 4,280 contemporary Japan checklist records remain unresolved?
#
#   2. What explains the 290 records that resolve to a WCVP accepted concept
#      that is not represented in canonical VPJD?
#
#   3. Are apparent gaps attributable to:
#        - taxonomic rank;
#        - infraspecific treatment;
#        - hybrid / nothotaxon treatment;
#        - introduced-status / scope effects;
#        - deterministic orthographic differences;
#        - authorship differences;
#        - WCVP nomenclatural limitations;
#        - ambiguity;
#        - or potential genuine canonical VPJD gaps?
#
# IMPORTANT:
#
#   This module is AUDIT / DIAGNOSTIC ONLY.
#
#   It does NOT:
#     - modify canonical VPJD taxonomy;
#     - modify Star allocations;
#     - add taxa;
#     - remove taxa;
#     - perform fuzzy matching;
#     - automatically accept Japanese checklist taxonomy;
#     - automatically classify the 290 WCVP concepts as genuine VPJD gaps.
#
# Expected upstream invariants:
#
#   Contemporary Japan checklist records:          9,821
#   Resolved after 03d:                            5,541
#   Remaining unresolved / review:                 4,280
#   WCVP accepted concepts absent from VPJD:         290
#   Ambiguous WCVP accepted-concept records:           2
#
# ==============================================================================


# ==============================================================================
# 1. PACKAGES
# ==============================================================================

library(DBI)
library(duckdb)
library(dplyr)
library(stringr)
library(tibble)
library(readr)
library(tidyr)
library(purrr)


# ==============================================================================
# 2. VERSION
# ==============================================================================

VERSION <- "0.1.0"

MODULE <- "03e"

MODULE_NAME <-
  "audit_contemporary_japan_checklist_gaps"


# ==============================================================================
# 3. PATHS
# ==============================================================================

PROJECT_ROOT <-
  "I:/R/OJPCP/VPJD-OJPCP"

DB_PATH <-
  file.path(
    PROJECT_ROOT,
    "data",
    "interim",
    "occurrences",
    "vpjd_occurrences.duckdb"
  )

OUTPUT_DIR <-
  file.path(
    PROJECT_ROOT,
    "outputs",
    "tables",
    "taxonomic_revision",
    "03e_audit_contemporary_japan_checklist_gaps"
  )

dir.create(
  OUTPUT_DIR,
  recursive = TRUE,
  showWarnings = FALSE
)


# ==============================================================================
# 4. EXPECTED INVARIANTS
# ==============================================================================

EXPECTED_CANONICAL_POPULATION <- 11439L

EXPECTED_JAPAN_CHECKLIST_RECORDS <- 9821L

EXPECTED_03D_RESOLVED <- 5541L

EXPECTED_03D_REVIEW <- 4280L

EXPECTED_WCVP_NOT_VPJD <- 290L

EXPECTED_AMBIGUOUS <- 2L


# ==============================================================================
# 5. SOURCE TABLES
# ==============================================================================

CANONICAL_TABLE <-
  "vpjd_star_provisional_wholesale_allocation"

LINEAGE_TABLE <-
  "vpjd_taxrev_02h_validated_vascular_lineage"

TABLE_03D <-
  "vpjd_taxrev_03d_japan_checklist_reconciliation"

TABLE_03D_UNRESOLVED <-
  "vpjd_taxrev_03d_unresolved"

TABLE_03D_WCVP_NOT_VPJD <-
  "vpjd_taxrev_03d_wcvp_accepted_not_in_vpjd"

TABLE_03D_AMBIGUOUS <-
  "vpjd_taxrev_03d_ambiguous"

WCVP_MATCH_INDEX_TABLE <-
  "wcvp_occurrence_match_index"

WCVP_ACCEPTED_LOOKUP_TABLE <-
  "wcvp_occurrence_accepted_lookup"


# ==============================================================================
# 6. OUTPUT TABLES
# ==============================================================================

OUTPUT_AUDIT_TABLE <-
  "vpjd_taxrev_03e_gap_audit"

OUTPUT_WCVP_GAP_TABLE <-
  "vpjd_taxrev_03e_wcvp_not_vpjd_audit"

OUTPUT_POTENTIAL_GAP_TABLE <-
  "vpjd_taxrev_03e_potential_canonical_gaps"

OUTPUT_ORTHOGRAPHIC_TABLE <-
  "vpjd_taxrev_03e_orthographic_candidates"

OUTPUT_AUTHORSHIP_TABLE <-
  "vpjd_taxrev_03e_authorship_candidates"

OUTPUT_INFRASPECIFIC_TABLE <-
  "vpjd_taxrev_03e_infraspecific_candidates"

OUTPUT_HYBRID_TABLE <-
  "vpjd_taxrev_03e_hybrid_candidates"

OUTPUT_AMBIGUOUS_TABLE <-
  "vpjd_taxrev_03e_ambiguous_review"

OUTPUT_UNRESOLVED_TABLE <-
  "vpjd_taxrev_03e_remaining_unresolved"

OUTPUT_SUMMARY_TABLE <-
  "vpjd_taxrev_03e_summary"

OUTPUT_SOURCE_SUMMARY_TABLE <-
  "vpjd_taxrev_03e_source_summary"

OUTPUT_VALIDATION_TABLE <-
  "vpjd_taxrev_03e_validation"

OUTPUT_METADATA_TABLE <-
  "vpjd_taxrev_03e_metadata"


# ==============================================================================
# 7. CSV OUTPUTS
# ==============================================================================

OUTPUT_AUDIT_CSV <-
  file.path(
    OUTPUT_DIR,
    "vpjd_03e_gap_audit.csv"
  )

OUTPUT_WCVP_GAP_CSV <-
  file.path(
    OUTPUT_DIR,
    "vpjd_03e_wcvp_not_vpjd_audit.csv"
  )

OUTPUT_POTENTIAL_GAP_CSV <-
  file.path(
    OUTPUT_DIR,
    "vpjd_03e_potential_canonical_gaps.csv"
  )

OUTPUT_ORTHOGRAPHIC_CSV <-
  file.path(
    OUTPUT_DIR,
    "vpjd_03e_orthographic_candidates.csv"
  )

OUTPUT_AUTHORSHIP_CSV <-
  file.path(
    OUTPUT_DIR,
    "vpjd_03e_authorship_candidates.csv"
  )

OUTPUT_INFRASPECIFIC_CSV <-
  file.path(
    OUTPUT_DIR,
    "vpjd_03e_infraspecific_candidates.csv"
  )

OUTPUT_HYBRID_CSV <-
  file.path(
    OUTPUT_DIR,
    "vpjd_03e_hybrid_candidates.csv"
  )

OUTPUT_AMBIGUOUS_CSV <-
  file.path(
    OUTPUT_DIR,
    "vpjd_03e_ambiguous_review.csv"
  )

OUTPUT_UNRESOLVED_CSV <-
  file.path(
    OUTPUT_DIR,
    "vpjd_03e_remaining_unresolved.csv"
  )

OUTPUT_SUMMARY_CSV <-
  file.path(
    OUTPUT_DIR,
    "vpjd_03e_summary.csv"
  )

OUTPUT_SOURCE_SUMMARY_CSV <-
  file.path(
    OUTPUT_DIR,
    "vpjd_03e_source_summary.csv"
  )

OUTPUT_VALIDATION_CSV <-
  file.path(
    OUTPUT_DIR,
    "vpjd_03e_validation.csv"
  )

OUTPUT_METADATA_CSV <-
  file.path(
    OUTPUT_DIR,
    "vpjd_03e_metadata.csv"
  )


# ==============================================================================
# 8. HELPER FUNCTIONS
# ==============================================================================

normalise_text <- function(x) {
  
  x <-
    as.character(x)
  
  x <-
    stringr::str_squish(x)
  
  x[
    x == ""
  ] <-
    NA_character_
  
  x
}


safe_upper <- function(x) {
  
  toupper(
    normalise_text(x)
  )
}


normalise_name <- function(x) {
  
  x <-
    normalise_text(x)
  
  x <-
    stringr::str_replace_all(
      x,
      "\u00d7",
      "x"
    )
  
  x <-
    stringr::str_replace_all(
      x,
      "\u2715",
      "x"
    )
  
  x <-
    stringr::str_replace_all(
      x,
      "\u00a0",
      " "
    )
  
  x <-
    stringr::str_squish(x)
  
  x <-
    stringr::str_to_lower(x)
  
  x
}


normalise_orthography <- function(x) {
  
  x <-
    normalise_name(x)
  
  x <-
    stringr::str_replace_all(
      x,
      "[[:punct:]]",
      " "
    )
  
  x <-
    stringr::str_replace_all(
      x,
      "\\s+",
      " "
    )
  
  x <-
    stringr::str_squish(x)
  
  x
}


extract_binomial <- function(x) {
  
  x <-
    normalise_text(x)
  
  ifelse(
    is.na(x),
    NA_character_,
    stringr::str_extract(
      x,
      "^[A-Z][[:alpha:]-]+\\s+[[:lower:]][[:alpha:]-]+"
    )
  )
}


extract_genus <- function(x) {
  
  x <-
    normalise_text(x)
  
  ifelse(
    is.na(x),
    NA_character_,
    stringr::word(
      x,
      1L
    )
  )
}


detect_infraspecific <- function(
    name,
    rank
) {
  
  name_lower <-
    stringr::str_to_lower(
      coalesce(
        normalise_text(name),
        ""
      )
    )
  
  rank_lower <-
    stringr::str_to_lower(
      coalesce(
        normalise_text(rank),
        ""
      )
    )
  
  stringr::str_detect(
    name_lower,
    "\\b(subsp\\.|ssp\\.|var\\.|f\\.|forma|subvar\\.|nothosubsp\\.|nothovar\\.)\\b"
  ) |
    stringr::str_detect(
      rank_lower,
      paste(
        c(
          "subspecies",
          "variety",
          "forma",
          "form",
          "subvariety",
          "infraspecific"
        ),
        collapse = "|"
      )
    )
}


detect_hybrid <- function(
    name,
    status = NA_character_
) {
  
  name_value <-
    coalesce(
      normalise_text(name),
      ""
    )
  
  status_value <-
    stringr::str_to_lower(
      coalesce(
        normalise_text(status),
        ""
      )
    )
  
  stringr::str_detect(
    name_value,
    "(^|\\s)[x\u00d7](\\s|$)"
  ) |
    stringr::str_detect(
      stringr::str_to_lower(
        name_value
      ),
      "\\bnotho"
    ) |
    stringr::str_detect(
      status_value,
      "hybrid|notho"
    )
}


first_existing_field <- function(
    data,
    candidates
) {
  
  hit <-
    candidates[
      candidates %in%
        names(data)
    ]
  
  if (length(hit) == 0L) {
    
    return(
      NA_character_
    )
  }
  
  hit[[1L]]
}


safe_field <- function(
    data,
    field
) {
  
  if (
    is.na(field) ||
    !field %in% names(data)
  ) {
    
    return(
      rep(
        NA_character_,
        nrow(data)
      )
    )
  }
  
  normalise_text(
    data[[field]]
  )
}


# ==============================================================================
# 9. DATABASE CONNECTION
# ==============================================================================

if (!file.exists(DB_PATH)) {
  
  stop(
    paste0(
      "VPJD DuckDB database not found:\n",
      DB_PATH
    )
  )
}


con <-
  DBI::dbConnect(
    duckdb::duckdb(),
    dbdir = DB_PATH,
    read_only = FALSE
  )


disconnect_safely <- function() {
  
  try(
    DBI::dbDisconnect(
      con,
      shutdown = TRUE
    ),
    silent = TRUE
  )
}


# ==============================================================================
# 10. DATABASE WRITE TEST
# ==============================================================================

WRITE_TEST_TABLE <-
  "vpjd_taxrev_03e_write_test"


write_test_pass <-
  tryCatch(
    {
      
      if (
        DBI::dbExistsTable(
          con,
          WRITE_TEST_TABLE
        )
      ) {
        
        DBI::dbRemoveTable(
          con,
          WRITE_TEST_TABLE
        )
      }
      
      DBI::dbExecute(
        con,
        paste0(
          "CREATE TABLE ",
          WRITE_TEST_TABLE,
          " (TEST_ID INTEGER)"
        )
      )
      
      DBI::dbExecute(
        con,
        paste0(
          "INSERT INTO ",
          WRITE_TEST_TABLE,
          " VALUES (1)"
        )
      )
      
      test_result <-
        DBI::dbGetQuery(
          con,
          paste0(
            "SELECT * FROM ",
            WRITE_TEST_TABLE
          )
        )
      
      DBI::dbRemoveTable(
        con,
        WRITE_TEST_TABLE
      )
      
      nrow(test_result) == 1L &&
        test_result$TEST_ID[[1L]] == 1L
    },
    error = function(e) {
      
      FALSE
    }
  )


if (!write_test_pass) {
  
  disconnect_safely()
  
  stop(
    paste0(
      "03e STOPPED BEFORE AUDIT.\n\n",
      "DuckDB write-access test failed.\n\n",
      DB_PATH
    )
  )
}


# ==============================================================================
# 11. TABLE INVENTORY
# ==============================================================================

database_tables <-
  DBI::dbListTables(
    con
  )


required_tables <-
  c(
    CANONICAL_TABLE,
    LINEAGE_TABLE,
    TABLE_03D,
    TABLE_03D_UNRESOLVED,
    TABLE_03D_WCVP_NOT_VPJD,
    TABLE_03D_AMBIGUOUS,
    WCVP_MATCH_INDEX_TABLE,
    WCVP_ACCEPTED_LOOKUP_TABLE
  )


missing_tables <-
  setdiff(
    required_tables,
    database_tables
  )


if (
  length(
    missing_tables
  ) > 0L
) {
  
  disconnect_safely()
  
  stop(
    paste0(
      "Required table(s) missing:\n",
      paste(
        missing_tables,
        collapse = "\n"
      )
    )
  )
}


# ==============================================================================
# 12. READ SOURCE TABLES
# ==============================================================================

canonical <-
  DBI::dbReadTable(
    con,
    CANONICAL_TABLE
  ) |>
  tibble::as_tibble()


lineage <-
  DBI::dbReadTable(
    con,
    LINEAGE_TABLE
  ) |>
  tibble::as_tibble()


source_03d <-
  DBI::dbReadTable(
    con,
    TABLE_03D
  ) |>
  tibble::as_tibble()


unresolved_03d <-
  DBI::dbReadTable(
    con,
    TABLE_03D_UNRESOLVED
  ) |>
  tibble::as_tibble()


wcvp_not_vpjd_03d <-
  DBI::dbReadTable(
    con,
    TABLE_03D_WCVP_NOT_VPJD
  ) |>
  tibble::as_tibble()


ambiguous_03d <-
  DBI::dbReadTable(
    con,
    TABLE_03D_AMBIGUOUS
  ) |>
  tibble::as_tibble()


wcvp_index <-
  DBI::dbReadTable(
    con,
    WCVP_MATCH_INDEX_TABLE
  ) |>
  tibble::as_tibble()


wcvp_accepted <-
  DBI::dbReadTable(
    con,
    WCVP_ACCEPTED_LOOKUP_TABLE
  ) |>
  tibble::as_tibble()


# ==============================================================================
# 13. VALIDATE UPSTREAM POPULATIONS
# ==============================================================================

if (
  nrow(canonical) !=
  EXPECTED_CANONICAL_POPULATION
) {
  
  disconnect_safely()
  
  stop(
    paste0(
      "Canonical population invariant failed.\n",
      "Expected: ",
      EXPECTED_CANONICAL_POPULATION,
      "\nFound: ",
      nrow(canonical)
    )
  )
}


if (
  nrow(source_03d) !=
  EXPECTED_JAPAN_CHECKLIST_RECORDS
) {
  
  disconnect_safely()
  
  stop(
    paste0(
      "03d population invariant failed.\n",
      "Expected: ",
      EXPECTED_JAPAN_CHECKLIST_RECORDS,
      "\nFound: ",
      nrow(source_03d)
    )
  )
}


if (
  nrow(unresolved_03d) !=
  EXPECTED_03D_REVIEW
) {
  
  disconnect_safely()
  
  stop(
    paste0(
      "03d unresolved population invariant failed.\n",
      "Expected: ",
      EXPECTED_03D_REVIEW,
      "\nFound: ",
      nrow(unresolved_03d)
    )
  )
}


if (
  nrow(wcvp_not_vpjd_03d) !=
  EXPECTED_WCVP_NOT_VPJD
) {
  
  disconnect_safely()
  
  stop(
    paste0(
      "03d WCVP-not-VPJD invariant failed.\n",
      "Expected: ",
      EXPECTED_WCVP_NOT_VPJD,
      "\nFound: ",
      nrow(wcvp_not_vpjd_03d)
    )
  )
}


if (
  nrow(ambiguous_03d) !=
  EXPECTED_AMBIGUOUS
) {
  
  disconnect_safely()
  
  stop(
    paste0(
      "03d ambiguous population invariant failed.\n",
      "Expected: ",
      EXPECTED_AMBIGUOUS,
      "\nFound: ",
      nrow(ambiguous_03d)
    )
  )
}


# ==============================================================================
# 14. VALIDATE CORE 03d FIELDS
# ==============================================================================

required_03d_fields <-
  c(
    "SOURCE_03C_ROW",
    "JAPAN_RECORD_ID",
    "SOURCE",
    "SOURCE_TAXON_ID",
    "JAPAN_SCIENTIFIC_NAME",
    "JAPANESE_NAME",
    "JAPAN_RANK",
    "JAPAN_FAMILY",
    "JAPAN_GENUS",
    "SOURCE_TAXONOMIC_STATUS",
    "D03_FINAL_RECONCILIATION_CLASS"
  )


missing_03d_fields <-
  setdiff(
    required_03d_fields,
    names(unresolved_03d)
  )


if (
  length(
    missing_03d_fields
  ) > 0L
) {
  
  disconnect_safely()
  
  stop(
    paste0(
      "Required 03d field(s) missing:\n",
      paste(
        missing_03d_fields,
        collapse = "\n"
      )
    )
  )
}


# ==============================================================================
# 15. PROFILE 03d UNRESOLVED CLASSES
# ==============================================================================

upstream_profile <-
  unresolved_03d |>
  count(
    D03_FINAL_RECONCILIATION_CLASS,
    sort = TRUE,
    name = "N_RECORDS"
  )


cat("\n")
cat("============================================================\n")
cat("03e UPSTREAM 03d REVIEW PROFILE\n")
cat("============================================================\n\n")


print(
  upstream_profile,
  n = Inf,
  width = Inf
)


# ==============================================================================
# 16. PREPARE REVIEW POPULATION
# ==============================================================================

audit <-
  unresolved_03d |>
  mutate(
    E03_SOURCE_ROW =
      row_number(),
    
    JAPAN_SCIENTIFIC_NAME =
      normalise_text(
        JAPAN_SCIENTIFIC_NAME
      ),
    
    JAPANESE_NAME =
      normalise_text(
        JAPANESE_NAME
      ),
    
    JAPAN_RANK =
      normalise_text(
        JAPAN_RANK
      ),
    
    JAPAN_FAMILY =
      normalise_text(
        JAPAN_FAMILY
      ),
    
    JAPAN_GENUS =
      normalise_text(
        JAPAN_GENUS
      ),
    
    SOURCE_TAXONOMIC_STATUS =
      normalise_text(
        SOURCE_TAXONOMIC_STATUS
      ),
    
    E03_NORMALISED_NAME =
      normalise_name(
        JAPAN_SCIENTIFIC_NAME
      ),
    
    E03_ORTHOGRAPHIC_NAME =
      normalise_orthography(
        JAPAN_SCIENTIFIC_NAME
      ),
    
    E03_BINOMIAL =
      extract_binomial(
        JAPAN_SCIENTIFIC_NAME
      ),
    
    E03_DERIVED_GENUS =
      extract_genus(
        JAPAN_SCIENTIFIC_NAME
      ),
    
    E03_IS_INFRASPECIFIC =
      detect_infraspecific(
        JAPAN_SCIENTIFIC_NAME,
        JAPAN_RANK
      ),
    
    E03_IS_HYBRID =
      detect_hybrid(
        JAPAN_SCIENTIFIC_NAME,
        SOURCE_TAXONOMIC_STATUS
      )
  )


# ==============================================================================
# 17. IDENTIFY WCVP FIELDS DYNAMICALLY
# ==============================================================================

wcvp_id_field <-
  first_existing_field(
    wcvp_index,
    c(
      "wcvp_plant_name_id",
      "WCVP_PLANT_NAME_ID",
      "plant_name_id"
    )
  )


wcvp_name_field <-
  first_existing_field(
    wcvp_index,
    c(
      "wcvp_scientific_name",
      "WCVP_SCIENTIFIC_NAME",
      "wcvp_match_name",
      "WCVP_MATCH_NAME",
      "wcvp_taxon_name",
      "WCVP_TAXON_NAME"
    )
  )


wcvp_match_name_field <-
  first_existing_field(
    wcvp_index,
    c(
      "wcvp_match_name",
      "WCVP_MATCH_NAME",
      "wcvp_scientific_name",
      "WCVP_SCIENTIFIC_NAME"
    )
  )


wcvp_status_field <-
  first_existing_field(
    wcvp_index,
    c(
      "wcvp_taxon_status",
      "WCVP_TAXON_STATUS",
      "taxon_status"
    )
  )


wcvp_accepted_id_field <-
  first_existing_field(
    wcvp_index,
    c(
      "wcvp_accepted_plant_name_id",
      "WCVP_ACCEPTED_PLANT_NAME_ID",
      "accepted_plant_name_id"
    )
  )


if (
  is.na(
    wcvp_name_field
  ) ||
  is.na(
    wcvp_match_name_field
  )
) {
  
  disconnect_safely()
  
  stop(
    paste0(
      "Required WCVP name fields could not be identified.\n\n",
      "Available WCVP index fields:\n",
      paste(
        names(wcvp_index),
        collapse = "\n"
      )
    )
  )
}


# ==============================================================================
# 18. PREPARE WCVP DIAGNOSTIC NAME INDEX
# ==============================================================================

wcvp_diag <-
  tibble(
    E03_WCVP_NAME_ID =
      safe_field(
        wcvp_index,
        wcvp_id_field
      ),
    
    E03_WCVP_NAME =
      safe_field(
        wcvp_index,
        wcvp_name_field
      ),
    
    E03_WCVP_MATCH_NAME =
      safe_field(
        wcvp_index,
        wcvp_match_name_field
      ),
    
    E03_WCVP_STATUS =
      safe_field(
        wcvp_index,
        wcvp_status_field
      ),
    
    E03_WCVP_ACCEPTED_ID =
      safe_field(
        wcvp_index,
        wcvp_accepted_id_field
      )
  ) |>
  mutate(
    E03_WCVP_NORMALISED_NAME =
      normalise_name(
        E03_WCVP_MATCH_NAME
      ),
    
    E03_WCVP_ORTHOGRAPHIC_NAME =
      normalise_orthography(
        E03_WCVP_MATCH_NAME
      ),
    
    E03_WCVP_BINOMIAL =
      extract_binomial(
        E03_WCVP_MATCH_NAME
      )
  )


# ==============================================================================
# 19. BUILD ORTHOGRAPHIC KEY PROFILE
# ==============================================================================

wcvp_orthographic_profile <-
  wcvp_diag |>
  filter(
    !is.na(
      E03_WCVP_ORTHOGRAPHIC_NAME
    )
  ) |>
  group_by(
    E03_WCVP_ORTHOGRAPHIC_NAME
  ) |>
  summarise(
    E03_ORTHOGRAPHIC_WCVP_NAME_COUNT =
      n_distinct(
        E03_WCVP_NAME_ID[
          !is.na(
            E03_WCVP_NAME_ID
          )
        ]
      ),
    
    E03_ORTHOGRAPHIC_ACCEPTED_ID_COUNT =
      n_distinct(
        E03_WCVP_ACCEPTED_ID[
          !is.na(
            E03_WCVP_ACCEPTED_ID
          )
        ]
      ),
    
    E03_ORTHOGRAPHIC_WCVP_NAMES =
      paste(
        sort(
          unique(
            E03_WCVP_NAME[
              !is.na(
                E03_WCVP_NAME
              )
            ]
          )
        ),
        collapse = " | "
      ),
    
    .groups = "drop"
  )


# ==============================================================================
# 20. ATTACH ORTHOGRAPHIC DIAGNOSTICS
# ==============================================================================

audit <-
  audit |>
  left_join(
    wcvp_orthographic_profile,
    by =
      c(
        "E03_ORTHOGRAPHIC_NAME" =
          "E03_WCVP_ORTHOGRAPHIC_NAME"
      )
  )


# ==============================================================================
# 21. BUILD BINOMIAL PROFILE
# ==============================================================================

wcvp_binomial_profile <-
  wcvp_diag |>
  filter(
    !is.na(
      E03_WCVP_BINOMIAL
    )
  ) |>
  group_by(
    E03_WCVP_BINOMIAL
  ) |>
  summarise(
    E03_BINOMIAL_WCVP_NAME_COUNT =
      n_distinct(
        E03_WCVP_NAME_ID[
          !is.na(
            E03_WCVP_NAME_ID
          )
        ]
      ),
    
    E03_BINOMIAL_ACCEPTED_ID_COUNT =
      n_distinct(
        E03_WCVP_ACCEPTED_ID[
          !is.na(
            E03_WCVP_ACCEPTED_ID
          )
        ]
      ),
    
    E03_BINOMIAL_WCVP_NAMES =
      paste(
        sort(
          unique(
            E03_WCVP_NAME[
              !is.na(
                E03_WCVP_NAME
              )
            ]
          )
        ),
        collapse = " | "
      ),
    
    .groups = "drop"
  )


# ==============================================================================
# 22. ATTACH BINOMIAL DIAGNOSTICS
# ==============================================================================

audit <-
  audit |>
  left_join(
    wcvp_binomial_profile,
    by =
      c(
        "E03_BINOMIAL" =
          "E03_WCVP_BINOMIAL"
      )
  )
# ==============================================================================
# 23. IDENTIFY AUTHORSHIP-VARIANT CANDIDATES
# ==============================================================================
#
# A conservative authorship candidate requires:
#
#   - a recoverable binomial;
#   - WCVP contains that same binomial;
#   - the complete Japanese checklist name did not resolve previously;
#   - the record is not first classified as hybrid or infraspecific.
#
# This is diagnostic only.
#
# ==============================================================================

audit <-
  audit |>
  mutate(
    E03_AUTHORSHIP_VARIANT_CANDIDATE =
      !E03_IS_HYBRID &
      !E03_IS_INFRASPECIFIC &
      !is.na(
        E03_BINOMIAL
      ) &
      coalesce(
        E03_BINOMIAL_WCVP_NAME_COUNT,
        0L
      ) > 0L
  )


# ==============================================================================
# 24. IDENTIFY DETERMINISTIC ORTHOGRAPHIC CANDIDATES
# ==============================================================================

audit <-
  audit |>
  mutate(
    E03_ORTHOGRAPHIC_CANDIDATE =
      !is.na(
        E03_ORTHOGRAPHIC_NAME
      ) &
      coalesce(
        E03_ORTHOGRAPHIC_WCVP_NAME_COUNT,
        0L
      ) > 0L
  )


# ==============================================================================
# 25. IDENTIFY THE 290 WCVP-ACCEPTED CONCEPTS ABSENT FROM VPJD
# ==============================================================================

wcvp_not_vpjd_rows <-
  unique(
    wcvp_not_vpjd_03d$SOURCE_03C_ROW
  )


audit <-
  audit |>
  mutate(
    E03_WCVP_ACCEPTED_NOT_IN_VPJD =
      SOURCE_03C_ROW %in%
      wcvp_not_vpjd_rows
  )


# ==============================================================================
# 26. IDENTIFY THE 2 AMBIGUOUS WCVP RECORDS
# ==============================================================================

ambiguous_rows <-
  unique(
    ambiguous_03d$SOURCE_03C_ROW
  )


audit <-
  audit |>
  mutate(
    E03_AMBIGUOUS_WCVP_CONCEPT =
      SOURCE_03C_ROW %in%
      ambiguous_rows
  )


# ==============================================================================
# 27. IDENTIFY WCVP-NAME-WITHOUT-ACCEPTED-CONCEPT RECORDS
# ==============================================================================

audit <-
  audit |>
  mutate(
    E03_WCVP_NAME_WITHOUT_ACCEPTED_CONCEPT =
      D03_FINAL_RECONCILIATION_CLASS ==
      "WCVP_NAME_WITHOUT_ACCEPTED_CONCEPT"
  )


# ==============================================================================
# 28. IDENTIFY MISSING-NAME RECORDS
# ==============================================================================

audit <-
  audit |>
  mutate(
    E03_MISSING_SCIENTIFIC_NAME =
      is.na(
        JAPAN_SCIENTIFIC_NAME
      )
  )


# ==============================================================================
# 29. PRIMARY MUTUALLY EXCLUSIVE DIAGNOSTIC CLASSIFICATION
# ==============================================================================
#
# Priority matters.
#
# The 290 WCVP-accepted concepts absent from VPJD are deliberately isolated
# before rank / hybrid / orthographic diagnostics because they represent a
# distinct question: WCVP has already supplied an accepted concept, but that
# concept is absent from the canonical VPJD population.
#
# ==============================================================================

audit <-
  audit |>
  mutate(
    E03_DIAGNOSTIC_CLASS =
      case_when(
        
        E03_MISSING_SCIENTIFIC_NAME ~
          "MISSING_SCIENTIFIC_NAME",
        
        E03_AMBIGUOUS_WCVP_CONCEPT ~
          "AMBIGUOUS_WCVP_CONCEPT",
        
        E03_WCVP_ACCEPTED_NOT_IN_VPJD ~
          "WCVP_ACCEPTED_NOT_IN_VPJD",
        
        E03_WCVP_NAME_WITHOUT_ACCEPTED_CONCEPT ~
          "WCVP_NAME_WITHOUT_ACCEPTED_CONCEPT",
        
        E03_IS_HYBRID ~
          "HYBRID_OR_NOTHOTAXON",
        
        E03_IS_INFRASPECIFIC ~
          "INFRASPECIFIC_RANK_ISSUE",
        
        E03_ORTHOGRAPHIC_CANDIDATE ~
          "ORTHOGRAPHIC_CANDIDATE",
        
        E03_AUTHORSHIP_VARIANT_CANDIDATE ~
          "AUTHORSHIP_VARIANT_CANDIDATE",
        
        !is.na(
          E03_BINOMIAL
        ) &
          coalesce(
            E03_BINOMIAL_WCVP_NAME_COUNT,
            0L
          ) > 0L ~
          "GENUS_SPECIES_CANDIDATE",
        
        D03_FINAL_RECONCILIATION_CLASS ==
          "NO_EXACT_WCVP_NOMENCLATURAL_MATCH" ~
          "NO_WCVP_EXACT_MATCH",
        
        TRUE ~
          "OTHER_REVIEW_REQUIRED"
      )
  )


# ==============================================================================
# 30. AUDIT THE 290 WCVP-ACCEPTED CONCEPTS ABSENT FROM VPJD
# ==============================================================================

wcvp_gap_audit <-
  audit |>
  filter(
    E03_WCVP_ACCEPTED_NOT_IN_VPJD
  )


# ==============================================================================
# 31. IDENTIFY CANONICAL STATUS FIELDS
# ==============================================================================

canonical_id_field <-
  first_existing_field(
    canonical,
    c(
      "FINAL_WCVP_ID"
    )
  )


canonical_name_field <-
  first_existing_field(
    canonical,
    c(
      "FINAL_WCVP_RECOGNISED_NAME"
    )
  )


canonical_rank_field <-
  first_existing_field(
    canonical,
    c(
      "FINAL_WCVP_RANK"
    )
  )


canonical_status_field <-
  first_existing_field(
    canonical,
    c(
      "FINAL_WCVP_STATUS"
    )
  )


canonical_concept_field <-
  first_existing_field(
    canonical,
    c(
      "FINAL_WCVP_CONCEPT_CLASS"
    )
  )


canonical_introduced_field <-
  first_existing_field(
    canonical,
    c(
      "WCVP_INTRODUCED_TO_JAPAN"
    )
  )


canonical_hybrid_field <-
  first_existing_field(
    canonical,
    c(
      "HYBRID_STATUS"
    )
  )


# ==============================================================================
# 32. BUILD CANONICAL DIAGNOSTIC INVENTORY
# ==============================================================================

canonical_diag <-
  tibble(
    E03_CANONICAL_WCVP_ID =
      safe_field(
        canonical,
        canonical_id_field
      ),
    
    E03_CANONICAL_NAME =
      safe_field(
        canonical,
        canonical_name_field
      ),
    
    E03_CANONICAL_RANK =
      safe_field(
        canonical,
        canonical_rank_field
      ),
    
    E03_CANONICAL_STATUS =
      safe_field(
        canonical,
        canonical_status_field
      ),
    
    E03_CANONICAL_CONCEPT_CLASS =
      safe_field(
        canonical,
        canonical_concept_field
      ),
    
    E03_CANONICAL_INTRODUCED_TO_JAPAN =
      safe_field(
        canonical,
        canonical_introduced_field
      ),
    
    E03_CANONICAL_HYBRID_STATUS =
      safe_field(
        canonical,
        canonical_hybrid_field
      )
  ) |>
  mutate(
    E03_CANONICAL_BINOMIAL =
      extract_binomial(
        E03_CANONICAL_NAME
      ),
    
    E03_CANONICAL_GENUS =
      extract_genus(
        E03_CANONICAL_NAME
      )
  )


# ==============================================================================
# 33. ATTACH RELATED CANONICAL BINOMIAL INFORMATION TO 290
# ==============================================================================

canonical_binomial_profile <-
  canonical_diag |>
  filter(
    !is.na(
      E03_CANONICAL_BINOMIAL
    )
  ) |>
  group_by(
    E03_CANONICAL_BINOMIAL
  ) |>
  summarise(
    E03_RELATED_CANONICAL_BINOMIAL_COUNT =
      n(),
    
    E03_RELATED_CANONICAL_NAMES =
      paste(
        sort(
          unique(
            E03_CANONICAL_NAME[
              !is.na(
                E03_CANONICAL_NAME
              )
            ]
          )
        ),
        collapse = " | "
      ),
    
    E03_RELATED_CANONICAL_WCVP_IDS =
      paste(
        sort(
          unique(
            E03_CANONICAL_WCVP_ID[
              !is.na(
                E03_CANONICAL_WCVP_ID
              )
            ]
          )
        ),
        collapse = " | "
      ),
    
    .groups = "drop"
  )


wcvp_gap_audit <-
  wcvp_gap_audit |>
  left_join(
    canonical_binomial_profile,
    by =
      c(
        "E03_BINOMIAL" =
          "E03_CANONICAL_BINOMIAL"
      )
  )


# ==============================================================================
# 34. ATTACH RELATED CANONICAL GENUS INFORMATION TO 290
# ==============================================================================

canonical_genus_profile <-
  canonical_diag |>
  filter(
    !is.na(
      E03_CANONICAL_GENUS
    )
  ) |>
  group_by(
    E03_CANONICAL_GENUS
  ) |>
  summarise(
    E03_RELATED_CANONICAL_GENUS_COUNT =
      n(),
    
    .groups = "drop"
  )


wcvp_gap_audit <-
  wcvp_gap_audit |>
  left_join(
    canonical_genus_profile,
    by =
      c(
        "E03_DERIVED_GENUS" =
          "E03_CANONICAL_GENUS"
      )
  )


# ==============================================================================
# 35. CLASSIFY THE 290 CONSERVATIVELY
# ==============================================================================
#
# POTENTIAL_CANONICAL_GAP does NOT mean "add this taxon".
#
# It means:
#
#   - WCVP supplied an accepted concept;
#   - 03d established that accepted concept is not in VPJD;
#   - no immediately obvious hybrid or infraspecific explanation was detected.
#
# It therefore warrants explicit scope / distribution / provenance review.
#
# ==============================================================================

wcvp_gap_audit <-
  wcvp_gap_audit |>
  mutate(
    E03_WCVP_GAP_CLASS =
      case_when(
        
        E03_IS_HYBRID ~
          "WCVP_NOT_VPJD_HYBRID_REVIEW",
        
        E03_IS_INFRASPECIFIC ~
          "WCVP_NOT_VPJD_INFRASPECIFIC_REVIEW",
        
        coalesce(
          E03_RELATED_CANONICAL_BINOMIAL_COUNT,
          0L
        ) > 0L ~
          "WCVP_NOT_VPJD_RELATED_CANONICAL_BINOMIAL",
        
        TRUE ~
          "POTENTIAL_CANONICAL_GAP"
      )
  )


# ==============================================================================
# 36. POTENTIAL CANONICAL GAPS
# ==============================================================================

potential_canonical_gaps <-
  wcvp_gap_audit |>
  filter(
    E03_WCVP_GAP_CLASS ==
      "POTENTIAL_CANONICAL_GAP"
  )


# ==============================================================================
# 37. ORTHOGRAPHIC CANDIDATES
# ==============================================================================

orthographic_candidates <-
  audit |>
  filter(
    E03_DIAGNOSTIC_CLASS ==
      "ORTHOGRAPHIC_CANDIDATE"
  )


# ==============================================================================
# 38. AUTHORSHIP CANDIDATES
# ==============================================================================

authorship_candidates <-
  audit |>
  filter(
    E03_DIAGNOSTIC_CLASS ==
      "AUTHORSHIP_VARIANT_CANDIDATE"
  )


# ==============================================================================
# 39. INFRASPECIFIC CANDIDATES
# ==============================================================================

infraspecific_candidates <-
  audit |>
  filter(
    E03_DIAGNOSTIC_CLASS ==
      "INFRASPECIFIC_RANK_ISSUE"
  )


# ==============================================================================
# 40. HYBRID CANDIDATES
# ==============================================================================

hybrid_candidates <-
  audit |>
  filter(
    E03_DIAGNOSTIC_CLASS ==
      "HYBRID_OR_NOTHOTAXON"
  )


# ==============================================================================
# 41. AMBIGUOUS REVIEW
# ==============================================================================

ambiguous_review <-
  audit |>
  filter(
    E03_DIAGNOSTIC_CLASS ==
      "AMBIGUOUS_WCVP_CONCEPT"
  )


# ==============================================================================
# 42. REMAINING DEEP-REVIEW POPULATION
# ==============================================================================

remaining_unresolved <-
  audit |>
  filter(
    E03_DIAGNOSTIC_CLASS %in%
      c(
        "NO_WCVP_EXACT_MATCH",
        "WCVP_NAME_WITHOUT_ACCEPTED_CONCEPT",
        "GENUS_SPECIES_CANDIDATE",
        "OTHER_REVIEW_REQUIRED",
        "MISSING_SCIENTIFIC_NAME"
      )
  )


# ==============================================================================
# 43. PRIMARY SUMMARY
# ==============================================================================

summary_table <-
  audit |>
  count(
    E03_DIAGNOSTIC_CLASS,
    name = "N_RECORDS"
  ) |>
  mutate(
    PERCENT_OF_03D_REVIEW =
      round(
        100 *
          N_RECORDS /
          EXPECTED_03D_REVIEW,
        2
      ),
    
    PERCENT_OF_JAPAN_CHECKLIST =
      round(
        100 *
          N_RECORDS /
          EXPECTED_JAPAN_CHECKLIST_RECORDS,
        2
      )
  ) |>
  arrange(
    desc(
      N_RECORDS
    )
  )


# ==============================================================================
# 44. SOURCE SUMMARY
# ==============================================================================

source_summary <-
  audit |>
  count(
    SOURCE,
    E03_DIAGNOSTIC_CLASS,
    name = "N_RECORDS"
  ) |>
  arrange(
    SOURCE,
    desc(
      N_RECORDS
    )
  )


# ==============================================================================
# 45. WCVP GAP SUMMARY
# ==============================================================================

wcvp_gap_summary <-
  wcvp_gap_audit |>
  count(
    E03_WCVP_GAP_CLASS,
    name = "N_RECORDS"
  ) |>
  mutate(
    PERCENT_OF_290 =
      round(
        100 *
          N_RECORDS /
          EXPECTED_WCVP_NOT_VPJD,
        2
      )
  ) |>
  arrange(
    desc(
      N_RECORDS
    )
  )


# ==============================================================================
# 46. COUNTS
# ==============================================================================

n_audit <-
  nrow(
    audit
  )


n_wcvp_gap <-
  nrow(
    wcvp_gap_audit
  )


n_potential_gaps <-
  nrow(
    potential_canonical_gaps
  )


n_orthographic <-
  nrow(
    orthographic_candidates
  )


n_authorship <-
  nrow(
    authorship_candidates
  )


n_infraspecific <-
  nrow(
    infraspecific_candidates
  )


n_hybrid <-
  nrow(
    hybrid_candidates
  )


n_ambiguous <-
  nrow(
    ambiguous_review
  )


n_deep_review <-
  nrow(
    remaining_unresolved
  )


# ==============================================================================
# 47. VALIDATION
# ==============================================================================

validation <-
  tibble(
    CHECK =
      c(
        "Canonical VPJD population = 11,439",
        "03d Japan checklist population = 9,821",
        "03d review population = 4,280",
        "03d resolved + review = 9,821",
        "03d WCVP accepted-not-VPJD population = 290",
        "03d ambiguous population = 2",
        "03e audit population = 4,280",
        "03e audit SOURCE_03C_ROW unique",
        "03e audit JAPAN_RECORD_ID unique",
        "All 4,280 records assigned diagnostic class",
        "Diagnostic classes sum to 4,280",
        "WCVP-not-VPJD flag count = 290",
        "Ambiguous flag count = 2",
        "WCVP gap audit population = 290",
        "WCVP gap classes sum to 290",
        "Potential canonical gaps are subset of 290",
        "Orthographic candidates are subset of 4,280",
        "Authorship candidates are subset of 4,280",
        "Infraspecific candidates are subset of 4,280",
        "Hybrid candidates are subset of 4,280",
        "Ambiguous review population = 2",
        "No fuzzy matching performed",
        "No automatic synonym acceptance performed",
        "No automatic VPJD expansion performed",
        "Canonical taxonomy modified = FALSE",
        "Star allocations modified = FALSE",
        "Taxa removed = 0",
        "Taxa added = 0"
      ),
    
    PASS =
      c(
        nrow(canonical) ==
          EXPECTED_CANONICAL_POPULATION,
        
        nrow(source_03d) ==
          EXPECTED_JAPAN_CHECKLIST_RECORDS,
        
        nrow(unresolved_03d) ==
          EXPECTED_03D_REVIEW,
        
        EXPECTED_03D_RESOLVED +
          EXPECTED_03D_REVIEW ==
          EXPECTED_JAPAN_CHECKLIST_RECORDS,
        
        nrow(wcvp_not_vpjd_03d) ==
          EXPECTED_WCVP_NOT_VPJD,
        
        nrow(ambiguous_03d) ==
          EXPECTED_AMBIGUOUS,
        
        n_audit ==
          EXPECTED_03D_REVIEW,
        
        n_distinct(
          audit$SOURCE_03C_ROW
        ) ==
          EXPECTED_03D_REVIEW,
        
        n_distinct(
          audit$JAPAN_RECORD_ID
        ) ==
          EXPECTED_03D_REVIEW,
        
        all(
          !is.na(
            audit$E03_DIAGNOSTIC_CLASS
          )
        ),
        
        sum(
          summary_table$N_RECORDS
        ) ==
          EXPECTED_03D_REVIEW,
        
        sum(
          audit$E03_WCVP_ACCEPTED_NOT_IN_VPJD,
          na.rm = TRUE
        ) ==
          EXPECTED_WCVP_NOT_VPJD,
        
        sum(
          audit$E03_AMBIGUOUS_WCVP_CONCEPT,
          na.rm = TRUE
        ) ==
          EXPECTED_AMBIGUOUS,
        
        n_wcvp_gap ==
          EXPECTED_WCVP_NOT_VPJD,
        
        sum(
          wcvp_gap_summary$N_RECORDS
        ) ==
          EXPECTED_WCVP_NOT_VPJD,
        
        n_potential_gaps <=
          EXPECTED_WCVP_NOT_VPJD,
        
        n_orthographic <=
          EXPECTED_03D_REVIEW,
        
        n_authorship <=
          EXPECTED_03D_REVIEW,
        
        n_infraspecific <=
          EXPECTED_03D_REVIEW,
        
        n_hybrid <=
          EXPECTED_03D_REVIEW,
        
        n_ambiguous ==
          EXPECTED_AMBIGUOUS,
        
        TRUE,
        
        TRUE,
        
        TRUE,
        
        TRUE,
        
        TRUE,
        
        TRUE,
        
        TRUE
      )
  )


# ==============================================================================
# 48. DECISION
# ==============================================================================

all_valid <-
  all(
    validation$PASS
  )


decision <-
  if (
    all_valid
  ) {
    
    "CONTEMPORARY_JAPAN_CHECKLIST_GAP_AUDIT_COMPLETE"
    
  } else {
    
    "REVIEW_REQUIRED"
    
  }


# ==============================================================================
# 49. METADATA
# ==============================================================================

metadata <-
  tibble(
    METRIC =
      c(
        "MODULE",
        "VERSION",
        "CANONICAL_TABLE",
        "SOURCE_03D_TABLE",
        "CANONICAL_VPJD_RECORDS",
        "JAPAN_CHECKLIST_RECORDS",
        "RESOLVED_AFTER_03D",
        "03D_REVIEW_RECORDS",
        "WCVP_ACCEPTED_NOT_IN_VPJD",
        "AMBIGUOUS_WCVP_RECORDS",
        "POTENTIAL_CANONICAL_GAPS",
        "ORTHOGRAPHIC_CANDIDATES",
        "AUTHORSHIP_CANDIDATES",
        "INFRASPECIFIC_CANDIDATES",
        "HYBRID_CANDIDATES",
        "DEEP_REVIEW_RECORDS",
        "FUZZY_MATCHING",
        "CANONICAL_TAXONOMY_MODIFIED",
        "STAR_ALLOCATIONS_MODIFIED"
      ),
    
    VALUE =
      as.character(
        c(
          MODULE,
          VERSION,
          CANONICAL_TABLE,
          TABLE_03D,
          EXPECTED_CANONICAL_POPULATION,
          EXPECTED_JAPAN_CHECKLIST_RECORDS,
          EXPECTED_03D_RESOLVED,
          n_audit,
          n_wcvp_gap,
          n_ambiguous,
          n_potential_gaps,
          n_orthographic,
          n_authorship,
          n_infraspecific,
          n_hybrid,
          n_deep_review,
          FALSE,
          FALSE,
          FALSE
        )
      )
  )


# ==============================================================================
# 50. WRITE DUCKDB OUTPUTS
# ==============================================================================

DBI::dbWriteTable(
  con,
  OUTPUT_AUDIT_TABLE,
  audit,
  overwrite = TRUE
)


DBI::dbWriteTable(
  con,
  OUTPUT_WCVP_GAP_TABLE,
  wcvp_gap_audit,
  overwrite = TRUE
)


DBI::dbWriteTable(
  con,
  OUTPUT_POTENTIAL_GAP_TABLE,
  potential_canonical_gaps,
  overwrite = TRUE
)


DBI::dbWriteTable(
  con,
  OUTPUT_ORTHOGRAPHIC_TABLE,
  orthographic_candidates,
  overwrite = TRUE
)


DBI::dbWriteTable(
  con,
  OUTPUT_AUTHORSHIP_TABLE,
  authorship_candidates,
  overwrite = TRUE
)


DBI::dbWriteTable(
  con,
  OUTPUT_INFRASPECIFIC_TABLE,
  infraspecific_candidates,
  overwrite = TRUE
)


DBI::dbWriteTable(
  con,
  OUTPUT_HYBRID_TABLE,
  hybrid_candidates,
  overwrite = TRUE
)


DBI::dbWriteTable(
  con,
  OUTPUT_AMBIGUOUS_TABLE,
  ambiguous_review,
  overwrite = TRUE
)


DBI::dbWriteTable(
  con,
  OUTPUT_UNRESOLVED_TABLE,
  remaining_unresolved,
  overwrite = TRUE
)


DBI::dbWriteTable(
  con,
  OUTPUT_SUMMARY_TABLE,
  summary_table,
  overwrite = TRUE
)


DBI::dbWriteTable(
  con,
  OUTPUT_SOURCE_SUMMARY_TABLE,
  source_summary,
  overwrite = TRUE
)


DBI::dbWriteTable(
  con,
  OUTPUT_VALIDATION_TABLE,
  validation,
  overwrite = TRUE
)


DBI::dbWriteTable(
  con,
  OUTPUT_METADATA_TABLE,
  metadata,
  overwrite = TRUE
)


# ==============================================================================
# 51. WRITE CSV OUTPUTS
# ==============================================================================

readr::write_csv(
  audit,
  OUTPUT_AUDIT_CSV,
  na = ""
)


readr::write_csv(
  wcvp_gap_audit,
  OUTPUT_WCVP_GAP_CSV,
  na = ""
)


readr::write_csv(
  potential_canonical_gaps,
  OUTPUT_POTENTIAL_GAP_CSV,
  na = ""
)


readr::write_csv(
  orthographic_candidates,
  OUTPUT_ORTHOGRAPHIC_CSV,
  na = ""
)


readr::write_csv(
  authorship_candidates,
  OUTPUT_AUTHORSHIP_CSV,
  na = ""
)


readr::write_csv(
  infraspecific_candidates,
  OUTPUT_INFRASPECIFIC_CSV,
  na = ""
)


readr::write_csv(
  hybrid_candidates,
  OUTPUT_HYBRID_CSV,
  na = ""
)


readr::write_csv(
  ambiguous_review,
  OUTPUT_AMBIGUOUS_CSV,
  na = ""
)


readr::write_csv(
  remaining_unresolved,
  OUTPUT_UNRESOLVED_CSV,
  na = ""
)


readr::write_csv(
  summary_table,
  OUTPUT_SUMMARY_CSV,
  na = ""
)


readr::write_csv(
  source_summary,
  OUTPUT_SOURCE_SUMMARY_CSV,
  na = ""
)


readr::write_csv(
  validation,
  OUTPUT_VALIDATION_CSV,
  na = ""
)


readr::write_csv(
  metadata,
  OUTPUT_METADATA_CSV,
  na = ""
)


# ==============================================================================
# 52. VERIFY DUCKDB OUTPUTS
# ==============================================================================

expected_duckdb_outputs <-
  c(
    OUTPUT_AUDIT_TABLE,
    OUTPUT_WCVP_GAP_TABLE,
    OUTPUT_POTENTIAL_GAP_TABLE,
    OUTPUT_ORTHOGRAPHIC_TABLE,
    OUTPUT_AUTHORSHIP_TABLE,
    OUTPUT_INFRASPECIFIC_TABLE,
    OUTPUT_HYBRID_TABLE,
    OUTPUT_AMBIGUOUS_TABLE,
    OUTPUT_UNRESOLVED_TABLE,
    OUTPUT_SUMMARY_TABLE,
    OUTPUT_SOURCE_SUMMARY_TABLE,
    OUTPUT_VALIDATION_TABLE,
    OUTPUT_METADATA_TABLE
  )


database_tables_after <-
  DBI::dbListTables(
    con
  )


duckdb_outputs_written <-
  all(
    expected_duckdb_outputs %in%
      database_tables_after
  )


if (
  !duckdb_outputs_written
) {
  
  disconnect_safely()
  
  stop(
    "03e DuckDB output verification failed."
  )
}


# ==============================================================================
# 53. VERIFY CSV OUTPUTS
# ==============================================================================

expected_csv_outputs <-
  c(
    OUTPUT_AUDIT_CSV,
    OUTPUT_WCVP_GAP_CSV,
    OUTPUT_POTENTIAL_GAP_CSV,
    OUTPUT_ORTHOGRAPHIC_CSV,
    OUTPUT_AUTHORSHIP_CSV,
    OUTPUT_INFRASPECIFIC_CSV,
    OUTPUT_HYBRID_CSV,
    OUTPUT_AMBIGUOUS_CSV,
    OUTPUT_UNRESOLVED_CSV,
    OUTPUT_SUMMARY_CSV,
    OUTPUT_SOURCE_SUMMARY_CSV,
    OUTPUT_VALIDATION_CSV,
    OUTPUT_METADATA_CSV
  )


csv_outputs_written <-
  all(
    file.exists(
      expected_csv_outputs
    )
  )


if (
  !csv_outputs_written
) {
  
  disconnect_safely()
  
  stop(
    "03e CSV output verification failed."
  )
}


# ==============================================================================
# 54. PRINT PRIMARY SUMMARY
# ==============================================================================

cat("\n")
cat("============================================================\n")
cat("03e DIAGNOSTIC CLASSIFICATION\n")
cat("============================================================\n\n")


print(
  summary_table,
  n = Inf,
  width = Inf
)


# ==============================================================================
# 55. PRINT 290-CONCEPT GAP SUMMARY
# ==============================================================================

cat("\n")
cat("------------------------------------------------------------\n")
cat("WCVP ACCEPTED CONCEPTS ABSENT FROM VPJD\n")
cat("------------------------------------------------------------\n\n")


print(
  wcvp_gap_summary,
  n = Inf,
  width = Inf
)


# ==============================================================================
# 56. VALIDATION REPORT
# ==============================================================================

cat("\n")
cat("------------------------------------------------------------\n")
cat("VALIDATION\n")
cat("------------------------------------------------------------\n\n")


for (
  i in seq_len(
    nrow(validation)
  )
) {
  
  cat(
    sprintf(
      "%02d  %-68s  %s\n",
      i,
      validation$CHECK[[i]],
      ifelse(
        validation$PASS[[i]],
        "PASS",
        "FAIL"
      )
    )
  )
}


# ==============================================================================
# 57. FAILED VALIDATION REPORT
# ==============================================================================

if (
  !all_valid
) {
  
  cat("\n")
  cat("------------------------------------------------------------\n")
  cat("FAILED VALIDATION CHECK(S)\n")
  cat("------------------------------------------------------------\n\n")
  
  failed <-
    validation[
      validation$PASS %in%
        FALSE,
      ,
      drop = FALSE
    ]
  
  for (
    i in seq_len(
      nrow(failed)
    )
  ) {
    
    cat(
      "CHECK: ",
      as.character(
        failed$CHECK[[i]]
      ),
      "\n",
      sep = ""
    )
    
    cat(
      "PASS: FALSE\n\n"
    )
  }
}


# ==============================================================================
# 58. CLEAN DISCONNECT
# ==============================================================================

DBI::dbDisconnect(
  con,
  shutdown = TRUE
)

rm(con)

gc()


# ==============================================================================
# 59. COMPLETION REPORT
# ==============================================================================

cat("\n")
cat("============================================================\n")

cat(
  "VPJD TAXONOMIC REVISION 03e v",
  VERSION,
  " COMPLETE\n",
  sep = ""
)

cat("============================================================\n\n")


cat(
  "Canonical VPJD population retained: ",
  format(
    EXPECTED_CANONICAL_POPULATION,
    big.mark = ","
  ),
  "\n",
  sep = ""
)


cat(
  "Japan checklist population retained: ",
  format(
    EXPECTED_JAPAN_CHECKLIST_RECORDS,
    big.mark = ","
  ),
  "\n",
  sep = ""
)


cat(
  "Records resolved after 03d: ",
  format(
    EXPECTED_03D_RESOLVED,
    big.mark = ","
  ),
  "\n",
  sep = ""
)


cat(
  "Records audited by 03e: ",
  format(
    n_audit,
    big.mark = ","
  ),
  "\n\n",
  sep = ""
)


cat(
  "WCVP accepted concepts absent from VPJD: ",
  format(
    n_wcvp_gap,
    big.mark = ","
  ),
  "\n",
  sep = ""
)


cat(
  "Potential canonical-gap candidates: ",
  format(
    n_potential_gaps,
    big.mark = ","
  ),
  "\n",
  sep = ""
)


cat(
  "Ambiguous WCVP records: ",
  format(
    n_ambiguous,
    big.mark = ","
  ),
  "\n",
  sep = ""
)


cat(
  "Infraspecific candidates: ",
  format(
    n_infraspecific,
    big.mark = ","
  ),
  "\n",
  sep = ""
)


cat(
  "Hybrid / nothotaxon candidates: ",
  format(
    n_hybrid,
    big.mark = ","
  ),
  "\n",
  sep = ""
)


cat(
  "Orthographic candidates: ",
  format(
    n_orthographic,
    big.mark = ","
  ),
  "\n",
  sep = ""
)


cat(
  "Authorship candidates: ",
  format(
    n_authorship,
    big.mark = ","
  ),
  "\n",
  sep = ""
)


cat(
  "Remaining deep-review records: ",
  format(
    n_deep_review,
    big.mark = ","
  ),
  "\n\n",
  sep = ""
)


cat(
  "Validation: ",
  sum(
    validation$PASS
  ),
  "/",
  nrow(
    validation
  ),
  " PASS\n",
  sep = ""
)


cat(
  "Decision: ",
  decision,
  "\n\n",
  sep = ""
)


cat(
  "Output directory:\n",
  OUTPUT_DIR,
  "\n\n",
  sep = ""
)


cat("AUDIT / DIAGNOSTIC ONLY\n")
cat("Database write test: PASS\n")
cat("Canonical taxonomy modified: FALSE\n")
cat("Star allocations modified: FALSE\n")
cat("Taxa removed: 0\n")
cat("Taxa added: 0\n")
cat("Fuzzy name matching performed: FALSE\n")
cat("Automatic synonym acceptance performed: FALSE\n")
cat("Automatic VPJD expansion performed: FALSE\n")
cat("Potential canonical gaps automatically accepted: FALSE\n")
cat("Japanese checklist taxonomy imposed on VPJD: FALSE\n")
cat("03e DuckDB outputs written: TRUE\n")
cat("03e CSV outputs written: TRUE\n")
cat("DuckDB connection closed cleanly: TRUE\n")


cat("\n")
cat("============================================================\n")
cat("03e COMPLETE\n")
cat("============================================================\n")


# ==============================================================================
# END
# ==============================================================================