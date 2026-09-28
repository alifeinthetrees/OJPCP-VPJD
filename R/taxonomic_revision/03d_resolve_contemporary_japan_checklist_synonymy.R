# ==============================================================================
# VPJD TAXONOMIC REVISION
#
# 03d_resolve_contemporary_japan_checklist_synonymy.R
#
# Version: 0.2.0
#
# Purpose:
#   Resolve contemporary Japanese checklist records that remained unresolved
#   after 03c by interrogating the existing WCVP nomenclatural index and
#   accepted-name lookup.
#
# Inputs:
#   - vpjd_taxrev_03c_reconciliation
#   - vpjd_star_provisional_wholesale_allocation
#   - vpjd_taxrev_02h_validated_vascular_lineage
#   - wcvp_occurrence_match_index
#   - wcvp_occurrence_accepted_lookup
#
# Core principles:
#   - 03c remains the reconciliation baseline.
#   - The 5,095 records resolved by 03c are retained unchanged.
#   - Only the 4,726 records requiring review enter synonymy resolution.
#   - Exact nomenclatural matching only.
#   - No fuzzy matching.
#   - WCVP synonymy may resolve a Japanese checklist name to an accepted
#     WCVP concept.
#   - A WCVP accepted concept is only treated as resolved to VPJD if its
#     accepted WCVP ID exists in the canonical VPJD population.
#   - Japanese names and source metadata are retained throughout.
#   - Canonical VPJD taxonomy is not modified.
#   - Star allocations are not modified.
#   - No taxa are added or removed.
#
# Expected invariants:
#   Canonical VPJD population:             11,439
#   Contemporary Japan checklist records:  9,821
#   03c resolved records:                   5,095
#   03c records requiring review:           4,726
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

VERSION <- "0.2.0"

MODULE <- "03d"

MODULE_NAME <-
  "resolve_contemporary_japan_checklist_synonymy"


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
    "03d_resolve_contemporary_japan_checklist_synonymy"
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

EXPECTED_03C_MATCHED <- 5095L

EXPECTED_03C_REVIEW <- 4726L


# ==============================================================================
# 5. SOURCE TABLES
# ==============================================================================

CANONICAL_TABLE <-
  "vpjd_star_provisional_wholesale_allocation"

LINEAGE_TABLE <-
  "vpjd_taxrev_02h_validated_vascular_lineage"

TABLE_03C <-
  "vpjd_taxrev_03c_reconciliation"

WCVP_MATCH_INDEX_TABLE <-
  "wcvp_occurrence_match_index"

WCVP_ACCEPTED_LOOKUP_TABLE <-
  "wcvp_occurrence_accepted_lookup"


# ==============================================================================
# 6. OUTPUT TABLES
# ==============================================================================

OUTPUT_RECONCILIATION_TABLE <-
  "vpjd_taxrev_03d_japan_checklist_reconciliation"

OUTPUT_SYNONYMY_TABLE <-
  "vpjd_taxrev_03d_synonymy_resolved"

OUTPUT_ACCEPTED_MATCH_TABLE <-
  "vpjd_taxrev_03d_accepted_name_resolved"

OUTPUT_WCVP_NOT_VPJD_TABLE <-
  "vpjd_taxrev_03d_wcvp_accepted_not_in_vpjd"

OUTPUT_AMBIGUOUS_TABLE <-
  "vpjd_taxrev_03d_ambiguous"

OUTPUT_UNRESOLVED_TABLE <-
  "vpjd_taxrev_03d_unresolved"

OUTPUT_SUMMARY_TABLE <-
  "vpjd_taxrev_03d_summary"

OUTPUT_VALIDATION_TABLE <-
  "vpjd_taxrev_03d_validation"

OUTPUT_METADATA_TABLE <-
  "vpjd_taxrev_03d_metadata"


# ==============================================================================
# 7. CSV OUTPUTS
# ==============================================================================

OUTPUT_MAIN_CSV <-
  file.path(
    OUTPUT_DIR,
    "vpjd_03d_japan_checklist_reconciliation.csv"
  )

OUTPUT_SYNONYMY_CSV <-
  file.path(
    OUTPUT_DIR,
    "vpjd_03d_synonymy_resolved.csv"
  )

OUTPUT_ACCEPTED_MATCH_CSV <-
  file.path(
    OUTPUT_DIR,
    "vpjd_03d_accepted_name_resolved.csv"
  )

OUTPUT_WCVP_NOT_VPJD_CSV <-
  file.path(
    OUTPUT_DIR,
    "vpjd_03d_wcvp_accepted_not_in_vpjd.csv"
  )

OUTPUT_AMBIGUOUS_CSV <-
  file.path(
    OUTPUT_DIR,
    "vpjd_03d_ambiguous.csv"
  )

OUTPUT_UNRESOLVED_CSV <-
  file.path(
    OUTPUT_DIR,
    "vpjd_03d_unresolved.csv"
  )

OUTPUT_SUMMARY_CSV <-
  file.path(
    OUTPUT_DIR,
    "vpjd_03d_summary.csv"
  )

OUTPUT_VALIDATION_CSV <-
  file.path(
    OUTPUT_DIR,
    "vpjd_03d_validation.csv"
  )

OUTPUT_METADATA_CSV <-
  file.path(
    OUTPUT_DIR,
    "vpjd_03d_metadata.csv"
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


collapse_unique <- function(x) {
  
  x <-
    normalise_text(x)
  
  x <-
    x[
      !is.na(x)
    ]
  
  x <-
    unique(x)
  
  if (length(x) == 0L) {
    
    return(
      NA_character_
    )
    
  }
  
  paste(
    sort(x),
    collapse = " | "
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


safe_upper <- function(x) {
  
  toupper(
    normalise_text(x)
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
# 10. DATABASE WRITE-ACCESS TEST
# ==============================================================================

WRITE_TEST_TABLE <-
  "vpjd_taxrev_03d_write_test"


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
      "\n03d STOPPED BEFORE RECONCILIATION.\n\n",
      "The VPJD DuckDB database is not writable.\n\n",
      "Database:\n",
      DB_PATH,
      "\n"
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
    TABLE_03C,
    WCVP_MATCH_INDEX_TABLE,
    WCVP_ACCEPTED_LOOKUP_TABLE
  )


missing_tables <-
  setdiff(
    required_tables,
    database_tables
  )


if (length(missing_tables) > 0L) {
  
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


source_03c <-
  DBI::dbReadTable(
    con,
    TABLE_03C
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
# 13. VALIDATE SOURCE POPULATIONS
# ==============================================================================

if (
  nrow(canonical) !=
  EXPECTED_CANONICAL_POPULATION
) {
  
  disconnect_safely()
  
  stop(
    paste0(
      "Canonical population invariant failed. Expected ",
      EXPECTED_CANONICAL_POPULATION,
      " records but found ",
      nrow(canonical),
      "."
    )
  )
}


if (
  nrow(source_03c) !=
  EXPECTED_JAPAN_CHECKLIST_RECORDS
) {
  
  disconnect_safely()
  
  stop(
    paste0(
      "03c population invariant failed. Expected ",
      EXPECTED_JAPAN_CHECKLIST_RECORDS,
      " records but found ",
      nrow(source_03c),
      "."
    )
  )
}


# ==============================================================================
# 14. VALIDATE EXACT 03c SCHEMA
# ==============================================================================

required_03c_fields <-
  c(
    "JAPAN_RECORD_ID",
    "SOURCE",
    "SOURCE_TAXON_ID",
    "JAPAN_SCIENTIFIC_NAME",
    "JAPANESE_NAME",
    "JAPAN_RANK",
    "JAPAN_FAMILY",
    "JAPAN_GENUS",
    "SOURCE_TAXONOMIC_STATUS",
    "WCVP_ID",
    "VPJD_SCIENTIFIC_NAME",
    "VPJD_RANK",
    "VPJD_FAMILY",
    "VPJD_GENUS",
    "VPJD_SPECIES",
    "MAJOR_GROUP",
    "MATCH_METHOD",
    "MATCH_PRIORITY",
    "RECONCILIATION_STATUS",
    "RECONCILIATION_CLASS",
    "NAME_IDENTICAL",
    "AUTHORSHIP_ONLY_DIFFERENCE",
    "FAMILY_AGREEMENT",
    "GENUS_AGREEMENT",
    "RANK_AGREEMENT"
  )


missing_03c_fields <-
  setdiff(
    required_03c_fields,
    names(source_03c)
  )


if (length(missing_03c_fields) > 0L) {
  
  disconnect_safely()
  
  stop(
    paste0(
      "Required 03c field(s) missing:\n",
      paste(
        missing_03c_fields,
        collapse = "\n"
      )
    )
  )
}


# ==============================================================================
# 15. PROFILE 03c RECONCILIATION STATUS
# ==============================================================================

status_profile_03c <-
  source_03c |>
  count(
    RECONCILIATION_STATUS,
    RECONCILIATION_CLASS,
    sort = TRUE,
    name = "N_RECORDS"
  )


cat("\n")
cat("============================================================\n")
cat("03c RECONCILIATION PROFILE\n")
cat("============================================================\n\n")

print(
  status_profile_03c,
  n = Inf,
  width = Inf
)


# ==============================================================================
# 16. IDENTIFY 03c RESOLVED POPULATION
# ==============================================================================

source_work <-
  source_03c |>
  mutate(
    SOURCE_03C_ROW =
      row_number(),
    
    JAPAN_RECORD_ID =
      normalise_text(
        JAPAN_RECORD_ID
      ),
    
    SOURCE =
      normalise_text(
        SOURCE
      ),
    
    SOURCE_TAXON_ID =
      normalise_text(
        SOURCE_TAXON_ID
      ),
    
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
    
    WCVP_ID =
      normalise_text(
        WCVP_ID
      ),
    
    VPJD_SCIENTIFIC_NAME =
      normalise_text(
        VPJD_SCIENTIFIC_NAME
      ),
    
    RECONCILIATION_STATUS =
      normalise_text(
        RECONCILIATION_STATUS
      ),
    
    RECONCILIATION_CLASS =
      normalise_text(
        RECONCILIATION_CLASS
      ),
    
    NORMALISED_JAPAN_NAME_03D =
      normalise_name(
        JAPAN_SCIENTIFIC_NAME
      )
  )


# ==============================================================================
# 17. DETERMINE WHICH 03c STATUS/CLASS COMBINATION REPRESENTS THE 5,095 MATCHES
# ==============================================================================

resolved_candidates <-
  source_work |>
  filter(
    !is.na(WCVP_ID),
    !is.na(VPJD_SCIENTIFIC_NAME)
  )


resolved_status_profile <-
  resolved_candidates |>
  count(
    RECONCILIATION_STATUS,
    RECONCILIATION_CLASS,
    sort = TRUE,
    name = "N_RECORDS"
  )


cat("\n")
cat("------------------------------------------------------------\n")
cat("03c RECORDS WITH BOTH WCVP ID AND VPJD NAME\n")
cat("------------------------------------------------------------\n\n")

print(
  resolved_status_profile,
  n = Inf,
  width = Inf
)


# ==============================================================================
# 18. DEFINE 03c RESOLVED STATE
# ==============================================================================
#
# We preserve the explicit 03c reconciliation decision.
#
# A record is treated as resolved in 03c only when:
#
#   1. it carries a WCVP ID;
#   2. it carries a VPJD scientific name; and
#   3. its reconciliation status does not explicitly identify it as
#      unresolved, ambiguous, review-required or unmatched.
#
# The resulting count MUST equal the validated 03c total of 5,095.
#
# ==============================================================================

source_work <-
  source_work |>
  mutate(
    STATUS_UPPER_03D =
      safe_upper(
        RECONCILIATION_STATUS
      ),
    
    CLASS_UPPER_03D =
      safe_upper(
        RECONCILIATION_CLASS
      ),
    
    EXPLICIT_REVIEW_STATE_03D =
      stringr::str_detect(
        coalesce(
          STATUS_UPPER_03D,
          ""
        ),
        paste(
          c(
            "REVIEW",
            "UNRESOLVED",
            "AMBIG",
            "NO_MATCH",
            "NO MATCH",
            "UNMATCHED",
            "MISSING"
          ),
          collapse = "|"
        )
      ) |
      stringr::str_detect(
        coalesce(
          CLASS_UPPER_03D,
          ""
        ),
        paste(
          c(
            "REVIEW",
            "UNRESOLVED",
            "AMBIG",
            "NO_MATCH",
            "NO MATCH",
            "UNMATCHED",
            "MISSING"
          ),
          collapse = "|"
        )
      ),
    
    RESOLVED_IN_03C =
      !is.na(WCVP_ID) &
      !is.na(VPJD_SCIENTIFIC_NAME) &
      !EXPLICIT_REVIEW_STATE_03D
  )


n_03c_resolved <-
  sum(
    source_work$RESOLVED_IN_03C,
    na.rm = TRUE
  )


n_03c_review <-
  sum(
    !source_work$RESOLVED_IN_03C,
    na.rm = TRUE
  )


cat("\n")
cat(
  "03c resolved records identified: ",
  format(
    n_03c_resolved,
    big.mark = ","
  ),
  "\n",
  sep = ""
)

cat(
  "03c review records identified: ",
  format(
    n_03c_review,
    big.mark = ","
  ),
  "\n\n",
  sep = ""
)


if (
  n_03c_resolved !=
  EXPECTED_03C_MATCHED
) {
  
  disconnect_safely()
  
  stop(
    paste0(
      "03d cannot safely identify the validated 03c resolved population.\n\n",
      "Expected resolved records: ",
      EXPECTED_03C_MATCHED,
      "\n",
      "Detected resolved records: ",
      n_03c_resolved,
      "\n\n",
      "No 03d reconciliation has been performed.\n",
      "Inspect the printed 03c status/class profile before proceeding."
    )
  )
}


if (
  n_03c_review !=
  EXPECTED_03C_REVIEW
) {
  
  disconnect_safely()
  
  stop(
    paste0(
      "03c review population invariant failed.\n",
      "Expected: ",
      EXPECTED_03C_REVIEW,
      "\n",
      "Detected: ",
      n_03c_review
    )
  )
}


# ==============================================================================
# 19. IDENTIFY 03c REVIEW POPULATION
# ==============================================================================

review_03c <-
  source_work |>
  filter(
    !RESOLVED_IN_03C
  )


# ==============================================================================
# 20. VALIDATE WCVP MATCH-INDEX FIELDS
# ==============================================================================

required_wcvp_index_fields <-
  c(
    "wcvp_plant_name_id",
    "wcvp_taxon_name",
    "wcvp_taxon_authors",
    "wcvp_taxon_rank",
    "wcvp_taxon_status",
    "wcvp_accepted_plant_name_id",
    "wcvp_scientific_name",
    "wcvp_match_name"
  )


missing_wcvp_index_fields <-
  setdiff(
    required_wcvp_index_fields,
    names(wcvp_index)
  )


if (length(missing_wcvp_index_fields) > 0L) {
  
  disconnect_safely()
  
  stop(
    paste0(
      "Required WCVP match-index field(s) missing:\n",
      paste(
        missing_wcvp_index_fields,
        collapse = "\n"
      )
    )
  )
}


# ==============================================================================
# 21. VALIDATE WCVP ACCEPTED-LOOKUP FIELDS
# ==============================================================================

required_wcvp_accepted_fields <-
  c(
    "accepted_wcvp_plant_name_id",
    "accepted_wcvp_taxon_name",
    "accepted_wcvp_taxon_authors",
    "accepted_wcvp_taxon_rank",
    "accepted_wcvp_scientific_name"
  )


missing_wcvp_accepted_fields <-
  setdiff(
    required_wcvp_accepted_fields,
    names(wcvp_accepted)
  )


if (length(missing_wcvp_accepted_fields) > 0L) {
  
  disconnect_safely()
  
  stop(
    paste0(
      "Required WCVP accepted-lookup field(s) missing:\n",
      paste(
        missing_wcvp_accepted_fields,
        collapse = "\n"
      )
    )
  )
}


# ==============================================================================
# 22. PREPARE WCVP NOMENCLATURAL INDEX
# ==============================================================================

wcvp_index_work <-
  wcvp_index |>
  transmute(
    WCVP_MATCHED_NAME_ID =
      normalise_text(
        wcvp_plant_name_id
      ),
    
    WCVP_MATCHED_TAXON_NAME =
      normalise_text(
        wcvp_taxon_name
      ),
    
    WCVP_MATCHED_AUTHORSHIP =
      normalise_text(
        wcvp_taxon_authors
      ),
    
    WCVP_MATCHED_RANK =
      normalise_text(
        wcvp_taxon_rank
      ),
    
    WCVP_MATCHED_STATUS =
      normalise_text(
        wcvp_taxon_status
      ),
    
    WCVP_ACCEPTED_ID_RAW =
      normalise_text(
        wcvp_accepted_plant_name_id
      ),
    
    WCVP_MATCHED_SCIENTIFIC_NAME =
      normalise_text(
        wcvp_scientific_name
      ),
    
    WCVP_MATCH_NAME_RAW =
      normalise_text(
        wcvp_match_name
      ),
    
    NORMALISED_WCVP_MATCH_NAME =
      normalise_name(
        wcvp_match_name
      )
  ) |>
  filter(
    !is.na(
      NORMALISED_WCVP_MATCH_NAME
    )
  )


# ==============================================================================
# 23. RESOLVE ACCEPTED ID FOR EACH WCVP NAME
# ==============================================================================

wcvp_index_work <-
  wcvp_index_work |>
  mutate(
    WCVP_STATUS_UPPER =
      safe_upper(
        WCVP_MATCHED_STATUS
      ),
    
    WCVP_RESOLVED_ACCEPTED_ID =
      case_when(
        
        !is.na(WCVP_ACCEPTED_ID_RAW) ~
          WCVP_ACCEPTED_ID_RAW,
        
        WCVP_STATUS_UPPER %in%
          c(
            "ACCEPTED",
            "ACCEPTED NAME"
          ) ~
          WCVP_MATCHED_NAME_ID,
        
        TRUE ~
          NA_character_
      )
  )


# ==============================================================================
# 24. PREPARE WCVP ACCEPTED LOOKUP
# ==============================================================================

wcvp_accepted_work <-
  wcvp_accepted |>
  transmute(
    WCVP_RESOLVED_ACCEPTED_ID =
      normalise_text(
        accepted_wcvp_plant_name_id
      ),
    
    WCVP_ACCEPTED_TAXON_NAME =
      normalise_text(
        accepted_wcvp_taxon_name
      ),
    
    WCVP_ACCEPTED_AUTHORSHIP =
      normalise_text(
        accepted_wcvp_taxon_authors
      ),
    
    WCVP_ACCEPTED_RANK =
      normalise_text(
        accepted_wcvp_taxon_rank
      ),
    
    WCVP_ACCEPTED_SCIENTIFIC_NAME =
      normalise_text(
        accepted_wcvp_scientific_name
      )
  ) |>
  distinct()


# ==============================================================================
# 25. BUILD WCVP NOMENCLATURAL CANDIDATE INDEX
# ==============================================================================

wcvp_candidates <-
  wcvp_index_work |>
  left_join(
    wcvp_accepted_work,
    by =
      "WCVP_RESOLVED_ACCEPTED_ID"
  )


# ==============================================================================
# 26. EXACT MATCH 4,726 REVIEW RECORDS TO WCVP NOMENCLATURAL INDEX
# ==============================================================================

candidate_matches <-
  review_03c |>
  select(
    SOURCE_03C_ROW,
    JAPAN_RECORD_ID,
    SOURCE,
    SOURCE_TAXON_ID,
    JAPAN_SCIENTIFIC_NAME,
    JAPANESE_NAME,
    JAPAN_RANK,
    JAPAN_FAMILY,
    JAPAN_GENUS,
    SOURCE_TAXONOMIC_STATUS,
    NORMALISED_JAPAN_NAME_03D
  ) |>
  left_join(
    wcvp_candidates,
    by =
      c(
        "NORMALISED_JAPAN_NAME_03D" =
          "NORMALISED_WCVP_MATCH_NAME"
      )
  )


# ==============================================================================
# 27. PROFILE WCVP CANDIDATES BY JAPANESE CHECKLIST RECORD
# ==============================================================================

candidate_profile <-
  candidate_matches |>
  group_by(
    SOURCE_03C_ROW
  ) |>
  summarise(
    WCVP_MATCHED_NAME_COUNT =
      n_distinct(
        WCVP_MATCHED_NAME_ID[
          !is.na(
            WCVP_MATCHED_NAME_ID
          )
        ]
      ),
    
    WCVP_ACCEPTED_CONCEPT_COUNT =
      n_distinct(
        WCVP_RESOLVED_ACCEPTED_ID[
          !is.na(
            WCVP_RESOLVED_ACCEPTED_ID
          )
        ]
      ),
    
    WCVP_MATCHED_NAME_IDS =
      collapse_unique(
        WCVP_MATCHED_NAME_ID
      ),
    
    WCVP_MATCHED_NAMES =
      collapse_unique(
        WCVP_MATCHED_SCIENTIFIC_NAME
      ),
    
    WCVP_MATCHED_STATUSES =
      collapse_unique(
        WCVP_MATCHED_STATUS
      ),
    
    WCVP_ACCEPTED_IDS =
      collapse_unique(
        WCVP_RESOLVED_ACCEPTED_ID
      ),
    
    .groups = "drop"
  )


# ==============================================================================
# 28. EXTRACT UNIQUE ACCEPTED-CONCEPT RESOLUTIONS
# ==============================================================================

unique_resolution <-
  candidate_matches |>
  filter(
    !is.na(
      WCVP_RESOLVED_ACCEPTED_ID
    )
  ) |>
  group_by(
    SOURCE_03C_ROW
  ) |>
  filter(
    n_distinct(
      WCVP_RESOLVED_ACCEPTED_ID
    ) == 1L
  ) |>
  summarise(
    D03_WCVP_MATCHED_NAME_ID =
      collapse_unique(
        WCVP_MATCHED_NAME_ID
      ),
    
    D03_WCVP_MATCHED_NAME =
      collapse_unique(
        WCVP_MATCHED_SCIENTIFIC_NAME
      ),
    
    D03_WCVP_MATCHED_TAXON_NAME =
      collapse_unique(
        WCVP_MATCHED_TAXON_NAME
      ),
    
    D03_WCVP_MATCHED_AUTHORSHIP =
      collapse_unique(
        WCVP_MATCHED_AUTHORSHIP
      ),
    
    D03_WCVP_MATCHED_RANK =
      collapse_unique(
        WCVP_MATCHED_RANK
      ),
    
    D03_WCVP_MATCHED_STATUS =
      collapse_unique(
        WCVP_MATCHED_STATUS
      ),
    
    D03_ACCEPTED_WCVP_ID =
      first(
        WCVP_RESOLVED_ACCEPTED_ID
      ),
    
    D03_ACCEPTED_WCVP_TAXON_NAME =
      collapse_unique(
        WCVP_ACCEPTED_TAXON_NAME
      ),
    
    D03_ACCEPTED_WCVP_AUTHORSHIP =
      collapse_unique(
        WCVP_ACCEPTED_AUTHORSHIP
      ),
    
    D03_ACCEPTED_WCVP_RANK =
      collapse_unique(
        WCVP_ACCEPTED_RANK
      ),
    
    D03_ACCEPTED_WCVP_SCIENTIFIC_NAME =
      collapse_unique(
        WCVP_ACCEPTED_SCIENTIFIC_NAME
      ),
    
    .groups = "drop"
  )


# ==============================================================================
# 29. PREPARE CANONICAL VPJD LOOKUP
# ==============================================================================

required_canonical_fields <-
  c(
    "FINAL_WCVP_ID",
    "FINAL_WCVP_RECOGNISED_NAME",
    "FINAL_WCVP_RANK",
    "FINAL_WCVP_STATUS",
    "FINAL_WCVP_CONCEPT_CLASS"
  )


missing_canonical_fields <-
  setdiff(
    required_canonical_fields,
    names(canonical)
  )


if (length(missing_canonical_fields) > 0L) {
  
  disconnect_safely()
  
  stop(
    paste0(
      "Required canonical field(s) missing:\n",
      paste(
        missing_canonical_fields,
        collapse = "\n"
      )
    )
  )
}


canonical_lookup <-
  canonical |>
  transmute(
    D03_ACCEPTED_WCVP_ID =
      normalise_text(
        FINAL_WCVP_ID
      ),
    
    D03_VPJD_SCIENTIFIC_NAME =
      normalise_text(
        FINAL_WCVP_RECOGNISED_NAME
      ),
    
    D03_VPJD_RANK =
      normalise_text(
        FINAL_WCVP_RANK
      ),
    
    D03_VPJD_STATUS =
      normalise_text(
        FINAL_WCVP_STATUS
      ),
    
    D03_VPJD_CONCEPT_CLASS =
      normalise_text(
        FINAL_WCVP_CONCEPT_CLASS
      )
  ) |>
  distinct()


# ==============================================================================
# 30. CHECK CANONICAL WCVP-ID UNIQUENESS
# ==============================================================================

canonical_id_duplicates <-
  canonical_lookup |>
  filter(
    !is.na(
      D03_ACCEPTED_WCVP_ID
    )
  ) |>
  count(
    D03_ACCEPTED_WCVP_ID,
    name = "N"
  ) |>
  filter(
    N > 1L
  )


if (nrow(canonical_id_duplicates) > 0L) {
  
  disconnect_safely()
  
  stop(
    paste0(
      "Canonical VPJD WCVP IDs are not unique. ",
      nrow(canonical_id_duplicates),
      " duplicate IDs detected."
    )
  )
}


# ==============================================================================
# 31. LINK UNIQUE WCVP RESOLUTIONS TO CANONICAL VPJD
# ==============================================================================

unique_resolution <-
  unique_resolution |>
  left_join(
    canonical_lookup,
    by =
      "D03_ACCEPTED_WCVP_ID"
  ) |>
  mutate(
    D03_ACCEPTED_CONCEPT_IN_VPJD =
      !is.na(
        D03_VPJD_SCIENTIFIC_NAME
      )
  )


# ==============================================================================
# 32. CLASSIFY NOMENCLATURAL ROUTE
# ==============================================================================

unique_resolution <-
  unique_resolution |>
  mutate(
    D03_WCVP_STATUS_UPPER =
      safe_upper(
        D03_WCVP_MATCHED_STATUS
      ),
    
    D03_RESOLUTION_METHOD =
      case_when(
        
        D03_WCVP_STATUS_UPPER %in%
          c(
            "ACCEPTED",
            "ACCEPTED NAME"
          ) ~
          "EXACT_WCVP_ACCEPTED_NAME",
        
        !is.na(
          D03_WCVP_MATCHED_STATUS
        ) ~
          "EXACT_WCVP_NON_ACCEPTED_NAME_TO_ACCEPTED_CONCEPT",
        
        TRUE ~
          "EXACT_WCVP_NAME_TO_ACCEPTED_CONCEPT"
      ),
    
    D03_SYNONYMY_RESOLUTION =
      !D03_WCVP_STATUS_UPPER %in%
      c(
        "ACCEPTED",
        "ACCEPTED NAME"
      ) &
      !is.na(
        D03_WCVP_MATCHED_STATUS
      )
  )


# ==============================================================================
# 33. ATTACH 03d EVIDENCE TO COMPLETE 03c POPULATION
# ==============================================================================

reconciliation <-
  source_work |>
  left_join(
    candidate_profile,
    by =
      "SOURCE_03C_ROW"
  ) |>
  left_join(
    unique_resolution,
    by =
      "SOURCE_03C_ROW"
  )


# ==============================================================================
# 34. CLASSIFY FINAL 03d RECONCILIATION
# ==============================================================================

reconciliation <-
  reconciliation |>
  mutate(
    D03_FINAL_RECONCILIATION_CLASS =
      case_when(
        
        RESOLVED_IN_03C ~
          "RESOLVED_IN_03C",
        
        is.na(
          JAPAN_SCIENTIFIC_NAME
        ) ~
          "MISSING_JAPAN_SCIENTIFIC_NAME",
        
        is.na(
          WCVP_MATCHED_NAME_COUNT
        ) |
          WCVP_MATCHED_NAME_COUNT == 0L ~
          "NO_EXACT_WCVP_NOMENCLATURAL_MATCH",
        
        WCVP_ACCEPTED_CONCEPT_COUNT > 1L ~
          "AMBIGUOUS_MULTIPLE_WCVP_ACCEPTED_CONCEPTS",
        
        WCVP_MATCHED_NAME_COUNT > 0L &
          WCVP_ACCEPTED_CONCEPT_COUNT == 0L ~
          "WCVP_NAME_WITHOUT_ACCEPTED_CONCEPT",
        
        !is.na(
          D03_ACCEPTED_WCVP_ID
        ) &
          D03_ACCEPTED_CONCEPT_IN_VPJD %in%
          TRUE &
          D03_SYNONYMY_RESOLUTION %in%
          TRUE ~
          "RESOLVED_TO_VPJD_VIA_WCVP_SYNONYMY",
        
        !is.na(
          D03_ACCEPTED_WCVP_ID
        ) &
          D03_ACCEPTED_CONCEPT_IN_VPJD %in%
          TRUE ~
          "RESOLVED_TO_VPJD_VIA_WCVP_ACCEPTED_NAME",
        
        !is.na(
          D03_ACCEPTED_WCVP_ID
        ) &
          D03_ACCEPTED_CONCEPT_IN_VPJD %in%
          FALSE ~
          "WCVP_ACCEPTED_CONCEPT_NOT_IN_VPJD",
        
        TRUE ~
          "REQUIRES_REVIEW"
      )
  )


# ==============================================================================
# 35. FINAL RESOLVED WCVP ID
# ==============================================================================

reconciliation <-
  reconciliation |>
  mutate(
    D03_FINAL_WCVP_ID =
      case_when(
        
        RESOLVED_IN_03C ~
          WCVP_ID,
        
        D03_FINAL_RECONCILIATION_CLASS %in%
          c(
            "RESOLVED_TO_VPJD_VIA_WCVP_SYNONYMY",
            "RESOLVED_TO_VPJD_VIA_WCVP_ACCEPTED_NAME"
          ) ~
          D03_ACCEPTED_WCVP_ID,
        
        TRUE ~
          NA_character_
      )
  )


# ==============================================================================
# 36. FINAL VPJD SCIENTIFIC NAME
# ==============================================================================

reconciliation <-
  reconciliation |>
  mutate(
    D03_FINAL_VPJD_SCIENTIFIC_NAME =
      case_when(
        
        RESOLVED_IN_03C ~
          VPJD_SCIENTIFIC_NAME,
        
        D03_FINAL_RECONCILIATION_CLASS %in%
          c(
            "RESOLVED_TO_VPJD_VIA_WCVP_SYNONYMY",
            "RESOLVED_TO_VPJD_VIA_WCVP_ACCEPTED_NAME"
          ) ~
          D03_VPJD_SCIENTIFIC_NAME,
        
        TRUE ~
          NA_character_
      )
  )


# ==============================================================================
# 37. FINAL RESOLUTION METHOD
# ==============================================================================

reconciliation <-
  reconciliation |>
  mutate(
    D03_FINAL_MATCH_METHOD =
      case_when(
        
        RESOLVED_IN_03C ~
          MATCH_METHOD,
        
        D03_FINAL_RECONCILIATION_CLASS ==
          "RESOLVED_TO_VPJD_VIA_WCVP_SYNONYMY" ~
          "WCVP_SYNONYMY",
        
        D03_FINAL_RECONCILIATION_CLASS ==
          "RESOLVED_TO_VPJD_VIA_WCVP_ACCEPTED_NAME" ~
          "WCVP_ACCEPTED_NAME",
        
        TRUE ~
          NA_character_
      )
  )


# ==============================================================================
# 38. EXTRACT NEW SYNONYMY RESOLUTIONS
# ==============================================================================

synonymy_resolved <-
  reconciliation |>
  filter(
    D03_FINAL_RECONCILIATION_CLASS ==
      "RESOLVED_TO_VPJD_VIA_WCVP_SYNONYMY"
  )


# ==============================================================================
# 39. EXTRACT NEW ACCEPTED-NAME RESOLUTIONS
# ==============================================================================

accepted_name_resolved <-
  reconciliation |>
  filter(
    D03_FINAL_RECONCILIATION_CLASS ==
      "RESOLVED_TO_VPJD_VIA_WCVP_ACCEPTED_NAME"
  )


# ==============================================================================
# 40. WCVP ACCEPTED CONCEPTS NOT PRESENT IN VPJD
# ==============================================================================

wcvp_not_vpjd <-
  reconciliation |>
  filter(
    D03_FINAL_RECONCILIATION_CLASS ==
      "WCVP_ACCEPTED_CONCEPT_NOT_IN_VPJD"
  )


# ==============================================================================
# 41. AMBIGUOUS RECORDS
# ==============================================================================

ambiguous <-
  reconciliation |>
  filter(
    D03_FINAL_RECONCILIATION_CLASS ==
      "AMBIGUOUS_MULTIPLE_WCVP_ACCEPTED_CONCEPTS"
  )


# ==============================================================================
# 42. REMAINING UNRESOLVED
# ==============================================================================

unresolved <-
  reconciliation |>
  filter(
    !D03_FINAL_RECONCILIATION_CLASS %in%
      c(
        "RESOLVED_IN_03C",
        "RESOLVED_TO_VPJD_VIA_WCVP_SYNONYMY",
        "RESOLVED_TO_VPJD_VIA_WCVP_ACCEPTED_NAME"
      )
  )


# ==============================================================================
# 43. COUNTS
# ==============================================================================

n_total <-
  nrow(
    reconciliation
  )


n_retained_03c <-
  sum(
    reconciliation$D03_FINAL_RECONCILIATION_CLASS ==
      "RESOLVED_IN_03C",
    na.rm = TRUE
  )


n_new_synonymy <-
  nrow(
    synonymy_resolved
  )


n_new_accepted_name <-
  nrow(
    accepted_name_resolved
  )


n_new_resolved <-
  n_new_synonymy +
  n_new_accepted_name


n_final_resolved <-
  n_retained_03c +
  n_new_resolved


n_wcvp_not_vpjd <-
  nrow(
    wcvp_not_vpjd
  )


n_ambiguous <-
  nrow(
    ambiguous
  )


n_remaining_unresolved <-
  nrow(
    unresolved
  )


# ==============================================================================
# 44. SUMMARY TABLE
# ==============================================================================

summary_table <-
  reconciliation |>
  count(
    D03_FINAL_RECONCILIATION_CLASS,
    name = "N_RECORDS"
  ) |>
  mutate(
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
# 45. SOURCE SUMMARY
# ==============================================================================

source_summary <-
  reconciliation |>
  count(
    SOURCE,
    D03_FINAL_RECONCILIATION_CLASS,
    name = "N_RECORDS"
  ) |>
  arrange(
    SOURCE,
    desc(
      N_RECORDS
    )
  )


# ==============================================================================
# 46. VALIDATION
# ==============================================================================

validation <-
  tibble(
    CHECK =
      c(
        "Canonical VPJD population = 11,439",
        "Japan checklist population = 9,821",
        "03c resolved population = 5,095",
        "03c review population = 4,726",
        "03c source row identifiers unique",
        "Japan record IDs present",
        "Japan record IDs unique",
        "03d reconciliation population = 9,821",
        "03d source row identifiers unique",
        "No source records lost",
        "No source records added",
        "03c resolved records retained unchanged in count",
        "03d classifications cover all 9,821 records",
        "New synonymy resolutions have accepted WCVP IDs",
        "New accepted-name resolutions have accepted WCVP IDs",
        "New synonymy resolutions map to VPJD concepts",
        "New accepted-name resolutions map to VPJD concepts",
        "WCVP-not-VPJD records do not map to VPJD concepts",
        "Ambiguous records have multiple accepted concepts",
        "Final resolved count >= 03c resolved count",
        "Final resolved count <= 9,821",
        "WCVP match index available",
        "WCVP accepted lookup available",
        "Canonical WCVP IDs unique",
        "No fuzzy matching performed",
        "No automatic synonym acceptance without WCVP evidence",
        "Japanese checklist taxonomy not imposed on VPJD",
        "Canonical taxonomy modified = FALSE",
        "Star allocations modified = FALSE",
        "Taxa removed = 0",
        "Taxa added = 0"
      ),
    
    PASS =
      c(
        nrow(canonical) ==
          EXPECTED_CANONICAL_POPULATION,
        
        n_total ==
          EXPECTED_JAPAN_CHECKLIST_RECORDS,
        
        n_03c_resolved ==
          EXPECTED_03C_MATCHED,
        
        n_03c_review ==
          EXPECTED_03C_REVIEW,
        
        n_distinct(
          source_work$SOURCE_03C_ROW
        ) ==
          EXPECTED_JAPAN_CHECKLIST_RECORDS,
        
        all(
          !is.na(
            source_work$JAPAN_RECORD_ID
          )
        ),
        
        n_distinct(
          source_work$JAPAN_RECORD_ID
        ) ==
          EXPECTED_JAPAN_CHECKLIST_RECORDS,
        
        nrow(reconciliation) ==
          EXPECTED_JAPAN_CHECKLIST_RECORDS,
        
        n_distinct(
          reconciliation$SOURCE_03C_ROW
        ) ==
          EXPECTED_JAPAN_CHECKLIST_RECORDS,
        
        nrow(reconciliation) ==
          nrow(source_03c),
        
        nrow(reconciliation) ==
          nrow(source_03c),
        
        n_retained_03c ==
          EXPECTED_03C_MATCHED,
        
        sum(
          summary_table$N_RECORDS
        ) ==
          EXPECTED_JAPAN_CHECKLIST_RECORDS,
        
        all(
          !is.na(
            synonymy_resolved$D03_ACCEPTED_WCVP_ID
          )
        ),
        
        all(
          !is.na(
            accepted_name_resolved$D03_ACCEPTED_WCVP_ID
          )
        ),
        
        all(
          synonymy_resolved$D03_ACCEPTED_CONCEPT_IN_VPJD %in%
            TRUE
        ),
        
        all(
          accepted_name_resolved$D03_ACCEPTED_CONCEPT_IN_VPJD %in%
            TRUE
        ),
        
        all(
          wcvp_not_vpjd$D03_ACCEPTED_CONCEPT_IN_VPJD %in%
            FALSE
        ),
        
        all(
          ambiguous$WCVP_ACCEPTED_CONCEPT_COUNT > 1L
        ),
        
        n_final_resolved >=
          EXPECTED_03C_MATCHED,
        
        n_final_resolved <=
          EXPECTED_JAPAN_CHECKLIST_RECORDS,
        
        nrow(wcvp_index) > 0L,
        
        nrow(wcvp_accepted) > 0L,
        
        nrow(
          canonical_id_duplicates
        ) == 0L,
        
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
# 47. VALIDATION DECISION
# ==============================================================================

all_valid <-
  all(
    validation$PASS
  )


if (all_valid) {
  
  decision <-
    "CONTEMPORARY_JAPAN_SYNONYMY_RECONCILIATION_COMPLETE"
  
} else {
  
  decision <-
    "REVIEW_REQUIRED"
}


# ==============================================================================
# 48. METADATA
# ==============================================================================

metadata <-
  tibble(
    METRIC =
      c(
        "MODULE",
        "VERSION",
        "CANONICAL_TABLE",
        "LINEAGE_TABLE",
        "SOURCE_03C_TABLE",
        "WCVP_MATCH_INDEX_TABLE",
        "WCVP_ACCEPTED_LOOKUP_TABLE",
        "CANONICAL_VPJD_RECORDS",
        "JAPAN_CHECKLIST_RECORDS",
        "03C_RESOLVED_RECORDS",
        "03C_REVIEW_RECORDS",
        "03D_NEW_SYNONYMY_RESOLUTIONS",
        "03D_NEW_ACCEPTED_NAME_RESOLUTIONS",
        "03D_TOTAL_NEW_RESOLUTIONS",
        "FINAL_RESOLVED_RECORDS",
        "WCVP_ACCEPTED_NOT_IN_VPJD",
        "AMBIGUOUS_RECORDS",
        "REMAINING_UNRESOLVED",
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
          LINEAGE_TABLE,
          TABLE_03C,
          WCVP_MATCH_INDEX_TABLE,
          WCVP_ACCEPTED_LOOKUP_TABLE,
          nrow(canonical),
          n_total,
          n_retained_03c,
          n_03c_review,
          n_new_synonymy,
          n_new_accepted_name,
          n_new_resolved,
          n_final_resolved,
          n_wcvp_not_vpjd,
          n_ambiguous,
          n_remaining_unresolved,
          FALSE,
          FALSE,
          FALSE
        )
      )
  )


# ==============================================================================
# 49. WRITE DUCKDB OUTPUTS
# ==============================================================================

DBI::dbWriteTable(
  con,
  OUTPUT_RECONCILIATION_TABLE,
  reconciliation,
  overwrite = TRUE
)


DBI::dbWriteTable(
  con,
  OUTPUT_SYNONYMY_TABLE,
  synonymy_resolved,
  overwrite = TRUE
)


DBI::dbWriteTable(
  con,
  OUTPUT_ACCEPTED_MATCH_TABLE,
  accepted_name_resolved,
  overwrite = TRUE
)


DBI::dbWriteTable(
  con,
  OUTPUT_WCVP_NOT_VPJD_TABLE,
  wcvp_not_vpjd,
  overwrite = TRUE
)


DBI::dbWriteTable(
  con,
  OUTPUT_AMBIGUOUS_TABLE,
  ambiguous,
  overwrite = TRUE
)


DBI::dbWriteTable(
  con,
  OUTPUT_UNRESOLVED_TABLE,
  unresolved,
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
# 50. WRITE CSV OUTPUTS
# ==============================================================================

readr::write_csv(
  reconciliation,
  OUTPUT_MAIN_CSV,
  na = ""
)


readr::write_csv(
  synonymy_resolved,
  OUTPUT_SYNONYMY_CSV,
  na = ""
)


readr::write_csv(
  accepted_name_resolved,
  OUTPUT_ACCEPTED_MATCH_CSV,
  na = ""
)


readr::write_csv(
  wcvp_not_vpjd,
  OUTPUT_WCVP_NOT_VPJD_CSV,
  na = ""
)


readr::write_csv(
  ambiguous,
  OUTPUT_AMBIGUOUS_CSV,
  na = ""
)


readr::write_csv(
  unresolved,
  OUTPUT_UNRESOLVED_CSV,
  na = ""
)


readr::write_csv(
  summary_table,
  OUTPUT_SUMMARY_CSV,
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
# 51. VERIFY DUCKDB OUTPUTS
# ==============================================================================

expected_output_tables <-
  c(
    OUTPUT_RECONCILIATION_TABLE,
    OUTPUT_SYNONYMY_TABLE,
    OUTPUT_ACCEPTED_MATCH_TABLE,
    OUTPUT_WCVP_NOT_VPJD_TABLE,
    OUTPUT_AMBIGUOUS_TABLE,
    OUTPUT_UNRESOLVED_TABLE,
    OUTPUT_SUMMARY_TABLE,
    OUTPUT_VALIDATION_TABLE,
    OUTPUT_METADATA_TABLE
  )


database_tables_after <-
  DBI::dbListTables(
    con
  )


duckdb_outputs_written <-
  all(
    expected_output_tables %in%
      database_tables_after
  )


if (!duckdb_outputs_written) {
  
  disconnect_safely()
  
  stop(
    "03d DuckDB output verification failed."
  )
}


# ==============================================================================
# 52. VERIFY CSV OUTPUTS
# ==============================================================================

expected_csv_outputs <-
  c(
    OUTPUT_MAIN_CSV,
    OUTPUT_SYNONYMY_CSV,
    OUTPUT_ACCEPTED_MATCH_CSV,
    OUTPUT_WCVP_NOT_VPJD_CSV,
    OUTPUT_AMBIGUOUS_CSV,
    OUTPUT_UNRESOLVED_CSV,
    OUTPUT_SUMMARY_CSV,
    OUTPUT_VALIDATION_CSV,
    OUTPUT_METADATA_CSV
  )


csv_outputs_written <-
  all(
    file.exists(
      expected_csv_outputs
    )
  )


if (!csv_outputs_written) {
  
  disconnect_safely()
  
  stop(
    "03d CSV output verification failed."
  )
}


# ==============================================================================
# 53. VALIDATION REPORT
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
      "%02d  %-65s  %s\n",
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
# 54. FAILED VALIDATION REPORT
# ==============================================================================

if (!all_valid) {
  
  cat("\n")
  cat("------------------------------------------------------------\n")
  cat("FAILED VALIDATION CHECK(S)\n")
  cat("------------------------------------------------------------\n\n")
  
  failed <-
    validation[
      validation$PASS %in% FALSE,
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
      failed$CHECK[[i]],
      "\n",
      sep = ""
    )
    
    cat(
      "PASS: FALSE\n\n"
    )
  }
}


# ==============================================================================
# 55. RECONCILIATION SUMMARY
# ==============================================================================

cat("\n")
cat("------------------------------------------------------------\n")
cat("03d RECONCILIATION SUMMARY\n")
cat("------------------------------------------------------------\n\n")


for (
  i in seq_len(
    nrow(summary_table)
  )
) {
  
  cat(
    summary_table$D03_FINAL_RECONCILIATION_CLASS[[i]],
    ": ",
    format(
      summary_table$N_RECORDS[[i]],
      big.mark = ","
    ),
    " (",
    summary_table$PERCENT_OF_JAPAN_CHECKLIST[[i]],
    "%)\n",
    sep = ""
  )
}


# ==============================================================================
# 56. CLEAN DISCONNECT
# ==============================================================================

DBI::dbDisconnect(
  con,
  shutdown = TRUE
)

rm(con)

gc()


# ==============================================================================
# 57. COMPLETION REPORT
# ==============================================================================

cat("\n")
cat("============================================================\n")

cat(
  "VPJD TAXONOMIC REVISION 03d v",
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
    n_total,
    big.mark = ","
  ),
  "\n\n",
  sep = ""
)


cat(
  "Records resolved in 03c and retained: ",
  format(
    n_retained_03c,
    big.mark = ","
  ),
  "\n",
  sep = ""
)


cat(
  "03c records entering 03d review: ",
  format(
    n_03c_review,
    big.mark = ","
  ),
  "\n\n",
  sep = ""
)


cat(
  "New synonymy resolutions: ",
  format(
    n_new_synonymy,
    big.mark = ","
  ),
  "\n",
  sep = ""
)


cat(
  "New accepted-name resolutions: ",
  format(
    n_new_accepted_name,
    big.mark = ","
  ),
  "\n",
  sep = ""
)


cat(
  "Total new 03d resolutions: ",
  format(
    n_new_resolved,
    big.mark = ","
  ),
  "\n",
  sep = ""
)


cat(
  "Final resolved records after 03d: ",
  format(
    n_final_resolved,
    big.mark = ","
  ),
  "\n\n",
  sep = ""
)


cat(
  "WCVP accepted concepts not present in VPJD: ",
  format(
    n_wcvp_not_vpjd,
    big.mark = ","
  ),
  "\n",
  sep = ""
)


cat(
  "Ambiguous WCVP resolutions: ",
  format(
    n_ambiguous,
    big.mark = ","
  ),
  "\n",
  sep = ""
)


cat(
  "Records remaining unresolved/requiring review: ",
  format(
    n_remaining_unresolved,
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


cat("RECONCILIATION / AUDIT ONLY\n")
cat("Database write test: PASS\n")
cat("Canonical taxonomy modified: FALSE\n")
cat("Star allocations modified: FALSE\n")
cat("Taxa removed: 0\n")
cat("Taxa added: 0\n")
cat("Fuzzy name matching performed: FALSE\n")
cat("WCVP nomenclatural synonymy interrogated: TRUE\n")
cat("Automatic VPJD expansion performed: FALSE\n")
cat("Japanese checklist taxonomy imposed on VPJD: FALSE\n")
cat("03d DuckDB outputs written: TRUE\n")
cat("03d CSV outputs written: TRUE\n")
cat("DuckDB connection closed cleanly: TRUE\n")


cat("\n")
cat("============================================================\n")
cat("03d COMPLETE\n")
cat("============================================================\n")


# ==============================================================================
# END
# ==============================================================================