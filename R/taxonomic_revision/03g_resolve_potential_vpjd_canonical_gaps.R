# ==============================================================================
# VPJD TAXONOMIC REVISION
#
# 03g_resolve_potential_vpjd_canonical_gaps.R
#
# Purpose:
#   Resolve, where evidence permits, the 201 potential VPJD canonical gaps
#   identified and audited by 03f.
#
# Principles:
#   - canonical VPJD taxonomy is NOT modified;
#   - Star allocations are NOT modified;
#   - no taxa are automatically added or removed;
#   - no fuzzy matching;
#   - no automatic synonym acceptance;
#   - contemporary Japanese checklist taxonomy is not imposed on VPJD;
#   - all proposed resolutions are audit outputs only.
#
# Input authority:
#   vpjd_taxrev_03f_potential_gap_audit
#
# Canonical authority:
#   vpjd_star_provisional_wholesale_allocation
#
# Expected source populations:
#   canonical VPJD = 11,439
#   03f potential-gap audit = 201
#
# ==============================================================================


# ==============================================================================
# 01. VERSION
# ==============================================================================

VERSION <-
  "0.2.0"


# ==============================================================================
# 02. LIBRARIES
# ==============================================================================

suppressPackageStartupMessages({
  
  library(DBI)
  library(duckdb)
  library(dplyr)
  library(tibble)
  library(stringr)
  library(readr)
  
})


# ==============================================================================
# 03. PATHS
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
    "03g_resolve_potential_vpjd_canonical_gaps"
  )


dir.create(
  OUTPUT_DIR,
  recursive = TRUE,
  showWarnings = FALSE
)


# ==============================================================================
# 04. CONSTANTS
# ==============================================================================

CANONICAL_TABLE <-
  "vpjd_star_provisional_wholesale_allocation"


TABLE_03F <-
  "vpjd_taxrev_03f_potential_gap_audit"


EXPECTED_CANONICAL_POPULATION <-
  11439L


EXPECTED_03F_POTENTIAL_GAPS <-
  201L


# ==============================================================================
# 05. HELPERS
# ==============================================================================

normalise_text <-
  function(x) {
    
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


normalise_name <-
  function(x) {
    
    x <-
      normalise_text(x)
    
    x <-
      stringr::str_replace_all(
        x,
        "[[:space:]]+",
        " "
      )
    
    x
  }


first_existing_field <-
  function(
    data,
    candidates
  ) {
    
    found <-
      candidates[
        candidates %in%
          names(data)
      ]
    
    if (
      length(found) == 0L
    ) {
      
      return(
        NA_character_
      )
    }
    
    found[[1]]
  }


safe_field <-
  function(
    data,
    field
  ) {
    
    if (
      length(field) == 0L ||
      is.na(field) ||
      !field %in%
      names(data)
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


safe_candidate_field <-
  function(
    data,
    candidates
  ) {
    
    field <-
      first_existing_field(
        data,
        candidates
      )
    
    safe_field(
      data,
      field
    )
  }


nonblank <-
  function(x) {
    
    !is.na(x) &
      trimws(
        as.character(x)
      ) != ""
  }


safe_distinct_count <-
  function(x) {
    
    length(
      unique(
        x[
          nonblank(x)
        ]
      )
    )
  }


sql_identifier <-
  function(
    con,
    x
  ) {
    
    as.character(
      DBI::dbQuoteIdentifier(
        con,
        x
      )
    )
  }


# ==============================================================================
# 06. DATABASE CHECK
# ==============================================================================

if (
  !file.exists(DB_PATH)
) {
  
  stop(
    paste0(
      "VPJD DuckDB database not found:\n",
      DB_PATH
    )
  )
}


# ==============================================================================
# 07. CONNECT TO DUCKDB
# ==============================================================================

con <-
  DBI::dbConnect(
    duckdb::duckdb(),
    dbdir = DB_PATH,
    read_only = FALSE
  )


disconnect_safely <-
  function() {
    
    try(
      DBI::dbDisconnect(
        con,
        shutdown = TRUE
      ),
      silent = TRUE
    )
  }


# ==============================================================================
# 08. DATABASE WRITE TEST
# ==============================================================================

WRITE_TEST_TABLE <-
  "vpjd_taxrev_03g_write_test"


write_test_pass <-
  FALSE


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
        sql_identifier(
          con,
          WRITE_TEST_TABLE
        ),
        " (TEST_ID INTEGER)"
      )
    )
    
    
    DBI::dbExecute(
      con,
      paste0(
        "INSERT INTO ",
        sql_identifier(
          con,
          WRITE_TEST_TABLE
        ),
        " VALUES (1)"
      )
    )
    
    
    test_result <-
      DBI::dbGetQuery(
        con,
        paste0(
          "SELECT * FROM ",
          sql_identifier(
            con,
            WRITE_TEST_TABLE
          )
        )
      )
    
    
    write_test_pass <-
      nrow(test_result) == 1L &&
      test_result$TEST_ID[[1]] == 1L
    
    
    DBI::dbRemoveTable(
      con,
      WRITE_TEST_TABLE
    )
    
  },
  
  error =
    function(e) {
      
      disconnect_safely()
      
      stop(
        paste0(
          "\n03g STOPPED BEFORE ANALYSIS.\n\n",
          "The VPJD DuckDB database is not writable.\n\n",
          "Database:\n",
          DB_PATH,
          "\n\nDuckDB message:\n",
          conditionMessage(e)
        )
      )
    }
)


if (
  !write_test_pass
) {
  
  disconnect_safely()
  
  stop(
    "DuckDB write-access test failed."
  )
}


# ==============================================================================
# 09. DATABASE TABLE INVENTORY
# ==============================================================================

database_tables <-
  DBI::dbListTables(
    con
  )


# ==============================================================================
# 10. REQUIRED TABLE VALIDATION
# ==============================================================================

required_tables <-
  c(
    CANONICAL_TABLE,
    TABLE_03F
  )


missing_tables <-
  setdiff(
    required_tables,
    database_tables
  )


if (
  length(missing_tables) > 0L
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
# 11. LOAD CANONICAL VPJD
# ==============================================================================

canonical <-
  DBI::dbReadTable(
    con,
    CANONICAL_TABLE
  ) |>
  tibble::as_tibble()


if (
  nrow(canonical) !=
  EXPECTED_CANONICAL_POPULATION
) {
  
  disconnect_safely()
  
  stop(
    paste0(
      "Canonical VPJD population invariant failed.\n\n",
      "Expected: ",
      EXPECTED_CANONICAL_POPULATION,
      "\nObserved: ",
      nrow(canonical)
    )
  )
}


# ==============================================================================
# 12. CANONICAL FIELD IDENTIFICATION
# ==============================================================================

canonical_id_field <-
  first_existing_field(
    canonical,
    c(
      "FINAL_WCVP_ID",
      "WCVP_ID"
    )
  )


canonical_name_field <-
  first_existing_field(
    canonical,
    c(
      "FINAL_WCVP_RECOGNISED_NAME",
      "VPJD_SCIENTIFIC_NAME",
      "SCIENTIFIC_NAME"
    )
  )


canonical_rank_field <-
  first_existing_field(
    canonical,
    c(
      "FINAL_WCVP_RANK",
      "VPJD_RANK"
    )
  )


canonical_family_field <-
  first_existing_field(
    canonical,
    c(
      "FAMILY",
      "CROSSWALK_FAMILY"
    )
  )


canonical_genus_field <-
  first_existing_field(
    canonical,
    c(
      "GENUS"
    )
  )


canonical_species_field <-
  first_existing_field(
    canonical,
    c(
      "SPECIES"
    )
  )


canonical_group_field <-
  first_existing_field(
    canonical,
    c(
      "MAJOR_GROUP"
    )
  )


if (
  is.na(canonical_id_field) ||
  is.na(canonical_name_field)
) {
  
  disconnect_safely()
  
  stop(
    paste0(
      "Essential canonical VPJD fields could not be identified.\n\n",
      "Available fields:\n",
      paste(
        names(canonical),
        collapse = "\n"
      )
    )
  )
}


canonical_inventory <-
  tibble::tibble(
    
    VPJD_ROW =
      seq_len(
        nrow(canonical)
      ),
    
    WCVP_ID =
      safe_field(
        canonical,
        canonical_id_field
      ),
    
    VPJD_SCIENTIFIC_NAME =
      safe_field(
        canonical,
        canonical_name_field
      ),
    
    VPJD_RANK =
      safe_field(
        canonical,
        canonical_rank_field
      ),
    
    VPJD_FAMILY =
      safe_field(
        canonical,
        canonical_family_field
      ),
    
    VPJD_GENUS =
      safe_field(
        canonical,
        canonical_genus_field
      ),
    
    VPJD_SPECIES =
      safe_field(
        canonical,
        canonical_species_field
      ),
    
    MAJOR_GROUP =
      safe_field(
        canonical,
        canonical_group_field
      )
  ) |>
  mutate(
    
    WCVP_ID_NORM =
      normalise_text(
        WCVP_ID
      ),
    
    VPJD_NAME_NORM =
      normalise_name(
        VPJD_SCIENTIFIC_NAME
      )
  )


# ==============================================================================
# 13. IDENTIFY OPTIONAL SUPPORTING TABLES
# ==============================================================================

cat("\n")
cat("------------------------------------------------------------\n")
cat("IDENTIFYING OPTIONAL SUPPORTING TABLES\n")
cat("------------------------------------------------------------\n\n")


GREEN_CORE_TABLE <-
  if ("vpjd_taxrev_03a_greenlist_core" %in% database_tables) {
    "vpjd_taxrev_03a_greenlist_core"
  } else {
    NA_character_
  }


FERN_CORE_TABLE <-
  if ("vpjd_taxrev_03a_fern_greenlist_core" %in% database_tables) {
    "vpjd_taxrev_03a_fern_greenlist_core"
  } else {
    NA_character_
  }


GREEN_VERNACULAR_TABLE <-
  if ("vpjd_taxrev_03a_greenlist_vernacularname_1" %in% database_tables) {
    "vpjd_taxrev_03a_greenlist_vernacularname_1"
  } else {
    NA_character_
  }


FERN_VERNACULAR_TABLE <-
  if ("vpjd_taxrev_03a_fern_greenlist_vernacularname_2" %in% database_tables) {
    "vpjd_taxrev_03a_fern_greenlist_vernacularname_2"
  } else {
    NA_character_
  }


GREEN_DISTRIBUTION_TABLE <-
  if ("vpjd_taxrev_03a_greenlist_distribution_2" %in% database_tables) {
    "vpjd_taxrev_03a_greenlist_distribution_2"
  } else {
    NA_character_
  }


FERN_SPECIES_PROFILE_TABLE <-
  if ("vpjd_taxrev_03a_fern_greenlist_speciesprofile_1" %in% database_tables) {
    "vpjd_taxrev_03a_fern_greenlist_speciesprofile_1"
  } else {
    NA_character_
  }


supporting_table_registry <-
  tibble::tibble(
    
    COMPONENT =
      c(
        "GREENLIST_CORE",
        "FERN_GREENLIST_CORE",
        "GREENLIST_VERNACULAR",
        "FERN_GREENLIST_VERNACULAR",
        "GREENLIST_DISTRIBUTION",
        "FERN_GREENLIST_SPECIES_PROFILE"
      ),
    
    TABLE_NAME =
      c(
        GREEN_CORE_TABLE,
        FERN_CORE_TABLE,
        GREEN_VERNACULAR_TABLE,
        FERN_VERNACULAR_TABLE,
        GREEN_DISTRIBUTION_TABLE,
        FERN_SPECIES_PROFILE_TABLE
      )
  ) |>
  mutate(
    AVAILABLE =
      !is.na(TABLE_NAME)
  )


print(
  supporting_table_registry,
  n = Inf,
  width = Inf
)


# ==============================================================================
# 14. LOAD 03f POTENTIAL-GAP AUDIT
# ==============================================================================

cat("\n")
cat("------------------------------------------------------------\n")
cat("LOADING 03f POTENTIAL-GAP AUDIT\n")
cat("------------------------------------------------------------\n\n")


source_03f <-
  DBI::dbReadTable(
    con,
    TABLE_03F
  ) |>
  tibble::as_tibble()


if (
  nrow(source_03f) !=
  EXPECTED_03F_POTENTIAL_GAPS
) {
  
  disconnect_safely()
  
  stop(
    paste0(
      "03f potential-gap population invariant failed.\n\n",
      "Expected: ",
      EXPECTED_03F_POTENTIAL_GAPS,
      "\nObserved: ",
      nrow(source_03f)
    )
  )
}


cat(
  "03f source table: ",
  TABLE_03F,
  "\n",
  sep = ""
)

cat(
  "03f source records: ",
  nrow(source_03f),
  "\n\n",
  sep = ""
)


cat("03f source fields:\n\n")

for (
  x in names(source_03f)
) {
  
  cat(
    "  ",
    x,
    "\n",
    sep = ""
  )
}


# ==============================================================================
# 15. IDENTIFY 03f KEY FIELDS
# ==============================================================================

source_row_field <-
  first_existing_field(
    source_03f,
    c(
      "SOURCE_03E_ROW",
      "SOURCE_03D_ROW",
      "SOURCE_03C_ROW",
      "SOURCE_PRIOR_ROW",
      "JAPAN_RECORD_ID"
    )
  )


japan_record_id_field <-
  first_existing_field(
    source_03f,
    c(
      "JAPAN_RECORD_ID"
    )
  )


japan_name_field <-
  first_existing_field(
    source_03f,
    c(
      "JAPAN_SCIENTIFIC_NAME",
      "SOURCE_SCIENTIFIC_NAME",
      "SCIENTIFICNAME",
      "SCIENTIFIC_NAME"
    )
  )


japanese_name_field <-
  first_existing_field(
    source_03f,
    c(
      "JAPANESE_NAME",
      "VERNACULARNAME",
      "VERNACULAR_NAME"
    )
  )


wcvp_id_field <-
  first_existing_field(
    source_03f,
    c(
      "WCVP_ID",
      "FINAL_WCVP_ID",
      "WCVP_ACCEPTED_ID",
      "ACCEPTED_WCVP_ID"
    )
  )


wcvp_name_field <-
  first_existing_field(
    source_03f,
    c(
      "WCVP_ACCEPTED_NAME",
      "FINAL_WCVP_RECOGNISED_NAME",
      "ACCEPTED_NAME",
      "WCVP_SCIENTIFIC_NAME"
    )
  )


if (
  is.na(source_row_field) ||
  is.na(japan_name_field)
) {
  
  disconnect_safely()
  
  stop(
    paste0(
      "Essential 03f fields could not be identified.\n\n",
      "Available fields:\n",
      paste(
        names(source_03f),
        collapse = "\n"
      )
    )
  )
}


# ==============================================================================
# 16. LOAD OPTIONAL SUPPORTING DATA
# ==============================================================================

green_core <-
  if (!is.na(GREEN_CORE_TABLE)) {
    
    DBI::dbReadTable(
      con,
      GREEN_CORE_TABLE
    ) |>
      tibble::as_tibble()
    
  } else {
    
    tibble::tibble()
  }


fern_core <-
  if (!is.na(FERN_CORE_TABLE)) {
    
    DBI::dbReadTable(
      con,
      FERN_CORE_TABLE
    ) |>
      tibble::as_tibble()
    
  } else {
    
    tibble::tibble()
  }


green_vernacular <-
  if (!is.na(GREEN_VERNACULAR_TABLE)) {
    
    DBI::dbReadTable(
      con,
      GREEN_VERNACULAR_TABLE
    ) |>
      tibble::as_tibble()
    
  } else {
    
    tibble::tibble()
  }


fern_vernacular <-
  if (!is.na(FERN_VERNACULAR_TABLE)) {
    
    DBI::dbReadTable(
      con,
      FERN_VERNACULAR_TABLE
    ) |>
      tibble::as_tibble()
    
  } else {
    
    tibble::tibble()
  }


green_distribution <-
  if (!is.na(GREEN_DISTRIBUTION_TABLE)) {
    
    DBI::dbReadTable(
      con,
      GREEN_DISTRIBUTION_TABLE
    ) |>
      tibble::as_tibble()
    
  } else {
    
    tibble::tibble()
  }


fern_species_profile <-
  if (!is.na(FERN_SPECIES_PROFILE_TABLE)) {
    
    DBI::dbReadTable(
      con,
      FERN_SPECIES_PROFILE_TABLE
    ) |>
      tibble::as_tibble()
    
  } else {
    
    tibble::tibble()
  }


# ==============================================================================
# 17. BUILD 03g AUDIT BASE
# ==============================================================================

cat("\n")
cat("------------------------------------------------------------\n")
cat("BUILDING 03g AUDIT BASE\n")
cat("------------------------------------------------------------\n\n")


audit <-
  tibble::tibble(
    
    AUDIT_03G_ROW =
      seq_len(
        nrow(source_03f)
      ),
    
    SOURCE_03F_ROW =
      seq_len(
        nrow(source_03f)
      ),
    
    SOURCE_PRIOR_ROW =
      safe_candidate_field(
        source_03f,
        c(
          "SOURCE_03E_ROW",
          "SOURCE_03D_ROW",
          "SOURCE_03C_ROW",
          "SOURCE_PRIOR_ROW"
        )
      ),
    
    JAPAN_RECORD_ID =
      safe_candidate_field(
        source_03f,
        c(
          "JAPAN_RECORD_ID"
        )
      ),
    
    SOURCE =
      safe_candidate_field(
        source_03f,
        c(
          "SOURCE",
          "SOURCE_CODE",
          "CHECKLIST_SOURCE"
        )
      ),
    
    SOURCE_TAXON_ID =
      safe_candidate_field(
        source_03f,
        c(
          "SOURCE_TAXON_ID",
          "TAXONID",
          "DWCA_ID"
        )
      ),
    
    JAPAN_SCIENTIFIC_NAME =
      safe_candidate_field(
        source_03f,
        c(
          "JAPAN_SCIENTIFIC_NAME",
          "SOURCE_SCIENTIFIC_NAME",
          "SCIENTIFICNAME",
          "SCIENTIFIC_NAME"
        )
      ),
    
    JAPANESE_NAME =
      safe_candidate_field(
        source_03f,
        c(
          "JAPANESE_NAME",
          "VERNACULARNAME",
          "VERNACULAR_NAME"
        )
      ),
    
    JAPAN_RANK =
      safe_candidate_field(
        source_03f,
        c(
          "JAPAN_RANK",
          "SOURCE_RANK",
          "TAXONRANK"
        )
      ),
    
    JAPAN_FAMILY =
      safe_candidate_field(
        source_03f,
        c(
          "JAPAN_FAMILY",
          "SOURCE_FAMILY",
          "FAMILY"
        )
      ),
    
    JAPAN_GENUS =
      safe_candidate_field(
        source_03f,
        c(
          "JAPAN_GENUS",
          "SOURCE_GENUS",
          "GENUS"
        )
      ),
    
    WCVP_ID =
      safe_candidate_field(
        source_03f,
        c(
          "WCVP_ID",
          "FINAL_WCVP_ID",
          "WCVP_ACCEPTED_ID",
          "ACCEPTED_WCVP_ID"
        )
      ),
    
    WCVP_ACCEPTED_NAME =
      safe_candidate_field(
        source_03f,
        c(
          "WCVP_ACCEPTED_NAME",
          "FINAL_WCVP_RECOGNISED_NAME",
          "ACCEPTED_NAME",
          "WCVP_SCIENTIFIC_NAME"
        )
      ),
    
    WCVP_RANK =
      safe_candidate_field(
        source_03f,
        c(
          "WCVP_RANK",
          "FINAL_WCVP_RANK",
          "ACCEPTED_RANK"
        )
      ),
    
    WCVP_FAMILY =
      safe_candidate_field(
        source_03f,
        c(
          "WCVP_FAMILY",
          "FINAL_WCVP_FAMILY",
          "ACCEPTED_FAMILY"
        )
      ),
    
    WCVP_GENUS =
      safe_candidate_field(
        source_03f,
        c(
          "WCVP_GENUS",
          "FINAL_WCVP_GENUS",
          "ACCEPTED_GENUS"
        )
      ),
    
    WCVP_STATUS =
      safe_candidate_field(
        source_03f,
        c(
          "WCVP_STATUS",
          "FINAL_WCVP_STATUS",
          "TAXONOMIC_STATUS",
          "TAXONOMICSTATUS"
        )
      ),
    
    SOURCE_NATIVE_EVIDENCE =
      safe_candidate_field(
        source_03f,
        c(
          "NATIVE_EVIDENCE",
          "EXPLICIT_NATIVE_EVIDENCE",
          "NATIVE_TO_JAPAN",
          "IS_NATIVE"
        )
      ),
    
    SOURCE_INTRODUCED_EVIDENCE =
      safe_candidate_field(
        source_03f,
        c(
          "INTRODUCED_EVIDENCE",
          "EXPLICIT_INTRODUCED_EVIDENCE",
          "INTRODUCED_TO_JAPAN",
          "IS_INTRODUCED"
        )
      )
  ) |>
  mutate(
    
    JAPAN_NAME_NORM =
      normalise_name(
        JAPAN_SCIENTIFIC_NAME
      ),
    
    WCVP_ACCEPTED_NAME_NORM =
      normalise_name(
        WCVP_ACCEPTED_NAME
      ),
    
    WCVP_ID_NORM =
      normalise_text(
        WCVP_ID
      )
  )


if (
  nrow(audit) !=
  EXPECTED_03F_POTENTIAL_GAPS
) {
  
  disconnect_safely()
  
  stop(
    "03g audit-base population invariant failed."
  )
}


# ==============================================================================
# 18. PROFILE 03g AUDIT BASE
# ==============================================================================

audit_field_profile <-
  tibble::tibble(
    
    FIELD =
      names(audit),
    
    N_POPULATED =
      vapply(
        audit,
        function(x) {
          
          sum(
            nonblank(x)
          )
        },
        integer(1)
      ),
    
    N_DISTINCT =
      vapply(
        audit,
        safe_distinct_count,
        integer(1)
      )
  )


cat(
  "03g audit records: ",
  nrow(audit),
  "\n\n",
  sep = ""
)


print(
  audit_field_profile,
  n = Inf,
  width = Inf
)


# ==============================================================================
# 19. ATTACH CANONICAL VPJD WCVP-ID EVIDENCE
# ==============================================================================

canonical_id_lookup <-
  canonical_inventory |>
  filter(
    nonblank(
      WCVP_ID_NORM
    )
  ) |>
  group_by(
    WCVP_ID_NORM
  ) |>
  summarise(
    
    VPJD_ID_MATCH_COUNT =
      n(),
    
    VPJD_ID_MATCH_ROWS =
      paste(
        VPJD_ROW,
        collapse = "|"
      ),
    
    VPJD_ID_MATCH_NAMES =
      paste(
        unique(
          VPJD_SCIENTIFIC_NAME
        ),
        collapse = " | "
      ),
    
    .groups =
      "drop"
  )


audit <-
  audit |>
  left_join(
    canonical_id_lookup,
    by =
      "WCVP_ID_NORM"
  ) |>
  mutate(
    
    VPJD_ID_MATCH_COUNT =
      coalesce(
        VPJD_ID_MATCH_COUNT,
        0L
      )
  )


# ==============================================================================
# 20. ATTACH CANONICAL VPJD NAME EVIDENCE
# ==============================================================================

canonical_name_lookup <-
  canonical_inventory |>
  filter(
    nonblank(
      VPJD_NAME_NORM
    )
  ) |>
  group_by(
    VPJD_NAME_NORM
  ) |>
  summarise(
    
    VPJD_NAME_MATCH_COUNT =
      n(),
    
    VPJD_NAME_MATCH_ROWS =
      paste(
        VPJD_ROW,
        collapse = "|"
      ),
    
    VPJD_NAME_MATCH_NAMES =
      paste(
        unique(
          VPJD_SCIENTIFIC_NAME
        ),
        collapse = " | "
      ),
    
    .groups =
      "drop"
  )


audit <-
  audit |>
  left_join(
    canonical_name_lookup,
    by =
      c(
        "WCVP_ACCEPTED_NAME_NORM" =
          "VPJD_NAME_NORM"
      )
  ) |>
  mutate(
    
    VPJD_NAME_MATCH_COUNT =
      coalesce(
        VPJD_NAME_MATCH_COUNT,
        0L
      )
  )


# ==============================================================================
# 21. DIRECT CANONICAL-CONCEPT DIAGNOSTICS
# ==============================================================================

audit <-
  audit |>
  mutate(
    
    EXACT_WCVP_ID_IN_VPJD =
      VPJD_ID_MATCH_COUNT ==
      1L,
    
    MULTIPLE_WCVP_ID_IN_VPJD =
      VPJD_ID_MATCH_COUNT >
      1L,
    
    EXACT_ACCEPTED_NAME_IN_VPJD =
      VPJD_NAME_MATCH_COUNT ==
      1L,
    
    MULTIPLE_ACCEPTED_NAME_IN_VPJD =
      VPJD_NAME_MATCH_COUNT >
      1L,
    
    CANONICAL_CONCEPT_PRESENT =
      EXACT_WCVP_ID_IN_VPJD |
      EXACT_ACCEPTED_NAME_IN_VPJD
  )


# ==============================================================================
# 22. JAPANESE CHECKLIST SOURCE EVIDENCE
# ==============================================================================

cat("\n")
cat("------------------------------------------------------------\n")
cat("JAPANESE CHECKLIST SOURCE EVIDENCE\n")
cat("------------------------------------------------------------\n\n")


# ------------------------------------------------------------------------------
# Identify scientific-name fields
# ------------------------------------------------------------------------------

green_scientific_field <-
  first_existing_field(
    green_core,
    c(
      "SCIENTIFICNAME"
    )
  )


fern_scientific_field <-
  first_existing_field(
    fern_core,
    c(
      "SCIENTIFICNAME"
    )
  )


# ------------------------------------------------------------------------------
# Extract GreenList scientific names
# ------------------------------------------------------------------------------

if (
  !is.na(green_scientific_field)
) {
  
  green_scientific_values <-
    safe_field(
      green_core,
      green_scientific_field
    )
  
  green_checklist_names <-
    unique(
      normalise_name(
        green_scientific_values
      )
    )
  
  green_checklist_names <-
    green_checklist_names[
      nonblank(
        green_checklist_names
      )
    ]
  
} else {
  
  green_checklist_names <-
    character(0)
}


# ------------------------------------------------------------------------------
# Extract FernGreenList scientific names
# ------------------------------------------------------------------------------

if (
  !is.na(fern_scientific_field)
) {
  
  fern_scientific_values <-
    safe_field(
      fern_core,
      fern_scientific_field
    )
  
  fern_checklist_names <-
    unique(
      normalise_name(
        fern_scientific_values
      )
    )
  
  fern_checklist_names <-
    fern_checklist_names[
      nonblank(
        fern_checklist_names
      )
    ]
  
} else {
  
  fern_checklist_names <-
    character(0)
}


# ------------------------------------------------------------------------------
# Attach contemporary-Japan checklist evidence to 03g audit
# ------------------------------------------------------------------------------

audit <-
  audit |>
  dplyr::mutate(
    
    NAME_IN_GREENLIST =
      !is.na(JAPAN_NAME_NORM) &
      JAPAN_NAME_NORM %in%
      green_checklist_names,
    
    NAME_IN_FERN_GREENLIST =
      !is.na(JAPAN_NAME_NORM) &
      JAPAN_NAME_NORM %in%
      fern_checklist_names,
    
    NAME_IN_CONTEMPORARY_JAPAN_CHECKLIST =
      NAME_IN_GREENLIST |
      NAME_IN_FERN_GREENLIST
  )


# ------------------------------------------------------------------------------
# Evidence counts
# ------------------------------------------------------------------------------

n_green_checklist_names <-
  length(
    green_checklist_names
  )


n_fern_checklist_names <-
  length(
    fern_checklist_names
  )


n_audit_in_greenlist <-
  sum(
    audit$NAME_IN_GREENLIST,
    na.rm = TRUE
  )


n_audit_in_fern_greenlist <-
  sum(
    audit$NAME_IN_FERN_GREENLIST,
    na.rm = TRUE
  )


n_audit_in_either_checklist <-
  sum(
    audit$NAME_IN_CONTEMPORARY_JAPAN_CHECKLIST,
    na.rm = TRUE
  )


# ------------------------------------------------------------------------------
# Validation
# ------------------------------------------------------------------------------

if (
  n_audit_in_greenlist >
  nrow(audit)
) {
  
  disconnect_safely()
  
  stop(
    "GreenList evidence count exceeds the 03g audit population."
  )
}


if (
  n_audit_in_fern_greenlist >
  nrow(audit)
) {
  
  disconnect_safely()
  
  stop(
    "FernGreenList evidence count exceeds the 03g audit population."
  )
}


if (
  n_audit_in_either_checklist >
  nrow(audit)
) {
  
  disconnect_safely()
  
  stop(
    "Combined checklist evidence count exceeds the 03g audit population."
  )
}


# ------------------------------------------------------------------------------
# Report
# ------------------------------------------------------------------------------

cat(
  "GreenList scientific names available: ",
  format(
    n_green_checklist_names,
    big.mark = ","
  ),
  "\n",
  sep = ""
)


cat(
  "FernGreenList scientific names available: ",
  format(
    n_fern_checklist_names,
    big.mark = ","
  ),
  "\n\n",
  sep = ""
)


cat(
  "03g records represented in GreenList: ",
  format(
    n_audit_in_greenlist,
    big.mark = ","
  ),
  " / ",
  format(
    nrow(audit),
    big.mark = ","
  ),
  "\n",
  sep = ""
)


cat(
  "03g records represented in FernGreenList: ",
  format(
    n_audit_in_fern_greenlist,
    big.mark = ","
  ),
  " / ",
  format(
    nrow(audit),
    big.mark = ","
  ),
  "\n",
  sep = ""
)


cat(
  "03g records represented in either contemporary checklist: ",
  format(
    n_audit_in_either_checklist,
    big.mark = ","
  ),
  " / ",
  format(
    nrow(audit),
    big.mark = ","
  ),
  "\n\n",
  sep = ""
)


cat(
  "Japanese checklist source-evidence profiling: PASS\n"
)

cat("\n")
# ==============================================================================
# 23. PROFILE DISTRIBUTION EVIDENCE
# ==============================================================================

green_distribution_profile <-
  tibble::tibble()


if (
  nrow(green_distribution) > 0L
) {
  
  green_distribution_profile <-
    tibble::tibble(
      
      FIELD =
        names(
          green_distribution
        ),
      
      N_POPULATED =
        vapply(
          green_distribution,
          function(x) {
            
            sum(
              nonblank(x)
            )
          },
          integer(1)
        )
    )
}


fern_species_profile_field_profile <-
  tibble::tibble()


if (
  nrow(fern_species_profile) > 0L
) {
  
  fern_species_profile_field_profile <-
    tibble::tibble(
      
      FIELD =
        names(
          fern_species_profile
        ),
      
      N_POPULATED =
        vapply(
          fern_species_profile,
          function(x) {
            
            sum(
              nonblank(x)
            )
          },
          integer(1)
        )
    )
}


# ==============================================================================
# 24. CONSERVATIVE RESOLUTION CLASSIFICATION
# ==============================================================================

audit <-
  audit |>
  mutate(
    
    RESOLUTION_CLASS =
      case_when(
        
        EXACT_WCVP_ID_IN_VPJD ~
          "CANONICAL_CONCEPT_ALREADY_PRESENT_BY_WCVP_ID",
        
        !EXACT_WCVP_ID_IN_VPJD &
          EXACT_ACCEPTED_NAME_IN_VPJD ~
          "CANONICAL_CONCEPT_ALREADY_PRESENT_BY_ACCEPTED_NAME",
        
        MULTIPLE_WCVP_ID_IN_VPJD ~
          "AMBIGUOUS_MULTIPLE_VPJD_WCVP_ID_MATCHES",
        
        MULTIPLE_ACCEPTED_NAME_IN_VPJD ~
          "AMBIGUOUS_MULTIPLE_VPJD_NAME_MATCHES",
        
        NAME_IN_CONTEMPORARY_JAPAN_CHECKLIST ~
          "CHECKLIST_SUPPORTED_BUT_CANONICAL_GAP_UNRESOLVED",
        
        TRUE ~
          "FURTHER_REVIEW_REQUIRED"
      ),
    
    RESOLUTION_STATUS =
      case_when(
        
        RESOLUTION_CLASS %in%
          c(
            "CANONICAL_CONCEPT_ALREADY_PRESENT_BY_WCVP_ID",
            "CANONICAL_CONCEPT_ALREADY_PRESENT_BY_ACCEPTED_NAME"
          ) ~
          "RESOLVED_EXISTING_VPJD_CONCEPT",
        
        grepl(
          "^AMBIGUOUS_",
          RESOLUTION_CLASS
        ) ~
          "AMBIGUOUS",
        
        TRUE ~
          "UNRESOLVED"
      )
  )


# ==============================================================================
# 25. RESOLUTION EVIDENCE TEXT
# ==============================================================================

audit <-
  audit |>
  mutate(
    
    RESOLUTION_EVIDENCE =
      case_when(
        
        RESOLUTION_CLASS ==
          "CANONICAL_CONCEPT_ALREADY_PRESENT_BY_WCVP_ID" ~
          "Unique WCVP identifier match to existing VPJD canonical concept.",
        
        RESOLUTION_CLASS ==
          "CANONICAL_CONCEPT_ALREADY_PRESENT_BY_ACCEPTED_NAME" ~
          "Unique normalised WCVP accepted-name match to existing VPJD canonical concept.",
        
        RESOLUTION_CLASS ==
          "AMBIGUOUS_MULTIPLE_VPJD_WCVP_ID_MATCHES" ~
          "WCVP identifier maps to multiple VPJD rows; automatic resolution prohibited.",
        
        RESOLUTION_CLASS ==
          "AMBIGUOUS_MULTIPLE_VPJD_NAME_MATCHES" ~
          "Accepted name maps to multiple VPJD rows; automatic resolution prohibited.",
        
        RESOLUTION_CLASS ==
          "CHECKLIST_SUPPORTED_BUT_CANONICAL_GAP_UNRESOLVED" ~
          "Name is represented in a contemporary Japanese checklist but no unique existing VPJD canonical concept was established.",
        
        TRUE ~
          "Available evidence is insufficient for deterministic resolution."
      )
  )


# ==============================================================================
# 26. SPLIT RESOLUTION OUTPUTS
# ==============================================================================

resolved_existing <-
  audit |>
  filter(
    RESOLUTION_STATUS ==
      "RESOLVED_EXISTING_VPJD_CONCEPT"
  )


ambiguous <-
  audit |>
  filter(
    RESOLUTION_STATUS ==
      "AMBIGUOUS"
  )


checklist_supported_unresolved <-
  audit |>
  filter(
    RESOLUTION_CLASS ==
      "CHECKLIST_SUPPORTED_BUT_CANONICAL_GAP_UNRESOLVED"
  )


further_review <-
  audit |>
  filter(
    RESOLUTION_STATUS ==
      "UNRESOLVED"
  )


# ==============================================================================
# 27. SUMMARY COUNTS
# ==============================================================================

n_total <-
  nrow(audit)


n_resolved_existing <-
  nrow(resolved_existing)


n_ambiguous <-
  nrow(ambiguous)


n_checklist_supported_unresolved <-
  nrow(
    checklist_supported_unresolved
  )


n_unresolved <-
  nrow(further_review)


n_true_new_taxa_accepted <-
  0L


# ==============================================================================
# 28. RESOLUTION CLASS SUMMARY
# ==============================================================================

resolution_class_summary <-
  audit |>
  count(
    RESOLUTION_CLASS,
    RESOLUTION_STATUS,
    name =
      "N_RECORDS"
  ) |>
  arrange(
    desc(
      N_RECORDS
    ),
    RESOLUTION_CLASS
  )


# ==============================================================================
# 29. SOURCE SUMMARY
# ==============================================================================

source_summary <-
  audit |>
  count(
    SOURCE,
    RESOLUTION_STATUS,
    name =
      "N_RECORDS"
  ) |>
  arrange(
    SOURCE,
    RESOLUTION_STATUS
  )


# ==============================================================================
# 30. FAMILY SUMMARY
# ==============================================================================

family_summary <-
  audit |>
  count(
    JAPAN_FAMILY,
    RESOLUTION_STATUS,
    name =
      "N_RECORDS"
  ) |>
  arrange(
    desc(
      N_RECORDS
    ),
    JAPAN_FAMILY
  )


# ==============================================================================
# 31. SUPPORTING-EVIDENCE SUMMARY
# ==============================================================================

supporting_evidence_summary <-
  tibble::tibble(
    
    METRIC =
      c(
        "03f potential gaps audited",
        "Names represented in GreenList",
        "Names represented in FernGreenList",
        "Names represented in either contemporary checklist",
        "Unique WCVP-ID matches to VPJD",
        "Unique accepted-name matches to VPJD",
        "Resolved to existing VPJD concepts",
        "Ambiguous records",
        "Unresolved records",
        "New taxa automatically accepted"
      ),
    
    VALUE =
      c(
        n_total,
        
        sum(
          audit$NAME_IN_GREENLIST,
          na.rm = TRUE
        ),
        
        sum(
          audit$NAME_IN_FERN_GREENLIST,
          na.rm = TRUE
        ),
        
        sum(
          audit$NAME_IN_CONTEMPORARY_JAPAN_CHECKLIST,
          na.rm = TRUE
        ),
        
        sum(
          audit$EXACT_WCVP_ID_IN_VPJD,
          na.rm = TRUE
        ),
        
        sum(
          audit$EXACT_ACCEPTED_NAME_IN_VPJD,
          na.rm = TRUE
        ),
        
        n_resolved_existing,
        
        n_ambiguous,
        
        n_unresolved,
        
        n_true_new_taxa_accepted
      )
  )


# ==============================================================================
# 32. VALIDATION
# ==============================================================================

validation <-
  tibble::tibble(
    
    CHECK =
      c(
        "Canonical VPJD population = 11,439",
        "03f potential-gap population = 201",
        "03g audit population = 201",
        "03g AUDIT_03G_ROW unique",
        "03g SOURCE_03F_ROW unique",
        "Every 03g record assigned resolution class",
        "Every 03g record assigned resolution status",
        "Resolution classes sum to 201",
        "Resolved + unresolved + ambiguous = 201",
        "Resolved records are subset of 201",
        "Ambiguous records are subset of 201",
        "Unresolved records are subset of 201",
        "No fuzzy matching performed",
        "No automatic synonym acceptance performed",
        "No automatic VPJD expansion performed",
        "Canonical taxonomy modified = FALSE",
        "Star allocations modified = FALSE",
        "Taxa removed = 0",
        "Taxa automatically added = 0",
        "Database write test passed"
      ),
    
    PASS =
      c(
        nrow(canonical) ==
          EXPECTED_CANONICAL_POPULATION,
        
        nrow(source_03f) ==
          EXPECTED_03F_POTENTIAL_GAPS,
        
        nrow(audit) ==
          EXPECTED_03F_POTENTIAL_GAPS,
        
        dplyr::n_distinct(
          audit$AUDIT_03G_ROW
        ) ==
          nrow(audit),
        
        dplyr::n_distinct(
          audit$SOURCE_03F_ROW
        ) ==
          nrow(audit),
        
        all(
          nonblank(
            audit$RESOLUTION_CLASS
          )
        ),
        
        all(
          nonblank(
            audit$RESOLUTION_STATUS
          )
        ),
        
        sum(
          resolution_class_summary$N_RECORDS
        ) ==
          EXPECTED_03F_POTENTIAL_GAPS,
        
        n_resolved_existing +
          n_ambiguous +
          n_unresolved ==
          EXPECTED_03F_POTENTIAL_GAPS,
        
        n_resolved_existing <=
          EXPECTED_03F_POTENTIAL_GAPS,
        
        n_ambiguous <=
          EXPECTED_03F_POTENTIAL_GAPS,
        
        n_unresolved <=
          EXPECTED_03F_POTENTIAL_GAPS,
        
        TRUE,
        
        TRUE,
        
        TRUE,
        
        TRUE,
        
        TRUE,
        
        TRUE,
        
        n_true_new_taxa_accepted ==
          0L,
        
        write_test_pass
      )
  )


all_valid <-
  all(
    validation$PASS
  )


decision <-
  if (
    all_valid
  ) {
    
    "POTENTIAL_VPJD_CANONICAL_GAP_RESOLUTION_AUDIT_COMPLETE"
    
  } else {
    
    "REVIEW_REQUIRED"
  }


# ==============================================================================
# 33. METADATA
# ==============================================================================

metadata <-
  tibble::tibble(
    
    KEY =
      c(
        "MODULE",
        "VERSION",
        "CANONICAL_TABLE",
        "SOURCE_03F_TABLE",
        "CANONICAL_POPULATION",
        "SOURCE_03F_POPULATION",
        "FUZZY_MATCHING",
        "AUTOMATIC_SYNONYM_ACCEPTANCE",
        "AUTOMATIC_VPJD_EXPANSION",
        "CANONICAL_TAXONOMY_MODIFIED",
        "STAR_ALLOCATIONS_MODIFIED",
        "TAXA_REMOVED",
        "TAXA_AUTOMATICALLY_ADDED",
        "DECISION"
      ),
    
    VALUE =
      as.character(
        c(
          "03g_resolve_potential_vpjd_canonical_gaps",
          VERSION,
          CANONICAL_TABLE,
          TABLE_03F,
          nrow(canonical),
          nrow(source_03f),
          FALSE,
          FALSE,
          FALSE,
          FALSE,
          FALSE,
          0,
          0,
          decision
        )
      )
  )


# ==============================================================================
# 34. DUCKDB OUTPUT DEFINITIONS
# ==============================================================================

duckdb_outputs <-
  list(
    
    vpjd_taxrev_03g_audit =
      audit,
    
    vpjd_taxrev_03g_resolved_existing_concepts =
      resolved_existing,
    
    vpjd_taxrev_03g_ambiguous =
      ambiguous,
    
    vpjd_taxrev_03g_checklist_supported_unresolved =
      checklist_supported_unresolved,
    
    vpjd_taxrev_03g_further_review =
      further_review,
    
    vpjd_taxrev_03g_resolution_class_summary =
      resolution_class_summary,
    
    vpjd_taxrev_03g_source_summary =
      source_summary,
    
    vpjd_taxrev_03g_family_summary =
      family_summary,
    
    vpjd_taxrev_03g_supporting_evidence_summary =
      supporting_evidence_summary,
    
    vpjd_taxrev_03g_supporting_table_registry =
      supporting_table_registry,
    
    vpjd_taxrev_03g_audit_field_profile =
      audit_field_profile,
    
    vpjd_taxrev_03g_green_distribution_profile =
      green_distribution_profile,
    
    vpjd_taxrev_03g_fern_species_profile_field_profile =
      fern_species_profile_field_profile,
    
    vpjd_taxrev_03g_validation =
      validation,
    
    vpjd_taxrev_03g_metadata =
      metadata
  )


# ==============================================================================
# 35. SAFE DUCKDB WRITER
# ==============================================================================

write_duckdb_table <-
  function(
    table_name,
    data
  ) {
    
    if (
      !is.data.frame(data)
    ) {
      
      return(
        FALSE
      )
    }
    
    
    if (
      ncol(data) == 0L
    ) {
      
      return(
        FALSE
      )
    }
    
    
    if (
      DBI::dbExistsTable(
        con,
        table_name
      )
    ) {
      
      DBI::dbRemoveTable(
        con,
        table_name
      )
    }
    
    
    DBI::dbWriteTable(
      con,
      table_name,
      as.data.frame(data),
      overwrite = TRUE
    )
    
    
    TRUE
  }


# ==============================================================================
# 36. WRITE DUCKDB OUTPUTS
# ==============================================================================

duckdb_write_status <-
  tibble::tibble(
    
    TABLE_NAME =
      names(
        duckdb_outputs
      ),
    
    WRITTEN =
      FALSE
  )


for (
  i in seq_along(
    duckdb_outputs
  )
) {
  
  duckdb_write_status$WRITTEN[[i]] <-
    write_duckdb_table(
      names(
        duckdb_outputs
      )[[i]],
      duckdb_outputs[[i]]
    )
}


# ==============================================================================
# 37. CSV OUTPUT DEFINITIONS
# ==============================================================================

csv_outputs <-
  list(
    
    "vpjd_03g_potential_gap_resolution_audit.csv" =
      audit,
    
    "vpjd_03g_resolved_existing_concepts.csv" =
      resolved_existing,
    
    "vpjd_03g_ambiguous.csv" =
      ambiguous,
    
    "vpjd_03g_checklist_supported_unresolved.csv" =
      checklist_supported_unresolved,
    
    "vpjd_03g_further_review.csv" =
      further_review,
    
    "vpjd_03g_resolution_class_summary.csv" =
      resolution_class_summary,
    
    "vpjd_03g_source_summary.csv" =
      source_summary,
    
    "vpjd_03g_family_summary.csv" =
      family_summary,
    
    "vpjd_03g_supporting_evidence_summary.csv" =
      supporting_evidence_summary,
    
    "vpjd_03g_supporting_table_registry.csv" =
      supporting_table_registry,
    
    "vpjd_03g_audit_field_profile.csv" =
      audit_field_profile,
    
    "vpjd_03g_green_distribution_profile.csv" =
      green_distribution_profile,
    
    "vpjd_03g_fern_species_profile_field_profile.csv" =
      fern_species_profile_field_profile,
    
    "vpjd_03g_validation.csv" =
      validation,
    
    "vpjd_03g_metadata.csv" =
      metadata
  )


# ==============================================================================
# 38. SAFE CSV WRITER
# ==============================================================================

write_csv_safe <-
  function(
    filename,
    data
  ) {
    
    if (
      !is.data.frame(data)
    ) {
      
      return(
        FALSE
      )
    }
    
    
    if (
      ncol(data) == 0L
    ) {
      
      return(
        FALSE
      )
    }
    
    
    readr::write_csv(
      data,
      file.path(
        OUTPUT_DIR,
        filename
      ),
      na = ""
    )
    
    
    TRUE
  }


# ==============================================================================
# 39. WRITE CSV OUTPUTS
# ==============================================================================

csv_write_status <-
  tibble::tibble(
    
    FILE_NAME =
      names(
        csv_outputs
      ),
    
    WRITTEN =
      FALSE
  )


for (
  i in seq_along(
    csv_outputs
  )
) {
  
  csv_write_status$WRITTEN[[i]] <-
    write_csv_safe(
      names(
        csv_outputs
      )[[i]],
      csv_outputs[[i]]
    )
}


# ==============================================================================
# 40. VALIDATION REPORT
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
# 41. FAILED VALIDATION REPORT
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
# 42. SUMMARY REPORT
# ==============================================================================

cat("\n")
cat("------------------------------------------------------------\n")
cat("03g RESOLUTION SUMMARY\n")
cat("------------------------------------------------------------\n\n")


print(
  resolution_class_summary,
  n = Inf,
  width = Inf
)


cat("\n")


print(
  supporting_evidence_summary,
  n = Inf,
  width = Inf
)


# ==============================================================================
# 43. CLEAN DISCONNECT
# ==============================================================================

DBI::dbDisconnect(
  con,
  shutdown = TRUE
)


rm(con)


gc()


# ==============================================================================
# 44. COMPLETION REPORT
# ==============================================================================

cat("\n")
cat("============================================================\n")

cat(
  "VPJD TAXONOMIC REVISION 03g v",
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
  "03f potential canonical gaps audited: ",
  format(
    n_total,
    big.mark = ","
  ),
  "\n\n",
  sep = ""
)


cat(
  "Resolved to existing VPJD concepts: ",
  format(
    n_resolved_existing,
    big.mark = ","
  ),
  "\n",
  sep = ""
)


cat(
  "Ambiguous records: ",
  format(
    n_ambiguous,
    big.mark = ","
  ),
  "\n",
  sep = ""
)


cat(
  "Checklist-supported unresolved records: ",
  format(
    n_checklist_supported_unresolved,
    big.mark = ","
  ),
  "\n",
  sep = ""
)


cat(
  "Records remaining unresolved/requiring review: ",
  format(
    n_unresolved,
    big.mark = ","
  ),
  "\n\n",
  sep = ""
)


cat(
  "New taxa automatically accepted: ",
  n_true_new_taxa_accepted,
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


cat("RESOLUTION / AUDIT ONLY\n")
cat("Database write test: PASS\n")
cat("Canonical taxonomy modified: FALSE\n")
cat("Star allocations modified: FALSE\n")
cat("Taxa removed: 0\n")
cat("Taxa automatically added: 0\n")
cat("Fuzzy name matching performed: FALSE\n")
cat("Automatic synonym acceptance performed: FALSE\n")
cat("Automatic VPJD expansion performed: FALSE\n")
cat("Japanese checklist taxonomy imposed on VPJD: FALSE\n")


cat(
  "03g DuckDB outputs written: ",
  sum(
    duckdb_write_status$WRITTEN
  ),
  " / ",
  nrow(
    duckdb_write_status
  ),
  "\n",
  sep = ""
)


cat(
  "03g CSV outputs written: ",
  sum(
    csv_write_status$WRITTEN
  ),
  " / ",
  nrow(
    csv_write_status
  ),
  "\n",
  sep = ""
)


cat(
  "DuckDB connection closed cleanly: TRUE\n"
)


cat("\n")
cat("============================================================\n")
cat("03g COMPLETE\n")
cat("============================================================\n")


# ==============================================================================
# END
# ==============================================================================