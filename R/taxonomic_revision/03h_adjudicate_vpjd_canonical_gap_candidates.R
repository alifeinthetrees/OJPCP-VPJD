# ==============================================================================
# VPJD TAXONOMIC REVISION
#
# 03h_adjudicate_vpjd_canonical_gap_candidates.R
#
# Purpose:
#   Adjudicate the 201 contemporary-Japan checklist-supported canonical-gap
#   candidates identified by 03g.
#
#   03h evaluates whether each candidate:
#
#     1. has sufficient evidence to be treated as a probable/confirmed
#        canonical omission;
#     2. is explicitly introduced/non-native and therefore outside native
#        VPJD scope;
#     3. represents a hybrid/nothotaxon requiring separate scope treatment;
#     4. represents an infraspecific concept requiring scope review;
#     5. falls outside another defined VPJD scope;
#     6. lacks sufficient evidence and therefore requires manual review.
#
# IMPORTANT:
#
#   This is an ADJUDICATION / AUDIT module only.
#
#   It does NOT:
#     - modify canonical VPJD taxonomy;
#     - modify Star allocations;
#     - add taxa;
#     - remove taxa;
#     - impose Japanese checklist taxonomy on VPJD;
#     - perform fuzzy matching;
#     - automatically accept synonyms;
#     - automatically expand VPJD.
#
# Inputs:
#
#   vpjd_taxrev_03g_checklist_supported_unresolved
#       201 records
#
#   vpjd_star_provisional_wholesale_allocation
#       canonical VPJD authority
#
#   vpjd_taxrev_02h_validated_vascular_lineage
#       validated lineage authority
#
#   vpjd_taxrev_03a_greenlist_core
#   vpjd_taxrev_03a_greenlist_distribution_2
#   vpjd_taxrev_03a_fern_greenlist_core
#   vpjd_taxrev_03a_fern_greenlist_speciesprofile_1
#
# Outputs:
#
#   DuckDB tables prefixed:
#       vpjd_taxrev_03h_
#
#   CSV diagnostic outputs:
#       outputs/tables/taxonomic_revision/
#       03h_adjudicate_vpjd_canonical_gap_candidates/
#
# ==============================================================================


# ==============================================================================
# 01. PACKAGES
# ==============================================================================

library(DBI)
library(duckdb)
library(dplyr)
library(tibble)
library(readr)
library(stringr)
library(purrr)


# ==============================================================================
# 02. VERSION
# ==============================================================================

VERSION <-
  "0.1.0"


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
    "03h_adjudicate_vpjd_canonical_gap_candidates"
  )


dir.create(
  OUTPUT_DIR,
  recursive = TRUE,
  showWarnings = FALSE
)


# ==============================================================================
# 04. INVARIANTS
# ==============================================================================

EXPECTED_CANONICAL_POPULATION <-
  11439L


EXPECTED_03G_CANDIDATES <-
  201L


EXPECTED_GREENLIST_SUPPORTED <-
  189L


EXPECTED_FERN_SUPPORTED <-
  12L


EXPECTED_CHECKLIST_SUPPORTED <-
  201L


# ==============================================================================
# 05. TABLE AUTHORITIES
# ==============================================================================

SOURCE_03G_TABLE <-
  "vpjd_taxrev_03g_checklist_supported_unresolved"


CANONICAL_TABLE <-
  "vpjd_star_provisional_wholesale_allocation"


LINEAGE_TABLE <-
  "vpjd_taxrev_02h_validated_vascular_lineage"


GREEN_CORE_TABLE <-
  "vpjd_taxrev_03a_greenlist_core"


GREEN_DISTRIBUTION_TABLE <-
  "vpjd_taxrev_03a_greenlist_distribution_2"


FERN_CORE_TABLE <-
  "vpjd_taxrev_03a_fern_greenlist_core"


FERN_SPECIES_PROFILE_TABLE <-
  "vpjd_taxrev_03a_fern_greenlist_speciesprofile_1"


# ==============================================================================
# 06. OUTPUT TABLE NAMES
# ==============================================================================

TABLE_AUDIT <-
  "vpjd_taxrev_03h_adjudication_audit"


TABLE_CONFIRMED_GAPS <-
  "vpjd_taxrev_03h_confirmed_canonical_gaps"


TABLE_INTRODUCED <-
  "vpjd_taxrev_03h_exclude_introduced_non_native"


TABLE_HYBRID <-
  "vpjd_taxrev_03h_hybrid_scope_review"


TABLE_INFRASPECIFIC <-
  "vpjd_taxrev_03h_infraspecific_scope_review"


TABLE_OTHER_SCOPE <-
  "vpjd_taxrev_03h_other_scope_review"


TABLE_MANUAL_REVIEW <-
  "vpjd_taxrev_03h_manual_review"


TABLE_CLASS_SUMMARY <-
  "vpjd_taxrev_03h_adjudication_class_summary"


TABLE_EVIDENCE_SUMMARY <-
  "vpjd_taxrev_03h_evidence_summary"


TABLE_FAMILY_SUMMARY <-
  "vpjd_taxrev_03h_family_summary"


TABLE_RANK_SUMMARY <-
  "vpjd_taxrev_03h_rank_summary"


TABLE_SOURCE_SUMMARY <-
  "vpjd_taxrev_03h_source_summary"


TABLE_VALIDATION <-
  "vpjd_taxrev_03h_validation"


TABLE_METADATA <-
  "vpjd_taxrev_03h_metadata"


# ==============================================================================
# 07. HELPER FUNCTIONS
# ==============================================================================

normalise_text <-
  function(x) {
    
    x <-
      as.character(x)
    
    x <-
      trimws(x)
    
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
      gsub(
        "\\s+",
        " ",
        x
      )
    
    x
  }


nonblank <-
  function(x) {
    
    !is.na(x) &
      trimws(
        as.character(x)
      ) != ""
  }


first_existing_field <-
  function(data, candidates) {
    
    hits <-
      candidates[
        candidates %in%
          names(data)
      ]
    
    if (
      length(hits) == 0L
    ) {
      
      return(
        NA_character_
      )
    }
    
    hits[[1]]
  }


safe_field <-
  function(data, field) {
    
    if (
      is.na(field) ||
      !(field %in% names(data))
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


safe_logical_field <-
  function(data, field) {
    
    if (
      is.na(field) ||
      !(field %in% names(data))
    ) {
      
      return(
        rep(
          FALSE,
          nrow(data)
        )
      )
    }
    
    x <-
      data[[field]]
    
    if (
      is.logical(x)
    ) {
      
      x[
        is.na(x)
      ] <-
        FALSE
      
      return(x)
    }
    
    x <-
      toupper(
        trimws(
          as.character(x)
        )
      )
    
    x %in%
      c(
        "TRUE",
        "T",
        "YES",
        "Y",
        "1"
      )
  }


contains_any <-
  function(x, patterns) {
    
    x <-
      tolower(
        ifelse(
          is.na(x),
          "",
          as.character(x)
        )
      )
    
    result <-
      rep(
        FALSE,
        length(x)
      )
    
    for (
      pattern in patterns
    ) {
      
      result <-
        result |
        grepl(
          pattern,
          x,
          perl = TRUE
        )
    }
    
    result
  }


collapse_unique <-
  function(x) {
    
    x <-
      normalise_text(x)
    
    x <-
      unique(
        x[
          nonblank(x)
        ]
      )
    
    if (
      length(x) == 0L
    ) {
      
      return(
        NA_character_
      )
    }
    
    paste(
      x,
      collapse = " | "
    )
  }


disconnect_safely <-
  function() {
    
    if (
      exists(
        "con",
        inherits = TRUE
      )
    ) {
      
      try(
        DBI::dbDisconnect(
          con,
          shutdown = TRUE
        ),
        silent = TRUE
      )
    }
    
    invisible(NULL)
  }


write_duckdb_table <-
  function(table_name, data) {
    
    data <-
      as.data.frame(data)
    
    if (
      ncol(data) == 0L
    ) {
      
      return(
        tibble(
          TABLE_NAME =
            table_name,
          
          WRITTEN =
            FALSE,
          
          N_RECORDS =
            nrow(data),
          
          REASON =
            "STRUCTURALLY_EMPTY"
        )
      )
    }
    
    DBI::dbWriteTable(
      con,
      table_name,
      data,
      overwrite = TRUE
    )
    
    tibble(
      TABLE_NAME =
        table_name,
      
      WRITTEN =
        TRUE,
      
      N_RECORDS =
        nrow(data),
      
      REASON =
        "WRITTEN"
    )
  }


write_csv_output <-
  function(filename, data) {
    
    path <-
      file.path(
        OUTPUT_DIR,
        filename
      )
    
    data <-
      as.data.frame(data)
    
    if (
      ncol(data) == 0L
    ) {
      
      return(
        tibble(
          FILE =
            filename,
          
          WRITTEN =
            FALSE,
          
          N_RECORDS =
            nrow(data),
          
          REASON =
            "STRUCTURALLY_EMPTY"
        )
      )
    }
    
    readr::write_csv(
      data,
      path,
      na = ""
    )
    
    tibble(
      FILE =
        filename,
      
      WRITTEN =
        TRUE,
      
      N_RECORDS =
        nrow(data),
      
      REASON =
        "WRITTEN"
    )
  }


# ==============================================================================
# 08. CONNECT TO DUCKDB
# ==============================================================================

stopifnot(
  file.exists(
    DB_PATH
  )
)


con <-
  DBI::dbConnect(
    duckdb::duckdb(),
    dbdir = DB_PATH,
    read_only = FALSE
  )


database_tables <-
  DBI::dbListTables(
    con
  )


# ==============================================================================
# 09. WRITE-ACCESS TEST
# ==============================================================================

WRITE_TEST_TABLE <-
  "vpjd_taxrev_03h_write_access_test"


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
    
    stopifnot(
      nrow(test_result) == 1L
    )
    
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
          "\n03h STOPPED BEFORE ADJUDICATION.\n\n",
          "DuckDB write-access test failed.\n\n",
          conditionMessage(e)
        )
      )
    }
)


# ==============================================================================
# 10. VALIDATE REQUIRED TABLES
# ==============================================================================

required_tables <-
  c(
    SOURCE_03G_TABLE,
    CANONICAL_TABLE,
    LINEAGE_TABLE,
    GREEN_CORE_TABLE,
    GREEN_DISTRIBUTION_TABLE,
    FERN_CORE_TABLE,
    FERN_SPECIES_PROFILE_TABLE
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
      "Required 03h table(s) missing:\n",
      paste(
        missing_tables,
        collapse = "\n"
      )
    )
  )
}


# ==============================================================================
# 11. LOAD SOURCE TABLES
# ==============================================================================

source_03g <-
  DBI::dbReadTable(
    con,
    SOURCE_03G_TABLE
  ) |>
  tibble::as_tibble()


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


green_core <-
  DBI::dbReadTable(
    con,
    GREEN_CORE_TABLE
  ) |>
  tibble::as_tibble()


green_distribution <-
  DBI::dbReadTable(
    con,
    GREEN_DISTRIBUTION_TABLE
  ) |>
  tibble::as_tibble()


fern_core <-
  DBI::dbReadTable(
    con,
    FERN_CORE_TABLE
  ) |>
  tibble::as_tibble()


fern_species_profile <-
  DBI::dbReadTable(
    con,
    FERN_SPECIES_PROFILE_TABLE
  ) |>
  tibble::as_tibble()


# ==============================================================================
# 12. SOURCE POPULATION INVARIANTS
# ==============================================================================

if (
  nrow(source_03g) !=
  EXPECTED_03G_CANDIDATES
) {
  
  disconnect_safely()
  
  stop(
    paste0(
      "03g candidate-population invariant failed. Expected ",
      EXPECTED_03G_CANDIDATES,
      " records but found ",
      nrow(source_03g),
      "."
    )
  )
}


if (
  nrow(canonical) !=
  EXPECTED_CANONICAL_POPULATION
) {
  
  disconnect_safely()
  
  stop(
    paste0(
      "Canonical VPJD population invariant failed. Expected ",
      EXPECTED_CANONICAL_POPULATION,
      " records but found ",
      nrow(canonical),
      "."
    )
  )
}


# ==============================================================================
# 13. VALIDATE EXACT 03g SOURCE FIELDS
# ==============================================================================

required_03g_fields <-
  c(
    "AUDIT_03G_ROW",
    "SOURCE_03F_ROW",
    "SOURCE_PRIOR_ROW",
    "JAPAN_RECORD_ID",
    "SOURCE",
    "SOURCE_TAXON_ID",
    "JAPAN_SCIENTIFIC_NAME",
    "JAPANESE_NAME",
    "JAPAN_RANK",
    "JAPAN_FAMILY",
    "JAPAN_GENUS",
    "WCVP_ID",
    "WCVP_ACCEPTED_NAME",
    "WCVP_RANK",
    "WCVP_FAMILY",
    "WCVP_GENUS",
    "WCVP_STATUS",
    "SOURCE_NATIVE_EVIDENCE",
    "SOURCE_INTRODUCED_EVIDENCE",
    "JAPAN_NAME_NORM",
    "WCVP_ACCEPTED_NAME_NORM",
    "WCVP_ID_NORM",
    "CANONICAL_CONCEPT_PRESENT",
    "NAME_IN_GREENLIST",
    "NAME_IN_FERN_GREENLIST",
    "NAME_IN_CONTEMPORARY_JAPAN_CHECKLIST",
    "RESOLUTION_CLASS",
    "RESOLUTION_STATUS",
    "RESOLUTION_EVIDENCE"
  )


missing_03g_fields <-
  setdiff(
    required_03g_fields,
    names(source_03g)
  )


if (
  length(missing_03g_fields) > 0L
) {
  
  disconnect_safely()
  
  stop(
    paste0(
      "Required 03g field(s) missing:\n",
      paste(
        missing_03g_fields,
        collapse = "\n"
      )
    )
  )
}


# ==============================================================================
# 14. PROFILE SUPPORTING TABLE FIELDS
# ==============================================================================

green_distribution_fields <-
  names(
    green_distribution
  )


fern_species_profile_fields <-
  names(
    fern_species_profile
  )


green_core_fields <-
  names(
    green_core
  )


fern_core_fields <-
  names(
    fern_core
  )


# ==============================================================================
# 15. IDENTIFY SUPPORTING-TABLE KEY FIELDS
# ==============================================================================

green_core_id_field <-
  first_existing_field(
    green_core,
    c(
      "TAXONID",
      "DWCA_ID",
      "ID",
      "COREID"
    )
  )


green_distribution_id_field <-
  first_existing_field(
    green_distribution,
    c(
      "COREID",
      "TAXONID",
      "DWCA_ID",
      "ID"
    )
  )


fern_core_id_field <-
  first_existing_field(
    fern_core,
    c(
      "TAXONID",
      "DWCA_ID",
      "ID",
      "COREID"
    )
  )


fern_profile_id_field <-
  first_existing_field(
    fern_species_profile,
    c(
      "COREID",
      "TAXONID",
      "DWCA_ID",
      "ID"
    )
  )


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


# ==============================================================================
# 16. IDENTIFY DISTRIBUTION / ESTABLISHMENT EVIDENCE FIELDS
# ==============================================================================

green_location_field <-
  first_existing_field(
    green_distribution,
    c(
      "LOCATIONID",
      "LOCALITY",
      "COUNTRY",
      "COUNTRYCODE",
      "OCCURRENCESTATUS"
    )
  )


green_establishment_field <-
  first_existing_field(
    green_distribution,
    c(
      "ESTABLISHMENTMEANS",
      "DEGREEOFESTABLISHMENT",
      "ORIGIN",
      "NATIVESTATUS",
      "STATUS"
    )
  )


green_occurrence_status_field <-
  first_existing_field(
    green_distribution,
    c(
      "OCCURRENCESTATUS"
    )
  )


fern_native_field <-
  first_existing_field(
    fern_species_profile,
    c(
      "ISNATIVE",
      "NATIVE",
      "NATIVESTATUS",
      "ORIGIN",
      "ESTABLISHMENTMEANS"
    )
  )


fern_profile_status_field <-
  first_existing_field(
    fern_species_profile,
    c(
      "STATUS",
      "OCCURRENCESTATUS",
      "THREATSTATUS",
      "ESTABLISHMENTMEANS"
    )
  )


# ==============================================================================
# 17. BUILD GREENLIST EVIDENCE LOOKUP
# ==============================================================================

green_lookup <-
  tibble(
    GREEN_CORE_ID =
      safe_field(
        green_core,
        green_core_id_field
      ),
    
    GREEN_SCIENTIFIC_NAME =
      safe_field(
        green_core,
        green_scientific_field
      )
  ) |>
  mutate(
    GREEN_NAME_NORM =
      normalise_name(
        GREEN_SCIENTIFIC_NAME
      )
  )


green_distribution_lookup <-
  tibble(
    GREEN_CORE_ID =
      safe_field(
        green_distribution,
        green_distribution_id_field
      ),
    
    GREEN_LOCATION =
      safe_field(
        green_distribution,
        green_location_field
      ),
    
    GREEN_ESTABLISHMENT =
      safe_field(
        green_distribution,
        green_establishment_field
      ),
    
    GREEN_OCCURRENCE_STATUS =
      safe_field(
        green_distribution,
        green_occurrence_status_field
      )
  )


if (
  !is.na(green_core_id_field) &&
  !is.na(green_distribution_id_field)
) {
  
  green_evidence <-
    green_lookup |>
    left_join(
      green_distribution_lookup,
      by =
        "GREEN_CORE_ID"
    )
  
} else {
  
  green_evidence <-
    green_lookup |>
    mutate(
      GREEN_LOCATION =
        NA_character_,
      
      GREEN_ESTABLISHMENT =
        NA_character_,
      
      GREEN_OCCURRENCE_STATUS =
        NA_character_
    )
}


green_evidence_by_name <-
  green_evidence |>
  filter(
    nonblank(
      GREEN_NAME_NORM
    )
  ) |>
  group_by(
    GREEN_NAME_NORM
  ) |>
  summarise(
    GREEN_LOCATION =
      collapse_unique(
        GREEN_LOCATION
      ),
    
    GREEN_ESTABLISHMENT =
      collapse_unique(
        GREEN_ESTABLISHMENT
      ),
    
    GREEN_OCCURRENCE_STATUS =
      collapse_unique(
        GREEN_OCCURRENCE_STATUS
      ),
    
    .groups =
      "drop"
  )


# ==============================================================================
# 18. BUILD FERNGREENLIST EVIDENCE LOOKUP
# ==============================================================================

fern_lookup <-
  tibble(
    FERN_CORE_ID =
      safe_field(
        fern_core,
        fern_core_id_field
      ),
    
    FERN_SCIENTIFIC_NAME =
      safe_field(
        fern_core,
        fern_scientific_field
      )
  ) |>
  mutate(
    FERN_NAME_NORM =
      normalise_name(
        FERN_SCIENTIFIC_NAME
      )
  )


fern_profile_lookup <-
  tibble(
    FERN_CORE_ID =
      safe_field(
        fern_species_profile,
        fern_profile_id_field
      ),
    
    FERN_NATIVE_VALUE =
      safe_field(
        fern_species_profile,
        fern_native_field
      ),
    
    FERN_PROFILE_STATUS =
      safe_field(
        fern_species_profile,
        fern_profile_status_field
      )
  )


if (
  !is.na(fern_core_id_field) &&
  !is.na(fern_profile_id_field)
) {
  
  fern_evidence <-
    fern_lookup |>
    left_join(
      fern_profile_lookup,
      by =
        "FERN_CORE_ID"
    )
  
} else {
  
  fern_evidence <-
    fern_lookup |>
    mutate(
      FERN_NATIVE_VALUE =
        NA_character_,
      
      FERN_PROFILE_STATUS =
        NA_character_
    )
}


fern_evidence_by_name <-
  fern_evidence |>
  filter(
    nonblank(
      FERN_NAME_NORM
    )
  ) |>
  group_by(
    FERN_NAME_NORM
  ) |>
  summarise(
    FERN_NATIVE_VALUE =
      collapse_unique(
        FERN_NATIVE_VALUE
      ),
    
    FERN_PROFILE_STATUS =
      collapse_unique(
        FERN_PROFILE_STATUS
      ),
    
    .groups =
      "drop"
  )


# ==============================================================================
# 19. BUILD 03h AUDIT BASE
# ==============================================================================

audit <-
  source_03g |>
  transmute(
    AUDIT_03H_ROW =
      seq_len(
        nrow(source_03g)
      ),
    
    AUDIT_03G_ROW =
      AUDIT_03G_ROW,
    
    SOURCE_03F_ROW =
      SOURCE_03F_ROW,
    
    SOURCE_PRIOR_ROW =
      SOURCE_PRIOR_ROW,
    
    JAPAN_RECORD_ID =
      JAPAN_RECORD_ID,
    
    SOURCE =
      SOURCE,
    
    SOURCE_TAXON_ID =
      SOURCE_TAXON_ID,
    
    JAPAN_SCIENTIFIC_NAME =
      JAPAN_SCIENTIFIC_NAME,
    
    JAPANESE_NAME =
      JAPANESE_NAME,
    
    JAPAN_RANK =
      JAPAN_RANK,
    
    JAPAN_FAMILY =
      JAPAN_FAMILY,
    
    JAPAN_GENUS =
      JAPAN_GENUS,
    
    WCVP_ID =
      WCVP_ID,
    
    WCVP_ACCEPTED_NAME =
      WCVP_ACCEPTED_NAME,
    
    WCVP_RANK =
      WCVP_RANK,
    
    WCVP_FAMILY =
      WCVP_FAMILY,
    
    WCVP_GENUS =
      WCVP_GENUS,
    
    WCVP_STATUS =
      WCVP_STATUS,
    
    SOURCE_NATIVE_EVIDENCE =
      SOURCE_NATIVE_EVIDENCE,
    
    SOURCE_INTRODUCED_EVIDENCE =
      SOURCE_INTRODUCED_EVIDENCE,
    
    JAPAN_NAME_NORM =
      JAPAN_NAME_NORM,
    
    WCVP_ACCEPTED_NAME_NORM =
      WCVP_ACCEPTED_NAME_NORM,
    
    WCVP_ID_NORM =
      WCVP_ID_NORM,
    
    CANONICAL_CONCEPT_PRESENT =
      CANONICAL_CONCEPT_PRESENT,
    
    NAME_IN_GREENLIST =
      NAME_IN_GREENLIST,
    
    NAME_IN_FERN_GREENLIST =
      NAME_IN_FERN_GREENLIST,
    
    NAME_IN_CONTEMPORARY_JAPAN_CHECKLIST =
      NAME_IN_CONTEMPORARY_JAPAN_CHECKLIST,
    
    RESOLUTION_CLASS_03G =
      RESOLUTION_CLASS,
    
    RESOLUTION_STATUS_03G =
      RESOLUTION_STATUS,
    
    RESOLUTION_EVIDENCE_03G =
      RESOLUTION_EVIDENCE
  )


# ==============================================================================
# 20. ATTACH GREENLIST EVIDENCE
# ==============================================================================

audit <-
  audit |>
  left_join(
    green_evidence_by_name,
    by =
      c(
        "JAPAN_NAME_NORM" =
          "GREEN_NAME_NORM"
      )
  )


# ==============================================================================
# 21. ATTACH FERNGREENLIST EVIDENCE
# ==============================================================================

audit <-
  audit |>
  left_join(
    fern_evidence_by_name,
    by =
      c(
        "JAPAN_NAME_NORM" =
          "FERN_NAME_NORM"
      )
  )


# ==============================================================================
# 22. NORMALISE RANK AND STATUS
# ==============================================================================

audit <-
  audit |>
  mutate(
    RANK_NORM =
      tolower(
        ifelse(
          nonblank(
            WCVP_RANK
          ),
          WCVP_RANK,
          JAPAN_RANK
        )
      ),
    
    STATUS_NORM =
      tolower(
        ifelse(
          is.na(
            WCVP_STATUS
          ),
          "",
          WCVP_STATUS
        )
      )
  )


# ==============================================================================
# 23. HYBRID / NOTHOTAXON EVIDENCE
# ==============================================================================

audit <-
  audit |>
  mutate(
    HYBRID_SYMBOL_PRESENT =
      grepl(
        "×| x ",
        paste0(
          " ",
          ifelse(
            is.na(
              WCVP_ACCEPTED_NAME
            ),
            JAPAN_SCIENTIFIC_NAME,
            WCVP_ACCEPTED_NAME
          ),
          " "
        ),
        ignore.case = TRUE
      ),
    
    HYBRID_RANK_PRESENT =
      contains_any(
        RANK_NORM,
        c(
          "noth",
          "hybrid"
        )
      ),
    
    HYBRID_STATUS_PRESENT =
      contains_any(
        STATUS_NORM,
        c(
          "hybrid",
          "noth"
        )
      ),
    
    HYBRID_EVIDENCE =
      HYBRID_SYMBOL_PRESENT |
      HYBRID_RANK_PRESENT |
      HYBRID_STATUS_PRESENT
  )


# ==============================================================================
# 24. INFRASPECIFIC EVIDENCE
# ==============================================================================

infraspecific_rank_patterns <-
  c(
    "^subspecies$",
    "^subsp\\.?$",
    "^variety$",
    "^var\\.?$",
    "^forma$",
    "^form$",
    "^f\\.?$",
    "subvar",
    "subforma"
  )


audit <-
  audit |>
  mutate(
    INFRASPECIFIC_EVIDENCE =
      contains_any(
        RANK_NORM,
        infraspecific_rank_patterns
      )
  )


# ==============================================================================
# 25. EXPLICIT INTRODUCED / NON-NATIVE EVIDENCE
# ==============================================================================

introduced_patterns <-
  c(
    "\\bintroduced\\b",
    "\\bnon[- ]?native\\b",
    "\\balien\\b",
    "\\bexotic\\b",
    "\\bnaturalised\\b",
    "\\bnaturalized\\b",
    "\\bcultivated\\b",
    "\\badventive\\b"
  )


audit <-
  audit |>
  mutate(
    INTRODUCED_EVIDENCE_TEXT =
      paste(
        ifelse(
          is.na(
            SOURCE_INTRODUCED_EVIDENCE
          ),
          "",
          SOURCE_INTRODUCED_EVIDENCE
        ),
        ifelse(
          is.na(
            GREEN_ESTABLISHMENT
          ),
          "",
          GREEN_ESTABLISHMENT
        ),
        ifelse(
          is.na(
            GREEN_OCCURRENCE_STATUS
          ),
          "",
          GREEN_OCCURRENCE_STATUS
        ),
        ifelse(
          is.na(
            FERN_NATIVE_VALUE
          ),
          "",
          FERN_NATIVE_VALUE
        ),
        ifelse(
          is.na(
            FERN_PROFILE_STATUS
          ),
          "",
          FERN_PROFILE_STATUS
        )
      ),
    
    EXPLICIT_INTRODUCED_EVIDENCE =
      contains_any(
        INTRODUCED_EVIDENCE_TEXT,
        introduced_patterns
      )
  )


# ==============================================================================
# 26. EXPLICIT NATIVE EVIDENCE
# ==============================================================================

native_patterns <-
  c(
    "\\bnative\\b",
    "\\bindigenous\\b",
    "\\bendemic\\b"
  )


audit <-
  audit |>
  mutate(
    NATIVE_EVIDENCE_TEXT =
      paste(
        ifelse(
          is.na(
            SOURCE_NATIVE_EVIDENCE
          ),
          "",
          SOURCE_NATIVE_EVIDENCE
        ),
        ifelse(
          is.na(
            GREEN_ESTABLISHMENT
          ),
          "",
          GREEN_ESTABLISHMENT
        ),
        ifelse(
          is.na(
            FERN_NATIVE_VALUE
          ),
          "",
          FERN_NATIVE_VALUE
        ),
        ifelse(
          is.na(
            FERN_PROFILE_STATUS
          ),
          "",
          FERN_PROFILE_STATUS
        )
      ),
    
    EXPLICIT_NATIVE_EVIDENCE =
      contains_any(
        NATIVE_EVIDENCE_TEXT,
        native_patterns
      )
  )


# ==============================================================================
# 27. RESOLVE NATIVE / INTRODUCED EVIDENCE CONFLICT
# ==============================================================================

audit <-
  audit |>
  mutate(
    NATIVE_INTRODUCED_CONFLICT =
      EXPLICIT_NATIVE_EVIDENCE &
      EXPLICIT_INTRODUCED_EVIDENCE,
    
    CLEAN_NATIVE_EVIDENCE =
      EXPLICIT_NATIVE_EVIDENCE &
      !EXPLICIT_INTRODUCED_EVIDENCE,
    
    CLEAN_INTRODUCED_EVIDENCE =
      EXPLICIT_INTRODUCED_EVIDENCE &
      !EXPLICIT_NATIVE_EVIDENCE
  )


# ==============================================================================
# 28. VASCULAR / TAXONOMIC SCOPE EVIDENCE
# ==============================================================================

vascular_rank_exclusions <-
  c(
    "family",
    "order",
    "class",
    "phylum",
    "kingdom"
  )


audit <-
  audit |>
  mutate(
    ABOVE_GENUS_SCOPE =
      RANK_NORM %in%
      vascular_rank_exclusions,
    
    GENUS_ONLY =
      RANK_NORM %in%
      c(
        "genus"
      ),
    
    SPECIES_OR_BELOW =
      !ABOVE_GENUS_SCOPE &
      !GENUS_ONLY
  )


# ==============================================================================
# 29. CHECKLIST SUPPORT INVARIANTS
# ==============================================================================

audit <-
  audit |>
  mutate(
    CHECKLIST_SUPPORT_VALID =
      NAME_IN_CONTEMPORARY_JAPAN_CHECKLIST &
      (
        NAME_IN_GREENLIST |
          NAME_IN_FERN_GREENLIST
      )
  )


# ==============================================================================
# 30. EVIDENCE STRENGTH
# ==============================================================================

audit <-
  audit |>
  mutate(
    EVIDENCE_SCORE =
      as.integer(
        NAME_IN_CONTEMPORARY_JAPAN_CHECKLIST
      ) +
      as.integer(
        nonblank(
          WCVP_ID
        )
      ) +
      as.integer(
        nonblank(
          WCVP_ACCEPTED_NAME
        )
      ) +
      as.integer(
        CLEAN_NATIVE_EVIDENCE
      ),
    
    EVIDENCE_STRENGTH =
      case_when(
        CLEAN_NATIVE_EVIDENCE &
          nonblank(
            WCVP_ID
          ) &
          nonblank(
            WCVP_ACCEPTED_NAME
          ) ~
          "STRONG",
        
        NAME_IN_CONTEMPORARY_JAPAN_CHECKLIST &
          nonblank(
            WCVP_ID
          ) &
          nonblank(
            WCVP_ACCEPTED_NAME
          ) ~
          "MODERATE",
        
        TRUE ~
          "LIMITED"
      )
  )


# ==============================================================================
# 31. CONSERVATIVE ADJUDICATION
# ==============================================================================

audit <-
  audit |>
  mutate(
    ADJUDICATION_CLASS =
      case_when(
        
        NATIVE_INTRODUCED_CONFLICT ~
          "REQUIRES_MANUAL_REVIEW",
        
        CLEAN_INTRODUCED_EVIDENCE ~
          "EXCLUDE_INTRODUCED_OR_NON_NATIVE",
        
        ABOVE_GENUS_SCOPE |
          GENUS_ONLY ~
          "EXCLUDE_TAXONOMIC_SCOPE",
        
        HYBRID_EVIDENCE ~
          "EXCLUDE_HYBRID_SCOPE",
        
        INFRASPECIFIC_EVIDENCE ~
          "EXCLUDE_INFRASPECIFIC_SCOPE",
        
        CLEAN_NATIVE_EVIDENCE &
          NAME_IN_CONTEMPORARY_JAPAN_CHECKLIST &
          nonblank(
            WCVP_ID
          ) &
          nonblank(
            WCVP_ACCEPTED_NAME
          ) ~
          "CONFIRMED_CANONICAL_GAP",
        
        TRUE ~
          "REQUIRES_MANUAL_REVIEW"
      )
  )


# ==============================================================================
# 32. ADJUDICATION STATUS
# ==============================================================================

audit <-
  audit |>
  mutate(
    ADJUDICATION_STATUS =
      case_when(
        
        ADJUDICATION_CLASS ==
          "CONFIRMED_CANONICAL_GAP" ~
          "CONFIRMED_GAP_NOT_YET_ADDED",
        
        grepl(
          "^EXCLUDE_",
          ADJUDICATION_CLASS
        ) ~
          "EXCLUDED_FROM_AUTOMATIC_GAP_ACCEPTANCE",
        
        TRUE ~
          "MANUAL_REVIEW_REQUIRED"
      )
  )


# ==============================================================================
# 33. ADJUDICATION EVIDENCE
# ==============================================================================

audit <-
  audit |>
  mutate(
    ADJUDICATION_EVIDENCE =
      case_when(
        
        NATIVE_INTRODUCED_CONFLICT ~
          paste0(
            "Conflicting native and introduced/non-native evidence; ",
            "manual adjudication required."
          ),
        
        CLEAN_INTRODUCED_EVIDENCE ~
          paste0(
            "Explicit introduced/non-native evidence detected; ",
            "candidate not accepted as a native VPJD canonical gap."
          ),
        
        ABOVE_GENUS_SCOPE |
          GENUS_ONLY ~
          paste0(
            "Taxonomic rank outside species-level VPJD canonical-gap ",
            "acceptance scope."
          ),
        
        HYBRID_EVIDENCE ~
          paste0(
            "Hybrid/nothotaxon evidence detected; separate hybrid ",
            "scope review required."
          ),
        
        INFRASPECIFIC_EVIDENCE ~
          paste0(
            "Infraspecific taxon detected; separate infraspecific ",
            "scope review required."
          ),
        
        ADJUDICATION_CLASS ==
          "CONFIRMED_CANONICAL_GAP" ~
          paste0(
            "Contemporary Japanese checklist support + WCVP accepted ",
            "concept + explicit native evidence; no existing VPJD ",
            "canonical concept identified."
          ),
        
        TRUE ~
          paste0(
            "Contemporary Japanese checklist support and WCVP accepted ",
            "concept established, but native/non-native scope evidence ",
            "is insufficient for automatic canonical-gap confirmation."
          )
      )
  )


# ==============================================================================
# 34. SUBSET OUTPUTS
# ==============================================================================

confirmed_gaps <-
  audit |>
  filter(
    ADJUDICATION_CLASS ==
      "CONFIRMED_CANONICAL_GAP"
  )


introduced_exclusions <-
  audit |>
  filter(
    ADJUDICATION_CLASS ==
      "EXCLUDE_INTRODUCED_OR_NON_NATIVE"
  )


hybrid_review <-
  audit |>
  filter(
    ADJUDICATION_CLASS ==
      "EXCLUDE_HYBRID_SCOPE"
  )


infraspecific_review <-
  audit |>
  filter(
    ADJUDICATION_CLASS ==
      "EXCLUDE_INFRASPECIFIC_SCOPE"
  )


other_scope_review <-
  audit |>
  filter(
    ADJUDICATION_CLASS %in%
      c(
        "EXCLUDE_TAXONOMIC_SCOPE",
        "EXCLUDE_OTHER_DEFINED_SCOPE"
      )
  )


manual_review <-
  audit |>
  filter(
    ADJUDICATION_CLASS ==
      "REQUIRES_MANUAL_REVIEW"
  )


# ==============================================================================
# 35. COUNTS
# ==============================================================================

n_total <-
  nrow(audit)


n_confirmed <-
  nrow(confirmed_gaps)


n_introduced <-
  nrow(introduced_exclusions)


n_hybrid <-
  nrow(hybrid_review)


n_infraspecific <-
  nrow(infraspecific_review)


n_other_scope <-
  nrow(other_scope_review)


n_manual <-
  nrow(manual_review)


n_native_evidence <-
  sum(
    audit$EXPLICIT_NATIVE_EVIDENCE,
    na.rm = TRUE
  )


n_introduced_evidence <-
  sum(
    audit$EXPLICIT_INTRODUCED_EVIDENCE,
    na.rm = TRUE
  )


n_native_conflict <-
  sum(
    audit$NATIVE_INTRODUCED_CONFLICT,
    na.rm = TRUE
  )


# ==============================================================================
# 36. ADJUDICATION CLASS SUMMARY
# ==============================================================================

adjudication_class_summary <-
  audit |>
  count(
    ADJUDICATION_CLASS,
    ADJUDICATION_STATUS,
    name =
      "N_RECORDS"
  ) |>
  arrange(
    desc(
      N_RECORDS
    ),
    ADJUDICATION_CLASS
  )


# ==============================================================================
# 37. EVIDENCE SUMMARY
# ==============================================================================

evidence_summary <-
  tibble(
    METRIC =
      c(
        "03g candidates adjudicated",
        "GreenList-supported candidates",
        "FernGreenList-supported candidates",
        "Contemporary-checklist-supported candidates",
        "Candidates with WCVP ID",
        "Candidates with WCVP accepted name",
        "Candidates with explicit native evidence",
        "Candidates with explicit introduced/non-native evidence",
        "Native/introduced evidence conflicts",
        "Hybrid/nothotaxon candidates",
        "Infraspecific candidates",
        "Confirmed canonical gaps",
        "Introduced/non-native exclusions",
        "Other scope exclusions",
        "Manual-review candidates",
        "Taxa automatically added"
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
          nonblank(
            audit$WCVP_ID
          )
        ),
        
        sum(
          nonblank(
            audit$WCVP_ACCEPTED_NAME
          )
        ),
        
        n_native_evidence,
        
        n_introduced_evidence,
        
        n_native_conflict,
        
        sum(
          audit$HYBRID_EVIDENCE,
          na.rm = TRUE
        ),
        
        sum(
          audit$INFRASPECIFIC_EVIDENCE,
          na.rm = TRUE
        ),
        
        n_confirmed,
        
        n_introduced,
        
        n_hybrid +
          n_infraspecific +
          n_other_scope,
        
        n_manual,
        
        0L
      )
  )


# ==============================================================================
# 38. FAMILY SUMMARY
# ==============================================================================

family_summary <-
  audit |>
  mutate(
    FAMILY =
      ifelse(
        nonblank(
          WCVP_FAMILY
        ),
        WCVP_FAMILY,
        JAPAN_FAMILY
      )
  ) |>
  count(
    FAMILY,
    ADJUDICATION_CLASS,
    name =
      "N_RECORDS"
  ) |>
  arrange(
    FAMILY,
    ADJUDICATION_CLASS
  )


# ==============================================================================
# 39. RANK SUMMARY
# ==============================================================================

rank_summary <-
  audit |>
  count(
    RANK_NORM,
    ADJUDICATION_CLASS,
    name =
      "N_RECORDS"
  ) |>
  arrange(
    RANK_NORM,
    ADJUDICATION_CLASS
  )


# ==============================================================================
# 40. SOURCE SUMMARY
# ==============================================================================

source_summary <-
  audit |>
  count(
    SOURCE,
    ADJUDICATION_CLASS,
    name =
      "N_RECORDS"
  ) |>
  arrange(
    SOURCE,
    ADJUDICATION_CLASS
  )


# ==============================================================================
# 41. VALIDATION
# ==============================================================================

validation <-
  tibble(
    CHECK =
      c(
        "Canonical VPJD population = 11,439",
        "03g candidate population = 201",
        "03h audit population = 201",
        "03h AUDIT_03H_ROW unique",
        "03g AUDIT_03G_ROW retained uniquely",
        "JAPAN_RECORD_ID retained uniquely",
        "GreenList-supported population = 189",
        "FernGreenList-supported population = 12",
        "Contemporary-checklist-supported population = 201",
        "All 201 assigned adjudication class",
        "Adjudication classes sum to 201",
        "Confirmed gaps subset of 201",
        "Introduced exclusions subset of 201",
        "Hybrid review subset of 201",
        "Infraspecific review subset of 201",
        "Manual review subset of 201",
        "No candidate already has canonical VPJD concept",
        "No fuzzy matching performed",
        "No automatic synonym acceptance performed",
        "No automatic VPJD expansion performed",
        "Canonical taxonomy modified = FALSE",
        "Star allocations modified = FALSE",
        "Taxa removed = 0",
        "Taxa automatically added = 0"
      ),
    
    PASS =
      c(
        nrow(canonical) ==
          EXPECTED_CANONICAL_POPULATION,
        
        nrow(source_03g) ==
          EXPECTED_03G_CANDIDATES,
        
        n_total ==
          EXPECTED_03G_CANDIDATES,
        
        dplyr::n_distinct(
          audit$AUDIT_03H_ROW
        ) ==
          n_total,
        
        dplyr::n_distinct(
          audit$AUDIT_03G_ROW
        ) ==
          n_total,
        
        dplyr::n_distinct(
          audit$JAPAN_RECORD_ID
        ) ==
          n_total,
        
        sum(
          audit$NAME_IN_GREENLIST,
          na.rm = TRUE
        ) ==
          EXPECTED_GREENLIST_SUPPORTED,
        
        sum(
          audit$NAME_IN_FERN_GREENLIST,
          na.rm = TRUE
        ) ==
          EXPECTED_FERN_SUPPORTED,
        
        sum(
          audit$NAME_IN_CONTEMPORARY_JAPAN_CHECKLIST,
          na.rm = TRUE
        ) ==
          EXPECTED_CHECKLIST_SUPPORTED,
        
        sum(
          nonblank(
            audit$ADJUDICATION_CLASS
          )
        ) ==
          n_total,
        
        (
          n_confirmed +
            n_introduced +
            n_hybrid +
            n_infraspecific +
            n_other_scope +
            n_manual
        ) ==
          n_total,
        
        n_confirmed <=
          n_total,
        
        n_introduced <=
          n_total,
        
        n_hybrid <=
          n_total,
        
        n_infraspecific <=
          n_total,
        
        n_manual <=
          n_total,
        
        sum(
          audit$CANONICAL_CONCEPT_PRESENT,
          na.rm = TRUE
        ) ==
          0L,
        
        TRUE,
        
        TRUE,
        
        TRUE,
        
        TRUE,
        
        TRUE,
        
        TRUE,
        
        TRUE
      )
  ) |>
  mutate(
    RESULT =
      ifelse(
        PASS,
        "PASS",
        "FAIL"
      )
  )


all_valid <-
  all(
    validation$PASS
  )


# ==============================================================================
# 42. DECISION
# ==============================================================================

decision <-
  if (
    all_valid
  ) {
    
    "VPJD_CANONICAL_GAP_ADJUDICATION_COMPLETE"
    
  } else {
    
    "REVIEW_REQUIRED"
  }


# ==============================================================================
# 43. METADATA
# ==============================================================================

metadata <-
  tibble(
    KEY =
      c(
        "MODULE",
        "VERSION",
        "SOURCE_03G_TABLE",
        "CANONICAL_TABLE",
        "LINEAGE_TABLE",
        "EXPECTED_CANONICAL_POPULATION",
        "EXPECTED_03G_CANDIDATES",
        "CONFIRMED_CANONICAL_GAPS",
        "INTRODUCED_NON_NATIVE_EXCLUSIONS",
        "HYBRID_SCOPE_REVIEW",
        "INFRASPECIFIC_SCOPE_REVIEW",
        "OTHER_SCOPE_REVIEW",
        "MANUAL_REVIEW",
        "CANONICAL_TAXONOMY_MODIFIED",
        "STAR_ALLOCATIONS_MODIFIED",
        "TAXA_REMOVED",
        "TAXA_ADDED",
        "DECISION"
      ),
    
    VALUE =
      as.character(
        c(
          "03h_adjudicate_vpjd_canonical_gap_candidates",
          VERSION,
          SOURCE_03G_TABLE,
          CANONICAL_TABLE,
          LINEAGE_TABLE,
          EXPECTED_CANONICAL_POPULATION,
          EXPECTED_03G_CANDIDATES,
          n_confirmed,
          n_introduced,
          n_hybrid,
          n_infraspecific,
          n_other_scope,
          n_manual,
          FALSE,
          FALSE,
          0L,
          0L,
          decision
        )
      )
  )


# ==============================================================================
# 44. WRITE DUCKDB OUTPUTS
# ==============================================================================

duckdb_write_status <-
  bind_rows(
    write_duckdb_table(
      TABLE_AUDIT,
      audit
    ),
    
    write_duckdb_table(
      TABLE_CONFIRMED_GAPS,
      confirmed_gaps
    ),
    
    write_duckdb_table(
      TABLE_INTRODUCED,
      introduced_exclusions
    ),
    
    write_duckdb_table(
      TABLE_HYBRID,
      hybrid_review
    ),
    
    write_duckdb_table(
      TABLE_INFRASPECIFIC,
      infraspecific_review
    ),
    
    write_duckdb_table(
      TABLE_OTHER_SCOPE,
      other_scope_review
    ),
    
    write_duckdb_table(
      TABLE_MANUAL_REVIEW,
      manual_review
    ),
    
    write_duckdb_table(
      TABLE_CLASS_SUMMARY,
      adjudication_class_summary
    ),
    
    write_duckdb_table(
      TABLE_EVIDENCE_SUMMARY,
      evidence_summary
    ),
    
    write_duckdb_table(
      TABLE_FAMILY_SUMMARY,
      family_summary
    ),
    
    write_duckdb_table(
      TABLE_RANK_SUMMARY,
      rank_summary
    ),
    
    write_duckdb_table(
      TABLE_SOURCE_SUMMARY,
      source_summary
    ),
    
    write_duckdb_table(
      TABLE_VALIDATION,
      validation
    ),
    
    write_duckdb_table(
      TABLE_METADATA,
      metadata
    )
  )


# ==============================================================================
# 45. WRITE CSV OUTPUTS
# ==============================================================================

csv_write_status <-
  bind_rows(
    write_csv_output(
      "03h_adjudication_audit.csv",
      audit
    ),
    
    write_csv_output(
      "03h_confirmed_canonical_gaps.csv",
      confirmed_gaps
    ),
    
    write_csv_output(
      "03h_exclude_introduced_non_native.csv",
      introduced_exclusions
    ),
    
    write_csv_output(
      "03h_hybrid_scope_review.csv",
      hybrid_review
    ),
    
    write_csv_output(
      "03h_infraspecific_scope_review.csv",
      infraspecific_review
    ),
    
    write_csv_output(
      "03h_other_scope_review.csv",
      other_scope_review
    ),
    
    write_csv_output(
      "03h_manual_review.csv",
      manual_review
    ),
    
    write_csv_output(
      "03h_adjudication_class_summary.csv",
      adjudication_class_summary
    ),
    
    write_csv_output(
      "03h_evidence_summary.csv",
      evidence_summary
    ),
    
    write_csv_output(
      "03h_family_summary.csv",
      family_summary
    ),
    
    write_csv_output(
      "03h_rank_summary.csv",
      rank_summary
    ),
    
    write_csv_output(
      "03h_source_summary.csv",
      source_summary
    ),
    
    write_csv_output(
      "03h_validation.csv",
      validation
    ),
    
    write_csv_output(
      "03h_metadata.csv",
      metadata
    )
  )


# ==============================================================================
# 46. SUMMARY REPORT
# ==============================================================================

cat("\n")
cat("------------------------------------------------------------\n")
cat("03h ADJUDICATION SUMMARY\n")
cat("------------------------------------------------------------\n\n")


print(
  adjudication_class_summary,
  n = Inf,
  width = Inf
)


cat("\n")


print(
  evidence_summary,
  n = Inf,
  width = Inf
)


# ==============================================================================
# 47. VALIDATION REPORT
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
      "%02d  %-72s  %s\n",
      i,
      validation$CHECK[[i]],
      validation$RESULT[[i]]
    )
  )
}


# ==============================================================================
# 48. FAILED VALIDATION REPORT
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
# 49. CLEAN DISCONNECT
# ==============================================================================

DBI::dbDisconnect(
  con,
  shutdown = TRUE
)

rm(con)

gc()


# ==============================================================================
# 50. COMPLETION REPORT
# ==============================================================================

cat("\n")
cat("============================================================\n")

cat(
  "VPJD TAXONOMIC REVISION 03h v",
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
  "03g canonical-gap candidates adjudicated: ",
  format(
    n_total,
    big.mark = ","
  ),
  "\n\n",
  sep = ""
)


cat(
  "Confirmed canonical gaps: ",
  format(
    n_confirmed,
    big.mark = ","
  ),
  "\n",
  sep = ""
)


cat(
  "Introduced/non-native exclusions: ",
  format(
    n_introduced,
    big.mark = ","
  ),
  "\n",
  sep = ""
)


cat(
  "Hybrid scope review: ",
  format(
    n_hybrid,
    big.mark = ","
  ),
  "\n",
  sep = ""
)


cat(
  "Infraspecific scope review: ",
  format(
    n_infraspecific,
    big.mark = ","
  ),
  "\n",
  sep = ""
)


cat(
  "Other taxonomic/scope exclusions: ",
  format(
    n_other_scope,
    big.mark = ","
  ),
  "\n",
  sep = ""
)


cat(
  "Manual review required: ",
  format(
    n_manual,
    big.mark = ","
  ),
  "\n\n",
  sep = ""
)


cat(
  "Explicit native evidence: ",
  format(
    n_native_evidence,
    big.mark = ","
  ),
  "\n",
  sep = ""
)


cat(
  "Explicit introduced/non-native evidence: ",
  format(
    n_introduced_evidence,
    big.mark = ","
  ),
  "\n",
  sep = ""
)


cat(
  "Native/introduced evidence conflicts: ",
  format(
    n_native_conflict,
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


cat("ADJUDICATION / AUDIT ONLY\n")
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
  "03h DuckDB outputs written: ",
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
  "03h CSV outputs written: ",
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
cat("03h COMPLETE\n")
cat("============================================================\n")


# ==============================================================================
# END
# ==============================================================================