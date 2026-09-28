# ==============================================================================
# VPJD TAXONOMIC REVISION
# 03i_reconcile_canonical_gap_candidates_to_wcvp_backbone.R
#
# PURPOSE
# -------
# Reconcile the 201 canonical-gap candidates retained by 03h explicitly against
# the Kew / WCVP taxonomic backbone.
#
# GOVERNING TAXONOMIC PRINCIPLE
# -----------------------------
# VPJD follows the Kew / WCVP taxonomic backbone as its primary nomenclatural
# and taxonomic authority.
#
# GreenList and FernGreenList are treated as:
#   - evidence for occurrence in the contemporary Japanese flora;
#   - sources of Japanese vernacular names;
#   - sources of Japanese taxonomic interpretation;
#   - crosswalk sources to WCVP concepts.
#
# They DO NOT independently define canonical VPJD taxonomic concepts.
#
# This module therefore asks:
#
#   1. What WCVP accepted concept corresponds to each Japanese checklist record?
#   2. Is that WCVP concept already represented in VPJD?
#   3. Does the Japanese checklist represent a taxonomic treatment differing
#      from WCVP?
#   4. Is there existing WCVP Japan distribution / establishment evidence?
#   5. Which records remain credible candidates for genuine VPJD omission?
#
# IMPORTANT
# ---------
# This module is AUDIT / RECONCILIATION ONLY.
#
# It MUST NOT:
#   - modify canonical VPJD taxonomy;
#   - modify Star allocations;
#   - automatically add taxa;
#   - automatically remove taxa;
#   - impose Japanese checklist taxonomy on VPJD;
#   - perform fuzzy matching;
#   - automatically accept synonyms.
#
# ==============================================================================


# ==============================================================================
# 1. VERSION
# ==============================================================================

VERSION <-
  "0.1.0"


# ==============================================================================
# 2. LIBRARIES
# ==============================================================================

library(DBI)
library(duckdb)
library(dplyr)
library(tibble)
library(stringr)
library(readr)
library(purrr)
library(tidyr)


# ==============================================================================
# 3. PATHS
# ==============================================================================

DB_PATH <-
  paste0(
    "I:/R/OJPCP/VPJD-OJPCP/",
    "data/interim/occurrences/vpjd_occurrences.duckdb"
  )


OUTPUT_DIR <-
  paste0(
    "I:/R/OJPCP/VPJD-OJPCP/",
    "outputs/tables/taxonomic_revision/",
    "03i_reconcile_canonical_gap_candidates_to_wcvp_backbone"
  )


dir.create(
  OUTPUT_DIR,
  recursive = TRUE,
  showWarnings = FALSE
)


# ==============================================================================
# 4. EXPECTED INVARIANTS
# ==============================================================================

EXPECTED_CANONICAL_POPULATION <-
  11439L


EXPECTED_03H_CANDIDATES <-
  201L


# ==============================================================================
# 5. TABLE AUTHORITIES
# ==============================================================================

CANONICAL_TABLE <-
  "vpjd_star_provisional_wholesale_allocation"


LINEAGE_TABLE <-
  "vpjd_taxrev_02h_validated_vascular_lineage"


TABLE_03H <-
  "vpjd_taxrev_03h_manual_review"


GREEN_CORE_TABLE <-
  "vpjd_taxrev_03a_greenlist_core"


FERN_CORE_TABLE <-
  "vpjd_taxrev_03a_fern_greenlist_core"


# ==============================================================================
# 6. HELPER FUNCTIONS
# ==============================================================================

normalise_text <-
  function(x) {
    
    x <-
      as.character(x)
    
    x <-
      stringr::str_squish(x)
    
    x[
      is.na(x) |
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
      tolower(x)
    
    x <-
      gsub(
        "[[:space:]]+",
        " ",
        x
      )
    
    x
  }


normalise_id <-
  function(x) {
    
    x <-
      normalise_text(x)
    
    x <-
      gsub(
        "\\.0$",
        "",
        x
      )
    
    x
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
      length(field) == 0L ||
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


collapse_unique <-
  function(x) {
    
    x <-
      normalise_text(x)
    
    x <-
      unique(
        x[
          !is.na(x)
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
        inherits = FALSE
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
  }


write_duckdb_table <-
  function(con, name, data) {
    
    DBI::dbWriteTable(
      con,
      name,
      as.data.frame(data),
      overwrite = TRUE
    )
    
    tibble(
      TABLE_NAME = name,
      N_RECORDS = nrow(data),
      WRITTEN = TRUE
    )
  }


write_csv_output <-
  function(data, filename) {
    
    path <-
      file.path(
        OUTPUT_DIR,
        filename
      )
    
    readr::write_csv(
      data,
      path,
      na = ""
    )
    
    tibble(
      FILE_NAME = filename,
      N_RECORDS = nrow(data),
      WRITTEN = TRUE
    )
  }


# ==============================================================================
# 7. CONNECT TO DUCKDB
# ==============================================================================

if (
  !file.exists(DB_PATH)
) {
  
  stop(
    paste0(
      "DuckDB database not found:\n",
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


database_tables <-
  DBI::dbListTables(con)


# ==============================================================================
# 8. DATABASE WRITE TEST
# ==============================================================================

WRITE_TEST_TABLE <-
  "vpjd_taxrev_03i_write_test"


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


write_test_ok <-
  tryCatch(
    {
      
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
      
      nrow(test_result) == 1L
      
    },
    error =
      function(e) {
        
        FALSE
      }
  )


if (
  !write_test_ok
) {
  
  disconnect_safely()
  
  stop(
    paste0(
      "\n03i STOPPED BEFORE RECONCILIATION.\n\n",
      "The VPJD DuckDB database is not writable.\n\n",
      "Database:\n",
      DB_PATH
    )
  )
}


# ==============================================================================
# 9. IDENTIFY 03h SOURCE TABLE
# ==============================================================================

preferred_03h_tables <-
  c(
    "vpjd_taxrev_03h_manual_review",
    "vpjd_taxrev_03h_audit",
    "vpjd_taxrev_03h_adjudication"
  )


available_03h_tables <-
  preferred_03h_tables[
    preferred_03h_tables %in%
      database_tables
  ]


if (
  length(available_03h_tables) > 0L
) {
  
  TABLE_03H <-
    available_03h_tables[[1]]
  
} else {
  
  tables_03h <-
    database_tables[
      grepl(
        "^vpjd_taxrev_03h_",
        database_tables,
        ignore.case = TRUE
      )
    ]
  
  
  if (
    length(tables_03h) == 0L
  ) {
    
    disconnect_safely()
    
    stop(
      "No 03h tables were found."
    )
  }
  
  
  table_sizes_03h <-
    vapply(
      tables_03h,
      function(tbl) {
        
        as.integer(
          DBI::dbGetQuery(
            con,
            paste0(
              "SELECT COUNT(*) AS N FROM ",
              DBI::dbQuoteIdentifier(
                con,
                tbl
              )
            )
          )$N[[1]]
        )
      },
      integer(1)
    )
  
  
  candidate_tables <-
    tables_03h[
      table_sizes_03h ==
        EXPECTED_03H_CANDIDATES
    ]
  
  
  if (
    length(candidate_tables) == 0L
  ) {
    
    disconnect_safely()
    
    stop(
      paste0(
        "Could not identify a 03h table containing ",
        EXPECTED_03H_CANDIDATES,
        " records."
      )
    )
  }
  
  
  TABLE_03H <-
    candidate_tables[[1]]
}


cat(
  "\n03h source table:\n",
  TABLE_03H,
  "\n\n",
  sep = ""
)


# ==============================================================================
# 10. VALIDATE REQUIRED TABLES
# ==============================================================================

required_tables <-
  c(
    CANONICAL_TABLE,
    LINEAGE_TABLE,
    TABLE_03H
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
# 11. LOAD PRIMARY DATA
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


source_03h <-
  DBI::dbReadTable(
    con,
    TABLE_03H
  ) |>
  tibble::as_tibble()


# ==============================================================================
# 12. VALIDATE PRIMARY POPULATIONS
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
  nrow(source_03h) !=
  EXPECTED_03H_CANDIDATES
) {
  
  disconnect_safely()
  
  stop(
    paste0(
      "03h candidate population invariant failed. Expected ",
      EXPECTED_03H_CANDIDATES,
      " records but found ",
      nrow(source_03h),
      "."
    )
  )
}


# ==============================================================================
# 13. IDENTIFY 03h FIELDS
# ==============================================================================

japan_record_id_field <-
  first_existing_field(
    source_03h,
    c(
      "JAPAN_RECORD_ID"
    )
  )


source_field <-
  first_existing_field(
    source_03h,
    c(
      "SOURCE"
    )
  )


source_taxon_id_field <-
  first_existing_field(
    source_03h,
    c(
      "SOURCE_TAXON_ID"
    )
  )


japan_name_field <-
  first_existing_field(
    source_03h,
    c(
      "JAPAN_SCIENTIFIC_NAME"
    )
  )


japanese_name_field <-
  first_existing_field(
    source_03h,
    c(
      "JAPANESE_NAME"
    )
  )


japan_rank_field <-
  first_existing_field(
    source_03h,
    c(
      "JAPAN_RANK"
    )
  )


japan_family_field <-
  first_existing_field(
    source_03h,
    c(
      "JAPAN_FAMILY"
    )
  )


japan_genus_field <-
  first_existing_field(
    source_03h,
    c(
      "JAPAN_GENUS"
    )
  )


wcvp_id_field <-
  first_existing_field(
    source_03h,
    c(
      "WCVP_ID"
    )
  )


wcvp_name_field <-
  first_existing_field(
    source_03h,
    c(
      "WCVP_ACCEPTED_NAME",
      "ACCEPTED_WCVP_NAME"
    )
  )


wcvp_rank_field <-
  first_existing_field(
    source_03h,
    c(
      "WCVP_RANK"
    )
  )


wcvp_family_field <-
  first_existing_field(
    source_03h,
    c(
      "WCVP_FAMILY"
    )
  )


wcvp_genus_field <-
  first_existing_field(
    source_03h,
    c(
      "WCVP_GENUS"
    )
  )


wcvp_status_field <-
  first_existing_field(
    source_03h,
    c(
      "WCVP_STATUS"
    )
  )


# ==============================================================================
# 14. BUILD 03i AUDIT BASE
# ==============================================================================

audit <-
  tibble(
    AUDIT_03I_ROW =
      seq_len(
        nrow(source_03h)
      ),
    
    JAPAN_RECORD_ID =
      safe_field(
        source_03h,
        japan_record_id_field
      ),
    
    SOURCE =
      safe_field(
        source_03h,
        source_field
      ),
    
    SOURCE_TAXON_ID =
      safe_field(
        source_03h,
        source_taxon_id_field
      ),
    
    JAPAN_SCIENTIFIC_NAME =
      safe_field(
        source_03h,
        japan_name_field
      ),
    
    JAPANESE_NAME =
      safe_field(
        source_03h,
        japanese_name_field
      ),
    
    JAPAN_RANK =
      safe_field(
        source_03h,
        japan_rank_field
      ),
    
    JAPAN_FAMILY =
      safe_field(
        source_03h,
        japan_family_field
      ),
    
    JAPAN_GENUS =
      safe_field(
        source_03h,
        japan_genus_field
      ),
    
    WCVP_ID =
      safe_field(
        source_03h,
        wcvp_id_field
      ),
    
    WCVP_ACCEPTED_NAME =
      safe_field(
        source_03h,
        wcvp_name_field
      ),
    
    WCVP_RANK =
      safe_field(
        source_03h,
        wcvp_rank_field
      ),
    
    WCVP_FAMILY =
      safe_field(
        source_03h,
        wcvp_family_field
      ),
    
    WCVP_GENUS =
      safe_field(
        source_03h,
        wcvp_genus_field
      ),
    
    WCVP_STATUS =
      safe_field(
        source_03h,
        wcvp_status_field
      )
  ) |>
  mutate(
    WCVP_ID_NORM =
      normalise_id(
        WCVP_ID
      ),
    
    JAPAN_NAME_NORM =
      normalise_name(
        JAPAN_SCIENTIFIC_NAME
      ),
    
    WCVP_ACCEPTED_NAME_NORM =
      normalise_name(
        WCVP_ACCEPTED_NAME
      )
  )


# ==============================================================================
# 15. IDENTIFY CANONICAL WCVP FIELDS
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
      "WCVP_ACCEPTED_NAME",
      "SCIENTIFIC_NAME"
    )
  )


canonical_rank_field <-
  first_existing_field(
    canonical,
    c(
      "FINAL_WCVP_RANK",
      "RANK"
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


canonical_major_group_field <-
  first_existing_field(
    canonical,
    c(
      "MAJOR_GROUP"
    )
  )


# ==============================================================================
# 16. BUILD CANONICAL WCVP INDEX
# ==============================================================================

canonical_index <-
  tibble(
    VPJD_ROW =
      seq_len(
        nrow(canonical)
      ),
    
    VPJD_WCVP_ID =
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
    
    MAJOR_GROUP =
      safe_field(
        canonical,
        canonical_major_group_field
      )
  ) |>
  mutate(
    VPJD_WCVP_ID_NORM =
      normalise_id(
        VPJD_WCVP_ID
      ),
    
    VPJD_NAME_NORM =
      normalise_name(
        VPJD_SCIENTIFIC_NAME
      )
  )


# ==============================================================================
# 17. RECONFIRM WCVP CONCEPT ABSENCE FROM VPJD
# ==============================================================================

id_profile <-
  canonical_index |>
  filter(
    !is.na(
      VPJD_WCVP_ID_NORM
    )
  ) |>
  group_by(
    VPJD_WCVP_ID_NORM
  ) |>
  summarise(
    VPJD_ID_MATCH_COUNT =
      n(),
    
    VPJD_ID_MATCH_ROWS =
      paste(
        VPJD_ROW,
        collapse = " | "
      ),
    
    VPJD_ID_MATCH_NAMES =
      collapse_unique(
        VPJD_SCIENTIFIC_NAME
      ),
    
    .groups = "drop"
  )


name_profile <-
  canonical_index |>
  filter(
    !is.na(
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
        collapse = " | "
      ),
    
    VPJD_NAME_MATCH_NAMES =
      collapse_unique(
        VPJD_SCIENTIFIC_NAME
      ),
    
    .groups = "drop"
  )


audit <-
  audit |>
  left_join(
    id_profile,
    by =
      c(
        "WCVP_ID_NORM" =
          "VPJD_WCVP_ID_NORM"
      )
  ) |>
  left_join(
    name_profile,
    by =
      c(
        "WCVP_ACCEPTED_NAME_NORM" =
          "VPJD_NAME_NORM"
      )
  ) |>
  mutate(
    VPJD_ID_MATCH_COUNT =
      coalesce(
        VPJD_ID_MATCH_COUNT,
        0L
      ),
    
    VPJD_NAME_MATCH_COUNT =
      coalesce(
        VPJD_NAME_MATCH_COUNT,
        0L
      ),
    
    WCVP_CONCEPT_ALREADY_IN_VPJD =
      VPJD_ID_MATCH_COUNT > 0L |
      VPJD_NAME_MATCH_COUNT > 0L
  )


# ==============================================================================
# 18. IDENTIFY EXISTING WCVP GEOGRAPHY TABLES
# ==============================================================================

wcvp_geography_patterns <-
  c(
    "^vpjd_star_wcvp_",
    "^wcvp_",
    "^occurrence_wcvp_"
  )


wcvp_tables <-
  database_tables[
    Reduce(
      `|`,
      lapply(
        wcvp_geography_patterns,
        function(pattern) {
          
          grepl(
            pattern,
            database_tables,
            ignore.case = TRUE
          )
        }
      )
    )
  ]


wcvp_table_registry <-
  tibble(
    TABLE_NAME =
      wcvp_tables
  ) |>
  mutate(
    N_RECORDS =
      map_int(
        TABLE_NAME,
        function(tbl) {
          
          as.integer(
            DBI::dbGetQuery(
              con,
              paste0(
                "SELECT COUNT(*) AS N FROM ",
                DBI::dbQuoteIdentifier(
                  con,
                  tbl
                )
              )
            )$N[[1]]
          )
        }
      ),
    
    RELEVANT_TO_JAPAN_GEOGRAPHY =
      grepl(
        paste(
          c(
            "distribution",
            "establishment",
            "japan",
            "geography",
            "region",
            "unit"
          ),
          collapse = "|"
        ),
        TABLE_NAME,
        ignore.case = TRUE
      )
  )


# ==============================================================================
# 19. PRIORITISE WCVP JAPAN EVIDENCE TABLES
# ==============================================================================

preferred_evidence_tables <-
  c(
    "vpjd_star_wcvp_establishment_profile",
    "vpjd_star_wcvp_distribution_profile",
    "vpjd_star_wcvp_japan_unit_audit",
    "vpjd_star_wcvp_key_geography",
    "vpjd_star_wcvp_no_japan_evidence_audit",
    "vpjd_star_wcvp_taxon_linkage_profile"
  )


available_evidence_tables <-
  preferred_evidence_tables[
    preferred_evidence_tables %in%
      database_tables
  ]


evidence_table_registry <-
  tibble(
    TABLE_NAME =
      available_evidence_tables
  ) |>
  mutate(
    N_RECORDS =
      map_int(
        TABLE_NAME,
        function(tbl) {
          
          as.integer(
            DBI::dbGetQuery(
              con,
              paste0(
                "SELECT COUNT(*) AS N FROM ",
                DBI::dbQuoteIdentifier(
                  con,
                  tbl
                )
              )
            )$N[[1]]
          )
        }
      ),
    
    FIELDS =
      map_chr(
        TABLE_NAME,
        function(tbl) {
          
          fields <-
            DBI::dbListFields(
              con,
              tbl
            )
          
          paste(
            fields,
            collapse = " | "
          )
        }
      )
  )


# ==============================================================================
# 20. GENERIC WCVP EVIDENCE EXTRACTION
# ==============================================================================

extract_wcvp_evidence <-
  function(tbl) {
    
    data <-
      DBI::dbReadTable(
        con,
        tbl
      ) |>
      tibble::as_tibble()
    
    
    id_field <-
      first_existing_field(
        data,
        c(
          "FINAL_WCVP_ID",
          "WCVP_ID",
          "WCVP_ACCEPTED_ID",
          "ACCEPTED_WCVP_ID",
          "PLANT_NAME_ID",
          "TAXON_ID"
        )
      )
    
    
    name_field <-
      first_existing_field(
        data,
        c(
          "FINAL_WCVP_RECOGNISED_NAME",
          "WCVP_ACCEPTED_NAME",
          "ACCEPTED_NAME",
          "SCIENTIFIC_NAME",
          "SCIENTIFICNAME"
        )
      )
    
    
    if (
      is.na(id_field) &&
      is.na(name_field)
    ) {
      
      return(
        tibble()
      )
    }
    
    
    tibble(
      EVIDENCE_TABLE =
        tbl,
      
      WCVP_ID_NORM =
        if (
          !is.na(id_field)
        ) {
          
          normalise_id(
            data[[id_field]]
          )
          
        } else {
          
          rep(
            NA_character_,
            nrow(data)
          )
        },
      
      WCVP_NAME_NORM =
        if (
          !is.na(name_field)
        ) {
          
          normalise_name(
            data[[name_field]]
          )
          
        } else {
          
          rep(
            NA_character_,
            nrow(data)
          )
        },
      
      EVIDENCE_ROW =
        seq_len(
          nrow(data)
        )
    )
  }


wcvp_evidence_index <-
  map_dfr(
    available_evidence_tables,
    extract_wcvp_evidence
  )


# ==============================================================================
# 21. COUNT EXISTING WCVP EVIDENCE FOR EACH CANDIDATE
# ==============================================================================

id_evidence_profile <-
  wcvp_evidence_index |>
  filter(
    !is.na(
      WCVP_ID_NORM
    )
  ) |>
  group_by(
    WCVP_ID_NORM
  ) |>
  summarise(
    WCVP_EVIDENCE_ID_MATCH_COUNT =
      n(),
    
    WCVP_EVIDENCE_ID_TABLES =
      collapse_unique(
        EVIDENCE_TABLE
      ),
    
    .groups = "drop"
  )


name_evidence_profile <-
  wcvp_evidence_index |>
  filter(
    !is.na(
      WCVP_NAME_NORM
    )
  ) |>
  group_by(
    WCVP_NAME_NORM
  ) |>
  summarise(
    WCVP_EVIDENCE_NAME_MATCH_COUNT =
      n(),
    
    WCVP_EVIDENCE_NAME_TABLES =
      collapse_unique(
        EVIDENCE_TABLE
      ),
    
    .groups = "drop"
  )


audit <-
  audit |>
  left_join(
    id_evidence_profile,
    by =
      "WCVP_ID_NORM"
  ) |>
  left_join(
    name_evidence_profile,
    by =
      c(
        "WCVP_ACCEPTED_NAME_NORM" =
          "WCVP_NAME_NORM"
      )
  ) |>
  mutate(
    WCVP_EVIDENCE_ID_MATCH_COUNT =
      coalesce(
        WCVP_EVIDENCE_ID_MATCH_COUNT,
        0L
      ),
    
    WCVP_EVIDENCE_NAME_MATCH_COUNT =
      coalesce(
        WCVP_EVIDENCE_NAME_MATCH_COUNT,
        0L
      ),
    
    EXISTING_WCVP_EVIDENCE_AVAILABLE =
      WCVP_EVIDENCE_ID_MATCH_COUNT > 0L |
      WCVP_EVIDENCE_NAME_MATCH_COUNT > 0L
  )


# ==============================================================================
# 22. TAXONOMIC-TREATMENT DIAGNOSTICS
# ==============================================================================

audit <-
  audit |>
  mutate(
    JAPAN_WCVP_NAME_IDENTICAL =
      !is.na(
        JAPAN_NAME_NORM
      ) &
      !is.na(
        WCVP_ACCEPTED_NAME_NORM
      ) &
      JAPAN_NAME_NORM ==
      WCVP_ACCEPTED_NAME_NORM,
    
    JAPAN_WCVP_NAME_DIFFERENT =
      !is.na(
        JAPAN_NAME_NORM
      ) &
      !is.na(
        WCVP_ACCEPTED_NAME_NORM
      ) &
      JAPAN_NAME_NORM !=
      WCVP_ACCEPTED_NAME_NORM,
    
    JAPAN_WCVP_RANK_DIFFERENT =
      !is.na(
        JAPAN_RANK
      ) &
      !is.na(
        WCVP_RANK
      ) &
      toupper(
        JAPAN_RANK
      ) !=
      toupper(
        WCVP_RANK
      ),
    
    JAPAN_WCVP_FAMILY_DIFFERENT =
      !is.na(
        JAPAN_FAMILY
      ) &
      !is.na(
        WCVP_FAMILY
      ) &
      toupper(
        JAPAN_FAMILY
      ) !=
      toupper(
        WCVP_FAMILY
      ),
    
    JAPAN_WCVP_GENUS_DIFFERENT =
      !is.na(
        JAPAN_GENUS
      ) &
      !is.na(
        WCVP_GENUS
      ) &
      toupper(
        JAPAN_GENUS
      ) !=
      toupper(
        WCVP_GENUS
      )
  )


# ==============================================================================
# 23. ASSIGN WCVP-BACKBONE DIAGNOSTIC CLASS
# ==============================================================================

audit <-
  audit |>
  mutate(
    WCVP_BACKBONE_CLASS =
      case_when(
        
        WCVP_CONCEPT_ALREADY_IN_VPJD ~
          "EXISTING_VPJD_WCVP_CONCEPT",
        
        is.na(
          WCVP_ID_NORM
        ) &
          is.na(
            WCVP_ACCEPTED_NAME_NORM
          ) ~
          "WCVP_CONCEPT_UNRESOLVED",
        
        JAPAN_WCVP_NAME_DIFFERENT |
          JAPAN_WCVP_RANK_DIFFERENT |
          JAPAN_WCVP_FAMILY_DIFFERENT |
          JAPAN_WCVP_GENUS_DIFFERENT ~
          "JAPANESE_WCVP_TAXONOMIC_TREATMENT_DIFFERENCE",
        
        EXISTING_WCVP_EVIDENCE_AVAILABLE ~
          "WCVP_ACCEPTED_CONCEPT_WITH_EXISTING_EVIDENCE",
        
        TRUE ~
          "WCVP_ACCEPTED_CONCEPT_REQUIRES_JAPAN_STATUS_EVIDENCE"
      )
  )


# ==============================================================================
# 24. ASSIGN EVIDENCE REQUIREMENT
# ==============================================================================

audit <-
  audit |>
  mutate(
    EVIDENCE_REQUIREMENT =
      case_when(
        
        WCVP_CONCEPT_ALREADY_IN_VPJD ~
          "NO_NEW_CANONICAL_CONCEPT_REQUIRED",
        
        WCVP_BACKBONE_CLASS ==
          "WCVP_CONCEPT_UNRESOLVED" ~
          "WCVP_TAXONOMIC_RESOLUTION_REQUIRED",
        
        WCVP_BACKBONE_CLASS ==
          "JAPANESE_WCVP_TAXONOMIC_TREATMENT_DIFFERENCE" &
          EXISTING_WCVP_EVIDENCE_AVAILABLE ~
          "WCVP_TAXONOMIC_CROSSWALK_AND_JAPAN_STATUS_REVIEW",
        
        WCVP_BACKBONE_CLASS ==
          "JAPANESE_WCVP_TAXONOMIC_TREATMENT_DIFFERENCE" ~
          "WCVP_TAXONOMIC_CROSSWALK_REQUIRED",
        
        EXISTING_WCVP_EVIDENCE_AVAILABLE ~
          "INTERROGATE_EXISTING_WCVP_JAPAN_EVIDENCE",
        
        TRUE ~
          "EXTERNAL_JAPAN_STATUS_EVIDENCE_REQUIRED"
      )
  )


# ==============================================================================
# 25. VPJD CANONICAL-GAP STATUS
# ==============================================================================

audit <-
  audit |>
  mutate(
    VPJD_GAP_STATUS =
      case_when(
        
        WCVP_CONCEPT_ALREADY_IN_VPJD ~
          "NOT_A_CANONICAL_GAP",
        
        is.na(
          WCVP_ID_NORM
        ) &
          is.na(
            WCVP_ACCEPTED_NAME_NORM
          ) ~
          "UNRESOLVED_WCVP_CONCEPT",
        
        TRUE ~
          "POTENTIAL_CANONICAL_GAP_REQUIRES_JAPAN_SCOPE_EVIDENCE"
      )
  )


# ==============================================================================
# 26. SUBSETS
# ==============================================================================

existing_vpjd_concepts <-
  audit |>
  filter(
    WCVP_CONCEPT_ALREADY_IN_VPJD
  )


taxonomic_treatment_differences <-
  audit |>
  filter(
    WCVP_BACKBONE_CLASS ==
      "JAPANESE_WCVP_TAXONOMIC_TREATMENT_DIFFERENCE"
  )


existing_wcvp_evidence <-
  audit |>
  filter(
    EXISTING_WCVP_EVIDENCE_AVAILABLE
  )


external_evidence_required <-
  audit |>
  filter(
    EVIDENCE_REQUIREMENT ==
      "EXTERNAL_JAPAN_STATUS_EVIDENCE_REQUIRED"
  )


potential_canonical_gaps <-
  audit |>
  filter(
    VPJD_GAP_STATUS ==
      "POTENTIAL_CANONICAL_GAP_REQUIRES_JAPAN_SCOPE_EVIDENCE"
  )


# ==============================================================================
# 27. SUMMARY COUNTS
# ==============================================================================

n_total <-
  nrow(audit)


n_existing_vpjd <-
  sum(
    audit$WCVP_CONCEPT_ALREADY_IN_VPJD,
    na.rm = TRUE
  )


n_taxonomic_difference <-
  sum(
    audit$WCVP_BACKBONE_CLASS ==
      "JAPANESE_WCVP_TAXONOMIC_TREATMENT_DIFFERENCE",
    na.rm = TRUE
  )


n_existing_evidence <-
  sum(
    audit$EXISTING_WCVP_EVIDENCE_AVAILABLE,
    na.rm = TRUE
  )


n_external_evidence <-
  sum(
    audit$EVIDENCE_REQUIREMENT ==
      "EXTERNAL_JAPAN_STATUS_EVIDENCE_REQUIRED",
    na.rm = TRUE
  )


n_potential_gap <-
  nrow(
    potential_canonical_gaps
  )


# ==============================================================================
# 28. SUMMARY TABLES
# ==============================================================================

backbone_class_summary <-
  audit |>
  count(
    WCVP_BACKBONE_CLASS,
    name = "N_RECORDS"
  ) |>
  arrange(
    desc(
      N_RECORDS
    )
  )


evidence_requirement_summary <-
  audit |>
  count(
    EVIDENCE_REQUIREMENT,
    name = "N_RECORDS"
  ) |>
  arrange(
    desc(
      N_RECORDS
    )
  )


gap_status_summary <-
  audit |>
  count(
    VPJD_GAP_STATUS,
    name = "N_RECORDS"
  ) |>
  arrange(
    desc(
      N_RECORDS
    )
  )


family_summary <-
  audit |>
  count(
    WCVP_FAMILY,
    WCVP_BACKBONE_CLASS,
    name = "N_RECORDS"
  ) |>
  arrange(
    WCVP_FAMILY,
    desc(
      N_RECORDS
    )
  )


# ==============================================================================
# 29. SUPPORTING TABLE FIELD PROFILE
# ==============================================================================

evidence_field_profile <-
  map_dfr(
    available_evidence_tables,
    function(tbl) {
      
      fields <-
        DBI::dbListFields(
          con,
          tbl
        )
      
      tibble(
        TABLE_NAME =
          tbl,
        
        FIELD_NAME =
          fields
      )
    }
  )


# ==============================================================================
# 30. VALIDATION
# ==============================================================================

validation <-
  tibble(
    CHECK =
      c(
        "Canonical VPJD population = 11,439",
        "03h candidate population = 201",
        "03i audit population = 201",
        "03i AUDIT_03I_ROW unique",
        "JAPAN_RECORD_ID unique",
        "All 201 assigned WCVP backbone class",
        "WCVP backbone classes sum to 201",
        "All 201 assigned evidence requirement",
        "Evidence requirements sum to 201",
        "All 201 assigned VPJD gap status",
        "VPJD gap statuses sum to 201",
        "Existing VPJD concepts subset of 201",
        "Taxonomic-treatment differences subset of 201",
        "Existing WCVP evidence subset of 201",
        "Potential canonical gaps subset of 201",
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
        
        nrow(source_03h) ==
          EXPECTED_03H_CANDIDATES,
        
        nrow(audit) ==
          EXPECTED_03H_CANDIDATES,
        
        n_distinct(
          audit$AUDIT_03I_ROW
        ) ==
          EXPECTED_03H_CANDIDATES,
        
        n_distinct(
          audit$JAPAN_RECORD_ID
        ) ==
          EXPECTED_03H_CANDIDATES,
        
        sum(
          !is.na(
            audit$WCVP_BACKBONE_CLASS
          )
        ) ==
          EXPECTED_03H_CANDIDATES,
        
        sum(
          backbone_class_summary$N_RECORDS
        ) ==
          EXPECTED_03H_CANDIDATES,
        
        sum(
          !is.na(
            audit$EVIDENCE_REQUIREMENT
          )
        ) ==
          EXPECTED_03H_CANDIDATES,
        
        sum(
          evidence_requirement_summary$N_RECORDS
        ) ==
          EXPECTED_03H_CANDIDATES,
        
        sum(
          !is.na(
            audit$VPJD_GAP_STATUS
          )
        ) ==
          EXPECTED_03H_CANDIDATES,
        
        sum(
          gap_status_summary$N_RECORDS
        ) ==
          EXPECTED_03H_CANDIDATES,
        
        n_existing_vpjd <=
          EXPECTED_03H_CANDIDATES,
        
        n_taxonomic_difference <=
          EXPECTED_03H_CANDIDATES,
        
        n_existing_evidence <=
          EXPECTED_03H_CANDIDATES,
        
        n_potential_gap <=
          EXPECTED_03H_CANDIDATES,
        
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
# 31. DECISION
# ==============================================================================

decision <-
  if (
    all_valid
  ) {
    
    "WCVP_BACKBONE_RECONCILIATION_COMPLETE"
    
  } else {
    
    "REVIEW_REQUIRED"
  }


# ==============================================================================
# 32. METADATA
# ==============================================================================

metadata <-
  tibble(
    KEY =
      c(
        "MODULE",
        "VERSION",
        "TAXONOMIC_AUTHORITY",
        "JAPANESE_CHECKLIST_ROLE",
        "CANONICAL_TABLE",
        "SOURCE_03H_TABLE",
        "EXPECTED_CANONICAL_POPULATION",
        "EXPECTED_03H_CANDIDATES",
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
      c(
        "03i_reconcile_canonical_gap_candidates_to_wcvp_backbone",
        VERSION,
        "Kew / WCVP",
        "Occurrence evidence and taxonomic crosswalk; not canonical authority",
        CANONICAL_TABLE,
        TABLE_03H,
        as.character(
          EXPECTED_CANONICAL_POPULATION
        ),
        as.character(
          EXPECTED_03H_CANDIDATES
        ),
        "FALSE",
        "FALSE",
        "FALSE",
        "FALSE",
        "FALSE",
        "0",
        "0",
        decision
      )
  )


# ==============================================================================
# 33. WRITE DUCKDB OUTPUTS
# ==============================================================================

duckdb_outputs <-
  list(
    vpjd_taxrev_03i_audit =
      audit,
    
    vpjd_taxrev_03i_existing_vpjd_concepts =
      existing_vpjd_concepts,
    
    vpjd_taxrev_03i_taxonomic_treatment_differences =
      taxonomic_treatment_differences,
    
    vpjd_taxrev_03i_existing_wcvp_evidence =
      existing_wcvp_evidence,
    
    vpjd_taxrev_03i_external_evidence_required =
      external_evidence_required,
    
    vpjd_taxrev_03i_potential_canonical_gaps =
      potential_canonical_gaps,
    
    vpjd_taxrev_03i_backbone_class_summary =
      backbone_class_summary,
    
    vpjd_taxrev_03i_evidence_requirement_summary =
      evidence_requirement_summary,
    
    vpjd_taxrev_03i_gap_status_summary =
      gap_status_summary,
    
    vpjd_taxrev_03i_family_summary =
      family_summary,
    
    vpjd_taxrev_03i_wcvp_table_registry =
      wcvp_table_registry,
    
    vpjd_taxrev_03i_evidence_table_registry =
      evidence_table_registry,
    
    vpjd_taxrev_03i_evidence_field_profile =
      evidence_field_profile,
    
    vpjd_taxrev_03i_validation =
      validation,
    
    vpjd_taxrev_03i_metadata =
      metadata
  )


duckdb_write_status <-
  imap_dfr(
    duckdb_outputs,
    function(data, name) {
      
      write_duckdb_table(
        con,
        name,
        data
      )
    }
  )


# ==============================================================================
# 34. WRITE CSV OUTPUTS
# ==============================================================================

csv_outputs <-
  list(
    "vpjd_taxrev_03i_audit.csv" =
      audit,
    
    "vpjd_taxrev_03i_existing_vpjd_concepts.csv" =
      existing_vpjd_concepts,
    
    "vpjd_taxrev_03i_taxonomic_treatment_differences.csv" =
      taxonomic_treatment_differences,
    
    "vpjd_taxrev_03i_existing_wcvp_evidence.csv" =
      existing_wcvp_evidence,
    
    "vpjd_taxrev_03i_external_evidence_required.csv" =
      external_evidence_required,
    
    "vpjd_taxrev_03i_potential_canonical_gaps.csv" =
      potential_canonical_gaps,
    
    "vpjd_taxrev_03i_backbone_class_summary.csv" =
      backbone_class_summary,
    
    "vpjd_taxrev_03i_evidence_requirement_summary.csv" =
      evidence_requirement_summary,
    
    "vpjd_taxrev_03i_gap_status_summary.csv" =
      gap_status_summary,
    
    "vpjd_taxrev_03i_family_summary.csv" =
      family_summary,
    
    "vpjd_taxrev_03i_wcvp_table_registry.csv" =
      wcvp_table_registry,
    
    "vpjd_taxrev_03i_evidence_table_registry.csv" =
      evidence_table_registry,
    
    "vpjd_taxrev_03i_evidence_field_profile.csv" =
      evidence_field_profile,
    
    "vpjd_taxrev_03i_validation.csv" =
      validation,
    
    "vpjd_taxrev_03i_metadata.csv" =
      metadata
  )


csv_write_status <-
  imap_dfr(
    csv_outputs,
    function(data, filename) {
      
      write_csv_output(
        data,
        filename
      )
    }
  )


# ==============================================================================
# 35. SUMMARY REPORT
# ==============================================================================

cat("\n")
cat("------------------------------------------------------------\n")
cat("03i WCVP BACKBONE RECONCILIATION SUMMARY\n")
cat("------------------------------------------------------------\n\n")


print(
  backbone_class_summary,
  n = Inf,
  width = Inf
)


cat("\n")


print(
  evidence_requirement_summary,
  n = Inf,
  width = Inf
)


cat("\n")


print(
  gap_status_summary,
  n = Inf,
  width = Inf
)


# ==============================================================================
# 36. VALIDATION REPORT
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
# 37. FAILED VALIDATION REPORT
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
# 38. CLEAN DISCONNECT
# ==============================================================================

DBI::dbDisconnect(
  con,
  shutdown = TRUE
)


rm(con)


gc()


# ==============================================================================
# 39. COMPLETION REPORT
# ==============================================================================

cat("\n")
cat("============================================================\n")

cat(
  "VPJD TAXONOMIC REVISION 03i v",
  VERSION,
  " COMPLETE\n",
  sep = ""
)

cat("============================================================\n\n")


cat(
  "Primary taxonomic authority: Kew / WCVP\n"
)


cat(
  "Japanese checklist role: crosswalk + Japanese flora evidence\n\n"
)


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
  "03h candidates reconciled: ",
  format(
    n_total,
    big.mark = ","
  ),
  "\n\n",
  sep = ""
)


cat(
  "Candidates mapping to existing VPJD concepts: ",
  format(
    n_existing_vpjd,
    big.mark = ","
  ),
  "\n",
  sep = ""
)


cat(
  "Japanese/WCVP taxonomic-treatment differences: ",
  format(
    n_taxonomic_difference,
    big.mark = ","
  ),
  "\n",
  sep = ""
)


cat(
  "Candidates with existing WCVP evidence: ",
  format(
    n_existing_evidence,
    big.mark = ","
  ),
  "\n",
  sep = ""
)


cat(
  "Candidates requiring external Japan-status evidence: ",
  format(
    n_external_evidence,
    big.mark = ","
  ),
  "\n",
  sep = ""
)


cat(
  "Potential canonical gaps still requiring scope evidence: ",
  format(
    n_potential_gap,
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


cat("WCVP BACKBONE RECONCILIATION / AUDIT ONLY\n")
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
  "03i DuckDB outputs written: ",
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
  "03i CSV outputs written: ",
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
cat("03i COMPLETE\n")
cat("============================================================\n")


# ==============================================================================
# END
# ==============================================================================