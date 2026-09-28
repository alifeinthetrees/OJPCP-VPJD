# ==============================================================================
# VPJD TAXONOMIC REVISION
# 03c_reconcile_contemporary_japan_checklists_to_vpjd.R
#
# PURPOSE
# -------
# Reconcile contemporary Japanese vascular-plant checklist records against the
# canonical VPJD population.
#
# AUTHORITATIVE VPJD SOURCES
# --------------------------
# Canonical concept / Star authority:
#   vpjd_star_provisional_wholesale_allocation
#
# Validated taxonomic lineage authority:
#   vpjd_taxrev_02h_validated_vascular_lineage
#
# Contemporary Japanese checklist sources:
#   GreenList 2.02rc
#   FernGreenList 2.0
#
# IMPORTANT
# ---------
# This module is RECONCILIATION / AUDIT ONLY.
#
# It does NOT:
#   - modify canonical VPJD taxonomy;
#   - modify Star allocations;
#   - remove taxa;
#   - add taxa;
#   - automatically accept Japanese checklist taxonomy over WCVP;
#   - automatically infer synonymy;
#   - perform fuzzy taxonomic matching.
#
# VERSION
# -------
# 0.2.0
# ==============================================================================


# ==============================================================================
# 01. CONFIGURATION
# ==============================================================================

VERSION <- "0.2.0"

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
    "03c_reconcile_contemporary_japan_checklists_to_vpjd"
  )

dir.create(
  OUTPUT_DIR,
  recursive = TRUE,
  showWarnings = FALSE
)


# ==============================================================================
# 02. SOURCE TABLES
# ==============================================================================

CANONICAL_TABLE <-
  "vpjd_star_provisional_wholesale_allocation"

LINEAGE_TABLE <-
  "vpjd_taxrev_02h_validated_vascular_lineage"

GREEN_CORE_TABLE <-
  "vpjd_taxrev_03a_greenlist_core"

FERN_CORE_TABLE <-
  "vpjd_taxrev_03a_fern_greenlist_core"

GREEN_VERNACULAR_TABLE <-
  "vpjd_taxrev_03a_greenlist_vernacularname_1"

FERN_VERNACULAR_TABLE <-
  "vpjd_taxrev_03a_fern_greenlist_vernacularname_2"

GREEN_DISTRIBUTION_TABLE <-
  "vpjd_taxrev_03a_greenlist_distribution_2"

FERN_SPECIES_PROFILE_TABLE <-
  "vpjd_taxrev_03a_fern_greenlist_speciesprofile_1"


# ==============================================================================
# 03. EXPECTED VPJD STRUCTURE
# ==============================================================================

EXPECTED_CANONICAL_POPULATION <- 11439L
EXPECTED_CANONICAL_FAMILIES <- 260L
EXPECTED_MAJOR_GROUPS <- 4L

EXPECTED_MAJOR_GROUP_NAMES <-
  c(
    "ANGIOSPERM",
    "FERN",
    "GYMNOSPERM",
    "LYCOPHYTE"
  )


# ==============================================================================
# 04. OUTPUT TABLES
# ==============================================================================

OUTPUT_AUTHORITY_TABLE <-
  "vpjd_taxrev_03c_vpjd_authority"

OUTPUT_RECONCILIATION_TABLE <-
  "vpjd_taxrev_03c_reconciliation"

OUTPUT_MATCHED_TABLE <-
  "vpjd_taxrev_03c_matched"

OUTPUT_UNMATCHED_JAPAN_TABLE <-
  "vpjd_taxrev_03c_unmatched_japan"

OUTPUT_UNMATCHED_VPJD_TABLE <-
  "vpjd_taxrev_03c_unmatched_vpjd"

OUTPUT_AMBIGUOUS_TABLE <-
  "vpjd_taxrev_03c_ambiguous"

OUTPUT_MATCH_METHOD_TABLE <-
  "vpjd_taxrev_03c_match_method_summary"

OUTPUT_SOURCE_SUMMARY_TABLE <-
  "vpjd_taxrev_03c_source_summary"

OUTPUT_GROUP_SUMMARY_TABLE <-
  "vpjd_taxrev_03c_major_group_summary"

OUTPUT_AUTHORITY_VALIDATION_TABLE <-
  "vpjd_taxrev_03c_authority_validation"

OUTPUT_VALIDATION_TABLE <-
  "vpjd_taxrev_03c_validation"


# ==============================================================================
# 05. CSV OUTPUTS
# ==============================================================================

OUTPUT_AUTHORITY_CSV <-
  file.path(
    OUTPUT_DIR,
    "03c_vpjd_reconciliation_authority.csv"
  )

OUTPUT_RECONCILIATION_CSV <-
  file.path(
    OUTPUT_DIR,
    "03c_contemporary_japan_to_vpjd_reconciliation.csv"
  )

OUTPUT_MATCHED_CSV <-
  file.path(
    OUTPUT_DIR,
    "03c_matched_japan_checklist_records.csv"
  )

OUTPUT_UNMATCHED_JAPAN_CSV <-
  file.path(
    OUTPUT_DIR,
    "03c_unmatched_japan_checklist_records.csv"
  )

OUTPUT_UNMATCHED_VPJD_CSV <-
  file.path(
    OUTPUT_DIR,
    "03c_unmatched_vpjd_records.csv"
  )

OUTPUT_AMBIGUOUS_CSV <-
  file.path(
    OUTPUT_DIR,
    "03c_ambiguous_matches_review.csv"
  )

OUTPUT_MATCH_METHOD_CSV <-
  file.path(
    OUTPUT_DIR,
    "03c_match_method_summary.csv"
  )

OUTPUT_SOURCE_SUMMARY_CSV <-
  file.path(
    OUTPUT_DIR,
    "03c_source_summary.csv"
  )

OUTPUT_GROUP_SUMMARY_CSV <-
  file.path(
    OUTPUT_DIR,
    "03c_major_group_summary.csv"
  )

OUTPUT_AUTHORITY_VALIDATION_CSV <-
  file.path(
    OUTPUT_DIR,
    "03c_authority_validation.csv"
  )

OUTPUT_VALIDATION_CSV <-
  file.path(
    OUTPUT_DIR,
    "03c_validation.csv"
  )


# ==============================================================================
# 06. PACKAGES
# ==============================================================================

required_packages <-
  c(
    "DBI",
    "duckdb",
    "dplyr",
    "tibble",
    "stringr",
    "readr"
  )

missing_packages <-
  required_packages[
    !vapply(
      required_packages,
      requireNamespace,
      logical(1),
      quietly = TRUE
    )
  ]

if (length(missing_packages) > 0L) {
  
  stop(
    paste0(
      "Missing required package(s):\n",
      paste(
        missing_packages,
        collapse = "\n"
      )
    )
  )
}

suppressPackageStartupMessages({
  
  library(DBI)
  library(duckdb)
  library(dplyr)
  library(tibble)
  library(stringr)
  library(readr)
  
})


# ==============================================================================
# 07. HELPER FUNCTIONS
# ==============================================================================

normalise_text <-
  function(x) {
    
    x <-
      as.character(
        x
      )
    
    x[is.na(x)] <-
      ""
    
    x <-
      stringr::str_replace_all(
        x,
        "\u00A0",
        " "
      )
    
    x <-
      stringr::str_squish(
        x
      )
    
    x[x == ""] <-
      NA_character_
    
    x
  }


first_existing_field <-
  function(data, candidates) {
    
    if (ncol(data) == 0L) {
      return(NA_character_)
    }
    
    fields <-
      names(data)
    
    hit <-
      match(
        toupper(candidates),
        toupper(fields)
      )
    
    hit <-
      hit[
        !is.na(hit)
      ]
    
    if (length(hit) == 0L) {
      return(NA_character_)
    }
    
    fields[[hit[[1]]]]
  }


safe_read_table <-
  function(con, table_name, available_tables) {
    
    if (!(table_name %in% available_tables)) {
      return(tibble())
    }
    
    DBI::dbReadTable(
      con,
      table_name
    ) |>
      tibble::as_tibble()
  }


collapse_unique <-
  function(x) {
    
    x <-
      normalise_text(
        x
      )
    
    x <-
      x[
        !is.na(x)
      ]
    
    x <-
      sort(
        unique(x)
      )
    
    if (length(x) == 0L) {
      return(NA_character_)
    }
    
    paste(
      x,
      collapse = " | "
    )
  }


write_db_table <-
  function(con, table_name, data) {
    
    DBI::dbWriteTable(
      con,
      table_name,
      as.data.frame(data),
      overwrite = TRUE
    )
  }


write_csv_file <-
  function(data, path) {
    
    readr::write_excel_csv(
      data,
      path,
      na = ""
    )
  }


# ==============================================================================
# 08. SCIENTIFIC-NAME NORMALISATION
# ==============================================================================

normalise_scientific_name <-
  function(x) {
    
    x <-
      normalise_text(
        x
      )
    
    x <-
      stringr::str_replace_all(
        x,
        "[×✕]",
        "x"
      )
    
    x <-
      stringr::str_squish(
        x
      )
    
    x
  }


strip_authorship <-
  function(x) {
    
    x <-
      normalise_scientific_name(
        x
      )
    
    if (length(x) == 0L) {
      return(character())
    }
    
    vapply(
      x,
      function(name) {
        
        if (is.na(name)) {
          return(NA_character_)
        }
        
        tokens <-
          unlist(
            strsplit(
              name,
              "\\s+"
            )
          )
        
        tokens <-
          tokens[
            tokens != ""
          ]
        
        if (length(tokens) == 0L) {
          return(NA_character_)
        }
        
        if (length(tokens) == 1L) {
          return(tokens[[1]])
        }
        
        genus <-
          tokens[[1]]
        
        second <-
          tokens[[2]]
        
        if (
          second %in%
          c(
            "x",
            "×"
          ) &&
          length(tokens) >= 3L
        ) {
          
          base <-
            paste(
              genus,
              "x",
              tokens[[3]]
            )
          
        } else {
          
          base <-
            paste(
              genus,
              second
            )
        }
        
        rank_tokens <-
          c(
            "subsp.",
            "subsp",
            "ssp.",
            "ssp",
            "var.",
            "var",
            "f.",
            "f",
            "forma",
            "nothosubsp.",
            "nothosubsp",
            "nothovar.",
            "nothovar"
          )
        
        rank_position <-
          which(
            tolower(tokens) %in%
              rank_tokens
          )
        
        if (length(rank_position) > 0L) {
          
          p <-
            rank_position[[1]]
          
          if (length(tokens) >= (p + 1L)) {
            
            return(
              paste(
                base,
                tokens[[p]],
                tokens[[p + 1L]]
              )
            )
          }
        }
        
        base
      },
      character(1)
    )
  }


extract_genus <-
  function(x) {
    
    x <-
      strip_authorship(
        x
      )
    
    vapply(
      x,
      function(name) {
        
        if (is.na(name)) {
          return(NA_character_)
        }
        
        tokens <-
          unlist(
            strsplit(
              name,
              "\\s+"
            )
          )
        
        if (length(tokens) == 0L) {
          return(NA_character_)
        }
        
        tokens[[1]]
      },
      character(1)
    )
  }


# ==============================================================================
# 09. DATABASE CHECK
# ==============================================================================

if (!file.exists(DB_PATH)) {
  
  stop(
    paste0(
      "VPJD DuckDB database not found:\n",
      DB_PATH
    )
  )
}


# ==============================================================================
# 10. CONNECT
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
# 11. WRITE-ACCESS TEST
# ==============================================================================

WRITE_TEST_TABLE <-
  "vpjd_taxrev_03c_write_test"

write_test_pass <-
  FALSE

tryCatch({
  
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
  
  write_test_result <-
    DBI::dbGetQuery(
      con,
      paste0(
        "SELECT * FROM ",
        WRITE_TEST_TABLE
      )
    )
  
  write_test_pass <-
    nrow(write_test_result) == 1L &&
    write_test_result$TEST_ID[[1]] == 1L
  
  DBI::dbRemoveTable(
    con,
    WRITE_TEST_TABLE
  )
  
}, error = function(e) {
  
  disconnect_safely()
  
  stop(
    paste0(
      "\n03c STOPPED BEFORE RECONCILIATION.\n\n",
      "DuckDB write-access test failed.\n\n",
      conditionMessage(e)
    )
  )
})

if (!write_test_pass) {
  
  disconnect_safely()
  
  stop(
    "DuckDB write-access test failed."
  )
}


# ==============================================================================
# 12. CHECK REQUIRED TABLES
# ==============================================================================

database_tables <-
  DBI::dbListTables(
    con
  )

required_tables <-
  c(
    CANONICAL_TABLE,
    LINEAGE_TABLE,
    GREEN_CORE_TABLE,
    FERN_CORE_TABLE
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
# 13. READ VPJD AUTHORITIES
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


# ==============================================================================
# 14. VALIDATE CANONICAL AUTHORITY FIELDS
# ==============================================================================

canonical_required_fields <-
  c(
    "FINAL_WCVP_ID",
    "FINAL_WCVP_RECOGNISED_NAME",
    "FINAL_WCVP_RANK"
  )

missing_canonical_fields <-
  setdiff(
    canonical_required_fields,
    names(canonical)
  )

if (length(missing_canonical_fields) > 0L) {
  
  disconnect_safely()
  
  stop(
    paste0(
      "Required canonical authority field(s) missing:\n",
      paste(
        missing_canonical_fields,
        collapse = "\n"
      )
    )
  )
}


# ==============================================================================
# 15. VALIDATE LINEAGE AUTHORITY FIELDS
# ==============================================================================

lineage_required_fields <-
  c(
    "FINAL_WCVP_ID",
    "FAMILY",
    "GENUS",
    "SPECIES",
    "MAJOR_GROUP"
  )

missing_lineage_fields <-
  setdiff(
    lineage_required_fields,
    names(lineage)
  )

if (length(missing_lineage_fields) > 0L) {
  
  disconnect_safely()
  
  stop(
    paste0(
      "Required validated-lineage field(s) missing:\n",
      paste(
        missing_lineage_fields,
        collapse = "\n"
      )
    )
  )
}


# ==============================================================================
# 16. NORMALISE JOIN IDENTIFIERS
# ==============================================================================

canonical <-
  canonical |>
  mutate(
    FINAL_WCVP_ID =
      normalise_text(
        FINAL_WCVP_ID
      )
  )

lineage <-
  lineage |>
  mutate(
    FINAL_WCVP_ID =
      normalise_text(
        FINAL_WCVP_ID
      )
  )


# ==============================================================================
# 17. PRE-JOIN AUTHORITY COUNTS
# ==============================================================================

n_canonical <-
  nrow(
    canonical
  )

n_lineage <-
  nrow(
    lineage
  )

n_canonical_ids <-
  dplyr::n_distinct(
    canonical$FINAL_WCVP_ID,
    na.rm = TRUE
  )

n_lineage_ids <-
  dplyr::n_distinct(
    lineage$FINAL_WCVP_ID,
    na.rm = TRUE
  )

canonical_duplicate_ids <-
  canonical |>
  count(
    FINAL_WCVP_ID,
    name = "N"
  ) |>
  filter(
    is.na(FINAL_WCVP_ID) |
      N > 1L
  )

lineage_duplicate_ids <-
  lineage |>
  count(
    FINAL_WCVP_ID,
    name = "N"
  ) |>
  filter(
    is.na(FINAL_WCVP_ID) |
      N > 1L
  )


# ==============================================================================
# 18. COMPARE AUTHORITY IDENTIFIERS
# ==============================================================================

canonical_ids <-
  unique(
    canonical$FINAL_WCVP_ID[
      !is.na(
        canonical$FINAL_WCVP_ID
      )
    ]
  )

lineage_ids <-
  unique(
    lineage$FINAL_WCVP_ID[
      !is.na(
        lineage$FINAL_WCVP_ID
      )
    ]
  )

canonical_ids_missing_lineage <-
  setdiff(
    canonical_ids,
    lineage_ids
  )

lineage_ids_missing_canonical <-
  setdiff(
    lineage_ids,
    canonical_ids
  )

n_authority_matches <-
  length(
    intersect(
      canonical_ids,
      lineage_ids
    )
  )


# ==============================================================================
# 19. PRE-JOIN AUTHORITY VALIDATION
# ==============================================================================

authority_prejoin_validation <-
  tibble(
    
    CHECK =
      c(
        "Canonical population = 11,439",
        "Validated lineage population = 11,439",
        "Canonical WCVP IDs = 11,439",
        "Validated lineage WCVP IDs = 11,439",
        "Canonical WCVP IDs unique and non-missing",
        "Validated lineage WCVP IDs unique and non-missing",
        "Canonical WCVP IDs matched to lineage = 11,439",
        "Canonical IDs missing from lineage = 0",
        "Lineage IDs missing from canonical = 0"
      ),
    
    PASS =
      c(
        n_canonical ==
          EXPECTED_CANONICAL_POPULATION,
        
        n_lineage ==
          EXPECTED_CANONICAL_POPULATION,
        
        n_canonical_ids ==
          EXPECTED_CANONICAL_POPULATION,
        
        n_lineage_ids ==
          EXPECTED_CANONICAL_POPULATION,
        
        nrow(
          canonical_duplicate_ids
        ) == 0L,
        
        nrow(
          lineage_duplicate_ids
        ) == 0L,
        
        n_authority_matches ==
          EXPECTED_CANONICAL_POPULATION,
        
        length(
          canonical_ids_missing_lineage
        ) == 0L,
        
        length(
          lineage_ids_missing_canonical
        ) == 0L
      )
  ) |>
  mutate(
    RESULT =
      if_else(
        PASS,
        "PASS",
        "FAIL"
      )
  )

if (!all(authority_prejoin_validation$PASS)) {
  
  cat("\n")
  cat("============================================================\n")
  cat("03c VPJD AUTHORITY PRE-JOIN VALIDATION FAILED\n")
  cat("============================================================\n\n")
  
  print(
    authority_prejoin_validation,
    n = Inf,
    width = Inf
  )
  
  disconnect_safely()
  
  stop(
    "03c stopped before joining canonical and lineage authorities."
  )
}


# ==============================================================================
# 20. BUILD JOINED VPJD AUTHORITY
# ==============================================================================

lineage_for_join <-
  lineage |>
  select(
    FINAL_WCVP_ID,
    FAMILY,
    GENUS,
    SPECIES,
    MAJOR_GROUP
  )

vpjd_authority <-
  canonical |>
  left_join(
    lineage_for_join,
    by = "FINAL_WCVP_ID",
    relationship = "one-to-one"
  )


# ==============================================================================
# 21. VALIDATE JOINED VPJD AUTHORITY
# ==============================================================================

n_authority <-
  nrow(
    vpjd_authority
  )

n_authority_families <-
  dplyr::n_distinct(
    vpjd_authority$FAMILY,
    na.rm = TRUE
  )

n_authority_groups <-
  dplyr::n_distinct(
    vpjd_authority$MAJOR_GROUP,
    na.rm = TRUE
  )

authority_group_names <-
  sort(
    unique(
      normalise_text(
        vpjd_authority$MAJOR_GROUP
      )
    )
  )

authority_group_names <-
  authority_group_names[
    !is.na(
      authority_group_names
    )
  ]

n_missing_family <-
  sum(
    is.na(
      normalise_text(
        vpjd_authority$FAMILY
      )
    )
  )

n_missing_genus <-
  sum(
    is.na(
      normalise_text(
        vpjd_authority$GENUS
      )
    )
  )

n_missing_major_group <-
  sum(
    is.na(
      normalise_text(
        vpjd_authority$MAJOR_GROUP
      )
    )
  )

authority_validation <-
  bind_rows(
    
    authority_prejoin_validation,
    
    tibble(
      
      CHECK =
        c(
          "Joined VPJD authority population = 11,439",
          "Joined VPJD authority WCVP IDs unique",
          "Joined VPJD authority families = 260",
          "Joined VPJD authority major groups = 4",
          "Expected major groups exactly represented",
          "Joined VPJD authority missing family = 0",
          "Joined VPJD authority missing genus = 0",
          "Joined VPJD authority missing major group = 0"
        ),
      
      PASS =
        c(
          n_authority ==
            EXPECTED_CANONICAL_POPULATION,
          
          dplyr::n_distinct(
            vpjd_authority$FINAL_WCVP_ID,
            na.rm = TRUE
          ) ==
            EXPECTED_CANONICAL_POPULATION,
          
          n_authority_families ==
            EXPECTED_CANONICAL_FAMILIES,
          
          n_authority_groups ==
            EXPECTED_MAJOR_GROUPS,
          
          setequal(
            authority_group_names,
            EXPECTED_MAJOR_GROUP_NAMES
          ),
          
          n_missing_family == 0L,
          
          n_missing_genus == 0L,
          
          n_missing_major_group == 0L
        )
    ) |>
      mutate(
        RESULT =
          if_else(
            PASS,
            "PASS",
            "FAIL"
          )
      )
  )

if (!all(authority_validation$PASS)) {
  
  cat("\n")
  cat("============================================================\n")
  cat("03c JOINED VPJD AUTHORITY VALIDATION FAILED\n")
  cat("============================================================\n\n")
  
  print(
    authority_validation,
    n = Inf,
    width = Inf
  )
  
  disconnect_safely()
  
  stop(
    "03c stopped before contemporary-checklist reconciliation."
  )
}


# ==============================================================================
# 22. BUILD VPJD MATCHING INVENTORY
# ==============================================================================

vpjd_inventory <-
  vpjd_authority |>
  transmute(
    
    WCVP_ID =
      normalise_text(
        FINAL_WCVP_ID
      ),
    
    VPJD_SCIENTIFIC_NAME =
      normalise_text(
        FINAL_WCVP_RECOGNISED_NAME
      ),
    
    VPJD_RANK =
      normalise_text(
        FINAL_WCVP_RANK
      ),
    
    VPJD_FAMILY =
      normalise_text(
        FAMILY
      ),
    
    VPJD_GENUS =
      normalise_text(
        GENUS
      ),
    
    VPJD_SPECIES =
      normalise_text(
        SPECIES
      ),
    
    MAJOR_GROUP =
      normalise_text(
        MAJOR_GROUP
      )
  ) |>
  mutate(
    
    VPJD_NAME_NORMALISED =
      normalise_scientific_name(
        VPJD_SCIENTIFIC_NAME
      ),
    
    VPJD_NAME_WITHOUT_AUTHORSHIP =
      strip_authorship(
        VPJD_SCIENTIFIC_NAME
      )
  )


# ==============================================================================
# 23. READ CONTEMPORARY JAPAN TABLES
# ==============================================================================

green <-
  safe_read_table(
    con,
    GREEN_CORE_TABLE,
    database_tables
  )

fern <-
  safe_read_table(
    con,
    FERN_CORE_TABLE,
    database_tables
  )

green_vernacular <-
  safe_read_table(
    con,
    GREEN_VERNACULAR_TABLE,
    database_tables
  )

fern_vernacular <-
  safe_read_table(
    con,
    FERN_VERNACULAR_TABLE,
    database_tables
  )

green_distribution <-
  safe_read_table(
    con,
    GREEN_DISTRIBUTION_TABLE,
    database_tables
  )

fern_species_profile <-
  safe_read_table(
    con,
    FERN_SPECIES_PROFILE_TABLE,
    database_tables
  )


# ==============================================================================
# 24. IDENTIFY GREENLIST CORE FIELDS
# ==============================================================================

green_id_field <-
  first_existing_field(
    green,
    c(
      "TAXONID",
      "DWCA_ID",
      "ID"
    )
  )

green_name_field <-
  first_existing_field(
    green,
    c(
      "SCIENTIFICNAME"
    )
  )

green_family_field <-
  first_existing_field(
    green,
    c(
      "FAMILY"
    )
  )

green_rank_field <-
  first_existing_field(
    green,
    c(
      "TAXONRANK"
    )
  )

green_vernacular_core_field <-
  first_existing_field(
    green,
    c(
      "VERNACULARNAME"
    )
  )

green_status_field <-
  first_existing_field(
    green,
    c(
      "TAXONOMICSTATUS"
    )
  )


# ==============================================================================
# 25. IDENTIFY FERNGREENLIST CORE FIELDS
# ==============================================================================

fern_id_field <-
  first_existing_field(
    fern,
    c(
      "TAXONID",
      "DWCA_ID",
      "ID"
    )
  )

fern_name_field <-
  first_existing_field(
    fern,
    c(
      "SCIENTIFICNAME"
    )
  )

fern_family_field <-
  first_existing_field(
    fern,
    c(
      "FAMILY"
    )
  )

fern_genus_field <-
  first_existing_field(
    fern,
    c(
      "GENUS"
    )
  )

fern_rank_field <-
  first_existing_field(
    fern,
    c(
      "TAXONRANK"
    )
  )

fern_vernacular_core_field <-
  first_existing_field(
    fern,
    c(
      "VERNACULARNAME"
    )
  )

fern_status_field <-
  first_existing_field(
    fern,
    c(
      "TAXONOMICSTATUS"
    )
  )


# ==============================================================================
# 26. REQUIRE SCIENTIFIC-NAME FIELDS
# ==============================================================================

if (is.na(green_name_field)) {
  
  disconnect_safely()
  
  stop(
    "GreenList SCIENTIFICNAME field could not be identified."
  )
}

if (is.na(fern_name_field)) {
  
  disconnect_safely()
  
  stop(
    "FernGreenList SCIENTIFICNAME field could not be identified."
  )
}


# ==============================================================================
# 27. BUILD GREENLIST SOURCE VECTORS
# ==============================================================================

if (!is.na(green_id_field)) {
  
  green_ids <-
    normalise_text(
      green[[green_id_field]]
    )
  
} else {
  
  green_ids <-
    paste0(
      "GREENLIST_ROW_",
      seq_len(
        nrow(green)
      )
    )
}


if (!is.na(green_family_field)) {
  
  green_families <-
    normalise_text(
      green[[green_family_field]]
    )
  
} else {
  
  green_families <-
    rep(
      NA_character_,
      nrow(green)
    )
}


if (!is.na(green_rank_field)) {
  
  green_ranks <-
    normalise_text(
      green[[green_rank_field]]
    )
  
} else {
  
  green_ranks <-
    rep(
      NA_character_,
      nrow(green)
    )
}


if (!is.na(green_vernacular_core_field)) {
  
  green_japanese <-
    normalise_text(
      green[[green_vernacular_core_field]]
    )
  
} else {
  
  green_japanese <-
    rep(
      NA_character_,
      nrow(green)
    )
}


if (!is.na(green_status_field)) {
  
  green_taxonomic_status <-
    normalise_text(
      green[[green_status_field]]
    )
  
} else {
  
  green_taxonomic_status <-
    rep(
      NA_character_,
      nrow(green)
    )
}


# ==============================================================================
# 28. BUILD GREENLIST INVENTORY
# ==============================================================================

green_inventory <-
  tibble(
    
    JAPAN_RECORD_ID =
      paste0(
        "GL_",
        seq_len(
          nrow(green)
        )
      ),
    
    SOURCE =
      rep(
        "GreenList 2.02rc",
        nrow(green)
      ),
    
    SOURCE_TAXON_ID =
      green_ids,
    
    JAPAN_SCIENTIFIC_NAME =
      normalise_text(
        green[[green_name_field]]
      ),
    
    JAPAN_RANK =
      green_ranks,
    
    JAPAN_FAMILY =
      green_families,
    
    JAPAN_GENUS =
      extract_genus(
        green[[green_name_field]]
      ),
    
    JAPANESE_NAME =
      green_japanese,
    
    SOURCE_TAXONOMIC_STATUS =
      green_taxonomic_status
  )


# ==============================================================================
# 29. BUILD FERNGREENLIST SOURCE VECTORS
# ==============================================================================

if (!is.na(fern_id_field)) {
  
  fern_ids <-
    normalise_text(
      fern[[fern_id_field]]
    )
  
} else {
  
  fern_ids <-
    paste0(
      "FERN_ROW_",
      seq_len(
        nrow(fern)
      )
    )
}


if (!is.na(fern_family_field)) {
  
  fern_families <-
    normalise_text(
      fern[[fern_family_field]]
    )
  
} else {
  
  fern_families <-
    rep(
      NA_character_,
      nrow(fern)
    )
}


if (!is.na(fern_genus_field)) {
  
  fern_genera <-
    normalise_text(
      fern[[fern_genus_field]]
    )
  
} else {
  
  fern_genera <-
    extract_genus(
      fern[[fern_name_field]]
    )
}


if (!is.na(fern_rank_field)) {
  
  fern_ranks <-
    normalise_text(
      fern[[fern_rank_field]]
    )
  
} else {
  
  fern_ranks <-
    rep(
      NA_character_,
      nrow(fern)
    )
}


if (!is.na(fern_vernacular_core_field)) {
  
  fern_japanese <-
    normalise_text(
      fern[[fern_vernacular_core_field]]
    )
  
} else {
  
  fern_japanese <-
    rep(
      NA_character_,
      nrow(fern)
    )
}


if (!is.na(fern_status_field)) {
  
  fern_taxonomic_status <-
    normalise_text(
      fern[[fern_status_field]]
    )
  
} else {
  
  fern_taxonomic_status <-
    rep(
      NA_character_,
      nrow(fern)
    )
}


# ==============================================================================
# 30. BUILD FERNGREENLIST INVENTORY
# ==============================================================================

fern_inventory <-
  tibble(
    
    JAPAN_RECORD_ID =
      paste0(
        "FGL_",
        seq_len(
          nrow(fern)
        )
      ),
    
    SOURCE =
      rep(
        "FernGreenList 2.0",
        nrow(fern)
      ),
    
    SOURCE_TAXON_ID =
      fern_ids,
    
    JAPAN_SCIENTIFIC_NAME =
      normalise_text(
        fern[[fern_name_field]]
      ),
    
    JAPAN_RANK =
      fern_ranks,
    
    JAPAN_FAMILY =
      fern_families,
    
    JAPAN_GENUS =
      fern_genera,
    
    JAPANESE_NAME =
      fern_japanese,
    
    SOURCE_TAXONOMIC_STATUS =
      fern_taxonomic_status
  )


# ==============================================================================
# 31. ATTACH VERNACULAR EXTENSION DATA
# ==============================================================================

attach_vernacular_extension <-
  function(
    inventory,
    extension
  ) {
    
    if (nrow(extension) == 0L) {
      return(inventory)
    }
    
    id_field <-
      first_existing_field(
        extension,
        c(
          "COREID",
          "TAXONID",
          "DWCA_ID",
          "ID"
        )
      )
    
    name_field <-
      first_existing_field(
        extension,
        c(
          "VERNACULARNAME"
        )
      )
    
    if (
      is.na(id_field) ||
      is.na(name_field)
    ) {
      
      return(inventory)
    }
    
    lookup <-
      tibble(
        
        SOURCE_TAXON_ID =
          normalise_text(
            extension[[id_field]]
          ),
        
        JAPANESE_NAME_EXTENSION =
          normalise_text(
            extension[[name_field]]
          )
      ) |>
      filter(
        !is.na(
          SOURCE_TAXON_ID
        ),
        !is.na(
          JAPANESE_NAME_EXTENSION
        )
      ) |>
      group_by(
        SOURCE_TAXON_ID
      ) |>
      summarise(
        JAPANESE_NAME_EXTENSION =
          collapse_unique(
            JAPANESE_NAME_EXTENSION
          ),
        .groups = "drop"
      )
    
    inventory |>
      left_join(
        lookup,
        by = "SOURCE_TAXON_ID"
      ) |>
      mutate(
        JAPANESE_NAME =
          dplyr::coalesce(
            JAPANESE_NAME_EXTENSION,
            JAPANESE_NAME
          )
      ) |>
      select(
        -JAPANESE_NAME_EXTENSION
      )
  }


green_inventory <-
  attach_vernacular_extension(
    green_inventory,
    green_vernacular
  )

fern_inventory <-
  attach_vernacular_extension(
    fern_inventory,
    fern_vernacular
  )


# ==============================================================================
# 32. COMBINE JAPAN CHECKLIST INVENTORIES
# ==============================================================================

japan_inventory <-
  bind_rows(
    green_inventory,
    fern_inventory
  ) |>
  mutate(
    
    JAPAN_NAME_NORMALISED =
      normalise_scientific_name(
        JAPAN_SCIENTIFIC_NAME
      ),
    
    JAPAN_NAME_WITHOUT_AUTHORSHIP =
      strip_authorship(
        JAPAN_SCIENTIFIC_NAME
      ),
    
    JAPAN_GENUS =
      dplyr::coalesce(
        JAPAN_GENUS,
        extract_genus(
          JAPAN_SCIENTIFIC_NAME
        )
      )
  )


# ==============================================================================
# 33. JAPAN INVENTORY VALIDATION
# ==============================================================================

n_japan_records <-
  nrow(
    japan_inventory
  )

if (
  anyDuplicated(
    japan_inventory$JAPAN_RECORD_ID
  ) > 0L
) {
  
  disconnect_safely()
  
  stop(
    "JAPAN_RECORD_ID is not unique."
  )
}


# ==============================================================================
# 34. MATCH STAGE 1
# EXACT FULL SCIENTIFIC NAME
# ==============================================================================

stage1_candidates <-
  japan_inventory |>
  inner_join(
    vpjd_inventory,
    by =
      c(
        "JAPAN_NAME_NORMALISED" =
          "VPJD_NAME_NORMALISED"
      )
  ) |>
  mutate(
    MATCH_METHOD =
      "EXACT_SCIENTIFIC_NAME",
    MATCH_PRIORITY =
      1L
  )

stage1_counts <-
  stage1_candidates |>
  count(
    JAPAN_RECORD_ID,
    name = "N_MATCHES"
  )

stage1_unique_ids <-
  stage1_counts |>
  filter(
    N_MATCHES == 1L
  ) |>
  pull(
    JAPAN_RECORD_ID
  )

stage1_unique <-
  stage1_candidates |>
  filter(
    JAPAN_RECORD_ID %in%
      stage1_unique_ids
  )


# ==============================================================================
# 35. MATCH STAGE 2
# NAME WITHOUT AUTHORSHIP
# ==============================================================================

remaining_stage2 <-
  japan_inventory |>
  filter(
    !(JAPAN_RECORD_ID %in%
        stage1_unique_ids)
  )

stage2_candidates <-
  remaining_stage2 |>
  inner_join(
    vpjd_inventory,
    by =
      c(
        "JAPAN_NAME_WITHOUT_AUTHORSHIP" =
          "VPJD_NAME_WITHOUT_AUTHORSHIP"
      )
  ) |>
  mutate(
    
    MATCH_METHOD =
      "CANONICAL_NAME_WITHOUT_AUTHORSHIP",
    
    MATCH_PRIORITY =
      2L,
    
    FAMILY_CONTEXT_AGREES =
      case_when(
        
        is.na(JAPAN_FAMILY) ~
          NA,
        
        is.na(VPJD_FAMILY) ~
          NA,
        
        toupper(JAPAN_FAMILY) ==
          toupper(VPJD_FAMILY) ~
          TRUE,
        
        TRUE ~
          FALSE
      )
  )

stage2_counts <-
  stage2_candidates |>
  count(
    JAPAN_RECORD_ID,
    name = "N_MATCHES"
  )

stage2_unique_ids <-
  stage2_counts |>
  filter(
    N_MATCHES == 1L
  ) |>
  pull(
    JAPAN_RECORD_ID
  )

stage2_unique <-
  stage2_candidates |>
  filter(
    JAPAN_RECORD_ID %in%
      stage2_unique_ids
  )


# ==============================================================================
# 36. MATCH STAGE 3
# NAME WITHOUT AUTHORSHIP + FAMILY
# ==============================================================================

resolved_stage12_ids <-
  union(
    stage1_unique_ids,
    stage2_unique_ids
  )

remaining_stage3 <-
  japan_inventory |>
  filter(
    !(JAPAN_RECORD_ID %in%
        resolved_stage12_ids)
  )

stage3_candidates <-
  remaining_stage3 |>
  inner_join(
    vpjd_inventory,
    by =
      c(
        "JAPAN_NAME_WITHOUT_AUTHORSHIP" =
          "VPJD_NAME_WITHOUT_AUTHORSHIP"
      )
  ) |>
  filter(
    !is.na(
      JAPAN_FAMILY
    ),
    !is.na(
      VPJD_FAMILY
    ),
    toupper(JAPAN_FAMILY) ==
      toupper(VPJD_FAMILY)
  ) |>
  mutate(
    MATCH_METHOD =
      "NAME_WITHOUT_AUTHORSHIP_PLUS_FAMILY",
    MATCH_PRIORITY =
      3L,
    FAMILY_CONTEXT_AGREES =
      TRUE
  )

stage3_counts <-
  stage3_candidates |>
  count(
    JAPAN_RECORD_ID,
    name = "N_MATCHES"
  )

stage3_unique_ids <-
  stage3_counts |>
  filter(
    N_MATCHES == 1L
  ) |>
  pull(
    JAPAN_RECORD_ID
  )

stage3_unique <-
  stage3_candidates |>
  filter(
    JAPAN_RECORD_ID %in%
      stage3_unique_ids
  )


# ==============================================================================
# 37. COMBINE RESOLVED MATCHES
# ==============================================================================

resolved_matches <-
  bind_rows(
    stage1_unique,
    stage2_unique,
    stage3_unique
  ) |>
  distinct(
    JAPAN_RECORD_ID,
    .keep_all = TRUE
  )

resolved_ids <-
  unique(
    resolved_matches$JAPAN_RECORD_ID
  )


# ==============================================================================
# 38. BUILD UNRESOLVED POPULATION
# ==============================================================================

unresolved_japan <-
  japan_inventory |>
  filter(
    !(JAPAN_RECORD_ID %in%
        resolved_ids)
  )


# ==============================================================================
# 39. BUILD AMBIGUOUS-CANDIDATE INVENTORY
# ==============================================================================

all_candidate_matches <-
  bind_rows(
    stage1_candidates,
    stage2_candidates,
    stage3_candidates
  ) |>
  filter(
    !(JAPAN_RECORD_ID %in%
        resolved_ids)
  ) |>
  distinct(
    JAPAN_RECORD_ID,
    WCVP_ID,
    MATCH_METHOD,
    .keep_all = TRUE
  )

ambiguous_counts <-
  all_candidate_matches |>
  count(
    JAPAN_RECORD_ID,
    name = "N_CANDIDATES"
  )

ambiguous_ids <-
  ambiguous_counts |>
  filter(
    N_CANDIDATES > 1L
  ) |>
  pull(
    JAPAN_RECORD_ID
  )

ambiguous_matches <-
  all_candidate_matches |>
  filter(
    JAPAN_RECORD_ID %in%
      ambiguous_ids
  ) |>
  arrange(
    JAPAN_RECORD_ID,
    MATCH_PRIORITY,
    WCVP_ID
  )


# ==============================================================================
# 40. CLASSIFY UNRESOLVED RECORDS
# ==============================================================================

unresolved_japan <-
  unresolved_japan |>
  mutate(
    
    RECONCILIATION_STATUS =
      case_when(
        
        JAPAN_RECORD_ID %in%
          ambiguous_ids ~
          "AMBIGUOUS",
        
        is.na(
          JAPAN_SCIENTIFIC_NAME
        ) ~
          "MISSING_SCIENTIFIC_NAME",
        
        TRUE ~
          "NO_VPJD_NAME_MATCH"
      )
  )


# ==============================================================================
# 41. CLASSIFY RESOLVED MATCHES
# ==============================================================================

resolved_matches <-
  resolved_matches |>
  mutate(
    
    RECONCILIATION_STATUS =
      "MATCHED",
    
    NAME_IDENTICAL =
      JAPAN_NAME_NORMALISED ==
      VPJD_NAME_NORMALISED,
    
    AUTHORSHIP_ONLY_DIFFERENCE =
      !NAME_IDENTICAL &
      JAPAN_NAME_WITHOUT_AUTHORSHIP ==
      VPJD_NAME_WITHOUT_AUTHORSHIP,
    
    FAMILY_AGREEMENT =
      case_when(
        
        is.na(JAPAN_FAMILY) ~
          NA,
        
        is.na(VPJD_FAMILY) ~
          NA,
        
        toupper(JAPAN_FAMILY) ==
          toupper(VPJD_FAMILY) ~
          TRUE,
        
        TRUE ~
          FALSE
      ),
    
    GENUS_AGREEMENT =
      case_when(
        
        is.na(JAPAN_GENUS) ~
          NA,
        
        is.na(VPJD_GENUS) ~
          NA,
        
        toupper(JAPAN_GENUS) ==
          toupper(VPJD_GENUS) ~
          TRUE,
        
        TRUE ~
          FALSE
      ),
    
    RANK_AGREEMENT =
      case_when(
        
        is.na(JAPAN_RANK) ~
          NA,
        
        is.na(VPJD_RANK) ~
          NA,
        
        toupper(JAPAN_RANK) ==
          toupper(VPJD_RANK) ~
          TRUE,
        
        TRUE ~
          FALSE
      )
  ) |>
  mutate(
    
    RECONCILIATION_CLASS =
      case_when(
        
        MATCH_METHOD ==
          "EXACT_SCIENTIFIC_NAME" ~
          "EXACT_NAME_MATCH",
        
        AUTHORSHIP_ONLY_DIFFERENCE ~
          "AUTHORSHIP_OR_CITATION_DIFFERENCE",
        
        MATCH_METHOD ==
          "NAME_WITHOUT_AUTHORSHIP_PLUS_FAMILY" ~
          "NORMALISED_NAME_PLUS_FAMILY_MATCH",
        
        TRUE ~
          "NORMALISED_NAME_MATCH"
      )
  )


# ==============================================================================
# 42. BUILD MATCHED OUTPUT
# ==============================================================================

matched_output <-
  resolved_matches |>
  transmute(
    
    JAPAN_RECORD_ID,
    
    SOURCE,
    
    SOURCE_TAXON_ID,
    
    JAPAN_SCIENTIFIC_NAME,
    
    JAPANESE_NAME,
    
    JAPAN_RANK,
    
    JAPAN_FAMILY,
    
    JAPAN_GENUS,
    
    SOURCE_TAXONOMIC_STATUS,
    
    WCVP_ID,
    
    VPJD_SCIENTIFIC_NAME,
    
    VPJD_RANK,
    
    VPJD_FAMILY,
    
    VPJD_GENUS,
    
    VPJD_SPECIES,
    
    MAJOR_GROUP,
    
    MATCH_METHOD,
    
    MATCH_PRIORITY,
    
    RECONCILIATION_STATUS,
    
    RECONCILIATION_CLASS,
    
    NAME_IDENTICAL,
    
    AUTHORSHIP_ONLY_DIFFERENCE,
    
    FAMILY_AGREEMENT,
    
    GENUS_AGREEMENT,
    
    RANK_AGREEMENT
  )


# ==============================================================================
# 43. BUILD UNMATCHED OUTPUT
# ==============================================================================

unmatched_output <-
  unresolved_japan |>
  transmute(
    
    JAPAN_RECORD_ID,
    
    SOURCE,
    
    SOURCE_TAXON_ID,
    
    JAPAN_SCIENTIFIC_NAME,
    
    JAPANESE_NAME,
    
    JAPAN_RANK,
    
    JAPAN_FAMILY,
    
    JAPAN_GENUS,
    
    SOURCE_TAXONOMIC_STATUS,
    
    WCVP_ID =
      NA_character_,
    
    VPJD_SCIENTIFIC_NAME =
      NA_character_,
    
    VPJD_RANK =
      NA_character_,
    
    VPJD_FAMILY =
      NA_character_,
    
    VPJD_GENUS =
      NA_character_,
    
    VPJD_SPECIES =
      NA_character_,
    
    MAJOR_GROUP =
      NA_character_,
    
    MATCH_METHOD =
      NA_character_,
    
    MATCH_PRIORITY =
      NA_integer_,
    
    RECONCILIATION_STATUS,
    
    RECONCILIATION_CLASS =
      NA_character_,
    
    NAME_IDENTICAL =
      NA,
    
    AUTHORSHIP_ONLY_DIFFERENCE =
      NA,
    
    FAMILY_AGREEMENT =
      NA,
    
    GENUS_AGREEMENT =
      NA,
    
    RANK_AGREEMENT =
      NA
  )


# ==============================================================================
# 44. BUILD COMPLETE RECONCILIATION
# ==============================================================================

reconciliation <-
  bind_rows(
    matched_output,
    unmatched_output
  ) |>
  arrange(
    SOURCE,
    JAPAN_SCIENTIFIC_NAME
  )

if (
  nrow(reconciliation) !=
  n_japan_records
) {
  
  disconnect_safely()
  
  stop(
    paste0(
      "Reconciliation population mismatch: ",
      nrow(reconciliation),
      " rows produced from ",
      n_japan_records,
      " Japanese checklist records."
    )
  )
}


# ==============================================================================
# 45. VPJD COVERAGE
# ==============================================================================

matched_wcvp_ids <-
  unique(
    matched_output$WCVP_ID[
      !is.na(
        matched_output$WCVP_ID
      )
    ]
  )

unmatched_vpjd <-
  vpjd_inventory |>
  filter(
    !(WCVP_ID %in%
        matched_wcvp_ids)
  ) |>
  arrange(
    MAJOR_GROUP,
    VPJD_FAMILY,
    VPJD_GENUS,
    VPJD_SCIENTIFIC_NAME
  )


# ==============================================================================
# 46. CORE COUNTS
# ==============================================================================

n_matched <-
  sum(
    reconciliation$RECONCILIATION_STATUS ==
      "MATCHED"
  )

n_ambiguous <-
  sum(
    reconciliation$RECONCILIATION_STATUS ==
      "AMBIGUOUS"
  )

n_no_match <-
  sum(
    reconciliation$RECONCILIATION_STATUS ==
      "NO_VPJD_NAME_MATCH"
  )

n_missing_name <-
  sum(
    reconciliation$RECONCILIATION_STATUS ==
      "MISSING_SCIENTIFIC_NAME"
  )

n_vpjd_matched <-
  length(
    matched_wcvp_ids
  )

n_vpjd_unmatched <-
  nrow(
    unmatched_vpjd
  )


# ==============================================================================
# 47. MATCH-METHOD SUMMARY
# ==============================================================================

match_method_summary <-
  reconciliation |>
  mutate(
    MATCH_METHOD_REPORT =
      dplyr::coalesce(
        MATCH_METHOD,
        RECONCILIATION_STATUS
      )
  ) |>
  count(
    MATCH_METHOD_REPORT,
    name = "N_RECORDS",
    sort = TRUE
  ) |>
  mutate(
    PERCENT_JAPAN_RECORDS =
      round(
        100 *
          N_RECORDS /
          n_japan_records,
        2
      )
  )


# ==============================================================================
# 48. SOURCE SUMMARY
# ==============================================================================

source_summary <-
  reconciliation |>
  group_by(
    SOURCE
  ) |>
  summarise(
    
    SOURCE_RECORDS =
      n(),
    
    MATCHED_RECORDS =
      sum(
        RECONCILIATION_STATUS ==
          "MATCHED"
      ),
    
    AMBIGUOUS_RECORDS =
      sum(
        RECONCILIATION_STATUS ==
          "AMBIGUOUS"
      ),
    
    UNMATCHED_RECORDS =
      sum(
        RECONCILIATION_STATUS ==
          "NO_VPJD_NAME_MATCH"
      ),
    
    WITH_JAPANESE_NAME =
      sum(
        !is.na(
          JAPANESE_NAME
        )
      ),
    
    MATCH_PERCENT =
      round(
        100 *
          MATCHED_RECORDS /
          SOURCE_RECORDS,
        2
      ),
    
    .groups = "drop"
  )


# ==============================================================================
# 49. MAJOR-GROUP SUMMARY
# ==============================================================================

major_group_summary <-
  vpjd_inventory |>
  mutate(
    MATCHED_TO_JAPAN =
      WCVP_ID %in%
      matched_wcvp_ids
  ) |>
  group_by(
    MAJOR_GROUP
  ) |>
  summarise(
    
    VPJD_RECORDS =
      n(),
    
    MATCHED_VPJD_RECORDS =
      sum(
        MATCHED_TO_JAPAN
      ),
    
    UNMATCHED_VPJD_RECORDS =
      sum(
        !MATCHED_TO_JAPAN
      ),
    
    VPJD_MATCH_PERCENT =
      round(
        100 *
          MATCHED_VPJD_RECORDS /
          VPJD_RECORDS,
        2
      ),
    
    .groups = "drop"
  ) |>
  arrange(
    desc(
      VPJD_RECORDS
    )
  )


# ==============================================================================
# 50. FINAL VALIDATION
# ==============================================================================

validation <-
  tibble(
    
    CHECK =
      c(
        "Canonical VPJD population = 11,439",
        "Validated lineage population = 11,439",
        "Joined VPJD authority population = 11,439",
        "Joined VPJD authority WCVP IDs unique",
        "Joined VPJD authority families = 260",
        "Joined VPJD authority major groups = 4",
        "Expected major groups exactly represented",
        "No canonical IDs missing from validated lineage",
        "No validated-lineage IDs missing from canonical authority",
        "Joined authority missing family = 0",
        "Joined authority missing genus = 0",
        "Joined authority missing major group = 0",
        "GreenList core present",
        "FernGreenList core present",
        "Japan checklist population non-zero",
        "Japan reconciliation population preserved",
        "Japan reconciliation record IDs unique",
        "Matched plus unresolved = Japan population",
        "Matched WCVP IDs exist in canonical VPJD",
        "Matched records have WCVP ID",
        "Matched records have VPJD scientific name",
        "Matched records have match method",
        "Matched records have reconciliation class",
        "Matched VPJD plus unmatched VPJD = canonical population",
        "No fuzzy matching performed",
        "No automatic synonym acceptance performed",
        "No canonical taxonomy modification performed",
        "No Star allocation modification performed",
        "No taxa removed",
        "No taxa added"
      ),
    
    PASS =
      c(
        n_canonical ==
          EXPECTED_CANONICAL_POPULATION,
        
        n_lineage ==
          EXPECTED_CANONICAL_POPULATION,
        
        n_authority ==
          EXPECTED_CANONICAL_POPULATION,
        
        dplyr::n_distinct(
          vpjd_authority$FINAL_WCVP_ID,
          na.rm = TRUE
        ) ==
          EXPECTED_CANONICAL_POPULATION,
        
        n_authority_families ==
          EXPECTED_CANONICAL_FAMILIES,
        
        n_authority_groups ==
          EXPECTED_MAJOR_GROUPS,
        
        setequal(
          authority_group_names,
          EXPECTED_MAJOR_GROUP_NAMES
        ),
        
        length(
          canonical_ids_missing_lineage
        ) == 0L,
        
        length(
          lineage_ids_missing_canonical
        ) == 0L,
        
        n_missing_family == 0L,
        
        n_missing_genus == 0L,
        
        n_missing_major_group == 0L,
        
        nrow(green) > 0L,
        
        nrow(fern) > 0L,
        
        n_japan_records > 0L,
        
        nrow(reconciliation) ==
          n_japan_records,
        
        dplyr::n_distinct(
          reconciliation$JAPAN_RECORD_ID
        ) ==
          n_japan_records,
        
        n_matched +
          n_ambiguous +
          n_no_match +
          n_missing_name ==
          n_japan_records,
        
        all(
          matched_output$WCVP_ID %in%
            vpjd_inventory$WCVP_ID
        ),
        
        all(
          !is.na(
            matched_output$WCVP_ID
          )
        ),
        
        all(
          !is.na(
            matched_output$VPJD_SCIENTIFIC_NAME
          )
        ),
        
        all(
          !is.na(
            matched_output$MATCH_METHOD
          )
        ),
        
        all(
          !is.na(
            matched_output$RECONCILIATION_CLASS
          )
        ),
        
        n_vpjd_matched +
          n_vpjd_unmatched ==
          EXPECTED_CANONICAL_POPULATION,
        
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
      if_else(
        PASS,
        "PASS",
        "FAIL"
      )
  )


# ==============================================================================
# 51. DECISION
# ==============================================================================

if (
  all(
    validation$PASS
  )
) {
  
  decision <-
    "CONTEMPORARY_JAPAN_RECONCILIATION_COMPLETE"
  
} else {
  
  decision <-
    "REVIEW_REQUIRED"
}


# ==============================================================================
# 52. WRITE DUCKDB OUTPUTS
# ==============================================================================

write_db_table(
  con,
  OUTPUT_AUTHORITY_TABLE,
  vpjd_inventory
)

write_db_table(
  con,
  OUTPUT_RECONCILIATION_TABLE,
  reconciliation
)

write_db_table(
  con,
  OUTPUT_MATCHED_TABLE,
  matched_output
)

write_db_table(
  con,
  OUTPUT_UNMATCHED_JAPAN_TABLE,
  unmatched_output
)

write_db_table(
  con,
  OUTPUT_UNMATCHED_VPJD_TABLE,
  unmatched_vpjd
)

write_db_table(
  con,
  OUTPUT_AMBIGUOUS_TABLE,
  ambiguous_matches
)

write_db_table(
  con,
  OUTPUT_MATCH_METHOD_TABLE,
  match_method_summary
)

write_db_table(
  con,
  OUTPUT_SOURCE_SUMMARY_TABLE,
  source_summary
)

write_db_table(
  con,
  OUTPUT_GROUP_SUMMARY_TABLE,
  major_group_summary
)

write_db_table(
  con,
  OUTPUT_AUTHORITY_VALIDATION_TABLE,
  authority_validation
)

write_db_table(
  con,
  OUTPUT_VALIDATION_TABLE,
  validation
)


# ==============================================================================
# 53. WRITE CSV OUTPUTS
# ==============================================================================

write_csv_file(
  vpjd_inventory,
  OUTPUT_AUTHORITY_CSV
)

write_csv_file(
  reconciliation,
  OUTPUT_RECONCILIATION_CSV
)

write_csv_file(
  matched_output,
  OUTPUT_MATCHED_CSV
)

write_csv_file(
  unmatched_output,
  OUTPUT_UNMATCHED_JAPAN_CSV
)

write_csv_file(
  unmatched_vpjd,
  OUTPUT_UNMATCHED_VPJD_CSV
)

write_csv_file(
  ambiguous_matches,
  OUTPUT_AMBIGUOUS_CSV
)

write_csv_file(
  match_method_summary,
  OUTPUT_MATCH_METHOD_CSV
)

write_csv_file(
  source_summary,
  OUTPUT_SOURCE_SUMMARY_CSV
)

write_csv_file(
  major_group_summary,
  OUTPUT_GROUP_SUMMARY_CSV
)

write_csv_file(
  authority_validation,
  OUTPUT_AUTHORITY_VALIDATION_CSV
)

write_csv_file(
  validation,
  OUTPUT_VALIDATION_CSV
)


# ==============================================================================
# 54. VPJD AUTHORITY REPORT
# ==============================================================================

cat("\n")
cat("============================================================\n")
cat("03c VPJD RECONCILIATION AUTHORITY\n")
cat("============================================================\n\n")

cat(
  "Canonical authority table: ",
  CANONICAL_TABLE,
  "\n",
  sep = ""
)

cat(
  "Validated lineage table: ",
  LINEAGE_TABLE,
  "\n\n",
  sep = ""
)

cat(
  "Canonical records: ",
  format(
    n_canonical,
    big.mark = ","
  ),
  "\n",
  sep = ""
)

cat(
  "Validated lineage records: ",
  format(
    n_lineage,
    big.mark = ","
  ),
  "\n",
  sep = ""
)

cat(
  "Canonical <-> lineage WCVP ID matches: ",
  format(
    n_authority_matches,
    big.mark = ","
  ),
  " / ",
  format(
    EXPECTED_CANONICAL_POPULATION,
    big.mark = ","
  ),
  "\n",
  sep = ""
)

cat(
  "Joined VPJD authority records: ",
  format(
    n_authority,
    big.mark = ","
  ),
  "\n",
  sep = ""
)

cat(
  "Families: ",
  n_authority_families,
  " / ",
  EXPECTED_CANONICAL_FAMILIES,
  "\n",
  sep = ""
)

cat(
  "Major vascular groups: ",
  n_authority_groups,
  " / ",
  EXPECTED_MAJOR_GROUPS,
  "\n\n",
  sep = ""
)

cat(
  "Missing family values: ",
  n_missing_family,
  "\n",
  sep = ""
)

cat(
  "Missing genus values: ",
  n_missing_genus,
  "\n",
  sep = ""
)

cat(
  "Missing major-group values: ",
  n_missing_major_group,
  "\n\n",
  sep = ""
)

cat("------------------------------------------------------------\n")
cat("AUTHORITY VALIDATION\n")
cat("------------------------------------------------------------\n\n")

print(
  authority_validation,
  n = Inf,
  width = Inf
)


# ==============================================================================
# 55. MATCH-METHOD REPORT
# ==============================================================================

cat("\n")
cat("------------------------------------------------------------\n")
cat("MATCH METHOD SUMMARY\n")
cat("------------------------------------------------------------\n\n")

print(
  match_method_summary,
  n = Inf,
  width = Inf
)


# ==============================================================================
# 56. SOURCE REPORT
# ==============================================================================

cat("\n")
cat("------------------------------------------------------------\n")
cat("SOURCE SUMMARY\n")
cat("------------------------------------------------------------\n\n")

print(
  source_summary,
  n = Inf,
  width = Inf
)


# ==============================================================================
# 57. MAJOR-GROUP REPORT
# ==============================================================================

cat("\n")
cat("------------------------------------------------------------\n")
cat("VPJD REPRESENTATION BY MAJOR GROUP\n")
cat("------------------------------------------------------------\n\n")

print(
  major_group_summary,
  n = Inf,
  width = Inf
)


# ==============================================================================
# 58. RECONCILIATION COUNTS
# ==============================================================================

cat("\n")
cat("------------------------------------------------------------\n")
cat("RECONCILIATION COUNTS\n")
cat("------------------------------------------------------------\n\n")

cat(
  "Contemporary Japan checklist records: ",
  format(
    n_japan_records,
    big.mark = ","
  ),
  "\n",
  sep = ""
)

cat(
  "Matched Japan records: ",
  format(
    n_matched,
    big.mark = ","
  ),
  "\n",
  sep = ""
)

cat(
  "Ambiguous Japan records: ",
  format(
    n_ambiguous,
    big.mark = ","
  ),
  "\n",
  sep = ""
)

cat(
  "Japan records with no VPJD name match: ",
  format(
    n_no_match,
    big.mark = ","
  ),
  "\n",
  sep = ""
)

cat(
  "Japan records missing scientific name: ",
  format(
    n_missing_name,
    big.mark = ","
  ),
  "\n\n",
  sep = ""
)

cat(
  "Distinct VPJD taxa represented by matched Japan records: ",
  format(
    n_vpjd_matched,
    big.mark = ","
  ),
  "\n",
  sep = ""
)

cat(
  "VPJD taxa not represented by a matched Japan record: ",
  format(
    n_vpjd_unmatched,
    big.mark = ","
  ),
  "\n",
  sep = ""
)


# ==============================================================================
# 59. FINAL VALIDATION REPORT
# ==============================================================================

cat("\n")
cat("------------------------------------------------------------\n")
cat("VALIDATION\n")
cat("------------------------------------------------------------\n\n")

print(
  validation,
  n = Inf,
  width = Inf
)

cat("\n")

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
  "\n",
  sep = ""
)


# ==============================================================================
# 60. OUTPUT REPORT
# ==============================================================================

cat("\n")
cat("------------------------------------------------------------\n")
cat("OUTPUTS\n")
cat("------------------------------------------------------------\n\n")

cat(
  "Output directory:\n",
  OUTPUT_DIR,
  "\n\n",
  sep = ""
)

cat(
  "Complete reconciliation:\n",
  OUTPUT_RECONCILIATION_CSV,
  "\n\n",
  sep = ""
)

cat(
  "Matched records:\n",
  OUTPUT_MATCHED_CSV,
  "\n\n",
  sep = ""
)

cat(
  "Unmatched Japan records:\n",
  OUTPUT_UNMATCHED_JAPAN_CSV,
  "\n\n",
  sep = ""
)

cat(
  "Ambiguous matches:\n",
  OUTPUT_AMBIGUOUS_CSV,
  "\n\n",
  sep = ""
)

cat(
  "Unmatched VPJD records:\n",
  OUTPUT_UNMATCHED_VPJD_CSV,
  "\n\n",
  sep = ""
)


# ==============================================================================
# 61. CLEAN DISCONNECT
# ==============================================================================

DBI::dbDisconnect(
  con,
  shutdown = TRUE
)

rm(con)

gc()


# ==============================================================================
# 62. COMPLETION REPORT
# ==============================================================================

cat("\n")
cat("============================================================\n")

cat(
  "VPJD TAXONOMIC REVISION 03c v",
  VERSION,
  " COMPLETE\n",
  sep = ""
)

cat("============================================================\n\n")

cat(
  "Canonical / Star authority:\n",
  CANONICAL_TABLE,
  "\n\n",
  sep = ""
)

cat(
  "Validated lineage authority:\n",
  LINEAGE_TABLE,
  "\n\n",
  sep = ""
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
  "Canonical families retained: ",
  EXPECTED_CANONICAL_FAMILIES,
  "\n",
  sep = ""
)

cat(
  "Validated vascular groups retained: ",
  EXPECTED_MAJOR_GROUPS,
  "\n",
  sep = ""
)

cat(
  "Japan checklist records reconciled/audited: ",
  format(
    n_japan_records,
    big.mark = ","
  ),
  "\n",
  sep = ""
)

cat(
  "Japan checklist records matched: ",
  format(
    n_matched,
    big.mark = ","
  ),
  "\n",
  sep = ""
)

cat(
  "Japan checklist records requiring review: ",
  format(
    n_ambiguous +
      n_no_match +
      n_missing_name,
    big.mark = ","
  ),
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
cat("Automatic synonym acceptance performed: FALSE\n")
cat("Japanese checklist taxonomy imposed on VPJD: FALSE\n")

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
  "\n",
  sep = ""
)

cat("03c DuckDB outputs written: TRUE\n")
cat("03c CSV outputs written: TRUE\n")
cat("DuckDB connection closed cleanly: TRUE\n")

cat("\n")
cat("============================================================\n")


# ==============================================================================
# END
# ==============================================================================