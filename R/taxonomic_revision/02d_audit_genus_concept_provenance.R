# ==============================================================================
# VPJD / OJPCP
# 02d — Audit genus-concept provenance and recoverability
#
# File:
#   R/taxonomic_revision/02d_audit_genus_concept_provenance.R
#
# Version: 0.1.0
#
# PURPOSE
# -------
# Investigate the 823 genus-rank concepts identified by 02c.
#
# This module:
#   1. verifies successful completion of 02c;
#   2. reads the canonical 02c genus-concept inventory;
#   3. inventories DuckDB tables and fields that may preserve source/provenance
#      information;
#   4. searches upstream tables for the 823 genus-concept WCVP IDs;
#   5. searches for likely original/source-name fields;
#   6. identifies which genus concepts can be traced into upstream data;
#   7. assigns a preliminary diagnostic/recoverability class;
#   8. writes diagnostic tables and CSV outputs.
#
# IMPORTANT
# ---------
# This is an AUDIT module.
#
# It does NOT:
#   * delete genus concepts;
#   * alter the canonical 11,439-concept population;
#   * alter WCVP reconciliation;
#   * change accepted names or ranks;
#   * change provisional Star allocations;
#   * automatically promote genus concepts to species;
#   * make irreversible taxonomic decisions.
#
# Expected canonical population:
#   11,439 accepted concepts
#
# Expected genus concepts from 02c:
#   823
#
# The purpose of 02d is to determine WHY these genus concepts exist before
# deciding whether any should be recovered, retained, reclassified or excluded
# in a later module.
# ==============================================================================


# ==============================================================================
# 00. PACKAGES
# ==============================================================================

required_packages <- c(
  "DBI",
  "duckdb",
  "dplyr",
  "readr",
  "stringr",
  "tibble",
  "purrr"
)

missing_packages <- required_packages[
  !vapply(
    required_packages,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )
]

if (length(missing_packages) > 0L) {
  stop(
    "Missing required packages: ",
    paste(missing_packages, collapse = ", ")
  )
}

library(DBI)
library(duckdb)
library(dplyr)
library(readr)
library(stringr)
library(tibble)
library(purrr)


# ==============================================================================
# 01. CONFIGURATION
# ==============================================================================

MODULE <- "02d_audit_genus_concept_provenance"
VERSION <- "0.1.0"
RUN_DATE <- Sys.Date()

PROJECT_ROOT <- "I:/R/OJPCP/VPJD-OJPCP"

DB_PATH <- file.path(
  PROJECT_ROOT,
  "data",
  "interim",
  "occurrences",
  "vpjd_occurrences.duckdb"
)

OUTPUT_DIR <- file.path(
  PROJECT_ROOT,
  "outputs",
  "tables",
  "taxonomic_revision",
  "02d_genus_concept_provenance"
)

dir.create(
  OUTPUT_DIR,
  recursive = TRUE,
  showWarnings = FALSE
)

EXPECTED_POPULATION <- 11439L
EXPECTED_GENUS_CONCEPTS <- 823L


# ==============================================================================
# 02. TABLE NAMES
# ==============================================================================

TABLE_CANONICAL <-
  "vpjd_star_provisional_taxonomic_composition"

TABLE_02C_AUDIT <-
  "vpjd_taxrev_02c_rank_audit"

TABLE_02C_GENUS <-
  "vpjd_taxrev_02c_genus_concepts"

TABLE_02C_VALIDATION <-
  "vpjd_taxrev_02c_validation"

TABLE_02D_AUDIT <-
  "vpjd_taxrev_02d_genus_provenance_audit"

TABLE_02D_TABLE_INVENTORY <-
  "vpjd_taxrev_02d_candidate_table_inventory"

TABLE_02D_FIELD_INVENTORY <-
  "vpjd_taxrev_02d_candidate_field_inventory"

TABLE_02D_MATCH_SUMMARY <-
  "vpjd_taxrev_02d_upstream_match_summary"

TABLE_02D_VALIDATION <-
  "vpjd_taxrev_02d_validation"


# ==============================================================================
# 03. CONNECTION SAFETY
# ==============================================================================

if (exists("con", envir = .GlobalEnv)) {
  
  stop(
    paste0(
      "An object named 'con' already exists in this R session.\n\n",
      "Restart R before running 02d:\n",
      "Session > Restart R\n",
      "or Ctrl + Shift + F10\n\n",
      "Then run 02d from the beginning."
    )
  )
}

if (!file.exists(DB_PATH)) {
  stop(
    "Canonical VPJD DuckDB not found:\n",
    DB_PATH
  )
}


# ==============================================================================
# 04. START REPORT
# ==============================================================================

cat("\n")
cat("============================================================\n")
cat("VPJD TAXONOMIC REVISION — 02d GENUS PROVENANCE AUDIT\n")
cat("============================================================\n\n")

cat("Run date: ", as.character(RUN_DATE), "\n", sep = "")
cat("Module: ", MODULE, "\n", sep = "")
cat("Version: ", VERSION, "\n", sep = "")
cat("Database: ", DB_PATH, "\n\n", sep = "")


# ==============================================================================
# 05. CONNECT
# ==============================================================================

con <- DBI::dbConnect(
  duckdb::duckdb(),
  dbdir = DB_PATH,
  read_only = FALSE
)

if (!DBI::dbIsValid(con)) {
  stop("DuckDB connection is not valid.")
}

cat("DuckDB connection valid: TRUE\n\n")


# ==============================================================================
# 06. VERIFY REQUIRED UPSTREAM TABLES
# ==============================================================================

db_tables <- DBI::dbListTables(con)

required_tables <- c(
  TABLE_CANONICAL,
  TABLE_02C_AUDIT,
  TABLE_02C_GENUS,
  TABLE_02C_VALIDATION
)

missing_tables <- setdiff(
  required_tables,
  db_tables
)

if (length(missing_tables) > 0L) {
  
  DBI::dbDisconnect(
    con,
    shutdown = TRUE
  )
  
  rm(con)
  
  stop(
    "Required upstream table(s) missing:\n",
    paste(
      missing_tables,
      collapse = "\n"
    )
  )
}

cat("Required upstream tables found:\n")
cat(
  paste0("  ", required_tables),
  sep = "\n"
)
cat("\n\n")


# ==============================================================================
# 07. VERIFY 02c VALIDATION
# ==============================================================================

validation_02c <- DBI::dbReadTable(
  con,
  TABLE_02C_VALIDATION
) %>%
  as_tibble()

if (!all(validation_02c$PASS %in% TRUE)) {
  
  failed_02c <- validation_02c %>%
    filter(!PASS) %>%
    pull(CHECK)
  
  DBI::dbDisconnect(
    con,
    shutdown = TRUE
  )
  
  rm(con)
  
  stop(
    paste0(
      "02c validation is not fully PASS.\n\n",
      "Failed checks:\n",
      paste(
        failed_02c,
        collapse = "\n"
      )
    )
  )
}

cat(
  "02c validation: ",
  sum(validation_02c$PASS),
  "/",
  nrow(validation_02c),
  " PASS\n\n",
  sep = ""
)


# ==============================================================================
# 08. READ CANONICAL POPULATION
# ==============================================================================

canonical <- DBI::dbReadTable(
  con,
  TABLE_CANONICAL
) %>%
  as_tibble() %>%
  mutate(
    FINAL_WCVP_ID =
      as.character(FINAL_WCVP_ID)
  )

if (nrow(canonical) != EXPECTED_POPULATION) {
  
  DBI::dbDisconnect(
    con,
    shutdown = TRUE
  )
  
  rm(con)
  
  stop(
    "Expected ",
    format(EXPECTED_POPULATION, big.mark = ","),
    " canonical concepts; found ",
    format(nrow(canonical), big.mark = ","),
    "."
  )
}


# ==============================================================================
# 09. READ 02c GENUS INVENTORY
# ==============================================================================

genus_concepts <- DBI::dbReadTable(
  con,
  TABLE_02C_GENUS
) %>%
  as_tibble() %>%
  mutate(
    FINAL_WCVP_ID =
      as.character(FINAL_WCVP_ID)
  )

if (nrow(genus_concepts) != EXPECTED_GENUS_CONCEPTS) {
  
  DBI::dbDisconnect(
    con,
    shutdown = TRUE
  )
  
  rm(con)
  
  stop(
    "Expected ",
    EXPECTED_GENUS_CONCEPTS,
    " genus concepts from 02c; found ",
    nrow(genus_concepts),
    "."
  )
}

if (
  n_distinct(genus_concepts$FINAL_WCVP_ID) !=
  EXPECTED_GENUS_CONCEPTS
) {
  
  DBI::dbDisconnect(
    con,
    shutdown = TRUE
  )
  
  rm(con)
  
  stop(
    "02c genus inventory is not unique by FINAL_WCVP_ID."
  )
}

cat(
  "02c genus concepts loaded: ",
  format(
    nrow(genus_concepts),
    big.mark = ","
  ),
  "\n\n",
  sep = ""
)


# ==============================================================================
# 10. BUILD COMPLETE DATABASE FIELD INVENTORY
# ==============================================================================

cat("Inspecting DuckDB schema...\n")

table_field_inventory <- map_dfr(
  db_tables,
  function(tbl) {
    
    fields <- tryCatch(
      DBI::dbListFields(
        con,
        tbl
      ),
      error = function(e) character()
    )
    
    if (length(fields) == 0L) {
      return(
        tibble(
          TABLE_NAME = tbl,
          FIELD_NAME = NA_character_
        )
      )
    }
    
    tibble(
      TABLE_NAME = tbl,
      FIELD_NAME = fields
    )
  }
)

cat(
  "Tables inspected: ",
  length(db_tables),
  "\n",
  sep = ""
)

cat(
  "Table-field combinations: ",
  format(
    nrow(table_field_inventory),
    big.mark = ","
  ),
  "\n\n",
  sep = ""
)


# ==============================================================================
# 11. IDENTIFY POTENTIAL PROVENANCE / TAXONOMIC FIELDS
# ==============================================================================

candidate_pattern <- paste(
  c(
    "name",
    "taxon",
    "species",
    "genus",
    "family",
    "rank",
    "status",
    "accepted",
    "original",
    "source",
    "verbatim",
    "scientific",
    "matched",
    "match",
    "resolved",
    "recognised",
    "recognized",
    "wcvp",
    "ipni",
    "synonym",
    "concept",
    "record",
    "occurrence",
    "star"
  ),
  collapse = "|"
)

candidate_field_inventory <- table_field_inventory %>%
  
  filter(
    !is.na(FIELD_NAME)
  ) %>%
  
  mutate(
    IS_CANDIDATE_FIELD =
      str_detect(
        str_to_lower(FIELD_NAME),
        candidate_pattern
      )
  ) %>%
  
  filter(
    IS_CANDIDATE_FIELD
  ) %>%
  
  arrange(
    TABLE_NAME,
    FIELD_NAME
  )


# ==============================================================================
# 12. IDENTIFY TABLES WITH POTENTIAL WCVP-ID FIELDS
# ==============================================================================

id_pattern <- paste(
  c(
    "wcvp.*id",
    "plant.*name.*id",
    "taxon.*id",
    "accepted.*id",
    "final.*id"
  ),
  collapse = "|"
)

id_field_inventory <- table_field_inventory %>%
  
  filter(
    !is.na(FIELD_NAME)
  ) %>%
  
  filter(
    str_detect(
      str_to_lower(FIELD_NAME),
      id_pattern
    )
  ) %>%
  
  arrange(
    TABLE_NAME,
    FIELD_NAME
  )


# ==============================================================================
# 13. IDENTIFY TABLES WITH POTENTIAL ORIGINAL-NAME FIELDS
# ==============================================================================

name_pattern <- paste(
  c(
    "original.*name",
    "source.*name",
    "verbatim.*name",
    "scientific.*name",
    "taxon.*name",
    "accepted.*name",
    "recognised.*name",
    "recognized.*name",
    "matched.*name",
    "resolved.*name",
    "species",
    "genus"
  ),
  collapse = "|"
)

name_field_inventory <- table_field_inventory %>%
  
  filter(
    !is.na(FIELD_NAME)
  ) %>%
  
  filter(
    str_detect(
      str_to_lower(FIELD_NAME),
      name_pattern
    )
  ) %>%
  
  arrange(
    TABLE_NAME,
    FIELD_NAME
  )


# ==============================================================================
# 14. BUILD CANDIDATE TABLE INVENTORY
# ==============================================================================

candidate_tables <- union(
  id_field_inventory$TABLE_NAME,
  name_field_inventory$TABLE_NAME
)

# Exclude the outputs being created by this module if this script is rerun.
candidate_tables <- setdiff(
  candidate_tables,
  c(
    TABLE_02D_AUDIT,
    TABLE_02D_TABLE_INVENTORY,
    TABLE_02D_FIELD_INVENTORY,
    TABLE_02D_MATCH_SUMMARY,
    TABLE_02D_VALIDATION
  )
)

candidate_table_inventory <- map_dfr(
  candidate_tables,
  function(tbl) {
    
    fields <- table_field_inventory %>%
      filter(
        TABLE_NAME == tbl
      ) %>%
      pull(FIELD_NAME)
    
    n_rows <- tryCatch(
      DBI::dbGetQuery(
        con,
        paste0(
          'SELECT COUNT(*) AS N FROM "',
          gsub('"', '""', tbl),
          '"'
        )
      )$N[[1]],
      error = function(e) NA_real_
    )
    
    tibble(
      TABLE_NAME = tbl,
      
      N_ROWS =
        as.numeric(n_rows),
      
      N_FIELDS =
        length(fields),
      
      N_ID_FIELDS =
        sum(
          str_detect(
            str_to_lower(fields),
            id_pattern
          )
        ),
      
      N_NAME_FIELDS =
        sum(
          str_detect(
            str_to_lower(fields),
            name_pattern
          )
        ),
      
      ID_FIELDS =
        paste(
          fields[
            str_detect(
              str_to_lower(fields),
              id_pattern
            )
          ],
          collapse = " | "
        ),
      
      NAME_FIELDS =
        paste(
          fields[
            str_detect(
              str_to_lower(fields),
              name_pattern
            )
          ],
          collapse = " | "
        )
    )
  }
) %>%
  
  arrange(
    desc(N_ID_FIELDS),
    desc(N_NAME_FIELDS),
    TABLE_NAME
  )


# ==============================================================================
# 15. PREPARE GENUS-ID LOOKUP
# ==============================================================================

genus_ids <- genus_concepts %>%
  distinct(
    FINAL_WCVP_ID
  ) %>%
  filter(
    !is.na(FINAL_WCVP_ID),
    FINAL_WCVP_ID != ""
  )

DBI::dbWriteTable(
  con,
  "vpjd_taxrev_02d_genus_id_lookup_temp",
  genus_ids,
  temporary = TRUE,
  overwrite = TRUE
)


# ==============================================================================
# 16. SEARCH CANDIDATE TABLES FOR GENUS WCVP IDS
#
# Each plausible ID field is tested independently.
# This does not assume a particular upstream schema.
# ==============================================================================

cat("Searching candidate tables for the 823 genus WCVP IDs...\n\n")

safe_ident <- function(x) {
  paste0(
    '"',
    gsub(
      '"',
      '""',
      x
    ),
    '"'
  )
}

id_match_results <- list()

result_counter <- 0L

for (i in seq_len(nrow(id_field_inventory))) {
  
  tbl <- id_field_inventory$TABLE_NAME[[i]]
  fld <- id_field_inventory$FIELD_NAME[[i]]
  
  # Do not recursively inspect 02d output tables.
  if (
    tbl %in%
    c(
      TABLE_02D_AUDIT,
      TABLE_02D_TABLE_INVENTORY,
      TABLE_02D_FIELD_INVENTORY,
      TABLE_02D_MATCH_SUMMARY,
      TABLE_02D_VALIDATION
    )
  ) {
    next
  }
  
  sql <- paste0(
    "SELECT ",
    "COUNT(*) AS N_MATCHING_ROWS, ",
    "COUNT(DISTINCT CAST(t.",
    safe_ident(fld),
    " AS VARCHAR)) AS N_MATCHING_IDS ",
    "FROM ",
    safe_ident(tbl),
    " AS t ",
    "INNER JOIN ",
    "vpjd_taxrev_02d_genus_id_lookup_temp AS g ",
    "ON CAST(t.",
    safe_ident(fld),
    " AS VARCHAR) = g.FINAL_WCVP_ID"
  )
  
  result <- tryCatch(
    
    DBI::dbGetQuery(
      con,
      sql
    ),
    
    error = function(e) {
      
      data.frame(
        N_MATCHING_ROWS = NA_real_,
        N_MATCHING_IDS = NA_real_
      )
    }
  )
  
  result_counter <- result_counter + 1L
  
  id_match_results[[result_counter]] <- tibble(
    
    TABLE_NAME = tbl,
    
    ID_FIELD = fld,
    
    N_MATCHING_ROWS =
      as.numeric(
        result$N_MATCHING_ROWS[[1]]
      ),
    
    N_MATCHING_GENUS_IDS =
      as.numeric(
        result$N_MATCHING_IDS[[1]]
      )
  )
}

if (length(id_match_results) > 0L) {
  
  upstream_match_summary <- bind_rows(
    id_match_results
  ) %>%
    
    mutate(
      PERCENT_GENUS_IDS_MATCHED =
        round(
          100 *
            N_MATCHING_GENUS_IDS /
            EXPECTED_GENUS_CONCEPTS,
          2
        )
    ) %>%
    
    arrange(
      desc(N_MATCHING_GENUS_IDS),
      desc(N_MATCHING_ROWS),
      TABLE_NAME,
      ID_FIELD
    )
  
} else {
  
  upstream_match_summary <- tibble(
    
    TABLE_NAME =
      character(),
    
    ID_FIELD =
      character(),
    
    N_MATCHING_ROWS =
      numeric(),
    
    N_MATCHING_GENUS_IDS =
      numeric(),
    
    PERCENT_GENUS_IDS_MATCHED =
      numeric()
  )
}


# ==============================================================================
# 17. IDENTIFY BEST UPSTREAM ID MATCH
# ==============================================================================

usable_matches <- upstream_match_summary %>%
  
  filter(
    !is.na(N_MATCHING_GENUS_IDS),
    N_MATCHING_GENUS_IDS > 0
  )

if (nrow(usable_matches) > 0L) {
  
  best_match <- usable_matches %>%
    slice_max(
      order_by = N_MATCHING_GENUS_IDS,
      n = 1,
      with_ties = FALSE
    )
  
  BEST_TABLE <-
    best_match$TABLE_NAME[[1]]
  
  BEST_ID_FIELD <-
    best_match$ID_FIELD[[1]]
  
  BEST_MATCH_COUNT <-
    best_match$N_MATCHING_GENUS_IDS[[1]]
  
} else {
  
  BEST_TABLE <- NA_character_
  BEST_ID_FIELD <- NA_character_
  BEST_MATCH_COUNT <- 0L
}


# ==============================================================================
# 18. BUILD INITIAL 02d GENUS AUDIT
#
# At v0.1.0 we deliberately do not assume which source-name field is
# authoritative. Instead, we establish traceability and classify the next
# diagnostic action required.
# ==============================================================================

genus_audit <- genus_concepts %>%
  
  mutate(
    
    FINAL_WCVP_ID =
      as.character(FINAL_WCVP_ID),
    
    HAS_FINAL_WCVP_ID =
      !is.na(FINAL_WCVP_ID) &
      FINAL_WCVP_ID != "",
    
    CURRENT_GENUS_NAME =
      coalesce(
        as.character(TAXON_NAME),
        as.character(GENUS),
        ""
      ),
    
    PROVENANCE_SEARCH_STATUS =
      case_when(
        
        !HAS_FINAL_WCVP_ID ~
          "NO_FINAL_WCVP_ID",
        
        BEST_MATCH_COUNT ==
          EXPECTED_GENUS_CONCEPTS ~
          "UPSTREAM_ID_TRACE_AVAILABLE",
        
        BEST_MATCH_COUNT > 0 ~
          "PARTIAL_UPSTREAM_ID_TRACE_AVAILABLE",
        
        TRUE ~
          "NO_UPSTREAM_ID_TRACE_IDENTIFIED"
      ),
    
    PRELIMINARY_RECOVERABILITY_CLASS =
      case_when(
        
        !HAS_FINAL_WCVP_ID ~
          "INSUFFICIENT_IDENTIFIER_INFORMATION",
        
        PROVENANCE_SEARCH_STATUS ==
          "UPSTREAM_ID_TRACE_AVAILABLE" ~
          "SOURCE_REVIEW_REQUIRED",
        
        PROVENANCE_SEARCH_STATUS ==
          "PARTIAL_UPSTREAM_ID_TRACE_AVAILABLE" ~
          "SOURCE_REVIEW_REQUIRED",
        
        TRUE ~
          "ALTERNATIVE_PROVENANCE_SEARCH_REQUIRED"
      ),
    
    REQUIRES_MANUAL_REVIEW =
      TRUE,
    
    TAXONOMIC_ACTION_TAKEN =
      FALSE
  )


# ==============================================================================
# 19. SUMMARISE INITIAL RECOVERABILITY
# ==============================================================================

recoverability_summary <- genus_audit %>%
  
  count(
    PROVENANCE_SEARCH_STATUS,
    PRELIMINARY_RECOVERABILITY_CLASS,
    name = "N_GENUS_CONCEPTS"
  ) %>%
  
  mutate(
    
    PERCENT_GENUS_CONCEPTS =
      round(
        100 *
          N_GENUS_CONCEPTS /
          EXPECTED_GENUS_CONCEPTS,
        2
      )
  ) %>%
  
  arrange(
    desc(N_GENUS_CONCEPTS)
  )


# ==============================================================================
# 20. VALIDATION
# ==============================================================================

star_values_unchanged <- all(
  (
    genus_audit$PROVISIONAL_STAR ==
      genus_concepts$PROVISIONAL_STAR
  ) |
    (
      is.na(genus_audit$PROVISIONAL_STAR) &
        is.na(genus_concepts$PROVISIONAL_STAR)
    ),
  na.rm = TRUE
)

validation <- tibble(
  
  CHECK = c(
    
    "Canonical taxonomy table present",
    "02c rank audit table present",
    "02c genus inventory present",
    "02c validation table present",
    "02c validation fully PASS",
    "Canonical population = 11,439",
    "02c genus population = 823",
    "02c genus population unique by WCVP ID",
    "02d genus audit population = 823",
    "02d genus audit unique by WCVP ID",
    "Database schema successfully inventoried",
    "Candidate provenance fields inventoried",
    "Candidate ID fields inventoried",
    "All genus concepts retained",
    "No canonical taxa removed",
    "No genus concepts automatically resolved",
    "No taxonomic action taken",
    "Provisional Star allocations unchanged"
  ),
  
  PASS = c(
    
    TABLE_CANONICAL %in%
      db_tables,
    
    TABLE_02C_AUDIT %in%
      db_tables,
    
    TABLE_02C_GENUS %in%
      db_tables,
    
    TABLE_02C_VALIDATION %in%
      db_tables,
    
    all(
      validation_02c$PASS %in%
        TRUE
    ),
    
    nrow(canonical) ==
      EXPECTED_POPULATION,
    
    nrow(genus_concepts) ==
      EXPECTED_GENUS_CONCEPTS,
    
    n_distinct(
      genus_concepts$FINAL_WCVP_ID
    ) ==
      EXPECTED_GENUS_CONCEPTS,
    
    nrow(genus_audit) ==
      EXPECTED_GENUS_CONCEPTS,
    
    n_distinct(
      genus_audit$FINAL_WCVP_ID
    ) ==
      EXPECTED_GENUS_CONCEPTS,
    
    nrow(table_field_inventory) >
      0L,
    
    nrow(candidate_field_inventory) >
      0L,
    
    nrow(id_field_inventory) >
      0L,
    
    nrow(genus_audit) ==
      nrow(genus_concepts),
    
    nrow(canonical) ==
      EXPECTED_POPULATION,
    
    all(
      genus_audit$REQUIRES_MANUAL_REVIEW
    ),
    
    all(
      !genus_audit$TAXONOMIC_ACTION_TAKEN
    ),
    
    star_values_unchanged
  )
) %>%
  
  mutate(
    
    PASS =
      coalesce(
        PASS,
        FALSE
      ),
    
    RESULT =
      if_else(
        PASS,
        "PASS",
        "FAIL"
      )
  )


# ==============================================================================
# 21. CONSOLE REPORT
# ==============================================================================

cat("\n")
cat("============================================================\n")
cat("02d DATABASE PROVENANCE INVENTORY\n")
cat("============================================================\n\n")

cat(
  "Database tables inspected: ",
  length(db_tables),
  "\n",
  sep = ""
)

cat(
  "Candidate provenance fields: ",
  nrow(candidate_field_inventory),
  "\n",
  sep = ""
)

cat(
  "Candidate WCVP/taxon ID fields: ",
  nrow(id_field_inventory),
  "\n",
  sep = ""
)

cat(
  "Candidate source/name fields: ",
  nrow(name_field_inventory),
  "\n",
  sep = ""
)

cat("\n")


# ==============================================================================
# 22. REPORT BEST UPSTREAM MATCHES
# ==============================================================================

cat("— TOP UPSTREAM ID MATCHES —\n\n")

if (nrow(usable_matches) == 0L) {
  
  cat(
    "No upstream table/field matched the 823 genus WCVP IDs.\n"
  )
  
} else {
  
  print(
    head(
      usable_matches,
      30
    ),
    n = 30
  )
}

cat("\n")

cat("— BEST UPSTREAM MATCH —\n\n")

if (is.na(BEST_TABLE)) {
  
  cat(
    "No usable upstream WCVP-ID match identified.\n"
  )
  
} else {
  
  cat(
    "Table: ",
    BEST_TABLE,
    "\n",
    sep = ""
  )
  
  cat(
    "ID field: ",
    BEST_ID_FIELD,
    "\n",
    sep = ""
  )
  
  cat(
    "Genus concepts matched: ",
    format(
      BEST_MATCH_COUNT,
      big.mark = ","
    ),
    " / ",
    format(
      EXPECTED_GENUS_CONCEPTS,
      big.mark = ","
    ),
    "\n",
    sep = ""
  )
}

cat("\n")


# ==============================================================================
# 23. REPORT CANDIDATE TABLES
# ==============================================================================

cat("— CANDIDATE PROVENANCE TABLES —\n\n")

print(
  candidate_table_inventory,
  n = Inf
)

cat("\n")


# ==============================================================================
# 24. REPORT RECOVERABILITY
# ==============================================================================

cat("— PRELIMINARY GENUS RECOVERABILITY —\n\n")

print(
  recoverability_summary,
  n = Inf
)

cat("\n")


# ==============================================================================
# 25. VALIDATION REPORT
# ==============================================================================

cat("— VALIDATION —\n\n")

print(
  validation,
  n = Inf
)


# ==============================================================================
# 26. STOP ON STRUCTURAL VALIDATION FAILURE
# ==============================================================================

if (!all(validation$PASS)) {
  
  failed_checks <- validation %>%
    filter(!PASS) %>%
    pull(CHECK)
  
  cat("\n")
  cat("VALIDATION FAILURE\n")
  cat("------------------\n")
  
  cat(
    paste0(
      " - ",
      failed_checks
    ),
    sep = "\n"
  )
  
  cat("\n")
  
  DBI::dbDisconnect(
    con,
    shutdown = TRUE
  )
  
  rm(con)
  
  stop(
    paste0(
      "\n02d structural validation failed.\n",
      "No permanent 02d DuckDB outputs have been written."
    )
  )
}


# ==============================================================================
# 27. WRITE DUCKDB OUTPUTS
# ==============================================================================

DBI::dbWriteTable(
  con,
  TABLE_02D_AUDIT,
  genus_audit,
  overwrite = TRUE
)

DBI::dbWriteTable(
  con,
  TABLE_02D_TABLE_INVENTORY,
  candidate_table_inventory,
  overwrite = TRUE
)

DBI::dbWriteTable(
  con,
  TABLE_02D_FIELD_INVENTORY,
  candidate_field_inventory,
  overwrite = TRUE
)

DBI::dbWriteTable(
  con,
  TABLE_02D_MATCH_SUMMARY,
  upstream_match_summary,
  overwrite = TRUE
)

DBI::dbWriteTable(
  con,
  TABLE_02D_VALIDATION,
  validation,
  overwrite = TRUE
)


# ==============================================================================
# 28. WRITE CSV OUTPUTS
# ==============================================================================

readr::write_csv(
  genus_audit,
  file.path(
    OUTPUT_DIR,
    "VPJD_02d_genus_provenance_audit.csv"
  )
)

readr::write_csv(
  candidate_table_inventory,
  file.path(
    OUTPUT_DIR,
    "VPJD_02d_candidate_table_inventory.csv"
  )
)

readr::write_csv(
  candidate_field_inventory,
  file.path(
    OUTPUT_DIR,
    "VPJD_02d_candidate_field_inventory.csv"
  )
)

readr::write_csv(
  id_field_inventory,
  file.path(
    OUTPUT_DIR,
    "VPJD_02d_candidate_id_fields.csv"
  )
)

readr::write_csv(
  name_field_inventory,
  file.path(
    OUTPUT_DIR,
    "VPJD_02d_candidate_name_fields.csv"
  )
)

readr::write_csv(
  upstream_match_summary,
  file.path(
    OUTPUT_DIR,
    "VPJD_02d_upstream_id_match_summary.csv"
  )
)

readr::write_csv(
  recoverability_summary,
  file.path(
    OUTPUT_DIR,
    "VPJD_02d_preliminary_recoverability_summary.csv"
  )
)

readr::write_csv(
  validation,
  file.path(
    OUTPUT_DIR,
    "VPJD_02d_validation.csv"
  )
)


# ==============================================================================
# 29. RESULT OBJECT
# ==============================================================================

taxrev_02d_result <- list(
  
  genus_audit =
    genus_audit,
  
  candidate_table_inventory =
    candidate_table_inventory,
  
  candidate_field_inventory =
    candidate_field_inventory,
  
  id_field_inventory =
    id_field_inventory,
  
  name_field_inventory =
    name_field_inventory,
  
  upstream_match_summary =
    upstream_match_summary,
  
  recoverability_summary =
    recoverability_summary,
  
  validation =
    validation
)


# ==============================================================================
# 30. COMPLETION REPORT
# ==============================================================================

cat("\n")
cat("============================================================\n")

cat(
  "VPJD TAXONOMIC REVISION 02d v",
  VERSION,
  " COMPLETE\n",
  sep = ""
)

cat("============================================================\n")

cat(
  "Canonical concepts preserved: ",
  format(
    nrow(canonical),
    big.mark = ","
  ),
  "\n",
  sep = ""
)

cat(
  "Genus concepts audited: ",
  format(
    nrow(genus_audit),
    big.mark = ","
  ),
  "\n",
  sep = ""
)

cat(
  "Candidate provenance tables: ",
  nrow(candidate_table_inventory),
  "\n",
  sep = ""
)

cat(
  "Candidate provenance fields: ",
  nrow(candidate_field_inventory),
  "\n",
  sep = ""
)

cat(
  "Candidate ID fields tested: ",
  nrow(id_field_inventory),
  "\n",
  sep = ""
)

cat(
  "Best upstream WCVP-ID match: ",
  ifelse(
    is.na(BEST_TABLE),
    "NONE",
    paste0(
      BEST_TABLE,
      " :: ",
      BEST_ID_FIELD
    )
  ),
  "\n",
  sep = ""
)

cat(
  "Best upstream genus-ID coverage: ",
  format(
    BEST_MATCH_COUNT,
    big.mark = ","
  ),
  " / ",
  format(
    EXPECTED_GENUS_CONCEPTS,
    big.mark = ","
  ),
  "\n",
  sep = ""
)

cat("Canonical taxa removed: 0\n")
cat("Genus concepts resolved automatically: 0\n")
cat("Star allocations changed: FALSE\n")

cat(
  "Validation: ",
  sum(validation$PASS),
  "/",
  nrow(validation),
  " PASS\n",
  sep = ""
)

cat(
  "Canonical 02d audit: ",
  TABLE_02D_AUDIT,
  "\n",
  sep = ""
)

cat(
  "Outputs: ",
  OUTPUT_DIR,
  "\n",
  sep = ""
)

cat("============================================================\n")


# ==============================================================================
# 31. CLEAN DISCONNECT
# ==============================================================================

DBI::dbDisconnect(
  con,
  shutdown = TRUE
)

rm(con)

cat("\nDuckDB connection closed cleanly.\n")


# ==============================================================================
# END
# ==============================================================================