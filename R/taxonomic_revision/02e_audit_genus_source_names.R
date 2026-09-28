# ==============================================================================
# VPJD TAXONOMIC REVISION
# 02e — AUDIT GENUS SOURCE NAMES
#
# Version: 0.1.2
#
# PURPOSE
# -------
# Investigate the 823 genus-rank concepts identified during the VPJD
# taxonomic revision and determine what taxonomic resolution is present
# in their underlying occurrence/source records.
#
# This module:
#   1. validates the canonical 11,439-concept VPJD population;
#   2. obtains the 823 genus concepts from the validated 02d audit where
#      available, with a canonical-rank fallback;
#   3. interrogates vpjd_japan_occurrences_integrated;
#   4. discovers candidate source-name fields from the actual schema;
#   5. extracts all occurrence evidence linked to the 823 genus concepts;
#   6. identifies source values apparently containing species or
#      infraspecific information;
#   7. creates concept-level and source-evidence-level audit outputs;
#   8. writes substantive outputs to DuckDB and CSV;
#   9. makes NO taxonomic or Star changes.
#
# IMPORTANT
# ---------
# This is an AUDIT module.
#
# It does NOT:
#   * remove taxa;
#   * alter WCVP IDs;
#   * alter accepted names;
#   * alter accepted ranks;
#   * alter provisional Star allocations;
#   * automatically resolve genus concepts;
#   * treat heuristic source-name signals as taxonomic decisions.
#
# Expected canonical population: 11,439
# Expected genus concepts:       823
#
# ==============================================================================


# ==============================================================================
# 01. PACKAGES
# ==============================================================================

required_packages <- c(
  "DBI",
  "duckdb",
  "dplyr",
  "tidyr",
  "tibble",
  "stringr",
  "readr",
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
library(tidyr)
library(tibble)
library(stringr)
library(readr)
library(purrr)


# ==============================================================================
# 02. MODULE METADATA
# ==============================================================================

MODULE <- "02e_audit_genus_source_names"
VERSION <- "0.1.2"
RUN_DATE <- Sys.Date()

EXPECTED_CANONICAL_POPULATION <- 11439L
EXPECTED_GENUS_CONCEPTS <- 823L


# ==============================================================================
# 03. PROJECT PATHS
# ==============================================================================

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
  "02e_genus_source_names"
)

dir.create(
  OUTPUT_DIR,
  recursive = TRUE,
  showWarnings = FALSE
)

if (!file.exists(DB_PATH)) {
  stop(
    "VPJD database not found:\n",
    DB_PATH
  )
}


# ==============================================================================
# 04. TABLE NAMES
# ==============================================================================

TABLE_CANONICAL <-
  "vpjd_star_provisional_wholesale_allocation"

TABLE_OCCURRENCES <-
  "vpjd_japan_occurrences_integrated"

TABLE_02D <-
  "vpjd_taxrev_02d_genus_provenance_audit"


# 02e output tables

TABLE_02E_FIELDS <-
  "vpjd_taxrev_02e_source_field_inventory"

TABLE_02E_SOURCE_LONG <-
  "vpjd_taxrev_02e_genus_source_name_long"

TABLE_02E_SOURCE_DISTINCT <-
  "vpjd_taxrev_02e_genus_source_name_distinct"

TABLE_02E_AUDIT <-
  "vpjd_taxrev_02e_genus_source_name_audit"

TABLE_02E_SUMMARY <-
  "vpjd_taxrev_02e_classification_summary"

TABLE_02E_FIELD_SUMMARY <-
  "vpjd_taxrev_02e_field_evidence_summary"

TABLE_02E_VALIDATION <-
  "vpjd_taxrev_02e_validation"


# ==============================================================================
# 05. START REPORT
# ==============================================================================

cat("\n")
cat("============================================================\n")
cat("VPJD TAXONOMIC REVISION — 02e\n")
cat("AUDIT GENUS SOURCE NAMES\n")
cat("============================================================\n\n")

cat("Run date: ", as.character(RUN_DATE), "\n", sep = "")
cat("Module: ", MODULE, "\n", sep = "")
cat("Version: ", VERSION, "\n\n", sep = "")

cat("Database:\n")
cat(DB_PATH, "\n\n")

cat("Output directory:\n")
cat(OUTPUT_DIR, "\n\n")

cat("Expected canonical population: 11,439\n")
cat("Expected genus concepts: 823\n")
cat("Mode: AUDIT ONLY\n")
cat("Canonical taxonomic modification: FALSE\n")
cat("Star modification: FALSE\n\n")


# ==============================================================================
# 06. CLOSE ANY EXISTING DATABASE CONNECTION
# ==============================================================================

if (exists("con", envir = .GlobalEnv)) {
  
  existing_con <- get(
    "con",
    envir = .GlobalEnv
  )
  
  existing_valid <- tryCatch(
    DBI::dbIsValid(existing_con),
    error = function(e) FALSE
  )
  
  if (isTRUE(existing_valid)) {
    
    cat("Closing existing database connection...\n")
    
    try(
      DBI::dbDisconnect(
        existing_con,
        shutdown = TRUE
      ),
      silent = TRUE
    )
  }
  
  rm(
    con,
    envir = .GlobalEnv
  )
}

gc()


# ==============================================================================
# 07. CONNECT
# ==============================================================================

cat("Opening VPJD DuckDB...\n")

con <- DBI::dbConnect(
  duckdb::duckdb(),
  dbdir = DB_PATH,
  read_only = FALSE
)

if (!DBI::dbIsValid(con)) {
  stop(
    "Could not establish a valid DuckDB connection."
  )
}

cat("Connection established.\n\n")


# ==============================================================================
# 08. DATABASE INVENTORY
# ==============================================================================

db_tables <- DBI::dbListTables(con)

cat(
  "Database tables detected: ",
  length(db_tables),
  "\n\n",
  sep = ""
)

relevant_tables <- db_tables[
  grepl(
    "vpjd|star|wcvp|occurrence|taxrev",
    db_tables,
    ignore.case = TRUE
  )
]

cat("Relevant database tables:\n\n")

cat(
  paste0(
    "  ",
    sort(relevant_tables)
  ),
  sep = "\n"
)

cat("\n\n")


# ==============================================================================
# 09. CHECK TRUE CORE DEPENDENCIES
# ==============================================================================

required_core_tables <- c(
  TABLE_CANONICAL,
  TABLE_OCCURRENCES
)

missing_core_tables <- setdiff(
  required_core_tables,
  db_tables
)

if (length(missing_core_tables) > 0L) {
  
  DBI::dbDisconnect(
    con,
    shutdown = TRUE
  )
  
  stop(
    "Required VPJD core table(s) missing:\n",
    paste(
      missing_core_tables,
      collapse = "\n"
    )
  )
}

cat("Core VPJD tables present.\n\n")


# ==============================================================================
# 10. LOAD CANONICAL POPULATION
# ==============================================================================

canonical <- DBI::dbGetQuery(
  con,
  paste0(
    'SELECT * FROM "',
    TABLE_CANONICAL,
    '"'
  )
) |>
  as_tibble()

if (!"FINAL_WCVP_ID" %in% names(canonical)) {
  
  DBI::dbDisconnect(
    con,
    shutdown = TRUE
  )
  
  stop(
    "FINAL_WCVP_ID is absent from ",
    TABLE_CANONICAL,
    "."
  )
}

canonical <- canonical |>
  mutate(
    FINAL_WCVP_ID =
      as.character(FINAL_WCVP_ID)
  )


# ==============================================================================
# 11. VALIDATE CANONICAL POPULATION
# ==============================================================================

if (
  nrow(canonical) !=
  EXPECTED_CANONICAL_POPULATION
) {
  
  DBI::dbDisconnect(
    con,
    shutdown = TRUE
  )
  
  stop(
    "Expected canonical population of ",
    format(
      EXPECTED_CANONICAL_POPULATION,
      big.mark = ","
    ),
    "; found ",
    format(
      nrow(canonical),
      big.mark = ","
    ),
    "."
  )
}

if (
  n_distinct(
    canonical$FINAL_WCVP_ID
  ) !=
  EXPECTED_CANONICAL_POPULATION
) {
  
  DBI::dbDisconnect(
    con,
    shutdown = TRUE
  )
  
  stop(
    "Canonical population is not unique by FINAL_WCVP_ID."
  )
}

cat(
  "Canonical VPJD population: ",
  format(
    nrow(canonical),
    big.mark = ","
  ),
  "\n",
  sep = ""
)


# ==============================================================================
# 12. IDENTIFY RANK FIELD IN CANONICAL TABLE
# ==============================================================================

rank_candidates <- c(
  "FINAL_WCVP_RANK",
  "wcvp_taxon_rank",
  "WCVP_TAXON_RANK",
  "RANK_NORMALISED",
  "TAXON_RANK",
  "taxon_rank"
)

rank_fields_found <- rank_candidates[
  rank_candidates %in%
    names(canonical)
]

if (length(rank_fields_found) > 0L) {
  
  rank_field <- rank_fields_found[[1]]
  
  cat(
    "Canonical rank field: ",
    rank_field,
    "\n",
    sep = ""
  )
  
} else {
  
  rank_field <- NA_character_
  
  cat(
    "Canonical allocation does not contain a usable rank field.\n"
  )
}


# ==============================================================================
# 13. ESTABLISH GENUS POPULATION
# ==============================================================================

# Preferred source:
#   validated 02d genus provenance audit.
#
# Fallback:
#   derive genus concepts directly from canonical allocation.

if (TABLE_02D %in% db_tables) {
  
  cat(
    "\nUsing 02d genus provenance audit as ",
    "the genus population source.\n"
  )
  
  genus_02d <- DBI::dbGetQuery(
    con,
    paste0(
      'SELECT * FROM "',
      TABLE_02D,
      '"'
    )
  ) |>
    as_tibble()
  
  if (!"FINAL_WCVP_ID" %in% names(genus_02d)) {
    
    DBI::dbDisconnect(
      con,
      shutdown = TRUE
    )
    
    stop(
      TABLE_02D,
      " does not contain FINAL_WCVP_ID."
    )
  }
  
  genus_02d <- genus_02d |>
    mutate(
      FINAL_WCVP_ID =
        as.character(FINAL_WCVP_ID)
    )
  
  genus_ids <- genus_02d |>
    distinct(
      FINAL_WCVP_ID
    )
  
  GENUS_POPULATION_SOURCE <- TABLE_02D
  
} else {
  
  cat(
    "\n02d table not present; deriving genus concepts ",
    "from canonical allocation.\n"
  )
  
  if (is.na(rank_field)) {
    
    DBI::dbDisconnect(
      con,
      shutdown = TRUE
    )
    
    stop(
      paste0(
        "Cannot establish genus population.\n",
        "02d table is absent and no usable rank field ",
        "exists in the canonical allocation."
      )
    )
  }
  
  genus_ids <- canonical |>
    mutate(
      AUDIT_RANK =
        toupper(
          trimws(
            as.character(
              .data[[rank_field]]
            )
          )
        )
    ) |>
    filter(
      AUDIT_RANK == "GENUS"
    ) |>
    distinct(
      FINAL_WCVP_ID
    )
  
  GENUS_POPULATION_SOURCE <-
    TABLE_CANONICAL
}


# ==============================================================================
# 14. VALIDATE GENUS POPULATION
# ==============================================================================

if (
  nrow(genus_ids) !=
  EXPECTED_GENUS_CONCEPTS
) {
  
  DBI::dbDisconnect(
    con,
    shutdown = TRUE
  )
  
  stop(
    "Expected ",
    EXPECTED_GENUS_CONCEPTS,
    " genus concepts; found ",
    format(
      nrow(genus_ids),
      big.mark = ","
    ),
    "."
  )
}

if (
  n_distinct(
    genus_ids$FINAL_WCVP_ID
  ) !=
  EXPECTED_GENUS_CONCEPTS
) {
  
  DBI::dbDisconnect(
    con,
    shutdown = TRUE
  )
  
  stop(
    "Genus population is not unique by FINAL_WCVP_ID."
  )
}

cat(
  "Genus concepts established: ",
  format(
    nrow(genus_ids),
    big.mark = ","
  ),
  "\n",
  sep = ""
)

cat(
  "Genus population source: ",
  GENUS_POPULATION_SOURCE,
  "\n\n",
  sep = ""
)


# ==============================================================================
# 15. ATTACH CANONICAL INFORMATION
# ==============================================================================

genus_concepts <- genus_ids |>
  left_join(
    canonical,
    by = "FINAL_WCVP_ID"
  )

if (
  nrow(genus_concepts) !=
  EXPECTED_GENUS_CONCEPTS
) {
  
  DBI::dbDisconnect(
    con,
    shutdown = TRUE
  )
  
  stop(
    "Joining genus IDs to canonical allocation altered the genus population."
  )
}


# ==============================================================================
# 16. INSPECT OCCURRENCE TABLE SCHEMA
# ==============================================================================

occurrence_schema <- DBI::dbGetQuery(
  con,
  paste0(
    "DESCRIBE SELECT * FROM ",
    DBI::dbQuoteIdentifier(
      con,
      TABLE_OCCURRENCES
    )
  )
) |>
  as_tibble()

cat(
  "Occurrence schema fields: ",
  nrow(occurrence_schema),
  "\n",
  sep = ""
)

schema_names_upper <- toupper(
  names(occurrence_schema)
)

field_name_index <- which(
  schema_names_upper %in%
    c(
      "COLUMN_NAME",
      "COLUMN",
      "NAME"
    )
)

if (length(field_name_index) == 0L) {
  
  DBI::dbDisconnect(
    con,
    shutdown = TRUE
  )
  
  stop(
    "Could not identify the field-name column ",
    "returned by DuckDB DESCRIBE."
  )
}

field_name_column <-
  names(occurrence_schema)[
    field_name_index[[1]]
  ]

occurrence_schema <- occurrence_schema |>
  mutate(
    FIELD_NAME =
      as.character(
        .data[[field_name_column]]
      )
  )

occurrence_fields <-
  occurrence_schema$FIELD_NAME


# ==============================================================================
# 17. IDENTIFY OCCURRENCE → WCVP LINK FIELD
# ==============================================================================

preferred_id_fields <- c(
  "FINAL_WCVP_ID",
  "final_wcvp_id",
  "WCVP_ID",
  "wcvp_id",
  "wcvp_plant_name_id",
  "WCVP_PLANT_NAME_ID"
)

occurrence_id_fields_found <- preferred_id_fields[
  preferred_id_fields %in%
    occurrence_fields
]

if (length(occurrence_id_fields_found) > 0L) {
  
  occurrence_id_field <-
    occurrence_id_fields_found[[1]]
  
} else {
  
  fuzzy_id_fields <- occurrence_fields[
    grepl(
      "wcvp.*id|id.*wcvp",
      occurrence_fields,
      ignore.case = TRUE
    )
  ]
  
  if (length(fuzzy_id_fields) == 0L) {
    
    DBI::dbDisconnect(
      con,
      shutdown = TRUE
    )
    
    stop(
      paste0(
        "Could not identify the WCVP linking field in ",
        TABLE_OCCURRENCES,
        "."
      )
    )
  }
  
  occurrence_id_field <-
    fuzzy_id_fields[[1]]
}

cat(
  "Occurrence WCVP link field: ",
  occurrence_id_field,
  "\n\n",
  sep = ""
)


# ==============================================================================
# 18. DISCOVER CANDIDATE SOURCE-NAME FIELDS
# ==============================================================================

candidate_source_fields <- occurrence_fields[
  grepl(
    paste(
      c(
        "scientific.*name",
        "original.*name",
        "verbatim.*name",
        "source.*name",
        "submitted.*name",
        "matched.*name",
        "accepted.*name",
        "taxon.*name",
        "canonical.*name",
        "^species$",
        "species.*name",
        "^genus$",
        "genus.*name",
        "infraspecific",
        "infra.*name",
        "subspecies",
        "variety",
        "forma"
      ),
      collapse = "|"
    ),
    occurrence_fields,
    ignore.case = TRUE
  )
]

candidate_source_fields <-
  candidate_source_fields[
    !grepl(
      paste(
        c(
          "_id$",
          "status",
          "rank$",
          "key$",
          "code$",
          "issue",
          "confidence",
          "match.*type"
        ),
        collapse = "|"
      ),
      candidate_source_fields,
      ignore.case = TRUE
    )
  ]

candidate_source_fields <- setdiff(
  unique(
    candidate_source_fields
  ),
  occurrence_id_field
)

if (length(candidate_source_fields) == 0L) {
  
  DBI::dbDisconnect(
    con,
    shutdown = TRUE
  )
  
  stop(
    paste0(
      "No plausible source-name fields were identified in ",
      TABLE_OCCURRENCES,
      "."
    )
  )
}

cat(
  "Candidate source-name fields discovered: ",
  length(candidate_source_fields),
  "\n\n",
  sep = ""
)

cat(
  paste0(
    "  ",
    candidate_source_fields
  ),
  sep = "\n"
)

cat("\n\n")


# ==============================================================================
# 19. SOURCE-FIELD INVENTORY
# ==============================================================================

source_field_inventory <- occurrence_schema |>
  mutate(
    
    IS_WCVP_LINK_FIELD =
      FIELD_NAME ==
      occurrence_id_field,
    
    IS_CANDIDATE_SOURCE_NAME =
      FIELD_NAME %in%
      candidate_source_fields,
    
    FIELD_PRIORITY =
      case_when(
        
        grepl(
          "verbatim|original|submitted|source",
          FIELD_NAME,
          ignore.case = TRUE
        ) ~ 1L,
        
        grepl(
          "scientific.*name",
          FIELD_NAME,
          ignore.case = TRUE
        ) ~ 2L,
        
        grepl(
          "taxon.*name|canonical.*name",
          FIELD_NAME,
          ignore.case = TRUE
        ) ~ 3L,
        
        grepl(
          "species",
          FIELD_NAME,
          ignore.case = TRUE
        ) ~ 4L,
        
        grepl(
          "genus",
          FIELD_NAME,
          ignore.case = TRUE
        ) ~ 5L,
        
        TRUE ~ 9L
      )
  ) |>
  arrange(
    desc(
      IS_CANDIDATE_SOURCE_NAME
    ),
    FIELD_PRIORITY,
    FIELD_NAME
  )


# ==============================================================================
# 20. CREATE TEMPORARY 823-ID LOOKUP
# ==============================================================================

DBI::dbWriteTable(
  con,
  "tmp_02e_genus_ids",
  genus_ids,
  temporary = TRUE,
  overwrite = TRUE
)


# ==============================================================================
# 21. SQL IDENTIFIER HELPER
# ==============================================================================

quote_identifier <- function(x) {
  
  as.character(
    DBI::dbQuoteIdentifier(
      con,
      x
    )
  )
}


# ==============================================================================
# 22. BUILD UNAMBIGUOUS OCCURRENCE EXTRACTION SQL
#
# IMPORTANT FIX IN v0.1.2
# -----------------------
# Both:
#
#   o = vpjd_japan_occurrences_integrated
#   g = tmp_02e_genus_ids
#
# contain FINAL_WCVP_ID.
#
# Therefore EVERY selected occurrence field is explicitly qualified with
# alias "o".
#
# The linking field is explicitly selected as:
#
#   CAST(o."FINAL_WCVP_ID" AS VARCHAR) AS "FINAL_WCVP_ID"
#
# This prevents DuckDB's ambiguous-column binder error.
# ==============================================================================

occurrence_table_sql <-
  quote_identifier(
    TABLE_OCCURRENCES
  )

occurrence_id_sql <-
  quote_identifier(
    occurrence_id_field
  )


# Build source-field SELECT expressions:
#
#   o."scientificName" AS "scientificName"
#   o."species"        AS "species"
#   etc.

source_select_expressions <- vapply(
  
  candidate_source_fields,
  
  function(field) {
    
    paste0(
      "o.",
      quote_identifier(field),
      " AS ",
      quote_identifier(field)
    )
  },
  
  character(1)
)


# Explicitly qualify and standardise the WCVP ID.

id_select_expression <- paste0(
  "CAST(o.",
  occurrence_id_sql,
  " AS VARCHAR) AS ",
  quote_identifier(
    "FINAL_WCVP_ID"
  )
)


select_expressions <- c(
  id_select_expression,
  source_select_expressions
)


select_clause <- paste(
  select_expressions,
  collapse = ",\n        "
)


sql_genus_occurrences <- paste0(
  
  "SELECT\n        ",
  
  select_clause,
  
  "\nFROM ",
  
  occurrence_table_sql,
  
  " AS o\n",
  
  "INNER JOIN tmp_02e_genus_ids AS g\n",
  
  "  ON CAST(o.",
  
  occurrence_id_sql,
  
  " AS VARCHAR) = CAST(g.FINAL_WCVP_ID AS VARCHAR)"
)


# ==============================================================================
# 23. EXTRACT GENUS-LINKED OCCURRENCES
# ==============================================================================

cat(
  "Extracting source records linked to the 823 genus concepts...\n"
)

genus_occurrences <- DBI::dbGetQuery(
  con,
  sql_genus_occurrences
) |>
  as_tibble()


# ==============================================================================
# 24. VALIDATE EXTRACTION
# ==============================================================================

if (
  !"FINAL_WCVP_ID" %in%
  names(genus_occurrences)
) {
  
  DBI::dbDisconnect(
    con,
    shutdown = TRUE
  )
  
  stop(
    "Occurrence extraction did not return FINAL_WCVP_ID."
  )
}

genus_occurrences <- genus_occurrences |>
  mutate(
    FINAL_WCVP_ID =
      as.character(
        FINAL_WCVP_ID
      )
  )

n_occurrence_concepts <-
  n_distinct(
    genus_occurrences$FINAL_WCVP_ID
  )

cat(
  "Genus-linked occurrence rows: ",
  format(
    nrow(genus_occurrences),
    big.mark = ","
  ),
  "\n",
  sep = ""
)

cat(
  "Genus concepts represented by occurrence records: ",
  format(
    n_occurrence_concepts,
    big.mark = ","
  ),
  " / ",
  EXPECTED_GENUS_CONCEPTS,
  "\n\n",
  sep = ""
)


# ==============================================================================
# 25. CONVERT SOURCE EVIDENCE TO LONG FORM
# ==============================================================================

source_long <- genus_occurrences |>
  pivot_longer(
    cols =
      all_of(
        candidate_source_fields
      ),
    names_to =
      "SOURCE_FIELD",
    values_to =
      "SOURCE_VALUE"
  ) |>
  mutate(
    
    SOURCE_VALUE =
      trimws(
        as.character(
          SOURCE_VALUE
        )
      ),
    
    SOURCE_VALUE =
      na_if(
        SOURCE_VALUE,
        ""
      ),
    
    SOURCE_VALUE_NORMALISED =
      str_squish(
        SOURCE_VALUE
      )
  ) |>
  filter(
    !is.na(
      SOURCE_VALUE_NORMALISED
    )
  )


# ==============================================================================
# 26. DIAGNOSTIC SOURCE-NAME HEURISTICS
# ==============================================================================

# IMPORTANT:
# These are evidence flags, not nomenclatural decisions.
#
# They identify source strings which APPEAR to contain information below
# genus rank.

source_long <- source_long |>
  mutate(
    
    HAS_SUBSPECIES_MARKER =
      str_detect(
        SOURCE_VALUE_NORMALISED,
        regex(
          "\\b(subsp\\.?|ssp\\.?)\\b",
          ignore_case = TRUE
        )
      ),
    
    HAS_VARIETY_MARKER =
      str_detect(
        SOURCE_VALUE_NORMALISED,
        regex(
          "\\bvar\\.?\\b",
          ignore_case = TRUE
        )
      ),
    
    HAS_FORMA_MARKER =
      str_detect(
        SOURCE_VALUE_NORMALISED,
        regex(
          "\\bforma\\b|\\bf\\.\\s",
          ignore_case = TRUE
        )
      ),
    
    HAS_NOTHOSUBSPECIES_MARKER =
      str_detect(
        SOURCE_VALUE_NORMALISED,
        regex(
          "\\bnothosubsp\\.?\\b",
          ignore_case = TRUE
        )
      ),
    
    HAS_INFRA_MARKER =
      HAS_SUBSPECIES_MARKER |
      HAS_VARIETY_MARKER |
      HAS_FORMA_MARKER |
      HAS_NOTHOSUBSPECIES_MARKER,
    
    HAS_BINOMIAL_PATTERN =
      str_detect(
        SOURCE_VALUE_NORMALISED,
        paste0(
          "^[[:upper:]][[:alpha:]'’-]+",
          "\\s+",
          "[×x]?\\s*",
          "[[:lower:]][[:alpha:]'’-]+"
        )
      ),
    
    HAS_BELOW_GENUS_SIGNAL =
      HAS_BINOMIAL_PATTERN |
      HAS_INFRA_MARKER,
    
    SOURCE_SIGNAL =
      case_when(
        
        HAS_INFRA_MARKER ~
          "INFRASPECIFIC_INFORMATION_PRESENT",
        
        HAS_BINOMIAL_PATTERN ~
          "SPECIES_INFORMATION_PRESENT",
        
        TRUE ~
          "NO_BELOW_GENUS_SIGNAL"
      )
  )


# ==============================================================================
# 27. DISTINCT SOURCE EVIDENCE
# ==============================================================================

source_distinct <- source_long |>
  group_by(
    FINAL_WCVP_ID,
    SOURCE_FIELD,
    SOURCE_VALUE_NORMALISED,
    SOURCE_SIGNAL
  ) |>
  summarise(
    N_OCCURRENCE_ROWS =
      n(),
    .groups =
      "drop"
  ) |>
  arrange(
    FINAL_WCVP_ID,
    SOURCE_FIELD,
    desc(
      N_OCCURRENCE_ROWS
    ),
    SOURCE_VALUE_NORMALISED
  )


# ==============================================================================
# 28. OCCURRENCE COUNTS BY GENUS CONCEPT
# ==============================================================================

occurrence_counts <- genus_occurrences |>
  count(
    FINAL_WCVP_ID,
    name =
      "N_OCCURRENCE_RECORDS"
  )


# ==============================================================================
# 29. SOURCE-EVIDENCE SUMMARY BY GENUS CONCEPT
# ==============================================================================

source_concept_summary <- source_long |>
  group_by(
    FINAL_WCVP_ID
  ) |>
  summarise(
    
    N_SOURCE_EVIDENCE_ROWS =
      n(),
    
    N_SOURCE_FIELDS =
      n_distinct(
        SOURCE_FIELD
      ),
    
    N_DISTINCT_SOURCE_VALUES =
      n_distinct(
        SOURCE_VALUE_NORMALISED
      ),
    
    N_BELOW_GENUS_SIGNALS =
      sum(
        HAS_BELOW_GENUS_SIGNAL,
        na.rm = TRUE
      ),
    
    N_SPECIES_SIGNALS =
      sum(
        SOURCE_SIGNAL ==
          "SPECIES_INFORMATION_PRESENT",
        na.rm = TRUE
      ),
    
    N_INFRA_SIGNALS =
      sum(
        SOURCE_SIGNAL ==
          "INFRASPECIFIC_INFORMATION_PRESENT",
        na.rm = TRUE
      ),
    
    ANY_SPECIES_SIGNAL =
      any(
        SOURCE_SIGNAL ==
          "SPECIES_INFORMATION_PRESENT",
        na.rm = TRUE
      ),
    
    ANY_INFRA_SIGNAL =
      any(
        SOURCE_SIGNAL ==
          "INFRASPECIFIC_INFORMATION_PRESENT",
        na.rm = TRUE
      ),
    
    ANY_BELOW_GENUS_SIGNAL =
      any(
        HAS_BELOW_GENUS_SIGNAL,
        na.rm = TRUE
      ),
    
    .groups =
      "drop"
  )


# ==============================================================================
# 30. PREPARE CANONICAL DISPLAY FIELDS
# ==============================================================================

preferred_display_fields <- intersect(
  c(
    "FINAL_WCVP_ID",
    "FINAL_WCVP_RECOGNISED_NAME",
    "FINAL_WCVP_RANK",
    "FINAL_WCVP_STATUS",
    "FINAL_WCVP_CONCEPT_CLASS",
    "PROVISIONAL_STAR",
    "IS_ALLOCATED",
    "IS_UNRESOLVED"
  ),
  names(genus_concepts)
)

genus_display <- genus_concepts |>
  select(
    all_of(
      preferred_display_fields
    )
  )

if (
  !"FINAL_WCVP_ID" %in%
  names(genus_display)
) {
  
  genus_display <- genus_concepts |>
    select(
      FINAL_WCVP_ID
    )
}


# ==============================================================================
# 31. BUILD CONCEPT-LEVEL 02e AUDIT
# ==============================================================================

genus_source_audit <- genus_display |>
  left_join(
    occurrence_counts,
    by =
      "FINAL_WCVP_ID"
  ) |>
  left_join(
    source_concept_summary,
    by =
      "FINAL_WCVP_ID"
  ) |>
  mutate(
    
    N_OCCURRENCE_RECORDS =
      coalesce(
        N_OCCURRENCE_RECORDS,
        0L
      ),
    
    N_SOURCE_EVIDENCE_ROWS =
      coalesce(
        N_SOURCE_EVIDENCE_ROWS,
        0L
      ),
    
    N_SOURCE_FIELDS =
      coalesce(
        N_SOURCE_FIELDS,
        0L
      ),
    
    N_DISTINCT_SOURCE_VALUES =
      coalesce(
        N_DISTINCT_SOURCE_VALUES,
        0L
      ),
    
    N_BELOW_GENUS_SIGNALS =
      coalesce(
        N_BELOW_GENUS_SIGNALS,
        0L
      ),
    
    N_SPECIES_SIGNALS =
      coalesce(
        N_SPECIES_SIGNALS,
        0L
      ),
    
    N_INFRA_SIGNALS =
      coalesce(
        N_INFRA_SIGNALS,
        0L
      ),
    
    ANY_SPECIES_SIGNAL =
      coalesce(
        ANY_SPECIES_SIGNAL,
        FALSE
      ),
    
    ANY_INFRA_SIGNAL =
      coalesce(
        ANY_INFRA_SIGNAL,
        FALSE
      ),
    
    ANY_BELOW_GENUS_SIGNAL =
      coalesce(
        ANY_BELOW_GENUS_SIGNAL,
        FALSE
      ),
    
    SOURCE_CLASSIFICATION =
      case_when(
        
        N_OCCURRENCE_RECORDS == 0L ~
          "SOURCE_NAME_UNAVAILABLE",
        
        N_SOURCE_EVIDENCE_ROWS == 0L ~
          "SOURCE_NAME_UNAVAILABLE",
        
        ANY_INFRA_SIGNAL &
          ANY_SPECIES_SIGNAL ~
          "MIXED_SOURCE_RESOLUTION",
        
        ANY_INFRA_SIGNAL ~
          "INFRASPECIFIC_INFORMATION_PRESENT",
        
        ANY_SPECIES_SIGNAL ~
          "SPECIES_INFORMATION_PRESENT",
        
        TRUE ~
          "GENUS_ONLY_SOURCE"
      ),
    
    REVIEW_STATUS =
      case_when(
        
        SOURCE_CLASSIFICATION ==
          "GENUS_ONLY_SOURCE" ~
          "GENUS_SOURCE_SUPPORTED",
        
        SOURCE_CLASSIFICATION ==
          "SOURCE_NAME_UNAVAILABLE" ~
          "SOURCE_REVIEW_REQUIRED",
        
        TRUE ~
          "REQUIRES_RECONCILIATION_REVIEW"
      ),
    
    TAXONOMIC_ACTION_TAKEN =
      FALSE,
    
    STAR_ACTION_TAKEN =
      FALSE
  ) |>
  arrange(
    SOURCE_CLASSIFICATION,
    FINAL_WCVP_ID
  )


# ==============================================================================
# 32. CLASSIFICATION SUMMARY
# ==============================================================================

classification_summary <- genus_source_audit |>
  count(
    SOURCE_CLASSIFICATION,
    REVIEW_STATUS,
    name =
      "N_CONCEPTS"
  ) |>
  mutate(
    PERCENT_OF_823 =
      round(
        100 *
          N_CONCEPTS /
          EXPECTED_GENUS_CONCEPTS,
        2
      )
  ) |>
  arrange(
    desc(
      N_CONCEPTS
    ),
    SOURCE_CLASSIFICATION
  )


# ==============================================================================
# 33. FIELD-EVIDENCE SUMMARY
# ==============================================================================

field_evidence_summary <- source_long |>
  group_by(
    SOURCE_FIELD
  ) |>
  summarise(
    
    N_EVIDENCE_ROWS =
      n(),
    
    N_GENUS_CONCEPTS =
      n_distinct(
        FINAL_WCVP_ID
      ),
    
    N_DISTINCT_VALUES =
      n_distinct(
        SOURCE_VALUE_NORMALISED
      ),
    
    N_BELOW_GENUS_SIGNALS =
      sum(
        HAS_BELOW_GENUS_SIGNAL,
        na.rm = TRUE
      ),
    
    N_SPECIES_SIGNALS =
      sum(
        SOURCE_SIGNAL ==
          "SPECIES_INFORMATION_PRESENT",
        na.rm = TRUE
      ),
    
    N_INFRA_SIGNALS =
      sum(
        SOURCE_SIGNAL ==
          "INFRASPECIFIC_INFORMATION_PRESENT",
        na.rm = TRUE
      ),
    
    .groups =
      "drop"
  ) |>
  arrange(
    desc(
      N_GENUS_CONCEPTS
    ),
    desc(
      N_BELOW_GENUS_SIGNALS
    ),
    SOURCE_FIELD
  )


# ==============================================================================
# 34. VERIFY CANONICAL TABLE HAS NOT CHANGED
# ==============================================================================

canonical_after <- DBI::dbGetQuery(
  con,
  paste0(
    'SELECT * FROM "',
    TABLE_CANONICAL,
    '"'
  )
) |>
  as_tibble() |>
  mutate(
    FINAL_WCVP_ID =
      as.character(
        FINAL_WCVP_ID
      )
  )


# ==============================================================================
# 35. VALIDATION
# ==============================================================================

validation <- tibble(
  
  CHECK = c(
    
    "Canonical allocation table present",
    
    "Occurrence source table present",
    
    "Canonical population = 11,439",
    
    "Canonical population unique by WCVP ID",
    
    "Genus population = 823",
    
    "Genus population unique by WCVP ID",
    
    "Occurrence WCVP link field identified",
    
    "Candidate source-name fields identified",
    
    "Occurrence extraction returned FINAL_WCVP_ID",
    
    "02e concept audit = 823",
    
    "02e concept audit unique by WCVP ID",
    
    "All 823 concepts classified",
    
    "All 823 concepts assigned review status",
    
    "02e classification summary totals 823",
    
    "Canonical population remains 11,439",
    
    "Canonical WCVP IDs unchanged",
    
    "No canonical taxa removed",
    
    "No automatic taxonomic actions taken",
    
    "No automatic Star actions taken"
  ),
  
  PASS = c(
    
    TABLE_CANONICAL %in%
      db_tables,
    
    TABLE_OCCURRENCES %in%
      db_tables,
    
    nrow(canonical) ==
      EXPECTED_CANONICAL_POPULATION,
    
    n_distinct(
      canonical$FINAL_WCVP_ID
    ) ==
      EXPECTED_CANONICAL_POPULATION,
    
    nrow(genus_ids) ==
      EXPECTED_GENUS_CONCEPTS,
    
    n_distinct(
      genus_ids$FINAL_WCVP_ID
    ) ==
      EXPECTED_GENUS_CONCEPTS,
    
    !is.na(
      occurrence_id_field
    ) &&
      nzchar(
        occurrence_id_field
      ),
    
    length(
      candidate_source_fields
    ) > 0L,
    
    "FINAL_WCVP_ID" %in%
      names(genus_occurrences),
    
    nrow(
      genus_source_audit
    ) ==
      EXPECTED_GENUS_CONCEPTS,
    
    n_distinct(
      genus_source_audit$FINAL_WCVP_ID
    ) ==
      EXPECTED_GENUS_CONCEPTS,
    
    all(
      !is.na(
        genus_source_audit$SOURCE_CLASSIFICATION
      )
    ),
    
    all(
      !is.na(
        genus_source_audit$REVIEW_STATUS
      )
    ),
    
    sum(
      classification_summary$N_CONCEPTS
    ) ==
      EXPECTED_GENUS_CONCEPTS,
    
    nrow(
      canonical_after
    ) ==
      EXPECTED_CANONICAL_POPULATION,
    
    setequal(
      canonical$FINAL_WCVP_ID,
      canonical_after$FINAL_WCVP_ID
    ),
    
    nrow(
      anti_join(
        canonical,
        canonical_after,
        by =
          "FINAL_WCVP_ID"
      )
    ) ==
      0L,
    
    all(
      !genus_source_audit$TAXONOMIC_ACTION_TAKEN
    ),
    
    all(
      !genus_source_audit$STAR_ACTION_TAKEN
    )
  )
) |>
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
# 36. DISPLAY VALIDATION BEFORE WRITING
# ==============================================================================

cat("\n")
cat("============================================================\n")
cat("02e VALIDATION\n")
cat("============================================================\n\n")

print(
  validation,
  n = Inf
)


# ==============================================================================
# 37. FAIL SAFELY
# ==============================================================================

if (!all(validation$PASS)) {
  
  failed_checks <- validation |>
    filter(
      !PASS
    ) |>
    pull(
      CHECK
    )
  
  DBI::dbDisconnect(
    con,
    shutdown = TRUE
  )
  
  rm(con)
  
  stop(
    "\n02e VALIDATION FAILED:\n",
    paste(
      failed_checks,
      collapse = "\n"
    ),
    "\n\nNo 02e audit tables were written."
  )
}


# ==============================================================================
# 38. WRITE AUDIT TABLES TO DUCKDB
# ==============================================================================

cat(
  "\nWriting validated 02e audit tables to DuckDB...\n"
)

DBI::dbWriteTable(
  con,
  TABLE_02E_FIELDS,
  source_field_inventory,
  overwrite = TRUE
)

DBI::dbWriteTable(
  con,
  TABLE_02E_SOURCE_LONG,
  source_long,
  overwrite = TRUE
)

DBI::dbWriteTable(
  con,
  TABLE_02E_SOURCE_DISTINCT,
  source_distinct,
  overwrite = TRUE
)

DBI::dbWriteTable(
  con,
  TABLE_02E_AUDIT,
  genus_source_audit,
  overwrite = TRUE
)

DBI::dbWriteTable(
  con,
  TABLE_02E_SUMMARY,
  classification_summary,
  overwrite = TRUE
)

DBI::dbWriteTable(
  con,
  TABLE_02E_FIELD_SUMMARY,
  field_evidence_summary,
  overwrite = TRUE
)

DBI::dbWriteTable(
  con,
  TABLE_02E_VALIDATION,
  validation,
  overwrite = TRUE
)

cat("DuckDB audit tables written.\n")


# ==============================================================================
# 39. WRITE CSV OUTPUTS
# ==============================================================================

cat("Writing CSV outputs...\n")

write_csv(
  source_field_inventory,
  file.path(
    OUTPUT_DIR,
    "VPJD_02e_source_field_inventory.csv"
  )
)

write_csv(
  source_long,
  file.path(
    OUTPUT_DIR,
    "VPJD_02e_genus_source_name_long.csv"
  )
)

write_csv(
  source_distinct,
  file.path(
    OUTPUT_DIR,
    "VPJD_02e_genus_source_name_distinct.csv"
  )
)

write_csv(
  genus_source_audit,
  file.path(
    OUTPUT_DIR,
    "VPJD_02e_genus_source_name_audit.csv"
  )
)

write_csv(
  classification_summary,
  file.path(
    OUTPUT_DIR,
    "VPJD_02e_classification_summary.csv"
  )
)

write_csv(
  field_evidence_summary,
  file.path(
    OUTPUT_DIR,
    "VPJD_02e_field_evidence_summary.csv"
  )
)

write_csv(
  validation,
  file.path(
    OUTPUT_DIR,
    "VPJD_02e_validation.csv"
  )
)


# ==============================================================================
# 40. OUTPUT INVENTORY
# ==============================================================================

output_inventory <- tibble(
  
  FILE = c(
    
    "VPJD_02e_source_field_inventory.csv",
    
    "VPJD_02e_genus_source_name_long.csv",
    
    "VPJD_02e_genus_source_name_distinct.csv",
    
    "VPJD_02e_genus_source_name_audit.csv",
    
    "VPJD_02e_classification_summary.csv",
    
    "VPJD_02e_field_evidence_summary.csv",
    
    "VPJD_02e_validation.csv"
  ),
  
  PURPOSE = c(
    
    "Occurrence schema and candidate source-name fields",
    
    "Long-form source-name evidence for genus concepts",
    
    "Distinct source-name evidence and occurrence frequencies",
    
    "Primary 823-concept genus source-name audit",
    
    "Classification totals for the 823 genus concepts",
    
    "Evidence contribution by occurrence source field",
    
    "02e validation checks"
  )
)

write_csv(
  output_inventory,
  file.path(
    OUTPUT_DIR,
    "VPJD_02e_output_inventory.csv"
  )
)

cat("CSV outputs written.\n")


# ==============================================================================
# 41. FINAL RESULTS
# ==============================================================================

cat("\n")
cat("============================================================\n")
cat("02e RESULTS\n")
cat("============================================================\n\n")

cat(
  "Canonical VPJD population: ",
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
    nrow(genus_source_audit),
    big.mark = ","
  ),
  "\n",
  sep = ""
)

cat(
  "Genus-linked occurrence rows: ",
  format(
    nrow(genus_occurrences),
    big.mark = ","
  ),
  "\n",
  sep = ""
)

cat(
  "Genus concepts represented in occurrences: ",
  format(
    n_occurrence_concepts,
    big.mark = ","
  ),
  " / ",
  EXPECTED_GENUS_CONCEPTS,
  "\n",
  sep = ""
)

cat(
  "Candidate source-name fields: ",
  length(
    candidate_source_fields
  ),
  "\n",
  sep = ""
)

cat(
  "Non-empty source evidence rows: ",
  format(
    nrow(source_long),
    big.mark = ","
  ),
  "\n",
  sep = ""
)

cat(
  "Distinct source evidence combinations: ",
  format(
    nrow(source_distinct),
    big.mark = ","
  ),
  "\n\n",
  sep = ""
)


# ==============================================================================
# 42. CLASSIFICATION REPORT
# ==============================================================================

cat("------------------------------------------------------------\n")
cat("CLASSIFICATION SUMMARY\n")
cat("------------------------------------------------------------\n\n")

print(
  classification_summary,
  n = Inf
)


# ==============================================================================
# 43. SOURCE-FIELD REPORT
# ==============================================================================

cat("\n")
cat("------------------------------------------------------------\n")
cat("SOURCE-FIELD EVIDENCE SUMMARY\n")
cat("------------------------------------------------------------\n\n")

print(
  field_evidence_summary,
  n = Inf
)


# ==============================================================================
# 44. FINAL VALIDATION REPORT
# ==============================================================================

cat("\n")
cat("------------------------------------------------------------\n")
cat("VALIDATION\n")
cat("------------------------------------------------------------\n\n")

print(
  validation,
  n = Inf
)


# ==============================================================================
# 45. CLEAN DISCONNECT
# ==============================================================================

DBI::dbDisconnect(
  con,
  shutdown = TRUE
)

rm(con)

gc()


# ==============================================================================
# 46. COMPLETION REPORT
# ==============================================================================

cat("\n")
cat("============================================================\n")

cat(
  "VPJD TAXONOMIC REVISION 02e v",
  VERSION,
  " COMPLETE\n",
  sep = ""
)

cat("============================================================\n\n")

cat(
  "Output directory:\n",
  OUTPUT_DIR,
  "\n\n",
  sep = ""
)

cat(
  "Canonical population: ",
  format(
    EXPECTED_CANONICAL_POPULATION,
    big.mark = ","
  ),
  "\n",
  sep = ""
)

cat(
  "Genus concepts audited: ",
  format(
    EXPECTED_GENUS_CONCEPTS,
    big.mark = ","
  ),
  "\n",
  sep = ""
)

cat("Canonical taxa removed: 0\n")
cat("Canonical taxonomy modified: FALSE\n")
cat("Star allocations modified: FALSE\n")
cat("Automatic taxonomic resolutions: 0\n")

cat(
  "Validation: ",
  sum(validation$PASS),
  "/",
  nrow(validation),
  " PASS\n",
  sep = ""
)

cat("02e audit tables written: TRUE\n")
cat("CSV outputs written: TRUE\n")
cat("DuckDB connection closed cleanly.\n")

cat("\n")
cat("============================================================\n")


# ==============================================================================
# END
# ==============================================================================
