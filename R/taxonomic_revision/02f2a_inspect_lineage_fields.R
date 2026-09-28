# ==============================================================================
# VPJD TAXONOMIC REVISION
# 02f.2a — INSPECT LINEAGE / HIGHER-TAXONOMY CLASSIFICATION FIELDS
#
# Version: 0.1.0
#
# PURPOSE
# -------
# Diagnose the biological classification fields available to the VPJD
# taxonomic-revision workflow.
#
# CONTEXT
# -------
# 02f.1 established complete linkage:
#
#   concept-level WCVP-ID coverage:   11,439 / 11,439
#   family-level coverage:            11,439 / 11,439
#
# 02f.2 then demonstrated complete agreement between the two linkage routes,
# but selected a logical field whose value was FALSE for all 11,439 concepts.
#
# Therefore:
#
#   * linkage is NOT the current problem;
#   * field identification IS the current problem.
#
# THIS MODULE:
#
#   1. opens the canonical VPJD DuckDB read-only;
#   2. inventories tables relevant to 02b / lineage classification;
#   3. inventories all fields in those tables;
#   4. records database field types;
#   5. extracts distinct values and frequencies;
#   6. measures WCVP-ID and family linkage coverage where possible;
#   7. identifies potentially informative lineage/classification fields;
#   8. identifies uninformative logical / constant fields;
#   9. writes complete diagnostic CSV outputs;
#  10. modifies NOTHING.
#
# IMPORTANT
# ---------
# This script makes no taxonomic decisions.
# It does not remove taxa.
# It does not modify canonical taxonomy.
# It does not modify Star allocations.
#
# ==============================================================================


# ==============================================================================
# 01. PACKAGES
# ==============================================================================

required_packages <- c(
  "DBI",
  "duckdb",
  "dplyr",
  "tibble",
  "readr",
  "stringr",
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
    paste(
      missing_packages,
      collapse = ", "
    )
  )
}

library(DBI)
library(duckdb)
library(dplyr)
library(tibble)
library(readr)
library(stringr)
library(purrr)


# ==============================================================================
# 02. MODULE METADATA
# ==============================================================================

MODULE <-
  "02f2a_inspect_lineage_fields"

VERSION <-
  "0.1.0"

RUN_DATE <-
  Sys.Date()

EXPECTED_CANONICAL_POPULATION <-
  11439L

EXPECTED_FAMILIES <-
  260L


# ==============================================================================
# 03. PROJECT PATHS
# ==============================================================================

PROJECT_ROOT <-
  "I:/R/OJPCP/VPJD-OJPCP"


DB_PATH <- file.path(
  PROJECT_ROOT,
  "data",
  "interim",
  "occurrences",
  "vpjd_occurrences.duckdb"
)


DIAGNOSTIC_02F1_DIR <- file.path(
  PROJECT_ROOT,
  "outputs",
  "tables",
  "taxonomic_revision",
  "02f_infraspecific_other_ranks",
  "02f1_lineage_linkage_diagnostic"
)


OUTPUT_DIR <- file.path(
  PROJECT_ROOT,
  "outputs",
  "tables",
  "taxonomic_revision",
  "02f_infraspecific_other_ranks",
  "02f2a_lineage_field_inspection"
)


dir.create(
  OUTPUT_DIR,
  recursive = TRUE,
  showWarnings = FALSE
)


# ==============================================================================
# 04. REQUIRED 02f.1 FILES
# ==============================================================================

FILE_FIELD_INVENTORY <- file.path(
  DIAGNOSTIC_02F1_DIR,
  "VPJD_02f1_field_inventory.csv"
)


FILE_ID_TESTS <- file.path(
  DIAGNOSTIC_02F1_DIR,
  "VPJD_02f1_wcvp_id_linkage_tests.csv"
)


FILE_FAMILY_TESTS <- file.path(
  DIAGNOSTIC_02F1_DIR,
  "VPJD_02f1_family_linkage_tests.csv"
)


required_files <- c(
  FILE_FIELD_INVENTORY,
  FILE_ID_TESTS,
  FILE_FAMILY_TESTS
)


missing_files <- required_files[
  !file.exists(
    required_files
  )
]


if (length(missing_files) > 0L) {
  
  stop(
    "Required 02f.1 diagnostic file(s) missing:\n",
    paste(
      missing_files,
      collapse = "\n"
    )
  )
}


# ==============================================================================
# 05. CANONICAL TABLES
# ==============================================================================

TABLE_CANONICAL <-
  "vpjd_star_provisional_wholesale_allocation"

TABLE_WCVP <-
  "occurrence_wcvp_accepted_taxa"


# ==============================================================================
# 06. START REPORT
# ==============================================================================

cat("\n")
cat("============================================================\n")
cat("VPJD TAXONOMIC REVISION — 02f.2a\n")
cat("INSPECT LINEAGE / CLASSIFICATION FIELDS\n")
cat("============================================================\n\n")

cat(
  "Module: ",
  MODULE,
  "\n",
  sep = ""
)

cat(
  "Version: ",
  VERSION,
  "\n",
  sep = ""
)

cat(
  "Run date: ",
  RUN_DATE,
  "\n\n",
  sep = ""
)

cat("Database mode: READ ONLY\n")
cat("Taxonomic decisions: NONE\n")
cat("Canonical taxonomy modified: FALSE\n")
cat("Star allocations modified: FALSE\n\n")


# ==============================================================================
# 07. READ 02f.1 DIAGNOSTICS
# ==============================================================================

field_inventory_02f1 <- read_csv(
  FILE_FIELD_INVENTORY,
  show_col_types = FALSE
)


id_tests_02f1 <- read_csv(
  FILE_ID_TESTS,
  show_col_types = FALSE
)


family_tests_02f1 <- read_csv(
  FILE_FAMILY_TESTS,
  show_col_types = FALSE
)


# ==============================================================================
# 08. CLOSE ANY EXISTING DATABASE CONNECTION
# ==============================================================================

if (exists("con", envir = .GlobalEnv)) {
  
  old_con <- get(
    "con",
    envir = .GlobalEnv
  )
  
  old_valid <- tryCatch(
    DBI::dbIsValid(
      old_con
    ),
    error = function(e) FALSE
  )
  
  if (isTRUE(old_valid)) {
    
    try(
      DBI::dbDisconnect(
        old_con,
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
# 09. OPEN DUCKDB READ-ONLY
# ==============================================================================

if (!file.exists(DB_PATH)) {
  
  stop(
    "VPJD database not found:\n",
    DB_PATH
  )
}


cat(
  "Opening VPJD DuckDB read-only...\n"
)


con <- DBI::dbConnect(
  duckdb::duckdb(),
  dbdir = DB_PATH,
  read_only = TRUE
)


if (!DBI::dbIsValid(con)) {
  
  stop(
    "Could not establish a valid DuckDB connection."
  )
}


cat(
  "Connection established.\n\n"
)


db_tables <-
  DBI::dbListTables(con)


# ==============================================================================
# 10. CHECK CANONICAL TABLES
# ==============================================================================

required_tables <- c(
  TABLE_CANONICAL,
  TABLE_WCVP
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
  
  stop(
    "Required canonical table(s) missing: ",
    paste(
      missing_tables,
      collapse = ", "
    )
  )
}


# ==============================================================================
# 11. LOAD CANONICAL VPJD POPULATION
# ==============================================================================

canonical <- DBI::dbGetQuery(
  con,
  paste0(
    '
    SELECT
      CAST(FINAL_WCVP_ID AS VARCHAR) AS FINAL_WCVP_ID,
      FINAL_WCVP_RECOGNISED_NAME,
      FINAL_WCVP_RANK,
      FINAL_WCVP_STATUS,
      FINAL_WCVP_CONCEPT_CLASS,
      PROVISIONAL_STAR,
      IS_ALLOCATED,
      IS_UNRESOLVED
    FROM "',
    TABLE_CANONICAL,
    '"
    '
  )
) |>
  as_tibble()


if (
  nrow(canonical) !=
  EXPECTED_CANONICAL_POPULATION
) {
  
  stop(
    "Expected 11,439 canonical concepts; found ",
    nrow(canonical),
    "."
  )
}


if (
  n_distinct(
    canonical$FINAL_WCVP_ID
  ) !=
  EXPECTED_CANONICAL_POPULATION
) {
  
  stop(
    "Canonical population is not unique by FINAL_WCVP_ID."
  )
}


# ==============================================================================
# 12. LOAD WCVP FAMILY LOOKUP
# ==============================================================================

wcvp_fields <- DBI::dbListFields(
  con,
  TABLE_WCVP
)


required_wcvp_fields <- c(
  "wcvp_plant_name_id",
  "wcvp_family"
)


missing_wcvp_fields <- setdiff(
  required_wcvp_fields,
  wcvp_fields
)


if (length(missing_wcvp_fields) > 0L) {
  
  stop(
    "Required WCVP field(s) missing: ",
    paste(
      missing_wcvp_fields,
      collapse = ", "
    )
  )
}


wcvp_family_lookup <- DBI::dbGetQuery(
  con,
  paste0(
    '
    SELECT
      CAST(wcvp_plant_name_id AS VARCHAR) AS FINAL_WCVP_ID,
      CAST(wcvp_family AS VARCHAR) AS WCVP_FAMILY
    FROM "',
    TABLE_WCVP,
    '"
    '
  )
) |>
  as_tibble() |>
  distinct(
    FINAL_WCVP_ID,
    .keep_all = TRUE
  )


canonical <- canonical |>
  left_join(
    wcvp_family_lookup,
    by = "FINAL_WCVP_ID"
  )


n_families <-
  canonical |>
  filter(
    !is.na(WCVP_FAMILY),
    WCVP_FAMILY != ""
  ) |>
  summarise(
    N =
      n_distinct(
        WCVP_FAMILY
      )
  ) |>
  pull(N)


if (
  n_families !=
  EXPECTED_FAMILIES
) {
  
  stop(
    "Expected 260 represented families; found ",
    n_families,
    "."
  )
}


# ==============================================================================
# 13. IDENTIFY TABLES TO INSPECT
#
# Use the actual tables found by 02f.1 plus relevant taxonomic / lineage tables
# in the database.
# ==============================================================================

tables_from_02f1 <- unique(
  c(
    field_inventory_02f1$TABLE_NAME,
    id_tests_02f1$TABLE_NAME,
    family_tests_02f1$TABLE_NAME
  )
)


tables_from_02f1 <- tables_from_02f1[
  !is.na(
    tables_from_02f1
  ) &
    tables_from_02f1 != ""
]


name_pattern_tables <- db_tables[
  str_detect(
    str_to_lower(
      db_tables
    ),
    paste0(
      "02a|02b|lineage|vascular|",
      "classification|higher|major|",
      "taxon|taxonomy|family|wcvp"
    )
  )
]


tables_to_inspect <- sort(
  unique(
    c(
      tables_from_02f1,
      name_pattern_tables,
      TABLE_CANONICAL,
      TABLE_WCVP
    )
  )
)


tables_to_inspect <- intersect(
  tables_to_inspect,
  db_tables
)


cat(
  "Tables selected for inspection: ",
  length(
    tables_to_inspect
  ),
  "\n\n",
  sep = ""
)


# ==============================================================================
# 14. DATABASE TABLE INVENTORY
# ==============================================================================

get_table_count <- function(tbl) {
  
  tryCatch(
    
    DBI::dbGetQuery(
      con,
      paste0(
        'SELECT COUNT(*) AS N FROM "',
        tbl,
        '"'
      )
    )$N[[1]],
    
    error = function(e) NA_real_
  )
}


table_inventory <- tibble(
  TABLE_NAME =
    tables_to_inspect
) |>
  mutate(
    N_ROWS =
      map_dbl(
        TABLE_NAME,
        get_table_count
      ),
    FROM_02F1 =
      TABLE_NAME %in%
      tables_from_02f1
  )


# ==============================================================================
# 15. COMPLETE FIELD INVENTORY
# ==============================================================================

get_table_fields <- function(tbl) {
  
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
        TABLE_NAME =
          character(),
        FIELD_NAME =
          character()
      )
    )
  }
  
  
  tibble(
    TABLE_NAME =
      tbl,
    FIELD_NAME =
      fields
  )
}


field_inventory <- map_dfr(
  tables_to_inspect,
  get_table_fields
)


# ==============================================================================
# 16. GET DUCKDB FIELD TYPES
# ==============================================================================

get_table_schema <- function(tbl) {
  
  sql <- paste0(
    "DESCRIBE SELECT * FROM \"",
    tbl,
    "\""
  )
  
  
  out <- tryCatch(
    
    DBI::dbGetQuery(
      con,
      sql
    ) |>
      as_tibble(),
    
    error = function(e) NULL
  )
  
  
  if (is.null(out)) {
    
    return(
      tibble(
        TABLE_NAME =
          character(),
        FIELD_NAME =
          character(),
        FIELD_TYPE =
          character()
      )
    )
  }
  
  
  name_col <- names(out)[
    str_to_lower(
      names(out)
    ) %in%
      c(
        "column_name",
        "name"
      )
  ][1]
  
  
  type_col <- names(out)[
    str_to_lower(
      names(out)
    ) %in%
      c(
        "column_type",
        "type"
      )
  ][1]
  
  
  if (
    is.na(name_col) ||
    is.na(type_col)
  ) {
    
    return(
      tibble(
        TABLE_NAME =
          character(),
        FIELD_NAME =
          character(),
        FIELD_TYPE =
          character()
      )
    )
  }
  
  
  tibble(
    TABLE_NAME =
      tbl,
    FIELD_NAME =
      as.character(
        out[[name_col]]
      ),
    FIELD_TYPE =
      as.character(
        out[[type_col]]
      )
  )
}


schema_inventory <- map_dfr(
  tables_to_inspect,
  get_table_schema
)


field_inventory <- field_inventory |>
  left_join(
    schema_inventory,
    by = c(
      "TABLE_NAME",
      "FIELD_NAME"
    )
  )


# ==============================================================================
# 17. CLASSIFY FIELD NAMES
# ==============================================================================

field_inventory <- field_inventory |>
  mutate(
    
    FIELD_NAME_LOWER =
      str_to_lower(
        FIELD_NAME
      ),
    
    IS_ID_FIELD =
      str_detect(
        FIELD_NAME_LOWER,
        "id$|_id$|wcvp.*id|plant.*name.*id"
      ),
    
    IS_FAMILY_FIELD =
      str_detect(
        FIELD_NAME_LOWER,
        "family"
      ),
    
    IS_RANK_FIELD =
      str_detect(
        FIELD_NAME_LOWER,
        "rank"
      ),
    
    IS_STATUS_FIELD =
      str_detect(
        FIELD_NAME_LOWER,
        "status"
      ),
    
    IS_LINEAGE_NAME_FIELD =
      str_detect(
        FIELD_NAME_LOWER,
        paste0(
          "lineage|vascular|non.?vascular|",
          "major.?group|higher.?group|",
          "plant.?group|taxonomic.?group|",
          "classification|division|phylum|",
          "class$|subclass|clade|kingdom"
        )
      ),
    
    IS_TAXONOMIC_CONTEXT_FIELD =
      str_detect(
        FIELD_NAME_LOWER,
        paste0(
          "family|genus|species|rank|",
          "taxon|taxonomy|name|status"
        )
      ),
    
    IS_LOGICAL_TYPE =
      str_detect(
        str_to_upper(
          coalesce(
            FIELD_TYPE,
            ""
          )
        ),
        "BOOLEAN|LOGICAL|BOOL"
      )
  )


# ==============================================================================
# 18. FIELD VALUE PROFILING FUNCTION
#
# IMPORTANT:
# CAST values to VARCHAR before returning them to R.
# This avoids the previous bind_rows() problem caused by mixed logical,
# numeric and character VALUE columns.
# ==============================================================================

profile_field <- function(
    tbl,
    fld
) {
  
  sql <- paste0(
    '
    SELECT
      CAST("',
    fld,
    '" AS VARCHAR) AS VALUE,
      COUNT(*) AS N_ROWS
    FROM "',
    tbl,
    '"
    GROUP BY 1
    ORDER BY N_ROWS DESC
    '
  )
  
  
  out <- tryCatch(
    
    DBI::dbGetQuery(
      con,
      sql
    ) |>
      as_tibble(),
    
    error = function(e) {
      
      return(
        tibble(
          VALUE =
            character(),
          N_ROWS =
            numeric()
        )
      )
    }
  )
  
  
  if (nrow(out) == 0L) {
    
    return(
      tibble(
        TABLE_NAME =
          character(),
        FIELD_NAME =
          character(),
        VALUE =
          character(),
        N_ROWS =
          numeric()
      )
    )
  }
  
  
  out |>
    mutate(
      VALUE =
        as.character(
          VALUE
        ),
      TABLE_NAME =
        tbl,
      FIELD_NAME =
        fld,
      .before = 1
    )
}


# ==============================================================================
# 19. PROFILE ALL FIELDS
#
# This may take a little time because we are intentionally inspecting the
# contents rather than relying on field names.
# ==============================================================================

cat(
  "Profiling field values...\n"
)


field_values <- pmap_dfr(
  
  field_inventory |>
    select(
      TABLE_NAME,
      FIELD_NAME
    ),
  
  function(
    TABLE_NAME,
    FIELD_NAME
  ) {
    
    profile_field(
      TABLE_NAME,
      FIELD_NAME
    )
  }
)


cat(
  "Field-value profiling complete.\n\n"
)


# ==============================================================================
# 20. FIELD-LEVEL STATISTICS
# ==============================================================================

field_statistics <- field_values |>
  group_by(
    TABLE_NAME,
    FIELD_NAME
  ) |>
  summarise(
    
    N_DISTINCT_VALUES =
      n(),
    
    N_NON_MISSING_VALUES =
      sum(
        !is.na(VALUE) &
          VALUE != ""
      ),
    
    N_ROWS_NON_MISSING =
      sum(
        N_ROWS[
          !is.na(VALUE) &
            VALUE != ""
        ],
        na.rm = TRUE
      ),
    
    .groups = "drop"
  )


field_inventory <- field_inventory |>
  left_join(
    field_statistics,
    by = c(
      "TABLE_NAME",
      "FIELD_NAME"
    )
  ) |>
  mutate(
    
    N_DISTINCT_VALUES =
      coalesce(
        N_DISTINCT_VALUES,
        0L
      ),
    
    N_NON_MISSING_VALUES =
      coalesce(
        N_NON_MISSING_VALUES,
        0L
      ),
    
    N_ROWS_NON_MISSING =
      coalesce(
        N_ROWS_NON_MISSING,
        0
      ),
    
    IS_CONSTANT_FIELD =
      N_NON_MISSING_VALUES ==
      1L
  )


# ==============================================================================
# 21. IDENTIFY BIOLOGICALLY INFORMATIVE VALUES
# ==============================================================================

BIOLOGICAL_PATTERN <- paste0(
  "vascular|non.?vascular|tracheoph|bryoph|",
  "angiosperm|gymnosperm|monocot|eudicot|",
  "fern|lycoph|pterid|moss|liverwort|hornwort|",
  "magnoliid|chloranth|ceratophyll|",
  "spermatoph|embryoph|plant|",
  "division|phylum|class|lineage|clade"
)


biological_value_hits <- field_values |>
  filter(
    !is.na(VALUE),
    VALUE != "",
    str_detect(
      str_to_lower(
        VALUE
      ),
      BIOLOGICAL_PATTERN
    )
  ) |>
  arrange(
    TABLE_NAME,
    FIELD_NAME,
    desc(
      N_ROWS
    )
  )


biological_hit_summary <- biological_value_hits |>
  group_by(
    TABLE_NAME,
    FIELD_NAME
  ) |>
  summarise(
    
    N_BIOLOGICAL_VALUES =
      n_distinct(
        VALUE
      ),
    
    N_ROWS_WITH_BIOLOGICAL_VALUE =
      sum(
        N_ROWS,
        na.rm = TRUE
      ),
    
    EXAMPLE_VALUES =
      paste(
        head(
          unique(VALUE),
          10
        ),
        collapse = " | "
      ),
    
    .groups = "drop"
  )


field_inventory <- field_inventory |>
  left_join(
    biological_hit_summary,
    by = c(
      "TABLE_NAME",
      "FIELD_NAME"
    )
  ) |>
  mutate(
    
    N_BIOLOGICAL_VALUES =
      coalesce(
        N_BIOLOGICAL_VALUES,
        0L
      ),
    
    N_ROWS_WITH_BIOLOGICAL_VALUE =
      coalesce(
        N_ROWS_WITH_BIOLOGICAL_VALUE,
        0
      )
  )


# ==============================================================================
# 22. IDENTIFY FALSE / TRUE CONSTANT FIELDS
#
# This should expose the type of field incorrectly selected by 02f.2.
# ==============================================================================

logical_like_fields <- field_values |>
  mutate(
    
    VALUE_NORMALISED =
      str_to_upper(
        str_trim(
          coalesce(
            VALUE,
            ""
          )
        )
      )
  ) |>
  group_by(
    TABLE_NAME,
    FIELD_NAME
  ) |>
  summarise(
    
    N_VALUES =
      n_distinct(
        VALUE_NORMALISED[
          VALUE_NORMALISED != ""
        ]
      ),
    
    ALL_VALUES_LOGICAL_LIKE =
      all(
        VALUE_NORMALISED[
          VALUE_NORMALISED != ""
        ] %in%
          c(
            "TRUE",
            "FALSE",
            "T",
            "F",
            "0",
            "1"
          )
      ),
    
    DISTINCT_VALUES =
      paste(
        sort(
          unique(
            VALUE_NORMALISED[
              VALUE_NORMALISED != ""
            ]
          )
        ),
        collapse = " | "
      ),
    
    .groups = "drop"
  ) |>
  filter(
    ALL_VALUES_LOGICAL_LIKE
  )


# ==============================================================================
# 23. IDENTIFY CANDIDATE CONCEPT-ID FIELDS
# ==============================================================================

candidate_id_fields <- field_inventory |>
  filter(
    IS_ID_FIELD
  ) |>
  select(
    TABLE_NAME,
    FIELD_NAME,
    FIELD_TYPE
  )


# ==============================================================================
# 24. TEST CONCEPT-LEVEL WCVP COVERAGE
# ==============================================================================

test_id_coverage <- function(
    tbl,
    fld
) {
  
  sql <- paste0(
    '
    SELECT DISTINCT
      CAST("',
    fld,
    '" AS VARCHAR) AS LINK_ID
    FROM "',
    tbl,
    '"
    WHERE "',
    fld,
    '" IS NOT NULL
    '
  )
  
  
  ids <- tryCatch(
    
    DBI::dbGetQuery(
      con,
      sql
    ) |>
      as_tibble(),
    
    error = function(e) NULL
  )
  
  
  if (is.null(ids)) {
    
    return(
      tibble(
        TABLE_NAME =
          tbl,
        FIELD_NAME =
          fld,
        N_DISTINCT_IDS =
          NA_integer_,
        N_CANONICAL_MATCHES =
          NA_integer_,
        PERCENT_CANONICAL =
          NA_real_
      )
    )
  }
  
  
  ids <- ids |>
    mutate(
      LINK_ID =
        str_trim(
          as.character(
            LINK_ID
          )
        )
    ) |>
    filter(
      !is.na(LINK_ID),
      LINK_ID != ""
    ) |>
    distinct(
      LINK_ID
    )
  
  
  n_matches <- canonical |>
    semi_join(
      ids,
      by = c(
        "FINAL_WCVP_ID" =
          "LINK_ID"
      )
    ) |>
    nrow()
  
  
  tibble(
    
    TABLE_NAME =
      tbl,
    
    FIELD_NAME =
      fld,
    
    N_DISTINCT_IDS =
      nrow(ids),
    
    N_CANONICAL_MATCHES =
      n_matches,
    
    PERCENT_CANONICAL =
      round(
        100 *
          n_matches /
          EXPECTED_CANONICAL_POPULATION,
        3
      )
  )
}


id_coverage <- pmap_dfr(
  
  candidate_id_fields |>
    select(
      TABLE_NAME,
      FIELD_NAME
    ),
  
  function(
    TABLE_NAME,
    FIELD_NAME
  ) {
    
    test_id_coverage(
      TABLE_NAME,
      FIELD_NAME
    )
  }
)


# ==============================================================================
# 25. IDENTIFY FAMILY FIELDS
# ==============================================================================

candidate_family_fields <- field_inventory |>
  filter(
    IS_FAMILY_FIELD
  ) |>
  select(
    TABLE_NAME,
    FIELD_NAME,
    FIELD_TYPE
  )


# ==============================================================================
# 26. TEST FAMILY COVERAGE
# ==============================================================================

test_family_coverage <- function(
    tbl,
    fld
) {
  
  sql <- paste0(
    '
    SELECT DISTINCT
      CAST("',
    fld,
    '" AS VARCHAR) AS FAMILY_VALUE
    FROM "',
    tbl,
    '"
    WHERE "',
    fld,
    '" IS NOT NULL
    '
  )
  
  
  families <- tryCatch(
    
    DBI::dbGetQuery(
      con,
      sql
    ) |>
      as_tibble(),
    
    error = function(e) NULL
  )
  
  
  if (is.null(families)) {
    
    return(
      tibble(
        TABLE_NAME =
          tbl,
        FIELD_NAME =
          fld,
        N_DISTINCT_FAMILIES =
          NA_integer_,
        N_VPJD_FAMILY_MATCHES =
          NA_integer_,
        N_CONCEPTS_COVERED =
          NA_integer_
      )
    )
  }
  
  
  families <- families |>
    mutate(
      FAMILY_VALUE =
        str_trim(
          as.character(
            FAMILY_VALUE
          )
        )
    ) |>
    filter(
      !is.na(FAMILY_VALUE),
      FAMILY_VALUE != ""
    ) |>
    distinct(
      FAMILY_VALUE
    )
  
  
  vpjd_families <- canonical |>
    filter(
      !is.na(WCVP_FAMILY),
      WCVP_FAMILY != ""
    ) |>
    distinct(
      WCVP_FAMILY
    )
  
  
  matched_families <- vpjd_families |>
    semi_join(
      families,
      by = c(
        "WCVP_FAMILY" =
          "FAMILY_VALUE"
      )
    )
  
  
  covered_concepts <- canonical |>
    semi_join(
      matched_families,
      by = "WCVP_FAMILY"
    )
  
  
  tibble(
    
    TABLE_NAME =
      tbl,
    
    FIELD_NAME =
      fld,
    
    N_DISTINCT_FAMILIES =
      nrow(families),
    
    N_VPJD_FAMILY_MATCHES =
      nrow(matched_families),
    
    N_CONCEPTS_COVERED =
      nrow(covered_concepts)
  )
}


family_coverage <- pmap_dfr(
  
  candidate_family_fields |>
    select(
      TABLE_NAME,
      FIELD_NAME
    ),
  
  function(
    TABLE_NAME,
    FIELD_NAME
  ) {
    
    test_family_coverage(
      TABLE_NAME,
      FIELD_NAME
    )
  }
)


# ==============================================================================
# 27. SCORE FIELDS FOR MANUAL REVIEW
#
# This is NOT an automatic taxonomic selection.
# It simply prioritises fields worth inspecting.
# ==============================================================================

review_inventory <- field_inventory |>
  mutate(
    
    SCORE_FIELD_NAME =
      case_when(
        
        str_detect(
          FIELD_NAME_LOWER,
          "vascular"
        ) ~ 100L,
        
        str_detect(
          FIELD_NAME_LOWER,
          "lineage"
        ) ~ 90L,
        
        str_detect(
          FIELD_NAME_LOWER,
          "major.?group"
        ) ~ 85L,
        
        str_detect(
          FIELD_NAME_LOWER,
          "higher.?group"
        ) ~ 80L,
        
        str_detect(
          FIELD_NAME_LOWER,
          "classification"
        ) ~ 75L,
        
        str_detect(
          FIELD_NAME_LOWER,
          "division|phylum"
        ) ~ 70L,
        
        str_detect(
          FIELD_NAME_LOWER,
          "class"
        ) ~ 60L,
        
        str_detect(
          FIELD_NAME_LOWER,
          "family"
        ) ~ 30L,
        
        TRUE ~ 0L
      ),
    
    SCORE_VALUES =
      case_when(
        
        N_BIOLOGICAL_VALUES >=
          2L ~ 100L,
        
        N_BIOLOGICAL_VALUES ==
          1L ~ 50L,
        
        TRUE ~ 0L
      ),
    
    PENALTY_LOGICAL =
      if_else(
        IS_LOGICAL_TYPE,
        100L,
        0L
      ),
    
    PENALTY_CONSTANT =
      if_else(
        IS_CONSTANT_FIELD,
        50L,
        0L
      ),
    
    REVIEW_SCORE =
      SCORE_FIELD_NAME +
      SCORE_VALUES -
      PENALTY_LOGICAL -
      PENALTY_CONSTANT
  ) |>
  arrange(
    desc(
      REVIEW_SCORE
    ),
    desc(
      N_BIOLOGICAL_VALUES
    ),
    TABLE_NAME,
    FIELD_NAME
  )


# ==============================================================================
# 28. HIGH-PRIORITY FIELD REVIEW
# ==============================================================================

high_priority_fields <- review_inventory |>
  filter(
    
    IS_LINEAGE_NAME_FIELD |
      N_BIOLOGICAL_VALUES >
      0L |
      
      (
        !IS_LOGICAL_TYPE &
          N_DISTINCT_VALUES >
          1L &
          N_DISTINCT_VALUES <=
          100L
      )
  ) |>
  select(
    TABLE_NAME,
    FIELD_NAME,
    FIELD_TYPE,
    N_DISTINCT_VALUES,
    N_ROWS_NON_MISSING,
    IS_LINEAGE_NAME_FIELD,
    IS_LOGICAL_TYPE,
    IS_CONSTANT_FIELD,
    N_BIOLOGICAL_VALUES,
    N_ROWS_WITH_BIOLOGICAL_VALUE,
    EXAMPLE_VALUES,
    REVIEW_SCORE
  ) |>
  arrange(
    desc(
      REVIEW_SCORE
    ),
    TABLE_NAME,
    FIELD_NAME
  )


# ==============================================================================
# 29. VALUES FOR HIGH-PRIORITY FIELDS
# ==============================================================================

high_priority_values <- field_values |>
  semi_join(
    high_priority_fields |>
      select(
        TABLE_NAME,
        FIELD_NAME
      ),
    by = c(
      "TABLE_NAME",
      "FIELD_NAME"
    )
  ) |>
  arrange(
    TABLE_NAME,
    FIELD_NAME,
    desc(
      N_ROWS
    )
  )


# ==============================================================================
# 30. PREVIOUS 02f.2 FAILURE SIGNATURE
#
# Explicitly identify fields whose only substantive value is FALSE.
# ==============================================================================

false_constant_fields <- field_values |>
  mutate(
    
    VALUE_NORMALISED =
      str_to_upper(
        str_trim(
          coalesce(
            VALUE,
            ""
          )
        )
      )
  ) |>
  filter(
    VALUE_NORMALISED != ""
  ) |>
  group_by(
    TABLE_NAME,
    FIELD_NAME
  ) |>
  summarise(
    
    N_DISTINCT =
      n_distinct(
        VALUE_NORMALISED
      ),
    
    ONLY_VALUE =
      if_else(
        N_DISTINCT ==
          1L,
        first(
          VALUE_NORMALISED
        ),
        NA_character_
      ),
    
    N_ROWS =
      sum(
        N_ROWS,
        na.rm = TRUE
      ),
    
    .groups = "drop"
  ) |>
  filter(
    N_DISTINCT ==
      1L,
    ONLY_VALUE ==
      "FALSE"
  )


# ==============================================================================
# 31. TABLE / FIELD SUMMARY
# ==============================================================================

table_field_summary <- field_inventory |>
  group_by(
    TABLE_NAME
  ) |>
  summarise(
    
    N_FIELDS =
      n(),
    
    N_LINEAGE_NAME_FIELDS =
      sum(
        IS_LINEAGE_NAME_FIELD,
        na.rm = TRUE
      ),
    
    N_BIOLOGICALLY_INFORMATIVE_FIELDS =
      sum(
        N_BIOLOGICAL_VALUES >
          0L,
        na.rm = TRUE
      ),
    
    N_LOGICAL_FIELDS =
      sum(
        IS_LOGICAL_TYPE,
        na.rm = TRUE
      ),
    
    N_CONSTANT_FIELDS =
      sum(
        IS_CONSTANT_FIELD,
        na.rm = TRUE
      ),
    
    .groups = "drop"
  ) |>
  left_join(
    table_inventory,
    by = "TABLE_NAME"
  ) |>
  arrange(
    desc(
      N_BIOLOGICALLY_INFORMATIVE_FIELDS
    ),
    desc(
      N_LINEAGE_NAME_FIELDS
    ),
    TABLE_NAME
  )


# ==============================================================================
# 32. DIAGNOSTIC SUMMARY
# ==============================================================================

diagnostic_summary <- tibble(
  
  METRIC = c(
    
    "Canonical VPJD concepts",
    
    "Canonical represented families",
    
    "Database tables inspected",
    
    "Fields inspected",
    
    "Fields with lineage/classification-like names",
    
    "Fields containing biological terminology",
    
    "High-priority fields for manual review",
    
    "Logical-like fields",
    
    "Constant FALSE fields",
    
    "Taxonomic changes made",
    
    "Star allocation changes made"
  ),
  
  N = c(
    
    nrow(canonical),
    
    n_families,
    
    length(
      tables_to_inspect
    ),
    
    nrow(
      field_inventory
    ),
    
    sum(
      field_inventory$IS_LINEAGE_NAME_FIELD,
      na.rm = TRUE
    ),
    
    sum(
      field_inventory$N_BIOLOGICAL_VALUES >
        0L,
      na.rm = TRUE
    ),
    
    nrow(
      high_priority_fields
    ),
    
    nrow(
      logical_like_fields
    ),
    
    nrow(
      false_constant_fields
    ),
    
    0L,
    
    0L
  )
)


# ==============================================================================
# 33. VALIDATION
# ==============================================================================

validation <- tibble(
  
  CHECK = c(
    
    "Canonical population = 11,439",
    
    "Canonical population unique by WCVP ID",
    
    "Canonical represented families = 260",
    
    "02f.1 field inventory loaded",
    
    "02f.1 WCVP-ID linkage tests loaded",
    
    "02f.1 family linkage tests loaded",
    
    "At least one database table inspected",
    
    "At least one field inspected",
    
    "Field-value profiling completed",
    
    "No taxonomic changes made",
    
    "No Star allocation changes made"
  ),
  
  PASS = c(
    
    nrow(canonical) ==
      EXPECTED_CANONICAL_POPULATION,
    
    n_distinct(
      canonical$FINAL_WCVP_ID
    ) ==
      EXPECTED_CANONICAL_POPULATION,
    
    n_families ==
      EXPECTED_FAMILIES,
    
    nrow(
      field_inventory_02f1
    ) >
      0L,
    
    nrow(
      id_tests_02f1
    ) >
      0L,
    
    nrow(
      family_tests_02f1
    ) >
      0L,
    
    length(
      tables_to_inspect
    ) >
      0L,
    
    nrow(
      field_inventory
    ) >
      0L,
    
    nrow(
      field_values
    ) >
      0L,
    
    TRUE,
    
    TRUE
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
# 34. WRITE OUTPUTS
# ==============================================================================

write_csv(
  table_inventory,
  file.path(
    OUTPUT_DIR,
    "VPJD_02f2a_table_inventory.csv"
  )
)


write_csv(
  field_inventory,
  file.path(
    OUTPUT_DIR,
    "VPJD_02f2a_complete_field_inventory.csv"
  )
)


write_csv(
  field_values,
  file.path(
    OUTPUT_DIR,
    "VPJD_02f2a_all_field_values.csv"
  )
)


write_csv(
  biological_value_hits,
  file.path(
    OUTPUT_DIR,
    "VPJD_02f2a_biological_value_hits.csv"
  )
)


write_csv(
  biological_hit_summary,
  file.path(
    OUTPUT_DIR,
    "VPJD_02f2a_biological_field_summary.csv"
  )
)


write_csv(
  logical_like_fields,
  file.path(
    OUTPUT_DIR,
    "VPJD_02f2a_logical_like_fields.csv"
  )
)


write_csv(
  false_constant_fields,
  file.path(
    OUTPUT_DIR,
    "VPJD_02f2a_constant_false_fields.csv"
  )
)


write_csv(
  id_coverage,
  file.path(
    OUTPUT_DIR,
    "VPJD_02f2a_wcvp_id_coverage.csv"
  )
)


write_csv(
  family_coverage,
  file.path(
    OUTPUT_DIR,
    "VPJD_02f2a_family_coverage.csv"
  )
)


write_csv(
  review_inventory,
  file.path(
    OUTPUT_DIR,
    "VPJD_02f2a_field_review_ranking.csv"
  )
)


write_csv(
  high_priority_fields,
  file.path(
    OUTPUT_DIR,
    "VPJD_02f2a_HIGH_PRIORITY_FIELDS.csv"
  )
)


write_csv(
  high_priority_values,
  file.path(
    OUTPUT_DIR,
    "VPJD_02f2a_HIGH_PRIORITY_FIELD_VALUES.csv"
  )
)


write_csv(
  table_field_summary,
  file.path(
    OUTPUT_DIR,
    "VPJD_02f2a_table_field_summary.csv"
  )
)


write_csv(
  diagnostic_summary,
  file.path(
    OUTPUT_DIR,
    "VPJD_02f2a_diagnostic_summary.csv"
  )
)


write_csv(
  validation,
  file.path(
    OUTPUT_DIR,
    "VPJD_02f2a_validation.csv"
  )
)


# ==============================================================================
# 35. OUTPUT INVENTORY
# ==============================================================================

output_inventory <- tibble(
  
  FILE = c(
    
    "VPJD_02f2a_table_inventory.csv",
    
    "VPJD_02f2a_complete_field_inventory.csv",
    
    "VPJD_02f2a_all_field_values.csv",
    
    "VPJD_02f2a_biological_value_hits.csv",
    
    "VPJD_02f2a_biological_field_summary.csv",
    
    "VPJD_02f2a_logical_like_fields.csv",
    
    "VPJD_02f2a_constant_false_fields.csv",
    
    "VPJD_02f2a_wcvp_id_coverage.csv",
    
    "VPJD_02f2a_family_coverage.csv",
    
    "VPJD_02f2a_field_review_ranking.csv",
    
    "VPJD_02f2a_HIGH_PRIORITY_FIELDS.csv",
    
    "VPJD_02f2a_HIGH_PRIORITY_FIELD_VALUES.csv",
    
    "VPJD_02f2a_table_field_summary.csv",
    
    "VPJD_02f2a_diagnostic_summary.csv",
    
    "VPJD_02f2a_validation.csv"
  ),
  
  PURPOSE = c(
    
    "Database tables inspected",
    
    "Complete field names, types and diagnostic characteristics",
    
    "All distinct field values and frequencies",
    
    "Values containing potentially biological lineage terminology",
    
    "Summary of fields containing biological terminology",
    
    "Fields whose values behave like logical flags",
    
    "Fields containing only FALSE; identifies likely 02f.2 failure candidates",
    
    "Canonical WCVP-ID linkage coverage",
    
    "Canonical family linkage coverage",
    
    "All fields ranked for diagnostic review",
    
    "Shortlist of fields most likely to contain useful lineage information",
    
    "Distinct values and counts for high-priority fields",
    
    "Summary of potentially informative fields by table",
    
    "02f.2a numerical diagnostic summary",
    
    "02f.2a validation record"
  )
)


write_csv(
  output_inventory,
  file.path(
    OUTPUT_DIR,
    "VPJD_02f2a_output_inventory.csv"
  )
)


# ==============================================================================
# 36. CONSOLE REPORT — DIAGNOSTIC SUMMARY
# ==============================================================================

cat("\n")
cat("============================================================\n")
cat("02f.2a DIAGNOSTIC SUMMARY\n")
cat("============================================================\n\n")


print(
  diagnostic_summary,
  n = Inf
)


# ==============================================================================
# 37. CONSOLE REPORT — TABLE SUMMARY
# ==============================================================================

cat("\n")
cat("------------------------------------------------------------\n")
cat("TABLE / FIELD SUMMARY\n")
cat("------------------------------------------------------------\n\n")


print(
  table_field_summary,
  n = Inf,
  width = Inf
)


# ==============================================================================
# 38. CONSOLE REPORT — HIGH-PRIORITY FIELDS
# ==============================================================================

cat("\n")
cat("------------------------------------------------------------\n")
cat("HIGH-PRIORITY LINEAGE / CLASSIFICATION FIELDS\n")
cat("------------------------------------------------------------\n\n")


if (
  nrow(
    high_priority_fields
  ) ==
  0L
) {
  
  cat(
    "No high-priority fields identified.\n"
  )
  
} else {
  
  print(
    high_priority_fields,
    n = Inf,
    width = Inf
  )
}


# ==============================================================================
# 39. CONSOLE REPORT — VALUES FOR HIGH-PRIORITY FIELDS
# ==============================================================================

cat("\n")
cat("------------------------------------------------------------\n")
cat("HIGH-PRIORITY FIELD VALUES\n")
cat("------------------------------------------------------------\n\n")


if (
  nrow(
    high_priority_values
  ) ==
  0L
) {
  
  cat(
    "No high-priority field values identified.\n"
  )
  
} else {
  
  print(
    high_priority_values,
    n = Inf,
    width = Inf
  )
}


# ==============================================================================
# 40. CONSOLE REPORT — CONSTANT FALSE FIELDS
# ==============================================================================

cat("\n")
cat("------------------------------------------------------------\n")
cat("CONSTANT FALSE FIELDS\n")
cat("------------------------------------------------------------\n\n")


if (
  nrow(
    false_constant_fields
  ) ==
  0L
) {
  
  cat(
    "No constant FALSE fields identified.\n"
  )
  
} else {
  
  print(
    false_constant_fields,
    n = Inf,
    width = Inf
  )
}


# ==============================================================================
# 41. CONSOLE REPORT — BIOLOGICAL VALUE HITS
# ==============================================================================

cat("\n")
cat("------------------------------------------------------------\n")
cat("FIELDS CONTAINING BIOLOGICAL / LINEAGE TERMINOLOGY\n")
cat("------------------------------------------------------------\n\n")


if (
  nrow(
    biological_hit_summary
  ) ==
  0L
) {
  
  cat(
    "No biological terminology detected by diagnostic patterns.\n"
  )
  
} else {
  
  print(
    biological_hit_summary,
    n = Inf,
    width = Inf
  )
}


# ==============================================================================
# 42. VALIDATION REPORT
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
  "VPJD TAXONOMIC REVISION 02f.2a v",
  VERSION,
  " COMPLETE\n",
  sep = ""
)

cat("============================================================\n\n")


cat(
  "Tables inspected: ",
  length(
    tables_to_inspect
  ),
  "\n",
  sep = ""
)


cat(
  "Fields inspected: ",
  nrow(
    field_inventory
  ),
  "\n",
  sep = ""
)


cat(
  "High-priority fields: ",
  nrow(
    high_priority_fields
  ),
  "\n",
  sep = ""
)


cat(
  "Fields containing biological terminology: ",
  nrow(
    biological_hit_summary
  ),
  "\n",
  sep = ""
)


cat(
  "Constant FALSE fields: ",
  nrow(
    false_constant_fields
  ),
  "\n",
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
  " PASS\n\n",
  sep = ""
)


cat("Database modified: FALSE\n")
cat("Canonical taxonomy modified: FALSE\n")
cat("Star allocations modified: FALSE\n")
cat("Taxa removed: 0\n")


cat(
  "\nOutput directory:\n",
  OUTPUT_DIR,
  "\n",
  sep = ""
)


cat("\nDuckDB connection closed cleanly.\n")
cat("============================================================\n")


# ==============================================================================
# END
# ==============================================================================