# ==============================================================================
# VPJD TAXONOMIC REVISION
# 02f.1 — DIAGNOSE 02b LINEAGE / VASCULAR CLASSIFICATION LINKAGE
#
# Version: 0.1.1
#
# PURPOSE
# -------
# Diagnose why 02f was unable to attach the validated 02b higher-lineage /
# vascular/non-vascular classification to the 11,439 canonical VPJD concepts.
#
# KNOWN STATE
# -----------
# 02b previously validated:
#
#   * canonical VPJD population: 11,439
#   * represented families: 260
#   * all 260 families explicitly classified
#   * no unresolved families
#   * no unresolved concepts
#   * all concepts assigned a recognised major group
#   * vascular + non-vascular population = 11,439
#
# 02f subsequently reported:
#
#   * non-vascular concepts identified: 0
#   * concepts lacking attached vascular classification: 11,439
#
# This indicates a LINKAGE/SCHEMA problem in 02f, not evidence that the 02b
# classification itself failed.
#
# THIS SCRIPT:
#
#   1. opens the VPJD DuckDB read-only;
#   2. inventories all 02a / 02b / lineage-related tables;
#   3. records their fields and dimensions;
#   4. identifies candidate family, WCVP-ID and classification fields;
#   5. safely inspects distinct classification values regardless of SQL type;
#   6. tests concept-level and family-level linkage to the canonical population;
#   7. writes diagnostic CSV files;
#   8. recommends the strongest linkage route;
#   9. modifies NOTHING.
#
# CHANGES IN v0.1.1
# -----------------
# Candidate classification values are explicitly CAST AS VARCHAR in SQL before
# being combined. This prevents dplyr::bind_rows() failures when source fields
# have heterogeneous DuckDB types such as VARCHAR, BOOLEAN, INTEGER, etc.
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

MODULE <- "02f1_diagnose_02b_lineage_linkage"
VERSION <- "0.1.1"
RUN_DATE <- Sys.Date()

EXPECTED_CANONICAL_POPULATION <- 11439L
EXPECTED_FAMILIES <- 260L


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

OUTPUT_DIR <- file.path(
  PROJECT_ROOT,
  "outputs",
  "tables",
  "taxonomic_revision",
  "02f_infraspecific_other_ranks",
  "02f1_lineage_linkage_diagnostic"
)

dir.create(
  OUTPUT_DIR,
  recursive = TRUE,
  showWarnings = FALSE
)


# ==============================================================================
# 04. KNOWN CANONICAL TABLES
# ==============================================================================

TABLE_CANONICAL <-
  "vpjd_star_provisional_wholesale_allocation"

TABLE_WCVP <-
  "occurrence_wcvp_accepted_taxa"

TABLE_02A <-
  "vpjd_taxrev_02a_non_vascular_audit"


# ==============================================================================
# 05. START REPORT
# ==============================================================================

cat("\n")
cat("============================================================\n")
cat("VPJD TAXONOMIC REVISION — 02f.1\n")
cat("DIAGNOSE 02b LINEAGE CLASSIFICATION LINKAGE\n")
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
  as.character(RUN_DATE),
  "\n\n",
  sep = ""
)

cat("Database mode: READ ONLY\n")
cat("Canonical taxonomy modified: FALSE\n")
cat("Star allocations modified: FALSE\n\n")


# ==============================================================================
# 06. CHECK DATABASE
# ==============================================================================

if (!file.exists(DB_PATH)) {
  
  stop(
    "VPJD DuckDB database not found:\n",
    DB_PATH
  )
}


# ==============================================================================
# 07. CLOSE ANY EXISTING CONNECTION
# ==============================================================================

if (exists("con", envir = .GlobalEnv)) {
  
  old_con <- get(
    "con",
    envir = .GlobalEnv
  )
  
  old_valid <- tryCatch(
    DBI::dbIsValid(old_con),
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
# 08. OPEN DATABASE READ-ONLY
# ==============================================================================

cat("Opening VPJD DuckDB read-only...\n")

con <- DBI::dbConnect(
  duckdb::duckdb(),
  dbdir = DB_PATH,
  read_only = TRUE
)

if (!DBI::dbIsValid(con)) {
  
  stop(
    "Could not establish valid DuckDB connection."
  )
}

cat("Connection established.\n\n")


# ==============================================================================
# 09. COMPLETE TABLE INVENTORY
# ==============================================================================

db_tables <- DBI::dbListTables(con)

cat(
  "Database tables: ",
  length(db_tables),
  "\n",
  sep = ""
)


# ==============================================================================
# 10. IDENTIFY TAXONOMIC-REVISION / LINEAGE TABLES
# ==============================================================================

candidate_tables <- db_tables[
  str_detect(
    str_to_lower(db_tables),
    paste0(
      "02a|02b|lineage|vascular|non_vascular|",
      "nonvascular|major_group|family_crosswalk|",
      "family_class|taxrev"
    )
  )
]

candidate_tables <- sort(
  unique(candidate_tables)
)

cat("\n")
cat("------------------------------------------------------------\n")
cat("CANDIDATE 02a / 02b / LINEAGE TABLES\n")
cat("------------------------------------------------------------\n\n")

if (length(candidate_tables) == 0L) {
  
  cat("No candidate tables identified by table name.\n")
  
} else {
  
  cat(
    paste0(
      "  ",
      candidate_tables
    ),
    sep = "\n"
  )
  
  cat("\n")
}


# ==============================================================================
# 11. TABLE DIMENSIONS
# ==============================================================================

get_table_n <- function(tbl) {
  
  tryCatch(
    
    DBI::dbGetQuery(
      con,
      paste0(
        'SELECT COUNT(*) AS N FROM "',
        tbl,
        '"'
      )
    )$N[[1]],
    
    error = function(e) {
      NA_real_
    }
  )
}


table_inventory <- tibble(
  TABLE_NAME = candidate_tables
) |>
  mutate(
    
    N_ROWS =
      map_dbl(
        TABLE_NAME,
        get_table_n
      ),
    
    N_FIELDS =
      map_int(
        TABLE_NAME,
        ~ length(
          DBI::dbListFields(
            con,
            .x
          )
        )
      )
  )


# ==============================================================================
# 12. FIELD INVENTORY
# ==============================================================================

field_inventory <- map_dfr(
  
  candidate_tables,
  
  function(tbl) {
    
    fields <- DBI::dbListFields(
      con,
      tbl
    )
    
    tibble(
      TABLE_NAME = tbl,
      FIELD_NAME = fields
    )
  }
)


# ==============================================================================
# 13. CLASSIFY POTENTIALLY IMPORTANT FIELDS
# ==============================================================================

field_inventory <- field_inventory |>
  mutate(
    
    FIELD_LOWER =
      str_to_lower(
        FIELD_NAME
      ),
    
    POSSIBLE_WCVP_ID =
      str_detect(
        FIELD_LOWER,
        "wcvp.*id|final_wcvp_id|plant_name_id"
      ),
    
    POSSIBLE_FAMILY =
      str_detect(
        FIELD_LOWER,
        "family"
      ),
    
    POSSIBLE_MAJOR_GROUP =
      str_detect(
        FIELD_LOWER,
        paste0(
          "major.*group|lineage|vascular|",
          "non.?vascular|plant.*group|",
          "taxonomic.*group|higher.*group"
        )
      ),
    
    POSSIBLE_CLASSIFICATION =
      str_detect(
        FIELD_LOWER,
        paste0(
          "class|status|group|lineage|",
          "vascular|decision|scope|",
          "include|exclude"
        )
      )
  )


# ==============================================================================
# 14. REPORT TABLE INVENTORY
# ==============================================================================

cat("\n")
cat("------------------------------------------------------------\n")
cat("CANDIDATE TABLE INVENTORY\n")
cat("------------------------------------------------------------\n\n")

print(
  table_inventory,
  n = Inf
)


# ==============================================================================
# 15. REPORT IMPORTANT FIELDS
# ==============================================================================

important_fields <- field_inventory |>
  filter(
    POSSIBLE_WCVP_ID |
      POSSIBLE_FAMILY |
      POSSIBLE_MAJOR_GROUP |
      POSSIBLE_CLASSIFICATION
  )

cat("\n")
cat("------------------------------------------------------------\n")
cat("POTENTIALLY IMPORTANT FIELDS\n")
cat("------------------------------------------------------------\n\n")

print(
  important_fields,
  n = Inf,
  width = Inf
)


# ==============================================================================
# 16. LOAD CANONICAL POPULATION
# ==============================================================================

if (!TABLE_CANONICAL %in% db_tables) {
  
  DBI::dbDisconnect(
    con,
    shutdown = TRUE
  )
  
  stop(
    "Canonical table missing: ",
    TABLE_CANONICAL
  )
}


canonical <- DBI::dbGetQuery(
  con,
  paste0(
    '
    SELECT
      CAST(FINAL_WCVP_ID AS VARCHAR) AS FINAL_WCVP_ID,
      FINAL_WCVP_RECOGNISED_NAME,
      FINAL_WCVP_RANK,
      PROVISIONAL_STAR
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
  
  DBI::dbDisconnect(
    con,
    shutdown = TRUE
  )
  
  stop(
    "Expected ",
    EXPECTED_CANONICAL_POPULATION,
    " canonical concepts; found ",
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
  
  DBI::dbDisconnect(
    con,
    shutdown = TRUE
  )
  
  stop(
    "Canonical population is not unique by FINAL_WCVP_ID."
  )
}


# ==============================================================================
# 17. LOAD WCVP FAMILY HIERARCHY
# ==============================================================================

if (!TABLE_WCVP %in% db_tables) {
  
  DBI::dbDisconnect(
    con,
    shutdown = TRUE
  )
  
  stop(
    "WCVP lookup table missing: ",
    TABLE_WCVP
  )
}


wcvp_fields <- DBI::dbListFields(
  con,
  TABLE_WCVP
)


required_wcvp <- c(
  "wcvp_plant_name_id",
  "wcvp_family"
)


missing_wcvp <- setdiff(
  required_wcvp,
  wcvp_fields
)


if (length(missing_wcvp) > 0L) {
  
  DBI::dbDisconnect(
    con,
    shutdown = TRUE
  )
  
  stop(
    "Required WCVP fields missing: ",
    paste(
      missing_wcvp,
      collapse = ", "
    )
  )
}


wcvp_family <- DBI::dbGetQuery(
  con,
  paste0(
    '
    SELECT
      CAST(wcvp_plant_name_id AS VARCHAR) AS FINAL_WCVP_ID,
      CAST(wcvp_family AS VARCHAR) AS wcvp_family
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


canonical_family <- canonical |>
  left_join(
    wcvp_family,
    by = "FINAL_WCVP_ID"
  )


# ==============================================================================
# 18. VALIDATE FAMILY POPULATION
# ==============================================================================

canonical_family_inventory <- canonical_family |>
  filter(
    !is.na(wcvp_family),
    wcvp_family != ""
  ) |>
  count(
    wcvp_family,
    name = "N_CONCEPTS"
  ) |>
  arrange(
    wcvp_family
  )


n_canonical_families <-
  nrow(canonical_family_inventory)


cat("\n")

cat(
  "Canonical represented families: ",
  n_canonical_families,
  "\n",
  sep = ""
)


# ==============================================================================
# 19. IDENTIFY CANDIDATE CLASSIFICATION FIELDS
# ==============================================================================

classification_fields <- important_fields |>
  filter(
    POSSIBLE_MAJOR_GROUP |
      POSSIBLE_CLASSIFICATION
  ) |>
  distinct(
    TABLE_NAME,
    FIELD_NAME
  )


# ==============================================================================
# 20. SAFELY EXTRACT DISTINCT CLASSIFICATION VALUES
#
# IMPORTANT:
#
# Every VALUE is explicitly CAST AS VARCHAR in DuckDB.
#
# Candidate fields may otherwise be returned to R as:
#
#   character
#   logical
#   integer
#   numeric
#   date/time
#
# pmap_dfr()/bind_rows() cannot safely combine heterogeneous VALUE types.
#
# ==============================================================================

get_distinct_values <- function(
    tbl,
    fld
) {
  
  sql <- paste0(
    'SELECT ',
    'CAST("',
    fld,
    '" AS VARCHAR) AS VALUE, ',
    'COUNT(*) AS N ',
    'FROM "',
    tbl,
    '" ',
    'GROUP BY "',
    fld,
    '" ',
    'ORDER BY N DESC'
  )
  
  out <- tryCatch(
    
    DBI::dbGetQuery(
      con,
      sql
    ) |>
      as_tibble() |>
      mutate(
        VALUE =
          as.character(VALUE),
        N =
          as.numeric(N)
      ),
    
    error = function(e) {
      
      tibble(
        VALUE = NA_character_,
        N = NA_real_,
        QUERY_ERROR =
          as.character(
            conditionMessage(e)
          )
      )
    }
  )
  
  
  # Ensure the same schema regardless of success/failure.
  
  if (!"QUERY_ERROR" %in% names(out)) {
    
    out <- out |>
      mutate(
        QUERY_ERROR =
          NA_character_
      )
  }
  
  
  out |>
    transmute(
      
      TABLE_NAME =
        as.character(tbl),
      
      FIELD_NAME =
        as.character(fld),
      
      VALUE =
        as.character(VALUE),
      
      N =
        as.numeric(N),
      
      QUERY_ERROR =
        as.character(QUERY_ERROR)
    )
}


# ==============================================================================
# 21. RUN CLASSIFICATION VALUE EXTRACTION
# ==============================================================================

if (nrow(classification_fields) > 0L) {
  
  classification_values <- pmap_dfr(
    
    classification_fields,
    
    function(
    TABLE_NAME,
    FIELD_NAME
    ) {
      
      get_distinct_values(
        TABLE_NAME,
        FIELD_NAME
      )
    }
  )
  
} else {
  
  classification_values <- tibble(
    
    TABLE_NAME =
      character(),
    
    FIELD_NAME =
      character(),
    
    VALUE =
      character(),
    
    N =
      numeric(),
    
    QUERY_ERROR =
      character()
  )
}


# ==============================================================================
# 22. REPORT DISTINCT CLASSIFICATION VALUES
# ==============================================================================

cat("\n")
cat("------------------------------------------------------------\n")
cat("DISTINCT VALUES IN CANDIDATE CLASSIFICATION FIELDS\n")
cat("------------------------------------------------------------\n\n")


if (nrow(classification_values) == 0L) {
  
  cat(
    "No candidate classification values recovered.\n"
  )
  
} else {
  
  print(
    classification_values,
    n = Inf,
    width = Inf
  )
}


# ==============================================================================
# 23. IDENTIFY WCVP-ID FIELDS
# ==============================================================================

id_fields <- field_inventory |>
  filter(
    POSSIBLE_WCVP_ID
  ) |>
  distinct(
    TABLE_NAME,
    FIELD_NAME
  )


# ==============================================================================
# 24. TEST WCVP-ID LINKAGE
# ==============================================================================

test_id_linkage <- function(
    tbl,
    fld
) {
  
  ids <- tryCatch(
    
    DBI::dbGetQuery(
      con,
      paste0(
        'SELECT DISTINCT ',
        'CAST("',
        fld,
        '" AS VARCHAR) ',
        'AS FINAL_WCVP_ID ',
        'FROM "',
        tbl,
        '" ',
        'WHERE "',
        fld,
        '" IS NOT NULL'
      )
    ) |>
      as_tibble() |>
      mutate(
        FINAL_WCVP_ID =
          as.character(
            FINAL_WCVP_ID
          )
      ),
    
    error = function(e) {
      NULL
    }
  )
  
  
  if (is.null(ids)) {
    
    return(
      tibble(
        
        TABLE_NAME =
          as.character(tbl),
        
        ID_FIELD =
          as.character(fld),
        
        N_DISTINCT_IDS =
          NA_integer_,
        
        N_CANONICAL_MATCHES =
          NA_integer_,
        
        PERCENT_CANONICAL_MATCHED =
          NA_real_
      )
    )
  }
  
  
  matches <- canonical |>
    semi_join(
      ids,
      by = "FINAL_WCVP_ID"
    )
  
  
  tibble(
    
    TABLE_NAME =
      as.character(tbl),
    
    ID_FIELD =
      as.character(fld),
    
    N_DISTINCT_IDS =
      as.integer(
        n_distinct(
          ids$FINAL_WCVP_ID
        )
      ),
    
    N_CANONICAL_MATCHES =
      as.integer(
        nrow(matches)
      ),
    
    PERCENT_CANONICAL_MATCHED =
      round(
        100 *
          nrow(matches) /
          EXPECTED_CANONICAL_POPULATION,
        3
      )
  )
}


if (nrow(id_fields) > 0L) {
  
  id_linkage_results <- pmap_dfr(
    
    id_fields,
    
    function(
    TABLE_NAME,
    FIELD_NAME
    ) {
      
      test_id_linkage(
        TABLE_NAME,
        FIELD_NAME
      )
    }
  )
  
} else {
  
  id_linkage_results <- tibble(
    
    TABLE_NAME =
      character(),
    
    ID_FIELD =
      character(),
    
    N_DISTINCT_IDS =
      integer(),
    
    N_CANONICAL_MATCHES =
      integer(),
    
    PERCENT_CANONICAL_MATCHED =
      numeric()
  )
}


# ==============================================================================
# 25. IDENTIFY FAMILY FIELDS
# ==============================================================================

family_fields <- field_inventory |>
  filter(
    POSSIBLE_FAMILY
  ) |>
  distinct(
    TABLE_NAME,
    FIELD_NAME
  )


# ==============================================================================
# 26. TEST FAMILY LINKAGE
# ==============================================================================

test_family_linkage <- function(
    tbl,
    fld
) {
  
  families <- tryCatch(
    
    DBI::dbGetQuery(
      con,
      paste0(
        'SELECT DISTINCT ',
        'CAST("',
        fld,
        '" AS VARCHAR) ',
        'AS FAMILY ',
        'FROM "',
        tbl,
        '" ',
        'WHERE "',
        fld,
        '" IS NOT NULL'
      )
    ) |>
      as_tibble() |>
      mutate(
        
        FAMILY =
          as.character(
            FAMILY
          ),
        
        FAMILY =
          str_trim(
            FAMILY
          )
      ) |>
      filter(
        FAMILY != ""
      ),
    
    error = function(e) {
      NULL
    }
  )
  
  
  if (is.null(families)) {
    
    return(
      tibble(
        
        TABLE_NAME =
          as.character(tbl),
        
        FAMILY_FIELD =
          as.character(fld),
        
        N_DISTINCT_FAMILIES =
          NA_integer_,
        
        N_VPJD_FAMILY_MATCHES =
          NA_integer_,
        
        PERCENT_260_MATCHED =
          NA_real_,
        
        N_CONCEPTS_COVERED =
          NA_integer_
      )
    )
  }
  
  
  family_matches <-
    canonical_family_inventory |>
    transmute(
      
      FAMILY =
        as.character(
          wcvp_family
        ),
      
      N_CONCEPTS =
        as.integer(
          N_CONCEPTS
        )
    ) |>
    semi_join(
      families,
      by = "FAMILY"
    )
  
  
  tibble(
    
    TABLE_NAME =
      as.character(tbl),
    
    FAMILY_FIELD =
      as.character(fld),
    
    N_DISTINCT_FAMILIES =
      as.integer(
        n_distinct(
          families$FAMILY
        )
      ),
    
    N_VPJD_FAMILY_MATCHES =
      as.integer(
        nrow(family_matches)
      ),
    
    PERCENT_260_MATCHED =
      round(
        100 *
          nrow(family_matches) /
          EXPECTED_FAMILIES,
        3
      ),
    
    N_CONCEPTS_COVERED =
      as.integer(
        sum(
          family_matches$N_CONCEPTS
        )
      )
  )
}


if (nrow(family_fields) > 0L) {
  
  family_linkage_results <- pmap_dfr(
    
    family_fields,
    
    function(
    TABLE_NAME,
    FIELD_NAME
    ) {
      
      test_family_linkage(
        TABLE_NAME,
        FIELD_NAME
      )
    }
  )
  
} else {
  
  family_linkage_results <- tibble(
    
    TABLE_NAME =
      character(),
    
    FAMILY_FIELD =
      character(),
    
    N_DISTINCT_FAMILIES =
      integer(),
    
    N_VPJD_FAMILY_MATCHES =
      integer(),
    
    PERCENT_260_MATCHED =
      numeric(),
    
    N_CONCEPTS_COVERED =
      integer()
  )
}


# ==============================================================================
# 27. REPORT LINKAGE TESTS
# ==============================================================================

cat("\n")
cat("------------------------------------------------------------\n")
cat("WCVP-ID LINKAGE TESTS\n")
cat("------------------------------------------------------------\n\n")


if (nrow(id_linkage_results) == 0L) {
  
  cat(
    "No candidate WCVP-ID fields found.\n"
  )
  
} else {
  
  print(
    id_linkage_results |>
      arrange(
        desc(
          N_CANONICAL_MATCHES
        )
      ),
    n = Inf,
    width = Inf
  )
}


cat("\n")
cat("------------------------------------------------------------\n")
cat("FAMILY LINKAGE TESTS\n")
cat("------------------------------------------------------------\n\n")


if (nrow(family_linkage_results) == 0L) {
  
  cat(
    "No candidate family fields found.\n"
  )
  
} else {
  
  print(
    family_linkage_results |>
      arrange(
        desc(
          N_CONCEPTS_COVERED
        ),
        desc(
          N_VPJD_FAMILY_MATCHES
        )
      ),
    n = Inf,
    width = Inf
  )
}


# ==============================================================================
# 28. IDENTIFY BEST CONCEPT-LEVEL LINKAGE ROUTE
# ==============================================================================

best_id_route <- id_linkage_results |>
  filter(
    !is.na(
      N_CANONICAL_MATCHES
    )
  ) |>
  arrange(
    desc(
      N_CANONICAL_MATCHES
    ),
    TABLE_NAME,
    ID_FIELD
  ) |>
  slice_head(
    n = 1
  )


# ==============================================================================
# 29. IDENTIFY BEST FAMILY-LEVEL LINKAGE ROUTE
# ==============================================================================

best_family_route <- family_linkage_results |>
  filter(
    !is.na(
      N_CONCEPTS_COVERED
    )
  ) |>
  arrange(
    desc(
      N_CONCEPTS_COVERED
    ),
    desc(
      N_VPJD_FAMILY_MATCHES
    ),
    TABLE_NAME,
    FAMILY_FIELD
  ) |>
  slice_head(
    n = 1
  )


# ==============================================================================
# 30. CALCULATE BEST COVERAGE
# ==============================================================================

best_id_coverage <- if (
  nrow(best_id_route) == 1L
) {
  
  best_id_route$N_CANONICAL_MATCHES[[1]]
  
} else {
  
  0L
}


best_family_coverage <- if (
  nrow(best_family_route) == 1L
) {
  
  best_family_route$N_CONCEPTS_COVERED[[1]]
  
} else {
  
  0L
}


# ==============================================================================
# 31. RECOMMEND LINKAGE TYPE
# ==============================================================================

recommended_route <- case_when(
  
  best_id_coverage ==
    EXPECTED_CANONICAL_POPULATION ~
    "CONCEPT_LEVEL_WCVP_ID",
  
  best_family_coverage ==
    EXPECTED_CANONICAL_POPULATION ~
    "FAMILY_LEVEL",
  
  best_id_coverage >
    best_family_coverage ~
    "PARTIAL_CONCEPT_LEVEL_REQUIRES_REVIEW",
  
  best_family_coverage >
    0L ~
    "PARTIAL_FAMILY_LEVEL_REQUIRES_REVIEW",
  
  TRUE ~
    "NO_VALID_LINKAGE_IDENTIFIED"
)


# ==============================================================================
# 32. DIAGNOSTIC SUMMARY
# ==============================================================================

diagnostic_summary <- tibble(
  
  METRIC = c(
    
    "Canonical VPJD concepts",
    
    "Canonical VPJD families",
    
    "Candidate 02a/02b/lineage tables",
    
    "Candidate WCVP-ID fields",
    
    "Candidate family fields",
    
    "Candidate classification fields",
    
    "Best concept-level coverage",
    
    "Best family-level concept coverage"
  ),
  
  VALUE = c(
    
    as.character(
      nrow(canonical)
    ),
    
    as.character(
      n_canonical_families
    ),
    
    as.character(
      length(candidate_tables)
    ),
    
    as.character(
      nrow(id_fields)
    ),
    
    as.character(
      nrow(family_fields)
    ),
    
    as.character(
      nrow(classification_fields)
    ),
    
    as.character(
      best_id_coverage
    ),
    
    as.character(
      best_family_coverage
    )
  )
)


# ==============================================================================
# 33. RECOMMENDATION TABLE
# ==============================================================================

recommendation <- tibble(
  
  RECOMMENDED_LINKAGE_ROUTE =
    as.character(
      recommended_route
    ),
  
  BEST_ID_TABLE =
    if (
      nrow(best_id_route) == 1L
    ) {
      as.character(
        best_id_route$TABLE_NAME[[1]]
      )
    } else {
      NA_character_
    },
  
  BEST_ID_FIELD =
    if (
      nrow(best_id_route) == 1L
    ) {
      as.character(
        best_id_route$ID_FIELD[[1]]
      )
    } else {
      NA_character_
    },
  
  BEST_ID_CONCEPT_COVERAGE =
    as.integer(
      best_id_coverage
    ),
  
  BEST_FAMILY_TABLE =
    if (
      nrow(best_family_route) == 1L
    ) {
      as.character(
        best_family_route$TABLE_NAME[[1]]
      )
    } else {
      NA_character_
    },
  
  BEST_FAMILY_FIELD =
    if (
      nrow(best_family_route) == 1L
    ) {
      as.character(
        best_family_route$FAMILY_FIELD[[1]]
      )
    } else {
      NA_character_
    },
  
  BEST_FAMILY_CONCEPT_COVERAGE =
    as.integer(
      best_family_coverage
    )
)


# ==============================================================================
# 34. QUERY ERROR SUMMARY
# ==============================================================================

query_errors <- classification_values |>
  filter(
    !is.na(
      QUERY_ERROR
    ),
    QUERY_ERROR != ""
  ) |>
  distinct(
    TABLE_NAME,
    FIELD_NAME,
    QUERY_ERROR
  )


cat("\n")
cat("------------------------------------------------------------\n")
cat("CLASSIFICATION-FIELD QUERY ERRORS\n")
cat("------------------------------------------------------------\n\n")


if (nrow(query_errors) == 0L) {
  
  cat(
    "No classification-field query errors detected.\n"
  )
  
} else {
  
  print(
    query_errors,
    n = Inf,
    width = Inf
  )
}


# ==============================================================================
# 35. VALIDATION
# ==============================================================================

validation <- tibble(
  
  CHECK = c(
    
    "Canonical table present",
    
    "WCVP lookup table present",
    
    "Canonical population = 11,439",
    
    "Canonical population unique by WCVP ID",
    
    "Canonical family population = 260",
    
    "Candidate 02a/02b/lineage tables identified",
    
    "At least one candidate classification field identified",
    
    "At least one family or WCVP-ID linkage field identified",
    
    "Classification-field extraction completed without query errors",
    
    "A linkage route was identified",
    
    "Database opened read-only",
    
    "Canonical taxonomy unchanged",
    
    "Star allocations unchanged"
  ),
  
  PASS = c(
    
    TABLE_CANONICAL %in%
      db_tables,
    
    TABLE_WCVP %in%
      db_tables,
    
    nrow(canonical) ==
      EXPECTED_CANONICAL_POPULATION,
    
    n_distinct(
      canonical$FINAL_WCVP_ID
    ) ==
      EXPECTED_CANONICAL_POPULATION,
    
    n_canonical_families ==
      EXPECTED_FAMILIES,
    
    length(candidate_tables) >
      0L,
    
    nrow(classification_fields) >
      0L,
    
    (
      nrow(id_fields) >
        0L
    ) |
      (
        nrow(family_fields) >
          0L
      ),
    
    nrow(query_errors) ==
      0L,
    
    recommended_route !=
      "NO_VALID_LINKAGE_IDENTIFIED",
    
    TRUE,
    
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
# 36. WRITE DIAGNOSTIC CSV OUTPUTS
# ==============================================================================

write_csv(
  table_inventory,
  file.path(
    OUTPUT_DIR,
    "VPJD_02f1_candidate_table_inventory.csv"
  )
)


write_csv(
  field_inventory,
  file.path(
    OUTPUT_DIR,
    "VPJD_02f1_field_inventory.csv"
  )
)


write_csv(
  important_fields,
  file.path(
    OUTPUT_DIR,
    "VPJD_02f1_important_fields.csv"
  )
)


write_csv(
  classification_values,
  file.path(
    OUTPUT_DIR,
    "VPJD_02f1_classification_values.csv"
  )
)


write_csv(
  query_errors,
  file.path(
    OUTPUT_DIR,
    "VPJD_02f1_classification_query_errors.csv"
  )
)


write_csv(
  id_linkage_results,
  file.path(
    OUTPUT_DIR,
    "VPJD_02f1_wcvp_id_linkage_tests.csv"
  )
)


write_csv(
  family_linkage_results,
  file.path(
    OUTPUT_DIR,
    "VPJD_02f1_family_linkage_tests.csv"
  )
)


write_csv(
  canonical_family_inventory,
  file.path(
    OUTPUT_DIR,
    "VPJD_02f1_canonical_family_inventory.csv"
  )
)


write_csv(
  diagnostic_summary,
  file.path(
    OUTPUT_DIR,
    "VPJD_02f1_diagnostic_summary.csv"
  )
)


write_csv(
  recommendation,
  file.path(
    OUTPUT_DIR,
    "VPJD_02f1_recommended_linkage_route.csv"
  )
)


write_csv(
  validation,
  file.path(
    OUTPUT_DIR,
    "VPJD_02f1_validation.csv"
  )
)


# ==============================================================================
# 37. OUTPUT INVENTORY
# ==============================================================================

output_inventory <- tibble(
  
  FILE = c(
    
    "VPJD_02f1_candidate_table_inventory.csv",
    
    "VPJD_02f1_field_inventory.csv",
    
    "VPJD_02f1_important_fields.csv",
    
    "VPJD_02f1_classification_values.csv",
    
    "VPJD_02f1_classification_query_errors.csv",
    
    "VPJD_02f1_wcvp_id_linkage_tests.csv",
    
    "VPJD_02f1_family_linkage_tests.csv",
    
    "VPJD_02f1_canonical_family_inventory.csv",
    
    "VPJD_02f1_diagnostic_summary.csv",
    
    "VPJD_02f1_recommended_linkage_route.csv",
    
    "VPJD_02f1_validation.csv"
  ),
  
  PURPOSE = c(
    
    "Inventory of candidate 02a/02b/lineage tables",
    
    "Complete field inventory for candidate tables",
    
    "Fields potentially relevant to lineage linkage",
    
    "Safely normalised distinct values in potential classification fields",
    
    "Any errors encountered while inspecting classification fields",
    
    "Tests of WCVP-ID linkage to the canonical population",
    
    "Tests of family-level linkage to the canonical population",
    
    "Canonical 260-family VPJD inventory",
    
    "Summary of diagnostic results",
    
    "Recommended linkage route for 02f repair",
    
    "Validation record for the diagnostic"
  )
)


write_csv(
  output_inventory,
  file.path(
    OUTPUT_DIR,
    "VPJD_02f1_output_inventory.csv"
  )
)


# ==============================================================================
# 38. FINAL REPORT
# ==============================================================================

cat("\n")
cat("============================================================\n")
cat("02f.1 DIAGNOSTIC RESULTS\n")
cat("============================================================\n\n")


print(
  diagnostic_summary,
  n = Inf
)


cat("\n")
cat("------------------------------------------------------------\n")
cat("RECOMMENDED LINKAGE ROUTE\n")
cat("------------------------------------------------------------\n\n")


print(
  recommendation,
  n = Inf,
  width = Inf
)


cat("\n")
cat("------------------------------------------------------------\n")
cat("VALIDATION\n")
cat("------------------------------------------------------------\n\n")


print(
  validation,
  n = Inf
)


# ==============================================================================
# 39. CLEAN DISCONNECT
# ==============================================================================

DBI::dbDisconnect(
  con,
  shutdown = TRUE
)

rm(con)

gc()


# ==============================================================================
# 40. COMPLETION REPORT
# ==============================================================================

cat("\n")
cat("============================================================\n")

cat(
  "VPJD TAXONOMIC REVISION 02f.1 v",
  VERSION,
  " COMPLETE\n",
  sep = ""
)

cat("============================================================\n\n")


cat(
  "Recommended linkage route: ",
  recommended_route,
  "\n",
  sep = ""
)


cat(
  "Best concept-level coverage: ",
  format(
    best_id_coverage,
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
  "Best family-level concept coverage: ",
  format(
    best_family_coverage,
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
  "Classification-field query errors: ",
  nrow(query_errors),
  "\n\n",
  sep = ""
)


cat(
  "Output directory:\n",
  OUTPUT_DIR,
  "\n\n",
  sep = ""
)


cat("Database modified: FALSE\n")
cat("Canonical taxonomy modified: FALSE\n")
cat("Star allocations modified: FALSE\n")
cat("Diagnostic CSV outputs written: TRUE\n")
cat("DuckDB connection closed cleanly.\n")

cat("\n")
cat("============================================================\n")


# ==============================================================================
# END
# ==============================================================================