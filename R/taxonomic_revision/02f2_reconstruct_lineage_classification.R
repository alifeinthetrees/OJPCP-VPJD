# ==============================================================================
# VPJD TAXONOMIC REVISION
# 02f.2 — RECONSTRUCT AND VALIDATE LINEAGE CLASSIFICATION
#
# Version: 0.1.0
#
# PURPOSE
# -------
# Repair the lineage-classification linkage problem identified in 02f.
#
# 02f.1 demonstrated:
#
#   * canonical population:                  11,439
#   * concept-level WCVP-ID coverage:        11,439 / 11,439
#   * family-level concept coverage:         11,439 / 11,439
#   * classification-field query errors:     0
#
# Therefore the classification evidence exists. The 02f failure was a linkage
# problem rather than absence of higher-taxonomic information.
#
# THIS MODULE:
#
#   1. discovers the validated 02b concept-level classification source;
#   2. discovers the validated 02b family-level classification source;
#   3. reconstructs classification for all 11,439 canonical concepts;
#   4. compares concept-level and family-level assignments;
#   5. requires complete coverage and complete agreement;
#   6. reconstructs the 1,710 non-species/non-genus 02f audit population;
#   7. identifies vascular/non-vascular composition;
#   8. produces review candidates for 02f.3;
#   9. modifies no canonical taxonomy;
#  10. modifies no Star allocations.
#
# IMPORTANT
# ---------
# No taxa are removed by this module.
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

MODULE <- "02f2_reconstruct_lineage_classification"
VERSION <- "0.1.0"
RUN_DATE <- Sys.Date()

EXPECTED_CANONICAL_POPULATION <- 11439L
EXPECTED_FAMILIES <- 260L

EXPECTED_SPECIES <- 8906L
EXPECTED_GENUS <- 823L

EXPECTED_SUBSPECIES <- 499L
EXPECTED_VARIETIES <- 1147L
EXPECTED_FORMS <- 62L
EXPECTED_OTHER_RANKS <- 2L

EXPECTED_INFRASPECIFIC <- 1708L
EXPECTED_NON_SPECIES_GENUS <- 1710L


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

DIAGNOSTIC_DIR <- file.path(
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
  "02f2_lineage_reconstruction"
)

dir.create(
  OUTPUT_DIR,
  recursive = TRUE,
  showWarnings = FALSE
)


# ==============================================================================
# 04. CANONICAL TABLES
# ==============================================================================

TABLE_CANONICAL <-
  "vpjd_star_provisional_wholesale_allocation"

TABLE_WCVP <-
  "occurrence_wcvp_accepted_taxa"


# ==============================================================================
# 05. START REPORT
# ==============================================================================

cat("\n")
cat("============================================================\n")
cat("VPJD TAXONOMIC REVISION — 02f.2\n")
cat("RECONSTRUCT AND VALIDATE LINEAGE CLASSIFICATION\n")
cat("============================================================\n\n")

cat("Module: ", MODULE, "\n", sep = "")
cat("Version: ", VERSION, "\n", sep = "")
cat("Run date: ", RUN_DATE, "\n\n", sep = "")

cat("Database mode: READ ONLY\n")
cat("Canonical taxonomy modified: FALSE\n")
cat("Star allocations modified: FALSE\n")
cat("Automatic exclusions: FALSE\n\n")


# ==============================================================================
# 06. CHECK REQUIRED FILES
# ==============================================================================

if (!file.exists(DB_PATH)) {
  
  stop(
    "VPJD DuckDB database not found:\n",
    DB_PATH
  )
}


diagnostic_files <- c(
  
  recommendation =
    file.path(
      DIAGNOSTIC_DIR,
      "VPJD_02f1_recommended_linkage_route.csv"
    ),
  
  field_inventory =
    file.path(
      DIAGNOSTIC_DIR,
      "VPJD_02f1_field_inventory.csv"
    ),
  
  id_tests =
    file.path(
      DIAGNOSTIC_DIR,
      "VPJD_02f1_wcvp_id_linkage_tests.csv"
    ),
  
  family_tests =
    file.path(
      DIAGNOSTIC_DIR,
      "VPJD_02f1_family_linkage_tests.csv"
    )
)


missing_diagnostic_files <-
  diagnostic_files[
    !file.exists(
      diagnostic_files
    )
  ]


if (length(missing_diagnostic_files) > 0L) {
  
  stop(
    "Required 02f.1 diagnostic file(s) missing:\n",
    paste(
      missing_diagnostic_files,
      collapse = "\n"
    )
  )
}


# ==============================================================================
# 07. READ 02f.1 DIAGNOSTIC OUTPUTS
# ==============================================================================

recommendation <-
  read_csv(
    diagnostic_files[["recommendation"]],
    show_col_types = FALSE
  )

field_inventory <-
  read_csv(
    diagnostic_files[["field_inventory"]],
    show_col_types = FALSE
  )

id_tests <-
  read_csv(
    diagnostic_files[["id_tests"]],
    show_col_types = FALSE
  )

family_tests <-
  read_csv(
    diagnostic_files[["family_tests"]],
    show_col_types = FALSE
  )


if (nrow(recommendation) != 1L) {
  
  stop(
    "02f.1 recommendation table should contain exactly one row."
  )
}


if (
  recommendation$RECOMMENDED_LINKAGE_ROUTE[[1]] !=
  "CONCEPT_LEVEL_WCVP_ID"
) {
  
  stop(
    "02f.1 did not recommend CONCEPT_LEVEL_WCVP_ID. ",
    "Review 02f.1 before proceeding."
  )
}


# ==============================================================================
# 08. CLOSE ANY EXISTING DATABASE CONNECTION
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
# 09. OPEN DATABASE READ-ONLY
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

db_tables <- DBI::dbListTables(con)


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
    "Required VPJD table(s) missing: ",
    paste(
      missing_tables,
      collapse = ", "
    )
  )
}


# ==============================================================================
# 11. LOAD CANONICAL POPULATION
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


canonical_ids_before <-
  sort(
    canonical$FINAL_WCVP_ID
  )

canonical_stars_before <-
  canonical |>
  select(
    FINAL_WCVP_ID,
    PROVISIONAL_STAR
  ) |>
  arrange(
    FINAL_WCVP_ID
  )


# ==============================================================================
# 12. LOAD WCVP TAXONOMIC HIERARCHY
# ==============================================================================

wcvp_fields <- DBI::dbListFields(
  con,
  TABLE_WCVP
)


required_wcvp_fields <- c(
  "wcvp_plant_name_id",
  "wcvp_taxon_name",
  "wcvp_taxon_rank",
  "wcvp_taxon_status",
  "wcvp_family",
  "wcvp_genus",
  "wcvp_species"
)


missing_wcvp_fields <- setdiff(
  required_wcvp_fields,
  wcvp_fields
)


if (length(missing_wcvp_fields) > 0L) {
  
  stop(
    "Required WCVP fields missing: ",
    paste(
      missing_wcvp_fields,
      collapse = ", "
    )
  )
}


wcvp <- DBI::dbGetQuery(
  con,
  paste0(
    '
    SELECT
      CAST(wcvp_plant_name_id AS VARCHAR) AS FINAL_WCVP_ID,
      CAST(wcvp_taxon_name AS VARCHAR) AS WCVP_TAXON_NAME,
      CAST(wcvp_taxon_rank AS VARCHAR) AS WCVP_TAXON_RANK,
      CAST(wcvp_taxon_status AS VARCHAR) AS WCVP_TAXON_STATUS,
      CAST(wcvp_family AS VARCHAR) AS WCVP_FAMILY,
      CAST(wcvp_genus AS VARCHAR) AS WCVP_GENUS,
      CAST(wcvp_species AS VARCHAR) AS WCVP_SPECIES
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


taxonomy <- canonical |>
  left_join(
    wcvp,
    by = "FINAL_WCVP_ID"
  ) |>
  mutate(
    
    RANK_NORMALISED =
      str_to_upper(
        str_trim(
          coalesce(
            WCVP_TAXON_RANK,
            FINAL_WCVP_RANK
          )
        )
      )
  )


# ==============================================================================
# 13. RECONSTRUCT RANK GROUP
# ==============================================================================

taxonomy <- taxonomy |>
  mutate(
    
    RANK_GROUP =
      case_when(
        
        RANK_NORMALISED ==
          "SPECIES" ~
          "Species",
        
        RANK_NORMALISED ==
          "GENUS" ~
          "Genus",
        
        RANK_NORMALISED ==
          "SUBSPECIES" ~
          "Subspecies",
        
        RANK_NORMALISED ==
          "VARIETY" ~
          "Variety",
        
        RANK_NORMALISED ==
          "FORM" ~
          "Form",
        
        TRUE ~
          "Other rank"
      )
  )


# ==============================================================================
# 14. IDENTIFY BEST 02b CONCEPT-LEVEL TABLE
# ==============================================================================

best_id_route <- id_tests |>
  filter(
    !is.na(
      N_CANONICAL_MATCHES
    )
  ) |>
  arrange(
    desc(
      N_CANONICAL_MATCHES
    )
  ) |>
  slice_head(
    n = 1
  )


if (nrow(best_id_route) != 1L) {
  
  stop(
    "Could not identify the best 02b concept-level linkage route."
  )
}


CONCEPT_TABLE <-
  best_id_route$TABLE_NAME[[1]]

CONCEPT_ID_FIELD <-
  best_id_route$ID_FIELD[[1]]


if (
  best_id_route$N_CANONICAL_MATCHES[[1]] !=
  EXPECTED_CANONICAL_POPULATION
) {
  
  stop(
    "Best concept-level route does not cover all 11,439 concepts."
  )
}


cat(
  "Primary concept-level table: ",
  CONCEPT_TABLE,
  "\n",
  sep = ""
)

cat(
  "Primary concept-level ID field: ",
  CONCEPT_ID_FIELD,
  "\n\n",
  sep = ""
)


# ==============================================================================
# 15. IDENTIFY BEST 02b FAMILY-LEVEL TABLE
# ==============================================================================

best_family_route <- family_tests |>
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
    )
  ) |>
  slice_head(
    n = 1
  )


if (nrow(best_family_route) != 1L) {
  
  stop(
    "Could not identify the best 02b family-level linkage route."
  )
}


FAMILY_TABLE <-
  best_family_route$TABLE_NAME[[1]]

FAMILY_FIELD <-
  best_family_route$FAMILY_FIELD[[1]]


if (
  best_family_route$N_CONCEPTS_COVERED[[1]] !=
  EXPECTED_CANONICAL_POPULATION
) {
  
  stop(
    "Best family-level route does not cover all 11,439 concepts."
  )
}


cat(
  "Independent family-level table: ",
  FAMILY_TABLE,
  "\n",
  sep = ""
)

cat(
  "Independent family field: ",
  FAMILY_FIELD,
  "\n\n",
  sep = ""
)


# ==============================================================================
# 16. IDENTIFY CLASSIFICATION FIELDS IN PRIMARY CONCEPT TABLE
# ==============================================================================

concept_fields <- field_inventory |>
  filter(
    TABLE_NAME ==
      CONCEPT_TABLE
  )


classification_candidates <- concept_fields |>
  filter(
    POSSIBLE_MAJOR_GROUP |
      POSSIBLE_CLASSIFICATION
  ) |>
  pull(
    FIELD_NAME
  ) |>
  unique()


if (length(classification_candidates) == 0L) {
  
  stop(
    "No classification fields identified in concept-level table: ",
    CONCEPT_TABLE
  )
}


# ==============================================================================
# 17. IDENTIFY CLASSIFICATION FIELDS IN FAMILY TABLE
# ==============================================================================

family_table_fields <- field_inventory |>
  filter(
    TABLE_NAME ==
      FAMILY_TABLE
  )


family_classification_candidates <-
  family_table_fields |>
  filter(
    POSSIBLE_MAJOR_GROUP |
      POSSIBLE_CLASSIFICATION
  ) |>
  pull(
    FIELD_NAME
  ) |>
  unique()


if (
  length(
    family_classification_candidates
  ) == 0L
) {
  
  stop(
    "No classification fields identified in family-level table: ",
    FAMILY_TABLE
  )
}


# ==============================================================================
# 18. SCORE CLASSIFICATION FIELDS
#
# Prefer explicit lineage / vascular / major-group fields.
# ==============================================================================

score_classification_field <- function(x) {
  
  x_lower <-
    str_to_lower(x)
  
  score <- 0L
  
  score <-
    score +
    if_else(
      str_detect(
        x_lower,
        "vascular"
      ),
      100L,
      0L
    )
  
  score <-
    score +
    if_else(
      str_detect(
        x_lower,
        "major.*group"
      ),
      90L,
      0L
    )
  
  score <-
    score +
    if_else(
      str_detect(
        x_lower,
        "lineage"
      ),
      80L,
      0L
    )
  
  score <-
    score +
    if_else(
      str_detect(
        x_lower,
        "plant.*group|taxonomic.*group|higher.*group"
      ),
      70L,
      0L
    )
  
  score <-
    score +
    if_else(
      str_detect(
        x_lower,
        "classification"
      ),
      60L,
      0L
    )
  
  score
}


concept_field_scores <- tibble(
  FIELD_NAME =
    classification_candidates
) |>
  mutate(
    SCORE =
      map_int(
        FIELD_NAME,
        score_classification_field
      )
  ) |>
  arrange(
    desc(SCORE),
    FIELD_NAME
  )


family_field_scores <- tibble(
  FIELD_NAME =
    family_classification_candidates
) |>
  mutate(
    SCORE =
      map_int(
        FIELD_NAME,
        score_classification_field
      )
  ) |>
  arrange(
    desc(SCORE),
    FIELD_NAME
  )


# ==============================================================================
# 19. FUNCTION — READ CLASSIFICATION FIELD
# ==============================================================================

read_classification_field <- function(
    table_name,
    id_field,
    class_field,
    output_id_name
) {
  
  sql <- paste0(
    'SELECT ',
    'CAST("',
    id_field,
    '" AS VARCHAR) AS LINK_ID, ',
    'CAST("',
    class_field,
    '" AS VARCHAR) AS CLASS_VALUE ',
    'FROM "',
    table_name,
    '"'
  )
  
  DBI::dbGetQuery(
    con,
    sql
  ) |>
    as_tibble() |>
    mutate(
      
      LINK_ID =
        str_trim(
          as.character(
            LINK_ID
          )
        ),
      
      CLASS_VALUE =
        str_trim(
          as.character(
            CLASS_VALUE
          )
        ),
      
      CLASS_VALUE =
        na_if(
          CLASS_VALUE,
          ""
        )
    ) |>
    rename(
      !!output_id_name :=
        LINK_ID
    )
}


# ==============================================================================
# 20. SELECT CONCEPT CLASSIFICATION FIELD BY COVERAGE
#
# We test all candidates rather than assuming a field name.
# ==============================================================================

test_concept_field <- function(class_field) {
  
  dat <- tryCatch(
    
    read_classification_field(
      CONCEPT_TABLE,
      CONCEPT_ID_FIELD,
      class_field,
      "FINAL_WCVP_ID"
    ),
    
    error = function(e) NULL
  )
  
  
  if (is.null(dat)) {
    
    return(
      tibble(
        FIELD_NAME =
          class_field,
        N_LINKED =
          0L,
        N_CLASSIFIED =
          0L,
        N_DISTINCT_VALUES =
          0L
      )
    )
  }
  
  
  dat <- dat |>
    distinct(
      FINAL_WCVP_ID,
      .keep_all = TRUE
    )
  
  
  linked <- canonical |>
    left_join(
      dat,
      by = "FINAL_WCVP_ID"
    )
  
  
  tibble(
    
    FIELD_NAME =
      class_field,
    
    N_LINKED =
      sum(
        !is.na(
          linked$FINAL_WCVP_ID
        )
      ),
    
    N_CLASSIFIED =
      sum(
        !is.na(
          linked$CLASS_VALUE
        )
      ),
    
    N_DISTINCT_VALUES =
      n_distinct(
        linked$CLASS_VALUE[
          !is.na(
            linked$CLASS_VALUE
          )
        ]
      )
  )
}


concept_field_tests <- map_dfr(
  classification_candidates,
  test_concept_field
) |>
  left_join(
    concept_field_scores,
    by = "FIELD_NAME"
  ) |>
  arrange(
    desc(N_CLASSIFIED),
    desc(SCORE),
    FIELD_NAME
  )


CONCEPT_CLASS_FIELD <-
  concept_field_tests$FIELD_NAME[[1]]


cat(
  "Selected concept classification field: ",
  CONCEPT_CLASS_FIELD,
  "\n",
  sep = ""
)


# ==============================================================================
# 21. SELECT FAMILY CLASSIFICATION FIELD BY COVERAGE
# ==============================================================================

test_family_class_field <- function(
    class_field
) {
  
  dat <- tryCatch(
    
    read_classification_field(
      FAMILY_TABLE,
      FAMILY_FIELD,
      class_field,
      "WCVP_FAMILY"
    ),
    
    error = function(e) NULL
  )
  
  
  if (is.null(dat)) {
    
    return(
      tibble(
        FIELD_NAME =
          class_field,
        N_FAMILIES =
          0L,
        N_CLASSIFIED =
          0L,
        N_DISTINCT_VALUES =
          0L
      )
    )
  }
  
  
  dat <- dat |>
    distinct(
      WCVP_FAMILY,
      .keep_all = TRUE
    )
  
  
  vpjd_families <- taxonomy |>
    filter(
      !is.na(
        WCVP_FAMILY
      ),
      WCVP_FAMILY != ""
    ) |>
    distinct(
      WCVP_FAMILY
    )
  
  
  linked <- vpjd_families |>
    left_join(
      dat,
      by = "WCVP_FAMILY"
    )
  
  
  tibble(
    
    FIELD_NAME =
      class_field,
    
    N_FAMILIES =
      nrow(linked),
    
    N_CLASSIFIED =
      sum(
        !is.na(
          linked$CLASS_VALUE
        )
      ),
    
    N_DISTINCT_VALUES =
      n_distinct(
        linked$CLASS_VALUE[
          !is.na(
            linked$CLASS_VALUE
          )
        ]
      )
  )
}


family_field_tests <- map_dfr(
  family_classification_candidates,
  test_family_class_field
) |>
  left_join(
    family_field_scores,
    by = "FIELD_NAME"
  ) |>
  arrange(
    desc(N_CLASSIFIED),
    desc(SCORE),
    FIELD_NAME
  )


FAMILY_CLASS_FIELD <-
  family_field_tests$FIELD_NAME[[1]]


cat(
  "Selected family classification field: ",
  FAMILY_CLASS_FIELD,
  "\n\n",
  sep = ""
)


# ==============================================================================
# 22. LOAD CONCEPT-LEVEL CLASSIFICATION
# ==============================================================================

concept_classification <-
  read_classification_field(
    CONCEPT_TABLE,
    CONCEPT_ID_FIELD,
    CONCEPT_CLASS_FIELD,
    "FINAL_WCVP_ID"
  ) |>
  distinct(
    FINAL_WCVP_ID,
    .keep_all = TRUE
  ) |>
  rename(
    CONCEPT_CLASSIFICATION =
      CLASS_VALUE
  )


# ==============================================================================
# 23. LOAD FAMILY-LEVEL CLASSIFICATION
# ==============================================================================

family_classification <-
  read_classification_field(
    FAMILY_TABLE,
    FAMILY_FIELD,
    FAMILY_CLASS_FIELD,
    "WCVP_FAMILY"
  ) |>
  distinct(
    WCVP_FAMILY,
    .keep_all = TRUE
  ) |>
  rename(
    FAMILY_CLASSIFICATION =
      CLASS_VALUE
  )


# ==============================================================================
# 24. NORMALISE CLASSIFICATION VALUES
#
# Preserve original source values, but create comparison forms.
# ==============================================================================

normalise_classification <- function(x) {
  
  x |>
    as.character() |>
    str_trim() |>
    str_to_upper() |>
    str_replace_all(
      "[^A-Z0-9]+",
      "_"
    ) |>
    str_replace_all(
      "^_+|_+$",
      ""
    )
}


concept_classification <- concept_classification |>
  mutate(
    CONCEPT_CLASSIFICATION_NORMALISED =
      normalise_classification(
        CONCEPT_CLASSIFICATION
      )
  )


family_classification <- family_classification |>
  mutate(
    FAMILY_CLASSIFICATION_NORMALISED =
      normalise_classification(
        FAMILY_CLASSIFICATION
      )
  )


# ==============================================================================
# 25. ATTACH BOTH CLASSIFICATION ROUTES
# ==============================================================================

reconstructed <- taxonomy |>
  left_join(
    concept_classification,
    by = "FINAL_WCVP_ID"
  ) |>
  left_join(
    family_classification,
    by = "WCVP_FAMILY"
  )


# ==============================================================================
# 26. COMPARE CONCEPT AND FAMILY CLASSIFICATION
# ==============================================================================

reconstructed <- reconstructed |>
  mutate(
    
    CONCEPT_CLASSIFICATION_PRESENT =
      !is.na(
        CONCEPT_CLASSIFICATION_NORMALISED
      ) &
      CONCEPT_CLASSIFICATION_NORMALISED != "",
    
    FAMILY_CLASSIFICATION_PRESENT =
      !is.na(
        FAMILY_CLASSIFICATION_NORMALISED
      ) &
      FAMILY_CLASSIFICATION_NORMALISED != "",
    
    CLASSIFICATION_AGREEMENT =
      CONCEPT_CLASSIFICATION_PRESENT &
      FAMILY_CLASSIFICATION_PRESENT &
      (
        CONCEPT_CLASSIFICATION_NORMALISED ==
          FAMILY_CLASSIFICATION_NORMALISED
      )
  )


n_concept_classified <-
  sum(
    reconstructed$CONCEPT_CLASSIFICATION_PRESENT
  )

n_family_classified <-
  sum(
    reconstructed$FAMILY_CLASSIFICATION_PRESENT
  )

n_agree <-
  sum(
    reconstructed$CLASSIFICATION_AGREEMENT
  )

n_disagree <-
  sum(
    reconstructed$CONCEPT_CLASSIFICATION_PRESENT &
      reconstructed$FAMILY_CLASSIFICATION_PRESENT &
      !reconstructed$CLASSIFICATION_AGREEMENT
  )


# ==============================================================================
# 27. CLASSIFICATION DISAGREEMENT TABLE
# ==============================================================================

classification_disagreements <- reconstructed |>
  filter(
    CONCEPT_CLASSIFICATION_PRESENT,
    FAMILY_CLASSIFICATION_PRESENT,
    !CLASSIFICATION_AGREEMENT
  ) |>
  select(
    FINAL_WCVP_ID,
    FINAL_WCVP_RECOGNISED_NAME,
    FINAL_WCVP_RANK,
    WCVP_FAMILY,
    CONCEPT_CLASSIFICATION,
    FAMILY_CLASSIFICATION
  ) |>
  arrange(
    WCVP_FAMILY,
    FINAL_WCVP_RECOGNISED_NAME
  )


# ==============================================================================
# 28. REQUIRE COMPLETE CLASSIFICATION COVERAGE
# ==============================================================================

if (
  n_concept_classified !=
  EXPECTED_CANONICAL_POPULATION
) {
  
  stop(
    "Concept-level classification covers ",
    n_concept_classified,
    " / ",
    EXPECTED_CANONICAL_POPULATION,
    " concepts. Expected complete coverage."
  )
}


if (
  n_family_classified !=
  EXPECTED_CANONICAL_POPULATION
) {
  
  stop(
    "Family-level classification covers ",
    n_family_classified,
    " / ",
    EXPECTED_CANONICAL_POPULATION,
    " concepts. Expected complete coverage."
  )
}


# ==============================================================================
# 29. REQUIRE COMPLETE AGREEMENT
# ==============================================================================

if (n_disagree > 0L) {
  
  write_csv(
    classification_disagreements,
    file.path(
      OUTPUT_DIR,
      "VPJD_02f2_CLASSIFICATION_DISAGREEMENTS_STOP.csv"
    )
  )
  
  stop(
    "Concept-level and family-level classifications disagree for ",
    n_disagree,
    " canonical concepts.\n",
    "Review VPJD_02f2_CLASSIFICATION_DISAGREEMENTS_STOP.csv."
  )
}


# ==============================================================================
# 30. DEFINE FINAL RECONSTRUCTED CLASSIFICATION
# ==============================================================================

reconstructed <- reconstructed |>
  mutate(
    
    RECONSTRUCTED_CLASSIFICATION =
      CONCEPT_CLASSIFICATION,
    
    RECONSTRUCTED_CLASSIFICATION_NORMALISED =
      CONCEPT_CLASSIFICATION_NORMALISED,
    
    CLASSIFICATION_SOURCE =
      "02b_CONCEPT_LEVEL_WCVP_ID_VALIDATED_BY_FAMILY"
  )


# ==============================================================================
# 31. CLASSIFICATION PROFILE
# ==============================================================================

classification_profile <- reconstructed |>
  count(
    RECONSTRUCTED_CLASSIFICATION,
    RECONSTRUCTED_CLASSIFICATION_NORMALISED,
    name = "N_CONCEPTS"
  ) |>
  mutate(
    PERCENT_CANONICAL =
      round(
        100 *
          N_CONCEPTS /
          EXPECTED_CANONICAL_POPULATION,
        3
      )
  ) |>
  arrange(
    desc(
      N_CONCEPTS
    ),
    RECONSTRUCTED_CLASSIFICATION
  )


# ==============================================================================
# 32. RECONSTRUCT 02f RANK COUNTS
# ==============================================================================

rank_profile <- reconstructed |>
  count(
    RANK_GROUP,
    name = "N_CONCEPTS"
  ) |>
  arrange(
    desc(
      N_CONCEPTS
    )
  )


n_species <-
  sum(
    reconstructed$RANK_GROUP ==
      "Species"
  )

n_genus <-
  sum(
    reconstructed$RANK_GROUP ==
      "Genus"
  )

n_subspecies <-
  sum(
    reconstructed$RANK_GROUP ==
      "Subspecies"
  )

n_variety <-
  sum(
    reconstructed$RANK_GROUP ==
      "Variety"
  )

n_form <-
  sum(
    reconstructed$RANK_GROUP ==
      "Form"
  )

n_other <-
  sum(
    reconstructed$RANK_GROUP ==
      "Other rank"
  )

n_infraspecific <-
  n_subspecies +
  n_variety +
  n_form

n_non_species_genus <-
  nrow(
    reconstructed |>
      filter(
        !RANK_GROUP %in%
          c(
            "Species",
            "Genus"
          )
      )
  )


# ==============================================================================
# 33. RECONSTRUCT 02f AUDIT POPULATION
# ==============================================================================

audit_02f <- reconstructed |>
  filter(
    !RANK_GROUP %in%
      c(
        "Species",
        "Genus"
      )
  ) |>
  select(
    FINAL_WCVP_ID,
    FINAL_WCVP_RECOGNISED_NAME,
    FINAL_WCVP_RANK,
    FINAL_WCVP_STATUS,
    FINAL_WCVP_CONCEPT_CLASS,
    WCVP_TAXON_NAME,
    WCVP_TAXON_RANK,
    WCVP_FAMILY,
    WCVP_GENUS,
    WCVP_SPECIES,
    RANK_NORMALISED,
    RANK_GROUP,
    PROVISIONAL_STAR,
    IS_ALLOCATED,
    IS_UNRESOLVED,
    CONCEPT_CLASSIFICATION,
    FAMILY_CLASSIFICATION,
    RECONSTRUCTED_CLASSIFICATION,
    RECONSTRUCTED_CLASSIFICATION_NORMALISED,
    CLASSIFICATION_SOURCE,
    CLASSIFICATION_AGREEMENT
  ) |>
  arrange(
    WCVP_FAMILY,
    WCVP_GENUS,
    FINAL_WCVP_RECOGNISED_NAME
  )


# ==============================================================================
# 34. CLASSIFICATION PROFILE WITHIN 02f POPULATION
# ==============================================================================

audit_02f_classification_profile <- audit_02f |>
  count(
    RANK_GROUP,
    RECONSTRUCTED_CLASSIFICATION,
    RECONSTRUCTED_CLASSIFICATION_NORMALISED,
    name = "N_CONCEPTS"
  ) |>
  arrange(
    RANK_GROUP,
    desc(
      N_CONCEPTS
    )
  )


# ==============================================================================
# 35. IDENTIFY VASCULAR / NON-VASCULAR SIGNALS
#
# This stage deliberately does not silently recode the original 02b values.
# It derives an audit interpretation from the validated classification string.
# ==============================================================================

interpret_vascular_status <- function(x) {
  
  x_norm <-
    normalise_classification(x)
  
  case_when(
    
    is.na(x_norm) |
      x_norm == "" ~
      "UNKNOWN",
    
    str_detect(
      x_norm,
      "NON_VASCULAR|NONVASCULAR"
    ) ~
      "NON_VASCULAR",
    
    str_detect(
      x_norm,
      "BRYOPHY|MOSSES|MOSS|LIVERWORT|HORNWORT"
    ) ~
      "NON_VASCULAR",
    
    str_detect(
      x_norm,
      "VASCULAR|TRACHEOPHY"
    ) ~
      "VASCULAR",
    
    TRUE ~
      "CLASSIFIED_OTHER"
  )
}


reconstructed <- reconstructed |>
  mutate(
    VASCULAR_STATUS_INTERPRETED =
      interpret_vascular_status(
        RECONSTRUCTED_CLASSIFICATION
      )
  )


audit_02f <- audit_02f |>
  left_join(
    reconstructed |>
      select(
        FINAL_WCVP_ID,
        VASCULAR_STATUS_INTERPRETED
      ),
    by = "FINAL_WCVP_ID"
  )


# ==============================================================================
# 36. VASCULAR STATUS PROFILE — WHOLE VPJD
# ==============================================================================

vascular_profile_all <- reconstructed |>
  count(
    VASCULAR_STATUS_INTERPRETED,
    name = "N_CONCEPTS"
  ) |>
  mutate(
    PERCENT_CANONICAL =
      round(
        100 *
          N_CONCEPTS /
          EXPECTED_CANONICAL_POPULATION,
        3
      )
  ) |>
  arrange(
    desc(
      N_CONCEPTS
    )
  )


# ==============================================================================
# 37. VASCULAR STATUS PROFILE — 02f POPULATION
# ==============================================================================

vascular_profile_02f <- audit_02f |>
  count(
    RANK_GROUP,
    VASCULAR_STATUS_INTERPRETED,
    name = "N_CONCEPTS"
  ) |>
  arrange(
    RANK_GROUP,
    VASCULAR_STATUS_INTERPRETED
  )


# ==============================================================================
# 38. NON-VASCULAR REVIEW CANDIDATES
#
# IMPORTANT:
# These are candidates for 02f.3 review.
# They are NOT removed here.
# ==============================================================================

non_vascular_candidates <- reconstructed |>
  filter(
    VASCULAR_STATUS_INTERPRETED ==
      "NON_VASCULAR"
  ) |>
  select(
    FINAL_WCVP_ID,
    FINAL_WCVP_RECOGNISED_NAME,
    FINAL_WCVP_RANK,
    FINAL_WCVP_STATUS,
    WCVP_FAMILY,
    WCVP_GENUS,
    WCVP_SPECIES,
    RANK_GROUP,
    PROVISIONAL_STAR,
    RECONSTRUCTED_CLASSIFICATION,
    VASCULAR_STATUS_INTERPRETED
  ) |>
  mutate(
    REVIEW_STATUS =
      "REQUIRES_02f3_SCOPE_REVIEW",
    TAXONOMIC_ACTION_TAKEN =
      FALSE,
    STAR_ACTION_TAKEN =
      FALSE
  ) |>
  arrange(
    WCVP_FAMILY,
    FINAL_WCVP_RECOGNISED_NAME
  )


# ==============================================================================
# 39. UNINTERPRETED CLASSIFICATION REVIEW
# ==============================================================================

classification_interpretation_review <- reconstructed |>
  filter(
    VASCULAR_STATUS_INTERPRETED %in%
      c(
        "UNKNOWN",
        "CLASSIFIED_OTHER"
      )
  ) |>
  select(
    FINAL_WCVP_ID,
    FINAL_WCVP_RECOGNISED_NAME,
    FINAL_WCVP_RANK,
    WCVP_FAMILY,
    RANK_GROUP,
    RECONSTRUCTED_CLASSIFICATION,
    RECONSTRUCTED_CLASSIFICATION_NORMALISED,
    VASCULAR_STATUS_INTERPRETED
  ) |>
  arrange(
    VASCULAR_STATUS_INTERPRETED,
    WCVP_FAMILY,
    FINAL_WCVP_RECOGNISED_NAME
  )


# ==============================================================================
# 40. VERIFY CANONICAL POPULATION REMAINS UNCHANGED
# ==============================================================================

canonical_ids_after <-
  sort(
    reconstructed$FINAL_WCVP_ID
  )


canonical_stars_after <-
  reconstructed |>
  select(
    FINAL_WCVP_ID,
    PROVISIONAL_STAR
  ) |>
  arrange(
    FINAL_WCVP_ID
  )


ids_unchanged <-
  identical(
    canonical_ids_before,
    canonical_ids_after
  )


stars_unchanged <-
  identical(
    canonical_stars_before,
    canonical_stars_after
  )


# ==============================================================================
# 41. SUMMARY
# ==============================================================================

summary <- tibble(
  
  METRIC = c(
    
    "Canonical VPJD concepts",
    
    "Represented families",
    
    "Concept-level classifications attached",
    
    "Family-level classifications attached",
    
    "Concept/family classification agreements",
    
    "Concept/family classification disagreements",
    
    "Species concepts",
    
    "Genus concepts",
    
    "Subspecies concepts",
    
    "Variety concepts",
    
    "Form concepts",
    
    "Recognised infraspecific concepts",
    
    "Other-rank concepts",
    
    "Non-species/non-genus audit population",
    
    "Non-vascular review candidates",
    
    "Uninterpreted classification concepts",
    
    "Taxonomic changes made",
    
    "Star allocation changes made"
  ),
  
  N = c(
    
    nrow(reconstructed),
    
    n_distinct(
      reconstructed$WCVP_FAMILY
    ),
    
    n_concept_classified,
    
    n_family_classified,
    
    n_agree,
    
    n_disagree,
    
    n_species,
    
    n_genus,
    
    n_subspecies,
    
    n_variety,
    
    n_form,
    
    n_infraspecific,
    
    n_other,
    
    n_non_species_genus,
    
    nrow(
      non_vascular_candidates
    ),
    
    nrow(
      classification_interpretation_review
    ),
    
    0L,
    
    0L
  )
)


# ==============================================================================
# 42. VALIDATION
# ==============================================================================

validation <- tibble(
  
  CHECK = c(
    
    "02f.1 recommends concept-level WCVP-ID linkage",
    
    "Canonical population = 11,439",
    
    "Canonical population unique by WCVP ID",
    
    "Canonical represented families = 260",
    
    "Concept-level classification covers 11,439",
    
    "Family-level classification covers 11,439",
    
    "Concept and family classifications agree for all 11,439",
    
    "No concept/family classification disagreements",
    
    "Species population = 8,906",
    
    "Genus population = 823",
    
    "Subspecies population = 499",
    
    "Variety population = 1,147",
    
    "Form population = 62",
    
    "Recognised infraspecific population = 1,708",
    
    "Other-rank population = 2",
    
    "Non-species/non-genus population = 1,710",
    
    "Rank populations total 11,439",
    
    "Canonical WCVP IDs unchanged",
    
    "Canonical Star allocations unchanged",
    
    "No canonical taxa removed",
    
    "No taxonomic changes made",
    
    "No Star allocation changes made"
  ),
  
  PASS = c(
    
    recommendation$RECOMMENDED_LINKAGE_ROUTE[[1]] ==
      "CONCEPT_LEVEL_WCVP_ID",
    
    nrow(reconstructed) ==
      EXPECTED_CANONICAL_POPULATION,
    
    n_distinct(
      reconstructed$FINAL_WCVP_ID
    ) ==
      EXPECTED_CANONICAL_POPULATION,
    
    n_distinct(
      reconstructed$WCVP_FAMILY
    ) ==
      EXPECTED_FAMILIES,
    
    n_concept_classified ==
      EXPECTED_CANONICAL_POPULATION,
    
    n_family_classified ==
      EXPECTED_CANONICAL_POPULATION,
    
    n_agree ==
      EXPECTED_CANONICAL_POPULATION,
    
    n_disagree ==
      0L,
    
    n_species ==
      EXPECTED_SPECIES,
    
    n_genus ==
      EXPECTED_GENUS,
    
    n_subspecies ==
      EXPECTED_SUBSPECIES,
    
    n_variety ==
      EXPECTED_VARIETIES,
    
    n_form ==
      EXPECTED_FORMS,
    
    n_infraspecific ==
      EXPECTED_INFRASPECIFIC,
    
    n_other ==
      EXPECTED_OTHER_RANKS,
    
    n_non_species_genus ==
      EXPECTED_NON_SPECIES_GENUS,
    
    (
      n_species +
        n_genus +
        n_subspecies +
        n_variety +
        n_form +
        n_other
    ) ==
      EXPECTED_CANONICAL_POPULATION,
    
    ids_unchanged,
    
    stars_unchanged,
    
    nrow(reconstructed) ==
      EXPECTED_CANONICAL_POPULATION,
    
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
# 43. STOP IF CORE VALIDATION FAILS
# ==============================================================================

core_failures <- validation |>
  filter(
    !PASS
  )


if (nrow(core_failures) > 0L) {
  
  write_csv(
    validation,
    file.path(
      OUTPUT_DIR,
      "VPJD_02f2_VALIDATION_FAILURE.csv"
    )
  )
  
  stop(
    "02f.2 validation failed for ",
    nrow(core_failures),
    " check(s). ",
    "Review VPJD_02f2_VALIDATION_FAILURE.csv."
  )
}


# ==============================================================================
# 44. WRITE OUTPUTS
# ==============================================================================

write_csv(
  reconstructed,
  file.path(
    OUTPUT_DIR,
    "VPJD_02f2_reconstructed_lineage_classification.csv"
  )
)


write_csv(
  classification_profile,
  file.path(
    OUTPUT_DIR,
    "VPJD_02f2_classification_profile.csv"
  )
)


write_csv(
  rank_profile,
  file.path(
    OUTPUT_DIR,
    "VPJD_02f2_rank_profile.csv"
  )
)


write_csv(
  audit_02f,
  file.path(
    OUTPUT_DIR,
    "VPJD_02f2_non_species_genus_audit.csv"
  )
)


write_csv(
  audit_02f_classification_profile,
  file.path(
    OUTPUT_DIR,
    "VPJD_02f2_non_species_genus_classification_profile.csv"
  )
)


write_csv(
  vascular_profile_all,
  file.path(
    OUTPUT_DIR,
    "VPJD_02f2_vascular_profile_all.csv"
  )
)


write_csv(
  vascular_profile_02f,
  file.path(
    OUTPUT_DIR,
    "VPJD_02f2_vascular_profile_02f_population.csv"
  )
)


write_csv(
  non_vascular_candidates,
  file.path(
    OUTPUT_DIR,
    "VPJD_02f2_non_vascular_review_candidates.csv"
  )
)


write_csv(
  classification_interpretation_review,
  file.path(
    OUTPUT_DIR,
    "VPJD_02f2_classification_interpretation_review.csv"
  )
)


write_csv(
  classification_disagreements,
  file.path(
    OUTPUT_DIR,
    "VPJD_02f2_classification_disagreements.csv"
  )
)


write_csv(
  concept_field_tests,
  file.path(
    OUTPUT_DIR,
    "VPJD_02f2_concept_classification_field_tests.csv"
  )
)


write_csv(
  family_field_tests,
  file.path(
    OUTPUT_DIR,
    "VPJD_02f2_family_classification_field_tests.csv"
  )
)


write_csv(
  summary,
  file.path(
    OUTPUT_DIR,
    "VPJD_02f2_summary.csv"
  )
)


write_csv(
  validation,
  file.path(
    OUTPUT_DIR,
    "VPJD_02f2_validation.csv"
  )
)


# ==============================================================================
# 45. OUTPUT INVENTORY
# ==============================================================================

output_inventory <- tibble(
  
  FILE = c(
    
    "VPJD_02f2_reconstructed_lineage_classification.csv",
    
    "VPJD_02f2_classification_profile.csv",
    
    "VPJD_02f2_rank_profile.csv",
    
    "VPJD_02f2_non_species_genus_audit.csv",
    
    "VPJD_02f2_non_species_genus_classification_profile.csv",
    
    "VPJD_02f2_vascular_profile_all.csv",
    
    "VPJD_02f2_vascular_profile_02f_population.csv",
    
    "VPJD_02f2_non_vascular_review_candidates.csv",
    
    "VPJD_02f2_classification_interpretation_review.csv",
    
    "VPJD_02f2_classification_disagreements.csv",
    
    "VPJD_02f2_concept_classification_field_tests.csv",
    
    "VPJD_02f2_family_classification_field_tests.csv",
    
    "VPJD_02f2_summary.csv",
    
    "VPJD_02f2_validation.csv"
  ),
  
  PURPOSE = c(
    
    "Complete 11,439-concept reconstructed lineage dataset",
    
    "Whole-VPJD profile of validated 02b classifications",
    
    "Canonical rank composition",
    
    "Reconstructed 1,710-concept 02f audit population",
    
    "Classification profile of the 02f population",
    
    "Whole-VPJD interpreted vascular-status profile",
    
    "Vascular-status profile of the 02f population",
    
    "Non-vascular candidates carried forward to 02f.3",
    
    "Classifications requiring interpretation review",
    
    "Concept/family classification disagreements; expected zero",
    
    "Diagnostic selection of concept classification field",
    
    "Diagnostic selection of family classification field",
    
    "02f.2 numerical summary",
    
    "02f.2 validation record"
  )
)


write_csv(
  output_inventory,
  file.path(
    OUTPUT_DIR,
    "VPJD_02f2_output_inventory.csv"
  )
)


# ==============================================================================
# 46. FINAL REPORT
# ==============================================================================

cat("\n")
cat("============================================================\n")
cat("02f.2 RESULTS\n")
cat("============================================================\n\n")

print(
  summary,
  n = Inf
)


cat("\n")
cat("------------------------------------------------------------\n")
cat("WHOLE-VPJD CLASSIFICATION PROFILE\n")
cat("------------------------------------------------------------\n\n")

print(
  classification_profile,
  n = Inf,
  width = Inf
)


cat("\n")
cat("------------------------------------------------------------\n")
cat("WHOLE-VPJD VASCULAR STATUS PROFILE\n")
cat("------------------------------------------------------------\n\n")

print(
  vascular_profile_all,
  n = Inf
)


cat("\n")
cat("------------------------------------------------------------\n")
cat("02f POPULATION VASCULAR STATUS PROFILE\n")
cat("------------------------------------------------------------\n\n")

print(
  vascular_profile_02f,
  n = Inf
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
# 47. CLEAN DISCONNECT
# ==============================================================================

DBI::dbDisconnect(
  con,
  shutdown = TRUE
)

rm(con)

gc()


# ==============================================================================
# 48. COMPLETION REPORT
# ==============================================================================

cat("\n")
cat("============================================================\n")

cat(
  "VPJD TAXONOMIC REVISION 02f.2 v",
  VERSION,
  " COMPLETE\n",
  sep = ""
)

cat("============================================================\n\n")


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
  "Concept-level classification coverage: ",
  format(
    n_concept_classified,
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
  "Family-level classification coverage: ",
  format(
    n_family_classified,
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
  "Concept/family agreements: ",
  format(
    n_agree,
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
  "Non-species/non-genus audit population: ",
  format(
    n_non_species_genus,
    big.mark = ","
  ),
  "\n",
  sep = ""
)


cat(
  "Non-vascular review candidates: ",
  format(
    nrow(
      non_vascular_candidates
    ),
    big.mark = ","
  ),
  "\n",
  sep = ""
)


cat(
  "Uninterpreted classification concepts: ",
  format(
    nrow(
      classification_interpretation_review
    ),
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
  nrow(validation),
  " PASS\n",
  sep = ""
)


cat("\n")
cat("Canonical taxa removed: 0\n")
cat("Canonical taxonomy modified: FALSE\n")
cat("Star allocations modified: FALSE\n")
cat("Automatic exclusions: 0\n")


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