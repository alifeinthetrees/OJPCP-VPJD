# ==============================================================================
# VPJD TAXONOMIC REVISION
#
# 02f.2b — RECONSTRUCT VASCULAR / MAJOR-LINEAGE CLASSIFICATION
#          FROM VALIDATED 02b FAMILY CROSSWALK
#
# Purpose:
#   Reconstruct the validated 02b family-level classification across the
#   contemporary canonical VPJD population.
#
#   This module:
#     1. opens the canonical VPJD DuckDB read-only;
#     2. identifies the canonical 11,439-concept allocation table;
#     3. searches existing VPJD tables for the validated 02b family crosswalk;
#     4. identifies the best family-level classification source;
#     5. joins that classification to all canonical concepts by family;
#     6. tests coverage and consistency;
#     7. compares reconstructed results with the known 02b validation state;
#     8. writes diagnostic CSV outputs only.
#
# IMPORTANT:
#   - NO canonical taxonomy is modified.
#   - NO Star allocation is modified.
#   - NO taxa are removed.
#   - NO database tables are created or overwritten.
#   - Database is opened READ-ONLY.
#
# Expected canonical population:
#   11,439 concepts
#
# Known validated 02b state:
#   260 represented families
#   260 / 260 families explicitly classified
#   0 unresolved families
#   0 unresolved concepts
#   11,439 concepts assigned recognised major group
#   vascular + non-vascular = 11,439
#   0 contradictory family assignments
#
# ==============================================================================


# ==============================================================================
# 01. PACKAGES
# ==============================================================================

suppressPackageStartupMessages({
  
  library(DBI)
  library(duckdb)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(stringr)
  library(readr)
  library(tibble)
  
})


# ==============================================================================
# 02. VERSION
# ==============================================================================

VERSION <- "0.1.0"


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
  "02f_infraspecific_other_ranks",
  "02f2b_family_crosswalk_reconstruction"
)

dir.create(
  OUTPUT_DIR,
  recursive = TRUE,
  showWarnings = FALSE
)


# ==============================================================================
# 04. EXPECTED VALIDATED STATE
# ==============================================================================

EXPECTED_CANONICAL_POPULATION <- 11439L

EXPECTED_FAMILIES <- 260L

EXPECTED_UNRESOLVED_FAMILIES <- 0L

EXPECTED_UNRESOLVED_CONCEPTS <- 0L


# ==============================================================================
# 05. START REPORT
# ==============================================================================

cat("\n")
cat("============================================================\n")
cat("VPJD TAXONOMIC REVISION 02f.2b\n")
cat("RECONSTRUCT FROM VALIDATED 02b FAMILY CROSSWALK\n")
cat("============================================================\n\n")

cat(
  "Version: ",
  VERSION,
  "\n",
  sep = ""
)

cat(
  "Database:\n",
  DB_PATH,
  "\n\n",
  sep = ""
)

cat(
  "Output directory:\n",
  OUTPUT_DIR,
  "\n\n",
  sep = ""
)


# ==============================================================================
# 06. DATABASE CHECK
# ==============================================================================

if (!file.exists(DB_PATH)) {
  
  stop(
    "VPJD DuckDB does not exist:\n",
    DB_PATH
  )
  
}


# ==============================================================================
# 07. OPEN DUCKDB READ-ONLY
# ==============================================================================

con <- DBI::dbConnect(
  duckdb::duckdb(),
  dbdir = DB_PATH,
  read_only = TRUE
)


on.exit({
  
  try(
    DBI::dbDisconnect(
      con,
      shutdown = TRUE
    ),
    silent = TRUE
  )
  
}, add = TRUE)


cat("DuckDB opened read-only.\n\n")


# ==============================================================================
# 08. TABLE INVENTORY
# ==============================================================================

db_tables <- DBI::dbListTables(con)

cat(
  "Database tables detected: ",
  length(db_tables),
  "\n",
  sep = ""
)


# ==============================================================================
# 09. IDENTIFY CANONICAL ALLOCATION TABLE
# ==============================================================================

canonical_candidates <- c(
  
  "vpjd_star_provisional_wholesale_allocation",
  "vpjd_star_provisional_wholesale_star_allocation",
  "vpjd_star_accepted_population",
  "vpjd_accepted_population"
  
)


canonical_table <- canonical_candidates[
  canonical_candidates %in% db_tables
][1]


if (
  length(canonical_table) == 0L ||
  is.na(canonical_table)
) {
  
  stop(
    "Could not identify canonical VPJD allocation table."
  )
  
}


cat(
  "Canonical allocation table: ",
  canonical_table,
  "\n",
  sep = ""
)


# ==============================================================================
# 10. LOAD CANONICAL POPULATION
# ==============================================================================

canonical <- DBI::dbReadTable(
  con,
  canonical_table
) |>
  as_tibble()


required_canonical_fields <- c(
  "FINAL_WCVP_ID",
  "FINAL_WCVP_RECOGNISED_NAME",
  "FINAL_WCVP_RANK"
)


missing_canonical_fields <- setdiff(
  required_canonical_fields,
  names(canonical)
)


if (length(missing_canonical_fields) > 0L) {
  
  stop(
    "Canonical table is missing required fields: ",
    paste(
      missing_canonical_fields,
      collapse = ", "
    )
  )
  
}


canonical <- canonical |>
  mutate(
    FINAL_WCVP_ID = as.character(
      FINAL_WCVP_ID
    )
  )


if (
  nrow(canonical) !=
  EXPECTED_CANONICAL_POPULATION
) {
  
  stop(
    "Canonical population is ",
    nrow(canonical),
    "; expected ",
    EXPECTED_CANONICAL_POPULATION,
    "."
  )
  
}


if (
  n_distinct(canonical$FINAL_WCVP_ID) !=
  EXPECTED_CANONICAL_POPULATION
) {
  
  stop(
    "Canonical population is not unique by FINAL_WCVP_ID."
  )
  
}


cat(
  "Canonical population: ",
  format(
    nrow(canonical),
    big.mark = ","
  ),
  "\n\n",
  sep = ""
)


# ==============================================================================
# 11. IDENTIFY CANONICAL FAMILY FIELD
# ==============================================================================

family_field_candidates <- names(canonical)[
  str_detect(
    toupper(names(canonical)),
    "FAMILY"
  )
]


cat("Canonical family-like fields:\n")

if (length(family_field_candidates) == 0L) {
  
  cat("  None\n")
  
} else {
  
  cat(
    paste0(
      "  ",
      family_field_candidates
    ),
    sep = "\n"
  )
  
  cat("\n")
  
}


# ==============================================================================
# 12. FIND WCVP LOOKUP TABLE IF FAMILY IS NOT ATTACHED
# ==============================================================================

canonical_family_field <- NA_character_


if (length(family_field_candidates) > 0L) {
  
  family_coverage <- map_dfr(
    
    family_field_candidates,
    
    function(fld) {
      
      x <- canonical[[fld]]
      
      tibble(
        FIELD = fld,
        N_NON_MISSING = sum(
          !is.na(x) &
            trimws(
              as.character(x)
            ) != ""
        ),
        N_DISTINCT = n_distinct(
          as.character(x)[
            !is.na(x) &
              trimws(
                as.character(x)
              ) != ""
          ]
        )
      )
      
    }
    
  ) |>
    arrange(
      desc(N_NON_MISSING),
      desc(N_DISTINCT)
    )
  
  
  print(
    family_coverage,
    n = Inf
  )
  
  
  if (
    nrow(family_coverage) > 0L &&
    family_coverage$N_NON_MISSING[[1]] ==
    EXPECTED_CANONICAL_POPULATION
  ) {
    
    canonical_family_field <-
      family_coverage$FIELD[[1]]
    
  }
  
}


# ==============================================================================
# 13. FALL BACK TO occurrence_wcvp_accepted_taxa
# ==============================================================================

if (is.na(canonical_family_field)) {
  
  cat(
    "\nCanonical allocation does not contain a complete family field.\n"
  )
  
  cat(
    "Attempting WCVP hierarchy linkage via occurrence_wcvp_accepted_taxa.\n\n"
  )
  
  
  WCVP_TABLE <- "occurrence_wcvp_accepted_taxa"
  
  
  if (!(WCVP_TABLE %in% db_tables)) {
    
    stop(
      "Cannot reconstruct family membership: ",
      WCVP_TABLE,
      " is absent."
    )
    
  }
  
  
  wcvp_fields <- DBI::dbListFields(
    con,
    WCVP_TABLE
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
      "WCVP lookup missing required fields: ",
      paste(
        missing_wcvp_fields,
        collapse = ", "
      )
    )
    
  }
  
  
  wcvp_family <- DBI::dbGetQuery(
    
    con,
    
    paste0(
      '
      SELECT
        wcvp_plant_name_id,
        wcvp_family
      FROM "',
      WCVP_TABLE,
      '"
      '
    )
    
  ) |>
    as_tibble() |>
    transmute(
      
      FINAL_WCVP_ID =
        as.character(
          wcvp_plant_name_id
        ),
      
      FAMILY =
        as.character(
          wcvp_family
        )
      
    ) |>
    filter(
      !is.na(FINAL_WCVP_ID)
    ) |>
    distinct(
      FINAL_WCVP_ID,
      .keep_all = TRUE
    )
  
  
  canonical <- canonical |>
    left_join(
      wcvp_family,
      by = "FINAL_WCVP_ID"
    )
  
  
  canonical_family_field <- "FAMILY"
  
}


# ==============================================================================
# 14. STANDARDISE CANONICAL FAMILY
# ==============================================================================

canonical <- canonical |>
  mutate(
    
    FAMILY_02F2B = str_squish(
      as.character(
        .data[[canonical_family_field]]
      )
    ),
    
    FAMILY_02F2B = na_if(
      FAMILY_02F2B,
      ""
    )
    
  )


n_missing_family <- sum(
  is.na(
    canonical$FAMILY_02F2B
  )
)


n_canonical_families <- n_distinct(
  canonical$FAMILY_02F2B[
    !is.na(
      canonical$FAMILY_02F2B
    )
  ]
)


cat("\n")
cat(
  "Canonical family field used: ",
  canonical_family_field,
  "\n",
  sep = ""
)

cat(
  "Canonical concepts lacking family: ",
  format(
    n_missing_family,
    big.mark = ","
  ),
  "\n",
  sep = ""
)

cat(
  "Represented canonical families: ",
  n_canonical_families,
  "\n\n",
  sep = ""
)


# ==============================================================================
# 15. IDENTIFY 02b-RELATED TABLES
# ==============================================================================

candidate_02b_tables <- db_tables[
  str_detect(
    tolower(db_tables),
    "02b|lineage|family|vascular|major_group|major"
  )
]


cat("------------------------------------------------------------\n")
cat("CANDIDATE 02b / LINEAGE TABLES\n")
cat("------------------------------------------------------------\n\n")


if (length(candidate_02b_tables) == 0L) {
  
  cat("No candidate tables identified by table name.\n")
  
} else {
  
  cat(
    paste0(
      "  ",
      sort(candidate_02b_tables)
    ),
    sep = "\n"
  )
  
  cat("\n")
  
}


# ==============================================================================
# 16. INSPECT ALL TABLE SCHEMAS FOR FAMILY + CLASSIFICATION FIELDS
# ==============================================================================

cat(
  "\nInspecting database schemas for family-level classification sources...\n"
)


schema_inventory <- map_dfr(
  
  db_tables,
  
  function(tbl) {
    
    fields <- tryCatch(
      
      DBI::dbListFields(
        con,
        tbl
      ),
      
      error = function(e) {
        character()
      }
      
    )
    
    
    if (length(fields) == 0L) {
      
      return(
        tibble()
      )
      
    }
    
    
    tibble(
      
      TABLE_NAME = tbl,
      FIELD_NAME = fields,
      
      FIELD_UPPER =
        toupper(fields)
      
    )
    
  }
  
)


family_schema <- schema_inventory |>
  filter(
    str_detect(
      FIELD_UPPER,
      "FAMILY"
    )
  )


classification_schema <- schema_inventory |>
  filter(
    str_detect(
      FIELD_UPPER,
      paste(
        c(
          "VASCULAR",
          "LINEAGE",
          "MAJOR",
          "GROUP",
          "DIVISION",
          "PHYLUM",
          "CLASSIFICATION",
          "PLANT_GROUP"
        ),
        collapse = "|"
      )
    )
  )


# ==============================================================================
# 17. TABLES CONTAINING BOTH FAMILY AND CLASSIFICATION-LIKE FIELDS
# ==============================================================================

family_tables <- unique(
  family_schema$TABLE_NAME
)


classification_tables <- unique(
  classification_schema$TABLE_NAME
)


crosswalk_candidate_tables <- intersect(
  family_tables,
  classification_tables
)


cat(
  "Tables containing family field(s): ",
  length(family_tables),
  "\n",
  sep = ""
)

cat(
  "Tables containing classification-like field(s): ",
  length(classification_tables),
  "\n",
  sep = ""
)

cat(
  "Tables containing BOTH: ",
  length(crosswalk_candidate_tables),
  "\n\n",
  sep = ""
)


# ==============================================================================
# 18. BUILD CROSSWALK CANDIDATE INVENTORY
# ==============================================================================

crosswalk_inventory <- map_dfr(
  
  crosswalk_candidate_tables,
  
  function(tbl) {
    
    fields <- DBI::dbListFields(
      con,
      tbl
    )
    
    
    family_fields <- fields[
      str_detect(
        toupper(fields),
        "FAMILY"
      )
    ]
    
    
    class_fields <- fields[
      str_detect(
        toupper(fields),
        paste(
          c(
            "VASCULAR",
            "LINEAGE",
            "MAJOR",
            "GROUP",
            "DIVISION",
            "PHYLUM",
            "CLASSIFICATION",
            "PLANT_GROUP"
          ),
          collapse = "|"
        )
      )
    ]
    
    
    n_rows <- tryCatch(
      
      DBI::dbGetQuery(
        
        con,
        
        paste0(
          'SELECT COUNT(*) AS N FROM "',
          tbl,
          '"'
        )
        
      )$N[[1]],
      
      error = function(e) {
        NA_integer_
      }
      
    )
    
    
    tibble(
      
      TABLE_NAME = tbl,
      
      N_ROWS = as.numeric(
        n_rows
      ),
      
      FAMILY_FIELDS = paste(
        family_fields,
        collapse = " | "
      ),
      
      CLASSIFICATION_FIELDS = paste(
        class_fields,
        collapse = " | "
      ),
      
      N_FAMILY_FIELDS =
        length(family_fields),
      
      N_CLASSIFICATION_FIELDS =
        length(class_fields),
      
      TABLE_NAME_02B_SIGNAL =
        str_detect(
          tolower(tbl),
          "02b"
        ),
      
      TABLE_NAME_LINEAGE_SIGNAL =
        str_detect(
          tolower(tbl),
          "lineage|vascular|major"
        ),
      
      ROWCOUNT_260_SIGNAL =
        !is.na(n_rows) &&
        n_rows == EXPECTED_FAMILIES
      
    )
    
  }
  
) |>
  mutate(
    
    PRIORITY_SCORE =
      as.integer(TABLE_NAME_02B_SIGNAL) * 10L +
      as.integer(ROWCOUNT_260_SIGNAL) * 8L +
      as.integer(TABLE_NAME_LINEAGE_SIGNAL) * 4L +
      pmin(
        N_CLASSIFICATION_FIELDS,
        3L
      )
    
  ) |>
  arrange(
    desc(PRIORITY_SCORE),
    TABLE_NAME
  )


cat("------------------------------------------------------------\n")
cat("FAMILY CROSSWALK CANDIDATES\n")
cat("------------------------------------------------------------\n\n")


print(
  crosswalk_inventory,
  n = Inf,
  width = Inf
)


# ==============================================================================
# 19. REQUIRE AT LEAST ONE CANDIDATE
# ==============================================================================

if (nrow(crosswalk_inventory) == 0L) {
  
  stop(
    paste0(
      "No database table containing both a family field ",
      "and classification-like field was identified.\n",
      "Review the schema outputs written by 02f.2a."
    )
  )
  
}


# ==============================================================================
# 20. SELECT BEST CANDIDATE TABLE
# ==============================================================================

best_crosswalk_table <-
  crosswalk_inventory$TABLE_NAME[[1]]


cat(
  "\nBest candidate crosswalk table: ",
  best_crosswalk_table,
  "\n",
  sep = ""
)


# ==============================================================================
# 21. LOAD BEST CANDIDATE
# ==============================================================================

crosswalk_raw <- DBI::dbReadTable(
  con,
  best_crosswalk_table
) |>
  as_tibble()


cat(
  "Candidate rows: ",
  format(
    nrow(crosswalk_raw),
    big.mark = ","
  ),
  "\n",
  sep = ""
)


# ==============================================================================
# 22. IDENTIFY FAMILY FIELD IN CROSSWALK
# ==============================================================================

crosswalk_family_fields <- names(crosswalk_raw)[
  str_detect(
    toupper(names(crosswalk_raw)),
    "FAMILY"
  )
]


if (length(crosswalk_family_fields) == 0L) {
  
  stop(
    "Selected crosswalk table has no family field."
  )
  
}


crosswalk_family_profile <- map_dfr(
  
  crosswalk_family_fields,
  
  function(fld) {
    
    values <- str_squish(
      as.character(
        crosswalk_raw[[fld]]
      )
    )
    
    
    tibble(
      
      FIELD = fld,
      
      N_NON_MISSING = sum(
        !is.na(values) &
          values != ""
      ),
      
      N_DISTINCT = n_distinct(
        values[
          !is.na(values) &
            values != ""
        ]
      )
      
    )
    
  }
  
) |>
  arrange(
    desc(
      N_DISTINCT ==
        EXPECTED_FAMILIES
    ),
    desc(N_NON_MISSING),
    desc(N_DISTINCT)
  )


crosswalk_family_field <-
  crosswalk_family_profile$FIELD[[1]]


cat(
  "Crosswalk family field: ",
  crosswalk_family_field,
  "\n",
  sep = ""
)


# ==============================================================================
# 23. IDENTIFY CLASSIFICATION FIELDS IN CROSSWALK
# ==============================================================================

crosswalk_class_fields <- names(crosswalk_raw)[
  
  str_detect(
    
    toupper(names(crosswalk_raw)),
    
    paste(
      c(
        "VASCULAR",
        "LINEAGE",
        "MAJOR",
        "GROUP",
        "DIVISION",
        "PHYLUM",
        "CLASSIFICATION",
        "PLANT_GROUP"
      ),
      collapse = "|"
    )
    
  )
  
]


if (length(crosswalk_class_fields) == 0L) {
  
  stop(
    "Selected crosswalk contains no classification fields."
  )
  
}


cat("\nCandidate classification fields:\n")

cat(
  paste0(
    "  ",
    crosswalk_class_fields
  ),
  sep = "\n"
)

cat("\n")


# ==============================================================================
# 24. PROFILE CLASSIFICATION FIELDS
# ==============================================================================

classification_field_profile <- map_dfr(
  
  crosswalk_class_fields,
  
  function(fld) {
    
    x <- str_squish(
      as.character(
        crosswalk_raw[[fld]]
      )
    )
    
    
    non_missing <- x[
      !is.na(x) &
        x != ""
    ]
    
    
    tibble(
      
      FIELD = fld,
      
      N_NON_MISSING =
        length(non_missing),
      
      N_DISTINCT =
        n_distinct(non_missing),
      
      VALUES =
        paste(
          head(
            sort(
              unique(non_missing)
            ),
            25
          ),
          collapse = " | "
        )
      
    )
    
  }
  
) |>
  arrange(
    desc(N_NON_MISSING),
    N_DISTINCT,
    FIELD
  )


cat("\n")
cat("------------------------------------------------------------\n")
cat("CLASSIFICATION FIELD PROFILE\n")
cat("------------------------------------------------------------\n\n")


print(
  classification_field_profile,
  n = Inf,
  width = Inf
)


# ==============================================================================
# 25. CONSTRUCT FAMILY CROSSWALK
# ==============================================================================

family_crosswalk <- crosswalk_raw |>
  transmute(
    
    FAMILY =
      str_squish(
        as.character(
          .data[[crosswalk_family_field]]
        )
      ),
    
    across(
      all_of(
        crosswalk_class_fields
      ),
      ~ str_squish(
        as.character(.x)
      )
    )
    
  ) |>
  mutate(
    FAMILY = na_if(
      FAMILY,
      ""
    )
  ) |>
  filter(
    !is.na(FAMILY)
  )


# ==============================================================================
# 26. CHECK FAMILY DUPLICATION / CONTRADICTION
# ==============================================================================

family_duplicate_profile <- family_crosswalk |>
  group_by(FAMILY) |>
  summarise(
    
    N_ROWS = n(),
    
    across(
      all_of(
        crosswalk_class_fields
      ),
      ~ n_distinct(
        .x[
          !is.na(.x) &
            .x != ""
        ]
      ),
      .names = "N_DISTINCT_{.col}"
    ),
    
    .groups = "drop"
    
  )


contradiction_fields <- paste0(
  "N_DISTINCT_",
  crosswalk_class_fields
)


family_duplicate_profile <-
  family_duplicate_profile |>
  mutate(
    
    CONTRADICTORY_CLASSIFICATION =
      if_any(
        all_of(
          contradiction_fields
        ),
        ~ .x > 1L
      )
    
  )


n_contradictory_families <- sum(
  family_duplicate_profile$CONTRADICTORY_CLASSIFICATION,
  na.rm = TRUE
)


cat(
  "\nContradictory family assignments: ",
  n_contradictory_families,
  "\n",
  sep = ""
)


# ==============================================================================
# 27. REDUCE TO UNIQUE FAMILY CROSSWALK
# ==============================================================================

family_crosswalk_unique <- family_crosswalk |>
  group_by(FAMILY) |>
  summarise(
    
    across(
      
      all_of(
        crosswalk_class_fields
      ),
      
      ~ {
        
        values <- unique(
          .x[
            !is.na(.x) &
              .x != ""
          ]
        )
        
        if (length(values) == 0L) {
          
          NA_character_
          
        } else if (length(values) == 1L) {
          
          values[[1]]
          
        } else {
          
          paste(
            sort(values),
            collapse = " || "
          )
          
        }
        
      }
      
    ),
    
    .groups = "drop"
    
  )


n_crosswalk_families <- nrow(
  family_crosswalk_unique
)


cat(
  "Unique classified families in candidate crosswalk: ",
  n_crosswalk_families,
  "\n",
  sep = ""
)


# ==============================================================================
# 28. COMPARE FAMILY SETS
# ==============================================================================

canonical_families <- canonical |>
  distinct(
    FAMILY = FAMILY_02F2B
  ) |>
  filter(
    !is.na(FAMILY)
  )


crosswalk_families <- family_crosswalk_unique |>
  distinct(FAMILY)


families_missing_from_crosswalk <- anti_join(
  canonical_families,
  crosswalk_families,
  by = "FAMILY"
)


families_not_in_canonical <- anti_join(
  crosswalk_families,
  canonical_families,
  by = "FAMILY"
)


cat(
  "Canonical families missing from crosswalk: ",
  nrow(families_missing_from_crosswalk),
  "\n",
  sep = ""
)

cat(
  "Crosswalk families absent from canonical VPJD: ",
  nrow(families_not_in_canonical),
  "\n\n",
  sep = ""
)


# ==============================================================================
# 29. JOIN CROSSWALK TO CANONICAL POPULATION
# ==============================================================================

reconstructed <- canonical |>
  left_join(
    family_crosswalk_unique,
    by = c(
      "FAMILY_02F2B" = "FAMILY"
    )
  )


if (
  nrow(reconstructed) !=
  EXPECTED_CANONICAL_POPULATION
) {
  
  stop(
    "Family crosswalk join altered canonical row count."
  )
  
}


if (
  n_distinct(
    reconstructed$FINAL_WCVP_ID
  ) !=
  EXPECTED_CANONICAL_POPULATION
) {
  
  stop(
    "Family crosswalk join altered canonical WCVP-ID uniqueness."
  )
  
}


# ==============================================================================
# 30. DETERMINE CLASSIFICATION COVERAGE
# ==============================================================================

classification_matrix <- reconstructed |>
  select(
    FINAL_WCVP_ID,
    FAMILY_02F2B,
    all_of(
      crosswalk_class_fields
    )
  )


classification_coverage <- map_dfr(
  
  crosswalk_class_fields,
  
  function(fld) {
    
    x <- str_squish(
      as.character(
        classification_matrix[[fld]]
      )
    )
    
    
    tibble(
      
      FIELD = fld,
      
      N_CLASSIFIED_CONCEPTS = sum(
        !is.na(x) &
          x != ""
      ),
      
      N_UNCLASSIFIED_CONCEPTS = sum(
        is.na(x) |
          x == ""
      ),
      
      PERCENT_CLASSIFIED = round(
        100 *
          sum(
            !is.na(x) &
              x != ""
          ) /
          EXPECTED_CANONICAL_POPULATION,
        4
      ),
      
      N_DISTINCT_VALUES = n_distinct(
        x[
          !is.na(x) &
            x != ""
        ]
      )
      
    )
    
  }
  
) |>
  arrange(
    desc(N_CLASSIFIED_CONCEPTS),
    N_DISTINCT_VALUES,
    FIELD
  )


cat("------------------------------------------------------------\n")
cat("CONCEPT-LEVEL CLASSIFICATION COVERAGE\n")
cat("------------------------------------------------------------\n\n")


print(
  classification_coverage,
  n = Inf
)


# ==============================================================================
# 31. SELECT BEST CLASSIFICATION FIELD
# ==============================================================================

best_classification_field <-
  classification_coverage$FIELD[[1]]


best_classification_coverage <-
  classification_coverage$N_CLASSIFIED_CONCEPTS[[1]]


cat(
  "\nBest classification field: ",
  best_classification_field,
  "\n",
  sep = ""
)

cat(
  "Coverage: ",
  format(
    best_classification_coverage,
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


# ==============================================================================
# 32. STANDARDISE SELECTED MAJOR GROUP
# ==============================================================================

reconstructed <- reconstructed |>
  mutate(
    
    VPJD_MAJOR_GROUP_02F2B =
      str_squish(
        as.character(
          .data[[
            best_classification_field
          ]]
        )
      ),
    
    VPJD_MAJOR_GROUP_02F2B =
      na_if(
        VPJD_MAJOR_GROUP_02F2B,
        ""
      )
    
  )


# ==============================================================================
# 33. MAJOR-GROUP PROFILE
# ==============================================================================

major_group_profile <- reconstructed |>
  count(
    VPJD_MAJOR_GROUP_02F2B,
    name = "N_CONCEPTS",
    sort = TRUE
  ) |>
  mutate(
    
    PERCENT_OF_VPJD = round(
      100 *
        N_CONCEPTS /
        EXPECTED_CANONICAL_POPULATION,
      4
    )
    
  )


cat("\n")
cat("------------------------------------------------------------\n")
cat("RECONSTRUCTED MAJOR-GROUP PROFILE\n")
cat("------------------------------------------------------------\n\n")


print(
  major_group_profile,
  n = Inf,
  width = Inf
)


# ==============================================================================
# 34. FAMILY-LEVEL PROFILE
# ==============================================================================

family_profile <- reconstructed |>
  distinct(
    FAMILY_02F2B,
    VPJD_MAJOR_GROUP_02F2B
  ) |>
  count(
    VPJD_MAJOR_GROUP_02F2B,
    name = "N_FAMILIES",
    sort = TRUE
  )


cat("\n")
cat("------------------------------------------------------------\n")
cat("FAMILY PROFILE BY MAJOR GROUP\n")
cat("------------------------------------------------------------\n\n")


print(
  family_profile,
  n = Inf,
  width = Inf
)


# ==============================================================================
# 35. RANK × MAJOR-GROUP PROFILE
# ==============================================================================

rank_group_profile <- reconstructed |>
  mutate(
    
    TAXON_RANK =
      str_squish(
        as.character(
          FINAL_WCVP_RANK
        )
      )
    
  ) |>
  count(
    VPJD_MAJOR_GROUP_02F2B,
    TAXON_RANK,
    name = "N_CONCEPTS"
  ) |>
  arrange(
    VPJD_MAJOR_GROUP_02F2B,
    desc(N_CONCEPTS),
    TAXON_RANK
  )


# ==============================================================================
# 36. UNCLASSIFIED CONCEPTS
# ==============================================================================

unclassified_concepts <- reconstructed |>
  filter(
    is.na(
      VPJD_MAJOR_GROUP_02F2B
    )
  )


n_unclassified_concepts <- nrow(
  unclassified_concepts
)


cat(
  "\nUnclassified canonical concepts: ",
  n_unclassified_concepts,
  "\n",
  sep = ""
)


# ==============================================================================
# 37. VALIDATION
# ==============================================================================

validation <- tibble(
  
  CHECK = c(
    
    "Canonical allocation table present",
    
    "Canonical population = 11,439",
    
    "Canonical population unique by WCVP ID",
    
    "Canonical concepts all have family",
    
    "Canonical family population = 260",
    
    "Family/classification crosswalk source identified",
    
    "Crosswalk contains 260 represented families",
    
    "No canonical families missing from crosswalk",
    
    "No contradictory family classifications",
    
    "Family join retains 11,439 concepts",
    
    "Family join retains unique WCVP IDs",
    
    "Best classification field covers 11,439 concepts",
    
    "No unresolved concepts after reconstruction",
    
    "Major-group profile totals 11,439 concepts",
    
    "Family profile totals 260 families",
    
    "Canonical taxonomy unchanged",
    
    "Star allocations unchanged",
    
    "No taxa removed"
    
  ),
  
  PASS = c(
    
    canonical_table %in% db_tables,
    
    nrow(canonical) ==
      EXPECTED_CANONICAL_POPULATION,
    
    n_distinct(
      canonical$FINAL_WCVP_ID
    ) ==
      EXPECTED_CANONICAL_POPULATION,
    
    n_missing_family == 0L,
    
    n_canonical_families ==
      EXPECTED_FAMILIES,
    
    !is.na(
      best_crosswalk_table
    ),
    
    n_crosswalk_families ==
      EXPECTED_FAMILIES,
    
    nrow(
      families_missing_from_crosswalk
    ) == 0L,
    
    n_contradictory_families == 0L,
    
    nrow(reconstructed) ==
      EXPECTED_CANONICAL_POPULATION,
    
    n_distinct(
      reconstructed$FINAL_WCVP_ID
    ) ==
      EXPECTED_CANONICAL_POPULATION,
    
    best_classification_coverage ==
      EXPECTED_CANONICAL_POPULATION,
    
    n_unclassified_concepts ==
      EXPECTED_UNRESOLVED_CONCEPTS,
    
    sum(
      major_group_profile$N_CONCEPTS
    ) ==
      EXPECTED_CANONICAL_POPULATION,
    
    sum(
      family_profile$N_FAMILIES
    ) ==
      EXPECTED_FAMILIES,
    
    TRUE,
    
    TRUE,
    
    TRUE
    
  )
  
) |>
  mutate(
    
    RESULT = if_else(
      PASS,
      "PASS",
      "FAIL"
    )
    
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
# 38. WRITE DIAGNOSTIC OUTPUTS
# ==============================================================================

write_csv(
  crosswalk_inventory,
  file.path(
    OUTPUT_DIR,
    "VPJD_02f2b_crosswalk_candidate_inventory.csv"
  )
)


write_csv(
  crosswalk_family_profile,
  file.path(
    OUTPUT_DIR,
    "VPJD_02f2b_crosswalk_family_field_profile.csv"
  )
)


write_csv(
  classification_field_profile,
  file.path(
    OUTPUT_DIR,
    "VPJD_02f2b_classification_field_profile.csv"
  )
)


write_csv(
  family_crosswalk_unique,
  file.path(
    OUTPUT_DIR,
    "VPJD_02f2b_reconstructed_family_crosswalk.csv"
  )
)


write_csv(
  family_duplicate_profile,
  file.path(
    OUTPUT_DIR,
    "VPJD_02f2b_family_consistency_audit.csv"
  )
)


write_csv(
  families_missing_from_crosswalk,
  file.path(
    OUTPUT_DIR,
    "VPJD_02f2b_canonical_families_missing_from_crosswalk.csv"
  )
)


write_csv(
  families_not_in_canonical,
  file.path(
    OUTPUT_DIR,
    "VPJD_02f2b_crosswalk_families_not_in_canonical.csv"
  )
)


write_csv(
  classification_coverage,
  file.path(
    OUTPUT_DIR,
    "VPJD_02f2b_classification_coverage.csv"
  )
)


write_csv(
  major_group_profile,
  file.path(
    OUTPUT_DIR,
    "VPJD_02f2b_major_group_profile.csv"
  )
)


write_csv(
  family_profile,
  file.path(
    OUTPUT_DIR,
    "VPJD_02f2b_family_profile_by_major_group.csv"
  )
)


write_csv(
  rank_group_profile,
  file.path(
    OUTPUT_DIR,
    "VPJD_02f2b_rank_by_major_group.csv"
  )
)


write_csv(
  unclassified_concepts,
  file.path(
    OUTPUT_DIR,
    "VPJD_02f2b_unclassified_concepts.csv"
  )
)


write_csv(
  validation,
  file.path(
    OUTPUT_DIR,
    "VPJD_02f2b_validation.csv"
  )
)


# ==============================================================================
# 39. WRITE RECONSTRUCTED CONCEPT AUDIT
# ==============================================================================

concept_audit <- reconstructed |>
  select(
    
    FINAL_WCVP_ID,
    
    FINAL_WCVP_RECOGNISED_NAME,
    
    FINAL_WCVP_RANK,
    
    any_of(
      c(
        "FINAL_WCVP_STATUS",
        "FINAL_WCVP_CONCEPT_CLASS",
        "PROVISIONAL_STAR",
        "IS_ALLOCATED",
        "IS_UNRESOLVED"
      )
    ),
    
    FAMILY_02F2B,
    
    VPJD_MAJOR_GROUP_02F2B
    
  )


write_csv(
  concept_audit,
  file.path(
    OUTPUT_DIR,
    "VPJD_02f2b_concept_level_reconstruction.csv"
  )
)


# ==============================================================================
# 40. FINAL SUMMARY
# ==============================================================================

summary_table <- tibble(
  
  METRIC = c(
    
    "Canonical VPJD concepts",
    
    "Canonical families",
    
    "Crosswalk families",
    
    "Canonical families absent from crosswalk",
    
    "Contradictory family classifications",
    
    "Concepts classified by best field",
    
    "Unclassified concepts",
    
    "Taxonomic changes made",
    
    "Star allocation changes made",
    
    "Taxa removed"
    
  ),
  
  N = c(
    
    EXPECTED_CANONICAL_POPULATION,
    
    n_canonical_families,
    
    n_crosswalk_families,
    
    nrow(
      families_missing_from_crosswalk
    ),
    
    n_contradictory_families,
    
    best_classification_coverage,
    
    n_unclassified_concepts,
    
    0L,
    
    0L,
    
    0L
    
  )
  
)


write_csv(
  summary_table,
  file.path(
    OUTPUT_DIR,
    "VPJD_02f2b_summary.csv"
  )
)


cat("\n")
cat("------------------------------------------------------------\n")
cat("02f.2b SUMMARY\n")
cat("------------------------------------------------------------\n\n")


print(
  summary_table,
  n = Inf
)


# ==============================================================================
# 41. DECISION
# ==============================================================================

all_valid <- all(
  validation$PASS
)


if (all_valid) {
  
  decision <-
    "VALIDATED_FAMILY_CROSSWALK_RECONSTRUCTION"
  
} else {
  
  decision <-
    "REVIEW_REQUIRED"
}


cat("\n")
cat(
  "02f.2b decision: ",
  decision,
  "\n",
  sep = ""
)


# ==============================================================================
# 42. CLEAN DISCONNECT
# ==============================================================================

DBI::dbDisconnect(
  con,
  shutdown = TRUE
)

rm(con)

gc()


# ==============================================================================
# 43. COMPLETION REPORT
# ==============================================================================

cat("\n")
cat("============================================================\n")

cat(
  "VPJD TAXONOMIC REVISION 02f.2b v",
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
  "Canonical families: ",
  n_canonical_families,
  "\n",
  sep = ""
)


cat(
  "Crosswalk source table: ",
  best_crosswalk_table,
  "\n",
  sep = ""
)


cat(
  "Selected classification field: ",
  best_classification_field,
  "\n",
  sep = ""
)


cat(
  "Classified concepts: ",
  format(
    best_classification_coverage,
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
  "Unclassified concepts: ",
  n_unclassified_concepts,
  "\n",
  sep = ""
)


cat(
  "Contradictory family assignments: ",
  n_contradictory_families,
  "\n",
  sep = ""
)


cat(
  "Validation: ",
  sum(validation$PASS),
  "/",
  nrow(validation),
  " PASS\n\n",
  sep = ""
)


cat(
  "Decision: ",
  decision,
  "\n\n",
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