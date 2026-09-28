# ==============================================================================
# VPJD / OJPCP
# 02c — Audit taxonomic ranks
#
# File:
#   R/taxonomic_revision/02c_audit_taxonomic_ranks.R
#
# Version: 0.1.0
#
# PURPOSE
# -------
# Audit the taxonomic-rank structure of the contemporary VPJD population after
# completion of the major-lineage scope audit (02b).
#
# This module:
#   1. reads the canonical 11,439-concept contemporary VPJD taxonomy;
#   2. verifies successful completion of 02b;
#   3. standardises WCVP rank labels;
#   4. profiles all represented taxonomic ranks;
#   5. inventories all genus-rank concepts;
#   6. inventories all unusual / other-rank concepts;
#   7. profiles genus concepts by family, major lineage and provisional Star;
#   8. flags genus concepts for later diagnostic/recovery work;
#   9. writes audit tables and CSV outputs.
#
# IMPORTANT
# ---------
# This is an AUDIT module.
#
# It does NOT:
#   * delete taxa;
#   * alter the canonical population;
#   * alter WCVP reconciliation;
#   * resolve genus concepts;
#   * change provisional Star allocations;
#   * change taxonomic names or ranks.
#
# Expected canonical population:
#   11,439 accepted concepts
#
# Previous 05a profile suggested:
#   Species              8,906
#   Subspecies             499
#   Varieties            1,147
#   Forms                   62
#   Other infraspecific      0
#   Genus concepts          823
#   Other ranks               2
#
# These historical expectations are used for diagnostic comparison only.
# The current canonical data remain authoritative.
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
  "tibble"
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


# ==============================================================================
# 01. CONFIGURATION
# ==============================================================================

MODULE <- "02c_audit_taxonomic_ranks"
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
  "02c_taxonomic_ranks"
)

dir.create(
  OUTPUT_DIR,
  recursive = TRUE,
  showWarnings = FALSE
)

EXPECTED_POPULATION <- 11439L
EXPECTED_FAMILIES <- 260L

# Previous 05a values — diagnostic only
EXPECTED_SPECIES_05A <- 8906L
EXPECTED_SUBSPECIES_05A <- 499L
EXPECTED_VARIETIES_05A <- 1147L
EXPECTED_FORMS_05A <- 62L
EXPECTED_GENUS_CONCEPTS_05A <- 823L
EXPECTED_OTHER_RANKS_05A <- 2L


# ==============================================================================
# 02. TABLE NAMES
# ==============================================================================

TABLE_INPUT <-
  "vpjd_star_provisional_taxonomic_composition"

TABLE_02B <-
  "vpjd_taxrev_02b_taxonomic_scope_audit"

TABLE_02B_VALIDATION <-
  "vpjd_taxrev_02b_validation"

TABLE_AUDIT <-
  "vpjd_taxrev_02c_rank_audit"

TABLE_RANK_PROFILE <-
  "vpjd_taxrev_02c_rank_profile"

TABLE_GENUS_CONCEPTS <-
  "vpjd_taxrev_02c_genus_concepts"

TABLE_GENUS_FAMILY_PROFILE <-
  "vpjd_taxrev_02c_genus_family_profile"

TABLE_OTHER_RANKS <-
  "vpjd_taxrev_02c_other_rank_concepts"

TABLE_VALIDATION <-
  "vpjd_taxrev_02c_validation"


# ==============================================================================
# 03. CONNECTION SAFETY
# ==============================================================================

if (exists("con", envir = .GlobalEnv)) {
  stop(
    paste0(
      "An object named 'con' already exists in this R session.\n\n",
      "Restart R before running 02c:\n",
      "Session > Restart R\n",
      "or Ctrl + Shift + F10\n\n",
      "Then run 02c from the beginning."
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
cat("VPJD TAXONOMIC REVISION — 02c TAXONOMIC RANK AUDIT\n")
cat("============================================================\n\n")

cat("Run date: ", RUN_DATE, "\n", sep = "")
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
# 06. VERIFY REQUIRED TABLES
# ==============================================================================

db_tables <- DBI::dbListTables(con)

required_tables <- c(
  TABLE_INPUT,
  TABLE_02B,
  TABLE_02B_VALIDATION
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
# 07. VERIFY 02b VALIDATION
# ==============================================================================

validation_02b <- DBI::dbReadTable(
  con,
  TABLE_02B_VALIDATION
) %>%
  as_tibble()

if (!all(validation_02b$PASS %in% TRUE)) {
  
  failed_02b <- validation_02b %>%
    filter(!PASS) %>%
    pull(CHECK)
  
  DBI::dbDisconnect(
    con,
    shutdown = TRUE
  )
  
  rm(con)
  
  stop(
    paste0(
      "02b validation is not fully PASS.\n\n",
      "Failed checks:\n",
      paste(
        failed_02b,
        collapse = "\n"
      )
    )
  )
}

cat(
  "02b validation: ",
  sum(validation_02b$PASS),
  "/",
  nrow(validation_02b),
  " PASS\n\n",
  sep = ""
)


# ==============================================================================
# 08. READ CANONICAL TAXONOMY
# ==============================================================================

taxonomy <- DBI::dbReadTable(
  con,
  TABLE_INPUT
) %>%
  as_tibble()

if (nrow(taxonomy) != EXPECTED_POPULATION) {
  
  DBI::dbDisconnect(
    con,
    shutdown = TRUE
  )
  
  rm(con)
  
  stop(
    "Expected ",
    format(
      EXPECTED_POPULATION,
      big.mark = ","
    ),
    " canonical concepts; found ",
    format(
      nrow(taxonomy),
      big.mark = ","
    ),
    "."
  )
}


# ==============================================================================
# 09. READ 02b SCOPE AUDIT
# ==============================================================================

scope_02b <- DBI::dbReadTable(
  con,
  TABLE_02B
) %>%
  as_tibble()

if (nrow(scope_02b) != EXPECTED_POPULATION) {
  
  DBI::dbDisconnect(
    con,
    shutdown = TRUE
  )
  
  rm(con)
  
  stop(
    "02b scope audit does not contain 11,439 concepts."
  )
}


# ==============================================================================
# 10. CHECK REQUIRED FIELDS
# ==============================================================================

required_taxonomy_fields <- c(
  "FINAL_WCVP_ID",
  "FINAL_WCVP_RECOGNISED_NAME",
  "FINAL_WCVP_RANK",
  "FINAL_WCVP_STATUS",
  "wcvp_taxon_name",
  "wcvp_taxon_rank",
  "wcvp_family",
  "wcvp_genus",
  "wcvp_species",
  "PROVISIONAL_STAR"
)

missing_taxonomy_fields <- setdiff(
  required_taxonomy_fields,
  names(taxonomy)
)

if (length(missing_taxonomy_fields) > 0L) {
  
  DBI::dbDisconnect(
    con,
    shutdown = TRUE
  )
  
  rm(con)
  
  stop(
    "Required taxonomy fields missing:\n",
    paste(
      missing_taxonomy_fields,
      collapse = ", "
    )
  )
}

required_scope_fields <- c(
  "FINAL_WCVP_ID",
  "MAJOR_GROUP",
  "SCOPE_STATUS"
)

missing_scope_fields <- setdiff(
  required_scope_fields,
  names(scope_02b)
)

if (length(missing_scope_fields) > 0L) {
  
  DBI::dbDisconnect(
    con,
    shutdown = TRUE
  )
  
  rm(con)
  
  stop(
    "Required 02b scope fields missing:\n",
    paste(
      missing_scope_fields,
      collapse = ", "
    )
  )
}


# ==============================================================================
# 11. STANDARDISE IDs BEFORE JOINING
# ==============================================================================

taxonomy <- taxonomy %>%
  mutate(
    FINAL_WCVP_ID =
      as.character(FINAL_WCVP_ID)
  )

scope_02b <- scope_02b %>%
  mutate(
    FINAL_WCVP_ID =
      as.character(FINAL_WCVP_ID)
  )


# ==============================================================================
# 12. PREPARE 02b HAND-OFF
#
# Only retain fields actually needed by 02c to avoid duplicate taxonomy columns.
# ==============================================================================

scope_lookup <- scope_02b %>%
  select(
    FINAL_WCVP_ID,
    MAJOR_GROUP,
    SCOPE_STATUS
  ) %>%
  distinct(
    FINAL_WCVP_ID,
    .keep_all = TRUE
  )

if (
  nrow(scope_lookup) !=
  EXPECTED_POPULATION
) {
  
  DBI::dbDisconnect(
    con,
    shutdown = TRUE
  )
  
  rm(con)
  
  stop(
    "02b scope lookup is not unique by FINAL_WCVP_ID."
  )
}


# ==============================================================================
# 13. BUILD 02c AUDIT POPULATION
# ==============================================================================

audit <- taxonomy %>%
  
  left_join(
    scope_lookup,
    by = "FINAL_WCVP_ID"
  ) %>%
  
  mutate(
    
    TAXON_NAME =
      coalesce(
        as.character(wcvp_taxon_name),
        as.character(
          FINAL_WCVP_RECOGNISED_NAME
        )
      ),
    
    TAXON_RANK_RAW =
      coalesce(
        as.character(wcvp_taxon_rank),
        as.character(FINAL_WCVP_RANK),
        "MISSING"
      ),
    
    TAXON_RANK_NORMALISED =
      toupper(
        trimws(
          TAXON_RANK_RAW
        )
      ),
    
    FAMILY =
      toupper(
        trimws(
          coalesce(
            as.character(wcvp_family),
            ""
          )
        )
      ),
    
    GENUS =
      trimws(
        coalesce(
          as.character(wcvp_genus),
          ""
        )
      ),
    
    SPECIES =
      trimws(
        coalesce(
          as.character(wcvp_species),
          ""
        )
      )
  )


# ==============================================================================
# 14. NORMALISE RANK GROUPS
# ==============================================================================

audit <- audit %>%
  
  mutate(
    
    RANK_GROUP = case_when(
      
      TAXON_RANK_NORMALISED ==
        "SPECIES" ~
        "SPECIES",
      
      TAXON_RANK_NORMALISED %in%
        c(
          "SUBSPECIES",
          "SUBSP.",
          "SUBSP"
        ) ~
        "SUBSPECIES",
      
      TAXON_RANK_NORMALISED %in%
        c(
          "VARIETY",
          "VAR.",
          "VAR"
        ) ~
        "VARIETY",
      
      TAXON_RANK_NORMALISED %in%
        c(
          "FORM",
          "FORMA",
          "F.",
          "F"
        ) ~
        "FORM",
      
      TAXON_RANK_NORMALISED ==
        "GENUS" ~
        "GENUS",
      
      TAXON_RANK_NORMALISED %in%
        c(
          "SUBVARIETY",
          "SUBVAR.",
          "SUBVAR",
          "SUBFORM",
          "SUBFORMA"
        ) ~
        "OTHER_INFRASPECIFIC",
      
      TRUE ~
        "OTHER_RANK"
    )
  )


# ==============================================================================
# 15. COMPLETE RANK PROFILE
# ==============================================================================

rank_profile <- audit %>%
  
  count(
    TAXON_RANK_RAW,
    TAXON_RANK_NORMALISED,
    RANK_GROUP,
    name = "N_TAXA"
  ) %>%
  
  mutate(
    
    PERCENT_ACCEPTED =
      round(
        100 *
          N_TAXA /
          EXPECTED_POPULATION,
        3
      )
  ) %>%
  
  arrange(
    desc(N_TAXA),
    TAXON_RANK_NORMALISED
  )


# ==============================================================================
# 16. NORMALISED RANK SUMMARY
# ==============================================================================

rank_group_summary <- audit %>%
  
  count(
    RANK_GROUP,
    name = "N_TAXA"
  ) %>%
  
  mutate(
    
    PERCENT_ACCEPTED =
      round(
        100 *
          N_TAXA /
          EXPECTED_POPULATION,
        3
      )
  ) %>%
  
  arrange(
    desc(N_TAXA)
  )


# ==============================================================================
# 17. GENUS CONCEPT INVENTORY
# ==============================================================================

genus_concepts <- audit %>%
  
  filter(
    RANK_GROUP == "GENUS"
  ) %>%
  
  mutate(
    
    GENUS_DIAGNOSTIC_CLASS =
      case_when(
        
        GENUS == "" ~
          "GENUS_RANK_WITHOUT_GENUS_FIELD",
        
        TAXON_NAME == "" |
          is.na(TAXON_NAME) ~
          "GENUS_RANK_WITHOUT_TAXON_NAME",
        
        TRUE ~
          "GENUS_RANK_REQUIRES_SOURCE_REVIEW"
      ),
    
    REQUIRES_02D_REVIEW =
      TRUE
  ) %>%
  
  select(
    FINAL_WCVP_ID,
    TAXON_NAME,
    TAXON_RANK_RAW,
    TAXON_RANK_NORMALISED,
    FAMILY,
    GENUS,
    SPECIES,
    MAJOR_GROUP,
    SCOPE_STATUS,
    PROVISIONAL_STAR,
    GENUS_DIAGNOSTIC_CLASS,
    REQUIRES_02D_REVIEW,
    everything()
  )


# ==============================================================================
# 18. GENUS CONCEPT PROFILE BY FAMILY
# ==============================================================================

genus_family_profile <- genus_concepts %>%
  
  group_by(
    MAJOR_GROUP,
    FAMILY
  ) %>%
  
  summarise(
    
    N_GENUS_CONCEPTS =
      n(),
    
    N_DISTINCT_GENERA =
      n_distinct(
        GENUS[
          !is.na(GENUS) &
            GENUS != ""
        ]
      ),
    
    .groups = "drop"
  ) %>%
  
  arrange(
    desc(N_GENUS_CONCEPTS),
    FAMILY
  )


# ==============================================================================
# 19. GENUS CONCEPT PROFILE BY MAJOR LINEAGE
# ==============================================================================

genus_lineage_profile <- genus_concepts %>%
  
  count(
    MAJOR_GROUP,
    name = "N_GENUS_CONCEPTS"
  ) %>%
  
  mutate(
    
    PERCENT_GENUS_CONCEPTS =
      round(
        100 *
          N_GENUS_CONCEPTS /
          nrow(genus_concepts),
        2
      )
  ) %>%
  
  arrange(
    desc(N_GENUS_CONCEPTS)
  )


# ==============================================================================
# 20. GENUS CONCEPT PROFILE BY STAR
# ==============================================================================

genus_star_profile <- genus_concepts %>%
  
  mutate(
    
    STAR_DISPLAY =
      coalesce(
        as.character(PROVISIONAL_STAR),
        "UNRESOLVED"
      )
  ) %>%
  
  count(
    STAR_DISPLAY,
    name = "N_GENUS_CONCEPTS"
  ) %>%
  
  mutate(
    
    PERCENT_GENUS_CONCEPTS =
      round(
        100 *
          N_GENUS_CONCEPTS /
          nrow(genus_concepts),
        2
      )
  ) %>%
  
  arrange(
    desc(N_GENUS_CONCEPTS),
    STAR_DISPLAY
  )


# ==============================================================================
# 21. OTHER-RANK INVENTORY
# ==============================================================================

other_rank_concepts <- audit %>%
  
  filter(
    RANK_GROUP %in%
      c(
        "OTHER_RANK",
        "OTHER_INFRASPECIFIC"
      )
  ) %>%
  
  mutate(
    REQUIRES_MANUAL_REVIEW =
      TRUE
  ) %>%
  
  select(
    FINAL_WCVP_ID,
    TAXON_NAME,
    TAXON_RANK_RAW,
    TAXON_RANK_NORMALISED,
    RANK_GROUP,
    FAMILY,
    GENUS,
    SPECIES,
    MAJOR_GROUP,
    PROVISIONAL_STAR,
    REQUIRES_MANUAL_REVIEW,
    everything()
  )


# ==============================================================================
# 22. RANK PROFILE BY MAJOR LINEAGE
# ==============================================================================

rank_lineage_profile <- audit %>%
  
  count(
    MAJOR_GROUP,
    RANK_GROUP,
    name = "N_TAXA"
  ) %>%
  
  arrange(
    MAJOR_GROUP,
    desc(N_TAXA)
  )


# ==============================================================================
# 23. RANK PROFILE BY PROVISIONAL STAR
# ==============================================================================

rank_star_profile <- audit %>%
  
  mutate(
    
    STAR_DISPLAY =
      coalesce(
        as.character(PROVISIONAL_STAR),
        "UNRESOLVED"
      )
  ) %>%
  
  count(
    STAR_DISPLAY,
    RANK_GROUP,
    name = "N_TAXA"
  ) %>%
  
  arrange(
    STAR_DISPLAY,
    desc(N_TAXA)
  )


# ==============================================================================
# 24. CORE COUNTS
# ==============================================================================

n_species <- sum(
  audit$RANK_GROUP == "SPECIES",
  na.rm = TRUE
)

n_subspecies <- sum(
  audit$RANK_GROUP == "SUBSPECIES",
  na.rm = TRUE
)

n_varieties <- sum(
  audit$RANK_GROUP == "VARIETY",
  na.rm = TRUE
)

n_forms <- sum(
  audit$RANK_GROUP == "FORM",
  na.rm = TRUE
)

n_other_infraspecific <- sum(
  audit$RANK_GROUP ==
    "OTHER_INFRASPECIFIC",
  na.rm = TRUE
)

n_genus_concepts <- sum(
  audit$RANK_GROUP == "GENUS",
  na.rm = TRUE
)

n_other_ranks <- sum(
  audit$RANK_GROUP == "OTHER_RANK",
  na.rm = TRUE
)

n_missing_scope <- sum(
  is.na(audit$MAJOR_GROUP) |
    audit$MAJOR_GROUP == ""
)

n_missing_family <- sum(
  is.na(audit$FAMILY) |
    audit$FAMILY == ""
)


# ==============================================================================
# 25. DIAGNOSTIC COMPARISON WITH 05a
#
# These are NOT hard validation requirements.
# If they differ, we want to see the difference rather than abort the module.
# ==============================================================================

comparison_05a <- tibble(
  
  RANK_GROUP = c(
    "SPECIES",
    "SUBSPECIES",
    "VARIETY",
    "FORM",
    "GENUS",
    "OTHER_RANK"
  ),
  
  PREVIOUS_05A = c(
    EXPECTED_SPECIES_05A,
    EXPECTED_SUBSPECIES_05A,
    EXPECTED_VARIETIES_05A,
    EXPECTED_FORMS_05A,
    EXPECTED_GENUS_CONCEPTS_05A,
    EXPECTED_OTHER_RANKS_05A
  ),
  
  CURRENT_02C = c(
    n_species,
    n_subspecies,
    n_varieties,
    n_forms,
    n_genus_concepts,
    n_other_ranks
  )
) %>%
  
  mutate(
    
    DIFFERENCE =
      CURRENT_02C -
      PREVIOUS_05A,
    
    MATCH =
      CURRENT_02C ==
      PREVIOUS_05A
  )


# ==============================================================================
# 26. VALIDATION
# ==============================================================================

valid_rank_groups <- c(
  "SPECIES",
  "SUBSPECIES",
  "VARIETY",
  "FORM",
  "OTHER_INFRASPECIFIC",
  "GENUS",
  "OTHER_RANK"
)

star_values_unchanged <- all(
  (
    audit$PROVISIONAL_STAR ==
      taxonomy$PROVISIONAL_STAR
  ) |
    (
      is.na(audit$PROVISIONAL_STAR) &
        is.na(taxonomy$PROVISIONAL_STAR)
    ),
  na.rm = TRUE
)

validation <- tibble(
  
  CHECK = c(
    
    "Canonical taxonomy table present",
    "02b scope audit table present",
    "02b validation table present",
    "02b validation fully PASS",
    "Canonical population = 11,439",
    "02c audit population = 11,439",
    "Population unique by WCVP ID",
    "All taxa retain 02b major-group classification",
    "All taxa have family",
    "All concepts assigned a normalised rank group",
    "Rank groups restricted to recognised audit categories",
    "Rank profile totals 11,439",
    "Rank-group summary totals 11,439",
    "Rank-lineage profile totals 11,439",
    "Rank-Star profile totals 11,439",
    "Genus inventory equals genus-rank count",
    "Other-rank inventory equals anomalous-rank count",
    "No canonical taxa removed",
    "Provisional Star allocations unchanged"
  ),
  
  PASS = c(
    
    TABLE_INPUT %in% db_tables,
    
    TABLE_02B %in% db_tables,
    
    TABLE_02B_VALIDATION %in%
      db_tables,
    
    all(
      validation_02b$PASS %in%
        TRUE
    ),
    
    nrow(taxonomy) ==
      EXPECTED_POPULATION,
    
    nrow(audit) ==
      EXPECTED_POPULATION,
    
    n_distinct(
      audit$FINAL_WCVP_ID
    ) ==
      EXPECTED_POPULATION,
    
    n_missing_scope ==
      0L,
    
    n_missing_family ==
      0L,
    
    all(
      !is.na(audit$RANK_GROUP) &
        audit$RANK_GROUP != ""
    ),
    
    all(
      audit$RANK_GROUP %in%
        valid_rank_groups
    ),
    
    sum(rank_profile$N_TAXA) ==
      EXPECTED_POPULATION,
    
    sum(rank_group_summary$N_TAXA) ==
      EXPECTED_POPULATION,
    
    sum(rank_lineage_profile$N_TAXA) ==
      EXPECTED_POPULATION,
    
    sum(rank_star_profile$N_TAXA) ==
      EXPECTED_POPULATION,
    
    nrow(genus_concepts) ==
      n_genus_concepts,
    
    nrow(other_rank_concepts) ==
      (
        n_other_ranks +
          n_other_infraspecific
      ),
    
    nrow(audit) ==
      nrow(taxonomy),
    
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
# 27. CONSOLE REPORT
# ==============================================================================

cat("\n")
cat("============================================================\n")
cat("CONTEMPORARY VPJD TAXONOMIC RANK PROFILE\n")
cat("============================================================\n\n")

print(
  rank_group_summary,
  n = Inf
)

cat("\n")
cat("— EXACT WCVP RANK PROFILE —\n\n")

print(
  rank_profile,
  n = Inf
)

cat("\n")
cat("— COMPARISON WITH PREVIOUS 05a PROFILE —\n\n")

print(
  comparison_05a,
  n = Inf
)

cat("\n")
cat("— GENUS CONCEPTS BY MAJOR LINEAGE —\n\n")

print(
  genus_lineage_profile,
  n = Inf
)

cat("\n")
cat("— GENUS CONCEPTS BY PROVISIONAL STAR —\n\n")

print(
  genus_star_profile,
  n = Inf
)

cat("\n")
cat("— TOP FAMILIES CONTAINING GENUS CONCEPTS —\n\n")

print(
  head(
    genus_family_profile,
    30
  ),
  n = 30
)

cat("\n")
cat("— OTHER / ANOMALOUS RANK CONCEPTS —\n\n")

if (nrow(other_rank_concepts) == 0L) {
  
  cat(
    "No other-rank concepts identified.\n"
  )
  
} else {
  
  print(
    other_rank_concepts %>%
      select(
        FINAL_WCVP_ID,
        TAXON_NAME,
        TAXON_RANK_RAW,
        RANK_GROUP,
        FAMILY,
        GENUS,
        MAJOR_GROUP,
        PROVISIONAL_STAR
      ),
    n = Inf
  )
}

cat("\n")
cat("— VALIDATION —\n\n")

print(
  validation,
  n = Inf
)


# ==============================================================================
# 28. STOP ON STRUCTURAL VALIDATION FAILURE
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
      "\n02c structural validation failed.\n",
      "No 02c DuckDB outputs have been written."
    )
  )
}


# ==============================================================================
# 29. WRITE DUCKDB OUTPUTS
# ==============================================================================

DBI::dbWriteTable(
  con,
  TABLE_AUDIT,
  audit,
  overwrite = TRUE
)

DBI::dbWriteTable(
  con,
  TABLE_RANK_PROFILE,
  rank_profile,
  overwrite = TRUE
)

DBI::dbWriteTable(
  con,
  TABLE_GENUS_CONCEPTS,
  genus_concepts,
  overwrite = TRUE
)

DBI::dbWriteTable(
  con,
  TABLE_GENUS_FAMILY_PROFILE,
  genus_family_profile,
  overwrite = TRUE
)

DBI::dbWriteTable(
  con,
  TABLE_OTHER_RANKS,
  other_rank_concepts,
  overwrite = TRUE
)

DBI::dbWriteTable(
  con,
  TABLE_VALIDATION,
  validation,
  overwrite = TRUE
)


# ==============================================================================
# 30. WRITE CSV OUTPUTS
# ==============================================================================

readr::write_csv(
  audit,
  file.path(
    OUTPUT_DIR,
    "VPJD_02c_taxonomic_rank_audit.csv"
  )
)

readr::write_csv(
  rank_profile,
  file.path(
    OUTPUT_DIR,
    "VPJD_02c_exact_rank_profile.csv"
  )
)

readr::write_csv(
  rank_group_summary,
  file.path(
    OUTPUT_DIR,
    "VPJD_02c_rank_group_summary.csv"
  )
)

readr::write_csv(
  comparison_05a,
  file.path(
    OUTPUT_DIR,
    "VPJD_02c_comparison_with_05a.csv"
  )
)

readr::write_csv(
  genus_concepts,
  file.path(
    OUTPUT_DIR,
    "VPJD_02c_genus_concepts.csv"
  )
)

readr::write_csv(
  genus_family_profile,
  file.path(
    OUTPUT_DIR,
    "VPJD_02c_genus_family_profile.csv"
  )
)

readr::write_csv(
  genus_lineage_profile,
  file.path(
    OUTPUT_DIR,
    "VPJD_02c_genus_lineage_profile.csv"
  )
)

readr::write_csv(
  genus_star_profile,
  file.path(
    OUTPUT_DIR,
    "VPJD_02c_genus_star_profile.csv"
  )
)

readr::write_csv(
  other_rank_concepts,
  file.path(
    OUTPUT_DIR,
    "VPJD_02c_other_rank_concepts.csv"
  )
)

readr::write_csv(
  rank_lineage_profile,
  file.path(
    OUTPUT_DIR,
    "VPJD_02c_rank_by_lineage.csv"
  )
)

readr::write_csv(
  rank_star_profile,
  file.path(
    OUTPUT_DIR,
    "VPJD_02c_rank_by_star.csv"
  )
)

readr::write_csv(
  validation,
  file.path(
    OUTPUT_DIR,
    "VPJD_02c_validation.csv"
  )
)


# ==============================================================================
# 31. RESULT OBJECT
# ==============================================================================

taxrev_02c_result <- list(
  
  audit =
    audit,
  
  rank_profile =
    rank_profile,
  
  rank_group_summary =
    rank_group_summary,
  
  comparison_05a =
    comparison_05a,
  
  genus_concepts =
    genus_concepts,
  
  genus_family_profile =
    genus_family_profile,
  
  genus_lineage_profile =
    genus_lineage_profile,
  
  genus_star_profile =
    genus_star_profile,
  
  other_rank_concepts =
    other_rank_concepts,
  
  rank_lineage_profile =
    rank_lineage_profile,
  
  rank_star_profile =
    rank_star_profile,
  
  validation =
    validation
)


# ==============================================================================
# 32. COMPLETION REPORT
# ==============================================================================

cat("\n")
cat("============================================================\n")

cat(
  "VPJD TAXONOMIC REVISION 02c v",
  VERSION,
  " COMPLETE\n",
  sep = ""
)

cat("============================================================\n")

cat(
  "Accepted concepts audited: ",
  format(
    nrow(audit),
    big.mark = ","
  ),
  "\n",
  sep = ""
)

cat(
  "Species: ",
  format(
    n_species,
    big.mark = ","
  ),
  "\n",
  sep = ""
)

cat(
  "Subspecies: ",
  format(
    n_subspecies,
    big.mark = ","
  ),
  "\n",
  sep = ""
)

cat(
  "Varieties: ",
  format(
    n_varieties,
    big.mark = ","
  ),
  "\n",
  sep = ""
)

cat(
  "Forms: ",
  format(
    n_forms,
    big.mark = ","
  ),
  "\n",
  sep = ""
)

cat(
  "Other infraspecific: ",
  format(
    n_other_infraspecific,
    big.mark = ","
  ),
  "\n",
  sep = ""
)

cat(
  "Genus concepts: ",
  format(
    n_genus_concepts,
    big.mark = ","
  ),
  "\n",
  sep = ""
)

cat(
  "Other ranks: ",
  format(
    n_other_ranks,
    big.mark = ","
  ),
  "\n",
  sep = ""
)

cat(
  "Genus concepts requiring 02d review: ",
  format(
    nrow(genus_concepts),
    big.mark = ","
  ),
  "\n",
  sep = ""
)

cat(
  "Other-rank concepts requiring review: ",
  format(
    nrow(other_rank_concepts),
    big.mark = ","
  ),
  "\n",
  sep = ""
)

cat("Canonical taxa removed: 0\n")
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
  "Canonical audit: ",
  TABLE_AUDIT,
  "\n",
  sep = ""
)

cat(
  "Genus inventory: ",
  TABLE_GENUS_CONCEPTS,
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
# 33. CLEAN DISCONNECT
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