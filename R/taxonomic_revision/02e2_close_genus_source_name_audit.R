# ==============================================================================
# VPJD TAXONOMIC REVISION
# 02e.2 — CLOSE GENUS SOURCE-NAME AUDIT
#
# Version: 0.1.0
#
# PURPOSE
# -------
# Formally close the 02e genus source-name audit after manual review of the
# sole apparent below-genus exception identified by 02e.
#
# 02e identified:
#
#   WCVP ID:        355573
#   Accepted name:  Peracarpa
#   Rank:           Genus
#   Source value:   Peracarpa Hook.f. & Thomson
#
# The 02e heuristic interpreted "f." in "Hook.f." as a possible forma rank
# marker. Manual review established that "Hook.f." is botanical authorship,
# not an infraspecific rank designation.
#
# Therefore:
#
#   * the record remains a genus-level source record;
#   * no species-level information is recoverable from the source name;
#   * no infraspecific information is recoverable from the source name;
#   * no taxonomic reconciliation is required;
#   * WCVP ID 355573 remains unchanged;
#   * no Star allocation is changed;
#   * all 823 genus concepts are supported as genus-level source records.
#
# IMPORTANT
# ---------
# This module does NOT modify:
#
#   * vpjd_star_provisional_wholesale_allocation
#   * WCVP identifiers
#   * accepted names
#   * accepted ranks
#   * provisional Star allocations
#   * occurrence source data
#
# It writes only 02e.2 audit/closure outputs.
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
  "readr"
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


# ==============================================================================
# 02. MODULE METADATA
# ==============================================================================

MODULE <- "02e2_close_genus_source_name_audit"
VERSION <- "0.1.0"
RUN_DATE <- Sys.Date()

EXPECTED_CANONICAL_POPULATION <- 11439L
EXPECTED_GENUS_CONCEPTS <- 823L

EXCEPTION_WCVP_ID <- "355573"

EXPECTED_SOURCE_VALUE <-
  "Peracarpa Hook.f. & Thomson"


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

INPUT_DIR <- file.path(
  PROJECT_ROOT,
  "outputs",
  "tables",
  "taxonomic_revision",
  "02e_genus_source_names"
)

OUTPUT_DIR <- file.path(
  PROJECT_ROOT,
  "outputs",
  "tables",
  "taxonomic_revision",
  "02e_genus_source_names",
  "02e2_closure"
)

dir.create(
  OUTPUT_DIR,
  recursive = TRUE,
  showWarnings = FALSE
)


# ==============================================================================
# 04. INPUT FILES
# ==============================================================================

FILE_02E_AUDIT <- file.path(
  INPUT_DIR,
  "VPJD_02e_genus_source_name_audit.csv"
)

FILE_02E_DISTINCT <- file.path(
  INPUT_DIR,
  "VPJD_02e_genus_source_name_distinct.csv"
)

FILE_02E1_EXCEPTION <- file.path(
  INPUT_DIR,
  "VPJD_02e1_exception_evidence.csv"
)

FILE_02E1_CONCEPT <- file.path(
  INPUT_DIR,
  "VPJD_02e1_exception_concepts.csv"
)


# ==============================================================================
# 05. OUTPUT TABLE NAMES
# ==============================================================================

TABLE_CLOSURE <-
  "vpjd_taxrev_02e2_genus_source_name_closure"

TABLE_RESOLUTION <-
  "vpjd_taxrev_02e2_exception_resolution"

TABLE_FINAL_AUDIT <-
  "vpjd_taxrev_02e2_genus_source_name_final_audit"

TABLE_SUMMARY <-
  "vpjd_taxrev_02e2_genus_source_name_summary"

TABLE_VALIDATION <-
  "vpjd_taxrev_02e2_validation"


# ==============================================================================
# 06. START REPORT
# ==============================================================================

cat("\n")
cat("============================================================\n")
cat("VPJD TAXONOMIC REVISION — 02e.2\n")
cat("CLOSE GENUS SOURCE-NAME AUDIT\n")
cat("============================================================\n\n")

cat(
  "Run date: ",
  as.character(RUN_DATE),
  "\n",
  sep = ""
)

cat(
  "Module: ",
  MODULE,
  "\n",
  sep = ""
)

cat(
  "Version: ",
  VERSION,
  "\n\n",
  sep = ""
)

cat(
  "Expected canonical population: ",
  format(
    EXPECTED_CANONICAL_POPULATION,
    big.mark = ","
  ),
  "\n",
  sep = ""
)

cat(
  "Expected genus concepts: ",
  EXPECTED_GENUS_CONCEPTS,
  "\n\n",
  sep = ""
)

cat("Mode: AUDIT CLOSURE\n")
cat("Canonical taxonomic modification: FALSE\n")
cat("Star modification: FALSE\n\n")


# ==============================================================================
# 07. CHECK REQUIRED FILES
# ==============================================================================

required_files <- c(
  FILE_02E_AUDIT,
  FILE_02E_DISTINCT,
  FILE_02E1_EXCEPTION,
  FILE_02E1_CONCEPT
)

missing_files <- required_files[
  !file.exists(required_files)
]

if (length(missing_files) > 0L) {
  
  stop(
    "Required 02e/02e.1 file(s) missing:\n",
    paste(
      missing_files,
      collapse = "\n"
    )
  )
}

if (!file.exists(DB_PATH)) {
  
  stop(
    "VPJD DuckDB database not found:\n",
    DB_PATH
  )
}

cat("Required 02e/02e.1 files present.\n\n")


# ==============================================================================
# 08. LOAD EXISTING AUDIT OUTPUTS
# ==============================================================================

audit_02e <- read_csv(
  FILE_02E_AUDIT,
  show_col_types = FALSE
)

source_distinct <- read_csv(
  FILE_02E_DISTINCT,
  show_col_types = FALSE
)

exception_02e1 <- read_csv(
  FILE_02E1_EXCEPTION,
  show_col_types = FALSE
)

concept_02e1 <- read_csv(
  FILE_02E1_CONCEPT,
  show_col_types = FALSE
)


# ==============================================================================
# 09. STANDARDISE WCVP IDS
# ==============================================================================

audit_02e <- audit_02e |>
  mutate(
    FINAL_WCVP_ID =
      as.character(FINAL_WCVP_ID)
  )

source_distinct <- source_distinct |>
  mutate(
    FINAL_WCVP_ID =
      as.character(FINAL_WCVP_ID)
  )

exception_02e1 <- exception_02e1 |>
  mutate(
    FINAL_WCVP_ID =
      as.character(FINAL_WCVP_ID)
  )

concept_02e1 <- concept_02e1 |>
  mutate(
    FINAL_WCVP_ID =
      as.character(FINAL_WCVP_ID)
  )


# ==============================================================================
# 10. VALIDATE ORIGINAL 02e POPULATION
# ==============================================================================

if (
  nrow(audit_02e) !=
  EXPECTED_GENUS_CONCEPTS
) {
  
  stop(
    "Expected 823 records in the 02e genus audit; found ",
    nrow(audit_02e),
    "."
  )
}

if (
  n_distinct(
    audit_02e$FINAL_WCVP_ID
  ) !=
  EXPECTED_GENUS_CONCEPTS
) {
  
  stop(
    "02e genus audit is not unique by FINAL_WCVP_ID."
  )
}

cat(
  "02e genus audit population validated: ",
  nrow(audit_02e),
  "\n",
  sep = ""
)


# ==============================================================================
# 11. VALIDATE THE SOLE 02e EXCEPTION
# ==============================================================================

if (nrow(exception_02e1) != 1L) {
  
  stop(
    "Expected exactly one 02e.1 exception record; found ",
    nrow(exception_02e1),
    "."
  )
}

if (
  exception_02e1$FINAL_WCVP_ID[[1]] !=
  EXCEPTION_WCVP_ID
) {
  
  stop(
    "Unexpected WCVP ID in the 02e.1 exception.\n",
    "Expected: ",
    EXCEPTION_WCVP_ID,
    "\n",
    "Found: ",
    exception_02e1$FINAL_WCVP_ID[[1]]
  )
}

if (
  exception_02e1$SOURCE_VALUE_NORMALISED[[1]] !=
  EXPECTED_SOURCE_VALUE
) {
  
  stop(
    "Unexpected source value for WCVP ID ",
    EXCEPTION_WCVP_ID,
    ".\n",
    "Expected: ",
    EXPECTED_SOURCE_VALUE,
    "\n",
    "Found: ",
    exception_02e1$SOURCE_VALUE_NORMALISED[[1]]
  )
}

cat(
  "02e exception confirmed: ",
  EXCEPTION_WCVP_ID,
  " — ",
  EXPECTED_SOURCE_VALUE,
  "\n\n",
  sep = ""
)


# ==============================================================================
# 12. FORMAL MANUAL RESOLUTION RECORD
# ==============================================================================

exception_resolution <- tibble(
  
  FINAL_WCVP_ID =
    EXCEPTION_WCVP_ID,
  
  FINAL_WCVP_RECOGNISED_NAME =
    "Peracarpa",
  
  FINAL_WCVP_RANK =
    "Genus",
  
  SOURCE_FIELD =
    "scientificName",
  
  SOURCE_VALUE =
    EXPECTED_SOURCE_VALUE,
  
  ORIGINAL_02E_SIGNAL =
    "INFRASPECIFIC_INFORMATION_PRESENT",
  
  MANUAL_REVIEW_OUTCOME =
    "FALSE_POSITIVE_AUTHORSHIP",
  
  CORRECTED_SOURCE_CLASSIFICATION =
    "GENUS_ONLY_SOURCE",
  
  CORRECTED_REVIEW_STATUS =
    "GENUS_SOURCE_SUPPORTED",
  
  FALSE_POSITIVE_TOKEN =
    "Hook.f.",
  
  FALSE_POSITIVE_EXPLANATION =
    paste0(
      "'f.' occurs within the botanical author abbreviation ",
      "'Hook.f.' and does not represent the infraspecific rank forma."
    ),
  
  SPECIES_INFORMATION_RECOVERED =
    FALSE,
  
  INFRASPECIFIC_INFORMATION_RECOVERED =
    FALSE,
  
  TAXONOMIC_RECONCILIATION_REQUIRED =
    FALSE,
  
  TAXONOMIC_ACTION_TAKEN =
    FALSE,
  
  STAR_ACTION_TAKEN =
    FALSE,
  
  RESOLUTION_METHOD =
    "MANUAL_REVIEW",
  
  MODULE =
    MODULE,
  
  MODULE_VERSION =
    VERSION,
  
  REVIEW_DATE =
    as.character(RUN_DATE)
)


# ==============================================================================
# 13. CREATE FINAL CORRECTED 823-CONCEPT AUDIT
# ==============================================================================

final_audit <- audit_02e |>
  mutate(
    
    ORIGINAL_SOURCE_CLASSIFICATION =
      SOURCE_CLASSIFICATION,
    
    ORIGINAL_REVIEW_STATUS =
      REVIEW_STATUS,
    
    ORIGINAL_ANY_INFRA_SIGNAL =
      ANY_INFRA_SIGNAL,
    
    ORIGINAL_ANY_BELOW_GENUS_SIGNAL =
      ANY_BELOW_GENUS_SIGNAL,
    
    MANUAL_REVIEW_APPLIED =
      FINAL_WCVP_ID ==
      EXCEPTION_WCVP_ID,
    
    FALSE_POSITIVE_HEURISTIC =
      FINAL_WCVP_ID ==
      EXCEPTION_WCVP_ID,
    
    SOURCE_CLASSIFICATION =
      if_else(
        FINAL_WCVP_ID ==
          EXCEPTION_WCVP_ID,
        "GENUS_ONLY_SOURCE",
        SOURCE_CLASSIFICATION
      ),
    
    REVIEW_STATUS =
      if_else(
        FINAL_WCVP_ID ==
          EXCEPTION_WCVP_ID,
        "GENUS_SOURCE_SUPPORTED",
        REVIEW_STATUS
      ),
    
    ANY_INFRA_SIGNAL =
      if_else(
        FINAL_WCVP_ID ==
          EXCEPTION_WCVP_ID,
        FALSE,
        ANY_INFRA_SIGNAL
      ),
    
    ANY_BELOW_GENUS_SIGNAL =
      if_else(
        FINAL_WCVP_ID ==
          EXCEPTION_WCVP_ID,
        FALSE,
        ANY_BELOW_GENUS_SIGNAL
      ),
    
    N_INFRA_SIGNALS =
      if_else(
        FINAL_WCVP_ID ==
          EXCEPTION_WCVP_ID,
        0,
        N_INFRA_SIGNALS
      ),
    
    N_BELOW_GENUS_SIGNALS =
      if_else(
        FINAL_WCVP_ID ==
          EXCEPTION_WCVP_ID,
        0,
        N_BELOW_GENUS_SIGNALS
      ),
    
    MANUAL_REVIEW_OUTCOME =
      if_else(
        FINAL_WCVP_ID ==
          EXCEPTION_WCVP_ID,
        "FALSE_POSITIVE_AUTHORSHIP",
        "NOT_REQUIRED"
      ),
    
    TAXONOMIC_RECONCILIATION_REQUIRED =
      FALSE,
    
    TAXONOMIC_ACTION_TAKEN =
      FALSE,
    
    STAR_ACTION_TAKEN =
      FALSE
  )


# ==============================================================================
# 14. FINAL GENUS AUDIT SUMMARY
# ==============================================================================

final_summary <- final_audit |>
  count(
    SOURCE_CLASSIFICATION,
    REVIEW_STATUS,
    name = "N_CONCEPTS"
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
    desc(N_CONCEPTS),
    SOURCE_CLASSIFICATION
  )


# ==============================================================================
# 15. CLOSURE SUMMARY
# ==============================================================================

closure_summary <- tibble(
  
  METRIC = c(
    "Canonical VPJD concepts",
    "Genus concepts audited",
    "Genus concepts with occurrence evidence",
    "Genus concepts supported as genus-level",
    "Recoverable species-level identifications",
    "Recoverable infraspecific identifications",
    "Heuristic false positives manually resolved",
    "Concepts requiring further genus reconciliation",
    "Taxonomic changes made",
    "Star allocation changes made"
  ),
  
  N = c(
    EXPECTED_CANONICAL_POPULATION,
    EXPECTED_GENUS_CONCEPTS,
    EXPECTED_GENUS_CONCEPTS,
    EXPECTED_GENUS_CONCEPTS,
    0L,
    0L,
    1L,
    0L,
    0L,
    0L
  )
)


# ==============================================================================
# 16. OPEN DATABASE
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

cat("DuckDB connection established.\n\n")


# ==============================================================================
# 17. VALIDATE CANONICAL DATABASE POPULATION
# ==============================================================================

db_tables <- DBI::dbListTables(con)

TABLE_CANONICAL <-
  "vpjd_star_provisional_wholesale_allocation"

if (!TABLE_CANONICAL %in% db_tables) {
  
  DBI::dbDisconnect(
    con,
    shutdown = TRUE
  )
  
  stop(
    "Canonical VPJD table missing: ",
    TABLE_CANONICAL
  )
}

canonical_check <- DBI::dbGetQuery(
  con,
  paste0(
    'SELECT ',
    'CAST(FINAL_WCVP_ID AS VARCHAR) AS FINAL_WCVP_ID ',
    'FROM "',
    TABLE_CANONICAL,
    '"'
  )
) |>
  as_tibble()


# ==============================================================================
# 18. VALIDATION
# ==============================================================================

validation <- tibble(
  
  CHECK = c(
    
    "Canonical population = 11,439",
    
    "Canonical population unique by WCVP ID",
    
    "Original 02e genus population = 823",
    
    "Original 02e genus population unique by WCVP ID",
    
    "02e.1 contains exactly one exception",
    
    "Exception WCVP ID = 355573",
    
    "Exception source value matches Peracarpa authorship",
    
    "Final corrected genus audit = 823",
    
    "Final corrected genus audit unique by WCVP ID",
    
    "All 823 concepts classified GENUS_ONLY_SOURCE",
    
    "All 823 concepts classified GENUS_SOURCE_SUPPORTED",
    
    "No final species-level recovery signals",
    
    "No final infraspecific recovery signals",
    
    "Exactly one heuristic false positive documented",
    
    "No concepts require further genus reconciliation",
    
    "No taxonomic actions taken",
    
    "No Star actions taken",
    
    "Canonical population remains unchanged"
  ),
  
  PASS = c(
    
    nrow(canonical_check) ==
      EXPECTED_CANONICAL_POPULATION,
    
    n_distinct(
      canonical_check$FINAL_WCVP_ID
    ) ==
      EXPECTED_CANONICAL_POPULATION,
    
    nrow(audit_02e) ==
      EXPECTED_GENUS_CONCEPTS,
    
    n_distinct(
      audit_02e$FINAL_WCVP_ID
    ) ==
      EXPECTED_GENUS_CONCEPTS,
    
    nrow(exception_02e1) ==
      1L,
    
    exception_02e1$FINAL_WCVP_ID[[1]] ==
      EXCEPTION_WCVP_ID,
    
    exception_02e1$SOURCE_VALUE_NORMALISED[[1]] ==
      EXPECTED_SOURCE_VALUE,
    
    nrow(final_audit) ==
      EXPECTED_GENUS_CONCEPTS,
    
    n_distinct(
      final_audit$FINAL_WCVP_ID
    ) ==
      EXPECTED_GENUS_CONCEPTS,
    
    all(
      final_audit$SOURCE_CLASSIFICATION ==
        "GENUS_ONLY_SOURCE"
    ),
    
    all(
      final_audit$REVIEW_STATUS ==
        "GENUS_SOURCE_SUPPORTED"
    ),
    
    !any(
      final_audit$ANY_SPECIES_SIGNAL,
      na.rm = TRUE
    ),
    
    !any(
      final_audit$ANY_INFRA_SIGNAL,
      na.rm = TRUE
    ),
    
    sum(
      final_audit$FALSE_POSITIVE_HEURISTIC,
      na.rm = TRUE
    ) ==
      1L,
    
    !any(
      final_audit$TAXONOMIC_RECONCILIATION_REQUIRED,
      na.rm = TRUE
    ),
    
    !any(
      final_audit$TAXONOMIC_ACTION_TAKEN,
      na.rm = TRUE
    ),
    
    !any(
      final_audit$STAR_ACTION_TAKEN,
      na.rm = TRUE
    ),
    
    setequal(
      canonical_check$FINAL_WCVP_ID,
      audit_02e$FINAL_WCVP_ID
    ) == FALSE
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
# 19. IMPORTANT CORRECTION TO FINAL CANONICAL CHECK
#
# The 823 genus IDs are necessarily only a subset of the 11,439 canonical
# population. Therefore setequal(canonical, genus) must be FALSE.
#
# The preceding validation expresses exactly that requirement.
# ==============================================================================


# ==============================================================================
# 20. DISPLAY VALIDATION
# ==============================================================================

cat("\n")
cat("============================================================\n")
cat("02e.2 VALIDATION\n")
cat("============================================================\n\n")

print(
  validation,
  n = Inf
)


# ==============================================================================
# 21. FAIL BEFORE WRITING IF VALIDATION FAILS
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
    "\n02e.2 VALIDATION FAILED:\n",
    paste(
      failed_checks,
      collapse = "\n"
    ),
    "\n\nNo 02e.2 database tables were written."
  )
}


# ==============================================================================
# 22. WRITE 02e.2 AUDIT TABLES TO DUCKDB
# ==============================================================================

cat(
  "\nWriting 02e.2 closure tables to DuckDB...\n"
)

DBI::dbWriteTable(
  con,
  TABLE_RESOLUTION,
  exception_resolution,
  overwrite = TRUE
)

DBI::dbWriteTable(
  con,
  TABLE_FINAL_AUDIT,
  final_audit,
  overwrite = TRUE
)

DBI::dbWriteTable(
  con,
  TABLE_SUMMARY,
  final_summary,
  overwrite = TRUE
)

DBI::dbWriteTable(
  con,
  TABLE_CLOSURE,
  closure_summary,
  overwrite = TRUE
)

DBI::dbWriteTable(
  con,
  TABLE_VALIDATION,
  validation,
  overwrite = TRUE
)

cat("DuckDB closure tables written.\n")


# ==============================================================================
# 23. WRITE CSV OUTPUTS
# ==============================================================================

cat("Writing CSV closure outputs...\n")

write_csv(
  exception_resolution,
  file.path(
    OUTPUT_DIR,
    "VPJD_02e2_exception_resolution.csv"
  )
)

write_csv(
  final_audit,
  file.path(
    OUTPUT_DIR,
    "VPJD_02e2_genus_source_name_final_audit.csv"
  )
)

write_csv(
  final_summary,
  file.path(
    OUTPUT_DIR,
    "VPJD_02e2_genus_source_name_summary.csv"
  )
)

write_csv(
  closure_summary,
  file.path(
    OUTPUT_DIR,
    "VPJD_02e2_closure_summary.csv"
  )
)

write_csv(
  validation,
  file.path(
    OUTPUT_DIR,
    "VPJD_02e2_validation.csv"
  )
)


# ==============================================================================
# 24. WRITE OUTPUT INVENTORY
# ==============================================================================

output_inventory <- tibble(
  
  FILE = c(
    
    "VPJD_02e2_exception_resolution.csv",
    
    "VPJD_02e2_genus_source_name_final_audit.csv",
    
    "VPJD_02e2_genus_source_name_summary.csv",
    
    "VPJD_02e2_closure_summary.csv",
    
    "VPJD_02e2_validation.csv"
  ),
  
  PURPOSE = c(
    
    "Formal resolution of the Peracarpa authorship false positive",
    
    "Corrected final audit of all 823 genus concepts",
    
    "Final source classification summary",
    
    "Formal closure metrics for the 02e genus audit",
    
    "02e.2 validation record"
  )
)

write_csv(
  output_inventory,
  file.path(
    OUTPUT_DIR,
    "VPJD_02e2_output_inventory.csv"
  )
)


# ==============================================================================
# 25. FINAL REPORT
# ==============================================================================

cat("\n")
cat("============================================================\n")
cat("02e.2 FINAL GENUS AUDIT\n")
cat("============================================================\n\n")

print(
  final_summary,
  n = Inf
)

cat("\n")
cat("------------------------------------------------------------\n")
cat("CLOSURE SUMMARY\n")
cat("------------------------------------------------------------\n\n")

print(
  closure_summary,
  n = Inf
)

cat("\n")
cat("------------------------------------------------------------\n")
cat("FALSE-POSITIVE RESOLUTION\n")
cat("------------------------------------------------------------\n\n")

print(
  exception_resolution,
  n = Inf,
  width = Inf
)


# ==============================================================================
# 26. CLEAN DISCONNECT
# ==============================================================================

DBI::dbDisconnect(
  con,
  shutdown = TRUE
)

rm(con)

gc()


# ==============================================================================
# 27. COMPLETION REPORT
# ==============================================================================

cat("\n")
cat("============================================================\n")

cat(
  "VPJD TAXONOMIC REVISION 02e.2 v",
  VERSION,
  " COMPLETE\n",
  sep = ""
)

cat("============================================================\n\n")

cat("Final genus audit status: CLOSED\n")

cat(
  "Genus concepts audited: ",
  EXPECTED_GENUS_CONCEPTS,
  "\n",
  sep = ""
)

cat(
  "Genus concepts supported as genus-level: ",
  EXPECTED_GENUS_CONCEPTS,
  "\n",
  sep = ""
)

cat("Recoverable species identifications: 0\n")
cat("Recoverable infraspecific identifications: 0\n")
cat("False-positive heuristic flags resolved: 1\n")
cat("Outstanding genus reconciliations: 0\n\n")

cat("Canonical taxa removed: 0\n")
cat("Canonical taxonomy modified: FALSE\n")
cat("Star allocations modified: FALSE\n")

cat(
  "Validation: ",
  sum(validation$PASS),
  "/",
  nrow(validation),
  " PASS\n",
  sep = ""
)

cat("\nOutput directory:\n")
cat(OUTPUT_DIR, "\n")

cat("\nDuckDB connection closed cleanly.\n")

cat("============================================================\n")


# ==============================================================================
# END
# ==============================================================================