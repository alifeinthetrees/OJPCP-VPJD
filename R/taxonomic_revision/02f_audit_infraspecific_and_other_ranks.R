# ==============================================================================
# VPJD TAXONOMIC REVISION
# 02f — AUDIT INFRASPECIFIC AND OTHER RANKS
#
# Version: 0.1.0
#
# PURPOSE
# -------
# Audit the taxonomic-rank composition of the canonical contemporary VPJD
# population, with particular attention to concepts that are neither Species
# nor Genus.
#
# This module:
#
#   1. validates the canonical VPJD population (11,439 concepts);
#   2. inventories every WCVP rank represented;
#   3. identifies Species, Genus, Subspecies, Variety, Form and other ranks;
#   4. isolates all infraspecific and unusual-rank concepts;
#   5. attaches family/genus hierarchy where available;
#   6. carries provisional Star allocations without changing them;
#   7. attaches the 02b vascular/non-vascular classification where available;
#   8. calculates occurrence-record counts;
#   9. assigns an AUDIT-ONLY publication taxonomic class;
#  10. writes DuckDB audit tables and VPJD-prefixed CSV outputs;
#  11. makes NO taxonomic or Star-allocation changes.
#
# IMPORTANT
# ---------
# PUBLICATION_TAXONOMIC_CLASS is an audit classification only.
# It does NOT determine final inclusion/exclusion from the published VPJD.
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
  "stringr"
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


# ==============================================================================
# 02. MODULE METADATA
# ==============================================================================

MODULE <- "02f_audit_infraspecific_and_other_ranks"
VERSION <- "0.1.0"
RUN_DATE <- Sys.Date()

EXPECTED_CANONICAL_POPULATION <- 11439L
EXPECTED_GENUS_CONCEPTS <- 823L


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
  "02f_infraspecific_other_ranks"
)

dir.create(
  OUTPUT_DIR,
  recursive = TRUE,
  showWarnings = FALSE
)


# ==============================================================================
# 04. DATABASE TABLES
# ==============================================================================

TABLE_CANONICAL <-
  "vpjd_star_provisional_wholesale_allocation"

TABLE_WCVP <-
  "occurrence_wcvp_accepted_taxa"

TABLE_OCCURRENCES <-
  "occurrence_wcvp"

TABLE_02B_CANDIDATES <- c(
  "vpjd_taxrev_02b_lineage_classification",
  "vpjd_taxrev_02b_family_classification",
  "vpjd_taxrev_02b_family_crosswalk"
)

TABLE_02E2 <-
  "vpjd_taxrev_02e2_genus_source_name_final_audit"


# ==============================================================================
# 05. OUTPUT DATABASE TABLES
# ==============================================================================

TABLE_RANK_INVENTORY <-
  "vpjd_taxrev_02f_rank_inventory"

TABLE_AUDIT <-
  "vpjd_taxrev_02f_rank_audit"

TABLE_NON_SPECIES_GENUS <-
  "vpjd_taxrev_02f_infraspecific_other_rank_audit"

TABLE_PUBLICATION_CLASS <-
  "vpjd_taxrev_02f_publication_taxonomic_class"

TABLE_SUMMARY <-
  "vpjd_taxrev_02f_summary"

TABLE_VALIDATION <-
  "vpjd_taxrev_02f_validation"


# ==============================================================================
# 06. START REPORT
# ==============================================================================

cat("\n")
cat("============================================================\n")
cat("VPJD TAXONOMIC REVISION — 02f\n")
cat("AUDIT INFRASPECIFIC AND OTHER RANKS\n")
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
  "\n\n",
  sep = ""
)

cat("Mode: NON-DESTRUCTIVE TAXONOMIC AUDIT\n")
cat("Canonical taxonomy modification: FALSE\n")
cat("Star modification: FALSE\n\n")


# ==============================================================================
# 07. CHECK DATABASE FILE
# ==============================================================================

if (!file.exists(DB_PATH)) {
  
  stop(
    "VPJD DuckDB database not found:\n",
    DB_PATH
  )
}


# ==============================================================================
# 08. CLOSE STALE CONNECTION IF PRESENT
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
# 09. OPEN DUCKDB
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

cat("DuckDB connection established.\n\n")


# ==============================================================================
# 10. TABLE INVENTORY
# ==============================================================================

db_tables <- DBI::dbListTables(con)

if (!TABLE_CANONICAL %in% db_tables) {
  
  DBI::dbDisconnect(
    con,
    shutdown = TRUE
  )
  
  stop(
    "Required canonical table missing: ",
    TABLE_CANONICAL
  )
}

if (!TABLE_WCVP %in% db_tables) {
  
  DBI::dbDisconnect(
    con,
    shutdown = TRUE
  )
  
  stop(
    "Required WCVP table missing: ",
    TABLE_WCVP
  )
}

cat("Canonical table found: ", TABLE_CANONICAL, "\n", sep = "")
cat("WCVP lookup table found: ", TABLE_WCVP, "\n", sep = "")


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


# ==============================================================================
# 12. VALIDATE CANONICAL POPULATION
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

cat(
  "Canonical population validated: ",
  format(
    nrow(canonical),
    big.mark = ","
  ),
  "\n\n",
  sep = ""
)


# ==============================================================================
# 13. LOAD WCVP TAXONOMIC HIERARCHY
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
  
  DBI::dbDisconnect(
    con,
    shutdown = TRUE
  )
  
  stop(
    "Required WCVP field(s) missing: ",
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
      wcvp_taxon_name,
      wcvp_taxon_rank,
      wcvp_taxon_status,
      wcvp_family,
      wcvp_genus,
      wcvp_species
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

cat(
  "WCVP accepted lookup concepts loaded: ",
  format(
    nrow(wcvp),
    big.mark = ","
  ),
  "\n",
  sep = ""
)


# ==============================================================================
# 14. JOIN CANONICAL POPULATION TO WCVP
# ==============================================================================

taxonomy <- canonical |>
  left_join(
    wcvp,
    by = "FINAL_WCVP_ID"
  ) |>
  mutate(
    
    TAXON_RANK_RAW =
      coalesce(
        wcvp_taxon_rank,
        FINAL_WCVP_RANK
      ),
    
    TAXON_RANK_NORMALISED =
      str_to_upper(
        str_trim(
          TAXON_RANK_RAW
        )
      )
  )

n_missing_wcvp <- sum(
  is.na(taxonomy$wcvp_taxon_name)
)

cat(
  "Canonical concepts linked to WCVP hierarchy: ",
  format(
    nrow(taxonomy) - n_missing_wcvp,
    big.mark = ","
  ),
  " / ",
  format(
    nrow(taxonomy),
    big.mark = ","
  ),
  "\n",
  sep = ""
)

cat(
  "Missing WCVP hierarchy links: ",
  n_missing_wcvp,
  "\n\n",
  sep = ""
)


# ==============================================================================
# 15. CLASSIFY TAXONOMIC RANKS
#
# No concepts are changed here.
# ==============================================================================

taxonomy <- taxonomy |>
  mutate(
    
    RANK_GROUP = case_when(
      
      TAXON_RANK_NORMALISED ==
        "SPECIES" ~
        "SPECIES",
      
      TAXON_RANK_NORMALISED ==
        "GENUS" ~
        "GENUS",
      
      TAXON_RANK_NORMALISED %in%
        c(
          "SUBSPECIES",
          "SUBSPECIES."
        ) ~
        "SUBSPECIES",
      
      TAXON_RANK_NORMALISED %in%
        c(
          "VARIETY",
          "VAR."
        ) ~
        "VARIETY",
      
      TAXON_RANK_NORMALISED %in%
        c(
          "FORM",
          "FORMA",
          "F."
        ) ~
        "FORM",
      
      is.na(TAXON_RANK_NORMALISED) |
        TAXON_RANK_NORMALISED == "" ~
        "MISSING_RANK",
      
      TRUE ~
        "OTHER_RANK"
    ),
    
    IS_SPECIES =
      RANK_GROUP ==
      "SPECIES",
    
    IS_GENUS =
      RANK_GROUP ==
      "GENUS",
    
    IS_INFRASPECIFIC =
      RANK_GROUP %in%
      c(
        "SUBSPECIES",
        "VARIETY",
        "FORM"
      ),
    
    IS_OTHER_RANK =
      RANK_GROUP %in%
      c(
        "OTHER_RANK",
        "MISSING_RANK"
      )
  )


# ==============================================================================
# 16. RANK INVENTORY
# ==============================================================================

rank_inventory <- taxonomy |>
  count(
    TAXON_RANK_RAW,
    TAXON_RANK_NORMALISED,
    RANK_GROUP,
    name = "N_CONCEPTS"
  ) |>
  mutate(
    PERCENT_OF_11439 =
      round(
        100 *
          N_CONCEPTS /
          EXPECTED_CANONICAL_POPULATION,
        3
      )
  ) |>
  arrange(
    desc(N_CONCEPTS),
    TAXON_RANK_NORMALISED
  )


# ==============================================================================
# 17. LOAD 02b VASCULAR / NON-VASCULAR CLASSIFICATION
#
# 02b evolved during the revision, so locate the available final table
# rather than assuming a single table name.
# ==============================================================================

table_02b <- TABLE_02B_CANDIDATES[
  TABLE_02B_CANDIDATES %in%
    db_tables
]

vascular_lookup <- NULL
vascular_source <- NA_character_

if (length(table_02b) > 0L) {
  
  vascular_source <- table_02b[[1]]
  
  fields_02b <- DBI::dbListFields(
    con,
    vascular_source
  )
  
  id_candidates <- c(
    "FINAL_WCVP_ID",
    "wcvp_plant_name_id"
  )
  
  vascular_candidates <- c(
    "VASCULAR_STATUS",
    "VASCULAR_CLASS",
    "VASCULAR_NONVASCULAR",
    "MAJOR_GROUP",
    "LINEAGE_GROUP",
    "PLANT_GROUP"
  )
  
  id_field <- id_candidates[
    id_candidates %in%
      fields_02b
  ]
  
  vascular_field <- vascular_candidates[
    vascular_candidates %in%
      fields_02b
  ]
  
  if (
    length(id_field) > 0L &&
    length(vascular_field) > 0L
  ) {
    
    vascular_lookup <- DBI::dbGetQuery(
      con,
      paste0(
        '
        SELECT
          CAST("',
        id_field[[1]],
        '" AS VARCHAR) AS FINAL_WCVP_ID,
          "',
        vascular_field[[1]],
        '" AS VASCULAR_CLASSIFICATION
        FROM "',
        vascular_source,
        '"
        '
      )
    ) |>
      as_tibble() |>
      distinct(
        FINAL_WCVP_ID,
        .keep_all = TRUE
      )
  }
}


# ==============================================================================
# 18. FALLBACK: FAMILY-LEVEL 02b CLASSIFICATION
#
# Some 02b outputs may classify families rather than individual WCVP IDs.
# If no concept-level lookup was recovered above, attempt a family join.
# ==============================================================================

family_vascular_lookup <- NULL

if (is.null(vascular_lookup)) {
  
  for (tbl in table_02b) {
    
    fields_tbl <- DBI::dbListFields(
      con,
      tbl
    )
    
    family_candidates <- c(
      "FAMILY",
      "wcvp_family",
      "WCVP_FAMILY"
    )
    
    vascular_candidates <- c(
      "VASCULAR_STATUS",
      "VASCULAR_CLASS",
      "VASCULAR_NONVASCULAR",
      "MAJOR_GROUP",
      "LINEAGE_GROUP",
      "PLANT_GROUP"
    )
    
    family_field <- family_candidates[
      family_candidates %in%
        fields_tbl
    ]
    
    vascular_field <- vascular_candidates[
      vascular_candidates %in%
        fields_tbl
    ]
    
    if (
      length(family_field) > 0L &&
      length(vascular_field) > 0L
    ) {
      
      family_vascular_lookup <-
        DBI::dbGetQuery(
          con,
          paste0(
            '
            SELECT
              "',
            family_field[[1]],
            '" AS wcvp_family,
              "',
            vascular_field[[1]],
            '" AS VASCULAR_CLASSIFICATION
            FROM "',
            tbl,
            '"
            '
          )
        ) |>
        as_tibble() |>
        distinct(
          wcvp_family,
          .keep_all = TRUE
        )
      
      vascular_source <- tbl
      
      break
    }
  }
}


# ==============================================================================
# 19. ATTACH VASCULAR CLASSIFICATION
# ==============================================================================

if (!is.null(vascular_lookup)) {
  
  taxonomy <- taxonomy |>
    left_join(
      vascular_lookup,
      by = "FINAL_WCVP_ID"
    )
  
} else if (!is.null(family_vascular_lookup)) {
  
  taxonomy <- taxonomy |>
    left_join(
      family_vascular_lookup,
      by = "wcvp_family"
    )
  
} else {
  
  taxonomy <- taxonomy |>
    mutate(
      VASCULAR_CLASSIFICATION =
        NA_character_
    )
}

cat(
  "02b vascular classification source: ",
  ifelse(
    is.na(vascular_source),
    "NOT LOCATED — retained as NA",
    vascular_source
  ),
  "\n\n",
  sep = ""
)


# ==============================================================================
# 20. IDENTIFY NON-VASCULAR STATUS
#
# Use the existing 02b classification only.
# Do not independently reclassify families here.
# ==============================================================================

taxonomy <- taxonomy |>
  mutate(
    
    VASCULAR_CLASSIFICATION_NORMALISED =
      str_to_upper(
        str_trim(
          VASCULAR_CLASSIFICATION
        )
      ),
    
    IS_NON_VASCULAR =
      case_when(
        
        is.na(
          VASCULAR_CLASSIFICATION_NORMALISED
        ) ~
          NA,
        
        str_detect(
          VASCULAR_CLASSIFICATION_NORMALISED,
          "NON.?VASCULAR|BRYOPHY|HORNWORT|LIVERWORT|MOSS"
        ) ~
          TRUE,
        
        TRUE ~
          FALSE
      )
  )


# ==============================================================================
# 21. OCCURRENCE COUNTS
#
# Prefer occurrence_wcvp if available.
# If the table is unavailable, retain NA rather than invent counts.
# ==============================================================================

occurrence_counts <- NULL
occurrence_source <- NA_character_

if (TABLE_OCCURRENCES %in% db_tables) {
  
  occurrence_fields <- DBI::dbListFields(
    con,
    TABLE_OCCURRENCES
  )
  
  occurrence_id_candidates <- c(
    "FINAL_WCVP_ID",
    "wcvp_plant_name_id",
    "WCVP_ID"
  )
  
  occurrence_id_field <-
    occurrence_id_candidates[
      occurrence_id_candidates %in%
        occurrence_fields
    ]
  
  if (length(occurrence_id_field) > 0L) {
    
    occurrence_source <- TABLE_OCCURRENCES
    
    occurrence_counts <- DBI::dbGetQuery(
      con,
      paste0(
        '
        SELECT
          CAST("',
        occurrence_id_field[[1]],
        '" AS VARCHAR) AS FINAL_WCVP_ID,
          COUNT(*) AS N_OCCURRENCE_RECORDS
        FROM "',
        TABLE_OCCURRENCES,
        '"
        WHERE "',
        occurrence_id_field[[1]],
        '" IS NOT NULL
        GROUP BY "',
        occurrence_id_field[[1]],
        '"
        '
      )
    ) |>
      as_tibble()
  }
}


# ==============================================================================
# 22. FALLBACK OCCURRENCE COUNTS FROM 02e.2 FOR GENUS CONCEPTS
# ==============================================================================

if (!is.null(occurrence_counts)) {
  
  taxonomy <- taxonomy |>
    left_join(
      occurrence_counts,
      by = "FINAL_WCVP_ID"
    )
  
} else {
  
  taxonomy <- taxonomy |>
    mutate(
      N_OCCURRENCE_RECORDS =
        NA_real_
    )
}

cat(
  "Occurrence-count source: ",
  ifelse(
    is.na(occurrence_source),
    "NOT LOCATED",
    occurrence_source
  ),
  "\n\n",
  sep = ""
)


# ==============================================================================
# 23. PUBLICATION TAXONOMIC CLASS
#
# AUDIT ONLY.
#
# This field is intended to organise subsequent review. It is NOT a final
# inclusion/exclusion decision.
# ==============================================================================

taxonomy <- taxonomy |>
  mutate(
    
    PUBLICATION_TAXONOMIC_CLASS =
      case_when(
        
        IS_NON_VASCULAR %in% TRUE ~
          "NON_VASCULAR",
        
        RANK_GROUP ==
          "SPECIES" ~
          "SPECIES_LEVEL",
        
        RANK_GROUP ==
          "GENUS" ~
          "GENUS_LEVEL",
        
        RANK_GROUP %in%
          c(
            "SUBSPECIES",
            "VARIETY",
            "FORM"
          ) ~
          "INFRASPECIFIC",
        
        TRUE ~
          "OTHER_REVIEW_REQUIRED"
      ),
    
    PUBLICATION_REVIEW_STATUS =
      case_when(
        
        IS_NON_VASCULAR %in% TRUE ~
          "REVIEW_FOR_SCOPE_EXCLUSION",
        
        RANK_GROUP ==
          "SPECIES" ~
          "STANDARD_SPECIES_CONCEPT",
        
        RANK_GROUP ==
          "GENUS" ~
          "GENUS_AUDIT_COMPLETED_02E2",
        
        RANK_GROUP %in%
          c(
            "SUBSPECIES",
            "VARIETY",
            "FORM"
          ) ~
          "INFRASPECIFIC_REVIEW_REQUIRED",
        
        TRUE ~
          "OTHER_RANK_REVIEW_REQUIRED"
      ),
    
    TAXONOMIC_ACTION_TAKEN =
      FALSE,
    
    STAR_ACTION_TAKEN =
      FALSE,
    
    MODULE =
      MODULE,
    
    MODULE_VERSION =
      VERSION
  )


# ==============================================================================
# 24. NON-SPECIES / NON-GENUS AUDIT POPULATION
# ==============================================================================

non_species_genus <- taxonomy |>
  filter(
    !RANK_GROUP %in%
      c(
        "SPECIES",
        "GENUS"
      )
  ) |>
  arrange(
    RANK_GROUP,
    wcvp_family,
    wcvp_genus,
    FINAL_WCVP_RECOGNISED_NAME
  )


# ==============================================================================
# 25. INFRASPECIFIC POPULATION
# ==============================================================================

infraspecific <- taxonomy |>
  filter(
    IS_INFRASPECIFIC
  ) |>
  arrange(
    RANK_GROUP,
    wcvp_family,
    wcvp_genus,
    FINAL_WCVP_RECOGNISED_NAME
  )


# ==============================================================================
# 26. OTHER / UNUSUAL RANK POPULATION
# ==============================================================================

other_ranks <- taxonomy |>
  filter(
    IS_OTHER_RANK
  ) |>
  arrange(
    TAXON_RANK_NORMALISED,
    wcvp_family,
    wcvp_genus,
    FINAL_WCVP_RECOGNISED_NAME
  )


# ==============================================================================
# 27. PUBLICATION CLASS SUMMARY
# ==============================================================================

publication_class_summary <- taxonomy |>
  count(
    PUBLICATION_TAXONOMIC_CLASS,
    PUBLICATION_REVIEW_STATUS,
    name = "N_CONCEPTS"
  ) |>
  mutate(
    PERCENT_OF_11439 =
      round(
        100 *
          N_CONCEPTS /
          EXPECTED_CANONICAL_POPULATION,
        3
      )
  ) |>
  arrange(
    desc(N_CONCEPTS),
    PUBLICATION_TAXONOMIC_CLASS
  )


# ==============================================================================
# 28. RANK-GROUP SUMMARY
# ==============================================================================

rank_group_summary <- taxonomy |>
  count(
    RANK_GROUP,
    name = "N_CONCEPTS"
  ) |>
  mutate(
    PERCENT_OF_11439 =
      round(
        100 *
          N_CONCEPTS /
          EXPECTED_CANONICAL_POPULATION,
        3
      )
  ) |>
  arrange(
    desc(N_CONCEPTS)
  )


# ==============================================================================
# 29. STAR × RANK SUMMARY
# ==============================================================================

star_rank_summary <- taxonomy |>
  count(
    PROVISIONAL_STAR,
    RANK_GROUP,
    name = "N_CONCEPTS"
  ) |>
  arrange(
    PROVISIONAL_STAR,
    desc(N_CONCEPTS),
    RANK_GROUP
  )


# ==============================================================================
# 30. FAMILY × RANK SUMMARY
# ==============================================================================

family_rank_summary <- taxonomy |>
  count(
    wcvp_family,
    RANK_GROUP,
    name = "N_CONCEPTS"
  ) |>
  arrange(
    wcvp_family,
    desc(N_CONCEPTS),
    RANK_GROUP
  )


# ==============================================================================
# 31. SUMMARY METRICS
# ==============================================================================

n_species <- sum(
  taxonomy$RANK_GROUP == "SPECIES"
)

n_genus <- sum(
  taxonomy$RANK_GROUP == "GENUS"
)

n_subspecies <- sum(
  taxonomy$RANK_GROUP == "SUBSPECIES"
)

n_variety <- sum(
  taxonomy$RANK_GROUP == "VARIETY"
)

n_form <- sum(
  taxonomy$RANK_GROUP == "FORM"
)

n_other <- sum(
  taxonomy$RANK_GROUP == "OTHER_RANK"
)

n_missing_rank <- sum(
  taxonomy$RANK_GROUP == "MISSING_RANK"
)

n_infraspecific <- sum(
  taxonomy$IS_INFRASPECIFIC
)

n_non_species_genus <-
  nrow(non_species_genus)

n_non_vascular <- sum(
  taxonomy$IS_NON_VASCULAR %in% TRUE,
  na.rm = TRUE
)

n_vascular_unknown <- sum(
  is.na(taxonomy$IS_NON_VASCULAR)
)


summary_metrics <- tibble(
  
  METRIC = c(
    "Canonical VPJD concepts",
    "Species concepts",
    "Genus concepts",
    "Subspecies concepts",
    "Variety concepts",
    "Form concepts",
    "Total recognised infraspecific concepts",
    "Other-rank concepts",
    "Missing-rank concepts",
    "All non-species/non-genus concepts",
    "Concepts classified non-vascular by 02b",
    "Concepts without attached vascular classification"
  ),
  
  N = c(
    nrow(taxonomy),
    n_species,
    n_genus,
    n_subspecies,
    n_variety,
    n_form,
    n_infraspecific,
    n_other,
    n_missing_rank,
    n_non_species_genus,
    n_non_vascular,
    n_vascular_unknown
  )
)


# ==============================================================================
# 32. VALIDATION
# ==============================================================================

validation <- tibble(
  
  CHECK = c(
    
    "Canonical allocation table present",
    
    "WCVP accepted lookup table present",
    
    "Canonical population = 11,439",
    
    "Canonical population unique by WCVP ID",
    
    "All canonical concepts linked to WCVP hierarchy",
    
    "All concepts assigned a normalised rank group",
    
    "Rank groups total 11,439",
    
    "Species + genus + non-species/non-genus = 11,439",
    
    "Genus population = 823",
    
    "02e.2 closure table present",
    
    "All concepts assigned publication taxonomic class",
    
    "Publication taxonomic classes total 11,439",
    
    "All concepts assigned publication review status",
    
    "No automatic taxonomic actions taken",
    
    "No automatic Star actions taken",
    
    "Canonical population remains 11,439",
    
    "Canonical WCVP IDs remain unique",
    
    "Provisional Star values retained",
    
    "02f audit population = 11,439"
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
    
    n_missing_wcvp ==
      0L,
    
    all(
      !is.na(taxonomy$RANK_GROUP)
    ),
    
    sum(
      rank_group_summary$N_CONCEPTS
    ) ==
      EXPECTED_CANONICAL_POPULATION,
    
    (
      n_species +
        n_genus +
        n_non_species_genus
    ) ==
      EXPECTED_CANONICAL_POPULATION,
    
    n_genus ==
      EXPECTED_GENUS_CONCEPTS,
    
    TABLE_02E2 %in%
      db_tables,
    
    all(
      !is.na(
        taxonomy$PUBLICATION_TAXONOMIC_CLASS
      )
    ),
    
    sum(
      publication_class_summary$N_CONCEPTS
    ) ==
      EXPECTED_CANONICAL_POPULATION,
    
    all(
      !is.na(
        taxonomy$PUBLICATION_REVIEW_STATUS
      )
    ),
    
    !any(
      taxonomy$TAXONOMIC_ACTION_TAKEN
    ),
    
    !any(
      taxonomy$STAR_ACTION_TAKEN
    ),
    
    nrow(taxonomy) ==
      EXPECTED_CANONICAL_POPULATION,
    
    n_distinct(
      taxonomy$FINAL_WCVP_ID
    ) ==
      EXPECTED_CANONICAL_POPULATION,
    
    identical(
      as.character(
        canonical$PROVISIONAL_STAR
      ),
      as.character(
        taxonomy$PROVISIONAL_STAR
      )
    ),
    
    nrow(taxonomy) ==
      EXPECTED_CANONICAL_POPULATION
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
# 33. REPORT — EXACT RANK INVENTORY
# ==============================================================================

cat("\n")
cat("============================================================\n")
cat("02f EXACT WCVP RANK INVENTORY\n")
cat("============================================================\n\n")

print(
  rank_inventory,
  n = Inf
)


# ==============================================================================
# 34. REPORT — NORMALISED RANK GROUPS
# ==============================================================================

cat("\n")
cat("------------------------------------------------------------\n")
cat("NORMALISED RANK GROUPS\n")
cat("------------------------------------------------------------\n\n")

print(
  rank_group_summary,
  n = Inf
)


# ==============================================================================
# 35. REPORT — PUBLICATION TAXONOMIC CLASSES
# ==============================================================================

cat("\n")
cat("------------------------------------------------------------\n")
cat("PUBLICATION TAXONOMIC CLASS — AUDIT ONLY\n")
cat("------------------------------------------------------------\n\n")

print(
  publication_class_summary,
  n = Inf
)


# ==============================================================================
# 36. REPORT — SUMMARY METRICS
# ==============================================================================

cat("\n")
cat("------------------------------------------------------------\n")
cat("02f SUMMARY METRICS\n")
cat("------------------------------------------------------------\n\n")

print(
  summary_metrics,
  n = Inf
)


# ==============================================================================
# 37. REPORT — UNUSUAL RANKS
# ==============================================================================

cat("\n")
cat("------------------------------------------------------------\n")
cat("OTHER / UNUSUAL RANK CONCEPTS\n")
cat("------------------------------------------------------------\n\n")

if (nrow(other_ranks) == 0L) {
  
  cat("No other or missing-rank concepts identified.\n")
  
} else {
  
  print(
    other_ranks |>
      select(
        FINAL_WCVP_ID,
        FINAL_WCVP_RECOGNISED_NAME,
        TAXON_RANK_RAW,
        RANK_GROUP,
        wcvp_family,
        wcvp_genus,
        PROVISIONAL_STAR,
        VASCULAR_CLASSIFICATION,
        N_OCCURRENCE_RECORDS,
        PUBLICATION_REVIEW_STATUS
      ),
    n = Inf,
    width = Inf
  )
}


# ==============================================================================
# 38. REPORT — VALIDATION
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
# 39. FAIL BEFORE WRITING IF CORE VALIDATION FAILS
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
    "\n02f VALIDATION FAILED:\n",
    paste(
      failed_checks,
      collapse = "\n"
    ),
    "\n\nNo 02f database tables were written."
  )
}


# ==============================================================================
# 40. WRITE DUCKDB AUDIT TABLES
# ==============================================================================

cat("\nWriting 02f audit tables to DuckDB...\n")

DBI::dbWriteTable(
  con,
  TABLE_RANK_INVENTORY,
  rank_inventory,
  overwrite = TRUE
)

DBI::dbWriteTable(
  con,
  TABLE_AUDIT,
  taxonomy,
  overwrite = TRUE
)

DBI::dbWriteTable(
  con,
  TABLE_NON_SPECIES_GENUS,
  non_species_genus,
  overwrite = TRUE
)

DBI::dbWriteTable(
  con,
  TABLE_PUBLICATION_CLASS,
  publication_class_summary,
  overwrite = TRUE
)

DBI::dbWriteTable(
  con,
  TABLE_SUMMARY,
  summary_metrics,
  overwrite = TRUE
)

DBI::dbWriteTable(
  con,
  TABLE_VALIDATION,
  validation,
  overwrite = TRUE
)

cat("DuckDB audit tables written.\n")


# ==============================================================================
# 41. WRITE CSV OUTPUTS
# ==============================================================================

cat("Writing VPJD CSV outputs...\n")

write_csv(
  taxonomy,
  file.path(
    OUTPUT_DIR,
    "VPJD_02f_full_rank_audit.csv"
  )
)

write_csv(
  rank_inventory,
  file.path(
    OUTPUT_DIR,
    "VPJD_02f_exact_rank_inventory.csv"
  )
)

write_csv(
  rank_group_summary,
  file.path(
    OUTPUT_DIR,
    "VPJD_02f_rank_group_summary.csv"
  )
)

write_csv(
  non_species_genus,
  file.path(
    OUTPUT_DIR,
    "VPJD_02f_infraspecific_other_rank_audit.csv"
  )
)

write_csv(
  infraspecific,
  file.path(
    OUTPUT_DIR,
    "VPJD_02f_infraspecific_concepts.csv"
  )
)

write_csv(
  other_ranks,
  file.path(
    OUTPUT_DIR,
    "VPJD_02f_other_unusual_rank_concepts.csv"
  )
)

write_csv(
  publication_class_summary,
  file.path(
    OUTPUT_DIR,
    "VPJD_02f_publication_taxonomic_class_summary.csv"
  )
)

write_csv(
  star_rank_summary,
  file.path(
    OUTPUT_DIR,
    "VPJD_02f_star_by_rank_summary.csv"
  )
)

write_csv(
  family_rank_summary,
  file.path(
    OUTPUT_DIR,
    "VPJD_02f_family_by_rank_summary.csv"
  )
)

write_csv(
  summary_metrics,
  file.path(
    OUTPUT_DIR,
    "VPJD_02f_summary_metrics.csv"
  )
)

write_csv(
  validation,
  file.path(
    OUTPUT_DIR,
    "VPJD_02f_validation.csv"
  )
)


# ==============================================================================
# 42. OUTPUT INVENTORY
# ==============================================================================

output_inventory <- tibble(
  
  FILE = c(
    
    "VPJD_02f_full_rank_audit.csv",
    
    "VPJD_02f_exact_rank_inventory.csv",
    
    "VPJD_02f_rank_group_summary.csv",
    
    "VPJD_02f_infraspecific_other_rank_audit.csv",
    
    "VPJD_02f_infraspecific_concepts.csv",
    
    "VPJD_02f_other_unusual_rank_concepts.csv",
    
    "VPJD_02f_publication_taxonomic_class_summary.csv",
    
    "VPJD_02f_star_by_rank_summary.csv",
    
    "VPJD_02f_family_by_rank_summary.csv",
    
    "VPJD_02f_summary_metrics.csv",
    
    "VPJD_02f_validation.csv"
  ),
  
  PURPOSE = c(
    
    "Complete 11,439-concept 02f rank audit",
    
    "Exact WCVP rank inventory",
    
    "Normalised rank-group summary",
    
    "All concepts outside Species and Genus",
    
    "Subspecies, variety and form concepts",
    
    "Unusual or missing-rank concepts requiring review",
    
    "Audit-only publication taxonomic class summary",
    
    "Provisional Star allocation by taxonomic rank",
    
    "Family composition by taxonomic rank",
    
    "Principal 02f population metrics",
    
    "Formal validation record"
  )
)

write_csv(
  output_inventory,
  file.path(
    OUTPUT_DIR,
    "VPJD_02f_output_inventory.csv"
  )
)


# ==============================================================================
# 43. FINAL REPORT
# ==============================================================================

cat("\n")
cat("============================================================\n")
cat("02f RESULTS\n")
cat("============================================================\n\n")

cat(
  "Canonical VPJD population: ",
  format(
    nrow(taxonomy),
    big.mark = ","
  ),
  "\n",
  sep = ""
)

cat(
  "Species concepts: ",
  format(
    n_species,
    big.mark = ","
  ),
  "\n",
  sep = ""
)

cat(
  "Genus concepts: ",
  format(
    n_genus,
    big.mark = ","
  ),
  "\n",
  sep = ""
)

cat(
  "Subspecies concepts: ",
  format(
    n_subspecies,
    big.mark = ","
  ),
  "\n",
  sep = ""
)

cat(
  "Variety concepts: ",
  format(
    n_variety,
    big.mark = ","
  ),
  "\n",
  sep = ""
)

cat(
  "Form concepts: ",
  format(
    n_form,
    big.mark = ","
  ),
  "\n",
  sep = ""
)

cat(
  "Total recognised infraspecific concepts: ",
  format(
    n_infraspecific,
    big.mark = ","
  ),
  "\n",
  sep = ""
)

cat(
  "Other-rank concepts: ",
  format(
    n_other,
    big.mark = ","
  ),
  "\n",
  sep = ""
)

cat(
  "Missing-rank concepts: ",
  format(
    n_missing_rank,
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
  "02b non-vascular concepts identified: ",
  format(
    n_non_vascular,
    big.mark = ","
  ),
  "\n",
  sep = ""
)

cat(
  "Concepts lacking attached vascular classification: ",
  format(
    n_vascular_unknown,
    big.mark = ","
  ),
  "\n\n",
  sep = ""
)

cat(
  "Validation: ",
  sum(validation$PASS),
  "/",
  nrow(validation),
  " PASS\n",
  sep = ""
)

cat("\nCanonical taxa removed: 0\n")
cat("Canonical taxonomy modified: FALSE\n")
cat("Star allocations modified: FALSE\n")


# ==============================================================================
# 44. CLEAN DISCONNECT
# ==============================================================================

DBI::dbDisconnect(
  con,
  shutdown = TRUE
)

rm(con)

gc()


# ==============================================================================
# 45. COMPLETION REPORT
# ==============================================================================

cat("\n")
cat("============================================================\n")

cat(
  "VPJD TAXONOMIC REVISION 02f v",
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

cat("Canonical population retained: 11,439\n")
cat("Canonical taxa removed: 0\n")
cat("Canonical taxonomy modified: FALSE\n")
cat("Star allocations modified: FALSE\n")
cat("02f audit tables written: TRUE\n")
cat("CSV outputs written: TRUE\n")
cat("DuckDB connection closed cleanly.\n")

cat("\n")
cat("============================================================\n")


# ==============================================================================
# END
# ==============================================================================