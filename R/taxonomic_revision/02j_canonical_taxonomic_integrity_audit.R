# ==============================================================================
# VPJD — TAXONOMIC REVISION
# 02j — CANONICAL TAXONOMIC INTEGRITY AUDIT
#
# Purpose:
#
#   Test the internal hierarchical integrity of the validated canonical
#   Vascular Plants of Japan Database (VPJD).
#
#   This module follows:
#
#     02h — validated vascular lineage
#     02i — taxonomic representation audit
#
#
# AUTHORITATIVE INPUT
#
#   vpjd_taxrev_02h_validated_vascular_lineage
#
#
# CORE QUESTIONS
#
#   1. Does every canonical WCVP ID occur exactly once?
#
#   2. Is every taxon assigned consistently to:
#
#        MAJOR_GROUP
#        FAMILY
#        GENUS
#        SPECIES
#
#      where those fields are biologically applicable?
#
#   3. Does each genus belong to one family and one major group?
#
#   4. Does each represented species belong to one genus, one family,
#      and one major group?
#
#   5. Do infraspecific records have sufficient parent-species information?
#
#   6. Are the 9,033 represented species versus 8,906 species-rank records
#      explained by species represented only through infraspecific records?
#
#   7. Are there orphaned or contradictory infraspecific taxa?
#
#   8. What are the two canonical records currently classified as OTHER rank?
#
#
# IMPORTANT
#
#   This is an INTEGRITY AUDIT.
#
#   Absence of an explicit genus-rank canonical record does NOT automatically
#   make a species record invalid.
#
#   Absence of an explicit species-rank canonical record does NOT automatically
#   make an infraspecific record invalid.
#
#   These conditions are measured separately from actual contradictions.
#
#
# SAFETY
#
#   AUDIT ONLY.
#
#   This script does NOT:
#
#     - modify DuckDB
#     - replace tables
#     - alter WCVP IDs
#     - alter accepted names
#     - alter taxonomy
#     - alter Star allocations
#     - add taxa
#     - remove taxa
#
#
# EXPECTED POPULATION
#
#   Canonical records:             11,439
#   Families represented:             260
#   Genera represented:             2,146
#   Genus-rank records:               823
#   Species represented:            9,033
#   Species-rank records:           8,906
#   Subspecies-rank records:          499
#   Variety-rank records:           1,147
#   Form-rank records:                 62
#   Other-rank records:                 2
#
#
# OUTPUT DIRECTORY
#
#   outputs/tables/taxonomic_revision/
#   02j_canonical_taxonomic_integrity_audit/
#
# ==============================================================================


# ==============================================================================
# 01. VERSION
# ==============================================================================

VERSION <- "0.1.0"


# ==============================================================================
# 02. PACKAGES
# ==============================================================================

suppressPackageStartupMessages({
  
  library(DBI)
  library(duckdb)
  
  library(dplyr)
  library(tidyr)
  library(tibble)
  
  library(stringr)
  library(readr)
  library(purrr)
  
})


# ==============================================================================
# 03. EXPECTED VALUES
# ==============================================================================

EXPECTED_CANONICAL_POPULATION <- 11439L

EXPECTED_FAMILIES <- 260L

EXPECTED_GENERA_REPRESENTED <- 2146L

EXPECTED_GENUS_RANK <- 823L

EXPECTED_SPECIES_REPRESENTED <- 9033L

EXPECTED_SPECIES_RANK <- 8906L

EXPECTED_SUBSPECIES_RANK <- 499L

EXPECTED_VARIETY_RANK <- 1147L

EXPECTED_FORM_RANK <- 62L

EXPECTED_OTHER_RANK <- 2L


EXPECTED_MAJOR_GROUPS <- c(
  
  "ANGIOSPERM",
  "GYMNOSPERM",
  "FERN",
  "LYCOPHYTE"
  
)


# ==============================================================================
# 04. PATHS
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
  "02j_canonical_taxonomic_integrity_audit"
  
)


dir.create(
  
  OUTPUT_DIR,
  
  recursive = TRUE,
  
  showWarnings = FALSE
  
)


# ==============================================================================
# 05. SOURCE
# ==============================================================================

SOURCE_TABLE <-
  "vpjd_taxrev_02h_validated_vascular_lineage"


# ==============================================================================
# 06. START REPORT
# ==============================================================================

cat("\n")

cat("============================================================\n")

cat(
  "VPJD TAXONOMIC REVISION 02j v",
  VERSION,
  "\n",
  sep = ""
)

cat("CANONICAL TAXONOMIC INTEGRITY AUDIT\n")

cat("============================================================\n\n")


cat(
  "Source table:\n",
  SOURCE_TABLE,
  "\n\n",
  sep = ""
)


cat("AUDIT ONLY\n")
cat("Database modification: DISABLED\n\n")


# ==============================================================================
# 07. DATABASE CHECK
# ==============================================================================

if (!file.exists(DB_PATH)) {
  
  stop(
    
    "DuckDB database not found:\n",
    DB_PATH
    
  )
  
}


# ==============================================================================
# 08. CONNECT READ-ONLY
# ==============================================================================

con <- DBI::dbConnect(
  
  duckdb::duckdb(),
  
  dbdir = DB_PATH,
  
  read_only = TRUE
  
)


# ==============================================================================
# 09. SAFE DISCONNECT
# ==============================================================================

disconnect_safely <- function() {
  
  if (
    exists(
      "con",
      inherits = TRUE
    ) &&
    DBI::dbIsValid(con)
  ) {
    
    DBI::dbDisconnect(
      
      con,
      
      shutdown = TRUE
      
    )
    
  }
  
}


# ==============================================================================
# 10. CHECK SOURCE TABLE
# ==============================================================================

db_tables <-
  DBI::dbListTables(con)


if (
  !SOURCE_TABLE %in%
  db_tables
) {
  
  disconnect_safely()
  
  
  stop(
    
    "Required 02h source table not found:\n",
    SOURCE_TABLE
    
  )
  
}


# ==============================================================================
# 11. LOAD DATA
# ==============================================================================

cat(
  "Loading validated 02h canonical vascular lineage...\n"
)


vpjd <-
  DBI::dbReadTable(
    
    con,
    
    SOURCE_TABLE
    
  ) |>
  
  as_tibble()


cat(
  
  "Records loaded: ",
  
  format(
    nrow(vpjd),
    big.mark = ","
  ),
  
  "\n\n",
  
  sep = ""
  
)


# ==============================================================================
# 12. REQUIRED FIELDS
# ==============================================================================

required_fields <- c(
  
  "FINAL_WCVP_ID",
  
  "FINAL_WCVP_RECOGNISED_NAME",
  
  "FINAL_WCVP_RANK",
  
  "FAMILY",
  
  "GENUS",
  
  "SPECIES",
  
  "MAJOR_GROUP"
  
)


missing_required_fields <-
  setdiff(
    
    required_fields,
    
    names(vpjd)
    
  )


if (
  length(
    missing_required_fields
  ) > 0L
) {
  
  disconnect_safely()
  
  
  stop(
    
    "Required field(s) missing:\n",
    
    paste(
      missing_required_fields,
      collapse = "\n"
    )
    
  )
  
}


# ==============================================================================
# 13. STANDARDISE CHARACTER FIELDS
# ==============================================================================

vpjd <- vpjd |>
  
  mutate(
    
    FINAL_WCVP_ID =
      str_squish(
        as.character(
          FINAL_WCVP_ID
        )
      ),
    
    FINAL_WCVP_RECOGNISED_NAME =
      str_squish(
        as.character(
          FINAL_WCVP_RECOGNISED_NAME
        )
      ),
    
    FINAL_WCVP_RANK =
      str_squish(
        as.character(
          FINAL_WCVP_RANK
        )
      ),
    
    FAMILY =
      str_squish(
        as.character(
          FAMILY
        )
      ),
    
    GENUS =
      str_squish(
        as.character(
          GENUS
        )
      ),
    
    SPECIES =
      str_squish(
        as.character(
          SPECIES
        )
      ),
    
    MAJOR_GROUP =
      toupper(
        str_squish(
          as.character(
            MAJOR_GROUP
          )
        )
      )
    
  )


# ==============================================================================
# 14. NORMALISE EMPTY STRINGS
# ==============================================================================

vpjd <- vpjd |>
  
  mutate(
    
    across(
      
      c(
        FINAL_WCVP_ID,
        FINAL_WCVP_RECOGNISED_NAME,
        FINAL_WCVP_RANK,
        FAMILY,
        GENUS,
        SPECIES,
        MAJOR_GROUP
      ),
      
      ~ na_if(
        .x,
        ""
      )
      
    )
    
  )


# ==============================================================================
# 15. NORMALISE RANK
# ==============================================================================

vpjd <- vpjd |>
  
  mutate(
    
    RANK_NORMALISED =
      case_when(
        
        str_to_lower(
          FINAL_WCVP_RANK
        ) ==
          "genus" ~
          "GENUS",
        
        str_to_lower(
          FINAL_WCVP_RANK
        ) ==
          "species" ~
          "SPECIES",
        
        str_to_lower(
          FINAL_WCVP_RANK
        ) %in%
          c(
            "subspecies",
            "subsp."
          ) ~
          "SUBSPECIES",
        
        str_to_lower(
          FINAL_WCVP_RANK
        ) %in%
          c(
            "variety",
            "var."
          ) ~
          "VARIETY",
        
        str_to_lower(
          FINAL_WCVP_RANK
        ) %in%
          c(
            "form",
            "forma",
            "f."
          ) ~
          "FORM",
        
        is.na(
          FINAL_WCVP_RANK
        ) ~
          "MISSING",
        
        TRUE ~
          "OTHER"
        
      )
    
  )


# ==============================================================================
# 16. BASIC COUNTS
# ==============================================================================

n_records <-
  nrow(vpjd)


n_ids <-
  n_distinct(
    vpjd$FINAL_WCVP_ID
  )


n_families <-
  vpjd |>
  
  filter(
    !is.na(FAMILY)
  ) |>
  
  summarise(
    N =
      n_distinct(
        FAMILY
      )
  ) |>
  
  pull(N)


n_genera <-
  vpjd |>
  
  filter(
    !is.na(GENUS)
  ) |>
  
  summarise(
    N =
      n_distinct(
        GENUS
      )
  ) |>
  
  pull(N)


# ==============================================================================
# 17. DUPLICATE CANONICAL WCVP IDs
# ==============================================================================

duplicate_wcvp_ids <- vpjd |>
  
  count(
    
    FINAL_WCVP_ID,
    
    name =
      "N_RECORDS"
    
  ) |>
  
  filter(
    
    is.na(
      FINAL_WCVP_ID
    ) |
      N_RECORDS != 1L
    
  ) |>
  
  arrange(
    desc(
      N_RECORDS
    )
  )


# ==============================================================================
# 18. FAMILY → MAJOR GROUP INTEGRITY
# ==============================================================================

family_parentage <- vpjd |>
  
  filter(
    !is.na(FAMILY)
  ) |>
  
  group_by(
    FAMILY
  ) |>
  
  summarise(
    
    N_MAJOR_GROUPS =
      n_distinct(
        MAJOR_GROUP
      ),
    
    MAJOR_GROUPS =
      paste(
        sort(
          unique(
            MAJOR_GROUP
          )
        ),
        collapse = " | "
      ),
    
    N_RECORDS =
      n(),
    
    .groups =
      "drop"
    
  )


contradictory_family_parentage <-
  family_parentage |>
  
  filter(
    N_MAJOR_GROUPS != 1L
  )


# ==============================================================================
# 19. GENUS → FAMILY / MAJOR GROUP INTEGRITY
# ==============================================================================

genus_parentage <- vpjd |>
  
  filter(
    !is.na(GENUS)
  ) |>
  
  group_by(
    GENUS
  ) |>
  
  summarise(
    
    N_FAMILIES =
      n_distinct(
        FAMILY
      ),
    
    FAMILIES =
      paste(
        sort(
          unique(
            FAMILY
          )
        ),
        collapse = " | "
      ),
    
    N_MAJOR_GROUPS =
      n_distinct(
        MAJOR_GROUP
      ),
    
    MAJOR_GROUPS =
      paste(
        sort(
          unique(
            MAJOR_GROUP
          )
        ),
        collapse = " | "
      ),
    
    N_RECORDS =
      n(),
    
    N_GENUS_RANK_RECORDS =
      sum(
        RANK_NORMALISED ==
          "GENUS"
      ),
    
    .groups =
      "drop"
    
  ) |>
  
  mutate(
    
    HAS_GENUS_RANK_RECORD =
      N_GENUS_RANK_RECORDS > 0L
    
  )


contradictory_genus_parentage <-
  genus_parentage |>
  
  filter(
    
    N_FAMILIES != 1L |
      N_MAJOR_GROUPS != 1L
    
  )


# ==============================================================================
# 20. GENUS-RANK RECORD INTEGRITY
# ==============================================================================

genus_rank_records <- vpjd |>
  
  filter(
    RANK_NORMALISED ==
      "GENUS"
  )


genus_rank_integrity <- genus_rank_records |>
  
  transmute(
    
    FINAL_WCVP_ID,
    
    FINAL_WCVP_RECOGNISED_NAME,
    
    FAMILY,
    
    GENUS,
    
    MAJOR_GROUP,
    
    GENUS_MISSING =
      is.na(GENUS),
    
    FAMILY_MISSING =
      is.na(FAMILY),
    
    MAJOR_GROUP_MISSING =
      is.na(MAJOR_GROUP)
    
  )


genus_rank_exceptions <- genus_rank_integrity |>
  
  filter(
    
    GENUS_MISSING |
      FAMILY_MISSING |
      MAJOR_GROUP_MISSING
    
  )


# ==============================================================================
# 21. SPECIES KEY
# ==============================================================================

# A species is identified here by GENUS + SPECIES.
#
# This is safer than SPECIES alone if SPECIES contains only the specific
# epithet. If SPECIES already contains a binomial, the combined key remains
# internally consistent for this audit.


vpjd <- vpjd |>
  
  mutate(
    
    SPECIES_KEY =
      case_when(
        
        !is.na(GENUS) &
          !is.na(SPECIES) ~
          paste(
            GENUS,
            SPECIES,
            sep = " | "
          ),
        
        TRUE ~
          NA_character_
        
      )
    
  )


# ==============================================================================
# 22. SPECIES → GENUS / FAMILY / MAJOR GROUP INTEGRITY
# ==============================================================================

species_parentage <- vpjd |>
  
  filter(
    !is.na(SPECIES_KEY)
  ) |>
  
  group_by(
    SPECIES_KEY
  ) |>
  
  summarise(
    
    GENUS =
      first(
        GENUS
      ),
    
    SPECIES =
      first(
        SPECIES
      ),
    
    N_GENERA =
      n_distinct(
        GENUS
      ),
    
    GENERA =
      paste(
        sort(
          unique(
            GENUS
          )
        ),
        collapse = " | "
      ),
    
    N_FAMILIES =
      n_distinct(
        FAMILY
      ),
    
    FAMILIES =
      paste(
        sort(
          unique(
            FAMILY
          )
        ),
        collapse = " | "
      ),
    
    N_MAJOR_GROUPS =
      n_distinct(
        MAJOR_GROUP
      ),
    
    MAJOR_GROUPS =
      paste(
        sort(
          unique(
            MAJOR_GROUP
          )
        ),
        collapse = " | "
      ),
    
    N_RECORDS =
      n(),
    
    N_SPECIES_RANK_RECORDS =
      sum(
        RANK_NORMALISED ==
          "SPECIES"
      ),
    
    N_SUBSPECIES_RECORDS =
      sum(
        RANK_NORMALISED ==
          "SUBSPECIES"
      ),
    
    N_VARIETY_RECORDS =
      sum(
        RANK_NORMALISED ==
          "VARIETY"
      ),
    
    N_FORM_RECORDS =
      sum(
        RANK_NORMALISED ==
          "FORM"
      ),
    
    .groups =
      "drop"
    
  ) |>
  
  mutate(
    
    N_INFRASPECIFIC_RECORDS =
      
      N_SUBSPECIES_RECORDS +
      N_VARIETY_RECORDS +
      N_FORM_RECORDS,
    
    HAS_SPECIES_RANK_RECORD =
      N_SPECIES_RANK_RECORDS > 0L,
    
    REPRESENTED_ONLY_INFRASPECIFIC =
      
      !HAS_SPECIES_RANK_RECORD &
      N_INFRASPECIFIC_RECORDS > 0L
    
  )


contradictory_species_parentage <-
  species_parentage |>
  
  filter(
    
    N_GENERA != 1L |
      N_FAMILIES != 1L |
      N_MAJOR_GROUPS != 1L
    
  )


# ==============================================================================
# 23. SPECIES-RANK RECORD INTEGRITY
# ==============================================================================

species_rank_records <- vpjd |>
  
  filter(
    RANK_NORMALISED ==
      "SPECIES"
  )


species_rank_integrity <- species_rank_records |>
  
  transmute(
    
    FINAL_WCVP_ID,
    
    FINAL_WCVP_RECOGNISED_NAME,
    
    FAMILY,
    
    GENUS,
    
    SPECIES,
    
    SPECIES_KEY,
    
    MAJOR_GROUP,
    
    GENUS_MISSING =
      is.na(GENUS),
    
    SPECIES_MISSING =
      is.na(SPECIES),
    
    FAMILY_MISSING =
      is.na(FAMILY),
    
    MAJOR_GROUP_MISSING =
      is.na(MAJOR_GROUP)
    
  )


species_rank_exceptions <- species_rank_integrity |>
  
  filter(
    
    GENUS_MISSING |
      SPECIES_MISSING |
      FAMILY_MISSING |
      MAJOR_GROUP_MISSING
    
  )


# ==============================================================================
# 24. INFRASPECIFIC RECORDS
# ==============================================================================

infraspecific_records <- vpjd |>
  
  filter(
    
    RANK_NORMALISED %in%
      c(
        "SUBSPECIES",
        "VARIETY",
        "FORM"
      )
    
  )


n_infraspecific_records <-
  nrow(
    infraspecific_records
  )


# ==============================================================================
# 25. INFRASPECIFIC STRUCTURAL INTEGRITY
# ==============================================================================

infraspecific_integrity <- infraspecific_records |>
  
  transmute(
    
    FINAL_WCVP_ID,
    
    FINAL_WCVP_RECOGNISED_NAME,
    
    FINAL_WCVP_RANK,
    
    RANK_NORMALISED,
    
    FAMILY,
    
    GENUS,
    
    SPECIES,
    
    SPECIES_KEY,
    
    MAJOR_GROUP,
    
    FAMILY_MISSING =
      is.na(FAMILY),
    
    GENUS_MISSING =
      is.na(GENUS),
    
    SPECIES_MISSING =
      is.na(SPECIES),
    
    MAJOR_GROUP_MISSING =
      is.na(MAJOR_GROUP)
    
  )


orphaned_infraspecific_records <-
  infraspecific_integrity |>
  
  filter(
    
    FAMILY_MISSING |
      GENUS_MISSING |
      SPECIES_MISSING |
      MAJOR_GROUP_MISSING
    
  )


# ==============================================================================
# 26. SPECIES REPRESENTED ONLY BY INFRASPECIFIC RECORDS
# ==============================================================================

species_only_infraspecific <- species_parentage |>
  
  filter(
    REPRESENTED_ONLY_INFRASPECIFIC
  ) |>
  
  arrange(
    GENUS,
    SPECIES
  )


n_species_only_infraspecific <-
  nrow(
    species_only_infraspecific
  )


# ==============================================================================
# 27. EXPECTED SPECIES REPRESENTATION DIFFERENCE
# ==============================================================================

n_species_represented <-
  nrow(
    species_parentage
  )


n_species_rank <-
  nrow(
    species_rank_records
  )


species_representation_difference <-
  
  n_species_represented -
  n_species_rank


# ==============================================================================
# 28. SPECIES WITH MULTIPLE SPECIES-RANK RECORDS
# ==============================================================================

multiple_species_rank_records <- species_parentage |>
  
  filter(
    N_SPECIES_RANK_RECORDS > 1L
  ) |>
  
  arrange(
    desc(
      N_SPECIES_RANK_RECORDS
    )
  )


# ==============================================================================
# 29. GENERA WITHOUT GENUS-RANK RECORD
# ==============================================================================

genera_without_genus_rank <- genus_parentage |>
  
  filter(
    !HAS_GENUS_RANK_RECORD
  ) |>
  
  arrange(
    GENUS
  )


n_genera_without_genus_rank <-
  nrow(
    genera_without_genus_rank
  )


# ==============================================================================
# 30. GENERA WITH MULTIPLE GENUS-RANK RECORDS
# ==============================================================================

multiple_genus_rank_records <- genus_parentage |>
  
  filter(
    N_GENUS_RANK_RECORDS > 1L
  ) |>
  
  arrange(
    desc(
      N_GENUS_RANK_RECORDS
    )
  )


# ==============================================================================
# 31. OTHER-RANK RECORDS
# ==============================================================================

other_rank_records <- vpjd |>
  
  filter(
    RANK_NORMALISED ==
      "OTHER"
  ) |>
  
  select(
    
    FINAL_WCVP_ID,
    
    FINAL_WCVP_RECOGNISED_NAME,
    
    FINAL_WCVP_RANK,
    
    FAMILY,
    
    GENUS,
    
    SPECIES,
    
    MAJOR_GROUP,
    
    everything()
    
  )


# ==============================================================================
# 32. MISSING-RANK RECORDS
# ==============================================================================

missing_rank_records <- vpjd |>
  
  filter(
    RANK_NORMALISED ==
      "MISSING"
  )


# ==============================================================================
# 33. MAJOR-GROUP SUMMARY
# ==============================================================================

major_group_summary <- vpjd |>
  
  group_by(
    MAJOR_GROUP
  ) |>
  
  summarise(
    
    N_RECORDS =
      n(),
    
    N_FAMILIES =
      n_distinct(
        FAMILY
      ),
    
    N_GENERA =
      n_distinct(
        GENUS,
        na.rm = TRUE
      ),
    
    N_SPECIES =
      n_distinct(
        SPECIES_KEY,
        na.rm = TRUE
      ),
    
    N_GENUS_RANK =
      sum(
        RANK_NORMALISED ==
          "GENUS"
      ),
    
    N_SPECIES_RANK =
      sum(
        RANK_NORMALISED ==
          "SPECIES"
      ),
    
    N_SUBSPECIES =
      sum(
        RANK_NORMALISED ==
          "SUBSPECIES"
      ),
    
    N_VARIETIES =
      sum(
        RANK_NORMALISED ==
          "VARIETY"
      ),
    
    N_FORMS =
      sum(
        RANK_NORMALISED ==
          "FORM"
      ),
    
    N_OTHER =
      sum(
        RANK_NORMALISED ==
          "OTHER"
      ),
    
    .groups =
      "drop"
    
  ) |>
  
  mutate(
    
    PERCENT_RECORDS =
      100 *
      N_RECORDS /
      sum(
        N_RECORDS
      )
    
  ) |>
  
  arrange(
    desc(
      N_RECORDS
    )
  )


# ==============================================================================
# 34. HIERARCHICAL EXCEPTION SUMMARY
# ==============================================================================

exception_summary <- tibble(
  
  EXCEPTION = c(
    
    "Duplicate or missing canonical WCVP IDs",
    
    "Families assigned to multiple major groups",
    
    "Genera assigned to multiple families or major groups",
    
    "Genus-rank records missing required hierarchy",
    
    "Species assigned contradictory parentage",
    
    "Species-rank records missing required hierarchy",
    
    "Infraspecific records missing required parent information",
    
    "Species represented only through infraspecific records",
    
    "Species with multiple species-rank records",
    
    "Genera represented without genus-rank record",
    
    "Genera with multiple genus-rank records",
    
    "Other-rank canonical records",
    
    "Missing-rank canonical records"
    
  ),
  
  
  N = c(
    
    nrow(
      duplicate_wcvp_ids
    ),
    
    nrow(
      contradictory_family_parentage
    ),
    
    nrow(
      contradictory_genus_parentage
    ),
    
    nrow(
      genus_rank_exceptions
    ),
    
    nrow(
      contradictory_species_parentage
    ),
    
    nrow(
      species_rank_exceptions
    ),
    
    nrow(
      orphaned_infraspecific_records
    ),
    
    nrow(
      species_only_infraspecific
    ),
    
    nrow(
      multiple_species_rank_records
    ),
    
    nrow(
      genera_without_genus_rank
    ),
    
    nrow(
      multiple_genus_rank_records
    ),
    
    nrow(
      other_rank_records
    ),
    
    nrow(
      missing_rank_records
    )
    
  )
  
)


# ==============================================================================
# 35. OVERALL SUMMARY
# ==============================================================================

overall_summary <- tibble(
  
  METRIC = c(
    
    "Canonical VPJD records",
    
    "Families represented",
    
    "Genera represented",
    
    "Genus-rank records",
    
    "Genera without genus-rank record",
    
    "Species represented",
    
    "Species-rank records",
    
    "Species represented only through infraspecific records",
    
    "Species representation minus species-rank records",
    
    "Infraspecific records",
    
    "Subspecies records",
    
    "Variety records",
    
    "Form records",
    
    "Other-rank records",
    
    "Missing-rank records",
    
    "Contradictory family parentage",
    
    "Contradictory genus parentage",
    
    "Contradictory species parentage",
    
    "Orphaned infraspecific records"
    
  ),
  
  
  N = c(
    
    n_records,
    
    n_families,
    
    n_genera,
    
    nrow(
      genus_rank_records
    ),
    
    n_genera_without_genus_rank,
    
    n_species_represented,
    
    n_species_rank,
    
    n_species_only_infraspecific,
    
    species_representation_difference,
    
    n_infraspecific_records,
    
    sum(
      vpjd$RANK_NORMALISED ==
        "SUBSPECIES"
    ),
    
    sum(
      vpjd$RANK_NORMALISED ==
        "VARIETY"
    ),
    
    sum(
      vpjd$RANK_NORMALISED ==
        "FORM"
    ),
    
    nrow(
      other_rank_records
    ),
    
    nrow(
      missing_rank_records
    ),
    
    nrow(
      contradictory_family_parentage
    ),
    
    nrow(
      contradictory_genus_parentage
    ),
    
    nrow(
      contradictory_species_parentage
    ),
    
    nrow(
      orphaned_infraspecific_records
    )
    
  )
  
)


# ==============================================================================
# 36. EXPECTED MAJOR GROUP CHECK
# ==============================================================================

observed_major_groups <-
  sort(
    unique(
      vpjd$MAJOR_GROUP
    )
  )


# ==============================================================================
# 37. VALIDATION
# ==============================================================================

validation <- tibble(
  
  CHECK = c(
    
    "02h validated lineage table present",
    
    "Canonical population = 11,439",
    
    "Canonical population unique by WCVP ID",
    
    "Families represented = 260",
    
    "Genera represented = 2,146",
    
    "Genus-rank records = 823",
    
    "Species represented = 9,033",
    
    "Species-rank records = 8,906",
    
    "Subspecies records = 499",
    
    "Variety records = 1,147",
    
    "Form records = 62",
    
    "Other-rank records = 2",
    
    "Major vascular groups exactly represented",
    
    "No contradictory family parentage",
    
    "No contradictory genus parentage",
    
    "No genus-rank hierarchy exceptions",
    
    "No contradictory species parentage",
    
    "No species-rank hierarchy exceptions",
    
    "No orphaned infraspecific records",
    
    "Species representation difference explained by infraspecific-only species",
    
    "No species has multiple species-rank records",
    
    "No genus has multiple genus-rank records",
    
    "No missing taxonomic ranks",
    
    "No automatic taxonomic action taken",
    
    "No automatic Star action taken",
    
    "No taxa removed",
    
    "No taxa added",
    
    "Database not modified"
    
  ),
  
  
  PASS = c(
    
    SOURCE_TABLE %in%
      db_tables,
    
    n_records ==
      EXPECTED_CANONICAL_POPULATION,
    
    n_ids ==
      EXPECTED_CANONICAL_POPULATION,
    
    n_families ==
      EXPECTED_FAMILIES,
    
    n_genera ==
      EXPECTED_GENERA_REPRESENTED,
    
    nrow(
      genus_rank_records
    ) ==
      EXPECTED_GENUS_RANK,
    
    n_species_represented ==
      EXPECTED_SPECIES_REPRESENTED,
    
    n_species_rank ==
      EXPECTED_SPECIES_RANK,
    
    sum(
      vpjd$RANK_NORMALISED ==
        "SUBSPECIES"
    ) ==
      EXPECTED_SUBSPECIES_RANK,
    
    sum(
      vpjd$RANK_NORMALISED ==
        "VARIETY"
    ) ==
      EXPECTED_VARIETY_RANK,
    
    sum(
      vpjd$RANK_NORMALISED ==
        "FORM"
    ) ==
      EXPECTED_FORM_RANK,
    
    nrow(
      other_rank_records
    ) ==
      EXPECTED_OTHER_RANK,
    
    setequal(
      
      observed_major_groups,
      
      EXPECTED_MAJOR_GROUPS
      
    ),
    
    nrow(
      contradictory_family_parentage
    ) ==
      0L,
    
    nrow(
      contradictory_genus_parentage
    ) ==
      0L,
    
    nrow(
      genus_rank_exceptions
    ) ==
      0L,
    
    nrow(
      contradictory_species_parentage
    ) ==
      0L,
    
    nrow(
      species_rank_exceptions
    ) ==
      0L,
    
    nrow(
      orphaned_infraspecific_records
    ) ==
      0L,
    
    species_representation_difference ==
      n_species_only_infraspecific,
    
    nrow(
      multiple_species_rank_records
    ) ==
      0L,
    
    nrow(
      multiple_genus_rank_records
    ) ==
      0L,
    
    nrow(
      missing_rank_records
    ) ==
      0L,
    
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
        
        "REVIEW"
        
      )
    
  )


# ==============================================================================
# 38. WRITE CSV OUTPUTS
# ==============================================================================

write_csv(
  
  overall_summary,
  
  file.path(
    OUTPUT_DIR,
    "VPJD_02j_overall_summary.csv"
  )
  
)


write_csv(
  
  major_group_summary,
  
  file.path(
    OUTPUT_DIR,
    "VPJD_02j_major_group_summary.csv"
  )
  
)


write_csv(
  
  exception_summary,
  
  file.path(
    OUTPUT_DIR,
    "VPJD_02j_exception_summary.csv"
  )
  
)


write_csv(
  
  family_parentage,
  
  file.path(
    OUTPUT_DIR,
    "VPJD_02j_family_parentage.csv"
  )
  
)


write_csv(
  
  contradictory_family_parentage,
  
  file.path(
    OUTPUT_DIR,
    "VPJD_02j_contradictory_family_parentage.csv"
  )
  
)


write_csv(
  
  genus_parentage,
  
  file.path(
    OUTPUT_DIR,
    "VPJD_02j_genus_parentage.csv"
  )
  
)


write_csv(
  
  contradictory_genus_parentage,
  
  file.path(
    OUTPUT_DIR,
    "VPJD_02j_contradictory_genus_parentage.csv"
  )
  
)


write_csv(
  
  genera_without_genus_rank,
  
  file.path(
    OUTPUT_DIR,
    "VPJD_02j_genera_without_genus_rank.csv"
  )
  
)


write_csv(
  
  multiple_genus_rank_records,
  
  file.path(
    OUTPUT_DIR,
    "VPJD_02j_multiple_genus_rank_records.csv"
  )
  
)


write_csv(
  
  genus_rank_exceptions,
  
  file.path(
    OUTPUT_DIR,
    "VPJD_02j_genus_rank_exceptions.csv"
  )
  
)


write_csv(
  
  species_parentage,
  
  file.path(
    OUTPUT_DIR,
    "VPJD_02j_species_parentage.csv"
  )
  
)


write_csv(
  
  contradictory_species_parentage,
  
  file.path(
    OUTPUT_DIR,
    "VPJD_02j_contradictory_species_parentage.csv"
  )
  
)


write_csv(
  
  species_only_infraspecific,
  
  file.path(
    OUTPUT_DIR,
    "VPJD_02j_species_only_infraspecific.csv"
  )
  
)


write_csv(
  
  multiple_species_rank_records,
  
  file.path(
    OUTPUT_DIR,
    "VPJD_02j_multiple_species_rank_records.csv"
  )
  
)


write_csv(
  
  species_rank_exceptions,
  
  file.path(
    OUTPUT_DIR,
    "VPJD_02j_species_rank_exceptions.csv"
  )
  
)


write_csv(
  
  infraspecific_integrity,
  
  file.path(
    OUTPUT_DIR,
    "VPJD_02j_infraspecific_integrity.csv"
  )
  
)


write_csv(
  
  orphaned_infraspecific_records,
  
  file.path(
    OUTPUT_DIR,
    "VPJD_02j_orphaned_infraspecific_records.csv"
  )
  
)


write_csv(
  
  other_rank_records,
  
  file.path(
    OUTPUT_DIR,
    "VPJD_02j_other_rank_records.csv"
  )
  
)


write_csv(
  
  duplicate_wcvp_ids,
  
  file.path(
    OUTPUT_DIR,
    "VPJD_02j_duplicate_wcvp_ids.csv"
  )
  
)


write_csv(
  
  validation,
  
  file.path(
    OUTPUT_DIR,
    "VPJD_02j_validation.csv"
  )
  
)


# ==============================================================================
# 39. OVERALL REPORT
# ==============================================================================

cat("\n")

cat("============================================================\n")
cat("02j CANONICAL TAXONOMIC INTEGRITY\n")
cat("============================================================\n\n")


print(
  
  overall_summary,
  
  n = Inf
  
)


# ==============================================================================
# 40. MAJOR-GROUP REPORT
# ==============================================================================

cat("\n")

cat("------------------------------------------------------------\n")
cat("MAJOR-GROUP TAXONOMIC REPRESENTATION\n")
cat("------------------------------------------------------------\n\n")


print(
  
  major_group_summary,
  
  n = Inf,
  
  width = Inf
  
)


# ==============================================================================
# 41. EXCEPTION REPORT
# ==============================================================================

cat("\n")

cat("------------------------------------------------------------\n")
cat("HIERARCHICAL EXCEPTIONS\n")
cat("------------------------------------------------------------\n\n")


print(
  
  exception_summary,
  
  n = Inf
  
)


# ==============================================================================
# 42. SPECIES REPRESENTATION EXPLANATION
# ==============================================================================

cat("\n")

cat("------------------------------------------------------------\n")
cat("SPECIES REPRESENTATION RECONCILIATION\n")
cat("------------------------------------------------------------\n\n")


cat(
  
  "Distinct species represented: ",
  
  format(
    n_species_represented,
    big.mark = ","
  ),
  
  "\n",
  
  sep = ""
  
)


cat(
  
  "Species-rank canonical records: ",
  
  format(
    n_species_rank,
    big.mark = ","
  ),
  
  "\n",
  
  sep = ""
  
)


cat(
  
  "Difference: ",
  
  format(
    species_representation_difference,
    big.mark = ","
  ),
  
  "\n",
  
  sep = ""
  
)


cat(
  
  "Species represented only through infraspecific records: ",
  
  format(
    n_species_only_infraspecific,
    big.mark = ","
  ),
  
  "\n\n",
  
  sep = ""
  
)


if (
  species_representation_difference ==
  n_species_only_infraspecific
) {
  
  cat(
    "SPECIES REPRESENTATION DIFFERENCE: FULLY RECONCILED\n"
  )
  
} else {
  
  cat(
    "SPECIES REPRESENTATION DIFFERENCE: REVIEW REQUIRED\n"
  )
  
}


# ==============================================================================
# 43. GENUS REPRESENTATION EXPLANATION
# ==============================================================================

cat("\n")

cat("------------------------------------------------------------\n")
cat("GENUS REPRESENTATION\n")
cat("------------------------------------------------------------\n\n")


cat(
  
  "Distinct genera represented: ",
  
  format(
    n_genera,
    big.mark = ","
  ),
  
  "\n",
  
  sep = ""
  
)


cat(
  
  "Genus-rank canonical records: ",
  
  format(
    nrow(
      genus_rank_records
    ),
    big.mark = ","
  ),
  
  "\n",
  
  sep = ""
  
)


cat(
  
  "Genera represented without genus-rank canonical record: ",
  
  format(
    n_genera_without_genus_rank,
    big.mark = ","
  ),
  
  "\n",
  
  sep = ""
  
)


# ==============================================================================
# 44. OTHER RANK REPORT
# ==============================================================================

cat("\n")

cat("------------------------------------------------------------\n")
cat("OTHER-RANK CANONICAL RECORDS\n")
cat("------------------------------------------------------------\n\n")


if (
  nrow(
    other_rank_records
  ) ==
  0L
) {
  
  cat(
    "No other-rank canonical records.\n"
  )
  
} else {
  
  print(
    
    other_rank_records |>
      
      select(
        
        FINAL_WCVP_ID,
        
        FINAL_WCVP_RECOGNISED_NAME,
        
        FINAL_WCVP_RANK,
        
        FAMILY,
        
        GENUS,
        
        SPECIES,
        
        MAJOR_GROUP
        
      ),
    
    n = Inf,
    
    width = Inf
    
  )
  
}


# ==============================================================================
# 45. VALIDATION REPORT
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
# 46. DECISION
# ==============================================================================

all_valid <-
  all(
    validation$PASS
  )


if (all_valid) {
  
  decision <-
    "CANONICAL_TAXONOMIC_INTEGRITY_VALIDATED"
  
} else {
  
  decision <-
    "REVIEW_REQUIRED"
  
}


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
  
  "VPJD TAXONOMIC REVISION 02j v",
  
  VERSION,
  
  " COMPLETE\n",
  
  sep = ""
  
)


cat("============================================================\n\n")


cat(
  
  "Canonical population: ",
  
  format(
    n_records,
    big.mark = ","
  ),
  
  "\n",
  
  sep = ""
  
)


cat(
  
  "Families represented: ",
  
  format(
    n_families,
    big.mark = ","
  ),
  
  "\n",
  
  sep = ""
  
)


cat(
  
  "Genera represented: ",
  
  format(
    n_genera,
    big.mark = ","
  ),
  
  "\n",
  
  sep = ""
  
)


cat(
  
  "Species represented: ",
  
  format(
    n_species_represented,
    big.mark = ","
  ),
  
  "\n",
  
  sep = ""
  
)


cat(
  
  "Infraspecific records: ",
  
  format(
    n_infraspecific_records,
    big.mark = ","
  ),
  
  "\n\n",
  
  sep = ""
  
)


cat(
  
  "Contradictory family parentage: ",
  
  nrow(
    contradictory_family_parentage
  ),
  
  "\n",
  
  sep = ""
  
)


cat(
  
  "Contradictory genus parentage: ",
  
  nrow(
    contradictory_genus_parentage
  ),
  
  "\n",
  
  sep = ""
  
)


cat(
  
  "Contradictory species parentage: ",
  
  nrow(
    contradictory_species_parentage
  ),
  
  "\n",
  
  sep = ""
  
)


cat(
  
  "Orphaned infraspecific records: ",
  
  nrow(
    orphaned_infraspecific_records
  ),
  
  "\n",
  
  sep = ""
  
)


cat(
  
  "Other-rank records requiring inspection: ",
  
  nrow(
    other_rank_records
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
  
  nrow(
    validation
  ),
  
  " PASS\n",
  
  sep = ""
  
)


cat(
  
  "Decision: ",
  
  decision,
  
  "\n\n",
  
  sep = ""
  
)


cat(
  
  "Output directory:\n",
  
  OUTPUT_DIR,
  
  "\n\n",
  
  sep = ""
  
)


cat("AUDIT ONLY\n")
cat("Database modified: FALSE\n")
cat("Canonical taxonomy modified: FALSE\n")
cat("Star allocations modified: FALSE\n")
cat("Taxa removed: 0\n")
cat("Taxa added: 0\n")
cat("CSV audit outputs written: TRUE\n")
cat("DuckDB connection closed cleanly.\n")


cat("\n")

cat("============================================================\n")


# ==============================================================================
# END
# ==============================================================================