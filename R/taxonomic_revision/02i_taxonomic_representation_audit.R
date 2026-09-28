# ==============================================================================
# VPJD — TAXONOMIC REVISION
# 02i — TAXONOMIC REPRESENTATION AUDIT
#
# Purpose:
#
#   Produce a publication-ready audit of taxonomic representation in the
#   validated Vascular Plants of Japan Database (VPJD).
#
#
# AUTHORITATIVE INPUT
#
#   vpjd_taxrev_02h_validated_vascular_lineage
#
#
# THIS MODULE DISTINGUISHES BETWEEN
#
#   A. CANONICAL RECORDS / CONCEPTS BY RANK
#
#      Number of canonical VPJD records explicitly held at:
#
#        Genus
#        Species
#        Subspecies
#        Variety
#        Form
#        Other ranks
#
#
#   B. DISTINCT TAXONOMIC NAMES REPRESENTED
#
#      Number of distinct:
#
#        Families
#        Genera
#        Species values
#
#      represented anywhere in the validated VPJD dataset.
#
#
#   C. MAJOR-GROUP REPRESENTATION
#
#      All metrics are broken down across:
#
#        ANGIOSPERM
#        GYMNOSPERM
#        FERN
#        LYCOPHYTE
#
#
# IMPORTANT INTERPRETATION
#
#   A distinct value in GENUS does NOT necessarily mean that there is a
#   genus-rank canonical record for that genus.
#
#   Likewise, a species may be represented through an infraspecific record
#   even where the corresponding species-rank concept needs to be counted
#   separately.
#
#   Therefore these metrics are deliberately reported separately.
#
#
# SAFETY
#
#   AUDIT ONLY.
#
#   This script does NOT:
#
#     - modify the DuckDB database
#     - create or replace database tables
#     - alter taxonomy
#     - alter WCVP IDs
#     - alter Star allocations
#     - add taxa
#     - remove taxa
#
#
# EXPECTED VPJD POPULATION
#
#   Records:   11,439
#   Families:     260
#
#
# OUTPUT DIRECTORY
#
#   outputs/tables/taxonomic_revision/
#   02i_taxonomic_representation_audit/
#
#
# OUTPUT FILES
#
#   VPJD_02i_overall_summary.csv
#   VPJD_02i_rank_summary.csv
#   VPJD_02i_rank_by_major_group.csv
#   VPJD_02i_representation_by_major_group.csv
#   VPJD_02i_family_inventory.csv
#   VPJD_02i_genus_inventory.csv
#   VPJD_02i_species_inventory.csv
#   VPJD_02i_genus_rank_comparison.csv
#   VPJD_02i_species_rank_comparison.csv
#   VPJD_02i_other_ranks.csv
#   VPJD_02i_missing_taxonomic_fields.csv
#   VPJD_02i_validation.csv
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
# 03. CONSTANTS
# ==============================================================================

EXPECTED_CANONICAL_POPULATION <- 11439L

EXPECTED_FAMILIES <- 260L


EXPECTED_MAJOR_GROUPS <- c(
  
  "ANGIOSPERM",
  "GYMNOSPERM",
  "FERN",
  "LYCOPHYTE"
  
)


# Known rank totals from the validated earlier audit.
#
# These are used as validation checks, NOT to generate the results.


EXPECTED_GENUS_RANK <- 823L

EXPECTED_SPECIES_RANK <- 8906L

EXPECTED_SUBSPECIES_RANK <- 499L

EXPECTED_VARIETY_RANK <- 1147L

EXPECTED_FORM_RANK <- 62L

EXPECTED_OTHER_RANK <- 2L


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
  "02i_taxonomic_representation_audit"
  
)


dir.create(
  
  OUTPUT_DIR,
  
  recursive = TRUE,
  
  showWarnings = FALSE
  
)


# ==============================================================================
# 05. SOURCE TABLE
# ==============================================================================

SOURCE_TABLE <-
  "vpjd_taxrev_02h_validated_vascular_lineage"


# ==============================================================================
# 06. START REPORT
# ==============================================================================

cat("\n")

cat("============================================================\n")

cat(
  "VPJD TAXONOMIC REVISION 02i v",
  VERSION,
  "\n",
  sep = ""
)

cat("TAXONOMIC REPRESENTATION AUDIT\n")

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
# 11. LOAD VALIDATED 02h DATA
# ==============================================================================

cat(
  "Loading validated 02h vascular-lineage dataset...\n"
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
    
    "Required 02h field(s) missing:\n",
    
    paste(
      missing_required_fields,
      collapse = "\n"
    )
    
  )
  
}


# ==============================================================================
# 13. STANDARDISE FIELDS
# ==============================================================================

vpjd <- vpjd |>
  
  mutate(
    
    FINAL_WCVP_ID =
      as.character(
        FINAL_WCVP_ID
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
# 14. NORMALISE RANK
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
        ) |
          FINAL_WCVP_RANK == "" ~
          "MISSING",
        
        TRUE ~
          "OTHER"
        
      )
    
  )


# ==============================================================================
# 15. BASIC POPULATION COUNTS
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
    
    !is.na(FAMILY),
    
    FAMILY != ""
    
  ) |>
  
  summarise(
    
    N =
      n_distinct(
        FAMILY
      )
    
  ) |>
  
  pull(N)


n_major_groups <-
  n_distinct(
    vpjd$MAJOR_GROUP
  )


# ==============================================================================
# 16. CANONICAL RANK COUNTS
# ==============================================================================

rank_summary <- vpjd |>
  
  count(
    
    RANK_NORMALISED,
    
    name =
      "N_RECORDS"
    
  ) |>
  
  mutate(
    
    PERCENT_OF_VPJD =
      100 *
      N_RECORDS /
      n_records
    
  ) |>
  
  arrange(
    desc(
      N_RECORDS
    )
  )


# Extract core rank totals.


get_rank_n <- function(rank_name) {
  
  result <- rank_summary |>
    
    filter(
      RANK_NORMALISED ==
        rank_name
    ) |>
    
    pull(
      N_RECORDS
    )
  
  
  if (
    length(result) == 0L
  ) {
    
    return(0L)
    
  }
  
  
  as.integer(
    result[[1]]
  )
  
}


n_genus_rank <-
  get_rank_n(
    "GENUS"
  )


n_species_rank <-
  get_rank_n(
    "SPECIES"
  )


n_subspecies_rank <-
  get_rank_n(
    "SUBSPECIES"
  )


n_variety_rank <-
  get_rank_n(
    "VARIETY"
  )


n_form_rank <-
  get_rank_n(
    "FORM"
  )


n_other_rank <-
  get_rank_n(
    "OTHER"
  )


n_missing_rank <-
  get_rank_n(
    "MISSING"
  )


# ==============================================================================
# 17. RANK × MAJOR GROUP
# ==============================================================================

rank_by_major_group <- vpjd |>
  
  count(
    
    MAJOR_GROUP,
    
    RANK_NORMALISED,
    
    name =
      "N_RECORDS"
    
  ) |>
  
  group_by(
    MAJOR_GROUP
  ) |>
  
  mutate(
    
    PERCENT_WITHIN_GROUP =
      100 *
      N_RECORDS /
      sum(
        N_RECORDS
      )
    
  ) |>
  
  ungroup() |>
  
  arrange(
    
    MAJOR_GROUP,
    
    desc(
      N_RECORDS
    )
    
  )


# ==============================================================================
# 18. FAMILY INVENTORY
# ==============================================================================

family_inventory <- vpjd |>
  
  filter(
    
    !is.na(FAMILY),
    
    FAMILY != ""
    
  ) |>
  
  group_by(
    
    MAJOR_GROUP,
    
    FAMILY
    
  ) |>
  
  summarise(
    
    N_RECORDS =
      n(),
    
    N_WCVP_IDS =
      n_distinct(
        FINAL_WCVP_ID
      ),
    
    N_DISTINCT_GENERA =
      n_distinct(
        
        GENUS[
          !is.na(GENUS) &
            GENUS != ""
        ]
        
      ),
    
    .groups =
      "drop"
    
  ) |>
  
  arrange(
    
    MAJOR_GROUP,
    
    FAMILY
    
  )


# ==============================================================================
# 19. GENUS INVENTORY
# ==============================================================================

# This measures DISTINCT VALUES represented in GENUS across the complete VPJD.
#
# It is deliberately NOT described as "genus concepts".


genus_inventory <- vpjd |>
  
  filter(
    
    !is.na(GENUS),
    
    GENUS != ""
    
  ) |>
  
  group_by(
    
    MAJOR_GROUP,
    
    FAMILY,
    
    GENUS
    
  ) |>
  
  summarise(
    
    N_RECORDS =
      n(),
    
    N_GENUS_RANK_RECORDS =
      sum(
        RANK_NORMALISED ==
          "GENUS"
      ),
    
    N_SPECIES_RANK_RECORDS =
      sum(
        RANK_NORMALISED ==
          "SPECIES"
      ),
    
    N_INFRASPECIFIC_RECORDS =
      sum(
        
        RANK_NORMALISED %in%
          c(
            "SUBSPECIES",
            "VARIETY",
            "FORM"
          )
        
      ),
    
    .groups =
      "drop"
    
  ) |>
  
  mutate(
    
    HAS_GENUS_RANK_RECORD =
      N_GENUS_RANK_RECORDS > 0L
    
  ) |>
  
  arrange(
    
    MAJOR_GROUP,
    
    FAMILY,
    
    GENUS
    
  )


# ==============================================================================
# 20. SPECIES INVENTORY
# ==============================================================================

# SPECIES is treated here as a represented species-level value.
#
# This is separate from the number of canonical records whose rank is Species.


species_inventory <- vpjd |>
  
  filter(
    
    !is.na(SPECIES),
    
    SPECIES != ""
    
  ) |>
  
  group_by(
    
    MAJOR_GROUP,
    
    FAMILY,
    
    GENUS,
    
    SPECIES
    
  ) |>
  
  summarise(
    
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
    
    HAS_SPECIES_RANK_RECORD =
      N_SPECIES_RANK_RECORDS > 0L,
    
    REPRESENTED_ONLY_INFRASPECIFIC =
      
      !HAS_SPECIES_RANK_RECORD &
      
      (
        N_SUBSPECIES_RECORDS +
          N_VARIETY_RECORDS +
          N_FORM_RECORDS
      ) > 0L
    
  ) |>
  
  arrange(
    
    MAJOR_GROUP,
    
    FAMILY,
    
    GENUS,
    
    SPECIES
    
  )


# ==============================================================================
# 21. REPRESENTATION COUNTS BY MAJOR GROUP
# ==============================================================================

record_group_summary <- vpjd |>
  
  group_by(
    MAJOR_GROUP
  ) |>
  
  summarise(
    
    N_RECORDS =
      n(),
    
    .groups =
      "drop"
    
  )


family_group_summary <- family_inventory |>
  
  count(
    
    MAJOR_GROUP,
    
    name =
      "N_FAMILIES"
    
  )


genus_group_summary <- genus_inventory |>
  
  count(
    
    MAJOR_GROUP,
    
    name =
      "N_DISTINCT_GENERA"
    
  )


species_group_summary <- species_inventory |>
  
  group_by(
    MAJOR_GROUP
  ) |>
  
  summarise(
    
    N_DISTINCT_SPECIES_VALUES =
      n(),
    
    N_WITH_SPECIES_RANK_RECORD =
      sum(
        HAS_SPECIES_RANK_RECORD
      ),
    
    N_ONLY_INFRASPECIFIC =
      sum(
        REPRESENTED_ONLY_INFRASPECIFIC
      ),
    
    .groups =
      "drop"
    
  )


rank_group_wide <- rank_by_major_group |>
  
  select(
    
    MAJOR_GROUP,
    
    RANK_NORMALISED,
    
    N_RECORDS
    
  ) |>
  
  pivot_wider(
    
    names_from =
      RANK_NORMALISED,
    
    values_from =
      N_RECORDS,
    
    values_fill =
      0L,
    
    names_prefix =
      "N_RANK_"
    
  )


representation_by_major_group <-
  
  record_group_summary |>
  
  full_join(
    
    family_group_summary,
    
    by =
      "MAJOR_GROUP"
    
  ) |>
  
  full_join(
    
    genus_group_summary,
    
    by =
      "MAJOR_GROUP"
    
  ) |>
  
  full_join(
    
    species_group_summary,
    
    by =
      "MAJOR_GROUP"
    
  ) |>
  
  full_join(
    
    rank_group_wide,
    
    by =
      "MAJOR_GROUP"
    
  ) |>
  
  mutate(
    
    PERCENT_OF_VPJD_RECORDS =
      100 *
      N_RECORDS /
      n_records
    
  ) |>
  
  arrange(
    desc(
      N_RECORDS
    )
  )


# ==============================================================================
# 22. GENUS REPRESENTATION VS GENUS-RANK RECORDS
# ==============================================================================

genus_rank_comparison <- genus_inventory |>
  
  group_by(
    MAJOR_GROUP
  ) |>
  
  summarise(
    
    N_DISTINCT_GENERA_REPRESENTED =
      n(),
    
    N_GENERA_WITH_GENUS_RANK_RECORD =
      sum(
        HAS_GENUS_RANK_RECORD
      ),
    
    N_GENERA_WITHOUT_GENUS_RANK_RECORD =
      sum(
        !HAS_GENUS_RANK_RECORD
      ),
    
    .groups =
      "drop"
    
  ) |>
  
  mutate(
    
    PERCENT_WITH_GENUS_RANK_RECORD =
      
      100 *
      N_GENERA_WITH_GENUS_RANK_RECORD /
      N_DISTINCT_GENERA_REPRESENTED
    
  )


# ==============================================================================
# 23. SPECIES REPRESENTATION VS SPECIES-RANK RECORDS
# ==============================================================================

species_rank_comparison <- species_inventory |>
  
  group_by(
    MAJOR_GROUP
  ) |>
  
  summarise(
    
    N_DISTINCT_SPECIES_REPRESENTED =
      n(),
    
    N_SPECIES_WITH_SPECIES_RANK_RECORD =
      sum(
        HAS_SPECIES_RANK_RECORD
      ),
    
    N_SPECIES_ONLY_INFRASPECIFIC =
      sum(
        REPRESENTED_ONLY_INFRASPECIFIC
      ),
    
    .groups =
      "drop"
    
  ) |>
  
  mutate(
    
    PERCENT_WITH_SPECIES_RANK_RECORD =
      
      100 *
      N_SPECIES_WITH_SPECIES_RANK_RECORD /
      N_DISTINCT_SPECIES_REPRESENTED
    
  )


# ==============================================================================
# 24. OTHER RANK RECORDS
# ==============================================================================

other_ranks <- vpjd |>
  
  filter(
    
    RANK_NORMALISED %in%
      c(
        "OTHER",
        "MISSING"
      )
    
  ) |>
  
  select(
    
    FINAL_WCVP_ID,
    
    FINAL_WCVP_RECOGNISED_NAME,
    
    FINAL_WCVP_RANK,
    
    RANK_NORMALISED,
    
    FAMILY,
    
    GENUS,
    
    SPECIES,
    
    MAJOR_GROUP
    
  ) |>
  
  arrange(
    
    RANK_NORMALISED,
    
    FINAL_WCVP_RANK,
    
    FINAL_WCVP_RECOGNISED_NAME
    
  )


# ==============================================================================
# 25. MISSING TAXONOMIC FIELD AUDIT
# ==============================================================================

missing_taxonomic_fields <- vpjd |>
  
  transmute(
    
    FINAL_WCVP_ID,
    
    FINAL_WCVP_RECOGNISED_NAME,
    
    FINAL_WCVP_RANK,
    
    MAJOR_GROUP,
    
    FAMILY,
    
    GENUS,
    
    SPECIES,
    
    FAMILY_MISSING =
      is.na(FAMILY) |
      FAMILY == "",
    
    GENUS_MISSING =
      is.na(GENUS) |
      GENUS == "",
    
    SPECIES_MISSING =
      is.na(SPECIES) |
      SPECIES == ""
    
  ) |>
  
  filter(
    
    FAMILY_MISSING |
      GENUS_MISSING |
      SPECIES_MISSING
    
  )


# ==============================================================================
# 26. OVERALL DISTINCT REPRESENTATION
# ==============================================================================

n_distinct_genera <-
  nrow(
    genus_inventory
  )


n_distinct_species_values <-
  nrow(
    species_inventory
  )


n_genera_with_genus_rank <-
  sum(
    genus_inventory$HAS_GENUS_RANK_RECORD
  )


n_genera_without_genus_rank <-
  sum(
    !genus_inventory$HAS_GENUS_RANK_RECORD
  )


n_species_with_species_rank <-
  sum(
    species_inventory$HAS_SPECIES_RANK_RECORD
  )


n_species_only_infraspecific <-
  sum(
    species_inventory$REPRESENTED_ONLY_INFRASPECIFIC
  )


# ==============================================================================
# 27. OVERALL SUMMARY
# ==============================================================================

overall_summary <- tibble(
  
  METRIC = c(
    
    "Canonical VPJD records",
    
    "Families represented",
    
    "Distinct genera represented",
    
    "Genus-rank canonical records",
    
    "Genera represented with genus-rank record",
    
    "Genera represented without genus-rank record",
    
    "Distinct species values represented",
    
    "Species-rank canonical records",
    
    "Species represented with species-rank record",
    
    "Species represented only through infraspecific records",
    
    "Subspecies-rank canonical records",
    
    "Variety-rank canonical records",
    
    "Form-rank canonical records",
    
    "Other-rank canonical records",
    
    "Missing-rank canonical records",
    
    "Major vascular groups"
    
  ),
  
  
  N = c(
    
    n_records,
    
    n_families,
    
    n_distinct_genera,
    
    n_genus_rank,
    
    n_genera_with_genus_rank,
    
    n_genera_without_genus_rank,
    
    n_distinct_species_values,
    
    n_species_rank,
    
    n_species_with_species_rank,
    
    n_species_only_infraspecific,
    
    n_subspecies_rank,
    
    n_variety_rank,
    
    n_form_rank,
    
    n_other_rank,
    
    n_missing_rank,
    
    n_major_groups
    
  )
  
)


# ==============================================================================
# 28. VALIDATE MAJOR GROUPS
# ==============================================================================

observed_major_groups <-
  sort(
    unique(
      vpjd$MAJOR_GROUP
    )
  )


unexpected_major_groups <-
  setdiff(
    
    observed_major_groups,
    
    EXPECTED_MAJOR_GROUPS
    
  )


missing_major_groups <-
  setdiff(
    
    EXPECTED_MAJOR_GROUPS,
    
    observed_major_groups
    
  )


# ==============================================================================
# 29. RANK TOTAL
# ==============================================================================

rank_total <-
  
  n_genus_rank +
  
  n_species_rank +
  
  n_subspecies_rank +
  
  n_variety_rank +
  
  n_form_rank +
  
  n_other_rank +
  
  n_missing_rank


# ==============================================================================
# 30. FAMILY TOTAL BY GROUP
# ==============================================================================

family_group_total <-
  sum(
    family_group_summary$N_FAMILIES
  )


# ==============================================================================
# 31. RECORD TOTAL BY GROUP
# ==============================================================================

record_group_total <-
  sum(
    representation_by_major_group$N_RECORDS
  )


# ==============================================================================
# 32. VALIDATION
# ==============================================================================

validation <- tibble(
  
  CHECK = c(
    
    "02h validated lineage table present",
    
    "Canonical population = 11,439",
    
    "Canonical population unique by WCVP ID",
    
    "Family population = 260",
    
    "Major vascular groups = 4",
    
    "Expected vascular groups exactly represented",
    
    "No unexpected major groups",
    
    "No missing expected major groups",
    
    "Rank totals = canonical population",
    
    "Major-group record totals = canonical population",
    
    "Major-group family totals = 260",
    
    "Genus-rank records = 823",
    
    "Species-rank records = 8,906",
    
    "Subspecies-rank records = 499",
    
    "Variety-rank records = 1,147",
    
    "Form-rank records = 62",
    
    "Other-rank records = 2",
    
    "Missing-rank records = 0",
    
    "All records have family",
    
    "All records have major group",
    
    "Distinct genus inventory contains no missing genus values",
    
    "Distinct species inventory contains no missing species values",
    
    "Genus representation >= genus-rank record count",
    
    "Species representation is non-zero",
    
    "No database modification performed"
    
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
    
    n_major_groups ==
      4L,
    
    setequal(
      
      observed_major_groups,
      
      EXPECTED_MAJOR_GROUPS
      
    ),
    
    length(
      unexpected_major_groups
    ) ==
      0L,
    
    length(
      missing_major_groups
    ) ==
      0L,
    
    rank_total ==
      EXPECTED_CANONICAL_POPULATION,
    
    record_group_total ==
      EXPECTED_CANONICAL_POPULATION,
    
    family_group_total ==
      EXPECTED_FAMILIES,
    
    n_genus_rank ==
      EXPECTED_GENUS_RANK,
    
    n_species_rank ==
      EXPECTED_SPECIES_RANK,
    
    n_subspecies_rank ==
      EXPECTED_SUBSPECIES_RANK,
    
    n_variety_rank ==
      EXPECTED_VARIETY_RANK,
    
    n_form_rank ==
      EXPECTED_FORM_RANK,
    
    n_other_rank ==
      EXPECTED_OTHER_RANK,
    
    n_missing_rank ==
      0L,
    
    all(
      
      !is.na(
        vpjd$FAMILY
      ) &
        
        vpjd$FAMILY != ""
      
    ),
    
    all(
      
      !is.na(
        vpjd$MAJOR_GROUP
      ) &
        
        vpjd$MAJOR_GROUP != ""
      
    ),
    
    all(
      
      !is.na(
        genus_inventory$GENUS
      ) &
        
        genus_inventory$GENUS != ""
      
    ),
    
    all(
      
      !is.na(
        species_inventory$SPECIES
      ) &
        
        species_inventory$SPECIES != ""
      
    ),
    
    n_distinct_genera >=
      n_genus_rank,
    
    n_distinct_species_values >
      0L,
    
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
# 33. WRITE OUTPUTS
# ==============================================================================

write_csv(
  
  overall_summary,
  
  file.path(
    
    OUTPUT_DIR,
    
    "VPJD_02i_overall_summary.csv"
    
  )
  
)


write_csv(
  
  rank_summary,
  
  file.path(
    
    OUTPUT_DIR,
    
    "VPJD_02i_rank_summary.csv"
    
  )
  
)


write_csv(
  
  rank_by_major_group,
  
  file.path(
    
    OUTPUT_DIR,
    
    "VPJD_02i_rank_by_major_group.csv"
    
  )
  
)


write_csv(
  
  representation_by_major_group,
  
  file.path(
    
    OUTPUT_DIR,
    
    "VPJD_02i_representation_by_major_group.csv"
    
  )
  
)


write_csv(
  
  family_inventory,
  
  file.path(
    
    OUTPUT_DIR,
    
    "VPJD_02i_family_inventory.csv"
    
  )
  
)


write_csv(
  
  genus_inventory,
  
  file.path(
    
    OUTPUT_DIR,
    
    "VPJD_02i_genus_inventory.csv"
    
  )
  
)


write_csv(
  
  species_inventory,
  
  file.path(
    
    OUTPUT_DIR,
    
    "VPJD_02i_species_inventory.csv"
    
  )
  
)


write_csv(
  
  genus_rank_comparison,
  
  file.path(
    
    OUTPUT_DIR,
    
    "VPJD_02i_genus_rank_comparison.csv"
    
  )
  
)


write_csv(
  
  species_rank_comparison,
  
  file.path(
    
    OUTPUT_DIR,
    
    "VPJD_02i_species_rank_comparison.csv"
    
  )
  
)


write_csv(
  
  other_ranks,
  
  file.path(
    
    OUTPUT_DIR,
    
    "VPJD_02i_other_ranks.csv"
    
  )
  
)


write_csv(
  
  missing_taxonomic_fields,
  
  file.path(
    
    OUTPUT_DIR,
    
    "VPJD_02i_missing_taxonomic_fields.csv"
    
  )
  
)


write_csv(
  
  validation,
  
  file.path(
    
    OUTPUT_DIR,
    
    "VPJD_02i_validation.csv"
    
  )
  
)


# ==============================================================================
# 34. OVERALL REPORT
# ==============================================================================

cat("\n")

cat("============================================================\n")
cat("02i TAXONOMIC REPRESENTATION\n")
cat("============================================================\n\n")


print(
  
  overall_summary,
  
  n = Inf
  
)


# ==============================================================================
# 35. MAJOR-GROUP REPORT
# ==============================================================================

cat("\n")

cat("------------------------------------------------------------\n")
cat("REPRESENTATION BY MAJOR VASCULAR GROUP\n")
cat("------------------------------------------------------------\n\n")


print(
  
  representation_by_major_group,
  
  n = Inf,
  
  width = Inf
  
)


# ==============================================================================
# 36. CANONICAL RANK REPORT
# ==============================================================================

cat("\n")

cat("------------------------------------------------------------\n")
cat("CANONICAL RECORDS BY TAXONOMIC RANK\n")
cat("------------------------------------------------------------\n\n")


print(
  
  rank_summary,
  
  n = Inf
  
)


# ==============================================================================
# 37. GENUS REPRESENTATION REPORT
# ==============================================================================

cat("\n")

cat("------------------------------------------------------------\n")
cat("GENUS REPRESENTATION\n")
cat("------------------------------------------------------------\n\n")


print(
  
  genus_rank_comparison,
  
  n = Inf,
  
  width = Inf
  
)


# ==============================================================================
# 38. SPECIES REPRESENTATION REPORT
# ==============================================================================

cat("\n")

cat("------------------------------------------------------------\n")
cat("SPECIES REPRESENTATION\n")
cat("------------------------------------------------------------\n\n")


print(
  
  species_rank_comparison,
  
  n = Inf,
  
  width = Inf
  
)


# ==============================================================================
# 39. VALIDATION REPORT
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
# 40. DECISION
# ==============================================================================

all_valid <-
  all(
    validation$PASS
  )


if (all_valid) {
  
  decision <-
    "TAXONOMIC_REPRESENTATION_VALIDATED"
  
} else {
  
  decision <-
    "REVIEW_REQUIRED"
  
}


# ==============================================================================
# 41. CLEAN DISCONNECT
# ==============================================================================

DBI::dbDisconnect(
  
  con,
  
  shutdown = TRUE
  
)


rm(con)


gc()


# ==============================================================================
# 42. COMPLETION REPORT
# ==============================================================================

cat("\n")

cat("============================================================\n")


cat(
  
  "VPJD TAXONOMIC REVISION 02i v",
  
  VERSION,
  
  " COMPLETE\n",
  
  sep = ""
  
)


cat("============================================================\n\n")


cat(
  "Canonical VPJD records: ",
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
  "Distinct genera represented: ",
  format(
    n_distinct_genera,
    big.mark = ","
  ),
  "\n",
  sep = ""
)


cat(
  "Genus-rank canonical records: ",
  format(
    n_genus_rank,
    big.mark = ","
  ),
  "\n",
  sep = ""
)


cat(
  "Distinct species values represented: ",
  format(
    n_distinct_species_values,
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
  "Subspecies-rank records: ",
  format(
    n_subspecies_rank,
    big.mark = ","
  ),
  "\n",
  sep = ""
)


cat(
  "Variety-rank records: ",
  format(
    n_variety_rank,
    big.mark = ","
  ),
  "\n",
  sep = ""
)


cat(
  "Form-rank records: ",
  format(
    n_form_rank,
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