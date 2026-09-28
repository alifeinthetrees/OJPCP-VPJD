# ==============================================================================
# VPJD — TAXONOMIC REVISION
# 02h — RECONSTRUCT VALIDATED VASCULAR LINEAGE
#
# Purpose:
#
#   Reconstruct a complete, validated vascular-lineage dataset for the
#   Vascular Plants of Japan Database (VPJD).
#
#
# AUTHORITATIVE INPUTS
#
#   1. CANONICAL / STAR AUTHORITY
#
#      vpjd_star_provisional_wholesale_allocation
#
#      Provides:
#        - canonical VPJD population
#        - FINAL_WCVP_ID
#        - recognised-name information held in the canonical allocation
#        - provisional Star allocations
#
#
#   2. TAXONOMIC STRUCTURE
#
#      vpjd_taxrev_02c_rank_audit
#
#      Provides complete 11,439-record taxonomic structure including:
#        - FINAL_WCVP_ID
#        - FINAL_WCVP_RECOGNISED_NAME
#        - FINAL_WCVP_RANK
#        - FAMILY
#        - GENUS
#        - SPECIES
#
#
#   3. VALIDATED FAMILY LINEAGE
#
#      vpjd_taxrev_02b_family_lineage_crosswalk
#
#      Provides:
#        - FAMILY
#        - MAJOR_GROUP
#
#      Validated family coverage:
#        - 260 / 260 families
#
#
# VALIDATED VPJD SCOPE
#
#   ANGIOSPERM
#   GYMNOSPERM
#   FERN
#   LYCOPHYTE
#
#
# EXPECTED POPULATION
#
#   Canonical VPJD records: 11,439
#   Canonical families:       260
#
#
# JOIN ARCHITECTURE
#
#   canonical Star table
#          |
#          | FINAL_WCVP_ID
#          v
#   02c rank audit
#          |
#          | normalised FAMILY
#          v
#   02b family lineage crosswalk
#
#
# SAFETY PRINCIPLES
#
#   This module does NOT:
#
#     - remove taxa
#     - add taxa
#     - alter WCVP IDs
#     - alter recognised taxonomic names
#     - alter taxonomic ranks
#     - alter provisional Star allocations
#     - infer missing family classifications
#     - infer missing major groups
#
#   The output table is written ONLY after all validation checks pass.
#
#
# OUTPUT DUCKDB TABLE
#
#   vpjd_taxrev_02h_validated_vascular_lineage
#
#
# CSV OUTPUTS
#
#   VPJD_02h_validated_vascular_lineage.csv
#   VPJD_02h_family_join_audit.csv
#   VPJD_02h_major_group_summary.csv
#   VPJD_02h_rank_by_major_group.csv
#   VPJD_02h_source_field_comparison.csv
#   VPJD_02h_unmatched_wcvp_ids.csv
#   VPJD_02h_unmatched_families.csv
#   VPJD_02h_unmatched_records.csv
#   VPJD_02h_validation.csv
#   VPJD_02h_summary.csv
#
# ==============================================================================


# ==============================================================================
# 01. VERSION
# ==============================================================================

VERSION <- "0.2.0"


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

EXPECTED_CANONICAL_FAMILIES <- 260L


EXPECTED_MAJOR_GROUPS <- c(
  
  "ANGIOSPERM",
  "GYMNOSPERM",
  "FERN",
  "LYCOPHYTE"
  
)


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
  "02h_validated_vascular_lineage"
  
)


dir.create(
  
  OUTPUT_DIR,
  
  recursive = TRUE,
  
  showWarnings = FALSE
  
)


# ==============================================================================
# 04. SOURCE TABLES
# ==============================================================================

CANONICAL_TABLE <-
  "vpjd_star_provisional_wholesale_allocation"


TAXONOMY_TABLE <-
  "vpjd_taxrev_02c_rank_audit"


CROSSWALK_TABLE <-
  "vpjd_taxrev_02b_family_lineage_crosswalk"


OUTPUT_TABLE <-
  "vpjd_taxrev_02h_validated_vascular_lineage"


# ==============================================================================
# 05. START REPORT
# ==============================================================================

cat("\n")

cat("============================================================\n")

cat(
  "VPJD TAXONOMIC REVISION 02h v",
  VERSION,
  "\n",
  sep = ""
)

cat("RECONSTRUCT VALIDATED VASCULAR LINEAGE\n")

cat("============================================================\n\n")


cat(
  "Database:\n",
  DB_PATH,
  "\n\n",
  sep = ""
)


cat(
  "Canonical / Star authority:\n",
  CANONICAL_TABLE,
  "\n\n",
  sep = ""
)


cat(
  "Taxonomic structure source:\n",
  TAXONOMY_TABLE,
  "\n\n",
  sep = ""
)


cat(
  "Family-lineage crosswalk:\n",
  CROSSWALK_TABLE,
  "\n\n",
  sep = ""
)


# ==============================================================================
# 06. CHECK DATABASE EXISTS
# ==============================================================================

if (!file.exists(DB_PATH)) {
  
  stop(
    "DuckDB database not found:\n",
    DB_PATH
  )
  
}


# ==============================================================================
# 07. CONNECT — WRITE ENABLED
# ==============================================================================

con <- DBI::dbConnect(
  
  duckdb::duckdb(),
  
  dbdir = DB_PATH,
  
  read_only = FALSE
  
)


# ==============================================================================
# 08. SAFE DISCONNECT FUNCTION
# ==============================================================================

disconnect_safely <- function() {
  
  if (
    exists("con", inherits = TRUE) &&
    DBI::dbIsValid(con)
  ) {
    
    DBI::dbDisconnect(
      
      con,
      
      shutdown = TRUE
      
    )
    
  }
  
}


# ==============================================================================
# 09. DATABASE TABLE INVENTORY
# ==============================================================================

db_tables <-
  DBI::dbListTables(con)


required_tables <- c(
  
  CANONICAL_TABLE,
  TAXONOMY_TABLE,
  CROSSWALK_TABLE
  
)


missing_tables <-
  setdiff(
    
    required_tables,
    
    db_tables
    
  )


if (
  length(missing_tables) > 0L
) {
  
  disconnect_safely()
  
  
  stop(
    
    "Required VPJD table(s) missing:\n",
    
    paste(
      missing_tables,
      collapse = "\n"
    )
    
  )
  
}


cat("All required source tables found.\n\n")


# ==============================================================================
# 10. LOAD CANONICAL / STAR TABLE
# ==============================================================================

cat(
  "Loading canonical / Star allocation table...\n"
)


canonical <-
  DBI::dbReadTable(
    
    con,
    
    CANONICAL_TABLE
    
  ) |>
  
  as_tibble()


cat(
  "Canonical records loaded: ",
  format(
    nrow(canonical),
    big.mark = ","
  ),
  "\n",
  sep = ""
)


# ==============================================================================
# 11. LOAD TAXONOMIC STRUCTURE
# ==============================================================================

cat(
  "Loading 02c taxonomic structure...\n"
)


taxonomy <-
  DBI::dbReadTable(
    
    con,
    
    TAXONOMY_TABLE
    
  ) |>
  
  as_tibble()


cat(
  "Taxonomic records loaded: ",
  format(
    nrow(taxonomy),
    big.mark = ","
  ),
  "\n",
  sep = ""
)


# ==============================================================================
# 12. LOAD FAMILY-LINEAGE CROSSWALK
# ==============================================================================

cat(
  "Loading 02b family-lineage crosswalk...\n"
)


crosswalk <-
  DBI::dbReadTable(
    
    con,
    
    CROSSWALK_TABLE
    
  ) |>
  
  as_tibble()


cat(
  "Crosswalk rows loaded: ",
  format(
    nrow(crosswalk),
    big.mark = ","
  ),
  "\n\n",
  sep = ""
)


# ==============================================================================
# 13. CHECK CANONICAL REQUIRED FIELDS
# ==============================================================================

canonical_required_fields <- c(
  
  "FINAL_WCVP_ID"
  
)


missing_canonical_fields <-
  setdiff(
    
    canonical_required_fields,
    
    names(canonical)
    
  )


if (
  length(missing_canonical_fields) > 0L
) {
  
  disconnect_safely()
  
  
  stop(
    
    "Required canonical field(s) missing:\n",
    
    paste(
      missing_canonical_fields,
      collapse = "\n"
    )
    
  )
  
}


# ==============================================================================
# 14. CHECK TAXONOMY REQUIRED FIELDS
# ==============================================================================

taxonomy_required_fields <- c(
  
  "FINAL_WCVP_ID",
  
  "FINAL_WCVP_RECOGNISED_NAME",
  
  "FINAL_WCVP_RANK",
  
  "FAMILY",
  
  "GENUS",
  
  "SPECIES"
  
)


missing_taxonomy_fields <-
  setdiff(
    
    taxonomy_required_fields,
    
    names(taxonomy)
    
  )


if (
  length(missing_taxonomy_fields) > 0L
) {
  
  disconnect_safely()
  
  
  stop(
    
    "Required 02c taxonomy field(s) missing:\n",
    
    paste(
      missing_taxonomy_fields,
      collapse = "\n"
    )
    
  )
  
}


# ==============================================================================
# 15. CHECK CROSSWALK REQUIRED FIELDS
# ==============================================================================

crosswalk_required_fields <- c(
  
  "FAMILY",
  
  "MAJOR_GROUP"
  
)


missing_crosswalk_fields <-
  setdiff(
    
    crosswalk_required_fields,
    
    names(crosswalk)
    
  )


if (
  length(missing_crosswalk_fields) > 0L
) {
  
  disconnect_safely()
  
  
  stop(
    
    "Required 02b crosswalk field(s) missing:\n",
    
    paste(
      missing_crosswalk_fields,
      collapse = "\n"
    )
    
  )
  
}


# ==============================================================================
# 16. STANDARDISE WCVP IDS
# ==============================================================================

canonical <- canonical |>
  
  mutate(
    
    FINAL_WCVP_ID =
      as.character(
        FINAL_WCVP_ID
      )
    
  )


taxonomy <- taxonomy |>
  
  mutate(
    
    FINAL_WCVP_ID =
      as.character(
        FINAL_WCVP_ID
      )
    
  )


# ==============================================================================
# 17. STANDARDISE TAXONOMIC FIELDS
# ==============================================================================

taxonomy <- taxonomy |>
  
  mutate(
    
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
      )
    
  )


crosswalk <- crosswalk |>
  
  mutate(
    
    FAMILY =
      str_squish(
        as.character(
          FAMILY
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
# 18. VALIDATE SOURCE POPULATIONS
# ==============================================================================

if (
  nrow(canonical) !=
  EXPECTED_CANONICAL_POPULATION
) {
  
  disconnect_safely()
  
  
  stop(
    
    "Canonical population = ",
    nrow(canonical),
    "; expected ",
    EXPECTED_CANONICAL_POPULATION,
    "."
    
  )
  
}


if (
  nrow(taxonomy) !=
  EXPECTED_CANONICAL_POPULATION
) {
  
  disconnect_safely()
  
  
  stop(
    
    "02c taxonomy population = ",
    nrow(taxonomy),
    "; expected ",
    EXPECTED_CANONICAL_POPULATION,
    "."
    
  )
  
}


# ==============================================================================
# 19. VALIDATE WCVP ID UNIQUENESS
# ==============================================================================

n_canonical_ids <-
  n_distinct(
    canonical$FINAL_WCVP_ID
  )


n_taxonomy_ids <-
  n_distinct(
    taxonomy$FINAL_WCVP_ID
  )


if (
  n_canonical_ids !=
  EXPECTED_CANONICAL_POPULATION
) {
  
  disconnect_safely()
  
  
  stop(
    "Canonical table is not unique by FINAL_WCVP_ID."
  )
  
}


if (
  n_taxonomy_ids !=
  EXPECTED_CANONICAL_POPULATION
) {
  
  disconnect_safely()
  
  
  stop(
    "02c taxonomy table is not unique by FINAL_WCVP_ID."
  )
  
}


# ==============================================================================
# 20. COMPARE CANONICAL AND TAXONOMY ID POPULATIONS
# ==============================================================================

canonical_ids <-
  sort(
    canonical$FINAL_WCVP_ID
  )


taxonomy_ids <-
  sort(
    taxonomy$FINAL_WCVP_ID
  )


canonical_missing_from_taxonomy <-
  setdiff(
    
    canonical_ids,
    
    taxonomy_ids
    
  )


taxonomy_missing_from_canonical <-
  setdiff(
    
    taxonomy_ids,
    
    canonical_ids
    
  )


unmatched_wcvp_ids <- bind_rows(
  
  tibble(
    
    FINAL_WCVP_ID =
      canonical_missing_from_taxonomy,
    
    STATUS =
      "CANONICAL_NOT_IN_02C"
    
  ),
  
  tibble(
    
    FINAL_WCVP_ID =
      taxonomy_missing_from_canonical,
    
    STATUS =
      "02C_NOT_IN_CANONICAL"
    
  )
  
)


write_csv(
  
  unmatched_wcvp_ids,
  
  file.path(
    
    OUTPUT_DIR,
    
    "VPJD_02h_unmatched_wcvp_ids.csv"
    
  )
  
)


if (
  nrow(unmatched_wcvp_ids) > 0L
) {
  
  disconnect_safely()
  
  
  stop(
    
    "Canonical and 02c FINAL_WCVP_ID populations differ. ",
    "See VPJD_02h_unmatched_wcvp_ids.csv."
    
  )
  
}


cat(
  "Canonical ↔ 02c WCVP ID match: 11,439 / 11,439\n\n"
)


# ==============================================================================
# 21. COMPARE OVERLAPPING CANONICAL FIELDS
# ==============================================================================

# We do not assume that every field in the Star table is also present in 02c.
#
# Only genuinely overlapping fields are compared.
#
# FINAL_WCVP_ID is excluded because it is the join key.


overlap_fields <- intersect(
  
  names(canonical),
  
  names(taxonomy)
  
)


overlap_fields <- setdiff(
  
  overlap_fields,
  
  "FINAL_WCVP_ID"
  
)


cat(
  "Overlapping canonical/02c fields available for comparison: ",
  length(overlap_fields),
  "\n",
  sep = ""
)


# ==============================================================================
# 22. SOURCE FIELD COMPARISON FUNCTION
# ==============================================================================

compare_field <- function(field_name) {
  
  canonical_values <- canonical |>
    
    select(
      
      FINAL_WCVP_ID,
      
      CANONICAL_VALUE =
        all_of(
          field_name
        )
      
    ) |>
    
    mutate(
      
      CANONICAL_VALUE =
        str_squish(
          as.character(
            CANONICAL_VALUE
          )
        )
      
    )
  
  
  taxonomy_values <- taxonomy |>
    
    select(
      
      FINAL_WCVP_ID,
      
      TAXONOMY_VALUE =
        all_of(
          field_name
        )
      
    ) |>
    
    mutate(
      
      TAXONOMY_VALUE =
        str_squish(
          as.character(
            TAXONOMY_VALUE
          )
        )
      
    )
  
  
  comparison <- canonical_values |>
    
    left_join(
      
      taxonomy_values,
      
      by =
        "FINAL_WCVP_ID"
      
    ) |>
    
    mutate(
      
      BOTH_MISSING =
        
        (
          is.na(CANONICAL_VALUE) |
            CANONICAL_VALUE == ""
        ) &
        
        (
          is.na(TAXONOMY_VALUE) |
            TAXONOMY_VALUE == ""
        ),
      
      
      MATCH =
        
        BOTH_MISSING |
        
        (
          !is.na(CANONICAL_VALUE) &
            !is.na(TAXONOMY_VALUE) &
            CANONICAL_VALUE ==
            TAXONOMY_VALUE
        )
      
    )
  
  
  tibble(
    
    FIELD_NAME =
      field_name,
    
    N_RECORDS =
      nrow(comparison),
    
    N_MATCH =
      sum(
        comparison$MATCH
      ),
    
    N_DIFFERENT =
      sum(
        !comparison$MATCH
      ),
    
    PERCENT_MATCH =
      100 *
      sum(
        comparison$MATCH
      ) /
      nrow(comparison)
    
  )
  
}


# ==============================================================================
# 23. RUN SOURCE FIELD COMPARISON
# ==============================================================================

if (
  length(overlap_fields) > 0L
) {
  
  source_field_comparison <-
    map_dfr(
      
      overlap_fields,
      
      compare_field
      
    )
  
} else {
  
  source_field_comparison <- tibble(
    
    FIELD_NAME =
      character(),
    
    N_RECORDS =
      integer(),
    
    N_MATCH =
      integer(),
    
    N_DIFFERENT =
      integer(),
    
    PERCENT_MATCH =
      double()
    
  )
  
}


write_csv(
  
  source_field_comparison,
  
  file.path(
    
    OUTPUT_DIR,
    
    "VPJD_02h_source_field_comparison.csv"
    
  )
  
)


cat("\n")
cat("------------------------------------------------------------\n")
cat("CANONICAL ↔ 02c OVERLAPPING FIELD COMPARISON\n")
cat("------------------------------------------------------------\n\n")


print(
  
  source_field_comparison,
  
  n = Inf
  
)


# ==============================================================================
# 24. CHECK CORE TAXONOMIC FIELDS IF THEY OVERLAP
# ==============================================================================

core_taxonomic_fields <- c(
  
  "FINAL_WCVP_RECOGNISED_NAME",
  
  "FINAL_WCVP_RANK"
  
)


core_overlap <- intersect(
  
  core_taxonomic_fields,
  
  source_field_comparison$FIELD_NAME
  
)


core_field_conflicts <- source_field_comparison |>
  
  filter(
    
    FIELD_NAME %in%
      core_overlap,
    
    N_DIFFERENT > 0L
    
  )


if (
  nrow(core_field_conflicts) > 0L
) {
  
  disconnect_safely()
  
  
  stop(
    
    "Canonical and 02c disagree on one or more core taxonomic fields. ",
    "Review VPJD_02h_source_field_comparison.csv before proceeding."
    
  )
  
}


# ==============================================================================
# 25. NORMALISE FAMILY FUNCTION
# ==============================================================================

normalise_family <- function(x) {
  
  x |>
    
    as.character() |>
    
    str_squish() |>
    
    str_to_upper() |>
    
    str_replace_all(
      
      "[^A-Z0-9]",
      
      ""
      
    )
  
}


# ==============================================================================
# 26. PREPARE TAXONOMY FAMILY KEYS
# ==============================================================================

taxonomy <- taxonomy |>
  
  mutate(
    
    FAMILY_JOIN_KEY =
      normalise_family(
        FAMILY
      )
    
  )


crosswalk <- crosswalk |>
  
  mutate(
    
    FAMILY_JOIN_KEY =
      normalise_family(
        FAMILY
      )
    
  )


# ==============================================================================
# 27. VALIDATE CANONICAL FAMILY POPULATION
# ==============================================================================

taxonomy_family_inventory <- taxonomy |>
  
  filter(
    
    !is.na(FAMILY),
    
    FAMILY != ""
    
  ) |>
  
  distinct(
    
    FAMILY_JOIN_KEY,
    
    FAMILY
    
  )


n_taxonomy_families <-
  nrow(
    taxonomy_family_inventory
  )


cat("\n")

cat(
  "02c canonical families: ",
  n_taxonomy_families,
  "\n",
  sep = ""
)


if (
  n_taxonomy_families !=
  EXPECTED_CANONICAL_FAMILIES
) {
  
  disconnect_safely()
  
  
  stop(
    
    "02c family population = ",
    n_taxonomy_families,
    "; expected ",
    EXPECTED_CANONICAL_FAMILIES,
    "."
    
  )
  
}


# ==============================================================================
# 28. PREPARE CROSSWALK FAMILY INVENTORY
# ==============================================================================

crosswalk_family_inventory <- crosswalk |>
  
  filter(
    
    !is.na(FAMILY_JOIN_KEY),
    
    FAMILY_JOIN_KEY != "",
    
    !is.na(MAJOR_GROUP),
    
    MAJOR_GROUP != ""
    
  ) |>
  
  distinct(
    
    FAMILY_JOIN_KEY,
    
    FAMILY,
    
    MAJOR_GROUP
    
  )


# ==============================================================================
# 29. CHECK CONTRADICTORY FAMILY CLASSIFICATIONS
# ==============================================================================

crosswalk_family_check <- crosswalk_family_inventory |>
  
  group_by(
    FAMILY_JOIN_KEY
  ) |>
  
  summarise(
    
    N_FAMILY_NAMES =
      n_distinct(
        FAMILY
      ),
    
    N_MAJOR_GROUPS =
      n_distinct(
        MAJOR_GROUP
      ),
    
    .groups =
      "drop"
    
  )


contradictory_families <-
  crosswalk_family_check |>
  
  filter(
    
    N_MAJOR_GROUPS > 1L
    
  )


n_contradictory_families <-
  nrow(
    contradictory_families
  )


if (
  n_contradictory_families > 0L
) {
  
  disconnect_safely()
  
  
  stop(
    
    "02b family crosswalk contains contradictory MAJOR_GROUP assignments."
    
  )
  
}


# ==============================================================================
# 30. COLLAPSE CROSSWALK TO ONE ROW PER NORMALISED FAMILY
# ==============================================================================

crosswalk_family_unique <- crosswalk_family_inventory |>
  
  group_by(
    FAMILY_JOIN_KEY
  ) |>
  
  summarise(
    
    CROSSWALK_FAMILY =
      first(
        FAMILY
      ),
    
    MAJOR_GROUP =
      first(
        MAJOR_GROUP
      ),
    
    .groups =
      "drop"
    
  )


n_crosswalk_families <-
  nrow(
    crosswalk_family_unique
  )


cat(
  "02b crosswalk families: ",
  n_crosswalk_families,
  "\n",
  sep = ""
)


if (
  n_crosswalk_families !=
  EXPECTED_CANONICAL_FAMILIES
) {
  
  disconnect_safely()
  
  
  stop(
    
    "02b crosswalk family population = ",
    n_crosswalk_families,
    "; expected ",
    EXPECTED_CANONICAL_FAMILIES,
    "."
    
  )
  
}


# ==============================================================================
# 31. VALIDATE CROSSWALK MAJOR GROUPS
# ==============================================================================

observed_crosswalk_groups <-
  sort(
    unique(
      crosswalk_family_unique$MAJOR_GROUP
    )
  )


unexpected_crosswalk_groups <-
  setdiff(
    
    observed_crosswalk_groups,
    
    EXPECTED_MAJOR_GROUPS
    
  )


missing_expected_groups <-
  setdiff(
    
    EXPECTED_MAJOR_GROUPS,
    
    observed_crosswalk_groups
    
  )


if (
  length(unexpected_crosswalk_groups) > 0L
) {
  
  disconnect_safely()
  
  
  stop(
    
    "Unexpected MAJOR_GROUP value(s) in 02b crosswalk:\n",
    
    paste(
      unexpected_crosswalk_groups,
      collapse = "\n"
    )
    
  )
  
}


if (
  length(missing_expected_groups) > 0L
) {
  
  disconnect_safely()
  
  
  stop(
    
    "Expected vascular MAJOR_GROUP value(s) absent from 02b crosswalk:\n",
    
    paste(
      missing_expected_groups,
      collapse = "\n"
    )
    
  )
  
}


# ==============================================================================
# 32. FAMILY JOIN AUDIT
# ==============================================================================

family_join_audit <- taxonomy_family_inventory |>
  
  left_join(
    
    crosswalk_family_unique,
    
    by =
      "FAMILY_JOIN_KEY"
    
  ) |>
  
  mutate(
    
    MATCHED =
      !is.na(
        MAJOR_GROUP
      )
    
  )


n_matched_families <-
  sum(
    family_join_audit$MATCHED
  )


unmatched_families <- family_join_audit |>
  
  filter(
    !MATCHED
  )


write_csv(
  
  family_join_audit,
  
  file.path(
    
    OUTPUT_DIR,
    
    "VPJD_02h_family_join_audit.csv"
    
  )
  
)


write_csv(
  
  unmatched_families,
  
  file.path(
    
    OUTPUT_DIR,
    
    "VPJD_02h_unmatched_families.csv"
    
  )
  
)


cat("\n")

cat(
  "Normalised family matches: ",
  n_matched_families,
  " / ",
  EXPECTED_CANONICAL_FAMILIES,
  "\n",
  sep = ""
)


if (
  n_matched_families !=
  EXPECTED_CANONICAL_FAMILIES
) {
  
  disconnect_safely()
  
  
  stop(
    
    "02h stopped safely: only ",
    n_matched_families,
    " / ",
    EXPECTED_CANONICAL_FAMILIES,
    " families matched the validated 02b crosswalk."
    
  )
  
}


# ==============================================================================
# 33. PREPARE TAXONOMIC STRUCTURE FOR CANONICAL JOIN
# ==============================================================================

# Only fields required for the reconstructed taxonomic structure are selected.
#
# This prevents unrelated audit fields from 02c being accidentally propagated.


taxonomy_for_join <- taxonomy |>
  
  select(
    
    FINAL_WCVP_ID,
    
    TAX_FINAL_WCVP_RECOGNISED_NAME =
      FINAL_WCVP_RECOGNISED_NAME,
    
    TAX_FINAL_WCVP_RANK =
      FINAL_WCVP_RANK,
    
    FAMILY,
    
    GENUS,
    
    SPECIES,
    
    FAMILY_JOIN_KEY
    
  )


# ==============================================================================
# 34. JOIN TAXONOMIC STRUCTURE TO CANONICAL POPULATION
# ==============================================================================

cat("\n")

cat(
  "Joining 02c taxonomic structure to canonical Star population...\n"
)


reconstructed <- canonical |>
  
  left_join(
    
    taxonomy_for_join,
    
    by =
      "FINAL_WCVP_ID"
    
  )


if (
  nrow(reconstructed) !=
  EXPECTED_CANONICAL_POPULATION
) {
  
  disconnect_safely()
  
  
  stop(
    
    "Population changed during canonical ↔ 02c join: ",
    nrow(reconstructed),
    " rows returned."
    
  )
  
}


# ==============================================================================
# 35. ATTACH VALIDATED FAMILY LINEAGE
# ==============================================================================

cat(
  "Attaching validated 02b family lineage...\n"
)


reconstructed <- reconstructed |>
  
  left_join(
    
    crosswalk_family_unique,
    
    by =
      "FAMILY_JOIN_KEY"
    
  ) |>
  
  mutate(
    
    VPJD_SCOPE =
      case_when(
        
        MAJOR_GROUP %in%
          EXPECTED_MAJOR_GROUPS ~
          "RETAIN",
        
        TRUE ~
          "REVIEW"
        
      )
    
  )


# ==============================================================================
# 36. CHECK RECORD-LEVEL LINEAGE COVERAGE
# ==============================================================================

unmatched_records <- reconstructed |>
  
  filter(
    
    is.na(FAMILY) |
      
      FAMILY == "" |
      
      is.na(MAJOR_GROUP) |
      
      !MAJOR_GROUP %in%
      EXPECTED_MAJOR_GROUPS |
      
      VPJD_SCOPE !=
      "RETAIN"
    
  )


n_unmatched_records <-
  nrow(
    unmatched_records
  )


write_csv(
  
  unmatched_records,
  
  file.path(
    
    OUTPUT_DIR,
    
    "VPJD_02h_unmatched_records.csv"
    
  )
  
)


# ==============================================================================
# 37. RESOLVE AUTHORITATIVE DISPLAY FIELDS
# ==============================================================================

# If the canonical Star table already contains recognised-name and/or rank
# fields, those values have already been checked against 02c above.
#
# We therefore retain canonical values where present and use the validated
# 02c fields where the canonical table does not contain them.


if (
  "FINAL_WCVP_RECOGNISED_NAME" %in%
  names(reconstructed)
) {
  
  reconstructed <-
    reconstructed |>
    
    mutate(
      
      FINAL_WCVP_RECOGNISED_NAME =
        as.character(
          FINAL_WCVP_RECOGNISED_NAME
        )
      
    )
  
} else {
  
  reconstructed <-
    reconstructed |>
    
    mutate(
      
      FINAL_WCVP_RECOGNISED_NAME =
        TAX_FINAL_WCVP_RECOGNISED_NAME
      
    )
  
}


if (
  "FINAL_WCVP_RANK" %in%
  names(reconstructed)
) {
  
  reconstructed <-
    reconstructed |>
    
    mutate(
      
      FINAL_WCVP_RANK =
        as.character(
          FINAL_WCVP_RANK
        )
      
    )
  
} else {
  
  reconstructed <-
    reconstructed |>
    
    mutate(
      
      FINAL_WCVP_RANK =
        TAX_FINAL_WCVP_RANK
      
    )
  
}


# ==============================================================================
# 38. REMOVE TEMPORARY TAXONOMY DUPLICATES
# ==============================================================================

reconstructed <- reconstructed |>
  
  select(
    
    -TAX_FINAL_WCVP_RECOGNISED_NAME,
    
    -TAX_FINAL_WCVP_RANK
    
  )


# ==============================================================================
# 39. VALIDATE RECONSTRUCTED POPULATION
# ==============================================================================

n_reconstructed <-
  nrow(
    reconstructed
  )


n_reconstructed_ids <-
  n_distinct(
    reconstructed$FINAL_WCVP_ID
  )


reconstructed_ids <-
  sort(
    reconstructed$FINAL_WCVP_ID
  )


ids_unchanged <-
  identical(
    
    canonical_ids,
    
    reconstructed_ids
    
  )


# ==============================================================================
# 40. MAJOR-GROUP SUMMARY
# ==============================================================================

major_group_summary <- reconstructed |>
  
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
        GENUS[
          !is.na(GENUS) &
            GENUS != ""
        ]
      ),
    
    N_SPECIES_VALUES =
      n_distinct(
        SPECIES[
          !is.na(SPECIES) &
            SPECIES != ""
        ]
      ),
    
    PERCENT_RECORDS =
      100 *
      N_RECORDS /
      EXPECTED_CANONICAL_POPULATION,
    
    .groups =
      "drop"
    
  ) |>
  
  arrange(
    desc(
      N_RECORDS
    )
  )


cat("\n")
cat("------------------------------------------------------------\n")
cat("MAJOR-GROUP SUMMARY\n")
cat("------------------------------------------------------------\n\n")


print(
  
  major_group_summary,
  
  n = Inf,
  
  width = Inf
  
)


# ==============================================================================
# 41. RANK × MAJOR-GROUP SUMMARY
# ==============================================================================

rank_by_major_group <- reconstructed |>
  
  mutate(
    
    FINAL_WCVP_RANK =
      str_squish(
        as.character(
          FINAL_WCVP_RANK
        )
      )
    
  ) |>
  
  count(
    
    MAJOR_GROUP,
    
    FINAL_WCVP_RANK,
    
    name =
      "N_RECORDS"
    
  ) |>
  
  arrange(
    
    MAJOR_GROUP,
    
    desc(
      N_RECORDS
    )
    
  )


# ==============================================================================
# 42. FAMILY COUNTS BY MAJOR GROUP
# ==============================================================================

family_group_summary <- family_join_audit |>
  
  count(
    
    MAJOR_GROUP,
    
    name =
      "N_FAMILIES"
    
  ) |>
  
  arrange(
    desc(
      N_FAMILIES
    )
  )


# ==============================================================================
# 43. STAR FIELD INTEGRITY
# ==============================================================================

star_fields <- names(canonical) [
  
  grepl(
    
    "STAR",
    
    names(canonical),
    
    ignore.case = TRUE
    
  )
  
]


star_allocations_unchanged <- TRUE


if (
  length(star_fields) > 0L
) {
  
  star_before <- canonical |>
    
    select(
      
      FINAL_WCVP_ID,
      
      all_of(
        star_fields
      )
      
    ) |>
    
    arrange(
      FINAL_WCVP_ID
    )
  
  
  star_after <- reconstructed |>
    
    select(
      
      FINAL_WCVP_ID,
      
      all_of(
        star_fields
      )
      
    ) |>
    
    arrange(
      FINAL_WCVP_ID
    )
  
  
  star_allocations_unchanged <-
    identical(
      
      star_before,
      
      star_after
      
    )
  
}


# ==============================================================================
# 44. CANONICAL COLUMN INTEGRITY
# ==============================================================================

# Every original canonical field should survive the reconstruction unchanged.


canonical_integrity_fields <-
  names(canonical)


canonical_integrity_before <- canonical |>
  
  arrange(
    FINAL_WCVP_ID
  ) |>
  
  select(
    all_of(
      canonical_integrity_fields
    )
  )


canonical_integrity_after <- reconstructed |>
  
  arrange(
    FINAL_WCVP_ID
  ) |>
  
  select(
    all_of(
      canonical_integrity_fields
    )
  )


canonical_fields_unchanged <-
  identical(
    
    canonical_integrity_before,
    
    canonical_integrity_after
    
  )


# ==============================================================================
# 45. EXPECTED FAMILY DISTRIBUTION
# ==============================================================================

n_angiosperm_families <-
  family_group_summary |>
  
  filter(
    MAJOR_GROUP == "ANGIOSPERM"
  ) |>
  
  pull(
    N_FAMILIES
  )


n_gymnosperm_families <-
  family_group_summary |>
  
  filter(
    MAJOR_GROUP == "GYMNOSPERM"
  ) |>
  
  pull(
    N_FAMILIES
  )


n_fern_families <-
  family_group_summary |>
  
  filter(
    MAJOR_GROUP == "FERN"
  ) |>
  
  pull(
    N_FAMILIES
  )


n_lycophyte_families <-
  family_group_summary |>
  
  filter(
    MAJOR_GROUP == "LYCOPHYTE"
  ) |>
  
  pull(
    N_FAMILIES
  )


# ==============================================================================
# 46. VALIDATION
# ==============================================================================

validation <- tibble(
  
  CHECK = c(
    
    "Canonical Star table present",
    
    "02c taxonomic structure table present",
    
    "02b family lineage crosswalk present",
    
    "Canonical population = 11,439",
    
    "02c taxonomy population = 11,439",
    
    "Canonical population unique by WCVP ID",
    
    "02c population unique by WCVP ID",
    
    "Canonical and 02c WCVP ID populations identical",
    
    "02c family population = 260",
    
    "02b crosswalk family population = 260",
    
    "Normalised family join = 260 / 260",
    
    "No unmatched canonical families",
    
    "No contradictory family classifications",
    
    "Exactly four expected vascular groups in crosswalk",
    
    "No unexpected vascular groups in crosswalk",
    
    "Reconstructed population = 11,439",
    
    "Reconstructed population unique by WCVP ID",
    
    "Canonical WCVP IDs unchanged",
    
    "No unmatched reconstructed records",
    
    "Every record assigned a major vascular group",
    
    "Exactly four major vascular groups represented",
    
    "All records assigned VPJD_SCOPE = RETAIN",
    
    "Major-group record totals = 11,439",
    
    "Major-group family totals = 260",
    
    "Angiosperm families = 228",
    
    "Gymnosperm families = 11",
    
    "Fern families = 18",
    
    "Lycophyte families = 3",
    
    "Original canonical fields unchanged",
    
    "Provisional Star allocations unchanged",
    
    "No canonical taxa removed",
    
    "No canonical taxa added"
    
  ),
  
  
  PASS = c(
    
    CANONICAL_TABLE %in%
      db_tables,
    
    TAXONOMY_TABLE %in%
      db_tables,
    
    CROSSWALK_TABLE %in%
      db_tables,
    
    nrow(canonical) ==
      EXPECTED_CANONICAL_POPULATION,
    
    nrow(taxonomy) ==
      EXPECTED_CANONICAL_POPULATION,
    
    n_canonical_ids ==
      EXPECTED_CANONICAL_POPULATION,
    
    n_taxonomy_ids ==
      EXPECTED_CANONICAL_POPULATION,
    
    nrow(unmatched_wcvp_ids) ==
      0L,
    
    n_taxonomy_families ==
      EXPECTED_CANONICAL_FAMILIES,
    
    n_crosswalk_families ==
      EXPECTED_CANONICAL_FAMILIES,
    
    n_matched_families ==
      EXPECTED_CANONICAL_FAMILIES,
    
    nrow(unmatched_families) ==
      0L,
    
    n_contradictory_families ==
      0L,
    
    setequal(
      
      observed_crosswalk_groups,
      
      EXPECTED_MAJOR_GROUPS
      
    ),
    
    length(
      unexpected_crosswalk_groups
    ) ==
      0L,
    
    n_reconstructed ==
      EXPECTED_CANONICAL_POPULATION,
    
    n_reconstructed_ids ==
      EXPECTED_CANONICAL_POPULATION,
    
    ids_unchanged,
    
    n_unmatched_records ==
      0L,
    
    all(
      reconstructed$MAJOR_GROUP %in%
        EXPECTED_MAJOR_GROUPS
    ),
    
    n_distinct(
      reconstructed$MAJOR_GROUP
    ) ==
      4L,
    
    all(
      reconstructed$VPJD_SCOPE ==
        "RETAIN"
    ),
    
    sum(
      major_group_summary$N_RECORDS
    ) ==
      EXPECTED_CANONICAL_POPULATION,
    
    sum(
      major_group_summary$N_FAMILIES
    ) ==
      EXPECTED_CANONICAL_FAMILIES,
    
    length(n_angiosperm_families) == 1L &&
      n_angiosperm_families == 228L,
    
    length(n_gymnosperm_families) == 1L &&
      n_gymnosperm_families == 11L,
    
    length(n_fern_families) == 1L &&
      n_fern_families == 18L,
    
    length(n_lycophyte_families) == 1L &&
      n_lycophyte_families == 3L,
    
    canonical_fields_unchanged,
    
    star_allocations_unchanged,
    
    length(
      setdiff(
        canonical_ids,
        reconstructed_ids
      )
    ) ==
      0L,
    
    length(
      setdiff(
        reconstructed_ids,
        canonical_ids
      )
    ) ==
      0L
    
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
# 47. WRITE VALIDATION BEFORE DATABASE MODIFICATION
# ==============================================================================

write_csv(
  
  validation,
  
  file.path(
    
    OUTPUT_DIR,
    
    "VPJD_02h_validation.csv"
    
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
# 48. STOP SAFELY IF VALIDATION FAILS
# ==============================================================================

if (
  !all(
    validation$PASS
  )
) {
  
  cat("\n")
  
  cat(
    "02h VALIDATION FAILED — OUTPUT TABLE WILL NOT BE WRITTEN.\n"
  )
  
  
  disconnect_safely()
  
  
  stop(
    
    "02h stopped safely because one or more validation checks failed. ",
    "Review VPJD_02h_validation.csv."
    
  )
  
}


# ==============================================================================
# 49. REMOVE TEMPORARY JOIN FIELDS
# ==============================================================================

validated_lineage_output <- reconstructed |>
  
  select(
    
    -FAMILY_JOIN_KEY
    
  )


# ==============================================================================
# 50. ORDER KEY OUTPUT FIELDS
# ==============================================================================

# Key taxonomic/lineage fields are moved toward the front while all original
# canonical fields remain present.


front_fields <- intersect(
  
  c(
    
    "FINAL_WCVP_ID",
    
    "FINAL_WCVP_RECOGNISED_NAME",
    
    "FINAL_WCVP_RANK",
    
    "FAMILY",
    
    "GENUS",
    
    "SPECIES",
    
    "MAJOR_GROUP",
    
    "VPJD_SCOPE"
    
  ),
  
  names(
    validated_lineage_output
  )
  
)


remaining_fields <- setdiff(
  
  names(
    validated_lineage_output
  ),
  
  front_fields
  
)


validated_lineage_output <-
  validated_lineage_output |>
  
  select(
    
    all_of(
      front_fields
    ),
    
    all_of(
      remaining_fields
    )
    
  )


# ==============================================================================
# 51. WRITE CSV DATASET FIRST
# ==============================================================================

cat("\n")

cat(
  "All validation checks passed.\n"
)

cat(
  "Writing validated 02h CSV dataset...\n"
)


write_csv(
  
  validated_lineage_output,
  
  file.path(
    
    OUTPUT_DIR,
    
    "VPJD_02h_validated_vascular_lineage.csv"
    
  )
  
)


# ==============================================================================
# 52. WRITE SUPPORTING CSV OUTPUTS
# ==============================================================================

write_csv(
  
  major_group_summary,
  
  file.path(
    
    OUTPUT_DIR,
    
    "VPJD_02h_major_group_summary.csv"
    
  )
  
)


write_csv(
  
  rank_by_major_group,
  
  file.path(
    
    OUTPUT_DIR,
    
    "VPJD_02h_rank_by_major_group.csv"
    
  )
  
)


write_csv(
  
  family_group_summary,
  
  file.path(
    
    OUTPUT_DIR,
    
    "VPJD_02h_family_group_summary.csv"
    
  )
  
)


# ==============================================================================
# 53. WRITE DUCKDB OUTPUT TABLE
# ==============================================================================

cat(
  "Writing validated 02h DuckDB table...\n"
)


DBI::dbWriteTable(
  
  con,
  
  OUTPUT_TABLE,
  
  validated_lineage_output,
  
  overwrite = TRUE
  
)


# ==============================================================================
# 54. VERIFY DUCKDB WRITE
# ==============================================================================

db_tables_after <-
  DBI::dbListTables(con)


output_table_present <-
  OUTPUT_TABLE %in%
  db_tables_after


if (!output_table_present) {
  
  disconnect_safely()
  
  
  stop(
    
    "02h output table was not created successfully."
    
  )
  
}


n_written <-
  DBI::dbGetQuery(
    
    con,
    
    paste0(
      
      'SELECT COUNT(*) AS N FROM "',
      
      OUTPUT_TABLE,
      
      '"'
      
    )
    
  )$N[[1]]


n_written_ids <-
  DBI::dbGetQuery(
    
    con,
    
    paste0(
      
      'SELECT COUNT(DISTINCT FINAL_WCVP_ID) AS N FROM "',
      
      OUTPUT_TABLE,
      
      '"'
      
    )
    
  )$N[[1]]


if (
  n_written !=
  EXPECTED_CANONICAL_POPULATION
) {
  
  disconnect_safely()
  
  
  stop(
    
    "02h output table contains ",
    n_written,
    " rows; expected ",
    EXPECTED_CANONICAL_POPULATION,
    "."
    
  )
  
}


if (
  n_written_ids !=
  EXPECTED_CANONICAL_POPULATION
) {
  
  disconnect_safely()
  
  
  stop(
    
    "02h output table is not unique by FINAL_WCVP_ID."
    
  )
  
}


# ==============================================================================
# 55. FINAL SUMMARY TABLE
# ==============================================================================

summary_table <- tibble(
  
  METRIC = c(
    
    "Canonical VPJD records",
    
    "02c taxonomic records",
    
    "Canonical WCVP IDs matched to 02c",
    
    "Canonical families",
    
    "02b crosswalk families",
    
    "Matched families",
    
    "Unmatched families",
    
    "Reconstructed records",
    
    "Unmatched reconstructed records",
    
    "Major vascular groups",
    
    "Angiosperm families",
    
    "Gymnosperm families",
    
    "Fern families",
    
    "Lycophyte families",
    
    "Contradictory family assignments",
    
    "Taxonomic changes made",
    
    "Star allocation changes made",
    
    "Taxa removed",
    
    "Taxa added"
    
  ),
  
  
  N = c(
    
    EXPECTED_CANONICAL_POPULATION,
    
    nrow(taxonomy),
    
    EXPECTED_CANONICAL_POPULATION -
      nrow(unmatched_wcvp_ids),
    
    n_taxonomy_families,
    
    n_crosswalk_families,
    
    n_matched_families,
    
    nrow(unmatched_families),
    
    nrow(validated_lineage_output),
    
    n_unmatched_records,
    
    n_distinct(
      validated_lineage_output$MAJOR_GROUP
    ),
    
    n_angiosperm_families,
    
    n_gymnosperm_families,
    
    n_fern_families,
    
    n_lycophyte_families,
    
    n_contradictory_families,
    
    0L,
    
    0L,
    
    0L,
    
    0L
    
  )
  
)


write_csv(
  
  summary_table,
  
  file.path(
    
    OUTPUT_DIR,
    
    "VPJD_02h_summary.csv"
    
  )
  
)


# ==============================================================================
# 56. FINAL REPORT
# ==============================================================================

cat("\n")

cat("============================================================\n")
cat("02h FINAL VALIDATED VASCULAR LINEAGE\n")
cat("============================================================\n\n")


print(
  
  summary_table,
  
  n = Inf
  
)


cat("\n")
cat("------------------------------------------------------------\n")
cat("MAJOR-GROUP REPRESENTATION\n")
cat("------------------------------------------------------------\n\n")


print(
  
  major_group_summary,
  
  n = Inf,
  
  width = Inf
  
)


cat("\n")
cat("------------------------------------------------------------\n")
cat("FAMILY REPRESENTATION BY MAJOR GROUP\n")
cat("------------------------------------------------------------\n\n")


print(
  
  family_group_summary,
  
  n = Inf
  
)


cat("\n")
cat("------------------------------------------------------------\n")
cat("VALIDATION RESULT\n")
cat("------------------------------------------------------------\n\n")


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


cat("\n")


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
  "Canonical ↔ 02c WCVP ID matches: ",
  format(
    EXPECTED_CANONICAL_POPULATION -
      nrow(unmatched_wcvp_ids),
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
  "Family crosswalk matches: ",
  n_matched_families,
  " / ",
  EXPECTED_CANONICAL_FAMILIES,
  "\n",
  sep = ""
)


cat(
  "Reconstructed records: ",
  format(
    nrow(validated_lineage_output),
    big.mark = ","
  ),
  "\n",
  sep = ""
)


cat(
  "Unmatched records: ",
  n_unmatched_records,
  "\n",
  sep = ""
)


cat(
  "Major vascular groups: ",
  n_distinct(
    validated_lineage_output$MAJOR_GROUP
  ),
  "\n\n",
  sep = ""
)


cat("Taxonomic changes made: 0\n")
cat("Star allocation changes made: 0\n")
cat("Taxa removed: 0\n")
cat("Taxa added: 0\n")


# ==============================================================================
# 57. CLEAN DISCONNECT
# ==============================================================================

DBI::dbDisconnect(
  
  con,
  
  shutdown = TRUE
  
)


rm(con)


gc()


# ==============================================================================
# 58. COMPLETION REPORT
# ==============================================================================

cat("\n")

cat("============================================================\n")


cat(
  
  "VPJD TAXONOMIC REVISION 02h v",
  
  VERSION,
  
  " COMPLETE\n",
  
  sep = ""
  
)


cat("============================================================\n\n")


cat(
  "Canonical / Star authority:\n",
  CANONICAL_TABLE,
  "\n\n",
  sep = ""
)


cat(
  "Taxonomic structure source:\n",
  TAXONOMY_TABLE,
  "\n\n",
  sep = ""
)


cat(
  "Family-lineage source:\n",
  CROSSWALK_TABLE,
  "\n\n",
  sep = ""
)


cat(
  "DuckDB output table:\n",
  OUTPUT_TABLE,
  "\n\n",
  sep = ""
)


cat(
  "CSV output directory:\n",
  OUTPUT_DIR,
  "\n\n",
  sep = ""
)


cat("Canonical population retained: 11,439\n")
cat("Canonical families retained: 260\n")
cat("Validated vascular groups: 4\n")
cat("Canonical taxa removed: 0\n")
cat("Canonical taxa added: 0\n")
cat("Canonical taxonomy modified: FALSE\n")
cat("Star allocations modified: FALSE\n")


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


cat("02h DuckDB table written: TRUE\n")
cat("02h CSV outputs written: TRUE\n")
cat("DuckDB connection closed cleanly.\n")


cat("\n")

cat("============================================================\n")


# ==============================================================================
# END
# ==============================================================================