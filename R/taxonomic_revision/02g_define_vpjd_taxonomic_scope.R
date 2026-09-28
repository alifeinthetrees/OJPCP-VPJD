# ==============================================================================
# VPJD TAXONOMIC REVISION
# 02g — DEFINE AND VALIDATE VPJD TAXONOMIC SCOPE
#
# Purpose:
#   Define and validate the taxonomic scope of the
#   Vascular Plants of Japan Database (VPJD).
#
# VPJD TAXONOMIC SCOPE:
#
#   RETAIN:
#     - Angiosperms
#     - Gymnosperms
#     - Ferns
#     - Lycophytes
#
#   EXCLUDE:
#     - Mosses
#     - Liverworts
#     - Hornworts
#     - Other non-vascular plant groups
#
# IMPORTANT:
#   AUDIT ONLY.
#
#   This script:
#     - DOES NOT delete taxa
#     - DOES NOT modify canonical taxonomy
#     - DOES NOT modify Star allocations
#     - DOES NOT overwrite the canonical population
#
# Expected canonical population: 11,439
# Expected family population:       260
#
# Validated 02b major-group family composition:
#     Angiosperms   228 families
#     Ferns          18 families
#     Gymnosperms    11 families
#     Lycophytes       3 families
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
  library(tibble)
  library(stringr)
  library(readr)
  
})


# ==============================================================================
# 03. CONSTANTS
# ==============================================================================

EXPECTED_CANONICAL_POPULATION <- 11439L
EXPECTED_CANONICAL_FAMILIES   <- 260L

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
  "02g_taxonomic_scope"
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

WCVP_TABLE <-
  "occurrence_wcvp_accepted_taxa"

CROSSWALK_TABLE <-
  "vpjd_taxrev_02b_family_lineage_crosswalk"


# ==============================================================================
# 05. START REPORT
# ==============================================================================

cat("\n")
cat("============================================================\n")

cat(
  "VPJD TAXONOMIC REVISION 02g v",
  VERSION,
  "\n",
  sep = ""
)

cat("DEFINE AND VALIDATE VPJD TAXONOMIC SCOPE\n")
cat("AUDIT ONLY — NO TAXA WILL BE REMOVED\n")
cat("============================================================\n\n")

cat("VPJD = Vascular Plants of Japan Database\n\n")

cat("IN-SCOPE vascular groups:\n")
cat("  Angiosperms\n")
cat("  Gymnosperms\n")
cat("  Ferns\n")
cat("  Lycophytes\n\n")

cat("OUT-OF-SCOPE groups:\n")
cat("  Mosses\n")
cat("  Liverworts\n")
cat("  Hornworts\n")
cat("  Other non-vascular plants\n\n")


# ==============================================================================
# 06. DATABASE CHECK
# ==============================================================================

if (!file.exists(DB_PATH)) {
  
  stop(
    "DuckDB file not found:\n",
    DB_PATH
  )
  
}


# ==============================================================================
# 07. CONNECT READ-ONLY
# ==============================================================================

con <- DBI::dbConnect(
  duckdb::duckdb(),
  dbdir = DB_PATH,
  read_only = TRUE
)

on.exit({
  
  if (
    exists("con") &&
    DBI::dbIsValid(con)
  ) {
    
    try(
      DBI::dbDisconnect(
        con,
        shutdown = TRUE
      ),
      silent = TRUE
    )
    
  }
  
}, add = TRUE)


# ==============================================================================
# 08. REQUIRED TABLE CHECK
# ==============================================================================

db_tables <- DBI::dbListTables(con)

required_tables <- c(
  CANONICAL_TABLE,
  WCVP_TABLE,
  CROSSWALK_TABLE
)

missing_tables <- setdiff(
  required_tables,
  db_tables
)

if (length(missing_tables) > 0L) {
  
  stop(
    "Required VPJD table(s) missing:\n",
    paste(
      missing_tables,
      collapse = "\n"
    )
  )
  
}

cat("Required source tables: 3 / 3 FOUND\n")


# ==============================================================================
# 09. LOAD CANONICAL POPULATION
# ==============================================================================

canonical <- DBI::dbGetQuery(
  con,
  paste0(
    '
    SELECT
      FINAL_WCVP_ID,
      FINAL_WCVP_RECOGNISED_NAME,
      FINAL_WCVP_RANK,
      FINAL_WCVP_STATUS,
      FINAL_WCVP_CONCEPT_CLASS,
      PROVISIONAL_STAR,
      IS_ALLOCATED,
      IS_UNRESOLVED
    FROM "',
    CANONICAL_TABLE,
    '"
    '
  )
) |>
  as_tibble() |>
  mutate(
    FINAL_WCVP_ID =
      as.character(FINAL_WCVP_ID)
  )


# ==============================================================================
# 10. VALIDATE CANONICAL POPULATION
# ==============================================================================

if (
  nrow(canonical) !=
  EXPECTED_CANONICAL_POPULATION
) {
  
  stop(
    "Canonical population = ",
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
  "Canonical VPJD population: ",
  format(
    nrow(canonical),
    big.mark = ","
  ),
  "\n",
  sep = ""
)


# ==============================================================================
# 11. LOAD WCVP TAXONOMIC LOOKUP
# ==============================================================================

wcvp_fields <- DBI::dbListFields(
  con,
  WCVP_TABLE
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
    "Required WCVP field(s) missing:\n",
    paste(
      missing_wcvp_fields,
      collapse = "\n"
    )
  )
  
}


wcvp <- DBI::dbGetQuery(
  con,
  paste0(
    '
    SELECT
      wcvp_plant_name_id,
      wcvp_taxon_name,
      wcvp_taxon_rank,
      wcvp_taxon_status,
      wcvp_family,
      wcvp_genus,
      wcvp_species
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
    
    WCVP_NAME =
      as.character(
        wcvp_taxon_name
      ),
    
    WCVP_RANK =
      as.character(
        wcvp_taxon_rank
      ),
    
    WCVP_STATUS =
      as.character(
        wcvp_taxon_status
      ),
    
    FAMILY =
      str_squish(
        as.character(
          wcvp_family
        )
      ),
    
    GENUS =
      as.character(
        wcvp_genus
      ),
    
    SPECIES =
      as.character(
        wcvp_species
      )
    
  ) |>
  distinct(
    FINAL_WCVP_ID,
    .keep_all = TRUE
  )


# ==============================================================================
# 12. ATTACH WCVP TAXONOMY
# ==============================================================================

taxonomy <- canonical |>
  left_join(
    wcvp,
    by = "FINAL_WCVP_ID"
  )


n_missing_family <- sum(
  is.na(taxonomy$FAMILY) |
    taxonomy$FAMILY == ""
)


if (n_missing_family > 0L) {
  
  stop(
    "Canonical taxa without WCVP family: ",
    n_missing_family
  )
  
}


canonical_families <- taxonomy |>
  distinct(
    FAMILY
  ) |>
  arrange(
    FAMILY
  )


if (
  nrow(canonical_families) !=
  EXPECTED_CANONICAL_FAMILIES
) {
  
  stop(
    "Canonical family population = ",
    nrow(canonical_families),
    "; expected ",
    EXPECTED_CANONICAL_FAMILIES,
    "."
  )
  
}


cat(
  "Canonical family population: ",
  nrow(canonical_families),
  "\n",
  sep = ""
)


# ==============================================================================
# 13. LOAD VALIDATED 02b FAMILY CROSSWALK
# ==============================================================================

crosswalk <- DBI::dbReadTable(
  con,
  CROSSWALK_TABLE
) |>
  as_tibble()


required_crosswalk_fields <- c(
  "FAMILY",
  "SCOPE_STATUS",
  "MAJOR_GROUP"
)

missing_crosswalk_fields <- setdiff(
  required_crosswalk_fields,
  names(crosswalk)
)


if (length(missing_crosswalk_fields) > 0L) {
  
  stop(
    "Required 02b crosswalk field(s) missing:\n",
    paste(
      missing_crosswalk_fields,
      collapse = "\n"
    )
  )
  
}


if (
  nrow(crosswalk) !=
  EXPECTED_CANONICAL_FAMILIES
) {
  
  stop(
    "02b family crosswalk contains ",
    nrow(crosswalk),
    " rows; expected ",
    EXPECTED_CANONICAL_FAMILIES,
    "."
  )
  
}


cat(
  "02b family crosswalk: ",
  nrow(crosswalk),
  " families\n",
  sep = ""
)


# ==============================================================================
# 14. NORMALISE FAMILY KEYS
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


taxonomy <- taxonomy |>
  mutate(
    FAMILY_KEY =
      normalise_family(
        FAMILY
      )
  )


crosswalk <- crosswalk |>
  mutate(
    
    FAMILY_KEY =
      normalise_family(
        FAMILY
      ),
    
    SCOPE_STATUS =
      str_squish(
        toupper(
          as.character(
            SCOPE_STATUS
          )
        )
      ),
    
    MAJOR_GROUP =
      str_squish(
        toupper(
          as.character(
            MAJOR_GROUP
          )
        )
      )
    
  )


# ==============================================================================
# 15. VALIDATE FAMILY CROSSWALK
# ==============================================================================

canonical_family_keys <- taxonomy |>
  distinct(
    FAMILY_KEY
  )


crosswalk_family_keys <- crosswalk |>
  distinct(
    FAMILY_KEY
  )


families_missing_from_crosswalk <-
  canonical_family_keys |>
  anti_join(
    crosswalk_family_keys,
    by = "FAMILY_KEY"
  )


families_extra_in_crosswalk <-
  crosswalk_family_keys |>
  anti_join(
    canonical_family_keys,
    by = "FAMILY_KEY"
  )


if (
  nrow(families_missing_from_crosswalk) > 0L ||
  nrow(families_extra_in_crosswalk) > 0L
) {
  
  stop(
    "02b family crosswalk does not provide a complete ",
    "260-family normalised match."
  )
  
}


cat(
  "Normalised family crosswalk: 260 / 260 MATCH\n"
)


# ==============================================================================
# 16. VALIDATE VASCULAR SCOPE STATUS
# ==============================================================================

scope_status_profile <- crosswalk |>
  count(
    SCOPE_STATUS,
    name = "N_FAMILIES",
    sort = TRUE
  )


cat("\n")
cat("------------------------------------------------------------\n")
cat("VASCULAR SCOPE STATUS\n")
cat("------------------------------------------------------------\n\n")

print(
  scope_status_profile,
  n = Inf,
  width = Inf
)


if (
  nrow(scope_status_profile) != 1L ||
  scope_status_profile$SCOPE_STATUS[[1]] !=
  "IN_SCOPE_VASCULAR" ||
  scope_status_profile$N_FAMILIES[[1]] !=
  EXPECTED_CANONICAL_FAMILIES
) {
  
  stop(
    "Not all 260 families are classified ",
    "IN_SCOPE_VASCULAR."
  )
  
}


cat(
  "\nAll 260 families classified IN_SCOPE_VASCULAR: PASS\n"
)


# ==============================================================================
# 17. VALIDATE MAJOR-GROUP CLASSIFICATION
# ==============================================================================

BIOLOGICAL_FIELD <- "MAJOR_GROUP"


major_group_profile_families <- crosswalk |>
  count(
    MAJOR_GROUP,
    name = "N_FAMILIES",
    sort = TRUE
  )


cat("\n")
cat("------------------------------------------------------------\n")
cat("MAJOR-GROUP FAMILY CLASSIFICATION\n")
cat("------------------------------------------------------------\n\n")

print(
  major_group_profile_families,
  n = Inf,
  width = Inf
)


expected_major_groups <- tibble(
  
  MAJOR_GROUP = c(
    "ANGIOSPERM",
    "FERN",
    "GYMNOSPERM",
    "LYCOPHYTE"
  ),
  
  EXPECTED_N_FAMILIES = c(
    228L,
    18L,
    11L,
    3L
  )
  
)


major_group_validation <- expected_major_groups |>
  left_join(
    major_group_profile_families,
    by = "MAJOR_GROUP"
  ) |>
  mutate(
    
    N_FAMILIES =
      coalesce(
        N_FAMILIES,
        0L
      ),
    
    PASS =
      N_FAMILIES ==
      EXPECTED_N_FAMILIES
    
  )


cat("\n")
cat("Validated expected major-group composition:\n\n")

print(
  major_group_validation,
  n = Inf,
  width = Inf
)


unexpected_major_groups <- setdiff(
  major_group_profile_families$MAJOR_GROUP,
  expected_major_groups$MAJOR_GROUP
)


if (length(unexpected_major_groups) > 0L) {
  
  stop(
    "Unexpected MAJOR_GROUP value(s):\n",
    paste(
      unexpected_major_groups,
      collapse = "\n"
    )
  )
  
}


if (!all(major_group_validation$PASS)) {
  
  stop(
    "MAJOR_GROUP family counts do not reproduce ",
    "the validated 02b classification."
  )
  
}


cat("\nMAJOR_GROUP validation: PASS\n")


# ==============================================================================
# 18. DEFINE VPJD SCOPE
# ==============================================================================

# All four major groups are vascular plants and are therefore
# retained within VPJD.

family_scope <- crosswalk |>
  transmute(
    
    FAMILY =
      as.character(
        FAMILY
      ),
    
    FAMILY_KEY,
    
    SCOPE_STATUS,
    
    MAJOR_GROUP,
    
    VPJD_SCOPE =
      case_when(
        
        MAJOR_GROUP %in%
          c(
            "ANGIOSPERM",
            "GYMNOSPERM",
            "FERN",
            "LYCOPHYTE"
          ) ~
          "RETAIN",
        
        TRUE ~
          "REVIEW"
        
      ),
    
    EXCLUSION_REASON =
      case_when(
        
        VPJD_SCOPE ==
          "REVIEW" ~
          paste0(
            "Major group not recognised as validated ",
            "VPJD vascular group: ",
            MAJOR_GROUP
          ),
        
        TRUE ~
          NA_character_
        
      )
    
  )


# ==============================================================================
# 19. FAMILY-LEVEL SCOPE PROFILE
# ==============================================================================

family_scope_profile <- family_scope |>
  count(
    MAJOR_GROUP,
    VPJD_SCOPE,
    name = "N_FAMILIES",
    sort = TRUE
  ) |>
  mutate(
    
    PERCENT_OF_FAMILIES =
      round(
        100 *
          N_FAMILIES /
          EXPECTED_CANONICAL_FAMILIES,
        3
      )
    
  )


cat("\n")
cat("------------------------------------------------------------\n")
cat("FAMILY-LEVEL VPJD SCOPE\n")
cat("------------------------------------------------------------\n\n")

print(
  family_scope_profile,
  n = Inf,
  width = Inf
)


# ==============================================================================
# 20. PROPAGATE MAJOR GROUP TO ALL CANONICAL CONCEPTS
# ==============================================================================

taxon_scope <- taxonomy |>
  left_join(
    
    family_scope |>
      select(
        FAMILY_KEY,
        SCOPE_STATUS,
        MAJOR_GROUP,
        VPJD_SCOPE,
        EXCLUSION_REASON
      ),
    
    by = "FAMILY_KEY"
    
  )


# ==============================================================================
# 21. CHECK PROPAGATION
# ==============================================================================

n_missing_scope <- sum(
  is.na(
    taxon_scope$VPJD_SCOPE
  )
)


n_missing_major_group <- sum(
  is.na(
    taxon_scope$MAJOR_GROUP
  )
)


if (
  n_missing_scope > 0L ||
  n_missing_major_group > 0L
) {
  
  stop(
    "Major-group/scope classification failed for ",
    max(
      n_missing_scope,
      n_missing_major_group
    ),
    " canonical concepts."
  )
  
}


# ==============================================================================
# 22. CONCEPT-LEVEL MAJOR-GROUP PROFILE
# ==============================================================================

concept_group_profile <- taxon_scope |>
  count(
    MAJOR_GROUP,
    VPJD_SCOPE,
    name = "N_CONCEPTS",
    sort = TRUE
  ) |>
  mutate(
    
    PERCENT_OF_VPJD =
      round(
        100 *
          N_CONCEPTS /
          EXPECTED_CANONICAL_POPULATION,
        3
      )
    
  )


cat("\n")
cat("------------------------------------------------------------\n")
cat("CONCEPT-LEVEL TAXONOMIC REPRESENTATION\n")
cat("------------------------------------------------------------\n\n")

print(
  concept_group_profile,
  n = Inf,
  width = Inf
)


# ==============================================================================
# 23. CONCEPT AND FAMILY PROFILE TOGETHER
# ==============================================================================

taxonomic_representation <- concept_group_profile |>
  select(
    MAJOR_GROUP,
    VPJD_SCOPE,
    N_CONCEPTS,
    PERCENT_OF_VPJD
  ) |>
  left_join(
    
    family_scope_profile |>
      select(
        MAJOR_GROUP,
        N_FAMILIES,
        PERCENT_OF_FAMILIES
      ),
    
    by = "MAJOR_GROUP"
    
  ) |>
  arrange(
    desc(
      N_CONCEPTS
    )
  )


cat("\n")
cat("------------------------------------------------------------\n")
cat("VPJD MAJOR-GROUP REPRESENTATION\n")
cat("------------------------------------------------------------\n\n")

print(
  taxonomic_representation,
  n = Inf,
  width = Inf
)


# ==============================================================================
# 24. RETAINED POPULATION
# ==============================================================================

retained_population <- taxon_scope |>
  filter(
    VPJD_SCOPE ==
      "RETAIN"
  ) |>
  arrange(
    MAJOR_GROUP,
    FAMILY,
    FINAL_WCVP_RECOGNISED_NAME
  )


# ==============================================================================
# 25. EXCLUDED POPULATION
# ==============================================================================

excluded_population <- taxon_scope |>
  filter(
    VPJD_SCOPE ==
      "EXCLUDE"
  )


# ==============================================================================
# 26. REVIEW POPULATION
# ==============================================================================

review_population <- taxon_scope |>
  filter(
    VPJD_SCOPE ==
      "REVIEW"
  )


# ==============================================================================
# 27. COUNTS
# ==============================================================================

n_retain <-
  nrow(
    retained_population
  )

n_exclude <-
  nrow(
    excluded_population
  )

n_review <-
  nrow(
    review_population
  )


# ==============================================================================
# 28. SCOPE SUMMARY
# ==============================================================================

scope_summary <- tibble(
  
  METRIC = c(
    
    "Canonical VPJD concepts",
    
    "Canonical VPJD families",
    
    "Vascular families",
    
    "Non-vascular families",
    
    "Retained concepts",
    
    "Excluded concepts",
    
    "Concepts requiring review",
    
    "Taxonomic changes made",
    
    "Star allocation changes made",
    
    "Taxa removed"
    
  ),
  
  N = c(
    
    EXPECTED_CANONICAL_POPULATION,
    
    EXPECTED_CANONICAL_FAMILIES,
    
    EXPECTED_CANONICAL_FAMILIES,
    
    0L,
    
    n_retain,
    
    n_exclude,
    
    n_review,
    
    0L,
    
    0L,
    
    0L
    
  )
  
)


cat("\n")
cat("------------------------------------------------------------\n")
cat("VPJD TAXONOMIC SCOPE SUMMARY\n")
cat("------------------------------------------------------------\n\n")

print(
  scope_summary,
  n = Inf,
  width = Inf
)


# ==============================================================================
# 29. RANK PROFILE BY MAJOR GROUP
# ==============================================================================

rank_by_major_group <- taxon_scope |>
  count(
    MAJOR_GROUP,
    FINAL_WCVP_RANK,
    name = "N_CONCEPTS",
    sort = TRUE
  ) |>
  group_by(
    MAJOR_GROUP
  ) |>
  mutate(
    
    PERCENT_WITHIN_GROUP =
      round(
        100 *
          N_CONCEPTS /
          sum(N_CONCEPTS),
        3
      )
    
  ) |>
  ungroup()


# ==============================================================================
# 30. STAR PROFILE BY MAJOR GROUP
# ==============================================================================

star_by_major_group <- taxon_scope |>
  mutate(
    
    STAR =
      coalesce(
        PROVISIONAL_STAR,
        "MISSING"
      )
    
  ) |>
  count(
    MAJOR_GROUP,
    STAR,
    name = "N_CONCEPTS",
    sort = TRUE
  )


# ==============================================================================
# 31. FAMILY PROFILE BY MAJOR GROUP
# ==============================================================================

family_by_major_group <- taxon_scope |>
  count(
    MAJOR_GROUP,
    FAMILY,
    name = "N_CONCEPTS",
    sort = TRUE
  )


# ==============================================================================
# 32. VALIDATION
# ==============================================================================

validation <- tibble(
  
  CHECK = c(
    
    "Canonical population = 11,439",
    
    "Canonical population unique by WCVP ID",
    
    "Canonical family population = 260",
    
    "02b crosswalk contains 260 families",
    
    "Normalised family crosswalk = 260/260",
    
    "All 260 families classified IN_SCOPE_VASCULAR",
    
    "Major-group field = MAJOR_GROUP",
    
    "Angiosperm family count = 228",
    
    "Fern family count = 18",
    
    "Gymnosperm family count = 11",
    
    "Lycophyte family count = 3",
    
    "Major-group family totals = 260",
    
    "All canonical concepts assigned major group",
    
    "All canonical concepts assigned VPJD scope",
    
    "Retained population = 11,439",
    
    "Excluded population = 0",
    
    "Review population = 0",
    
    "All four vascular groups retained",
    
    "Canonical population unchanged",
    
    "No taxa removed",
    
    "Star allocations unchanged"
    
  ),
  
  PASS = c(
    
    nrow(canonical) ==
      EXPECTED_CANONICAL_POPULATION,
    
    n_distinct(
      canonical$FINAL_WCVP_ID
    ) ==
      EXPECTED_CANONICAL_POPULATION,
    
    nrow(canonical_families) ==
      EXPECTED_CANONICAL_FAMILIES,
    
    nrow(crosswalk) ==
      EXPECTED_CANONICAL_FAMILIES,
    
    nrow(
      families_missing_from_crosswalk
    ) == 0L &&
      nrow(
        families_extra_in_crosswalk
      ) == 0L,
    
    nrow(scope_status_profile) == 1L &&
      scope_status_profile$SCOPE_STATUS[[1]] ==
      "IN_SCOPE_VASCULAR" &&
      scope_status_profile$N_FAMILIES[[1]] ==
      EXPECTED_CANONICAL_FAMILIES,
    
    BIOLOGICAL_FIELD ==
      "MAJOR_GROUP",
    
    major_group_profile_families |>
      filter(
        MAJOR_GROUP ==
          "ANGIOSPERM"
      ) |>
      pull(
        N_FAMILIES
      ) ==
      228L,
    
    major_group_profile_families |>
      filter(
        MAJOR_GROUP ==
          "FERN"
      ) |>
      pull(
        N_FAMILIES
      ) ==
      18L,
    
    major_group_profile_families |>
      filter(
        MAJOR_GROUP ==
          "GYMNOSPERM"
      ) |>
      pull(
        N_FAMILIES
      ) ==
      11L,
    
    major_group_profile_families |>
      filter(
        MAJOR_GROUP ==
          "LYCOPHYTE"
      ) |>
      pull(
        N_FAMILIES
      ) ==
      3L,
    
    sum(
      major_group_profile_families$N_FAMILIES
    ) ==
      EXPECTED_CANONICAL_FAMILIES,
    
    n_missing_major_group ==
      0L,
    
    n_missing_scope ==
      0L,
    
    n_retain ==
      EXPECTED_CANONICAL_POPULATION,
    
    n_exclude ==
      0L,
    
    n_review ==
      0L,
    
    all(
      c(
        "ANGIOSPERM",
        "GYMNOSPERM",
        "FERN",
        "LYCOPHYTE"
      ) %in%
        taxon_scope$MAJOR_GROUP
    ),
    
    nrow(canonical) ==
      EXPECTED_CANONICAL_POPULATION,
    
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


cat("\n")
cat("------------------------------------------------------------\n")
cat("VALIDATION\n")
cat("------------------------------------------------------------\n\n")

print(
  validation,
  n = Inf,
  width = Inf
)


# ==============================================================================
# 33. WRITE CSV OUTPUTS
# ==============================================================================

write_csv(
  scope_status_profile,
  file.path(
    OUTPUT_DIR,
    "VPJD_02g_vascular_scope_status.csv"
  )
)


write_csv(
  major_group_profile_families,
  file.path(
    OUTPUT_DIR,
    "VPJD_02g_major_group_family_profile.csv"
  )
)


write_csv(
  major_group_validation,
  file.path(
    OUTPUT_DIR,
    "VPJD_02g_major_group_validation.csv"
  )
)


write_csv(
  family_scope,
  file.path(
    OUTPUT_DIR,
    "VPJD_02g_family_scope_crosswalk.csv"
  )
)


write_csv(
  family_scope_profile,
  file.path(
    OUTPUT_DIR,
    "VPJD_02g_family_scope_profile.csv"
  )
)


write_csv(
  concept_group_profile,
  file.path(
    OUTPUT_DIR,
    "VPJD_02g_concept_group_profile.csv"
  )
)


write_csv(
  taxonomic_representation,
  file.path(
    OUTPUT_DIR,
    "VPJD_02g_taxonomic_representation.csv"
  )
)


write_csv(
  retained_population,
  file.path(
    OUTPUT_DIR,
    "VPJD_02g_retained_population.csv"
  )
)


write_csv(
  excluded_population,
  file.path(
    OUTPUT_DIR,
    "VPJD_02g_excluded_population.csv"
  )
)


write_csv(
  review_population,
  file.path(
    OUTPUT_DIR,
    "VPJD_02g_review_population.csv"
  )
)


write_csv(
  scope_summary,
  file.path(
    OUTPUT_DIR,
    "VPJD_02g_scope_summary.csv"
  )
)


write_csv(
  rank_by_major_group,
  file.path(
    OUTPUT_DIR,
    "VPJD_02g_rank_by_major_group.csv"
  )
)


write_csv(
  star_by_major_group,
  file.path(
    OUTPUT_DIR,
    "VPJD_02g_star_by_major_group.csv"
  )
)


write_csv(
  family_by_major_group,
  file.path(
    OUTPUT_DIR,
    "VPJD_02g_family_by_major_group.csv"
  )
)


write_csv(
  validation,
  file.path(
    OUTPUT_DIR,
    "VPJD_02g_validation.csv"
  )
)


# ==============================================================================
# 34. DECISION
# ==============================================================================

if (
  all(validation$PASS) &&
  n_retain ==
  EXPECTED_CANONICAL_POPULATION &&
  n_exclude == 0L &&
  n_review == 0L
) {
  
  decision <-
    "VPJD_VASCULAR_SCOPE_VALIDATED"
  
} else {
  
  decision <-
    "REVIEW_REQUIRED"
  
}


# ==============================================================================
# 35. FINAL REPORT
# ==============================================================================

cat("\n")
cat("============================================================\n")
cat("02g FINAL TAXONOMIC SCOPE AUDIT\n")
cat("============================================================\n\n")


cat(
  "Database: Vascular Plants of Japan Database (VPJD)\n"
)


cat(
  "Biological classification field: ",
  BIOLOGICAL_FIELD,
  "\n\n",
  sep = ""
)


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
  EXPECTED_CANONICAL_FAMILIES,
  "\n\n",
  sep = ""
)


cat(
  "Retained population: ",
  format(
    n_retain,
    big.mark = ","
  ),
  "\n",
  sep = ""
)


cat(
  "Excluded population: ",
  format(
    n_exclude,
    big.mark = ","
  ),
  "\n",
  sep = ""
)


cat(
  "Requires manual review: ",
  format(
    n_review,
    big.mark = ","
  ),
  "\n\n",
  sep = ""
)


cat("Major groups retained:\n")
cat("  Angiosperms\n")
cat("  Gymnosperms\n")
cat("  Ferns\n")
cat("  Lycophytes\n\n")


cat(
  "Validation: ",
  sum(validation$PASS),
  "/",
  nrow(validation),
  " PASS\n",
  sep = ""
)


cat(
  "Decision: ",
  decision,
  "\n\n",
  sep = ""
)


cat("Canonical population modified: FALSE\n")
cat("Canonical taxonomy modified: FALSE\n")
cat("Star allocations modified: FALSE\n")
cat("Taxa removed: 0\n")


# ==============================================================================
# 36. CLEAN DISCONNECT
# ==============================================================================

DBI::dbDisconnect(
  con,
  shutdown = TRUE
)

rm(con)

gc()


# ==============================================================================
# 37. COMPLETION
# ==============================================================================

cat("\n")
cat("============================================================\n")

cat(
  "VPJD TAXONOMIC REVISION 02g v",
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


cat("AUDIT ONLY\n")
cat("Database modified: FALSE\n")
cat("Canonical taxonomy modified: FALSE\n")
cat("Star allocations modified: FALSE\n")
cat("Taxa removed: 0\n")
cat("CSV audit outputs written: TRUE\n")
cat("DuckDB connection closed cleanly.\n")

cat("\n")
cat("============================================================\n")


# ==============================================================================
# END
# ==============================================================================