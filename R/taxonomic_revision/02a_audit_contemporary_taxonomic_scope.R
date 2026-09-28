# ==============================================================================
# VPJD / OJPCP
# 02a — Audit non-vascular taxa
#
# File:
#   R/taxonomic_revision/02a_audit_non_vascular_taxa.R
#
# Version: 0.1.1
#
# PURPOSE
# -------
# First-pass audit of the contemporary VPJD accepted taxonomic population for
# taxa falling outside the intended vascular-plant scope.
#
# This module:
#   1. reads the canonical 11,439-concept contemporary VPJD taxonomy;
#   2. profiles families and ranks represented in VPJD;
#   3. flags recognised bryophyte families:
#        - mosses
#        - liverworts
#        - hornworts
#   4. identifies taxa with missing family information for manual review;
#   5. simulates the effect of excluding confirmed non-vascular candidates;
#   6. profiles the provisional Star allocations of flagged taxa;
#   7. writes complete audit outputs to DuckDB and CSV.
#
# IMPORTANT
# ---------
# This is an AUDIT module.
#
# It does NOT:
#   * delete taxa;
#   * overwrite the canonical VPJD population;
#   * change WCVP reconciliation;
#   * change provisional Star allocations;
#   * assume every unrecognised family is non-vascular.
#
# Canonical input:
#   vpjd_star_provisional_taxonomic_composition
#
# Expected contemporary population:
#   11,439 accepted concepts
#
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

MODULE <- "02a_audit_non_vascular_taxa"
VERSION <- "0.1.1"
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
  "02a_non_vascular_audit"
)

dir.create(
  OUTPUT_DIR,
  recursive = TRUE,
  showWarnings = FALSE
)

EXPECTED_POPULATION <- 11439L

TABLE_INPUT <-
  "vpjd_star_provisional_taxonomic_composition"

TABLE_AUDIT <-
  "vpjd_taxrev_02a_non_vascular_audit"

TABLE_FAMILY_PROFILE <-
  "vpjd_taxrev_02a_family_profile"

TABLE_CANDIDATES <-
  "vpjd_taxrev_02a_non_vascular_candidates"

TABLE_SUMMARY <-
  "vpjd_taxrev_02a_summary"

TABLE_VALIDATION <-
  "vpjd_taxrev_02a_validation"


# ==============================================================================
# 02. DATABASE CONNECTION SAFETY
#
# This module should be run from a clean R session.
# It creates and closes its own DuckDB connection.
# ==============================================================================

if (exists("con", envir = .GlobalEnv)) {
  
  stop(
    paste0(
      "An object named 'con' already exists in this R session.\n\n",
      "For database safety, restart R before running 02a:\n",
      "Session > Restart R\n",
      "or Ctrl + Shift + F10\n\n",
      "Then run 02a from the beginning."
    )
  )
}

# ==============================================================================
# 03. CHECK DATABASE FILE
# ==============================================================================

if (!file.exists(DB_PATH)) {
  stop(
    "Canonical VPJD DuckDB not found:\n",
    DB_PATH
  )
}

db_size <- file.info(DB_PATH)$size

cat("\n")
cat("============================================================\n")
cat("VPJD TAXONOMIC REVISION — NON-VASCULAR AUDIT\n")
cat("============================================================\n\n")

cat("Run date: ", RUN_DATE, "\n", sep = "")
cat("Module: ", MODULE, "\n", sep = "")
cat("Version: ", VERSION, "\n", sep = "")
cat("Database: ", DB_PATH, "\n", sep = "")

cat(
  "Database size: ",
  format(
    db_size,
    big.mark = ",",
    scientific = FALSE
  ),
  " bytes\n\n",
  sep = ""
)


# ==============================================================================
# 04. CONNECT TO CANONICAL DUCKDB
#
# IMPORTANT:
# read_only = FALSE is required because this audit writes NEW vpjd_taxrev_*
# tables back to the project database.
#
# No canonical upstream tables are overwritten.
# ==============================================================================

con <- DBI::dbConnect(
  duckdb::duckdb(),
  dbdir = DB_PATH,
  read_only = FALSE
)

on.exit(
  {
    try(
      DBI::dbDisconnect(
        con,
        shutdown = TRUE
      ),
      silent = TRUE
    )
  },
  add = TRUE
)


# ==============================================================================
# 05. VERIFY CONNECTION
# ==============================================================================

if (!DBI::dbIsValid(con)) {
  stop(
    "DuckDB connection was created but is not valid."
  )
}

connection_class <- class(con)

if (!any(grepl(
  "duckdb",
  connection_class,
  ignore.case = TRUE
))) {
  stop(
    "Database connection is not a DuckDB connection.\n",
    "Connection class: ",
    paste(connection_class, collapse = ", ")
  )
}

cat(
  "Connection class: ",
  paste(connection_class, collapse = ", "),
  "\n",
  sep = ""
)

cat("Connection valid: TRUE\n\n")


# ==============================================================================
# 06. CHECK CANONICAL INPUT
# ==============================================================================

db_tables <- DBI::dbListTables(con)

cat(
  "DuckDB tables available: ",
  format(length(db_tables), big.mark = ","),
  "\n",
  sep = ""
)

if (!TABLE_INPUT %in% db_tables) {
  stop(
    "Required canonical input table missing:\n",
    TABLE_INPUT
  )
}

cat(
  "Canonical input found: ",
  TABLE_INPUT,
  "\n\n",
  sep = ""
)


# ==============================================================================
# 07. READ CANONICAL CONTEMPORARY TAXONOMY
# ==============================================================================

taxonomy <- DBI::dbReadTable(
  con,
  TABLE_INPUT
) %>%
  as_tibble()

cat(
  "Canonical contemporary population: ",
  format(nrow(taxonomy), big.mark = ","),
  "\n",
  sep = ""
)

if (nrow(taxonomy) != EXPECTED_POPULATION) {
  stop(
    "Expected ",
    format(EXPECTED_POPULATION, big.mark = ","),
    " contemporary accepted concepts; found ",
    format(nrow(taxonomy), big.mark = ","),
    "."
  )
}


# ==============================================================================
# 08. REQUIRED INPUT FIELDS
# ==============================================================================

required_fields <- c(
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

missing_fields <- setdiff(
  required_fields,
  names(taxonomy)
)

if (length(missing_fields) > 0L) {
  stop(
    paste0(
      "Required fields missing from ",
      TABLE_INPUT,
      ":\n",
      paste(
        missing_fields,
        collapse = ", "
      ),
      "\n\nAvailable fields:\n",
      paste(
        names(taxonomy),
        collapse = ", "
      )
    )
  )
}


# ==============================================================================
# 09. STANDARDISE TAXONOMIC FIELDS
# ==============================================================================

taxonomy <- taxonomy %>%
  mutate(
    
    FINAL_WCVP_ID =
      as.character(FINAL_WCVP_ID),
    
    TAXON_NAME =
      coalesce(
        as.character(wcvp_taxon_name),
        as.character(FINAL_WCVP_RECOGNISED_NAME)
      ),
    
    TAXON_RANK =
      toupper(
        trimws(
          coalesce(
            as.character(wcvp_taxon_rank),
            as.character(FINAL_WCVP_RANK),
            ""
          )
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
# 10. NON-VASCULAR FAMILY REFERENCE REGISTER
#
# Conservative first-pass screening register.
#
# Presence in this register creates a CANDIDATE exclusion only.
# Nothing is deleted or reclassified automatically.
# ==============================================================================

non_vascular_family_register <- tribble(
  
  ~FAMILY,                    ~NON_VASCULAR_GROUP,
  
  # ---------------------------------------------------------------------------
  # HORNWORTS
  # ---------------------------------------------------------------------------
  
  "ANTHOCEROTACEAE",          "HORNWORT",
  "DENDROCEROTACEAE",         "HORNWORT",
  "NOTOTHYLADACEAE",          "HORNWORT",
  
  # ---------------------------------------------------------------------------
  # LIVERWORTS
  # ---------------------------------------------------------------------------
  
  "ACROBOLBACEAE",            "LIVERWORT",
  "ADELANTHACEAE",            "LIVERWORT",
  "ANEURACEAE",               "LIVERWORT",
  "ANTHELIACEAE",             "LIVERWORT",
  "ARNELLIACEAE",             "LIVERWORT",
  "AYTONIACEAE",              "LIVERWORT",
  "BALANTIOPSIDACEAE",        "LIVERWORT",
  "BLASIACEAE",               "LIVERWORT",
  "CALYPOGEIACEAE",           "LIVERWORT",
  "CEPHALOZIACEAE",           "LIVERWORT",
  "CEPHALOZIELLACEAE",        "LIVERWORT",
  "CLEVEACEAE",               "LIVERWORT",
  "CONOCEPHALACEAE",          "LIVERWORT",
  "CORSINIACEAE",             "LIVERWORT",
  "FOSSOMBRONIACEAE",         "LIVERWORT",
  "FRULLANIACEAE",            "LIVERWORT",
  "GEOCALYCACEAE",            "LIVERWORT",
  "GYMNOMITRIACEAE",          "LIVERWORT",
  "HAPLOMITRIACEAE",          "LIVERWORT",
  "HERBERTACEAE",             "LIVERWORT",
  "JUBULACEAE",               "LIVERWORT",
  "JUNGERMANNIACEAE",         "LIVERWORT",
  "LEJEUNEACEAE",             "LIVERWORT",
  "LEPICOLEACEAE",            "LIVERWORT",
  "LEPIDOZIACEAE",            "LIVERWORT",
  "LOPHOCOLEACEAE",           "LIVERWORT",
  "LUNULARIACEAE",            "LIVERWORT",
  "MARCHANTIACEAE",           "LIVERWORT",
  "MARSUPELLACEAE",           "LIVERWORT",
  "METZGERIACEAE",            "LIVERWORT",
  "PALLAVICINIACEAE",         "LIVERWORT",
  "PELLIACEAE",               "LIVERWORT",
  "PLAGIOCHILACEAE",          "LIVERWORT",
  "PORELLACEAE",              "LIVERWORT",
  "RADULACEAE",               "LIVERWORT",
  "RICCIACEAE",               "LIVERWORT",
  "SCAPANIACEAE",             "LIVERWORT",
  "SOLENOSTOMATACEAE",        "LIVERWORT",
  "TARGIONIACEAE",            "LIVERWORT",
  "TRICHOCOLEACEAE",          "LIVERWORT",
  
  # ---------------------------------------------------------------------------
  # MOSSES
  # ---------------------------------------------------------------------------
  
  "AMBLYSTEGIACEAE",          "MOSS",
  "ANDREAEACEAE",             "MOSS",
  "ANDREAEOBRYACEAE",         "MOSS",
  "ARCHIDIACEAE",             "MOSS",
  "BARTRAMIACEAE",            "MOSS",
  "BRACHYTHECIACEAE",         "MOSS",
  "BRYACEAE",                 "MOSS",
  "BUXBAUMIACEAE",            "MOSS",
  "CALYMPERACEAE",            "MOSS",
  "CLIMACIACEAE",             "MOSS",
  "CRYPHAEACEAE",             "MOSS",
  "DALTONIACEAE",             "MOSS",
  "DICRANACEAE",              "MOSS",
  "DITRICHACEAE",             "MOSS",
  "ENCALYPTACEAE",            "MOSS",
  "ENTODONTACEAE",            "MOSS",
  "ERPODIACEAE",              "MOSS",
  "FABRONIACEAE",             "MOSS",
  "FISSIDENTACEAE",           "MOSS",
  "FONTINALACEAE",            "MOSS",
  "FUNARIACEAE",              "MOSS",
  "GRIMMIACEAE",              "MOSS",
  "HEDWIGIACEAE",             "MOSS",
  "HOOKERIACEAE",             "MOSS",
  "HYLOCOMIACEAE",            "MOSS",
  "HYPNACEAE",                "MOSS",
  "HYPNODENDRACEAE",          "MOSS",
  "LEPTODONTACEAE",           "MOSS",
  "LEUCOBRYACEAE",            "MOSS",
  "LEUCODONTACEAE",           "MOSS",
  "MEESIACEAE",               "MOSS",
  "METEORIACEAE",             "MOSS",
  "MNIACEAE",                 "MOSS",
  "NECKERACEAE",              "MOSS",
  "ORTHOTRICHACEAE",          "MOSS",
  "PHYLLOGONIACEAE",          "MOSS",
  "PILOTRICHACEAE",           "MOSS",
  "PLAGIOTHECIACEAE",         "MOSS",
  "POLYTRICHACEAE",           "MOSS",
  "POTTIACEAE",               "MOSS",
  "PTEROBRYACEAE",            "MOSS",
  "PTYCHOMITRIACEAE",         "MOSS",
  "RACOPILACEAE",             "MOSS",
  "RHIZOGONIACEAE",           "MOSS",
  "RHYTIDIACEAE",             "MOSS",
  "SEMATOPHYLLACEAE",         "MOSS",
  "SPHAGNACEAE",              "MOSS",
  "SPLACHNACEAE",             "MOSS",
  "THUIDIACEAE",              "MOSS"
) %>%
  distinct(
    FAMILY,
    .keep_all = TRUE
  )


# ==============================================================================
# 11. FAMILY PROFILE
# ==============================================================================

family_profile <- taxonomy %>%
  group_by(FAMILY) %>%
  summarise(
    
    N_TAXA = n(),
    
    N_GENERA = n_distinct(
      GENUS[
        !is.na(GENUS) &
          GENUS != ""
      ]
    ),
    
    N_SPECIES_RANK = sum(
      TAXON_RANK == "SPECIES",
      na.rm = TRUE
    ),
    
    N_INFRA = sum(
      TAXON_RANK %in% c(
        "SUBSPECIES",
        "SUBSP.",
        "VARIETY",
        "VAR.",
        "FORM",
        "FORMA",
        "F."
      ),
      na.rm = TRUE
    ),
    
    N_GENUS_RANK = sum(
      TAXON_RANK == "GENUS",
      na.rm = TRUE
    ),
    
    .groups = "drop"
  ) %>%
  
  left_join(
    non_vascular_family_register,
    by = "FAMILY"
  ) %>%
  
  mutate(
    
    IS_KNOWN_NON_VASCULAR_FAMILY =
      !is.na(NON_VASCULAR_GROUP),
    
    FAMILY_SCOPE = case_when(
      
      FAMILY == "" ~
        "REVIEW_MISSING_FAMILY",
      
      IS_KNOWN_NON_VASCULAR_FAMILY ~
        "KNOWN_NON_VASCULAR",
      
      TRUE ~
        "NOT_FLAGGED_NON_VASCULAR"
    )
  ) %>%
  
  arrange(
    desc(IS_KNOWN_NON_VASCULAR_FAMILY),
    desc(N_TAXA),
    FAMILY
  )


# ==============================================================================
# 12. TAXON-LEVEL AUDIT
# ==============================================================================

audit <- taxonomy %>%
  
  left_join(
    non_vascular_family_register,
    by = "FAMILY"
  ) %>%
  
  mutate(
    
    IS_KNOWN_NON_VASCULAR_FAMILY =
      !is.na(NON_VASCULAR_GROUP),
    
    HAS_FAMILY =
      FAMILY != "",
    
    HAS_GENUS =
      GENUS != "",
    
    TAXONOMIC_SCOPE_STATUS = case_when(
      
      NON_VASCULAR_GROUP == "MOSS" ~
        "EXCLUDE_CANDIDATE_MOSS",
      
      NON_VASCULAR_GROUP == "LIVERWORT" ~
        "EXCLUDE_CANDIDATE_LIVERWORT",
      
      NON_VASCULAR_GROUP == "HORNWORT" ~
        "EXCLUDE_CANDIDATE_HORNWORT",
      
      !HAS_FAMILY ~
        "REVIEW_MISSING_FAMILY",
      
      TRUE ~
        "NO_NON_VASCULAR_FLAG"
    ),
    
    PROPOSED_SCOPE_ACTION = case_when(
      
      IS_KNOWN_NON_VASCULAR_FAMILY ~
        "REVIEW_FOR_EXCLUSION",
      
      !HAS_FAMILY ~
        "MANUAL_REVIEW",
      
      TRUE ~
        "RETAIN_PENDING_FURTHER_AUDIT"
    ),
    
    AUDIT_REASON = case_when(
      
      NON_VASCULAR_GROUP == "MOSS" ~
        "WCVP family identified in 02a screening register as moss",
      
      NON_VASCULAR_GROUP == "LIVERWORT" ~
        "WCVP family identified in 02a screening register as liverwort",
      
      NON_VASCULAR_GROUP == "HORNWORT" ~
        "WCVP family identified in 02a screening register as hornwort",
      
      !HAS_FAMILY ~
        "No WCVP family available; vascular status requires manual review",
      
      TRUE ~
        "No non-vascular family flag in 02a"
    )
  )


# ==============================================================================
# 13. CANDIDATE NON-VASCULAR POPULATION
# ==============================================================================

non_vascular_candidates <- audit %>%
  
  filter(
    IS_KNOWN_NON_VASCULAR_FAMILY
  ) %>%
  
  select(
    FINAL_WCVP_ID,
    TAXON_NAME,
    TAXON_RANK,
    FAMILY,
    GENUS,
    SPECIES,
    NON_VASCULAR_GROUP,
    PROVISIONAL_STAR,
    TAXONOMIC_SCOPE_STATUS,
    PROPOSED_SCOPE_ACTION,
    AUDIT_REASON
  ) %>%
  
  arrange(
    NON_VASCULAR_GROUP,
    FAMILY,
    GENUS,
    TAXON_NAME
  )


# ==============================================================================
# 14. NON-VASCULAR GROUP PROFILE
# ==============================================================================

non_vascular_group_profile <- non_vascular_candidates %>%
  
  count(
    NON_VASCULAR_GROUP,
    name = "N_TAXA",
    sort = TRUE
  ) %>%
  
  mutate(
    PERCENT_ACCEPTED =
      round(
        100 * N_TAXA / EXPECTED_POPULATION,
        4
      )
  )


# ==============================================================================
# 15. NON-VASCULAR FAMILY PROFILE
# ==============================================================================

non_vascular_family_profile <- non_vascular_candidates %>%
  
  group_by(
    NON_VASCULAR_GROUP,
    FAMILY
  ) %>%
  
  summarise(
    
    N_TAXA = n(),
    
    N_GENERA = n_distinct(
      GENUS[
        !is.na(GENUS) &
          GENUS != ""
      ]
    ),
    
    .groups = "drop"
  ) %>%
  
  arrange(
    NON_VASCULAR_GROUP,
    desc(N_TAXA),
    FAMILY
  )


# ==============================================================================
# 16. RANK PROFILE OF FLAGGED TAXA
# ==============================================================================

non_vascular_rank_profile <- non_vascular_candidates %>%
  
  count(
    NON_VASCULAR_GROUP,
    TAXON_RANK,
    name = "N_TAXA",
    sort = TRUE
  ) %>%
  
  arrange(
    NON_VASCULAR_GROUP,
    desc(N_TAXA),
    TAXON_RANK
  )


# ==============================================================================
# 17. PROVISIONAL STAR PROFILE OF FLAGGED TAXA
#
# No Star allocation is changed here.
# ==============================================================================

non_vascular_star_profile <- non_vascular_candidates %>%
  
  mutate(
    STAR_DISPLAY =
      coalesce(
        as.character(PROVISIONAL_STAR),
        "UNRESOLVED"
      )
  ) %>%
  
  count(
    NON_VASCULAR_GROUP,
    STAR_DISPLAY,
    name = "N_TAXA",
    sort = TRUE
  ) %>%
  
  arrange(
    NON_VASCULAR_GROUP,
    STAR_DISPLAY
  )


# ==============================================================================
# 18. MISSING-FAMILY REVIEW
# ==============================================================================

missing_family_review <- audit %>%
  
  filter(
    TAXONOMIC_SCOPE_STATUS ==
      "REVIEW_MISSING_FAMILY"
  ) %>%
  
  select(
    FINAL_WCVP_ID,
    TAXON_NAME,
    TAXON_RANK,
    FAMILY,
    GENUS,
    SPECIES,
    PROVISIONAL_STAR,
    TAXONOMIC_SCOPE_STATUS,
    PROPOSED_SCOPE_ACTION,
    AUDIT_REASON
  ) %>%
  
  arrange(
    TAXON_NAME
  )


# ==============================================================================
# 19. SIMULATED PUBLICATION POPULATION
#
# Nothing is actually removed.
# ==============================================================================

n_non_vascular <-
  nrow(non_vascular_candidates)

n_moss <-
  sum(
    non_vascular_candidates$NON_VASCULAR_GROUP == "MOSS",
    na.rm = TRUE
  )

n_liverwort <-
  sum(
    non_vascular_candidates$NON_VASCULAR_GROUP == "LIVERWORT",
    na.rm = TRUE
  )

n_hornwort <-
  sum(
    non_vascular_candidates$NON_VASCULAR_GROUP == "HORNWORT",
    na.rm = TRUE
  )

n_missing_family <-
  nrow(missing_family_review)

simulated_population <-
  EXPECTED_POPULATION -
  n_non_vascular


summary_table <- tibble(
  
  METRIC = c(
    "Contemporary accepted VPJD concepts",
    "Candidate moss exclusions",
    "Candidate liverwort exclusions",
    "Candidate hornwort exclusions",
    "Total candidate non-vascular exclusions",
    "Taxa requiring review because family missing",
    "Simulated vascular publication population"
  ),
  
  N = c(
    EXPECTED_POPULATION,
    n_moss,
    n_liverwort,
    n_hornwort,
    n_non_vascular,
    n_missing_family,
    simulated_population
  )
)


# ==============================================================================
# 20. SIMULATED RETAINED POPULATION
# ==============================================================================

retained_simulation <- audit %>%
  
  filter(
    !IS_KNOWN_NON_VASCULAR_FAMILY
  )


# ==============================================================================
# 21. TAXONOMIC COMPOSITION BEFORE / AFTER SIMULATION
# ==============================================================================

count_rank <- function(data, ranks) {
  
  sum(
    data$TAXON_RANK %in% ranks,
    na.rm = TRUE
  )
}


composition_simulation <- tibble(
  
  METRIC = c(
    "Accepted concepts",
    "Families",
    "Genera",
    "Species-rank concepts",
    "Subspecies-rank concepts",
    "Variety-rank concepts",
    "Form-rank concepts",
    "Genus-rank concepts"
  ),
  
  CURRENT = c(
    
    nrow(audit),
    
    n_distinct(
      audit$FAMILY[
        !is.na(audit$FAMILY) &
          audit$FAMILY != ""
      ]
    ),
    
    n_distinct(
      audit$GENUS[
        !is.na(audit$GENUS) &
          audit$GENUS != ""
      ]
    ),
    
    count_rank(
      audit,
      "SPECIES"
    ),
    
    count_rank(
      audit,
      c(
        "SUBSPECIES",
        "SUBSP."
      )
    ),
    
    count_rank(
      audit,
      c(
        "VARIETY",
        "VAR."
      )
    ),
    
    count_rank(
      audit,
      c(
        "FORM",
        "FORMA",
        "F."
      )
    ),
    
    count_rank(
      audit,
      "GENUS"
    )
  ),
  
  SIMULATED_AFTER_NON_VASCULAR_EXCLUSION = c(
    
    nrow(retained_simulation),
    
    n_distinct(
      retained_simulation$FAMILY[
        !is.na(retained_simulation$FAMILY) &
          retained_simulation$FAMILY != ""
      ]
    ),
    
    n_distinct(
      retained_simulation$GENUS[
        !is.na(retained_simulation$GENUS) &
          retained_simulation$GENUS != ""
      ]
    ),
    
    count_rank(
      retained_simulation,
      "SPECIES"
    ),
    
    count_rank(
      retained_simulation,
      c(
        "SUBSPECIES",
        "SUBSP."
      )
    ),
    
    count_rank(
      retained_simulation,
      c(
        "VARIETY",
        "VAR."
      )
    ),
    
    count_rank(
      retained_simulation,
      c(
        "FORM",
        "FORMA",
        "F."
      )
    ),
    
    count_rank(
      retained_simulation,
      "GENUS"
    )
  )
) %>%
  
  mutate(
    DIFFERENCE =
      SIMULATED_AFTER_NON_VASCULAR_EXCLUSION -
      CURRENT
  )


# ==============================================================================
# 22. VALIDATION
# ==============================================================================

candidate_families_valid <-
  if (nrow(non_vascular_candidates) == 0L) {
    TRUE
  } else {
    all(
      non_vascular_candidates$FAMILY %in%
        non_vascular_family_register$FAMILY
    )
  }


candidate_groups_valid <-
  if (nrow(non_vascular_candidates) == 0L) {
    TRUE
  } else {
    all(
      non_vascular_candidates$NON_VASCULAR_GROUP %in%
        c(
          "MOSS",
          "LIVERWORT",
          "HORNWORT"
        )
    )
  }


candidate_ids_valid <-
  if (nrow(non_vascular_candidates) == 0L) {
    TRUE
  } else {
    all(
      !is.na(
        non_vascular_candidates$FINAL_WCVP_ID
      ) &
        non_vascular_candidates$FINAL_WCVP_ID != ""
    )
  }


star_values_unchanged <-
  all(
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
    
    "Canonical 05a taxonomy table present",
    
    "Canonical population = 11,439",
    
    "Canonical population unique by WCVP ID",
    
    "All canonical records have Accepted status",
    
    "All taxa assigned a taxonomic-scope status",
    
    "All taxa assigned a proposed scope action",
    
    "Flagged non-vascular taxa belong to registered non-vascular families",
    
    "Non-vascular groups restricted to MOSS/LIVERWORT/HORNWORT",
    
    "No unflagged taxon automatically proposed for exclusion",
    
    "Candidate exclusions plus simulated retained population = 11,439",
    
    "Audit population unchanged at 11,439",
    
    "Provisional Star values unchanged",
    
    "Summary current population = 11,439",
    
    "Simulated population cannot exceed current population",
    
    "All candidate exclusion records retain WCVP ID"
  ),
  
  PASS = c(
    
    TABLE_INPUT %in%
      db_tables,
    
    nrow(audit) ==
      EXPECTED_POPULATION,
    
    n_distinct(
      audit$FINAL_WCVP_ID
    ) ==
      EXPECTED_POPULATION,
    
    all(
      audit$FINAL_WCVP_STATUS ==
        "Accepted",
      na.rm = FALSE
    ),
    
    all(
      !is.na(
        audit$TAXONOMIC_SCOPE_STATUS
      )
    ),
    
    all(
      !is.na(
        audit$PROPOSED_SCOPE_ACTION
      )
    ),
    
    candidate_families_valid,
    
    candidate_groups_valid,
    
    !any(
      !audit$IS_KNOWN_NON_VASCULAR_FAMILY &
        audit$PROPOSED_SCOPE_ACTION ==
        "REVIEW_FOR_EXCLUSION",
      na.rm = TRUE
    ),
    
    n_non_vascular +
      nrow(retained_simulation) ==
      EXPECTED_POPULATION,
    
    nrow(audit) ==
      nrow(taxonomy),
    
    star_values_unchanged,
    
    summary_table$N[
      summary_table$METRIC ==
        "Contemporary accepted VPJD concepts"
    ] ==
      EXPECTED_POPULATION,
    
    simulated_population <=
      EXPECTED_POPULATION,
    
    candidate_ids_valid
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
# 23. CONSOLE RESULTS BEFORE WRITING
# ==============================================================================

cat("\n")
cat("— NON-VASCULAR AUDIT SUMMARY —\n\n")

print(
  summary_table,
  n = Inf
)


cat("\n")
cat("— NON-VASCULAR GROUP PROFILE —\n\n")

if (nrow(non_vascular_group_profile) == 0L) {
  
  cat("No candidate non-vascular taxa detected.\n")
  
} else {
  
  print(
    non_vascular_group_profile,
    n = Inf
  )
}


cat("\n")
cat("— NON-VASCULAR FAMILY PROFILE —\n\n")

if (nrow(non_vascular_family_profile) == 0L) {
  
  cat("No candidate non-vascular families detected.\n")
  
} else {
  
  print(
    non_vascular_family_profile,
    n = Inf
  )
}


cat("\n")
cat("— TAXONOMIC COMPOSITION SIMULATION —\n\n")

print(
  composition_simulation,
  n = Inf
)


cat("\n")
cat("— PROVISIONAL STAR PROFILE OF FLAGGED TAXA —\n\n")

if (nrow(non_vascular_star_profile) == 0L) {
  
  cat("No flagged taxa intersect provisional Stars.\n")
  
} else {
  
  print(
    non_vascular_star_profile,
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
# 24. STOP BEFORE WRITING IF VALIDATION FAILS
#
# This prevents a failed audit being written back as a canonical taxrev output.
# ==============================================================================

if (!all(validation$PASS)) {
  
  failed_checks <- validation %>%
    filter(!PASS) %>%
    pull(CHECK)
  
  stop(
    paste0(
      "VPJD taxonomic revision 02a validation failed.\n\n",
      "Failed checks:\n",
      paste0(
        " - ",
        failed_checks,
        collapse = "\n"
      ),
      "\n\nNo 02a DuckDB audit tables have been written."
    )
  )
}


# ==============================================================================
# 25. WRITE DUCKDB OUTPUTS
#
# Only vpjd_taxrev_02a_* tables are created/replaced.
# Canonical upstream tables remain untouched.
# ==============================================================================

DBI::dbWriteTable(
  con,
  TABLE_AUDIT,
  audit,
  overwrite = TRUE
)

DBI::dbWriteTable(
  con,
  TABLE_FAMILY_PROFILE,
  family_profile,
  overwrite = TRUE
)

DBI::dbWriteTable(
  con,
  TABLE_CANDIDATES,
  non_vascular_candidates,
  overwrite = TRUE
)

DBI::dbWriteTable(
  con,
  TABLE_SUMMARY,
  summary_table,
  overwrite = TRUE
)

DBI::dbWriteTable(
  con,
  TABLE_VALIDATION,
  validation,
  overwrite = TRUE
)


# ==============================================================================
# 26. WRITE CSV OUTPUTS
# ==============================================================================

readr::write_csv(
  audit,
  file.path(
    OUTPUT_DIR,
    "VPJD_02a_non_vascular_taxon_audit.csv"
  )
)

readr::write_csv(
  family_profile,
  file.path(
    OUTPUT_DIR,
    "VPJD_02a_family_scope_profile.csv"
  )
)

readr::write_csv(
  non_vascular_candidates,
  file.path(
    OUTPUT_DIR,
    "VPJD_02a_non_vascular_candidates.csv"
  )
)

readr::write_csv(
  non_vascular_group_profile,
  file.path(
    OUTPUT_DIR,
    "VPJD_02a_non_vascular_group_profile.csv"
  )
)

readr::write_csv(
  non_vascular_family_profile,
  file.path(
    OUTPUT_DIR,
    "VPJD_02a_non_vascular_family_profile.csv"
  )
)

readr::write_csv(
  non_vascular_rank_profile,
  file.path(
    OUTPUT_DIR,
    "VPJD_02a_non_vascular_rank_profile.csv"
  )
)

readr::write_csv(
  non_vascular_star_profile,
  file.path(
    OUTPUT_DIR,
    "VPJD_02a_non_vascular_star_profile.csv"
  )
)

readr::write_csv(
  missing_family_review,
  file.path(
    OUTPUT_DIR,
    "VPJD_02a_missing_family_review.csv"
  )
)

readr::write_csv(
  summary_table,
  file.path(
    OUTPUT_DIR,
    "VPJD_02a_summary.csv"
  )
)

readr::write_csv(
  composition_simulation,
  file.path(
    OUTPUT_DIR,
    "VPJD_02a_composition_simulation.csv"
  )
)

readr::write_csv(
  validation,
  file.path(
    OUTPUT_DIR,
    "VPJD_02a_validation.csv"
  )
)


# ==============================================================================
# 27. VERIFY DATABASE OUTPUTS
# ==============================================================================

tables_after_write <-
  DBI::dbListTables(con)

expected_output_tables <- c(
  TABLE_AUDIT,
  TABLE_FAMILY_PROFILE,
  TABLE_CANDIDATES,
  TABLE_SUMMARY,
  TABLE_VALIDATION
)

missing_output_tables <- setdiff(
  expected_output_tables,
  tables_after_write
)

if (length(missing_output_tables) > 0L) {
  stop(
    "02a completed validation but the following DuckDB output tables ",
    "could not be verified after writing:\n",
    paste(
      missing_output_tables,
      collapse = ", "
    )
  )
}


# ==============================================================================
# 28. RETURN RESULT OBJECT
# ==============================================================================

taxrev_02a_result <- list(
  
  audit =
    audit,
  
  family_profile =
    family_profile,
  
  non_vascular_candidates =
    non_vascular_candidates,
  
  non_vascular_group_profile =
    non_vascular_group_profile,
  
  non_vascular_family_profile =
    non_vascular_family_profile,
  
  non_vascular_rank_profile =
    non_vascular_rank_profile,
  
  non_vascular_star_profile =
    non_vascular_star_profile,
  
  missing_family_review =
    missing_family_review,
  
  summary =
    summary_table,
  
  composition_simulation =
    composition_simulation,
  
  validation =
    validation
)


# ==============================================================================
# 29. COMPLETION REPORT
# ==============================================================================

cat("\n")
cat("============================================================\n")
cat(
  "VPJD TAXONOMIC REVISION 02a v",
  VERSION,
  " COMPLETE\n",
  sep = ""
)
cat("============================================================\n")

cat(
  "Contemporary accepted concepts audited: ",
  format(
    EXPECTED_POPULATION,
    big.mark = ","
  ),
  "\n",
  sep = ""
)

cat(
  "Candidate moss exclusions: ",
  format(
    n_moss,
    big.mark = ","
  ),
  "\n",
  sep = ""
)

cat(
  "Candidate liverwort exclusions: ",
  format(
    n_liverwort,
    big.mark = ","
  ),
  "\n",
  sep = ""
)

cat(
  "Candidate hornwort exclusions: ",
  format(
    n_hornwort,
    big.mark = ","
  ),
  "\n",
  sep = ""
)

cat(
  "Total candidate non-vascular exclusions: ",
  format(
    n_non_vascular,
    big.mark = ","
  ),
  "\n",
  sep = ""
)

cat(
  "Missing-family review: ",
  format(
    n_missing_family,
    big.mark = ","
  ),
  "\n",
  sep = ""
)

cat(
  "Simulated vascular publication population: ",
  format(
    simulated_population,
    big.mark = ","
  ),
  "\n",
  sep = ""
)

cat("Canonical taxa removed: 0\n")
cat("Star allocations changed: FALSE\n")

cat(
  "DuckDB audit tables written: ",
  length(expected_output_tables),
  "\n",
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

cat(
  "Canonical audit: ",
  TABLE_AUDIT,
  "\n",
  sep = ""
)

cat(
  "Candidate exclusions: ",
  TABLE_CANDIDATES,
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
# 30. CLEAN DISCONNECT
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