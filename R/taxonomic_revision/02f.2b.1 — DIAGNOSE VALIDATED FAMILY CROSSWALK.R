# ==============================================================================
# VPJD TAXONOMIC REVISION
# 02f.2b.1 — DIAGNOSE VALIDATED FAMILY CROSSWALK
#
# Purpose:
#   Diagnose the family-name and biological-lineage fields in the validated
#   02b family crosswalk before reconstructing major-lineage classification
#   for the canonical VPJD population.
#
# READ ONLY
#
# Expected canonical population: 11,439
# Expected canonical families:   260
# ==============================================================================


# ==============================================================================
# 01. VERSION
# ==============================================================================

VERSION <- "0.1.1"


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
  library(purrr)
  
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
  "02f_infraspecific_other_ranks",
  "02f2b_family_crosswalk_reconstruction",
  "02f2b1_diagnostic"
)

dir.create(
  OUTPUT_DIR,
  recursive = TRUE,
  showWarnings = FALSE
)


# ==============================================================================
# 04. TABLE NAMES
# ==============================================================================

CROSSWALK_TABLE <-
  "vpjd_taxrev_02b_family_lineage_crosswalk"

CANONICAL_TABLE <-
  "vpjd_star_provisional_wholesale_allocation"

WCVP_TABLE <-
  "occurrence_wcvp_accepted_taxa"


# ==============================================================================
# 05. START REPORT
# ==============================================================================

cat("\n")
cat("============================================================\n")
cat(
  "VPJD TAXONOMIC REVISION 02f.2b.1 v",
  VERSION,
  "\n",
  sep = ""
)
cat("DIAGNOSE VALIDATED FAMILY CROSSWALK\n")
cat("============================================================\n\n")

cat("Database:\n")
cat(DB_PATH, "\n\n")

cat("Output directory:\n")
cat(OUTPUT_DIR, "\n\n")


# ==============================================================================
# 06. CHECK DATABASE
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
# 08. TABLE INVENTORY
# ==============================================================================

db_tables <- DBI::dbListTables(con)

required_tables <- c(
  CROSSWALK_TABLE,
  CANONICAL_TABLE,
  WCVP_TABLE
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

cat("Required tables located: 3 / 3\n")


# ==============================================================================
# 09. LOAD CROSSWALK
# ==============================================================================

crosswalk <- DBI::dbReadTable(
  con,
  CROSSWALK_TABLE
) |>
  as_tibble()


cat("\n")
cat("------------------------------------------------------------\n")
cat("02b FAMILY CROSSWALK\n")
cat("------------------------------------------------------------\n\n")

cat(
  "Rows: ",
  format(nrow(crosswalk), big.mark = ","),
  "\n",
  sep = ""
)

cat(
  "Columns: ",
  ncol(crosswalk),
  "\n",
  sep = ""
)


# ==============================================================================
# 10. FIELD INVENTORY
# ==============================================================================

field_inventory <- tibble(
  
  FIELD_NUMBER =
    seq_along(names(crosswalk)),
  
  FIELD_NAME =
    names(crosswalk),
  
  FIELD_CLASS =
    vapply(
      crosswalk,
      function(x) class(x)[1],
      character(1)
    ),
  
  N_NON_MISSING =
    vapply(
      crosswalk,
      function(x) {
        
        x <- str_squish(
          as.character(x)
        )
        
        sum(
          !is.na(x) &
            x != ""
        )
        
      },
      integer(1)
    ),
  
  N_DISTINCT =
    vapply(
      crosswalk,
      function(x) {
        
        x <- str_squish(
          as.character(x)
        )
        
        x <- x[
          !is.na(x) &
            x != ""
        ]
        
        length(unique(x))
        
      },
      integer(1)
    )
  
)


cat("\n")
cat("------------------------------------------------------------\n")
cat("CROSSWALK FIELD INVENTORY\n")
cat("------------------------------------------------------------\n\n")

print(
  field_inventory,
  n = Inf,
  width = Inf
)


# ==============================================================================
# 11. POTENTIALLY RELEVANT FIELDS
# ==============================================================================

relevant_pattern <- paste(
  c(
    "FAMILY",
    "LINEAGE",
    "VASCULAR",
    "NON.?VASCULAR",
    "MAJOR",
    "GROUP",
    "PLANT",
    "CLASS",
    "DIVISION",
    "PHYLUM",
    "CLADE",
    "METHOD",
    "SOURCE",
    "STATUS"
  ),
  collapse = "|"
)


relevant_fields <- names(crosswalk)[
  str_detect(
    toupper(names(crosswalk)),
    relevant_pattern
  )
]


cat("\n")
cat("------------------------------------------------------------\n")
cat("POTENTIALLY RELEVANT FIELDS\n")
cat("------------------------------------------------------------\n\n")

if (length(relevant_fields) == 0L) {
  
  cat("No relevant fields identified by field-name pattern.\n")
  
} else {
  
  cat(
    paste0(
      "  ",
      relevant_fields
    ),
    sep = "\n"
  )
  
  cat("\n")
  
}


# ==============================================================================
# 12. GENERIC FIELD PROFILER
# ==============================================================================

profile_field <- function(data, field_name) {
  
  values <- str_squish(
    as.character(
      data[[field_name]]
    )
  )
  
  tibble(
    VALUE = values
  ) |>
    
    filter(
      !is.na(VALUE),
      VALUE != ""
    ) |>
    
    count(
      VALUE,
      name = "N",
      sort = TRUE
    ) |>
    
    mutate(
      FIELD_NAME = field_name,
      .before = 1
    )
  
}


# ==============================================================================
# 13. PROFILE RELEVANT FIELDS
# ==============================================================================

relevant_value_profiles <- map_dfr(
  relevant_fields,
  function(fld) {
    
    profile_field(
      crosswalk,
      fld
    )
    
  }
)


cat("\n")
cat("------------------------------------------------------------\n")
cat("DISTINCT VALUES IN RELEVANT FIELDS\n")
cat("------------------------------------------------------------\n")


for (fld in relevant_fields) {
  
  cat("\nFIELD: ", fld, "\n", sep = "")
  cat("----------------------------------------\n")
  
  tmp <- relevant_value_profiles |>
    filter(
      FIELD_NAME == fld
    )
  
  print(
    tmp,
    n = Inf,
    width = Inf
  )
  
}


# ==============================================================================
# 14. FAMILY FIELDS
# ==============================================================================

family_fields <- names(crosswalk)[
  str_detect(
    toupper(names(crosswalk)),
    "FAMILY"
  )
]


cat("\n")
cat("------------------------------------------------------------\n")
cat("CROSSWALK FAMILY FIELDS\n")
cat("------------------------------------------------------------\n\n")


if (length(family_fields) == 0L) {
  
  cat("No family-named fields detected.\n")
  
} else {
  
  for (fld in family_fields) {
    
    values <- str_squish(
      as.character(
        crosswalk[[fld]]
      )
    )
    
    valid_values <- values[
      !is.na(values) &
        values != ""
    ]
    
    cat(
      "FIELD: ",
      fld,
      "\n",
      sep = ""
    )
    
    cat(
      "Non-empty records: ",
      format(
        length(valid_values),
        big.mark = ","
      ),
      "\n",
      sep = ""
    )
    
    cat(
      "Distinct values: ",
      format(
        n_distinct(valid_values),
        big.mark = ","
      ),
      "\n",
      sep = ""
    )
    
    cat("First 20 sorted values:\n")
    
    print(
      head(
        sort(
          unique(valid_values)
        ),
        20
      )
    )
    
    cat("\n")
    
  }
  
}


# ==============================================================================
# 15. LOAD CANONICAL VPJD POPULATION
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
      as.character(
        FINAL_WCVP_ID
      )
  )


if (
  nrow(canonical) !=
  EXPECTED_CANONICAL_POPULATION
) {
  
  stop(
    "Canonical VPJD population = ",
    nrow(canonical),
    "; expected ",
    EXPECTED_CANONICAL_POPULATION,
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


cat("\n")
cat(
  "Canonical population validated: ",
  format(
    nrow(canonical),
    big.mark = ","
  ),
  "\n",
  sep = ""
)


# ==============================================================================
# 16. LOAD WCVP FAMILY LOOKUP
# ==============================================================================

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
    "Required WCVP field(s) missing:\n",
    paste(
      missing_wcvp_fields,
      collapse = "\n"
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
    
    CANONICAL_FAMILY =
      str_squish(
        as.character(
          wcvp_family
        )
      )
    
  ) |>
  
  distinct(
    FINAL_WCVP_ID,
    .keep_all = TRUE
  )


# ==============================================================================
# 17. CANONICAL FAMILY POPULATION
# ==============================================================================

canonical_with_family <- canonical |>
  
  left_join(
    wcvp_family,
    by = "FINAL_WCVP_ID"
  )


canonical_families <- canonical_with_family |>
  
  filter(
    !is.na(CANONICAL_FAMILY),
    CANONICAL_FAMILY != ""
  ) |>
  
  distinct(
    CANONICAL_FAMILY
  ) |>
  
  arrange(
    CANONICAL_FAMILY
  )


n_canonical_families <-
  nrow(canonical_families)


cat("\n")
cat("------------------------------------------------------------\n")
cat("CANONICAL WCVP FAMILY POPULATION\n")
cat("------------------------------------------------------------\n\n")

cat(
  "Distinct canonical families: ",
  n_canonical_families,
  "\n",
  sep = ""
)


if (
  n_canonical_families !=
  EXPECTED_CANONICAL_FAMILIES
) {
  
  warning(
    "Canonical family population = ",
    n_canonical_families,
    "; expected ",
    EXPECTED_CANONICAL_FAMILIES,
    "."
  )
  
}


# ==============================================================================
# 18. FAMILY NORMALISATION
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


canonical_family_test <- canonical_families |>
  
  mutate(
    
    FAMILY_EXACT =
      CANONICAL_FAMILY,
    
    FAMILY_NORMALISED =
      normalise_family(
        CANONICAL_FAMILY
      )
    
  )


# ==============================================================================
# 19. TEST FAMILY FIELDS
# ==============================================================================

family_match_results <- map_dfr(
  family_fields,
  function(fld) {
    
    crosswalk_family <- tibble(
      
      CROSSWALK_FAMILY =
        str_squish(
          as.character(
            crosswalk[[fld]]
          )
        )
      
    ) |>
      
      filter(
        !is.na(CROSSWALK_FAMILY),
        CROSSWALK_FAMILY != ""
      ) |>
      
      distinct(
        CROSSWALK_FAMILY
      ) |>
      
      mutate(
        FAMILY_NORMALISED =
          normalise_family(
            CROSSWALK_FAMILY
          )
      )
    
    
    n_exact_matches <- sum(
      canonical_family_test$FAMILY_EXACT %in%
        crosswalk_family$CROSSWALK_FAMILY
    )
    
    
    n_normalised_matches <- sum(
      canonical_family_test$FAMILY_NORMALISED %in%
        crosswalk_family$FAMILY_NORMALISED
    )
    
    
    tibble(
      
      CROSSWALK_FIELD =
        fld,
      
      N_CROSSWALK_VALUES =
        nrow(crosswalk_family),
      
      N_CANONICAL_FAMILIES =
        nrow(canonical_family_test),
      
      N_EXACT_MATCHES =
        n_exact_matches,
      
      N_EXACT_UNMATCHED =
        nrow(canonical_family_test) -
        n_exact_matches,
      
      N_NORMALISED_MATCHES =
        n_normalised_matches,
      
      N_NORMALISED_UNMATCHED =
        nrow(canonical_family_test) -
        n_normalised_matches
      
    )
    
  }
)


cat("\n")
cat("------------------------------------------------------------\n")
cat("FAMILY FIELD MATCH TEST\n")
cat("------------------------------------------------------------\n\n")

print(
  family_match_results,
  n = Inf,
  width = Inf
)


# ==============================================================================
# 20. SELECT BEST FAMILY FIELD
# ==============================================================================

if (nrow(family_match_results) == 0L) {
  
  stop(
    "No candidate family fields available."
  )
  
}


best_family_match <- family_match_results |>
  
  arrange(
    desc(N_NORMALISED_MATCHES),
    desc(N_EXACT_MATCHES)
  ) |>
  
  slice_head(
    n = 1
  )


best_family_field <-
  best_family_match$CROSSWALK_FIELD[[1]]


cat("\n")

cat(
  "Best candidate family field: ",
  best_family_field,
  "\n",
  sep = ""
)

cat(
  "Exact matches: ",
  best_family_match$N_EXACT_MATCHES[[1]],
  " / ",
  EXPECTED_CANONICAL_FAMILIES,
  "\n",
  sep = ""
)

cat(
  "Normalised matches: ",
  best_family_match$N_NORMALISED_MATCHES[[1]],
  " / ",
  EXPECTED_CANONICAL_FAMILIES,
  "\n",
  sep = ""
)


# ==============================================================================
# 21. BUILD BEST CROSSWALK FAMILY LOOKUP
# ==============================================================================

# IMPORTANT:
# Correct R dynamic column extraction is:
#
#   crosswalk[[best_family_field]]
#
# NOT:
#
#   crosswalk[[best_family_field]
#   crosswalk[ [best_family_field]
# etc.

best_crosswalk_families <- tibble(
  
  CROSSWALK_FAMILY =
    str_squish(
      as.character(
        crosswalk[[best_family_field]]
      )
    )
  
) |>
  
  filter(
    !is.na(CROSSWALK_FAMILY),
    CROSSWALK_FAMILY != ""
  ) |>
  
  distinct(
    CROSSWALK_FAMILY
  ) |>
  
  mutate(
    FAMILY_NORMALISED =
      normalise_family(
        CROSSWALK_FAMILY
      )
  )


# ==============================================================================
# 22. UNMATCHED CANONICAL FAMILIES
# ==============================================================================

unmatched_families <- canonical_family_test |>
  
  anti_join(
    best_crosswalk_families,
    by = "FAMILY_NORMALISED"
  )


cat("\n")
cat("------------------------------------------------------------\n")
cat("UNMATCHED CANONICAL FAMILIES AFTER NORMALISATION\n")
cat("------------------------------------------------------------\n\n")


if (nrow(unmatched_families) == 0L) {
  
  cat(
    "All 260 canonical families match the 02b crosswalk after normalisation.\n"
  )
  
} else {
  
  print(
    unmatched_families,
    n = Inf,
    width = Inf
  )
  
}


# ==============================================================================
# 23. EXAMINE WHY EXACT FAMILY MATCHING FAILED
# ==============================================================================

family_format_comparison <- canonical_family_test |>
  
  inner_join(
    best_crosswalk_families,
    by = "FAMILY_NORMALISED"
  ) |>
  
  transmute(
    
    CANONICAL_FAMILY,
    
    CROSSWALK_FAMILY,
    
    FAMILY_NORMALISED,
    
    EXACT_MATCH =
      CANONICAL_FAMILY ==
      CROSSWALK_FAMILY
    
  ) |>
  
  arrange(
    CANONICAL_FAMILY
  )


cat("\n")
cat("------------------------------------------------------------\n")
cat("FAMILY REPRESENTATION COMPARISON\n")
cat("------------------------------------------------------------\n\n")

print(
  family_format_comparison |>
    slice_head(n = 30),
  n = 30,
  width = Inf
)


# ==============================================================================
# 24. CANDIDATE BIOLOGICAL CLASSIFICATION FIELDS
# ==============================================================================

classification_name_pattern <- paste(
  c(
    "LINEAGE",
    "VASCULAR",
    "GROUP",
    "MAJOR",
    "DIVISION",
    "PHYLUM",
    "CLADE",
    "CLASS"
  ),
  collapse = "|"
)


candidate_classification_fields <- names(crosswalk)[
  
  str_detect(
    toupper(names(crosswalk)),
    classification_name_pattern
  )
  
]


# Remove obvious metadata fields.

candidate_classification_fields <-
  candidate_classification_fields[
    
    !str_detect(
      toupper(candidate_classification_fields),
      "METHOD|SOURCE|STATUS|NOTE|COMMENT"
    )
    
  ]


cat("\n")
cat("------------------------------------------------------------\n")
cat("CANDIDATE BIOLOGICAL CLASSIFICATION FIELDS\n")
cat("------------------------------------------------------------\n\n")


if (length(candidate_classification_fields) == 0L) {
  
  cat(
    "No candidate biological classification fields identified by name.\n"
  )
  
} else {
  
  cat(
    paste0(
      "  ",
      candidate_classification_fields
    ),
    sep = "\n"
  )
  
  cat("\n")
  
}


# ==============================================================================
# 25. SEARCH EVERY CROSSWALK FIELD FOR BIOLOGICAL TERMINOLOGY
# ==============================================================================

biological_regex <- paste(
  c(
    "angiosperm",
    "gymnosperm",
    "fern",
    "monilophyte",
    "lycophyte",
    "vascular",
    "non-vascular",
    "nonvascular",
    "bryophyte",
    "moss",
    "liverwort",
    "hornwort"
  ),
  collapse = "|"
)


biological_field_hits <- map_dfr(
  names(crosswalk),
  function(fld) {
    
    values <- str_squish(
      as.character(
        crosswalk[[fld]]
      )
    )
    
    valid <- (
      !is.na(values) &
        values != ""
    )
    
    if (!any(valid)) {
      return(tibble())
    }
    
    
    hit <- str_detect(
      str_to_lower(values),
      biological_regex
    )
    
    
    if (!any(hit, na.rm = TRUE)) {
      return(tibble())
    }
    
    
    tibble(
      FIELD_NAME = fld,
      VALUE = values[hit]
    ) |>
      
      filter(
        !is.na(VALUE),
        VALUE != ""
      ) |>
      
      count(
        FIELD_NAME,
        VALUE,
        name = "N",
        sort = TRUE
      )
    
  }
)


cat("\n")
cat("------------------------------------------------------------\n")
cat("BIOLOGICAL LINEAGE TERMINOLOGY FOUND IN CROSSWALK\n")
cat("------------------------------------------------------------\n\n")


if (nrow(biological_field_hits) == 0L) {
  
  cat(
    "No explicit biological lineage terminology detected.\n"
  )
  
} else {
  
  print(
    biological_field_hits,
    n = Inf,
    width = Inf
  )
  
}


# ==============================================================================
# 26. PROFILE CANDIDATE CLASSIFICATION FIELDS
# ==============================================================================

classification_profiles <- map_dfr(
  candidate_classification_fields,
  function(fld) {
    
    profile_field(
      crosswalk,
      fld
    )
    
  }
)


cat("\n")
cat("------------------------------------------------------------\n")
cat("CANDIDATE CLASSIFICATION FIELD VALUES\n")
cat("------------------------------------------------------------\n")


if (nrow(classification_profiles) == 0L) {
  
  cat("\nNo candidate classification profiles available.\n")
  
} else {
  
  for (fld in candidate_classification_fields) {
    
    cat("\nFIELD: ", fld, "\n", sep = "")
    cat("----------------------------------------\n")
    
    tmp <- classification_profiles |>
      filter(
        FIELD_NAME == fld
      )
    
    print(
      tmp,
      n = Inf,
      width = Inf
    )
    
  }
  
}


# ==============================================================================
# 27. CLASSIFICATION_METHOD PROFILE
# ==============================================================================

cat("\n")
cat("------------------------------------------------------------\n")
cat("CLASSIFICATION_METHOD CHECK\n")
cat("------------------------------------------------------------\n\n")


if (
  "CLASSIFICATION_METHOD" %in%
  names(crosswalk)
) {
  
  method_profile <- profile_field(
    crosswalk,
    "CLASSIFICATION_METHOD"
  )
  
  print(
    method_profile,
    n = Inf,
    width = Inf
  )
  
  cat("\n")
  cat(
    "CLASSIFICATION_METHOD is metadata and is excluded from automatic\n"
  )
  cat(
    "selection as the biological lineage field.\n"
  )
  
} else {
  
  method_profile <- tibble()
  
  cat(
    "CLASSIFICATION_METHOD field not present.\n"
  )
  
}


# ==============================================================================
# 28. CROSSWALK SAMPLE
# ==============================================================================

display_fields <- unique(
  c(
    best_family_field,
    candidate_classification_fields,
    "CLASSIFICATION_METHOD"
  )
)

display_fields <- intersect(
  display_fields,
  names(crosswalk)
)


cat("\n")
cat("------------------------------------------------------------\n")
cat("CROSSWALK SAMPLE — FIRST 30 ROWS\n")
cat("------------------------------------------------------------\n\n")


print(
  crosswalk |>
    
    select(
      all_of(display_fields)
    ) |>
    
    slice_head(
      n = 30
    ),
  
  n = 30,
  width = Inf
)


# ==============================================================================
# 29. FAMILY × CLASSIFICATION PROFILE
# ==============================================================================

family_classification_profiles <- tibble()


if (
  length(candidate_classification_fields) > 0L
) {
  
  family_classification_profiles <- map_dfr(
    candidate_classification_fields,
    function(class_field) {
      
      tibble(
        
        FAMILY =
          str_squish(
            as.character(
              crosswalk[[best_family_field]]
            )
          ),
        
        CLASSIFICATION =
          str_squish(
            as.character(
              crosswalk[[class_field]]
            )
          )
        
      ) |>
        
        filter(
          !is.na(FAMILY),
          FAMILY != "",
          !is.na(CLASSIFICATION),
          CLASSIFICATION != ""
        ) |>
        
        distinct(
          FAMILY,
          CLASSIFICATION
        ) |>
        
        count(
          CLASSIFICATION,
          name = "N_FAMILIES",
          sort = TRUE
        ) |>
        
        mutate(
          CLASSIFICATION_FIELD =
            class_field,
          .before = 1
        )
      
    }
  )
  
}


cat("\n")
cat("------------------------------------------------------------\n")
cat("FAMILY COUNTS BY CANDIDATE CLASSIFICATION\n")
cat("------------------------------------------------------------\n\n")


if (
  nrow(family_classification_profiles) == 0L
) {
  
  cat(
    "No family × classification profile could be generated.\n"
  )
  
} else {
  
  print(
    family_classification_profiles,
    n = Inf,
    width = Inf
  )
  
}


# ==============================================================================
# 30. WRITE CSV OUTPUTS
# ==============================================================================

write_csv(
  field_inventory,
  file.path(
    OUTPUT_DIR,
    "VPJD_02f2b1_crosswalk_field_inventory.csv"
  )
)


write_csv(
  relevant_value_profiles,
  file.path(
    OUTPUT_DIR,
    "VPJD_02f2b1_relevant_field_values.csv"
  )
)


write_csv(
  family_match_results,
  file.path(
    OUTPUT_DIR,
    "VPJD_02f2b1_family_match_results.csv"
  )
)


write_csv(
  unmatched_families,
  file.path(
    OUTPUT_DIR,
    "VPJD_02f2b1_unmatched_canonical_families.csv"
  )
)


write_csv(
  family_format_comparison,
  file.path(
    OUTPUT_DIR,
    "VPJD_02f2b1_family_format_comparison.csv"
  )
)


write_csv(
  biological_field_hits,
  file.path(
    OUTPUT_DIR,
    "VPJD_02f2b1_biological_lineage_hits.csv"
  )
)


write_csv(
  classification_profiles,
  file.path(
    OUTPUT_DIR,
    "VPJD_02f2b1_candidate_classification_profiles.csv"
  )
)


write_csv(
  family_classification_profiles,
  file.path(
    OUTPUT_DIR,
    "VPJD_02f2b1_family_classification_profiles.csv"
  )
)


write_csv(
  method_profile,
  file.path(
    OUTPUT_DIR,
    "VPJD_02f2b1_classification_method_profile.csv"
  )
)


# ==============================================================================
# 31. VALIDATION
# ==============================================================================

best_exact_matches <-
  max(
    family_match_results$N_EXACT_MATCHES,
    na.rm = TRUE
  )


best_normalised_matches <-
  max(
    family_match_results$N_NORMALISED_MATCHES,
    na.rm = TRUE
  )


validation <- tibble(
  
  CHECK = c(
    
    "Crosswalk table present",
    
    "Canonical allocation table present",
    
    "WCVP lookup table present",
    
    "Canonical population = 11,439",
    
    "Canonical population unique by WCVP ID",
    
    "Canonical family population = 260",
    
    "Crosswalk contains 260 rows",
    
    "Family field identified",
    
    "Best family field = FAMILY",
    
    "All 260 canonical families match after normalisation",
    
    "No canonical families unmatched after normalisation",
    
    "CLASSIFICATION_METHOD excluded as biological lineage",
    
    "Database remains read-only"
    
  ),
  
  PASS = c(
    
    CROSSWALK_TABLE %in%
      db_tables,
    
    CANONICAL_TABLE %in%
      db_tables,
    
    WCVP_TABLE %in%
      db_tables,
    
    nrow(canonical) ==
      EXPECTED_CANONICAL_POPULATION,
    
    n_distinct(
      canonical$FINAL_WCVP_ID
    ) ==
      EXPECTED_CANONICAL_POPULATION,
    
    n_canonical_families ==
      EXPECTED_CANONICAL_FAMILIES,
    
    nrow(crosswalk) ==
      EXPECTED_CANONICAL_FAMILIES,
    
    !is.na(best_family_field),
    
    best_family_field ==
      "FAMILY",
    
    best_normalised_matches ==
      EXPECTED_CANONICAL_FAMILIES,
    
    nrow(unmatched_families) ==
      0L,
    
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


write_csv(
  validation,
  file.path(
    OUTPUT_DIR,
    "VPJD_02f2b1_validation.csv"
  )
)


# ==============================================================================
# 32. DIAGNOSTIC SUMMARY
# ==============================================================================

diagnostic_summary <- tibble(
  
  METRIC = c(
    
    "Canonical VPJD concepts",
    
    "Canonical families",
    
    "02b crosswalk rows",
    
    "Crosswalk fields",
    
    "Family-like fields",
    
    "Candidate biological classification fields",
    
    "Fields containing biological lineage terminology",
    
    "Best exact family matches",
    
    "Best normalised family matches",
    
    "Unmatched canonical families after normalisation"
    
  ),
  
  N = c(
    
    nrow(canonical),
    
    n_canonical_families,
    
    nrow(crosswalk),
    
    ncol(crosswalk),
    
    length(family_fields),
    
    length(candidate_classification_fields),
    
    n_distinct(
      biological_field_hits$FIELD_NAME
    ),
    
    best_exact_matches,
    
    best_normalised_matches,
    
    nrow(unmatched_families)
    
  )
  
)


write_csv(
  diagnostic_summary,
  file.path(
    OUTPUT_DIR,
    "VPJD_02f2b1_diagnostic_summary.csv"
  )
)


# ==============================================================================
# 33. FINAL REPORT
# ==============================================================================

cat("\n")
cat("============================================================\n")
cat("02f.2b.1 DIAGNOSTIC SUMMARY\n")
cat("============================================================\n\n")

print(
  diagnostic_summary,
  n = Inf
)

cat("\n")

cat(
  "Selected family field: ",
  best_family_field,
  "\n",
  sep = ""
)

cat(
  "Exact family matches: ",
  best_exact_matches,
  " / ",
  EXPECTED_CANONICAL_FAMILIES,
  "\n",
  sep = ""
)

cat(
  "Normalised family matches: ",
  best_normalised_matches,
  " / ",
  EXPECTED_CANONICAL_FAMILIES,
  "\n",
  sep = ""
)

cat(
  "Unmatched canonical families: ",
  nrow(unmatched_families),
  "\n",
  sep = ""
)


if (
  best_normalised_matches ==
  EXPECTED_CANONICAL_FAMILIES
) {
  
  cat(
    "FAMILY JOIN STATUS: FULL 260/260 MATCH AVAILABLE\n"
  )
  
} else {
  
  cat(
    "FAMILY JOIN STATUS: REVIEW REQUIRED\n"
  )
  
}


if (
  nrow(biological_field_hits) > 0L
) {
  
  cat(
    "BIOLOGICAL LINEAGE EVIDENCE: FOUND\n"
  )
  
} else {
  
  cat(
    "BIOLOGICAL LINEAGE EVIDENCE: NOT YET IDENTIFIED\n"
  )
  
}


cat(
  "CLASSIFICATION_METHOD: EXCLUDED AS AUTOMATIC LINEAGE FIELD\n"
)

cat(
  "Validation: ",
  sum(validation$PASS),
  "/",
  nrow(validation),
  " PASS\n",
  sep = ""
)


# ==============================================================================
# 34. CLEAN DISCONNECT
# ==============================================================================

DBI::dbDisconnect(
  con,
  shutdown = TRUE
)

rm(con)

gc()


# ==============================================================================
# 35. COMPLETION REPORT
# ==============================================================================

cat("\n")
cat("============================================================\n")

cat(
  "VPJD TAXONOMIC REVISION 02f.2b.1 v",
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

cat("Database modified: FALSE\n")
cat("Canonical taxonomy modified: FALSE\n")
cat("Star allocations modified: FALSE\n")
cat("Taxa removed: 0\n")
cat("Diagnostic CSV outputs written: TRUE\n")
cat("DuckDB connection closed cleanly.\n")

cat("\n")
cat("============================================================\n")


# ==============================================================================
# END
# ==============================================================================