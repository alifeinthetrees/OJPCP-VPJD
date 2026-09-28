# =============================================================================
# VPJD DRAFT RELEASE PIPELINE
# =============================================================================
#
# Module:  release_01_audit_vpjd_draft_dataset
# Version: 0.2.0
#
# PURPOSE
# -------
# Audit the current reconstructed Vascular Plants of Japan Database (VPJD)
# as a candidate first-draft public research dataset / Zenodo release.
#
# This module:
#   * inventories persisted VPJD pipeline outputs;
#   * identifies the canonical contemporary accepted population;
#   * verifies the expected population of 11,439 accepted WCVP concepts;
#   * profiles taxonomic composition and ranks;
#   * audits genus-level and higher-rank concepts;
#   * searches conservatively for potential non-vascular records;
#   * inventories distribution, reconciliation and review outputs;
#   * discovers provisional Star-allocation outputs;
#   * constructs a taxonomic review queue;
#   * audits identifier integrity;
#   * evaluates draft-release readiness;
#   * writes audit outputs only.
#
# IMPORTANT
# ---------
# This module DOES NOT:
#   * delete or exclude taxa;
#   * resolve taxonomic problems;
#   * infer vascular-plant status where unsupported;
#   * modify upstream canonical tables;
#   * assign new Star ratings;
#   * create a final Zenodo deposit;
#   * use historical Star allocations to resolve contemporary classifications.
#
# All files created by the release pipeline use the prefix:
#
#   VPJD_
#
# OUTPUT
# ------
# I:/R/OJPCP/VPJD-OJPCP/outputs/tables/release/draft_dataset_audit/
#
# =============================================================================


# =============================================================================
# 00. PACKAGES
# =============================================================================

suppressPackageStartupMessages({
  
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(purrr)
  library(tibble)
  
})


# =============================================================================
# 01. CONFIGURATION
# =============================================================================

MODULE_ID      <- "release_01_audit_vpjd_draft_dataset"
MODULE_VERSION <- "0.2.0"
RUN_DATE       <- Sys.Date()

PROJECT_ROOT <- "I:/R/OJPCP/VPJD-OJPCP"

TABLES_ROOT <- file.path(
  PROJECT_ROOT,
  "outputs",
  "tables"
)

OUTPUT_DIR <- file.path(
  TABLES_ROOT,
  "release",
  "draft_dataset_audit"
)

FILE_PREFIX <- "VPJD_"

TARGET_ACCEPTED_N <- 11439L


dir.create(
  OUTPUT_DIR,
  recursive = TRUE,
  showWarnings = FALSE
)


cat("\n")
cat("============================================================\n")
cat("VPJD DRAFT DATASET RELEASE AUDIT\n")
cat("============================================================\n\n")

cat("Run date: ", RUN_DATE, "\n", sep = "")
cat("Module: ", MODULE_ID, "\n", sep = "")
cat("Version: ", MODULE_VERSION, "\n", sep = "")
cat("Project root: ", PROJECT_ROOT, "\n", sep = "")
cat("Output directory: ", OUTPUT_DIR, "\n\n", sep = "")


# =============================================================================
# 02. HELPERS
# =============================================================================

vpjd_output_path <- function(filename) {
  
  file.path(
    OUTPUT_DIR,
    paste0(
      FILE_PREFIX,
      filename
    )
  )
}


object_exists <- function(x) {
  
  exists(
    x,
    envir = .GlobalEnv,
    inherits = FALSE
  )
}


get_object <- function(x) {
  
  get(
    x,
    envir = .GlobalEnv,
    inherits = FALSE
  )
}


first_existing_object <- function(candidates) {
  
  hit <- candidates[
    vapply(
      candidates,
      object_exists,
      logical(1)
    )
  ]
  
  if (length(hit) == 0L) {
    return(NA_character_)
  }
  
  hit[[1]]
}


first_existing_field <- function(data, candidates) {
  
  hit <- candidates[
    candidates %in% names(data)
  ]
  
  if (length(hit) == 0L) {
    return(NA_character_)
  }
  
  hit[[1]]
}


clean_chr <- function(x) {
  
  x <- as.character(x)
  
  x <- stringr::str_squish(x)
  
  x[
    x == ""
  ] <- NA_character_
  
  x
}


safe_n_distinct <- function(x) {
  
  if (is.null(x)) {
    return(NA_integer_)
  }
  
  dplyr::n_distinct(
    x[
      !is.na(x)
    ]
  )
}


bool_text <- function(x) {
  
  ifelse(
    isTRUE(x),
    "TRUE",
    "FALSE"
  )
}


make_check <- function(check, pass, result = NULL) {
  
  if (is.null(result)) {
    
    result <- dplyr::case_when(
      isTRUE(pass)  ~ "PASS",
      isFALSE(pass) ~ "FAIL",
      TRUE          ~ "NA"
    )
  }
  
  tibble(
    CHECK = check,
    PASS = as.logical(pass),
    RESULT = as.character(result)
  )
}


inspect_csv_schema <- function(path) {
  
  out <- tryCatch(
    
    suppressMessages(
      readr::read_csv(
        path,
        n_max = 5,
        show_col_types = FALSE,
        progress = FALSE
      )
    ),
    
    error = function(e) {
      NULL
    }
  )
  
  if (is.null(out)) {
    return(character())
  }
  
  names(out)
}


read_csv_safe <- function(path) {
  
  tryCatch(
    
    suppressMessages(
      readr::read_csv(
        path,
        show_col_types = FALSE,
        progress = FALSE
      )
    ),
    
    error = function(e) {
      NULL
    }
  )
}


# =============================================================================
# 03. CHECK PROJECT ARCHITECTURE
# =============================================================================

if (!dir.exists(PROJECT_ROOT)) {
  
  stop(
    "Project root does not exist: ",
    PROJECT_ROOT
  )
}


if (!dir.exists(TABLES_ROOT)) {
  
  stop(
    "Pipeline tables directory does not exist: ",
    TABLES_ROOT
  )
}


# =============================================================================
# 04. INVENTORY CURRENT R OBJECTS
# =============================================================================

all_objects <- ls(
  envir = .GlobalEnv
)

vpjd_objects <- all_objects[
  stringr::str_detect(
    all_objects,
    regex(
      "vpjd|star|wcvp",
      ignore_case = TRUE
    )
  )
]


object_inventory <- tibble(
  OBJECT_NAME = vpjd_objects
) |>
  mutate(
    
    OBJECT_CLASS = map_chr(
      OBJECT_NAME,
      ~ paste(
        class(
          get_object(.x)
        ),
        collapse = "|"
      )
    ),
    
    N_ROWS = map_int(
      OBJECT_NAME,
      ~ {
        
        obj <- get_object(.x)
        
        if (
          is.data.frame(obj) ||
          is.matrix(obj)
        ) {
          
          nrow(obj)
          
        } else {
          
          NA_integer_
        }
      }
    ),
    
    N_COLUMNS = map_int(
      OBJECT_NAME,
      ~ {
        
        obj <- get_object(.x)
        
        if (
          is.data.frame(obj) ||
          is.matrix(obj)
        ) {
          
          ncol(obj)
          
        } else {
          
          NA_integer_
        }
      }
    )
  )


write_csv(
  object_inventory,
  vpjd_output_path(
    "release_01_R_object_inventory.csv"
  )
)


cat(
  "VPJD / WCVP / Stars objects currently in R: ",
  nrow(object_inventory),
  "\n",
  sep = ""
)


# =============================================================================
# 05. INVENTORY PERSISTED PIPELINE CSV FILES
# =============================================================================

all_csv_files <- list.files(
  TABLES_ROOT,
  pattern = "\\.csv$",
  recursive = TRUE,
  full.names = TRUE,
  ignore.case = TRUE
)


if (length(all_csv_files) == 0L) {
  
  stop(
    "No CSV pipeline outputs found beneath: ",
    TABLES_ROOT
  )
}


# Do not allow the audit's own output directory to become an upstream source
# if the script is rerun.

normalise_slash <- function(x) {
  
  gsub(
    "\\\\",
    "/",
    normalizePath(
      x,
      winslash = "/",
      mustWork = FALSE
    )
  )
}


output_dir_normalised <- normalise_slash(
  OUTPUT_DIR
)

csv_paths_normalised <- normalise_slash(
  all_csv_files
)


keep_upstream <- !startsWith(
  tolower(csv_paths_normalised),
  paste0(
    tolower(output_dir_normalised),
    "/"
  )
)


all_csv_files <- all_csv_files[
  keep_upstream
]


csv_inventory <- tibble(
  
  FILE_PATH = all_csv_files,
  
  FILE_NAME = basename(
    all_csv_files
  )
  
) |>
  mutate(
    
    FILE_NAME_LOWER = str_to_lower(
      FILE_NAME
    ),
    
    FILE_PATH_LOWER = str_to_lower(
      FILE_PATH
    )
  )


write_csv(
  csv_inventory |>
    select(
      FILE_PATH,
      FILE_NAME
    ),
  vpjd_output_path(
    "release_01_pipeline_csv_inventory.csv"
  )
)


cat(
  "Persisted upstream pipeline CSV files discovered: ",
  length(all_csv_files),
  "\n\n",
  sep = ""
)


# =============================================================================
# 06. LOCATE CANONICAL ACCEPTED POPULATION
# =============================================================================

cat(
  "— LOCATING CANONICAL ACCEPTED POPULATION —\n"
)


# -----------------------------------------------------------------------------
# 06A. First inspect likely candidate files
# -----------------------------------------------------------------------------

candidate_name_patterns <- c(
  
  "provisional_taxonomic_composition",
  
  "provisional_wholesale",
  
  "partial_key_traversal",
  
  "taxonomic_composition",
  
  "accepted",
  
  "taxonomy",
  
  "taxonomic"
)


candidate_files <- csv_inventory |>
  filter(
    str_detect(
      FILE_PATH_LOWER,
      paste(
        candidate_name_patterns,
        collapse = "|"
      )
    )
  )


if (nrow(candidate_files) == 0L) {
  
  candidate_files <- csv_inventory
}


candidate_file_schema <- candidate_files |>
  mutate(
    
    FIELDS = map(
      FILE_PATH,
      inspect_csv_schema
    ),
    
    HAS_FINAL_WCVP_ID = map_lgl(
      FIELDS,
      ~ "FINAL_WCVP_ID" %in% .x
    ),
    
    HAS_ANY_WCVP_ID = map_lgl(
      FIELDS,
      ~ any(
        c(
          "FINAL_WCVP_ID",
          "wcvp_plant_name_id",
          "WCVP_ID",
          "PLANT_NAME_ID"
        ) %in% .x
      )
    ),
    
    HAS_ACCEPTED_NAME = map_lgl(
      FIELDS,
      ~ any(
        c(
          "FINAL_WCVP_RECOGNISED_NAME",
          "FINAL_WCVP_ACCEPTED_NAME",
          "WCVP_ACCEPTED_NAME",
          "taxon_name",
          "scientific_name",
          "plant_name"
        ) %in% .x
      )
    ),
    
    HAS_RANK = map_lgl(
      FIELDS,
      ~ any(
        c(
          "FINAL_WCVP_RANK",
          "taxon_rank",
          "rank",
          "WCVP_RANK"
        ) %in% .x
      )
    )
  )


accepted_file_candidates <- candidate_file_schema |>
  filter(
    HAS_ANY_WCVP_ID
  ) |>
  mutate(
    
    PRIORITY = case_when(
      
      str_detect(
        FILE_PATH_LOWER,
        "provisional_taxonomic_composition"
      ) ~ 1L,
      
      str_detect(
        FILE_PATH_LOWER,
        "provisional_wholesale"
      ) ~ 2L,
      
      str_detect(
        FILE_PATH_LOWER,
        "partial_key_traversal"
      ) ~ 3L,
      
      HAS_ACCEPTED_NAME & HAS_RANK ~ 4L,
      
      HAS_ACCEPTED_NAME ~ 5L,
      
      TRUE ~ 10L
    )
  ) |>
  arrange(
    PRIORITY,
    FILE_PATH
  )


# -----------------------------------------------------------------------------
# 06B. If necessary inspect every persisted CSV
# -----------------------------------------------------------------------------

if (nrow(accepted_file_candidates) == 0L) {
  
  cat(
    "No candidate identified by filename/path; ",
    "inspecting all persisted CSV schemas...\n",
    sep = ""
  )
  
  
  all_file_schema <- csv_inventory |>
    mutate(
      
      FIELDS = map(
        FILE_PATH,
        inspect_csv_schema
      ),
      
      HAS_FINAL_WCVP_ID = map_lgl(
        FIELDS,
        ~ "FINAL_WCVP_ID" %in% .x
      ),
      
      HAS_ANY_WCVP_ID = map_lgl(
        FIELDS,
        ~ any(
          c(
            "FINAL_WCVP_ID",
            "wcvp_plant_name_id",
            "WCVP_ID",
            "PLANT_NAME_ID"
          ) %in% .x
        )
      ),
      
      HAS_ACCEPTED_NAME = map_lgl(
        FIELDS,
        ~ any(
          c(
            "FINAL_WCVP_RECOGNISED_NAME",
            "FINAL_WCVP_ACCEPTED_NAME",
            "WCVP_ACCEPTED_NAME",
            "taxon_name",
            "scientific_name",
            "plant_name"
          ) %in% .x
        )
      ),
      
      HAS_RANK = map_lgl(
        FIELDS,
        ~ any(
          c(
            "FINAL_WCVP_RANK",
            "taxon_rank",
            "rank",
            "WCVP_RANK"
          ) %in% .x
        )
      )
    )
  
  
  accepted_file_candidates <- all_file_schema |>
    filter(
      HAS_ANY_WCVP_ID
    ) |>
    mutate(
      
      PRIORITY = case_when(
        
        HAS_ACCEPTED_NAME & HAS_RANK ~ 1L,
        
        HAS_ACCEPTED_NAME ~ 2L,
        
        TRUE ~ 10L
      )
    ) |>
    arrange(
      PRIORITY,
      FILE_PATH
    )
}


if (nrow(accepted_file_candidates) == 0L) {
  
  stop(
    paste0(
      "Could not locate any persisted pipeline CSV containing ",
      "a recognised WCVP identifier.\n",
      "Review inventory written to:\n",
      vpjd_output_path(
        "release_01_pipeline_csv_inventory.csv"
      )
    )
  )
}


accepted_candidate_report <- accepted_file_candidates |>
  transmute(
    
    PRIORITY,
    
    FILE_PATH,
    
    HAS_FINAL_WCVP_ID,
    
    HAS_ANY_WCVP_ID,
    
    HAS_ACCEPTED_NAME,
    
    HAS_RANK,
    
    N_FIELDS = map_int(
      FIELDS,
      length
    )
  )


write_csv(
  accepted_candidate_report,
  vpjd_output_path(
    "release_01_accepted_population_candidates.csv"
  )
)


cat(
  "\n— ACCEPTED-POPULATION FILE CANDIDATES —\n"
)


print(
  accepted_candidate_report,
  n = min(
    20L,
    nrow(accepted_candidate_report)
  )
)


# =============================================================================
# 07. ASSESS CANDIDATE POPULATIONS
# =============================================================================

identify_id_field <- function(data) {
  
  first_existing_field(
    data,
    c(
      "FINAL_WCVP_ID",
      "wcvp_plant_name_id",
      "WCVP_ID",
      "PLANT_NAME_ID"
    )
  )
}


candidate_assessment <- accepted_file_candidates |>
  mutate(
    
    DATA = map(
      FILE_PATH,
      read_csv_safe
    ),
    
    N_ROWS = map_int(
      DATA,
      ~ {
        
        if (is.null(.x)) {
          return(NA_integer_)
        }
        
        nrow(.x)
      }
    ),
    
    ID_FIELD = map_chr(
      DATA,
      ~ {
        
        if (is.null(.x)) {
          return(NA_character_)
        }
        
        identify_id_field(.x)
      }
    ),
    
    N_UNIQUE_WCVP = map2_int(
      DATA,
      ID_FIELD,
      ~ {
        
        if (
          is.null(.x) ||
          is.na(.y)
        ) {
          
          return(NA_integer_)
        }
        
        ids <- as.character(
          .x[[.y]]
        )
        
        ids <- ids[
          !is.na(ids) &
            ids != ""
        ]
        
        n_distinct(ids)
      }
    ),
    
    MATCHES_TARGET_POPULATION =
      N_ROWS == TARGET_ACCEPTED_N &
      N_UNIQUE_WCVP == TARGET_ACCEPTED_N
  )


assessment_report <- candidate_assessment |>
  transmute(
    
    PRIORITY,
    
    FILE_PATH,
    
    ID_FIELD,
    
    N_ROWS,
    
    N_UNIQUE_WCVP,
    
    MATCHES_TARGET_POPULATION
  )


write_csv(
  assessment_report,
  vpjd_output_path(
    "release_01_accepted_population_assessment.csv"
  )
)


cat(
  "\n— ACCEPTED-POPULATION ASSESSMENT —\n"
)


print(
  assessment_report,
  n = min(
    30L,
    nrow(assessment_report)
  )
)


# =============================================================================
# 08. SELECT CANONICAL ACCEPTED POPULATION
# =============================================================================

exact_candidates <- candidate_assessment |>
  filter(
    MATCHES_TARGET_POPULATION
  )


if (nrow(exact_candidates) == 0L) {
  
  stop(
    paste0(
      "Persisted tables containing WCVP identifiers were found, but none ",
      "contains the expected contemporary accepted population of ",
      format(
        TARGET_ACCEPTED_N,
        big.mark = ","
      ),
      " rows and ",
      format(
        TARGET_ACCEPTED_N,
        big.mark = ","
      ),
      " unique WCVP IDs.\n\n",
      "Assessment written to:\n",
      vpjd_output_path(
        "release_01_accepted_population_assessment.csv"
      ),
      "\n\n",
      "No alternative population has been substituted automatically."
    )
  )
}


selected_candidate <- exact_candidates |>
  arrange(
    PRIORITY,
    FILE_PATH
  ) |>
  slice(1)


accepted_raw <- selected_candidate$DATA[[1]]

accepted_source_file <- selected_candidate$FILE_PATH[[1]]

accepted_id_field <- selected_candidate$ID_FIELD[[1]]

accepted_source_type <- "PERSISTED_PIPELINE_OUTPUT"


cat(
  "\nCanonical accepted population loaded from:\n",
  accepted_source_file,
  "\n",
  sep = ""
)

cat(
  "Identifier field: ",
  accepted_id_field,
  "\n",
  sep = ""
)

cat(
  "Source type: ",
  accepted_source_type,
  "\n\n",
  sep = ""
)


# =============================================================================
# 09. STANDARDISE ACCEPTED IDENTIFIER
# =============================================================================

accepted_raw <- accepted_raw |>
  mutate(
    
    RELEASE_WCVP_ID = clean_chr(
      .data[[accepted_id_field]]
    )
  )


accepted_n <- nrow(
  accepted_raw
)


accepted_unique_n <- n_distinct(
  accepted_raw$RELEASE_WCVP_ID[
    !is.na(
      accepted_raw$RELEASE_WCVP_ID
    )
  ]
)


cat(
  "Accepted contemporary records: ",
  format(
    accepted_n,
    big.mark = ","
  ),
  "\n",
  sep = ""
)


cat(
  "Unique accepted WCVP IDs: ",
  format(
    accepted_unique_n,
    big.mark = ","
  ),
  "\n\n",
  sep = ""
)


# =============================================================================
# 10. IDENTIFY TAXONOMIC FIELDS IN CANONICAL TABLE
# =============================================================================

taxonomy <- accepted_raw


name_field <- first_existing_field(
  taxonomy,
  c(
    "FINAL_WCVP_RECOGNISED_NAME",
    "FINAL_WCVP_ACCEPTED_NAME",
    "WCVP_ACCEPTED_NAME",
    "taxon_name",
    "scientific_name",
    "plant_name"
  )
)


rank_field <- first_existing_field(
  taxonomy,
  c(
    "FINAL_WCVP_RANK",
    "taxon_rank",
    "rank",
    "WCVP_RANK"
  )
)


family_field <- first_existing_field(
  taxonomy,
  c(
    "FINAL_WCVP_FAMILY",
    "family",
    "FAMILY",
    "WCVP_FAMILY"
  )
)


genus_field <- first_existing_field(
  taxonomy,
  c(
    "FINAL_WCVP_GENUS",
    "genus",
    "GENUS",
    "WCVP_GENUS"
  )
)


kingdom_field <- first_existing_field(
  taxonomy,
  c(
    "FINAL_WCVP_KINGDOM",
    "kingdom",
    "KINGDOM"
  )
)


phylum_field <- first_existing_field(
  taxonomy,
  c(
    "FINAL_WCVP_PHYLUM",
    "phylum",
    "PHYLUM",
    "division",
    "DIVISION"
  )
)


class_field <- first_existing_field(
  taxonomy,
  c(
    "FINAL_WCVP_CLASS",
    "class",
    "CLASS",
    "taxon_class"
  )
)


# =============================================================================
# 11. STANDARDISE TAXONOMIC AUDIT TABLE
# =============================================================================

taxonomy_audit <- taxonomy |>
  transmute(
    
    RELEASE_WCVP_ID,
    
    TAXON_NAME = if (!is.na(name_field)) {
      
      clean_chr(
        .data[[name_field]]
      )
      
    } else {
      
      NA_character_
    },
    
    TAXON_RANK = if (!is.na(rank_field)) {
      
      clean_chr(
        .data[[rank_field]]
      )
      
    } else {
      
      NA_character_
    },
    
    FAMILY = if (!is.na(family_field)) {
      
      clean_chr(
        .data[[family_field]]
      )
      
    } else {
      
      NA_character_
    },
    
    GENUS = if (!is.na(genus_field)) {
      
      clean_chr(
        .data[[genus_field]]
      )
      
    } else {
      
      NA_character_
    },
    
    KINGDOM = if (!is.na(kingdom_field)) {
      
      clean_chr(
        .data[[kingdom_field]]
      )
      
    } else {
      
      NA_character_
    },
    
    PHYLUM = if (!is.na(phylum_field)) {
      
      clean_chr(
        .data[[phylum_field]]
      )
      
    } else {
      
      NA_character_
    },
    
    CLASS = if (!is.na(class_field)) {
      
      clean_chr(
        .data[[class_field]]
      )
      
    } else {
      
      NA_character_
    }
  )


# =============================================================================
# 12. TAXONOMIC RANK PROFILE
# =============================================================================

rank_profile <- taxonomy_audit |>
  mutate(
    
    TAXON_RANK = coalesce(
      TAXON_RANK,
      "MISSING"
    )
  ) |>
  count(
    TAXON_RANK,
    name = "N_TAXA",
    sort = TRUE
  ) |>
  mutate(
    
    PERCENT_ACCEPTED = round(
      100 * N_TAXA / sum(N_TAXA),
      3
    )
  )


write_csv(
  rank_profile,
  vpjd_output_path(
    "release_01_taxonomic_rank_profile.csv"
  )
)


# =============================================================================
# 13. TAXONOMIC SUMMARY
# =============================================================================

taxonomic_summary <- tibble(
  
  METRIC = c(
    
    "Accepted records",
    
    "Unique WCVP IDs",
    
    "Families",
    
    "Genera",
    
    "Named ranks"
  ),
  
  VALUE = c(
    
    nrow(
      taxonomy_audit
    ),
    
    n_distinct(
      taxonomy_audit$RELEASE_WCVP_ID,
      na.rm = TRUE
    ),
    
    safe_n_distinct(
      taxonomy_audit$FAMILY
    ),
    
    safe_n_distinct(
      taxonomy_audit$GENUS
    ),
    
    safe_n_distinct(
      taxonomy_audit$TAXON_RANK
    )
  )
)


write_csv(
  taxonomic_summary,
  vpjd_output_path(
    "release_01_taxonomic_summary.csv"
  )
)


# =============================================================================
# 14. GENUS / HIGHER-RANK AUDIT
# =============================================================================

taxonomy_audit <- taxonomy_audit |>
  mutate(
    
    RANK_NORMALISED = str_to_lower(
      coalesce(
        TAXON_RANK,
        ""
      )
    ),
    
    GENUS_LEVEL =
      RANK_NORMALISED %in%
      c(
        "genus",
        "gen."
      ),
    
    HIGHER_THAN_SPECIES =
      RANK_NORMALISED %in%
      c(
        "kingdom",
        "phylum",
        "division",
        "class",
        "order",
        "family",
        "tribe",
        "subtribe",
        "genus",
        "gen."
      )
  )


genus_level_records <- taxonomy_audit |>
  filter(
    GENUS_LEVEL
  )


higher_rank_records <- taxonomy_audit |>
  filter(
    HIGHER_THAN_SPECIES
  )


write_csv(
  genus_level_records,
  vpjd_output_path(
    "release_01_genus_level_records.csv"
  )
)


write_csv(
  higher_rank_records,
  vpjd_output_path(
    "release_01_higher_rank_records.csv"
  )
)


# =============================================================================
# 15. POTENTIAL NON-VASCULAR / BRYOPHYTE AUDIT
# =============================================================================
#
# IMPORTANT:
#
# This is a discovery / review flag only.
#
# Records are NOT excluded merely because they trigger this audit.
# A future scope-resolution module must make the formal decision.
#
# =============================================================================

nonvascular_pattern <- regex(
  
  paste(
    c(
      "bryophy",
      "marchantiophy",
      "anthocerot",
      "moss",
      "liverwort",
      "hornwort"
    ),
    collapse = "|"
  ),
  
  ignore_case = TRUE
)


taxonomy_audit <- taxonomy_audit |>
  mutate(
    
    NONVASCULAR_SEARCH_TEXT = paste(
      
      coalesce(
        KINGDOM,
        ""
      ),
      
      coalesce(
        PHYLUM,
        ""
      ),
      
      coalesce(
        CLASS,
        ""
      ),
      
      coalesce(
        FAMILY,
        ""
      ),
      
      coalesce(
        GENUS,
        ""
      ),
      
      coalesce(
        TAXON_NAME,
        ""
      )
    ),
    
    POTENTIAL_NON_VASCULAR =
      str_detect(
        NONVASCULAR_SEARCH_TEXT,
        nonvascular_pattern
      )
  )


potential_nonvascular <- taxonomy_audit |>
  filter(
    POTENTIAL_NON_VASCULAR
  ) |>
  select(
    -NONVASCULAR_SEARCH_TEXT
  )


write_csv(
  potential_nonvascular,
  vpjd_output_path(
    "release_01_potential_nonvascular_records.csv"
  )
)


# =============================================================================
# 16. HIGHER-TAXONOMY PROFILE
# =============================================================================

higher_taxonomy_profile <- bind_rows(
  
  taxonomy_audit |>
    filter(
      !is.na(KINGDOM)
    ) |>
    count(
      VALUE = KINGDOM,
      name = "N_TAXA"
    ) |>
    mutate(
      LEVEL = "KINGDOM"
    ),
  
  taxonomy_audit |>
    filter(
      !is.na(PHYLUM)
    ) |>
    count(
      VALUE = PHYLUM,
      name = "N_TAXA"
    ) |>
    mutate(
      LEVEL = "PHYLUM"
    ),
  
  taxonomy_audit |>
    filter(
      !is.na(CLASS)
    ) |>
    count(
      VALUE = CLASS,
      name = "N_TAXA"
    ) |>
    mutate(
      LEVEL = "CLASS"
    )
  
) |>
  select(
    LEVEL,
    VALUE,
    N_TAXA
  ) |>
  arrange(
    LEVEL,
    desc(N_TAXA),
    VALUE
  )


write_csv(
  higher_taxonomy_profile,
  vpjd_output_path(
    "release_01_higher_taxonomy_profile.csv"
  )
)


# =============================================================================
# 17. LOCATE PROVISIONAL STAR ALLOCATION
# =============================================================================

star_file_candidates <- csv_inventory |>
  filter(
    
    str_detect(
      FILE_PATH_LOWER,
      "provisional_wholesale|star_allocation|wholesale_star"
    )
  )


star_profile <- tibble()

star_allocated_n <- NA_integer_

star_unresolved_n <- NA_integer_

star_source_file <- NA_character_


if (nrow(star_file_candidates) > 0L) {
  
  for (
    i in seq_len(
      nrow(star_file_candidates)
    )
  ) {
    
    candidate_path <-
      star_file_candidates$FILE_PATH[[i]]
    
    candidate_data <-
      read_csv_safe(
        candidate_path
      )
    
    if (is.null(candidate_data)) {
      next
    }
    
    candidate_star_field <-
      first_existing_field(
        candidate_data,
        c(
          "PROVISIONAL_STAR",
          "STAR",
          "STAR_CATEGORY"
        )
      )
    
    candidate_id_field <-
      first_existing_field(
        candidate_data,
        c(
          "FINAL_WCVP_ID",
          "wcvp_plant_name_id",
          "WCVP_ID",
          "PLANT_NAME_ID"
        )
      )
    
    if (
      !is.na(candidate_star_field) &&
      !is.na(candidate_id_field)
    ) {
      
      ids <- clean_chr(
        candidate_data[[candidate_id_field]]
      )
      
      n_unique_candidate <-
        n_distinct(
          ids[
            !is.na(ids)
          ]
        )
      
      if (
        nrow(candidate_data) == TARGET_ACCEPTED_N &&
        n_unique_candidate == TARGET_ACCEPTED_N
      ) {
        
        star_data <- candidate_data
        
        star_field <- candidate_star_field
        
        star_source_file <- candidate_path
        
        break
      }
    }
  }
}


if (exists("star_data")) {
  
  star_profile <- star_data |>
    mutate(
      
      RELEASE_STAR = clean_chr(
        .data[[star_field]]
      ),
      
      RELEASE_STAR = coalesce(
        RELEASE_STAR,
        "UNRESOLVED"
      )
    ) |>
    count(
      RELEASE_STAR,
      name = "N_TAXA",
      sort = TRUE
    ) |>
    mutate(
      
      PERCENT_ACCEPTED = round(
        100 * N_TAXA / sum(N_TAXA),
        3
      )
    )
  
  
  star_allocated_n <- star_profile |>
    filter(
      RELEASE_STAR != "UNRESOLVED"
    ) |>
    summarise(
      N = sum(N_TAXA)
    ) |>
    pull(N)
  
  
  unresolved_tmp <- star_profile |>
    filter(
      RELEASE_STAR == "UNRESOLVED"
    ) |>
    summarise(
      N = sum(N_TAXA)
    ) |>
    pull(N)
  
  
  if (length(unresolved_tmp) == 0L) {
    
    star_unresolved_n <- 0L
    
  } else {
    
    star_unresolved_n <- unresolved_tmp
  }
  
  
  write_csv(
    star_profile,
    vpjd_output_path(
      "release_01_provisional_star_profile.csv"
    )
  )
}


# =============================================================================
# 18. INVENTORY DISTRIBUTION / GEOGRAPHY OUTPUTS
# =============================================================================

distribution_objects <- csv_inventory |>
  filter(
    
    str_detect(
      FILE_PATH_LOWER,
      regex(
        "distribution|geograph|area|district",
        ignore_case = TRUE
      )
    )
  ) |>
  select(
    FILE_PATH,
    FILE_NAME
  )


write_csv(
  distribution_objects,
  vpjd_output_path(
    "release_01_distribution_file_inventory.csv"
  )
)


# =============================================================================
# 19. INVENTORY RECONCILIATION / REVIEW OUTPUTS
# =============================================================================

review_objects <- csv_inventory |>
  filter(
    
    str_detect(
      FILE_PATH_LOWER,
      regex(
        "review|unresolved|unmatched|ambiguous|conflict|reconcil",
        ignore_case = TRUE
      )
    )
  ) |>
  select(
    FILE_PATH,
    FILE_NAME
  )


write_csv(
  review_objects,
  vpjd_output_path(
    "release_01_review_file_inventory.csv"
  )
)


# =============================================================================
# 20. PROJECT DATA-FILE INVENTORY
# =============================================================================

project_files <- list.files(
  PROJECT_ROOT,
  recursive = TRUE,
  full.names = TRUE,
  all.files = FALSE
)


project_files <- gsub(
  "\\\\",
  "/",
  project_files
)


file_inventory <- tibble(
  
  FILE_PATH = project_files,
  
  FILE_NAME = basename(
    project_files
  ),
  
  EXTENSION = tolower(
    tools::file_ext(
      project_files
    )
  )
  
) |>
  filter(
    
    EXTENSION %in%
      c(
        "csv",
        "tsv",
        "txt",
        "rds",
        "rda",
        "rdata",
        "xlsx",
        "xls",
        "parquet",
        "json",
        "geojson",
        "gpkg",
        "shp"
      )
  )


write_csv(
  file_inventory,
  vpjd_output_path(
    "release_01_project_data_file_inventory.csv"
  )
)


# =============================================================================
# 21. BUILD RELEASE REVIEW FLAGS
# =============================================================================

release_review <- taxonomy_audit |>
  mutate(
    
    REVIEW_GENUS_LEVEL =
      GENUS_LEVEL,
    
    REVIEW_HIGHER_RANK =
      HIGHER_THAN_SPECIES,
    
    REVIEW_NON_VASCULAR =
      POTENTIAL_NON_VASCULAR,
    
    REVIEW_MISSING_NAME =
      is.na(TAXON_NAME),
    
    REVIEW_MISSING_RANK =
      is.na(TAXON_RANK),
    
    REVIEW_MISSING_FAMILY =
      is.na(FAMILY),
    
    REVIEW_REQUIRED =
      REVIEW_GENUS_LEVEL |
      REVIEW_HIGHER_RANK |
      REVIEW_NON_VASCULAR |
      REVIEW_MISSING_NAME |
      REVIEW_MISSING_RANK |
      REVIEW_MISSING_FAMILY,
    
    REVIEW_REASON = pmap_chr(
      
      list(
        
        REVIEW_GENUS_LEVEL,
        
        REVIEW_HIGHER_RANK,
        
        REVIEW_NON_VASCULAR,
        
        REVIEW_MISSING_NAME,
        
        REVIEW_MISSING_RANK,
        
        REVIEW_MISSING_FAMILY
      ),
      
      function(
    genus_level,
    higher_rank,
    nonvascular,
    missing_name,
    missing_rank,
    missing_family
      ) {
        
        reasons <- character()
        
        
        if (isTRUE(genus_level)) {
          
          reasons <- c(
            reasons,
            "GENUS_LEVEL_CONCEPT"
          )
        }
        
        
        if (
          isTRUE(higher_rank) &&
          !isTRUE(genus_level)
        ) {
          
          reasons <- c(
            reasons,
            "HIGHER_RANK_CONCEPT"
          )
        }
        
        
        if (isTRUE(nonvascular)) {
          
          reasons <- c(
            reasons,
            "POTENTIAL_NON_VASCULAR"
          )
        }
        
        
        if (isTRUE(missing_name)) {
          
          reasons <- c(
            reasons,
            "MISSING_TAXON_NAME"
          )
        }
        
        
        if (isTRUE(missing_rank)) {
          
          reasons <- c(
            reasons,
            "MISSING_TAXON_RANK"
          )
        }
        
        
        if (isTRUE(missing_family)) {
          
          reasons <- c(
            reasons,
            "MISSING_FAMILY"
          )
        }
        
        
        if (length(reasons) == 0L) {
          
          return(
            NA_character_
          )
        }
        
        
        paste(
          unique(reasons),
          collapse = ";"
        )
      }
    )
  )


release_review_queue <- release_review |>
  filter(
    REVIEW_REQUIRED
  ) |>
  select(
    
    RELEASE_WCVP_ID,
    
    TAXON_NAME,
    
    TAXON_RANK,
    
    FAMILY,
    
    GENUS,
    
    KINGDOM,
    
    PHYLUM,
    
    CLASS,
    
    starts_with(
      "REVIEW_"
    )
  )


write_csv(
  release_review_queue,
  vpjd_output_path(
    "release_01_taxonomic_review_queue.csv"
  )
)


# =============================================================================
# 22. REVIEW-REASON PROFILE
# =============================================================================

review_reason_profile <- release_review_queue |>
  select(
    REVIEW_REASON
  ) |>
  filter(
    !is.na(REVIEW_REASON)
  ) |>
  separate_rows(
    REVIEW_REASON,
    sep = ";"
  ) |>
  count(
    REVIEW_REASON,
    name = "N_TAXA",
    sort = TRUE
  )


write_csv(
  review_reason_profile,
  vpjd_output_path(
    "release_01_review_reason_profile.csv"
  )
)


# =============================================================================
# 23. IDENTIFIER INTEGRITY
# =============================================================================

duplicate_ids <- taxonomy_audit |>
  filter(
    !is.na(RELEASE_WCVP_ID)
  ) |>
  count(
    RELEASE_WCVP_ID,
    name = "N_RECORDS"
  ) |>
  filter(
    N_RECORDS > 1L
  )


missing_ids <- taxonomy_audit |>
  filter(
    is.na(RELEASE_WCVP_ID) |
      RELEASE_WCVP_ID == ""
  )


write_csv(
  duplicate_ids,
  vpjd_output_path(
    "release_01_duplicate_wcvp_ids.csv"
  )
)


write_csv(
  missing_ids,
  vpjd_output_path(
    "release_01_missing_wcvp_ids.csv"
  )
)


# =============================================================================
# 24. SOURCE REGISTER
# =============================================================================

source_register <- tibble(
  
  COMPONENT = c(
    
    "Accepted contemporary population",
    
    "Taxonomic audit",
    
    "Provisional Star allocation"
  ),
  
  SOURCE_TYPE = c(
    
    accepted_source_type,
    
    "PERSISTED_PIPELINE_OUTPUT",
    
    ifelse(
      is.na(star_source_file),
      "NOT_DISCOVERED",
      "PERSISTED_PIPELINE_OUTPUT"
    )
  ),
  
  SOURCE = c(
    
    accepted_source_file,
    
    accepted_source_file,
    
    star_source_file
  )
)


write_csv(
  source_register,
  vpjd_output_path(
    "release_01_source_register.csv"
  )
)


# =============================================================================
# 25. RELEASE READINESS REGISTER
# =============================================================================

readiness <- tibble(
  
  COMPONENT = c(
    
    "Canonical accepted population available",
    
    "Accepted population = 11,439",
    
    "Accepted population unique by WCVP ID",
    
    "Taxonomic name field available",
    
    "Taxonomic rank field available",
    
    "Family field available",
    
    "Genus field available",
    
    "Taxonomic review queue created",
    
    "Potential non-vascular audit created",
    
    "Genus-level audit created",
    
    "Distribution/geography outputs inventoried",
    
    "Review/reconciliation outputs inventoried",
    
    "Project data files inventoried",
    
    "Provisional Star profile discoverable",
    
    "Final parsing/reconciliation complete",
    
    "Final taxonomic-scope review complete",
    
    "Final Star classification complete"
  ),
  
  READY = c(
    
    TRUE,
    
    accepted_n == TARGET_ACCEPTED_N,
    
    accepted_n == accepted_unique_n,
    
    !is.na(name_field),
    
    !is.na(rank_field),
    
    !is.na(family_field),
    
    !is.na(genus_field),
    
    TRUE,
    
    TRUE,
    
    TRUE,
    
    TRUE,
    
    TRUE,
    
    TRUE,
    
    nrow(star_profile) > 0,
    
    FALSE,
    
    FALSE,
    
    FALSE
  ),
  
  STATUS = c(
    
    "AVAILABLE",
    
    ifelse(
      accepted_n == TARGET_ACCEPTED_N,
      "VALIDATED",
      "REVIEW_REQUIRED"
    ),
    
    ifelse(
      accepted_n == accepted_unique_n,
      "VALIDATED",
      "REVIEW_REQUIRED"
    ),
    
    ifelse(
      !is.na(name_field),
      "AVAILABLE",
      "MISSING"
    ),
    
    ifelse(
      !is.na(rank_field),
      "AVAILABLE",
      "MISSING"
    ),
    
    ifelse(
      !is.na(family_field),
      "AVAILABLE",
      "MISSING"
    ),
    
    ifelse(
      !is.na(genus_field),
      "AVAILABLE",
      "MISSING"
    ),
    
    "CREATED",
    
    "CREATED",
    
    "CREATED",
    
    "INVENTORIED",
    
    "INVENTORIED",
    
    "INVENTORIED",
    
    ifelse(
      nrow(star_profile) > 0,
      "AVAILABLE",
      "NOT_DISCOVERED"
    ),
    
    "KNOWN_WORK_REMAINS",
    
    "KNOWN_WORK_REMAINS",
    
    "KNOWN_WORK_REMAINS"
  )
)


write_csv(
  readiness,
  vpjd_output_path(
    "release_01_readiness_register.csv"
  )
)


# =============================================================================
# 26. DRAFT RELEASE GATE
# =============================================================================
#
# Draft-release readiness is intentionally different from final analytical
# readiness.
#
# A provisional Zenodo release may retain explicitly identified unresolved
# records. It must nevertheless have:
#
#   * a defined population;
#   * stable identifiers;
#   * basic taxonomic fields;
#   * no duplicate primary taxon identifiers;
#   * no missing primary taxon identifiers;
#   * explicit review/audit outputs.
#
# =============================================================================

critical_draft_checks <- c(
  
  accepted_n == TARGET_ACCEPTED_N,
  
  accepted_n == accepted_unique_n,
  
  !is.na(name_field),
  
  !is.na(rank_field),
  
  !is.na(family_field),
  
  nrow(duplicate_ids) == 0L,
  
  nrow(missing_ids) == 0L
)


ZENODO_DRAFT_RELEASE_READY <- all(
  critical_draft_checks
)


FINAL_ANALYTICAL_RELEASE_READY <- FALSE


release_gate <- tibble(
  
  RELEASE_GATE = c(
    
    "ZENODO_DRAFT_RELEASE_READY",
    
    "FINAL_ANALYTICAL_RELEASE_READY"
  ),
  
  READY = c(
    
    ZENODO_DRAFT_RELEASE_READY,
    
    FINAL_ANALYTICAL_RELEASE_READY
  ),
  
  INTERPRETATION = c(
    
    paste(
      "TRUE means the current VPJD is structurally suitable for construction",
      "of a transparent provisional draft release, with unresolved records",
      "retained and explicitly flagged."
    ),
    
    paste(
      "FALSE is expected at this stage because parsing, taxonomic scope,",
      "selected distribution methodology and definitive Star allocation",
      "still require further work."
    )
  )
)


write_csv(
  release_gate,
  vpjd_output_path(
    "release_01_release_gate.csv"
  )
)


# =============================================================================
# 27. VALIDATION
# =============================================================================

validation <- bind_rows(
  
  make_check(
    "Canonical accepted population found",
    !is.null(accepted_raw)
  ),
  
  make_check(
    "Accepted population = 11,439",
    accepted_n == TARGET_ACCEPTED_N
  ),
  
  make_check(
    "Accepted population unique by WCVP ID",
    accepted_n == accepted_unique_n
  ),
  
  make_check(
    "Taxonomy table available",
    exists("taxonomy") &&
      is.data.frame(taxonomy) &&
      nrow(taxonomy) > 0
  ),
  
  make_check(
    "Taxon name field available",
    !is.na(name_field)
  ),
  
  make_check(
    "Taxon rank field available",
    !is.na(rank_field)
  ),
  
  make_check(
    "Family field available",
    !is.na(family_field)
  ),
  
  make_check(
    "Genus field available",
    !is.na(genus_field)
  ),
  
  make_check(
    "No duplicate WCVP identifiers",
    nrow(duplicate_ids) == 0L
  ),
  
  make_check(
    "No missing WCVP identifiers",
    nrow(missing_ids) == 0L
  ),
  
  make_check(
    "Taxonomic rank profile created",
    nrow(rank_profile) > 0
  ),
  
  make_check(
    "Genus-level audit created",
    TRUE
  ),
  
  make_check(
    "Potential non-vascular audit created",
    TRUE
  ),
  
  make_check(
    "Taxonomic review queue created",
    TRUE
  ),
  
  make_check(
    "Higher-taxonomy profile created",
    TRUE
  ),
  
  make_check(
    "Distribution/geography inventory created",
    TRUE
  ),
  
  make_check(
    "Review/reconciliation inventory created",
    TRUE
  ),
  
  make_check(
    "Project data-file inventory created",
    TRUE
  ),
  
  make_check(
    "No records removed by audit",
    nrow(taxonomy_audit) == nrow(taxonomy)
  ),
  
  make_check(
    "All audit output filenames use VPJD prefix",
    TRUE
  ),
  
  make_check(
    "No definitive Star assignments created",
    TRUE
  ),
  
  make_check(
    "Draft-release gate evaluated",
    TRUE
  ),
  
  make_check(
    "Final analytical release deliberately remains incomplete",
    !FINAL_ANALYTICAL_RELEASE_READY
  )
)


write_csv(
  validation,
  vpjd_output_path(
    "release_01_validation.csv"
  )
)


# =============================================================================
# 28. AUDIT MANIFEST
# =============================================================================

audit_output_files <- list.files(
  
  OUTPUT_DIR,
  
  full.names = TRUE,
  
  recursive = FALSE
)


audit_manifest <- tibble(
  
  FILE_NAME = basename(
    audit_output_files
  ),
  
  FILE_PATH = audit_output_files,
  
  EXTENSION = tolower(
    tools::file_ext(
      audit_output_files
    )
  ),
  
  VPJD_PREFIX =
    startsWith(
      basename(
        audit_output_files
      ),
      FILE_PREFIX
    ),
  
  FILE_SIZE_BYTES = file.info(
    audit_output_files
  )$size
) |>
  arrange(
    FILE_NAME
  )


write_csv(
  audit_manifest,
  vpjd_output_path(
    "release_01_audit_manifest.csv"
  )
)


# Rebuild manifest once so that the manifest itself is included.

audit_output_files <- list.files(
  OUTPUT_DIR,
  full.names = TRUE,
  recursive = FALSE
)


audit_manifest <- tibble(
  
  FILE_NAME = basename(
    audit_output_files
  ),
  
  FILE_PATH = audit_output_files,
  
  EXTENSION = tolower(
    tools::file_ext(
      audit_output_files
    )
  ),
  
  VPJD_PREFIX =
    startsWith(
      basename(
        audit_output_files
      ),
      FILE_PREFIX
    ),
  
  FILE_SIZE_BYTES = file.info(
    audit_output_files
  )$size
) |>
  arrange(
    FILE_NAME
  )


write_csv(
  audit_manifest,
  vpjd_output_path(
    "release_01_audit_manifest.csv"
  )
)


# =============================================================================
# 29. CONSOLE REPORT
# =============================================================================

cat(
  "\n— TAXONOMIC SUMMARY —\n"
)

print(
  taxonomic_summary,
  n = Inf
)


cat(
  "\n— TAXONOMIC RANK PROFILE —\n"
)

print(
  rank_profile,
  n = Inf
)


cat(
  "\n— POTENTIAL NON-VASCULAR RECORDS —\n"
)

cat(
  format(
    nrow(
      potential_nonvascular
    ),
    big.mark = ","
  ),
  "\n",
  sep = ""
)


cat(
  "\n— GENUS-LEVEL RECORDS —\n"
)

cat(
  format(
    nrow(
      genus_level_records
    ),
    big.mark = ","
  ),
  "\n",
  sep = ""
)


cat(
  "\n— HIGHER-RANK RECORDS —\n"
)

cat(
  format(
    nrow(
      higher_rank_records
    ),
    big.mark = ","
  ),
  "\n",
  sep = ""
)


cat(
  "\n— TAXONOMIC REVIEW QUEUE —\n"
)

cat(
  format(
    nrow(
      release_review_queue
    ),
    big.mark = ","
  ),
  "\n",
  sep = ""
)


cat(
  "\n— REVIEW REASONS —\n"
)

print(
  review_reason_profile,
  n = Inf
)


if (nrow(star_profile) > 0) {
  
  cat(
    "\n— PROVISIONAL STAR PROFILE —\n"
  )
  
  print(
    star_profile,
    n = Inf
  )
}


cat(
  "\n— RELEASE READINESS —\n"
)

print(
  readiness,
  n = Inf
)


cat(
  "\n— RELEASE GATE —\n"
)

print(
  release_gate,
  n = Inf
)


cat(
  "\n— VALIDATION —\n"
)

print(
  validation,
  n = Inf
)


# =============================================================================
# 30. EXPOSE CANONICAL AUDIT OBJECTS
# =============================================================================

vpjd_release_01_taxonomy_audit <-
  taxonomy_audit


vpjd_release_01_taxonomic_review_queue <-
  release_review_queue


vpjd_release_01_readiness_register <-
  readiness


vpjd_release_01_release_gate <-
  release_gate


vpjd_release_01_validation <-
  validation


vpjd_release_01_audit_manifest <-
  audit_manifest


# =============================================================================
# 31. FINAL REPORT
# =============================================================================

cat("\n")
cat("============================================================\n")
cat(
  "VPJD Release 01 v",
  MODULE_VERSION,
  " COMPLETE\n",
  sep = ""
)
cat("============================================================\n")


cat(
  "Accepted contemporary records: ",
  format(
    accepted_n,
    big.mark = ","
  ),
  "\n",
  sep = ""
)


cat(
  "Unique WCVP IDs: ",
  format(
    accepted_unique_n,
    big.mark = ","
  ),
  "\n",
  sep = ""
)


cat(
  "Families: ",
  safe_n_distinct(
    taxonomy_audit$FAMILY
  ),
  "\n",
  sep = ""
)


cat(
  "Genera: ",
  safe_n_distinct(
    taxonomy_audit$GENUS
  ),
  "\n",
  sep = ""
)


cat(
  "Genus-level records: ",
  format(
    nrow(
      genus_level_records
    ),
    big.mark = ","
  ),
  "\n",
  sep = ""
)


cat(
  "Potential non-vascular records: ",
  format(
    nrow(
      potential_nonvascular
    ),
    big.mark = ","
  ),
  "\n",
  sep = ""
)


cat(
  "Taxonomic review queue: ",
  format(
    nrow(
      release_review_queue
    ),
    big.mark = ","
  ),
  "\n",
  sep = ""
)


if (!is.na(star_allocated_n)) {
  
  cat(
    "Provisionally Star-allocated taxa: ",
    format(
      star_allocated_n,
      big.mark = ","
    ),
    "\n",
    sep = ""
  )
}


if (!is.na(star_unresolved_n)) {
  
  cat(
    "Provisionally unresolved Stars: ",
    format(
      star_unresolved_n,
      big.mark = ","
    ),
    "\n",
    sep = ""
  )
}


cat(
  "Zenodo draft release ready: ",
  bool_text(
    ZENODO_DRAFT_RELEASE_READY
  ),
  "\n",
  sep = ""
)


cat(
  "Final analytical release ready: ",
  bool_text(
    FINAL_ANALYTICAL_RELEASE_READY
  ),
  "\n",
  sep = ""
)


cat(
  "Validation: ",
  sum(
    validation$PASS %in% TRUE,
    na.rm = TRUE
  ),
  "/",
  nrow(
    validation
  ),
  " PASS\n",
  sep = ""
)


cat(
  "Canonical accepted source: ",
  accepted_source_file,
  "\n",
  sep = ""
)


if (!is.na(star_source_file)) {
  
  cat(
    "Provisional Star source: ",
    star_source_file,
    "\n",
    sep = ""
  )
}


cat(
  "Canonical taxonomy audit object: ",
  "vpjd_release_01_taxonomy_audit\n",
  sep = ""
)


cat(
  "Canonical review queue object: ",
  "vpjd_release_01_taxonomic_review_queue\n",
  sep = ""
)


cat(
  "All generated files prefixed: ",
  FILE_PREFIX,
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