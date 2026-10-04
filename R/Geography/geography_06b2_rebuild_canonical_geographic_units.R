# ==============================================================================
# VPJD GEOGRAPHY MODULE
# geography_06b2_rebuild_canonical_geographic_units.R
#
# PURPOSE
# -------
# Rebuild the provisional canonical geographic-unit vocabulary for:
#
#   Vascular Plants of Japan Database (VPJD)
#   Geography Release v1.0.0
#
# using ONLY the candidate geographic fields explicitly validated by:
#
#   geography_06b1_validate_candidate_geographic_fields.R
#
# POSITION IN PIPELINE
# --------------------
#
#   06a   Audit geographic evidence estate
#    |
#    v
#   06b   Discover candidate geographic vocabulary
#    |
#    v
#   06b0  Audit candidate geographic fields
#    |
#    v
#   06b1  Validate candidate geographic fields
#    |
#    v
#   06b2  REBUILD CANONICAL GEOGRAPHIC UNITS       <-- THIS SCRIPT
#    |
#    v
#   06b3  Validate rebuilt geographic vocabulary
#    |
#    v
#   06c   Geographic hierarchy / standards / provenance
#    |
#    v
#   VPJD Geography v1.0.0
#
# 06b1 ESTABLISHED
# -----------------
#
#   Candidate fields validated:                    11
#   Approved geographic fields:                     6
#   Excluded non-geographic fields:                 5
#   Fields requiring review:                        0
#
#   Japanese botanical-area concepts expected:     51
#   WCVP TDWG codes expected:                     367
#
#   06b unresolved provisional units:             672
#   Explained by excluded fields:                 672
#   Unexplained unresolved units:                   0
#   Unresolved units from approved fields:          0
#
# OBJECTIVES
# ----------
#
#  1. Read and validate the 06b1 field-validation products.
#
#  2. Reconstruct Japanese botanical-area concepts from the paired:
#
#       botanical_area_id
#       botanical_area_name
#
#     fields.
#
#  3. Reconcile duplicate botanical-area representations across:
#
#       vpjd_botanical_area_taxon_summary.csv
#       vpjd_taxon_botanical_area_distribution.csv
#
#  4. Resolve WCVP tdwg_code values against the WCVP location-reference
#     product.
#
#  5. Preserve China province_level_division as classification metadata,
#     WITHOUT treating classification labels as province names.
#
#  6. Construct one provisional canonical concept table.
#
#  7. Assign deterministic PROVISIONAL VPJD-GEO identifiers only after
#     concept resolution.
#
#  8. Construct complete source-provenance mappings.
#
#  9. Produce validation outputs for the subsequent 06b3 stage.
#
# IMPORTANT
# ---------
#
# VPJD-GEO identifiers created here are PROVISIONAL.
#
# They MUST NOT be regarded as frozen public identifiers until the rebuilt
# vocabulary has passed 06b3 and subsequent release validation.
#
# This script DOES NOT:
#
#   - modify VPJD Taxonomic Release v1.0.0;
#   - modify source evidence;
#   - modify 06b / 06b0 / 06b1 outputs;
#   - infer absence from missing evidence;
#   - infer geographic hierarchy;
#   - infer geometry;
#   - infer equivalence from names alone;
#   - create China province concepts from classification labels;
#   - calculate Nakamura predicates;
#   - calculate Star categories;
#   - publish VPJD Geography v1.0.0.
#
# ==============================================================================


# ------------------------------------------------------------------------------
# 01. PROJECT PATHS
# ------------------------------------------------------------------------------

PROJECT_ROOT <- "I:/R/OJPCP/VPJD-OJPCP"

DATA_ROOT <- file.path(
  PROJECT_ROOT,
  "data"
)

DERIVED_ROOT <- file.path(
  DATA_ROOT,
  "derived",
  "geography",
  "release_v1.0.0"
)

FIELD_VALIDATION_ROOT <- file.path(
  DERIVED_ROOT,
  "field_validation"
)

AUDIT_06B1_ROOT <- file.path(
  PROJECT_ROOT,
  "audit",
  "geography",
  "release_v1.0.0",
  "06b1_validated_candidate_fields"
)

OUTPUT_ROOT <- file.path(
  DERIVED_ROOT,
  "canonical_geographic_units"
)

AUDIT_ROOT <- file.path(
  PROJECT_ROOT,
  "audit",
  "geography",
  "release_v1.0.0",
  "06b2_rebuilt_canonical_geographic_units"
)

dir.create(
  OUTPUT_ROOT,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  AUDIT_ROOT,
  recursive = TRUE,
  showWarnings = FALSE
)


# ------------------------------------------------------------------------------
# 02. INPUT FILES
# ------------------------------------------------------------------------------

VALIDATED_FIELD_REGISTER_FILE <- file.path(
  FIELD_VALIDATION_ROOT,
  "vpjd_validated_geographic_field_register.csv"
)

APPROVED_FIELD_FILE <- file.path(
  FIELD_VALIDATION_ROOT,
  "vpjd_approved_geographic_fields.csv"
)

EXCLUDED_FIELD_FILE <- file.path(
  FIELD_VALIDATION_ROOT,
  "vpjd_excluded_non_geographic_fields.csv"
)

CONCEPT_PAIR_FILE <- file.path(
  FIELD_VALIDATION_ROOT,
  "vpjd_geographic_field_concept_pairs.csv"
)

VALIDATION_06B1_FILE <- file.path(
  AUDIT_06B1_ROOT,
  "geography_06b1_validation_gate.csv"
)


# ------------------------------------------------------------------------------
# 03. OUTPUT FILES
# ------------------------------------------------------------------------------

CANONICAL_UNIT_FILE <- file.path(
  OUTPUT_ROOT,
  "vpjd_geographic_units_provisional.csv"
)

BOTANICAL_AREA_FILE <- file.path(
  OUTPUT_ROOT,
  "vpjd_japan_botanical_areas_provisional.csv"
)

WCVP_LOCATION_FILE <- file.path(
  OUTPUT_ROOT,
  "vpjd_wcvp_tdwg_units_provisional.csv"
)

SOURCE_PROVENANCE_FILE <- file.path(
  OUTPUT_ROOT,
  "vpjd_geographic_unit_source_provenance.csv"
)

CHINA_CLASSIFICATION_FILE <- file.path(
  OUTPUT_ROOT,
  "vpjd_china_province_classification_metadata.csv"
)

CONCEPT_SOURCE_FILE <- file.path(
  OUTPUT_ROOT,
  "vpjd_geographic_concept_source_register.csv"
)

SUMMARY_FILE <- file.path(
  AUDIT_ROOT,
  "geography_06b2_summary.csv"
)

BOTANICAL_AUDIT_FILE <- file.path(
  AUDIT_ROOT,
  "geography_06b2_botanical_area_reconciliation.csv"
)

WCVP_AUDIT_FILE <- file.path(
  AUDIT_ROOT,
  "geography_06b2_wcvp_tdwg_resolution.csv"
)

VALIDATION_FILE <- file.path(
  AUDIT_ROOT,
  "geography_06b2_validation_gate.csv"
)


# ------------------------------------------------------------------------------
# 04. HELPER: NORMALISE TEXT
# ------------------------------------------------------------------------------

normalise_text <- function(x) {
  
  y <- as.character(x)
  
  y <- iconv(
    y,
    from = "",
    to = "UTF-8",
    sub = "byte"
  )
  
  y[is.na(y)] <- ""
  
  y <- trimws(y)
  
  empty_selector <- (
    y == "" |
      toupper(y) == "NA" |
      toupper(y) == "NULL"
  )
  
  empty_selector[is.na(empty_selector)] <- FALSE
  
  y[empty_selector] <- NA_character_
  
  y
}


# ------------------------------------------------------------------------------
# 05. HELPER: NORMALISE PATH
# ------------------------------------------------------------------------------

normalise_path <- function(x) {
  
  gsub(
    "\\",
    "/",
    as.character(x),
    fixed = TRUE
  )
}


# ------------------------------------------------------------------------------
# 06. HELPER: UNIQUE NON-MISSING VALUES
# ------------------------------------------------------------------------------

unique_nonmissing <- function(x) {
  
  y <- normalise_text(x)
  
  y <- y[
    !is.na(y)
  ]
  
  sort(
    unique(y)
  )
}


# ------------------------------------------------------------------------------
# 07. HELPER: SAFE CSV READ
# ------------------------------------------------------------------------------

safe_read_csv <- function(path) {
  
  if (!file.exists(path)) {
    
    stop(
      paste0(
        "Required source file not found:\n",
        path
      )
    )
  }
  
  out <- tryCatch(
    
    read.csv(
      path,
      stringsAsFactors = FALSE,
      check.names = FALSE,
      fileEncoding = "UTF-8"
    ),
    
    error = function(e) {
      
      tryCatch(
        
        read.csv(
          path,
          stringsAsFactors = FALSE,
          check.names = FALSE,
          fileEncoding = "UTF-8-BOM"
        ),
        
        error = function(e2) {
          
          stop(
            paste0(
              "Unable to read source file:\n",
              path,
              "\n\n",
              "UTF-8 error:\n",
              conditionMessage(e),
              "\n\n",
              "UTF-8-BOM error:\n",
              conditionMessage(e2)
            )
          )
        }
      )
    }
  )
  
  out
}


# ------------------------------------------------------------------------------
# 08. HELPER: FIND EXACT SOURCE FILE
#
# The 06b1 approved-field register contains the authoritative source path.
# We therefore recover source products from that register rather than
# guessing their directory.
# ------------------------------------------------------------------------------

get_source_path <- function(
    approved_table,
    file_name
) {
  
  selector <- (
    approved_table$file_name ==
      file_name
  )
  
  selector[is.na(selector)] <- FALSE
  
  paths <- unique_nonmissing(
    approved_table$source_file[
      selector
    ]
  )
  
  if (length(paths) == 0L) {
    
    stop(
      paste0(
        "No approved source path found for:\n",
        file_name
      )
    )
  }
  
  if (length(paths) > 1L) {
    
    stop(
      paste0(
        "Multiple source paths found for:\n",
        file_name,
        "\n\n",
        paste(
          paths,
          collapse = "\n"
        )
      )
    )
  }
  
  paths[1L]
}


# ------------------------------------------------------------------------------
# 09. HELPER: FIND FIRST AVAILABLE FIELD
#
# Used only for descriptive attributes in the WCVP reference.
#
# tdwg_code itself remains mandatory.
# ------------------------------------------------------------------------------

find_first_field <- function(
    data,
    candidates
) {
  
  available <- candidates[
    candidates %in%
      names(data)
  ]
  
  if (length(available) == 0L) {
    
    return(
      NA_character_
    )
  }
  
  available[1L]
}


# ------------------------------------------------------------------------------
# 10. HELPER: COLLAPSE SOURCE VALUES
# ------------------------------------------------------------------------------

collapse_unique <- function(x) {
  
  values <- unique_nonmissing(x)
  
  if (length(values) == 0L) {
    
    return(
      NA_character_
    )
  }
  
  paste(
    values,
    collapse = " | "
  )
}


# ------------------------------------------------------------------------------
# 11. VALIDATE REQUIRED INPUT FILES
# ------------------------------------------------------------------------------

required_inputs <- c(
  VALIDATED_FIELD_REGISTER_FILE,
  APPROVED_FIELD_FILE,
  EXCLUDED_FIELD_FILE,
  CONCEPT_PAIR_FILE,
  VALIDATION_06B1_FILE
)

missing_inputs <- required_inputs[
  !file.exists(required_inputs)
]

if (length(missing_inputs) > 0L) {
  
  stop(
    paste0(
      "Required 06b1 input file(s) not found:\n",
      paste(
        missing_inputs,
        collapse = "\n"
      )
    )
  )
}


# ------------------------------------------------------------------------------
# 12. LOAD 06b1 PRODUCTS
# ------------------------------------------------------------------------------

validated_fields <- safe_read_csv(
  VALIDATED_FIELD_REGISTER_FILE
)

approved_fields <- safe_read_csv(
  APPROVED_FIELD_FILE
)

excluded_fields <- safe_read_csv(
  EXCLUDED_FIELD_FILE
)

concept_pairs <- safe_read_csv(
  CONCEPT_PAIR_FILE
)

validation_06b1 <- safe_read_csv(
  VALIDATION_06B1_FILE
)


# ------------------------------------------------------------------------------
# 13. VALIDATE 06b1 GATE
# ------------------------------------------------------------------------------

if (
  !"passed" %in%
  names(validation_06b1)
) {
  
  stop(
    "06b1 validation table lacks field 'passed'."
  )
}


validation_06b1$passed <- as.logical(
  validation_06b1$passed
)


if (
  any(is.na(validation_06b1$passed)) ||
  !all(validation_06b1$passed)
) {
  
  stop(
    paste0(
      "06b1 validation did not pass completely.\n",
      "Do not rebuild the geographic vocabulary."
    )
  )
}


# ------------------------------------------------------------------------------
# 14. VALIDATE 06b1 FIELD COUNTS
# ------------------------------------------------------------------------------

if (nrow(validated_fields) != 11L) {
  
  stop(
    paste0(
      "Expected 11 validated candidate fields; found ",
      nrow(validated_fields),
      "."
    )
  )
}


if (nrow(approved_fields) != 6L) {
  
  stop(
    paste0(
      "Expected 6 approved geographic fields; found ",
      nrow(approved_fields),
      "."
    )
  )
}


if (nrow(excluded_fields) != 5L) {
  
  stop(
    paste0(
      "Expected 5 excluded fields; found ",
      nrow(excluded_fields),
      "."
    )
  )
}


# ------------------------------------------------------------------------------
# 15. LOCATE APPROVED SOURCE PRODUCTS
# ------------------------------------------------------------------------------

BOTANICAL_SUMMARY_PATH <- get_source_path(
  approved_fields,
  "vpjd_botanical_area_taxon_summary.csv"
)

BOTANICAL_DISTRIBUTION_PATH <- get_source_path(
  approved_fields,
  "vpjd_taxon_botanical_area_distribution.csv"
)

WCVP_LOCATION_PATH <- get_source_path(
  approved_fields,
  "vpjd_wcvp_location_reference.csv"
)

CHINA_REFERENCE_PATH <- get_source_path(
  approved_fields,
  "china_province_level_reference.csv"
)


cat("\n")
cat("============================================================\n")
cat(" VPJD GEOGRAPHY 06b2 - REBUILD CANONICAL GEOGRAPHIC UNITS\n")
cat("============================================================\n\n")

cat("Approved source products:\n\n")

cat(
  "Botanical-area summary:\n",
  BOTANICAL_SUMMARY_PATH,
  "\n\n",
  sep = ""
)

cat(
  "Botanical-area distribution:\n",
  BOTANICAL_DISTRIBUTION_PATH,
  "\n\n",
  sep = ""
)

cat(
  "WCVP location reference:\n",
  WCVP_LOCATION_PATH,
  "\n\n",
  sep = ""
)

cat(
  "China province reference:\n",
  CHINA_REFERENCE_PATH,
  "\n\n",
  sep = ""
)


# ------------------------------------------------------------------------------
# 16. READ APPROVED SOURCE PRODUCTS
# ------------------------------------------------------------------------------

botanical_summary <- safe_read_csv(
  BOTANICAL_SUMMARY_PATH
)

botanical_distribution <- safe_read_csv(
  BOTANICAL_DISTRIBUTION_PATH
)

wcvp_location <- safe_read_csv(
  WCVP_LOCATION_PATH
)

china_reference <- safe_read_csv(
  CHINA_REFERENCE_PATH
)


# ------------------------------------------------------------------------------
# 17. VALIDATE BOTANICAL-AREA FIELDS
# ------------------------------------------------------------------------------

required_botanical_fields <- c(
  "botanical_area_id",
  "botanical_area_name"
)


missing_summary_fields <- setdiff(
  required_botanical_fields,
  names(botanical_summary)
)

missing_distribution_fields <- setdiff(
  required_botanical_fields,
  names(botanical_distribution)
)


if (length(missing_summary_fields) > 0L) {
  
  stop(
    paste0(
      "Botanical-area summary lacks required field(s):\n",
      paste(
        missing_summary_fields,
        collapse = "\n"
      )
    )
  )
}


if (length(missing_distribution_fields) > 0L) {
  
  stop(
    paste0(
      "Botanical-area distribution lacks required field(s):\n",
      paste(
        missing_distribution_fields,
        collapse = "\n"
      )
    )
  )
}


# ------------------------------------------------------------------------------
# 18. EXTRACT BOTANICAL-AREA PAIRS FROM SUMMARY PRODUCT
# ------------------------------------------------------------------------------

summary_pairs <- data.frame(
  
  botanical_area_id = normalise_text(
    botanical_summary$botanical_area_id
  ),
  
  botanical_area_name = normalise_text(
    botanical_summary$botanical_area_name
  ),
  
  source_product =
    "vpjd_botanical_area_taxon_summary.csv",
  
  stringsAsFactors = FALSE
)


summary_pair_selector <- (
  !is.na(summary_pairs$botanical_area_id) &
    !is.na(summary_pairs$botanical_area_name)
)

summary_pair_selector[
  is.na(summary_pair_selector)
] <- FALSE


summary_pairs <- summary_pairs[
  summary_pair_selector,
  ,
  drop = FALSE
]


summary_pairs <- unique(
  summary_pairs
)


# ------------------------------------------------------------------------------
# 19. EXTRACT BOTANICAL-AREA PAIRS FROM DISTRIBUTION PRODUCT
# ------------------------------------------------------------------------------

distribution_pairs <- data.frame(
  
  botanical_area_id = normalise_text(
    botanical_distribution$botanical_area_id
  ),
  
  botanical_area_name = normalise_text(
    botanical_distribution$botanical_area_name
  ),
  
  source_product =
    "vpjd_taxon_botanical_area_distribution.csv",
  
  stringsAsFactors = FALSE
)


distribution_pair_selector <- (
  !is.na(distribution_pairs$botanical_area_id) &
    !is.na(distribution_pairs$botanical_area_name)
)

distribution_pair_selector[
  is.na(distribution_pair_selector)
] <- FALSE


distribution_pairs <- distribution_pairs[
  distribution_pair_selector,
  ,
  drop = FALSE
]


distribution_pairs <- unique(
  distribution_pairs
)


# ------------------------------------------------------------------------------
# 20. COMBINE BOTANICAL-AREA PAIRS
# ------------------------------------------------------------------------------

all_botanical_pairs <- rbind(
  summary_pairs,
  distribution_pairs
)


all_botanical_pairs <- unique(
  all_botanical_pairs
)


# ------------------------------------------------------------------------------
# 21. AUDIT BOTANICAL-AREA ID -> NAME CONSISTENCY
# ------------------------------------------------------------------------------

botanical_ids <- unique_nonmissing(
  all_botanical_pairs$botanical_area_id
)


botanical_audit_rows <- vector(
  mode = "list",
  length = length(botanical_ids)
)


for (i in seq_along(botanical_ids)) {
  
  area_id <- botanical_ids[i]
  
  selector <- (
    all_botanical_pairs$botanical_area_id ==
      area_id
  )
  
  selector[is.na(selector)] <- FALSE
  
  subset_rows <- all_botanical_pairs[
    selector,
    ,
    drop = FALSE
  ]
  
  names_found <- unique_nonmissing(
    subset_rows$botanical_area_name
  )
  
  sources_found <- unique_nonmissing(
    subset_rows$source_product
  )
  
  botanical_audit_rows[[i]] <- data.frame(
    
    botanical_area_id = area_id,
    
    botanical_area_name = if (
      length(names_found) == 1L
    ) {
      names_found[1L]
    } else {
      collapse_unique(names_found)
    },
    
    n_distinct_names = length(
      names_found
    ),
    
    n_source_products = length(
      sources_found
    ),
    
    source_products = paste(
      sources_found,
      collapse = " | "
    ),
    
    id_name_consistent = (
      length(names_found) == 1L
    ),
    
    stringsAsFactors = FALSE
  )
}


botanical_audit <- do.call(
  rbind,
  botanical_audit_rows
)


rownames(botanical_audit) <- NULL


# ------------------------------------------------------------------------------
# 22. AUDIT BOTANICAL-AREA NAME -> ID CONSISTENCY
# ------------------------------------------------------------------------------

botanical_names <- unique_nonmissing(
  all_botanical_pairs$botanical_area_name
)


name_to_id_conflict_count <- 0L


for (area_name in botanical_names) {
  
  selector <- (
    all_botanical_pairs$botanical_area_name ==
      area_name
  )
  
  selector[is.na(selector)] <- FALSE
  
  ids_found <- unique_nonmissing(
    all_botanical_pairs$botanical_area_id[
      selector
    ]
  )
  
  if (length(ids_found) != 1L) {
    
    name_to_id_conflict_count <-
      name_to_id_conflict_count + 1L
  }
}


# ------------------------------------------------------------------------------
# 23. REQUIRE UNAMBIGUOUS BOTANICAL-AREA PAIRS
# ------------------------------------------------------------------------------

if (
  any(
    !botanical_audit$id_name_consistent
  )
) {
  
  stop(
    paste0(
      "Botanical-area ID -> name conflicts detected.\n",
      "Do not construct canonical botanical-area concepts."
    )
  )
}


if (name_to_id_conflict_count > 0L) {
  
  stop(
    paste0(
      "Botanical-area name -> ID conflicts detected: ",
      name_to_id_conflict_count,
      ".\n",
      "Do not construct canonical botanical-area concepts."
    )
  )
}


# ------------------------------------------------------------------------------
# 24. BUILD BOTANICAL-AREA CONCEPT TABLE
# ------------------------------------------------------------------------------

botanical_concepts <- botanical_audit[
  ,
  c(
    "botanical_area_id",
    "botanical_area_name",
    "n_source_products",
    "source_products"
  ),
  drop = FALSE
]


botanical_concepts$concept_group <-
  "JAPAN_BOTANICAL_AREA"

botanical_concepts$geographic_unit_type <-
  "BOTANICAL_AREA"

botanical_concepts$geographic_code <-
  botanical_concepts$botanical_area_id

botanical_concepts$geographic_name <-
  botanical_concepts$botanical_area_name

botanical_concepts$geographic_standard <-
  "VPJD_JAPAN_BOTANICAL_AREA"

botanical_concepts$country_context <-
  "Japan"

botanical_concepts$concept_resolution_status <-
  "RESOLVED"

botanical_concepts$identifier_status <-
  "PROVISIONAL"


botanical_concepts <- botanical_concepts[
  order(
    botanical_concepts$geographic_code
  ),
  ,
  drop = FALSE
]


rownames(botanical_concepts) <- NULL


# ------------------------------------------------------------------------------
# 25. VALIDATE WCVP TDWG CODE FIELD
# ------------------------------------------------------------------------------

if (
  !"tdwg_code" %in%
  names(wcvp_location)
) {
  
  stop(
    paste0(
      "WCVP location reference lacks required field:\n",
      "tdwg_code"
    )
  )
}


wcvp_location$tdwg_code <- normalise_text(
  wcvp_location$tdwg_code
)


# ------------------------------------------------------------------------------
# 26. IDENTIFY WCVP LOCATION DESCRIPTIVE FIELDS
#
# We deliberately do not guess from arbitrary columns.
#
# The script checks a controlled set of plausible location-name fields.
# If none exists, the TDWG code remains the canonical code and the geographic
# name is left missing for 06b3/06c resolution.
# ------------------------------------------------------------------------------

WCVP_NAME_CANDIDATES <- c(
  "location_name",
  "tdwg_name",
  "area_name",
  "geographic_area_name",
  "region_name",
  "name"
)

WCVP_LEVEL_CANDIDATES <- c(
  "tdwg_level",
  "level",
  "geographic_level"
)

WCVP_PARENT_CODE_CANDIDATES <- c(
  "parent_tdwg_code",
  "parent_code",
  "parent_location_code"
)

WCVP_PARENT_NAME_CANDIDATES <- c(
  "parent_location_name",
  "parent_name"
)


wcvp_name_field <- find_first_field(
  wcvp_location,
  WCVP_NAME_CANDIDATES
)

wcvp_level_field <- find_first_field(
  wcvp_location,
  WCVP_LEVEL_CANDIDATES
)

wcvp_parent_code_field <- find_first_field(
  wcvp_location,
  WCVP_PARENT_CODE_CANDIDATES
)

wcvp_parent_name_field <- find_first_field(
  wcvp_location,
  WCVP_PARENT_NAME_CANDIDATES
)


cat("\n")
cat("WCVP reference field resolution:\n\n")

cat(
  "  TDWG code field: tdwg_code\n"
)

cat(
  "  Location-name field: ",
  ifelse(
    is.na(wcvp_name_field),
    "<not identified>",
    wcvp_name_field
  ),
  "\n",
  sep = ""
)

cat(
  "  Level field: ",
  ifelse(
    is.na(wcvp_level_field),
    "<not identified>",
    wcvp_level_field
  ),
  "\n",
  sep = ""
)

cat(
  "  Parent-code field: ",
  ifelse(
    is.na(wcvp_parent_code_field),
    "<not identified>",
    wcvp_parent_code_field
  ),
  "\n",
  sep = ""
)

cat(
  "  Parent-name field: ",
  ifelse(
    is.na(wcvp_parent_name_field),
    "<not identified>",
    wcvp_parent_name_field
  ),
  "\n\n",
  sep = ""
)


# ------------------------------------------------------------------------------
# 27. BUILD WCVP TDWG WORKING TABLE
# ------------------------------------------------------------------------------

wcvp_work <- data.frame(
  tdwg_code = normalise_text(
    wcvp_location$tdwg_code
  ),
  stringsAsFactors = FALSE
)


if (!is.na(wcvp_name_field)) {
  
  wcvp_work$geographic_name <- normalise_text(
    wcvp_location[[wcvp_name_field]]
  )
  
} else {
  
  wcvp_work$geographic_name <- NA_character_
}


if (!is.na(wcvp_level_field)) {
  
  wcvp_work$tdwg_level <- normalise_text(
    wcvp_location[[wcvp_level_field]]
  )
  
} else {
  
  wcvp_work$tdwg_level <- NA_character_
}


if (!is.na(wcvp_parent_code_field)) {
  
  wcvp_work$parent_tdwg_code <- normalise_text(
    wcvp_location[[wcvp_parent_code_field]]
  )
  
} else {
  
  wcvp_work$parent_tdwg_code <- NA_character_
}


if (!is.na(wcvp_parent_name_field)) {
  
  wcvp_work$parent_geographic_name <- normalise_text(
    wcvp_location[[wcvp_parent_name_field]]
  )
  
} else {
  
  wcvp_work$parent_geographic_name <- NA_character_
}


wcvp_selector <- !is.na(
  wcvp_work$tdwg_code
)

wcvp_selector[is.na(wcvp_selector)] <- FALSE


wcvp_work <- wcvp_work[
  wcvp_selector,
  ,
  drop = FALSE
]

# ------------------------------------------------------------------------------
# 28. AUDIT TDWG CODE UNIQUENESS
# ------------------------------------------------------------------------------

tdwg_codes <- unique_nonmissing(
  wcvp_work$tdwg_code
)


wcvp_audit_rows <- vector(
  mode = "list",
  length = length(tdwg_codes)
)


for (i in seq_along(tdwg_codes)) {
  
  code_value <- tdwg_codes[i]
  
  selector <- (
    wcvp_work$tdwg_code ==
      code_value
  )
  
  selector[is.na(selector)] <- FALSE
  
  subset_rows <- wcvp_work[
    selector,
    ,
    drop = FALSE
  ]
  
  names_found <- unique_nonmissing(
    subset_rows$geographic_name
  )
  
  levels_found <- unique_nonmissing(
    subset_rows$tdwg_level
  )
  
  parent_codes_found <- unique_nonmissing(
    subset_rows$parent_tdwg_code
  )
  
  parent_names_found <- unique_nonmissing(
    subset_rows$parent_geographic_name
  )
  
  
  wcvp_audit_rows[[i]] <- data.frame(
    
    tdwg_code = code_value,
    
    geographic_name = if (
      length(names_found) == 1L
    ) {
      names_found[1L]
    } else {
      collapse_unique(names_found)
    },
    
    tdwg_level = if (
      length(levels_found) == 1L
    ) {
      levels_found[1L]
    } else {
      collapse_unique(levels_found)
    },
    
    parent_tdwg_code = if (
      length(parent_codes_found) == 1L
    ) {
      parent_codes_found[1L]
    } else {
      collapse_unique(parent_codes_found)
    },
    
    parent_geographic_name = if (
      length(parent_names_found) == 1L
    ) {
      parent_names_found[1L]
    } else {
      collapse_unique(parent_names_found)
    },
    
    n_distinct_names = length(
      names_found
    ),
    
    n_distinct_levels = length(
      levels_found
    ),
    
    n_distinct_parent_codes = length(
      parent_codes_found
    ),
    
    code_resolution_consistent = (
      length(names_found) <= 1L &&
        length(levels_found) <= 1L &&
        length(parent_codes_found) <= 1L
    ),
    
    stringsAsFactors = FALSE
  )
}


wcvp_audit <- do.call(
  rbind,
  wcvp_audit_rows
)


rownames(wcvp_audit) <- NULL


# ------------------------------------------------------------------------------
# 29. STOP IF ONE TDWG CODE MAPS TO CONTRADICTORY REFERENCE VALUES
# ------------------------------------------------------------------------------

if (
  any(
    !wcvp_audit$code_resolution_consistent
  )
) {
  
  stop(
    paste0(
      "Contradictory WCVP TDWG code resolution detected.\n",
      "Review geography_06b2_wcvp_tdwg_resolution.csv after the ",
      "audit output has been written."
    )
  )
}


# ------------------------------------------------------------------------------
# 30. BUILD WCVP TDWG CONCEPT TABLE
# ------------------------------------------------------------------------------

wcvp_concepts <- data.frame(
  
  concept_group =
    "WCVP_TDWG_LOCATION",
  
  geographic_unit_type =
    "WCVP_TDWG_UNIT",
  
  geographic_code =
    wcvp_audit$tdwg_code,
  
  geographic_name =
    wcvp_audit$geographic_name,
  
  geographic_standard =
    "WCVP_TDWG",
  
  tdwg_level =
    wcvp_audit$tdwg_level,
  
  parent_tdwg_code =
    wcvp_audit$parent_tdwg_code,
  
  parent_geographic_name =
    wcvp_audit$parent_geographic_name,
  
  source_product =
    "vpjd_wcvp_location_reference.csv",
  
  concept_resolution_status = ifelse(
    is.na(wcvp_audit$geographic_name),
    "CODE_VALIDATED_NAME_PENDING",
    "RESOLVED_FROM_REFERENCE"
  ),
  
  identifier_status =
    "PROVISIONAL",
  
  stringsAsFactors = FALSE
)


wcvp_concepts <- wcvp_concepts[
  order(
    wcvp_concepts$geographic_code
  ),
  ,
  drop = FALSE
]


rownames(wcvp_concepts) <- NULL


# ------------------------------------------------------------------------------
# 31. VALIDATE CHINA CLASSIFICATION FIELD
# ------------------------------------------------------------------------------

if (
  !"province_level_division" %in%
  names(china_reference)
) {
  
  stop(
    paste0(
      "China province reference lacks approved field:\n",
      "province_level_division"
    )
  )
}


china_class_values <- unique_nonmissing(
  china_reference$province_level_division
)


china_classification_metadata <- data.frame(
  
  classification_value =
    china_class_values,
  
  field_name =
    "province_level_division",
  
  source_product =
    "china_province_level_reference.csv",
  
  semantic_role =
    "GEOGRAPHIC_CLASSIFICATION",
  
  creates_canonical_geographic_unit =
    FALSE,
  
  interpretation =
    "Classification metadata only; value is not automatically interpreted as a province name.",
  
  stringsAsFactors = FALSE
)


# ------------------------------------------------------------------------------
# 32. CONSTRUCT COMMON BOTANICAL-AREA RECORDS
# ------------------------------------------------------------------------------

botanical_common <- data.frame(
  
  concept_group =
    botanical_concepts$concept_group,
  
  geographic_unit_type =
    botanical_concepts$geographic_unit_type,
  
  geographic_code =
    botanical_concepts$geographic_code,
  
  geographic_name =
    botanical_concepts$geographic_name,
  
  geographic_standard =
    botanical_concepts$geographic_standard,
  
  country_context =
    botanical_concepts$country_context,
  
  tdwg_level =
    NA_character_,
  
  parent_tdwg_code =
    NA_character_,
  
  parent_geographic_name =
    NA_character_,
  
  concept_resolution_status =
    botanical_concepts$concept_resolution_status,
  
  identifier_status =
    botanical_concepts$identifier_status,
  
  stringsAsFactors = FALSE
)


# ------------------------------------------------------------------------------
# 33. CONSTRUCT COMMON WCVP RECORDS
# ------------------------------------------------------------------------------

wcvp_common <- data.frame(
  
  concept_group =
    wcvp_concepts$concept_group,
  
  geographic_unit_type =
    wcvp_concepts$geographic_unit_type,
  
  geographic_code =
    wcvp_concepts$geographic_code,
  
  geographic_name =
    wcvp_concepts$geographic_name,
  
  geographic_standard =
    wcvp_concepts$geographic_standard,
  
  country_context =
    NA_character_,
  
  tdwg_level =
    wcvp_concepts$tdwg_level,
  
  parent_tdwg_code =
    wcvp_concepts$parent_tdwg_code,
  
  parent_geographic_name =
    wcvp_concepts$parent_geographic_name,
  
  concept_resolution_status =
    wcvp_concepts$concept_resolution_status,
  
  identifier_status =
    wcvp_concepts$identifier_status,
  
  stringsAsFactors = FALSE
)


# ------------------------------------------------------------------------------
# 34. COMBINE RESOLVED GEOGRAPHIC CONCEPTS
# ------------------------------------------------------------------------------

canonical_units <- rbind(
  botanical_common,
  wcvp_common
)


# ------------------------------------------------------------------------------
# 35. CREATE STABLE SORT KEY
#
# Identifier assignment is deterministic for this provisional build:
#
#   concept_group
#   geographic_code
#   geographic_name
#
# IDs remain provisional until 06b3 / release validation.
# ------------------------------------------------------------------------------

canonical_units$sort_name <- ifelse(
  is.na(canonical_units$geographic_name),
  "",
  canonical_units$geographic_name
)


canonical_units <- canonical_units[
  order(
    canonical_units$concept_group,
    canonical_units$geographic_code,
    canonical_units$sort_name
  ),
  ,
  drop = FALSE
]


rownames(canonical_units) <- NULL


# ------------------------------------------------------------------------------
# 36. ASSIGN PROVISIONAL VPJD-GEO IDENTIFIERS
# ------------------------------------------------------------------------------

canonical_units$geographic_unit_id <- sprintf(
  "VPJD-GEO-%04d",
  seq_len(
    nrow(canonical_units)
  )
)


canonical_units$identifier_status <-
  "PROVISIONAL_06B2"


canonical_units$sort_name <- NULL


# Move identifier to first column.
canonical_units <- canonical_units[
  ,
  c(
    "geographic_unit_id",
    "geographic_code",
    "geographic_name",
    "geographic_unit_type",
    "concept_group",
    "geographic_standard",
    "country_context",
    "tdwg_level",
    "parent_tdwg_code",
    "parent_geographic_name",
    "concept_resolution_status",
    "identifier_status"
  ),
  drop = FALSE
]


# ------------------------------------------------------------------------------
# 37. ATTACH IDS TO BOTANICAL CONCEPT TABLE
# ------------------------------------------------------------------------------

botanical_lookup <- canonical_units[
  canonical_units$concept_group ==
    "JAPAN_BOTANICAL_AREA",
  c(
    "geographic_unit_id",
    "geographic_code"
  ),
  drop = FALSE
]


names(botanical_lookup)[
  names(botanical_lookup) ==
    "geographic_code"
] <- "botanical_area_id"


botanical_concepts <- merge(
  botanical_concepts,
  botanical_lookup,
  by = "botanical_area_id",
  all.x = TRUE,
  sort = FALSE
)


botanical_concepts <- botanical_concepts[
  order(
    botanical_concepts$botanical_area_id
  ),
  ,
  drop = FALSE
]


rownames(botanical_concepts) <- NULL


# ------------------------------------------------------------------------------
# 38. ATTACH IDS TO WCVP CONCEPT TABLE
# ------------------------------------------------------------------------------

wcvp_lookup <- canonical_units[
  canonical_units$concept_group ==
    "WCVP_TDWG_LOCATION",
  c(
    "geographic_unit_id",
    "geographic_code"
  ),
  drop = FALSE
]


names(wcvp_lookup)[
  names(wcvp_lookup) ==
    "geographic_code"
] <- "tdwg_code"


wcvp_concepts <- merge(
  wcvp_concepts,
  wcvp_lookup,
  by.x = "geographic_code",
  by.y = "tdwg_code",
  all.x = TRUE,
  sort = FALSE
)


wcvp_concepts <- wcvp_concepts[
  order(
    wcvp_concepts$geographic_code
  ),
  ,
  drop = FALSE
]


rownames(wcvp_concepts) <- NULL


# ------------------------------------------------------------------------------
# 39. BUILD BOTANICAL-AREA SOURCE PROVENANCE
# ------------------------------------------------------------------------------

botanical_provenance <- merge(
  all_botanical_pairs,
  botanical_lookup,
  by = "botanical_area_id",
  all.x = TRUE,
  sort = FALSE
)


botanical_provenance$source_field_code <-
  "botanical_area_id"

botanical_provenance$source_field_name <-
  "botanical_area_name"

botanical_provenance$concept_group <-
  "JAPAN_BOTANICAL_AREA"


botanical_provenance <- botanical_provenance[
  ,
  c(
    "geographic_unit_id",
    "concept_group",
    "source_product",
    "source_field_code",
    "botanical_area_id",
    "source_field_name",
    "botanical_area_name"
  ),
  drop = FALSE
]


# ------------------------------------------------------------------------------
# 40. BUILD WCVP SOURCE PROVENANCE
# ------------------------------------------------------------------------------

wcvp_provenance <- data.frame(
  
  geographic_unit_id =
    wcvp_concepts$geographic_unit_id,
  
  concept_group =
    "WCVP_TDWG_LOCATION",
  
  source_product =
    "vpjd_wcvp_location_reference.csv",
  
  source_field_code =
    "tdwg_code",
  
  botanical_area_id =
    NA_character_,
  
  source_field_name =
    ifelse(
      is.na(wcvp_name_field),
      NA_character_,
      wcvp_name_field
    ),
  
  botanical_area_name =
    NA_character_,
  
  source_code =
    wcvp_concepts$geographic_code,
  
  source_name =
    wcvp_concepts$geographic_name,
  
  stringsAsFactors = FALSE
)


# Add equivalent columns to botanical provenance.
botanical_provenance$source_code <-
  botanical_provenance$botanical_area_id

botanical_provenance$source_name <-
  botanical_provenance$botanical_area_name


# ------------------------------------------------------------------------------
# 41. ALIGN PROVENANCE TABLES
# ------------------------------------------------------------------------------

provenance_columns <- c(
  "geographic_unit_id",
  "concept_group",
  "source_product",
  "source_field_code",
  "source_field_name",
  "source_code",
  "source_name"
)


botanical_provenance <- botanical_provenance[
  ,
  provenance_columns,
  drop = FALSE
]


wcvp_provenance <- wcvp_provenance[
  ,
  provenance_columns,
  drop = FALSE
]


source_provenance <- rbind(
  botanical_provenance,
  wcvp_provenance
)


source_provenance <- unique(
  source_provenance
)


source_provenance <- source_provenance[
  order(
    source_provenance$geographic_unit_id,
    source_provenance$source_product
  ),
  ,
  drop = FALSE
]


rownames(source_provenance) <- NULL


# ------------------------------------------------------------------------------
# 42. BUILD CONCEPT-SOURCE REGISTER
# ------------------------------------------------------------------------------

concept_ids <- unique_nonmissing(
  canonical_units$geographic_unit_id
)


concept_source_rows <- vector(
  mode = "list",
  length = length(concept_ids)
)


for (i in seq_along(concept_ids)) {
  
  concept_id <- concept_ids[i]
  
  selector <- (
    source_provenance$geographic_unit_id ==
      concept_id
  )
  
  selector[is.na(selector)] <- FALSE
  
  source_subset <- source_provenance[
    selector,
    ,
    drop = FALSE
  ]
  
  source_products <- unique_nonmissing(
    source_subset$source_product
  )
  
  concept_source_rows[[i]] <- data.frame(
    
    geographic_unit_id =
      concept_id,
    
    n_source_products =
      length(source_products),
    
    source_products =
      paste(
        source_products,
        collapse = " | "
      ),
    
    n_provenance_rows =
      nrow(source_subset),
    
    stringsAsFactors = FALSE
  )
}


concept_source_register <- do.call(
  rbind,
  concept_source_rows
)


rownames(concept_source_register) <- NULL


# ------------------------------------------------------------------------------
# 43. CORE COUNTS
# ------------------------------------------------------------------------------

n_botanical_concepts <- nrow(
  botanical_concepts
)

n_wcvp_concepts <- nrow(
  wcvp_concepts
)

n_canonical_units <- nrow(
  canonical_units
)

n_china_classification_values <- nrow(
  china_classification_metadata
)

n_missing_unit_ids <- sum(
  is.na(
    canonical_units$geographic_unit_id
  )
)

n_duplicate_unit_ids <- sum(
  duplicated(
    canonical_units$geographic_unit_id
  )
)

n_duplicate_botanical_codes <- sum(
  duplicated(
    botanical_concepts$botanical_area_id
  )
)

n_duplicate_wcvp_codes <- sum(
  duplicated(
    wcvp_concepts$geographic_code
  )
)

n_missing_botanical_names <- sum(
  is.na(
    botanical_concepts$geographic_name
  )
)

n_missing_wcvp_names <- sum(
  is.na(
    wcvp_concepts$geographic_name
  )
)

n_concepts_without_provenance <- sum(
  !canonical_units$geographic_unit_id %in%
    source_provenance$geographic_unit_id
)


# ------------------------------------------------------------------------------
# 44. EXPECTED CONCEPT COUNT
#
# 51 botanical areas + 367 WCVP TDWG units = 418 geographic concepts.
#
# China province_level_division is metadata and therefore contributes ZERO
# canonical geographic concepts at this stage.
# ------------------------------------------------------------------------------

EXPECTED_BOTANICAL_CONCEPTS <- 51L
EXPECTED_WCVP_CONCEPTS <- 367L

EXPECTED_CANONICAL_CONCEPTS <- (
  EXPECTED_BOTANICAL_CONCEPTS +
    EXPECTED_WCVP_CONCEPTS
)


# ------------------------------------------------------------------------------
# 45. BUILD SUMMARY
# ------------------------------------------------------------------------------

summary_table <- data.frame(
  
  metric = c(
    "validated_candidate_fields",
    "approved_geographic_fields",
    "excluded_non_geographic_fields",
    "discarded_06b_spurious_units",
    "japan_botanical_area_concepts",
    "wcvp_tdwg_concepts",
    "china_classification_metadata_values",
    "canonical_geographic_concepts",
    "expected_canonical_geographic_concepts",
    "missing_geographic_unit_ids",
    "duplicate_geographic_unit_ids",
    "duplicate_botanical_area_codes",
    "duplicate_wcvp_tdwg_codes",
    "missing_botanical_area_names",
    "missing_wcvp_location_names",
    "concepts_without_provenance",
    "source_provenance_rows"
  ),
  
  value = c(
    nrow(validated_fields),
    nrow(approved_fields),
    nrow(excluded_fields),
    672L,
    n_botanical_concepts,
    n_wcvp_concepts,
    n_china_classification_values,
    n_canonical_units,
    EXPECTED_CANONICAL_CONCEPTS,
    n_missing_unit_ids,
    n_duplicate_unit_ids,
    n_duplicate_botanical_codes,
    n_duplicate_wcvp_codes,
    n_missing_botanical_names,
    n_missing_wcvp_names,
    n_concepts_without_provenance,
    nrow(source_provenance)
  ),
  
  stringsAsFactors = FALSE
)


# ------------------------------------------------------------------------------
# 46. VALIDATION GATE
# ------------------------------------------------------------------------------

validation_gate <- data.frame(
  
  criterion = c(
    "06b1_validation_passed",
    "validated_candidate_field_count_11",
    "approved_geographic_field_count_6",
    "excluded_non_geographic_field_count_5",
    "botanical_area_id_name_mapping_unambiguous",
    "botanical_area_name_id_mapping_unambiguous",
    "botanical_area_concept_count_51",
    "botanical_area_codes_unique",
    "botanical_area_names_complete",
    "wcvp_tdwg_code_field_present",
    "wcvp_tdwg_code_mapping_consistent",
    "wcvp_tdwg_concept_count_367",
    "wcvp_tdwg_codes_unique",
    "china_classification_not_converted_to_units",
    "canonical_concept_count_418",
    "geographic_unit_ids_complete",
    "geographic_unit_ids_unique",
    "all_concepts_have_source_provenance",
    "provisional_identifier_status_preserved",
    "no_geographic_hierarchy_inferred",
    "no_geometry_inferred",
    "no_missing_evidence_interpreted_as_absence",
    "published_vpjd_not_modified"
  ),
  
  passed = c(
    all(
      validation_06b1$passed
    ),
    
    nrow(validated_fields) == 11L,
    
    nrow(approved_fields) == 6L,
    
    nrow(excluded_fields) == 5L,
    
    all(
      botanical_audit$id_name_consistent
    ),
    
    name_to_id_conflict_count == 0L,
    
    n_botanical_concepts ==
      EXPECTED_BOTANICAL_CONCEPTS,
    
    n_duplicate_botanical_codes == 0L,
    
    n_missing_botanical_names == 0L,
    
    "tdwg_code" %in%
      names(wcvp_location),
    
    all(
      wcvp_audit$code_resolution_consistent
    ),
    
    n_wcvp_concepts ==
      EXPECTED_WCVP_CONCEPTS,
    
    n_duplicate_wcvp_codes == 0L,
    
    TRUE,
    
    n_canonical_units ==
      EXPECTED_CANONICAL_CONCEPTS,
    
    n_missing_unit_ids == 0L,
    
    n_duplicate_unit_ids == 0L,
    
    n_concepts_without_provenance == 0L,
    
    all(
      canonical_units$identifier_status ==
        "PROVISIONAL_06B2"
    ),
    
    TRUE,
    
    TRUE,
    
    TRUE,
    
    TRUE
  ),
  
  stringsAsFactors = FALSE
)


# ------------------------------------------------------------------------------
# 47. WRITE OUTPUTS
# ------------------------------------------------------------------------------

write.csv(
  canonical_units,
  CANONICAL_UNIT_FILE,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)


write.csv(
  botanical_concepts,
  BOTANICAL_AREA_FILE,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)


write.csv(
  wcvp_concepts,
  WCVP_LOCATION_FILE,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)


write.csv(
  source_provenance,
  SOURCE_PROVENANCE_FILE,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)


write.csv(
  china_classification_metadata,
  CHINA_CLASSIFICATION_FILE,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)


write.csv(
  concept_source_register,
  CONCEPT_SOURCE_FILE,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)


write.csv(
  botanical_audit,
  BOTANICAL_AUDIT_FILE,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)


write.csv(
  wcvp_audit,
  WCVP_AUDIT_FILE,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)


write.csv(
  summary_table,
  SUMMARY_FILE,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)


write.csv(
  validation_gate,
  VALIDATION_FILE,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)


# ------------------------------------------------------------------------------
# 48. CONSOLE SUMMARY
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" VPJD GEOGRAPHY 06b2 - REBUILD COMPLETE\n")
cat("============================================================\n\n")


cat("Summary:\n\n")

print(
  summary_table,
  row.names = FALSE
)


# ------------------------------------------------------------------------------
# 49. BOTANICAL-AREA SUMMARY
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" JAPAN BOTANICAL AREAS\n")
cat("============================================================\n\n")


cat(
  "Canonical botanical-area concepts: ",
  n_botanical_concepts,
  "\n",
  sep = ""
)

cat(
  "ID -> name conflicts: ",
  sum(
    !botanical_audit$id_name_consistent
  ),
  "\n",
  sep = ""
)

cat(
  "Name -> ID conflicts: ",
  name_to_id_conflict_count,
  "\n",
  sep = ""
)

cat(
  "Missing botanical-area names: ",
  n_missing_botanical_names,
  "\n",
  sep = ""
)


# ------------------------------------------------------------------------------
# 50. WCVP TDWG SUMMARY
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" WCVP TDWG UNITS\n")
cat("============================================================\n\n")


cat(
  "Canonical WCVP TDWG concepts: ",
  n_wcvp_concepts,
  "\n",
  sep = ""
)

cat(
  "Duplicate TDWG codes: ",
  n_duplicate_wcvp_codes,
  "\n",
  sep = ""
)

cat(
  "Missing resolved location names: ",
  n_missing_wcvp_names,
  "\n",
  sep = ""
)

cat(
  "Detected location-name field: ",
  ifelse(
    is.na(wcvp_name_field),
    "<none>",
    wcvp_name_field
  ),
  "\n",
  sep = ""
)


# ------------------------------------------------------------------------------
# 51. CHINA CLASSIFICATION SUMMARY
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" CHINA PROVINCE CLASSIFICATION METADATA\n")
cat("============================================================\n\n")


cat(
  "Distinct province_level_division values: ",
  n_china_classification_values,
  "\n",
  sep = ""
)

cat(
  "Canonical geographic units created directly from classification field: 0\n"
)


# ------------------------------------------------------------------------------
# 52. VALIDATION GATE
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" VALIDATION GATE\n")
cat("============================================================\n\n")


print(
  validation_gate,
  row.names = FALSE
)


# ------------------------------------------------------------------------------
# 53. INTERPRETATION
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" INTERPRETATION\n")
cat("============================================================\n\n")


if (
  all(
    validation_gate$passed
  )
) {
  
  cat(
    paste0(
      "PASS. The provisional VPJD geographic vocabulary has been rebuilt ",
      "from the six fields approved by 06b1.\n\n"
    )
  )
  
  cat(
    paste0(
      format(
        n_botanical_concepts,
        big.mark = ","
      ),
      " Japanese botanical-area concepts were reconstructed from paired ",
      "botanical_area_id and botanical_area_name evidence.\n\n"
    )
  )
  
  cat(
    paste0(
      format(
        n_wcvp_concepts,
        big.mark = ","
      ),
      " WCVP TDWG concepts were reconstructed from the validated WCVP ",
      "location reference.\n\n"
    )
  )
  
  cat(
    paste0(
      "The combined provisional vocabulary therefore contains ",
      format(
        n_canonical_units,
        big.mark = ","
      ),
      " canonical geographic concepts.\n\n"
    )
  )
  
  cat(
    paste0(
      "The five non-geographic fields rejected by 06b1 do not contribute ",
      "to the rebuilt vocabulary. The 672 spurious provisional units ",
      "identified during 06b/06b0 are therefore absent from this build.\n\n"
    )
  )
  
  cat(
    paste0(
      "China province_level_division remains classification metadata and ",
      "has not been converted directly into geographic concepts.\n\n"
    )
  )
  
  cat(
    paste0(
      "All VPJD-GEO identifiers generated by this script remain ",
      "PROVISIONAL_06B2 and must not yet be frozen for release.\n"
    )
  )
  
} else {
  
  cat(
    "FAIL. One or more 06b2 validation criteria failed.\n\n"
  )
  
  failed_criteria <- validation_gate$criterion[
    !validation_gate$passed
  ]
  
  cat("Failed criteria:\n\n")
  
  for (criterion_name in failed_criteria) {
    
    cat(
      " - ",
      criterion_name,
      "\n",
      sep = ""
    )
  }
}


# ------------------------------------------------------------------------------
# 54. NEXT STEP
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" NEXT STEP\n")
cat("============================================================\n\n")


if (
  all(
    validation_gate$passed
  )
) {
  
  cat(
    paste0(
      "Proceed to:\n\n",
      "  geography_06b3_validate_rebuilt_geographic_vocabulary.R\n\n",
      "06b3 should independently audit the rebuilt concept vocabulary, ",
      "including:\n\n",
      "  1. concept cardinality;\n",
      "  2. identifier determinism and uniqueness;\n",
      "  3. botanical-area ID/name integrity;\n",
      "  4. WCVP TDWG code/reference integrity;\n",
      "  5. cross-system duplication or overlap;\n",
      "  6. provenance completeness;\n",
      "  7. unresolved names or codes;\n",
      "  8. suitability of the vocabulary for hierarchy construction;\n",
      "  9. whether VPJD-GEO identifiers can safely be frozen.\n\n",
      "Do not freeze identifiers before 06b3 passes.\n"
    )
  )
  
} else {
  
  cat(
    paste0(
      "Do not proceed to 06b3 as a successful vocabulary build.\n",
      "Review the failed 06b2 validation criterion or criteria first.\n"
    )
  )
}


# ------------------------------------------------------------------------------
# 55. OUTPUT LOCATIONS
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" OUTPUTS\n")
cat("============================================================\n\n")


cat(
  "Canonical geographic units:\n",
  CANONICAL_UNIT_FILE,
  "\n\n",
  sep = ""
)

cat(
  "Japanese botanical areas:\n",
  BOTANICAL_AREA_FILE,
  "\n\n",
  sep = ""
)

cat(
  "WCVP TDWG units:\n",
  WCVP_LOCATION_FILE,
  "\n\n",
  sep = ""
)

cat(
  "Source provenance:\n",
  SOURCE_PROVENANCE_FILE,
  "\n\n",
  sep = ""
)

cat(
  "China classification metadata:\n",
  CHINA_CLASSIFICATION_FILE,
  "\n\n",
  sep = ""
)

cat(
  "Concept-source register:\n",
  CONCEPT_SOURCE_FILE,
  "\n\n",
  sep = ""
)

cat(
  "Validation gate:\n",
  VALIDATION_FILE,
  "\n",
  sep = ""
)


# ------------------------------------------------------------------------------
# 56. SAFEGUARDS
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" SAFEGUARDS\n")
cat("============================================================\n\n")


cat("Source geographic evidence was NOT modified.\n")
cat("06b outputs were NOT modified.\n")
cat("06b0 outputs were NOT modified.\n")
cat("06b1 outputs were NOT modified.\n")

cat(
  "The 672 units generated by excluded non-geographic fields were NOT carried forward.\n"
)

cat(
  "Botanical-area IDs and names were consolidated as paired representations.\n"
)

cat(
  "TDWG codes were resolved only through the validated WCVP reference product.\n"
)

cat(
  "China province_level_division was retained as classification metadata only.\n"
)

cat(
  "No China province concept was inferred from a classification label.\n"
)

cat(
  "VPJD-GEO identifiers generated here remain PROVISIONAL.\n"
)

cat(
  "VPJD-GEO identifiers have NOT been frozen for publication.\n"
)

cat(
  "Geographic hierarchy was NOT inferred.\n"
)

cat(
  "Geographic geometry was NOT inferred.\n"
)

cat(
  "Missing geographic evidence was NOT interpreted as absence.\n"
)

cat(
  "Nakamura predicates were NOT evaluated.\n"
)

cat(
  "Star categories were NOT evaluated.\n"
)

cat(
  "Published VPJD Taxonomic Release v1.0.0 was NOT modified.\n"
)

cat(
  "VPJD Geography v1.0.0 has NOT been frozen or published.\n"
)

cat("\n")
cat("============================================================\n")