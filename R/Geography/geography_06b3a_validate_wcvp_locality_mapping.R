# ==============================================================================
# VPJD GEOGRAPHY MODULE
# geography_06b3a_validate_wcvp_locality_mapping.R
#
# PURPOSE
# -------
# Validate whether:
#
#   wcvp_locality
#
# in the validated WCVP location-reference product provides the authoritative
# human-readable geographic label associated with:
#
#   tdwg_code
#
# for the 367 WCVP/TDWG geographic concepts currently present in the
# provisional VPJD Geography v1.0.0 vocabulary.
#
#
# POSITION IN PIPELINE
# --------------------
#
#   06b2  Rebuild canonical geographic vocabulary
#     |
#     v
#   06b3  Validate rebuilt geographic vocabulary
#     |
#     |-- STRUCTURAL PASS
#     |
#     |-- 51 Japanese botanical areas resolved
#     |
#     `-- 367 WCVP/TDWG names pending
#               |
#               v
#   06b3a VALIDATE WCVP LOCALITY MAPPING             <-- THIS SCRIPT
#               |
#               v
#   06b3b Integrate validated WCVP locality names
#               |
#               v
#          Release-readiness audit
#
#
# 06b3 ESTABLISHED
# ----------------
#
#   Canonical geographic concepts:                 418
#   Japanese botanical-area concepts:               51
#   WCVP/TDWG concepts:                            367
#
#   Missing WCVP TDWG codes:                         0
#   Missing WCVP geographic names:                 367
#   Duplicate WCVP TDWG codes:                       0
#   Canonical codes absent from WCVP reference:      0
#   Reference codes absent from canonical table:     0
#
#   WCVP reference fields:
#
#       wcvp_location_id
#       wcvp_locality
#       tdwg_code
#       location_reference_source
#
#
# OBJECTIVES
# ----------
#
#  1. Read the authoritative WCVP location-reference product.
#
#  2. Confirm that all required fields are present:
#
#       wcvp_location_id
#       wcvp_locality
#       tdwg_code
#       location_reference_source
#
#  3. Validate completeness of:
#
#       wcvp_location_id
#       wcvp_locality
#       tdwg_code
#
#  4. Test whether:
#
#       tdwg_code -> wcvp_locality
#
#     is one-to-one / unambiguous.
#
#  5. Test the reverse relationship:
#
#       wcvp_locality -> tdwg_code
#
#     and report any duplicate locality labels associated with different
#     TDWG codes.
#
#  6. Test:
#
#       tdwg_code -> wcvp_location_id
#
#     for uniqueness.
#
#  7. Compare all 367 reference TDWG codes with the provisional canonical
#     WCVP/TDWG concepts produced by 06b2.
#
#  8. Validate the provenance field:
#
#       location_reference_source
#
#  9. Construct a validated code-to-locality crosswalk for use by 06b3b.
#
# 10. DO NOT modify the canonical geographic vocabulary.
#
#
# IMPORTANT
# ---------
#
# This script validates evidence only.
#
# It DOES NOT:
#
#   - update geographic_name;
#   - change concept_resolution_status;
#   - freeze VPJD-GEO identifiers;
#   - modify the 06b2 vocabulary;
#   - infer locality names;
#   - infer TDWG hierarchy;
#   - infer geometry;
#   - alter source evidence;
#   - modify VPJD Taxonomic Release v1.0.0;
#   - evaluate Nakamura predicates;
#   - assign Star categories.
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

CANONICAL_ROOT <- file.path(
  DERIVED_ROOT,
  "canonical_geographic_units"
)

FIELD_VALIDATION_ROOT <- file.path(
  DERIVED_ROOT,
  "field_validation"
)

AUDIT_06B3_ROOT <- file.path(
  PROJECT_ROOT,
  "audit",
  "geography",
  "release_v1.0.0",
  "06b3_validate_rebuilt_geographic_vocabulary"
)

OUTPUT_ROOT <- file.path(
  DERIVED_ROOT,
  "wcvp_locality_validation"
)

AUDIT_ROOT <- file.path(
  PROJECT_ROOT,
  "audit",
  "geography",
  "release_v1.0.0",
  "06b3a_validate_wcvp_locality_mapping"
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

CANONICAL_UNIT_FILE <- file.path(
  CANONICAL_ROOT,
  "vpjd_geographic_units_provisional.csv"
)

WCVP_UNIT_FILE <- file.path(
  CANONICAL_ROOT,
  "vpjd_wcvp_tdwg_units_provisional.csv"
)

APPROVED_FIELD_FILE <- file.path(
  FIELD_VALIDATION_ROOT,
  "vpjd_approved_geographic_fields.csv"
)

STRUCTURAL_GATE_06B3_FILE <- file.path(
  AUDIT_06B3_ROOT,
  "geography_06b3_structural_validation_gate.csv"
)


# ------------------------------------------------------------------------------
# 03. OUTPUT FILES
# ------------------------------------------------------------------------------

VALIDATED_CROSSWALK_FILE <- file.path(
  OUTPUT_ROOT,
  "vpjd_wcvp_tdwg_locality_crosswalk_validated.csv"
)

REFERENCE_PROFILE_FILE <- file.path(
  AUDIT_ROOT,
  "geography_06b3a_wcvp_reference_profile.csv"
)

CODE_MAPPING_AUDIT_FILE <- file.path(
  AUDIT_ROOT,
  "geography_06b3a_tdwg_code_locality_mapping.csv"
)

LOCALITY_MAPPING_AUDIT_FILE <- file.path(
  AUDIT_ROOT,
  "geography_06b3a_locality_tdwg_code_mapping.csv"
)

LOCATION_ID_AUDIT_FILE <- file.path(
  AUDIT_ROOT,
  "geography_06b3a_location_id_mapping.csv"
)

CANONICAL_COMPARISON_FILE <- file.path(
  AUDIT_ROOT,
  "geography_06b3a_canonical_wcvp_comparison.csv"
)

DUPLICATE_LOCALITY_FILE <- file.path(
  AUDIT_ROOT,
  "geography_06b3a_duplicate_locality_labels.csv"
)

SUMMARY_FILE <- file.path(
  AUDIT_ROOT,
  "geography_06b3a_summary.csv"
)

VALIDATION_FILE <- file.path(
  AUDIT_ROOT,
  "geography_06b3a_validation_gate.csv"
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
# 05. HELPER: NORMALISE LABEL FOR COMPARISON
#
# This does NOT alter source labels.
#
# It creates a comparison-only representation to identify labels differing
# only in case or repeated whitespace.
# ------------------------------------------------------------------------------

normalise_label_for_comparison <- function(x) {
  
  y <- normalise_text(x)
  
  y <- gsub(
    "[[:space:]]+",
    " ",
    y
  )
  
  y <- tolower(y)
  
  y
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
# 07. HELPER: COLLAPSE UNIQUE VALUES
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
# 08. HELPER: SAFE CSV READ
# ------------------------------------------------------------------------------

safe_read_csv <- function(path) {
  
  if (!file.exists(path)) {
    
    stop(
      paste0(
        "Required file not found:\n",
        path
      )
    )
  }
  
  result <- tryCatch(
    
    read.csv(
      path,
      stringsAsFactors = FALSE,
      check.names = FALSE,
      fileEncoding = "UTF-8"
    ),
    
    error = function(e1) {
      
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
              "Unable to read file:\n",
              path,
              "\n\n",
              "UTF-8 error:\n",
              conditionMessage(e1),
              "\n\n",
              "UTF-8-BOM error:\n",
              conditionMessage(e2)
            )
          )
        }
      )
    }
  )
  
  result
}


# ------------------------------------------------------------------------------
# 09. VALIDATE REQUIRED INPUT FILES
# ------------------------------------------------------------------------------

required_inputs <- c(
  CANONICAL_UNIT_FILE,
  WCVP_UNIT_FILE,
  APPROVED_FIELD_FILE,
  STRUCTURAL_GATE_06B3_FILE
)

missing_inputs <- required_inputs[
  !file.exists(required_inputs)
]

if (length(missing_inputs) > 0L) {
  
  stop(
    paste0(
      "Required input file(s) missing:\n",
      paste(
        missing_inputs,
        collapse = "\n"
      )
    )
  )
}


# ------------------------------------------------------------------------------
# 10. LOAD PIPELINE PRODUCTS
# ------------------------------------------------------------------------------

canonical_units <- safe_read_csv(
  CANONICAL_UNIT_FILE
)

wcvp_units <- safe_read_csv(
  WCVP_UNIT_FILE
)

approved_fields <- safe_read_csv(
  APPROVED_FIELD_FILE
)

structural_gate_06b3 <- safe_read_csv(
  STRUCTURAL_GATE_06B3_FILE
)


# ------------------------------------------------------------------------------
# 11. REQUIRE 06b3 STRUCTURAL PASS
# ------------------------------------------------------------------------------

if (!"passed" %in% names(structural_gate_06b3)) {
  
  stop(
    "06b3 structural validation table lacks field 'passed'."
  )
}


structural_gate_06b3$passed <- as.logical(
  structural_gate_06b3$passed
)


if (
  any(is.na(structural_gate_06b3$passed)) ||
  !all(structural_gate_06b3$passed)
) {
  
  stop(
    paste0(
      "06b3 structural validation did not pass completely.\n",
      "Do not validate the WCVP locality mapping."
    )
  )
}


# ------------------------------------------------------------------------------
# 12. LOCATE AUTHORITATIVE WCVP REFERENCE
# ------------------------------------------------------------------------------

required_approved_fields <- c(
  "file_name",
  "source_file",
  "field_name"
)

missing_approved_fields <- setdiff(
  required_approved_fields,
  names(approved_fields)
)

if (length(missing_approved_fields) > 0L) {
  
  stop(
    paste0(
      "Approved-field register lacks required field(s):\n",
      paste(
        missing_approved_fields,
        collapse = "\n"
      )
    )
  )
}


wcvp_reference_selector <- (
  approved_fields$file_name ==
    "vpjd_wcvp_location_reference.csv"
)

wcvp_reference_selector[
  is.na(wcvp_reference_selector)
] <- FALSE


wcvp_reference_paths <- unique_nonmissing(
  approved_fields$source_file[
    wcvp_reference_selector
  ]
)


if (length(wcvp_reference_paths) != 1L) {
  
  stop(
    paste0(
      "Expected exactly one WCVP location-reference source path; found ",
      length(wcvp_reference_paths),
      "."
    )
  )
}


WCVP_REFERENCE_FILE <- wcvp_reference_paths[1L]


if (!file.exists(WCVP_REFERENCE_FILE)) {
  
  stop(
    paste0(
      "Authoritative WCVP reference file not found:\n",
      WCVP_REFERENCE_FILE
    )
  )
}


# ------------------------------------------------------------------------------
# 13. READ WCVP REFERENCE
# ------------------------------------------------------------------------------

wcvp_reference <- safe_read_csv(
  WCVP_REFERENCE_FILE
)


# ------------------------------------------------------------------------------
# 14. VALIDATE EXPECTED WCVP REFERENCE SCHEMA
# ------------------------------------------------------------------------------

required_wcvp_fields <- c(
  "wcvp_location_id",
  "wcvp_locality",
  "tdwg_code",
  "location_reference_source"
)

missing_wcvp_fields <- setdiff(
  required_wcvp_fields,
  names(wcvp_reference)
)


if (length(missing_wcvp_fields) > 0L) {
  
  stop(
    paste0(
      "WCVP reference lacks required field(s):\n",
      paste(
        missing_wcvp_fields,
        collapse = "\n"
      )
    )
  )
}


# ------------------------------------------------------------------------------
# 15. NORMALISE WORKING VALUES
#
# Original source data remain untouched.
# ------------------------------------------------------------------------------

reference_work <- data.frame(
  
  wcvp_location_id = normalise_text(
    wcvp_reference$wcvp_location_id
  ),
  
  wcvp_locality = normalise_text(
    wcvp_reference$wcvp_locality
  ),
  
  tdwg_code = normalise_text(
    wcvp_reference$tdwg_code
  ),
  
  location_reference_source = normalise_text(
    wcvp_reference$location_reference_source
  ),
  
  stringsAsFactors = FALSE
)


reference_work$wcvp_locality_comparison <- normalise_label_for_comparison(
  reference_work$wcvp_locality
)


# ------------------------------------------------------------------------------
# 16. CONSOLE HEADER
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" VPJD GEOGRAPHY 06b3a - WCVP LOCALITY VALIDATION\n")
cat("============================================================\n\n")


cat(
  "WCVP reference:\n",
  WCVP_REFERENCE_FILE,
  "\n\n",
  sep = ""
)

cat(
  "Reference rows: ",
  nrow(reference_work),
  "\n\n",
  sep = ""
)


# ------------------------------------------------------------------------------
# 17. BASIC REFERENCE PROFILE
# ------------------------------------------------------------------------------

reference_profile <- data.frame(
  
  metric = c(
    "reference_rows",
    "distinct_wcvp_location_ids",
    "distinct_wcvp_localities",
    "distinct_normalised_wcvp_localities",
    "distinct_tdwg_codes",
    "distinct_location_reference_sources",
    "missing_wcvp_location_ids",
    "missing_wcvp_localities",
    "missing_tdwg_codes",
    "missing_location_reference_sources"
  ),
  
  value = c(
    nrow(reference_work),
    
    length(
      unique_nonmissing(
        reference_work$wcvp_location_id
      )
    ),
    
    length(
      unique_nonmissing(
        reference_work$wcvp_locality
      )
    ),
    
    length(
      unique_nonmissing(
        reference_work$wcvp_locality_comparison
      )
    ),
    
    length(
      unique_nonmissing(
        reference_work$tdwg_code
      )
    ),
    
    length(
      unique_nonmissing(
        reference_work$location_reference_source
      )
    ),
    
    sum(
      is.na(
        reference_work$wcvp_location_id
      )
    ),
    
    sum(
      is.na(
        reference_work$wcvp_locality
      )
    ),
    
    sum(
      is.na(
        reference_work$tdwg_code
      )
    ),
    
    sum(
      is.na(
        reference_work$location_reference_source
      )
    )
  ),
  
  stringsAsFactors = FALSE
)


# ------------------------------------------------------------------------------
# 18. BUILD TDWG CODE -> LOCALITY AUDIT
# ------------------------------------------------------------------------------

tdwg_codes <- unique_nonmissing(
  reference_work$tdwg_code
)


code_mapping_rows <- vector(
  mode = "list",
  length = length(tdwg_codes)
)


for (i in seq_along(tdwg_codes)) {
  
  code_value <- tdwg_codes[i]
  
  selector <- (
    reference_work$tdwg_code ==
      code_value
  )
  
  selector[is.na(selector)] <- FALSE
  
  
  subset_rows <- reference_work[
    selector,
    ,
    drop = FALSE
  ]
  
  
  locality_values <- unique_nonmissing(
    subset_rows$wcvp_locality
  )
  
  
  normalised_locality_values <- unique_nonmissing(
    subset_rows$wcvp_locality_comparison
  )
  
  
  location_ids <- unique_nonmissing(
    subset_rows$wcvp_location_id
  )
  
  
  provenance_values <- unique_nonmissing(
    subset_rows$location_reference_source
  )
  
  
  code_mapping_rows[[i]] <- data.frame(
    
    tdwg_code = code_value,
    
    wcvp_locality = if (
      length(locality_values) == 1L
    ) {
      locality_values[1L]
    } else {
      collapse_unique(locality_values)
    },
    
    wcvp_location_id = if (
      length(location_ids) == 1L
    ) {
      location_ids[1L]
    } else {
      collapse_unique(location_ids)
    },
    
    location_reference_source = if (
      length(provenance_values) == 1L
    ) {
      provenance_values[1L]
    } else {
      collapse_unique(provenance_values)
    },
    
    n_reference_rows = nrow(
      subset_rows
    ),
    
    n_distinct_localities = length(
      locality_values
    ),
    
    n_distinct_normalised_localities = length(
      normalised_locality_values
    ),
    
    n_distinct_location_ids = length(
      location_ids
    ),
    
    n_distinct_reference_sources = length(
      provenance_values
    ),
    
    locality_complete = (
      length(locality_values) == 1L
    ),
    
    locality_mapping_unambiguous = (
      length(normalised_locality_values) == 1L
    ),
    
    location_id_mapping_unambiguous = (
      length(location_ids) == 1L
    ),
    
    provenance_mapping_unambiguous = (
      length(provenance_values) == 1L
    ),
    
    stringsAsFactors = FALSE
  )
}


code_mapping_audit <- do.call(
  rbind,
  code_mapping_rows
)


rownames(code_mapping_audit) <- NULL


# ------------------------------------------------------------------------------
# 19. BUILD LOCALITY -> TDWG CODE AUDIT
#
# This is deliberately diagnostic.
#
# Two different TDWG codes sharing the same locality label is not silently
# accepted as equivalent.
# ------------------------------------------------------------------------------

locality_values <- unique_nonmissing(
  reference_work$wcvp_locality_comparison
)


locality_mapping_rows <- vector(
  mode = "list",
  length = length(locality_values)
)


for (i in seq_along(locality_values)) {
  
  locality_value <- locality_values[i]
  
  selector <- (
    reference_work$wcvp_locality_comparison ==
      locality_value
  )
  
  selector[is.na(selector)] <- FALSE
  
  
  subset_rows <- reference_work[
    selector,
    ,
    drop = FALSE
  ]
  
  
  source_labels <- unique_nonmissing(
    subset_rows$wcvp_locality
  )
  
  
  codes_found <- unique_nonmissing(
    subset_rows$tdwg_code
  )
  
  
  location_ids_found <- unique_nonmissing(
    subset_rows$wcvp_location_id
  )
  
  
  locality_mapping_rows[[i]] <- data.frame(
    
    locality_comparison_key = locality_value,
    
    source_locality_labels = collapse_unique(
      source_labels
    ),
    
    tdwg_codes = collapse_unique(
      codes_found
    ),
    
    wcvp_location_ids = collapse_unique(
      location_ids_found
    ),
    
    n_distinct_source_labels = length(
      source_labels
    ),
    
    n_distinct_tdwg_codes = length(
      codes_found
    ),
    
    n_distinct_location_ids = length(
      location_ids_found
    ),
    
    locality_to_code_unambiguous = (
      length(codes_found) == 1L
    ),
    
    stringsAsFactors = FALSE
  )
}


locality_mapping_audit <- do.call(
  rbind,
  locality_mapping_rows
)


rownames(locality_mapping_audit) <- NULL


# ------------------------------------------------------------------------------
# 20. BUILD WCVP LOCATION ID AUDIT
# ------------------------------------------------------------------------------

location_ids <- unique_nonmissing(
  reference_work$wcvp_location_id
)


location_id_rows <- vector(
  mode = "list",
  length = length(location_ids)
)


for (i in seq_along(location_ids)) {
  
  location_id_value <- location_ids[i]
  
  selector <- (
    reference_work$wcvp_location_id ==
      location_id_value
  )
  
  selector[is.na(selector)] <- FALSE
  
  
  subset_rows <- reference_work[
    selector,
    ,
    drop = FALSE
  ]
  
  
  codes_found <- unique_nonmissing(
    subset_rows$tdwg_code
  )
  
  
  localities_found <- unique_nonmissing(
    subset_rows$wcvp_locality
  )
  
  
  location_id_rows[[i]] <- data.frame(
    
    wcvp_location_id = location_id_value,
    
    tdwg_codes = collapse_unique(
      codes_found
    ),
    
    wcvp_localities = collapse_unique(
      localities_found
    ),
    
    n_distinct_tdwg_codes = length(
      codes_found
    ),
    
    n_distinct_localities = length(
      localities_found
    ),
    
    location_id_mapping_unambiguous = (
      length(codes_found) == 1L &&
        length(localities_found) == 1L
    ),
    
    stringsAsFactors = FALSE
  )
}


location_id_audit <- do.call(
  rbind,
  location_id_rows
)


rownames(location_id_audit) <- NULL


# ------------------------------------------------------------------------------
# 21. IDENTIFY DUPLICATE LOCALITY LABELS
# ------------------------------------------------------------------------------

duplicate_locality_selector <- (
  locality_mapping_audit$n_distinct_tdwg_codes >
    1L
)

duplicate_locality_selector[
  is.na(duplicate_locality_selector)
] <- FALSE


duplicate_localities <- locality_mapping_audit[
  duplicate_locality_selector,
  ,
  drop = FALSE
]


# ------------------------------------------------------------------------------
# 22. EXTRACT CANONICAL WCVP CONCEPTS
# ------------------------------------------------------------------------------

required_canonical_fields <- c(
  "geographic_unit_id",
  "geographic_code",
  "geographic_name",
  "concept_group",
  "concept_resolution_status",
  "identifier_status"
)

missing_canonical_fields <- setdiff(
  required_canonical_fields,
  names(canonical_units)
)


if (length(missing_canonical_fields) > 0L) {
  
  stop(
    paste0(
      "Canonical vocabulary lacks required field(s):\n",
      paste(
        missing_canonical_fields,
        collapse = "\n"
      )
    )
  )
}


canonical_selector <- (
  canonical_units$concept_group ==
    "WCVP_TDWG_LOCATION"
)

canonical_selector[
  is.na(canonical_selector)
] <- FALSE


canonical_wcvp <- canonical_units[
  canonical_selector,
  ,
  drop = FALSE
]


# ------------------------------------------------------------------------------
# 23. VALIDATE CANONICAL WCVP CARDINALITY
# ------------------------------------------------------------------------------

if (nrow(canonical_wcvp) != 367L) {
  
  stop(
    paste0(
      "Expected 367 canonical WCVP/TDWG concepts; found ",
      nrow(canonical_wcvp),
      "."
    )
  )
}


# ------------------------------------------------------------------------------
# 24. PREPARE CANONICAL COMPARISON
# ------------------------------------------------------------------------------

canonical_comparison <- data.frame(
  
  geographic_unit_id = normalise_text(
    canonical_wcvp$geographic_unit_id
  ),
  
  tdwg_code = normalise_text(
    canonical_wcvp$geographic_code
  ),
  
  current_geographic_name = normalise_text(
    canonical_wcvp$geographic_name
  ),
  
  current_resolution_status = normalise_text(
    canonical_wcvp$concept_resolution_status
  ),
  
  identifier_status = normalise_text(
    canonical_wcvp$identifier_status
  ),
  
  stringsAsFactors = FALSE
)


# ------------------------------------------------------------------------------
# 25. JOIN VALIDATED REFERENCE VALUES TO CANONICAL CODES
# ------------------------------------------------------------------------------

reference_crosswalk_work <- code_mapping_audit[
  ,
  c(
    "tdwg_code",
    "wcvp_locality",
    "wcvp_location_id",
    "location_reference_source",
    "n_distinct_localities",
    "n_distinct_normalised_localities",
    "n_distinct_location_ids",
    "n_distinct_reference_sources",
    "locality_complete",
    "locality_mapping_unambiguous",
    "location_id_mapping_unambiguous",
    "provenance_mapping_unambiguous"
  ),
  drop = FALSE
]


canonical_comparison <- merge(
  canonical_comparison,
  reference_crosswalk_work,
  by = "tdwg_code",
  all.x = TRUE,
  all.y = FALSE,
  sort = FALSE
)


# Restore deterministic canonical order.
canonical_comparison <- canonical_comparison[
  order(
    canonical_comparison$geographic_unit_id
  ),
  ,
  drop = FALSE
]


rownames(canonical_comparison) <- NULL


# ------------------------------------------------------------------------------
# 26. ADD COMPARISON FLAGS
# ------------------------------------------------------------------------------

canonical_comparison$reference_match_present <- (
  !is.na(
    canonical_comparison$wcvp_locality
  )
)


canonical_comparison$current_name_missing <- is.na(
  canonical_comparison$current_geographic_name
)


canonical_comparison$ready_for_name_integration <- (
  canonical_comparison$reference_match_present &
    canonical_comparison$locality_complete &
    canonical_comparison$locality_mapping_unambiguous &
    canonical_comparison$location_id_mapping_unambiguous &
    canonical_comparison$provenance_mapping_unambiguous
)


canonical_comparison$ready_for_name_integration[
  is.na(
    canonical_comparison$ready_for_name_integration
  )
] <- FALSE


# ------------------------------------------------------------------------------
# 27. COMPARE CANONICAL AND REFERENCE CODE SETS
# ------------------------------------------------------------------------------

canonical_codes <- unique_nonmissing(
  canonical_comparison$tdwg_code
)


reference_codes <- unique_nonmissing(
  reference_work$tdwg_code
)


canonical_codes_not_in_reference <- setdiff(
  canonical_codes,
  reference_codes
)


reference_codes_not_in_canonical <- setdiff(
  reference_codes,
  canonical_codes
)


# ------------------------------------------------------------------------------
# 28. BUILD VALIDATED CROSSWALK
#
# This output is generated only from code mappings that satisfy every mapping
# requirement.
#
# It does NOT modify the canonical vocabulary.
# ------------------------------------------------------------------------------

validated_selector <- (
  code_mapping_audit$locality_complete &
    code_mapping_audit$locality_mapping_unambiguous &
    code_mapping_audit$location_id_mapping_unambiguous &
    code_mapping_audit$provenance_mapping_unambiguous
)

validated_selector[
  is.na(validated_selector)
] <- FALSE


validated_crosswalk <- code_mapping_audit[
  validated_selector,
  c(
    "tdwg_code",
    "wcvp_locality",
    "wcvp_location_id",
    "location_reference_source"
  ),
  drop = FALSE
]


validated_crosswalk$validation_status <-
  "VALIDATED_06B3A"


validated_crosswalk <- validated_crosswalk[
  order(
    validated_crosswalk$tdwg_code
  ),
  ,
  drop = FALSE
]


rownames(validated_crosswalk) <- NULL


# ------------------------------------------------------------------------------
# 29. CORE COUNTS
# ------------------------------------------------------------------------------

n_reference_rows <- nrow(
  reference_work
)

n_reference_codes <- length(
  reference_codes
)

n_reference_localities <- length(
  unique_nonmissing(
    reference_work$wcvp_locality
  )
)

n_reference_localities_normalised <- length(
  unique_nonmissing(
    reference_work$wcvp_locality_comparison
  )
)

n_location_ids <- length(
  unique_nonmissing(
    reference_work$wcvp_location_id
  )
)

n_missing_location_ids <- sum(
  is.na(
    reference_work$wcvp_location_id
  )
)

n_missing_localities <- sum(
  is.na(
    reference_work$wcvp_locality
  )
)

n_missing_codes <- sum(
  is.na(
    reference_work$tdwg_code
  )
)

n_missing_provenance <- sum(
  is.na(
    reference_work$location_reference_source
  )
)

n_code_to_locality_conflicts <- sum(
  !code_mapping_audit$locality_mapping_unambiguous
)

n_code_to_location_id_conflicts <- sum(
  !code_mapping_audit$location_id_mapping_unambiguous
)

n_code_to_provenance_conflicts <- sum(
  !code_mapping_audit$provenance_mapping_unambiguous
)

n_duplicate_locality_labels <- nrow(
  duplicate_localities
)

n_location_id_conflicts <- sum(
  !location_id_audit$location_id_mapping_unambiguous
)

n_canonical_codes_not_in_reference <- length(
  canonical_codes_not_in_reference
)

n_reference_codes_not_in_canonical <- length(
  reference_codes_not_in_canonical
)

n_ready_for_integration <- sum(
  canonical_comparison$ready_for_name_integration
)

n_validated_crosswalk <- nrow(
  validated_crosswalk
)


# ------------------------------------------------------------------------------
# 30. PROVENANCE PROFILE
# ------------------------------------------------------------------------------

provenance_values <- unique_nonmissing(
  reference_work$location_reference_source
)


provenance_profile <- data.frame(
  
  location_reference_source =
    provenance_values,
  
  n_rows = vapply(
    provenance_values,
    function(source_value) {
      
      selector <- (
        reference_work$location_reference_source ==
          source_value
      )
      
      selector[is.na(selector)] <- FALSE
      
      sum(selector)
    },
    integer(1)
  ),
  
  stringsAsFactors = FALSE
)


# ------------------------------------------------------------------------------
# 31. SUMMARY TABLE
# ------------------------------------------------------------------------------

summary_table <- data.frame(
  
  metric = c(
    "reference_rows",
    "distinct_wcvp_location_ids",
    "distinct_wcvp_localities",
    "distinct_normalised_wcvp_localities",
    "distinct_tdwg_codes",
    "canonical_wcvp_concepts",
    "missing_wcvp_location_ids",
    "missing_wcvp_localities",
    "missing_tdwg_codes",
    "missing_location_reference_sources",
    "tdwg_code_to_locality_conflicts",
    "tdwg_code_to_location_id_conflicts",
    "tdwg_code_to_provenance_conflicts",
    "duplicate_locality_labels_across_codes",
    "wcvp_location_id_mapping_conflicts",
    "canonical_codes_not_in_reference",
    "reference_codes_not_in_canonical",
    "canonical_concepts_ready_for_name_integration",
    "validated_crosswalk_rows"
  ),
  
  value = c(
    n_reference_rows,
    n_location_ids,
    n_reference_localities,
    n_reference_localities_normalised,
    n_reference_codes,
    nrow(canonical_wcvp),
    n_missing_location_ids,
    n_missing_localities,
    n_missing_codes,
    n_missing_provenance,
    n_code_to_locality_conflicts,
    n_code_to_location_id_conflicts,
    n_code_to_provenance_conflicts,
    n_duplicate_locality_labels,
    n_location_id_conflicts,
    n_canonical_codes_not_in_reference,
    n_reference_codes_not_in_canonical,
    n_ready_for_integration,
    n_validated_crosswalk
  ),
  
  stringsAsFactors = FALSE
)


# ------------------------------------------------------------------------------
# 32. VALIDATION GATE
#
# Important distinction:
#
# TDWG code -> locality MUST be unambiguous.
#
# Locality -> TDWG code is audited separately because duplicate human-readable
# labels may be legitimate in a geographic reference system. Such duplicates
# are therefore reported rather than automatically treated as a fatal failure.
# ------------------------------------------------------------------------------

validation_gate <- data.frame(
  
  criterion = c(
    "06b3_structural_validation_passed",
    "wcvp_reference_has_required_fields",
    "reference_row_count_367",
    "distinct_tdwg_code_count_367",
    "canonical_wcvp_concept_count_367",
    "wcvp_location_ids_complete",
    "wcvp_localities_complete",
    "tdwg_codes_complete",
    "location_reference_sources_complete",
    "tdwg_code_to_locality_mapping_unambiguous",
    "tdwg_code_to_location_id_mapping_unambiguous",
    "tdwg_code_to_provenance_mapping_unambiguous",
    "wcvp_location_id_mapping_unambiguous",
    "all_canonical_codes_present_in_reference",
    "all_reference_codes_present_in_canonical",
    "all_canonical_concepts_ready_for_name_integration",
    "validated_crosswalk_count_367",
    "canonical_vocabulary_not_modified",
    "identifiers_not_frozen",
    "no_locality_names_inferred",
    "published_vpjd_not_modified"
  ),
  
  passed = c(
    all(
      structural_gate_06b3$passed
    ),
    
    all(
      required_wcvp_fields %in%
        names(wcvp_reference)
    ),
    
    n_reference_rows == 367L,
    
    n_reference_codes == 367L,
    
    nrow(canonical_wcvp) == 367L,
    
    n_missing_location_ids == 0L,
    
    n_missing_localities == 0L,
    
    n_missing_codes == 0L,
    
    n_missing_provenance == 0L,
    
    n_code_to_locality_conflicts == 0L,
    
    n_code_to_location_id_conflicts == 0L,
    
    n_code_to_provenance_conflicts == 0L,
    
    n_location_id_conflicts == 0L,
    
    n_canonical_codes_not_in_reference == 0L,
    
    n_reference_codes_not_in_canonical == 0L,
    
    n_ready_for_integration == 367L,
    
    n_validated_crosswalk == 367L,
    
    TRUE,
    
    TRUE,
    
    TRUE,
    
    TRUE
  ),
  
  stringsAsFactors = FALSE
)


# ------------------------------------------------------------------------------
# 33. WRITE AUDIT OUTPUTS
# ------------------------------------------------------------------------------

write.csv(
  reference_profile,
  REFERENCE_PROFILE_FILE,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)


write.csv(
  code_mapping_audit,
  CODE_MAPPING_AUDIT_FILE,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)


write.csv(
  locality_mapping_audit,
  LOCALITY_MAPPING_AUDIT_FILE,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)


write.csv(
  location_id_audit,
  LOCATION_ID_AUDIT_FILE,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)


write.csv(
  canonical_comparison,
  CANONICAL_COMPARISON_FILE,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)


write.csv(
  duplicate_localities,
  DUPLICATE_LOCALITY_FILE,
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
# 34. WRITE VALIDATED CROSSWALK ONLY AFTER VALIDATION
# ------------------------------------------------------------------------------

if (all(validation_gate$passed)) {
  
  write.csv(
    validated_crosswalk,
    VALIDATED_CROSSWALK_FILE,
    row.names = FALSE,
    na = "",
    fileEncoding = "UTF-8"
  )
  
}


# ------------------------------------------------------------------------------
# 35. CONSOLE SUMMARY
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" VPJD GEOGRAPHY 06b3a - VALIDATION COMPLETE\n")
cat("============================================================\n\n")


cat("Summary:\n\n")

print(
  summary_table,
  row.names = FALSE
)


# ------------------------------------------------------------------------------
# 36. REFERENCE PROFILE
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" WCVP REFERENCE PROFILE\n")
cat("============================================================\n\n")


print(
  reference_profile,
  row.names = FALSE
)


# ------------------------------------------------------------------------------
# 37. PROVENANCE PROFILE
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" LOCATION REFERENCE SOURCE\n")
cat("============================================================\n\n")


print(
  provenance_profile,
  row.names = FALSE
)


# ------------------------------------------------------------------------------
# 38. DUPLICATE LOCALITY LABELS
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" LOCALITY LABEL DUPLICATION AUDIT\n")
cat("============================================================\n\n")


if (nrow(duplicate_localities) == 0L) {
  
  cat(
    "No normalised locality label is associated with more than one TDWG code.\n"
  )
  
} else {
  
  cat(
    paste0(
      nrow(duplicate_localities),
      " normalised locality label(s) are associated with more than one ",
      "TDWG code.\n\n"
    )
  )
  
  print(
    duplicate_localities,
    row.names = FALSE
  )
}


# ------------------------------------------------------------------------------
# 39. VALIDATION GATE
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
# 40. INTERPRETATION
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" INTERPRETATION\n")
cat("============================================================\n\n")


if (all(validation_gate$passed)) {
  
  cat(
    paste0(
      "PASS. wcvp_locality is validated as the human-readable geographic ",
      "label associated with tdwg_code in the current WCVP location-reference ",
      "product.\n\n"
    )
  )
  
  cat(
    paste0(
      format(
        n_reference_codes,
        big.mark = ","
      ),
      " TDWG codes are present, and all map unambiguously to one populated ",
      "WCVP locality, one WCVP location identifier and one recorded reference ",
      "source.\n\n"
    )
  )
  
  cat(
    paste0(
      "All ",
      format(
        n_ready_for_integration,
        big.mark = ","
      ),
      " canonical WCVP/TDWG concepts are therefore ready for controlled ",
      "name integration.\n\n"
    )
  )
  
  if (n_duplicate_locality_labels > 0L) {
    
    cat(
      paste0(
        "The reverse locality-to-code audit identified ",
        format(
          n_duplicate_locality_labels,
          big.mark = ","
        ),
        " locality label(s) associated with multiple TDWG codes. These have ",
        "been retained as an audit finding and do not invalidate the ",
        "code-to-locality mapping.\n\n"
      )
    )
  }
  
  cat(
    paste0(
      "A validated 367-row TDWG code-to-locality crosswalk has been written ",
      "for use by the next integration stage.\n"
    )
  )
  
} else {
  
  cat(
    "FAIL. One or more WCVP locality-validation criteria failed.\n\n"
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
# 41. NEXT STEP
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" NEXT STEP\n")
cat("============================================================\n\n")


if (all(validation_gate$passed)) {
  
  cat(
    paste0(
      "Proceed to:\n\n",
      "  geography_06b3b_integrate_wcvp_locality_names.R\n\n",
      "06b3b should use ONLY the validated crosswalk produced here to:\n\n",
      "  1. populate geographic_name for the 367 WCVP/TDWG concepts;\n",
      "  2. retain the existing geographic_unit_id values unchanged;\n",
      "  3. retain tdwg_code unchanged;\n",
      "  4. retain wcvp_location_id and source provenance;\n",
      "  5. update concept_resolution_status only where validation permits;\n",
      "  6. preserve identifier_status as provisional;\n",
      "  7. rerun the relevant 06b3 release-readiness tests;\n",
      "  8. NOT freeze VPJD-GEO identifiers.\n"
    )
  )
  
} else {
  
  cat(
    paste0(
      "Do not integrate WCVP locality names into the canonical vocabulary.\n",
      "Resolve the failed validation criterion or criteria first.\n"
    )
  )
}


# ------------------------------------------------------------------------------
# 42. OUTPUT LOCATIONS
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" OUTPUTS\n")
cat("============================================================\n\n")


cat(
  "Reference profile:\n",
  REFERENCE_PROFILE_FILE,
  "\n\n",
  sep = ""
)

cat(
  "TDWG code -> locality audit:\n",
  CODE_MAPPING_AUDIT_FILE,
  "\n\n",
  sep = ""
)

cat(
  "Locality -> TDWG code audit:\n",
  LOCALITY_MAPPING_AUDIT_FILE,
  "\n\n",
  sep = ""
)

cat(
  "WCVP location-ID audit:\n",
  LOCATION_ID_AUDIT_FILE,
  "\n\n",
  sep = ""
)

cat(
  "Canonical comparison:\n",
  CANONICAL_COMPARISON_FILE,
  "\n\n",
  sep = ""
)

cat(
  "Duplicate locality-label audit:\n",
  DUPLICATE_LOCALITY_FILE,
  "\n\n",
  sep = ""
)

cat(
  "Validation gate:\n",
  VALIDATION_FILE,
  "\n\n",
  sep = ""
)


if (all(validation_gate$passed)) {
  
  cat(
    "Validated crosswalk:\n",
    VALIDATED_CROSSWALK_FILE,
    "\n\n",
    sep = ""
  )
  
} else {
  
  cat(
    "Validated crosswalk: NOT WRITTEN because validation did not pass.\n\n"
  )
}


# ------------------------------------------------------------------------------
# 43. SAFEGUARDS
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" SAFEGUARDS\n")
cat("============================================================\n\n")


cat("WCVP source reference was NOT modified.\n")
cat("06b2 canonical geographic vocabulary was NOT modified.\n")
cat("06b3 audit outputs were NOT modified.\n")
cat("wcvp_locality values were NOT inferred or rewritten.\n")
cat("TDWG codes were NOT inferred or rewritten.\n")
cat("VPJD-GEO identifiers were NOT changed.\n")
cat("VPJD-GEO identifiers were NOT frozen.\n")
cat("Geographic hierarchy was NOT inferred.\n")
cat("Geographic geometry was NOT inferred.\n")
cat("Missing geographic evidence was NOT interpreted as absence.\n")
cat("Nakamura predicates were NOT evaluated.\n")
cat("Star categories were NOT evaluated.\n")
cat("Published VPJD Taxonomic Release v1.0.0 was NOT modified.\n")
cat("VPJD Geography v1.0.0 was NOT frozen or published.\n")

cat("\n")
cat("============================================================\n")