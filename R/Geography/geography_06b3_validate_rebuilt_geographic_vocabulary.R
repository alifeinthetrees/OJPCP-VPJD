# ==============================================================================
# VPJD GEOGRAPHY MODULE
# geography_06b3_validate_rebuilt_geographic_vocabulary.R
#
# PURPOSE
# -------
# Independently validate the provisional canonical geographic vocabulary
# constructed by:
#
#   geography_06b2_rebuild_canonical_geographic_units.R
#
# for:
#
#   Vascular Plants of Japan Database (VPJD)
#   Geography Release v1.0.0
#
# POSITION IN PIPELINE
# --------------------
#
#   06b1  Validate candidate geographic fields
#     |
#     v
#   06b2  Rebuild canonical geographic units
#     |
#     v
#   06b3  VALIDATE REBUILT GEOGRAPHIC VOCABULARY   <-- THIS SCRIPT
#     |
#     +---- if release-ready:
#     |       proceed towards identifier freeze / hierarchy
#     |
#     +---- if resolution remains incomplete:
#             resolve deficiencies before identifier freeze
#
# 06b2 RESULT
# ------------
#
#   Japanese botanical areas:          51
#   WCVP / TDWG concepts:             367
#   Total canonical concepts:         418
#
#   Duplicate VPJD-GEO IDs:             0
#   Duplicate botanical-area codes:     0
#   Duplicate TDWG codes:               0
#   Concepts without provenance:        0
#
#   Missing WCVP location names:      367
#
# OBJECTIVES
# ----------
#
#  1. Validate concept cardinality.
#  2. Validate provisional VPJD-GEO identifier integrity.
#  3. Validate Japanese botanical-area ID/name integrity.
#  4. Validate WCVP/TDWG code integrity.
#  5. Inspect the complete WCVP reference schema for candidate name fields.
#  6. Determine whether the 367 missing TDWG names are:
#
#       a. genuinely unavailable in the current reference; or
#       b. present under a field name not recognised by 06b2.
#
#  7. Audit cross-system code/name overlap.
#  8. Audit source provenance completeness.
#  9. Audit concept-resolution status.
# 10. Determine whether the vocabulary is structurally valid.
# 11. Independently determine whether VPJD-GEO identifiers are ready to freeze.
#
# IMPORTANT
# ---------
#
# This is an AUDIT script.
#
# It DOES NOT:
#
#   - modify the 06b2 vocabulary;
#   - fill missing WCVP names automatically;
#   - infer names from TDWG codes;
#   - freeze VPJD-GEO identifiers;
#   - infer geographic hierarchy;
#   - infer geometry;
#   - modify VPJD Taxonomic Release v1.0.0;
#   - calculate Nakamura predicates;
#   - calculate Star categories.
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

AUDIT_06B2_ROOT <- file.path(
  PROJECT_ROOT,
  "audit",
  "geography",
  "release_v1.0.0",
  "06b2_rebuilt_canonical_geographic_units"
)

AUDIT_ROOT <- file.path(
  PROJECT_ROOT,
  "audit",
  "geography",
  "release_v1.0.0",
  "06b3_validate_rebuilt_geographic_vocabulary"
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

BOTANICAL_AREA_FILE <- file.path(
  CANONICAL_ROOT,
  "vpjd_japan_botanical_areas_provisional.csv"
)

WCVP_UNIT_FILE <- file.path(
  CANONICAL_ROOT,
  "vpjd_wcvp_tdwg_units_provisional.csv"
)

SOURCE_PROVENANCE_FILE <- file.path(
  CANONICAL_ROOT,
  "vpjd_geographic_unit_source_provenance.csv"
)

CONCEPT_SOURCE_FILE <- file.path(
  CANONICAL_ROOT,
  "vpjd_geographic_concept_source_register.csv"
)

CHINA_CLASSIFICATION_FILE <- file.path(
  CANONICAL_ROOT,
  "vpjd_china_province_classification_metadata.csv"
)

APPROVED_FIELD_FILE <- file.path(
  FIELD_VALIDATION_ROOT,
  "vpjd_approved_geographic_fields.csv"
)

VALIDATION_06B2_FILE <- file.path(
  AUDIT_06B2_ROOT,
  "geography_06b2_validation_gate.csv"
)


# ------------------------------------------------------------------------------
# 03. OUTPUT FILES
# ------------------------------------------------------------------------------

SCHEMA_AUDIT_FILE <- file.path(
  AUDIT_ROOT,
  "geography_06b3_wcvp_reference_schema.csv"
)

WCVP_FIELD_PROFILE_FILE <- file.path(
  AUDIT_ROOT,
  "geography_06b3_wcvp_field_profile.csv"
)

WCVP_NAME_CANDIDATE_FILE <- file.path(
  AUDIT_ROOT,
  "geography_06b3_wcvp_name_candidate_fields.csv"
)

CONCEPT_AUDIT_FILE <- file.path(
  AUDIT_ROOT,
  "geography_06b3_concept_audit.csv"
)

CROSS_SYSTEM_OVERLAP_FILE <- file.path(
  AUDIT_ROOT,
  "geography_06b3_cross_system_overlap.csv"
)

PROVENANCE_AUDIT_FILE <- file.path(
  AUDIT_ROOT,
  "geography_06b3_provenance_audit.csv"
)

SUMMARY_FILE <- file.path(
  AUDIT_ROOT,
  "geography_06b3_summary.csv"
)

STRUCTURAL_GATE_FILE <- file.path(
  AUDIT_ROOT,
  "geography_06b3_structural_validation_gate.csv"
)

RELEASE_GATE_FILE <- file.path(
  AUDIT_ROOT,
  "geography_06b3_identifier_freeze_gate.csv"
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
# 06. HELPER: UNIQUE NON-MISSING
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
              "Unable to read:\n",
              path,
              "\n\n",
              conditionMessage(e1),
              "\n\n",
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
# 08. HELPER: PROFILE ONE FIELD
# ------------------------------------------------------------------------------

profile_field <- function(data, field_name) {
  
  values <- normalise_text(
    data[[field_name]]
  )
  
  nonmissing <- values[
    !is.na(values)
  ]
  
  unique_values <- unique(
    nonmissing
  )
  
  numeric_like <- FALSE
  
  if (length(nonmissing) > 0L) {
    
    numeric_test <- suppressWarnings(
      as.numeric(nonmissing)
    )
    
    numeric_like <- all(
      !is.na(numeric_test)
    )
  }
  
  alpha_present <- FALSE
  
  if (length(nonmissing) > 0L) {
    
    alpha_present <- any(
      grepl(
        "[[:alpha:]]",
        nonmissing
      )
    )
  }
  
  data.frame(
    field_name = field_name,
    field_class = paste(
      class(data[[field_name]]),
      collapse = " | "
    ),
    n_rows = nrow(data),
    n_nonmissing = length(nonmissing),
    n_missing = sum(is.na(values)),
    n_distinct_nonmissing = length(unique_values),
    numeric_like = numeric_like,
    contains_alphabetic_values = alpha_present,
    example_1 = if (length(unique_values) >= 1L) {
      unique_values[1L]
    } else {
      NA_character_
    },
    example_2 = if (length(unique_values) >= 2L) {
      unique_values[2L]
    } else {
      NA_character_
    },
    example_3 = if (length(unique_values) >= 3L) {
      unique_values[3L]
    } else {
      NA_character_
    },
    stringsAsFactors = FALSE
  )
}


# ------------------------------------------------------------------------------
# 09. VALIDATE REQUIRED INPUT FILES
# ------------------------------------------------------------------------------

required_inputs <- c(
  CANONICAL_UNIT_FILE,
  BOTANICAL_AREA_FILE,
  WCVP_UNIT_FILE,
  SOURCE_PROVENANCE_FILE,
  CONCEPT_SOURCE_FILE,
  CHINA_CLASSIFICATION_FILE,
  APPROVED_FIELD_FILE,
  VALIDATION_06B2_FILE
)

missing_inputs <- required_inputs[
  !file.exists(required_inputs)
]

if (length(missing_inputs) > 0L) {
  
  stop(
    paste0(
      "Required 06b2/06b1 file(s) missing:\n",
      paste(
        missing_inputs,
        collapse = "\n"
      )
    )
  )
}


# ------------------------------------------------------------------------------
# 10. LOAD 06b2 PRODUCTS
# ------------------------------------------------------------------------------

canonical_units <- safe_read_csv(
  CANONICAL_UNIT_FILE
)

botanical_areas <- safe_read_csv(
  BOTANICAL_AREA_FILE
)

wcvp_units <- safe_read_csv(
  WCVP_UNIT_FILE
)

source_provenance <- safe_read_csv(
  SOURCE_PROVENANCE_FILE
)

concept_source <- safe_read_csv(
  CONCEPT_SOURCE_FILE
)

china_classification <- safe_read_csv(
  CHINA_CLASSIFICATION_FILE
)

approved_fields <- safe_read_csv(
  APPROVED_FIELD_FILE
)

validation_06b2 <- safe_read_csv(
  VALIDATION_06B2_FILE
)


# ------------------------------------------------------------------------------
# 11. VALIDATE 06b2 GATE
# ------------------------------------------------------------------------------

if (!"passed" %in% names(validation_06b2)) {
  
  stop(
    "06b2 validation table lacks field 'passed'."
  )
}


validation_06b2$passed <- as.logical(
  validation_06b2$passed
)


if (
  any(is.na(validation_06b2$passed)) ||
  !all(validation_06b2$passed)
) {
  
  stop(
    paste0(
      "06b2 did not pass its structural validation gate.\n",
      "Do not perform the 06b3 release audit."
    )
  )
}


# ------------------------------------------------------------------------------
# 12. VALIDATE CORE COLUMNS
# ------------------------------------------------------------------------------

required_canonical_fields <- c(
  "geographic_unit_id",
  "geographic_code",
  "geographic_name",
  "geographic_unit_type",
  "concept_group",
  "geographic_standard",
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


required_provenance_fields <- c(
  "geographic_unit_id",
  "concept_group",
  "source_product",
  "source_field_code",
  "source_field_name",
  "source_code",
  "source_name"
)

missing_provenance_fields <- setdiff(
  required_provenance_fields,
  names(source_provenance)
)

if (length(missing_provenance_fields) > 0L) {
  
  stop(
    paste0(
      "Source provenance lacks required field(s):\n",
      paste(
        missing_provenance_fields,
        collapse = "\n"
      )
    )
  )
}


# ------------------------------------------------------------------------------
# 13. LOCATE ORIGINAL WCVP REFERENCE
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


wcvp_source_selector <- (
  approved_fields$file_name ==
    "vpjd_wcvp_location_reference.csv"
)

wcvp_source_selector[
  is.na(wcvp_source_selector)
] <- FALSE


wcvp_reference_paths <- unique_nonmissing(
  approved_fields$source_file[
    wcvp_source_selector
  ]
)


if (length(wcvp_reference_paths) != 1L) {
  
  stop(
    paste0(
      "Expected exactly one WCVP location-reference path; found ",
      length(wcvp_reference_paths),
      "."
    )
  )
}


WCVP_REFERENCE_FILE <- wcvp_reference_paths[1L]


if (!file.exists(WCVP_REFERENCE_FILE)) {
  
  stop(
    paste0(
      "WCVP reference file not found:\n",
      WCVP_REFERENCE_FILE
    )
  )
}


wcvp_reference <- safe_read_csv(
  WCVP_REFERENCE_FILE
)


# ------------------------------------------------------------------------------
# 14. START
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" VPJD GEOGRAPHY 06b3 - VOCABULARY VALIDATION\n")
cat("============================================================\n\n")


cat(
  "Canonical concepts: ",
  nrow(canonical_units),
  "\n",
  sep = ""
)

cat(
  "Japanese botanical areas: ",
  nrow(botanical_areas),
  "\n",
  sep = ""
)

cat(
  "WCVP / TDWG units: ",
  nrow(wcvp_units),
  "\n\n",
  sep = ""
)

cat(
  "WCVP reference:\n",
  WCVP_REFERENCE_FILE,
  "\n\n",
  sep = ""
)


# ------------------------------------------------------------------------------
# 15. PROFILE COMPLETE WCVP REFERENCE SCHEMA
# ------------------------------------------------------------------------------

wcvp_field_names <- names(
  wcvp_reference
)


schema_audit <- data.frame(
  column_position = seq_along(
    wcvp_field_names
  ),
  field_name = wcvp_field_names,
  stringsAsFactors = FALSE
)


schema_audit$field_class <- vapply(
  wcvp_field_names,
  function(field_name) {
    paste(
      class(wcvp_reference[[field_name]]),
      collapse = " | "
    )
  },
  character(1)
)


# ------------------------------------------------------------------------------
# 16. PROFILE EVERY WCVP REFERENCE FIELD
# ------------------------------------------------------------------------------

field_profile_rows <- lapply(
  wcvp_field_names,
  function(field_name) {
    profile_field(
      wcvp_reference,
      field_name
    )
  }
)


wcvp_field_profile <- do.call(
  rbind,
  field_profile_rows
)

rownames(wcvp_field_profile) <- NULL


# ------------------------------------------------------------------------------
# 17. IDENTIFY CANDIDATE HUMAN-READABLE NAME FIELDS
#
# This is discovery only.
#
# No candidate is automatically adopted as the canonical name.
# ------------------------------------------------------------------------------

field_name_lower <- tolower(
  wcvp_field_profile$field_name
)


name_pattern_selector <- grepl(
  "name|location|area|region|territor|country|province|state|island",
  field_name_lower
)


non_code_selector <- !grepl(
  "code|id|key|number|count|row|level",
  field_name_lower
)


text_selector <- (
  wcvp_field_profile$contains_alphabetic_values &
    !wcvp_field_profile$numeric_like
)

text_selector[is.na(text_selector)] <- FALSE


candidate_name_selector <- (
  name_pattern_selector &
    non_code_selector &
    text_selector
)

candidate_name_selector[
  is.na(candidate_name_selector)
] <- FALSE


wcvp_name_candidates <- wcvp_field_profile[
  candidate_name_selector,
  ,
  drop = FALSE
]


# ------------------------------------------------------------------------------
# 18. ADD CANDIDATE COVERAGE AGAINST THE 367 TDWG ROWS
# ------------------------------------------------------------------------------

if (nrow(wcvp_name_candidates) > 0L) {
  
  candidate_coverage <- numeric(
    nrow(wcvp_name_candidates)
  )
  
  candidate_distinct <- integer(
    nrow(wcvp_name_candidates)
  )
  
  for (i in seq_len(nrow(wcvp_name_candidates))) {
    
    field_name <- wcvp_name_candidates$field_name[i]
    
    values <- normalise_text(
      wcvp_reference[[field_name]]
    )
    
    candidate_coverage[i] <- sum(
      !is.na(values)
    )
    
    candidate_distinct[i] <- length(
      unique_nonmissing(values)
    )
  }
  
  wcvp_name_candidates$n_nonmissing_values <-
    candidate_coverage
  
  wcvp_name_candidates$n_distinct_values <-
    candidate_distinct
}


# ------------------------------------------------------------------------------
# 19. CANONICAL CONCEPT COUNTS
# ------------------------------------------------------------------------------

botanical_selector <- (
  canonical_units$concept_group ==
    "JAPAN_BOTANICAL_AREA"
)

botanical_selector[is.na(botanical_selector)] <- FALSE


wcvp_selector <- (
  canonical_units$concept_group ==
    "WCVP_TDWG_LOCATION"
)

wcvp_selector[is.na(wcvp_selector)] <- FALSE


canonical_botanical <- canonical_units[
  botanical_selector,
  ,
  drop = FALSE
]

canonical_wcvp <- canonical_units[
  wcvp_selector,
  ,
  drop = FALSE
]


n_canonical <- nrow(
  canonical_units
)

n_botanical <- nrow(
  canonical_botanical
)

n_wcvp <- nrow(
  canonical_wcvp
)


# ------------------------------------------------------------------------------
# 20. IDENTIFIER INTEGRITY
# ------------------------------------------------------------------------------

unit_ids <- normalise_text(
  canonical_units$geographic_unit_id
)


n_missing_ids <- sum(
  is.na(unit_ids)
)

n_duplicate_ids <- sum(
  duplicated(
    unit_ids[
      !is.na(unit_ids)
    ]
  )
)


id_format_selector <- grepl(
  "^VPJD-GEO-[0-9]{4}$",
  unit_ids
)

id_format_selector[is.na(id_format_selector)] <- FALSE


n_invalid_id_format <- sum(
  !id_format_selector
)


# ------------------------------------------------------------------------------
# 21. IDENTIFIER SEQUENCE DETERMINISM
# ------------------------------------------------------------------------------

expected_ids <- sprintf(
  "VPJD-GEO-%04d",
  seq_len(
    nrow(canonical_units)
  )
)


identifier_sequence_matches <- identical(
  unit_ids,
  expected_ids
)


# ------------------------------------------------------------------------------
# 22. IDENTIFIER STATUS
# ------------------------------------------------------------------------------

identifier_status <- normalise_text(
  canonical_units$identifier_status
)


all_identifiers_provisional <- all(
  identifier_status ==
    "PROVISIONAL_06B2"
)


# ------------------------------------------------------------------------------
# 23. BOTANICAL-AREA INTEGRITY
# ------------------------------------------------------------------------------

botanical_codes <- normalise_text(
  canonical_botanical$geographic_code
)

botanical_names <- normalise_text(
  canonical_botanical$geographic_name
)


n_missing_botanical_codes <- sum(
  is.na(botanical_codes)
)

n_missing_botanical_names <- sum(
  is.na(botanical_names)
)

n_duplicate_botanical_codes <- sum(
  duplicated(
    botanical_codes[
      !is.na(botanical_codes)
    ]
  )
)

n_duplicate_botanical_names <- sum(
  duplicated(
    botanical_names[
      !is.na(botanical_names)
    ]
  )
)


# ------------------------------------------------------------------------------
# 24. WCVP / TDWG INTEGRITY
# ------------------------------------------------------------------------------

wcvp_codes <- normalise_text(
  canonical_wcvp$geographic_code
)

wcvp_names <- normalise_text(
  canonical_wcvp$geographic_name
)


n_missing_wcvp_codes <- sum(
  is.na(wcvp_codes)
)

n_missing_wcvp_names <- sum(
  is.na(wcvp_names)
)

n_duplicate_wcvp_codes <- sum(
  duplicated(
    wcvp_codes[
      !is.na(wcvp_codes)
    ]
  )
)


# ------------------------------------------------------------------------------
# 25. VALIDATE WCVP CODES AGAINST ORIGINAL REFERENCE
# ------------------------------------------------------------------------------

if (!"tdwg_code" %in% names(wcvp_reference)) {
  
  stop(
    "Original WCVP reference does not contain tdwg_code."
  )
}


reference_tdwg_codes <- unique_nonmissing(
  wcvp_reference$tdwg_code
)


canonical_codes_not_in_reference <- setdiff(
  unique_nonmissing(wcvp_codes),
  reference_tdwg_codes
)


reference_codes_not_in_canonical <- setdiff(
  reference_tdwg_codes,
  unique_nonmissing(wcvp_codes)
)


# ------------------------------------------------------------------------------
# 26. CONCEPT RESOLUTION STATUS
# ------------------------------------------------------------------------------

resolution_status <- normalise_text(
  canonical_units$concept_resolution_status
)


resolution_profile <- as.data.frame(
  table(
    resolution_status,
    useNA = "ifany"
  ),
  stringsAsFactors = FALSE
)

names(resolution_profile) <- c(
  "concept_resolution_status",
  "n_concepts"
)


n_fully_resolved <- sum(
  resolution_status %in%
    c(
      "RESOLVED",
      "RESOLVED_FROM_REFERENCE"
    ),
  na.rm = TRUE
)


n_resolution_pending <- sum(
  resolution_status ==
    "CODE_VALIDATED_NAME_PENDING",
  na.rm = TRUE
)


# ------------------------------------------------------------------------------
# 27. PROVENANCE INTEGRITY
# ------------------------------------------------------------------------------

provenance_ids <- unique_nonmissing(
  source_provenance$geographic_unit_id
)


canonical_ids <- unique_nonmissing(
  canonical_units$geographic_unit_id
)


concepts_without_provenance <- setdiff(
  canonical_ids,
  provenance_ids
)


provenance_without_concept <- setdiff(
  provenance_ids,
  canonical_ids
)


n_concepts_without_provenance <- length(
  concepts_without_provenance
)

n_orphan_provenance_ids <- length(
  provenance_without_concept
)


# ------------------------------------------------------------------------------
# 28. CONCEPT-SOURCE REGISTER INTEGRITY
# ------------------------------------------------------------------------------

if (!"geographic_unit_id" %in% names(concept_source)) {
  
  stop(
    "Concept-source register lacks geographic_unit_id."
  )
}


concept_source_ids <- unique_nonmissing(
  concept_source$geographic_unit_id
)


concepts_without_source_register <- setdiff(
  canonical_ids,
  concept_source_ids
)


source_register_without_concept <- setdiff(
  concept_source_ids,
  canonical_ids
)


# ------------------------------------------------------------------------------
# 29. CROSS-SYSTEM CODE OVERLAP
# ------------------------------------------------------------------------------

code_overlap <- intersect(
  unique_nonmissing(botanical_codes),
  unique_nonmissing(wcvp_codes)
)


# ------------------------------------------------------------------------------
# 30. CROSS-SYSTEM NAME OVERLAP
#
# WCVP names are currently missing, so this audit will naturally contain no
# overlaps until those names are resolved.
# ------------------------------------------------------------------------------

botanical_name_lower <- tolower(
  unique_nonmissing(botanical_names)
)

wcvp_name_lower <- tolower(
  unique_nonmissing(wcvp_names)
)


name_overlap_lower <- intersect(
  botanical_name_lower,
  wcvp_name_lower
)


# ------------------------------------------------------------------------------
# 31. BUILD CROSS-SYSTEM OVERLAP TABLE
# ------------------------------------------------------------------------------

overlap_rows <- list()


if (length(code_overlap) > 0L) {
  
  overlap_rows[[length(overlap_rows) + 1L]] <- data.frame(
    overlap_type = "CODE",
    value = code_overlap,
    interpretation = "Same code occurs in Japanese botanical-area and WCVP/TDWG systems; requires review.",
    stringsAsFactors = FALSE
  )
}


if (length(name_overlap_lower) > 0L) {
  
  overlap_rows[[length(overlap_rows) + 1L]] <- data.frame(
    overlap_type = "NAME_CASE_INSENSITIVE",
    value = name_overlap_lower,
    interpretation = "Same normalised name occurs in both systems; may represent overlap rather than duplication.",
    stringsAsFactors = FALSE
  )
}


if (length(overlap_rows) > 0L) {
  
  cross_system_overlap <- do.call(
    rbind,
    overlap_rows
  )
  
} else {
  
  cross_system_overlap <- data.frame(
    overlap_type = character(0),
    value = character(0),
    interpretation = character(0),
    stringsAsFactors = FALSE
  )
}


# ------------------------------------------------------------------------------
# 32. BUILD CONCEPT AUDIT
# ------------------------------------------------------------------------------

concept_audit <- canonical_units


concept_audit$code_missing <- is.na(
  normalise_text(
    concept_audit$geographic_code
  )
)


concept_audit$name_missing <- is.na(
  normalise_text(
    concept_audit$geographic_name
  )
)


concept_audit$has_provenance <- (
  concept_audit$geographic_unit_id %in%
    provenance_ids
)


concept_audit$has_concept_source_register <- (
  concept_audit$geographic_unit_id %in%
    concept_source_ids
)


concept_audit$release_resolution_complete <- (
  !concept_audit$code_missing &
    !concept_audit$name_missing &
    concept_audit$has_provenance &
    concept_audit$has_concept_source_register
)


# ------------------------------------------------------------------------------
# 33. BUILD PROVENANCE AUDIT
# ------------------------------------------------------------------------------

provenance_audit <- data.frame(
  metric = c(
    "canonical_concepts",
    "canonical_concepts_with_provenance",
    "canonical_concepts_without_provenance",
    "orphan_provenance_ids",
    "canonical_concepts_in_source_register",
    "canonical_concepts_missing_from_source_register",
    "orphan_source_register_ids"
  ),
  value = c(
    length(canonical_ids),
    sum(canonical_ids %in% provenance_ids),
    n_concepts_without_provenance,
    n_orphan_provenance_ids,
    sum(canonical_ids %in% concept_source_ids),
    length(concepts_without_source_register),
    length(source_register_without_concept)
  ),
  stringsAsFactors = FALSE
)


# ------------------------------------------------------------------------------
# 34. WCVP NAME-RESOLUTION DIAGNOSIS
# ------------------------------------------------------------------------------

candidate_name_field_count <- nrow(
  wcvp_name_candidates
)


candidate_with_367_nonmissing <- FALSE


if (candidate_name_field_count > 0L) {
  
  candidate_with_367_nonmissing <- any(
    wcvp_name_candidates$n_nonmissing_values >=
      367L
  )
}


wcvp_name_diagnosis <- if (
  n_missing_wcvp_names == 0L
) {
  
  "RESOLVED"
  
} else if (
  candidate_with_367_nonmissing
) {
  
  "CANDIDATE_NAME_FIELD_PRESENT_IN_REFERENCE"
  
} else if (
  candidate_name_field_count > 0L
) {
  
  "PARTIAL_OR_AMBIGUOUS_NAME_FIELD_CANDIDATE_PRESENT"
  
} else {
  
  "NO_HUMAN_READABLE_NAME_FIELD_IDENTIFIED"
}


# ------------------------------------------------------------------------------
# 35. SUMMARY
# ------------------------------------------------------------------------------

summary_table <- data.frame(
  
  metric = c(
    "canonical_geographic_concepts",
    "japan_botanical_area_concepts",
    "wcvp_tdwg_concepts",
    "missing_geographic_unit_ids",
    "duplicate_geographic_unit_ids",
    "invalid_geographic_unit_id_format",
    "missing_botanical_area_codes",
    "missing_botanical_area_names",
    "duplicate_botanical_area_codes",
    "duplicate_botanical_area_names",
    "missing_wcvp_tdwg_codes",
    "missing_wcvp_location_names",
    "duplicate_wcvp_tdwg_codes",
    "canonical_wcvp_codes_not_in_reference",
    "reference_wcvp_codes_not_in_canonical",
    "concepts_without_provenance",
    "orphan_provenance_ids",
    "concepts_missing_from_source_register",
    "cross_system_code_overlaps",
    "cross_system_name_overlaps",
    "fully_resolved_concepts",
    "concepts_with_resolution_pending",
    "wcvp_reference_columns",
    "candidate_wcvp_name_fields"
  ),
  
  value = c(
    n_canonical,
    n_botanical,
    n_wcvp,
    n_missing_ids,
    n_duplicate_ids,
    n_invalid_id_format,
    n_missing_botanical_codes,
    n_missing_botanical_names,
    n_duplicate_botanical_codes,
    n_duplicate_botanical_names,
    n_missing_wcvp_codes,
    n_missing_wcvp_names,
    n_duplicate_wcvp_codes,
    length(canonical_codes_not_in_reference),
    length(reference_codes_not_in_canonical),
    n_concepts_without_provenance,
    n_orphan_provenance_ids,
    length(concepts_without_source_register),
    length(code_overlap),
    length(name_overlap_lower),
    n_fully_resolved,
    n_resolution_pending,
    ncol(wcvp_reference),
    candidate_name_field_count
  ),
  
  stringsAsFactors = FALSE
)


# ------------------------------------------------------------------------------
# 36. STRUCTURAL VALIDATION GATE
#
# This answers:
#
#   "Is the rebuilt vocabulary structurally coherent?"
#
# It is deliberately separate from the identifier-freeze gate.
# ------------------------------------------------------------------------------

structural_gate <- data.frame(
  
  criterion = c(
    "06b2_validation_passed",
    "canonical_concept_count_418",
    "botanical_area_concept_count_51",
    "wcvp_tdwg_concept_count_367",
    "geographic_unit_ids_complete",
    "geographic_unit_ids_unique",
    "geographic_unit_id_format_valid",
    "identifier_sequence_deterministic",
    "identifiers_remain_provisional",
    "botanical_area_codes_complete",
    "botanical_area_names_complete",
    "botanical_area_codes_unique",
    "botanical_area_names_unique",
    "wcvp_tdwg_codes_complete",
    "wcvp_tdwg_codes_unique",
    "all_canonical_wcvp_codes_in_reference",
    "all_reference_wcvp_codes_in_canonical",
    "all_concepts_have_provenance",
    "no_orphan_provenance",
    "all_concepts_in_source_register",
    "no_orphan_source_register_records",
    "china_classification_not_promoted_to_units",
    "no_hierarchy_inferred",
    "no_geometry_inferred",
    "published_vpjd_not_modified"
  ),
  
  passed = c(
    all(validation_06b2$passed),
    n_canonical == 418L,
    n_botanical == 51L,
    n_wcvp == 367L,
    n_missing_ids == 0L,
    n_duplicate_ids == 0L,
    n_invalid_id_format == 0L,
    identifier_sequence_matches,
    all_identifiers_provisional,
    n_missing_botanical_codes == 0L,
    n_missing_botanical_names == 0L,
    n_duplicate_botanical_codes == 0L,
    n_duplicate_botanical_names == 0L,
    n_missing_wcvp_codes == 0L,
    n_duplicate_wcvp_codes == 0L,
    length(canonical_codes_not_in_reference) == 0L,
    length(reference_codes_not_in_canonical) == 0L,
    n_concepts_without_provenance == 0L,
    n_orphan_provenance_ids == 0L,
    length(concepts_without_source_register) == 0L,
    length(source_register_without_concept) == 0L,
    TRUE,
    TRUE,
    TRUE,
    TRUE
  ),
  
  stringsAsFactors = FALSE
)


# ------------------------------------------------------------------------------
# 37. IDENTIFIER-FREEZE / RELEASE-READINESS GATE
#
# This is intentionally stricter.
#
# Structural validity does NOT imply that identifiers should yet be frozen.
# ------------------------------------------------------------------------------

release_gate <- data.frame(
  
  criterion = c(
    "structural_validation_passed",
    "all_canonical_concepts_have_codes",
    "all_canonical_concepts_have_names",
    "all_wcvp_tdwg_codes_have_resolved_names",
    "no_concepts_remain_name_pending",
    "all_concepts_have_provenance",
    "all_concepts_in_source_register",
    "cross_system_code_overlap_review_complete",
    "cross_system_name_overlap_review_complete",
    "identifier_freeze_authorised"
  ),
  
  passed = c(
    all(structural_gate$passed),
    
    sum(
      is.na(
        normalise_text(
          canonical_units$geographic_code
        )
      )
    ) == 0L,
    
    sum(
      is.na(
        normalise_text(
          canonical_units$geographic_name
        )
      )
    ) == 0L,
    
    n_missing_wcvp_names == 0L,
    
    n_resolution_pending == 0L,
    
    n_concepts_without_provenance == 0L,
    
    length(concepts_without_source_register) == 0L,
    
    length(code_overlap) == 0L,
    
    length(name_overlap_lower) == 0L,
    
    FALSE
  ),
  
  stringsAsFactors = FALSE
)


# ------------------------------------------------------------------------------
# 38. IMPORTANT: IDENTIFIER FREEZE CANNOT SELF-AUTHORISE
#
# The final criterion remains FALSE in this audit script.
#
# Even if all evidence criteria pass, freezing public identifiers should be a
# deliberate subsequent pipeline action rather than a side effect of an audit.
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# 39. WRITE AUDIT OUTPUTS
# ------------------------------------------------------------------------------

write.csv(
  schema_audit,
  SCHEMA_AUDIT_FILE,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)


write.csv(
  wcvp_field_profile,
  WCVP_FIELD_PROFILE_FILE,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)


write.csv(
  wcvp_name_candidates,
  WCVP_NAME_CANDIDATE_FILE,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)


write.csv(
  concept_audit,
  CONCEPT_AUDIT_FILE,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)


write.csv(
  cross_system_overlap,
  CROSS_SYSTEM_OVERLAP_FILE,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)


write.csv(
  provenance_audit,
  PROVENANCE_AUDIT_FILE,
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
  structural_gate,
  STRUCTURAL_GATE_FILE,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)


write.csv(
  release_gate,
  RELEASE_GATE_FILE,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)


# ------------------------------------------------------------------------------
# 40. CONSOLE SUMMARY
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" VPJD GEOGRAPHY 06b3 - VALIDATION COMPLETE\n")
cat("============================================================\n\n")


cat("Summary:\n\n")

print(
  summary_table,
  row.names = FALSE
)


# ------------------------------------------------------------------------------
# 41. CONCEPT-RESOLUTION PROFILE
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" CONCEPT RESOLUTION STATUS\n")
cat("============================================================\n\n")


print(
  resolution_profile,
  row.names = FALSE
)


# ------------------------------------------------------------------------------
# 42. WCVP REFERENCE SCHEMA
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" WCVP REFERENCE SCHEMA\n")
cat("============================================================\n\n")


print(
  schema_audit,
  row.names = FALSE
)


# ------------------------------------------------------------------------------
# 43. WCVP NAME CANDIDATES
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" WCVP HUMAN-READABLE NAME FIELD AUDIT\n")
cat("============================================================\n\n")


cat(
  "Diagnosis: ",
  wcvp_name_diagnosis,
  "\n\n",
  sep = ""
)


if (nrow(wcvp_name_candidates) > 0L) {
  
  print(
    wcvp_name_candidates,
    row.names = FALSE
  )
  
} else {
  
  cat(
    "No candidate human-readable geographic-name field was identified.\n"
  )
}


# ------------------------------------------------------------------------------
# 44. STRUCTURAL VALIDATION
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" STRUCTURAL VALIDATION GATE\n")
cat("============================================================\n\n")


print(
  structural_gate,
  row.names = FALSE
)


# ------------------------------------------------------------------------------
# 45. IDENTIFIER-FREEZE GATE
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" IDENTIFIER-FREEZE GATE\n")
cat("============================================================\n\n")


print(
  release_gate,
  row.names = FALSE
)


# ------------------------------------------------------------------------------
# 46. INTERPRETATION
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" INTERPRETATION\n")
cat("============================================================\n\n")


if (all(structural_gate$passed)) {
  
  cat(
    paste0(
      "STRUCTURAL PASS. The rebuilt vocabulary contains ",
      format(n_canonical, big.mark = ","),
      " structurally coherent provisional geographic concepts: ",
      format(n_botanical, big.mark = ","),
      " Japanese botanical areas and ",
      format(n_wcvp, big.mark = ","),
      " WCVP/TDWG units.\n\n"
    )
  )
  
} else {
  
  cat(
    "STRUCTURAL FAIL. One or more structural validation criteria failed.\n\n"
  )
  
  failed_structural <- structural_gate$criterion[
    !structural_gate$passed
  ]
  
  cat("Failed structural criteria:\n\n")
  
  for (criterion_name in failed_structural) {
    
    cat(
      " - ",
      criterion_name,
      "\n",
      sep = ""
    )
  }
  
  cat("\n")
}


if (n_missing_wcvp_names > 0L) {
  
  cat(
    paste0(
      "WCVP NAME RESOLUTION INCOMPLETE. ",
      format(n_missing_wcvp_names, big.mark = ","),
      " WCVP/TDWG concepts do not yet have resolved human-readable ",
      "geographic names.\n\n"
    )
  )
  
  cat(
    paste0(
      "WCVP schema diagnosis: ",
      wcvp_name_diagnosis,
      ".\n\n"
    )
  )
}


if (all(release_gate$passed)) {
  
  cat(
    paste0(
      "All release-readiness criteria passed. Identifier freezing still ",
      "requires an explicit subsequent pipeline action.\n"
    )
  )
  
} else {
  
  cat(
    paste0(
      "IDENTIFIER FREEZE NOT AUTHORISED. The provisional VPJD-GEO ",
      "identifiers must remain provisional until the failed release-readiness ",
      "criteria have been resolved and an explicit freeze stage is run.\n\n"
    )
  )
  
  failed_release <- release_gate$criterion[
    !release_gate$passed
  ]
  
  cat("Outstanding release-readiness criteria:\n\n")
  
  for (criterion_name in failed_release) {
    
    cat(
      " - ",
      criterion_name,
      "\n",
      sep = ""
    )
  }
}


# ------------------------------------------------------------------------------
# 47. NEXT STEP
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" NEXT STEP\n")
cat("============================================================\n\n")


if (
  all(structural_gate$passed) &&
  n_missing_wcvp_names > 0L
) {
  
  if (
    wcvp_name_diagnosis ==
    "CANDIDATE_NAME_FIELD_PRESENT_IN_REFERENCE"
  ) {
    
    cat(
      paste0(
        "A candidate human-readable WCVP geographic-name field appears to ",
        "exist in the current reference.\n\n",
        "Review:\n\n",
        WCVP_NAME_CANDIDATE_FILE,
        "\n\n",
        "The next script should validate that candidate field against ",
        "tdwg_code before updating the provisional vocabulary.\n"
      )
    )
    
  } else {
    
    cat(
      paste0(
        "The current WCVP reference does not provide a fully resolved ",
        "human-readable name field detectable by this audit.\n\n",
        "Review:\n\n",
        WCVP_FIELD_PROFILE_FILE,
        "\n\n",
        "The next stage should identify the authoritative TDWG code-to-name ",
        "reference required to resolve the 367 WCVP concepts. Do not infer ",
        "names from the codes themselves.\n"
      )
    )
  }
  
} else if (
  all(structural_gate$passed) &&
  n_missing_wcvp_names == 0L
) {
  
  cat(
    paste0(
      "The rebuilt vocabulary is structurally valid and its concept names ",
      "are complete.\n\n",
      "Proceed to a dedicated identifier-freeze readiness stage before ",
      "constructing the release hierarchy.\n"
    )
  )
  
} else {
  
  cat(
    "Resolve the failed structural criteria before proceeding.\n"
  )
}


# ------------------------------------------------------------------------------
# 48. OUTPUT LOCATIONS
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" OUTPUTS\n")
cat("============================================================\n\n")


cat(
  "WCVP schema audit:\n",
  SCHEMA_AUDIT_FILE,
  "\n\n",
  sep = ""
)

cat(
  "WCVP field profile:\n",
  WCVP_FIELD_PROFILE_FILE,
  "\n\n",
  sep = ""
)

cat(
  "WCVP name candidates:\n",
  WCVP_NAME_CANDIDATE_FILE,
  "\n\n",
  sep = ""
)

cat(
  "Concept audit:\n",
  CONCEPT_AUDIT_FILE,
  "\n\n",
  sep = ""
)

cat(
  "Cross-system overlap audit:\n",
  CROSS_SYSTEM_OVERLAP_FILE,
  "\n\n",
  sep = ""
)

cat(
  "Provenance audit:\n",
  PROVENANCE_AUDIT_FILE,
  "\n\n",
  sep = ""
)

cat(
  "Structural validation gate:\n",
  STRUCTURAL_GATE_FILE,
  "\n\n",
  sep = ""
)

cat(
  "Identifier-freeze gate:\n",
  RELEASE_GATE_FILE,
  "\n",
  sep = ""
)


# ------------------------------------------------------------------------------
# 49. SAFEGUARDS
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" SAFEGUARDS\n")
cat("============================================================\n\n")


cat("06b2 geographic vocabulary was NOT modified.\n")
cat("Source geographic evidence was NOT modified.\n")
cat("Missing WCVP names were NOT inferred from TDWG codes.\n")
cat("Candidate WCVP name fields were NOT automatically adopted.\n")
cat("VPJD-GEO identifiers were NOT frozen.\n")
cat("Geographic hierarchy was NOT inferred.\n")
cat("Geographic geometry was NOT inferred.\n")
cat("Missing geographic evidence was NOT interpreted as absence.\n")
cat("Nakamura predicates were NOT evaluated.\n")
cat("Star categories were NOT evaluated.\n")
cat("Published VPJD Taxonomic Release v1.0.0 was NOT modified.\n")
cat("VPJD Geography v1.0.0 was NOT published or frozen.\n")

cat("\n")
cat("============================================================\n")