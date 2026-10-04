# ==============================================================================
# VPJD GEOGRAPHY MODULE
# geography_06b1_validate_candidate_geographic_fields.R
#
# PURPOSE
# -------
# Convert the diagnostic candidate-field audit produced by:
#
#   geography_06b0_audit_candidate_geographic_fields.R
#
# into an explicit, validated field-disposition register for construction of
# the canonical geographic-unit vocabulary for:
#
#   Vascular Plants of Japan Database (VPJD)
#   Geography Release v1.0.0
#
# POSITION IN PIPELINE
# --------------------
#
#   06a  Audit geographic evidence estate
#    |
#    v
#   06b  Discover candidate geographic vocabulary
#    |
#    v
#   06b0 Audit candidate fields
#    |
#    v
#   06b1 VALIDATE CANDIDATE FIELD ROLES        <-- THIS SCRIPT
#    |
#    v
#   06b2 Rebuild canonical geographic concepts
#    |
#    v
#   06c  Geographic hierarchy / provenance / standards
#    |
#    v
#   VPJD Geography v1.0.0
#
# BACKGROUND
# ----------
# 06b0 identified 11 candidate geographic fields:
#
#   RETAIN:
#
#     1. china_province_level_reference.csv
#          province_level_division
#
#     2. vpjd_botanical_area_taxon_summary.csv
#          botanical_area_id
#
#     3. vpjd_botanical_area_taxon_summary.csv
#          botanical_area_name
#
#     4. vpjd_taxon_botanical_area_distribution.csv
#          botanical_area_id
#
#     5. vpjd_taxon_botanical_area_distribution.csv
#          botanical_area_name
#
#     6. vpjd_wcvp_location_reference.csv
#          tdwg_code
#
#   EXCLUDE:
#
#     7. vpjd_taxon_botanical_area_distribution.csv
#          n_occurrence_records
#
#     8. vpjd_taxon_botanical_area_distribution.csv
#          n_unique_occurrences
#
#     9. vpjd_wcvp_taxon_distribution_summary.csv
#          recreated_distribution_rows
#
#    10. vpjd_botanical_area_taxon_summary.csv
#          n_occurrence_records
#
#    11. vpjd_botanical_area_taxon_summary.csv
#          n_unique_occurrences
#
# The five excluded fields generated all 672 unresolved provisional units
# created by 06b.
#
# OBJECTIVES
# ----------
#  1. Read the 06b0 candidate-field audit.
#  2. Validate that the expected 11 fields are present.
#  3. Explicitly approve the six geographic fields.
#  4. Explicitly exclude the five measure/count fields.
#  5. Assign semantic roles to approved fields:
#
#       GEOGRAPHIC_NAME
#       GEOGRAPHIC_CODE
#       GEOGRAPHIC_CLASSIFICATION
#
#  6. Identify fields that represent the same geographic concept.
#  7. Explicitly pair botanical_area_id with botanical_area_name.
#  8. Mark tdwg_code as a code requiring resolution against the WCVP
#     location reference.
#  9. Mark province_level_division as a geographic classification field.
# 10. Build a machine-readable validated field register for 06b2.
# 11. Confirm that all 672 unresolved 06b units are explained exclusively
#     by excluded fields.
#
# IMPORTANT
# ---------
# This script DOES NOT:
#
#   - rebuild the geographic vocabulary;
#   - assign new VPJD-GEO identifiers;
#   - freeze existing VPJD-GEO identifiers;
#   - modify 06b outputs;
#   - modify source evidence;
#   - infer geographic hierarchy;
#   - infer equivalence between unrelated geographic names;
#   - infer geometry;
#   - calculate Nakamura predicates;
#   - calculate Star categories;
#   - modify VPJD Taxonomic Release v1.0.0.
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

AUDIT_06B_ROOT <- file.path(
  PROJECT_ROOT,
  "audit",
  "geography",
  "release_v1.0.0",
  "06b_canonical_geographic_units"
)

AUDIT_06B0_ROOT <- file.path(
  PROJECT_ROOT,
  "audit",
  "geography",
  "release_v1.0.0",
  "06b0_candidate_field_audit"
)

DERIVED_06B_ROOT <- file.path(
  DATA_ROOT,
  "derived",
  "geography",
  "release_v1.0.0",
  "geographic_units"
)

AUDIT_ROOT <- file.path(
  PROJECT_ROOT,
  "audit",
  "geography",
  "release_v1.0.0",
  "06b1_validated_candidate_fields"
)

DERIVED_ROOT <- file.path(
  DATA_ROOT,
  "derived",
  "geography",
  "release_v1.0.0",
  "field_validation"
)

dir.create(
  AUDIT_ROOT,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  DERIVED_ROOT,
  recursive = TRUE,
  showWarnings = FALSE
)


# ------------------------------------------------------------------------------
# 02. REQUIRED INPUT FILES
# ------------------------------------------------------------------------------

FIELD_PROFILE_FILE <- file.path(
  AUDIT_06B0_ROOT,
  "geography_06b0_candidate_field_profile.csv"
)

FALSE_POSITIVE_FILE <- file.path(
  AUDIT_06B0_ROOT,
  "geography_06b0_probable_false_positive_fields.csv"
)

GEOGRAPHIC_FIELD_FILE <- file.path(
  AUDIT_06B0_ROOT,
  "geography_06b0_probable_geographic_fields.csv"
)

REVIEW_FIELD_FILE <- file.path(
  AUDIT_06B0_ROOT,
  "geography_06b0_fields_requiring_review.csv"
)

VALIDATION_06B0_FILE <- file.path(
  AUDIT_06B0_ROOT,
  "geography_06b0_validation_gate.csv"
)

SOURCE_CROSSWALK_FILE <- file.path(
  DERIVED_06B_ROOT,
  "vpjd_geographic_unit_source_crosswalk.csv"
)

CANONICAL_UNIT_FILE <- file.path(
  DERIVED_06B_ROOT,
  "vpjd_geographic_units.csv"
)


# ------------------------------------------------------------------------------
# 03. OUTPUT FILES
# ------------------------------------------------------------------------------

VALIDATED_FIELD_REGISTER_FILE <- file.path(
  DERIVED_ROOT,
  "vpjd_validated_geographic_field_register.csv"
)

APPROVED_FIELD_FILE <- file.path(
  DERIVED_ROOT,
  "vpjd_approved_geographic_fields.csv"
)

EXCLUDED_FIELD_FILE <- file.path(
  DERIVED_ROOT,
  "vpjd_excluded_non_geographic_fields.csv"
)

CONCEPT_PAIR_FILE <- file.path(
  DERIVED_ROOT,
  "vpjd_geographic_field_concept_pairs.csv"
)

EXCLUDED_UNIT_AUDIT_FILE <- file.path(
  AUDIT_ROOT,
  "geography_06b1_excluded_unit_audit.csv"
)

SUMMARY_FILE <- file.path(
  AUDIT_ROOT,
  "geography_06b1_summary.csv"
)

VALIDATION_FILE <- file.path(
  AUDIT_ROOT,
  "geography_06b1_validation_gate.csv"
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
# 06. HELPER: BUILD FIELD KEY
# ------------------------------------------------------------------------------

build_field_key <- function(source_file, field_name) {
  
  paste(
    normalise_path(source_file),
    as.character(field_name),
    sep = "::FIELD::"
  )
}


# ------------------------------------------------------------------------------
# 07. HELPER: SAFE UNIQUE NON-MISSING
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
# 08. VALIDATE REQUIRED INPUT FILES
# ------------------------------------------------------------------------------

required_inputs <- c(
  FIELD_PROFILE_FILE,
  FALSE_POSITIVE_FILE,
  GEOGRAPHIC_FIELD_FILE,
  REVIEW_FIELD_FILE,
  VALIDATION_06B0_FILE,
  SOURCE_CROSSWALK_FILE,
  CANONICAL_UNIT_FILE
)

missing_inputs <- required_inputs[
  !file.exists(required_inputs)
]

if (length(missing_inputs) > 0L) {
  
  stop(
    paste0(
      "Required 06b0/06b input file(s) not found:\n",
      paste(
        missing_inputs,
        collapse = "\n"
      )
    )
  )
}


# ------------------------------------------------------------------------------
# 09. LOAD INPUTS
# ------------------------------------------------------------------------------

field_profile <- read.csv(
  FIELD_PROFILE_FILE,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

false_positive_fields <- read.csv(
  FALSE_POSITIVE_FILE,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

geographic_fields <- read.csv(
  GEOGRAPHIC_FIELD_FILE,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

review_fields <- read.csv(
  REVIEW_FIELD_FILE,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

validation_06b0 <- read.csv(
  VALIDATION_06B0_FILE,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

source_crosswalk <- read.csv(
  SOURCE_CROSSWALK_FILE,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

canonical_units <- read.csv(
  CANONICAL_UNIT_FILE,
  stringsAsFactors = FALSE,
  check.names = FALSE
)


# ------------------------------------------------------------------------------
# 10. VALIDATE 06b0 GATE
# ------------------------------------------------------------------------------

if (
  !"passed" %in%
  names(validation_06b0)
) {
  
  stop(
    "06b0 validation table does not contain field 'passed'."
  )
}


validation_06b0$passed <- as.logical(
  validation_06b0$passed
)


if (
  any(is.na(validation_06b0$passed)) ||
  !all(validation_06b0$passed)
) {
  
  stop(
    paste0(
      "06b0 validation gate did not pass completely.\n",
      "Do not validate candidate geographic fields."
    )
  )
}


# ------------------------------------------------------------------------------
# 11. VALIDATE REQUIRED FIELD-PROFILE COLUMNS
# ------------------------------------------------------------------------------

required_profile_fields <- c(
  "source_file",
  "file_name",
  "product_class",
  "field_name",
  "inferred_unit_type",
  "n_raw_unit_records",
  "n_distinct_source_values",
  "n_canonical_units_generated",
  "n_unresolved_canonical_units",
  "recommended_disposition",
  "disposition_reason"
)

missing_profile_fields <- setdiff(
  required_profile_fields,
  names(field_profile)
)

if (length(missing_profile_fields) > 0L) {
  
  stop(
    paste0(
      "06b0 field profile lacks required field(s):\n",
      paste(
        missing_profile_fields,
        collapse = "\n"
      )
    )
  )
}


required_crosswalk_fields <- c(
  "geographic_unit_id",
  "source_file",
  "source_field",
  "source_value",
  "inferred_unit_type"
)

missing_crosswalk_fields <- setdiff(
  required_crosswalk_fields,
  names(source_crosswalk)
)

if (length(missing_crosswalk_fields) > 0L) {
  
  stop(
    paste0(
      "06b source crosswalk lacks required field(s):\n",
      paste(
        missing_crosswalk_fields,
        collapse = "\n"
      )
    )
  )
}


required_canonical_fields <- c(
  "geographic_unit_id",
  "geographic_unit_name",
  "geographic_unit_type"
)

missing_canonical_fields <- setdiff(
  required_canonical_fields,
  names(canonical_units)
)

if (length(missing_canonical_fields) > 0L) {
  
  stop(
    paste0(
      "06b canonical unit table lacks required field(s):\n",
      paste(
        missing_canonical_fields,
        collapse = "\n"
      )
    )
  )
}


# ------------------------------------------------------------------------------
# 12. NORMALISE PATHS AND BUILD FIELD KEYS
# ------------------------------------------------------------------------------

field_profile$source_file <- normalise_path(
  field_profile$source_file
)

source_crosswalk$source_file <- normalise_path(
  source_crosswalk$source_file
)


field_profile$field_key <- build_field_key(
  field_profile$source_file,
  field_profile$field_name
)

source_crosswalk$field_key <- build_field_key(
  source_crosswalk$source_file,
  source_crosswalk$source_field
)


# ------------------------------------------------------------------------------
# 13. START
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" VPJD GEOGRAPHY 06b1 - VALIDATE CANDIDATE GEOGRAPHIC FIELDS\n")
cat("============================================================\n\n")


cat(
  "Candidate fields from 06b0: ",
  nrow(field_profile),
  "\n",
  sep = ""
)

cat(
  "06b0 probable geographic fields: ",
  nrow(geographic_fields),
  "\n",
  sep = ""
)

cat(
  "06b0 probable false-positive fields: ",
  nrow(false_positive_fields),
  "\n",
  sep = ""
)

cat(
  "06b0 fields requiring review: ",
  nrow(review_fields),
  "\n\n",
  sep = ""
)


# ------------------------------------------------------------------------------
# 14. DEFINE EXPLICIT VALIDATION RULES
#
# IMPORTANT:
#
# These are explicit field-level decisions based on the completed 06b0 audit.
#
# No field is approved merely because its name "looks geographic".
# ------------------------------------------------------------------------------

validation_rules <- data.frame(
  
  file_name = c(
    "china_province_level_reference.csv",
    
    "vpjd_botanical_area_taxon_summary.csv",
    "vpjd_botanical_area_taxon_summary.csv",
    
    "vpjd_taxon_botanical_area_distribution.csv",
    "vpjd_taxon_botanical_area_distribution.csv",
    
    "vpjd_wcvp_location_reference.csv",
    
    "vpjd_taxon_botanical_area_distribution.csv",
    "vpjd_taxon_botanical_area_distribution.csv",
    
    "vpjd_wcvp_taxon_distribution_summary.csv",
    
    "vpjd_botanical_area_taxon_summary.csv",
    "vpjd_botanical_area_taxon_summary.csv"
  ),
  
  field_name = c(
    "province_level_division",
    
    "botanical_area_id",
    "botanical_area_name",
    
    "botanical_area_id",
    "botanical_area_name",
    
    "tdwg_code",
    
    "n_occurrence_records",
    "n_unique_occurrences",
    
    "recreated_distribution_rows",
    
    "n_occurrence_records",
    "n_unique_occurrences"
  ),
  
  validation_decision = c(
    "APPROVE",
    
    "APPROVE",
    "APPROVE",
    
    "APPROVE",
    "APPROVE",
    
    "APPROVE",
    
    "EXCLUDE",
    "EXCLUDE",
    
    "EXCLUDE",
    
    "EXCLUDE",
    "EXCLUDE"
  ),
  
  field_role = c(
    "GEOGRAPHIC_CLASSIFICATION",
    
    "GEOGRAPHIC_CODE",
    "GEOGRAPHIC_NAME",
    
    "GEOGRAPHIC_CODE",
    "GEOGRAPHIC_NAME",
    
    "GEOGRAPHIC_CODE",
    
    "MEASURE_NOT_GEOGRAPHY",
    "MEASURE_NOT_GEOGRAPHY",
    
    "MEASURE_NOT_GEOGRAPHY",
    
    "MEASURE_NOT_GEOGRAPHY",
    "MEASURE_NOT_GEOGRAPHY"
  ),
  
  canonical_concept_role = c(
    "CHINA_PROVINCE_LEVEL_CLASSIFICATION",
    
    "JAPAN_BOTANICAL_AREA_CODE",
    "JAPAN_BOTANICAL_AREA_NAME",
    
    "JAPAN_BOTANICAL_AREA_CODE",
    "JAPAN_BOTANICAL_AREA_NAME",
    
    "WCVP_TDWG_CODE",
    
    "NONE",
    "NONE",
    
    "NONE",
    
    "NONE",
    "NONE"
  ),
  
  concept_group = c(
    "CHINA_PROVINCE_LEVEL",
    
    "JAPAN_BOTANICAL_AREA",
    "JAPAN_BOTANICAL_AREA",
    
    "JAPAN_BOTANICAL_AREA",
    "JAPAN_BOTANICAL_AREA",
    
    "WCVP_TDWG_LOCATION",
    
    "NONE",
    "NONE",
    
    "NONE",
    
    "NONE",
    "NONE"
  ),
  
  pairing_role = c(
    "STANDALONE_CLASSIFICATION",
    
    "CODE",
    "NAME",
    
    "CODE",
    "NAME",
    
    "CODE_REQUIRES_REFERENCE_RESOLUTION",
    
    "NONE",
    "NONE",
    
    "NONE",
    
    "NONE",
    "NONE"
  ),
  
  validation_basis = c(
    "06b0 retained as recognised geographic classification field",
    
    "06b0 retained as recognised geographic code field",
    "06b0 retained as recognised geographic name field",
    
    "06b0 retained as recognised geographic code field",
    "06b0 retained as recognised geographic name field",
    
    "06b0 retained as recognised WCVP/TDWG geographic code field",
    
    "06b0 identified as numeric occurrence-count field",
    "06b0 identified as numeric occurrence-count field",
    
    "06b0 identified as numeric reconstructed-row count field",
    
    "06b0 identified as numeric occurrence-count field",
    "06b0 identified as numeric occurrence-count field"
  ),
  
  stringsAsFactors = FALSE
)


# ------------------------------------------------------------------------------
# 15. VALIDATE THAT RULES MATCH THE 06b0 FIELD ESTATE EXACTLY
# ------------------------------------------------------------------------------

profile_rule_key <- paste(
  field_profile$file_name,
  field_profile$field_name,
  sep = "::"
)

validation_rule_key <- paste(
  validation_rules$file_name,
  validation_rules$field_name,
  sep = "::"
)


duplicate_profile_keys <- unique(
  profile_rule_key[
    duplicated(profile_rule_key)
  ]
)

duplicate_rule_keys <- unique(
  validation_rule_key[
    duplicated(validation_rule_key)
  ]
)


if (length(duplicate_profile_keys) > 0L) {
  
  stop(
    paste0(
      "Duplicate candidate field key(s) detected in 06b0 field profile:\n",
      paste(
        duplicate_profile_keys,
        collapse = "\n"
      )
    )
  )
}


if (length(duplicate_rule_keys) > 0L) {
  
  stop(
    paste0(
      "Duplicate explicit validation rule key(s) detected:\n",
      paste(
        duplicate_rule_keys,
        collapse = "\n"
      )
    )
  )
}


fields_without_rule <- setdiff(
  profile_rule_key,
  validation_rule_key
)

rules_without_field <- setdiff(
  validation_rule_key,
  profile_rule_key
)


if (length(fields_without_rule) > 0L) {
  
  stop(
    paste0(
      "06b0 candidate field(s) lack an explicit 06b1 validation rule:\n",
      paste(
        fields_without_rule,
        collapse = "\n"
      )
    )
  )
}


if (length(rules_without_field) > 0L) {
  
  stop(
    paste0(
      "06b1 validation rule(s) do not correspond to a 06b0 candidate field:\n",
      paste(
        rules_without_field,
        collapse = "\n"
      )
    )
  )
}


# ------------------------------------------------------------------------------
# 16. ATTACH EXPLICIT VALIDATION RULES
# ------------------------------------------------------------------------------

field_profile$rule_key <- profile_rule_key

validation_rules$rule_key <- validation_rule_key


rule_lookup <- validation_rules[
  ,
  c(
    "rule_key",
    "validation_decision",
    "field_role",
    "canonical_concept_role",
    "concept_group",
    "pairing_role",
    "validation_basis"
  ),
  drop = FALSE
]


validated_field_register <- merge(
  field_profile,
  rule_lookup,
  by = "rule_key",
  all.x = TRUE,
  sort = FALSE
)


# Restore original 06b0 field order.
validated_field_register$original_order <- match(
  validated_field_register$rule_key,
  profile_rule_key
)

validated_field_register <- validated_field_register[
  order(
    validated_field_register$original_order
  ),
  ,
  drop = FALSE
]

validated_field_register$original_order <- NULL

rownames(validated_field_register) <- NULL


# ------------------------------------------------------------------------------
# 17. VERIFY 06b0 DISPOSITION AGREES WITH EXPLICIT DECISION
# ------------------------------------------------------------------------------

validated_field_register$disposition_agreement <- FALSE


approve_selector <- (
  validated_field_register$validation_decision ==
    "APPROVE"
)

approve_selector[is.na(approve_selector)] <- FALSE


exclude_selector <- (
  validated_field_register$validation_decision ==
    "EXCLUDE"
)

exclude_selector[is.na(exclude_selector)] <- FALSE


validated_field_register$disposition_agreement[
  approve_selector
] <- (
  validated_field_register$recommended_disposition[
    approve_selector
  ] == "RETAIN"
)


validated_field_register$disposition_agreement[
  exclude_selector
] <- (
  validated_field_register$recommended_disposition[
    exclude_selector
  ] == "EXCLUDE_PROBABLE_NON_GEOGRAPHIC"
)


validated_field_register$disposition_agreement[
  is.na(
    validated_field_register$disposition_agreement
  )
] <- FALSE


# ------------------------------------------------------------------------------
# 18. ADD FIELD-LEVEL RELEASE STATUS
# ------------------------------------------------------------------------------

validated_field_register$field_validation_status <- ifelse(
  validated_field_register$validation_decision == "APPROVE",
  "VALIDATED_GEOGRAPHIC_FIELD",
  "VALIDATED_EXCLUSION"
)


validated_field_register$eligible_for_06b2 <- (
  validated_field_register$validation_decision ==
    "APPROVE"
)


# ------------------------------------------------------------------------------
# 19. BUILD APPROVED / EXCLUDED TABLES
# ------------------------------------------------------------------------------

approved_fields <- validated_field_register[
  validated_field_register$validation_decision == "APPROVE",
  ,
  drop = FALSE
]


excluded_fields <- validated_field_register[
  validated_field_register$validation_decision == "EXCLUDE",
  ,
  drop = FALSE
]


rownames(approved_fields) <- NULL
rownames(excluded_fields) <- NULL


# ------------------------------------------------------------------------------
# 20. VALIDATE EXPECTED COUNTS
# ------------------------------------------------------------------------------

expected_candidate_fields <- 11L
expected_approved_fields <- 6L
expected_excluded_fields <- 5L
expected_review_fields <- 0L
expected_spurious_units <- 672L


# ------------------------------------------------------------------------------
# 21. BUILD CONCEPT PAIR REGISTER
#
# IMPORTANT:
#
# This register defines how approved source fields should be interpreted by
# 06b2. It does NOT itself merge or rebuild geographic concepts.
# ------------------------------------------------------------------------------

concept_pairs <- data.frame(
  
  concept_group = c(
    "JAPAN_BOTANICAL_AREA",
    "JAPAN_BOTANICAL_AREA",
    "WCVP_TDWG_LOCATION",
    "CHINA_PROVINCE_LEVEL"
  ),
  
  source_product = c(
    "vpjd_botanical_area_taxon_summary.csv",
    "vpjd_taxon_botanical_area_distribution.csv",
    "vpjd_wcvp_location_reference.csv",
    "china_province_level_reference.csv"
  ),
  
  code_field = c(
    "botanical_area_id",
    "botanical_area_id",
    "tdwg_code",
    NA_character_
  ),
  
  name_field = c(
    "botanical_area_name",
    "botanical_area_name",
    NA_character_,
    NA_character_
  ),
  
  classification_field = c(
    NA_character_,
    NA_character_,
    NA_character_,
    "province_level_division"
  ),
  
  expected_concept_count = c(
    51L,
    51L,
    367L,
    NA_integer_
  ),
  
  concept_resolution_instruction = c(
    "Pair botanical_area_id with botanical_area_name row-wise; duplicate representations across source products must resolve to the same botanical-area concepts.",
    
    "Pair botanical_area_id with botanical_area_name row-wise; duplicate representations across source products must resolve to the same botanical-area concepts.",
    
    "Resolve tdwg_code against the WCVP location reference before constructing canonical geographic concepts; do not treat the code as an opaque final geographic name.",
    
    "Retain province_level_division as a geographic classification attribute; do not interpret the classification label itself as a province name."
  ),
  
  stringsAsFactors = FALSE
)


# ------------------------------------------------------------------------------
# 22. VALIDATE BOTANICAL-AREA PAIRING AVAILABILITY
# ------------------------------------------------------------------------------

botanical_summary_fields <- approved_fields[
  approved_fields$file_name ==
    "vpjd_botanical_area_taxon_summary.csv",
  ,
  drop = FALSE
]


botanical_distribution_fields <- approved_fields[
  approved_fields$file_name ==
    "vpjd_taxon_botanical_area_distribution.csv",
  ,
  drop = FALSE
]


summary_has_botanical_id <- (
  "botanical_area_id" %in%
    botanical_summary_fields$field_name
)

summary_has_botanical_name <- (
  "botanical_area_name" %in%
    botanical_summary_fields$field_name
)

distribution_has_botanical_id <- (
  "botanical_area_id" %in%
    botanical_distribution_fields$field_name
)

distribution_has_botanical_name <- (
  "botanical_area_name" %in%
    botanical_distribution_fields$field_name
)


botanical_pairing_available <- (
  summary_has_botanical_id &&
    summary_has_botanical_name &&
    distribution_has_botanical_id &&
    distribution_has_botanical_name
)


# ------------------------------------------------------------------------------
# 23. VALIDATE TDWG FIELD AVAILABILITY
# ------------------------------------------------------------------------------

tdwg_selector <- (
  approved_fields$file_name ==
    "vpjd_wcvp_location_reference.csv" &
    approved_fields$field_name ==
    "tdwg_code"
)

tdwg_selector[is.na(tdwg_selector)] <- FALSE


tdwg_field_available <- (
  sum(tdwg_selector) == 1L
)


# ------------------------------------------------------------------------------
# 24. VALIDATE CHINA CLASSIFICATION FIELD AVAILABILITY
# ------------------------------------------------------------------------------

china_selector <- (
  approved_fields$file_name ==
    "china_province_level_reference.csv" &
    approved_fields$field_name ==
    "province_level_division"
)

china_selector[is.na(china_selector)] <- FALSE


china_classification_available <- (
  sum(china_selector) == 1L
)


# ------------------------------------------------------------------------------
# 25. IDENTIFY ALL 06b UNRESOLVED UNITS
# ------------------------------------------------------------------------------

unresolved_selector <- (
  canonical_units$geographic_unit_type ==
    "UNRESOLVED"
)

unresolved_selector[is.na(unresolved_selector)] <- FALSE


unresolved_units <- canonical_units[
  unresolved_selector,
  ,
  drop = FALSE
]


unresolved_ids <- unique_nonmissing(
  unresolved_units$geographic_unit_id
)


# ------------------------------------------------------------------------------
# 26. IDENTIFY UNIT IDS GENERATED BY EXCLUDED FIELDS
# ------------------------------------------------------------------------------

excluded_field_keys <- build_field_key(
  excluded_fields$source_file,
  excluded_fields$field_name
)


excluded_crosswalk_selector <- (
  source_crosswalk$field_key %in%
    excluded_field_keys
)

excluded_crosswalk_selector[
  is.na(excluded_crosswalk_selector)
] <- FALSE


excluded_crosswalk <- source_crosswalk[
  excluded_crosswalk_selector,
  ,
  drop = FALSE
]


excluded_unit_ids <- unique_nonmissing(
  excluded_crosswalk$geographic_unit_id
)


# ------------------------------------------------------------------------------
# 27. CONFIRM EXCLUDED FIELDS EXPLAIN ALL UNRESOLVED UNITS
# ------------------------------------------------------------------------------

unresolved_not_excluded <- setdiff(
  unresolved_ids,
  excluded_unit_ids
)

excluded_not_unresolved <- setdiff(
  excluded_unit_ids,
  unresolved_ids
)


all_unresolved_explained_by_excluded_fields <- (
  length(unresolved_not_excluded) == 0L
)


no_resolved_units_generated_exclusively_as_excluded <- (
  length(excluded_not_unresolved) == 0L
)


# ------------------------------------------------------------------------------
# 28. BUILD EXCLUDED UNIT AUDIT
# ------------------------------------------------------------------------------

if (length(excluded_unit_ids) > 0L) {
  
  excluded_unit_audit <- canonical_units[
    canonical_units$geographic_unit_id %in%
      excluded_unit_ids,
    ,
    drop = FALSE
  ]
  
  excluded_unit_audit$exclusion_reason <- (
    "GENERATED_FROM_VALIDATED_NON_GEOGRAPHIC_FIELD"
  )
  
  excluded_unit_audit$eligible_for_06b2 <- FALSE
  
} else {
  
  excluded_unit_audit <- data.frame(
    geographic_unit_id = character(0),
    geographic_unit_name = character(0),
    geographic_unit_type = character(0),
    exclusion_reason = character(0),
    eligible_for_06b2 = logical(0),
    stringsAsFactors = FALSE
  )
}


# ------------------------------------------------------------------------------
# 29. CHECK APPROVED FIELDS DO NOT GENERATE UNRESOLVED UNITS
# ------------------------------------------------------------------------------

approved_field_keys <- build_field_key(
  approved_fields$source_file,
  approved_fields$field_name
)


approved_crosswalk_selector <- (
  source_crosswalk$field_key %in%
    approved_field_keys
)

approved_crosswalk_selector[
  is.na(approved_crosswalk_selector)
] <- FALSE


approved_crosswalk <- source_crosswalk[
  approved_crosswalk_selector,
  ,
  drop = FALSE
]


approved_unit_ids <- unique_nonmissing(
  approved_crosswalk$geographic_unit_id
)


approved_unresolved_ids <- intersect(
  approved_unit_ids,
  unresolved_ids
)


approved_fields_generate_no_unresolved_units <- (
  length(approved_unresolved_ids) == 0L
)


# ------------------------------------------------------------------------------
# 30. VALIDATE FIELD ROLE VALUES
# ------------------------------------------------------------------------------

allowed_field_roles <- c(
  "GEOGRAPHIC_NAME",
  "GEOGRAPHIC_CODE",
  "GEOGRAPHIC_CLASSIFICATION",
  "MEASURE_NOT_GEOGRAPHY"
)


field_roles_valid <- all(
  validated_field_register$field_role %in%
    allowed_field_roles
)


allowed_decisions <- c(
  "APPROVE",
  "EXCLUDE"
)


decisions_valid <- all(
  validated_field_register$validation_decision %in%
    allowed_decisions
)


# ------------------------------------------------------------------------------
# 31. VALIDATE THAT NO REVIEW FIELDS REMAIN
# ------------------------------------------------------------------------------

no_review_fields_remain <- (
  nrow(review_fields) == 0L
)


# ------------------------------------------------------------------------------
# 32. VALIDATE 06b0 / 06b1 AGREEMENT
# ------------------------------------------------------------------------------

all_dispositions_agree <- all(
  validated_field_register$disposition_agreement
)


# ------------------------------------------------------------------------------
# 33. SUMMARY TABLE
# ------------------------------------------------------------------------------

summary_table <- data.frame(
  
  metric = c(
    "candidate_fields_validated",
    "approved_geographic_fields",
    "excluded_non_geographic_fields",
    "fields_requiring_review",
    "approved_geographic_code_fields",
    "approved_geographic_name_fields",
    "approved_geographic_classification_fields",
    "excluded_measure_fields",
    "botanical_area_source_products",
    "botanical_area_expected_concepts",
    "wcvp_tdwg_codes_expected",
    "unresolved_06b_units",
    "units_generated_by_excluded_fields",
    "unresolved_units_not_explained_by_exclusion",
    "approved_field_unresolved_units"
  ),
  
  value = c(
    nrow(
      validated_field_register
    ),
    
    nrow(
      approved_fields
    ),
    
    nrow(
      excluded_fields
    ),
    
    nrow(
      review_fields
    ),
    
    sum(
      approved_fields$field_role ==
        "GEOGRAPHIC_CODE"
    ),
    
    sum(
      approved_fields$field_role ==
        "GEOGRAPHIC_NAME"
    ),
    
    sum(
      approved_fields$field_role ==
        "GEOGRAPHIC_CLASSIFICATION"
    ),
    
    sum(
      excluded_fields$field_role ==
        "MEASURE_NOT_GEOGRAPHY"
    ),
    
    2L,
    
    51L,
    
    367L,
    
    length(
      unresolved_ids
    ),
    
    length(
      excluded_unit_ids
    ),
    
    length(
      unresolved_not_excluded
    ),
    
    length(
      approved_unresolved_ids
    )
  ),
  
  stringsAsFactors = FALSE
)


# ------------------------------------------------------------------------------
# 34. VALIDATION GATE
# ------------------------------------------------------------------------------

validation_gate <- data.frame(
  
  criterion = c(
    "06b0_validation_passed",
    "candidate_field_count_11",
    "approved_field_count_6",
    "excluded_field_count_5",
    "no_review_fields_remain",
    "all_candidate_fields_have_explicit_rule",
    "no_extra_validation_rules",
    "validation_decisions_valid",
    "field_roles_valid",
    "all_06b0_dispositions_agree_with_06b1",
    "botanical_area_id_name_pair_available",
    "tdwg_code_field_available",
    "china_province_classification_field_available",
    "unresolved_06b_unit_count_672",
    "excluded_fields_account_for_672_units",
    "all_unresolved_units_explained_by_excluded_fields",
    "excluded_fields_generate_no_resolved_geographic_units",
    "approved_fields_generate_no_unresolved_units",
    "no_vpjd_geo_ids_frozen",
    "06b_outputs_not_modified",
    "published_vpjd_not_modified"
  ),
  
  passed = c(
    all(
      validation_06b0$passed
    ),
    
    nrow(
      validated_field_register
    ) == expected_candidate_fields,
    
    nrow(
      approved_fields
    ) == expected_approved_fields,
    
    nrow(
      excluded_fields
    ) == expected_excluded_fields,
    
    no_review_fields_remain &&
      nrow(review_fields) ==
      expected_review_fields,
    
    length(
      fields_without_rule
    ) == 0L,
    
    length(
      rules_without_field
    ) == 0L,
    
    decisions_valid,
    
    field_roles_valid,
    
    all_dispositions_agree,
    
    botanical_pairing_available,
    
    tdwg_field_available,
    
    china_classification_available,
    
    length(
      unresolved_ids
    ) == expected_spurious_units,
    
    length(
      excluded_unit_ids
    ) == expected_spurious_units,
    
    all_unresolved_explained_by_excluded_fields,
    
    no_resolved_units_generated_exclusively_as_excluded,
    
    approved_fields_generate_no_unresolved_units,
    
    TRUE,
    
    TRUE,
    
    TRUE
  ),
  
  stringsAsFactors = FALSE
)


# ------------------------------------------------------------------------------
# 35. WRITE OUTPUTS
# ------------------------------------------------------------------------------

write.csv(
  validated_field_register,
  VALIDATED_FIELD_REGISTER_FILE,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)


write.csv(
  approved_fields,
  APPROVED_FIELD_FILE,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)


write.csv(
  excluded_fields,
  EXCLUDED_FIELD_FILE,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)


write.csv(
  concept_pairs,
  CONCEPT_PAIR_FILE,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)


write.csv(
  excluded_unit_audit,
  EXCLUDED_UNIT_AUDIT_FILE,
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
# 36. CONSOLE SUMMARY
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" VPJD GEOGRAPHY 06b1 - FIELD VALIDATION COMPLETE\n")
cat("============================================================\n\n")


cat("Summary:\n\n")

print(
  summary_table,
  row.names = FALSE
)


# ------------------------------------------------------------------------------
# 37. APPROVED GEOGRAPHIC FIELDS
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" APPROVED GEOGRAPHIC FIELDS\n")
cat("============================================================\n\n")


print(
  approved_fields[
    ,
    c(
      "file_name",
      "field_name",
      "inferred_unit_type",
      "field_role",
      "canonical_concept_role",
      "concept_group",
      "pairing_role"
    ),
    drop = FALSE
  ],
  row.names = FALSE
)


# ------------------------------------------------------------------------------
# 38. EXCLUDED NON-GEOGRAPHIC FIELDS
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" EXCLUDED NON-GEOGRAPHIC FIELDS\n")
cat("============================================================\n\n")


print(
  excluded_fields[
    ,
    c(
      "file_name",
      "field_name",
      "field_role",
      "n_canonical_units_generated",
      "n_unresolved_canonical_units",
      "validation_basis"
    ),
    drop = FALSE
  ],
  row.names = FALSE
)


# ------------------------------------------------------------------------------
# 39. CONCEPT-PAIR REGISTER
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" GEOGRAPHIC CONCEPT-PAIR REGISTER\n")
cat("============================================================\n\n")


print(
  concept_pairs,
  row.names = FALSE
)


# ------------------------------------------------------------------------------
# 40. EXCLUDED UNIT ACCOUNTING
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" EXCLUDED UNIT ACCOUNTING\n")
cat("============================================================\n\n")


cat(
  "06b unresolved units: ",
  length(unresolved_ids),
  "\n",
  sep = ""
)

cat(
  "Units generated by excluded fields: ",
  length(excluded_unit_ids),
  "\n",
  sep = ""
)

cat(
  "Unresolved units not explained by exclusions: ",
  length(unresolved_not_excluded),
  "\n",
  sep = ""
)

cat(
  "Resolved units incorrectly captured by exclusions: ",
  length(excluded_not_unresolved),
  "\n",
  sep = ""
)

cat(
  "Unresolved units generated by approved fields: ",
  length(approved_unresolved_ids),
  "\n",
  sep = ""
)


# ------------------------------------------------------------------------------
# 41. VALIDATION GATE
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
# 42. INTERPRETATION
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
      "PASS. The 11 candidate fields discovered by 06b have now been ",
      "explicitly validated.\n\n"
    )
  )
  
  cat(
    paste0(
      "Six fields are approved as geographic evidence and five fields are ",
      "excluded as non-geographic measures.\n\n"
    )
  )
  
  cat(
    paste0(
      "The five excluded fields account for all ",
      format(
        length(unresolved_ids),
        big.mark = ","
      ),
      " unresolved provisional units generated by 06b. No unresolved ",
      "unit is generated by an approved geographic field.\n\n"
    )
  )
  
  cat(
    paste0(
      "The Japanese botanical-area ID and name fields are explicitly ",
      "identified as paired representations of the same geographic ",
      "concepts. They must therefore be consolidated in 06b2 rather than ",
      "treated as separate geographic units.\n\n"
    )
  )
  
  cat(
    paste0(
      "The WCVP TDWG code field is approved as geographic evidence but ",
      "must be resolved against the WCVP location reference before final ",
      "canonical concepts are constructed.\n\n"
    )
  )
  
  cat(
    paste0(
      "The China province-level field is retained as a geographic ",
      "classification attribute. Its value must not automatically be ",
      "interpreted as a province name.\n"
    )
  )
  
} else {
  
  cat(
    "FAIL. One or more 06b1 validation criteria failed.\n\n"
  )
  
  failed_criteria <- validation_gate$criterion[
    !validation_gate$passed
  ]
  
  cat(
    "Failed criteria:\n\n"
  )
  
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
# 43. NEXT STEP
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
      "The candidate-field estate is now validated.\n\n",
      "Proceed to:\n\n",
      "  geography_06b2_rebuild_canonical_geographic_units.R\n\n",
      "06b2 should rebuild the geographic vocabulary from APPROVED fields ",
      "only. It should:\n\n",
      "  1. discard the 672 provisional units generated by excluded fields;\n",
      "  2. consolidate botanical_area_id + botanical_area_name into 51 ",
      "Japanese botanical-area concepts;\n",
      "  3. reconcile duplicate botanical-area representations across the ",
      "two source products;\n",
      "  4. resolve WCVP tdwg_code values against the WCVP location ",
      "reference;\n",
      "  5. treat province_level_division as classification metadata rather ",
      "than automatically creating a province concept from the label;\n",
      "  6. rebuild provisional VPJD-GEO identifiers only after concept ",
      "resolution;\n",
      "  7. preserve complete source provenance.\n\n",
      "Do not freeze VPJD-GEO identifiers until the rebuilt vocabulary has ",
      "passed subsequent validation.\n"
    )
  )
  
} else {
  
  cat(
    "Do not proceed to 06b2 until all 06b1 validation criteria pass.\n"
  )
}


# ------------------------------------------------------------------------------
# 44. SAFEGUARDS
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" SAFEGUARDS\n")
cat("============================================================\n\n")


cat(
  "06b source outputs were NOT modified.\n"
)

cat(
  "06b0 audit outputs were NOT modified.\n"
)

cat(
  "Source geographic evidence was NOT modified.\n"
)

cat(
  "Existing VPJD-GEO identifiers were NOT frozen.\n"
)

cat(
  "Existing VPJD-GEO identifiers were NOT reassigned.\n"
)

cat(
  "No canonical geographic vocabulary was rebuilt in this script.\n"
)

cat(
  "Botanical-area IDs and names were NOT treated as separate final concepts.\n"
)

cat(
  "TDWG codes were NOT treated as final geographic names.\n"
)

cat(
  "China province-level classification was NOT interpreted as a province name.\n"
)

cat(
  "Missing geographic evidence was NOT interpreted as absence.\n"
)

cat(
  "Geographic hierarchy was NOT inferred.\n"
)

cat(
  "Geographic geometry was NOT inferred.\n"
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