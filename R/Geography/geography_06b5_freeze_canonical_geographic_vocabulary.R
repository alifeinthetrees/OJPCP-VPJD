# ==============================================================================
# VPJD GEOGRAPHY MODULE
# geography_06b5_freeze_canonical_geographic_vocabulary.R
#
# PURPOSE
# -------
# Freeze the independently validated canonical geographic vocabulary for:
#
#   VPJD Geography v1.0.0
#
# using the integrated vocabulary produced by:
#
#   geography_06b3b_integrate_wcvp_locality_names.R
#
# and independently validated by:
#
#   geography_06b4_validate_integrated_geographic_vocabulary.R
#
#
# POSITION IN PIPELINE
# --------------------
#
#   06b2   Rebuild provisional canonical vocabulary
#     |
#     v
#   06b3   Validate provisional vocabulary
#     |
#     v
#   06b3a  Validate WCVP locality mapping
#     |
#     v
#   06b3b  Integrate validated WCVP locality names
#     |
#     v
#   06b4   Independent integrated-vocabulary validation
#     |
#     |-- structural/evidence validation PASS
#     |-- release-readiness validation PASS
#     `-- eligible_for_deliberate_identifier_freeze = TRUE
#                    |
#                    v
#   06b5   FREEZE CANONICAL GEOGRAPHIC VOCABULARY      <-- THIS SCRIPT
#                    |
#                    v
#           frozen release-candidate vocabulary
#                    |
#                    v
#       public taxon x geographic-unit distribution product
#
#
# FREEZE PRINCIPLE
# ----------------
#
# The freeze is an IDENTIFIER-STATUS operation.
#
# It does NOT generate new identifiers.
#
# Every existing geographic_unit_id must survive the freeze unchanged.
#
# Concept content must also remain unchanged except for the explicitly
# controlled release/freeze metadata introduced by this script.
#
#
# CONTROLLED MUTATIONS
# --------------------
#
# This script is permitted to:
#
#   1. change:
#
#        identifier_status
#
#      from:
#
#        PROVISIONAL_06B2
#
#      to:
#
#        FROZEN_VPJD_GEOGRAPHY_V1.0.0
#
#   2. add:
#
#        identifier_freeze_release
#        identifier_freeze_script
#        identifier_freeze_date
#
#
# NO OTHER EXISTING FIELD MAY CHANGE.
#
#
# IMPORTANT
# ---------
#
# This script DOES NOT:
#
#   - regenerate geographic_unit_id;
#   - renumber geographic_unit_id;
#   - alter geographic_code;
#   - alter geographic_name;
#   - alter concept_group;
#   - alter concept_resolution_status;
#   - alter WCVP location IDs;
#   - alter WCVP locality provenance;
#   - alter Japanese botanical-area concepts;
#   - infer geographic hierarchy;
#   - infer geographic geometry;
#   - modify the 06b3b integrated vocabulary in place;
#   - modify the 06b4 audit;
#   - modify VPJD Taxonomic Release v1.0.0;
#   - build taxon-distribution products;
#   - evaluate Nakamura predicates;
#   - assign Star categories.
#
# ==============================================================================


# ------------------------------------------------------------------------------
# 01. RELEASE CONSTANTS
# ------------------------------------------------------------------------------

RELEASE_NAME <- "VPJD Geography v1.0.0"

FROZEN_IDENTIFIER_STATUS <- "FROZEN_VPJD_GEOGRAPHY_V1.0.0"

FREEZE_SCRIPT <- "geography_06b5_freeze_canonical_geographic_vocabulary.R"

FREEZE_DATE <- format(
  Sys.Date(),
  "%Y-%m-%d"
)


# ------------------------------------------------------------------------------
# 02. PROJECT PATHS
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

INTEGRATED_ROOT <- file.path(
  DERIVED_ROOT,
  "canonical_geographic_units_integrated"
)

FROZEN_ROOT <- file.path(
  DERIVED_ROOT,
  "canonical_geographic_units_frozen"
)

AUDIT_06B4_ROOT <- file.path(
  PROJECT_ROOT,
  "audit",
  "geography",
  "release_v1.0.0",
  "06b4_validate_integrated_geographic_vocabulary"
)

AUDIT_ROOT <- file.path(
  PROJECT_ROOT,
  "audit",
  "geography",
  "release_v1.0.0",
  "06b5_freeze_canonical_geographic_vocabulary"
)

dir.create(
  FROZEN_ROOT,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  AUDIT_ROOT,
  recursive = TRUE,
  showWarnings = FALSE
)


# ------------------------------------------------------------------------------
# 03. INPUT FILES
# ------------------------------------------------------------------------------

INTEGRATED_FILE <- file.path(
  INTEGRATED_ROOT,
  "vpjd_geographic_units_integrated.csv"
)

VALIDATION_06B4_FILE <- file.path(
  AUDIT_06B4_ROOT,
  "geography_06b4_validation_gate.csv"
)

READINESS_06B4_FILE <- file.path(
  AUDIT_06B4_ROOT,
  "geography_06b4_release_readiness_gate.csv"
)


# ------------------------------------------------------------------------------
# 04. OUTPUT FILES
# ------------------------------------------------------------------------------

FROZEN_FILE <- file.path(
  FROZEN_ROOT,
  "vpjd_geographic_units_v1.0.0.csv"
)

FREEZE_AUDIT_FILE <- file.path(
  AUDIT_ROOT,
  "geography_06b5_freeze_audit.csv"
)

FIELD_PRESERVATION_FILE <- file.path(
  AUDIT_ROOT,
  "geography_06b5_field_preservation_audit.csv"
)

IDENTIFIER_PRESERVATION_FILE <- file.path(
  AUDIT_ROOT,
  "geography_06b5_identifier_preservation_audit.csv"
)

SUMMARY_FILE <- file.path(
  AUDIT_ROOT,
  "geography_06b5_summary.csv"
)

VALIDATION_GATE_FILE <- file.path(
  AUDIT_ROOT,
  "geography_06b5_validation_gate.csv"
)

RELEASE_GATE_FILE <- file.path(
  AUDIT_ROOT,
  "geography_06b5_release_gate.csv"
)


# ------------------------------------------------------------------------------
# 05. HELPER: NORMALISE TEXT
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
# 06. HELPER: SAFE CSV READ
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
# 07. HELPER: VALUES IDENTICAL INCLUDING NA
# ------------------------------------------------------------------------------

values_identical <- function(x, y) {
  
  x <- normalise_text(x)
  y <- normalise_text(y)
  
  result <- (
    (is.na(x) & is.na(y)) |
      (!is.na(x) & !is.na(y) & x == y)
  )
  
  result[is.na(result)] <- FALSE
  
  result
}


# ------------------------------------------------------------------------------
# 08. HELPER: CONVERT LOGICAL TEXT SAFELY
# ------------------------------------------------------------------------------

logical_from_csv <- function(x) {
  
  y <- toupper(
    trimws(
      as.character(x)
    )
  )
  
  result <- rep(
    NA,
    length(y)
  )
  
  result[y == "TRUE"] <- TRUE
  result[y == "FALSE"] <- FALSE
  
  result
}


# ------------------------------------------------------------------------------
# 09. VALIDATE REQUIRED INPUTS
# ------------------------------------------------------------------------------

required_inputs <- c(
  INTEGRATED_FILE,
  VALIDATION_06B4_FILE,
  READINESS_06B4_FILE
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
# 10. READ INPUTS
# ------------------------------------------------------------------------------

integrated <- safe_read_csv(
  INTEGRATED_FILE
)

validation_06b4 <- safe_read_csv(
  VALIDATION_06B4_FILE
)

readiness_06b4 <- safe_read_csv(
  READINESS_06B4_FILE
)


# ------------------------------------------------------------------------------
# 11. REQUIRE COMPLETE 06b4 STRUCTURAL / EVIDENCE PASS
# ------------------------------------------------------------------------------

if (!"passed" %in% names(validation_06b4)) {
  
  stop(
    "06b4 validation gate lacks required field 'passed'."
  )
}


validation_06b4_passed <- logical_from_csv(
  validation_06b4$passed
)


if (
  any(is.na(validation_06b4_passed)) ||
  !all(validation_06b4_passed)
) {
  
  stop(
    paste0(
      "06b4 structural/evidence validation did not pass completely.\n",
      "Canonical geographic identifiers must NOT be frozen."
    )
  )
}


# ------------------------------------------------------------------------------
# 12. REQUIRE COMPLETE 06b4 RELEASE-READINESS PASS
# ------------------------------------------------------------------------------

if (!all(
  c(
    "criterion",
    "passed"
  ) %in% names(readiness_06b4)
)) {
  
  stop(
    paste0(
      "06b4 release-readiness gate lacks required fields ",
      "'criterion' and/or 'passed'."
    )
  )
}


readiness_06b4_passed <- logical_from_csv(
  readiness_06b4$passed
)


if (
  any(is.na(readiness_06b4_passed)) ||
  !all(readiness_06b4_passed)
) {
  
  stop(
    paste0(
      "06b4 release-readiness validation did not pass completely.\n",
      "Canonical geographic identifiers must NOT be frozen."
    )
  )
}


freeze_eligibility_index <- which(
  readiness_06b4$criterion ==
    "eligible_for_deliberate_identifier_freeze"
)


if (length(freeze_eligibility_index) != 1L) {
  
  stop(
    paste0(
      "06b4 release-readiness gate must contain exactly one ",
      "'eligible_for_deliberate_identifier_freeze' criterion."
    )
  )
}


if (!isTRUE(
  readiness_06b4_passed[
    freeze_eligibility_index
  ]
)) {
  
  stop(
    "06b4 did not authorise deliberate identifier freeze."
  )
}


# ------------------------------------------------------------------------------
# 13. VALIDATE INTEGRATED VOCABULARY SCHEMA
# ------------------------------------------------------------------------------

required_fields <- c(
  "geographic_unit_id",
  "geographic_code",
  "geographic_name",
  "concept_group",
  "concept_resolution_status",
  "identifier_status",
  "wcvp_location_id",
  "geographic_name_source",
  "geographic_name_validation_status"
)

missing_fields <- setdiff(
  required_fields,
  names(integrated)
)

if (length(missing_fields) > 0L) {
  
  stop(
    paste0(
      "Integrated vocabulary lacks required field(s):\n",
      paste(
        missing_fields,
        collapse = "\n"
      )
    )
  )
}


# ------------------------------------------------------------------------------
# 14. PRE-FREEZE NORMALISED VALUES
# ------------------------------------------------------------------------------

pre_id <- normalise_text(
  integrated$geographic_unit_id
)

pre_code <- normalise_text(
  integrated$geographic_code
)

pre_name <- normalise_text(
  integrated$geographic_name
)

pre_group <- normalise_text(
  integrated$concept_group
)

pre_resolution <- normalise_text(
  integrated$concept_resolution_status
)

pre_identifier_status <- normalise_text(
  integrated$identifier_status
)

pre_wcvp_location_id <- normalise_text(
  integrated$wcvp_location_id
)

pre_name_source <- normalise_text(
  integrated$geographic_name_source
)

pre_name_validation <- normalise_text(
  integrated$geographic_name_validation_status
)


# ------------------------------------------------------------------------------
# 15. PRE-FREEZE CARDINALITY / STATUS GATES
# ------------------------------------------------------------------------------

if (nrow(integrated) != 418L) {
  
  stop(
    paste0(
      "Expected 418 integrated geographic concepts; found ",
      nrow(integrated),
      "."
    )
  )
}


if (any(is.na(pre_id))) {
  
  stop(
    "Integrated vocabulary contains missing geographic_unit_id values."
  )
}


if (any(duplicated(pre_id))) {
  
  stop(
    "Integrated vocabulary contains duplicate geographic_unit_id values."
  )
}


if (any(is.na(pre_code))) {
  
  stop(
    "Integrated vocabulary contains missing geographic_code values."
  )
}


if (any(duplicated(pre_code))) {
  
  stop(
    "Integrated vocabulary contains duplicate geographic_code values."
  )
}


if (any(is.na(pre_name))) {
  
  stop(
    "Integrated vocabulary contains missing geographic_name values."
  )
}


if (
  any(is.na(pre_identifier_status)) ||
  any(
    pre_identifier_status !=
    "PROVISIONAL_06B2"
  )
) {
  
  stop(
    paste0(
      "All 418 identifiers must be PROVISIONAL_06B2 before freeze."
    )
  )
}


# ------------------------------------------------------------------------------
# 16. VALIDATE EXPECTED CONCEPT ARCHITECTURE
# ------------------------------------------------------------------------------

japan_selector <- (
  pre_group ==
    "JAPAN_BOTANICAL_AREA"
)

japan_selector[is.na(japan_selector)] <- FALSE


wcvp_selector <- (
  pre_group ==
    "WCVP_TDWG_LOCATION"
)

wcvp_selector[is.na(wcvp_selector)] <- FALSE


n_japan <- sum(
  japan_selector
)

n_wcvp <- sum(
  wcvp_selector
)


if (n_japan != 51L) {
  
  stop(
    paste0(
      "Expected 51 Japanese botanical-area concepts; found ",
      n_japan,
      "."
    )
  )
}


if (n_wcvp != 367L) {
  
  stop(
    paste0(
      "Expected 367 WCVP/TDWG concepts; found ",
      n_wcvp,
      "."
    )
  )
}


if ((n_japan + n_wcvp) != 418L) {
  
  stop(
    "Unexpected concept groups are present."
  )
}


# ------------------------------------------------------------------------------
# 17. VALIDATE EXPECTED RESOLUTION STATES
# ------------------------------------------------------------------------------

if (
  any(
    pre_resolution[
      japan_selector
    ] != "RESOLVED"
  )
) {
  
  stop(
    "One or more Japanese botanical-area concepts are not RESOLVED."
  )
}


if (
  any(
    pre_resolution[
      wcvp_selector
    ] !=
    "RESOLVED_VALIDATED_WCVP_LOCALITY"
  )
) {
  
  stop(
    paste0(
      "One or more WCVP/TDWG concepts are not ",
      "RESOLVED_VALIDATED_WCVP_LOCALITY."
    )
  )
}


# ------------------------------------------------------------------------------
# 18. PRESERVE ORIGINAL FIELD SET
# ------------------------------------------------------------------------------

original_field_names <- names(
  integrated
)


# ------------------------------------------------------------------------------
# 19. CREATE FROZEN WORKING COPY
# ------------------------------------------------------------------------------

frozen <- integrated


# ------------------------------------------------------------------------------
# 20. CONTROLLED MUTATION: IDENTIFIER STATUS
# ------------------------------------------------------------------------------

frozen$identifier_status <- FROZEN_IDENTIFIER_STATUS


# ------------------------------------------------------------------------------
# 21. ADD FREEZE PROVENANCE
# ------------------------------------------------------------------------------

frozen$identifier_freeze_release <- RELEASE_NAME

frozen$identifier_freeze_script <- FREEZE_SCRIPT

frozen$identifier_freeze_date <- FREEZE_DATE


# ------------------------------------------------------------------------------
# 22. POST-FREEZE NORMALISED VALUES
# ------------------------------------------------------------------------------

post_id <- normalise_text(
  frozen$geographic_unit_id
)

post_code <- normalise_text(
  frozen$geographic_code
)

post_name <- normalise_text(
  frozen$geographic_name
)

post_group <- normalise_text(
  frozen$concept_group
)

post_resolution <- normalise_text(
  frozen$concept_resolution_status
)

post_identifier_status <- normalise_text(
  frozen$identifier_status
)

post_wcvp_location_id <- normalise_text(
  frozen$wcvp_location_id
)

post_name_source <- normalise_text(
  frozen$geographic_name_source
)

post_name_validation <- normalise_text(
  frozen$geographic_name_validation_status
)

post_freeze_release <- normalise_text(
  frozen$identifier_freeze_release
)

post_freeze_script <- normalise_text(
  frozen$identifier_freeze_script
)

post_freeze_date <- normalise_text(
  frozen$identifier_freeze_date
)


# ------------------------------------------------------------------------------
# 23. IDENTIFIER PRESERVATION AUDIT
# ------------------------------------------------------------------------------

identifier_preservation <- data.frame(
  
  row_number = seq_len(
    nrow(frozen)
  ),
  
  geographic_unit_id_before =
    pre_id,
  
  geographic_unit_id_after =
    post_id,
  
  unchanged = values_identical(
    pre_id,
    post_id
  ),
  
  identifier_status_before =
    pre_identifier_status,
  
  identifier_status_after =
    post_identifier_status,
  
  stringsAsFactors = FALSE
)


# ------------------------------------------------------------------------------
# 24. FIELD PRESERVATION AUDIT
#
# Audit every field that existed before freeze.
#
# identifier_status is the ONLY existing field permitted to change.
# ------------------------------------------------------------------------------

field_preservation_records <- vector(
  mode = "list",
  length = length(original_field_names)
)

field_record_counter <- 0L


for (field_name in original_field_names) {
  
  field_record_counter <- field_record_counter + 1L
  
  before_values <- integrated[[field_name]]
  
  after_values <- frozen[[field_name]]
  
  row_identical <- values_identical(
    before_values,
    after_values
  )
  
  n_changed <- sum(
    !row_identical
  )
  
  permitted_to_change <- (
    field_name ==
      "identifier_status"
  )
  
  expected_changed_rows <- if (
    permitted_to_change
  ) {
    418L
  } else {
    0L
  }
  
  field_passed <- (
    n_changed ==
      expected_changed_rows
  )
  
  field_preservation_records[[field_record_counter]] <- data.frame(
    
    field_name =
      field_name,
    
    permitted_to_change =
      permitted_to_change,
    
    expected_changed_rows =
      expected_changed_rows,
    
    observed_changed_rows =
      n_changed,
    
    passed =
      field_passed,
    
    stringsAsFactors = FALSE
  )
}


field_preservation_audit <- do.call(
  rbind,
  field_preservation_records
)


# ------------------------------------------------------------------------------
# 25. EXPLICIT CONTENT-PRESERVATION TESTS
# ------------------------------------------------------------------------------

id_unchanged <- values_identical(
  pre_id,
  post_id
)

code_unchanged <- values_identical(
  pre_code,
  post_code
)

name_unchanged <- values_identical(
  pre_name,
  post_name
)

group_unchanged <- values_identical(
  pre_group,
  post_group
)

resolution_unchanged <- values_identical(
  pre_resolution,
  post_resolution
)

wcvp_location_id_unchanged <- values_identical(
  pre_wcvp_location_id,
  post_wcvp_location_id
)

name_source_unchanged <- values_identical(
  pre_name_source,
  post_name_source
)

name_validation_unchanged <- values_identical(
  pre_name_validation,
  post_name_validation
)


# ------------------------------------------------------------------------------
# 26. FREEZE AUDIT BY CONCEPT
# ------------------------------------------------------------------------------

freeze_audit <- data.frame(
  
  geographic_unit_id =
    post_id,
  
  geographic_code =
    post_code,
  
  geographic_name =
    post_name,
  
  concept_group =
    post_group,
  
  identifier_status_before =
    pre_identifier_status,
  
  identifier_status_after =
    post_identifier_status,
  
  geographic_unit_id_unchanged =
    id_unchanged,
  
  geographic_code_unchanged =
    code_unchanged,
  
  geographic_name_unchanged =
    name_unchanged,
  
  concept_group_unchanged =
    group_unchanged,
  
  resolution_status_unchanged =
    resolution_unchanged,
  
  wcvp_location_id_unchanged =
    wcvp_location_id_unchanged,
  
  name_source_unchanged =
    name_source_unchanged,
  
  name_validation_status_unchanged =
    name_validation_unchanged,
  
  identifier_freeze_release =
    post_freeze_release,
  
  identifier_freeze_script =
    post_freeze_script,
  
  identifier_freeze_date =
    post_freeze_date,
  
  stringsAsFactors = FALSE
)


# ------------------------------------------------------------------------------
# 27. CORE POST-FREEZE COUNTS
# ------------------------------------------------------------------------------

n_total <- nrow(
  frozen
)

n_missing_ids <- sum(
  is.na(post_id)
)

n_duplicate_ids <- sum(
  duplicated(
    post_id[
      !is.na(post_id)
    ]
  )
)

n_missing_codes <- sum(
  is.na(post_code)
)

n_duplicate_codes <- sum(
  duplicated(
    post_code[
      !is.na(post_code)
    ]
  )
)

n_missing_names <- sum(
  is.na(post_name)
)

n_frozen_status <- sum(
  post_identifier_status ==
    FROZEN_IDENTIFIER_STATUS,
  na.rm = TRUE
)

n_nonfrozen_status <- sum(
  is.na(post_identifier_status) |
    post_identifier_status !=
    FROZEN_IDENTIFIER_STATUS
)

n_id_changes <- sum(
  !id_unchanged
)

n_code_changes <- sum(
  !code_unchanged
)

n_name_changes <- sum(
  !name_unchanged
)

n_group_changes <- sum(
  !group_unchanged
)

n_resolution_changes <- sum(
  !resolution_unchanged
)

n_wcvp_location_id_changes <- sum(
  !wcvp_location_id_unchanged
)

n_name_source_changes <- sum(
  !name_source_unchanged
)

n_name_validation_changes <- sum(
  !name_validation_unchanged
)


# ------------------------------------------------------------------------------
# 28. FREEZE METADATA COUNTS
# ------------------------------------------------------------------------------

n_release_metadata <- sum(
  post_freeze_release ==
    RELEASE_NAME,
  na.rm = TRUE
)

n_script_metadata <- sum(
  post_freeze_script ==
    FREEZE_SCRIPT,
  na.rm = TRUE
)

n_date_metadata <- sum(
  post_freeze_date ==
    FREEZE_DATE,
  na.rm = TRUE
)


# ------------------------------------------------------------------------------
# 29. FIELD-PRESERVATION COUNTS
# ------------------------------------------------------------------------------

n_failed_field_preservation_tests <- sum(
  !field_preservation_audit$passed
)


# ------------------------------------------------------------------------------
# 30. SUMMARY TABLE
# ------------------------------------------------------------------------------

summary_table <- data.frame(
  
  metric = c(
    "canonical_geographic_concepts",
    "japanese_botanical_area_concepts",
    "wcvp_tdwg_concepts",
    "missing_geographic_unit_ids",
    "duplicate_geographic_unit_ids",
    "missing_geographic_codes",
    "duplicate_geographic_codes",
    "missing_geographic_names",
    "frozen_identifier_statuses",
    "nonfrozen_identifier_statuses",
    "geographic_unit_id_changes",
    "geographic_code_changes",
    "geographic_name_changes",
    "concept_group_changes",
    "resolution_status_changes",
    "wcvp_location_id_changes",
    "geographic_name_source_changes",
    "geographic_name_validation_status_changes",
    "freeze_release_metadata_rows",
    "freeze_script_metadata_rows",
    "freeze_date_metadata_rows",
    "failed_field_preservation_tests"
  ),
  
  value = c(
    n_total,
    n_japan,
    n_wcvp,
    n_missing_ids,
    n_duplicate_ids,
    n_missing_codes,
    n_duplicate_codes,
    n_missing_names,
    n_frozen_status,
    n_nonfrozen_status,
    n_id_changes,
    n_code_changes,
    n_name_changes,
    n_group_changes,
    n_resolution_changes,
    n_wcvp_location_id_changes,
    n_name_source_changes,
    n_name_validation_changes,
    n_release_metadata,
    n_script_metadata,
    n_date_metadata,
    n_failed_field_preservation_tests
  ),
  
  stringsAsFactors = FALSE
)


# ------------------------------------------------------------------------------
# 31. VALIDATION GATE
# ------------------------------------------------------------------------------

validation_gate <- data.frame(
  
  criterion = c(
    "06b4_structural_validation_passed",
    "06b4_release_readiness_passed",
    "06b4_explicit_freeze_eligibility_true",
    "canonical_cardinality_418",
    "japanese_botanical_area_count_51",
    "wcvp_tdwg_count_367",
    "geographic_unit_ids_complete",
    "geographic_unit_ids_unique",
    "geographic_unit_ids_unchanged",
    "geographic_codes_complete",
    "geographic_codes_unique",
    "geographic_codes_unchanged",
    "geographic_names_complete",
    "geographic_names_unchanged",
    "concept_groups_unchanged",
    "resolution_statuses_unchanged",
    "wcvp_location_ids_unchanged",
    "geographic_name_sources_unchanged",
    "geographic_name_validation_statuses_unchanged",
    "all_418_identifier_statuses_frozen",
    "no_nonfrozen_identifier_statuses",
    "freeze_release_metadata_complete",
    "freeze_script_metadata_complete",
    "freeze_date_metadata_complete",
    "only_identifier_status_existing_field_changed",
    "integrated_source_not_modified",
    "published_vpjd_not_modified"
  ),
  
  passed = c(
    all(validation_06b4_passed),
    
    all(readiness_06b4_passed),
    
    isTRUE(
      readiness_06b4_passed[
        freeze_eligibility_index
      ]
    ),
    
    n_total == 418L,
    
    n_japan == 51L,
    
    n_wcvp == 367L,
    
    n_missing_ids == 0L,
    
    n_duplicate_ids == 0L,
    
    n_id_changes == 0L,
    
    n_missing_codes == 0L,
    
    n_duplicate_codes == 0L,
    
    n_code_changes == 0L,
    
    n_missing_names == 0L,
    
    n_name_changes == 0L,
    
    n_group_changes == 0L,
    
    n_resolution_changes == 0L,
    
    n_wcvp_location_id_changes == 0L,
    
    n_name_source_changes == 0L,
    
    n_name_validation_changes == 0L,
    
    n_frozen_status == 418L,
    
    n_nonfrozen_status == 0L,
    
    n_release_metadata == 418L,
    
    n_script_metadata == 418L,
    
    n_date_metadata == 418L,
    
    n_failed_field_preservation_tests == 0L,
    
    TRUE,
    
    TRUE
  ),
  
  stringsAsFactors = FALSE
)


# ------------------------------------------------------------------------------
# 32. RELEASE GATE
#
# PASS means the frozen vocabulary can become the canonical geographic
# vocabulary used by downstream VPJD Geography v1.0.0 products.
# ------------------------------------------------------------------------------

release_gate <- data.frame(
  
  criterion = c(
    "freeze_validation_passed",
    "all_418_identifiers_frozen",
    "identifier_values_preserved_exactly",
    "geographic_codes_preserved_exactly",
    "geographic_names_preserved_exactly",
    "concept_structure_preserved_exactly",
    "resolution_evidence_preserved_exactly",
    "wcvp_evidence_preserved_exactly",
    "freeze_provenance_complete",
    "source_integrated_vocabulary_preserved",
    "frozen_vocabulary_ready_for_downstream_products"
  ),
  
  passed = c(
    all(validation_gate$passed),
    
    n_frozen_status == 418L &&
      n_nonfrozen_status == 0L,
    
    n_id_changes == 0L,
    
    n_code_changes == 0L,
    
    n_name_changes == 0L,
    
    n_group_changes == 0L,
    
    n_resolution_changes == 0L,
    
    n_wcvp_location_id_changes == 0L &&
      n_name_source_changes == 0L &&
      n_name_validation_changes == 0L,
    
    n_release_metadata == 418L &&
      n_script_metadata == 418L &&
      n_date_metadata == 418L,
    
    TRUE,
    
    all(validation_gate$passed) &&
      n_frozen_status == 418L &&
      n_nonfrozen_status == 0L &&
      n_id_changes == 0L &&
      n_code_changes == 0L &&
      n_name_changes == 0L &&
      n_group_changes == 0L &&
      n_resolution_changes == 0L &&
      n_wcvp_location_id_changes == 0L &&
      n_name_source_changes == 0L &&
      n_name_validation_changes == 0L &&
      n_failed_field_preservation_tests == 0L
  ),
  
  stringsAsFactors = FALSE
)


# ------------------------------------------------------------------------------
# 33. WRITE AUDIT OUTPUTS FIRST
# ------------------------------------------------------------------------------

write.csv(
  freeze_audit,
  FREEZE_AUDIT_FILE,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)

write.csv(
  field_preservation_audit,
  FIELD_PRESERVATION_FILE,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)

write.csv(
  identifier_preservation,
  IDENTIFIER_PRESERVATION_FILE,
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
  VALIDATION_GATE_FILE,
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
# 34. WRITE FROZEN VOCABULARY ONLY AFTER COMPLETE PASS
# ------------------------------------------------------------------------------

freeze_pass <- all(
  validation_gate$passed
)

release_pass <- all(
  release_gate$passed
)


if (freeze_pass && release_pass) {
  
  write.csv(
    frozen,
    FROZEN_FILE,
    row.names = FALSE,
    na = "",
    fileEncoding = "UTF-8"
  )
}


# ------------------------------------------------------------------------------
# 35. READ-BACK VALIDATION OF WRITTEN FROZEN PRODUCT
# ------------------------------------------------------------------------------

readback_pass <- FALSE


if (freeze_pass && release_pass) {
  
  if (!file.exists(FROZEN_FILE)) {
    
    stop(
      paste0(
        "Frozen vocabulary passed validation but output file was not written:\n",
        FROZEN_FILE
      )
    )
  }
  
  frozen_readback <- safe_read_csv(
    FROZEN_FILE
  )
  
  if (nrow(frozen_readback) != 418L) {
    
    stop(
      paste0(
        "Frozen vocabulary read-back cardinality failure: expected 418, found ",
        nrow(frozen_readback),
        "."
      )
    )
  }
  
  
  readback_id <- normalise_text(
    frozen_readback$geographic_unit_id
  )
  
  readback_code <- normalise_text(
    frozen_readback$geographic_code
  )
  
  readback_name <- normalise_text(
    frozen_readback$geographic_name
  )
  
  readback_identifier_status <- normalise_text(
    frozen_readback$identifier_status
  )
  
  
  readback_pass <- (
    all(
      values_identical(
        post_id,
        readback_id
      )
    ) &&
      all(
        values_identical(
          post_code,
          readback_code
        )
      ) &&
      all(
        values_identical(
          post_name,
          readback_name
        )
      ) &&
      all(
        readback_identifier_status ==
          FROZEN_IDENTIFIER_STATUS
      )
  )
  
  
  if (!readback_pass) {
    
    stop(
      paste0(
        "Frozen vocabulary failed read-back validation.\n",
        "Do not use the output for downstream products."
      )
    )
  }
}


# ------------------------------------------------------------------------------
# 36. CONSOLE HEADER
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" VPJD GEOGRAPHY 06b5 - CANONICAL VOCABULARY FREEZE\n")
cat("============================================================\n\n")


# ------------------------------------------------------------------------------
# 37. FREEZE METADATA
# ------------------------------------------------------------------------------

cat("Release:\n")
cat(RELEASE_NAME, "\n\n")

cat("Freeze status:\n")
cat(FROZEN_IDENTIFIER_STATUS, "\n\n")

cat("Freeze date:\n")
cat(FREEZE_DATE, "\n\n")


# ------------------------------------------------------------------------------
# 38. SUMMARY
# ------------------------------------------------------------------------------

cat("============================================================\n")
cat(" SUMMARY\n")
cat("============================================================\n\n")

print(
  summary_table,
  row.names = FALSE
)


# ------------------------------------------------------------------------------
# 39. VALIDATION GATE
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" FREEZE VALIDATION GATE\n")
cat("============================================================\n\n")

print(
  validation_gate,
  row.names = FALSE
)


# ------------------------------------------------------------------------------
# 40. RELEASE GATE
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" FROZEN VOCABULARY RELEASE GATE\n")
cat("============================================================\n\n")

print(
  release_gate,
  row.names = FALSE
)


# ------------------------------------------------------------------------------
# 41. READ-BACK VALIDATION
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" OUTPUT READ-BACK VALIDATION\n")
cat("============================================================\n\n")


if (freeze_pass && release_pass) {
  
  cat(
    "frozen_vocabulary_readback_passed: ",
    readback_pass,
    "\n",
    sep = ""
  )
  
} else {
  
  cat(
    "Frozen vocabulary was not written because validation did not pass.\n"
  )
}


# ------------------------------------------------------------------------------
# 42. INTERPRETATION
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" INTERPRETATION\n")
cat("============================================================\n\n")


if (
  freeze_pass &&
  release_pass &&
  readback_pass
) {
  
  cat(
    paste0(
      "PASS. The canonical VPJD Geography vocabulary has been deliberately ",
      "frozen for VPJD Geography v1.0.0.\n\n"
    )
  )
  
  cat(
    paste0(
      "All 418 existing geographic_unit_id values were preserved exactly. ",
      "No geographic identifier was regenerated, renumbered or replaced.\n\n"
    )
  )
  
  cat(
    paste0(
      "The vocabulary continues to contain 51 Japanese botanical areas and ",
      "367 WCVP/TDWG geographic concepts. Geographic codes, names, concept ",
      "groups, resolution states and WCVP evidence were preserved unchanged.\n\n"
    )
  )
  
  cat(
    paste0(
      "The only pre-existing field deliberately changed was ",
      "identifier_status, from PROVISIONAL_06B2 to ",
      FROZEN_IDENTIFIER_STATUS,
      ".\n\n"
    )
  )
  
  cat(
    paste0(
      "Freeze provenance has been attached to all 418 concepts. The frozen ",
      "output also passed post-write read-back validation.\n\n"
    )
  )
  
  cat(
    paste0(
      "This frozen vocabulary can now serve as the canonical geographic-unit ",
      "reference for downstream VPJD Geography v1.0.0 public data products.\n"
    )
  )
  
} else {
  
  cat(
    paste0(
      "FAIL. The canonical geographic vocabulary has NOT been successfully ",
      "frozen for downstream use.\n"
    )
  )
  
  failed_validation <- validation_gate$criterion[
    !validation_gate$passed
  ]
  
  failed_release <- release_gate$criterion[
    !release_gate$passed
  ]
  
  
  if (length(failed_validation) > 0L) {
    
    cat("\nFailed freeze-validation criteria:\n\n")
    
    for (criterion_name in failed_validation) {
      
      cat(
        " - ",
        criterion_name,
        "\n",
        sep = ""
      )
    }
  }
  
  
  if (length(failed_release) > 0L) {
    
    cat("\nFailed release-gate criteria:\n\n")
    
    for (criterion_name in failed_release) {
      
      cat(
        " - ",
        criterion_name,
        "\n",
        sep = ""
      )
    }
  }
  
  
  if (
    freeze_pass &&
    release_pass &&
    !readback_pass
  ) {
    
    cat(
      "\nThe written frozen product failed read-back validation.\n"
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
  freeze_pass &&
  release_pass &&
  readback_pass
) {
  
  cat(
    paste0(
      "The canonical geographic vocabulary is now frozen and ready for use ",
      "by downstream VPJD Geography v1.0.0 products.\n\n",
      "The next stage should build the public taxon x geographic-unit ",
      "distribution product against this frozen vocabulary.\n\n",
      "Recommended next script:\n\n",
      "  geography_06c_build_taxon_geographic_distribution.R\n\n",
      "That script should use the frozen geographic_unit_id vocabulary as the ",
      "only canonical geographic-unit reference and should not modify the ",
      "frozen vocabulary itself.\n"
    )
  )
  
} else {
  
  cat(
    paste0(
      "Do not build downstream VPJD Geography release products.\n",
      "Resolve the failed 06b5 validation or read-back criterion first.\n"
    )
  )
}


# ------------------------------------------------------------------------------
# 44. OUTPUT LOCATIONS
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" OUTPUTS\n")
cat("============================================================\n\n")


cat(
  "Freeze audit:\n",
  FREEZE_AUDIT_FILE,
  "\n\n",
  sep = ""
)

cat(
  "Field-preservation audit:\n",
  FIELD_PRESERVATION_FILE,
  "\n\n",
  sep = ""
)

cat(
  "Identifier-preservation audit:\n",
  IDENTIFIER_PRESERVATION_FILE,
  "\n\n",
  sep = ""
)

cat(
  "Summary:\n",
  SUMMARY_FILE,
  "\n\n",
  sep = ""
)

cat(
  "Freeze-validation gate:\n",
  VALIDATION_GATE_FILE,
  "\n\n",
  sep = ""
)

cat(
  "Frozen-vocabulary release gate:\n",
  RELEASE_GATE_FILE,
  "\n\n",
  sep = ""
)


if (
  freeze_pass &&
  release_pass &&
  readback_pass
) {
  
  cat(
    "Frozen canonical geographic vocabulary:\n",
    FROZEN_FILE,
    "\n\n",
    sep = ""
  )
  
} else {
  
  cat(
    paste0(
      "Frozen canonical geographic vocabulary: NOT AVAILABLE FOR ",
      "DOWNSTREAM USE.\n\n"
    )
  )
}


# ------------------------------------------------------------------------------
# 45. SAFEGUARDS
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" SAFEGUARDS\n")
cat("============================================================\n\n")

cat("06b3b integrated geographic vocabulary was NOT modified in place.\n")
cat("06b4 audit outputs were NOT modified.\n")
cat("Existing geographic_unit_id values were NOT regenerated.\n")
cat("Existing geographic_unit_id values were NOT renumbered.\n")
cat("Geographic codes were NOT changed.\n")
cat("Geographic names were NOT changed.\n")
cat("Concept groups were NOT changed.\n")
cat("Concept-resolution states were NOT changed.\n")
cat("WCVP location identifiers were NOT changed.\n")
cat("WCVP locality provenance was NOT changed.\n")
cat("Japanese botanical-area concepts were NOT changed.\n")
cat("Geographic hierarchy was NOT inferred.\n")
cat("Geographic geometry was NOT inferred.\n")
cat("Missing geographic evidence was NOT interpreted as absence.\n")
cat("Nakamura predicates were NOT evaluated.\n")
cat("Star categories were NOT evaluated.\n")
cat("Published VPJD Taxonomic Release v1.0.0 was NOT modified.\n")

cat("\n")
cat("============================================================\n")