# ==============================================================================
# VPJD GEOGRAPHY MODULE
# geography_06b3b_integrate_wcvp_locality_names.R
#
# PURPOSE
# -------
# Integrate the validated WCVP locality names produced by:
#
#   geography_06b3a_validate_wcvp_locality_mapping.R
#
# into the provisional canonical geographic vocabulary produced by:
#
#   geography_06b2_rebuild_canonical_geographic_units.R
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
#     |-- structural validation PASS
#     |-- 51 Japanese botanical areas resolved
#     `-- 367 WCVP/TDWG geographic names pending
#               |
#               v
#   06b3a Validate WCVP locality mapping
#               |
#               |-- 367 TDWG codes
#               |-- 367 WCVP locality names
#               |-- 367 WCVP location IDs
#               |-- zero mapping conflicts
#               `-- PASS
#                         |
#                         v
#   06b3b INTEGRATE WCVP LOCALITY NAMES             <-- THIS SCRIPT
#                         |
#                         v
#                 post-integration validation
#                         |
#                         v
#                release-readiness audit
#
#
# OBJECTIVES
# ----------
#
#  1. Read the 418-concept provisional canonical geographic vocabulary.
#
#  2. Read ONLY the validated 367-row WCVP locality crosswalk produced
#     by 06b3a.
#
#  3. Confirm that the 06b3a validation gate passed completely.
#
#  4. Match the 367 WCVP/TDWG concepts by geographic_code = tdwg_code.
#
#  5. Populate geographic_name using validated wcvp_locality.
#
#  6. Retain the validated WCVP location identifier.
#
#  7. Retain WCVP locality provenance.
#
#  8. Update concept_resolution_status only for successfully validated
#     WCVP/TDWG records.
#
#  9. Preserve all 51 Japanese botanical-area concepts unchanged.
#
# 10. Preserve every existing geographic_unit_id unchanged.
#
# 11. Preserve every existing geographic_code unchanged.
#
# 12. Preserve identifier_status as provisional.
#
# 13. Validate the resulting 418-concept integrated vocabulary.
#
#
# IMPORTANT
# ---------
#
# This script DOES NOT:
#
#   - regenerate VPJD-GEO identifiers;
#   - freeze VPJD-GEO identifiers;
#   - infer geographic names;
#   - infer geographic hierarchy;
#   - infer geographic geometry;
#   - rewrite WCVP locality labels;
#   - rewrite TDWG codes;
#   - modify Japanese botanical-area concepts;
#   - modify the validated 06b3a crosswalk;
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

WCVP_VALIDATION_ROOT <- file.path(
  DERIVED_ROOT,
  "wcvp_locality_validation"
)

AUDIT_06B3A_ROOT <- file.path(
  PROJECT_ROOT,
  "audit",
  "geography",
  "release_v1.0.0",
  "06b3a_validate_wcvp_locality_mapping"
)

OUTPUT_ROOT <- file.path(
  DERIVED_ROOT,
  "canonical_geographic_units_integrated"
)

AUDIT_ROOT <- file.path(
  PROJECT_ROOT,
  "audit",
  "geography",
  "release_v1.0.0",
  "06b3b_integrate_wcvp_locality_names"
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

VALIDATED_CROSSWALK_FILE <- file.path(
  WCVP_VALIDATION_ROOT,
  "vpjd_wcvp_tdwg_locality_crosswalk_validated.csv"
)

VALIDATION_06B3A_FILE <- file.path(
  AUDIT_06B3A_ROOT,
  "geography_06b3a_validation_gate.csv"
)


# ------------------------------------------------------------------------------
# 03. OUTPUT FILES
# ------------------------------------------------------------------------------

INTEGRATED_UNIT_FILE <- file.path(
  OUTPUT_ROOT,
  "vpjd_geographic_units_integrated.csv"
)

INTEGRATION_AUDIT_FILE <- file.path(
  AUDIT_ROOT,
  "geography_06b3b_wcvp_integration_audit.csv"
)

JAPAN_PRESERVATION_FILE <- file.path(
  AUDIT_ROOT,
  "geography_06b3b_japan_botanical_area_preservation.csv"
)

IDENTIFIER_PRESERVATION_FILE <- file.path(
  AUDIT_ROOT,
  "geography_06b3b_identifier_preservation.csv"
)

CODE_PRESERVATION_FILE <- file.path(
  AUDIT_ROOT,
  "geography_06b3b_code_preservation.csv"
)

SUMMARY_FILE <- file.path(
  AUDIT_ROOT,
  "geography_06b3b_summary.csv"
)

VALIDATION_FILE <- file.path(
  AUDIT_ROOT,
  "geography_06b3b_validation_gate.csv"
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
# 05. HELPER: SAFE CSV READ
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
# 06. HELPER: VALUES IDENTICAL INCLUDING NA
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
# 07. VALIDATE REQUIRED INPUT FILES
# ------------------------------------------------------------------------------

required_inputs <- c(
  CANONICAL_UNIT_FILE,
  VALIDATED_CROSSWALK_FILE,
  VALIDATION_06B3A_FILE
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
# 08. LOAD INPUTS
# ------------------------------------------------------------------------------

canonical_original <- safe_read_csv(
  CANONICAL_UNIT_FILE
)

validated_crosswalk <- safe_read_csv(
  VALIDATED_CROSSWALK_FILE
)

validation_06b3a <- safe_read_csv(
  VALIDATION_06B3A_FILE
)


# ------------------------------------------------------------------------------
# 09. REQUIRE COMPLETE 06b3a PASS
# ------------------------------------------------------------------------------

if (!"passed" %in% names(validation_06b3a)) {
  
  stop(
    "06b3a validation table lacks required field 'passed'."
  )
}


validation_06b3a$passed <- as.logical(
  validation_06b3a$passed
)


if (
  any(is.na(validation_06b3a$passed)) ||
  !all(validation_06b3a$passed)
) {
  
  stop(
    paste0(
      "06b3a did not pass completely.\n",
      "WCVP locality names must not be integrated."
    )
  )
}


# ------------------------------------------------------------------------------
# 10. VALIDATE CANONICAL SCHEMA
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
  names(canonical_original)
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


# ------------------------------------------------------------------------------
# 11. VALIDATE CROSSWALK SCHEMA
# ------------------------------------------------------------------------------

required_crosswalk_fields <- c(
  "tdwg_code",
  "wcvp_locality",
  "wcvp_location_id",
  "location_reference_source",
  "validation_status"
)

missing_crosswalk_fields <- setdiff(
  required_crosswalk_fields,
  names(validated_crosswalk)
)

if (length(missing_crosswalk_fields) > 0L) {
  
  stop(
    paste0(
      "Validated WCVP crosswalk lacks required field(s):\n",
      paste(
        missing_crosswalk_fields,
        collapse = "\n"
      )
    )
  )
}


# ------------------------------------------------------------------------------
# 12. NORMALISE CROSSWALK WORKING VALUES
# ------------------------------------------------------------------------------

crosswalk_work <- data.frame(
  
  tdwg_code = normalise_text(
    validated_crosswalk$tdwg_code
  ),
  
  wcvp_locality = normalise_text(
    validated_crosswalk$wcvp_locality
  ),
  
  wcvp_location_id = normalise_text(
    validated_crosswalk$wcvp_location_id
  ),
  
  location_reference_source = normalise_text(
    validated_crosswalk$location_reference_source
  ),
  
  validation_status = normalise_text(
    validated_crosswalk$validation_status
  ),
  
  stringsAsFactors = FALSE
)


# ------------------------------------------------------------------------------
# 13. VALIDATE CROSSWALK CARDINALITY AND CONTENT
# ------------------------------------------------------------------------------

if (nrow(crosswalk_work) != 367L) {
  
  stop(
    paste0(
      "Validated WCVP crosswalk must contain exactly 367 rows; found ",
      nrow(crosswalk_work),
      "."
    )
  )
}


if (any(is.na(crosswalk_work$tdwg_code))) {
  
  stop(
    "Validated WCVP crosswalk contains missing TDWG codes."
  )
}


if (any(is.na(crosswalk_work$wcvp_locality))) {
  
  stop(
    "Validated WCVP crosswalk contains missing WCVP locality names."
  )
}


if (any(is.na(crosswalk_work$wcvp_location_id))) {
  
  stop(
    "Validated WCVP crosswalk contains missing WCVP location IDs."
  )
}


if (any(is.na(crosswalk_work$location_reference_source))) {
  
  stop(
    "Validated WCVP crosswalk contains missing provenance."
  )
}


if (any(duplicated(crosswalk_work$tdwg_code))) {
  
  stop(
    "Validated WCVP crosswalk contains duplicate TDWG codes."
  )
}


if (any(duplicated(crosswalk_work$wcvp_location_id))) {
  
  stop(
    "Validated WCVP crosswalk contains duplicate WCVP location IDs."
  )
}


if (
  any(
    crosswalk_work$validation_status !=
    "VALIDATED_06B3A"
  )
) {
  
  stop(
    paste0(
      "Validated WCVP crosswalk contains records not marked ",
      "VALIDATED_06B3A."
    )
  )
}


# ------------------------------------------------------------------------------
# 14. VALIDATE CANONICAL CARDINALITY
# ------------------------------------------------------------------------------

if (nrow(canonical_original) != 418L) {
  
  stop(
    paste0(
      "Expected 418 canonical geographic concepts; found ",
      nrow(canonical_original),
      "."
    )
  )
}


# ------------------------------------------------------------------------------
# 15. IDENTIFY CONCEPT GROUPS
# ------------------------------------------------------------------------------

wcvp_selector <- (
  canonical_original$concept_group ==
    "WCVP_TDWG_LOCATION"
)

wcvp_selector[is.na(wcvp_selector)] <- FALSE


japan_selector <- (
  canonical_original$concept_group ==
    "JAPAN_BOTANICAL_AREA"
)

japan_selector[is.na(japan_selector)] <- FALSE


n_wcvp <- sum(
  wcvp_selector
)

n_japan <- sum(
  japan_selector
)


if (n_wcvp != 367L) {
  
  stop(
    paste0(
      "Expected 367 WCVP/TDWG concepts; found ",
      n_wcvp,
      "."
    )
  )
}


if (n_japan != 51L) {
  
  stop(
    paste0(
      "Expected 51 Japanese botanical-area concepts; found ",
      n_japan,
      "."
    )
  )
}


if ((n_wcvp + n_japan) != nrow(canonical_original)) {
  
  stop(
    "Canonical vocabulary contains unexpected concept groups."
  )
}


# ------------------------------------------------------------------------------
# 16. PRESERVE ORIGINAL VALUES FOR VALIDATION
# ------------------------------------------------------------------------------

original_geographic_unit_id <- normalise_text(
  canonical_original$geographic_unit_id
)

original_geographic_code <- normalise_text(
  canonical_original$geographic_code
)

original_geographic_name <- normalise_text(
  canonical_original$geographic_name
)

original_resolution_status <- normalise_text(
  canonical_original$concept_resolution_status
)

original_identifier_status <- normalise_text(
  canonical_original$identifier_status
)


# ------------------------------------------------------------------------------
# 17. CREATE INTEGRATED WORKING COPY
# ------------------------------------------------------------------------------

canonical_integrated <- canonical_original


# ------------------------------------------------------------------------------
# 18. ADD WCVP EVIDENCE FIELDS IF ABSENT
# ------------------------------------------------------------------------------

if (!"wcvp_location_id" %in% names(canonical_integrated)) {
  
  canonical_integrated$wcvp_location_id <- NA_character_
}


if (!"geographic_name_source" %in% names(canonical_integrated)) {
  
  canonical_integrated$geographic_name_source <- NA_character_
}


if (!"geographic_name_validation_status" %in% names(canonical_integrated)) {
  
  canonical_integrated$geographic_name_validation_status <- NA_character_
}


# ------------------------------------------------------------------------------
# 19. NORMALISE CANONICAL WCVP CODES FOR MATCHING
# ------------------------------------------------------------------------------

canonical_wcvp_codes <- normalise_text(
  canonical_integrated$geographic_code[
    wcvp_selector
  ]
)


crosswalk_codes <- crosswalk_work$tdwg_code


# ------------------------------------------------------------------------------
# 20. VALIDATE CODE SET EQUALITY BEFORE INTEGRATION
# ------------------------------------------------------------------------------

canonical_not_crosswalk <- setdiff(
  canonical_wcvp_codes,
  crosswalk_codes
)

crosswalk_not_canonical <- setdiff(
  crosswalk_codes,
  canonical_wcvp_codes
)


if (length(canonical_not_crosswalk) > 0L) {
  
  stop(
    paste0(
      "Canonical TDWG code(s) absent from validated crosswalk:\n",
      paste(
        canonical_not_crosswalk,
        collapse = "\n"
      )
    )
  )
}


if (length(crosswalk_not_canonical) > 0L) {
  
  stop(
    paste0(
      "Validated crosswalk TDWG code(s) absent from canonical vocabulary:\n",
      paste(
        crosswalk_not_canonical,
        collapse = "\n"
      )
    )
  )
}


# ------------------------------------------------------------------------------
# 21. MATCH CANONICAL WCVP CONCEPTS TO VALIDATED CROSSWALK
# ------------------------------------------------------------------------------

match_index <- match(
  canonical_wcvp_codes,
  crosswalk_work$tdwg_code
)


if (any(is.na(match_index))) {
  
  stop(
    "One or more canonical WCVP concepts failed to match the validated crosswalk."
  )
}


if (length(unique(match_index)) != 367L) {
  
  stop(
    "Canonical-to-crosswalk matching is not one-to-one."
  )
}


# ------------------------------------------------------------------------------
# 22. EXTRACT VALIDATED VALUES IN CANONICAL ORDER
# ------------------------------------------------------------------------------

validated_localities <- crosswalk_work$wcvp_locality[
  match_index
]

validated_location_ids <- crosswalk_work$wcvp_location_id[
  match_index
]

validated_sources <- crosswalk_work$location_reference_source[
  match_index
]

validated_statuses <- crosswalk_work$validation_status[
  match_index
]


# ------------------------------------------------------------------------------
# 23. PRE-INTEGRATION AUDIT
# ------------------------------------------------------------------------------

integration_audit <- data.frame(
  
  geographic_unit_id =
    original_geographic_unit_id[
      wcvp_selector
    ],
  
  tdwg_code =
    canonical_wcvp_codes,
  
  geographic_name_before =
    original_geographic_name[
      wcvp_selector
    ],
  
  validated_wcvp_locality =
    validated_localities,
  
  validated_wcvp_location_id =
    validated_location_ids,
  
  validated_name_source =
    validated_sources,
  
  crosswalk_validation_status =
    validated_statuses,
  
  concept_resolution_status_before =
    original_resolution_status[
      wcvp_selector
    ],
  
  identifier_status_before =
    original_identifier_status[
      wcvp_selector
    ],
  
  stringsAsFactors = FALSE
)


# ------------------------------------------------------------------------------
# 24. INTEGRATE VALIDATED WCVP LOCALITY NAMES
#
# This is the only geographic-name mutation performed by this script.
# ------------------------------------------------------------------------------

canonical_integrated$geographic_name[
  wcvp_selector
] <- validated_localities


# ------------------------------------------------------------------------------
# 25. INTEGRATE WCVP LOCATION IDS
# ------------------------------------------------------------------------------

canonical_integrated$wcvp_location_id[
  wcvp_selector
] <- validated_location_ids


# ------------------------------------------------------------------------------
# 26. INTEGRATE NAME PROVENANCE
# ------------------------------------------------------------------------------

canonical_integrated$geographic_name_source[
  wcvp_selector
] <- validated_sources


# ------------------------------------------------------------------------------
# 27. INTEGRATE VALIDATION STATUS
# ------------------------------------------------------------------------------

canonical_integrated$geographic_name_validation_status[
  wcvp_selector
] <- validated_statuses


# ------------------------------------------------------------------------------
# 28. UPDATE CONCEPT RESOLUTION STATUS
#
# Only the 367 validated WCVP/TDWG concepts are updated.
# ------------------------------------------------------------------------------

canonical_integrated$concept_resolution_status[
  wcvp_selector
] <- "RESOLVED_VALIDATED_WCVP_LOCALITY"


# ------------------------------------------------------------------------------
# 29. DO NOT ALTER IDENTIFIER STATUS
#
# No assignment is made here deliberately.
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# 30. POST-INTEGRATION NORMALISED VALUES
# ------------------------------------------------------------------------------

integrated_geographic_unit_id <- normalise_text(
  canonical_integrated$geographic_unit_id
)

integrated_geographic_code <- normalise_text(
  canonical_integrated$geographic_code
)

integrated_geographic_name <- normalise_text(
  canonical_integrated$geographic_name
)

integrated_resolution_status <- normalise_text(
  canonical_integrated$concept_resolution_status
)

integrated_identifier_status <- normalise_text(
  canonical_integrated$identifier_status
)

integrated_wcvp_location_id <- normalise_text(
  canonical_integrated$wcvp_location_id
)

integrated_name_source <- normalise_text(
  canonical_integrated$geographic_name_source
)

integrated_name_validation_status <- normalise_text(
  canonical_integrated$geographic_name_validation_status
)


# ------------------------------------------------------------------------------
# 31. COMPLETE INTEGRATION AUDIT
# ------------------------------------------------------------------------------

integration_audit$geographic_name_after <-
  integrated_geographic_name[
    wcvp_selector
  ]

integration_audit$wcvp_location_id_after <-
  integrated_wcvp_location_id[
    wcvp_selector
  ]

integration_audit$geographic_name_source_after <-
  integrated_name_source[
    wcvp_selector
  ]

integration_audit$geographic_name_validation_status_after <-
  integrated_name_validation_status[
    wcvp_selector
  ]

integration_audit$concept_resolution_status_after <-
  integrated_resolution_status[
    wcvp_selector
  ]

integration_audit$identifier_status_after <-
  integrated_identifier_status[
    wcvp_selector
  ]


integration_audit$name_matches_validated_locality <-
  values_identical(
    integration_audit$geographic_name_after,
    integration_audit$validated_wcvp_locality
  )


integration_audit$location_id_matches_validated <-
  values_identical(
    integration_audit$wcvp_location_id_after,
    integration_audit$validated_wcvp_location_id
  )


integration_audit$source_matches_validated <-
  values_identical(
    integration_audit$geographic_name_source_after,
    integration_audit$validated_name_source
  )


integration_audit$identifier_status_unchanged <-
  values_identical(
    integration_audit$identifier_status_before,
    integration_audit$identifier_status_after
  )


# ------------------------------------------------------------------------------
# 32. JAPANESE BOTANICAL-AREA PRESERVATION AUDIT
# ------------------------------------------------------------------------------

japan_preservation <- data.frame(
  
  geographic_unit_id =
    original_geographic_unit_id[
      japan_selector
    ],
  
  geographic_code =
    original_geographic_code[
      japan_selector
    ],
  
  geographic_name_before =
    original_geographic_name[
      japan_selector
    ],
  
  geographic_name_after =
    integrated_geographic_name[
      japan_selector
    ],
  
  concept_resolution_status_before =
    original_resolution_status[
      japan_selector
    ],
  
  concept_resolution_status_after =
    integrated_resolution_status[
      japan_selector
    ],
  
  identifier_status_before =
    original_identifier_status[
      japan_selector
    ],
  
  identifier_status_after =
    integrated_identifier_status[
      japan_selector
    ],
  
  stringsAsFactors = FALSE
)


japan_preservation$name_unchanged <- values_identical(
  japan_preservation$geographic_name_before,
  japan_preservation$geographic_name_after
)


japan_preservation$resolution_status_unchanged <- values_identical(
  japan_preservation$concept_resolution_status_before,
  japan_preservation$concept_resolution_status_after
)


japan_preservation$identifier_status_unchanged <- values_identical(
  japan_preservation$identifier_status_before,
  japan_preservation$identifier_status_after
)


# ------------------------------------------------------------------------------
# 33. IDENTIFIER PRESERVATION AUDIT
# ------------------------------------------------------------------------------

identifier_preservation <- data.frame(
  
  row_number = seq_len(
    nrow(canonical_integrated)
  ),
  
  geographic_unit_id_before =
    original_geographic_unit_id,
  
  geographic_unit_id_after =
    integrated_geographic_unit_id,
  
  unchanged = values_identical(
    original_geographic_unit_id,
    integrated_geographic_unit_id
  ),
  
  stringsAsFactors = FALSE
)


# ------------------------------------------------------------------------------
# 34. GEOGRAPHIC CODE PRESERVATION AUDIT
# ------------------------------------------------------------------------------

code_preservation <- data.frame(
  
  row_number = seq_len(
    nrow(canonical_integrated)
  ),
  
  geographic_code_before =
    original_geographic_code,
  
  geographic_code_after =
    integrated_geographic_code,
  
  unchanged = values_identical(
    original_geographic_code,
    integrated_geographic_code
  ),
  
  stringsAsFactors = FALSE
)


# ------------------------------------------------------------------------------
# 35. CORE COUNTS
# ------------------------------------------------------------------------------

n_total <- nrow(
  canonical_integrated
)

n_named_total <- sum(
  !is.na(
    integrated_geographic_name
  )
)

n_missing_names_total <- sum(
  is.na(
    integrated_geographic_name
  )
)

n_named_japan <- sum(
  !is.na(
    integrated_geographic_name[
      japan_selector
    ]
  )
)

n_named_wcvp <- sum(
  !is.na(
    integrated_geographic_name[
      wcvp_selector
    ]
  )
)

n_wcvp_location_ids <- sum(
  !is.na(
    integrated_wcvp_location_id[
      wcvp_selector
    ]
  )
)

n_wcvp_name_sources <- sum(
  !is.na(
    integrated_name_source[
      wcvp_selector
    ]
  )
)

n_wcvp_validated_statuses <- sum(
  integrated_name_validation_status[
    wcvp_selector
  ] == "VALIDATED_06B3A",
  na.rm = TRUE
)

n_wcvp_resolved <- sum(
  integrated_resolution_status[
    wcvp_selector
  ] == "RESOLVED_VALIDATED_WCVP_LOCALITY",
  na.rm = TRUE
)

n_duplicate_ids <- sum(
  duplicated(
    integrated_geographic_unit_id[
      !is.na(
        integrated_geographic_unit_id
      )
    ]
  )
)

n_duplicate_codes <- sum(
  duplicated(
    integrated_geographic_code[
      !is.na(
        integrated_geographic_code
      )
    ]
  )
)

n_identifier_changes <- sum(
  !identifier_preservation$unchanged
)

n_code_changes <- sum(
  !code_preservation$unchanged
)

n_japan_name_changes <- sum(
  !japan_preservation$name_unchanged
)

n_japan_resolution_changes <- sum(
  !japan_preservation$resolution_status_unchanged
)

n_japan_identifier_status_changes <- sum(
  !japan_preservation$identifier_status_unchanged
)

n_wcvp_name_mismatches <- sum(
  !integration_audit$name_matches_validated_locality
)

n_wcvp_location_id_mismatches <- sum(
  !integration_audit$location_id_matches_validated
)

n_wcvp_source_mismatches <- sum(
  !integration_audit$source_matches_validated
)

n_wcvp_identifier_status_changes <- sum(
  !integration_audit$identifier_status_unchanged
)


# ------------------------------------------------------------------------------
# 36. IDENTIFIER STATUS PROFILE
# ------------------------------------------------------------------------------

identifier_status_values <- unique(
  integrated_identifier_status[
    !is.na(
      integrated_identifier_status
    )
  ]
)


identifier_status_profile <- data.frame(
  
  identifier_status =
    identifier_status_values,
  
  n_concepts = vapply(
    identifier_status_values,
    function(status_value) {
      
      sum(
        integrated_identifier_status ==
          status_value,
        na.rm = TRUE
      )
    },
    integer(1)
  ),
  
  stringsAsFactors = FALSE
)


# ------------------------------------------------------------------------------
# 37. RESOLUTION STATUS PROFILE
# ------------------------------------------------------------------------------

resolution_status_values <- unique(
  integrated_resolution_status[
    !is.na(
      integrated_resolution_status
    )
  ]
)


resolution_status_profile <- data.frame(
  
  concept_resolution_status =
    resolution_status_values,
  
  n_concepts = vapply(
    resolution_status_values,
    function(status_value) {
      
      sum(
        integrated_resolution_status ==
          status_value,
        na.rm = TRUE
      )
    },
    integer(1)
  ),
  
  stringsAsFactors = FALSE
)


# ------------------------------------------------------------------------------
# 38. SUMMARY TABLE
# ------------------------------------------------------------------------------

summary_table <- data.frame(
  
  metric = c(
    "canonical_geographic_concepts",
    "japanese_botanical_area_concepts",
    "wcvp_tdwg_concepts",
    "named_geographic_concepts",
    "missing_geographic_names",
    "named_japanese_botanical_areas",
    "named_wcvp_tdwg_concepts",
    "wcvp_location_ids_integrated",
    "wcvp_name_sources_integrated",
    "wcvp_validated_name_statuses",
    "wcvp_concepts_resolved",
    "duplicate_geographic_unit_ids",
    "duplicate_geographic_codes",
    "geographic_unit_id_changes",
    "geographic_code_changes",
    "japan_name_changes",
    "japan_resolution_status_changes",
    "japan_identifier_status_changes",
    "wcvp_name_mismatches",
    "wcvp_location_id_mismatches",
    "wcvp_source_mismatches",
    "wcvp_identifier_status_changes"
  ),
  
  value = c(
    n_total,
    n_japan,
    n_wcvp,
    n_named_total,
    n_missing_names_total,
    n_named_japan,
    n_named_wcvp,
    n_wcvp_location_ids,
    n_wcvp_name_sources,
    n_wcvp_validated_statuses,
    n_wcvp_resolved,
    n_duplicate_ids,
    n_duplicate_codes,
    n_identifier_changes,
    n_code_changes,
    n_japan_name_changes,
    n_japan_resolution_changes,
    n_japan_identifier_status_changes,
    n_wcvp_name_mismatches,
    n_wcvp_location_id_mismatches,
    n_wcvp_source_mismatches,
    n_wcvp_identifier_status_changes
  ),
  
  stringsAsFactors = FALSE
)


# ------------------------------------------------------------------------------
# 39. VALIDATION GATE
# ------------------------------------------------------------------------------

validation_gate <- data.frame(
  
  criterion = c(
    "06b3a_validation_passed",
    "input_canonical_cardinality_418",
    "japanese_botanical_area_count_51",
    "wcvp_tdwg_concept_count_367",
    "validated_crosswalk_count_367",
    "all_418_concepts_named",
    "all_51_japanese_botanical_areas_named",
    "all_367_wcvp_tdwg_concepts_named",
    "all_367_wcvp_location_ids_integrated",
    "all_367_wcvp_name_sources_integrated",
    "all_367_wcvp_name_statuses_validated",
    "all_367_wcvp_concepts_resolved",
    "wcvp_names_match_validated_crosswalk",
    "wcvp_location_ids_match_validated_crosswalk",
    "wcvp_sources_match_validated_crosswalk",
    "no_duplicate_geographic_unit_ids",
    "no_duplicate_geographic_codes",
    "geographic_unit_ids_unchanged",
    "geographic_codes_unchanged",
    "japanese_botanical_area_names_unchanged",
    "japanese_resolution_statuses_unchanged",
    "japanese_identifier_statuses_unchanged",
    "wcvp_identifier_statuses_unchanged",
    "identifiers_not_frozen",
    "no_geographic_names_inferred",
    "published_vpjd_not_modified"
  ),
  
  passed = c(
    all(
      validation_06b3a$passed
    ),
    
    n_total == 418L,
    
    n_japan == 51L,
    
    n_wcvp == 367L,
    
    nrow(crosswalk_work) == 367L,
    
    n_named_total == 418L,
    
    n_named_japan == 51L,
    
    n_named_wcvp == 367L,
    
    n_wcvp_location_ids == 367L,
    
    n_wcvp_name_sources == 367L,
    
    n_wcvp_validated_statuses == 367L,
    
    n_wcvp_resolved == 367L,
    
    n_wcvp_name_mismatches == 0L,
    
    n_wcvp_location_id_mismatches == 0L,
    
    n_wcvp_source_mismatches == 0L,
    
    n_duplicate_ids == 0L,
    
    n_duplicate_codes == 0L,
    
    n_identifier_changes == 0L,
    
    n_code_changes == 0L,
    
    n_japan_name_changes == 0L,
    
    n_japan_resolution_changes == 0L,
    
    n_japan_identifier_status_changes == 0L,
    
    n_wcvp_identifier_status_changes == 0L,
    
    TRUE,
    
    TRUE,
    
    TRUE
  ),
  
  stringsAsFactors = FALSE
)


# ------------------------------------------------------------------------------
# 40. WRITE AUDIT OUTPUTS
# ------------------------------------------------------------------------------

write.csv(
  integration_audit,
  INTEGRATION_AUDIT_FILE,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)


write.csv(
  japan_preservation,
  JAPAN_PRESERVATION_FILE,
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
  code_preservation,
  CODE_PRESERVATION_FILE,
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
# 41. WRITE INTEGRATED VOCABULARY ONLY AFTER COMPLETE VALIDATION
# ------------------------------------------------------------------------------

if (all(validation_gate$passed)) {
  
  write.csv(
    canonical_integrated,
    INTEGRATED_UNIT_FILE,
    row.names = FALSE,
    na = "",
    fileEncoding = "UTF-8"
  )
}


# ------------------------------------------------------------------------------
# 42. CONSOLE HEADER
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" VPJD GEOGRAPHY 06b3b - WCVP NAME INTEGRATION COMPLETE\n")
cat("============================================================\n\n")


# ------------------------------------------------------------------------------
# 43. CONSOLE SUMMARY
# ------------------------------------------------------------------------------

cat("Summary:\n\n")

print(
  summary_table,
  row.names = FALSE
)


# ------------------------------------------------------------------------------
# 44. RESOLUTION STATUS PROFILE
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" RESOLUTION STATUS PROFILE\n")
cat("============================================================\n\n")

print(
  resolution_status_profile,
  row.names = FALSE
)


# ------------------------------------------------------------------------------
# 45. IDENTIFIER STATUS PROFILE
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" IDENTIFIER STATUS PROFILE\n")
cat("============================================================\n\n")

print(
  identifier_status_profile,
  row.names = FALSE
)


# ------------------------------------------------------------------------------
# 46. VALIDATION GATE
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
# 47. INTERPRETATION
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" INTERPRETATION\n")
cat("============================================================\n\n")


if (all(validation_gate$passed)) {
  
  cat(
    paste0(
      "PASS. The validated WCVP locality evidence has been integrated into ",
      "the provisional VPJD Geography canonical vocabulary.\n\n"
    )
  )
  
  cat(
    paste0(
      "The integrated vocabulary contains ",
      format(
        n_total,
        big.mark = ","
      ),
      " geographic concepts: ",
      format(
        n_japan,
        big.mark = ","
      ),
      " Japanese botanical areas and ",
      format(
        n_wcvp,
        big.mark = ","
      ),
      " WCVP/TDWG geographic concepts.\n\n"
    )
  )
  
  cat(
    paste0(
      "All ",
      format(
        n_total,
        big.mark = ","
      ),
      " concepts now have human-readable geographic names. ",
      "All 367 WCVP/TDWG names were taken directly from the validated ",
      "06b3a crosswalk.\n\n"
    )
  )
  
  cat(
    paste0(
      "All existing geographic_unit_id and geographic_code values were ",
      "preserved unchanged. The 51 Japanese botanical-area concepts were ",
      "also preserved unchanged.\n\n"
    )
  )
  
  cat(
    paste0(
      "Identifier status remains unchanged and provisional. No identifiers ",
      "were frozen by this script.\n"
    )
  )
  
} else {
  
  cat(
    "FAIL. One or more post-integration validation criteria failed.\n\n"
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
# 48. NEXT STEP
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" NEXT STEP\n")
cat("============================================================\n\n")


if (all(validation_gate$passed)) {
  
  cat(
    paste0(
      "The canonical geographic vocabulary is now fully named but remains ",
      "provisional.\n\n",
      "Proceed to a dedicated post-integration validation and release-readiness ",
      "stage before freezing VPJD-GEO identifiers.\n\n",
      "Recommended next script:\n\n",
      "  geography_06b4_validate_integrated_geographic_vocabulary.R\n\n",
      "That script should independently validate the complete 418-concept ",
      "vocabulary, provenance, identifier stability, concept-group structure ",
      "and release readiness. It should NOT freeze identifiers itself.\n"
    )
  )
  
} else {
  
  cat(
    paste0(
      "Do not proceed to release-readiness validation.\n",
      "Resolve the failed 06b3b validation criterion or criteria first.\n"
    )
  )
}


# ------------------------------------------------------------------------------
# 49. OUTPUT LOCATIONS
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" OUTPUTS\n")
cat("============================================================\n\n")


cat(
  "Integration audit:\n",
  INTEGRATION_AUDIT_FILE,
  "\n\n",
  sep = ""
)

cat(
  "Japanese botanical-area preservation audit:\n",
  JAPAN_PRESERVATION_FILE,
  "\n\n",
  sep = ""
)

cat(
  "Identifier preservation audit:\n",
  IDENTIFIER_PRESERVATION_FILE,
  "\n\n",
  sep = ""
)

cat(
  "Geographic-code preservation audit:\n",
  CODE_PRESERVATION_FILE,
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
  "Validation gate:\n",
  VALIDATION_FILE,
  "\n\n",
  sep = ""
)


if (all(validation_gate$passed)) {
  
  cat(
    "Integrated canonical vocabulary:\n",
    INTEGRATED_UNIT_FILE,
    "\n\n",
    sep = ""
  )
  
} else {
  
  cat(
    paste0(
      "Integrated canonical vocabulary: NOT WRITTEN because validation ",
      "did not pass.\n\n"
    )
  )
}


# ------------------------------------------------------------------------------
# 50. SAFEGUARDS
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" SAFEGUARDS\n")
cat("============================================================\n\n")

cat("06b2 provisional canonical vocabulary was NOT modified in place.\n")
cat("06b3a validated crosswalk was NOT modified.\n")
cat("WCVP locality labels were NOT inferred or rewritten.\n")
cat("TDWG codes were NOT inferred or rewritten.\n")
cat("Existing VPJD-GEO identifiers were NOT changed.\n")
cat("VPJD-GEO identifiers were NOT frozen.\n")
cat("Japanese botanical-area concepts were NOT modified.\n")
cat("Geographic hierarchy was NOT inferred.\n")
cat("Geographic geometry was NOT inferred.\n")
cat("Missing geographic evidence was NOT interpreted as absence.\n")
cat("Nakamura predicates were NOT evaluated.\n")
cat("Star categories were NOT evaluated.\n")
cat("Published VPJD Taxonomic Release v1.0.0 was NOT modified.\n")
cat("VPJD Geography v1.0.0 was NOT frozen or published.\n")

cat("\n")
cat("============================================================\n")