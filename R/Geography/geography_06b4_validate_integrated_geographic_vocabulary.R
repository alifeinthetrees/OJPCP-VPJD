# ==============================================================================
# VPJD GEOGRAPHY MODULE
# geography_06b4_validate_integrated_geographic_vocabulary.R
#
# PURPOSE
# -------
# Independently validate the fully named, integrated canonical geographic
# vocabulary produced by:
#
#   geography_06b3b_integrate_wcvp_locality_names.R
#
# and determine whether that vocabulary is eligible to proceed to a deliberate
# identifier-freeze stage for VPJD Geography v1.0.0.
#
#
# POSITION IN PIPELINE
# --------------------
#
#   06b2  Rebuild canonical geographic vocabulary
#     |
#     v
#   06b3  Validate rebuilt vocabulary
#     |
#     v
#   06b3a Validate WCVP locality mapping
#     |
#     v
#   06b3b Integrate validated WCVP locality names
#     |
#     |-- 418 concepts
#     |-- 51 Japanese botanical areas
#     |-- 367 WCVP/TDWG concepts
#     |-- 418 named
#     |-- zero identifier changes
#     |-- zero code changes
#     `-- PASS
#               |
#               v
#   06b4  INDEPENDENT INTEGRATED-VOCABULARY VALIDATION   <-- THIS SCRIPT
#               |
#               | PASS
#               v
#   06b5  Deliberate identifier freeze
#
#
# OBJECTIVES
# ----------
#
#  1. Independently read the integrated 418-concept vocabulary.
#
#  2. Confirm the expected 51 + 367 concept architecture.
#
#  3. Validate completeness and uniqueness of geographic_unit_id.
#
#  4. Validate completeness and uniqueness of geographic_code.
#
#  5. Validate completeness of geographic_name.
#
#  6. Validate concept-group membership.
#
#  7. Validate concept-resolution status.
#
#  8. Validate WCVP location IDs and geographic-name provenance.
#
#  9. Reconcile the 367 WCVP concepts independently against the validated
#     06b3a crosswalk.
#
# 10. Reconcile the integrated vocabulary against the pre-integration 06b2
#     vocabulary to prove identifier and code stability.
#
# 11. Confirm that the 51 Japanese botanical-area concepts were preserved.
#
# 12. Confirm that all identifiers remain provisional.
#
# 13. Produce an explicit release-readiness / identifier-freeze eligibility
#     gate.
#
#
# IMPORTANT
# ---------
#
# This script is an AUDIT ONLY.
#
# It DOES NOT:
#
#   - modify the integrated vocabulary;
#   - regenerate geographic_unit_id;
#   - change geographic_code;
#   - change geographic_name;
#   - freeze identifiers;
#   - change identifier_status;
#   - infer names;
#   - infer hierarchy;
#   - infer geometry;
#   - alter WCVP evidence;
#   - alter Japanese botanical-area evidence;
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

PREINTEGRATION_ROOT <- file.path(
  DERIVED_ROOT,
  "canonical_geographic_units"
)

INTEGRATED_ROOT <- file.path(
  DERIVED_ROOT,
  "canonical_geographic_units_integrated"
)

WCVP_VALIDATION_ROOT <- file.path(
  DERIVED_ROOT,
  "wcvp_locality_validation"
)

AUDIT_06B3B_ROOT <- file.path(
  PROJECT_ROOT,
  "audit",
  "geography",
  "release_v1.0.0",
  "06b3b_integrate_wcvp_locality_names"
)

AUDIT_ROOT <- file.path(
  PROJECT_ROOT,
  "audit",
  "geography",
  "release_v1.0.0",
  "06b4_validate_integrated_geographic_vocabulary"
)

dir.create(
  AUDIT_ROOT,
  recursive = TRUE,
  showWarnings = FALSE
)


# ------------------------------------------------------------------------------
# 02. INPUT FILES
# ------------------------------------------------------------------------------

INTEGRATED_FILE <- file.path(
  INTEGRATED_ROOT,
  "vpjd_geographic_units_integrated.csv"
)

PREINTEGRATION_FILE <- file.path(
  PREINTEGRATION_ROOT,
  "vpjd_geographic_units_provisional.csv"
)

VALIDATED_CROSSWALK_FILE <- file.path(
  WCVP_VALIDATION_ROOT,
  "vpjd_wcvp_tdwg_locality_crosswalk_validated.csv"
)

VALIDATION_06B3B_FILE <- file.path(
  AUDIT_06B3B_ROOT,
  "geography_06b3b_validation_gate.csv"
)


# ------------------------------------------------------------------------------
# 03. OUTPUT FILES
# ------------------------------------------------------------------------------

SUMMARY_FILE <- file.path(
  AUDIT_ROOT,
  "geography_06b4_summary.csv"
)

STRUCTURE_PROFILE_FILE <- file.path(
  AUDIT_ROOT,
  "geography_06b4_concept_structure_profile.csv"
)

RESOLUTION_PROFILE_FILE <- file.path(
  AUDIT_ROOT,
  "geography_06b4_resolution_status_profile.csv"
)

IDENTIFIER_PROFILE_FILE <- file.path(
  AUDIT_ROOT,
  "geography_06b4_identifier_status_profile.csv"
)

WCVP_RECONCILIATION_FILE <- file.path(
  AUDIT_ROOT,
  "geography_06b4_wcvp_crosswalk_reconciliation.csv"
)

PREINTEGRATION_RECONCILIATION_FILE <- file.path(
  AUDIT_ROOT,
  "geography_06b4_preintegration_reconciliation.csv"
)

JAPAN_PRESERVATION_FILE <- file.path(
  AUDIT_ROOT,
  "geography_06b4_japan_preservation_audit.csv"
)

VALIDATION_GATE_FILE <- file.path(
  AUDIT_ROOT,
  "geography_06b4_validation_gate.csv"
)

RELEASE_READINESS_FILE <- file.path(
  AUDIT_ROOT,
  "geography_06b4_release_readiness_gate.csv"
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
# 07. HELPER: PROFILE VALUES
# ------------------------------------------------------------------------------

build_profile <- function(x, field_name) {
  
  values <- normalise_text(x)
  
  nonmissing_values <- values[
    !is.na(values)
  ]
  
  unique_values <- sort(
    unique(nonmissing_values)
  )
  
  if (length(unique_values) == 0L) {
    
    result <- data.frame(
      value = character(0),
      n_concepts = integer(0),
      stringsAsFactors = FALSE
    )
    
    names(result)[1L] <- field_name
    
    return(result)
  }
  
  counts <- vapply(
    unique_values,
    function(value) {
      
      sum(
        values == value,
        na.rm = TRUE
      )
    },
    integer(1)
  )
  
  result <- data.frame(
    value = unique_values,
    n_concepts = counts,
    stringsAsFactors = FALSE
  )
  
  names(result)[1L] <- field_name
  
  result
}


# ------------------------------------------------------------------------------
# 08. VALIDATE REQUIRED INPUT FILES
# ------------------------------------------------------------------------------

required_inputs <- c(
  INTEGRATED_FILE,
  PREINTEGRATION_FILE,
  VALIDATED_CROSSWALK_FILE,
  VALIDATION_06B3B_FILE
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
# 09. READ INPUTS INDEPENDENTLY
# ------------------------------------------------------------------------------

integrated <- safe_read_csv(
  INTEGRATED_FILE
)

preintegration <- safe_read_csv(
  PREINTEGRATION_FILE
)

crosswalk <- safe_read_csv(
  VALIDATED_CROSSWALK_FILE
)

validation_06b3b <- safe_read_csv(
  VALIDATION_06B3B_FILE
)


# ------------------------------------------------------------------------------
# 10. REQUIRE 06b3b PASS
# ------------------------------------------------------------------------------

if (!"passed" %in% names(validation_06b3b)) {
  
  stop(
    "06b3b validation gate lacks required field 'passed'."
  )
}


validation_06b3b$passed <- as.logical(
  validation_06b3b$passed
)


if (
  any(is.na(validation_06b3b$passed)) ||
  !all(validation_06b3b$passed)
) {
  
  stop(
    paste0(
      "06b3b did not pass completely.\n",
      "Do not assess the integrated vocabulary for identifier freeze."
    )
  )
}


# ------------------------------------------------------------------------------
# 11. VALIDATE INTEGRATED SCHEMA
# ------------------------------------------------------------------------------

required_integrated_fields <- c(
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

missing_integrated_fields <- setdiff(
  required_integrated_fields,
  names(integrated)
)

if (length(missing_integrated_fields) > 0L) {
  
  stop(
    paste0(
      "Integrated vocabulary lacks required field(s):\n",
      paste(
        missing_integrated_fields,
        collapse = "\n"
      )
    )
  )
}


# ------------------------------------------------------------------------------
# 12. VALIDATE PRE-INTEGRATION SCHEMA
# ------------------------------------------------------------------------------

required_preintegration_fields <- c(
  "geographic_unit_id",
  "geographic_code",
  "geographic_name",
  "concept_group",
  "concept_resolution_status",
  "identifier_status"
)

missing_preintegration_fields <- setdiff(
  required_preintegration_fields,
  names(preintegration)
)

if (length(missing_preintegration_fields) > 0L) {
  
  stop(
    paste0(
      "Pre-integration vocabulary lacks required field(s):\n",
      paste(
        missing_preintegration_fields,
        collapse = "\n"
      )
    )
  )
}


# ------------------------------------------------------------------------------
# 13. VALIDATE CROSSWALK SCHEMA
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
  names(crosswalk)
)

if (length(missing_crosswalk_fields) > 0L) {
  
  stop(
    paste0(
      "Validated crosswalk lacks required field(s):\n",
      paste(
        missing_crosswalk_fields,
        collapse = "\n"
      )
    )
  )
}


# ------------------------------------------------------------------------------
# 14. CREATE NORMALISED WORKING FIELDS
# ------------------------------------------------------------------------------

integrated_id <- normalise_text(
  integrated$geographic_unit_id
)

integrated_code <- normalise_text(
  integrated$geographic_code
)

integrated_name <- normalise_text(
  integrated$geographic_name
)

integrated_group <- normalise_text(
  integrated$concept_group
)

integrated_resolution <- normalise_text(
  integrated$concept_resolution_status
)

integrated_identifier_status <- normalise_text(
  integrated$identifier_status
)

integrated_wcvp_location_id <- normalise_text(
  integrated$wcvp_location_id
)

integrated_name_source <- normalise_text(
  integrated$geographic_name_source
)

integrated_name_validation <- normalise_text(
  integrated$geographic_name_validation_status
)


pre_id <- normalise_text(
  preintegration$geographic_unit_id
)

pre_code <- normalise_text(
  preintegration$geographic_code
)

pre_name <- normalise_text(
  preintegration$geographic_name
)

pre_group <- normalise_text(
  preintegration$concept_group
)

pre_resolution <- normalise_text(
  preintegration$concept_resolution_status
)

pre_identifier_status <- normalise_text(
  preintegration$identifier_status
)


crosswalk_code <- normalise_text(
  crosswalk$tdwg_code
)

crosswalk_locality <- normalise_text(
  crosswalk$wcvp_locality
)

crosswalk_location_id <- normalise_text(
  crosswalk$wcvp_location_id
)

crosswalk_source <- normalise_text(
  crosswalk$location_reference_source
)

crosswalk_validation <- normalise_text(
  crosswalk$validation_status
)


# ------------------------------------------------------------------------------
# 15. CONCEPT-GROUP SELECTORS
# ------------------------------------------------------------------------------

japan_selector <- (
  integrated_group ==
    "JAPAN_BOTANICAL_AREA"
)

japan_selector[is.na(japan_selector)] <- FALSE


wcvp_selector <- (
  integrated_group ==
    "WCVP_TDWG_LOCATION"
)

wcvp_selector[is.na(wcvp_selector)] <- FALSE


# ------------------------------------------------------------------------------
# 16. CORE STRUCTURAL COUNTS
# ------------------------------------------------------------------------------

n_total <- nrow(
  integrated
)

n_japan <- sum(
  japan_selector
)

n_wcvp <- sum(
  wcvp_selector
)

n_unrecognised_groups <- sum(
  !(japan_selector | wcvp_selector)
)


n_missing_ids <- sum(
  is.na(integrated_id)
)

n_missing_codes <- sum(
  is.na(integrated_code)
)

n_missing_names <- sum(
  is.na(integrated_name)
)

n_missing_groups <- sum(
  is.na(integrated_group)
)

n_missing_resolution <- sum(
  is.na(integrated_resolution)
)

n_missing_identifier_status <- sum(
  is.na(integrated_identifier_status)
)


n_duplicate_ids <- sum(
  duplicated(
    integrated_id[
      !is.na(integrated_id)
    ]
  )
)

n_duplicate_codes <- sum(
  duplicated(
    integrated_code[
      !is.na(integrated_code)
    ]
  )
)


# ------------------------------------------------------------------------------
# 17. EXPECTED RESOLUTION STATES
# ------------------------------------------------------------------------------

n_japan_resolved <- sum(
  integrated_resolution[
    japan_selector
  ] == "RESOLVED",
  na.rm = TRUE
)


n_wcvp_resolved <- sum(
  integrated_resolution[
    wcvp_selector
  ] == "RESOLVED_VALIDATED_WCVP_LOCALITY",
  na.rm = TRUE
)


n_unexpected_resolution <- sum(
  (
    japan_selector &
      integrated_resolution != "RESOLVED"
  ) |
    (
      wcvp_selector &
        integrated_resolution !=
        "RESOLVED_VALIDATED_WCVP_LOCALITY"
    ),
  na.rm = TRUE
)


# ------------------------------------------------------------------------------
# 18. IDENTIFIER STATUS AUDIT
#
# At 06b4 every identifier is expected to remain provisional.
# ------------------------------------------------------------------------------

n_provisional_identifiers <- sum(
  integrated_identifier_status ==
    "PROVISIONAL_06B2",
  na.rm = TRUE
)


n_nonprovisional_identifiers <- sum(
  !is.na(integrated_identifier_status) &
    integrated_identifier_status !=
    "PROVISIONAL_06B2"
)


# ------------------------------------------------------------------------------
# 19. WCVP EVIDENCE COMPLETENESS
# ------------------------------------------------------------------------------

n_wcvp_names <- sum(
  !is.na(
    integrated_name[
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

n_wcvp_sources <- sum(
  !is.na(
    integrated_name_source[
      wcvp_selector
    ]
  )
)

n_wcvp_validated_names <- sum(
  integrated_name_validation[
    wcvp_selector
  ] == "VALIDATED_06B3A",
  na.rm = TRUE
)


# ------------------------------------------------------------------------------
# 20. JAPANESE CONCEPT WCVP-FIELD AUDIT
#
# WCVP-specific fields added during 06b3b should not have been populated for
# Japanese botanical-area concepts.
# ------------------------------------------------------------------------------

n_japan_wcvp_location_ids <- sum(
  !is.na(
    integrated_wcvp_location_id[
      japan_selector
    ]
  )
)

n_japan_wcvp_name_sources <- sum(
  !is.na(
    integrated_name_source[
      japan_selector
    ]
  )
)

n_japan_wcvp_validation_statuses <- sum(
  !is.na(
    integrated_name_validation[
      japan_selector
    ]
  )
)


# ------------------------------------------------------------------------------
# 21. VALIDATE CROSSWALK CARDINALITY
# ------------------------------------------------------------------------------

n_crosswalk_rows <- nrow(
  crosswalk
)

n_crosswalk_codes <- length(
  unique(
    crosswalk_code[
      !is.na(crosswalk_code)
    ]
  )
)

n_crosswalk_location_ids <- length(
  unique(
    crosswalk_location_id[
      !is.na(crosswalk_location_id)
    ]
  )
)


# ------------------------------------------------------------------------------
# 22. RECONCILE WCVP CONCEPTS AGAINST 06b3a CROSSWALK
# ------------------------------------------------------------------------------

integrated_wcvp_codes <- integrated_code[
  wcvp_selector
]


crosswalk_match_index <- match(
  integrated_wcvp_codes,
  crosswalk_code
)


wcvp_reconciliation <- data.frame(
  
  geographic_unit_id =
    integrated_id[
      wcvp_selector
    ],
  
  tdwg_code =
    integrated_wcvp_codes,
  
  integrated_geographic_name =
    integrated_name[
      wcvp_selector
    ],
  
  integrated_wcvp_location_id =
    integrated_wcvp_location_id[
      wcvp_selector
    ],
  
  integrated_name_source =
    integrated_name_source[
      wcvp_selector
    ],
  
  integrated_name_validation_status =
    integrated_name_validation[
      wcvp_selector
    ],
  
  crosswalk_match_found =
    !is.na(crosswalk_match_index),
  
  stringsAsFactors = FALSE
)


wcvp_reconciliation$crosswalk_locality <- NA_character_
wcvp_reconciliation$crosswalk_wcvp_location_id <- NA_character_
wcvp_reconciliation$crosswalk_source <- NA_character_
wcvp_reconciliation$crosswalk_validation_status <- NA_character_


matched_crosswalk_selector <- !is.na(
  crosswalk_match_index
)


wcvp_reconciliation$crosswalk_locality[
  matched_crosswalk_selector
] <- crosswalk_locality[
  crosswalk_match_index[
    matched_crosswalk_selector
  ]
]


wcvp_reconciliation$crosswalk_wcvp_location_id[
  matched_crosswalk_selector
] <- crosswalk_location_id[
  crosswalk_match_index[
    matched_crosswalk_selector
  ]
]


wcvp_reconciliation$crosswalk_source[
  matched_crosswalk_selector
] <- crosswalk_source[
  crosswalk_match_index[
    matched_crosswalk_selector
  ]
]


wcvp_reconciliation$crosswalk_validation_status[
  matched_crosswalk_selector
] <- crosswalk_validation[
  crosswalk_match_index[
    matched_crosswalk_selector
  ]
]


wcvp_reconciliation$name_matches <- values_identical(
  wcvp_reconciliation$integrated_geographic_name,
  wcvp_reconciliation$crosswalk_locality
)


wcvp_reconciliation$location_id_matches <- values_identical(
  wcvp_reconciliation$integrated_wcvp_location_id,
  wcvp_reconciliation$crosswalk_wcvp_location_id
)


wcvp_reconciliation$source_matches <- values_identical(
  wcvp_reconciliation$integrated_name_source,
  wcvp_reconciliation$crosswalk_source
)


wcvp_reconciliation$validation_status_matches <- values_identical(
  wcvp_reconciliation$integrated_name_validation_status,
  wcvp_reconciliation$crosswalk_validation_status
)


n_wcvp_crosswalk_missing <- sum(
  !wcvp_reconciliation$crosswalk_match_found
)

n_wcvp_name_mismatches <- sum(
  !wcvp_reconciliation$name_matches
)

n_wcvp_location_id_mismatches <- sum(
  !wcvp_reconciliation$location_id_matches
)

n_wcvp_source_mismatches <- sum(
  !wcvp_reconciliation$source_matches
)

n_wcvp_validation_mismatches <- sum(
  !wcvp_reconciliation$validation_status_matches
)


# ------------------------------------------------------------------------------
# 23. TEST CROSSWALK CODE-SET EQUALITY
# ------------------------------------------------------------------------------

integrated_wcvp_unique_codes <- sort(
  unique(
    integrated_wcvp_codes[
      !is.na(integrated_wcvp_codes)
    ]
  )
)

crosswalk_unique_codes <- sort(
  unique(
    crosswalk_code[
      !is.na(crosswalk_code)
    ]
  )
)


integrated_codes_not_in_crosswalk <- setdiff(
  integrated_wcvp_unique_codes,
  crosswalk_unique_codes
)

crosswalk_codes_not_in_integrated <- setdiff(
  crosswalk_unique_codes,
  integrated_wcvp_unique_codes
)


# ------------------------------------------------------------------------------
# 24. RECONCILE AGAINST PRE-INTEGRATION VOCABULARY
#
# Match on geographic_unit_id, not row order.
# ------------------------------------------------------------------------------

pre_match_index <- match(
  integrated_id,
  pre_id
)


preintegration_reconciliation <- data.frame(
  
  geographic_unit_id =
    integrated_id,
  
  integrated_geographic_code =
    integrated_code,
  
  integrated_geographic_name =
    integrated_name,
  
  integrated_concept_group =
    integrated_group,
  
  integrated_resolution_status =
    integrated_resolution,
  
  integrated_identifier_status =
    integrated_identifier_status,
  
  preintegration_match_found =
    !is.na(pre_match_index),
  
  stringsAsFactors = FALSE
)


preintegration_reconciliation$pre_geographic_code <- NA_character_
preintegration_reconciliation$pre_geographic_name <- NA_character_
preintegration_reconciliation$pre_concept_group <- NA_character_
preintegration_reconciliation$pre_resolution_status <- NA_character_
preintegration_reconciliation$pre_identifier_status <- NA_character_


matched_pre_selector <- !is.na(
  pre_match_index
)


preintegration_reconciliation$pre_geographic_code[
  matched_pre_selector
] <- pre_code[
  pre_match_index[
    matched_pre_selector
  ]
]


preintegration_reconciliation$pre_geographic_name[
  matched_pre_selector
] <- pre_name[
  pre_match_index[
    matched_pre_selector
  ]
]


preintegration_reconciliation$pre_concept_group[
  matched_pre_selector
] <- pre_group[
  pre_match_index[
    matched_pre_selector
  ]
]


preintegration_reconciliation$pre_resolution_status[
  matched_pre_selector
] <- pre_resolution[
  pre_match_index[
    matched_pre_selector
  ]
]


preintegration_reconciliation$pre_identifier_status[
  matched_pre_selector
] <- pre_identifier_status[
  pre_match_index[
    matched_pre_selector
  ]
]


preintegration_reconciliation$code_unchanged <- values_identical(
  preintegration_reconciliation$integrated_geographic_code,
  preintegration_reconciliation$pre_geographic_code
)


preintegration_reconciliation$concept_group_unchanged <- values_identical(
  preintegration_reconciliation$integrated_concept_group,
  preintegration_reconciliation$pre_concept_group
)


preintegration_reconciliation$identifier_status_unchanged <- values_identical(
  preintegration_reconciliation$integrated_identifier_status,
  preintegration_reconciliation$pre_identifier_status
)


n_preintegration_missing <- sum(
  !preintegration_reconciliation$preintegration_match_found
)

n_code_changes <- sum(
  !preintegration_reconciliation$code_unchanged
)

n_group_changes <- sum(
  !preintegration_reconciliation$concept_group_unchanged
)

n_identifier_status_changes <- sum(
  !preintegration_reconciliation$identifier_status_unchanged
)


# ------------------------------------------------------------------------------
# 25. TEST PRE-INTEGRATION ID SET EQUALITY
# ------------------------------------------------------------------------------

integrated_unique_ids <- sort(
  unique(
    integrated_id[
      !is.na(integrated_id)
    ]
  )
)

pre_unique_ids <- sort(
  unique(
    pre_id[
      !is.na(pre_id)
    ]
  )
)


integrated_ids_not_in_pre <- setdiff(
  integrated_unique_ids,
  pre_unique_ids
)

pre_ids_not_in_integrated <- setdiff(
  pre_unique_ids,
  integrated_unique_ids
)


# ------------------------------------------------------------------------------
# 26. JAPANESE BOTANICAL-AREA PRESERVATION AUDIT
# ------------------------------------------------------------------------------

japan_integrated_ids <- integrated_id[
  japan_selector
]


japan_pre_match_index <- match(
  japan_integrated_ids,
  pre_id
)


japan_preservation <- data.frame(
  
  geographic_unit_id =
    japan_integrated_ids,
  
  integrated_geographic_code =
    integrated_code[
      japan_selector
    ],
  
  integrated_geographic_name =
    integrated_name[
      japan_selector
    ],
  
  integrated_resolution_status =
    integrated_resolution[
      japan_selector
    ],
  
  integrated_identifier_status =
    integrated_identifier_status[
      japan_selector
    ],
  
  preintegration_match_found =
    !is.na(japan_pre_match_index),
  
  stringsAsFactors = FALSE
)


japan_preservation$pre_geographic_code <- NA_character_
japan_preservation$pre_geographic_name <- NA_character_
japan_preservation$pre_resolution_status <- NA_character_
japan_preservation$pre_identifier_status <- NA_character_


japan_matched_selector <- !is.na(
  japan_pre_match_index
)


japan_preservation$pre_geographic_code[
  japan_matched_selector
] <- pre_code[
  japan_pre_match_index[
    japan_matched_selector
  ]
]


japan_preservation$pre_geographic_name[
  japan_matched_selector
] <- pre_name[
  japan_pre_match_index[
    japan_matched_selector
  ]
]


japan_preservation$pre_resolution_status[
  japan_matched_selector
] <- pre_resolution[
  japan_pre_match_index[
    japan_matched_selector
  ]
]


japan_preservation$pre_identifier_status[
  japan_matched_selector
] <- pre_identifier_status[
  japan_pre_match_index[
    japan_matched_selector
  ]
]


japan_preservation$code_unchanged <- values_identical(
  japan_preservation$integrated_geographic_code,
  japan_preservation$pre_geographic_code
)


japan_preservation$name_unchanged <- values_identical(
  japan_preservation$integrated_geographic_name,
  japan_preservation$pre_geographic_name
)


japan_preservation$resolution_status_unchanged <- values_identical(
  japan_preservation$integrated_resolution_status,
  japan_preservation$pre_resolution_status
)


japan_preservation$identifier_status_unchanged <- values_identical(
  japan_preservation$integrated_identifier_status,
  japan_preservation$pre_identifier_status
)


n_japan_missing_pre <- sum(
  !japan_preservation$preintegration_match_found
)

n_japan_code_changes <- sum(
  !japan_preservation$code_unchanged
)

n_japan_name_changes <- sum(
  !japan_preservation$name_unchanged
)

n_japan_resolution_changes <- sum(
  !japan_preservation$resolution_status_unchanged
)

n_japan_identifier_changes <- sum(
  !japan_preservation$identifier_status_unchanged
)


# ------------------------------------------------------------------------------
# 27. CONCEPT STRUCTURE PROFILE
# ------------------------------------------------------------------------------

structure_profile <- build_profile(
  integrated_group,
  "concept_group"
)


# ------------------------------------------------------------------------------
# 28. RESOLUTION STATUS PROFILE
# ------------------------------------------------------------------------------

resolution_profile <- build_profile(
  integrated_resolution,
  "concept_resolution_status"
)


# ------------------------------------------------------------------------------
# 29. IDENTIFIER STATUS PROFILE
# ------------------------------------------------------------------------------

identifier_profile <- build_profile(
  integrated_identifier_status,
  "identifier_status"
)


# ------------------------------------------------------------------------------
# 30. SUMMARY
# ------------------------------------------------------------------------------

summary_table <- data.frame(
  
  metric = c(
    "canonical_geographic_concepts",
    "japanese_botanical_area_concepts",
    "wcvp_tdwg_concepts",
    "unrecognised_concept_groups",
    "missing_geographic_unit_ids",
    "missing_geographic_codes",
    "missing_geographic_names",
    "missing_concept_groups",
    "missing_resolution_statuses",
    "missing_identifier_statuses",
    "duplicate_geographic_unit_ids",
    "duplicate_geographic_codes",
    "japanese_concepts_resolved",
    "wcvp_concepts_resolved",
    "unexpected_resolution_states",
    "provisional_identifiers",
    "nonprovisional_identifiers",
    "wcvp_names_present",
    "wcvp_location_ids_present",
    "wcvp_name_sources_present",
    "wcvp_validated_name_statuses",
    "japan_wcvp_location_ids_present",
    "japan_wcvp_name_sources_present",
    "japan_wcvp_validation_statuses_present",
    "validated_crosswalk_rows",
    "validated_crosswalk_distinct_codes",
    "validated_crosswalk_distinct_location_ids",
    "wcvp_crosswalk_missing_matches",
    "wcvp_name_mismatches",
    "wcvp_location_id_mismatches",
    "wcvp_source_mismatches",
    "wcvp_validation_status_mismatches",
    "integrated_wcvp_codes_not_in_crosswalk",
    "crosswalk_codes_not_in_integrated",
    "preintegration_missing_matches",
    "integrated_ids_not_in_preintegration",
    "preintegration_ids_not_in_integrated",
    "geographic_code_changes_since_06b2",
    "concept_group_changes_since_06b2",
    "identifier_status_changes_since_06b2",
    "japan_missing_preintegration_matches",
    "japan_code_changes",
    "japan_name_changes",
    "japan_resolution_status_changes",
    "japan_identifier_status_changes"
  ),
  
  value = c(
    n_total,
    n_japan,
    n_wcvp,
    n_unrecognised_groups,
    n_missing_ids,
    n_missing_codes,
    n_missing_names,
    n_missing_groups,
    n_missing_resolution,
    n_missing_identifier_status,
    n_duplicate_ids,
    n_duplicate_codes,
    n_japan_resolved,
    n_wcvp_resolved,
    n_unexpected_resolution,
    n_provisional_identifiers,
    n_nonprovisional_identifiers,
    n_wcvp_names,
    n_wcvp_location_ids,
    n_wcvp_sources,
    n_wcvp_validated_names,
    n_japan_wcvp_location_ids,
    n_japan_wcvp_name_sources,
    n_japan_wcvp_validation_statuses,
    n_crosswalk_rows,
    n_crosswalk_codes,
    n_crosswalk_location_ids,
    n_wcvp_crosswalk_missing,
    n_wcvp_name_mismatches,
    n_wcvp_location_id_mismatches,
    n_wcvp_source_mismatches,
    n_wcvp_validation_mismatches,
    length(integrated_codes_not_in_crosswalk),
    length(crosswalk_codes_not_in_integrated),
    n_preintegration_missing,
    length(integrated_ids_not_in_pre),
    length(pre_ids_not_in_integrated),
    n_code_changes,
    n_group_changes,
    n_identifier_status_changes,
    n_japan_missing_pre,
    n_japan_code_changes,
    n_japan_name_changes,
    n_japan_resolution_changes,
    n_japan_identifier_changes
  ),
  
  stringsAsFactors = FALSE
)


# ------------------------------------------------------------------------------
# 31. STRUCTURAL / EVIDENCE VALIDATION GATE
# ------------------------------------------------------------------------------

validation_gate <- data.frame(
  
  criterion = c(
    "06b3b_validation_passed",
    "canonical_cardinality_418",
    "japanese_botanical_area_count_51",
    "wcvp_tdwg_count_367",
    "no_unrecognised_concept_groups",
    "geographic_unit_ids_complete",
    "geographic_unit_ids_unique",
    "geographic_codes_complete",
    "geographic_codes_unique",
    "geographic_names_complete",
    "concept_groups_complete",
    "resolution_statuses_complete",
    "identifier_statuses_complete",
    "all_japanese_concepts_resolved",
    "all_wcvp_concepts_resolved",
    "no_unexpected_resolution_states",
    "all_identifiers_still_provisional",
    "no_identifiers_already_frozen",
    "all_wcvp_names_present",
    "all_wcvp_location_ids_present",
    "all_wcvp_name_sources_present",
    "all_wcvp_name_statuses_validated",
    "japanese_concepts_have_no_wcvp_location_ids",
    "japanese_concepts_have_no_wcvp_name_sources",
    "japanese_concepts_have_no_wcvp_validation_statuses",
    "validated_crosswalk_cardinality_367",
    "validated_crosswalk_codes_unique_367",
    "validated_crosswalk_location_ids_unique_367",
    "all_wcvp_concepts_match_crosswalk",
    "wcvp_names_match_crosswalk",
    "wcvp_location_ids_match_crosswalk",
    "wcvp_sources_match_crosswalk",
    "wcvp_validation_statuses_match_crosswalk",
    "integrated_and_crosswalk_code_sets_identical",
    "preintegration_id_set_preserved",
    "all_integrated_concepts_match_preintegration",
    "geographic_codes_preserved_since_06b2",
    "concept_groups_preserved_since_06b2",
    "identifier_statuses_preserved_since_06b2",
    "all_japanese_concepts_match_preintegration",
    "japanese_codes_preserved",
    "japanese_names_preserved",
    "japanese_resolution_statuses_preserved",
    "japanese_identifier_statuses_preserved",
    "integrated_vocabulary_not_modified",
    "published_vpjd_not_modified"
  ),
  
  passed = c(
    all(validation_06b3b$passed),
    n_total == 418L,
    n_japan == 51L,
    n_wcvp == 367L,
    n_unrecognised_groups == 0L,
    n_missing_ids == 0L,
    n_duplicate_ids == 0L,
    n_missing_codes == 0L,
    n_duplicate_codes == 0L,
    n_missing_names == 0L,
    n_missing_groups == 0L,
    n_missing_resolution == 0L,
    n_missing_identifier_status == 0L,
    n_japan_resolved == 51L,
    n_wcvp_resolved == 367L,
    n_unexpected_resolution == 0L,
    n_provisional_identifiers == 418L,
    n_nonprovisional_identifiers == 0L,
    n_wcvp_names == 367L,
    n_wcvp_location_ids == 367L,
    n_wcvp_sources == 367L,
    n_wcvp_validated_names == 367L,
    n_japan_wcvp_location_ids == 0L,
    n_japan_wcvp_name_sources == 0L,
    n_japan_wcvp_validation_statuses == 0L,
    n_crosswalk_rows == 367L,
    n_crosswalk_codes == 367L,
    n_crosswalk_location_ids == 367L,
    n_wcvp_crosswalk_missing == 0L,
    n_wcvp_name_mismatches == 0L,
    n_wcvp_location_id_mismatches == 0L,
    n_wcvp_source_mismatches == 0L,
    n_wcvp_validation_mismatches == 0L,
    length(integrated_codes_not_in_crosswalk) == 0L &&
      length(crosswalk_codes_not_in_integrated) == 0L,
    length(integrated_ids_not_in_pre) == 0L &&
      length(pre_ids_not_in_integrated) == 0L,
    n_preintegration_missing == 0L,
    n_code_changes == 0L,
    n_group_changes == 0L,
    n_identifier_status_changes == 0L,
    n_japan_missing_pre == 0L,
    n_japan_code_changes == 0L,
    n_japan_name_changes == 0L,
    n_japan_resolution_changes == 0L,
    n_japan_identifier_changes == 0L,
    TRUE,
    TRUE
  ),
  
  stringsAsFactors = FALSE
)


# ------------------------------------------------------------------------------
# 32. RELEASE-READINESS / IDENTIFIER-FREEZE ELIGIBILITY GATE
#
# This is intentionally separate from the structural validation gate.
#
# PASS means:
#
#   the current vocabulary is eligible to proceed to a deliberate freeze stage.
#
# PASS DOES NOT mean:
#
#   identifiers have been frozen.
# ------------------------------------------------------------------------------

release_readiness_gate <- data.frame(
  
  criterion = c(
    "structural_and_evidence_validation_passed",
    "canonical_vocabulary_complete",
    "canonical_vocabulary_fully_named",
    "canonical_vocabulary_fully_resolved",
    "canonical_identifiers_unique_and_complete",
    "canonical_codes_unique_and_complete",
    "wcvp_provenance_complete",
    "wcvp_crosswalk_reconciliation_complete",
    "preintegration_identifier_set_preserved",
    "japanese_botanical_areas_preserved",
    "all_identifiers_currently_provisional",
    "identifier_freeze_not_yet_performed",
    "eligible_for_deliberate_identifier_freeze"
  ),
  
  passed = c(
    all(validation_gate$passed),
    
    n_total == 418L &&
      n_japan == 51L &&
      n_wcvp == 367L,
    
    n_missing_names == 0L,
    
    n_japan_resolved == 51L &&
      n_wcvp_resolved == 367L &&
      n_unexpected_resolution == 0L,
    
    n_missing_ids == 0L &&
      n_duplicate_ids == 0L,
    
    n_missing_codes == 0L &&
      n_duplicate_codes == 0L,
    
    n_wcvp_location_ids == 367L &&
      n_wcvp_sources == 367L &&
      n_wcvp_validated_names == 367L,
    
    n_wcvp_crosswalk_missing == 0L &&
      n_wcvp_name_mismatches == 0L &&
      n_wcvp_location_id_mismatches == 0L &&
      n_wcvp_source_mismatches == 0L &&
      n_wcvp_validation_mismatches == 0L,
    
    length(integrated_ids_not_in_pre) == 0L &&
      length(pre_ids_not_in_integrated) == 0L,
    
    n_japan_missing_pre == 0L &&
      n_japan_code_changes == 0L &&
      n_japan_name_changes == 0L &&
      n_japan_resolution_changes == 0L &&
      n_japan_identifier_changes == 0L,
    
    n_provisional_identifiers == 418L &&
      n_nonprovisional_identifiers == 0L,
    
    n_nonprovisional_identifiers == 0L,
    
    all(validation_gate$passed) &&
      n_total == 418L &&
      n_missing_ids == 0L &&
      n_duplicate_ids == 0L &&
      n_missing_codes == 0L &&
      n_duplicate_codes == 0L &&
      n_missing_names == 0L &&
      n_japan_resolved == 51L &&
      n_wcvp_resolved == 367L &&
      n_wcvp_crosswalk_missing == 0L &&
      n_wcvp_name_mismatches == 0L &&
      n_wcvp_location_id_mismatches == 0L &&
      n_wcvp_source_mismatches == 0L &&
      n_wcvp_validation_mismatches == 0L &&
      n_provisional_identifiers == 418L &&
      n_nonprovisional_identifiers == 0L
  ),
  
  stringsAsFactors = FALSE
)


# ------------------------------------------------------------------------------
# 33. WRITE AUDIT OUTPUTS
# ------------------------------------------------------------------------------

write.csv(
  summary_table,
  SUMMARY_FILE,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)

write.csv(
  structure_profile,
  STRUCTURE_PROFILE_FILE,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)

write.csv(
  resolution_profile,
  RESOLUTION_PROFILE_FILE,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)

write.csv(
  identifier_profile,
  IDENTIFIER_PROFILE_FILE,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)

write.csv(
  wcvp_reconciliation,
  WCVP_RECONCILIATION_FILE,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)

write.csv(
  preintegration_reconciliation,
  PREINTEGRATION_RECONCILIATION_FILE,
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
  validation_gate,
  VALIDATION_GATE_FILE,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)

write.csv(
  release_readiness_gate,
  RELEASE_READINESS_FILE,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)


# ------------------------------------------------------------------------------
# 34. CONSOLE HEADER
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" VPJD GEOGRAPHY 06b4 - INTEGRATED VOCABULARY VALIDATION\n")
cat("============================================================\n\n")


# ------------------------------------------------------------------------------
# 35. SUMMARY
# ------------------------------------------------------------------------------

cat("Summary:\n\n")

print(
  summary_table,
  row.names = FALSE
)


# ------------------------------------------------------------------------------
# 36. CONCEPT STRUCTURE
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" CONCEPT STRUCTURE\n")
cat("============================================================\n\n")

print(
  structure_profile,
  row.names = FALSE
)


# ------------------------------------------------------------------------------
# 37. RESOLUTION STATUS
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" RESOLUTION STATUS\n")
cat("============================================================\n\n")

print(
  resolution_profile,
  row.names = FALSE
)


# ------------------------------------------------------------------------------
# 38. IDENTIFIER STATUS
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" IDENTIFIER STATUS\n")
cat("============================================================\n\n")

print(
  identifier_profile,
  row.names = FALSE
)


# ------------------------------------------------------------------------------
# 39. STRUCTURAL / EVIDENCE VALIDATION GATE
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" STRUCTURAL / EVIDENCE VALIDATION GATE\n")
cat("============================================================\n\n")

print(
  validation_gate,
  row.names = FALSE
)


# ------------------------------------------------------------------------------
# 40. RELEASE-READINESS GATE
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" RELEASE-READINESS / IDENTIFIER-FREEZE ELIGIBILITY\n")
cat("============================================================\n\n")

print(
  release_readiness_gate,
  row.names = FALSE
)


# ------------------------------------------------------------------------------
# 41. INTERPRETATION
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" INTERPRETATION\n")
cat("============================================================\n\n")


structural_pass <- all(
  validation_gate$passed
)

release_ready <- all(
  release_readiness_gate$passed
)


if (structural_pass && release_ready) {
  
  cat(
    paste0(
      "PASS. The integrated VPJD Geography canonical geographic vocabulary ",
      "has passed independent structural, evidence, provenance and stability ",
      "validation.\n\n"
    )
  )
  
  cat(
    paste0(
      "The vocabulary contains exactly 418 geographic concepts: 51 Japanese ",
      "botanical areas and 367 WCVP/TDWG geographic concepts. All concepts ",
      "are named and resolved, and all geographic_unit_id and geographic_code ",
      "values are complete and unique.\n\n"
    )
  )
  
  cat(
    paste0(
      "The 367 WCVP/TDWG concepts reconcile completely against the validated ",
      "06b3a WCVP locality crosswalk, including locality names, WCVP location ",
      "identifiers, provenance and validation status.\n\n"
    )
  )
  
  cat(
    paste0(
      "The integrated identifier set is unchanged from the 06b2 provisional ",
      "vocabulary, and all 51 Japanese botanical-area concepts remain ",
      "unchanged.\n\n"
    )
  )
  
  cat(
    paste0(
      "All 418 identifiers remain PROVISIONAL_06B2. This validation therefore ",
      "authorises progression to a separate deliberate identifier-freeze ",
      "stage; it does NOT itself freeze any identifier.\n"
    )
  )
  
} else {
  
  cat(
    "FAIL. The integrated vocabulary is not yet eligible for identifier freeze.\n\n"
  )
  
  failed_validation <- validation_gate$criterion[
    !validation_gate$passed
  ]
  
  failed_readiness <- release_readiness_gate$criterion[
    !release_readiness_gate$passed
  ]
  
  if (length(failed_validation) > 0L) {
    
    cat("Failed structural/evidence criteria:\n\n")
    
    for (criterion_name in failed_validation) {
      
      cat(
        " - ",
        criterion_name,
        "\n",
        sep = ""
      )
    }
    
    cat("\n")
  }
  
  if (length(failed_readiness) > 0L) {
    
    cat("Failed release-readiness criteria:\n\n")
    
    for (criterion_name in failed_readiness) {
      
      cat(
        " - ",
        criterion_name,
        "\n",
        sep = ""
      )
    }
  }
}


# ------------------------------------------------------------------------------
# 42. NEXT STEP
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" NEXT STEP\n")
cat("============================================================\n\n")


if (structural_pass && release_ready) {
  
  cat(
    paste0(
      "The canonical geographic vocabulary is eligible for deliberate ",
      "identifier freeze.\n\n",
      "Recommended next script:\n\n",
      "  geography_06b5_freeze_canonical_geographic_vocabulary.R\n\n",
      "06b5 should create a new frozen release-candidate product rather than ",
      "modifying the 06b3b integrated vocabulary in place. It should preserve ",
      "all 418 geographic_unit_id values exactly, change identifier_status ",
      "only through an explicit controlled freeze operation, record the freeze ",
      "provenance, and validate that no concept content changed during freeze.\n\n",
      "Do not yet build the public taxon x geographic-unit distribution product ",
      "until the identifier freeze has passed its own validation gate.\n"
    )
  )
  
} else {
  
  cat(
    paste0(
      "Do not freeze VPJD-GEO identifiers.\n",
      "Resolve the failed 06b4 criterion or criteria first.\n"
    )
  )
}


# ------------------------------------------------------------------------------
# 43. OUTPUT LOCATIONS
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" OUTPUTS\n")
cat("============================================================\n\n")


cat(
  "Summary:\n",
  SUMMARY_FILE,
  "\n\n",
  sep = ""
)

cat(
  "Concept-structure profile:\n",
  STRUCTURE_PROFILE_FILE,
  "\n\n",
  sep = ""
)

cat(
  "Resolution-status profile:\n",
  RESOLUTION_PROFILE_FILE,
  "\n\n",
  sep = ""
)

cat(
  "Identifier-status profile:\n",
  IDENTIFIER_PROFILE_FILE,
  "\n\n",
  sep = ""
)

cat(
  "WCVP crosswalk reconciliation:\n",
  WCVP_RECONCILIATION_FILE,
  "\n\n",
  sep = ""
)

cat(
  "Pre-integration reconciliation:\n",
  PREINTEGRATION_RECONCILIATION_FILE,
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
  "Structural/evidence validation gate:\n",
  VALIDATION_GATE_FILE,
  "\n\n",
  sep = ""
)

cat(
  "Release-readiness gate:\n",
  RELEASE_READINESS_FILE,
  "\n\n",
  sep = ""
)


# ------------------------------------------------------------------------------
# 44. SAFEGUARDS
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" SAFEGUARDS\n")
cat("============================================================\n\n")

cat("Integrated canonical geographic vocabulary was NOT modified.\n")
cat("06b2 provisional geographic vocabulary was NOT modified.\n")
cat("06b3a validated WCVP crosswalk was NOT modified.\n")
cat("Existing VPJD-GEO identifiers were NOT changed.\n")
cat("VPJD-GEO identifiers were NOT frozen.\n")
cat("Identifier status was NOT changed.\n")
cat("Geographic codes were NOT changed.\n")
cat("Geographic names were NOT changed or inferred.\n")
cat("Japanese botanical-area concepts were NOT modified.\n")
cat("WCVP locality evidence was NOT modified.\n")
cat("Geographic hierarchy was NOT inferred.\n")
cat("Geographic geometry was NOT inferred.\n")
cat("Missing geographic evidence was NOT interpreted as absence.\n")
cat("Nakamura predicates were NOT evaluated.\n")
cat("Star categories were NOT evaluated.\n")
cat("Published VPJD Taxonomic Release v1.0.0 was NOT modified.\n")
cat("VPJD Geography v1.0.0 was NOT frozen or published.\n")

cat("\n")
cat("============================================================\n")