# ==============================================================================
# VPJD GEOGRAPHY MODULE
# geography_06b0_audit_candidate_geographic_fields.R
#
# PURPOSE
# -------
# Audit the candidate geographic fields admitted by:
#
#   geography_06b_build_canonical_geographic_unit_reference.R
#
# before validating or freezing the canonical geographic-unit vocabulary for:
#
#   Vascular Plants of Japan Database (VPJD)
#   Geography Release v1.0.0
#
# BACKGROUND
# ----------
# geography_06b produced:
#
#   1,142 provisional canonical geographic units
#
# including:
#
#     102 BOTANICAL_AREA
#     367 WCVP_TDWG_UNIT
#       1 PROVINCE
#     672 UNRESOLVED
#
# Inspection of the unresolved units showed many numeric-only values, indicating
# that one or more fields identified as geography candidates are probably IDs,
# codes, counts, or other non-geographic attributes.
#
# OBJECTIVES
# ----------
#  1. Read the exact candidate fields selected by 06b.
#  2. Read the exact raw geographic-unit values extracted by 06b.
#  3. Profile every source-file × source-field combination.
#  4. Quantify:
#
#       - distinct values;
#       - numeric-only values;
#       - integer-like values;
#       - decimal-like values;
#       - alphabetic values;
#       - mixed alphanumeric values;
#       - short code-like values;
#       - long numeric identifier-like values;
#       - inferred geographic-unit types;
#       - contribution to the 06b canonical vocabulary.
#
#  5. Generate representative sample values for every candidate field.
#  6. Identify which fields generated UNRESOLVED units.
#  7. Flag probable false-positive geographic fields conservatively.
#  8. Produce a recommended field disposition:
#
#       RETAIN
#       REVIEW
#       EXCLUDE_PROBABLE_NON_GEOGRAPHIC
#
#  9. Produce an exclusion candidate table for inspection.
# 10. Do NOT alter the 06b outputs.
#
# IMPORTANT
# ---------
# This is an AUDIT script.
#
# It DOES NOT:
#
#   - modify source evidence;
#   - modify the 06b candidate-field table;
#   - modify the 06b raw-unit table;
#   - modify the 06b canonical-unit table;
#   - modify the 06b source crosswalk;
#   - freeze VPJD-GEO identifiers;
#   - automatically exclude fields from VPJD Geography;
#   - infer geographic equivalence;
#   - infer geographic hierarchy;
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
  "06b0_candidate_field_audit"
)

dir.create(
  AUDIT_ROOT,
  recursive = TRUE,
  showWarnings = FALSE
)


# ------------------------------------------------------------------------------
# 02. REQUIRED 06b INPUT FILES
# ------------------------------------------------------------------------------

CANDIDATE_FIELD_FILE <- file.path(
  AUDIT_06B_ROOT,
  "geography_06b_candidate_geographic_fields.csv"
)

RAW_UNIT_FILE <- file.path(
  AUDIT_06B_ROOT,
  "geography_06b_raw_geographic_units.csv"
)

NORMALISED_UNIT_FILE <- file.path(
  AUDIT_06B_ROOT,
  "geography_06b_normalised_geographic_units.csv"
)

VALIDATION_06B_FILE <- file.path(
  AUDIT_06B_ROOT,
  "geography_06b_validation_gate.csv"
)

CANONICAL_UNIT_FILE <- file.path(
  DERIVED_06B_ROOT,
  "vpjd_geographic_units.csv"
)

SOURCE_CROSSWALK_FILE <- file.path(
  DERIVED_06B_ROOT,
  "vpjd_geographic_unit_source_crosswalk.csv"
)


# ------------------------------------------------------------------------------
# 03. OUTPUT FILES
# ------------------------------------------------------------------------------

FIELD_PROFILE_FILE <- file.path(
  AUDIT_ROOT,
  "geography_06b0_candidate_field_profile.csv"
)

FIELD_SAMPLE_FILE <- file.path(
  AUDIT_ROOT,
  "geography_06b0_candidate_field_samples.csv"
)

UNRESOLVED_SOURCE_FILE <- file.path(
  AUDIT_ROOT,
  "geography_06b0_unresolved_unit_source_profile.csv"
)

NUMERIC_FIELD_FILE <- file.path(
  AUDIT_ROOT,
  "geography_06b0_numeric_candidate_fields.csv"
)

EXCLUSION_CANDIDATE_FILE <- file.path(
  AUDIT_ROOT,
  "geography_06b0_probable_false_positive_fields.csv"
)

RETAIN_CANDIDATE_FILE <- file.path(
  AUDIT_ROOT,
  "geography_06b0_probable_geographic_fields.csv"
)

REVIEW_FIELD_FILE <- file.path(
  AUDIT_ROOT,
  "geography_06b0_fields_requiring_review.csv"
)

SUMMARY_FILE <- file.path(
  AUDIT_ROOT,
  "geography_06b0_summary.csv"
)

VALIDATION_FILE <- file.path(
  AUDIT_ROOT,
  "geography_06b0_validation_gate.csv"
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
# 05. HELPER: COLLAPSE UNIQUE VALUES
# ------------------------------------------------------------------------------

collapse_unique <- function(x, separator = " | ") {
  
  y <- normalise_text(x)
  
  y <- y[
    !is.na(y)
  ]
  
  y <- sort(
    unique(y)
  )
  
  if (length(y) == 0L) {
    return(NA_character_)
  }
  
  paste(
    y,
    collapse = separator
  )
}


# ------------------------------------------------------------------------------
# 06. HELPER: SAMPLE VALUES
# ------------------------------------------------------------------------------

sample_values <- function(x, n = 15L) {
  
  y <- normalise_text(x)
  
  y <- y[
    !is.na(y)
  ]
  
  y <- sort(
    unique(y)
  )
  
  if (length(y) == 0L) {
    return(NA_character_)
  }
  
  y <- head(
    y,
    n
  )
  
  paste(
    y,
    collapse = " | "
  )
}


# ------------------------------------------------------------------------------
# 07. HELPER: SAFE PROPORTION
# ------------------------------------------------------------------------------

safe_proportion <- function(selector) {
  
  selector <- as.logical(selector)
  
  selector[is.na(selector)] <- FALSE
  
  if (length(selector) == 0L) {
    return(NA_real_)
  }
  
  mean(selector)
}


# ------------------------------------------------------------------------------
# 08. HELPER: VALUE-SHAPE CLASSIFICATION
# ------------------------------------------------------------------------------

classify_value_shapes <- function(x) {
  
  y <- normalise_text(x)
  
  y <- y[
    !is.na(y)
  ]
  
  if (length(y) == 0L) {
    
    return(
      list(
        n_values = 0L,
        n_numeric_only = 0L,
        pct_numeric_only = NA_real_,
        n_integer_like = 0L,
        pct_integer_like = NA_real_,
        n_decimal_like = 0L,
        pct_decimal_like = NA_real_,
        n_alpha_present = 0L,
        pct_alpha_present = NA_real_,
        n_alpha_only = 0L,
        pct_alpha_only = NA_real_,
        n_alphanumeric = 0L,
        pct_alphanumeric = NA_real_,
        n_short_code_like = 0L,
        pct_short_code_like = NA_real_,
        n_long_numeric_id_like = 0L,
        pct_long_numeric_id_like = NA_real_,
        min_numeric_value = NA_real_,
        max_numeric_value = NA_real_
      )
    )
  }
  
  numeric_only <- grepl(
    "^[0-9]+$",
    y,
    perl = TRUE
  )
  
  integer_like <- grepl(
    "^[+-]?[0-9]+$",
    y,
    perl = TRUE
  )
  
  decimal_like <- grepl(
    "^[+-]?[0-9]+[.][0-9]+$",
    y,
    perl = TRUE
  )
  
  alpha_present <- grepl(
    "[[:alpha:]]",
    y,
    perl = TRUE
  )
  
  alpha_only <- grepl(
    "^[[:alpha:]][[:alpha:] .'-]*$",
    y,
    perl = TRUE
  )
  
  alphanumeric <- (
    grepl(
      "[[:alpha:]]",
      y,
      perl = TRUE
    ) &
      grepl(
        "[0-9]",
        y,
        perl = TRUE
      )
  )
  
  short_code_like <- grepl(
    "^[[:alnum:]_-]{1,6}$",
    y,
    perl = TRUE
  )
  
  long_numeric_id_like <- grepl(
    "^[0-9]{4,}$",
    y,
    perl = TRUE
  )
  
  numeric_conversion <- suppressWarnings(
    as.numeric(y)
  )
  
  finite_numeric <- numeric_conversion[
    is.finite(numeric_conversion)
  ]
  
  if (length(finite_numeric) > 0L) {
    
    min_numeric_value <- min(
      finite_numeric
    )
    
    max_numeric_value <- max(
      finite_numeric
    )
    
  } else {
    
    min_numeric_value <- NA_real_
    max_numeric_value <- NA_real_
  }
  
  list(
    n_values = length(y),
    
    n_numeric_only = sum(
      numeric_only,
      na.rm = TRUE
    ),
    
    pct_numeric_only = safe_proportion(
      numeric_only
    ),
    
    n_integer_like = sum(
      integer_like,
      na.rm = TRUE
    ),
    
    pct_integer_like = safe_proportion(
      integer_like
    ),
    
    n_decimal_like = sum(
      decimal_like,
      na.rm = TRUE
    ),
    
    pct_decimal_like = safe_proportion(
      decimal_like
    ),
    
    n_alpha_present = sum(
      alpha_present,
      na.rm = TRUE
    ),
    
    pct_alpha_present = safe_proportion(
      alpha_present
    ),
    
    n_alpha_only = sum(
      alpha_only,
      na.rm = TRUE
    ),
    
    pct_alpha_only = safe_proportion(
      alpha_only
    ),
    
    n_alphanumeric = sum(
      alphanumeric,
      na.rm = TRUE
    ),
    
    pct_alphanumeric = safe_proportion(
      alphanumeric
    ),
    
    n_short_code_like = sum(
      short_code_like,
      na.rm = TRUE
    ),
    
    pct_short_code_like = safe_proportion(
      short_code_like
    ),
    
    n_long_numeric_id_like = sum(
      long_numeric_id_like,
      na.rm = TRUE
    ),
    
    pct_long_numeric_id_like = safe_proportion(
      long_numeric_id_like
    ),
    
    min_numeric_value = min_numeric_value,
    
    max_numeric_value = max_numeric_value
  )
}


# ------------------------------------------------------------------------------
# 09. HELPER: FIELD-NAME SIGNALS
# ------------------------------------------------------------------------------

classify_field_name <- function(field_name) {
  
  x <- tolower(
    normalise_text(field_name)
  )
  
  if (
    length(x) == 0L ||
    is.na(x)
  ) {
    
    return(
      list(
        geographic_name_signal = FALSE,
        identifier_name_signal = FALSE,
        count_name_signal = FALSE,
        coordinate_name_signal = FALSE
      )
    )
  }
  
  geographic_terms <- c(
    "geograph",
    "location",
    "botanical",
    "prefecture",
    "province",
    "district",
    "country",
    "territory",
    "region",
    "island",
    "tdwg",
    "wcvp"
  )
  
  identifier_terms <- c(
    "id",
    "key",
    "identifier",
    "code",
    "number",
    "no."
  )
  
  count_terms <- c(
    "count",
    "n_",
    "number",
    "total",
    "frequency"
  )
  
  coordinate_terms <- c(
    "latitude",
    "longitude",
    "lat",
    "lon",
    "x_coord",
    "y_coord"
  )
  
  geographic_signal <- FALSE
  identifier_signal <- FALSE
  count_signal <- FALSE
  coordinate_signal <- FALSE
  
  for (term in geographic_terms) {
    
    if (
      grepl(
        term,
        x,
        fixed = TRUE
      )
    ) {
      
      geographic_signal <- TRUE
      break
    }
  }
  
  for (term in identifier_terms) {
    
    if (
      grepl(
        term,
        x,
        fixed = TRUE
      )
    ) {
      
      identifier_signal <- TRUE
      break
    }
  }
  
  for (term in count_terms) {
    
    if (
      grepl(
        term,
        x,
        fixed = TRUE
      )
    ) {
      
      count_signal <- TRUE
      break
    }
  }
  
  for (term in coordinate_terms) {
    
    if (
      grepl(
        term,
        x,
        fixed = TRUE
      )
    ) {
      
      coordinate_signal <- TRUE
      break
    }
  }
  
  list(
    geographic_name_signal = geographic_signal,
    identifier_name_signal = identifier_signal,
    count_name_signal = count_signal,
    coordinate_name_signal = coordinate_signal
  )
}


# ------------------------------------------------------------------------------
# 10. VALIDATE REQUIRED INPUT FILES
# ------------------------------------------------------------------------------

required_inputs <- c(
  CANDIDATE_FIELD_FILE,
  RAW_UNIT_FILE,
  NORMALISED_UNIT_FILE,
  VALIDATION_06B_FILE,
  CANONICAL_UNIT_FILE,
  SOURCE_CROSSWALK_FILE
)

missing_inputs <- required_inputs[
  !file.exists(required_inputs)
]

if (length(missing_inputs) > 0L) {
  
  stop(
    paste0(
      "Required 06b input file(s) not found:\n",
      paste(
        missing_inputs,
        collapse = "\n"
      )
    )
  )
}


# ------------------------------------------------------------------------------
# 11. LOAD 06b PRODUCTS
# ------------------------------------------------------------------------------

candidate_fields <- read.csv(
  CANDIDATE_FIELD_FILE,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

raw_units <- read.csv(
  RAW_UNIT_FILE,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

normalised_units <- read.csv(
  NORMALISED_UNIT_FILE,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

validation_06b <- read.csv(
  VALIDATION_06B_FILE,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

canonical_units <- read.csv(
  CANONICAL_UNIT_FILE,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

source_crosswalk <- read.csv(
  SOURCE_CROSSWALK_FILE,
  stringsAsFactors = FALSE,
  check.names = FALSE
)


# ------------------------------------------------------------------------------
# 12. VALIDATE 06b GATE
# ------------------------------------------------------------------------------

if (
  !"passed" %in%
  names(validation_06b)
) {
  
  stop(
    "06b validation table does not contain field 'passed'."
  )
}

validation_06b$passed <- as.logical(
  validation_06b$passed
)

if (
  any(is.na(validation_06b$passed)) ||
  !all(validation_06b$passed)
) {
  
  stop(
    paste0(
      "06b validation gate did not pass completely.\n",
      "Do not audit the candidate geographic vocabulary until 06b is valid."
    )
  )
}


# ------------------------------------------------------------------------------
# 13. VALIDATE REQUIRED TABLE FIELDS
# ------------------------------------------------------------------------------

required_candidate_fields <- c(
  "source_file",
  "file_name",
  "product_class",
  "field_name",
  "inferred_unit_type"
)

missing_candidate_fields <- setdiff(
  required_candidate_fields,
  names(candidate_fields)
)

if (length(missing_candidate_fields) > 0L) {
  
  stop(
    paste0(
      "06b candidate-field table lacks required field(s):\n",
      paste(
        missing_candidate_fields,
        collapse = "\n"
      )
    )
  )
}


required_raw_fields <- c(
  "source_file",
  "file_name",
  "source_field",
  "source_value",
  "inferred_unit_type",
  "normalised_value",
  "normalised_unit_key"
)

missing_raw_fields <- setdiff(
  required_raw_fields,
  names(raw_units)
)

if (length(missing_raw_fields) > 0L) {
  
  stop(
    paste0(
      "06b raw-unit table lacks required field(s):\n",
      paste(
        missing_raw_fields,
        collapse = "\n"
      )
    )
  )
}


required_canonical_fields <- c(
  "geographic_unit_id",
  "geographic_unit_name",
  "geographic_unit_type",
  "normalised_geographic_unit_name",
  "release_status"
)

missing_canonical_fields <- setdiff(
  required_canonical_fields,
  names(canonical_units)
)

if (length(missing_canonical_fields) > 0L) {
  
  stop(
    paste0(
      "06b canonical-unit table lacks required field(s):\n",
      paste(
        missing_canonical_fields,
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
  "inferred_unit_type",
  "normalised_value",
  "normalised_unit_key"
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


# ------------------------------------------------------------------------------
# 14. START
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" VPJD GEOGRAPHY 06b0 - CANDIDATE FIELD AUDIT\n")
cat("============================================================\n\n")

cat(
  "Candidate fields from 06b: ",
  format(
    nrow(candidate_fields),
    big.mark = ","
  ),
  "\n",
  sep = ""
)

cat(
  "Raw geographic-unit records from 06b: ",
  format(
    nrow(raw_units),
    big.mark = ","
  ),
  "\n",
  sep = ""
)

cat(
  "Provisional canonical units from 06b: ",
  format(
    nrow(canonical_units),
    big.mark = ","
  ),
  "\n\n",
  sep = ""
)


# ------------------------------------------------------------------------------
# 15. BUILD UNIQUE FIELD KEYS
#
# source_file + field_name is the audit unit.
# ------------------------------------------------------------------------------

candidate_fields$field_key <- paste(
  candidate_fields$source_file,
  candidate_fields$field_name,
  sep = "::FIELD::"
)

raw_units$field_key <- paste(
  raw_units$source_file,
  raw_units$source_field,
  sep = "::FIELD::"
)

source_crosswalk$field_key <- paste(
  source_crosswalk$source_file,
  source_crosswalk$source_field,
  sep = "::FIELD::"
)


field_keys <- unique(
  candidate_fields$field_key
)


# ------------------------------------------------------------------------------
# 16. PROFILE EACH CANDIDATE FIELD
# ------------------------------------------------------------------------------

field_profile_records <- list()

field_profile_counter <- 0L

field_sample_records <- list()

field_sample_counter <- 0L


if (length(field_keys) > 0L) {
  
  for (
    field_index in seq_along(
      field_keys
    )
  ) {
    
    field_key <- field_keys[
      field_index
    ]
    
    candidate_row <- candidate_fields[
      candidate_fields$field_key == field_key,
      ,
      drop = FALSE
    ]
    
    candidate_row <- candidate_row[
      1L,
      ,
      drop = FALSE
    ]
    
    source_file <- candidate_row$source_file[
      1L
    ]
    
    file_name <- candidate_row$file_name[
      1L
    ]
    
    field_name <- candidate_row$field_name[
      1L
    ]
    
    product_class <- candidate_row$product_class[
      1L
    ]
    
    inferred_unit_type <- candidate_row$inferred_unit_type[
      1L
    ]
    
    field_raw <- raw_units[
      raw_units$field_key == field_key,
      ,
      drop = FALSE
    ]
    
    field_crosswalk <- source_crosswalk[
      source_crosswalk$field_key == field_key,
      ,
      drop = FALSE
    ]
    
    distinct_values <- sort(
      unique(
        normalise_text(
          field_raw$source_value
        )
      )
    )
    
    distinct_values <- distinct_values[
      !is.na(distinct_values)
    ]
    
    distinct_normalised_values <- sort(
      unique(
        normalise_text(
          field_raw$normalised_value
        )
      )
    )
    
    distinct_normalised_values <- distinct_normalised_values[
      !is.na(distinct_normalised_values)
    ]
    
    distinct_unit_ids <- sort(
      unique(
        normalise_text(
          field_crosswalk$geographic_unit_id
        )
      )
    )
    
    distinct_unit_ids <- distinct_unit_ids[
      !is.na(distinct_unit_ids)
    ]
    
    shape_profile <- classify_value_shapes(
      distinct_values
    )
    
    name_profile <- classify_field_name(
      field_name
    )
    
    n_unresolved_unit_ids <- 0L
    
    n_resolved_unit_ids <- 0L
    
    unresolved_unit_ids <- character(0)
    
    if (length(distinct_unit_ids) > 0L) {
      
      unit_rows <- canonical_units[
        canonical_units$geographic_unit_id %in%
          distinct_unit_ids,
        ,
        drop = FALSE
      ]
      
      unresolved_selector <- (
        unit_rows$geographic_unit_type ==
          "UNRESOLVED"
      )
      
      unresolved_selector[
        is.na(unresolved_selector)
      ] <- FALSE
      
      unresolved_unit_ids <- unique(
        unit_rows$geographic_unit_id[
          unresolved_selector
        ]
      )
      
      n_unresolved_unit_ids <- length(
        unresolved_unit_ids
      )
      
      n_resolved_unit_ids <- length(
        unique(
          unit_rows$geographic_unit_id[
            !unresolved_selector
          ]
        )
      )
    }
    
    pct_unresolved_units <- if (
      length(distinct_unit_ids) > 0L
    ) {
      
      n_unresolved_unit_ids /
        length(distinct_unit_ids)
      
    } else {
      
      NA_real_
    }
    
    
    # --------------------------------------------------------------------------
    # Conservative disposition logic
    #
    # This does NOT alter any data.
    #
    # Strong probable false-positive signals:
    #
    #   - all values numeric and inferred type unresolved;
    #   - overwhelmingly numeric and inferred type unresolved;
    #   - coordinate/count field-name signal;
    #   - all generated canonical units unresolved AND values are strongly
    #     identifier-like.
    #
    # RETAIN requires stronger positive geographic evidence.
    # Everything else remains REVIEW.
    # --------------------------------------------------------------------------
    
    probable_non_geographic <- FALSE
    
    probable_geographic <- FALSE
    
    disposition_reason <- character(0)
    
    
    if (
      isTRUE(
        name_profile$coordinate_name_signal
      )
    ) {
      
      probable_non_geographic <- TRUE
      
      disposition_reason <- c(
        disposition_reason,
        "field name indicates coordinate"
      )
    }
    
    
    if (
      isTRUE(
        name_profile$count_name_signal
      )
    ) {
      
      probable_non_geographic <- TRUE
      
      disposition_reason <- c(
        disposition_reason,
        "field name indicates count/total"
      )
    }
    
    
    if (
      identical(
        inferred_unit_type,
        "UNRESOLVED"
      ) &&
      !is.na(
        shape_profile$pct_numeric_only
      ) &&
      shape_profile$pct_numeric_only == 1
    ) {
      
      probable_non_geographic <- TRUE
      
      disposition_reason <- c(
        disposition_reason,
        "all distinct values numeric-only and unit type unresolved"
      )
    }
    
    
    if (
      identical(
        inferred_unit_type,
        "UNRESOLVED"
      ) &&
      !is.na(
        shape_profile$pct_numeric_only
      ) &&
      shape_profile$pct_numeric_only >= 0.90
    ) {
      
      probable_non_geographic <- TRUE
      
      disposition_reason <- c(
        disposition_reason,
        "at least 90% of distinct values numeric-only"
      )
    }
    
    
    if (
      identical(
        inferred_unit_type,
        "UNRESOLVED"
      ) &&
      !is.na(
        shape_profile$pct_long_numeric_id_like
      ) &&
      shape_profile$pct_long_numeric_id_like >= 0.50
    ) {
      
      probable_non_geographic <- TRUE
      
      disposition_reason <- c(
        disposition_reason,
        "numeric identifier-like value profile"
      )
    }
    
    
    if (
      !identical(
        inferred_unit_type,
        "UNRESOLVED"
      ) &&
      isTRUE(
        name_profile$geographic_name_signal
      ) &&
      !isTRUE(
        name_profile$coordinate_name_signal
      ) &&
      !isTRUE(
        name_profile$count_name_signal
      )
    ) {
      
      probable_geographic <- TRUE
      
      disposition_reason <- c(
        disposition_reason,
        "recognised geographic unit type and geographic field-name signal"
      )
    }
    
    
    if (
      !identical(
        inferred_unit_type,
        "UNRESOLVED"
      ) &&
      n_unresolved_unit_ids == 0L
    ) {
      
      probable_geographic <- TRUE
      
      disposition_reason <- c(
        disposition_reason,
        "all generated units have recognised geographic type"
      )
    }
    
    
    if (probable_non_geographic) {
      
      recommended_disposition <- "EXCLUDE_PROBABLE_NON_GEOGRAPHIC"
      
    } else if (probable_geographic) {
      
      recommended_disposition <- "RETAIN"
      
    } else {
      
      recommended_disposition <- "REVIEW"
    }
    
    
    if (length(disposition_reason) == 0L) {
      
      disposition_reason <- "insufficient evidence for automatic disposition"
      
    } else {
      
      disposition_reason <- paste(
        unique(disposition_reason),
        collapse = "; "
      )
    }
    
    
    # --------------------------------------------------------------------------
    # Profile record
    # --------------------------------------------------------------------------
    
    field_profile_counter <- field_profile_counter + 1L
    
    field_profile_records[[field_profile_counter]] <- data.frame(
      source_file = source_file,
      file_name = file_name,
      product_class = product_class,
      field_name = field_name,
      inferred_unit_type = inferred_unit_type,
      
      n_raw_unit_records = nrow(
        field_raw
      ),
      
      n_distinct_source_values = length(
        distinct_values
      ),
      
      n_distinct_normalised_values = length(
        distinct_normalised_values
      ),
      
      n_canonical_units_generated = length(
        distinct_unit_ids
      ),
      
      n_resolved_canonical_units = n_resolved_unit_ids,
      
      n_unresolved_canonical_units = n_unresolved_unit_ids,
      
      pct_unresolved_canonical_units = pct_unresolved_units,
      
      n_numeric_only = shape_profile$n_numeric_only,
      
      pct_numeric_only = shape_profile$pct_numeric_only,
      
      n_integer_like = shape_profile$n_integer_like,
      
      pct_integer_like = shape_profile$pct_integer_like,
      
      n_decimal_like = shape_profile$n_decimal_like,
      
      pct_decimal_like = shape_profile$pct_decimal_like,
      
      n_alpha_present = shape_profile$n_alpha_present,
      
      pct_alpha_present = shape_profile$pct_alpha_present,
      
      n_alpha_only = shape_profile$n_alpha_only,
      
      pct_alpha_only = shape_profile$pct_alpha_only,
      
      n_alphanumeric = shape_profile$n_alphanumeric,
      
      pct_alphanumeric = shape_profile$pct_alphanumeric,
      
      n_short_code_like = shape_profile$n_short_code_like,
      
      pct_short_code_like = shape_profile$pct_short_code_like,
      
      n_long_numeric_id_like = shape_profile$n_long_numeric_id_like,
      
      pct_long_numeric_id_like = shape_profile$pct_long_numeric_id_like,
      
      min_numeric_value = shape_profile$min_numeric_value,
      
      max_numeric_value = shape_profile$max_numeric_value,
      
      geographic_field_name_signal = name_profile$geographic_name_signal,
      
      identifier_field_name_signal = name_profile$identifier_name_signal,
      
      count_field_name_signal = name_profile$count_name_signal,
      
      coordinate_field_name_signal = name_profile$coordinate_name_signal,
      
      sample_values = sample_values(
        distinct_values,
        n = 15L
      ),
      
      recommended_disposition = recommended_disposition,
      
      disposition_reason = disposition_reason,
      
      stringsAsFactors = FALSE
    )
    
    
    # --------------------------------------------------------------------------
    # Long-format sample table
    # --------------------------------------------------------------------------
    
    if (length(distinct_values) > 0L) {
      
      sample_n <- min(
        25L,
        length(distinct_values)
      )
      
      sampled_values <- distinct_values[
        seq_len(sample_n)
      ]
      
      for (
        sample_index in seq_along(
          sampled_values
        )
      ) {
        
        sample_value <- sampled_values[
          sample_index
        ]
        
        field_sample_counter <- field_sample_counter + 1L
        
        field_sample_records[[field_sample_counter]] <- data.frame(
          source_file = source_file,
          file_name = file_name,
          field_name = field_name,
          inferred_unit_type = inferred_unit_type,
          sample_rank = sample_index,
          sample_value = sample_value,
          stringsAsFactors = FALSE
        )
      }
    }
  }
}


# ------------------------------------------------------------------------------
# 17. COMBINE FIELD PROFILES
# ------------------------------------------------------------------------------

if (length(field_profile_records) > 0L) {
  
  field_profile <- do.call(
    rbind,
    field_profile_records
  )
  
} else {
  
  field_profile <- data.frame(
    source_file = character(0),
    file_name = character(0),
    product_class = character(0),
    field_name = character(0),
    inferred_unit_type = character(0),
    n_raw_unit_records = integer(0),
    n_distinct_source_values = integer(0),
    n_distinct_normalised_values = integer(0),
    n_canonical_units_generated = integer(0),
    n_resolved_canonical_units = integer(0),
    n_unresolved_canonical_units = integer(0),
    pct_unresolved_canonical_units = numeric(0),
    n_numeric_only = integer(0),
    pct_numeric_only = numeric(0),
    n_integer_like = integer(0),
    pct_integer_like = numeric(0),
    n_decimal_like = integer(0),
    pct_decimal_like = numeric(0),
    n_alpha_present = integer(0),
    pct_alpha_present = numeric(0),
    n_alpha_only = integer(0),
    pct_alpha_only = numeric(0),
    n_alphanumeric = integer(0),
    pct_alphanumeric = numeric(0),
    n_short_code_like = integer(0),
    pct_short_code_like = numeric(0),
    n_long_numeric_id_like = integer(0),
    pct_long_numeric_id_like = numeric(0),
    min_numeric_value = numeric(0),
    max_numeric_value = numeric(0),
    geographic_field_name_signal = logical(0),
    identifier_field_name_signal = logical(0),
    count_field_name_signal = logical(0),
    coordinate_field_name_signal = logical(0),
    sample_values = character(0),
    recommended_disposition = character(0),
    disposition_reason = character(0),
    stringsAsFactors = FALSE
  )
}


if (length(field_sample_records) > 0L) {
  
  field_samples <- do.call(
    rbind,
    field_sample_records
  )
  
} else {
  
  field_samples <- data.frame(
    source_file = character(0),
    file_name = character(0),
    field_name = character(0),
    inferred_unit_type = character(0),
    sample_rank = integer(0),
    sample_value = character(0),
    stringsAsFactors = FALSE
  )
}


# ------------------------------------------------------------------------------
# 18. SORT FIELD PROFILE
#
# Highest unresolved contribution first.
# ------------------------------------------------------------------------------

if (nrow(field_profile) > 0L) {
  
  field_profile <- field_profile[
    order(
      -field_profile$n_unresolved_canonical_units,
      -field_profile$pct_numeric_only,
      field_profile$file_name,
      field_profile$field_name
    ),
    ,
    drop = FALSE
  ]
  
  rownames(field_profile) <- NULL
}


# ------------------------------------------------------------------------------
# 19. BUILD UNRESOLVED UNIT SOURCE PROFILE
# ------------------------------------------------------------------------------

unresolved_canonical <- canonical_units[
  canonical_units$geographic_unit_type == "UNRESOLVED",
  ,
  drop = FALSE
]


if (nrow(unresolved_canonical) > 0L) {
  
  unresolved_crosswalk <- source_crosswalk[
    source_crosswalk$geographic_unit_id %in%
      unresolved_canonical$geographic_unit_id,
    ,
    drop = FALSE
  ]
  
} else {
  
  unresolved_crosswalk <- source_crosswalk[
    FALSE,
    ,
    drop = FALSE
  ]
}


unresolved_profile_records <- list()

unresolved_profile_counter <- 0L


if (nrow(unresolved_crosswalk) > 0L) {
  
  unresolved_field_keys <- unique(
    unresolved_crosswalk$field_key
  )
  
  for (
    unresolved_index in seq_along(
      unresolved_field_keys
    )
  ) {
    
    field_key <- unresolved_field_keys[
      unresolved_index
    ]
    
    field_rows <- unresolved_crosswalk[
      unresolved_crosswalk$field_key == field_key,
      ,
      drop = FALSE
    ]
    
    source_values <- sort(
      unique(
        normalise_text(
          field_rows$source_value
        )
      )
    )
    
    source_values <- source_values[
      !is.na(source_values)
    ]
    
    unresolved_unit_ids <- sort(
      unique(
        normalise_text(
          field_rows$geographic_unit_id
        )
      )
    )
    
    unresolved_unit_ids <- unresolved_unit_ids[
      !is.na(unresolved_unit_ids)
    ]
    
    unresolved_profile_counter <- unresolved_profile_counter + 1L
    
    unresolved_profile_records[[unresolved_profile_counter]] <- data.frame(
      source_file = field_rows$source_file[1L],
      file_name = basename(
        field_rows$source_file[1L]
      ),
      source_field = field_rows$source_field[1L],
      inferred_unit_type = field_rows$inferred_unit_type[1L],
      
      n_unresolved_units = length(
        unresolved_unit_ids
      ),
      
      n_distinct_source_values = length(
        source_values
      ),
      
      sample_values = sample_values(
        source_values,
        n = 20L
      ),
      
      stringsAsFactors = FALSE
    )
  }
}


if (length(unresolved_profile_records) > 0L) {
  
  unresolved_source_profile <- do.call(
    rbind,
    unresolved_profile_records
  )
  
  unresolved_source_profile <- unresolved_source_profile[
    order(
      -unresolved_source_profile$n_unresolved_units,
      unresolved_source_profile$file_name,
      unresolved_source_profile$source_field
    ),
    ,
    drop = FALSE
  ]
  
  rownames(unresolved_source_profile) <- NULL
  
} else {
  
  unresolved_source_profile <- data.frame(
    source_file = character(0),
    file_name = character(0),
    source_field = character(0),
    inferred_unit_type = character(0),
    n_unresolved_units = integer(0),
    n_distinct_source_values = integer(0),
    sample_values = character(0),
    stringsAsFactors = FALSE
  )
}


# ------------------------------------------------------------------------------
# 20. NUMERIC-DOMINATED FIELD PROFILE
# ------------------------------------------------------------------------------

numeric_selector <- (
  !is.na(
    field_profile$pct_numeric_only
  ) &
    field_profile$pct_numeric_only >= 0.50
)

numeric_selector[
  is.na(numeric_selector)
] <- FALSE


numeric_fields <- field_profile[
  numeric_selector,
  ,
  drop = FALSE
]


# ------------------------------------------------------------------------------
# 21. PROBABLE FALSE-POSITIVE FIELDS
# ------------------------------------------------------------------------------

exclusion_selector <- (
  field_profile$recommended_disposition ==
    "EXCLUDE_PROBABLE_NON_GEOGRAPHIC"
)

exclusion_selector[
  is.na(exclusion_selector)
] <- FALSE


probable_false_positive_fields <- field_profile[
  exclusion_selector,
  ,
  drop = FALSE
]


# ------------------------------------------------------------------------------
# 22. PROBABLE GEOGRAPHIC FIELDS
# ------------------------------------------------------------------------------

retain_selector <- (
  field_profile$recommended_disposition ==
    "RETAIN"
)

retain_selector[
  is.na(retain_selector)
] <- FALSE


probable_geographic_fields <- field_profile[
  retain_selector,
  ,
  drop = FALSE
]


# ------------------------------------------------------------------------------
# 23. FIELDS REQUIRING REVIEW
# ------------------------------------------------------------------------------

review_selector <- (
  field_profile$recommended_disposition ==
    "REVIEW"
)

review_selector[
  is.na(review_selector)
] <- FALSE


review_fields <- field_profile[
  review_selector,
  ,
  drop = FALSE
]


# ------------------------------------------------------------------------------
# 24. DISPOSITION PROFILE
# ------------------------------------------------------------------------------

if (nrow(field_profile) > 0L) {
  
  disposition_profile <- as.data.frame(
    table(
      field_profile$recommended_disposition,
      useNA = "ifany"
    ),
    stringsAsFactors = FALSE
  )
  
  names(disposition_profile) <- c(
    "recommended_disposition",
    "n_fields"
  )
  
  disposition_profile <- disposition_profile[
    disposition_profile$n_fields > 0L,
    ,
    drop = FALSE
  ]
  
} else {
  
  disposition_profile <- data.frame(
    recommended_disposition = character(0),
    n_fields = integer(0),
    stringsAsFactors = FALSE
  )
}


# ------------------------------------------------------------------------------
# 25. ACCOUNT FOR UNRESOLVED UNITS
#
# Determine whether every unresolved 06b canonical unit can be traced back to
# one or more audited candidate fields.
# ------------------------------------------------------------------------------

all_unresolved_ids <- sort(
  unique(
    normalise_text(
      unresolved_canonical$geographic_unit_id
    )
  )
)

all_unresolved_ids <- all_unresolved_ids[
  !is.na(all_unresolved_ids)
]


traced_unresolved_ids <- sort(
  unique(
    normalise_text(
      unresolved_crosswalk$geographic_unit_id
    )
  )
)

traced_unresolved_ids <- traced_unresolved_ids[
  !is.na(traced_unresolved_ids)
]


untraced_unresolved_ids <- setdiff(
  all_unresolved_ids,
  traced_unresolved_ids
)


all_unresolved_units_traced <- (
  length(
    untraced_unresolved_ids
  ) == 0L
)


# ------------------------------------------------------------------------------
# 26. ESTIMATE IMPACT OF PROBABLE FALSE POSITIVES
#
# This is diagnostic only.
#
# Count how many current 06b canonical IDs are generated by fields recommended
# for exclusion.
# ------------------------------------------------------------------------------

excluded_field_keys <- character(0)


if (nrow(probable_false_positive_fields) > 0L) {
  
  excluded_field_keys <- paste(
    probable_false_positive_fields$source_file,
    probable_false_positive_fields$field_name,
    sep = "::FIELD::"
  )
}


if (length(excluded_field_keys) > 0L) {
  
  excluded_crosswalk_rows <- source_crosswalk[
    source_crosswalk$field_key %in%
      excluded_field_keys,
    ,
    drop = FALSE
  ]
  
  probable_false_positive_unit_ids <- sort(
    unique(
      normalise_text(
        excluded_crosswalk_rows$geographic_unit_id
      )
    )
  )
  
  probable_false_positive_unit_ids <- probable_false_positive_unit_ids[
    !is.na(probable_false_positive_unit_ids)
  ]
  
} else {
  
  probable_false_positive_unit_ids <- character(0)
}


n_probable_false_positive_units <- length(
  probable_false_positive_unit_ids
)


n_unresolved_false_positive_units <- length(
  intersect(
    probable_false_positive_unit_ids,
    all_unresolved_ids
  )
)


# ------------------------------------------------------------------------------
# 27. SUMMARY TABLE
# ------------------------------------------------------------------------------

summary_table <- data.frame(
  metric = c(
    "candidate_fields_audited",
    "source_files_represented",
    "raw_unit_records_audited",
    "canonical_units_in_06b",
    "unresolved_canonical_units_in_06b",
    "unresolved_units_traced_to_candidate_fields",
    "unresolved_units_untraced",
    "numeric_dominated_candidate_fields",
    "probable_geographic_fields",
    "probable_false_positive_fields",
    "fields_requiring_review",
    "canonical_units_generated_by_probable_false_positive_fields",
    "unresolved_units_generated_by_probable_false_positive_fields"
  ),
  
  value = c(
    nrow(
      field_profile
    ),
    
    length(
      unique(
        field_profile$source_file
      )
    ),
    
    nrow(
      raw_units
    ),
    
    nrow(
      canonical_units
    ),
    
    length(
      all_unresolved_ids
    ),
    
    length(
      traced_unresolved_ids
    ),
    
    length(
      untraced_unresolved_ids
    ),
    
    nrow(
      numeric_fields
    ),
    
    nrow(
      probable_geographic_fields
    ),
    
    nrow(
      probable_false_positive_fields
    ),
    
    nrow(
      review_fields
    ),
    
    n_probable_false_positive_units,
    
    n_unresolved_false_positive_units
  ),
  
  stringsAsFactors = FALSE
)


# ------------------------------------------------------------------------------
# 28. VALIDATION GATE
# ------------------------------------------------------------------------------

candidate_fields_accounted_for <- (
  nrow(field_profile) ==
    length(
      unique(
        candidate_fields$field_key
      )
    )
)


all_profile_fields_have_disposition <- TRUE


if (nrow(field_profile) > 0L) {
  
  all_profile_fields_have_disposition <- all(
    !is.na(
      field_profile$recommended_disposition
    ) &
      field_profile$recommended_disposition %in%
      c(
        "RETAIN",
        "REVIEW",
        "EXCLUDE_PROBABLE_NON_GEOGRAPHIC"
      )
  )
}


raw_records_preserved <- (
  nrow(raw_units) ==
    sum(
      field_profile$n_raw_unit_records
    )
)


validation_gate <- data.frame(
  criterion = c(
    "06b_validation_passed",
    "candidate_field_table_loaded",
    "raw_unit_table_loaded",
    "canonical_unit_table_loaded",
    "source_crosswalk_loaded",
    "all_candidate_fields_profiled",
    "raw_unit_records_accounted_for",
    "all_fields_have_disposition",
    "all_unresolved_units_traced",
    "audit_did_not_modify_06b_outputs",
    "no_geographic_units_automatically_excluded",
    "published_vpjd_not_modified"
  ),
  
  passed = c(
    all(
      validation_06b$passed
    ),
    
    nrow(
      candidate_fields
    ) > 0L,
    
    nrow(
      raw_units
    ) > 0L,
    
    nrow(
      canonical_units
    ) > 0L,
    
    nrow(
      source_crosswalk
    ) > 0L,
    
    candidate_fields_accounted_for,
    
    raw_records_preserved,
    
    all_profile_fields_have_disposition,
    
    all_unresolved_units_traced,
    
    TRUE,
    
    TRUE,
    
    TRUE
  ),
  
  stringsAsFactors = FALSE
)


# ------------------------------------------------------------------------------
# 29. WRITE OUTPUTS
# ------------------------------------------------------------------------------

write.csv(
  field_profile,
  FIELD_PROFILE_FILE,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)


write.csv(
  field_samples,
  FIELD_SAMPLE_FILE,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)


write.csv(
  unresolved_source_profile,
  UNRESOLVED_SOURCE_FILE,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)


write.csv(
  numeric_fields,
  NUMERIC_FIELD_FILE,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)


write.csv(
  probable_false_positive_fields,
  EXCLUSION_CANDIDATE_FILE,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)


write.csv(
  probable_geographic_fields,
  RETAIN_CANDIDATE_FILE,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)


write.csv(
  review_fields,
  REVIEW_FIELD_FILE,
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
# 30. CONSOLE SUMMARY
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" VPJD GEOGRAPHY 06b0 - AUDIT COMPLETE\n")
cat("============================================================\n\n")


cat("Summary:\n\n")

print(
  summary_table,
  row.names = FALSE
)


# ------------------------------------------------------------------------------
# 31. FIELD DISPOSITION PROFILE
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" FIELD DISPOSITION PROFILE\n")
cat("============================================================\n\n")


print(
  disposition_profile,
  row.names = FALSE
)


# ------------------------------------------------------------------------------
# 32. COMPLETE CANDIDATE FIELD PROFILE
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" CANDIDATE FIELD PROFILE\n")
cat("============================================================\n\n")


if (nrow(field_profile) > 0L) {
  
  print(
    field_profile[
      ,
      c(
        "file_name",
        "field_name",
        "inferred_unit_type",
        "n_distinct_source_values",
        "n_canonical_units_generated",
        "n_unresolved_canonical_units",
        "pct_numeric_only",
        "pct_alpha_present",
        "recommended_disposition",
        "disposition_reason"
      ),
      drop = FALSE
    ],
    row.names = FALSE
  )
  
} else {
  
  cat(
    "No candidate fields available.\n"
  )
}


# ------------------------------------------------------------------------------
# 33. UNRESOLVED UNIT SOURCES
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" SOURCES OF UNRESOLVED 06b UNITS\n")
cat("============================================================\n\n")


if (nrow(unresolved_source_profile) > 0L) {
  
  print(
    unresolved_source_profile,
    row.names = FALSE
  )
  
} else {
  
  cat(
    "No unresolved canonical units were generated by 06b.\n"
  )
}


# ------------------------------------------------------------------------------
# 34. PROBABLE FALSE-POSITIVE FIELDS
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" PROBABLE FALSE-POSITIVE GEOGRAPHIC FIELDS\n")
cat("============================================================\n\n")


if (nrow(probable_false_positive_fields) > 0L) {
  
  print(
    probable_false_positive_fields[
      ,
      c(
        "file_name",
        "field_name",
        "inferred_unit_type",
        "n_distinct_source_values",
        "n_canonical_units_generated",
        "n_unresolved_canonical_units",
        "pct_numeric_only",
        "pct_long_numeric_id_like",
        "sample_values",
        "disposition_reason"
      ),
      drop = FALSE
    ],
    row.names = FALSE
  )
  
} else {
  
  cat(
    "No fields met the conservative probable-false-positive criteria.\n"
  )
}


# ------------------------------------------------------------------------------
# 35. PROBABLE GEOGRAPHIC FIELDS
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" PROBABLE GEOGRAPHIC FIELDS\n")
cat("============================================================\n\n")


if (nrow(probable_geographic_fields) > 0L) {
  
  print(
    probable_geographic_fields[
      ,
      c(
        "file_name",
        "field_name",
        "inferred_unit_type",
        "n_distinct_source_values",
        "n_canonical_units_generated",
        "sample_values",
        "disposition_reason"
      ),
      drop = FALSE
    ],
    row.names = FALSE
  )
  
} else {
  
  cat(
    "No fields met the conservative RETAIN criteria.\n"
  )
}


# ------------------------------------------------------------------------------
# 36. FIELDS REQUIRING REVIEW
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" FIELDS REQUIRING REVIEW\n")
cat("============================================================\n\n")


if (nrow(review_fields) > 0L) {
  
  print(
    review_fields[
      ,
      c(
        "file_name",
        "field_name",
        "inferred_unit_type",
        "n_distinct_source_values",
        "n_canonical_units_generated",
        "n_unresolved_canonical_units",
        "pct_numeric_only",
        "pct_alpha_present",
        "sample_values",
        "disposition_reason"
      ),
      drop = FALSE
    ],
    row.names = FALSE
  )
  
} else {
  
  cat(
    "No candidate fields remain in REVIEW status.\n"
  )
}


# ------------------------------------------------------------------------------
# 37. VALIDATION GATE
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
# 38. INTERPRETATION
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
      "PASS. All 06b candidate geographic fields have been audited and ",
      "all unresolved canonical units can be traced back to their source ",
      "field(s).\n\n"
    )
  )
  
  cat(
    paste0(
      nrow(
        probable_false_positive_fields
      ),
      " field(s) meet conservative criteria for probable non-geographic ",
      "false positives.\n"
    )
  )
  
  cat(
    paste0(
      "These field(s) currently account for ",
      format(
        n_probable_false_positive_units,
        big.mark = ","
      ),
      " provisional 06b canonical unit(s), including ",
      format(
        n_unresolved_false_positive_units,
        big.mark = ","
      ),
      " unresolved unit(s).\n\n"
    )
  )
  
  cat(
    paste0(
      nrow(
        probable_geographic_fields
      ),
      " field(s) meet conservative criteria for retention as geographic ",
      "evidence, while ",
      nrow(
        review_fields
      ),
      " field(s) require manual or source-aware review.\n\n"
    )
  )
  
  cat(
    paste0(
      "No fields or geographic units have been removed. The recommendations ",
      "in this audit are diagnostic only.\n"
    )
  )
  
} else {
  
  cat(
    "FAIL. One or more 06b0 validation criteria failed.\n\n"
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
# 39. NEXT STEP
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
      "Review the candidate-field audit, especially:\n\n",
      FIELD_PROFILE_FILE,
      "\n\n",
      EXCLUSION_CANDIDATE_FILE,
      "\n\n",
      REVIEW_FIELD_FILE,
      "\n\n",
      "The next script should use the reviewed field disposition to rebuild ",
      "the provisional geographic vocabulary from genuine geographic fields ",
      "only.\n\n",
      "Recommended next stage:\n\n",
      "  geography_06b1_validate_candidate_geographic_fields.R\n\n",
      "That stage should explicitly approve/exclude source fields before ",
      "VPJD-GEO identifiers are rebuilt and subsequently validated.\n"
    )
  )
  
} else {
  
  cat(
    "Do not proceed until the failed 06b0 validation criterion is resolved.\n"
  )
}


# ------------------------------------------------------------------------------
# 40. SAFEGUARDS
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" SAFEGUARDS\n")
cat("============================================================\n\n")


cat(
  "06b candidate-field data were NOT modified.\n"
)

cat(
  "06b raw geographic-unit data were NOT modified.\n"
)

cat(
  "06b canonical geographic-unit data were NOT modified.\n"
)

cat(
  "No candidate fields were automatically excluded.\n"
)

cat(
  "No VPJD-GEO identifiers were frozen or reassigned.\n"
)

cat(
  "Source geographic evidence was NOT modified.\n"
)

cat(
  "Published VPJD Taxonomic Release v1.0.0 was NOT modified.\n"
)

cat(
  "Missing geographic evidence was NOT interpreted as absence.\n"
)

cat(
  "Geographic equivalence was NOT inferred.\n"
)

cat(
  "Geographic hierarchy was NOT inferred.\n"
)

cat(
  "Nakamura predicates were NOT evaluated.\n"
)

cat(
  "Star categories were NOT evaluated.\n"
)

cat(
  "VPJD Geography v1.0.0 has NOT been frozen or published.\n"
)

cat("\n")
cat("============================================================\n")