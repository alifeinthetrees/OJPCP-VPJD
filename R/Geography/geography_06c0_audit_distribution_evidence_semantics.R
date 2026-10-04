# ==============================================================================
# VPJD GEOGRAPHY MODULE
# geography_06c0_audit_distribution_evidence_semantics.R
#
# PURPOSE
# -------
# Audit the evidence semantics of:
#
#   vpjd_taxon_botanical_area_distribution.csv
#
# before rebuilding the canonical positive-evidence taxon x geographic-unit
# distribution for VPJD Geography v1.0.0.
#
#
# WHY THIS AUDIT IS REQUIRED
# --------------------------
#
# geography_06c_build_taxon_geographic_distribution.R selected:
#
#   vpjd_taxon_botanical_area_distribution.csv
#
# and identified:
#
#   source_taxon_identifier = wcvp_plant_name_id
#   source_geographic_field = botanical_area_name
#   source_evidence_field   = <NA>
#
# The script therefore interpreted every source row as positive evidence.
#
# The source contained:
#
#   613,887 rows
#
# which equals:
#
#   12,037 recognised taxa x 51 Japanese botanical areas
#
# This strongly indicates that the source represents the COMPLETE
# taxon x botanical-area combination space rather than a positive-only
# relationship table.
#
#
# PREVIOUSLY VALIDATED ANALYTICAL COUNTS
# --------------------------------------
#
# Earlier Geography validation established approximately:
#
#   total taxon x area combinations       613,887
#   supported combinations                128,824
#   unsupported combinations              485,063
#   taxa with positive Japan evidence      11,470
#   taxa without positive Japan evidence      567
#   Japanese botanical areas                   51
#
# This audit must determine how that distinction is represented in the
# source data.
#
#
# THIS SCRIPT DOES
# ----------------
#
#   - read the candidate distribution table;
#   - inspect its schema;
#   - profile every field;
#   - identify logical/binary/low-cardinality fields;
#   - identify numeric fields that may encode evidence counts;
#   - inspect missingness;
#   - inspect distinct-value counts;
#   - produce frequency tables for manageable categorical fields;
#   - test candidate evidence interpretations;
#   - test whether any field can reconstruct the known 128,824 / 485,063 split;
#   - test resulting taxon coverage;
#   - write detailed audit outputs;
#   - recommend candidate evidence fields where supported by the data.
#
#
# THIS SCRIPT DOES NOT
# --------------------
#
#   - alter the source distribution table;
#   - alter VPJD Taxonomic Release v1.0.0;
#   - alter the frozen geographic vocabulary;
#   - write a release distribution product;
#   - infer absence;
#   - infer presence merely because a taxon x area row exists;
#   - use Nakamura logic;
#   - assign Star categories.
#
# ==============================================================================


# ------------------------------------------------------------------------------
# 01. CONSTANTS
# ------------------------------------------------------------------------------

SCRIPT_NAME <- "geography_06c0_audit_distribution_evidence_semantics.R"

BUILD_DATE <- format(
  Sys.Date(),
  "%Y-%m-%d"
)

EXPECTED_TAXA <- 12037L

EXPECTED_AREAS <- 51L

EXPECTED_COMBINATIONS <- 613887L

REFERENCE_SUPPORTED_COMBINATIONS <- 128824L

REFERENCE_UNSUPPORTED_COMBINATIONS <- 485063L

REFERENCE_TAXA_WITH_EVIDENCE <- 11470L

REFERENCE_TAXA_WITHOUT_EVIDENCE <- 567L


# ------------------------------------------------------------------------------
# 02. PATHS
# ------------------------------------------------------------------------------

PROJECT_ROOT <- "I:/R/OJPCP/VPJD-OJPCP"

SOURCE_FILE <- file.path(
  PROJECT_ROOT,
  "data",
  "derived",
  "geography",
  "vpjd_taxon_botanical_area_distribution.csv"
)

AUDIT_ROOT <- file.path(
  PROJECT_ROOT,
  "audit",
  "geography",
  "release_v1.0.0",
  "06c0_audit_distribution_evidence_semantics"
)


dir.create(
  AUDIT_ROOT,
  recursive = TRUE,
  showWarnings = FALSE
)


# ------------------------------------------------------------------------------
# 03. OUTPUT PATHS
# ------------------------------------------------------------------------------

SCHEMA_FILE <- file.path(
  AUDIT_ROOT,
  "geography_06c0_schema.csv"
)

FIELD_PROFILE_FILE <- file.path(
  AUDIT_ROOT,
  "geography_06c0_field_profile.csv"
)

LOW_CARDINALITY_FILE <- file.path(
  AUDIT_ROOT,
  "geography_06c0_low_cardinality_values.csv"
)

NUMERIC_PROFILE_FILE <- file.path(
  AUDIT_ROOT,
  "geography_06c0_numeric_field_profile.csv"
)

CANDIDATE_FIELD_FILE <- file.path(
  AUDIT_ROOT,
  "geography_06c0_candidate_evidence_fields.csv"
)

CANDIDATE_RULE_FILE <- file.path(
  AUDIT_ROOT,
  "geography_06c0_candidate_evidence_rules.csv"
)

SUMMARY_FILE <- file.path(
  AUDIT_ROOT,
  "geography_06c0_summary.csv"
)

INTERPRETATION_FILE <- file.path(
  AUDIT_ROOT,
  "geography_06c0_interpretation.txt"
)


# ------------------------------------------------------------------------------
# 04. HELPERS
# ------------------------------------------------------------------------------

normalise_text <- function(x) {
  
  y <- as.character(x)
  
  y <- iconv(
    y,
    from = "",
    to = "UTF-8",
    sub = ""
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


normalise_key <- function(x) {
  
  y <- normalise_text(x)
  
  y <- tolower(y)
  
  y <- gsub(
    "[[:space:]]+",
    " ",
    y
  )
  
  trimws(y)
}


safe_read_csv <- function(path) {
  
  if (!file.exists(path)) {
    
    stop(
      paste0(
        "Required source file not found:\n",
        path
      )
    )
  }
  
  
  attempts <- list(
    
    function() {
      
      read.csv(
        path,
        stringsAsFactors = FALSE,
        check.names = FALSE,
        fileEncoding = "UTF-8"
      )
    },
    
    function() {
      
      read.csv(
        path,
        stringsAsFactors = FALSE,
        check.names = FALSE,
        fileEncoding = "UTF-8-BOM"
      )
    },
    
    function() {
      
      read.csv(
        path,
        stringsAsFactors = FALSE,
        check.names = FALSE,
        fileEncoding = "latin1"
      )
    }
  )
  
  
  errors <- character(0)
  
  
  for (attempt_index in seq_along(attempts)) {
    
    result <- tryCatch(
      
      attempts[[attempt_index]](),
      
      error = function(e) {
        
        errors <<- c(
          errors,
          conditionMessage(e)
        )
        
        NULL
      }
    )
    
    
    if (!is.null(result)) {
      
      return(result)
    }
  }
  
  
  stop(
    paste0(
      "Unable to read source CSV:\n",
      path,
      "\n\nErrors:\n",
      paste(
        errors,
        collapse = "\n"
      )
    )
  )
}


safe_numeric <- function(x) {
  
  suppressWarnings(
    as.numeric(
      normalise_text(x)
    )
  )
}


# ------------------------------------------------------------------------------
# 05. START
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" VPJD GEOGRAPHY 06c0 - DISTRIBUTION EVIDENCE AUDIT\n")
cat("============================================================\n\n")

cat("Source:\n")
cat(SOURCE_FILE, "\n\n")

cat("Reading source table...\n")


# ------------------------------------------------------------------------------
# 06. READ SOURCE
# ------------------------------------------------------------------------------

distribution <- safe_read_csv(
  SOURCE_FILE
)


cat(
  "Rows: ",
  format(
    nrow(distribution),
    big.mark = ","
  ),
  "\n",
  sep = ""
)

cat(
  "Fields: ",
  ncol(distribution),
  "\n\n",
  sep = ""
)


# ------------------------------------------------------------------------------
# 07. BASIC DIMENSION TESTS
# ------------------------------------------------------------------------------

dimension_summary <- data.frame(
  
  metric = c(
    "source_rows",
    "source_fields",
    "expected_taxa",
    "expected_botanical_areas",
    "expected_complete_combination_space",
    "reference_supported_combinations",
    "reference_unsupported_combinations",
    "reference_taxa_with_evidence",
    "reference_taxa_without_evidence"
  ),
  
  value = c(
    nrow(distribution),
    ncol(distribution),
    EXPECTED_TAXA,
    EXPECTED_AREAS,
    EXPECTED_COMBINATIONS,
    REFERENCE_SUPPORTED_COMBINATIONS,
    REFERENCE_UNSUPPORTED_COMBINATIONS,
    REFERENCE_TAXA_WITH_EVIDENCE,
    REFERENCE_TAXA_WITHOUT_EVIDENCE
  ),
  
  stringsAsFactors = FALSE
)


# ------------------------------------------------------------------------------
# 08. SCHEMA
# ------------------------------------------------------------------------------

field_names <- names(
  distribution
)


schema <- data.frame(
  
  field_number =
    seq_along(field_names),
  
  field_name =
    field_names,
  
  class =
    vapply(
      distribution,
      function(x) {
        paste(
          class(x),
          collapse = ";"
        )
      },
      character(1)
    ),
  
  stringsAsFactors = FALSE
)


write.csv(
  schema,
  SCHEMA_FILE,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)


cat("Schema:\n\n")

print(
  schema,
  row.names = FALSE
)


# ------------------------------------------------------------------------------
# 09. PROFILE EVERY FIELD
# ------------------------------------------------------------------------------

field_profile_records <- vector(
  mode = "list",
  length = length(field_names)
)


for (field_index in seq_along(field_names)) {
  
  field_name <- field_names[field_index]
  
  values <- distribution[[field_name]]
  
  text_values <- normalise_text(
    values
  )
  
  non_missing <- text_values[
    !is.na(text_values)
  ]
  
  n_missing <- sum(
    is.na(text_values)
  )
  
  n_non_missing <- length(
    non_missing
  )
  
  n_distinct <- length(
    unique(non_missing)
  )
  
  
  field_profile_records[[field_index]] <- data.frame(
    
    field_number =
      field_index,
    
    field_name =
      field_name,
    
    class =
      paste(
        class(values),
        collapse = ";"
      ),
    
    n_rows =
      length(values),
    
    n_missing =
      n_missing,
    
    pct_missing =
      round(
        100 * n_missing / length(values),
        6
      ),
    
    n_non_missing =
      n_non_missing,
    
    n_distinct_non_missing =
      n_distinct,
    
    pct_distinct_non_missing =
      if (
        n_non_missing > 0L
      ) {
        
        round(
          100 * n_distinct / n_non_missing,
          6
        )
        
      } else {
        
        NA_real_
      },
    
    stringsAsFactors = FALSE
  )
}


field_profile <- do.call(
  rbind,
  field_profile_records
)


write.csv(
  field_profile,
  FIELD_PROFILE_FILE,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)


# ------------------------------------------------------------------------------
# 10. IDENTIFY TAXON AND GEOGRAPHIC FIELDS
# ------------------------------------------------------------------------------

taxon_field_candidates <- c(
  "wcvp_plant_name_id",
  "plant_name_id",
  "wcvp_id",
  "wcvp_taxon_id",
  "taxon_id"
)


taxon_field_matches <- taxon_field_candidates[
  taxon_field_candidates %in%
    field_names
]


if (length(taxon_field_matches) == 0L) {
  
  stop(
    paste0(
      "Unable to identify a taxon identifier field.\n\n",
      "Fields present:\n",
      paste(
        field_names,
        collapse = "\n"
      )
    )
  )
}


taxon_field <- taxon_field_matches[1L]


geographic_field_candidates <- c(
  "botanical_area_name",
  "botanical_area",
  "botanical_area_code",
  "geographic_unit_id",
  "geographic_code",
  "area_name",
  "area"
)


geographic_field_matches <- geographic_field_candidates[
  geographic_field_candidates %in%
    field_names
]


if (length(geographic_field_matches) == 0L) {
  
  stop(
    paste0(
      "Unable to identify botanical-area field.\n\n",
      "Fields present:\n",
      paste(
        field_names,
        collapse = "\n"
      )
    )
  )
}


geographic_field <- geographic_field_matches[1L]


taxon_values <- normalise_text(
  distribution[[taxon_field]]
)

geographic_values <- normalise_text(
  distribution[[geographic_field]]
)


n_distinct_taxa <- length(
  unique(
    taxon_values[
      !is.na(taxon_values)
    ]
  )
)

n_distinct_areas <- length(
  unique(
    geographic_values[
      !is.na(geographic_values)
    ]
  )
)


# ------------------------------------------------------------------------------
# 11. TEST COMPLETE CROSS-PRODUCT STRUCTURE
# ------------------------------------------------------------------------------

combination_key <- paste(
  taxon_values,
  geographic_values,
  sep = "||"
)


n_distinct_combinations <- length(
  unique(
    combination_key
  )
)


expected_from_observed_dimensions <- (
  n_distinct_taxa *
    n_distinct_areas
)


complete_cross_product <- (
  nrow(distribution) ==
    expected_from_observed_dimensions &&
    n_distinct_combinations ==
    expected_from_observed_dimensions
)


# ------------------------------------------------------------------------------
# 12. LOW-CARDINALITY FIELD VALUES
#
# These are especially important because the evidence flag may be encoded
# under a name that 06c did not recognise.
# ------------------------------------------------------------------------------

LOW_CARDINALITY_LIMIT <- 100L


low_cardinality_records <- list()

low_cardinality_counter <- 0L


for (field_index in seq_along(field_names)) {
  
  field_name <- field_names[field_index]
  
  values <- normalise_text(
    distribution[[field_name]]
  )
  
  non_missing <- values[
    !is.na(values)
  ]
  
  unique_values <- unique(
    non_missing
  )
  
  
  if (
    length(unique_values) > 0L &&
    length(unique_values) <=
    LOW_CARDINALITY_LIMIT
  ) {
    
    value_table <- sort(
      table(non_missing),
      decreasing = TRUE
    )
    
    
    for (value_index in seq_along(value_table)) {
      
      low_cardinality_counter <- (
        low_cardinality_counter + 1L
      )
      
      
      low_cardinality_records[[low_cardinality_counter]] <- data.frame(
        
        field_name =
          field_name,
        
        value =
          names(value_table)[value_index],
        
        count =
          as.integer(
            value_table[value_index]
          ),
        
        percentage =
          round(
            100 *
              as.integer(
                value_table[value_index]
              ) /
              length(values),
            6
          ),
        
        stringsAsFactors = FALSE
      )
    }
  }
}


if (length(low_cardinality_records) > 0L) {
  
  low_cardinality_values <- do.call(
    rbind,
    low_cardinality_records
  )
  
} else {
  
  low_cardinality_values <- data.frame(
    
    field_name =
      character(0),
    
    value =
      character(0),
    
    count =
      integer(0),
    
    percentage =
      numeric(0),
    
    stringsAsFactors = FALSE
  )
}


write.csv(
  low_cardinality_values,
  LOW_CARDINALITY_FILE,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)


# ------------------------------------------------------------------------------
# 13. NUMERIC FIELD PROFILE
#
# A positive-evidence distinction may be represented by a count rather than
# a Boolean field.
# ------------------------------------------------------------------------------

numeric_profile_records <- list()

numeric_profile_counter <- 0L


for (field_index in seq_along(field_names)) {
  
  field_name <- field_names[field_index]
  
  raw_values <- distribution[[field_name]]
  
  numeric_values <- safe_numeric(
    raw_values
  )
  
  original_non_missing <- sum(
    !is.na(
      normalise_text(
        raw_values
      )
    )
  )
  
  numeric_non_missing <- sum(
    !is.na(numeric_values)
  )
  
  
  # Treat a field as numerically interpretable only where at least 95% of
  # its non-missing values successfully convert to numeric.
  
  numeric_fraction <- if (
    original_non_missing > 0L
  ) {
    
    numeric_non_missing /
      original_non_missing
    
  } else {
    
    0
  }
  
  
  if (numeric_fraction >= 0.95) {
    
    numeric_profile_counter <- (
      numeric_profile_counter + 1L
    )
    
    
    non_missing_numeric <- numeric_values[
      !is.na(numeric_values)
    ]
    
    
    numeric_profile_records[[numeric_profile_counter]] <- data.frame(
      
      field_name =
        field_name,
      
      n_numeric =
        length(non_missing_numeric),
      
      numeric_fraction =
        numeric_fraction,
      
      minimum =
        if (
          length(non_missing_numeric) > 0L
        ) {
          
          min(non_missing_numeric)
          
        } else {
          
          NA_real_
        },
      
      maximum =
        if (
          length(non_missing_numeric) > 0L
        ) {
          
          max(non_missing_numeric)
          
        } else {
          
          NA_real_
        },
      
      mean =
        if (
          length(non_missing_numeric) > 0L
        ) {
          
          mean(non_missing_numeric)
          
        } else {
          
          NA_real_
        },
      
      median =
        if (
          length(non_missing_numeric) > 0L
        ) {
          
          median(non_missing_numeric)
          
        } else {
          
          NA_real_
        },
      
      n_zero =
        sum(
          numeric_values == 0,
          na.rm = TRUE
        ),
      
      n_gt_zero =
        sum(
          numeric_values > 0,
          na.rm = TRUE
        ),
      
      n_eq_one =
        sum(
          numeric_values == 1,
          na.rm = TRUE
        ),
      
      stringsAsFactors = FALSE
    )
  }
}


if (length(numeric_profile_records) > 0L) {
  
  numeric_profile <- do.call(
    rbind,
    numeric_profile_records
  )
  
} else {
  
  numeric_profile <- data.frame(
    
    field_name =
      character(0),
    
    n_numeric =
      integer(0),
    
    numeric_fraction =
      numeric(0),
    
    minimum =
      numeric(0),
    
    maximum =
      numeric(0),
    
    mean =
      numeric(0),
    
    median =
      numeric(0),
    
    n_zero =
      integer(0),
    
    n_gt_zero =
      integer(0),
    
    n_eq_one =
      integer(0),
    
    stringsAsFactors = FALSE
  )
}


write.csv(
  numeric_profile,
  NUMERIC_PROFILE_FILE,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)


# ------------------------------------------------------------------------------
# 14. SCORE FIELD NAMES FOR EVIDENCE SEMANTICS
# ------------------------------------------------------------------------------

field_name_key <- tolower(
  field_names
)


evidence_name_pattern <- paste(
  c(
    "support",
    "present",
    "presence",
    "occur",
    "evidence",
    "record",
    "count",
    "observ",
    "source",
    "status",
    "distribution"
  ),
  collapse = "|"
)


name_candidate_selector <- grepl(
  evidence_name_pattern,
  field_name_key
)


candidate_field_names <- field_names[
  name_candidate_selector
]


candidate_field_records <- list()

candidate_field_counter <- 0L


for (field_name in candidate_field_names) {
  
  candidate_field_counter <- (
    candidate_field_counter + 1L
  )
  
  
  profile_row <- field_profile[
    field_profile$field_name ==
      field_name,
    ,
    drop = FALSE
  ]
  
  
  candidate_field_records[[candidate_field_counter]] <- data.frame(
    
    field_name =
      field_name,
    
    class =
      profile_row$class[1L],
    
    n_missing =
      profile_row$n_missing[1L],
    
    n_distinct_non_missing =
      profile_row$n_distinct_non_missing[1L],
    
    evidence_name_match =
      TRUE,
    
    stringsAsFactors = FALSE
  )
}


if (length(candidate_field_records) > 0L) {
  
  candidate_fields <- do.call(
    rbind,
    candidate_field_records
  )
  
} else {
  
  candidate_fields <- data.frame(
    
    field_name =
      character(0),
    
    class =
      character(0),
    
    n_missing =
      integer(0),
    
    n_distinct_non_missing =
      integer(0),
    
    evidence_name_match =
      logical(0),
    
    stringsAsFactors = FALSE
  )
}


write.csv(
  candidate_fields,
  CANDIDATE_FIELD_FILE,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)


# ------------------------------------------------------------------------------
# 15. TEST GENERIC POSITIVE CATEGORICAL VALUES
# ------------------------------------------------------------------------------

positive_tokens <- c(
  "true",
  "t",
  "yes",
  "y",
  "1",
  "present",
  "presence",
  "supported",
  "support",
  "positive",
  "occurs",
  "occurrence",
  "observed",
  "recorded",
  "native",
  "introduced",
  "extant"
)


candidate_rule_records <- list()

candidate_rule_counter <- 0L


for (field_index in seq_along(field_names)) {
  
  field_name <- field_names[field_index]
  
  values <- normalise_key(
    distribution[[field_name]]
  )
  
  unique_values <- unique(
    values[
      !is.na(values)
    ]
  )
  
  
  # --------------------------------------------------------------------------
  # Rule A: recognised positive categorical tokens
  # --------------------------------------------------------------------------
  
  positive_selector <- (
    !is.na(values) &
      values %in%
      positive_tokens
  )
  
  
  n_positive <- sum(
    positive_selector
  )
  
  
  if (n_positive > 0L) {
    
    positive_taxa <- length(
      unique(
        taxon_values[
          positive_selector &
            !is.na(taxon_values)
        ]
      )
    )
    
    
    positive_areas <- length(
      unique(
        geographic_values[
          positive_selector &
            !is.na(geographic_values)
        ]
      )
    )
    
    
    candidate_rule_counter <- (
      candidate_rule_counter + 1L
    )
    
    
    candidate_rule_records[[candidate_rule_counter]] <- data.frame(
      
      field_name =
        field_name,
      
      rule =
        "RECOGNISED_POSITIVE_TOKEN",
      
      positive_rows =
        n_positive,
      
      unsupported_rows =
        nrow(distribution) -
        n_positive,
      
      taxa_with_positive_evidence =
        positive_taxa,
      
      areas_with_positive_evidence =
        positive_areas,
      
      difference_from_reference_supported =
        abs(
          n_positive -
            REFERENCE_SUPPORTED_COMBINATIONS
        ),
      
      exact_supported_count_match =
        n_positive ==
        REFERENCE_SUPPORTED_COMBINATIONS,
      
      exact_taxon_count_match =
        positive_taxa ==
        REFERENCE_TAXA_WITH_EVIDENCE,
      
      stringsAsFactors = FALSE
    )
  }
  
  
  # --------------------------------------------------------------------------
  # Rule B: numeric > 0
  # --------------------------------------------------------------------------
  
  numeric_values <- safe_numeric(
    distribution[[field_name]]
  )
  
  
  original_non_missing <- sum(
    !is.na(values)
  )
  
  numeric_non_missing <- sum(
    !is.na(numeric_values)
  )
  
  
  numeric_fraction <- if (
    original_non_missing > 0L
  ) {
    
    numeric_non_missing /
      original_non_missing
    
  } else {
    
    0
  }
  
  
  if (numeric_fraction >= 0.95) {
    
    numeric_positive_selector <- (
      !is.na(numeric_values) &
        numeric_values > 0
    )
    
    
    numeric_positive_count <- sum(
      numeric_positive_selector
    )
    
    
    numeric_positive_taxa <- length(
      unique(
        taxon_values[
          numeric_positive_selector &
            !is.na(taxon_values)
        ]
      )
    )
    
    
    numeric_positive_areas <- length(
      unique(
        geographic_values[
          numeric_positive_selector &
            !is.na(geographic_values)
        ]
      )
    )
    
    
    candidate_rule_counter <- (
      candidate_rule_counter + 1L
    )
    
    
    candidate_rule_records[[candidate_rule_counter]] <- data.frame(
      
      field_name =
        field_name,
      
      rule =
        "NUMERIC_GREATER_THAN_ZERO",
      
      positive_rows =
        numeric_positive_count,
      
      unsupported_rows =
        nrow(distribution) -
        numeric_positive_count,
      
      taxa_with_positive_evidence =
        numeric_positive_taxa,
      
      areas_with_positive_evidence =
        numeric_positive_areas,
      
      difference_from_reference_supported =
        abs(
          numeric_positive_count -
            REFERENCE_SUPPORTED_COMBINATIONS
        ),
      
      exact_supported_count_match =
        numeric_positive_count ==
        REFERENCE_SUPPORTED_COMBINATIONS,
      
      exact_taxon_count_match =
        numeric_positive_taxa ==
        REFERENCE_TAXA_WITH_EVIDENCE,
      
      stringsAsFactors = FALSE
    )
  }
}


if (length(candidate_rule_records) > 0L) {
  
  candidate_rules <- do.call(
    rbind,
    candidate_rule_records
  )
  
  candidate_rules <- candidate_rules[
    order(
      candidate_rules$difference_from_reference_supported,
      candidate_rules$field_name,
      candidate_rules$rule
    ),
    ,
    drop = FALSE
  ]
  
  rownames(
    candidate_rules
  ) <- NULL
  
} else {
  
  candidate_rules <- data.frame(
    
    field_name =
      character(0),
    
    rule =
      character(0),
    
    positive_rows =
      integer(0),
    
    unsupported_rows =
      integer(0),
    
    taxa_with_positive_evidence =
      integer(0),
    
    areas_with_positive_evidence =
      integer(0),
    
    difference_from_reference_supported =
      integer(0),
    
    exact_supported_count_match =
      logical(0),
    
    exact_taxon_count_match =
      logical(0),
    
    stringsAsFactors = FALSE
  )
}


write.csv(
  candidate_rules,
  CANDIDATE_RULE_FILE,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)


# ------------------------------------------------------------------------------
# 16. IDENTIFY BEST CANDIDATE RULE
# ------------------------------------------------------------------------------

best_candidate_available <- (
  nrow(candidate_rules) > 0L
)


if (best_candidate_available) {
  
  best_candidate <- candidate_rules[
    1L,
    ,
    drop = FALSE
  ]
  
  best_field <- best_candidate$field_name[1L]
  
  best_rule <- best_candidate$rule[1L]
  
  best_positive_rows <- best_candidate$positive_rows[1L]
  
  best_taxa <- best_candidate$taxa_with_positive_evidence[1L]
  
  best_areas <- best_candidate$areas_with_positive_evidence[1L]
  
  best_difference <- best_candidate$difference_from_reference_supported[1L]
  
} else {
  
  best_field <- NA_character_
  
  best_rule <- NA_character_
  
  best_positive_rows <- NA_integer_
  
  best_taxa <- NA_integer_
  
  best_areas <- NA_integer_
  
  best_difference <- NA_integer_
}


# ------------------------------------------------------------------------------
# 17. SUMMARY
# ------------------------------------------------------------------------------

summary_table <- data.frame(
  
  metric = c(
    "source_rows",
    "source_fields",
    "distinct_taxa",
    "distinct_botanical_areas",
    "distinct_taxon_area_combinations",
    "observed_taxa_x_areas",
    "complete_cross_product",
    "matches_expected_613887_rows",
    "reference_supported_combinations",
    "reference_unsupported_combinations",
    "reference_taxa_with_evidence",
    "reference_taxa_without_evidence",
    "candidate_evidence_fields_by_name",
    "candidate_evidence_rules_tested",
    "best_candidate_positive_rows",
    "best_candidate_taxa",
    "best_candidate_areas",
    "best_candidate_difference_from_reference"
  ),
  
  value = c(
    nrow(distribution),
    ncol(distribution),
    n_distinct_taxa,
    n_distinct_areas,
    n_distinct_combinations,
    expected_from_observed_dimensions,
    complete_cross_product,
    nrow(distribution) ==
      EXPECTED_COMBINATIONS,
    REFERENCE_SUPPORTED_COMBINATIONS,
    REFERENCE_UNSUPPORTED_COMBINATIONS,
    REFERENCE_TAXA_WITH_EVIDENCE,
    REFERENCE_TAXA_WITHOUT_EVIDENCE,
    nrow(candidate_fields),
    nrow(candidate_rules),
    best_positive_rows,
    best_taxa,
    best_areas,
    best_difference
  ),
  
  stringsAsFactors = FALSE
)


write.csv(
  summary_table,
  SUMMARY_FILE,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)


# ------------------------------------------------------------------------------
# 18. INTERPRETATION
# ------------------------------------------------------------------------------

interpretation_lines <- character(0)


interpretation_lines <- c(
  interpretation_lines,
  "VPJD Geography 06c0 - Distribution Evidence Semantics Audit",
  "",
  paste0(
    "Build date: ",
    BUILD_DATE
  ),
  "",
  paste0(
    "Source: ",
    SOURCE_FILE
  ),
  "",
  paste0(
    "Rows: ",
    format(
      nrow(distribution),
      big.mark = ","
    )
  ),
  paste0(
    "Distinct taxa: ",
    format(
      n_distinct_taxa,
      big.mark = ","
    )
  ),
  paste0(
    "Distinct botanical areas: ",
    n_distinct_areas
  ),
  paste0(
    "Distinct taxon x area combinations: ",
    format(
      n_distinct_combinations,
      big.mark = ","
    )
  ),
  ""
)


if (complete_cross_product) {
  
  interpretation_lines <- c(
    interpretation_lines,
    paste0(
      "The source is a complete taxon x botanical-area cross-product: ",
      format(
        n_distinct_taxa,
        big.mark = ","
      ),
      " x ",
      n_distinct_areas,
      " = ",
      format(
        expected_from_observed_dimensions,
        big.mark = ","
      ),
      " combinations."
    ),
    "",
    paste0(
      "Therefore row existence alone CANNOT be interpreted as positive ",
      "distribution evidence."
    ),
    ""
  )
  
} else {
  
  interpretation_lines <- c(
    interpretation_lines,
    paste0(
      "The source is not an exact complete taxon x botanical-area ",
      "cross-product."
    ),
    ""
  )
}


if (best_candidate_available) {
  
  interpretation_lines <- c(
    interpretation_lines,
    "Best automatically tested evidence interpretation:",
    paste0(
      "  field: ",
      best_field
    ),
    paste0(
      "  rule: ",
      best_rule
    ),
    paste0(
      "  positive rows: ",
      format(
        best_positive_rows,
        big.mark = ","
      )
    ),
    paste0(
      "  taxa with positive evidence: ",
      format(
        best_taxa,
        big.mark = ","
      )
    ),
    paste0(
      "  botanical areas represented: ",
      best_areas
    ),
    paste0(
      "  difference from reference supported count: ",
      format(
        best_difference,
        big.mark = ","
      )
    ),
    ""
  )
  
  
  if (
    best_positive_rows ==
    REFERENCE_SUPPORTED_COMBINATIONS &&
    best_taxa ==
    REFERENCE_TAXA_WITH_EVIDENCE
  ) {
    
    interpretation_lines <- c(
      interpretation_lines,
      paste0(
        "STRONG MATCH: this rule exactly reconstructs both the previously ",
        "validated supported-combination count and taxon-coverage count."
      ),
      "",
      paste0(
        "This field/rule is a strong candidate for explicit use in the ",
        "corrected geography_06c_build_taxon_geographic_distribution.R."
      )
    )
    
  } else {
    
    interpretation_lines <- c(
      interpretation_lines,
      paste0(
        "No automatically tested rule has yet been demonstrated to exactly ",
        "reconstruct the previously validated evidence counts."
      ),
      "",
      paste0(
        "Inspect the field-profile, low-cardinality and candidate-rule ",
        "outputs before changing geography_06c."
      )
    )
  }
  
} else {
  
  interpretation_lines <- c(
    interpretation_lines,
    paste0(
      "No candidate evidence rule could be generated automatically."
    ),
    "",
    paste0(
      "Inspect the schema and low-cardinality value audit manually before ",
      "changing geography_06c."
    )
  )
}


writeLines(
  interpretation_lines,
  INTERPRETATION_FILE,
  useBytes = TRUE
)


# ------------------------------------------------------------------------------
# 19. CONSOLE OUTPUT
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" DIMENSION SUMMARY\n")
cat("============================================================\n\n")

print(
  dimension_summary,
  row.names = FALSE
)


cat("\n")
cat("============================================================\n")
cat(" FIELD PROFILE\n")
cat("============================================================\n\n")

print(
  field_profile,
  row.names = FALSE
)


cat("\n")
cat("============================================================\n")
cat(" CROSS-PRODUCT TEST\n")
cat("============================================================\n\n")

cat(
  "Taxon field: ",
  taxon_field,
  "\n",
  sep = ""
)

cat(
  "Geographic field: ",
  geographic_field,
  "\n",
  sep = ""
)

cat(
  "Distinct taxa: ",
  format(
    n_distinct_taxa,
    big.mark = ","
  ),
  "\n",
  sep = ""
)

cat(
  "Distinct botanical areas: ",
  n_distinct_areas,
  "\n",
  sep = ""
)

cat(
  "Taxa x areas: ",
  format(
    expected_from_observed_dimensions,
    big.mark = ","
  ),
  "\n",
  sep = ""
)

cat(
  "Source rows: ",
  format(
    nrow(distribution),
    big.mark = ","
  ),
  "\n",
  sep = ""
)

cat(
  "Complete cross-product: ",
  complete_cross_product,
  "\n",
  sep = ""
)


cat("\n")
cat("============================================================\n")
cat(" CANDIDATE EVIDENCE FIELDS\n")
cat("============================================================\n\n")


if (nrow(candidate_fields) > 0L) {
  
  print(
    candidate_fields,
    row.names = FALSE
  )
  
} else {
  
  cat(
    "No evidence-like field names identified.\n"
  )
}


cat("\n")
cat("============================================================\n")
cat(" CANDIDATE EVIDENCE RULES\n")
cat("============================================================\n\n")


if (nrow(candidate_rules) > 0L) {
  
  print(
    candidate_rules,
    row.names = FALSE
  )
  
} else {
  
  cat(
    "No candidate evidence rules generated.\n"
  )
}


cat("\n")
cat("============================================================\n")
cat(" INTERPRETATION\n")
cat("============================================================\n\n")

cat(
  paste(
    interpretation_lines,
    collapse = "\n"
  ),
  "\n"
)


# ------------------------------------------------------------------------------
# 20. OUTPUTS
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" OUTPUTS\n")
cat("============================================================\n\n")

cat(
  "Schema:\n",
  SCHEMA_FILE,
  "\n\n",
  sep = ""
)

cat(
  "Field profile:\n",
  FIELD_PROFILE_FILE,
  "\n\n",
  sep = ""
)

cat(
  "Low-cardinality values:\n",
  LOW_CARDINALITY_FILE,
  "\n\n",
  sep = ""
)

cat(
  "Numeric-field profile:\n",
  NUMERIC_PROFILE_FILE,
  "\n\n",
  sep = ""
)

cat(
  "Candidate evidence fields:\n",
  CANDIDATE_FIELD_FILE,
  "\n\n",
  sep = ""
)

cat(
  "Candidate evidence rules:\n",
  CANDIDATE_RULE_FILE,
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
  "Interpretation:\n",
  INTERPRETATION_FILE,
  "\n\n",
  sep = ""
)


# ------------------------------------------------------------------------------
# 21. SAFEGUARDS
# ------------------------------------------------------------------------------

cat("============================================================\n")
cat(" SAFEGUARDS\n")
cat("============================================================\n\n")

cat("Source distribution table was NOT modified.\n")
cat("Published VPJD Taxonomic Release v1.0.0 was NOT modified.\n")
cat("Frozen geographic vocabulary was NOT modified.\n")
cat("No release distribution product was written.\n")
cat("Row existence was NOT assumed to represent presence.\n")
cat("Unsupported combinations were NOT converted to presence.\n")
cat("Missing evidence was NOT interpreted as absence.\n")
cat("Nakamura logic was NOT used.\n")
cat("Star categories were NOT assigned.\n")


# ------------------------------------------------------------------------------
# 22. NEXT STEP
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" NEXT STEP\n")
cat("============================================================\n\n")


if (
  best_candidate_available &&
  best_positive_rows ==
  REFERENCE_SUPPORTED_COMBINATIONS &&
  best_taxa ==
  REFERENCE_TAXA_WITH_EVIDENCE
) {
  
  cat(
    paste0(
      "A candidate evidence rule exactly reconstructs the previously ",
      "validated analytical counts.\n\n",
      "Review the candidate rule and low-cardinality audit, then revise:\n\n",
      "  geography_06c_build_taxon_geographic_distribution.R\n\n",
      "to require explicit positive evidence rather than interpreting every ",
      "taxon x area row as positive.\n"
    )
  )
  
} else {
  
  cat(
    paste0(
      "Evidence semantics have not yet been resolved automatically.\n\n",
      "Review the schema, low-cardinality values, numeric profile and ",
      "candidate-rule outputs before revising geography_06c.\n"
    )
  )
}


cat("\n")
cat("============================================================\n")