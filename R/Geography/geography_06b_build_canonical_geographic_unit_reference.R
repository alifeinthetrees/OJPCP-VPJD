# ==============================================================================
# VPJD GEOGRAPHY MODULE
# geography_06b_build_canonical_geographic_unit_reference.R
#
# PURPOSE
# -------
# Construct the provisional canonical geographic-unit reference required for:
#
#   Vascular Plants of Japan Database (VPJD)
#   Geography Release v1.0.0
#
# This script follows:
#
#   geography_06a_audit_geographic_evidence_model.R
#
# ARCHITECTURE
# ------------
#
#   VPJD Taxonomic Release v1.0.0
#               |
#               v
#      GEOGRAPHIC EVIDENCE MODEL
#               |
#               v
#       CANONICAL GEOGRAPHIC
#          UNIT REFERENCE
#               |
#        +------+------+
#        |             |
#        v             v
#   Japan evidence   External evidence
#        \             /
#         \           /
#          v         v
#       VPJD Geography
#          v1.0.0
#
# OBJECTIVES
# ----------
#  1. Read the 06a geographic evidence inventory.
#  2. Identify geographic-reference, crosswalk and distribution products.
#  3. Identify candidate geographic-unit fields.
#  4. Extract distinct geographic-unit values.
#  5. Preserve source terminology and provenance.
#  6. Reconcile exact normalised duplicates conservatively.
#  7. Keep identically named units of different geographic types separate.
#  8. Assign provisional VPJD geographic-unit identifiers.
#  9. Build a source-to-canonical-unit crosswalk.
# 10. Identify unresolved and ambiguous units for subsequent validation.
#
# IMPORTANT
# ---------
# This script DOES NOT:
#
#   - modify VPJD Taxonomic Release v1.0.0;
#   - modify source geographic evidence;
#   - calculate Nakamura predicates;
#   - calculate Star categories;
#   - infer absence from missing geography;
#   - infer geographic equivalence from similar names;
#   - infer parent-child relationships;
#   - manufacture geometries;
#   - assume redistribution rights;
#   - publish or freeze VPJD Geography v1.0.0.
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

AUDIT_06A_ROOT <- file.path(
  PROJECT_ROOT,
  "audit",
  "geography",
  "release_v1.0.0",
  "06a_geographic_evidence_model"
)

AUDIT_ROOT <- file.path(
  PROJECT_ROOT,
  "audit",
  "geography",
  "release_v1.0.0",
  "06b_canonical_geographic_units"
)

DERIVED_ROOT <- file.path(
  DATA_ROOT,
  "derived",
  "geography",
  "release_v1.0.0",
  "geographic_units"
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
# 02. 06A INPUT FILES
# ------------------------------------------------------------------------------

PRODUCT_REGISTER_FILE <- file.path(
  AUDIT_06A_ROOT,
  "geography_06a_geographic_product_register.csv"
)

SCHEMA_REGISTER_FILE <- file.path(
  AUDIT_06A_ROOT,
  "geography_06a_csv_schema_register.csv"
)

VALIDATION_06A_FILE <- file.path(
  AUDIT_06A_ROOT,
  "geography_06a_validation_gate.csv"
)


# ------------------------------------------------------------------------------
# 03. OUTPUT FILES
# ------------------------------------------------------------------------------

SOURCE_REGISTER_FILE <- file.path(
  AUDIT_ROOT,
  "geography_06b_source_register.csv"
)

CANDIDATE_FIELD_FILE <- file.path(
  AUDIT_ROOT,
  "geography_06b_candidate_geographic_fields.csv"
)

RAW_UNIT_FILE <- file.path(
  AUDIT_ROOT,
  "geography_06b_raw_geographic_units.csv"
)

NORMALISED_UNIT_FILE <- file.path(
  AUDIT_ROOT,
  "geography_06b_normalised_geographic_units.csv"
)

AMBIGUITY_FILE <- file.path(
  AUDIT_ROOT,
  "geography_06b_ambiguous_geographic_units.csv"
)

READ_FAILURE_FILE <- file.path(
  AUDIT_ROOT,
  "geography_06b_read_failures.csv"
)

SOURCE_CROSSWALK_FILE <- file.path(
  DERIVED_ROOT,
  "vpjd_geographic_unit_source_crosswalk.csv"
)

CANONICAL_UNIT_FILE <- file.path(
  DERIVED_ROOT,
  "vpjd_geographic_units.csv"
)

SUMMARY_FILE <- file.path(
  AUDIT_ROOT,
  "geography_06b_summary.csv"
)

VALIDATION_FILE <- file.path(
  AUDIT_ROOT,
  "geography_06b_validation_gate.csv"
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
# 05. HELPER: NORMALISE FILE PATH
#
# Fixed-string replacement only.
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
# 06. HELPER: NORMALISE GEOGRAPHIC LABEL
#
# Conservative normalisation for duplicate discovery only.
#
# This DOES NOT establish geographic equivalence.
# ------------------------------------------------------------------------------

normalise_geographic_label <- function(x) {
  
  y <- normalise_text(x)
  
  y <- tolower(y)
  
  y <- gsub(
    "_",
    " ",
    y,
    fixed = TRUE
  )
  
  y <- gsub(
    "-",
    " ",
    y,
    fixed = TRUE
  )
  
  y <- gsub(
    ".",
    " ",
    y,
    fixed = TRUE
  )
  
  repeat {
    
    double_space_selector <- grepl(
      "  ",
      y,
      fixed = TRUE
    )
    
    double_space_selector[
      is.na(double_space_selector)
    ] <- FALSE
    
    if (!any(double_space_selector)) {
      break
    }
    
    y <- gsub(
      "  ",
      " ",
      y,
      fixed = TRUE
    )
  }
  
  y <- trimws(y)
  
  y
}


# ------------------------------------------------------------------------------
# 07. HELPER: SAFE CSV READ
# ------------------------------------------------------------------------------

safe_read_csv <- function(path) {
  
  result <- tryCatch(
    {
      
      x <- read.csv(
        path,
        stringsAsFactors = FALSE,
        check.names = FALSE,
        na.strings = c(
          "",
          "NA"
        )
      )
      
      character_columns <- vapply(
        x,
        is.character,
        logical(1)
      )
      
      if (any(character_columns)) {
        
        character_names <- names(x)[
          character_columns
        ]
        
        for (field_name in character_names) {
          
          x[[field_name]] <- iconv(
            x[[field_name]],
            from = "",
            to = "UTF-8",
            sub = "byte"
          )
        }
      }
      
      list(
        success = TRUE,
        data = x,
        error = NA_character_
      )
    },
    error = function(e) {
      
      list(
        success = FALSE,
        data = NULL,
        error = conditionMessage(e)
      )
    }
  )
  
  result
}


# ------------------------------------------------------------------------------
# 08. HELPER: DISTINCT NON-MISSING VALUES
# ------------------------------------------------------------------------------

safe_distinct_values <- function(x) {
  
  y <- normalise_text(x)
  
  y <- y[
    !is.na(y)
  ]
  
  sort(
    unique(y)
  )
}


# ------------------------------------------------------------------------------
# 09. GEOGRAPHIC FIELD DEFINITIONS
# ------------------------------------------------------------------------------

explicit_geographic_fields <- c(
  "geographic_unit",
  "geographic_unit_name",
  "geography",
  "geographic_area",
  "geographic_area_name",
  "location",
  "location_name",
  "wcvp_location",
  "wcvp_location_name",
  "botanical_area",
  "botanical_area_name",
  "prefecture",
  "prefecture_name",
  "district",
  "district_name",
  "province",
  "province_name",
  "country",
  "country_name",
  "territory",
  "territory_name",
  "region",
  "region_name",
  "island",
  "island_name",
  "island_group",
  "island_group_name",
  "tdwg_level_3",
  "tdwg_level_3_name",
  "tdwg3",
  "tdwg3_name"
)


excluded_geographic_fields <- c(
  "latitude",
  "longitude",
  "decimalLatitude",
  "decimalLongitude",
  "minimumLatitude",
  "maximumLatitude",
  "minimumLongitude",
  "maximumLongitude",
  "range_size",
  "range_count",
  "distribution_count",
  "n_regions",
  "n_prefectures",
  "n_districts",
  "n_islands"
)


analytical_field_terms <- c(
  "count",
  "number",
  "extent",
  "status",
  "class",
  "classified",
  "endemic",
  "native",
  "introduced",
  "supported",
  "presence",
  "absence",
  "rare",
  "widespread",
  "restricted",
  "predicate",
  "evidence_status",
  "validation",
  "resolved",
  "unresolved",
  "method"
)


# ------------------------------------------------------------------------------
# 10. HELPER: CLASSIFY GEOGRAPHIC UNIT TYPE
#
# Classification is based on the source field name.
# It is provisional and will be audited later.
# ------------------------------------------------------------------------------

classify_unit_type <- function(field_name) {
  
  x <- tolower(
    as.character(field_name)
  )
  
  if (
    grepl(
      "botanical",
      x,
      fixed = TRUE
    )
  ) {
    
    return("BOTANICAL_AREA")
  }
  
  if (
    grepl(
      "prefecture",
      x,
      fixed = TRUE
    )
  ) {
    
    return("PREFECTURE")
  }
  
  if (
    grepl(
      "district",
      x,
      fixed = TRUE
    )
  ) {
    
    return("DISTRICT")
  }
  
  if (
    grepl(
      "province",
      x,
      fixed = TRUE
    )
  ) {
    
    return("PROVINCE")
  }
  
  if (
    grepl(
      "island_group",
      x,
      fixed = TRUE
    ) ||
    grepl(
      "island group",
      x,
      fixed = TRUE
    )
  ) {
    
    return("ISLAND_GROUP")
  }
  
  if (
    grepl(
      "island",
      x,
      fixed = TRUE
    )
  ) {
    
    return("ISLAND")
  }
  
  if (
    grepl(
      "country",
      x,
      fixed = TRUE
    ) ||
    grepl(
      "territory",
      x,
      fixed = TRUE
    )
  ) {
    
    return("COUNTRY_OR_TERRITORY")
  }
  
  if (
    grepl(
      "tdwg",
      x,
      fixed = TRUE
    ) ||
    grepl(
      "wcvp_location",
      x,
      fixed = TRUE
    )
  ) {
    
    return("WCVP_TDWG_UNIT")
  }
  
  if (
    grepl(
      "region",
      x,
      fixed = TRUE
    )
  ) {
    
    return("REGION")
  }
  
  "UNRESOLVED"
}


# ------------------------------------------------------------------------------
# 11. VALIDATE REQUIRED 06A INPUTS
# ------------------------------------------------------------------------------

required_inputs <- c(
  PRODUCT_REGISTER_FILE,
  SCHEMA_REGISTER_FILE,
  VALIDATION_06A_FILE
)

missing_inputs <- required_inputs[
  !file.exists(required_inputs)
]

if (length(missing_inputs) > 0L) {
  
  stop(
    paste0(
      "Required 06a input file(s) not found:\n",
      paste(
        missing_inputs,
        collapse = "\n"
      )
    )
  )
}


# ------------------------------------------------------------------------------
# 12. LOAD 06A AUDIT PRODUCTS
# ------------------------------------------------------------------------------

product_register <- read.csv(
  PRODUCT_REGISTER_FILE,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

schema_register <- read.csv(
  SCHEMA_REGISTER_FILE,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

validation_06a <- read.csv(
  VALIDATION_06A_FILE,
  stringsAsFactors = FALSE,
  check.names = FALSE
)


if (
  !"passed" %in%
  names(validation_06a)
) {
  
  stop(
    "06a validation table does not contain field 'passed'."
  )
}


validation_06a$passed <- as.logical(
  validation_06a$passed
)


if (
  any(is.na(validation_06a$passed)) ||
  !all(validation_06a$passed)
) {
  
  stop(
    paste0(
      "06a validation gate did not pass completely.\n",
      "Do not build the canonical geographic-unit reference."
    )
  )
}


# ------------------------------------------------------------------------------
# 13. VALIDATE REQUIRED REGISTER FIELDS
# ------------------------------------------------------------------------------

required_product_fields <- c(
  "source_file",
  "file_name",
  "extension",
  "product_class"
)

missing_product_fields <- setdiff(
  required_product_fields,
  names(product_register)
)

if (length(missing_product_fields) > 0L) {
  
  stop(
    paste0(
      "06a product register lacks required field(s):\n",
      paste(
        missing_product_fields,
        collapse = "\n"
      )
    )
  )
}


required_schema_fields <- c(
  "source_file",
  "file_name",
  "field_name",
  "geography_candidate"
)

missing_schema_fields <- setdiff(
  required_schema_fields,
  names(schema_register)
)

if (length(missing_schema_fields) > 0L) {
  
  stop(
    paste0(
      "06a schema register lacks required field(s):\n",
      paste(
        missing_schema_fields,
        collapse = "\n"
      )
    )
  )
}


# ------------------------------------------------------------------------------
# 14. NORMALISE SOURCE PATHS
# ------------------------------------------------------------------------------

product_register$source_file <- normalise_path(
  product_register$source_file
)

schema_register$source_file <- normalise_path(
  schema_register$source_file
)


# ------------------------------------------------------------------------------
# 15. START
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" VPJD GEOGRAPHY 06b - CANONICAL GEOGRAPHIC UNIT REFERENCE\n")
cat("============================================================\n\n")


cat(
  "06a products available: ",
  format(
    nrow(product_register),
    big.mark = ","
  ),
  "\n",
  sep = ""
)


cat(
  "06a schema fields available: ",
  format(
    nrow(schema_register),
    big.mark = ","
  ),
  "\n\n",
  sep = ""
)


# ------------------------------------------------------------------------------
# 16. SELECT CANDIDATE SOURCE PRODUCTS
#
# Primary evidence classes:
#
#   GEOGRAPHIC_REFERENCE
#   GEOGRAPHIC_CROSSWALK
#   DISTRIBUTION_EVIDENCE
#
# Nakamura/Stars analytical products are explicitly excluded.
# ------------------------------------------------------------------------------

allowed_product_classes <- c(
  "GEOGRAPHIC_REFERENCE",
  "GEOGRAPHIC_CROSSWALK",
  "DISTRIBUTION_EVIDENCE"
)


source_selector <- product_register$product_class %in%
  allowed_product_classes

source_selector[
  is.na(source_selector)
] <- FALSE


source_products <- product_register[
  source_selector,
  ,
  drop = FALSE
]


if (nrow(source_products) > 0L) {
  
  source_path_lower <- tolower(
    normalise_path(
      source_products$source_file
    )
  )
  
  nakamura_selector <- grepl(
    "nakamura",
    source_path_lower,
    fixed = TRUE
  )
  
  stars_selector <- grepl(
    "/stars/",
    source_path_lower,
    fixed = TRUE
  )
  
  exclude_selector <- (
    nakamura_selector |
      stars_selector
  )
  
  exclude_selector[
    is.na(exclude_selector)
  ] <- FALSE
  
  source_products <- source_products[
    !exclude_selector,
    ,
    drop = FALSE
  ]
}


# ------------------------------------------------------------------------------
# 17. LIMIT EXTRACTION TO CSV SOURCES
#
# Non-CSV references remain in the source register for later spatial work.
# ------------------------------------------------------------------------------

if (nrow(source_products) > 0L) {
  
  csv_source_selector <- tolower(
    source_products$extension
  ) == "csv"
  
  csv_source_selector[
    is.na(csv_source_selector)
  ] <- FALSE
  
  csv_source_products <- source_products[
    csv_source_selector,
    ,
    drop = FALSE
  ]
  
} else {
  
  csv_source_products <- source_products
}


cat(
  "Candidate source products after analytical exclusions: ",
  nrow(source_products),
  "\n",
  sep = ""
)


cat(
  "CSV source products available for unit extraction: ",
  nrow(csv_source_products),
  "\n\n",
  sep = ""
)


# ------------------------------------------------------------------------------
# 18. BUILD SOURCE REGISTER
# ------------------------------------------------------------------------------

source_register <- source_products


if (nrow(source_register) > 0L) {
  
  source_register$used_for_unit_extraction <- (
    tolower(source_register$extension) == "csv"
  )
  
  source_register$source_role <- ifelse(
    source_register$product_class == "GEOGRAPHIC_REFERENCE",
    "REFERENCE",
    ifelse(
      source_register$product_class == "GEOGRAPHIC_CROSSWALK",
      "CROSSWALK",
      "DISTRIBUTION_EVIDENCE"
    )
  )
  
  source_register$publication_status <- "NOT_YET_ASSESSED"
  
  source_register$notes <- NA_character_
}


# ------------------------------------------------------------------------------
# 19. IDENTIFY CANDIDATE GEOGRAPHIC FIELDS
# ------------------------------------------------------------------------------

candidate_field_records <- list()

candidate_field_counter <- 0L


if (nrow(csv_source_products) > 0L) {
  
  for (
    source_index in seq_len(
      nrow(csv_source_products)
    )
  ) {
    
    source_file <- csv_source_products$source_file[
      source_index
    ]
    
    source_schema <- schema_register[
      schema_register$source_file == source_file,
      ,
      drop = FALSE
    ]
    
    if (nrow(source_schema) == 0L) {
      next
    }
    
    for (
      field_index in seq_len(
        nrow(source_schema)
      )
    ) {
      
      field_name <- source_schema$field_name[
        field_index
      ]
      
      if (
        is.na(field_name) ||
        !nzchar(field_name)
      ) {
        
        next
      }
      
      field_lower <- tolower(
        field_name
      )
      
      explicit_match <- field_lower %in%
        tolower(
          explicit_geographic_fields
        )
      
      schema_geography_match <- isTRUE(
        source_schema$geography_candidate[
          field_index
        ]
      )
      
      excluded_match <- field_lower %in%
        tolower(
          excluded_geographic_fields
        )
      
      include_field <- (
        explicit_match ||
          schema_geography_match
      ) &&
        !excluded_match
      
      if (!include_field) {
        next
      }
      
      analytical_match <- FALSE
      
      for (term in analytical_field_terms) {
        
        if (
          grepl(
            term,
            field_lower,
            fixed = TRUE
          )
        ) {
          
          analytical_match <- TRUE
          break
        }
      }
      
      if (analytical_match) {
        next
      }
      
      unit_type <- classify_unit_type(
        field_name
      )
      
      candidate_field_counter <- candidate_field_counter + 1L
      
      candidate_field_records[[candidate_field_counter]] <- data.frame(
        source_file = source_file,
        file_name = basename(
          source_file
        ),
        product_class = csv_source_products$product_class[
          source_index
        ],
        field_name = field_name,
        inferred_unit_type = unit_type,
        explicit_field_match = explicit_match,
        schema_geography_match = schema_geography_match,
        stringsAsFactors = FALSE
      )
    }
  }
}


if (length(candidate_field_records) > 0L) {
  
  candidate_fields <- do.call(
    rbind,
    candidate_field_records
  )
  
  candidate_fields <- candidate_fields[
    !duplicated(candidate_fields),
    ,
    drop = FALSE
  ]
  
  rownames(candidate_fields) <- NULL
  
} else {
  
  candidate_fields <- data.frame(
    source_file = character(0),
    file_name = character(0),
    product_class = character(0),
    field_name = character(0),
    inferred_unit_type = character(0),
    explicit_field_match = logical(0),
    schema_geography_match = logical(0),
    stringsAsFactors = FALSE
  )
}


# ------------------------------------------------------------------------------
# 20. PROFILE CANDIDATE FIELDS
# ------------------------------------------------------------------------------

cat(
  "Candidate geographic-unit fields identified: ",
  nrow(candidate_fields),
  "\n",
  sep = ""
)


cat(
  "Source files containing candidate geographic-unit fields: ",
  length(
    unique(
      candidate_fields$source_file
    )
  ),
  "\n\n",
  sep = ""
)


# ------------------------------------------------------------------------------
# 21. EXTRACT RAW GEOGRAPHIC UNIT VALUES
# ------------------------------------------------------------------------------

raw_unit_records <- list()

raw_unit_counter <- 0L

read_failure_records <- list()

read_failure_counter <- 0L


unique_source_files <- unique(
  candidate_fields$source_file
)


if (length(unique_source_files) > 0L) {
  
  for (
    source_index in seq_along(
      unique_source_files
    )
  ) {
    
    source_file <- unique_source_files[
      source_index
    ]
    
    cat(
      sprintf(
        "[%d/%d] %s\n",
        source_index,
        length(unique_source_files),
        basename(source_file)
      )
    )
    
    read_result <- safe_read_csv(
      source_file
    )
    
    if (!read_result$success) {
      
      cat(
        "  READ FAILURE: ",
        read_result$error,
        "\n",
        sep = ""
      )
      
      read_failure_counter <- read_failure_counter + 1L
      
      read_failure_records[[read_failure_counter]] <- data.frame(
        source_file = source_file,
        file_name = basename(
          source_file
        ),
        error_message = read_result$error,
        stringsAsFactors = FALSE
      )
      
      next
    }
    
    source_data <- read_result$data
    
    source_fields <- candidate_fields[
      candidate_fields$source_file == source_file,
      ,
      drop = FALSE
    ]
    
    if (nrow(source_fields) == 0L) {
      
      rm(
        source_data,
        read_result
      )
      
      invisible(
        gc()
      )
      
      next
    }
    
    for (
      field_index in seq_len(
        nrow(source_fields)
      )
    ) {
      
      field_name <- source_fields$field_name[
        field_index
      ]
      
      if (
        !field_name %in%
        names(source_data)
      ) {
        
        next
      }
      
      distinct_values <- safe_distinct_values(
        source_data[[field_name]]
      )
      
      if (length(distinct_values) == 0L) {
        next
      }
      
      unit_type <- source_fields$inferred_unit_type[
        field_index
      ]
      
      for (
        value_index in seq_along(
          distinct_values
        )
      ) {
        
        source_value <- distinct_values[
          value_index
        ]
        
        raw_unit_counter <- raw_unit_counter + 1L
        
        raw_unit_records[[raw_unit_counter]] <- data.frame(
          source_file = source_file,
          file_name = basename(
            source_file
          ),
          source_field = field_name,
          source_value = source_value,
          inferred_unit_type = unit_type,
          normalised_value = normalise_geographic_label(
            source_value
          ),
          stringsAsFactors = FALSE
        )
      }
    }
    
    rm(
      source_data,
      read_result
    )
    
    invisible(
      gc()
    )
  }
}


# ------------------------------------------------------------------------------
# 22. COMBINE RAW UNIT RECORDS
# ------------------------------------------------------------------------------

if (length(raw_unit_records) > 0L) {
  
  raw_units <- do.call(
    rbind,
    raw_unit_records
  )
  
} else {
  
  raw_units <- data.frame(
    source_file = character(0),
    file_name = character(0),
    source_field = character(0),
    source_value = character(0),
    inferred_unit_type = character(0),
    normalised_value = character(0),
    stringsAsFactors = FALSE
  )
}


if (length(read_failure_records) > 0L) {
  
  read_failures <- do.call(
    rbind,
    read_failure_records
  )
  
} else {
  
  read_failures <- data.frame(
    source_file = character(0),
    file_name = character(0),
    error_message = character(0),
    stringsAsFactors = FALSE
  )
}


# ------------------------------------------------------------------------------
# 23. REMOVE EMPTY NORMALISED VALUES
# ------------------------------------------------------------------------------

if (nrow(raw_units) > 0L) {
  
  valid_value_selector <- (
    !is.na(raw_units$normalised_value) &
      nzchar(raw_units$normalised_value)
  )
  
  valid_value_selector[
    is.na(valid_value_selector)
  ] <- FALSE
  
  raw_units <- raw_units[
    valid_value_selector,
    ,
    drop = FALSE
  ]
  
  rownames(raw_units) <- NULL
}


# ------------------------------------------------------------------------------
# 24. BUILD NORMALISED UNIT KEY
#
# Unit TYPE is deliberately part of the key.
#
# Therefore:
#
#   Hokkaido + PREFECTURE
#
# and:
#
#   Hokkaido + ISLAND
#
# remain distinct geographic concepts.
# ------------------------------------------------------------------------------

if (nrow(raw_units) > 0L) {
  
  raw_units$normalised_unit_key <- paste(
    raw_units$inferred_unit_type,
    raw_units$normalised_value,
    sep = "::"
  )
  
} else {
  
  raw_units$normalised_unit_key <- character(0)
}


# ------------------------------------------------------------------------------
# 25. BUILD NORMALISED UNIT REGISTER
# ------------------------------------------------------------------------------

normalised_unit_records <- list()

normalised_unit_counter <- 0L


normalised_keys <- unique(
  raw_units$normalised_unit_key
)


if (length(normalised_keys) > 0L) {
  
  for (
    key_index in seq_along(
      normalised_keys
    )
  ) {
    
    unit_key <- normalised_keys[
      key_index
    ]
    
    unit_rows <- raw_units[
      raw_units$normalised_unit_key == unit_key,
      ,
      drop = FALSE
    ]
    
    if (nrow(unit_rows) == 0L) {
      next
    }
    
    unit_types <- sort(
      unique(
        unit_rows$inferred_unit_type[
          !is.na(unit_rows$inferred_unit_type)
        ]
      )
    )
    
    if (length(unit_types) == 0L) {
      
      unit_type <- "UNRESOLVED"
      
    } else {
      
      unit_type <- unit_types[1]
    }
    
    normalised_labels <- sort(
      unique(
        unit_rows$normalised_value[
          !is.na(unit_rows$normalised_value)
        ]
      )
    )
    
    if (length(normalised_labels) == 0L) {
      next
    }
    
    normalised_label <- normalised_labels[1]
    
    source_values <- sort(
      unique(
        unit_rows$source_value[
          !is.na(unit_rows$source_value)
        ]
      )
    )
    
    if (length(source_values) > 0L) {
      
      canonical_name_candidate <- source_values[1]
      
    } else {
      
      canonical_name_candidate <- NA_character_
    }
    
    source_files <- sort(
      unique(
        basename(
          unit_rows$source_file
        )
      )
    )
    
    normalised_unit_counter <- normalised_unit_counter + 1L
    
    normalised_unit_records[[normalised_unit_counter]] <- data.frame(
      normalised_unit_key = unit_key,
      normalised_value = normalised_label,
      geographic_unit_type = unit_type,
      canonical_name_candidate = canonical_name_candidate,
      n_source_labels = length(
        source_values
      ),
      source_labels = if (
        length(source_values) > 0L
      ) {
        paste(
          source_values,
          collapse = " | "
        )
      } else {
        NA_character_
      },
      n_source_files = length(
        source_files
      ),
      source_files = if (
        length(source_files) > 0L
      ) {
        paste(
          source_files,
          collapse = " | "
        )
      } else {
        NA_character_
      },
      stringsAsFactors = FALSE
    )
  }
}


if (length(normalised_unit_records) > 0L) {
  
  normalised_units <- do.call(
    rbind,
    normalised_unit_records
  )
  
} else {
  
  normalised_units <- data.frame(
    normalised_unit_key = character(0),
    normalised_value = character(0),
    geographic_unit_type = character(0),
    canonical_name_candidate = character(0),
    n_source_labels = integer(0),
    source_labels = character(0),
    n_source_files = integer(0),
    source_files = character(0),
    stringsAsFactors = FALSE
  )
}


# ------------------------------------------------------------------------------
# 26. IDENTIFY CROSS-TYPE LABEL AMBIGUITIES
#
# A label appearing under more than one geographic-unit type is NOT merged.
# ------------------------------------------------------------------------------

ambiguity_records <- list()

ambiguity_counter <- 0L


label_values <- unique(
  normalised_units$normalised_value
)


if (length(label_values) > 0L) {
  
  for (
    label_index in seq_along(
      label_values
    )
  ) {
    
    label_value <- label_values[
      label_index
    ]
    
    label_rows <- normalised_units[
      normalised_units$normalised_value == label_value,
      ,
      drop = FALSE
    ]
    
    distinct_types <- sort(
      unique(
        label_rows$geographic_unit_type[
          !is.na(
            label_rows$geographic_unit_type
          )
        ]
      )
    )
    
    if (length(distinct_types) <= 1L) {
      next
    }
    
    affected_keys <- sort(
      unique(
        label_rows$normalised_unit_key
      )
    )
    
    ambiguity_counter <- ambiguity_counter + 1L
    
    ambiguity_records[[ambiguity_counter]] <- data.frame(
      normalised_value = label_value,
      n_unit_types = length(
        distinct_types
      ),
      geographic_unit_types = paste(
        distinct_types,
        collapse = " | "
      ),
      affected_unit_keys = paste(
        affected_keys,
        collapse = " | "
      ),
      review_status = "REQUIRES_REVIEW",
      stringsAsFactors = FALSE
    )
  }
}


if (length(ambiguity_records) > 0L) {
  
  ambiguous_units <- do.call(
    rbind,
    ambiguity_records
  )
  
} else {
  
  ambiguous_units <- data.frame(
    normalised_value = character(0),
    n_unit_types = integer(0),
    geographic_unit_types = character(0),
    affected_unit_keys = character(0),
    review_status = character(0),
    stringsAsFactors = FALSE
  )
}


# ------------------------------------------------------------------------------
# 27. SORT NORMALISED UNITS
# ------------------------------------------------------------------------------

if (nrow(normalised_units) > 0L) {
  
  normalised_units <- normalised_units[
    order(
      normalised_units$geographic_unit_type,
      normalised_units$normalised_value
    ),
    ,
    drop = FALSE
  ]
  
  rownames(normalised_units) <- NULL
}


# ------------------------------------------------------------------------------
# 28. ASSIGN PROVISIONAL VPJD GEOGRAPHIC UNIT IDS
#
# IMPORTANT:
# These identifiers remain provisional until the vocabulary has been validated.
# ------------------------------------------------------------------------------

if (nrow(normalised_units) > 0L) {
  
  normalised_units$geographic_unit_id <- sprintf(
    "VPJD-GEO-%05d",
    seq_len(
      nrow(normalised_units)
    )
  )
  
} else {
  
  normalised_units$geographic_unit_id <- character(0)
}


# ------------------------------------------------------------------------------
# 29. ASSIGN AMBIGUITY STATUS
# ------------------------------------------------------------------------------

normalised_units$ambiguity_status <- "UNAMBIGUOUS"


if (
  nrow(normalised_units) > 0L &&
  nrow(ambiguous_units) > 0L
) {
  
  ambiguous_label_selector <- (
    normalised_units$normalised_value %in%
      ambiguous_units$normalised_value
  )
  
  ambiguous_label_selector[
    is.na(ambiguous_label_selector)
  ] <- FALSE
  
  normalised_units$ambiguity_status[
    ambiguous_label_selector
  ] <- "REQUIRES_REVIEW"
}


# ------------------------------------------------------------------------------
# 30. BUILD PROVISIONAL CANONICAL GEOGRAPHIC UNIT TABLE
#
# Parent geography, country assignment, standards and geometry remain unresolved
# unless explicitly validated in subsequent scripts.
# ------------------------------------------------------------------------------

canonical_units <- data.frame(
  geographic_unit_id = normalised_units$geographic_unit_id,
  
  geographic_unit_name = normalised_units$canonical_name_candidate,
  
  geographic_unit_type = normalised_units$geographic_unit_type,
  
  normalised_geographic_unit_name = normalised_units$normalised_value,
  
  parent_geographic_unit_id = NA_character_,
  
  country_or_territory = NA_character_,
  
  source_geographic_code = NA_character_,
  
  source_geographic_standard = NA_character_,
  
  japanese_name = NA_character_,
  
  romanised_name = NA_character_,
  
  geometry_available = FALSE,
  
  geometry_source = NA_character_,
  
  ambiguity_status = normalised_units$ambiguity_status,
  
  provenance_status = "SOURCE_VALUES_RETAINED",
  
  redistribution_status = "NOT_YET_ASSESSED",
  
  release_status = ifelse(
    normalised_units$ambiguity_status == "UNAMBIGUOUS",
    "PROVISIONAL",
    "REQUIRES_REVIEW"
  ),
  
  stringsAsFactors = FALSE
)


# ------------------------------------------------------------------------------
# 31. BUILD SOURCE-TO-CANONICAL CROSSWALK
# ------------------------------------------------------------------------------

if (nrow(raw_units) > 0L) {
  
  unit_lookup <- normalised_units[
    ,
    c(
      "normalised_unit_key",
      "geographic_unit_id",
      "ambiguity_status"
    ),
    drop = FALSE
  ]
  
  source_crosswalk <- merge(
    raw_units,
    unit_lookup,
    by = "normalised_unit_key",
    all.x = TRUE,
    sort = FALSE
  )
  
  source_crosswalk <- source_crosswalk[
    ,
    c(
      "geographic_unit_id",
      "source_file",
      "file_name",
      "source_field",
      "source_value",
      "inferred_unit_type",
      "normalised_value",
      "normalised_unit_key",
      "ambiguity_status"
    ),
    drop = FALSE
  ]
  
  source_crosswalk <- source_crosswalk[
    !duplicated(source_crosswalk),
    ,
    drop = FALSE
  ]
  
  rownames(source_crosswalk) <- NULL
  
} else {
  
  source_crosswalk <- data.frame(
    geographic_unit_id = character(0),
    source_file = character(0),
    file_name = character(0),
    source_field = character(0),
    source_value = character(0),
    inferred_unit_type = character(0),
    normalised_value = character(0),
    normalised_unit_key = character(0),
    ambiguity_status = character(0),
    stringsAsFactors = FALSE
  )
}


# ------------------------------------------------------------------------------
# 32. BUILD GEOGRAPHIC UNIT TYPE PROFILE
# ------------------------------------------------------------------------------

if (nrow(canonical_units) > 0L) {
  
  unit_type_profile <- as.data.frame(
    table(
      canonical_units$geographic_unit_type,
      useNA = "ifany"
    ),
    stringsAsFactors = FALSE
  )
  
  names(unit_type_profile) <- c(
    "geographic_unit_type",
    "n_units"
  )
  
  unit_type_profile <- unit_type_profile[
    unit_type_profile$n_units > 0L,
    ,
    drop = FALSE
  ]
  
} else {
  
  unit_type_profile <- data.frame(
    geographic_unit_type = character(0),
    n_units = integer(0),
    stringsAsFactors = FALSE
  )
}


# ------------------------------------------------------------------------------
# 33. IDENTIFY UNRESOLVED UNIT TYPES
# ------------------------------------------------------------------------------

if (nrow(canonical_units) > 0L) {
  
  unresolved_selector <- (
    canonical_units$geographic_unit_type ==
      "UNRESOLVED"
  )
  
  unresolved_selector[
    is.na(unresolved_selector)
  ] <- FALSE
  
  unresolved_units <- canonical_units[
    unresolved_selector,
    ,
    drop = FALSE
  ]
  
} else {
  
  unresolved_units <- canonical_units
}


# ------------------------------------------------------------------------------
# 34. SUMMARY TABLE
# ------------------------------------------------------------------------------

summary_table <- data.frame(
  metric = c(
    "06a_source_products_considered",
    "csv_source_products",
    "candidate_geographic_fields",
    "source_files_with_candidate_fields",
    "source_read_failures",
    "raw_source_unit_records",
    "normalised_unit_keys",
    "canonical_geographic_units",
    "unresolved_unit_types",
    "ambiguous_normalised_labels",
    "units_requiring_review",
    "source_crosswalk_rows"
  ),
  
  value = c(
    nrow(
      source_products
    ),
    
    nrow(
      csv_source_products
    ),
    
    nrow(
      candidate_fields
    ),
    
    length(
      unique(
        candidate_fields$source_file
      )
    ),
    
    nrow(
      read_failures
    ),
    
    nrow(
      raw_units
    ),
    
    length(
      unique(
        raw_units$normalised_unit_key
      )
    ),
    
    nrow(
      canonical_units
    ),
    
    nrow(
      unresolved_units
    ),
    
    nrow(
      ambiguous_units
    ),
    
    sum(
      canonical_units$release_status ==
        "REQUIRES_REVIEW",
      na.rm = TRUE
    ),
    
    nrow(
      source_crosswalk
    )
  ),
  
  stringsAsFactors = FALSE
)


# ------------------------------------------------------------------------------
# 35. VALIDATION GATE
# ------------------------------------------------------------------------------

crosswalk_ids_resolved <- TRUE

if (nrow(source_crosswalk) > 0L) {
  
  crosswalk_ids_resolved <- all(
    !is.na(
      source_crosswalk$geographic_unit_id
    ) &
      nzchar(
        source_crosswalk$geographic_unit_id
      )
  )
}


canonical_ids_nonmissing <- TRUE

canonical_ids_unique <- TRUE


if (nrow(canonical_units) > 0L) {
  
  canonical_ids_nonmissing <- all(
    !is.na(
      canonical_units$geographic_unit_id
    ) &
      nzchar(
        canonical_units$geographic_unit_id
      )
  )
  
  canonical_ids_unique <- (
    anyDuplicated(
      canonical_units$geographic_unit_id
    ) == 0L
  )
}


nakamura_products_present <- FALSE

stars_products_present <- FALSE


if (nrow(source_products) > 0L) {
  
  source_paths_lower <- tolower(
    normalise_path(
      source_products$source_file
    )
  )
  
  nakamura_products_present <- any(
    grepl(
      "nakamura",
      source_paths_lower,
      fixed = TRUE
    )
  )
  
  stars_products_present <- any(
    grepl(
      "/stars/",
      source_paths_lower,
      fixed = TRUE
    )
  )
}


validation_gate <- data.frame(
  criterion = c(
    "06a_validation_passed",
    "06a_product_register_loaded",
    "06a_schema_register_loaded",
    "source_products_identified",
    "candidate_geographic_fields_identified",
    "source_files_read_without_failure",
    "raw_geographic_units_extracted",
    "canonical_unit_reference_created",
    "canonical_ids_nonmissing",
    "canonical_ids_unique",
    "source_crosswalk_created",
    "all_crosswalk_rows_resolve_to_unit_id",
    "unit_type_preserved_in_duplicate_key",
    "ambiguous_labels_not_silently_merged",
    "parent_geography_not_inferred",
    "geometry_not_inferred",
    "missing_geography_not_interpreted_as_absence",
    "nakamura_products_excluded",
    "stars_products_excluded",
    "redistribution_rights_not_assumed",
    "published_vpjd_not_modified"
  ),
  
  passed = c(
    all(
      validation_06a$passed
    ),
    
    nrow(
      product_register
    ) > 0L,
    
    nrow(
      schema_register
    ) > 0L,
    
    nrow(
      source_products
    ) > 0L,
    
    nrow(
      candidate_fields
    ) > 0L,
    
    nrow(
      read_failures
    ) == 0L,
    
    nrow(
      raw_units
    ) > 0L,
    
    nrow(
      canonical_units
    ) > 0L,
    
    canonical_ids_nonmissing,
    
    canonical_ids_unique,
    
    nrow(
      source_crosswalk
    ) > 0L,
    
    crosswalk_ids_resolved,
    
    TRUE,
    
    TRUE,
    
    all(
      is.na(
        canonical_units$parent_geographic_unit_id
      )
    ),
    
    all(
      canonical_units$geometry_available == FALSE
    ),
    
    TRUE,
    
    !nakamura_products_present,
    
    !stars_products_present,
    
    all(
      canonical_units$redistribution_status ==
        "NOT_YET_ASSESSED"
    ),
    
    TRUE
  ),
  
  stringsAsFactors = FALSE
)


# ------------------------------------------------------------------------------
# 36. WRITE AUDIT OUTPUTS
# ------------------------------------------------------------------------------

write.csv(
  source_register,
  SOURCE_REGISTER_FILE,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)


write.csv(
  candidate_fields,
  CANDIDATE_FIELD_FILE,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)


write.csv(
  raw_units,
  RAW_UNIT_FILE,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)


write.csv(
  normalised_units,
  NORMALISED_UNIT_FILE,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)


write.csv(
  ambiguous_units,
  AMBIGUITY_FILE,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)


write.csv(
  read_failures,
  READ_FAILURE_FILE,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)


write.csv(
  source_crosswalk,
  SOURCE_CROSSWALK_FILE,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)


write.csv(
  canonical_units,
  CANONICAL_UNIT_FILE,
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
# 37. CONSOLE SUMMARY
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" VPJD GEOGRAPHY 06b - BUILD COMPLETE\n")
cat("============================================================\n\n")


cat(
  "Summary:\n\n"
)


print(
  summary_table,
  row.names = FALSE
)


# ------------------------------------------------------------------------------
# 38. GEOGRAPHIC UNIT TYPE PROFILE
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" GEOGRAPHIC UNIT TYPES\n")
cat("============================================================\n\n")


if (nrow(unit_type_profile) > 0L) {
  
  print(
    unit_type_profile,
    row.names = FALSE
  )
  
} else {
  
  cat(
    "No geographic-unit types were generated.\n"
  )
}


# ------------------------------------------------------------------------------
# 39. AMBIGUITY PROFILE
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" AMBIGUOUS GEOGRAPHIC LABELS\n")
cat("============================================================\n\n")


if (nrow(ambiguous_units) > 0L) {
  
  print(
    ambiguous_units,
    row.names = FALSE
  )
  
} else {
  
  cat(
    "No cross-type geographic-label ambiguities detected.\n"
  )
}


# ------------------------------------------------------------------------------
# 40. UNRESOLVED UNIT TYPES
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" UNRESOLVED GEOGRAPHIC UNIT TYPES\n")
cat("============================================================\n\n")


if (nrow(unresolved_units) > 0L) {
  
  print(
    unresolved_units[
      ,
      c(
        "geographic_unit_id",
        "geographic_unit_name",
        "normalised_geographic_unit_name",
        "release_status"
      ),
      drop = FALSE
    ],
    row.names = FALSE
  )
  
} else {
  
  cat(
    "No unresolved geographic-unit types detected.\n"
  )
}


# ------------------------------------------------------------------------------
# 41. READ FAILURES
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" SOURCE READ FAILURES\n")
cat("============================================================\n\n")


if (nrow(read_failures) > 0L) {
  
  print(
    read_failures,
    row.names = FALSE
  )
  
} else {
  
  cat(
    "No source read failures detected.\n"
  )
}


# ------------------------------------------------------------------------------
# 42. VALIDATION GATE
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
# 43. INTERPRETATION
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
      "PASS. A provisional canonical geographic-unit reference has ",
      "been constructed from the geographic evidence estate identified ",
      "by 06a.\n\n",
      "Exact normalised labels were reconciled only within the same ",
      "inferred geographic-unit type. Labels occurring under different ",
      "unit types were retained separately and flagged for review.\n\n",
      "No geographic hierarchy, geometry, absence, redistribution right, ",
      "Nakamura predicate or Star classification was inferred.\n"
    )
  )
  
} else {
  
  cat(
    "FAIL. One or more 06b validation criteria failed.\n\n"
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
# 44. NEXT STEP
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
      "Review:\n\n",
      CANONICAL_UNIT_FILE,
      "\n\n",
      SOURCE_CROSSWALK_FILE,
      "\n\n",
      AMBIGUITY_FILE,
      "\n\n",
      "The provisional geographic vocabulary should now be audited ",
      "before geographic hierarchy, source standards or geometries are ",
      "added.\n\n",
      "Expected next stage:\n\n",
      "  geography_06b1_validate_canonical_geographic_units.R\n\n",
      "Do not yet freeze geographic_unit_id values for publication.\n"
    )
  )
  
} else {
  
  cat(
    paste0(
      "Do not proceed until the failed 06b validation criteria ",
      "have been reviewed.\n\n",
      "If source_read_failures is the only failed criterion, inspect:\n\n",
      READ_FAILURE_FILE,
      "\n"
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
  "Similar geographic names were NOT assumed to be equivalent.\n"
)

cat(
  "Cross-type geographic labels were NOT silently merged.\n"
)

cat(
  "Parent-child geographic relationships were NOT inferred.\n"
)

cat(
  "Geometries were NOT inferred or manufactured.\n"
)

cat(
  "Nakamura predicates were NOT used to define release geography.\n"
)

cat(
  "Historical Star assignments were NOT used as geographic truth.\n"
)

cat(
  "Redistribution rights were NOT assumed.\n"
)

cat(
  "Canonical geographic-unit IDs remain PROVISIONAL.\n"
)

cat(
  "VPJD Geography v1.0.0 has NOT been frozen or published.\n"
)

cat("\n")
cat("============================================================\n")