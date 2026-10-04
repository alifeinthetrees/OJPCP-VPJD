# ==============================================================================
# VPJD GEOGRAPHY MODULE
# geography_05a6_decode_historical_wcvp_location_ids.R
#
# PURPOSE
#
# Decode the historical field `wcvp_location_ids` in:
#
#   outputs/tables/stars/wcvp_distribution/
#   stars03b_taxon_distribution_summary.csv
#
# geography_05a5 demonstrated that treating `wcvp_location_ids` as a
# delimiter-separated list of actual WCVP location identifiers was incorrect.
#
# This script asks:
#
#   1. What is the data type and value distribution of wcvp_location_ids?
#   2. Is it actually a count of distinct WCVP location IDs?
#   3. How does it relate to wcvp_distribution_rows?
#   4. Does the historical 03b script reveal how it was calculated?
#   5. What upstream object supplied the underlying WCVP distribution records?
#   6. Can that upstream source still be located in the project?
#
# IMPORTANT
#
# This is a READ-ONLY diagnostic script.
#
# It does NOT:
#
#   * reconstruct global ranges;
#   * assign Star categories;
#   * interpret missing evidence as absence;
#   * modify historical outputs;
#   * modify published VPJD;
#   * modify occurrence data;
#   * modify the current taxon range matrix.
#
# ==============================================================================


# ------------------------------------------------------------------------------
# 01. PATHS
# ------------------------------------------------------------------------------

PROJECT_ROOT <- "I:/R/OJPCP/VPJD-OJPCP"

HISTORICAL_03B_SCRIPT <- file.path(
  PROJECT_ROOT,
  "R",
  "stars",
  "03b_profile_wcvp_distribution.R"
)

SUMMARY_FILE <- file.path(
  PROJECT_ROOT,
  "outputs",
  "tables",
  "stars",
  "wcvp_distribution",
  "stars03b_taxon_distribution_summary.csv"
)

AUDIT_ROOT <- file.path(
  PROJECT_ROOT,
  "audit",
  "geography",
  "taxon_range",
  "wcvp_location_decode"
)

dir.create(
  AUDIT_ROOT,
  recursive = TRUE,
  showWarnings = FALSE
)


# ------------------------------------------------------------------------------
# 02. START
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" VPJD GEOGRAPHY 05a6 - DECODE HISTORICAL WCVP LOCATION FIELD\n")
cat("============================================================\n\n")


# ------------------------------------------------------------------------------
# 03. HELPERS
# ------------------------------------------------------------------------------

normalise_name <- function(x) {
  
  x <- tolower(
    trimws(
      as.character(x)
    )
  )
  
  x <- gsub(
    "[^a-z0-9]+",
    "_",
    x
  )
  
  x <- gsub(
    "^_+|_+$",
    "",
    x
  )
  
  x
}


clean_character <- function(x) {
  
  x <- trimws(
    as.character(x)
  )
  
  x[
    is.na(x) |
      x %in% c(
        "",
        "NA",
        "NaN",
        "NULL",
        "<NA>"
      )
  ] <- NA_character_
  
  x
}


safe_numeric <- function(x) {
  
  suppressWarnings(
    as.numeric(
      clean_character(x)
    )
  )
}


safe_read_csv <- function(path) {
  
  if (!file.exists(path)) {
    return(NULL)
  }
  
  tryCatch(
    read.csv(
      path,
      stringsAsFactors = FALSE,
      check.names = FALSE,
      encoding = "UTF-8"
    ),
    error = function(e1) {
      
      tryCatch(
        read.csv(
          path,
          stringsAsFactors = FALSE,
          check.names = FALSE
        ),
        error = function(e2) {
          NULL
        }
      )
    }
  )
}


safe_read_lines <- function(path) {
  
  if (!file.exists(path)) {
    return(character(0))
  }
  
  tryCatch(
    readLines(
      path,
      warn = FALSE,
      encoding = "UTF-8"
    ),
    error = function(e1) {
      
      tryCatch(
        readLines(
          path,
          warn = FALSE
        ),
        error = function(e2) {
          character(0)
        }
      )
    }
  )
}


write_audit <- function(x, filename) {
  
  write.csv(
    x,
    file.path(
      AUDIT_ROOT,
      filename
    ),
    row.names = FALSE,
    na = "",
    fileEncoding = "UTF-8"
  )
}


first_existing_field <- function(dat, candidates) {
  
  hits <- candidates[
    candidates %in% names(dat)
  ]
  
  if (length(hits) == 0L) {
    return(NA_character_)
  }
  
  hits[1L]
}


# ------------------------------------------------------------------------------
# 04. VERIFY REQUIRED INPUTS
# ------------------------------------------------------------------------------

input_status <- data.frame(
  input = c(
    "historical_03b_script",
    "historical_taxon_distribution_summary"
  ),
  full_path = c(
    HISTORICAL_03B_SCRIPT,
    SUMMARY_FILE
  ),
  exists = c(
    file.exists(HISTORICAL_03B_SCRIPT),
    file.exists(SUMMARY_FILE)
  ),
  stringsAsFactors = FALSE
)

write_audit(
  input_status,
  "geography_05a6_input_status.csv"
)

cat("Input status:\n\n")

print(
  input_status,
  row.names = FALSE
)


if (!file.exists(SUMMARY_FILE)) {
  
  stop(
    paste0(
      "Required historical summary not found:\n",
      SUMMARY_FILE
    )
  )
}


if (!file.exists(HISTORICAL_03B_SCRIPT)) {
  
  stop(
    paste0(
      "Historical 03b script not found:\n",
      HISTORICAL_03B_SCRIPT
    )
  )
}


# ------------------------------------------------------------------------------
# 05. READ HISTORICAL SUMMARY
# ------------------------------------------------------------------------------

taxon_summary <- safe_read_csv(
  SUMMARY_FILE
)


if (is.null(taxon_summary)) {
  
  stop(
    paste0(
      "Unable to read historical summary:\n",
      SUMMARY_FILE
    )
  )
}


names(taxon_summary) <- normalise_name(
  names(taxon_summary)
)


cat(
  "\nHistorical summary rows:",
  nrow(taxon_summary),
  "\n"
)

cat(
  "Historical summary columns:",
  ncol(taxon_summary),
  "\n\n"
)


# ------------------------------------------------------------------------------
# 06. IDENTIFY FIELDS
# ------------------------------------------------------------------------------

TAXON_ID_FIELD <- first_existing_field(
  taxon_summary,
  c(
    "final_wcvp_id",
    "wcvp_plant_name_id",
    "final_wcvp_accepted_plant_name_id",
    "accepted_plant_name_id",
    "plant_name_id",
    "wcvp_id",
    "taxon_id"
  )
)


LOCATION_FIELD <- first_existing_field(
  taxon_summary,
  c(
    "wcvp_location_ids",
    "location_ids",
    "wcvp_locations",
    "distribution_location_ids"
  )
)


DISTRIBUTION_ROWS_FIELD <- first_existing_field(
  taxon_summary,
  c(
    "wcvp_distribution_rows",
    "distribution_rows",
    "n_distribution_rows"
  )
)


if (is.na(TAXON_ID_FIELD)) {
  stop("No taxon identifier field detected.")
}

if (is.na(LOCATION_FIELD)) {
  stop("No wcvp_location_ids field detected.")
}

if (is.na(DISTRIBUTION_ROWS_FIELD)) {
  stop("No wcvp_distribution_rows field detected.")
}


detected_fields <- data.frame(
  role = c(
    "taxon_identifier",
    "wcvp_location_field",
    "wcvp_distribution_row_field"
  ),
  field = c(
    TAXON_ID_FIELD,
    LOCATION_FIELD,
    DISTRIBUTION_ROWS_FIELD
  ),
  stringsAsFactors = FALSE
)


write_audit(
  detected_fields,
  "geography_05a6_detected_fields.csv"
)


cat("Detected fields:\n\n")

print(
  detected_fields,
  row.names = FALSE
)


# ------------------------------------------------------------------------------
# 07. PROFILE RAW LOCATION FIELD
# ------------------------------------------------------------------------------

raw_location <- clean_character(
  taxon_summary[[LOCATION_FIELD]]
)

location_numeric <- safe_numeric(
  taxon_summary[[LOCATION_FIELD]]
)

distribution_rows <- safe_numeric(
  taxon_summary[[DISTRIBUTION_ROWS_FIELD]]
)

taxon_ids <- clean_character(
  taxon_summary[[TAXON_ID_FIELD]]
)


non_missing_location_numeric <- location_numeric[
  !is.na(location_numeric)
]


raw_location_profile <- data.frame(
  metric = c(
    "n_rows",
    "n_non_missing",
    "n_missing",
    "n_distinct_values",
    "n_numeric_values",
    "n_non_numeric_values",
    "minimum_numeric_value",
    "maximum_numeric_value",
    "median_numeric_value"
  ),
  value = c(
    length(raw_location),
    sum(!is.na(raw_location)),
    sum(is.na(raw_location)),
    length(
      unique(
        raw_location[
          !is.na(raw_location)
        ]
      )
    ),
    sum(!is.na(location_numeric)),
    sum(
      !is.na(raw_location) &
        is.na(location_numeric)
    ),
    if (length(non_missing_location_numeric) > 0L) {
      min(non_missing_location_numeric)
    } else {
      NA_real_
    },
    if (length(non_missing_location_numeric) > 0L) {
      max(non_missing_location_numeric)
    } else {
      NA_real_
    },
    if (length(non_missing_location_numeric) > 0L) {
      median(non_missing_location_numeric)
    } else {
      NA_real_
    }
  ),
  stringsAsFactors = FALSE
)


write_audit(
  raw_location_profile,
  "geography_05a6_raw_location_profile.csv"
)


# ------------------------------------------------------------------------------
# 08. CHARACTER-STRUCTURE TEST
# ------------------------------------------------------------------------------

character_structure <- data.frame(
  metric = c(
    "contains_pipe",
    "contains_semicolon",
    "contains_comma",
    "contains_space",
    "contains_letters",
    "contains_hyphen",
    "contains_square_brackets",
    "contains_parentheses"
  ),
  n_values = c(
    sum(
      grepl("\\|", raw_location),
      na.rm = TRUE
    ),
    sum(
      grepl(";", raw_location),
      na.rm = TRUE
    ),
    sum(
      grepl(",", raw_location),
      na.rm = TRUE
    ),
    sum(
      grepl("\\s", raw_location),
      na.rm = TRUE
    ),
    sum(
      grepl("[A-Za-z]", raw_location),
      na.rm = TRUE
    ),
    sum(
      grepl("-", raw_location),
      na.rm = TRUE
    ),
    sum(
      grepl("\\[|\\]", raw_location),
      na.rm = TRUE
    ),
    sum(
      grepl("\\(|\\)", raw_location),
      na.rm = TRUE
    )
  ),
  stringsAsFactors = FALSE
)


write_audit(
  character_structure,
  "geography_05a6_location_character_structure.csv"
)


# ------------------------------------------------------------------------------
# 09. LOCATION VALUE FREQUENCY
# ------------------------------------------------------------------------------

location_frequency <- as.data.frame(
  table(
    raw_location,
    useNA = "ifany"
  ),
  stringsAsFactors = FALSE
)


names(location_frequency) <- c(
  "wcvp_location_ids_value",
  "n_taxa"
)


location_frequency <- location_frequency[
  order(
    -location_frequency$n_taxa
  ),
  ,
  drop = FALSE
]


write_audit(
  location_frequency,
  "geography_05a6_location_value_frequency.csv"
)


# ------------------------------------------------------------------------------
# 10. DIRECT RELATIONSHIP WITH DISTRIBUTION ROW COUNT
# ------------------------------------------------------------------------------

comparison <- data.frame(
  final_wcvp_id = taxon_ids,
  wcvp_distribution_rows = distribution_rows,
  wcvp_location_ids_raw = raw_location,
  wcvp_location_ids_numeric = location_numeric,
  stringsAsFactors = FALSE
)


comparison$location_value_is_numeric <-
  !is.na(
    comparison$wcvp_location_ids_numeric
  )


comparison$location_value_le_distribution_rows <-
  ifelse(
    !is.na(comparison$wcvp_location_ids_numeric) &
      !is.na(comparison$wcvp_distribution_rows),
    comparison$wcvp_location_ids_numeric <=
      comparison$wcvp_distribution_rows,
    NA
  )


comparison$location_value_eq_distribution_rows <-
  ifelse(
    !is.na(comparison$wcvp_location_ids_numeric) &
      !is.na(comparison$wcvp_distribution_rows),
    comparison$wcvp_location_ids_numeric ==
      comparison$wcvp_distribution_rows,
    NA
  )


comparison$distribution_rows_minus_location_value <-
  comparison$wcvp_distribution_rows -
  comparison$wcvp_location_ids_numeric


comparison$location_to_distribution_ratio <-
  ifelse(
    !is.na(comparison$wcvp_distribution_rows) &
      comparison$wcvp_distribution_rows > 0 &
      !is.na(comparison$wcvp_location_ids_numeric),
    comparison$wcvp_location_ids_numeric /
      comparison$wcvp_distribution_rows,
    NA_real_
  )


write_audit(
  comparison,
  "geography_05a6_location_vs_distribution_rows.csv"
)


# ------------------------------------------------------------------------------
# 11. TEST COUNT HYPOTHESIS
# ------------------------------------------------------------------------------

valid_comparison <- comparison[
  !is.na(comparison$wcvp_location_ids_numeric) &
    !is.na(comparison$wcvp_distribution_rows),
  ,
  drop = FALSE
]


count_hypothesis <- data.frame(
  metric = c(
    "taxa_testable",
    "location_values_numeric",
    "location_value_le_distribution_rows",
    "location_value_eq_distribution_rows",
    "location_value_lt_distribution_rows",
    "location_value_gt_distribution_rows",
    "pct_location_value_le_distribution_rows",
    "pct_location_value_eq_distribution_rows"
  ),
  value = c(
    nrow(valid_comparison),
    sum(valid_comparison$location_value_is_numeric),
    sum(
      valid_comparison$wcvp_location_ids_numeric <=
        valid_comparison$wcvp_distribution_rows
    ),
    sum(
      valid_comparison$wcvp_location_ids_numeric ==
        valid_comparison$wcvp_distribution_rows
    ),
    sum(
      valid_comparison$wcvp_location_ids_numeric <
        valid_comparison$wcvp_distribution_rows
    ),
    sum(
      valid_comparison$wcvp_location_ids_numeric >
        valid_comparison$wcvp_distribution_rows
    ),
    if (nrow(valid_comparison) > 0L) {
      round(
        100 *
          sum(
            valid_comparison$wcvp_location_ids_numeric <=
              valid_comparison$wcvp_distribution_rows
          ) /
          nrow(valid_comparison),
        4
      )
    } else {
      NA_real_
    },
    if (nrow(valid_comparison) > 0L) {
      round(
        100 *
          sum(
            valid_comparison$wcvp_location_ids_numeric ==
              valid_comparison$wcvp_distribution_rows
          ) /
          nrow(valid_comparison),
        4
      )
    } else {
      NA_real_
    }
  ),
  stringsAsFactors = FALSE
)


write_audit(
  count_hypothesis,
  "geography_05a6_count_hypothesis.csv"
)


# ------------------------------------------------------------------------------
# 12. PROFILE REPRESENTATIVE TAXA
# ------------------------------------------------------------------------------

target_distribution_counts <- c(
  1,
  2,
  3,
  5,
  10,
  20,
  50,
  100,
  200
)


representative_list <- list()


for (target in target_distribution_counts) {
  
  candidates <- comparison[
    !is.na(comparison$wcvp_distribution_rows) &
      comparison$wcvp_distribution_rows == target,
    ,
    drop = FALSE
  ]
  
  
  if (nrow(candidates) == 0L) {
    next
  }
  
  
  candidates <- head(
    candidates,
    10L
  )
  
  
  candidates$target_distribution_rows <- target
  
  
  list_index <- length(representative_list) + 1L
  
  representative_list[[list_index]] <- candidates
}


upper_examples <- comparison[
  !is.na(comparison$wcvp_distribution_rows),
  ,
  drop = FALSE
]


upper_examples <- upper_examples[
  order(
    -upper_examples$wcvp_distribution_rows
  ),
  ,
  drop = FALSE
]


upper_examples <- head(
  upper_examples,
  30L
)


if (nrow(upper_examples) > 0L) {
  
  upper_examples$target_distribution_rows <- NA_real_
  
  list_index <- length(representative_list) + 1L
  
  representative_list[[list_index]] <- upper_examples
}


if (length(representative_list) > 0L) {
  
  representative_taxa <- do.call(
    rbind,
    representative_list
  )
  
} else {
  
  representative_taxa <- comparison[
    0,
    ,
    drop = FALSE
  ]
  
  representative_taxa$target_distribution_rows <- numeric(0)
}


write_audit(
  representative_taxa,
  "geography_05a6_representative_taxa.csv"
)


# ------------------------------------------------------------------------------
# 13. READ HISTORICAL 03b SCRIPT
# ------------------------------------------------------------------------------

lines_03b <- safe_read_lines(
  HISTORICAL_03B_SCRIPT
)


if (length(lines_03b) == 0L) {
  
  stop(
    "Historical 03b script exists but could not be read."
  )
}


script_table <- data.frame(
  line_number = seq_along(lines_03b),
  line_text = lines_03b,
  stringsAsFactors = FALSE
)


# ------------------------------------------------------------------------------
# 14. FIND EXACT wcvp_location_ids REFERENCES
# ------------------------------------------------------------------------------

location_hits <- grep(
  "wcvp_location_ids",
  lines_03b,
  ignore.case = TRUE
)


exact_location_trace_list <- list()


if (length(location_hits) > 0L) {
  
  for (hit in location_hits) {
    
    window_start <- max(
      1L,
      hit - 15L
    )
    
    window_end <- min(
      length(lines_03b),
      hit + 15L
    )
    
    window_lines <- seq.int(
      window_start,
      window_end
    )
    
    
    for (line_number in window_lines) {
      
      list_index <- length(exact_location_trace_list) + 1L
      
      exact_location_trace_list[[list_index]] <- data.frame(
        target_line = hit,
        line_number = line_number,
        offset_from_target = line_number - hit,
        line_text = lines_03b[line_number],
        stringsAsFactors = FALSE
      )
    }
  }
}


if (length(exact_location_trace_list) > 0L) {
  
  exact_location_trace <- do.call(
    rbind,
    exact_location_trace_list
  )
  
  exact_location_trace <- unique(
    exact_location_trace
  )
  
  exact_location_trace <- exact_location_trace[
    order(
      exact_location_trace$target_line,
      exact_location_trace$line_number
    ),
    ,
    drop = FALSE
  ]
  
} else {
  
  exact_location_trace <- data.frame(
    target_line = integer(0),
    line_number = integer(0),
    offset_from_target = integer(0),
    line_text = character(0),
    stringsAsFactors = FALSE
  )
}


write_audit(
  exact_location_trace,
  "geography_05a6_exact_wcvp_location_ids_code_trace.csv"
)


# ------------------------------------------------------------------------------
# 15. FIND DISTRIBUTION OBJECTS AND INPUT EXPRESSIONS
# ------------------------------------------------------------------------------

distribution_patterns <- c(
  "wcvp_distribution",
  "wcvp_distributions",
  "distribution_data",
  "distribution_df",
  "distribution_tbl",
  "location_id",
  "read.csv",
  "read_csv",
  "readRDS",
  "read_rds",
  "fread",
  "dbReadTable",
  "wcvp_distribution_rows",
  "n_distinct",
  "n_distinct\\(",
  "length\\(unique",
  "unique\\(",
  "summarise",
  "summarize",
  "group_by"
)


distribution_trace_list <- list()


for (pattern in distribution_patterns) {
  
  hits <- grep(
    pattern,
    lines_03b,
    ignore.case = TRUE
  )
  
  
  if (length(hits) == 0L) {
    next
  }
  
  
  for (hit in hits) {
    
    list_index <- length(distribution_trace_list) + 1L
    
    distribution_trace_list[[list_index]] <- data.frame(
      search_pattern = pattern,
      line_number = hit,
      line_text = lines_03b[hit],
      stringsAsFactors = FALSE
    )
  }
}


if (length(distribution_trace_list) > 0L) {
  
  distribution_trace <- do.call(
    rbind,
    distribution_trace_list
  )
  
  distribution_trace <- unique(
    distribution_trace
  )
  
  distribution_trace <- distribution_trace[
    order(
      distribution_trace$line_number
    ),
    ,
    drop = FALSE
  ]
  
} else {
  
  distribution_trace <- data.frame(
    search_pattern = character(0),
    line_number = integer(0),
    line_text = character(0),
    stringsAsFactors = FALSE
  )
}


write_audit(
  distribution_trace,
  "geography_05a6_distribution_object_trace.csv"
)


# ------------------------------------------------------------------------------
# 16. FIND RELEVANT ASSIGNMENTS
# ------------------------------------------------------------------------------

assignment_mask <- grepl(
  "<-|=",
  lines_03b
)


relevance_mask <- grepl(
  "distribution|location|wcvp|taxon",
  lines_03b,
  ignore.case = TRUE
)


relevant_assignments <- script_table[
  assignment_mask &
    relevance_mask,
  ,
  drop = FALSE
]


write_audit(
  relevant_assignments,
  "geography_05a6_relevant_historical_assignments.csv"
)


# ------------------------------------------------------------------------------
# 17. EXTRACT FILE / PATH REFERENCES FROM 03b
# ------------------------------------------------------------------------------

quoted_reference_list <- list()


for (i in seq_along(lines_03b)) {
  
  line <- lines_03b[i]
  
  
  matches <- gregexpr(
    "\"[^\"]+\"|'[^']+'",
    line,
    perl = TRUE
  )
  
  
  values <- regmatches(
    line,
    matches
  )[[1]]
  
  
  if (
    length(values) == 0L ||
    identical(
      values,
      character(0)
    )
  ) {
    next
  }
  
  
  for (value in values) {
    
    value_clean <- gsub(
      "^['\"]|['\"]$",
      "",
      value
    )
    
    
    if (
      !grepl(
        "wcvp|distribution|location|\\.csv|\\.rds|\\.rda|\\.rdata|data",
        value_clean,
        ignore.case = TRUE
      )
    ) {
      next
    }
    
    
    list_index <- length(quoted_reference_list) + 1L
    
    quoted_reference_list[[list_index]] <- data.frame(
      line_number = i,
      quoted_reference = value_clean,
      source_line = line,
      stringsAsFactors = FALSE
    )
  }
}


if (length(quoted_reference_list) > 0L) {
  
  quoted_references <- do.call(
    rbind,
    quoted_reference_list
  )
  
  quoted_references <- unique(
    quoted_references
  )
  
} else {
  
  quoted_references <- data.frame(
    line_number = integer(0),
    quoted_reference = character(0),
    source_line = character(0),
    stringsAsFactors = FALSE
  )
}


write_audit(
  quoted_references,
  "geography_05a6_historical_source_references.csv"
)


# ------------------------------------------------------------------------------
# 18. SEARCH PROJECT FOR LIKELY UPSTREAM DISTRIBUTION SOURCES
# ------------------------------------------------------------------------------

cat(
  "\nSearching project for likely upstream WCVP distribution sources...\n"
)


all_project_files <- list.files(
  PROJECT_ROOT,
  recursive = TRUE,
  full.names = TRUE,
  include.dirs = FALSE
)


upstream_mask <- grepl(
  "wcvp.*distribution|distribution.*wcvp",
  basename(all_project_files),
  ignore.case = TRUE
)


upstream_files <- all_project_files[
  upstream_mask
]


if (length(upstream_files) > 0L) {
  
  upstream_info <- file.info(
    upstream_files
  )
  
  
  upstream_inventory <- data.frame(
    file_name = basename(upstream_files),
    extension = tolower(
      tools::file_ext(
        upstream_files
      )
    ),
    size_bytes = upstream_info$size,
    size_mb = round(
      upstream_info$size / 1024^2,
      3
    ),
    full_path = upstream_files,
    stringsAsFactors = FALSE
  )
  
  
  upstream_inventory <- upstream_inventory[
    order(
      -upstream_inventory$size_bytes,
      upstream_inventory$file_name
    ),
    ,
    drop = FALSE
  ]
  
} else {
  
  upstream_inventory <- data.frame(
    file_name = character(0),
    extension = character(0),
    size_bytes = numeric(0),
    size_mb = numeric(0),
    full_path = character(0),
    stringsAsFactors = FALSE
  )
}


write_audit(
  upstream_inventory,
  "geography_05a6_upstream_distribution_file_inventory.csv"
)


# ------------------------------------------------------------------------------
# 19. PROFILE CANDIDATE UPSTREAM CSV SCHEMAS
# ------------------------------------------------------------------------------

upstream_csv <- upstream_inventory[
  upstream_inventory$extension == "csv",
  ,
  drop = FALSE
]


upstream_schema_list <- list()


if (nrow(upstream_csv) > 0L) {
  
  for (i in seq_len(nrow(upstream_csv))) {
    
    dat <- tryCatch(
      read.csv(
        upstream_csv$full_path[i],
        stringsAsFactors = FALSE,
        check.names = FALSE,
        nrows = 100L
      ),
      error = function(e) {
        NULL
      }
    )
    
    
    if (is.null(dat)) {
      next
    }
    
    
    original_names <- names(dat)
    
    normalised_names <- normalise_name(
      original_names
    )
    
    
    for (j in seq_along(original_names)) {
      
      field <- normalised_names[j]
      
      list_index <- length(upstream_schema_list) + 1L
      
      upstream_schema_list[[list_index]] <- data.frame(
        file_name = upstream_csv$file_name[i],
        size_mb = upstream_csv$size_mb[i],
        column_number = j,
        original_column_name = original_names[j],
        normalised_column_name = field,
        
        is_taxon_id_candidate = grepl(
          paste0(
            "plant_name_id|",
            "accepted_plant_name_id|",
            "final_wcvp_id|",
            "taxon_id|",
            "wcvp_id"
          ),
          field
        ),
        
        is_location_id_candidate = grepl(
          paste0(
            "^location_id$|",
            "wcvp_location_id|",
            "tdwg.*code|",
            "geographic.*unit|",
            "area_code"
          ),
          field
        ),
        
        full_path = upstream_csv$full_path[i],
        stringsAsFactors = FALSE
      )
    }
  }
}


if (length(upstream_schema_list) > 0L) {
  
  upstream_schema <- do.call(
    rbind,
    upstream_schema_list
  )
  
} else {
  
  upstream_schema <- data.frame(
    file_name = character(0),
    size_mb = numeric(0),
    column_number = integer(0),
    original_column_name = character(0),
    normalised_column_name = character(0),
    is_taxon_id_candidate = logical(0),
    is_location_id_candidate = logical(0),
    full_path = character(0),
    stringsAsFactors = FALSE
  )
}


write_audit(
  upstream_schema,
  "geography_05a6_upstream_distribution_schema.csv"
)


# ------------------------------------------------------------------------------
# 20. IDENTIFY FILES WITH BOTH TAXON AND LOCATION CANDIDATES
# ------------------------------------------------------------------------------

candidate_source_list <- list()


if (nrow(upstream_schema) > 0L) {
  
  upstream_file_names <- unique(
    upstream_schema$file_name
  )
  
  
  for (file_name in upstream_file_names) {
    
    file_schema <- upstream_schema[
      upstream_schema$file_name == file_name,
      ,
      drop = FALSE
    ]
    
    
    has_taxon_id <- any(
      file_schema$is_taxon_id_candidate
    )
    
    
    has_location_id <- any(
      file_schema$is_location_id_candidate
    )
    
    
    taxon_fields <- file_schema$normalised_column_name[
      file_schema$is_taxon_id_candidate
    ]
    
    
    location_fields <- file_schema$normalised_column_name[
      file_schema$is_location_id_candidate
    ]
    
    
    list_index <- length(candidate_source_list) + 1L
    
    candidate_source_list[[list_index]] <- data.frame(
      file_name = file_name,
      has_taxon_id = has_taxon_id,
      has_location_id = has_location_id,
      has_both = has_taxon_id && has_location_id,
      
      taxon_fields = if (length(taxon_fields) > 0L) {
        paste(
          taxon_fields,
          collapse = " | "
        )
      } else {
        NA_character_
      },
      
      location_fields = if (length(location_fields) > 0L) {
        paste(
          location_fields,
          collapse = " | "
        )
      } else {
        NA_character_
      },
      
      size_mb = file_schema$size_mb[1L],
      full_path = file_schema$full_path[1L],
      stringsAsFactors = FALSE
    )
  }
}


if (length(candidate_source_list) > 0L) {
  
  candidate_sources <- do.call(
    rbind,
    candidate_source_list
  )
  
  
  candidate_sources <- candidate_sources[
    order(
      -as.integer(candidate_sources$has_both),
      -as.integer(candidate_sources$has_location_id),
      -as.integer(candidate_sources$has_taxon_id),
      -candidate_sources$size_mb
    ),
    ,
    drop = FALSE
  ]
  
} else {
  
  candidate_sources <- data.frame(
    file_name = character(0),
    has_taxon_id = logical(0),
    has_location_id = logical(0),
    has_both = logical(0),
    taxon_fields = character(0),
    location_fields = character(0),
    size_mb = numeric(0),
    full_path = character(0),
    stringsAsFactors = FALSE
  )
}


write_audit(
  candidate_sources,
  "geography_05a6_candidate_upstream_sources.csv"
)


# ------------------------------------------------------------------------------
# 21. TEST WHETHER LOCATION FIELD BEHAVES LIKE A COUNT
# ------------------------------------------------------------------------------

n_non_missing_raw <- sum(
  !is.na(raw_location)
)


n_numeric_raw <- sum(
  !is.na(location_numeric)
)


all_numeric <-
  n_non_missing_raw > 0L &&
  n_non_missing_raw == n_numeric_raw


all_non_negative_integer <- FALSE


if (all_numeric) {
  
  non_missing_numeric <- location_numeric[
    !is.na(location_numeric)
  ]
  
  
  all_non_negative_integer <- all(
    non_missing_numeric >= 0 &
      non_missing_numeric ==
      floor(
        non_missing_numeric
      )
  )
}


all_le_distribution_rows <- FALSE


if (nrow(valid_comparison) > 0L) {
  
  all_le_distribution_rows <- all(
    valid_comparison$wcvp_location_ids_numeric <=
      valid_comparison$wcvp_distribution_rows
  )
}


count_like_field <-
  all_numeric &&
  all_non_negative_integer &&
  all_le_distribution_rows


field_interpretation <- data.frame(
  test = c(
    "all_non_missing_values_numeric",
    "all_numeric_values_non_negative_integers",
    "all_values_le_distribution_row_count",
    "field_behaves_like_unique_location_count"
  ),
  passed = c(
    all_numeric,
    all_non_negative_integer,
    all_le_distribution_rows,
    count_like_field
  ),
  stringsAsFactors = FALSE
)


write_audit(
  field_interpretation,
  "geography_05a6_field_interpretation.csv"
)


# ------------------------------------------------------------------------------
# 22. DIAGNOSTIC CONCLUSION
# ------------------------------------------------------------------------------

exact_code_found <-
  nrow(exact_location_trace) > 0L


candidate_upstream_source_found <-
  nrow(candidate_sources) > 0L &&
  any(
    candidate_sources$has_both,
    na.rm = TRUE
  )


diagnostic_summary <- data.frame(
  criterion = c(
    "wcvp_location_ids_present",
    "all_location_values_numeric",
    "all_location_values_integer",
    "all_location_values_le_distribution_rows",
    "field_behaves_like_count",
    "historical_creation_code_found",
    "candidate_upstream_taxon_location_source_found"
  ),
  result = c(
    TRUE,
    all_numeric,
    all_non_negative_integer,
    all_le_distribution_rows,
    count_like_field,
    exact_code_found,
    candidate_upstream_source_found
  ),
  stringsAsFactors = FALSE
)


write_audit(
  diagnostic_summary,
  "geography_05a6_diagnostic_summary.csv"
)


# ------------------------------------------------------------------------------
# 23. RUN METADATA
# ------------------------------------------------------------------------------

run_metadata <- data.frame(
  item = c(
    "script",
    "run_time",
    "historical_summary",
    "historical_script",
    "taxon_id_field",
    "location_field",
    "distribution_rows_field",
    "historical_outputs_modified",
    "published_vpjd_modified",
    "occurrence_data_modified",
    "range_matrix_modified",
    "star_categories_assigned"
  ),
  value = c(
    "geography_05a6_decode_historical_wcvp_location_ids.R",
    format(
      Sys.time(),
      "%Y-%m-%d %H:%M:%S %Z"
    ),
    SUMMARY_FILE,
    HISTORICAL_03B_SCRIPT,
    TAXON_ID_FIELD,
    LOCATION_FIELD,
    DISTRIBUTION_ROWS_FIELD,
    "FALSE",
    "FALSE",
    "FALSE",
    "FALSE",
    "FALSE"
  ),
  stringsAsFactors = FALSE
)


write_audit(
  run_metadata,
  "geography_05a6_run_metadata.csv"
)


capture.output(
  sessionInfo(),
  file = file.path(
    AUDIT_ROOT,
    "geography_05a6_sessionInfo.txt"
  )
)


# ------------------------------------------------------------------------------
# 24. CONSOLE SUMMARY
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" VPJD GEOGRAPHY 05a6 - LOCATION FIELD DECODE COMPLETE\n")
cat("============================================================\n\n")


cat("Raw wcvp_location_ids profile:\n\n")

print(
  raw_location_profile,
  row.names = FALSE
)


cat("\nCharacter structure:\n\n")

print(
  character_structure,
  row.names = FALSE
)


cat("\nCount hypothesis:\n\n")

print(
  count_hypothesis,
  row.names = FALSE
)


cat("\nField interpretation:\n\n")

print(
  field_interpretation,
  row.names = FALSE
)


cat("\nDiagnostic summary:\n\n")

print(
  diagnostic_summary,
  row.names = FALSE
)


cat(
  "\nMost common raw wcvp_location_ids values:\n\n"
)

print(
  head(
    location_frequency,
    30L
  ),
  row.names = FALSE
)


cat(
  "\nRepresentative taxon comparisons:\n\n"
)


representative_print_fields <- c(
  "final_wcvp_id",
  "wcvp_distribution_rows",
  "wcvp_location_ids_raw",
  "wcvp_location_ids_numeric",
  "distribution_rows_minus_location_value",
  "location_to_distribution_ratio"
)


representative_print_fields <-
  representative_print_fields[
    representative_print_fields %in%
      names(representative_taxa)
  ]


print(
  head(
    representative_taxa[
      ,
      representative_print_fields,
      drop = FALSE
    ],
    50L
  ),
  row.names = FALSE
)


cat(
  "\nExact historical code around wcvp_location_ids:\n\n"
)


if (nrow(exact_location_trace) > 0L) {
  
  print(
    exact_location_trace,
    row.names = FALSE
  )
  
} else {
  
  cat(
    "No explicit wcvp_location_ids reference found in 03b.\n"
  )
}


cat(
  "\nCandidate upstream WCVP distribution sources:\n\n"
)


if (nrow(candidate_sources) > 0L) {
  
  print_fields <- c(
    "file_name",
    "has_taxon_id",
    "has_location_id",
    "has_both",
    "taxon_fields",
    "location_fields",
    "size_mb",
    "full_path"
  )
  
  
  print_fields <- print_fields[
    print_fields %in%
      names(candidate_sources)
  ]
  
  
  print(
    head(
      candidate_sources[
        ,
        print_fields,
        drop = FALSE
      ],
      30L
    ),
    row.names = FALSE
  )
  
} else {
  
  cat(
    "No candidate upstream files identified by filename search.\n"
  )
}


cat("\nAudit outputs written to:\n")

cat(
  AUDIT_ROOT,
  "\n\n"
)


cat("Historical outputs were NOT modified.\n")
cat("Published VPJD v1.0.0 was NOT modified.\n")
cat("Occurrence data were NOT modified.\n")
cat("Current taxon range matrix was NOT modified.\n")
cat("NO Star categories were assigned.\n")


cat("\nINTERPRETATION:\n")


if (count_like_field) {
  
  cat(
    paste0(
      "The historical wcvp_location_ids field behaves numerically like ",
      "a count rather than a list of geographic identifiers: all ",
      "non-missing values are non-negative integers and do not exceed ",
      "the corresponding number of WCVP distribution rows. The exact ",
      "03b code trace must now be used to determine whether this field ",
      "was explicitly calculated as the number of distinct location_id ",
      "values.\n"
    )
  )
  
} else {
  
  cat(
    paste0(
      "The historical wcvp_location_ids field does not satisfy all ",
      "tests expected of a simple unique-location count. The exact 03b ",
      "code trace should therefore be treated as authoritative when ",
      "determining the field's meaning.\n"
    )
  )
}


cat("\nNEXT STEP:\n")


if (
  count_like_field &&
  candidate_upstream_source_found
) {
  
  cat(
    paste0(
      "The field behaves like a location count and at least one ",
      "candidate upstream source contains both a taxon identifier and ",
      "a location identifier. Review the candidate source and historical ",
      "03b code. If they agree, the next stage can extract the original ",
      "taxon x WCVP-location evidence directly rather than reconstructing ",
      "it from the summary statistic.\n"
    )
  )
  
} else if (count_like_field) {
  
  cat(
    paste0(
      "The field behaves like a location count, but the underlying ",
      "taxon x location source has not yet been identified automatically. ",
      "Use geography_05a6_exact_wcvp_location_ids_code_trace.csv and ",
      "geography_05a6_distribution_object_trace.csv to identify the ",
      "upstream object and source before proceeding to 05b.\n"
    )
  )
  
} else {
  
  cat(
    paste0(
      "Do NOT proceed to geography_05b. Review the exact historical ",
      "code trace and representative taxon comparisons to establish ",
      "the true semantics of wcvp_location_ids and identify its upstream ",
      "distribution source.\n"
    )
  )
}


cat("\n============================================================\n")