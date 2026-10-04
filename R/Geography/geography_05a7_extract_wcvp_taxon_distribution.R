# ==============================================================================
# VPJD GEOGRAPHY MODULE
# geography_05a7_extract_wcvp_taxon_distribution.R
#
# PURPOSE
#
# Recover authoritative positive WCVP global-distribution evidence directly
# from the raw WCVP distribution source:
#
#   data/raw/geography/wcvp_dwca/wcvp_distribution.csv
#
# Historical 03b code established that the previous summary fields were:
#
#   COUNT(d.taxon_id)              AS WCVP_DISTRIBUTION_ROWS
#   COUNT(DISTINCT d.location_id)  AS WCVP_LOCATION_IDS
#   COUNT(DISTINCT d.locality)     AS WCVP_LOCALITIES
#   COUNT(DISTINCT d.establishment_means)
#                                   AS ESTABLISHMENT_VALUES
#
# where:
#
#   FINAL_WCVP_ID = d.taxon_id
#
# Therefore `wcvp_location_ids` in the historical 03b summary is a COUNT,
# not a collapsed list of geographic identifiers.
#
# This script:
#
#   1. Reads the raw WCVP distribution CSV.
#   2. Profiles and validates its actual schema.
#   3. Identifies taxon_id, location_id, locality and establishment_means.
#   4. Reads the historical 03b taxon-distribution summary.
#   5. Restricts raw WCVP distribution evidence to VPJD-linked WCVP taxa.
#   6. Extracts actual distinct taxon_id x location_id pairs.
#   7. Recreates the historical 03b summary directly from the raw source.
#   8. Validates reconstructed counts against historical 03b.
#   9. Writes authoritative positive WCVP taxon x location evidence.
#
# IMPORTANT
#
# This script does NOT:
#
#   * assign Star categories;
#   * infer geographic absence from missing records;
#   * classify taxa as endemic/non-endemic;
#   * interpret WCVP location IDs geographically;
#   * modify published VPJD;
#   * modify the occurrence DuckDB;
#   * modify historical outputs;
#   * modify the current taxon range matrix.
#
# ==============================================================================


# ------------------------------------------------------------------------------
# 01. PATHS
# ------------------------------------------------------------------------------

PROJECT_ROOT <- "I:/R/OJPCP/VPJD-OJPCP"


RAW_WCVP_FILE <- file.path(
  PROJECT_ROOT,
  "data",
  "raw",
  "geography",
  "wcvp_dwca",
  "wcvp_distribution.csv"
)


HISTORICAL_SUMMARY_FILE <- file.path(
  PROJECT_ROOT,
  "outputs",
  "tables",
  "stars",
  "wcvp_distribution",
  "stars03b_taxon_distribution_summary.csv"
)


OUTPUT_ROOT <- file.path(
  PROJECT_ROOT,
  "data",
  "derived",
  "geography",
  "taxon_range",
  "wcvp"
)


AUDIT_ROOT <- file.path(
  PROJECT_ROOT,
  "audit",
  "geography",
  "taxon_range",
  "wcvp_taxon_distribution_extract"
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


OUTPUT_PAIR_FILE <- file.path(
  OUTPUT_ROOT,
  "vpjd_wcvp_taxon_location_pairs.csv"
)


OUTPUT_DETAIL_FILE <- file.path(
  OUTPUT_ROOT,
  "vpjd_wcvp_distribution_records.csv"
)


OUTPUT_SUMMARY_FILE <- file.path(
  OUTPUT_ROOT,
  "vpjd_wcvp_taxon_distribution_summary.csv"
)


# ------------------------------------------------------------------------------
# 02. START
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" VPJD GEOGRAPHY 05a7 - EXTRACT WCVP TAXON DISTRIBUTION\n")
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
          
          stop(
            paste0(
              "Unable to read CSV:\n",
              path,
              "\n\n",
              conditionMessage(e2)
            )
          )
        }
      )
    }
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


safe_distinct_count <- function(x) {
  
  x <- clean_character(x)
  
  length(
    unique(
      x[
        !is.na(x)
      ]
    )
  )
}


# ------------------------------------------------------------------------------
# 04. VERIFY INPUTS
# ------------------------------------------------------------------------------

input_status <- data.frame(
  
  input = c(
    "raw_wcvp_distribution",
    "historical_03b_summary"
  ),
  
  full_path = c(
    RAW_WCVP_FILE,
    HISTORICAL_SUMMARY_FILE
  ),
  
  exists = c(
    file.exists(RAW_WCVP_FILE),
    file.exists(HISTORICAL_SUMMARY_FILE)
  ),
  
  size_bytes = c(
    if (file.exists(RAW_WCVP_FILE)) {
      file.info(RAW_WCVP_FILE)$size
    } else {
      NA_real_
    },
    if (file.exists(HISTORICAL_SUMMARY_FILE)) {
      file.info(HISTORICAL_SUMMARY_FILE)$size
    } else {
      NA_real_
    }
  ),
  
  stringsAsFactors = FALSE
)


write_audit(
  input_status,
  "geography_05a7_input_status.csv"
)


cat("Input status:\n\n")

print(
  input_status,
  row.names = FALSE
)


if (!file.exists(RAW_WCVP_FILE)) {
  
  stop(
    paste0(
      "Raw WCVP distribution file not found:\n",
      RAW_WCVP_FILE
    )
  )
}


if (!file.exists(HISTORICAL_SUMMARY_FILE)) {
  
  stop(
    paste0(
      "Historical 03b summary not found:\n",
      HISTORICAL_SUMMARY_FILE
    )
  )
}


# ------------------------------------------------------------------------------
# 05. READ RAW WCVP DISTRIBUTION
# ------------------------------------------------------------------------------

cat(
  "\nReading raw WCVP distribution file...\n"
)

# IMPORTANT:
# The WCVP DwC-A distribution file uses pipe delimiters despite its .csv
# extension. Read explicitly with sep = "|".

wcvp_raw <- tryCatch(
  
  read.csv(
    RAW_WCVP_FILE,
    sep = "|",
    quote = "\"",
    stringsAsFactors = FALSE,
    check.names = FALSE,
    encoding = "UTF-8"
  ),
  
  error = function(e1) {
    
    tryCatch(
      
      read.csv(
        RAW_WCVP_FILE,
        sep = "|",
        quote = "\"",
        stringsAsFactors = FALSE,
        check.names = FALSE
      ),
      
      error = function(e2) {
        
        stop(
          paste0(
            "Unable to read pipe-delimited WCVP distribution file:\n",
            RAW_WCVP_FILE,
            "\n\n",
            conditionMessage(e2)
          )
        )
      }
    )
  }
)


if (is.null(wcvp_raw)) {
  stop("Raw WCVP distribution file could not be read.")
}


original_wcvp_names <- names(
  wcvp_raw
)


normalised_wcvp_names <- normalise_name(
  original_wcvp_names
)


wcvp_schema <- data.frame(
  
  column_number = seq_along(
    original_wcvp_names
  ),
  
  original_column_name = original_wcvp_names,
  
  normalised_column_name = normalised_wcvp_names,
  
  stringsAsFactors = FALSE
)


write_audit(
  wcvp_schema,
  "geography_05a7_raw_wcvp_schema.csv"
)


names(wcvp_raw) <- normalised_wcvp_names


cat(
  "\nRaw WCVP rows:",
  nrow(wcvp_raw),
  "\n"
)

cat(
  "Raw WCVP columns:",
  ncol(wcvp_raw),
  "\n\n"
)


cat(
  "Raw WCVP schema:\n\n"
)

print(
  wcvp_schema,
  row.names = FALSE
)


if (ncol(wcvp_raw) == 1L) {
  
  stop(
    paste0(
      "WCVP distribution file still parsed as one column. ",
      "Expected a pipe-delimited six-column DwC-A distribution table."
    )
  )
}
# ------------------------------------------------------------------------------
# 06. IDENTIFY REQUIRED WCVP FIELDS
# ------------------------------------------------------------------------------

# Actual WCVP DwC-A distribution schema:
#
#   coreid
#   locality
#   establishmentmeans
#   locationid
#   occurrencestatus
#   threatstatus
#
# Historical VPJD 03b terminology maps these as:
#
#   coreid             -> taxon_id
#   locationid         -> location_id
#   locality           -> locality
#   establishmentmeans -> establishment_means


TAXON_ID_FIELD <- "coreid"

LOCATION_ID_FIELD <- "locationid"

LOCALITY_FIELD <- "locality"

ESTABLISHMENT_FIELD <- "establishmentmeans"

OCCURRENCE_STATUS_FIELD <- "occurrencestatus"

THREAT_STATUS_FIELD <- "threatstatus"


required_wcvp_fields <- c(
  TAXON_ID_FIELD,
  LOCATION_ID_FIELD,
  LOCALITY_FIELD,
  ESTABLISHMENT_FIELD,
  OCCURRENCE_STATUS_FIELD,
  THREAT_STATUS_FIELD
)


missing_wcvp_fields <- setdiff(
  required_wcvp_fields,
  names(wcvp_raw)
)


if (length(missing_wcvp_fields) > 0L) {
  
  stop(
    paste0(
      "Expected WCVP DwC-A distribution fields are missing: ",
      paste(
        missing_wcvp_fields,
        collapse = ", "
      )
    )
  )
}


detected_wcvp_fields <- data.frame(
  
  historical_role = c(
    "taxon_id",
    "location_id",
    "locality",
    "establishment_means",
    "occurrence_status",
    "threat_status"
  ),
  
  raw_wcvp_field = c(
    TAXON_ID_FIELD,
    LOCATION_ID_FIELD,
    LOCALITY_FIELD,
    ESTABLISHMENT_FIELD,
    OCCURRENCE_STATUS_FIELD,
    THREAT_STATUS_FIELD
  ),
  
  detected = required_wcvp_fields %in%
    names(wcvp_raw),
  
  stringsAsFactors = FALSE
)


write_audit(
  detected_wcvp_fields,
  "geography_05a7_detected_wcvp_fields.csv"
)


cat(
  "Detected WCVP fields:\n\n"
)


print(
  detected_wcvp_fields,
  row.names = FALSE
)
# ------------------------------------------------------------------------------
# 07. STANDARDISE RAW WCVP DISTRIBUTION
# ------------------------------------------------------------------------------

wcvp_distribution <- data.frame(
  
  taxon_id = clean_character(
    wcvp_raw[[TAXON_ID_FIELD]]
  ),
  
  location_id = clean_character(
    wcvp_raw[[LOCATION_ID_FIELD]]
  ),
  
  stringsAsFactors = FALSE
)


if (!is.na(LOCALITY_FIELD)) {
  
  wcvp_distribution$locality <- clean_character(
    wcvp_raw[[LOCALITY_FIELD]]
  )
  
} else {
  
  wcvp_distribution$locality <- NA_character_
}


if (!is.na(ESTABLISHMENT_FIELD)) {
  
  wcvp_distribution$establishment_means <- clean_character(
    wcvp_raw[[ESTABLISHMENT_FIELD]]
  )
  
} else {
  
  wcvp_distribution$establishment_means <- NA_character_
}

wcvp_distribution$occurrence_status <- clean_character(
  wcvp_raw[[OCCURRENCE_STATUS_FIELD]]
)

wcvp_distribution$threat_status <- clean_character(
  wcvp_raw[[THREAT_STATUS_FIELD]]
)


# ------------------------------------------------------------------------------
# 08. PROFILE RAW DISTRIBUTION
# ------------------------------------------------------------------------------

raw_profile <- data.frame(
  
  metric = c(
    "raw_distribution_rows",
    "rows_with_taxon_id",
    "rows_without_taxon_id",
    "rows_with_location_id",
    "rows_without_location_id",
    "distinct_taxon_ids",
    "distinct_location_ids",
    "distinct_localities",
    "distinct_establishment_values"
  ),
  
  value = c(
    nrow(wcvp_distribution),
    
    sum(
      !is.na(
        wcvp_distribution$taxon_id
      )
    ),
    
    sum(
      is.na(
        wcvp_distribution$taxon_id
      )
    ),
    
    sum(
      !is.na(
        wcvp_distribution$location_id
      )
    ),
    
    sum(
      is.na(
        wcvp_distribution$location_id
      )
    ),
    
    safe_distinct_count(
      wcvp_distribution$taxon_id
    ),
    
    safe_distinct_count(
      wcvp_distribution$location_id
    ),
    
    safe_distinct_count(
      wcvp_distribution$locality
    ),
    
    safe_distinct_count(
      wcvp_distribution$establishment_means
    )
  ),
  
  stringsAsFactors = FALSE
)


write_audit(
  raw_profile,
  "geography_05a7_raw_distribution_profile.csv"
)


# ------------------------------------------------------------------------------
# 09. READ HISTORICAL 03b SUMMARY
# ------------------------------------------------------------------------------

historical_summary <- safe_read_csv(
  HISTORICAL_SUMMARY_FILE
)


if (is.null(historical_summary)) {
  
  stop(
    "Historical 03b summary could not be read."
  )
}


names(historical_summary) <- normalise_name(
  names(historical_summary)
)


HISTORICAL_TAXON_FIELD <- first_existing_field(
  historical_summary,
  c(
    "final_wcvp_id",
    "wcvp_plant_name_id",
    "taxon_id"
  )
)


HISTORICAL_ROWS_FIELD <- first_existing_field(
  historical_summary,
  c(
    "wcvp_distribution_rows",
    "distribution_rows"
  )
)


HISTORICAL_LOCATION_COUNT_FIELD <- first_existing_field(
  historical_summary,
  c(
    "wcvp_location_ids",
    "location_ids"
  )
)


HISTORICAL_LOCALITY_COUNT_FIELD <- first_existing_field(
  historical_summary,
  c(
    "wcvp_localities",
    "localities"
  )
)


HISTORICAL_ESTABLISHMENT_COUNT_FIELD <- first_existing_field(
  historical_summary,
  c(
    "establishment_values",
    "wcvp_establishment_values"
  )
)


if (is.na(HISTORICAL_TAXON_FIELD)) {
  
  stop(
    "Historical taxon identifier field not detected."
  )
}


if (is.na(HISTORICAL_ROWS_FIELD)) {
  
  stop(
    "Historical WCVP distribution-row field not detected."
  )
}


if (is.na(HISTORICAL_LOCATION_COUNT_FIELD)) {
  
  stop(
    "Historical WCVP location-count field not detected."
  )
}


historical_taxon_ids <- unique(
  clean_character(
    historical_summary[[HISTORICAL_TAXON_FIELD]]
  )
)


historical_taxon_ids <- historical_taxon_ids[
  !is.na(historical_taxon_ids)
]


cat(
  "\nHistorical VPJD-linked WCVP taxa:",
  length(historical_taxon_ids),
  "\n"
)


# ------------------------------------------------------------------------------
# 10. RESTRICT RAW DISTRIBUTION TO HISTORICAL VPJD-LINKED TAXA
# ------------------------------------------------------------------------------

vpjd_wcvp_distribution <- wcvp_distribution[
  !is.na(wcvp_distribution$taxon_id) &
    wcvp_distribution$taxon_id %in%
    historical_taxon_ids,
  ,
  drop = FALSE
]


cat(
  "Raw WCVP rows linked to historical VPJD taxa:",
  nrow(vpjd_wcvp_distribution),
  "\n"
)


cat(
  "Distinct linked WCVP taxa:",
  safe_distinct_count(
    vpjd_wcvp_distribution$taxon_id
  ),
  "\n\n"
)


# ------------------------------------------------------------------------------
# 11. IDENTIFY HISTORICAL TAXA NOT RECOVERED FROM RAW SOURCE
# ------------------------------------------------------------------------------

recovered_taxon_ids <- unique(
  vpjd_wcvp_distribution$taxon_id[
    !is.na(
      vpjd_wcvp_distribution$taxon_id
    )
  ]
)


historical_taxa_not_recovered <- setdiff(
  historical_taxon_ids,
  recovered_taxon_ids
)


not_recovered_table <- data.frame(
  final_wcvp_id = historical_taxa_not_recovered,
  stringsAsFactors = FALSE
)


write_audit(
  not_recovered_table,
  "geography_05a7_historical_taxa_not_recovered.csv"
)


# ------------------------------------------------------------------------------
# 12. EXTRACT DISTINCT TAXON x LOCATION PAIRS
# ------------------------------------------------------------------------------

taxon_location_pairs <- vpjd_wcvp_distribution[
  !is.na(vpjd_wcvp_distribution$taxon_id) &
    !is.na(vpjd_wcvp_distribution$location_id),
  c(
    "taxon_id",
    "location_id"
  ),
  drop = FALSE
]


taxon_location_pairs <- unique(
  taxon_location_pairs
)


taxon_location_pairs <- taxon_location_pairs[
  order(
    taxon_location_pairs$taxon_id,
    taxon_location_pairs$location_id
  ),
  ,
  drop = FALSE
]


names(taxon_location_pairs) <- c(
  "final_wcvp_id",
  "wcvp_location_id"
)


write.csv(
  taxon_location_pairs,
  OUTPUT_PAIR_FILE,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)


# ------------------------------------------------------------------------------
# 13. WRITE POSITIVE DISTRIBUTION DETAIL
# ------------------------------------------------------------------------------

vpjd_distribution_detail <- vpjd_wcvp_distribution[
  !is.na(vpjd_wcvp_distribution$taxon_id) &
    !is.na(vpjd_wcvp_distribution$location_id),
  ,
  drop = FALSE
]


names(vpjd_distribution_detail)[
  names(vpjd_distribution_detail) == "taxon_id"
] <- "final_wcvp_id"


names(vpjd_distribution_detail)[
  names(vpjd_distribution_detail) == "location_id"
] <- "wcvp_location_id"


write.csv(
  vpjd_distribution_detail,
  OUTPUT_DETAIL_FILE,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)


# ------------------------------------------------------------------------------
# 14. RECREATE HISTORICAL 03b COUNTS FROM RAW DISTRIBUTION
# ------------------------------------------------------------------------------

historical_id_order <- historical_taxon_ids


recreated_list <- vector(
  "list",
  length(historical_id_order)
)


for (i in seq_along(historical_id_order)) {
  
  taxon_id_value <- historical_id_order[i]
  
  
  taxon_rows <- vpjd_wcvp_distribution[
    !is.na(vpjd_wcvp_distribution$taxon_id) &
      vpjd_wcvp_distribution$taxon_id == taxon_id_value,
    ,
    drop = FALSE
  ]
  
  
  recreated_list[[i]] <- data.frame(
    
    final_wcvp_id = taxon_id_value,
    
    recreated_distribution_rows = nrow(
      taxon_rows
    ),
    
    recreated_location_ids = safe_distinct_count(
      taxon_rows$location_id
    ),
    
    recreated_localities = safe_distinct_count(
      taxon_rows$locality
    ),
    
    recreated_establishment_values = safe_distinct_count(
      taxon_rows$establishment_means
    ),
    
    stringsAsFactors = FALSE
  )
}


recreated_summary <- do.call(
  rbind,
  recreated_list
)


write.csv(
  recreated_summary,
  OUTPUT_SUMMARY_FILE,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)


# ------------------------------------------------------------------------------
# 15. STANDARDISE HISTORICAL COUNTS
# ------------------------------------------------------------------------------

historical_validation <- data.frame(
  
  final_wcvp_id = clean_character(
    historical_summary[[HISTORICAL_TAXON_FIELD]]
  ),
  
  historical_distribution_rows = safe_numeric(
    historical_summary[[HISTORICAL_ROWS_FIELD]]
  ),
  
  historical_location_ids = safe_numeric(
    historical_summary[[HISTORICAL_LOCATION_COUNT_FIELD]]
  ),
  
  stringsAsFactors = FALSE
)


if (!is.na(HISTORICAL_LOCALITY_COUNT_FIELD)) {
  
  historical_validation$historical_localities <-
    safe_numeric(
      historical_summary[[HISTORICAL_LOCALITY_COUNT_FIELD]]
    )
  
} else {
  
  historical_validation$historical_localities <-
    NA_real_
}


if (!is.na(HISTORICAL_ESTABLISHMENT_COUNT_FIELD)) {
  
  historical_validation$historical_establishment_values <-
    safe_numeric(
      historical_summary[[HISTORICAL_ESTABLISHMENT_COUNT_FIELD]]
    )
  
} else {
  
  historical_validation$historical_establishment_values <-
    NA_real_
}


# ------------------------------------------------------------------------------
# 16. JOIN HISTORICAL AND RECREATED COUNTS
# ------------------------------------------------------------------------------

validation <- merge(
  historical_validation,
  recreated_summary,
  by = "final_wcvp_id",
  all.x = TRUE,
  sort = FALSE
)


validation$distribution_rows_match <-
  !is.na(validation$historical_distribution_rows) &
  !is.na(validation$recreated_distribution_rows) &
  validation$historical_distribution_rows ==
  validation$recreated_distribution_rows


validation$location_ids_match <-
  !is.na(validation$historical_location_ids) &
  !is.na(validation$recreated_location_ids) &
  validation$historical_location_ids ==
  validation$recreated_location_ids


validation$localities_match <-
  ifelse(
    is.na(validation$historical_localities),
    NA,
    validation$historical_localities ==
      validation$recreated_localities
  )


validation$establishment_values_match <-
  ifelse(
    is.na(validation$historical_establishment_values),
    NA,
    validation$historical_establishment_values ==
      validation$recreated_establishment_values
  )


write_audit(
  validation,
  "geography_05a7_historical_vs_recreated_counts.csv"
)


# ------------------------------------------------------------------------------
# 17. VALIDATION SUMMARY
# ------------------------------------------------------------------------------

validation_summary <- data.frame(
  
  metric = c(
    "historical_taxa",
    "historical_taxa_recovered_from_raw",
    "historical_taxa_not_recovered",
    "raw_wcvp_distribution_rows",
    "vpjd_linked_distribution_rows",
    "distinct_vpjd_linked_taxa",
    "distinct_taxon_location_pairs",
    "distinct_wcvp_location_ids",
    "distribution_row_counts_matching",
    "distribution_row_counts_not_matching",
    "location_counts_matching",
    "location_counts_not_matching",
    "pct_distribution_row_counts_matching",
    "pct_location_counts_matching"
  ),
  
  value = c(
    length(historical_taxon_ids),
    
    length(
      recovered_taxon_ids
    ),
    
    length(
      historical_taxa_not_recovered
    ),
    
    nrow(
      wcvp_distribution
    ),
    
    nrow(
      vpjd_wcvp_distribution
    ),
    
    safe_distinct_count(
      vpjd_wcvp_distribution$taxon_id
    ),
    
    nrow(
      taxon_location_pairs
    ),
    
    safe_distinct_count(
      taxon_location_pairs$wcvp_location_id
    ),
    
    sum(
      validation$distribution_rows_match,
      na.rm = TRUE
    ),
    
    sum(
      !validation$distribution_rows_match,
      na.rm = TRUE
    ),
    
    sum(
      validation$location_ids_match,
      na.rm = TRUE
    ),
    
    sum(
      !validation$location_ids_match,
      na.rm = TRUE
    ),
    
    if (nrow(validation) > 0L) {
      round(
        100 *
          mean(
            validation$distribution_rows_match,
            na.rm = TRUE
          ),
        4
      )
    } else {
      NA_real_
    },
    
    if (nrow(validation) > 0L) {
      round(
        100 *
          mean(
            validation$location_ids_match,
            na.rm = TRUE
          ),
        4
      )
    } else {
      NA_real_
    }
  ),
  
  stringsAsFactors = FALSE
)


write_audit(
  validation_summary,
  "geography_05a7_validation_summary.csv"
)


# ------------------------------------------------------------------------------
# 18. IDENTIFY COUNT MISMATCHES
# ------------------------------------------------------------------------------

validation_mismatches <- validation[
  !validation$distribution_rows_match |
    !validation$location_ids_match,
  ,
  drop = FALSE
]


write_audit(
  validation_mismatches,
  "geography_05a7_count_mismatches.csv"
)


# ------------------------------------------------------------------------------
# 19. LOCATION FREQUENCY
# ------------------------------------------------------------------------------

location_frequency <- aggregate(
  final_wcvp_id ~ wcvp_location_id,
  data = taxon_location_pairs,
  FUN = function(x) {
    length(
      unique(x)
    )
  }
)


names(location_frequency) <- c(
  "wcvp_location_id",
  "n_vpjd_taxa"
)


location_frequency <- location_frequency[
  order(
    -location_frequency$n_vpjd_taxa,
    location_frequency$wcvp_location_id
  ),
  ,
  drop = FALSE
]


write_audit(
  location_frequency,
  "geography_05a7_wcvp_location_frequency.csv"
)


# ------------------------------------------------------------------------------
# 20. ESTABLISHMENT-MEANS PROFILE
# ------------------------------------------------------------------------------

establishment_profile <- as.data.frame(
  table(
    vpjd_wcvp_distribution$establishment_means,
    useNA = "ifany"
  ),
  stringsAsFactors = FALSE
)


names(establishment_profile) <- c(
  "establishment_means",
  "n_rows"
)


establishment_profile <- establishment_profile[
  order(
    -establishment_profile$n_rows
  ),
  ,
  drop = FALSE
]


write_audit(
  establishment_profile,
  "geography_05a7_establishment_means_profile.csv"
)


# ------------------------------------------------------------------------------
# 21. OUTPUT MANIFEST
# ------------------------------------------------------------------------------

output_files <- c(
  OUTPUT_PAIR_FILE,
  OUTPUT_DETAIL_FILE,
  OUTPUT_SUMMARY_FILE
)


output_manifest <- data.frame(
  
  file_name = basename(
    output_files
  ),
  
  full_path = output_files,
  
  exists = file.exists(
    output_files
  ),
  
  size_bytes = vapply(
    output_files,
    function(path) {
      
      if (file.exists(path)) {
        file.info(path)$size
      } else {
        NA_real_
      }
    },
    numeric(1)
  ),
  
  stringsAsFactors = FALSE
)


write_audit(
  output_manifest,
  "geography_05a7_output_manifest.csv"
)


# ------------------------------------------------------------------------------
# 22. DECISION GATE FOR NEXT STAGE
# ------------------------------------------------------------------------------

n_historical_taxa <- length(
  historical_taxon_ids
)


n_distribution_matches <- sum(
  validation$distribution_rows_match,
  na.rm = TRUE
)


n_location_matches <- sum(
  validation$location_ids_match,
  na.rm = TRUE
)


all_distribution_counts_match <-
  n_distribution_matches ==
  n_historical_taxa


all_location_counts_match <-
  n_location_matches ==
  n_historical_taxa


zero_distribution_taxa_accounted_for <-
  all(
    validation$recreated_distribution_rows[
      validation$final_wcvp_id %in%
        historical_taxa_not_recovered
    ] == 0
  ) &&
  all(
    validation$recreated_location_ids[
      validation$final_wcvp_id %in%
        historical_taxa_not_recovered
    ] == 0
  )


sufficient_for_05b <-
  all_distribution_counts_match &&
  all_location_counts_match &&
  zero_distribution_taxa_accounted_for &&
  nrow(taxon_location_pairs) > 0L


validation_gate <- data.frame(
  
  criterion = c(
    "historical_distribution_row_counts_reproduced",
    "historical_distinct_location_counts_reproduced",
    "zero_distribution_taxa_accounted_for",
    "taxon_location_pairs_extracted",
    "sufficient_for_05b"
  ),
  
  passed = c(
    all_distribution_counts_match,
    all_location_counts_match,
    zero_distribution_taxa_accounted_for,
    nrow(taxon_location_pairs) > 0L,
    sufficient_for_05b
  ),
  
  stringsAsFactors = FALSE
)


write_audit(
  validation_gate,
  "geography_05a7_validation_gate.csv"
)


# ------------------------------------------------------------------------------
# 23. RUN METADATA
# ------------------------------------------------------------------------------

run_metadata <- data.frame(
  
  item = c(
    "script",
    "run_time",
    "raw_wcvp_distribution",
    "historical_summary",
    "taxon_id_field",
    "location_id_field",
    "locality_field",
    "establishment_field",
    "historical_outputs_modified",
    "published_vpjd_modified",
    "occurrence_data_modified",
    "range_matrix_modified",
    "historical_star_assignments_used",
    "star_categories_assigned"
  ),
  
  value = c(
    "geography_05a7_extract_wcvp_taxon_distribution.R",
    
    format(
      Sys.time(),
      "%Y-%m-%d %H:%M:%S %Z"
    ),
    
    RAW_WCVP_FILE,
    HISTORICAL_SUMMARY_FILE,
    TAXON_ID_FIELD,
    LOCATION_ID_FIELD,
    
    ifelse(
      is.na(LOCALITY_FIELD),
      "NOT DETECTED",
      LOCALITY_FIELD
    ),
    
    ifelse(
      is.na(ESTABLISHMENT_FIELD),
      "NOT DETECTED",
      ESTABLISHMENT_FIELD
    ),
    
    "FALSE",
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
  "geography_05a7_run_metadata.csv"
)


capture.output(
  sessionInfo(),
  file = file.path(
    AUDIT_ROOT,
    "geography_05a7_sessionInfo.txt"
  )
)


# ------------------------------------------------------------------------------
# 24. CONSOLE SUMMARY
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" VPJD GEOGRAPHY 05a7 - EXTRACTION COMPLETE\n")
cat("============================================================\n\n")


cat("Raw WCVP distribution profile:\n\n")

print(
  raw_profile,
  row.names = FALSE
)


cat("\nValidation summary:\n\n")

print(
  validation_summary,
  row.names = FALSE
)


cat("\nValidation gate:\n\n")

print(
  validation_gate,
  row.names = FALSE
)


cat(
  "\nMost frequently represented WCVP location IDs:\n\n"
)


if (nrow(location_frequency) > 0L) {
  
  print(
    head(
      location_frequency,
      30L
    ),
    row.names = FALSE
  )
  
} else {
  
  cat(
    "No WCVP taxon x location pairs recovered.\n"
  )
}


cat(
  "\nEstablishment-means profile:\n\n"
)


if (nrow(establishment_profile) > 0L) {
  
  print(
    establishment_profile,
    row.names = FALSE
  )
  
} else {
  
  cat(
    "No establishment-means values available.\n"
  )
}


cat("\nOutputs:\n\n")

print(
  output_manifest,
  row.names = FALSE
)


cat(
  "\nAuthoritative candidate taxon x WCVP-location evidence:\n"
)

cat(
  OUTPUT_PAIR_FILE,
  "\n\n"
)


cat(
  "Detailed positive WCVP distribution records:\n"
)

cat(
  OUTPUT_DETAIL_FILE,
  "\n\n"
)


cat(
  "Recreated historical distribution summary:\n"
)

cat(
  OUTPUT_SUMMARY_FILE,
  "\n\n"
)


cat(
  "Audit outputs:\n"
)

cat(
  AUDIT_ROOT,
  "\n\n"
)


cat("Historical WCVP outputs were NOT modified.\n")
cat("Published VPJD v1.0.0 was NOT modified.\n")
cat("Occurrence data were NOT modified.\n")
cat("Current VPJD range matrix was NOT modified.\n")
cat("Historical Star assignments were NOT used.\n")
cat("NO Star categories were assigned.\n")


cat("\nINTERPRETATION:\n")


if (sufficient_for_05b) {
  
  cat(
    paste0(
      "The raw WCVP distribution source successfully reproduces the ",
      "historical 03b distribution-row and distinct-location counts for ",
      "all historical VPJD-linked WCVP taxa. The extracted distinct ",
      "taxon_id x location_id pairs can therefore be treated as the ",
      "candidate authoritative positive WCVP global-distribution evidence ",
      "for the next Geography Module stage.\n"
    )
  )
  
} else {
  
  cat(
    paste0(
      "The raw WCVP distribution source has been extracted, but one or ",
      "more validation criteria do not yet reproduce the historical 03b ",
      "results exactly. Review geography_05a7_count_mismatches.csv, ",
      "geography_05a7_historical_taxa_not_recovered.csv and the detected ",
      "raw schema before treating the extracted taxon x location pairs as ",
      "authoritative.\n"
    )
  )
}


cat("\nNEXT STEP:\n")


if (sufficient_for_05b) {
  
  cat(
    paste0(
      "Proceed to geography_05b_build_global_taxon_range.R. ",
      "That script should map each recovered WCVP location_id to its ",
      "geographic meaning, integrate the positive global WCVP range with ",
      "the published VPJD taxonomic backbone and existing Japanese ",
      "occurrence-supported range matrix, and derive the geographic ",
      "variables required by Nakamura's Key. It should still assign NO ",
      "Star categories.\n"
    )
  )
  
} else {
  
  cat(
    paste0(
      "Do NOT proceed to geography_05b yet. Resolve any discrepancies ",
      "between the raw WCVP distribution and the historical 03b summary ",
      "first. The validation should reproduce the historical counts before ",
      "the global range evidence is promoted downstream.\n"
    )
  )
}


cat("\n============================================================\n")