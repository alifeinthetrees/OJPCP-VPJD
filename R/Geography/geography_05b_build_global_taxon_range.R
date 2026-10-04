# ==============================================================================
# VPJD GEOGRAPHY MODULE
# geography_05b_build_global_taxon_range.R
#
# PURPOSE
#
# Build the authoritative global taxon-range model for published VPJD v1.0.0
# taxa by integrating:
#
#   1. Published VPJD taxonomic backbone
#   2. Validated WCVP taxon x TDWG location evidence from geography_05a7
#   3. WCVP distribution-record establishment evidence
#   4. WCVP / DwC-A geographic location reference
#   5. Positive Japanese occurrence-supported range evidence from
#      geography_05a
#
# PRINCIPLE
#
# WCVP provides the GLOBAL distribution framework.
#
# VPJD analytical occurrences provide the finer WITHIN-JAPAN distribution
# framework required for later application of Nakamura's Key to Stars.
#
# This script DOES NOT:
#
#   * assign Star categories;
#   * derive final Nakamura decision variables;
#   * infer absence from missing occurrence/distribution records;
#   * use historical Star assignments to determine range;
#   * modify published VPJD;
#   * modify WCVP source data;
#   * modify occurrence data.
#
# OUTPUT CONCEPT
#
# For each published VPJD taxon, retain:
#
#   * WCVP/TDWG global geographic units
#   * geographic names where recoverable
#   * establishment status
#   * native/unspecified versus introduced evidence
#   * whether Japan occurs in WCVP
#   * positive Japanese botanical-area evidence
#   * counts of global and Japanese geographic units
#   * explicit evidence availability/status
#
# ==============================================================================


# ------------------------------------------------------------------------------
# 01. PATHS
# ------------------------------------------------------------------------------

PROJECT_ROOT <- "I:/R/OJPCP/VPJD-OJPCP"

PUBLISHED_ROOT <- "I:/R/Data/VPJD_v1.0.0"


PUBLISHED_TAXA_FILE <- file.path(
  PUBLISHED_ROOT,
  "data",
  "vpjd_recognised_taxa.csv"
)

WCVP_PAIR_FILE <- file.path(
  PROJECT_ROOT,
  "data",
  "derived",
  "geography",
  "taxon_range",
  "wcvp",
  "vpjd_wcvp_taxon_location_pairs.csv"
)


WCVP_DETAIL_FILE <- file.path(
  PROJECT_ROOT,
  "data",
  "derived",
  "geography",
  "taxon_range",
  "wcvp",
  "vpjd_wcvp_distribution_records.csv"
)


JAPAN_RANGE_FILE <- file.path(
  PROJECT_ROOT,
  "data",
  "derived",
  "geography",
  "taxon_range",
  "vpjd_taxon_japan_positive_range_long.csv"
)


JAPAN_MATRIX_FILE <- file.path(
  PROJECT_ROOT,
  "data",
  "derived",
  "geography",
  "taxon_range",
  "vpjd_taxon_range_matrix.csv"
)


WCVP_DWCA_ROOT <- file.path(
  PROJECT_ROOT,
  "data",
  "raw",
  "geography",
  "wcvp_dwca"
)


OUTPUT_ROOT <- file.path(
  PROJECT_ROOT,
  "data",
  "derived",
  "geography",
  "global_range"
)


AUDIT_ROOT <- file.path(
  PROJECT_ROOT,
  "audit",
  "geography",
  "global_range"
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


GLOBAL_LONG_FILE <- file.path(
  OUTPUT_ROOT,
  "vpjd_global_range_long.csv"
)


GLOBAL_TAXON_SUMMARY_FILE <- file.path(
  OUTPUT_ROOT,
  "vpjd_global_taxon_range_summary.csv"
)


INTEGRATED_RANGE_FILE <- file.path(
  OUTPUT_ROOT,
  "vpjd_integrated_taxon_range.csv"
)


LOCATION_REFERENCE_FILE <- file.path(
  OUTPUT_ROOT,
  "vpjd_wcvp_location_reference.csv"
)


# ------------------------------------------------------------------------------
# 02. START
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" VPJD GEOGRAPHY 05b - BUILD GLOBAL TAXON RANGE\n")
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


safe_read_csv <- function(
    path,
    sep = ","
) {
  
  if (!file.exists(path)) {
    return(NULL)
  }
  
  
  tryCatch(
    
    read.csv(
      path,
      sep = sep,
      stringsAsFactors = FALSE,
      check.names = FALSE,
      encoding = "UTF-8"
    ),
    
    error = function(e1) {
      
      tryCatch(
        
        read.csv(
          path,
          sep = sep,
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


write_audit <- function(
    x,
    filename
) {
  
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


first_existing_field <- function(
    dat,
    candidates
) {
  
  hits <- candidates[
    candidates %in% names(dat)
  ]
  
  
  if (length(hits) == 0L) {
    return(NA_character_)
  }
  
  
  hits[1L]
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


collapse_unique <- function(
    x,
    separator = " | "
) {
  
  x <- clean_character(x)
  
  x <- sort(
    unique(
      x[
        !is.na(x)
      ]
    )
  )
  
  
  if (length(x) == 0L) {
    return(NA_character_)
  }
  
  
  paste(
    x,
    collapse = separator
  )
}


# ------------------------------------------------------------------------------
# 04. VERIFY CORE INPUTS
# ------------------------------------------------------------------------------

core_inputs <- data.frame(
  
  input = c(
    "published_vpjd_taxa",
    "wcvp_taxon_location_pairs",
    "wcvp_distribution_detail",
    "japan_positive_range",
    "japan_range_matrix"
  ),
  
  full_path = c(
    PUBLISHED_TAXA_FILE,
    WCVP_PAIR_FILE,
    WCVP_DETAIL_FILE,
    JAPAN_RANGE_FILE,
    JAPAN_MATRIX_FILE
  ),
  
  exists = c(
    file.exists(PUBLISHED_TAXA_FILE),
    file.exists(WCVP_PAIR_FILE),
    file.exists(WCVP_DETAIL_FILE),
    file.exists(JAPAN_RANGE_FILE),
    file.exists(JAPAN_MATRIX_FILE)
  ),
  
  stringsAsFactors = FALSE
)


write_audit(
  core_inputs,
  "geography_05b_input_status.csv"
)


cat("Core input status:\n\n")

print(
  core_inputs,
  row.names = FALSE
)


if (!all(core_inputs$exists)) {
  
  stop(
    paste0(
      "One or more required 05b inputs are missing. ",
      "Review geography_05b_input_status.csv."
    )
  )
}


# ------------------------------------------------------------------------------
# 05. READ PUBLISHED VPJD TAXON BACKBONE
# ------------------------------------------------------------------------------

published_taxa <- safe_read_csv(
  PUBLISHED_TAXA_FILE
)


if (is.null(published_taxa)) {
  stop("Unable to read published VPJD taxonomic backbone.")
}


names(published_taxa) <- normalise_name(
  names(published_taxa)
)


PUBLISHED_ID_FIELD <- first_existing_field(
  published_taxa,
  c(
    "wcvp_plant_name_id",
    "final_wcvp_accepted_plant_name_id",
    "final_wcvp_id",
    "plant_name_id"
  )
)


PUBLISHED_NAME_FIELD <- first_existing_field(
  published_taxa,
  c(
    "wcvp_taxon_name",
    "taxon_name",
    "scientific_name",
    "scientificname"
  )
)


PUBLISHED_RANK_FIELD <- first_existing_field(
  published_taxa,
  c(
    "wcvp_taxon_rank",
    "taxon_rank",
    "rank"
  )
)


if (is.na(PUBLISHED_ID_FIELD)) {
  stop("Published VPJD WCVP identifier field not detected.")
}


published_taxa$star_evidence_taxon_id <- clean_character(
  published_taxa[[PUBLISHED_ID_FIELD]]
)


if (anyDuplicated(
  published_taxa$star_evidence_taxon_id
) > 0L) {
  
  stop(
    "Published VPJD taxon backbone contains duplicate WCVP identifiers."
  )
}


cat(
  "\nPublished VPJD taxa:",
  nrow(published_taxa),
  "\n"
)


# ------------------------------------------------------------------------------
# 06. READ VALIDATED 05a7 WCVP RANGE EVIDENCE
# ------------------------------------------------------------------------------

wcvp_pairs <- safe_read_csv(
  WCVP_PAIR_FILE
)


wcvp_detail <- safe_read_csv(
  WCVP_DETAIL_FILE
)


if (
  is.null(wcvp_pairs) ||
  is.null(wcvp_detail)
) {
  
  stop(
    "Unable to read validated geography_05a7 WCVP outputs."
  )
}


names(wcvp_pairs) <- normalise_name(
  names(wcvp_pairs)
)


names(wcvp_detail) <- normalise_name(
  names(wcvp_detail)
)


PAIR_TAXON_FIELD <- first_existing_field(
  wcvp_pairs,
  c(
    "final_wcvp_id",
    "taxon_id"
  )
)


PAIR_LOCATION_FIELD <- first_existing_field(
  wcvp_pairs,
  c(
    "wcvp_location_id",
    "location_id"
  )
)


DETAIL_TAXON_FIELD <- first_existing_field(
  wcvp_detail,
  c(
    "final_wcvp_id",
    "taxon_id"
  )
)


DETAIL_LOCATION_FIELD <- first_existing_field(
  wcvp_detail,
  c(
    "wcvp_location_id",
    "location_id"
  )
)


DETAIL_LOCALITY_FIELD <- first_existing_field(
  wcvp_detail,
  c(
    "locality"
  )
)


DETAIL_ESTABLISHMENT_FIELD <- first_existing_field(
  wcvp_detail,
  c(
    "establishment_means",
    "establishmentmeans"
  )
)


if (
  is.na(PAIR_TAXON_FIELD) ||
  is.na(PAIR_LOCATION_FIELD) ||
  is.na(DETAIL_TAXON_FIELD) ||
  is.na(DETAIL_LOCATION_FIELD)
) {
  
  stop(
    "Required taxon/location fields not detected in 05a7 outputs."
  )
}


# ------------------------------------------------------------------------------
# 07. STANDARDISE WCVP DETAIL
# ------------------------------------------------------------------------------

global_detail <- data.frame(
  
  star_evidence_taxon_id = clean_character(
    wcvp_detail[[DETAIL_TAXON_FIELD]]
  ),
  
  wcvp_location_id = clean_character(
    wcvp_detail[[DETAIL_LOCATION_FIELD]]
  ),
  
  stringsAsFactors = FALSE
)


if (!is.na(DETAIL_LOCALITY_FIELD)) {
  
  global_detail$wcvp_locality <- clean_character(
    wcvp_detail[[DETAIL_LOCALITY_FIELD]]
  )
  
} else {
  
  global_detail$wcvp_locality <- NA_character_
}


if (!is.na(DETAIL_ESTABLISHMENT_FIELD)) {
  
  global_detail$establishment_means <- tolower(
    clean_character(
      wcvp_detail[[DETAIL_ESTABLISHMENT_FIELD]]
    )
  )
  
} else {
  
  global_detail$establishment_means <- NA_character_
}


global_detail <- global_detail[
  !is.na(global_detail$star_evidence_taxon_id) &
    !is.na(global_detail$wcvp_location_id),
  ,
  drop = FALSE
]


# ------------------------------------------------------------------------------
# 08. DISCOVER WCVP / TDWG LOCATION REFERENCE
# ------------------------------------------------------------------------------

dwca_files <- list.files(
  WCVP_DWCA_ROOT,
  recursive = TRUE,
  full.names = TRUE,
  include.dirs = FALSE
)


dwca_inventory <- data.frame(
  
  file_name = basename(
    dwca_files
  ),
  
  extension = tolower(
    tools::file_ext(
      dwca_files
    )
  ),
  
  size_bytes = if (length(dwca_files) > 0L) {
    file.info(dwca_files)$size
  } else {
    numeric(0)
  },
  
  full_path = dwca_files,
  
  stringsAsFactors = FALSE
)


if (nrow(dwca_inventory) > 0L) {
  
  dwca_inventory$location_reference_score <-
    as.integer(
      grepl(
        "location",
        dwca_inventory$file_name,
        ignore.case = TRUE
      )
    ) * 5L +
    as.integer(
      grepl(
        "geograph|tdwg|area|region",
        dwca_inventory$file_name,
        ignore.case = TRUE
      )
    ) * 3L +
    as.integer(
      dwca_inventory$extension %in%
        c(
          "csv",
          "txt",
          "tsv"
        )
    )
  
  dwca_inventory <- dwca_inventory[
    order(
      -dwca_inventory$location_reference_score,
      -dwca_inventory$size_bytes
    ),
    ,
    drop = FALSE
  ]
}


write_audit(
  dwca_inventory,
  "geography_05b_wcvp_dwca_inventory.csv"
)


# ------------------------------------------------------------------------------
# 09. PROFILE POSSIBLE LOCATION-REFERENCE FILES
# ------------------------------------------------------------------------------

location_reference_candidates <- list()


if (nrow(dwca_inventory) > 0L) {
  
  candidate_rows <- which(
    dwca_inventory$location_reference_score > 0L
  )
  
  
  for (i in candidate_rows) {
    
    candidate_path <- dwca_inventory$full_path[i]
    
    
    header_line <- tryCatch(
      readLines(
        candidate_path,
        n = 1L,
        warn = FALSE,
        encoding = "UTF-8"
      ),
      error = function(e) {
        character(0)
      }
    )
    
    
    if (length(header_line) == 0L) {
      next
    }
    
    
    pipe_fields <- strsplit(
      header_line,
      "\\|"
    )[[1]]
    
    
    comma_fields <- strsplit(
      header_line,
      ","
    )[[1]]
    
    
    tab_fields <- strsplit(
      header_line,
      "\t"
    )[[1]]
    
    
    delimiter <- ","
    
    
    if (
      length(pipe_fields) >=
      max(
        length(comma_fields),
        length(tab_fields)
      )
    ) {
      
      delimiter <- "|"
      
    } else if (
      length(tab_fields) >
      length(comma_fields)
    ) {
      
      delimiter <- "\t"
    }
    
    
    sample_dat <- tryCatch(
      
      read.csv(
        candidate_path,
        sep = delimiter,
        stringsAsFactors = FALSE,
        check.names = FALSE,
        nrows = 100L
      ),
      
      error = function(e) {
        NULL
      }
    )
    
    
    if (is.null(sample_dat)) {
      next
    }
    
    
    sample_names <- normalise_name(
      names(sample_dat)
    )
    
    
    id_hits <- sample_names[
      grepl(
        "locationid|location_id|tdwg|code",
        sample_names
      )
    ]
    
    
    name_hits <- sample_names[
      grepl(
        "locality|name|location|region|area",
        sample_names
      )
    ]
    
    
    list_index <-
      length(location_reference_candidates) + 1L
    
    
    location_reference_candidates[[list_index]] <-
      data.frame(
        
        file_name = dwca_inventory$file_name[i],
        
        delimiter = ifelse(
          delimiter == "\t",
          "TAB",
          delimiter
        ),
        
        n_columns = ncol(
          sample_dat
        ),
        
        candidate_id_fields = if (
          length(id_hits) > 0L
        ) {
          paste(
            id_hits,
            collapse = " | "
          )
        } else {
          NA_character_
        },
        
        candidate_name_fields = if (
          length(name_hits) > 0L
        ) {
          paste(
            name_hits,
            collapse = " | "
          )
        } else {
          NA_character_
        },
        
        full_path = candidate_path,
        
        stringsAsFactors = FALSE
      )
  }
}


if (
  length(location_reference_candidates) > 0L
) {
  
  location_reference_profile <- do.call(
    rbind,
    location_reference_candidates
  )
  
} else {
  
  location_reference_profile <- data.frame(
    
    file_name = character(0),
    delimiter = character(0),
    n_columns = integer(0),
    candidate_id_fields = character(0),
    candidate_name_fields = character(0),
    full_path = character(0),
    
    stringsAsFactors = FALSE
  )
}


write_audit(
  location_reference_profile,
  "geography_05b_location_reference_candidates.csv"
)


# ------------------------------------------------------------------------------
# 10. BUILD LOCATION REFERENCE FROM VALIDATED WCVP DETAIL
# ------------------------------------------------------------------------------

# The validated 05a7 detail contains both:
#
#   wcvp_location_id
#   wcvp_locality
#
# This provides the primary observed location lookup for the 367 WCVP/TDWG
# units represented by VPJD taxa.
#
# We do NOT need to invent geographic names.

location_reference <- unique(
  global_detail[
    ,
    c(
      "wcvp_location_id",
      "wcvp_locality"
    ),
    drop = FALSE
  ]
)


location_reference <- location_reference[
  !is.na(location_reference$wcvp_location_id),
  ,
  drop = FALSE
]


location_reference <- location_reference[
  order(
    location_reference$wcvp_location_id,
    location_reference$wcvp_locality
  ),
  ,
  drop = FALSE
]


# Check whether one location ID maps to more than one locality.

location_mapping_counts <- aggregate(
  wcvp_locality ~ wcvp_location_id,
  data = location_reference,
  FUN = function(x) {
    safe_distinct_count(x)
  }
)


names(location_mapping_counts)[2] <-
  "n_distinct_localities"


ambiguous_location_mappings <-
  location_mapping_counts[
    location_mapping_counts$n_distinct_localities > 1L,
    ,
    drop = FALSE
  ]


write_audit(
  ambiguous_location_mappings,
  "geography_05b_ambiguous_location_mappings.csv"
)


if (nrow(ambiguous_location_mappings) > 0L) {
  
  stop(
    paste0(
      "One or more WCVP location IDs map to multiple locality names. ",
      "Review geography_05b_ambiguous_location_mappings.csv before ",
      "building the global range."
    )
  )
}


location_reference <- location_reference[
  !duplicated(
    location_reference$wcvp_location_id
  ),
  ,
  drop = FALSE
]


location_reference$tdwg_code <- sub(
  "^TDWG:",
  "",
  location_reference$wcvp_location_id
)


location_reference$location_reference_source <-
  "WCVP distribution DwC-A locality field"


write.csv(
  location_reference,
  LOCATION_REFERENCE_FILE,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)


# ------------------------------------------------------------------------------
# 11. CLASSIFY ESTABLISHMENT EVIDENCE
# ------------------------------------------------------------------------------

global_detail$is_introduced <-
  !is.na(global_detail$establishment_means) &
  global_detail$establishment_means ==
  "introduced"


global_detail$is_native_or_unspecified <-
  !global_detail$is_introduced


global_detail$establishment_evidence_class <-
  ifelse(
    global_detail$is_introduced,
    "introduced",
    "native_or_unspecified"
  )


# IMPORTANT:
#
# WCVP records with missing establishment_means are NOT asserted to be native.
# They are deliberately retained as "native_or_unspecified" for range evidence.
#
# This distinction is preserved so 05c can decide exactly how Nakamura's Key
# should use native, introduced and unspecified range evidence.


# ------------------------------------------------------------------------------
# 12. COLLAPSE TO TAXON x GLOBAL LOCATION
# ------------------------------------------------------------------------------

taxon_location_keys <- unique(
  global_detail[
    ,
    c(
      "star_evidence_taxon_id",
      "wcvp_location_id"
    ),
    drop = FALSE
  ]
)


global_range_list <- vector(
  "list",
  nrow(taxon_location_keys)
)


if (nrow(taxon_location_keys) > 0L) {
  
  for (i in seq_len(nrow(taxon_location_keys))) {
    
    taxon_value <-
      taxon_location_keys$star_evidence_taxon_id[i]
    
    location_value <-
      taxon_location_keys$wcvp_location_id[i]
    
    
    rows <- global_detail[
      global_detail$star_evidence_taxon_id ==
        taxon_value &
        global_detail$wcvp_location_id ==
        location_value,
      ,
      drop = FALSE
    ]
    
    
    introduced_present <- any(
      rows$is_introduced,
      na.rm = TRUE
    )
    
    
    native_or_unspecified_present <- any(
      rows$is_native_or_unspecified,
      na.rm = TRUE
    )
    
    
    establishment_class <-
      if (
        introduced_present &&
        native_or_unspecified_present
      ) {
        
        "mixed"
        
      } else if (introduced_present) {
        
        "introduced"
        
      } else {
        
        "native_or_unspecified"
      }
    
    
    global_range_list[[i]] <- data.frame(
      
      star_evidence_taxon_id = taxon_value,
      
      wcvp_location_id = location_value,
      
      wcvp_locality = collapse_unique(
        rows$wcvp_locality
      ),
      
      introduced_evidence = introduced_present,
      
      native_or_unspecified_evidence =
        native_or_unspecified_present,
      
      establishment_class =
        establishment_class,
      
      stringsAsFactors = FALSE
    )
  }
}


if (length(global_range_list) > 0L) {
  
  global_range_long <- do.call(
    rbind,
    global_range_list
  )
  
} else {
  
  global_range_long <- data.frame(
    
    star_evidence_taxon_id = character(0),
    wcvp_location_id = character(0),
    wcvp_locality = character(0),
    introduced_evidence = logical(0),
    native_or_unspecified_evidence = logical(0),
    establishment_class = character(0),
    
    stringsAsFactors = FALSE
  )
}


global_range_long$tdwg_code <- sub(
  "^TDWG:",
  "",
  global_range_long$wcvp_location_id
)


global_range_long$is_japan_wcvp_unit <-
  global_range_long$wcvp_location_id ==
  "TDWG:JAP"


global_range_long$is_outside_japan_wcvp_unit <-
  !global_range_long$is_japan_wcvp_unit


global_range_long <- global_range_long[
  order(
    global_range_long$star_evidence_taxon_id,
    global_range_long$wcvp_location_id
  ),
  ,
  drop = FALSE
]


write.csv(
  global_range_long,
  GLOBAL_LONG_FILE,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)


# ------------------------------------------------------------------------------
# 13. VALIDATE GLOBAL LONG TABLE
# ------------------------------------------------------------------------------

global_long_validation <- data.frame(
  
  metric = c(
    "global_taxon_location_rows",
    "distinct_taxa",
    "distinct_wcvp_locations",
    "duplicate_taxon_location_pairs",
    "japan_taxon_location_rows",
    "outside_japan_taxon_location_rows",
    "introduced_taxon_location_rows",
    "native_or_unspecified_taxon_location_rows",
    "mixed_establishment_taxon_location_rows"
  ),
  
  value = c(
    nrow(global_range_long),
    
    safe_distinct_count(
      global_range_long$star_evidence_taxon_id
    ),
    
    safe_distinct_count(
      global_range_long$wcvp_location_id
    ),
    
    nrow(global_range_long) -
      nrow(
        unique(
          global_range_long[
            ,
            c(
              "star_evidence_taxon_id",
              "wcvp_location_id"
            ),
            drop = FALSE
          ]
        )
      ),
    
    sum(
      global_range_long$is_japan_wcvp_unit,
      na.rm = TRUE
    ),
    
    sum(
      global_range_long$is_outside_japan_wcvp_unit,
      na.rm = TRUE
    ),
    
    sum(
      global_range_long$establishment_class ==
        "introduced",
      na.rm = TRUE
    ),
    
    sum(
      global_range_long$establishment_class ==
        "native_or_unspecified",
      na.rm = TRUE
    ),
    
    sum(
      global_range_long$establishment_class ==
        "mixed",
      na.rm = TRUE
    )
  ),
  
  stringsAsFactors = FALSE
)


write_audit(
  global_long_validation,
  "geography_05b_global_long_validation.csv"
)


# ------------------------------------------------------------------------------
# 14. BUILD TAXON-LEVEL GLOBAL SUMMARY
# ------------------------------------------------------------------------------

published_ids <- published_taxa$star_evidence_taxon_id


global_taxon_summary_list <- vector(
  "list",
  length(published_ids)
)


for (i in seq_along(published_ids)) {
  
  taxon_value <- published_ids[i]
  
  
  rows <- global_range_long[
    global_range_long$star_evidence_taxon_id ==
      taxon_value,
    ,
    drop = FALSE
  ]
  
  
  n_global_locations <- nrow(rows)
  
  
  n_outside_japan_locations <- sum(
    rows$is_outside_japan_wcvp_unit,
    na.rm = TRUE
  )
  
  
  n_introduced_locations <- sum(
    rows$introduced_evidence,
    na.rm = TRUE
  )
  
  
  n_native_or_unspecified_locations <- sum(
    rows$native_or_unspecified_evidence,
    na.rm = TRUE
  )
  
  
  japan_rows <- rows[
    rows$is_japan_wcvp_unit,
    ,
    drop = FALSE
  ]
  
  
  wcvp_japan_present <-
    nrow(japan_rows) > 0L
  
  
  wcvp_japan_introduced <-
    if (nrow(japan_rows) > 0L) {
      any(
        japan_rows$introduced_evidence,
        na.rm = TRUE
      )
    } else {
      FALSE
    }
  
  
  wcvp_japan_native_or_unspecified <-
    if (nrow(japan_rows) > 0L) {
      any(
        japan_rows$native_or_unspecified_evidence,
        na.rm = TRUE
      )
    } else {
      FALSE
    }
  
  
  global_taxon_summary_list[[i]] <- data.frame(
    
    star_evidence_taxon_id = taxon_value,
    
    wcvp_global_distribution_evidence_available =
      n_global_locations > 0L,
    
    n_wcvp_global_locations =
      n_global_locations,
    
    n_wcvp_outside_japan_locations =
      n_outside_japan_locations,
    
    n_wcvp_introduced_locations =
      n_introduced_locations,
    
    n_wcvp_native_or_unspecified_locations =
      n_native_or_unspecified_locations,
    
    wcvp_japan_present =
      wcvp_japan_present,
    
    wcvp_japan_introduced =
      wcvp_japan_introduced,
    
    wcvp_japan_native_or_unspecified =
      wcvp_japan_native_or_unspecified,
    
    wcvp_outside_japan_present =
      n_outside_japan_locations > 0L,
    
    wcvp_location_ids =
      collapse_unique(
        rows$wcvp_location_id
      ),
    
    wcvp_localities =
      collapse_unique(
        rows$wcvp_locality
      ),
    
    stringsAsFactors = FALSE
  )
}


global_taxon_summary <- do.call(
  rbind,
  global_taxon_summary_list
)


write.csv(
  global_taxon_summary,
  GLOBAL_TAXON_SUMMARY_FILE,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)


# ------------------------------------------------------------------------------
# 15. READ JAPANESE POSITIVE RANGE EVIDENCE
# ------------------------------------------------------------------------------

japan_range <- safe_read_csv(
  JAPAN_RANGE_FILE
)


japan_matrix <- safe_read_csv(
  JAPAN_MATRIX_FILE
)


if (
  is.null(japan_range) ||
  is.null(japan_matrix)
) {
  
  stop(
    "Unable to read geography_05a Japanese range outputs."
  )
}


names(japan_range) <- normalise_name(
  names(japan_range)
)


names(japan_matrix) <- normalise_name(
  names(japan_matrix)
)


JAPAN_TAXON_FIELD <- first_existing_field(
  japan_range,
  c(
    "wcvp_plant_name_id",
    "star_evidence_taxon_id",
    "final_wcvp_id"
  )
)


JAPAN_AREA_FIELD <- first_existing_field(
  japan_range,
  c(
    "botanical_area",
    "botanical_area_name",
    "area_name",
    "area"
  )
)


if (is.na(JAPAN_TAXON_FIELD)) {
  
  stop(
    "Japanese positive-range taxon identifier field not detected."
  )
}


if (is.na(JAPAN_AREA_FIELD)) {
  
  stop(
    paste0(
      "Japanese botanical-area field not detected. ",
      "Inspect vpjd_taxon_japan_positive_range_long.csv."
    )
  )
}


japan_positive <- data.frame(
  
  star_evidence_taxon_id = clean_character(
    japan_range[[JAPAN_TAXON_FIELD]]
  ),
  
  japan_botanical_area = clean_character(
    japan_range[[JAPAN_AREA_FIELD]]
  ),
  
  stringsAsFactors = FALSE
)


japan_positive <- japan_positive[
  !is.na(japan_positive$star_evidence_taxon_id) &
    !is.na(japan_positive$japan_botanical_area),
  ,
  drop = FALSE
]


japan_positive <- unique(
  japan_positive
)


# ------------------------------------------------------------------------------
# 16. SUMMARISE JAPANESE RANGE PER TAXON
# ------------------------------------------------------------------------------

japan_summary_list <- vector(
  "list",
  length(published_ids)
)


for (i in seq_along(published_ids)) {
  
  taxon_value <- published_ids[i]
  
  
  rows <- japan_positive[
    japan_positive$star_evidence_taxon_id ==
      taxon_value,
    ,
    drop = FALSE
  ]
  
  
  japan_summary_list[[i]] <- data.frame(
    
    star_evidence_taxon_id =
      taxon_value,
    
    japan_distribution_evidence_available =
      nrow(rows) > 0L,
    
    n_japan_botanical_areas =
      safe_distinct_count(
        rows$japan_botanical_area
      ),
    
    japan_botanical_areas =
      collapse_unique(
        rows$japan_botanical_area
      ),
    
    stringsAsFactors = FALSE
  )
}


japan_summary <- do.call(
  rbind,
  japan_summary_list
)


# ------------------------------------------------------------------------------
# 17. BUILD INTEGRATED TAXON RANGE
# ------------------------------------------------------------------------------

integrated_range <- published_taxa


integrated_range <- merge(
  integrated_range,
  global_taxon_summary,
  by = "star_evidence_taxon_id",
  all.x = TRUE,
  sort = FALSE
)


integrated_range <- merge(
  integrated_range,
  japan_summary,
  by = "star_evidence_taxon_id",
  all.x = TRUE,
  sort = FALSE
)


# ------------------------------------------------------------------------------
# 18. RESTORE EXPLICIT ZERO / FALSE STATES
# ------------------------------------------------------------------------------

logical_fields <- c(
  "wcvp_global_distribution_evidence_available",
  "wcvp_japan_present",
  "wcvp_japan_introduced",
  "wcvp_japan_native_or_unspecified",
  "wcvp_outside_japan_present",
  "japan_distribution_evidence_available"
)


for (field in logical_fields) {
  
  if (field %in% names(integrated_range)) {
    
    integrated_range[[field]][
      is.na(
        integrated_range[[field]]
      )
    ] <- FALSE
  }
}


count_fields <- c(
  "n_wcvp_global_locations",
  "n_wcvp_outside_japan_locations",
  "n_wcvp_introduced_locations",
  "n_wcvp_native_or_unspecified_locations",
  "n_japan_botanical_areas"
)


for (field in count_fields) {
  
  if (field %in% names(integrated_range)) {
    
    integrated_range[[field]][
      is.na(
        integrated_range[[field]]
      )
    ] <- 0L
  }
}


# ------------------------------------------------------------------------------
# 19. DERIVE EVIDENCE-STATUS FIELDS
# ------------------------------------------------------------------------------

integrated_range$global_range_evidence_status <-
  ifelse(
    integrated_range$wcvp_global_distribution_evidence_available,
    "wcvp_distribution_supported",
    "no_wcvp_distribution_evidence"
  )


integrated_range$japan_range_evidence_status <-
  ifelse(
    integrated_range$japan_distribution_evidence_available,
    "occurrence_supported",
    "no_japan_occurrence_evidence"
  )


integrated_range$combined_range_evidence_status <-
  ifelse(
    
    integrated_range$wcvp_global_distribution_evidence_available &
      integrated_range$japan_distribution_evidence_available,
    
    "global_and_japan_evidence",
    
    ifelse(
      
      integrated_range$wcvp_global_distribution_evidence_available,
      
      "global_evidence_only",
      
      ifelse(
        
        integrated_range$japan_distribution_evidence_available,
        
        "japan_evidence_only",
        
        "no_current_range_evidence"
      )
    )
  )


# ------------------------------------------------------------------------------
# 20. IMPORTANT JAPAN/WCVP CROSS-CHECK
# ------------------------------------------------------------------------------

integrated_range$japan_wcvp_occurrence_concordance <-
  ifelse(
    
    integrated_range$wcvp_japan_present &
      integrated_range$japan_distribution_evidence_available,
    
    "both_support_japan",
    
    ifelse(
      
      integrated_range$wcvp_japan_present &
        !integrated_range$japan_distribution_evidence_available,
      
      "wcvp_japan_only",
      
      ifelse(
        
        !integrated_range$wcvp_japan_present &
          integrated_range$japan_distribution_evidence_available,
        
        "vpjd_occurrence_japan_only",
        
        "neither_currently_supports_japan"
      )
    )
  )


# ------------------------------------------------------------------------------
# 21. WRITE INTEGRATED RANGE
# ------------------------------------------------------------------------------

write.csv(
  integrated_range,
  INTEGRATED_RANGE_FILE,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)


# ------------------------------------------------------------------------------
# 22. VALIDATE PUBLISHED BACKBONE CARDINALITY
# ------------------------------------------------------------------------------

integrated_validation <- data.frame(
  
  metric = c(
    "published_vpjd_taxa",
    "integrated_range_rows",
    "distinct_integrated_taxon_ids",
    "duplicate_integrated_taxon_ids",
    "taxa_with_wcvp_global_evidence",
    "taxa_without_wcvp_global_evidence",
    "taxa_with_japan_occurrence_evidence",
    "taxa_without_japan_occurrence_evidence",
    "taxa_with_wcvp_japan_evidence",
    "taxa_with_wcvp_outside_japan_evidence",
    "taxa_with_wcvp_japan_introduced_evidence"
  ),
  
  value = c(
    nrow(published_taxa),
    
    nrow(integrated_range),
    
    safe_distinct_count(
      integrated_range$star_evidence_taxon_id
    ),
    
    nrow(integrated_range) -
      safe_distinct_count(
        integrated_range$star_evidence_taxon_id
      ),
    
    sum(
      integrated_range$wcvp_global_distribution_evidence_available,
      na.rm = TRUE
    ),
    
    sum(
      !integrated_range$wcvp_global_distribution_evidence_available,
      na.rm = TRUE
    ),
    
    sum(
      integrated_range$japan_distribution_evidence_available,
      na.rm = TRUE
    ),
    
    sum(
      !integrated_range$japan_distribution_evidence_available,
      na.rm = TRUE
    ),
    
    sum(
      integrated_range$wcvp_japan_present,
      na.rm = TRUE
    ),
    
    sum(
      integrated_range$wcvp_outside_japan_present,
      na.rm = TRUE
    ),
    
    sum(
      integrated_range$wcvp_japan_introduced,
      na.rm = TRUE
    )
  ),
  
  stringsAsFactors = FALSE
)


write_audit(
  integrated_validation,
  "geography_05b_integrated_validation.csv"
)


# ------------------------------------------------------------------------------
# 23. EVIDENCE-STATUS SUMMARIES
# ------------------------------------------------------------------------------

combined_status_summary <- as.data.frame(
  table(
    integrated_range$combined_range_evidence_status,
    useNA = "ifany"
  ),
  stringsAsFactors = FALSE
)


names(combined_status_summary) <- c(
  "combined_range_evidence_status",
  "n_taxa"
)


write_audit(
  combined_status_summary,
  "geography_05b_combined_evidence_status.csv"
)


japan_concordance_summary <- as.data.frame(
  table(
    integrated_range$japan_wcvp_occurrence_concordance,
    useNA = "ifany"
  ),
  stringsAsFactors = FALSE
)


names(japan_concordance_summary) <- c(
  "japan_wcvp_occurrence_concordance",
  "n_taxa"
)


write_audit(
  japan_concordance_summary,
  "geography_05b_japan_concordance_summary.csv"
)


# ------------------------------------------------------------------------------
# 24. LOCATION REFERENCE VALIDATION
# ------------------------------------------------------------------------------

location_reference_validation <- data.frame(
  
  metric = c(
    "distinct_wcvp_locations_in_global_range",
    "distinct_location_reference_ids",
    "location_ids_with_locality",
    "location_ids_without_locality",
    "ambiguous_location_id_mappings"
  ),
  
  value = c(
    safe_distinct_count(
      global_range_long$wcvp_location_id
    ),
    
    safe_distinct_count(
      location_reference$wcvp_location_id
    ),
    
    sum(
      !is.na(
        location_reference$wcvp_locality
      )
    ),
    
    sum(
      is.na(
        location_reference$wcvp_locality
      )
    ),
    
    nrow(
      ambiguous_location_mappings
    )
  ),
  
  stringsAsFactors = FALSE
)


write_audit(
  location_reference_validation,
  "geography_05b_location_reference_validation.csv"
)


# ------------------------------------------------------------------------------
# 25. OUTPUT MANIFEST
# ------------------------------------------------------------------------------

output_files <- c(
  GLOBAL_LONG_FILE,
  GLOBAL_TAXON_SUMMARY_FILE,
  INTEGRATED_RANGE_FILE,
  LOCATION_REFERENCE_FILE
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
  "geography_05b_output_manifest.csv"
)


# ------------------------------------------------------------------------------
# 26. VALIDATION GATE
# ------------------------------------------------------------------------------

backbone_cardinality_preserved <-
  nrow(integrated_range) ==
  nrow(published_taxa) &&
  safe_distinct_count(
    integrated_range$star_evidence_taxon_id
  ) ==
  nrow(published_taxa)


global_pairs_unique <-
  nrow(global_range_long) ==
  nrow(
    unique(
      global_range_long[
        ,
        c(
          "star_evidence_taxon_id",
          "wcvp_location_id"
        ),
        drop = FALSE
      ]
    )
  )


location_reference_complete <-
  safe_distinct_count(
    global_range_long$wcvp_location_id
  ) ==
  safe_distinct_count(
    location_reference$wcvp_location_id
  )


location_reference_unambiguous <-
  nrow(
    ambiguous_location_mappings
  ) == 0L


sufficient_for_05c <-
  backbone_cardinality_preserved &&
  global_pairs_unique &&
  location_reference_complete &&
  location_reference_unambiguous


validation_gate <- data.frame(
  
  criterion = c(
    "published_backbone_cardinality_preserved",
    "global_taxon_location_pairs_unique",
    "global_location_reference_complete",
    "global_location_reference_unambiguous",
    "sufficient_for_05c"
  ),
  
  passed = c(
    backbone_cardinality_preserved,
    global_pairs_unique,
    location_reference_complete,
    location_reference_unambiguous,
    sufficient_for_05c
  ),
  
  stringsAsFactors = FALSE
)


write_audit(
  validation_gate,
  "geography_05b_validation_gate.csv"
)


# ------------------------------------------------------------------------------
# 27. RUN METADATA
# ------------------------------------------------------------------------------

run_metadata <- data.frame(
  
  item = c(
    "script",
    "run_time",
    "published_taxa",
    "wcvp_distribution_detail",
    "japan_positive_range",
    "published_vpjd_modified",
    "raw_wcvp_modified",
    "occurrence_data_modified",
    "historical_star_assignments_used",
    "star_categories_assigned"
  ),
  
  value = c(
    "geography_05b_build_global_taxon_range.R",
    
    format(
      Sys.time(),
      "%Y-%m-%d %H:%M:%S %Z"
    ),
    
    PUBLISHED_TAXA_FILE,
    WCVP_DETAIL_FILE,
    JAPAN_RANGE_FILE,
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
  "geography_05b_run_metadata.csv"
)


capture.output(
  sessionInfo(),
  file = file.path(
    AUDIT_ROOT,
    "geography_05b_sessionInfo.txt"
  )
)


# ------------------------------------------------------------------------------
# 28. CONSOLE SUMMARY
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" VPJD GEOGRAPHY 05b - GLOBAL TAXON RANGE COMPLETE\n")
cat("============================================================\n\n")


cat("Global range validation:\n\n")

print(
  global_long_validation,
  row.names = FALSE
)


cat("\nIntegrated range validation:\n\n")

print(
  integrated_validation,
  row.names = FALSE
)


cat("\nWCVP location-reference validation:\n\n")

print(
  location_reference_validation,
  row.names = FALSE
)


cat("\nCombined evidence status:\n\n")

print(
  combined_status_summary,
  row.names = FALSE
)


cat("\nJapan evidence concordance:\n\n")

print(
  japan_concordance_summary,
  row.names = FALSE
)


cat("\nValidation gate:\n\n")

print(
  validation_gate,
  row.names = FALSE
)


cat(
  "\nMost frequently represented global WCVP/TDWG units:\n\n"
)


location_frequency <- aggregate(
  star_evidence_taxon_id ~
    wcvp_location_id +
    wcvp_locality,
  data = global_range_long,
  FUN = function(x) {
    length(
      unique(x)
    )
  }
)


names(location_frequency)[3] <-
  "n_vpjd_taxa"


location_frequency <- location_frequency[
  order(
    -location_frequency$n_vpjd_taxa
  ),
  ,
  drop = FALSE
]


print(
  head(
    location_frequency,
    30L
  ),
  row.names = FALSE
)


cat("\nOutputs:\n\n")

print(
  output_manifest,
  row.names = FALSE
)


cat(
  "\nIntegrated authoritative range table:\n"
)

cat(
  INTEGRATED_RANGE_FILE,
  "\n\n"
)


cat(
  "Global taxon x WCVP/TDWG range:\n"
)

cat(
  GLOBAL_LONG_FILE,
  "\n\n"
)


cat(
  "WCVP/TDWG location reference:\n"
)

cat(
  LOCATION_REFERENCE_FILE,
  "\n\n"
)


cat(
  "Audit outputs:\n"
)

cat(
  AUDIT_ROOT,
  "\n\n"
)


cat("Published VPJD v1.0.0 was NOT modified.\n")
cat("Raw WCVP data were NOT modified.\n")
cat("Occurrence data were NOT modified.\n")
cat("Historical Star assignments were NOT used.\n")
cat("NO Star categories were assigned.\n")


cat("\nINTERPRETATION:\n")


if (sufficient_for_05c) {
  
  cat(
    paste0(
      "The validated WCVP taxon x TDWG distribution evidence has been ",
      "converted into an authoritative positive global-range model and ",
      "integrated with the published VPJD taxonomic backbone and positive ",
      "Japanese occurrence-supported range evidence. WCVP provides the ",
      "global geographic context; VPJD occurrences retain the finer ",
      "within-Japan geography required by Nakamura's Key. Introduced ",
      "distribution evidence has been retained explicitly and has not been ",
      "silently treated as native distribution. No Star classifications ",
      "have been made.\n"
    )
  )
  
} else {
  
  cat(
    paste0(
      "The global-range model has been constructed, but one or more ",
      "validation criteria failed. Review geography_05b_validation_gate.csv ",
      "and the associated location-reference and cardinality audits before ",
      "deriving Nakamura Key variables.\n"
    )
  )
}


cat("\nNEXT STEP:\n")


if (sufficient_for_05c) {
  
  cat(
    paste0(
      "Proceed to geography_05c_derive_nakamura_variables.R. ",
      "That stage should convert the integrated range evidence into the ",
      "specific geographic predicates required by the already-encoded ",
      "Nakamura Key: Japan endemicity, Japanese district/prefecture range, ",
      "small-island restriction, and the Taiwan, Korea, Kuriles/Sakhalin ",
      "and China external-range conditions. It should expose every derived ",
      "predicate and its evidence provenance before any Star category is ",
      "assigned.\n"
    )
  )
  
} else {
  
  cat(
    paste0(
      "Do NOT proceed to 05c until the 05b validation gate passes.\n"
    )
  )
}


cat("\n============================================================\n")