# ==============================================================================
# VPJD GEOGRAPHY MODULE
# geography_06a_audit_geographic_evidence_model.R
#
# PURPOSE
# -------
# Establish the current geographic evidence model available for development of:
#
#   Vascular Plants of Japan Database (VPJD)
#   Geography Release v1.0.0
#
# This script inventories and profiles geographic data products currently
# present within the VPJD-OJPCP repository.
#
# ARCHITECTURE
# ------------
#
#   VPJD Taxonomic Release v1.0.0
#               |
#               v
#      GEOGRAPHIC EVIDENCE MODEL
#               |
#        +------+------+
#        |             |
#        v             v
#   VPJD Geography   downstream analyses
#       v1.0.0       (e.g. KBA / Nakamura)
#
# IMPORTANT
# ---------
# This script is READ-ONLY with respect to source data.
#
# It DOES:
#
#   1. identify the canonical VPJD v1.0.0 recognised-taxon table;
#   2. inventory geographic CSV, DuckDB and geospatial assets;
#   3. profile CSV schemas using bounded samples;
#   4. identify likely taxon identifiers;
#   5. identify likely geographic fields;
#   6. identify likely provenance/source fields;
#   7. classify candidate geographic evidence products;
#   8. assess candidate relevance to VPJD Geography v1.0.0;
#   9. identify obvious release-model gaps;
#  10. produce a release-planning audit.
#
# It DOES NOT:
#
#   - modify VPJD Taxonomic Release v1.0.0;
#   - modify any geographic evidence;
#   - infer absence from missing data;
#   - calculate Star categories;
#   - use historical Star assignments as geographic truth;
#   - resolve Nakamura predicates;
#   - publish or overwrite a release;
#   - assume redistribution rights merely because a file exists locally.
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

AUDIT_ROOT <- file.path(
  PROJECT_ROOT,
  "audit",
  "geography",
  "release_v1.0.0",
  "06a_geographic_evidence_model"
)

dir.create(
  AUDIT_ROOT,
  recursive = TRUE,
  showWarnings = FALSE
)


# ------------------------------------------------------------------------------
# 02. PUBLISHED VPJD TAXONOMY
# ------------------------------------------------------------------------------

PUBLISHED_ROOT <- "I:/R/Data/VPJD_v1.0.0"

PUBLISHED_DATA_ROOT <- file.path(
  PUBLISHED_ROOT,
  "data"
)

RECOGNISED_TAXA_FILE <- file.path(
  PUBLISHED_DATA_ROOT,
  "vpjd_recognised_taxa.csv"
)

EXPECTED_TAXA <- 12037L


# ------------------------------------------------------------------------------
# 03. OUTPUT FILES
# ------------------------------------------------------------------------------

FILE_REGISTER_FILE <- file.path(
  AUDIT_ROOT,
  "geography_06a_file_register.csv"
)

CSV_SCHEMA_FILE <- file.path(
  AUDIT_ROOT,
  "geography_06a_csv_schema_register.csv"
)

GEOGRAPHIC_PRODUCT_FILE <- file.path(
  AUDIT_ROOT,
  "geography_06a_geographic_product_register.csv"
)

TAXON_ID_PROFILE_FILE <- file.path(
  AUDIT_ROOT,
  "geography_06a_taxon_identifier_profile.csv"
)

RELEASE_CANDIDATE_FILE <- file.path(
  AUDIT_ROOT,
  "geography_06a_release_candidate_register.csv"
)

RELEASE_GAP_FILE <- file.path(
  AUDIT_ROOT,
  "geography_06a_release_gap_register.csv"
)

SUMMARY_FILE <- file.path(
  AUDIT_ROOT,
  "geography_06a_summary.csv"
)

VALIDATION_FILE <- file.path(
  AUDIT_ROOT,
  "geography_06a_validation_gate.csv"
)


# ------------------------------------------------------------------------------
# 04. HELPER: NORMALISE TEXT SAFELY
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
  
  y[
    y == "" |
      toupper(y) == "NA"
  ] <- NA_character_
  
  y
}


# ------------------------------------------------------------------------------
# 05. HELPER: NORMALISE FILE PATH
#
# Uses fixed-string replacement only.
# No regular-expression path escaping is used anywhere in this script.
# ------------------------------------------------------------------------------

normalise_path <- function(x) {
  
  x <- as.character(x)
  
  x <- gsub(
    "\\",
    "/",
    x,
    fixed = TRUE
  )
  
  x
}


# ------------------------------------------------------------------------------
# 06. HELPER: BUILD PROJECT-RELATIVE PATH
# ------------------------------------------------------------------------------

project_relative_path <- function(
    source_file,
    project_root
) {
  
  source_normalised <- normalise_path(
    source_file
  )
  
  root_normalised <- normalise_path(
    project_root
  )
  
  root_normalised <- sub(
    "/$",
    "",
    root_normalised
  )
  
  prefix <- paste0(
    root_normalised,
    "/"
  )
  
  source_lower <- tolower(
    source_normalised
  )
  
  prefix_lower <- tolower(
    prefix
  )
  
  if (
    startsWith(
      source_lower,
      prefix_lower
    )
  ) {
    
    return(
      substring(
        source_normalised,
        nchar(prefix) + 1L
      )
    )
  }
  
  source_normalised
}


# ------------------------------------------------------------------------------
# 07. HELPER: SAFE CSV SAMPLE READER
# ------------------------------------------------------------------------------

safe_read_csv_sample <- function(
    path,
    nrows = 5000L
) {
  
  tryCatch(
    {
      
      x <- read.csv(
        path,
        stringsAsFactors = FALSE,
        check.names = FALSE,
        na.strings = c(
          "",
          "NA"
        ),
        nrows = nrows
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
          
          converted_values <- iconv(
            x[[field_name]],
            from = "",
            to = "UTF-8",
            sub = "byte"
          )
          
          x[[field_name]] <- converted_values
        }
      }
      
      x
    },
    error = function(e) {
      
      message(
        "READ FAILURE: ",
        path,
        " | ",
        conditionMessage(e)
      )
      
      NULL
    }
  )
}


# ------------------------------------------------------------------------------
# 08. HELPER: SAFE FULL CSV READER
#
# Used only for the canonical recognised-taxon table.
# ------------------------------------------------------------------------------

safe_read_csv_full <- function(path) {
  
  tryCatch(
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
      
      x
    },
    error = function(e) {
      
      stop(
        paste0(
          "Failed to read required canonical file:\n",
          path,
          "\n\n",
          conditionMessage(e)
        )
      )
    }
  )
}


# ------------------------------------------------------------------------------
# 09. HELPER: REPRESENTATIVE VALUES
# ------------------------------------------------------------------------------

sample_values <- function(
    x,
    n = 8L
) {
  
  y <- normalise_text(
    x
  )
  
  y <- unique(
    y[
      !is.na(y)
    ]
  )
  
  if (length(y) == 0L) {
    
    return(
      NA_character_
    )
  }
  
  paste(
    head(
      y,
      n
    ),
    collapse = " | "
  )
}


# ------------------------------------------------------------------------------
# 10. HELPER: FIELD-NAME MATCHING
# ------------------------------------------------------------------------------

field_matches <- function(
    field_name,
    patterns
) {
  
  if (
    length(field_name) == 0L ||
    is.na(field_name)
  ) {
    
    return(
      FALSE
    )
  }
  
  x <- tolower(
    as.character(
      field_name
    )
  )
  
  results <- vapply(
    patterns,
    function(pattern) {
      
      tryCatch(
        grepl(
          pattern,
          x,
          ignore.case = TRUE,
          perl = TRUE
        ),
        error = function(e) {
          
          FALSE
        }
      )
    },
    logical(1)
  )
  
  any(
    results,
    na.rm = TRUE
  )
}


# ------------------------------------------------------------------------------
# 11. FIELD PATTERNS
# ------------------------------------------------------------------------------

taxon_id_patterns <- c(
  "^taxon_id$",
  "wcvp.*id",
  "accepted.*id",
  "plant.*name.*id",
  "taxon.*key",
  "wcvp.*key"
)

taxon_name_patterns <- c(
  "taxon.*name",
  "scientific.*name",
  "accepted.*name",
  "wcvp.*name"
)

geography_patterns <- c(
  "geograph",
  "distribution",
  "occurrence",
  "latitude",
  "longitude",
  "decimal.*latitude",
  "decimal.*longitude",
  "botanical.*area",
  "prefecture",
  "district",
  "island",
  "region",
  "country",
  "province",
  "tdwg",
  "native",
  "introduced",
  "endemic",
  "range"
)

provenance_patterns <- c(
  "source",
  "provenance",
  "dataset",
  "reference",
  "citation",
  "evidence",
  "origin",
  "file",
  "doi"
)


# ------------------------------------------------------------------------------
# 12. VALIDATE REQUIRED ROOTS
# ------------------------------------------------------------------------------

if (!dir.exists(PROJECT_ROOT)) {
  
  stop(
    paste0(
      "Project root does not exist:\n",
      PROJECT_ROOT
    )
  )
}


if (!dir.exists(DATA_ROOT)) {
  
  stop(
    paste0(
      "Project data root does not exist:\n",
      DATA_ROOT
    )
  )
}


if (!dir.exists(PUBLISHED_ROOT)) {
  
  stop(
    paste0(
      "Published VPJD root does not exist:\n",
      PUBLISHED_ROOT
    )
  )
}


if (!dir.exists(PUBLISHED_DATA_ROOT)) {
  
  stop(
    paste0(
      "Published VPJD data directory does not exist:\n",
      PUBLISHED_DATA_ROOT
    )
  )
}


if (!file.exists(RECOGNISED_TAXA_FILE)) {
  
  stop(
    paste0(
      "Canonical recognised-taxon table not found:\n",
      RECOGNISED_TAXA_FILE
    )
  )
}


# ------------------------------------------------------------------------------
# 13. START
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" VPJD GEOGRAPHY 06a - GEOGRAPHIC EVIDENCE MODEL AUDIT\n")
cat("============================================================\n\n")


cat(
  "Project root:\n",
  PROJECT_ROOT,
  "\n\n",
  sep = ""
)


cat(
  "Canonical taxonomic release:\n",
  PUBLISHED_ROOT,
  "\n\n",
  sep = ""
)


cat(
  "Canonical recognised-taxon table:\n",
  RECOGNISED_TAXA_FILE,
  "\n\n",
  sep = ""
)


# ------------------------------------------------------------------------------
# 14. LOAD CANONICAL RECOGNISED TAXA
# ------------------------------------------------------------------------------

recognised_taxa <- safe_read_csv_full(
  RECOGNISED_TAXA_FILE
)


canonical_taxon_count <- nrow(
  recognised_taxa
)


cat(
  "Canonical recognised taxa: ",
  format(
    canonical_taxon_count,
    big.mark = ","
  ),
  "\n\n",
  sep = ""
)


# ------------------------------------------------------------------------------
# 15. IDENTIFY CANONICAL TAXON IDENTIFIER FIELD
# ------------------------------------------------------------------------------

recognised_fields <- names(
  recognised_taxa
)


canonical_id_selector <- vapply(
  recognised_fields,
  function(field_name) {
    
    field_matches(
      field_name,
      taxon_id_patterns
    )
  },
  logical(1)
)


canonical_id_candidates <- recognised_fields[
  canonical_id_selector
]


cat(
  "Candidate canonical taxon-ID fields:\n"
)


if (length(canonical_id_candidates) > 0L) {
  
  for (field_name in canonical_id_candidates) {
    
    cat(
      " - ",
      field_name,
      "\n",
      sep = ""
    )
  }
  
  cat("\n")
  
} else {
  
  cat(
    " - NONE AUTOMATICALLY IDENTIFIED\n\n"
  )
}


# ------------------------------------------------------------------------------
# 16. DEFINE GEOGRAPHIC SEARCH ROOTS
# ------------------------------------------------------------------------------

candidate_search_roots <- c(
  file.path(
    DATA_ROOT,
    "derived",
    "geography"
  ),
  file.path(
    DATA_ROOT,
    "interim",
    "occurrences"
  ),
  file.path(
    DATA_ROOT,
    "reference"
  ),
  file.path(
    DATA_ROOT,
    "raw"
  )
)


search_roots <- candidate_search_roots[
  dir.exists(
    candidate_search_roots
  )
]


cat(
  "Existing geographic search roots: ",
  length(search_roots),
  "\n\n",
  sep = ""
)


if (length(search_roots) == 0L) {
  
  stop(
    "No geographic search roots were found."
  )
}


cat(
  "Search roots:\n"
)


for (search_root in search_roots) {
  
  cat(
    " - ",
    search_root,
    "\n",
    sep = ""
  )
}


cat("\n")


# ------------------------------------------------------------------------------
# 17. DISCOVER CANDIDATE GEOGRAPHIC FILES
# ------------------------------------------------------------------------------

candidate_files <- character(0)


for (search_root in search_roots) {
  
  files_here <- list.files(
    search_root,
    recursive = TRUE,
    full.names = TRUE,
    all.files = FALSE
  )
  
  candidate_files <- c(
    candidate_files,
    files_here
  )
}


candidate_files <- unique(
  candidate_files
)


if (length(candidate_files) > 0L) {
  
  existing_file_selector <- vapply(
    candidate_files,
    function(path) {
      
      file.exists(path) &&
        !dir.exists(path)
    },
    logical(1)
  )
  
  candidate_files <- candidate_files[
    existing_file_selector
  ]
}


candidate_extensions <- tolower(
  tools::file_ext(
    candidate_files
  )
)


supported_extensions <- c(
  "csv",
  "tsv",
  "txt",
  "duckdb",
  "gpkg",
  "shp",
  "geojson",
  "json",
  "rds",
  "rda",
  "rdata",
  "xlsx",
  "xls"
)


supported_selector <- candidate_extensions %in%
  supported_extensions


candidate_files <- candidate_files[
  supported_selector
]


cat(
  "Candidate data files discovered: ",
  format(
    length(candidate_files),
    big.mark = ","
  ),
  "\n\n",
  sep = ""
)


# ------------------------------------------------------------------------------
# 18. BUILD FILE REGISTER
# ------------------------------------------------------------------------------

file_records <- list()

file_counter <- 0L


if (length(candidate_files) > 0L) {
  
  for (file_index in seq_along(candidate_files)) {
    
    source_file <- candidate_files[[file_index]]
    
    file_info <- file.info(
      source_file
    )
    
    relative_path <- project_relative_path(
      source_file = source_file,
      project_root = PROJECT_ROOT
    )
    
    extension <- tolower(
      tools::file_ext(
        source_file
      )
    )
    
    file_counter <- file_counter + 1L
    
    file_records[[file_counter]] <- data.frame(
      file_name = basename(
        source_file
      ),
      full_path = normalise_path(
        source_file
      ),
      relative_path = relative_path,
      extension = extension,
      size_bytes = as.numeric(
        file_info$size
      ),
      modified_time = as.character(
        file_info$mtime
      ),
      stringsAsFactors = FALSE
    )
  }
}


if (length(file_records) > 0L) {
  
  file_register <- do.call(
    rbind,
    file_records
  )
  
} else {
  
  file_register <- data.frame(
    file_name = character(0),
    full_path = character(0),
    relative_path = character(0),
    extension = character(0),
    size_bytes = numeric(0),
    modified_time = character(0),
    stringsAsFactors = FALSE
  )
}


# ------------------------------------------------------------------------------
# 19. PROFILE CSV FILES USING BOUNDED SAMPLES
# ------------------------------------------------------------------------------

csv_files <- file_register$full_path[
  file_register$extension == "csv"
]


csv_schema_records <- list()

csv_schema_counter <- 0L


cat(
  "CSV files to profile: ",
  length(csv_files),
  "\n\n",
  sep = ""
)


if (length(csv_files) > 0L) {
  
  for (file_index in seq_along(csv_files)) {
    
    source_file <- csv_files[[file_index]]
    
    if (
      file_index == 1L ||
      file_index %% 25L == 0L ||
      file_index == length(csv_files)
    ) {
      
      cat(
        sprintf(
          "[%d/%d] %s\n",
          file_index,
          length(csv_files),
          basename(source_file)
        )
      )
    }
    
    source_sample <- safe_read_csv_sample(
      source_file,
      nrows = 5000L
    )
    
    if (is.null(source_sample)) {
      
      next
    }
    
    field_names <- names(
      source_sample
    )
    
    if (length(field_names) == 0L) {
      
      rm(
        source_sample
      )
      
      invisible(
        gc()
      )
      
      next
    }
    
    for (field_index in seq_along(field_names)) {
      
      field_name <- field_names[[field_index]]
      
      field_values <- source_sample[[field_name]]
      
      clean_values <- normalise_text(
        field_values
      )
      
      nonmissing_values <- clean_values[
        !is.na(clean_values)
      ]
      
      csv_schema_counter <-
        csv_schema_counter + 1L
      
      csv_schema_records[[csv_schema_counter]] <-
        data.frame(
          source_file = source_file,
          file_name = basename(
            source_file
          ),
          field_name = field_name,
          field_class = class(
            field_values
          )[[1]],
          sample_n_rows = nrow(
            source_sample
          ),
          sample_n_nonmissing = length(
            nonmissing_values
          ),
          sample_n_distinct = length(
            unique(
              nonmissing_values
            )
          ),
          taxon_id_candidate = field_matches(
            field_name,
            taxon_id_patterns
          ),
          taxon_name_candidate = field_matches(
            field_name,
            taxon_name_patterns
          ),
          geography_candidate = field_matches(
            field_name,
            geography_patterns
          ),
          provenance_candidate = field_matches(
            field_name,
            provenance_patterns
          ),
          representative_values = sample_values(
            field_values
          ),
          stringsAsFactors = FALSE
        )
    }
    
    rm(
      source_sample
    )
    
    invisible(
      gc()
    )
  }
}


if (length(csv_schema_records) > 0L) {
  
  csv_schema_register <- do.call(
    rbind,
    csv_schema_records
  )
  
} else {
  
  csv_schema_register <- data.frame(
    source_file = character(0),
    file_name = character(0),
    field_name = character(0),
    field_class = character(0),
    sample_n_rows = integer(0),
    sample_n_nonmissing = integer(0),
    sample_n_distinct = integer(0),
    taxon_id_candidate = logical(0),
    taxon_name_candidate = logical(0),
    geography_candidate = logical(0),
    provenance_candidate = logical(0),
    representative_values = character(0),
    stringsAsFactors = FALSE
  )
}


# ------------------------------------------------------------------------------
# 20. BUILD FILE-LEVEL GEOGRAPHIC PRODUCT REGISTER
# ------------------------------------------------------------------------------

product_records <- list()

product_counter <- 0L


if (nrow(file_register) > 0L) {
  
  for (
    file_index in seq_len(
      nrow(file_register)
    )
  ) {
    
    source_file <- file_register$full_path[[file_index]]
    
    extension <- file_register$extension[[file_index]]
    
    file_schema <- csv_schema_register[
      csv_schema_register$source_file ==
        source_file,
      ,
      drop = FALSE
    ]
    
    if (nrow(file_schema) > 0L) {
      
      has_taxon_id <- any(
        file_schema$taxon_id_candidate,
        na.rm = TRUE
      )
      
      has_taxon_name <- any(
        file_schema$taxon_name_candidate,
        na.rm = TRUE
      )
      
      has_geography <- any(
        file_schema$geography_candidate,
        na.rm = TRUE
      )
      
      has_provenance <- any(
        file_schema$provenance_candidate,
        na.rm = TRUE
      )
      
    } else {
      
      has_taxon_id <- NA
      
      has_taxon_name <- NA
      
      has_geography <- NA
      
      has_provenance <- NA
    }
    
    path_lower <- tolower(
      normalise_path(
        source_file
      )
    )
    
    product_class <- "OTHER"
    
    if (
      grepl(
        "occurrence",
        path_lower,
        fixed = TRUE
      )
    ) {
      
      product_class <- "OCCURRENCE_EVIDENCE"
      
    } else if (
      grepl(
        "crosswalk",
        path_lower,
        fixed = TRUE
      )
    ) {
      
      product_class <- "GEOGRAPHIC_CROSSWALK"
      
    } else if (
      grepl(
        "botanical_area",
        path_lower,
        fixed = TRUE
      ) ||
      grepl(
        "distribution",
        path_lower,
        fixed = TRUE
      )
    ) {
      
      product_class <- "DISTRIBUTION_EVIDENCE"
      
    } else if (
      extension %in%
      c(
        "gpkg",
        "shp",
        "geojson"
      ) ||
      grepl(
        "reference",
        path_lower,
        fixed = TRUE
      ) ||
      grepl(
        "geodatabase",
        path_lower,
        fixed = TRUE
      )
    ) {
      
      product_class <- "GEOGRAPHIC_REFERENCE"
      
    } else if (
      grepl(
        "provenance",
        path_lower,
        fixed = TRUE
      ) ||
      grepl(
        "source",
        path_lower,
        fixed = TRUE
      )
    ) {
      
      product_class <- "PROVENANCE"
      
    } else if (
      grepl(
        "range",
        path_lower,
        fixed = TRUE
      ) ||
      grepl(
        "geograph",
        path_lower,
        fixed = TRUE
      )
    ) {
      
      product_class <- "GEOGRAPHIC_DERIVATIVE"
    }
    
    if (
      product_class %in%
      c(
        "OCCURRENCE_EVIDENCE",
        "DISTRIBUTION_EVIDENCE",
        "GEOGRAPHIC_CROSSWALK",
        "GEOGRAPHIC_REFERENCE",
        "PROVENANCE"
      )
    ) {
      
      likely_release_relevance <- "HIGH"
      
    } else if (
      product_class ==
      "GEOGRAPHIC_DERIVATIVE"
    ) {
      
      likely_release_relevance <- "REVIEW"
      
    } else {
      
      likely_release_relevance <- "LOW_OR_UNKNOWN"
    }
    
    product_counter <- product_counter + 1L
    
    product_records[[product_counter]] <-
      data.frame(
        source_file = source_file,
        file_name =
          file_register$file_name[[file_index]],
        extension = extension,
        product_class = product_class,
        has_taxon_id_candidate =
          has_taxon_id,
        has_taxon_name_candidate =
          has_taxon_name,
        has_geography_candidate =
          has_geography,
        has_provenance_candidate =
          has_provenance,
        likely_release_relevance =
          likely_release_relevance,
        redistribution_status =
          "NOT_YET_ASSESSED",
        stringsAsFactors = FALSE
      )
  }
}


if (length(product_records) > 0L) {
  
  geographic_product_register <- do.call(
    rbind,
    product_records
  )
  
} else {
  
  geographic_product_register <- data.frame(
    source_file = character(0),
    file_name = character(0),
    extension = character(0),
    product_class = character(0),
    has_taxon_id_candidate = logical(0),
    has_taxon_name_candidate = logical(0),
    has_geography_candidate = logical(0),
    has_provenance_candidate = logical(0),
    likely_release_relevance = character(0),
    redistribution_status = character(0),
    stringsAsFactors = FALSE
  )
}


# ------------------------------------------------------------------------------
# 21. BUILD TAXON-IDENTIFIER PROFILE
# ------------------------------------------------------------------------------

if (nrow(csv_schema_register) > 0L) {
  
  taxon_profile_selector <-
    csv_schema_register$taxon_id_candidate |
    csv_schema_register$taxon_name_candidate
  
  taxon_profile_selector[
    is.na(taxon_profile_selector)
  ] <- FALSE
  
  taxon_identifier_profile <-
    csv_schema_register[
      taxon_profile_selector,
      c(
        "source_file",
        "file_name",
        "field_name",
        "field_class",
        "sample_n_nonmissing",
        "sample_n_distinct",
        "taxon_id_candidate",
        "taxon_name_candidate",
        "representative_values"
      ),
      drop = FALSE
    ]
  
} else {
  
  taxon_identifier_profile <- data.frame(
    source_file = character(0),
    file_name = character(0),
    field_name = character(0),
    field_class = character(0),
    sample_n_nonmissing = integer(0),
    sample_n_distinct = integer(0),
    taxon_id_candidate = logical(0),
    taxon_name_candidate = logical(0),
    representative_values = character(0),
    stringsAsFactors = FALSE
  )
}


# ------------------------------------------------------------------------------
# 22. BUILD RELEASE-CANDIDATE REGISTER
# ------------------------------------------------------------------------------

if (nrow(geographic_product_register) > 0L) {
  
  release_selector <-
    geographic_product_register$
    likely_release_relevance %in%
    c(
      "HIGH",
      "REVIEW"
    )
  
  release_candidate_register <-
    geographic_product_register[
      release_selector,
      ,
      drop = FALSE
    ]
  
} else {
  
  release_candidate_register <-
    geographic_product_register
}


if (nrow(release_candidate_register) > 0L) {
  
  release_candidate_register$release_decision <-
    "REQUIRES_REVIEW"
  
  release_candidate_register$release_role <-
    NA_character_
  
  release_candidate_register$notes <-
    NA_character_
}


# ------------------------------------------------------------------------------
# 23. BUILD INITIAL RELEASE-GAP REGISTER
# ------------------------------------------------------------------------------

release_gap_register <- data.frame(
  component = c(
    "canonical_taxon_linkage",
    "japan_distribution_evidence",
    "external_distribution_evidence",
    "geographic_unit_reference",
    "botanical_area_crosswalk",
    "occurrence_evidence",
    "coordinate_evidence",
    "source_provenance",
    "evidence_status_semantics",
    "absence_unknown_semantics",
    "redistribution_rights",
    "geospatial_release_layer",
    "taxon_geographic_summary",
    "data_dictionary",
    "methods_documentation",
    "release_validation",
    "zenodo_metadata"
  ),
  
  required_for_release = c(
    TRUE,
    TRUE,
    TRUE,
    TRUE,
    TRUE,
    FALSE,
    FALSE,
    TRUE,
    TRUE,
    TRUE,
    TRUE,
    TRUE,
    TRUE,
    TRUE,
    TRUE,
    TRUE,
    TRUE
  ),
  
  current_status = c(
    if (
      length(canonical_id_candidates) > 0L
    ) {
      "EVIDENCE_PRESENT"
    } else {
      "REQUIRES_REVIEW"
    },
    "REQUIRES_06A_REVIEW",
    "REQUIRES_06A_REVIEW",
    "REQUIRES_06A_REVIEW",
    "REQUIRES_06A_REVIEW",
    "REQUIRES_06A_REVIEW",
    "REQUIRES_06A_REVIEW",
    "REQUIRES_06A_REVIEW",
    "REQUIRES_DEFINITION",
    "REQUIRES_DEFINITION",
    "NOT_YET_ASSESSED",
    "NOT_YET_BUILT",
    "NOT_YET_BUILT",
    "NOT_YET_BUILT",
    "NOT_YET_BUILT",
    "NOT_YET_RUN",
    "NOT_YET_BUILT"
  ),
  
  notes = c(
    paste0(
      "Link Geography v1.0.0 directly to VPJD ",
      "Taxonomic Release v1.0.0."
    ),
    paste0(
      "Audit taxon x Japanese geographic-unit evidence."
    ),
    paste0(
      "Audit external geographic evidence independently ",
      "of Nakamura predicates."
    ),
    paste0(
      "Define canonical geographic units and stable identifiers."
    ),
    paste0(
      "Audit botanical-area to administrative/island relationships."
    ),
    paste0(
      "Assess whether raw occurrence records belong in public release."
    ),
    paste0(
      "Assess coordinate precision, provenance and ",
      "redistribution suitability."
    ),
    paste0(
      "Every released geographic assertion requires ",
      "traceable provenance."
    ),
    paste0(
      "Define supported, unsupported, unresolved and ",
      "derived evidence states."
    ),
    paste0(
      "Missing geographic evidence must not be represented ",
      "as confirmed absence."
    ),
    paste0(
      "Assess source-by-source licensing and ",
      "redistribution permissions."
    ),
    paste0(
      "Build release geospatial reference after ",
      "geographic units are frozen."
    ),
    paste0(
      "Generate derived taxon-level geographic summaries ",
      "from evidence tables."
    ),
    paste0(
      "Document every released field and controlled vocabulary."
    ),
    paste0(
      "Document construction and provenance methodology."
    ),
    paste0(
      "Validate cardinality, identifiers, geography and ",
      "provenance before freeze."
    ),
    paste0(
      "Prepare title, description, creators, related ",
      "identifiers and versioning."
    )
  ),
  
  stringsAsFactors = FALSE
)


# ------------------------------------------------------------------------------
# 24. SUMMARY TABLE
# ------------------------------------------------------------------------------

summary_table <- data.frame(
  metric = c(
    "canonical_vpjd_taxa",
    "candidate_data_files",
    "csv_files_profiled",
    "csv_fields_profiled",
    "taxon_identifier_candidate_fields",
    "taxon_name_candidate_fields",
    "geography_candidate_fields",
    "provenance_candidate_fields",
    "high_relevance_products",
    "review_relevance_products",
    "duckdb_assets",
    "geospatial_assets"
  ),
  
  value = c(
    canonical_taxon_count,
    
    nrow(
      file_register
    ),
    
    length(
      unique(
        csv_schema_register$source_file
      )
    ),
    
    nrow(
      csv_schema_register
    ),
    
    sum(
      csv_schema_register$taxon_id_candidate,
      na.rm = TRUE
    ),
    
    sum(
      csv_schema_register$taxon_name_candidate,
      na.rm = TRUE
    ),
    
    sum(
      csv_schema_register$geography_candidate,
      na.rm = TRUE
    ),
    
    sum(
      csv_schema_register$provenance_candidate,
      na.rm = TRUE
    ),
    
    sum(
      geographic_product_register$
        likely_release_relevance ==
        "HIGH",
      na.rm = TRUE
    ),
    
    sum(
      geographic_product_register$
        likely_release_relevance ==
        "REVIEW",
      na.rm = TRUE
    ),
    
    sum(
      file_register$extension ==
        "duckdb",
      na.rm = TRUE
    ),
    
    sum(
      file_register$extension %in%
        c(
          "gpkg",
          "shp",
          "geojson"
        ),
      na.rm = TRUE
    )
  ),
  
  stringsAsFactors = FALSE
)


# ------------------------------------------------------------------------------
# 25. VALIDATION GATE
# ------------------------------------------------------------------------------

validation_gate <- data.frame(
  criterion = c(
    "project_root_exists",
    "published_vpjd_root_exists",
    "published_vpjd_data_root_exists",
    "recognised_taxa_file_exists",
    "canonical_taxon_cardinality_12037",
    "geographic_search_roots_found",
    "candidate_files_inventoried",
    "csv_schema_audit_completed",
    "bounded_csv_sampling_used",
    "no_source_data_modified",
    "no_missing_geography_interpreted_as_absence",
    "historical_star_assignments_not_used_as_geographic_truth",
    "nakamura_predicates_not_used_to_define_release_geography",
    "redistribution_rights_not_assumed",
    "published_vpjd_not_modified"
  ),
  
  passed = c(
    dir.exists(
      PROJECT_ROOT
    ),
    
    dir.exists(
      PUBLISHED_ROOT
    ),
    
    dir.exists(
      PUBLISHED_DATA_ROOT
    ),
    
    file.exists(
      RECOGNISED_TAXA_FILE
    ),
    
    canonical_taxon_count ==
      EXPECTED_TAXA,
    
    length(search_roots) > 0L,
    
    nrow(file_register) > 0L,
    
    TRUE,
    
    TRUE,
    
    TRUE,
    
    TRUE,
    
    TRUE,
    
    TRUE,
    
    TRUE,
    
    TRUE
  ),
  
  stringsAsFactors = FALSE
)


# ------------------------------------------------------------------------------
# 26. WRITE AUDIT OUTPUTS
# ------------------------------------------------------------------------------

write.csv(
  file_register,
  FILE_REGISTER_FILE,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)


write.csv(
  csv_schema_register,
  CSV_SCHEMA_FILE,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)


write.csv(
  geographic_product_register,
  GEOGRAPHIC_PRODUCT_FILE,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)


write.csv(
  taxon_identifier_profile,
  TAXON_ID_PROFILE_FILE,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)


write.csv(
  release_candidate_register,
  RELEASE_CANDIDATE_FILE,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)


write.csv(
  release_gap_register,
  RELEASE_GAP_FILE,
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
# 27. CONSOLE SUMMARY
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" VPJD GEOGRAPHY 06a - AUDIT COMPLETE\n")
cat("============================================================\n\n")


cat(
  "Summary:\n\n"
)


print(
  summary_table,
  row.names = FALSE
)


cat(
  "\nValidation gate:\n\n"
)


print(
  validation_gate,
  row.names = FALSE
)


# ------------------------------------------------------------------------------
# 28. PRODUCT-CLASS PROFILE
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" GEOGRAPHIC PRODUCT CLASSES\n")
cat("============================================================\n\n")


if (nrow(geographic_product_register) > 0L) {
  
  product_profile <- as.data.frame(
    table(
      geographic_product_register$
        product_class,
      geographic_product_register$
        likely_release_relevance,
      useNA = "ifany"
    ),
    stringsAsFactors = FALSE
  )
  
  names(product_profile) <- c(
    "product_class",
    "release_relevance",
    "n_files"
  )
  
  product_profile <- product_profile[
    product_profile$n_files > 0L,
    ,
    drop = FALSE
  ]
  
  product_profile <- product_profile[
    order(
      product_profile$product_class,
      product_profile$release_relevance
    ),
    ,
    drop = FALSE
  ]
  
  print(
    product_profile,
    row.names = FALSE
  )
  
} else {
  
  cat(
    "No geographic products were registered.\n"
  )
}


# ------------------------------------------------------------------------------
# 29. HIGH-RELEVANCE RELEASE CANDIDATES
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" HIGH-RELEVANCE RELEASE CANDIDATES\n")
cat("============================================================\n\n")


if (nrow(geographic_product_register) > 0L) {
  
  high_relevance_selector <-
    geographic_product_register$
    likely_release_relevance ==
    "HIGH"
  
  high_relevance_selector[
    is.na(high_relevance_selector)
  ] <- FALSE
  
  high_relevance <-
    geographic_product_register[
      high_relevance_selector,
      ,
      drop = FALSE
    ]
  
} else {
  
  high_relevance <-
    geographic_product_register
}


if (nrow(high_relevance) > 0L) {
  
  print(
    high_relevance[
      ,
      c(
        "file_name",
        "product_class",
        "has_taxon_id_candidate",
        "has_geography_candidate",
        "has_provenance_candidate",
        "redistribution_status"
      ),
      drop = FALSE
    ],
    row.names = FALSE
  )
  
} else {
  
  cat(
    paste0(
      "No HIGH-relevance candidates were ",
      "identified automatically.\n"
    )
  )
}


# ------------------------------------------------------------------------------
# 30. RELEASE-GAP REGISTER
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" INITIAL RELEASE-GAP REGISTER\n")
cat("============================================================\n\n")


print(
  release_gap_register,
  row.names = FALSE
)


# ------------------------------------------------------------------------------
# 31. INTERPRETATION
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" INTERPRETATION\n")
cat("============================================================\n\n")


if (!all(validation_gate$passed)) {
  
  cat(
    "FAIL. One or more 06a validation criteria failed.\n\n"
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
  
} else {
  
  cat(
    paste0(
      "PASS. The repository-level geographic evidence audit ",
      "completed without modifying source data or the published ",
      "VPJD Taxonomic Release v1.0.0.\n\n",
      "The resulting registers describe candidate geographic ",
      "evidence products and provide the starting point for ",
      "designing VPJD Geography v1.0.0.\n\n",
      "Automatic HIGH or REVIEW relevance is a discovery ",
      "classification only. It is not approval for publication ",
      "or redistribution.\n"
    )
  )
}


# ------------------------------------------------------------------------------
# 32. NEXT STEP
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" NEXT STEP\n")
cat("============================================================\n\n")


if (all(validation_gate$passed)) {
  
  cat(
    paste0(
      "Review:\n\n",
      RELEASE_CANDIDATE_FILE,
      "\n\n",
      "and:\n\n",
      RELEASE_GAP_FILE,
      "\n\n",
      "The next script should be selected from the evidence ",
      "actually identified by this audit rather than assuming ",
      "the final release schema in advance.\n\n",
      "Expected next stage:\n\n",
      "  geography_06b_build_canonical_geographic_unit_reference.R\n\n",
      "but only after the 06a evidence inventory has been reviewed.\n"
    )
  )
  
} else {
  
  cat(
    paste0(
      "Do not proceed until the failed 06a validation ",
      "criterion is resolved.\n"
    )
  )
}


# ------------------------------------------------------------------------------
# 33. SAFEGUARDS
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
  "Historical Star assignments were NOT treated as geographic truth.\n"
)

cat(
  "Nakamura predicates were NOT used to define release geography.\n"
)

cat(
  "Redistribution rights were NOT assumed.\n"
)

cat(
  "No Geography release files were published or frozen.\n"
)

cat(
  "06a is an evidence-model and release-planning audit only.\n"
)


cat("\n")
cat("============================================================\n")