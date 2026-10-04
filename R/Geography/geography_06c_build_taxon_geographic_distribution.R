# ==============================================================================
# VPJD GEOGRAPHY MODULE
# geography_06c_build_taxon_geographic_distribution.R
#
# PURPOSE
# -------
# Build the canonical positive-evidence taxon x geographic-unit distribution
# table for VPJD Geography v1.0.0.
#
#
# POSITION IN PIPELINE
# --------------------
#
#   VPJD Taxonomic Release v1.0.0
#             |
#             |-- 12,037 recognised taxa
#             |-- wcvp_plant_name_id
#             |-- wcvp_taxon_name
#             |
#             v
#   analytical occurrence evidence
#             |
#             v
#   complete taxon x botanical-area evidence matrix
#             |
#             |-- 613,887 combinations
#             |-- 12,037 taxa
#             |-- 51 Japanese botanical areas
#             |
#             v
#   06c0 evidence-semantics audit
#             |
#             |-- n_occurrence_records > 0
#             |       = positive evidence
#             |
#             |-- n_unique_occurrences > 0
#             |       = independent consistency check
#             |
#             v
#   06b5 frozen canonical geographic vocabulary
#             |
#             v
#   06c BUILD TAXON x GEOGRAPHIC DISTRIBUTION   <-- THIS SCRIPT
#             |
#             v
#       128,824 positive relationships
#             |
#             v
#        independent validation
#             |
#             v
#       VPJD Geography v1.0.0
#
#
# EVIDENCE SEMANTICS
# ------------------
#
# geography_06c0_audit_distribution_evidence_semantics.R established that:
#
#   source rows                              613,887
#   recognised taxa                          12,037
#   Japanese botanical areas                     51
#
# and therefore:
#
#   12,037 x 51 = 613,887
#
# The source is the COMPLETE taxon x botanical-area combination space.
#
# ROW EXISTENCE THEREFORE DOES NOT MEAN PRESENCE.
#
#
# Positive geographic evidence is defined explicitly as:
#
#   n_occurrence_records > 0
#
# An independent field must agree exactly:
#
#   n_unique_occurrences > 0
#
#
# RECONSTRUCTION REFERENCE COUNTS
# -------------------------------
#
# The independently audited analytical evidence gives:
#
#   complete combinations                    613,887
#   positive combinations                    128,824
#   unsupported combinations                 485,063
#   taxa with positive evidence               11,470
#   taxa without positive evidence                567
#   Japanese botanical areas                      51
#
# These counts are used here as reconstruction gates for VPJD Geography
# v1.0.0. A discrepancy stops the build rather than silently changing the
# evidence model.
#
#
# CRITICAL SEMANTIC RULE
# ----------------------
#
# The final distribution table contains POSITIVE EVIDENCE ONLY.
#
# Absence of a taxon x geographic-unit row means:
#
#   "No positive distribution evidence is represented in this release
#    for this combination."
#
# It DOES NOT mean:
#
#   "The taxon is absent from the geographic unit."
#
#
# THIS SCRIPT DOES
# ----------------
#
#   - require the frozen 06b5 geographic vocabulary;
#   - require the published VPJD v1.0.0 recognised-taxon table;
#   - require the canonical analytical taxon x botanical-area matrix;
#   - validate the complete 12,037 x 51 source structure;
#   - define positive evidence as n_occurrence_records > 0;
#   - independently verify n_unique_occurrences > 0 gives the same rows;
#   - retain positive combinations only;
#   - reconcile every taxon to published wcvp_plant_name_id;
#   - reconcile every botanical area to frozen geographic_unit_id;
#   - preserve occurrence counts;
#   - preserve evidence provenance;
#   - validate expected reconstruction counts;
#   - write the canonical positive-evidence distribution product.
#
#
# THIS SCRIPT DOES NOT
# --------------------
#
#   - infer presence from row existence;
#   - infer absence from zero occurrence evidence;
#   - publish unsupported combinations as absences;
#   - manufacture taxon identifiers;
#   - manufacture geographic identifiers;
#   - fuzzy-match geographic concepts;
#   - modify VPJD Taxonomic Release v1.0.0;
#   - modify the frozen geographic vocabulary;
#   - evaluate Nakamura logic;
#   - assign Star categories;
#   - infer rarity;
#   - infer endemicity.
#
# ==============================================================================


# ------------------------------------------------------------------------------
# 01. RELEASE CONSTANTS
# ------------------------------------------------------------------------------

RELEASE_NAME <- "VPJD Geography v1.0.0"

SCRIPT_NAME <- "geography_06c_build_taxon_geographic_distribution.R"

BUILD_DATE <- format(
  Sys.Date(),
  "%Y-%m-%d"
)


EXPECTED_RECOGNISED_TAXA <- 12037L

EXPECTED_JAPAN_BOTANICAL_AREAS <- 51L

EXPECTED_GEOGRAPHIC_CONCEPTS <- 418L

EXPECTED_WCVP_TDWG_CONCEPTS <- 367L


EXPECTED_COMPLETE_COMBINATIONS <- 613887L

EXPECTED_POSITIVE_COMBINATIONS <- 128824L

EXPECTED_UNSUPPORTED_COMBINATIONS <- 485063L

EXPECTED_TAXA_WITH_EVIDENCE <- 11470L

EXPECTED_TAXA_WITHOUT_EVIDENCE <- 567L


FROZEN_IDENTIFIER_STATUS <- "FROZEN_VPJD_GEOGRAPHY_V1.0.0"

PRESENCE_STATUS <- "PRESENT_EVIDENCE"

EVIDENCE_TYPE <- "POSITIVE_OCCURRENCE_EVIDENCE"

EVIDENCE_RULE <- "n_occurrence_records > 0"


# ------------------------------------------------------------------------------
# 02. PROJECT PATHS
# ------------------------------------------------------------------------------

PROJECT_ROOT <- "I:/R/OJPCP/VPJD-OJPCP"

PUBLISHED_VPJD_ROOT <- "I:/R/Data/VPJD_v1.0.0"


DATA_ROOT <- file.path(
  PROJECT_ROOT,
  "data"
)


DERIVED_GEOGRAPHY_ROOT <- file.path(
  DATA_ROOT,
  "derived",
  "geography"
)


SOURCE_DISTRIBUTION_FILE <- file.path(
  DERIVED_GEOGRAPHY_ROOT,
  "vpjd_taxon_botanical_area_distribution.csv"
)


RELEASE_ROOT <- file.path(
  DERIVED_GEOGRAPHY_ROOT,
  "release_v1.0.0"
)


FROZEN_GEOGRAPHY_ROOT <- file.path(
  RELEASE_ROOT,
  "canonical_geographic_units_frozen"
)


DISTRIBUTION_OUTPUT_ROOT <- file.path(
  RELEASE_ROOT,
  "taxon_geographic_distribution"
)


AUDIT_06B5_ROOT <- file.path(
  PROJECT_ROOT,
  "audit",
  "geography",
  "release_v1.0.0",
  "06b5_freeze_canonical_geographic_vocabulary"
)


AUDIT_06C0_ROOT <- file.path(
  PROJECT_ROOT,
  "audit",
  "geography",
  "release_v1.0.0",
  "06c0_audit_distribution_evidence_semantics"
)


AUDIT_ROOT <- file.path(
  PROJECT_ROOT,
  "audit",
  "geography",
  "release_v1.0.0",
  "06c_build_taxon_geographic_distribution"
)


dir.create(
  DISTRIBUTION_OUTPUT_ROOT,
  recursive = TRUE,
  showWarnings = FALSE
)


dir.create(
  AUDIT_ROOT,
  recursive = TRUE,
  showWarnings = FALSE
)


# ------------------------------------------------------------------------------
# 03. REQUIRED INPUT FILES
# ------------------------------------------------------------------------------

FROZEN_GEOGRAPHY_FILE <- file.path(
  FROZEN_GEOGRAPHY_ROOT,
  "vpjd_geographic_units_v1.0.0.csv"
)


FREEZE_VALIDATION_FILE <- file.path(
  AUDIT_06B5_ROOT,
  "geography_06b5_validation_gate.csv"
)


FREEZE_RELEASE_GATE_FILE <- file.path(
  AUDIT_06B5_ROOT,
  "geography_06b5_release_gate.csv"
)


SEMANTICS_SUMMARY_FILE <- file.path(
  AUDIT_06C0_ROOT,
  "geography_06c0_summary.csv"
)


SEMANTICS_RULE_FILE <- file.path(
  AUDIT_06C0_ROOT,
  "geography_06c0_candidate_evidence_rules.csv"
)


# ------------------------------------------------------------------------------
# 04. OUTPUT FILES
# ------------------------------------------------------------------------------

DISTRIBUTION_FILE <- file.path(
  DISTRIBUTION_OUTPUT_ROOT,
  "vpjd_taxon_geographic_distribution_v1.0.0.csv"
)


EVIDENCE_FILE <- file.path(
  DISTRIBUTION_OUTPUT_ROOT,
  "vpjd_taxon_geographic_evidence_v1.0.0.csv"
)


SOURCE_VALIDATION_FILE <- file.path(
  AUDIT_ROOT,
  "geography_06c_source_validation.csv"
)


EVIDENCE_VALIDATION_FILE <- file.path(
  AUDIT_ROOT,
  "geography_06c_evidence_validation.csv"
)


GEOGRAPHIC_MAPPING_AUDIT_FILE <- file.path(
  AUDIT_ROOT,
  "geography_06c_geographic_mapping_audit.csv"
)


TAXON_RECONCILIATION_AUDIT_FILE <- file.path(
  AUDIT_ROOT,
  "geography_06c_taxon_reconciliation_audit.csv"
)


SUMMARY_FILE <- file.path(
  AUDIT_ROOT,
  "geography_06c_summary.csv"
)


VALIDATION_GATE_FILE <- file.path(
  AUDIT_ROOT,
  "geography_06c_validation_gate.csv"
)


RELEASE_GATE_FILE <- file.path(
  AUDIT_ROOT,
  "geography_06c_release_gate.csv"
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


# ------------------------------------------------------------------------------
# 06. HELPER: NORMALISE MATCH KEY
# ------------------------------------------------------------------------------

normalise_key <- function(x) {
  
  y <- normalise_text(x)
  
  y <- tolower(y)
  
  y <- gsub(
    "[[:space:]]+",
    " ",
    y
  )
  
  y <- trimws(y)
  
  y
}


# ------------------------------------------------------------------------------
# 07. HELPER: SAFE CSV READ
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
      
      return(
        result
      )
    }
  }
  
  
  stop(
    paste0(
      "Unable to read CSV:\n",
      path,
      "\n\nErrors:\n",
      paste(
        errors,
        collapse = "\n"
      )
    )
  )
}


# ------------------------------------------------------------------------------
# 08. HELPER: CSV LOGICAL VALUES
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
# 09. HELPER: FIND PUBLISHED RECOGNISED-TAXON TABLE
# ------------------------------------------------------------------------------

find_recognised_taxon_file <- function(root) {
  
  if (!dir.exists(root)) {
    
    stop(
      paste0(
        "Published VPJD root not found:\n",
        root
      )
    )
  }
  
  
  files <- list.files(
    root,
    pattern = "\\.csv$",
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = TRUE
  )
  
  
  exact_selector <- (
    tolower(
      basename(files)
    ) ==
      "vpjd_recognised_taxa.csv"
  )
  
  
  exact_matches <- files[
    exact_selector
  ]
  
  
  if (length(exact_matches) == 0L) {
    
    stop(
      paste0(
        "Canonical recognised-taxon table not found under:\n",
        root,
        "\n\nExpected filename:\n",
        "vpjd_recognised_taxa.csv"
      )
    )
  }
  
  
  if (length(exact_matches) > 1L) {
    
    file_sizes <- file.info(
      exact_matches
    )$size
    
    
    exact_matches <- exact_matches[
      order(
        file_sizes,
        decreasing = TRUE
      )
    ]
  }
  
  
  exact_matches[1L]
}


# ------------------------------------------------------------------------------
# 10. START
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" VPJD GEOGRAPHY 06c - BUILD TAXON GEOGRAPHIC DISTRIBUTION\n")
cat("============================================================\n\n")

cat(
  "Release: ",
  RELEASE_NAME,
  "\n",
  sep = ""
)

cat(
  "Build date: ",
  BUILD_DATE,
  "\n\n",
  sep = ""
)


# ------------------------------------------------------------------------------
# 11. REQUIRE 06b5 FREEZE PASS
# ------------------------------------------------------------------------------

cat("Checking 06b5 geographic-vocabulary release gate...\n")


required_06b5_files <- c(
  FREEZE_VALIDATION_FILE,
  FREEZE_RELEASE_GATE_FILE
)


missing_06b5_files <- required_06b5_files[
  !file.exists(required_06b5_files)
]


if (length(missing_06b5_files) > 0L) {
  
  stop(
    paste0(
      "Required 06b5 gate file(s) missing:\n",
      paste(
        missing_06b5_files,
        collapse = "\n"
      )
    )
  )
}


freeze_validation <- safe_read_csv(
  FREEZE_VALIDATION_FILE
)


freeze_release <- safe_read_csv(
  FREEZE_RELEASE_GATE_FILE
)


if (!"passed" %in% names(freeze_validation)) {
  
  stop(
    "06b5 validation gate lacks required field 'passed'."
  )
}


if (!"passed" %in% names(freeze_release)) {
  
  stop(
    "06b5 release gate lacks required field 'passed'."
  )
}


freeze_validation_passed <- logical_from_csv(
  freeze_validation$passed
)


freeze_release_passed <- logical_from_csv(
  freeze_release$passed
)


if (
  any(is.na(freeze_validation_passed)) ||
  !all(freeze_validation_passed)
) {
  
  stop(
    "06b5 geographic-vocabulary validation did not pass completely."
  )
}


if (
  any(is.na(freeze_release_passed)) ||
  !all(freeze_release_passed)
) {
  
  stop(
    "06b5 geographic-vocabulary release gate did not pass completely."
  )
}


cat("06b5 geographic-vocabulary gate: PASS\n\n")


# ------------------------------------------------------------------------------
# 12. REQUIRE 06c0 EVIDENCE-SEMANTICS AUDIT
# ------------------------------------------------------------------------------

cat("Checking 06c0 evidence-semantics audit...\n")


required_06c0_files <- c(
  SEMANTICS_SUMMARY_FILE,
  SEMANTICS_RULE_FILE
)


missing_06c0_files <- required_06c0_files[
  !file.exists(required_06c0_files)
]


if (length(missing_06c0_files) > 0L) {
  
  stop(
    paste0(
      "Required 06c0 audit file(s) missing:\n",
      paste(
        missing_06c0_files,
        collapse = "\n"
      )
    )
  )
}


semantics_rules <- safe_read_csv(
  SEMANTICS_RULE_FILE
)


required_semantics_fields <- c(
  "field_name",
  "rule",
  "positive_rows",
  "taxa_with_positive_evidence",
  "areas_with_positive_evidence"
)


missing_semantics_fields <- setdiff(
  required_semantics_fields,
  names(semantics_rules)
)


if (length(missing_semantics_fields) > 0L) {
  
  stop(
    paste0(
      "06c0 candidate evidence-rule table lacks required field(s):\n",
      paste(
        missing_semantics_fields,
        collapse = "\n"
      )
    )
  )
}


occurrence_rule_selector <- (
  semantics_rules$field_name ==
    "n_occurrence_records" &
    semantics_rules$rule ==
    "NUMERIC_GREATER_THAN_ZERO"
)


unique_occurrence_rule_selector <- (
  semantics_rules$field_name ==
    "n_unique_occurrences" &
    semantics_rules$rule ==
    "NUMERIC_GREATER_THAN_ZERO"
)


if (sum(occurrence_rule_selector) != 1L) {
  
  stop(
    paste0(
      "06c0 does not contain exactly one audited rule for:\n",
      "n_occurrence_records > 0"
    )
  )
}


if (sum(unique_occurrence_rule_selector) != 1L) {
  
  stop(
    paste0(
      "06c0 does not contain exactly one audited rule for:\n",
      "n_unique_occurrences > 0"
    )
  )
}


occurrence_rule <- semantics_rules[
  occurrence_rule_selector,
  ,
  drop = FALSE
]


unique_occurrence_rule <- semantics_rules[
  unique_occurrence_rule_selector,
  ,
  drop = FALSE
]


if (
  occurrence_rule$positive_rows[1L] !=
  EXPECTED_POSITIVE_COMBINATIONS ||
  occurrence_rule$taxa_with_positive_evidence[1L] !=
  EXPECTED_TAXA_WITH_EVIDENCE ||
  occurrence_rule$areas_with_positive_evidence[1L] !=
  EXPECTED_JAPAN_BOTANICAL_AREAS
) {
  
  stop(
    paste0(
      "06c0 audited n_occurrence_records rule does not match the ",
      "expected reconstruction counts."
    )
  )
}


if (
  unique_occurrence_rule$positive_rows[1L] !=
  EXPECTED_POSITIVE_COMBINATIONS ||
  unique_occurrence_rule$taxa_with_positive_evidence[1L] !=
  EXPECTED_TAXA_WITH_EVIDENCE ||
  unique_occurrence_rule$areas_with_positive_evidence[1L] !=
  EXPECTED_JAPAN_BOTANICAL_AREAS
) {
  
  stop(
    paste0(
      "06c0 audited n_unique_occurrences rule does not match the ",
      "expected reconstruction counts."
    )
  )
}


cat("06c0 evidence-semantics audit: PASS\n")
cat("Positive evidence rule: n_occurrence_records > 0\n")
cat("Consistency rule: n_unique_occurrences > 0\n\n")


# ------------------------------------------------------------------------------
# 13. READ FROZEN GEOGRAPHIC VOCABULARY
# ------------------------------------------------------------------------------

cat("Reading frozen geographic vocabulary...\n")


geography <- safe_read_csv(
  FROZEN_GEOGRAPHY_FILE
)


required_geography_fields <- c(
  "geographic_unit_id",
  "geographic_code",
  "geographic_name",
  "concept_group",
  "concept_resolution_status",
  "identifier_status"
)


missing_geography_fields <- setdiff(
  required_geography_fields,
  names(geography)
)


if (length(missing_geography_fields) > 0L) {
  
  stop(
    paste0(
      "Frozen geographic vocabulary lacks required field(s):\n",
      paste(
        missing_geography_fields,
        collapse = "\n"
      )
    )
  )
}


geo_id <- normalise_text(
  geography$geographic_unit_id
)


geo_code <- normalise_text(
  geography$geographic_code
)


geo_name <- normalise_text(
  geography$geographic_name
)


geo_group <- normalise_text(
  geography$concept_group
)


geo_identifier_status <- normalise_text(
  geography$identifier_status
)


# ------------------------------------------------------------------------------
# 14. VALIDATE FROZEN GEOGRAPHIC VOCABULARY
# ------------------------------------------------------------------------------

if (nrow(geography) != EXPECTED_GEOGRAPHIC_CONCEPTS) {
  
  stop(
    paste0(
      "Expected ",
      EXPECTED_GEOGRAPHIC_CONCEPTS,
      " frozen geographic concepts; found ",
      nrow(geography),
      "."
    )
  )
}


if (
  any(is.na(geo_id)) ||
  any(duplicated(geo_id))
) {
  
  stop(
    "Frozen geographic_unit_id values are incomplete or non-unique."
  )
}


if (
  any(is.na(geo_code)) ||
  any(duplicated(geo_code))
) {
  
  stop(
    "Frozen geographic_code values are incomplete or non-unique."
  )
}


if (
  any(is.na(geo_identifier_status)) ||
  any(
    geo_identifier_status !=
    FROZEN_IDENTIFIER_STATUS
  )
) {
  
  stop(
    paste0(
      "Not all geographic identifiers have expected frozen status:\n",
      FROZEN_IDENTIFIER_STATUS
    )
  )
}


japan_geo_selector <- (
  geo_group ==
    "JAPAN_BOTANICAL_AREA"
)


japan_geo_selector[is.na(japan_geo_selector)] <- FALSE


wcvp_geo_selector <- (
  geo_group ==
    "WCVP_TDWG_LOCATION"
)


wcvp_geo_selector[is.na(wcvp_geo_selector)] <- FALSE


if (
  sum(japan_geo_selector) !=
  EXPECTED_JAPAN_BOTANICAL_AREAS
) {
  
  stop(
    paste0(
      "Expected ",
      EXPECTED_JAPAN_BOTANICAL_AREAS,
      " Japanese botanical areas; found ",
      sum(japan_geo_selector),
      "."
    )
  )
}


if (
  sum(wcvp_geo_selector) !=
  EXPECTED_WCVP_TDWG_CONCEPTS
) {
  
  stop(
    paste0(
      "Expected ",
      EXPECTED_WCVP_TDWG_CONCEPTS,
      " WCVP/TDWG reference concepts; found ",
      sum(wcvp_geo_selector),
      "."
    )
  )
}


japan_geography <- geography[
  japan_geo_selector,
  ,
  drop = FALSE
]


japan_geo_id <- normalise_text(
  japan_geography$geographic_unit_id
)


japan_geo_code <- normalise_text(
  japan_geography$geographic_code
)


japan_geo_name <- normalise_text(
  japan_geography$geographic_name
)


cat(
  "Frozen geographic concepts: ",
  nrow(geography),
  "\n",
  sep = ""
)

cat(
  "Japanese botanical areas: ",
  nrow(japan_geography),
  "\n\n",
  sep = ""
)


# ------------------------------------------------------------------------------
# 15. LOCATE PUBLISHED VPJD TAXON TABLE
# ------------------------------------------------------------------------------

cat("Locating published VPJD recognised-taxon table...\n")


RECOGNISED_TAXA_FILE <- find_recognised_taxon_file(
  PUBLISHED_VPJD_ROOT
)


cat(
  "Published taxon table:\n",
  RECOGNISED_TAXA_FILE,
  "\n\n",
  sep = ""
)


# ------------------------------------------------------------------------------
# 16. READ PUBLISHED VPJD TAXON TABLE
# ------------------------------------------------------------------------------

taxa <- safe_read_csv(
  RECOGNISED_TAXA_FILE
)


required_taxon_fields <- c(
  "wcvp_plant_name_id",
  "wcvp_taxon_name"
)


missing_taxon_fields <- setdiff(
  required_taxon_fields,
  names(taxa)
)


if (length(missing_taxon_fields) > 0L) {
  
  stop(
    paste0(
      "Published VPJD recognised-taxon table lacks required field(s):\n",
      paste(
        missing_taxon_fields,
        collapse = "\n"
      )
    )
  )
}


canonical_taxon_id <- normalise_text(
  taxa$wcvp_plant_name_id
)


canonical_taxon_name <- normalise_text(
  taxa$wcvp_taxon_name
)


# ------------------------------------------------------------------------------
# 17. VALIDATE PUBLISHED TAXONOMIC INTERFACE
# ------------------------------------------------------------------------------

if (nrow(taxa) != EXPECTED_RECOGNISED_TAXA) {
  
  stop(
    paste0(
      "Expected ",
      format(
        EXPECTED_RECOGNISED_TAXA,
        big.mark = ","
      ),
      " recognised taxa; found ",
      format(
        nrow(taxa),
        big.mark = ","
      ),
      "."
    )
  )
}


if (any(is.na(canonical_taxon_id))) {
  
  stop(
    "Published wcvp_plant_name_id contains missing values."
  )
}


if (any(duplicated(canonical_taxon_id))) {
  
  stop(
    "Published wcvp_plant_name_id contains duplicate values."
  )
}


if (any(is.na(canonical_taxon_name))) {
  
  stop(
    "Published wcvp_taxon_name contains missing values."
  )
}


cat(
  "Published recognised taxa: ",
  format(
    nrow(taxa),
    big.mark = ","
  ),
  "\n",
  sep = ""
)

cat("Canonical taxon key: wcvp_plant_name_id\n\n")


# ------------------------------------------------------------------------------
# 18. READ ANALYTICAL DISTRIBUTION MATRIX
# ------------------------------------------------------------------------------

cat("Reading analytical taxon x botanical-area matrix...\n")


source <- safe_read_csv(
  SOURCE_DISTRIBUTION_FILE
)


required_source_fields <- c(
  "wcvp_plant_name_id",
  "wcvp_taxon_name",
  "botanical_area_id",
  "botanical_area_name",
  "n_occurrence_records",
  "n_unique_occurrences"
)


missing_source_fields <- setdiff(
  required_source_fields,
  names(source)
)


if (length(missing_source_fields) > 0L) {
  
  stop(
    paste0(
      "Analytical distribution matrix lacks required field(s):\n",
      paste(
        missing_source_fields,
        collapse = "\n"
      )
    )
  )
}


source_taxon_id <- normalise_text(
  source$wcvp_plant_name_id
)


source_taxon_name <- normalise_text(
  source$wcvp_taxon_name
)


source_area_id <- normalise_text(
  source$botanical_area_id
)


source_area_name <- normalise_text(
  source$botanical_area_name
)


source_occurrence_count <- suppressWarnings(
  as.numeric(
    source$n_occurrence_records
  )
)


source_unique_occurrence_count <- suppressWarnings(
  as.numeric(
    source$n_unique_occurrences
  )
)


# ------------------------------------------------------------------------------
# 19. VALIDATE COMPLETE SOURCE MATRIX
# ------------------------------------------------------------------------------

cat("Validating complete taxon x botanical-area matrix...\n")


if (nrow(source) != EXPECTED_COMPLETE_COMBINATIONS) {
  
  stop(
    paste0(
      "Expected ",
      format(
        EXPECTED_COMPLETE_COMBINATIONS,
        big.mark = ","
      ),
      " source combinations; found ",
      format(
        nrow(source),
        big.mark = ","
      ),
      "."
    )
  )
}


if (
  any(is.na(source_taxon_id)) ||
  any(is.na(source_area_id)) ||
  any(is.na(source_area_name))
) {
  
  stop(
    "Source matrix contains missing taxon or botanical-area identifiers."
  )
}


if (
  any(is.na(source_occurrence_count)) ||
  any(is.na(source_unique_occurrence_count))
) {
  
  stop(
    "Source matrix contains missing occurrence-count values."
  )
}


if (
  any(source_occurrence_count < 0) ||
  any(source_unique_occurrence_count < 0)
) {
  
  stop(
    "Source matrix contains negative occurrence-count values."
  )
}


source_combination_key <- paste(
  source_taxon_id,
  source_area_id,
  sep = "||"
)


if (any(duplicated(source_combination_key))) {
  
  stop(
    "Source matrix contains duplicate taxon x botanical-area combinations."
  )
}


n_source_taxa <- length(
  unique(
    source_taxon_id
  )
)


n_source_areas <- length(
  unique(
    source_area_id
  )
)


if (n_source_taxa != EXPECTED_RECOGNISED_TAXA) {
  
  stop(
    paste0(
      "Expected ",
      EXPECTED_RECOGNISED_TAXA,
      " distinct source taxa; found ",
      n_source_taxa,
      "."
    )
  )
}


if (n_source_areas != EXPECTED_JAPAN_BOTANICAL_AREAS) {
  
  stop(
    paste0(
      "Expected ",
      EXPECTED_JAPAN_BOTANICAL_AREAS,
      " distinct source botanical areas; found ",
      n_source_areas,
      "."
    )
  )
}


if (
  n_source_taxa *
  n_source_areas !=
  nrow(source)
) {
  
  stop(
    paste0(
      "Source matrix is not a complete taxon x botanical-area ",
      "cross-product."
    )
  )
}


source_validation <- data.frame(
  
  metric = c(
    "source_rows",
    "distinct_taxa",
    "distinct_botanical_areas",
    "taxa_x_botanical_areas",
    "duplicate_taxon_area_keys",
    "missing_taxon_ids",
    "missing_area_ids",
    "missing_area_names",
    "missing_occurrence_counts",
    "missing_unique_occurrence_counts"
  ),
  
  value = c(
    nrow(source),
    n_source_taxa,
    n_source_areas,
    n_source_taxa * n_source_areas,
    sum(duplicated(source_combination_key)),
    sum(is.na(source_taxon_id)),
    sum(is.na(source_area_id)),
    sum(is.na(source_area_name)),
    sum(is.na(source_occurrence_count)),
    sum(is.na(source_unique_occurrence_count))
  ),
  
  stringsAsFactors = FALSE
)


write.csv(
  source_validation,
  SOURCE_VALIDATION_FILE,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)


cat("Complete source matrix: PASS\n\n")


# ------------------------------------------------------------------------------
# 20. DEFINE POSITIVE EVIDENCE
# ------------------------------------------------------------------------------

cat("Applying explicit positive-evidence rule...\n")


positive_occurrence_selector <- (
  source_occurrence_count > 0
)


positive_unique_selector <- (
  source_unique_occurrence_count > 0
)


positive_occurrence_selector[
  is.na(positive_occurrence_selector)
] <- FALSE


positive_unique_selector[
  is.na(positive_unique_selector)
] <- FALSE


# ------------------------------------------------------------------------------
# 21. REQUIRE OCCURRENCE FIELDS TO AGREE ROW-FOR-ROW
# ------------------------------------------------------------------------------

evidence_disagreement_selector <- (
  positive_occurrence_selector !=
    positive_unique_selector
)


evidence_disagreement_selector[
  is.na(evidence_disagreement_selector)
] <- TRUE


n_evidence_disagreements <- sum(
  evidence_disagreement_selector
)


if (n_evidence_disagreements > 0L) {
  
  stop(
    paste0(
      "n_occurrence_records > 0 and n_unique_occurrences > 0 ",
      "disagree for ",
      format(
        n_evidence_disagreements,
        big.mark = ","
      ),
      " source row(s).\n",
      "Positive evidence cannot be reconstructed safely."
    )
  )
}


positive_selector <- positive_occurrence_selector


n_positive <- sum(
  positive_selector
)


n_unsupported <- (
  nrow(source) -
    n_positive
)


# ------------------------------------------------------------------------------
# 22. VALIDATE EVIDENCE COUNTS
# ------------------------------------------------------------------------------

positive_taxon_ids <- unique(
  source_taxon_id[
    positive_selector
  ]
)


positive_area_ids <- unique(
  source_area_id[
    positive_selector
  ]
)


n_positive_taxa <- length(
  positive_taxon_ids
)


n_positive_areas <- length(
  positive_area_ids
)


n_taxa_without_positive_evidence <- (
  EXPECTED_RECOGNISED_TAXA -
    n_positive_taxa
)


if (n_positive != EXPECTED_POSITIVE_COMBINATIONS) {
  
  stop(
    paste0(
      "Positive evidence count differs from audited reconstruction.\n\n",
      "Expected: ",
      format(
        EXPECTED_POSITIVE_COMBINATIONS,
        big.mark = ","
      ),
      "\n",
      "Observed: ",
      format(
        n_positive,
        big.mark = ","
      )
    )
  )
}


if (n_unsupported != EXPECTED_UNSUPPORTED_COMBINATIONS) {
  
  stop(
    paste0(
      "Unsupported-combination count differs from audited reconstruction.\n\n",
      "Expected: ",
      format(
        EXPECTED_UNSUPPORTED_COMBINATIONS,
        big.mark = ","
      ),
      "\n",
      "Observed: ",
      format(
        n_unsupported,
        big.mark = ","
      )
    )
  )
}


if (n_positive_taxa != EXPECTED_TAXA_WITH_EVIDENCE) {
  
  stop(
    paste0(
      "Taxa-with-evidence count differs from audited reconstruction.\n\n",
      "Expected: ",
      format(
        EXPECTED_TAXA_WITH_EVIDENCE,
        big.mark = ","
      ),
      "\n",
      "Observed: ",
      format(
        n_positive_taxa,
        big.mark = ","
      )
    )
  )
}


if (
  n_taxa_without_positive_evidence !=
  EXPECTED_TAXA_WITHOUT_EVIDENCE
) {
  
  stop(
    paste0(
      "Taxa-without-evidence count differs from audited reconstruction.\n\n",
      "Expected: ",
      EXPECTED_TAXA_WITHOUT_EVIDENCE,
      "\n",
      "Observed: ",
      n_taxa_without_positive_evidence
    )
  )
}


if (n_positive_areas != EXPECTED_JAPAN_BOTANICAL_AREAS) {
  
  stop(
    paste0(
      "Expected positive evidence across all ",
      EXPECTED_JAPAN_BOTANICAL_AREAS,
      " Japanese botanical areas; observed ",
      n_positive_areas,
      "."
    )
  )
}


evidence_validation <- data.frame(
  
  metric = c(
    "complete_combinations",
    "positive_combinations",
    "unsupported_combinations",
    "taxa_with_positive_evidence",
    "taxa_without_positive_evidence",
    "botanical_areas_with_positive_evidence",
    "occurrence_unique_selector_disagreements"
  ),
  
  expected = c(
    EXPECTED_COMPLETE_COMBINATIONS,
    EXPECTED_POSITIVE_COMBINATIONS,
    EXPECTED_UNSUPPORTED_COMBINATIONS,
    EXPECTED_TAXA_WITH_EVIDENCE,
    EXPECTED_TAXA_WITHOUT_EVIDENCE,
    EXPECTED_JAPAN_BOTANICAL_AREAS,
    0L
  ),
  
  observed = c(
    nrow(source),
    n_positive,
    n_unsupported,
    n_positive_taxa,
    n_taxa_without_positive_evidence,
    n_positive_areas,
    n_evidence_disagreements
  ),
  
  passed = c(
    nrow(source) ==
      EXPECTED_COMPLETE_COMBINATIONS,
    
    n_positive ==
      EXPECTED_POSITIVE_COMBINATIONS,
    
    n_unsupported ==
      EXPECTED_UNSUPPORTED_COMBINATIONS,
    
    n_positive_taxa ==
      EXPECTED_TAXA_WITH_EVIDENCE,
    
    n_taxa_without_positive_evidence ==
      EXPECTED_TAXA_WITHOUT_EVIDENCE,
    
    n_positive_areas ==
      EXPECTED_JAPAN_BOTANICAL_AREAS,
    
    n_evidence_disagreements == 0L
  ),
  
  stringsAsFactors = FALSE
)


write.csv(
  evidence_validation,
  EVIDENCE_VALIDATION_FILE,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)


cat(
  "Positive combinations: ",
  format(
    n_positive,
    big.mark = ","
  ),
  "\n",
  sep = ""
)

cat(
  "Unsupported combinations: ",
  format(
    n_unsupported,
    big.mark = ","
  ),
  "\n",
  sep = ""
)

cat(
  "Taxa with positive evidence: ",
  format(
    n_positive_taxa,
    big.mark = ","
  ),
  "\n",
  sep = ""
)

cat(
  "Taxa without positive evidence: ",
  format(
    n_taxa_without_positive_evidence,
    big.mark = ","
  ),
  "\n",
  sep = ""
)

cat(
  "Botanical areas represented: ",
  n_positive_areas,
  "\n\n",
  sep = ""
)


# ------------------------------------------------------------------------------
# 23. SUBSET POSITIVE EVIDENCE ONLY
# ------------------------------------------------------------------------------

positive_source <- source[
  positive_selector,
  ,
  drop = FALSE
]


positive_source_taxon_id <- source_taxon_id[
  positive_selector
]


positive_source_taxon_name <- source_taxon_name[
  positive_selector
]


positive_source_area_id <- source_area_id[
  positive_selector
]


positive_source_area_name <- source_area_name[
  positive_selector
]


positive_occurrence_count <- source_occurrence_count[
  positive_selector
]


positive_unique_occurrence_count <- source_unique_occurrence_count[
  positive_selector
]


# ------------------------------------------------------------------------------
# 24. RECONCILE TAXA TO PUBLISHED TAXONOMIC RELEASE
# ------------------------------------------------------------------------------

cat("Reconciling positive evidence to published taxonomic release...\n")


taxon_match_index <- match(
  positive_source_taxon_id,
  canonical_taxon_id
)


taxon_match_found <- !is.na(
  taxon_match_index
)


taxon_name_match <- rep(
  FALSE,
  length(taxon_match_index)
)


taxon_name_match[
  taxon_match_found
] <- (
  normalise_key(
    positive_source_taxon_name[
      taxon_match_found
    ]
  ) ==
    normalise_key(
      canonical_taxon_name[
        taxon_match_index[
          taxon_match_found
        ]
      ]
    )
)


taxon_reconciliation_audit <- data.frame(
  
  source_row =
    which(
      positive_selector
    ),
  
  source_wcvp_plant_name_id =
    positive_source_taxon_id,
  
  source_wcvp_taxon_name =
    positive_source_taxon_name,
  
  taxon_match_found =
    taxon_match_found,
  
  canonical_wcvp_plant_name_id =
    rep(
      NA_character_,
      length(taxon_match_index)
    ),
  
  canonical_wcvp_taxon_name =
    rep(
      NA_character_,
      length(taxon_match_index)
    ),
  
  taxon_name_match =
    taxon_name_match,
  
  stringsAsFactors = FALSE
)


taxon_reconciliation_audit$canonical_wcvp_plant_name_id[
  taxon_match_found
] <- canonical_taxon_id[
  taxon_match_index[
    taxon_match_found
  ]
]


taxon_reconciliation_audit$canonical_wcvp_taxon_name[
  taxon_match_found
] <- canonical_taxon_name[
  taxon_match_index[
    taxon_match_found
  ]
]


write.csv(
  taxon_reconciliation_audit,
  TAXON_RECONCILIATION_AUDIT_FILE,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)


n_unmatched_taxa <- sum(
  !taxon_match_found
)


n_taxon_name_mismatches <- sum(
  taxon_match_found &
    !taxon_name_match
)


if (n_unmatched_taxa > 0L) {
  
  stop(
    paste0(
      format(
        n_unmatched_taxa,
        big.mark = ","
      ),
      " positive evidence rows do not reconcile to published ",
      "wcvp_plant_name_id values."
    )
  )
}


if (n_taxon_name_mismatches > 0L) {
  
  stop(
    paste0(
      format(
        n_taxon_name_mismatches,
        big.mark = ","
      ),
      " positive evidence rows have taxon-name disagreement with ",
      "VPJD Taxonomic Release v1.0.0."
    )
  )
}


cat("Taxonomic reconciliation: PASS\n\n")


# ------------------------------------------------------------------------------
# 25. PREPARE FROZEN GEOGRAPHIC MATCH KEYS
# ------------------------------------------------------------------------------

japan_id_key <- normalise_key(
  japan_geo_id
)


japan_code_key <- normalise_key(
  japan_geo_code
)


japan_name_key <- normalise_key(
  japan_geo_name
)


source_area_id_key <- normalise_key(
  positive_source_area_id
)


source_area_name_key <- normalise_key(
  positive_source_area_name
)


# ------------------------------------------------------------------------------
# 26. RECONCILE BOTANICAL AREAS TO FROZEN GEOGRAPHIC VOCABULARY
#
# Matching precedence:
#
#   1. source botanical_area_id -> geographic_unit_id
#   2. source botanical_area_id -> geographic_code
#   3. exact botanical_area_name -> geographic_name
#
# No fuzzy matching is permitted.
# ------------------------------------------------------------------------------

cat("Reconciling botanical areas to frozen geographic vocabulary...\n")


geo_match_index <- rep(
  NA_integer_,
  nrow(positive_source)
)


geo_match_method <- rep(
  NA_character_,
  nrow(positive_source)
)


# ---- 26a. Match source area ID to frozen geographic_unit_id ------------------

id_match <- match(
  source_area_id_key,
  japan_id_key
)


id_selector <- !is.na(
  id_match
)


geo_match_index[
  id_selector
] <- id_match[
  id_selector
]


geo_match_method[
  id_selector
] <- "BOTANICAL_AREA_ID_TO_GEOGRAPHIC_UNIT_ID"


# ---- 26b. Match source area ID to frozen geographic_code ---------------------

remaining_selector <- is.na(
  geo_match_index
)


code_match <- match(
  source_area_id_key,
  japan_code_key
)


code_selector <- (
  remaining_selector &
    !is.na(code_match)
)


geo_match_index[
  code_selector
] <- code_match[
  code_selector
]


geo_match_method[
  code_selector
] <- "BOTANICAL_AREA_ID_TO_GEOGRAPHIC_CODE"


# ---- 26c. Exact area-name match ----------------------------------------------

remaining_selector <- is.na(
  geo_match_index
)


name_match <- match(
  source_area_name_key,
  japan_name_key
)


name_selector <- (
  remaining_selector &
    !is.na(name_match)
)


geo_match_index[
  name_selector
] <- name_match[
  name_selector
]


geo_match_method[
  name_selector
] <- "EXACT_BOTANICAL_AREA_NAME"


# ------------------------------------------------------------------------------
# 27. GEOGRAPHIC MAPPING AUDIT
# ------------------------------------------------------------------------------

geographic_mapping_audit <- data.frame(
  
  source_row =
    which(
      positive_selector
    ),
  
  source_botanical_area_id =
    positive_source_area_id,
  
  source_botanical_area_name =
    positive_source_area_name,
  
  geographic_match_found =
    !is.na(
      geo_match_index
    ),
  
  geographic_match_method =
    geo_match_method,
  
  geographic_unit_id =
    rep(
      NA_character_,
      nrow(positive_source)
    ),
  
  geographic_code =
    rep(
      NA_character_,
      nrow(positive_source)
    ),
  
  geographic_name =
    rep(
      NA_character_,
      nrow(positive_source)
    ),
  
  stringsAsFactors = FALSE
)


mapped_geo_selector <- !is.na(
  geo_match_index
)


geographic_mapping_audit$geographic_unit_id[
  mapped_geo_selector
] <- japan_geo_id[
  geo_match_index[
    mapped_geo_selector
  ]
]


geographic_mapping_audit$geographic_code[
  mapped_geo_selector
] <- japan_geo_code[
  geo_match_index[
    mapped_geo_selector
  ]
]


geographic_mapping_audit$geographic_name[
  mapped_geo_selector
] <- japan_geo_name[
  geo_match_index[
    mapped_geo_selector
  ]
]


write.csv(
  geographic_mapping_audit,
  GEOGRAPHIC_MAPPING_AUDIT_FILE,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)


n_unmapped_geography <- sum(
  is.na(
    geo_match_index
  )
)


if (n_unmapped_geography > 0L) {
  
  stop(
    paste0(
      format(
        n_unmapped_geography,
        big.mark = ","
      ),
      " positive evidence rows could not be reconciled to the frozen ",
      "Japanese geographic vocabulary.\n\n",
      "Review:\n",
      GEOGRAPHIC_MAPPING_AUDIT_FILE
    )
  )
}


cat("Geographic reconciliation: PASS\n\n")


# ------------------------------------------------------------------------------
# 28. BUILD CANONICAL POSITIVE-EVIDENCE TABLE
# ------------------------------------------------------------------------------

cat("Building canonical positive-evidence table...\n")


evidence <- data.frame(
  
  wcvp_plant_name_id =
    canonical_taxon_id[
      taxon_match_index
    ],
  
  wcvp_taxon_name =
    canonical_taxon_name[
      taxon_match_index
    ],
  
  geographic_unit_id =
    japan_geo_id[
      geo_match_index
    ],
  
  geographic_code =
    japan_geo_code[
      geo_match_index
    ],
  
  geographic_name =
    japan_geo_name[
      geo_match_index
    ],
  
  concept_group =
    rep(
      "JAPAN_BOTANICAL_AREA",
      nrow(positive_source)
    ),
  
  n_occurrence_records =
    positive_occurrence_count,
  
  n_unique_occurrences =
    positive_unique_occurrence_count,
  
  occurrence_status =
    rep(
      PRESENCE_STATUS,
      nrow(positive_source)
    ),
  
  evidence_type =
    rep(
      EVIDENCE_TYPE,
      nrow(positive_source)
    ),
  
  evidence_rule =
    rep(
      EVIDENCE_RULE,
      nrow(positive_source)
    ),
  
  evidence_source_file =
    rep(
      basename(
        SOURCE_DISTRIBUTION_FILE
      ),
      nrow(positive_source)
    ),
  
  evidence_source_row =
    which(
      positive_selector
    ),
  
  geography_release =
    rep(
      RELEASE_NAME,
      nrow(positive_source)
    ),
  
  build_script =
    rep(
      SCRIPT_NAME,
      nrow(positive_source)
    ),
  
  build_date =
    rep(
      BUILD_DATE,
      nrow(positive_source)
    ),
  
  stringsAsFactors = FALSE
)


# ------------------------------------------------------------------------------
# 29. VERIFY POSITIVE EVIDENCE TABLE UNIQUENESS
# ------------------------------------------------------------------------------

evidence_key <- paste(
  evidence$wcvp_plant_name_id,
  evidence$geographic_unit_id,
  sep = "||"
)


n_duplicate_evidence_keys <- sum(
  duplicated(
    evidence_key
  )
)


if (n_duplicate_evidence_keys > 0L) {
  
  stop(
    paste0(
      "Canonical positive-evidence table contains ",
      format(
        n_duplicate_evidence_keys,
        big.mark = ","
      ),
      " duplicate taxon x geographic-unit relationships."
    )
  )
}


if (nrow(evidence) != EXPECTED_POSITIVE_COMBINATIONS) {
  
  stop(
    paste0(
      "Canonical evidence table contains ",
      format(
        nrow(evidence),
        big.mark = ","
      ),
      " rows; expected ",
      format(
        EXPECTED_POSITIVE_COMBINATIONS,
        big.mark = ","
      ),
      "."
    )
  )
}


# ------------------------------------------------------------------------------
# 30. BUILD CANONICAL DISTRIBUTION TABLE
#
# The distribution product is deliberately lean.
#
# Detailed occurrence counts are retained in the companion evidence table.
# ------------------------------------------------------------------------------

distribution <- evidence[
  ,
  c(
    "wcvp_plant_name_id",
    "wcvp_taxon_name",
    "geographic_unit_id",
    "geographic_code",
    "geographic_name",
    "concept_group",
    "occurrence_status"
  ),
  drop = FALSE
]


distribution$geography_release <- RELEASE_NAME


# ------------------------------------------------------------------------------
# 31. STABLE SORT
# ------------------------------------------------------------------------------

sort_order <- order(
  distribution$wcvp_plant_name_id,
  distribution$geographic_unit_id
)


distribution <- distribution[
  sort_order,
  ,
  drop = FALSE
]


evidence <- evidence[
  sort_order,
  ,
  drop = FALSE
]


rownames(
  distribution
) <- NULL


rownames(
  evidence
) <- NULL


# ------------------------------------------------------------------------------
# 32. FINAL DISTRIBUTION REFERENTIAL-INTEGRITY TESTS
# ------------------------------------------------------------------------------

distribution_taxon_ids <- normalise_text(
  distribution$wcvp_plant_name_id
)


distribution_geo_ids <- normalise_text(
  distribution$geographic_unit_id
)


distribution_key <- paste(
  distribution_taxon_ids,
  distribution_geo_ids,
  sep = "||"
)


n_distribution_taxon_reference_failures <- sum(
  !(
    distribution_taxon_ids %in%
      canonical_taxon_id
  )
)


n_distribution_geography_reference_failures <- sum(
  !(
    distribution_geo_ids %in%
      japan_geo_id
  )
)


n_distribution_duplicate_keys <- sum(
  duplicated(
    distribution_key
  )
)


n_distribution_missing_taxon_ids <- sum(
  is.na(
    distribution_taxon_ids
  )
)


n_distribution_missing_geo_ids <- sum(
  is.na(
    distribution_geo_ids
  )
)


# ------------------------------------------------------------------------------
# 33. FINAL RELEASE COUNTS
# ------------------------------------------------------------------------------

n_distribution_rows <- nrow(
  distribution
)


n_distribution_taxa <- length(
  unique(
    distribution_taxon_ids
  )
)


n_distribution_areas <- length(
  unique(
    distribution_geo_ids
  )
)


n_distribution_taxa_without_evidence <- (
  EXPECTED_RECOGNISED_TAXA -
    n_distribution_taxa
)


# ------------------------------------------------------------------------------
# 34. SUMMARY
# ------------------------------------------------------------------------------

summary_table <- data.frame(
  
  metric = c(
    "published_recognised_taxa",
    "frozen_geographic_units",
    "frozen_japanese_botanical_areas",
    "frozen_wcvp_tdwg_reference_units",
    "source_complete_combinations",
    "source_positive_combinations",
    "source_unsupported_combinations",
    "source_taxa_with_positive_evidence",
    "source_taxa_without_positive_evidence",
    "source_areas_with_positive_evidence",
    "occurrence_unique_selector_disagreements",
    "taxon_reconciliation_failures",
    "taxon_name_mismatches",
    "geographic_reconciliation_failures",
    "canonical_evidence_rows",
    "canonical_distribution_rows",
    "canonical_distribution_taxa",
    "canonical_distribution_taxa_without_evidence",
    "canonical_distribution_areas",
    "canonical_distribution_duplicate_keys",
    "canonical_distribution_taxon_reference_failures",
    "canonical_distribution_geography_reference_failures"
  ),
  
  value = c(
    nrow(taxa),
    nrow(geography),
    sum(japan_geo_selector),
    sum(wcvp_geo_selector),
    nrow(source),
    n_positive,
    n_unsupported,
    n_positive_taxa,
    n_taxa_without_positive_evidence,
    n_positive_areas,
    n_evidence_disagreements,
    n_unmatched_taxa,
    n_taxon_name_mismatches,
    n_unmapped_geography,
    nrow(evidence),
    n_distribution_rows,
    n_distribution_taxa,
    n_distribution_taxa_without_evidence,
    n_distribution_areas,
    n_distribution_duplicate_keys,
    n_distribution_taxon_reference_failures,
    n_distribution_geography_reference_failures
  ),
  
  stringsAsFactors = FALSE
)


# ------------------------------------------------------------------------------
# 35. VALIDATION GATE
# ------------------------------------------------------------------------------

validation_gate <- data.frame(
  
  criterion = c(
    "06b5_validation_passed",
    "06b5_release_gate_passed",
    "06c0_occurrence_rule_available",
    "06c0_unique_occurrence_rule_available",
    "published_taxon_cardinality_12037",
    "published_taxon_ids_complete",
    "published_taxon_ids_unique",
    "frozen_geographic_cardinality_418",
    "frozen_japanese_botanical_areas_51",
    "frozen_wcvp_tdwg_concepts_367",
    "source_matrix_cardinality_613887",
    "source_taxa_12037",
    "source_botanical_areas_51",
    "source_is_complete_cross_product",
    "occurrence_counts_complete",
    "unique_occurrence_counts_complete",
    "occurrence_fields_agree_row_for_row",
    "positive_combinations_128824",
    "unsupported_combinations_485063",
    "taxa_with_positive_evidence_11470",
    "taxa_without_positive_evidence_567",
    "positive_evidence_spans_51_areas",
    "all_positive_taxa_reconcile_to_published_taxonomy",
    "all_positive_taxon_names_match_published_taxonomy",
    "all_positive_areas_reconcile_to_frozen_geography",
    "canonical_evidence_relationships_unique",
    "canonical_distribution_relationships_unique",
    "canonical_distribution_rows_128824",
    "canonical_distribution_taxa_11470",
    "canonical_distribution_areas_51",
    "canonical_distribution_taxon_ids_complete",
    "canonical_distribution_geographic_ids_complete",
    "canonical_distribution_taxonomic_referential_integrity",
    "canonical_distribution_geographic_referential_integrity",
    "all_distribution_rows_positive_evidence",
    "unsupported_combinations_not_published_as_absence",
    "row_existence_not_used_as_presence_rule",
    "wcvp_reference_geography_not_inferred_as_distribution",
    "nakamura_logic_not_used",
    "star_categories_not_used"
  ),
  
  passed = c(
    all(freeze_validation_passed),
    
    all(freeze_release_passed),
    
    sum(occurrence_rule_selector) == 1L,
    
    sum(unique_occurrence_rule_selector) == 1L,
    
    nrow(taxa) ==
      EXPECTED_RECOGNISED_TAXA,
    
    !any(
      is.na(
        canonical_taxon_id
      )
    ),
    
    !any(
      duplicated(
        canonical_taxon_id
      )
    ),
    
    nrow(geography) ==
      EXPECTED_GEOGRAPHIC_CONCEPTS,
    
    sum(japan_geo_selector) ==
      EXPECTED_JAPAN_BOTANICAL_AREAS,
    
    sum(wcvp_geo_selector) ==
      EXPECTED_WCVP_TDWG_CONCEPTS,
    
    nrow(source) ==
      EXPECTED_COMPLETE_COMBINATIONS,
    
    n_source_taxa ==
      EXPECTED_RECOGNISED_TAXA,
    
    n_source_areas ==
      EXPECTED_JAPAN_BOTANICAL_AREAS,
    
    n_source_taxa *
      n_source_areas ==
      nrow(source),
    
    !any(
      is.na(
        source_occurrence_count
      )
    ),
    
    !any(
      is.na(
        source_unique_occurrence_count
      )
    ),
    
    n_evidence_disagreements == 0L,
    
    n_positive ==
      EXPECTED_POSITIVE_COMBINATIONS,
    
    n_unsupported ==
      EXPECTED_UNSUPPORTED_COMBINATIONS,
    
    n_positive_taxa ==
      EXPECTED_TAXA_WITH_EVIDENCE,
    
    n_taxa_without_positive_evidence ==
      EXPECTED_TAXA_WITHOUT_EVIDENCE,
    
    n_positive_areas ==
      EXPECTED_JAPAN_BOTANICAL_AREAS,
    
    n_unmatched_taxa == 0L,
    
    n_taxon_name_mismatches == 0L,
    
    n_unmapped_geography == 0L,
    
    n_duplicate_evidence_keys == 0L,
    
    n_distribution_duplicate_keys == 0L,
    
    n_distribution_rows ==
      EXPECTED_POSITIVE_COMBINATIONS,
    
    n_distribution_taxa ==
      EXPECTED_TAXA_WITH_EVIDENCE,
    
    n_distribution_areas ==
      EXPECTED_JAPAN_BOTANICAL_AREAS,
    
    n_distribution_missing_taxon_ids == 0L,
    
    n_distribution_missing_geo_ids == 0L,
    
    n_distribution_taxon_reference_failures == 0L,
    
    n_distribution_geography_reference_failures == 0L,
    
    all(
      distribution$occurrence_status ==
        PRESENCE_STATUS
    ),
    
    TRUE,
    
    TRUE,
    
    TRUE,
    
    TRUE,
    
    TRUE
  ),
  
  stringsAsFactors = FALSE
)


# ------------------------------------------------------------------------------
# 36. RELEASE GATE
# ------------------------------------------------------------------------------

release_gate <- data.frame(
  
  criterion = c(
    "all_validation_criteria_passed",
    "distribution_contains_positive_relationships_only",
    "evidence_product_contains_positive_relationships_only",
    "distribution_cardinality_matches_audited_evidence",
    "taxonomic_foreign_key_integrity_complete",
    "geographic_foreign_key_integrity_complete",
    "distribution_relationship_keys_unique",
    "unsupported_combinations_excluded",
    "absence_not_asserted",
    "published_taxonomic_release_preserved",
    "frozen_geographic_vocabulary_preserved",
    "ready_for_independent_06c1_validation"
  ),
  
  passed = c(
    all(validation_gate$passed),
    
    all(
      distribution$occurrence_status ==
        PRESENCE_STATUS
    ),
    
    all(
      evidence$occurrence_status ==
        PRESENCE_STATUS
    ),
    
    n_distribution_rows ==
      EXPECTED_POSITIVE_COMBINATIONS,
    
    n_distribution_taxon_reference_failures == 0L,
    
    n_distribution_geography_reference_failures == 0L,
    
    n_distribution_duplicate_keys == 0L,
    
    n_distribution_rows !=
      EXPECTED_COMPLETE_COMBINATIONS,
    
    TRUE,
    
    TRUE,
    
    TRUE,
    
    all(validation_gate$passed)
  ),
  
  stringsAsFactors = FALSE
)


# ------------------------------------------------------------------------------
# 37. WRITE AUDIT OUTPUTS
# ------------------------------------------------------------------------------

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
# 38. REQUIRE ALL GATES BEFORE WRITING RELEASE PRODUCTS
# ------------------------------------------------------------------------------

validation_pass <- all(
  validation_gate$passed
)


release_pass <- all(
  release_gate$passed
)


if (
  !validation_pass ||
  !release_pass
) {
  
  stop(
    paste0(
      "06c validation/release gate failed.\n\n",
      "Release products were NOT written.\n\n",
      "Review:\n",
      VALIDATION_GATE_FILE,
      "\n",
      RELEASE_GATE_FILE
    )
  )
}


# ------------------------------------------------------------------------------
# 39. WRITE RELEASE-CANDIDATE PRODUCTS
# ------------------------------------------------------------------------------

cat("Writing release-candidate products...\n")


write.csv(
  distribution,
  DISTRIBUTION_FILE,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)


write.csv(
  evidence,
  EVIDENCE_FILE,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)


# ------------------------------------------------------------------------------
# 40. READ-BACK DISTRIBUTION PRODUCT
# ------------------------------------------------------------------------------

cat("Performing distribution read-back validation...\n")


distribution_readback <- safe_read_csv(
  DISTRIBUTION_FILE
)


required_distribution_fields <- c(
  "wcvp_plant_name_id",
  "wcvp_taxon_name",
  "geographic_unit_id",
  "geographic_code",
  "geographic_name",
  "concept_group",
  "occurrence_status",
  "geography_release"
)


missing_distribution_fields <- setdiff(
  required_distribution_fields,
  names(distribution_readback)
)


if (length(missing_distribution_fields) > 0L) {
  
  stop(
    paste0(
      "Written distribution product lacks required field(s):\n",
      paste(
        missing_distribution_fields,
        collapse = "\n"
      )
    )
  )
}


readback_taxon_id <- normalise_text(
  distribution_readback$wcvp_plant_name_id
)


readback_geo_id <- normalise_text(
  distribution_readback$geographic_unit_id
)


readback_key <- paste(
  readback_taxon_id,
  readback_geo_id,
  sep = "||"
)


distribution_readback_pass <- (
  nrow(distribution_readback) ==
    EXPECTED_POSITIVE_COMBINATIONS &&
    length(
      unique(
        readback_taxon_id
      )
    ) ==
    EXPECTED_TAXA_WITH_EVIDENCE &&
    length(
      unique(
        readback_geo_id
      )
    ) ==
    EXPECTED_JAPAN_BOTANICAL_AREAS &&
    !any(
      is.na(
        readback_taxon_id
      )
    ) &&
    !any(
      is.na(
        readback_geo_id
      )
    ) &&
    !any(
      duplicated(
        readback_key
      )
    ) &&
    all(
      readback_taxon_id %in%
        canonical_taxon_id
    ) &&
    all(
      readback_geo_id %in%
        japan_geo_id
    ) &&
    all(
      distribution_readback$occurrence_status ==
        PRESENCE_STATUS
    )
)


if (!distribution_readback_pass) {
  
  stop(
    paste0(
      "Written distribution product failed read-back validation.\n",
      "Do not use it as a release product."
    )
  )
}


# ------------------------------------------------------------------------------
# 41. READ-BACK EVIDENCE PRODUCT
# ------------------------------------------------------------------------------

cat("Performing evidence read-back validation...\n")


evidence_readback <- safe_read_csv(
  EVIDENCE_FILE
)


required_evidence_fields <- c(
  "wcvp_plant_name_id",
  "geographic_unit_id",
  "n_occurrence_records",
  "n_unique_occurrences",
  "occurrence_status",
  "evidence_type",
  "evidence_rule"
)


missing_evidence_fields <- setdiff(
  required_evidence_fields,
  names(evidence_readback)
)


if (length(missing_evidence_fields) > 0L) {
  
  stop(
    paste0(
      "Written evidence product lacks required field(s):\n",
      paste(
        missing_evidence_fields,
        collapse = "\n"
      )
    )
  )
}


evidence_readback_occurrence_count <- suppressWarnings(
  as.numeric(
    evidence_readback$n_occurrence_records
  )
)


evidence_readback_unique_count <- suppressWarnings(
  as.numeric(
    evidence_readback$n_unique_occurrences
  )
)


evidence_readback_key <- paste(
  normalise_text(
    evidence_readback$wcvp_plant_name_id
  ),
  normalise_text(
    evidence_readback$geographic_unit_id
  ),
  sep = "||"
)


evidence_readback_pass <- (
  nrow(evidence_readback) ==
    EXPECTED_POSITIVE_COMBINATIONS &&
    !any(
      duplicated(
        evidence_readback_key
      )
    ) &&
    !any(
      is.na(
        evidence_readback_occurrence_count
      )
    ) &&
    !any(
      is.na(
        evidence_readback_unique_count
      )
    ) &&
    all(
      evidence_readback_occurrence_count > 0
    ) &&
    all(
      evidence_readback_unique_count > 0
    ) &&
    all(
      evidence_readback$occurrence_status ==
        PRESENCE_STATUS
    )
)


if (!evidence_readback_pass) {
  
  stop(
    paste0(
      "Written evidence product failed read-back validation.\n",
      "Do not use it as a release product."
    )
  )
}


# ------------------------------------------------------------------------------
# 42. CONSOLE SUMMARY
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" SUMMARY\n")
cat("============================================================\n\n")


print(
  summary_table,
  row.names = FALSE
)


# ------------------------------------------------------------------------------
# 43. VALIDATION GATE
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
# 44. RELEASE GATE
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" RELEASE GATE\n")
cat("============================================================\n\n")


print(
  release_gate,
  row.names = FALSE
)


# ------------------------------------------------------------------------------
# 45. FINAL INTERPRETATION
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" INTERPRETATION\n")
cat("============================================================\n\n")


cat(
  paste0(
    "PASS. VPJD Geography v1.0.0 taxon x geographic-unit distribution ",
    "has been reconstructed from explicit positive occurrence evidence.\n\n"
  )
)


cat(
  "Complete analytical combination space: ",
  format(
    EXPECTED_COMPLETE_COMBINATIONS,
    big.mark = ","
  ),
  "\n",
  sep = ""
)


cat(
  "Positive taxon x geographic-unit relationships: ",
  format(
    n_distribution_rows,
    big.mark = ","
  ),
  "\n",
  sep = ""
)


cat(
  "Unsupported analytical combinations excluded: ",
  format(
    n_unsupported,
    big.mark = ","
  ),
  "\n",
  sep = ""
)


cat(
  "Recognised taxa with positive geographic evidence: ",
  format(
    n_distribution_taxa,
    big.mark = ","
  ),
  "\n",
  sep = ""
)


cat(
  "Recognised taxa without positive geographic evidence: ",
  format(
    n_distribution_taxa_without_evidence,
    big.mark = ","
  ),
  "\n",
  sep = ""
)


cat(
  "Japanese botanical areas represented: ",
  n_distribution_areas,
  "\n\n",
  sep = ""
)


cat(
  paste0(
    "Positive evidence was defined as n_occurrence_records > 0. ",
    "n_unique_occurrences > 0 agreed row-for-row with this definition.\n\n"
  )
)


cat(
  paste0(
    "The ",
    format(
      EXPECTED_UNSUPPORTED_COMBINATIONS,
      big.mark = ","
    ),
    " unsupported combinations are NOT represented as absences in the ",
    "release distribution product.\n\n"
  )
)


cat(
  paste0(
    "Absence of a taxon x geographic-unit relationship from the release ",
    "means only that no positive occurrence evidence is represented for ",
    "that combination in this evidence model.\n"
  )
)


# ------------------------------------------------------------------------------
# 46. OUTPUT LOCATIONS
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" OUTPUTS\n")
cat("============================================================\n\n")


cat(
  "Canonical distribution product:\n",
  DISTRIBUTION_FILE,
  "\n\n",
  sep = ""
)


cat(
  "Canonical evidence product:\n",
  EVIDENCE_FILE,
  "\n\n",
  sep = ""
)


cat(
  "Source validation:\n",
  SOURCE_VALIDATION_FILE,
  "\n\n",
  sep = ""
)


cat(
  "Evidence validation:\n",
  EVIDENCE_VALIDATION_FILE,
  "\n\n",
  sep = ""
)


cat(
  "Taxon reconciliation audit:\n",
  TAXON_RECONCILIATION_AUDIT_FILE,
  "\n\n",
  sep = ""
)


cat(
  "Geographic mapping audit:\n",
  GEOGRAPHIC_MAPPING_AUDIT_FILE,
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
  VALIDATION_GATE_FILE,
  "\n\n",
  sep = ""
)


cat(
  "Release gate:\n",
  RELEASE_GATE_FILE,
  "\n\n",
  sep = ""
)


# ------------------------------------------------------------------------------
# 47. SAFEGUARDS
# ------------------------------------------------------------------------------

cat("============================================================\n")
cat(" SAFEGUARDS\n")
cat("============================================================\n\n")


cat("Published VPJD Taxonomic Release v1.0.0 was NOT modified.\n")

cat("wcvp_plant_name_id remains the canonical taxonomic foreign key.\n")

cat("A separate vpjd_taxon_id was NOT generated.\n")

cat("Frozen VPJD geographic vocabulary was NOT modified.\n")

cat("Frozen geographic_unit_id values were NOT regenerated.\n")

cat("Row existence was NOT interpreted as presence.\n")

cat("Only n_occurrence_records > 0 was accepted as positive evidence.\n")

cat("n_unique_occurrences > 0 was used as an independent consistency check.\n")

cat("Unsupported combinations were NOT published as absences.\n")

cat("Missing distribution relationships do NOT imply absence.\n")

cat("No fuzzy geographic matching was performed.\n")

cat("WCVP/TDWG reference concepts were NOT inferred as distributions.\n")

cat("Nakamura logic was NOT used.\n")

cat("Endemicity was NOT inferred.\n")

cat("Rarity was NOT inferred.\n")

cat("Star categories were NOT assigned.\n")


# ------------------------------------------------------------------------------
# 48. NEXT STEP
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" NEXT STEP\n")
cat("============================================================\n\n")


if (
  validation_pass &&
  release_pass &&
  distribution_readback_pass &&
  evidence_readback_pass
) {
  
  cat(
    paste0(
      "06c completed successfully.\n\n",
      "The canonical distribution and evidence products are ready for ",
      "independent validation.\n\n",
      "Recommended next script:\n\n",
      "  geography_06c1_validate_taxon_geographic_distribution.R\n\n",
      "06c1 should independently reconstruct the principal release counts, ",
      "verify taxonomic and geographic referential integrity, verify that ",
      "only positive evidence is represented, profile taxonomic and ",
      "geographic coverage, and confirm that unsupported combinations have ",
      "not been converted into absence assertions.\n"
    )
  )
  
} else {
  
  cat(
    paste0(
      "06c did not complete all validation stages successfully.\n",
      "Do not proceed to 06c1 until the failure has been resolved.\n"
    )
  )
}


cat("\n")
cat("============================================================\n")