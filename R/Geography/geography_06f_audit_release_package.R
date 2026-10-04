# ==============================================================================
# VPJD GEOGRAPHY MODULE
# geography_06f_audit_release_package.R
#
# PURPOSE
# -------
# Perform the final independent release-package audit for:
#
#   Vascular Plants of Japan Database (VPJD)
#   Geography Release v1.0.0
#
# This is a READ-ONLY audit.
#
# The script reads the frozen release products back from disk and checks:
#
#   1. required files;
#   2. inherited validation gates;
#   3. expected row counts;
#   4. required schemas;
#   5. canonical identifier uniqueness;
#   6. referential integrity;
#   7. taxon x geographic-unit relationship uniqueness;
#   8. evidence/distribution reconciliation;
#   9. summary reconciliation;
#  10. metadata completeness;
#  11. unresolved Zenodo placeholders;
#  12. SHA-256 checksums;
#  13. final data-release readiness;
#  14. final Zenodo-metadata readiness.
#
# IMPORTANT
# ---------
# This script MUST NOT repair, rewrite or modify any upstream release product.
#
# It writes audit outputs only.
#
# ==============================================================================


# ------------------------------------------------------------------------------
# 01. CONSTANTS
# ------------------------------------------------------------------------------

RELEASE_VERSION <- "1.0.0"

RELEASE_NAME <- "VPJD Geography v1.0.0"

SCRIPT_NAME <- "geography_06f_audit_release_package.R"

AUDIT_DATE <- format(
  Sys.Date(),
  "%Y-%m-%d"
)


EXPECTED_RECOGNISED_TAXA <- 12037L
EXPECTED_GEOGRAPHIC_CONCEPTS <- 418L
EXPECTED_JAPAN_BOTANICAL_AREAS <- 51L
EXPECTED_WCVP_TDWG_CONCEPTS <- 367L

EXPECTED_COMPLETE_COMBINATIONS <- 613887L
EXPECTED_POSITIVE_RELATIONSHIPS <- 128824L
EXPECTED_UNSUPPORTED_COMBINATIONS <- 485063L

EXPECTED_TAXA_WITH_EVIDENCE <- 11470L
EXPECTED_TAXA_WITHOUT_EVIDENCE <- 567L


# ------------------------------------------------------------------------------
# 02. PROJECT PATHS
# ------------------------------------------------------------------------------

PROJECT_ROOT <- "I:/R/OJPCP/VPJD-OJPCP"

PUBLISHED_VPJD_ROOT <- "I:/R/Data/VPJD_v1.0.0"

DATA_ROOT <- file.path(
  PROJECT_ROOT,
  "data"
)

GEOGRAPHY_ROOT <- file.path(
  DATA_ROOT,
  "derived",
  "geography"
)

RELEASE_ROOT <- file.path(
  GEOGRAPHY_ROOT,
  "release_v1.0.0"
)

GEOGRAPHIC_VOCABULARY_ROOT <- file.path(
  RELEASE_ROOT,
  "canonical_geographic_units_frozen"
)

DISTRIBUTION_ROOT <- file.path(
  RELEASE_ROOT,
  "taxon_geographic_distribution"
)

METADATA_ROOT <- file.path(
  RELEASE_ROOT,
  "metadata"
)

SUMMARY_ROOT <- file.path(
  RELEASE_ROOT,
  "summaries"
)

AUDIT_ROOT <- file.path(
  PROJECT_ROOT,
  "audit",
  "geography",
  "release_v1.0.0",
  "06f_audit_release_package"
)

dir.create(
  AUDIT_ROOT,
  recursive = TRUE,
  showWarnings = FALSE
)


# ------------------------------------------------------------------------------
# 03. CORE RELEASE FILES
# ------------------------------------------------------------------------------

GEOGRAPHY_FILE <- file.path(
  GEOGRAPHIC_VOCABULARY_ROOT,
  "vpjd_geographic_units_v1.0.0.csv"
)

DISTRIBUTION_FILE <- file.path(
  DISTRIBUTION_ROOT,
  "vpjd_taxon_geographic_distribution_v1.0.0.csv"
)

EVIDENCE_FILE <- file.path(
  DISTRIBUTION_ROOT,
  "vpjd_taxon_geographic_evidence_v1.0.0.csv"
)


# ------------------------------------------------------------------------------
# 04. METADATA FILES
# ------------------------------------------------------------------------------

RELEASE_METADATA_FILE <- file.path(
  METADATA_ROOT,
  "vpjd_geography_release_metadata_v1.0.0.csv"
)

DATA_DICTIONARY_FILE <- file.path(
  METADATA_ROOT,
  "vpjd_geography_data_dictionary_v1.0.0.csv"
)

RELEASE_STATISTICS_FILE <- file.path(
  METADATA_ROOT,
  "vpjd_geography_release_statistics_v1.0.0.csv"
)

RELEASE_MANIFEST_FILE <- file.path(
  METADATA_ROOT,
  "vpjd_geography_release_manifest_v1.0.0.csv"
)

PROVENANCE_FILE <- file.path(
  METADATA_ROOT,
  "vpjd_geography_provenance_v1.0.0.csv"
)

SAFEGUARDS_FILE <- file.path(
  METADATA_ROOT,
  "vpjd_geography_methodological_safeguards_v1.0.0.csv"
)

README_FILE <- file.path(
  METADATA_ROOT,
  "vpjd_geography_README_v1.0.0.txt"
)

ZENODO_METADATA_FILE <- file.path(
  METADATA_ROOT,
  "vpjd_geography_zenodo_metadata_v1.0.0.txt"
)


# ------------------------------------------------------------------------------
# 05. SUMMARY FILES
# ------------------------------------------------------------------------------

OVERVIEW_FILE <- file.path(
  SUMMARY_ROOT,
  "vpjd_geography_summary_overview_v1.0.0.csv"
)

TAXON_COVERAGE_FILE <- file.path(
  SUMMARY_ROOT,
  "vpjd_geography_taxon_coverage_v1.0.0.csv"
)

AREA_RICHNESS_FILE <- file.path(
  SUMMARY_ROOT,
  "vpjd_geography_area_richness_v1.0.0.csv"
)

COVERAGE_FREQUENCY_FILE <- file.path(
  SUMMARY_ROOT,
  "vpjd_geography_taxon_coverage_frequency_v1.0.0.csv"
)

EVIDENCE_SUPPORT_FILE <- file.path(
  SUMMARY_ROOT,
  "vpjd_geography_evidence_support_summary_v1.0.0.csv"
)

FAMILY_SUMMARY_FILE <- file.path(
  SUMMARY_ROOT,
  "vpjd_geography_family_summary_v1.0.0.csv"
)

GENUS_SUMMARY_FILE <- file.path(
  SUMMARY_ROOT,
  "vpjd_geography_genus_summary_v1.0.0.csv"
)

DESCRIPTIVE_STATISTICS_FILE <- file.path(
  SUMMARY_ROOT,
  "vpjd_geography_descriptive_statistics_v1.0.0.csv"
)

TEXT_SUMMARY_FILE <- file.path(
  SUMMARY_ROOT,
  "vpjd_geography_release_summary_v1.0.0.txt"
)


# ------------------------------------------------------------------------------
# 06. INHERITED VALIDATION GATES
# ------------------------------------------------------------------------------

METADATA_GATE_FILE <- file.path(
  PROJECT_ROOT,
  "audit",
  "geography",
  "release_v1.0.0",
  "06d_build_release_metadata",
  "geography_06d_metadata_gate.csv"
)

SUMMARY_GATE_FILE <- file.path(
  PROJECT_ROOT,
  "audit",
  "geography",
  "release_v1.0.0",
  "06e_build_release_summaries",
  "geography_06e_summary_gate.csv"
)


# ------------------------------------------------------------------------------
# 07. AUDIT OUTPUT FILES
# ------------------------------------------------------------------------------

FILE_INVENTORY_FILE <- file.path(
  AUDIT_ROOT,
  "geography_06f_file_inventory.csv"
)

SCHEMA_AUDIT_FILE <- file.path(
  AUDIT_ROOT,
  "geography_06f_schema_audit.csv"
)

REFERENTIAL_AUDIT_FILE <- file.path(
  AUDIT_ROOT,
  "geography_06f_referential_integrity.csv"
)

CHECKSUM_FILE <- file.path(
  AUDIT_ROOT,
  "geography_06f_sha256_checksums.csv"
)

PLACEHOLDER_AUDIT_FILE <- file.path(
  AUDIT_ROOT,
  "geography_06f_metadata_placeholders.csv"
)

FINAL_GATE_FILE <- file.path(
  AUDIT_ROOT,
  "geography_06f_final_release_gate.csv"
)

FINAL_STATUS_FILE <- file.path(
  AUDIT_ROOT,
  "geography_06f_release_status.csv"
)

AUDIT_REPORT_FILE <- file.path(
  AUDIT_ROOT,
  "geography_06f_final_audit_report.txt"
)


# ------------------------------------------------------------------------------
# 08. HELPER FUNCTIONS
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
  
  return(y)
}


safe_read_csv <- function(path) {
  
  if (!file.exists(path)) {
    
    stop(
      paste0(
        "Required input file not found:\n",
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
    error = function(e) NULL
  )
  
  if (is.null(result)) {
    
    result <- tryCatch(
      read.csv(
        path,
        stringsAsFactors = FALSE,
        check.names = FALSE,
        fileEncoding = "UTF-8-BOM"
      ),
      error = function(e) NULL
    )
  }
  
  if (is.null(result)) {
    
    result <- read.csv(
      path,
      stringsAsFactors = FALSE,
      check.names = FALSE,
      fileEncoding = "latin1"
    )
  }
  
  return(result)
}


safe_read_lines <- function(path) {
  
  if (!file.exists(path)) {
    return(character(0))
  }
  
  result <- tryCatch(
    readLines(
      path,
      warn = FALSE,
      encoding = "UTF-8"
    ),
    error = function(e) character(0)
  )
  
  result <- iconv(
    result,
    from = "",
    to = "UTF-8",
    sub = ""
  )
  
  return(result)
}


find_recognised_taxon_file <- function(root) {
  
  if (!dir.exists(root)) {
    
    stop(
      paste0(
        "VPJD Taxonomic Release root not found:\n",
        root
      )
    )
  }
  
  candidate_files <- list.files(
    root,
    pattern = "\\.csv$",
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = TRUE
  )
  
  candidate_names <- tolower(
    basename(candidate_files)
  )
  
  matches <- candidate_files[
    candidate_names == "vpjd_recognised_taxa.csv"
  ]
  
  if (length(matches) == 0L) {
    
    stop(
      paste0(
        "vpjd_recognised_taxa.csv not found beneath:\n",
        root
      )
    )
  }
  
  if (length(matches) > 1L) {
    
    sizes <- file.info(
      matches
    )$size
    
    matches <- matches[
      order(
        sizes,
        decreasing = TRUE
      )
    ]
  }
  
  return(matches[1L])
}


gate_passes <- function(path) {
  
  if (!file.exists(path)) {
    return(FALSE)
  }
  
  gate <- safe_read_csv(
    path
  )
  
  if (!all(c("criterion", "passed") %in% names(gate))) {
    return(FALSE)
  }
  
  values <- toupper(
    normalise_text(
      gate$passed
    )
  )
  
  if (length(values) == 0L) {
    return(FALSE)
  }
  
  result <- all(
    values %in% c(
      "TRUE",
      "T",
      "1"
    )
  )
  
  return(result)
}


required_fields_pass <- function(
    object,
    required_fields
) {
  
  result <- all(
    required_fields %in% names(object)
  )
  
  return(result)
}


count_missing_fields <- function(
    object,
    required_fields
) {
  
  result <- length(
    setdiff(
      required_fields,
      names(object)
    )
  )
  
  return(result)
}


duplicate_key_count <- function(x) {
  
  x <- normalise_text(
    x
  )
  
  x <- x[
    !is.na(x)
  ]
  
  result <- sum(
    duplicated(x)
  )
  
  return(result)
}


composite_key <- function(
    first_value,
    second_value
) {
  
  first_value <- normalise_text(
    first_value
  )
  
  second_value <- normalise_text(
    second_value
  )
  
  result <- paste(
    first_value,
    second_value,
    sep = "||"
  )
  
  return(result)
}


calculate_sha256 <- function(path) {
  
  if (!file.exists(path)) {
    return(NA_character_)
  }
  
  if (!requireNamespace("digest", quietly = TRUE)) {
    return(NA_character_)
  }
  
  result <- digest::digest(
    file = path,
    algo = "sha256",
    serialize = FALSE
  )
  
  return(result)
}


write_utf8_lines <- function(lines, path) {
  
  connection <- file(
    path,
    open = "w",
    encoding = "UTF-8"
  )
  
  writeLines(
    lines,
    connection,
    useBytes = TRUE
  )
  
  close(connection)
}


# ------------------------------------------------------------------------------
# 09. START
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" VPJD GEOGRAPHY 06f - FINAL RELEASE PACKAGE AUDIT\n")
cat("============================================================\n\n")

cat("Release:", RELEASE_NAME, "\n")
cat("Audit date:", AUDIT_DATE, "\n\n")

cat(
  "This is a read-only audit. No upstream release product will be modified.\n\n"
)


# ------------------------------------------------------------------------------
# 10. LOCATE TAXONOMIC INTERFACE
# ------------------------------------------------------------------------------

cat("Locating VPJD recognised-taxon table...\n")

RECOGNISED_TAXA_FILE <- find_recognised_taxon_file(
  PUBLISHED_VPJD_ROOT
)

cat(
  "Recognised-taxon table:",
  RECOGNISED_TAXA_FILE,
  "\n\n"
)


# ------------------------------------------------------------------------------
# 11. DEFINE REQUIRED RELEASE PACKAGE
# ------------------------------------------------------------------------------

required_files <- c(
  RECOGNISED_TAXA_FILE,
  GEOGRAPHY_FILE,
  DISTRIBUTION_FILE,
  EVIDENCE_FILE,
  RELEASE_METADATA_FILE,
  DATA_DICTIONARY_FILE,
  RELEASE_STATISTICS_FILE,
  RELEASE_MANIFEST_FILE,
  PROVENANCE_FILE,
  SAFEGUARDS_FILE,
  README_FILE,
  ZENODO_METADATA_FILE,
  OVERVIEW_FILE,
  TAXON_COVERAGE_FILE,
  AREA_RICHNESS_FILE,
  COVERAGE_FREQUENCY_FILE,
  EVIDENCE_SUPPORT_FILE,
  FAMILY_SUMMARY_FILE,
  GENUS_SUMMARY_FILE,
  DESCRIPTIVE_STATISTICS_FILE,
  TEXT_SUMMARY_FILE,
  METADATA_GATE_FILE,
  SUMMARY_GATE_FILE
)

required_roles <- c(
  "taxonomic_interface",
  "canonical_geographic_vocabulary",
  "canonical_taxon_geographic_distribution",
  "canonical_taxon_geographic_evidence",
  "release_metadata",
  "data_dictionary",
  "release_statistics",
  "release_manifest",
  "provenance",
  "methodological_safeguards",
  "readme",
  "zenodo_metadata_draft",
  "summary_overview",
  "taxon_coverage_summary",
  "area_richness_summary",
  "coverage_frequency_summary",
  "evidence_support_summary",
  "family_summary",
  "genus_summary",
  "descriptive_statistics",
  "text_release_summary",
  "06d_metadata_gate",
  "06e_summary_gate"
)


# ------------------------------------------------------------------------------
# 12. FILE INVENTORY
# ------------------------------------------------------------------------------

cat("Auditing required files...\n")

file_exists_values <- file.exists(
  required_files
)

file_size_values <- rep(
  NA_real_,
  length(required_files)
)

for (file_index in seq_along(required_files)) {
  
  current_file <- required_files[file_index]
  
  if (file.exists(current_file)) {
    
    file_size_values[file_index] <- file.info(
      current_file
    )$size
  }
}

file_inventory <- data.frame(
  file_role = required_roles,
  file_name = basename(required_files),
  file_path = required_files,
  exists = file_exists_values,
  size_bytes = file_size_values,
  non_empty = (
    file_exists_values &
      !is.na(file_size_values) &
      file_size_values > 0
  ),
  stringsAsFactors = FALSE
)

write.csv(
  file_inventory,
  FILE_INVENTORY_FILE,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)

all_required_files_exist <- all(
  file_inventory$exists
)

all_required_files_non_empty <- all(
  file_inventory$non_empty
)

if (!all_required_files_exist) {
  
  missing_files <- file_inventory$file_path[
    !file_inventory$exists
  ]
  
  stop(
    paste(
      "Required release-package files are missing:",
      paste(
        missing_files,
        collapse = "\n"
      ),
      sep = "\n\n"
    )
  )
}

cat("Required files: PASS\n\n")


# ------------------------------------------------------------------------------
# 13. VERIFY INHERITED GATES
# ------------------------------------------------------------------------------

metadata_gate_pass <- gate_passes(
  METADATA_GATE_FILE
)

summary_gate_pass <- gate_passes(
  SUMMARY_GATE_FILE
)

cat(
  "06d metadata gate:",
  ifelse(
    metadata_gate_pass,
    "PASS",
    "FAIL"
  ),
  "\n"
)

cat(
  "06e summary gate:",
  ifelse(
    summary_gate_pass,
    "PASS",
    "FAIL"
  ),
  "\n\n"
)


# ------------------------------------------------------------------------------
# 14. READ CORE RELEASE PRODUCTS
# ------------------------------------------------------------------------------

cat("Reading core release products from disk...\n")

taxa <- safe_read_csv(
  RECOGNISED_TAXA_FILE
)

geography <- safe_read_csv(
  GEOGRAPHY_FILE
)

distribution <- safe_read_csv(
  DISTRIBUTION_FILE
)

evidence <- safe_read_csv(
  EVIDENCE_FILE
)

release_metadata <- safe_read_csv(
  RELEASE_METADATA_FILE
)

release_statistics <- safe_read_csv(
  RELEASE_STATISTICS_FILE
)

taxon_coverage <- safe_read_csv(
  TAXON_COVERAGE_FILE
)

area_richness <- safe_read_csv(
  AREA_RICHNESS_FILE
)

coverage_frequency <- safe_read_csv(
  COVERAGE_FREQUENCY_FILE
)

family_summary <- safe_read_csv(
  FAMILY_SUMMARY_FILE
)

genus_summary <- safe_read_csv(
  GENUS_SUMMARY_FILE
)

cat("Release products read successfully.\n\n")


# ------------------------------------------------------------------------------
# 15. REQUIRED SCHEMAS
# ------------------------------------------------------------------------------

required_taxon_fields <- c(
  "wcvp_plant_name_id",
  "wcvp_taxon_name",
  "wcvp_taxon_authors",
  "wcvp_taxon_rank",
  "wcvp_taxon_status",
  "wcvp_family",
  "wcvp_genus",
  "wcvp_species",
  "vpjd_taxonomic_status"
)

required_geography_fields <- c(
  "geographic_unit_id",
  "geographic_code",
  "geographic_name",
  "concept_group",
  "identifier_status"
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

required_evidence_fields <- c(
  "wcvp_plant_name_id",
  "wcvp_taxon_name",
  "geographic_unit_id",
  "geographic_code",
  "geographic_name",
  "concept_group",
  "n_occurrence_records",
  "n_unique_occurrences",
  "occurrence_status",
  "evidence_type",
  "evidence_rule",
  "evidence_source_file",
  "evidence_source_row",
  "geography_release",
  "build_script",
  "build_date"
)

required_taxon_coverage_fields <- c(
  "wcvp_plant_name_id",
  "wcvp_taxon_name",
  "wcvp_family",
  "wcvp_genus",
  "n_botanical_areas",
  "n_occurrence_records",
  "n_unique_occurrences",
  "has_positive_geographic_evidence"
)

required_area_richness_fields <- c(
  "geographic_unit_id",
  "geographic_code",
  "geographic_name",
  "n_taxa",
  "n_occurrence_records",
  "n_unique_occurrences",
  "proportion_of_recognised_taxa"
)

required_coverage_frequency_fields <- c(
  "n_botanical_areas",
  "n_taxa",
  "proportion_of_taxa",
  "percentage_of_taxa"
)


# ------------------------------------------------------------------------------
# 16. SCHEMA AUDIT
# ------------------------------------------------------------------------------

schema_audit <- data.frame(
  product = c(
    "recognised_taxa",
    "geographic_vocabulary",
    "distribution",
    "evidence",
    "taxon_coverage",
    "area_richness",
    "coverage_frequency"
  ),
  required_field_count = c(
    length(required_taxon_fields),
    length(required_geography_fields),
    length(required_distribution_fields),
    length(required_evidence_fields),
    length(required_taxon_coverage_fields),
    length(required_area_richness_fields),
    length(required_coverage_frequency_fields)
  ),
  missing_field_count = c(
    count_missing_fields(
      taxa,
      required_taxon_fields
    ),
    count_missing_fields(
      geography,
      required_geography_fields
    ),
    count_missing_fields(
      distribution,
      required_distribution_fields
    ),
    count_missing_fields(
      evidence,
      required_evidence_fields
    ),
    count_missing_fields(
      taxon_coverage,
      required_taxon_coverage_fields
    ),
    count_missing_fields(
      area_richness,
      required_area_richness_fields
    ),
    count_missing_fields(
      coverage_frequency,
      required_coverage_frequency_fields
    )
  ),
  stringsAsFactors = FALSE
)

schema_audit$passed <- (
  schema_audit$missing_field_count == 0L
)

write.csv(
  schema_audit,
  SCHEMA_AUDIT_FILE,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)

schema_pass <- all(
  schema_audit$passed
)

if (!schema_pass) {
  
  stop(
    paste(
      "Required release schemas are incomplete.",
      "Review geography_06f_schema_audit.csv."
    )
  )
}

cat("Required schemas: PASS\n\n")


# ------------------------------------------------------------------------------
# 17. NORMALISE IDENTIFIERS
# ------------------------------------------------------------------------------

taxa$wcvp_plant_name_id <- normalise_text(
  taxa$wcvp_plant_name_id
)

geography$geographic_unit_id <- normalise_text(
  geography$geographic_unit_id
)

distribution$wcvp_plant_name_id <- normalise_text(
  distribution$wcvp_plant_name_id
)

distribution$geographic_unit_id <- normalise_text(
  distribution$geographic_unit_id
)

evidence$wcvp_plant_name_id <- normalise_text(
  evidence$wcvp_plant_name_id
)

evidence$geographic_unit_id <- normalise_text(
  evidence$geographic_unit_id
)


# ------------------------------------------------------------------------------
# 18. RECONSTRUCT CORE COUNTS
# ------------------------------------------------------------------------------

taxon_count <- nrow(
  taxa
)

geographic_concept_count <- nrow(
  geography
)

concept_group <- normalise_text(
  geography$concept_group
)

japan_selector <- (
  concept_group == "JAPAN_BOTANICAL_AREA"
)

japan_selector[is.na(japan_selector)] <- FALSE

wcvp_selector <- (
  concept_group == "WCVP_TDWG_LOCATION"
)

wcvp_selector[is.na(wcvp_selector)] <- FALSE

japan_area_count <- sum(
  japan_selector
)

wcvp_reference_count <- sum(
  wcvp_selector
)

distribution_count <- nrow(
  distribution
)

evidence_count <- nrow(
  evidence
)

represented_taxa <- unique(
  distribution$wcvp_plant_name_id
)

represented_taxa <- represented_taxa[
  !is.na(represented_taxa)
]

taxa_with_evidence_count <- length(
  represented_taxa
)

taxa_without_evidence_count <- (
  taxon_count -
    taxa_with_evidence_count
)

complete_combination_count <- (
  taxon_count *
    japan_area_count
)

unsupported_combination_count <- (
  complete_combination_count -
    distribution_count
)


# ------------------------------------------------------------------------------
# 19. IDENTIFIER UNIQUENESS
# ------------------------------------------------------------------------------

taxon_id_missing_count <- sum(
  is.na(
    taxa$wcvp_plant_name_id
  )
)

geographic_id_missing_count <- sum(
  is.na(
    geography$geographic_unit_id
  )
)

taxon_id_duplicate_count <- duplicate_key_count(
  taxa$wcvp_plant_name_id
)

geographic_id_duplicate_count <- duplicate_key_count(
  geography$geographic_unit_id
)

distribution_key <- composite_key(
  distribution$wcvp_plant_name_id,
  distribution$geographic_unit_id
)

evidence_key <- composite_key(
  evidence$wcvp_plant_name_id,
  evidence$geographic_unit_id
)

distribution_duplicate_relationships <- sum(
  duplicated(
    distribution_key
  )
)

evidence_duplicate_relationships <- sum(
  duplicated(
    evidence_key
  )
)


# ------------------------------------------------------------------------------
# 20. REFERENTIAL INTEGRITY
# ------------------------------------------------------------------------------

canonical_taxon_ids <- unique(
  taxa$wcvp_plant_name_id
)

canonical_taxon_ids <- canonical_taxon_ids[
  !is.na(canonical_taxon_ids)
]

canonical_geographic_ids <- unique(
  geography$geographic_unit_id
)

canonical_geographic_ids <- canonical_geographic_ids[
  !is.na(canonical_geographic_ids)
]

distribution_taxon_orphans <- sum(
  !distribution$wcvp_plant_name_id %in%
    canonical_taxon_ids
)

distribution_geography_orphans <- sum(
  !distribution$geographic_unit_id %in%
    canonical_geographic_ids
)

evidence_taxon_orphans <- sum(
  !evidence$wcvp_plant_name_id %in%
    canonical_taxon_ids
)

evidence_geography_orphans <- sum(
  !evidence$geographic_unit_id %in%
    canonical_geographic_ids
)


# ------------------------------------------------------------------------------
# 21. DISTRIBUTION / EVIDENCE KEY RECONCILIATION
# ------------------------------------------------------------------------------

distribution_unique_keys <- unique(
  distribution_key
)

evidence_unique_keys <- unique(
  evidence_key
)

distribution_without_evidence <- setdiff(
  distribution_unique_keys,
  evidence_unique_keys
)

evidence_without_distribution <- setdiff(
  evidence_unique_keys,
  distribution_unique_keys
)

distribution_without_evidence_count <- length(
  distribution_without_evidence
)

evidence_without_distribution_count <- length(
  evidence_without_distribution
)


# ------------------------------------------------------------------------------
# 22. JAPANESE-AREA SCOPE CHECK
# ------------------------------------------------------------------------------

japan_geographic_ids <- geography$geographic_unit_id[
  japan_selector
]

japan_geographic_ids <- unique(
  japan_geographic_ids
)

japan_geographic_ids <- japan_geographic_ids[
  !is.na(japan_geographic_ids)
]

distribution_non_japan_count <- sum(
  !distribution$geographic_unit_id %in%
    japan_geographic_ids
)

evidence_non_japan_count <- sum(
  !evidence$geographic_unit_id %in%
    japan_geographic_ids
)


# ------------------------------------------------------------------------------
# 23. POSITIVE-EVIDENCE SEMANTICS
# ------------------------------------------------------------------------------

evidence_occurrence_counts <- suppressWarnings(
  as.numeric(
    evidence$n_occurrence_records
  )
)

evidence_unique_counts <- suppressWarnings(
  as.numeric(
    evidence$n_unique_occurrences
  )
)

non_positive_occurrence_count <- sum(
  is.na(evidence_occurrence_counts) |
    evidence_occurrence_counts <= 0
)

negative_unique_occurrence_count <- sum(
  !is.na(evidence_unique_counts) &
    evidence_unique_counts < 0
)


# ------------------------------------------------------------------------------
# 24. REFERENTIAL AUDIT TABLE
# ------------------------------------------------------------------------------

referential_audit <- data.frame(
  criterion = c(
    "taxon_identifier_missing_count",
    "geographic_identifier_missing_count",
    "taxon_identifier_duplicate_count",
    "geographic_identifier_duplicate_count",
    "distribution_duplicate_relationship_count",
    "evidence_duplicate_relationship_count",
    "distribution_taxon_orphan_count",
    "distribution_geography_orphan_count",
    "evidence_taxon_orphan_count",
    "evidence_geography_orphan_count",
    "distribution_without_evidence_count",
    "evidence_without_distribution_count",
    "distribution_non_japan_relationship_count",
    "evidence_non_japan_relationship_count",
    "non_positive_occurrence_evidence_count",
    "negative_unique_occurrence_count"
  ),
  value = c(
    taxon_id_missing_count,
    geographic_id_missing_count,
    taxon_id_duplicate_count,
    geographic_id_duplicate_count,
    distribution_duplicate_relationships,
    evidence_duplicate_relationships,
    distribution_taxon_orphans,
    distribution_geography_orphans,
    evidence_taxon_orphans,
    evidence_geography_orphans,
    distribution_without_evidence_count,
    evidence_without_distribution_count,
    distribution_non_japan_count,
    evidence_non_japan_count,
    non_positive_occurrence_count,
    negative_unique_occurrence_count
  ),
  expected = rep(
    0L,
    16L
  ),
  stringsAsFactors = FALSE
)

referential_audit$passed <- (
  referential_audit$value ==
    referential_audit$expected
)

write.csv(
  referential_audit,
  REFERENTIAL_AUDIT_FILE,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)

referential_pass <- all(
  referential_audit$passed
)


# ------------------------------------------------------------------------------
# 25. RECONCILE 06e SUMMARY PRODUCTS
# ------------------------------------------------------------------------------

taxon_coverage_relationship_sum <- sum(
  suppressWarnings(
    as.numeric(
      taxon_coverage$n_botanical_areas
    )
  ),
  na.rm = TRUE
)

area_richness_relationship_sum <- sum(
  suppressWarnings(
    as.numeric(
      area_richness$n_taxa
    )
  ),
  na.rm = TRUE
)

coverage_frequency_taxon_sum <- sum(
  suppressWarnings(
    as.numeric(
      coverage_frequency$n_taxa
    )
  ),
  na.rm = TRUE
)

coverage_frequency_relationship_sum <- sum(
  suppressWarnings(
    as.numeric(
      coverage_frequency$n_botanical_areas
    )
  ) *
    suppressWarnings(
      as.numeric(
        coverage_frequency$n_taxa
      )
    ),
  na.rm = TRUE
)

family_taxon_sum <- sum(
  suppressWarnings(
    as.numeric(
      family_summary$n_recognised_taxa
    )
  ),
  na.rm = TRUE
)

genus_taxon_sum <- sum(
  suppressWarnings(
    as.numeric(
      genus_summary$n_recognised_taxa
    )
  ),
  na.rm = TRUE
)


# ------------------------------------------------------------------------------
# 26. METADATA PLACEHOLDER AUDIT
# ------------------------------------------------------------------------------

cat("Auditing unresolved release metadata...\n")

metadata_text <- c(
  safe_read_lines(
    README_FILE
  ),
  safe_read_lines(
    ZENODO_METADATA_FILE
  )
)

metadata_values <- normalise_text(
  release_metadata$value
)

metadata_values <- metadata_values[
  !is.na(metadata_values)
]

metadata_text <- c(
  metadata_text,
  metadata_values
)

placeholder_patterns <- c(
  "TO_CONFIRM",
  "TO CONFIRM",
  "TO_BE_ASSIGNED",
  "TO BE ASSIGNED",
  "TO_ADD",
  "TO ADD",
  "TO_BE_ADDED",
  "TO BE ADDED",
  "TO_BE_CONFIRMED",
  "TO BE CONFIRMED",
  "TO CONFIRM BEFORE",
  "TO ADD EXISTING"
)

placeholder_records <- list()

placeholder_counter <- 0L

for (pattern_index in seq_along(placeholder_patterns)) {
  
  current_pattern <- placeholder_patterns[pattern_index]
  
  hit_selector <- grepl(
    current_pattern,
    metadata_text,
    fixed = TRUE,
    ignore.case = TRUE
  )
  
  hit_selector[is.na(hit_selector)] <- FALSE
  
  hit_values <- metadata_text[
    hit_selector
  ]
  
  if (length(hit_values) > 0L) {
    
    for (hit_index in seq_along(hit_values)) {
      
      placeholder_counter <- placeholder_counter + 1L
      
      placeholder_records[[placeholder_counter]] <- data.frame(
        placeholder_pattern = current_pattern,
        matched_text = hit_values[hit_index],
        stringsAsFactors = FALSE
      )
    }
  }
}

if (length(placeholder_records) > 0L) {
  
  placeholder_audit <- do.call(
    rbind,
    placeholder_records
  )
  
  placeholder_audit <- unique(
    placeholder_audit
  )
  
  rownames(placeholder_audit) <- NULL
  
} else {
  
  placeholder_audit <- data.frame(
    placeholder_pattern = character(0),
    matched_text = character(0),
    stringsAsFactors = FALSE
  )
}

write.csv(
  placeholder_audit,
  PLACEHOLDER_AUDIT_FILE,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)

unresolved_placeholder_count <- nrow(
  placeholder_audit
)


# ------------------------------------------------------------------------------
# 27. SHA-256 CHECKSUMS
# ------------------------------------------------------------------------------

cat("Calculating SHA-256 checksums...\n")

digest_available <- requireNamespace(
  "digest",
  quietly = TRUE
)

checksum_values <- rep(
  NA_character_,
  length(required_files)
)

if (digest_available) {
  
  for (file_index in seq_along(required_files)) {
    
    checksum_values[file_index] <- calculate_sha256(
      required_files[file_index]
    )
  }
}

checksum_table <- data.frame(
  file_role = required_roles,
  file_name = basename(required_files),
  size_bytes = file_size_values,
  sha256 = checksum_values,
  stringsAsFactors = FALSE
)

write.csv(
  checksum_table,
  CHECKSUM_FILE,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)

checksum_complete <- (
  digest_available &&
    all(
      !is.na(
        checksum_table$sha256
      )
    )
)


# ------------------------------------------------------------------------------
# 28. FINAL DATA-RELEASE GATE
# ------------------------------------------------------------------------------

final_gate <- data.frame(
  criterion = c(
    "all_required_files_exist",
    "all_required_files_non_empty",
    "06d_metadata_gate_passed",
    "06e_summary_gate_passed",
    "all_required_schemas_present",
    "recognised_taxa_12037",
    "geographic_concepts_418",
    "japanese_botanical_areas_51",
    "wcvp_tdwg_reference_concepts_367",
    "possible_combinations_613887",
    "positive_relationships_128824",
    "evidence_rows_128824",
    "unsupported_combinations_485063",
    "taxa_with_positive_evidence_11470",
    "taxa_without_positive_evidence_567",
    "canonical_taxon_ids_complete",
    "canonical_taxon_ids_unique",
    "canonical_geographic_ids_complete",
    "canonical_geographic_ids_unique",
    "distribution_relationships_unique",
    "evidence_relationships_unique",
    "distribution_taxa_referentially_valid",
    "distribution_geography_referentially_valid",
    "evidence_taxa_referentially_valid",
    "evidence_geography_referentially_valid",
    "distribution_keys_all_have_evidence",
    "evidence_keys_all_have_distribution",
    "distribution_uses_japanese_areas_only",
    "evidence_uses_japanese_areas_only",
    "all_evidence_relationships_positive",
    "no_negative_unique_occurrence_counts",
    "taxon_coverage_rows_12037",
    "area_richness_rows_51",
    "taxon_coverage_reconciles_128824",
    "area_richness_reconciles_128824",
    "coverage_frequency_taxa_reconcile_12037",
    "coverage_frequency_relationships_reconcile_128824",
    "family_taxa_reconcile_12037",
    "genus_taxa_reconcile_12037",
    "sha256_checksums_complete"
  ),
  passed = c(
    all_required_files_exist,
    all_required_files_non_empty,
    metadata_gate_pass,
    summary_gate_pass,
    schema_pass,
    taxon_count == EXPECTED_RECOGNISED_TAXA,
    geographic_concept_count == EXPECTED_GEOGRAPHIC_CONCEPTS,
    japan_area_count == EXPECTED_JAPAN_BOTANICAL_AREAS,
    wcvp_reference_count == EXPECTED_WCVP_TDWG_CONCEPTS,
    complete_combination_count == EXPECTED_COMPLETE_COMBINATIONS,
    distribution_count == EXPECTED_POSITIVE_RELATIONSHIPS,
    evidence_count == EXPECTED_POSITIVE_RELATIONSHIPS,
    unsupported_combination_count == EXPECTED_UNSUPPORTED_COMBINATIONS,
    taxa_with_evidence_count == EXPECTED_TAXA_WITH_EVIDENCE,
    taxa_without_evidence_count == EXPECTED_TAXA_WITHOUT_EVIDENCE,
    taxon_id_missing_count == 0L,
    taxon_id_duplicate_count == 0L,
    geographic_id_missing_count == 0L,
    geographic_id_duplicate_count == 0L,
    distribution_duplicate_relationships == 0L,
    evidence_duplicate_relationships == 0L,
    distribution_taxon_orphans == 0L,
    distribution_geography_orphans == 0L,
    evidence_taxon_orphans == 0L,
    evidence_geography_orphans == 0L,
    distribution_without_evidence_count == 0L,
    evidence_without_distribution_count == 0L,
    distribution_non_japan_count == 0L,
    evidence_non_japan_count == 0L,
    non_positive_occurrence_count == 0L,
    negative_unique_occurrence_count == 0L,
    nrow(taxon_coverage) == EXPECTED_RECOGNISED_TAXA,
    nrow(area_richness) == EXPECTED_JAPAN_BOTANICAL_AREAS,
    taxon_coverage_relationship_sum == EXPECTED_POSITIVE_RELATIONSHIPS,
    area_richness_relationship_sum == EXPECTED_POSITIVE_RELATIONSHIPS,
    coverage_frequency_taxon_sum == EXPECTED_RECOGNISED_TAXA,
    coverage_frequency_relationship_sum == EXPECTED_POSITIVE_RELATIONSHIPS,
    family_taxon_sum == EXPECTED_RECOGNISED_TAXA,
    genus_taxon_sum == EXPECTED_RECOGNISED_TAXA,
    checksum_complete
  ),
  stringsAsFactors = FALSE
)

write.csv(
  final_gate,
  FINAL_GATE_FILE,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)

DATA_RELEASE_READY <- all(
  final_gate$passed
)


# ------------------------------------------------------------------------------
# 29. ZENODO METADATA READINESS
# ------------------------------------------------------------------------------

ZENODO_METADATA_READY <- (
  DATA_RELEASE_READY &&
    unresolved_placeholder_count == 0L
)


# ------------------------------------------------------------------------------
# 30. FINAL RELEASE STATUS
# ------------------------------------------------------------------------------

release_status <- data.frame(
  release = RELEASE_NAME,
  audit_date = AUDIT_DATE,
  data_release_ready = DATA_RELEASE_READY,
  zenodo_metadata_ready = ZENODO_METADATA_READY,
  unresolved_metadata_placeholders = unresolved_placeholder_count,
  sha256_complete = checksum_complete,
  inherited_06d_gate = metadata_gate_pass,
  inherited_06e_gate = summary_gate_pass,
  stringsAsFactors = FALSE
)

write.csv(
  release_status,
  FINAL_STATUS_FILE,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)


# ------------------------------------------------------------------------------
# 31. FINAL AUDIT REPORT
# ------------------------------------------------------------------------------

failed_criteria <- final_gate$criterion[
  !final_gate$passed
]

if (length(failed_criteria) == 0L) {
  failed_criteria_text <- "None"
} else {
  failed_criteria_text <- paste(
    failed_criteria,
    collapse = "; "
  )
}

audit_report_lines <- c(
  "VASCULAR PLANTS OF JAPAN DATABASE (VPJD)",
  "GEOGRAPHY RELEASE v1.0.0",
  "",
  "FINAL RELEASE-PACKAGE AUDIT",
  "",
  paste0(
    "Audit date: ",
    AUDIT_DATE
  ),
  paste0(
    "Audit script: ",
    SCRIPT_NAME
  ),
  "",
  "FINAL STATUS",
  "",
  paste0(
    "DATA_RELEASE_READY = ",
    toupper(
      as.character(
        DATA_RELEASE_READY
      )
    )
  ),
  paste0(
    "ZENODO_METADATA_READY = ",
    toupper(
      as.character(
        ZENODO_METADATA_READY
      )
    )
  ),
  "",
  "CORE RELEASE COUNTS",
  "",
  paste0(
    "Recognised taxa: ",
    format(
      taxon_count,
      big.mark = ","
    )
  ),
  paste0(
    "Geographic concepts: ",
    format(
      geographic_concept_count,
      big.mark = ","
    )
  ),
  paste0(
    "Japanese botanical areas: ",
    format(
      japan_area_count,
      big.mark = ","
    )
  ),
  paste0(
    "Positive taxon x area relationships: ",
    format(
      distribution_count,
      big.mark = ","
    )
  ),
  paste0(
    "Taxa with positive geographic evidence: ",
    format(
      taxa_with_evidence_count,
      big.mark = ","
    )
  ),
  paste0(
    "Taxa without positive geographic evidence: ",
    format(
      taxa_without_evidence_count,
      big.mark = ","
    )
  ),
  "",
  "REFERENTIAL INTEGRITY",
  "",
  paste0(
    "Taxon identifier duplicates: ",
    taxon_id_duplicate_count
  ),
  paste0(
    "Geographic identifier duplicates: ",
    geographic_id_duplicate_count
  ),
  paste0(
    "Distribution duplicate relationships: ",
    distribution_duplicate_relationships
  ),
  paste0(
    "Evidence duplicate relationships: ",
    evidence_duplicate_relationships
  ),
  paste0(
    "Distribution taxon orphans: ",
    distribution_taxon_orphans
  ),
  paste0(
    "Distribution geography orphans: ",
    distribution_geography_orphans
  ),
  paste0(
    "Evidence taxon orphans: ",
    evidence_taxon_orphans
  ),
  paste0(
    "Evidence geography orphans: ",
    evidence_geography_orphans
  ),
  paste0(
    "Distribution relationships without evidence: ",
    distribution_without_evidence_count
  ),
  paste0(
    "Evidence relationships without distribution: ",
    evidence_without_distribution_count
  ),
  "",
  "METADATA",
  "",
  paste0(
    "Unresolved metadata placeholders: ",
    unresolved_placeholder_count
  ),
  paste0(
    "SHA-256 checksum coverage complete: ",
    toupper(
      as.character(
        checksum_complete
      )
    )
  ),
  "",
  "FAILED DATA-RELEASE CRITERIA",
  "",
  failed_criteria_text,
  "",
  "INTERPRETATION",
  "",
  paste(
    "DATA_RELEASE_READY evaluates the structural and scientific",
    "integrity of the frozen VPJD Geography v1.0.0 package."
  ),
  "",
  paste(
    "ZENODO_METADATA_READY additionally requires all administrative",
    "metadata placeholders to have been resolved."
  ),
  "",
  paste(
    "A FALSE Zenodo-metadata status does not by itself indicate",
    "failure of the geographic data release."
  ),
  "",
  "SAFEGUARDS",
  "",
  "No upstream release product was modified.",
  "No missing relationship was interpreted as biological absence.",
  "No endemicity was inferred.",
  "No rarity was inferred.",
  "No Nakamura Key-to-Stars logic was applied.",
  "No Star category was assigned."
)

write_utf8_lines(
  audit_report_lines,
  AUDIT_REPORT_FILE
)


# ------------------------------------------------------------------------------
# 32. CONSOLE - FILE INVENTORY
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" FILE INVENTORY\n")
cat("============================================================\n\n")

print(
  file_inventory[
    ,
    c(
      "file_role",
      "file_name",
      "exists",
      "non_empty"
    ),
    drop = FALSE
  ],
  row.names = FALSE
)


# ------------------------------------------------------------------------------
# 33. CONSOLE - SCHEMA AUDIT
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" SCHEMA AUDIT\n")
cat("============================================================\n\n")

print(
  schema_audit,
  row.names = FALSE
)


# ------------------------------------------------------------------------------
# 34. CONSOLE - REFERENTIAL INTEGRITY
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" REFERENTIAL INTEGRITY\n")
cat("============================================================\n\n")

print(
  referential_audit,
  row.names = FALSE
)


# ------------------------------------------------------------------------------
# 35. CONSOLE - METADATA PLACEHOLDERS
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" METADATA PLACEHOLDERS\n")
cat("============================================================\n\n")

cat(
  "Unresolved placeholder records:",
  unresolved_placeholder_count,
  "\n\n"
)

if (unresolved_placeholder_count > 0L) {
  
  print(
    placeholder_audit,
    row.names = FALSE
  )
  
} else {
  
  cat(
    "No unresolved metadata placeholders detected.\n"
  )
}


# ------------------------------------------------------------------------------
# 36. CONSOLE - FINAL GATE
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" FINAL DATA-RELEASE GATE\n")
cat("============================================================\n\n")

print(
  final_gate,
  row.names = FALSE
)


# ------------------------------------------------------------------------------
# 37. CONSOLE - FINAL STATUS
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" FINAL RELEASE STATUS\n")
cat("============================================================\n\n")

print(
  release_status,
  row.names = FALSE
)


# ------------------------------------------------------------------------------
# 38. OUTPUT FILES
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" AUDIT OUTPUTS\n")
cat("============================================================\n\n")

audit_outputs <- c(
  FILE_INVENTORY_FILE,
  SCHEMA_AUDIT_FILE,
  REFERENTIAL_AUDIT_FILE,
  CHECKSUM_FILE,
  PLACEHOLDER_AUDIT_FILE,
  FINAL_GATE_FILE,
  FINAL_STATUS_FILE,
  AUDIT_REPORT_FILE
)

for (output_path in audit_outputs) {
  
  cat(
    output_path,
    "\n"
  )
}


# ------------------------------------------------------------------------------
# 39. SAFEGUARDS
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" SAFEGUARDS\n")
cat("============================================================\n\n")

cat("This audit was read-only with respect to the release package.\n")
cat("Canonical geographic vocabulary was not modified.\n")
cat("Canonical taxon distribution was not modified.\n")
cat("Canonical evidence table was not modified.\n")
cat("VPJD Taxonomic Release v1.0.0 was not modified.\n")
cat("No identifiers were generated or repaired.\n")
cat("No relationships were generated or repaired.\n")
cat("No biological absence was inferred.\n")
cat("No endemicity was inferred.\n")
cat("No rarity was inferred.\n")
cat("No Nakamura logic was applied.\n")
cat("No Star categories were assigned.\n")


# ------------------------------------------------------------------------------
# 40. FINAL INTERPRETATION
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" FINAL INTERPRETATION\n")
cat("============================================================\n\n")

if (DATA_RELEASE_READY) {
  
  cat(
    "DATA_RELEASE_READY = TRUE\n\n"
  )
  
  cat(
    paste(
      "The frozen VPJD Geography v1.0.0 data package has passed",
      "the final structural and scientific release audit."
    ),
    "\n\n"
  )
  
} else {
  
  cat(
    "DATA_RELEASE_READY = FALSE\n\n"
  )
  
  cat(
    paste(
      "One or more final data-release criteria failed.",
      "Review geography_06f_final_release_gate.csv before release."
    ),
    "\n\n"
  )
}


if (ZENODO_METADATA_READY) {
  
  cat(
    "ZENODO_METADATA_READY = TRUE\n\n"
  )
  
  cat(
    "No unresolved administrative metadata placeholders remain.\n"
  )
  
} else {
  
  cat(
    "ZENODO_METADATA_READY = FALSE\n\n"
  )
  
  cat(
    paste(
      "The data package may still be scientifically ready,",
      "but one or more Zenodo metadata placeholders remain unresolved."
    ),
    "\n"
  )
}


# ------------------------------------------------------------------------------
# 41. NEXT STEP
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" NEXT STEP\n")
cat("============================================================\n\n")

if (DATA_RELEASE_READY && ZENODO_METADATA_READY) {
  
  cat(
    "VPJD Geography v1.0.0 is ready for final Zenodo packaging and deposit.\n"
  )
  
} else if (DATA_RELEASE_READY && !ZENODO_METADATA_READY) {
  
  cat(
    "Resolve the remaining Zenodo metadata fields before deposit.\n"
  )
  
} else {
  
  cat(
    "Resolve failed data-release criteria before any Zenodo deposit.\n"
  )
}

cat("\n")
cat("============================================================\n")