# ==============================================================================
# VPJD GEOGRAPHY MODULE
# geography_06h_assemble_zenodo_release_package.R
#
# PURPOSE
# -------
# Assemble and validate the final Zenodo deposit package for:
#
#   Vascular Plants of Japan Database (VPJD)
#   Geography Release v1.0.0
#
# Reserved DOI:
#
#   10.5281/zenodo.23111498
#
# Related Taxonomic Release:
#
#   VPJD Taxonomic Release v1.0.0
#   DOI: 10.5281/zenodo.23017356
#
# PREREQUISITE
# ------------
# geography_06g_finalise_zenodo_metadata.R
#
# Required inherited state:
#
#   DATA_RELEASE_READY   = TRUE
#   ZENODO_METADATA_READY = TRUE
#
# PRINCIPLE
# ---------
# This script performs PACKAGE ASSEMBLY ONLY.
#
# It:
#
#   1. validates the 06g readiness gate;
#   2. identifies the frozen scientific release products;
#   3. identifies the final administrative metadata products;
#   4. creates a clean Zenodo package directory;
#   5. copies files without transforming their contents;
#   6. calculates source and package SHA-256 hashes;
#   7. proves copied files are byte-identical to their sources;
#   8. creates a package manifest;
#   9. audits the assembled package;
#  10. returns ZENODO_PACKAGE_READY = TRUE only if every gate passes.
#
# THIS SCRIPT MUST NOT:
#
#   * modify VPJD Taxonomic Release v1.0.0;
#   * modify the canonical geographic vocabulary;
#   * modify taxon geographic distribution;
#   * modify taxon geographic evidence;
#   * repair identifiers;
#   * generate new taxon-area relationships;
#   * infer biological absence;
#   * infer endemicity;
#   * infer rarity;
#   * apply Nakamura Key-to-Stars logic;
#   * assign Star categories.
#
# This script deliberately avoids [[ indexing throughout.
#
# ==============================================================================


# ------------------------------------------------------------------------------
# 01. RELEASE CONSTANTS
# ------------------------------------------------------------------------------

RELEASE_VERSION <- "1.0.0"

RELEASE_NAME <- "VPJD Geography v1.0.0"

RELEASE_TITLE <- paste0(
  "Vascular Plants of Japan Database (VPJD): ",
  "Geography Release v",
  RELEASE_VERSION
)

SCRIPT_NAME <- "geography_06h_assemble_zenodo_release_package.R"

BUILD_DATE <- format(
  Sys.Date(),
  "%Y-%m-%d"
)

GEOGRAPHY_RELEASE_DOI <- "10.5281/zenodo.23111498"

TAXONOMIC_RELEASE_DOI <- "10.5281/zenodo.23017356"


# ------------------------------------------------------------------------------
# 02. PROJECT PATHS
# ------------------------------------------------------------------------------

PROJECT_ROOT <- "I:/R/OJPCP/VPJD-OJPCP"

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


# ------------------------------------------------------------------------------
# 03. FROZEN SCIENTIFIC RELEASE PATHS
# ------------------------------------------------------------------------------

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

PROVENANCE_FILE <- file.path(
  METADATA_ROOT,
  "vpjd_geography_provenance_v1.0.0.csv"
)

SAFEGUARDS_FILE <- file.path(
  METADATA_ROOT,
  "vpjd_geography_methodological_safeguards_v1.0.0.csv"
)


# ------------------------------------------------------------------------------
# 04. 06g STAGING PRODUCTS
# ------------------------------------------------------------------------------

STAGING_ROOT <- file.path(
  RELEASE_ROOT,
  "zenodo_staging"
)

STAGING_METADATA_ROOT <- file.path(
  STAGING_ROOT,
  "metadata"
)


ZENODO_METADATA_CSV <- file.path(
  STAGING_METADATA_ROOT,
  "vpjd_geography_zenodo_metadata_v1.0.0.csv"
)

ZENODO_METADATA_TXT <- file.path(
  STAGING_METADATA_ROOT,
  "vpjd_geography_zenodo_metadata_v1.0.0.txt"
)

README_FILE <- file.path(
  STAGING_ROOT,
  "README_VPJD_Geography_v1.0.0.txt"
)

RELATED_IDENTIFIERS_FILE <- file.path(
  STAGING_METADATA_ROOT,
  "vpjd_geography_related_identifiers_v1.0.0.csv"
)

CONTRIBUTORS_FILE <- file.path(
  STAGING_METADATA_ROOT,
  "vpjd_geography_contributors_v1.0.0.csv"
)

KEYWORDS_FILE <- file.path(
  STAGING_METADATA_ROOT,
  "vpjd_geography_keywords_v1.0.0.csv"
)


# ------------------------------------------------------------------------------
# 05. 06g READINESS GATE
# ------------------------------------------------------------------------------

AUDIT_06G_ROOT <- file.path(
  PROJECT_ROOT,
  "audit",
  "geography",
  "release_v1.0.0",
  "06g_finalise_zenodo_metadata"
)

READINESS_06G_FILE <- file.path(
  AUDIT_06G_ROOT,
  "geography_06g_zenodo_readiness.csv"
)


# ------------------------------------------------------------------------------
# 06. 06h OUTPUT PATHS
# ------------------------------------------------------------------------------

PACKAGE_ROOT <- file.path(
  RELEASE_ROOT,
  "zenodo_package_v1.0.0"
)

PACKAGE_DATA_ROOT <- file.path(
  PACKAGE_ROOT,
  "data"
)

PACKAGE_METADATA_ROOT <- file.path(
  PACKAGE_ROOT,
  "metadata"
)

AUDIT_06H_ROOT <- file.path(
  PROJECT_ROOT,
  "audit",
  "geography",
  "release_v1.0.0",
  "06h_assemble_zenodo_release_package"
)


PACKAGE_MANIFEST_FILE <- file.path(
  PACKAGE_ROOT,
  "vpjd_geography_package_manifest_v1.0.0.csv"
)

PACKAGE_CHECKSUM_FILE <- file.path(
  PACKAGE_ROOT,
  "vpjd_geography_package_sha256_v1.0.0.csv"
)

AUDIT_MANIFEST_FILE <- file.path(
  AUDIT_06H_ROOT,
  "geography_06h_package_manifest.csv"
)

COPY_VALIDATION_FILE <- file.path(
  AUDIT_06H_ROOT,
  "geography_06h_copy_validation.csv"
)

PACKAGE_AUDIT_FILE <- file.path(
  AUDIT_06H_ROOT,
  "geography_06h_package_audit.csv"
)

FINAL_STATUS_FILE <- file.path(
  AUDIT_06H_ROOT,
  "geography_06h_package_readiness.csv"
)

FINAL_REPORT_FILE <- file.path(
  AUDIT_06H_ROOT,
  "geography_06h_final_report.txt"
)


# ------------------------------------------------------------------------------
# 07. HELPER FUNCTIONS
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


to_logical <- function(x) {
  
  y <- toupper(
    normalise_text(x)
  )
  
  result <- y %in% c(
    "TRUE",
    "T",
    "1"
  )
  
  return(result)
}


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


calculate_sha256 <- function(path) {
  
  if (!file.exists(path)) {
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
# 08. START
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" VPJD GEOGRAPHY 06h - ASSEMBLE ZENODO RELEASE PACKAGE\n")
cat("============================================================\n\n")

cat(
  "Release:",
  RELEASE_NAME,
  "\n"
)

cat(
  "Build date:",
  BUILD_DATE,
  "\n"
)

cat(
  "Reserved Geography DOI:",
  GEOGRAPHY_RELEASE_DOI,
  "\n"
)

cat(
  "Taxonomic Release DOI:",
  TAXONOMIC_RELEASE_DOI,
  "\n\n"
)


# ------------------------------------------------------------------------------
# 09. VERIFY DIGEST PACKAGE
# ------------------------------------------------------------------------------

if (!requireNamespace(
  "digest",
  quietly = TRUE
)) {
  
  stop(
    paste(
      "Package 'digest' is required for SHA-256 verification.",
      "Install it before running 06h."
    )
  )
}


cat(
  "SHA-256 support: PASS\n\n"
)


# ------------------------------------------------------------------------------
# 10. VERIFY 06g READINESS GATE
# ------------------------------------------------------------------------------

cat(
  "Verifying 06g Zenodo-readiness gate...\n"
)


if (!file.exists(READINESS_06G_FILE)) {
  
  stop(
    paste0(
      "06g readiness file not found:\n",
      READINESS_06G_FILE,
      "\n\n",
      "Run geography_06g_finalise_zenodo_metadata.R first."
    )
  )
}


readiness_06g <- safe_read_csv(
  READINESS_06G_FILE
)


required_06g_fields <- c(
  "data_release_ready_06f",
  "required_metadata_complete",
  "optional_metadata_valid",
  "unresolved_placeholder_count",
  "staging_checksums_complete",
  "geography_doi_reserved",
  "zenodo_metadata_ready"
)


missing_06g_fields <- setdiff(
  required_06g_fields,
  names(readiness_06g)
)


if (length(missing_06g_fields) > 0L) {
  
  stop(
    paste(
      "06g readiness schema is incomplete.",
      "Missing fields:",
      paste(
        missing_06g_fields,
        collapse = ", "
      )
    )
  )
}


data_release_ready <- to_logical(
  readiness_06g$data_release_ready_06f[1]
)

required_metadata_complete <- to_logical(
  readiness_06g$required_metadata_complete[1]
)

optional_metadata_valid <- to_logical(
  readiness_06g$optional_metadata_valid[1]
)

unresolved_placeholder_count <- suppressWarnings(
  as.integer(
    readiness_06g$unresolved_placeholder_count[1]
  )
)

staging_checksums_complete <- to_logical(
  readiness_06g$staging_checksums_complete[1]
)

geography_doi_reserved <- to_logical(
  readiness_06g$geography_doi_reserved[1]
)

zenodo_metadata_ready <- to_logical(
  readiness_06g$zenodo_metadata_ready[1]
)


if (is.na(unresolved_placeholder_count)) {
  
  stop(
    "Unable to interpret unresolved_placeholder_count from 06g."
  )
}


if (!data_release_ready) {
  
  stop(
    "06h stopped: 06g reports DATA_RELEASE_READY = FALSE."
  )
}


if (!zenodo_metadata_ready) {
  
  stop(
    "06h stopped: 06g reports ZENODO_METADATA_READY = FALSE."
  )
}


if (unresolved_placeholder_count != 0L) {
  
  stop(
    paste0(
      "06h stopped: 06g reports ",
      unresolved_placeholder_count,
      " unresolved metadata placeholders."
    )
  )
}


cat(
  "06g DATA_RELEASE_READY: TRUE\n"
)

cat(
  "06g required metadata complete:",
  required_metadata_complete,
  "\n"
)

cat(
  "06g optional metadata valid:",
  optional_metadata_valid,
  "\n"
)

cat(
  "06g unresolved placeholders:",
  unresolved_placeholder_count,
  "\n"
)

cat(
  "06g staging checksums complete:",
  staging_checksums_complete,
  "\n"
)

cat(
  "06g Geography DOI reserved:",
  geography_doi_reserved,
  "\n"
)

cat(
  "06g ZENODO_METADATA_READY: TRUE\n\n"
)


# ------------------------------------------------------------------------------
# 11. DEFINE PACKAGE CONTENTS
# ------------------------------------------------------------------------------

source_files <- c(
  GEOGRAPHY_FILE,
  DISTRIBUTION_FILE,
  EVIDENCE_FILE,
  RELEASE_METADATA_FILE,
  DATA_DICTIONARY_FILE,
  RELEASE_STATISTICS_FILE,
  PROVENANCE_FILE,
  SAFEGUARDS_FILE,
  ZENODO_METADATA_CSV,
  ZENODO_METADATA_TXT,
  README_FILE,
  RELATED_IDENTIFIERS_FILE,
  CONTRIBUTORS_FILE,
  KEYWORDS_FILE
)


package_sections <- c(
  "data",
  "data",
  "data",
  "metadata",
  "metadata",
  "metadata",
  "metadata",
  "metadata",
  "metadata",
  "metadata",
  "root",
  "metadata",
  "metadata",
  "metadata"
)


source_classes <- c(
  "scientific_data",
  "scientific_data",
  "scientific_data",
  "scientific_metadata",
  "scientific_metadata",
  "scientific_metadata",
  "scientific_metadata",
  "scientific_metadata",
  "zenodo_metadata",
  "zenodo_metadata",
  "documentation",
  "zenodo_metadata",
  "zenodo_metadata",
  "zenodo_metadata"
)


if (
  length(source_files) != length(package_sections) ||
  length(source_files) != length(source_classes)
) {
  
  stop(
    "Internal 06h package-definition vectors have unequal lengths."
  )
}


# ------------------------------------------------------------------------------
# 12. VERIFY ALL SOURCE FILES
# ------------------------------------------------------------------------------

source_exists <- file.exists(
  source_files
)


if (!all(source_exists)) {
  
  missing_files <- source_files[
    !source_exists
  ]
  
  stop(
    paste(
      "One or more required release files are missing:",
      paste(
        missing_files,
        collapse = "\n"
      ),
      sep = "\n\n"
    )
  )
}


cat(
  "Required source files present:",
  length(source_files),
  "/",
  length(source_files),
  "\n\n",
  sep = ""
)


# ------------------------------------------------------------------------------
# 13. CLEAN PREVIOUS PACKAGE DIRECTORY
# ------------------------------------------------------------------------------

cat(
  "Preparing clean Zenodo package directory...\n"
)


if (dir.exists(PACKAGE_ROOT)) {
  
  unlink(
    PACKAGE_ROOT,
    recursive = TRUE,
    force = TRUE
  )
}


if (dir.exists(PACKAGE_ROOT)) {
  
  stop(
    paste0(
      "Unable to remove previous package directory:\n",
      PACKAGE_ROOT
    )
  )
}


dir.create(
  PACKAGE_ROOT,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  PACKAGE_DATA_ROOT,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  PACKAGE_METADATA_ROOT,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  AUDIT_06H_ROOT,
  recursive = TRUE,
  showWarnings = FALSE
)


cat(
  "Clean package directory created:\n",
  PACKAGE_ROOT,
  "\n\n"
)


# ------------------------------------------------------------------------------
# 14. BUILD PACKAGE DESTINATION PATHS
# ------------------------------------------------------------------------------

destination_files <- character(
  length(source_files)
)


for (file_index in seq_along(source_files)) {
  
  current_section <- package_sections[
    file_index
  ]
  
  current_basename <- basename(
    source_files[
      file_index
    ]
  )
  
  
  if (current_section == "data") {
    
    destination_files[
      file_index
    ] <- file.path(
      PACKAGE_DATA_ROOT,
      current_basename
    )
    
  } else if (current_section == "metadata") {
    
    destination_files[
      file_index
    ] <- file.path(
      PACKAGE_METADATA_ROOT,
      current_basename
    )
    
  } else if (current_section == "root") {
    
    destination_files[
      file_index
    ] <- file.path(
      PACKAGE_ROOT,
      current_basename
    )
    
  } else {
    
    stop(
      paste0(
        "Unknown package section: ",
        current_section
      )
    )
  }
}


# ------------------------------------------------------------------------------
# 15. CALCULATE SOURCE HASHES BEFORE COPY
# ------------------------------------------------------------------------------

cat(
  "Calculating source SHA-256 hashes...\n"
)


source_sha256 <- character(
  length(source_files)
)


for (file_index in seq_along(source_files)) {
  
  source_sha256[
    file_index
  ] <- calculate_sha256(
    source_files[
      file_index
    ]
  )
}


if (any(is.na(source_sha256))) {
  
  stop(
    "Unable to calculate SHA-256 for one or more source files."
  )
}


cat(
  "Source SHA-256 calculation: PASS\n\n"
)


# ------------------------------------------------------------------------------
# 16. COPY RELEASE PRODUCTS
# ------------------------------------------------------------------------------

cat(
  "Copying release products without transformation...\n"
)


copy_success <- logical(
  length(source_files)
)


for (file_index in seq_along(source_files)) {
  
  copy_success[
    file_index
  ] <- file.copy(
    from = source_files[
      file_index
    ],
    to = destination_files[
      file_index
    ],
    overwrite = TRUE,
    copy.mode = TRUE,
    copy.date = TRUE
  )
}


if (!all(copy_success)) {
  
  failed_copy_files <- source_files[
    !copy_success
  ]
  
  stop(
    paste(
      "One or more package files could not be copied:",
      paste(
        failed_copy_files,
        collapse = "\n"
      ),
      sep = "\n\n"
    )
  )
}


cat(
  "Files copied:",
  sum(copy_success),
  "/",
  length(copy_success),
  "\n\n",
  sep = ""
)


# ------------------------------------------------------------------------------
# 17. VERIFY DESTINATION FILES EXIST
# ------------------------------------------------------------------------------

destination_exists <- file.exists(
  destination_files
)


if (!all(destination_exists)) {
  
  missing_destination_files <- destination_files[
    !destination_exists
  ]
  
  stop(
    paste(
      "One or more copied package files are missing:",
      paste(
        missing_destination_files,
        collapse = "\n"
      ),
      sep = "\n\n"
    )
  )
}


# ------------------------------------------------------------------------------
# 18. CALCULATE PACKAGE HASHES
# ------------------------------------------------------------------------------

cat(
  "Calculating package SHA-256 hashes...\n"
)


package_sha256 <- character(
  length(destination_files)
)


for (file_index in seq_along(destination_files)) {
  
  package_sha256[
    file_index
  ] <- calculate_sha256(
    destination_files[
      file_index
    ]
  )
}


if (any(is.na(package_sha256))) {
  
  stop(
    "Unable to calculate SHA-256 for one or more package files."
  )
}


# ------------------------------------------------------------------------------
# 19. VERIFY BYTE-IDENTICAL COPIES
# ------------------------------------------------------------------------------

hash_match <- (
  source_sha256 == package_sha256
)


source_size <- file.info(
  source_files
)$size


package_size <- file.info(
  destination_files
)$size


size_match <- (
  source_size == package_size
)


copy_validation <- data.frame(
  file_name = basename(
    source_files
  ),
  source_class = source_classes,
  package_section = package_sections,
  source_path = source_files,
  package_path = destination_files,
  source_size_bytes = source_size,
  package_size_bytes = package_size,
  size_match = size_match,
  source_sha256 = source_sha256,
  package_sha256 = package_sha256,
  sha256_match = hash_match,
  copy_valid = (
    copy_success &
      destination_exists &
      size_match &
      hash_match
  ),
  stringsAsFactors = FALSE
)


write.csv(
  copy_validation,
  COPY_VALIDATION_FILE,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)


all_copies_valid <- all(
  copy_validation$copy_valid
)


if (!all_copies_valid) {
  
  failed_validation <- copy_validation$file_name[
    !copy_validation$copy_valid
  ]
  
  stop(
    paste(
      "Package integrity validation failed for:",
      paste(
        failed_validation,
        collapse = "\n"
      ),
      sep = "\n\n"
    )
  )
}


cat(
  "Source/package SHA-256 identity: PASS\n"
)

cat(
  "Source/package byte-size identity: PASS\n\n"
)


# ------------------------------------------------------------------------------
# 20. BUILD INITIAL PACKAGE MANIFEST
# ------------------------------------------------------------------------------

relative_package_path <- character(
  length(destination_files)
)


for (file_index in seq_along(destination_files)) {
  
  current_section <- package_sections[
    file_index
  ]
  
  current_name <- basename(
    destination_files[
      file_index
    ]
  )
  
  
  if (current_section == "root") {
    
    relative_package_path[
      file_index
    ] <- current_name
    
  } else {
    
    relative_package_path[
      file_index
    ] <- paste0(
      current_section,
      "/",
      current_name
    )
  }
}


package_manifest <- data.frame(
  release = rep(
    RELEASE_NAME,
    length(destination_files)
  ),
  doi = rep(
    GEOGRAPHY_RELEASE_DOI,
    length(destination_files)
  ),
  file_name = basename(
    destination_files
  ),
  relative_path = relative_package_path,
  package_section = package_sections,
  content_class = source_classes,
  size_bytes = package_size,
  sha256 = package_sha256,
  source_file = source_files,
  source_sha256 = source_sha256,
  source_identical = hash_match,
  stringsAsFactors = FALSE
)


write.csv(
  package_manifest,
  PACKAGE_MANIFEST_FILE,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)


write.csv(
  package_manifest,
  AUDIT_MANIFEST_FILE,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)


# ------------------------------------------------------------------------------
# 21. BUILD USER-FACING CHECKSUM TABLE
# ------------------------------------------------------------------------------

package_checksum_table <- data.frame(
  relative_path = relative_package_path,
  size_bytes = package_size,
  sha256 = package_sha256,
  stringsAsFactors = FALSE
)


write.csv(
  package_checksum_table,
  PACKAGE_CHECKSUM_FILE,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)


# ------------------------------------------------------------------------------
# 22. VERIFY PACKAGE CONTENT AFTER MANIFEST CREATION
# ------------------------------------------------------------------------------

expected_core_files <- basename(
  destination_files
)


actual_package_files <- list.files(
  PACKAGE_ROOT,
  recursive = TRUE,
  full.names = TRUE,
  all.files = FALSE,
  include.dirs = FALSE
)


actual_package_names <- basename(
  actual_package_files
)


expected_auxiliary_files <- c(
  basename(PACKAGE_MANIFEST_FILE),
  basename(PACKAGE_CHECKSUM_FILE)
)


expected_all_names <- c(
  expected_core_files,
  expected_auxiliary_files
)


missing_expected_names <- setdiff(
  expected_all_names,
  actual_package_names
)


unexpected_names <- setdiff(
  actual_package_names,
  expected_all_names
)


expected_files_complete <- (
  length(missing_expected_names) == 0L
)


no_unexpected_files <- (
  length(unexpected_names) == 0L
)


# ------------------------------------------------------------------------------
# 23. CHECK FOR ZERO-BYTE FILES
# ------------------------------------------------------------------------------

actual_file_info <- file.info(
  actual_package_files
)


zero_byte_selector <- (
  is.na(actual_file_info$size) |
    actual_file_info$size <= 0
)


zero_byte_files <- actual_package_files[
  zero_byte_selector
]


no_zero_byte_files <- (
  length(zero_byte_files) == 0L
)


# ------------------------------------------------------------------------------
# 24. VERIFY DOI VALUES IN FINAL METADATA
# ------------------------------------------------------------------------------

zenodo_metadata <- safe_read_csv(
  file.path(
    PACKAGE_METADATA_ROOT,
    basename(
      ZENODO_METADATA_CSV
    )
  )
)


metadata_schema_valid <- all(
  c(
    "field",
    "value"
  ) %in%
    names(zenodo_metadata)
)


if (!metadata_schema_valid) {
  
  stop(
    "Packaged Zenodo metadata does not contain field/value columns."
  )
}


geography_doi_rows <- which(
  zenodo_metadata$field ==
    "geography_release_doi"
)


taxonomic_doi_rows <- which(
  zenodo_metadata$field ==
    "related_taxonomic_release_doi"
)


geography_doi_metadata_valid <- FALSE

taxonomic_doi_metadata_valid <- FALSE


if (length(geography_doi_rows) == 1L) {
  
  geography_doi_metadata_valid <- (
    normalise_text(
      zenodo_metadata$value[
        geography_doi_rows
      ]
    ) ==
      GEOGRAPHY_RELEASE_DOI
  )
}


if (length(taxonomic_doi_rows) == 1L) {
  
  taxonomic_doi_metadata_valid <- (
    normalise_text(
      zenodo_metadata$value[
        taxonomic_doi_rows
      ]
    ) ==
      TAXONOMIC_RELEASE_DOI
  )
}


# ------------------------------------------------------------------------------
# 25. PACKAGE AUDIT TABLE
# ------------------------------------------------------------------------------

package_audit <- data.frame(
  check = c(
    "06g_data_release_ready",
    "06g_zenodo_metadata_ready",
    "06g_required_metadata_complete",
    "06g_optional_metadata_valid",
    "06g_no_unresolved_placeholders",
    "06g_staging_checksums_complete",
    "06g_geography_doi_reserved",
    "all_required_source_files_present",
    "all_source_hashes_calculated",
    "all_files_copied",
    "all_destination_files_present",
    "source_destination_sizes_identical",
    "source_destination_sha256_identical",
    "expected_package_files_complete",
    "no_unexpected_package_files",
    "no_zero_byte_package_files",
    "geography_doi_matches_final_metadata",
    "taxonomic_doi_matches_final_metadata"
  ),
  passed = c(
    data_release_ready,
    zenodo_metadata_ready,
    required_metadata_complete,
    optional_metadata_valid,
    unresolved_placeholder_count == 0L,
    staging_checksums_complete,
    geography_doi_reserved,
    all(source_exists),
    all(!is.na(source_sha256)),
    all(copy_success),
    all(destination_exists),
    all(size_match),
    all(hash_match),
    expected_files_complete,
    no_unexpected_files,
    no_zero_byte_files,
    geography_doi_metadata_valid,
    taxonomic_doi_metadata_valid
  ),
  stringsAsFactors = FALSE
)


write.csv(
  package_audit,
  PACKAGE_AUDIT_FILE,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)


# ------------------------------------------------------------------------------
# 26. FINAL PACKAGE GATE
# ------------------------------------------------------------------------------

ZENODO_PACKAGE_READY <- all(
  package_audit$passed
)


# ------------------------------------------------------------------------------
# 27. PACKAGE STATISTICS
# ------------------------------------------------------------------------------

core_file_count <- length(
  destination_files
)


total_package_file_count <- length(
  actual_package_files
)


total_package_size_bytes <- sum(
  actual_file_info$size,
  na.rm = TRUE
)


total_package_size_mb <- (
  total_package_size_bytes /
    1024^2
)


# ------------------------------------------------------------------------------
# 28. FINAL STATUS
# ------------------------------------------------------------------------------

final_status <- data.frame(
  release = RELEASE_NAME,
  build_date = BUILD_DATE,
  geography_release_doi = GEOGRAPHY_RELEASE_DOI,
  taxonomic_release_doi = TAXONOMIC_RELEASE_DOI,
  core_release_files = core_file_count,
  total_package_files = total_package_file_count,
  total_package_size_bytes = total_package_size_bytes,
  total_package_size_mb = round(
    total_package_size_mb,
    3
  ),
  source_copy_integrity = all_copies_valid,
  package_structure_valid = (
    expected_files_complete &&
      no_unexpected_files
  ),
  no_zero_byte_files = no_zero_byte_files,
  doi_metadata_valid = (
    geography_doi_metadata_valid &&
      taxonomic_doi_metadata_valid
  ),
  zenodo_package_ready = ZENODO_PACKAGE_READY,
  stringsAsFactors = FALSE
)


write.csv(
  final_status,
  FINAL_STATUS_FILE,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)


# ------------------------------------------------------------------------------
# 29. IDENTIFY FAILED CHECKS
# ------------------------------------------------------------------------------

failed_checks <- package_audit$check[
  !package_audit$passed
]


if (length(failed_checks) == 0L) {
  
  failed_checks_text <- "None"
  
} else {
  
  failed_checks_text <- paste(
    failed_checks,
    collapse = "; "
  )
}


# ------------------------------------------------------------------------------
# 30. FINAL REPORT
# ------------------------------------------------------------------------------

report_lines <- c(
  "VASCULAR PLANTS OF JAPAN DATABASE (VPJD)",
  "GEOGRAPHY RELEASE v1.0.0",
  "",
  "ZENODO RELEASE-PACKAGE ASSEMBLY REPORT",
  "",
  paste0(
    "Build date: ",
    BUILD_DATE
  ),
  paste0(
    "Script: ",
    SCRIPT_NAME
  ),
  paste0(
    "Geography Release DOI: ",
    GEOGRAPHY_RELEASE_DOI
  ),
  paste0(
    "Related Taxonomic Release DOI: ",
    TAXONOMIC_RELEASE_DOI
  ),
  "",
  "PACKAGE",
  "",
  paste0(
    "Package directory: ",
    PACKAGE_ROOT
  ),
  paste0(
    "Core release files: ",
    core_file_count
  ),
  paste0(
    "Total package files: ",
    total_package_file_count
  ),
  paste0(
    "Total package size (bytes): ",
    total_package_size_bytes
  ),
  paste0(
    "Total package size (MiB): ",
    round(
      total_package_size_mb,
      3
    )
  ),
  "",
  "INTEGRITY",
  "",
  paste0(
    "Source/copy SHA-256 identity: ",
    toupper(
      as.character(
        all(hash_match)
      )
    )
  ),
  paste0(
    "Source/copy byte-size identity: ",
    toupper(
      as.character(
        all(size_match)
      )
    )
  ),
  paste0(
    "Expected package files complete: ",
    toupper(
      as.character(
        expected_files_complete
      )
    )
  ),
  paste0(
    "No unexpected package files: ",
    toupper(
      as.character(
        no_unexpected_files
      )
    )
  ),
  paste0(
    "No zero-byte package files: ",
    toupper(
      as.character(
        no_zero_byte_files
      )
    )
  ),
  "",
  "IDENTIFIERS",
  "",
  paste0(
    "Geography DOI matches final metadata: ",
    toupper(
      as.character(
        geography_doi_metadata_valid
      )
    )
  ),
  paste0(
    "Taxonomic DOI matches final metadata: ",
    toupper(
      as.character(
        taxonomic_doi_metadata_valid
      )
    )
  ),
  "",
  "FAILED CHECKS",
  "",
  failed_checks_text,
  "",
  "FINAL STATUS",
  "",
  paste0(
    "ZENODO_PACKAGE_READY = ",
    toupper(
      as.character(
        ZENODO_PACKAGE_READY
      )
    )
  ),
  "",
  "SAFEGUARDS",
  "",
  "Package assembly copied source files without scientific transformation.",
  "VPJD Taxonomic Release v1.0.0 was not modified.",
  "Canonical geographic vocabulary was not modified.",
  "Canonical taxon geographic distribution was not modified.",
  "Canonical geographic evidence was not modified.",
  "No identifiers were repaired.",
  "No taxon-area relationships were generated or repaired.",
  "No biological absence was inferred.",
  "No endemicity was inferred.",
  "No rarity was inferred.",
  "No Nakamura logic was applied.",
  "No Star categories were assigned."
)


write_utf8_lines(
  report_lines,
  FINAL_REPORT_FILE
)


# ------------------------------------------------------------------------------
# 31. CONSOLE - COPY VALIDATION
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" SOURCE / PACKAGE COPY VALIDATION\n")
cat("============================================================\n\n")


print(
  copy_validation[
    ,
    c(
      "file_name",
      "source_class",
      "size_match",
      "sha256_match",
      "copy_valid"
    ),
    drop = FALSE
  ],
  row.names = FALSE
)


# ------------------------------------------------------------------------------
# 32. CONSOLE - PACKAGE AUDIT
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" PACKAGE AUDIT\n")
cat("============================================================\n\n")


print(
  package_audit,
  row.names = FALSE
)


# ------------------------------------------------------------------------------
# 33. CONSOLE - PACKAGE CONTENTS
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" FINAL PACKAGE CONTENTS\n")
cat("============================================================\n\n")


final_package_listing <- list.files(
  PACKAGE_ROOT,
  recursive = TRUE,
  full.names = FALSE,
  all.files = FALSE,
  include.dirs = FALSE
)


final_package_listing <- sort(
  final_package_listing
)


for (file_index in seq_along(final_package_listing)) {
  
  cat(
    final_package_listing[
      file_index
    ],
    "\n"
  )
}


# ------------------------------------------------------------------------------
# 34. CONSOLE - FINAL STATUS
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" FINAL ZENODO PACKAGE STATUS\n")
cat("============================================================\n\n")


print(
  final_status,
  row.names = FALSE
)


# ------------------------------------------------------------------------------
# 35. OUTPUT PATHS
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" 06h OUTPUTS\n")
cat("============================================================\n\n")


cat(
  "Zenodo package:\n",
  PACKAGE_ROOT,
  "\n\n"
)

cat(
  "Package manifest:\n",
  PACKAGE_MANIFEST_FILE,
  "\n\n"
)

cat(
  "Package checksums:\n",
  PACKAGE_CHECKSUM_FILE,
  "\n\n"
)

cat(
  "Copy validation:\n",
  COPY_VALIDATION_FILE,
  "\n\n"
)

cat(
  "Package audit:\n",
  PACKAGE_AUDIT_FILE,
  "\n\n"
)

cat(
  "Final status:\n",
  FINAL_STATUS_FILE,
  "\n\n"
)

cat(
  "Final report:\n",
  FINAL_REPORT_FILE,
  "\n"
)


# ------------------------------------------------------------------------------
# 36. SAFEGUARDS
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" SAFEGUARDS\n")
cat("============================================================\n\n")


cat("06h assembled the Zenodo package only.\n")
cat("Scientific release files were copied without transformation.\n")
cat("Source and destination SHA-256 hashes were compared.\n")
cat("Source and destination file sizes were compared.\n")
cat("VPJD Taxonomic Release v1.0.0 was not modified.\n")
cat("Canonical geographic vocabulary was not modified.\n")
cat("Canonical taxon geographic distribution was not modified.\n")
cat("Canonical geographic evidence was not modified.\n")
cat("No identifiers were repaired.\n")
cat("No taxon-area relationships were generated or repaired.\n")
cat("No biological absence was inferred.\n")
cat("No endemicity was inferred.\n")
cat("No rarity was inferred.\n")
cat("No Nakamura logic was applied.\n")
cat("No Star categories were assigned.\n")


# ------------------------------------------------------------------------------
# 37. FINAL INTERPRETATION
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" FINAL INTERPRETATION\n")
cat("============================================================\n\n")


cat(
  "ZENODO_PACKAGE_READY = ",
  toupper(
    as.character(
      ZENODO_PACKAGE_READY
    )
  ),
  "\n\n",
  sep = ""
)


if (ZENODO_PACKAGE_READY) {
  
  cat(
    paste(
      "The VPJD Geography v1.0.0 Zenodo release package has been",
      "assembled successfully and passed all package-integrity checks."
    ),
    "\n\n"
  )
  
  cat(
    paste(
      "The contents of the package directory can now be used for",
      "the Zenodo deposit associated with DOI",
      GEOGRAPHY_RELEASE_DOI
    ),
    "\n"
  )
  
} else {
  
  cat(
    paste(
      "The package is NOT ready for Zenodo deposit.",
      "Review the failed package-audit checks before proceeding."
    ),
    "\n\n"
  )
  
  
  if (length(failed_checks) > 0L) {
    
    cat(
      "Failed checks:\n\n"
    )
    
    for (check_index in seq_along(failed_checks)) {
      
      cat(
        " - ",
        failed_checks[
          check_index
        ],
        "\n",
        sep = ""
      )
    }
  }
}


cat("\n")
cat("============================================================\n")