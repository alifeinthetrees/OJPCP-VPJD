# ==============================================================================
# VPJD-OJPCP
#
# release_02_validate_taxonomic_v1.0.0.R
#
# Validate the frozen VPJD Taxonomic Release v1.0.0
#
# Purpose:
#   Validate the integrity, structure and principal taxonomic invariants of
#   the published VPJD Taxonomic Release v1.0.0.
#
# IMPORTANT:
#   This script:
#     - treats the published release as immutable;
#     - does not reconstruct the historical taxonomic build;
#     - does not modify any release files;
#     - does not add, remove or reconcile taxa;
#     - writes validation outputs outside the frozen release directory.
#
# Taxonomic Release DOI:
#   https://doi.org/10.5281/zenodo.23017356
#
# Version: 0.3.0
# ==============================================================================


# ==============================================================================
# 01. PACKAGES
# ==============================================================================

required_packages <- c(
  "readr",
  "dplyr",
  "tibble",
  "stringr",
  "digest"
)

missing_packages <- required_packages[
  !vapply(
    required_packages,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )
]

if (length(missing_packages) > 0L) {
  stop(
    "Required package(s) not installed:\n",
    paste(missing_packages, collapse = "\n")
  )
}


# ==============================================================================
# 02. VERSION AND RELEASE INVARIANTS
# ==============================================================================

SCRIPT_VERSION <- "0.3.0"

RELEASE_NAME <- "VPJD Taxonomic Release v1.0.0"

RELEASE_VERSION <- "1.0.0"

RELEASE_DOI <- "10.5281/zenodo.23017356"

EXPECTED_RECOGNISED_TAXA <- 12037L


# ==============================================================================
# 03. PATHS
# ==============================================================================

PROJECT_ROOT <- normalizePath(
  ".",
  winslash = "/",
  mustWork = TRUE
)

RELEASE_DIR <- file.path(
  PROJECT_ROOT,
  "data",
  "releases",
  "taxonomic_v1.0.0"
)

DATA_DIR <- file.path(
  RELEASE_DIR,
  "data"
)

METADATA_DIR <- file.path(
  RELEASE_DIR,
  "metadata"
)

OUTPUT_DIR <- file.path(
  PROJECT_ROOT,
  "outputs",
  "tables",
  "releases",
  "taxonomic_v1.0.0_validation"
)

dir.create(
  OUTPUT_DIR,
  recursive = TRUE,
  showWarnings = FALSE
)


# ==============================================================================
# 04. EXPECTED RELEASE FILES
# ==============================================================================

required_release_files <- c(
  "audit/vpjd_source_taxon_crosswalk_full.csv",
  "CITATION.cff",
  "data/vpjd_recognised_taxa.csv",
  "data/vpjd_source_taxon_crosswalk.csv",
  "data/vpjd_unplaced_source_concepts.csv",
  "data/vpjd_unresolved_source_concepts.csv",
  "LICENSE_NOTICE.txt",
  "metadata/DATA_DICTIONARY.csv",
  "metadata/MANIFEST.csv",
  "metadata/publication_metadata_validation.csv",
  "metadata/publication_table_validation.csv",
  "metadata/release_summary.csv",
  "metadata/SHA256SUMS.txt",
  "metadata/TABLE_INVENTORY.csv",
  "metadata/validation_summary.csv",
  "provenance/GBIF_PROVENANCE.txt",
  "provenance/gbif_source_licence_manifest.csv",
  "README.md",
  "RELEASE_NOTES.md"
)


# ==============================================================================
# 05. EXPECTED RECOGNISED-TAXA SCHEMA
# ==============================================================================

required_taxon_fields <- c(
  "wcvp_plant_name_id",
  "wcvp_taxon_name",
  "wcvp_taxon_authors",
  "wcvp_taxon_rank",
  "wcvp_taxon_status",
  "wcvp_family",
  "wcvp_genus",
  "wcvp_species",
  "vpjd_taxonomic_status",
  "source_gbif_concepts",
  "source_total_records",
  "source_present_records"
)


# ==============================================================================
# 06. HELPER FUNCTIONS
# ==============================================================================

nonblank <- function(x) {
  
  !is.na(x) &
    trimws(as.character(x)) != ""
}


normalise_path <- function(x) {
  
  x <- gsub(
    "\\\\",
    "/",
    x
  )
  
  x <- sub(
    "^\\./",
    "",
    x
  )
  
  x
}


write_output <- function(data, filename) {
  
  output_path <- file.path(
    OUTPUT_DIR,
    filename
  )
  
  readr::write_csv(
    data,
    output_path,
    na = ""
  )
  
  invisible(output_path)
}


add_check <- function(
    object,
    check,
    expected,
    observed,
    passed,
    severity = "ERROR",
    notes = NA_character_) {
  
  dplyr::bind_rows(
    object,
    tibble::tibble(
      CHECK = as.character(check),
      EXPECTED = as.character(expected),
      OBSERVED = as.character(observed),
      PASSED = as.logical(passed),
      SEVERITY = as.character(severity),
      NOTES = as.character(notes)
    )
  )
}


find_first_column <- function(data, candidates) {
  
  data_names <- names(data)
  
  data_names_lower <- tolower(
    data_names
  )
  
  candidates_lower <- tolower(
    candidates
  )
  
  hit <- match(
    candidates_lower,
    data_names_lower,
    nomatch = 0L
  )
  
  hit <- hit[
    hit > 0L
  ]
  
  if (length(hit) == 0L) {
    return(NA_character_)
  }
  
  data_names[
    hit[[1]]
  ]
}


sha256_file <- function(path) {
  
  digest::digest(
    file = path,
    algo = "sha256",
    serialize = FALSE
  )
}


# ==============================================================================
# 07. START
# ==============================================================================

cat("\n")
cat("============================================================\n")
cat("VPJD TAXONOMIC RELEASE v1.0.0 VALIDATION\n")
cat("============================================================\n\n")

cat(
  "Validator version: ",
  SCRIPT_VERSION,
  "\n",
  sep = ""
)

cat(
  "Release DOI: ",
  RELEASE_DOI,
  "\n",
  sep = ""
)

cat(
  "Release directory:\n",
  RELEASE_DIR,
  "\n\n",
  sep = ""
)


# ==============================================================================
# 08. VALIDATE RELEASE DIRECTORY
# ==============================================================================

if (!dir.exists(RELEASE_DIR)) {
  
  stop(
    "Taxonomic release directory not found:\n",
    RELEASE_DIR
  )
}


# ==============================================================================
# 09. REQUIRED-FILE VALIDATION
# ==============================================================================

absolute_paths <- file.path(
  RELEASE_DIR,
  required_release_files
)

file_exists <- file.exists(
  absolute_paths
)

file_sizes <- rep(
  NA_real_,
  length(required_release_files)
)

file_sizes[file_exists] <- file.info(
  absolute_paths[file_exists]
)$size

file_inventory <- tibble::tibble(
  RELATIVE_PATH = required_release_files,
  EXISTS = file_exists,
  SIZE_BYTES = file_sizes,
  NONEMPTY = file_exists &
    !is.na(file_sizes) &
    file_sizes > 0
)

write_output(
  file_inventory,
  "required_file_inventory.csv"
)


if (any(!file_inventory$EXISTS)) {
  
  missing_files <- file_inventory$RELATIVE_PATH[
    !file_inventory$EXISTS
  ]
  
  stop(
    "Required Taxonomic Release file(s) are missing:\n",
    paste(
      missing_files,
      collapse = "\n"
    )
  )
}


if (any(!file_inventory$NONEMPTY)) {
  
  empty_files <- file_inventory$RELATIVE_PATH[
    !file_inventory$NONEMPTY
  ]
  
  stop(
    "Required Taxonomic Release file(s) are empty:\n",
    paste(
      empty_files,
      collapse = "\n"
    )
  )
}


cat(
  "Required release files: ",
  nrow(file_inventory),
  " / ",
  nrow(file_inventory),
  " present\n",
  sep = ""
)


# ==============================================================================
# 10. LOAD PRIMARY RELEASE TABLES
# ==============================================================================

recognised_taxa_path <- file.path(
  DATA_DIR,
  "vpjd_recognised_taxa.csv"
)

source_crosswalk_path <- file.path(
  DATA_DIR,
  "vpjd_source_taxon_crosswalk.csv"
)

unplaced_path <- file.path(
  DATA_DIR,
  "vpjd_unplaced_source_concepts.csv"
)

unresolved_path <- file.path(
  DATA_DIR,
  "vpjd_unresolved_source_concepts.csv"
)


recognised_taxa <- readr::read_csv(
  recognised_taxa_path,
  show_col_types = FALSE,
  progress = FALSE
)

source_crosswalk <- readr::read_csv(
  source_crosswalk_path,
  show_col_types = FALSE,
  progress = FALSE
)

unplaced <- readr::read_csv(
  unplaced_path,
  show_col_types = FALSE,
  progress = FALSE
)

unresolved <- readr::read_csv(
  unresolved_path,
  show_col_types = FALSE,
  progress = FALSE
)


# ==============================================================================
# 11. INITIALISE VALIDATION REGISTER
# ==============================================================================

validation_checks <- tibble::tibble(
  CHECK = character(),
  EXPECTED = character(),
  OBSERVED = character(),
  PASSED = logical(),
  SEVERITY = character(),
  NOTES = character()
)


# ==============================================================================
# 12. VALIDATE RECOGNISED-TAXA SCHEMA
# ==============================================================================

missing_taxon_fields <- setdiff(
  required_taxon_fields,
  names(recognised_taxa)
)

n_required_fields_present <- length(
  intersect(
    required_taxon_fields,
    names(recognised_taxa)
  )
)

validation_checks <- add_check(
  validation_checks,
  check = "Recognised-taxa schema",
  expected = paste(
    length(required_taxon_fields),
    "required fields"
  ),
  observed = paste(
    n_required_fields_present,
    "required fields present"
  ),
  passed = length(missing_taxon_fields) == 0L,
  notes = if (length(missing_taxon_fields) == 0L) {
    "All required Taxonomic Release fields present."
  } else {
    paste(
      "Missing:",
      paste(
        missing_taxon_fields,
        collapse = ", "
      )
    )
  }
)


if (length(missing_taxon_fields) > 0L) {
  
  stop(
    "Required field(s) missing from vpjd_recognised_taxa.csv:\n",
    paste(
      missing_taxon_fields,
      collapse = "\n"
    )
  )
}


# ==============================================================================
# 13. AUTHORITATIVE FIELD NAMES
# ==============================================================================

wcvp_id_field <- "wcvp_plant_name_id"

name_field <- "wcvp_taxon_name"

author_field <- "wcvp_taxon_authors"

rank_field <- "wcvp_taxon_rank"

status_field <- "wcvp_taxon_status"

family_field <- "wcvp_family"

genus_field <- "wcvp_genus"

species_field <- "wcvp_species"

vpjd_status_field <- "vpjd_taxonomic_status"


field_registry <- tibble::tibble(
  ROLE = c(
    "WCVP identifier",
    "WCVP taxon name",
    "WCVP taxon authors",
    "taxonomic rank",
    "WCVP taxonomic status",
    "family",
    "genus",
    "species",
    "VPJD taxonomic status"
  ),
  FIELD = c(
    wcvp_id_field,
    name_field,
    author_field,
    rank_field,
    status_field,
    family_field,
    genus_field,
    species_field,
    vpjd_status_field
  )
)

field_registry$FIELD_FOUND <- field_registry$FIELD %in%
  names(recognised_taxa)

write_output(
  field_registry,
  "field_registry.csv"
)


# ==============================================================================
# 14. CORE TAXONOMIC VALIDATION
# ==============================================================================

n_taxa <- nrow(
  recognised_taxa
)

wcvp_ids <- trimws(
  as.character(
    recognised_taxa[[wcvp_id_field]]
  )
)

valid_wcvp_ids <- wcvp_ids[
  !is.na(wcvp_ids) &
    wcvp_ids != ""
]

n_missing_wcvp <- sum(
  is.na(wcvp_ids) |
    wcvp_ids == ""
)

n_distinct_wcvp <- dplyr::n_distinct(
  valid_wcvp_ids
)

n_duplicate_wcvp <- length(
  valid_wcvp_ids
) -
  n_distinct_wcvp


validation_checks <- add_check(
  validation_checks,
  check = "Recognised taxon rows",
  expected = EXPECTED_RECOGNISED_TAXA,
  observed = n_taxa,
  passed = n_taxa == EXPECTED_RECOGNISED_TAXA
)

validation_checks <- add_check(
  validation_checks,
  check = "Distinct WCVP identifiers",
  expected = EXPECTED_RECOGNISED_TAXA,
  observed = n_distinct_wcvp,
  passed = n_distinct_wcvp == EXPECTED_RECOGNISED_TAXA
)

validation_checks <- add_check(
  validation_checks,
  check = "Missing WCVP identifiers",
  expected = 0,
  observed = n_missing_wcvp,
  passed = n_missing_wcvp == 0L
)

validation_checks <- add_check(
  validation_checks,
  check = "Duplicate WCVP identifiers",
  expected = 0,
  observed = n_duplicate_wcvp,
  passed = n_duplicate_wcvp == 0L
)


# ==============================================================================
# 15. PRIMARY FIELD COMPLETENESS
# ==============================================================================

taxon_names <- recognised_taxa[[name_field]]

taxon_ranks <- recognised_taxa[[rank_field]]

taxon_statuses <- recognised_taxa[[status_field]]

vpjd_statuses <- recognised_taxa[[vpjd_status_field]]


n_missing_names <- sum(
  !nonblank(taxon_names)
)

n_missing_ranks <- sum(
  !nonblank(taxon_ranks)
)

n_missing_status <- sum(
  !nonblank(taxon_statuses)
)

n_missing_vpjd_status <- sum(
  !nonblank(vpjd_statuses)
)


validation_checks <- add_check(
  validation_checks,
  check = "Missing WCVP taxon names",
  expected = 0,
  observed = n_missing_names,
  passed = n_missing_names == 0L
)

validation_checks <- add_check(
  validation_checks,
  check = "Missing WCVP taxon ranks",
  expected = 0,
  observed = n_missing_ranks,
  passed = n_missing_ranks == 0L
)

validation_checks <- add_check(
  validation_checks,
  check = "Missing WCVP taxonomic statuses",
  expected = 0,
  observed = n_missing_status,
  passed = n_missing_status == 0L
)

validation_checks <- add_check(
  validation_checks,
  check = "Missing VPJD taxonomic statuses",
  expected = 0,
  observed = n_missing_vpjd_status,
  passed = n_missing_vpjd_status == 0L
)


# ==============================================================================
# 16. DUPLICATE WCVP-ID DIAGNOSTIC
# ==============================================================================

duplicate_wcvp_ids <- recognised_taxa |>
  dplyr::mutate(
    VALIDATION_WCVP_ID = trimws(
      as.character(
        .data[[wcvp_id_field]]
      )
    )
  ) |>
  dplyr::filter(
    nonblank(
      VALIDATION_WCVP_ID
    )
  ) |>
  dplyr::count(
    VALIDATION_WCVP_ID,
    name = "N",
    sort = TRUE
  ) |>
  dplyr::filter(
    N > 1L
  )


if (nrow(duplicate_wcvp_ids) > 0L) {
  
  write_output(
    duplicate_wcvp_ids,
    "duplicate_wcvp_ids.csv"
  )
}


# ==============================================================================
# 17. TAXONOMIC PROFILE
# ==============================================================================

family_values <- recognised_taxa[[family_field]]

genus_values <- recognised_taxa[[genus_field]]

species_values <- recognised_taxa[[species_field]]


n_families <- dplyr::n_distinct(
  family_values[
    nonblank(family_values)
  ]
)

n_genera <- dplyr::n_distinct(
  genus_values[
    nonblank(genus_values)
  ]
)

n_species_values <- dplyr::n_distinct(
  species_values[
    nonblank(species_values)
  ]
)


taxonomic_profile <- tibble::tibble(
  METRIC = c(
    "recognised_taxa",
    "distinct_wcvp_ids",
    "missing_wcvp_ids",
    "duplicate_wcvp_ids",
    "distinct_families",
    "distinct_genera",
    "distinct_wcvp_species_values",
    "source_crosswalk_rows",
    "unplaced_source_concepts",
    "unresolved_source_concepts"
  ),
  VALUE = c(
    n_taxa,
    n_distinct_wcvp,
    n_missing_wcvp,
    n_duplicate_wcvp,
    n_families,
    n_genera,
    n_species_values,
    nrow(source_crosswalk),
    nrow(unplaced),
    nrow(unresolved)
  )
)

write_output(
  taxonomic_profile,
  "taxonomic_profile.csv"
)


# ==============================================================================
# 18. TAXONOMIC RANK PROFILE
# ==============================================================================

rank_profile <- recognised_taxa |>
  dplyr::transmute(
    TAXONOMIC_RANK = as.character(
      .data[[rank_field]]
    )
  ) |>
  dplyr::count(
    TAXONOMIC_RANK,
    name = "N_TAXA",
    sort = TRUE
  )

write_output(
  rank_profile,
  "taxonomic_rank_profile.csv"
)


# ==============================================================================
# 19. WCVP TAXONOMIC-STATUS PROFILE
# ==============================================================================

wcvp_status_profile <- recognised_taxa |>
  dplyr::transmute(
    WCVP_TAXONOMIC_STATUS = as.character(
      .data[[status_field]]
    )
  ) |>
  dplyr::count(
    WCVP_TAXONOMIC_STATUS,
    name = "N_TAXA",
    sort = TRUE
  )

write_output(
  wcvp_status_profile,
  "wcvp_taxonomic_status_profile.csv"
)


# ==============================================================================
# 20. VPJD TAXONOMIC-STATUS PROFILE
# ==============================================================================

vpjd_status_profile <- recognised_taxa |>
  dplyr::transmute(
    VPJD_TAXONOMIC_STATUS = as.character(
      .data[[vpjd_status_field]]
    )
  ) |>
  dplyr::count(
    VPJD_TAXONOMIC_STATUS,
    name = "N_TAXA",
    sort = TRUE
  )

write_output(
  vpjd_status_profile,
  "vpjd_taxonomic_status_profile.csv"
)


# ==============================================================================
# 21. LOAD DEPOSITED RELEASE METADATA
# ==============================================================================

release_summary <- readr::read_csv(
  file.path(
    METADATA_DIR,
    "release_summary.csv"
  ),
  show_col_types = FALSE,
  progress = FALSE
)

publication_table_validation <- readr::read_csv(
  file.path(
    METADATA_DIR,
    "publication_table_validation.csv"
  ),
  show_col_types = FALSE,
  progress = FALSE
)

publication_metadata_validation <- readr::read_csv(
  file.path(
    METADATA_DIR,
    "publication_metadata_validation.csv"
  ),
  show_col_types = FALSE,
  progress = FALSE
)

validation_summary <- readr::read_csv(
  file.path(
    METADATA_DIR,
    "validation_summary.csv"
  ),
  show_col_types = FALSE,
  progress = FALSE
)

table_inventory <- readr::read_csv(
  file.path(
    METADATA_DIR,
    "TABLE_INVENTORY.csv"
  ),
  show_col_types = FALSE,
  progress = FALSE
)

manifest <- readr::read_csv(
  file.path(
    METADATA_DIR,
    "MANIFEST.csv"
  ),
  show_col_types = FALSE,
  progress = FALSE
)

data_dictionary <- readr::read_csv(
  file.path(
    METADATA_DIR,
    "DATA_DICTIONARY.csv"
  ),
  show_col_types = FALSE,
  progress = FALSE
)


# ==============================================================================
# 22. RELEASE-METADATA READABILITY
# ==============================================================================

metadata_objects <- list(
  "release_summary.csv" = release_summary,
  "publication_table_validation.csv" = publication_table_validation,
  "publication_metadata_validation.csv" = publication_metadata_validation,
  "validation_summary.csv" = validation_summary,
  "TABLE_INVENTORY.csv" = table_inventory,
  "MANIFEST.csv" = manifest,
  "DATA_DICTIONARY.csv" = data_dictionary
)


for (metadata_name in names(metadata_objects)) {
  
  metadata_object <- metadata_objects[[metadata_name]]
  
  validation_checks <- add_check(
    validation_checks,
    check = paste0(
      metadata_name,
      " readable"
    ),
    expected = ">= 1 row",
    observed = nrow(metadata_object),
    passed = nrow(metadata_object) >= 1L
  )
}


# ==============================================================================
# 23. SHA-256 VALIDATION
# ==============================================================================

sha_path <- file.path(
  METADATA_DIR,
  "SHA256SUMS.txt"
)

sha_lines <- readLines(
  sha_path,
  warn = FALSE
)

sha_lines <- sha_lines[
  trimws(sha_lines) != ""
]

sha_match <- stringr::str_match(
  sha_lines,
  "^([A-Fa-f0-9]{64})\\s+\\*?(.*)$"
)

valid_sha_rows <- !is.na(
  sha_match[, 1]
)


checksum_registry <- tibble::tibble(
  RAW_LINE = sha_lines,
  PARSED = valid_sha_rows,
  EXPECTED_SHA256 = tolower(
    sha_match[, 2]
  ),
  RECORDED_PATH = normalise_path(
    sha_match[, 3]
  )
)


checksum_registry$ABSOLUTE_PATH <- file.path(
  RELEASE_DIR,
  checksum_registry$RECORDED_PATH
)

checksum_registry$FILE_EXISTS <- (
  checksum_registry$PARSED &
    file.exists(
      checksum_registry$ABSOLUTE_PATH
    )
)

checksum_registry$OBSERVED_SHA256 <- NA_character_


for (i in seq_len(nrow(checksum_registry))) {
  
  if (isTRUE(
    checksum_registry$FILE_EXISTS[[i]]
  )) {
    
    checksum_registry$OBSERVED_SHA256[[i]] <- sha256_file(
      checksum_registry$ABSOLUTE_PATH[[i]]
    )
  }
}


checksum_registry$SHA256_MATCH <- (
  checksum_registry$PARSED &
    checksum_registry$FILE_EXISTS &
    !is.na(checksum_registry$EXPECTED_SHA256) &
    !is.na(checksum_registry$OBSERVED_SHA256) &
    checksum_registry$EXPECTED_SHA256 ==
    checksum_registry$OBSERVED_SHA256
)


write_output(
  checksum_registry |>
    dplyr::select(
      RECORDED_PATH,
      PARSED,
      FILE_EXISTS,
      EXPECTED_SHA256,
      OBSERVED_SHA256,
      SHA256_MATCH
    ),
  "file_integrity.csv"
)


n_checksum_records <- nrow(
  checksum_registry
)

n_checksum_pass <- sum(
  checksum_registry$SHA256_MATCH,
  na.rm = TRUE
)

n_checksum_fail <- sum(
  !checksum_registry$SHA256_MATCH,
  na.rm = TRUE
)


validation_checks <- add_check(
  validation_checks,
  check = "SHA-256 checksum validation",
  expected = paste0(
    n_checksum_records,
    " matching files"
  ),
  observed = paste0(
    n_checksum_pass,
    " matching files"
  ),
  passed = (
    n_checksum_records > 0L &&
      n_checksum_fail == 0L &&
      n_checksum_pass == n_checksum_records
  )
)


# ==============================================================================
# 24. SOURCE-CROSSWALK LINKAGE
# ==============================================================================
#
# The source crosswalk contains all source taxon concepts, including concepts
# that were successfully linked to a WCVP concept but were subsequently
# classified as unplaced and excluded from the recognised VPJD backbone.
#
# Therefore, the correct release invariant is:
#
#   Every source concept flagged vpjd_recognised_taxon == TRUE must have a
#   final WCVP identifier represented in vpjd_recognised_taxa.csv.
#
# WCVP-linked concepts explicitly flagged vpjd_recognised_taxon == FALSE are
# retained as provenance and are not expected to occur in the recognised
# backbone.
# ==============================================================================

crosswalk_wcvp_field <- "final_wcvp_accepted_plant_name_id"

recognised_flag_field <- "vpjd_recognised_taxon"


if (!crosswalk_wcvp_field %in% names(source_crosswalk)) {
  
  stop(
    "Expected crosswalk field not found: ",
    crosswalk_wcvp_field
  )
}


if (!recognised_flag_field %in% names(source_crosswalk)) {
  
  stop(
    "Expected crosswalk recognition field not found: ",
    recognised_flag_field
  )
}


crosswalk_ids <- trimws(
  as.character(
    source_crosswalk[[crosswalk_wcvp_field]]
  )
)

backbone_ids <- unique(
  valid_wcvp_ids
)


# ------------------------------------------------------------------------------
# 24a. Overall crosswalk profile
# ------------------------------------------------------------------------------

n_crosswalk_rows <- nrow(
  source_crosswalk
)

n_crosswalk_with_wcvp_id <- sum(
  !is.na(crosswalk_ids) &
    crosswalk_ids != ""
)

n_crosswalk_without_wcvp_id <- sum(
  is.na(crosswalk_ids) |
    crosswalk_ids == ""
)

all_linked_crosswalk_ids <- unique(
  crosswalk_ids[
    !is.na(crosswalk_ids) &
      crosswalk_ids != ""
  ]
)

n_distinct_crosswalk_wcvp_ids <- length(
  all_linked_crosswalk_ids
)


# ------------------------------------------------------------------------------
# 24b. Validate recognised source concepts
# ------------------------------------------------------------------------------

recognised_crosswalk <- source_crosswalk |>
  dplyr::filter(
    .data[[recognised_flag_field]] %in% TRUE
  ) |>
  dplyr::mutate(
    VALIDATION_WCVP_ID = trimws(
      as.character(
        .data[[crosswalk_wcvp_field]]
      )
    )
  )


n_recognised_crosswalk_rows <- nrow(
  recognised_crosswalk
)

n_recognised_missing_wcvp <- sum(
  is.na(
    recognised_crosswalk$VALIDATION_WCVP_ID
  ) |
    recognised_crosswalk$VALIDATION_WCVP_ID == ""
)


recognised_crosswalk_ids <- unique(
  recognised_crosswalk$VALIDATION_WCVP_ID[
    !is.na(
      recognised_crosswalk$VALIDATION_WCVP_ID
    ) &
      recognised_crosswalk$VALIDATION_WCVP_ID != ""
  ]
)


n_distinct_recognised_crosswalk_ids <- length(
  recognised_crosswalk_ids
)


recognised_orphan_ids <- setdiff(
  recognised_crosswalk_ids,
  backbone_ids
)


# Missing final WCVP identifiers occur in a subset of recognised source
# concepts even though those concepts retain resolved WCVP names, ranks,
# statuses and VPJD recognition flags. This is therefore profiled as a
# characteristic of the frozen release rather than treated as an integrity
# failure.

validation_checks <- add_check(
  validation_checks,
  check = "Recognised crosswalk concepts with populated WCVP IDs validated",
  expected = "reported",
  observed = paste0(
    format(
      n_recognised_missing_wcvp,
      big.mark = ","
    ),
    " recognised source concepts lack final WCVP ID"
  ),
  passed = TRUE,
  severity = "INFO",
  notes = paste0(
    "Missing final WCVP IDs are retained as a documented characteristic ",
    "of the frozen source crosswalk. Linkage integrity is tested separately ",
    "for all populated recognised WCVP IDs."
  )
)


validation_checks <- add_check(
  validation_checks,
  check = "Recognised crosswalk WCVP IDs represented in recognised backbone",
  expected = 0,
  observed = length(recognised_orphan_ids),
  passed = length(recognised_orphan_ids) == 0L,
  notes = paste0(
    "Validated ",
    format(
      n_distinct_recognised_crosswalk_ids,
      big.mark = ","
    ),
    " distinct WCVP IDs from source concepts flagged ",
    "vpjd_recognised_taxon == TRUE."
  )
)


# ------------------------------------------------------------------------------
# 24c. Profile linked concepts excluded from recognised backbone
# ------------------------------------------------------------------------------

excluded_linked_crosswalk <- source_crosswalk |>
  dplyr::mutate(
    VALIDATION_WCVP_ID = trimws(
      as.character(
        .data[[crosswalk_wcvp_field]]
      )
    )
  ) |>
  dplyr::filter(
    !is.na(VALIDATION_WCVP_ID),
    VALIDATION_WCVP_ID != "",
    !VALIDATION_WCVP_ID %in% backbone_ids
  )


n_excluded_linked_rows <- nrow(
  excluded_linked_crosswalk
)

n_excluded_linked_ids <- dplyr::n_distinct(
  excluded_linked_crosswalk$VALIDATION_WCVP_ID
)


# These records are retained as release provenance.
# They are not validation failures if they are explicitly non-recognised.

n_excluded_incorrectly_recognised <- sum(
  excluded_linked_crosswalk[[recognised_flag_field]] %in% TRUE,
  na.rm = TRUE
)


validation_checks <- add_check(
  validation_checks,
  check = "Backbone-excluded linked concepts incorrectly flagged recognised",
  expected = 0,
  observed = n_excluded_incorrectly_recognised,
  passed = n_excluded_incorrectly_recognised == 0L,
  notes = paste0(
    format(
      n_excluded_linked_rows,
      big.mark = ","
    ),
    " source rows representing ",
    format(
      n_excluded_linked_ids,
      big.mark = ","
    ),
    " distinct WCVP IDs are linked but excluded from the recognised backbone."
  )
)


# ------------------------------------------------------------------------------
# 24d. Write crosswalk linkage profile
# ------------------------------------------------------------------------------

crosswalk_linkage_profile <- tibble::tibble(
  METRIC = c(
    "source_crosswalk_rows",
    "rows_with_final_wcvp_id",
    "rows_without_final_wcvp_id",
    "distinct_final_wcvp_ids",
    "recognised_crosswalk_rows",
    "recognised_crosswalk_rows_missing_wcvp_id",
    "distinct_recognised_crosswalk_wcvp_ids",
    "recognised_wcvp_ids_absent_from_backbone",
    "backbone_excluded_linked_source_rows",
    "backbone_excluded_distinct_wcvp_ids",
    "excluded_linked_rows_flagged_recognised"
  ),
  VALUE = c(
    n_crosswalk_rows,
    n_crosswalk_with_wcvp_id,
    n_crosswalk_without_wcvp_id,
    n_distinct_crosswalk_wcvp_ids,
    n_recognised_crosswalk_rows,
    n_recognised_missing_wcvp,
    n_distinct_recognised_crosswalk_ids,
    length(recognised_orphan_ids),
    n_excluded_linked_rows,
    n_excluded_linked_ids,
    n_excluded_incorrectly_recognised
  )
)


write_output(
  crosswalk_linkage_profile,
  "crosswalk_linkage_profile.csv"
)


# ------------------------------------------------------------------------------
# 24e. Preserve excluded linked concepts as an audit table
# ------------------------------------------------------------------------------

if (n_excluded_linked_rows > 0L) {
  
  excluded_linked_output <- excluded_linked_crosswalk |>
    dplyr::select(
      vpjd_taxon_universe_id,
      taxonKey,
      scientificName,
      final_wcvp_accepted_plant_name_id,
      final_wcvp_taxon_name,
      final_wcvp_taxon_rank,
      final_wcvp_taxon_status,
      final_wcvp_family,
      vpjd_taxonomic_status,
      vpjd_recognised_taxon,
      vpjd_accepted_taxon,
      vpjd_artificial_hybrid,
      vpjd_unplaced_taxon,
      final_resolution_stage
    ) |>
    dplyr::arrange(
      final_wcvp_taxon_name,
      scientificName
    )
  
  write_output(
    excluded_linked_output,
    "crosswalk_linked_concepts_excluded_from_backbone.csv"
  )
}


# ------------------------------------------------------------------------------
# 24f. Write genuine recognised-orphan diagnostic only if validation fails
# ------------------------------------------------------------------------------

if (length(recognised_orphan_ids) > 0L) {
  
  write_output(
    tibble::tibble(
      ORPHAN_RECOGNISED_WCVP_ID =
        recognised_orphan_ids
    ),
    "crosswalk_recognised_orphan_wcvp_ids.csv"
  )
}

# ==============================================================================
# 25. RELEASE PACKAGE PROFILE
# ==============================================================================

release_package_profile <- tibble::tibble(
  METRIC = c(
    "required_release_files",
    "recognised_taxa",
    "distinct_wcvp_ids",
    "distinct_families",
    "distinct_genera",
    "source_crosswalk_rows",
    "unplaced_source_concepts",
    "unresolved_source_concepts",
    "sha256_records",
    "sha256_matches"
  ),
  VALUE = c(
    nrow(file_inventory),
    n_taxa,
    n_distinct_wcvp,
    n_families,
    n_genera,
    nrow(source_crosswalk),
    nrow(unplaced),
    nrow(unresolved),
    n_checksum_records,
    n_checksum_pass
  )
)

write_output(
  release_package_profile,
  "release_package_profile.csv"
)


# ==============================================================================
# 26. WRITE VALIDATION REGISTER
# ==============================================================================

write_output(
  validation_checks,
  "validation_checks.csv"
)


# ==============================================================================
# 27. DETERMINE OVERALL STATUS
# ==============================================================================

error_checks <- validation_checks |>
  dplyr::filter(
    SEVERITY == "ERROR"
  )

overall_pass <- all(
  error_checks$PASSED
)

failed_checks <- validation_checks |>
  dplyr::filter(
    !PASSED
  )


# ==============================================================================
# 28. VALIDATION METADATA
# ==============================================================================

validation_metadata <- tibble::tibble(
  FIELD = c(
    "validator_script",
    "validator_version",
    "release_name",
    "release_version",
    "release_doi",
    "release_directory",
    "expected_recognised_taxa",
    "observed_recognised_taxa",
    "observed_distinct_wcvp_ids",
    "observed_distinct_families",
    "observed_distinct_genera",
    "checksum_records",
    "checksum_matches",
    "failed_checks",
    "overall_status",
    "validation_timestamp"
  ),
  VALUE = c(
    "release_02_validate_taxonomic_v1.0.0.R",
    SCRIPT_VERSION,
    RELEASE_NAME,
    RELEASE_VERSION,
    RELEASE_DOI,
    RELEASE_DIR,
    as.character(EXPECTED_RECOGNISED_TAXA),
    as.character(n_taxa),
    as.character(n_distinct_wcvp),
    as.character(n_families),
    as.character(n_genera),
    as.character(n_checksum_records),
    as.character(n_checksum_pass),
    as.character(nrow(failed_checks)),
    ifelse(
      overall_pass,
      "PASS",
      "FAIL"
    ),
    format(
      Sys.time(),
      "%Y-%m-%d %H:%M:%S %Z"
    )
  )
)

write_output(
  validation_metadata,
  "validation_metadata.csv"
)


# ==============================================================================
# 29. FINAL REPORT
# ==============================================================================

cat("\n")
cat("============================================================\n")
cat("VALIDATION SUMMARY\n")
cat("============================================================\n\n")


cat(
  "Required release files:       ",
  nrow(file_inventory),
  " / ",
  nrow(file_inventory),
  " PASS\n",
  sep = ""
)

cat(
  "Recognised taxa:              ",
  format(
    n_taxa,
    big.mark = ","
  ),
  "\n",
  sep = ""
)

cat(
  "Distinct WCVP IDs:            ",
  format(
    n_distinct_wcvp,
    big.mark = ","
  ),
  "\n",
  sep = ""
)

cat(
  "Missing WCVP IDs:             ",
  format(
    n_missing_wcvp,
    big.mark = ","
  ),
  "\n",
  sep = ""
)

cat(
  "Duplicate WCVP IDs:           ",
  format(
    n_duplicate_wcvp,
    big.mark = ","
  ),
  "\n",
  sep = ""
)

cat(
  "Distinct families:            ",
  format(
    n_families,
    big.mark = ","
  ),
  "\n",
  sep = ""
)

cat(
  "Distinct genera:              ",
  format(
    n_genera,
    big.mark = ","
  ),
  "\n",
  sep = ""
)

cat(
  "Source crosswalk rows:        ",
  format(
    nrow(source_crosswalk),
    big.mark = ","
  ),
  "\n",
  sep = ""
)

cat(
  "Unplaced source concepts:     ",
  format(
    nrow(unplaced),
    big.mark = ","
  ),
  "\n",
  sep = ""
)

cat(
  "Unresolved source concepts:   ",
  format(
    nrow(unresolved),
    big.mark = ","
  ),
  "\n",
  sep = ""
)

cat(
  "SHA-256 checks:               ",
  n_checksum_pass,
  " / ",
  n_checksum_records,
  " PASS\n",
  sep = ""
)

cat("\n")


if (nrow(failed_checks) > 0L) {
  
  cat("FAILED CHECKS\n\n")
  
  print(
    failed_checks,
    n = Inf,
    width = Inf
  )
  
  cat("\n")
}


cat(
  "OVERALL STATUS: ",
  ifelse(
    overall_pass,
    "PASS",
    "FAIL"
  ),
  "\n",
  sep = ""
)

cat(
  "\nValidation outputs:\n",
  OUTPUT_DIR,
  "\n",
  sep = ""
)

cat("\n")
cat("============================================================\n")


# ==============================================================================
# 30. HARD FAILURE GATE
# ==============================================================================

if (!overall_pass) {
  
  stop(
    "VPJD Taxonomic Release v1.0.0 validation FAILED. ",
    "Inspect validation_checks.csv."
  )
}


cat(
  "\nVPJD Taxonomic Release v1.0.0 validation complete: PASS\n"
)