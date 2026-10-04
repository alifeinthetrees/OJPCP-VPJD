# ==============================================================================
# VPJD Geography Release v1.0.0 — Independent Validation
#
# Purpose:
#   Independently validate the frozen VPJD Geography Release v1.0.0.
#
# This script DOES NOT rebuild or modify the Geography Release.
#
# It validates:
#   - required release files
#   - published table schemas
#   - expected release cardinalities
#   - geographic identifier integrity
#   - taxon × geographic-unit uniqueness
#   - distribution/evidence concordance
#   - linkage to the frozen VPJD Taxonomic Release v1.0.0
#   - positive-evidence semantics
#   - package manifest
#   - SHA-256 checksums
#
# Author:
#   Ben M. Jones
#
# ==============================================================================


suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tibble)
  library(digest)
})


# ==============================================================================
# 1. Paths
# ==============================================================================

PROJECT_ROOT <- "I:/R/OJPCP/VPJD-OJPCP"

GEOGRAPHY_DIR <- file.path(
  PROJECT_ROOT,
  "data/releases/geography_v1.0.0"
)

TAXONOMIC_DIR <- file.path(
  PROJECT_ROOT,
  "data/releases/taxonomic_v1.0.0"
)

OUTPUT_DIR <- file.path(
  PROJECT_ROOT,
  "outputs/tables/releases/geography_v1.0.0_validation"
)

dir.create(
  OUTPUT_DIR,
  recursive = TRUE,
  showWarnings = FALSE
)


# ==============================================================================
# 2. Expected release constants
# ==============================================================================

EXPECTED_GEOGRAPHIC_UNITS <- 418L
EXPECTED_JAPAN_BOTANICAL_AREAS <- 51L
EXPECTED_WCVP_TDWG_UNITS <- 367L

EXPECTED_DISTRIBUTION_ROWS <- 128824L
EXPECTED_EVIDENCE_ROWS <- 128824L

EXPECTED_TAXONOMIC_BACKBONE <- 12037L
EXPECTED_TAXA_WITH_EVIDENCE <- 11470L
EXPECTED_TAXA_WITHOUT_EVIDENCE <- 567L

EXPECTED_RELEASE_NAME <- "VPJD Geography v1.0.0"


# ==============================================================================
# 3. Required files
# ==============================================================================

required_files <- c(
  "data/vpjd_geographic_units_v1.0.0.csv",
  "data/vpjd_taxon_geographic_distribution_v1.0.0.csv",
  "data/vpjd_taxon_geographic_evidence_v1.0.0.csv",
  "metadata/vpjd_geography_contributors_v1.0.0.csv",
  "metadata/vpjd_geography_data_dictionary_v1.0.0.csv",
  "metadata/vpjd_geography_keywords_v1.0.0.csv",
  "metadata/vpjd_geography_methodological_safeguards_v1.0.0.csv",
  "metadata/vpjd_geography_provenance_v1.0.0.csv",
  "metadata/vpjd_geography_related_identifiers_v1.0.0.csv",
  "metadata/vpjd_geography_release_metadata_v1.0.0.csv",
  "metadata/vpjd_geography_release_statistics_v1.0.0.csv",
  "metadata/vpjd_geography_zenodo_metadata_v1.0.0.csv",
  "metadata/vpjd_geography_zenodo_metadata_v1.0.0.txt",
  "README_VPJD_Geography_v1.0.0.txt",
  "vpjd_geography_package_manifest_v1.0.0.csv",
  "vpjd_geography_package_sha256_v1.0.0.csv"
)

required_file_status <- tibble(
  relative_path = required_files,
  exists = file.exists(
    file.path(GEOGRAPHY_DIR, required_files)
  )
)

if (!all(required_file_status$exists)) {

  stop(
    "Required Geography release files are missing:\n",
    paste(
      required_file_status$relative_path[
        !required_file_status$exists
      ],
      collapse = "\n"
    )
  )
}


# ==============================================================================
# 4. Load principal Geography tables
# ==============================================================================

units <- read_csv(
  file.path(
    GEOGRAPHY_DIR,
    "data/vpjd_geographic_units_v1.0.0.csv"
  ),
  show_col_types = FALSE
)

distribution <- read_csv(
  file.path(
    GEOGRAPHY_DIR,
    "data/vpjd_taxon_geographic_distribution_v1.0.0.csv"
  ),
  show_col_types = FALSE
)

evidence <- read_csv(
  file.path(
    GEOGRAPHY_DIR,
    "data/vpjd_taxon_geographic_evidence_v1.0.0.csv"
  ),
  show_col_types = FALSE
)


# ==============================================================================
# 5. Load Taxonomic v1.0.0 backbone
# ==============================================================================

taxa <- read_csv(
  file.path(
    TAXONOMIC_DIR,
    "data/vpjd_recognised_taxa.csv"
  ),
  show_col_types = FALSE
)

if (!"wcvp_plant_name_id" %in% names(taxa)) {
  stop(
    "Taxonomic Release does not contain wcvp_plant_name_id."
  )
}


# ==============================================================================
# 6. Load release metadata / integrity tables
# ==============================================================================

manifest <- read_csv(
  file.path(
    GEOGRAPHY_DIR,
    "vpjd_geography_package_manifest_v1.0.0.csv"
  ),
  show_col_types = FALSE
)

checksum_table <- read_csv(
  file.path(
    GEOGRAPHY_DIR,
    "vpjd_geography_package_sha256_v1.0.0.csv"
  ),
  show_col_types = FALSE
)

release_statistics <- read_csv(
  file.path(
    GEOGRAPHY_DIR,
    "metadata/vpjd_geography_release_statistics_v1.0.0.csv"
  ),
  show_col_types = FALSE
)

data_dictionary <- read_csv(
  file.path(
    GEOGRAPHY_DIR,
    "metadata/vpjd_geography_data_dictionary_v1.0.0.csv"
  ),
  show_col_types = FALSE
)

provenance <- read_csv(
  file.path(
    GEOGRAPHY_DIR,
    "metadata/vpjd_geography_provenance_v1.0.0.csv"
  ),
  show_col_types = FALSE
)


# ==============================================================================
# 7. Validation helper
# ==============================================================================

checks <- list()

add_check <- function(
    check,
    expected,
    observed,
    passed,
    note = NA_character_
) {

  checks[[length(checks) + 1L]] <<- tibble(
    check = check,
    expected = as.character(expected),
    observed = as.character(observed),
    status = ifelse(passed, "PASS", "FAIL"),
    note = note
  )
}


# ==============================================================================
# 8. Schema validation
# ==============================================================================

required_unit_fields <- c(
  "geographic_unit_id",
  "geographic_code",
  "geographic_name",
  "geographic_unit_type",
  "concept_group",
  "geographic_standard",
  "country_context",
  "tdwg_level",
  "parent_tdwg_code",
  "parent_geographic_name",
  "concept_resolution_status",
  "identifier_status",
  "wcvp_location_id",
  "geographic_name_source",
  "geographic_name_validation_status",
  "identifier_freeze_release",
  "identifier_freeze_script",
  "identifier_freeze_date"
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

missing_unit_fields <- setdiff(
  required_unit_fields,
  names(units)
)

missing_distribution_fields <- setdiff(
  required_distribution_fields,
  names(distribution)
)

missing_evidence_fields <- setdiff(
  required_evidence_fields,
  names(evidence)
)

add_check(
  "Geographic-unit schema",
  length(required_unit_fields),
  length(intersect(required_unit_fields, names(units))),
  length(missing_unit_fields) == 0,
  ifelse(
    length(missing_unit_fields) == 0,
    "All required geographic-unit fields present.",
    paste(missing_unit_fields, collapse = "; ")
  )
)

add_check(
  "Distribution schema",
  length(required_distribution_fields),
  length(intersect(required_distribution_fields, names(distribution))),
  length(missing_distribution_fields) == 0,
  ifelse(
    length(missing_distribution_fields) == 0,
    "All required distribution fields present.",
    paste(missing_distribution_fields, collapse = "; ")
  )
)

add_check(
  "Evidence schema",
  length(required_evidence_fields),
  length(intersect(required_evidence_fields, names(evidence))),
  length(missing_evidence_fields) == 0,
  ifelse(
    length(missing_evidence_fields) == 0,
    "All required evidence fields present.",
    paste(missing_evidence_fields, collapse = "; ")
  )
)


# ==============================================================================
# 9. Geographic vocabulary validation
# ==============================================================================

n_units <- nrow(units)

n_distinct_unit_ids <- n_distinct(
  units$geographic_unit_id
)

missing_unit_ids <- sum(
  is.na(units$geographic_unit_id) |
    trimws(units$geographic_unit_id) == ""
)

duplicate_unit_ids <- n_units -
  n_distinct_unit_ids

n_japan_areas <- sum(
  units$concept_group == "JAPAN_BOTANICAL_AREA",
  na.rm = TRUE
)

n_wcvp_units <- sum(
  units$concept_group == "WCVP_TDWG_LOCATION",
  na.rm = TRUE
)

add_check(
  "Geographic units",
  EXPECTED_GEOGRAPHIC_UNITS,
  n_units,
  n_units == EXPECTED_GEOGRAPHIC_UNITS
)

add_check(
  "Distinct geographic-unit identifiers",
  EXPECTED_GEOGRAPHIC_UNITS,
  n_distinct_unit_ids,
  n_distinct_unit_ids == EXPECTED_GEOGRAPHIC_UNITS
)

add_check(
  "Missing geographic-unit identifiers",
  0,
  missing_unit_ids,
  missing_unit_ids == 0
)

add_check(
  "Duplicate geographic-unit identifiers",
  0,
  duplicate_unit_ids,
  duplicate_unit_ids == 0
)

add_check(
  "Japanese botanical areas",
  EXPECTED_JAPAN_BOTANICAL_AREAS,
  n_japan_areas,
  n_japan_areas == EXPECTED_JAPAN_BOTANICAL_AREAS
)

add_check(
  "WCVP/TDWG geographic units",
  EXPECTED_WCVP_TDWG_UNITS,
  n_wcvp_units,
  n_wcvp_units == EXPECTED_WCVP_TDWG_UNITS
)


# ==============================================================================
# 10. Distribution-table structural validation
# ==============================================================================

distribution_key <- distribution %>%
  count(
    wcvp_plant_name_id,
    geographic_unit_id,
    name = "n"
  )

duplicate_distribution_cells <- distribution_key %>%
  filter(n > 1)

n_distribution_taxa <- n_distinct(
  distribution$wcvp_plant_name_id
)

n_distribution_units <- n_distinct(
  distribution$geographic_unit_id
)

add_check(
  "Distribution rows",
  EXPECTED_DISTRIBUTION_ROWS,
  nrow(distribution),
  nrow(distribution) == EXPECTED_DISTRIBUTION_ROWS
)

add_check(
  "Duplicate taxon × geographic-unit distribution cells",
  0,
  nrow(duplicate_distribution_cells),
  nrow(duplicate_distribution_cells) == 0
)

add_check(
  "Taxa with positive geographic evidence",
  EXPECTED_TAXA_WITH_EVIDENCE,
  n_distribution_taxa,
  n_distribution_taxa == EXPECTED_TAXA_WITH_EVIDENCE
)

add_check(
  "Geographic units represented in published distribution",
  EXPECTED_JAPAN_BOTANICAL_AREAS,
  n_distribution_units,
  n_distribution_units == EXPECTED_JAPAN_BOTANICAL_AREAS
)


# ==============================================================================
# 11. Distribution → geographic vocabulary integrity
# ==============================================================================

distribution_units_missing <- distribution %>%
  distinct(geographic_unit_id) %>%
  anti_join(
    units %>% distinct(geographic_unit_id),
    by = "geographic_unit_id"
  )

add_check(
  "Distribution geographic-unit IDs represented in vocabulary",
  0,
  nrow(distribution_units_missing),
  nrow(distribution_units_missing) == 0
)


# ==============================================================================
# 12. Distribution → Taxonomic v1.0.0 integrity
# ==============================================================================

distribution_taxa_missing <- distribution %>%
  distinct(wcvp_plant_name_id) %>%
  anti_join(
    taxa %>% distinct(wcvp_plant_name_id),
    by = "wcvp_plant_name_id"
  )

add_check(
  "Distribution WCVP IDs represented in Taxonomic v1.0.0",
  0,
  nrow(distribution_taxa_missing),
  nrow(distribution_taxa_missing) == 0
)

taxa_without_geography <- taxa %>%
  distinct(wcvp_plant_name_id) %>%
  anti_join(
    distribution %>% distinct(wcvp_plant_name_id),
    by = "wcvp_plant_name_id"
  )

add_check(
  "Taxa without positive geographic evidence",
  EXPECTED_TAXA_WITHOUT_EVIDENCE,
  nrow(taxa_without_geography),
  nrow(taxa_without_geography) == EXPECTED_TAXA_WITHOUT_EVIDENCE,
  paste0(
    EXPECTED_TAXONOMIC_BACKBONE,
    " recognised taxa minus ",
    EXPECTED_TAXA_WITH_EVIDENCE,
    " taxa with positive evidence."
  )
)


# ==============================================================================
# 13. Geographic labels agree with vocabulary
# ==============================================================================

distribution_unit_mismatch <- distribution %>%
  left_join(
    units %>%
      select(
        geographic_unit_id,
        vocabulary_code = geographic_code,
        vocabulary_name = geographic_name,
        vocabulary_group = concept_group
      ),
    by = "geographic_unit_id"
  ) %>%
  filter(
    geographic_code != vocabulary_code |
      geographic_name != vocabulary_name |
      concept_group != vocabulary_group
  )

add_check(
  "Distribution geographic labels agree with vocabulary",
  0,
  nrow(distribution_unit_mismatch),
  nrow(distribution_unit_mismatch) == 0
)


# ==============================================================================
# 14. Taxon names agree with Taxonomic v1.0.0
# ==============================================================================

taxon_name_mismatch <- distribution %>%
  distinct(
    wcvp_plant_name_id,
    wcvp_taxon_name
  ) %>%
  left_join(
    taxa %>%
      select(
        wcvp_plant_name_id,
        taxonomic_release_name = wcvp_taxon_name
      ),
    by = "wcvp_plant_name_id"
  ) %>%
  filter(
    is.na(taxonomic_release_name) |
      wcvp_taxon_name != taxonomic_release_name
  )

add_check(
  "Distribution taxon names agree with Taxonomic v1.0.0",
  0,
  nrow(taxon_name_mismatch),
  nrow(taxon_name_mismatch) == 0
)


# ==============================================================================
# 15. Distribution semantics
# ==============================================================================

invalid_distribution_group <- sum(
  is.na(distribution$concept_group) |
    distribution$concept_group != "JAPAN_BOTANICAL_AREA"
)

invalid_distribution_status <- sum(
  is.na(distribution$occurrence_status) |
    distribution$occurrence_status != "PRESENT_EVIDENCE"
)

invalid_distribution_release <- sum(
  is.na(distribution$geography_release) |
    distribution$geography_release != EXPECTED_RELEASE_NAME
)

add_check(
  "Distribution concept group",
  "JAPAN_BOTANICAL_AREA only",
  invalid_distribution_group,
  invalid_distribution_group == 0
)

add_check(
  "Distribution occurrence status",
  "PRESENT_EVIDENCE only",
  invalid_distribution_status,
  invalid_distribution_status == 0
)

add_check(
  "Distribution release identifier",
  EXPECTED_RELEASE_NAME,
  invalid_distribution_release,
  invalid_distribution_release == 0
)


# ==============================================================================
# 16. Evidence-table structural validation
# ==============================================================================

evidence_key <- evidence %>%
  count(
    wcvp_plant_name_id,
    geographic_unit_id,
    name = "n"
  )

duplicate_evidence_cells <- evidence_key %>%
  filter(n > 1)

add_check(
  "Evidence rows",
  EXPECTED_EVIDENCE_ROWS,
  nrow(evidence),
  nrow(evidence) == EXPECTED_EVIDENCE_ROWS
)

add_check(
  "Duplicate taxon × geographic-unit evidence cells",
  0,
  nrow(duplicate_evidence_cells),
  nrow(duplicate_evidence_cells) == 0
)


# ==============================================================================
# 17. Distribution ↔ evidence concordance
# ==============================================================================

distribution_pairs <- distribution %>%
  select(
    wcvp_plant_name_id,
    geographic_unit_id
  ) %>%
  distinct()

evidence_pairs <- evidence %>%
  select(
    wcvp_plant_name_id,
    geographic_unit_id
  ) %>%
  distinct()

distribution_without_evidence <- anti_join(
  distribution_pairs,
  evidence_pairs,
  by = c(
    "wcvp_plant_name_id",
    "geographic_unit_id"
  )
)

evidence_without_distribution <- anti_join(
  evidence_pairs,
  distribution_pairs,
  by = c(
    "wcvp_plant_name_id",
    "geographic_unit_id"
  )
)

add_check(
  "Distribution cells lacking evidence rows",
  0,
  nrow(distribution_without_evidence),
  nrow(distribution_without_evidence) == 0
)

add_check(
  "Evidence rows lacking distribution cells",
  0,
  nrow(evidence_without_distribution),
  nrow(evidence_without_distribution) == 0
)


# ==============================================================================
# 18. Positive-evidence semantics
# ==============================================================================

invalid_occurrence_counts <- evidence %>%
  filter(
    is.na(n_occurrence_records) |
      n_occurrence_records <= 0
  )

invalid_unique_counts <- evidence %>%
  filter(
    is.na(n_unique_occurrences) |
      n_unique_occurrences <= 0 |
      n_unique_occurrences > n_occurrence_records
  )

invalid_evidence_type <- evidence %>%
  filter(
    is.na(evidence_type) |
      evidence_type != "POSITIVE_OCCURRENCE_EVIDENCE"
  )

invalid_evidence_status <- evidence %>%
  filter(
    is.na(occurrence_status) |
      occurrence_status != "PRESENT_EVIDENCE"
  )

add_check(
  "Evidence rows with non-positive occurrence counts",
  0,
  nrow(invalid_occurrence_counts),
  nrow(invalid_occurrence_counts) == 0
)

add_check(
  "Invalid unique-occurrence counts",
  0,
  nrow(invalid_unique_counts),
  nrow(invalid_unique_counts) == 0
)

add_check(
  "Evidence type",
  "POSITIVE_OCCURRENCE_EVIDENCE only",
  nrow(invalid_evidence_type),
  nrow(invalid_evidence_type) == 0
)

add_check(
  "Evidence occurrence status",
  "PRESENT_EVIDENCE only",
  nrow(invalid_evidence_status),
  nrow(invalid_evidence_status) == 0
)


# ==============================================================================
# 19. Evidence → vocabulary / taxonomy integrity
# ==============================================================================

evidence_units_missing <- evidence %>%
  distinct(geographic_unit_id) %>%
  anti_join(
    units %>% distinct(geographic_unit_id),
    by = "geographic_unit_id"
  )

evidence_taxa_missing <- evidence %>%
  distinct(wcvp_plant_name_id) %>%
  anti_join(
    taxa %>% distinct(wcvp_plant_name_id),
    by = "wcvp_plant_name_id"
  )

add_check(
  "Evidence geographic-unit IDs represented in vocabulary",
  0,
  nrow(evidence_units_missing),
  nrow(evidence_units_missing) == 0
)

add_check(
  "Evidence WCVP IDs represented in Taxonomic v1.0.0",
  0,
  nrow(evidence_taxa_missing),
  nrow(evidence_taxa_missing) == 0
)


# ==============================================================================
# 20. Metadata-table readability
# ==============================================================================

add_check(
  "Release statistics readable",
  ">= 1 row",
  nrow(release_statistics),
  nrow(release_statistics) > 0
)

add_check(
  "Data dictionary readable",
  ">= 1 row",
  nrow(data_dictionary),
  nrow(data_dictionary) > 0
)

add_check(
  "Provenance table readable",
  ">= 1 row",
  nrow(provenance),
  nrow(provenance) > 0
)

add_check(
  "Package manifest readable",
  14,
  nrow(manifest),
  nrow(manifest) == 14
)

add_check(
  "SHA-256 table readable",
  14,
  nrow(checksum_table),
  nrow(checksum_table) == 14
)


# ==============================================================================
# 21. SHA-256 validation
# ==============================================================================

checksum_validation <- checksum_table %>%
  mutate(
    full_path = file.path(
      GEOGRAPHY_DIR,
      relative_path
    ),
    file_exists = file.exists(full_path),
    observed_sha256 = vapply(
      full_path,
      function(f) {
        if (!file.exists(f)) {
          return(NA_character_)
        }

        digest(
          file = f,
          algo = "sha256",
          serialize = FALSE
        )
      },
      character(1)
    ),
    checksum_match =
      file_exists &
      tolower(observed_sha256) == tolower(sha256)
  )

n_checksum_matches <- sum(
  checksum_validation$checksum_match,
  na.rm = TRUE
)

add_check(
  "SHA-256 checksum validation",
  nrow(checksum_table),
  n_checksum_matches,
  n_checksum_matches == nrow(checksum_table),
  paste0(
    n_checksum_matches,
    " of ",
    nrow(checksum_table),
    " package checksums match."
  )
)


# ==============================================================================
# 22. Compile validation results
# ==============================================================================

validation_checks <- bind_rows(checks)

overall_pass <- all(
  validation_checks$status == "PASS"
)


# ==============================================================================
# 23. Write validation outputs
# ==============================================================================

write_csv(
  validation_checks,
  file.path(
    OUTPUT_DIR,
    "geography_v1.0.0_validation_checks.csv"
  )
)

write_csv(
  required_file_status,
  file.path(
    OUTPUT_DIR,
    "geography_v1.0.0_required_files.csv"
  )
)

write_csv(
  checksum_validation %>%
    select(
      relative_path,
      size_bytes,
      sha256,
      file_exists,
      observed_sha256,
      checksum_match
    ),
  file.path(
    OUTPUT_DIR,
    "geography_v1.0.0_checksum_validation.csv"
  )
)

write_csv(
  taxa_without_geography,
  file.path(
    OUTPUT_DIR,
    "geography_v1.0.0_taxa_without_positive_evidence.csv"
  )
)

write_csv(
  distribution_taxa_missing,
  file.path(
    OUTPUT_DIR,
    "geography_v1.0.0_distribution_taxa_not_in_taxonomic_release.csv"
  )
)

write_csv(
  distribution_units_missing,
  file.path(
    OUTPUT_DIR,
    "geography_v1.0.0_distribution_units_not_in_vocabulary.csv"
  )
)

write_csv(
  taxon_name_mismatch,
  file.path(
    OUTPUT_DIR,
    "geography_v1.0.0_taxon_name_mismatches.csv"
  )
)

write_csv(
  distribution_unit_mismatch,
  file.path(
    OUTPUT_DIR,
    "geography_v1.0.0_geographic_label_mismatches.csv"
  )
)


# ==============================================================================
# 24. Console report
# ==============================================================================

cat("\n")
cat("============================================================\n")
cat("VPJD Geography Release v1.0.0 validation\n")
cat("============================================================\n\n")

print(
  validation_checks,
  n = Inf,
  width = Inf
)

cat("\n------------------------------------------------------------\n")

if (overall_pass) {

  cat(
    "VPJD Geography Release v1.0.0 validation complete: PASS\n"
  )

} else {

  cat(
    "VPJD Geography Release v1.0.0 validation complete: FAIL\n"
  )

  cat("\nFailed checks:\n")

  print(
    validation_checks %>%
      filter(status == "FAIL"),
    n = Inf,
    width = Inf
  )
}

cat("------------------------------------------------------------\n")

cat(
  "Validation outputs: ",
  OUTPUT_DIR,
  "\n",
  sep = ""
)

cat("============================================================\n")


# ==============================================================================
# 25. Fail script if release validation fails
# ==============================================================================

if (!overall_pass) {
  stop(
    "VPJD Geography Release v1.0.0 failed one or more validation checks."
  )
}
