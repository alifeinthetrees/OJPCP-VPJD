# ==============================================================================
# VPJD GEOGRAPHY MODULE
# geography_06e_build_release_summaries.R
#
# PURPOSE
# -------
# Build descriptive, publication-ready summaries for:
#
#   Vascular Plants of Japan Database (VPJD)
#   Geography Release v1.0.0
#
# This script operates only on the frozen release products.
#
# It MUST NOT:
#
#   - modify the canonical geographic vocabulary;
#   - modify the canonical distribution table;
#   - modify the canonical evidence table;
#   - modify VPJD Taxonomic Release v1.0.0;
#   - infer biological absence;
#   - infer endemicity;
#   - infer rarity;
#   - apply Nakamura Key-to-Stars logic;
#   - assign Star categories.
#
#
# OUTPUTS
# -------
#
# 1. vpjd_geography_summary_overview_v1.0.0.csv
# 2. vpjd_geography_taxon_coverage_v1.0.0.csv
# 3. vpjd_geography_area_richness_v1.0.0.csv
# 4. vpjd_geography_taxon_coverage_frequency_v1.0.0.csv
# 5. vpjd_geography_evidence_support_summary_v1.0.0.csv
# 6. vpjd_geography_family_summary_v1.0.0.csv
# 7. vpjd_geography_genus_summary_v1.0.0.csv
# 8. vpjd_geography_descriptive_statistics_v1.0.0.csv
# 9. vpjd_geography_release_summary_v1.0.0.txt
# 10. geography_06e_summary_gate.csv
#
# ==============================================================================


# ------------------------------------------------------------------------------
# 01. CONSTANTS
# ------------------------------------------------------------------------------

RELEASE_VERSION <- "1.0.0"

RELEASE_NAME <- "VPJD Geography v1.0.0"

SCRIPT_NAME <- "geography_06e_build_release_summaries.R"

BUILD_DATE <- format(
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
  "06e_build_release_summaries"
)

dir.create(
  SUMMARY_ROOT,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  AUDIT_ROOT,
  recursive = TRUE,
  showWarnings = FALSE
)


# ------------------------------------------------------------------------------
# 03. INPUT FILES
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

METADATA_GATE_FILE <- file.path(
  PROJECT_ROOT,
  "audit",
  "geography",
  "release_v1.0.0",
  "06d_build_release_metadata",
  "geography_06d_metadata_gate.csv"
)


# ------------------------------------------------------------------------------
# 04. OUTPUT FILES
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

SUMMARY_GATE_FILE <- file.path(
  AUDIT_ROOT,
  "geography_06e_summary_gate.csv"
)


# ------------------------------------------------------------------------------
# 05. HELPER FUNCTIONS
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


safe_numeric <- function(x) {
  
  y <- suppressWarnings(
    as.numeric(x)
  )
  
  y[is.na(y)] <- 0
  
  return(y)
}


safe_stat <- function(x, statistic) {
  
  x <- as.numeric(x)
  
  x <- x[
    is.finite(x)
  ]
  
  if (length(x) == 0L) {
    return(NA_real_)
  }
  
  if (statistic == "min") {
    return(min(x))
  }
  
  if (statistic == "q1") {
    return(
      as.numeric(
        quantile(
          x,
          0.25,
          names = FALSE
        )
      )
    )
  }
  
  if (statistic == "median") {
    return(median(x))
  }
  
  if (statistic == "mean") {
    return(mean(x))
  }
  
  if (statistic == "q3") {
    return(
      as.numeric(
        quantile(
          x,
          0.75,
          names = FALSE
        )
      )
    )
  }
  
  if (statistic == "max") {
    return(max(x))
  }
  
  return(NA_real_)
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
# 06. START
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" VPJD GEOGRAPHY 06e - BUILD RELEASE SUMMARIES\n")
cat("============================================================\n\n")

cat("Release:", RELEASE_NAME, "\n")
cat("Build date:", BUILD_DATE, "\n\n")


# ------------------------------------------------------------------------------
# 07. LOCATE TAXONOMIC INTERFACE
# ------------------------------------------------------------------------------

cat("Locating recognised-taxon table...\n")

RECOGNISED_TAXA_FILE <- find_recognised_taxon_file(
  PUBLISHED_VPJD_ROOT
)

cat(
  "Recognised-taxon table:",
  RECOGNISED_TAXA_FILE,
  "\n\n"
)


# ------------------------------------------------------------------------------
# 08. CHECK INPUTS
# ------------------------------------------------------------------------------

required_inputs <- c(
  RECOGNISED_TAXA_FILE,
  GEOGRAPHY_FILE,
  DISTRIBUTION_FILE,
  EVIDENCE_FILE,
  METADATA_GATE_FILE
)

input_exists <- file.exists(
  required_inputs
)

if (!all(input_exists)) {
  
  missing_inputs <- required_inputs[
    !input_exists
  ]
  
  stop(
    paste(
      "Required input files are missing:",
      paste(
        missing_inputs,
        collapse = "\n"
      ),
      sep = "\n\n"
    )
  )
}

cat("Required inputs: PASS\n\n")


# ------------------------------------------------------------------------------
# 09. READ INPUTS
# ------------------------------------------------------------------------------

cat("Reading frozen release products...\n")

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

metadata_gate <- safe_read_csv(
  METADATA_GATE_FILE
)

cat("Frozen release products read successfully.\n\n")


# ------------------------------------------------------------------------------
# 10. REQUIRE 06d METADATA GATE
# ------------------------------------------------------------------------------

required_gate_fields <- c(
  "criterion",
  "passed"
)

if (!all(required_gate_fields %in% names(metadata_gate))) {
  
  stop(
    "06d metadata gate does not contain criterion and passed fields."
  )
}

gate_values <- toupper(
  normalise_text(
    metadata_gate$passed
  )
)

metadata_gate_pass <- all(
  gate_values %in% c(
    "TRUE",
    "T",
    "1"
  )
)

if (!metadata_gate_pass) {
  
  stop(
    paste(
      "06d metadata gate is not completely green.",
      "Release summaries must not be built."
    )
  )
}

cat("06d metadata gate: PASS\n\n")


# ------------------------------------------------------------------------------
# 11. REQUIRED FIELDS
# ------------------------------------------------------------------------------

required_taxon_fields <- c(
  "wcvp_plant_name_id",
  "wcvp_taxon_name",
  "wcvp_family",
  "wcvp_genus"
)

required_geography_fields <- c(
  "geographic_unit_id",
  "geographic_code",
  "geographic_name",
  "concept_group"
)

required_distribution_fields <- c(
  "wcvp_plant_name_id",
  "wcvp_taxon_name",
  "geographic_unit_id",
  "geographic_code",
  "geographic_name",
  "concept_group"
)

required_evidence_fields <- c(
  "wcvp_plant_name_id",
  "geographic_unit_id",
  "n_occurrence_records",
  "n_unique_occurrences"
)

missing_taxon_fields <- setdiff(
  required_taxon_fields,
  names(taxa)
)

missing_geography_fields <- setdiff(
  required_geography_fields,
  names(geography)
)

missing_distribution_fields <- setdiff(
  required_distribution_fields,
  names(distribution)
)

missing_evidence_fields <- setdiff(
  required_evidence_fields,
  names(evidence)
)

if (length(missing_taxon_fields) > 0L) {
  
  stop(
    paste(
      "Missing recognised-taxon fields:",
      paste(
        missing_taxon_fields,
        collapse = ", "
      )
    )
  )
}

if (length(missing_geography_fields) > 0L) {
  
  stop(
    paste(
      "Missing geography fields:",
      paste(
        missing_geography_fields,
        collapse = ", "
      )
    )
  )
}

if (length(missing_distribution_fields) > 0L) {
  
  stop(
    paste(
      "Missing distribution fields:",
      paste(
        missing_distribution_fields,
        collapse = ", "
      )
    )
  )
}

if (length(missing_evidence_fields) > 0L) {
  
  stop(
    paste(
      "Missing evidence fields:",
      paste(
        missing_evidence_fields,
        collapse = ", "
      )
    )
  )
}

cat("Required fields: PASS\n\n")


# ------------------------------------------------------------------------------
# 12. NORMALISE CORE IDENTIFIERS
# ------------------------------------------------------------------------------

taxa$wcvp_plant_name_id <- normalise_text(
  taxa$wcvp_plant_name_id
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

geography$geographic_unit_id <- normalise_text(
  geography$geographic_unit_id
)


# ------------------------------------------------------------------------------
# 13. VERIFY FROZEN RELEASE COUNTS
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

baseline_checks <- c(
  taxon_count == EXPECTED_RECOGNISED_TAXA,
  geographic_concept_count == EXPECTED_GEOGRAPHIC_CONCEPTS,
  japan_area_count == EXPECTED_JAPAN_BOTANICAL_AREAS,
  wcvp_reference_count == EXPECTED_WCVP_TDWG_CONCEPTS,
  distribution_count == EXPECTED_POSITIVE_RELATIONSHIPS,
  evidence_count == EXPECTED_POSITIVE_RELATIONSHIPS,
  complete_combination_count == EXPECTED_COMPLETE_COMBINATIONS,
  unsupported_combination_count == EXPECTED_UNSUPPORTED_COMBINATIONS,
  taxa_with_evidence_count == EXPECTED_TAXA_WITH_EVIDENCE,
  taxa_without_evidence_count == EXPECTED_TAXA_WITHOUT_EVIDENCE
)

if (!all(baseline_checks)) {
  
  stop(
    paste(
      "Frozen release counts do not match the validated release.",
      "Release summaries must not be generated."
    )
  )
}

cat("Frozen release baseline: PASS\n\n")


# ------------------------------------------------------------------------------
# 14. BUILD TAXON COVERAGE COUNTS
# ------------------------------------------------------------------------------

cat("Building taxon geographic-coverage summary...\n")

taxon_area_counts <- aggregate(
  geographic_unit_id ~ wcvp_plant_name_id,
  data = distribution,
  FUN = function(x) {
    length(
      unique(x)
    )
  }
)

names(taxon_area_counts)[2L] <- "n_botanical_areas"


# ------------------------------------------------------------------------------
# 15. BUILD TAXON EVIDENCE COUNTS
# ------------------------------------------------------------------------------

evidence$n_occurrence_records <- safe_numeric(
  evidence$n_occurrence_records
)

evidence$n_unique_occurrences <- safe_numeric(
  evidence$n_unique_occurrences
)

taxon_occurrence_counts <- aggregate(
  n_occurrence_records ~ wcvp_plant_name_id,
  data = evidence,
  FUN = sum
)

names(taxon_occurrence_counts)[2L] <- "n_occurrence_records"

taxon_unique_counts <- aggregate(
  n_unique_occurrences ~ wcvp_plant_name_id,
  data = evidence,
  FUN = sum
)

names(taxon_unique_counts)[2L] <- "n_unique_occurrences"


# ------------------------------------------------------------------------------
# 16. BUILD COMPLETE TAXON COVERAGE TABLE
# ------------------------------------------------------------------------------

taxon_coverage <- taxa[
  ,
  c(
    "wcvp_plant_name_id",
    "wcvp_taxon_name",
    "wcvp_family",
    "wcvp_genus"
  ),
  drop = FALSE
]

taxon_coverage <- merge(
  taxon_coverage,
  taxon_area_counts,
  by = "wcvp_plant_name_id",
  all.x = TRUE,
  sort = FALSE
)

taxon_coverage <- merge(
  taxon_coverage,
  taxon_occurrence_counts,
  by = "wcvp_plant_name_id",
  all.x = TRUE,
  sort = FALSE
)

taxon_coverage <- merge(
  taxon_coverage,
  taxon_unique_counts,
  by = "wcvp_plant_name_id",
  all.x = TRUE,
  sort = FALSE
)

taxon_coverage$n_botanical_areas[
  is.na(taxon_coverage$n_botanical_areas)
] <- 0L

taxon_coverage$n_occurrence_records[
  is.na(taxon_coverage$n_occurrence_records)
] <- 0

taxon_coverage$n_unique_occurrences[
  is.na(taxon_coverage$n_unique_occurrences)
] <- 0

taxon_coverage$has_positive_geographic_evidence <- (
  taxon_coverage$n_botanical_areas > 0
)

taxon_coverage <- taxon_coverage[
  order(
    -taxon_coverage$n_botanical_areas,
    -taxon_coverage$n_unique_occurrences,
    taxon_coverage$wcvp_taxon_name
  ),
  ,
  drop = FALSE
]

rownames(taxon_coverage) <- NULL

write.csv(
  taxon_coverage,
  TAXON_COVERAGE_FILE,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)

cat(
  "Taxon coverage rows:",
  nrow(taxon_coverage),
  "\n"
)


# ------------------------------------------------------------------------------
# 17. BUILD BOTANICAL-AREA RICHNESS TABLE
# ------------------------------------------------------------------------------

cat("Building botanical-area richness summary...\n")

japan_geography <- geography[
  japan_selector,
  ,
  drop = FALSE
]

area_taxon_counts <- aggregate(
  wcvp_plant_name_id ~ geographic_unit_id,
  data = distribution,
  FUN = function(x) {
    length(
      unique(x)
    )
  }
)

names(area_taxon_counts)[2L] <- "n_taxa"

area_occurrence_counts <- aggregate(
  n_occurrence_records ~ geographic_unit_id,
  data = evidence,
  FUN = sum
)

names(area_occurrence_counts)[2L] <- "n_occurrence_records"

area_unique_counts <- aggregate(
  n_unique_occurrences ~ geographic_unit_id,
  data = evidence,
  FUN = sum
)

names(area_unique_counts)[2L] <- "n_unique_occurrences"

area_richness <- japan_geography[
  ,
  c(
    "geographic_unit_id",
    "geographic_code",
    "geographic_name"
  ),
  drop = FALSE
]

area_richness <- merge(
  area_richness,
  area_taxon_counts,
  by = "geographic_unit_id",
  all.x = TRUE,
  sort = FALSE
)

area_richness <- merge(
  area_richness,
  area_occurrence_counts,
  by = "geographic_unit_id",
  all.x = TRUE,
  sort = FALSE
)

area_richness <- merge(
  area_richness,
  area_unique_counts,
  by = "geographic_unit_id",
  all.x = TRUE,
  sort = FALSE
)

area_richness$n_taxa[
  is.na(area_richness$n_taxa)
] <- 0L

area_richness$n_occurrence_records[
  is.na(area_richness$n_occurrence_records)
] <- 0

area_richness$n_unique_occurrences[
  is.na(area_richness$n_unique_occurrences)
] <- 0

area_richness$proportion_of_recognised_taxa <- (
  area_richness$n_taxa /
    taxon_count
)

area_richness <- area_richness[
  order(
    -area_richness$n_taxa,
    -area_richness$n_unique_occurrences,
    area_richness$geographic_name
  ),
  ,
  drop = FALSE
]

rownames(area_richness) <- NULL

write.csv(
  area_richness,
  AREA_RICHNESS_FILE,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)

cat(
  "Botanical-area rows:",
  nrow(area_richness),
  "\n"
)


# ------------------------------------------------------------------------------
# 18. TAXON COVERAGE FREQUENCY DISTRIBUTION
# ------------------------------------------------------------------------------

cat("Building taxon coverage-frequency distribution...\n")

coverage_levels <- 0:japan_area_count

coverage_frequency <- data.frame(
  n_botanical_areas = coverage_levels,
  stringsAsFactors = FALSE
)

coverage_table <- table(
  factor(
    taxon_coverage$n_botanical_areas,
    levels = coverage_levels
  )
)

coverage_frequency$n_taxa <- as.integer(
  coverage_table
)

coverage_frequency$proportion_of_taxa <- (
  coverage_frequency$n_taxa /
    taxon_count
)

coverage_frequency$percentage_of_taxa <- (
  coverage_frequency$proportion_of_taxa *
    100
)

write.csv(
  coverage_frequency,
  COVERAGE_FREQUENCY_FILE,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)


# ------------------------------------------------------------------------------
# 19. EVIDENCE SUPPORT SUMMARY
# ------------------------------------------------------------------------------

cat("Building evidence-support summary...\n")

occurrence_values <- evidence$n_occurrence_records

unique_values <- evidence$n_unique_occurrences

evidence_support_summary <- data.frame(
  metric = c(
    "relationship_count",
    "occurrence_records_total",
    "occurrence_records_min",
    "occurrence_records_q1",
    "occurrence_records_median",
    "occurrence_records_mean",
    "occurrence_records_q3",
    "occurrence_records_max",
    "unique_occurrences_total",
    "unique_occurrences_min",
    "unique_occurrences_q1",
    "unique_occurrences_median",
    "unique_occurrences_mean",
    "unique_occurrences_q3",
    "unique_occurrences_max"
  ),
  value = c(
    nrow(evidence),
    sum(occurrence_values),
    safe_stat(occurrence_values, "min"),
    safe_stat(occurrence_values, "q1"),
    safe_stat(occurrence_values, "median"),
    safe_stat(occurrence_values, "mean"),
    safe_stat(occurrence_values, "q3"),
    safe_stat(occurrence_values, "max"),
    sum(unique_values),
    safe_stat(unique_values, "min"),
    safe_stat(unique_values, "q1"),
    safe_stat(unique_values, "median"),
    safe_stat(unique_values, "mean"),
    safe_stat(unique_values, "q3"),
    safe_stat(unique_values, "max")
  ),
  stringsAsFactors = FALSE
)

write.csv(
  evidence_support_summary,
  EVIDENCE_SUPPORT_FILE,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)


# ------------------------------------------------------------------------------
# 20. FAMILY SUMMARY
# ------------------------------------------------------------------------------

cat("Building family-level summary...\n")

family_values <- normalise_text(
  taxon_coverage$wcvp_family
)

family_values[
  is.na(family_values)
] <- "UNASSIGNED"

taxon_coverage$summary_family <- family_values

family_taxa <- aggregate(
  wcvp_plant_name_id ~ summary_family,
  data = taxon_coverage,
  FUN = length
)

names(family_taxa) <- c(
  "wcvp_family",
  "n_recognised_taxa"
)

family_represented <- aggregate(
  has_positive_geographic_evidence ~ summary_family,
  data = taxon_coverage,
  FUN = sum
)

names(family_represented) <- c(
  "wcvp_family",
  "n_taxa_with_positive_evidence"
)

family_area_mean <- aggregate(
  n_botanical_areas ~ summary_family,
  data = taxon_coverage,
  FUN = mean
)

names(family_area_mean) <- c(
  "wcvp_family",
  "mean_botanical_areas_per_taxon"
)

family_area_median <- aggregate(
  n_botanical_areas ~ summary_family,
  data = taxon_coverage,
  FUN = median
)

names(family_area_median) <- c(
  "wcvp_family",
  "median_botanical_areas_per_taxon"
)

family_summary <- merge(
  family_taxa,
  family_represented,
  by = "wcvp_family",
  all = TRUE,
  sort = FALSE
)

family_summary <- merge(
  family_summary,
  family_area_mean,
  by = "wcvp_family",
  all = TRUE,
  sort = FALSE
)

family_summary <- merge(
  family_summary,
  family_area_median,
  by = "wcvp_family",
  all = TRUE,
  sort = FALSE
)

family_summary$n_taxa_without_positive_evidence <- (
  family_summary$n_recognised_taxa -
    family_summary$n_taxa_with_positive_evidence
)

family_summary$proportion_with_positive_evidence <- (
  family_summary$n_taxa_with_positive_evidence /
    family_summary$n_recognised_taxa
)

family_summary <- family_summary[
  order(
    -family_summary$n_recognised_taxa,
    family_summary$wcvp_family
  ),
  ,
  drop = FALSE
]

rownames(family_summary) <- NULL

write.csv(
  family_summary,
  FAMILY_SUMMARY_FILE,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)


# ------------------------------------------------------------------------------
# 21. GENUS SUMMARY
# ------------------------------------------------------------------------------

cat("Building genus-level summary...\n")

genus_values <- normalise_text(
  taxon_coverage$wcvp_genus
)

genus_values[
  is.na(genus_values)
] <- "UNASSIGNED"

taxon_coverage$summary_genus <- genus_values

genus_taxa <- aggregate(
  wcvp_plant_name_id ~ summary_genus,
  data = taxon_coverage,
  FUN = length
)

names(genus_taxa) <- c(
  "wcvp_genus",
  "n_recognised_taxa"
)

genus_represented <- aggregate(
  has_positive_geographic_evidence ~ summary_genus,
  data = taxon_coverage,
  FUN = sum
)

names(genus_represented) <- c(
  "wcvp_genus",
  "n_taxa_with_positive_evidence"
)

genus_area_mean <- aggregate(
  n_botanical_areas ~ summary_genus,
  data = taxon_coverage,
  FUN = mean
)

names(genus_area_mean) <- c(
  "wcvp_genus",
  "mean_botanical_areas_per_taxon"
)

genus_area_median <- aggregate(
  n_botanical_areas ~ summary_genus,
  data = taxon_coverage,
  FUN = median
)

names(genus_area_median) <- c(
  "wcvp_genus",
  "median_botanical_areas_per_taxon"
)

genus_summary <- merge(
  genus_taxa,
  genus_represented,
  by = "wcvp_genus",
  all = TRUE,
  sort = FALSE
)

genus_summary <- merge(
  genus_summary,
  genus_area_mean,
  by = "wcvp_genus",
  all = TRUE,
  sort = FALSE
)

genus_summary <- merge(
  genus_summary,
  genus_area_median,
  by = "wcvp_genus",
  all = TRUE,
  sort = FALSE
)

genus_summary$n_taxa_without_positive_evidence <- (
  genus_summary$n_recognised_taxa -
    genus_summary$n_taxa_with_positive_evidence
)

genus_summary$proportion_with_positive_evidence <- (
  genus_summary$n_taxa_with_positive_evidence /
    genus_summary$n_recognised_taxa
)

genus_summary <- genus_summary[
  order(
    -genus_summary$n_recognised_taxa,
    genus_summary$wcvp_genus
  ),
  ,
  drop = FALSE
]

rownames(genus_summary) <- NULL

write.csv(
  genus_summary,
  GENUS_SUMMARY_FILE,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)


# ------------------------------------------------------------------------------
# 22. DESCRIPTIVE STATISTICS
# ------------------------------------------------------------------------------

cat("Building descriptive statistics...\n")

represented_taxon_coverage <- taxon_coverage$n_botanical_areas[
  taxon_coverage$n_botanical_areas > 0
]

area_taxon_richness <- area_richness$n_taxa

descriptive_statistics <- data.frame(
  domain = c(
    rep("taxon_geographic_coverage", 6L),
    rep("botanical_area_taxon_richness", 6L)
  ),
  statistic = rep(
    c(
      "minimum",
      "first_quartile",
      "median",
      "mean",
      "third_quartile",
      "maximum"
    ),
    2L
  ),
  value = c(
    safe_stat(represented_taxon_coverage, "min"),
    safe_stat(represented_taxon_coverage, "q1"),
    safe_stat(represented_taxon_coverage, "median"),
    safe_stat(represented_taxon_coverage, "mean"),
    safe_stat(represented_taxon_coverage, "q3"),
    safe_stat(represented_taxon_coverage, "max"),
    safe_stat(area_taxon_richness, "min"),
    safe_stat(area_taxon_richness, "q1"),
    safe_stat(area_taxon_richness, "median"),
    safe_stat(area_taxon_richness, "mean"),
    safe_stat(area_taxon_richness, "q3"),
    safe_stat(area_taxon_richness, "max")
  ),
  stringsAsFactors = FALSE
)

write.csv(
  descriptive_statistics,
  DESCRIPTIVE_STATISTICS_FILE,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)


# ------------------------------------------------------------------------------
# 23. RELEASE OVERVIEW
# ------------------------------------------------------------------------------

relationship_density <- (
  distribution_count /
    complete_combination_count
)

taxon_evidence_proportion <- (
  taxa_with_evidence_count /
    taxon_count
)

overview <- data.frame(
  metric = c(
    "recognised_taxa",
    "geographic_concepts",
    "japanese_botanical_areas",
    "wcvp_tdwg_reference_concepts",
    "possible_taxon_area_combinations",
    "positive_taxon_area_relationships",
    "unsupported_taxon_area_combinations",
    "taxa_with_positive_geographic_evidence",
    "taxa_without_positive_geographic_evidence",
    "proportion_taxa_with_positive_geographic_evidence",
    "positive_relationship_density"
  ),
  value = c(
    taxon_count,
    geographic_concept_count,
    japan_area_count,
    wcvp_reference_count,
    complete_combination_count,
    distribution_count,
    unsupported_combination_count,
    taxa_with_evidence_count,
    taxa_without_evidence_count,
    taxon_evidence_proportion,
    relationship_density
  ),
  interpretation = c(
    "Recognised taxa in VPJD Taxonomic Release v1.0.0.",
    "Concepts in the frozen VPJD geographic vocabulary.",
    "Japanese botanical areas used in the distribution model.",
    "WCVP/TDWG reference concepts retained in the vocabulary.",
    "Complete taxon x Japanese botanical-area analytical universe.",
    "Relationships supported by positive occurrence evidence.",
    "Combinations without represented positive occurrence evidence.",
    "Taxa represented in at least one positive geographic relationship.",
    "Taxa without a positive geographic relationship represented.",
    "Evidence coverage of recognised taxa; not biological completeness.",
    "Proportion of possible taxon x area combinations supported by positive evidence."
  ),
  stringsAsFactors = FALSE
)

write.csv(
  overview,
  OVERVIEW_FILE,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)


# ------------------------------------------------------------------------------
# 24. INTERNAL RECONCILIATION
# ------------------------------------------------------------------------------

cat("Reconciling descriptive products to frozen release...\n")

coverage_relationship_sum <- sum(
  taxon_coverage$n_botanical_areas
)

area_relationship_sum <- sum(
  area_richness$n_taxa
)

coverage_frequency_taxon_sum <- sum(
  coverage_frequency$n_taxa
)

coverage_frequency_relationship_sum <- sum(
  coverage_frequency$n_botanical_areas *
    coverage_frequency$n_taxa
)

family_taxon_sum <- sum(
  family_summary$n_recognised_taxa
)

family_represented_sum <- sum(
  family_summary$n_taxa_with_positive_evidence
)

genus_taxon_sum <- sum(
  genus_summary$n_recognised_taxa
)

genus_represented_sum <- sum(
  genus_summary$n_taxa_with_positive_evidence
)


# ------------------------------------------------------------------------------
# 25. SUMMARY VALIDATION GATE
# ------------------------------------------------------------------------------

summary_gate <- data.frame(
  criterion = c(
    "06d_metadata_gate_passed",
    "recognised_taxa_12037",
    "geographic_concepts_418",
    "japanese_botanical_areas_51",
    "wcvp_tdwg_reference_concepts_367",
    "positive_relationships_128824",
    "taxa_with_positive_evidence_11470",
    "taxa_without_positive_evidence_567",
    "taxon_coverage_rows_equal_recognised_taxa",
    "area_richness_rows_equal_51",
    "taxon_coverage_relationship_sum_equal_128824",
    "area_richness_relationship_sum_equal_128824",
    "coverage_frequency_taxon_sum_equal_12037",
    "coverage_frequency_relationship_sum_equal_128824",
    "family_taxon_sum_equal_12037",
    "family_represented_sum_equal_11470",
    "genus_taxon_sum_equal_12037",
    "genus_represented_sum_equal_11470",
    "overview_created",
    "taxon_coverage_created",
    "area_richness_created",
    "coverage_frequency_created",
    "evidence_support_summary_created",
    "family_summary_created",
    "genus_summary_created",
    "descriptive_statistics_created"
  ),
  passed = c(
    metadata_gate_pass,
    taxon_count == EXPECTED_RECOGNISED_TAXA,
    geographic_concept_count == EXPECTED_GEOGRAPHIC_CONCEPTS,
    japan_area_count == EXPECTED_JAPAN_BOTANICAL_AREAS,
    wcvp_reference_count == EXPECTED_WCVP_TDWG_CONCEPTS,
    distribution_count == EXPECTED_POSITIVE_RELATIONSHIPS,
    taxa_with_evidence_count == EXPECTED_TAXA_WITH_EVIDENCE,
    taxa_without_evidence_count == EXPECTED_TAXA_WITHOUT_EVIDENCE,
    nrow(taxon_coverage) == EXPECTED_RECOGNISED_TAXA,
    nrow(area_richness) == EXPECTED_JAPAN_BOTANICAL_AREAS,
    coverage_relationship_sum == EXPECTED_POSITIVE_RELATIONSHIPS,
    area_relationship_sum == EXPECTED_POSITIVE_RELATIONSHIPS,
    coverage_frequency_taxon_sum == EXPECTED_RECOGNISED_TAXA,
    coverage_frequency_relationship_sum == EXPECTED_POSITIVE_RELATIONSHIPS,
    family_taxon_sum == EXPECTED_RECOGNISED_TAXA,
    family_represented_sum == EXPECTED_TAXA_WITH_EVIDENCE,
    genus_taxon_sum == EXPECTED_RECOGNISED_TAXA,
    genus_represented_sum == EXPECTED_TAXA_WITH_EVIDENCE,
    file.exists(OVERVIEW_FILE),
    file.exists(TAXON_COVERAGE_FILE),
    file.exists(AREA_RICHNESS_FILE),
    file.exists(COVERAGE_FREQUENCY_FILE),
    file.exists(EVIDENCE_SUPPORT_FILE),
    file.exists(FAMILY_SUMMARY_FILE),
    file.exists(GENUS_SUMMARY_FILE),
    file.exists(DESCRIPTIVE_STATISTICS_FILE)
  ),
  stringsAsFactors = FALSE
)

write.csv(
  summary_gate,
  SUMMARY_GATE_FILE,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)

summary_gate_pass <- all(
  summary_gate$passed
)


# ------------------------------------------------------------------------------
# 26. EXTRACT DESCRIPTIVE VALUES FOR TEXT SUMMARY
# ------------------------------------------------------------------------------

taxon_coverage_min <- safe_stat(
  represented_taxon_coverage,
  "min"
)

taxon_coverage_q1 <- safe_stat(
  represented_taxon_coverage,
  "q1"
)

taxon_coverage_median <- safe_stat(
  represented_taxon_coverage,
  "median"
)

taxon_coverage_mean <- safe_stat(
  represented_taxon_coverage,
  "mean"
)

taxon_coverage_q3 <- safe_stat(
  represented_taxon_coverage,
  "q3"
)

taxon_coverage_max <- safe_stat(
  represented_taxon_coverage,
  "max"
)

area_richness_min <- safe_stat(
  area_taxon_richness,
  "min"
)

area_richness_q1 <- safe_stat(
  area_taxon_richness,
  "q1"
)

area_richness_median <- safe_stat(
  area_taxon_richness,
  "median"
)

area_richness_mean <- safe_stat(
  area_taxon_richness,
  "mean"
)

area_richness_q3 <- safe_stat(
  area_taxon_richness,
  "q3"
)

area_richness_max <- safe_stat(
  area_taxon_richness,
  "max"
)


# ------------------------------------------------------------------------------
# 27. TEXT RELEASE SUMMARY
# ------------------------------------------------------------------------------

summary_lines <- c(
  "VASCULAR PLANTS OF JAPAN DATABASE (VPJD)",
  "GEOGRAPHY RELEASE v1.0.0",
  "",
  "DESCRIPTIVE RELEASE SUMMARY",
  "",
  paste0(
    "Summary build date: ",
    BUILD_DATE
  ),
  "",
  "RELEASE ARCHITECTURE",
  "",
  paste(
    "VPJD Geography v1.0.0 links recognised taxa from",
    "VPJD Taxonomic Release v1.0.0 to a frozen vocabulary",
    "of Japanese botanical areas using explicit positive",
    "occurrence evidence."
  ),
  "",
  "CORE COUNTS",
  "",
  paste0(
    "Recognised taxa: ",
    format(
      taxon_count,
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
  paste0(
    "Possible taxon x area combinations: ",
    format(
      complete_combination_count,
      big.mark = ","
    )
  ),
  paste0(
    "Unsupported combinations: ",
    format(
      unsupported_combination_count,
      big.mark = ","
    )
  ),
  "",
  "TAXON GEOGRAPHIC COVERAGE",
  "",
  paste0(
    "Minimum botanical areas per represented taxon: ",
    round(
      taxon_coverage_min,
      2
    )
  ),
  paste0(
    "First quartile: ",
    round(
      taxon_coverage_q1,
      2
    )
  ),
  paste0(
    "Median: ",
    round(
      taxon_coverage_median,
      2
    )
  ),
  paste0(
    "Mean: ",
    round(
      taxon_coverage_mean,
      2
    )
  ),
  paste0(
    "Third quartile: ",
    round(
      taxon_coverage_q3,
      2
    )
  ),
  paste0(
    "Maximum botanical areas per represented taxon: ",
    round(
      taxon_coverage_max,
      2
    )
  ),
  "",
  "BOTANICAL-AREA TAXON RICHNESS",
  "",
  paste0(
    "Minimum represented taxa per botanical area: ",
    round(
      area_richness_min,
      2
    )
  ),
  paste0(
    "First quartile: ",
    round(
      area_richness_q1,
      2
    )
  ),
  paste0(
    "Median: ",
    round(
      area_richness_median,
      2
    )
  ),
  paste0(
    "Mean: ",
    round(
      area_richness_mean,
      2
    )
  ),
  paste0(
    "Third quartile: ",
    round(
      area_richness_q3,
      2
    )
  ),
  paste0(
    "Maximum represented taxa per botanical area: ",
    round(
      area_richness_max,
      2
    )
  ),
  "",
  "INTERPRETATION",
  "",
  paste(
    "All geographic relationships in this release represent",
    "positive occurrence evidence."
  ),
  "",
  paste(
    "A taxon x geographic-unit combination that is not represented",
    "in the release is unsupported by the current positive evidence",
    "model and must not be interpreted as biological absence."
  ),
  "",
  paste(
    "Descriptive measures of geographic coverage therefore describe",
    "the structure of VPJD evidence, not definitive biological range size."
  ),
  "",
  "SCOPE",
  "",
  "This release does not infer:",
  "",
  "- biological absence;",
  "- endemicity;",
  "- rarity;",
  "- Nakamura Key-to-Stars categories;",
  "- Star categories.",
  "",
  "VALIDATION",
  "",
  paste0(
    "Taxon coverage relationships reconciled to: ",
    format(
      coverage_relationship_sum,
      big.mark = ","
    )
  ),
  paste0(
    "Area richness relationships reconciled to: ",
    format(
      area_relationship_sum,
      big.mark = ","
    )
  ),
  paste0(
    "Taxa represented in coverage-frequency table: ",
    format(
      coverage_frequency_taxon_sum,
      big.mark = ","
    )
  ),
  "",
  paste0(
    "06e summary validation gate: ",
    ifelse(
      summary_gate_pass,
      "PASS",
      "FAIL"
    )
  )
)

write_utf8_lines(
  summary_lines,
  TEXT_SUMMARY_FILE
)


# ------------------------------------------------------------------------------
# 28. CONSOLE OUTPUT - OVERVIEW
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" RELEASE OVERVIEW\n")
cat("============================================================\n\n")

print(
  overview,
  row.names = FALSE
)


# ------------------------------------------------------------------------------
# 29. CONSOLE OUTPUT - DESCRIPTIVE STATISTICS
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" DESCRIPTIVE STATISTICS\n")
cat("============================================================\n\n")

print(
  descriptive_statistics,
  row.names = FALSE
)


# ------------------------------------------------------------------------------
# 30. CONSOLE OUTPUT - TOP BOTANICAL AREAS
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" BOTANICAL AREAS - HIGHEST TAXON REPRESENTATION\n")
cat("============================================================\n\n")

top_area_n <- min(
  10L,
  nrow(area_richness)
)

print(
  area_richness[
    seq_len(top_area_n),
    c(
      "geographic_code",
      "geographic_name",
      "n_taxa",
      "n_unique_occurrences"
    ),
    drop = FALSE
  ],
  row.names = FALSE
)


# ------------------------------------------------------------------------------
# 31. CONSOLE OUTPUT - LOWEST BOTANICAL AREAS
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" BOTANICAL AREAS - LOWEST TAXON REPRESENTATION\n")
cat("============================================================\n\n")

area_richness_ascending <- area_richness[
  order(
    area_richness$n_taxa,
    area_richness$geographic_name
  ),
  ,
  drop = FALSE
]

bottom_area_n <- min(
  10L,
  nrow(area_richness_ascending)
)

print(
  area_richness_ascending[
    seq_len(bottom_area_n),
    c(
      "geographic_code",
      "geographic_name",
      "n_taxa",
      "n_unique_occurrences"
    ),
    drop = FALSE
  ],
  row.names = FALSE
)


# ------------------------------------------------------------------------------
# 32. CONSOLE OUTPUT - COVERAGE FREQUENCY
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" TAXON GEOGRAPHIC-COVERAGE FREQUENCY\n")
cat("============================================================\n\n")

print(
  coverage_frequency,
  row.names = FALSE
)


# ------------------------------------------------------------------------------
# 33. CONSOLE OUTPUT - SUMMARY GATE
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" SUMMARY VALIDATION GATE\n")
cat("============================================================\n\n")

print(
  summary_gate,
  row.names = FALSE
)


# ------------------------------------------------------------------------------
# 34. INTERPRETATION
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" INTERPRETATION\n")
cat("============================================================\n\n")

if (summary_gate_pass) {
  
  cat(
    "PASS. VPJD Geography v1.0.0 descriptive summaries were built successfully.\n"
  )
  
  cat(
    "All aggregate relationship counts reconcile to the frozen release.\n"
  )
  
  cat(
    "No upstream release product was modified.\n"
  )
  
  cat(
    "No unsupported relationship was interpreted as biological absence.\n"
  )
  
} else {
  
  cat(
    "FAIL. One or more 06e summary validation criteria did not pass.\n"
  )
}


# ------------------------------------------------------------------------------
# 35. OUTPUT FILES
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" OUTPUT FILES\n")
cat("============================================================\n\n")

summary_outputs <- c(
  OVERVIEW_FILE,
  TAXON_COVERAGE_FILE,
  AREA_RICHNESS_FILE,
  COVERAGE_FREQUENCY_FILE,
  EVIDENCE_SUPPORT_FILE,
  FAMILY_SUMMARY_FILE,
  GENUS_SUMMARY_FILE,
  DESCRIPTIVE_STATISTICS_FILE,
  TEXT_SUMMARY_FILE,
  SUMMARY_GATE_FILE
)

for (output_path in summary_outputs) {
  
  cat(
    output_path,
    "\n"
  )
}


# ------------------------------------------------------------------------------
# 36. SAFEGUARDS
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" SAFEGUARDS\n")
cat("============================================================\n\n")

cat("Canonical geographic vocabulary was not modified.\n")
cat("Canonical taxon distribution was not modified.\n")
cat("Canonical evidence table was not modified.\n")
cat("VPJD Taxonomic Release v1.0.0 was not modified.\n")
cat("No biological absence was inferred.\n")
cat("No endemicity was inferred.\n")
cat("No rarity was inferred.\n")
cat("No Nakamura logic was applied.\n")
cat("No Star categories were assigned.\n")


# ------------------------------------------------------------------------------
# 37. NEXT STEP
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" NEXT STEP\n")
cat("============================================================\n\n")

if (summary_gate_pass) {
  
  cat("06e completed successfully.\n\n")
  
  cat(
    paste(
      "The frozen VPJD Geography v1.0.0 release now has",
      "validated metadata and descriptive release summaries."
    ),
    "\n\n"
  )
  
  cat("Recommended next script:\n\n")
  
  cat(
    "geography_06f_audit_release_package.R\n\n"
  )
  
  cat(
    paste(
      "06f should perform the final independent release-package audit,",
      "including required-file checks, row counts, schema checks,",
      "referential integrity, metadata completeness, file hashes,",
      "and final Zenodo packaging readiness."
    ),
    "\n"
  )
  
} else {
  
  cat(
    paste(
      "06e summary validation failed.",
      "Do not proceed to final release-package auditing."
    ),
    "\n"
  )
}

cat("\n")
cat("============================================================\n")