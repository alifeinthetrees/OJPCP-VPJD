# ==============================================================================
# VPJD GEOGRAPHY MODULE
# geography_06d_build_release_metadata.R
#
# PURPOSE
# -------
# Build the formal metadata layer for:
#
#   Vascular Plants of Japan Database (VPJD):
#   Geography Release v1.0.0
#
# This script documents the frozen release. It does not modify the canonical
# distribution, evidence table, geographic vocabulary, or taxonomic release.
# ==============================================================================


# ------------------------------------------------------------------------------
# 01. RELEASE CONSTANTS
# ------------------------------------------------------------------------------

RELEASE_TITLE <- paste(
  "Vascular Plants of Japan Database (VPJD):",
  "Geography Release v1.0.0"
)

RELEASE_SHORT_NAME <- "VPJD Geography v1.0.0"
RELEASE_VERSION <- "1.0.0"
RELEASE_TYPE <- "dataset"

SCRIPT_NAME <- "geography_06d_build_release_metadata.R"

BUILD_DATE <- format(
  Sys.Date(),
  "%Y-%m-%d"
)

TAXONOMIC_RELEASE_NAME <- "VPJD Taxonomic Release v1.0.0"


# ------------------------------------------------------------------------------
# 02. EXPECTED RELEASE STATISTICS
# ------------------------------------------------------------------------------

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
# 03. PROJECT PATHS
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

RELEASE_ROOT <- file.path(
  DERIVED_GEOGRAPHY_ROOT,
  "release_v1.0.0"
)

FROZEN_GEOGRAPHY_ROOT <- file.path(
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

VALIDATION_ROOT <- file.path(
  PROJECT_ROOT,
  "audit",
  "geography",
  "release_v1.0.0",
  "06c1_validate_taxon_geographic_distribution"
)

AUDIT_ROOT <- file.path(
  PROJECT_ROOT,
  "audit",
  "geography",
  "release_v1.0.0",
  "06d_build_release_metadata"
)

dir.create(
  METADATA_ROOT,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  AUDIT_ROOT,
  recursive = TRUE,
  showWarnings = FALSE
)


# ------------------------------------------------------------------------------
# 04. INPUT FILES
# ------------------------------------------------------------------------------

GEOGRAPHY_FILE <- file.path(
  FROZEN_GEOGRAPHY_ROOT,
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

RELEASE_GATE_INPUT_FILE <- file.path(
  VALIDATION_ROOT,
  "geography_06c1_release_gate.csv"
)

DISTRIBUTION_SUMMARY_INPUT_FILE <- file.path(
  VALIDATION_ROOT,
  "geography_06c1_distribution_summary.csv"
)


# ------------------------------------------------------------------------------
# 05. OUTPUT FILES
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

METADATA_GATE_FILE <- file.path(
  AUDIT_ROOT,
  "geography_06d_metadata_gate.csv"
)


# ------------------------------------------------------------------------------
# 06. HELPER FUNCTIONS
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
    
    sizes <- file.info(matches)$size
    
    matches <- matches[
      order(
        sizes,
        decreasing = TRUE
      )
    ]
  }
  
  return(matches[1L])
}


calculate_sha256 <- function(path) {
  
  if (!file.exists(path)) {
    return(NA_character_)
  }
  
  if (!requireNamespace("digest", quietly = TRUE)) {
    return(NA_character_)
  }
  
  hash_value <- digest::digest(
    file = path,
    algo = "sha256",
    serialize = FALSE
  )
  
  return(hash_value)
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
# 07. START
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" VPJD GEOGRAPHY 06d - BUILD RELEASE METADATA\n")
cat("============================================================\n\n")

cat("Release:", RELEASE_SHORT_NAME, "\n")
cat("Build date:", BUILD_DATE, "\n\n")


# ------------------------------------------------------------------------------
# 08. LOCATE TAXONOMIC RELEASE
# ------------------------------------------------------------------------------

cat("Locating VPJD Taxonomic Release v1.0.0...\n")

RECOGNISED_TAXA_FILE <- find_recognised_taxon_file(
  PUBLISHED_VPJD_ROOT
)

cat(
  "Recognised-taxon table:",
  RECOGNISED_TAXA_FILE,
  "\n\n"
)


# ------------------------------------------------------------------------------
# 09. CHECK REQUIRED INPUT FILES
# ------------------------------------------------------------------------------

required_inputs <- c(
  RECOGNISED_TAXA_FILE,
  GEOGRAPHY_FILE,
  DISTRIBUTION_FILE,
  EVIDENCE_FILE,
  RELEASE_GATE_INPUT_FILE,
  DISTRIBUTION_SUMMARY_INPUT_FILE
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
# 10. READ INPUT PRODUCTS
# ------------------------------------------------------------------------------

cat("Reading validated release products...\n")

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

release_gate_input <- safe_read_csv(
  RELEASE_GATE_INPUT_FILE
)

distribution_summary_input <- safe_read_csv(
  DISTRIBUTION_SUMMARY_INPUT_FILE
)

cat("Input products read successfully.\n\n")


# ------------------------------------------------------------------------------
# 11. REQUIRE 06c1 RELEASE GATE TO PASS
# ------------------------------------------------------------------------------

required_gate_fields <- c(
  "criterion",
  "passed"
)

if (!all(required_gate_fields %in% names(release_gate_input))) {
  
  stop(
    "06c1 release gate does not contain criterion and passed fields."
  )
}

release_gate_values <- toupper(
  normalise_text(
    release_gate_input$passed
  )
)

release_gate_pass <- all(
  release_gate_values %in% c(
    "TRUE",
    "T",
    "1"
  )
)

if (!release_gate_pass) {
  
  stop(
    paste(
      "06c1 release gate is not completely green.",
      "Release metadata must not be built."
    )
  )
}

cat("06c1 independent release gate: PASS\n\n")


# ------------------------------------------------------------------------------
# 12. CHECK REQUIRED CORE FIELDS
# ------------------------------------------------------------------------------

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

cat("Required product fields: PASS\n\n")


# ------------------------------------------------------------------------------
# 13. RECONSTRUCT CORE RELEASE COUNTS
# ------------------------------------------------------------------------------

taxon_count <- nrow(taxa)

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

distribution_relationship_count <- nrow(
  distribution
)

evidence_relationship_count <- nrow(
  evidence
)

distribution_taxon_ids <- normalise_text(
  distribution$wcvp_plant_name_id
)

distribution_taxon_ids <- distribution_taxon_ids[
  !is.na(distribution_taxon_ids)
]

distribution_taxon_ids <- unique(
  distribution_taxon_ids
)

taxa_with_evidence_count <- length(
  distribution_taxon_ids
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
    distribution_relationship_count
)


# ------------------------------------------------------------------------------
# 14. VERIFY CORE COUNTS
# ------------------------------------------------------------------------------

core_count_checks <- c(
  taxon_count == EXPECTED_RECOGNISED_TAXA,
  geographic_concept_count == EXPECTED_GEOGRAPHIC_CONCEPTS,
  japan_area_count == EXPECTED_JAPAN_BOTANICAL_AREAS,
  wcvp_reference_count == EXPECTED_WCVP_TDWG_CONCEPTS,
  complete_combination_count == EXPECTED_COMPLETE_COMBINATIONS,
  distribution_relationship_count == EXPECTED_POSITIVE_RELATIONSHIPS,
  evidence_relationship_count == EXPECTED_POSITIVE_RELATIONSHIPS,
  unsupported_combination_count == EXPECTED_UNSUPPORTED_COMBINATIONS,
  taxa_with_evidence_count == EXPECTED_TAXA_WITH_EVIDENCE,
  taxa_without_evidence_count == EXPECTED_TAXA_WITHOUT_EVIDENCE
)

if (!all(core_count_checks)) {
  
  stop(
    paste(
      "Core release counts do not match the validated 06c1 release.",
      "Do not construct release metadata."
    )
  )
}

cat("Core release statistics: PASS\n\n")


# ------------------------------------------------------------------------------
# 15. RELEASE METADATA
# ------------------------------------------------------------------------------

release_description <- paste(
  "VPJD Geography v1.0.0 is the geographic evidence release of the",
  "Vascular Plants of Japan Database (VPJD).",
  "It links recognised taxa from VPJD Taxonomic Release v1.0.0",
  "to a frozen vocabulary of Japanese botanical geographic units",
  "using explicit positive occurrence evidence.",
  "Published taxon x geographic-unit relationships represent positive",
  "evidence only.",
  "The absence of a relationship must not be interpreted as evidence",
  "that a taxon is biologically absent from that geographic unit."
)

release_metadata <- data.frame(
  field = c(
    "title",
    "short_name",
    "version",
    "release_type",
    "metadata_build_date",
    "parent_dataset",
    "parent_taxonomic_release",
    "geographic_scope",
    "taxonomic_scope",
    "evidence_scope",
    "description",
    "evidence_interpretation",
    "absence_interpretation",
    "canonical_taxon_identifier",
    "canonical_geographic_identifier",
    "geographic_vocabulary_status",
    "taxonomic_interface_status",
    "nakamura_model_included",
    "star_categories_included",
    "endemicity_inferred",
    "rarity_inferred",
    "licence",
    "zenodo_doi",
    "related_taxonomic_release_doi",
    "repository",
    "metadata_build_script"
  ),
  value = c(
    RELEASE_TITLE,
    RELEASE_SHORT_NAME,
    RELEASE_VERSION,
    RELEASE_TYPE,
    BUILD_DATE,
    "Vascular Plants of Japan Database (VPJD)",
    TAXONOMIC_RELEASE_NAME,
    "Japan",
    paste(
      "Recognised vascular plant taxa represented in",
      "VPJD Taxonomic Release v1.0.0"
    ),
    "Positive taxon x Japanese botanical-area occurrence evidence",
    release_description,
    "Published relationships represent positive occurrence evidence.",
    paste(
      "No relationship means that positive evidence is not represented",
      "for that taxon x geographic-unit combination in this release;",
      "it does not mean biological absence."
    ),
    "wcvp_plant_name_id",
    "geographic_unit_id",
    "FROZEN_VPJD_GEOGRAPHY_V1.0.0",
    TAXONOMIC_RELEASE_NAME,
    "No",
    "No",
    "No",
    "No",
    "TO_CONFIRM_BEFORE_ZENODO_DEPOSIT",
    "TO_BE_ASSIGNED_BY_ZENODO",
    "TO_ADD_EXISTING_TAXONOMIC_RELEASE_DOI",
    "OJPCP-VPJD",
    SCRIPT_NAME
  ),
  stringsAsFactors = FALSE
)

write.csv(
  release_metadata,
  RELEASE_METADATA_FILE,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)


# ------------------------------------------------------------------------------
# 16. RELEASE STATISTICS
# ------------------------------------------------------------------------------

release_statistics <- data.frame(
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
    "distribution_rows",
    "evidence_rows"
  ),
  value = c(
    taxon_count,
    geographic_concept_count,
    japan_area_count,
    wcvp_reference_count,
    complete_combination_count,
    distribution_relationship_count,
    unsupported_combination_count,
    taxa_with_evidence_count,
    taxa_without_evidence_count,
    nrow(distribution),
    nrow(evidence)
  ),
  interpretation = c(
    "Recognised taxa in VPJD Taxonomic Release v1.0.0",
    "All concepts in the frozen VPJD Geography vocabulary",
    "Japanese botanical areas used for published distribution relationships",
    "WCVP/TDWG reference concepts retained in the geographic vocabulary",
    "Recognised taxa multiplied by Japanese botanical areas",
    "Relationships supported by positive occurrence evidence",
    "Combinations without positive occurrence evidence represented",
    "Recognised taxa represented in at least one positive relationship",
    "Recognised taxa without a positive relationship represented",
    "Rows in the canonical distribution product",
    "Rows in the canonical evidence product"
  ),
  stringsAsFactors = FALSE
)

write.csv(
  release_statistics,
  RELEASE_STATISTICS_FILE,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)


# ------------------------------------------------------------------------------
# 17. DATA DICTIONARY DEFINITIONS
# ------------------------------------------------------------------------------

geography_definitions <- c(
  geographic_unit_id =
    "Stable canonical VPJD identifier for the geographic concept.",
  geographic_code =
    "Canonical code associated with the geographic concept.",
  geographic_name =
    "Canonical human-readable geographic name.",
  concept_group =
    "Geographic concept family.",
  identifier_status =
    "Status of the canonical geographic identifier in the frozen vocabulary."
)

distribution_definitions <- c(
  wcvp_plant_name_id =
    "Canonical WCVP plant-name identifier used by VPJD.",
  wcvp_taxon_name =
    "Recognised taxon name associated with the WCVP identifier.",
  geographic_unit_id =
    "Stable canonical VPJD identifier for the Japanese botanical area.",
  geographic_code =
    "Canonical code for the Japanese botanical area.",
  geographic_name =
    "Canonical human-readable name of the Japanese botanical area.",
  concept_group =
    "Geographic concept family.",
  occurrence_status =
    "Interpretive status of the relationship.",
  geography_release =
    "VPJD Geography release in which the relationship is published."
)

evidence_definitions <- c(
  wcvp_plant_name_id =
    "Canonical WCVP plant-name identifier used by VPJD.",
  wcvp_taxon_name =
    "Recognised taxon name associated with the WCVP identifier.",
  geographic_unit_id =
    "Stable canonical VPJD identifier for the Japanese botanical area.",
  geographic_code =
    "Canonical code for the Japanese botanical area.",
  geographic_name =
    "Canonical human-readable name of the Japanese botanical area.",
  concept_group =
    "Geographic concept family.",
  n_occurrence_records =
    "Number of occurrence records supporting the relationship.",
  n_unique_occurrences =
    "Number of unique occurrence records supporting the relationship.",
  occurrence_status =
    "Interpretive status of the relationship.",
  evidence_type =
    "Classification of evidence supporting the relationship.",
  evidence_rule =
    "Explicit rule determining positive evidence.",
  evidence_source_file =
    "Source file associated with the evidence relationship.",
  evidence_source_row =
    "Source-row provenance associated with the evidence relationship.",
  geography_release =
    "VPJD Geography release containing the evidence relationship.",
  build_script =
    "Script responsible for constructing the product.",
  build_date =
    "Date on which the product was constructed."
)


# ------------------------------------------------------------------------------
# 18. DATA DICTIONARY HELPER
# ------------------------------------------------------------------------------

build_dictionary <- function(
    product_name,
    object,
    definitions
) {
  
  field_names <- names(object)
  
  output_rows <- vector(
    "list",
    length(field_names)
  )
  
  for (field_index in seq_along(field_names)) {
    
    field_name <- field_names[field_index]
    
    definition_value <- definitions[field_name]
    
    if (length(definition_value) == 0L) {
      definition_value <- NA_character_
    }
    
    if (is.na(definition_value)) {
      definition_value <- paste(
        "Field present in",
        product_name
      )
    }
    
    output_rows[[field_index]] <- data.frame(
      product = product_name,
      field = field_name,
      r_class = paste(
        class(object[[field_name]]),
        collapse = ";"
      ),
      definition = unname(definition_value),
      stringsAsFactors = FALSE
    )
  }
  
  output <- do.call(
    rbind,
    output_rows
  )
  
  rownames(output) <- NULL
  
  return(output)
}


# ------------------------------------------------------------------------------
# 19. BUILD DATA DICTIONARY
# ------------------------------------------------------------------------------

dictionary_geography <- build_dictionary(
  "vpjd_geographic_units_v1.0.0.csv",
  geography,
  geography_definitions
)

dictionary_distribution <- build_dictionary(
  "vpjd_taxon_geographic_distribution_v1.0.0.csv",
  distribution,
  distribution_definitions
)

dictionary_evidence <- build_dictionary(
  "vpjd_taxon_geographic_evidence_v1.0.0.csv",
  evidence,
  evidence_definitions
)

data_dictionary <- rbind(
  dictionary_geography,
  dictionary_distribution,
  dictionary_evidence
)

rownames(data_dictionary) <- NULL

write.csv(
  data_dictionary,
  DATA_DICTIONARY_FILE,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)


# ------------------------------------------------------------------------------
# 20. PROVENANCE
# ------------------------------------------------------------------------------

provenance <- data.frame(
  stage = c(
    "taxonomy",
    "geographic_vocabulary",
    "distribution_evidence",
    "distribution_build",
    "independent_validation",
    "release_metadata"
  ),
  product = c(
    TAXONOMIC_RELEASE_NAME,
    "Frozen canonical VPJD geographic vocabulary",
    "Positive occurrence evidence model",
    "Canonical taxon x geographic-unit distribution",
    "Independent post-build validation",
    "VPJD Geography v1.0.0 release metadata"
  ),
  script_or_source = c(
    RECOGNISED_TAXA_FILE,
    "geography_06b5_freeze_canonical_geographic_vocabulary.R",
    "Validated analytical occurrence evidence",
    "geography_06c_build_taxon_geographic_distribution.R",
    "geography_06c1_validate_taxon_geographic_distribution.R",
    SCRIPT_NAME
  ),
  role = c(
    "Provides the frozen recognised-taxon interface.",
    "Provides stable canonical geographic concepts and identifiers.",
    "Provides explicit positive occurrence support.",
    "Constructs canonical distribution and evidence products.",
    "Independently validates products read back from disk.",
    "Formalises metadata, provenance and release documentation."
  ),
  modifies_upstream_data = rep(
    "No",
    6L
  ),
  stringsAsFactors = FALSE
)

write.csv(
  provenance,
  PROVENANCE_FILE,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)


# ------------------------------------------------------------------------------
# 21. METHODOLOGICAL SAFEGUARDS
# ------------------------------------------------------------------------------

safeguards <- data.frame(
  safeguard = c(
    "positive_evidence_only",
    "absence_not_inferred",
    "unsupported_not_absent",
    "taxonomic_interface_frozen",
    "geographic_interface_frozen",
    "stable_taxon_identifier",
    "stable_geographic_identifier",
    "wcvp_reference_geography_not_distribution",
    "no_fuzzy_matching_at_release_stage",
    "no_nakamura_logic",
    "no_star_assignment",
    "no_endemicity_inference",
    "no_rarity_inference",
    "independent_post_build_validation"
  ),
  statement = c(
    "Published relationships require explicit positive occurrence evidence.",
    "Missing relationships are not interpreted as biological absence.",
    "Unsupported taxon x area combinations are unsupported, not absent.",
    "Taxonomic relationships resolve to VPJD Taxonomic Release v1.0.0.",
    "Geographic relationships resolve to the frozen VPJD Geography vocabulary.",
    "wcvp_plant_name_id is the canonical taxonomic identifier.",
    "geographic_unit_id is the canonical geographic identifier.",
    "WCVP/TDWG reference geography is not treated as VPJD occurrence evidence.",
    "No fuzzy name matching is performed during release metadata construction.",
    "Nakamura Key-to-Stars logic is outside this release.",
    "Star categories are outside this release.",
    "Endemicity is not inferred by this release.",
    "Rarity is not inferred by this release.",
    "Distribution and evidence products passed independent post-build validation."
  ),
  stringsAsFactors = FALSE
)

write.csv(
  safeguards,
  SAFEGUARDS_FILE,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)


# ------------------------------------------------------------------------------
# 22. README
# ------------------------------------------------------------------------------

readme_lines <- c(
  RELEASE_TITLE,
  paste0("Version: ", RELEASE_VERSION),
  paste0("Metadata build date: ", BUILD_DATE),
  "",
  "OVERVIEW",
  "",
  paste(
    "VPJD Geography v1.0.0 is the geographic evidence release",
    "of the Vascular Plants of Japan Database (VPJD)."
  ),
  "",
  paste(
    "The release links recognised taxa from VPJD Taxonomic Release v1.0.0",
    "to a frozen vocabulary of Japanese botanical areas using explicit",
    "positive occurrence evidence."
  ),
  "",
  "CORE RELEASE STATISTICS",
  "",
  paste0(
    "Recognised taxa: ",
    format(taxon_count, big.mark = ",")
  ),
  paste0(
    "Geographic concepts: ",
    format(geographic_concept_count, big.mark = ",")
  ),
  paste0(
    "Japanese botanical areas: ",
    format(japan_area_count, big.mark = ",")
  ),
  paste0(
    "WCVP/TDWG reference concepts: ",
    format(wcvp_reference_count, big.mark = ",")
  ),
  paste0(
    "Possible taxon x area combinations: ",
    format(complete_combination_count, big.mark = ",")
  ),
  paste0(
    "Positive taxon x area relationships: ",
    format(distribution_relationship_count, big.mark = ",")
  ),
  paste0(
    "Unsupported combinations: ",
    format(unsupported_combination_count, big.mark = ",")
  ),
  paste0(
    "Taxa with positive geographic evidence: ",
    format(taxa_with_evidence_count, big.mark = ",")
  ),
  paste0(
    "Taxa without positive geographic evidence: ",
    format(taxa_without_evidence_count, big.mark = ",")
  ),
  "",
  "EVIDENCE SEMANTICS",
  "",
  "Published relationships represent POSITIVE OCCURRENCE EVIDENCE.",
  "",
  "Release criterion:",
  "",
  "n_occurrence_records > 0",
  "",
  paste(
    "The absence of a taxon x geographic-unit relationship does not",
    "constitute evidence that the taxon is biologically absent."
  ),
  "",
  "CANONICAL IDENTIFIERS",
  "",
  "Taxon identifier: wcvp_plant_name_id",
  "Geographic identifier: geographic_unit_id",
  "",
  "PRINCIPAL DATA PRODUCTS",
  "",
  "vpjd_geographic_units_v1.0.0.csv",
  "Frozen canonical geographic vocabulary.",
  "",
  "vpjd_taxon_geographic_distribution_v1.0.0.csv",
  "Canonical positive taxon x Japanese botanical-area relationships.",
  "",
  "vpjd_taxon_geographic_evidence_v1.0.0.csv",
  "Evidence and provenance supporting each positive relationship.",
  "",
  "SCOPE EXCLUSIONS",
  "",
  "VPJD Geography v1.0.0 does not:",
  "",
  "- infer biological absence;",
  "- infer endemicity;",
  "- infer rarity;",
  "- use WCVP/TDWG reference geography as occurrence evidence;",
  "- apply Nakamura Key-to-Stars logic;",
  "- assign Star categories.",
  "",
  "VALIDATION",
  "",
  paste(
    "The canonical distribution and evidence products passed both",
    "build-time validation and independent post-build validation."
  ),
  "",
  "LICENCE",
  "",
  "TO CONFIRM BEFORE ZENODO DEPOSIT.",
  "",
  "DOI",
  "",
  "TO BE ASSIGNED BY ZENODO.",
  "",
  "RELATED RELEASE",
  "",
  TAXONOMIC_RELEASE_NAME,
  "DOI: TO ADD EXISTING TAXONOMIC RELEASE DOI"
)

write_utf8_lines(
  readme_lines,
  README_FILE
)


# ------------------------------------------------------------------------------
# 23. ZENODO METADATA DRAFT
# ------------------------------------------------------------------------------

zenodo_description <- paste(
  "VPJD Geography v1.0.0 is the geographic evidence release of the",
  "Vascular Plants of Japan Database (VPJD).",
  "The release links 12,037 recognised vascular plant taxa from",
  "VPJD Taxonomic Release v1.0.0 to a frozen geographic vocabulary",
  "containing 51 Japanese botanical areas and 367 WCVP/TDWG reference",
  "concepts.",
  "The principal distribution product contains 128,824 positive",
  "taxon x geographic-unit relationships representing 11,470 recognised",
  "taxa.",
  "A further 567 recognised taxa have no positive geographic evidence",
  "represented in this release.",
  "Across 613,887 possible taxon x Japanese botanical-area combinations,",
  "485,063 combinations are unsupported by positive occurrence evidence.",
  "Unsupported relationships must not be interpreted as biological absences.",
  "The release includes a frozen canonical geographic vocabulary,",
  "a canonical taxon x geographic-unit distribution, and an evidence table",
  "providing occurrence counts and provenance.",
  "The release does not assign endemicity, rarity or Star categories and",
  "does not apply the Nakamura Key-to-Stars model."
)

zenodo_lines <- c(
  "ZENODO METADATA DRAFT",
  "",
  paste0("Title: ", RELEASE_TITLE),
  "",
  "Upload type: Dataset",
  "",
  paste0("Publication date: ", BUILD_DATE),
  "",
  paste0("Version: ", RELEASE_VERSION),
  "",
  "Creators:",
  "Ben Jones - University of Oxford Botanic Garden and Arboretum",
  "",
  "Additional creators/contributors:",
  "TO CONFIRM",
  "",
  "Description:",
  "",
  zenodo_description,
  "",
  "Keywords:",
  "Vascular Plants of Japan Database",
  "VPJD",
  "Japan",
  "vascular plants",
  "plant distribution",
  "occurrence data",
  "biodiversity",
  "biogeography",
  "botanical geography",
  "plant conservation",
  "Key Biodiversity Areas",
  "KBA",
  "World Checklist of Vascular Plants",
  "WCVP",
  "",
  "Licence:",
  "TO CONFIRM BEFORE DEPOSIT",
  "",
  "Related identifier:",
  TAXONOMIC_RELEASE_NAME,
  "DOI: TO ADD",
  "",
  "Zenodo DOI:",
  "TO BE ASSIGNED",
  "",
  "Important interpretation:",
  paste(
    "Published relationships represent positive occurrence evidence only.",
    "Missing relationships must not be interpreted as biological absence."
  )
)

write_utf8_lines(
  zenodo_lines,
  ZENODO_METADATA_FILE
)


# ------------------------------------------------------------------------------
# 24. RELEASE MANIFEST
# ------------------------------------------------------------------------------

manifest_paths <- c(
  GEOGRAPHY_FILE,
  DISTRIBUTION_FILE,
  EVIDENCE_FILE,
  RELEASE_METADATA_FILE,
  DATA_DICTIONARY_FILE,
  RELEASE_STATISTICS_FILE,
  PROVENANCE_FILE,
  SAFEGUARDS_FILE,
  README_FILE,
  ZENODO_METADATA_FILE
)

manifest_roles <- c(
  "canonical_geographic_vocabulary",
  "canonical_taxon_geographic_distribution",
  "canonical_taxon_geographic_evidence",
  "release_metadata",
  "data_dictionary",
  "release_statistics",
  "provenance",
  "methodological_safeguards",
  "readme",
  "zenodo_metadata_draft"
)

manifest_exists <- file.exists(
  manifest_paths
)

manifest_size <- rep(
  NA_real_,
  length(manifest_paths)
)

manifest_sha256 <- rep(
  NA_character_,
  length(manifest_paths)
)

for (manifest_index in seq_along(manifest_paths)) {
  
  current_path <- manifest_paths[manifest_index]
  
  if (file.exists(current_path)) {
    
    manifest_size[manifest_index] <- file.info(
      current_path
    )$size
    
    manifest_sha256[manifest_index] <- calculate_sha256(
      current_path
    )
  }
}

release_manifest <- data.frame(
  file_role = manifest_roles,
  file_name = basename(manifest_paths),
  file_path = manifest_paths,
  exists = manifest_exists,
  size_bytes = manifest_size,
  sha256 = manifest_sha256,
  stringsAsFactors = FALSE
)

write.csv(
  release_manifest,
  RELEASE_MANIFEST_FILE,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)


# ------------------------------------------------------------------------------
# 25. METADATA VALIDATION GATE
# ------------------------------------------------------------------------------

metadata_gate <- data.frame(
  criterion = c(
    "06c1_release_gate_passed",
    "recognised_taxa_12037",
    "geographic_concepts_418",
    "japanese_botanical_areas_51",
    "wcvp_tdwg_reference_concepts_367",
    "complete_combination_universe_613887",
    "positive_distribution_relationships_128824",
    "positive_evidence_relationships_128824",
    "unsupported_combinations_485063",
    "taxa_with_positive_evidence_11470",
    "taxa_without_positive_evidence_567",
    "release_metadata_created",
    "data_dictionary_created",
    "release_statistics_created",
    "release_manifest_created",
    "provenance_created",
    "methodological_safeguards_created",
    "readme_created",
    "zenodo_metadata_draft_created",
    "all_manifest_files_exist",
    "absence_semantics_documented",
    "nakamura_excluded",
    "star_categories_excluded"
  ),
  passed = c(
    release_gate_pass,
    taxon_count == EXPECTED_RECOGNISED_TAXA,
    geographic_concept_count == EXPECTED_GEOGRAPHIC_CONCEPTS,
    japan_area_count == EXPECTED_JAPAN_BOTANICAL_AREAS,
    wcvp_reference_count == EXPECTED_WCVP_TDWG_CONCEPTS,
    complete_combination_count == EXPECTED_COMPLETE_COMBINATIONS,
    distribution_relationship_count == EXPECTED_POSITIVE_RELATIONSHIPS,
    evidence_relationship_count == EXPECTED_POSITIVE_RELATIONSHIPS,
    unsupported_combination_count == EXPECTED_UNSUPPORTED_COMBINATIONS,
    taxa_with_evidence_count == EXPECTED_TAXA_WITH_EVIDENCE,
    taxa_without_evidence_count == EXPECTED_TAXA_WITHOUT_EVIDENCE,
    file.exists(RELEASE_METADATA_FILE),
    file.exists(DATA_DICTIONARY_FILE),
    file.exists(RELEASE_STATISTICS_FILE),
    file.exists(RELEASE_MANIFEST_FILE),
    file.exists(PROVENANCE_FILE),
    file.exists(SAFEGUARDS_FILE),
    file.exists(README_FILE),
    file.exists(ZENODO_METADATA_FILE),
    all(release_manifest$exists),
    TRUE,
    TRUE,
    TRUE
  ),
  stringsAsFactors = FALSE
)

write.csv(
  metadata_gate,
  METADATA_GATE_FILE,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)

metadata_pass <- all(
  metadata_gate$passed
)


# ------------------------------------------------------------------------------
# 26. CONSOLE SUMMARY
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" RELEASE STATISTICS\n")
cat("============================================================\n\n")

print(
  release_statistics,
  row.names = FALSE
)

cat("\n")
cat("============================================================\n")
cat(" RELEASE MANIFEST\n")
cat("============================================================\n\n")

print(
  release_manifest[
    ,
    c(
      "file_role",
      "file_name",
      "exists",
      "size_bytes"
    ),
    drop = FALSE
  ],
  row.names = FALSE
)

cat("\n")
cat("============================================================\n")
cat(" METADATA GATE\n")
cat("============================================================\n\n")

print(
  metadata_gate,
  row.names = FALSE
)


# ------------------------------------------------------------------------------
# 27. INTERPRETATION
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" INTERPRETATION\n")
cat("============================================================\n\n")

if (metadata_pass) {
  
  cat(
    "PASS. The VPJD Geography v1.0.0 metadata layer was built successfully.\n"
  )
  
  cat(
    "Positive taxon x geographic-unit relationships:",
    format(
      distribution_relationship_count,
      big.mark = ","
    ),
    "\n"
  )
  
  cat(
    "Taxa with positive geographic evidence:",
    format(
      taxa_with_evidence_count,
      big.mark = ","
    ),
    "\n"
  )
  
  cat(
    "Unsupported taxon x area combinations:",
    format(
      unsupported_combination_count,
      big.mark = ","
    ),
    "\n"
  )
  
  cat(
    "Unsupported combinations remain explicitly distinct from biological absence.\n"
  )
  
} else {
  
  cat(
    "FAIL. One or more metadata validation criteria did not pass.\n"
  )
}


# ------------------------------------------------------------------------------
# 28. OUTPUTS
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" OUTPUTS\n")
cat("============================================================\n\n")

metadata_outputs <- c(
  RELEASE_METADATA_FILE,
  DATA_DICTIONARY_FILE,
  RELEASE_STATISTICS_FILE,
  RELEASE_MANIFEST_FILE,
  PROVENANCE_FILE,
  SAFEGUARDS_FILE,
  README_FILE,
  ZENODO_METADATA_FILE,
  METADATA_GATE_FILE
)

for (output_path in metadata_outputs) {
  
  cat(
    output_path,
    "\n"
  )
}


# ------------------------------------------------------------------------------
# 29. UNRESOLVED RELEASE METADATA
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" UNRESOLVED RELEASE METADATA\n")
cat("============================================================\n\n")

cat("1. Final Zenodo DOI for VPJD Geography v1.0.0.\n")
cat("2. DOI of VPJD Taxonomic Release v1.0.0.\n")
cat("3. Final licence for VPJD Geography v1.0.0.\n")
cat("4. Final creator/contributor list and affiliations.\n")
cat("5. ORCID identifiers where applicable.\n")
cat("6. Final Zenodo relationship to the taxonomic release.\n")


# ------------------------------------------------------------------------------
# 30. SAFEGUARDS
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" SAFEGUARDS\n")
cat("============================================================\n\n")

cat("Canonical distribution was not modified.\n")
cat("Canonical evidence was not modified.\n")
cat("Frozen geographic vocabulary was not modified.\n")
cat("VPJD Taxonomic Release v1.0.0 was not modified.\n")
cat("No taxon identifiers were generated.\n")
cat("No geographic identifiers were generated.\n")
cat("No fuzzy matching was performed.\n")
cat("No biological absence was inferred.\n")
cat("No endemicity was inferred.\n")
cat("No rarity was inferred.\n")
cat("No Nakamura logic was applied.\n")
cat("No Star categories were assigned.\n")


# ------------------------------------------------------------------------------
# 31. NEXT STEP
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" NEXT STEP\n")
cat("============================================================\n\n")

if (metadata_pass) {
  
  cat("06d completed successfully.\n\n")
  
  cat(
    paste(
      "The core geographic products are validated and the formal",
      "release metadata layer has now been constructed."
    ),
    "\n\n"
  )
  
  cat("Recommended next script:\n\n")
  
  cat(
    "geography_06e_build_release_summaries.R\n\n"
  )
  
  cat(
    paste(
      "06e should construct publication-ready descriptive summaries",
      "of the frozen geographic release without changing the data model."
    ),
    "\n"
  )
  
} else {
  
  cat(
    paste(
      "06d metadata validation failed.",
      "Do not proceed until all failed criteria have been resolved."
    ),
    "\n"
  )
}

cat("\n")
cat("============================================================\n")