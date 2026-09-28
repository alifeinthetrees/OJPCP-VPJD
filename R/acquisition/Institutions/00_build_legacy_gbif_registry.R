# ==============================================================================
# 00_build_legacy_gbif_registry.R
# VPJD / Oxford-Japan Plant Conservation Partnership
#
# Purpose:
#   Inventory legacy GBIF R scripts and extract institutional GBIF dataset
#   definitions into a consolidated migration registry.
#
# Outputs:
#   1. legacy_gbif_script_inventory.csv
#      Complete inventory of every legacy R script.
#
#   2. legacy_gbif_dataset_candidates.csv
#      Every datasetKey extracted from legacy scripts, with provenance.
#
#   3. gbif_institution_registry.csv
#      Production registry: one row per unique institutional GBIF datasetKey.
#
#   4. legacy_gbif_registry_review.csv
#      Legacy support/template/multi-dataset material retained for review.
#
# IMPORTANT:
# - Legacy scripts are READ AS TEXT ONLY.
# - Legacy scripts are NOT sourced or executed.
# - No GBIF requests are made.
# - No legacy files are modified.
# - Generic/template/pipeline scripts remain in the inventory but are not
#   promoted to the production institutional registry.
#
# Version: 0.1.2
# ==============================================================================

VPJD_LEGACY_REGISTRY_VERSION <- "0.1.2"

# Packages ---------------------------------------------------------------------

required_packages <- c(
  "here", "readr", "dplyr", "stringr", "purrr", "tibble", "tidyr"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0L) {
  stop(
    "Required package(s) not installed: ",
    paste(missing_packages, collapse = ", ")
  )
}

suppressPackageStartupMessages({
  library(here)
  library(readr)
  library(dplyr)
  library(stringr)
  library(purrr)
  library(tibble)
  library(tidyr)
})

# Paths ------------------------------------------------------------------------

legacy_dir <- paste0(
  "C:/Users/alife/OXFORD/OneDrive - Nexus365/",
  "JBRCI-VPJD/JBCRI-VPJD/scripts/GBIF Scripts"
)

output_dir <- here::here(
  "data", "raw", "external", "legacy_gbif"
)

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

inventory_path <- file.path(
  output_dir,
  "legacy_gbif_script_inventory.csv"
)

candidate_path <- file.path(
  output_dir,
  "legacy_gbif_dataset_candidates.csv"
)

institution_registry_path <- file.path(
  output_dir,
  "gbif_institution_registry.csv"
)

review_path <- file.path(
  output_dir,
  "legacy_gbif_registry_review.csv"
)

# Validate legacy directory ----------------------------------------------------

if (!dir.exists(legacy_dir)) {
  stop(
    "Legacy GBIF script directory not found:\n",
    legacy_dir
  )
}

# Find scripts -----------------------------------------------------------------

legacy_scripts <- list.files(
  legacy_dir,
  pattern = "\\.[Rr]$",
  full.names = TRUE,
  recursive = TRUE
)

if (length(legacy_scripts) == 0L) {
  stop(
    "No R scripts found in:\n",
    legacy_dir
  )
}

message("")
message("==============================================================")
message("VPJD LEGACY GBIF REGISTRY BUILDER")
message("==============================================================")
message("Version: ", VPJD_LEGACY_REGISTRY_VERSION)
message("Legacy directory: ", legacy_dir)
message("R scripts found: ", length(legacy_scripts))
message("")

# Helpers ----------------------------------------------------------------------

uuid_pattern <- paste0(
  "[0-9a-fA-F]{8}-",
  "[0-9a-fA-F]{4}-",
  "[0-9a-fA-F]{4}-",
  "[0-9a-fA-F]{4}-",
  "[0-9a-fA-F]{12}"
)

extract_uuids <- function(text) {
  hits <- stringr::str_extract_all(
    text,
    regex(uuid_pattern)
  )[[1]]
  
  unique(tolower(hits))
}

extract_assignment <- function(text, variable) {
  pattern <- paste0(
    "(?m)^\\s*",
    variable,
    "\\s*<-\\s*[\"']([^\"']+)[\"']"
  )
  
  hit <- stringr::str_match(
    text,
    regex(pattern)
  )
  
  if (
    nrow(hit) == 0L ||
    is.na(hit[1, 2])
  ) {
    return(NA_character_)
  }
  
  stringr::str_squish(
    hit[1, 2]
  )
}

extract_dataset_title <- function(text, filename) {
  candidates <- c(
    extract_assignment(text, "dataset_title"),
    extract_assignment(text, "dataset_name"),
    extract_assignment(text, "target_title"),
    extract_assignment(text, "title")
  )
  
  candidates <- candidates[
    !is.na(candidates) &
      candidates != ""
  ]
  
  if (length(candidates) > 0L) {
    return(candidates[[1]])
  }
  
  lines <- strsplit(
    text,
    "\n",
    fixed = TRUE
  )[[1]]
  
  comments <- lines[
    grepl("^\\s*#", lines)
  ]
  
  comments <- gsub(
    "^\\s*#+\\s*",
    "",
    comments
  )
  
  comments <- stringr::str_squish(
    comments
  )
  
  comments <- comments[
    comments != "" &
      !grepl("^[=-]+$", comments)
  ]
  
  likely <- comments[
    grepl(
      paste0(
        "Herbarium|University|Museum|Institute|",
        "Collection|Botanic|Garden|Research|",
        "Observatory|Biodiversity|Specimens"
      ),
      comments,
      ignore.case = TRUE
    )
  ]
  
  if (length(likely) > 0L) {
    return(likely[[1]])
  }
  
  tools::file_path_sans_ext(
    basename(filename)
  )
}

has_pattern <- function(text, pattern) {
  stringr::str_detect(
    text,
    regex(
      pattern,
      ignore_case = TRUE
    )
  )
}

# Script classification --------------------------------------------------------

classify_script <- function(filename, text, uuid_count) {
  name <- tolower(
    tools::file_path_sans_ext(
      basename(filename)
    )
  )
  
  # Explicit generic/test scripts that may contain genuine dataset UUIDs but
  # must not themselves become authoritative institutional sources.
  explicit_generic_names <- c(
    "gbif-download dataset-arboricare",
    "gbif-download dataset",
    "gbif-script generator"
  )
  
  if (name %in% explicit_generic_names) {
    return("support_or_template")
  }
  
  # Multiple UUIDs are classified explicitly before broader generic rules.
  if (uuid_count > 1L) {
    return("multi_dataset_script")
  }
  
  generic_name_pattern <- paste0(
    "aggregator|generator|compile|housekeeping|",
    "run all|pipeline|ordination|phylogen|",
    "standardisation|standardization|",
    "parsing|parse|audit|summary|",
    "extract unique|data compilation|",
    "process datasets|patch all|",
    "star joiner|filter jp|",
    "^00_download_gbif$|",
    "^01_acquire_gbif_and_add_foj$|",
    "^01_clean_gbif$|",
    "^gbif pipeline$"
  )
  
  generic_filename <- grepl(
    generic_name_pattern,
    name,
    ignore.case = TRUE
  )
  
  template_content <- has_pattern(
    text,
    "\\{title_safe\\}|\\{dataset|script generator"
  )
  
  if (generic_filename || template_content) {
    return("support_or_template")
  }
  
  if (uuid_count == 1L) {
    return("institution_candidate")
  }
  
  "support_or_analysis"
}

# Parse one script -------------------------------------------------------------

parse_legacy_script <- function(path) {
  lines <- readLines(
    path,
    warn = FALSE,
    encoding = "UTF-8"
  )
  
  text <- paste(
    lines,
    collapse = "\n"
  )
  
  uuids <- extract_uuids(text)
  uuid_count <- length(uuids)
  
  dataset_keys <- if (uuid_count == 0L) {
    NA_character_
  } else {
    paste(
      uuids,
      collapse = " | "
    )
  }
  
  script_class <- classify_script(
    path,
    text,
    uuid_count
  )
  
  tibble::tibble(
    legacy_filename = basename(path),
    legacy_path = normalizePath(
      path,
      winslash = "/",
      mustWork = FALSE
    ),
    dataset_title = extract_dataset_title(
      text,
      path
    ),
    dataset_keys = dataset_keys,
    dataset_key_count = uuid_count,
    script_class = script_class,
    country_jp = (
      has_pattern(
        text,
        "countryCode\\s*==\\s*[\"']JP[\"']"
      ) ||
        has_pattern(
          text,
          "country\\s*=\\s*[\"']JP[\"']"
        ) ||
        has_pattern(
          text,
          paste0(
            "pred\\s*\\(\\s*[\"']country[\"']",
            "\\s*,\\s*[\"']JP[\"']"
          )
        )
    ),
    uses_occ_download = has_pattern(
      text,
      "\\bocc_download\\s*\\("
    ),
    uses_occ_search = has_pattern(
      text,
      "\\bocc_search\\s*\\("
    ),
    uses_occ_count = has_pattern(
      text,
      "\\bocc_count\\s*\\("
    ),
    coordinate_logic = has_pattern(
      text,
      "hasCoordinate|decimalLatitude|decimalLongitude"
    ),
    geometry_logic = has_pattern(
      text,
      "\\bgeometry\\b"
    ),
    taxonomic_logic = has_pattern(
      text,
      paste0(
        "taxonKey|scientificName|kingdomKey|",
        "phylumKey|classKey"
      )
    ),
    basis_of_record_logic = has_pattern(
      text,
      "basisOfRecord"
    ),
    occurrence_status_logic = has_pattern(
      text,
      "occurrenceStatus"
    ),
    foj_logic = has_pattern(
      text,
      "FOJ"
    ),
    star_logic = has_pattern(
      text,
      "\\bStar\\b|Star rating|FOJ_STARS"
    ),
    ghi_logic = has_pattern(
      text,
      "\\bGHI\\b|Genetic Heat Index"
    ),
    plotting_logic = has_pattern(
      text,
      "ggplot|geom_|plot\\s*\\("
    ),
    hardcoded_path = has_pattern(
      text,
      "setwd\\s*\\(|[A-Za-z]:[/\\\\]"
    ),
    legacy_script_lines = length(lines)
  )
}

# Complete script inventory ----------------------------------------------------

message("Reading legacy scripts as text...")

inventory <- purrr::map_dfr(
  legacy_scripts,
  parse_legacy_script
)

inventory <- inventory |>
  arrange(
    script_class,
    dataset_title,
    legacy_filename
  ) |>
  mutate(
    inventory_id = sprintf(
      "LEGACY_%03d",
      row_number()
    )
  ) |>
  select(
    inventory_id,
    everything()
  )

# Extract datasetKey candidates ------------------------------------------------

message("Extracting datasetKey candidates...")

candidate_list <- vector(
  "list",
  nrow(inventory)
)

for (i in seq_len(nrow(inventory))) {
  keys <- inventory$dataset_keys[[i]]
  
  if (is.na(keys)) {
    next
  }
  
  key_vector <- strsplit(
    keys,
    " | ",
    fixed = TRUE
  )[[1]]
  
  candidate_list[[i]] <- tibble::tibble(
    inventory_id = inventory$inventory_id[[i]],
    legacy_filename = inventory$legacy_filename[[i]],
    legacy_path = inventory$legacy_path[[i]],
    dataset_title = inventory$dataset_title[[i]],
    dataset_key = key_vector,
    script_class = inventory$script_class[[i]],
    country_jp = inventory$country_jp[[i]],
    uses_occ_download = inventory$uses_occ_download[[i]],
    foj_logic = inventory$foj_logic[[i]],
    star_logic = inventory$star_logic[[i]]
  )
}

dataset_candidates <- dplyr::bind_rows(
  candidate_list
)

if (nrow(dataset_candidates) == 0L) {
  stop(
    "No GBIF dataset UUIDs were extracted."
  )
}

# Dataset-level provenance -----------------------------------------------------

dataset_provenance <- dataset_candidates |>
  group_by(dataset_key) |>
  summarise(
    scripts_containing_key = n_distinct(
      legacy_filename
    ),
    institution_candidate_scripts = sum(
      script_class == "institution_candidate"
    ),
    source_scripts = paste(
      sort(unique(legacy_filename)),
      collapse = " | "
    ),
    .groups = "drop"
  )

dataset_candidates <- dataset_candidates |>
  left_join(
    dataset_provenance,
    by = "dataset_key"
  ) |>
  arrange(
    dataset_key,
    legacy_filename
  )

# Institutional registry -------------------------------------------------------

message("Building institutional dataset registry...")

institution_sources <- dataset_candidates |>
  filter(
    script_class == "institution_candidate"
  )

institution_duplicates <- institution_sources |>
  count(
    dataset_key,
    name = "institution_script_count"
  )

institution_sources <- institution_sources |>
  left_join(
    institution_duplicates,
    by = "dataset_key"
  )

institution_registry <- institution_sources |>
  arrange(
    dataset_key,
    desc(country_jp),
    dataset_title,
    legacy_filename
  ) |>
  group_by(dataset_key) |>
  slice(1L) |>
  ungroup() |>
  mutate(
    registry_id = sprintf(
      "GBIF_INST_%03d",
      row_number()
    ),
    active = TRUE,
    country_filter = "JP",
    registry_status = case_when(
      institution_script_count > 1L ~
        "REVIEW_DUPLICATE_SOURCE",
      TRUE ~
        "READY"
    ),
    acquisition_model =
      "generic_institutional_acquisition"
  ) |>
  select(
    registry_id,
    dataset_title,
    dataset_key,
    active,
    country_filter,
    registry_status,
    acquisition_model,
    institution_script_count,
    scripts_containing_key,
    source_scripts,
    legacy_filename,
    legacy_path
  )

# Review queue -----------------------------------------------------------------

review_scripts <- inventory |>
  filter(
    script_class != "institution_candidate"
  ) |>
  transmute(
    review_type = "SCRIPT",
    identifier = inventory_id,
    dataset_key = NA_character_,
    legacy_filename,
    dataset_title,
    reason = script_class,
    legacy_path
  )

review_duplicate_datasets <- institution_registry |>
  filter(
    registry_status != "READY"
  ) |>
  transmute(
    review_type = "DATASET",
    identifier = registry_id,
    dataset_key,
    legacy_filename,
    dataset_title,
    reason = registry_status,
    legacy_path
  )

review_registry <- bind_rows(
  review_scripts,
  review_duplicate_datasets
) |>
  arrange(
    review_type,
    reason,
    legacy_filename
  )

# Export -----------------------------------------------------------------------

readr::write_csv(
  inventory,
  inventory_path
)

readr::write_csv(
  dataset_candidates,
  candidate_path
)

readr::write_csv(
  institution_registry,
  institution_registry_path
)

readr::write_csv(
  review_registry,
  review_path
)

# Validation -------------------------------------------------------------------

n_scripts <- nrow(inventory)

n_no_uuid <- sum(
  inventory$dataset_key_count == 0L
)

n_one_uuid <- sum(
  inventory$dataset_key_count == 1L
)

n_multi_uuid <- sum(
  inventory$dataset_key_count > 1L
)

n_uuid_occurrences <- nrow(
  dataset_candidates
)

n_unique_dataset_keys <- n_distinct(
  dataset_candidates$dataset_key
)

n_institution_scripts <- sum(
  inventory$script_class ==
    "institution_candidate"
)

n_institution_datasets <- nrow(
  institution_registry
)

n_ready <- sum(
  institution_registry$registry_status ==
    "READY"
)

n_duplicate_review <- sum(
  institution_registry$registry_status ==
    "REVIEW_DUPLICATE_SOURCE"
)

n_support_template <- sum(
  inventory$script_class ==
    "support_or_template"
)

n_support_analysis <- sum(
  inventory$script_class ==
    "support_or_analysis"
)

n_multi_dataset_scripts <- sum(
  inventory$script_class ==
    "multi_dataset_script"
)

if (
  n_institution_datasets !=
  n_distinct(institution_registry$dataset_key)
) {
  stop(
    "Institution registry does not contain ",
    "one row per unique datasetKey."
  )
}

if (
  any(
    is.na(institution_registry$dataset_key)
  )
) {
  stop(
    "Institution registry contains missing datasetKey."
  )
}

# Final report -----------------------------------------------------------------

message("")
message("==============================================================")
message("LEGACY GBIF REGISTRY COMPLETE")
message("==============================================================")

message(
  "Legacy R scripts: ",
  format(n_scripts, big.mark = ",")
)

message(
  "Scripts with no UUID: ",
  format(n_no_uuid, big.mark = ",")
)

message(
  "Scripts with one UUID: ",
  format(n_one_uuid, big.mark = ",")
)

message(
  "Scripts with multiple UUIDs: ",
  format(n_multi_uuid, big.mark = ",")
)

message("")
message(
  "Total datasetKey occurrences extracted: ",
  format(n_uuid_occurrences, big.mark = ",")
)

message(
  "Unique datasetKeys across legacy scripts: ",
  format(n_unique_dataset_keys, big.mark = ",")
)

message("")
message(
  "Institution-candidate scripts: ",
  format(n_institution_scripts, big.mark = ",")
)

message(
  "Unique institutional datasets: ",
  format(n_institution_datasets, big.mark = ",")
)

message(
  "Institutional datasets READY: ",
  format(n_ready, big.mark = ",")
)

message(
  "Institutional datasets requiring duplicate-source review: ",
  format(n_duplicate_review, big.mark = ",")
)

message("")
message(
  "Support/template scripts: ",
  format(n_support_template, big.mark = ",")
)

message(
  "Support/analysis scripts: ",
  format(n_support_analysis, big.mark = ",")
)

message(
  "Multi-dataset scripts: ",
  format(n_multi_dataset_scripts, big.mark = ",")
)

message("")
message("Outputs:")
message("  Script inventory: ", inventory_path)
message("  Dataset candidates: ", candidate_path)
message(
  "  Institutional registry: ",
  institution_registry_path
)
message("  Review queue: ", review_path)

message("")
message(
  "No legacy scripts were executed or modified."
)

message(
  "No GBIF requests were made."
)

message("")
message(
  "00_build_legacy_gbif_registry.R v",
  VPJD_LEGACY_REGISTRY_VERSION,
  " complete."
)