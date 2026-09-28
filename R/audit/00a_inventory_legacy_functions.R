# ==============================================================================
# 00a_inventory_legacy_functions.R
# VPJD / Oxford-Japan Plant Conservation Partnership
#
# Purpose:
#   Audit reusable methodological code in the legacy JBRCI/VPJD GBIF system.
#
# Scope:
#   1. All R scripts under:
#      GBIF Scripts/Functions/
#
#   2. Non-institutional/support scripts identified by:
#      legacy_gbif_script_inventory.csv
#
# Outputs:
#   - legacy_method_script_inventory.csv
#   - legacy_function_inventory.csv
#   - legacy_function_duplicates.csv
#   - legacy_method_review.csv
#
# IMPORTANT:
# - Legacy scripts are READ AS TEXT ONLY.
# - Nothing is sourced or executed.
# - No legacy files are modified.
# - No GBIF requests are made.
#
# Version: 0.1.0
# ==============================================================================

VPJD_LEGACY_FUNCTION_AUDIT_VERSION <- "0.1.0"

# Packages ---------------------------------------------------------------------

required_packages <- c(
  "here", "readr", "dplyr", "stringr",
  "purrr", "tibble", "tidyr"
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

legacy_root <- paste0(
  "C:/Users/alife/OXFORD/OneDrive - Nexus365/",
  "JBRCI-VPJD/JBCRI-VPJD/scripts/GBIF Scripts"
)

functions_dir <- file.path(
  legacy_root,
  "Functions"
)

legacy_registry_path <- here::here(
  "data", "raw", "external", "legacy_gbif",
  "legacy_gbif_script_inventory.csv"
)

output_dir <- here::here(
  "data", "raw", "external", "legacy_gbif",
  "functions"
)

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

script_inventory_path <- file.path(
  output_dir,
  "legacy_method_script_inventory.csv"
)

function_inventory_path <- file.path(
  output_dir,
  "legacy_function_inventory.csv"
)

duplicate_path <- file.path(
  output_dir,
  "legacy_function_duplicates.csv"
)

review_path <- file.path(
  output_dir,
  "legacy_method_review.csv"
)

# Validate inputs --------------------------------------------------------------

if (!dir.exists(legacy_root)) {
  stop(
    "Legacy GBIF directory not found:\n",
    legacy_root
  )
}

if (!dir.exists(functions_dir)) {
  stop(
    "Legacy Functions directory not found:\n",
    functions_dir
  )
}

if (!file.exists(legacy_registry_path)) {
  stop(
    "Legacy GBIF script inventory not found:\n",
    legacy_registry_path,
    "\nRun 00_build_legacy_gbif_registry.R first."
  )
}

message("")
message("==============================================================")
message("VPJD LEGACY METHODS / FUNCTIONS AUDIT")
message("==============================================================")
message(
  "Version: ",
  VPJD_LEGACY_FUNCTION_AUDIT_VERSION
)
message("")

# Helpers ----------------------------------------------------------------------

has_pattern <- function(text, pattern) {
  stringr::str_detect(
    text,
    regex(
      pattern,
      ignore_case = TRUE
    )
  )
}

collapse_unique <- function(x) {
  x <- unique(x[
    !is.na(x) &
      x != ""
  ])
  
  if (length(x) == 0L) {
    return(NA_character_)
  }
  
  paste(
    sort(x),
    collapse = " | "
  )
}

extract_namespaces <- function(text) {
  hits <- stringr::str_match_all(
    text,
    "\\b([A-Za-z][A-Za-z0-9.]*)::[A-Za-z][A-Za-z0-9._]*"
  )[[1]]
  
  if (nrow(hits) == 0L) {
    return(NA_character_)
  }
  
  collapse_unique(
    hits[, 2]
  )
}

extract_library_packages <- function(text) {
  hits <- stringr::str_match_all(
    text,
    paste0(
      "(?:library|require)\\s*\\(\\s*",
      "[\"']?([A-Za-z][A-Za-z0-9.]*)[\"']?\\s*\\)"
    )
  )[[1]]
  
  if (nrow(hits) == 0L) {
    return(NA_character_)
  }
  
  collapse_unique(
    hits[, 2]
  )
}

extract_function_definitions <- function(lines) {
  results <- vector(
    "list",
    length(lines)
  )
  
  for (i in seq_along(lines)) {
    line <- lines[[i]]
    
    hit <- stringr::str_match(
      line,
      paste0(
        "^\\s*",
        "([A-Za-z.][A-Za-z0-9._]*)",
        "\\s*(?:<-|=)\\s*function\\s*\\((.*)$"
      )
    )
    
    if (
      nrow(hit) == 0L ||
      is.na(hit[1, 2])
    ) {
      next
    }
    
    function_name <- hit[1, 2]
    remainder <- hit[1, 3]
    
    argument_text <- remainder
    
    if (grepl("\\)", argument_text)) {
      argument_text <- sub(
        "\\).*",
        "",
        argument_text
      )
    } else {
      j <- i + 1L
      
      while (
        j <= length(lines) &&
        !grepl("\\)", argument_text)
      ) {
        argument_text <- paste(
          argument_text,
          lines[[j]],
          sep = " "
        )
        j <- j + 1L
      }
      
      argument_text <- sub(
        "\\).*",
        "",
        argument_text
      )
    }
    
    argument_text <- stringr::str_squish(
      argument_text
    )
    
    results[[i]] <- tibble::tibble(
      function_name = function_name,
      function_arguments = argument_text,
      definition_line = i
    )
  }
  
  dplyr::bind_rows(
    results
  )
}

classify_method <- function(text, filename) {
  name <- tolower(
    basename(filename)
  )
  
  scores <- c(
    acquisition = sum(c(
      has_pattern(text, "occ_download"),
      has_pattern(text, "occ_search"),
      has_pattern(text, "occ_count"),
      has_pattern(text, "datasetKey")
    )),
    taxonomy = sum(c(
      has_pattern(text, "WCVP"),
      has_pattern(text, "WFO"),
      has_pattern(text, "taxon"),
      has_pattern(text, "scientificName"),
      has_pattern(text, "synonym")
    )),
    cleaning = sum(c(
      has_pattern(text, "quarantine"),
      has_pattern(text, "duplicate"),
      has_pattern(text, "coordinate"),
      has_pattern(text, "clean"),
      has_pattern(text, "basisOfRecord")
    )),
    stars = sum(c(
      has_pattern(text, "FOJ"),
      has_pattern(text, "FOJ_STARS"),
      has_pattern(text, "\\bStar\\b")
    )),
    geography = sum(c(
      has_pattern(text, "prefecture"),
      has_pattern(text, "gazetteer"),
      has_pattern(text, "locality"),
      has_pattern(text, "countryCode")
    )),
    spatial = sum(c(
      has_pattern(text, "\\bsf\\b"),
      has_pattern(text, "st_"),
      has_pattern(text, "geometry"),
      has_pattern(text, "decimalLatitude"),
      has_pattern(text, "decimalLongitude")
    )),
    bioquality = sum(c(
      has_pattern(text, "\\bGHI\\b"),
      has_pattern(text, "Genetic Heat Index"),
      has_pattern(text, "bioquality")
    )),
    analysis = sum(c(
      has_pattern(text, "ordination"),
      has_pattern(text, "phylogen"),
      has_pattern(text, "richness"),
      has_pattern(text, "vegan"),
      has_pattern(text, "ggplot")
    ))
  )
  
  max_score <- max(scores)
  
  if (max_score == 0L) {
    return("general")
  }
  
  winners <- names(
    scores[scores == max_score]
  )
  
  if (length(winners) > 1L) {
    if (grepl("wcvp|wfo|taxon", name)) {
      return("taxonomy")
    }
    
    if (grepl("star|foj", name)) {
      return("stars")
    }
    
    if (grepl("clean|filter|quarantine", name)) {
      return("cleaning")
    }
    
    if (grepl("phylogen|ordination|richness", name)) {
      return("analysis")
    }
  }
  
  winners[[1]]
}

assign_review_status <- function(
    category,
    text,
    source_type,
    function_count
) {
  # Existing rebuilt VPJD modules already cover these broad areas.
  # This is a triage status, not a deletion decision.
  
  if (
    category == "acquisition" &&
    source_type == "top_level_support"
  ) {
    return("REPLACED")
  }
  
  if (
    category == "taxonomy" &&
    (
      has_pattern(text, "WFO") ||
      has_pattern(text, "WCVP")
    )
  ) {
    return("REVIEW_LATER")
  }
  
  if (
    category %in% c(
      "stars",
      "geography",
      "spatial",
      "bioquality",
      "analysis"
    )
  ) {
    return("REVIEW_LATER")
  }
  
  if (
    function_count > 0L &&
    source_type == "functions_directory"
  ) {
    return("REUSE")
  }
  
  if (
    category == "cleaning"
  ) {
    return("REVIEW_LATER")
  }
  
  "UNKNOWN"
}

# Read existing GBIF script inventory ------------------------------------------

legacy_registry <- readr::read_csv(
  legacy_registry_path,
  show_col_types = FALSE
)

required_registry_columns <- c(
  "legacy_filename",
  "legacy_path",
  "script_class"
)

missing_registry_columns <- setdiff(
  required_registry_columns,
  names(legacy_registry)
)

if (length(missing_registry_columns) > 0L) {
  stop(
    "Legacy registry is missing required column(s): ",
    paste(
      missing_registry_columns,
      collapse = ", "
    )
  )
}

# Functions directory scripts -------------------------------------------------

function_scripts <- list.files(
  functions_dir,
  pattern = "\\.[Rr]$",
  recursive = TRUE,
  full.names = TRUE
)

message(
  "R scripts in Functions directory: ",
  length(function_scripts)
)

# Non-institutional top-level scripts ------------------------------------------

support_scripts <- legacy_registry |>
  filter(
    script_class != "institution_candidate"
  ) |>
  pull(
    legacy_path
  )

support_scripts <- unique(
  support_scripts[
    file.exists(support_scripts)
  ]
)

message(
  "Non-institutional/support scripts: ",
  length(support_scripts)
)

# Combine audit targets --------------------------------------------------------

targets <- bind_rows(
  tibble::tibble(
    script_path = function_scripts,
    source_type = "functions_directory"
  ),
  tibble::tibble(
    script_path = support_scripts,
    source_type = "top_level_support"
  )
) |>
  distinct(
    script_path,
    .keep_all = TRUE
  )

if (nrow(targets) == 0L) {
  stop(
    "No legacy method/function scripts found."
  )
}

message(
  "Unique scripts to audit: ",
  nrow(targets)
)
message("")

# Parse scripts ----------------------------------------------------------------

parse_method_script <- function(path, source_type) {
  lines <- readLines(
    path,
    warn = FALSE,
    encoding = "UTF-8"
  )
  
  text <- paste(
    lines,
    collapse = "\n"
  )
  
  functions <- extract_function_definitions(
    lines
  )
  
  function_count <- nrow(
    functions
  )
  
  category <- classify_method(
    text,
    path
  )
  
  packages <- collapse_unique(
    c(
      extract_namespaces(text),
      extract_library_packages(text)
    )
  )
  
  review_status <- assign_review_status(
    category = category,
    text = text,
    source_type = source_type,
    function_count = function_count
  )
  
  script_row <- tibble::tibble(
    source_type = source_type,
    legacy_filename = basename(path),
    legacy_path = normalizePath(
      path,
      winslash = "/",
      mustWork = FALSE
    ),
    script_lines = length(lines),
    function_count = function_count,
    function_names = if (
      function_count == 0L
    ) {
      NA_character_
    } else {
      collapse_unique(
        functions$function_name
      )
    },
    packages = packages,
    category = category,
    review_status = review_status,
    gbif_logic = has_pattern(
      text,
      "rgbif|occ_download|occ_search|occ_count|GBIF"
    ),
    wcvp_logic = has_pattern(
      text,
      "WCVP|rWCVP"
    ),
    wfo_logic = has_pattern(
      text,
      "\\bWFO\\b|World Flora Online"
    ),
    foj_logic = has_pattern(
      text,
      "\\bFOJ\\b|FOJ_STARS"
    ),
    star_logic = has_pattern(
      text,
      "\\bStar\\b|Star rating"
    ),
    ghi_logic = has_pattern(
      text,
      "\\bGHI\\b|Genetic Heat Index"
    ),
    coordinate_logic = has_pattern(
      text,
      "decimalLatitude|decimalLongitude|coordinate"
    ),
    spatial_logic = has_pattern(
      text,
      "\\bsf\\b|st_|geometry"
    ),
    duckdb_logic = has_pattern(
      text,
      "duckdb|DBI"
    ),
    plotting_logic = has_pattern(
      text,
      "ggplot|geom_|plot\\s*\\("
    ),
    file_io_logic = has_pattern(
      text,
      paste0(
        "read_csv|write_csv|read\\.csv|write\\.csv|",
        "readRDS|saveRDS|fread|fwrite"
      )
    )
  )
  
  if (function_count > 0L) {
    functions <- functions |>
      mutate(
        source_type = source_type,
        legacy_filename = basename(path),
        legacy_path = normalizePath(
          path,
          winslash = "/",
          mustWork = FALSE
        ),
        category = category,
        script_review_status = review_status,
        packages = packages
      ) |>
      select(
        source_type,
        legacy_filename,
        legacy_path,
        function_name,
        function_arguments,
        definition_line,
        category,
        script_review_status,
        packages
      )
  }
  
  list(
    script = script_row,
    functions = functions
  )
}

message("Reading legacy methods/functions as text...")

parsed <- purrr::map2(
  targets$script_path,
  targets$source_type,
  parse_method_script
)

script_inventory <- purrr::map_dfr(
  parsed,
  "script"
)

function_inventory <- purrr::map_dfr(
  parsed,
  "functions"
)

# Add stable audit IDs ---------------------------------------------------------

script_inventory <- script_inventory |>
  arrange(
    source_type,
    category,
    legacy_filename
  ) |>
  mutate(
    script_audit_id = sprintf(
      "LEGACY_METHOD_%03d",
      row_number()
    )
  ) |>
  select(
    script_audit_id,
    everything()
  )

if (nrow(function_inventory) > 0L) {
  function_inventory <- function_inventory |>
    arrange(
      function_name,
      legacy_filename
    ) |>
    mutate(
      function_audit_id = sprintf(
        "LEGACY_FUNC_%03d",
        row_number()
      )
    ) |>
    select(
      function_audit_id,
      everything()
    )
}

# Duplicate function names -----------------------------------------------------

if (nrow(function_inventory) > 0L) {
  duplicate_functions <- function_inventory |>
    count(
      function_name,
      name = "definition_count"
    ) |>
    filter(
      definition_count > 1L
    ) |>
    left_join(
      function_inventory |>
        group_by(function_name) |>
        summarise(
          source_files = paste(
            sort(unique(legacy_filename)),
            collapse = " | "
          ),
          categories = paste(
            sort(unique(category)),
            collapse = " | "
          ),
          .groups = "drop"
        ),
      by = "function_name"
    ) |>
    arrange(
      desc(definition_count),
      function_name
    )
} else {
  duplicate_functions <- tibble::tibble(
    function_name = character(),
    definition_count = integer(),
    source_files = character(),
    categories = character()
  )
}

# Review table -----------------------------------------------------------------

review_table <- script_inventory |>
  select(
    script_audit_id,
    source_type,
    legacy_filename,
    legacy_path,
    category,
    review_status,
    function_count,
    function_names,
    packages,
    gbif_logic,
    wcvp_logic,
    wfo_logic,
    foj_logic,
    star_logic,
    ghi_logic,
    coordinate_logic,
    spatial_logic,
    duckdb_logic,
    plotting_logic,
    file_io_logic,
    script_lines
  ) |>
  arrange(
    factor(
      review_status,
      levels = c(
        "REUSE",
        "REVIEW_LATER",
        "UNKNOWN",
        "REPLACED",
        "LEGACY_ONLY"
      )
    ),
    category,
    legacy_filename
  )

# Export -----------------------------------------------------------------------

readr::write_csv(
  script_inventory,
  script_inventory_path
)

readr::write_csv(
  function_inventory,
  function_inventory_path
)

readr::write_csv(
  duplicate_functions,
  duplicate_path
)

readr::write_csv(
  review_table,
  review_path
)

# Summary ----------------------------------------------------------------------

n_scripts <- nrow(
  script_inventory
)

n_function_dir_scripts <- sum(
  script_inventory$source_type ==
    "functions_directory"
)

n_support_scripts <- sum(
  script_inventory$source_type ==
    "top_level_support"
)

n_functions <- nrow(
  function_inventory
)

n_unique_functions <- if (
  n_functions == 0L
) {
  0L
} else {
  dplyr::n_distinct(
    function_inventory$function_name
  )
}

n_duplicate_names <- nrow(
  duplicate_functions
)

status_summary <- script_inventory |>
  count(
    review_status,
    name = "scripts"
  ) |>
  arrange(
    review_status
  )

category_summary <- script_inventory |>
  count(
    category,
    name = "scripts"
  ) |>
  arrange(
    desc(scripts),
    category
  )

# Final report -----------------------------------------------------------------

message("")
message("==============================================================")
message("LEGACY METHODS / FUNCTIONS AUDIT COMPLETE")
message("==============================================================")

message(
  "Scripts audited: ",
  format(n_scripts, big.mark = ",")
)

message(
  "Functions-directory scripts: ",
  format(n_function_dir_scripts, big.mark = ",")
)

message(
  "Top-level support scripts: ",
  format(n_support_scripts, big.mark = ",")
)

message("")
message(
  "Function definitions found: ",
  format(n_functions, big.mark = ",")
)

message(
  "Unique function names: ",
  format(n_unique_functions, big.mark = ",")
)

message(
  "Duplicated function names: ",
  format(n_duplicate_names, big.mark = ",")
)

message("")
message("Review-status summary:")

for (i in seq_len(nrow(status_summary))) {
  message(
    "  ",
    status_summary$review_status[[i]],
    ": ",
    format(
      status_summary$scripts[[i]],
      big.mark = ","
    )
  )
}

message("")
message("Method-category summary:")

for (i in seq_len(nrow(category_summary))) {
  message(
    "  ",
    category_summary$category[[i]],
    ": ",
    format(
      category_summary$scripts[[i]],
      big.mark = ","
    )
  )
}

message("")
message("Outputs:")
message(
  "  Script inventory: ",
  script_inventory_path
)
message(
  "  Function inventory: ",
  function_inventory_path
)
message(
  "  Duplicate functions: ",
  duplicate_path
)
message(
  "  Review table: ",
  review_path
)

message("")
message(
  "No legacy scripts were executed or modified."
)

message(
  "No GBIF requests were made."
)

message("")
message(
  "00a_inventory_legacy_functions.R v",
  VPJD_LEGACY_FUNCTION_AUDIT_VERSION,
  " complete."
)