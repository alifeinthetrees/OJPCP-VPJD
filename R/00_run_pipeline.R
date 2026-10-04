# ==============================================================================
# 00_run_pipeline.R
# VPJD / Oxford-Japan Plant Conservation Partnership
#
# Master pipeline runner
#
# Project:
#   Vascular Plants of Japan Database (VPJD)
#   Oxford-Japan Plant Conservation Partnership (OJPCP)
#
# Architecture:
#   GBIF occurrence acquisition
#       -> occurrence cleaning
#       -> WCVP taxonomic reconciliation
#       -> geographic standardisation
#       -> global distribution assessment
#       -> Star ratings
#       -> GHI / richness / floristic analyses
#       -> audit
#
# IMPORTANT:
#   The VPJD rebuild is occurrence-first.
#   Existing GEOJAPAN/FOJ taxonomic modules are reference/reconciliation
#   infrastructure and do not define the taxa acquired from GBIF.
# ==============================================================================

suppressPackageStartupMessages({
  library(here)
})

message("==============================================================")
message("VPJD-OJPCP")
message("Vascular Plants of Japan Database")
message("Project root: ", here::here())
message("==============================================================")

# ------------------------------------------------------------------------------
# Validate project
# ------------------------------------------------------------------------------

project_file <- here::here("VPJD-OJPCP.Rproj")

if (!file.exists(project_file)) {
  stop(
    "VPJD-OJPCP.Rproj was not found at the project root.\n",
    "Open the VPJD-OJPCP RStudio project before running the pipeline."
  )
}

# ------------------------------------------------------------------------------
# Core directories
# ------------------------------------------------------------------------------

required_directories <- c(
  here::here("R"),
  here::here("R", "acquisition"),
  here::here("R", "cleaning"),
  here::here("R", "taxonomy"),
  here::here("R", "geography"),
  here::here("R", "audit"),
  here::here("R", "functions"),
  here::here("data", "raw"),
  here::here("data", "raw", "GBIF"),
  here::here("data", "raw", "WCVP"),
  here::here("data", "raw", "external"),
  here::here("data", "interim"),
  here::here("data", "processed"),
  here::here("outputs"),
  here::here("outputs", "tables"),
  here::here("outputs", "maps"),
  here::here("outputs", "figures"),
  here::here("docs"),
  here::here("archive")
)

invisible(
  lapply(
    required_directories,
    dir.create,
    recursive = TRUE,
    showWarnings = FALSE
  )
)

# ------------------------------------------------------------------------------
# Helper: safely source a module
# ------------------------------------------------------------------------------

source_module <- function(...) {
  
  path <- here::here(...)
  
  if (!file.exists(path)) {
    stop(
      "Pipeline module not found: ",
      path
    )
  }
  
  message("")
  message("--------------------------------------------------------------")
  message("Loading: ", path)
  message("--------------------------------------------------------------")
  
  source(
    path,
    local = FALSE,
    echo = FALSE
  )
  
  invisible(TRUE)
}

# ------------------------------------------------------------------------------
# Core utility functions
# ------------------------------------------------------------------------------

utility_file <- here::here(
  "R",
  "functions",
  "utility_functions.R"
)

if (file.exists(utility_file)) {
  source(utility_file)
} else {
  message(
    "Utility module not yet present; continuing without it."
  )
}

# ------------------------------------------------------------------------------
# Pipeline modules
#
# Only modules belonging to the active occurrence-first pipeline should be
# activated here.
#
# Frozen GEOJAPAN/FOJ reconciliation modules remain available independently
# under R/taxonomy/ but are not automatically run during GBIF acquisition.
# ------------------------------------------------------------------------------

run_vpjd_pipeline <- function(
    acquire = TRUE,
    clean = FALSE,
    taxonomy = FALSE,
    geography = FALSE,
    audit = FALSE) {
  
  message("")
  message("==============================================================")
  message("STARTING VPJD PIPELINE")
  message("==============================================================")
  
  if (acquire) {
    source_module(
      "R",
      "acquisition",
      "01_gbif_acquire.R"
    )
  } else {
    message("Skipping GBIF acquisition.")
  }
  
  if (clean) {
    source_module(
      "R",
      "cleaning",
      "03_clean_occurrences.R"
    )
  } else {
    message("Skipping occurrence cleaning.")
  }
  
if (taxonomy) {
  source_module(
    "R",
    "taxonomy",
    "00_run_taxonomy_pipeline.R"
  )
} else {
  message("Skipping occurrence-derived WCVP taxonomic reconciliation.")
}
  
  if (geography) {
    source_module(
      "R",
      "geography",
      "06_build_japan_gazetteer.R"
    )
  } else {
    message("Skipping geographic processing.")
  }

  if (audit) {
    source_module(
      "R",
      "audit",
      "08_audit_dataset.R"
    )
  } else {
    message("Skipping final dataset audit.")
  }
  
  message("")
  message("==============================================================")
  message("VPJD PIPELINE COMPLETE")
  message("==============================================================")
  
  invisible(TRUE)
}

# ------------------------------------------------------------------------------
# Default behaviour
#
# At the present development stage, acquisition and the occurrence-derived
# WCVP taxonomy pipeline are validated components.
#
# Later stages will be enabled as they are individually validated.
# ------------------------------------------------------------------------------

message("")
message("00_run_pipeline.R loaded successfully.")
message("")
message("Current development command:")
message("  run_vpjd_pipeline()")
message("")
message("Validated components: acquisition and occurrence-derived WCVP taxonomy.")
