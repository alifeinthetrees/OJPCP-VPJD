# ==============================================================================
# VPJD GEOGRAPHY MODULE
# geography_01_audit_existing_sources.R
#
# Purpose:
#   Audit existing geographic and distributional data sources before any
#   harmonisation, taxonomic reconciliation, deduplication or Star assignment.
#
# Taxonomic baseline:
#   Vascular Plants of Japan Database (VPJD) v1.0.0
#   Published Zenodo release.
#
# IMPORTANT:
#   I:/R/Data/VPJD_v1.0.0 is treated as READ ONLY.
#
# This script does NOT:
#   - alter VPJD v1.0.0
#   - alter source datasets
#   - standardise taxon names
#   - deduplicate occurrence records
#   - reconcile names to VPJD
#   - calculate geographic ranges
#   - assign Star categories
#
# ==============================================================================


# ------------------------------------------------------------------------------
# 0. SET PATHS
# ------------------------------------------------------------------------------

# Published VPJD v1.0.0 release — READ ONLY
VPJD_RELEASE_ROOT <- "I:/R/Data/VPJD_v1.0.0"

VPJD_RELEASE_DATA <- file.path(
  VPJD_RELEASE_ROOT,
  "data"
)

VPJD_RELEASE_METADATA <- file.path(
  VPJD_RELEASE_ROOT,
  "metadata"
)

VPJD_RELEASE_PROVENANCE <- file.path(
  VPJD_RELEASE_ROOT,
  "provenance"
)

VPJD_RELEASE_AUDIT <- file.path(
  VPJD_RELEASE_ROOT,
  "audit"
)


# Active VPJD/OJPCP repository
VPJD_PROJECT_ROOT <- "I:/R/OJPCP/VPJD-OJPCP"

GEOGRAPHY_ROOT <- file.path(
  VPJD_PROJECT_ROOT,
  "data",
  "interim",
  "geography"
)

NLNI_ROOT <- file.path(
  GEOGRAPHY_ROOT,
  "nlni_n03_2025"
)

GBIF_ROOT <- file.path(
  VPJD_PROJECT_ROOT,
  "data",
  "raw",
  "GBIF"
)

GBIF_DEVELOPMENT_ROOT <- file.path(
  GBIF_ROOT,
  "development"
)


# Separate OJPCP field-data repository
OJPCP_FIELD_DATA_ROOT <- "I:/R/OJPCP/OJPCP-Field-Data"


# Geography audit outputs
GEOGRAPHY_AUDIT_ROOT <- file.path(
  VPJD_PROJECT_ROOT,
  "audit",
  "geography"
)


# ------------------------------------------------------------------------------
# 1. SAFETY CHECKS
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" VPJD Geography Module — Existing Sources Audit\n")
cat("============================================================\n\n")

cat("Published VPJD baseline:\n")
cat(VPJD_RELEASE_ROOT, "\n\n")

if (!dir.exists(VPJD_RELEASE_ROOT)) {
  stop(
    "Published VPJD v1.0.0 release not found:\n",
    VPJD_RELEASE_ROOT
  )
}

cat("VPJD v1.0.0 release found.\n")
cat("This directory will be treated as READ ONLY.\n\n")


# ------------------------------------------------------------------------------
# 2. CREATE AUDIT OUTPUT DIRECTORY
# ------------------------------------------------------------------------------

if (!dir.exists(GEOGRAPHY_AUDIT_ROOT)) {
  dir.create(
    GEOGRAPHY_AUDIT_ROOT,
    recursive = TRUE
  )
}

if (!dir.exists(GEOGRAPHY_AUDIT_ROOT)) {
  stop(
    "Could not create geography audit directory:\n",
    GEOGRAPHY_AUDIT_ROOT
  )
}

cat("Audit output directory:\n")
cat(GEOGRAPHY_AUDIT_ROOT, "\n\n")


# ------------------------------------------------------------------------------
# 3. HELPER FUNCTIONS
# ------------------------------------------------------------------------------

normalise_path_safe <- function(x) {
  
  normalizePath(
    x,
    winslash = "/",
    mustWork = FALSE
  )
  
}


get_extension <- function(x) {
  
  ext <- tools::file_ext(x)
  
  ext[ext == ""] <- NA_character_
  
  tolower(ext)
  
}


classify_file_type <- function(extension) {
  
  if (is.na(extension)) {
    return("unknown")
  }
  
  if (extension %in% c(
    "csv", "tsv", "txt", "xlsx", "xls",
    "rds", "rdata", "rda", "parquet"
  )) {
    return("tabular_data")
  }
  
  if (extension %in% c(
    "shp", "gpkg", "geojson", "kml",
    "kmz", "gdb", "tif", "tiff"
  )) {
    return("spatial_data")
  }
  
  if (extension %in% c(
    "r", "rmd", "qmd", "py"
  )) {
    return("script")
  }
  
  if (extension %in% c(
    "md", "pdf", "doc", "docx",
    "cff", "json", "xml", "yml", "yaml"
  )) {
    return("documentation_or_metadata")
  }
  
  if (extension %in% c(
    "zip", "gz", "7z", "tar"
  )) {
    return("archive")
  }
  
  "other"
  
}


audit_directory <- function(
    path,
    source_name,
    source_class,
    canonical_status = "audit_source"
) {
  
  if (!dir.exists(path)) {
    
    cat(
      "WARNING: directory not found:",
      path,
      "\n"
    )
    
    return(NULL)
    
  }
  
  files <- list.files(
    path,
    recursive = TRUE,
    full.names = TRUE,
    all.files = FALSE,
    include.dirs = FALSE
  )
  
  if (length(files) == 0) {
    
    cat(
      "No files found:",
      path,
      "\n"
    )
    
    return(NULL)
    
  }
  
  info <- file.info(files)
  
  extension <- get_extension(files)
  
  data.frame(
    source_name = source_name,
    source_class = source_class,
    canonical_status = canonical_status,
    root_directory = normalise_path_safe(path),
    file_name = basename(files),
    extension = extension,
    file_type = vapply(
      extension,
      classify_file_type,
      character(1)
    ),
    full_path = normalise_path_safe(files),
    size_bytes = info$size,
    modified = info$mtime,
    stringsAsFactors = FALSE
  )
  
}


# ------------------------------------------------------------------------------
# 4. DEFINE SOURCE REGISTRY
# ------------------------------------------------------------------------------

source_registry <- data.frame(
  
  source_name = c(
    "VPJD_v1.0.0_published_data",
    "VPJD_v1.0.0_metadata",
    "VPJD_v1.0.0_provenance",
    "VPJD_v1.0.0_audit",
    "VPJD_existing_geography",
    "Japan_NLNI_N03_2025",
    "GBIF_raw",
    "GBIF_development",
    "OJPCP_field_data"
  ),
  
  path = c(
    VPJD_RELEASE_DATA,
    VPJD_RELEASE_METADATA,
    VPJD_RELEASE_PROVENANCE,
    VPJD_RELEASE_AUDIT,
    GEOGRAPHY_ROOT,
    NLNI_ROOT,
    GBIF_ROOT,
    GBIF_DEVELOPMENT_ROOT,
    OJPCP_FIELD_DATA_ROOT
  ),
  
  source_class = c(
    "published_taxonomic_baseline",
    "published_metadata",
    "published_provenance",
    "published_audit",
    "existing_geographic_data",
    "administrative_geography",
    "occurrence_data",
    "development_test_data",
    "field_data"
  ),
  
  canonical_status = c(
    "immutable_reference",
    "immutable_reference",
    "immutable_reference",
    "immutable_reference",
    "audit_source",
    "audit_source",
    "audit_source",
    "development_only",
    "audit_source"
  ),
  
  stringsAsFactors = FALSE
)


# Check whether sources exist
source_registry$exists <- dir.exists(
  source_registry$path
)


# ------------------------------------------------------------------------------
# 5. DISPLAY SOURCE REGISTRY
# ------------------------------------------------------------------------------

cat("Source directories being audited:\n\n")

print(
  source_registry[
    ,
    c(
      "source_name",
      "source_class",
      "canonical_status",
      "exists",
      "path"
    )
  ],
  row.names = FALSE
)

cat("\n")


# ------------------------------------------------------------------------------
# 6. INVENTORY ALL SOURCE FILES
# ------------------------------------------------------------------------------

audit_results <- vector(
  "list",
  nrow(source_registry)
)

for (i in seq_len(nrow(source_registry))) {
  
  cat(
    "Auditing:",
    source_registry$source_name[i],
    "\n"
  )
  
  audit_results[[i]] <- audit_directory(
    
    path =
      source_registry$path[i],
    
    source_name =
      source_registry$source_name[i],
    
    source_class =
      source_registry$source_class[i],
    
    canonical_status =
      source_registry$canonical_status[i]
    
  )
  
}


audit_results <- Filter(
  Negate(is.null),
  audit_results
)


if (length(audit_results) == 0) {
  
  stop(
    "No files were discovered in any audit source."
  )
  
}


file_inventory <- do.call(
  rbind,
  audit_results
)


row.names(file_inventory) <- NULL


# ------------------------------------------------------------------------------
# 7. IDENTIFY POTENTIALLY RELEVANT DATA FILES
# ------------------------------------------------------------------------------

candidate_extensions <- c(
  "csv",
  "tsv",
  "txt",
  "xlsx",
  "xls",
  "rds",
  "rdata",
  "rda",
  "parquet",
  "shp",
  "gpkg",
  "geojson",
  "kml",
  "kmz"
)


candidate_data_files <- file_inventory[
  !is.na(file_inventory$extension) &
    file_inventory$extension %in% candidate_extensions,
]


# ------------------------------------------------------------------------------
# 8. FLAG FILES WITH GEOGRAPHICALLY RELEVANT NAMES
# ------------------------------------------------------------------------------

geography_terms <- paste(
  c(
    "geograph",
    "distribution",
    "occurrence",
    "locality",
    "location",
    "latitude",
    "longitude",
    "coordinate",
    "prefecture",
    "municip",
    "gbif",
    "specimen",
    "herbarium",
    "range",
    "native",
    "introduced",
    "n03",
    "gis"
  ),
  collapse = "|"
)


file_inventory$geography_name_flag <- grepl(
  geography_terms,
  file_inventory$file_name,
  ignore.case = TRUE
)


# ------------------------------------------------------------------------------
# 9. FLAG POSSIBLE TAXONOMIC TABLES
# ------------------------------------------------------------------------------

taxonomy_terms <- paste(
  c(
    "taxon",
    "taxonomy",
    "accepted",
    "species",
    "scientific",
    "vpjd",
    "name"
  ),
  collapse = "|"
)


file_inventory$taxonomy_name_flag <- grepl(
  taxonomy_terms,
  file_inventory$file_name,
  ignore.case = TRUE
)


# ------------------------------------------------------------------------------
# 10. IDENTIFY PUBLISHED VPJD DATA CANDIDATES
# ------------------------------------------------------------------------------

published_vpjd_candidates <- file_inventory[
  
  file_inventory$source_name ==
    "VPJD_v1.0.0_published_data" &
    
    file_inventory$file_type ==
    "tabular_data",
  
]


# ------------------------------------------------------------------------------
# 11. FILE-TYPE SUMMARY
# ------------------------------------------------------------------------------

file_type_summary <- as.data.frame(
  table(
    file_inventory$source_name,
    file_inventory$file_type
  ),
  stringsAsFactors = FALSE
)

names(file_type_summary) <- c(
  "source_name",
  "file_type",
  "n_files"
)

file_type_summary <- file_type_summary[
  file_type_summary$n_files > 0,
]


# ------------------------------------------------------------------------------
# 12. SOURCE SUMMARY
# ------------------------------------------------------------------------------

source_summary <- aggregate(
  
  file_inventory$file_name,
  
  by = list(
    source_name =
      file_inventory$source_name,
    source_class =
      file_inventory$source_class,
    canonical_status =
      file_inventory$canonical_status
  ),
  
  FUN = length
  
)

names(source_summary)[
  names(source_summary) == "x"
] <- "n_files"


# ------------------------------------------------------------------------------
# 13. WRITE AUDIT OUTPUTS
# ------------------------------------------------------------------------------

output_source_registry <- file.path(
  GEOGRAPHY_AUDIT_ROOT,
  "geography_01_source_registry.csv"
)

output_file_inventory <- file.path(
  GEOGRAPHY_AUDIT_ROOT,
  "geography_01_file_inventory.csv"
)

output_candidate_files <- file.path(
  GEOGRAPHY_AUDIT_ROOT,
  "geography_01_candidate_data_files.csv"
)

output_published_candidates <- file.path(
  GEOGRAPHY_AUDIT_ROOT,
  "geography_01_published_vpjd_candidates.csv"
)

output_source_summary <- file.path(
  GEOGRAPHY_AUDIT_ROOT,
  "geography_01_source_summary.csv"
)

output_file_type_summary <- file.path(
  GEOGRAPHY_AUDIT_ROOT,
  "geography_01_file_type_summary.csv"
)


write.csv(
  source_registry,
  output_source_registry,
  row.names = FALSE,
  na = ""
)

write.csv(
  file_inventory,
  output_file_inventory,
  row.names = FALSE,
  na = ""
)

write.csv(
  candidate_data_files,
  output_candidate_files,
  row.names = FALSE,
  na = ""
)

write.csv(
  published_vpjd_candidates,
  output_published_candidates,
  row.names = FALSE,
  na = ""
)

write.csv(
  source_summary,
  output_source_summary,
  row.names = FALSE,
  na = ""
)

write.csv(
  file_type_summary,
  output_file_type_summary,
  row.names = FALSE,
  na = ""
)


# ------------------------------------------------------------------------------
# 14. SAVE SESSION INFORMATION
# ------------------------------------------------------------------------------

session_output <- file.path(
  GEOGRAPHY_AUDIT_ROOT,
  "geography_01_sessionInfo.txt"
)

capture.output(
  sessionInfo(),
  file = session_output
)


# ------------------------------------------------------------------------------
# 15. CONSOLE SUMMARY
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" AUDIT COMPLETE\n")
cat("============================================================\n\n")

cat(
  "Total files inventoried:",
  nrow(file_inventory),
  "\n"
)

cat(
  "Candidate data/spatial files:",
  nrow(candidate_data_files),
  "\n"
)

cat(
  "Candidate published VPJD tables:",
  nrow(published_vpjd_candidates),
  "\n\n"
)


cat("Files by source:\n\n")

print(
  source_summary,
  row.names = FALSE
)


cat("\nCandidate published VPJD data files:\n\n")

if (nrow(published_vpjd_candidates) > 0) {
  
  print(
    published_vpjd_candidates[
      ,
      c(
        "file_name",
        "extension",
        "size_bytes",
        "full_path"
      )
    ],
    row.names = FALSE
  )
  
} else {
  
  cat(
    "No candidate tabular files identified ",
    "inside the published VPJD data directory.\n"
  )
  
}


cat("\nAudit outputs written to:\n")
cat(GEOGRAPHY_AUDIT_ROOT, "\n\n")

cat(
  "No source files or published VPJD files ",
  "have been modified.\n"
)

cat("\n")
cat("Next step:\n")
cat(
  "Review geography_01_source_registry.csv and ",
  "geography_01_published_vpjd_candidates.csv\n"
)
cat(
  "before proceeding to detailed inspection of ",
  "the geographic source contents.\n"
)

cat("\n============================================================\n")