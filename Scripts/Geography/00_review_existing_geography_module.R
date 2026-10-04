# ==============================================================================
# VPJD GEOGRAPHY MODULE
# 00_review_existing_geography_module.R
#
# Purpose:
#   Review and document the existing VPJD/OJPCP Geography work before
#   designing the post-VPJD-v1.0.0 Geography Module.
#
# This script is READ ONLY with respect to existing geography work.
#
# It:
#   - inventories the existing Geography directory
#   - identifies and orders existing R scripts
#   - extracts useful structural information from those scripts
#   - identifies file paths referenced by the scripts
#   - identifies likely input/output datasets
#   - identifies packages and functions used
#   - inventories legacy DBF and other data files
#   - flags key geographic concepts already implemented
#   - searches for VPJD/taxonomic integration
#   - searches for occurrence, botanical-area and coordinate logic
#   - writes review outputs outside the existing Geography directory
#
# It does NOT:
#   - execute the legacy scripts
#   - alter existing files
#   - move or rename files
#   - change taxonomic data
#   - change geographic data
#   - overwrite the published VPJD v1.0.0 release
#
# Published taxonomic baseline:
#   I:/R/Data/VPJD_v1.0.0
#
# ==============================================================================


# ------------------------------------------------------------------------------
# 0. PATHS
# ------------------------------------------------------------------------------

GEOGRAPHY_LEGACY_ROOT <- "I:/R/OJPCP/VPJD-OJPCP/R/Geography"

VPJD_RELEASE_ROOT <- "I:/R/Data/VPJD_v1.0.0"

VPJD_RECOGNISED_TAXA <- file.path(
  VPJD_RELEASE_ROOT,
  "data",
  "vpjd_recognised_taxa.csv"
)

VPJD_CROSSWALK <- file.path(
  VPJD_RELEASE_ROOT,
  "data",
  "vpjd_source_taxon_crosswalk.csv"
)

PROJECT_ROOT <- "I:/R/OJPCP/VPJD-OJPCP"

REVIEW_OUTPUT_ROOT <- file.path(
  PROJECT_ROOT,
  "audit",
  "geography",
  "existing_module_review"
)


# ------------------------------------------------------------------------------
# 1. INITIAL SAFETY CHECKS
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" VPJD — Review Existing Geography Module\n")
cat("============================================================\n\n")

if (!dir.exists(GEOGRAPHY_LEGACY_ROOT)) {
  stop(
    "Existing Geography directory not found:\n",
    GEOGRAPHY_LEGACY_ROOT
  )
}

cat("Existing Geography directory found:\n")
cat(GEOGRAPHY_LEGACY_ROOT, "\n\n")

if (!dir.exists(VPJD_RELEASE_ROOT)) {
  stop(
    "Published VPJD v1.0.0 release not found:\n",
    VPJD_RELEASE_ROOT
  )
}

cat("Published VPJD v1.0.0 baseline found:\n")
cat(VPJD_RELEASE_ROOT, "\n\n")


# ------------------------------------------------------------------------------
# 2. CREATE REVIEW OUTPUT DIRECTORY
# ------------------------------------------------------------------------------

if (!dir.exists(REVIEW_OUTPUT_ROOT)) {
  dir.create(
    REVIEW_OUTPUT_ROOT,
    recursive = TRUE
  )
}

if (!dir.exists(REVIEW_OUTPUT_ROOT)) {
  stop(
    "Could not create review output directory:\n",
    REVIEW_OUTPUT_ROOT
  )
}

cat("Review outputs will be written to:\n")
cat(REVIEW_OUTPUT_ROOT, "\n\n")


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


collapse_unique <- function(x) {
  
  x <- unique(x)
  
  x <- x[
    !is.na(x) &
      nzchar(x)
  ]
  
  if (length(x) == 0) {
    return(NA_character_)
  }
  
  paste(
    x,
    collapse = " | "
  )
}


safe_read_lines <- function(path) {
  
  tryCatch(
    
    readLines(
      path,
      warn = FALSE,
      encoding = "UTF-8"
    ),
    
    error = function(e) {
      
      tryCatch(
        
        readLines(
          path,
          warn = FALSE
        ),
        
        error = function(e2) {
          character(0)
        }
        
      )
      
    }
    
  )
  
}


extract_matches <- function(lines, pattern) {
  
  hits <- grep(
    pattern,
    lines,
    value = TRUE,
    ignore.case = TRUE,
    perl = TRUE
  )
  
  hits <- trimws(hits)
  
  hits[nzchar(hits)]
  
}


count_matches <- function(lines, pattern) {
  
  sum(
    grepl(
      pattern,
      lines,
      ignore.case = TRUE,
      perl = TRUE
    )
  )
  
}


# ------------------------------------------------------------------------------
# 4. INVENTORY ENTIRE EXISTING GEOGRAPHY DIRECTORY
# ------------------------------------------------------------------------------

all_files <- list.files(
  GEOGRAPHY_LEGACY_ROOT,
  recursive = TRUE,
  full.names = TRUE,
  all.files = FALSE,
  include.dirs = FALSE
)

file_info <- file.info(all_files)

extensions <- tolower(
  tools::file_ext(all_files)
)

extensions[
  extensions == ""
] <- NA_character_


file_inventory <- data.frame(
  
  file_name = basename(all_files),
  
  extension = extensions,
  
  size_bytes = file_info$size,
  
  modified = file_info$mtime,
  
  full_path = normalise_path_safe(
    all_files
  ),
  
  stringsAsFactors = FALSE
  
)


# ------------------------------------------------------------------------------
# 5. CLASSIFY FILES
# ------------------------------------------------------------------------------

file_inventory$file_class <- "other"


file_inventory$file_class[
  file_inventory$extension == "r"
] <- "R_script"


file_inventory$file_class[
  file_inventory$extension %in% c(
    "dbf",
    "csv",
    "tsv",
    "txt",
    "xlsx",
    "xls",
    "rds",
    "rda",
    "rdata"
  )
] <- "tabular_data"


file_inventory$file_class[
  file_inventory$extension %in% c(
    "shp",
    "shx",
    "prj",
    "cpg",
    "gpkg",
    "geojson",
    "kml",
    "kmz",
    "tif",
    "tiff"
  )
] <- "spatial_data"


# ------------------------------------------------------------------------------
# 6. IDENTIFY R SCRIPTS
# ------------------------------------------------------------------------------

r_scripts <- file_inventory[
  file_inventory$file_class == "R_script",
]

r_scripts <- r_scripts[
  order(r_scripts$file_name),
]

row.names(r_scripts) <- NULL


cat(
  "R scripts discovered:",
  nrow(r_scripts),
  "\n"
)

cat(
  "Other files discovered:",
  nrow(file_inventory) - nrow(r_scripts),
  "\n\n"
)


# ------------------------------------------------------------------------------
# 7. DEFINE CONCEPTS WE WANT TO FIND
# ------------------------------------------------------------------------------

concept_patterns <- list(
  
  integrated_japan =
    "integrated.?japan",
  
  botanical_area =
    "botanical.?area|botanical_area",
  
  occurrence =
    "occurrence|occurrences",
  
  coordinate =
    "coordinate|latitude|longitude|decimalLatitude|decimalLongitude",
  
  nlni =
    "nlni|N03",
  
  gazetteer =
    "gazetteer",
  
  prefecture =
    "prefecture|prefectural",
  
  hokkaido =
    "hokkaido",
  
  kurile =
    "kurile|kuril",
  
  ryukyu =
    "ryukyu",
  
  shikoku =
    "shikoku",
  
  quarantine =
    "quarantine",
  
  outside_area =
    "outside.?botanical|outside_botanical",
  
  taxon =
    "taxon|taxa|scientific.?name|scientificName",
  
  vpjd =
    "vpjd",
  
  star =
    "star.?rating|star_rating|black.?star|gold.?star|blue.?star|green.?star",
  
  range =
    "range.?size|range_size|distribution",
  
  gbif =
    "gbif",
  
  wcvp =
    "wcvp",
  
  native_status =
    "native|introduced|alien|endemic",
  
  spatial_join =
    "st_join|st_intersects|st_within|st_contains",
  
  sf =
    "library\\(sf\\)|require\\(sf\\)|sf::",
  
  CRS =
    "crs|EPSG|st_transform|st_crs"
  
)


# ------------------------------------------------------------------------------
# 8. REVIEW EACH R SCRIPT
# ------------------------------------------------------------------------------

script_reviews <- vector(
  "list",
  nrow(r_scripts)
)

script_hits <- list()

hit_counter <- 1L


for (i in seq_len(nrow(r_scripts))) {
  
  script_path <- r_scripts$full_path[i]
  
  lines <- safe_read_lines(
    script_path
  )
  
  cat(
    "Reviewing:",
    r_scripts$file_name[i],
    "\n"
  )
  
  
  # --------------------------------------------------------------------------
  # Packages
  # --------------------------------------------------------------------------
  
  package_lines <- extract_matches(
    lines,
    "library\\s*\\(|require\\s*\\("
  )
  
  
  # --------------------------------------------------------------------------
  # File/path operations
  # --------------------------------------------------------------------------
  
  path_lines <- extract_matches(
    lines,
    "read\\.|read_|write\\.|write_|st_read|st_write|saveRDS|readRDS|load\\(|save\\(|file\\.path|[A-Za-z]:[/\\\\]"
  )
  
  
  # --------------------------------------------------------------------------
  # Spatial operations
  # --------------------------------------------------------------------------
  
  spatial_lines <- extract_matches(
    lines,
    "st_|sf::|sp::|terra::|raster::"
  )
  
  
  # --------------------------------------------------------------------------
  # Data transformations
  # --------------------------------------------------------------------------
  
  transform_lines <- extract_matches(
    lines,
    "mutate\\(|filter\\(|select\\(|rename\\(|left_join\\(|inner_join\\(|full_join\\(|merge\\("
  )
  
  
  # --------------------------------------------------------------------------
  # Script-level summary
  # --------------------------------------------------------------------------
  
  script_reviews[[i]] <- data.frame(
    
    script_order = i,
    
    script_name =
      r_scripts$file_name[i],
    
    full_path =
      script_path,
    
    size_bytes =
      r_scripts$size_bytes[i],
    
    modified =
      r_scripts$modified[i],
    
    n_lines =
      length(lines),
    
    n_package_lines =
      length(package_lines),
    
    n_path_io_lines =
      length(path_lines),
    
    n_spatial_lines =
      length(spatial_lines),
    
    n_transform_lines =
      length(transform_lines),
    
    packages =
      collapse_unique(package_lines),
    
    stringsAsFactors = FALSE
    
  )
  
  
  # --------------------------------------------------------------------------
  # Concept search
  # --------------------------------------------------------------------------
  
  for (concept_name in names(concept_patterns)) {
    
    pattern <- concept_patterns[[concept_name]]
    
    hits <- grep(
      pattern,
      lines,
      ignore.case = TRUE,
      perl = TRUE
    )
    
    if (length(hits) > 0) {
      
      for (h in hits) {
        
        script_hits[[hit_counter]] <- data.frame(
          
          script_name =
            r_scripts$file_name[i],
          
          concept =
            concept_name,
          
          line_number =
            h,
          
          line_text =
            trimws(lines[h]),
          
          stringsAsFactors = FALSE
          
        )
        
        hit_counter <- hit_counter + 1L
        
      }
      
    }
    
  }
  
}


script_review <- do.call(
  rbind,
  script_reviews
)


if (length(script_hits) > 0) {
  
  concept_hits <- do.call(
    rbind,
    script_hits
  )
  
} else {
  
  concept_hits <- data.frame(
    script_name = character(0),
    concept = character(0),
    line_number = integer(0),
    line_text = character(0),
    stringsAsFactors = FALSE
  )
  
}


# ------------------------------------------------------------------------------
# 9. BUILD SCRIPT × CONCEPT MATRIX
# ------------------------------------------------------------------------------

concept_matrix <- data.frame(
  script_name = r_scripts$file_name,
  stringsAsFactors = FALSE
)


for (concept_name in names(concept_patterns)) {
  
  concept_matrix[[concept_name]] <- vapply(
    
    r_scripts$full_path,
    
    function(path) {
      
      lines <- safe_read_lines(
        path
      )
      
      count_matches(
        lines,
        concept_patterns[[concept_name]]
      )
      
    },
    
    integer(1)
    
  )
  
}


# ------------------------------------------------------------------------------
# 10. EXTRACT REFERENCED FILE PATHS / DATASET NAMES
# ------------------------------------------------------------------------------

path_reference_results <- list()

path_counter <- 1L


for (i in seq_len(nrow(r_scripts))) {
  
  lines <- safe_read_lines(
    r_scripts$full_path[i]
  )
  
  hits <- grep(
    
    paste(
      c(
        "\\.csv",
        "\\.dbf",
        "\\.shp",
        "\\.gpkg",
        "\\.geojson",
        "\\.rds",
        "\\.rda",
        "\\.rdata",
        "\\.xlsx",
        "\\.xls",
        "\\.txt"
      ),
      collapse = "|"
    ),
    
    lines,
    
    ignore.case = TRUE,
    
    value = FALSE
    
  )
  
  
  if (length(hits) > 0) {
    
    for (h in hits) {
      
      path_reference_results[[path_counter]] <- data.frame(
        
        script_name =
          r_scripts$file_name[i],
        
        line_number =
          h,
        
        line_text =
          trimws(lines[h]),
        
        stringsAsFactors = FALSE
        
      )
      
      path_counter <- path_counter + 1L
      
    }
    
  }
  
}


if (length(path_reference_results) > 0) {
  
  path_references <- do.call(
    rbind,
    path_reference_results
  )
  
} else {
  
  path_references <- data.frame(
    script_name = character(0),
    line_number = integer(0),
    line_text = character(0),
    stringsAsFactors = FALSE
  )
  
}


# ------------------------------------------------------------------------------
# 11. IDENTIFY INPUT / OUTPUT OPERATIONS
# ------------------------------------------------------------------------------

io_results <- list()

io_counter <- 1L


input_pattern <- paste(
  c(
    "read\\.csv",
    "read_csv",
    "read\\.table",
    "read_tsv",
    "readRDS",
    "st_read",
    "read\\.dbf",
    "foreign::read\\.dbf",
    "read_excel",
    "load\\("
  ),
  collapse = "|"
)


output_pattern <- paste(
  c(
    "write\\.csv",
    "write_csv",
    "write_tsv",
    "saveRDS",
    "st_write",
    "write\\.dbf",
    "write_xlsx",
    "save\\("
  ),
  collapse = "|"
)


for (i in seq_len(nrow(r_scripts))) {
  
  lines <- safe_read_lines(
    r_scripts$full_path[i]
  )
  
  for (j in seq_along(lines)) {
    
    operation <- NA_character_
    
    if (grepl(
      input_pattern,
      lines[j],
      ignore.case = TRUE,
      perl = TRUE
    )) {
      operation <- "input"
    }
    
    if (grepl(
      output_pattern,
      lines[j],
      ignore.case = TRUE,
      perl = TRUE
    )) {
      operation <- ifelse(
        is.na(operation),
        "output",
        "input_and_output"
      )
    }
    
    if (!is.na(operation)) {
      
      io_results[[io_counter]] <- data.frame(
        
        script_name =
          r_scripts$file_name[i],
        
        line_number =
          j,
        
        operation =
          operation,
        
        line_text =
          trimws(lines[j]),
        
        stringsAsFactors = FALSE
        
      )
      
      io_counter <- io_counter + 1L
      
    }
    
  }
  
}


if (length(io_results) > 0) {
  
  io_operations <- do.call(
    rbind,
    io_results
  )
  
} else {
  
  io_operations <- data.frame(
    script_name = character(0),
    line_number = integer(0),
    operation = character(0),
    line_text = character(0),
    stringsAsFactors = FALSE
  )
  
}


# ------------------------------------------------------------------------------
# 12. INVENTORY LEGACY DBF FILES
# ------------------------------------------------------------------------------

dbf_inventory <- file_inventory[
  file_inventory$extension == "dbf",
]

row.names(dbf_inventory) <- NULL


# ------------------------------------------------------------------------------
# 13. OPTIONAL: INSPECT DBF STRUCTURE
#
# READ ONLY.
# Requires package 'foreign', which normally ships with R.
# ------------------------------------------------------------------------------

dbf_structure_results <- list()

dbf_counter <- 1L


if (
  nrow(dbf_inventory) > 0 &&
  requireNamespace(
    "foreign",
    quietly = TRUE
  )
) {
  
  for (i in seq_len(nrow(dbf_inventory))) {
    
    dbf_path <- dbf_inventory$full_path[i]
    
    cat(
      "Inspecting DBF structure:",
      dbf_inventory$file_name[i],
      "\n"
    )
    
    dbf_data <- tryCatch(
      
      foreign::read.dbf(
        dbf_path,
        as.is = TRUE
      ),
      
      error = function(e) NULL
      
    )
    
    
    if (!is.null(dbf_data)) {
      
      dbf_structure_results[[dbf_counter]] <- data.frame(
        
        file_name =
          dbf_inventory$file_name[i],
        
        n_rows =
          nrow(dbf_data),
        
        n_columns =
          ncol(dbf_data),
        
        column_names =
          paste(
            names(dbf_data),
            collapse = " | "
          ),
        
        stringsAsFactors = FALSE
        
      )
      
      dbf_counter <- dbf_counter + 1L
      
    }
    
    rm(dbf_data)
    
  }
  
}


if (length(dbf_structure_results) > 0) {
  
  dbf_structure <- do.call(
    rbind,
    dbf_structure_results
  )
  
} else {
  
  dbf_structure <- data.frame(
    file_name = character(0),
    n_rows = integer(0),
    n_columns = integer(0),
    column_names = character(0),
    stringsAsFactors = FALSE
  )
  
}


# ------------------------------------------------------------------------------
# 14. REVIEW PUBLISHED VPJD BACKBONE STRUCTURE
#
# Only column names and dimensions are inspected.
# ------------------------------------------------------------------------------

vpjd_structure <- data.frame()


if (file.exists(VPJD_RECOGNISED_TAXA)) {
  
  vpjd_taxa <- read.csv(
    VPJD_RECOGNISED_TAXA,
    nrows = 5,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  
  vpjd_structure <- data.frame(
    
    file =
      "vpjd_recognised_taxa.csv",
    
    column_number =
      seq_along(names(vpjd_taxa)),
    
    column_name =
      names(vpjd_taxa),
    
    stringsAsFactors = FALSE
    
  )
  
  rm(vpjd_taxa)
  
}


# ------------------------------------------------------------------------------
# 15. SUMMARY OF CONCEPTS ALREADY IMPLEMENTED
# ------------------------------------------------------------------------------

concept_summary <- data.frame(
  
  concept =
    names(concept_patterns),
  
  scripts_with_hits =
    vapply(
      
      names(concept_patterns),
      
      function(x) {
        
        sum(
          concept_matrix[[x]] > 0,
          na.rm = TRUE
        )
        
      },
      
      integer(1)
      
    ),
  
  total_hits =
    vapply(
      
      names(concept_patterns),
      
      function(x) {
        
        sum(
          concept_matrix[[x]],
          na.rm = TRUE
        )
        
      },
      
      integer(1)
      
    ),
  
  stringsAsFactors = FALSE
  
)


concept_summary <- concept_summary[
  order(
    -concept_summary$scripts_with_hits,
    -concept_summary$total_hits
  ),
]


# ------------------------------------------------------------------------------
# 16. SCRIPT PIPELINE SUMMARY
# ------------------------------------------------------------------------------

pipeline_summary <- data.frame(
  
  order =
    seq_len(nrow(r_scripts)),
  
  script =
    r_scripts$file_name,
  
  modified =
    r_scripts$modified,
  
  size_kb =
    round(
      r_scripts$size_bytes / 1024,
      1
    ),
  
  stringsAsFactors = FALSE
  
)


# ------------------------------------------------------------------------------
# 17. WRITE REVIEW OUTPUTS
# ------------------------------------------------------------------------------

write.csv(
  file_inventory,
  file.path(
    REVIEW_OUTPUT_ROOT,
    "00_existing_geography_file_inventory.csv"
  ),
  row.names = FALSE,
  na = ""
)


write.csv(
  pipeline_summary,
  file.path(
    REVIEW_OUTPUT_ROOT,
    "00_existing_geography_pipeline.csv"
  ),
  row.names = FALSE,
  na = ""
)


write.csv(
  script_review,
  file.path(
    REVIEW_OUTPUT_ROOT,
    "00_existing_geography_script_review.csv"
  ),
  row.names = FALSE,
  na = ""
)


write.csv(
  concept_matrix,
  file.path(
    REVIEW_OUTPUT_ROOT,
    "00_existing_geography_concept_matrix.csv"
  ),
  row.names = FALSE,
  na = ""
)


write.csv(
  concept_summary,
  file.path(
    REVIEW_OUTPUT_ROOT,
    "00_existing_geography_concept_summary.csv"
  ),
  row.names = FALSE,
  na = ""
)


write.csv(
  concept_hits,
  file.path(
    REVIEW_OUTPUT_ROOT,
    "00_existing_geography_concept_hits.csv"
  ),
  row.names = FALSE,
  na = ""
)


write.csv(
  path_references,
  file.path(
    REVIEW_OUTPUT_ROOT,
    "00_existing_geography_path_references.csv"
  ),
  row.names = FALSE,
  na = ""
)


write.csv(
  io_operations,
  file.path(
    REVIEW_OUTPUT_ROOT,
    "00_existing_geography_io_operations.csv"
  ),
  row.names = FALSE,
  na = ""
)


write.csv(
  dbf_inventory,
  file.path(
    REVIEW_OUTPUT_ROOT,
    "00_existing_geography_dbf_inventory.csv"
  ),
  row.names = FALSE,
  na = ""
)


write.csv(
  dbf_structure,
  file.path(
    REVIEW_OUTPUT_ROOT,
    "00_existing_geography_dbf_structure.csv"
  ),
  row.names = FALSE,
  na = ""
)


if (nrow(vpjd_structure) > 0) {
  
  write.csv(
    vpjd_structure,
    file.path(
      REVIEW_OUTPUT_ROOT,
      "00_published_vpjd_backbone_structure.csv"
    ),
    row.names = FALSE,
    na = ""
  )
  
}


# ------------------------------------------------------------------------------
# 18. SAVE SESSION INFORMATION
# ------------------------------------------------------------------------------

capture.output(
  sessionInfo(),
  file = file.path(
    REVIEW_OUTPUT_ROOT,
    "00_existing_geography_review_sessionInfo.txt"
  )
)


# ------------------------------------------------------------------------------
# 19. CONSOLE SUMMARY
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" EXISTING GEOGRAPHY MODULE REVIEW COMPLETE\n")
cat("============================================================\n\n")


cat(
  "Existing R scripts reviewed:",
  nrow(r_scripts),
  "\n"
)

cat(
  "Total files inventoried:",
  nrow(file_inventory),
  "\n"
)

cat(
  "Legacy DBF files:",
  nrow(dbf_inventory),
  "\n\n"
)


cat("Existing script pipeline:\n\n")

print(
  pipeline_summary,
  row.names = FALSE
)


cat("\n")
cat("Geographic concepts already represented:\n\n")

print(
  concept_summary,
  row.names = FALSE
)


cat("\n")
cat("Legacy DBF structures:\n\n")

if (nrow(dbf_structure) > 0) {
  
  print(
    dbf_structure,
    row.names = FALSE
  )
  
} else {
  
  cat(
    "No DBF structures were read.\n"
  )
  
}


cat("\n")
cat("Review outputs written to:\n")
cat(REVIEW_OUTPUT_ROOT, "\n\n")


cat(
  "No existing Geography files have been modified.\n"
)

cat(
  "No published VPJD v1.0.0 files have been modified.\n\n"
)


cat("NEXT STEP:\n")
cat(
  "Use this review to determine which existing geographic ",
  "components can be retained, updated or connected to ",
  "the published VPJD v1.0.0 taxonomic backbone.\n"
)


cat("\n============================================================\n")