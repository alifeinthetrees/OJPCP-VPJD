# ==============================================================================
# VPJD GEOGRAPHY MODULE
# 00b_audit_vpjd_reference_gdb.R
#
# Purpose:
#   Read-only audit of the existing Esri File Geodatabase:
#
#   I:/R/OJPCP/VPJD-OJPCP/data/derived/geography/vpjd_reference.gdb
#
# The audit identifies:
#   - geodatabase layers / feature classes
#   - feature counts
#   - geometry types
#   - coordinate reference systems
#   - bounding boxes
#   - field names and classes
#   - candidate taxonomic fields
#   - candidate geographic fields
#   - candidate botanical-area fields
#   - candidate occurrence fields
#   - candidate Star/Bioquality fields
#
# IMPORTANT:
#   The geodatabase is READ ONLY.
#   Nothing inside vpjd_reference.gdb is modified.
#
# ==============================================================================


# ------------------------------------------------------------------------------
# 0. PATHS
# ------------------------------------------------------------------------------

PROJECT_ROOT <- "I:/R/OJPCP/VPJD-OJPCP"

GDB_PATH <- file.path(
  PROJECT_ROOT,
  "data",
  "derived",
  "geography",
  "vpjd_reference.gdb"
)

AUDIT_ROOT <- file.path(
  PROJECT_ROOT,
  "audit",
  "geography",
  "vpjd_reference_gdb"
)


# ------------------------------------------------------------------------------
# 1. INITIAL CHECKS
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" VPJD Reference Geodatabase Audit\n")
cat("============================================================\n\n")

cat("Geodatabase:\n")
cat(GDB_PATH, "\n\n")


if (!dir.exists(GDB_PATH)) {
  
  stop(
    "Geodatabase not found:\n",
    GDB_PATH
  )
  
}


if (!requireNamespace("sf", quietly = TRUE)) {
  
  stop(
    "Package 'sf' is required.\n",
    "Install it with install.packages('sf') and rerun."
  )
  
}


if (!dir.exists(AUDIT_ROOT)) {
  
  dir.create(
    AUDIT_ROOT,
    recursive = TRUE
  )
  
}


cat("Geodatabase found.\n")
cat("Audit mode: READ ONLY\n\n")


# ------------------------------------------------------------------------------
# 2. INVENTORY PHYSICAL GDB FILES
#
# These files are inventoried only.
# They must NOT be opened or manipulated individually.
# ------------------------------------------------------------------------------

physical_files <- list.files(
  GDB_PATH,
  recursive = TRUE,
  full.names = TRUE,
  all.files = TRUE
)

physical_info <- file.info(
  physical_files
)


physical_inventory <- data.frame(
  
  file_name =
    basename(physical_files),
  
  extension =
    tolower(
      tools::file_ext(physical_files)
    ),
  
  size_bytes =
    physical_info$size,
  
  modified =
    physical_info$mtime,
  
  full_path =
    normalizePath(
      physical_files,
      winslash = "/",
      mustWork = FALSE
    ),
  
  stringsAsFactors = FALSE
  
)


# ------------------------------------------------------------------------------
# 3. DISCOVER LOGICAL GEODATABASE LAYERS
# ------------------------------------------------------------------------------

cat("Discovering logical geodatabase layers...\n\n")


gdb_layers <- sf::st_layers(
  GDB_PATH
)


layer_registry <- data.frame(
  
  layer_name =
    gdb_layers$name,
  
  geometry_type =
    as.character(
      gdb_layers$geomtype
    ),
  
  stringsAsFactors = FALSE
  
)


cat(
  "Logical layers discovered:",
  nrow(layer_registry),
  "\n\n"
)


# ------------------------------------------------------------------------------
# 4. HELPER FUNCTIONS
# ------------------------------------------------------------------------------

collapse_unique <- function(x) {
  
  x <- unique(
    x[
      !is.na(x) &
        nzchar(x)
    ]
  )
  
  if (length(x) == 0) {
    return(NA_character_)
  }
  
  paste(
    x,
    collapse = " | "
  )
  
}


match_fields <- function(fields, pattern) {
  
  hits <- fields[
    grepl(
      pattern,
      fields,
      ignore.case = TRUE,
      perl = TRUE
    )
  ]
  
  collapse_unique(hits)
  
}


safe_bbox <- function(x) {
  
  bb <- tryCatch(
    sf::st_bbox(x),
    error = function(e) NULL
  )
  
  if (is.null(bb)) {
    
    return(
      c(
        xmin = NA_real_,
        ymin = NA_real_,
        xmax = NA_real_,
        ymax = NA_real_
      )
    )
    
  }
  
  as.numeric(bb)[1:4]
  
}


# ------------------------------------------------------------------------------
# 5. FIELD-CONCEPT PATTERNS
# ------------------------------------------------------------------------------

taxonomy_pattern <- paste(
  c(
    "taxon",
    "species",
    "scientific",
    "accepted",
    "genus",
    "family",
    "name"
  ),
  collapse = "|"
)


coordinate_pattern <- paste(
  c(
    "^lat$",
    "^long$",
    "latitude",
    "longitude",
    "decimal",
    "coord",
    "xcoord",
    "ycoord"
  ),
  collapse = "|"
)


botanical_area_pattern <- paste(
  c(
    "botanical",
    "area",
    "region",
    "geocode",
    "prefecture",
    "island"
  ),
  collapse = "|"
)


occurrence_pattern <- paste(
  c(
    "occurrence",
    "record",
    "specimen",
    "gbif",
    "locality",
    "location",
    "gaz"
  ),
  collapse = "|"
)


star_pattern <- paste(
  c(
    "star",
    "bioquality",
    "rarity",
    "range"
  ),
  collapse = "|"
)


status_pattern <- paste(
  c(
    "native",
    "introduced",
    "alien",
    "endemic",
    "status"
  ),
  collapse = "|"
)


# ------------------------------------------------------------------------------
# 6. AUDIT EACH LOGICAL LAYER
# ------------------------------------------------------------------------------

layer_summaries <- list()
field_summaries <- list()

layer_counter <- 1L
field_counter <- 1L


for (i in seq_len(nrow(layer_registry))) {
  
  layer_name <- layer_registry$layer_name[i]
  
  cat(
    "Inspecting layer:",
    layer_name,
    "\n"
  )
  
  
  layer <- tryCatch(
    
    suppressWarnings(
      sf::st_read(
        dsn = GDB_PATH,
        layer = layer_name,
        quiet = TRUE
      )
    ),
    
    error = function(e) {
      
      cat(
        "  WARNING: could not read layer:",
        conditionMessage(e),
        "\n"
      )
      
      NULL
      
    }
    
  )
  
  
  if (is.null(layer)) {
    next
  }
  
  
  # --------------------------------------------------------------------------
  # Basic structure
  # --------------------------------------------------------------------------
  
  n_features <- nrow(layer)
  
  n_fields <- ncol(layer)
  
  geometry_column <- attr(
    layer,
    "sf_column"
  )
  
  
  attribute_fields <- setdiff(
    names(layer),
    geometry_column
  )
  
  
  # --------------------------------------------------------------------------
  # Geometry
  # --------------------------------------------------------------------------
  
  geometry_types <- tryCatch(
    
    unique(
      as.character(
        sf::st_geometry_type(
          layer,
          by_geometry = TRUE
        )
      )
    ),
    
    error = function(e) {
      NA_character_
    }
    
  )
  
  
  # --------------------------------------------------------------------------
  # CRS
  # --------------------------------------------------------------------------
  
  layer_crs <- tryCatch(
    sf::st_crs(layer),
    error = function(e) NULL
  )
  
  
  epsg <- NA_integer_
  crs_name <- NA_character_
  crs_wkt <- NA_character_
  
  
  if (!is.null(layer_crs)) {
    
    epsg <- layer_crs$epsg
    
    crs_wkt <- layer_crs$wkt
    
    if (!is.null(layer_crs$Name)) {
      crs_name <- layer_crs$Name
    }
    
  }
  
  
  # --------------------------------------------------------------------------
  # Bounding box
  # --------------------------------------------------------------------------
  
  bb <- safe_bbox(layer)
  
  
  # --------------------------------------------------------------------------
  # Conceptual field matching
  # --------------------------------------------------------------------------
  
  taxon_fields <- match_fields(
    attribute_fields,
    taxonomy_pattern
  )
  
  coordinate_fields <- match_fields(
    attribute_fields,
    coordinate_pattern
  )
  
  botanical_area_fields <- match_fields(
    attribute_fields,
    botanical_area_pattern
  )
  
  occurrence_fields <- match_fields(
    attribute_fields,
    occurrence_pattern
  )
  
  star_fields <- match_fields(
    attribute_fields,
    star_pattern
  )
  
  status_fields <- match_fields(
    attribute_fields,
    status_pattern
  )
  
  
  # --------------------------------------------------------------------------
  # Layer summary
  # --------------------------------------------------------------------------
  
  layer_summaries[[layer_counter]] <- data.frame(
    
    layer_name =
      layer_name,
    
    n_features =
      n_features,
    
    n_columns =
      n_fields,
    
    geometry_types =
      collapse_unique(
        geometry_types
      ),
    
    epsg =
      epsg,
    
    crs_name =
      crs_name,
    
    xmin =
      bb[1],
    
    ymin =
      bb[2],
    
    xmax =
      bb[3],
    
    ymax =
      bb[4],
    
    taxon_fields =
      taxon_fields,
    
    coordinate_fields =
      coordinate_fields,
    
    botanical_area_fields =
      botanical_area_fields,
    
    occurrence_fields =
      occurrence_fields,
    
    star_or_range_fields =
      star_fields,
    
    status_fields =
      status_fields,
    
    stringsAsFactors = FALSE
    
  )
  
  
  layer_counter <- layer_counter + 1L
  
  
  # --------------------------------------------------------------------------
  # Field-level schema
  # --------------------------------------------------------------------------
  
  for (field in attribute_fields) {
    
    x <- layer[[field]]
    
    field_summaries[[field_counter]] <- data.frame(
      
      layer_name =
        layer_name,
      
      field_name =
        field,
      
      field_class =
        paste(
          class(x),
          collapse = "|"
        ),
      
      n_missing =
        sum(
          is.na(x)
        ),
      
      n_non_missing =
        sum(
          !is.na(x)
        ),
      
      n_unique =
        tryCatch(
          length(
            unique(x)
          ),
          error = function(e) NA_integer_
        ),
      
      stringsAsFactors = FALSE
      
    )
    
    field_counter <- field_counter + 1L
    
  }
  
  
  rm(layer)
  
  invisible(gc())
  
}


# ------------------------------------------------------------------------------
# 7. COMBINE RESULTS
# ------------------------------------------------------------------------------

if (length(layer_summaries) > 0) {
  
  layer_summary <- do.call(
    rbind,
    layer_summaries
  )
  
} else {
  
  layer_summary <- data.frame()
  
}


if (length(field_summaries) > 0) {
  
  field_summary <- do.call(
    rbind,
    field_summaries
  )
  
} else {
  
  field_summary <- data.frame()
  
}


# ------------------------------------------------------------------------------
# 8. IDENTIFY POTENTIALLY IMPORTANT LAYERS
# ------------------------------------------------------------------------------

if (nrow(layer_summary) > 0) {
  
  important_layers <- layer_summary[
    
    !is.na(layer_summary$taxon_fields) |
      !is.na(layer_summary$botanical_area_fields) |
      !is.na(layer_summary$occurrence_fields) |
      !is.na(layer_summary$star_or_range_fields),
    
  ]
  
} else {
  
  important_layers <- data.frame()
  
}


# ------------------------------------------------------------------------------
# 9. WRITE AUDIT OUTPUTS
# ------------------------------------------------------------------------------

write.csv(
  physical_inventory,
  file.path(
    AUDIT_ROOT,
    "00b_gdb_physical_file_inventory.csv"
  ),
  row.names = FALSE,
  na = ""
)


write.csv(
  layer_registry,
  file.path(
    AUDIT_ROOT,
    "00b_gdb_layer_registry.csv"
  ),
  row.names = FALSE,
  na = ""
)


write.csv(
  layer_summary,
  file.path(
    AUDIT_ROOT,
    "00b_gdb_layer_summary.csv"
  ),
  row.names = FALSE,
  na = ""
)


write.csv(
  field_summary,
  file.path(
    AUDIT_ROOT,
    "00b_gdb_field_summary.csv"
  ),
  row.names = FALSE,
  na = ""
)


write.csv(
  important_layers,
  file.path(
    AUDIT_ROOT,
    "00b_gdb_potentially_important_layers.csv"
  ),
  row.names = FALSE,
  na = ""
)


# ------------------------------------------------------------------------------
# 10. SAVE SESSION INFORMATION
# ------------------------------------------------------------------------------

capture.output(
  sessionInfo(),
  file = file.path(
    AUDIT_ROOT,
    "00b_gdb_sessionInfo.txt"
  )
)


# ------------------------------------------------------------------------------
# 11. CONSOLE SUMMARY
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" VPJD REFERENCE GEODATABASE AUDIT COMPLETE\n")
cat("============================================================\n\n")


cat(
  "Physical files inside GDB:",
  nrow(physical_inventory),
  "\n"
)


cat(
  "Logical layers discovered:",
  nrow(layer_registry),
  "\n"
)


cat(
  "Logical layers successfully inspected:",
  nrow(layer_summary),
  "\n\n"
)


cat("Layer summary:\n\n")

if (nrow(layer_summary) > 0) {
  
  print(
    layer_summary[
      ,
      c(
        "layer_name",
        "n_features",
        "n_columns",
        "geometry_types",
        "epsg"
      )
    ],
    row.names = FALSE
  )
  
}


cat("\nPotentially important VPJD Geography layers:\n\n")

if (nrow(important_layers) > 0) {
  
  print(
    important_layers[
      ,
      c(
        "layer_name",
        "n_features",
        "taxon_fields",
        "botanical_area_fields",
        "occurrence_fields",
        "star_or_range_fields"
      )
    ],
    row.names = FALSE
  )
  
} else {
  
  cat(
    "No layers automatically flagged.\n"
  )
  
}


cat("\nAudit outputs written to:\n")
cat(AUDIT_ROOT, "\n\n")


cat(
  "No files within vpjd_reference.gdb have been modified.\n"
)


cat("\nNEXT STEP:\n")
cat(
  "Review the logical layers and their schemas before deciding ",
  "which geodatabase components should be retained or integrated ",
  "into the post-v1.0.0 VPJD Geography Module.\n"
)


cat("\n============================================================\n")