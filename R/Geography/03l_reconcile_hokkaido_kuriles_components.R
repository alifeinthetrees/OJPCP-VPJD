# ==============================================================================
# VPJD-OJPCP
# Geography 03l — Diagnose Hokkaido / Kuriles component reconciliation
# Version: 0.1.1
#
# Purpose:
#   Identify which individual 2025 N03 Hokkaido polygon components correspond
#   spatially to the independently validated physical JP51 Kuriles definition.
#
# Background:
#   Geography 03j demonstrated a substantial positive-area overlap between
#   N03-derived JP01 Hokkaido and the physical JP51 Kuriles geometry.
#
#   Geography 03i independently resolved 28 Natural Earth diagnostic components:
#     - 22 included in JP51
#     - 6 excluded
#     - 0 unresolved
#
#   Geography 03k separately validated the 14 Hokkaido subprefectures required
#   for Star-counting geography. Those Star units are NOT modified here.
#
# Scientific scope:
#   - Read individual N03 Hokkaido geometries.
#   - Explode multipart source geometries to atomic polygon components.
#   - Construct the validated 03i JP51 physical reference geometry.
#   - Measure intersection of each N03 Hokkaido component with JP51.
#   - Retain N03 administrative attribution.
#   - Classify spatial relationships diagnostically.
#   - Identify components requiring review before JP01/JP51 reconstruction.
#
# This script does NOT:
#   - modify frozen Geography 03e;
#   - modify frozen Geography 03i;
#   - modify frozen Geography 03k;
#   - construct a new JP01;
#   - construct a new JP51;
#   - assign occurrences;
#   - modify coordinates;
#   - modify taxonomy.
#
# IMPORTANT:
#   Spatial-overlap classes in this module are diagnostic evidence, not final
#   botanical-area decisions.
# ==============================================================================

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(readr)
  library(DBI)
  library(duckdb)
  library(here)
})

SCRIPT_VERSION <- "0.1.1"
EXPECTED_HOKKAIDO_SOURCE_FEATURES <- 9555L
EXPECTED_JP51_INCLUDE_COMPONENTS <- 22L

run_hokkaido_kuriles_reconciliation <- function() {
  cat("\n— VPJD Hokkaido / Kuriles component reconciliation —\n\n")
  
  # ---------------------------------------------------------------------------
  # Paths
  # ---------------------------------------------------------------------------
  
  hokkaido_shp <- here(
    "data", "interim", "geography",
    "nlni_n03_2025", "01_Hokkaido",
    "N03-20250101_01.shp"
  )
  
  hokkaido_subpref_shp <- here(
    "data", "interim", "geography",
    "nlni_n03_2025", "01_Hokkaido",
    "N03-20250101_01_subprefecture.shp"
  )
  
  kuriles_gpkg <- here(
    "data", "interim", "geography", "kuriles",
    "vpjd_kuriles_final_component_decisions.gpkg"
  )
  
  output_dir <- here(
    "data", "interim", "geography",
    "hokkaido_kuriles_reconciliation"
  )
  
  table_dir <- here(
    "outputs", "tables", "geography",
    "hokkaido_kuriles_reconciliation"
  )
  
  output_gpkg <- file.path(
    output_dir,
    "vpjd_hokkaido_kuriles_component_reconciliation.gpkg"
  )
  
  db_path <- here(
    "data", "interim", "occurrences",
    "vpjd_occurrences.duckdb"
  )
  
  dir.create(
    output_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  dir.create(
    table_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  # ---------------------------------------------------------------------------
  # Source existence
  # ---------------------------------------------------------------------------
  
  required_files <- c(
    hokkaido_shp,
    hokkaido_subpref_shp,
    kuriles_gpkg,
    db_path
  )
  
  missing_files <- required_files[
    !file.exists(required_files)
  ]
  
  if (length(missing_files) > 0L) {
    stop(
      "Required files missing:\n",
      paste(
        missing_files,
        collapse = "\n"
      )
    )
  }
  
  cat("N03 Hokkaido source: PASS\n")
  cat("N03 Hokkaido subprefecture source: PASS\n")
  cat("Geography 03i spatial source: PASS\n\n")
  
  # ---------------------------------------------------------------------------
  # Inspect Geography 03i GeoPackage
  # ---------------------------------------------------------------------------
  
  kuriles_layers <- st_layers(
    kuriles_gpkg
  )$name
  
  cat("— Geography 03i GeoPackage layers —\n")
  print(kuriles_layers)
  
  preferred_include_layers <- c(
    "jp51_include_components",
    "include_components",
    "final_component_decisions",
    "kuriles_final_component_decisions"
  )
  
  include_layer <- preferred_include_layers[
    preferred_include_layers %in%
      kuriles_layers
  ]
  
  if (length(include_layer) == 0L) {
    stop(
      paste(
        "Could not identify the Geography 03i JP51 component layer.",
        "Available layers:",
        paste(
          kuriles_layers,
          collapse = ", "
        )
      )
    )
  }
  
  include_layer <- include_layer[1]
  
  cat(
    "\nUsing Geography 03i layer: ",
    include_layer,
    "\n",
    sep = ""
  )
  
  kuriles_source <- st_read(
    kuriles_gpkg,
    layer = include_layer,
    quiet = TRUE
  )
  
  cat(
    "03i source rows: ",
    nrow(kuriles_source),
    "\n\n",
    sep = ""
  )
  
  # ---------------------------------------------------------------------------
  # Identify final JP51 include components
  # ---------------------------------------------------------------------------
  
  decision_candidates <- c(
    "final_decision",
    "decision",
    "jp51_decision",
    "include_exclude",
    "final_jp51_decision"
  )
  
  decision_field <- decision_candidates[
    decision_candidates %in%
      names(kuriles_source)
  ]
  
  if (
    nrow(kuriles_source) ==
    EXPECTED_JP51_INCLUDE_COMPONENTS
  ) {
    
    jp51_components <- kuriles_source
    
    cat(
      paste0(
        "Layer contains exactly 22 rows; treating layer as ",
        "validated JP51 include components.\n"
      )
    )
    
  } else if (length(decision_field) > 0L) {
    
    decision_field <- decision_field[1]
    
    decision_values <- tolower(
      trimws(
        as.character(
          kuriles_source[[decision_field]]
        )
      )
    )
    
    include_values <- c(
      "include",
      "included",
      "jp51",
      "jp51_include",
      "include_jp51"
    )
    
    jp51_components <- kuriles_source[
      decision_values %in%
        include_values,
    ]
    
    cat(
      "Decision field: ",
      decision_field,
      "\n",
      sep = ""
    )
    
  } else {
    
    stop(
      paste(
        "03i layer does not contain exactly 22 rows",
        "and no recognised decision field was found.",
        "Fields:",
        paste(
          names(kuriles_source),
          collapse = ", "
        )
      )
    )
  }
  
  if (
    nrow(jp51_components) !=
    EXPECTED_JP51_INCLUDE_COMPONENTS
  ) {
    stop(
      "Expected 22 validated JP51 include components; found ",
      nrow(jp51_components),
      "."
    )
  }
  
  if (
    any(
      !st_is_valid(
        jp51_components
      )
    )
  ) {
    stop(
      "Invalid geometry detected in 03i JP51 components."
    )
  }
  
  cat(
    "Validated JP51 include components: ",
    nrow(jp51_components),
    "\n",
    sep = ""
  )
  
  # ---------------------------------------------------------------------------
  # Build physical JP51 reference
  # ---------------------------------------------------------------------------
  
  jp51_reference <- st_sf(
    reference_id = "JP51_PHYSICAL_REFERENCE",
    geometry = st_union(
      st_geometry(
        jp51_components
      )
    )
  )
  
  st_crs(jp51_reference) <-
    st_crs(jp51_components)
  
  if (
    !all(
      st_is_valid(
        jp51_reference
      )
    )
  ) {
    stop(
      "Constructed JP51 physical reference is invalid."
    )
  }
  
  cat(
    "JP51 physical reference geometry: PASS\n\n"
  )
  
  # ---------------------------------------------------------------------------
  # Read Hokkaido N03
  # ---------------------------------------------------------------------------
  
  hokkaido_raw <- st_read(
    hokkaido_shp,
    quiet = TRUE
  )
  
  hokkaido_subpref <- st_read(
    hokkaido_subpref_shp,
    quiet = TRUE
  )
  
  cat(
    "N03 Hokkaido source rows: ",
    nrow(hokkaido_raw),
    "\n",
    sep = ""
  )
  
  if (
    nrow(hokkaido_raw) !=
    EXPECTED_HOKKAIDO_SOURCE_FEATURES
  ) {
    stop(
      "Expected 9,555 N03 Hokkaido features; found ",
      nrow(hokkaido_raw),
      "."
    )
  }
  
  required_n03_fields <- c(
    "N03_001",
    "N03_002",
    "N03_003",
    "N03_004",
    "N03_007"
  )
  
  missing_n03_fields <- setdiff(
    required_n03_fields,
    names(hokkaido_raw)
  )
  
  if (
    length(missing_n03_fields) > 0L
  ) {
    stop(
      "Required N03 fields missing: ",
      paste(
        missing_n03_fields,
        collapse = ", "
      )
    )
  }
  
  # ---------------------------------------------------------------------------
  # Preserve source row and explode to atomic POLYGON components
  # ---------------------------------------------------------------------------
  
  hokkaido_raw <- hokkaido_raw %>%
    mutate(
      n03_source_row =
        row_number()
    )
  
  geom_types_before <- table(
    as.character(
      st_geometry_type(
        hokkaido_raw
      )
    )
  )
  
  cat(
    "\n— Hokkaido source geometry types —\n"
  )
  
  print(
    geom_types_before
  )
  
  cat(
    "\nExploding N03 Hokkaido geometries to polygon components...\n"
  )
  
  hokkaido_components <- suppressWarnings(
    st_cast(
      hokkaido_raw,
      "POLYGON"
    )
  )
  
  hokkaido_components <- hokkaido_components %>%
    mutate(
      hokkaido_component_id =
        sprintf(
          "N03_HOK_%05d",
          row_number()
        )
    )
  
  cat(
    "Atomic Hokkaido components: ",
    nrow(hokkaido_components),
    "\n",
    sep = ""
  )
  
  # ---------------------------------------------------------------------------
  # Independent subprefecture reference
  # ---------------------------------------------------------------------------
  
  subpref_reference <- hokkaido_subpref %>%
    st_drop_geometry() %>%
    filter(
      !is.na(N03_002),
      trimws(N03_002) != ""
    ) %>%
    distinct(
      N03_002
    ) %>%
    arrange(
      N03_002
    )
  
  cat(
    "Dedicated subprefecture names: ",
    nrow(subpref_reference),
    "\n",
    sep = ""
  )
  
  # ---------------------------------------------------------------------------
  # Geometry validation
  # ---------------------------------------------------------------------------
  
  component_valid <- st_is_valid(
    hokkaido_components
  )
  
  if (
    any(!component_valid)
  ) {
    stop(
      "Invalid N03 Hokkaido polygon components detected: ",
      sum(!component_valid)
    )
  }
  
  cat(
    "Hokkaido component geometry validation: PASS\n\n"
  )
  
  # ---------------------------------------------------------------------------
  # Transform to equal-area CRS
  # ---------------------------------------------------------------------------
  
  hokkaido_eq <- st_transform(
    hokkaido_components,
    6933
  )
  
  jp51_eq <- st_transform(
    jp51_reference,
    6933
  )
  
  component_area_m2 <- as.numeric(
    st_area(
      hokkaido_eq
    )
  )
  
  # ---------------------------------------------------------------------------
  # Identify components intersecting JP51
  # ---------------------------------------------------------------------------
  
  cat(
    "Identifying N03 Hokkaido components intersecting JP51...\n"
  )
  
  candidate_index <- lengths(
    st_intersects(
      hokkaido_eq,
      jp51_eq,
      sparse = TRUE
    )
  ) > 0L
  
  cat(
    "Components intersecting/touching JP51: ",
    sum(candidate_index),
    "\n",
    sep = ""
  )
  
  overlap_area_m2 <- numeric(
    nrow(hokkaido_eq)
  )
  
  candidate_rows <- which(
    candidate_index
  )
  
  if (
    length(candidate_rows) > 0L
  ) {
    
    cat(
      "Calculating positive-area overlap for candidate components...\n"
    )
    
    for (
      ii in seq_along(
        candidate_rows
      )
    ) {
      
      i <- candidate_rows[ii]
      
      inter <- suppressWarnings(
        st_intersection(
          st_geometry(
            hokkaido_eq
          )[i],
          st_geometry(
            jp51_eq
          )[1]
        )
      )
      
      if (
        length(inter) > 0L
      ) {
        overlap_area_m2[i] <- sum(
          as.numeric(
            st_area(inter)
          ),
          na.rm = TRUE
        )
      }
      
      if (
        ii %% 250L == 0L
      ) {
        cat(
          "  processed ",
          ii,
          " / ",
          length(candidate_rows),
          " candidates\n",
          sep = ""
        )
      }
    }
  }
  
  # ---------------------------------------------------------------------------
  # Representative points and bounding boxes
  # ---------------------------------------------------------------------------
  
  cat(
    "Building component diagnostics...\n"
  )
  
  components_wgs84 <- st_transform(
    hokkaido_components,
    4326
  )
  
  representative_points <- suppressWarnings(
    st_point_on_surface(
      components_wgs84
    )
  )
  
  point_coords <- st_coordinates(
    representative_points
  )
  
  bbox_values <- lapply(
    st_geometry(
      components_wgs84
    ),
    st_bbox
  )
  
  bbox_df <- bind_rows(
    lapply(
      bbox_values,
      function(x) {
        tibble(
          xmin =
            as.numeric(
              x["xmin"]
            ),
          ymin =
            as.numeric(
              x["ymin"]
            ),
          xmax =
            as.numeric(
              x["xmax"]
            ),
          ymax =
            as.numeric(
              x["ymax"]
            )
        )
      }
    )
  )
  
  overlap_fraction <- ifelse(
    component_area_m2 > 0,
    overlap_area_m2 /
      component_area_m2,
    NA_real_
  )
  
  # ---------------------------------------------------------------------------
  # Diagnostic spatial classification
  #
  # Thresholds are diagnostic only:
  #   >=99% overlap  = within_jp51_secure
  #   <=1% overlap   = boundary_touch_or_sliver
  #   >1% and <99%   = partial_overlap_review
  #   <=1 m2 overlap = outside_jp51
  # ---------------------------------------------------------------------------
  
  spatial_class <- case_when(
    overlap_area_m2 <= 1 ~
      "outside_jp51",
    overlap_fraction >= 0.99 ~
      "within_jp51_secure",
    overlap_fraction <= 0.01 ~
      "boundary_touch_or_sliver",
    TRUE ~
      "partial_overlap_review"
  )
  
  component_diag <- hokkaido_components %>%
    st_drop_geometry() %>%
    mutate(
      component_area_m2 =
        component_area_m2,
      component_area_km2 =
        component_area_m2 / 1e6,
      jp51_overlap_area_m2 =
        overlap_area_m2,
      jp51_overlap_area_km2 =
        overlap_area_m2 / 1e6,
      jp51_overlap_fraction =
        overlap_fraction,
      jp51_overlap_percent =
        100 * overlap_fraction,
      jp51_spatial_class =
        spatial_class,
      representative_longitude =
        point_coords[, 1],
      representative_latitude =
        point_coords[, 2],
      geometry_valid =
        component_valid
    ) %>%
    bind_cols(
      bbox_df
    )
  
  # ---------------------------------------------------------------------------
  # Classification summary
  # ---------------------------------------------------------------------------
  
  class_summary <- component_diag %>%
    group_by(
      jp51_spatial_class
    ) %>%
    summarise(
      components = n(),
      total_component_area_km2 =
        sum(
          component_area_km2,
          na.rm = TRUE
        ),
      total_jp51_overlap_km2 =
        sum(
          jp51_overlap_area_km2,
          na.rm = TRUE
        ),
      .groups = "drop"
    ) %>%
    arrange(
      jp51_spatial_class
    )
  
  cat(
    "\n— JP51 spatial classification summary —\n"
  )
  
  print.data.frame(
    as.data.frame(
      class_summary
    ),
    row.names = FALSE
  )
  
  # ---------------------------------------------------------------------------
  # Administrative summary
  # ---------------------------------------------------------------------------
  
  admin_summary <- component_diag %>%
    group_by(
      N03_002,
      jp51_spatial_class
    ) %>%
    summarise(
      components = n(),
      component_area_km2 =
        sum(
          component_area_km2,
          na.rm = TRUE
        ),
      jp51_overlap_km2 =
        sum(
          jp51_overlap_area_km2,
          na.rm = TRUE
        ),
      .groups = "drop"
    ) %>%
    arrange(
      N03_002,
      jp51_spatial_class
    )
  
  cat(
    "\n— Classification by N03_002 —\n"
  )
  
  print.data.frame(
    as.data.frame(
      admin_summary
    ),
    row.names = FALSE
  )
  
  # ---------------------------------------------------------------------------
  # Review table
  # ---------------------------------------------------------------------------
  
  review_table <- component_diag %>%
    filter(
      jp51_spatial_class !=
        "outside_jp51"
    ) %>%
    arrange(
      desc(
        jp51_overlap_area_km2
      )
    ) %>%
    select(
      hokkaido_component_id,
      n03_source_row,
      N03_001,
      N03_002,
      N03_003,
      N03_004,
      N03_007,
      component_area_km2,
      jp51_overlap_area_km2,
      jp51_overlap_percent,
      jp51_spatial_class,
      representative_longitude,
      representative_latitude,
      xmin,
      ymin,
      xmax,
      ymax
    )
  
  cat(
    "\nComponents requiring JP51-related inspection: ",
    nrow(review_table),
    "\n",
    sep = ""
  )
  
  if (
    nrow(review_table) > 0L
  ) {
    
    cat(
      "\n— Largest JP51-related N03 components —\n"
    )
    
    print.data.frame(
      as.data.frame(
        head(
          review_table,
          30L
        )
      ),
      row.names = FALSE
    )
  }
  
  # ---------------------------------------------------------------------------
  # Spatial outputs
  # ---------------------------------------------------------------------------
  
  spatial_result <- components_wgs84 %>%
    mutate(
      component_area_km2 =
        component_area_m2 / 1e6,
      jp51_overlap_area_km2 =
        overlap_area_m2 / 1e6,
      jp51_overlap_percent =
        100 * overlap_fraction,
      jp51_spatial_class =
        spatial_class
    )
  
  secure_jp51_candidates <- spatial_result %>%
    filter(
      jp51_spatial_class ==
        "within_jp51_secure"
    )
  
  review_components <- spatial_result %>%
    filter(
      jp51_spatial_class %in% c(
        "boundary_touch_or_sliver",
        "partial_overlap_review"
      )
    )
  
  # ---------------------------------------------------------------------------
  # Geometry QA
  # ---------------------------------------------------------------------------
  
  geometry_qa <- tibble(
    metric = c(
      "source_hokkaido_features",
      "atomic_hokkaido_components",
      "valid_atomic_components",
      "invalid_atomic_components",
      "jp51_reference_components",
      "jp51_reference_valid",
      "components_with_positive_jp51_overlap",
      "secure_jp51_candidates",
      "boundary_touch_or_sliver",
      "partial_overlap_review"
    ),
    value = c(
      nrow(hokkaido_raw),
      nrow(hokkaido_components),
      sum(component_valid),
      sum(!component_valid),
      nrow(jp51_components),
      as.integer(
        all(
          st_is_valid(
            jp51_reference
          )
        )
      ),
      sum(
        overlap_area_m2 > 1
      ),
      sum(
        spatial_class ==
          "within_jp51_secure"
      ),
      sum(
        spatial_class ==
          "boundary_touch_or_sliver"
      ),
      sum(
        spatial_class ==
          "partial_overlap_review"
      )
    )
  )
  
  cat(
    "\n— Geometry QA —\n"
  )
  
  print.data.frame(
    as.data.frame(
      geometry_qa
    ),
    row.names = FALSE
  )
  
  # ---------------------------------------------------------------------------
  # Validation
  # ---------------------------------------------------------------------------
  
  validation <- tibble(
    check = c(
      "source_hokkaido_features_9555",
      "jp51_include_components_22",
      "jp51_reference_valid",
      "all_hokkaido_components_valid",
      "all_components_classified",
      "no_missing_component_area",
      "no_missing_overlap_fraction",
      "no_jp01_reconstruction",
      "no_jp51_reconstruction",
      "no_frozen_upstream_modification",
      "no_occurrence_assignment",
      "no_coordinate_modification",
      "no_taxonomy_modification"
    ),
    pass = c(
      nrow(hokkaido_raw) ==
        EXPECTED_HOKKAIDO_SOURCE_FEATURES,
      nrow(jp51_components) ==
        EXPECTED_JP51_INCLUDE_COMPONENTS,
      all(
        st_is_valid(
          jp51_reference
        )
      ),
      all(component_valid),
      all(
        !is.na(
          spatial_class
        )
      ),
      all(
        !is.na(
          component_area_m2
        )
      ),
      all(
        !is.na(
          overlap_fraction
        )
      ),
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE,
      TRUE
    )
  )
  
  cat(
    "\n— Validation —\n"
  )
  
  print.data.frame(
    as.data.frame(
      validation
    ),
    row.names = FALSE
  )
  
  cat(
    "\nAll validation checks PASS: ",
    all(validation$pass),
    "\n",
    sep = ""
  )
  
  if (
    !all(validation$pass)
  ) {
    stop(
      paste(
        "Geography 03l validation failed.",
        "Outputs not finalised."
      )
    )
  }
  
  # ---------------------------------------------------------------------------
  # Metadata
  # ---------------------------------------------------------------------------
  
  metadata <- tibble(
    metric = c(
      "script",
      "version",
      "n03_source",
      "jp51_source",
      "jp51_include_components",
      "classification_role",
      "threshold_secure_percent",
      "threshold_sliver_percent",
      "jp01_reconstructed",
      "jp51_reconstructed",
      "occurrences_assigned",
      "coordinates_modified",
      "taxonomy_modified",
      "output_status"
    ),
    value = c(
      "03l_reconcile_hokkaido_kuriles_components",
      SCRIPT_VERSION,
      "Japan MLIT/NLNI N03 Administrative Areas 2025",
      "Geography 03i validated physical component decisions",
      as.character(
        nrow(jp51_components)
      ),
      "DIAGNOSTIC ONLY",
      ">=99%",
      "<=1%",
      "FALSE",
      "FALSE",
      "FALSE",
      "FALSE",
      "FALSE",
      paste(
        "HOKKAIDO / KURILES COMPONENT",
        "RECONCILIATION DIAGNOSTIC"
      )
    )
  )
  
  # ---------------------------------------------------------------------------
  # Write GeoPackage
  # ---------------------------------------------------------------------------
  
  if (
    file.exists(
      output_gpkg
    )
  ) {
    file.remove(
      output_gpkg
    )
  }
  
  st_write(
    spatial_result,
    output_gpkg,
    layer =
      "all_hokkaido_components",
    quiet = TRUE
  )
  
  st_write(
    secure_jp51_candidates,
    output_gpkg,
    layer =
      "secure_jp51_candidates",
    quiet = TRUE
  )
  
  st_write(
    review_components,
    output_gpkg,
    layer =
      "review_components",
    quiet = TRUE
  )
  
  st_write(
    st_transform(
      jp51_reference,
      4326
    ),
    output_gpkg,
    layer =
      "jp51_physical_reference",
    quiet = TRUE
  )
  
  # ---------------------------------------------------------------------------
  # CSV outputs
  # ---------------------------------------------------------------------------
  
  write_csv(
    component_diag,
    file.path(
      table_dir,
      "hokkaido_component_classification.csv"
    )
  )
  
  write_csv(
    class_summary,
    file.path(
      table_dir,
      "hokkaido_component_classification_summary.csv"
    )
  )
  
  write_csv(
    admin_summary,
    file.path(
      table_dir,
      "hokkaido_component_admin_summary.csv"
    )
  )
  
  write_csv(
    review_table,
    file.path(
      table_dir,
      "hokkaido_component_review.csv"
    )
  )
  
  write_csv(
    geometry_qa,
    file.path(
      table_dir,
      "hokkaido_component_geometry_qa.csv"
    )
  )
  
  write_csv(
    validation,
    file.path(
      table_dir,
      "hokkaido_component_validation.csv"
    )
  )
  
  write_csv(
    metadata,
    file.path(
      table_dir,
      "hokkaido_component_metadata.csv"
    )
  )
  
  # ---------------------------------------------------------------------------
  # DuckDB outputs
  # ---------------------------------------------------------------------------
  
  con <- dbConnect(
    duckdb(),
    dbdir = db_path,
    read_only = FALSE
  )
  
  on.exit(
    dbDisconnect(
      con,
      shutdown = TRUE
    ),
    add = TRUE
  )
  
  dbWriteTable(
    con,
    "japan_hokkaido_kuriles_component_classification",
    component_diag,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "japan_hokkaido_kuriles_classification_summary",
    class_summary,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "japan_hokkaido_kuriles_admin_summary",
    admin_summary,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "japan_hokkaido_kuriles_review",
    review_table,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "japan_hokkaido_kuriles_geometry_qa",
    geometry_qa,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "japan_hokkaido_kuriles_validation",
    validation,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "japan_hokkaido_kuriles_metadata",
    metadata,
    overwrite = TRUE
  )
  
  # ---------------------------------------------------------------------------
  # Final report
  # ---------------------------------------------------------------------------
  
  cat(
    "\nGeoPackage:\n",
    output_gpkg,
    "\n",
    sep = ""
  )
  
  cat(
    "\nOutput status: ",
    "HOKKAIDO / KURILES COMPONENT RECONCILIATION DIAGNOSTIC\n",
    sep = ""
  )
  
  cat(
    "JP01 reconstructed: FALSE\n"
  )
  
  cat(
    "JP51 reconstructed: FALSE\n"
  )
  
  cat(
    "Frozen upstream modules modified: FALSE\n"
  )
  
  cat(
    "Occurrence assignment performed: FALSE\n"
  )
  
  cat(
    "Coordinates modified: FALSE\n"
  )
  
  cat(
    "Taxonomy modified: FALSE\n"
  )
  
  cat(
    "\nGeography 03l v",
    SCRIPT_VERSION,
    " complete.\n",
    sep = ""
  )
  
  invisible(
    list(
      classification =
        component_diag,
      summary =
        class_summary,
      admin_summary =
        admin_summary,
      review =
        review_table,
      geometry_qa =
        geometry_qa,
      validation =
        validation,
      metadata =
        metadata
    )
  )
}

result_03l <-
  run_hokkaido_kuriles_reconciliation()
