# ==============================================================================
# VPJD-OJPCP
# Geography 03k — Build Hokkaido Star-geography units
# Version: 0.1.3
#
# Purpose:
#   Build and validate the 14 Hokkaido subprefecture geometries required for
#   the VPJD Star-rating geography.
#
# Scientific scope:
#   - JP01 remains one botanical area in the canonical JP01–JP51 framework.
#   - The 14 Hokkaido subprefectures are a separate Star-geography layer.
#   - This script does NOT reconstruct, split or modify JP01.
#   - This script does NOT resolve the JP01/JP51 Kuriles boundary issue.
#   - This script does NOT assign occurrences.
#   - This script does NOT modify taxonomy or coordinates.
#
# Source:
#   Japan MLIT/NLNI N03 Administrative Areas 2025.
#
# Note:
#   Earlier v0.1.2 calculated an exact symmetric difference between the
#   dissolved Hokkaido subprefecture layer and JP01. This was computationally
#   expensive and scientifically unnecessary for the purpose of this module.
#   v0.1.3 removes that operation. JP01 is checked only as a valid reference.
# ==============================================================================

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(readr)
  library(DBI)
  library(duckdb)
  library(here)
})

SCRIPT_VERSION <- "0.1.3"
SOURCE_VERSION <- "N03-2025"
EXPECTED_MAIN_FEATURES <- 9555L
EXPECTED_SUBPREF_FEATURES <- 9376L
EXPECTED_SUBPREFECTURES <- 14L

run_hokkaido_star_geography <- function() {
  cat("\n— VPJD Hokkaido Star-geography units —\n\n")
  
  # ---------------------------------------------------------------------------
  # Paths
  # ---------------------------------------------------------------------------
  
  hokkaido_dir <- here(
    "data", "interim", "geography",
    "nlni_n03_2025", "01_Hokkaido"
  )
  
  main_shp <- file.path(
    hokkaido_dir,
    "N03-20250101_01.shp"
  )
  
  subpref_shp <- file.path(
    hokkaido_dir,
    "N03-20250101_01_subprefecture.shp"
  )
  
  jp01_50_gpkg <- here(
    "data", "interim", "geography",
    "botanical_boundaries",
    "vpjd_japan_botanical_boundaries.gpkg"
  )
  
  output_dir <- here(
    "data", "interim", "geography",
    "hokkaido_star_geography"
  )
  
  table_dir <- here(
    "outputs", "tables", "geography",
    "hokkaido_star_geography"
  )
  
  output_gpkg <- file.path(
    output_dir,
    "vpjd_hokkaido_star_geography.gpkg"
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
  # Source validation
  # ---------------------------------------------------------------------------
  
  if (!file.exists(main_shp)) {
    stop(
      "Hokkaido main N03 shapefile not found: ",
      main_shp
    )
  }
  
  if (!file.exists(subpref_shp)) {
    stop(
      "Hokkaido subprefecture shapefile not found: ",
      subpref_shp
    )
  }
  
  if (!file.exists(jp01_50_gpkg)) {
    stop(
      "Geography 03e boundary GeoPackage not found: ",
      jp01_50_gpkg
    )
  }
  
  if (!file.exists(db_path)) {
    stop(
      "VPJD DuckDB not found: ",
      db_path
    )
  }
  
  cat("Hokkaido main source: PASS\n")
  cat("Hokkaido subprefecture source: PASS\n\n")
  
  # ---------------------------------------------------------------------------
  # Read source data
  # ---------------------------------------------------------------------------
  
  hokkaido_main <- st_read(
    main_shp,
    quiet = TRUE
  )
  
  hokkaido_subpref <- st_read(
    subpref_shp,
    quiet = TRUE
  )
  
  jp01_50 <- st_read(
    jp01_50_gpkg,
    layer = "botanical_areas_jp01_jp50",
    quiet = TRUE
  )
  
  cat(
    "Hokkaido main features: ",
    nrow(hokkaido_main),
    "\n",
    sep = ""
  )
  
  cat(
    "Hokkaido subprefecture features: ",
    nrow(hokkaido_subpref),
    "\n",
    sep = ""
  )
  
  cat(
    "Source CRS: ",
    st_crs(hokkaido_subpref)$input,
    "\n\n",
    sep = ""
  )
  
  if (nrow(hokkaido_main) != EXPECTED_MAIN_FEATURES) {
    stop(
      "Expected ",
      EXPECTED_MAIN_FEATURES,
      " Hokkaido main features; found ",
      nrow(hokkaido_main),
      "."
    )
  }
  
  if (nrow(hokkaido_subpref) != EXPECTED_SUBPREF_FEATURES) {
    stop(
      "Expected ",
      EXPECTED_SUBPREF_FEATURES,
      " Hokkaido subprefecture features; found ",
      nrow(hokkaido_subpref),
      "."
    )
  }
  
  # ---------------------------------------------------------------------------
  # Schema inspection
  # ---------------------------------------------------------------------------
  
  required_fields <- c(
    "N03_001",
    "N03_002"
  )
  
  missing_fields <- setdiff(
    required_fields,
    names(hokkaido_subpref)
  )
  
  if (length(missing_fields) > 0L) {
    stop(
      "Required N03 fields missing: ",
      paste(
        missing_fields,
        collapse = ", "
      )
    )
  }
  
  # N03_001 = prefecture
  # N03_002 = Hokkaido subprefecture / promotion bureau
  
  source_summary <- hokkaido_subpref %>%
    st_drop_geometry() %>%
    summarise(
      features = n(),
      prefectures = n_distinct(
        N03_001,
        na.rm = TRUE
      ),
      subprefectures = n_distinct(
        N03_002,
        na.rm = TRUE
      ),
      missing_subprefecture = sum(
        is.na(N03_002) |
          trimws(N03_002) == ""
      )
    )
  
  cat("— Source summary —\n")
  
  print.data.frame(
    as.data.frame(source_summary),
    row.names = FALSE
  )
  
  subpref_names <- hokkaido_subpref %>%
    st_drop_geometry() %>%
    filter(
      !is.na(N03_002),
      trimws(N03_002) != ""
    ) %>%
    count(
      subprefecture_name = N03_002,
      name = "source_components"
    ) %>%
    arrange(
      subprefecture_name
    )
  
  cat(
    "\n— Hokkaido subprefecture names —\n"
  )
  
  print.data.frame(
    as.data.frame(subpref_names),
    row.names = FALSE
  )
  
  if (
    nrow(subpref_names) !=
    EXPECTED_SUBPREFECTURES
  ) {
    stop(
      "Expected ",
      EXPECTED_SUBPREFECTURES,
      " Hokkaido subprefectures; found ",
      nrow(subpref_names),
      "."
    )
  }
  
  # ---------------------------------------------------------------------------
  # Build the 14 Star-geography units
  # ---------------------------------------------------------------------------
  
  cat(
    "\nBuilding 14 dissolved Hokkaido Star-geography units...\n"
  )
  
  hokkaido_units <- hokkaido_subpref %>%
    filter(
      !is.na(N03_002),
      trimws(N03_002) != ""
    ) %>%
    mutate(
      subprefecture_name =
        trimws(N03_002)
    ) %>%
    group_by(
      subprefecture_name
    ) %>%
    summarise(
      source_components = n(),
      geometry = st_union(geometry),
      .groups = "drop"
    ) %>%
    arrange(
      subprefecture_name
    )
  
  hokkaido_units <- hokkaido_units %>%
    mutate(
      hokkaido_unit_no = row_number(),
      hokkaido_unit_id = sprintf(
        "JP01-H%02d",
        hokkaido_unit_no
      ),
      parent_botanical_area_id = "JP01",
      parent_botanical_area_name = "Hokkaido",
      geography_role = "star_counting_unit",
      source_dataset = paste(
        "Japan MLIT/NLNI N03",
        "Administrative Areas 2025"
      ),
      source_layer =
        "N03-20250101_01_subprefecture",
      source_field = "N03_002",
      script_version = SCRIPT_VERSION
    ) %>%
    select(
      hokkaido_unit_id,
      hokkaido_unit_no,
      subprefecture_name,
      parent_botanical_area_id,
      parent_botanical_area_name,
      geography_role,
      source_components,
      source_dataset,
      source_layer,
      source_field,
      script_version,
      geometry
    )
  
  cat(
    "Dissolved units constructed: ",
    nrow(hokkaido_units),
    "\n",
    sep = ""
  )
  
  # ---------------------------------------------------------------------------
  # Geometry QA
  # ---------------------------------------------------------------------------
  
  valid <- st_is_valid(
    hokkaido_units
  )
  
  geometry_qa <- tibble(
    metric = c(
      "star_units",
      "unique_unit_ids",
      "unique_subprefecture_names",
      "valid_geometries",
      "invalid_geometries"
    ),
    value = c(
      nrow(hokkaido_units),
      n_distinct(
        hokkaido_units$hokkaido_unit_id
      ),
      n_distinct(
        hokkaido_units$subprefecture_name
      ),
      sum(valid),
      sum(!valid)
    )
  )
  
  cat(
    "\n— Geometry QA —\n"
  )
  
  print.data.frame(
    as.data.frame(geometry_qa),
    row.names = FALSE
  )
  
  if (any(!valid)) {
    stop(
      "Invalid Hokkaido subprefecture geometries detected."
    )
  }
  
  # ---------------------------------------------------------------------------
  # Lightweight JP01 reference validation
  #
  # IMPORTANT:
  # No intersection, difference, symmetric difference or clipping operation
  # is performed here.
  # ---------------------------------------------------------------------------
  
  area_id_field <- intersect(
    c(
      "botanical_area_id",
      "area_id"
    ),
    names(jp01_50)
  )
  
  if (length(area_id_field) != 1L) {
    stop(
      paste(
        "Could not uniquely identify",
        "JP01 area-ID field in Geography 03e."
      )
    )
  }
  
  jp01 <- jp01_50[
    jp01_50[[area_id_field]] == "JP01",
  ]
  
  if (nrow(jp01) != 1L) {
    stop(
      "Expected exactly one JP01 geometry; found ",
      nrow(jp01),
      "."
    )
  }
  
  jp01_valid <- all(
    st_is_valid(jp01)
  )
  
  jp01_crs_present <- !is.na(
    st_crs(jp01)
  )
  
  jp01_reference_qa <- tibble(
    metric = c(
      "jp01_reference_rows",
      "jp01_geometry_valid",
      "jp01_crs_present",
      "jp01_spatial_comparison_performed"
    ),
    value = c(
      as.character(nrow(jp01)),
      as.character(jp01_valid),
      as.character(jp01_crs_present),
      "FALSE"
    )
  )
  
  cat(
    "\n— JP01 reference QA —\n"
  )
  
  print.data.frame(
    as.data.frame(jp01_reference_qa),
    row.names = FALSE
  )
  
  # ---------------------------------------------------------------------------
  # Unit areas
  # ---------------------------------------------------------------------------
  
  cat(
    "\nCalculating Star-unit areas...\n"
  )
  
  hokkaido_units_eq <- st_transform(
    hokkaido_units,
    6933
  )
  
  unit_area_km2 <- as.numeric(
    st_area(
      hokkaido_units_eq
    )
  ) / 1e6
  
  unit_diagnostics <- hokkaido_units %>%
    mutate(
      geometry_valid =
        st_is_valid(.),
      area_km2 =
        unit_area_km2
    ) %>%
    st_drop_geometry() %>%
    select(
      hokkaido_unit_id,
      hokkaido_unit_no,
      subprefecture_name,
      source_components,
      geometry_valid,
      area_km2
    )
  
  cat(
    "\n— Hokkaido Star-geography units —\n"
  )
  
  print.data.frame(
    as.data.frame(unit_diagnostics),
    row.names = FALSE
  )
  
  # ---------------------------------------------------------------------------
  # Internal overlap QA
  #
  # Only 14 dissolved geometries are compared. This is substantially lighter
  # than the removed JP01 symmetric-difference calculation.
  # ---------------------------------------------------------------------------
  
  cat(
    "\nChecking internal Star-unit overlaps...\n"
  )
  
  intersections <- st_intersects(
    hokkaido_units_eq,
    sparse = TRUE
  )
  
  overlap_rows <- list()
  k <- 1L
  
  for (
    i in seq_len(
      nrow(hokkaido_units_eq)
    )
  ) {
    js <- intersections[[i]]
    js <- js[js > i]
    
    if (length(js) == 0L) {
      next
    }
    
    for (j in js) {
      inter <- suppressWarnings(
        st_intersection(
          st_geometry(
            hokkaido_units_eq
          )[i],
          st_geometry(
            hokkaido_units_eq
          )[j]
        )
      )
      
      area_m2 <- if (
        length(inter) == 0L
      ) {
        0
      } else {
        sum(
          as.numeric(
            st_area(inter)
          ),
          na.rm = TRUE
        )
      }
      
      if (area_m2 > 1) {
        overlap_rows[[k]] <- tibble(
          unit_1 =
            hokkaido_units_eq$
            hokkaido_unit_id[i],
          unit_2 =
            hokkaido_units_eq$
            hokkaido_unit_id[j],
          overlap_area_m2 =
            area_m2
        )
        
        k <- k + 1L
      }
    }
  }
  
  if (length(overlap_rows) == 0L) {
    positive_overlaps <- tibble(
      unit_1 = character(),
      unit_2 = character(),
      overlap_area_m2 = numeric()
    )
  } else {
    positive_overlaps <- bind_rows(
      overlap_rows
    )
  }
  
  cat(
    "Positive-area overlaps >1 m²: ",
    nrow(positive_overlaps),
    "\n",
    sep = ""
  )
  
  if (nrow(positive_overlaps) > 0L) {
    print.data.frame(
      as.data.frame(
        positive_overlaps
      ),
      row.names = FALSE
    )
  }
  
  # ---------------------------------------------------------------------------
  # Validation
  # ---------------------------------------------------------------------------
  
  validation <- tibble(
    check = c(
      "main_source_feature_count_9555",
      "subpref_source_feature_count_9376",
      "exactly_14_subprefectures",
      "14_unique_unit_ids",
      "14_unique_names",
      "all_geometries_valid",
      "no_positive_area_overlaps_gt_1m2",
      "jp01_reference_present",
      "jp01_reference_valid",
      "parent_area_is_jp01",
      "no_jp01_spatial_reconstruction",
      "no_jp51_boundary_modification",
      "no_occurrence_assignment",
      "no_coordinate_modification",
      "no_taxonomy_modification"
    ),
    pass = c(
      nrow(hokkaido_main) ==
        EXPECTED_MAIN_FEATURES,
      nrow(hokkaido_subpref) ==
        EXPECTED_SUBPREF_FEATURES,
      nrow(hokkaido_units) ==
        EXPECTED_SUBPREFECTURES,
      n_distinct(
        hokkaido_units$
          hokkaido_unit_id
      ) ==
        EXPECTED_SUBPREFECTURES,
      n_distinct(
        hokkaido_units$
          subprefecture_name
      ) ==
        EXPECTED_SUBPREFECTURES,
      all(
        st_is_valid(
          hokkaido_units
        )
      ),
      nrow(positive_overlaps) == 0L,
      nrow(jp01) == 1L,
      jp01_valid,
      all(
        hokkaido_units$
          parent_botanical_area_id ==
          "JP01"
      ),
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
    as.data.frame(validation),
    row.names = FALSE
  )
  
  cat(
    "\nAll validation checks PASS: ",
    all(validation$pass),
    "\n",
    sep = ""
  )
  
  if (!all(validation$pass)) {
    stop(
      paste(
        "Geography 03k validation failed.",
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
      "source_version",
      "main_source_shapefile",
      "subprefecture_source_shapefile",
      "source_main_features",
      "source_subprefecture_features",
      "star_units",
      "parent_botanical_area",
      "geography_role",
      "jp01_spatial_comparison",
      "jp01_modified",
      "jp51_modified",
      "occurrences_assigned",
      "coordinates_modified",
      "taxonomy_modified",
      "output_status"
    ),
    value = c(
      "03k_build_hokkaido_star_geography",
      SCRIPT_VERSION,
      SOURCE_VERSION,
      basename(main_shp),
      basename(subpref_shp),
      as.character(
        nrow(hokkaido_main)
      ),
      as.character(
        nrow(hokkaido_subpref)
      ),
      as.character(
        nrow(hokkaido_units)
      ),
      "JP01 Hokkaido",
      "Star-counting geography only",
      "REFERENCE CHECK ONLY",
      "FALSE",
      "FALSE",
      "FALSE",
      "FALSE",
      "FALSE",
      paste(
        "VALIDATED HOKKAIDO",
        "STAR-GEOGRAPHY REFERENCE LAYER"
      )
    )
  )
  
  # ---------------------------------------------------------------------------
  # Write GeoPackage
  # ---------------------------------------------------------------------------
  
  if (file.exists(output_gpkg)) {
    file.remove(output_gpkg)
  }
  
  st_write(
    hokkaido_units,
    output_gpkg,
    layer = "hokkaido_star_units",
    quiet = TRUE
  )
  
  st_write(
    jp01,
    output_gpkg,
    layer = "jp01_reference",
    quiet = TRUE
  )
  
  # ---------------------------------------------------------------------------
  # CSV outputs
  # ---------------------------------------------------------------------------
  
  write_csv(
    unit_diagnostics,
    file.path(
      table_dir,
      "hokkaido_star_units.csv"
    )
  )
  
  write_csv(
    source_summary,
    file.path(
      table_dir,
      "hokkaido_source_summary.csv"
    )
  )
  
  write_csv(
    subpref_names,
    file.path(
      table_dir,
      "hokkaido_subprefecture_names.csv"
    )
  )
  
  write_csv(
    geometry_qa,
    file.path(
      table_dir,
      "hokkaido_geometry_qa.csv"
    )
  )
  
  write_csv(
    jp01_reference_qa,
    file.path(
      table_dir,
      "hokkaido_jp01_reference_qa.csv"
    )
  )
  
  write_csv(
    positive_overlaps,
    file.path(
      table_dir,
      "hokkaido_star_unit_overlaps.csv"
    )
  )
  
  write_csv(
    validation,
    file.path(
      table_dir,
      "hokkaido_star_geography_validation.csv"
    )
  )
  
  write_csv(
    metadata,
    file.path(
      table_dir,
      "hokkaido_star_geography_metadata.csv"
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
    "japan_hokkaido_star_units",
    st_drop_geometry(
      hokkaido_units
    ),
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "japan_hokkaido_star_unit_summary",
    unit_diagnostics,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "japan_hokkaido_star_geometry_qa",
    geometry_qa,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "japan_hokkaido_star_jp01_reference_qa",
    jp01_reference_qa,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "japan_hokkaido_star_overlap_qa",
    positive_overlaps,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "japan_hokkaido_star_validation",
    validation,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "japan_hokkaido_star_metadata",
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
    "VALIDATED HOKKAIDO STAR-GEOGRAPHY REFERENCE LAYER\n",
    sep = ""
  )
  
  cat(
    "Hokkaido Star units: ",
    nrow(hokkaido_units),
    "\n",
    sep = ""
  )
  
  cat(
    "JP01 spatial reconstruction performed: FALSE\n"
  )
  
  cat(
    "JP51 boundary modified: FALSE\n"
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
    "\nGeography 03k v",
    SCRIPT_VERSION,
    " complete.\n",
    sep = ""
  )
  
  invisible(
    list(
      units =
        hokkaido_units,
      diagnostics =
        unit_diagnostics,
      geometry_qa =
        geometry_qa,
      jp01_reference_qa =
        jp01_reference_qa,
      overlaps =
        positive_overlaps,
      validation =
        validation,
      metadata =
        metadata
    )
  )
}

result_03k <-
  run_hokkaido_star_geography()