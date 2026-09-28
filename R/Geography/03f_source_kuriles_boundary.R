# ==============================================================================
# VPJD-OJPCP
# 03f_source_kuriles_boundary.R
# Version: 0.1.1
#
# Purpose:
#   Acquire and inspect physical land geometry suitable for constructing the
#   contemporary VPJD botanical area JP51 Kuriles.
#
# Source:
#   Natural Earth 1:10m Physical Vectors
#   Land v5.1.1
#
# Methodological position:
#   JP51 will be a contemporary VPJD botanical-geographic unit.
#   Historical botanical geography, including Nakamura (2012), provides useful
#   precedent but does not independently define the contemporary VPJD boundary.
#
#   Natural Earth is used here solely as a physical coastline/island geometry
#   source. It is not used as a political authority and this module makes no
#   statement concerning sovereignty or administrative jurisdiction.
#
# This module:
#   - validates frozen Geography 03e v0.1.1;
#   - preserves the raw Natural Earth source archive;
#   - reads Natural Earth Land v5.1.1;
#   - extracts a deliberately broad Kuriles-region diagnostic window;
#   - separates land geometry into individual polygon components;
#   - records representative points, extents and areas;
#   - DOES NOT define the final JP51 boundary;
#   - DOES NOT modify JP01-JP50;
#   - DOES NOT assign occurrences;
#   - DOES NOT modify coordinates or taxonomy.
#
# Required frozen input:
#   Geography 03e v0.1.1
# ==============================================================================

suppressPackageStartupMessages({
  library(DBI)
  library(duckdb)
  library(dplyr)
  library(tibble)
  library(readr)
  library(here)
})

if (!requireNamespace("sf", quietly = TRUE)) {
  stop("Package 'sf' is required.")
}

SCRIPT_VERSION <- "0.1.1"
EXPECTED_GEOGRAPHY_03E_VERSION <- "0.1.1"

LAND_URL <- paste0(
  "https://naturalearth.s3.amazonaws.com/",
  "5.1.1/10m_physical/ne_10m_land.zip"
)

run_kuriles_source <- function() {
  
  cat("\n— VPJD Kuriles physical-boundary source inspection —\n\n")
  
  # ---------------------------------------------------------------------------
  # Database
  # ---------------------------------------------------------------------------
  
  db_path <- here(
    "data",
    "interim",
    "occurrences",
    "vpjd_occurrences.duckdb"
  )
  
  if (!file.exists(db_path)) {
    stop("DuckDB not found: ", db_path)
  }
  
  con <- dbConnect(
    duckdb::duckdb(),
    dbdir = db_path,
    read_only = FALSE
  )
  
  on.exit(
    dbDisconnect(con, shutdown = TRUE),
    add = TRUE
  )
  
  # ---------------------------------------------------------------------------
  # Validate frozen Geography 03e
  # ---------------------------------------------------------------------------
  
  required_tables <- c(
    "japan_botanical_boundary_metadata",
    "japan_botanical_boundary_validation"
  )
  
  missing_tables <- setdiff(
    required_tables,
    dbListTables(con)
  )
  
  if (length(missing_tables) > 0) {
    stop(
      "Required Geography 03e tables missing: ",
      paste(missing_tables, collapse = ", ")
    )
  }
  
  metadata_03e <- dbReadTable(
    con,
    "japan_botanical_boundary_metadata"
  ) %>%
    as_tibble()
  
  if (
    !all(
      c("metric", "value") %in%
      names(metadata_03e)
    )
  ) {
    stop(
      "Unexpected Geography 03e metadata schema."
    )
  }
  
  version_03e <- metadata_03e %>%
    filter(metric == "script_version") %>%
    pull(value)
  
  if (
    length(version_03e) != 1 ||
    version_03e != EXPECTED_GEOGRAPHY_03E_VERSION
  ) {
    stop(
      "Expected Geography 03e v",
      EXPECTED_GEOGRAPHY_03E_VERSION,
      "; found ",
      paste(version_03e, collapse = ", "),
      "."
    )
  }
  
  validation_03e <- dbReadTable(
    con,
    "japan_botanical_boundary_validation"
  ) %>%
    as_tibble()
  
  if (!"pass" %in% names(validation_03e)) {
    stop(
      "Geography 03e validation table lacks field 'pass'."
    )
  }
  
  if (!all(validation_03e$pass)) {
    stop(
      "Geography 03e validation is not fully PASS."
    )
  }
  
  cat(
    "Geography 03e version: ",
    version_03e,
    "\n",
    "Geography 03e validation: PASS\n\n",
    sep = ""
  )
  
  # ---------------------------------------------------------------------------
  # Source directories
  # ---------------------------------------------------------------------------
  
  raw_dir <- here(
    "data",
    "raw",
    "geography",
    "natural_earth_10m"
  )
  
  extract_dir <- here(
    "data",
    "interim",
    "geography",
    "natural_earth_10m",
    "land_v5.1.1"
  )
  
  dir.create(
    raw_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  dir.create(
    extract_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  land_zip <- file.path(
    raw_dir,
    "ne_10m_land_v5.1.1.zip"
  )
  
  # ---------------------------------------------------------------------------
  # Acquire source archive if absent
  # ---------------------------------------------------------------------------
  
  if (
    file.exists(land_zip) &&
    file.info(land_zip)$size > 0
  ) {
    
    cat(
      "Existing source retained: ",
      basename(land_zip),
      "\n",
      sep = ""
    )
    
  } else {
    
    cat(
      "Downloading: ",
      basename(land_zip),
      "\n",
      sep = ""
    )
    
    download.file(
      LAND_URL,
      destfile = land_zip,
      mode = "wb",
      quiet = FALSE
    )
    
    if (
      !file.exists(land_zip) ||
      file.info(land_zip)$size <= 0
    ) {
      stop(
        "Natural Earth Land download failed."
      )
    }
  }
  
  cat(
    "Source archive size: ",
    format(
      file.info(land_zip)$size,
      big.mark = ","
    ),
    " bytes\n\n",
    sep = ""
  )
  
  # ---------------------------------------------------------------------------
  # Validate ZIP
  # ---------------------------------------------------------------------------
  
  zip_contents <- tryCatch(
    unzip(
      land_zip,
      list = TRUE
    ),
    error = function(e) NULL
  )
  
  if (
    is.null(zip_contents) ||
    nrow(zip_contents) == 0
  ) {
    stop(
      "Natural Earth Land ZIP is invalid or unreadable."
    )
  }
  
  shp_entries <- zip_contents$Name[
    grepl(
      "\\.shp$",
      zip_contents$Name,
      ignore.case = TRUE
    )
  ]
  
  if (length(shp_entries) != 1) {
    stop(
      "Expected exactly one shapefile in Natural Earth Land archive; found ",
      length(shp_entries),
      "."
    )
  }
  
  cat(
    "ZIP validation: PASS\n",
    "Shapefile in archive: ",
    basename(shp_entries),
    "\n\n",
    sep = ""
  )
  
  # ---------------------------------------------------------------------------
  # Extract source
  # ---------------------------------------------------------------------------
  
  unzip(
    land_zip,
    exdir = extract_dir,
    overwrite = TRUE
  )
  
  land_shp <- list.files(
    extract_dir,
    pattern = "\\.shp$",
    full.names = TRUE,
    recursive = TRUE,
    ignore.case = TRUE
  )
  
  if (length(land_shp) != 1) {
    stop(
      "Expected one extracted Land shapefile; found ",
      length(land_shp),
      "."
    )
  }
  
  # ---------------------------------------------------------------------------
  # Read physical land geometry
  # ---------------------------------------------------------------------------
  
  cat(
    "Reading Natural Earth Land v5.1.1...\n\n"
  )
  
  land <- sf::st_read(
    land_shp,
    quiet = TRUE,
    stringsAsFactors = FALSE
  )
  
  if (is.na(sf::st_crs(land))) {
    stop(
      "Natural Earth Land source has no CRS."
    )
  }
  
  land <- sf::st_transform(
    land,
    4326
  )
  
  if (sf::st_crs(land)$epsg != 4326) {
    stop(
      "Natural Earth Land could not be transformed to EPSG:4326."
    )
  }
  
  land <- sf::st_make_valid(
    land
  )
  
  source_feature_count <- nrow(land)
  
  cat(
    "Natural Earth source features: ",
    format(
      source_feature_count,
      big.mark = ","
    ),
    "\n",
    "CRS after transformation: EPSG:4326\n\n",
    sep = ""
  )
  
  # ---------------------------------------------------------------------------
  # Broad diagnostic window
  #
  # IMPORTANT:
  # This window is NOT the definition of JP51.
  #
  # It intentionally includes geographical context around the Kuril chain so
  # that subsequent classification can distinguish the chain from neighbouring
  # Hokkaido and Kamchatka geometry.
  # ---------------------------------------------------------------------------
  
  diagnostic_bbox <- sf::st_bbox(
    c(
      xmin = 143.0,
      ymin = 42.5,
      xmax = 158.0,
      ymax = 51.5
    ),
    crs = sf::st_crs(4326)
  )
  
  diagnostic_window <- sf::st_as_sfc(
    diagnostic_bbox
  )
  
  cat(
    "Diagnostic window:\n",
    "  Longitude: 143.0 to 158.0 E\n",
    "  Latitude:   42.5 to 51.5 N\n",
    "  Status:     retrieval/inspection only\n\n",
    sep = ""
  )
  
  # ---------------------------------------------------------------------------
  # Select only source features intersecting window before clipping
  # ---------------------------------------------------------------------------
  
  candidate_index <- lengths(
    sf::st_intersects(
      land,
      diagnostic_window
    )
  ) > 0
  
  land_candidates <- land[
    candidate_index,
  ]
  
  if (nrow(land_candidates) == 0) {
    stop(
      "No Natural Earth land geometry intersects diagnostic window."
    )
  }
  
  cat(
    "Source features intersecting diagnostic window: ",
    nrow(land_candidates),
    "\n\n",
    sep = ""
  )
  
  # ---------------------------------------------------------------------------
  # Clip to diagnostic window
  # ---------------------------------------------------------------------------
  
  land_region <- suppressWarnings(
    sf::st_intersection(
      land_candidates,
      diagnostic_window
    )
  )
  
  land_region <- land_region[
    !sf::st_is_empty(land_region),
  ]
  
  if (nrow(land_region) == 0) {
    stop(
      "Diagnostic intersection produced no geometry."
    )
  }
  
  # ---------------------------------------------------------------------------
  # Retain polygonal geometry only
  # ---------------------------------------------------------------------------
  
  geometry_types <- as.character(
    sf::st_geometry_type(
      land_region
    )
  )
  
  polygon_mask <- geometry_types %in%
    c(
      "POLYGON",
      "MULTIPOLYGON",
      "GEOMETRYCOLLECTION"
    )
  
  land_region <- land_region[
    polygon_mask,
  ]
  
  if (nrow(land_region) == 0) {
    stop(
      "No polygonal geometry remains after diagnostic intersection."
    )
  }
  
  # ---------------------------------------------------------------------------
  # Extract polygon components
  # ---------------------------------------------------------------------------
  
  land_region <- suppressWarnings(
    sf::st_collection_extract(
      land_region,
      "POLYGON"
    )
  )
  
  components <- suppressWarnings(
    sf::st_cast(
      land_region,
      "POLYGON"
    )
  )
  
  components <- sf::st_make_valid(
    components
  )
  
  components <- components[
    !sf::st_is_empty(components),
  ]
  
  if (nrow(components) == 0) {
    stop(
      "No polygon components produced."
    )
  }
  
  # Strip inherited Natural Earth attributes.
  # At this stage only the physical geometry and explicit source provenance are
  # required.
  
  components <- sf::st_sf(
    source_layer =
      rep(
        "Natural Earth 10m Land",
        nrow(components)
      ),
    source_version =
      rep(
        "5.1.1",
        nrow(components)
      ),
    geometry =
      sf::st_geometry(components),
    crs =
      sf::st_crs(components)
  )
  
  components$component_id <- sprintf(
    "KUR_DIAG_%05d",
    seq_len(nrow(components))
  )
  
  # ---------------------------------------------------------------------------
  # Representative points
  # ---------------------------------------------------------------------------
  
  representative_points <- suppressWarnings(
    sf::st_point_on_surface(
      components
    )
  )
  
  xy <- sf::st_coordinates(
    representative_points
  )
  
  components$point_lon <- xy[, 1]
  components$point_lat <- xy[, 2]
  
  # ---------------------------------------------------------------------------
  # Component bounding boxes
  # ---------------------------------------------------------------------------
  
  component_bbox <- bind_rows(
    lapply(
      seq_len(nrow(components)),
      function(i) {
        
        b <- sf::st_bbox(
          components[i, ]
        )
        
        tibble(
          xmin =
            as.numeric(
              b["xmin"]
            ),
          ymin =
            as.numeric(
              b["ymin"]
            ),
          xmax =
            as.numeric(
              b["xmax"]
            ),
          ymax =
            as.numeric(
              b["ymax"]
            )
        )
      }
    )
  )
  
  components$xmin <-
    component_bbox$xmin
  
  components$ymin <-
    component_bbox$ymin
  
  components$xmax <-
    component_bbox$xmax
  
  components$ymax <-
    component_bbox$ymax
  
  # ---------------------------------------------------------------------------
  # Component areas
  # ---------------------------------------------------------------------------
  
  components_equal_area <- sf::st_transform(
    components,
    6933
  )
  
  components$area_km2 <-
    as.numeric(
      sf::st_area(
        components_equal_area
      )
    ) / 1e6
  
  # ---------------------------------------------------------------------------
  # Broad diagnostic zones
  #
  # These are inspection labels only.
  # They are NOT botanical classifications and are NOT JP51 assignments.
  # ---------------------------------------------------------------------------
  
  components <- components %>%
    mutate(
      diagnostic_zone = case_when(
        
        point_lat < 44.0 ~
          "southern_context",
        
        point_lat >= 44.0 &
          point_lat < 46.0 ~
          "southern_chain_context",
        
        point_lat >= 46.0 &
          point_lat < 49.0 ~
          "central_chain_context",
        
        point_lat >= 49.0 &
          point_lat <= 51.5 ~
          "northern_chain_context",
        
        TRUE ~
          "review"
      )
    )
  
  # ---------------------------------------------------------------------------
  # Analytical component table
  # ---------------------------------------------------------------------------
  
  component_table <- components %>%
    sf::st_drop_geometry() %>%
    select(
      component_id,
      source_layer,
      source_version,
      diagnostic_zone,
      point_lon,
      point_lat,
      xmin,
      ymin,
      xmax,
      ymax,
      area_km2
    ) %>%
    arrange(
      point_lat,
      point_lon
    ) %>%
    as_tibble()
  
  # ---------------------------------------------------------------------------
  # Diagnostic summaries
  # ---------------------------------------------------------------------------
  
  zone_summary <- component_table %>%
    group_by(
      diagnostic_zone
    ) %>%
    summarise(
      polygon_components = n(),
      total_area_km2 =
        sum(
          area_km2,
          na.rm = TRUE
        ),
      min_lon =
        min(
          xmin,
          na.rm = TRUE
        ),
      min_lat =
        min(
          ymin,
          na.rm = TRUE
        ),
      max_lon =
        max(
          xmax,
          na.rm = TRUE
        ),
      max_lat =
        max(
          ymax,
          na.rm = TRUE
        ),
      .groups = "drop"
    ) %>%
    arrange(
      min_lat
    ) %>%
    as_tibble()
  
  geometry_valid <- sf::st_is_valid(
    components
  )
  
  geometry_qa <- tibble(
    metric = c(
      "source_land_features",
      "source_features_intersecting_window",
      "diagnostic_polygon_components",
      "valid_diagnostic_geometries",
      "invalid_diagnostic_geometries",
      "review_zone_components",
      "components_under_2_km2",
      "components_2_km2_or_larger"
    ),
    value = c(
      source_feature_count,
      nrow(land_candidates),
      nrow(components),
      sum(geometry_valid),
      sum(!geometry_valid),
      sum(
        components$
          diagnostic_zone ==
          "review"
      ),
      sum(
        components$
          area_km2 < 2
      ),
      sum(
        components$
          area_km2 >= 2
      )
    )
  )
  
  # ---------------------------------------------------------------------------
  # Source adequacy diagnostics
  #
  # We deliberately report small components rather than assuming the separate
  # Natural Earth Minor Islands layer is required. This allows the actual Land
  # v5.1.1 representation to be evaluated first.
  # ---------------------------------------------------------------------------
  
  size_summary <- component_table %>%
    mutate(
      size_class = case_when(
        area_km2 < 0.1 ~
          "<0.1 km2",
        area_km2 < 0.5 ~
          "0.1-<0.5 km2",
        area_km2 < 1 ~
          "0.5-<1 km2",
        area_km2 < 2 ~
          "1-<2 km2",
        area_km2 < 10 ~
          "2-<10 km2",
        area_km2 < 100 ~
          "10-<100 km2",
        area_km2 < 1000 ~
          "100-<1000 km2",
        TRUE ~
          ">=1000 km2"
      )
    ) %>%
    count(
      size_class,
      name = "polygon_components"
    ) %>%
    mutate(
      size_class = factor(
        size_class,
        levels = c(
          "<0.1 km2",
          "0.1-<0.5 km2",
          "0.5-<1 km2",
          "1-<2 km2",
          "2-<10 km2",
          "10-<100 km2",
          "100-<1000 km2",
          ">=1000 km2"
        )
      )
    ) %>%
    arrange(
      size_class
    ) %>%
    mutate(
      size_class =
        as.character(
          size_class
        )
    ) %>%
    as_tibble()
  
  # ---------------------------------------------------------------------------
  # Persist analytical outputs
  # ---------------------------------------------------------------------------
  
  dbWriteTable(
    con,
    "japan_kuriles_source_components",
    component_table,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "japan_kuriles_source_zone_summary",
    zone_summary,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "japan_kuriles_source_size_summary",
    size_summary,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "japan_kuriles_source_geometry_qa",
    geometry_qa,
    overwrite = TRUE
  )
  
  # ---------------------------------------------------------------------------
  # Spatial diagnostic output
  # ---------------------------------------------------------------------------
  
  spatial_dir <- here(
    "data",
    "interim",
    "geography",
    "kuriles"
  )
  
  dir.create(
    spatial_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  gpkg_path <- file.path(
    spatial_dir,
    "vpjd_kuriles_source_diagnostic.gpkg"
  )
  
  if (file.exists(gpkg_path)) {
    file.remove(gpkg_path)
  }
  
  sf::st_write(
    components,
    gpkg_path,
    layer = "diagnostic_land_components",
    quiet = TRUE
  )
  
  diagnostic_window_sf <- sf::st_sf(
    diagnostic_role =
      "retrieval_and_inspection_only",
    geometry =
      diagnostic_window
  )
  
  sf::st_write(
    diagnostic_window_sf,
    gpkg_path,
    layer = "diagnostic_window",
    append = TRUE,
    quiet = TRUE
  )
  
  # ---------------------------------------------------------------------------
  # Metadata
  # ---------------------------------------------------------------------------
  
  metadata <- tibble(
    metric = c(
      "script_version",
      "input_geography_03e_version",
      "source_provider",
      "source_dataset",
      "source_version",
      "source_scale",
      "source_geometry_role",
      "source_url",
      "diagnostic_xmin",
      "diagnostic_ymin",
      "diagnostic_xmax",
      "diagnostic_ymax",
      "diagnostic_window_defines_jp51",
      "jp51_boundary_constructed",
      "jp51_botanical_definition_finalised",
      "political_boundary_used_to_define_jp51",
      "minor_islands_supplement_used",
      "jp01_jp50_modified",
      "occurrences_assigned",
      "coordinates_modified",
      "taxonomy_modified",
      "historical_framework_role"
    ),
    value = c(
      SCRIPT_VERSION,
      version_03e,
      "Natural Earth",
      "10m Land",
      "5.1.1",
      "1:10m",
      "physical coastline/island geometry only",
      LAND_URL,
      "143.0",
      "42.5",
      "158.0",
      "51.5",
      "FALSE",
      "FALSE",
      "FALSE",
      "FALSE",
      "FALSE",
      "FALSE",
      "FALSE",
      "FALSE",
      "FALSE",
      paste(
        "Historical botanical geography provides precedent and",
        "comparison only; it does not independently define the",
        "contemporary VPJD JP51 boundary."
      )
    )
  )
  
  dbWriteTable(
    con,
    "japan_kuriles_source_metadata",
    metadata,
    overwrite = TRUE
  )
  
  # ---------------------------------------------------------------------------
  # CSV outputs
  # ---------------------------------------------------------------------------
  
  out_dir <- here(
    "outputs",
    "tables",
    "geography",
    "kuriles_source"
  )
  
  dir.create(
    out_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  write_csv(
    component_table,
    file.path(
      out_dir,
      "kuriles_diagnostic_components.csv"
    )
  )
  
  write_csv(
    zone_summary,
    file.path(
      out_dir,
      "kuriles_diagnostic_zone_summary.csv"
    )
  )
  
  write_csv(
    size_summary,
    file.path(
      out_dir,
      "kuriles_diagnostic_size_summary.csv"
    )
  )
  
  write_csv(
    geometry_qa,
    file.path(
      out_dir,
      "geometry_qa.csv"
    )
  )
  
  write_csv(
    metadata,
    file.path(
      out_dir,
      "metadata.csv"
    )
  )
  
  # ---------------------------------------------------------------------------
  # Console report
  # ---------------------------------------------------------------------------
  
  cat(
    "— Source acquisition —\n\n"
  )
  
  cat(
    "Provider: Natural Earth\n",
    "Dataset: 10m Land\n",
    "Version: 5.1.1\n",
    "Archive: ",
    basename(land_zip),
    "\n",
    "Archive size: ",
    format(
      file.info(land_zip)$size,
      big.mark = ","
    ),
    " bytes\n\n",
    sep = ""
  )
  
  cat(
    "— Diagnostic geometry QA —\n\n"
  )
  
  print.data.frame(
    as.data.frame(
      geometry_qa
    ),
    row.names = FALSE
  )
  
  cat(
    "\n— Diagnostic zone summary —\n\n"
  )
  
  print.data.frame(
    as.data.frame(
      zone_summary
    ),
    row.names = FALSE
  )
  
  cat(
    "\n— Component size summary —\n\n"
  )
  
  print.data.frame(
    as.data.frame(
      size_summary
    ),
    row.names = FALSE
  )
  
  cat(
    "\n— Largest diagnostic components —\n\n"
  )
  
  print.data.frame(
    as.data.frame(
      component_table %>%
        arrange(
          desc(area_km2)
        ) %>%
        select(
          component_id,
          diagnostic_zone,
          point_lon,
          point_lat,
          area_km2,
          xmin,
          ymin,
          xmax,
          ymax
        ) %>%
        head(30)
    ),
    row.names = FALSE
  )
  
  cat(
    "\nSpatial diagnostic written:\n",
    gpkg_path,
    "\n",
    sep = ""
  )
  
  cat("\n— Safety —\n")
  cat("JP51 boundary constructed: FALSE\n")
  cat("JP51 botanical definition finalised: FALSE\n")
  cat("Diagnostic window defines JP51: FALSE\n")
  cat("Political boundary used to define JP51: FALSE\n")
  cat("Minor-islands supplement used: FALSE\n")
  cat("JP01-JP50 modified: FALSE\n")
  cat("Occurrences assigned: FALSE\n")
  cat("Coordinates modified: FALSE\n")
  cat("Taxonomy modified: FALSE\n")
  cat(
    "Output status: KURILES PHYSICAL-GEOMETRY SOURCE / DIAGNOSTIC ONLY\n"
  )
  
  cat(
    "\n03f_source_kuriles_boundary.R v",
    SCRIPT_VERSION,
    " complete.\n",
    sep = ""
  )
  
  invisible(
    list(
      components =
        components,
      component_table =
        component_table,
      zone_summary =
        zone_summary,
      size_summary =
        size_summary,
      geometry_qa =
        geometry_qa,
      metadata =
        metadata
    )
  )
}

geography_03f <-
  run_kuriles_source()