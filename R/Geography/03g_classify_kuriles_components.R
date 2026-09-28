# ==============================================================================
# VPJD-OJPCP
# 03g_classify_kuriles_components.R
# Version: 0.1.0
#
# Purpose:
#   Classify the physical land components identified by Geography 03f into
#   Kuriles candidates and neighbouring geographic context.
#
# Methodological position:
#   JP51 is a contemporary VPJD botanical-geographic unit.
#   This module classifies physical geometry only. It does not use sovereignty
#   or administrative jurisdiction as the basis of classification.
#
#   Particular attention is given to the southern end of the island chain,
#   where botanical treatment must distinguish Hokkaido context from the
#   Habomai/Shikotan/Kunashir/Iturup region.
#
# This module:
#   - validates frozen Geography 03f v0.1.1;
#   - reads the 03f diagnostic Natural Earth geometry;
#   - classifies obvious Hokkaido, Sakhalin and Kamchatka context;
#   - identifies the core Kuril-chain candidate components;
#   - retains the southern transition components for explicit review;
#   - DOES NOT construct/dissolve JP51;
#   - DOES NOT modify JP01-JP50;
#   - DOES NOT assign occurrences;
#   - DOES NOT modify coordinates or taxonomy.
#
# Required frozen input:
#   Geography 03f v0.1.1
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

SCRIPT_VERSION <- "0.1.0"
EXPECTED_GEOGRAPHY_03F_VERSION <- "0.1.1"

run_kuriles_classification <- function() {
  cat("\n— VPJD Kuriles component classification —\n\n")
  
  # ---------------------------------------------------------------------------
  # Database
  # ---------------------------------------------------------------------------
  
  db_path <- here(
    "data", "interim", "occurrences",
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
  # Validate Geography 03f
  # ---------------------------------------------------------------------------
  
  required_tables <- c(
    "japan_kuriles_source_components",
    "japan_kuriles_source_geometry_qa",
    "japan_kuriles_source_metadata"
  )
  
  missing_tables <- setdiff(
    required_tables,
    dbListTables(con)
  )
  
  if (length(missing_tables) > 0) {
    stop(
      "Required Geography 03f tables missing: ",
      paste(missing_tables, collapse = ", ")
    )
  }
  
  metadata_03f <- dbReadTable(
    con,
    "japan_kuriles_source_metadata"
  ) %>%
    as_tibble()
  
  if (!all(c("metric", "value") %in% names(metadata_03f))) {
    stop("Unexpected Geography 03f metadata schema.")
  }
  
  version_03f <- metadata_03f %>%
    filter(metric == "script_version") %>%
    pull(value)
  
  if (
    length(version_03f) != 1 ||
    version_03f != EXPECTED_GEOGRAPHY_03F_VERSION
  ) {
    stop(
      "Expected Geography 03f v",
      EXPECTED_GEOGRAPHY_03F_VERSION,
      "; found ",
      paste(version_03f, collapse = ", "),
      "."
    )
  }
  
  qa_03f <- dbReadTable(
    con,
    "japan_kuriles_source_geometry_qa"
  ) %>%
    as_tibble()
  
  invalid_03f <- qa_03f %>%
    filter(metric == "invalid_diagnostic_geometries") %>%
    pull(value)
  
  if (
    length(invalid_03f) != 1 ||
    as.numeric(invalid_03f) != 0
  ) {
    stop(
      "Geography 03f contains invalid diagnostic geometry."
    )
  }
  
  cat(
    "Geography 03f version: ",
    version_03f,
    "\n",
    "Geography 03f geometry validation: PASS\n\n",
    sep = ""
  )
  
  # ---------------------------------------------------------------------------
  # Read spatial diagnostic layer
  # ---------------------------------------------------------------------------
  
  gpkg_path <- here(
    "data", "interim", "geography", "kuriles",
    "vpjd_kuriles_source_diagnostic.gpkg"
  )
  
  if (!file.exists(gpkg_path)) {
    stop(
      "03f diagnostic GeoPackage not found: ",
      gpkg_path
    )
  }
  
  layers <- sf::st_layers(gpkg_path)$name
  
  if (!"diagnostic_land_components" %in% layers) {
    stop(
      "Layer 'diagnostic_land_components' not found."
    )
  }
  
  components <- sf::st_read(
    gpkg_path,
    layer = "diagnostic_land_components",
    quiet = TRUE
  )
  
  components <- sf::st_transform(
    components,
    4326
  )
  
  if (nrow(components) != 28) {
    stop(
      "Expected 28 Geography 03f components; found ",
      nrow(components),
      "."
    )
  }
  
  if (any(!sf::st_is_valid(components))) {
    stop(
      "One or more Geography 03f component geometries are invalid."
    )
  }
  
  required_fields <- c(
    "component_id",
    "point_lon",
    "point_lat",
    "xmin",
    "ymin",
    "xmax",
    "ymax",
    "area_km2"
  )
  
  missing_fields <- setdiff(
    required_fields,
    names(components)
  )
  
  if (length(missing_fields) > 0) {
    stop(
      "Required component fields missing: ",
      paste(missing_fields, collapse = ", ")
    )
  }
  
  # ---------------------------------------------------------------------------
  # Classification strategy
  #
  # These rules operate on physical position/geometry only.
  #
  # 1. Large clipped western/southern land masses are contextual:
  #       Hokkaido / Sakhalin.
  #
  # 2. Large northern mainland component is Kamchatka context.
  #
  # 3. The coherent NE-trending oceanic island chain is the Kuriles candidate.
  #
  # 4. Southern components below ~44.2 N and west of ~146.6 E are deliberately
  #    retained as SOUTHERN TRANSITION REVIEW rather than automatically placed
  #    in JP01 or JP51.
  #
  # The thresholds below classify the observed 03f component geometry.
  # They are NOT the final botanical boundary of JP51.
  # ---------------------------------------------------------------------------
  
  components <- components %>%
    mutate(
      component_class = case_when(
        
        # Large Hokkaido context clipped by the diagnostic window
        area_km2 > 10000 &
          point_lat < 45 ~
          "hokkaido_context",
        
        # Large Sakhalin context on western edge of window
        area_km2 > 10000 &
          point_lon < 145 &
          point_lat >= 45 ~
          "sakhalin_context",
        
        # Kamchatka mainland context
        area_km2 > 1000 &
          point_lon > 156 &
          point_lat > 50.8 ~
          "kamchatka_context",
        
        # Southern transition:
        # retain explicitly rather than making a botanical decision here
        point_lat < 44.2 &
          point_lon < 146.6 ~
          "southern_transition_review",
        
        # Physical Kuril-chain candidates
        point_lon >= 145.0 &
          point_lat >= 43.3 &
          point_lat <= 51.1 ~
          "kuriles_candidate",
        
        TRUE ~
          "other_context_review"
      )
    )
  
  # ---------------------------------------------------------------------------
  # Add broad chain-position labels to Kuriles candidates only
  # ---------------------------------------------------------------------------
  
  components <- components %>%
    mutate(
      chain_position = case_when(
        
        component_class !=
          "kuriles_candidate" ~
          NA_character_,
        
        point_lat < 45.0 ~
          "southern",
        
        point_lat < 47.0 ~
          "south_central",
        
        point_lat < 49.0 ~
          "central",
        
        TRUE ~
          "northern"
      )
    )
  
  # ---------------------------------------------------------------------------
  # Component table
  # ---------------------------------------------------------------------------
  
  classification_table <- components %>%
    sf::st_drop_geometry() %>%
    select(
      component_id,
      component_class,
      chain_position,
      point_lon,
      point_lat,
      area_km2,
      xmin,
      ymin,
      xmax,
      ymax
    ) %>%
    arrange(
      point_lat,
      point_lon
    ) %>%
    as_tibble()
  
  # ---------------------------------------------------------------------------
  # Classification summary
  # ---------------------------------------------------------------------------
  
  classification_summary <- classification_table %>%
    group_by(
      component_class
    ) %>%
    summarise(
      polygon_components = n(),
      total_area_km2 = sum(
        area_km2,
        na.rm = TRUE
      ),
      min_lon = min(
        xmin,
        na.rm = TRUE
      ),
      min_lat = min(
        ymin,
        na.rm = TRUE
      ),
      max_lon = max(
        xmax,
        na.rm = TRUE
      ),
      max_lat = max(
        ymax,
        na.rm = TRUE
      ),
      .groups = "drop"
    ) %>%
    arrange(
      min_lat,
      min_lon
    ) %>%
    as_tibble()
  
  chain_summary <- classification_table %>%
    filter(
      component_class ==
        "kuriles_candidate"
    ) %>%
    group_by(
      chain_position
    ) %>%
    summarise(
      polygon_components = n(),
      total_area_km2 = sum(
        area_km2,
        na.rm = TRUE
      ),
      min_lon = min(
        xmin,
        na.rm = TRUE
      ),
      min_lat = min(
        ymin,
        na.rm = TRUE
      ),
      max_lon = max(
        xmax,
        na.rm = TRUE
      ),
      max_lat = max(
        ymax,
        na.rm = TRUE
      ),
      .groups = "drop"
    ) %>%
    arrange(
      min_lat
    ) %>%
    as_tibble()
  
  # ---------------------------------------------------------------------------
  # Review table
  # ---------------------------------------------------------------------------
  
  review_table <- classification_table %>%
    filter(
      component_class %in% c(
        "southern_transition_review",
        "other_context_review"
      )
    ) %>%
    arrange(
      point_lat,
      point_lon
    ) %>%
    as_tibble()
  
  # ---------------------------------------------------------------------------
  # Geometry QA
  # ---------------------------------------------------------------------------
  
  geometry_qa <- tibble(
    metric = c(
      "input_components",
      "classified_components",
      "valid_geometries",
      "invalid_geometries",
      "kuriles_candidate_components",
      "southern_transition_review_components",
      "other_context_review_components",
      "unclassified_components"
    ),
    value = c(
      nrow(components),
      sum(
        !is.na(
          components$
            component_class
        )
      ),
      sum(
        sf::st_is_valid(
          components
        )
      ),
      sum(
        !sf::st_is_valid(
          components
        )
      ),
      sum(
        components$
          component_class ==
          "kuriles_candidate"
      ),
      sum(
        components$
          component_class ==
          "southern_transition_review"
      ),
      sum(
        components$
          component_class ==
          "other_context_review"
      ),
      sum(
        is.na(
          components$
            component_class
        )
      )
    )
  )
  
  # ---------------------------------------------------------------------------
  # Validation
  # ---------------------------------------------------------------------------
  
  validation <- tibble(
    check = c(
      "Exactly 28 source components",
      "All source components classified",
      "All geometries valid",
      "At least one Kuriles candidate",
      "Southern transition retained for review",
      "JP51 not yet constructed"
    ),
    pass = c(
      nrow(components) == 28,
      
      all(
        !is.na(
          components$
            component_class
        )
      ),
      
      all(
        sf::st_is_valid(
          components
        )
      ),
      
      any(
        components$
          component_class ==
          "kuriles_candidate"
      ),
      
      any(
        components$
          component_class ==
          "southern_transition_review"
      ),
      
      TRUE
    )
  )
  
  # ---------------------------------------------------------------------------
  # Persist analytical outputs
  # ---------------------------------------------------------------------------
  
  dbWriteTable(
    con,
    "japan_kuriles_component_classification",
    classification_table,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "japan_kuriles_component_classification_summary",
    classification_summary,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "japan_kuriles_chain_summary",
    chain_summary,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "japan_kuriles_component_review",
    review_table,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "japan_kuriles_component_geometry_qa",
    geometry_qa,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "japan_kuriles_component_validation",
    validation,
    overwrite = TRUE
  )
  
  # ---------------------------------------------------------------------------
  # Spatial output
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
  
  output_gpkg <- file.path(
    spatial_dir,
    "vpjd_kuriles_component_classification.gpkg"
  )
  
  if (file.exists(output_gpkg)) {
    file.remove(output_gpkg)
  }
  
  sf::st_write(
    components,
    output_gpkg,
    layer = "classified_components",
    quiet = TRUE
  )
  
  kuriles_candidates <- components %>%
    filter(
      component_class ==
        "kuriles_candidate"
    )
  
  sf::st_write(
    kuriles_candidates,
    output_gpkg,
    layer = "kuriles_candidates",
    append = TRUE,
    quiet = TRUE
  )
  
  transition_components <- components %>%
    filter(
      component_class ==
        "southern_transition_review"
    )
  
  sf::st_write(
    transition_components,
    output_gpkg,
    layer = "southern_transition_review",
    append = TRUE,
    quiet = TRUE
  )
  
  # ---------------------------------------------------------------------------
  # Metadata
  # ---------------------------------------------------------------------------
  
  metadata <- tibble(
    metric = c(
      "script_version",
      "input_geography_03f_version",
      "source_provider",
      "source_dataset",
      "source_version",
      "classification_basis",
      "classification_uses_sovereignty",
      "classification_is_final_botanical_assignment",
      "southern_transition_decision_finalised",
      "jp51_boundary_constructed",
      "jp01_jp50_modified",
      "occurrences_assigned",
      "coordinates_modified",
      "taxonomy_modified"
    ),
    value = c(
      SCRIPT_VERSION,
      version_03f,
      "Natural Earth",
      "10m Land",
      "5.1.1",
      paste(
        "Physical component position, extent and area;",
        "diagnostic classification only"
      ),
      "FALSE",
      "FALSE",
      "FALSE",
      "FALSE",
      "FALSE",
      "FALSE",
      "FALSE",
      "FALSE"
    )
  )
  
  dbWriteTable(
    con,
    "japan_kuriles_component_metadata",
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
    "kuriles_components"
  )
  
  dir.create(
    out_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  write_csv(
    classification_table,
    file.path(
      out_dir,
      "kuriles_component_classification.csv"
    )
  )
  
  write_csv(
    classification_summary,
    file.path(
      out_dir,
      "classification_summary.csv"
    )
  )
  
  write_csv(
    chain_summary,
    file.path(
      out_dir,
      "kuriles_chain_summary.csv"
    )
  )
  
  write_csv(
    review_table,
    file.path(
      out_dir,
      "component_review.csv"
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
    validation,
    file.path(
      out_dir,
      "validation.csv"
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
    "Input components: ",
    nrow(components),
    "\n\n",
    sep = ""
  )
  
  cat(
    "— Classification summary —\n\n"
  )
  
  print.data.frame(
    as.data.frame(
      classification_summary
    ),
    row.names = FALSE
  )
  
  cat(
    "\n— Kuriles candidate chain summary —\n\n"
  )
  
  print.data.frame(
    as.data.frame(
      chain_summary
    ),
    row.names = FALSE
  )
  
  cat(
    "\n— Kuriles candidate components —\n\n"
  )
  
  print.data.frame(
    as.data.frame(
      classification_table %>%
        filter(
          component_class ==
            "kuriles_candidate"
        ) %>%
        select(
          component_id,
          chain_position,
          point_lon,
          point_lat,
          area_km2,
          xmin,
          ymin,
          xmax,
          ymax
        )
    ),
    row.names = FALSE
  )
  
  cat(
    "\n— Components requiring review —\n\n"
  )
  
  if (nrow(review_table) == 0) {
    cat("None.\n")
  } else {
    print.data.frame(
      as.data.frame(
        review_table
      ),
      row.names = FALSE
    )
  }
  
  cat(
    "\n— Geometry QA —\n\n"
  )
  
  print.data.frame(
    as.data.frame(
      geometry_qa
    ),
    row.names = FALSE
  )
  
  cat(
    "\n— Validation —\n\n"
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
  
  cat(
    "\nSpatial classification written:\n",
    output_gpkg,
    "\n",
    sep = ""
  )
  
  cat("\n— Safety —\n")
  cat("JP51 boundary constructed: FALSE\n")
  cat("JP51 botanical definition finalised: FALSE\n")
  cat("Southern transition decision finalised: FALSE\n")
  cat("Sovereignty used for classification: FALSE\n")
  cat("JP01-JP50 modified: FALSE\n")
  cat("Occurrences assigned: FALSE\n")
  cat("Coordinates modified: FALSE\n")
  cat("Taxonomy modified: FALSE\n")
  cat(
    "Output status: KURILES COMPONENT CLASSIFICATION / DIAGNOSTIC ONLY\n"
  )
  
  cat(
    "\n03g_classify_kuriles_components.R v",
    SCRIPT_VERSION,
    " complete.\n",
    sep = ""
  )
  
  invisible(
    list(
      components =
        components,
      classification_table =
        classification_table,
      classification_summary =
        classification_summary,
      chain_summary =
        chain_summary,
      review_table =
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

geography_03g <-
  run_kuriles_classification()