# ==============================================================================
# VPJD-OJPCP
# 03d_validate_ogasawara_kazan_scope.R
# Version: 0.1.0
#
# Purpose:
#   Validate the spatial scope of the provisional JP49 Ogasawara and
#   JP50 Kazan component classifications produced by Geography 03c.
#
# Problem:
#   Ogasawara Village contains geographically separated island groups.
#   A simple latitude rule can correctly separate Ogasawara from Kazan, but
#   Tokyo also includes remote islands such as Okinotorishima and
#   Minamitorishima. These must not silently become botanical "Kazan".
#
# Strategy:
#   - read the frozen 03c Tokyo component layer;
#   - retain provisional JP49/JP50 classifications unchanged;
#   - classify remote components into spatial diagnostic clusters;
#   - distinguish Kazan proper from remote outliers;
#   - expose all results for QA;
#   - make NO final botanical-boundary decision.
#
# Reference spatial centres used only for diagnostic clustering:
#   Ogasawara proper: ~27.0 N, 142.2 E
#   Kazan proper:     ~24.8 N, 141.3 E
#   Okinotorishima:   ~20.4 N, 136.1 E
#   Minamitorishima:  ~24.3 N, 154.0 E
#
# Required frozen inputs:
#   Geography 02  v0.1.0
#   Geography 03a v0.1.0
#   Geography 03b v0.1.1
#   Geography 03c v0.1.0
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
EXPECTED_GEOGRAPHY_02_VERSION <- "0.1.0"
EXPECTED_GEOGRAPHY_03A_VERSION <- "0.1.0"
EXPECTED_GEOGRAPHY_03B_VERSION <- "0.1.1"
EXPECTED_GEOGRAPHY_03C_VERSION <- "0.1.0"

get_metadata_value <- function(con, table_name, metric_name) {
  fields <- dbListFields(con, table_name)
  
  if (!all(c("metric", "value") %in% fields)) {
    stop("Expected metric/value metadata table: ", table_name)
  }
  
  sql <- paste0(
    "SELECT CAST(value AS VARCHAR) AS value FROM ",
    dbQuoteIdentifier(con, table_name),
    " WHERE CAST(metric AS VARCHAR) = ?"
  )
  
  x <- dbGetQuery(
    con,
    sql,
    params = list(metric_name)
  )
  
  if (nrow(x) != 1) {
    stop(
      "Expected one metadata row for ",
      metric_name, " in ", table_name, "."
    )
  }
  
  as.character(x$value[[1]])
}

run_ogasawara_kazan_validation <- function() {
  
  cat("\n— VPJD Ogasawara / Kazan scope validation —\n\n")
  
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
  
  required_tables <- c(
    "japan_botanical_framework_metadata",
    "japan_nlni_n03_acquisition_metadata",
    "japan_nlni_n03_inspection_metadata",
    "japan_nlni_special_component_metadata"
  )
  
  missing_tables <- setdiff(
    required_tables,
    dbListTables(con)
  )
  
  if (length(missing_tables) > 0) {
    stop(
      "Required tables missing: ",
      paste(missing_tables, collapse = ", ")
    )
  }
  
  g02 <- get_metadata_value(
    con,
    "japan_botanical_framework_metadata",
    "script_version"
  )
  
  g03a <- get_metadata_value(
    con,
    "japan_nlni_n03_acquisition_metadata",
    "script_version"
  )
  
  g03b <- get_metadata_value(
    con,
    "japan_nlni_n03_inspection_metadata",
    "script_version"
  )
  
  g03c <- get_metadata_value(
    con,
    "japan_nlni_special_component_metadata",
    "script_version"
  )
  
  expected <- c(
    g02 = EXPECTED_GEOGRAPHY_02_VERSION,
    g03a = EXPECTED_GEOGRAPHY_03A_VERSION,
    g03b = EXPECTED_GEOGRAPHY_03B_VERSION,
    g03c = EXPECTED_GEOGRAPHY_03C_VERSION
  )
  
  observed <- c(
    g02 = g02,
    g03a = g03a,
    g03b = g03b,
    g03c = g03c
  )
  
  if (!all(observed == expected)) {
    stop(
      "Unexpected upstream versions. Observed: ",
      paste(
        names(observed),
        observed,
        collapse = "; "
      )
    )
  }
  
  cat(
    "Geography 02 version: ", g02, "\n",
    "Geography 03a version: ", g03a, "\n",
    "Geography 03b version: ", g03b, "\n",
    "Geography 03c version: ", g03c, "\n\n",
    sep = ""
  )
  
  # ---------------------------------------------------------------------------
  # Read frozen 03c spatial output
  # ---------------------------------------------------------------------------
  
  gpkg_path <- here(
    "data",
    "interim",
    "geography",
    "botanical_components",
    "vpjd_special_island_components.gpkg"
  )
  
  if (!file.exists(gpkg_path)) {
    stop(
      "03c GeoPackage not found: ",
      gpkg_path
    )
  }
  
  tokyo <- sf::st_read(
    gpkg_path,
    layer = "tokyo_components",
    quiet = TRUE
  )
  
  if (sf::st_crs(tokyo)$epsg != 4326) {
    tokyo <- sf::st_transform(
      tokyo,
      4326
    )
  }
  
  required_fields <- c(
    "source_feature_id",
    "N03_001",
    "N03_004",
    "N03_007",
    "point_lon",
    "point_lat",
    "xmin",
    "ymin",
    "xmax",
    "ymax",
    "component_class",
    "target_area_id"
  )
  
  missing_fields <- setdiff(
    required_fields,
    names(tokyo)
  )
  
  if (length(missing_fields) > 0) {
    stop(
      "Required 03c fields missing: ",
      paste(missing_fields, collapse = ", ")
    )
  }
  
  remote <- tokyo %>%
    filter(
      target_area_id %in%
        c("JP49", "JP50")
    )
  
  if (nrow(remote) == 0) {
    stop("No provisional JP49/JP50 components found.")
  }
  
  cat(
    "Provisional JP49/JP50 polygon features: ",
    format(nrow(remote), big.mark = ","),
    "\n\n",
    sep = ""
  )
  
  # ---------------------------------------------------------------------------
  # Diagnostic spatial clusters
  #
  # These deliberately use broad, non-overlapping windows around the four
  # geographically distinct remote-island groups visible in N03.
  #
  # They are QA labels, not final botanical assignments.
  # ---------------------------------------------------------------------------
  
  remote <- remote %>%
    mutate(
      diagnostic_cluster = case_when(
        
        # Okinotorishima:
        # isolated far to the SW around 20.4 N / 136.1 E
        point_lat < 22 &
          point_lon < 138 ~
          "Okinotorishima_remote",
        
        # Minamitorishima:
        # isolated far to the east around 24.3 N / 154 E
        point_lon > 150 ~
          "Minamitorishima_remote",
        
        # Kazan / Volcano Islands proper:
        # south of Ogasawara proper, around 24-25.5 N / 141-142 E
        point_lat < 26 &
          point_lon >= 139 &
          point_lon <= 145 ~
          "Kazan_proper",
        
        # Ogasawara / Bonin groups proper
        point_lat >= 26 &
          point_lat < 30 &
          point_lon >= 140 &
          point_lon <= 145 ~
          "Ogasawara_proper",
        
        TRUE ~
          "review_other_remote"
      ),
      
      provisional_consistency = case_when(
        target_area_id == "JP49" &
          diagnostic_cluster ==
          "Ogasawara_proper" ~
          "consistent",
        
        target_area_id == "JP50" &
          diagnostic_cluster ==
          "Kazan_proper" ~
          "consistent",
        
        target_area_id == "JP50" &
          diagnostic_cluster %in% c(
            "Okinotorishima_remote",
            "Minamitorishima_remote"
          ) ~
          "JP50_remote_outlier",
        
        TRUE ~
          "review"
      )
    )
  
  # ---------------------------------------------------------------------------
  # Component table
  # ---------------------------------------------------------------------------
  
  remote_table <- remote %>%
    sf::st_drop_geometry() %>%
    transmute(
      source_feature_id =
        source_feature_id,
      municipality_jp =
        as.character(N03_004),
      admin_code =
        as.character(N03_007),
      provisional_component_class =
        component_class,
      provisional_target_area_id =
        target_area_id,
      point_lon =
        point_lon,
      point_lat =
        point_lat,
      xmin =
        xmin,
      ymin =
        ymin,
      xmax =
        xmax,
      ymax =
        ymax,
      diagnostic_cluster =
        diagnostic_cluster,
      provisional_consistency =
        provisional_consistency
    ) %>%
    as_tibble()
  
  # ---------------------------------------------------------------------------
  # Summary
  # ---------------------------------------------------------------------------
  
  cluster_summary <- remote_table %>%
    count(
      provisional_target_area_id,
      diagnostic_cluster,
      provisional_consistency,
      name = "polygon_features"
    ) %>%
    arrange(
      provisional_target_area_id,
      diagnostic_cluster
    ) %>%
    as_tibble()
  
  cluster_extents <- remote_table %>%
    group_by(
      diagnostic_cluster
    ) %>%
    summarise(
      polygon_features = n(),
      municipalities =
        n_distinct(municipality_jp),
      xmin = min(xmin, na.rm = TRUE),
      ymin = min(ymin, na.rm = TRUE),
      xmax = max(xmax, na.rm = TRUE),
      ymax = max(ymax, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(
      ymin,
      xmin
    ) %>%
    as_tibble()
  
  outliers <- remote_table %>%
    filter(
      provisional_consistency !=
        "consistent"
    ) %>%
    arrange(
      diagnostic_cluster,
      point_lat,
      point_lon
    ) %>%
    as_tibble()
  
  # ---------------------------------------------------------------------------
  # Municipality × cluster profile
  # ---------------------------------------------------------------------------
  
  municipality_cluster_summary <- remote_table %>%
    count(
      municipality_jp,
      admin_code,
      diagnostic_cluster,
      provisional_target_area_id,
      name = "polygon_features"
    ) %>%
    arrange(
      municipality_jp,
      diagnostic_cluster
    ) %>%
    as_tibble()
  
  # ---------------------------------------------------------------------------
  # Geometry QA
  # ---------------------------------------------------------------------------
  
  geometry_qa <- tibble(
    metric = c(
      "remote_polygon_features",
      "valid_geometries",
      "invalid_geometries",
      "ogasawara_proper_features",
      "kazan_proper_features",
      "okinotorishima_remote_features",
      "minamitorishima_remote_features",
      "review_other_remote_features",
      "provisional_consistent_features",
      "provisional_outlier_or_review_features"
    ),
    value = c(
      nrow(remote),
      sum(sf::st_is_valid(remote)),
      sum(!sf::st_is_valid(remote)),
      sum(
        remote$diagnostic_cluster ==
          "Ogasawara_proper"
      ),
      sum(
        remote$diagnostic_cluster ==
          "Kazan_proper"
      ),
      sum(
        remote$diagnostic_cluster ==
          "Okinotorishima_remote"
      ),
      sum(
        remote$diagnostic_cluster ==
          "Minamitorishima_remote"
      ),
      sum(
        remote$diagnostic_cluster ==
          "review_other_remote"
      ),
      sum(
        remote$provisional_consistency ==
          "consistent"
      ),
      sum(
        remote$provisional_consistency !=
          "consistent"
      )
    )
  )
  
  # ---------------------------------------------------------------------------
  # Persist diagnostic tables
  # ---------------------------------------------------------------------------
  
  dbWriteTable(
    con,
    "japan_ogasawara_kazan_component_validation",
    remote_table,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "japan_ogasawara_kazan_cluster_summary",
    cluster_summary,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "japan_ogasawara_kazan_cluster_extents",
    cluster_extents,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "japan_ogasawara_kazan_outliers",
    outliers,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "japan_ogasawara_kazan_municipality_clusters",
    municipality_cluster_summary,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "japan_ogasawara_kazan_geometry_qa",
    geometry_qa,
    overwrite = TRUE
  )
  
  # ---------------------------------------------------------------------------
  # Spatial QA GeoPackage
  # ---------------------------------------------------------------------------
  
  spatial_dir <- here(
    "data",
    "interim",
    "geography",
    "botanical_components"
  )
  
  out_gpkg <- file.path(
    spatial_dir,
    "vpjd_ogasawara_kazan_validation.gpkg"
  )
  
  if (file.exists(out_gpkg)) {
    file.remove(out_gpkg)
  }
  
  sf::st_write(
    remote,
    out_gpkg,
    layer = "remote_tokyo_components",
    quiet = TRUE
  )
  
  # ---------------------------------------------------------------------------
  # Metadata
  # ---------------------------------------------------------------------------
  
  metadata <- tibble(
    metric = c(
      "script_version",
      "input_geography_02_version",
      "input_geography_03a_version",
      "input_geography_03b_version",
      "input_geography_03c_version",
      "validation_scope",
      "classification_modified",
      "final_botanical_boundaries_created",
      "kuriles_constructed",
      "occurrences_assigned",
      "coordinates_modified",
      "taxonomy_modified",
      "upstream_tables_modified"
    ),
    value = c(
      SCRIPT_VERSION,
      g02,
      g03a,
      g03b,
      g03c,
      "provisional JP49 and JP50 N03 polygon components",
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
    "japan_ogasawara_kazan_validation_metadata",
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
    "ogasawara_kazan_validation"
  )
  
  dir.create(
    out_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  write_csv(
    remote_table,
    file.path(
      out_dir,
      "component_validation.csv"
    )
  )
  
  write_csv(
    cluster_summary,
    file.path(
      out_dir,
      "cluster_summary.csv"
    )
  )
  
  write_csv(
    cluster_extents,
    file.path(
      out_dir,
      "cluster_extents.csv"
    )
  )
  
  write_csv(
    outliers,
    file.path(
      out_dir,
      "provisional_outliers.csv"
    )
  )
  
  write_csv(
    municipality_cluster_summary,
    file.path(
      out_dir,
      "municipality_cluster_summary.csv"
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
  
  cat("— Diagnostic cluster summary —\n\n")
  
  print.data.frame(
    as.data.frame(cluster_summary),
    row.names = FALSE
  )
  
  cat("\n— Diagnostic cluster extents —\n\n")
  
  print.data.frame(
    as.data.frame(cluster_extents),
    row.names = FALSE
  )
  
  cat("\n— Municipality × cluster profile —\n\n")
  
  print.data.frame(
    as.data.frame(
      municipality_cluster_summary
    ),
    row.names = FALSE
  )
  
  cat("\n— Geometry / classification QA —\n\n")
  
  print.data.frame(
    as.data.frame(geometry_qa),
    row.names = FALSE
  )
  
  cat("\n— Provisional JP49/JP50 outliers —\n\n")
  
  if (nrow(outliers) == 0) {
    cat("None.\n")
  } else {
    print.data.frame(
      as.data.frame(outliers),
      row.names = FALSE
    )
  }
  
  cat("\n— Validation —\n")
  
  cat(
    "Ogasawara proper detected: ",
    any(
      remote$diagnostic_cluster ==
        "Ogasawara_proper"
    ),
    "\n",
    sep = ""
  )
  
  cat(
    "Kazan proper detected: ",
    any(
      remote$diagnostic_cluster ==
        "Kazan_proper"
    ),
    "\n",
    sep = ""
  )
  
  cat(
    "Okinotorishima remote cluster detected: ",
    any(
      remote$diagnostic_cluster ==
        "Okinotorishima_remote"
    ),
    "\n",
    sep = ""
  )
  
  cat(
    "Minamitorishima remote cluster detected: ",
    any(
      remote$diagnostic_cluster ==
        "Minamitorishima_remote"
    ),
    "\n",
    sep = ""
  )
  
  cat(
    "Other remote components requiring review: ",
    sum(
      remote$diagnostic_cluster ==
        "review_other_remote"
    ),
    "\n",
    sep = ""
  )
  
  cat("\n— Safety —\n")
  cat("03c classifications modified: FALSE\n")
  cat("Final botanical boundaries created: FALSE\n")
  cat("Kuriles constructed: FALSE\n")
  cat("Occurrences assigned: FALSE\n")
  cat("Coordinates modified: FALSE\n")
  cat("Taxonomy modified: FALSE\n")
  cat("Frozen upstream tables modified: FALSE\n")
  cat(
    "Output status: OGASAWARA / KAZAN SCOPE VALIDATION\n"
  )
  
  cat(
    "\n03d_validate_ogasawara_kazan_scope.R v",
    SCRIPT_VERSION,
    " complete.\n",
    sep = ""
  )
  
  invisible(
    list(
      remote_table =
        remote_table,
      cluster_summary =
        cluster_summary,
      cluster_extents =
        cluster_extents,
      outliers =
        outliers,
      municipality_cluster_summary =
        municipality_cluster_summary,
      geometry_qa =
        geometry_qa,
      metadata =
        metadata
    )
  )
}

geography_03d <-
  run_ogasawara_kazan_validation()