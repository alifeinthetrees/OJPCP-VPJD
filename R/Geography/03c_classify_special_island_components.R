# ==============================================================================
# VPJD-OJPCP
# 03c_classify_special_island_components.R
# Version: 0.1.0
#
# Purpose:
#   Classify individual NLNI N03 polygon components belonging to the special
#   Japanese botanical island areas required by the VPJD framework.
#
# Special treatment:
#   JP47 Ryukyu Islands:
#     - Okinawa Prefecture
#     - Amami component of Kagoshima Prefecture
#
#   JP48 Izu Islands:
#     - Tokyo island components south of mainland Tokyo and north of 30 N
#
#   JP49 Ogasawara Islands:
#     - Tokyo remote-island components from 26 N to <30 N
#
#   JP50 Kazan Islands:
#     - Tokyo remote-island components south of 26 N
#
#   JP51 Kuriles:
#     - NOT constructed here; requires a separate non-N03 source.
#
# Important:
#   Classification is performed on individual N03 polygon features, NOT on
#   municipality bounding boxes.
#
# This module:
#   - creates a diagnostic component-classification layer;
#   - records centroid and bounding-box evidence;
#   - identifies threshold-crossing components;
#   - does NOT create final botanical-area polygons;
#   - does NOT assign occurrences.
#
# Required frozen inputs:
#   Geography 02  v0.1.0
#   Geography 03a v0.1.0
#   Geography 03b v0.1.1
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

get_metadata_value <- function(con, table_name, metric_name) {
  fields <- dbListFields(con, table_name)
  if (all(c("metric", "value") %in% fields)) {
    sql <- paste0(
      "SELECT CAST(value AS VARCHAR) AS value FROM ",
      dbQuoteIdentifier(con, table_name),
      " WHERE CAST(metric AS VARCHAR) = ?"
    )
    x <- dbGetQuery(con, sql, params = list(metric_name))
    if (nrow(x) != 1) {
      stop(
        "Expected one metadata row for ",
        metric_name, " in ", table_name, "."
      )
    }
    return(as.character(x$value[[1]]))
  }
  stop(
    "Metadata item not found: ",
    metric_name, " in ", table_name, "."
  )
}

bbox_table <- function(x) {
  bind_rows(
    lapply(
      seq_len(nrow(x)),
      function(i) {
        b <- sf::st_bbox(x[i, ])
        tibble(
          xmin = as.numeric(b["xmin"]),
          ymin = as.numeric(b["ymin"]),
          xmax = as.numeric(b["xmax"]),
          ymax = as.numeric(b["ymax"])
        )
      }
    )
  )
}

run_special_component_classification <- function() {
  cat("\n— VPJD special-island component classification —\n\n")
  
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
    "japan_nlni_n03_inspection_metadata"
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
  
  if (g02 != EXPECTED_GEOGRAPHY_02_VERSION) {
    stop("Expected Geography 02 v0.1.0; found ", g02, ".")
  }
  
  if (g03a != EXPECTED_GEOGRAPHY_03A_VERSION) {
    stop("Expected Geography 03a v0.1.0; found ", g03a, ".")
  }
  
  if (g03b != EXPECTED_GEOGRAPHY_03B_VERSION) {
    stop("Expected Geography 03b v0.1.1; found ", g03b, ".")
  }
  
  cat(
    "Geography 02 version: ", g02, "\n",
    "Geography 03a version: ", g03a, "\n",
    "Geography 03b version: ", g03b, "\n\n",
    sep = ""
  )
  
  # ---------------------------------------------------------------------------
  # Locate the three relevant N03 main layers
  # ---------------------------------------------------------------------------
  
  extract_root <- here(
    "data", "interim", "geography",
    "nlni_n03_2025"
  )
  
  find_main_layer <- function(code) {
    pattern <- paste0(
      "^N03-20250101_",
      code,
      "\\.shp$"
    )
    
    x <- list.files(
      extract_root,
      pattern = pattern,
      recursive = TRUE,
      full.names = TRUE,
      ignore.case = TRUE
    )
    
    if (length(x) != 1) {
      stop(
        "Expected one main N03 layer for code ",
        code, "; found ", length(x), "."
      )
    }
    
    x
  }
  
  tokyo_path <- find_main_layer("13")
  kagoshima_path <- find_main_layer("46")
  okinawa_path <- find_main_layer("47")
  
  cat("— Source layers —\n")
  cat("Tokyo:     ", basename(tokyo_path), "\n", sep = "")
  cat("Kagoshima: ", basename(kagoshima_path), "\n", sep = "")
  cat("Okinawa:   ", basename(okinawa_path), "\n\n", sep = "")
  
  # ---------------------------------------------------------------------------
  # Read relevant N03 polygons
  # ---------------------------------------------------------------------------
  
  read_n03 <- function(path) {
    x <- sf::st_read(
      path,
      quiet = TRUE,
      stringsAsFactors = FALSE
    )
    
    required <- c(
      "N03_001",
      "N03_002",
      "N03_003",
      "N03_004",
      "N03_007"
    )
    
    if (!all(required %in% names(x))) {
      stop(
        "Required N03 fields missing from ",
        basename(path), "."
      )
    }
    
    if (sf::st_crs(x)$epsg != 6668) {
      stop(
        basename(path),
        " is not EPSG:6668."
      )
    }
    
    x %>%
      select(
        N03_001,
        N03_002,
        N03_003,
        N03_004,
        N03_007
      ) %>%
      sf::st_transform(4326)
  }
  
  tokyo <- read_n03(tokyo_path)
  kagoshima <- read_n03(kagoshima_path)
  okinawa <- read_n03(okinawa_path)
  
  cat(
    "Tokyo features: ",
    format(nrow(tokyo), big.mark = ","), "\n",
    "Kagoshima features: ",
    format(nrow(kagoshima), big.mark = ","), "\n",
    "Okinawa features: ",
    format(nrow(okinawa), big.mark = ","), "\n\n",
    sep = ""
  )
  
  # ---------------------------------------------------------------------------
  # Attach stable diagnostic feature IDs
  # ---------------------------------------------------------------------------
  
  tokyo$source_feature_id <- sprintf(
    "TOKYO_%06d",
    seq_len(nrow(tokyo))
  )
  
  kagoshima$source_feature_id <- sprintf(
    "KAGOSHIMA_%06d",
    seq_len(nrow(kagoshima))
  )
  
  okinawa$source_feature_id <- sprintf(
    "OKINAWA_%06d",
    seq_len(nrow(okinawa))
  )
  
  # ---------------------------------------------------------------------------
  # Geometry diagnostics
  #
  # st_point_on_surface() is used rather than st_centroid() because a centroid
  # of a concave polygon can fall outside the polygon itself.
  # ---------------------------------------------------------------------------
  
  add_geometry_diagnostics <- function(x) {
    b <- bbox_table(x)
    
    pts <- suppressWarnings(
      sf::st_point_on_surface(x)
    )
    
    coords <- sf::st_coordinates(pts)
    
    x$point_lon <- coords[, 1]
    x$point_lat <- coords[, 2]
    x$xmin <- b$xmin
    x$ymin <- b$ymin
    x$xmax <- b$xmax
    x$ymax <- b$ymax
    x
  }
  
  tokyo <- add_geometry_diagnostics(tokyo)
  kagoshima <- add_geometry_diagnostics(kagoshima)
  okinawa <- add_geometry_diagnostics(okinawa)
  
  # ---------------------------------------------------------------------------
  # Classification rules
  #
  # IMPORTANT:
  # These latitude thresholds are used only to classify disconnected island
  # polygon components after inspection of the N03 geometry.
  #
  # Tokyo:
  #   >=35 N       mainland / ordinary Tokyo
  #   30-<35 N     Izu
  #   26-<30 N     Ogasawara
  #   <26 N        Kazan
  #
  # Kagoshima:
  #   <29 N        Amami -> botanical Ryukyu
  #   >=29 N       ordinary Kagoshima
  #
  # Okinawa:
  #   all polygons -> botanical Ryukyu
  #
  # A polygon whose bbox crosses a threshold is NOT silently assigned by its
  # representative point. It is flagged for review.
  # ---------------------------------------------------------------------------
  
  tokyo <- tokyo %>%
    mutate(
      threshold_crossing =
        (ymin < 35 & ymax >= 35) |
        (ymin < 30 & ymax >= 30) |
        (ymin < 26 & ymax >= 26),
      
      component_class = case_when(
        threshold_crossing ~
          "review_threshold_crossing",
        
        ymax < 26 ~
          "Kazan",
        
        ymin >= 26 & ymax < 30 ~
          "Ogasawara",
        
        ymin >= 30 & ymax < 35 ~
          "Izu",
        
        ymin >= 35 ~
          "Tokyo_remainder",
        
        TRUE ~
          "review_unclassified"
      ),
      
      target_area_id = case_when(
        component_class == "Izu" ~ "JP48",
        component_class == "Ogasawara" ~ "JP49",
        component_class == "Kazan" ~ "JP50",
        component_class == "Tokyo_remainder" ~ "JP13",
        TRUE ~ NA_character_
      ),
      
      target_area_name = case_when(
        component_class == "Izu" ~ "Izu Islands",
        component_class == "Ogasawara" ~ "Ogasawara Islands",
        component_class == "Kazan" ~ "Kazan Islands",
        component_class == "Tokyo_remainder" ~ "Tokyo",
        TRUE ~ NA_character_
      ),
      
      classification_method =
        "N03 individual polygon component + latitude band"
    )
  
  kagoshima <- kagoshima %>%
    mutate(
      threshold_crossing =
        ymin < 29 & ymax >= 29,
      
      component_class = case_when(
        threshold_crossing ~
          "review_threshold_crossing",
        
        ymax < 29 ~
          "Amami_Ryukyu",
        
        ymin >= 29 ~
          "Kagoshima_remainder",
        
        TRUE ~
          "review_unclassified"
      ),
      
      target_area_id = case_when(
        component_class == "Amami_Ryukyu" ~ "JP47",
        component_class == "Kagoshima_remainder" ~ "JP46",
        TRUE ~ NA_character_
      ),
      
      target_area_name = case_when(
        component_class == "Amami_Ryukyu" ~ "Ryukyu Islands",
        component_class == "Kagoshima_remainder" ~ "Kagoshima",
        TRUE ~ NA_character_
      ),
      
      classification_method =
        "N03 individual polygon component + latitude band"
    )
  
  okinawa <- okinawa %>%
    mutate(
      threshold_crossing = FALSE,
      component_class = "Okinawa_Ryukyu",
      target_area_id = "JP47",
      target_area_name = "Ryukyu Islands",
      classification_method =
        "N03 Okinawa Prefecture -> botanical Ryukyu"
    )
  
  # ---------------------------------------------------------------------------
  # Attribute-only diagnostic tables
  # ---------------------------------------------------------------------------
  
  make_component_table <- function(x, source_prefecture) {
    x %>%
      sf::st_drop_geometry() %>%
      transmute(
        source_prefecture = source_prefecture,
        source_feature_id = source_feature_id,
        prefecture_jp = as.character(N03_001),
        subprefecture_jp = as.character(N03_002),
        district_jp = as.character(N03_003),
        municipality_jp = as.character(N03_004),
        admin_code = as.character(N03_007),
        point_lon = point_lon,
        point_lat = point_lat,
        xmin = xmin,
        ymin = ymin,
        xmax = xmax,
        ymax = ymax,
        threshold_crossing = threshold_crossing,
        component_class = component_class,
        target_area_id = target_area_id,
        target_area_name = target_area_name,
        classification_method = classification_method
      ) %>%
      as_tibble()
  }
  
  tokyo_table <- make_component_table(
    tokyo,
    "Tokyo"
  )
  
  kagoshima_table <- make_component_table(
    kagoshima,
    "Kagoshima"
  )
  
  okinawa_table <- make_component_table(
    okinawa,
    "Okinawa"
  )
  
  all_components <- bind_rows(
    tokyo_table,
    kagoshima_table,
    okinawa_table
  )
  
  # ---------------------------------------------------------------------------
  # Summary
  # ---------------------------------------------------------------------------
  
  classification_summary <- all_components %>%
    count(
      source_prefecture,
      component_class,
      target_area_id,
      target_area_name,
      name = "polygon_features"
    ) %>%
    arrange(
      source_prefecture,
      component_class
    ) %>%
    as_tibble()
  
  review_components <- all_components %>%
    filter(
      threshold_crossing |
        is.na(target_area_id) |
        grepl(
          "^review_",
          component_class
        )
    ) %>%
    arrange(
      source_prefecture,
      ymin,
      xmin
    ) %>%
    as_tibble()
  
  special_components <- all_components %>%
    filter(
      target_area_id %in%
        c("JP47", "JP48", "JP49", "JP50")
    ) %>%
    arrange(
      target_area_id,
      point_lat,
      point_lon
    ) %>%
    as_tibble()
  
  # ---------------------------------------------------------------------------
  # Municipality diagnostics
  #
  # This lets us see which municipalities contribute polygon components to
  # more than one botanical classification.
  # ---------------------------------------------------------------------------
  
  municipality_classes <- all_components %>%
    filter(
      !is.na(municipality_jp),
      trimws(municipality_jp) != ""
    ) %>%
    distinct(
      source_prefecture,
      municipality_jp,
      admin_code,
      component_class,
      target_area_id
    ) %>%
    group_by(
      source_prefecture,
      municipality_jp,
      admin_code
    ) %>%
    summarise(
      n_component_classes =
        n_distinct(component_class),
      component_classes =
        paste(
          sort(
            unique(component_class)
          ),
          collapse = ";"
        ),
      target_area_ids =
        paste(
          sort(
            unique(
              target_area_id[
                !is.na(target_area_id)
              ]
            )
          ),
          collapse = ";"
        ),
      .groups = "drop"
    ) %>%
    filter(
      n_component_classes > 1
    ) %>%
    arrange(
      source_prefecture,
      municipality_jp
    ) %>%
    as_tibble()
  
  # ---------------------------------------------------------------------------
  # Botanical-component extents
  # ---------------------------------------------------------------------------
  
  special_sf <- rbind(
    tokyo %>%
      filter(
        target_area_id %in%
          c("JP48", "JP49", "JP50")
      ),
    kagoshima %>%
      filter(target_area_id == "JP47"),
    okinawa %>%
      filter(target_area_id == "JP47")
  )
  
  special_area_extents <- lapply(
    c("JP47", "JP48", "JP49", "JP50"),
    function(id) {
      x <- special_sf %>%
        filter(target_area_id == id)
      
      if (nrow(x) == 0) {
        return(
          tibble(
            target_area_id = id,
            target_area_name = NA_character_,
            polygon_features = 0L,
            xmin = NA_real_,
            ymin = NA_real_,
            xmax = NA_real_,
            ymax = NA_real_
          )
        )
      }
      
      b <- sf::st_bbox(x)
      
      tibble(
        target_area_id = id,
        target_area_name =
          unique(x$target_area_name)[1],
        polygon_features = nrow(x),
        xmin = as.numeric(b["xmin"]),
        ymin = as.numeric(b["ymin"]),
        xmax = as.numeric(b["xmax"]),
        ymax = as.numeric(b["ymax"])
      )
    }
  ) %>%
    bind_rows() %>%
    as_tibble()
  
  # ---------------------------------------------------------------------------
  # Geometry validity
  # ---------------------------------------------------------------------------
  
  geometry_qa <- tibble(
    source_prefecture = c(
      "Tokyo",
      "Kagoshima",
      "Okinawa"
    ),
    polygon_features = c(
      nrow(tokyo),
      nrow(kagoshima),
      nrow(okinawa)
    ),
    valid_geometries = c(
      sum(sf::st_is_valid(tokyo)),
      sum(sf::st_is_valid(kagoshima)),
      sum(sf::st_is_valid(okinawa))
    ),
    invalid_geometries = c(
      sum(!sf::st_is_valid(tokyo)),
      sum(!sf::st_is_valid(kagoshima)),
      sum(!sf::st_is_valid(okinawa))
    )
  )
  
  # ---------------------------------------------------------------------------
  # Persist diagnostic tables
  # ---------------------------------------------------------------------------
  
  dbWriteTable(
    con,
    "japan_nlni_special_component_classification",
    all_components,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "japan_nlni_special_component_summary",
    classification_summary,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "japan_nlni_special_component_review",
    review_components,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "japan_nlni_special_components",
    special_components,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "japan_nlni_special_municipality_class_conflicts",
    municipality_classes,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "japan_nlni_special_area_extents",
    special_area_extents,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "japan_nlni_special_component_geometry_qa",
    geometry_qa,
    overwrite = TRUE
  )
  
  # ---------------------------------------------------------------------------
  # Persist spatial QA layer
  #
  # GeoPackage is used because DuckDB is currently our analytical database,
  # not our canonical sf geometry store.
  # ---------------------------------------------------------------------------
  
  spatial_dir <- here(
    "data",
    "interim",
    "geography",
    "botanical_components"
  )
  
  dir.create(
    spatial_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  gpkg_path <- file.path(
    spatial_dir,
    "vpjd_special_island_components.gpkg"
  )
  
  if (file.exists(gpkg_path)) {
    file.remove(gpkg_path)
  }
  
  sf::st_write(
    tokyo,
    gpkg_path,
    layer = "tokyo_components",
    quiet = TRUE
  )
  
  sf::st_write(
    kagoshima,
    gpkg_path,
    layer = "kagoshima_components",
    append = TRUE,
    quiet = TRUE
  )
  
  sf::st_write(
    okinawa,
    gpkg_path,
    layer = "okinawa_components",
    append = TRUE,
    quiet = TRUE
  )
  
  sf::st_write(
    special_sf,
    gpkg_path,
    layer = "candidate_botanical_components",
    append = TRUE,
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
      "classification_level",
      "tokyo_polygon_features",
      "kagoshima_polygon_features",
      "okinawa_polygon_features",
      "review_components",
      "kuriles_constructed",
      "final_botanical_boundaries_created",
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
      "individual_N03_polygon_feature",
      as.character(nrow(tokyo)),
      as.character(nrow(kagoshima)),
      as.character(nrow(okinawa)),
      as.character(nrow(review_components)),
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
    "japan_nlni_special_component_metadata",
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
    "special_island_components"
  )
  
  dir.create(
    out_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  write_csv(
    classification_summary,
    file.path(
      out_dir,
      "classification_summary.csv"
    )
  )
  
  write_csv(
    special_area_extents,
    file.path(
      out_dir,
      "special_area_extents.csv"
    )
  )
  
  write_csv(
    review_components,
    file.path(
      out_dir,
      "review_components.csv"
    )
  )
  
  write_csv(
    municipality_classes,
    file.path(
      out_dir,
      "municipality_class_conflicts.csv"
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
    special_components,
    file.path(
      out_dir,
      "special_components.csv"
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
  
  cat("\n— Component classification summary —\n\n")
  print.data.frame(
    as.data.frame(
      classification_summary
    ),
    row.names = FALSE
  )
  
  cat("\n— Candidate botanical-area extents —\n\n")
  print.data.frame(
    as.data.frame(
      special_area_extents
    ),
    row.names = FALSE
  )
  
  cat("\n— Geometry QA —\n\n")
  print.data.frame(
    as.data.frame(
      geometry_qa
    ),
    row.names = FALSE
  )
  
  cat("\n— Components requiring review —\n\n")
  
  if (nrow(review_components) == 0) {
    cat("None.\n")
  } else {
    print.data.frame(
      as.data.frame(
        review_components %>%
          select(
            source_prefecture,
            source_feature_id,
            municipality_jp,
            admin_code,
            point_lon,
            point_lat,
            xmin,
            ymin,
            xmax,
            ymax,
            component_class
          )
      ),
      row.names = FALSE
    )
  }
  
  cat("\n— Municipalities spanning classifications —\n\n")
  
  if (nrow(municipality_classes) == 0) {
    cat("None.\n")
  } else {
    print.data.frame(
      as.data.frame(
        municipality_classes
      ),
      row.names = FALSE
    )
  }
  
  cat("\n— Validation —\n")
  cat(
    "Tokyo polygons classified: ",
    nrow(tokyo), "\n",
    sep = ""
  )
  cat(
    "Kagoshima polygons classified: ",
    nrow(kagoshima), "\n",
    sep = ""
  )
  cat(
    "Okinawa polygons classified: ",
    nrow(okinawa), "\n",
    sep = ""
  )
  cat(
    "Components requiring review: ",
    nrow(review_components), "\n",
    sep = ""
  )
  cat(
    "JP47 candidate polygons present: ",
    any(
      special_components$target_area_id ==
        "JP47"
    ),
    "\n",
    sep = ""
  )
  cat(
    "JP48 candidate polygons present: ",
    any(
      special_components$target_area_id ==
        "JP48"
    ),
    "\n",
    sep = ""
  )
  cat(
    "JP49 candidate polygons present: ",
    any(
      special_components$target_area_id ==
        "JP49"
    ),
    "\n",
    sep = ""
  )
  cat(
    "JP50 candidate polygons present: ",
    any(
      special_components$target_area_id ==
        "JP50"
    ),
    "\n",
    sep = ""
  )
  
  cat("\n— Safety —\n")
  cat("Final botanical boundaries created: FALSE\n")
  cat("Kuriles constructed: FALSE\n")
  cat("Occurrences assigned: FALSE\n")
  cat("Coordinates modified: FALSE\n")
  cat("Taxonomy modified: FALSE\n")
  cat("Frozen upstream tables modified: FALSE\n")
  cat(
    "Output status: SPECIAL-ISLAND COMPONENT CLASSIFICATION / QA\n"
  )
  
  cat(
    "\n03c_classify_special_island_components.R v",
    SCRIPT_VERSION,
    " complete.\n",
    sep = ""
  )
  
  invisible(
    list(
      classification_summary =
        classification_summary,
      special_area_extents =
        special_area_extents,
      review_components =
        review_components,
      municipality_classes =
        municipality_classes,
      geometry_qa =
        geometry_qa,
      metadata =
        metadata
    )
  )
}

geography_03c <-
  run_special_component_classification()