# ==============================================================================
# VPJD-OJPCP
# 03j_build_complete_botanical_boundaries.R
# Version: 0.1.1
#
# Purpose:
#   Construct canonical JP51 Kuriles from the component decisions frozen in
#   Geography 03i, combine it with validated JP01-JP50 from Geography 03e,
#   and validate the complete 51-area VPJD botanical geography.
#
# Sources:
#   JP01-JP50: Japan MLIT/NLNI N03 Administrative Areas, 2025.
#   JP51: Natural Earth 10m Land v5.1.1.
#
# This module:
#   - validates Geography 03e v0.1.1;
#   - validates Geography 03i v0.1.0;
#   - validates the actual frozen 03e spatial schema;
#   - dissolves the 22 approved JP51 components;
#   - attaches frozen Geography 02 metadata;
#   - combines JP01-JP51;
#   - validates geometry and positive-area topology;
#   - explicitly tests JP01/JP51 overlap;
#   - preserves mixed-source provenance;
#   - DOES NOT assign occurrences;
#   - DOES NOT modify occurrence coordinates or taxonomy.
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
EXPECTED_GEOGRAPHY_03I_VERSION <- "0.1.0"
OVERLAP_TOLERANCE_M2 <- 1

run_complete_botanical_boundaries <- function() {
  cat("\n— VPJD complete botanical-area boundary construction —\n\n")
  
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
  # Validate Geography 03e
  # ---------------------------------------------------------------------------
  
  required_03e <- c(
    "japan_botanical_boundary_areas",
    "japan_botanical_boundary_validation",
    "japan_botanical_boundary_metadata"
  )
  
  missing_03e <- setdiff(
    required_03e,
    dbListTables(con)
  )
  
  if (length(missing_03e) > 0) {
    stop(
      "Required Geography 03e tables missing: ",
      paste(missing_03e, collapse = ", ")
    )
  }
  
  metadata_03e <- dbReadTable(
    con,
    "japan_botanical_boundary_metadata"
  ) %>%
    as_tibble()
  
  if (!all(c("metric", "value") %in% names(metadata_03e))) {
    stop("Unexpected Geography 03e metadata schema.")
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
  
  if (
    !"pass" %in% names(validation_03e) ||
    !all(validation_03e$pass)
  ) {
    stop("Geography 03e validation is not fully PASS.")
  }
  
  cat(
    "Geography 03e version: ", version_03e,
    "\nGeography 03e validation: PASS\n",
    sep = ""
  )
  
  # ---------------------------------------------------------------------------
  # Validate Geography 03i
  # ---------------------------------------------------------------------------
  
  required_03i <- c(
    "japan_kuriles_final_component_decisions",
    "japan_kuriles_jp51_candidate_components",
    "japan_kuriles_residual_validation",
    "japan_kuriles_residual_metadata"
  )
  
  missing_03i <- setdiff(
    required_03i,
    dbListTables(con)
  )
  
  if (length(missing_03i) > 0) {
    stop(
      "Required Geography 03i tables missing: ",
      paste(missing_03i, collapse = ", ")
    )
  }
  
  metadata_03i <- dbReadTable(
    con,
    "japan_kuriles_residual_metadata"
  ) %>%
    as_tibble()
  
  if (!all(c("metric", "value") %in% names(metadata_03i))) {
    stop("Unexpected Geography 03i metadata schema.")
  }
  
  version_03i <- metadata_03i %>%
    filter(metric == "script_version") %>%
    pull(value)
  
  if (
    length(version_03i) != 1 ||
    version_03i != EXPECTED_GEOGRAPHY_03I_VERSION
  ) {
    stop(
      "Expected Geography 03i v",
      EXPECTED_GEOGRAPHY_03I_VERSION,
      "; found ",
      paste(version_03i, collapse = ", "),
      "."
    )
  }
  
  validation_03i <- dbReadTable(
    con,
    "japan_kuriles_residual_validation"
  ) %>%
    as_tibble()
  
  if (
    !"pass" %in% names(validation_03i) ||
    !all(validation_03i$pass)
  ) {
    stop("Geography 03i validation is not fully PASS.")
  }
  
  decisions_03i <- dbReadTable(
    con,
    "japan_kuriles_final_component_decisions"
  ) %>%
    as_tibble()
  
  if (
    nrow(decisions_03i) != 28 ||
    sum(decisions_03i$jp51_inclusion_final == "include") != 22 ||
    sum(decisions_03i$jp51_inclusion_final == "exclude") != 6 ||
    any(decisions_03i$jp51_inclusion_final == "unresolved")
  ) {
    stop(
      "Geography 03i accounting is not ",
      "22 include / 6 exclude / 0 unresolved."
    )
  }
  
  cat(
    "Geography 03i version: ", version_03i,
    "\nGeography 03i validation: PASS\n",
    "JP51 component accounting: 22 include / 6 exclude / 0 unresolved\n\n",
    sep = ""
  )
  
  # ---------------------------------------------------------------------------
  # Validate Geography 02 framework
  # ---------------------------------------------------------------------------
  
  if (!"japan_botanical_areas" %in% dbListTables(con)) {
    stop("Frozen Geography 02 table 'japan_botanical_areas' is missing.")
  }
  
  framework <- dbReadTable(
    con,
    "japan_botanical_areas"
  ) %>%
    as_tibble()
  
  required_framework_fields <- c(
    "area_id",
    "area_no",
    "analytical_area_name",
    "area_type",
    "district",
    "political_parent",
    "display_area_id",
    "display_area_no",
    "display_area_name",
    "star_small_island_group",
    "special_assignment_required",
    "notes"
  )
  
  missing_framework_fields <- setdiff(
    required_framework_fields,
    names(framework)
  )
  
  if (length(missing_framework_fields) > 0) {
    stop(
      "Required Geography 02 fields missing: ",
      paste(missing_framework_fields, collapse = ", ")
    )
  }
  
  expected_51 <- sprintf("JP%02d", 1:51)
  
  if (
    nrow(framework) != 51 ||
    n_distinct(framework$area_id) != 51 ||
    !setequal(framework$area_id, expected_51)
  ) {
    stop(
      "Geography 02 framework does not contain exactly JP01-JP51."
    )
  }
  
  jp51_reference <- framework %>%
    filter(area_id == "JP51")
  
  if (
    nrow(jp51_reference) != 1 ||
    jp51_reference$area_no != 51 ||
    jp51_reference$analytical_area_name != "Kuriles"
  ) {
    stop("Unexpected Geography 02 JP51 reference definition.")
  }
  
  cat(
    "Geography 02 analytical framework: PASS\n",
    "Reference areas: 51\n",
    "JP51 reference name: ",
    jp51_reference$analytical_area_name,
    "\n\n",
    sep = ""
  )
  
  # ---------------------------------------------------------------------------
  # Read and validate actual Geography 03e spatial schema
  # ---------------------------------------------------------------------------
  
  boundary_03e_gpkg <- here(
    "data", "interim", "geography", "botanical_boundaries",
    "vpjd_japan_botanical_boundaries.gpkg"
  )
  
  if (!file.exists(boundary_03e_gpkg)) {
    stop("Geography 03e GeoPackage not found: ", boundary_03e_gpkg)
  }
  
  layers_03e <- sf::st_layers(boundary_03e_gpkg)$name
  
  if (!"botanical_areas_jp01_jp50" %in% layers_03e) {
    stop(
      "Layer 'botanical_areas_jp01_jp50' not found in Geography 03e."
    )
  }
  
  jp01_50_raw <- sf::st_read(
    boundary_03e_gpkg,
    layer = "botanical_areas_jp01_jp50",
    quiet = TRUE
  ) %>%
    sf::st_transform(4326)
  
  required_03e_spatial_fields <- c(
    "botanical_area_id",
    "source_polygon_features",
    "botanical_area_name"
  )
  
  missing_03e_spatial_fields <- setdiff(
    required_03e_spatial_fields,
    names(jp01_50_raw)
  )
  
  if (length(missing_03e_spatial_fields) > 0) {
    stop(
      "Required Geography 03e spatial fields missing: ",
      paste(missing_03e_spatial_fields, collapse = ", ")
    )
  }
  
  expected_50 <- sprintf("JP%02d", 1:50)
  
  if (
    nrow(jp01_50_raw) != 50 ||
    n_distinct(jp01_50_raw$botanical_area_id) != 50 ||
    !setequal(jp01_50_raw$botanical_area_id, expected_50)
  ) {
    stop(
      "Geography 03e spatial layer does not contain exactly JP01-JP50."
    )
  }
  
  if (any(!sf::st_is_valid(jp01_50_raw))) {
    stop(
      "One or more Geography 03e JP01-JP50 geometries are invalid."
    )
  }
  
  jp01_50_raw <- jp01_50_raw %>%
    arrange(
      match(
        botanical_area_id,
        expected_50
      )
    )
  
  cat(
    "Geography 03e spatial schema: PASS\n",
    "Canonical JP01-JP50 spatial layer: PASS\n",
    "Areas: ", nrow(jp01_50_raw), "\n",
    "Geometry type: MULTIPOLYGON\n",
    "CRS: EPSG:4326\n\n",
    sep = ""
  )
  
  # ---------------------------------------------------------------------------
  # Read Geography 03i JP51 component geometry
  # ---------------------------------------------------------------------------
  
  kuriles_03i_gpkg <- here(
    "data", "interim", "geography", "kuriles",
    "vpjd_kuriles_final_component_decisions.gpkg"
  )
  
  if (!file.exists(kuriles_03i_gpkg)) {
    stop("Geography 03i GeoPackage not found: ", kuriles_03i_gpkg)
  }
  
  layers_03i <- sf::st_layers(kuriles_03i_gpkg)$name
  
  if (!"jp51_include_components" %in% layers_03i) {
    stop(
      "Layer 'jp51_include_components' not found in Geography 03i."
    )
  }
  
  jp51_components <- sf::st_read(
    kuriles_03i_gpkg,
    layer = "jp51_include_components",
    quiet = TRUE
  ) %>%
    sf::st_transform(4326)
  
  if (nrow(jp51_components) != 22) {
    stop(
      "Expected 22 JP51 include components; found ",
      nrow(jp51_components), "."
    )
  }
  
  if (any(!sf::st_is_valid(jp51_components))) {
    stop("One or more JP51 source components are invalid.")
  }
  
  expected_jp51_ids <- decisions_03i %>%
    filter(jp51_inclusion_final == "include") %>%
    pull(component_id) %>%
    sort()
  
  spatial_jp51_ids <- sort(
    jp51_components$component_id
  )
  
  if (!identical(
    expected_jp51_ids,
    spatial_jp51_ids
  )) {
    stop(
      "JP51 spatial component IDs do not match frozen Geography 03i decisions."
    )
  }
  
  cat(
    "JP51 spatial component verification: PASS\n",
    "Approved components: 22\n\n",
    sep = ""
  )
  
  # ---------------------------------------------------------------------------
  # Construct JP51
  # ---------------------------------------------------------------------------
  
  cat("Constructing JP51 Kuriles geometry...\n")
  
  jp51_union <- sf::st_union(
    sf::st_geometry(
      jp51_components
    )
  )
  
  jp51_union <- sf::st_make_valid(
    jp51_union
  )
  
  jp51 <- sf::st_sf(
    area_id = "JP51",
    area_no = 51L,
    analytical_area_name =
      jp51_reference$analytical_area_name,
    area_type =
      jp51_reference$area_type,
    district =
      jp51_reference$district,
    political_parent =
      jp51_reference$political_parent,
    display_area_id =
      jp51_reference$display_area_id,
    display_area_no =
      jp51_reference$display_area_no,
    display_area_name =
      jp51_reference$display_area_name,
    star_small_island_group =
      jp51_reference$star_small_island_group,
    special_assignment_required =
      jp51_reference$special_assignment_required,
    notes =
      jp51_reference$notes,
    source_polygon_features = 22L,
    boundary_source =
      "Natural Earth 10m Land",
    boundary_source_version =
      "5.1.1",
    boundary_method =
      paste(
        "Union of 22 physical island components selected",
        "through Geography 03f-03i"
      ),
    geometry = jp51_union,
    crs = 4326
  )
  
  if (
    nrow(jp51) != 1 ||
    !sf::st_is_valid(jp51)[1]
  ) {
    stop("Constructed JP51 geometry failed validation.")
  }
  
  jp51_area_km2 <- as.numeric(
    sf::st_area(
      sf::st_transform(jp51, 6933)
    )
  ) / 1e6
  
  jp51_bbox <- sf::st_bbox(jp51)
  
  jp51_geometry_summary <- tibble(
    area_id = "JP51",
    analytical_area_name = "Kuriles",
    source_components = 22L,
    area_km2 = jp51_area_km2,
    xmin = as.numeric(jp51_bbox["xmin"]),
    ymin = as.numeric(jp51_bbox["ymin"]),
    xmax = as.numeric(jp51_bbox["xmax"]),
    ymax = as.numeric(jp51_bbox["ymax"]),
    geometry_valid = sf::st_is_valid(jp51)[1],
    boundary_source = "Natural Earth 10m Land",
    boundary_source_version = "5.1.1"
  )
  
  # ---------------------------------------------------------------------------
  # Attach Geography 02 metadata to JP01-JP50
  # ---------------------------------------------------------------------------
  
  jp01_50_geometry <- jp01_50_raw %>%
    transmute(
      area_id = botanical_area_id,
      source_polygon_features =
        source_polygon_features,
      source_botanical_area_name =
        botanical_area_name,
      geometry = sf::st_geometry(jp01_50_raw)
    )
  
  jp01_50_framework <- framework %>%
    filter(area_id %in% expected_50) %>%
    arrange(area_no)
  
  jp01_50_complete <- jp01_50_framework %>%
    left_join(
      jp01_50_geometry,
      by = "area_id"
    )
  
  jp01_50_complete <- sf::st_as_sf(
    jp01_50_complete,
    sf_column_name = "geometry",
    crs = 4326
  )
  
  if (any(sf::st_is_empty(jp01_50_complete))) {
    stop(
      "One or more JP01-JP50 geometries failed to join to Geography 02."
    )
  }
  
  name_check <- jp01_50_complete %>%
    sf::st_drop_geometry() %>%
    transmute(
      area_id,
      analytical_area_name,
      source_botanical_area_name,
      names_match =
        analytical_area_name ==
        source_botanical_area_name
    )
  
  if (!all(name_check$names_match)) {
    stop(
      "Geography 02 and Geography 03e botanical-area names disagree."
    )
  }
  
  jp01_50_complete <- jp01_50_complete %>%
    select(
      -source_botanical_area_name
    ) %>%
    mutate(
      boundary_source =
        "Japan MLIT/NLNI N03 Administrative Areas",
      boundary_source_version =
        "2025",
      boundary_method =
        paste(
          "Canonical Geography 03e botanical boundary;",
          "component-level special-island treatment where required"
        )
    )
  
  # ---------------------------------------------------------------------------
  # Combine JP01-JP51
  # ---------------------------------------------------------------------------
  
  common_fields <- c(
    "area_id",
    "area_no",
    "analytical_area_name",
    "area_type",
    "district",
    "political_parent",
    "display_area_id",
    "display_area_no",
    "display_area_name",
    "star_small_island_group",
    "special_assignment_required",
    "notes",
    "source_polygon_features",
    "boundary_source",
    "boundary_source_version",
    "boundary_method",
    "geometry"
  )
  
  jp01_50_complete <- jp01_50_complete %>%
    select(all_of(common_fields))
  
  jp51 <- jp51 %>%
    select(all_of(common_fields))
  
  complete_51 <- rbind(
    jp01_50_complete,
    jp51
  ) %>%
    arrange(area_no)
  
  geometry_valid <- sf::st_is_valid(
    complete_51
  )
  
  if (!all(geometry_valid)) {
    stop(
      "One or more complete botanical-area geometries are invalid."
    )
  }
  
  if (
    nrow(complete_51) != 51 ||
    n_distinct(complete_51$area_id) != 51 ||
    !identical(
      complete_51$area_id,
      expected_51
    )
  ) {
    stop(
      "Complete boundary layer does not contain ordered JP01-JP51."
    )
  }
  
  cat(
    "Complete JP01-JP51 assembly: PASS\n",
    "Botanical areas: 51\n",
    "Valid geometries: 51\n\n",
    sep = ""
  )
  
  # ---------------------------------------------------------------------------
  # Area and extent diagnostics
  # ---------------------------------------------------------------------------
  
  area_values_km2 <- as.numeric(
    sf::st_area(
      sf::st_transform(
        complete_51,
        6933
      )
    )
  ) / 1e6
  
  bboxes <- lapply(
    seq_len(nrow(complete_51)),
    function(i) {
      bb <- sf::st_bbox(
        complete_51[i, ]
      )
      
      tibble(
        area_id = complete_51$area_id[i],
        xmin = as.numeric(bb["xmin"]),
        ymin = as.numeric(bb["ymin"]),
        xmax = as.numeric(bb["xmax"]),
        ymax = as.numeric(bb["ymax"])
      )
    }
  ) %>%
    bind_rows()
  
  boundary_summary <- complete_51 %>%
    sf::st_drop_geometry() %>%
    mutate(
      area_km2 = area_values_km2,
      geometry_valid = geometry_valid
    ) %>%
    left_join(
      bboxes,
      by = "area_id"
    ) %>%
    arrange(area_no) %>%
    as_tibble()
  
  # ---------------------------------------------------------------------------
  # Positive-area overlap QA
  # ---------------------------------------------------------------------------
  
  cat("Running 51-area positive-overlap QA...\n")
  
  complete_equal_area <- sf::st_transform(
    complete_51,
    6933
  )
  
  intersection_index <- sf::st_intersects(
    complete_equal_area,
    sparse = TRUE
  )
  
  candidate_pairs <- list()
  pair_counter <- 0L
  
  for (i in seq_len(nrow(complete_equal_area) - 1L)) {
    js <- intersection_index[[i]]
    js <- js[js > i]
    
    if (length(js) == 0) {
      next
    }
    
    for (j in js) {
      pair_counter <- pair_counter + 1L
      
      candidate_pairs[[pair_counter]] <- tibble(
        i = i,
        j = j
      )
    }
  }
  
  if (length(candidate_pairs) == 0) {
    overlap_qa <- tibble(
      area_id_1 = character(),
      area_name_1 = character(),
      area_id_2 = character(),
      area_name_2 = character(),
      intersection_area_m2 = double(),
      positive_overlap_gt_1m2 = logical()
    )
  } else {
    candidate_pairs <- bind_rows(
      candidate_pairs
    )
    
    overlap_records <- vector(
      "list",
      nrow(candidate_pairs)
    )
    
    for (k in seq_len(nrow(candidate_pairs))) {
      i <- candidate_pairs$i[k]
      j <- candidate_pairs$j[k]
      
      intersection_geometry <- suppressWarnings(
        sf::st_intersection(
          sf::st_geometry(
            complete_equal_area[i, ]
          ),
          sf::st_geometry(
            complete_equal_area[j, ]
          )
        )
      )
      
      intersection_area <- if (
        length(intersection_geometry) == 0
      ) {
        0
      } else {
        sum(
          as.numeric(
            sf::st_area(
              intersection_geometry
            )
          ),
          na.rm = TRUE
        )
      }
      
      overlap_records[[k]] <- tibble(
        area_id_1 =
          complete_51$area_id[i],
        area_name_1 =
          complete_51$analytical_area_name[i],
        area_id_2 =
          complete_51$area_id[j],
        area_name_2 =
          complete_51$analytical_area_name[j],
        intersection_area_m2 =
          intersection_area,
        positive_overlap_gt_1m2 =
          intersection_area >
          OVERLAP_TOLERANCE_M2
      )
    }
    
    overlap_qa <- bind_rows(
      overlap_records
    ) %>%
      arrange(
        desc(intersection_area_m2),
        area_id_1,
        area_id_2
      )
  }
  
  positive_overlaps <- overlap_qa %>%
    filter(
      positive_overlap_gt_1m2
    )
  
  # ---------------------------------------------------------------------------
  # Explicit JP01 / JP51 overlap QA
  # ---------------------------------------------------------------------------
  
  jp01_equal_area <- complete_equal_area %>%
    filter(area_id == "JP01")
  
  jp51_equal_area <- complete_equal_area %>%
    filter(area_id == "JP51")
  
  jp01_jp51_intersection <- suppressWarnings(
    sf::st_intersection(
      sf::st_geometry(jp01_equal_area),
      sf::st_geometry(jp51_equal_area)
    )
  )
  
  jp01_jp51_overlap_m2 <- if (
    length(jp01_jp51_intersection) == 0
  ) {
    0
  } else {
    sum(
      as.numeric(
        sf::st_area(
          jp01_jp51_intersection
        )
      ),
      na.rm = TRUE
    )
  }
  
  jp01_jp51_qa <- tibble(
    area_id_1 = "JP01",
    area_name_1 = "Hokkaido",
    area_id_2 = "JP51",
    area_name_2 = "Kuriles",
    intersection_area_m2 =
      jp01_jp51_overlap_m2,
    overlap_tolerance_m2 =
      OVERLAP_TOLERANCE_M2,
    pass =
      jp01_jp51_overlap_m2 <=
      OVERLAP_TOLERANCE_M2
  )
  
  # ---------------------------------------------------------------------------
  # Provenance and QA
  # ---------------------------------------------------------------------------
  
  provenance_summary <- boundary_summary %>%
    count(
      boundary_source,
      boundary_source_version,
      name = "botanical_areas"
    ) %>%
    arrange(boundary_source) %>%
    as_tibble()
  
  qa_summary <- tibble(
    metric = c(
      "botanical_areas",
      "unique_area_ids",
      "valid_geometries",
      "invalid_geometries",
      "jp01_jp50_areas",
      "jp51_areas",
      "jp51_source_components",
      "positive_area_overlaps_gt_1m2",
      "jp01_jp51_overlap_m2",
      "boundary_source_datasets"
    ),
    value = c(
      nrow(complete_51),
      n_distinct(complete_51$area_id),
      sum(geometry_valid),
      sum(!geometry_valid),
      sum(complete_51$area_id != "JP51"),
      sum(complete_51$area_id == "JP51"),
      nrow(jp51_components),
      nrow(positive_overlaps),
      jp01_jp51_overlap_m2,
      nrow(provenance_summary)
    )
  )
  
  validation <- tibble(
    check = c(
      "Geography 03e validation PASS",
      "Geography 03i validation PASS",
      "Geography 02 contains exactly JP01-JP51",
      "03e spatial schema validated",
      "JP01-JP50 canonical geometry retained",
      "Geography 02 and 03e names agree",
      "Exactly 22 approved JP51 source components",
      "JP51 source IDs match frozen 03i decisions",
      "JP51 geometry valid after dissolve",
      "Exactly 51 botanical areas",
      "Exactly 51 unique area IDs",
      "Area IDs exactly JP01-JP51",
      "All 51 geometries valid",
      "No positive-area overlaps >1 m2",
      "JP01 and JP51 do not positively overlap >1 m2",
      "JP01-JP50 provenance is N03 2025",
      "JP51 provenance is Natural Earth Land 5.1.1",
      "No occurrences assigned"
    ),
    pass = c(
      all(validation_03e$pass),
      all(validation_03i$pass),
      nrow(framework) == 51 &&
        n_distinct(framework$area_id) == 51,
      nrow(jp01_50_raw) == 50 &&
        all(required_03e_spatial_fields %in% names(jp01_50_raw)),
      nrow(jp01_50_complete) == 50,
      all(name_check$names_match),
      nrow(jp51_components) == 22,
      identical(expected_jp51_ids, spatial_jp51_ids),
      sf::st_is_valid(jp51)[1],
      nrow(complete_51) == 51,
      n_distinct(complete_51$area_id) == 51,
      identical(complete_51$area_id, expected_51),
      all(geometry_valid),
      nrow(positive_overlaps) == 0,
      jp01_jp51_overlap_m2 <= OVERLAP_TOLERANCE_M2,
      all(
        complete_51$boundary_source[
          complete_51$area_id != "JP51"
        ] ==
          "Japan MLIT/NLNI N03 Administrative Areas"
      ),
      all(
        complete_51$boundary_source[
          complete_51$area_id == "JP51"
        ] ==
          "Natural Earth 10m Land"
      ),
      TRUE
    )
  )
  
  # ---------------------------------------------------------------------------
  # Write canonical spatial output
  # ---------------------------------------------------------------------------
  
  output_dir <- here(
    "data", "interim", "geography",
    "botanical_boundaries"
  )
  
  dir.create(
    output_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  output_gpkg <- file.path(
    output_dir,
    "vpjd_japan_botanical_boundaries_complete.gpkg"
  )
  
  if (file.exists(output_gpkg)) {
    file.remove(output_gpkg)
  }
  
  sf::st_write(
    complete_51,
    output_gpkg,
    layer = "botanical_areas_jp01_jp51",
    quiet = TRUE
  )
  
  sf::st_write(
    jp51,
    output_gpkg,
    layer = "jp51_kuriles",
    append = TRUE,
    quiet = TRUE
  )
  
  sf::st_write(
    jp51_components,
    output_gpkg,
    layer = "jp51_source_components",
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
      "input_geography_03i_version",
      "framework_area_count",
      "canonical_boundary_area_count",
      "jp01_jp50_source",
      "jp01_jp50_source_version",
      "jp51_source",
      "jp51_source_version",
      "jp51_source_components",
      "jp51_definition",
      "jp51_definition_basis",
      "historical_nakamura_role",
      "sovereignty_used_to_define_jp51",
      "mixed_source_geometry",
      "positive_overlap_tolerance_m2",
      "occurrences_assigned",
      "coordinates_modified",
      "taxonomy_modified",
      "output_status"
    ),
    value = c(
      SCRIPT_VERSION,
      version_03e,
      version_03i,
      "51",
      as.character(nrow(complete_51)),
      "Japan MLIT/NLNI N03 Administrative Areas",
      "2025",
      "Natural Earth 10m Land",
      "5.1.1",
      as.character(nrow(jp51_components)),
      paste(
        "Physical Kuril island chain represented by the 22 components",
        "validated through Geography 03f-03i, including Habomai,",
        "Shikotan and Kunashir and continuing through the central",
        "and northern Kuril chain; Hokkaido, Sakhalin and Kamchatka",
        "context excluded."
      ),
      paste(
        "Contemporary physical botanical-geographic unit;",
        "not an administrative or sovereignty-based boundary."
      ),
      paste(
        "Historical methodological precedent and later",
        "comparison/calibration; not contemporary geometry authority."
      ),
      "FALSE",
      "TRUE",
      as.character(OVERLAP_TOLERANCE_M2),
      "FALSE",
      "FALSE",
      "FALSE",
      "COMPLETE 51-AREA BOTANICAL BOUNDARY FRAMEWORK"
    )
  )
  
  # ---------------------------------------------------------------------------
  # Persist DuckDB outputs
  # ---------------------------------------------------------------------------
  
  dbWriteTable(
    con,
    "japan_botanical_boundaries_complete",
    boundary_summary,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "japan_botanical_jp51_geometry_summary",
    jp51_geometry_summary,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "japan_botanical_boundary_provenance",
    provenance_summary,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "japan_botanical_boundary_overlap_qa_complete",
    overlap_qa,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "japan_botanical_boundary_positive_overlaps_complete",
    positive_overlaps,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "japan_botanical_jp01_jp51_overlap_qa",
    jp01_jp51_qa,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "japan_botanical_boundary_qa_complete",
    qa_summary,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "japan_botanical_boundary_validation_complete",
    validation,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "japan_botanical_boundary_metadata_complete",
    metadata,
    overwrite = TRUE
  )
  
  # ---------------------------------------------------------------------------
  # CSV outputs
  # ---------------------------------------------------------------------------
  
  table_dir <- here(
    "outputs", "tables", "geography",
    "botanical_boundaries_complete"
  )
  
  dir.create(
    table_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  write_csv(
    boundary_summary,
    file.path(
      table_dir,
      "botanical_boundaries_jp01_jp51.csv"
    )
  )
  
  write_csv(
    jp51_geometry_summary,
    file.path(
      table_dir,
      "jp51_geometry_summary.csv"
    )
  )
  
  write_csv(
    provenance_summary,
    file.path(
      table_dir,
      "boundary_provenance.csv"
    )
  )
  
  write_csv(
    overlap_qa,
    file.path(
      table_dir,
      "overlap_qa.csv"
    )
  )
  
  write_csv(
    positive_overlaps,
    file.path(
      table_dir,
      "positive_overlaps_gt_1m2.csv"
    )
  )
  
  write_csv(
    jp01_jp51_qa,
    file.path(
      table_dir,
      "jp01_jp51_overlap_qa.csv"
    )
  )
  
  write_csv(
    qa_summary,
    file.path(
      table_dir,
      "qa_summary.csv"
    )
  )
  
  write_csv(
    validation,
    file.path(
      table_dir,
      "validation.csv"
    )
  )
  
  write_csv(
    metadata,
    file.path(
      table_dir,
      "metadata.csv"
    )
  )
  
  # ---------------------------------------------------------------------------
  # Console report
  # ---------------------------------------------------------------------------
  
  cat("\n— JP51 Kuriles geometry —\n\n")
  print.data.frame(
    as.data.frame(jp51_geometry_summary),
    row.names = FALSE
  )
  
  cat("\n— Boundary provenance —\n\n")
  print.data.frame(
    as.data.frame(provenance_summary),
    row.names = FALSE
  )
  
  cat("\n— Complete geography QA —\n\n")
  print.data.frame(
    as.data.frame(qa_summary),
    row.names = FALSE
  )
  
  cat("\n— JP01 / JP51 overlap QA —\n\n")
  print.data.frame(
    as.data.frame(jp01_jp51_qa),
    row.names = FALSE
  )
  
  cat("\n— Positive-area overlaps >1 m2 —\n\n")
  
  if (nrow(positive_overlaps) == 0) {
    cat("None.\n")
  } else {
    print.data.frame(
      as.data.frame(positive_overlaps),
      row.names = FALSE
    )
  }
  
  cat("\n— Validation —\n\n")
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
  
  cat(
    "\nCanonical 51-area spatial layer written:\n",
    output_gpkg,
    "\n",
    sep = ""
  )
  
  cat("\n— Safety —\n")
  cat("JP51 boundary constructed: TRUE\n")
  cat("JP51 botanical definition finalised: TRUE\n")
  cat("Canonical JP01-JP51 layer constructed: TRUE\n")
  cat("JP01-JP50 source geometry modified: FALSE\n")
  cat("Sovereignty used to define JP51: FALSE\n")
  cat("Occurrences assigned: FALSE\n")
  cat("Occurrence coordinates modified: FALSE\n")
  cat("Taxonomy modified: FALSE\n")
  cat(
    "Output status: COMPLETE 51-AREA BOTANICAL BOUNDARY FRAMEWORK\n"
  )
  
  cat(
    "\n03j_build_complete_botanical_boundaries.R v",
    SCRIPT_VERSION,
    " complete.\n",
    sep = ""
  )
  
  invisible(
    list(
      complete_51 = complete_51,
      jp51 = jp51,
      jp51_components = jp51_components,
      boundary_summary = boundary_summary,
      jp51_geometry_summary = jp51_geometry_summary,
      provenance_summary = provenance_summary,
      overlap_qa = overlap_qa,
      positive_overlaps = positive_overlaps,
      jp01_jp51_qa = jp01_jp51_qa,
      qa_summary = qa_summary,
      validation = validation,
      metadata = metadata
    )
  )
}

geography_03j <-
  run_complete_botanical_boundaries()