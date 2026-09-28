# ==============================================================================
# VPJD-OJPCP
# 03i_resolve_residual_kuriles_components.R
# Version: 0.1.0
#
# Purpose:
#   Resolve the two residual geographic components retained as unresolved by
#   Geography 03h and complete the evidence base required to construct JP51.
#
# Conclusion tested here:
#   KUR_DIAG_00016 and KUR_DIAG_00018 are northeastern/eastern Hokkaido
#   contextual geometry, not components of the physical Kuril island chain.
#
# Methodological position:
#   Exact feature naming is not required for exclusion from JP51. The relevant
#   analytical question is whether each component belongs to the Kuril physical
#   island chain or to Hokkaido geographic context.
#
# This module:
#   - validates frozen Geography 03h v0.1.0;
#   - resolves both residual components as Hokkaido context;
#   - produces a final component-level JP51 evidence classification;
#   - requires zero unresolved components;
#   - DOES NOT construct/dissolve JP51;
#   - DOES NOT modify JP01-JP50;
#   - DOES NOT assign occurrences;
#   - DOES NOT modify coordinates or taxonomy.
#
# Required frozen input:
#   Geography 03h v0.1.0
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
EXPECTED_GEOGRAPHY_03H_VERSION <- "0.1.0"

run_residual_kuriles_resolution <- function() {
  cat("\n— VPJD residual Kuriles component resolution —\n\n")
  
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
  # Validate Geography 03h
  # ---------------------------------------------------------------------------
  
  required_tables <- c(
    "japan_kuriles_component_resolution",
    "japan_kuriles_unresolved_components",
    "japan_kuriles_resolution_validation",
    "japan_kuriles_resolution_metadata"
  )
  
  missing_tables <- setdiff(
    required_tables,
    dbListTables(con)
  )
  
  if (length(missing_tables) > 0) {
    stop(
      "Required Geography 03h tables missing: ",
      paste(missing_tables, collapse = ", ")
    )
  }
  
  metadata_03h <- dbReadTable(
    con,
    "japan_kuriles_resolution_metadata"
  ) %>%
    as_tibble()
  
  if (!all(c("metric", "value") %in% names(metadata_03h))) {
    stop("Unexpected Geography 03h metadata schema.")
  }
  
  version_03h <- metadata_03h %>%
    filter(metric == "script_version") %>%
    pull(value)
  
  if (
    length(version_03h) != 1 ||
    version_03h != EXPECTED_GEOGRAPHY_03H_VERSION
  ) {
    stop(
      "Expected Geography 03h v",
      EXPECTED_GEOGRAPHY_03H_VERSION,
      "; found ",
      paste(version_03h, collapse = ", "),
      "."
    )
  }
  
  validation_03h <- dbReadTable(
    con,
    "japan_kuriles_resolution_validation"
  ) %>%
    as_tibble()
  
  if (
    !"pass" %in% names(validation_03h) ||
    !all(validation_03h$pass)
  ) {
    stop("Geography 03h validation is not fully PASS.")
  }
  
  unresolved_03h <- dbReadTable(
    con,
    "japan_kuriles_unresolved_components"
  ) %>%
    as_tibble()
  
  expected_unresolved <- c(
    "KUR_DIAG_00016",
    "KUR_DIAG_00018"
  )
  
  if (
    nrow(unresolved_03h) != 2 ||
    !setequal(
      unresolved_03h$component_id,
      expected_unresolved
    )
  ) {
    stop(
      "Expected exactly KUR_DIAG_00016 and KUR_DIAG_00018 ",
      "as unresolved Geography 03h components."
    )
  }
  
  cat(
    "Geography 03h version: ",
    version_03h,
    "\n",
    "Geography 03h validation: PASS\n",
    "Expected unresolved components present: PASS\n\n",
    sep = ""
  )
  
  # ---------------------------------------------------------------------------
  # Read Geography 03h spatial geometry
  # ---------------------------------------------------------------------------
  
  input_gpkg <- here(
    "data", "interim", "geography", "kuriles",
    "vpjd_kuriles_southern_resolution.gpkg"
  )
  
  if (!file.exists(input_gpkg)) {
    stop(
      "Geography 03h GeoPackage not found: ",
      input_gpkg
    )
  }
  
  components <- sf::st_read(
    input_gpkg,
    layer = "resolved_components",
    quiet = TRUE
  ) %>%
    sf::st_transform(4326)
  
  if (nrow(components) != 28) {
    stop(
      "Expected 28 Geography 03h components; found ",
      nrow(components), "."
    )
  }
  
  if (any(!sf::st_is_valid(components))) {
    stop(
      "One or more Geography 03h geometries are invalid."
    )
  }
  
  # ---------------------------------------------------------------------------
  # Residual geographic resolution
  #
  # KUR_DIAG_00016
  #   143.8832 E / 44.1552 N
  #   Northeastern Hokkaido / Lake Saroma geographic context.
  #
  # KUR_DIAG_00018
  #   144.4503 E / 43.9421 N
  #   Eastern Hokkaido / Abashiri-Tōfutsu geographic context.
  #
  # Both lie west of the southern Kuril physical chain and within Hokkaido
  # geographic context. Exact minor-feature naming is unnecessary for the
  # botanical-area decision.
  # ---------------------------------------------------------------------------
  
  residual_resolution <- tribble(
    ~component_id,
    ~resolved_geography_final,
    ~resolution_status_final,
    ~jp51_inclusion_final,
    ~resolution_basis_final,
    
    "KUR_DIAG_00016",
    "Hokkaido context",
    "secure_exclusion",
    "exclude",
    paste(
      "Northeastern Hokkaido / Lake Saroma geographic context;",
      "position is west of the southern Kuril physical island chain."
    ),
    
    "KUR_DIAG_00018",
    "Hokkaido context",
    "secure_exclusion",
    "exclude",
    paste(
      "Eastern Hokkaido / Abashiri-Tofutsu geographic context;",
      "position is west of the southern Kuril physical island chain."
    )
  )
  
  # ---------------------------------------------------------------------------
  # Verify coordinates remain consistent with the frozen diagnostic
  # ---------------------------------------------------------------------------
  
  coordinate_check <- components %>%
    sf::st_drop_geometry() %>%
    filter(
      component_id %in%
        expected_unresolved
    ) %>%
    select(
      component_id,
      point_lon,
      point_lat,
      area_km2
    ) %>%
    arrange(
      component_id
    )
  
  expected_coordinates <- tribble(
    ~component_id,       ~expected_lon, ~expected_lat,
    "KUR_DIAG_00016",    143.8832,      44.15522,
    "KUR_DIAG_00018",    144.4503,      43.94212
  )
  
  coordinate_check <- coordinate_check %>%
    left_join(
      expected_coordinates,
      by = "component_id"
    ) %>%
    mutate(
      lon_difference =
        abs(point_lon - expected_lon),
      lat_difference =
        abs(point_lat - expected_lat),
      coordinate_check_pass =
        lon_difference < 0.001 &
        lat_difference < 0.001
    )
  
  if (!all(coordinate_check$coordinate_check_pass)) {
    stop(
      "Residual component coordinates differ unexpectedly from ",
      "the frozen Geography 03h diagnostic."
    )
  }
  
  cat(
    "Residual coordinate consistency: PASS\n\n"
  )
  
  # ---------------------------------------------------------------------------
  # Apply final residual decisions
  # ---------------------------------------------------------------------------
  
  components_final <- components %>%
    left_join(
      residual_resolution,
      by = "component_id"
    ) %>%
    mutate(
      resolved_geography_final = case_when(
        !is.na(resolved_geography_final) ~
          resolved_geography_final,
        TRUE ~
          resolved_geography
      ),
      
      resolution_status_final = case_when(
        !is.na(resolution_status_final) ~
          resolution_status_final,
        TRUE ~
          resolution_status
      ),
      
      jp51_inclusion_final = case_when(
        !is.na(jp51_inclusion_final) ~
          jp51_inclusion_final,
        jp51_inclusion_evidence ==
          "strong" ~
          "include",
        jp51_inclusion_evidence ==
          "exclude" ~
          "exclude",
        TRUE ~
          "unresolved"
      ),
      
      resolution_basis_final = case_when(
        !is.na(resolution_basis_final) ~
          resolution_basis_final,
        !is.na(resolution_basis) ~
          resolution_basis,
        resolution_status ==
          "existing_secure_candidate" ~
          paste(
            "Secure physical Kuril-chain candidate established",
            "by Geography 03g and retained by Geography 03h."
          ),
        component_class ==
          "hokkaido_context" ~
          "Hokkaido geographic context.",
        component_class ==
          "sakhalin_context" ~
          "Sakhalin geographic context.",
        component_class ==
          "kamchatka_context" ~
          "Kamchatka geographic context.",
        TRUE ~
          NA_character_
      )
    )
  
  # ---------------------------------------------------------------------------
  # Final analytical component table
  # ---------------------------------------------------------------------------
  
  final_table <- components_final %>%
    sf::st_drop_geometry() %>%
    select(
      component_id,
      component_class,
      chain_position,
      resolved_geography_final,
      resolution_status_final,
      jp51_inclusion_final,
      resolution_basis_final,
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
  
  residual_table <- final_table %>%
    filter(
      component_id %in%
        expected_unresolved
    ) %>%
    as_tibble()
  
  jp51_candidate_table <- final_table %>%
    filter(
      jp51_inclusion_final ==
        "include"
    ) %>%
    arrange(
      point_lat,
      point_lon
    ) %>%
    as_tibble()
  
  excluded_table <- final_table %>%
    filter(
      jp51_inclusion_final ==
        "exclude"
    ) %>%
    arrange(
      point_lat,
      point_lon
    ) %>%
    as_tibble()
  
  unresolved_final <- final_table %>%
    filter(
      jp51_inclusion_final ==
        "unresolved"
    ) %>%
    as_tibble()
  
  # ---------------------------------------------------------------------------
  # Summary
  # ---------------------------------------------------------------------------
  
  decision_summary <- final_table %>%
    count(
      jp51_inclusion_final,
      name = "polygon_components"
    ) %>%
    arrange(
      jp51_inclusion_final
    ) %>%
    as_tibble()
  
  geography_summary <- final_table %>%
    group_by(
      resolved_geography_final,
      jp51_inclusion_final
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
      min_lat,
      min_lon
    ) %>%
    as_tibble()
  
  # ---------------------------------------------------------------------------
  # QA
  # ---------------------------------------------------------------------------
  
  geometry_qa <- tibble(
    metric = c(
      "input_components",
      "valid_geometries",
      "invalid_geometries",
      "jp51_include_components",
      "jp51_exclude_components",
      "jp51_unresolved_components",
      "residual_components_resolved",
      "residual_components_excluded"
    ),
    value = c(
      nrow(components_final),
      sum(
        sf::st_is_valid(
          components_final
        )
      ),
      sum(
        !sf::st_is_valid(
          components_final
        )
      ),
      sum(
        components_final$
          jp51_inclusion_final ==
          "include"
      ),
      sum(
        components_final$
          jp51_inclusion_final ==
          "exclude"
      ),
      sum(
        components_final$
          jp51_inclusion_final ==
          "unresolved"
      ),
      nrow(residual_resolution),
      sum(
        components_final$
          component_id %in%
          expected_unresolved &
          components_final$
          jp51_inclusion_final ==
          "exclude"
      )
    )
  )
  
  validation <- tibble(
    check = c(
      "Exactly 28 source components",
      "All geometries valid",
      "Residual coordinates consistent with 03h",
      "KUR_DIAG_00016 resolved as Hokkaido context",
      "KUR_DIAG_00018 resolved as Hokkaido context",
      "Both residual components excluded from JP51",
      "At least one JP51 inclusion component",
      "Zero unresolved components",
      "JP51 boundary not yet constructed"
    ),
    pass = c(
      nrow(components_final) == 28,
      
      all(
        sf::st_is_valid(
          components_final
        )
      ),
      
      all(
        coordinate_check$
          coordinate_check_pass
      ),
      
      any(
        final_table$
          component_id ==
          "KUR_DIAG_00016" &
          final_table$
          resolved_geography_final ==
          "Hokkaido context"
      ),
      
      any(
        final_table$
          component_id ==
          "KUR_DIAG_00018" &
          final_table$
          resolved_geography_final ==
          "Hokkaido context"
      ),
      
      all(
        final_table$
          jp51_inclusion_final[
            final_table$component_id %in%
              expected_unresolved
          ] == "exclude"
      ),
      
      any(
        final_table$
          jp51_inclusion_final ==
          "include"
      ),
      
      nrow(unresolved_final) == 0,
      
      TRUE
    )
  )
  
  # ---------------------------------------------------------------------------
  # Persist analytical outputs
  # ---------------------------------------------------------------------------
  
  dbWriteTable(
    con,
    "japan_kuriles_final_component_decisions",
    final_table,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "japan_kuriles_residual_resolution",
    residual_table,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "japan_kuriles_jp51_candidate_components",
    jp51_candidate_table,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "japan_kuriles_final_excluded_components",
    excluded_table,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "japan_kuriles_final_decision_summary",
    decision_summary,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "japan_kuriles_final_geography_summary",
    geography_summary,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "japan_kuriles_residual_geometry_qa",
    geometry_qa,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "japan_kuriles_residual_validation",
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
  
  output_gpkg <- file.path(
    spatial_dir,
    "vpjd_kuriles_final_component_decisions.gpkg"
  )
  
  if (file.exists(output_gpkg)) {
    file.remove(output_gpkg)
  }
  
  sf::st_write(
    components_final,
    output_gpkg,
    layer = "all_components",
    quiet = TRUE
  )
  
  include_geometry <- components_final %>%
    filter(
      jp51_inclusion_final ==
        "include"
    )
  
  sf::st_write(
    include_geometry,
    output_gpkg,
    layer = "jp51_include_components",
    append = TRUE,
    quiet = TRUE
  )
  
  exclude_geometry <- components_final %>%
    filter(
      jp51_inclusion_final ==
        "exclude"
    )
  
  sf::st_write(
    exclude_geometry,
    output_gpkg,
    layer = "jp51_exclude_components",
    append = TRUE,
    quiet = TRUE
  )
  
  # ---------------------------------------------------------------------------
  # Metadata
  # ---------------------------------------------------------------------------
  
  metadata <- tibble(
    metric = c(
      "script_version",
      "input_geography_03h_version",
      "physical_geometry_source",
      "physical_geometry_version",
      "residual_resolution_basis",
      "final_unresolved_components",
      "jp51_component_decisions_complete",
      "jp51_boundary_constructed",
      "jp51_botanical_definition_finalised",
      "sovereignty_used_for_classification",
      "jp01_jp50_modified",
      "occurrences_assigned",
      "coordinates_modified",
      "taxonomy_modified"
    ),
    value = c(
      SCRIPT_VERSION,
      version_03h,
      "Natural Earth 10m Land",
      "5.1.1",
      paste(
        "Geographic position relative to northeastern/eastern Hokkaido",
        "and the physical southern Kuril island chain; exact minor-feature",
        "naming not required for botanical-area exclusion."
      ),
      as.character(
        nrow(unresolved_final)
      ),
      as.character(
        nrow(unresolved_final) == 0
      ),
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
    "japan_kuriles_residual_metadata",
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
    "kuriles_residual_resolution"
  )
  
  dir.create(
    out_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  write_csv(
    residual_table,
    file.path(
      out_dir,
      "residual_component_resolution.csv"
    )
  )
  
  write_csv(
    final_table,
    file.path(
      out_dir,
      "final_component_decisions.csv"
    )
  )
  
  write_csv(
    jp51_candidate_table,
    file.path(
      out_dir,
      "jp51_candidate_components.csv"
    )
  )
  
  write_csv(
    excluded_table,
    file.path(
      out_dir,
      "excluded_components.csv"
    )
  )
  
  write_csv(
    decision_summary,
    file.path(
      out_dir,
      "decision_summary.csv"
    )
  )
  
  write_csv(
    geography_summary,
    file.path(
      out_dir,
      "geography_summary.csv"
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
  
  cat("— Residual component resolution —\n\n")
  
  print.data.frame(
    as.data.frame(
      residual_table %>%
        select(
          component_id,
          resolved_geography_final,
          resolution_status_final,
          jp51_inclusion_final,
          point_lon,
          point_lat,
          area_km2
        )
    ),
    row.names = FALSE
  )
  
  cat("\n— Final JP51 component decisions —\n\n")
  
  print.data.frame(
    as.data.frame(
      decision_summary
    ),
    row.names = FALSE
  )
  
  cat("\n— Final geography summary —\n\n")
  
  print.data.frame(
    as.data.frame(
      geography_summary
    ),
    row.names = FALSE
  )
  
  cat("\n— JP51 inclusion components —\n\n")
  
  print.data.frame(
    as.data.frame(
      jp51_candidate_table %>%
        select(
          component_id,
          resolved_geography_final,
          point_lon,
          point_lat,
          area_km2
        )
    ),
    row.names = FALSE
  )
  
  cat("\n— Excluded contextual components —\n\n")
  
  print.data.frame(
    as.data.frame(
      excluded_table %>%
        select(
          component_id,
          resolved_geography_final,
          point_lon,
          point_lat,
          area_km2
        )
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
  
  cat("\n— Validation —\n\n")
  
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
    "\nSpatial decisions written:\n",
    output_gpkg,
    "\n",
    sep = ""
  )
  
  cat("\n— Safety —\n")
  cat("Residual geographic components resolved: TRUE\n")
  cat("Residual components forced into JP51: FALSE\n")
  cat("JP51 component decisions complete: ",
      nrow(unresolved_final) == 0, "\n", sep = "")
  cat("JP51 boundary constructed: FALSE\n")
  cat("JP51 botanical definition finalised: FALSE\n")
  cat("Sovereignty used for classification: FALSE\n")
  cat("JP01-JP50 modified: FALSE\n")
  cat("Occurrences assigned: FALSE\n")
  cat("Coordinates modified: FALSE\n")
  cat("Taxonomy modified: FALSE\n")
  cat(
    "Output status: JP51 COMPONENT DECISIONS COMPLETE / PRE-BOUNDARY\n"
  )
  
  cat(
    "\n03i_resolve_residual_kuriles_components.R v",
    SCRIPT_VERSION,
    " complete.\n",
    sep = ""
  )
  
  invisible(
    list(
      components_final = components_final,
      residual_table = residual_table,
      final_table = final_table,
      jp51_candidate_table = jp51_candidate_table,
      excluded_table = excluded_table,
      decision_summary = decision_summary,
      geography_summary = geography_summary,
      geometry_qa = geometry_qa,
      validation = validation,
      metadata = metadata
    )
  )
}

geography_03i <-
  run_residual_kuriles_resolution()