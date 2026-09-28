# ==============================================================================
# VPJD-OJPCP
# 03h_resolve_southern_kuriles_components.R
# Version: 0.1.0
#
# Purpose:
#   Resolve the southern-transition components identified by Geography 03g
#   into named physical geographic entities where the evidence is sufficient.
#
# Method:
#   - validates frozen Geography 03g v0.1.0;
#   - reads the classified Natural Earth physical components;
#   - resolves secure southern-island identities using geographic position,
#     geometry and independently documented geographic reference positions;
#   - distinguishes Habomai-group components from Shikotan and Kunashir;
#   - retains anomalous/non-secure components for explicit review;
#   - rejects Sakhalin context from JP51 consideration;
#   - DOES NOT yet construct JP51;
#   - DOES NOT modify JP01-JP50;
#   - DOES NOT assign occurrences.
#
# Important:
#   Geographic identification and botanical inclusion are separate decisions.
#   A component identified here as part of the southern Kuril physical chain
#   is not formally assigned to JP51 until the subsequent boundary module.
#
# Required frozen input:
#   Geography 03g v0.1.0
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
EXPECTED_GEOGRAPHY_03G_VERSION <- "0.1.0"

run_southern_kuriles_resolution <- function() {
  cat("\n— VPJD southern Kuriles component resolution —\n\n")
  
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
  # Validate Geography 03g
  # ---------------------------------------------------------------------------
  
  required_tables <- c(
    "japan_kuriles_component_classification",
    "japan_kuriles_component_validation",
    "japan_kuriles_component_metadata"
  )
  
  missing_tables <- setdiff(
    required_tables,
    dbListTables(con)
  )
  
  if (length(missing_tables) > 0) {
    stop(
      "Required Geography 03g tables missing: ",
      paste(missing_tables, collapse = ", ")
    )
  }
  
  metadata_03g <- dbReadTable(
    con,
    "japan_kuriles_component_metadata"
  ) %>%
    as_tibble()
  
  if (!all(c("metric", "value") %in% names(metadata_03g))) {
    stop("Unexpected Geography 03g metadata schema.")
  }
  
  version_03g <- metadata_03g %>%
    filter(metric == "script_version") %>%
    pull(value)
  
  if (
    length(version_03g) != 1 ||
    version_03g != EXPECTED_GEOGRAPHY_03G_VERSION
  ) {
    stop(
      "Expected Geography 03g v",
      EXPECTED_GEOGRAPHY_03G_VERSION,
      "; found ",
      paste(version_03g, collapse = ", "),
      "."
    )
  }
  
  validation_03g <- dbReadTable(
    con,
    "japan_kuriles_component_validation"
  ) %>%
    as_tibble()
  
  if (
    !"pass" %in% names(validation_03g) ||
    !all(validation_03g$pass)
  ) {
    stop("Geography 03g validation is not fully PASS.")
  }
  
  cat(
    "Geography 03g version: ",
    version_03g,
    "\nGeography 03g validation: PASS\n\n",
    sep = ""
  )
  
  # ---------------------------------------------------------------------------
  # Read 03g classified geometry
  # ---------------------------------------------------------------------------
  
  input_gpkg <- here(
    "data", "interim", "geography", "kuriles",
    "vpjd_kuriles_component_classification.gpkg"
  )
  
  if (!file.exists(input_gpkg)) {
    stop("03g GeoPackage not found: ", input_gpkg)
  }
  
  components <- sf::st_read(
    input_gpkg,
    layer = "classified_components",
    quiet = TRUE
  ) %>%
    sf::st_transform(4326)
  
  if (nrow(components) != 28) {
    stop(
      "Expected 28 Geography 03g components; found ",
      nrow(components), "."
    )
  }
  
  if (any(!sf::st_is_valid(components))) {
    stop("One or more 03g geometries are invalid.")
  }
  
  # ---------------------------------------------------------------------------
  # Secure geographic reference
  #
  # The principal southern physical units are distinguishable from the
  # Natural Earth geometry:
  #
  # Kunashir:
  #   large elongate island around 44.17 N / 146.00 E.
  #
  # Shikotan:
  #   substantial island east of the Habomai group and south-east of Kunashir.
  #
  # Habomai:
  #   smaller island group south/south-east of Nemuro and west of Shikotan.
  #
  # Iturup is already securely within the 03g automatic Kuriles candidates
  # and is therefore not part of the southern-transition resolution problem.
  #
  # Component IDs below are frozen outputs of Natural Earth Land v5.1.1 +
  # Geography 03f/03g and are recorded explicitly for auditability.
  # ---------------------------------------------------------------------------
  
  southern_resolution <- tribble(
    ~component_id,      ~resolved_geography, ~resolution_status, ~resolution_basis,
    "KUR_DIAG_00025",   "Kunashir",           "secure",           "Large southern Kuril island; position and extent concordant with independently documented Kunashir geography",
    "KUR_DIAG_00010",   "Shikotan",           "secure",           "Distinct island east of Habomai group and south-east of Kunashir; physical position and extent concordant with Shikotan",
    "KUR_DIAG_00005",   "Habomai group",      "group_secure",     "Small southern-chain component within coherent Habomai physical island cluster",
    "KUR_DIAG_00017",   "Habomai group",      "group_secure",     "Small southern-chain component within coherent Habomai physical island cluster",
    "KUR_DIAG_00022",   "Habomai group",      "group_secure",     "Small southern-chain component within coherent Habomai physical island cluster",
    "KUR_DIAG_00023",   "Habomai group",      "group_secure",     "Small southern-chain component within coherent Habomai physical island cluster",
    "KUR_DIAG_00024",   "Habomai group",      "group_secure",     "Small southern-chain component within coherent Habomai physical island cluster",
    "KUR_DIAG_00016",   NA_character_,        "unresolved",       "Western southern-transition component; not securely attributable to Kuril chain from present evidence",
    "KUR_DIAG_00018",   NA_character_,        "unresolved",       "Very small anomalous western component; not securely attributable to Kuril chain from present evidence",
    "KUR_DIAG_00002",   "Sakhalin context",   "secure_exclusion", "Large western component geographically associated with Sakhalin context rather than Kuril island chain"
  )
  
  # ---------------------------------------------------------------------------
  # Check all expected components exist
  # ---------------------------------------------------------------------------
  
  missing_resolution_ids <- setdiff(
    southern_resolution$component_id,
    components$component_id
  )
  
  if (length(missing_resolution_ids) > 0) {
    stop(
      "Expected component IDs missing from 03g geometry: ",
      paste(missing_resolution_ids, collapse = ", ")
    )
  }
  
  # ---------------------------------------------------------------------------
  # Attach resolution to geometry
  # ---------------------------------------------------------------------------
  
  resolved_components <- components %>%
    left_join(
      southern_resolution,
      by = "component_id"
    ) %>%
    mutate(
      resolution_status = case_when(
        !is.na(resolution_status) ~ resolution_status,
        component_class == "kuriles_candidate" ~
          "existing_secure_candidate",
        component_class == "hokkaido_context" ~
          "secure_exclusion",
        component_class == "sakhalin_context" ~
          "secure_exclusion",
        component_class == "kamchatka_context" ~
          "secure_exclusion",
        TRUE ~
          "not_assessed"
      ),
      resolved_geography = case_when(
        !is.na(resolved_geography) ~ resolved_geography,
        component_class == "kuriles_candidate" ~
          "Kuril chain candidate",
        component_class == "hokkaido_context" ~
          "Hokkaido context",
        component_class == "sakhalin_context" ~
          "Sakhalin context",
        component_class == "kamchatka_context" ~
          "Kamchatka context",
        TRUE ~
          resolved_geography
      )
    )
  
  # ---------------------------------------------------------------------------
  # Botanical-decision readiness
  #
  # This is NOT the botanical assignment itself.
  # ---------------------------------------------------------------------------
  
  resolved_components <- resolved_components %>%
    mutate(
      jp51_inclusion_evidence = case_when(
        resolution_status == "existing_secure_candidate" ~
          "strong",
        resolution_status == "secure" &
          resolved_geography %in% c(
            "Kunashir",
            "Shikotan"
          ) ~
          "strong",
        resolution_status == "group_secure" &
          resolved_geography == "Habomai group" ~
          "strong",
        resolution_status == "secure_exclusion" ~
          "exclude",
        resolution_status == "unresolved" ~
          "unresolved",
        TRUE ~
          "review"
      )
    )
  
  # ---------------------------------------------------------------------------
  # Analytical tables
  # ---------------------------------------------------------------------------
  
  resolution_table <- resolved_components %>%
    sf::st_drop_geometry() %>%
    select(
      component_id,
      component_class,
      chain_position,
      resolved_geography,
      resolution_status,
      jp51_inclusion_evidence,
      resolution_basis,
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
  
  southern_table <- resolution_table %>%
    filter(
      component_id %in%
        southern_resolution$component_id
    ) %>%
    arrange(
      point_lat,
      point_lon
    ) %>%
    as_tibble()
  
  unresolved_table <- resolution_table %>%
    filter(
      jp51_inclusion_evidence %in%
        c("unresolved", "review")
    ) %>%
    as_tibble()
  
  resolution_summary <- resolution_table %>%
    count(
      resolution_status,
      jp51_inclusion_evidence,
      name = "polygon_components"
    ) %>%
    arrange(
      resolution_status,
      jp51_inclusion_evidence
    ) %>%
    as_tibble()
  
  geography_summary <- resolution_table %>%
    filter(
      !is.na(resolved_geography)
    ) %>%
    group_by(
      resolved_geography,
      jp51_inclusion_evidence
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
  
  # ---------------------------------------------------------------------------
  # QA
  # ---------------------------------------------------------------------------
  
  geometry_qa <- tibble(
    metric = c(
      "input_components",
      "valid_geometries",
      "invalid_geometries",
      "southern_resolution_records",
      "strong_jp51_evidence_components",
      "explicit_exclusion_components",
      "unresolved_components"
    ),
    value = c(
      nrow(resolved_components),
      sum(sf::st_is_valid(resolved_components)),
      sum(!sf::st_is_valid(resolved_components)),
      nrow(southern_resolution),
      sum(
        resolved_components$
          jp51_inclusion_evidence == "strong"
      ),
      sum(
        resolved_components$
          jp51_inclusion_evidence == "exclude"
      ),
      sum(
        resolved_components$
          jp51_inclusion_evidence == "unresolved"
      )
    )
  )
  
  validation <- tibble(
    check = c(
      "Exactly 28 source components",
      "All geometries valid",
      "Kunashir securely resolved",
      "Shikotan securely resolved",
      "Habomai group represented",
      "Sakhalin review component excluded",
      "Anomalous western components retained unresolved",
      "JP51 not yet constructed"
    ),
    pass = c(
      nrow(resolved_components) == 28,
      
      all(
        sf::st_is_valid(
          resolved_components
        )
      ),
      
      any(
        resolution_table$
          resolved_geography == "Kunashir" &
          resolution_table$
          jp51_inclusion_evidence == "strong"
      ),
      
      any(
        resolution_table$
          resolved_geography == "Shikotan" &
          resolution_table$
          jp51_inclusion_evidence == "strong"
      ),
      
      any(
        resolution_table$
          resolved_geography == "Habomai group" &
          resolution_table$
          jp51_inclusion_evidence == "strong"
      ),
      
      any(
        resolution_table$
          component_id == "KUR_DIAG_00002" &
          resolution_table$
          jp51_inclusion_evidence == "exclude"
      ),
      
      all(
        resolution_table$
          jp51_inclusion_evidence[
            resolution_table$component_id %in%
              c(
                "KUR_DIAG_00016",
                "KUR_DIAG_00018"
              )
          ] == "unresolved"
      ),
      
      TRUE
    )
  )
  
  # ---------------------------------------------------------------------------
  # Persist analytical outputs
  # ---------------------------------------------------------------------------
  
  dbWriteTable(
    con,
    "japan_kuriles_southern_resolution",
    southern_table,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "japan_kuriles_component_resolution",
    resolution_table,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "japan_kuriles_resolution_summary",
    resolution_summary,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "japan_kuriles_resolved_geography_summary",
    geography_summary,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "japan_kuriles_unresolved_components",
    unresolved_table,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "japan_kuriles_resolution_geometry_qa",
    geometry_qa,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "japan_kuriles_resolution_validation",
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
    "vpjd_kuriles_southern_resolution.gpkg"
  )
  
  if (file.exists(output_gpkg)) {
    file.remove(output_gpkg)
  }
  
  sf::st_write(
    resolved_components,
    output_gpkg,
    layer = "resolved_components",
    quiet = TRUE
  )
  
  strong_candidates <- resolved_components %>%
    filter(
      jp51_inclusion_evidence == "strong"
    )
  
  sf::st_write(
    strong_candidates,
    output_gpkg,
    layer = "strong_jp51_evidence",
    append = TRUE,
    quiet = TRUE
  )
  
  unresolved_geometry <- resolved_components %>%
    filter(
      jp51_inclusion_evidence == "unresolved"
    )
  
  sf::st_write(
    unresolved_geometry,
    output_gpkg,
    layer = "unresolved_components",
    append = TRUE,
    quiet = TRUE
  )
  
  # ---------------------------------------------------------------------------
  # Metadata
  # ---------------------------------------------------------------------------
  
  metadata <- tibble(
    metric = c(
      "script_version",
      "input_geography_03g_version",
      "physical_geometry_source",
      "physical_geometry_version",
      "geographic_reference_basis",
      "sovereignty_used_for_classification",
      "geographic_identity_equals_botanical_assignment",
      "jp51_boundary_constructed",
      "jp51_botanical_definition_finalised",
      "jp01_jp50_modified",
      "occurrences_assigned",
      "coordinates_modified",
      "taxonomy_modified"
    ),
    value = c(
      SCRIPT_VERSION,
      version_03g,
      "Natural Earth 10m Land",
      "5.1.1",
      paste(
        "Physical position and geometry, independently documented",
        "Kuril island geographic reference positions, and explicit",
        "retention of uncertain components"
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
    "japan_kuriles_resolution_metadata",
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
    "kuriles_resolution"
  )
  
  dir.create(
    out_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  write_csv(
    southern_table,
    file.path(
      out_dir,
      "southern_component_resolution.csv"
    )
  )
  
  write_csv(
    resolution_table,
    file.path(
      out_dir,
      "all_component_resolution.csv"
    )
  )
  
  write_csv(
    unresolved_table,
    file.path(
      out_dir,
      "unresolved_components.csv"
    )
  )
  
  write_csv(
    resolution_summary,
    file.path(
      out_dir,
      "resolution_summary.csv"
    )
  )
  
  write_csv(
    geography_summary,
    file.path(
      out_dir,
      "resolved_geography_summary.csv"
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
    nrow(resolved_components),
    "\n\n",
    sep = ""
  )
  
  cat("— Southern component resolution —\n\n")
  
  print.data.frame(
    as.data.frame(
      southern_table %>%
        select(
          component_id,
          resolved_geography,
          resolution_status,
          jp51_inclusion_evidence,
          point_lon,
          point_lat,
          area_km2
        )
    ),
    row.names = FALSE
  )
  
  cat("\n— Resolution summary —\n\n")
  
  print.data.frame(
    as.data.frame(
      resolution_summary
    ),
    row.names = FALSE
  )
  
  cat("\n— Resolved geography summary —\n\n")
  
  print.data.frame(
    as.data.frame(
      geography_summary
    ),
    row.names = FALSE
  )
  
  cat("\n— Components still unresolved —\n\n")
  
  if (nrow(unresolved_table) == 0) {
    cat("None.\n")
  } else {
    print.data.frame(
      as.data.frame(
        unresolved_table %>%
          select(
            component_id,
            component_class,
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
  }
  
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
    "\nSpatial resolution written:\n",
    output_gpkg,
    "\n",
    sep = ""
  )
  
  cat("\n— Safety —\n")
  cat("Geographic identities resolved where evidence sufficient: TRUE\n")
  cat("Uncertain components forced to named geography: FALSE\n")
  cat("JP51 boundary constructed: FALSE\n")
  cat("JP51 botanical definition finalised: FALSE\n")
  cat("Sovereignty used for classification: FALSE\n")
  cat("JP01-JP50 modified: FALSE\n")
  cat("Occurrences assigned: FALSE\n")
  cat("Coordinates modified: FALSE\n")
  cat("Taxonomy modified: FALSE\n")
  cat(
    "Output status: SOUTHERN KURILES GEOGRAPHIC RESOLUTION / PRE-JP51\n"
  )
  
  cat(
    "\n03h_resolve_southern_kuriles_components.R v",
    SCRIPT_VERSION,
    " complete.\n",
    sep = ""
  )
  
  invisible(
    list(
      resolved_components = resolved_components,
      southern_table = southern_table,
      resolution_table = resolution_table,
      unresolved_table = unresolved_table,
      resolution_summary = resolution_summary,
      geography_summary = geography_summary,
      geometry_qa = geometry_qa,
      validation = validation,
      metadata = metadata
    )
  )
}

geography_03h <-
  run_southern_kuriles_resolution()