# ==============================================================================
# VPJD-OJPCP
# 03e_build_nlni_botanical_boundaries.R
# Version: 0.1.1
#
# Purpose:
#   Construct the canonical N03-derived spatial botanical-area framework for
#   JP01-JP50.
#
# Botanical framework:
#   JP01-JP46 = political-prefecture botanical areas, modified where special
#               botanical island groups supersede political administration.
#   JP47       = Ryukyu Islands = Okinawa + Amami component of Kagoshima.
#   JP48       = Izu Islands.
#   JP49       = Ogasawara Islands proper.
#   JP50       = Kazan Islands proper.
#   JP51       = Kuriles: NOT constructed here.
#
# Explicit exclusions:
#   Okinotorishima and Minamitorishima are retained separately and are not
#   silently assigned to JP50.
#
# This module:
#   - validates all required upstream schemas before expensive spatial work;
#   - creates canonical JP01-JP50 botanical geometries;
#   - saves the canonical geometry before downstream topology QA;
#   - validates geometry, IDs and positive-area overlap;
#   - preserves excluded remote Tokyo geometries;
#   - does NOT assign occurrences;
#   - does NOT construct JP51 Kuriles.
#
# Required frozen inputs:
#   Geography 02  v0.1.0
#   Geography 03a v0.1.0
#   Geography 03b v0.1.1
#   Geography 03c v0.1.0
#   Geography 03d v0.1.0
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
EXPECTED_GEOGRAPHY_02_VERSION <- "0.1.0"
EXPECTED_GEOGRAPHY_03A_VERSION <- "0.1.0"
EXPECTED_GEOGRAPHY_03B_VERSION <- "0.1.1"
EXPECTED_GEOGRAPHY_03C_VERSION <- "0.1.0"
EXPECTED_GEOGRAPHY_03D_VERSION <- "0.1.0"

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
  x <- dbGetQuery(con, sql, params = list(metric_name))
  if (nrow(x) != 1) {
    stop(
      "Expected one metadata row for ",
      metric_name, " in ", table_name, "."
    )
  }
  as.character(x$value[[1]])
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

run_botanical_boundary_build <- function() {
  cat("\n— VPJD N03 botanical-boundary construction —\n\n")
  
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
  # Validate upstream tables and versions BEFORE expensive spatial processing
  # ---------------------------------------------------------------------------
  
  required_tables <- c(
    "japan_botanical_framework_metadata",
    "japan_botanical_areas",
    "japan_nlni_n03_acquisition_metadata",
    "japan_nlni_n03_inspection_metadata",
    "japan_nlni_special_component_metadata",
    "japan_ogasawara_kazan_validation_metadata"
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
  g03d <- get_metadata_value(
    con,
    "japan_ogasawara_kazan_validation_metadata",
    "script_version"
  )
  
  observed <- c(
    g02 = g02,
    g03a = g03a,
    g03b = g03b,
    g03c = g03c,
    g03d = g03d
  )
  
  expected <- c(
    g02 = EXPECTED_GEOGRAPHY_02_VERSION,
    g03a = EXPECTED_GEOGRAPHY_03A_VERSION,
    g03b = EXPECTED_GEOGRAPHY_03B_VERSION,
    g03c = EXPECTED_GEOGRAPHY_03C_VERSION,
    g03d = EXPECTED_GEOGRAPHY_03D_VERSION
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
    "Geography 03c version: ", g03c, "\n",
    "Geography 03d version: ", g03d, "\n\n",
    sep = ""
  )
  
  # ---------------------------------------------------------------------------
  # Validate botanical reference framework schema BEFORE spatial processing
  # ---------------------------------------------------------------------------
  
  framework <- dbReadTable(
    con,
    "japan_botanical_areas"
  ) %>%
    as_tibble()
  
  required_framework_fields <- c(
    "area_id",
    "analytical_area_name"
  )
  
  missing_framework_fields <- setdiff(
    required_framework_fields,
    names(framework)
  )
  
  if (length(missing_framework_fields) > 0) {
    stop(
      "Required botanical framework fields missing: ",
      paste(
        missing_framework_fields,
        collapse = ", "
      )
    )
  }
  
  framework_01_50 <- framework %>%
    filter(
      area_id %in%
        sprintf("JP%02d", 1:50)
    )
  
  if (
    nrow(framework_01_50) != 50 ||
    n_distinct(
      framework_01_50$area_id
    ) != 50
  ) {
    stop(
      "Expected exactly 50 framework rows for JP01-JP50."
    )
  }
  
  framework_min <- framework_01_50 %>%
    transmute(
      botanical_area_id = area_id,
      botanical_area_name =
        analytical_area_name
    )
  
  if (
    any(
      is.na(
        framework_min$
        botanical_area_name
      )
    ) ||
    any(
      trimws(
        framework_min$
        botanical_area_name
      ) == ""
    )
  ) {
    stop(
      "One or more JP01-JP50 analytical area names are missing."
    )
  }
  
  cat(
    "Botanical framework schema: PASS\n",
    "JP01-JP50 reference rows: 50\n\n",
    sep = ""
  )
  
  # ---------------------------------------------------------------------------
  # Locate 47 main N03 layers
  # ---------------------------------------------------------------------------
  
  extract_root <- here(
    "data", "interim", "geography",
    "nlni_n03_2025"
  )
  
  shp_files <- list.files(
    extract_root,
    pattern = "\\.shp$",
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = TRUE
  )
  
  main_shp <- shp_files[
    !grepl(
      "_subprefecture\\.shp$",
      shp_files,
      ignore.case = TRUE
    )
  ]
  
  if (length(main_shp) != 47) {
    stop(
      "Expected 47 main N03 shapefiles; found ",
      length(main_shp), "."
    )
  }
  
  get_code <- function(path) {
    sub(
      "^N03-[0-9]+_([0-9]{2}).*$",
      "\\1",
      basename(path)
    )
  }
  
  jis_codes <- vapply(
    main_shp,
    get_code,
    character(1)
  )
  
  expected_jis <- sprintf(
    "%02d",
    1:47
  )
  
  if (
    !setequal(
      jis_codes,
      expected_jis
    )
  ) {
    stop(
      "Main N03 layers do not contain exactly JIS 01-47."
    )
  }
  
  main_shp <- main_shp[
    order(
      as.integer(jis_codes)
    )
  ]
  
  # ---------------------------------------------------------------------------
  # Read all main N03 geometry
  # ---------------------------------------------------------------------------
  
  cat("— Reading 47 main N03 layers —\n\n")
  
  admin_list <- vector(
    "list",
    length(main_shp)
  )
  
  for (i in seq_along(main_shp)) {
    path <- main_shp[[i]]
    code <- get_code(path)
    
    cat(
      sprintf(
        "[%02d/47] JIS %s  %s\n",
        i,
        code,
        basename(path)
      )
    )
    
    x <- sf::st_read(
      path,
      quiet = TRUE,
      stringsAsFactors = FALSE
    )
    
    required_n03_fields <- c(
      "N03_001",
      "N03_002",
      "N03_003",
      "N03_004",
      "N03_007"
    )
    
    missing_n03_fields <- setdiff(
      required_n03_fields,
      names(x)
    )
    
    if (length(missing_n03_fields) > 0) {
      stop(
        "Required N03 fields missing from ",
        basename(path),
        ": ",
        paste(
          missing_n03_fields,
          collapse = ", "
        )
      )
    }
    
    if (sf::st_crs(x)$epsg != 6668) {
      stop(
        basename(path),
        " is not EPSG:6668."
      )
    }
    
    x <- x %>%
      select(
        N03_001,
        N03_002,
        N03_003,
        N03_004,
        N03_007
      ) %>%
      mutate(
        source_jis = code,
        source_feature_number =
          seq_len(n())
      ) %>%
      sf::st_transform(4326)
    
    admin_list[[i]] <- x
  }
  
  admin <- do.call(
    rbind,
    admin_list
  )
  
  cat(
    "\nCombined N03 features: ",
    format(
      nrow(admin),
      big.mark = ","
    ),
    "\n\n",
    sep = ""
  )
  
  if (nrow(admin) != 124094) {
    stop(
      "Expected 124,094 main N03 features; found ",
      nrow(admin), "."
    )
  }
  
  if (
    any(
      !sf::st_is_valid(admin)
    )
  ) {
    stop(
      "One or more source N03 geometries are invalid."
    )
  }
  
  # ---------------------------------------------------------------------------
  # Reproduce validated 03c component classification
  # ---------------------------------------------------------------------------
  
  special_mask <- admin$source_jis %in%
    c("13", "46", "47")
  
  special <- admin[
    special_mask,
  ]
  
  ordinary <- admin[
    !special_mask,
  ]
  
  b <- bbox_table(
    special
  )
  
  special$xmin <- b$xmin
  special$ymin <- b$ymin
  special$xmax <- b$xmax
  special$ymax <- b$ymax
  
  special <- special %>%
    mutate(
      provisional_class = case_when(
        source_jis == "13" &
          ymax < 26 ~
          "Tokyo_south_26",
        
        source_jis == "13" &
          ymin >= 26 &
          ymax < 30 ~
          "Ogasawara",
        
        source_jis == "13" &
          ymin >= 30 &
          ymax < 35 ~
          "Izu",
        
        source_jis == "13" &
          ymin >= 35 ~
          "Tokyo_remainder",
        
        source_jis == "46" &
          ymax < 29 ~
          "Amami_Ryukyu",
        
        source_jis == "46" &
          ymin >= 29 ~
          "Kagoshima_remainder",
        
        source_jis == "47" ~
          "Okinawa_Ryukyu",
        
        TRUE ~
          "review"
      )
    )
  
  if (
    any(
      special$provisional_class ==
      "review"
    )
  ) {
    stop(
      "Special polygons remain unclassified."
    )
  }
  
  # ---------------------------------------------------------------------------
  # Apply validated 03d Ogasawara/Kazan scope
  # ---------------------------------------------------------------------------
  
  special <- special %>%
    mutate(
      final_component_class = case_when(
        provisional_class ==
          "Tokyo_south_26" &
          xmax < 138 ~
          "Okinotorishima_excluded",
        
        provisional_class ==
          "Tokyo_south_26" &
          xmin > 150 ~
          "Minamitorishima_excluded",
        
        provisional_class ==
          "Tokyo_south_26" &
          xmin >= 139 &
          xmax <= 145 ~
          "Kazan",
        
        provisional_class ==
          "Ogasawara" ~
          "Ogasawara",
        
        provisional_class ==
          "Izu" ~
          "Izu",
        
        provisional_class ==
          "Tokyo_remainder" ~
          "Tokyo_remainder",
        
        provisional_class ==
          "Amami_Ryukyu" ~
          "Amami_Ryukyu",
        
        provisional_class ==
          "Kagoshima_remainder" ~
          "Kagoshima_remainder",
        
        provisional_class ==
          "Okinawa_Ryukyu" ~
          "Okinawa_Ryukyu",
        
        TRUE ~
          "review"
      )
    )
  
  if (
    any(
      special$final_component_class ==
      "review"
    )
  ) {
    stop(
      "Final special-component classification contains review cases."
    )
  }
  
  component_counts <- special %>%
    sf::st_drop_geometry() %>%
    count(
      final_component_class,
      name = "polygon_features"
    ) %>%
    arrange(
      final_component_class
    )
  
  cat(
    "— Validated special-component counts —\n\n"
  )
  
  print.data.frame(
    as.data.frame(component_counts),
    row.names = FALSE
  )
  
  # ---------------------------------------------------------------------------
  # Assign included polygons to botanical areas
  # ---------------------------------------------------------------------------
  
  ordinary <- ordinary %>%
    mutate(
      botanical_area_id =
        paste0(
          "JP",
          source_jis
        ),
      assignment_method =
        "N03 political prefecture"
    )
  
  special <- special %>%
    mutate(
      botanical_area_id = case_when(
        final_component_class ==
          "Tokyo_remainder" ~
          "JP13",
        
        final_component_class ==
          "Kagoshima_remainder" ~
          "JP46",
        
        final_component_class %in% c(
          "Amami_Ryukyu",
          "Okinawa_Ryukyu"
        ) ~
          "JP47",
        
        final_component_class ==
          "Izu" ~
          "JP48",
        
        final_component_class ==
          "Ogasawara" ~
          "JP49",
        
        final_component_class ==
          "Kazan" ~
          "JP50",
        
        final_component_class %in% c(
          "Okinotorishima_excluded",
          "Minamitorishima_excluded"
        ) ~
          NA_character_,
        
        TRUE ~
          NA_character_
      ),
      
      assignment_method = case_when(
        final_component_class ==
          "Tokyo_remainder" ~
          "N03 Tokyo remainder",
        
        final_component_class ==
          "Kagoshima_remainder" ~
          "N03 Kagoshima remainder",
        
        final_component_class ==
          "Amami_Ryukyu" ~
          "N03 Amami -> botanical Ryukyu",
        
        final_component_class ==
          "Okinawa_Ryukyu" ~
          "N03 Okinawa -> botanical Ryukyu",
        
        final_component_class ==
          "Izu" ~
          "N03 component -> Izu",
        
        final_component_class ==
          "Ogasawara" ~
          "N03 component -> Ogasawara proper",
        
        final_component_class ==
          "Kazan" ~
          "N03 component -> Kazan proper",
        
        final_component_class ==
          "Okinotorishima_excluded" ~
          "excluded pending botanical treatment",
        
        final_component_class ==
          "Minamitorishima_excluded" ~
          "excluded pending botanical treatment",
        
        TRUE ~
          "review"
      )
    )
  
  included_special <- special %>%
    filter(
      !is.na(botanical_area_id)
    )
  
  excluded_remote <- special %>%
    filter(
      is.na(botanical_area_id)
    )
  
  if (nrow(excluded_remote) != 18) {
    stop(
      "Expected 18 excluded remote Tokyo polygons; found ",
      nrow(excluded_remote), "."
    )
  }
  
  assigned_components <- rbind(
    ordinary,
    included_special %>%
      select(
        N03_001,
        N03_002,
        N03_003,
        N03_004,
        N03_007,
        source_jis,
        source_feature_number,
        botanical_area_id,
        assignment_method,
        geometry
      )
  )
  
  # ---------------------------------------------------------------------------
  # Assignment QA before expensive dissolve
  # ---------------------------------------------------------------------------
  
  expected_ids <- sprintf(
    "JP%02d",
    1:50
  )
  
  observed_ids <- sort(
    unique(
      assigned_components$
        botanical_area_id
    )
  )
  
  missing_ids <- setdiff(
    expected_ids,
    observed_ids
  )
  
  unexpected_ids <- setdiff(
    observed_ids,
    expected_ids
  )
  
  if (length(missing_ids) > 0) {
    stop(
      "Missing botanical IDs before dissolve: ",
      paste(
        missing_ids,
        collapse = ", "
      )
    )
  }
  
  if (length(unexpected_ids) > 0) {
    stop(
      "Unexpected botanical IDs before dissolve: ",
      paste(
        unexpected_ids,
        collapse = ", "
      )
    )
  }
  
  cat(
    "\nPre-dissolve botanical-area assignment: PASS\n",
    "Expected JP01-JP50 represented: TRUE\n",
    "Excluded remote Tokyo polygons: ",
    nrow(excluded_remote),
    "\n\n",
    sep = ""
  )
  
  # ---------------------------------------------------------------------------
  # Dissolve to canonical botanical areas
  # ---------------------------------------------------------------------------
  
  cat(
    "— Dissolving source polygons to JP01-JP50 —\n\n"
  )
  
  botanical <- assigned_components %>%
    group_by(
      botanical_area_id
    ) %>%
    summarise(
      source_polygon_features = n(),
      geometry = sf::st_union(geometry),
      .groups = "drop"
    )
  
  botanical <- sf::st_make_valid(
    botanical
  )
  
  if (nrow(botanical) != 50) {
    stop(
      "Expected 50 dissolved botanical areas; found ",
      nrow(botanical), "."
    )
  }
  
  if (
    n_distinct(
      botanical$botanical_area_id
    ) != 50
  ) {
    stop(
      "Botanical area IDs are not unique."
    )
  }
  
  if (
    any(
      !sf::st_is_valid(botanical)
    )
  ) {
    stop(
      "One or more dissolved botanical geometries remain invalid."
    )
  }
  
  # ---------------------------------------------------------------------------
  # Attach confirmed Geography 02 analytical names
  # ---------------------------------------------------------------------------
  
  botanical <- botanical %>%
    left_join(
      framework_min,
      by = "botanical_area_id"
    ) %>%
    arrange(
      botanical_area_id
    )
  
  if (
    any(
      is.na(
        botanical$
        botanical_area_name
      )
    )
  ) {
    stop(
      "One or more botanical area names failed to join."
    )
  }
  
  cat(
    "Spatial dissolve: PASS\n",
    "Canonical areas constructed: 50\n",
    "Geography 02 analytical names attached: PASS\n\n",
    sep = ""
  )
  
  # ---------------------------------------------------------------------------
  # SAVE CANONICAL GEOMETRY IMMEDIATELY BEFORE DOWNSTREAM TOPOLOGY QA
  # ---------------------------------------------------------------------------
  
  spatial_dir <- here(
    "data",
    "interim",
    "geography",
    "botanical_boundaries"
  )
  
  dir.create(
    spatial_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  gpkg_path <- file.path(
    spatial_dir,
    "vpjd_japan_botanical_boundaries.gpkg"
  )
  
  if (file.exists(gpkg_path)) {
    file.remove(gpkg_path)
  }
  
  sf::st_write(
    botanical,
    gpkg_path,
    layer = "botanical_areas_jp01_jp50",
    quiet = TRUE
  )
  
  if (nrow(excluded_remote) > 0) {
    sf::st_write(
      excluded_remote,
      gpkg_path,
      layer = "excluded_remote_tokyo",
      append = TRUE,
      quiet = TRUE
    )
  }
  
  cat(
    "Canonical spatial checkpoint written:\n",
    gpkg_path,
    "\n\n",
    sep = ""
  )
  
  # ---------------------------------------------------------------------------
  # Area geometry diagnostics
  # ---------------------------------------------------------------------------
  
  area_bbox <- bind_rows(
    lapply(
      seq_len(nrow(botanical)),
      function(i) {
        b <- sf::st_bbox(
          botanical[i, ]
        )
        
        tibble(
          botanical_area_id =
            botanical$
            botanical_area_id[[i]],
          xmin =
            as.numeric(b["xmin"]),
          ymin =
            as.numeric(b["ymin"]),
          xmax =
            as.numeric(b["xmax"]),
          ymax =
            as.numeric(b["ymax"])
        )
      }
    )
  )
  
  botanical_table <- botanical %>%
    sf::st_drop_geometry() %>%
    left_join(
      area_bbox,
      by = "botanical_area_id"
    ) %>%
    mutate(
      geometry_valid =
        sf::st_is_valid(botanical)
    ) %>%
    arrange(
      botanical_area_id
    ) %>%
    as_tibble()
  
  # ---------------------------------------------------------------------------
  # Excluded remote geometry table
  # ---------------------------------------------------------------------------
  
  excluded_table <- excluded_remote %>%
    sf::st_drop_geometry() %>%
    transmute(
      source_jis =
        source_jis,
      source_feature_number =
        source_feature_number,
      prefecture_jp =
        as.character(N03_001),
      municipality_jp =
        as.character(N03_004),
      admin_code =
        as.character(N03_007),
      excluded_class =
        final_component_class,
      exclusion_reason =
        assignment_method,
      xmin =
        xmin,
      ymin =
        ymin,
      xmax =
        xmax,
      ymax =
        ymax
    ) %>%
    as_tibble()
  
  exclusion_summary <- excluded_table %>%
    count(
      excluded_class,
      exclusion_reason,
      name = "polygon_features"
    ) %>%
    arrange(
      excluded_class
    ) %>%
    as_tibble()
  
  component_summary <- assigned_components %>%
    sf::st_drop_geometry() %>%
    count(
      botanical_area_id,
      assignment_method,
      name = "polygon_features"
    ) %>%
    arrange(
      botanical_area_id,
      assignment_method
    ) %>%
    as_tibble()
  
  # ---------------------------------------------------------------------------
  # Positive-area overlap QA
  #
  # Shared boundaries are expected. We therefore inspect only intersections
  # with positive area >1 m2 in an equal-area CRS.
  # ---------------------------------------------------------------------------
  
  cat(
    "— Running positive-area topology QA —\n\n"
  )
  
  botanical_qa_crs <- sf::st_transform(
    botanical,
    6933
  )
  
  intersection_matrix <- sf::st_intersects(
    botanical_qa_crs,
    botanical_qa_crs,
    sparse = FALSE
  )
  
  diag(
    intersection_matrix
  ) <- FALSE
  
  candidate_pairs <- which(
    upper.tri(
      intersection_matrix
    ) &
      intersection_matrix,
    arr.ind = TRUE
  )
  
  overlap_rows <- list()
  
  if (nrow(candidate_pairs) > 0) {
    overlap_rows <- lapply(
      seq_len(
        nrow(candidate_pairs)
      ),
      function(i) {
        a <- candidate_pairs[i, 1]
        b <- candidate_pairs[i, 2]
        
        inter <- suppressWarnings(
          sf::st_intersection(
            botanical_qa_crs[a, ],
            botanical_qa_crs[b, ]
          )
        )
        
        area_m2 <- if (
          nrow(inter) == 0
        ) {
          0
        } else {
          sum(
            as.numeric(
              sf::st_area(inter)
            )
          )
        }
        
        tibble(
          area_id_1 =
            botanical$
            botanical_area_id[[a]],
          area_id_2 =
            botanical$
            botanical_area_id[[b]],
          overlap_area_m2 =
            area_m2
        )
      }
    )
  }
  
  overlap_table <- if (
    length(overlap_rows) == 0
  ) {
    tibble(
      area_id_1 = character(),
      area_id_2 = character(),
      overlap_area_m2 = numeric()
    )
  } else {
    bind_rows(
      overlap_rows
    ) %>%
      filter(
        overlap_area_m2 > 1
      ) %>%
      arrange(
        desc(overlap_area_m2)
      )
  }
  
  # ---------------------------------------------------------------------------
  # QA summary and validation
  # ---------------------------------------------------------------------------
  
  qa_summary <- tibble(
    metric = c(
      "canonical_botanical_areas_built",
      "expected_botanical_areas_jp01_jp50",
      "distinct_botanical_area_ids",
      "valid_botanical_geometries",
      "invalid_botanical_geometries",
      "positive_area_overlap_pairs",
      "excluded_remote_polygon_features",
      "jp51_kuriles_present"
    ),
    value = c(
      nrow(botanical),
      50,
      n_distinct(
        botanical$
          botanical_area_id
      ),
      sum(
        sf::st_is_valid(botanical)
      ),
      sum(
        !sf::st_is_valid(botanical)
      ),
      nrow(overlap_table),
      nrow(excluded_table),
      sum(
        botanical$
          botanical_area_id ==
          "JP51"
      )
    )
  )
  
  validation <- tibble(
    check = c(
      "Exactly 50 JP01-JP50 areas",
      "Exactly 50 unique IDs",
      "All botanical geometries valid",
      "No positive-area overlaps >1 m2",
      "JP47 present",
      "JP48 present",
      "JP49 present",
      "JP50 present",
      "JP51 absent",
      "Okinotorishima excluded",
      "Minamitorishima excluded"
    ),
    pass = c(
      nrow(botanical) == 50,
      
      n_distinct(
        botanical$
          botanical_area_id
      ) == 50,
      
      all(
        sf::st_is_valid(botanical)
      ),
      
      nrow(overlap_table) == 0,
      
      "JP47" %in%
        botanical$
        botanical_area_id,
      
      "JP48" %in%
        botanical$
        botanical_area_id,
      
      "JP49" %in%
        botanical$
        botanical_area_id,
      
      "JP50" %in%
        botanical$
        botanical_area_id,
      
      !"JP51" %in%
        botanical$
        botanical_area_id,
      
      any(
        excluded_table$
          excluded_class ==
          "Okinotorishima_excluded"
      ),
      
      any(
        excluded_table$
          excluded_class ==
          "Minamitorishima_excluded"
      )
    )
  )
  
  # ---------------------------------------------------------------------------
  # Persist analytical tables
  # ---------------------------------------------------------------------------
  
  dbWriteTable(
    con,
    "japan_botanical_boundary_areas",
    botanical_table,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "japan_botanical_boundary_component_summary",
    component_summary,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "japan_botanical_boundary_excluded_remote",
    excluded_table,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "japan_botanical_boundary_exclusion_summary",
    exclusion_summary,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "japan_botanical_boundary_overlap_qa",
    overlap_table,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "japan_botanical_boundary_qa_summary",
    qa_summary,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "japan_botanical_boundary_validation",
    validation,
    overwrite = TRUE
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
      "input_geography_03d_version",
      "boundary_source",
      "boundary_source_release",
      "boundary_crs_epsg",
      "canonical_spatial_areas_built",
      "botanical_name_field",
      "kuriles_constructed",
      "okinotorishima_assigned",
      "minamitorishima_assigned",
      "occurrences_assigned",
      "coordinates_modified",
      "taxonomy_modified",
      "frozen_upstream_tables_modified"
    ),
    value = c(
      SCRIPT_VERSION,
      g02,
      g03a,
      g03b,
      g03c,
      g03d,
      "Japan MLIT NLNI N03 Administrative Areas",
      "2025",
      "4326",
      as.character(
        nrow(botanical)
      ),
      "analytical_area_name",
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
    "japan_botanical_boundary_metadata",
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
    "botanical_boundaries"
  )
  
  dir.create(
    out_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  write_csv(
    botanical_table,
    file.path(
      out_dir,
      "botanical_areas_jp01_jp50.csv"
    )
  )
  
  write_csv(
    component_summary,
    file.path(
      out_dir,
      "component_assignment_summary.csv"
    )
  )
  
  write_csv(
    excluded_table,
    file.path(
      out_dir,
      "excluded_remote_tokyo.csv"
    )
  )
  
  write_csv(
    exclusion_summary,
    file.path(
      out_dir,
      "exclusion_summary.csv"
    )
  )
  
  write_csv(
    overlap_table,
    file.path(
      out_dir,
      "positive_area_overlap_qa.csv"
    )
  )
  
  write_csv(
    qa_summary,
    file.path(
      out_dir,
      "qa_summary.csv"
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
    "\nCanonical botanical areas built: ",
    nrow(botanical),
    "\n\n",
    sep = ""
  )
  
  cat(
    "— Botanical-area summary —\n\n"
  )
  
  print.data.frame(
    as.data.frame(
      botanical_table %>%
        select(
          botanical_area_id,
          botanical_area_name,
          source_polygon_features,
          xmin,
          ymin,
          xmax,
          ymax,
          geometry_valid
        )
    ),
    row.names = FALSE
  )
  
  cat(
    "\n— Special-area component assignments —\n\n"
  )
  
  print.data.frame(
    as.data.frame(
      component_summary %>%
        filter(
          botanical_area_id %in%
            c(
              "JP13",
              "JP46",
              "JP47",
              "JP48",
              "JP49",
              "JP50"
            )
        )
    ),
    row.names = FALSE
  )
  
  cat(
    "\n— Explicitly excluded remote Tokyo geometry —\n\n"
  )
  
  print.data.frame(
    as.data.frame(
      exclusion_summary
    ),
    row.names = FALSE
  )
  
  cat(
    "\n— Positive-area overlap QA —\n\n"
  )
  
  if (nrow(overlap_table) == 0) {
    cat("None >1 m2.\n")
  } else {
    print.data.frame(
      as.data.frame(
        overlap_table
      ),
      row.names = FALSE
    )
  }
  
  cat(
    "\n— QA summary —\n\n"
  )
  
  print.data.frame(
    as.data.frame(
      qa_summary
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
  
  cat("\n— Safety —\n")
  cat("Canonical JP01-JP50 geometries created: TRUE\n")
  cat("JP51 Kuriles constructed: FALSE\n")
  cat("Okinotorishima assigned: FALSE\n")
  cat("Minamitorishima assigned: FALSE\n")
  cat("Occurrences assigned: FALSE\n")
  cat("Coordinates modified: FALSE\n")
  cat("Taxonomy modified: FALSE\n")
  cat("Frozen upstream tables modified: FALSE\n")
  cat(
    "Output status: CANONICAL N03-DERIVED BOTANICAL BOUNDARIES / PRE-OCCURRENCE ASSIGNMENT\n"
  )
  
  cat(
    "\n03e_build_nlni_botanical_boundaries.R v",
    SCRIPT_VERSION,
    " complete.\n",
    sep = ""
  )
  
  invisible(
    list(
      botanical =
        botanical,
      botanical_table =
        botanical_table,
      component_summary =
        component_summary,
      excluded_table =
        excluded_table,
      overlap_table =
        overlap_table,
      qa_summary =
        qa_summary,
      validation =
        validation,
      metadata =
        metadata
    )
  )
}

geography_03e <-
  run_botanical_boundary_build()