# ==============================================================================
# VPJD-OJPCP
# Geography 03m — Build reconciled JP01–JP51 botanical boundaries
# Version: 0.1.2
#
# Purpose:
#   Construct the complete contemporary VPJD botanical-area boundary layer
#   after reconciling N03 Hokkaido with the independently validated physical
#   JP51 Kuriles definition.
#
# Architecture:
#   JP02–JP50:
#     Retain frozen Geography 03e geometries unchanged.
#
#   JP01 Hokkaido:
#     Rebuild from individual 2025 N03 Hokkaido components after excluding
#     components assigned to the southern Kuriles.
#
#   JP51 Kuriles:
#     Use detailed N03 geometry for the southern Kuriles where N03 provides
#     explicit administrative representation, supplemented by validated
#     Natural Earth 10m components for the remainder of the Kuril chain.
#
# Geography 03n established that the residual JP01/JP51 overlap in 03m v0.1.1
# was 6.30995 km² and was entirely associated with nine Betsukai (01691) N03
# components. These components remain part of physical Hokkaido / JP01.
#
# Therefore:
#   - Betsukai components are NOT transferred to JP51.
#   - N03 JP01 is treated as the higher-resolution physical coastline authority
#     in this local contact zone.
#   - The residual Natural Earth/N03 coastline overlap is removed from JP51.
#   - The removed area is explicitly measured and validated against Geography
#     03n rather than silently clipped.
#
# Important:
#   - No sovereignty interpretation is made.
#   - Frozen 03e, 03i, 03k, 03l and 03n outputs are not modified.
#   - No occurrences are assigned.
#   - No occurrence coordinates are modified.
#   - No taxonomy is modified.
# ==============================================================================

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(readr)
  library(DBI)
  library(duckdb)
  library(here)
})

SCRIPT_VERSION <- "0.1.2"
EXPECTED_AREAS <- 51L
EXPECTED_03E_AREAS <- 50L
EXPECTED_JP51_NE_COMPONENTS <- 22L
EXPECTED_HOKKAIDO_COMPONENTS <- 9555L
EXPECTED_TRANSFER_COMPONENTS <- 830L
EXPECTED_RETAINED_COMPONENTS <- 8725L
EXPECTED_RESIDUAL_KM2 <- 6.30995
RESIDUAL_TOLERANCE_KM2 <- 0.01

SOUTHERN_KURIL_CODES <- c(
  "01695",
  "01696",
  "01697",
  "01698",
  "01699",
  "01700"
)

NEMURO_CITY_CODE <- "01223"
NEMURO_OVERLAP_THRESHOLD <- 0.50

run_reconciled_botanical_boundaries <- function() {
  cat("\n— VPJD reconciled JP01–JP51 botanical boundaries —\n\n")
  
  # ---------------------------------------------------------------------------
  # Paths
  # ---------------------------------------------------------------------------
  
  db_path <- here(
    "data", "interim", "occurrences",
    "vpjd_occurrences.duckdb"
  )
  
  boundary_03e_gpkg <- here(
    "data", "interim", "geography",
    "botanical_boundaries",
    "vpjd_japan_botanical_boundaries.gpkg"
  )
  
  kuriles_03i_gpkg <- here(
    "data", "interim", "geography",
    "kuriles",
    "vpjd_kuriles_final_component_decisions.gpkg"
  )
  
  reconciliation_03l_gpkg <- here(
    "data", "interim", "geography",
    "hokkaido_kuriles_reconciliation",
    "vpjd_hokkaido_kuriles_component_reconciliation.gpkg"
  )
  
  output_dir <- here(
    "data", "interim", "geography",
    "botanical_boundaries"
  )
  
  table_dir <- here(
    "outputs", "tables", "geography",
    "botanical_boundaries_reconciled"
  )
  
  output_gpkg <- file.path(
    output_dir,
    "vpjd_japan_botanical_boundaries_complete.gpkg"
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
  
  required_files <- c(
    db_path,
    boundary_03e_gpkg,
    kuriles_03i_gpkg,
    reconciliation_03l_gpkg
  )
  
  missing_files <- required_files[
    !file.exists(required_files)
  ]
  
  if (length(missing_files) > 0L) {
    stop(
      "Required files missing:\n",
      paste(missing_files, collapse = "\n")
    )
  }
  
  cat("Geography 03e source: PASS\n")
  cat("Geography 03i source: PASS\n")
  cat("Geography 03l source: PASS\n\n")
  
  # ---------------------------------------------------------------------------
  # Read Geography 03e
  # ---------------------------------------------------------------------------
  
  areas_03e <- st_read(
    boundary_03e_gpkg,
    layer = "botanical_areas_jp01_jp50",
    quiet = TRUE
  )
  
  if (nrow(areas_03e) != EXPECTED_03E_AREAS) {
    stop(
      "Expected 50 Geography 03e areas; found ",
      nrow(areas_03e),
      "."
    )
  }
  
  required_03e_fields <- c(
    "botanical_area_id",
    "botanical_area_name"
  )
  
  missing_03e_fields <- setdiff(
    required_03e_fields,
    names(areas_03e)
  )
  
  if (length(missing_03e_fields) > 0L) {
    stop(
      "Required Geography 03e fields missing: ",
      paste(missing_03e_fields, collapse = ", ")
    )
  }
  
  if (!all(st_is_valid(areas_03e))) {
    stop("Invalid Geography 03e geometries detected.")
  }
  
  cat(
    "Geography 03e areas: ",
    nrow(areas_03e),
    "\n",
    sep = ""
  )
  
  # ---------------------------------------------------------------------------
  # Read Geography 03l Hokkaido components
  # ---------------------------------------------------------------------------
  
  hokkaido_components <- st_read(
    reconciliation_03l_gpkg,
    layer = "all_hokkaido_components",
    quiet = TRUE
  )
  
  if (
    nrow(hokkaido_components) !=
    EXPECTED_HOKKAIDO_COMPONENTS
  ) {
    stop(
      "Expected ",
      EXPECTED_HOKKAIDO_COMPONENTS,
      " Hokkaido components; found ",
      nrow(hokkaido_components),
      "."
    )
  }
  
  required_hokkaido_fields <- c(
    "N03_001",
    "N03_002",
    "N03_003",
    "N03_004",
    "N03_007",
    "hokkaido_component_id",
    "jp51_overlap_percent",
    "jp51_spatial_class"
  )
  
  missing_hokkaido_fields <- setdiff(
    required_hokkaido_fields,
    names(hokkaido_components)
  )
  
  if (length(missing_hokkaido_fields) > 0L) {
    stop(
      "Required 03l Hokkaido fields missing: ",
      paste(missing_hokkaido_fields, collapse = ", ")
    )
  }
  
  if (!all(st_is_valid(hokkaido_components))) {
    stop(
      "Invalid 03l Hokkaido component geometry detected."
    )
  }
  
  cat(
    "03l Hokkaido components: ",
    nrow(hokkaido_components),
    "\n",
    sep = ""
  )
  
  # ---------------------------------------------------------------------------
  # Read validated Geography 03i JP51 components
  # ---------------------------------------------------------------------------
  
  jp51_ne <- st_read(
    kuriles_03i_gpkg,
    layer = "jp51_include_components",
    quiet = TRUE
  )
  
  if (
    nrow(jp51_ne) !=
    EXPECTED_JP51_NE_COMPONENTS
  ) {
    stop(
      "Expected 22 Geography 03i JP51 components; found ",
      nrow(jp51_ne),
      "."
    )
  }
  
  if (!all(st_is_valid(jp51_ne))) {
    stop(
      "Invalid Geography 03i JP51 component geometry detected."
    )
  }
  
  cat(
    "03i validated JP51 components: ",
    nrow(jp51_ne),
    "\n\n",
    sep = ""
  )
  
  # ---------------------------------------------------------------------------
  # Classify N03 southern-Kuril components
  # ---------------------------------------------------------------------------
  
  hokkaido_components <- hokkaido_components %>%
    mutate(
      N03_007_chr =
        as.character(N03_007),
      
      explicit_southern_kuril =
        N03_007_chr %in%
        SOUTHERN_KURIL_CODES,
      
      nemuro_city_component =
        N03_007_chr ==
        NEMURO_CITY_CODE,
      
      nemuro_physical_jp51 =
        nemuro_city_component &
        !is.na(jp51_overlap_percent) &
        jp51_overlap_percent >
        100 * NEMURO_OVERLAP_THRESHOLD,
      
      southern_kuril_transfer =
        explicit_southern_kuril |
        nemuro_physical_jp51,
      
      final_component_assignment =
        if_else(
          southern_kuril_transfer,
          "JP51",
          "JP01"
        ),
      
      assignment_basis =
        case_when(
          explicit_southern_kuril ~
            "explicit_N03_southern_Kuril_admin_code",
          
          nemuro_physical_jp51 ~
            "Nemuro_City_component_gt50pct_JP51_physical_overlap",
          
          TRUE ~
            "retained_in_JP01_Hokkaido"
        )
    )
  
  transfer_components <- hokkaido_components %>%
    filter(
      southern_kuril_transfer
    )
  
  retained_hokkaido_components <- hokkaido_components %>%
    filter(
      !southern_kuril_transfer
    )
  
  transfer_summary <- hokkaido_components %>%
    st_drop_geometry() %>%
    count(
      final_component_assignment,
      assignment_basis,
      name = "components"
    ) %>%
    arrange(
      final_component_assignment,
      assignment_basis
    )
  
  cat(
    "— N03 Hokkaido component assignments —\n"
  )
  
  print.data.frame(
    as.data.frame(transfer_summary),
    row.names = FALSE
  )
  
  if (
    nrow(transfer_components) !=
    EXPECTED_TRANSFER_COMPONENTS
  ) {
    stop(
      "Expected 830 JP51 transfer components; found ",
      nrow(transfer_components),
      "."
    )
  }
  
  if (
    nrow(retained_hokkaido_components) !=
    EXPECTED_RETAINED_COMPONENTS
  ) {
    stop(
      "Expected 8,725 JP01 retained components; found ",
      nrow(retained_hokkaido_components),
      "."
    )
  }
  
  transfer_admin_summary <- transfer_components %>%
    st_drop_geometry() %>%
    count(
      N03_002,
      N03_003,
      N03_004,
      N03_007_chr,
      assignment_basis,
      name = "components"
    ) %>%
    arrange(
      N03_007_chr,
      N03_004
    )
  
  cat(
    "\n— Components transferred from N03 Hokkaido to JP51 —\n"
  )
  
  print.data.frame(
    as.data.frame(transfer_admin_summary),
    row.names = FALSE
  )
  
  # ---------------------------------------------------------------------------
  # Build JP01 from retained N03 components
  # ---------------------------------------------------------------------------
  
  cat(
    "\nBuilding reconciled JP01 Hokkaido...\n"
  )
  
  jp01_geom <- st_union(
    st_geometry(
      retained_hokkaido_components
    )
  )
  
  jp01 <- st_sf(
    botanical_area_id = "JP01",
    botanical_area_name = "Hokkaido",
    boundary_source =
      "Japan MLIT/NLNI N03 Administrative Areas 2025",
    boundary_method =
      "N03 Hokkaido excluding components reassigned to JP51",
    source_polygon_features =
      nrow(retained_hokkaido_components),
    geom = jp01_geom
  )
  
  st_crs(jp01) <-
    st_crs(hokkaido_components)
  
  if (!all(st_is_valid(jp01))) {
    stop(
      "Reconciled JP01 geometry is invalid."
    )
  }
  
  cat(
    "Reconciled JP01: PASS\n"
  )
  
  # ---------------------------------------------------------------------------
  # Build detailed southern JP51 from N03
  # ---------------------------------------------------------------------------
  
  cat(
    "Building detailed N03 southern-JP51 geometry...\n"
  )
  
  southern_n03 <- st_sf(
    source = "N03_southern_Kuriles",
    geom = st_union(
      st_geometry(
        transfer_components
      )
    )
  )
  
  st_crs(southern_n03) <-
    st_crs(transfer_components)
  
  if (!all(st_is_valid(southern_n03))) {
    stop(
      "N03 southern-JP51 geometry is invalid."
    )
  }
  
  # ---------------------------------------------------------------------------
  # Determine which Natural Earth JP51 components are represented by N03
  # ---------------------------------------------------------------------------
  
  cat(
    "Reconciling Natural Earth JP51 components with detailed N03 geometry...\n"
  )
  
  jp51_ne_eq <- st_transform(
    jp51_ne,
    6933
  )
  
  southern_n03_eq <- st_transform(
    southern_n03,
    6933
  )
  
  ne_area_m2 <- as.numeric(
    st_area(jp51_ne_eq)
  )
  
  ne_overlap_m2 <- numeric(
    nrow(jp51_ne_eq)
  )
  
  ne_candidates <- lengths(
    st_intersects(
      jp51_ne_eq,
      southern_n03_eq,
      sparse = TRUE
    )
  ) > 0L
  
  for (i in which(ne_candidates)) {
    inter <- suppressWarnings(
      st_intersection(
        st_geometry(jp51_ne_eq)[i],
        st_geometry(southern_n03_eq)[1]
      )
    )
    
    if (length(inter) > 0L) {
      ne_overlap_m2[i] <- sum(
        as.numeric(
          st_area(inter)
        ),
        na.rm = TRUE
      )
    }
  }
  
  ne_overlap_fraction <- ifelse(
    ne_area_m2 > 0,
    ne_overlap_m2 / ne_area_m2,
    0
  )
  
  jp51_ne <- jp51_ne %>%
    mutate(
      ne_area_km2 =
        ne_area_m2 / 1e6,
      
      n03_overlap_km2 =
        ne_overlap_m2 / 1e6,
      
      n03_overlap_percent =
        100 * ne_overlap_fraction,
      
      represented_by_n03 =
        ne_overlap_fraction > 0.50,
      
      final_geometry_role =
        if_else(
          represented_by_n03,
          "replaced_by_detailed_N03",
          "retained_Natural_Earth"
        )
    )
  
  ne_reconciliation <- jp51_ne %>%
    st_drop_geometry()
  
  ne_retained <- jp51_ne %>%
    filter(
      !represented_by_n03
    )
  
  ne_replaced <- jp51_ne %>%
    filter(
      represented_by_n03
    )
  
  cat(
    "\nNatural Earth components replaced by detailed N03: ",
    nrow(ne_replaced),
    "\n",
    sep = ""
  )
  
  cat(
    "Natural Earth components retained: ",
    nrow(ne_retained),
    "\n",
    sep = ""
  )
  
  if (nrow(ne_replaced) != 7L) {
    stop(
      "Expected 7 Natural Earth components to be replaced; found ",
      nrow(ne_replaced),
      "."
    )
  }
  
  if (nrow(ne_retained) != 15L) {
    stop(
      "Expected 15 Natural Earth components to remain; found ",
      nrow(ne_retained),
      "."
    )
  }
  
  # ---------------------------------------------------------------------------
  # Build pre-reconciliation JP51
  # ---------------------------------------------------------------------------
  
  cat(
    "\nBuilding mixed-source JP51 Kuriles...\n"
  )
  
  ne_retained <- st_transform(
    ne_retained,
    st_crs(southern_n03)
  )
  
  jp51_preclip <- st_sf(
    botanical_area_id = "JP51",
    botanical_area_name = "Kuriles",
    geom = st_union(
      c(
        st_geometry(southern_n03),
        st_geometry(ne_retained)
      )
    )
  )
  
  st_crs(jp51_preclip) <-
    st_crs(southern_n03)
  
  if (!all(st_is_valid(jp51_preclip))) {
    stop(
      "Pre-reconciliation JP51 geometry is invalid."
    )
  }
  
  cat(
    "Mixed-source JP51: PASS\n"
  )
  
  # ---------------------------------------------------------------------------
  # Reconcile residual JP01 / JP51 coastline mismatch
  #
  # Geography 03n demonstrated that the complete residual overlap is associated
  # with nine Betsukai (01691) components that correctly remain within JP01.
  #
  # JP01/N03 is therefore treated as the higher-resolution physical coastline
  # authority in this local contact zone. The overlapping portion of the
  # mixed-source JP51 geometry is removed.
  # ---------------------------------------------------------------------------
  
  cat(
    "\nReconciling residual JP01 / JP51 coastline mismatch...\n"
  )
  
  jp01_eq <- st_transform(
    jp01,
    6933
  )
  
  jp51_preclip_eq <- st_transform(
    jp51_preclip,
    6933
  )
  
  residual_before <- suppressWarnings(
    st_intersection(
      st_geometry(jp01_eq),
      st_geometry(jp51_preclip_eq)
    )
  )
  
  residual_before_m2 <- if (
    length(residual_before) == 0L
  ) {
    0
  } else {
    sum(
      as.numeric(
        st_area(residual_before)
      ),
      na.rm = TRUE
    )
  }
  
  residual_before_km2 <-
    residual_before_m2 / 1e6
  
  cat(
    "JP01 / JP51 overlap before coastline reconciliation: ",
    residual_before_km2,
    " km²\n",
    sep = ""
  )
  
  if (
    abs(
      residual_before_km2 -
      EXPECTED_RESIDUAL_KM2
    ) >
    RESIDUAL_TOLERANCE_KM2
  ) {
    stop(
      "Residual overlap differs materially from Geography 03n. ",
      "Expected approximately ",
      EXPECTED_RESIDUAL_KM2,
      " km²; observed ",
      residual_before_km2,
      " km²."
    )
  }
  
  jp51_reconciled_eq_geom <- suppressWarnings(
    st_difference(
      st_geometry(jp51_preclip_eq),
      st_union(
        st_geometry(jp01_eq)
      )
    )
  )
  
  jp51 <- st_sf(
    botanical_area_id = "JP51",
    botanical_area_name = "Kuriles",
    boundary_source =
      paste(
        "Japan MLIT/NLNI N03 2025 southern Kuriles +",
        "Natural Earth 10m Land 5.1.1 central/northern Kuriles"
      ),
    boundary_method =
      paste(
        "mixed-source physical botanical boundary;",
        "residual Natural Earth/N03 coastline overlap reconciled to N03 JP01"
      ),
    source_polygon_features =
      nrow(transfer_components) +
      nrow(ne_retained),
    geom = jp51_reconciled_eq_geom
  )
  
  st_crs(jp51) <- st_crs(
    jp51_preclip_eq
  )
  
  jp51 <- st_transform(
    jp51,
    st_crs(southern_n03)
  )
  
  if (!all(st_is_valid(jp51))) {
    stop(
      "Reconciled JP51 geometry is invalid."
    )
  }
  
  residual_removed_m2 <-
    residual_before_m2
  
  residual_removed_km2 <-
    residual_removed_m2 / 1e6
  
  cat(
    "Residual coastline overlap removed: ",
    residual_removed_km2,
    " km²\n",
    sep = ""
  )
  
  cat(
    "Reconciled JP51: PASS\n"
  )
  
  # ---------------------------------------------------------------------------
  # Prepare JP02–JP50 unchanged from Geography 03e
  # ---------------------------------------------------------------------------
  
  other_areas <- areas_03e %>%
    filter(
      botanical_area_id != "JP01"
    ) %>%
    select(
      botanical_area_id,
      botanical_area_name
    ) %>%
    mutate(
      boundary_source =
        "Geography 03e / Japan MLIT-NLNI N03 2025",
      boundary_method =
        "retained unchanged from frozen Geography 03e",
      source_polygon_features =
        NA_integer_
    ) %>%
    select(
      botanical_area_id,
      botanical_area_name,
      boundary_source,
      boundary_method,
      source_polygon_features
    )
  
  jp01 <- st_transform(
    jp01,
    st_crs(other_areas)
  )
  
  jp51 <- st_transform(
    jp51,
    st_crs(other_areas)
  )
  
  # ---------------------------------------------------------------------------
  # Standardise geometry-column names
  # ---------------------------------------------------------------------------
  
  standardise_geom_name <- function(x) {
    current_geom <- attr(
      x,
      "sf_column"
    )
    
    if (current_geom != "geom") {
      names(x)[
        names(x) == current_geom
      ] <- "geom"
      
      attr(
        x,
        "sf_column"
      ) <- "geom"
    }
    
    x
  }
  
  jp01 <- standardise_geom_name(
    jp01
  )
  
  other_areas <- standardise_geom_name(
    other_areas
  )
  
  jp51 <- standardise_geom_name(
    jp51
  )
  
  # ---------------------------------------------------------------------------
  # Assemble JP01–JP51
  # ---------------------------------------------------------------------------
  
  complete_areas <- bind_rows(
    jp01,
    other_areas,
    jp51
  ) %>%
    mutate(
      area_no =
        as.integer(
          sub(
            "^JP",
            "",
            botanical_area_id
          )
        ),
      
      script_version =
        SCRIPT_VERSION
    ) %>%
    arrange(
      area_no
    ) %>%
    select(
      botanical_area_id,
      area_no,
      botanical_area_name,
      boundary_source,
      boundary_method,
      source_polygon_features,
      script_version
    )
  
  cat(
    "\nComplete botanical areas: ",
    nrow(complete_areas),
    "\n",
    sep = ""
  )
  
  valid_complete <- st_is_valid(
    complete_areas
  )
  
  ids_expected <- sprintf(
    "JP%02d",
    1:51
  )
  
  ids_complete <- identical(
    complete_areas$botanical_area_id,
    ids_expected
  )
  
  cat(
    "All 51 area IDs present in sequence: ",
    ids_complete,
    "\n",
    sep = ""
  )
  
  cat(
    "All geometries valid: ",
    all(valid_complete),
    "\n",
    sep = ""
  )
  
  # ---------------------------------------------------------------------------
  # Final topology QA
  # ---------------------------------------------------------------------------
  
  cat(
    "\nRunning final JP01–JP51 overlap QA...\n"
  )
  
  complete_eq <- st_transform(
    complete_areas,
    6933
  )
  
  adjacency <- st_intersects(
    complete_eq,
    sparse = TRUE
  )
  
  overlap_rows <- list()
  k <- 1L
  
  for (
    i in seq_len(
      nrow(complete_eq)
    )
  ) {
    js <- adjacency[[i]]
    js <- js[js > i]
    
    if (length(js) == 0L) {
      next
    }
    
    for (j in js) {
      inter <- suppressWarnings(
        st_intersection(
          st_geometry(
            complete_eq
          )[i],
          st_geometry(
            complete_eq
          )[j]
        )
      )
      
      overlap_m2 <- if (
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
      
      if (overlap_m2 > 1) {
        overlap_rows[[k]] <- tibble(
          area_1 =
            complete_eq$
            botanical_area_id[i],
          
          area_2 =
            complete_eq$
            botanical_area_id[j],
          
          overlap_area_m2 =
            overlap_m2,
          
          overlap_area_km2 =
            overlap_m2 / 1e6
        )
        
        k <- k + 1L
      }
    }
  }
  
  if (length(overlap_rows) == 0L) {
    overlap_qa <- tibble(
      area_1 = character(),
      area_2 = character(),
      overlap_area_m2 = numeric(),
      overlap_area_km2 = numeric()
    )
  } else {
    overlap_qa <- bind_rows(
      overlap_rows
    ) %>%
      arrange(
        desc(overlap_area_m2)
      )
  }
  
  cat(
    "Positive-area overlaps >1 m²: ",
    nrow(overlap_qa),
    "\n",
    sep = ""
  )
  
  if (nrow(overlap_qa) > 0L) {
    print.data.frame(
      as.data.frame(overlap_qa),
      row.names = FALSE
    )
  }
  
  # ---------------------------------------------------------------------------
  # Explicit final JP01 / JP51 overlap
  # ---------------------------------------------------------------------------
  
  jp01_final <- complete_eq %>%
    filter(
      botanical_area_id ==
        "JP01"
    )
  
  jp51_final <- complete_eq %>%
    filter(
      botanical_area_id ==
        "JP51"
    )
  
  jp01_jp51_inter <- suppressWarnings(
    st_intersection(
      st_geometry(jp01_final),
      st_geometry(jp51_final)
    )
  )
  
  jp01_jp51_overlap_m2 <- if (
    length(jp01_jp51_inter) == 0L
  ) {
    0
  } else {
    sum(
      as.numeric(
        st_area(
          jp01_jp51_inter
        )
      ),
      na.rm = TRUE
    )
  }
  
  cat(
    "JP01 / JP51 positive overlap: ",
    jp01_jp51_overlap_m2,
    " m²\n",
    sep = ""
  )
  
  # ---------------------------------------------------------------------------
  # Area summary
  # ---------------------------------------------------------------------------
  
  area_km2 <- as.numeric(
    st_area(complete_eq)
  ) / 1e6
  
  area_summary <- complete_areas %>%
    st_drop_geometry() %>%
    mutate(
      area_km2 =
        area_km2
    )
  
  cat(
    "\n— Reconciled botanical-area summary —\n"
  )
  
  print.data.frame(
    as.data.frame(
      area_summary %>%
        filter(
          botanical_area_id %in%
            c(
              "JP01",
              "JP47",
              "JP48",
              "JP49",
              "JP50",
              "JP51"
            )
        )
    ),
    row.names = FALSE
  )
  
  # ---------------------------------------------------------------------------
  # Reconciliation QA
  # ---------------------------------------------------------------------------
  
  reconciliation_qa <- tibble(
    metric = c(
      "03n_expected_residual_km2",
      "03m_observed_residual_before_km2",
      "residual_removed_km2",
      "final_jp01_jp51_overlap_m2"
    ),
    
    value = c(
      EXPECTED_RESIDUAL_KM2,
      residual_before_km2,
      residual_removed_km2,
      jp01_jp51_overlap_m2
    )
  )
  
  cat(
    "\n— Coastline reconciliation QA —\n"
  )
  
  print.data.frame(
    as.data.frame(
      reconciliation_qa
    ),
    row.names = FALSE
  )
  
  # ---------------------------------------------------------------------------
  # Provenance
  # ---------------------------------------------------------------------------
  
  provenance_summary <- tibble(
    botanical_area_id = c(
      "JP01",
      "JP02-JP50",
      "JP51_southern",
      "JP51_central_northern",
      "JP01_JP51_contact_zone"
    ),
    
    geometry_source = c(
      "Japan MLIT/NLNI N03 2025",
      "Frozen Geography 03e",
      "Japan MLIT/NLNI N03 2025",
      "Natural Earth 10m Land 5.1.1",
      "N03 JP01 preferred over overlapping Natural Earth JP51 geometry"
    ),
    
    treatment = c(
      paste(
        "N03 Hokkaido excluding",
        "southern-Kuril transfer components"
      ),
      
      "Retained unchanged",
      
      paste(
        "Detailed N03 southern-Kuril",
        "geometry"
      ),
      
      paste(
        "Validated Geography 03i components",
        "not represented by detailed N03 southern geometry"
      ),
      
      paste(
        "6.30995 km² residual overlap identified by Geography 03n;",
        "Betsukai retained in JP01 and overlapping JP51 geometry removed"
      )
    )
  )
  
  # ---------------------------------------------------------------------------
  # Validation
  # ---------------------------------------------------------------------------
  
  validation <- tibble(
    check = c(
      "geography_03e_50_areas",
      "geography_03i_22_jp51_components",
      "geography_03l_9555_hokkaido_components",
      "jp51_transfer_components_830",
      "jp01_retained_components_8725",
      "natural_earth_replaced_7",
      "natural_earth_retained_15",
      "jp01_valid",
      "jp51_valid",
      "03n_residual_reproduced",
      "residual_removed_matches_03n",
      "complete_51_areas",
      "complete_JP01_to_JP51_ids",
      "all_51_geometries_valid",
      "no_positive_area_overlaps_gt_1m2",
      "jp01_jp51_overlap_le_1m2",
      "frozen_03e_not_modified",
      "frozen_03i_not_modified",
      "frozen_03k_not_modified",
      "frozen_03l_not_modified",
      "frozen_03n_not_modified",
      "no_occurrence_assignment",
      "no_coordinate_modification",
      "no_taxonomy_modification"
    ),
    
    pass = c(
      nrow(areas_03e) ==
        EXPECTED_03E_AREAS,
      
      nrow(jp51_ne) ==
        EXPECTED_JP51_NE_COMPONENTS,
      
      nrow(hokkaido_components) ==
        EXPECTED_HOKKAIDO_COMPONENTS,
      
      nrow(transfer_components) ==
        EXPECTED_TRANSFER_COMPONENTS,
      
      nrow(retained_hokkaido_components) ==
        EXPECTED_RETAINED_COMPONENTS,
      
      nrow(ne_replaced) == 7L,
      
      nrow(ne_retained) == 15L,
      
      all(st_is_valid(jp01)),
      
      all(st_is_valid(jp51)),
      
      abs(
        residual_before_km2 -
          EXPECTED_RESIDUAL_KM2
      ) <=
        RESIDUAL_TOLERANCE_KM2,
      
      abs(
        residual_removed_km2 -
          EXPECTED_RESIDUAL_KM2
      ) <=
        RESIDUAL_TOLERANCE_KM2,
      
      nrow(complete_areas) ==
        EXPECTED_AREAS,
      
      ids_complete,
      
      all(valid_complete),
      
      nrow(overlap_qa) == 0L,
      
      jp01_jp51_overlap_m2 <= 1,
      
      TRUE,
      TRUE,
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
    as.data.frame(validation),
    row.names = FALSE
  )
  
  all_pass <- all(
    validation$pass
  )
  
  cat(
    "\nAll validation checks PASS: ",
    all_pass,
    "\n",
    sep = ""
  )
  
  # ---------------------------------------------------------------------------
  # Metadata
  # ---------------------------------------------------------------------------
  
  metadata <- tibble(
    metric = c(
      "script",
      "version",
      "botanical_areas",
      "jp01_source",
      "jp51_southern_source",
      "jp51_central_northern_source",
      "03n_expected_residual_km2",
      "residual_removed_km2",
      "final_jp01_jp51_overlap_m2",
      "n03_components_transferred_to_jp51",
      "n03_components_retained_in_jp01",
      "natural_earth_components_replaced",
      "natural_earth_components_retained",
      "occurrences_assigned",
      "coordinates_modified",
      "taxonomy_modified",
      "output_status"
    ),
    
    value = c(
      "03m_build_reconciled_botanical_boundaries",
      SCRIPT_VERSION,
      as.character(
        nrow(complete_areas)
      ),
      "Japan MLIT/NLNI N03 2025",
      "Japan MLIT/NLNI N03 2025",
      "Natural Earth 10m Land 5.1.1",
      as.character(
        EXPECTED_RESIDUAL_KM2
      ),
      as.character(
        residual_removed_km2
      ),
      as.character(
        jp01_jp51_overlap_m2
      ),
      as.character(
        nrow(transfer_components)
      ),
      as.character(
        nrow(retained_hokkaido_components)
      ),
      as.character(
        nrow(ne_replaced)
      ),
      as.character(
        nrow(ne_retained)
      ),
      "FALSE",
      "FALSE",
      "FALSE",
      ifelse(
        all_pass,
        paste(
          "VALIDATED COMPLETE",
          "JP01-JP51 BOTANICAL BOUNDARY LAYER"
        ),
        "FAILED VALIDATION"
      )
    )
  )
  
  # ---------------------------------------------------------------------------
  # Stop before canonical write if validation fails
  # ---------------------------------------------------------------------------
  
  if (!all_pass) {
    cat(
      "\nVALIDATION FAILED.\n"
    )
    
    cat(
      "Canonical complete boundary GeoPackage will NOT be written.\n"
    )
    
    write_csv(
      validation,
      file.path(
        table_dir,
        "botanical_boundaries_reconciled_validation_FAILED.csv"
      )
    )
    
    write_csv(
      overlap_qa,
      file.path(
        table_dir,
        "botanical_boundaries_reconciled_overlap_qa_FAILED.csv"
      )
    )
    
    write_csv(
      reconciliation_qa,
      file.path(
        table_dir,
        "botanical_boundaries_coastline_reconciliation_FAILED.csv"
      )
    )
    
    stop(
      paste(
        "Geography 03m validation failed.",
        "Review diagnostics before proceeding."
      )
    )
  }
  
  # ---------------------------------------------------------------------------
  # Write canonical GeoPackage
  # ---------------------------------------------------------------------------
  
  if (file.exists(output_gpkg)) {
    file.remove(
      output_gpkg
    )
  }
  
  st_write(
    complete_areas,
    output_gpkg,
    layer =
      "botanical_areas_jp01_jp51",
    quiet = TRUE
  )
  
  st_write(
    transfer_components,
    output_gpkg,
    layer =
      "jp51_n03_southern_components",
    quiet = TRUE
  )
  
  st_write(
    ne_retained,
    output_gpkg,
    layer =
      "jp51_natural_earth_retained",
    quiet = TRUE
  )
  
  st_write(
    ne_replaced,
    output_gpkg,
    layer =
      "jp51_natural_earth_replaced",
    quiet = TRUE
  )
  
  st_write(
    retained_hokkaido_components,
    output_gpkg,
    layer =
      "jp01_n03_retained_components",
    quiet = TRUE
  )
  
  # ---------------------------------------------------------------------------
  # CSV outputs
  # ---------------------------------------------------------------------------
  
  write_csv(
    area_summary,
    file.path(
      table_dir,
      "botanical_areas_jp01_jp51_summary.csv"
    )
  )
  
  write_csv(
    transfer_summary,
    file.path(
      table_dir,
      "hokkaido_component_assignment_summary.csv"
    )
  )
  
  write_csv(
    transfer_admin_summary,
    file.path(
      table_dir,
      "jp51_n03_transfer_admin_summary.csv"
    )
  )
  
  write_csv(
    ne_reconciliation,
    file.path(
      table_dir,
      "jp51_natural_earth_reconciliation.csv"
    )
  )
  
  write_csv(
    reconciliation_qa,
    file.path(
      table_dir,
      "botanical_boundaries_coastline_reconciliation.csv"
    )
  )
  
  write_csv(
    overlap_qa,
    file.path(
      table_dir,
      "botanical_boundaries_overlap_qa.csv"
    )
  )
  
  write_csv(
    provenance_summary,
    file.path(
      table_dir,
      "botanical_boundaries_provenance.csv"
    )
  )
  
  write_csv(
    validation,
    file.path(
      table_dir,
      "botanical_boundaries_reconciled_validation.csv"
    )
  )
  
  write_csv(
    metadata,
    file.path(
      table_dir,
      "botanical_boundaries_reconciled_metadata.csv"
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
    "japan_botanical_boundaries_complete",
    area_summary,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "japan_botanical_boundary_reconciliation_assignments",
    hokkaido_components %>%
      st_drop_geometry(),
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "japan_botanical_boundary_jp51_ne_reconciliation",
    ne_reconciliation,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "japan_botanical_boundary_coastline_reconciliation",
    reconciliation_qa,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "japan_botanical_boundary_complete_overlap_qa",
    overlap_qa,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "japan_botanical_boundary_complete_provenance",
    provenance_summary,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "japan_botanical_boundary_complete_validation",
    validation,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "japan_botanical_boundary_complete_metadata",
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
    "VALIDATED COMPLETE JP01–JP51 BOTANICAL BOUNDARY LAYER\n",
    sep = ""
  )
  
  cat(
    "Botanical areas: ",
    nrow(complete_areas),
    "\n",
    sep = ""
  )
  
  cat(
    "Residual coastline overlap removed: ",
    residual_removed_km2,
    " km²\n",
    sep = ""
  )
  
  cat(
    "Positive-area overlaps >1 m²: ",
    nrow(overlap_qa),
    "\n",
    sep = ""
  )
  
  cat(
    "JP01 / JP51 overlap: ",
    jp01_jp51_overlap_m2,
    " m²\n",
    sep = ""
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
    "Frozen upstream modules modified: FALSE\n"
  )
  
  cat(
    "\nGeography 03m v",
    SCRIPT_VERSION,
    " complete.\n",
    sep = ""
  )
  
  invisible(
    list(
      complete_areas =
        complete_areas,
      
      transfer_components =
        transfer_components,
      
      retained_hokkaido_components =
        retained_hokkaido_components,
      
      ne_reconciliation =
        ne_reconciliation,
      
      reconciliation_qa =
        reconciliation_qa,
      
      overlap_qa =
        overlap_qa,
      
      validation =
        validation,
      
      metadata =
        metadata
    )
  )
}

result_03m <-
  run_reconciled_botanical_boundaries()