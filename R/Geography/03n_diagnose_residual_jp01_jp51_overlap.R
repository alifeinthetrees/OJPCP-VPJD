# ==============================================================================
# VPJD-OJPCP
# Geography 03n — Diagnose residual JP01 / JP51 overlap
# Version: 0.1.0
#
# Purpose:
#   Identify the exact retained N03 Hokkaido components responsible for the
#   residual 6.31 km² JP01 / JP51 overlap observed in Geography 03m v0.1.1.
#
# Diagnostic only:
#   - no botanical boundaries modified
#   - no occurrence assignment
#   - no coordinate modification
#   - no taxonomy modification
# ==============================================================================

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(readr)
  library(here)
})

SCRIPT_VERSION <- "0.1.0"

SOUTHERN_KURIL_CODES <- c(
  "01695", "01696", "01697",
  "01698", "01699", "01700"
)

NEMURO_CITY_CODE <- "01223"
NEMURO_OVERLAP_THRESHOLD <- 0.50

run_residual_jp01_jp51_diagnostic <- function() {
  cat("\n— VPJD residual JP01 / JP51 overlap diagnostic —\n\n")
  
  reconciliation_03l_gpkg <- here(
    "data", "interim", "geography",
    "hokkaido_kuriles_reconciliation",
    "vpjd_hokkaido_kuriles_component_reconciliation.gpkg"
  )
  
  kuriles_03i_gpkg <- here(
    "data", "interim", "geography",
    "kuriles",
    "vpjd_kuriles_final_component_decisions.gpkg"
  )
  
  output_dir <- here(
    "outputs", "tables", "geography",
    "jp01_jp51_residual_overlap"
  )
  
  gpkg_dir <- here(
    "data", "interim", "geography",
    "hokkaido_kuriles_reconciliation"
  )
  
  output_gpkg <- file.path(
    gpkg_dir,
    "vpjd_jp01_jp51_residual_overlap_diagnostic.gpkg"
  )
  
  dir.create(
    output_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  dir.create(
    gpkg_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  if (!file.exists(reconciliation_03l_gpkg)) {
    stop("Geography 03l GeoPackage not found.")
  }
  
  if (!file.exists(kuriles_03i_gpkg)) {
    stop("Geography 03i GeoPackage not found.")
  }
  
  # ---------------------------------------------------------------------------
  # Read source components
  # ---------------------------------------------------------------------------
  
  hokkaido_components <- st_read(
    reconciliation_03l_gpkg,
    layer = "all_hokkaido_components",
    quiet = TRUE
  )
  
  jp51_ne <- st_read(
    kuriles_03i_gpkg,
    layer = "jp51_include_components",
    quiet = TRUE
  )
  
  cat(
    "Hokkaido components: ",
    nrow(hokkaido_components),
    "\n",
    sep = ""
  )
  
  cat(
    "Validated Natural Earth JP51 components: ",
    nrow(jp51_ne),
    "\n",
    sep = ""
  )
  
  if (nrow(hokkaido_components) != 9555L) {
    stop("Expected 9,555 Hokkaido components.")
  }
  
  if (nrow(jp51_ne) != 22L) {
    stop("Expected 22 validated JP51 components.")
  }
  
  # ---------------------------------------------------------------------------
  # Recreate 03m Hokkaido assignment
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
        nemuro_physical_jp51
    )
  
  transfer_components <- hokkaido_components %>%
    filter(
      southern_kuril_transfer
    )
  
  retained_components <- hokkaido_components %>%
    filter(
      !southern_kuril_transfer
    )
  
  cat(
    "03m transfer components: ",
    nrow(transfer_components),
    "\n",
    sep = ""
  )
  
  cat(
    "03m retained JP01 components: ",
    nrow(retained_components),
    "\n",
    sep = ""
  )
  
  # ---------------------------------------------------------------------------
  # Recreate detailed southern JP51 geometry
  # ---------------------------------------------------------------------------
  
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
  
  # ---------------------------------------------------------------------------
  # Recreate 03m Natural Earth replacement logic
  # ---------------------------------------------------------------------------
  
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
  
  candidates <- lengths(
    st_intersects(
      jp51_ne_eq,
      southern_n03_eq,
      sparse = TRUE
    )
  ) > 0L
  
  for (i in which(candidates)) {
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
      represented_by_n03 =
        ne_overlap_fraction > 0.50
    )
  
  ne_retained <- jp51_ne %>%
    filter(
      !represented_by_n03
    ) %>%
    st_transform(
      st_crs(southern_n03)
    )
  
  cat(
    "Natural Earth components retained in final JP51: ",
    nrow(ne_retained),
    "\n",
    sep = ""
  )
  
  # ---------------------------------------------------------------------------
  # Recreate final JP51
  # ---------------------------------------------------------------------------
  
  jp51_final <- st_sf(
    botanical_area_id = "JP51",
    geom = st_union(
      c(
        st_geometry(southern_n03),
        st_geometry(ne_retained)
      )
    )
  )
  
  st_crs(jp51_final) <-
    st_crs(southern_n03)
  
  cat(
    "Reconstructed JP51 valid: ",
    all(st_is_valid(jp51_final)),
    "\n\n",
    sep = ""
  )
  
  # ---------------------------------------------------------------------------
  # Test each retained JP01 component against final JP51
  # ---------------------------------------------------------------------------
  
  cat(
    "Testing retained JP01 components against final JP51...\n"
  )
  
  retained_eq <- st_transform(
    retained_components,
    6933
  )
  
  jp51_eq <- st_transform(
    jp51_final,
    6933
  )
  
  candidate_index <- which(
    lengths(
      st_intersects(
        retained_eq,
        jp51_eq,
        sparse = TRUE
      )
    ) > 0L
  )
  
  cat(
    "Retained components spatially intersecting JP51: ",
    length(candidate_index),
    "\n",
    sep = ""
  )
  
  overlap_m2 <- numeric(
    length(candidate_index)
  )
  
  for (k in seq_along(candidate_index)) {
    i <- candidate_index[k]
    
    inter <- suppressWarnings(
      st_intersection(
        st_geometry(retained_eq)[i],
        st_geometry(jp51_eq)[1]
      )
    )
    
    if (length(inter) > 0L) {
      overlap_m2[k] <- sum(
        as.numeric(
          st_area(inter)
        ),
        na.rm = TRUE
      )
    }
  }
  
  component_area_m2 <- as.numeric(
    st_area(
      retained_eq[candidate_index, ]
    )
  )
  
  overlap_percent_component <- ifelse(
    component_area_m2 > 0,
    100 * overlap_m2 /
      component_area_m2,
    NA_real_
  )
  
  residual <- retained_components[
    candidate_index,
  ] %>%
    mutate(
      residual_overlap_m2 =
        overlap_m2,
      
      residual_overlap_km2 =
        overlap_m2 / 1e6,
      
      component_area_km2 =
        component_area_m2 / 1e6,
      
      residual_overlap_percent_component =
        overlap_percent_component
    ) %>%
    filter(
      residual_overlap_m2 > 1
    ) %>%
    arrange(
      desc(residual_overlap_m2)
    )
  
  cat(
    "Components with positive overlap >1 m²: ",
    nrow(residual),
    "\n",
    sep = ""
  )
  
  residual_total_m2 <- sum(
    residual$residual_overlap_m2,
    na.rm = TRUE
  )
  
  cat(
    "Total residual overlap: ",
    residual_total_m2,
    " m² (",
    residual_total_m2 / 1e6,
    " km²)\n\n",
    sep = ""
  )
  
  # ---------------------------------------------------------------------------
  # Diagnostic table
  # ---------------------------------------------------------------------------
  
  residual_table <- residual %>%
    st_drop_geometry() %>%
    select(
      hokkaido_component_id,
      N03_001,
      N03_002,
      N03_003,
      N03_004,
      N03_007,
      jp51_overlap_percent,
      jp51_spatial_class,
      component_area_km2,
      residual_overlap_km2,
      residual_overlap_percent_component
    )
  
  cat(
    "— Residual overlapping components —\n"
  )
  
  print.data.frame(
    as.data.frame(residual_table),
    row.names = FALSE
  )
  
  # ---------------------------------------------------------------------------
  # Summary by administrative identity
  # ---------------------------------------------------------------------------
  
  admin_summary <- residual %>%
    st_drop_geometry() %>%
    group_by(
      N03_002,
      N03_003,
      N03_004,
      N03_007
    ) %>%
    summarise(
      components = n(),
      component_area_km2 =
        sum(
          component_area_km2,
          na.rm = TRUE
        ),
      residual_overlap_km2 =
        sum(
          residual_overlap_km2,
          na.rm = TRUE
        ),
      .groups = "drop"
    ) %>%
    arrange(
      desc(residual_overlap_km2)
    )
  
  cat(
    "\n— Residual overlap by N03 administrative identity —\n"
  )
  
  print.data.frame(
    as.data.frame(admin_summary),
    row.names = FALSE
  )
  
  # ---------------------------------------------------------------------------
  # Validation
  # ---------------------------------------------------------------------------
  
  validation <- tibble(
    check = c(
      "source_hokkaido_components_9555",
      "source_jp51_components_22",
      "03m_transfer_components_830",
      "03m_retained_components_8725",
      "jp51_geometry_valid",
      "residual_overlap_detected",
      "residual_overlap_approximately_6_31_km2",
      "no_boundary_modification",
      "no_occurrence_assignment",
      "no_coordinate_modification",
      "no_taxonomy_modification"
    ),
    
    pass = c(
      nrow(hokkaido_components) == 9555L,
      nrow(jp51_ne) == 22L,
      nrow(transfer_components) == 830L,
      nrow(retained_components) == 8725L,
      all(st_is_valid(jp51_final)),
      nrow(residual) > 0L,
      abs(
        residual_total_m2 / 1e6 -
          6.30995
      ) < 0.01,
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
    "\nAll diagnostic validation checks PASS: ",
    all_pass,
    "\n",
    sep = ""
  )
  
  # ---------------------------------------------------------------------------
  # Outputs
  # ---------------------------------------------------------------------------
  
  write_csv(
    residual_table,
    file.path(
      output_dir,
      "jp01_jp51_residual_overlap_components.csv"
    )
  )
  
  write_csv(
    admin_summary,
    file.path(
      output_dir,
      "jp01_jp51_residual_overlap_admin_summary.csv"
    )
  )
  
  write_csv(
    validation,
    file.path(
      output_dir,
      "jp01_jp51_residual_overlap_validation.csv"
    )
  )
  
  if (file.exists(output_gpkg)) {
    file.remove(output_gpkg)
  }
  
  if (nrow(residual) > 0L) {
    st_write(
      residual,
      output_gpkg,
      layer =
        "residual_overlap_components",
      quiet = TRUE
    )
  }
  
  st_write(
    jp51_final,
    output_gpkg,
    layer =
      "reconstructed_jp51",
    quiet = TRUE
  )
  
  cat(
    "\nOutput status: ",
    "DIAGNOSTIC RESIDUAL JP01 / JP51 OVERLAP\n",
    sep = ""
  )
  
  cat(
    "Residual components: ",
    nrow(residual),
    "\n",
    sep = ""
  )
  
  cat(
    "Residual overlap: ",
    residual_total_m2 / 1e6,
    " km²\n",
    sep = ""
  )
  
  cat(
    "Boundary modification performed: FALSE\n"
  )
  
  cat(
    "Occurrence assignment performed: FALSE\n"
  )
  
  cat(
    "\nGeography 03n v",
    SCRIPT_VERSION,
    " complete.\n",
    sep = ""
  )
  
  invisible(
    list(
      residual_components =
        residual,
      residual_table =
        residual_table,
      admin_summary =
        admin_summary,
      validation =
        validation
    )
  )
}

result_03n <-
  run_residual_jp01_jp51_diagnostic()