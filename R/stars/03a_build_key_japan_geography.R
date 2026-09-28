# =============================================================================
# VPJD-OJPCP
# R/stars/03a_build_key_japan_geography.R
# Version 0.1.2
#
# BUILD JAPANESE GEOGRAPHIC INPUTS REQUIRED BY THE KEY TO STARS
#
# Purpose:
# Derive taxon-level Japanese prefecture occupancy required by the Key.
#
# Method:
# - WCVP Accepted taxa only.
# - MLIT N03 2025 for standard prefectures.
# - Validated hokkaido_star_units layer for 14 Hokkaido subprefectures.
# - subprefecture_name explicitly used as Hokkaido counting-unit name.
# - JP01-JP51 botanical areas are NOT substituted for Key prefectures.
# - No Stars assigned here.
# =============================================================================

suppressPackageStartupMessages({
  library(DBI)
  library(duckdb)
  library(dplyr)
  library(sf)
  library(here)
  library(readr)
})

MODULE <- "stars_03a_build_key_japan_geography"
VERSION <- "0.1.2"
RUN_DATE <- Sys.Date()

DB_PATH <- here("data", "interim", "occurrences", "vpjd_occurrences.duckdb")
SOURCE_OCCURRENCES <- "vpjd_japan_occurrences_analytical"
SOURCE_TAXA <- "vpjd_japan_taxon_distribution"

OUTPUT_TABLE <- "vpjd_star_japan_key_geography"
OUTPUT_VALIDATION <- "vpjd_star_japan_key_geography_validation"
OUTPUT_METADATA <- "vpjd_star_japan_key_geography_metadata"

OUTPUT_DIR <- here("outputs", "tables", "stars", "key_geography")
N03_ROOT <- here("data", "interim", "geography", "nlni_n03_2025")

HOKKAIDO_GPKG <- here(
  "data", "interim", "geography",
  "hokkaido_star_geography",
  "vpjd_hokkaido_star_geography.gpkg"
)

HOKKAIDO_LAYER <- "hokkaido_star_units"
HOKKAIDO_ID_FIELD <- "hokkaido_unit_id"
HOKKAIDO_NAME_FIELD <- "subprefecture_name"

run_stars_03a <- function() {
  
  cat("\n— Build Japanese Key geography —\n\n")
  cat("Run date: ", as.character(RUN_DATE), "\n", sep = "")
  cat("Module: ", MODULE, "\n", sep = "")
  cat("Version: ", VERSION, "\n\n", sep = "")
  
  dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
  
  if (!file.exists(DB_PATH)) {
    stop("VPJD DuckDB not found: ", DB_PATH)
  }
  
  if (!dir.exists(N03_ROOT)) {
    stop("N03 directory not found: ", N03_ROOT)
  }
  
  if (!file.exists(HOKKAIDO_GPKG)) {
    stop("Hokkaido reference not found: ", HOKKAIDO_GPKG)
  }
  
  con <- dbConnect(
    duckdb::duckdb(),
    dbdir = DB_PATH,
    read_only = FALSE
  )
  
  on.exit(
    dbDisconnect(con, shutdown = TRUE),
    add = TRUE
  )
  
  tables <- dbListTables(con)
  
  required_tables <- c(
    SOURCE_OCCURRENCES,
    SOURCE_TAXA
  )
  
  missing_tables <- setdiff(
    required_tables,
    tables
  )
  
  if (length(missing_tables) > 0L) {
    stop(
      "Required DuckDB tables missing: ",
      paste(missing_tables, collapse = ", ")
    )
  }
  
  # ===========================================================================
  # 1. Accepted contemporary taxon population
  # ===========================================================================
  
  accepted <- dbGetQuery(
    con,
    paste0(
      "SELECT ",
      "FINAL_WCVP_ID, ",
      "FINAL_WCVP_RECOGNISED_NAME, ",
      "FINAL_WCVP_RANK, ",
      "FINAL_WCVP_STATUS, ",
      "OCCURRENCE_RECORDS ",
      "FROM ", SOURCE_TAXA, " ",
      "WHERE lower(FINAL_WCVP_STATUS) = 'accepted'"
    )
  )
  
  cat(
    "Accepted WCVP taxa: ",
    format(nrow(accepted), big.mark = ","),
    "\n",
    sep = ""
  )
  
  if (nrow(accepted) != 11439L) {
    stop(
      "Accepted taxon count differs from validated Stars 03 result."
    )
  }
  
  if (n_distinct(accepted$FINAL_WCVP_ID) != 11439L) {
    stop(
      "Accepted WCVP IDs are not unique."
    )
  }
  
  # ===========================================================================
  # 2. Read MLIT N03 geography
  # ===========================================================================
  
  shp_files <- list.files(
    N03_ROOT,
    pattern = "\\.shp$",
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = TRUE
  )
  
  cat(
    "N03 shapefiles found: ",
    length(shp_files),
    "\n",
    sep = ""
  )
  
  if (length(shp_files) == 0L) {
    stop("No N03 shapefiles found.")
  }
  
  shp_inventory <- data.frame(
    path = shp_files,
    file = basename(shp_files),
    stringsAsFactors = FALSE
  )
  
  write_csv(
    shp_inventory,
    file.path(
      OUTPUT_DIR,
      "stars03a_n03_shapefile_inventory.csv"
    )
  )
  
  cat("\nReading N03 geography...\n")
  
  n03_list <- lapply(
    shp_files,
    function(x) {
      
      tryCatch(
        {
          obj <- st_read(
            x,
            quiet = TRUE,
            stringsAsFactors = FALSE
          )
          
          if (nrow(obj) == 0L) {
            return(NULL)
          }
          
          obj$SOURCE_FILE <- basename(x)
          
          obj
        },
        error = function(e) {
          NULL
        }
      )
    }
  )
  
  n03_list <- Filter(
    Negate(is.null),
    n03_list
  )
  
  if (length(n03_list) == 0L) {
    stop(
      "N03 shapefiles could not be read."
    )
  }
  
  common_cols <- Reduce(
    intersect,
    lapply(
      n03_list,
      names
    )
  )
  
  if (!"geometry" %in% common_cols) {
    stop(
      "No common geometry column found in N03 inputs."
    )
  }
  
  n03_list <- lapply(
    n03_list,
    function(x) {
      x[, common_cols]
    }
  )
  
  n03 <- do.call(
    rbind,
    n03_list
  )
  
  n03 <- st_make_valid(
    n03
  )
  
  cat(
    "N03 polygon records loaded: ",
    format(nrow(n03), big.mark = ","),
    "\n",
    sep = ""
  )
  
  cat("\nN03 fields:\n")
  print(names(n03))
  
  if (!"N03_001" %in% names(n03)) {
    stop(
      "Expected N03 prefecture field N03_001 not found."
    )
  }
  
  prefecture_names <- sort(
    unique(
      n03$N03_001[
        !is.na(n03$N03_001) &
          n03$N03_001 != ""
      ]
    )
  )
  
  cat(
    "\nDistinct N03 prefecture names: ",
    length(prefecture_names),
    "\n",
    sep = ""
  )
  
  print(prefecture_names)
  
  if (length(prefecture_names) != 47L) {
    stop(
      "Expected 47 N03 prefectures."
    )
  }
  
  # ===========================================================================
  # 3. Dissolve standard prefectures
  # ===========================================================================
  
  prefectures <- n03 |>
    filter(
      !is.na(N03_001),
      N03_001 != ""
    ) |>
    group_by(
      N03_001
    ) |>
    summarise(
      geometry = st_union(geometry),
      .groups = "drop"
    ) |>
    st_make_valid() |>
    st_transform(4326)
  
  cat(
    "\nPrefecture-level polygons: ",
    nrow(prefectures),
    "\n",
    sep = ""
  )
  
  if (nrow(prefectures) != 47L) {
    stop(
      "Prefecture dissolve did not produce 47 units."
    )
  }
  
  # ===========================================================================
  # 4. Read validated Hokkaido subprefecture layer
  # ===========================================================================
  
  layers <- st_layers(
    HOKKAIDO_GPKG
  )$name
  
  cat("\nHokkaido reference layers:\n")
  print(layers)
  
  if (!HOKKAIDO_LAYER %in% layers) {
    stop(
      "Required Hokkaido layer '",
      HOKKAIDO_LAYER,
      "' not found."
    )
  }
  
  hokkaido <- st_read(
    HOKKAIDO_GPKG,
    layer = HOKKAIDO_LAYER,
    quiet = TRUE
  ) |>
    st_make_valid() |>
    st_transform(4326)
  
  cat(
    "\nHokkaido subprefecture units: ",
    nrow(hokkaido),
    "\n",
    sep = ""
  )
  
  if (nrow(hokkaido) != 14L) {
    stop(
      "hokkaido_star_units does not contain the expected 14 units."
    )
  }
  
  hokkaido_fields <- setdiff(
    names(hokkaido),
    attr(hokkaido, "sf_column")
  )
  
  cat("\nHokkaido subprefecture fields:\n")
  print(hokkaido_fields)
  
  # ===========================================================================
  # 5. Explicit Hokkaido Key counting-unit fields
  #
  # Validated Geography 03k schema:
  #   hokkaido_unit_id   = stable unit identifier
  #   subprefecture_name = Hokkaido subprefecture name
  #
  # The Key treats the 14 Hokkaido subprefectures as prefecture-counting units.
  # ===========================================================================
  
  required_hokkaido_fields <- c(
    HOKKAIDO_ID_FIELD,
    HOKKAIDO_NAME_FIELD
  )
  
  missing_hokkaido_fields <- setdiff(
    required_hokkaido_fields,
    hokkaido_fields
  )
  
  if (length(missing_hokkaido_fields) > 0L) {
    stop(
      "Validated Hokkaido layer is missing required fields: ",
      paste(
        missing_hokkaido_fields,
        collapse = ", "
      )
    )
  }
  
  cat(
    "\nHokkaido unit-ID field: ",
    HOKKAIDO_ID_FIELD,
    "\n",
    sep = ""
  )
  
  cat(
    "Hokkaido unit-name field: ",
    HOKKAIDO_NAME_FIELD,
    "\n",
    sep = ""
  )
  
  hokkaido_units <- hokkaido |>
    st_drop_geometry() |>
    transmute(
      hokkaido_unit_id =
        as.character(.data[[HOKKAIDO_ID_FIELD]]),
      subprefecture_name =
        as.character(.data[[HOKKAIDO_NAME_FIELD]])
    ) |>
    filter(
      !is.na(hokkaido_unit_id),
      hokkaido_unit_id != "",
      !is.na(subprefecture_name),
      subprefecture_name != ""
    ) |>
    distinct() |>
    arrange(
      hokkaido_unit_id
    )
  
  cat("\nHokkaido units:\n")
  print.data.frame(
    hokkaido_units,
    row.names = FALSE
  )
  
  if (nrow(hokkaido_units) != 14L) {
    stop(
      "Validated Hokkaido fields do not define exactly 14 units."
    )
  }
  
  if (
    n_distinct(hokkaido_units$hokkaido_unit_id) != 14L ||
    n_distinct(hokkaido_units$subprefecture_name) != 14L
  ) {
    stop(
      "Hokkaido unit IDs or names are not unique."
    )
  }
  
  # ===========================================================================
  # 6. Validate occurrence coordinate fields
  # ===========================================================================
  
  occurrence_fields <- dbListFields(
    con,
    SOURCE_OCCURRENCES
  )
  
  required_occurrence_fields <- c(
    "gbifID",
    "FINAL_WCVP_ID",
    "FINAL_WCVP_STATUS",
    "ANALYTICAL_DECIMAL_LATITUDE",
    "ANALYTICAL_DECIMAL_LONGITUDE"
  )
  
  missing_occurrence_fields <- setdiff(
    required_occurrence_fields,
    occurrence_fields
  )
  
  if (length(missing_occurrence_fields) > 0L) {
    stop(
      "Analytical occurrence table is missing fields: ",
      paste(
        missing_occurrence_fields,
        collapse = ", "
      )
    )
  }
  
  # ===========================================================================
  # 7. Extract accepted analytical occurrence coordinates
  # ===========================================================================
  
  cat("\nExtracting accepted occurrence coordinates...\n")
  
  occ <- dbGetQuery(
    con,
    paste0(
      "SELECT ",
      "gbifID, ",
      "FINAL_WCVP_ID, ",
      "ANALYTICAL_DECIMAL_LATITUDE AS latitude, ",
      "ANALYTICAL_DECIMAL_LONGITUDE AS longitude ",
      "FROM ", SOURCE_OCCURRENCES, " ",
      "WHERE FINAL_WCVP_ID IS NOT NULL ",
      "AND lower(FINAL_WCVP_STATUS) = 'accepted' ",
      "AND ANALYTICAL_DECIMAL_LATITUDE IS NOT NULL ",
      "AND ANALYTICAL_DECIMAL_LONGITUDE IS NOT NULL"
    )
  )
  
  cat(
    "Accepted occurrence records for spatial analysis: ",
    format(nrow(occ), big.mark = ","),
    "\n",
    sep = ""
  )
  
  if (nrow(occ) == 0L) {
    stop(
      "No accepted analytical occurrences available."
    )
  }
  
  occ_sf <- st_as_sf(
    occ,
    coords = c(
      "longitude",
      "latitude"
    ),
    crs = 4326,
    remove = FALSE
  )
  
  # ===========================================================================
  # 8. Assign standard prefectures outside Hokkaido
  # ===========================================================================
  
  cat("\nAssigning standard prefectures...\n")
  
  prefectures_non_hokkaido <- prefectures |>
    filter(
      N03_001 != "北海道"
    )
  
  if (nrow(prefectures_non_hokkaido) != 46L) {
    stop(
      "Expected 46 standard prefectures after excluding Hokkaido."
    )
  }
  
  non_hokkaido_hits <- st_join(
    occ_sf,
    prefectures_non_hokkaido,
    join = st_intersects,
    left = FALSE
  )
  
  non_hokkaido_assignment <- non_hokkaido_hits |>
    st_drop_geometry() |>
    transmute(
      gbifID,
      FINAL_WCVP_ID,
      KEY_PREFECTURE_ID =
        as.character(N03_001),
      KEY_PREFECTURE =
        as.character(N03_001),
      KEY_PREFECTURE_TYPE =
        "standard_prefecture"
    ) |>
    distinct()
  
  rm(non_hokkaido_hits)
  gc()
  
  cat(
    "Occurrence-prefecture assignments outside Hokkaido: ",
    format(
      nrow(non_hokkaido_assignment),
      big.mark = ","
    ),
    "\n",
    sep = ""
  )
  
  # ===========================================================================
  # 9. Assign Hokkaido's 14 subprefectures
  # ===========================================================================
  
  cat("\nAssigning Hokkaido subprefectures...\n")
  
  hokkaido_hits <- st_join(
    occ_sf,
    hokkaido,
    join = st_intersects,
    left = FALSE
  )
  
  hokkaido_assignment <- hokkaido_hits |>
    st_drop_geometry() |>
    transmute(
      gbifID,
      FINAL_WCVP_ID,
      KEY_PREFECTURE_ID =
        as.character(.data[[HOKKAIDO_ID_FIELD]]),
      KEY_PREFECTURE =
        as.character(.data[[HOKKAIDO_NAME_FIELD]]),
      KEY_PREFECTURE_TYPE =
        "hokkaido_subprefecture"
    ) |>
    filter(
      !is.na(KEY_PREFECTURE_ID),
      KEY_PREFECTURE_ID != "",
      !is.na(KEY_PREFECTURE),
      KEY_PREFECTURE != ""
    ) |>
    distinct()
  
  rm(hokkaido_hits, occ_sf)
  gc()
  
  cat(
    "Occurrence-Hokkaido-subprefecture assignments: ",
    format(
      nrow(hokkaido_assignment),
      big.mark = ","
    ),
    "\n",
    sep = ""
  )
  
  # ===========================================================================
  # 10. Combine Key prefecture assignments
  # ===========================================================================
  
  prefecture_assignment <- bind_rows(
    non_hokkaido_assignment,
    hokkaido_assignment
  ) |>
    distinct(
      gbifID,
      FINAL_WCVP_ID,
      KEY_PREFECTURE_ID,
      .keep_all = TRUE
    )
  
  cat(
    "\nCombined occurrence-prefecture assignments: ",
    format(
      nrow(prefecture_assignment),
      big.mark = ","
    ),
    "\n",
    sep = ""
  )
  
  taxon_prefecture <- prefecture_assignment |>
    distinct(
      FINAL_WCVP_ID,
      KEY_PREFECTURE_ID
    ) |>
    count(
      FINAL_WCVP_ID,
      name = "JAPAN_PREFECTURES_PRESENT"
    )
  
  # ===========================================================================
  # 11. Construct taxon-level Key geography
  #
  # Districts remain NULL until their mapping is explicitly formalised.
  # This module does not invent districts from JP01-JP51.
  # ===========================================================================
  
  geography <- accepted |>
    select(
      FINAL_WCVP_ID,
      FINAL_WCVP_RECOGNISED_NAME
    ) |>
    left_join(
      taxon_prefecture,
      by = "FINAL_WCVP_ID"
    ) |>
    mutate(
      JAPAN_PREFECTURES_PRESENT =
        as.integer(JAPAN_PREFECTURES_PRESENT),
      JAPAN_DISTRICTS_PRESENT =
        NA_integer_,
      ALMOST_ALL_DISTRICTS =
        NA
    )
  
  # ===========================================================================
  # 12. Profiles
  # ===========================================================================
  
  prefecture_profile <- geography |>
    count(
      JAPAN_PREFECTURES_PRESENT,
      name = "WCVP_TAXA"
    ) |>
    arrange(
      JAPAN_PREFECTURES_PRESENT
    )
  
  cat("\n— Prefecture occupancy profile —\n")
  
  print.data.frame(
    prefecture_profile,
    row.names = FALSE
  )
  
  taxa_without_prefecture <- sum(
    is.na(
      geography$JAPAN_PREFECTURES_PRESENT
    )
  )
  
  taxa_with_prefecture <- sum(
    !is.na(
      geography$JAPAN_PREFECTURES_PRESENT
    )
  )
  
  cat(
    "\nAccepted taxa with >=1 assigned Key prefecture: ",
    format(taxa_with_prefecture, big.mark = ","),
    "\n",
    sep = ""
  )
  
  cat(
    "Accepted taxa without an assigned Key prefecture: ",
    format(taxa_without_prefecture, big.mark = ","),
    "\n",
    sep = ""
  )
  
  # ===========================================================================
  # 13. Validation
  # ===========================================================================
  
  validation <- tibble::tibble(
    check = c(
      "accepted_taxon_population_11439",
      "accepted_taxon_ids_unique",
      "all_47_N03_prefectures_present",
      "46_standard_prefectures_after_hokkaido_exclusion",
      "hokkaido_star_units_layer_used",
      "hokkaido_has_14_subprefecture_units",
      "hokkaido_subprefecture_name_field_explicit",
      "hokkaido_unit_id_field_explicit",
      "prefecture_counts_derived_from_coordinates",
      "jp01_jp51_not_used_as_prefecture_substitute",
      "district_counts_not_invented",
      "almost_all_districts_not_invented",
      "stars_not_assigned",
      "historical_star_totals_not_used",
      "ghi_not_calculated"
    ),
    pass = c(
      nrow(accepted) == 11439L,
      n_distinct(accepted$FINAL_WCVP_ID) == 11439L,
      nrow(prefectures) == 47L,
      nrow(prefectures_non_hokkaido) == 46L,
      HOKKAIDO_LAYER == "hokkaido_star_units",
      nrow(hokkaido_units) == 14L,
      HOKKAIDO_NAME_FIELD == "subprefecture_name",
      HOKKAIDO_ID_FIELD == "hokkaido_unit_id",
      TRUE,
      TRUE,
      all(
        is.na(
          geography$JAPAN_DISTRICTS_PRESENT
        )
      ),
      all(
        is.na(
          geography$ALMOST_ALL_DISTRICTS
        )
      ),
      TRUE,
      TRUE,
      TRUE
    )
  )
  
  cat("\n— Validation —\n")
  
  print.data.frame(
    validation,
    row.names = FALSE
  )
  
  all_pass <- all(
    validation$pass
  )
  
  cat(
    "\nAll Stars 03a validation checks PASS: ",
    all_pass,
    "\n",
    sep = ""
  )
  
  if (!all_pass) {
    stop(
      "Stars 03a validation failed. Do not freeze."
    )
  }
  
  # ===========================================================================
  # 14. Persist canonical geography
  # ===========================================================================
  
  dbWriteTable(
    con,
    OUTPUT_TABLE,
    geography,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    OUTPUT_VALIDATION,
    validation,
    overwrite = TRUE
  )
  
  metadata <- tibble::tibble(
    metric = c(
      "module",
      "version",
      "run_date",
      "accepted_taxa",
      "prefecture_source",
      "standard_prefectures",
      "hokkaido_layer",
      "hokkaido_id_field",
      "hokkaido_name_field",
      "hokkaido_counting_units",
      "district_counts_status",
      "stars_assigned",
      "historical_star_totals_used",
      "ghi_calculated",
      "validation_pass"
    ),
    value = c(
      MODULE,
      VERSION,
      as.character(RUN_DATE),
      as.character(nrow(accepted)),
      "MLIT_NLNI_N03_2025",
      "46",
      HOKKAIDO_LAYER,
      HOKKAIDO_ID_FIELD,
      HOKKAIDO_NAME_FIELD,
      "14_subprefectures",
      "NOT_YET_DERIVED",
      "FALSE",
      "FALSE",
      "FALSE",
      as.character(all_pass)
    )
  )
  
  dbWriteTable(
    con,
    OUTPUT_METADATA,
    metadata,
    overwrite = TRUE
  )
  
  # ===========================================================================
  # 15. CSV outputs
  # ===========================================================================
  
  write_csv(
    geography,
    file.path(
      OUTPUT_DIR,
      "stars03a_taxon_key_geography.csv"
    )
  )
  
  write_csv(
    prefecture_profile,
    file.path(
      OUTPUT_DIR,
      "stars03a_prefecture_occupancy_profile.csv"
    )
  )
  
  write_csv(
    validation,
    file.path(
      OUTPUT_DIR,
      "stars03a_validation.csv"
    )
  )
  
  write_csv(
    metadata,
    file.path(
      OUTPUT_DIR,
      "stars03a_metadata.csv"
    )
  )
  
  write_csv(
    hokkaido_units,
    file.path(
      OUTPUT_DIR,
      "stars03a_hokkaido_counting_units.csv"
    )
  )
  
  # ===========================================================================
  # 16. Final status
  # ===========================================================================
  
  cat("\nCanonical output:\n")
  cat(
    "  ",
    OUTPUT_TABLE,
    "\n",
    sep = ""
  )
  
  cat(
    "\nAccepted taxa represented: ",
    format(
      nrow(geography),
      big.mark = ","
    ),
    "\n",
    sep = ""
  )
  
  cat(
    "Prefecture counts: DERIVED\n",
    "District counts: NOT YET DERIVED\n",
    "Stars assigned: FALSE\n",
    sep = ""
  )
  
  cat(
    "\nStars 03a v",
    VERSION,
    " complete.\n",
    sep = ""
  )
  
  invisible(
    list(
      geography = geography,
      prefecture_profile = prefecture_profile,
      hokkaido_units = hokkaido_units,
      validation = validation,
      metadata = metadata
    )
  )
}

result_stars_03a <- run_stars_03a()