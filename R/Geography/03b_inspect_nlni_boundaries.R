# ==============================================================================
# VPJD-OJPCP
# 03b_inspect_nlni_boundaries.R
# Version: 0.1.1
#
# Purpose:
#   Inspect the official NLNI N03 2025 administrative-area spatial data.
#
# Important:
#   The dedicated Hokkaido subprefecture shapefile is retained and inspected
#   separately. It is NOT combined with the 47 main prefectural N03 layers.
#
# This module is diagnostic only.
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
      stop("Expected one metadata row for ", metric_name, ".")
    }
    return(as.character(x$value[[1]]))
  }
  stop("Metadata item not found: ", metric_name)
}

run_nlni_inspection <- function() {
  cat("\n— VPJD NLNI N03 spatial inspection —\n\n")
  
  db_path <- here(
    "data", "interim", "occurrences",
    "vpjd_occurrences.duckdb"
  )
  
  con <- dbConnect(
    duckdb::duckdb(),
    dbdir = db_path,
    read_only = FALSE
  )
  
  on.exit(
    dbDisconnect(con, shutdown = TRUE),
    add = TRUE
  )
  
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
  
  if (g02 != EXPECTED_GEOGRAPHY_02_VERSION) {
    stop("Expected Geography 02 v0.1.0.")
  }
  
  if (g03a != EXPECTED_GEOGRAPHY_03A_VERSION) {
    stop("Expected Geography 03a v0.1.0.")
  }
  
  cat(
    "Geography 02 version: ", g02, "\n",
    "Geography 03a version: ", g03a, "\n\n",
    sep = ""
  )
  
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
  
  if (length(shp_files) != 48) {
    stop(
      "Expected 48 shapefiles; found ",
      length(shp_files), "."
    )
  }
  
  main_shp <- shp_files[
    !grepl(
      "_subprefecture\\.shp$",
      shp_files,
      ignore.case = TRUE
    )
  ]
  
  subpref_shp <- shp_files[
    grepl(
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
  
  if (length(subpref_shp) != 1) {
    stop(
      "Expected one Hokkaido subprefecture shapefile; found ",
      length(subpref_shp), "."
    )
  }
  
  cat("Total shapefiles: 48\n")
  cat("Main prefectural layers: 47\n")
  cat("Hokkaido subprefecture layers: 1\n\n")
  
  get_prefecture_id <- function(path) {
    b <- basename(path)
    code <- sub(
      "^N03-[0-9]+_([0-9]{2}).*$",
      "\\1",
      b
    )
    code
  }
  
  prefecture_lookup <- tibble(
    jis_code = sprintf("%02d", 1:47),
    prefecture_en = c(
      "Hokkaido", "Aomori", "Iwate", "Miyagi",
      "Akita", "Yamagata", "Fukushima", "Ibaraki",
      "Tochigi", "Gunma", "Saitama", "Chiba",
      "Tokyo", "Kanagawa", "Niigata", "Toyama",
      "Ishikawa", "Fukui", "Yamanashi", "Nagano",
      "Gifu", "Shizuoka", "Aichi", "Mie",
      "Shiga", "Kyoto", "Osaka", "Hyogo",
      "Nara", "Wakayama", "Tottori", "Shimane",
      "Okayama", "Hiroshima", "Yamaguchi",
      "Tokushima", "Kagawa", "Ehime", "Kochi",
      "Fukuoka", "Saga", "Nagasaki", "Kumamoto",
      "Oita", "Miyazaki", "Kagoshima", "Okinawa"
    )
  )
  
  cat("— Reading 47 main N03 layers —\n\n")
  
  admin_list <- vector("list", length(main_shp))
  layer_profile_list <- vector("list", length(main_shp))
  
  for (i in seq_along(main_shp)) {
    path <- main_shp[[i]]
    code <- get_prefecture_id(path)
    
    pref_en <- prefecture_lookup %>%
      filter(jis_code == code) %>%
      pull(prefecture_en)
    
    cat(
      sprintf(
        "[%02d/47] %-10s %s\n",
        i, pref_en, basename(path)
      )
    )
    
    x <- sf::st_read(
      path,
      quiet = TRUE,
      stringsAsFactors = FALSE
    )
    
    required_fields <- c(
      "N03_001", "N03_002", "N03_003",
      "N03_004", "N03_007"
    )
    
    if (!all(required_fields %in% names(x))) {
      stop(
        "Required N03 fields missing from ",
        basename(path), "."
      )
    }
    
    b <- sf::st_bbox(x)
    
    layer_profile_list[[i]] <- tibble(
      jis_code = code,
      prefecture_en = pref_en,
      shapefile = basename(path),
      feature_count = nrow(x),
      geometry_types = paste(
        unique(
          as.character(
            sf::st_geometry_type(
              x,
              by_geometry = TRUE
            )
          )
        ),
        collapse = ";"
      ),
      crs_epsg = sf::st_crs(x)$epsg,
      xmin = as.numeric(b["xmin"]),
      ymin = as.numeric(b["ymin"]),
      xmax = as.numeric(b["xmax"]),
      ymax = as.numeric(b["ymax"])
    )
    
    admin_list[[i]] <- x %>%
      select(
        N03_001,
        N03_002,
        N03_003,
        N03_004,
        N03_007
      )
  }
  
  layer_profile <- bind_rows(
    layer_profile_list
  )
  
  target_crs <- sf::st_crs(
    admin_list[[1]]
  )
  
  admin_list <- lapply(
    admin_list,
    function(x) {
      if (sf::st_crs(x) != target_crs) {
        sf::st_transform(x, target_crs)
      } else {
        x
      }
    }
  )
  
  admin <- do.call(
    rbind,
    admin_list
  )
  
  cat(
    "\nCombined main administrative features: ",
    format(nrow(admin), big.mark = ","),
    "\n",
    sep = ""
  )
  
  cat("\n— Reading Hokkaido subprefecture layer separately —\n\n")
  
  hokkaido_sub <- sf::st_read(
    subpref_shp[[1]],
    quiet = TRUE,
    stringsAsFactors = FALSE
  )
  
  cat(
    "Hokkaido subprefecture-layer features: ",
    format(nrow(hokkaido_sub), big.mark = ","),
    "\n",
    sep = ""
  )
  
  cat(
    "Hokkaido subprefecture-layer CRS: EPSG:",
    sf::st_crs(hokkaido_sub)$epsg,
    "\n\n",
    sep = ""
  )
  
  # ---------------------------------------------------------------------------
  # Main field profile
  # ---------------------------------------------------------------------------
  
  fields <- c(
    "N03_001",
    "N03_002",
    "N03_003",
    "N03_004",
    "N03_007"
  )
  
  admin_profile <- tibble(
    field = fields,
    nonmissing = vapply(
      fields,
      function(f) {
        z <- as.character(admin[[f]])
        sum(!is.na(z) & trimws(z) != "")
      },
      integer(1)
    ),
    distinct_values = vapply(
      fields,
      function(f) {
        z <- as.character(admin[[f]])
        dplyr::n_distinct(
          z[!is.na(z) & trimws(z) != ""]
        )
      },
      integer(1)
    )
  )
  
  # ---------------------------------------------------------------------------
  # Hokkaido subprefecture profile
  # ---------------------------------------------------------------------------
  
  hokkaido_sub_profile <- hokkaido_sub %>%
    sf::st_drop_geometry() %>%
    transmute(
      prefecture = as.character(N03_001),
      subprefecture = as.character(N03_002),
      district = as.character(N03_003),
      municipality = as.character(N03_004),
      admin_code = as.character(N03_007)
    ) %>%
    filter(
      !is.na(subprefecture),
      trimws(subprefecture) != ""
    ) %>%
    count(
      subprefecture,
      name = "spatial_features"
    ) %>%
    arrange(subprefecture) %>%
    as_tibble()
  
  # ---------------------------------------------------------------------------
  # Special prefectures
  # ---------------------------------------------------------------------------
  
  special_jp <- c(
    "北海道",
    "東京都",
    "鹿児島県",
    "沖縄県"
  )
  
  special <- admin %>%
    filter(
      N03_001 %in% special_jp
    ) %>%
    sf::st_transform(4326)
  
  special_summary <- lapply(
    special_jp,
    function(pref) {
      x <- special %>%
        filter(N03_001 == pref)
      
      b <- sf::st_bbox(x)
      
      tibble(
        prefecture = pref,
        features = nrow(x),
        municipalities = n_distinct(
          as.character(x$N03_004)
        ),
        subprefectures = n_distinct(
          as.character(x$N03_002)[
            !is.na(x$N03_002) &
              trimws(
                as.character(x$N03_002)
              ) != ""
          ]
        ),
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
  # Dissolve special areas to municipality/code and calculate extents
  # ---------------------------------------------------------------------------
  
  special_units_sf <- special %>%
    mutate(
      prefecture = as.character(N03_001),
      subprefecture = as.character(N03_002),
      district = as.character(N03_003),
      municipality = as.character(N03_004),
      admin_code = as.character(N03_007)
    ) %>%
    group_by(
      prefecture,
      subprefecture,
      district,
      municipality,
      admin_code
    ) %>%
    summarise(
      geometry = sf::st_union(geometry),
      .groups = "drop"
    )
  
  extent_list <- lapply(
    seq_len(nrow(special_units_sf)),
    function(i) {
      b <- sf::st_bbox(
        special_units_sf[i, ]
      )
      
      tibble(
        prefecture =
          special_units_sf$prefecture[[i]],
        subprefecture =
          special_units_sf$subprefecture[[i]],
        district =
          special_units_sf$district[[i]],
        municipality =
          special_units_sf$municipality[[i]],
        admin_code =
          special_units_sf$admin_code[[i]],
        xmin = as.numeric(b["xmin"]),
        ymin = as.numeric(b["ymin"]),
        xmax = as.numeric(b["xmax"]),
        ymax = as.numeric(b["ymax"])
      )
    }
  )
  
  special_extents <- bind_rows(
    extent_list
  ) %>%
    mutate(
      candidate_zone = case_when(
        prefecture == "東京都" &
          ymin >= 30 &
          ymax < 35.0 ~
          "Izu_candidate",
        
        prefecture == "東京都" &
          ymin >= 26 &
          ymax < 30 ~
          "Ogasawara_candidate",
        
        prefecture == "東京都" &
          ymax < 26 ~
          "Kazan_candidate",
        
        prefecture == "鹿児島県" &
          ymax < 29.0 ~
          "Amami_Ryukyu_candidate",
        
        prefecture == "沖縄県" ~
          "Ryukyu_candidate",
        
        TRUE ~
          "ordinary_or_review"
      )
    ) %>%
    arrange(
      prefecture,
      ymin,
      xmin
    ) %>%
    as_tibble()
  
  candidate_summary <- special_extents %>%
    count(
      prefecture,
      candidate_zone,
      name = "administrative_units"
    ) %>%
    arrange(
      prefecture,
      candidate_zone
    ) %>%
    as_tibble()
  
  candidate_units <- special_extents %>%
    filter(
      candidate_zone !=
        "ordinary_or_review"
    ) %>%
    as_tibble()
  
  # ---------------------------------------------------------------------------
  # Tokyo candidate names
  # ---------------------------------------------------------------------------
  
  tokyo_candidates <- candidate_units %>%
    filter(prefecture == "東京都") %>%
    select(
      candidate_zone,
      municipality,
      admin_code,
      xmin,
      ymin,
      xmax,
      ymax
    ) %>%
    arrange(
      candidate_zone,
      ymin
    ) %>%
    as_tibble()
  
  kagoshima_candidates <- candidate_units %>%
    filter(prefecture == "鹿児島県") %>%
    select(
      candidate_zone,
      municipality,
      admin_code,
      xmin,
      ymin,
      xmax,
      ymax
    ) %>%
    arrange(ymin) %>%
    as_tibble()
  
  okinawa_candidates <- candidate_units %>%
    filter(prefecture == "沖縄県") %>%
    select(
      candidate_zone,
      municipality,
      admin_code,
      xmin,
      ymin,
      xmax,
      ymax
    ) %>%
    arrange(ymin) %>%
    as_tibble()
  
  # ---------------------------------------------------------------------------
  # Persist
  # ---------------------------------------------------------------------------
  
  dbWriteTable(
    con,
    "japan_nlni_n03_main_layer_profile",
    layer_profile,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "japan_nlni_n03_admin_field_profile",
    admin_profile,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "japan_nlni_n03_special_prefecture_summary",
    special_summary,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "japan_nlni_n03_hokkaido_subprefectures",
    hokkaido_sub_profile,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "japan_nlni_n03_special_municipality_extents",
    special_extents,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "japan_nlni_n03_special_candidate_summary",
    candidate_summary,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "japan_nlni_n03_tokyo_special_candidates",
    tokyo_candidates,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "japan_nlni_n03_kagoshima_special_candidates",
    kagoshima_candidates,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "japan_nlni_n03_okinawa_special_candidates",
    okinawa_candidates,
    overwrite = TRUE
  )
  
  metadata <- tibble(
    metric = c(
      "script_version",
      "input_geography_02_version",
      "input_geography_03a_version",
      "all_shapefiles",
      "main_prefecture_layers",
      "separate_hokkaido_subprefecture_layers",
      "combined_main_admin_features",
      "main_crs_epsg",
      "botanical_boundaries_created",
      "occurrences_assigned",
      "coordinates_modified",
      "taxonomy_modified",
      "upstream_tables_modified"
    ),
    value = c(
      SCRIPT_VERSION,
      g02,
      g03a,
      "48",
      "47",
      "1",
      as.character(nrow(admin)),
      as.character(target_crs$epsg),
      "FALSE",
      "FALSE",
      "FALSE",
      "FALSE",
      "FALSE"
    )
  )
  
  dbWriteTable(
    con,
    "japan_nlni_n03_inspection_metadata",
    metadata,
    overwrite = TRUE
  )
  
  # ---------------------------------------------------------------------------
  # CSV outputs
  # ---------------------------------------------------------------------------
  
  out_dir <- here(
    "outputs", "tables", "geography",
    "nlni_n03_inspection"
  )
  
  dir.create(
    out_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  write_csv(
    layer_profile,
    file.path(
      out_dir,
      "main_layer_profile.csv"
    )
  )
  
  write_csv(
    admin_profile,
    file.path(
      out_dir,
      "admin_field_profile.csv"
    )
  )
  
  write_csv(
    special_summary,
    file.path(
      out_dir,
      "special_prefecture_summary.csv"
    )
  )
  
  write_csv(
    hokkaido_sub_profile,
    file.path(
      out_dir,
      "hokkaido_subprefectures.csv"
    )
  )
  
  write_csv(
    special_extents,
    file.path(
      out_dir,
      "special_municipality_extents.csv"
    )
  )
  
  write_csv(
    candidate_summary,
    file.path(
      out_dir,
      "special_area_candidate_summary.csv"
    )
  )
  
  write_csv(
    tokyo_candidates,
    file.path(
      out_dir,
      "tokyo_special_candidates.csv"
    )
  )
  
  write_csv(
    kagoshima_candidates,
    file.path(
      out_dir,
      "kagoshima_special_candidates.csv"
    )
  )
  
  write_csv(
    okinawa_candidates,
    file.path(
      out_dir,
      "okinawa_special_candidates.csv"
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
  
  cat("\n— Standard N03 field profile —\n\n")
  print.data.frame(
    as.data.frame(admin_profile),
    row.names = FALSE
  )
  
  cat("\n— Special-prefecture summary —\n\n")
  print.data.frame(
    as.data.frame(special_summary),
    row.names = FALSE
  )
  
  cat("\n— Hokkaido subprefectural structure —\n\n")
  print.data.frame(
    as.data.frame(hokkaido_sub_profile),
    row.names = FALSE
  )
  
  cat("\n— Candidate special botanical zones —\n\n")
  print.data.frame(
    as.data.frame(candidate_summary),
    row.names = FALSE
  )
  
  cat("\n— Tokyo special-area candidates —\n\n")
  print.data.frame(
    as.data.frame(tokyo_candidates),
    row.names = FALSE
  )
  
  cat("\n— Kagoshima / Amami candidates —\n\n")
  print.data.frame(
    as.data.frame(kagoshima_candidates),
    row.names = FALSE
  )
  
  cat("\n— Okinawa / Ryukyu candidates —\n\n")
  print.data.frame(
    as.data.frame(okinawa_candidates),
    row.names = FALSE
  )
  
  cat("\n— Validation —\n")
  cat("48 total shapefiles: PASS\n")
  cat("47 main administrative layers: PASS\n")
  cat("Hokkaido subprefecture layer separated: PASS\n")
  cat("All main layers EPSG:6668: ",
      all(layer_profile$crs_epsg == 6668),
      "\n", sep = "")
  cat("Special prefectures profiled: PASS\n")
  
  cat("\n— Safety —\n")
  cat("Botanical boundaries created: FALSE\n")
  cat("Occurrences assigned: FALSE\n")
  cat("Coordinates modified: FALSE\n")
  cat("Taxonomy modified: FALSE\n")
  cat("Frozen upstream tables modified: FALSE\n")
  cat("Output status: N03 SPATIAL DIAGNOSTIC ONLY\n")
  
  cat(
    "\n03b_inspect_nlni_boundaries.R v",
    SCRIPT_VERSION,
    " complete.\n",
    sep = ""
  )
  
  invisible(
    list(
      layer_profile = layer_profile,
      admin_profile = admin_profile,
      special_summary = special_summary,
      hokkaido_subprefectures = hokkaido_sub_profile,
      special_extents = special_extents,
      candidate_summary = candidate_summary,
      tokyo_candidates = tokyo_candidates,
      kagoshima_candidates = kagoshima_candidates,
      okinawa_candidates = okinawa_candidates,
      metadata = metadata
    )
  )
}

geography_03b <-
  run_nlni_inspection()