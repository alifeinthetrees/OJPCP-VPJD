# ==============================================================================
# VPJD-OJPCP
# 03a_acquire_nlni_boundaries.R
# Version: 0.1.0
#
# Purpose:
#   Acquire and inventory the official Japanese National Land Numerical
#   Information (NLNI / KSJ) N03 administrative-area polygons required for
#   construction of the VPJD botanical-area boundary layer.
#
# Source:
#   Ministry of Land, Infrastructure, Transport and Tourism (MLIT), Japan
#   National Land Numerical Information
#   Dataset: N03 Administrative Areas
#   Reference year: 2025
#
# Strategy:
#   - Download individual prefecture packages rather than the national package.
#   - Preserve ZIP files unchanged as raw source data.
#   - Inspect package contents only.
#   - Do NOT yet construct botanical boundaries.
#
# This module does NOT:
#   - assign occurrences;
#   - modify occurrence coordinates;
#   - create botanical polygons;
#   - alter administrative boundaries;
#   - modify taxonomy;
#   - modify frozen upstream tables.
#
# Required frozen input:
#   Geography 02 v0.1.0
# ==============================================================================

suppressPackageStartupMessages({
  library(DBI)
  library(duckdb)
  library(dplyr)
  library(tibble)
  library(readr)
  library(here)
})

SCRIPT_VERSION <- "0.1.0"
EXPECTED_GEOGRAPHY_02_VERSION <- "0.1.0"
N03_YEAR <- "2025"
N03_DATE <- "20250101"

get_metadata_value <- function(con, table_name, metric_name) {
  fields <- dbListFields(con, table_name)
  
  if (all(c("metric", "value") %in% fields)) {
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
        "Expected one '", metric_name,
        "' row in ", table_name,
        "; found ", nrow(x), "."
      )
    }
    
    return(as.character(x$value[[1]]))
  }
  
  if (metric_name %in% fields) {
    x <- dbReadTable(con, table_name)
    
    if (nrow(x) != 1) {
      stop("Expected one row in ", table_name, ".")
    }
    
    return(as.character(x[[metric_name]][[1]]))
  }
  
  stop(
    "Metadata item '", metric_name,
    "' not found in ", table_name, "."
  )
}

run_nlni_acquisition <- function() {
  
  cat("\n— VPJD NLNI N03 boundary acquisition —\n\n")
  
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
  
  if (
    !"japan_botanical_framework_metadata" %in%
    dbListTables(con)
  ) {
    stop(
      "Required Geography 02 metadata table not found."
    )
  }
  
  geography_02_version <- get_metadata_value(
    con,
    "japan_botanical_framework_metadata",
    "script_version"
  )
  
  if (
    geography_02_version !=
    EXPECTED_GEOGRAPHY_02_VERSION
  ) {
    stop(
      "Expected Geography 02 v",
      EXPECTED_GEOGRAPHY_02_VERSION,
      "; detected ",
      geography_02_version, "."
    )
  }
  
  cat(
    "Geography 02 version: ",
    geography_02_version, "\n",
    "NLNI dataset: N03 Administrative Areas\n",
    "Pinned reference year: ",
    N03_YEAR, "\n\n",
    sep = ""
  )
  
  # ---------------------------------------------------------------------------
  # Prefecture registry
  #
  # JIS prefecture codes 01-47.
  # Geography 02 contains 46 political botanical areas because Okinawa is
  # represented within the Ryukyu botanical treatment rather than as an
  # independent political botanical area.
  #
  # Nevertheless, all 47 administrative prefecture packages are acquired.
  # ---------------------------------------------------------------------------
  
  prefectures <- tribble(
    ~jis_code, ~prefecture_en,
    "01", "Hokkaido",
    "02", "Aomori",
    "03", "Iwate",
    "04", "Miyagi",
    "05", "Akita",
    "06", "Yamagata",
    "07", "Fukushima",
    "08", "Ibaraki",
    "09", "Tochigi",
    "10", "Gunma",
    "11", "Saitama",
    "12", "Chiba",
    "13", "Tokyo",
    "14", "Kanagawa",
    "15", "Niigata",
    "16", "Toyama",
    "17", "Ishikawa",
    "18", "Fukui",
    "19", "Yamanashi",
    "20", "Nagano",
    "21", "Gifu",
    "22", "Shizuoka",
    "23", "Aichi",
    "24", "Mie",
    "25", "Shiga",
    "26", "Kyoto",
    "27", "Osaka",
    "28", "Hyogo",
    "29", "Nara",
    "30", "Wakayama",
    "31", "Tottori",
    "32", "Shimane",
    "33", "Okayama",
    "34", "Hiroshima",
    "35", "Yamaguchi",
    "36", "Tokushima",
    "37", "Kagawa",
    "38", "Ehime",
    "39", "Kochi",
    "40", "Fukuoka",
    "41", "Saga",
    "42", "Nagasaki",
    "43", "Kumamoto",
    "44", "Oita",
    "45", "Miyazaki",
    "46", "Kagoshima",
    "47", "Okinawa"
  ) %>%
    mutate(
      n03_filename = paste0(
        "N03-",
        N03_DATE,
        "_",
        jis_code,
        "_GML.zip"
      ),
      n03_url = paste0(
        "https://nlftp.mlit.go.jp/ksj/gml/data/N03/",
        "N03-2025/",
        n03_filename
      )
    )
  
  if (nrow(prefectures) != 47) {
    stop("Expected 47 administrative prefectures.")
  }
  
  if (n_distinct(prefectures$jis_code) != 47) {
    stop("JIS prefecture codes are not unique.")
  }
  
  # ---------------------------------------------------------------------------
  # Raw source directory
  # ---------------------------------------------------------------------------
  
  raw_dir <- here(
    "data",
    "raw",
    "geography",
    "nlni_n03_2025"
  )
  
  dir.create(
    raw_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  # ---------------------------------------------------------------------------
  # Download
  #
  # Existing non-empty files are retained and not downloaded again.
  # ---------------------------------------------------------------------------
  
  cat("— Downloading / validating 47 prefecture packages —\n\n")
  
  download_results <- vector(
    "list",
    nrow(prefectures)
  )
  
  for (i in seq_len(nrow(prefectures))) {
    
    p <- prefectures[i, ]
    
    dest <- file.path(
      raw_dir,
      p$n03_filename
    )
    
    cat(
      sprintf(
        "[%02d/47] %-10s ",
        i,
        p$prefecture_en
      )
    )
    
    status <- "existing"
    
    if (
      !file.exists(dest) ||
      is.na(file.info(dest)$size) ||
      file.info(dest)$size <= 0
    ) {
      
      status <- "downloaded"
      
      ok <- tryCatch(
        {
          utils::download.file(
            url = p$n03_url,
            destfile = dest,
            mode = "wb",
            quiet = TRUE
          )
          TRUE
        },
        error = function(e) {
          message(
            "\nDownload failed for ",
            p$prefecture_en,
            ": ",
            conditionMessage(e)
          )
          FALSE
        }
      )
      
      if (!ok) {
        status <- "download_failed"
      }
    }
    
    exists_now <- file.exists(dest)
    
    size_bytes <- if (exists_now) {
      as.numeric(file.info(dest)$size)
    } else {
      NA_real_
    }
    
    zip_valid <- FALSE
    zip_members <- NA_integer_
    shapefile_present <- FALSE
    
    if (
      exists_now &&
      !is.na(size_bytes) &&
      size_bytes > 0
    ) {
      
      zip_info <- tryCatch(
        utils::unzip(
          dest,
          list = TRUE
        ),
        error = function(e) NULL
      )
      
      if (!is.null(zip_info)) {
        zip_valid <- TRUE
        zip_members <- nrow(zip_info)
        
        shapefile_present <- any(
          grepl(
            "\\.shp$",
            zip_info$Name,
            ignore.case = TRUE
          )
        )
      }
    }
    
    cat(
      status,
      " | ",
      ifelse(
        is.na(size_bytes),
        "missing",
        paste0(
          format(
            round(size_bytes / 1024^2, 2),
            nsmall = 2
          ),
          " MB"
        )
      ),
      " | ZIP=",
      zip_valid,
      " | SHP=",
      shapefile_present,
      "\n",
      sep = ""
    )
    
    download_results[[i]] <- tibble(
      jis_code = p$jis_code,
      prefecture_en = p$prefecture_en,
      n03_filename = p$n03_filename,
      n03_url = p$n03_url,
      local_path = dest,
      download_status = status,
      file_exists = exists_now,
      size_bytes = size_bytes,
      zip_valid = zip_valid,
      zip_members = zip_members,
      shapefile_present = shapefile_present
    )
  }
  
  inventory <- bind_rows(
    download_results
  )
  
  # ---------------------------------------------------------------------------
  # Validation
  # ---------------------------------------------------------------------------
  
  missing_files <- inventory %>%
    filter(
      !file_exists |
        is.na(size_bytes) |
        size_bytes <= 0
    )
  
  invalid_zips <- inventory %>%
    filter(
      file_exists &
        !zip_valid
    )
  
  no_shapefile <- inventory %>%
    filter(
      zip_valid &
        !shapefile_present
    )
  
  cat("\n— Acquisition summary —\n\n")
  
  acquisition_summary <- tibble(
    metric = c(
      "expected_prefecture_packages",
      "files_present",
      "valid_zip_packages",
      "packages_with_shapefile",
      "missing_or_empty_files",
      "invalid_zip_packages",
      "packages_without_shapefile"
    ),
    value = c(
      47L,
      sum(
        inventory$file_exists &
          !is.na(inventory$size_bytes) &
          inventory$size_bytes > 0
      ),
      sum(inventory$zip_valid),
      sum(inventory$shapefile_present),
      nrow(missing_files),
      nrow(invalid_zips),
      nrow(no_shapefile)
    )
  )
  
  print(
    acquisition_summary,
    n = Inf
  )
  
  # ---------------------------------------------------------------------------
  # Inspect ZIP contents
  # ---------------------------------------------------------------------------
  
  zip_contents <- vector(
    "list",
    nrow(inventory)
  )
  
  for (i in seq_len(nrow(inventory))) {
    
    if (!inventory$zip_valid[[i]]) {
      next
    }
    
    z <- utils::unzip(
      inventory$local_path[[i]],
      list = TRUE
    )
    
    zip_contents[[i]] <- tibble(
      jis_code =
        inventory$jis_code[[i]],
      prefecture_en =
        inventory$prefecture_en[[i]],
      member_name = z$Name,
      member_length = z$Length
    )
  }
  
  zip_contents <- bind_rows(
    zip_contents
  )
  
  extension_summary <- zip_contents %>%
    mutate(
      extension = tolower(
        tools::file_ext(member_name)
      ),
      extension = if_else(
        extension == "",
        "[none]",
        extension
      )
    ) %>%
    count(
      extension,
      name = "files"
    ) %>%
    arrange(desc(files))
  
  # ---------------------------------------------------------------------------
  # Persist diagnostics
  # ---------------------------------------------------------------------------
  
  dbWriteTable(
    con,
    "japan_nlni_n03_prefecture_registry",
    prefectures,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "japan_nlni_n03_raw_inventory",
    inventory,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "japan_nlni_n03_zip_contents",
    zip_contents,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "japan_nlni_n03_extension_summary",
    extension_summary,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "japan_nlni_n03_acquisition_summary",
    acquisition_summary,
    overwrite = TRUE
  )
  
  metadata <- tibble(
    metric = c(
      "script_version",
      "input_geography_02_version",
      "source_organisation",
      "source_dataset",
      "source_dataset_code",
      "source_reference_year",
      "administrative_prefecture_packages_expected",
      "administrative_prefecture_packages_valid",
      "botanical_boundaries_created",
      "occurrences_assigned",
      "coordinates_modified",
      "taxonomy_modified",
      "upstream_tables_modified"
    ),
    value = c(
      SCRIPT_VERSION,
      geography_02_version,
      paste(
        "Ministry of Land, Infrastructure,",
        "Transport and Tourism, Japan"
      ),
      "National Land Numerical Information Administrative Areas",
      "N03",
      N03_YEAR,
      "47",
      as.character(
        sum(
          inventory$zip_valid &
            inventory$shapefile_present
        )
      ),
      "FALSE",
      "FALSE",
      "FALSE",
      "FALSE",
      "FALSE"
    )
  )
  
  dbWriteTable(
    con,
    "japan_nlni_n03_acquisition_metadata",
    metadata,
    overwrite = TRUE
  )
  
  out_dir <- here(
    "outputs",
    "tables",
    "geography",
    "nlni_n03_acquisition"
  )
  
  dir.create(
    out_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  write_csv(
    prefectures,
    file.path(
      out_dir,
      "prefecture_registry.csv"
    )
  )
  
  write_csv(
    inventory,
    file.path(
      out_dir,
      "raw_file_inventory.csv"
    )
  )
  
  write_csv(
    zip_contents,
    file.path(
      out_dir,
      "zip_contents.csv"
    )
  )
  
  write_csv(
    extension_summary,
    file.path(
      out_dir,
      "extension_summary.csv"
    )
  )
  
  write_csv(
    acquisition_summary,
    file.path(
      out_dir,
      "acquisition_summary.csv"
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
  # Final validation
  # ---------------------------------------------------------------------------
  
  if (nrow(missing_files) > 0) {
    stop(
      nrow(missing_files),
      " N03 package(s) are missing or empty."
    )
  }
  
  if (nrow(invalid_zips) > 0) {
    stop(
      nrow(invalid_zips),
      " N03 package(s) are invalid ZIP files."
    )
  }
  
  if (nrow(no_shapefile) > 0) {
    stop(
      nrow(no_shapefile),
      " N03 package(s) contain no shapefile."
    )
  }
  
  if (
    sum(
      inventory$zip_valid &
      inventory$shapefile_present
    ) != 47
  ) {
    stop(
      "Expected 47 valid N03 prefecture packages."
    )
  }
  
  cat("\n— ZIP extension profile —\n\n")
  print(
    extension_summary,
    n = Inf
  )
  
  cat("\n— Validation —\n")
  cat("47 administrative prefecture packages: PASS\n")
  cat("47 valid ZIP archives: PASS\n")
  cat("47 packages containing shapefiles: PASS\n")
  cat("Raw source files preserved: PASS\n")
  
  cat("\n— Safety —\n")
  cat("Botanical boundaries created: FALSE\n")
  cat("Occurrences assigned: FALSE\n")
  cat("Coordinates modified: FALSE\n")
  cat("Taxonomy modified: FALSE\n")
  cat("Frozen upstream tables modified: FALSE\n")
  cat("Output status: RAW OFFICIAL BOUNDARY ACQUISITION\n")
  
  cat(
    "\n03a_acquire_nlni_boundaries.R v",
    SCRIPT_VERSION,
    " complete.\n",
    sep = ""
  )
  
  invisible(
    list(
      prefectures = prefectures,
      inventory = inventory,
      zip_contents = zip_contents,
      extension_summary = extension_summary,
      acquisition_summary = acquisition_summary,
      metadata = metadata
    )
  )
}

geography_03a <-
  run_nlni_acquisition()