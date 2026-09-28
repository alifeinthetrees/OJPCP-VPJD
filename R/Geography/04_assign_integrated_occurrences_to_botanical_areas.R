# ==============================================================================
# VPJD-OJPCP
# Geography 04 — Assign integrated occurrences to botanical areas
# Version: 0.1.1
#
# Purpose:
#   Spatially assign the canonical integrated Japanese occurrence dataset to
#   the validated/frozen JP01–JP51 botanical-area framework produced by
#   Geography 03m v0.1.2.
#
# Assignment hierarchy:
#   1. Unique polygon containment -> assigned_unique
#   2. No containment, but intersects one polygon boundary -> assigned_boundary
#   3. No containment/intersection -> outside_botanical_areas
#   4. Multiple candidate areas -> ambiguous_multiple_areas
#
# Coordinates:
#   The canonical ANALYTICAL_DECIMAL_LONGITUDE and
#   ANALYTICAL_DECIMAL_LATITUDE fields are used. These incorporate the
#   analytical coordinate selected during integration, including recovered
#   gazetteer coordinates where applicable.
#
# Important:
#   - Original occurrence coordinates are never modified.
#   - Analytical coordinates are never modified.
#   - No taxonomy is modified.
#   - No occurrence records are deleted.
#   - No Star ratings or GHI values are calculated.
#   - Geography 03m is treated as frozen canonical geography.
#   - Processing is chunked to avoid creating >3 million sf points at once.
# ==============================================================================

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(readr)
  library(DBI)
  library(duckdb)
  library(here)
})

SCRIPT_VERSION <- "0.1.1"
SOURCE_TABLE <- "vpjd_japan_occurrences_integrated"
OUTPUT_TABLE <- "vpjd_japan_occurrence_botanical_area_assignment"
EXPECTED_RECORDS <- 3113089L
EXPECTED_AREAS <- 51L
CHUNK_SIZE <- 250000L

run_botanical_area_assignment <- function() {
  cat("\n— VPJD integrated occurrence botanical-area assignment —\n\n")
  
  # ---------------------------------------------------------------------------
  # Paths
  # ---------------------------------------------------------------------------
  
  db_path <- here(
    "data", "interim", "occurrences",
    "vpjd_occurrences.duckdb"
  )
  
  boundary_gpkg <- here(
    "data", "interim", "geography",
    "botanical_boundaries",
    "vpjd_japan_botanical_boundaries_complete.gpkg"
  )
  
  output_dir <- here(
    "outputs", "tables", "geography",
    "occurrence_botanical_area_assignment"
  )
  
  dir.create(
    output_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  if (!file.exists(db_path)) {
    stop(
      "DuckDB database not found: ",
      db_path
    )
  }
  
  if (!file.exists(boundary_gpkg)) {
    stop(
      "Canonical Geography 03m GeoPackage not found: ",
      boundary_gpkg
    )
  }
  
  # ---------------------------------------------------------------------------
  # Read canonical JP01–JP51 geography
  # ---------------------------------------------------------------------------
  
  areas <- st_read(
    boundary_gpkg,
    layer = "botanical_areas_jp01_jp51",
    quiet = TRUE
  )
  
  if (nrow(areas) != EXPECTED_AREAS) {
    stop(
      "Expected 51 botanical areas; found ",
      nrow(areas),
      "."
    )
  }
  
  expected_ids <- sprintf(
    "JP%02d",
    1:51
  )
  
  if (!identical(
    areas$botanical_area_id,
    expected_ids
  )) {
    stop(
      "Botanical-area IDs are not canonical JP01–JP51 sequence."
    )
  }
  
  if (!all(st_is_valid(areas))) {
    stop(
      "Invalid canonical botanical-area geometry detected."
    )
  }
  
  if (is.na(st_crs(areas))) {
    stop(
      "Canonical botanical-area layer has no CRS."
    )
  }
  
  cat(
    "Canonical botanical areas: ",
    nrow(areas),
    "\n",
    sep = ""
  )
  
  cat(
    "Canonical botanical geometry: PASS\n"
  )
  
  cat(
    "Canonical area IDs JP01–JP51: PASS\n\n"
  )
  
  # ---------------------------------------------------------------------------
  # Connect to DuckDB
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
  
  existing_tables <- dbListTables(
    con
  )
  
  if (!(SOURCE_TABLE %in% existing_tables)) {
    stop(
      "Source table not found: ",
      SOURCE_TABLE
    )
  }
  
  # ---------------------------------------------------------------------------
  # Inspect source table
  # ---------------------------------------------------------------------------
  
  source_fields <- dbListFields(
    con,
    SOURCE_TABLE
  )
  
  cat(
    "Source table: ",
    SOURCE_TABLE,
    "\n",
    sep = ""
  )
  
  source_count <- dbGetQuery(
    con,
    paste0(
      "SELECT COUNT(*) AS n FROM ",
      SOURCE_TABLE
    )
  )$n[[1]]
  
  cat(
    "Integrated occurrence records: ",
    format(
      source_count,
      big.mark = ",",
      scientific = FALSE
    ),
    "\n",
    sep = ""
  )
  
  if (source_count != EXPECTED_RECORDS) {
    stop(
      "Expected ",
      format(
        EXPECTED_RECORDS,
        big.mark = ","
      ),
      " integrated records; found ",
      format(
        source_count,
        big.mark = ","
      ),
      "."
    )
  }
  
  # ---------------------------------------------------------------------------
  # Detect required fields
  # ---------------------------------------------------------------------------
  
  find_field <- function(
    fields,
    candidates,
    description
  ) {
    fields_lower <- tolower(
      fields
    )
    
    candidates_lower <- tolower(
      candidates
    )
    
    hit <- match(
      candidates_lower,
      fields_lower,
      nomatch = 0L
    )
    
    hit <- hit[
      hit > 0L
    ]
    
    if (length(hit) == 0L) {
      stop(
        "Could not identify ",
        description,
        " field. Available fields:\n",
        paste(
          fields,
          collapse = ", "
        )
      )
    }
    
    fields[
      hit[[1]]
    ]
  }
  
  id_field <- find_field(
    source_fields,
    c(
      "gbifID",
      "gbif_id"
    ),
    "GBIF ID"
  )
  
  lon_field <- find_field(
    source_fields,
    c(
      "ANALYTICAL_DECIMAL_LONGITUDE",
      "decimalLongitude",
      "decimal_longitude",
      "longitude"
    ),
    "analytical longitude"
  )
  
  lat_field <- find_field(
    source_fields,
    c(
      "ANALYTICAL_DECIMAL_LATITUDE",
      "decimalLatitude",
      "decimal_latitude",
      "latitude"
    ),
    "analytical latitude"
  )
  
  cat(
    "GBIF ID field: ",
    id_field,
    "\n",
    sep = ""
  )
  
  cat(
    "Longitude field: ",
    lon_field,
    "\n",
    sep = ""
  )
  
  cat(
    "Latitude field: ",
    lat_field,
    "\n\n",
    sep = ""
  )
  
  quote_id <- function(x) {
    paste0(
      '"',
      gsub(
        '"',
        '""',
        x
      ),
      '"'
    )
  }
  
  q_source <- quote_id(
    SOURCE_TABLE
  )
  
  q_id <- quote_id(
    id_field
  )
  
  q_lon <- quote_id(
    lon_field
  )
  
  q_lat <- quote_id(
    lat_field
  )
  
  # ---------------------------------------------------------------------------
  # Coordinate QA
  # ---------------------------------------------------------------------------
  
  coordinate_qa <- dbGetQuery(
    con,
    paste0(
      "SELECT ",
      "COUNT(*) AS total_records, ",
      "SUM(CASE WHEN ",
      q_lon,
      " IS NOT NULL AND ",
      q_lat,
      " IS NOT NULL THEN 1 ELSE 0 END) AS coordinate_records, ",
      "SUM(CASE WHEN ",
      q_lon,
      " IS NULL OR ",
      q_lat,
      " IS NULL THEN 1 ELSE 0 END) AS missing_coordinate_records ",
      "FROM ",
      q_source
    )
  )
  
  cat(
    "Records with analytical coordinates: ",
    format(
      coordinate_qa$coordinate_records[[1]],
      big.mark = ",",
      scientific = FALSE
    ),
    "\n",
    sep = ""
  )
  
  cat(
    "Records missing analytical coordinates: ",
    format(
      coordinate_qa$missing_coordinate_records[[1]],
      big.mark = ",",
      scientific = FALSE
    ),
    "\n\n",
    sep = ""
  )
  
  # ---------------------------------------------------------------------------
  # Prepare botanical polygons
  # ---------------------------------------------------------------------------
  
  areas_assignment <- areas %>%
    select(
      botanical_area_id,
      botanical_area_name
    ) %>%
    st_transform(
      4326
    )
  
  # ---------------------------------------------------------------------------
  # Prepare output table
  # ---------------------------------------------------------------------------
  
  if (OUTPUT_TABLE %in% dbListTables(con)) {
    dbExecute(
      con,
      paste0(
        "DROP TABLE ",
        quote_id(OUTPUT_TABLE)
      )
    )
  }
  
  dbExecute(
    con,
    paste0(
      "CREATE TABLE ",
      quote_id(OUTPUT_TABLE),
      " (",
      "source_row_number BIGINT, ",
      "gbifID VARCHAR, ",
      "analytical_decimal_longitude DOUBLE, ",
      "analytical_decimal_latitude DOUBLE, ",
      "botanical_area_id VARCHAR, ",
      "botanical_area_name VARCHAR, ",
      "assignment_status VARCHAR, ",
      "candidate_area_count INTEGER, ",
      "assignment_method VARCHAR, ",
      "geography_script_version VARCHAR",
      ")"
    )
  )
  
  # ---------------------------------------------------------------------------
  # Chunk plan
  # ---------------------------------------------------------------------------
  
  n_chunks <- ceiling(
    source_count /
      CHUNK_SIZE
  )
  
  cat(
    "Chunk size: ",
    format(
      CHUNK_SIZE,
      big.mark = ","
    ),
    "\n",
    sep = ""
  )
  
  cat(
    "Chunks required: ",
    n_chunks,
    "\n\n",
    sep = ""
  )
  
  # ---------------------------------------------------------------------------
  # Process chunks
  # ---------------------------------------------------------------------------
  
  for (
    chunk_no in seq_len(
      n_chunks
    )
  ) {
    row_start <-
      (chunk_no - 1L) *
      CHUNK_SIZE + 1L
    
    row_end <- min(
      chunk_no *
        CHUNK_SIZE,
      source_count
    )
    
    cat(
      "Chunk ",
      chunk_no,
      "/",
      n_chunks,
      ": rows ",
      format(
        row_start,
        big.mark = ","
      ),
      "–",
      format(
        row_end,
        big.mark = ","
      ),
      "\n",
      sep = ""
    )
    
    chunk_query <- paste0(
      "SELECT ",
      "source_row_number, ",
      q_id,
      " AS gbifID, ",
      q_lon,
      " AS analytical_decimal_longitude, ",
      q_lat,
      " AS analytical_decimal_latitude ",
      "FROM (",
      "SELECT ",
      "ROW_NUMBER() OVER () AS source_row_number, ",
      q_id,
      ", ",
      q_lon,
      ", ",
      q_lat,
      " FROM ",
      q_source,
      ") q ",
      "WHERE source_row_number BETWEEN ",
      row_start,
      " AND ",
      row_end,
      " ORDER BY source_row_number"
    )
    
    chunk <- dbGetQuery(
      con,
      chunk_query
    )
    
    if (nrow(chunk) == 0L) {
      stop(
        "Chunk ",
        chunk_no,
        " unexpectedly returned zero records."
      )
    }
    
    result <- chunk %>%
      transmute(
        source_row_number =
          as.numeric(
            source_row_number
          ),
        
        gbifID =
          as.character(
            gbifID
          ),
        
        analytical_decimal_longitude =
          as.numeric(
            analytical_decimal_longitude
          ),
        
        analytical_decimal_latitude =
          as.numeric(
            analytical_decimal_latitude
          ),
        
        botanical_area_id =
          NA_character_,
        
        botanical_area_name =
          NA_character_,
        
        assignment_status =
          if_else(
            is.na(
              analytical_decimal_longitude
            ) |
              is.na(
                analytical_decimal_latitude
              ),
            "missing_coordinates",
            "pending"
          ),
        
        candidate_area_count =
          if_else(
            assignment_status ==
              "missing_coordinates",
            0L,
            NA_integer_
          ),
        
        assignment_method =
          if_else(
            assignment_status ==
              "missing_coordinates",
            "not_attempted",
            NA_character_
          ),
        
        geography_script_version =
          SCRIPT_VERSION
      )
    
    valid_idx <- which(
      result$assignment_status ==
        "pending"
    )
    
    if (length(valid_idx) > 0L) {
      valid_df <- result[
        valid_idx,
        c(
          "source_row_number",
          "gbifID",
          "analytical_decimal_longitude",
          "analytical_decimal_latitude"
        )
      ]
      
      points <- st_as_sf(
        valid_df,
        coords = c(
          "analytical_decimal_longitude",
          "analytical_decimal_latitude"
        ),
        crs = 4326,
        remove = FALSE
      )
      
      # -----------------------------------------------------------------------
      # Primary assignment: strict polygon containment
      # -----------------------------------------------------------------------
      
      within_list <- st_within(
        points,
        areas_assignment,
        sparse = TRUE
      )
      
      within_count <- lengths(
        within_list
      )
      
      unique_within <- which(
        within_count == 1L
      )
      
      if (
        length(unique_within) > 0L
      ) {
        area_index <- vapply(
          within_list[
            unique_within
          ],
          function(x) x[[1]],
          integer(1)
        )
        
        target_idx <- valid_idx[
          unique_within
        ]
        
        result$botanical_area_id[
          target_idx
        ] <- areas_assignment$
          botanical_area_id[
            area_index
          ]
        
        result$botanical_area_name[
          target_idx
        ] <- areas_assignment$
          botanical_area_name[
            area_index
          ]
        
        result$assignment_status[
          target_idx
        ] <- "assigned_unique"
        
        result$candidate_area_count[
          target_idx
        ] <- 1L
        
        result$assignment_method[
          target_idx
        ] <- "point_within_polygon"
      }
      
      # -----------------------------------------------------------------------
      # Multiple containment candidates
      # -----------------------------------------------------------------------
      
      multiple_within <- which(
        within_count > 1L
      )
      
      if (
        length(multiple_within) > 0L
      ) {
        target_idx <- valid_idx[
          multiple_within
        ]
        
        result$assignment_status[
          target_idx
        ] <-
          "ambiguous_multiple_areas"
        
        result$candidate_area_count[
          target_idx
        ] <-
          within_count[
            multiple_within
          ]
        
        result$assignment_method[
          target_idx
        ] <-
          "multiple_point_within_polygon"
      }
      
      # -----------------------------------------------------------------------
      # No strict containment:
      # test polygon intersection to identify exact boundary points
      # -----------------------------------------------------------------------
      
      no_within <- which(
        within_count == 0L
      )
      
      if (
        length(no_within) > 0L
      ) {
        boundary_points <- points[
          no_within,
        ]
        
        intersect_list <- st_intersects(
          boundary_points,
          areas_assignment,
          sparse = TRUE
        )
        
        intersect_count <- lengths(
          intersect_list
        )
        
        unique_boundary <- which(
          intersect_count == 1L
        )
        
        if (
          length(unique_boundary) > 0L
        ) {
          area_index <- vapply(
            intersect_list[
              unique_boundary
            ],
            function(x) x[[1]],
            integer(1)
          )
          
          point_position <- no_within[
            unique_boundary
          ]
          
          target_idx <- valid_idx[
            point_position
          ]
          
          result$botanical_area_id[
            target_idx
          ] <- areas_assignment$
            botanical_area_id[
              area_index
            ]
          
          result$botanical_area_name[
            target_idx
          ] <- areas_assignment$
            botanical_area_name[
              area_index
            ]
          
          result$assignment_status[
            target_idx
          ] <- "assigned_boundary"
          
          result$candidate_area_count[
            target_idx
          ] <- 1L
          
          result$assignment_method[
            target_idx
          ] <-
            "point_intersects_polygon_boundary"
        }
        
        multiple_boundary <- which(
          intersect_count > 1L
        )
        
        if (
          length(multiple_boundary) > 0L
        ) {
          point_position <- no_within[
            multiple_boundary
          ]
          
          target_idx <- valid_idx[
            point_position
          ]
          
          result$assignment_status[
            target_idx
          ] <-
            "ambiguous_multiple_areas"
          
          result$candidate_area_count[
            target_idx
          ] <-
            intersect_count[
              multiple_boundary
            ]
          
          result$assignment_method[
            target_idx
          ] <-
            "multiple_polygon_boundary_intersections"
        }
        
        outside <- which(
          intersect_count == 0L
        )
        
        if (
          length(outside) > 0L
        ) {
          point_position <- no_within[
            outside
          ]
          
          target_idx <- valid_idx[
            point_position
          ]
          
          result$assignment_status[
            target_idx
          ] <-
            "outside_botanical_areas"
          
          result$candidate_area_count[
            target_idx
          ] <- 0L
          
          result$assignment_method[
            target_idx
          ] <-
            "no_polygon_intersection"
        }
      }
    }
    
    if (
      any(
        result$assignment_status ==
        "pending"
      )
    ) {
      stop(
        "Unresolved pending records remain in chunk ",
        chunk_no,
        "."
      )
    }
    
    dbAppendTable(
      con,
      OUTPUT_TABLE,
      result
    )
    
    rm(
      chunk,
      result
    )
    
    if (
      exists("points")
    ) {
      rm(points)
    }
    
    if (
      exists("valid_df")
    ) {
      rm(valid_df)
    }
    
    if (
      exists("boundary_points")
    ) {
      rm(boundary_points)
    }
    
    gc(
      verbose = FALSE
    )
  }
  
  # ---------------------------------------------------------------------------
  # Output count QA
  # ---------------------------------------------------------------------------
  
  output_count <- dbGetQuery(
    con,
    paste0(
      "SELECT COUNT(*) AS n FROM ",
      quote_id(OUTPUT_TABLE)
    )
  )$n[[1]]
  
  cat(
    "\nAssignment rows written: ",
    format(
      output_count,
      big.mark = ",",
      scientific = FALSE
    ),
    "\n",
    sep = ""
  )
  
  # ---------------------------------------------------------------------------
  # GBIF ID QA
  # ---------------------------------------------------------------------------
  
  id_qa <- dbGetQuery(
    con,
    paste0(
      "SELECT ",
      "COUNT(*) AS total_rows, ",
      "COUNT(gbifID) AS nonnull_gbifids, ",
      "COUNT(DISTINCT gbifID) AS distinct_gbifids ",
      "FROM ",
      quote_id(OUTPUT_TABLE)
    )
  )
  
  cat(
    "Non-null GBIF IDs: ",
    format(
      id_qa$nonnull_gbifids[[1]],
      big.mark = ",",
      scientific = FALSE
    ),
    "\n",
    sep = ""
  )
  
  cat(
    "Distinct GBIF IDs: ",
    format(
      id_qa$distinct_gbifids[[1]],
      big.mark = ",",
      scientific = FALSE
    ),
    "\n",
    sep = ""
  )
  
  # ---------------------------------------------------------------------------
  # Assignment status summary
  # ---------------------------------------------------------------------------
  
  status_summary <- dbGetQuery(
    con,
    paste0(
      "SELECT ",
      "assignment_status, ",
      "assignment_method, ",
      "COUNT(*) AS records ",
      "FROM ",
      quote_id(OUTPUT_TABLE),
      " GROUP BY ",
      "assignment_status, ",
      "assignment_method ",
      "ORDER BY records DESC"
    )
  )
  
  cat(
    "\n— Assignment status summary —\n"
  )
  
  print.data.frame(
    status_summary,
    row.names = FALSE
  )
  
  # ---------------------------------------------------------------------------
  # Botanical-area counts
  # ---------------------------------------------------------------------------
  
  area_summary <- dbGetQuery(
    con,
    paste0(
      "SELECT ",
      "botanical_area_id, ",
      "botanical_area_name, ",
      "COUNT(*) AS records ",
      "FROM ",
      quote_id(OUTPUT_TABLE),
      " WHERE botanical_area_id IS NOT NULL ",
      "GROUP BY ",
      "botanical_area_id, ",
      "botanical_area_name ",
      "ORDER BY botanical_area_id"
    )
  )
  
  cat(
    "\n— Assigned records by botanical area —\n"
  )
  
  print.data.frame(
    area_summary,
    row.names = FALSE
  )
  
  # ---------------------------------------------------------------------------
  # Outside-area diagnostics
  # ---------------------------------------------------------------------------
  
  outside_summary <- dbGetQuery(
    con,
    paste0(
      "SELECT ",
      "COUNT(*) AS records, ",
      "MIN(analytical_decimal_longitude) AS min_longitude, ",
      "MAX(analytical_decimal_longitude) AS max_longitude, ",
      "MIN(analytical_decimal_latitude) AS min_latitude, ",
      "MAX(analytical_decimal_latitude) AS max_latitude ",
      "FROM ",
      quote_id(OUTPUT_TABLE),
      " WHERE assignment_status = ",
      "'outside_botanical_areas'"
    )
  )
  
  cat(
    "\n— Outside-area coordinate extent —\n"
  )
  
  print.data.frame(
    outside_summary,
    row.names = FALSE
  )
  
  # ---------------------------------------------------------------------------
  # Candidate-count QA
  # ---------------------------------------------------------------------------
  
  candidate_summary <- dbGetQuery(
    con,
    paste0(
      "SELECT ",
      "candidate_area_count, ",
      "COUNT(*) AS records ",
      "FROM ",
      quote_id(OUTPUT_TABLE),
      " GROUP BY candidate_area_count ",
      "ORDER BY candidate_area_count"
    )
  )
  
  cat(
    "\n— Candidate-area count QA —\n"
  )
  
  print.data.frame(
    candidate_summary,
    row.names = FALSE
  )
  
  # ---------------------------------------------------------------------------
  # Validation
  # ---------------------------------------------------------------------------
  
  unresolved_pending <- dbGetQuery(
    con,
    paste0(
      "SELECT COUNT(*) AS n FROM ",
      quote_id(OUTPUT_TABLE),
      " WHERE assignment_status = 'pending'"
    )
  )$n[[1]]
  
  invalid_area_ids <- dbGetQuery(
    con,
    paste0(
      "SELECT COUNT(*) AS n FROM ",
      quote_id(OUTPUT_TABLE),
      " WHERE botanical_area_id IS NOT NULL ",
      "AND botanical_area_id NOT IN (",
      paste(
        paste0(
          "'",
          expected_ids,
          "'"
        ),
        collapse = ","
      ),
      ")"
    )
  )$n[[1]]
  
  validation <- tibble(
    check = c(
      "source_table_present",
      "source_records_3113089",
      "canonical_51_areas",
      "canonical_JP01_to_JP51_ids",
      "canonical_geometries_valid",
      "output_rows_equal_source_rows",
      "no_pending_assignments",
      "no_invalid_botanical_area_ids",
      "analytical_coordinates_not_modified",
      "taxonomy_not_modified",
      "source_occurrences_not_modified",
      "frozen_geography_not_modified",
      "stars_not_calculated",
      "ghi_not_calculated"
    ),
    
    pass = c(
      SOURCE_TABLE %in%
        existing_tables,
      
      source_count ==
        EXPECTED_RECORDS,
      
      nrow(areas) ==
        EXPECTED_AREAS,
      
      identical(
        areas$botanical_area_id,
        expected_ids
      ),
      
      all(
        st_is_valid(areas)
      ),
      
      output_count ==
        source_count,
      
      unresolved_pending == 0,
      
      invalid_area_ids == 0,
      
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
    "\nAll structural validation checks PASS: ",
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
      "source_table",
      "source_records",
      "longitude_field",
      "latitude_field",
      "canonical_botanical_areas",
      "boundary_source",
      "chunk_size",
      "chunks",
      "output_table",
      "output_records",
      "analytical_coordinates_modified",
      "taxonomy_modified",
      "stars_calculated",
      "ghi_calculated",
      "structural_validation_pass"
    ),
    
    value = c(
      "04_assign_integrated_occurrences_to_botanical_areas",
      SCRIPT_VERSION,
      SOURCE_TABLE,
      as.character(
        source_count
      ),
      lon_field,
      lat_field,
      as.character(
        nrow(areas)
      ),
      "Geography 03m v0.1.2",
      as.character(
        CHUNK_SIZE
      ),
      as.character(
        n_chunks
      ),
      OUTPUT_TABLE,
      as.character(
        output_count
      ),
      "FALSE",
      "FALSE",
      "FALSE",
      "FALSE",
      as.character(
        all_pass
      )
    )
  )
  
  # ---------------------------------------------------------------------------
  # Write QA tables
  # ---------------------------------------------------------------------------
  
  write_csv(
    coordinate_qa,
    file.path(
      output_dir,
      "occurrence_botanical_area_coordinate_qa.csv"
    )
  )
  
  write_csv(
    status_summary,
    file.path(
      output_dir,
      "occurrence_botanical_area_status_summary.csv"
    )
  )
  
  write_csv(
    area_summary,
    file.path(
      output_dir,
      "occurrence_botanical_area_counts.csv"
    )
  )
  
  write_csv(
    outside_summary,
    file.path(
      output_dir,
      "occurrence_botanical_area_outside_summary.csv"
    )
  )
  
  write_csv(
    candidate_summary,
    file.path(
      output_dir,
      "occurrence_botanical_area_candidate_summary.csv"
    )
  )
  
  write_csv(
    validation,
    file.path(
      output_dir,
      "occurrence_botanical_area_validation.csv"
    )
  )
  
  write_csv(
    metadata,
    file.path(
      output_dir,
      "occurrence_botanical_area_metadata.csv"
    )
  )
  
  # ---------------------------------------------------------------------------
  # Store QA in DuckDB
  # ---------------------------------------------------------------------------
  
  dbWriteTable(
    con,
    "vpjd_japan_occurrence_botanical_area_status_summary",
    status_summary,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "vpjd_japan_occurrence_botanical_area_counts",
    area_summary,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "vpjd_japan_occurrence_botanical_area_validation",
    validation,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "vpjd_japan_occurrence_botanical_area_metadata",
    metadata,
    overwrite = TRUE
  )
  
  # ---------------------------------------------------------------------------
  # Final report
  # ---------------------------------------------------------------------------
  
  if (!all_pass) {
    cat(
      "\nOutput status: FAILED STRUCTURAL VALIDATION\n"
    )
    
    stop(
      paste(
        "Geography 04 structural validation failed.",
        "Review QA before proceeding."
      )
    )
  }
  
  cat(
    "\nOutput table:\n",
    OUTPUT_TABLE,
    "\n",
    sep = ""
  )
  
  cat(
    "\nOutput status: ",
    "STRUCTURALLY VALIDATED BOTANICAL-AREA ASSIGNMENT\n",
    sep = ""
  )
  
  cat(
    "Source occurrences: ",
    format(
      source_count,
      big.mark = ",",
      scientific = FALSE
    ),
    "\n",
    sep = ""
  )
  
  cat(
    "Assignment rows: ",
    format(
      output_count,
      big.mark = ",",
      scientific = FALSE
    ),
    "\n",
    sep = ""
  )
  
  cat(
    "Botanical areas: ",
    nrow(areas),
    "\n",
    sep = ""
  )
  
  cat(
    "Analytical coordinates modified: FALSE\n"
  )
  
  cat(
    "Taxonomy modified: FALSE\n"
  )
  
  cat(
    "Stars calculated: FALSE\n"
  )
  
  cat(
    "GHI calculated: FALSE\n"
  )
  
  cat(
    "\nGeography 04 v",
    SCRIPT_VERSION,
    " complete.\n",
    sep = ""
  )
  
  invisible(
    list(
      coordinate_qa =
        coordinate_qa,
      
      status_summary =
        status_summary,
      
      area_summary =
        area_summary,
      
      outside_summary =
        outside_summary,
      
      candidate_summary =
        candidate_summary,
      
      validation =
        validation,
      
      metadata =
        metadata
    )
  )
}

result_04 <-
  run_botanical_area_assignment()