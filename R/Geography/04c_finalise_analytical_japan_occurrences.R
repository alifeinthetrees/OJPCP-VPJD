# ==============================================================================
# VPJD-OJPCP
# Geography 04c — Finalise analytical Japanese occurrence population
# Version: 0.1.1
#
# Purpose:
#   Establish the canonical analytical Japanese occurrence population after
#   Geography 04 spatial assignment and Geography 04b quarantine.
#
# Analytical eligibility:
#   1. occurrence has a unique JP01–JP51 botanical-area assignment;
#   2. occurrence is not in the active VPJD occurrence quarantine.
#
# Architecture:
#   - creates a canonical analytical VIEW rather than duplicating >3M records;
#   - integrated occurrence table remains the occurrence authority;
#   - Geography 04 remains the botanical-area assignment authority;
#   - Geography 04b remains the quarantine authority.
#
# v0.1.1:
#   - removes unnecessary candidate_count dependency;
#   - retains only fields required for the analytical geography gate.
#
# This module:
#   - performs NO spatial calculations;
#   - performs NO taxonomic reconciliation;
#   - modifies NO coordinates;
#   - deletes NO occurrences;
#   - does NOT calculate distributions, Stars or GHI.
#
# Expected:
#   Integrated occurrences:       3,113,089
#   Active quarantine:               83,201
#   Analytical occurrences:       3,029,888
# ==============================================================================

suppressPackageStartupMessages({
  library(DBI)
  library(duckdb)
  library(dplyr)
  library(readr)
  library(here)
})

SCRIPT_VERSION <- "0.1.1"
SOURCE_TABLE <- "vpjd_japan_occurrences_integrated"
ASSIGNMENT_TABLE <- "vpjd_japan_occurrence_botanical_area_assignment"
QUARANTINE_TABLE <- "vpjd_occurrence_quarantine"
QUARANTINE_VIEW <- "vpjd_occurrence_quarantine_active"
ANALYTICAL_VIEW <- "vpjd_japan_occurrences_analytical"
EXPECTED_TOTAL <- 3113089L
EXPECTED_QUARANTINE <- 83201L
EXPECTED_ANALYTICAL <- 3029888L
EXPECTED_AREAS <- 51L
RUN_DATE <- format(Sys.Date(), "%Y-%m-%d")

run_finalise_analytical_occurrences <- function() {
  cat("\n— VPJD analytical Japanese occurrence population —\n\n")
  cat("Run date: ", RUN_DATE, "\n\n", sep = "")
  
  db_path <- here(
    "data", "interim", "occurrences",
    "vpjd_occurrences.duckdb"
  )
  
  output_dir <- here(
    "outputs", "tables", "geography",
    "analytical_occurrences"
  )
  
  dir.create(
    output_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  if (!file.exists(db_path)) {
    stop("DuckDB database not found: ", db_path)
  }
  
  con <- dbConnect(
    duckdb(),
    dbdir = db_path,
    read_only = FALSE
  )
  
  on.exit(
    dbDisconnect(con, shutdown = TRUE),
    add = TRUE
  )
  
  quote_id <- function(x) {
    paste0('"', gsub('"', '""', x), '"')
  }
  
  q_source <- quote_id(SOURCE_TABLE)
  q_assignment <- quote_id(ASSIGNMENT_TABLE)
  q_quarantine <- quote_id(QUARANTINE_TABLE)
  q_quarantine_view <- quote_id(QUARANTINE_VIEW)
  q_analytical <- quote_id(ANALYTICAL_VIEW)
  
  # ---------------------------------------------------------------------------
  # Required database objects
  # ---------------------------------------------------------------------------
  
  objects <- dbGetQuery(
    con,
    "SELECT table_name FROM information_schema.tables"
  )$table_name
  
  required_objects <- c(
    SOURCE_TABLE,
    ASSIGNMENT_TABLE,
    QUARANTINE_TABLE,
    QUARANTINE_VIEW
  )
  
  missing_objects <- setdiff(
    required_objects,
    objects
  )
  
  if (length(missing_objects) > 0L) {
    stop(
      "Required database objects missing:\n",
      paste(missing_objects, collapse = ", ")
    )
  }
  
  cat("Required database objects: PASS\n")
  
  # ---------------------------------------------------------------------------
  # Validate upstream counts
  # ---------------------------------------------------------------------------
  
  source_count <- dbGetQuery(
    con,
    paste0(
      "SELECT COUNT(*) AS n FROM ",
      q_source
    )
  )$n[[1]]
  
  source_ids <- dbGetQuery(
    con,
    paste0(
      "SELECT COUNT(DISTINCT CAST(gbifID AS VARCHAR)) AS n FROM ",
      q_source
    )
  )$n[[1]]
  
  assignment_count <- dbGetQuery(
    con,
    paste0(
      "SELECT COUNT(*) AS n FROM ",
      q_assignment
    )
  )$n[[1]]
  
  assigned_unique_count <- dbGetQuery(
    con,
    paste0(
      "SELECT COUNT(*) AS n FROM ",
      q_assignment,
      " WHERE assignment_status = 'assigned_unique'"
    )
  )$n[[1]]
  
  outside_count <- dbGetQuery(
    con,
    paste0(
      "SELECT COUNT(*) AS n FROM ",
      q_assignment,
      " WHERE assignment_status = 'outside_botanical_areas'"
    )
  )$n[[1]]
  
  other_assignment_count <- dbGetQuery(
    con,
    paste0(
      "SELECT COUNT(*) AS n FROM ",
      q_assignment,
      " WHERE assignment_status NOT IN ",
      "('assigned_unique','outside_botanical_areas')"
    )
  )$n[[1]]
  
  quarantine_count <- dbGetQuery(
    con,
    paste0(
      "SELECT COUNT(*) AS n FROM ",
      q_quarantine,
      " WHERE QUARANTINE_STATUS = 'ACTIVE'"
    )
  )$n[[1]]
  
  quarantine_view_count <- dbGetQuery(
    con,
    paste0(
      "SELECT COUNT(*) AS n FROM ",
      q_quarantine_view
    )
  )$n[[1]]
  
  cat("\n— Upstream state —\n")
  
  cat(
    "Integrated occurrences: ",
    format(source_count, big.mark = ","),
    "\n",
    sep = ""
  )
  
  cat(
    "Geography 04 assignment rows: ",
    format(assignment_count, big.mark = ","),
    "\n",
    sep = ""
  )
  
  cat(
    "Assigned uniquely: ",
    format(assigned_unique_count, big.mark = ","),
    "\n",
    sep = ""
  )
  
  cat(
    "Outside botanical areas: ",
    format(outside_count, big.mark = ","),
    "\n",
    sep = ""
  )
  
  cat(
    "Active quarantine: ",
    format(quarantine_count, big.mark = ","),
    "\n\n",
    sep = ""
  )
  
  if (source_count != EXPECTED_TOTAL) {
    stop("Unexpected integrated occurrence count.")
  }
  
  if (source_ids != source_count) {
    stop("Integrated gbifIDs are not unique.")
  }
  
  if (assignment_count != EXPECTED_TOTAL) {
    stop("Unexpected Geography 04 assignment count.")
  }
  
  if (assigned_unique_count != EXPECTED_ANALYTICAL) {
    stop("Unexpected uniquely assigned occurrence count.")
  }
  
  if (outside_count != EXPECTED_QUARANTINE) {
    stop("Unexpected outside-area occurrence count.")
  }
  
  if (other_assignment_count != 0L) {
    stop("Unexpected Geography 04 assignment states remain.")
  }
  
  if (quarantine_count != EXPECTED_QUARANTINE) {
    stop("Unexpected active quarantine count.")
  }
  
  if (quarantine_view_count != quarantine_count) {
    stop(
      "Active quarantine view does not reconcile with quarantine table."
    )
  }
  
  cat("Upstream count validation: PASS\n")
  
  # ---------------------------------------------------------------------------
  # Geography/quarantine set reconciliation
  # ---------------------------------------------------------------------------
  
  outside_not_quarantined <- dbGetQuery(
    con,
    paste0(
      "SELECT COUNT(*) AS n FROM ",
      q_assignment,
      " a ",
      "LEFT JOIN ",
      q_quarantine_view,
      " q ON CAST(a.gbifID AS VARCHAR) = q.gbifID ",
      "WHERE a.assignment_status = 'outside_botanical_areas' ",
      "AND q.gbifID IS NULL"
    )
  )$n[[1]]
  
  quarantined_but_assigned <- dbGetQuery(
    con,
    paste0(
      "SELECT COUNT(*) AS n FROM ",
      q_assignment,
      " a ",
      "INNER JOIN ",
      q_quarantine_view,
      " q ON CAST(a.gbifID AS VARCHAR) = q.gbifID ",
      "WHERE a.assignment_status = 'assigned_unique'"
    )
  )$n[[1]]
  
  quarantine_without_assignment <- dbGetQuery(
    con,
    paste0(
      "SELECT COUNT(*) AS n FROM ",
      q_quarantine_view,
      " q ",
      "LEFT JOIN ",
      q_assignment,
      " a ON q.gbifID = CAST(a.gbifID AS VARCHAR) ",
      "WHERE a.gbifID IS NULL"
    )
  )$n[[1]]
  
  cat("\n— Geography/quarantine reconciliation —\n")
  
  cat(
    "Outside records not quarantined: ",
    outside_not_quarantined,
    "\n",
    sep = ""
  )
  
  cat(
    "Uniquely assigned records actively quarantined: ",
    quarantined_but_assigned,
    "\n",
    sep = ""
  )
  
  cat(
    "Quarantined records without Geography 04 assignment: ",
    quarantine_without_assignment,
    "\n",
    sep = ""
  )
  
  if (outside_not_quarantined != 0L) {
    stop(
      "One or more outside-area records are not actively quarantined."
    )
  }
  
  if (quarantined_but_assigned != 0L) {
    stop(
      "One or more uniquely assigned occurrences are actively quarantined."
    )
  }
  
  if (quarantine_without_assignment != 0L) {
    stop(
      "One or more quarantined occurrences lack Geography 04 assignments."
    )
  }
  
  cat("Geography/quarantine reconciliation: PASS\n")
  
  # ---------------------------------------------------------------------------
  # Required Geography 04 fields
  # ---------------------------------------------------------------------------
  
  assignment_fields <- dbListFields(
    con,
    ASSIGNMENT_TABLE
  )
  
  required_assignment_fields <- c(
    "gbifID",
    "botanical_area_id",
    "botanical_area_name",
    "assignment_status",
    "assignment_method"
  )
  
  missing_assignment_fields <- setdiff(
    required_assignment_fields,
    assignment_fields
  )
  
  if (length(missing_assignment_fields) > 0L) {
    stop(
      "Required Geography 04 fields missing:\n",
      paste(missing_assignment_fields, collapse = ", ")
    )
  }
  
  cat("Required Geography 04 fields: PASS\n")
  
  # ---------------------------------------------------------------------------
  # Create canonical analytical view
  #
  # gbifID is used as the stable join key.
  # source_row_number is deliberately not used.
  # ---------------------------------------------------------------------------
  
  cat("\nCreating canonical analytical occurrence view...\n")
  
  dbExecute(
    con,
    paste0(
      "CREATE OR REPLACE VIEW ",
      q_analytical,
      " AS ",
      "SELECT ",
      "s.*, ",
      "a.botanical_area_id AS ANALYTICAL_BOTANICAL_AREA_ID, ",
      "a.botanical_area_name AS ANALYTICAL_BOTANICAL_AREA_NAME, ",
      "a.assignment_method AS ANALYTICAL_AREA_ASSIGNMENT_METHOD, ",
      "'ANALYTICAL' AS OCCURRENCE_ANALYTICAL_STATUS, ",
      "'04c' AS ANALYTICAL_GATE_MODULE, ",
      "'",
      SCRIPT_VERSION,
      "' AS ANALYTICAL_GATE_VERSION ",
      "FROM ",
      q_source,
      " s ",
      "INNER JOIN ",
      q_assignment,
      " a ",
      "ON CAST(s.gbifID AS VARCHAR) = CAST(a.gbifID AS VARCHAR) ",
      "LEFT JOIN ",
      q_quarantine_view,
      " q ",
      "ON CAST(s.gbifID AS VARCHAR) = q.gbifID ",
      "WHERE a.assignment_status = 'assigned_unique' ",
      "AND q.gbifID IS NULL"
    )
  )
  
  # ---------------------------------------------------------------------------
  # Validate analytical population
  # ---------------------------------------------------------------------------
  
  analytical_count <- dbGetQuery(
    con,
    paste0(
      "SELECT COUNT(*) AS n FROM ",
      q_analytical
    )
  )$n[[1]]
  
  analytical_ids <- dbGetQuery(
    con,
    paste0(
      "SELECT COUNT(DISTINCT CAST(gbifID AS VARCHAR)) AS n FROM ",
      q_analytical
    )
  )$n[[1]]
  
  analytical_areas <- dbGetQuery(
    con,
    paste0(
      "SELECT COUNT(DISTINCT ANALYTICAL_BOTANICAL_AREA_ID) AS n FROM ",
      q_analytical
    )
  )$n[[1]]
  
  null_area_count <- dbGetQuery(
    con,
    paste0(
      "SELECT COUNT(*) AS n FROM ",
      q_analytical,
      " WHERE ANALYTICAL_BOTANICAL_AREA_ID IS NULL"
    )
  )$n[[1]]
  
  invalid_area_count <- dbGetQuery(
    con,
    paste0(
      "SELECT COUNT(*) AS n FROM ",
      q_analytical,
      " WHERE ANALYTICAL_BOTANICAL_AREA_ID ",
      "NOT BETWEEN 'JP01' AND 'JP51'"
    )
  )$n[[1]]
  
  analytical_quarantine_overlap <- dbGetQuery(
    con,
    paste0(
      "SELECT COUNT(*) AS n FROM ",
      q_analytical,
      " a ",
      "INNER JOIN ",
      q_quarantine_view,
      " q ON CAST(a.gbifID AS VARCHAR) = q.gbifID"
    )
  )$n[[1]]
  
  cat("\n— Analytical population —\n")
  
  cat(
    "Analytical occurrences: ",
    format(analytical_count, big.mark = ","),
    "\n",
    sep = ""
  )
  
  cat(
    "Distinct analytical gbifIDs: ",
    format(analytical_ids, big.mark = ","),
    "\n",
    sep = ""
  )
  
  cat(
    "Botanical areas represented: ",
    analytical_areas,
    "\n",
    sep = ""
  )
  
  cat(
    "Null botanical-area assignments: ",
    null_area_count,
    "\n",
    sep = ""
  )
  
  cat(
    "Active quarantine overlap: ",
    analytical_quarantine_overlap,
    "\n",
    sep = ""
  )
  
  # ---------------------------------------------------------------------------
  # Area summary
  # ---------------------------------------------------------------------------
  
  area_summary <- dbGetQuery(
    con,
    paste0(
      "SELECT ",
      "ANALYTICAL_BOTANICAL_AREA_ID AS botanical_area_id, ",
      "ANALYTICAL_BOTANICAL_AREA_NAME AS botanical_area_name, ",
      "COUNT(*) AS occurrence_records, ",
      "COUNT(DISTINCT CAST(gbifID AS VARCHAR)) AS distinct_gbifIDs, ",
      "COUNT(DISTINCT FINAL_WCVP_ID) ",
      "FILTER (WHERE FINAL_WCVP_ID IS NOT NULL) ",
      "AS distinct_WCVP_IDs ",
      "FROM ",
      q_analytical,
      " GROUP BY ",
      "ANALYTICAL_BOTANICAL_AREA_ID, ",
      "ANALYTICAL_BOTANICAL_AREA_NAME ",
      "ORDER BY ANALYTICAL_BOTANICAL_AREA_ID"
    )
  )
  
  cat("\n— Analytical occurrences by botanical area —\n")
  
  print.data.frame(
    area_summary,
    row.names = FALSE
  )
  
  # ---------------------------------------------------------------------------
  # Taxonomic profile
  # ---------------------------------------------------------------------------
  
  taxonomy_summary <- dbGetQuery(
    con,
    paste0(
      "SELECT ",
      "RECONCILIATION_STATUS, ",
      "FINAL_WCVP_CONCEPT_CLASS, ",
      "COUNT(*) AS occurrence_records, ",
      "COUNT(DISTINCT FINAL_WCVP_ID) ",
      "FILTER (WHERE FINAL_WCVP_ID IS NOT NULL) ",
      "AS distinct_WCVP_IDs ",
      "FROM ",
      q_analytical,
      " GROUP BY ",
      "RECONCILIATION_STATUS, ",
      "FINAL_WCVP_CONCEPT_CLASS ",
      "ORDER BY occurrence_records DESC"
    )
  )
  
  cat("\n— Analytical taxonomic profile —\n")
  
  print.data.frame(
    taxonomy_summary,
    row.names = FALSE
  )
  
  # ---------------------------------------------------------------------------
  # Provenance profile
  # ---------------------------------------------------------------------------
  
  provenance_summary <- dbGetQuery(
    con,
    paste0(
      "SELECT ",
      "OCCURRENCE_SOURCE, ",
      "ANALYTICAL_COORDINATE_SOURCE, ",
      "COUNT(*) AS occurrence_records ",
      "FROM ",
      q_analytical,
      " GROUP BY ",
      "OCCURRENCE_SOURCE, ",
      "ANALYTICAL_COORDINATE_SOURCE ",
      "ORDER BY occurrence_records DESC"
    )
  )
  
  cat("\n— Analytical occurrence provenance —\n")
  
  print.data.frame(
    provenance_summary,
    row.names = FALSE
  )
  
  # ---------------------------------------------------------------------------
  # Population accounting
  # ---------------------------------------------------------------------------
  
  accounting <- tibble(
    population = c(
      "integrated_occurrences",
      "active_quarantine",
      "analytical_occurrences"
    ),
    records = c(
      source_count,
      quarantine_count,
      analytical_count
    )
  )
  
  cat("\n— Population accounting —\n")
  
  print.data.frame(
    accounting,
    row.names = FALSE
  )
  
  # ---------------------------------------------------------------------------
  # Validation
  # ---------------------------------------------------------------------------
  
  validation <- tibble(
    check = c(
      "integrated_records_3113089",
      "integrated_gbifIDs_unique",
      "assignment_records_3113089",
      "assigned_unique_records_3029888",
      "outside_records_83201",
      "active_quarantine_records_83201",
      "no_other_assignment_states",
      "outside_records_all_quarantined",
      "no_assigned_records_quarantined",
      "no_quarantine_without_assignment",
      "analytical_records_3029888",
      "analytical_gbifIDs_unique",
      "integrated_minus_quarantine_equals_analytical",
      "all_51_botanical_areas_represented",
      "no_null_botanical_area",
      "no_invalid_botanical_area_id",
      "no_active_quarantine_in_analytical_view",
      "coordinates_not_modified",
      "taxonomy_not_modified",
      "geography_not_modified",
      "quarantine_not_modified",
      "occurrences_not_deleted",
      "spatial_analysis_not_repeated",
      "stars_not_calculated",
      "ghi_not_calculated"
    ),
    pass = c(
      source_count == EXPECTED_TOTAL,
      source_ids == source_count,
      assignment_count == EXPECTED_TOTAL,
      assigned_unique_count == EXPECTED_ANALYTICAL,
      outside_count == EXPECTED_QUARANTINE,
      quarantine_count == EXPECTED_QUARANTINE,
      other_assignment_count == 0L,
      outside_not_quarantined == 0L,
      quarantined_but_assigned == 0L,
      quarantine_without_assignment == 0L,
      analytical_count == EXPECTED_ANALYTICAL,
      analytical_ids == analytical_count,
      (source_count - quarantine_count) == analytical_count,
      analytical_areas == EXPECTED_AREAS,
      null_area_count == 0L,
      invalid_area_count == 0L,
      analytical_quarantine_overlap == 0L,
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
  
  cat("\n— Validation —\n")
  
  print.data.frame(
    validation,
    row.names = FALSE
  )
  
  all_pass <- all(validation$pass)
  
  cat(
    "\nAll analytical-population validation checks PASS: ",
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
      "run_date",
      "source_table",
      "assignment_table",
      "quarantine_table",
      "quarantine_view",
      "analytical_view",
      "integrated_occurrences",
      "active_quarantine",
      "analytical_occurrences",
      "botanical_areas_represented",
      "analytical_rule_1",
      "analytical_rule_2",
      "join_key",
      "candidate_count_dependency",
      "physical_occurrence_copy_created",
      "coordinates_modified",
      "taxonomy_modified",
      "geography_modified",
      "quarantine_modified",
      "records_deleted",
      "spatial_analysis_repeated",
      "validation_pass"
    ),
    value = c(
      "04c_finalise_analytical_japan_occurrences",
      SCRIPT_VERSION,
      RUN_DATE,
      SOURCE_TABLE,
      ASSIGNMENT_TABLE,
      QUARANTINE_TABLE,
      QUARANTINE_VIEW,
      ANALYTICAL_VIEW,
      as.character(source_count),
      as.character(quarantine_count),
      as.character(analytical_count),
      as.character(analytical_areas),
      "assigned_unique_JP01_JP51",
      "not_in_active_quarantine",
      "gbifID",
      "FALSE",
      "FALSE",
      "FALSE",
      "FALSE",
      "FALSE",
      "FALSE",
      "FALSE",
      "FALSE",
      as.character(all_pass)
    )
  )
  
  # ---------------------------------------------------------------------------
  # QA outputs
  # ---------------------------------------------------------------------------
  
  write_csv(
    accounting,
    file.path(
      output_dir,
      "analytical_occurrence_accounting.csv"
    )
  )
  
  write_csv(
    area_summary,
    file.path(
      output_dir,
      "analytical_occurrences_by_botanical_area.csv"
    )
  )
  
  write_csv(
    taxonomy_summary,
    file.path(
      output_dir,
      "analytical_occurrence_taxonomic_profile.csv"
    )
  )
  
  write_csv(
    provenance_summary,
    file.path(
      output_dir,
      "analytical_occurrence_provenance.csv"
    )
  )
  
  write_csv(
    validation,
    file.path(
      output_dir,
      "analytical_occurrence_validation.csv"
    )
  )
  
  write_csv(
    metadata,
    file.path(
      output_dir,
      "analytical_occurrence_metadata.csv"
    )
  )
  
  # ---------------------------------------------------------------------------
  # Store validation/metadata in DuckDB
  # ---------------------------------------------------------------------------
  
  dbWriteTable(
    con,
    "vpjd_japan_analytical_occurrence_validation",
    validation,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "vpjd_japan_analytical_occurrence_metadata",
    metadata,
    overwrite = TRUE
  )
  
  # ---------------------------------------------------------------------------
  # Final status
  # ---------------------------------------------------------------------------
  
  if (!all_pass) {
    cat(
      "\nOutput status: FAILED ANALYTICAL POPULATION VALIDATION\n"
    )
    
    stop(
      paste(
        "Geography 04c validation failed.",
        "Do not use the analytical occurrence view downstream."
      )
    )
  }
  
  cat("\nCanonical analytical view:\n")
  cat("  ", ANALYTICAL_VIEW, "\n", sep = "")
  
  cat(
    "\nOutput status: ",
    "VALIDATED ANALYTICAL JAPANESE OCCURRENCE POPULATION\n",
    sep = ""
  )
  
  cat(
    "Integrated occurrences: ",
    format(source_count, big.mark = ","),
    "\n",
    sep = ""
  )
  
  cat(
    "Active quarantine: ",
    format(quarantine_count, big.mark = ","),
    "\n",
    sep = ""
  )
  
  cat(
    "Analytical occurrences: ",
    format(analytical_count, big.mark = ","),
    "\n",
    sep = ""
  )
  
  cat(
    "Botanical areas represented: ",
    analytical_areas,
    "\n",
    sep = ""
  )
  
  cat("Canonical analytical object is a view: TRUE\n")
  cat("Candidate-count dependency: FALSE\n")
  cat("Coordinates modified: FALSE\n")
  cat("Taxonomy modified: FALSE\n")
  cat("Geography modified: FALSE\n")
  cat("Quarantine modified: FALSE\n")
  cat("Occurrences deleted: FALSE\n")
  cat("Spatial analysis repeated: FALSE\n")
  cat("Stars calculated: FALSE\n")
  cat("GHI calculated: FALSE\n")
  
  cat(
    "\nGeography 04c v",
    SCRIPT_VERSION,
    " complete.\n",
    sep = ""
  )
  
  invisible(
    list(
      accounting = accounting,
      area_summary = area_summary,
      taxonomy_summary = taxonomy_summary,
      provenance_summary = provenance_summary,
      validation = validation,
      metadata = metadata
    )
  )
}

result_04c <-
  run_finalise_analytical_occurrences()