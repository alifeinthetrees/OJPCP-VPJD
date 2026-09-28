# ==============================================================================
# VPJD-OJPCP
# 02_reconcile_recovered_taxa.R
# Version: 0.1.4
#
# Purpose:
#   Reconcile taxa represented by recovered, gazetteer-georeferenced
#   institutional occurrences against the frozen 04p VPJD/WCVP taxonomy.
#
# Principles:
#   - Reuse frozen 04p taxonomic decisions wherever possible.
#   - Exact scientific-name matching only.
#   - No fuzzy/edit-distance matching.
#   - No higher-rank inference.
#   - No new taxonomic opinion.
#   - Preserve original recovered scientificName.
#   - Do not modify frozen taxonomy or Integration 01 tables.
#   - Do not merge records into occurrences_clean.
#
# Required frozen inputs:
#   Integration 01 v0.1.0
#   Taxonomy 04p v0.1.1
# ==============================================================================

suppressPackageStartupMessages({
  library(DBI)
  library(duckdb)
  library(dplyr)
  library(stringr)
  library(readr)
  library(tibble)
  library(here)
})

SCRIPT_VERSION <- "0.1.4"
EXPECTED_INTEGRATION_01_VERSION <- "0.1.0"
EXPECTED_TAXONOMY_04P_VERSION <- "0.1.1"
EXPECTED_RECOVERED_RECORDS <- 13290L

normalise_taxon_name <- function(x) {
  x <- as.character(x)
  x <- enc2utf8(x)
  x <- str_replace_all(x, "[[:cntrl:]]", "")
  x <- str_squish(x)
  x[x == ""] <- NA_character_
  x
}

normalise_taxon_key <- function(x) {
  x <- normalise_taxon_name(x)
  x <- str_to_lower(x)
  x <- str_replace_all(x, "×", "x")
  x <- str_replace_all(x, "\\s+", " ")
  x
}

get_metadata_value <- function(con, table_name, metric_name) {
  fields <- dbListFields(con, table_name)
  
  if (all(c("metric", "value") %in% fields)) {
    sql <- paste0(
      "SELECT CAST(value AS VARCHAR) AS value FROM ",
      dbQuoteIdentifier(con, table_name),
      " WHERE CAST(metric AS VARCHAR) = ?"
    )
    result <- dbGetQuery(con, sql, params = list(metric_name))
    
    if (nrow(result) != 1) {
      stop(
        "Expected exactly one '", metric_name,
        "' row in ", table_name,
        "; found ", nrow(result), "."
      )
    }
    
    return(as.character(result$value[[1]]))
  }
  
  if (metric_name %in% fields) {
    metadata <- dbReadTable(con, table_name)
    
    if (nrow(metadata) != 1) {
      stop(
        "Expected exactly one metadata row in ",
        table_name, "; found ", nrow(metadata), "."
      )
    }
    
    return(as.character(metadata[[metric_name]][[1]]))
  }
  
  stop(
    "Could not find metadata item '", metric_name,
    "' in ", table_name, "."
  )
}

run_recovered_taxon_reconciliation <- function() {
  
  cat("\n— VPJD recovered institutional taxon reconciliation —\n\n")
  
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
  
  required_tables <- c(
    "recovered_institutional_occurrences",
    "recovered_institutional_occurrence_metadata",
    "occurrence_wcvp_consolidated",
    "occurrence_wcvp_recognised_taxa",
    "occurrence_wcvp_accepted_taxa",
    "occurrence_wcvp_artificial_hybrid_taxa",
    "occurrence_wcvp_unplaced_taxa",
    "occurrence_wcvp_consolidation_metadata"
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
  
  # ---------------------------------------------------------------------------
  # Validate frozen input versions
  # ---------------------------------------------------------------------------
  
  integration_version <- get_metadata_value(
    con,
    "recovered_institutional_occurrence_metadata",
    "script_version"
  )
  
  taxonomy_version <- get_metadata_value(
    con,
    "occurrence_wcvp_consolidation_metadata",
    "script_version"
  )
  
  if (integration_version != EXPECTED_INTEGRATION_01_VERSION) {
    stop(
      "Expected Integration 01 v",
      EXPECTED_INTEGRATION_01_VERSION,
      "; detected ", integration_version, "."
    )
  }
  
  if (taxonomy_version != EXPECTED_TAXONOMY_04P_VERSION) {
    stop(
      "Expected taxonomy 04p v",
      EXPECTED_TAXONOMY_04P_VERSION,
      "; detected ", taxonomy_version, "."
    )
  }
  
  cat(
    "Frozen Integration 01 version: ",
    integration_version, "\n", sep = ""
  )
  
  cat(
    "Frozen taxonomy 04p version: ",
    taxonomy_version, "\n", sep = ""
  )
  
  # ---------------------------------------------------------------------------
  # Read recovered occurrence layer
  # ---------------------------------------------------------------------------
  
  recovered <- dbReadTable(
    con,
    "recovered_institutional_occurrences"
  ) %>%
    as_tibble()
  
  required_recovered_fields <- c(
    "gbifID",
    "scientificName",
    "taxonRank"
  )
  
  missing_recovered_fields <- setdiff(
    required_recovered_fields,
    names(recovered)
  )
  
  if (length(missing_recovered_fields) > 0) {
    stop(
      "Recovered occurrence layer lacks required fields: ",
      paste(missing_recovered_fields, collapse = ", ")
    )
  }
  
  if (nrow(recovered) != EXPECTED_RECOVERED_RECORDS) {
    stop(
      "Unexpected recovered record count: ",
      nrow(recovered),
      "; expected ",
      EXPECTED_RECOVERED_RECORDS, "."
    )
  }
  
  if (n_distinct(recovered$gbifID) != EXPECTED_RECOVERED_RECORDS) {
    stop(
      "Recovered layer does not contain exactly ",
      EXPECTED_RECOVERED_RECORDS,
      " distinct gbifID values."
    )
  }
  
  cat(
    "Recovered institutional records: ",
    format(nrow(recovered), big.mark = ","),
    "\n", sep = ""
  )
  
  recovered <- recovered %>%
    mutate(
      RECOVERED_SCIENTIFIC_NAME =
        normalise_taxon_name(scientificName),
      RECOVERED_NAME_KEY =
        normalise_taxon_key(scientificName),
      RECOVERED_TAXON_RANK =
        normalise_taxon_name(taxonRank)
    )
  
  if (any(is.na(recovered$RECOVERED_NAME_KEY))) {
    stop(
      "At least one recovered record lacks a usable scientificName."
    )
  }
  
  recovered_names <- recovered %>%
    group_by(RECOVERED_NAME_KEY) %>%
    summarise(
      RECOVERED_SCIENTIFIC_NAME =
        first(RECOVERED_SCIENTIFIC_NAME),
      RECOVERED_TAXON_RANK =
        paste(
          sort(
            unique(
              RECOVERED_TAXON_RANK[
                !is.na(RECOVERED_TAXON_RANK)
              ]
            )
          ),
          collapse = "; "
        ),
      OCCURRENCE_RECORDS = n(),
      DISTINCT_GBIF_IDS = n_distinct(gbifID),
      .groups = "drop"
    )
  
  cat(
    "Distinct recovered scientific names: ",
    format(nrow(recovered_names), big.mark = ","),
    "\n\n", sep = ""
  )
  
  # ---------------------------------------------------------------------------
  # Read frozen 04p tables
  # ---------------------------------------------------------------------------
  
  consolidated <- dbReadTable(
    con,
    "occurrence_wcvp_consolidated"
  ) %>%
    as_tibble()
  
  recognised <- dbReadTable(
    con,
    "occurrence_wcvp_recognised_taxa"
  ) %>%
    as_tibble()
  
  accepted <- dbReadTable(
    con,
    "occurrence_wcvp_accepted_taxa"
  ) %>%
    as_tibble()
  
  artificial <- dbReadTable(
    con,
    "occurrence_wcvp_artificial_hybrid_taxa"
  ) %>%
    as_tibble()
  
  unplaced <- dbReadTable(
    con,
    "occurrence_wcvp_unplaced_taxa"
  ) %>%
    as_tibble()
  
  # ---------------------------------------------------------------------------
  # Validate known 04p schemas
  # ---------------------------------------------------------------------------
  
  required_consolidated_fields <- c(
    "scientificName",
    "final_wcvp_accepted_plant_name_id",
    "final_wcvp_taxon_name",
    "final_wcvp_taxon_rank",
    "final_wcvp_taxon_status",
    "vpjd_taxonomic_status",
    "vpjd_recognised_taxon",
    "vpjd_accepted_taxon",
    "vpjd_artificial_hybrid",
    "vpjd_unplaced_taxon"
  )
  
  required_recognised_fields <- c(
    "wcvp_plant_name_id",
    "wcvp_taxon_name",
    "wcvp_taxon_rank",
    "wcvp_taxon_status",
    "vpjd_taxonomic_status"
  )
  
  missing_consolidated_fields <- setdiff(
    required_consolidated_fields,
    names(consolidated)
  )
  
  missing_recognised_fields <- setdiff(
    required_recognised_fields,
    names(recognised)
  )
  
  if (length(missing_consolidated_fields) > 0) {
    stop(
      "04p consolidated table lacks expected fields: ",
      paste(missing_consolidated_fields, collapse = ", ")
    )
  }
  
  if (length(missing_recognised_fields) > 0) {
    stop(
      "04p recognised table lacks expected fields: ",
      paste(missing_recognised_fields, collapse = ", ")
    )
  }
  
  if (!"wcvp_plant_name_id" %in% names(accepted)) {
    stop("Accepted-taxonomy table lacks wcvp_plant_name_id.")
  }
  
  if (!"wcvp_plant_name_id" %in% names(artificial)) {
    stop("Artificial-hybrid table lacks wcvp_plant_name_id.")
  }
  
  if (
    !"final_wcvp_accepted_plant_name_id" %in%
    names(unplaced)
  ) {
    stop(
      "Unplaced table lacks final_wcvp_accepted_plant_name_id."
    )
  }
  
  cat("04p schema validation: PASS\n\n")
  
  # ---------------------------------------------------------------------------
  # Build authoritative recognised WCVP concept lookup
  # ---------------------------------------------------------------------------
  
  recognised_lookup <- recognised %>%
    transmute(
      WCVP_ID =
        as.character(wcvp_plant_name_id),
      WCVP_RECOGNISED_NAME =
        normalise_taxon_name(wcvp_taxon_name),
      WCVP_RECOGNISED_NAME_KEY =
        normalise_taxon_key(wcvp_taxon_name),
      WCVP_RANK =
        as.character(wcvp_taxon_rank),
      WCVP_STATUS =
        as.character(wcvp_taxon_status),
      VPJD_TAXONOMIC_STATUS =
        as.character(vpjd_taxonomic_status)
    ) %>%
    filter(
      !is.na(WCVP_ID),
      WCVP_ID != ""
    )
  
  if (
    anyDuplicated(
      recognised_lookup$WCVP_ID
    ) > 0
  ) {
    stop(
      "Recognised 04p table contains duplicate wcvp_plant_name_id values."
    )
  }
  
  accepted_ids <- accepted %>%
    transmute(
      WCVP_ID =
        as.character(wcvp_plant_name_id)
    ) %>%
    filter(
      !is.na(WCVP_ID),
      WCVP_ID != ""
    ) %>%
    distinct() %>%
    pull(WCVP_ID)
  
  artificial_ids <- artificial %>%
    transmute(
      WCVP_ID =
        as.character(wcvp_plant_name_id)
    ) %>%
    filter(
      !is.na(WCVP_ID),
      WCVP_ID != ""
    ) %>%
    distinct() %>%
    pull(WCVP_ID)
  
  unplaced_ids <- unplaced %>%
    filter(
      vpjd_unplaced_taxon %in% TRUE
    ) %>%
    transmute(
      WCVP_ID =
        as.character(
          final_wcvp_accepted_plant_name_id
        )
    ) %>%
    filter(
      !is.na(WCVP_ID),
      WCVP_ID != ""
    ) %>%
    distinct() %>%
    pull(WCVP_ID)
  
  recognised_lookup <- recognised_lookup %>%
    mutate(
      WCVP_CONCEPT_CLASS =
        case_when(
          WCVP_ID %in% artificial_ids ~
            "artificial_hybrid_wcvp",
          WCVP_ID %in% unplaced_ids ~
            "unplaced_wcvp",
          WCVP_ID %in% accepted_ids ~
            "accepted_wcvp",
          TRUE ~
            "recognised_wcvp"
        )
    )
  
  cat(
    "Frozen recognised WCVP taxa: ",
    format(nrow(recognised_lookup), big.mark = ","),
    "\n", sep = ""
  )
  
  cat(
    "Frozen accepted WCVP taxa: ",
    format(length(accepted_ids), big.mark = ","),
    "\n", sep = ""
  )
  
  cat(
    "Frozen artificial hybrids: ",
    format(length(artificial_ids), big.mark = ","),
    "\n", sep = ""
  )
  
  cat(
    "Frozen unplaced WCVP IDs: ",
    format(length(unplaced_ids), big.mark = ","),
    "\n\n", sep = ""
  )
  
  # ---------------------------------------------------------------------------
  # Stage 1:
  # Reuse exact source scientificName -> final 04p decision
  # ---------------------------------------------------------------------------
  
  source_crosswalk <- consolidated %>%
    transmute(
      MATCH_NAME =
        normalise_taxon_name(scientificName),
      MATCH_NAME_KEY =
        normalise_taxon_key(scientificName),
      WCVP_ID =
        as.character(
          final_wcvp_accepted_plant_name_id
        ),
      FINAL_WCVP_TAXON_NAME =
        normalise_taxon_name(
          final_wcvp_taxon_name
        ),
      FINAL_WCVP_TAXON_RANK =
        as.character(
          final_wcvp_taxon_rank
        ),
      FINAL_WCVP_TAXON_STATUS =
        as.character(
          final_wcvp_taxon_status
        ),
      VPJD_TAXONOMIC_STATUS =
        as.character(
          vpjd_taxonomic_status
        ),
      VPJD_RECOGNISED_TAXON =
        as.logical(
          vpjd_recognised_taxon
        ),
      VPJD_ACCEPTED_TAXON =
        as.logical(
          vpjd_accepted_taxon
        ),
      VPJD_ARTIFICIAL_HYBRID =
        as.logical(
          vpjd_artificial_hybrid
        ),
      VPJD_UNPLACED_TAXON =
        as.logical(
          vpjd_unplaced_taxon
        )
    ) %>%
    filter(
      !is.na(MATCH_NAME_KEY)
    )
  
  source_name_diagnostics <- source_crosswalk %>%
    group_by(MATCH_NAME_KEY) %>%
    summarise(
      SOURCE_ROWS = n(),
      DISTINCT_FINAL_WCVP_IDS =
        n_distinct(
          WCVP_ID[
            !is.na(WCVP_ID) &
              WCVP_ID != ""
          ]
        ),
      .groups = "drop"
    )
  
  ambiguous_source_names <- source_name_diagnostics %>%
    filter(
      DISTINCT_FINAL_WCVP_IDS > 1
    )
  
  secure_source_crosswalk <- source_crosswalk %>%
    inner_join(
      source_name_diagnostics %>%
        filter(
          DISTINCT_FINAL_WCVP_IDS <= 1
        ),
      by = "MATCH_NAME_KEY"
    ) %>%
    group_by(MATCH_NAME_KEY) %>%
    summarise(
      SOURCE_MATCH_NAME =
        first(MATCH_NAME),
      SOURCE_MATCH_WCVP_ID = {
        x <- WCVP_ID[
          !is.na(WCVP_ID) &
            WCVP_ID != ""
        ]
        if (length(x) == 0) NA_character_ else first(x)
      },
      SOURCE_FINAL_WCVP_TAXON_NAME = {
        x <- FINAL_WCVP_TAXON_NAME[
          !is.na(FINAL_WCVP_TAXON_NAME)
        ]
        if (length(x) == 0) NA_character_ else first(x)
      },
      SOURCE_FINAL_WCVP_TAXON_RANK = {
        x <- FINAL_WCVP_TAXON_RANK[
          !is.na(FINAL_WCVP_TAXON_RANK)
        ]
        if (length(x) == 0) NA_character_ else first(x)
      },
      SOURCE_FINAL_WCVP_TAXON_STATUS = {
        x <- FINAL_WCVP_TAXON_STATUS[
          !is.na(FINAL_WCVP_TAXON_STATUS)
        ]
        if (length(x) == 0) NA_character_ else first(x)
      },
      SOURCE_VPJD_TAXONOMIC_STATUS = {
        x <- VPJD_TAXONOMIC_STATUS[
          !is.na(VPJD_TAXONOMIC_STATUS)
        ]
        if (length(x) == 0) NA_character_ else first(x)
      },
      .groups = "drop"
    )
  
  cat(
    "Secure frozen 04p source-name mappings: ",
    format(
      sum(
        !is.na(
          secure_source_crosswalk$SOURCE_MATCH_WCVP_ID
        )
      ),
      big.mark = ","
    ),
    "\n", sep = ""
  )
  
  cat(
    "Ambiguous frozen 04p source names: ",
    format(nrow(ambiguous_source_names), big.mark = ","),
    "\n\n", sep = ""
  )
  
  # ---------------------------------------------------------------------------
  # Stage 2:
  # Exact match directly to recognised WCVP taxon names
  # ---------------------------------------------------------------------------
  
  recognised_name_diagnostics <- recognised_lookup %>%
    filter(
      !is.na(
        WCVP_RECOGNISED_NAME_KEY
      )
    ) %>%
    group_by(
      WCVP_RECOGNISED_NAME_KEY
    ) %>%
    summarise(
      DISTINCT_WCVP_IDS =
        n_distinct(WCVP_ID),
      .groups = "drop"
    )
  
  secure_recognised_name_crosswalk <- recognised_lookup %>%
    inner_join(
      recognised_name_diagnostics %>%
        filter(
          DISTINCT_WCVP_IDS == 1
        ),
      by = "WCVP_RECOGNISED_NAME_KEY"
    ) %>%
    distinct(
      WCVP_RECOGNISED_NAME_KEY,
      .keep_all = TRUE
    ) %>%
    transmute(
      RECOVERED_NAME_KEY =
        WCVP_RECOGNISED_NAME_KEY,
      RECOGNISED_MATCH_WCVP_ID =
        WCVP_ID
    )
  
  # ---------------------------------------------------------------------------
  # Reconcile recovered name universe
  # ---------------------------------------------------------------------------
  
  reconciliation <- recovered_names %>%
    left_join(
      secure_source_crosswalk,
      by = c(
        "RECOVERED_NAME_KEY" =
          "MATCH_NAME_KEY"
      )
    ) %>%
    left_join(
      ambiguous_source_names %>%
        transmute(
          RECOVERED_NAME_KEY =
            MATCH_NAME_KEY,
          SOURCE_NAME_AMBIGUOUS =
            TRUE
        ),
      by = "RECOVERED_NAME_KEY"
    ) %>%
    left_join(
      secure_recognised_name_crosswalk,
      by = "RECOVERED_NAME_KEY"
    ) %>%
    mutate(
      SOURCE_NAME_AMBIGUOUS =
        coalesce(
          SOURCE_NAME_AMBIGUOUS,
          FALSE
        ),
      
      FINAL_WCVP_ID =
        case_when(
          !is.na(
            SOURCE_MATCH_WCVP_ID
          ) ~
            SOURCE_MATCH_WCVP_ID,
          
          is.na(
            SOURCE_MATCH_WCVP_ID
          ) &
            !SOURCE_NAME_AMBIGUOUS &
            !is.na(
              RECOGNISED_MATCH_WCVP_ID
            ) ~
            RECOGNISED_MATCH_WCVP_ID,
          
          TRUE ~
            NA_character_
        ),
      
      RECONCILIATION_METHOD =
        case_when(
          !is.na(
            SOURCE_MATCH_WCVP_ID
          ) ~
            "exact_name_reuse_frozen_04p",
          
          is.na(
            SOURCE_MATCH_WCVP_ID
          ) &
            !SOURCE_NAME_AMBIGUOUS &
            !is.na(
              RECOGNISED_MATCH_WCVP_ID
            ) ~
            "exact_recognised_wcvp_name",
          
          SOURCE_NAME_AMBIGUOUS ~
            "ambiguous_existing_04p_name",
          
          TRUE ~
            "unresolved"
        )
    )
  
  # ---------------------------------------------------------------------------
  # Attach authoritative recognised concept attributes
  # ---------------------------------------------------------------------------
  
  reconciliation <- reconciliation %>%
    left_join(
      recognised_lookup %>%
        select(
          FINAL_WCVP_ID = WCVP_ID,
          FINAL_WCVP_RECOGNISED_NAME =
            WCVP_RECOGNISED_NAME,
          FINAL_WCVP_RANK =
            WCVP_RANK,
          FINAL_WCVP_STATUS =
            WCVP_STATUS,
          FINAL_VPJD_TAXONOMIC_STATUS =
            VPJD_TAXONOMIC_STATUS,
          FINAL_WCVP_CONCEPT_CLASS =
            WCVP_CONCEPT_CLASS
        ),
      by = "FINAL_WCVP_ID"
    ) %>%
    mutate(
      RECONCILIATION_STATUS =
        case_when(
          is.na(FINAL_WCVP_ID) ~
            "unresolved",
          
          FINAL_WCVP_CONCEPT_CLASS ==
            "accepted_wcvp" ~
            "accepted_wcvp",
          
          FINAL_WCVP_CONCEPT_CLASS ==
            "artificial_hybrid_wcvp" ~
            "artificial_hybrid_wcvp",
          
          FINAL_WCVP_CONCEPT_CLASS ==
            "unplaced_wcvp" ~
            "unplaced_wcvp",
          
          TRUE ~
            "recognised_wcvp"
        ),
      
      WCVP_CONCEPT_ALREADY_IN_NATIONAL_VPJD =
        !is.na(FINAL_WCVP_ID) &
        FINAL_WCVP_ID %in%
        recognised_lookup$WCVP_ID
    )
  
  # ---------------------------------------------------------------------------
  # Safety check: one row per recovered name
  # ---------------------------------------------------------------------------
  
  if (
    nrow(reconciliation) !=
    nrow(recovered_names)
  ) {
    stop(
      "Reconciliation changed the recovered scientific-name row count."
    )
  }
  
  if (
    anyDuplicated(
      reconciliation$RECOVERED_NAME_KEY
    ) > 0
  ) {
    stop(
      "Reconciliation contains duplicate RECOVERED_NAME_KEY values."
    )
  }
  
  # ---------------------------------------------------------------------------
  # Expand decisions back to individual occurrences
  # ---------------------------------------------------------------------------
  
  reconciled_occurrences <- recovered %>%
    left_join(
      reconciliation %>%
        select(
          RECOVERED_NAME_KEY,
          FINAL_WCVP_ID,
          FINAL_WCVP_RECOGNISED_NAME,
          FINAL_WCVP_RANK,
          FINAL_WCVP_STATUS,
          FINAL_VPJD_TAXONOMIC_STATUS,
          FINAL_WCVP_CONCEPT_CLASS,
          RECONCILIATION_METHOD,
          RECONCILIATION_STATUS,
          WCVP_CONCEPT_ALREADY_IN_NATIONAL_VPJD
        ),
      by = "RECOVERED_NAME_KEY"
    )
  
  if (
    nrow(reconciled_occurrences) !=
    EXPECTED_RECOVERED_RECORDS
  ) {
    stop(
      "Occurrence reconciliation changed recovered record count."
    )
  }
  
  if (
    n_distinct(
      reconciled_occurrences$gbifID
    ) !=
    EXPECTED_RECOVERED_RECORDS
  ) {
    stop(
      "Occurrence reconciliation changed distinct gbifID count."
    )
  }
  
  # ---------------------------------------------------------------------------
  # Summaries
  # ---------------------------------------------------------------------------
  
  reconciliation_summary <- reconciliation %>%
    group_by(
      RECONCILIATION_STATUS,
      RECONCILIATION_METHOD
    ) %>%
    summarise(
      scientific_names = n(),
      occurrence_records =
        sum(OCCURRENCE_RECORDS),
      distinct_wcvp_ids =
        n_distinct(
          FINAL_WCVP_ID,
          na.rm = TRUE
        ),
      .groups = "drop"
    ) %>%
    mutate(
      pct_names =
        100 *
        scientific_names /
        sum(scientific_names),
      pct_records =
        100 *
        occurrence_records /
        sum(occurrence_records)
    ) %>%
    arrange(
      desc(occurrence_records)
    )
  
  status_summary <- reconciliation %>%
    group_by(
      RECONCILIATION_STATUS
    ) %>%
    summarise(
      scientific_names = n(),
      occurrence_records =
        sum(OCCURRENCE_RECORDS),
      distinct_wcvp_ids =
        n_distinct(
          FINAL_WCVP_ID,
          na.rm = TRUE
        ),
      .groups = "drop"
    ) %>%
    mutate(
      pct_names =
        100 *
        scientific_names /
        sum(scientific_names),
      pct_records =
        100 *
        occurrence_records /
        sum(occurrence_records)
    ) %>%
    arrange(
      desc(occurrence_records)
    )
  
  national_concept_summary <- reconciliation %>%
    mutate(
      NATIONAL_CONCEPT_STATUS =
        case_when(
          is.na(FINAL_WCVP_ID) ~
            "unresolved",
          
          WCVP_CONCEPT_ALREADY_IN_NATIONAL_VPJD ~
            "already_in_national_vpjd",
          
          TRUE ~
            "new_wcvp_concept_to_national_vpjd"
        )
    ) %>%
    group_by(
      NATIONAL_CONCEPT_STATUS
    ) %>%
    summarise(
      scientific_names = n(),
      occurrence_records =
        sum(OCCURRENCE_RECORDS),
      distinct_wcvp_ids =
        n_distinct(
          FINAL_WCVP_ID,
          na.rm = TRUE
        ),
      .groups = "drop"
    ) %>%
    arrange(
      desc(occurrence_records)
    )
  
  unresolved <- reconciliation %>%
    filter(
      RECONCILIATION_STATUS ==
        "unresolved"
    ) %>%
    arrange(
      desc(OCCURRENCE_RECORDS),
      RECOVERED_SCIENTIFIC_NAME
    )
  
  resolved <- reconciliation %>%
    filter(
      RECONCILIATION_STATUS !=
        "unresolved"
    ) %>%
    arrange(
      desc(OCCURRENCE_RECORDS),
      RECOVERED_SCIENTIFIC_NAME
    )
  
  # ---------------------------------------------------------------------------
  # Accounting
  # ---------------------------------------------------------------------------
  
  if (
    sum(
      reconciliation$OCCURRENCE_RECORDS
    ) !=
    EXPECTED_RECOVERED_RECORDS
  ) {
    stop(
      "Reconciliation does not account for all recovered records."
    )
  }
  
  if (
    sum(
      status_summary$occurrence_records
    ) !=
    EXPECTED_RECOVERED_RECORDS
  ) {
    stop(
      "Status summary does not account for all recovered records."
    )
  }
  
  resolved_names <- sum(
    !is.na(
      reconciliation$FINAL_WCVP_ID
    )
  )
  
  unresolved_names <-
    nrow(reconciliation) -
    resolved_names
  
  resolved_records <- sum(
    reconciliation$OCCURRENCE_RECORDS[
      !is.na(
        reconciliation$FINAL_WCVP_ID
      )
    ]
  )
  
  unresolved_records <-
    EXPECTED_RECOVERED_RECORDS -
    resolved_records
  
  distinct_resolved_wcvp_ids <- n_distinct(
    reconciliation$FINAL_WCVP_ID,
    na.rm = TRUE
  )
  
  new_wcvp_ids <- reconciliation %>%
    filter(
      !is.na(FINAL_WCVP_ID),
      !WCVP_CONCEPT_ALREADY_IN_NATIONAL_VPJD
    ) %>%
    distinct(FINAL_WCVP_ID)
  
  # ---------------------------------------------------------------------------
  # Metadata
  # ---------------------------------------------------------------------------
  
  metadata <- tibble(
    metric = c(
      "script_version",
      "input_integration_01_version",
      "input_taxonomy_04p_version",
      "recovered_occurrence_records",
      "distinct_recovered_scientific_names",
      "resolved_scientific_names",
      "unresolved_scientific_names",
      "resolved_occurrence_records",
      "unresolved_occurrence_records",
      "distinct_resolved_wcvp_ids",
      "new_wcvp_ids_to_national_vpjd",
      "fuzzy_matching_used",
      "taxonomic_inference_used",
      "records_merged_into_occurrences_clean",
      "frozen_taxonomy_tables_modified",
      "upstream_integration_tables_modified"
    ),
    value = c(
      SCRIPT_VERSION,
      integration_version,
      taxonomy_version,
      as.character(
        EXPECTED_RECOVERED_RECORDS
      ),
      as.character(
        nrow(reconciliation)
      ),
      as.character(
        resolved_names
      ),
      as.character(
        unresolved_names
      ),
      as.character(
        resolved_records
      ),
      as.character(
        unresolved_records
      ),
      as.character(
        distinct_resolved_wcvp_ids
      ),
      as.character(
        nrow(new_wcvp_ids)
      ),
      "FALSE",
      "FALSE",
      "0",
      "FALSE",
      "FALSE"
    )
  )
  
  # ---------------------------------------------------------------------------
  # Persist Integration 02 outputs
  # ---------------------------------------------------------------------------
  
  dbWriteTable(
    con,
    "recovered_institutional_taxon_reconciliation",
    reconciliation,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "recovered_institutional_occurrences_taxonomised",
    reconciled_occurrences,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "recovered_institutional_taxon_reconciliation_summary",
    reconciliation_summary,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "recovered_institutional_taxon_status_summary",
    status_summary,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "recovered_institutional_taxon_national_contribution",
    national_concept_summary,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "recovered_institutional_taxon_unresolved",
    unresolved,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "recovered_institutional_taxon_integration_metadata",
    metadata,
    overwrite = TRUE
  )
  
  # ---------------------------------------------------------------------------
  # CSV diagnostics
  # ---------------------------------------------------------------------------
  
  out_dir <- here(
    "outputs",
    "tables",
    "integration",
    "recovered_institutional_taxonomy"
  )
  
  dir.create(
    out_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  write_csv(
    reconciliation_summary,
    file.path(
      out_dir,
      "reconciliation_summary.csv"
    )
  )
  
  write_csv(
    status_summary,
    file.path(
      out_dir,
      "status_summary.csv"
    )
  )
  
  write_csv(
    national_concept_summary,
    file.path(
      out_dir,
      "national_concept_summary.csv"
    )
  )
  
  write_csv(
    unresolved,
    file.path(
      out_dir,
      "unresolved_scientific_names.csv"
    )
  )
  
  write_csv(
    resolved,
    file.path(
      out_dir,
      "resolved_scientific_names.csv"
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
    "\n— Reconciliation method summary —\n\n"
  )
  
  print(
    reconciliation_summary,
    n = Inf
  )
  
  cat(
    "\n— Final taxonomic-status summary —\n\n"
  )
  
  print(
    status_summary,
    n = Inf
  )
  
  cat(
    "\n— Contribution relative to frozen national VPJD taxonomy —\n\n"
  )
  
  print(
    national_concept_summary,
    n = Inf
  )
  
  cat(
    "\n— Largest unresolved recovered names —\n\n"
  )
  
  if (nrow(unresolved) == 0) {
    cat("None.\n")
  } else {
    print(
      unresolved %>%
        select(
          RECOVERED_SCIENTIFIC_NAME,
          RECOVERED_TAXON_RANK,
          OCCURRENCE_RECORDS,
          RECONCILIATION_METHOD
        ) %>%
        head(40),
      n = 40
    )
  }
  
  cat(
    "\n— Accounting —\n"
  )
  
  cat(
    "Recovered occurrence records: ",
    format(
      EXPECTED_RECOVERED_RECORDS,
      big.mark = ","
    ),
    "\n", sep = ""
  )
  
  cat(
    "Distinct recovered scientific names: ",
    format(
      nrow(reconciliation),
      big.mark = ","
    ),
    "\n", sep = ""
  )
  
  cat(
    "Resolved scientific names: ",
    format(
      resolved_names,
      big.mark = ","
    ),
    "\n", sep = ""
  )
  
  cat(
    "Unresolved scientific names: ",
    format(
      unresolved_names,
      big.mark = ","
    ),
    "\n", sep = ""
  )
  
  cat(
    "Resolved occurrence records: ",
    format(
      resolved_records,
      big.mark = ","
    ),
    "\n", sep = ""
  )
  
  cat(
    "Unresolved occurrence records: ",
    format(
      unresolved_records,
      big.mark = ","
    ),
    "\n", sep = ""
  )
  
  cat(
    "Distinct resolved WCVP IDs: ",
    format(
      distinct_resolved_wcvp_ids,
      big.mark = ","
    ),
    "\n", sep = ""
  )
  
  cat(
    "WCVP IDs new to frozen national VPJD: ",
    format(
      nrow(new_wcvp_ids),
      big.mark = ","
    ),
    "\n", sep = ""
  )
  
  cat(
    "\n— Safety —\n"
  )
  
  cat(
    "Fuzzy/edit-distance matching used: FALSE\n"
  )
  
  cat(
    "Higher-rank taxonomic inference used: FALSE\n"
  )
  
  cat(
    "New taxonomic opinions introduced: FALSE\n"
  )
  
  cat(
    "Records merged into occurrences_clean: 0\n"
  )
  
  cat(
    "Frozen taxonomy tables modified: FALSE\n"
  )
  
  cat(
    "Frozen Integration 01 tables modified: FALSE\n"
  )
  
  cat(
    "Output status: TAXONOMIC RECONCILIATION LAYER ONLY\n"
  )
  
  cat(
    "\n02_reconcile_recovered_taxa.R v",
    SCRIPT_VERSION,
    " complete.\n",
    sep = ""
  )
  
  invisible(
    list(
      reconciliation =
        reconciliation,
      reconciled_occurrences =
        reconciled_occurrences,
      reconciliation_summary =
        reconciliation_summary,
      status_summary =
        status_summary,
      national_concept_summary =
        national_concept_summary,
      unresolved =
        unresolved,
      resolved =
        resolved,
      metadata =
        metadata
    )
  )
}

integration_02 <-
  run_recovered_taxon_reconciliation()

integration_02_reconciliation <-
  integration_02$reconciliation

integration_02_occurrences <-
  integration_02$reconciled_occurrences

integration_02_summary <-
  integration_02$reconciliation_summary

integration_02_status_summary <-
  integration_02$status_summary

integration_02_national_contribution <-
  integration_02$national_concept_summary

integration_02_unresolved <-
  integration_02$unresolved