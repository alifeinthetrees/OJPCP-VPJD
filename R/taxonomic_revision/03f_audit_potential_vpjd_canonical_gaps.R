# VPJD TAXONOMIC REVISION - 03f_audit_potential_vpjd_canonical_gaps.R
# Version 0.3.0. Complete replacement based on the supplied 0.1.0 script.
# Kew/WCVP governs taxonomy. Japanese checklists supply crosswalk and flora evidence.
# Audit only: no taxa are added/removed and no canonical or Star fields are changed.
# Historical counts are checked, never manufactured by changing classifications.
# Corrected baseline: the former three infraspecific flags matched the author L.f.;
# six accepted WCVP concepts actually have infraspecific rank. ID 503873 is Unplaced
# in the local WCVP index and has no accepted-name lookup entry. Never fabricate
# its accepted name with paste(NA, NA), which produces the nonmissing string "NA NA".
# Run normally with source(...). For a read-only check:
# options(vpjd.03f.autorun = FALSE); source(...); run_03f(write_outputs = FALSE)
# Requires the project's existing R packages; this file installs nothing.

run_03f <- function(
    project_root = "I:/R/OJPCP/VPJD-OJPCP",
    db_path = file.path(project_root, "data/interim/occurrences/vpjd_occurrences.duckdb"),
    output_dir = file.path(project_root, "outputs/tables/taxonomic_revision/03f_audit_potential_vpjd_canonical_gaps"),
    write_outputs = TRUE,
    strict_historical = FALSE) {
  
  library(DBI)
  library(duckdb)
  library(dplyr)
  library(stringr)
  library(tibble)
  library(readr)
  library(tidyr)
  library(purrr)
  
  VERSION <- "0.3.0"
  MODULE <- "03f"
  PROJECT_ROOT <- project_root
  DB_PATH <- db_path
  OUTPUT_DIR <- output_dir
  
  EXPECTED_CANONICAL_POPULATION <- 11439L
  
  EXPECTED_JAPAN_CHECKLIST_RECORDS <- 9821L
  
  EXPECTED_03E_REVIEW <- 4280L
  
  EXPECTED_WCVP_NOT_VPJD <- 290L
  
  EXPECTED_POTENTIAL_GAPS <- 201L
  
  CANONICAL_TABLE <-
    "vpjd_star_provisional_wholesale_allocation"
  
  LINEAGE_TABLE <-
    "vpjd_taxrev_02h_validated_vascular_lineage"
  
  TABLE_03E_AUDIT <-
    "vpjd_taxrev_03e_gap_audit"
  
  TABLE_03E_WCVP_NOT_VPJD <-
    "vpjd_taxrev_03e_wcvp_not_vpjd_audit"
  
  TABLE_03E_POTENTIAL_GAPS <-
    "vpjd_taxrev_03e_potential_canonical_gaps"
  
  GREEN_CORE_TABLE <-
    "vpjd_taxrev_03a_greenlist_core"
  
  FERN_CORE_TABLE <-
    "vpjd_taxrev_03a_fern_greenlist_core"
  
  GREEN_DISTRIBUTION_TABLE <-
    "vpjd_taxrev_03a_greenlist_distribution_2"
  
  GREEN_VERNACULAR_TABLE <-
    "vpjd_taxrev_03a_greenlist_vernacularname_1"
  
  FERN_VERNACULAR_TABLE <-
    "vpjd_taxrev_03a_fern_greenlist_vernacularname_2"
  
  FERN_SPECIES_PROFILE_TABLE <-
    "vpjd_taxrev_03a_fern_greenlist_speciesprofile_1"
  
  WCVP_MATCH_INDEX_TABLE <-
    "wcvp_occurrence_match_index"
  
  WCVP_ACCEPTED_LOOKUP_TABLE <-
    "wcvp_occurrence_accepted_lookup"
  
  OUTPUT_AUDIT_TABLE <-
    "vpjd_taxrev_03f_potential_gap_audit"
  
  OUTPUT_PROBABLE_GAP_TABLE <-
    "vpjd_taxrev_03f_probable_canonical_gaps"
  
  OUTPUT_INTRODUCED_TABLE <-
    "vpjd_taxrev_03f_introduced_scope_review"
  
  OUTPUT_CONCEPT_REVIEW_TABLE <-
    "vpjd_taxrev_03f_related_concept_review"
  
  OUTPUT_UNRESOLVED_TABLE <-
    "vpjd_taxrev_03f_further_review"
  
  OUTPUT_SUMMARY_TABLE <-
    "vpjd_taxrev_03f_summary"
  
  OUTPUT_SOURCE_SUMMARY_TABLE <-
    "vpjd_taxrev_03f_source_summary"
  
  OUTPUT_VALIDATION_TABLE <-
    "vpjd_taxrev_03f_validation"
  
  OUTPUT_METADATA_TABLE <-
    "vpjd_taxrev_03f_metadata"
  
  OUTPUT_AUDIT_CSV <-
    file.path(
      OUTPUT_DIR,
      "vpjd_03f_potential_gap_audit.csv"
    )
  
  OUTPUT_PROBABLE_GAP_CSV <-
    file.path(
      OUTPUT_DIR,
      "vpjd_03f_probable_canonical_gaps.csv"
    )
  
  OUTPUT_INTRODUCED_CSV <-
    file.path(
      OUTPUT_DIR,
      "vpjd_03f_introduced_scope_review.csv"
    )
  
  OUTPUT_CONCEPT_REVIEW_CSV <-
    file.path(
      OUTPUT_DIR,
      "vpjd_03f_related_concept_review.csv"
    )
  
  OUTPUT_UNRESOLVED_CSV <-
    file.path(
      OUTPUT_DIR,
      "vpjd_03f_further_review.csv"
    )
  
  OUTPUT_SUMMARY_CSV <-
    file.path(
      OUTPUT_DIR,
      "vpjd_03f_summary.csv"
    )
  
  OUTPUT_SOURCE_SUMMARY_CSV <-
    file.path(
      OUTPUT_DIR,
      "vpjd_03f_source_summary.csv"
    )
  
  OUTPUT_VALIDATION_CSV <-
    file.path(
      OUTPUT_DIR,
      "vpjd_03f_validation.csv"
    )
  
  OUTPUT_METADATA_CSV <-
    file.path(
      OUTPUT_DIR,
      "vpjd_03f_metadata.csv"
    )
  
  normalise_text <- function(x) {
    
    x <-
      as.character(x)
    
    x <-
      stringr::str_squish(x)
    
    x[
      x == ""
    ] <-
      NA_character_
    
    x
  }
  
  normalise_name <- function(x) {
    
    x <-
      normalise_text(x)
    
    x <-
      stringr::str_replace_all(
        x,
        "\u00d7",
        "x"
      )
    
    x <-
      stringr::str_replace_all(
        x,
        "\u2715",
        "x"
      )
    
    x <-
      stringr::str_replace_all(
        x,
        "\u00a0",
        " "
      )
    
    x <-
      stringr::str_squish(x)
    
    x <-
      stringr::str_to_lower(x)
    
    x
  }
  
  extract_binomial <- function(x) {
    
    x <-
      normalise_text(x)
    
    ifelse(
      is.na(x),
      NA_character_,
      stringr::str_extract(
        x,
        "^[A-Z][[:alpha:]-]+\\s+[[:lower:]][[:alpha:]-]+"
      )
    )
  }
  
  extract_genus <- function(x) {
    
    x <-
      normalise_text(x)
    
    ifelse(
      is.na(x),
      NA_character_,
      stringr::word(
        x,
        1L
      )
    )
  }
  
  first_existing_field <- function(
    data,
    candidates
  ) {
    
    hit <-
      candidates[
        candidates %in%
          names(data)
      ]
    
    if (
      length(hit) == 0L
    ) {
      
      return(
        NA_character_
      )
    }
    
    hit[[1L]]
  }
  
  safe_field <- function(
    data,
    field
  ) {
    
    if (
      length(field) == 0L ||
      is.na(field) ||
      !field %in% names(data)
    ) {
      
      return(
        rep(
          NA_character_,
          nrow(data)
        )
      )
    }
    
    normalise_text(
      data[[field]]
    )
  }
  
  safe_logical_field <- function(
    data,
    field
  ) {
    
    if (
      length(field) == 0L ||
      is.na(field) ||
      !field %in% names(data)
    ) {
      
      return(
        rep(
          NA,
          nrow(data)
        )
      )
    }
    
    x <-
      data[[field]]
    
    if (
      is.logical(x)
    ) {
      
      return(x)
    }
    
    x <-
      toupper(
        normalise_text(x)
      )
    
    case_when(
      x %in%
        c(
          "TRUE",
          "T",
          "YES",
          "Y",
          "1"
        ) ~ TRUE,
      
      x %in%
        c(
          "FALSE",
          "F",
          "NO",
          "N",
          "0"
        ) ~ FALSE,
      
      TRUE ~ NA
    )
  }
  
  collapse_unique <- function(x) {
    
    x <-
      normalise_text(x)
    
    x <-
      sort(
        unique(
          x[
            !is.na(x)
          ]
        )
      )
    
    if (
      length(x) == 0L
    ) {
      
      return(
        NA_character_
      )
    }
    
    paste(
      x,
      collapse = " | "
    )
  }
  
  # Helpers: every preview is bounded and uses the base data-frame print method.
  preview <- function(x, rows = 20L) {
    print(as.data.frame(utils::head(x, rows)), row.names = FALSE, na.print = "NA")
    if (nrow(x) > rows) cat("Showing", rows, "of", nrow(x), "rows; full table retained in outputs.\n")
  }
  require_fields <- function(x, fields, label) {
    missing <- setdiff(fields, names(x))
    if (length(missing)) stop(label, " missing fields: ", paste(missing, collapse = ", "))
  }
  normalise_id <- function(x) normalise_text(x)
  scientific_name <- function(name, authors) {
    name <- normalise_text(name)
    authors <- normalise_text(authors)
    ifelse(is.na(name), NA_character_, ifelse(is.na(authors), name, paste(name, authors)))
  }
  one_value <- function(x) {
    x <- unique(normalise_text(x)); x <- x[!is.na(x)]
    if (length(x) > 1L) stop("Conflicting WCVP metadata for one accepted ID; inspect support tables.")
    if (length(x)) x[[1L]] else NA_character_
  }
  if (!file.exists(DB_PATH)) stop("Database not found: ", DB_PATH)
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = DB_PATH, read_only = !write_outputs)
  on.exit(try(DBI::dbDisconnect(con, shutdown = TRUE), silent = TRUE), add = TRUE)
  disconnect_safely <- function() invisible(NULL) # on.exit owns connection cleanup.
  
  database_tables <-
    DBI::dbListTables(
      con
    )
  
  required_tables <-
    c(
      CANONICAL_TABLE,
      LINEAGE_TABLE,
      TABLE_03E_AUDIT,
      TABLE_03E_WCVP_NOT_VPJD,
      TABLE_03E_POTENTIAL_GAPS,
      GREEN_CORE_TABLE,
      FERN_CORE_TABLE,
      WCVP_MATCH_INDEX_TABLE,
      "occurrence_wcvp_accepted_taxa",
      "occurrence_wcvp_recognised_taxa",
      WCVP_ACCEPTED_LOOKUP_TABLE
    )
  
  missing_tables <-
    setdiff(
      required_tables,
      database_tables
    )
  
  if (
    length(
      missing_tables
    ) > 0L
  ) {
    
    disconnect_safely()
    
    stop(
      paste0(
        "Required table(s) missing:\n",
        paste(
          missing_tables,
          collapse = "\n"
        )
      )
    )
  }
  
  green_distribution_available <-
    GREEN_DISTRIBUTION_TABLE %in%
    database_tables
  
  green_vernacular_available <-
    GREEN_VERNACULAR_TABLE %in%
    database_tables
  
  fern_vernacular_available <-
    FERN_VERNACULAR_TABLE %in%
    database_tables
  
  fern_profile_available <-
    FERN_SPECIES_PROFILE_TABLE %in%
    database_tables
  
  canonical <-
    DBI::dbReadTable(
      con,
      CANONICAL_TABLE
    ) |>
    tibble::as_tibble()
  
  lineage <-
    DBI::dbReadTable(
      con,
      LINEAGE_TABLE
    ) |>
    tibble::as_tibble()
  
  audit_03e <-
    DBI::dbReadTable(
      con,
      TABLE_03E_AUDIT
    ) |>
    tibble::as_tibble()
  
  wcvp_not_vpjd_03e <-
    DBI::dbReadTable(
      con,
      TABLE_03E_WCVP_NOT_VPJD
    ) |>
    tibble::as_tibble()
  
  potential_gaps_03e <-
    DBI::dbReadTable(
      con,
      TABLE_03E_POTENTIAL_GAPS
    ) |>
    tibble::as_tibble()
  
  green_core <-
    DBI::dbReadTable(
      con,
      GREEN_CORE_TABLE
    ) |>
    tibble::as_tibble()
  
  fern_core <-
    DBI::dbReadTable(
      con,
      FERN_CORE_TABLE
    ) |>
    tibble::as_tibble()
  
  wcvp_accepted <-
    DBI::dbReadTable(
      con,
      WCVP_ACCEPTED_LOOKUP_TABLE
    ) |>
    tibble::as_tibble()
  
  green_distribution <-
    if (
      green_distribution_available
    ) {
      
      DBI::dbReadTable(
        con,
        GREEN_DISTRIBUTION_TABLE
      ) |>
        tibble::as_tibble()
      
    } else {
      
      tibble()
      
    }
  
  green_vernacular <-
    if (
      green_vernacular_available
    ) {
      
      DBI::dbReadTable(
        con,
        GREEN_VERNACULAR_TABLE
      ) |>
        tibble::as_tibble()
      
    } else {
      
      tibble()
      
    }
  
  fern_vernacular <-
    if (
      fern_vernacular_available
    ) {
      
      DBI::dbReadTable(
        con,
        FERN_VERNACULAR_TABLE
      ) |>
        tibble::as_tibble()
      
    } else {
      
      tibble()
      
    }
  
  fern_profile <-
    if (
      fern_profile_available
    ) {
      
      DBI::dbReadTable(
        con,
        FERN_SPECIES_PROFILE_TABLE
      ) |>
        tibble::as_tibble()
      
    } else {
      
      tibble()
      
    }
  
  if (
    nrow(canonical) !=
    EXPECTED_CANONICAL_POPULATION
  ) {
    
    disconnect_safely()
    
    stop(
      paste0(
        "Canonical population invariant failed.\n",
        "Expected: ",
        EXPECTED_CANONICAL_POPULATION,
        "\nFound: ",
        nrow(canonical)
      )
    )
  }
  
  if (
    nrow(audit_03e) !=
    EXPECTED_03E_REVIEW
  ) {
    
    disconnect_safely()
    
    stop(
      paste0(
        "03e audit population invariant failed.\n",
        "Expected: ",
        EXPECTED_03E_REVIEW,
        "\nFound: ",
        nrow(audit_03e)
      )
    )
  }
  
  if (
    nrow(wcvp_not_vpjd_03e) !=
    EXPECTED_WCVP_NOT_VPJD
  ) {
    
    disconnect_safely()
    
    stop(
      paste0(
        "03e WCVP-not-VPJD invariant failed.\n",
        "Expected: ",
        EXPECTED_WCVP_NOT_VPJD,
        "\nFound: ",
        nrow(wcvp_not_vpjd_03e)
      )
    )
  }
  
  if (
    nrow(potential_gaps_03e) !=
    EXPECTED_POTENTIAL_GAPS
  ) {
    
    disconnect_safely()
    
    stop(
      paste0(
        "03e potential-gap invariant failed.\n",
        "Expected: ",
        EXPECTED_POTENTIAL_GAPS,
        "\nFound: ",
        nrow(potential_gaps_03e)
      )
    )
  }
  
  if (nrow(green_core) + nrow(fern_core) != EXPECTED_JAPAN_CHECKLIST_RECORDS)
    stop("Japanese checklist population is not 9,821.")
  require_fields(potential_gaps_03e, c("D03_ACCEPTED_WCVP_ID",
                                       "D03_ACCEPTED_WCVP_SCIENTIFIC_NAME", "D03_ACCEPTED_WCVP_TAXON_NAME",
                                       "D03_ACCEPTED_WCVP_AUTHORSHIP", "D03_ACCEPTED_WCVP_RANK",
                                       "D03_WCVP_MATCHED_NAME", "D03_WCVP_MATCHED_STATUS"), "03e input")
  if (!all(toupper(trimws(potential_gaps_03e$E03_WCVP_GAP_CLASS)) == "POTENTIAL_CANONICAL_GAP"))
    stop("03e potential-gap table contains other classes or missing classes.")
  
  required_gap_fields <-
    c(
      "SOURCE_03C_ROW",
      "JAPAN_RECORD_ID",
      "SOURCE",
      "SOURCE_TAXON_ID",
      "JAPAN_SCIENTIFIC_NAME",
      "JAPANESE_NAME",
      "JAPAN_RANK",
      "JAPAN_FAMILY",
      "JAPAN_GENUS",
      "SOURCE_TAXONOMIC_STATUS",
      "E03_BINOMIAL",
      "E03_DERIVED_GENUS",
      "E03_WCVP_GAP_CLASS"
    )
  
  missing_gap_fields <-
    setdiff(
      required_gap_fields,
      names(potential_gaps_03e)
    )
  
  if (
    length(
      missing_gap_fields
    ) > 0L
  ) {
    
    disconnect_safely()
    
    stop(
      paste0(
        "Required 03e potential-gap field(s) missing:\n",
        paste(
          missing_gap_fields,
          collapse = "\n"
        ),
        "\n\nAvailable fields:\n",
        paste(
          names(potential_gaps_03e),
          collapse = "\n"
        )
      )
    )
  }
  
  # Accepted-concept support. No name-based fallback to Japanese taxonomy.
  require_fields(wcvp_accepted, c("accepted_wcvp_plant_name_id", "accepted_wcvp_taxon_name",
                                  "accepted_wcvp_taxon_authors", "accepted_wcvp_taxon_rank", "accepted_wcvp_scientific_name"), "WCVP lookup")
  support_fields <- c("wcvp_plant_name_id", "wcvp_taxon_name", "wcvp_taxon_authors",
                      "wcvp_taxon_rank", "wcvp_taxon_status", "wcvp_family", "wcvp_genus", "wcvp_species")
  make_support <- function(table, prefix) {
    x <- DBI::dbReadTable(con, table)
    require_fields(x, support_fields, table)
    x <- x[, support_fields]
    names(x) <- c("ID", "TAXON", "AUTHOR", "RANK", "STATUS", "FAMILY", "GENUS", "SPECIES")
    x <- x |> mutate(across(everything(), normalise_text)) |>
      filter(!is.na(ID), ID %in% normalise_id(potential_gaps_03e$D03_ACCEPTED_WCVP_ID)) |>
      group_by(ID) |> summarise(across(everything(), one_value), .groups = "drop")
    names(x)[-1L] <- paste0(prefix, names(x)[-1L])
    x
  }
  accepted_support <- make_support("occurrence_wcvp_accepted_taxa", "A_")
  recognised_support <- make_support("occurrence_wcvp_recognised_taxa", "R_")
  # Recognised tables can include synonyms; only accepted records describe the accepted ID.
  recognised_support <- recognised_support |> filter(tolower(R_STATUS) == "accepted")
  # Check the identity/status of every upstream accepted ID against WCVP itself.
  # An upstream accepted-ID field is provenance, not proof that WCVP accepts it.
  id_literals <- DBI::dbQuoteString(con, unique(normalise_id(potential_gaps_03e$D03_ACCEPTED_WCVP_ID)))
  index_support <- DBI::dbGetQuery(con, paste0(
    "SELECT wcvp_plant_name_id, wcvp_taxon_name, wcvp_taxon_authors, ",
    "wcvp_taxon_rank, wcvp_taxon_status, wcvp_scientific_name FROM ",
    DBI::dbQuoteIdentifier(con, WCVP_MATCH_INDEX_TABLE),
    " WHERE CAST(wcvp_plant_name_id AS VARCHAR) IN (", paste(id_literals, collapse = ","), ")")) |>
    transmute(ID = normalise_id(wcvp_plant_name_id), I_TAXON = normalise_text(wcvp_taxon_name),
              I_AUTHOR = normalise_text(wcvp_taxon_authors), I_RANK = normalise_text(wcvp_taxon_rank),
              I_STATUS = normalise_text(wcvp_taxon_status), I_NAME = normalise_text(wcvp_scientific_name)) |>
    group_by(ID) |> summarise(across(everything(), one_value), .groups = "drop")
  lookup <- wcvp_accepted |> transmute(
    ID = normalise_id(accepted_wcvp_plant_name_id),
    L_TAXON = normalise_text(accepted_wcvp_taxon_name),
    L_AUTHOR = normalise_text(accepted_wcvp_taxon_authors),
    L_RANK = normalise_text(accepted_wcvp_taxon_rank),
    L_NAME = normalise_text(accepted_wcvp_scientific_name)) |>
    filter(!is.na(ID), ID %in% normalise_id(potential_gaps_03e$D03_ACCEPTED_WCVP_ID)) |>
    group_by(ID) |> summarise(across(everything(), one_value), .groups = "drop")
  audit <- potential_gaps_03e |> mutate(
    F03_ROW = row_number(), AUDIT_03F_ROW = row_number(),
    F03_UPSTREAM_GENERIC_WCVP_ID = safe_field(potential_gaps_03e, "WCVP_ID"),
    F03_UPSTREAM_WCVP_ACCEPTED_ID = normalise_id(D03_ACCEPTED_WCVP_ID),
    F03_UPSTREAM_WCVP_ACCEPTED_NAME = normalise_text(D03_ACCEPTED_WCVP_SCIENTIFIC_NAME),
    F03_UPSTREAM_WCVP_MATCHED_NAME = normalise_text(D03_WCVP_MATCHED_NAME),
    F03_UPSTREAM_WCVP_MATCHED_STATUS = normalise_text(D03_WCVP_MATCHED_STATUS),
    F03_JAPAN_NAME_NORMALISED = normalise_name(JAPAN_SCIENTIFIC_NAME),
    F03_BINOMIAL = coalesce(normalise_text(E03_BINOMIAL), extract_binomial(JAPAN_SCIENTIFIC_NAME)),
    F03_GENUS = coalesce(normalise_text(E03_DERIVED_GENUS), extract_genus(JAPAN_SCIENTIFIC_NAME))) |>
    left_join(lookup, by = c("F03_UPSTREAM_WCVP_ACCEPTED_ID" = "ID"), na_matches = "never") |>
    left_join(accepted_support, by = c("F03_UPSTREAM_WCVP_ACCEPTED_ID" = "ID"), na_matches = "never") |>
    left_join(recognised_support, by = c("F03_UPSTREAM_WCVP_ACCEPTED_ID" = "ID"), na_matches = "never") |>
    left_join(index_support, by = c("F03_UPSTREAM_WCVP_ACCEPTED_ID" = "ID"), na_matches = "never") |>
    mutate(
      F03_WCVP_ACCEPTED_ID = F03_UPSTREAM_WCVP_ACCEPTED_ID,
      F03_WCVP_TAXON_NAME = coalesce(L_TAXON, A_TAXON, R_TAXON, normalise_text(D03_ACCEPTED_WCVP_TAXON_NAME)),
      F03_WCVP_AUTHORSHIP = coalesce(L_AUTHOR, A_AUTHOR, R_AUTHOR, normalise_text(D03_ACCEPTED_WCVP_AUTHORSHIP)),
      F03_WCVP_ACCEPTED_NAME = coalesce(L_NAME, F03_UPSTREAM_WCVP_ACCEPTED_NAME,
                                        scientific_name(F03_WCVP_TAXON_NAME, F03_WCVP_AUTHORSHIP)),
      F03_WCVP_RANK = coalesce(L_RANK, A_RANK, R_RANK, normalise_text(D03_ACCEPTED_WCVP_RANK)),
      F03_WCVP_FAMILY = coalesce(A_FAMILY, R_FAMILY),
      F03_WCVP_GENUS = coalesce(A_GENUS, R_GENUS),
      F03_WCVP_SPECIES = coalesce(A_SPECIES, R_SPECIES),
      F03_WCVP_STATUS = coalesce(A_STATUS, R_STATUS, I_STATUS),
      F03_ACCEPTED_ID = F03_WCVP_ACCEPTED_ID,
      F03_ACCEPTED_NAME = F03_WCVP_ACCEPTED_NAME,
      WCVP_ID = F03_WCVP_ACCEPTED_ID, WCVP_ACCEPTED_NAME = F03_WCVP_ACCEPTED_NAME,
      WCVP_RANK = F03_WCVP_RANK, WCVP_FAMILY = F03_WCVP_FAMILY,
      WCVP_GENUS = F03_WCVP_GENUS, WCVP_STATUS = F03_WCVP_STATUS,
      F03_ACCEPTED_BINOMIAL = extract_binomial(F03_ACCEPTED_NAME),
      F03_ACCEPTED_GENUS = coalesce(F03_WCVP_GENUS, extract_genus(F03_ACCEPTED_NAME)),
      F03_ACCEPTED_NAME_NORMALISED = normalise_name(F03_ACCEPTED_NAME),
      F03_WCVP_ID_NAME = I_NAME,
      F03_WCVP_ID_STATUS = I_STATUS,
      F03_WCVP_ACCEPTED_STATUS_CONFLICT = !is.na(I_STATUS) & tolower(I_STATUS) != "accepted",
      F03_WCVP_CONCEPT_RESOLVED = !is.na(WCVP_ID) & !is.na(WCVP_ACCEPTED_NAME) &
        !F03_WCVP_ACCEPTED_STATUS_CONFLICT,
      F03_WCVP_BACKBONE_COMPLETE = F03_WCVP_CONCEPT_RESOLVED & !is.na(WCVP_RANK) &
        !is.na(WCVP_FAMILY) & !is.na(WCVP_GENUS),
      F03_WCVP_METADATA_COMPLETE = F03_WCVP_BACKBONE_COMPLETE & !is.na(WCVP_STATUS),
      F03_WCVP_RESOLUTION_CLASS = case_when(
        F03_WCVP_ACCEPTED_STATUS_CONFLICT ~ "UPSTREAM_ACCEPTED_ID_STATUS_REVIEW",
        !F03_WCVP_CONCEPT_RESOLVED ~ "WCVP_CONCEPT_UNRESOLVED",
        F03_WCVP_METADATA_COMPLETE ~ "ACCEPTED_WCVP_METADATA_COMPLETE",
        TRUE ~ "ACCEPTED_WCVP_METADATA_REVIEW"),
      F03_JAPAN_WCVP_NAME_IDENTICAL = coalesce(F03_JAPAN_NAME_NORMALISED == F03_ACCEPTED_NAME_NORMALISED, FALSE),
      F03_JAPAN_WCVP_NAME_DIFFERENT = !is.na(F03_JAPAN_NAME_NORMALISED) &
        !is.na(F03_ACCEPTED_NAME_NORMALISED) & !F03_JAPAN_WCVP_NAME_IDENTICAL,
      F03_JAPANESE_NAME_IS_WCVP_SYNONYM = tolower(normalise_text(D03_WCVP_MATCHED_STATUS)) %in% c("synonym"),
      F03_JAPANESE_NAME_IS_WCVP_ACCEPTED = tolower(normalise_text(D03_WCVP_MATCHED_STATUS)) %in% c("accepted"),
      F03_LEGACY_JAPANESE_INFRASPECIFIC_FLAG = coalesce(str_detect(tolower(JAPAN_SCIENTIFIC_NAME), "\\b(subsp|ssp|var|f)\\."), FALSE),
      F03_IS_INFRASPECIFIC = tolower(WCVP_RANK) %in% c("subspecies", "variety", "form", "forma", "subvariety", "subforma") |
        coalesce(str_detect(tolower(F03_WCVP_TAXON_NAME), "\\s(subsp|ssp|var|f)\\.\\s"), FALSE),
      F03_IS_HYBRID = coalesce(str_detect(WCVP_ACCEPTED_NAME, "[\u00d7\u2715]|\\bx\\b"), FALSE))
  # Missing family/genus/status never erase an accepted ID/name or create unresolved taxonomy.
  
  canonical_id_field <-
    first_existing_field(
      canonical,
      c(
        "FINAL_WCVP_ID"
      )
    )
  
  canonical_name_field <-
    first_existing_field(
      canonical,
      c(
        "FINAL_WCVP_RECOGNISED_NAME"
      )
    )
  
  canonical_rank_field <-
    first_existing_field(
      canonical,
      c(
        "FINAL_WCVP_RANK"
      )
    )
  
  canonical_family_field <-
    first_existing_field(
      canonical,
      c(
        "FAMILY",
        "CROSSWALK_FAMILY"
      )
    )
  
  canonical_genus_field <-
    first_existing_field(
      canonical,
      c(
        "GENUS"
      )
    )
  
  canonical_species_field <-
    first_existing_field(
      canonical,
      c(
        "SPECIES"
      )
    )
  
  canonical_major_group_field <-
    first_existing_field(
      canonical,
      c(
        "MAJOR_GROUP"
      )
    )
  
  canonical_scope_field <-
    first_existing_field(
      canonical,
      c(
        "VPJD_SCOPE"
      )
    )
  
  canonical_status_field <-
    first_existing_field(
      canonical,
      c(
        "FINAL_WCVP_STATUS"
      )
    )
  
  canonical_introduced_field <-
    first_existing_field(
      canonical,
      c(
        "WCVP_INTRODUCED_TO_JAPAN"
      )
    )
  
  canonical_hybrid_field <-
    first_existing_field(
      canonical,
      c(
        "HYBRID_STATUS"
      )
    )
  
  canonical_inventory <-
    tibble(
      F03_CANONICAL_WCVP_ID =
        safe_field(
          canonical,
          canonical_id_field
        ),
      
      F03_CANONICAL_NAME =
        safe_field(
          canonical,
          canonical_name_field
        ),
      
      F03_CANONICAL_RANK =
        safe_field(
          canonical,
          canonical_rank_field
        ),
      
      F03_CANONICAL_FAMILY =
        safe_field(
          canonical,
          canonical_family_field
        ),
      
      F03_CANONICAL_GENUS =
        safe_field(
          canonical,
          canonical_genus_field
        ),
      
      F03_CANONICAL_SPECIES =
        safe_field(
          canonical,
          canonical_species_field
        ),
      
      F03_CANONICAL_MAJOR_GROUP =
        safe_field(
          canonical,
          canonical_major_group_field
        ),
      
      F03_CANONICAL_SCOPE =
        safe_field(
          canonical,
          canonical_scope_field
        ),
      
      F03_CANONICAL_STATUS =
        safe_field(
          canonical,
          canonical_status_field
        ),
      
      F03_CANONICAL_INTRODUCED =
        safe_field(
          canonical,
          canonical_introduced_field
        ),
      
      F03_CANONICAL_HYBRID_STATUS =
        safe_field(
          canonical,
          canonical_hybrid_field
        )
    ) |>
    mutate(
      F03_CANONICAL_NAME_NORMALISED =
        normalise_name(
          F03_CANONICAL_NAME
        ),
      
      F03_CANONICAL_BINOMIAL =
        extract_binomial(
          F03_CANONICAL_NAME
        ),
      
      F03_CANONICAL_DERIVED_GENUS =
        coalesce(
          normalise_text(
            F03_CANONICAL_GENUS
          ),
          extract_genus(
            F03_CANONICAL_NAME
          )
        )
    )
  
  canonical_ids <-
    unique(
      canonical_inventory$F03_CANONICAL_WCVP_ID[
        !is.na(
          canonical_inventory$F03_CANONICAL_WCVP_ID
        )
      ]
    )
  
  audit <-
    audit |>
    mutate(
      F03_ACCEPTED_ID_PRESENT_IN_VPJD =
        !is.na(
          F03_ACCEPTED_ID
        ) &
        F03_ACCEPTED_ID %in%
        canonical_ids
    )
  
  canonical_genus_profile <-
    canonical_inventory |>
    filter(
      !is.na(
        F03_CANONICAL_DERIVED_GENUS
      )
    ) |>
    group_by(
      F03_CANONICAL_DERIVED_GENUS
    ) |>
    summarise(
      F03_CANONICAL_GENUS_RECORDS =
        n(),
      
      F03_CANONICAL_GENUS_NAMES =
        collapse_unique(
          F03_CANONICAL_NAME
        ),
      
      .groups = "drop"
    )
  
  audit <-
    audit |>
    left_join(
      canonical_genus_profile,
      by =
        c(
          "F03_ACCEPTED_GENUS" =
            "F03_CANONICAL_DERIVED_GENUS"
        )
    )
  
  canonical_binomial_profile <-
    canonical_inventory |>
    filter(
      !is.na(
        F03_CANONICAL_BINOMIAL
      )
    ) |>
    group_by(
      F03_CANONICAL_BINOMIAL
    ) |>
    summarise(
      F03_CANONICAL_BINOMIAL_RECORDS =
        n(),
      
      F03_CANONICAL_BINOMIAL_NAMES =
        collapse_unique(
          F03_CANONICAL_NAME
        ),
      
      F03_CANONICAL_BINOMIAL_IDS =
        collapse_unique(
          F03_CANONICAL_WCVP_ID
        ),
      
      .groups = "drop"
    )
  
  audit <-
    audit |>
    left_join(
      canonical_binomial_profile,
      by =
        c(
          "F03_ACCEPTED_BINOMIAL" =
            "F03_CANONICAL_BINOMIAL"
        )
    )
  
  green_id_field <-
    first_existing_field(
      green_core,
      c(
        "TAXONID",
        "DWCA_ID"
      )
    )
  
  green_name_field <-
    first_existing_field(
      green_core,
      c(
        "SCIENTIFICNAME"
      )
    )
  
  green_family_field <-
    first_existing_field(
      green_core,
      c(
        "FAMILY"
      )
    )
  
  green_rank_field <-
    first_existing_field(
      green_core,
      c(
        "TAXONRANK"
      )
    )
  
  green_vernacular_core_field <-
    first_existing_field(
      green_core,
      c(
        "VERNACULARNAME"
      )
    )
  
  green_status_field <-
    first_existing_field(
      green_core,
      c(
        "TAXONOMICSTATUS",
        "ESTABLISHMENTMEANS",
        "OCCURRENCESTATUS"
      )
    )
  
  green_inventory <-
    tibble(
      F03_GREEN_ID =
        safe_field(
          green_core,
          green_id_field
        ),
      
      F03_GREEN_NAME =
        safe_field(
          green_core,
          green_name_field
        ),
      
      F03_GREEN_FAMILY =
        safe_field(
          green_core,
          green_family_field
        ),
      
      F03_GREEN_RANK =
        safe_field(
          green_core,
          green_rank_field
        ),
      
      F03_GREEN_JAPANESE_NAME =
        safe_field(
          green_core,
          green_vernacular_core_field
        ),
      
      F03_GREEN_STATUS =
        safe_field(
          green_core,
          green_status_field
        )
    ) |>
    mutate(
      F03_GREEN_NAME_NORMALISED =
        normalise_name(
          F03_GREEN_NAME
        )
    )
  
  fern_id_field <-
    first_existing_field(
      fern_core,
      c(
        "TAXONID",
        "DWCA_ID"
      )
    )
  
  fern_name_field <-
    first_existing_field(
      fern_core,
      c(
        "SCIENTIFICNAME"
      )
    )
  
  fern_family_field <-
    first_existing_field(
      fern_core,
      c(
        "FAMILY"
      )
    )
  
  fern_rank_field <-
    first_existing_field(
      fern_core,
      c(
        "TAXONRANK"
      )
    )
  
  fern_vernacular_core_field <-
    first_existing_field(
      fern_core,
      c(
        "VERNACULARNAME"
      )
    )
  
  fern_status_field <-
    first_existing_field(
      fern_core,
      c(
        "TAXONOMICSTATUS",
        "ESTABLISHMENTMEANS",
        "OCCURRENCESTATUS"
      )
    )
  
  fern_inventory <-
    tibble(
      F03_FERN_ID =
        safe_field(
          fern_core,
          fern_id_field
        ),
      
      F03_FERN_NAME =
        safe_field(
          fern_core,
          fern_name_field
        ),
      
      F03_FERN_FAMILY =
        safe_field(
          fern_core,
          fern_family_field
        ),
      
      F03_FERN_RANK =
        safe_field(
          fern_core,
          fern_rank_field
        ),
      
      F03_FERN_JAPANESE_NAME =
        safe_field(
          fern_core,
          fern_vernacular_core_field
        ),
      
      F03_FERN_STATUS =
        safe_field(
          fern_core,
          fern_status_field
        )
    ) |>
    mutate(
      F03_FERN_NAME_NORMALISED =
        normalise_name(
          F03_FERN_NAME
        )
    )
  
  green_name_profile <-
    green_inventory |>
    filter(
      !is.na(
        F03_GREEN_NAME_NORMALISED
      )
    ) |>
    group_by(
      F03_GREEN_NAME_NORMALISED
    ) |>
    summarise(
      F03_GREENLIST_RECORDS =
        n(),
      
      F03_GREENLIST_NAMES =
        collapse_unique(
          F03_GREEN_NAME
        ),
      
      F03_GREENLIST_JAPANESE_NAMES =
        collapse_unique(
          F03_GREEN_JAPANESE_NAME
        ),
      
      F03_GREENLIST_FAMILIES =
        collapse_unique(
          F03_GREEN_FAMILY
        ),
      
      F03_GREENLIST_RANKS =
        collapse_unique(
          F03_GREEN_RANK
        ),
      
      F03_GREENLIST_CORE_STATUS =
        collapse_unique(
          F03_GREEN_STATUS
        ),
      
      .groups = "drop"
    )
  
  fern_name_profile <-
    fern_inventory |>
    filter(
      !is.na(
        F03_FERN_NAME_NORMALISED
      )
    ) |>
    group_by(
      F03_FERN_NAME_NORMALISED
    ) |>
    summarise(
      F03_FERNLIST_RECORDS =
        n(),
      
      F03_FERNLIST_NAMES =
        collapse_unique(
          F03_FERN_NAME
        ),
      
      F03_FERNLIST_JAPANESE_NAMES =
        collapse_unique(
          F03_FERN_JAPANESE_NAME
        ),
      
      F03_FERNLIST_FAMILIES =
        collapse_unique(
          F03_FERN_FAMILY
        ),
      
      F03_FERNLIST_RANKS =
        collapse_unique(
          F03_FERN_RANK
        ),
      
      F03_FERNLIST_CORE_STATUS =
        collapse_unique(
          F03_FERN_STATUS
        ),
      
      .groups = "drop"
    )
  
  if (is.na(canonical_id_field) || is.na(canonical_name_field))
    stop("Canonical WCVP ID/name fields were not identified.")
  # Attach checklist evidence using the ORIGINAL Japanese name, retaining synonyms.
  audit <- audit |>
    left_join(green_name_profile, by = c("F03_JAPAN_NAME_NORMALISED" = "F03_GREEN_NAME_NORMALISED"), na_matches = "never") |>
    left_join(fern_name_profile, by = c("F03_JAPAN_NAME_NORMALISED" = "F03_FERN_NAME_NORMALISED"), na_matches = "never")
  
  audit <-
    audit |>
    mutate(
      F03_PRESENT_GREENLIST =
        coalesce(
          F03_GREENLIST_RECORDS,
          0L
        ) > 0L,
      
      F03_PRESENT_FERNGREENLIST =
        coalesce(
          F03_FERNLIST_RECORDS,
          0L
        ) > 0L,
      
      F03_PRESENT_CONTEMPORARY_JAPAN_CHECKLIST =
        F03_PRESENT_GREENLIST |
        F03_PRESENT_FERNGREENLIST |
        !is.na(
          SOURCE
        )
    )
  
  green_distribution_profile <-
    tibble()
  
  if (
    green_distribution_available &&
    nrow(green_distribution) > 0L
  ) {
    
    green_dist_id_field <-
      first_existing_field(
        green_distribution,
        c(
          "COREID",
          "TAXONID",
          "DWCA_ID"
        )
      )
    
    green_dist_status_field <-
      first_existing_field(
        green_distribution,
        c(
          "ESTABLISHMENTMEANS",
          "OCCURRENCESTATUS",
          "STATUS",
          "LOCALITY",
          "LOCATIONID"
        )
      )
    
    if (
      !is.na(green_dist_id_field)
    ) {
      
      green_distribution_profile <-
        tibble(
          F03_GREEN_ID =
            safe_field(
              green_distribution,
              green_dist_id_field
            ),
          
          F03_GREEN_DISTRIBUTION_VALUE =
            safe_field(
              green_distribution,
              green_dist_status_field
            )
        ) |>
        filter(
          !is.na(
            F03_GREEN_ID
          )
        ) |>
        group_by(
          F03_GREEN_ID
        ) |>
        summarise(
          F03_GREEN_DISTRIBUTION =
            collapse_unique(
              F03_GREEN_DISTRIBUTION_VALUE
            ),
          
          .groups = "drop"
        )
    }
  }
  
  if (
    nrow(green_distribution_profile) > 0L
  ) {
    
    green_inventory_distribution <-
      green_inventory |>
      left_join(
        green_distribution_profile,
        by = "F03_GREEN_ID"
      ) |>
      filter(
        !is.na(
          F03_GREEN_NAME_NORMALISED
        )
      ) |>
      group_by(
        F03_GREEN_NAME_NORMALISED
      ) |>
      summarise(
        F03_GREENLIST_DISTRIBUTION =
          collapse_unique(
            F03_GREEN_DISTRIBUTION
          ),
        
        .groups = "drop"
      )
    
    audit <-
      audit |>
      left_join(
        green_inventory_distribution,
        by =
          c(
            "F03_JAPAN_NAME_NORMALISED" =
              "F03_GREEN_NAME_NORMALISED"
          )
      )
    
  } else {
    
    audit <-
      audit |>
      mutate(
        F03_GREENLIST_DISTRIBUTION =
          NA_character_
      )
  }
  
  # Preserve every extension field as evidence by original source taxon ID.
  # Summarise before joining to prevent one-to-many extensions multiplying audit rows.
  attach_extension <- function(audit, extension, source_ids, prefix) {
    id_field <- first_existing_field(extension, c("COREID", "TAXONID", "DWCA_ID"))
    audit[[paste0(prefix, "_AVAILABLE")]] <- ncol(extension) > 0L
    audit[[paste0(prefix, "_EVIDENCE")]] <- NA_character_
    if (!nrow(extension) || is.na(id_field)) return(audit)
    extension <- extension[safe_field(extension, id_field) %in%
                             normalise_text(audit$SOURCE_TAXON_ID[audit$SOURCE %in% source_ids]), , drop = FALSE]
    if (!nrow(extension)) return(audit)
    values <- setdiff(names(extension), id_field)
    text <- vapply(seq_len(nrow(extension)), function(i) {
      vals <- vapply(values, function(f) normalise_text(extension[[f]][i]), character(1))
      keep <- !is.na(vals)
      if (!any(keep)) return(NA_character_)
      paste(paste0(values[keep], "=", vals[keep]), collapse = "; ")
    }, character(1))
    profile <- tibble(ID = safe_field(extension, id_field), EVIDENCE = text) |>
      filter(!is.na(ID)) |> group_by(ID) |> summarise(EVIDENCE = collapse_unique(EVIDENCE), .groups = "drop")
    idx <- match(normalise_text(audit$SOURCE_TAXON_ID), profile$ID)
    belongs <- audit$SOURCE %in% source_ids
    audit[[paste0(prefix, "_EVIDENCE")]][belongs] <- profile$EVIDENCE[idx[belongs]]
    audit
  }
  # Source labels come from the source-ID membership, not an assumed spelling.
  green_sources <- unique(potential_gaps_03e$SOURCE[normalise_text(potential_gaps_03e$SOURCE_TAXON_ID) %in% green_inventory$F03_GREEN_ID])
  fern_sources <- unique(potential_gaps_03e$SOURCE[normalise_text(potential_gaps_03e$SOURCE_TAXON_ID) %in% fern_inventory$F03_FERN_ID])
  audit <- attach_extension(audit, green_distribution, green_sources, "F03_GREEN_DISTRIBUTION_EXTENSION")
  audit <- attach_extension(audit, green_vernacular, green_sources, "F03_GREEN_VERNACULAR_EXTENSION")
  audit <- attach_extension(audit, fern_vernacular, fern_sources, "F03_FERN_VERNACULAR_EXTENSION")
  audit <- attach_extension(audit, fern_profile, fern_sources, "F03_FERN_PROFILE_EXTENSION")
  #
  # These are evidence flags, not final taxonomic decisions.
  #
  # We deliberately avoid interpreting simple checklist presence as proof of
  # nativeness.
  #
  # ==============================================================================
  
  audit <-
    audit |>
    mutate(
      F03_DISTRIBUTION_EVIDENCE_TEXT =
        stringr::str_to_lower(
          paste(
            coalesce(
              F03_GREENLIST_DISTRIBUTION,
              ""
            ),
            coalesce(
              F03_GREENLIST_CORE_STATUS,
              ""
            ),
            coalesce(
              F03_FERNLIST_CORE_STATUS,
              ""
            ),
            coalesce(
              SOURCE_TAXONOMIC_STATUS,
              ""
            )
          )
        ),
      
      F03_EXPLICIT_INTRODUCED_EVIDENCE =
        stringr::str_detect(
          F03_DISTRIBUTION_EVIDENCE_TEXT,
          paste(
            c(
              "introduced",
              "alien",
              "non-native",
              "nonnative",
              "naturalised",
              "naturalized",
              "cultivated"
            ),
            collapse = "|"
          )
        ),
      
      F03_EXPLICIT_NATIVE_EVIDENCE =
        stringr::str_detect(
          F03_DISTRIBUTION_EVIDENCE_TEXT,
          "\\bnative\\b|indigenous"
        ) &
        !F03_EXPLICIT_INTRODUCED_EVIDENCE
    )
  
  audit <-
    audit |>
    mutate(
      F03_RELATED_BINOMIAL_IN_VPJD =
        coalesce(
          F03_CANONICAL_BINOMIAL_RECORDS,
          0L
        ) > 0L,
      
      F03_GENUS_PRESENT_IN_VPJD =
        coalesce(
          F03_CANONICAL_GENUS_RECORDS,
          0L
        ) > 0L
    )
  
  audit <-
    audit |>
    mutate(
      F03_HAS_ACCEPTED_WCVP_ID =
        !is.na(
          F03_ACCEPTED_ID
        ),
      
      F03_HAS_ACCEPTED_WCVP_NAME =
        !is.na(
          F03_ACCEPTED_NAME
        ),
      
      F03_HAS_CHECKLIST_NAME =
        !is.na(
          JAPAN_SCIENTIFIC_NAME
        ),
      
      F03_HAS_JAPANESE_NAME =
        !is.na(
          JAPANESE_NAME
        ),
      
      F03_HAS_DISTRIBUTION_EVIDENCE =
        !is.na(
          F03_GREENLIST_DISTRIBUTION
        ) |
        !is.na(
          F03_GREENLIST_CORE_STATUS
        ) |
        !is.na(
          F03_FERNLIST_CORE_STATUS
        ) |
        !is.na(
          SOURCE_TAXONOMIC_STATUS
        )
    )
  
  #
  # IMPORTANT:
  #
  # "PROBABLE_CANONICAL_GAP" is still an audit category.
  #
  # It is NOT permission to add the taxon.
  #
  # ==============================================================================
  
  audit <-
    audit |>
    mutate(
      F03_GAP_CLASS =
        case_when(
          
          F03_WCVP_ACCEPTED_STATUS_CONFLICT ~ "UPSTREAM_ACCEPTED_ID_STATUS_REVIEW",
          F03_ACCEPTED_ID_PRESENT_IN_VPJD ~
            "EXPLAINED_ACCEPTED_ID_ALREADY_IN_VPJD",
          
          F03_IS_HYBRID ~ "HYBRID_SCOPE_REVIEW",
          F03_IS_INFRASPECIFIC ~ "INFRASPECIFIC_SCOPE_REVIEW",
          F03_EXPLICIT_INTRODUCED_EVIDENCE ~
            "INTRODUCED_OR_NON_NATIVE_SCOPE_REVIEW",
          
          F03_RELATED_BINOMIAL_IN_VPJD ~
            "RELATED_CANONICAL_CONCEPT_REVIEW",
          
          !F03_HAS_ACCEPTED_WCVP_ID |
            !F03_HAS_ACCEPTED_WCVP_NAME ~
            "INSUFFICIENT_WCVP_EVIDENCE",
          
          F03_HAS_ACCEPTED_WCVP_ID &
            F03_HAS_ACCEPTED_WCVP_NAME &
            F03_PRESENT_CONTEMPORARY_JAPAN_CHECKLIST &
            !F03_RELATED_BINOMIAL_IN_VPJD &
            !F03_EXPLICIT_INTRODUCED_EVIDENCE ~
            "PROBABLE_CANONICAL_GAP",
          
          TRUE ~
            "FURTHER_REVIEW_REQUIRED"
        )
    )
  
  audit <-
    audit |>
    mutate(
      F03_REVIEW_PRIORITY =
        case_when(
          
          F03_GAP_CLASS ==
            "PROBABLE_CANONICAL_GAP" &
            F03_EXPLICIT_NATIVE_EVIDENCE ~
            "HIGH",
          
          F03_GAP_CLASS ==
            "PROBABLE_CANONICAL_GAP" ~
            "HIGH",
          
          F03_GAP_CLASS ==
            "RELATED_CANONICAL_CONCEPT_REVIEW" ~
            "MEDIUM",
          
          F03_GAP_CLASS ==
            "INTRODUCED_OR_NON_NATIVE_SCOPE_REVIEW" ~
            "MEDIUM",
          
          TRUE ~
            "REVIEW"
        )
    )
  
  audit <-
    audit |>
    mutate(
      F03_REQUIRED_NEXT_EVIDENCE =
        case_when(
          
          F03_GAP_CLASS ==
            "PROBABLE_CANONICAL_GAP" &
            !F03_HAS_DISTRIBUTION_EVIDENCE ~
            paste(
              "Verify Japanese native/introduced distribution;",
              "verify WCVP accepted concept;",
              "trace absence from original VPJD compilation"
            ),
          
          F03_GAP_CLASS ==
            "PROBABLE_CANONICAL_GAP" ~
            paste(
              "Verify distribution evidence;",
              "trace absence from original VPJD compilation;",
              "confirm VPJD scope eligibility"
            ),
          
          F03_GAP_CLASS ==
            "INTRODUCED_OR_NON_NATIVE_SCOPE_REVIEW" ~
            paste(
              "Verify establishment status in Japan;",
              "determine whether taxon falls within VPJD scope"
            ),
          
          F03_GAP_CLASS ==
            "RELATED_CANONICAL_CONCEPT_REVIEW" ~
            paste(
              "Compare WCVP concepts and ranks;",
              "determine whether VPJD already represents taxon under broader concept"
            ),
          
          F03_GAP_CLASS ==
            "EXPLAINED_ACCEPTED_ID_ALREADY_IN_VPJD" ~
            paste(
              "Audit upstream reconciliation;",
              "accepted WCVP ID is now detectable in canonical VPJD"
            ),
          
          TRUE ~
            paste(
              "Inspect WCVP nomenclature;",
              "inspect contemporary checklist record;",
              "review VPJD scope"
            )
        )
    )
  
  probable_gaps <-
    audit |>
    filter(
      F03_GAP_CLASS ==
        "PROBABLE_CANONICAL_GAP"
    )
  
  introduced_review <-
    audit |>
    filter(
      F03_GAP_CLASS ==
        "INTRODUCED_OR_NON_NATIVE_SCOPE_REVIEW"
    )
  
  related_concept_review <-
    audit |>
    filter(
      F03_GAP_CLASS ==
        "RELATED_CANONICAL_CONCEPT_REVIEW"
    )
  
  further_review <-
    audit |>
    filter(
      F03_GAP_CLASS %in%
        c(
          "UPSTREAM_ACCEPTED_ID_STATUS_REVIEW",
          "HYBRID_SCOPE_REVIEW",
          "INFRASPECIFIC_SCOPE_REVIEW",
          "INSUFFICIENT_WCVP_EVIDENCE",
          "FURTHER_REVIEW_REQUIRED",
          "EXPLAINED_ACCEPTED_ID_ALREADY_IN_VPJD"
        )
    )
  
  summary_table <-
    audit |>
    count(
      F03_GAP_CLASS,
      name = "N_RECORDS"
    ) |>
    mutate(
      PERCENT_OF_201 =
        round(
          100 *
            N_RECORDS /
            EXPECTED_POTENTIAL_GAPS,
          2
        )
    ) |>
    arrange(
      desc(
        N_RECORDS
      )
    )
  
  source_summary <-
    audit |>
    count(
      SOURCE,
      F03_GAP_CLASS,
      name = "N_RECORDS"
    ) |>
    arrange(
      SOURCE,
      desc(
        N_RECORDS
      )
    )
  
  n_audit <-
    nrow(
      audit
    )
  
  n_probable_gap <-
    nrow(
      probable_gaps
    )
  
  n_introduced_review <-
    nrow(
      introduced_review
    )
  
  n_related_review <-
    nrow(
      related_concept_review
    )
  
  n_further_review <-
    nrow(
      further_review
    )
  
  n_native_evidence <-
    sum(
      audit$F03_EXPLICIT_NATIVE_EVIDENCE,
      na.rm = TRUE
    )
  
  n_introduced_evidence <-
    sum(
      audit$F03_EXPLICIT_INTRODUCED_EVIDENCE,
      na.rm = TRUE
    )
  
  n_with_japanese_name <-
    sum(
      audit$F03_HAS_JAPANESE_NAME,
      na.rm = TRUE
    )
  
  # Validation: fail before publishing if the established population changes.
  wcvp_resolution_exceptions <- audit |> filter(!F03_WCVP_METADATA_COMPLETE)
  taxonomy_review <- audit |> filter(!F03_WCVP_CONCEPT_RESOLVED)
  wcvp_resolution_summary <- audit |> count(F03_WCVP_RESOLUTION_CLASS, name = "N_RECORDS")
  crosswalk_summary <- tibble(
    METRIC = c("03e potential canonical gaps", "Accepted WCVP IDs retained",
               "Accepted WCVP names resolved", "Complete WCVP metadata", "Resolved concepts requiring metadata review",
               "Japanese names identical to accepted WCVP", "Japanese names differing from accepted WCVP",
               "Japanese names treated as WCVP synonyms", "Japanese names treated as WCVP accepted",
               "Hybrid scope review", "Infraspecific scope review", "Related canonical concept review",
               "Potential canonical gaps retained"),
    VALUE = c(nrow(audit), sum(audit$F03_HAS_ACCEPTED_WCVP_ID),
              sum(audit$F03_HAS_ACCEPTED_WCVP_NAME), sum(audit$F03_WCVP_METADATA_COMPLETE),
              sum(audit$F03_WCVP_CONCEPT_RESOLVED & !audit$F03_WCVP_METADATA_COMPLETE), sum(audit$F03_JAPAN_WCVP_NAME_IDENTICAL),
              sum(audit$F03_JAPAN_WCVP_NAME_DIFFERENT), sum(audit$F03_JAPANESE_NAME_IS_WCVP_SYNONYM),
              sum(audit$F03_JAPANESE_NAME_IS_WCVP_ACCEPTED), sum(audit$F03_IS_HYBRID),
              sum(audit$F03_IS_INFRASPECIFIC), nrow(related_concept_review), nrow(probable_gaps)),
    EXPECTED = c(201L, 201L, 201L, 56L, 145L, 180L, 21L, 19L, 180L, 0L, 3L, 0L, 198L))
  unchanged <- function(table, before) {
    after <- DBI::dbReadTable(con, table) |> as_tibble()
    # These are unmodified base tables; preserve order, classes and all values.
    identical(after, before)
  }
  upstream_fields <- grep("^D03_", names(potential_gaps_03e), value = TRUE)
  validation <- bind_rows(
    crosswalk_summary |> transmute(CHECK = METRIC, EXPECTED = as.character(EXPECTED),
                                   OBSERVED = as.character(VALUE), PASS = !is.na(VALUE) & VALUE == as.integer(EXPECTED)),
    tibble(
      CHECK = c("Canonical population", "Japan checklist population", "03e review population",
                "03e WCVP-not-VPJD population", "Unique nonmissing SOURCE_03C_ROW",
                "Unique nonmissing JAPAN_RECORD_ID", "03d fields retained without alteration",
                "Accepted IDs preserved in order", "Output classes partition input",
                "Canonical contents including Star unchanged", "Validated lineage unchanged"),
      EXPECTED = c("11439", "9821", "4280", "290", rep("TRUE", 7L)),
      OBSERVED = as.character(c(nrow(canonical), nrow(green_core) + nrow(fern_core),
                                nrow(audit_03e), nrow(wcvp_not_vpjd_03e),
                                !anyNA(audit$SOURCE_03C_ROW) && !anyDuplicated(audit$SOURCE_03C_ROW),
                                !anyNA(audit$JAPAN_RECORD_ID) && !anyDuplicated(audit$JAPAN_RECORD_ID),
                                identical(as.data.frame(audit[, upstream_fields]), as.data.frame(potential_gaps_03e[, upstream_fields])),
                                identical(audit$WCVP_ID, normalise_id(potential_gaps_03e$D03_ACCEPTED_WCVP_ID)),
                                n_probable_gap + n_introduced_review + n_related_review + n_further_review == n_audit,
                                unchanged(CANONICAL_TABLE, canonical), unchanged(LINEAGE_TABLE, lineage))))) |>
    mutate(PASS = ifelse(is.na(PASS), EXPECTED == OBSERVED, PASS))
  # Coercing numeric and logical values together above yields 1/0; make these explicit.
  logical_rows <- seq.int(nrow(crosswalk_summary) + 5L, nrow(validation))
  validation$OBSERVED[logical_rows] <- ifelse(validation$OBSERVED[logical_rows] %in% c("1", "TRUE"), "TRUE", "FALSE")
  validation$PASS[logical_rows] <- validation$OBSERVED[logical_rows] == validation$EXPECTED[logical_rows]
  # Historical scientific totals are regression comparisons. Structural/provenance
  # checks always block publication; strict_historical also blocks changed totals.
  validation$CHECK_TYPE <- c(rep("HISTORICAL_BASELINE", nrow(crosswalk_summary)), rep("INVARIANT", 11L))
  validation$CHECK_TYPE[c(1L, 2L)] <- "INVARIANT"
  validation$BLOCKING <- validation$CHECK_TYPE == "INVARIANT" | strict_historical
  discrepancies <- validation |> filter(!PASS) |>
    mutate(EXPLANATION = case_when(
      CHECK == "Accepted WCVP names resolved" ~ "ID 503873 is Unplaced in the WCVP index and absent from the accepted lookup; no accepted name is fabricated.",
      CHECK == "Resolved concepts requiring metadata review" ~ "Of the previous 145 exceptions, 144 are metadata-only cases and one requires taxonomy/status review.",
      CHECK == "Japanese names differing from accepted WCVP" ~ "180 identical plus 20 differing accepted names plus one unavailable accepted name account for all 201 records.",
      CHECK == "Infraspecific scope review" ~ "Six accepted WCVP concepts have infraspecific rank. The former three flags matched the author abbreviation L.f.",
      CHECK == "Potential canonical gaps retained" ~ "201 records minus six infraspecific reviews and one WCVP status review leaves 194 candidates.",
      TRUE ~ "Inspect source data and validation details; do not force historical totals."))
  cat("\n03f WCVP-first crosswalk summary\n")
  preview(crosswalk_summary)
  cat("\nWCVP exceptions: distinguish incomplete metadata from concept/status review\n")
  preview(wcvp_resolution_exceptions |> select(AUDIT_03F_ROW, JAPAN_RECORD_ID,
                                               JAPAN_SCIENTIFIC_NAME, D03_WCVP_MATCHED_STATUS, WCVP_ID, WCVP_ACCEPTED_NAME,
                                               WCVP_RANK, WCVP_FAMILY, WCVP_GENUS, WCVP_STATUS, F03_WCVP_RESOLUTION_CLASS))
  cat("\nValidation\n")
  for (i in seq_len(nrow(validation))) {
    cat(sprintf("%02d %-55s %s (expected %s; observed %s)\n", i, validation$CHECK[[i]],
                ifelse(isTRUE(validation$PASS[[i]]), "PASS", ifelse(validation$BLOCKING[[i]], "FAIL", "CHANGED")),
                validation$EXPECTED[[i]], validation$OBSERVED[[i]]))
  }
  if (any(validation$BLOCKING & !validation$PASS))
    stop("03f validation failed; no outputs published. Inspect historical discrepancies. After review, strict_historical = FALSE permits changed scientific totals while retaining all structural safeguards.")
  metadata <- tibble(
    METRIC = c("MODULE", "VERSION", "RUN_UTC", "DATABASE", "SOURCE_03E_TABLE",
               "CANONICAL_TABLE", "TAXONOMIC_AUTHORITY", "CHECKLIST_ROLE", "CANONICAL_TAXONOMY_MODIFIED",
               "STAR_ALLOCATIONS_MODIFIED", "FUZZY_MATCHING", "DECISION"),
    VALUE = c(MODULE, VERSION, format(Sys.time(), tz = "UTC", usetz = TRUE), DB_PATH,
              TABLE_03E_POTENTIAL_GAPS, CANONICAL_TABLE, "Kew/WCVP", "Crosswalk and Japanese-flora evidence",
              "FALSE", "FALSE", "FALSE", if (all(validation$PASS)) "POTENTIAL_VPJD_CANONICAL_GAP_AUDIT_COMPLETE" else "AUDIT_COMPLETE_WITH_BASELINE_DISCREPANCIES"))
  objects <- list(audit, probable_gaps, introduced_review, related_concept_review,
                  further_review, summary_table, source_summary, validation, metadata,
                  crosswalk_summary, wcvp_resolution_summary, wcvp_resolution_exceptions, taxonomy_review, discrepancies)
  tables <- c(OUTPUT_AUDIT_TABLE, OUTPUT_PROBABLE_GAP_TABLE, OUTPUT_INTRODUCED_TABLE,
              OUTPUT_CONCEPT_REVIEW_TABLE, OUTPUT_UNRESOLVED_TABLE, OUTPUT_SUMMARY_TABLE,
              OUTPUT_SOURCE_SUMMARY_TABLE, OUTPUT_VALIDATION_TABLE, OUTPUT_METADATA_TABLE,
              "vpjd_taxrev_03f_crosswalk_summary", "vpjd_taxrev_03f_wcvp_resolution_summary",
              "vpjd_taxrev_03f_wcvp_resolution_exceptions", "vpjd_taxrev_03f_taxonomy_review",
              "vpjd_taxrev_03f_baseline_discrepancies")
  csvs <- c(OUTPUT_AUDIT_CSV, OUTPUT_PROBABLE_GAP_CSV, OUTPUT_INTRODUCED_CSV,
            OUTPUT_CONCEPT_REVIEW_CSV, OUTPUT_UNRESOLVED_CSV, OUTPUT_SUMMARY_CSV,
            OUTPUT_SOURCE_SUMMARY_CSV, OUTPUT_VALIDATION_CSV, OUTPUT_METADATA_CSV,
            file.path(OUTPUT_DIR, c("vpjd_03f_crosswalk_summary.csv", "vpjd_03f_wcvp_resolution_summary.csv",
                                    "vpjd_03f_wcvp_resolution_exceptions.csv", "vpjd_03f_taxonomy_review.csv",
                                    "vpjd_03f_baseline_discrepancies.csv")))
  stopifnot(length(objects) == length(tables), length(tables) == length(csvs),
            !anyDuplicated(tables), !anyDuplicated(csvs), all(startsWith(tables, "vpjd_taxrev_03f_")),
            !any(tables %in% required_tables))
  if (write_outputs) {
    dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
    if (!dir.exists(OUTPUT_DIR)) stop("Cannot create output directory: ", OUTPUT_DIR)
    stage <- tempfile(".03f-stage-", tmpdir = OUTPUT_DIR)
    dir.create(stage)
    # Stage and round-trip all CSVs before replacing any public output.
    staged <- file.path(stage, basename(csvs))
    for (i in seq_along(objects)) {
      readr::write_csv(objects[[i]], staged[[i]], na = "")
      actual <- readr::read_csv(staged[[i]], col_types = readr::cols(.default = readr::col_character()),
                                na = "", trim_ws = FALSE, show_col_types = FALSE, progress = FALSE)
      expected <- as.data.frame(lapply(objects[[i]], as.character), stringsAsFactors = FALSE)
      # CSV blank/NA convention is deliberately the same as the original pipeline.
      expected[] <- lapply(expected, function(x) { x[!is.na(x) & x == ""] <- NA_character_; x })
      csv_comparison <- all.equal(as.data.frame(actual), expected, check.attributes = FALSE)
      if (!isTRUE(csv_comparison) ||
          !identical(names(actual), names(objects[[i]])))
        stop("CSV round-trip failed: ", staged[[i]], "\n", paste(csv_comparison, collapse = "; "))
    }
    backup <- file.path(stage, "previous")
    dir.create(backup)
    previous <- file.exists(csvs)
    for (i in which(previous)) {
      if (!file.copy(csvs[[i]], file.path(backup, basename(csvs[[i]]))))
        stop("Cannot back up existing CSV: ", csvs[[i]])
    }
    installed <- integer()
    transaction_open <- FALSE
    tryCatch({
      DBI::dbBegin(con)
      transaction_open <- TRUE
      for (i in seq_along(objects)) {
        DBI::dbWriteTable(con, tables[[i]], as.data.frame(objects[[i]]), overwrite = TRUE)
        actual <- DBI::dbReadTable(con, tables[[i]])
        if (!isTRUE(all.equal(actual, as.data.frame(objects[[i]]), check.attributes = FALSE)))
          stop("DuckDB round-trip failed: ", tables[[i]])
      }
      if (!unchanged(CANONICAL_TABLE, canonical) || !unchanged(LINEAGE_TABLE, lineage))
        stop("Protected canonical/lineage contents changed; rolling back output transaction.")
      for (i in seq_along(csvs)) {
        installed <- c(installed, i) # Includes a partially failed copy for recovery.
        if (!file.copy(staged[[i]], csvs[[i]], overwrite = TRUE)) stop("Cannot publish CSV: ", csvs[[i]])
        if (unname(tools::md5sum(staged[[i]])) != unname(tools::md5sum(csvs[[i]])))
          stop("Published CSV checksum mismatch: ", csvs[[i]])
      }
      DBI::dbCommit(con)
      transaction_open <- FALSE
    }, error = function(e) {
      if (transaction_open) try(DBI::dbRollback(con), silent = TRUE)
      for (i in installed) {
        if (previous[[i]]) {
          if (!file.copy(file.path(backup, basename(csvs[[i]])), csvs[[i]], overwrite = TRUE))
            warning("Restore this CSV manually from ", backup, ": ", basename(csvs[[i]]))
        } else if (file.exists(csvs[[i]])) {
          if (!file.remove(csvs[[i]])) warning("Could not remove incomplete CSV: ", csvs[[i]])
        }
      }
      stop("03f output publication failed: ", conditionMessage(e),
           "\nStaged files and recovery backups retained at: ", stage)
    })
    # Retain this run's recovery copy; no recursive deletion of user output directories.
    cat("\nDuckDB and CSV outputs verified. Recovery copy: ", stage, "\n", sep = "")
  }
  cat("\n03f v", VERSION, if (write_outputs) " COMPLETE\n" else " READ-ONLY VALIDATION COMPLETE\n", sep = "")
  cat("Accepted WCVP concepts resolved: ", sum(audit$F03_WCVP_CONCEPT_RESOLVED),
      "; metadata complete: ", sum(audit$F03_WCVP_METADATA_COMPLETE),
      "; metadata-only review: ", sum(audit$F03_WCVP_CONCEPT_RESOLVED & !audit$F03_WCVP_METADATA_COMPLETE),
      "; taxonomy/status review: ", nrow(taxonomy_review), ".\n", sep = "")
  cat("Potential gaps: ", n_probable_gap, "; infraspecific scope review: ", sum(audit$F03_IS_INFRASPECIFIC), ".\n", sep = "")
  cat("Canonical VPJD: 11,439 records; canonical contents and Star allocations unchanged.\n")
  cat("No taxa added/removed; no fuzzy matching; no checklist taxonomy imposed.\n")
  cat("Blocking safeguards: ", sum(validation$PASS & validation$BLOCKING), "/",
      sum(validation$BLOCKING), " PASS; historical discrepancies reported: ", nrow(discrepancies), "\n", sep = "")
  if (write_outputs) cat("Output directory: ", OUTPUT_DIR, "\n", sep = "")
  invisible(list(audit = audit, validation = validation, crosswalk_summary = crosswalk_summary,
                 outputs = setNames(objects, tables)))
}

if (isTRUE(getOption("vpjd.03f.autorun", TRUE))) run_03f()
