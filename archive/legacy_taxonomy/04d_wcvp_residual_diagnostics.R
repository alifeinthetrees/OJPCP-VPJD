# ==============================================================================
# 04d_wcvp_residual_diagnostics.R
#
# Oxford–Japan Plant Conservation Partnership (OJPCP)
# Vascular Plants of Japan Database (VPJD)
#
# Purpose:
#   Diagnose the residual WCVP reconciliation pool remaining after the
#   validated deterministic reconciliation pipeline:
#
#     03  source taxon universe          v0.2.0
#     04  WCVP standardisation           v0.3.3
#     04b structured-name recovery       v0.1.1
#     04c deterministic consolidation    v0.1.2
#
# This module:
#   - performs NO new taxonomic matching;
#   - performs NO fuzzy matching;
#   - makes NO taxonomic decisions;
#   - does NOT alter frozen 03/04/04b/04c outputs;
#   - classifies residual candidates using existing source/WCVP evidence;
#   - produces candidate-level diagnostic tables and audits to guide 04e.
#
# Expected residual population:
#   Total candidates                      32,292
#   Deterministically resolved            26,610
#   Total unresolved                       5,682
#     unmatched                            3,703
#     ambiguous                            1,928
#     review                                  51
#
# Version: 0.1.0
# ==============================================================================

WCVP_RESIDUAL_DIAGNOSTICS_VERSION <- "0.1.0"

# ------------------------------------------------------------------------------
# Packages
# ------------------------------------------------------------------------------

required_packages <- c(
  "dplyr", "tidyr", "stringr", "readr", "tibble", "here"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    "Missing required packages: ",
    paste(missing_packages, collapse = ", "),
    call. = FALSE
  )
}

# ------------------------------------------------------------------------------
# Validated expectations
# ------------------------------------------------------------------------------

EXPECTED_CANDIDATES <- 32292L
EXPECTED_RESOLVED <- 26610L
EXPECTED_UNRESOLVED <- 5682L
EXPECTED_UNMATCHED <- 3703L
EXPECTED_AMBIGUOUS <- 1928L
EXPECTED_REVIEW <- 51L
EXPECTED_SOURCE_RECORDS <- 40891L

# ------------------------------------------------------------------------------
# Helpers
# ------------------------------------------------------------------------------

clean_character_04d <- function(x) {
  x <- as.character(x)
  x <- stringr::str_squish(x)
  x[
    is.na(x) |
      x == "" |
      toupper(x) %in% c("NA", "N/A", "NULL")
  ] <- NA_character_
  x
}

assert_equal_04d <- function(observed, expected, label) {
  if (
    length(observed) != 1L ||
    is.na(observed) ||
    observed != expected
  ) {
    stop(
      label,
      ": expected ",
      format(expected, big.mark = ","),
      ", observed ",
      format(observed, big.mark = ","),
      ".",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

collapse_unique_04d <- function(x, sep = " | ") {
  x <- clean_character_04d(x)
  x <- sort(unique(x[!is.na(x)]))
  if (length(x) == 0L) NA_character_ else paste(x, collapse = sep)
}

first_existing_field_04d <- function(data, candidates) {
  found <- candidates[candidates %in% names(data)]
  if (length(found) == 0L) NA_character_ else found[[1]]
}

extract_optional_04d <- function(data, field) {
  if (is.na(field) || !field %in% names(data)) {
    return(rep(NA_character_, nrow(data)))
  }
  clean_character_04d(data[[field]])
}

# ------------------------------------------------------------------------------
# Input loaders
# ------------------------------------------------------------------------------

load_04d_consolidated_candidates <- function() {
  path <- here::here(
    "data", "interim", "taxonomy", "WCVP", "consolidated",
    "vpjd_wcvp_consolidated_candidates.rds"
  )
  
  if (!file.exists(path)) {
    stop("04c consolidated candidate file not found: ", path)
  }
  
  x <- readRDS(path)
  
  required <- c(
    "wcvp_candidate_id",
    "wcvp_submitted_name",
    "original_04_status",
    "recovery_status",
    "final_reconciliation_status",
    "final_wcvp_concept_id"
  )
  
  missing <- setdiff(required, names(x))
  
  if (length(missing) > 0L) {
    stop(
      "Required 04c candidate fields missing: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  
  x
}

load_04d_source_crosswalk <- function() {
  path <- here::here(
    "data", "interim", "taxonomy",
    "source_to_wcvp_candidate.rds"
  )
  
  if (!file.exists(path)) {
    stop("Source-to-candidate crosswalk not found: ", path)
  }
  
  x <- readRDS(path)
  
  required <- c(
    "source_record_id",
    "source_repository",
    "source_spnumber",
    "source_fullname",
    "source_matching_name",
    "wcvp_candidate_id",
    "wcvp_submitted_name"
  )
  
  missing <- setdiff(required, names(x))
  
  if (length(missing) > 0L) {
    stop(
      "Required source-crosswalk fields missing: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  
  x
}

load_04d_candidate_resolution <- function() {
  path <- here::here(
    "data", "interim", "taxonomy", "WCVP",
    "vpjd_wcvp_candidate_resolution.rds"
  )
  
  if (!file.exists(path)) {
    stop("04 candidate-resolution file not found: ", path)
  }
  
  readRDS(path)
}

load_04d_raw_matches <- function() {
  path <- here::here(
    "data", "interim", "taxonomy", "WCVP",
    "vpjd_wcvp_raw_matches.rds"
  )
  
  if (!file.exists(path)) {
    stop("04 raw-match file not found: ", path)
  }
  
  readRDS(path)
}

load_04d_recovery_resolution <- function() {
  path <- here::here(
    "data", "interim", "taxonomy", "WCVP", "recovery",
    "vpjd_wcvp_recovery_resolution.rds"
  )
  
  if (!file.exists(path)) {
    stop("04b recovery-resolution file not found: ", path)
  }
  
  readRDS(path)
}

load_04d_recovery_alternatives <- function() {
  path <- here::here(
    "data", "interim", "taxonomy", "WCVP", "recovery",
    "vpjd_wcvp_recovery_alternatives.rds"
  )
  
  if (!file.exists(path)) {
    stop("04b recovery-alternatives file not found: ", path)
  }
  
  readRDS(path)
}

load_04d_geojapan_taxa <- function() {
  path <- here::here(
    "data", "interim", "GEOJAPAN",
    "geojapan_taxa.rds"
  )
  
  if (!file.exists(path)) {
    stop("Curated GEOJAPAN taxa file not found: ", path)
  }
  
  readRDS(path)
}

# ------------------------------------------------------------------------------
# Validate frozen baseline
# ------------------------------------------------------------------------------

validate_04d_baseline <- function(candidates, source_crosswalk) {
  message("Validating frozen 04c baseline...")
  
  assert_equal_04d(
    nrow(candidates),
    EXPECTED_CANDIDATES,
    "Candidate count"
  )
  
  assert_equal_04d(
    nrow(source_crosswalk),
    EXPECTED_SOURCE_RECORDS,
    "Source-record count"
  )
  
  resolved <- sum(
    candidates$final_reconciliation_status %in%
      c("resolved", "resolved_hybrid"),
    na.rm = TRUE
  )
  
  unmatched <- sum(
    candidates$final_reconciliation_status == "unmatched",
    na.rm = TRUE
  )
  
  ambiguous <- sum(
    candidates$final_reconciliation_status == "ambiguous",
    na.rm = TRUE
  )
  
  review <- sum(
    candidates$final_reconciliation_status == "review",
    na.rm = TRUE
  )
  
  assert_equal_04d(
    resolved,
    EXPECTED_RESOLVED,
    "Resolved candidate count"
  )
  
  assert_equal_04d(
    unmatched,
    EXPECTED_UNMATCHED,
    "Unmatched candidate count"
  )
  
  assert_equal_04d(
    ambiguous,
    EXPECTED_AMBIGUOUS,
    "Ambiguous candidate count"
  )
  
  assert_equal_04d(
    review,
    EXPECTED_REVIEW,
    "Review candidate count"
  )
  
  assert_equal_04d(
    unmatched + ambiguous + review,
    EXPECTED_UNRESOLVED,
    "Total unresolved candidate count"
  )
  
  message("Frozen 04c baseline validation passed.")
  invisible(TRUE)
}

# ------------------------------------------------------------------------------
# Residual candidate universe
# ------------------------------------------------------------------------------

build_04d_residual_candidates <- function(candidates) {
  candidates %>%
    dplyr::filter(
      final_reconciliation_status %in%
        c("unmatched", "ambiguous", "review")
    ) %>%
    dplyr::mutate(
      wcvp_candidate_id = as.character(wcvp_candidate_id),
      residual_status = final_reconciliation_status
    )
}

# ------------------------------------------------------------------------------
# Source-evidence summary
# ------------------------------------------------------------------------------

build_04d_source_evidence <- function(
    residual_candidates,
    source_crosswalk
) {
  residual_ids <- residual_candidates$wcvp_candidate_id
  
  source_crosswalk %>%
    dplyr::filter(wcvp_candidate_id %in% residual_ids) %>%
    dplyr::group_by(wcvp_candidate_id) %>%
    dplyr::summarise(
      source_record_count = dplyr::n(),
      geojapan_record_count = sum(
        source_repository == "GEOJAPAN",
        na.rm = TRUE
      ),
      foj_record_count = sum(
        source_repository == "FOJ",
        na.rm = TRUE
      ),
      source_repository_count = dplyr::n_distinct(
        source_repository
      ),
      source_repositories = collapse_unique_04d(
        source_repository
      ),
      source_spnumbers = collapse_unique_04d(
        source_spnumber
      ),
      source_fullnames = collapse_unique_04d(
        source_fullname
      ),
      source_matching_names = collapse_unique_04d(
        source_matching_name
      ),
      source_fullname_count = dplyr::n_distinct(
        source_fullname,
        na.rm = TRUE
      ),
      source_matching_name_count = dplyr::n_distinct(
        source_matching_name,
        na.rm = TRUE
      ),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      source_class = dplyr::case_when(
        geojapan_record_count > 0 &
          foj_record_count > 0 ~ "both_sources",
        geojapan_record_count > 0 ~ "GEOJAPAN_only",
        foj_record_count > 0 ~ "FOJ_only",
        TRUE ~ "unknown"
      )
    )
}

# ------------------------------------------------------------------------------
# Candidate string diagnostics
# ------------------------------------------------------------------------------

build_04d_string_diagnostics <- function(residual_candidates) {
  residual_candidates %>%
    dplyr::transmute(
      wcvp_candidate_id,
      submitted_name = clean_character_04d(
        wcvp_submitted_name
      ),
      submitted_word_count = stringr::str_count(
        submitted_name,
        "\\S+"
      ),
      has_hybrid_marker = stringr::str_detect(
        submitted_name,
        "(?i)(^|\\s)(×|x)(\\s|$)|×"
      ),
      has_rank_marker = stringr::str_detect(
        submitted_name,
        "(?i)(?<![[:alnum:]])(subsp\\.|ssp\\.|var\\.|f\\.|forma)(?=\\s)"
      ),
      has_parentheses = stringr::str_detect(
        submitted_name,
        "[()]"
      ),
      has_square_brackets = stringr::str_detect(
        submitted_name,
        "[\\[\\]]"
      ),
      has_question_mark = stringr::str_detect(
        submitted_name,
        "\\?"
      ),
      has_comma = stringr::str_detect(
        submitted_name,
        ","
      ),
      has_digits = stringr::str_detect(
        submitted_name,
        "[0-9]"
      ),
      has_apostrophe = stringr::str_detect(
        submitted_name,
        "['’]"
      ),
      has_period = stringr::str_detect(
        submitted_name,
        "\\."
      ),
      has_non_ascii = stringr::str_detect(
        submitted_name,
        "[^\\x01-\\x7F]"
      )
    )
}

# ------------------------------------------------------------------------------
# GEOJAPAN relationship evidence
# ------------------------------------------------------------------------------

build_04d_geojapan_relationship_evidence <- function(
    residual_candidates,
    source_crosswalk,
    geojapan_taxa
) {
  residual_geo <- source_crosswalk %>%
    dplyr::filter(
      wcvp_candidate_id %in%
        residual_candidates$wcvp_candidate_id,
      source_repository == "GEOJAPAN"
    ) %>%
    dplyr::transmute(
      wcvp_candidate_id,
      source_record_id,
      geojapan_spnumber = suppressWarnings(
        as.numeric(source_spnumber)
      )
    )
  
  if (nrow(residual_geo) == 0L) {
    return(
      tibble::tibble(
        wcvp_candidate_id = character(),
        geojapan_has_synof = logical(),
        geojapan_has_merge_to = logical(),
        geojapan_has_validlink = logical(),
        geojapan_synof_values = character(),
        geojapan_merge_to_values = character(),
        geojapan_validlink_values = character(),
        geojapan_taxstats = character()
      )
    )
  }
  
  fields <- list(
    spnumber = first_existing_field_04d(
      geojapan_taxa,
      c("geojapan_spnumber")
    ),
    synof = first_existing_field_04d(
      geojapan_taxa,
      c("geojapan_synof")
    ),
    merge_to = first_existing_field_04d(
      geojapan_taxa,
      c("geojapan_merge_to")
    ),
    validlink = first_existing_field_04d(
      geojapan_taxa,
      c("geojapan_validlink")
    ),
    taxstat = first_existing_field_04d(
      geojapan_taxa,
      c("geojapan_taxstat")
    )
  )
  
  if (is.na(fields$spnumber)) {
    stop(
      "Could not identify GEOJAPAN SPNUMBER field.",
      call. = FALSE
    )
  }
  
  geo_lookup <- tibble::tibble(
    geojapan_spnumber = suppressWarnings(
      as.numeric(geojapan_taxa[[fields$spnumber]])
    ),
    geojapan_synof = extract_optional_04d(
      geojapan_taxa,
      fields$synof
    ),
    geojapan_merge_to = extract_optional_04d(
      geojapan_taxa,
      fields$merge_to
    ),
    geojapan_validlink = extract_optional_04d(
      geojapan_taxa,
      fields$validlink
    ),
    geojapan_taxstat = extract_optional_04d(
      geojapan_taxa,
      fields$taxstat
    )
  )
  
  residual_geo %>%
    dplyr::left_join(
      geo_lookup,
      by = "geojapan_spnumber"
    ) %>%
    dplyr::group_by(wcvp_candidate_id) %>%
    dplyr::summarise(
      geojapan_has_synof = any(
        !is.na(geojapan_synof) &
          !geojapan_synof %in% c("0", "-9"),
        na.rm = TRUE
      ),
      geojapan_has_merge_to = any(
        !is.na(geojapan_merge_to) &
          !geojapan_merge_to %in% c("0", "-9"),
        na.rm = TRUE
      ),
      geojapan_has_validlink = any(
        !is.na(geojapan_validlink) &
          !geojapan_validlink %in% c("0", "-9"),
        na.rm = TRUE
      ),
      geojapan_synof_values = collapse_unique_04d(
        geojapan_synof
      ),
      geojapan_merge_to_values = collapse_unique_04d(
        geojapan_merge_to
      ),
      geojapan_validlink_values = collapse_unique_04d(
        geojapan_validlink
      ),
      geojapan_taxstats = collapse_unique_04d(
        geojapan_taxstat
      ),
      .groups = "drop"
    )
}

# ------------------------------------------------------------------------------
# 04b structured-recovery evidence
# ------------------------------------------------------------------------------

build_04d_recovery_evidence <- function(
    residual_candidates,
    recovery_resolution,
    recovery_alternatives
) {
  residual_ids <- residual_candidates$wcvp_candidate_id
  
  resolution <- recovery_resolution %>%
    dplyr::filter(
      wcvp_candidate_id %in% residual_ids
    ) %>%
    dplyr::transmute(
      wcvp_candidate_id = as.character(wcvp_candidate_id),
      recovery_status = clean_character_04d(
        recovery_status
      )
    )
  
  alternatives <- recovery_alternatives %>%
    dplyr::filter(
      wcvp_candidate_id %in% residual_ids
    ) %>%
    dplyr::group_by(wcvp_candidate_id) %>%
    dplyr::summarise(
      recovery_source_record_count = dplyr::n_distinct(
        source_record_id
      ),
      structured_alternative_count = dplyr::n_distinct(
        reconstructed_name[
          is_structured_alternative %in% TRUE
        ],
        na.rm = TRUE
      ),
      structured_alternatives = {
        vals <- sort(
          unique(
            reconstructed_name[
              is_structured_alternative %in% TRUE &
                !is.na(reconstructed_name)
            ]
          )
        )
        if (length(vals) == 0L) {
          NA_character_
        } else {
          paste(vals, collapse = " | ")
        }
      },
      .groups = "drop"
    )
  
  dplyr::full_join(
    resolution,
    alternatives,
    by = "wcvp_candidate_id"
  )
}

# ------------------------------------------------------------------------------
# Raw WCVP match diagnostics
# ------------------------------------------------------------------------------

detect_raw_match_fields_04d <- function(raw_matches) {
  list(
    candidate_id = first_existing_field_04d(
      raw_matches,
      c("wcvp_candidate_id")
    ),
    matched_id = first_existing_field_04d(
      raw_matches,
      c(
        "wcvp_id",
        "plant_name_id",
        "wcvp_matched_id",
        "wcvp_match_id"
      )
    ),
    matched_status = first_existing_field_04d(
      raw_matches,
      c(
        "taxon_status",
        "wcvp_status",
        "matched_wcvp_status",
        "wcvp_matched_status"
      )
    ),
    resolved_id = first_existing_field_04d(
      raw_matches,
      c(
        "wcvp_resolved_concept_id",
        "wcvp_concept_id",
        "resolved_wcvp_concept_id",
        "accepted_plant_name_id",
        "accepted_name_id"
      )
    ),
    resolved_status = first_existing_field_04d(
      raw_matches,
      c(
        "wcvp_concept_status",
        "resolved_wcvp_status",
        "accepted_status"
      )
    )
  )
}

build_04d_raw_match_evidence <- function(
    residual_candidates,
    candidate_resolution,
    raw_matches
) {
  residual_ids <- residual_candidates$wcvp_candidate_id
  
  base <- candidate_resolution %>%
    dplyr::filter(
      wcvp_candidate_id %in% residual_ids
    ) %>%
    dplyr::transmute(
      wcvp_candidate_id = as.character(wcvp_candidate_id),
      original_match_rows = wcvp_match_rows,
      matched_wcvp_records = matched_wcvp_records,
      resolvable_concept_count = resolvable_concept_count,
      non_resolvable_concept_count =
        non_resolvable_concept_count,
      matched_statuses = clean_character_04d(
        matched_statuses
      ),
      any_multiple_match_flag = any_multiple_match_flag,
      any_unplaced_match = any_unplaced_match
    )
  
  fields <- detect_raw_match_fields_04d(raw_matches)
  
  if (is.na(fields$candidate_id)) {
    stop(
      "Could not identify candidate ID in 04 raw matches.",
      call. = FALSE
    )
  }
  
  raw_subset <- raw_matches %>%
    dplyr::filter(
      .data[[fields$candidate_id]] %in% residual_ids
    )
  
  raw_summary <- raw_subset %>%
    dplyr::mutate(
      diagnostic_candidate_id = as.character(
        .data[[fields$candidate_id]]
      ),
      diagnostic_matched_id = if (
        is.na(fields$matched_id)
      ) {
        NA_character_
      } else {
        as.character(.data[[fields$matched_id]])
      },
      diagnostic_matched_status = if (
        is.na(fields$matched_status)
      ) {
        NA_character_
      } else {
        clean_character_04d(
          .data[[fields$matched_status]]
        )
      },
      diagnostic_resolved_id = if (
        is.na(fields$resolved_id)
      ) {
        NA_character_
      } else {
        as.character(.data[[fields$resolved_id]])
      },
      diagnostic_resolved_status = if (
        is.na(fields$resolved_status)
      ) {
        NA_character_
      } else {
        clean_character_04d(
          .data[[fields$resolved_status]]
        )
      }
    ) %>%
    dplyr::group_by(diagnostic_candidate_id) %>%
    dplyr::summarise(
      raw_match_row_count = dplyr::n(),
      raw_matched_id_count = dplyr::n_distinct(
        diagnostic_matched_id,
        na.rm = TRUE
      ),
      raw_resolved_id_count = dplyr::n_distinct(
        diagnostic_resolved_id,
        na.rm = TRUE
      ),
      raw_matched_statuses = collapse_unique_04d(
        diagnostic_matched_status
      ),
      raw_resolved_statuses = collapse_unique_04d(
        diagnostic_resolved_status
      ),
      .groups = "drop"
    ) %>%
    dplyr::rename(
      wcvp_candidate_id = diagnostic_candidate_id
    )
  
  base %>%
    dplyr::left_join(
      raw_summary,
      by = "wcvp_candidate_id"
    )
}

# ------------------------------------------------------------------------------
# Diagnostic classification
# ------------------------------------------------------------------------------

classify_04d_residuals <- function(data) {
  data %>%
    dplyr::mutate(
      diagnostic_class = dplyr::case_when(
        residual_status == "review" &
          any_unplaced_match %in% TRUE ~
          "review_unplaced",
        
        residual_status == "review" &
          non_resolvable_concept_count > 0 ~
          "review_non_resolvable",
        
        residual_status == "review" ~
          "review_other",
        
        residual_status == "ambiguous" &
          resolvable_concept_count > 1 ~
          "ambiguous_multiple_terminal_concepts",
        
        residual_status == "ambiguous" ~
          "ambiguous_other",
        
        residual_status == "unmatched" &
          recovery_status == "divergent_recovery" ~
          "unmatched_structured_divergent",
        
        residual_status == "unmatched" &
          recovery_status == "no_exact_recovery" ~
          "unmatched_structured_no_exact",
        
        residual_status == "unmatched" &
          source_class == "FOJ_only" ~
          "unmatched_foj_only",
        
        residual_status == "unmatched" &
          recovery_status == "no_structured_alternative" &
          has_hybrid_marker ~
          "unmatched_hybrid_notation",
        
        residual_status == "unmatched" &
          recovery_status == "no_structured_alternative" &
          (
            has_parentheses |
              has_square_brackets |
              has_question_mark |
              has_digits
          ) ~
          "unmatched_string_artefact",
        
        residual_status == "unmatched" &
          recovery_status == "no_structured_alternative" ~
          "unmatched_no_structured_alternative",
        
        residual_status == "unmatched" &
          is.na(recovery_status) ~
          "unmatched_no_geojapan_recovery_record",
        
        residual_status == "unmatched" ~
          "unmatched_other",
        
        TRUE ~
          "other"
      ),
      
      evidence_priority = dplyr::case_when(
        diagnostic_class == "review_unplaced" ~
          "manual_review",
        
        diagnostic_class == "review_non_resolvable" ~
          "manual_review",
        
        diagnostic_class == "review_other" ~
          "manual_review",
        
        diagnostic_class ==
          "ambiguous_multiple_terminal_concepts" ~
          "ambiguity_evidence",
        
        diagnostic_class == "ambiguous_other" ~
          "ambiguity_evidence",
        
        diagnostic_class ==
          "unmatched_structured_divergent" ~
          "ambiguity_evidence",
        
        diagnostic_class ==
          "unmatched_structured_no_exact" ~
          "deterministic_name_diagnostics",
        
        diagnostic_class == "unmatched_foj_only" ~
          "source_relationship_diagnostics",
        
        diagnostic_class ==
          "unmatched_hybrid_notation" ~
          "nomenclatural_normalisation",
        
        diagnostic_class ==
          "unmatched_string_artefact" ~
          "string_normalisation",
        
        diagnostic_class ==
          "unmatched_no_structured_alternative" ~
          "additional_deterministic_diagnostics",
        
        diagnostic_class ==
          "unmatched_no_geojapan_recovery_record" ~
          "source_relationship_diagnostics",
        
        TRUE ~
          "additional_deterministic_diagnostics"
      )
    )
}

# ------------------------------------------------------------------------------
# Build master residual diagnostic table
# ------------------------------------------------------------------------------

build_04d_master_table <- function(
    residual_candidates,
    source_evidence,
    string_diagnostics,
    relationship_evidence,
    recovery_evidence,
    raw_match_evidence
) {
  residual_candidates %>%
    dplyr::select(
      wcvp_candidate_id,
      wcvp_submitted_name,
      residual_status,
      original_04_status,
      recovery_status_04c = recovery_status
    ) %>%
    dplyr::left_join(
      source_evidence,
      by = "wcvp_candidate_id"
    ) %>%
    dplyr::left_join(
      string_diagnostics,
      by = "wcvp_candidate_id"
    ) %>%
    dplyr::left_join(
      relationship_evidence,
      by = "wcvp_candidate_id"
    ) %>%
    dplyr::left_join(
      recovery_evidence,
      by = "wcvp_candidate_id"
    ) %>%
    dplyr::left_join(
      raw_match_evidence,
      by = "wcvp_candidate_id"
    ) %>%
    dplyr::mutate(
      recovery_status = dplyr::coalesce(
        recovery_status,
        recovery_status_04c
      )
    ) %>%
    dplyr::select(
      -recovery_status_04c
    ) %>%
    classify_04d_residuals() %>%
    dplyr::arrange(
      factor(
        residual_status,
        levels = c(
          "review",
          "ambiguous",
          "unmatched"
        )
      ),
      diagnostic_class,
      wcvp_submitted_name
    )
}

# ------------------------------------------------------------------------------
# Diagnostic validation
# ------------------------------------------------------------------------------

validate_04d_master <- function(master) {
  message("Validating residual diagnostic table...")
  
  assert_equal_04d(
    nrow(master),
    EXPECTED_UNRESOLVED,
    "Residual diagnostic row count"
  )
  
  assert_equal_04d(
    dplyr::n_distinct(master$wcvp_candidate_id),
    EXPECTED_UNRESOLVED,
    "Distinct residual candidate IDs"
  )
  
  assert_equal_04d(
    sum(
      master$residual_status == "unmatched",
      na.rm = TRUE
    ),
    EXPECTED_UNMATCHED,
    "Residual unmatched count"
  )
  
  assert_equal_04d(
    sum(
      master$residual_status == "ambiguous",
      na.rm = TRUE
    ),
    EXPECTED_AMBIGUOUS,
    "Residual ambiguous count"
  )
  
  assert_equal_04d(
    sum(
      master$residual_status == "review",
      na.rm = TRUE
    ),
    EXPECTED_REVIEW,
    "Residual review count"
  )
  
  missing_class <- sum(
    is.na(master$diagnostic_class),
    na.rm = TRUE
  )
  
  assert_equal_04d(
    missing_class,
    0L,
    "Candidates without diagnostic class"
  )
  
  message("Residual diagnostic validation passed.")
  invisible(TRUE)
}

# ------------------------------------------------------------------------------
# Audits
# ------------------------------------------------------------------------------

build_04d_summary <- function(master) {
  tibble::tibble(
    metric = c(
      "total_residual_candidates",
      "unmatched",
      "ambiguous",
      "review",
      "geojapan_only",
      "foj_only",
      "both_sources",
      "with_geojapan_relationship_evidence",
      "with_structured_alternative",
      "with_hybrid_marker",
      "with_rank_marker"
    ),
    value = c(
      nrow(master),
      sum(master$residual_status == "unmatched", na.rm = TRUE),
      sum(master$residual_status == "ambiguous", na.rm = TRUE),
      sum(master$residual_status == "review", na.rm = TRUE),
      sum(master$source_class == "GEOJAPAN_only", na.rm = TRUE),
      sum(master$source_class == "FOJ_only", na.rm = TRUE),
      sum(master$source_class == "both_sources", na.rm = TRUE),
      sum(
        master$geojapan_has_synof %in% TRUE |
          master$geojapan_has_merge_to %in% TRUE |
          master$geojapan_has_validlink %in% TRUE,
        na.rm = TRUE
      ),
      sum(
        master$structured_alternative_count > 0,
        na.rm = TRUE
      ),
      sum(master$has_hybrid_marker, na.rm = TRUE),
      sum(master$has_rank_marker, na.rm = TRUE)
    )
  )
}

build_04d_class_audit <- function(master) {
  master %>%
    dplyr::count(
      residual_status,
      diagnostic_class,
      evidence_priority,
      name = "candidate_names",
      sort = TRUE
    )
}

build_04d_source_audit <- function(master) {
  master %>%
    dplyr::count(
      residual_status,
      source_class,
      name = "candidate_names"
    ) %>%
    dplyr::arrange(
      residual_status,
      dplyr::desc(candidate_names)
    )
}

build_04d_recovery_audit <- function(master) {
  master %>%
    dplyr::count(
      residual_status,
      recovery_status,
      name = "candidate_names"
    ) %>%
    dplyr::arrange(
      residual_status,
      dplyr::desc(candidate_names)
    )
}

build_04d_ambiguity_audit <- function(master) {
  master %>%
    dplyr::filter(
      residual_status == "ambiguous"
    ) %>%
    dplyr::count(
      original_match_rows,
      resolvable_concept_count,
      non_resolvable_concept_count,
      matched_statuses,
      name = "candidate_names",
      sort = TRUE
    )
}

build_04d_review_audit <- function(master) {
  master %>%
    dplyr::filter(
      residual_status == "review"
    ) %>%
    dplyr::select(
      wcvp_candidate_id,
      wcvp_submitted_name,
      diagnostic_class,
      source_class,
      original_match_rows,
      resolvable_concept_count,
      non_resolvable_concept_count,
      matched_statuses,
      any_unplaced_match,
      geojapan_has_synof,
      geojapan_has_merge_to,
      geojapan_has_validlink,
      source_fullnames
    ) %>%
    dplyr::arrange(
      diagnostic_class,
      wcvp_submitted_name
    )
}

build_04d_unmatched_audit <- function(master) {
  master %>%
    dplyr::filter(
      residual_status == "unmatched"
    ) %>%
    dplyr::count(
      diagnostic_class,
      source_class,
      recovery_status,
      name = "candidate_names",
      sort = TRUE
    )
}

# ------------------------------------------------------------------------------
# Output writer
# ------------------------------------------------------------------------------

write_04d_outputs <- function(master, audits) {
  data_dir <- here::here(
    "data", "interim", "taxonomy", "WCVP",
    "residual_diagnostics"
  )
  
  audit_dir <- here::here(
    "outputs", "tables", "taxonomy", "WCVP",
    "residual_diagnostics"
  )
  
  dir.create(
    data_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  dir.create(
    audit_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  saveRDS(
    master,
    file.path(
      data_dir,
      "vpjd_wcvp_residual_diagnostics.rds"
    )
  )
  
  readr::write_csv(
    master,
    file.path(
      data_dir,
      "vpjd_wcvp_residual_diagnostics.csv"
    )
  )
  
  readr::write_csv(
    audits$summary,
    file.path(
      audit_dir,
      "vpjd_wcvp_residual_summary.csv"
    )
  )
  
  readr::write_csv(
    audits$diagnostic_class,
    file.path(
      audit_dir,
      "vpjd_wcvp_residual_class_audit.csv"
    )
  )
  
  readr::write_csv(
    audits$source,
    file.path(
      audit_dir,
      "vpjd_wcvp_residual_source_audit.csv"
    )
  )
  
  readr::write_csv(
    audits$recovery,
    file.path(
      audit_dir,
      "vpjd_wcvp_residual_recovery_audit.csv"
    )
  )
  
  readr::write_csv(
    audits$ambiguity,
    file.path(
      audit_dir,
      "vpjd_wcvp_ambiguity_audit.csv"
    )
  )
  
  readr::write_csv(
    audits$review,
    file.path(
      audit_dir,
      "vpjd_wcvp_review_candidates.csv"
    )
  )
  
  readr::write_csv(
    audits$unmatched,
    file.path(
      audit_dir,
      "vpjd_wcvp_unmatched_audit.csv"
    )
  )
  
  invisible(
    list(
      data_dir = data_dir,
      audit_dir = audit_dir
    )
  )
}

# ------------------------------------------------------------------------------
# Main
# ------------------------------------------------------------------------------

diagnose_vpjd_wcvp_residuals <- function(
    write_outputs = TRUE
) {
  message("\nStarting VPJD WCVP residual diagnostics...")
  message(
    "Residual diagnostics module version: ",
    WCVP_RESIDUAL_DIAGNOSTICS_VERSION
  )
  
  candidates <- load_04d_consolidated_candidates()
  source_crosswalk <- load_04d_source_crosswalk()
  candidate_resolution <- load_04d_candidate_resolution()
  raw_matches <- load_04d_raw_matches()
  recovery_resolution <- load_04d_recovery_resolution()
  recovery_alternatives <- load_04d_recovery_alternatives()
  geojapan_taxa <- load_04d_geojapan_taxa()
  
  validate_04d_baseline(
    candidates,
    source_crosswalk
  )
  
  residual_candidates <- build_04d_residual_candidates(
    candidates
  )
  
  message(
    "Residual candidates: ",
    format(
      nrow(residual_candidates),
      big.mark = ","
    )
  )
  
  source_evidence <- build_04d_source_evidence(
    residual_candidates,
    source_crosswalk
  )
  
  string_diagnostics <- build_04d_string_diagnostics(
    residual_candidates
  )
  
  relationship_evidence <-
    build_04d_geojapan_relationship_evidence(
      residual_candidates,
      source_crosswalk,
      geojapan_taxa
    )
  
  recovery_evidence <- build_04d_recovery_evidence(
    residual_candidates,
    recovery_resolution,
    recovery_alternatives
  )
  
  raw_match_evidence <- build_04d_raw_match_evidence(
    residual_candidates,
    candidate_resolution,
    raw_matches
  )
  
  master <- build_04d_master_table(
    residual_candidates = residual_candidates,
    source_evidence = source_evidence,
    string_diagnostics = string_diagnostics,
    relationship_evidence = relationship_evidence,
    recovery_evidence = recovery_evidence,
    raw_match_evidence = raw_match_evidence
  )
  
  validate_04d_master(master)
  
  audits <- list(
    summary = build_04d_summary(master),
    diagnostic_class = build_04d_class_audit(master),
    source = build_04d_source_audit(master),
    recovery = build_04d_recovery_audit(master),
    ambiguity = build_04d_ambiguity_audit(master),
    review = build_04d_review_audit(master),
    unmatched = build_04d_unmatched_audit(master)
  )
  
  if (write_outputs) {
    paths <- write_04d_outputs(
      master,
      audits
    )
  } else {
    paths <- NULL
  }
  
  message("\nVPJD WCVP residual diagnostics complete.")
  
  message(
    "Residual candidates: ",
    format(nrow(master), big.mark = ",")
  )
  
  message(
    "Unmatched: ",
    format(
      sum(
        master$residual_status == "unmatched",
        na.rm = TRUE
      ),
      big.mark = ","
    )
  )
  
  message(
    "Ambiguous: ",
    format(
      sum(
        master$residual_status == "ambiguous",
        na.rm = TRUE
      ),
      big.mark = ","
    )
  )
  
  message(
    "Review: ",
    format(
      sum(
        master$residual_status == "review",
        na.rm = TRUE
      ),
      big.mark = ","
    )
  )
  
  list(
    residuals = master,
    audits = audits,
    paths = paths
  )
}

# ------------------------------------------------------------------------------
# Module load message
# ------------------------------------------------------------------------------

message(
  "04d_wcvp_residual_diagnostics.R v",
  WCVP_RESIDUAL_DIAGNOSTICS_VERSION,
  " loaded."
)

message(
  "Run diagnose_vpjd_wcvp_residuals() to classify the residual WCVP pool."
)