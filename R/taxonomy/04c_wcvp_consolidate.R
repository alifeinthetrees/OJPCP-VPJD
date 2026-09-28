# ==============================================================================
# 04c_wcvp_consolidate.R
#
# Oxford–Japan Plant Conservation Partnership (OJPCP)
# Vascular Plants of Japan Database (VPJD)
#
# Purpose:
#   Consolidate the validated deterministic WCVP reconciliation layers:
#     04_wcvp_standardise.R v0.3.3
#     04b_wcvp_recovery.R   v0.1.1
#
#   into a single authoritative deterministic reconciliation table.
#
# Principles:
#   - Performs NO new taxonomic matching.
#   - Performs NO fuzzy matching.
#   - Does NOT modify 04 or 04b outputs.
#   - Uses the validated 04 candidate-resolution table directly.
#   - Existing 04 resolutions are preserved exactly.
#   - Only validated 04b recoverable/convergent results supplement 04.
#   - Ambiguous, review and divergent cases remain unresolved.
#
# Expected validated totals:
#   Candidate names                         32,292
#   04 Accepted                            25,309
#   04 Artificial Hybrid                       26
#   04b additional Accepted                 1,275
#   Final deterministic concepts           26,610
#   Remaining unmatched                     3,703
#   Remaining ambiguous                     1,928
#   Remaining review                           51
#   Total unresolved                        5,682
#   Valid source records                   40,891
#
# Version: 0.1.2
# ==============================================================================

WCVP_CONSOLIDATE_VERSION <- "0.1.2"

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
    paste(missing_packages, collapse = ", ")
  )
}

# ------------------------------------------------------------------------------
# Validated expectations
# ------------------------------------------------------------------------------

EXPECTED_CANDIDATES <- 32292L
EXPECTED_SOURCE_RECORDS <- 40891L
EXPECTED_04_ACCEPTED <- 25309L
EXPECTED_04_HYBRID <- 26L
EXPECTED_04B_RECOVERED <- 1275L
EXPECTED_04B_DIVERGENT <- 2L
EXPECTED_FINAL_ACCEPTED <- 26584L
EXPECTED_FINAL_HYBRID <- 26L
EXPECTED_FINAL_RESOLVED <- 26610L
EXPECTED_FINAL_UNMATCHED <- 3703L
EXPECTED_FINAL_AMBIGUOUS <- 1928L
EXPECTED_FINAL_REVIEW <- 51L
EXPECTED_FINAL_UNRESOLVED <- 5682L

# ------------------------------------------------------------------------------
# Helpers
# ------------------------------------------------------------------------------

clean_character_04c <- function(x) {
  x <- as.character(x)
  x <- stringr::str_squish(x)
  
  x[
    is.na(x) |
      x == "" |
      toupper(x) %in% c("NA", "N/A", "NULL")
  ] <- NA_character_
  
  x
}

assert_equal_04c <- function(observed, expected, label) {
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

# ------------------------------------------------------------------------------
# Input loaders
# ------------------------------------------------------------------------------

load_04_candidate_resolution <- function() {
  path <- here::here(
    "data", "interim", "taxonomy", "WCVP",
    "vpjd_wcvp_candidate_resolution.rds"
  )
  
  if (!file.exists(path)) {
    stop("04 candidate-resolution file not found: ", path)
  }
  
  x <- readRDS(path)
  
  required <- c(
    "wcvp_candidate_id",
    "wcvp_submitted_name",
    "wcvp_resolved_concept_id",
    "wcvp_concept_name",
    "wcvp_concept_authors",
    "wcvp_concept_family",
    "wcvp_concept_genus",
    "wcvp_concept_species",
    "wcvp_concept_rank",
    "wcvp_concept_status",
    "wcvp_resolution_method",
    "wcvp_reconciliation_status"
  )
  
  missing <- setdiff(required, names(x))
  
  if (length(missing) > 0) {
    stop(
      "Required 04 candidate-resolution fields missing: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  
  x
}

load_04b_recovery_resolution <- function() {
  path <- here::here(
    "data", "interim", "taxonomy", "WCVP", "recovery",
    "vpjd_wcvp_recovery_resolution.rds"
  )
  
  if (!file.exists(path)) {
    stop("04b recovery-resolution file not found: ", path)
  }
  
  x <- readRDS(path)
  
  required <- c(
    "wcvp_candidate_id",
    "original_submitted_name",
    "recovery_status",
    "recovered_concept_id",
    "wcvp_concept_name",
    "wcvp_concept_authors",
    "wcvp_concept_family",
    "wcvp_concept_genus",
    "wcvp_concept_species",
    "wcvp_concept_rank",
    "wcvp_concept_status"
  )
  
  missing <- setdiff(required, names(x))
  
  if (length(missing) > 0) {
    stop(
      "Required 04b recovery fields missing: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  
  x
}

load_04b_recovery_alternatives <- function() {
  path <- here::here(
    "data", "interim", "taxonomy", "WCVP", "recovery",
    "vpjd_wcvp_recovery_alternatives.rds"
  )
  
  if (!file.exists(path)) {
    stop("04b recovery-alternatives file not found: ", path)
  }
  
  x <- readRDS(path)
  
  required <- c(
    "wcvp_candidate_id",
    "source_record_id",
    "reconstructed_name",
    "is_structured_alternative"
  )
  
  missing <- setdiff(required, names(x))
  
  if (length(missing) > 0) {
    stop(
      "Required 04b alternative fields missing: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  
  x
}

load_source_candidate_crosswalk_04c <- function() {
  path <- here::here(
    "data", "interim", "taxonomy",
    "source_to_wcvp_candidate.rds"
  )
  
  if (!file.exists(path)) {
    stop("Source-to-WCVP-candidate crosswalk not found: ", path)
  }
  
  x <- readRDS(path)
  
  required <- c(
    "source_record_id",
    "source_repository",
    "source_dataset",
    "source_spnumber",
    "source_fullname",
    "source_matching_name",
    "matching_name_method",
    "wcvp_candidate_id",
    "wcvp_submitted_name"
  )
  
  missing <- setdiff(required, names(x))
  
  if (length(missing) > 0) {
    stop(
      "Required source crosswalk fields missing: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  
  x
}

# ------------------------------------------------------------------------------
# Prepare validated 04 layer
# ------------------------------------------------------------------------------

prepare_04_candidate_layer <- function(candidate_resolution) {
  if (anyDuplicated(candidate_resolution$wcvp_candidate_id) > 0) {
    stop(
      "04 candidate-resolution table contains duplicate candidate IDs.",
      call. = FALSE
    )
  }
  
  candidate_resolution %>%
    dplyr::transmute(
      wcvp_candidate_id = as.character(wcvp_candidate_id),
      wcvp_submitted_name = clean_character_04c(wcvp_submitted_name),
      
      original_04_status = clean_character_04c(
        wcvp_reconciliation_status
      ),
      
      original_04_resolution_method = clean_character_04c(
        wcvp_resolution_method
      ),
      
      concept_04_id = as.character(
        wcvp_resolved_concept_id
      ),
      
      concept_04_name = clean_character_04c(
        wcvp_concept_name
      ),
      
      concept_04_authors = clean_character_04c(
        wcvp_concept_authors
      ),
      
      concept_04_family = clean_character_04c(
        wcvp_concept_family
      ),
      
      concept_04_genus = clean_character_04c(
        wcvp_concept_genus
      ),
      
      concept_04_species = clean_character_04c(
        wcvp_concept_species
      ),
      
      concept_04_rank = clean_character_04c(
        wcvp_concept_rank
      ),
      
      concept_04_status = clean_character_04c(
        wcvp_concept_status
      )
    )
}

# ------------------------------------------------------------------------------
# Summarise 04b alternatives
# ------------------------------------------------------------------------------

summarise_04b_alternatives <- function(alternatives) {
  alternatives %>%
    dplyr::group_by(wcvp_candidate_id) %>%
    dplyr::summarise(
      recovery_source_records = dplyr::n_distinct(source_record_id),
      
      reconstructed_name_count = dplyr::n_distinct(
        reconstructed_name[
          is_structured_alternative %in% TRUE
        ],
        na.rm = TRUE
      ),
      
      reconstructed_names = {
        vals <- sort(
          unique(
            reconstructed_name[
              is_structured_alternative %in% TRUE &
                !is.na(reconstructed_name)
            ]
          )
        )
        
        if (length(vals) == 0) {
          NA_character_
        } else {
          paste(vals, collapse = " | ")
        }
      },
      
      .groups = "drop"
    )
}

# ------------------------------------------------------------------------------
# Prepare validated 04b layer
# ------------------------------------------------------------------------------

prepare_04b_layer <- function(
    recovery_resolution,
    recovery_alternatives
) {
  if (anyDuplicated(recovery_resolution$wcvp_candidate_id) > 0) {
    stop(
      "04b recovery-resolution table contains duplicate candidate IDs.",
      call. = FALSE
    )
  }
  
  alternative_summary <- summarise_04b_alternatives(
    recovery_alternatives
  )
  
  recovery_resolution %>%
    dplyr::transmute(
      wcvp_candidate_id = as.character(wcvp_candidate_id),
      recovery_attempted = TRUE,
      recovery_status = clean_character_04c(recovery_status),
      
      recovery_concept_id = as.character(
        recovered_concept_id
      ),
      
      recovery_concept_name = clean_character_04c(
        wcvp_concept_name
      ),
      
      recovery_concept_authors = clean_character_04c(
        wcvp_concept_authors
      ),
      
      recovery_concept_family = clean_character_04c(
        wcvp_concept_family
      ),
      
      recovery_concept_genus = clean_character_04c(
        wcvp_concept_genus
      ),
      
      recovery_concept_species = clean_character_04c(
        wcvp_concept_species
      ),
      
      recovery_concept_rank = clean_character_04c(
        wcvp_concept_rank
      ),
      
      recovery_concept_status = clean_character_04c(
        wcvp_concept_status
      )
    ) %>%
    dplyr::left_join(
      alternative_summary,
      by = "wcvp_candidate_id"
    )
}

# ------------------------------------------------------------------------------
# Consolidate candidate-level reconciliation
# ------------------------------------------------------------------------------

build_consolidated_candidate_reconciliation <- function(
    layer_04,
    layer_04b
) {
  layer_04 %>%
    dplyr::left_join(
      layer_04b,
      by = "wcvp_candidate_id"
    ) %>%
    dplyr::mutate(
      recovery_attempted = tidyr::replace_na(
        recovery_attempted,
        FALSE
      ),
      
      is_04_resolved = original_04_status %in% c(
        "resolved",
        "resolved_hybrid"
      ),
      
      is_04b_resolved =
        !is_04_resolved &
        recovery_status %in% c(
          "recoverable",
          "convergent_recovery"
        ),
      
      final_resolution_stage = dplyr::case_when(
        is_04_resolved ~ "04",
        is_04b_resolved ~ "04b",
        TRUE ~ NA_character_
      ),
      
      final_resolution_method = dplyr::case_when(
        is_04_resolved ~ paste0(
          "04_",
          original_04_resolution_method
        ),
        
        recovery_status == "recoverable" &
          is_04b_resolved ~
          "04b_structured_exact",
        
        recovery_status == "convergent_recovery" &
          is_04b_resolved ~
          "04b_structured_exact_convergent",
        
        TRUE ~ NA_character_
      ),
      
      final_reconciliation_status = dplyr::case_when(
        original_04_status == "resolved" ~
          "resolved",
        
        original_04_status == "resolved_hybrid" ~
          "resolved_hybrid",
        
        is_04b_resolved ~
          "resolved",
        
        original_04_status == "ambiguous" ~
          "ambiguous",
        
        original_04_status == "review" ~
          "review",
        
        original_04_status == "unmatched" ~
          "unmatched",
        
        TRUE ~
          "review"
      ),
      
      final_wcvp_concept_id = dplyr::case_when(
        is_04_resolved ~ concept_04_id,
        is_04b_resolved ~ recovery_concept_id,
        TRUE ~ NA_character_
      ),
      
      final_wcvp_concept_name = dplyr::case_when(
        is_04_resolved ~ concept_04_name,
        is_04b_resolved ~ recovery_concept_name,
        TRUE ~ NA_character_
      ),
      
      final_wcvp_concept_authors = dplyr::case_when(
        is_04_resolved ~ concept_04_authors,
        is_04b_resolved ~ recovery_concept_authors,
        TRUE ~ NA_character_
      ),
      
      final_wcvp_concept_family = dplyr::case_when(
        is_04_resolved ~ concept_04_family,
        is_04b_resolved ~ recovery_concept_family,
        TRUE ~ NA_character_
      ),
      
      final_wcvp_concept_genus = dplyr::case_when(
        is_04_resolved ~ concept_04_genus,
        is_04b_resolved ~ recovery_concept_genus,
        TRUE ~ NA_character_
      ),
      
      final_wcvp_concept_species = dplyr::case_when(
        is_04_resolved ~ concept_04_species,
        is_04b_resolved ~ recovery_concept_species,
        TRUE ~ NA_character_
      ),
      
      final_wcvp_concept_rank = dplyr::case_when(
        is_04_resolved ~ concept_04_rank,
        is_04b_resolved ~ recovery_concept_rank,
        TRUE ~ NA_character_
      ),
      
      final_wcvp_concept_status = dplyr::case_when(
        is_04_resolved ~ concept_04_status,
        is_04b_resolved ~ recovery_concept_status,
        TRUE ~ NA_character_
      )
    ) %>%
    dplyr::select(
      wcvp_candidate_id,
      wcvp_submitted_name,
      original_04_status,
      original_04_resolution_method,
      recovery_attempted,
      recovery_status,
      recovery_source_records,
      reconstructed_name_count,
      reconstructed_names,
      final_reconciliation_status,
      final_resolution_stage,
      final_resolution_method,
      final_wcvp_concept_id,
      final_wcvp_concept_name,
      final_wcvp_concept_authors,
      final_wcvp_concept_family,
      final_wcvp_concept_genus,
      final_wcvp_concept_species,
      final_wcvp_concept_rank,
      final_wcvp_concept_status
    )
}

# ------------------------------------------------------------------------------
# Candidate validation
# ------------------------------------------------------------------------------

validate_consolidated_candidates <- function(candidates) {
  message("Validating consolidated candidate reconciliation...")
  
  assert_equal_04c(
    nrow(candidates),
    EXPECTED_CANDIDATES,
    "Candidate-row count"
  )
  
  assert_equal_04c(
    dplyr::n_distinct(candidates$wcvp_candidate_id),
    EXPECTED_CANDIDATES,
    "Distinct candidate IDs"
  )
  
  if (anyDuplicated(candidates$wcvp_candidate_id) > 0) {
    stop("Duplicate candidate IDs detected.", call. = FALSE)
  }
  
  observed_04_accepted <- sum(
    candidates$original_04_status == "resolved",
    na.rm = TRUE
  )
  
  observed_04_hybrid <- sum(
    candidates$original_04_status == "resolved_hybrid",
    na.rm = TRUE
  )
  
  assert_equal_04c(
    observed_04_accepted,
    EXPECTED_04_ACCEPTED,
    "Original 04 Accepted count"
  )
  
  assert_equal_04c(
    observed_04_hybrid,
    EXPECTED_04_HYBRID,
    "Original 04 Artificial Hybrid count"
  )
  
  observed_04b_recovered <- sum(
    candidates$final_resolution_stage == "04b",
    na.rm = TRUE
  )
  
  assert_equal_04c(
    observed_04b_recovered,
    EXPECTED_04B_RECOVERED,
    "04b recovered count"
  )
  
  observed_divergent <- sum(
    candidates$recovery_status == "divergent_recovery",
    na.rm = TRUE
  )
  
  assert_equal_04c(
    observed_divergent,
    EXPECTED_04B_DIVERGENT,
    "04b divergent count"
  )
  
  observed_resolved <- sum(
    candidates$final_reconciliation_status %in% c(
      "resolved",
      "resolved_hybrid"
    ),
    na.rm = TRUE
  )
  
  observed_unmatched <- sum(
    candidates$final_reconciliation_status == "unmatched",
    na.rm = TRUE
  )
  
  observed_ambiguous <- sum(
    candidates$final_reconciliation_status == "ambiguous",
    na.rm = TRUE
  )
  
  observed_review <- sum(
    candidates$final_reconciliation_status == "review",
    na.rm = TRUE
  )
  
  observed_unresolved <-
    observed_unmatched +
    observed_ambiguous +
    observed_review
  
  assert_equal_04c(
    observed_resolved,
    EXPECTED_FINAL_RESOLVED,
    "Final deterministic resolved count"
  )
  
  assert_equal_04c(
    observed_unmatched,
    EXPECTED_FINAL_UNMATCHED,
    "Final unmatched count"
  )
  
  assert_equal_04c(
    observed_ambiguous,
    EXPECTED_FINAL_AMBIGUOUS,
    "Final ambiguous count"
  )
  
  assert_equal_04c(
    observed_review,
    EXPECTED_FINAL_REVIEW,
    "Final review count"
  )
  
  assert_equal_04c(
    observed_unresolved,
    EXPECTED_FINAL_UNRESOLVED,
    "Final unresolved count"
  )
  
  observed_final_accepted <- sum(
    candidates$final_wcvp_concept_status == "Accepted",
    na.rm = TRUE
  )
  
  observed_final_hybrid <- sum(
    candidates$final_wcvp_concept_status == "Artificial Hybrid",
    na.rm = TRUE
  )
  
  assert_equal_04c(
    observed_final_accepted,
    EXPECTED_FINAL_ACCEPTED,
    "Final Accepted concept count"
  )
  
  assert_equal_04c(
    observed_final_hybrid,
    EXPECTED_FINAL_HYBRID,
    "Final Artificial Hybrid concept count"
  )
  
  bad_resolved <- candidates %>%
    dplyr::filter(
      final_reconciliation_status %in% c(
        "resolved",
        "resolved_hybrid"
      ),
      is.na(final_wcvp_concept_id)
    )
  
  if (nrow(bad_resolved) > 0) {
    stop(
      "Resolved candidates lacking final WCVP concept ID: ",
      nrow(bad_resolved),
      call. = FALSE
    )
  }
  
  bad_unresolved <- candidates %>%
    dplyr::filter(
      final_reconciliation_status %in% c(
        "unmatched",
        "ambiguous",
        "review"
      ),
      !is.na(final_wcvp_concept_id)
    )
  
  if (nrow(bad_unresolved) > 0) {
    stop(
      "Unresolved candidates unexpectedly have final WCVP concepts: ",
      nrow(bad_unresolved),
      call. = FALSE
    )
  }
  
  overwritten_04 <- candidates %>%
    dplyr::filter(
      original_04_status %in% c(
        "resolved",
        "resolved_hybrid"
      ),
      final_resolution_stage != "04"
    )
  
  if (nrow(overwritten_04) > 0) {
    stop(
      "04 resolutions were overwritten downstream: ",
      nrow(overwritten_04),
      call. = FALSE
    )
  }
  
  invalid_04b <- candidates %>%
    dplyr::filter(
      final_resolution_stage == "04b",
      original_04_status != "unmatched"
    )
  
  if (nrow(invalid_04b) > 0) {
    stop(
      "04b resolved candidates not originally unmatched: ",
      nrow(invalid_04b),
      call. = FALSE
    )
  }
  
  bad_divergent <- candidates %>%
    dplyr::filter(
      recovery_status == "divergent_recovery",
      !is.na(final_wcvp_concept_id)
    )
  
  if (nrow(bad_divergent) > 0) {
    stop(
      "Divergent recovery cases received final concepts.",
      call. = FALSE
    )
  }
  
  message("Candidate reconciliation validation passed.")
  invisible(TRUE)
}

# ------------------------------------------------------------------------------
# Source-record crosswalk
# ------------------------------------------------------------------------------

build_final_source_crosswalk <- function(
    source_crosswalk,
    candidates
) {
  candidate_fields <- candidates %>%
    dplyr::select(
      wcvp_candidate_id,
      original_04_status,
      original_04_resolution_method,
      recovery_attempted,
      recovery_status,
      reconstructed_names,
      final_reconciliation_status,
      final_resolution_stage,
      final_resolution_method,
      final_wcvp_concept_id,
      final_wcvp_concept_name,
      final_wcvp_concept_authors,
      final_wcvp_concept_family,
      final_wcvp_concept_genus,
      final_wcvp_concept_species,
      final_wcvp_concept_rank,
      final_wcvp_concept_status
    )
  
  source_crosswalk %>%
    dplyr::mutate(
      wcvp_candidate_id = as.character(wcvp_candidate_id)
    ) %>%
    dplyr::left_join(
      candidate_fields,
      by = "wcvp_candidate_id"
    )
}

validate_final_source_crosswalk <- function(source_final) {
  message("Validating final source-record crosswalk...")
  
  assert_equal_04c(
    nrow(source_final),
    EXPECTED_SOURCE_RECORDS,
    "Final source-record count"
  )
  
  assert_equal_04c(
    dplyr::n_distinct(source_final$source_record_id),
    EXPECTED_SOURCE_RECORDS,
    "Distinct source-record IDs"
  )
  
  missing_candidate <- source_final %>%
    dplyr::filter(is.na(wcvp_candidate_id))
  
  if (nrow(missing_candidate) > 0) {
    stop(
      "Source records without candidate IDs: ",
      nrow(missing_candidate),
      call. = FALSE
    )
  }
  
  missing_final_status <- source_final %>%
    dplyr::filter(is.na(final_reconciliation_status))
  
  if (nrow(missing_final_status) > 0) {
    stop(
      "Source records without final reconciliation status: ",
      nrow(missing_final_status),
      call. = FALSE
    )
  }
  
  message("Source-record crosswalk validation passed.")
  invisible(TRUE)
}

# ------------------------------------------------------------------------------
# Audits
# ------------------------------------------------------------------------------

build_04c_transition_audit <- function(candidates) {
  candidates %>%
    dplyr::mutate(
      consolidation_route = dplyr::case_when(
        final_resolution_stage == "04" ~
          "retained_04_resolution",
        final_resolution_stage == "04b" ~
          "resolved_via_04b",
        TRUE ~
          "remains_unresolved"
      )
    ) %>%
    dplyr::count(
      original_04_status,
      final_reconciliation_status,
      consolidation_route,
      name = "candidate_names"
    ) %>%
    dplyr::arrange(
      original_04_status,
      final_reconciliation_status,
      consolidation_route
    )
}

build_04c_final_status_audit <- function(candidates) {
  candidates %>%
    dplyr::count(
      final_reconciliation_status,
      name = "candidate_names",
      sort = TRUE
    ) %>%
    dplyr::mutate(
      percent = round(
        100 * candidate_names / sum(candidate_names),
        2
      )
    )
}

build_04c_resolution_stage_audit <- function(candidates) {
  candidates %>%
    dplyr::mutate(
      final_resolution_stage = tidyr::replace_na(
        final_resolution_stage,
        "unresolved"
      ),
      final_resolution_method = tidyr::replace_na(
        final_resolution_method,
        "unresolved"
      )
    ) %>%
    dplyr::count(
      final_resolution_stage,
      final_resolution_method,
      name = "candidate_names",
      sort = TRUE
    )
}

build_04c_concept_status_audit <- function(candidates) {
  candidates %>%
    dplyr::filter(!is.na(final_wcvp_concept_id)) %>%
    dplyr::count(
      final_wcvp_concept_status,
      name = "candidate_names",
      sort = TRUE
    )
}

build_04c_summary <- function(candidates, source_final) {
  resolved <- sum(
    candidates$final_reconciliation_status %in% c(
      "resolved",
      "resolved_hybrid"
    ),
    na.rm = TRUE
  )
  
  unresolved <- nrow(candidates) - resolved
  
  tibble::tibble(
    metric = c(
      "candidate_names",
      "valid_source_records",
      "resolved_by_04",
      "recovered_by_04b",
      "final_deterministic_resolved",
      "final_accepted",
      "final_artificial_hybrid",
      "remaining_unmatched",
      "remaining_ambiguous",
      "remaining_review",
      "total_unresolved",
      "deterministic_resolution_percent"
    ),
    
    value = c(
      nrow(candidates),
      nrow(source_final),
      
      sum(
        candidates$final_resolution_stage == "04",
        na.rm = TRUE
      ),
      
      sum(
        candidates$final_resolution_stage == "04b",
        na.rm = TRUE
      ),
      
      resolved,
      
      sum(
        candidates$final_wcvp_concept_status == "Accepted",
        na.rm = TRUE
      ),
      
      sum(
        candidates$final_wcvp_concept_status == "Artificial Hybrid",
        na.rm = TRUE
      ),
      
      sum(
        candidates$final_reconciliation_status == "unmatched",
        na.rm = TRUE
      ),
      
      sum(
        candidates$final_reconciliation_status == "ambiguous",
        na.rm = TRUE
      ),
      
      sum(
        candidates$final_reconciliation_status == "review",
        na.rm = TRUE
      ),
      
      unresolved,
      
      round(
        100 * resolved / nrow(candidates),
        2
      )
    )
  )
}

# ------------------------------------------------------------------------------
# Output writer
# ------------------------------------------------------------------------------

write_04c_outputs <- function(
    candidates,
    source_final,
    audits
) {
  data_dir <- here::here(
    "data", "interim", "taxonomy", "WCVP", "consolidated"
  )
  
  audit_dir <- here::here(
    "outputs", "tables", "taxonomy", "WCVP", "consolidated"
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
    candidates,
    file.path(
      data_dir,
      "vpjd_wcvp_consolidated_candidates.rds"
    )
  )
  
  readr::write_csv(
    candidates,
    file.path(
      data_dir,
      "vpjd_wcvp_consolidated_candidates.csv"
    )
  )
  
  saveRDS(
    source_final,
    file.path(
      data_dir,
      "source_to_wcvp_consolidated.rds"
    )
  )
  
  readr::write_csv(
    source_final,
    file.path(
      data_dir,
      "source_to_wcvp_consolidated.csv"
    )
  )
  
  readr::write_csv(
    audits$summary,
    file.path(
      audit_dir,
      "vpjd_wcvp_consolidated_summary.csv"
    )
  )
  
  readr::write_csv(
    audits$transition,
    file.path(
      audit_dir,
      "vpjd_wcvp_consolidated_transition_audit.csv"
    )
  )
  
  readr::write_csv(
    audits$final_status,
    file.path(
      audit_dir,
      "vpjd_wcvp_consolidated_status_audit.csv"
    )
  )
  
  readr::write_csv(
    audits$resolution_stage,
    file.path(
      audit_dir,
      "vpjd_wcvp_consolidated_resolution_stage_audit.csv"
    )
  )
  
  readr::write_csv(
    audits$concept_status,
    file.path(
      audit_dir,
      "vpjd_wcvp_consolidated_concept_status_audit.csv"
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

consolidate_vpjd_wcvp <- function(write_outputs = TRUE) {
  message("\nStarting VPJD deterministic WCVP consolidation...")
  message("Consolidation module version: ", WCVP_CONSOLIDATE_VERSION)
  
  candidate_resolution <- load_04_candidate_resolution()
  recovery_resolution <- load_04b_recovery_resolution()
  recovery_alternatives <- load_04b_recovery_alternatives()
  source_crosswalk <- load_source_candidate_crosswalk_04c()
  
  message(
    "04 candidate names loaded: ",
    format(nrow(candidate_resolution), big.mark = ",")
  )
  
  message(
    "04b candidate recovery rows loaded: ",
    format(nrow(recovery_resolution), big.mark = ",")
  )
  
  message(
    "Source records loaded: ",
    format(nrow(source_crosswalk), big.mark = ",")
  )
  
  layer_04 <- prepare_04_candidate_layer(
    candidate_resolution
  )
  
  layer_04b <- prepare_04b_layer(
    recovery_resolution = recovery_resolution,
    recovery_alternatives = recovery_alternatives
  )
  
  candidates <- build_consolidated_candidate_reconciliation(
    layer_04 = layer_04,
    layer_04b = layer_04b
  )
  
  validate_consolidated_candidates(candidates)
  
  source_final <- build_final_source_crosswalk(
    source_crosswalk = source_crosswalk,
    candidates = candidates
  )
  
  validate_final_source_crosswalk(source_final)
  
  audits <- list(
    summary = build_04c_summary(
      candidates = candidates,
      source_final = source_final
    ),
    
    transition = build_04c_transition_audit(
      candidates
    ),
    
    final_status = build_04c_final_status_audit(
      candidates
    ),
    
    resolution_stage = build_04c_resolution_stage_audit(
      candidates
    ),
    
    concept_status = build_04c_concept_status_audit(
      candidates
    )
  )
  
  if (write_outputs) {
    paths <- write_04c_outputs(
      candidates = candidates,
      source_final = source_final,
      audits = audits
    )
  } else {
    paths <- NULL
  }
  
  message("\nVPJD deterministic WCVP consolidation complete.")
  
  message(
    "Candidate names: ",
    format(nrow(candidates), big.mark = ",")
  )
  
  message(
    "Resolved by 04: ",
    format(
      sum(
        candidates$final_resolution_stage == "04",
        na.rm = TRUE
      ),
      big.mark = ","
    )
  )
  
  message(
    "Recovered by 04b: ",
    format(
      sum(
        candidates$final_resolution_stage == "04b",
        na.rm = TRUE
      ),
      big.mark = ","
    )
  )
  
  message(
    "Final deterministic concepts: ",
    format(
      sum(!is.na(candidates$final_wcvp_concept_id)),
      big.mark = ","
    )
  )
  
  message(
    "Remaining unmatched: ",
    format(
      sum(
        candidates$final_reconciliation_status == "unmatched",
        na.rm = TRUE
      ),
      big.mark = ","
    )
  )
  
  message(
    "Remaining ambiguous: ",
    format(
      sum(
        candidates$final_reconciliation_status == "ambiguous",
        na.rm = TRUE
      ),
      big.mark = ","
    )
  )
  
  message(
    "Remaining review: ",
    format(
      sum(
        candidates$final_reconciliation_status == "review",
        na.rm = TRUE
      ),
      big.mark = ","
    )
  )
  
  message(
    "Source records mapped: ",
    format(nrow(source_final), big.mark = ",")
  )
  
  list(
    candidates = candidates,
    source_crosswalk = source_final,
    audits = audits,
    paths = paths
  )
}

# ------------------------------------------------------------------------------
# Module load message
# ------------------------------------------------------------------------------

message(
  "04c_wcvp_consolidate.R v",
  WCVP_CONSOLIDATE_VERSION,
  " loaded."
)

message(
  "Run consolidate_vpjd_wcvp() to build the deterministic WCVP consolidation."
)