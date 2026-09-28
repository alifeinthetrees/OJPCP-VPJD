# ==============================================================================
# 05_build_vpjd_backbone.R
# Oxford–Japan Plant Conservation Partnership (OJPCP)
# Vascular Plants of Japan Database (VPJD)
#
# Purpose:
#   Build the working VPJD vascular-plant backbone from the validated
#   deterministic WCVP reconciliation.
#
# Upstream validated modules:
#   03   source taxon universe          v0.2.0
#   04   WCVP standardisation           v0.3.3
#   04b  structured-name recovery       v0.1.1
#   04c  deterministic consolidation    v0.1.2
#   04d  residual diagnostics           v0.1.0
#   04e  deterministic SYNOF recovery   v0.1.4
#
# Principles:
#   - No additional taxonomic matching.
#   - No fuzzy matching.
#   - Frozen 04c results are retained.
#   - Only validated 04e GEOJAPAN SYNOF recoveries may augment 04c.
#   - One backbone row per terminal WCVP concept.
#   - WCVP defines the working vascular-plant scope.
#   - Unresolved candidates are retained separately and are not assumed
#     to be non-vascular.
#
# Version: 0.1.1
# ==============================================================================

VPJD_BACKBONE_VERSION <- "0.1.1"

# ------------------------------------------------------------------------------
# Packages
# ------------------------------------------------------------------------------

required_packages <- c(
  "dplyr",
  "stringr",
  "readr",
  "tibble",
  "here"
)

missing_packages <- required_packages[
  !vapply(
    required_packages,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )
]

if (length(missing_packages) > 0L) {
  stop(
    "Missing required packages: ",
    paste(missing_packages, collapse = ", "),
    call. = FALSE
  )
}

# ------------------------------------------------------------------------------
# Frozen regression expectations
# ------------------------------------------------------------------------------

EXPECTED_TOTAL_CANDIDATES <- 32292L
EXPECTED_SOURCE_RECORDS <- 40891L
EXPECTED_04C_RESOLVED <- 26610L
EXPECTED_04E_RECOVERED <- 3153L
EXPECTED_FINAL_RESOLVED <- 29763L
EXPECTED_FINAL_UNRESOLVED <- 2529L
EXPECTED_FINAL_UNMATCHED <- 1397L
EXPECTED_FINAL_AMBIGUOUS <- 1098L
EXPECTED_FINAL_REVIEW <- 34L

# ------------------------------------------------------------------------------
# Helpers
# ------------------------------------------------------------------------------

clean_character_05 <- function(x) {
  x <- as.character(x)
  x <- stringr::str_squish(x)
  x[
    is.na(x) |
      x == "" |
      toupper(x) %in% c("NA", "N/A", "NULL")
  ] <- NA_character_
  x
}

assert_equal_05 <- function(observed, expected, label) {
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

collapse_unique_05 <- function(x, sep = " | ") {
  x <- clean_character_05(x)
  x <- sort(unique(x[!is.na(x)]))
  
  if (length(x) == 0L) {
    NA_character_
  } else {
    paste(x, collapse = sep)
  }
}

make_vpjd_taxon_id <- function(wcvp_concept_id) {
  paste0(
    "VPJD_WCVP_",
    as.character(wcvp_concept_id)
  )
}

# ------------------------------------------------------------------------------
# Paths
# ------------------------------------------------------------------------------

path_04c_candidates <- function() {
  here::here(
    "data",
    "interim",
    "taxonomy",
    "WCVP",
    "consolidated",
    "vpjd_wcvp_consolidated_candidates.rds"
  )
}

path_04c_source <- function() {
  here::here(
    "data",
    "interim",
    "taxonomy",
    "WCVP",
    "consolidated",
    "source_to_wcvp_consolidated.rds"
  )
}

path_04e_assessment <- function() {
  here::here(
    "data",
    "interim",
    "taxonomy",
    "WCVP",
    "deterministic_recovery",
    "vpjd_wcvp_04e_assessment.rds"
  )
}

path_source_candidate <- function() {
  here::here(
    "data",
    "interim",
    "taxonomy",
    "source_to_wcvp_candidate.rds"
  )
}

# ------------------------------------------------------------------------------
# Load inputs
# ------------------------------------------------------------------------------

load_05_inputs <- function() {
  paths <- c(
    candidates_04c = path_04c_candidates(),
    source_04c = path_04c_source(),
    assessment_04e = path_04e_assessment(),
    source_candidate = path_source_candidate()
  )
  
  missing <- paths[!file.exists(paths)]
  
  if (length(missing) > 0L) {
    stop(
      "Required input file(s) not found:\n",
      paste(missing, collapse = "\n"),
      call. = FALSE
    )
  }
  
  list(
    candidates_04c =
      readRDS(paths[["candidates_04c"]]),
    source_04c =
      readRDS(paths[["source_04c"]]),
    assessment_04e =
      readRDS(paths[["assessment_04e"]]),
    source_candidate =
      readRDS(paths[["source_candidate"]])
  )
}

# ------------------------------------------------------------------------------
# Validate inputs
# ------------------------------------------------------------------------------

validate_05_inputs <- function(inputs) {
  message("Validating frozen taxonomic inputs...")
  
  c04 <- inputs$candidates_04c
  s04 <- inputs$source_04c
  e04 <- inputs$assessment_04e
  sc <- inputs$source_candidate
  
  required_04c <- c(
    "wcvp_candidate_id",
    "wcvp_submitted_name",
    "final_reconciliation_status",
    "final_wcvp_concept_id",
    "final_wcvp_concept_name",
    "final_wcvp_concept_authors",
    "final_wcvp_concept_family",
    "final_wcvp_concept_genus",
    "final_wcvp_concept_species",
    "final_wcvp_concept_rank",
    "final_wcvp_concept_status"
  )
  
  required_04e <- c(
    "wcvp_candidate_id",
    "residual_status",
    "deterministic_recovery",
    "recovery_method",
    "selected_concept_id",
    "proposed_concept_name",
    "proposed_concept_authors",
    "proposed_concept_family",
    "proposed_concept_genus",
    "proposed_concept_species",
    "proposed_concept_rank",
    "proposed_concept_status"
  )
  
  required_source <- c(
    "source_record_id",
    "source_repository",
    "source_spnumber",
    "source_fullname",
    "wcvp_candidate_id"
  )
  
  missing_04c <- setdiff(required_04c, names(c04))
  missing_04e <- setdiff(required_04e, names(e04))
  missing_source <- setdiff(required_source, names(sc))
  
  if (length(missing_04c) > 0L) {
    stop(
      "Missing 04c fields: ",
      paste(missing_04c, collapse = ", "),
      call. = FALSE
    )
  }
  
  if (length(missing_04e) > 0L) {
    stop(
      "Missing 04e fields: ",
      paste(missing_04e, collapse = ", "),
      call. = FALSE
    )
  }
  
  if (length(missing_source) > 0L) {
    stop(
      "Missing source-crosswalk fields: ",
      paste(missing_source, collapse = ", "),
      call. = FALSE
    )
  }
  
  assert_equal_05(
    nrow(c04),
    EXPECTED_TOTAL_CANDIDATES,
    "04c candidate rows"
  )
  
  assert_equal_05(
    dplyr::n_distinct(c04$wcvp_candidate_id),
    EXPECTED_TOTAL_CANDIDATES,
    "04c distinct candidates"
  )
  
  assert_equal_05(
    nrow(sc),
    EXPECTED_SOURCE_RECORDS,
    "Source-record rows"
  )
  
  assert_equal_05(
    nrow(s04),
    EXPECTED_SOURCE_RECORDS,
    "04c source-record rows"
  )
  
  assert_equal_05(
    sum(
      c04$final_reconciliation_status %in%
        c("resolved", "resolved_hybrid"),
      na.rm = TRUE
    ),
    EXPECTED_04C_RESOLVED,
    "04c resolved candidates"
  )
  
  assert_equal_05(
    sum(e04$deterministic_recovery, na.rm = TRUE),
    EXPECTED_04E_RECOVERED,
    "04e deterministic recoveries"
  )
  
  if (
    any(
      e04$deterministic_recovery &
      e04$recovery_method != "04e_geojapan_synof",
      na.rm = TRUE
    )
  ) {
    stop(
      "04e contains a deterministic recovery not produced by ",
      "04e_geojapan_synof.",
      call. = FALSE
    )
  }
  
  message("Frozen taxonomic input validation passed.")
  invisible(TRUE)
}

# ------------------------------------------------------------------------------
# Final candidate reconciliation
# ------------------------------------------------------------------------------

build_05_candidate_reconciliation <- function(
    candidates_04c,
    assessment_04e
) {
  recovery <- assessment_04e %>%
    dplyr::filter(deterministic_recovery) %>%
    dplyr::transmute(
      wcvp_candidate_id,
      recovery_04e = TRUE,
      recovery_04e_method = recovery_method,
      recovery_04e_concept_id =
        as.character(selected_concept_id),
      recovery_04e_concept_name =
        clean_character_05(proposed_concept_name),
      recovery_04e_concept_authors =
        clean_character_05(proposed_concept_authors),
      recovery_04e_concept_family =
        clean_character_05(proposed_concept_family),
      recovery_04e_concept_genus =
        clean_character_05(proposed_concept_genus),
      recovery_04e_concept_species =
        clean_character_05(proposed_concept_species),
      recovery_04e_concept_rank =
        clean_character_05(proposed_concept_rank),
      recovery_04e_concept_status =
        clean_character_05(proposed_concept_status)
    )
  
  residual <- assessment_04e %>%
    dplyr::select(
      wcvp_candidate_id,
      residual_status
    )
  
  candidates_04c %>%
    dplyr::left_join(
      recovery,
      by = "wcvp_candidate_id"
    ) %>%
    dplyr::left_join(
      residual,
      by = "wcvp_candidate_id"
    ) %>%
    dplyr::mutate(
      recovery_04e =
        dplyr::coalesce(recovery_04e, FALSE),
      
      vpjd_reconciliation_status =
        dplyr::case_when(
          final_reconciliation_status %in%
            c("resolved", "resolved_hybrid") ~
            final_reconciliation_status,
          
          recovery_04e &
            recovery_04e_concept_status ==
            "Artificial Hybrid" ~
            "resolved_hybrid",
          
          recovery_04e ~
            "resolved",
          
          !is.na(residual_status) ~
            residual_status,
          
          TRUE ~
            final_reconciliation_status
        ),
      
      vpjd_resolution_stage =
        dplyr::case_when(
          final_reconciliation_status %in%
            c("resolved", "resolved_hybrid") ~
            "04c",
          
          recovery_04e ~
            "04e",
          
          TRUE ~
            NA_character_
        ),
      
      vpjd_resolution_method =
        dplyr::case_when(
          recovery_04e ~
            recovery_04e_method,
          
          final_reconciliation_status %in%
            c("resolved", "resolved_hybrid") ~
            "04c_frozen_reconciliation",
          
          TRUE ~
            NA_character_
        ),
      
      vpjd_wcvp_concept_id =
        dplyr::case_when(
          recovery_04e ~
            recovery_04e_concept_id,
          
          final_reconciliation_status %in%
            c("resolved", "resolved_hybrid") ~
            as.character(final_wcvp_concept_id),
          
          TRUE ~
            NA_character_
        ),
      
      vpjd_wcvp_concept_name =
        dplyr::case_when(
          recovery_04e ~
            recovery_04e_concept_name,
          
          final_reconciliation_status %in%
            c("resolved", "resolved_hybrid") ~
            clean_character_05(
              final_wcvp_concept_name
            ),
          
          TRUE ~
            NA_character_
        ),
      
      vpjd_wcvp_concept_authors =
        dplyr::case_when(
          recovery_04e ~
            recovery_04e_concept_authors,
          
          final_reconciliation_status %in%
            c("resolved", "resolved_hybrid") ~
            clean_character_05(
              final_wcvp_concept_authors
            ),
          
          TRUE ~
            NA_character_
        ),
      
      vpjd_wcvp_concept_family =
        dplyr::case_when(
          recovery_04e ~
            recovery_04e_concept_family,
          
          final_reconciliation_status %in%
            c("resolved", "resolved_hybrid") ~
            clean_character_05(
              final_wcvp_concept_family
            ),
          
          TRUE ~
            NA_character_
        ),
      
      vpjd_wcvp_concept_genus =
        dplyr::case_when(
          recovery_04e ~
            recovery_04e_concept_genus,
          
          final_reconciliation_status %in%
            c("resolved", "resolved_hybrid") ~
            clean_character_05(
              final_wcvp_concept_genus
            ),
          
          TRUE ~
            NA_character_
        ),
      
      vpjd_wcvp_concept_species =
        dplyr::case_when(
          recovery_04e ~
            recovery_04e_concept_species,
          
          final_reconciliation_status %in%
            c("resolved", "resolved_hybrid") ~
            clean_character_05(
              final_wcvp_concept_species
            ),
          
          TRUE ~
            NA_character_
        ),
      
      vpjd_wcvp_concept_rank =
        dplyr::case_when(
          recovery_04e ~
            recovery_04e_concept_rank,
          
          final_reconciliation_status %in%
            c("resolved", "resolved_hybrid") ~
            clean_character_05(
              final_wcvp_concept_rank
            ),
          
          TRUE ~
            NA_character_
        ),
      
      vpjd_wcvp_concept_status =
        dplyr::case_when(
          recovery_04e ~
            recovery_04e_concept_status,
          
          final_reconciliation_status %in%
            c("resolved", "resolved_hybrid") ~
            clean_character_05(
              final_wcvp_concept_status
            ),
          
          TRUE ~
            NA_character_
        ),
      
      vpjd_deterministically_resolved =
        vpjd_reconciliation_status %in%
        c("resolved", "resolved_hybrid")
    )
}

# ------------------------------------------------------------------------------
# Candidate validation
# ------------------------------------------------------------------------------

validate_05_candidate_reconciliation <- function(candidates) {
  message("Validating final candidate reconciliation...")
  
  assert_equal_05(
    nrow(candidates),
    EXPECTED_TOTAL_CANDIDATES,
    "Final candidate rows"
  )
  
  assert_equal_05(
    dplyr::n_distinct(candidates$wcvp_candidate_id),
    EXPECTED_TOTAL_CANDIDATES,
    "Final distinct candidates"
  )
  
  assert_equal_05(
    sum(
      candidates$vpjd_deterministically_resolved,
      na.rm = TRUE
    ),
    EXPECTED_FINAL_RESOLVED,
    "Final resolved candidates"
  )
  
  assert_equal_05(
    sum(
      !candidates$vpjd_deterministically_resolved,
      na.rm = TRUE
    ),
    EXPECTED_FINAL_UNRESOLVED,
    "Final unresolved candidates"
  )
  
  assert_equal_05(
    sum(
      candidates$vpjd_resolution_stage == "04e",
      na.rm = TRUE
    ),
    EXPECTED_04E_RECOVERED,
    "Candidates resolved by 04e"
  )
  
  bad_resolved <- candidates %>%
    dplyr::filter(
      vpjd_deterministically_resolved,
      is.na(vpjd_wcvp_concept_id)
    )
  
  if (nrow(bad_resolved) > 0L) {
    stop(
      "Resolved candidates without terminal WCVP concept IDs.",
      call. = FALSE
    )
  }
  
  bad_unresolved <- candidates %>%
    dplyr::filter(
      !vpjd_deterministically_resolved,
      !is.na(vpjd_wcvp_concept_id)
    )
  
  if (nrow(bad_unresolved) > 0L) {
    stop(
      "Unresolved candidates unexpectedly carry terminal WCVP concept IDs.",
      call. = FALSE
    )
  }
  
  message("Final candidate reconciliation validation passed.")
  invisible(TRUE)
}

# ------------------------------------------------------------------------------
# Source-to-terminal-concept crosswalk
# ------------------------------------------------------------------------------

build_05_source_crosswalk <- function(
    source_candidate,
    final_candidates
) {
  final_lookup <- final_candidates %>%
    dplyr::select(
      wcvp_candidate_id,
      vpjd_reconciliation_status,
      vpjd_resolution_stage,
      vpjd_resolution_method,
      vpjd_deterministically_resolved,
      vpjd_wcvp_concept_id,
      vpjd_wcvp_concept_name,
      vpjd_wcvp_concept_authors,
      vpjd_wcvp_concept_family,
      vpjd_wcvp_concept_genus,
      vpjd_wcvp_concept_species,
      vpjd_wcvp_concept_rank,
      vpjd_wcvp_concept_status
    )
  
  source_candidate %>%
    dplyr::left_join(
      final_lookup,
      by = "wcvp_candidate_id"
    ) %>%
    dplyr::mutate(
      vpjd_taxon_id =
        dplyr::if_else(
          vpjd_deterministically_resolved,
          make_vpjd_taxon_id(
            vpjd_wcvp_concept_id
          ),
          NA_character_
        )
    )
}

validate_05_source_crosswalk <- function(source_crosswalk) {
  message("Validating final source crosswalk...")
  
  assert_equal_05(
    nrow(source_crosswalk),
    EXPECTED_SOURCE_RECORDS,
    "Final source crosswalk rows"
  )
  
  assert_equal_05(
    dplyr::n_distinct(
      source_crosswalk$source_record_id
    ),
    EXPECTED_SOURCE_RECORDS,
    "Final distinct source records"
  )
  
  if (
    any(
      source_crosswalk$vpjd_deterministically_resolved &
      is.na(source_crosswalk$vpjd_taxon_id),
      na.rm = TRUE
    )
  ) {
    stop(
      "Resolved source record lacks VPJD taxon ID.",
      call. = FALSE
    )
  }
  
  message("Final source crosswalk validation passed.")
  invisible(TRUE)
}

# ------------------------------------------------------------------------------
# One-row-per-WCVP-concept VPJD backbone
# ------------------------------------------------------------------------------

build_05_vascular_backbone <- function(source_crosswalk) {
  resolved_sources <- source_crosswalk %>%
    dplyr::filter(
      vpjd_deterministically_resolved,
      !is.na(vpjd_wcvp_concept_id)
    )
  
  resolved_sources %>%
    dplyr::group_by(
      vpjd_taxon_id,
      vpjd_wcvp_concept_id
    ) %>%
    dplyr::summarise(
      wcvp_accepted_name =
        collapse_unique_05(
          vpjd_wcvp_concept_name
        ),
      
      wcvp_authors =
        collapse_unique_05(
          vpjd_wcvp_concept_authors
        ),
      
      family =
        collapse_unique_05(
          vpjd_wcvp_concept_family
        ),
      
      genus =
        collapse_unique_05(
          vpjd_wcvp_concept_genus
        ),
      
      species =
        collapse_unique_05(
          vpjd_wcvp_concept_species
        ),
      
      taxon_rank =
        collapse_unique_05(
          vpjd_wcvp_concept_rank
        ),
      
      wcvp_status =
        collapse_unique_05(
          vpjd_wcvp_concept_status
        ),
      
      source_record_count =
        dplyr::n_distinct(
          source_record_id
        ),
      
      source_candidate_count =
        dplyr::n_distinct(
          wcvp_candidate_id
        ),
      
      geojapan_record_count =
        dplyr::n_distinct(
          source_record_id[
            source_repository == "GEOJAPAN"
          ]
        ),
      
      foj_record_count =
        dplyr::n_distinct(
          source_record_id[
            source_repository == "FOJ"
          ]
        ),
      
      geojapan_spnumbers =
        collapse_unique_05(
          as.character(
            source_spnumber[
              source_repository == "GEOJAPAN"
            ]
          )
        ),
      
      foj_spnumbers =
        collapse_unique_05(
          as.character(
            source_spnumber[
              source_repository == "FOJ"
            ]
          )
        ),
      
      source_repositories =
        collapse_unique_05(
          source_repository
        ),
      
      source_candidate_ids =
        collapse_unique_05(
          wcvp_candidate_id
        ),
      
      source_names =
        collapse_unique_05(
          source_fullname
        ),
      
      resolution_stages =
        collapse_unique_05(
          vpjd_resolution_stage
        ),
      
      resolution_methods =
        collapse_unique_05(
          vpjd_resolution_method
        ),
      
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      vpjd_backbone_version =
        VPJD_BACKBONE_VERSION,
      
      vpjd_scope =
        "vascular_plant",
      
      gbif_taxon_key =
        NA_integer_,
      
      gbif_match_status =
        "not_yet_matched"
    ) %>%
    dplyr::select(
      vpjd_taxon_id,
      vpjd_backbone_version,
      vpjd_scope,
      vpjd_wcvp_concept_id,
      wcvp_accepted_name,
      wcvp_authors,
      family,
      genus,
      species,
      taxon_rank,
      wcvp_status,
      source_record_count,
      source_candidate_count,
      geojapan_record_count,
      foj_record_count,
      geojapan_spnumbers,
      foj_spnumbers,
      source_repositories,
      source_candidate_ids,
      source_names,
      resolution_stages,
      resolution_methods,
      gbif_taxon_key,
      gbif_match_status
    ) %>%
    dplyr::arrange(
      family,
      genus,
      wcvp_accepted_name
    )
}

# ------------------------------------------------------------------------------
# Backbone validation
# ------------------------------------------------------------------------------

validate_05_backbone <- function(
    backbone,
    source_crosswalk
) {
  message("Validating VPJD vascular backbone...")
  
  if (nrow(backbone) == 0L) {
    stop(
      "VPJD vascular backbone contains zero taxa.",
      call. = FALSE
    )
  }
  
  if (anyDuplicated(backbone$vpjd_taxon_id) > 0L) {
    stop(
      "Duplicate VPJD taxon IDs detected.",
      call. = FALSE
    )
  }
  
  if (
    anyDuplicated(
      backbone$vpjd_wcvp_concept_id
    ) > 0L
  ) {
    stop(
      "Duplicate WCVP terminal concepts detected in backbone.",
      call. = FALSE
    )
  }
  
  if (
    any(
      is.na(backbone$vpjd_wcvp_concept_id)
    )
  ) {
    stop(
      "Backbone contains missing WCVP concept IDs.",
      call. = FALSE
    )
  }
  
  if (
    any(
      is.na(backbone$wcvp_accepted_name)
    )
  ) {
    stop(
      "Backbone contains missing WCVP accepted names.",
      call. = FALSE
    )
  }
  
  source_concepts <- source_crosswalk %>%
    dplyr::filter(
      vpjd_deterministically_resolved
    ) %>%
    dplyr::distinct(
      vpjd_wcvp_concept_id
    ) %>%
    dplyr::pull(
      vpjd_wcvp_concept_id
    )
  
  if (
    !setequal(
      source_concepts,
      backbone$vpjd_wcvp_concept_id
    )
  ) {
    stop(
      "Backbone WCVP concepts do not exactly equal resolved source concepts.",
      call. = FALSE
    )
  }
  
  metadata_conflicts <- backbone %>%
    dplyr::filter(
      stringr::str_detect(
        dplyr::coalesce(
          wcvp_accepted_name,
          ""
        ),
        stringr::fixed(" | ")
      ) |
        stringr::str_detect(
          dplyr::coalesce(
            family,
            ""
          ),
          stringr::fixed(" | ")
        ) |
        stringr::str_detect(
          dplyr::coalesce(
            genus,
            ""
          ),
          stringr::fixed(" | ")
        ) |
        stringr::str_detect(
          dplyr::coalesce(
            taxon_rank,
            ""
          ),
          stringr::fixed(" | ")
        ) |
        stringr::str_detect(
          dplyr::coalesce(
            wcvp_status,
            ""
          ),
          stringr::fixed(" | ")
        )
    )
  
  if (nrow(metadata_conflicts) > 0L) {
    stop(
      "Conflicting terminal WCVP metadata detected for ",
      nrow(metadata_conflicts),
      " backbone concept(s).",
      call. = FALSE
    )
  }
  
  message("VPJD vascular backbone validation passed.")
  invisible(TRUE)
}

# ------------------------------------------------------------------------------
# Unresolved candidate queue
# ------------------------------------------------------------------------------

build_05_unresolved_candidates <- function(
    final_candidates,
    source_candidate
) {
  source_summary <- source_candidate %>%
    dplyr::group_by(
      wcvp_candidate_id
    ) %>%
    dplyr::summarise(
      source_record_count =
        dplyr::n_distinct(
          source_record_id
        ),
      
      geojapan_record_count =
        dplyr::n_distinct(
          source_record_id[
            source_repository == "GEOJAPAN"
          ]
        ),
      
      foj_record_count =
        dplyr::n_distinct(
          source_record_id[
            source_repository == "FOJ"
          ]
        ),
      
      source_repositories =
        collapse_unique_05(
          source_repository
        ),
      
      source_names =
        collapse_unique_05(
          source_fullname
        ),
      
      source_spnumbers =
        collapse_unique_05(
          paste0(
            source_repository,
            ":",
            source_spnumber
          )
        ),
      
      .groups = "drop"
    )
  
  final_candidates %>%
    dplyr::filter(
      !vpjd_deterministically_resolved
    ) %>%
    dplyr::select(
      -dplyr::any_of(
        c(
          "source_record_count",
          "geojapan_record_count",
          "foj_record_count",
          "source_repositories",
          "source_names",
          "source_spnumbers"
        )
      )
    ) %>%
    dplyr::left_join(
      source_summary,
      by = "wcvp_candidate_id"
    ) %>%
    dplyr::mutate(
      vpjd_scope_status =
        "unresolved_scope_not_assumed"
    ) %>%
    dplyr::relocate(
      wcvp_candidate_id,
      wcvp_submitted_name,
      vpjd_reconciliation_status,
      dplyr::any_of(
        "residual_status"
      ),
      vpjd_scope_status,
      source_record_count,
      geojapan_record_count,
      foj_record_count,
      source_repositories,
      source_names,
      source_spnumbers
    )
}

# ------------------------------------------------------------------------------
# Unresolved validation
# ------------------------------------------------------------------------------

validate_05_unresolved <- function(unresolved) {
  message("Validating unresolved candidate queue...")
  
  assert_equal_05(
    nrow(unresolved),
    EXPECTED_FINAL_UNRESOLVED,
    "Unresolved candidate rows"
  )
  
  assert_equal_05(
    sum(
      unresolved$vpjd_reconciliation_status ==
        "unmatched",
      na.rm = TRUE
    ),
    EXPECTED_FINAL_UNMATCHED,
    "Final unmatched candidates"
  )
  
  assert_equal_05(
    sum(
      unresolved$vpjd_reconciliation_status ==
        "ambiguous",
      na.rm = TRUE
    ),
    EXPECTED_FINAL_AMBIGUOUS,
    "Final ambiguous candidates"
  )
  
  assert_equal_05(
    sum(
      unresolved$vpjd_reconciliation_status ==
        "review",
      na.rm = TRUE
    ),
    EXPECTED_FINAL_REVIEW,
    "Final review candidates"
  )
  
  if (
    any(
      unresolved$vpjd_deterministically_resolved,
      na.rm = TRUE
    )
  ) {
    stop(
      "Resolved candidates found in unresolved queue.",
      call. = FALSE
    )
  }
  
  message("Unresolved candidate queue validation passed.")
  invisible(TRUE)
}

# ------------------------------------------------------------------------------
# Audits
# ------------------------------------------------------------------------------

build_05_audits <- function(
    final_candidates,
    source_crosswalk,
    backbone,
    unresolved
) {
  summary <- tibble::tibble(
    metric = c(
      "source_records",
      "candidate_names",
      "resolved_candidates",
      "resolved_by_04c",
      "resolved_by_04e",
      "unresolved_candidates",
      "unmatched_candidates",
      "ambiguous_candidates",
      "review_candidates",
      "distinct_wcvp_backbone_taxa",
      "backbone_families",
      "backbone_genera",
      "backbone_artificial_hybrids"
    ),
    
    value = c(
      nrow(source_crosswalk),
      
      nrow(final_candidates),
      
      sum(
        final_candidates$
          vpjd_deterministically_resolved,
        na.rm = TRUE
      ),
      
      sum(
        final_candidates$
          vpjd_resolution_stage == "04c",
        na.rm = TRUE
      ),
      
      sum(
        final_candidates$
          vpjd_resolution_stage == "04e",
        na.rm = TRUE
      ),
      
      nrow(unresolved),
      
      sum(
        unresolved$
          vpjd_reconciliation_status ==
          "unmatched",
        na.rm = TRUE
      ),
      
      sum(
        unresolved$
          vpjd_reconciliation_status ==
          "ambiguous",
        na.rm = TRUE
      ),
      
      sum(
        unresolved$
          vpjd_reconciliation_status ==
          "review",
        na.rm = TRUE
      ),
      
      nrow(backbone),
      
      dplyr::n_distinct(
        backbone$family[
          !is.na(backbone$family)
        ]
      ),
      
      dplyr::n_distinct(
        backbone$genus[
          !is.na(backbone$genus)
        ]
      ),
      
      sum(
        backbone$wcvp_status ==
          "Artificial Hybrid",
        na.rm = TRUE
      )
    )
  )
  
  ranks <- backbone %>%
    dplyr::count(
      taxon_rank,
      name = "taxa",
      sort = TRUE
    )
  
  families <- backbone %>%
    dplyr::count(
      family,
      name = "taxa",
      sort = TRUE
    )
  
  provenance <- backbone %>%
    dplyr::count(
      source_repositories,
      resolution_stages,
      name = "taxa",
      sort = TRUE
    )
  
  unresolved_status <- unresolved %>%
    dplyr::count(
      vpjd_reconciliation_status,
      residual_status,
      name = "candidate_names",
      sort = TRUE
    )
  
  list(
    summary = summary,
    ranks = ranks,
    families = families,
    provenance = provenance,
    unresolved_status =
      unresolved_status
  )
}

# ------------------------------------------------------------------------------
# Write outputs
# ------------------------------------------------------------------------------

write_05_outputs <- function(
    final_candidates,
    source_crosswalk,
    backbone,
    unresolved,
    audits
) {
  processed_dir <- here::here(
    "data",
    "processed",
    "taxonomy"
  )
  
  interim_dir <- here::here(
    "data",
    "interim",
    "taxonomy"
  )
  
  audit_dir <- here::here(
    "outputs",
    "tables",
    "taxonomy",
    "backbone"
  )
  
  dir.create(
    processed_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  dir.create(
    interim_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  dir.create(
    audit_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  saveRDS(
    final_candidates,
    file.path(
      interim_dir,
      "vpjd_final_candidate_reconciliation.rds"
    )
  )
  
  readr::write_csv(
    final_candidates,
    file.path(
      interim_dir,
      "vpjd_final_candidate_reconciliation.csv"
    )
  )
  
  saveRDS(
    backbone,
    file.path(
      processed_dir,
      "vpjd_vascular_taxa.rds"
    )
  )
  
  readr::write_csv(
    backbone,
    file.path(
      processed_dir,
      "vpjd_vascular_taxa.csv"
    )
  )
  
  saveRDS(
    source_crosswalk,
    file.path(
      processed_dir,
      "vpjd_source_to_vascular_taxon.rds"
    )
  )
  
  readr::write_csv(
    source_crosswalk,
    file.path(
      processed_dir,
      "vpjd_source_to_vascular_taxon.csv"
    )
  )
  
  saveRDS(
    unresolved,
    file.path(
      interim_dir,
      "vpjd_unresolved_candidates.rds"
    )
  )
  
  readr::write_csv(
    unresolved,
    file.path(
      interim_dir,
      "vpjd_unresolved_candidates.csv"
    )
  )
  
  readr::write_csv(
    audits$summary,
    file.path(
      audit_dir,
      "vpjd_backbone_summary.csv"
    )
  )
  
  readr::write_csv(
    audits$ranks,
    file.path(
      audit_dir,
      "vpjd_backbone_rank_audit.csv"
    )
  )
  
  readr::write_csv(
    audits$families,
    file.path(
      audit_dir,
      "vpjd_backbone_family_audit.csv"
    )
  )
  
  readr::write_csv(
    audits$provenance,
    file.path(
      audit_dir,
      "vpjd_backbone_provenance_audit.csv"
    )
  )
  
  readr::write_csv(
    audits$unresolved_status,
    file.path(
      audit_dir,
      "vpjd_backbone_unresolved_audit.csv"
    )
  )
  
  invisible(
    list(
      processed_dir = processed_dir,
      interim_dir = interim_dir,
      audit_dir = audit_dir
    )
  )
}

# ------------------------------------------------------------------------------
# Main
# ------------------------------------------------------------------------------

build_vpjd_backbone <- function(
    write_outputs = TRUE
) {
  message(
    "\nBuilding VPJD vascular taxonomic backbone..."
  )
  
  message(
    "Backbone module version: ",
    VPJD_BACKBONE_VERSION
  )
  
  inputs <- load_05_inputs()
  
  validate_05_inputs(
    inputs
  )
  
  message(
    "Combining frozen 04c reconciliation with validated 04e recoveries..."
  )
  
  final_candidates <-
    build_05_candidate_reconciliation(
      inputs$candidates_04c,
      inputs$assessment_04e
    )
  
  validate_05_candidate_reconciliation(
    final_candidates
  )
  
  message(
    "Building source-to-terminal-concept crosswalk..."
  )
  
  source_crosswalk <-
    build_05_source_crosswalk(
      inputs$source_candidate,
      final_candidates
    )
  
  validate_05_source_crosswalk(
    source_crosswalk
  )
  
  message(
    "Collapsing resolved source records to terminal WCVP concepts..."
  )
  
  backbone <-
    build_05_vascular_backbone(
      source_crosswalk
    )
  
  validate_05_backbone(
    backbone,
    source_crosswalk
  )
  
  message(
    "Building unresolved candidate queue..."
  )
  
  unresolved <-
    build_05_unresolved_candidates(
      final_candidates,
      inputs$source_candidate
    )
  
  validate_05_unresolved(
    unresolved
  )
  
  message(
    "Building backbone audits..."
  )
  
  audits <-
    build_05_audits(
      final_candidates,
      source_crosswalk,
      backbone,
      unresolved
    )
  
  if (write_outputs) {
    message(
      "Writing VPJD backbone outputs..."
    )
    
    paths <-
      write_05_outputs(
        final_candidates,
        source_crosswalk,
        backbone,
        unresolved,
        audits
      )
  } else {
    paths <- NULL
  }
  
  message(
    "\nVPJD vascular taxonomic backbone complete."
  )
  
  message(
    "Source records: ",
    format(
      nrow(source_crosswalk),
      big.mark = ","
    )
  )
  
  message(
    "Candidate names: ",
    format(
      nrow(final_candidates),
      big.mark = ","
    )
  )
  
  message(
    "Deterministically resolved candidates: ",
    format(
      sum(
        final_candidates$
          vpjd_deterministically_resolved,
        na.rm = TRUE
      ),
      big.mark = ","
    )
  )
  
  message(
    "Distinct terminal WCVP taxa: ",
    format(
      nrow(backbone),
      big.mark = ","
    )
  )
  
  message(
    "Remaining unresolved candidates: ",
    format(
      nrow(unresolved),
      big.mark = ","
    )
  )
  
  message(
    "Backbone ready for WCVP -> GBIF reconciliation."
  )
  
  list(
    final_candidates =
      final_candidates,
    source_crosswalk =
      source_crosswalk,
    backbone =
      backbone,
    unresolved =
      unresolved,
    audits =
      audits,
    paths =
      paths
  )
}

# ------------------------------------------------------------------------------
# Module load message
# ------------------------------------------------------------------------------

message(
  "05_build_vpjd_backbone.R v",
  VPJD_BACKBONE_VERSION,
  " loaded."
)

message(
  "Run build_vpjd_backbone() to create the WCVP-based VPJD vascular backbone."
)