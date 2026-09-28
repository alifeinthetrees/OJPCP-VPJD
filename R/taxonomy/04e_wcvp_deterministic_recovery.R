# ==============================================================================
# 04e_wcvp_deterministic_recovery.R
#
# Oxford–Japan Plant Conservation Partnership (OJPCP)
# Vascular Plants of Japan Database (VPJD)
#
# Purpose:
#   Apply additional conservative deterministic taxonomic recovery to
#   candidates remaining unresolved after the frozen 04c consolidation and
#   classified by 04d residual diagnostics.
#
# Upstream validated modules:
#   03   source taxon universe          v0.2.0
#   04   WCVP standardisation           v0.3.3
#   04b  structured-name recovery       v0.1.1
#   04c  deterministic consolidation    v0.1.2
#   04d  residual diagnostics           v0.1.0
#
# Resolution-bearing evidence:
#   GEOJAPAN SYNOF relationships only.
#
#   A residual candidate may be deterministically recovered where:
#
#   1. it has explicit GEOJAPAN SYNOF evidence;
#   2. the referenced GEOJAPAN target maps through frozen 04c to a
#      deterministic terminal WCVP concept;
#   3. all applicable resolution-bearing SYNOF evidence converges on one
#      terminal WCVP concept; and
#   4. for a candidate already classified as WCVP-ambiguous, that concept is
#      one of its existing WCVP terminal alternatives.
#
# Diagnostic-only evidence:
#   Conservative nomenclatural normalisation is retained for investigation,
#   but NEVER changes reconciliation status in this module.
#
#   Current diagnostic normalisations:
#     - hybrid "x" -> multiplication sign
#     - hybrid prefix "x" -> multiplication sign
#     - ssp. -> subsp.
#     - forma -> f.
#
# Principles:
#   - NO fuzzy matching.
#   - NO edit-distance matching.
#   - NO probabilistic matching.
#   - NO automatic preference for an Accepted WCVP row.
#   - NO modification of frozen upstream outputs.
#   - NO resolution from nomenclatural normalisation alone.
#   - Diagnostic evidence cannot veto valid SYNOF resolution evidence.
#   - Conflicting SYNOF relationships remain unresolved.
#   - Ambiguous candidates cannot be resolved outside their existing WCVP
#     terminal concept set.
#   - Full source and resolution provenance is retained.
#
# Regression provenance:
#   v0.1.2 combined resolution-bearing SYNOF evidence and diagnostic
#   nomenclatural-normalisation evidence in one proposal pool. This caused
#   diagnostic matches to veto otherwise deterministic SYNOF recoveries.
#
#   Separation of the two evidence classes identified two additional valid
#   ambiguous recoveries:
#
#     WCVPCAND0007076
#       submitted: Carex x sharensis
#       GEOJAPAN SYNOF target -> WCVP 228886
#       Carex × musashiensis
#
#     WCVPCAND0018351
#       submitted: Lilium x bulbiferum
#       GEOJAPAN SYNOF target -> WCVP 279974
#       Lilium × elegans
#
#   Both selected concepts are members of the candidates' pre-existing WCVP
#   ambiguity sets. Their recovery therefore follows the same conservative
#   rule as all other ambiguous 04e recoveries.
#
# Validated 04e result:
#   residual candidates entering 04e: 5,682
#   deterministic SYNOF recoveries:    3,153
#     unmatched:                       2,306
#     ambiguous:                         830
#     review:                             17
#   remaining unresolved:              2,529
#
#   total deterministic resolution after 04e:
#     29,763 / 32,292 = 92.17%
#
# This module produces validated recovery evidence.
# Final incorporation into the authoritative reconciliation belongs in 04f.
#
# Version: 0.1.4
# ==============================================================================

WCVP_DETERMINISTIC_RECOVERY_VERSION <- "0.1.4"

# ------------------------------------------------------------------------------
# Packages
# ------------------------------------------------------------------------------

required_packages <- c(
  "dplyr", "tidyr", "stringr", "readr",
  "tibble", "purrr", "here"
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
# Frozen baseline expectations
# ------------------------------------------------------------------------------

EXPECTED_TOTAL_CANDIDATES <- 32292L
EXPECTED_04C_RESOLVED <- 26610L
EXPECTED_RESIDUAL_CANDIDATES <- 5682L
EXPECTED_UNMATCHED <- 3703L
EXPECTED_AMBIGUOUS <- 1928L
EXPECTED_REVIEW <- 51L
EXPECTED_SOURCE_RECORDS <- 40891L

# ------------------------------------------------------------------------------
# Validated 04e regression expectations
# ------------------------------------------------------------------------------

EXPECTED_04E_RECOVERED <- 3153L
EXPECTED_04E_RECOVERED_UNMATCHED <- 2306L
EXPECTED_04E_RECOVERED_AMBIGUOUS <- 830L
EXPECTED_04E_RECOVERED_REVIEW <- 17L

EXPECTED_04E_SYNOF_EVIDENCE_ROWS <- 3498L
EXPECTED_04E_CONFLICTING_CANDIDATES <- 89L
EXPECTED_04E_OUTSIDE_AMBIGUITY <- 133L

EXPECTED_POST_04E_RESOLVED <- 29763L
EXPECTED_POST_04E_UNRESOLVED <- 2529L
EXPECTED_POST_04E_UNMATCHED <- 1397L
EXPECTED_POST_04E_AMBIGUOUS <- 1098L
EXPECTED_POST_04E_REVIEW <- 34L

# Explicit regression cases introduced by evidence separation.
EXPECTED_REGRESSION_CANDIDATES <- c(
  "WCVPCAND0007076",
  "WCVPCAND0018351"
)

EXPECTED_REGRESSION_CONCEPTS <- c(
  WCVPCAND0007076 = "228886",
  WCVPCAND0018351 = "279974"
)

# ------------------------------------------------------------------------------
# Helpers
# ------------------------------------------------------------------------------

clean_character_04e <- function(x) {
  x <- as.character(x)
  x <- stringr::str_squish(x)
  
  x[
    is.na(x) |
      x == "" |
      toupper(x) %in% c("NA", "N/A", "NULL")
  ] <- NA_character_
  
  x
}

assert_equal_04e <- function(
    observed,
    expected,
    label
) {
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

collapse_unique_04e <- function(
    x,
    sep = " | "
) {
  x <- clean_character_04e(x)
  x <- sort(unique(x[!is.na(x)]))
  
  if (length(x) == 0L) {
    NA_character_
  } else {
    paste(x, collapse = sep)
  }
}

first_existing_field_04e <- function(
    data,
    candidates
) {
  found <- candidates[
    candidates %in% names(data)
  ]
  
  if (length(found) == 0L) {
    NA_character_
  } else {
    found[[1]]
  }
}

normalise_source_id_04e <- function(x) {
  x <- suppressWarnings(
    as.numeric(x)
  )
  
  x[
    is.na(x) |
      x <= 0
  ] <- NA_real_
  
  x
}

empty_resolution_evidence_04e <- function() {
  tibble::tibble(
    wcvp_candidate_id = character(),
    evidence_type = character(),
    evidence_strategy = character(),
    evidence_detail = character(),
    source_record_id = character(),
    source_spnumber = numeric(),
    target_spnumber = numeric(),
    target_candidate_id = character(),
    proposed_concept_id = character(),
    proposed_concept_name = character(),
    proposed_concept_authors = character(),
    proposed_concept_family = character(),
    proposed_concept_genus = character(),
    proposed_concept_species = character(),
    proposed_concept_rank = character(),
    proposed_concept_status = character()
  )
}

empty_diagnostic_evidence_04e <- function() {
  tibble::tibble(
    wcvp_candidate_id = character(),
    evidence_type = character(),
    diagnostic_strategy = character(),
    original_name = character(),
    diagnostic_name = character(),
    matched_wcvp_name_id = character(),
    matched_wcvp_status = character(),
    diagnostic_concept_id = character(),
    diagnostic_concept_name = character(),
    diagnostic_concept_status = character()
  )
}

# ------------------------------------------------------------------------------
# Input loaders
# ------------------------------------------------------------------------------

load_04e_residual_diagnostics <- function() {
  path <- here::here(
    "data", "interim", "taxonomy", "WCVP",
    "residual_diagnostics",
    "vpjd_wcvp_residual_diagnostics.rds"
  )
  
  if (!file.exists(path)) {
    stop(
      "04d residual diagnostic file not found: ",
      path,
      call. = FALSE
    )
  }
  
  x <- readRDS(path)
  
  required <- c(
    "wcvp_candidate_id",
    "wcvp_submitted_name",
    "residual_status",
    "source_class",
    "diagnostic_class"
  )
  
  missing <- setdiff(
    required,
    names(x)
  )
  
  if (length(missing) > 0L) {
    stop(
      "Required 04d fields missing: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  
  x
}

load_04e_consolidated_candidates <- function() {
  path <- here::here(
    "data", "interim", "taxonomy", "WCVP",
    "consolidated",
    "vpjd_wcvp_consolidated_candidates.rds"
  )
  
  if (!file.exists(path)) {
    stop(
      "04c consolidated candidate file not found: ",
      path,
      call. = FALSE
    )
  }
  
  x <- readRDS(path)
  
  required <- c(
    "wcvp_candidate_id",
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
  
  missing <- setdiff(
    required,
    names(x)
  )
  
  if (length(missing) > 0L) {
    stop(
      "Required 04c candidate fields missing: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  
  x
}

load_04e_consolidated_source_crosswalk <- function() {
  path <- here::here(
    "data", "interim", "taxonomy", "WCVP",
    "consolidated",
    "source_to_wcvp_consolidated.rds"
  )
  
  if (!file.exists(path)) {
    stop(
      "04c consolidated source crosswalk not found: ",
      path,
      call. = FALSE
    )
  }
  
  readRDS(path)
}

load_04e_source_candidate_crosswalk <- function() {
  path <- here::here(
    "data", "interim", "taxonomy",
    "source_to_wcvp_candidate.rds"
  )
  
  if (!file.exists(path)) {
    stop(
      "Source-to-candidate crosswalk not found: ",
      path,
      call. = FALSE
    )
  }
  
  x <- readRDS(path)
  
  required <- c(
    "source_record_id",
    "source_repository",
    "source_spnumber",
    "wcvp_candidate_id"
  )
  
  missing <- setdiff(
    required,
    names(x)
  )
  
  if (length(missing) > 0L) {
    stop(
      "Required source-crosswalk fields missing: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  
  x
}

load_04e_geojapan_taxa <- function() {
  path <- here::here(
    "data", "interim", "GEOJAPAN",
    "geojapan_taxa.rds"
  )
  
  if (!file.exists(path)) {
    stop(
      "GEOJAPAN taxa file not found: ",
      path,
      call. = FALSE
    )
  }
  
  readRDS(path)
}

load_04e_raw_matches <- function() {
  path <- here::here(
    "data", "interim", "taxonomy", "WCVP",
    "vpjd_wcvp_raw_matches.rds"
  )
  
  if (!file.exists(path)) {
    stop(
      "04 raw WCVP match file not found: ",
      path,
      call. = FALSE
    )
  }
  
  readRDS(path)
}

load_04e_wcvp_names <- function() {
  if (!requireNamespace(
    "rWCVPdata",
    quietly = TRUE
  )) {
    stop(
      "Package rWCVPdata is required.",
      call. = FALSE
    )
  }
  
  data(
    "wcvp_names",
    package = "rWCVPdata",
    envir = environment()
  )
  
  get(
    "wcvp_names",
    envir = environment()
  )
}

# ------------------------------------------------------------------------------
# Frozen baseline validation
# ------------------------------------------------------------------------------

validate_04e_baseline <- function(
    residuals,
    consolidated_candidates,
    source_crosswalk
) {
  message(
    "Validating frozen 04c/04d baseline..."
  )
  
  assert_equal_04e(
    nrow(residuals),
    EXPECTED_RESIDUAL_CANDIDATES,
    "Residual candidate count"
  )
  
  assert_equal_04e(
    sum(
      residuals$residual_status == "unmatched",
      na.rm = TRUE
    ),
    EXPECTED_UNMATCHED,
    "Residual unmatched count"
  )
  
  assert_equal_04e(
    sum(
      residuals$residual_status == "ambiguous",
      na.rm = TRUE
    ),
    EXPECTED_AMBIGUOUS,
    "Residual ambiguous count"
  )
  
  assert_equal_04e(
    sum(
      residuals$residual_status == "review",
      na.rm = TRUE
    ),
    EXPECTED_REVIEW,
    "Residual review count"
  )
  
  assert_equal_04e(
    nrow(consolidated_candidates),
    EXPECTED_TOTAL_CANDIDATES,
    "04c candidate count"
  )
  
  assert_equal_04e(
    sum(
      consolidated_candidates$final_reconciliation_status %in%
        c(
          "resolved",
          "resolved_hybrid"
        ),
      na.rm = TRUE
    ),
    EXPECTED_04C_RESOLVED,
    "04c deterministic resolved count"
  )
  
  assert_equal_04e(
    nrow(source_crosswalk),
    EXPECTED_SOURCE_RECORDS,
    "04c source-record count"
  )
  
  message(
    "Frozen 04c/04d baseline validation passed."
  )
  
  invisible(TRUE)
}

# ------------------------------------------------------------------------------
# WCVP concept lookup
# ------------------------------------------------------------------------------

build_04e_wcvp_lookup <- function(wcvp) {
  required <- c(
    "plant_name_id",
    "taxon_name",
    "taxon_authors",
    "family",
    "genus",
    "species",
    "taxon_rank",
    "taxon_status"
  )
  
  missing <- setdiff(
    required,
    names(wcvp)
  )
  
  if (length(missing) > 0L) {
    stop(
      "Required WCVP fields missing: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  
  wcvp %>%
    dplyr::filter(
      taxon_status %in% c(
        "Accepted",
        "Artificial Hybrid"
      )
    ) %>%
    dplyr::transmute(
      wcvp_concept_id =
        as.character(plant_name_id),
      wcvp_concept_name =
        clean_character_04e(taxon_name),
      wcvp_concept_authors =
        clean_character_04e(taxon_authors),
      wcvp_concept_family =
        clean_character_04e(family),
      wcvp_concept_genus =
        clean_character_04e(genus),
      wcvp_concept_species =
        clean_character_04e(species),
      wcvp_concept_rank =
        clean_character_04e(taxon_rank),
      wcvp_concept_status =
        clean_character_04e(taxon_status)
    ) %>%
    dplyr::distinct(
      wcvp_concept_id,
      .keep_all = TRUE
    )
}

# ------------------------------------------------------------------------------
# Resolution evidence: GEOJAPAN SYNOF only
# ------------------------------------------------------------------------------

build_04e_synof_edges <- function(
    residuals,
    source_candidate_crosswalk,
    geojapan_taxa
) {
  required_geo <- c(
    "geojapan_spnumber",
    "geojapan_synof"
  )
  
  missing_geo <- setdiff(
    required_geo,
    names(geojapan_taxa)
  )
  
  if (length(missing_geo) > 0L) {
    stop(
      "Required GEOJAPAN SYNOF fields missing: ",
      paste(missing_geo, collapse = ", "),
      call. = FALSE
    )
  }
  
  geo <- geojapan_taxa %>%
    dplyr::transmute(
      source_spnumber =
        normalise_source_id_04e(
          geojapan_spnumber
        ),
      target_spnumber =
        normalise_source_id_04e(
          geojapan_synof
        )
    ) %>%
    dplyr::filter(
      !is.na(source_spnumber),
      !is.na(target_spnumber)
    )
  
  source_candidate_crosswalk %>%
    dplyr::filter(
      source_repository == "GEOJAPAN",
      wcvp_candidate_id %in%
        residuals$wcvp_candidate_id
    ) %>%
    dplyr::transmute(
      wcvp_candidate_id =
        as.character(wcvp_candidate_id),
      source_record_id =
        as.character(source_record_id),
      source_spnumber =
        normalise_source_id_04e(
          source_spnumber
        )
    ) %>%
    dplyr::left_join(
      geo,
      by = "source_spnumber"
    ) %>%
    dplyr::filter(
      !is.na(target_spnumber)
    ) %>%
    dplyr::distinct()
}

build_04e_target_lookup <- function(
    source_candidate_crosswalk,
    consolidated_candidates
) {
  source_candidate_crosswalk %>%
    dplyr::filter(
      source_repository == "GEOJAPAN"
    ) %>%
    dplyr::transmute(
      target_spnumber =
        normalise_source_id_04e(
          source_spnumber
        ),
      target_candidate_id =
        as.character(wcvp_candidate_id)
    ) %>%
    dplyr::left_join(
      consolidated_candidates %>%
        dplyr::select(
          wcvp_candidate_id,
          target_final_status =
            final_reconciliation_status,
          target_concept_id =
            final_wcvp_concept_id,
          target_concept_name =
            final_wcvp_concept_name,
          target_concept_authors =
            final_wcvp_concept_authors,
          target_concept_family =
            final_wcvp_concept_family,
          target_concept_genus =
            final_wcvp_concept_genus,
          target_concept_species =
            final_wcvp_concept_species,
          target_concept_rank =
            final_wcvp_concept_rank,
          target_concept_status =
            final_wcvp_concept_status
        ),
      by = c(
        "target_candidate_id" =
          "wcvp_candidate_id"
      )
    ) %>%
    dplyr::distinct()
}

build_04e_resolution_evidence <- function(
    synof_edges,
    target_lookup
) {
  if (nrow(synof_edges) == 0L) {
    return(
      empty_resolution_evidence_04e()
    )
  }
  
  synof_edges %>%
    dplyr::left_join(
      target_lookup,
      by = "target_spnumber"
    ) %>%
    dplyr::filter(
      target_final_status %in% c(
        "resolved",
        "resolved_hybrid"
      ),
      !is.na(target_concept_id)
    ) %>%
    dplyr::transmute(
      wcvp_candidate_id,
      evidence_type =
        "resolution",
      evidence_strategy =
        "source_relationship_synof",
      evidence_detail =
        paste0(
          "GEOJAPAN:",
          source_spnumber,
          " synof -> GEOJAPAN:",
          target_spnumber,
          " -> ",
          target_candidate_id
        ),
      source_record_id,
      source_spnumber,
      target_spnumber,
      target_candidate_id,
      proposed_concept_id =
        as.character(
          target_concept_id
        ),
      proposed_concept_name =
        clean_character_04e(
          target_concept_name
        ),
      proposed_concept_authors =
        clean_character_04e(
          target_concept_authors
        ),
      proposed_concept_family =
        clean_character_04e(
          target_concept_family
        ),
      proposed_concept_genus =
        clean_character_04e(
          target_concept_genus
        ),
      proposed_concept_species =
        clean_character_04e(
          target_concept_species
        ),
      proposed_concept_rank =
        clean_character_04e(
          target_concept_rank
        ),
      proposed_concept_status =
        clean_character_04e(
          target_concept_status
        )
    ) %>%
    dplyr::distinct()
}

# ------------------------------------------------------------------------------
# Existing WCVP ambiguity concepts
# ------------------------------------------------------------------------------

build_04e_existing_ambiguity_concepts <- function(
    residuals,
    raw_matches
) {
  ambiguous_ids <- residuals %>%
    dplyr::filter(
      residual_status == "ambiguous"
    ) %>%
    dplyr::pull(
      wcvp_candidate_id
    )
  
  candidate_field <-
    first_existing_field_04e(
      raw_matches,
      c(
        "wcvp_candidate_id"
      )
    )
  
  concept_field <-
    first_existing_field_04e(
      raw_matches,
      c(
        "wcvp_resolved_concept_id",
        "wcvp_concept_id",
        "resolved_wcvp_concept_id"
      )
    )
  
  if (
    is.na(candidate_field) ||
    is.na(concept_field)
  ) {
    return(
      tibble::tibble(
        wcvp_candidate_id = character(),
        ambiguity_concept_id = character()
      )
    )
  }
  
  raw_matches %>%
    dplyr::filter(
      .data[[candidate_field]] %in%
        ambiguous_ids
    ) %>%
    dplyr::transmute(
      wcvp_candidate_id =
        as.character(
          .data[[candidate_field]]
        ),
      ambiguity_concept_id =
        as.character(
          .data[[concept_field]]
        )
    ) %>%
    dplyr::filter(
      !is.na(
        ambiguity_concept_id
      )
    ) %>%
    dplyr::distinct()
}

# ------------------------------------------------------------------------------
# Assess resolution-bearing evidence
# ------------------------------------------------------------------------------

assess_04e_resolution <- function(
    residuals,
    resolution_evidence,
    ambiguity_concepts
) {
  evidence_summary <-
    resolution_evidence %>%
    dplyr::group_by(
      wcvp_candidate_id
    ) %>%
    dplyr::summarise(
      resolution_evidence_rows =
        dplyr::n(),
      resolution_concept_count =
        dplyr::n_distinct(
          proposed_concept_id
        ),
      resolution_concept_ids =
        collapse_unique_04e(
          proposed_concept_id
        ),
      resolution_evidence_detail =
        collapse_unique_04e(
          evidence_detail
        ),
      selected_concept_id =
        if (
          dplyr::n_distinct(
            proposed_concept_id
          ) == 1L
        ) {
          unique(
            proposed_concept_id
          )[[1]]
        } else {
          NA_character_
        },
      .groups = "drop"
    )
  
  concept_metadata <-
    resolution_evidence %>%
    dplyr::distinct(
      proposed_concept_id,
      proposed_concept_name,
      proposed_concept_authors,
      proposed_concept_family,
      proposed_concept_genus,
      proposed_concept_species,
      proposed_concept_rank,
      proposed_concept_status
    )
  
  ambiguity_summary <-
    ambiguity_concepts %>%
    dplyr::group_by(
      wcvp_candidate_id
    ) %>%
    dplyr::summarise(
      ambiguity_concept_ids =
        collapse_unique_04e(
          ambiguity_concept_id
        ),
      ambiguity_concept_count =
        dplyr::n_distinct(
          ambiguity_concept_id
        ),
      .groups = "drop"
    )
  
  x <- residuals %>%
    dplyr::select(
      wcvp_candidate_id,
      wcvp_submitted_name,
      residual_status,
      source_class,
      diagnostic_class
    ) %>%
    dplyr::left_join(
      evidence_summary,
      by = "wcvp_candidate_id"
    ) %>%
    dplyr::left_join(
      ambiguity_summary,
      by = "wcvp_candidate_id"
    ) %>%
    dplyr::left_join(
      concept_metadata,
      by = c(
        "selected_concept_id" =
          "proposed_concept_id"
      )
    )
  
  x <- x %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      selected_is_existing_ambiguity_concept =
        dplyr::case_when(
          residual_status != "ambiguous" ~
            NA,
          is.na(
            selected_concept_id
          ) ~
            FALSE,
          is.na(
            ambiguity_concept_ids
          ) ~
            FALSE,
          TRUE ~
            selected_concept_id %in%
            stringr::str_split(
              ambiguity_concept_ids,
              stringr::fixed(" | ")
            )[[1]]
        )
    ) %>%
    dplyr::ungroup()
  
  x %>%
    dplyr::mutate(
      recovery_status =
        dplyr::case_when(
          is.na(
            resolution_evidence_rows
          ) ~
            "no_resolution_evidence",
          
          resolution_concept_count > 1L ~
            "conflicting_synof",
          
          residual_status == "ambiguous" &
            selected_is_existing_ambiguity_concept %in%
            TRUE ~
            "recoverable_supported_ambiguity",
          
          residual_status == "ambiguous" ~
            "synof_outside_existing_ambiguity",
          
          resolution_concept_count == 1L ~
            "recoverable_single_concept",
          
          TRUE ~
            "review"
        ),
      
      deterministic_recovery =
        recovery_status %in% c(
          "recoverable_single_concept",
          "recoverable_supported_ambiguity"
        ),
      
      recovery_method =
        dplyr::if_else(
          deterministic_recovery,
          "04e_geojapan_synof",
          NA_character_
        )
    )
}

# ------------------------------------------------------------------------------
# Diagnostic-only nomenclatural normalisation
# ------------------------------------------------------------------------------

build_04e_diagnostic_names <- function(
    residuals
) {
  base <- residuals %>%
    dplyr::transmute(
      wcvp_candidate_id =
        as.character(
          wcvp_candidate_id
        ),
      original_name =
        clean_character_04e(
          wcvp_submitted_name
        )
    )
  
  dplyr::bind_rows(
    base %>%
      dplyr::transmute(
        wcvp_candidate_id,
        original_name,
        diagnostic_strategy =
          "hybrid_x_to_multiplication",
        diagnostic_name =
          stringr::str_replace_all(
            original_name,
            "(?<=\\s)[xX](?=\\s)",
            "×"
          )
      ),
    
    base %>%
      dplyr::transmute(
        wcvp_candidate_id,
        original_name,
        diagnostic_strategy =
          "hybrid_prefix_x_to_multiplication",
        diagnostic_name =
          stringr::str_replace(
            original_name,
            "^([A-Z][[:alpha:]-]+)\\s+[xX]\\s+",
            "\\1 × "
          )
      ),
    
    base %>%
      dplyr::transmute(
        wcvp_candidate_id,
        original_name,
        diagnostic_strategy =
          "ssp_to_subsp",
        diagnostic_name =
          stringr::str_replace_all(
            original_name,
            "(?i)(?<![[:alnum:]])ssp\\.(?=\\s)",
            "subsp."
          )
      ),
    
    base %>%
      dplyr::transmute(
        wcvp_candidate_id,
        original_name,
        diagnostic_strategy =
          "forma_to_f",
        diagnostic_name =
          stringr::str_replace_all(
            original_name,
            "(?i)(?<![[:alnum:]])forma(?=\\s)",
            "f."
          )
      )
  ) %>%
    dplyr::mutate(
      diagnostic_name =
        clean_character_04e(
          diagnostic_name
        )
    ) %>%
    dplyr::filter(
      !is.na(
        diagnostic_name
      ),
      diagnostic_name !=
        original_name
    ) %>%
    dplyr::distinct(
      wcvp_candidate_id,
      diagnostic_strategy,
      diagnostic_name,
      .keep_all = TRUE
    )
}

match_04e_diagnostic_names <- function(
    diagnostic_names,
    wcvp
) {
  if (nrow(diagnostic_names) == 0L) {
    return(
      tibble::tibble()
    )
  }
  
  if (!requireNamespace(
    "rWCVP",
    quietly = TRUE
  )) {
    stop(
      "Package rWCVP is required for diagnostic exact matching.",
      call. = FALSE
    )
  }
  
  indexed <- diagnostic_names %>%
    dplyr::mutate(
      diagnostic_name_id =
        paste0(
          "04E_DIAG_",
          stringr::str_pad(
            dplyr::row_number(),
            width = 7,
            pad = "0"
          )
        )
    )
  
  match_input <- indexed %>%
    dplyr::transmute(
      diagnostic_name_id,
      wcvp_submitted_name =
        diagnostic_name
    )
  
  matched <- rWCVP::wcvp_match_exact(
    match_input,
    wcvp_names = wcvp,
    name_col = "wcvp_submitted_name",
    id_col = "diagnostic_name_id"
  )
  
  matched %>%
    dplyr::left_join(
      indexed %>%
        dplyr::select(
          diagnostic_name_id,
          wcvp_candidate_id,
          original_name,
          diagnostic_strategy,
          diagnostic_name
        ),
      by = "diagnostic_name_id"
    )
}

detect_04e_match_fields <- function(
    matches
) {
  list(
    matched_id =
      first_existing_field_04e(
        matches,
        c(
          "plant_name_id",
          "wcvp_id",
          "wcvp_matched_id"
        )
      ),
    
    status =
      first_existing_field_04e(
        matches,
        c(
          "taxon_status",
          "wcvp_status"
        )
      ),
    
    accepted_id =
      first_existing_field_04e(
        matches,
        c(
          "accepted_plant_name_id",
          "wcvp_accepted_id"
        )
      )
  )
}

build_04e_diagnostic_evidence <- function(
    exact_matches,
    wcvp_lookup
) {
  if (nrow(exact_matches) == 0L) {
    return(
      empty_diagnostic_evidence_04e()
    )
  }
  
  required_metadata <- c(
    "wcvp_candidate_id",
    "original_name",
    "diagnostic_strategy",
    "diagnostic_name"
  )
  
  missing_metadata <- setdiff(
    required_metadata,
    names(exact_matches)
  )
  
  if (length(missing_metadata) > 0L) {
    stop(
      "04e diagnostic metadata missing: ",
      paste(
        missing_metadata,
        collapse = ", "
      ),
      call. = FALSE
    )
  }
  
  fields <- detect_04e_match_fields(
    exact_matches
  )
  
  if (
    is.na(fields$matched_id) ||
    is.na(fields$status)
  ) {
    stop(
      "Could not identify required fields in 04e diagnostic WCVP matches.",
      call. = FALSE
    )
  }
  
  x <- exact_matches %>%
    dplyr::mutate(
      matched_wcvp_name_id =
        as.character(
          .data[[fields$matched_id]]
        ),
      
      matched_wcvp_status =
        clean_character_04e(
          .data[[fields$status]]
        ),
      
      accepted_id_04e =
        if (
          is.na(
            fields$accepted_id
          )
        ) {
          NA_character_
        } else {
          as.character(
            .data[[fields$accepted_id]]
          )
        },
      
      diagnostic_concept_id =
        dplyr::case_when(
          matched_wcvp_status %in% c(
            "Accepted",
            "Artificial Hybrid"
          ) ~
            matched_wcvp_name_id,
          
          !is.na(
            accepted_id_04e
          ) ~
            accepted_id_04e,
          
          TRUE ~
            NA_character_
        )
    ) %>%
    dplyr::left_join(
      wcvp_lookup,
      by = c(
        "diagnostic_concept_id" =
          "wcvp_concept_id"
      )
    )
  
  x %>%
    dplyr::transmute(
      wcvp_candidate_id =
        as.character(
          wcvp_candidate_id
        ),
      evidence_type =
        "diagnostic_only",
      diagnostic_strategy,
      original_name,
      diagnostic_name,
      matched_wcvp_name_id,
      matched_wcvp_status,
      diagnostic_concept_id =
        as.character(
          diagnostic_concept_id
        ),
      diagnostic_concept_name =
        wcvp_concept_name,
      diagnostic_concept_status =
        wcvp_concept_status
    ) %>%
    dplyr::distinct()
}

# ------------------------------------------------------------------------------
# Regression validation
# ------------------------------------------------------------------------------

validate_04e_regression_cases <- function(
    assessment
) {
  regression <- assessment %>%
    dplyr::filter(
      wcvp_candidate_id %in%
        EXPECTED_REGRESSION_CANDIDATES
    ) %>%
    dplyr::select(
      wcvp_candidate_id,
      residual_status,
      recovery_status,
      deterministic_recovery,
      selected_concept_id,
      selected_is_existing_ambiguity_concept
    )
  
  assert_equal_04e(
    nrow(regression),
    length(
      EXPECTED_REGRESSION_CANDIDATES
    ),
    "04e explicit regression candidate count"
  )
  
  missing_regression <- setdiff(
    EXPECTED_REGRESSION_CANDIDATES,
    regression$wcvp_candidate_id
  )
  
  if (length(missing_regression) > 0L) {
    stop(
      "Missing explicit 04e regression candidates: ",
      paste(
        missing_regression,
        collapse = ", "
      ),
      call. = FALSE
    )
  }
  
  for (
    candidate_id in
    EXPECTED_REGRESSION_CANDIDATES
  ) {
    observed <- regression %>%
      dplyr::filter(
        wcvp_candidate_id ==
          candidate_id
      )
    
    expected_concept <-
      unname(
        EXPECTED_REGRESSION_CONCEPTS[
          candidate_id
        ]
      )
    
    if (
      nrow(observed) != 1L ||
      !isTRUE(
        observed$deterministic_recovery[[1]]
      ) ||
      observed$residual_status[[1]] !=
      "ambiguous" ||
      observed$recovery_status[[1]] !=
      "recoverable_supported_ambiguity" ||
      observed$selected_concept_id[[1]] !=
      expected_concept ||
      !isTRUE(
        observed$
        selected_is_existing_ambiguity_concept[[1]]
      )
    ) {
      stop(
        "04e explicit regression case failed for ",
        candidate_id,
        ".",
        call. = FALSE
      )
    }
  }
  
  invisible(TRUE)
}

# ------------------------------------------------------------------------------
# Hard validation
# ------------------------------------------------------------------------------

validate_04e_recovery <- function(
    assessment,
    resolution_evidence,
    diagnostic_evidence
) {
  message(
    "Validating 04e deterministic recovery..."
  )
  
  assert_equal_04e(
    nrow(assessment),
    EXPECTED_RESIDUAL_CANDIDATES,
    "04e assessment rows"
  )
  
  assert_equal_04e(
    dplyr::n_distinct(
      assessment$wcvp_candidate_id
    ),
    EXPECTED_RESIDUAL_CANDIDATES,
    "04e distinct candidate IDs"
  )
  
  assert_equal_04e(
    nrow(
      resolution_evidence
    ),
    EXPECTED_04E_SYNOF_EVIDENCE_ROWS,
    "04e SYNOF resolution-evidence rows"
  )
  
  assert_equal_04e(
    sum(
      assessment$deterministic_recovery,
      na.rm = TRUE
    ),
    EXPECTED_04E_RECOVERED,
    "04e deterministic recoveries"
  )
  
  assert_equal_04e(
    sum(
      assessment$deterministic_recovery &
        assessment$residual_status ==
        "unmatched",
      na.rm = TRUE
    ),
    EXPECTED_04E_RECOVERED_UNMATCHED,
    "04e recovered unmatched"
  )
  
  assert_equal_04e(
    sum(
      assessment$deterministic_recovery &
        assessment$residual_status ==
        "ambiguous",
      na.rm = TRUE
    ),
    EXPECTED_04E_RECOVERED_AMBIGUOUS,
    "04e recovered ambiguous"
  )
  
  assert_equal_04e(
    sum(
      assessment$deterministic_recovery &
        assessment$residual_status ==
        "review",
      na.rm = TRUE
    ),
    EXPECTED_04E_RECOVERED_REVIEW,
    "04e recovered review"
  )
  
  assert_equal_04e(
    sum(
      assessment$recovery_status ==
        "conflicting_synof",
      na.rm = TRUE
    ),
    EXPECTED_04E_CONFLICTING_CANDIDATES,
    "04e conflicting SYNOF candidates"
  )
  
  assert_equal_04e(
    sum(
      assessment$recovery_status ==
        "synof_outside_existing_ambiguity",
      na.rm = TRUE
    ),
    EXPECTED_04E_OUTSIDE_AMBIGUITY,
    "04e SYNOF outside existing ambiguity"
  )
  
  bad_method <- assessment %>%
    dplyr::filter(
      deterministic_recovery,
      recovery_method !=
        "04e_geojapan_synof"
    )
  
  if (nrow(bad_method) > 0L) {
    stop(
      "04e recovery found using a non-SYNOF method.",
      call. = FALSE
    )
  }
  
  bad_concept <- assessment %>%
    dplyr::filter(
      deterministic_recovery,
      is.na(
        selected_concept_id
      )
    )
  
  if (nrow(bad_concept) > 0L) {
    stop(
      "04e recovery without a selected WCVP concept.",
      call. = FALSE
    )
  }
  
  bad_conflict <- assessment %>%
    dplyr::filter(
      deterministic_recovery,
      resolution_concept_count > 1L
    )
  
  if (nrow(bad_conflict) > 0L) {
    stop(
      "Conflicting SYNOF evidence marked recoverable.",
      call. = FALSE
    )
  }
  
  bad_ambiguity <- assessment %>%
    dplyr::filter(
      residual_status == "ambiguous",
      deterministic_recovery,
      selected_is_existing_ambiguity_concept %in%
        c(
          FALSE,
          NA
        )
    )
  
  if (nrow(bad_ambiguity) > 0L) {
    stop(
      "Ambiguous recovery outside existing WCVP concept set.",
      call. = FALSE
    )
  }
  
  # Diagnostic evidence is strictly non-resolution-bearing.
  if (
    "evidence_type" %in%
    names(
      diagnostic_evidence
    ) &&
    any(
      diagnostic_evidence$evidence_type !=
      "diagnostic_only",
      na.rm = TRUE
    )
  ) {
    stop(
      "Non-diagnostic evidence detected in diagnostic evidence table.",
      call. = FALSE
    )
  }
  
  validate_04e_regression_cases(
    assessment
  )
  
  post_unmatched <-
    sum(
      assessment$residual_status ==
        "unmatched" &
        !assessment$deterministic_recovery,
      na.rm = TRUE
    )
  
  post_ambiguous <-
    sum(
      assessment$residual_status ==
        "ambiguous" &
        !assessment$deterministic_recovery,
      na.rm = TRUE
    )
  
  post_review <-
    sum(
      assessment$residual_status ==
        "review" &
        !assessment$deterministic_recovery,
      na.rm = TRUE
    )
  
  post_unresolved <-
    post_unmatched +
    post_ambiguous +
    post_review
  
  post_resolved <-
    EXPECTED_04C_RESOLVED +
    sum(
      assessment$deterministic_recovery,
      na.rm = TRUE
    )
  
  assert_equal_04e(
    post_unmatched,
    EXPECTED_POST_04E_UNMATCHED,
    "Post-04e unmatched"
  )
  
  assert_equal_04e(
    post_ambiguous,
    EXPECTED_POST_04E_AMBIGUOUS,
    "Post-04e ambiguous"
  )
  
  assert_equal_04e(
    post_review,
    EXPECTED_POST_04E_REVIEW,
    "Post-04e review"
  )
  
  assert_equal_04e(
    post_unresolved,
    EXPECTED_POST_04E_UNRESOLVED,
    "Post-04e unresolved"
  )
  
  assert_equal_04e(
    post_resolved,
    EXPECTED_POST_04E_RESOLVED,
    "Post-04e deterministic resolved"
  )
  
  assert_equal_04e(
    post_resolved +
      post_unresolved,
    EXPECTED_TOTAL_CANDIDATES,
    "Post-04e total candidate accounting"
  )
  
  message(
    "04e deterministic recovery validation passed."
  )
  
  invisible(TRUE)
}

# ------------------------------------------------------------------------------
# Audits
# ------------------------------------------------------------------------------

build_04e_summary <- function(
    assessment,
    resolution_evidence,
    diagnostic_names,
    diagnostic_evidence
) {
  recovered <-
    sum(
      assessment$deterministic_recovery,
      na.rm = TRUE
    )
  
  post_resolved <-
    EXPECTED_04C_RESOLVED +
    recovered
  
  post_unresolved <-
    EXPECTED_TOTAL_CANDIDATES -
    post_resolved
  
  tibble::tibble(
    metric = c(
      "total_candidate_names",
      "resolved_before_04e",
      "residual_candidates_assessed",
      "synof_resolution_evidence_rows",
      "candidates_with_synof_resolution_evidence",
      "diagnostic_name_variants",
      "diagnostic_wcvp_rows",
      "recovered_by_04e",
      "recovered_unmatched",
      "recovered_ambiguous",
      "recovered_review",
      "conflicting_synof_candidates",
      "synof_outside_existing_ambiguity",
      "resolved_after_04e",
      "unresolved_after_04e",
      "unmatched_after_04e",
      "ambiguous_after_04e",
      "review_after_04e"
    ),
    
    value = c(
      EXPECTED_TOTAL_CANDIDATES,
      EXPECTED_04C_RESOLVED,
      nrow(assessment),
      nrow(
        resolution_evidence
      ),
      dplyr::n_distinct(
        resolution_evidence$
          wcvp_candidate_id
      ),
      nrow(
        diagnostic_names
      ),
      nrow(
        diagnostic_evidence
      ),
      recovered,
      sum(
        assessment$deterministic_recovery &
          assessment$residual_status ==
          "unmatched",
        na.rm = TRUE
      ),
      sum(
        assessment$deterministic_recovery &
          assessment$residual_status ==
          "ambiguous",
        na.rm = TRUE
      ),
      sum(
        assessment$deterministic_recovery &
          assessment$residual_status ==
          "review",
        na.rm = TRUE
      ),
      sum(
        assessment$recovery_status ==
          "conflicting_synof",
        na.rm = TRUE
      ),
      sum(
        assessment$recovery_status ==
          "synof_outside_existing_ambiguity",
        na.rm = TRUE
      ),
      post_resolved,
      post_unresolved,
      sum(
        assessment$residual_status ==
          "unmatched" &
          !assessment$deterministic_recovery,
        na.rm = TRUE
      ),
      sum(
        assessment$residual_status ==
          "ambiguous" &
          !assessment$deterministic_recovery,
        na.rm = TRUE
      ),
      sum(
        assessment$residual_status ==
          "review" &
          !assessment$deterministic_recovery,
        na.rm = TRUE
      )
    )
  )
}

build_04e_status_audit <- function(
    assessment
) {
  assessment %>%
    dplyr::count(
      residual_status,
      recovery_status,
      deterministic_recovery,
      name = "candidate_names",
      sort = TRUE
    )
}

build_04e_resolution_concept_audit <- function(
    assessment
) {
  assessment %>%
    dplyr::filter(
      deterministic_recovery
    ) %>%
    dplyr::count(
      residual_status,
      proposed_concept_status,
      name = "candidate_names",
      sort = TRUE
    )
}

build_04e_diagnostic_strategy_audit <- function(
    diagnostic_evidence
) {
  diagnostic_evidence %>%
    dplyr::count(
      diagnostic_strategy,
      name = "diagnostic_rows",
      sort = TRUE
    )
}

build_04e_conflict_audit <- function(
    assessment
) {
  assessment %>%
    dplyr::filter(
      recovery_status %in% c(
        "conflicting_synof",
        "synof_outside_existing_ambiguity"
      )
    ) %>%
    dplyr::select(
      wcvp_candidate_id,
      wcvp_submitted_name,
      residual_status,
      diagnostic_class,
      source_class,
      recovery_status,
      resolution_concept_count,
      resolution_concept_ids,
      ambiguity_concept_ids,
      resolution_evidence_detail
    ) %>%
    dplyr::arrange(
      recovery_status,
      wcvp_submitted_name
    )
}

build_04e_recoverable_audit <- function(
    assessment
) {
  assessment %>%
    dplyr::filter(
      deterministic_recovery
    ) %>%
    dplyr::select(
      wcvp_candidate_id,
      wcvp_submitted_name,
      residual_status,
      diagnostic_class,
      source_class,
      recovery_status,
      recovery_method,
      resolution_evidence_rows,
      resolution_evidence_detail,
      selected_concept_id,
      proposed_concept_name,
      proposed_concept_authors,
      proposed_concept_family,
      proposed_concept_rank,
      proposed_concept_status
    ) %>%
    dplyr::arrange(
      residual_status,
      wcvp_submitted_name
    )
}

build_04e_regression_audit <- function(
    assessment
) {
  assessment %>%
    dplyr::filter(
      wcvp_candidate_id %in%
        EXPECTED_REGRESSION_CANDIDATES
    ) %>%
    dplyr::select(
      wcvp_candidate_id,
      wcvp_submitted_name,
      residual_status,
      recovery_status,
      recovery_method,
      resolution_evidence_detail,
      selected_concept_id,
      proposed_concept_name,
      proposed_concept_status,
      ambiguity_concept_ids,
      selected_is_existing_ambiguity_concept
    ) %>%
    dplyr::arrange(
      wcvp_candidate_id
    )
}

# ------------------------------------------------------------------------------
# Output writer
# ------------------------------------------------------------------------------

write_04e_outputs <- function(
    resolution_evidence,
    diagnostic_evidence,
    assessment,
    audits
) {
  data_dir <- here::here(
    "data", "interim", "taxonomy", "WCVP",
    "deterministic_recovery"
  )
  
  audit_dir <- here::here(
    "outputs", "tables", "taxonomy", "WCVP",
    "deterministic_recovery"
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
    resolution_evidence,
    file.path(
      data_dir,
      "vpjd_wcvp_04e_resolution_evidence.rds"
    )
  )
  
  readr::write_csv(
    resolution_evidence,
    file.path(
      data_dir,
      "vpjd_wcvp_04e_resolution_evidence.csv"
    )
  )
  
  saveRDS(
    diagnostic_evidence,
    file.path(
      data_dir,
      "vpjd_wcvp_04e_diagnostic_evidence.rds"
    )
  )
  
  readr::write_csv(
    diagnostic_evidence,
    file.path(
      data_dir,
      "vpjd_wcvp_04e_diagnostic_evidence.csv"
    )
  )
  
  saveRDS(
    assessment,
    file.path(
      data_dir,
      "vpjd_wcvp_04e_assessment.rds"
    )
  )
  
  readr::write_csv(
    assessment,
    file.path(
      data_dir,
      "vpjd_wcvp_04e_assessment.csv"
    )
  )
  
  readr::write_csv(
    audits$summary,
    file.path(
      audit_dir,
      "vpjd_wcvp_04e_summary.csv"
    )
  )
  
  readr::write_csv(
    audits$status,
    file.path(
      audit_dir,
      "vpjd_wcvp_04e_status_audit.csv"
    )
  )
  
  readr::write_csv(
    audits$concept,
    file.path(
      audit_dir,
      "vpjd_wcvp_04e_concept_audit.csv"
    )
  )
  
  readr::write_csv(
    audits$diagnostic_strategy,
    file.path(
      audit_dir,
      "vpjd_wcvp_04e_diagnostic_strategy_audit.csv"
    )
  )
  
  readr::write_csv(
    audits$conflicts,
    file.path(
      audit_dir,
      "vpjd_wcvp_04e_conflicts.csv"
    )
  )
  
  readr::write_csv(
    audits$recoverable,
    file.path(
      audit_dir,
      "vpjd_wcvp_04e_recoverable.csv"
    )
  )
  
  readr::write_csv(
    audits$regression,
    file.path(
      audit_dir,
      "vpjd_wcvp_04e_regression_cases.csv"
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

recover_vpjd_wcvp_deterministic <- function(
    write_outputs = TRUE
) {
  message(
    "\nStarting VPJD additional deterministic WCVP recovery..."
  )
  
  message(
    "Deterministic recovery module version: ",
    WCVP_DETERMINISTIC_RECOVERY_VERSION
  )
  
  residuals <-
    load_04e_residual_diagnostics()
  
  consolidated_candidates <-
    load_04e_consolidated_candidates()
  
  consolidated_source_crosswalk <-
    load_04e_consolidated_source_crosswalk()
  
  source_candidate_crosswalk <-
    load_04e_source_candidate_crosswalk()
  
  geojapan_taxa <-
    load_04e_geojapan_taxa()
  
  raw_matches <-
    load_04e_raw_matches()
  
  wcvp <-
    load_04e_wcvp_names()
  
  validate_04e_baseline(
    residuals,
    consolidated_candidates,
    consolidated_source_crosswalk
  )
  
  message(
    "Building GEOJAPAN SYNOF resolution evidence..."
  )
  
  synof_edges <-
    build_04e_synof_edges(
      residuals,
      source_candidate_crosswalk,
      geojapan_taxa
    )
  
  target_lookup <-
    build_04e_target_lookup(
      source_candidate_crosswalk,
      consolidated_candidates
    )
  
  resolution_evidence <-
    build_04e_resolution_evidence(
      synof_edges,
      target_lookup
    )
  
  message(
    "SYNOF resolution-evidence rows: ",
    format(
      nrow(
        resolution_evidence
      ),
      big.mark = ","
    )
  )
  
  ambiguity_concepts <-
    build_04e_existing_ambiguity_concepts(
      residuals,
      raw_matches
    )
  
  assessment <-
    assess_04e_resolution(
      residuals,
      resolution_evidence,
      ambiguity_concepts
    )
  
  message(
    "Building diagnostic-only nomenclatural variants..."
  )
  
  diagnostic_names <-
    build_04e_diagnostic_names(
      residuals
    )
  
  message(
    "Diagnostic name variants: ",
    format(
      nrow(
        diagnostic_names
      ),
      big.mark = ","
    )
  )
  
  wcvp_lookup <-
    build_04e_wcvp_lookup(
      wcvp
    )
  
  if (
    nrow(
      diagnostic_names
    ) > 0L
  ) {
    diagnostic_matches <-
      match_04e_diagnostic_names(
        diagnostic_names,
        wcvp
      )
    
    diagnostic_evidence <-
      build_04e_diagnostic_evidence(
        diagnostic_matches,
        wcvp_lookup
      )
  } else {
    diagnostic_matches <-
      tibble::tibble()
    
    diagnostic_evidence <-
      empty_diagnostic_evidence_04e()
  }
  
  message(
    "Diagnostic WCVP rows retained: ",
    format(
      nrow(
        diagnostic_evidence
      ),
      big.mark = ","
    )
  )
  
  validate_04e_recovery(
    assessment,
    resolution_evidence,
    diagnostic_evidence
  )
  
  audits <- list(
    summary =
      build_04e_summary(
        assessment,
        resolution_evidence,
        diagnostic_names,
        diagnostic_evidence
      ),
    
    status =
      build_04e_status_audit(
        assessment
      ),
    
    concept =
      build_04e_resolution_concept_audit(
        assessment
      ),
    
    diagnostic_strategy =
      build_04e_diagnostic_strategy_audit(
        diagnostic_evidence
      ),
    
    conflicts =
      build_04e_conflict_audit(
        assessment
      ),
    
    recoverable =
      build_04e_recoverable_audit(
        assessment
      ),
    
    regression =
      build_04e_regression_audit(
        assessment
      )
  )
  
  if (write_outputs) {
    paths <-
      write_04e_outputs(
        resolution_evidence,
        diagnostic_evidence,
        assessment,
        audits
      )
  } else {
    paths <- NULL
  }
  
  recovered <-
    sum(
      assessment$deterministic_recovery,
      na.rm = TRUE
    )
  
  post_resolved <-
    EXPECTED_04C_RESOLVED +
    recovered
  
  post_unresolved <-
    EXPECTED_TOTAL_CANDIDATES -
    post_resolved
  
  message(
    "\nVPJD additional deterministic WCVP recovery complete."
  )
  
  message(
    "Residual candidates assessed: ",
    format(
      nrow(
        assessment
      ),
      big.mark = ","
    )
  )
  
  message(
    "Deterministically recovered via GEOJAPAN SYNOF: ",
    format(
      recovered,
      big.mark = ","
    )
  )
  
  message(
    "Resolved after 04e: ",
    format(
      post_resolved,
      big.mark = ","
    ),
    " / ",
    format(
      EXPECTED_TOTAL_CANDIDATES,
      big.mark = ","
    ),
    " (",
    sprintf(
      "%.2f",
      100 *
        post_resolved /
        EXPECTED_TOTAL_CANDIDATES
    ),
    "%)"
  )
  
  message(
    "Remaining unresolved: ",
    format(
      post_unresolved,
      big.mark = ","
    )
  )
  
  message(
    "  unmatched: ",
    format(
      sum(
        assessment$residual_status ==
          "unmatched" &
          !assessment$deterministic_recovery,
        na.rm = TRUE
      ),
      big.mark = ","
    )
  )
  
  message(
    "  ambiguous: ",
    format(
      sum(
        assessment$residual_status ==
          "ambiguous" &
          !assessment$deterministic_recovery,
        na.rm = TRUE
      ),
      big.mark = ","
    )
  )
  
  message(
    "  review: ",
    format(
      sum(
        assessment$residual_status ==
          "review" &
          !assessment$deterministic_recovery,
        na.rm = TRUE
      ),
      big.mark = ","
    )
  )
  
  list(
    synof_edges =
      synof_edges,
    
    target_lookup =
      target_lookup,
    
    resolution_evidence =
      resolution_evidence,
    
    ambiguity_concepts =
      ambiguity_concepts,
    
    assessment =
      assessment,
    
    diagnostic_names =
      diagnostic_names,
    
    diagnostic_matches =
      diagnostic_matches,
    
    diagnostic_evidence =
      diagnostic_evidence,
    
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
  "04e_wcvp_deterministic_recovery.R v",
  WCVP_DETERMINISTIC_RECOVERY_VERSION,
  " loaded."
)

message(
  "Run recover_vpjd_wcvp_deterministic() to apply validated ",
  "GEOJAPAN SYNOF recovery and retain name normalisation as ",
  "diagnostic evidence only."
)