# =============================================================================
# 04_wcvp_standardise.R
#
# VPJD / OJPCP
# WCVP taxonomic reconciliation
#
# Version: 0.3.3
#
# PURPOSE
# -------
# Reconcile the VPJD source taxon-name universe against WCVP while preserving
# the complete one-to-many nomenclatural evidence returned by rWCVP.
#
# INPUT
# -----
# data/interim/taxonomy/wcvp_candidate_names.rds
# data/interim/taxonomy/source_to_wcvp_candidate.rds
#
# CREATED BY
# ----------
# R/taxonomy/03_build_source_taxon_universe.R
#
#
# CONCEPTUAL MODEL
# ----------------
#
# VPJD source name
#       |
#       v
# WCVP submitted-name match
#       |
#       v
# WCVP MATCHED NAME
#       |
#       v
# WCVP RESOLVED CONCEPT
#       |
#       +-- Accepted -----------> resolved
#       |
#       +-- Artificial Hybrid --> resolved_hybrid
#       |
#       +-- Unplaced -----------> review
#       |
#       +-- no terminal concept -> review
#
#
# MULTIPLE MATCHES
# ----------------
#
# Multiple WCVP rows
#       |
#       v
# distinct resolvable concepts?
#       |
#       +-- 1 -> convergent_multiple
#       |
#       +-- >1 -> divergent_multiple / ambiguous
#
#
# PRINCIPLES
# ----------
# 1. Exact WCVP matching only.
# 2. Fuzzy matching is not performed automatically.
# 3. Raw one-to-many WCVP evidence is never discarded.
# 4. Matched-name status and resolved-concept status are distinct.
# 5. "Accepted" retains its WCVP meaning.
# 6. Artificial Hybrid is a valid terminal WCVP concept but is explicitly
#    distinguished from an Accepted taxon.
# 7. Unplaced concepts are not silently promoted to resolved taxa.
# 8. Multiple matches are resolved automatically only where all usable
#    alternatives converge on one terminal concept.
# 9. Source provenance is preserved.
# 10. Raw source data are never modified.
# 11. Raw WCVP matching is cached independently of downstream reconciliation.
# =============================================================================


# =============================================================================
# 0. PACKAGES
# =============================================================================

suppressPackageStartupMessages({
  
  library(dplyr)
  library(stringr)
  library(readr)
  library(here)
  library(rWCVP)
  library(rWCVPdata)
  
})


WCVP_STANDARDISE_VERSION <- "0.3.3"


# =============================================================================
# 1. PATHS
# =============================================================================

wcvp_candidate_path <- here::here(
  "data",
  "interim",
  "taxonomy",
  "wcvp_candidate_names.rds"
)


source_candidate_crosswalk_path <- here::here(
  "data",
  "interim",
  "taxonomy",
  "source_to_wcvp_candidate.rds"
)


wcvp_taxonomy_dir <- here::here(
  "data",
  "interim",
  "taxonomy",
  "WCVP"
)


wcvp_taxonomy_audit_dir <- here::here(
  "outputs",
  "tables",
  "taxonomy",
  "WCVP"
)


wcvp_raw_match_cache_path <- file.path(
  wcvp_taxonomy_dir,
  "vpjd_wcvp_raw_match_cache.rds"
)


dir.create(
  wcvp_taxonomy_dir,
  recursive = TRUE,
  showWarnings = FALSE
)


dir.create(
  wcvp_taxonomy_audit_dir,
  recursive = TRUE,
  showWarnings = FALSE
)


# =============================================================================
# 2. LOAD WCVP
# =============================================================================

get_wcvp_names <- function() {
  
  data(
    "wcvp_names",
    package = "rWCVPdata",
    envir = environment()
  )
  
  
  if (!exists(
    "wcvp_names",
    inherits = FALSE
  )) {
    
    stop(
      "Unable to load rWCVPdata::wcvp_names."
    )
  }
  
  
  wcvp <- get(
    "wcvp_names",
    envir = environment()
  )
  
  
  if (!is.data.frame(
    wcvp
  )) {
    
    stop(
      "Loaded WCVP object is not a data frame."
    )
  }
  
  
  wcvp
}


# =============================================================================
# 3. BUILD COMPLETE WCVP CONCEPT LOOKUP
# =============================================================================

build_wcvp_concept_lookup <- function(
    wcvp
) {
  
  required <- c(
    "plant_name_id",
    "taxon_name",
    "taxon_authors",
    "family",
    "genus",
    "species",
    "taxon_rank",
    "taxon_status",
    "accepted_plant_name_id"
  )
  
  
  missing <- setdiff(
    required,
    names(
      wcvp
    )
  )
  
  
  if (length(
    missing
  ) > 0) {
    
    stop(
      "Required WCVP field(s) missing: ",
      paste(
        missing,
        collapse = ", "
      )
    )
  }
  
  
  wcvp %>%
    
    transmute(
      
      wcvp_concept_id =
        as.character(
          plant_name_id
        ),
      
      wcvp_concept_name =
        taxon_name,
      
      wcvp_concept_authors =
        taxon_authors,
      
      wcvp_concept_family =
        family,
      
      wcvp_concept_genus =
        genus,
      
      wcvp_concept_species =
        species,
      
      wcvp_concept_rank =
        taxon_rank,
      
      wcvp_concept_status =
        taxon_status,
      
      wcvp_concept_accepted_id =
        as.character(
          accepted_plant_name_id
        )
      
    ) %>%
    
    distinct(
      wcvp_concept_id,
      .keep_all = TRUE
    )
}


# =============================================================================
# 4. RETAIN LEGACY ACCEPTED LOOKUP FOR COMPATIBILITY
# =============================================================================

build_accepted_wcvp_lookup <- function(
    wcvp
) {
  
  build_wcvp_concept_lookup(
    wcvp
  ) %>%
    
    filter(
      wcvp_concept_status ==
        "Accepted"
    ) %>%
    
    transmute(
      
      wcvp_accepted_id =
        wcvp_concept_id,
      
      wcvp_accepted_name =
        wcvp_concept_name,
      
      wcvp_accepted_authors =
        wcvp_concept_authors,
      
      wcvp_accepted_family =
        wcvp_concept_family,
      
      wcvp_accepted_genus =
        wcvp_concept_genus,
      
      wcvp_accepted_species =
        wcvp_concept_species,
      
      wcvp_accepted_rank =
        wcvp_concept_rank
    )
}


# =============================================================================
# 5. GENERAL-PURPOSE EXACT WCVP STANDARDISATION
# =============================================================================

standardise_wcvp <- function(
    input,
    label,
    name_col = "gbif_binom",
    id_col = NULL
) {
  
  if (!is.data.frame(
    input
  )) {
    
    stop(
      "`input` must be a data frame or tibble."
    )
  }
  
  
  if (!name_col %in%
      names(
        input
      )) {
    
    stop(
      "Name column not found in input: ",
      name_col
    )
  }
  
  
  temporary_id_created <- FALSE
  
  
  if (is.null(
    id_col
  )) {
    
    id_col <- ".vpjd_wcvp_row_id"
    
    input[[id_col]] <-
      seq_len(
        nrow(
          input
        )
      )
    
    temporary_id_created <- TRUE
    
  } else {
    
    if (!id_col %in%
        names(
          input
        )) {
      
      stop(
        "ID column not found in input: ",
        id_col
      )
    }
  }
  
  
  if (anyDuplicated(
    input[[id_col]]
  )) {
    
    stop(
      "ID column contains duplicate values: ",
      id_col
    )
  }
  
  
  wcvp <- get_wcvp_names()
  
  
  message(
    "Matching ",
    format(
      nrow(
        input
      ),
      big.mark = ","
    ),
    " records against WCVP..."
  )
  
  
  matched <-
    rWCVP::wcvp_match_exact(
      input,
      wcvp_names = wcvp,
      name_col = name_col,
      id_col = id_col
    )
  
  
  matched_n <-
    matched %>%
    
    filter(
      !is.na(
        wcvp_id
      )
    ) %>%
    
    distinct(
      .data[[id_col]]
    ) %>%
    
    nrow()
  
  
  message(
    "WCVP match: ",
    format(
      matched_n,
      big.mark = ","
    ),
    "/",
    format(
      nrow(
        input
      ),
      big.mark = ","
    ),
    " submitted records."
  )
  
  
  if (isTRUE(
    temporary_id_created
  )) {
    
    matched <-
      matched %>%
      
      select(
        -all_of(
          id_col
        )
      )
  }
  
  
  interim_dir <- here::here(
    "data",
    "interim"
  )
  
  
  audit_dir <- here::here(
    "outputs",
    "tables"
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
    matched,
    file.path(
      interim_dir,
      paste0(
        label,
        "_wcvp.rds"
      )
    )
  )
  
  
  readr::write_csv(
    matched,
    file.path(
      interim_dir,
      paste0(
        label,
        "_wcvp.csv"
      )
    ),
    na = ""
  )
  
  
  audit <-
    matched %>%
    
    count(
      match_type,
      wcvp_status,
      sort = TRUE,
      name = "match_rows"
    )
  
  
  readr::write_csv(
    audit,
    file.path(
      audit_dir,
      paste0(
        label,
        "_wcvp_match_audit.csv"
      )
    ),
    na = ""
  )
  
  
  invisible(
    matched
  )
}


# =============================================================================
# 6. LOAD VPJD CANDIDATES
# =============================================================================

load_vpjd_wcvp_candidates <- function() {
  
  if (!file.exists(
    wcvp_candidate_path
  )) {
    
    stop(
      "VPJD WCVP candidate table not found: ",
      wcvp_candidate_path,
      "\nRun 03_build_source_taxon_universe.R first."
    )
  }
  
  
  candidates <-
    readRDS(
      wcvp_candidate_path
    )
  
  
  required <- c(
    "wcvp_candidate_id",
    "wcvp_submitted_name",
    "source_record_count",
    "geojapan_record_count",
    "foj_record_count"
  )
  
  
  missing <- setdiff(
    required,
    names(
      candidates
    )
  )
  
  
  if (length(
    missing
  ) > 0) {
    
    stop(
      "Required WCVP candidate field(s) missing: ",
      paste(
        missing,
        collapse = ", "
      )
    )
  }
  
  
  if (anyDuplicated(
    candidates$wcvp_candidate_id
  )) {
    
    stop(
      "Duplicate wcvp_candidate_id values detected."
    )
  }
  
  
  blank_name <-
    is.na(
      candidates$wcvp_submitted_name
    ) |
    !nzchar(
      trimws(
        candidates$wcvp_submitted_name
      )
    )
  
  
  if (any(
    blank_name
  )) {
    
    stop(
      "Missing or blank WCVP submitted names detected."
    )
  }
  
  
  candidates
}


# =============================================================================
# 7. VALIDATE RAW WCVP MATCH CACHE
# =============================================================================

validate_wcvp_raw_matches <- function(
    matches,
    candidates
) {
  
  required <- c(
    "wcvp_candidate_id",
    "wcvp_submitted_name",
    "match_type",
    "multiple_matches",
    "wcvp_id",
    "wcvp_name",
    "wcvp_authors",
    "wcvp_rank",
    "wcvp_status",
    "wcvp_accepted_id"
  )
  
  
  missing_fields <- setdiff(
    required,
    names(
      matches
    )
  )
  
  
  if (length(
    missing_fields
  ) > 0) {
    
    stop(
      "Raw WCVP match table is missing field(s): ",
      paste(
        missing_fields,
        collapse = ", "
      )
    )
  }
  
  
  returned_ids <-
    unique(
      matches$wcvp_candidate_id
    )
  
  
  missing_ids <- setdiff(
    candidates$wcvp_candidate_id,
    returned_ids
  )
  
  
  if (length(
    missing_ids
  ) > 0) {
    
    stop(
      "Raw WCVP matches are missing ",
      length(
        missing_ids
      ),
      " candidate IDs."
    )
  }
  
  
  extra_ids <- setdiff(
    returned_ids,
    candidates$wcvp_candidate_id
  )
  
  
  if (length(
    extra_ids
  ) > 0) {
    
    stop(
      "Raw WCVP matches contain ",
      length(
        extra_ids
      ),
      " unexpected candidate IDs."
    )
  }
  
  
  candidate_name_check <-
    matches %>%
    
    distinct(
      wcvp_candidate_id,
      wcvp_submitted_name
    ) %>%
    
    inner_join(
      candidates %>%
        select(
          wcvp_candidate_id,
          candidate_name =
            wcvp_submitted_name
        ),
      by = "wcvp_candidate_id"
    ) %>%
    
    filter(
      wcvp_submitted_name !=
        candidate_name
    )
  
  
  if (nrow(
    candidate_name_check
  ) > 0) {
    
    stop(
      "Raw WCVP match cache does not correspond to the current candidate ",
      "universe. Re-run with use_cache = FALSE."
    )
  }
  
  
  invisible(
    TRUE
  )
}


# =============================================================================
# 8. MATCH CANDIDATES TO WCVP
# =============================================================================

match_vpjd_candidates_to_wcvp <- function(
    candidates,
    wcvp = get_wcvp_names(),
    use_cache = TRUE
) {
  
  if (
    isTRUE(
      use_cache
    ) &&
    file.exists(
      wcvp_raw_match_cache_path
    )
  ) {
    
    message("")
    message(
      "Loading cached VPJD/WCVP raw matches..."
    )
    
    
    cached_matches <-
      readRDS(
        wcvp_raw_match_cache_path
      )
    
    
    cache_valid <-
      tryCatch(
        
        {
          
          validate_wcvp_raw_matches(
            cached_matches,
            candidates
          )
          
          TRUE
        },
        
        error = function(
    e
        ) {
          
          message(
            "Existing WCVP cache is not valid for the current candidate universe."
          )
          
          message(
            "Cache validation: ",
            conditionMessage(
              e
            )
          )
          
          FALSE
        }
      )
    
    
    if (isTRUE(
      cache_valid
    )) {
      
      message(
        "Cached WCVP matches validated: ",
        format(
          nrow(
            cached_matches
          ),
          big.mark = ","
        ),
        " rows."
      )
      
      
      return(
        cached_matches
      )
    }
  }
  
  
  message("")
  message(
    "Matching ",
    format(
      nrow(
        candidates
      ),
      big.mark = ","
    ),
    " VPJD candidate names against WCVP..."
  )
  
  
  match_input <-
    candidates %>%
    
    select(
      wcvp_candidate_id,
      wcvp_submitted_name
    )
  
  
  matches <-
    rWCVP::wcvp_match_exact(
      match_input,
      wcvp_names = wcvp,
      name_col = "wcvp_submitted_name",
      id_col = "wcvp_candidate_id"
    )
  
  
  validate_wcvp_raw_matches(
    matches,
    candidates
  )
  
  
  message(
    "WCVP returned ",
    format(
      nrow(
        matches
      ),
      big.mark = ","
    ),
    " match rows for ",
    format(
      nrow(
        candidates
      ),
      big.mark = ","
    ),
    " candidate names."
  )
  
  
  saveRDS(
    matches,
    wcvp_raw_match_cache_path
  )
  
  
  message(
    "Raw WCVP match cache saved: ",
    wcvp_raw_match_cache_path
  )
  
  
  matches
}


# =============================================================================
# 9. NORMALISE MATCHED NAMES TO TERMINAL WCVP CONCEPT IDS
# =============================================================================

normalise_wcvp_match_concepts <- function(
    matches
) {
  
  required <- c(
    "wcvp_candidate_id",
    "wcvp_submitted_name",
    "match_type",
    "multiple_matches",
    "wcvp_id",
    "wcvp_name",
    "wcvp_authors",
    "wcvp_rank",
    "wcvp_status",
    "wcvp_accepted_id"
  )
  
  
  missing <- setdiff(
    required,
    names(
      matches
    )
  )
  
  
  if (length(
    missing
  ) > 0) {
    
    stop(
      "Expected rWCVP field(s) missing: ",
      paste(
        missing,
        collapse = ", "
      )
    )
  }
  
  
  matches %>%
    
    mutate(
      
      wcvp_matched_id =
        as.character(
          wcvp_id
        ),
      
      wcvp_matched_name =
        wcvp_name,
      
      wcvp_matched_authors =
        wcvp_authors,
      
      wcvp_matched_rank =
        wcvp_rank,
      
      wcvp_matched_status =
        wcvp_status,
      
      wcvp_target_id_raw =
        as.character(
          wcvp_accepted_id
        ),
      
      # -----------------------------------------------------------------------
      # TERMINAL CONCEPT ID
      #
      # Accepted:
      #   matched record itself.
      #
      # Artificial Hybrid:
      #   WCVP may use the hybrid record itself as the terminal concept.
      #
      # Other nomenclatural statuses:
      #   follow WCVP's supplied target where present.
      #
      # No target:
      #   retain NA here; the matched record remains available for review.
      # -----------------------------------------------------------------------
      
      wcvp_resolved_concept_id =
        case_when(
          
          is.na(
            wcvp_matched_id
          ) ~
            NA_character_,
          
          wcvp_matched_status ==
            "Accepted" ~
            wcvp_matched_id,
          
          wcvp_matched_status ==
            "Artificial Hybrid" &
            !is.na(
              wcvp_target_id_raw
            ) ~
            wcvp_target_id_raw,
          
          !is.na(
            wcvp_target_id_raw
          ) ~
            wcvp_target_id_raw,
          
          TRUE ~
            NA_character_
        )
    )
}


# =============================================================================
# 10. ATTACH RESOLVED-CONCEPT METADATA TO RAW MATCHES
# =============================================================================

attach_wcvp_concept_metadata <- function(
    normalised_matches,
    wcvp
) {
  
  concept_lookup <-
    build_wcvp_concept_lookup(
      wcvp
    )
  
  
  normalised_matches %>%
    
    left_join(
      concept_lookup,
      by = c(
        "wcvp_resolved_concept_id" =
          "wcvp_concept_id"
      )
    ) %>%
    
    mutate(
      
      # A terminal concept is suitable for automatic VPJD reconciliation
      # only where WCVP itself classifies it as Accepted or Artificial Hybrid.
      
      wcvp_concept_is_resolvable =
        case_when(
          
          is.na(
            wcvp_resolved_concept_id
          ) ~
            FALSE,
          
          wcvp_concept_status %in%
            c(
              "Accepted",
              "Artificial Hybrid"
            ) ~
            TRUE,
          
          TRUE ~
            FALSE
        ),
      
      wcvp_concept_is_hybrid =
        wcvp_concept_status ==
        "Artificial Hybrid"
    )
}


# =============================================================================
# 11. BUILD MATCH MULTIPLICITY
# =============================================================================

build_wcvp_match_multiplicity <- function(
    concept_matches
) {
  
  concept_matches %>%
    
    group_by(
      wcvp_candidate_id,
      wcvp_submitted_name
    ) %>%
    
    summarise(
      
      wcvp_match_rows =
        n(),
      
      matched_wcvp_records =
        n_distinct(
          wcvp_matched_id,
          na.rm = TRUE
        ),
      
      terminal_concept_count =
        n_distinct(
          wcvp_resolved_concept_id[
            wcvp_concept_is_resolvable %in%
              TRUE
          ],
          na.rm = TRUE
        ),
      
      non_resolvable_concept_count =
        n_distinct(
          wcvp_resolved_concept_id[
            !wcvp_concept_is_resolvable &
              !is.na(
                wcvp_resolved_concept_id
              )
          ],
          na.rm = TRUE
        ),
      
      matched_statuses =
        {
          
          statuses <-
            sort(
              unique(
                wcvp_matched_status[
                  !is.na(
                    wcvp_matched_status
                  )
                ]
              )
            )
          
          if (length(
            statuses
          ) == 0) {
            
            NA_character_
            
          } else {
            
            paste(
              statuses,
              collapse = " | "
            )
          }
        },
      
      .groups = "drop"
    )
}


# =============================================================================
# 12. RESOLVE CANDIDATES
# =============================================================================

resolve_wcvp_candidates <- function(
    candidates,
    concept_matches
) {
  
  candidate_resolution <-
    concept_matches %>%
    
    group_by(
      wcvp_candidate_id,
      wcvp_submitted_name
    ) %>%
    
    summarise(
      
      wcvp_match_rows =
        n(),
      
      matched_wcvp_records =
        n_distinct(
          wcvp_matched_id,
          na.rm = TRUE
        ),
      
      resolvable_concept_count =
        n_distinct(
          wcvp_resolved_concept_id[
            wcvp_concept_is_resolvable %in%
              TRUE
          ],
          na.rm = TRUE
        ),
      
      non_resolvable_concept_count =
        n_distinct(
          wcvp_resolved_concept_id[
            !wcvp_concept_is_resolvable &
              !is.na(
                wcvp_resolved_concept_id
              )
          ],
          na.rm = TRUE
        ),
      
      wcvp_resolved_concept_id =
        {
          
          ids <-
            unique(
              wcvp_resolved_concept_id[
                wcvp_concept_is_resolvable %in%
                  TRUE &
                  !is.na(
                    wcvp_resolved_concept_id
                  )
              ]
            )
          
          if (length(
            ids
          ) == 1) {
            
            ids[[1]]
            
          } else {
            
            NA_character_
          }
        },
      
      matched_statuses =
        {
          
          statuses <-
            sort(
              unique(
                wcvp_matched_status[
                  !is.na(
                    wcvp_matched_status
                  )
                ]
              )
            )
          
          if (length(
            statuses
          ) == 0) {
            
            NA_character_
            
          } else {
            
            paste(
              statuses,
              collapse = " | "
            )
          }
        },
      
      any_multiple_match_flag =
        any(
          multiple_matches %in%
            TRUE,
          na.rm = TRUE
        ),
      
      any_unplaced_match =
        any(
          wcvp_matched_status ==
            "Unplaced",
          na.rm = TRUE
        ),
      
      .groups = "drop"
    )
  
  
  # ---------------------------------------------------------------------------
  # Attach the metadata of the selected terminal concept.
  # ---------------------------------------------------------------------------
  
  selected_concept_metadata <-
    concept_matches %>%
    
    filter(
      wcvp_concept_is_resolvable %in%
        TRUE,
      !is.na(
        wcvp_resolved_concept_id
      )
    ) %>%
    
    select(
      wcvp_resolved_concept_id,
      wcvp_concept_name,
      wcvp_concept_authors,
      wcvp_concept_family,
      wcvp_concept_genus,
      wcvp_concept_species,
      wcvp_concept_rank,
      wcvp_concept_status
    ) %>%
    
    distinct(
      wcvp_resolved_concept_id,
      .keep_all = TRUE
    )
  
  
  candidate_resolution <-
    candidate_resolution %>%
    
    left_join(
      selected_concept_metadata,
      by = "wcvp_resolved_concept_id"
    )
  
  
  # ---------------------------------------------------------------------------
  # Candidate-level reconciliation classification.
  # ---------------------------------------------------------------------------
  
  candidate_resolution <-
    candidate_resolution %>%
    
    mutate(
      
      wcvp_resolution_method =
        case_when(
          
          matched_wcvp_records ==
            0 ~
            "unmatched",
          
          resolvable_concept_count >
            1 ~
            "divergent_multiple",
          
          resolvable_concept_count ==
            1 &
            matched_wcvp_records >
            1 ~
            "convergent_multiple",
          
          resolvable_concept_count ==
            1 &
            matched_wcvp_records ==
            1 ~
            "unique_exact",
          
          resolvable_concept_count ==
            0 &
            any_unplaced_match ~
            "unplaced",
          
          resolvable_concept_count ==
            0 &
            matched_wcvp_records >
            0 ~
            "matched_no_resolvable_concept",
          
          TRUE ~
            "review"
        ),
      
      wcvp_reconciliation_status =
        case_when(
          
          wcvp_resolution_method ==
            "divergent_multiple" ~
            "ambiguous",
          
          wcvp_resolution_method ==
            "unmatched" ~
            "unmatched",
          
          wcvp_resolution_method ==
            "unplaced" ~
            "review",
          
          wcvp_resolution_method ==
            "matched_no_resolvable_concept" ~
            "review",
          
          wcvp_resolution_method %in%
            c(
              "unique_exact",
              "convergent_multiple"
            ) &
            wcvp_concept_status ==
            "Accepted" ~
            "resolved",
          
          wcvp_resolution_method %in%
            c(
              "unique_exact",
              "convergent_multiple"
            ) &
            wcvp_concept_status ==
            "Artificial Hybrid" ~
            "resolved_hybrid",
          
          TRUE ~
            "review"
        ),
      
      wcvp_is_artificial_hybrid =
        wcvp_reconciliation_status ==
        "resolved_hybrid"
    )
  
  
  # ---------------------------------------------------------------------------
  # Attach source provenance.
  # ---------------------------------------------------------------------------
  
  candidate_metadata_fields <- c(
    "wcvp_candidate_id",
    "source_record_count",
    "geojapan_record_count",
    "foj_record_count"
  )
  
  
  if ("source_repositories" %in%
      names(
        candidates
      )) {
    
    candidate_metadata_fields <-
      c(
        candidate_metadata_fields,
        "source_repositories"
      )
  }
  
  
  candidate_metadata <-
    candidates %>%
    
    select(
      all_of(
        candidate_metadata_fields
      )
    )
  
  
  candidate_resolution %>%
    
    left_join(
      candidate_metadata,
      by = "wcvp_candidate_id"
    )
}


# =============================================================================
# 13. VALIDATE CANDIDATE RESOLUTION
# =============================================================================

validate_wcvp_candidate_resolution <- function(
    candidates,
    candidate_resolution
) {
  
  if (nrow(
    candidate_resolution
  ) !=
  nrow(
    candidates
  )) {
    
    stop(
      "Candidate-level WCVP resolution does not contain exactly one row ",
      "per candidate. Candidates: ",
      nrow(
        candidates
      ),
      "; resolution rows: ",
      nrow(
        candidate_resolution
      ),
      "."
    )
  }
  
  
  if (anyDuplicated(
    candidate_resolution$wcvp_candidate_id
  )) {
    
    stop(
      "Duplicate candidate IDs detected in candidate-level WCVP resolution."
    )
  }
  
  
  missing_ids <-
    setdiff(
      candidates$wcvp_candidate_id,
      candidate_resolution$wcvp_candidate_id
    )
  
  
  if (length(
    missing_ids
  ) > 0) {
    
    stop(
      "Candidate IDs lost during WCVP resolution: ",
      length(
        missing_ids
      )
    )
  }
  
  
  # ---------------------------------------------------------------------------
  # Ordinary resolved concepts must actually be Accepted in WCVP.
  # ---------------------------------------------------------------------------
  
  invalid_resolved <-
    candidate_resolution %>%
    
    filter(
      wcvp_reconciliation_status ==
        "resolved",
      is.na(
        wcvp_resolved_concept_id
      ) |
        wcvp_concept_status !=
        "Accepted"
    )
  
  
  if (nrow(
    invalid_resolved
  ) > 0) {
    
    stop(
      "Ordinary resolved candidates without an Accepted WCVP concept: ",
      nrow(
        invalid_resolved
      )
    )
  }
  
  
  # ---------------------------------------------------------------------------
  # Resolved hybrids must actually be Artificial Hybrid concepts.
  # ---------------------------------------------------------------------------
  
  invalid_hybrids <-
    candidate_resolution %>%
    
    filter(
      wcvp_reconciliation_status ==
        "resolved_hybrid",
      is.na(
        wcvp_resolved_concept_id
      ) |
        wcvp_concept_status !=
        "Artificial Hybrid"
    )
  
  
  if (nrow(
    invalid_hybrids
  ) > 0) {
    
    stop(
      "Resolved hybrids without an Artificial Hybrid WCVP concept: ",
      nrow(
        invalid_hybrids
      )
    )
  }
  
  
  # ---------------------------------------------------------------------------
  # Ambiguous records must not have a selected terminal concept.
  # ---------------------------------------------------------------------------
  
  invalid_ambiguous <-
    candidate_resolution %>%
    
    filter(
      wcvp_reconciliation_status ==
        "ambiguous",
      !is.na(
        wcvp_resolved_concept_id
      )
    )
  
  
  if (nrow(
    invalid_ambiguous
  ) > 0) {
    
    stop(
      "Ambiguous candidates were assigned terminal WCVP concepts."
    )
  }
  
  
  invisible(
    TRUE
  )
}


# =============================================================================
# 14. BUILD DISTINCT RESOLVED WCVP CONCEPT UNIVERSE
# =============================================================================

build_vpjd_wcvp_concepts <- function(
    candidate_resolution
) {
  
  candidate_resolution %>%
    
    filter(
      wcvp_reconciliation_status %in%
        c(
          "resolved",
          "resolved_hybrid"
        ),
      !is.na(
        wcvp_resolved_concept_id
      )
    ) %>%
    
    group_by(
      wcvp_resolved_concept_id
    ) %>%
    
    summarise(
      
      wcvp_concept_name =
        first(
          wcvp_concept_name
        ),
      
      wcvp_concept_authors =
        first(
          wcvp_concept_authors
        ),
      
      wcvp_concept_family =
        first(
          wcvp_concept_family
        ),
      
      wcvp_concept_genus =
        first(
          wcvp_concept_genus
        ),
      
      wcvp_concept_species =
        first(
          wcvp_concept_species
        ),
      
      wcvp_concept_rank =
        first(
          wcvp_concept_rank
        ),
      
      wcvp_concept_status =
        first(
          wcvp_concept_status
        ),
      
      candidate_name_count =
        n(),
      
      source_record_count =
        sum(
          source_record_count,
          na.rm = TRUE
        ),
      
      geojapan_record_count =
        sum(
          geojapan_record_count,
          na.rm = TRUE
        ),
      
      foj_record_count =
        sum(
          foj_record_count,
          na.rm = TRUE
        ),
      
      .groups = "drop"
    ) %>%
    
    arrange(
      wcvp_concept_family,
      wcvp_concept_genus,
      wcvp_concept_name
    )
}


# =============================================================================
# 15. BUILD SOURCE -> WCVP CONCEPT CROSSWALK
# =============================================================================

build_source_to_wcvp_concept <- function(
    candidate_resolution
) {
  
  if (!file.exists(
    source_candidate_crosswalk_path
  )) {
    
    stop(
      "Source-to-candidate crosswalk not found: ",
      source_candidate_crosswalk_path
    )
  }
  
  
  source_crosswalk <-
    readRDS(
      source_candidate_crosswalk_path
    )
  
  
  if (!"wcvp_candidate_id" %in%
      names(
        source_crosswalk
      )) {
    
    stop(
      "source_to_wcvp_candidate.rds does not contain wcvp_candidate_id."
    )
  }
  
  
  source_crosswalk %>%
    
    left_join(
      
      candidate_resolution %>%
        
        select(
          
          wcvp_candidate_id,
          
          wcvp_submitted_name,
          
          wcvp_match_rows,
          
          matched_wcvp_records,
          
          resolvable_concept_count,
          
          non_resolvable_concept_count,
          
          matched_statuses,
          
          wcvp_resolution_method,
          
          wcvp_reconciliation_status,
          
          wcvp_resolved_concept_id,
          
          wcvp_concept_name,
          
          wcvp_concept_authors,
          
          wcvp_concept_family,
          
          wcvp_concept_genus,
          
          wcvp_concept_species,
          
          wcvp_concept_rank,
          
          wcvp_concept_status,
          
          wcvp_is_artificial_hybrid
          
        ),
      
      by = "wcvp_candidate_id"
    )
}


# =============================================================================
# 16. AUDITS
# =============================================================================

build_vpjd_wcvp_status_audit <- function(
    candidate_resolution
) {
  
  candidate_resolution %>%
    
    count(
      wcvp_reconciliation_status,
      sort = TRUE,
      name = "candidate_names"
    ) %>%
    
    mutate(
      percent =
        round(
          100 *
            candidate_names /
            sum(
              candidate_names
            ),
          2
        )
    )
}


build_vpjd_wcvp_resolution_method_audit <- function(
    candidate_resolution
) {
  
  candidate_resolution %>%
    
    count(
      wcvp_resolution_method,
      wcvp_reconciliation_status,
      sort = TRUE,
      name = "candidate_names"
    ) %>%
    
    mutate(
      percent =
        round(
          100 *
            candidate_names /
            sum(
              candidate_names
            ),
          2
        )
    )
}


build_vpjd_wcvp_concept_status_audit <- function(
    candidate_resolution
) {
  
  candidate_resolution %>%
    
    count(
      wcvp_concept_status,
      wcvp_reconciliation_status,
      sort = TRUE,
      name = "candidate_names"
    )
}


build_vpjd_wcvp_multiplicity_audit <- function(
    candidate_resolution
) {
  
  candidate_resolution %>%
    
    count(
      wcvp_match_rows,
      resolvable_concept_count,
      wcvp_reconciliation_status,
      sort = TRUE,
      name = "candidate_names"
    )
}


build_vpjd_wcvp_unresolved_audit <- function(
    candidate_resolution
) {
  
  candidate_resolution %>%
    
    filter(
      !wcvp_reconciliation_status %in%
        c(
          "resolved",
          "resolved_hybrid"
        )
    ) %>%
    
    arrange(
      wcvp_reconciliation_status,
      wcvp_submitted_name
    )
}


build_vpjd_wcvp_ambiguous_match_audit <- function(
    candidate_resolution,
    concept_matches
) {
  
  ambiguous_ids <-
    candidate_resolution %>%
    
    filter(
      wcvp_reconciliation_status ==
        "ambiguous"
    ) %>%
    
    pull(
      wcvp_candidate_id
    )
  
  
  concept_matches %>%
    
    filter(
      wcvp_candidate_id %in%
        ambiguous_ids
    ) %>%
    
    select(
      
      wcvp_candidate_id,
      
      wcvp_submitted_name,
      
      match_type,
      
      multiple_matches,
      
      match_similarity,
      
      match_edit_distance,
      
      wcvp_matched_id,
      
      wcvp_matched_name,
      
      wcvp_matched_authors,
      
      wcvp_matched_rank,
      
      wcvp_matched_status,
      
      wcvp_target_id_raw,
      
      wcvp_resolved_concept_id,
      
      wcvp_concept_name,
      
      wcvp_concept_authors,
      
      wcvp_concept_rank,
      
      wcvp_concept_status,
      
      wcvp_concept_is_resolvable
      
    ) %>%
    
    arrange(
      wcvp_candidate_id,
      wcvp_resolved_concept_id,
      wcvp_matched_id
    )
}


build_vpjd_wcvp_summary <- function(
    candidates,
    raw_matches,
    candidate_resolution,
    resolved_concepts,
    source_to_wcvp
) {
  
  tibble::tibble(
    
    metric = c(
      
      "candidate_names",
      
      "raw_wcvp_match_rows",
      
      "resolved_accepted_candidate_names",
      
      "resolved_hybrid_candidate_names",
      
      "unique_exact_candidate_names",
      
      "convergent_multiple_candidate_names",
      
      "ambiguous_candidate_names",
      
      "unmatched_candidate_names",
      
      "review_candidate_names",
      
      "distinct_resolved_wcvp_concepts",
      
      "source_records_mapped_forward"
    ),
    
    value = c(
      
      nrow(
        candidates
      ),
      
      nrow(
        raw_matches
      ),
      
      sum(
        candidate_resolution$
          wcvp_reconciliation_status ==
          "resolved",
        na.rm = TRUE
      ),
      
      sum(
        candidate_resolution$
          wcvp_reconciliation_status ==
          "resolved_hybrid",
        na.rm = TRUE
      ),
      
      sum(
        candidate_resolution$
          wcvp_resolution_method ==
          "unique_exact",
        na.rm = TRUE
      ),
      
      sum(
        candidate_resolution$
          wcvp_resolution_method ==
          "convergent_multiple",
        na.rm = TRUE
      ),
      
      sum(
        candidate_resolution$
          wcvp_reconciliation_status ==
          "ambiguous",
        na.rm = TRUE
      ),
      
      sum(
        candidate_resolution$
          wcvp_reconciliation_status ==
          "unmatched",
        na.rm = TRUE
      ),
      
      sum(
        candidate_resolution$
          wcvp_reconciliation_status ==
          "review",
        na.rm = TRUE
      ),
      
      nrow(
        resolved_concepts
      ),
      
      nrow(
        source_to_wcvp
      )
    )
  )
}


# =============================================================================
# 17. MAIN RECONCILIATION
# =============================================================================

reconcile_vpjd_taxa_wcvp <- function(
    write_outputs = TRUE,
    use_cache = TRUE
) {
  
  message("")
  message(
    "Starting VPJD candidate-name WCVP reconciliation..."
  )
  
  
  candidates <-
    load_vpjd_wcvp_candidates()
  
  
  wcvp <-
    get_wcvp_names()
  
  
  message(
    "WCVP records loaded: ",
    format(
      nrow(
        wcvp
      ),
      big.mark = ","
    )
  )
  
  
  raw_matches <-
    match_vpjd_candidates_to_wcvp(
      candidates = candidates,
      wcvp = wcvp,
      use_cache = use_cache
    )
  
  
  normalised_matches <-
    normalise_wcvp_match_concepts(
      raw_matches
    )
  
  
  concept_matches <-
    attach_wcvp_concept_metadata(
      normalised_matches,
      wcvp
    )
  
  
  match_multiplicity <-
    build_wcvp_match_multiplicity(
      concept_matches
    )
  
  
  candidate_resolution <-
    resolve_wcvp_candidates(
      candidates,
      concept_matches
    )
  
  
  validate_wcvp_candidate_resolution(
    candidates,
    candidate_resolution
  )
  
  
  resolved_concepts <-
    build_vpjd_wcvp_concepts(
      candidate_resolution
    )
  
  
  source_to_wcvp <-
    build_source_to_wcvp_concept(
      candidate_resolution
    )
  
  
  status_audit <-
    build_vpjd_wcvp_status_audit(
      candidate_resolution
    )
  
  
  resolution_method_audit <-
    build_vpjd_wcvp_resolution_method_audit(
      candidate_resolution
    )
  
  
  concept_status_audit <-
    build_vpjd_wcvp_concept_status_audit(
      candidate_resolution
    )
  
  
  multiplicity_audit <-
    build_vpjd_wcvp_multiplicity_audit(
      candidate_resolution
    )
  
  
  unresolved_audit <-
    build_vpjd_wcvp_unresolved_audit(
      candidate_resolution
    )
  
  
  ambiguous_match_audit <-
    build_vpjd_wcvp_ambiguous_match_audit(
      candidate_resolution,
      concept_matches
    )
  
  
  summary_audit <-
    build_vpjd_wcvp_summary(
      candidates,
      concept_matches,
      candidate_resolution,
      resolved_concepts,
      source_to_wcvp
    )
  
  
  # ---------------------------------------------------------------------------
  # WRITE OUTPUTS
  # ---------------------------------------------------------------------------
  
  if (isTRUE(
    write_outputs
  )) {
    
    saveRDS(
      concept_matches,
      file.path(
        wcvp_taxonomy_dir,
        "vpjd_wcvp_raw_matches.rds"
      )
    )
    
    
    readr::write_csv(
      concept_matches,
      file.path(
        wcvp_taxonomy_dir,
        "vpjd_wcvp_raw_matches.csv"
      ),
      na = ""
    )
    
    
    saveRDS(
      candidate_resolution,
      file.path(
        wcvp_taxonomy_dir,
        "vpjd_wcvp_candidate_resolution.rds"
      )
    )
    
    
    readr::write_csv(
      candidate_resolution,
      file.path(
        wcvp_taxonomy_dir,
        "vpjd_wcvp_candidate_resolution.csv"
      ),
      na = ""
    )
    
    
    saveRDS(
      resolved_concepts,
      file.path(
        wcvp_taxonomy_dir,
        "vpjd_wcvp_resolved_concepts.rds"
      )
    )
    
    
    readr::write_csv(
      resolved_concepts,
      file.path(
        wcvp_taxonomy_dir,
        "vpjd_wcvp_resolved_concepts.csv"
      ),
      na = ""
    )
    
    
    saveRDS(
      source_to_wcvp,
      file.path(
        wcvp_taxonomy_dir,
        "source_to_wcvp_concept.rds"
      )
    )
    
    
    readr::write_csv(
      source_to_wcvp,
      file.path(
        wcvp_taxonomy_dir,
        "source_to_wcvp_concept.csv"
      ),
      na = ""
    )
    
    
    readr::write_csv(
      summary_audit,
      file.path(
        wcvp_taxonomy_audit_dir,
        "vpjd_wcvp_summary.csv"
      )
    )
    
    
    readr::write_csv(
      status_audit,
      file.path(
        wcvp_taxonomy_audit_dir,
        "vpjd_wcvp_status_audit.csv"
      )
    )
    
    
    readr::write_csv(
      resolution_method_audit,
      file.path(
        wcvp_taxonomy_audit_dir,
        "vpjd_wcvp_resolution_method_audit.csv"
      )
    )
    
    
    readr::write_csv(
      concept_status_audit,
      file.path(
        wcvp_taxonomy_audit_dir,
        "vpjd_wcvp_concept_status_audit.csv"
      )
    )
    
    
    readr::write_csv(
      multiplicity_audit,
      file.path(
        wcvp_taxonomy_audit_dir,
        "vpjd_wcvp_multiplicity_audit.csv"
      )
    )
    
    
    readr::write_csv(
      match_multiplicity,
      file.path(
        wcvp_taxonomy_audit_dir,
        "vpjd_wcvp_candidate_match_multiplicity.csv"
      )
    )
    
    
    readr::write_csv(
      unresolved_audit,
      file.path(
        wcvp_taxonomy_audit_dir,
        "vpjd_wcvp_unresolved_review.csv"
      ),
      na = ""
    )
    
    
    readr::write_csv(
      ambiguous_match_audit,
      file.path(
        wcvp_taxonomy_audit_dir,
        "vpjd_wcvp_ambiguous_raw_matches.csv"
      ),
      na = ""
    )
  }
  
  
  # ---------------------------------------------------------------------------
  # CONSOLE SUMMARY
  # ---------------------------------------------------------------------------
  
  message("")
  message(
    "VPJD WCVP reconciliation complete."
  )
  
  
  message(
    "Candidate names: ",
    format(
      nrow(
        candidates
      ),
      big.mark = ","
    )
  )
  
  
  message(
    "Raw WCVP match rows: ",
    format(
      nrow(
        concept_matches
      ),
      big.mark = ","
    )
  )
  
  
  message(
    "Resolved Accepted candidates: ",
    format(
      sum(
        candidate_resolution$
          wcvp_reconciliation_status ==
          "resolved",
        na.rm = TRUE
      ),
      big.mark = ","
    )
  )
  
  
  message(
    "Resolved Artificial Hybrid candidates: ",
    format(
      sum(
        candidate_resolution$
          wcvp_reconciliation_status ==
          "resolved_hybrid",
        na.rm = TRUE
      ),
      big.mark = ","
    )
  )
  
  
  message(
    "Ambiguous candidates: ",
    format(
      sum(
        candidate_resolution$
          wcvp_reconciliation_status ==
          "ambiguous",
        na.rm = TRUE
      ),
      big.mark = ","
    )
  )
  
  
  message(
    "Unmatched candidates: ",
    format(
      sum(
        candidate_resolution$
          wcvp_reconciliation_status ==
          "unmatched",
        na.rm = TRUE
      ),
      big.mark = ","
    )
  )
  
  
  message(
    "Review candidates: ",
    format(
      sum(
        candidate_resolution$
          wcvp_reconciliation_status ==
          "review",
        na.rm = TRUE
      ),
      big.mark = ","
    )
  )
  
  
  message(
    "Distinct resolved WCVP concepts: ",
    format(
      nrow(
        resolved_concepts
      ),
      big.mark = ","
    )
  )
  
  
  message(
    "Source records mapped forward: ",
    format(
      nrow(
        source_to_wcvp
      ),
      big.mark = ","
    )
  )
  
  
  message(
    "Output directory: ",
    wcvp_taxonomy_dir
  )
  
  
  invisible(
    
    list(
      
      candidates =
        candidates,
      
      raw_matches =
        concept_matches,
      
      candidate_resolution =
        candidate_resolution,
      
      resolved_concepts =
        resolved_concepts,
      
      source_to_wcvp =
        source_to_wcvp,
      
      audits = list(
        
        summary =
          summary_audit,
        
        status =
          status_audit,
        
        resolution_method =
          resolution_method_audit,
        
        concept_status =
          concept_status_audit,
        
        multiplicity =
          multiplicity_audit,
        
        candidate_match_multiplicity =
          match_multiplicity,
        
        unresolved =
          unresolved_audit,
        
        ambiguous_raw_matches =
          ambiguous_match_audit
      )
    )
  )
}


# =============================================================================
# 18. MODULE MESSAGE
# =============================================================================

message(
  "04_wcvp_standardise.R v",
  WCVP_STANDARDISE_VERSION,
  " loaded."
)


message(
  "Run reconcile_vpjd_taxa_wcvp() to reconcile the VPJD candidate universe."
)