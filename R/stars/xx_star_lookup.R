# =============================================================================
# VPJD-OJPCP
# R/stars/05_star_lookup.R
#
# Flora of Japan Star taxonomy reconciliation and Star assignment
#
# Purpose
# -------
# 1. Preserve the authoritative Flora of Japan (FOJ) Star source.
# 2. Reconstruct full FOJ taxonomic names, including infraspecific ranks.
# 3. Preserve FOJ spnumber and synof relationships.
# 4. Preserve mixed FOJ source metadata without destructive type coercion.
# 5. Reconcile FOJ taxa independently against WCVP.
# 6. Resolve multiple WCVP name records at accepted-concept level.
# 7. Quarantine genuinely ambiguous WCVP mappings.
# 8. Build:
#       a. an exact FOJ taxon-name Star lookup;
#       b. a conservative WCVP accepted-concept Star consensus lookup.
# 9. Assign Stars using:
#       exact FOJ match -> WCVP consensus -> unresolved.
#
# Scientific principles
# ---------------------
# - FOJ source records are never collapsed or discarded.
# - foj_spnumber is the stable FOJ source-record identifier.
# - Infraspecific taxa retain their original taxonomic rank.
# - Star is fundamentally an attribute of the FOJ taxon concept.
# - WCVP provides the current taxonomic backbone and reconciliation bridge.
# - Modern WCVP lumping must not erase valid FOJ Star distinctions.
# - Multiple WCVP records are acceptable when they converge on one accepted
#   WCVP concept.
# - Multiple accepted WCVP concepts are treated as ambiguous.
# - Same-name FOJ records carrying different Stars are treated as conflicts.
# - WCVP accepted concepts receive a consensus Star only where all resolved,
#   Star-rated FOJ concepts mapped to that accepted concept agree.
# - synof relationships are preserved but do not overwrite source Stars.
# - Weight is preserved as mixed FOJ source metadata.
# - Star coefficients and GHI calculations belong in 06_ghi.R.
#
# Star codes
# ----------
# BK = Black
# GD = Gold
# BU = Blue
# GN = Green
# GX = GX classification
#
# Legacy BL is normalised to BU by normalise_star().
#
# GX is retained as an authoritative source classification. Its treatment
# within GHI must be defined explicitly in R/bioquality/06_ghi.R.
# =============================================================================


suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(here)
  library(rWCVP)
  library(rWCVPdata)
})


source(
  here(
    "R",
    "functions",
    "utility_functions.R"
  )
)


# =============================================================================
# WCVP DATA
# =============================================================================

get_star_wcvp_names <- function() {
  
  data(
    "wcvp_names",
    package = "rWCVPdata",
    envir = environment()
  )
  
  wcvp_names
}


# =============================================================================
# NORMALISE FOJ RANK TERMINOLOGY
# =============================================================================

normalise_foj_rank <- function(x) {
  
  x <- stringr::str_squish(x)
  
  dplyr::case_when(
    is.na(x) ~ NA_character_,
    x == "" ~ NA_character_,
    
    x %in% c(
      "subsp.",
      "ssp."
    ) ~ "subsp.",
    
    x %in% c(
      "var.",
      "Var."
    ) ~ "var.",
    
    x == "f." ~ "f.",
    
    TRUE ~ x
  )
}


# =============================================================================
# NORMALISE FOJ STAR CODES
# =============================================================================

normalise_foj_star <- function(x) {
  
  x <- stringr::str_to_upper(
    stringr::str_squish(
      as.character(x)
    )
  )
  
  dplyr::case_when(
    is.na(x) ~ NA_character_,
    x == "" ~ NA_character_,
    
    # Historical/legacy Blue code
    x == "BL" ~ "BU",
    
    # Authoritative current source codes
    x %in% c(
      "BK",
      "GD",
      "BU",
      "GN",
      "GX"
    ) ~ x,
    
    # Preserve unexpected source values for audit rather than silently
    # converting them to NA.
    TRUE ~ x
  )
}


# =============================================================================
# CONSTRUCT FULL FOJ TAXON NAME
# =============================================================================

construct_foj_name <- function(
    genus,
    sp1,
    rank1,
    sp2,
    rank2,
    sp3
) {
  
  r1 <- normalise_foj_rank(rank1)
  r2 <- normalise_foj_rank(rank2)
  
  parts <- c(
    genus,
    sp1,
    r1,
    sp2,
    r2,
    sp3
  )
  
  parts <- parts[
    !is.na(parts) &
      parts != ""
  ]
  
  if (length(parts) == 0) {
    return(NA_character_)
  }
  
  stringr::str_squish(
    paste(
      parts,
      collapse = " "
    )
  )
}


# =============================================================================
# LOAD FOJ STAR SOURCE
# =============================================================================

load_star_table <- function(
    path = here(
      "data",
      "raw",
      "external",
      "FOJ_STARS.csv"
    )
) {
  
  if (!file.exists(path)) {
    
    stop(
      "Star table not found: ",
      path
    )
  }
  
  
  # ---------------------------------------------------------------------------
  # Import source columns as character.
  #
  # This is deliberate. FOJ_STARS.csv contains mixed source metadata.
  # In particular, Weight contains categorical values such as CR, EN, VU,
  # NT and DD for some GX records. Type guessing would incorrectly infer
  # Weight as numeric and destroy those values.
  # ---------------------------------------------------------------------------
  
  x <- readr::read_csv(
    path,
    col_types = readr::cols(
      .default =
        readr::col_character()
    ),
    show_col_types = FALSE
  )
  
  
  # ---------------------------------------------------------------------------
  # Validate essential source columns
  # ---------------------------------------------------------------------------
  
  required <- c(
    "spnumber",
    "synof",
    "Full Name",
    "Star",
    "Weight",
    "genus",
    "sp1",
    "rank1",
    "sp2",
    "rank2",
    "sp3"
  )
  
  
  missing_cols <- setdiff(
    required,
    names(x)
  )
  
  
  if (length(missing_cols) > 0) {
    
    stop(
      "Star table is missing required columns: ",
      paste(
        missing_cols,
        collapse = ", "
      )
    )
  }
  
  
  # ---------------------------------------------------------------------------
  # Preserve raw source fields before conversion
  # ---------------------------------------------------------------------------
  
  x <- x %>%
    mutate(
      foj_spnumber_raw =
        spnumber,
      
      foj_synof_raw =
        synof,
      
      foj_weight_raw =
        Weight,
      
      foj_star_raw =
        Star
    )
  
  
  # ---------------------------------------------------------------------------
  # Convert only fields whose numeric meaning is established
  # ---------------------------------------------------------------------------
  
  x <- x %>%
    mutate(
      spnumber =
        suppressWarnings(
          as.numeric(spnumber)
        ),
      
      synof =
        suppressWarnings(
          as.numeric(synof)
        ),
      
      # Optional numeric representation of Weight.
      # The raw field remains authoritative and is preserved separately.
      foj_weight_numeric =
        suppressWarnings(
          as.numeric(Weight)
        )
    )
  
  
  # ---------------------------------------------------------------------------
  # Validate identifier conversion
  # ---------------------------------------------------------------------------
  
  if (
    any(
      !is.na(x$foj_spnumber_raw) &
      is.na(x$spnumber)
    )
  ) {
    
    stop(
      "One or more FOJ spnumber values could not be converted to numeric."
    )
  }
  
  
  if (
    any(
      !is.na(x$foj_synof_raw) &
      x$foj_synof_raw != "" &
      is.na(x$synof)
    )
  ) {
    
    stop(
      "One or more FOJ synof values could not be converted to numeric."
    )
  }
  
  
  # ---------------------------------------------------------------------------
  # spnumber must uniquely identify every FOJ source record
  # ---------------------------------------------------------------------------
  
  if (any(is.na(x$spnumber))) {
    
    stop(
      "FOJ Star table contains missing spnumber values."
    )
  }
  
  
  if (
    dplyr::n_distinct(x$spnumber) !=
    nrow(x)
  ) {
    
    stop(
      "FOJ Star table contains duplicate spnumber values."
    )
  }
  
  
  # ---------------------------------------------------------------------------
  # Preserve source data and create normalised fields
  # ---------------------------------------------------------------------------
  
  x <- x %>%
    mutate(
      foj_spnumber =
        spnumber,
      
      foj_synof =
        if_else(
          is.na(synof),
          0,
          synof
        ),
      
      foj_is_synonym =
        foj_synof > 0,
      
      foj_full_name =
        `Full Name`,
      
      foj_rank1 =
        normalise_foj_rank(rank1),
      
      foj_rank2 =
        normalise_foj_rank(rank2),
      
      Star =
        normalise_foj_star(Star)
    )
  
  
  # ---------------------------------------------------------------------------
  # Construct full taxonomic names from structured FOJ fields
  # ---------------------------------------------------------------------------
  
  x$foj_taxon_name <- mapply(
    construct_foj_name,
    genus = x$genus,
    sp1 = x$sp1,
    rank1 = x$rank1,
    sp2 = x$sp2,
    rank2 = x$rank2,
    sp3 = x$sp3,
    USE.NAMES = FALSE
  )
  
  
  # ---------------------------------------------------------------------------
  # Flag non-standard rank terminology
  # ---------------------------------------------------------------------------
  
  recognised_ranks <- c(
    "subsp.",
    "var.",
    "f."
  )
  
  
  x <- x %>%
    mutate(
      foj_rank_review =
        (
          !is.na(foj_rank1) &
            !(foj_rank1 %in% recognised_ranks)
        ) |
        (
          !is.na(foj_rank2) &
            !(foj_rank2 %in% recognised_ranks)
        )
    )
  
  
  # ---------------------------------------------------------------------------
  # Flag unexpected Star codes without deleting them
  # ---------------------------------------------------------------------------
  
  recognised_stars <- c(
    "BK",
    "GD",
    "BU",
    "GN",
    "GX"
  )
  
  
  x <- x %>%
    mutate(
      foj_star_review =
        !is.na(Star) &
        !(Star %in% recognised_stars)
    )
  
  
  # ---------------------------------------------------------------------------
  # Final source-row integrity check
  # ---------------------------------------------------------------------------
  
  if (
    dplyr::n_distinct(x$foj_spnumber) !=
    nrow(x)
  ) {
    
    stop(
      "FOJ spnumber is not unique after source-table preparation."
    )
  }
  
  
  x
}


# =============================================================================
# RESOLVE EXPLICIT FOJ SYNONYM TARGET
# =============================================================================

add_foj_synonym_target <- function(stars) {
  
  synonym_lookup <- stars %>%
    select(
      synonym_target_spnumber =
        foj_spnumber,
      
      synonym_target_name =
        foj_taxon_name,
      
      synonym_target_full_name =
        foj_full_name,
      
      synonym_target_star =
        Star
    )
  
  
  stars %>%
    left_join(
      synonym_lookup,
      by = c(
        "foj_synof" =
          "synonym_target_spnumber"
      ),
      relationship = "many-to-one"
    )
}


# =============================================================================
# MATCH FOJ TAXA TO WCVP
# =============================================================================

match_foj_to_wcvp <- function(stars) {
  
  wcvp <- get_star_wcvp_names()
  
  
  # ---------------------------------------------------------------------------
  # Prepare unique automatically matchable FOJ taxon names
  # ---------------------------------------------------------------------------
  
  names_to_match <- stars %>%
    filter(
      !is.na(foj_taxon_name),
      foj_taxon_name != "",
      !foj_rank_review
    ) %>%
    distinct(
      foj_taxon_name
    ) %>%
    mutate(
      .foj_wcvp_match_id =
        row_number()
    )
  
  
  message(
    "Matching ",
    format(
      nrow(names_to_match),
      big.mark = ","
    ),
    " unique FOJ taxon names against WCVP..."
  )
  
  
  # ---------------------------------------------------------------------------
  # Obtain all exact WCVP matches
  # ---------------------------------------------------------------------------
  
  raw_matches <- rWCVP::wcvp_match_exact(
    names_df =
      names_to_match,
    
    wcvp_names =
      wcvp,
    
    name_col =
      "foj_taxon_name",
    
    id_col =
      ".foj_wcvp_match_id"
  ) %>%
    transmute(
      foj_taxon_name,
      
      foj_wcvp_match_type =
        match_type,
      
      foj_wcvp_multiple_matches =
        multiple_matches,
      
      foj_wcvp_match_similarity =
        match_similarity,
      
      foj_wcvp_match_edit_distance =
        match_edit_distance,
      
      foj_wcvp_id =
        wcvp_id,
      
      foj_wcvp_name =
        wcvp_name,
      
      foj_wcvp_authors =
        wcvp_authors,
      
      foj_wcvp_rank =
        wcvp_rank,
      
      foj_wcvp_status =
        wcvp_status,
      
      foj_wcvp_accepted_id =
        wcvp_accepted_id
    )
  
  
  # ---------------------------------------------------------------------------
  # Summarise WCVP candidate records and accepted concepts
  # ---------------------------------------------------------------------------
  
  resolution <- raw_matches %>%
    group_by(
      foj_taxon_name
    ) %>%
    summarise(
      foj_wcvp_n_records =
        n_distinct(
          foj_wcvp_id,
          na.rm = TRUE
        ),
      
      foj_wcvp_n_accepted_concepts =
        n_distinct(
          foj_wcvp_accepted_id,
          na.rm = TRUE
        ),
      
      foj_wcvp_candidate_ids =
        paste(
          sort(
            unique(
              na.omit(
                foj_wcvp_id
              )
            )
          ),
          collapse = " / "
        ),
      
      foj_wcvp_candidate_accepted_ids =
        paste(
          sort(
            unique(
              na.omit(
                foj_wcvp_accepted_id
              )
            )
          ),
          collapse = " / "
        ),
      
      .groups =
        "drop"
    ) %>%
    mutate(
      foj_wcvp_resolution =
        case_when(
          foj_wcvp_n_accepted_concepts == 1 ~
            "resolved",
          
          foj_wcvp_n_accepted_concepts > 1 ~
            "ambiguous",
          
          TRUE ~
            "unmatched"
        )
    )
  
  
  # ---------------------------------------------------------------------------
  # Extract accepted ID only when candidates converge on one concept
  # ---------------------------------------------------------------------------
  
  resolved_ids <- raw_matches %>%
    filter(
      !is.na(
        foj_wcvp_accepted_id
      )
    ) %>%
    group_by(
      foj_taxon_name
    ) %>%
    summarise(
      accepted_ids =
        list(
          unique(
            foj_wcvp_accepted_id
          )
        ),
      
      .groups =
        "drop"
    ) %>%
    mutate(
      resolved_accepted_id =
        vapply(
          accepted_ids,
          function(ids) {
            
            ids <- ids[
              !is.na(ids)
            ]
            
            if (
              length(ids) == 1
            ) {
              
              as.numeric(
                ids[[1]]
              )
              
            } else {
              
              NA_real_
            }
          },
          numeric(1)
        )
    ) %>%
    select(
      foj_taxon_name,
      resolved_accepted_id
    )
  
  
  # ---------------------------------------------------------------------------
  # Accepted WCVP taxon lookup
  # ---------------------------------------------------------------------------
  
  accepted_lookup <- wcvp %>%
    filter(
      taxon_status ==
        "Accepted"
    ) %>%
    transmute(
      resolved_accepted_id =
        plant_name_id,
      
      foj_wcvp_accepted_name =
        taxon_name,
      
      foj_wcvp_accepted_authors =
        taxon_authors,
      
      foj_wcvp_accepted_rank =
        taxon_rank
    ) %>%
    distinct(
      resolved_accepted_id,
      .keep_all = TRUE
    )
  
  
  # ---------------------------------------------------------------------------
  # One reconciliation record per FOJ taxon name
  # ---------------------------------------------------------------------------
  
  reconciled_names <- names_to_match %>%
    select(
      foj_taxon_name
    ) %>%
    left_join(
      resolution,
      by =
        "foj_taxon_name",
      relationship =
        "one-to-one"
    ) %>%
    left_join(
      resolved_ids,
      by =
        "foj_taxon_name",
      relationship =
        "one-to-one"
    ) %>%
    left_join(
      accepted_lookup,
      by =
        "resolved_accepted_id",
      relationship =
        "many-to-one"
    ) %>%
    mutate(
      foj_wcvp_accepted_id =
        if_else(
          foj_wcvp_resolution ==
            "resolved",
          resolved_accepted_id,
          NA_real_
        )
    ) %>%
    select(
      -resolved_accepted_id
    )
  
  
  # ---------------------------------------------------------------------------
  # Join reconciliation back to every FOJ source record
  # ---------------------------------------------------------------------------
  
  out <- stars %>%
    left_join(
      reconciled_names,
      by =
        "foj_taxon_name",
      relationship =
        "many-to-one"
    ) %>%
    mutate(
      foj_wcvp_resolution =
        case_when(
          foj_rank_review ~
            "rank_review",
          
          is.na(
            foj_wcvp_resolution
          ) ~
            "unmatched",
          
          TRUE ~
            foj_wcvp_resolution
        )
    )
  
  
  # ---------------------------------------------------------------------------
  # Critical source-row invariants
  # ---------------------------------------------------------------------------
  
  if (
    nrow(out) !=
    nrow(stars)
  ) {
    
    stop(
      paste0(
        "FOJ/WCVP reconciliation changed the number of source records: ",
        nrow(stars),
        " input rows -> ",
        nrow(out),
        " output rows."
      )
    )
  }
  
  
  if (
    dplyr::n_distinct(
      out$foj_spnumber
    ) !=
    nrow(out)
  ) {
    
    stop(
      "FOJ spnumber is no longer unique after WCVP reconciliation."
    )
  }
  
  
  # ---------------------------------------------------------------------------
  # Console reconciliation audit
  # ---------------------------------------------------------------------------
  
  audit <- out %>%
    count(
      foj_wcvp_resolution,
      name = "records"
    ) %>%
    mutate(
      pct =
        round(
          100 *
            records /
            nrow(out),
          2
        )
    )
  
  
  message(
    "FOJ/WCVP reconciliation complete: ",
    format(
      nrow(out),
      big.mark = ","
    ),
    " source records preserved."
  )
  
  
  print(
    audit
  )
  
  
  out
}


# =============================================================================
# PREPARE COMPLETE FOJ STAR TAXONOMY
# =============================================================================

prepare_star_taxonomy <- function(
    star_path = here(
      "data",
      "raw",
      "external",
      "FOJ_STARS.csv"
    )
) {
  
  stars <- load_star_table(
    star_path
  )
  
  
  stars <- add_foj_synonym_target(
    stars
  )
  
  
  stars <- match_foj_to_wcvp(
    stars
  )
  
  
  stars
}


# =============================================================================
# BUILD EXACT FOJ TAXON-NAME STAR LOOKUP
# =============================================================================

build_foj_star_lookup <- function(stars) {
  
  required <- c(
    "foj_spnumber",
    "foj_taxon_name",
    "Star"
  )
  
  
  missing_cols <- setdiff(
    required,
    names(stars)
  )
  
  
  if (
    length(missing_cols) > 0
  ) {
    
    stop(
      "Prepared Star table is missing required columns: ",
      paste(
        missing_cols,
        collapse = ", "
      )
    )
  }
  
  
  # ---------------------------------------------------------------------------
  # An exact reconstructed FOJ name is usable only where all Star-rated
  # source records carrying that name agree.
  # ---------------------------------------------------------------------------
  
  stars %>%
    filter(
      !is.na(
        foj_taxon_name
      ),
      
      foj_taxon_name != "",
      
      !is.na(
        Star
      )
    ) %>%
    group_by(
      foj_taxon_name
    ) %>%
    summarise(
      foj_exact_n_source_records =
        n(),
      
      foj_exact_n_stars =
        n_distinct(
          Star,
          na.rm = TRUE
        ),
      
      foj_exact_conflict =
        foj_exact_n_stars > 1,
      
      foj_exact_star =
        if (
          foj_exact_n_stars == 1
        ) {
          
          first(
            na.omit(
              Star
            )
          )
          
        } else {
          
          NA_character_
        },
      
      foj_exact_source_spnumbers =
        paste(
          sort(
            unique(
              foj_spnumber
            )
          ),
          collapse = " / "
        ),
      
      .groups =
        "drop"
    )
}


# =============================================================================
# BUILD WCVP ACCEPTED-CONCEPT STAR CONSENSUS LOOKUP
# =============================================================================

build_star_lookup <- function(stars) {
  
  required <- c(
    "foj_spnumber",
    "foj_taxon_name",
    "foj_wcvp_resolution",
    "foj_wcvp_accepted_id",
    "Star"
  )
  
  
  missing_cols <- setdiff(
    required,
    names(stars)
  )
  
  
  if (
    length(missing_cols) > 0
  ) {
    
    stop(
      "Prepared Star table is missing required columns: ",
      paste(
        missing_cols,
        collapse = ", "
      )
    )
  }
  
  
  candidate <- stars %>%
    filter(
      foj_wcvp_resolution ==
        "resolved",
      
      !is.na(
        foj_wcvp_accepted_id
      ),
      
      !is.na(
        Star
      )
    )
  
  
  candidate %>%
    group_by(
      foj_wcvp_accepted_id
    ) %>%
    summarise(
      star_n_ratings =
        n_distinct(
          Star,
          na.rm = TRUE
        ),
      
      star_conflict =
        star_n_ratings > 1,
      
      wcvp_consensus_star =
        if (
          star_n_ratings == 1
        ) {
          
          first(
            na.omit(
              Star
            )
          )
          
        } else {
          
          NA_character_
        },
      
      star_source_records =
        n(),
      
      star_source_foj_names =
        n_distinct(
          foj_taxon_name
        ),
      
      star_source_spnumbers =
        paste(
          sort(
            unique(
              foj_spnumber
            )
          ),
          collapse = " / "
        ),
      
      .groups =
        "drop"
    ) %>%
    rename(
      wcvp_accepted_id =
        foj_wcvp_accepted_id
    )
}


# =============================================================================
# BUILD WCVP STAR-CONFLICT AUDIT
# =============================================================================

build_star_conflict_audit <- function(stars) {
  
  stars %>%
    filter(
      foj_wcvp_resolution ==
        "resolved",
      
      !is.na(
        foj_wcvp_accepted_id
      ),
      
      !is.na(
        Star
      )
    ) %>%
    group_by(
      foj_wcvp_accepted_id,
      foj_wcvp_accepted_name,
      foj_wcvp_accepted_rank
    ) %>%
    summarise(
      n_foj_records =
        n(),
      
      n_foj_names =
        n_distinct(
          foj_taxon_name
        ),
      
      n_stars =
        n_distinct(
          Star
        ),
      
      stars =
        paste(
          sort(
            unique(
              Star
            )
          ),
          collapse = " / "
        ),
      
      n_synonyms =
        sum(
          foj_is_synonym,
          na.rm = TRUE
        ),
      
      source_spnumbers =
        paste(
          sort(
            unique(
              foj_spnumber
            )
          ),
          collapse = " / "
        ),
      
      .groups =
        "drop"
    ) %>%
    filter(
      n_stars > 1
    ) %>%
    mutate(
      conflict_type =
        case_when(
          n_foj_names > 1 ~
            "multiple_FOJ_concepts",
          
          n_foj_names == 1 &
            n_stars > 1 ~
            "same_FOJ_concept_different_Stars",
          
          TRUE ~
            "other"
        )
    )
}


# =============================================================================
# ADD STAR RATINGS TO WCVP-STANDARDISED VPJD DATA
# =============================================================================

add_star_ratings <- function(
    input,
    label,
    name_col = NULL,
    star_path = here(
      "data",
      "raw",
      "external",
      "FOJ_STARS.csv"
    )
) {
  
  dirs <- vpjd_dirs()
  
  slug <- safe_slug(
    label
  )
  
  
  # ---------------------------------------------------------------------------
  # Read VPJD input
  # ---------------------------------------------------------------------------
  
  dat <- if (
    is.character(input) &&
    length(input) == 1
  ) {
    
    readRDS(
      input
    )
    
  } else {
    
    tibble::as_tibble(
      input
    )
  }
  
  
  # ---------------------------------------------------------------------------
  # Require prior WCVP reconciliation
  # ---------------------------------------------------------------------------
  
  if (
    !"wcvp_accepted_id" %in%
    names(dat)
  ) {
    
    stop(
      paste0(
        "Input does not contain 'wcvp_accepted_id'. ",
        "Run standardise_wcvp() before add_star_ratings()."
      )
    )
  }
  
  
  # ---------------------------------------------------------------------------
  # Determine original/pre-WCVP taxon-name column
  # ---------------------------------------------------------------------------
  
  if (
    is.null(
      name_col
    )
  ) {
    
    candidate_name_cols <- c(
      "gbif_binom",
      "scientificName",
      "species"
    )
    
    
    candidate_name_cols <- candidate_name_cols[
      candidate_name_cols %in%
        names(dat)
    ]
    
    
    if (
      length(candidate_name_cols) > 0
    ) {
      
      name_col <-
        candidate_name_cols[1]
      
    } else {
      
      name_col <-
        NULL
    }
  }
  
  
  # ---------------------------------------------------------------------------
  # Prepare authoritative FOJ taxonomy and lookup levels
  # ---------------------------------------------------------------------------
  
  stars <- prepare_star_taxonomy(
    star_path
  )
  
  
  foj_lookup <- build_foj_star_lookup(
    stars
  )
  
  
  wcvp_lookup <- build_star_lookup(
    stars
  )
  
  
  conflict_audit <- build_star_conflict_audit(
    stars
  )
  
  
  # ---------------------------------------------------------------------------
  # Exact FOJ taxon-name matching
  # ---------------------------------------------------------------------------
  
  if (
    !is.null(
      name_col
    )
  ) {
    
    dat <- dat %>%
      mutate(
        star_input_name =
          stringr::str_squish(
            as.character(
              .data[[name_col]]
            )
          )
      ) %>%
      left_join(
        foj_lookup,
        by = c(
          "star_input_name" =
            "foj_taxon_name"
        ),
        relationship =
          "many-to-one"
      )
    
  } else {
    
    dat <- dat %>%
      mutate(
        star_input_name =
          NA_character_,
        
        foj_exact_n_source_records =
          NA_integer_,
        
        foj_exact_n_stars =
          NA_integer_,
        
        foj_exact_conflict =
          NA,
        
        foj_exact_star =
          NA_character_,
        
        foj_exact_source_spnumbers =
          NA_character_
      )
  }
  
  
  # ---------------------------------------------------------------------------
  # WCVP accepted-concept consensus matching
  # ---------------------------------------------------------------------------
  
  dat <- dat %>%
    left_join(
      wcvp_lookup,
      by =
        "wcvp_accepted_id",
      relationship =
        "many-to-one"
    )
  
  
  # ---------------------------------------------------------------------------
  # Final hierarchical Star assignment
  #
  # Priority:
  #   1. exact FOJ name with one source Star;
  #   2. WCVP accepted-concept consensus;
  #   3. unresolved.
  #
  # Exact same-name FOJ conflicts do not fall through to WCVP consensus.
  # ---------------------------------------------------------------------------
  
  dat <- dat %>%
    mutate(
      Star =
        case_when(
          !is.na(
            foj_exact_star
          ) ~
            foj_exact_star,
          
          foj_exact_conflict %in% TRUE ~
            NA_character_,
          
          !is.na(
            wcvp_consensus_star
          ) ~
            wcvp_consensus_star,
          
          TRUE ~
            NA_character_
        ),
      
      star_match_method =
        case_when(
          !is.na(
            foj_exact_star
          ) ~
            "foj_exact",
          
          foj_exact_conflict %in% TRUE ~
            "foj_exact_conflict",
          
          !is.na(
            wcvp_consensus_star
          ) ~
            "wcvp_consensus",
          
          star_conflict %in% TRUE ~
            "wcvp_conflict",
          
          is.na(
            wcvp_accepted_id
          ) ~
            "no_wcvp_concept",
          
          TRUE ~
            "no_star"
        ),
      
      star_resolved =
        !is.na(
          Star
        )
    )
  
  
  # ---------------------------------------------------------------------------
  # FOJ/WCVP resolution audit
  # ---------------------------------------------------------------------------
  
  taxonomy_audit <- stars %>%
    count(
      foj_wcvp_resolution,
      name = "records"
    ) %>%
    mutate(
      pct =
        round(
          100 *
            records /
            nrow(stars),
          2
        )
    )
  
  
  # ---------------------------------------------------------------------------
  # FOJ Star distribution
  # ---------------------------------------------------------------------------
  
  star_distribution <- stars %>%
    count(
      Star,
      name =
        "records",
      sort =
        TRUE
    ) %>%
    mutate(
      pct =
        round(
          100 *
            records /
            nrow(stars),
          2
        )
    )
  
  
  # ---------------------------------------------------------------------------
  # Taxonomy summary
  # ---------------------------------------------------------------------------
  
  taxonomy_summary <- tibble::tibble(
    metric = c(
      "foj_records",
      "foj_unique_spnumbers",
      "foj_star_rated_records",
      "foj_gx_records",
      "foj_synonym_records",
      "foj_infraspecific_records",
      "foj_rank_review_records",
      "foj_star_review_records",
      "foj_wcvp_resolved_records",
      "foj_wcvp_ambiguous_records",
      "foj_wcvp_unmatched_records",
      "foj_exact_name_conflicts",
      "wcvp_star_conflicts",
      "wcvp_multiple_foj_concept_conflicts",
      "wcvp_same_foj_name_conflicts"
    ),
    
    value = c(
      nrow(stars),
      
      dplyr::n_distinct(
        stars$foj_spnumber
      ),
      
      sum(
        !is.na(
          stars$Star
        )
      ),
      
      sum(
        stars$Star ==
          "GX",
        na.rm = TRUE
      ),
      
      sum(
        stars$foj_is_synonym,
        na.rm = TRUE
      ),
      
      sum(
        !is.na(
          stars$foj_rank1
        ),
        na.rm = TRUE
      ),
      
      sum(
        stars$foj_wcvp_resolution ==
          "rank_review",
        na.rm = TRUE
      ),
      
      sum(
        stars$foj_star_review,
        na.rm = TRUE
      ),
      
      sum(
        stars$foj_wcvp_resolution ==
          "resolved",
        na.rm = TRUE
      ),
      
      sum(
        stars$foj_wcvp_resolution ==
          "ambiguous",
        na.rm = TRUE
      ),
      
      sum(
        stars$foj_wcvp_resolution ==
          "unmatched",
        na.rm = TRUE
      ),
      
      sum(
        foj_lookup$foj_exact_conflict,
        na.rm = TRUE
      ),
      
      nrow(
        conflict_audit
      ),
      
      sum(
        conflict_audit$conflict_type ==
          "multiple_FOJ_concepts",
        na.rm = TRUE
      ),
      
      sum(
        conflict_audit$conflict_type ==
          "same_FOJ_concept_different_Stars",
        na.rm = TRUE
      )
    )
  )
  
  
  # ---------------------------------------------------------------------------
  # Write taxonomy audit outputs
  # ---------------------------------------------------------------------------
  
  readr::write_csv(
    taxonomy_audit,
    file.path(
      dirs$tables,
      paste0(
        slug,
        "_foj_wcvp_resolution.csv"
      )
    )
  )
  
  
  readr::write_csv(
    taxonomy_summary,
    file.path(
      dirs$tables,
      paste0(
        slug,
        "_foj_taxonomy_audit.csv"
      )
    )
  )
  
  
  readr::write_csv(
    star_distribution,
    file.path(
      dirs$tables,
      paste0(
        slug,
        "_foj_star_distribution.csv"
      )
    )
  )
  
  
  # ---------------------------------------------------------------------------
  # Unresolved FOJ/WCVP reconciliation
  # ---------------------------------------------------------------------------
  
  unresolved_taxonomy <- stars %>%
    filter(
      foj_wcvp_resolution !=
        "resolved"
    )
  
  
  readr::write_csv(
    unresolved_taxonomy,
    file.path(
      dirs$tables,
      paste0(
        slug,
        "_foj_wcvp_review.csv"
      )
    )
  )
  
  
  # ---------------------------------------------------------------------------
  # Unexpected Star-code review
  # ---------------------------------------------------------------------------
  
  star_code_review <- stars %>%
    filter(
      foj_star_review
    )
  
  
  readr::write_csv(
    star_code_review,
    file.path(
      dirs$tables,
      paste0(
        slug,
        "_foj_star_code_review.csv"
      )
    )
  )
  
  
  # ---------------------------------------------------------------------------
  # Exact FOJ same-name Star conflicts
  # ---------------------------------------------------------------------------
  
  foj_exact_conflicts <- foj_lookup %>%
    filter(
      foj_exact_conflict
    )
  
  
  readr::write_csv(
    foj_exact_conflicts,
    file.path(
      dirs$tables,
      paste0(
        slug,
        "_foj_exact_star_conflicts.csv"
      )
    )
  )
  
  
  # ---------------------------------------------------------------------------
  # WCVP accepted-concept Star conflicts
  # ---------------------------------------------------------------------------
  
  readr::write_csv(
    conflict_audit,
    file.path(
      dirs$tables,
      paste0(
        slug,
        "_wcvp_star_conflicts.csv"
      )
    )
  )
  
  
  # ---------------------------------------------------------------------------
  # VPJD Star-assignment audit
  # ---------------------------------------------------------------------------
  
  assignment_audit <- dat %>%
    count(
      star_match_method,
      name =
        "records"
    ) %>%
    mutate(
      pct =
        round(
          100 *
            records /
            nrow(dat),
          2
        )
    ) %>%
    arrange(
      desc(
        records
      )
    )
  
  
  readr::write_csv(
    assignment_audit,
    file.path(
      dirs$tables,
      paste0(
        slug,
        "_star_assignment_audit.csv"
      )
    )
  )
  
  
  # ---------------------------------------------------------------------------
  # Taxon-level Star coverage audit
  # ---------------------------------------------------------------------------
  
  accepted_name_col <- if (
    "accepted_name" %in%
    names(dat)
  ) {
    
    "accepted_name"
    
  } else if (
    "wcvp_accepted_name" %in%
    names(dat)
  ) {
    
    "wcvp_accepted_name"
    
  } else {
    
    NULL
  }
  
  
  if (
    !is.null(
      accepted_name_col
    )
  ) {
    
    species_audit <- dat %>%
      distinct(
        wcvp_accepted_id,
        
        accepted_name =
          .data[[accepted_name_col]],
        
        Star,
        star_match_method
      ) %>%
      summarise(
        taxa =
          sum(
            !is.na(
              wcvp_accepted_id
            )
          ),
        
        taxa_with_star =
          sum(
            !is.na(
              Star
            )
          ),
        
        pct_taxa_with_star =
          if (
            taxa > 0
          ) {
            
            round(
              100 *
                taxa_with_star /
                taxa,
              2
            )
            
          } else {
            
            NA_real_
          }
      )
    
  } else {
    
    species_audit <- dat %>%
      distinct(
        wcvp_accepted_id,
        Star,
        star_match_method
      ) %>%
      summarise(
        taxa =
          sum(
            !is.na(
              wcvp_accepted_id
            )
          ),
        
        taxa_with_star =
          sum(
            !is.na(
              Star
            )
          ),
        
        pct_taxa_with_star =
          if (
            taxa > 0
          ) {
            
            round(
              100 *
                taxa_with_star /
                taxa,
              2
            )
            
          } else {
            
            NA_real_
          }
      )
  }
  
  
  readr::write_csv(
    species_audit,
    file.path(
      dirs$tables,
      paste0(
        slug,
        "_star_coverage.csv"
      )
    )
  )
  
  
  # ---------------------------------------------------------------------------
  # Save processed VPJD dataset
  # ---------------------------------------------------------------------------
  
  out_rds <- file.path(
    dirs$processed,
    paste0(
      slug,
      "_wcvp_stars.rds"
    )
  )
  
  
  out_csv <- file.path(
    dirs$processed,
    paste0(
      slug,
      "_wcvp_stars.csv"
    )
  )
  
  
  saveRDS(
    dat,
    out_rds
  )
  
  
  readr::write_csv(
    dat,
    out_csv
  )
  
  
  # ---------------------------------------------------------------------------
  # Console summary
  # ---------------------------------------------------------------------------
  
  message(
    "Star assignment complete."
  )
  
  
  print(
    assignment_audit
  )
  
  
  message(
    "Star-rated WCVP taxon coverage: ",
    species_audit$taxa_with_star,
    "/",
    species_audit$taxa,
    " (",
    species_audit$pct_taxa_with_star,
    "%)"
  )
  
  
  invisible(
    dat
  )
}


# =============================================================================
# ADD STAR RATINGS TO A CHECKLIST
# =============================================================================

add_star_to_checklist <- function(
    checklist,
    name_col = NULL,
    star_path = here(
      "data",
      "raw",
      "external",
      "FOJ_STARS.csv"
    )
) {
  
  # ---------------------------------------------------------------------------
  # Prepare authoritative FOJ/WCVP Star resources
  # ---------------------------------------------------------------------------
  
  stars <- prepare_star_taxonomy(
    star_path
  )
  
  
  foj_lookup <- build_foj_star_lookup(
    stars
  )
  
  
  wcvp_lookup <- build_star_lookup(
    stars
  )
  
  
  # ---------------------------------------------------------------------------
  # Character-vector input
  # ---------------------------------------------------------------------------
  
  if (
    is.character(checklist) &&
    !is.data.frame(checklist)
  ) {
    
    df <- tibble::tibble(
      species =
        checklist
    )
    
    name_col <-
      "species"
    
  } else {
    
    df <- tibble::as_tibble(
      checklist
    )
    
    
    if (
      is.null(
        name_col
      )
    ) {
      
      name_col <-
        names(df)[1]
    }
  }
  
  
  if (
    !name_col %in%
    names(df)
  ) {
    
    stop(
      "Checklist name column not found: ",
      name_col
    )
  }
  
  
  # ---------------------------------------------------------------------------
  # Preserve original checklist name
  # ---------------------------------------------------------------------------
  
  df <- df %>%
    mutate(
      checklist_name =
        stringr::str_squish(
          as.character(
            .data[[name_col]]
          )
        )
    )
  
  
  # ---------------------------------------------------------------------------
  # Exact FOJ taxon-name lookup
  # ---------------------------------------------------------------------------
  
  df <- df %>%
    left_join(
      foj_lookup,
      by = c(
        "checklist_name" =
          "foj_taxon_name"
      ),
      relationship =
        "many-to-one"
    )
  
  
  # ---------------------------------------------------------------------------
  # Independently reconcile checklist names against WCVP
  # ---------------------------------------------------------------------------
  
  wcvp <- get_star_wcvp_names()
  
  
  checklist_names <- df %>%
    filter(
      !is.na(
        checklist_name
      ),
      
      checklist_name != ""
    ) %>%
    distinct(
      checklist_name
    ) %>%
    mutate(
      .checklist_match_id =
        row_number()
    )
  
  
  checklist_matches <- rWCVP::wcvp_match_exact(
    names_df =
      checklist_names,
    
    wcvp_names =
      wcvp,
    
    name_col =
      "checklist_name",
    
    id_col =
      ".checklist_match_id"
  ) %>%
    transmute(
      checklist_name,
      
      checklist_wcvp_id =
        wcvp_id,
      
      checklist_wcvp_accepted_id =
        wcvp_accepted_id
    )
  
  
  # ---------------------------------------------------------------------------
  # Resolve checklist WCVP concept
  # ---------------------------------------------------------------------------
  
  checklist_resolution <- checklist_matches %>%
    group_by(
      checklist_name
    ) %>%
    summarise(
      n_accepted_concepts =
        n_distinct(
          checklist_wcvp_accepted_id,
          na.rm = TRUE
        ),
      
      accepted_ids =
        list(
          unique(
            na.omit(
              checklist_wcvp_accepted_id
            )
          )
        ),
      
      .groups =
        "drop"
    ) %>%
    mutate(
      wcvp_accepted_id =
        vapply(
          accepted_ids,
          function(ids) {
            
            if (
              length(ids) == 1
            ) {
              
              as.numeric(
                ids[[1]]
              )
              
            } else {
              
              NA_real_
            }
          },
          numeric(1)
        ),
      
      checklist_wcvp_resolution =
        case_when(
          n_accepted_concepts == 1 ~
            "resolved",
          
          n_accepted_concepts > 1 ~
            "ambiguous",
          
          TRUE ~
            "unmatched"
        )
    ) %>%
    select(
      -accepted_ids
    )
  
  
  # ---------------------------------------------------------------------------
  # Join WCVP reconciliation and consensus Star
  # ---------------------------------------------------------------------------
  
  df <- df %>%
    left_join(
      checklist_resolution,
      by =
        "checklist_name",
      relationship =
        "many-to-one"
    ) %>%
    left_join(
      wcvp_lookup,
      by =
        "wcvp_accepted_id",
      relationship =
        "many-to-one"
    )
  
  
  # ---------------------------------------------------------------------------
  # Hierarchical Star assignment
  # ---------------------------------------------------------------------------
  
  df %>%
    mutate(
      Star =
        case_when(
          !is.na(
            foj_exact_star
          ) ~
            foj_exact_star,
          
          foj_exact_conflict %in% TRUE ~
            NA_character_,
          
          !is.na(
            wcvp_consensus_star
          ) ~
            wcvp_consensus_star,
          
          TRUE ~
            NA_character_
        ),
      
      star_match_method =
        case_when(
          !is.na(
            foj_exact_star
          ) ~
            "foj_exact",
          
          foj_exact_conflict %in% TRUE ~
            "foj_exact_conflict",
          
          !is.na(
            wcvp_consensus_star
          ) ~
            "wcvp_consensus",
          
          star_conflict %in% TRUE ~
            "wcvp_conflict",
          
          checklist_wcvp_resolution ==
            "ambiguous" ~
            "wcvp_ambiguous",
          
          is.na(
            wcvp_accepted_id
          ) ~
            "no_star",
          
          TRUE ~
            "no_star"
        ),
      
      star_resolved =
        !is.na(
          Star
        )
    )
}