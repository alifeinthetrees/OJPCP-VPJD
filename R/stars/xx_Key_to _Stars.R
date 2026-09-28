# =============================================================================
# VPJD-OJPCP
# Key to Stars for the Flora of Japan
#
# Reproducible implementation building on:
#   Nakamura, N. (2012) Keys to Stars for the Japanese flora
#
# Purpose
# -------
# Implement the geographic component of Nakamura's Japanese Keys to Stars
# as an explicit, auditable decision system suitable for VPJD.
#
# The implementation distinguishes:
#
#   1. GX / non-native or cultivated taxa
#   2. Nakamura main geographic key
#   3. Nakamura supplementary key for limited geographic information
#   4. globally restricted non-endemic exception
#   5. infraspecific adjustment
#   6. exceptional taxonomic-distinctiveness review
#
# Important
# ---------
# Star assignment is based principally on GLOBAL geographic rarity.
#
# A taxon that is rare in Japan but widespread globally must not receive a
# high Star solely because its Japanese range is restricted.
#
# GX taxa are retained in VPJD but excluded from GHI calculations.
#
# This module assigns Stars.
# It does NOT calculate GHI.
#
# Outputs retain:
#   - Star
#   - rule ID
#   - assessment method
#   - review status
#   - explanatory note
#
# This makes every automated assignment reproducible and auditable.
# =============================================================================


# =============================================================================
# CONSTANTS
# =============================================================================

VALID_STARS <- c(
  "BK",
  "GD",
  "BU",
  "GN",
  "GX"
)


# =============================================================================
# RESULT CONSTRUCTOR
# =============================================================================

star_result <- function(
    Star = NA_character_,
    rule_id = NA_character_,
    method = NA_character_,
    review_required = FALSE,
    note = NA_character_
) {
  
  tibble::tibble(
    Star = Star,
    star_rule_id = rule_id,
    star_method = method,
    star_review_required = review_required,
    star_rule_note = note
  )
}


# =============================================================================
# JAPANESE DISTRIBUTION CATEGORY
# =============================================================================

japan_distribution_category <- function(
    n_districts_jp
) {
  
  if (
    is.na(n_districts_jp)
  ) {
    
    return(NA_character_)
  }
  
  
  if (
    n_districts_jp < 0
  ) {
    
    stop(
      "n_districts_jp cannot be negative."
    )
  }
  
  
  dplyr::case_when(
    n_districts_jp <= 1 ~
      "islands_or_leq1dist",
    
    n_districts_jp <= 2 ~
      "leq2dist",
    
    n_districts_jp <= 3 ~
      "leq3dist",
    
    n_districts_jp > 3 ~
      "gt3dist"
  )
}


# =============================================================================
# NAKAMURA MAIN KEY: JAPANESE ENDEMICS
# =============================================================================

assign_endemic_star <- function(
    small_islands_only,
    n_districts_jp,
    n_prefectures_jp,
    rare_within_range = FALSE,
    almost_all_districts = NA
) {
  
  # ---------------------------------------------------------------------------
  # Nakamura:
  # Endemic to Ryukyu, Ogasawara and/or Izu island groups
  # ---------------------------------------------------------------------------
  
  if (
    isTRUE(
      small_islands_only
    )
  ) {
    
    return(
      star_result(
        Star = "BK",
        rule_id = "N12-E01",
        method = "nakamura_main_key",
        note =
          "Japanese endemic restricted to specified small-island groups."
      )
    )
  }
  
  
  # ---------------------------------------------------------------------------
  # Need district and prefecture information for main endemic key
  # ---------------------------------------------------------------------------
  
  if (
    is.na(n_districts_jp) ||
    is.na(n_prefectures_jp)
  ) {
    
    return(
      star_result(
        rule_id = "N12-E00",
        method = "nakamura_main_key",
        review_required = TRUE,
        note =
          "Insufficient Japanese distribution information for main endemic key."
      )
    )
  }
  
  
  # ---------------------------------------------------------------------------
  # Nakamura:
  # Not so widespread:
  # <= 2 districts AND <= 14 prefectures
  # ---------------------------------------------------------------------------
  
  if (
    n_districts_jp <= 2 &&
    n_prefectures_jp <= 14
  ) {
    
    if (
      n_prefectures_jp <= 7
    ) {
      
      return(
        star_result(
          Star = "BK",
          rule_id = "N12-E02",
          method = "nakamura_main_key",
          note =
            "Japanese endemic in <=2 districts, <=14 prefectures, and <=7 prefectures."
        )
      )
      
    } else {
      
      return(
        star_result(
          Star = "GD",
          rule_id = "N12-E03",
          method = "nakamura_main_key",
          note =
            "Japanese endemic in <=2 districts, <=14 prefectures, but >7 prefectures."
        )
      )
    }
  }
  
  
  # ---------------------------------------------------------------------------
  # Nakamura:
  # Widespread in Japan:
  # >2 districts OR >14 prefectures
  #
  # Occurs in <=10 prefectures
  # ---------------------------------------------------------------------------
  
  if (
    n_prefectures_jp <= 10
  ) {
    
    if (
      n_prefectures_jp <= 5 &&
      isTRUE(
        rare_within_range
      )
    ) {
      
      return(
        star_result(
          Star = "BK",
          rule_id = "N12-E04",
          method = "nakamura_main_key",
          note =
            "Wider Japanese endemic but <=5 prefectures and rare within its range."
        )
      )
      
    } else {
      
      return(
        star_result(
          Star = "GD",
          rule_id = "N12-E05",
          method = "nakamura_main_key",
          note =
            "Wider Japanese endemic occurring in <=10 prefectures."
        )
      )
    }
  }
  
  
  # ---------------------------------------------------------------------------
  # Nakamura:
  # >10 prefectures:
  #   not in all districts  = BLUE
  #   in almost all districts = GREEN
  #
  # IMPORTANT:
  # Do NOT infer "almost all districts" from an arbitrary district count.
  # It must be represented explicitly in the source evidence.
  # ---------------------------------------------------------------------------
  
  if (
    n_prefectures_jp > 10
  ) {
    
    if (
      isTRUE(
        almost_all_districts
      )
    ) {
      
      return(
        star_result(
          Star = "GN",
          rule_id = "N12-E07",
          method = "nakamura_main_key",
          note =
            "Japanese endemic in >10 prefectures and almost all districts."
        )
      )
    }
    
    
    if (
      identical(
        almost_all_districts,
        FALSE
      )
    ) {
      
      return(
        star_result(
          Star = "BU",
          rule_id = "N12-E06",
          method = "nakamura_main_key",
          note =
            "Japanese endemic in >10 prefectures but not almost all districts."
        )
      )
    }
    
    
    return(
      star_result(
        rule_id = "N12-E08",
        method = "nakamura_main_key",
        review_required = TRUE,
        note =
          "Need explicit evidence for Nakamura's 'almost all districts' criterion."
      )
    )
  }
  
  
  star_result(
    rule_id = "N12-E99",
    method = "nakamura_main_key",
    review_required = TRUE,
    note =
      "Endemic distribution did not resolve through the main key."
  )
}


# =============================================================================
# NAKAMURA MAIN KEY: NON-ENDEMIC TAXA
# =============================================================================

assign_nonendemic_star <- function(
    n_districts_jp,
    extra_region,
    taiwan_cov = NA_character_,
    korea_restrict = NA_character_,
    ks_restrict = NA_character_,
    china_provinces = NA
) {
  
  jp_cat <-
    japan_distribution_category(
      n_districts_jp
    )
  
  
  # ---------------------------------------------------------------------------
  # Insufficient Japanese range information
  # ---------------------------------------------------------------------------
  
  if (
    is.na(jp_cat)
  ) {
    
    return(
      star_result(
        rule_id = "N12-N00",
        method = "nakamura_main_key",
        review_required = TRUE,
        note =
          "Japanese district distribution is unavailable."
      )
    )
  }
  
  
  # ---------------------------------------------------------------------------
  # Nakamura:
  # Beyond only one of Taiwan, Korea, Kuriles/Sakhalin or restricted China
  # -> GREEN
  # ---------------------------------------------------------------------------
  
  if (
    extra_region %in%
    c(
      "multiple",
      "beyond_key_regions"
    )
  ) {
    
    return(
      star_result(
        Star = "GN",
        rule_id = "N12-N01",
        method = "nakamura_main_key",
        note =
          "Distribution extends beyond the restricted neighbouring-region conditions."
      )
    )
  }
  
  
  # ---------------------------------------------------------------------------
  # Taiwan
  # ---------------------------------------------------------------------------
  
  if (
    identical(
      extra_region,
      "Taiwan"
    )
  ) {
    
    if (
      is.na(taiwan_cov)
    ) {
      
      return(
        star_result(
          rule_id = "N12-T00",
          method = "nakamura_main_key",
          review_required = TRUE,
          note =
            "Taiwan distribution extent is required."
        )
      )
    }
    
    
    if (
      identical(
        taiwan_cov,
        "<half"
      )
    ) {
      
      stars <- c(
        islands_or_leq1dist = "BK",
        leq2dist = "GD",
        leq3dist = "BU",
        gt3dist = "GN"
      )
      
      return(
        star_result(
          Star = unname(
            stars[[jp_cat]]
          ),
          rule_id = "N12-T01",
          method = "nakamura_main_key",
          note =
            "Outside Japan restricted to < half of Taiwan."
        )
      )
    }
    
    
    if (
      identical(
        taiwan_cov,
        ">=half"
      )
    ) {
      
      stars <- c(
        islands_or_leq1dist = "GD",
        leq2dist = "BU",
        leq3dist = "GN",
        gt3dist = "GN"
      )
      
      return(
        star_result(
          Star = unname(
            stars[[jp_cat]]
          ),
          rule_id = "N12-T02",
          method = "nakamura_main_key",
          note =
            "Outside Japan occurs across >= half of Taiwan."
        )
      )
    }
    
    
    return(
      star_result(
        rule_id = "N12-T99",
        method = "nakamura_main_key",
        review_required = TRUE,
        note =
          "Unrecognised Taiwan distribution category."
      )
    )
  }
  
  
  # ---------------------------------------------------------------------------
  # Korea
  # ---------------------------------------------------------------------------
  
  if (
    identical(
      extra_region,
      "Korea"
    )
  ) {
    
    if (
      is.na(korea_restrict)
    ) {
      
      return(
        star_result(
          rule_id = "N12-K00",
          method = "nakamura_main_key",
          review_required = TRUE,
          note =
            "Korean distribution extent is required."
        )
      )
    }
    
    
    if (
      identical(
        korea_restrict,
        "islands_sparse"
      )
    ) {
      
      stars <- c(
        islands_or_leq1dist = "BK",
        leq2dist = "GD",
        leq3dist = "BU",
        gt3dist = "GN"
      )
      
      return(
        star_result(
          Star = unname(
            stars[[jp_cat]]
          ),
          rule_id = "N12-K01",
          method = "nakamura_main_key",
          note =
            "Outside Japan restricted to Korean islands or sparse distribution."
        )
      )
    }
    
    
    if (
      identical(
        korea_restrict,
        "widespread"
      )
    ) {
      
      stars <- c(
        islands_or_leq1dist = "GD",
        leq2dist = "BU",
        leq3dist = "GN",
        gt3dist = "GN"
      )
      
      return(
        star_result(
          Star = unname(
            stars[[jp_cat]]
          ),
          rule_id = "N12-K02",
          method = "nakamura_main_key",
          note =
            "Korean distribution not restricted to islands or sparse occurrences."
        )
      )
    }
    
    
    return(
      star_result(
        rule_id = "N12-K99",
        method = "nakamura_main_key",
        review_required = TRUE,
        note =
          "Unrecognised Korean distribution category."
      )
    )
  }
  
  
  # ---------------------------------------------------------------------------
  # Kuriles and Sakhalin
  # ---------------------------------------------------------------------------
  
  if (
    identical(
      extra_region,
      "KurilesSakhalin"
    )
  ) {
    
    if (
      is.na(ks_restrict)
    ) {
      
      return(
        star_result(
          rule_id = "N12-KS00",
          method = "nakamura_main_key",
          review_required = TRUE,
          note =
            "Kuriles/Sakhalin distribution extent is required."
        )
      )
    }
    
    
    if (
      identical(
        ks_restrict,
        "south_only"
      )
    ) {
      
      stars <- c(
        islands_or_leq1dist = "BK",
        leq2dist = "GD",
        leq3dist = "BU",
        gt3dist = "GN"
      )
      
      return(
        star_result(
          Star = unname(
            stars[[jp_cat]]
          ),
          rule_id = "N12-KS01",
          method = "nakamura_main_key",
          note =
            "Outside Japan restricted to the southern Kuriles."
        )
      )
    }
    
    
    if (
      identical(
        ks_restrict,
        "not_south_only"
      )
    ) {
      
      stars <- c(
        islands_or_leq1dist = "GD",
        leq2dist = "BU",
        leq3dist = "GN",
        gt3dist = "GN"
      )
      
      return(
        star_result(
          Star = unname(
            stars[[jp_cat]]
          ),
          rule_id = "N12-KS02",
          method = "nakamura_main_key",
          note =
            "Kuriles/Sakhalin distribution extends beyond the southern Kuriles."
        )
      )
    }
    
    
    return(
      star_result(
        rule_id = "N12-KS99",
        method = "nakamura_main_key",
        review_required = TRUE,
        note =
          "Unrecognised Kuriles/Sakhalin distribution category."
      )
    )
  }
  
  
  # ---------------------------------------------------------------------------
  # China
  # ---------------------------------------------------------------------------
  
  if (
    identical(
      extra_region,
      "China"
    )
  ) {
    
    # Missing information must remain missing.
    # It must NOT be silently interpreted as >2 provinces.
    
    if (
      length(china_provinces) == 0 ||
      is.na(china_provinces)
    ) {
      
      return(
        star_result(
          rule_id = "N12-C00",
          method = "nakamura_main_key",
          review_required = TRUE,
          note =
            "Number/extent of Chinese provinces is unavailable."
        )
      )
    }
    
    
    china_cat <- NA_character_
    
    
    if (
      is.numeric(
        china_provinces
      )
    ) {
      
      china_cat <-
        ifelse(
          china_provinces <= 2,
          "<=2",
          ">2"
        )
      
    } else {
      
      china_cat <-
        as.character(
          china_provinces
        )
    }
    
    
    if (
      identical(
        china_cat,
        "<=2"
      )
    ) {
      
      stars <- c(
        islands_or_leq1dist = "GD",
        leq2dist = "BU",
        leq3dist = "GN",
        gt3dist = "GN"
      )
      
      return(
        star_result(
          Star = unname(
            stars[[jp_cat]]
          ),
          rule_id = "N12-C01",
          method = "nakamura_main_key",
          note =
            "Outside Japan restricted to <=2 Chinese provinces."
        )
      )
    }
    
    
    if (
      identical(
        china_cat,
        ">2"
      )
    ) {
      
      return(
        star_result(
          Star = "GN",
          rule_id = "N12-C02",
          method = "nakamura_main_key",
          note =
            "Outside Japan occurs in >2 Chinese provinces."
        )
      )
    }
    
    
    return(
      star_result(
        rule_id = "N12-C99",
        method = "nakamura_main_key",
        review_required = TRUE,
        note =
          "Unrecognised Chinese distribution category."
      )
    )
  }
  
  
  # ---------------------------------------------------------------------------
  # No supported global-distribution branch
  # ---------------------------------------------------------------------------
  
  star_result(
    rule_id = "N12-N99",
    method = "nakamura_main_key",
    review_required = TRUE,
    note =
      "Non-endemic global distribution is not sufficiently represented by the main key."
  )
}


# =============================================================================
# NAKAMURA SUPPLEMENTARY KEY:
# LIMITED GEOGRAPHIC INFORMATION
# =============================================================================

assign_limited_information_star <- function(
    endemic_to_japan,
    small_islands_only = FALSE,
    n_districts_jp = NA_integer_,
    across_mainland_japan = NA
) {
  
  # Nakamura states that non-endemic taxa should return to the main Keys.
  
  if (
    !isTRUE(
      endemic_to_japan
    )
  ) {
    
    return(
      star_result(
        rule_id = "N12-L00",
        method = "nakamura_limited_information_key",
        review_required = TRUE,
        note =
          "Nakamura's limited-information supplementary key directs non-endemics back to the main key."
      )
    )
  }
  
  
  if (
    isTRUE(
      small_islands_only
    )
  ) {
    
    return(
      star_result(
        Star = "BK",
        rule_id = "N12-L01",
        method = "nakamura_limited_information_key",
        note =
          "Japanese endemic restricted to specified small-island groups."
      )
    )
  }
  
  
  if (
    !is.na(n_districts_jp) &&
    n_districts_jp <= 2
  ) {
    
    return(
      star_result(
        Star = "GD",
        rule_id = "N12-L02",
        method = "nakamura_limited_information_key",
        note =
          "Limited-information Japanese endemic restricted to <=2 districts."
      )
    )
  }
  
  
  if (
    !is.na(n_districts_jp) &&
    n_districts_jp > 2
  ) {
    
    if (
      identical(
        across_mainland_japan,
        FALSE
      )
    ) {
      
      return(
        star_result(
          Star = "BU",
          rule_id = "N12-L03",
          method = "nakamura_limited_information_key",
          note =
            "Limited-information endemic occurs over >2 districts but not across mainland Japan."
        )
      )
    }
    
    
    if (
      isTRUE(
        across_mainland_japan
      )
    ) {
      
      return(
        star_result(
          Star = "GN",
          rule_id = "N12-L04",
          method = "nakamura_limited_information_key",
          note =
            "Limited-information endemic occurs across mainland Japan."
        )
      )
    }
  }
  
  
  # Nakamura also allows a distribution described only as "Japan"
  # to resolve to Green.
  
  if (
    isTRUE(
      across_mainland_japan
    )
  ) {
    
    return(
      star_result(
        Star = "GN",
        rule_id = "N12-L05",
        method = "nakamura_limited_information_key",
        note =
          "Distribution available only at the broad 'Japan' level."
      )
    )
  }
  
  
  star_result(
    rule_id = "N12-L99",
    method = "nakamura_limited_information_key",
    review_required = TRUE,
    note =
      "Available geographic information remains insufficient for supplementary assignment."
  )
}


# =============================================================================
# NAKAMURA EXCEPTION:
# GLOBALLY RESTRICTED NON-ENDEMIC TAXA
# =============================================================================

assign_global_restricted_exception <- function(
    endemic_to_japan,
    japan_islands_or_one_district,
    outside_japan_small_islands_or_one_district
) {
  
  if (
    isFALSE(
      endemic_to_japan
    ) &&
    isTRUE(
      japan_islands_or_one_district
    ) &&
    isTRUE(
      outside_japan_small_islands_or_one_district
    )
  ) {
    
    return(
      star_result(
        Star = "BU",
        rule_id = "N12-GR01",
        method = "nakamura_global_range_exception",
        note =
          "Non-endemic taxon with globally restricted distribution meeting both Nakamura criteria."
      )
    )
  }
  
  
  star_result(
    rule_id = "N12-GR00",
    method = "nakamura_global_range_exception",
    review_required = TRUE,
    note =
      "Taxon does not meet both criteria for Nakamura's globally restricted non-endemic exception."
  )
}


# =============================================================================
# NAKAMURA INFRASPECIFIC ADJUSTMENT
# =============================================================================

adjust_infraspecific_star <- function(
    species_star,
    infra_default_star,
    extremely_localised = FALSE,
    morphologically_distinct = FALSE
) {
  
  if (
    is.na(species_star) ||
    is.na(infra_default_star)
  ) {
    
    return(
      star_result(
        rule_id = "N12-I00",
        method = "nakamura_infraspecific_adjustment",
        review_required = TRUE,
        note =
          "Species and infraspecific default Stars are both required."
      )
    )
  }
  
  
  # ---------------------------------------------------------------------------
  # Table 2.1, Nakamura 2012
  #
  # Rows    = species-level Star
  # Columns = infraspecific Star based on range alone
  # ---------------------------------------------------------------------------
  
  adjustment <- list(
    
    BK = c(
      BK = "BK"
    ),
    
    GD = c(
      BK = "BK",
      GD = "GD"
    ),
    
    BU = c(
      BK = "GD",
      GD = "GD",
      BU = "BU"
    ),
    
    GN = c(
      BK = "BU",
      GD = "BU",
      BU = "BU",
      GN = "GN"
    )
  )
  
  
  if (
    !species_star %in%
    names(adjustment)
  ) {
    
    return(
      star_result(
        rule_id = "N12-I98",
        method = "nakamura_infraspecific_adjustment",
        review_required = TRUE,
        note =
          "Species Star is not valid for Nakamura infraspecific adjustment."
      )
    )
  }
  
  
  if (
    !infra_default_star %in%
    names(
      adjustment[[species_star]]
    )
  ) {
    
    return(
      star_result(
        rule_id = "N12-I99",
        method = "nakamura_infraspecific_adjustment",
        review_required = TRUE,
        note =
          "Infraspecific default Star is inconsistent with the containing species Star."
      )
    )
  }
  
  
  adjusted_star <-
    unname(
      adjustment[[species_star]][
        [infra_default_star]
      ]
    )
  
  
  # ---------------------------------------------------------------------------
  # Nakamura's starred cases:
  #
  # GD species / BK infra
  # BU species / GD infra
  # GN species / BU infra
  #
  # Higher infraspecific status can be retained where the infra taxon is
  # extremely localised OR particularly morphologically distinct.
  # ---------------------------------------------------------------------------
  
  exceptional_pair <-
    (
      species_star == "GD" &&
        infra_default_star == "BK"
    ) ||
    (
      species_star == "BU" &&
        infra_default_star == "GD"
    ) ||
    (
      species_star == "GN" &&
        infra_default_star == "BU"
    )
  
  
  if (
    exceptional_pair &&
    (
      isTRUE(
        extremely_localised
      ) ||
      isTRUE(
        morphologically_distinct
      )
    )
  ) {
    
    adjusted_star <-
      infra_default_star
  }
  
  
  star_result(
    Star = adjusted_star,
    rule_id =
      ifelse(
        exceptional_pair,
        "N12-I02",
        "N12-I01"
      ),
    method =
      "nakamura_infraspecific_adjustment",
    note =
      ifelse(
        exceptional_pair &&
          adjusted_star ==
          infra_default_star,
        
        "Higher infraspecific Star retained under Nakamura's exceptional criteria.",
        
        "Infraspecific Star adjusted according to Nakamura Table 2.1."
      )
  )
}


# =============================================================================
# MASTER KEY
# =============================================================================

assign_star <- function(
    endemic_to_japan,
    small_islands_only = FALSE,
    n_districts_jp = NA_integer_,
    n_prefectures_jp = NA_integer_,
    rare_within_range = FALSE,
    almost_all_districts = NA,
    
    # Native status
    introduced = FALSE,
    naturalised = FALSE,
    cultivated_only = FALSE,
    
    # Geographic evidence
    geographic_information_limited = FALSE,
    across_mainland_japan = NA,
    
    # Distribution outside Japan
    extra_region = NA_character_,
    taiwan_cov = NA_character_,
    korea_restrict = NA_character_,
    ks_restrict = NA_character_,
    china_provinces = NA,
    
    # Globally restricted exception
    global_range_exception = FALSE,
    japan_islands_or_one_district = FALSE,
    outside_japan_small_islands_or_one_district = FALSE,
    
    # Taxonomic status
    taxon_rank = "species",
    species_star = NA_character_,
    extremely_localised = FALSE,
    morphologically_distinct = FALSE,
    
    # Higher-level distinctiveness
    higher_taxonomic_distinctiveness = FALSE
) {
  
  # ---------------------------------------------------------------------------
  # 0. GX
  # ---------------------------------------------------------------------------
  
  if (
    isTRUE(introduced) ||
    isTRUE(naturalised) ||
    isTRUE(cultivated_only)
  ) {
    
    return(
      star_result(
        Star = "GX",
        rule_id = "VPJD-GX01",
        method = "vpjd_native_status",
        note =
          "Introduced, naturalised or cultivated-only taxon."
      )
    )
  }
  
  
  # ---------------------------------------------------------------------------
  # 1. Hybrids and cultivars
  #
  # Nakamura: no Star by default.
  # ---------------------------------------------------------------------------
  
  if (
    taxon_rank %in%
    c(
      "hybrid",
      "cultivar"
    )
  ) {
    
    return(
      star_result(
        rule_id = "N12-TAX01",
        method = "nakamura_taxonomic_rule",
        note =
          "Hybrid or cultivar: no Star assigned by default."
      )
    )
  }
  
  
  # ---------------------------------------------------------------------------
  # 2. Formae
  #
  # Nakamura: same Star as containing taxon.
  # ---------------------------------------------------------------------------
  
  if (
    identical(
      taxon_rank,
      "forma"
    )
  ) {
    
    if (
      is.na(
        species_star
      )
    ) {
      
      return(
        star_result(
          rule_id = "N12-TAX02",
          method = "nakamura_taxonomic_rule",
          review_required = TRUE,
          note =
            "Forma requires the Star of its containing taxon."
        )
      )
    }
    
    
    return(
      star_result(
        Star = species_star,
        rule_id = "N12-TAX03",
        method = "nakamura_taxonomic_rule",
        note =
          "Forma inherits Star from containing taxon."
      )
    )
  }
  
  
  # ---------------------------------------------------------------------------
  # 3. Default geographic Star
  # ---------------------------------------------------------------------------
  
  if (
    isTRUE(
      geographic_information_limited
    )
  ) {
    
    result <-
      assign_limited_information_star(
        endemic_to_japan =
          endemic_to_japan,
        
        small_islands_only =
          small_islands_only,
        
        n_districts_jp =
          n_districts_jp,
        
        across_mainland_japan =
          across_mainland_japan
      )
    
  } else if (
    isTRUE(
      endemic_to_japan
    )
  ) {
    
    result <-
      assign_endemic_star(
        small_islands_only =
          small_islands_only,
        
        n_districts_jp =
          n_districts_jp,
        
        n_prefectures_jp =
          n_prefectures_jp,
        
        rare_within_range =
          rare_within_range,
        
        almost_all_districts =
          almost_all_districts
      )
    
  } else {
    
    result <-
      assign_nonendemic_star(
        n_districts_jp =
          n_districts_jp,
        
        extra_region =
          extra_region,
        
        taiwan_cov =
          taiwan_cov,
        
        korea_restrict =
          korea_restrict,
        
        ks_restrict =
          ks_restrict,
        
        china_provinces =
          china_provinces
      )
  }
  
  
  # ---------------------------------------------------------------------------
  # 4. Nakamura globally restricted non-endemic exception
  #
  # Apply only where the normal key did not provide a satisfactory result
  # and the exception has explicitly been invoked.
  # ---------------------------------------------------------------------------
  
  if (
    isTRUE(
      global_range_exception
    )
  ) {
    
    exception <-
      assign_global_restricted_exception(
        endemic_to_japan =
          endemic_to_japan,
        
        japan_islands_or_one_district =
          japan_islands_or_one_district,
        
        outside_japan_small_islands_or_one_district =
          outside_japan_small_islands_or_one_district
      )
    
    
    if (
      !is.na(
        exception$Star
      )
    ) {
      
      result <-
        exception
    }
  }
  
  
  # ---------------------------------------------------------------------------
  # 5. Infraspecific adjustment
  # ---------------------------------------------------------------------------
  
  if (
    taxon_rank %in%
    c(
      "subspecies",
      "subsp.",
      "ssp.",
      "variety",
      "var."
    ) &&
    !is.na(
      result$Star
    )
  ) {
    
    if (
      is.na(
        species_star
      )
    ) {
      
      result$star_review_required <-
        TRUE
      
      result$star_rule_note <-
        paste(
          result$star_rule_note,
          "Infraspecific taxon requires species-level Star before final adjustment."
        )
      
      return(
        result
      )
    }
    
    
    result <-
      adjust_infraspecific_star(
        species_star =
          species_star,
        
        infra_default_star =
          result$Star,
        
        extremely_localised =
          extremely_localised,
        
        morphologically_distinct =
          morphologically_distinct
      )
  }
  
  
  # ---------------------------------------------------------------------------
  # 6. Higher taxonomic distinctiveness
  #
  # Nakamura permits an upgrade where a species is distinctive at a higher
  # taxonomic level and was already close to the boundary for the higher Star.
  #
  # "Close to borderline" requires evidence/expert judgement and is not
  # mechanically inferable here. Therefore flag for review rather than
  # automatically upgrading.
  # ---------------------------------------------------------------------------
  
  if (
    isTRUE(
      higher_taxonomic_distinctiveness
    )
  ) {
    
    result$star_review_required <-
      TRUE
    
    result$star_rule_note <-
      paste(
        result$star_rule_note,
        "Higher taxonomic distinctiveness identified; review Nakamura upgrade criterion."
      )
  }
  
  
  result
}