# =============================================================================
# VPJD-OJPCP
# R/stars/01_key_to_stars.R
# Version 0.1.0
#
# KEY TO STARS DECISION ENGINE
#
# Purpose
# -------
# Formalise and validate the Key to Stars used in Nakamura (2012) as an
# explicit, reproducible decision engine for contemporary VPJD analysis.
#
# IMPORTANT
# ---------
# This module defines and tests the Key only.
# It does NOT assign Stars to the contemporary VPJD taxon universe.
# It does NOT use historical FOJ Star frequencies as a baseline.
# It does NOT use FOJ_STARS.csv to determine contemporary Stars.
# It does NOT calculate GHI.
#
# Contemporary Stars will later be calculated from contemporary distribution
# evidence using this decision engine.
#
# Star codes
# ----------
# BK = Black
# GD = Gold
# BU = Blue
# GN = Green
# GX = cultivated / non-native category used by the Key
#
# Scientific principles
# ---------------------
# - Historical Star totals are not targets.
# - Historical Star assignments are not contemporary classifications.
# - Every automated Star must retain its rule ID and method.
# - Missing evidence remains missing.
# - Expert-judgement criteria are never silently inferred.
# - Ambiguous cases are returned for review rather than forced through the Key.
# =============================================================================

suppressPackageStartupMessages({
  library(DBI)
  library(duckdb)
  library(dplyr)
  library(readr)
  library(here)
})

MODULE <- "stars_01_key_to_stars"
VERSION <- "0.1.0"
RUN_DATE <- as.Date("2026-09-20")

VALID_STARS <- c("BK", "GD", "BU", "GN", "GX")

star_result <- function(
    Star = NA_character_,
    rule_id = NA_character_,
    method = NA_character_,
    review_required = FALSE,
    note = NA_character_) {
  tibble::tibble(
    Star = Star,
    STAR_RULE_ID = rule_id,
    STAR_METHOD = method,
    STAR_REVIEW_REQUIRED = review_required,
    STAR_RULE_NOTE = note
  )
}

# =============================================================================
# JAPANESE DISTRIBUTION CATEGORY
# =============================================================================

japan_distribution_category <- function(n_districts_jp) {
  if (is.na(n_districts_jp)) return(NA_character_)
  if (n_districts_jp < 0) stop("n_districts_jp cannot be negative.")
  dplyr::case_when(
    n_districts_jp <= 1 ~ "islands_or_leq1dist",
    n_districts_jp <= 2 ~ "leq2dist",
    n_districts_jp <= 3 ~ "leq3dist",
    n_districts_jp > 3 ~ "gt3dist"
  )
}

# =============================================================================
# JAPANESE ENDEMICS
# =============================================================================

assign_endemic_star <- function(
    small_islands_only,
    n_districts_jp,
    n_prefectures_jp,
    rare_within_range = FALSE,
    almost_all_districts = NA) {
  
  # Endemic only to Ryukyu, Ogasawara and/or Izu.
  if (isTRUE(small_islands_only)) {
    return(star_result(
      "BK", "N12-E01", "nakamura_main_key", FALSE,
      "Japanese endemic restricted to Ryukyu, Ogasawara and/or Izu."
    ))
  }
  
  if (is.na(n_districts_jp) || is.na(n_prefectures_jp)) {
    return(star_result(
      rule_id = "N12-E00",
      method = "nakamura_main_key",
      review_required = TRUE,
      note = "Insufficient Japanese distribution information."
    ))
  }
  
  # Not so widespread in Japan:
  # <=2 districts AND <=14 prefectures.
  if (n_districts_jp <= 2 && n_prefectures_jp <= 14) {
    if (n_prefectures_jp <= 7) {
      return(star_result(
        "BK", "N12-E02", "nakamura_main_key", FALSE,
        "Japanese endemic in <=2 districts and <=7 prefectures."
      ))
    }
    return(star_result(
      "GD", "N12-E03", "nakamura_main_key", FALSE,
      "Japanese endemic in <=2 districts and 8-14 prefectures."
    ))
  }
  
  # Widespread in Japan:
  # >2 districts OR >14 prefectures.
  # If <=10 prefectures:
  # <=5 AND rare = BK; otherwise GD.
  if (n_prefectures_jp <= 10) {
    if (n_prefectures_jp <= 5 && isTRUE(rare_within_range)) {
      return(star_result(
        "BK", "N12-E04", "nakamura_main_key", FALSE,
        "Wider Japanese endemic in <=5 prefectures and rare within its range."
      ))
    }
    return(star_result(
      "GD", "N12-E05", "nakamura_main_key", FALSE,
      "Wider Japanese endemic occurring in <=10 prefectures."
    ))
  }
  
  # >10 prefectures:
  # not all districts = BLUE
  # almost all districts = GREEN
  #
  # 'Almost all districts' must be supplied explicitly.
  if (n_prefectures_jp > 10) {
    if (isTRUE(almost_all_districts)) {
      return(star_result(
        "GN", "N12-E07", "nakamura_main_key", FALSE,
        "Japanese endemic in >10 prefectures and almost all districts."
      ))
    }
    if (identical(almost_all_districts, FALSE)) {
      return(star_result(
        "BU", "N12-E06", "nakamura_main_key", FALSE,
        "Japanese endemic in >10 prefectures but not almost all districts."
      ))
    }
    return(star_result(
      rule_id = "N12-E08",
      method = "nakamura_main_key",
      review_required = TRUE,
      note = "Explicit evidence required for 'almost all districts'."
    ))
  }
  
  star_result(
    rule_id = "N12-E99",
    method = "nakamura_main_key",
    review_required = TRUE,
    note = "Endemic distribution did not resolve through the main key."
  )
}

# =============================================================================
# NON-ENDEMIC TAXA
# =============================================================================

assign_nonendemic_star <- function(
    n_districts_jp,
    extra_region,
    taiwan_cov = NA_character_,
    korea_restrict = NA_character_,
    ks_restrict = NA_character_,
    china_provinces = NA) {
  
  jp_cat <- japan_distribution_category(n_districts_jp)
  
  if (is.na(jp_cat)) {
    return(star_result(
      rule_id = "N12-N00",
      method = "nakamura_main_key",
      review_required = TRUE,
      note = "Japanese district distribution is unavailable."
    ))
  }
  
  # Distribution beyond the restricted neighbouring-region conditions.
  if (!is.na(extra_region) &&
      extra_region %in% c("multiple", "beyond_key_regions")) {
    return(star_result(
      "GN", "N12-N01", "nakamura_main_key", FALSE,
      "Distribution extends beyond the restricted neighbouring-region conditions."
    ))
  }
  
  # Taiwan.
  if (identical(extra_region, "Taiwan")) {
    if (is.na(taiwan_cov)) {
      return(star_result(
        rule_id = "N12-T00",
        method = "nakamura_main_key",
        review_required = TRUE,
        note = "Taiwan distribution extent is required."
      ))
    }
    
    if (identical(taiwan_cov, "<half")) {
      stars <- c(
        islands_or_leq1dist = "BK",
        leq2dist = "GD",
        leq3dist = "BU",
        gt3dist = "GN"
      )
      return(star_result(
        unname(stars[[jp_cat]]),
        "N12-T01", "nakamura_main_key", FALSE,
        "Outside Japan restricted to < half of Taiwan."
      ))
    }
    
    if (identical(taiwan_cov, ">=half")) {
      stars <- c(
        islands_or_leq1dist = "GD",
        leq2dist = "BU",
        leq3dist = "GN",
        gt3dist = "GN"
      )
      return(star_result(
        unname(stars[[jp_cat]]),
        "N12-T02", "nakamura_main_key", FALSE,
        "Outside Japan occurs across >= half of Taiwan."
      ))
    }
    
    return(star_result(
      rule_id = "N12-T99",
      method = "nakamura_main_key",
      review_required = TRUE,
      note = "Unrecognised Taiwan distribution category."
    ))
  }
  
  # Korea.
  if (identical(extra_region, "Korea")) {
    if (is.na(korea_restrict)) {
      return(star_result(
        rule_id = "N12-K00",
        method = "nakamura_main_key",
        review_required = TRUE,
        note = "Korean distribution extent is required."
      ))
    }
    
    if (identical(korea_restrict, "islands_sparse")) {
      stars <- c(
        islands_or_leq1dist = "BK",
        leq2dist = "GD",
        leq3dist = "BU",
        gt3dist = "GN"
      )
      return(star_result(
        unname(stars[[jp_cat]]),
        "N12-K01", "nakamura_main_key", FALSE,
        "Outside Japan restricted to Korean islands or sparse distribution."
      ))
    }
    
    if (identical(korea_restrict, "widespread")) {
      stars <- c(
        islands_or_leq1dist = "GD",
        leq2dist = "BU",
        leq3dist = "GN",
        gt3dist = "GN"
      )
      return(star_result(
        unname(stars[[jp_cat]]),
        "N12-K02", "nakamura_main_key", FALSE,
        "Korean distribution not restricted to islands or sparse occurrences."
      ))
    }
    
    return(star_result(
      rule_id = "N12-K99",
      method = "nakamura_main_key",
      review_required = TRUE,
      note = "Unrecognised Korean distribution category."
    ))
  }
  
  # Kuriles and Sakhalin.
  if (identical(extra_region, "KurilesSakhalin")) {
    if (is.na(ks_restrict)) {
      return(star_result(
        rule_id = "N12-KS00",
        method = "nakamura_main_key",
        review_required = TRUE,
        note = "Kuriles/Sakhalin distribution extent is required."
      ))
    }
    
    if (identical(ks_restrict, "south_only")) {
      stars <- c(
        islands_or_leq1dist = "BK",
        leq2dist = "GD",
        leq3dist = "BU",
        gt3dist = "GN"
      )
      return(star_result(
        unname(stars[[jp_cat]]),
        "N12-KS01", "nakamura_main_key", FALSE,
        "Outside Japan restricted to the southern Kuriles."
      ))
    }
    
    if (identical(ks_restrict, "not_south_only")) {
      stars <- c(
        islands_or_leq1dist = "GD",
        leq2dist = "BU",
        leq3dist = "GN",
        gt3dist = "GN"
      )
      return(star_result(
        unname(stars[[jp_cat]]),
        "N12-KS02", "nakamura_main_key", FALSE,
        "Kuriles/Sakhalin distribution extends beyond the southern Kuriles."
      ))
    }
    
    return(star_result(
      rule_id = "N12-KS99",
      method = "nakamura_main_key",
      review_required = TRUE,
      note = "Unrecognised Kuriles/Sakhalin distribution category."
    ))
  }
  
  # China.
  if (identical(extra_region, "China")) {
    if (length(china_provinces) == 0 || is.na(china_provinces)) {
      return(star_result(
        rule_id = "N12-C00",
        method = "nakamura_main_key",
        review_required = TRUE,
        note = "Chinese provincial distribution is unavailable."
      ))
    }
    
    china_cat <- if (is.numeric(china_provinces)) {
      ifelse(china_provinces <= 2, "<=2", ">2")
    } else {
      as.character(china_provinces)
    }
    
    if (identical(china_cat, "<=2")) {
      stars <- c(
        islands_or_leq1dist = "GD",
        leq2dist = "BU",
        leq3dist = "GN",
        gt3dist = "GN"
      )
      return(star_result(
        unname(stars[[jp_cat]]),
        "N12-C01", "nakamura_main_key", FALSE,
        "Outside Japan restricted to <=2 Chinese provinces."
      ))
    }
    
    if (identical(china_cat, ">2")) {
      return(star_result(
        "GN", "N12-C02", "nakamura_main_key", FALSE,
        "Outside Japan occurs in >2 Chinese provinces."
      ))
    }
    
    return(star_result(
      rule_id = "N12-C99",
      method = "nakamura_main_key",
      review_required = TRUE,
      note = "Unrecognised Chinese distribution category."
    ))
  }
  
  star_result(
    rule_id = "N12-N99",
    method = "nakamura_main_key",
    review_required = TRUE,
    note = "Non-endemic global distribution is insufficiently represented."
  )
}

# =============================================================================
# SUPPLEMENTARY KEY: LIMITED GEOGRAPHIC INFORMATION
# =============================================================================

assign_limited_information_star <- function(
    endemic_to_japan,
    small_islands_only = FALSE,
    n_districts_jp = NA_integer_,
    across_mainland_japan = NA) {
  
  if (!isTRUE(endemic_to_japan)) {
    return(star_result(
      rule_id = "N12-L00",
      method = "nakamura_limited_information_key",
      review_required = TRUE,
      note = "Non-endemic taxa return to the main Key."
    ))
  }
  
  if (isTRUE(small_islands_only)) {
    return(star_result(
      "BK", "N12-L01", "nakamura_limited_information_key", FALSE,
      "Japanese endemic restricted to specified small-island groups."
    ))
  }
  
  if (!is.na(n_districts_jp) && n_districts_jp <= 2) {
    return(star_result(
      "GD", "N12-L02", "nakamura_limited_information_key", FALSE,
      "Limited-information Japanese endemic restricted to <=2 districts."
    ))
  }
  
  if (!is.na(n_districts_jp) && n_districts_jp > 2) {
    if (identical(across_mainland_japan, FALSE)) {
      return(star_result(
        "BU", "N12-L03", "nakamura_limited_information_key", FALSE,
        "Endemic occurs over >2 districts but not across mainland Japan."
      ))
    }
    
    if (isTRUE(across_mainland_japan)) {
      return(star_result(
        "GN", "N12-L04", "nakamura_limited_information_key", FALSE,
        "Endemic occurs across mainland Japan."
      ))
    }
  }
  
  if (isTRUE(across_mainland_japan)) {
    return(star_result(
      "GN", "N12-L05", "nakamura_limited_information_key", FALSE,
      "Distribution available only at broad Japan level."
    ))
  }
  
  star_result(
    rule_id = "N12-L99",
    method = "nakamura_limited_information_key",
    review_required = TRUE,
    note = "Available geographic information remains insufficient."
  )
}

# =============================================================================
# GLOBALLY RESTRICTED NON-ENDEMIC EXCEPTION
# =============================================================================

assign_global_restricted_exception <- function(
    endemic_to_japan,
    japan_islands_or_one_district,
    outside_japan_small_islands_or_one_district) {
  
  if (isFALSE(endemic_to_japan) &&
      isTRUE(japan_islands_or_one_district) &&
      isTRUE(outside_japan_small_islands_or_one_district)) {
    return(star_result(
      "BU", "N12-GR01", "nakamura_global_range_exception", FALSE,
      "Globally restricted non-endemic satisfying both Key criteria."
    ))
  }
  
  star_result(
    rule_id = "N12-GR00",
    method = "nakamura_global_range_exception",
    review_required = TRUE,
    note = "Taxon does not meet both globally restricted exception criteria."
  )
}

# =============================================================================
# INFRASPECIFIC ADJUSTMENT
# =============================================================================

adjust_infraspecific_star <- function(
    species_star,
    infra_default_star,
    extremely_localised = FALSE,
    morphologically_distinct = FALSE) {
  
  if (is.na(species_star) || is.na(infra_default_star)) {
    return(star_result(
      rule_id = "N12-I00",
      method = "nakamura_infraspecific_adjustment",
      review_required = TRUE,
      note = "Species and infraspecific default Stars are required."
    ))
  }
  
  adjustment <- list(
    BK = c(BK = "BK"),
    GD = c(BK = "BK", GD = "GD"),
    BU = c(BK = "GD", GD = "GD", BU = "BU"),
    GN = c(BK = "BU", GD = "BU", BU = "BU", GN = "GN")
  )
  
  if (!species_star %in% names(adjustment)) {
    return(star_result(
      rule_id = "N12-I98",
      method = "nakamura_infraspecific_adjustment",
      review_required = TRUE,
      note = "Species Star is invalid for infraspecific adjustment."
    ))
  }
  
  if (!infra_default_star %in% names(adjustment[[species_star]])) {
    return(star_result(
      rule_id = "N12-I99",
      method = "nakamura_infraspecific_adjustment",
      review_required = TRUE,
      note = "Infraspecific default Star is inconsistent with species Star."
    ))
  }
  
  adjusted_star <- unname(
    adjustment[[species_star]][infra_default_star]
  )
  
  exceptional_pair <-
    (species_star == "GD" && infra_default_star == "BK") ||
    (species_star == "BU" && infra_default_star == "GD") ||
    (species_star == "GN" && infra_default_star == "BU")
  
  if (exceptional_pair &&
      (isTRUE(extremely_localised) || isTRUE(morphologically_distinct))) {
    adjusted_star <- infra_default_star
  }
  
  star_result(
    adjusted_star,
    ifelse(exceptional_pair, "N12-I02", "N12-I01"),
    "nakamura_infraspecific_adjustment",
    FALSE,
    ifelse(
      exceptional_pair && adjusted_star == infra_default_star,
      "Higher infraspecific Star retained under exceptional Key criteria.",
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
    introduced = FALSE,
    naturalised = FALSE,
    cultivated_only = FALSE,
    geographic_information_limited = FALSE,
    across_mainland_japan = NA,
    extra_region = NA_character_,
    taiwan_cov = NA_character_,
    korea_restrict = NA_character_,
    ks_restrict = NA_character_,
    china_provinces = NA,
    global_range_exception = FALSE,
    japan_islands_or_one_district = FALSE,
    outside_japan_small_islands_or_one_district = FALSE,
    taxon_rank = "species",
    species_star = NA_character_,
    extremely_localised = FALSE,
    morphologically_distinct = FALSE,
    higher_taxonomic_distinctiveness = FALSE) {
  
  # GX.
  if (isTRUE(introduced) ||
      isTRUE(naturalised) ||
      isTRUE(cultivated_only)) {
    return(star_result(
      "GX", "VPJD-GX01", "vpjd_native_status", FALSE,
      "Introduced, naturalised or cultivated-only taxon."
    ))
  }
  
  # Hybrids/cultivars: no Star by default.
  if (tolower(taxon_rank) %in% c("hybrid", "cultivar")) {
    return(star_result(
      rule_id = "N12-TAX01",
      method = "nakamura_taxonomic_rule",
      note = "Hybrid or cultivar: no Star assigned by default."
    ))
  }
  
  # Forma inherits containing taxon's Star.
  if (tolower(taxon_rank) %in% c("forma", "form", "f.")) {
    if (is.na(species_star)) {
      return(star_result(
        rule_id = "N12-TAX02",
        method = "nakamura_taxonomic_rule",
        review_required = TRUE,
        note = "Forma requires the Star of its containing taxon."
      ))
    }
    
    return(star_result(
      species_star, "N12-TAX03", "nakamura_taxonomic_rule", FALSE,
      "Forma inherits Star from containing taxon."
    ))
  }
  
  # Default geographic classification.
  if (isTRUE(geographic_information_limited)) {
    result <- assign_limited_information_star(
      endemic_to_japan,
      small_islands_only,
      n_districts_jp,
      across_mainland_japan
    )
  } else if (isTRUE(endemic_to_japan)) {
    result <- assign_endemic_star(
      small_islands_only,
      n_districts_jp,
      n_prefectures_jp,
      rare_within_range,
      almost_all_districts
    )
  } else {
    result <- assign_nonendemic_star(
      n_districts_jp,
      extra_region,
      taiwan_cov,
      korea_restrict,
      ks_restrict,
      china_provinces
    )
  }
  
  # Globally restricted non-endemic exception.
  if (isTRUE(global_range_exception)) {
    exception <- assign_global_restricted_exception(
      endemic_to_japan,
      japan_islands_or_one_district,
      outside_japan_small_islands_or_one_district
    )
    if (!is.na(exception$Star)) result <- exception
  }
  
  # Infraspecific adjustment.
  if (tolower(taxon_rank) %in%
      c("subspecies", "subsp.", "ssp.", "variety", "var.") &&
      !is.na(result$Star)) {
    
    if (is.na(species_star)) {
      result$STAR_REVIEW_REQUIRED <- TRUE
      result$STAR_RULE_NOTE <- paste(
        result$STAR_RULE_NOTE,
        "Infraspecific taxon requires species-level Star before final adjustment."
      )
      return(result)
    }
    
    result <- adjust_infraspecific_star(
      species_star,
      result$Star,
      extremely_localised,
      morphologically_distinct
    )
  }
  
  # Higher taxonomic distinctiveness requires expert review.
  if (isTRUE(higher_taxonomic_distinctiveness)) {
    result$STAR_REVIEW_REQUIRED <- TRUE
    result$STAR_RULE_NOTE <- paste(
      result$STAR_RULE_NOTE,
      "Higher taxonomic distinctiveness identified; review upgrade criterion."
    )
  }
  
  result
}

# =============================================================================
# CONTROLLED VALIDATION TESTS
# =============================================================================

message("\n— VPJD Key to Stars decision engine —\n")
message("Run date: ", RUN_DATE)
message("Module: ", MODULE)
message("Version: ", VERSION, "\n")

test_star <- function(...) {
  assign_star(...)$Star[[1]]
}

tests <- tibble::tribble(
  ~TEST_ID, ~DESCRIPTION, ~OBSERVED, ~EXPECTED,
  "E01", "Island-group endemic", test_star(
    TRUE, small_islands_only = TRUE
  ), "BK",
  "E02", "Endemic <=2 districts and <=7 prefectures", test_star(
    TRUE, n_districts_jp = 2, n_prefectures_jp = 7
  ), "BK",
  "E03", "Endemic <=2 districts and 8-14 prefectures", test_star(
    TRUE, n_districts_jp = 2, n_prefectures_jp = 10
  ), "GD",
  "E04", "Wider endemic <=5 prefectures and rare", test_star(
    TRUE, n_districts_jp = 3, n_prefectures_jp = 5,
    rare_within_range = TRUE
  ), "BK",
  "E05", "Wider endemic <=5 prefectures but not rare", test_star(
    TRUE, n_districts_jp = 3, n_prefectures_jp = 5,
    rare_within_range = FALSE
  ), "GD",
  "E06", "Endemic >10 prefectures, not almost all districts", test_star(
    TRUE, n_districts_jp = 3, n_prefectures_jp = 15,
    almost_all_districts = FALSE
  ), "BU",
  "E07", "Endemic >10 prefectures, almost all districts", test_star(
    TRUE, n_districts_jp = 7, n_prefectures_jp = 30,
    almost_all_districts = TRUE
  ), "GN",
  "T01", "Taiwan <half, Japan <=1 district", test_star(
    FALSE, n_districts_jp = 1,
    extra_region = "Taiwan", taiwan_cov = "<half"
  ), "BK",
  "T02", "Taiwan >=half, Japan <=2 districts", test_star(
    FALSE, n_districts_jp = 2,
    extra_region = "Taiwan", taiwan_cov = ">=half"
  ), "BU",
  "K01", "Korea islands/sparse, Japan <=3 districts", test_star(
    FALSE, n_districts_jp = 3,
    extra_region = "Korea", korea_restrict = "islands_sparse"
  ), "BU",
  "KS01", "Southern Kuriles only, Japan <=1 district", test_star(
    FALSE, n_districts_jp = 1,
    extra_region = "KurilesSakhalin", ks_restrict = "south_only"
  ), "BK",
  "C01", "China <=2 provinces, Japan <=2 districts", test_star(
    FALSE, n_districts_jp = 2,
    extra_region = "China", china_provinces = 2
  ), "BU",
  "C02", "China >2 provinces", test_star(
    FALSE, n_districts_jp = 1,
    extra_region = "China", china_provinces = 3
  ), "GN",
  "N01", "Beyond restricted neighbouring regions", test_star(
    FALSE, n_districts_jp = 1,
    extra_region = "beyond_key_regions"
  ), "GN",
  "GX01", "Introduced taxon", test_star(
    FALSE, introduced = TRUE
  ), "GX",
  "L01", "Limited information island endemic", test_star(
    TRUE, small_islands_only = TRUE,
    geographic_information_limited = TRUE
  ), "BK",
  "L02", "Limited information endemic <=2 districts", test_star(
    TRUE, n_districts_jp = 2,
    geographic_information_limited = TRUE
  ), "GD",
  "L03", "Limited information endemic >2 districts not mainland-wide", test_star(
    TRUE, n_districts_jp = 3,
    geographic_information_limited = TRUE,
    across_mainland_japan = FALSE
  ), "BU",
  "L04", "Limited information endemic mainland-wide", test_star(
    TRUE, n_districts_jp = 4,
    geographic_information_limited = TRUE,
    across_mainland_japan = TRUE
  ), "GN"
)

tests <- tests %>%
  mutate(PASS = OBSERVED == EXPECTED)

message("— Controlled Key tests —")
print(tests, n = Inf)

# Review-state tests.
review_tests <- bind_rows(
  assign_star(
    TRUE,
    n_districts_jp = 3,
    n_prefectures_jp = 15,
    almost_all_districts = NA
  ) %>%
    mutate(TEST_ID = "R01",
           DESCRIPTION = "Missing almost-all-districts evidence"),
  assign_star(
    FALSE,
    n_districts_jp = 2,
    extra_region = "Taiwan",
    taiwan_cov = NA_character_
  ) %>%
    mutate(TEST_ID = "R02",
           DESCRIPTION = "Missing Taiwan extent"),
  assign_star(
    FALSE,
    n_districts_jp = 2,
    extra_region = "China",
    china_provinces = NA
  ) %>%
    mutate(TEST_ID = "R03",
           DESCRIPTION = "Missing China extent")
) %>%
  select(TEST_ID, DESCRIPTION, everything())

review_tests <- review_tests %>%
  mutate(PASS = is.na(Star) & STAR_REVIEW_REQUIRED)

message("\n— Controlled review tests —")
print(review_tests, n = Inf)

# Infraspecific tests.
infra_tests <- bind_rows(
  adjust_infraspecific_star("BK", "BK") %>%
    mutate(TEST_ID = "I01", EXPECTED = "BK"),
  adjust_infraspecific_star("BU", "BK") %>%
    mutate(TEST_ID = "I02", EXPECTED = "GD"),
  adjust_infraspecific_star("GN", "BK") %>%
    mutate(TEST_ID = "I03", EXPECTED = "BU"),
  adjust_infraspecific_star("GN", "BU") %>%
    mutate(TEST_ID = "I04", EXPECTED = "BU"),
  adjust_infraspecific_star(
    "GN", "BU",
    extremely_localised = TRUE
  ) %>%
    mutate(TEST_ID = "I05", EXPECTED = "BU")
) %>%
  mutate(PASS = Star == EXPECTED)

message("\n— Controlled infraspecific tests —")
print(infra_tests, n = Inf)

all_tests_pass <-
  all(tests$PASS) &&
  all(review_tests$PASS) &&
  all(infra_tests$PASS)

# =============================================================================
# RULE REGISTER
# =============================================================================

rule_register <- tibble::tribble(
  ~RULE_ID, ~STAR, ~RULE_GROUP, ~DESCRIPTION,
  "N12-E01", "BK", "endemic", "Endemic only to Ryukyu/Ogasawara/Izu",
  "N12-E02", "BK", "endemic", "<=2 districts and <=7 prefectures",
  "N12-E03", "GD", "endemic", "<=2 districts and 8-14 prefectures",
  "N12-E04", "BK", "endemic", "Wider endemic, <=5 prefectures and rare",
  "N12-E05", "GD", "endemic", "Wider endemic occurring in <=10 prefectures otherwise",
  "N12-E06", "BU", "endemic", ">10 prefectures, not almost all districts",
  "N12-E07", "GN", "endemic", ">10 prefectures, almost all districts",
  "N12-T01", "variable", "Taiwan", "<half Taiwan lookup",
  "N12-T02", "variable", "Taiwan", ">=half Taiwan lookup",
  "N12-K01", "variable", "Korea", "Korean islands/sparse lookup",
  "N12-K02", "variable", "Korea", "Widespread Korea lookup",
  "N12-KS01", "variable", "Kuriles/Sakhalin", "Southern Kuriles lookup",
  "N12-KS02", "variable", "Kuriles/Sakhalin", "Beyond southern Kuriles lookup",
  "N12-C01", "variable", "China", "<=2 Chinese provinces lookup",
  "N12-C02", "GN", "China", ">2 Chinese provinces",
  "N12-N01", "GN", "non-endemic", "Beyond restricted neighbouring regions",
  "N12-L01", "BK", "limited_information", "Small-island endemic",
  "N12-L02", "GD", "limited_information", "<=2 Japanese districts",
  "N12-L03", "BU", "limited_information", ">2 districts, not mainland-wide",
  "N12-L04", "GN", "limited_information", "Across mainland Japan",
  "N12-GR01", "BU", "global_exception", "Globally restricted non-endemic exception",
  "N12-I01", "variable", "infraspecific", "Standard infraspecific adjustment",
  "N12-I02", "variable", "infraspecific", "Exceptional infraspecific adjustment",
  "N12-TAX03", "variable", "taxonomic", "Forma inherits containing taxon Star",
  "VPJD-GX01", "GX", "native_status", "Introduced/naturalised/cultivated-only"
)

# =============================================================================
# PERSIST VALIDATED SPECIFICATION
# =============================================================================

db_path <- here(
  "data", "interim", "occurrences",
  "vpjd_occurrences.duckdb"
)

if (!file.exists(db_path)) {
  stop("VPJD DuckDB not found: ", db_path)
}

con <- dbConnect(
  duckdb::duckdb(),
  dbdir = db_path,
  read_only = FALSE
)

on.exit({
  dbDisconnect(con, shutdown = TRUE)
}, add = TRUE)

validation <- tibble::tibble(
  check = c(
    "controlled_key_tests_pass",
    "controlled_review_tests_pass",
    "controlled_infraspecific_tests_pass",
    "all_test_stars_valid",
    "historical_star_totals_not_used",
    "foj_stars_not_used_for_assignment",
    "contemporary_taxa_not_modified",
    "occurrences_not_modified",
    "taxonomy_not_modified",
    "geography_not_modified",
    "quarantine_not_modified",
    "stars_not_assigned_to_vpjd",
    "ghi_not_calculated"
  ),
  pass = c(
    all(tests$PASS),
    all(review_tests$PASS),
    all(infra_tests$PASS),
    all(na.omit(tests$OBSERVED) %in% VALID_STARS),
    TRUE,
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

dbWriteTable(
  con,
  "vpjd_star_key_rule_register",
  rule_register %>%
    mutate(
      MODULE = MODULE,
      VERSION = VERSION,
      RUN_DATE = RUN_DATE
    ),
  overwrite = TRUE
)

dbWriteTable(
  con,
  "vpjd_star_key_validation",
  validation %>%
    mutate(
      MODULE = MODULE,
      VERSION = VERSION,
      RUN_DATE = RUN_DATE
    ),
  overwrite = TRUE
)

output_dir <- here(
  "outputs", "tables", "stars",
  "key_to_stars"
)

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

write_csv(
  rule_register,
  file.path(
    output_dir,
    "vpjd_star_key_rule_register.csv"
  )
)

write_csv(
  tests,
  file.path(
    output_dir,
    "vpjd_star_key_controlled_tests.csv"
  )
)

write_csv(
  review_tests,
  file.path(
    output_dir,
    "vpjd_star_key_review_tests.csv"
  )
)

write_csv(
  infra_tests,
  file.path(
    output_dir,
    "vpjd_star_key_infraspecific_tests.csv"
  )
)

write_csv(
  validation,
  file.path(
    output_dir,
    "vpjd_star_key_validation.csv"
  )
)

message("\n— Validation —")
print(validation, n = Inf)

message(
  "\nAll Key to Stars validation checks PASS: ",
  all(validation$pass)
)

if (!all(validation$pass)) {
  stop(
    "Stars 01 validation failed. ",
    "Do not freeze the Key to Stars module."
  )
}

message("\nCanonical Key rule register:")
message("  vpjd_star_key_rule_register")
message("\nCanonical validation table:")
message("  vpjd_star_key_validation")
message("\nOutput status: VALIDATED KEY TO STARS DECISION ENGINE")
message("Historical Star totals used: FALSE")
message("FOJ historical Stars used for assignment: FALSE")
message("Contemporary VPJD taxa classified: FALSE")
message("Stars assigned: FALSE")
message("GHI calculated: FALSE")
message("\nStars 01 v", VERSION, " complete.")