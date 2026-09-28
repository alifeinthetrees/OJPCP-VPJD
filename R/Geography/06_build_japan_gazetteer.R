# =============================================================================
# VPJD-OJPCP
# 06_build_japan_gazetteer.R
#
# Japan Botanical Geography Gazetteer
# Version 0.2.0
#
# Purpose
# -------
# Build a controlled geographical framework for the Vascular Plants of Japan
# Database (VPJD), initially based on the botanical geography used by
# Nakamura (2012) in developing the Keys to Stars for the Flora of Japan.
#
# The gazetteer is intended to:
#
#   1. preserve Nakamura's botanical geographical concepts;
#   2. separate botanical geography from modern administrative geography;
#   3. provide stable VPJD geographic identifiers;
#   4. support reproducible interpretation of Flora of Japan distributions;
#   5. provide geographic inputs to the Japanese Key to Stars;
#   6. support later crosswalking to modern GIS/geographic datasets;
#   7. distinguish geographic schemes used for Star assignment and mapping;
#   8. preserve geographic evidence and its original resolution.
#
# IMPORTANT
# ---------
# This script builds the controlled geography.
#
# It does NOT:
#   - parse Flora of Japan distribution strings;
#   - calculate Stars;
#   - calculate GHI;
#   - infer botanical district membership not yet validated;
#   - silently substitute modern administrative geography for Nakamura's
#     botanical geography.
#
# Raw source geography should always be retained separately.
# =============================================================================


# =============================================================================
# 0. PACKAGES
# =============================================================================

library(dplyr)
library(readr)
library(tibble)
library(stringr)
library(here)


# =============================================================================
# 1. OUTPUT DIRECTORIES
# =============================================================================

geo_dir <- here(
  "data",
  "processed",
  "geography"
)

dir.create(
  geo_dir,
  recursive = TRUE,
  showWarnings = FALSE
)


# =============================================================================
# 2. NAKAMURA BOTANICAL PREFECTURES
# =============================================================================
#
# Nakamura's Chapter 2 / Figure 2.4 geographic framework contains
# 50 numbered botanical prefectures:
#
#   - 46 mainland political prefectures
#   - Ryukyu Islands
#   - Izu Islands
#   - Ogasawara Islands
#   - Kuriles
#
# These are botanical concepts used for Star assessment and should not be
# assumed to be identical to modern administrative geography.
#
# Ogasawara in the Chapter 2 Key geography includes the Kazan Islands.
# =============================================================================

jp_botanical_units <- tribble(
  
  ~nakamura_no, ~name,                    ~unit_type,
  
  1, "Hokkaido",                         "botanical_prefecture",
  2, "Aomori",                           "botanical_prefecture",
  3, "Iwate",                            "botanical_prefecture",
  4, "Miyagi",                           "botanical_prefecture",
  5, "Akita",                            "botanical_prefecture",
  6, "Yamagata",                         "botanical_prefecture",
  7, "Fukushima",                        "botanical_prefecture",
  8, "Ibaraki",                          "botanical_prefecture",
  9, "Tochigi",                          "botanical_prefecture",
  10, "Gunma",                            "botanical_prefecture",
  11, "Saitama",                          "botanical_prefecture",
  12, "Chiba",                            "botanical_prefecture",
  13, "Tokyo",                            "botanical_prefecture",
  14, "Kanagawa",                         "botanical_prefecture",
  15, "Niigata",                          "botanical_prefecture",
  16, "Toyama",                           "botanical_prefecture",
  17, "Ishikawa",                         "botanical_prefecture",
  18, "Fukui",                            "botanical_prefecture",
  19, "Yamanashi",                        "botanical_prefecture",
  20, "Nagano",                           "botanical_prefecture",
  21, "Gifu",                             "botanical_prefecture",
  22, "Shizuoka",                         "botanical_prefecture",
  23, "Aichi",                            "botanical_prefecture",
  24, "Mie",                              "botanical_prefecture",
  25, "Shiga",                            "botanical_prefecture",
  26, "Kyoto",                            "botanical_prefecture",
  27, "Osaka",                            "botanical_prefecture",
  28, "Hyogo",                            "botanical_prefecture",
  29, "Nara",                             "botanical_prefecture",
  30, "Wakayama",                         "botanical_prefecture",
  31, "Tottori",                          "botanical_prefecture",
  32, "Shimane",                          "botanical_prefecture",
  33, "Okayama",                          "botanical_prefecture",
  34, "Hiroshima",                        "botanical_prefecture",
  35, "Yamaguchi",                        "botanical_prefecture",
  36, "Tokushima",                        "botanical_prefecture",
  37, "Kagawa",                           "botanical_prefecture",
  38, "Ehime",                            "botanical_prefecture",
  39, "Kochi",                            "botanical_prefecture",
  40, "Fukuoka",                          "botanical_prefecture",
  41, "Saga",                             "botanical_prefecture",
  42, "Nagasaki",                         "botanical_prefecture",
  43, "Kumamoto",                         "botanical_prefecture",
  44, "Oita",                             "botanical_prefecture",
  45, "Miyazaki",                         "botanical_prefecture",
  46, "Kagoshima",                        "botanical_prefecture",
  47, "Ryukyu Islands",                   "island_botanical_prefecture",
  48, "Izu Islands",                      "island_botanical_prefecture",
  49, "Ogasawara Islands",                "island_botanical_prefecture",
  50, "Kuriles",                          "island_botanical_prefecture"
  
) %>%
  
  mutate(
    
    geo_id = sprintf(
      "JP-BP-%02d",
      nakamura_no
    ),
    
    country_context = "Japan",
    
    geographic_system =
      "Nakamura_2012_botanical_prefecture",
    
    source =
      "Nakamura 2012",
    
    active =
      TRUE
    
  ) %>%
  
  select(
    geo_id,
    nakamura_no,
    name,
    unit_type,
    country_context,
    geographic_system,
    source,
    active
  )


# =============================================================================
# 3. BASIC VALIDATION
# =============================================================================

if (
  nrow(jp_botanical_units) != 50
) {
  
  stop(
    "Expected exactly 50 Nakamura botanical prefectures."
  )
}


if (
  anyDuplicated(
    jp_botanical_units$geo_id
  )
) {
  
  stop(
    "Duplicate VPJD geographic IDs detected."
  )
}


if (
  anyDuplicated(
    jp_botanical_units$nakamura_no
  )
) {
  
  stop(
    "Duplicate Nakamura botanical-prefecture numbers detected."
  )
}


if (
  anyDuplicated(
    jp_botanical_units$name
  )
) {
  
  stop(
    "Duplicate botanical-prefecture names detected."
  )
}


# =============================================================================
# 4. ISLAND-GROUP ATTRIBUTES
# =============================================================================
#
# These flags identify botanical island groups explicitly relevant to
# Nakamura's Japanese Keys to Stars.
#
# They are not intended to imply that all Japanese islands are geographically
# or biologically equivalent.
# =============================================================================

jp_botanical_units <- jp_botanical_units %>%
  
  mutate(
    
    island_botanical_prefecture =
      unit_type ==
      "island_botanical_prefecture",
    
    nakamura_small_island_group =
      name %in%
      c(
        "Ryukyu Islands",
        "Izu Islands",
        "Ogasawara Islands"
      ),
    
    kuriles =
      name ==
      "Kuriles"
    
  )


# =============================================================================
# 5. NAKAMURA GEOGRAPHIC SCHEMES
# =============================================================================
#
# Nakamura uses related but distinct geographic schemes.
#
# N12-KEY
# --------
# Chapter 2 / Figure 2.4 geography used to develop and apply the
# Key to Stars.
#
# N12-MAP
# --------
# Later national Bioquality mapping geography.
#
# The schemes must remain distinguishable because some island concepts,
# particularly Kazan, are treated differently.
# =============================================================================

geographic_schemes <- tribble(
  
  ~scheme_id,
  ~scheme_name,
  ~purpose,
  
  "N12-KEY",
  "Nakamura 2012 Key to Stars",
  "star_assignment",
  
  "N12-MAP",
  "Nakamura 2012 national mapping",
  "bioquality_mapping"
  
) %>%
  
  mutate(
    source = "Nakamura 2012",
    active = TRUE
  )


# =============================================================================
# 6. HOKKAIDO KEY UNITS
# =============================================================================
#
# Nakamura explicitly treats the 14 sub-prefectures of Hokkaido as
# prefectures when applying the Key to Stars.
#
# Hokkaido therefore exists at two relevant levels:
#
#   JP-BP-01 Hokkaido
#          |
#          +-- 14 Hokkaido Key units
#
# The 14 units below provide a controlled crosswalk for implementation.
# =============================================================================

hokkaido_key_units <- tribble(
  
  ~geo_id,       ~name,
  
  "JP-HK-01",    "Sorachi",
  "JP-HK-02",    "Ishikari",
  "JP-HK-03",    "Shiribeshi",
  "JP-HK-04",    "Iburi",
  "JP-HK-05",    "Hidaka",
  "JP-HK-06",    "Oshima",
  "JP-HK-07",    "Hiyama",
  "JP-HK-08",    "Kamikawa",
  "JP-HK-09",    "Rumoi",
  "JP-HK-10",    "Soya",
  "JP-HK-11",    "Okhotsk",
  "JP-HK-12",    "Tokachi",
  "JP-HK-13",    "Kushiro",
  "JP-HK-14",    "Nemuro"
  
) %>%
  
  mutate(
    
    unit_type =
      "hokkaido_key_prefecture",
    
    parent_geo_id =
      "JP-BP-01",
    
    key_prefecture_equivalent =
      TRUE,
    
    geographic_system =
      "Nakamura_2012_star_key",
    
    source =
      "Nakamura 2012 rule; names crosswalked to Hokkaido subprefectures",
    
    active =
      TRUE
    
  )


if (
  nrow(hokkaido_key_units) != 14
) {
  
  stop(
    "Nakamura Key requires exactly 14 Hokkaido sub-prefectural units."
  )
}


if (
  anyDuplicated(
    hokkaido_key_units$geo_id
  )
) {
  
  stop(
    "Duplicate Hokkaido Key geographic IDs detected."
  )
}


# =============================================================================
# 7. SPECIAL ISLAND GEOGRAPHY
# =============================================================================
#
# Some island concepts require treatment beyond ordinary prefectural
# geography.
#
# Important examples:
#
#   - Amami is administratively associated with Kagoshima but Nakamura's
#     botanical treatment places it within the Ryukyu botanical prefecture.
#
#   - Kazan is included within Ogasawara in the Chapter 2 Key geography,
#     but treated as a separate island-group unit in the Chapter 3 national
#     mapping analysis.
#
# Scheme-specific relationships are therefore preserved explicitly.
# =============================================================================

jp_special_geography <- tribble(
  
  ~geo_id,       ~name,                    ~unit_type,
  
  "JP-IG-RYU",   "Ryukyu Islands",         "island_group",
  "JP-IG-AMA",   "Amami Islands",          "island_group",
  "JP-IG-IZU",   "Izu Islands",            "island_group",
  "JP-IG-OGA",   "Ogasawara Islands",      "island_group",
  "JP-IG-KAZ",   "Kazan Islands",          "island_group",
  "JP-IG-KUR",   "Kuriles",                "island_group"
  
) %>%
  
  mutate(
    
    country_context =
      "Japan",
    
    source =
      "Nakamura 2012",
    
    active =
      TRUE
    
  )


# =============================================================================
# 8. SCHEME-SPECIFIC GEOGRAPHIC RELATIONSHIPS
# =============================================================================
#
# Geographic relationships can differ between analytical schemes.
#
# We therefore avoid forcing every relationship into a single hierarchy.
# =============================================================================

jp_scheme_relationships <- tribble(
  
  ~scheme_id,
  ~child_geo_id,
  ~parent_geo_id,
  ~relationship,
  ~source_note,
  
  # ---------------------------------------------------------------------------
  # Chapter 2 Key-to-Stars geography
  # ---------------------------------------------------------------------------
  
  "N12-KEY",
  "JP-IG-AMA",
  "JP-BP-47",
  "included_in",
  "Amami Islands treated within Ryukyu botanical prefecture",
  
  "N12-KEY",
  "JP-IG-KAZ",
  "JP-BP-49",
  "included_in",
  "Kazan Islands included within Ogasawara Islands in Figure 2.4",
  
  # ---------------------------------------------------------------------------
  # Chapter 3 national mapping geography
  # ---------------------------------------------------------------------------
  
  "N12-MAP",
  "JP-IG-AMA",
  "JP-IG-RYU",
  "included_in",
  "Amami included within Ryukyu botanical unit",
  
  "N12-MAP",
  "JP-IG-KAZ",
  NA_character_,
  "independent_sampling_unit",
  "Kazan Islands treated as a separate island-group mapping unit",
  
  "N12-MAP",
  "JP-IG-OGA",
  NA_character_,
  "independent_sampling_unit",
  "Ogasawara treated as a separate island-group mapping unit"
  
)


# =============================================================================
# 9. INITIAL GEOGRAPHIC ALIASES
# =============================================================================
#
# This is deliberately a small initial dictionary.
#
# It should be expanded from actual Flora of Japan and VPJD source strings,
# rather than populated speculatively.
#
# match_relationship:
#
#   exact       = alias represents the same geographic concept
#   broader     = alias is geographically broader than the target
#   narrower    = alias is geographically narrower than the target
#   ambiguous   = cannot be resolved automatically
#
# Only exact aliases should eventually be eligible for automatic
# substitution without additional evidence.
# =============================================================================

jp_geographic_aliases <- tribble(
  
  ~alias,
  ~geo_id,
  ~match_relationship,
  ~alias_source,
  
  "Ryukyu Is.",
  "JP-BP-47",
  "exact",
  "common_abbreviation",
  
  "Ryukyu Islands",
  "JP-BP-47",
  "exact",
  "canonical",
  
  "Ryukyus",
  "JP-BP-47",
  "exact",
  "common_variant",
  
  "Izu Is.",
  "JP-BP-48",
  "exact",
  "common_abbreviation",
  
  "Izu Islands",
  "JP-BP-48",
  "exact",
  "canonical",
  
  "Ogasawara Is.",
  "JP-BP-49",
  "exact",
  "common_abbreviation",
  
  "Ogasawara Islands",
  "JP-BP-49",
  "exact",
  "canonical",
  
  "Bonin Is.",
  "JP-BP-49",
  "exact",
  "historical_English",
  
  "Bonin Islands",
  "JP-BP-49",
  "exact",
  "historical_English",
  
  "Kuriles",
  "JP-BP-50",
  "exact",
  "canonical",
  
  "Kurile Islands",
  "JP-BP-50",
  "exact",
  "common_variant"
  
) %>%
  
  left_join(
    
    jp_botanical_units %>%
      select(
        geo_id,
        canonical_name = name
      ),
    
    by = "geo_id"
    
  )


# =============================================================================
# 10. VALIDATE ALIAS REFERENCES
# =============================================================================

unknown_alias_ids <- setdiff(
  jp_geographic_aliases$geo_id,
  jp_botanical_units$geo_id
)


if (
  length(unknown_alias_ids) > 0
) {
  
  stop(
    paste(
      "Alias table contains unknown geographic IDs:",
      paste(
        unknown_alias_ids,
        collapse = ", "
      )
    )
  )
}


if (
  anyDuplicated(
    jp_geographic_aliases$alias
  )
) {
  
  stop(
    "Duplicate aliases detected in initial geographic alias table."
  )
}


# =============================================================================
# 11. EAST-ASIAN REGIONS USED BY THE KEY TO STARS
# =============================================================================
#
# These are controlled geographic concepts required by the non-endemic
# component of Nakamura's Japanese Key.
#
# Detailed subdivisions will be added later, particularly:
#
#   - Taiwan distribution extent
#   - Korean distribution extent
#   - southern vs wider Kuriles/Sakhalin
#   - Chinese provinces
#
# =============================================================================

east_asia_star_regions <- tribble(
  
  ~geo_id,
  ~name,
  ~unit_type,
  ~key_region,
  
  "EA-TW",
  "Taiwan",
  "external_region",
  "Taiwan",
  
  "EA-KR",
  "Korea",
  "external_region",
  "Korea",
  
  "EA-KUR",
  "Kuriles",
  "external_region",
  "KurilesSakhalin",
  
  "EA-SAK",
  "Sakhalin",
  "external_region",
  "KurilesSakhalin",
  
  "EA-CN",
  "China",
  "external_region",
  "China"
  
) %>%
  
  mutate(
    
    geographic_system =
      "Nakamura_2012_star_key",
    
    source =
      "Nakamura 2012",
    
    active =
      TRUE
    
  )


# =============================================================================
# 12. BOTANICAL DISTRICTS
# =============================================================================
#
# District membership is essential to Nakamura's Key.
#
# Figure 2.4 defines the botanical districts spatially.
#
# We deliberately do NOT populate district -> prefecture relationships until
# those boundaries have been transcribed and validated against Nakamura's
# geographic framework.
#
# This prevents modern conventional Japanese regional divisions from being
# silently substituted for Nakamura's botanical districts.
# =============================================================================

jp_botanical_districts <- tibble(
  
  district_id =
    character(),
  
  district_name =
    character(),
  
  source =
    character(),
  
  validation_status =
    character()
  
)


jp_district_prefecture_crosswalk <- tibble(
  
  district_id =
    character(),
  
  geo_id =
    character(),
  
  relationship =
    character(),
  
  source =
    character(),
  
  validation_status =
    character()
  
)


# =============================================================================
# 13. GENERAL GEOGRAPHIC HIERARCHY
# =============================================================================
#
# This table will eventually contain validated hierarchical relationships:
#
#   Japan
#      |
#      +-- botanical districts
#              |
#              +-- botanical prefectures
#                       |
#                       +-- island groups / islands where appropriate
#
# Hokkaido and scheme-specific island relationships are already represented
# in dedicated tables above.
#
# District relationships remain intentionally unpopulated pending validation.
# =============================================================================

jp_geographic_hierarchy <- tibble(
  
  child_geo_id =
    character(),
  
  parent_geo_id =
    character(),
  
  relationship =
    character(),
  
  source =
    character(),
  
  validation_status =
    character()
  
)


# =============================================================================
# 14. GEOGRAPHIC EVIDENCE RULES
# =============================================================================
#
# These encode Nakamura's instructions for interpreting geographic evidence.
#
# They are methodological rules, not distribution observations.
# =============================================================================

geographic_evidence_rules <- tribble(
  
  ~rule_id,
  ~rule_type,
  ~rule_description,
  
  "N12-GEO-01",
  "hokkaido_subprefecture",
  paste(
    "Treat each of the 14 Hokkaido sub-prefectures",
    "as a prefecture when applying the Key to Stars."
  ),
  
  "N12-GEO-02",
  "district_expansion",
  paste(
    "Where distribution is stated only at district scale",
    "and no prefectures are specified, assign all prefectures",
    "within that district as a conservative approximation."
  ),
  
  "N12-GEO-03",
  "amami_assignment",
  paste(
    "Treat the Amami Islands as part of the Ryukyu botanical",
    "prefecture despite their administrative inclusion in Kagoshima."
  ),
  
  "N12-GEO-04",
  "kazan_key_assignment",
  paste(
    "For the Chapter 2 Key geography, Kazan Islands are included",
    "within the Ogasawara botanical prefecture."
  ),
  
  "N12-GEO-05",
  "preserve_resolution",
  paste(
    "Preserve the geographic resolution of the source assertion",
    "before deriving broader botanical units."
  )
  
) %>%
  
  mutate(
    source = "Nakamura 2012"
  )


# =============================================================================
# 15. VALIDATE CROSS-TABLE REFERENCES
# =============================================================================

valid_scheme_ids <-
  geographic_schemes$scheme_id


unknown_scheme_ids <-
  setdiff(
    jp_scheme_relationships$scheme_id,
    valid_scheme_ids
  )


if (
  length(unknown_scheme_ids) > 0
) {
  
  stop(
    paste(
      "Unknown geographic scheme IDs:",
      paste(
        unknown_scheme_ids,
        collapse = ", "
      )
    )
  )
}


all_known_japan_geo_ids <- unique(
  c(
    jp_botanical_units$geo_id,
    hokkaido_key_units$geo_id,
    jp_special_geography$geo_id
  )
)


unknown_child_ids <- setdiff(
  jp_scheme_relationships$child_geo_id,
  all_known_japan_geo_ids
)


if (
  length(unknown_child_ids) > 0
) {
  
  stop(
    paste(
      "Unknown child geographic IDs:",
      paste(
        unknown_child_ids,
        collapse = ", "
      )
    )
  )
}


relationship_parent_ids <-
  jp_scheme_relationships$parent_geo_id[
    !is.na(
      jp_scheme_relationships$parent_geo_id
    )
  ]


unknown_parent_ids <- setdiff(
  relationship_parent_ids,
  all_known_japan_geo_ids
)


if (
  length(unknown_parent_ids) > 0
) {
  
  stop(
    paste(
      "Unknown parent geographic IDs:",
      paste(
        unknown_parent_ids,
        collapse = ", "
      )
    )
  )
}


# =============================================================================
# 16. GAZETTEER METADATA
# =============================================================================

gazetteer_metadata <- tibble(
  
  dataset =
    c(
      "jp_botanical_units",
      "jp_geographic_aliases",
      "jp_geographic_hierarchy",
      "geographic_schemes",
      "hokkaido_key_units",
      "jp_special_geography",
      "jp_scheme_relationships",
      "jp_botanical_districts",
      "jp_district_prefecture_crosswalk",
      "geographic_evidence_rules",
      "east_asia_star_regions"
    ),
  
  version =
    "0.2.0",
  
  status =
    c(
      "initial_validated_structure",
      "initial",
      "awaiting_district_hierarchy",
      "initial",
      "initial",
      "initial",
      "initial",
      "awaiting_validated_transcription",
      "awaiting_validated_transcription",
      "initial",
      "initial"
    ),
  
  primary_reference =
    "Nakamura 2012",
  
  build_date =
    as.character(
      Sys.Date()
    ),
  
  notes =
    c(
      "Initial 50 botanical prefectures used by Nakamura.",
      "Initial conservative geographic alias dictionary.",
      "General hierarchy intentionally incomplete pending district validation.",
      "Distinguishes Key-to-Stars and national mapping schemes.",
      "Fourteen Hokkaido units treated as prefecture-equivalents for the Key.",
      "Special island concepts including Amami and Kazan.",
      "Scheme-specific island relationships.",
      "Botanical districts pending source-faithful transcription.",
      "District-to-prefecture relationships pending source-faithful transcription.",
      "Explicit Nakamura geographic evidence rules.",
      "External East Asian regions required by the non-endemic Star key."
    )
  
)


# =============================================================================
# 17. WRITE CSV OUTPUTS
# =============================================================================

write_csv(
  jp_botanical_units,
  file.path(
    geo_dir,
    "jp_botanical_units.csv"
  )
)


write_csv(
  jp_geographic_aliases,
  file.path(
    geo_dir,
    "jp_geographic_aliases.csv"
  )
)


write_csv(
  jp_geographic_hierarchy,
  file.path(
    geo_dir,
    "jp_geographic_hierarchy.csv"
  )
)


write_csv(
  geographic_schemes,
  file.path(
    geo_dir,
    "geographic_schemes.csv"
  )
)


write_csv(
  hokkaido_key_units,
  file.path(
    geo_dir,
    "hokkaido_key_units.csv"
  )
)


write_csv(
  jp_special_geography,
  file.path(
    geo_dir,
    "jp_special_geography.csv"
  )
)


write_csv(
  jp_scheme_relationships,
  file.path(
    geo_dir,
    "jp_scheme_relationships.csv"
  )
)


write_csv(
  jp_botanical_districts,
  file.path(
    geo_dir,
    "jp_botanical_districts.csv"
  )
)


write_csv(
  jp_district_prefecture_crosswalk,
  file.path(
    geo_dir,
    "jp_district_prefecture_crosswalk.csv"
  )
)


write_csv(
  geographic_evidence_rules,
  file.path(
    geo_dir,
    "geographic_evidence_rules.csv"
  )
)


write_csv(
  east_asia_star_regions,
  file.path(
    geo_dir,
    "east_asia_star_regions.csv"
  )
)


write_csv(
  gazetteer_metadata,
  file.path(
    geo_dir,
    "gazetteer_metadata.csv"
  )
)


# =============================================================================
# 18. SAVE COMPLETE R GAZETTEER OBJECT
# =============================================================================

japan_gazetteer <- list(
  
  botanical_units =
    jp_botanical_units,
  
  aliases =
    jp_geographic_aliases,
  
  hierarchy =
    jp_geographic_hierarchy,
  
  geographic_schemes =
    geographic_schemes,
  
  hokkaido_key_units =
    hokkaido_key_units,
  
  special_geography =
    jp_special_geography,
  
  scheme_relationships =
    jp_scheme_relationships,
  
  botanical_districts =
    jp_botanical_districts,
  
  district_prefecture_crosswalk =
    jp_district_prefecture_crosswalk,
  
  geographic_evidence_rules =
    geographic_evidence_rules,
  
  east_asia_regions =
    east_asia_star_regions,
  
  metadata =
    gazetteer_metadata
  
)


saveRDS(
  japan_gazetteer,
  file.path(
    geo_dir,
    "japan_gazetteer.rds"
  )
)


# =============================================================================
# 19. FINAL VALIDATION
# =============================================================================

if (
  !file.exists(
    file.path(
      geo_dir,
      "japan_gazetteer.rds"
    )
  )
) {
  
  stop(
    "Gazetteer RDS was not created successfully."
  )
}


required_outputs <- c(
  
  "jp_botanical_units.csv",
  "jp_geographic_aliases.csv",
  "jp_geographic_hierarchy.csv",
  "geographic_schemes.csv",
  "hokkaido_key_units.csv",
  "jp_special_geography.csv",
  "jp_scheme_relationships.csv",
  "jp_botanical_districts.csv",
  "jp_district_prefecture_crosswalk.csv",
  "geographic_evidence_rules.csv",
  "east_asia_star_regions.csv",
  "gazetteer_metadata.csv",
  "japan_gazetteer.rds"
  
)


missing_outputs <- required_outputs[
  !file.exists(
    file.path(
      geo_dir,
      required_outputs
    )
  )
]


if (
  length(missing_outputs) > 0
) {
  
  stop(
    paste(
      "Missing gazetteer outputs:",
      paste(
        missing_outputs,
        collapse = ", "
      )
    )
  )
}


# =============================================================================
# 20. BUILD SUMMARY
# =============================================================================

message(
  "Japan gazetteer build complete."
)

message(
  "Gazetteer version: 0.2.0"
)

message(
  "Botanical units: ",
  nrow(
    jp_botanical_units
  )
)

message(
  "Aliases: ",
  nrow(
    jp_geographic_aliases
  )
)

message(
  "Hokkaido Key units: ",
  nrow(
    hokkaido_key_units
  )
)

message(
  "Special island concepts: ",
  nrow(
    jp_special_geography
  )
)

message(
  "Geographic schemes: ",
  nrow(
    geographic_schemes
  )
)

message(
  "Scheme-specific relationships: ",
  nrow(
    jp_scheme_relationships
  )
)

message(
  "Geographic evidence rules: ",
  nrow(
    geographic_evidence_rules
  )
)

message(
  "East Asian Star-key regions: ",
  nrow(
    east_asia_star_regions
  )
)

message(
  "Botanical district crosswalk: pending validated transcription"
)

message(
  "Output directory: ",
  geo_dir
)