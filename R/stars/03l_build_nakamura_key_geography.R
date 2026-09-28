# ==============================================================================
# VPJD-OJPCP
# Stars 03l — Build Nakamura Key geography
# Version: 0.1.0
#
# PURPOSE
# Formalise the Japanese district geography required by Nakamura's Key to Stars
# and calculate contemporary district occupancy for accepted VPJD taxa.
#
# METHODOLOGICAL BASIS
# Nakamura thesis, Chapter 2, Figure 2.4:
# - Japanese "districts" are broad geographic regions, distinct from botanical
#   prefectures.
# - The 14 Hokkaido subprefectures are treated as prefectures in the Key.
# - Nakamura used 50 botanical prefectures.
# - Ryukyu, Izu, Ogasawara (including Kazan) and Kuriles are explicit botanical
#   prefectures requiring separate treatment.
#
# IMPORTANT
# - NO Stars are assigned.
# - NO rarity criterion is implemented.
# - NO "almost all districts" threshold is implemented.
# - Special-island district membership is NOT inferred in v0.1.0.
# - Contemporary JP49 Ogasawara and JP50 Kazan both correspond to Nakamura
#   botanical prefecture 49.
# - Output district counts are MINIMUM CONFIRMED counts until treatment of the
#   special island groups is methodologically resolved.
# ==============================================================================

library(here)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(readr)

MODULE <- "stars_03l_build_nakamura_key_geography"
VERSION <- "0.1.0"
RUN_DATE <- Sys.Date()

out_dir <- here("outputs", "tables", "stars", "geography")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

cat("\n— Build Nakamura Key geography —\n\n")
cat("Run date:", as.character(RUN_DATE), "\n")
cat("Module:", MODULE, "\n")
cat("Version:", VERSION, "\n\n")

# ------------------------------------------------------------------------------
# 1. Locate canonical contemporary distribution inputs
# ------------------------------------------------------------------------------

find_first_existing <- function(paths) {
  hit <- paths[file.exists(paths)]
  if (length(hit) == 0) return(NA_character_)
  hit[[1]]
}

area_distribution_path <- find_first_existing(c(
  here("data", "derived", "distribution", "vpjd_japan_taxon_area_distribution.rds"),
  here("outputs", "tables", "distribution", "vpjd_japan_taxon_area_distribution.rds"),
  here("data", "processed", "distribution", "vpjd_japan_taxon_area_distribution.rds"),
  here("data", "derived", "vpjd_japan_taxon_area_distribution.rds")
))

taxon_distribution_path <- find_first_existing(c(
  here("data", "derived", "distribution", "vpjd_japan_taxon_distribution.rds"),
  here("outputs", "tables", "distribution", "vpjd_japan_taxon_distribution.rds"),
  here("data", "processed", "distribution", "vpjd_japan_taxon_distribution.rds"),
  here("data", "derived", "vpjd_japan_taxon_distribution.rds")
))

star_geography_path <- find_first_existing(c(
  here("data", "derived", "stars", "vpjd_star_key_japan_geography.rds"),
  here("outputs", "tables", "stars", "vpjd_star_key_japan_geography.rds"),
  here("outputs", "tables", "stars", "geography", "vpjd_star_key_japan_geography.rds")
))

cat("Input discovery:\n")
cat(" Area distribution:", area_distribution_path, "\n")
cat(" Taxon distribution:", taxon_distribution_path, "\n")
cat(" Existing Star geography:", star_geography_path, "\n\n")

if (is.na(area_distribution_path)) {
  stop(
    "Cannot locate vpjd_japan_taxon_area_distribution.rds. ",
    "03l requires the canonical contemporary taxon-area distribution table."
  )
}

area_dist <- readRDS(area_distribution_path)

if (!is.data.frame(area_dist)) {
  stop("Canonical taxon-area distribution object is not a data frame.")
}

# ------------------------------------------------------------------------------
# 2. Identify taxon identifier
# ------------------------------------------------------------------------------

candidate_taxon_fields <- c(
  "WCVP_ID",
  "wcvp_id",
  "WCVP_TAXON_ID",
  "wcvp_taxon_id",
  "TAXON_ID",
  "taxon_id"
)

taxon_id_field <- candidate_taxon_fields[
  candidate_taxon_fields %in% names(area_dist)
][1]

if (is.na(taxon_id_field)) {
  stop(
    "Unable to identify WCVP/taxon identifier in ",
    "vpjd_japan_taxon_area_distribution."
  )
}

cat("Taxon identifier:", taxon_id_field, "\n")

if (!"BOTANICAL_AREA_ID" %in% names(area_dist)) {
  stop(
    "BOTANICAL_AREA_ID not found in canonical taxon-area distribution table."
  )
}

area_dist <- area_dist %>%
  mutate(
    TAXON_ID_03L = as.character(.data[[taxon_id_field]]),
    BOTANICAL_AREA_ID = as.character(BOTANICAL_AREA_ID)
  )

# ------------------------------------------------------------------------------
# 3. Nakamura botanical-prefecture concordance
# ------------------------------------------------------------------------------

# Nakamura Figure 2.4 uses 50 botanical prefectures.
#
# Contemporary VPJD uses 51 botanical areas:
# JP01-JP46 = political prefectures
# JP47      = Ryukyu
# JP48      = Izu
# JP49      = Ogasawara
# JP50      = Kazan
# JP51      = Kuriles
#
# Nakamura combines Ogasawara and Kazan as botanical prefecture 49.
# Therefore JP49 and JP50 deliberately map to the same Nakamura prefecture.

nakamura_prefecture_lookup <- tibble(
  BOTANICAL_AREA_ID = sprintf("JP%02d", 1:51),
  NAKAMURA_PREFECTURE_ID = c(
    1:49,
    49,
    50
  )
)

# Names of Nakamura's 50 botanical prefectures.
nakamura_prefecture_names <- tibble(
  NAKAMURA_PREFECTURE_ID = 1:50,
  NAKAMURA_PREFECTURE_NAME = c(
    "Hokkaido",
    "Aomori",
    "Iwate",
    "Miyagi",
    "Akita",
    "Yamagata",
    "Fukushima",
    "Ibaraki",
    "Tochigi",
    "Gunma",
    "Saitama",
    "Chiba",
    "Tokyo",
    "Kanagawa",
    "Niigata",
    "Toyama",
    "Ishikawa",
    "Fukui",
    "Yamanashi",
    "Nagano",
    "Gifu",
    "Shizuoka",
    "Aichi",
    "Mie",
    "Shiga",
    "Kyoto",
    "Osaka",
    "Hyogo",
    "Nara",
    "Wakayama",
    "Tottori",
    "Shimane",
    "Okayama",
    "Hiroshima",
    "Yamaguchi",
    "Tokushima",
    "Kagawa",
    "Ehime",
    "Kochi",
    "Fukuoka",
    "Saga",
    "Nagasaki",
    "Kumamoto",
    "Oita",
    "Miyazaki",
    "Kagoshima",
    "Ryukyu Islands",
    "Izu Islands",
    "Ogasawara Islands incl. Kazan Islands",
    "Kuriles"
  )
)

nakamura_prefecture_lookup <- nakamura_prefecture_lookup %>%
  left_join(
    nakamura_prefecture_names,
    by = "NAKAMURA_PREFECTURE_ID"
  )

# ------------------------------------------------------------------------------
# 4. Formalise unequivocal Nakamura district geography
# ------------------------------------------------------------------------------

# Mainland/main-island district interpretation represented by Figure 2.4.
#
# Special botanical prefectures 47-50 are deliberately NOT forced into these
# districts in v0.1.0.

nakamura_main_district_lookup <- tibble(
  NAKAMURA_PREFECTURE_ID = 1:46,
  NAKAMURA_DISTRICT = case_when(
    NAKAMURA_PREFECTURE_ID == 1 ~ "Hokkaido",
    between(NAKAMURA_PREFECTURE_ID, 2, 7) ~ "Tohoku",
    between(NAKAMURA_PREFECTURE_ID, 8, 14) ~ "Kanto",
    between(NAKAMURA_PREFECTURE_ID, 15, 23) ~ "Chubu",
    between(NAKAMURA_PREFECTURE_ID, 24, 30) ~ "Kinki",
    between(NAKAMURA_PREFECTURE_ID, 31, 35) ~ "Chugoku",
    between(NAKAMURA_PREFECTURE_ID, 36, 39) ~ "Shikoku",
    between(NAKAMURA_PREFECTURE_ID, 40, 46) ~ "Kyushu",
    TRUE ~ NA_character_
  )
)

special_island_lookup <- tibble(
  NAKAMURA_PREFECTURE_ID = 47:50,
  SPECIAL_ISLAND_GROUP = c(
    "RYUKYU",
    "IZU",
    "OGASAWARA_KAZAN",
    "KURILES"
  ),
  DISTRICT_TREATMENT_03L = "UNRESOLVED"
)

nakamura_geography_lookup <- nakamura_prefecture_lookup %>%
  left_join(
    nakamura_main_district_lookup,
    by = "NAKAMURA_PREFECTURE_ID"
  ) %>%
  left_join(
    special_island_lookup,
    by = "NAKAMURA_PREFECTURE_ID"
  ) %>%
  mutate(
    GEOGRAPHY_CLASS = case_when(
      !is.na(NAKAMURA_DISTRICT) ~ "CONFIRMED_DISTRICT",
      !is.na(SPECIAL_ISLAND_GROUP) ~ "SPECIAL_ISLAND_UNRESOLVED",
      TRUE ~ "UNRESOLVED"
    ),
    DISTRICT_METHOD_STATUS = case_when(
      GEOGRAPHY_CLASS == "CONFIRMED_DISTRICT" ~ "METHOD_VALIDATED",
      GEOGRAPHY_CLASS == "SPECIAL_ISLAND_UNRESOLVED" ~ "UNRESOLVED",
      TRUE ~ "UNRESOLVED"
    )
  )

# ------------------------------------------------------------------------------
# 5. Validate geographic lookup itself
# ------------------------------------------------------------------------------

lookup_validation <- tibble(
  CHECK = c(
    "51 contemporary VPJD botanical areas represented",
    "50 Nakamura botanical prefectures represented",
    "JP49 Ogasawara maps to Nakamura prefecture 49",
    "JP50 Kazan maps to Nakamura prefecture 49",
    "JP51 Kuriles maps to Nakamura prefecture 50",
    "46 standard prefectures have confirmed district",
    "8 confirmed Nakamura districts represented",
    "4 special island botanical prefectures remain unresolved",
    "No contemporary botanical area maps to multiple Nakamura prefectures"
  ),
  PASS = c(
    n_distinct(nakamura_geography_lookup$BOTANICAL_AREA_ID) == 51,
    n_distinct(nakamura_geography_lookup$NAKAMURA_PREFECTURE_ID) == 50,
    identical(
      nakamura_geography_lookup %>%
        filter(BOTANICAL_AREA_ID == "JP49") %>%
        pull(NAKAMURA_PREFECTURE_ID),
      49L
    ),
    identical(
      nakamura_geography_lookup %>%
        filter(BOTANICAL_AREA_ID == "JP50") %>%
        pull(NAKAMURA_PREFECTURE_ID),
      49L
    ),
    identical(
      nakamura_geography_lookup %>%
        filter(BOTANICAL_AREA_ID == "JP51") %>%
        pull(NAKAMURA_PREFECTURE_ID),
      50L
    ),
    sum(
      !is.na(nakamura_geography_lookup$NAKAMURA_DISTRICT)
    ) == 46,
    n_distinct(
      nakamura_geography_lookup$NAKAMURA_DISTRICT,
      na.rm = TRUE
    ) == 8,
    n_distinct(
      nakamura_geography_lookup$NAKAMURA_PREFECTURE_ID[
        nakamura_geography_lookup$GEOGRAPHY_CLASS ==
          "SPECIAL_ISLAND_UNRESOLVED"
      ]
    ) == 4,
    nrow(nakamura_geography_lookup) ==
      n_distinct(nakamura_geography_lookup$BOTANICAL_AREA_ID)
  )
)

cat("\n— Geography lookup validation —\n")
print(lookup_validation, n = Inf)

if (!all(lookup_validation$PASS)) {
  stop("03l geography lookup validation failed.")
}

# ------------------------------------------------------------------------------
# 6. Join contemporary taxon-area evidence to Nakamura geography
# ------------------------------------------------------------------------------

taxon_area_nakamura <- area_dist %>%
  select(
    TAXON_ID_03L,
    BOTANICAL_AREA_ID,
    everything()
  ) %>%
  left_join(
    nakamura_geography_lookup,
    by = "BOTANICAL_AREA_ID"
  )

unmapped_areas <- taxon_area_nakamura %>%
  filter(
    is.na(NAKAMURA_PREFECTURE_ID)
  ) %>%
  distinct(
    BOTANICAL_AREA_ID
  )

if (nrow(unmapped_areas) > 0) {
  stop(
    "Contemporary botanical areas remain unmapped: ",
    paste(
      unmapped_areas$BOTANICAL_AREA_ID,
      collapse = ", "
    )
  )
}

# ------------------------------------------------------------------------------
# 7. Collapse JP49 + JP50 to Nakamura botanical prefecture 49
# ------------------------------------------------------------------------------

taxon_nakamura_prefecture <- taxon_area_nakamura %>%
  distinct(
    TAXON_ID_03L,
    NAKAMURA_PREFECTURE_ID,
    NAKAMURA_PREFECTURE_NAME,
    NAKAMURA_DISTRICT,
    SPECIAL_ISLAND_GROUP,
    GEOGRAPHY_CLASS
  )

# ------------------------------------------------------------------------------
# 8. Calculate confirmed district occupancy
# ------------------------------------------------------------------------------

taxon_confirmed_districts <- taxon_nakamura_prefecture %>%
  filter(
    !is.na(NAKAMURA_DISTRICT)
  ) %>%
  distinct(
    TAXON_ID_03L,
    NAKAMURA_DISTRICT
  )

taxon_district_summary <- taxon_confirmed_districts %>%
  group_by(
    TAXON_ID_03L
  ) %>%
  summarise(
    JAPAN_DISTRICT_COUNT_MIN = n_distinct(
      NAKAMURA_DISTRICT
    ),
    JAPAN_DISTRICTS_CONFIRMED = paste(
      sort(
        unique(
          NAKAMURA_DISTRICT
        )
      ),
      collapse = "; "
    ),
    .groups = "drop"
  )

# ------------------------------------------------------------------------------
# 9. Profile special-island occupancy
# ------------------------------------------------------------------------------

taxon_special_islands <- taxon_nakamura_prefecture %>%
  filter(
    !is.na(SPECIAL_ISLAND_GROUP)
  ) %>%
  mutate(
    PRESENT = TRUE
  ) %>%
  select(
    TAXON_ID_03L,
    SPECIAL_ISLAND_GROUP,
    PRESENT
  ) %>%
  distinct() %>%
  pivot_wider(
    names_from = SPECIAL_ISLAND_GROUP,
    values_from = PRESENT,
    values_fill = FALSE,
    names_prefix = "PRESENT_"
  )

required_special_fields <- c(
  "PRESENT_RYUKYU",
  "PRESENT_IZU",
  "PRESENT_OGASAWARA_KAZAN",
  "PRESENT_KURILES"
)

for (nm in required_special_fields) {
  if (!nm %in% names(taxon_special_islands)) {
    taxon_special_islands[[nm]] <- FALSE
  }
}

taxon_special_islands <- taxon_special_islands %>%
  mutate(
    SPECIAL_ISLAND_GROUP_COUNT =
      as.integer(PRESENT_RYUKYU) +
      as.integer(PRESENT_IZU) +
      as.integer(PRESENT_OGASAWARA_KAZAN) +
      as.integer(PRESENT_KURILES),
    SPECIAL_ISLAND_DISTRICT_REVIEW =
      SPECIAL_ISLAND_GROUP_COUNT > 0
  )

# ------------------------------------------------------------------------------
# 10. Establish complete contemporary taxon population
# ------------------------------------------------------------------------------

all_taxa <- area_dist %>%
  distinct(
    TAXON_ID_03L
  )

if (!is.na(star_geography_path)) {
  star_geo <- readRDS(star_geography_path)
  
  star_taxon_field <- candidate_taxon_fields[
    candidate_taxon_fields %in% names(star_geo)
  ][1]
  
  if (!is.na(star_taxon_field)) {
    star_taxa <- star_geo %>%
      transmute(
        TAXON_ID_03L = as.character(
          .data[[star_taxon_field]]
        )
      ) %>%
      distinct()
    
    if (nrow(star_taxa) == 11439) {
      all_taxa <- star_taxa
    }
  }
}

cat(
  "\nTaxa in 03l population:",
  format(nrow(all_taxa), big.mark = ","),
  "\n"
)

# ------------------------------------------------------------------------------
# 11. Build taxon-level Nakamura geography table
# ------------------------------------------------------------------------------

vpjd_star_nakamura_geography <- all_taxa %>%
  left_join(
    taxon_district_summary,
    by = "TAXON_ID_03L"
  ) %>%
  left_join(
    taxon_special_islands,
    by = "TAXON_ID_03L"
  ) %>%
  mutate(
    JAPAN_DISTRICT_COUNT_MIN = coalesce(
      JAPAN_DISTRICT_COUNT_MIN,
      0L
    ),
    JAPAN_DISTRICTS_CONFIRMED = coalesce(
      JAPAN_DISTRICTS_CONFIRMED,
      ""
    ),
    across(
      all_of(required_special_fields),
      ~ coalesce(.x, FALSE)
    ),
    SPECIAL_ISLAND_GROUP_COUNT = coalesce(
      SPECIAL_ISLAND_GROUP_COUNT,
      0L
    ),
    SPECIAL_ISLAND_DISTRICT_REVIEW = coalesce(
      SPECIAL_ISLAND_DISTRICT_REVIEW,
      FALSE
    ),
    JAPAN_DISTRICT_COUNT_STATUS = case_when(
      SPECIAL_ISLAND_DISTRICT_REVIEW ~
        "MINIMUM_CONFIRMED_SPECIAL_ISLAND_REVIEW",
      TRUE ~
        "CONFIRMED_FROM_MAIN_DISTRICTS"
    ),
    JAPAN_DISTRICT_COUNT_FINAL = if_else(
      SPECIAL_ISLAND_DISTRICT_REVIEW,
      NA_integer_,
      JAPAN_DISTRICT_COUNT_MIN
    ),
    DISTRICT_LE_1_CONFIRMED = case_when(
      SPECIAL_ISLAND_DISTRICT_REVIEW ~ NA,
      TRUE ~ JAPAN_DISTRICT_COUNT_MIN <= 1
    ),
    DISTRICT_LE_2_CONFIRMED = case_when(
      SPECIAL_ISLAND_DISTRICT_REVIEW ~ NA,
      TRUE ~ JAPAN_DISTRICT_COUNT_MIN <= 2
    ),
    DISTRICT_LE_3_CONFIRMED = case_when(
      SPECIAL_ISLAND_DISTRICT_REVIEW ~ NA,
      TRUE ~ JAPAN_DISTRICT_COUNT_MIN <= 3
    ),
    DISTRICT_GT_2_CONFIRMED = case_when(
      SPECIAL_ISLAND_DISTRICT_REVIEW ~ NA,
      TRUE ~ JAPAN_DISTRICT_COUNT_MIN > 2
    ),
    DISTRICT_GT_3_CONFIRMED = case_when(
      SPECIAL_ISLAND_DISTRICT_REVIEW ~ NA,
      TRUE ~ JAPAN_DISTRICT_COUNT_MIN > 3
    )
  )

# ------------------------------------------------------------------------------
# 12. District occupancy profile
# ------------------------------------------------------------------------------

district_occupancy <- taxon_confirmed_districts %>%
  count(
    NAKAMURA_DISTRICT,
    name = "N_TAXA"
  ) %>%
  arrange(
    desc(N_TAXA)
  )

district_count_profile <- vpjd_star_nakamura_geography %>%
  count(
    JAPAN_DISTRICT_COUNT_MIN,
    SPECIAL_ISLAND_DISTRICT_REVIEW,
    name = "N_TAXA"
  ) %>%
  arrange(
    JAPAN_DISTRICT_COUNT_MIN,
    SPECIAL_ISLAND_DISTRICT_REVIEW
  )

special_island_profile <- vpjd_star_nakamura_geography %>%
  summarise(
    N_TAXA = n(),
    RYUKYU = sum(PRESENT_RYUKYU),
    IZU = sum(PRESENT_IZU),
    OGASAWARA_KAZAN = sum(PRESENT_OGASAWARA_KAZAN),
    KURILES = sum(PRESENT_KURILES),
    ANY_SPECIAL_ISLAND = sum(
      SPECIAL_ISLAND_DISTRICT_REVIEW
    ),
    NO_SPECIAL_ISLAND = sum(
      !SPECIAL_ISLAND_DISTRICT_REVIEW
    )
  )

# ------------------------------------------------------------------------------
# 13. Methodological status
# ------------------------------------------------------------------------------

methodological_status <- tibble(
  ITEM = c(
    "DISTRICT_CONCEPT",
    "MAIN_DISTRICT_MEMBERSHIP",
    "HOKKAIDO_SUBPREFECTURES_AS_PREFECTURES",
    "NAKAMURA_50_PREFECTURE_SYSTEM",
    "OGASAWARA_KAZAN_COLLAPSE",
    "SPECIAL_ISLAND_DISTRICT_MEMBERSHIP",
    "DISTRICT_LE_1",
    "DISTRICT_LE_2",
    "DISTRICT_LE_3",
    "DISTRICT_GT_2",
    "DISTRICT_GT_3",
    "ALMOST_ALL_DISTRICTS",
    "RARE"
  ),
  STATUS = c(
    "METHOD_VALIDATED",
    "METHOD_VALIDATED",
    "METHOD_VALIDATED",
    "METHOD_VALIDATED",
    "METHOD_VALIDATED",
    "UNRESOLVED",
    "PARTIALLY_OPERATIONAL",
    "PARTIALLY_OPERATIONAL",
    "PARTIALLY_OPERATIONAL",
    "PARTIALLY_OPERATIONAL",
    "PARTIALLY_OPERATIONAL",
    "UNRESOLVED",
    "UNRESOLVED"
  ),
  NOTE = c(
    "Nakamura Figure 2.4 explicitly distinguishes Japanese districts from botanical prefectures.",
    "Eight unequivocal broad Japanese districts formalised from Figure 2.4.",
    "Nakamura explicitly treats the 14 Hokkaido subprefectures as prefectures in the Key.",
    "Nakamura Figure 2.4 uses 50 botanical prefectures.",
    "Nakamura botanical prefecture 49 combines Ogasawara and Kazan; contemporary JP49 and JP50 therefore collapse to 49.",
    "Ryukyu, Izu, Ogasawara/Kazan and Kuriles are retained separately pending explicit district treatment.",
    "Operational only where special-island district treatment cannot affect the count.",
    "Operational only where special-island district treatment cannot affect the count.",
    "Operational only where special-island district treatment cannot affect the count.",
    "Operational only where special-island district treatment cannot affect the count.",
    "Operational only where special-island district treatment cannot affect the count.",
    "Nakamura uses the phrase 'almost all districts' without a numeric threshold in the extracted Key text.",
    "Nakamura gives qualitative examples of rarity but no reproducible numerical rule has yet been adopted."
  )
)

# ------------------------------------------------------------------------------
# 14. Validation
# ------------------------------------------------------------------------------

validation <- tibble(
  CHECK = c(
    "Canonical taxon-area distribution loaded",
    "All source botanical areas map to Nakamura geography",
    "51 contemporary areas represented in lookup",
    "50 Nakamura botanical prefectures represented",
    "8 confirmed districts represented",
    "No Star field created",
    "No almost-all-district criterion implemented",
    "No rarity criterion implemented",
    "District counts never exceed 8",
    "Final district count withheld for special-island review taxa",
    "Taxon-level output has one row per taxon",
    "Expected accepted Star population retained if available"
  ),
  PASS = c(
    nrow(area_dist) > 0,
    nrow(unmapped_areas) == 0,
    n_distinct(
      nakamura_geography_lookup$BOTANICAL_AREA_ID
    ) == 51,
    n_distinct(
      nakamura_geography_lookup$NAKAMURA_PREFECTURE_ID
    ) == 50,
    n_distinct(
      nakamura_geography_lookup$NAKAMURA_DISTRICT,
      na.rm = TRUE
    ) == 8,
    !any(
      c(
        "STAR",
        "STAR_CLASS",
        "FINAL_STAR"
      ) %in%
        names(vpjd_star_nakamura_geography)
    ),
    !"ALMOST_ALL_DISTRICTS" %in%
      names(vpjd_star_nakamura_geography),
    !"RARE" %in%
      names(vpjd_star_nakamura_geography),
    max(
      vpjd_star_nakamura_geography$JAPAN_DISTRICT_COUNT_MIN,
      na.rm = TRUE
    ) <= 8,
    all(
      is.na(
        vpjd_star_nakamura_geography$
          JAPAN_DISTRICT_COUNT_FINAL[
            vpjd_star_nakamura_geography$
              SPECIAL_ISLAND_DISTRICT_REVIEW
          ]
      )
    ),
    nrow(vpjd_star_nakamura_geography) ==
      n_distinct(
        vpjd_star_nakamura_geography$TAXON_ID_03L
      ),
    if (!is.na(star_geography_path)) {
      nrow(vpjd_star_nakamura_geography) == 11439
    } else {
      TRUE
    }
  )
)

cat("\n— Validation —\n")
print(validation, n = Inf)

if (!all(validation$PASS)) {
  stop("03l validation failure.")
}

# ------------------------------------------------------------------------------
# 15. Save canonical and diagnostic outputs
# ------------------------------------------------------------------------------

outputs <- list(
  vpjd_star_nakamura_geography = vpjd_star_nakamura_geography,
  vpjd_star_nakamura_geography_lookup = nakamura_geography_lookup,
  vpjd_star_nakamura_prefecture_lookup = nakamura_prefecture_lookup,
  vpjd_star_nakamura_district_occupancy = district_occupancy,
  vpjd_star_nakamura_district_count_profile = district_count_profile,
  vpjd_star_nakamura_special_island_profile = special_island_profile,
  vpjd_star_nakamura_methodological_status = methodological_status,
  vpjd_star_03l_validation = validation
)

walk2(
  outputs,
  names(outputs),
  function(x, nm) {
    saveRDS(
      x,
      file.path(
        out_dir,
        paste0(nm, ".rds")
      )
    )
    
    write_csv(
      x,
      file.path(
        out_dir,
        paste0(nm, ".csv")
      ),
      na = ""
    )
  }
)

metadata <- tibble(
  MODULE = MODULE,
  VERSION = VERSION,
  RUN_DATE = as.character(RUN_DATE),
  SOURCE = "Nakamura thesis Chapter 2, Figure 2.4",
  STARS_ASSIGNED = FALSE,
  RARITY_IMPLEMENTED = FALSE,
  ALMOST_ALL_DISTRICTS_IMPLEMENTED = FALSE,
  SPECIAL_ISLAND_DISTRICT_MEMBERSHIP_RESOLVED = FALSE,
  METHOD_STATUS = "PARTIALLY_METHOD_VALIDATED_GEOGRAPHY"
)

saveRDS(
  metadata,
  file.path(
    out_dir,
    "vpjd_star_03l_metadata.rds"
  )
)

write_csv(
  metadata,
  file.path(
    out_dir,
    "vpjd_star_03l_metadata.csv"
  ),
  na = ""
)

# ------------------------------------------------------------------------------
# 16. Console summary
# ------------------------------------------------------------------------------

cat("\n— Nakamura district occupancy —\n")
print(district_occupancy, n = Inf)

cat("\n— Minimum confirmed district-count profile —\n")
print(district_count_profile, n = Inf)

cat("\n— Special-island profile —\n")
print(special_island_profile, n = Inf)

cat("\n— Methodological status —\n")
print(methodological_status, n = Inf)

cat("\n— Summary —\n")
cat(
  "Taxa:",
  format(
    nrow(vpjd_star_nakamura_geography),
    big.mark = ","
  ),
  "\n"
)
cat(
  "Confirmed Nakamura districts:",
  n_distinct(
    nakamura_geography_lookup$NAKAMURA_DISTRICT,
    na.rm = TRUE
  ),
  "\n"
)
cat(
  "Taxa with special-island district review:",
  format(
    sum(
      vpjd_star_nakamura_geography$
        SPECIAL_ISLAND_DISTRICT_REVIEW
    ),
    big.mark = ","
  ),
  "\n"
)
cat(
  "Taxa with final district count currently available:",
  format(
    sum(
      !is.na(
        vpjd_star_nakamura_geography$
          JAPAN_DISTRICT_COUNT_FINAL
      )
    ),
    big.mark = ","
  ),
  "\n"
)
cat(
  "Taxa with final district count withheld:",
  format(
    sum(
      is.na(
        vpjd_star_nakamura_geography$
          JAPAN_DISTRICT_COUNT_FINAL
      )
    ),
    big.mark = ","
  ),
  "\n"
)

cat(
  "\nStars 03l v",
  VERSION,
  " complete.\n",
  sep = ""
)

cat(
  "GEOGRAPHY ONLY — no Stars assigned.\n"
)
cat(
  "Special-island district treatment, RARE and ALMOST_ALL_DISTRICTS remain unresolved.\n"
)