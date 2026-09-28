# ==============================================================================
# VPJD-OJPCP
# 02_build_botanical_area_framework.R
# Version: 0.1.0
#
# Purpose:
#   Build the canonical VPJD Japanese botanical-area framework.
#
# Basis:
#   Nakamura (2012):
#     - 46 political prefectures
#     - 5 additional botanical island groups:
#         Ryukyu
#         Izu
#         Ogasawara
#         Kazan
#         Kuriles
#
#   Canonical analytical geography = 51 botanical areas.
#
#   Nakamura Figure 2.4 presents 50 numbered units by including Kazan within
#   Ogasawara. This script therefore preserves:
#
#     1. 51-area analytical framework.
#     2. 50-area display/reproduction framework.
#
# Important:
#   - Amami Islands are assigned botanically to Ryukyu, not Kagoshima.
#   - Izu Islands are separate from Tokyo.
#   - Ogasawara Islands are separate from Tokyo.
#   - Kazan Islands are analytically separate, but roll up to Ogasawara in
#     the 50-area display framework.
#   - Kuriles are separate from Hokkaido.
#   - Hokkaido remains one botanical area here.
#   - Hokkaido's 14 subprefectures will be handled later in the separate
#     Star-counting geography required by Nakamura's Key to the Stars.
#
# This module does NOT:
#   - assign occurrences;
#   - create spatial polygons;
#   - infer island membership from coordinates;
#   - modify occurrence coordinates;
#   - modify taxonomy;
#   - modify frozen upstream tables.
#
# Required frozen input:
#   Geography 01 v0.1.0
# ==============================================================================

suppressPackageStartupMessages({
  library(DBI)
  library(duckdb)
  library(dplyr)
  library(tibble)
  library(readr)
  library(here)
})

SCRIPT_VERSION <- "0.1.0"
EXPECTED_GEOGRAPHY_01_VERSION <- "0.1.0"

get_metadata_value <- function(con, table_name, metric_name) {
  fields <- dbListFields(con, table_name)
  
  if (all(c("metric", "value") %in% fields)) {
    sql <- paste0(
      "SELECT CAST(value AS VARCHAR) AS value FROM ",
      dbQuoteIdentifier(con, table_name),
      " WHERE CAST(metric AS VARCHAR) = ?"
    )
    
    x <- dbGetQuery(
      con,
      sql,
      params = list(metric_name)
    )
    
    if (nrow(x) != 1) {
      stop(
        "Expected one '", metric_name,
        "' row in ", table_name,
        "; found ", nrow(x), "."
      )
    }
    
    return(as.character(x$value[[1]]))
  }
  
  if (metric_name %in% fields) {
    x <- dbReadTable(con, table_name)
    
    if (nrow(x) != 1) {
      stop("Expected one row in ", table_name, ".")
    }
    
    return(as.character(x[[metric_name]][[1]]))
  }
  
  stop(
    "Metadata item '", metric_name,
    "' not found in ", table_name, "."
  )
}

run_botanical_area_framework <- function() {
  
  cat("\n— VPJD Japanese botanical-area framework —\n\n")
  
  db_path <- here(
    "data", "interim", "occurrences",
    "vpjd_occurrences.duckdb"
  )
  
  if (!file.exists(db_path)) {
    stop("DuckDB not found: ", db_path)
  }
  
  con <- dbConnect(
    duckdb::duckdb(),
    dbdir = db_path,
    read_only = FALSE
  )
  
  on.exit(
    dbDisconnect(con, shutdown = TRUE),
    add = TRUE
  )
  
  if (
    !"japan_geography_profile_metadata" %in%
    dbListTables(con)
  ) {
    stop(
      "Required Geography 01 metadata table not found."
    )
  }
  
  geography_01_version <- get_metadata_value(
    con,
    "japan_geography_profile_metadata",
    "script_version"
  )
  
  if (
    geography_01_version !=
    EXPECTED_GEOGRAPHY_01_VERSION
  ) {
    stop(
      "Expected Geography 01 v",
      EXPECTED_GEOGRAPHY_01_VERSION,
      "; detected ",
      geography_01_version, "."
    )
  }
  
  cat(
    "Geography 01 version: ",
    geography_01_version, "\n\n",
    sep = ""
  )
  
  # ---------------------------------------------------------------------------
  # 46 political prefectures
  #
  # Districts follow the broad Japanese geographic divisions required later
  # for distribution summaries and the Key to the Stars.
  # ---------------------------------------------------------------------------
  
  political <- tribble(
    ~area_no, ~area_name, ~district,
    
    1L, "Hokkaido",   "Hokkaido",
    2L, "Aomori",     "Tohoku",
    3L, "Iwate",      "Tohoku",
    4L, "Miyagi",     "Tohoku",
    5L, "Akita",      "Tohoku",
    6L, "Yamagata",   "Tohoku",
    7L, "Fukushima",  "Tohoku",
    
    8L, "Ibaraki",    "Kanto",
    9L, "Tochigi",    "Kanto",
    10L, "Gunma",      "Kanto",
    11L, "Saitama",    "Kanto",
    12L, "Chiba",      "Kanto",
    13L, "Tokyo",      "Kanto",
    14L, "Kanagawa",   "Kanto",
    
    15L, "Niigata",    "Chubu",
    16L, "Toyama",     "Chubu",
    17L, "Ishikawa",   "Chubu",
    18L, "Fukui",      "Chubu",
    19L, "Yamanashi",  "Chubu",
    20L, "Nagano",     "Chubu",
    21L, "Gifu",       "Chubu",
    22L, "Shizuoka",   "Chubu",
    23L, "Aichi",      "Chubu",
    
    24L, "Mie",        "Kinki",
    25L, "Shiga",      "Kinki",
    26L, "Kyoto",      "Kinki",
    27L, "Osaka",      "Kinki",
    28L, "Hyogo",      "Kinki",
    29L, "Nara",       "Kinki",
    30L, "Wakayama",   "Kinki",
    
    31L, "Tottori",    "Chugoku",
    32L, "Shimane",    "Chugoku",
    33L, "Okayama",    "Chugoku",
    34L, "Hiroshima",  "Chugoku",
    35L, "Yamaguchi",  "Chugoku",
    
    36L, "Tokushima",  "Shikoku",
    37L, "Kagawa",     "Shikoku",
    38L, "Ehime",      "Shikoku",
    39L, "Kochi",      "Shikoku",
    
    40L, "Fukuoka",    "Kyushu",
    41L, "Saga",       "Kyushu",
    42L, "Nagasaki",   "Kyushu",
    43L, "Kumamoto",   "Kyushu",
    44L, "Oita",       "Kyushu",
    45L, "Miyazaki",   "Kyushu",
    46L, "Kagoshima",  "Kyushu"
  ) %>%
    mutate(
      area_id = sprintf("JP%02d", area_no),
      area_type = "political_prefecture",
      political_parent = area_name,
      analytical_area_name = area_name,
      display_area_name = area_name,
      display_area_no = area_no,
      display_area_id =
        sprintf("JP%02d", area_no),
      star_small_island_group = FALSE,
      special_assignment_required = FALSE,
      notes = NA_character_
    )
  
  # ---------------------------------------------------------------------------
  # Five additional botanical island groups
  # ---------------------------------------------------------------------------
  
  island_groups <- tribble(
    ~area_no,
    ~area_name,
    ~district,
    ~political_parent,
    ~display_area_no,
    ~display_area_name,
    ~star_small_island_group,
    ~notes,
    
    47L,
    "Ryukyu Islands",
    "Ryukyu",
    "Kagoshima / Okinawa",
    47L,
    "Ryukyu Islands",
    TRUE,
    paste(
      "Includes the Amami Islands despite their",
      "political administration within Kagoshima Prefecture."
    ),
    
    48L,
    "Izu Islands",
    "Izu",
    "Tokyo",
    48L,
    "Izu Islands",
    TRUE,
    paste(
      "Botanically separate from Tokyo political prefecture."
    ),
    
    49L,
    "Ogasawara Islands",
    "Ogasawara",
    "Tokyo",
    49L,
    "Ogasawara Islands",
    TRUE,
    paste(
      "Botanically separate from Tokyo political prefecture."
    ),
    
    50L,
    "Kazan Islands",
    "Kazan",
    "Tokyo",
    49L,
    "Ogasawara Islands",
    FALSE,
    paste(
      "Separate analytical unit in Nakamura's national",
      "hotspot dataset; included with Ogasawara in the",
      "50-area Figure 2.4 representation."
    ),
    
    51L,
    "Kuriles",
    "Kuriles",
    "Hokkaido / non-Japanese administration",
    50L,
    "Kuriles",
    FALSE,
    paste(
      "Botanical unit distinct from Hokkaido.",
      "Historical distribution terminology requires",
      "separate treatment."
    )
  ) %>%
    mutate(
      area_id = sprintf("JP%02d", area_no),
      area_type = "botanical_island_group",
      analytical_area_name = area_name,
      display_area_id =
        sprintf("JP%02d", display_area_no),
      special_assignment_required = TRUE
    )
  
  # ---------------------------------------------------------------------------
  # Combine canonical 51-area analytical framework
  # ---------------------------------------------------------------------------
  
  botanical_areas <- bind_rows(
    political,
    island_groups
  ) %>%
    select(
      area_id,
      area_no,
      analytical_area_name,
      area_type,
      district,
      political_parent,
      display_area_id,
      display_area_no,
      display_area_name,
      star_small_island_group,
      special_assignment_required,
      notes
    ) %>%
    arrange(area_no)
  
  # ---------------------------------------------------------------------------
  # Validation
  # ---------------------------------------------------------------------------
  
  if (nrow(botanical_areas) != 51) {
    stop(
      "Expected 51 analytical botanical areas; found ",
      nrow(botanical_areas), "."
    )
  }
  
  if (n_distinct(botanical_areas$area_id) != 51) {
    stop("Analytical area_id values are not unique.")
  }
  
  if (
    n_distinct(
      botanical_areas$analytical_area_name
    ) != 51
  ) {
    stop(
      "Analytical botanical-area names are not unique."
    )
  }
  
  if (
    n_distinct(
      botanical_areas$display_area_id
    ) != 50
  ) {
    stop(
      "Expected 50 display botanical areas; found ",
      n_distinct(botanical_areas$display_area_id), "."
    )
  }
  
  political_n <- sum(
    botanical_areas$area_type ==
      "political_prefecture"
  )
  
  island_n <- sum(
    botanical_areas$area_type ==
      "botanical_island_group"
  )
  
  if (political_n != 46) {
    stop(
      "Expected 46 political prefectures; found ",
      political_n, "."
    )
  }
  
  if (island_n != 5) {
    stop(
      "Expected 5 botanical island groups; found ",
      island_n, "."
    )
  }
  
  # ---------------------------------------------------------------------------
  # Build 50-area display framework
  # ---------------------------------------------------------------------------
  
  botanical_display_areas <- botanical_areas %>%
    group_by(
      display_area_id,
      display_area_no,
      display_area_name
    ) %>%
    summarise(
      analytical_area_count = n(),
      analytical_area_ids =
        paste(area_id, collapse = ";"),
      analytical_area_names =
        paste(
          analytical_area_name,
          collapse = ";"
        ),
      .groups = "drop"
    ) %>%
    arrange(display_area_no)
  
  if (nrow(botanical_display_areas) != 50) {
    stop(
      "Expected 50 display areas; found ",
      nrow(botanical_display_areas), "."
    )
  }
  
  # ---------------------------------------------------------------------------
  # Explicit special-area crosswalk
  #
  # This table describes the required interpretation.
  # It does NOT yet perform spatial assignment.
  # ---------------------------------------------------------------------------
  
  special_crosswalk <- tribble(
    ~evidence_term,
    ~target_area_id,
    ~target_area_name,
    ~political_context,
    ~rule_type,
    ~priority,
    ~notes,
    
    "Amami",
    "JP47",
    "Ryukyu Islands",
    "Kagoshima",
    "explicit_botanical_override",
    1L,
    "Amami is botanical Ryukyu, not Kagoshima.",
    
    "Amami Islands",
    "JP47",
    "Ryukyu Islands",
    "Kagoshima",
    "explicit_botanical_override",
    1L,
    "Amami is botanical Ryukyu, not Kagoshima.",
    
    "Amami-oshima",
    "JP47",
    "Ryukyu Islands",
    "Kagoshima",
    "explicit_botanical_override",
    1L,
    "Specific Amami locality.",
    
    "Ryukyu",
    "JP47",
    "Ryukyu Islands",
    "Kagoshima / Okinawa",
    "explicit_botanical_area",
    1L,
    "Ryukyu botanical area.",
    
    "Ryukyu Islands",
    "JP47",
    "Ryukyu Islands",
    "Kagoshima / Okinawa",
    "explicit_botanical_area",
    1L,
    "Ryukyu botanical area.",
    
    "Izu Islands",
    "JP48",
    "Izu Islands",
    "Tokyo",
    "explicit_botanical_override",
    1L,
    "Izu Islands are not assigned to Tokyo botanical area.",
    
    "Izu Shoto",
    "JP48",
    "Izu Islands",
    "Tokyo",
    "explicit_botanical_override",
    1L,
    "Izu island-group synonym.",
    
    "Ogasawara",
    "JP49",
    "Ogasawara Islands",
    "Tokyo",
    "explicit_botanical_override",
    1L,
    "Ogasawara is not assigned to Tokyo botanical area.",
    
    "Ogasawara Islands",
    "JP49",
    "Ogasawara Islands",
    "Tokyo",
    "explicit_botanical_override",
    1L,
    "Ogasawara botanical area.",
    
    "Bonin Islands",
    "JP49",
    "Ogasawara Islands",
    "Tokyo",
    "explicit_botanical_override",
    1L,
    "Historical English name for Ogasawara.",
    
    "Kazan Islands",
    "JP50",
    "Kazan Islands",
    "Tokyo",
    "explicit_botanical_override",
    1L,
    "Separate analytical area; display roll-up is Ogasawara.",
    
    "Volcano Islands",
    "JP50",
    "Kazan Islands",
    "Tokyo",
    "explicit_botanical_override",
    1L,
    "Historical English name for Kazan Islands.",
    
    "Kuriles",
    "JP51",
    "Kuriles",
    "Hokkaido",
    "explicit_botanical_override",
    1L,
    "Kuriles are not assigned to Hokkaido botanical area.",
    
    "Kurile Islands",
    "JP51",
    "Kuriles",
    "Hokkaido",
    "explicit_botanical_override",
    1L,
    "Kurile island-group synonym.",
    
    "Chishima",
    "JP51",
    "Kuriles",
    "Hokkaido",
    "explicit_botanical_override",
    1L,
    "Japanese geographical terminology for the Kuriles."
  )
  
  if (
    any(
      !special_crosswalk$target_area_id %in%
      botanical_areas$area_id
    )
  ) {
    stop(
      "Special crosswalk contains unknown botanical area IDs."
    )
  }
  
  # ---------------------------------------------------------------------------
  # Political-prefecture crosswalk
  #
  # These are default relationships only.
  # Special island rules must take precedence during occurrence assignment.
  # ---------------------------------------------------------------------------
  
  prefecture_crosswalk <- political %>%
    transmute(
      administrative_name = area_name,
      target_area_id = area_id,
      target_area_name =
        analytical_area_name,
      rule_type =
        "default_political_prefecture",
      priority = 2L,
      notes = case_when(
        area_name == "Tokyo" ~
          paste(
            "Default applies only after excluding",
            "Izu, Ogasawara and Kazan."
          ),
        
        area_name == "Kagoshima" ~
          paste(
            "Default applies only after excluding",
            "Amami/Ryukyu botanical territory."
          ),
        
        area_name == "Hokkaido" ~
          paste(
            "Default applies only after excluding",
            "Kuriles."
          ),
        
        TRUE ~
          NA_character_
      )
    )
  
  # ---------------------------------------------------------------------------
  # District reference
  #
  # These broad districts will later support Star-distribution calculations.
  # Island groups remain explicit rather than silently forced into mainland
  # district counts.
  # ---------------------------------------------------------------------------
  
  district_reference <- botanical_areas %>%
    count(
      district,
      name = "analytical_area_count"
    ) %>%
    arrange(district)
  
  # ---------------------------------------------------------------------------
  # Star geography notes
  #
  # This is deliberately NOT the Star-counting framework itself.
  # It records constraints to prevent later misuse.
  # ---------------------------------------------------------------------------
  
  star_geography_rules <- tribble(
    ~rule_id,
    ~rule,
    ~implementation_stage,
    
    "STAR_GEO_01",
    paste(
      "The 14 subprefectures of Hokkaido are treated",
      "as prefectures in Nakamura's Key to the Stars."
    ),
    "future_star_geography",
    
    "STAR_GEO_02",
    paste(
      "Where Japanese distribution is described only",
      "at district level, all prefectures in that district",
      "are counted as a conservative approximation."
    ),
    "future_star_geography",
    
    "STAR_GEO_03",
    paste(
      "Ryukyu, Ogasawara and Izu are explicitly used",
      "as small-island groups in the endemic branch",
      "of Nakamura's Key."
    ),
    "future_star_geography",
    
    "STAR_GEO_04",
    paste(
      "Botanical-area assignment and Star-counting",
      "geography must remain separate analytical concepts."
    ),
    "future_star_geography"
  )
  
  # ---------------------------------------------------------------------------
  # Framework summary
  # ---------------------------------------------------------------------------
  
  framework_summary <- tibble(
    metric = c(
      "analytical_botanical_areas",
      "political_prefecture_areas",
      "botanical_island_group_areas",
      "display_botanical_areas",
      "special_assignment_areas",
      "star_small_island_groups"
    ),
    value = c(
      nrow(botanical_areas),
      political_n,
      island_n,
      nrow(botanical_display_areas),
      sum(
        botanical_areas$
          special_assignment_required
      ),
      sum(
        botanical_areas$
          star_small_island_group
      )
    )
  )
  
  # ---------------------------------------------------------------------------
  # Metadata
  # ---------------------------------------------------------------------------
  
  metadata <- tibble(
    metric = c(
      "script_version",
      "input_geography_01_version",
      "framework_source",
      "analytical_botanical_areas",
      "display_botanical_areas",
      "political_prefectures",
      "additional_botanical_island_groups",
      "kazan_analytically_separate",
      "kazan_display_rollup",
      "amami_botanical_area",
      "hokkaido_subprefectures_applied",
      "occurrences_assigned",
      "spatial_polygons_created",
      "coordinates_modified",
      "taxonomy_modified",
      "upstream_tables_modified"
    ),
    value = c(
      SCRIPT_VERSION,
      geography_01_version,
      "Nakamura_2012",
      "51",
      "50",
      "46",
      "5",
      "TRUE",
      "Ogasawara Islands",
      "Ryukyu Islands",
      "FALSE",
      "FALSE",
      "FALSE",
      "FALSE",
      "FALSE",
      "FALSE"
    )
  )
  
  # ---------------------------------------------------------------------------
  # Persist framework
  # ---------------------------------------------------------------------------
  
  dbWriteTable(
    con,
    "japan_botanical_areas",
    botanical_areas,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "japan_botanical_display_areas",
    botanical_display_areas,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "japan_botanical_special_crosswalk",
    special_crosswalk,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "japan_botanical_prefecture_crosswalk",
    prefecture_crosswalk,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "japan_botanical_district_reference",
    district_reference,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "japan_botanical_star_geography_rules",
    star_geography_rules,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "japan_botanical_framework_summary",
    framework_summary,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "japan_botanical_framework_metadata",
    metadata,
    overwrite = TRUE
  )
  
  # ---------------------------------------------------------------------------
  # CSV outputs
  # ---------------------------------------------------------------------------
  
  out_dir <- here(
    "outputs",
    "tables",
    "geography",
    "botanical_area_framework"
  )
  
  dir.create(
    out_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  write_csv(
    botanical_areas,
    file.path(
      out_dir,
      "japan_botanical_areas_51.csv"
    )
  )
  
  write_csv(
    botanical_display_areas,
    file.path(
      out_dir,
      "japan_botanical_display_areas_50.csv"
    )
  )
  
  write_csv(
    special_crosswalk,
    file.path(
      out_dir,
      "special_area_crosswalk.csv"
    )
  )
  
  write_csv(
    prefecture_crosswalk,
    file.path(
      out_dir,
      "prefecture_crosswalk.csv"
    )
  )
  
  write_csv(
    district_reference,
    file.path(
      out_dir,
      "district_reference.csv"
    )
  )
  
  write_csv(
    star_geography_rules,
    file.path(
      out_dir,
      "star_geography_rules.csv"
    )
  )
  
  write_csv(
    framework_summary,
    file.path(
      out_dir,
      "framework_summary.csv"
    )
  )
  
  write_csv(
    metadata,
    file.path(
      out_dir,
      "metadata.csv"
    )
  )
  
  # ---------------------------------------------------------------------------
  # Console report
  # ---------------------------------------------------------------------------
  
  cat("— Framework summary —\n\n")
  print(
    framework_summary,
    n = Inf
  )
  
  cat(
    "\n— 51 analytical botanical areas —\n\n"
  )
  
  print(
    botanical_areas %>%
      select(
        area_id,
        area_no,
        analytical_area_name,
        area_type,
        district,
        display_area_no
      ),
    n = Inf
  )
  
  cat(
    "\n— Special botanical-area rules —\n\n"
  )
  
  print(
    special_crosswalk,
    n = Inf
  )
  
  cat(
    "\n— 50-area display roll-up differences —\n\n"
  )
  
  print(
    botanical_display_areas %>%
      filter(
        analytical_area_count > 1
      ),
    n = Inf
  )
  
  cat(
    "\n— Star geography constraints —\n\n"
  )
  
  print(
    star_geography_rules,
    n = Inf
  )
  
  cat("\n— Validation —\n")
  cat("51 analytical botanical areas: PASS\n")
  cat("46 political prefectures: PASS\n")
  cat("5 additional island groups: PASS\n")
  cat("50 display botanical areas: PASS\n")
  cat("Kazan analytical separation retained: PASS\n")
  cat("Amami -> Ryukyu rule retained: PASS\n")
  cat("Hokkaido Star geography deferred: PASS\n")
  
  cat("\n— Safety —\n")
  cat("Occurrences assigned: FALSE\n")
  cat("Spatial polygons created: FALSE\n")
  cat("Coordinates modified: FALSE\n")
  cat("Taxonomy modified: FALSE\n")
  cat("Frozen upstream tables modified: FALSE\n")
  cat("Output status: BOTANICAL GEOGRAPHY REFERENCE FRAMEWORK\n")
  
  cat(
    "\n02_build_botanical_area_framework.R v",
    SCRIPT_VERSION,
    " complete.\n",
    sep = ""
  )
  
  invisible(
    list(
      botanical_areas =
        botanical_areas,
      botanical_display_areas =
        botanical_display_areas,
      special_crosswalk =
        special_crosswalk,
      prefecture_crosswalk =
        prefecture_crosswalk,
      district_reference =
        district_reference,
      star_geography_rules =
        star_geography_rules,
      framework_summary =
        framework_summary,
      metadata =
        metadata
    )
  )
}

geography_02 <-
  run_botanical_area_framework()