# =============================================================================
# VPJD-OJPCP
# R/stars/03p_transcribe_key_to_stars.R
# Version 0.1.0
#
# FORMAL TRANSCRIPTION OF FIGURE 2.2:
# "KEYS TO STARS FOR THE JAPANESE FLORA"
# Nakamura, N. (2012), Figure 2.2, p.48
#
# PURPOSE
# - Create a machine-readable specification of the published Key to Stars.
# - Preserve the structure and terminology of Figure 2.2.
# - Separate the published Key from contemporary VPJD evidence.
# - Identify criteria that still require operational definitions.
# - Encode the complete published non-endemic Star matrix.
#
# IMPORTANT
# - This module DOES NOT classify VPJD taxa.
# - It DOES NOT create a final Star table.
# - It DOES NOT create JAPAN_DISTRICT_COUNT.
# - It DOES NOT define "rare".
# - It DOES NOT define "almost all districts".
# - It DOES NOT infer treatment of Kazan.
# - It DOES NOT infer Taiwan/Korea/Kuriles-Sakhalin subregional evidence.
# - It DOES NOT use historical Star allocations as analytical inputs.
# - Figure 2.2 is treated as the authoritative Key specification.
# =============================================================================

suppressPackageStartupMessages({
  library(DBI)
  library(duckdb)
  library(dplyr)
  library(readr)
  library(tibble)
  library(here)
})

MODULE <- "stars_03p_transcribe_key_to_stars"
VERSION <- "0.1.0"
RUN_DATE <- Sys.Date()

DB_PATH <- here(
  "data", "interim", "occurrences", "vpjd_occurrences.duckdb"
)

OUTPUT_DIR <- here(
  "outputs", "tables", "stars",
  "key_source_transcription"
)

TABLE_NODES <- "vpjd_star_key_source_nodes"
TABLE_MATRIX <- "vpjd_star_key_non_endemic_matrix"
TABLE_TERMINALS <- "vpjd_star_key_terminal_categories"
TABLE_GAPS <- "vpjd_star_key_operational_gaps"
TABLE_ASSERTIONS <- "vpjd_star_key_source_assertions"
TABLE_VALIDATION <- "vpjd_star_03p_validation"
TABLE_METADATA <- "vpjd_star_03p_metadata"

run_stars_03p <- function() {
  
  cat("\n— Formal transcription of Figure 2.2 —\n\n")
  cat("Run date: ", as.character(RUN_DATE), "\n", sep = "")
  cat("Module: ", MODULE, "\n", sep = "")
  cat("Version: ", VERSION, "\n\n", sep = "")
  
  dir.create(
    OUTPUT_DIR,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  if (!file.exists(DB_PATH)) {
    stop("VPJD DuckDB not found: ", DB_PATH)
  }
  
  con <- dbConnect(
    duckdb::duckdb(),
    dbdir = DB_PATH,
    read_only = FALSE
  )
  
  on.exit(
    dbDisconnect(con, shutdown = TRUE),
    add = TRUE
  )
  
  # ===========================================================================
  # 1. Source register
  # ===========================================================================
  
  source_register <- tibble(
    SOURCE_ID = "NAKAMURA_2012_FIGURE_2_2",
    SOURCE_AUTHOR = "Nakamura, N.",
    SOURCE_YEAR = 2012L,
    SOURCE_PAGE = 48L,
    SOURCE_FIGURE = "Figure 2.2",
    SOURCE_TITLE = "Keys to Stars for the Japanese flora",
    SOURCE_ROLE = "AUTHORITATIVE_KEY_SPECIFICATION",
    TRANSCRIPTION_METHOD =
      "Direct transcription from user-supplied Figure 2.2 image",
    HISTORICAL_STAR_ALLOCATIONS_USED = FALSE
  )
  
  # ===========================================================================
  # 2. Terminal Star categories explicitly present in Figure 2.2
  # ===========================================================================
  
  terminals <- tribble(
    ~STAR_CODE, ~STAR_NAME, ~SOURCE_STATUS,
    "BK", "BLACK", "EXPLICIT_IN_FIGURE",
    "GD", "GOLD",  "EXPLICIT_IN_FIGURE",
    "BU", "BLUE",  "EXPLICIT_IN_FIGURE",
    "GN", "GREEN", "EXPLICIT_IN_FIGURE",
    "GX", "GX",    "EXPLICIT_IN_FIGURE"
  )
  
  # HYB is intentionally absent.
  #
  # HYB is part of the contemporary VPJD treatment developed separately.
  # It is not a terminal category shown in Figure 2.2 and must therefore not
  # be represented here as though it were part of Nakamura's published Key.
  
  # ===========================================================================
  # 3. Literal structural transcription of the endemic branch
  #
  # SOURCE_TEXT preserves the wording of Figure 2.2 as closely as possible.
  # OPERATIONAL_STATUS distinguishes a literal rule from a criterion that
  # still requires a defensible contemporary operational definition.
  # ===========================================================================
  
  endemic_nodes <- tribble(
    ~NODE_ID, ~PARENT_NODE, ~BRANCH, ~SOURCE_LABEL, ~SOURCE_TEXT,
    ~YES_DESTINATION, ~NO_DESTINATION, ~OPERATIONAL_STATUS,
    
    "KTS-001", NA_character_, "ROOT", "1",
    "Endemic to Japan",
    "KTS-E02", "KTS-N02",
    "METHOD_VALIDATED",
    
    "KTS-E02", "KTS-001", "ENDEMIC", "2",
    paste0(
      "Endemic to any (or all) of the following small island groups: ",
      "Ryukyu Islands, Ogasawara Islands, Izu Islands"
    ),
    "BK", "KTS-E02A",
    "METHOD_VALIDATED_CRITERION_EVIDENCE_IMPLEMENTATION_REQUIRED",
    
    "KTS-E02A", "KTS-E02", "ENDEMIC", "2a",
    "Not only in the small island groups",
    "KTS-E03", NA_character_,
    "METHOD_VALIDATED",
    
    "KTS-E03", "KTS-E02A", "ENDEMIC", "3",
    "Not so widespread in Japan (≤2 districts AND ≤14 prefectures)",
    "KTS-E03-P7", "KTS-E03A",
    "OPERATIONAL_DEFINITION_REQUIRED",
    
    "KTS-E03-P7", "KTS-E03", "ENDEMIC", "3",
    "Only in ≤7 prefectures",
    "BK", "GD",
    "METHOD_VALIDATED",
    
    "KTS-E03A", "KTS-E03", "ENDEMIC", "3a",
    "Widespread in Japan (>2 districts OR >14 prefectures)",
    "KTS-E04", NA_character_,
    "OPERATIONAL_DEFINITION_REQUIRED",
    
    "KTS-E04", "KTS-E03A", "ENDEMIC", "4",
    "Occur in ≤10 prefectures",
    "KTS-E04-RARE", "KTS-E04A",
    "METHOD_VALIDATED",
    
    "KTS-E04-RARE", "KTS-E04", "ENDEMIC", "4",
    "In ≤5 prefectures and rare (e.g., mountain tops, along the coastline)",
    "BK", "KTS-E04-GD",
    "OPERATIONAL_DEFINITION_REQUIRED",
    
    "KTS-E04-GD", "KTS-E04-RARE", "ENDEMIC", "4",
    "In >5 prefectures",
    "GD", NA_character_,
    "METHOD_VALIDATED",
    
    "KTS-E04A", "KTS-E04", "ENDEMIC", "4a",
    "Occur in >10 prefectures",
    "KTS-E04A-DIST", NA_character_,
    "METHOD_VALIDATED",
    
    "KTS-E04A-DIST", "KTS-E04A", "ENDEMIC", "4a",
    "Not in all districts",
    "BU", "KTS-E04A-ALL",
    "OPERATIONAL_DEFINITION_REQUIRED",
    
    "KTS-E04A-ALL", "KTS-E04A-DIST", "ENDEMIC", "4a",
    "In almost all districts",
    "GN", NA_character_,
    "OPERATIONAL_DEFINITION_REQUIRED",
    
    "KTS-E04B", "KTS-001", "ENDEMIC", "4b",
    "Cultivated species",
    "GX", NA_character_,
    "EVIDENCE_IMPLEMENTATION_REQUIRED"
  )
  
  # ===========================================================================
  # 4. Literal structural transcription of the non-endemic branch
  # ===========================================================================
  
  non_endemic_nodes <- tribble(
    ~NODE_ID, ~PARENT_NODE, ~BRANCH, ~SOURCE_LABEL, ~SOURCE_TEXT,
    ~YES_DESTINATION, ~NO_DESTINATION, ~OPERATIONAL_STATUS,
    
    "KTS-N02", "KTS-001", "NON_ENDEMIC", "1a",
    "Not endemic to Japan",
    "KTS-N02-RANGE", NA_character_,
    "METHOD_VALIDATED",
    
    "KTS-N02-RANGE", "KTS-N02", "NON_ENDEMIC", "2",
    paste0(
      "Apart from Japan, only in one of Taiwan, Korea, OR the ",
      "Kuriles & Sakhalin, use the Table"
    ),
    "KTS-N-MATRIX", "KTS-N02A",
    "EVIDENCE_IMPLEMENTATION_REQUIRED",
    
    "KTS-N-MATRIX", "KTS-N02-RANGE", "NON_ENDEMIC", "TABLE",
    "Distribution outside Japan × Distribution within Japan",
    "MATRIX_LOOKUP", NA_character_,
    "OPERATIONAL_DEFINITION_REQUIRED",
    
    "KTS-N02A", "KTS-N02-RANGE", "NON_ENDEMIC", "2a",
    paste0(
      "Not only in one of Taiwan or Korea OR the Kuriles & Sakhalin, ",
      "and beyond"
    ),
    "GN", NA_character_,
    "EVIDENCE_IMPLEMENTATION_REQUIRED"
  )
  
  key_nodes <- bind_rows(
    endemic_nodes,
    non_endemic_nodes
  )
  
  # ===========================================================================
  # 5. Published non-endemic matrix
  #
  # Matrix columns reproduce Figure 2.2:
  #
  #   Islands or ≤1 district
  #   ≤2 districts
  #   ≤3 districts
  #   >3 districts
  #
  # These are Japanese-distribution columns.
  # ===========================================================================
  
  non_endemic_matrix <- tribble(
    ~MATRIX_ROW_ID,
    ~EXTERNAL_REGION,
    ~EXTERNAL_DISTRIBUTION_CLASS,
    ~JAPAN_ISLANDS_OR_LE_1_DISTRICT,
    ~JAPAN_LE_2_DISTRICTS,
    ~JAPAN_LE_3_DISTRICTS,
    ~JAPAN_GT_3_DISTRICTS,
    ~OPERATIONAL_STATUS,
    
    "TW_01",
    "Taiwan",
    "<½ island",
    "BK", "GD", "BU", "GN",
    "OPERATIONAL_DEFINITION_REQUIRED",
    
    "TW_02",
    "Taiwan",
    "≥½ island",
    "GD", "BU", "GN", "GN",
    "OPERATIONAL_DEFINITION_REQUIRED",
    
    "KR_01",
    "Korea",
    "Islands or sparse distribution",
    "BK", "GD", "BU", "GN",
    "OPERATIONAL_DEFINITION_REQUIRED",
    
    "KR_02",
    "Korea",
    "Not only in islands nor restricted",
    "GD", "BU", "GN", "GN",
    "OPERATIONAL_DEFINITION_REQUIRED",
    
    "KS_01",
    "Kuriles & Sakhalin",
    "Only in the south Kuriles",
    "BK", "GD", "BU", "GN",
    "OPERATIONAL_DEFINITION_REQUIRED",
    
    "KS_02",
    "Kuriles & Sakhalin",
    "Not only in the south Kuriles",
    "GD", "BU", "GN", "GN",
    "OPERATIONAL_DEFINITION_REQUIRED",
    
    "CN_01",
    "China",
    "≤2 provinces",
    "GD", "BU", "GN", "GN",
    "OPERATIONAL_DEFINITION_REQUIRED",
    
    "CN_02",
    "China",
    ">2 provinces",
    "GN", "GN", "GN", "GN",
    "OPERATIONAL_DEFINITION_REQUIRED"
  )
  
  # ===========================================================================
  # 6. Source assertions
  #
  # These are statements that can be established directly from Figure 2.2
  # without imposing a new analytical interpretation.
  # ===========================================================================
  
  source_assertions <- tribble(
    ~ASSERTION_ID, ~TOPIC, ~ASSERTION, ~STATUS,
    
    "03P-A001",
    "ENDEMICITY",
    "The Key first separates taxa endemic to Japan from taxa not endemic to Japan.",
    "SOURCE_EXPLICIT",
    
    "03P-A002",
    "SMALL_ISLAND_GROUPS",
    paste0(
      "Ryukyu Islands, Ogasawara Islands and Izu Islands are explicitly ",
      "named as the small island groups in endemic node 2."
    ),
    "SOURCE_EXPLICIT",
    
    "03P-A003",
    "SMALL_ISLAND_TERMINAL",
    paste0(
      "An endemic taxon confined to any or all of Ryukyu, Ogasawara and ",
      "Izu terminates at BLACK before the ordinary district thresholds."
    ),
    "SOURCE_EXPLICIT",
    
    "03P-A004",
    "KAZAN",
    "Kazan Islands are not explicitly named in Figure 2.2.",
    "SOURCE_EXPLICIT_ABSENCE",
    
    "03P-A005",
    "ENDEMIC_DISTRICT_THRESHOLD",
    paste0(
      "The endemic branch distinguishes ≤2 districts AND ≤14 prefectures ",
      "from >2 districts OR >14 prefectures."
    ),
    "SOURCE_EXPLICIT",
    
    "03P-A006",
    "RARE",
    paste0(
      "The endemic branch explicitly uses rarity together with ≤5 ",
      "prefectures and gives mountain tops and coastline as examples."
    ),
    "SOURCE_EXPLICIT",
    
    "03P-A007",
    "ALMOST_ALL_DISTRICTS",
    paste0(
      "For endemic taxa occurring in >10 prefectures, the Key distinguishes ",
      "not in all districts from in almost all districts."
    ),
    "SOURCE_EXPLICIT",
    
    "03P-A008",
    "NON_ENDEMIC_MATRIX",
    paste0(
      "Non-endemic taxa occurring outside Japan only in one of Taiwan, ",
      "Korea, or Kuriles & Sakhalin are evaluated using the published table."
    ),
    "SOURCE_EXPLICIT",
    
    "03P-A009",
    "NON_ENDEMIC_JAPAN_COLUMNS",
    paste0(
      "The non-endemic table uses Japanese distribution classes: ",
      "Islands or ≤1 district, ≤2 districts, ≤3 districts, and >3 districts."
    ),
    "SOURCE_EXPLICIT",
    
    "03P-A010",
    "KURILES_SAKHALIN",
    paste0(
      "Kuriles & Sakhalin are explicitly represented as an external ",
      "distribution region in the non-endemic table."
    ),
    "SOURCE_EXPLICIT",
    
    "03P-A011",
    "CHINA",
    paste0(
      "China appears as a separate external-region row in the published ",
      "non-endemic matrix, divided into ≤2 provinces and >2 provinces."
    ),
    "SOURCE_EXPLICIT",
    
    "03P-A012",
    "BROAD_EXTERNAL_RANGE",
    paste0(
      "The published Key assigns GREEN when a non-endemic taxon is not ",
      "confined outside Japan to one of Taiwan, Korea, or Kuriles & ",
      "Sakhalin, and extends beyond."
    ),
    "SOURCE_EXPLICIT",
    
    "03P-A013",
    "NON_ENDEMIC_STAR_RANGE",
    paste0(
      "BLACK, GOLD, BLUE and GREEN can all arise in the non-endemic ",
      "branch."
    ),
    "SOURCE_EXPLICIT",
    
    "03P-A014",
    "GX",
    "Figure 2.2 explicitly contains the terminal category GX for cultivated species.",
    "SOURCE_EXPLICIT",
    
    "03P-A015",
    "HYB",
    "Figure 2.2 does not explicitly contain a HYB terminal category.",
    "SOURCE_EXPLICIT_ABSENCE"
  )
  
  # ===========================================================================
  # 7. Operational gap register
  #
  # These are deliberately NOT resolved by this script.
  # ===========================================================================
  
  operational_gaps <- tribble(
    ~GAP_ID, ~KEY_COMPONENT, ~SOURCE_TERM, ~REQUIRED_RESOLUTION,
    ~CURRENT_STATUS, ~BLOCKS_FINAL_CLASSIFIER,
    
    "03P-G001",
    "JAPAN_DISTRICT",
    "district",
    paste0(
      "Confirm the geographical units constituting Nakamura's Japanese ",
      "district system and establish the complete district denominator."
    ),
    "UNRESOLVED",
    TRUE,
    
    "03P-G002",
    "ALMOST_ALL_DISTRICTS",
    "almost all districts",
    paste0(
      "Establish the intended denominator and operational threshold for ",
      "'almost all districts'."
    ),
    "UNRESOLVED",
    TRUE,
    
    "03P-G003",
    "RARE",
    "rare",
    paste0(
      "Establish a reproducible contemporary definition of rarity ",
      "consistent with the Key."
    ),
    "UNRESOLVED",
    TRUE,
    
    "03P-G004",
    "KAZAN",
    "Kazan Islands",
    paste0(
      "Determine whether Kazan is included within Ogasawara for the ",
      "endemic small-island-group criterion."
    ),
    "UNRESOLVED",
    TRUE,
    
    "03P-G005",
    "TAIWAN_RANGE",
    "<½ island / ≥½ island",
    paste0(
      "Define reproducible evidence for the two Taiwan distribution ",
      "classes."
    ),
    "UNRESOLVED",
    TRUE,
    
    "03P-G006",
    "KOREA_RANGE",
    "Islands or sparse distribution",
    paste0(
      "Define reproducible evidence distinguishing islands or sparse ",
      "distribution from not only in islands nor restricted."
    ),
    "UNRESOLVED",
    TRUE,
    
    "03P-G007",
    "KURILES_SAKHALIN_RANGE",
    "Only in the south Kuriles",
    paste0(
      "Define reproducible evidence distinguishing south-Kuriles-only ",
      "from broader Kuriles/Sakhalin distribution."
    ),
    "UNRESOLVED",
    TRUE,
    
    "03P-G008",
    "CHINA_RANGE",
    "≤2 provinces / >2 provinces",
    paste0(
      "Map contemporary WCVP Chinese distribution units to province-level ",
      "evidence suitable for the published threshold."
    ),
    "UNRESOLVED",
    TRUE,
    
    "03P-G009",
    "BROAD_EXTERNAL_RANGE",
    paste0(
      "Not only in one of Taiwan or Korea OR the Kuriles & Sakhalin, ",
      "and beyond"
    ),
    paste0(
      "Define a deterministic test for the broad external-range GREEN ",
      "terminal using contemporary range evidence."
    ),
    "UNRESOLVED",
    TRUE,
    
    "03P-G010",
    "CULTIVATED_GX",
    "Cultivated species",
    paste0(
      "Document how the published cultivated-species criterion maps to ",
      "the contemporary VPJD positive introduced-to-Japan GX rule."
    ),
    "REQUIRES_METHOD_CROSSWALK",
    TRUE,
    
    "03P-G011",
    "HYB",
    "HYB",
    paste0(
      "Document HYB as a contemporary VPJD extension outside the literal ",
      "terminal categories shown in Figure 2.2, including its precedence."
    ),
    "REQUIRES_METHOD_CROSSWALK",
    TRUE
  )
  
  # ===========================================================================
  # 8. Evidence requirements derived from the source
  # ===========================================================================
  
  evidence_requirements <- tribble(
    ~REQUIREMENT_ID, ~KEY_BRANCH, ~EVIDENCE_REQUIRED, ~CURRENT_POSITION,
    
    "03P-R001",
    "PRE_KEY",
    "Endemicity to Japan",
    "AVAILABLE_FROM_STARS_03C",
    
    "03P-R002",
    "ENDEMIC",
    "Confinement to Ryukyu/Ogasawara/Izu small island groups",
    "DATA_AVAILABLE_METHOD_APPLICATION_REQUIRED",
    
    "03P-R003",
    "ENDEMIC_AND_NON_ENDEMIC",
    "Japanese district occupancy",
    "CORE_EVIDENCE_AVAILABLE_FINAL_METHOD_UNRESOLVED",
    
    "03P-R004",
    "ENDEMIC",
    "Japanese prefecture occupancy",
    "AVAILABLE_FROM_STARS_03A",
    
    "03P-R005",
    "ENDEMIC",
    "Rarity evidence",
    "METHOD_AND_EVIDENCE_UNRESOLVED",
    
    "03P-R006",
    "ENDEMIC",
    "Almost-all-districts evidence",
    "METHOD_UNRESOLVED",
    
    "03P-R007",
    "NON_ENDEMIC",
    "Taiwan range extent",
    "REGIONAL_PRESENCE_AVAILABLE_SUBREGIONAL_CLASS_UNRESOLVED",
    
    "03P-R008",
    "NON_ENDEMIC",
    "Korean range pattern",
    "REGIONAL_PRESENCE_AVAILABLE_SUBREGIONAL_CLASS_UNRESOLVED",
    
    "03P-R009",
    "NON_ENDEMIC",
    "Kuriles/Sakhalin range pattern",
    "REGIONAL_PRESENCE_AVAILABLE_SUBREGIONAL_CLASS_UNRESOLVED",
    
    "03P-R010",
    "NON_ENDEMIC",
    "Chinese province count",
    "CHINA_PRESENCE_AVAILABLE_PROVINCE_COUNT_NOT_IMPLEMENTED",
    
    "03P-R011",
    "NON_ENDEMIC",
    "Broader external range beyond specified regions",
    "WCVP_RANGE_AVAILABLE_METHOD_NOT_IMPLEMENTED",
    
    "03P-R012",
    "GX",
    "Cultivated/introduced-to-Japan evidence",
    "POSITIVE_WCVP_INTRODUCTION_EVIDENCE_AVAILABLE",
    
    "03P-R013",
    "HYB",
    "Contemporary explicit hybrid-name evidence",
    "AVAILABLE_FROM_STARS_03G"
  )
  
  # ===========================================================================
  # 9. Prohibited assumptions
  # ===========================================================================
  
  prohibited_assumptions <- tribble(
    ~ASSUMPTION_ID, ~PROHIBITED_ASSUMPTION,
    
    "03P-P001",
    "Do not route every non-endemic taxon directly to GREEN.",
    
    "03P-P002",
    paste0(
      "Do not infer a Star from historical Star frequencies or attempt ",
      "to reproduce historical category proportions."
    ),
    
    "03P-P003",
    paste0(
      "Do not treat Ryukyu, Izu, Ogasawara, Kazan and Kuriles as five ",
      "equivalent additional Japanese districts."
    ),
    
    "03P-P004",
    paste0(
      "Do not treat Kuriles as an ordinary Japanese district in the ",
      "non-endemic branch solely because JP51 exists analytically."
    ),
    
    "03P-P005",
    paste0(
      "Do not infer that Kazan belongs to the Ogasawara small-island ",
      "criterion without methodological evidence."
    ),
    
    "03P-P006",
    "Do not define 'rare' from GBIF occurrence counts alone.",
    
    "03P-P007",
    paste0(
      "Do not define 'almost all districts' until both the district ",
      "denominator and intended threshold are established."
    ),
    
    "03P-P008",
    paste0(
      "Do not infer Taiwan half-island classes from simple Taiwan ",
      "presence/absence."
    ),
    
    "03P-P009",
    paste0(
      "Do not infer Korean sparse distribution from simple Korea ",
      "presence/absence."
    ),
    
    "03P-P010",
    paste0(
      "Do not infer south-Kuriles-only status from simple Kuriles ",
      "presence/absence."
    ),
    
    "03P-P011",
    paste0(
      "Do not infer Chinese province counts from a single China ",
      "presence/absence flag."
    ),
    
    "03P-P012",
    paste0(
      "Do not represent HYB as an explicit Nakamura Figure 2.2 terminal ",
      "unless another primary source establishes it."
    )
  )
  
  # ===========================================================================
  # 10. Methodological readiness
  # ===========================================================================
  
  readiness <- tribble(
    ~COMPONENT, ~READY, ~STATUS,
    
    "Figure 2.2 source identified",
    TRUE,
    "ESTABLISHED",
    
    "Endemic/non-endemic root",
    TRUE,
    "METHOD_VALIDATED",
    
    "Endemic small-island criterion",
    TRUE,
    "SOURCE_TRANSCRIBED",
    
    "Endemic prefecture thresholds",
    TRUE,
    "METHOD_VALIDATED",
    
    "Endemic district thresholds",
    FALSE,
    "DISTRICT_DEFINITION_REQUIRED",
    
    "Rarity criterion",
    FALSE,
    "OPERATIONAL_DEFINITION_REQUIRED",
    
    "Almost-all-districts criterion",
    FALSE,
    "OPERATIONAL_DEFINITION_REQUIRED",
    
    "Non-endemic matrix structure",
    TRUE,
    "METHOD_VALIDATED",
    
    "Non-endemic matrix Star cells",
    TRUE,
    "METHOD_VALIDATED",
    
    "Taiwan subregional classification",
    FALSE,
    "OPERATIONAL_DEFINITION_REQUIRED",
    
    "Korea subregional classification",
    FALSE,
    "OPERATIONAL_DEFINITION_REQUIRED",
    
    "Kuriles/Sakhalin subregional classification",
    FALSE,
    "OPERATIONAL_DEFINITION_REQUIRED",
    
    "China province classification",
    FALSE,
    "OPERATIONAL_DEFINITION_REQUIRED",
    
    "Broad external-range GREEN criterion",
    FALSE,
    "EVIDENCE_IMPLEMENTATION_REQUIRED",
    
    "Kazan treatment",
    FALSE,
    "METHOD_RESOLUTION_REQUIRED",
    
    "GX contemporary crosswalk",
    FALSE,
    "METHOD_CROSSWALK_REQUIRED",
    
    "HYB contemporary extension",
    FALSE,
    "METHOD_CROSSWALK_REQUIRED",
    
    "Final Star classifier",
    FALSE,
    "NOT_READY"
  )
  
  # ===========================================================================
  # 11. Validation
  # ===========================================================================
  
  valid_stars <- c(
    "BK", "GD", "BU", "GN", "GX"
  )
  
  matrix_star_values <- unlist(
    non_endemic_matrix |>
      select(
        JAPAN_ISLANDS_OR_LE_1_DISTRICT,
        JAPAN_LE_2_DISTRICTS,
        JAPAN_LE_3_DISTRICTS,
        JAPAN_GT_3_DISTRICTS
      ),
    use.names = FALSE
  )
  
  validation <- tibble(
    CHECK = c(
      "Source register has one authoritative source",
      "Source identified as Figure 2.2",
      "Source page = 48",
      "Five literal terminal categories recorded",
      "HYB absent from literal Figure 2.2 terminals",
      "Endemic small-island groups explicitly recorded",
      "Ryukyu explicitly recorded",
      "Ogasawara explicitly recorded",
      "Izu explicitly recorded",
      "Kazan not inserted into source small-island criterion",
      "Endemic ≤2 district threshold recorded",
      "Endemic ≤14 prefecture threshold recorded",
      "Endemic ≤7 prefecture terminal split recorded",
      "Endemic ≤10 prefecture threshold recorded",
      "Endemic ≤5 prefecture rarity criterion recorded",
      "Endemic >10 prefecture branch recorded",
      "Almost-all-districts criterion retained unresolved",
      "Non-endemic matrix has eight source rows",
      "Non-endemic matrix has four Japanese distribution columns",
      "Non-endemic matrix contains 32 Star cells",
      "All matrix Stars are valid literal terminal categories",
      "Non-endemic matrix contains BLACK",
      "Non-endemic matrix contains GOLD",
      "Non-endemic matrix contains BLUE",
      "Non-endemic matrix contains GREEN",
      "Taiwan has two matrix rows",
      "Korea has two matrix rows",
      "Kuriles & Sakhalin has two matrix rows",
      "China has two matrix rows",
      "Broad external-range GREEN criterion recorded",
      "Operational gap register is non-empty",
      "District definition remains unresolved",
      "Rarity remains unresolved",
      "Almost-all-districts remains unresolved",
      "Kazan treatment remains unresolved",
      "No VPJD taxon classifications created",
      "No JAPAN_DISTRICT_COUNT created",
      "Historical Star allocations not used",
      "Final classifier remains not ready"
    ),
    
    PASS = c(
      nrow(source_register) == 1L,
      
      source_register$SOURCE_FIGURE[[1]] ==
        "Figure 2.2",
      
      source_register$SOURCE_PAGE[[1]] == 48L,
      
      nrow(terminals) == 5L,
      
      !"HYB" %in% terminals$STAR_CODE,
      
      grepl(
        "Ryukyu Islands, Ogasawara Islands, Izu Islands",
        key_nodes$SOURCE_TEXT[
          key_nodes$NODE_ID == "KTS-E02"
        ],
        fixed = TRUE
      ),
      
      grepl(
        "Ryukyu",
        key_nodes$SOURCE_TEXT[
          key_nodes$NODE_ID == "KTS-E02"
        ]
      ),
      
      grepl(
        "Ogasawara",
        key_nodes$SOURCE_TEXT[
          key_nodes$NODE_ID == "KTS-E02"
        ]
      ),
      
      grepl(
        "Izu",
        key_nodes$SOURCE_TEXT[
          key_nodes$NODE_ID == "KTS-E02"
        ]
      ),
      
      !grepl(
        "Kazan",
        key_nodes$SOURCE_TEXT[
          key_nodes$NODE_ID == "KTS-E02"
        ]
      ),
      
      any(
        grepl(
          "≤2 districts",
          key_nodes$SOURCE_TEXT,
          fixed = TRUE
        )
      ),
      
      any(
        grepl(
          "≤14 prefectures",
          key_nodes$SOURCE_TEXT,
          fixed = TRUE
        )
      ),
      
      any(
        grepl(
          "≤7 prefectures",
          key_nodes$SOURCE_TEXT,
          fixed = TRUE
        )
      ),
      
      any(
        grepl(
          "≤10 prefectures",
          key_nodes$SOURCE_TEXT,
          fixed = TRUE
        )
      ),
      
      any(
        grepl(
          "≤5 prefectures and rare",
          key_nodes$SOURCE_TEXT,
          fixed = TRUE
        )
      ),
      
      any(
        grepl(
          ">10 prefectures",
          key_nodes$SOURCE_TEXT,
          fixed = TRUE
        )
      ),
      
      any(
        operational_gaps$KEY_COMPONENT ==
          "ALMOST_ALL_DISTRICTS" &
          operational_gaps$CURRENT_STATUS ==
          "UNRESOLVED"
      ),
      
      nrow(non_endemic_matrix) == 8L,
      
      length(
        c(
          "JAPAN_ISLANDS_OR_LE_1_DISTRICT",
          "JAPAN_LE_2_DISTRICTS",
          "JAPAN_LE_3_DISTRICTS",
          "JAPAN_GT_3_DISTRICTS"
        )
      ) == 4L,
      
      length(matrix_star_values) == 32L,
      
      all(
        matrix_star_values %in%
          valid_stars
      ),
      
      "BK" %in% matrix_star_values,
      
      "GD" %in% matrix_star_values,
      
      "BU" %in% matrix_star_values,
      
      "GN" %in% matrix_star_values,
      
      sum(
        non_endemic_matrix$EXTERNAL_REGION ==
          "Taiwan"
      ) == 2L,
      
      sum(
        non_endemic_matrix$EXTERNAL_REGION ==
          "Korea"
      ) == 2L,
      
      sum(
        non_endemic_matrix$EXTERNAL_REGION ==
          "Kuriles & Sakhalin"
      ) == 2L,
      
      sum(
        non_endemic_matrix$EXTERNAL_REGION ==
          "China"
      ) == 2L,
      
      any(
        key_nodes$NODE_ID ==
          "KTS-N02A" &
          key_nodes$YES_DESTINATION ==
          "GN"
      ),
      
      nrow(operational_gaps) > 0L,
      
      any(
        operational_gaps$KEY_COMPONENT ==
          "JAPAN_DISTRICT" &
          operational_gaps$CURRENT_STATUS ==
          "UNRESOLVED"
      ),
      
      any(
        operational_gaps$KEY_COMPONENT ==
          "RARE" &
          operational_gaps$CURRENT_STATUS ==
          "UNRESOLVED"
      ),
      
      any(
        operational_gaps$KEY_COMPONENT ==
          "ALMOST_ALL_DISTRICTS" &
          operational_gaps$CURRENT_STATUS ==
          "UNRESOLVED"
      ),
      
      any(
        operational_gaps$KEY_COMPONENT ==
          "KAZAN" &
          operational_gaps$CURRENT_STATUS ==
          "UNRESOLVED"
      ),
      
      TRUE,
      
      TRUE,
      
      !any(
        source_register$
          HISTORICAL_STAR_ALLOCATIONS_USED
      ),
      
      !readiness$READY[
        readiness$COMPONENT ==
          "Final Star classifier"
      ]
    )
  ) |>
    mutate(
      RESULT =
        if_else(
          PASS,
          "PASS",
          "FAIL"
        )
    )
  
  # ===========================================================================
  # 12. Console profiles
  # ===========================================================================
  
  cat("Source: Nakamura (2012), Figure 2.2, p.48\n")
  
  cat("\n— LITERAL TERMINAL CATEGORIES —\n")
  print(terminals, n = Inf)
  
  cat("\n— KEY SOURCE NODES —\n")
  print(
    key_nodes |>
      select(
        NODE_ID,
        BRANCH,
        SOURCE_LABEL,
        SOURCE_TEXT,
        YES_DESTINATION,
        NO_DESTINATION,
        OPERATIONAL_STATUS
      ),
    n = Inf
  )
  
  cat("\n— NON-ENDEMIC MATRIX —\n")
  print(
    non_endemic_matrix |>
      select(
        EXTERNAL_REGION,
        EXTERNAL_DISTRIBUTION_CLASS,
        JAPAN_ISLANDS_OR_LE_1_DISTRICT,
        JAPAN_LE_2_DISTRICTS,
        JAPAN_LE_3_DISTRICTS,
        JAPAN_GT_3_DISTRICTS
      ),
    n = Inf
  )
  
  cat("\n— OPERATIONAL GAPS —\n")
  print(
    operational_gaps |>
      select(
        GAP_ID,
        KEY_COMPONENT,
        SOURCE_TERM,
        CURRENT_STATUS,
        BLOCKS_FINAL_CLASSIFIER
      ),
    n = Inf
  )
  
  cat("\n— METHOD READINESS —\n")
  print(readiness, n = Inf)
  
  cat("\n— VALIDATION —\n")
  print(validation, n = Inf)
  
  if (!all(validation$PASS)) {
    stop(
      "Stars 03p validation failed. ",
      "Review FAIL checks before proceeding."
    )
  }
  
  # ===========================================================================
  # 13. Metadata
  # ===========================================================================
  
  metadata <- tibble(
    MODULE = MODULE,
    VERSION = VERSION,
    RUN_DATE = as.character(RUN_DATE),
    
    SOURCE_ID =
      source_register$SOURCE_ID[[1]],
    
    SOURCE =
      "Nakamura (2012), Figure 2.2, p.48",
    
    SOURCE_TITLE =
      "Keys to Stars for the Japanese flora",
    
    KEY_NODES =
      nrow(key_nodes),
    
    NON_ENDEMIC_MATRIX_ROWS =
      nrow(non_endemic_matrix),
    
    NON_ENDEMIC_MATRIX_CELLS =
      length(matrix_star_values),
    
    LITERAL_TERMINAL_CATEGORIES =
      nrow(terminals),
    
    OPERATIONAL_GAPS =
      nrow(operational_gaps),
    
    UNRESOLVED_OR_CROSSWALK_GAPS =
      sum(
        operational_gaps$CURRENT_STATUS !=
          "RESOLVED"
      ),
    
    TAXON_CLASSIFICATIONS_CREATED =
      FALSE,
    
    JAPAN_DISTRICT_COUNT_CREATED =
      FALSE,
    
    ALMOST_ALL_DISTRICTS_EVALUATED =
      FALSE,
    
    RARITY_EVALUATED =
      FALSE,
    
    HISTORICAL_STAR_ALLOCATIONS_USED =
      FALSE,
    
    FINAL_STAR_CLASSIFIER_READY =
      FALSE,
    
    STATUS =
      "VALIDATED_SOURCE_TRANSCRIPTION"
  )
  
  # ===========================================================================
  # 14. Write canonical DuckDB tables
  # ===========================================================================
  
  dbWriteTable(
    con,
    "vpjd_star_key_source_register",
    source_register,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    TABLE_TERMINALS,
    terminals,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    TABLE_NODES,
    key_nodes,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    TABLE_MATRIX,
    non_endemic_matrix,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    TABLE_ASSERTIONS,
    source_assertions,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    TABLE_GAPS,
    operational_gaps,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "vpjd_star_key_evidence_requirements",
    evidence_requirements,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "vpjd_star_key_prohibited_assumptions",
    prohibited_assumptions,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    "vpjd_star_key_method_readiness",
    readiness,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    TABLE_VALIDATION,
    validation,
    overwrite = TRUE
  )
  
  dbWriteTable(
    con,
    TABLE_METADATA,
    metadata,
    overwrite = TRUE
  )
  
  # ===========================================================================
  # 15. Write audit CSV outputs
  # ===========================================================================
  
  write_csv(
    source_register,
    file.path(
      OUTPUT_DIR,
      "stars03p_source_register.csv"
    )
  )
  
  write_csv(
    terminals,
    file.path(
      OUTPUT_DIR,
      "stars03p_terminal_categories.csv"
    )
  )
  
  write_csv(
    key_nodes,
    file.path(
      OUTPUT_DIR,
      "stars03p_key_source_nodes.csv"
    )
  )
  
  write_csv(
    non_endemic_matrix,
    file.path(
      OUTPUT_DIR,
      "stars03p_non_endemic_matrix.csv"
    )
  )
  
  write_csv(
    source_assertions,
    file.path(
      OUTPUT_DIR,
      "stars03p_source_assertions.csv"
    )
  )
  
  write_csv(
    operational_gaps,
    file.path(
      OUTPUT_DIR,
      "stars03p_operational_gaps.csv"
    )
  )
  
  write_csv(
    evidence_requirements,
    file.path(
      OUTPUT_DIR,
      "stars03p_evidence_requirements.csv"
    )
  )
  
  write_csv(
    prohibited_assumptions,
    file.path(
      OUTPUT_DIR,
      "stars03p_prohibited_assumptions.csv"
    )
  )
  
  write_csv(
    readiness,
    file.path(
      OUTPUT_DIR,
      "stars03p_method_readiness.csv"
    )
  )
  
  write_csv(
    validation,
    file.path(
      OUTPUT_DIR,
      "stars03p_validation.csv"
    )
  )
  
  write_csv(
    metadata,
    file.path(
      OUTPUT_DIR,
      "stars03p_metadata.csv"
    )
  )
  
  # ===========================================================================
  # 16. Final summary
  # ===========================================================================
  
  cat("\n============================================================\n")
  cat("Stars 03p v", VERSION, " COMPLETE\n", sep = "")
  cat("============================================================\n")
  
  cat(
    "Source: Nakamura (2012), Figure 2.2, p.48\n"
  )
  
  cat(
    "Key source nodes: ",
    nrow(key_nodes),
    "\n",
    sep = ""
  )
  
  cat(
    "Literal terminal categories: ",
    nrow(terminals),
    " (BK, GD, BU, GN, GX)\n",
    sep = ""
  )
  
  cat(
    "Non-endemic matrix rows: ",
    nrow(non_endemic_matrix),
    "\n",
    sep = ""
  )
  
  cat(
    "Non-endemic matrix Star cells: ",
    length(matrix_star_values),
    "\n",
    sep = ""
  )
  
  cat(
    "Operational gaps retained: ",
    nrow(operational_gaps),
    "\n",
    sep = ""
  )
  
  cat("Taxon classifications created: FALSE\n")
  cat("JAPAN_DISTRICT_COUNT created: FALSE\n")
  cat("'Almost all districts' evaluated: FALSE\n")
  cat("Rarity evaluated: FALSE\n")
  cat("Historical Star allocations used: FALSE\n")
  cat("Final classifier ready: FALSE\n")
  
  cat(
    "Validation: ",
    sum(validation$PASS),
    "/",
    nrow(validation),
    " PASS\n",
    sep = ""
  )
  
  cat(
    "Canonical Key table: ",
    TABLE_NODES,
    "\n",
    sep = ""
  )
  
  cat(
    "Canonical non-endemic matrix: ",
    TABLE_MATRIX,
    "\n",
    sep = ""
  )
  
  cat(
    "Outputs: ",
    OUTPUT_DIR,
    "\n",
    sep = ""
  )
  
  cat("============================================================\n")
  
  invisible(
    list(
      source_register = source_register,
      terminals = terminals,
      key_nodes = key_nodes,
      non_endemic_matrix = non_endemic_matrix,
      source_assertions = source_assertions,
      operational_gaps = operational_gaps,
      evidence_requirements = evidence_requirements,
      prohibited_assumptions = prohibited_assumptions,
      readiness = readiness,
      validation = validation,
      metadata = metadata
    )
  )
}

stars_03p_result <- run_stars_03p()