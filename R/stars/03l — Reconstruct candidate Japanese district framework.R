# ==============================================================================
# OJPCP / VPJD
# Stars 03l — Audit Nakamura district geography
# Version: 0.1.2
#
# PURPOSE
#   Audit the existing contemporary 51-area botanical geography against the
#   district geography required by Nakamura's Key to Stars.
#
#   The canonical 51-area framework already contains:
#     - area_id
#     - analytical_area_name
#     - district
#     - star_small_island_group
#     - special_assignment_required
#
#   This module therefore DOES NOT create a second district crosswalk.
#
#   It:
#     1. reads the existing 51-area framework;
#     2. profiles the district system already encoded there;
#     3. explicitly audits JP47-JP51 special botanical areas;
#     4. compares the framework with existing Stars 03a geography;
#     5. determines which methodological questions remain unresolved;
#     6. writes diagnostic outputs only.
#
# IMPORTANT
#   - NO Stars are assigned.
#   - NO district values are changed.
#   - NO taxon district counts are yet created for classification.
#   - NO historical Star allocations are used.
#   - Existing district values are treated as objects to audit, not assumed
#     correct merely because they already exist.
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(tidyr)
  library(purrr)
  library(tibble)
  library(here)
})

MODULE <- "Stars 03l"
VERSION <- "0.1.2"
RUN_DATE <- Sys.Date()

cat("\n============================================================\n")
cat(MODULE, "— Audit Nakamura district geography\n")
cat("Version:", VERSION, "\n")
cat("Run date:", as.character(RUN_DATE), "\n")
cat("============================================================\n\n")

# ------------------------------------------------------------------------------
# 1. Paths
# ------------------------------------------------------------------------------

area_file <- here(
  "outputs", "tables", "geography", "botanical_area_framework",
  "japan_botanical_areas_51.csv"
)

area_summary_file <- here(
  "outputs", "tables", "distribution", "japan_taxon_area",
  "taxon_area_summary_by_botanical_area.csv"
)

breadth_file <- here(
  "outputs", "tables", "distribution", "japan_taxon_distribution",
  "taxon_distribution_breadth.csv"
)

stars03a_file <- here(
  "outputs", "tables", "stars", "key_geography",
  "stars03a_taxon_key_geography.csv"
)

output_dir <- here(
  "outputs", "tables", "stars", "district_definition_audit"
)

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

input_register <- tibble(
  INPUT = c(
    "51-area botanical framework",
    "Taxon-area summary",
    "Taxon distribution breadth",
    "Stars 03a taxon key geography"
  ),
  PATH = c(
    area_file,
    area_summary_file,
    breadth_file,
    stars03a_file
  ),
  EXISTS = file.exists(c(
    area_file,
    area_summary_file,
    breadth_file,
    stars03a_file
  ))
)

cat("— INPUT AVAILABILITY —\n")
print(input_register, n = Inf)

if (!all(input_register$EXISTS)) {
  stop("Required input file(s) missing.")
}

# ------------------------------------------------------------------------------
# 2. Read inputs
# ------------------------------------------------------------------------------

areas <- read_csv(area_file, show_col_types = FALSE)
area_summary <- read_csv(area_summary_file, show_col_types = FALSE)
breadth <- read_csv(breadth_file, show_col_types = FALSE)
stars03a <- read_csv(stars03a_file, show_col_types = FALSE)

input_dimensions <- tibble(
  INPUT = input_register$INPUT,
  ROWS = c(
    nrow(areas),
    nrow(area_summary),
    nrow(breadth),
    nrow(stars03a)
  ),
  COLUMNS = c(
    ncol(areas),
    ncol(area_summary),
    ncol(breadth),
    ncol(stars03a)
  )
)

cat("\n— INPUT DIMENSIONS —\n")
print(input_dimensions, n = Inf)

# ------------------------------------------------------------------------------
# 3. Validate canonical 51-area schema
# ------------------------------------------------------------------------------

required_area_fields <- c(
  "area_id",
  "area_no",
  "analytical_area_name",
  "area_type",
  "district",
  "political_parent",
  "display_area_id",
  "display_area_no",
  "display_area_name",
  "star_small_island_group",
  "special_assignment_required",
  "notes"
)

missing_area_fields <- setdiff(
  required_area_fields,
  names(areas)
)

if (length(missing_area_fields) > 0) {
  stop(
    "Missing required fields from 51-area framework: ",
    paste(missing_area_fields, collapse = ", ")
  )
}

area_framework <- areas %>%
  mutate(
    area_id = as.character(area_id),
    analytical_area_name = as.character(analytical_area_name),
    district = as.character(district),
    star_small_island_group = as.logical(star_small_island_group),
    special_assignment_required = as.logical(special_assignment_required)
  ) %>%
  arrange(area_no)

cat("\n— CANONICAL 51-AREA FRAMEWORK —\n")
print(
  area_framework %>%
    select(
      area_id,
      area_no,
      analytical_area_name,
      area_type,
      district,
      display_area_id,
      display_area_name,
      star_small_island_group,
      special_assignment_required
    ),
  n = Inf
)

# ------------------------------------------------------------------------------
# 4. Audit district vocabulary already encoded
# ------------------------------------------------------------------------------

district_profile <- area_framework %>%
  count(
    district,
    name = "N_ANALYTICAL_AREAS",
    sort = FALSE
  ) %>%
  arrange(district)

district_membership <- area_framework %>%
  filter(!is.na(district), district != "") %>%
  group_by(district) %>%
  summarise(
    N_ANALYTICAL_AREAS = n(),
    AREA_IDS = paste(area_id, collapse = "; "),
    AREA_NAMES = paste(analytical_area_name, collapse = "; "),
    .groups = "drop"
  ) %>%
  arrange(district)

cat("\n— DISTRICT PROFILE —\n")
print(district_profile, n = Inf)

cat("\n— DISTRICT MEMBERSHIP —\n")
print(district_membership, n = Inf)

# ------------------------------------------------------------------------------
# 5. Expected core Nakamura/chihō district vocabulary
# ------------------------------------------------------------------------------

expected_core_districts <- c(
  "Hokkaido",
  "Tohoku",
  "Kanto",
  "Chubu",
  "Kinki",
  "Chugoku",
  "Shikoku",
  "Kyushu"
)

observed_districts <- sort(
  unique(
    area_framework$district[
      !is.na(area_framework$district) &
        area_framework$district != ""
    ]
  )
)

core_district_audit <- tibble(
  DISTRICT = expected_core_districts,
  PRESENT_IN_FRAMEWORK = expected_core_districts %in% observed_districts,
  STATUS = if_else(
    PRESENT_IN_FRAMEWORK,
    "PRESENT",
    "MISSING"
  )
)

additional_districts <- setdiff(
  observed_districts,
  expected_core_districts
)

additional_district_audit <- tibble(
  DISTRICT = additional_districts,
  STATUS = "ADDITIONAL_FRAMEWORK_DISTRICT",
  REQUIRES_REVIEW = TRUE
)

cat("\n— CORE DISTRICT AUDIT —\n")
print(core_district_audit, n = Inf)

cat("\n— ADDITIONAL DISTRICT VALUES —\n")

if (nrow(additional_district_audit) == 0) {
  cat("None.\n")
} else {
  print(additional_district_audit, n = Inf)
}

# ------------------------------------------------------------------------------
# 6. Special botanical-area audit
# ------------------------------------------------------------------------------

special_area_ids <- c(
  "JP47",
  "JP48",
  "JP49",
  "JP50",
  "JP51"
)

special_area_audit <- area_framework %>%
  filter(area_id %in% special_area_ids) %>%
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
  mutate(
    KEY_ROLE = case_when(
      area_id == "JP47" ~
        "Ryukyu: explicit small-island endemic branch",
      area_id == "JP48" ~
        "Izu: explicit small-island endemic branch",
      area_id == "JP49" ~
        "Ogasawara: explicit small-island endemic branch",
      area_id == "JP50" ~
        "Kazan: contemporary separate analytical area; Nakamura combines with Ogasawara botanical prefecture",
      area_id == "JP51" ~
        "Kuriles: relevant to external-range branch and district interpretation",
      TRUE ~ NA_character_
    ),
    DISTRICT_METHOD_STATUS =
      "REQUIRES_EXPLICIT_REVIEW_BEFORE_CLASSIFIER"
  )

cat("\n— SPECIAL BOTANICAL AREAS JP47–JP51 —\n")
print(special_area_audit, n = Inf)

# ------------------------------------------------------------------------------
# 7. Display-area structure
# ------------------------------------------------------------------------------

display_area_profile <- area_framework %>%
  distinct(
    display_area_id,
    display_area_no,
    display_area_name
  ) %>%
  arrange(display_area_no)

display_crosswalk <- area_framework %>%
  select(
    area_id,
    area_no,
    analytical_area_name,
    display_area_id,
    display_area_no,
    display_area_name
  ) %>%
  arrange(display_area_no, area_no)

cat("\n— DISPLAY-AREA PROFILE —\n")
cat(
  "Unique analytical areas:",
  n_distinct(area_framework$area_id),
  "\n"
)
cat(
  "Unique display areas:",
  n_distinct(area_framework$display_area_id),
  "\n"
)

cat("\n— ANALYTICAL → DISPLAY AREA CROSSWALK —\n")
print(display_crosswalk, n = Inf)

# ------------------------------------------------------------------------------
# 8. Existing distribution output schemas
# ------------------------------------------------------------------------------

cat("\n— TAXON-AREA SUMMARY FIELDS —\n")
print(tibble(FIELD = names(area_summary)), n = Inf)

cat("\n— TAXON DISTRIBUTION BREADTH FIELDS —\n")
print(tibble(FIELD = names(breadth)), n = Inf)

cat("\n— STARS 03a FIELDS —\n")
print(tibble(FIELD = names(stars03a)), n = Inf)

# ------------------------------------------------------------------------------
# 9. Link botanical-area summary to canonical geography where possible
# ------------------------------------------------------------------------------

area_summary_id_candidates <- c(
  "BOTANICAL_AREA_ID",
  "botanical_area_id",
  "area_id"
)

area_summary_id_field <- area_summary_id_candidates[
  area_summary_id_candidates %in% names(area_summary)
][1]

if (is.na(area_summary_id_field)) {
  area_summary_geography <- tibble(
    STATUS = "NO_AREA_ID_FIELD_DETECTED"
  )
  area_summary_geography_profile <- tibble(
    STATUS = "NOT_EVALUATED"
  )
} else {
  area_summary_geography <- area_summary %>%
    mutate(
      area_id = as.character(
        .data[[area_summary_id_field]]
      )
    ) %>%
    left_join(
      area_framework %>%
        select(
          area_id,
          analytical_area_name,
          district,
          star_small_island_group,
          special_assignment_required
        ),
      by = "area_id"
    )
  
  area_summary_geography_profile <- area_summary_geography %>%
    summarise(
      N_ROWS = n(),
      N_AREA_IDS = n_distinct(area_id),
      N_MATCHED = sum(!is.na(analytical_area_name)),
      N_UNMATCHED = sum(is.na(analytical_area_name)),
      N_DISTRICTS = n_distinct(district, na.rm = TRUE)
    )
}

cat("\n— TAXON-AREA SUMMARY GEOGRAPHY LINKAGE —\n")
print(area_summary_geography_profile, n = Inf)

# ------------------------------------------------------------------------------
# 10. Audit Stars 03a geography fields
# ------------------------------------------------------------------------------

stars03a_district_fields <- names(stars03a)[
  str_detect(
    str_to_lower(names(stars03a)),
    "district"
  )
]

stars03a_area_fields <- names(stars03a)[
  str_detect(
    str_to_lower(names(stars03a)),
    "area|prefecture|geograph"
  )
]

stars03a_field_audit <- tibble(
  FIELD = names(stars03a),
  DISTRICT_RELATED =
    names(stars03a) %in% stars03a_district_fields,
  AREA_OR_PREFECTURE_RELATED =
    names(stars03a) %in% stars03a_area_fields
)

cat("\n— STARS 03a DISTRICT-RELATED FIELDS —\n")

if (length(stars03a_district_fields) == 0) {
  cat("None detected.\n")
} else {
  print(
    tibble(FIELD = stars03a_district_fields),
    n = Inf
  )
}

# ------------------------------------------------------------------------------
# 11. Methodological evidence register
# ------------------------------------------------------------------------------

methodological_audit <- tribble(
  ~DECISION_ID, ~KEY_TERM, ~EVIDENCE, ~STATUS, ~IMPLEMENT_IN_CLASSIFIER,
  "03L-001",
  "DISTRICT",
  "Nakamura thesis explicitly distinguishes broad Japanese districts from botanical prefectures; existing VPJD geography independently contains a district field using Hokkaido, Tohoku, Kanto, Chubu, Kinki, Chugoku, Shikoku and Kyushu terminology.",
  "PROVISIONAL_PENDING_FULL_MEMBERSHIP_AUDIT",
  FALSE,
  
  "03L-002",
  "DISTRICT_NOT_GUN",
  "Legacy diagnostic evidence did not support administrative gun as the Key district unit.",
  "SUPPORTED",
  FALSE,
  
  "03L-003",
  "DISTRICT_NOT_FLORISTIC_REGION",
  "No evidence supports conflating the Key district system with a separate floristic regionalisation.",
  "SUPPORTED",
  FALSE,
  
  "03L-004",
  "HOKKAIDO_SUBPREFECTURES",
  "Nakamura explicitly treats Hokkaido's 14 subprefectural units as prefectures for Key prefecture counting.",
  "METHOD_VALIDATED",
  FALSE,
  
  "03L-005",
  "SPECIAL_SMALL_ISLAND_GROUPS",
  "Ryukyu, Izu and Ogasawara are explicit small-island groups in the endemic branch.",
  "METHOD_VALIDATED",
  FALSE,
  
  "03L-006",
  "OGASAWARA_KAZAN",
  "Contemporary VPJD separates Ogasawara and Kazan analytically; Nakamura's botanical prefecture system combines Kazan with Ogasawara.",
  "METHOD_VALIDATED_RELATIONSHIP",
  FALSE,
  
  "03L-007",
  "SPECIAL_ISLAND_DISTRICT_COUNTING",
  "Exact contribution of Ryukyu, Izu, Ogasawara/Kazan and Kuriles to Japanese district counts must be confirmed before taxon-level district occupancy enters classification.",
  "UNRESOLVED",
  FALSE,
  
  "03L-008",
  "ALMOST_ALL_DISTRICTS",
  "Nakamura uses 'almost all districts', but a numeric operational threshold has not yet been justified.",
  "UNRESOLVED",
  FALSE
)

cat("\n— METHODOLOGICAL AUDIT —\n")
print(methodological_audit, n = Inf)

# ------------------------------------------------------------------------------
# 12. Remaining requirements
# ------------------------------------------------------------------------------

remaining_requirements <- tribble(
  ~REQUIREMENT_ID, ~REQUIREMENT, ~STATUS,
  "REQ-03L-01",
  "Verify all district memberships encoded in the 51-area framework against Nakamura Figure 2.4.",
  "REQUIRED",
  
  "REQ-03L-02",
  "Resolve whether Ryukyu contributes to a Japanese district count and, if so, how.",
  "REQUIRED",
  
  "REQ-03L-03",
  "Resolve whether Izu contributes to a Japanese district count and, if so, how.",
  "REQUIRED",
  
  "REQ-03L-04",
  "Resolve whether Ogasawara/Kazan contributes to a Japanese district count and, if so, how.",
  "REQUIRED",
  
  "REQ-03L-05",
  "Resolve treatment of Kuriles in Japanese district counting versus external-range evidence.",
  "REQUIRED",
  
  "REQ-03L-06",
  "Define 'almost all districts' reproducibly.",
  "REQUIRED",
  
  "REQ-03L-07",
  "Only after district geography is method-validated, calculate taxon-level district occupancy.",
  "BLOCKED"
)

cat("\n— REMAINING REQUIREMENTS —\n")
print(remaining_requirements, n = Inf)

# ------------------------------------------------------------------------------
# 13. Validation
# ------------------------------------------------------------------------------

validation <- tibble(
  CHECK = c(
    "51-area framework loaded",
    "51 unique analytical area IDs",
    "Required framework fields present",
    "Core eight district labels present",
    "No duplicate analytical area IDs",
    "JP47 present",
    "JP48 present",
    "JP49 present",
    "JP50 present",
    "JP51 present",
    "No district values modified",
    "No Star classifications assigned",
    "No taxon district counts created",
    "Special-area district treatment remains explicit",
    "Final classifier remains blocked"
  ),
  PASS = c(
    nrow(area_framework) == 51,
    n_distinct(area_framework$area_id) == 51,
    length(missing_area_fields) == 0,
    all(
      expected_core_districts %in%
        observed_districts
    ),
    !anyDuplicated(area_framework$area_id),
    "JP47" %in% area_framework$area_id,
    "JP48" %in% area_framework$area_id,
    "JP49" %in% area_framework$area_id,
    "JP50" %in% area_framework$area_id,
    "JP51" %in% area_framework$area_id,
    TRUE,
    TRUE,
    TRUE,
    all(
      special_area_audit$
        DISTRICT_METHOD_STATUS ==
        "REQUIRES_EXPLICIT_REVIEW_BEFORE_CLASSIFIER"
    ),
    remaining_requirements %>%
      filter(REQUIREMENT_ID == "REQ-03L-07") %>%
      pull(STATUS) == "BLOCKED"
  )
) %>%
  mutate(
    RESULT = if_else(
      PASS,
      "PASS",
      "FAIL"
    )
  )

cat("\n— VALIDATION —\n")
print(validation, n = Inf)

if (!all(validation$PASS)) {
  stop(
    "Stars 03l validation failed. Review FAIL checks before proceeding."
  )
}

# ------------------------------------------------------------------------------
# 14. Metadata
# ------------------------------------------------------------------------------

metadata <- tibble(
  MODULE = MODULE,
  VERSION = VERSION,
  RUN_DATE = as.character(RUN_DATE),
  PURPOSE =
    "Audit existing VPJD 51-area district geography against Nakamura Key requirements.",
  EXISTING_DISTRICT_FIELD_USED = TRUE,
  NEW_DISTRICT_CROSSWALK_CREATED = FALSE,
  DISTRICT_VALUES_MODIFIED = FALSE,
  TAXON_DISTRICT_COUNTS_CREATED = FALSE,
  STAR_CLASSIFICATIONS_CREATED = FALSE,
  SPECIAL_ISLAND_DISTRICT_TREATMENT_RESOLVED = FALSE,
  ALMOST_ALL_DISTRICTS_RESOLVED = FALSE,
  FINAL_STAR_CLASSIFIER_READY = FALSE,
  STATUS = "VALIDATED_DIAGNOSTIC"
)

# ------------------------------------------------------------------------------
# 15. Write outputs
# ------------------------------------------------------------------------------

write_csv(
  input_register,
  file.path(
    output_dir,
    "stars03l_input_register.csv"
  )
)

write_csv(
  input_dimensions,
  file.path(
    output_dir,
    "stars03l_input_dimensions.csv"
  )
)

write_csv(
  district_profile,
  file.path(
    output_dir,
    "stars03l_district_profile.csv"
  )
)

write_csv(
  district_membership,
  file.path(
    output_dir,
    "stars03l_district_membership.csv"
  )
)

write_csv(
  core_district_audit,
  file.path(
    output_dir,
    "stars03l_core_district_audit.csv"
  )
)

write_csv(
  additional_district_audit,
  file.path(
    output_dir,
    "stars03l_additional_district_audit.csv"
  )
)

write_csv(
  special_area_audit,
  file.path(
    output_dir,
    "stars03l_special_area_audit.csv"
  )
)

write_csv(
  display_crosswalk,
  file.path(
    output_dir,
    "stars03l_analytical_display_crosswalk.csv"
  )
)

write_csv(
  area_summary_geography,
  file.path(
    output_dir,
    "stars03l_area_summary_geography.csv"
  )
)

write_csv(
  area_summary_geography_profile,
  file.path(
    output_dir,
    "stars03l_area_summary_geography_profile.csv"
  )
)

write_csv(
  stars03a_field_audit,
  file.path(
    output_dir,
    "stars03l_stars03a_field_audit.csv"
  )
)

write_csv(
  methodological_audit,
  file.path(
    output_dir,
    "stars03l_methodological_audit.csv"
  )
)

write_csv(
  remaining_requirements,
  file.path(
    output_dir,
    "stars03l_remaining_requirements.csv"
  )
)

write_csv(
  validation,
  file.path(
    output_dir,
    "stars03l_validation.csv"
  )
)

write_csv(
  metadata,
  file.path(
    output_dir,
    "stars03l_metadata.csv"
  )
)

# ------------------------------------------------------------------------------
# 16. Final summary
# ------------------------------------------------------------------------------

cat("\n============================================================\n")
cat(MODULE, "v", VERSION, " COMPLETE\n", sep = "")
cat("============================================================\n")

cat(
  "Analytical botanical areas:",
  n_distinct(area_framework$area_id),
  "\n"
)

cat(
  "Display botanical areas:",
  n_distinct(area_framework$display_area_id),
  "\n"
)

cat(
  "Observed district values:",
  n_distinct(
    area_framework$district,
    na.rm = TRUE
  ),
  "\n"
)

cat(
  "Core eight districts present:",
  sum(
    expected_core_districts %in%
      observed_districts
  ),
  "/ 8\n"
)

cat(
  "Additional district values:",
  length(additional_districts),
  "\n"
)

cat(
  "Special botanical areas audited:",
  nrow(special_area_audit),
  "\n"
)

cat(
  "Taxon district counts created:",
  metadata$TAXON_DISTRICT_COUNTS_CREATED,
  "\n"
)

cat(
  "Star classifications created:",
  metadata$STAR_CLASSIFICATIONS_CREATED,
  "\n"
)

cat(
  "Final classifier ready:",
  metadata$FINAL_STAR_CLASSIFIER_READY,
  "\n"
)

cat(
  "Validation:",
  sum(validation$PASS),
  "/",
  nrow(validation),
  "PASS\n"
)

cat(
  "Outputs:",
  output_dir,
  "\n"
)

cat("============================================================\n")