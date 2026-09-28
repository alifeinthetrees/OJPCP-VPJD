# ==============================================================================
# OJPCP / VPJD
# Stars 03m — Resolve special-island district treatment
# Version: 0.1.0
#
# PURPOSE
#   Formalise the methodological status of the five exceptional Japanese
#   geographical units relevant to Nakamura's Key to Stars:
#     - Ryukyu
#     - Izu
#     - Ogasawara
#     - Kazan
#     - Kuriles
#
#   Stars 03l established that the contemporary 51-area framework contains the
#   eight core Japanese geographical regions (chihō):
#     Hokkaido, Tohoku, Kanto, Chubu, Kinki, Chugoku, Shikoku, Kyushu.
#
#   This module asks a narrower question:
#     How should JP47-JP51 interact with Nakamura's JAPAN DISTRICT COUNT?
#
# IMPORTANT
#   - NO Stars are assigned.
#   - NO taxon district counts are calculated.
#   - NO geography is modified.
#   - NO unresolved island treatment is inferred.
#   - Existing framework values are evidence, not methodological authority.
#   - "Almost all districts" remains a separate unresolved criterion.
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(tidyr)
  library(tibble)
  library(here)
})

MODULE <- "Stars 03m"
VERSION <- "0.1.0"
RUN_DATE <- Sys.Date()

cat("\n============================================================\n")
cat(MODULE, " — Resolve special-island district treatment\n", sep = "")
cat("Version: ", VERSION, "\n", sep = "")
cat("Run date: ", as.character(RUN_DATE), "\n", sep = "")
cat("============================================================\n\n")

# ------------------------------------------------------------------------------
# 1. Paths
# ------------------------------------------------------------------------------

area_file <- here(
  "outputs", "tables", "geography", "botanical_area_framework",
  "japan_botanical_areas_51.csv"
)

audit03l_file <- here(
  "outputs", "tables", "stars", "district_definition_audit",
  "stars03l_special_area_audit.csv"
)

district03l_file <- here(
  "outputs", "tables", "stars", "district_definition_audit",
  "stars03l_district_membership.csv"
)

method03l_file <- here(
  "outputs", "tables", "stars", "district_definition_audit",
  "stars03l_methodological_audit.csv"
)

requirements03l_file <- here(
  "outputs", "tables", "stars", "district_definition_audit",
  "stars03l_remaining_requirements.csv"
)

output_dir <- here(
  "outputs", "tables", "stars", "special_island_district_treatment"
)

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

input_register <- tibble(
  INPUT = c(
    "51-area botanical framework",
    "Stars 03l special-area audit",
    "Stars 03l district membership",
    "Stars 03l methodological audit",
    "Stars 03l remaining requirements"
  ),
  PATH = c(
    area_file,
    audit03l_file,
    district03l_file,
    method03l_file,
    requirements03l_file
  )
) %>%
  mutate(EXISTS = file.exists(PATH))

cat("— INPUT AVAILABILITY —\n")
print(input_register, n = Inf)

if (!all(input_register$EXISTS)) {
  stop("Required Stars 03m input file(s) missing.")
}

# ------------------------------------------------------------------------------
# 2. Read inputs
# ------------------------------------------------------------------------------

areas <- read_csv(area_file, show_col_types = FALSE)
audit03l <- read_csv(audit03l_file, show_col_types = FALSE)
district03l <- read_csv(district03l_file, show_col_types = FALSE)
method03l <- read_csv(method03l_file, show_col_types = FALSE)
requirements03l <- read_csv(requirements03l_file, show_col_types = FALSE)

input_dimensions <- tibble(
  INPUT = input_register$INPUT,
  ROWS = c(
    nrow(areas),
    nrow(audit03l),
    nrow(district03l),
    nrow(method03l),
    nrow(requirements03l)
  ),
  COLUMNS = c(
    ncol(areas),
    ncol(audit03l),
    ncol(district03l),
    ncol(method03l),
    ncol(requirements03l)
  )
)

cat("\n— INPUT DIMENSIONS —\n")
print(input_dimensions, n = Inf)

# ------------------------------------------------------------------------------
# 3. Validate framework
# ------------------------------------------------------------------------------

required_fields <- c(
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

missing_fields <- setdiff(required_fields, names(areas))

if (length(missing_fields) > 0) {
  stop(
    "Missing required geography fields: ",
    paste(missing_fields, collapse = ", ")
  )
}

areas <- areas %>%
  mutate(
    area_id = as.character(area_id),
    district = as.character(district),
    display_area_id = as.character(display_area_id),
    star_small_island_group = as.logical(star_small_island_group),
    special_assignment_required = as.logical(special_assignment_required)
  ) %>%
  arrange(area_no)

special_ids <- c("JP47", "JP48", "JP49", "JP50", "JP51")

special <- areas %>%
  filter(area_id %in% special_ids) %>%
  arrange(area_no)

if (nrow(special) != 5) {
  stop("Expected exactly five special analytical areas JP47-JP51.")
}

# ------------------------------------------------------------------------------
# 4. Establish the core district system
# ------------------------------------------------------------------------------

core_districts <- c(
  "Hokkaido",
  "Tohoku",
  "Kanto",
  "Chubu",
  "Kinki",
  "Chugoku",
  "Shikoku",
  "Kyushu"
)

core_framework <- areas %>%
  filter(district %in% core_districts) %>%
  select(
    area_id,
    area_no,
    analytical_area_name,
    district
  )

core_profile <- core_framework %>%
  group_by(district) %>%
  summarise(
    N_ANALYTICAL_AREAS = n(),
    AREA_IDS = paste(area_id, collapse = "; "),
    AREA_NAMES = paste(analytical_area_name, collapse = "; "),
    .groups = "drop"
  ) %>%
  mutate(
    DISTRICT_ORDER = match(district, core_districts)
  ) %>%
  arrange(DISTRICT_ORDER) %>%
  select(-DISTRICT_ORDER)

cat("\n— CORE JAPANESE DISTRICT SYSTEM —\n")
print(core_profile, n = Inf)

cat("\nCore district count:", length(core_districts), "\n")

# ------------------------------------------------------------------------------
# 5. Profile the five exceptional units
# ------------------------------------------------------------------------------

special_profile <- special %>%
  transmute(
    AREA_ID = area_id,
    AREA_NO = area_no,
    AREA_NAME = analytical_area_name,
    FRAMEWORK_DISTRICT = district,
    POLITICAL_PARENT = political_parent,
    DISPLAY_AREA_ID = display_area_id,
    DISPLAY_AREA_NAME = display_area_name,
    STAR_SMALL_ISLAND_GROUP = star_small_island_group,
    SPECIAL_ASSIGNMENT_REQUIRED = special_assignment_required,
    NOTES = notes
  )

cat("\n— SPECIAL-AREA FRAMEWORK PROFILE —\n")
print(special_profile, n = Inf)

# ------------------------------------------------------------------------------
# 6. Separate three geographical concepts
# ------------------------------------------------------------------------------

concept_register <- tribble(
  ~CONCEPT_ID, ~CONCEPT, ~DEFINITION, ~STATUS,
  "GEO-01",
  "ANALYTICAL_AREA",
  "Contemporary VPJD unit used to retain geographically explicit occurrence evidence.",
  "ESTABLISHED",
  "GEO-02",
  "DISPLAY_AREA",
  "Reporting/display unit; may combine analytical units where required by the botanical-geography framework.",
  "ESTABLISHED",
  "GEO-03",
  "KEY_DISTRICT",
  "Broad Japanese geographical region used by Nakamura's Key to measure within-Japan distributional breadth.",
  "PROVISIONAL_METHOD_VALIDATED",
  "GEO-04",
  "BOTANICAL_PREFECTURE",
  "Botanical distribution unit used by Nakamura for prefecture-count thresholds; distinct from Key district.",
  "ESTABLISHED",
  "GEO-05",
  "SMALL_ISLAND_GROUP",
  "Ryukyu, Izu and Ogasawara groups explicitly used by the endemic small-island branch of the Key.",
  "METHOD_VALIDATED"
)

cat("\n— GEOGRAPHICAL CONCEPT REGISTER —\n")
print(concept_register, n = Inf)

# ------------------------------------------------------------------------------
# 7. Special-island evidence matrix
# ------------------------------------------------------------------------------

# IMPORTANT:
# KEY_DISTRICT_TREATMENT remains unresolved unless the existing evidence
# establishes the relationship. This table does not infer missing decisions.

island_evidence <- tibble(
  AREA_ID = c("JP47", "JP48", "JP49", "JP50", "JP51"),
  AREA_NAME = c(
    "Ryukyu Islands",
    "Izu Islands",
    "Ogasawara Islands",
    "Kazan Islands",
    "Kuriles"
  ),
  FRAMEWORK_DISTRICT = c(
    "Ryukyu",
    "Izu",
    "Ogasawara",
    "Kazan",
    "Kuriles"
  ),
  EXPLICIT_SMALL_ISLAND_KEY_GROUP = c(
    TRUE, TRUE, TRUE, FALSE, FALSE
  ),
  CONTEMPORARY_ANALYTICAL_AREA = rep(TRUE, 5),
  BOTANICAL_PREFECTURE_RELATIONSHIP = c(
    "Ryukyu botanical unit retained separately",
    "Izu botanical unit retained separately",
    "Ogasawara botanical unit",
    "Combined with Ogasawara for display/botanical-prefecture treatment",
    "Kuriles botanical unit retained separately"
  ),
  KEY_DISTRICT_TREATMENT = rep(NA_character_, 5),
  KEY_DISTRICT_COUNTABLE = rep(NA, 5),
  KEY_DISTRICT_PARENT = rep(NA_character_, 5),
  METHOD_STATUS = rep("UNRESOLVED", 5),
  REASON = c(
    "Ryukyu is explicitly important to the small-island endemic branch, but this does not by itself establish how it contributes to district count.",
    "Izu is explicitly important to the small-island endemic branch, but this does not by itself establish how it contributes to district count.",
    "Ogasawara is explicitly important to the small-island endemic branch, but this does not by itself establish how it contributes to district count.",
    "Kazan is analytically separate but combined with Ogasawara in the display/botanical-prefecture structure; district treatment still requires explicit justification.",
    "Kuriles participates in the Key's external-range logic; its role in within-Japan district counting must therefore be established independently."
  )
)

cat("\n— SPECIAL-ISLAND EVIDENCE MATRIX —\n")
print(island_evidence, n = Inf)

# ------------------------------------------------------------------------------
# 8. Evaluate what can and cannot yet be concluded
# ------------------------------------------------------------------------------

decision_register <- tribble(
  ~DECISION_ID, ~KEY_TERM, ~DECISION, ~STATUS, ~RATIONALE,
  "03M-001",
  "DISTRICT",
  "District refers to a broad Japanese geographical region (chihō), distinct from botanical prefecture, administrative gun and floristic regionalisation.",
  "PROVISIONAL_METHOD_VALIDATED",
  "Nakamura terminology plus legacy and contemporary geography converge on the eight broad Japanese macro-regions.",
  "03M-002",
  "CORE_DISTRICT_SET",
  "Hokkaido, Tohoku, Kanto, Chubu, Kinki, Chugoku, Shikoku and Kyushu form the established core district set.",
  "METHOD_VALIDATED_PENDING_SOURCE_TRANSCRIPTION",
  "All eight are present coherently in the existing geography and match the reconstructed Nakamura district interpretation.",
  "03M-003",
  "RYUKYU_DISTRICT_TREATMENT",
  NA_character_,
  "UNRESOLVED",
  "Small-island status does not determine district-count treatment.",
  "03M-004",
  "IZU_DISTRICT_TREATMENT",
  NA_character_,
  "UNRESOLVED",
  "Small-island status does not determine district-count treatment.",
  "03M-005",
  "OGASAWARA_DISTRICT_TREATMENT",
  NA_character_,
  "UNRESOLVED",
  "Small-island status does not determine district-count treatment.",
  "03M-006",
  "KAZAN_DISTRICT_TREATMENT",
  NA_character_,
  "UNRESOLVED",
  "Contemporary analytical separation and botanical-prefecture combination do not independently establish district treatment.",
  "03M-007",
  "KURILES_DISTRICT_TREATMENT",
  NA_character_,
  "UNRESOLVED",
  "External-range use in the Key must not be conflated with within-Japan district counting.",
  "03M-008",
  "ALMOST_ALL_DISTRICTS",
  NA_character_,
  "UNRESOLVED",
  "Requires a defensible operational threshold after the complete district denominator is established."
)

cat("\n— DECISION REGISTER —\n")
print(decision_register, n = Inf)

# ------------------------------------------------------------------------------
# 9. Explicit prohibited assumptions
# ------------------------------------------------------------------------------

prohibited_assumptions <- tribble(
  ~ASSUMPTION_ID, ~PROHIBITED_ASSUMPTION, ~RATIONALE,
  "PA-03M-01",
  "Treat every value in the existing district field as automatically being a Nakamura Key district.",
  "The field was created before the Key district methodology was fully resolved.",
  "PA-03M-02",
  "Count Ryukyu as a district solely because district == 'Ryukyu' in the contemporary framework.",
  "Existing implementation is not methodological authority.",
  "PA-03M-03",
  "Count Izu as a district solely because district == 'Izu'.",
  "Existing implementation is not methodological authority.",
  "PA-03M-04",
  "Count Ogasawara as a district solely because district == 'Ogasawara'.",
  "Existing implementation is not methodological authority.",
  "PA-03M-05",
  "Count Kazan as a district solely because district == 'Kazan'.",
  "Analytical separation does not establish Key treatment.",
  "PA-03M-06",
  "Count Kuriles as a Japanese district solely because it exists in the 51-area framework.",
  "Its Key role overlaps with external-range assessment and requires explicit interpretation.",
  "PA-03M-07",
  "Assign Izu to Kanto merely because its political parent is Tokyo.",
  "Political administration and Key district geography are separate concepts.",
  "PA-03M-08",
  "Assign Ogasawara or Kazan to Kanto merely because their political parent is Tokyo.",
  "Political administration and Key district geography are separate concepts.",
  "PA-03M-09",
  "Assign Ryukyu to Kyushu merely because part of its political geography is Kagoshima.",
  "Political administration and Key district geography are separate concepts.",
  "PA-03M-10",
  "Assign Kuriles to Hokkaido merely because of geographical or administrative association.",
  "The Key's own treatment must determine the relationship.",
  "PA-03M-11",
  "Define 'almost all districts' from an assumed denominator.",
  "The complete district set must be method-validated first."
)

cat("\n— PROHIBITED ASSUMPTIONS —\n")
print(prohibited_assumptions, n = Inf)

# ------------------------------------------------------------------------------
# 10. Source-evidence requirements
# ------------------------------------------------------------------------------

evidence_requirements <- tribble(
  ~EVIDENCE_ID, ~TARGET, ~QUESTION, ~PREFERRED_EVIDENCE, ~STATUS,
  "EV-03M-01",
  "Ryukyu",
  "Does Nakamura treat Ryukyu as an independent district, part of another district, or outside the ordinary district denominator?",
  "Nakamura thesis geography figure/text and original Key context",
  "REQUIRED",
  "EV-03M-02",
  "Izu",
  "Does Nakamura treat Izu as an independent district, part of another district, or outside the ordinary district denominator?",
  "Nakamura thesis geography figure/text and original Key context",
  "REQUIRED",
  "EV-03M-03",
  "Ogasawara",
  "Does Nakamura treat Ogasawara as an independent district, part of another district, or outside the ordinary district denominator?",
  "Nakamura thesis geography figure/text and original Key context",
  "REQUIRED",
  "EV-03M-04",
  "Kazan",
  "Is Kazan subsumed within Ogasawara for all Key geography, or only botanical-prefecture/display treatment?",
  "Nakamura thesis geography figure/text and original Key context",
  "REQUIRED",
  "EV-03M-05",
  "Kuriles",
  "Are the Kuriles counted within Japanese districts, used only as an external-range unit, or treated differently by branch?",
  "Nakamura thesis geography figure/text and original Key context",
  "REQUIRED",
  "EV-03M-06",
  "Almost all districts",
  "What district denominator and threshold were intended by 'almost all districts'?",
  "Nakamura thesis text, Key figure, associated explanatory material",
  "REQUIRED"
)

cat("\n— SOURCE-EVIDENCE REQUIREMENTS —\n")
print(evidence_requirements, n = Inf)

# ------------------------------------------------------------------------------
# 11. Readiness gate
# ------------------------------------------------------------------------------

island_decisions_resolved <- all(
  island_evidence$METHOD_STATUS == "METHOD_VALIDATED"
)

almost_all_resolved <- decision_register %>%
  filter(KEY_TERM == "ALMOST_ALL_DISTRICTS") %>%
  pull(STATUS) %>%
  identical("METHOD_VALIDATED")

readiness <- tibble(
  COMPONENT = c(
    "District concept",
    "Eight core district identities",
    "Ryukyu district treatment",
    "Izu district treatment",
    "Ogasawara district treatment",
    "Kazan district treatment",
    "Kuriles district treatment",
    "Complete district denominator",
    "Almost-all-districts threshold",
    "Taxon-level district counting",
    "Final Star classifier"
  ),
  READY = c(
    TRUE,
    TRUE,
    FALSE,
    FALSE,
    FALSE,
    FALSE,
    FALSE,
    FALSE,
    FALSE,
    FALSE,
    FALSE
  )
)

cat("\n— READINESS GATE —\n")
print(readiness, n = Inf)

# ------------------------------------------------------------------------------
# 12. Validation
# ------------------------------------------------------------------------------

validation <- tibble(
  CHECK = c(
    "51-area framework loaded",
    "Exactly five special areas present",
    "Eight core district labels present",
    "JP47 Ryukyu present",
    "JP48 Izu present",
    "JP49 Ogasawara present",
    "JP50 Kazan present",
    "JP51 Kuriles present",
    "Ryukyu flagged as small-island group",
    "Izu flagged as small-island group",
    "Ogasawara flagged as small-island group",
    "Kazan not independently flagged as small-island group",
    "Kazan display area combined with Ogasawara",
    "No special-island district treatment inferred",
    "No taxon district counts calculated",
    "No Star classifications assigned",
    "Almost-all criterion remains unresolved",
    "Final classifier remains blocked"
  ),
  PASS = c(
    nrow(areas) == 51,
    nrow(special) == 5,
    all(core_districts %in% areas$district),
    "JP47" %in% special$area_id,
    "JP48" %in% special$area_id,
    "JP49" %in% special$area_id,
    "JP50" %in% special$area_id,
    "JP51" %in% special$area_id,
    special$star_small_island_group[special$area_id == "JP47"] %in% TRUE,
    special$star_small_island_group[special$area_id == "JP48"] %in% TRUE,
    special$star_small_island_group[special$area_id == "JP49"] %in% TRUE,
    special$star_small_island_group[special$area_id == "JP50"] %in% FALSE,
    special$display_area_id[special$area_id == "JP50"] ==
      special$display_area_id[special$area_id == "JP49"],
    all(is.na(island_evidence$KEY_DISTRICT_TREATMENT)),
    TRUE,
    TRUE,
    !almost_all_resolved,
    !all(readiness$READY)
  )
) %>%
  mutate(RESULT = if_else(PASS, "PASS", "FAIL"))

cat("\n— VALIDATION —\n")
print(validation, n = Inf)

if (!all(validation$PASS)) {
  stop("Stars 03m validation failed. Review FAIL checks before proceeding.")
}

# ------------------------------------------------------------------------------
# 13. Metadata
# ------------------------------------------------------------------------------

metadata <- tibble(
  MODULE = MODULE,
  VERSION = VERSION,
  RUN_DATE = as.character(RUN_DATE),
  PURPOSE =
    "Resolve methodological requirements for special-island treatment in Nakamura district counting.",
  CORE_DISTRICT_COUNT = length(core_districts),
  SPECIAL_AREAS_REVIEWED = nrow(special),
  SPECIAL_ISLAND_DISTRICT_TREATMENT_RESOLVED = island_decisions_resolved,
  ALMOST_ALL_DISTRICTS_RESOLVED = almost_all_resolved,
  TAXON_DISTRICT_COUNTS_CREATED = FALSE,
  STAR_CLASSIFICATIONS_CREATED = FALSE,
  GEOGRAPHY_MODIFIED = FALSE,
  FINAL_STAR_CLASSIFIER_READY = all(readiness$READY),
  STATUS = "VALIDATED_METHODOLOGICAL_DIAGNOSTIC"
)

# ------------------------------------------------------------------------------
# 14. Write outputs
# ------------------------------------------------------------------------------

write_csv(
  input_register,
  file.path(output_dir, "stars03m_input_register.csv")
)

write_csv(
  input_dimensions,
  file.path(output_dir, "stars03m_input_dimensions.csv")
)

write_csv(
  core_profile,
  file.path(output_dir, "stars03m_core_district_profile.csv")
)

write_csv(
  special_profile,
  file.path(output_dir, "stars03m_special_area_profile.csv")
)

write_csv(
  concept_register,
  file.path(output_dir, "stars03m_geographical_concept_register.csv")
)

write_csv(
  island_evidence,
  file.path(output_dir, "stars03m_special_island_evidence.csv")
)

write_csv(
  decision_register,
  file.path(output_dir, "stars03m_decision_register.csv")
)

write_csv(
  prohibited_assumptions,
  file.path(output_dir, "stars03m_prohibited_assumptions.csv")
)

write_csv(
  evidence_requirements,
  file.path(output_dir, "stars03m_evidence_requirements.csv")
)

write_csv(
  readiness,
  file.path(output_dir, "stars03m_readiness_gate.csv")
)

write_csv(
  validation,
  file.path(output_dir, "stars03m_validation.csv")
)

write_csv(
  metadata,
  file.path(output_dir, "stars03m_metadata.csv")
)

# ------------------------------------------------------------------------------
# 15. Final summary
# ------------------------------------------------------------------------------

cat("\n============================================================\n")
cat(MODULE, " v", VERSION, " COMPLETE\n", sep = "")
cat("============================================================\n")
cat("Core Japanese districts:", length(core_districts), "\n")
cat("Special areas reviewed:", nrow(special), "\n")
cat(
  "Special-island district treatments resolved:",
  sum(island_evidence$METHOD_STATUS == "METHOD_VALIDATED"),
  "/ 5\n"
)
cat(
  "Special-island district treatment complete:",
  island_decisions_resolved,
  "\n"
)
cat(
  "'Almost all districts' resolved:",
  almost_all_resolved,
  "\n"
)
cat("Taxon district counts created: FALSE\n")
cat("Star classifications created: FALSE\n")
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
cat("Outputs:", output_dir, "\n")
cat("============================================================\n")