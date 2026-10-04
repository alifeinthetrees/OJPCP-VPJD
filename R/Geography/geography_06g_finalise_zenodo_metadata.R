# ==============================================================================
# VPJD GEOGRAPHY MODULE
# geography_06g_finalise_zenodo_metadata.R
#
# PURPOSE
# -------
# Finalise publication and Zenodo metadata for:
#
#   Vascular Plants of Japan Database (VPJD)
#   Geography Release v1.0.0
#
# PREREQUISITE
# ------------
# geography_06f_audit_release_package.R
#
# with:
#
#   DATA_RELEASE_READY = TRUE
#
# IMPORTANT
# ---------
# This script performs ADMINISTRATIVE FINALISATION ONLY.
#
# It MUST NOT modify:
#
#   * VPJD Taxonomic Release v1.0.0
#   * canonical geographic vocabulary
#   * taxon geographic distribution
#   * taxon geographic evidence
#   * 06f audit products
#
# This version deliberately avoids [[ indexing throughout.
#
# ==============================================================================


# ------------------------------------------------------------------------------
# 01. RELEASE CONSTANTS
# ------------------------------------------------------------------------------

RELEASE_VERSION <- "1.0.0"

RELEASE_NAME <- "VPJD Geography v1.0.0"

RELEASE_TITLE <- paste0(
  "Vascular Plants of Japan Database (VPJD): ",
  "Geography Release v",
  RELEASE_VERSION
)

SCRIPT_NAME <- "geography_06g_finalise_zenodo_metadata.R"

BUILD_DATE <- format(
  Sys.Date(),
  "%Y-%m-%d"
)


# ------------------------------------------------------------------------------
# 02. HUMAN-CONFIRMED PUBLICATION METADATA
# ------------------------------------------------------------------------------

# ---- Creator ---------------------------------------------------------------

CREATOR_NAME <- "Ben Jones"

CREATOR_AFFILIATION <- paste(
  "University of Oxford Botanic Garden and Arboretum,",
  "University of Oxford"
)

CREATOR_ORCID <- "0000-0003-1430-004X"


# ---- Contact ---------------------------------------------------------------

CONTACT_NAME <- "Ben Jones"

CONTACT_AFFILIATION <- CREATOR_AFFILIATION

CONTACT_EMAIL <- "ben.jones@obg.ox.ac.uk"


# ---- Licence ---------------------------------------------------------------

RELEASE_LICENCE <- "CC-BY-4.0"


# ---- Existing VPJD Taxonomic Release ---------------------------------------

TAXONOMIC_RELEASE_VERSION <- "1.0.0"

TAXONOMIC_RELEASE_TITLE <- paste0(
  "Vascular Plants of Japan Database (VPJD): ",
  "Taxonomic Release v",
  TAXONOMIC_RELEASE_VERSION
)

TAXONOMIC_RELEASE_DOI <- "10.5281/zenodo.23017356"


# ---- Geography Release -----------------------------------------------------

# Reserved Zenodo DOI for VPJD Geography Release v1.0.0.

GEOGRAPHY_RELEASE_DOI <- "10.5281/zenodo.23111498"


# ---- Contributors ----------------------------------------------------------

CONTRIBUTORS <- data.frame(
  name = character(0),
  affiliation = character(0),
  orcid = character(0),
  contribution_type = character(0),
  stringsAsFactors = FALSE
)


# ---- Keywords --------------------------------------------------------------

RELEASE_KEYWORDS <- c(
  "Vascular Plants of Japan Database",
  "VPJD",
  "vascular plants",
  "Japan",
  "plant distribution",
  "botanical geography",
  "biodiversity",
  "flora",
  "plant conservation",
  "occurrence data",
  "WCVP",
  "TDWG",
  "Key Biodiversity Areas",
  "KBA"
)


# ------------------------------------------------------------------------------
# 03. RELEASE DESCRIPTION
# ------------------------------------------------------------------------------

RELEASE_DESCRIPTION <- paste(
  "The Vascular Plants of Japan Database (VPJD) Geography Release v1.0.0",
  "provides the geographic evidence layer associated with the VPJD",
  "Taxonomic Release v1.0.0.",
  "",
  "The release links the recognised VPJD taxonomic backbone to a canonical",
  "geographic vocabulary and a reproducible set of positive taxon-by-area",
  "distribution relationships for Japan.",
  "",
  "The release comprises 12,037 recognised vascular plant taxa,",
  "418 canonical geographic concepts, including 51 Japanese botanical areas",
  "and 367 WCVP/TDWG reference concepts, and 128,824 positive",
  "taxon-by-botanical-area relationships.",
  "",
  "Positive geographic evidence is available for 11,470 recognised taxa;",
  "567 recognised taxa currently have no positive geographic evidence",
  "in the release.",
  "",
  "The geographic evidence model provides a reproducible basis for analysing",
  "the distribution of the Japanese vascular flora and for subsequent",
  "conservation analyses, including plant-focused Key Biodiversity Area",
  "(KBA) assessment.",
  "",
  "The absence of a positive taxon-by-area relationship must not be",
  "interpreted as evidence of biological absence.",
  "",
  "This release represents the VPJD geographic evidence model only.",
  "It does not infer endemicity or rarity and does not apply the Nakamura",
  "Key to Stars or assign Star categories.",
  "",
  "VPJD Geography v1.0.0 should be used together with VPJD Taxonomic",
  "Release v1.0.0."
)


# ------------------------------------------------------------------------------
# 04. PROJECT PATHS
# ------------------------------------------------------------------------------

PROJECT_ROOT <- "I:/R/OJPCP/VPJD-OJPCP"

PUBLISHED_VPJD_ROOT <- "I:/R/Data/VPJD_v1.0.0"

DATA_ROOT <- file.path(
  PROJECT_ROOT,
  "data"
)

GEOGRAPHY_ROOT <- file.path(
  DATA_ROOT,
  "derived",
  "geography"
)

RELEASE_ROOT <- file.path(
  GEOGRAPHY_ROOT,
  "release_v1.0.0"
)

GEOGRAPHIC_VOCABULARY_ROOT <- file.path(
  RELEASE_ROOT,
  "canonical_geographic_units_frozen"
)

DISTRIBUTION_ROOT <- file.path(
  RELEASE_ROOT,
  "taxon_geographic_distribution"
)

METADATA_ROOT <- file.path(
  RELEASE_ROOT,
  "metadata"
)


# ------------------------------------------------------------------------------
# 05. 06f AUDIT PATHS
# ------------------------------------------------------------------------------

AUDIT_06F_ROOT <- file.path(
  PROJECT_ROOT,
  "audit",
  "geography",
  "release_v1.0.0",
  "06f_audit_release_package"
)

FINAL_06F_STATUS_FILE <- file.path(
  AUDIT_06F_ROOT,
  "geography_06f_release_status.csv"
)


# ------------------------------------------------------------------------------
# 06. 06g STAGING PATHS
# ------------------------------------------------------------------------------

STAGING_ROOT <- file.path(
  RELEASE_ROOT,
  "zenodo_staging"
)

STAGING_METADATA_ROOT <- file.path(
  STAGING_ROOT,
  "metadata"
)

AUDIT_06G_ROOT <- file.path(
  PROJECT_ROOT,
  "audit",
  "geography",
  "release_v1.0.0",
  "06g_finalise_zenodo_metadata"
)

dir.create(
  STAGING_ROOT,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  STAGING_METADATA_ROOT,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  AUDIT_06G_ROOT,
  recursive = TRUE,
  showWarnings = FALSE
)


# ------------------------------------------------------------------------------
# 07. EXISTING FROZEN RELEASE PRODUCTS
# ------------------------------------------------------------------------------

GEOGRAPHY_FILE <- file.path(
  GEOGRAPHIC_VOCABULARY_ROOT,
  "vpjd_geographic_units_v1.0.0.csv"
)

DISTRIBUTION_FILE <- file.path(
  DISTRIBUTION_ROOT,
  "vpjd_taxon_geographic_distribution_v1.0.0.csv"
)

EVIDENCE_FILE <- file.path(
  DISTRIBUTION_ROOT,
  "vpjd_taxon_geographic_evidence_v1.0.0.csv"
)

SOURCE_RELEASE_METADATA_FILE <- file.path(
  METADATA_ROOT,
  "vpjd_geography_release_metadata_v1.0.0.csv"
)

SOURCE_DATA_DICTIONARY_FILE <- file.path(
  METADATA_ROOT,
  "vpjd_geography_data_dictionary_v1.0.0.csv"
)

SOURCE_RELEASE_STATISTICS_FILE <- file.path(
  METADATA_ROOT,
  "vpjd_geography_release_statistics_v1.0.0.csv"
)

SOURCE_PROVENANCE_FILE <- file.path(
  METADATA_ROOT,
  "vpjd_geography_provenance_v1.0.0.csv"
)

SOURCE_SAFEGUARDS_FILE <- file.path(
  METADATA_ROOT,
  "vpjd_geography_methodological_safeguards_v1.0.0.csv"
)


# ------------------------------------------------------------------------------
# 08. 06g OUTPUT FILES
# ------------------------------------------------------------------------------

FINAL_METADATA_FILE <- file.path(
  STAGING_METADATA_ROOT,
  "vpjd_geography_zenodo_metadata_v1.0.0.csv"
)

FINAL_METADATA_TEXT_FILE <- file.path(
  STAGING_METADATA_ROOT,
  "vpjd_geography_zenodo_metadata_v1.0.0.txt"
)

FINAL_README_FILE <- file.path(
  STAGING_ROOT,
  "README_VPJD_Geography_v1.0.0.txt"
)

RELATIONSHIPS_FILE <- file.path(
  STAGING_METADATA_ROOT,
  "vpjd_geography_related_identifiers_v1.0.0.csv"
)

CONTRIBUTORS_FILE <- file.path(
  STAGING_METADATA_ROOT,
  "vpjd_geography_contributors_v1.0.0.csv"
)

KEYWORDS_FILE <- file.path(
  STAGING_METADATA_ROOT,
  "vpjd_geography_keywords_v1.0.0.csv"
)

METADATA_VALIDATION_FILE <- file.path(
  AUDIT_06G_ROOT,
  "geography_06g_metadata_validation.csv"
)

PLACEHOLDER_AUDIT_FILE <- file.path(
  AUDIT_06G_ROOT,
  "geography_06g_placeholder_audit.csv"
)

CHECKSUM_FILE <- file.path(
  AUDIT_06G_ROOT,
  "geography_06g_staging_sha256.csv"
)

FINAL_STATUS_FILE <- file.path(
  AUDIT_06G_ROOT,
  "geography_06g_zenodo_readiness.csv"
)

FINAL_REPORT_FILE <- file.path(
  AUDIT_06G_ROOT,
  "geography_06g_finalisation_report.txt"
)


# ------------------------------------------------------------------------------
# 09. HELPER FUNCTIONS
# ------------------------------------------------------------------------------

normalise_text <- function(x) {
  
  y <- as.character(x)
  
  y <- iconv(
    y,
    from = "",
    to = "UTF-8",
    sub = ""
  )
  
  y[is.na(y)] <- ""
  
  y <- trimws(y)
  
  empty_selector <- (
    y == "" |
      toupper(y) == "NA" |
      toupper(y) == "NULL"
  )
  
  empty_selector[is.na(empty_selector)] <- FALSE
  
  y[empty_selector] <- NA_character_
  
  return(y)
}


is_placeholder <- function(x) {
  
  y <- toupper(
    normalise_text(x)
  )
  
  if (
    length(y) == 0L ||
    all(is.na(y))
  ) {
    return(FALSE)
  }
  
  patterns <- c(
    "^PASTE_",
    "^TO_CONFIRM",
    "^TO ADD",
    "^TO_ADD",
    "^TO_BE_",
    "^UNRESOLVED$",
    "^TBC$",
    "^TBD$"
  )
  
  result <- FALSE
  
  for (pattern_index in seq_along(patterns)) {
    
    current_match <- grepl(
      patterns[pattern_index],
      y,
      perl = TRUE
    )
    
    current_match[is.na(current_match)] <- FALSE
    
    if (any(current_match)) {
      result <- TRUE
    }
  }
  
  return(result)
}


is_present <- function(x) {
  
  y <- normalise_text(x)
  
  if (
    length(y) == 0L ||
    all(is.na(y))
  ) {
    return(FALSE)
  }
  
  if (is_placeholder(y)) {
    return(FALSE)
  }
  
  return(TRUE)
}


is_valid_doi <- function(x) {
  
  y <- normalise_text(x)
  
  if (
    length(y) != 1L ||
    is.na(y)
  ) {
    return(FALSE)
  }
  
  if (is_placeholder(y)) {
    return(FALSE)
  }
  
  result <- grepl(
    "^10\\.[0-9]{4,9}/\\S+$",
    y,
    perl = TRUE
  )
  
  return(result)
}


is_valid_orcid <- function(x) {
  
  y <- normalise_text(x)
  
  if (
    length(y) != 1L ||
    is.na(y)
  ) {
    return(FALSE)
  }
  
  if (is_placeholder(y)) {
    return(FALSE)
  }
  
  result <- grepl(
    "^[0-9]{4}-[0-9]{4}-[0-9]{4}-[0-9]{3}[0-9X]$",
    y
  )
  
  return(result)
}


safe_read_csv <- function(path) {
  
  if (!file.exists(path)) {
    
    stop(
      paste0(
        "Required file not found:\n",
        path
      )
    )
  }
  
  result <- tryCatch(
    read.csv(
      path,
      stringsAsFactors = FALSE,
      check.names = FALSE,
      fileEncoding = "UTF-8"
    ),
    error = function(e) NULL
  )
  
  if (is.null(result)) {
    
    result <- tryCatch(
      read.csv(
        path,
        stringsAsFactors = FALSE,
        check.names = FALSE,
        fileEncoding = "UTF-8-BOM"
      ),
      error = function(e) NULL
    )
  }
  
  if (is.null(result)) {
    
    result <- read.csv(
      path,
      stringsAsFactors = FALSE,
      check.names = FALSE,
      fileEncoding = "latin1"
    )
  }
  
  return(result)
}


write_utf8_lines <- function(lines, path) {
  
  connection <- file(
    path,
    open = "w",
    encoding = "UTF-8"
  )
  
  writeLines(
    lines,
    connection,
    useBytes = TRUE
  )
  
  close(connection)
}


calculate_sha256 <- function(path) {
  
  if (!file.exists(path)) {
    return(NA_character_)
  }
  
  if (!requireNamespace(
    "digest",
    quietly = TRUE
  )) {
    return(NA_character_)
  }
  
  result <- digest::digest(
    file = path,
    algo = "sha256",
    serialize = FALSE
  )
  
  return(result)
}


to_logical <- function(x) {
  
  y <- toupper(
    normalise_text(x)
  )
  
  result <- y %in% c(
    "TRUE",
    "T",
    "1"
  )
  
  return(result)
}


# ------------------------------------------------------------------------------
# 10. START
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" VPJD GEOGRAPHY 06g - FINALISE ZENODO METADATA\n")
cat("============================================================\n\n")

cat(
  "Release:",
  RELEASE_NAME,
  "\n"
)

cat(
  "Build date:",
  BUILD_DATE,
  "\n"
)

cat(
  "Reserved Geography DOI:",
  GEOGRAPHY_RELEASE_DOI,
  "\n\n"
)


# ------------------------------------------------------------------------------
# 11. VERIFY 06f DATA-RELEASE GATE
# ------------------------------------------------------------------------------

cat(
  "Verifying 06f final release status...\n"
)

if (!file.exists(FINAL_06F_STATUS_FILE)) {
  
  stop(
    paste0(
      "06f final release status not found:\n",
      FINAL_06F_STATUS_FILE,
      "\n\n",
      "Run geography_06f_audit_release_package.R first."
    )
  )
}


status_06f <- safe_read_csv(
  FINAL_06F_STATUS_FILE
)


required_06f_fields <- c(
  "data_release_ready",
  "sha256_complete",
  "inherited_06d_gate",
  "inherited_06e_gate"
)


missing_06f_fields <- setdiff(
  required_06f_fields,
  names(status_06f)
)


if (length(missing_06f_fields) > 0L) {
  
  stop(
    paste(
      "06f release-status schema is incomplete.",
      "Missing fields:",
      paste(
        missing_06f_fields,
        collapse = ", "
      )
    )
  )
}


data_release_ready_06f <- to_logical(
  status_06f$data_release_ready[1]
)

sha256_complete_06f <- to_logical(
  status_06f$sha256_complete[1]
)

metadata_gate_06f <- to_logical(
  status_06f$inherited_06d_gate[1]
)

summary_gate_06f <- to_logical(
  status_06f$inherited_06e_gate[1]
)


if (!data_release_ready_06f) {
  
  stop(
    paste(
      "06g cannot proceed because the 06f data-release gate",
      "did not return DATA_RELEASE_READY = TRUE."
    )
  )
}


cat("06f DATA_RELEASE_READY: TRUE\n")

cat(
  "06f SHA-256 complete:",
  sha256_complete_06f,
  "\n"
)

cat(
  "06d inherited gate:",
  metadata_gate_06f,
  "\n"
)

cat(
  "06e inherited gate:",
  summary_gate_06f,
  "\n\n"
)


# ------------------------------------------------------------------------------
# 12. VERIFY SCIENTIFIC RELEASE PRODUCTS
# ------------------------------------------------------------------------------

scientific_files <- c(
  GEOGRAPHY_FILE,
  DISTRIBUTION_FILE,
  EVIDENCE_FILE,
  SOURCE_RELEASE_METADATA_FILE,
  SOURCE_DATA_DICTIONARY_FILE,
  SOURCE_RELEASE_STATISTICS_FILE,
  SOURCE_PROVENANCE_FILE,
  SOURCE_SAFEGUARDS_FILE
)


missing_scientific_files <- scientific_files[
  !file.exists(scientific_files)
]


if (length(missing_scientific_files) > 0L) {
  
  stop(
    paste(
      "Required frozen release products are missing:",
      paste(
        missing_scientific_files,
        collapse = "\n"
      ),
      sep = "\n\n"
    )
  )
}


cat(
  "Frozen scientific release products present: PASS\n\n"
)


# ------------------------------------------------------------------------------
# 13. VALIDATE HUMAN-CONFIRMED METADATA
# ------------------------------------------------------------------------------

cat(
  "Validating publication metadata...\n"
)


creator_name_valid <- is_present(
  CREATOR_NAME
)

creator_affiliation_valid <- is_present(
  CREATOR_AFFILIATION
)

creator_orcid_present <- is_present(
  CREATOR_ORCID
)

creator_orcid_valid <- (
  creator_orcid_present &&
    is_valid_orcid(
      CREATOR_ORCID
    )
)


contact_name_valid <- is_present(
  CONTACT_NAME
)

contact_affiliation_valid <- is_present(
  CONTACT_AFFILIATION
)

contact_email_valid <- (
  is_present(CONTACT_EMAIL) &&
    grepl(
      "^[^[:space:]@]+@[^[:space:]@]+\\.[^[:space:]@]+$",
      CONTACT_EMAIL
    )
)


licence_valid <- (
  is_present(RELEASE_LICENCE) &&
    RELEASE_LICENCE == "CC-BY-4.0"
)


taxonomic_doi_present <- is_present(
  TAXONOMIC_RELEASE_DOI
)

taxonomic_doi_valid <- (
  taxonomic_doi_present &&
    is_valid_doi(
      TAXONOMIC_RELEASE_DOI
    )
)


geography_doi_present <- is_present(
  GEOGRAPHY_RELEASE_DOI
)

geography_doi_valid <- (
  geography_doi_present &&
    is_valid_doi(
      GEOGRAPHY_RELEASE_DOI
    )
)


# ------------------------------------------------------------------------------
# 14. VALIDATE CONTRIBUTORS
# ------------------------------------------------------------------------------

contributors_valid <- TRUE


required_contributor_fields <- c(
  "name",
  "affiliation",
  "orcid",
  "contribution_type"
)


contributors_schema_valid <- all(
  required_contributor_fields %in%
    names(CONTRIBUTORS)
)


if (!contributors_schema_valid) {
  contributors_valid <- FALSE
}


if (
  contributors_schema_valid &&
  nrow(CONTRIBUTORS) > 0L
) {
  
  contributor_names <- normalise_text(
    CONTRIBUTORS$name
  )
  
  contributor_affiliations <- normalise_text(
    CONTRIBUTORS$affiliation
  )
  
  contributor_orcids <- normalise_text(
    CONTRIBUTORS$orcid
  )
  
  contributor_types <- normalise_text(
    CONTRIBUTORS$contribution_type
  )
  
  
  if (any(is.na(contributor_names))) {
    contributors_valid <- FALSE
  }
  
  if (any(is.na(contributor_affiliations))) {
    contributors_valid <- FALSE
  }
  
  if (any(is.na(contributor_types))) {
    contributors_valid <- FALSE
  }
  
  
  orcid_rows <- which(
    !is.na(contributor_orcids)
  )
  
  
  if (length(orcid_rows) > 0L) {
    
    for (orcid_index in orcid_rows) {
      
      current_orcid <- contributor_orcids[
        orcid_index
      ]
      
      if (!is_valid_orcid(current_orcid)) {
        contributors_valid <- FALSE
      }
    }
  }
}


# ------------------------------------------------------------------------------
# 15. METADATA VALIDATION TABLE
# ------------------------------------------------------------------------------

metadata_validation <- data.frame(
  field = c(
    "creator_name",
    "creator_affiliation",
    "creator_orcid",
    "contact_name",
    "contact_affiliation",
    "contact_email",
    "release_licence",
    "taxonomic_release_doi",
    "geography_release_doi",
    "contributors"
  ),
  required_before_deposit = c(
    TRUE,
    TRUE,
    TRUE,
    TRUE,
    TRUE,
    TRUE,
    TRUE,
    TRUE,
    TRUE,
    FALSE
  ),
  value_present = c(
    creator_name_valid,
    creator_affiliation_valid,
    creator_orcid_present,
    contact_name_valid,
    contact_affiliation_valid,
    is_present(CONTACT_EMAIL),
    is_present(RELEASE_LICENCE),
    taxonomic_doi_present,
    geography_doi_present,
    TRUE
  ),
  valid = c(
    creator_name_valid,
    creator_affiliation_valid,
    creator_orcid_valid,
    contact_name_valid,
    contact_affiliation_valid,
    contact_email_valid,
    licence_valid,
    taxonomic_doi_valid,
    geography_doi_valid,
    contributors_valid
  ),
  stringsAsFactors = FALSE
)


metadata_validation$passed <- (
  metadata_validation$valid &
    (
      metadata_validation$value_present |
        !metadata_validation$required_before_deposit
    )
)


write.csv(
  metadata_validation,
  METADATA_VALIDATION_FILE,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)


# ------------------------------------------------------------------------------
# 16. DISPLAY VALUES FOR STAGING
# ------------------------------------------------------------------------------

creator_orcid_display <- if (
  creator_orcid_valid
) {
  CREATOR_ORCID
} else {
  "UNRESOLVED"
}


taxonomic_doi_display <- if (
  taxonomic_doi_valid
) {
  TAXONOMIC_RELEASE_DOI
} else {
  "UNRESOLVED"
}


# ------------------------------------------------------------------------------
# 17. BUILD FINAL METADATA TABLE
# ------------------------------------------------------------------------------

final_metadata <- data.frame(
  field = c(
    "title",
    "dataset",
    "release",
    "version",
    "publication_date",
    "resource_type",
    "creator_name",
    "creator_affiliation",
    "creator_orcid",
    "contact_name",
    "contact_affiliation",
    "contact_email",
    "licence",
    "related_taxonomic_release",
    "related_taxonomic_release_version",
    "related_taxonomic_release_doi",
    "geography_release_doi",
    "description",
    "build_script",
    "build_date"
  ),
  value = c(
    RELEASE_TITLE,
    "Vascular Plants of Japan Database (VPJD)",
    "Geography Release",
    RELEASE_VERSION,
    BUILD_DATE,
    "Dataset",
    CREATOR_NAME,
    CREATOR_AFFILIATION,
    creator_orcid_display,
    CONTACT_NAME,
    CONTACT_AFFILIATION,
    CONTACT_EMAIL,
    RELEASE_LICENCE,
    TAXONOMIC_RELEASE_TITLE,
    TAXONOMIC_RELEASE_VERSION,
    taxonomic_doi_display,
    GEOGRAPHY_RELEASE_DOI,
    RELEASE_DESCRIPTION,
    SCRIPT_NAME,
    BUILD_DATE
  ),
  stringsAsFactors = FALSE
)


write.csv(
  final_metadata,
  FINAL_METADATA_FILE,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)


# ------------------------------------------------------------------------------
# 18. BUILD RELATED IDENTIFIERS
# ------------------------------------------------------------------------------

relationships <- data.frame(
  related_resource = character(0),
  identifier_type = character(0),
  identifier = character(0),
  relationship = character(0),
  stringsAsFactors = FALSE
)


if (taxonomic_doi_valid) {
  
  taxonomic_relationship <- data.frame(
    related_resource = TAXONOMIC_RELEASE_TITLE,
    identifier_type = "DOI",
    identifier = TAXONOMIC_RELEASE_DOI,
    relationship = "isSupplementTo",
    stringsAsFactors = FALSE
  )
  
  relationships <- rbind(
    relationships,
    taxonomic_relationship
  )
}


if (geography_doi_valid) {
  
  geography_relationship <- data.frame(
    related_resource = RELEASE_TITLE,
    identifier_type = "DOI",
    identifier = GEOGRAPHY_RELEASE_DOI,
    relationship = "isIdenticalTo",
    stringsAsFactors = FALSE
  )
  
  relationships <- rbind(
    relationships,
    geography_relationship
  )
}


write.csv(
  relationships,
  RELATIONSHIPS_FILE,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)


# ------------------------------------------------------------------------------
# 19. WRITE CONTRIBUTORS
# ------------------------------------------------------------------------------

write.csv(
  CONTRIBUTORS,
  CONTRIBUTORS_FILE,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)


# ------------------------------------------------------------------------------
# 20. WRITE KEYWORDS
# ------------------------------------------------------------------------------

keywords_table <- data.frame(
  keyword = RELEASE_KEYWORDS,
  stringsAsFactors = FALSE
)


write.csv(
  keywords_table,
  KEYWORDS_FILE,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)


# ------------------------------------------------------------------------------
# 21. BUILD HUMAN-READABLE ZENODO METADATA
# ------------------------------------------------------------------------------

zenodo_metadata_lines <- c(
  RELEASE_TITLE,
  "",
  paste0(
    "Version: ",
    RELEASE_VERSION
  ),
  "Resource type: Dataset",
  paste0(
    "Publication date: ",
    BUILD_DATE
  ),
  "",
  "CREATOR",
  "",
  paste0(
    "Name: ",
    CREATOR_NAME
  ),
  paste0(
    "Affiliation: ",
    CREATOR_AFFILIATION
  ),
  paste0(
    "ORCID: ",
    creator_orcid_display
  ),
  "",
  "CONTACT",
  "",
  paste0(
    "Name: ",
    CONTACT_NAME
  ),
  paste0(
    "Affiliation: ",
    CONTACT_AFFILIATION
  ),
  paste0(
    "Email: ",
    CONTACT_EMAIL
  ),
  "",
  "LICENCE",
  "",
  paste0(
    "Licence: ",
    RELEASE_LICENCE
  ),
  "",
  "RELATED TAXONOMIC RELEASE",
  "",
  paste0(
    "Title: ",
    TAXONOMIC_RELEASE_TITLE
  ),
  paste0(
    "DOI: ",
    taxonomic_doi_display
  ),
  "",
  "GEOGRAPHY RELEASE DOI",
  "",
  paste0(
    "DOI: ",
    GEOGRAPHY_RELEASE_DOI
  ),
  "",
  "DESCRIPTION",
  "",
  RELEASE_DESCRIPTION,
  "",
  "KEYWORDS",
  "",
  paste(
    RELEASE_KEYWORDS,
    collapse = "; "
  )
)


write_utf8_lines(
  zenodo_metadata_lines,
  FINAL_METADATA_TEXT_FILE
)


# ------------------------------------------------------------------------------
# 22. BUILD FINAL README
# ------------------------------------------------------------------------------

readme_lines <- c(
  "VASCULAR PLANTS OF JAPAN DATABASE (VPJD)",
  "GEOGRAPHY RELEASE v1.0.0",
  "",
  "README",
  "",
  paste0(
    "Release date: ",
    BUILD_DATE
  ),
  paste0(
    "DOI: ",
    GEOGRAPHY_RELEASE_DOI
  ),
  "",
  "OVERVIEW",
  "",
  RELEASE_DESCRIPTION,
  "",
  "RELEASE CONTENT",
  "",
  "Recognised taxa: 12,037",
  "Canonical geographic concepts: 418",
  "Japanese botanical areas: 51",
  "WCVP/TDWG reference concepts: 367",
  "Positive taxon x botanical-area relationships: 128,824",
  "Taxa with positive geographic evidence: 11,470",
  "Taxa without positive geographic evidence: 567",
  "",
  "CORE DATA PRODUCTS",
  "",
  basename(GEOGRAPHY_FILE),
  basename(DISTRIBUTION_FILE),
  basename(EVIDENCE_FILE),
  "",
  "INTERPRETATION",
  "",
  paste(
    "The taxon geographic distribution table records positive",
    "geographic evidence only."
  ),
  "",
  paste(
    "The absence of a taxon x area relationship must not be interpreted",
    "as evidence that the taxon is biologically absent from that area."
  ),
  "",
  "TAXONOMIC BASIS",
  "",
  TAXONOMIC_RELEASE_TITLE,
  paste0(
    "DOI: ",
    taxonomic_doi_display
  ),
  "",
  "METHODOLOGICAL BOUNDARY",
  "",
  paste(
    "This release represents the VPJD geographic evidence model.",
    "It does not infer biological absence, endemicity or rarity."
  ),
  "",
  paste(
    "The Nakamura Key to Stars is not applied in this release,",
    "and no Star categories are assigned."
  ),
  "",
  "CREATOR",
  "",
  CREATOR_NAME,
  CREATOR_AFFILIATION,
  paste0(
    "ORCID: ",
    creator_orcid_display
  ),
  "",
  "CONTACT",
  "",
  paste0(
    CONTACT_NAME,
    " - ",
    CONTACT_EMAIL
  ),
  "",
  "LICENCE",
  "",
  RELEASE_LICENCE,
  "",
  "GEOGRAPHY RELEASE DOI",
  "",
  GEOGRAPHY_RELEASE_DOI
)


write_utf8_lines(
  readme_lines,
  FINAL_README_FILE
)


# ------------------------------------------------------------------------------
# 23. PLACEHOLDER AUDIT - INITIALISE
# ------------------------------------------------------------------------------

placeholder_audit <- data.frame(
  file_name = character(0),
  location = character(0),
  placeholder = character(0),
  matched_text = character(0),
  stringsAsFactors = FALSE
)


placeholder_terms <- c(
  "PASTE_",
  "TO_CONFIRM",
  "TO CONFIRM",
  "TO_BE_ASSIGNED",
  "TO BE ASSIGNED",
  "TO_ADD",
  "TO ADD",
  "TO_BE_ADDED",
  "TO BE ADDED",
  "TO_BE_CONFIRMED",
  "TO BE CONFIRMED",
  "UNRESOLVED",
  "TBC",
  "TBD"
)


# ------------------------------------------------------------------------------
# 24. AUDIT TEXT FILES FOR PLACEHOLDERS
# ------------------------------------------------------------------------------

staged_text_files <- c(
  FINAL_METADATA_TEXT_FILE,
  FINAL_README_FILE
)


for (file_index in seq_along(staged_text_files)) {
  
  current_file <- staged_text_files[
    file_index
  ]
  
  current_lines <- readLines(
    current_file,
    warn = FALSE,
    encoding = "UTF-8"
  )
  
  current_lines <- iconv(
    current_lines,
    from = "",
    to = "UTF-8",
    sub = ""
  )
  
  
  for (term_index in seq_along(placeholder_terms)) {
    
    current_term <- placeholder_terms[
      term_index
    ]
    
    selector <- grepl(
      current_term,
      current_lines,
      fixed = TRUE,
      ignore.case = TRUE
    )
    
    selector[is.na(selector)] <- FALSE
    
    matched_lines <- which(
      selector
    )
    
    
    if (length(matched_lines) > 0L) {
      
      for (match_index in seq_along(matched_lines)) {
        
        current_line_number <- matched_lines[
          match_index
        ]
        
        new_record <- data.frame(
          file_name = basename(
            current_file
          ),
          location = paste0(
            "line ",
            current_line_number
          ),
          placeholder = current_term,
          matched_text = current_lines[
            current_line_number
          ],
          stringsAsFactors = FALSE
        )
        
        placeholder_audit <- rbind(
          placeholder_audit,
          new_record
        )
      }
    }
  }
}


# ------------------------------------------------------------------------------
# 25. AUDIT CSV FILES FOR PLACEHOLDERS
# ------------------------------------------------------------------------------

staged_csv_files <- c(
  FINAL_METADATA_FILE,
  RELATIONSHIPS_FILE,
  CONTRIBUTORS_FILE,
  KEYWORDS_FILE
)


for (file_index in seq_along(staged_csv_files)) {
  
  current_file <- staged_csv_files[
    file_index
  ]
  
  current_object <- safe_read_csv(
    current_file
  )
  
  if (nrow(current_object) == 0L) {
    next
  }
  
  
  for (column_index in seq_along(current_object)) {
    
    current_column_name <- names(
      current_object
    )[column_index]
    
    current_values <- normalise_text(
      current_object[
        ,
        column_index
      ]
    )
    
    
    for (term_index in seq_along(placeholder_terms)) {
      
      current_term <- placeholder_terms[
        term_index
      ]
      
      selector <- grepl(
        current_term,
        current_values,
        fixed = TRUE,
        ignore.case = TRUE
      )
      
      selector[is.na(selector)] <- FALSE
      
      matched_rows <- which(
        selector
      )
      
      
      if (length(matched_rows) > 0L) {
        
        for (match_index in seq_along(matched_rows)) {
          
          current_row <- matched_rows[
            match_index
          ]
          
          new_record <- data.frame(
            file_name = basename(
              current_file
            ),
            location = paste0(
              "row ",
              current_row,
              ", field ",
              current_column_name
            ),
            placeholder = current_term,
            matched_text = current_values[
              current_row
            ],
            stringsAsFactors = FALSE
          )
          
          placeholder_audit <- rbind(
            placeholder_audit,
            new_record
          )
        }
      }
    }
  }
}


if (nrow(placeholder_audit) > 0L) {
  
  placeholder_audit <- unique(
    placeholder_audit
  )
  
  rownames(
    placeholder_audit
  ) <- NULL
}


write.csv(
  placeholder_audit,
  PLACEHOLDER_AUDIT_FILE,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)


unresolved_placeholder_count <- nrow(
  placeholder_audit
)


# ------------------------------------------------------------------------------
# 26. CHECKSUM STAGED ADMINISTRATIVE PRODUCTS
# ------------------------------------------------------------------------------

staged_outputs <- c(
  FINAL_METADATA_FILE,
  FINAL_METADATA_TEXT_FILE,
  FINAL_README_FILE,
  RELATIONSHIPS_FILE,
  CONTRIBUTORS_FILE,
  KEYWORDS_FILE
)


digest_available <- requireNamespace(
  "digest",
  quietly = TRUE
)


checksum_values <- rep(
  NA_character_,
  length(staged_outputs)
)


if (digest_available) {
  
  for (file_index in seq_along(staged_outputs)) {
    
    checksum_values[
      file_index
    ] <- calculate_sha256(
      staged_outputs[
        file_index
      ]
    )
  }
}


checksum_table <- data.frame(
  file_name = basename(
    staged_outputs
  ),
  file_path = staged_outputs,
  size_bytes = file.info(
    staged_outputs
  )$size,
  sha256 = checksum_values,
  stringsAsFactors = FALSE
)


write.csv(
  checksum_table,
  CHECKSUM_FILE,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)


checksum_complete <- (
  digest_available &&
    all(
      !is.na(
        checksum_table$sha256
      )
    )
)


# ------------------------------------------------------------------------------
# 27. FINAL READINESS LOGIC
# ------------------------------------------------------------------------------

required_metadata_selector <- (
  metadata_validation$required_before_deposit
)


required_metadata_pass <- all(
  metadata_validation$passed[
    required_metadata_selector
  ]
)


optional_metadata_selector <- (
  !metadata_validation$required_before_deposit
)


optional_metadata_valid <- all(
  metadata_validation$valid[
    optional_metadata_selector
  ]
)


ZENODO_METADATA_READY <- (
  data_release_ready_06f &&
    required_metadata_pass &&
    optional_metadata_valid &&
    unresolved_placeholder_count == 0L &&
    checksum_complete
)


# ------------------------------------------------------------------------------
# 28. FINAL STATUS TABLE
# ------------------------------------------------------------------------------

final_status <- data.frame(
  release = RELEASE_NAME,
  build_date = BUILD_DATE,
  geography_release_doi = GEOGRAPHY_RELEASE_DOI,
  data_release_ready_06f = data_release_ready_06f,
  required_metadata_complete = required_metadata_pass,
  optional_metadata_valid = optional_metadata_valid,
  unresolved_placeholder_count = unresolved_placeholder_count,
  staging_checksums_complete = checksum_complete,
  geography_doi_reserved = geography_doi_valid,
  zenodo_metadata_ready = ZENODO_METADATA_READY,
  stringsAsFactors = FALSE
)


write.csv(
  final_status,
  FINAL_STATUS_FILE,
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)


# ------------------------------------------------------------------------------
# 29. IDENTIFY FAILED REQUIRED METADATA
# ------------------------------------------------------------------------------

failed_selector <- (
  !metadata_validation$passed &
    metadata_validation$required_before_deposit
)


failed_metadata <- metadata_validation$field[
  failed_selector
]


if (length(failed_metadata) == 0L) {
  
  failed_metadata_text <- "None"
  
} else {
  
  failed_metadata_text <- paste(
    failed_metadata,
    collapse = "; "
  )
}


# ------------------------------------------------------------------------------
# 30. FINAL REPORT
# ------------------------------------------------------------------------------

report_lines <- c(
  "VASCULAR PLANTS OF JAPAN DATABASE (VPJD)",
  "GEOGRAPHY RELEASE v1.0.0",
  "",
  "ZENODO METADATA FINALISATION REPORT",
  "",
  paste0(
    "Build date: ",
    BUILD_DATE
  ),
  paste0(
    "Script: ",
    SCRIPT_NAME
  ),
  paste0(
    "Reserved Geography DOI: ",
    GEOGRAPHY_RELEASE_DOI
  ),
  "",
  "INHERITED RELEASE STATUS",
  "",
  paste0(
    "06f DATA_RELEASE_READY = ",
    toupper(
      as.character(
        data_release_ready_06f
      )
    )
  ),
  "",
  "ADMINISTRATIVE METADATA",
  "",
  paste0(
    "Required metadata complete: ",
    toupper(
      as.character(
        required_metadata_pass
      )
    )
  ),
  paste0(
    "Unresolved staged placeholders: ",
    unresolved_placeholder_count
  ),
  paste0(
    "Staging SHA-256 checksums complete: ",
    toupper(
      as.character(
        checksum_complete
      )
    )
  ),
  paste0(
    "Geography DOI reserved: ",
    toupper(
      as.character(
        geography_doi_valid
      )
    )
  ),
  "",
  "FAILED REQUIRED METADATA",
  "",
  failed_metadata_text,
  "",
  "FINAL STATUS",
  "",
  paste0(
    "ZENODO_METADATA_READY = ",
    toupper(
      as.character(
        ZENODO_METADATA_READY
      )
    )
  ),
  "",
  "SAFEGUARDS",
  "",
  "VPJD Taxonomic Release v1.0.0 was not modified.",
  "Canonical geographic vocabulary was not modified.",
  "Canonical taxon geographic distribution was not modified.",
  "Canonical geographic evidence was not modified.",
  "06f audit products were not modified.",
  "No taxonomic relationships were generated.",
  "No geographic relationships were generated.",
  "No biological absence was inferred.",
  "No endemicity was inferred.",
  "No rarity was inferred.",
  "No Nakamura logic was applied.",
  "No Star categories were assigned."
)


write_utf8_lines(
  report_lines,
  FINAL_REPORT_FILE
)


# ------------------------------------------------------------------------------
# 31. CONSOLE - METADATA VALIDATION
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" METADATA VALIDATION\n")
cat("============================================================\n\n")


print(
  metadata_validation,
  row.names = FALSE
)


# ------------------------------------------------------------------------------
# 32. CONSOLE - PLACEHOLDER AUDIT
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" PLACEHOLDER AUDIT\n")
cat("============================================================\n\n")


cat(
  "Unresolved staged placeholders:",
  unresolved_placeholder_count,
  "\n\n"
)


if (unresolved_placeholder_count > 0L) {
  
  print(
    placeholder_audit,
    row.names = FALSE
  )
  
} else {
  
  cat(
    "No unresolved staged metadata placeholders detected.\n"
  )
}


# ------------------------------------------------------------------------------
# 33. CONSOLE - STAGING CHECKSUMS
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" STAGING SHA-256 CHECKSUMS\n")
cat("============================================================\n\n")


print(
  checksum_table[
    ,
    c(
      "file_name",
      "size_bytes",
      "sha256"
    ),
    drop = FALSE
  ],
  row.names = FALSE
)


# ------------------------------------------------------------------------------
# 34. CONSOLE - FINAL STATUS
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" FINAL ZENODO READINESS\n")
cat("============================================================\n\n")


print(
  final_status,
  row.names = FALSE
)


# ------------------------------------------------------------------------------
# 35. OUTPUTS
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" 06g OUTPUTS\n")
cat("============================================================\n\n")


output_files <- c(
  FINAL_METADATA_FILE,
  FINAL_METADATA_TEXT_FILE,
  FINAL_README_FILE,
  RELATIONSHIPS_FILE,
  CONTRIBUTORS_FILE,
  KEYWORDS_FILE,
  METADATA_VALIDATION_FILE,
  PLACEHOLDER_AUDIT_FILE,
  CHECKSUM_FILE,
  FINAL_STATUS_FILE,
  FINAL_REPORT_FILE
)


for (output_index in seq_along(output_files)) {
  
  cat(
    output_files[
      output_index
    ],
    "\n"
  )
}


# ------------------------------------------------------------------------------
# 36. SAFEGUARDS
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" SAFEGUARDS\n")
cat("============================================================\n\n")


cat("06g produced administrative staging products only.\n")
cat("VPJD Taxonomic Release v1.0.0 was not modified.\n")
cat("Canonical geographic vocabulary was not modified.\n")
cat("Canonical taxon geographic distribution was not modified.\n")
cat("Canonical geographic evidence was not modified.\n")
cat("06f audit products were not modified.\n")
cat("No identifiers were repaired.\n")
cat("No distribution relationships were generated or repaired.\n")
cat("No biological absence was inferred.\n")
cat("No endemicity was inferred.\n")
cat("No rarity was inferred.\n")
cat("No Nakamura logic was applied.\n")
cat("No Star categories were assigned.\n")


# ------------------------------------------------------------------------------
# 37. FINAL INTERPRETATION
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" FINAL INTERPRETATION\n")
cat("============================================================\n\n")


cat(
  "DATA_RELEASE_READY = ",
  toupper(
    as.character(
      data_release_ready_06f
    )
  ),
  "\n\n",
  sep = ""
)


cat(
  "ZENODO_METADATA_READY = ",
  toupper(
    as.character(
      ZENODO_METADATA_READY
    )
  ),
  "\n\n",
  sep = ""
)


if (ZENODO_METADATA_READY) {
  
  cat(
    paste(
      "The VPJD Geography v1.0.0 scientific release and",
      "administrative metadata are ready for Zenodo packaging."
    ),
    "\n\n"
  )
  
} else {
  
  cat(
    "The scientific release remains frozen and valid.\n\n"
  )
  
  cat(
    paste(
      "One or more administrative metadata fields require",
      "human confirmation before Zenodo deposit."
    ),
    "\n\n"
  )
  
  
  if (length(failed_metadata) > 0L) {
    
    cat(
      "Fields requiring attention:\n\n"
    )
    
    for (field_index in seq_along(failed_metadata)) {
      
      cat(
        " - ",
        failed_metadata[
          field_index
        ],
        "\n",
        sep = ""
      )
    }
  }
}


# ------------------------------------------------------------------------------
# 38. NEXT STEP
# ------------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" NEXT STEP\n")
cat("============================================================\n\n")


if (ZENODO_METADATA_READY) {
  
  cat(
    paste(
      "Proceed to final Zenodo release-package assembly.",
      "Do not modify the frozen scientific data products."
    ),
    "\n"
  )
  
} else {
  
  cat(
    paste(
      "Resolve any metadata fields reported above,",
      "then rerun this script."
    ),
    "\n"
  )
}


cat("\n")
cat("============================================================\n")