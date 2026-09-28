# ==============================================================================
# VPJD / OJPCP
# 01b — Catalogue legacy JAPAN PROJECT research resources
#
# Version: 0.2.0
#
# PURPOSE
# -------
# Inventory the legacy:
#
#   I:/JAPAN PROJECT
#
# with emphasis on research/data resources potentially relevant to VPJD/OJPCP.
#
# This version:
#   * recursively inventories the archive
#   * EXCLUDES media from the principal research-resource catalogue
#   * does NOT hash media files
#   * does NOT open/read file contents
#   * classifies likely research resources
#   * identifies duplicate candidates using metadata
#   * calculates MD5 only for non-media duplicate candidates
#   * preserves all source files unchanged
#
# IMPORTANT
# ---------
# READ-ONLY against I:/JAPAN PROJECT.
# Nothing is moved, renamed, deleted or modified.
# ==============================================================================


# ==============================================================================
# 00. CONFIGURATION
# ==============================================================================

MODULE  <- "01b_catalogue_legacy_japan_project"
VERSION <- "0.2.0"
RUN_DATE <- Sys.Date()

SOURCE_ROOT <- "I:/JAPAN PROJECT"

PROJECT_ROOT <- "I:/R/OJPCP/VPJD-OJPCP"

OUTPUT_ROOT <- file.path(
  PROJECT_ROOT,
  "outputs",
  "catalogues",
  "legacy_japan_project"
)

dir.create(
  OUTPUT_ROOT,
  recursive = TRUE,
  showWarnings = FALSE
)


# ==============================================================================
# 01. PACKAGES
# ==============================================================================

required_packages <- c(
  "dplyr",
  "readr",
  "stringr",
  "tibble",
  "purrr"
)

missing_packages <- required_packages[
  !vapply(
    required_packages,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )
]

if (length(missing_packages) > 0L) {
  
  stop(
    paste0(
      "Missing required packages:\n",
      paste(missing_packages, collapse = ", "),
      "\n\nInstall these packages before rerunning."
    )
  )
}

library(dplyr)
library(readr)
library(stringr)
library(tibble)
library(purrr)


# ==============================================================================
# 02. START
# ==============================================================================

cat("\n")
cat("============================================================\n")
cat("VPJD LEGACY JAPAN PROJECT CATALOGUE\n")
cat("============================================================\n")
cat("Run date: ", RUN_DATE, "\n", sep = "")
cat("Module:   ", MODULE, "\n", sep = "")
cat("Version:  ", VERSION, "\n", sep = "")
cat("Source:   ", SOURCE_ROOT, "\n", sep = "")
cat("Output:   ", OUTPUT_ROOT, "\n", sep = "")
cat("============================================================\n\n")


# ==============================================================================
# 03. VALIDATE SOURCE
# ==============================================================================

if (!dir.exists(SOURCE_ROOT)) {
  
  stop(
    paste0(
      "Source directory does not exist:\n",
      SOURCE_ROOT
    )
  )
}


# ==============================================================================
# 04. MEDIA DEFINITIONS
#
# Media are inventoried separately but excluded from:
#   * principal research catalogue
#   * duplicate hashing
#   * research-resource prioritisation
#
# Includes common still-image, RAW, video and audio formats.
# ==============================================================================

IMAGE_EXTENSIONS <- c(
  "jpg", "jpeg", "jpe",
  "png",
  "gif",
  "bmp",
  "webp",
  "heic", "heif",
  "tif", "tiff",
  
  # RAW photography
  "dng",
  "cr2", "cr3",
  "nef", "nrw",
  "arw", "srf", "sr2",
  "orf",
  "rw2",
  "raf",
  "pef",
  "raw"
)

VIDEO_EXTENSIONS <- c(
  "mp4",
  "mov",
  "avi",
  "mkv",
  "mpeg",
  "mpg",
  "m4v",
  "wmv",
  "mts",
  "m2ts",
  "3gp",
  "webm",
  "lrv"
)

AUDIO_EXTENSIONS <- c(
  "mp3",
  "wav",
  "m4a",
  "aac",
  "flac",
  "ogg",
  "wma"
)

MEDIA_EXTENSIONS <- unique(
  c(
    IMAGE_EXTENSIONS,
    VIDEO_EXTENSIONS,
    AUDIO_EXTENSIONS
  )
)


# ==============================================================================
# 05. RESEARCH-DATA EXTENSIONS
# ==============================================================================

TABULAR_EXTENSIONS <- c(
  "csv",
  "tsv",
  "txt",
  "xls",
  "xlsx",
  "xlsm",
  "xlsb",
  "ods",
  "dbf"
)

DATABASE_EXTENSIONS <- c(
  "accdb",
  "mdb",
  "sqlite",
  "sqlite3",
  "db",
  "duckdb"
)

GIS_EXTENSIONS <- c(
  "shp",
  "shx",
  "prj",
  "cpg",
  "geojson",
  "gpkg",
  "kml",
  "kmz",
  "gpx",
  "asc",
  "grd"
)

DOCUMENT_EXTENSIONS <- c(
  "pdf",
  "doc",
  "docx",
  "rtf",
  "odt"
)

PRESENTATION_EXTENSIONS <- c(
  "ppt",
  "pptx",
  "odp"
)

ARCHIVE_EXTENSIONS <- c(
  "zip",
  "7z",
  "rar",
  "tar",
  "gz"
)

CODE_EXTENSIONS <- c(
  "r",
  "rmd",
  "qmd",
  "py",
  "sql"
)


# ==============================================================================
# 06. SAFE RECURSIVE FILE INVENTORY
#
# Use base R rather than fs here to avoid Windows PATH_MAX problems encountered
# in the previous version.
# ==============================================================================

cat("Scanning directory tree ...\n")

paths <- list.files(
  path = SOURCE_ROOT,
  recursive = TRUE,
  full.names = TRUE,
  all.files = TRUE,
  include.dirs = FALSE,
  no.. = TRUE
)

if (length(paths) == 0L) {
  stop("No files found under SOURCE_ROOT.")
}

cat(
  "Files discovered: ",
  format(length(paths), big.mark = ","),
  "\n",
  sep = ""
)


# ==============================================================================
# 07. BUILD BASIC FILE CATALOGUE
# ==============================================================================

cat("Reading filesystem metadata ...\n")

file_info <- file.info(paths)

catalogue <- tibble(
  SOURCE_PATH = paths,
  FILE_NAME   = basename(paths),
  FILE_SIZE_BYTES = as.numeric(file_info$size),
  DATE_MODIFIED = file_info$mtime,
  DATE_CREATED  = file_info$ctime
) %>%
  
  mutate(
    
    EXTENSION = str_to_lower(
      tools::file_ext(FILE_NAME)
    ),
    
    EXTENSION = if_else(
      EXTENSION == "",
      NA_character_,
      EXTENSION
    ),
    
    RELATIVE_PATH = str_remove(
      SOURCE_PATH,
      paste0(
        "^",
        stringr::fixed(SOURCE_ROOT),
        "[/\\\\]?"
      )
    ),
    
    PARENT_FOLDER = basename(
      dirname(SOURCE_PATH)
    ),
    
    TOP_LEVEL_FOLDER = str_split_fixed(
      RELATIVE_PATH,
      "[/\\\\]",
      2
    )[, 1],
    
    FILE_SIZE_MB = FILE_SIZE_BYTES / 1024^2,
    
    FILE_SIZE_GB = FILE_SIZE_BYTES / 1024^3
  )


# ==============================================================================
# 08. CLASSIFY MEDIA
# ==============================================================================

catalogue <- catalogue %>%
  
  mutate(
    
    MEDIA_CLASS = case_when(
      
      EXTENSION %in% IMAGE_EXTENSIONS ~
        "IMAGE",
      
      EXTENSION %in% VIDEO_EXTENSIONS ~
        "VIDEO",
      
      EXTENSION %in% AUDIO_EXTENSIONS ~
        "AUDIO",
      
      TRUE ~
        "NON_MEDIA"
    ),
    
    IS_MEDIA = MEDIA_CLASS != "NON_MEDIA"
  )


# ==============================================================================
# 09. GENERAL FILE-TYPE CLASSIFICATION
# ==============================================================================

catalogue <- catalogue %>%
  
  mutate(
    
    FILE_CLASS = case_when(
      
      IS_MEDIA ~
        "MEDIA",
      
      EXTENSION %in% TABULAR_EXTENSIONS ~
        "TABULAR_DATA",
      
      EXTENSION %in% DATABASE_EXTENSIONS ~
        "DATABASE",
      
      EXTENSION %in% GIS_EXTENSIONS ~
        "GIS_SPATIAL",
      
      EXTENSION %in% DOCUMENT_EXTENSIONS ~
        "DOCUMENT",
      
      EXTENSION %in% PRESENTATION_EXTENSIONS ~
        "PRESENTATION",
      
      EXTENSION %in% ARCHIVE_EXTENSIONS ~
        "ARCHIVE",
      
      EXTENSION %in% CODE_EXTENSIONS ~
        "CODE",
      
      TRUE ~
        "OTHER"
    )
  )


# ==============================================================================
# 10. RESEARCH-CONTENT CLASSIFICATION
#
# Conservative filename/path-based classification only.
#
# This does NOT inspect file contents.
# ==============================================================================

classification_text <- str_to_lower(
  paste(
    catalogue$RELATIVE_PATH,
    catalogue$FILE_NAME
  )
)

catalogue <- catalogue %>%
  
  mutate(
    
    RESEARCH_CATEGORY = case_when(
      
      IS_MEDIA ~
        "MEDIA_EXCLUDED",
      
      str_detect(
        classification_text,
        "rapid botanic|\\brbs\\b|botanic survey"
      ) ~
        "RAPID_BOTANIC_SURVEY",
      
      str_detect(
        classification_text,
        "seed collection|seed collections|\\boujp\\b.*seed|accession"
      ) ~
        "SEED_COLLECTION",
      
      str_detect(
        classification_text,
        "checklist|check list|species list|plant list|flora list"
      ) ~
        "CHECKLIST",
      
      str_detect(
        classification_text,
        "permit|permission|authorisation|authorization"
      ) ~
        "PERMIT",
      
      str_detect(
        classification_text,
        "phyto|phytosanitary|plant health|import requirement"
      ) ~
        "PHYTOSANITARY_PLANT_HEALTH",
      
      str_detect(
        classification_text,
        "map|mapping|gis|spatial|coordinate|gps"
      ) ~
        "MAPPING_GIS",
      
      str_detect(
        classification_text,
        "mou|agreement|data sharing"
      ) ~
        "AGREEMENT_MOU",
      
      str_detect(
        classification_text,
        "grant|funding|proposal|gb[s]?f|gcc"
      ) ~
        "FUNDING",
      
      str_detect(
        classification_text,
        "report"
      ) ~
        "REPORT",
      
      str_detect(
        classification_text,
        "presentation|slides"
      ) ~
        "PRESENTATION",
      
      FILE_CLASS == "TABULAR_DATA" ~
        "UNCLASSIFIED_TABULAR_DATA",
      
      FILE_CLASS == "DATABASE" ~
        "UNCLASSIFIED_DATABASE",
      
      FILE_CLASS == "GIS_SPATIAL" ~
        "UNCLASSIFIED_GIS",
      
      FILE_CLASS == "DOCUMENT" ~
        "UNCLASSIFIED_DOCUMENT",
      
      TRUE ~
        "OTHER"
    )
  )


# ==============================================================================
# 11. RELEVANCE PRIORITY
#
# Designed to identify material worth inspecting next for integration with
# VPJD/OJPCP.
# ==============================================================================

catalogue <- catalogue %>%
  
  mutate(
    
    VPJD_PRIORITY = case_when(
      
      IS_MEDIA ~
        "EXCLUDED_MEDIA",
      
      RESEARCH_CATEGORY %in% c(
        "RAPID_BOTANIC_SURVEY",
        "SEED_COLLECTION",
        "CHECKLIST",
        "MAPPING_GIS",
        "UNCLASSIFIED_TABULAR_DATA",
        "UNCLASSIFIED_DATABASE",
        "UNCLASSIFIED_GIS"
      ) ~
        "HIGH",
      
      RESEARCH_CATEGORY %in% c(
        "PERMIT",
        "PHYTOSANITARY_PLANT_HEALTH",
        "REPORT",
        "AGREEMENT_MOU"
      ) ~
        "MEDIUM",
      
      TRUE ~
        "LOW"
    )
  )


# ==============================================================================
# 12. SPLIT MEDIA / NON-MEDIA
# ==============================================================================

media_catalogue <- catalogue %>%
  filter(IS_MEDIA)

research_catalogue <- catalogue %>%
  filter(!IS_MEDIA)

cat("\n— INVENTORY SPLIT —\n")

cat(
  "Total files:      ",
  format(nrow(catalogue), big.mark = ","),
  "\n",
  sep = ""
)

cat(
  "Media excluded:   ",
  format(nrow(media_catalogue), big.mark = ","),
  "\n",
  sep = ""
)

cat(
  "Non-media files:  ",
  format(nrow(research_catalogue), big.mark = ","),
  "\n",
  sep = ""
)


# ==============================================================================
# 13. MEDIA SUMMARY
#
# Do NOT hash individual media files.
# Keep only summary-level information for this workflow.
# ==============================================================================

media_summary <- media_catalogue %>%
  
  group_by(
    TOP_LEVEL_FOLDER,
    MEDIA_CLASS
  ) %>%
  
  summarise(
    N_FILES = n(),
    TOTAL_SIZE_BYTES = sum(
      FILE_SIZE_BYTES,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  
  mutate(
    TOTAL_SIZE_GB = TOTAL_SIZE_BYTES / 1024^3
  ) %>%
  
  arrange(
    desc(TOTAL_SIZE_BYTES)
  )


# ==============================================================================
# 14. FILE-CLASS SUMMARY
# ==============================================================================

file_class_summary <- research_catalogue %>%
  
  count(
    FILE_CLASS,
    sort = TRUE,
    name = "N_FILES"
  )


# ==============================================================================
# 15. RESEARCH-CATEGORY SUMMARY
# ==============================================================================

research_category_summary <- research_catalogue %>%
  
  count(
    RESEARCH_CATEGORY,
    VPJD_PRIORITY,
    sort = TRUE,
    name = "N_FILES"
  )


# ==============================================================================
# 16. TOP-LEVEL FOLDER SUMMARY
# ==============================================================================

folder_summary <- catalogue %>%
  
  group_by(TOP_LEVEL_FOLDER) %>%
  
  summarise(
    
    N_FILES = n(),
    
    N_MEDIA = sum(
      IS_MEDIA,
      na.rm = TRUE
    ),
    
    N_NON_MEDIA = sum(
      !IS_MEDIA,
      na.rm = TRUE
    ),
    
    N_HIGH_PRIORITY = sum(
      VPJD_PRIORITY == "HIGH",
      na.rm = TRUE
    ),
    
    TOTAL_SIZE_BYTES = sum(
      FILE_SIZE_BYTES,
      na.rm = TRUE
    ),
    
    .groups = "drop"
  ) %>%
  
  mutate(
    TOTAL_SIZE_GB = TOTAL_SIZE_BYTES / 1024^3
  ) %>%
  
  arrange(
    desc(N_HIGH_PRIORITY),
    desc(N_NON_MEDIA)
  )


# ==============================================================================
# 17. HIGH-PRIORITY RESEARCH RESOURCES
# ==============================================================================

high_priority <- research_catalogue %>%
  
  filter(
    VPJD_PRIORITY == "HIGH"
  ) %>%
  
  arrange(
    RESEARCH_CATEGORY,
    RELATIVE_PATH
  )


# ==============================================================================
# 18. METADATA-BASED DUPLICATE CANDIDATES
#
# First pass:
#   same file size AND same filename
#
# Second pass:
#   same file size irrespective of filename
#
# Zero-byte files are excluded.
# ==============================================================================

duplicate_name_size <- research_catalogue %>%
  
  filter(
    !is.na(FILE_SIZE_BYTES),
    FILE_SIZE_BYTES > 0
  ) %>%
  
  group_by(
    FILE_NAME,
    FILE_SIZE_BYTES
  ) %>%
  
  mutate(
    DUPLICATE_NAME_SIZE_COUNT = n()
  ) %>%
  
  ungroup() %>%
  
  filter(
    DUPLICATE_NAME_SIZE_COUNT > 1
  ) %>%
  
  arrange(
    FILE_NAME,
    FILE_SIZE_BYTES,
    SOURCE_PATH
  )


duplicate_size_only <- research_catalogue %>%
  
  filter(
    !is.na(FILE_SIZE_BYTES),
    FILE_SIZE_BYTES > 0
  ) %>%
  
  group_by(
    FILE_SIZE_BYTES
  ) %>%
  
  mutate(
    DUPLICATE_SIZE_COUNT = n()
  ) %>%
  
  ungroup() %>%
  
  filter(
    DUPLICATE_SIZE_COUNT > 1
  ) %>%
  
  arrange(
    desc(FILE_SIZE_BYTES),
    SOURCE_PATH
  )


# ==============================================================================
# 19. SELECT FILES FOR MD5 CONFIRMATION
#
# IMPORTANT:
# Only non-media files already identified as metadata duplicate candidates
# are hashed.
#
# This avoids reading the entire image / GoPro archive.
# ==============================================================================

hash_candidates <- duplicate_size_only %>%
  
  distinct(
    SOURCE_PATH,
    .keep_all = TRUE
  )

cat("\n— DUPLICATE AUDIT —\n")

cat(
  "Same-name + same-size candidate files: ",
  format(nrow(duplicate_name_size), big.mark = ","),
  "\n",
  sep = ""
)

cat(
  "Same-size candidate files for selective hashing: ",
  format(nrow(hash_candidates), big.mark = ","),
  "\n",
  sep = ""
)


# ==============================================================================
# 20. SELECTIVE MD5 HASHING
#
# Hash files individually so one unreadable/long-path file does not terminate
# the entire catalogue.
#
# MEDIA NEVER REACHES THIS STAGE.
# ==============================================================================

safe_md5 <- function(path) {
  
  tryCatch(
    
    {
      
      hash <- unname(
        tools::md5sum(path)
      )
      
      if (
        length(hash) == 0L ||
        is.na(hash)
      ) {
        return(NA_character_)
      }
      
      as.character(hash)
      
    },
    
    error = function(e) {
      
      warning(
        paste0(
          "Could not hash:\n",
          path,
          "\n",
          conditionMessage(e)
        ),
        call. = FALSE
      )
      
      NA_character_
    }
  )
}


if (nrow(hash_candidates) > 0L) {
  
  cat(
    "Calculating MD5 only for non-media duplicate candidates ...\n"
  )
  
  hash_candidates <- hash_candidates %>%
    
    mutate(
      MD5 = map_chr(
        SOURCE_PATH,
        safe_md5
      )
    )
  
} else {
  
  hash_candidates <- hash_candidates %>%
    mutate(
      MD5 = character()
    )
}


# ==============================================================================
# 21. CONFIRMED DUPLICATES
# ==============================================================================

confirmed_duplicates <- hash_candidates %>%
  
  filter(
    !is.na(MD5),
    MD5 != ""
  ) %>%
  
  group_by(MD5) %>%
  
  mutate(
    CONFIRMED_DUPLICATE_COUNT = n()
  ) %>%
  
  ungroup() %>%
  
  filter(
    CONFIRMED_DUPLICATE_COUNT > 1
  ) %>%
  
  arrange(
    MD5,
    SOURCE_PATH
  )


# ==============================================================================
# 22. DUPLICATE GROUP IDS
# ==============================================================================

if (nrow(confirmed_duplicates) > 0L) {
  
  duplicate_groups <- confirmed_duplicates %>%
    
    distinct(MD5) %>%
    
    arrange(MD5) %>%
    
    mutate(
      DUPLICATE_GROUP_ID = sprintf(
        "DUP_%05d",
        row_number()
      )
    )
  
  confirmed_duplicates <- confirmed_duplicates %>%
    
    left_join(
      duplicate_groups,
      by = "MD5"
    ) %>%
    
    arrange(
      DUPLICATE_GROUP_ID,
      SOURCE_PATH
    )
  
} else {
  
  confirmed_duplicates <- confirmed_duplicates %>%
    mutate(
      DUPLICATE_GROUP_ID = character()
    )
}


# ==============================================================================
# 23. DUPLICATE GROUP SUMMARY
# ==============================================================================

duplicate_summary <- confirmed_duplicates %>%
  
  group_by(
    DUPLICATE_GROUP_ID,
    MD5,
    FILE_SIZE_BYTES
  ) %>%
  
  summarise(
    
    N_COPIES = n(),
    
    FILE_NAMES = paste(
      unique(FILE_NAME),
      collapse = " | "
    ),
    
    LOCATIONS = paste(
      SOURCE_PATH,
      collapse = " | "
    ),
    
    .groups = "drop"
  ) %>%
  
  mutate(
    FILE_SIZE_MB = FILE_SIZE_BYTES / 1024^2,
    
    POTENTIAL_REDUNDANT_BYTES =
      FILE_SIZE_BYTES * (N_COPIES - 1),
    
    POTENTIAL_REDUNDANT_MB =
      POTENTIAL_REDUNDANT_BYTES / 1024^2
  ) %>%
  
  arrange(
    desc(POTENTIAL_REDUNDANT_BYTES)
  )


# ==============================================================================
# 24. EXPORT FULL NON-MEDIA CATALOGUE
# ==============================================================================

catalogue_file <- file.path(
  OUTPUT_ROOT,
  "VPJD_legacy_Japan_Project_research_catalogue.csv"
)

write_csv(
  research_catalogue,
  catalogue_file,
  na = ""
)


# ==============================================================================
# 25. EXPORT HIGH-PRIORITY RESOURCE REGISTER
# ==============================================================================

high_priority_file <- file.path(
  OUTPUT_ROOT,
  "VPJD_legacy_Japan_Project_high_priority_resources.csv"
)

write_csv(
  high_priority,
  high_priority_file,
  na = ""
)


# ==============================================================================
# 26. EXPORT MEDIA SUMMARY
#
# Deliberately summary-only.
# ==============================================================================

media_summary_file <- file.path(
  OUTPUT_ROOT,
  "VPJD_legacy_Japan_Project_media_summary.csv"
)

write_csv(
  media_summary,
  media_summary_file,
  na = ""
)


# ==============================================================================
# 27. EXPORT FOLDER SUMMARY
# ==============================================================================

folder_summary_file <- file.path(
  OUTPUT_ROOT,
  "VPJD_legacy_Japan_Project_folder_summary.csv"
)

write_csv(
  folder_summary,
  folder_summary_file,
  na = ""
)


# ==============================================================================
# 28. EXPORT RESEARCH-CATEGORY SUMMARY
# ==============================================================================

category_summary_file <- file.path(
  OUTPUT_ROOT,
  "VPJD_legacy_Japan_Project_category_summary.csv"
)

write_csv(
  research_category_summary,
  category_summary_file,
  na = ""
)


# ==============================================================================
# 29. EXPORT FILE-CLASS SUMMARY
# ==============================================================================

file_class_file <- file.path(
  OUTPUT_ROOT,
  "VPJD_legacy_Japan_Project_file_class_summary.csv"
)

write_csv(
  file_class_summary,
  file_class_file,
  na = ""
)


# ==============================================================================
# 30. EXPORT DUPLICATE CANDIDATES
# ==============================================================================

duplicate_candidates_file <- file.path(
  OUTPUT_ROOT,
  "VPJD_legacy_Japan_Project_duplicate_candidates.csv"
)

write_csv(
  duplicate_size_only,
  duplicate_candidates_file,
  na = ""
)


# ==============================================================================
# 31. EXPORT CONFIRMED DUPLICATES
# ==============================================================================

confirmed_duplicates_file <- file.path(
  OUTPUT_ROOT,
  "VPJD_legacy_Japan_Project_confirmed_duplicates.csv"
)

write_csv(
  confirmed_duplicates,
  confirmed_duplicates_file,
  na = ""
)


duplicate_summary_file <- file.path(
  OUTPUT_ROOT,
  "VPJD_legacy_Japan_Project_duplicate_groups.csv"
)

write_csv(
  duplicate_summary,
  duplicate_summary_file,
  na = ""
)


# ==============================================================================
# 32. VALIDATION
# ==============================================================================

validation <- tibble(
  
  CHECK = c(
    
    "Source directory exists",
    
    "At least one file discovered",
    
    "Catalogue population equals discovered files",
    
    "Media + non-media equals total population",
    
    "Research catalogue contains no media",
    
    "Hash candidate population contains no media",
    
    "High-priority register contains no media",
    
    "Research catalogue written",
    
    "High-priority register written",
    
    "Media summary written",
    
    "Folder summary written",
    
    "Category summary written",
    
    "Duplicate candidate register written",
    
    "Confirmed duplicate register written",
    
    "No source files modified by script"
  ),
  
  PASS = c(
    
    dir.exists(SOURCE_ROOT),
    
    length(paths) > 0L,
    
    nrow(catalogue) == length(paths),
    
    (
      nrow(media_catalogue) +
        nrow(research_catalogue)
    ) == nrow(catalogue),
    
    !any(
      research_catalogue$IS_MEDIA,
      na.rm = TRUE
    ),
    
    !any(
      hash_candidates$IS_MEDIA,
      na.rm = TRUE
    ),
    
    !any(
      high_priority$IS_MEDIA,
      na.rm = TRUE
    ),
    
    file.exists(catalogue_file),
    
    file.exists(high_priority_file),
    
    file.exists(media_summary_file),
    
    file.exists(folder_summary_file),
    
    file.exists(category_summary_file),
    
    file.exists(duplicate_candidates_file),
    
    file.exists(confirmed_duplicates_file),
    
    TRUE
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

print(
  validation,
  n = Inf
)


if (!all(validation$PASS, na.rm = TRUE)) {
  
  stop(
    "01b catalogue validation failed. Review FAIL checks."
  )
}


# ==============================================================================
# 33. CONSOLE SUMMARIES
# ==============================================================================

cat("\n— NON-MEDIA FILE CLASSES —\n")

print(
  file_class_summary,
  n = Inf
)


cat("\n— RESEARCH RESOURCE CATEGORIES —\n")

print(
  research_category_summary,
  n = Inf
)


cat("\n— MEDIA EXCLUDED —\n")

print(
  media_summary,
  n = Inf
)


cat("\n— TOP-LEVEL FOLDERS —\n")

print(
  folder_summary,
  n = Inf
)


# ==============================================================================
# 34. COMPLETION SUMMARY
# ==============================================================================

potential_redundant_bytes <- if (
  nrow(duplicate_summary) > 0L
) {
  
  sum(
    duplicate_summary$POTENTIAL_REDUNDANT_BYTES,
    na.rm = TRUE
  )
  
} else {
  
  0
}


cat("\n")
cat("============================================================\n")
cat("01b LEGACY JAPAN PROJECT CATALOGUE COMPLETE\n")
cat("============================================================\n")

cat(
  "Files discovered: ",
  format(nrow(catalogue), big.mark = ","),
  "\n",
  sep = ""
)

cat(
  "Media files excluded: ",
  format(nrow(media_catalogue), big.mark = ","),
  "\n",
  sep = ""
)

cat(
  "Non-media files catalogued: ",
  format(nrow(research_catalogue), big.mark = ","),
  "\n",
  sep = ""
)

cat(
  "High-priority research resources: ",
  format(nrow(high_priority), big.mark = ","),
  "\n",
  sep = ""
)

cat(
  "Metadata duplicate candidates: ",
  format(nrow(hash_candidates), big.mark = ","),
  "\n",
  sep = ""
)

cat(
  "Confirmed duplicate files: ",
  format(nrow(confirmed_duplicates), big.mark = ","),
  "\n",
  sep = ""
)

cat(
  "Confirmed duplicate groups: ",
  format(nrow(duplicate_summary), big.mark = ","),
  "\n",
  sep = ""
)

cat(
  "Potential redundant non-media storage: ",
  round(
    potential_redundant_bytes / 1024^2,
    2
  ),
  " MB\n",
  sep = ""
)

cat(
  "Media hashed: FALSE\n"
)

cat(
  "Media content inspected: FALSE\n"
)

cat(
  "Source files moved: FALSE\n"
)

cat(
  "Source files modified: FALSE\n"
)

cat(
  "Validation: ",
  sum(validation$PASS, na.rm = TRUE),
  "/",
  nrow(validation),
  " PASS\n",
  sep = ""
)

cat(
  "Outputs: ",
  OUTPUT_ROOT,
  "\n",
  sep = ""
)

cat("============================================================\n")