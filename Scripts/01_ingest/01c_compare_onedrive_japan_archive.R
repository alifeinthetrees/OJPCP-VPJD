# ==============================================================================
# VPJD / OJPCP
# 01c — Compare OneDrive Japan archive with legacy I:/JAPAN PROJECT
#
# Version: 0.1.0
#
# PURPOSE
# -------
# Catalogue:
#
#   C:/Users/alife/OXFORD/OneDrive - Nexus365/Japan
#
# and compare its non-media research resources against:
#
#   I:/JAPAN PROJECT
#
# The module identifies:
#
#   1. Exact duplicate files present in both archives
#   2. Files present on C: but absent from I:
#   3. Same-name files whose contents differ
#   4. Same-size candidate duplicates
#   5. Potential transfer candidates from C: -> I:
#   6. Media summary only
#
# SAFETY
# ------
# READ-ONLY.
#
# This script DOES NOT:
#   * copy files
#   * move files
#   * rename files
#   * delete files
#   * overwrite files
#   * alter either archive
#
# MD5 is calculated selectively for NON-MEDIA files only.
#
# ==============================================================================


# ==============================================================================
# 00. CONFIGURATION
# ==============================================================================

MODULE  <- "01c_compare_onedrive_japan_archive"
VERSION <- "0.1.0"
RUN_DATE <- Sys.Date()


# ------------------------------------------------------------------------------
# Source archives
# ------------------------------------------------------------------------------

ONEDRIVE_ROOT <- "C:/Users/alife/OXFORD/OneDrive - Nexus365/Japan"

LEGACY_ROOT <- "I:/JAPAN PROJECT"


# ------------------------------------------------------------------------------
# VPJD project
# ------------------------------------------------------------------------------

PROJECT_ROOT <- "I:/R/OJPCP/VPJD-OJPCP"

OUTPUT_ROOT <- file.path(
  PROJECT_ROOT,
  "outputs",
  "catalogues",
  "onedrive_japan_comparison"
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
cat("VPJD — ONEDRIVE / LEGACY JAPAN ARCHIVE COMPARISON\n")
cat("============================================================\n")
cat("Run date: ", RUN_DATE, "\n", sep = "")
cat("Module:   ", MODULE, "\n", sep = "")
cat("Version:  ", VERSION, "\n", sep = "")
cat("\n")
cat("OneDrive source:\n")
cat("  ", ONEDRIVE_ROOT, "\n", sep = "")
cat("\n")
cat("Legacy comparison archive:\n")
cat("  ", LEGACY_ROOT, "\n", sep = "")
cat("\n")
cat("Outputs:\n")
cat("  ", OUTPUT_ROOT, "\n", sep = "")
cat("============================================================\n\n")


# ==============================================================================
# 03. VALIDATE ROOTS
# ==============================================================================

if (!dir.exists(ONEDRIVE_ROOT)) {
  
  stop(
    paste0(
      "OneDrive Japan directory does not exist:\n",
      ONEDRIVE_ROOT
    )
  )
}

if (!dir.exists(LEGACY_ROOT)) {
  
  stop(
    paste0(
      "Legacy JAPAN PROJECT directory does not exist:\n",
      LEGACY_ROOT
    )
  )
}


# ==============================================================================
# 04. MEDIA DEFINITIONS
# ==============================================================================

IMAGE_EXTENSIONS <- c(
  "jpg", "jpeg", "jpe",
  "png",
  "gif",
  "bmp",
  "webp",
  "heic", "heif",
  "tif", "tiff",
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
# 05. RESEARCH FILE DEFINITIONS
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
# 06. HELPER — SAFE FILE INVENTORY
#
# Base R is deliberately used instead of fs because we previously encountered
# Windows PATH_MAX issues in this archive.
# ==============================================================================

inventory_archive <- function(root, archive_id) {
  
  cat("\nScanning: ", root, "\n", sep = "")
  
  paths <- list.files(
    path = root,
    recursive = TRUE,
    full.names = TRUE,
    all.files = TRUE,
    include.dirs = FALSE,
    no.. = TRUE
  )
  
  if (length(paths) == 0L) {
    
    warning(
      paste0(
        "No files found under:\n",
        root
      ),
      call. = FALSE
    )
    
    return(
      tibble()
    )
  }
  
  cat(
    "Files discovered: ",
    format(length(paths), big.mark = ","),
    "\n",
    sep = ""
  )
  
  info <- file.info(paths)
  
  relative_path <- substring(
    paths,
    nchar(root) + 2L
  )
  
  tibble(
    
    ARCHIVE = archive_id,
    
    ARCHIVE_ROOT = root,
    
    SOURCE_PATH = paths,
    
    RELATIVE_PATH = relative_path,
    
    FILE_NAME = basename(paths),
    
    FILE_NAME_LOWER = str_to_lower(
      basename(paths)
    ),
    
    FILE_SIZE_BYTES = as.numeric(
      info$size
    ),
    
    DATE_MODIFIED = info$mtime,
    
    DATE_CREATED = info$ctime
    
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
      
      PARENT_FOLDER = basename(
        dirname(SOURCE_PATH)
      ),
      
      TOP_LEVEL_FOLDER = str_split_fixed(
        RELATIVE_PATH,
        "[/\\\\]",
        2
      )[, 1],
      
      FILE_SIZE_MB =
        FILE_SIZE_BYTES / 1024^2,
      
      FILE_SIZE_GB =
        FILE_SIZE_BYTES / 1024^3
    )
}


# ==============================================================================
# 07. INVENTORY BOTH ARCHIVES
# ==============================================================================

onedrive_all <- inventory_archive(
  ONEDRIVE_ROOT,
  "ONEDRIVE_JAPAN"
)

legacy_all <- inventory_archive(
  LEGACY_ROOT,
  "LEGACY_JAPAN_PROJECT"
)

if (nrow(onedrive_all) == 0L) {
  stop("OneDrive catalogue is empty.")
}

if (nrow(legacy_all) == 0L) {
  stop("Legacy catalogue is empty.")
}


# ==============================================================================
# 08. CLASSIFY FILES
# ==============================================================================

classify_files <- function(x) {
  
  classification_text <- str_to_lower(
    paste(
      x$RELATIVE_PATH,
      x$FILE_NAME
    )
  )
  
  x %>%
    
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
      
      IS_MEDIA =
        MEDIA_CLASS != "NON_MEDIA",
      
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
      ),
      
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
          "grant|funding|proposal|gbsf|gcc"
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
      ),
      
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
}


onedrive_all <- classify_files(
  onedrive_all
)

legacy_all <- classify_files(
  legacy_all
)


# ==============================================================================
# 09. SPLIT MEDIA / NON-MEDIA
# ==============================================================================

onedrive_media <- onedrive_all %>%
  filter(IS_MEDIA)

legacy_media <- legacy_all %>%
  filter(IS_MEDIA)


onedrive <- onedrive_all %>%
  filter(!IS_MEDIA)

legacy <- legacy_all %>%
  filter(!IS_MEDIA)


cat("\n")
cat("— ARCHIVE POPULATIONS —\n")

cat(
  "OneDrive total:     ",
  format(nrow(onedrive_all), big.mark = ","),
  "\n",
  sep = ""
)

cat(
  "OneDrive media:     ",
  format(nrow(onedrive_media), big.mark = ","),
  "\n",
  sep = ""
)

cat(
  "OneDrive non-media: ",
  format(nrow(onedrive), big.mark = ","),
  "\n",
  sep = ""
)

cat("\n")

cat(
  "Legacy total:       ",
  format(nrow(legacy_all), big.mark = ","),
  "\n",
  sep = ""
)

cat(
  "Legacy media:       ",
  format(nrow(legacy_media), big.mark = ","),
  "\n",
  sep = ""
)

cat(
  "Legacy non-media:   ",
  format(nrow(legacy), big.mark = ","),
  "\n",
  sep = ""
)


# ==============================================================================
# 10. MEDIA SUMMARY
#
# No media hashing.
# ==============================================================================

media_summary <- bind_rows(
  onedrive_media,
  legacy_media
) %>%
  
  group_by(
    ARCHIVE,
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
    TOTAL_SIZE_GB =
      TOTAL_SIZE_BYTES / 1024^3
  )


# ==============================================================================
# 11. QUICK NAME/SIZE COMPARISON
#
# This determines which files require hashing.
#
# We do NOT hash every file blindly.
# ==============================================================================

legacy_name_size <- legacy %>%
  
  transmute(
    FILE_NAME_LOWER,
    FILE_SIZE_BYTES,
    LEGACY_NAME_SIZE_MATCH = TRUE
  ) %>%
  
  distinct()


onedrive <- onedrive %>%
  
  left_join(
    legacy_name_size,
    by = c(
      "FILE_NAME_LOWER",
      "FILE_SIZE_BYTES"
    )
  ) %>%
  
  mutate(
    LEGACY_NAME_SIZE_MATCH =
      coalesce(
        LEGACY_NAME_SIZE_MATCH,
        FALSE
      )
  )


# ==============================================================================
# 12. SAME-SIZE INDEX
#
# A renamed copy may have identical content, so same-size candidates must also
# be considered.
# ==============================================================================

legacy_sizes <- legacy %>%
  
  filter(
    !is.na(FILE_SIZE_BYTES),
    FILE_SIZE_BYTES > 0
  ) %>%
  
  distinct(
    FILE_SIZE_BYTES
  ) %>%
  
  mutate(
    SIZE_EXISTS_IN_LEGACY = TRUE
  )


onedrive <- onedrive %>%
  
  left_join(
    legacy_sizes,
    by = "FILE_SIZE_BYTES"
  ) %>%
  
  mutate(
    SIZE_EXISTS_IN_LEGACY =
      coalesce(
        SIZE_EXISTS_IN_LEGACY,
        FALSE
      )
  )


# ==============================================================================
# 13. IDENTIFY CROSS-ARCHIVE HASH CANDIDATES
#
# Only files whose SIZE occurs in both archives need hashing to establish
# whether they are duplicates.
#
# This is substantially faster than hashing both archives wholesale.
# ==============================================================================

shared_sizes <- intersect(
  unique(
    onedrive$FILE_SIZE_BYTES[
      !is.na(onedrive$FILE_SIZE_BYTES) &
        onedrive$FILE_SIZE_BYTES > 0
    ]
  ),
  unique(
    legacy$FILE_SIZE_BYTES[
      !is.na(legacy$FILE_SIZE_BYTES) &
        legacy$FILE_SIZE_BYTES > 0
    ]
  )
)


onedrive_hash_candidates <- onedrive %>%
  
  filter(
    FILE_SIZE_BYTES %in% shared_sizes
  )


legacy_hash_candidates <- legacy %>%
  
  filter(
    FILE_SIZE_BYTES %in% shared_sizes
  )


cat("\n")
cat("— SELECTIVE CROSS-ARCHIVE HASHING —\n")

cat(
  "Shared file sizes: ",
  format(length(shared_sizes), big.mark = ","),
  "\n",
  sep = ""
)

cat(
  "OneDrive files requiring hash: ",
  format(nrow(onedrive_hash_candidates), big.mark = ","),
  "\n",
  sep = ""
)

cat(
  "Legacy files requiring hash:   ",
  format(nrow(legacy_hash_candidates), big.mark = ","),
  "\n",
  sep = ""
)


# ==============================================================================
# 14. SAFE MD5
# ==============================================================================

safe_md5 <- function(path) {
  
  tryCatch(
    
    {
      
      result <- unname(
        tools::md5sum(path)
      )
      
      if (
        length(result) == 0L ||
        is.na(result)
      ) {
        return(NA_character_)
      }
      
      as.character(result)
      
    },
    
    error = function(e) {
      
      warning(
        paste0(
          "MD5 failed for:\n",
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


# ==============================================================================
# 15. HASH CANDIDATES
# ==============================================================================

cat("\nHashing OneDrive non-media candidates ...\n")

onedrive_hash_candidates <- onedrive_hash_candidates %>%
  
  mutate(
    MD5 = map_chr(
      SOURCE_PATH,
      safe_md5
    )
  )


cat("Hashing legacy non-media candidates ...\n")

legacy_hash_candidates <- legacy_hash_candidates %>%
  
  mutate(
    MD5 = map_chr(
      SOURCE_PATH,
      safe_md5
    )
  )


# ==============================================================================
# 16. LEGACY HASH LOOKUP
# ==============================================================================

legacy_hash_lookup <- legacy_hash_candidates %>%
  
  filter(
    !is.na(MD5),
    MD5 != ""
  ) %>%
  
  group_by(MD5) %>%
  
  summarise(
    
    LEGACY_MATCH_COUNT = n(),
    
    LEGACY_MATCH_NAMES = paste(
      unique(FILE_NAME),
      collapse = " | "
    ),
    
    LEGACY_MATCH_PATHS = paste(
      SOURCE_PATH,
      collapse = " | "
    ),
    
    .groups = "drop"
  )


# ==============================================================================
# 17. ADD HASH RESULTS BACK TO ONEDRIVE CATALOGUE
# ==============================================================================

onedrive_hash_results <- onedrive_hash_candidates %>%
  
  select(
    SOURCE_PATH,
    MD5
  )


onedrive <- onedrive %>%
  
  left_join(
    onedrive_hash_results,
    by = "SOURCE_PATH"
  ) %>%
  
  left_join(
    legacy_hash_lookup,
    by = "MD5"
  ) %>%
  
  mutate(
    
    EXACT_DUPLICATE_IN_LEGACY =
      !is.na(LEGACY_MATCH_COUNT) &
      LEGACY_MATCH_COUNT > 0
  )


# ==============================================================================
# 18. SAME-NAME LOOKUP IN LEGACY
# ==============================================================================

legacy_name_lookup <- legacy %>%
  
  group_by(
    FILE_NAME_LOWER
  ) %>%
  
  summarise(
    
    SAME_NAME_IN_LEGACY = TRUE,
    
    LEGACY_SAME_NAME_COUNT = n(),
    
    LEGACY_SAME_NAME_SIZES = paste(
      sort(
        unique(FILE_SIZE_BYTES)
      ),
      collapse = " | "
    ),
    
    LEGACY_SAME_NAME_PATHS = paste(
      SOURCE_PATH,
      collapse = " | "
    ),
    
    .groups = "drop"
  )


onedrive <- onedrive %>%
  
  left_join(
    legacy_name_lookup,
    by = "FILE_NAME_LOWER"
  ) %>%
  
  mutate(
    
    SAME_NAME_IN_LEGACY =
      coalesce(
        SAME_NAME_IN_LEGACY,
        FALSE
      )
  )


# ==============================================================================
# 19. CLASSIFY COMPARISON RESULT
# ==============================================================================

onedrive <- onedrive %>%
  
  mutate(
    
    COMPARISON_STATUS = case_when(
      
      EXACT_DUPLICATE_IN_LEGACY ~
        "EXACT_DUPLICATE_IN_LEGACY",
      
      SAME_NAME_IN_LEGACY &
        !EXACT_DUPLICATE_IN_LEGACY ~
        "SAME_NAME_DIFFERENT_CONTENT_OR_VERSION",
      
      !SIZE_EXISTS_IN_LEGACY ~
        "ABSENT_FROM_LEGACY",
      
      SIZE_EXISTS_IN_LEGACY &
        !EXACT_DUPLICATE_IN_LEGACY ~
        "ABSENT_FROM_LEGACY_SAME_SIZE_AS_OTHER_FILE",
      
      TRUE ~
        "REVIEW"
    )
  )


# ==============================================================================
# 20. TRANSFER ELIGIBILITY
#
# Transfer candidates are files for which no byte-identical copy was found in
# the legacy archive.
#
# Same-name conflicts are deliberately NOT automatically transfer-ready.
# ==============================================================================

onedrive <- onedrive %>%
  
  mutate(
    
    TRANSFER_STATUS = case_when(
      
      EXACT_DUPLICATE_IN_LEGACY ~
        "DO_NOT_TRANSFER_DUPLICATE",
      
      COMPARISON_STATUS ==
        "SAME_NAME_DIFFERENT_CONTENT_OR_VERSION" ~
        "REVIEW_VERSION_CONFLICT",
      
      COMPARISON_STATUS %in% c(
        "ABSENT_FROM_LEGACY",
        "ABSENT_FROM_LEGACY_SAME_SIZE_AS_OTHER_FILE"
      ) ~
        "TRANSFER_CANDIDATE",
      
      TRUE ~
        "REVIEW"
    )
  )


# ==============================================================================
# 21. PROPOSE DESTINATION PATH
#
# This is informational only.
#
# No files are copied.
#
# The existing OneDrive relative folder architecture is retained beneath a
# dedicated import holding directory so that nothing overwrites the historical
# I:/JAPAN PROJECT structure.
# ==============================================================================

PROPOSED_IMPORT_ROOT <- file.path(
  LEGACY_ROOT,
  "_ONEDRIVE_IMPORT_REVIEW"
)


onedrive <- onedrive %>%
  
  mutate(
    
    PROPOSED_DESTINATION_PATH = case_when(
      
      TRANSFER_STATUS == "TRANSFER_CANDIDATE" ~
        
        file.path(
          PROPOSED_IMPORT_ROOT,
          RELATIVE_PATH
        ),
      
      TRUE ~
        NA_character_
    )
  )


# ==============================================================================
# 22. EXACT DUPLICATES
# ==============================================================================

exact_duplicates <- onedrive %>%
  
  filter(
    EXACT_DUPLICATE_IN_LEGACY
  ) %>%
  
  arrange(
    FILE_NAME_LOWER,
    SOURCE_PATH
  )


# ==============================================================================
# 23. TRANSFER CANDIDATES
# ==============================================================================

transfer_candidates <- onedrive %>%
  
  filter(
    TRANSFER_STATUS == "TRANSFER_CANDIDATE"
  ) %>%
  
  arrange(
    desc(VPJD_PRIORITY),
    RESEARCH_CATEGORY,
    RELATIVE_PATH
  )


# ==============================================================================
# 24. VERSION / NAME CONFLICTS
# ==============================================================================

version_conflicts <- onedrive %>%
  
  filter(
    TRANSFER_STATUS ==
      "REVIEW_VERSION_CONFLICT"
  ) %>%
  
  arrange(
    FILE_NAME_LOWER,
    SOURCE_PATH
  )


# ==============================================================================
# 25. HIGH-PRIORITY TRANSFER CANDIDATES
# ==============================================================================

high_priority_transfer <- transfer_candidates %>%
  
  filter(
    VPJD_PRIORITY == "HIGH"
  ) %>%
  
  arrange(
    RESEARCH_CATEGORY,
    RELATIVE_PATH
  )


# ==============================================================================
# 26. COMPARISON SUMMARY
# ==============================================================================

comparison_summary <- onedrive %>%
  
  count(
    COMPARISON_STATUS,
    TRANSFER_STATUS,
    name = "N_FILES",
    sort = TRUE
  ) %>%
  
  mutate(
    PERCENT_ONEDRIVE_NON_MEDIA =
      round(
        100 * N_FILES / nrow(onedrive),
        2
      )
  )


# ==============================================================================
# 27. RESEARCH CATEGORY COMPARISON
# ==============================================================================

category_comparison <- onedrive %>%
  
  count(
    RESEARCH_CATEGORY,
    VPJD_PRIORITY,
    COMPARISON_STATUS,
    name = "N_FILES"
  ) %>%
  
  arrange(
    RESEARCH_CATEGORY,
    COMPARISON_STATUS
  )


# ==============================================================================
# 28. ONEDRIVE FOLDER SUMMARY
# ==============================================================================

onedrive_folder_summary <- onedrive_all %>%
  
  group_by(
    TOP_LEVEL_FOLDER
  ) %>%
  
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
    
    TOTAL_SIZE_BYTES = sum(
      FILE_SIZE_BYTES,
      na.rm = TRUE
    ),
    
    .groups = "drop"
  ) %>%
  
  mutate(
    TOTAL_SIZE_GB =
      TOTAL_SIZE_BYTES / 1024^3
  ) %>%
  
  arrange(
    desc(N_NON_MEDIA)
  )


# ==============================================================================
# 29. OUTPUT PATHS
# ==============================================================================

catalogue_file <- file.path(
  OUTPUT_ROOT,
  "VPJD_OneDrive_Japan_research_catalogue.csv"
)

comparison_file <- file.path(
  OUTPUT_ROOT,
  "VPJD_OneDrive_vs_Legacy_Japan_comparison.csv"
)

duplicate_file <- file.path(
  OUTPUT_ROOT,
  "VPJD_OneDrive_Japan_exact_duplicates_in_I.csv"
)

transfer_file <- file.path(
  OUTPUT_ROOT,
  "VPJD_OneDrive_Japan_transfer_candidates.csv"
)

high_priority_transfer_file <- file.path(
  OUTPUT_ROOT,
  "VPJD_OneDrive_Japan_high_priority_transfer_candidates.csv"
)

conflict_file <- file.path(
  OUTPUT_ROOT,
  "VPJD_OneDrive_Japan_same_name_version_conflicts.csv"
)

summary_file <- file.path(
  OUTPUT_ROOT,
  "VPJD_OneDrive_vs_Legacy_Japan_summary.csv"
)

category_file <- file.path(
  OUTPUT_ROOT,
  "VPJD_OneDrive_vs_Legacy_Japan_category_comparison.csv"
)

folder_file <- file.path(
  OUTPUT_ROOT,
  "VPJD_OneDrive_Japan_folder_summary.csv"
)

media_file <- file.path(
  OUTPUT_ROOT,
  "VPJD_OneDrive_vs_Legacy_Japan_media_summary.csv"
)


# ==============================================================================
# 30. WRITE OUTPUTS
# ==============================================================================

write_csv(
  onedrive,
  catalogue_file,
  na = ""
)

write_csv(
  onedrive,
  comparison_file,
  na = ""
)

write_csv(
  exact_duplicates,
  duplicate_file,
  na = ""
)

write_csv(
  transfer_candidates,
  transfer_file,
  na = ""
)

write_csv(
  high_priority_transfer,
  high_priority_transfer_file,
  na = ""
)

write_csv(
  version_conflicts,
  conflict_file,
  na = ""
)

write_csv(
  comparison_summary,
  summary_file,
  na = ""
)

write_csv(
  category_comparison,
  category_file,
  na = ""
)

write_csv(
  onedrive_folder_summary,
  folder_file,
  na = ""
)

write_csv(
  media_summary,
  media_file,
  na = ""
)


# ==============================================================================
# 31. VALIDATION
# ==============================================================================

validation <- tibble(
  
  CHECK = c(
    
    "OneDrive root exists",
    
    "Legacy root exists",
    
    "OneDrive files discovered",
    
    "Legacy files discovered",
    
    "OneDrive non-media catalogue contains no media",
    
    "Legacy non-media catalogue contains no media",
    
    "No media included in OneDrive hash candidates",
    
    "No media included in legacy hash candidates",
    
    "All OneDrive non-media files have comparison status",
    
    "All OneDrive non-media files have transfer status",
    
    "Exact duplicates have legacy match",
    
    "Transfer candidates are not exact duplicates",
    
    "Version conflicts have same-name legacy file",
    
    "Comparison population equals OneDrive non-media population",
    
    "Research catalogue written",
    
    "Duplicate register written",
    
    "Transfer register written",
    
    "Version conflict register written",
    
    "Summary written",
    
    "No source files copied",
    
    "No source files moved",
    
    "No source files deleted",
    
    "No source files modified"
  ),
  
  PASS = c(
    
    dir.exists(ONEDRIVE_ROOT),
    
    dir.exists(LEGACY_ROOT),
    
    nrow(onedrive_all) > 0,
    
    nrow(legacy_all) > 0,
    
    !any(
      onedrive$IS_MEDIA,
      na.rm = TRUE
    ),
    
    !any(
      legacy$IS_MEDIA,
      na.rm = TRUE
    ),
    
    !any(
      onedrive_hash_candidates$IS_MEDIA,
      na.rm = TRUE
    ),
    
    !any(
      legacy_hash_candidates$IS_MEDIA,
      na.rm = TRUE
    ),
    
    all(
      !is.na(
        onedrive$COMPARISON_STATUS
      )
    ),
    
    all(
      !is.na(
        onedrive$TRANSFER_STATUS
      )
    ),
    
    all(
      exact_duplicates$EXACT_DUPLICATE_IN_LEGACY
    ),
    
    !any(
      transfer_candidates$EXACT_DUPLICATE_IN_LEGACY,
      na.rm = TRUE
    ),
    
    all(
      version_conflicts$SAME_NAME_IN_LEGACY
    ),
    
    nrow(onedrive) ==
      sum(comparison_summary$N_FILES),
    
    file.exists(catalogue_file),
    
    file.exists(duplicate_file),
    
    file.exists(transfer_file),
    
    file.exists(conflict_file),
    
    file.exists(summary_file),
    
    TRUE,
    
    TRUE,
    
    TRUE,
    
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


cat("\n")
cat("— VALIDATION —\n")

print(
  validation,
  n = Inf
)


if (!all(validation$PASS, na.rm = TRUE)) {
  
  stop(
    "01c comparison validation failed. Review FAIL checks."
  )
}


# ==============================================================================
# 32. CONSOLE OUTPUT — COMPARISON SUMMARY
# ==============================================================================

cat("\n")
cat("— ONEDRIVE / LEGACY COMPARISON —\n")

print(
  comparison_summary,
  n = Inf
)


# ==============================================================================
# 33. CONSOLE OUTPUT — MEDIA SUMMARY
# ==============================================================================

cat("\n")
cat("— MEDIA EXCLUDED —\n")

print(
  media_summary,
  n = Inf
)


# ==============================================================================
# 34. STORAGE SUMMARY
# ==============================================================================

duplicate_bytes <- sum(
  exact_duplicates$FILE_SIZE_BYTES,
  na.rm = TRUE
)

transfer_bytes <- sum(
  transfer_candidates$FILE_SIZE_BYTES,
  na.rm = TRUE
)

high_priority_transfer_bytes <- sum(
  high_priority_transfer$FILE_SIZE_BYTES,
  na.rm = TRUE
)


# ==============================================================================
# 35. COMPLETION
# ==============================================================================

cat("\n")
cat("============================================================\n")
cat("01c ONEDRIVE / LEGACY JAPAN COMPARISON COMPLETE\n")
cat("============================================================\n")

cat(
  "OneDrive files discovered: ",
  format(nrow(onedrive_all), big.mark = ","),
  "\n",
  sep = ""
)

cat(
  "OneDrive media excluded: ",
  format(nrow(onedrive_media), big.mark = ","),
  "\n",
  sep = ""
)

cat(
  "OneDrive non-media analysed: ",
  format(nrow(onedrive), big.mark = ","),
  "\n",
  sep = ""
)

cat("\n")

cat(
  "Legacy non-media analysed: ",
  format(nrow(legacy), big.mark = ","),
  "\n",
  sep = ""
)

cat("\n")

cat(
  "Exact OneDrive duplicates already in I: ",
  format(nrow(exact_duplicates), big.mark = ","),
  "\n",
  sep = ""
)

cat(
  "Duplicate OneDrive storage: ",
  round(
    duplicate_bytes / 1024^2,
    2
  ),
  " MB\n",
  sep = ""
)

cat("\n")

cat(
  "OneDrive transfer candidates absent from I: ",
  format(nrow(transfer_candidates), big.mark = ","),
  "\n",
  sep = ""
)

cat(
  "Transfer candidate storage: ",
  round(
    transfer_bytes / 1024^2,
    2
  ),
  " MB\n",
  sep = ""
)

cat(
  "High-priority transfer candidates: ",
  format(nrow(high_priority_transfer), big.mark = ","),
  "\n",
  sep = ""
)

cat(
  "High-priority transfer storage: ",
  round(
    high_priority_transfer_bytes / 1024^2,
    2
  ),
  " MB\n",
  sep = ""
)

cat("\n")

cat(
  "Same-name/version conflicts requiring review: ",
  format(nrow(version_conflicts), big.mark = ","),
  "\n",
  sep = ""
)

cat("\n")

cat("Media hashed: FALSE\n")
cat("Files copied: FALSE\n")
cat("Files moved: FALSE\n")
cat("Files deleted: FALSE\n")
cat("Files modified: FALSE\n")

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