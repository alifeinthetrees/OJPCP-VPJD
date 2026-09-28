# =============================================================================
# VPJD-OJPCP
# R/stars/03j_inspect_legacy_geography_dbfs.R
# Version 0.1.0
#
# INSPECT LEGACY GEOGRAPHY DBF STRUCTURE
#
# PURPOSE
# - Forensically inspect legacy geography DBFs for evidence relevant to the
#   geographical terminology used by the original Key to Stars.
# - Identify schemas, place/location types, hierarchy fields and terminology.
# - Search specifically for evidence concerning "district" and related
#   Japanese administrative/geographical units.
#
# IMPORTANT
# - DIAGNOSTIC / RESEARCH ONLY.
# - Legacy DBFs are NOT analytical inputs to the contemporary VPJD.
# - Nothing in this script assigns districts or Stars.
# - Nothing in this script modifies canonical VPJD tables.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(tibble)
  library(here)
})

MODULE <- "stars_03j_inspect_legacy_geography_dbfs"
VERSION <- "0.1.0"
RUN_DATE <- Sys.Date()

DBF_DIR <- here("R", "Geography")
OUTPUT_DIR <- here("outputs", "tables", "stars", "methodology", "legacy_geography")

DBF_FILES <- c(
  "NON-STDAREAS.dbf",
  "SHIKOKU.DBF",
  "STDAREAS.DBF",
  "POINTLOCS.DBF",
  "ISO2S.DBF",
  "LOCTYPES.DBF",
  "MYPLACES.DBF",
  "PLACETYPES.DBF"
)

SEARCH_TERMS <- c(
  "district",
  "gun",
  "郡",
  "prefecture",
  "ken",
  "県",
  "fu",
  "府",
  "to",
  "都",
  "do",
  "道",
  "subprefecture",
  "sub-prefecture",
  "shicho",
  "支庁",
  "city",
  "shi",
  "市",
  "town",
  "cho",
  "chō",
  "machi",
  "町",
  "village",
  "son",
  "mura",
  "村",
  "ward",
  "ku",
  "区"
)

read_dbf_safe <- function(path) {
  if (!requireNamespace("foreign", quietly = TRUE)) {
    stop(
      "Package 'foreign' is required. Install with:\n",
      "install.packages('foreign')"
    )
  }
  
  foreign::read.dbf(
    path,
    as.is = TRUE
  ) |>
    as_tibble()
}

field_profile <- function(x, file_name) {
  tibble(
    FILE = file_name,
    FIELD = names(x),
    CLASS = vapply(
      x,
      function(z) paste(class(z), collapse = "/"),
      character(1)
    ),
    N_ROWS = nrow(x),
    N_NON_MISSING = vapply(
      x,
      function(z) sum(!is.na(z)),
      integer(1)
    ),
    N_UNIQUE = vapply(
      x,
      function(z) n_distinct(z, na.rm = TRUE),
      integer(1)
    )
  )
}

search_character_fields <- function(x, file_name, terms) {
  char_fields <- names(x)[
    vapply(
      x,
      function(z) is.character(z) || is.factor(z),
      logical(1)
    )
  ]
  
  if (length(char_fields) == 0L) {
    return(
      tibble(
        FILE = character(),
        ROW_NUMBER = integer(),
        FIELD = character(),
        SEARCH_TERM = character(),
        VALUE = character()
      )
    )
  }
  
  results <- list()
  k <- 1L
  
  for (field in char_fields) {
    values <- as.character(x[[field]])
    
    for (term in terms) {
      hits <- which(
        !is.na(values) &
          str_detect(
            str_to_lower(values),
            fixed(
              str_to_lower(term)
            )
          )
      )
      
      if (length(hits) > 0L) {
        results[[k]] <- tibble(
          FILE = file_name,
          ROW_NUMBER = hits,
          FIELD = field,
          SEARCH_TERM = term,
          VALUE = values[hits]
        )
        
        k <- k + 1L
      }
    }
  }
  
  if (length(results) == 0L) {
    return(
      tibble(
        FILE = character(),
        ROW_NUMBER = integer(),
        FIELD = character(),
        SEARCH_TERM = character(),
        VALUE = character()
      )
    )
  }
  
  bind_rows(results)
}

profile_character_values <- function(x, file_name) {
  char_fields <- names(x)[
    vapply(
      x,
      function(z) is.character(z) || is.factor(z),
      logical(1)
    )
  ]
  
  if (length(char_fields) == 0L) {
    return(
      tibble(
        FILE = character(),
        FIELD = character(),
        VALUE = character(),
        N = integer()
      )
    )
  }
  
  bind_rows(
    lapply(
      char_fields,
      function(field) {
        tibble(
          VALUE = as.character(x[[field]])
        ) |>
          filter(
            !is.na(VALUE),
            VALUE != ""
          ) |>
          count(
            VALUE,
            name = "N",
            sort = TRUE
          ) |>
          mutate(
            FILE = file_name,
            FIELD = field,
            .before = 1
          )
      }
    )
  )
}

run_stars_03j <- function() {
  cat("\n— Inspect legacy geography DBFs —\n\n")
  cat("Run date: ", as.character(RUN_DATE), "\n", sep = "")
  cat("Module: ", MODULE, "\n", sep = "")
  cat("Version: ", VERSION, "\n", sep = "")
  cat("DBF directory: ", DBF_DIR, "\n\n", sep = "")
  
  dir.create(
    OUTPUT_DIR,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  paths <- file.path(
    DBF_DIR,
    DBF_FILES
  )
  
  availability <- tibble(
    FILE = DBF_FILES,
    PATH = paths,
    EXISTS = file.exists(paths),
    SIZE_BYTES = ifelse(
      file.exists(paths),
      file.info(paths)$size,
      NA_real_
    )
  )
  
  cat("— File availability —\n")
  print.data.frame(
    availability,
    row.names = FALSE
  )
  
  missing <- availability |>
    filter(!EXISTS)
  
  if (nrow(missing) > 0L) {
    stop(
      "Expected DBF files missing: ",
      paste(
        missing$FILE,
        collapse = ", "
      )
    )
  }
  
  dbfs <- setNames(
    lapply(
      paths,
      read_dbf_safe
    ),
    DBF_FILES
  )
  
  dimensions <- bind_rows(
    lapply(
      names(dbfs),
      function(file_name) {
        x <- dbfs[[file_name]]
        
        tibble(
          FILE = file_name,
          ROWS = nrow(x),
          COLUMNS = ncol(x)
        )
      }
    )
  )
  
  cat("\n— DBF dimensions —\n")
  print.data.frame(
    dimensions,
    row.names = FALSE
  )
  
  schemas <- bind_rows(
    lapply(
      names(dbfs),
      function(file_name) {
        field_profile(
          dbfs[[file_name]],
          file_name
        )
      }
    )
  )
  
  cat("\n— DBF schemas —\n")
  print.data.frame(
    schemas |>
      select(
        FILE,
        FIELD,
        CLASS,
        N_UNIQUE
      ),
    row.names = FALSE
  )
  
  values <- bind_rows(
    lapply(
      names(dbfs),
      function(file_name) {
        profile_character_values(
          dbfs[[file_name]],
          file_name
        )
      }
    )
  )
  
  search_hits <- bind_rows(
    lapply(
      names(dbfs),
      function(file_name) {
        search_character_fields(
          dbfs[[file_name]],
          file_name,
          SEARCH_TERMS
        )
      }
    )
  )
  
  cat("\n— Administrative/geographical terminology hits —\n")
  
  if (nrow(search_hits) == 0L) {
    cat("No search terms found.\n")
  } else {
    hit_summary <- search_hits |>
      count(
        FILE,
        FIELD,
        SEARCH_TERM,
        name = "HITS",
        sort = TRUE
      )
    
    print.data.frame(
      hit_summary,
      row.names = FALSE
    )
  }
  
  # ===========================================================================
  # Targeted inspection of files most likely to define place semantics
  # ===========================================================================
  
  priority_files <- intersect(
    c(
      "PLACETYPES.DBF",
      "LOCTYPES.DBF",
      "MYPLACES.DBF",
      "POINTLOCS.DBF",
      "STDAREAS.DBF",
      "NON-STDAREAS.dbf"
    ),
    names(dbfs)
  )
  
  cat("\n— Priority DBF previews —\n")
  
  for (file_name in priority_files) {
    cat("\n### ", file_name, "\n", sep = "")
    
    print.data.frame(
      head(
        dbfs[[file_name]],
        20
      ),
      row.names = FALSE
    )
  }
  
  # ===========================================================================
  # Identify fields potentially encoding hierarchy or administrative level
  # ===========================================================================
  
  hierarchy_pattern <- paste(
    c(
      "type",
      "level",
      "parent",
      "place",
      "loc",
      "area",
      "district",
      "pref",
      "admin",
      "region",
      "code",
      "id"
    ),
    collapse = "|"
  )
  
  hierarchy_fields <- schemas |>
    filter(
      str_detect(
        str_to_lower(FIELD),
        hierarchy_pattern
      )
    )
  
  cat("\n— Potential hierarchy fields —\n")
  print.data.frame(
    hierarchy_fields |>
      select(
        FILE,
        FIELD,
        CLASS,
        N_UNIQUE
      ),
    row.names = FALSE
  )
  
  # ===========================================================================
  # Explicit district/gun evidence
  # ===========================================================================
  
  district_hits <- search_hits |>
    filter(
      str_to_lower(SEARCH_TERM) %in%
        c(
          "district",
          "gun",
          "郡"
        )
    )
  
  cat("\n— Explicit district / gun evidence —\n")
  
  if (nrow(district_hits) == 0L) {
    cat("No explicit district/gun terminology found.\n")
  } else {
    print.data.frame(
      district_hits |>
        distinct(
          FILE,
          FIELD,
          SEARCH_TERM,
          VALUE
        ),
      row.names = FALSE
    )
  }
  
  # ===========================================================================
  # Export diagnostics
  # ===========================================================================
  
  write_csv(
    availability,
    file.path(
      OUTPUT_DIR,
      "stars03j_dbf_availability.csv"
    )
  )
  
  write_csv(
    dimensions,
    file.path(
      OUTPUT_DIR,
      "stars03j_dbf_dimensions.csv"
    )
  )
  
  write_csv(
    schemas,
    file.path(
      OUTPUT_DIR,
      "stars03j_dbf_schemas.csv"
    )
  )
  
  write_csv(
    values,
    file.path(
      OUTPUT_DIR,
      "stars03j_character_value_profile.csv"
    )
  )
  
  write_csv(
    search_hits,
    file.path(
      OUTPUT_DIR,
      "stars03j_geographic_term_hits.csv"
    )
  )
  
  write_csv(
    hierarchy_fields,
    file.path(
      OUTPUT_DIR,
      "stars03j_potential_hierarchy_fields.csv"
    )
  )
  
  write_csv(
    district_hits,
    file.path(
      OUTPUT_DIR,
      "stars03j_district_gun_hits.csv"
    )
  )
  
  metadata <- tibble(
    METRIC = c(
      "module",
      "version",
      "run_date",
      "dbf_files_examined",
      "total_rows_examined",
      "total_fields_examined",
      "geographic_term_hits",
      "district_gun_hits",
      "analytical_tables_modified",
      "stars_assigned",
      "legacy_data_used_as_analytical_input"
    ),
    VALUE = c(
      MODULE,
      VERSION,
      as.character(RUN_DATE),
      as.character(length(dbfs)),
      as.character(sum(dimensions$ROWS)),
      as.character(nrow(schemas)),
      as.character(nrow(search_hits)),
      as.character(nrow(district_hits)),
      "FALSE",
      "FALSE",
      "FALSE"
    )
  )
  
  write_csv(
    metadata,
    file.path(
      OUTPUT_DIR,
      "stars03j_metadata.csv"
    )
  )
  
  cat("\n— Summary —\n")
  cat(
    "DBFs examined: ",
    length(dbfs),
    "\n",
    sep = ""
  )
  cat(
    "Rows examined: ",
    format(
      sum(dimensions$ROWS),
      big.mark = ","
    ),
    "\n",
    sep = ""
  )
  cat(
    "Fields examined: ",
    nrow(schemas),
    "\n",
    sep = ""
  )
  cat(
    "Geographic terminology hits: ",
    format(
      nrow(search_hits),
      big.mark = ","
    ),
    "\n",
    sep = ""
  )
  cat(
    "Explicit district/gun hits: ",
    format(
      nrow(district_hits),
      big.mark = ","
    ),
    "\n",
    sep = ""
  )
  
  cat(
    "\nStars 03j v",
    VERSION,
    " complete.\n",
    sep = ""
  )
  
  cat(
    "DIAGNOSTIC ONLY — no legacy geography has entered the contemporary analytical pipeline.\n"
  )
  
  invisible(
    list(
      availability = availability,
      dimensions = dimensions,
      schemas = schemas,
      values = values,
      search_hits = search_hits,
      hierarchy_fields = hierarchy_fields,
      district_hits = district_hits,
      metadata = metadata
    )
  )
}

result_stars_03j <- run_stars_03j()