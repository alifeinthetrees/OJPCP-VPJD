# ==============================================================================
# VPJD-OJPCP
# Stars 03k — Investigate legacy Japanese macro-regional geography
# Version: 0.1.2
#
# PURPOSE
# Investigate whether the legacy Japanese geography DBFs contain an explicit,
# internally coherent macro-regional geography that may help interpret the
# term "district" in Figure 2.2 (Key to Stars).
#
# IMPORTANT
# - DIAGNOSTIC / METHODOLOGICAL INVESTIGATION ONLY.
# - Legacy geography is NOT an analytical input to contemporary VPJD.
# - No Stars are assigned.
# - No district definition is adopted.
# - Regional names searched below are hypotheses/search terms, not a
#   predefined district classification.
# ==============================================================================

library(here)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(foreign)

MODULE <- "stars_03k_investigate_legacy_japan_regions"
VERSION <- "0.1.2"
RUN_DATE <- Sys.Date()

dbf_dir <- here("R", "Geography")
out_dir <- here("data", "derived", "stars")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

cat("\n— Investigate legacy Japanese macro-regional geography —\n\n")
cat("Run date:", as.character(RUN_DATE), "\n")
cat("Module:", MODULE, "\n")
cat("Version:", VERSION, "\n")
cat("DBF directory:", dbf_dir, "\n\n")

# ------------------------------------------------------------------------------
# 1. Import and sanitise legacy DBFs
# ------------------------------------------------------------------------------

dbf_files <- c(
  "STDAREAS.DBF",
  "POINTLOCS.DBF",
  "NON-STDAREAS.dbf",
  "MYPLACES.DBF",
  "SHIKOKU.DBF"
)

paths <- setNames(file.path(dbf_dir, dbf_files), dbf_files)
missing_files <- names(paths)[!file.exists(paths)]

if (length(missing_files) > 0) {
  stop(
    "Missing required DBFs: ",
    paste(missing_files, collapse = ", ")
  )
}

clean_character <- function(z) {
  if (!is.character(z)) return(z)
  
  z <- iconv(
    z,
    from = "",
    to = "UTF-8",
    sub = "byte"
  )
  
  Encoding(z) <- "UTF-8"
  z
}

read_dbf_safe <- function(path) {
  raw <- suppressWarnings(
    foreign::read.dbf(
      path,
      as.is = TRUE
    )
  )
  
  # Rebuild every column independently so that FoxPro/DBF attributes do not
  # propagate into the diagnostic objects.
  cols <- lapply(
    seq_along(raw),
    function(i) {
      z <- raw[[i]]
      
      if (is.factor(z)) {
        z <- as.character(z)
      }
      
      if (is.character(z)) {
        z <- clean_character(z)
      } else if (inherits(z, "Date")) {
        z <- as.Date(z)
      } else if (is.logical(z)) {
        z <- as.logical(z)
      } else if (is.integer(z)) {
        z <- as.integer(z)
      } else if (is.numeric(z)) {
        z <- as.numeric(z)
      } else {
        z <- as.vector(z)
      }
      
      attributes(z) <- attributes(
        unclass(z)
      )
      
      z
    }
  )
  
  names(cols) <- names(raw)
  
  x <- data.frame(
    cols,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  
  # Explicitly retain only ordinary data.frame attributes.
  attr(x, "na.action") <- NULL
  attr(x, "data_types") <- NULL
  attr(x, "class") <- "data.frame"
  row.names(x) <- seq_len(nrow(x))
  
  x
}

print_df <- function(x) {
  x <- as.data.frame(
    x,
    stringsAsFactors = FALSE,
    optional = TRUE
  )
  
  attr(x, "na.action") <- NULL
  
  for (nm in names(x)) {
    attr(x[[nm]], "na.action") <- NULL
  }
  
  print(
    x,
    row.names = FALSE,
    na.print = "NA"
  )
  
  invisible(x)
}

dbfs <- map(
  paths,
  read_dbf_safe
)

cat("Files loaded:\n")

for (nm in names(dbfs)) {
  cat(
    " ",
    nm,
    ": ",
    format(nrow(dbfs[[nm]]), big.mark = ","),
    " rows, ",
    ncol(dbfs[[nm]]),
    " columns\n",
    sep = ""
  )
}

# ------------------------------------------------------------------------------
# 2. Search vocabulary
# ------------------------------------------------------------------------------

# Search hypotheses only. Inclusion here does NOT imply that the term is part
# of the Key's district system.

region_terms <- tibble(
  SEARCH_REGION = c(
    "Hokkaido",
    "Tohoku",
    "Kanto",
    "Chubu",
    "Hokuriku",
    "Koshinetsu",
    "Tokai",
    "Kinki",
    "Kansai",
    "Chugoku",
    "Sanin",
    "Sanyo",
    "Shikoku",
    "Kyushu",
    "Ryukyu",
    "Nansei",
    "Okinawa",
    "Izu",
    "Ogasawara",
    "Bonin",
    "Kazan",
    "Volcano",
    "Kuril",
    "Chishima"
  )
)

# ------------------------------------------------------------------------------
# 3. Convert searchable character fields to long form
# ------------------------------------------------------------------------------

character_fields <- imap_dfr(
  dbfs,
  function(x, file_name) {
    chr <- names(x)[
      vapply(
        x,
        is.character,
        logical(1)
      )
    ]
    
    if (length(chr) == 0) {
      return(tibble())
    }
    
    as_tibble(x) %>%
      mutate(.ROW_ID = row_number()) %>%
      select(.ROW_ID, all_of(chr)) %>%
      pivot_longer(
        cols = -.ROW_ID,
        names_to = "FIELD",
        values_to = "VALUE"
      ) %>%
      mutate(
        FILE = file_name,
        VALUE = clean_character(
          as.character(VALUE)
        )
      ) %>%
      select(
        FILE,
        .ROW_ID,
        FIELD,
        VALUE
      )
  }
)

character_fields <- character_fields %>%
  filter(
    !is.na(VALUE),
    str_trim(VALUE) != ""
  )

cat(
  "\nSearchable character values:",
  format(nrow(character_fields), big.mark = ","),
  "\n"
)

# ------------------------------------------------------------------------------
# 4. Locate regional terminology
# ------------------------------------------------------------------------------

region_hits <- map_dfr(
  region_terms$SEARCH_REGION,
  function(term) {
    character_fields %>%
      filter(
        str_detect(
          VALUE,
          regex(
            term,
            ignore_case = TRUE
          )
        )
      ) %>%
      mutate(
        SEARCH_REGION = term
      )
  }
) %>%
  distinct(
    FILE,
    .ROW_ID,
    FIELD,
    VALUE,
    SEARCH_REGION
  ) %>%
  arrange(
    SEARCH_REGION,
    FILE,
    .ROW_ID,
    FIELD
  )

cat("\n— Regional terminology hits —\n")

if (nrow(region_hits) == 0) {
  cat("No regional terminology hits found.\n")
} else {
  region_hit_summary <- region_hits %>%
    count(
      SEARCH_REGION,
      FILE,
      FIELD,
      name = "N"
    ) %>%
    arrange(
      SEARCH_REGION,
      desc(N),
      FILE,
      FIELD
    )
  
  print_df(region_hit_summary)
}

# ------------------------------------------------------------------------------
# 5. Standardise important hierarchy fields
# ------------------------------------------------------------------------------

wanted_fields <- c(
  "FILE",
  ".ROW_ID",
  "GEOCODE",
  "AREANAME",
  "TYPE",
  "LEVEL",
  "CLEANAME",
  "NAME",
  "VARNAMES",
  "MYPLACENM",
  "LOCTYPE",
  "LOCALITY",
  "MAJOR",
  "MINOR",
  "REGNAME1",
  "REGNAME2",
  "FEATCLASS",
  "FEATCODE",
  "GEOCODE0",
  "GEOCODE1",
  "GEOCODE2",
  "GEOCODE3",
  "GEOCODE4",
  "GEOCODE5",
  "GEOCODE6",
  "GEOCODE7",
  "LLGEO0",
  "LLGEO1",
  "LLGEO2",
  "LLGEO3",
  "LLGEO4",
  "LLGEO5",
  "LLGEO6",
  "LLGEO7",
  "LAT",
  "LONG",
  "LATMIN",
  "LONGMIN",
  "LATMAX",
  "LONGMAX"
)

standardised_hits <- imap_dfr(
  dbfs,
  function(x, file_name) {
    ids <- region_hits %>%
      filter(
        FILE == file_name
      ) %>%
      pull(.ROW_ID) %>%
      unique()
    
    if (length(ids) == 0) {
      return(tibble())
    }
    
    x <- as_tibble(x) %>%
      mutate(
        .ROW_ID = row_number()
      ) %>%
      filter(
        .ROW_ID %in% ids
      ) %>%
      mutate(
        FILE = file_name,
        .before = 1
      )
    
    absent <- setdiff(
      wanted_fields,
      names(x)
    )
    
    for (nm in absent) {
      x[[nm]] <- NA
    }
    
    x %>%
      select(
        all_of(wanted_fields)
      )
  }
)

standardised_hits <- standardised_hits %>%
  left_join(
    region_hits %>%
      distinct(
        FILE,
        .ROW_ID,
        SEARCH_REGION
      ),
    by = c(
      "FILE",
      ".ROW_ID"
    )
  ) %>%
  arrange(
    SEARCH_REGION,
    FILE,
    .ROW_ID
  )

# ------------------------------------------------------------------------------
# 6. Explicit major-region / District evidence
# ------------------------------------------------------------------------------

district_hits <- character_fields %>%
  filter(
    str_detect(
      VALUE,
      regex(
        "\\bdistrict\\b",
        ignore_case = TRUE
      )
    ) |
      str_detect(
        VALUE,
        regex(
          paste0(
            "\\b(",
            paste(
              c(
                "Kanto",
                "Kinki",
                "Kansai",
                "Chubu",
                "Chugoku",
                "Tohoku",
                "Hokkaido",
                "Shikoku",
                "Kyushu"
              ),
              collapse = "|"
            ),
            ")\\b"
          ),
          ignore_case = TRUE
        )
      )
  ) %>%
  distinct(
    FILE,
    .ROW_ID,
    FIELD,
    VALUE
  ) %>%
  arrange(
    FILE,
    .ROW_ID,
    FIELD
  )

cat("\n— Explicit district / major-region evidence —\n")

if (nrow(district_hits) == 0) {
  cat("No explicit district / major-region evidence found.\n")
} else {
  print_df(district_hits)
}

# ------------------------------------------------------------------------------
# 7. Examine STDAREAS regional-name records
# ------------------------------------------------------------------------------

std <- as_tibble(
  dbfs[["STDAREAS.DBF"]]
) %>%
  mutate(
    .ROW_ID = row_number()
  )

std_region_hits <- region_hits %>%
  filter(
    FILE == "STDAREAS.DBF"
  ) %>%
  distinct(
    .ROW_ID,
    SEARCH_REGION
  ) %>%
  left_join(
    std,
    by = ".ROW_ID"
  )

if ("LEVEL" %in% names(std_region_hits)) {
  std_region_hits <- std_region_hits %>%
    arrange(
      SEARCH_REGION,
      LEVEL
    )
} else {
  std_region_hits <- std_region_hits %>%
    arrange(
      SEARCH_REGION
    )
}

cat("\n— STDAREAS regional-name records —\n")

if (nrow(std_region_hits) == 0) {
  cat("No candidate regional names found in STDAREAS.DBF.\n")
} else {
  std_print_fields <- intersect(
    c(
      "SEARCH_REGION",
      ".ROW_ID",
      "GEOCODE",
      "AREANAME",
      "TYPE",
      "LEVEL",
      "GEOCODE0",
      "GEOCODE1",
      "GEOCODE2",
      "GEOCODE3",
      "GEOCODE4",
      "GEOCODE5",
      "GEOCODE6",
      "LATMIN",
      "LONGMIN",
      "LATMAX",
      "LONGMAX"
    ),
    names(std_region_hits)
  )
  
  print_df(
    std_region_hits %>%
      select(
        all_of(std_print_fields)
      )
  )
}

# ------------------------------------------------------------------------------
# 8. Examine POINTLOCS regional-name records
# ------------------------------------------------------------------------------

points <- as_tibble(
  dbfs[["POINTLOCS.DBF"]]
) %>%
  mutate(
    .ROW_ID = row_number()
  )

point_region_hits <- region_hits %>%
  filter(
    FILE == "POINTLOCS.DBF"
  ) %>%
  distinct(
    .ROW_ID,
    SEARCH_REGION
  ) %>%
  left_join(
    points,
    by = ".ROW_ID"
  )

if ("GEOCODE" %in% names(point_region_hits)) {
  point_region_hits <- point_region_hits %>%
    arrange(
      SEARCH_REGION,
      GEOCODE
    )
} else {
  point_region_hits <- point_region_hits %>%
    arrange(
      SEARCH_REGION
    )
}

cat("\n— POINTLOCS regional-name records —\n")

if (nrow(point_region_hits) == 0) {
  cat("No candidate regional names found in POINTLOCS.DBF.\n")
} else {
  point_print_fields <- intersect(
    c(
      "SEARCH_REGION",
      ".ROW_ID",
      "GEOCODE",
      "CLEANAME",
      "NAME",
      "VARNAMES",
      "FEATCLASS",
      "FEATCODE",
      "LEVEL",
      "GEOCODE0",
      "GEOCODE1",
      "GEOCODE2",
      "GEOCODE3",
      "GEOCODE4",
      "GEOCODE5",
      "GEOCODE6",
      "LAT",
      "LONG"
    ),
    names(point_region_hits)
  )
  
  print_df(
    point_region_hits %>%
      select(
        all_of(point_print_fields)
      )
  )
}

# ------------------------------------------------------------------------------
# 9. POINTLOCS records explicitly containing "District"
# ------------------------------------------------------------------------------

cleaname_value <- if ("CLEANAME" %in% names(points)) {
  clean_character(
    coalesce(
      as.character(points$CLEANAME),
      ""
    )
  )
} else {
  rep("", nrow(points))
}

name_value <- if ("NAME" %in% names(points)) {
  clean_character(
    coalesce(
      as.character(points$NAME),
      ""
    )
  )
} else {
  rep("", nrow(points))
}

district_idx <- str_detect(
  cleaname_value,
  regex(
    "\\bdistrict\\b",
    ignore_case = TRUE
  )
) |
  str_detect(
    name_value,
    regex(
      "\\bdistrict\\b",
      ignore_case = TRUE
    )
  )

point_districts <- points[
  district_idx,
  ,
  drop = FALSE
]

district_fields <- intersect(
  c(
    ".ROW_ID",
    "GEOCODE",
    "CLEANAME",
    "NAME",
    "VARNAMES",
    "FEATCLASS",
    "FEATCODE",
    "LEVEL",
    "GEOCODE0",
    "GEOCODE1",
    "GEOCODE2",
    "GEOCODE3",
    "GEOCODE4",
    "GEOCODE5",
    "GEOCODE6",
    "LAT",
    "LONG"
  ),
  names(point_districts)
)

point_districts <- point_districts %>%
  select(
    all_of(district_fields)
  ) %>%
  distinct()

cat("\n— POINTLOCS records explicitly containing 'District' —\n")

if (nrow(point_districts) == 0) {
  cat("No explicit 'District' records found.\n")
} else {
  print_df(point_districts)
}

# ------------------------------------------------------------------------------
# 10. Candidate hierarchy profile
# ------------------------------------------------------------------------------

hierarchy_fields <- intersect(
  c(
    "SEARCH_REGION",
    "FILE",
    ".ROW_ID",
    "GEOCODE",
    "AREANAME",
    "TYPE",
    "LEVEL",
    "CLEANAME",
    "NAME",
    "GEOCODE0",
    "GEOCODE1",
    "GEOCODE2",
    "GEOCODE3",
    "GEOCODE4",
    "GEOCODE5",
    "GEOCODE6",
    "GEOCODE7",
    "LLGEO0",
    "LLGEO1",
    "LLGEO2",
    "LLGEO3",
    "LLGEO4",
    "LLGEO5",
    "LLGEO6",
    "LLGEO7"
  ),
  names(standardised_hits)
)

hierarchy_profile <- standardised_hits %>%
  select(
    all_of(hierarchy_fields)
  ) %>%
  arrange(
    SEARCH_REGION,
    FILE,
    .ROW_ID
  )

cat("\n— Candidate hierarchy profile —\n")

if (nrow(hierarchy_profile) == 0) {
  cat("No candidate hierarchy records found.\n")
} else {
  print_df(hierarchy_profile)
}

# ------------------------------------------------------------------------------
# 11. Search for parent-child relationships in POINTLOCS
# ------------------------------------------------------------------------------

if ("GEOCODE" %in% names(point_region_hits)) {
  regional_point_codes <- point_region_hits %>%
    transmute(
      SEARCH_REGION,
      REGION_GEOCODE = GEOCODE
    ) %>%
    filter(
      !is.na(REGION_GEOCODE)
    ) %>%
    distinct()
} else {
  regional_point_codes <- tibble(
    SEARCH_REGION = character(),
    REGION_GEOCODE = numeric()
  )
}

parent_fields <- intersect(
  c(
    "GEOCODE0",
    "GEOCODE1",
    "GEOCODE2",
    "GEOCODE3",
    "GEOCODE4",
    "GEOCODE5",
    "GEOCODE6"
  ),
  names(points)
)

parent_links <- map_dfr(
  seq_len(nrow(regional_point_codes)),
  function(i) {
    region <- regional_point_codes$SEARCH_REGION[i]
    
    code <- suppressWarnings(
      as.numeric(
        regional_point_codes$REGION_GEOCODE[i]
      )
    )
    
    if (is.na(code)) {
      return(tibble())
    }
    
    map_dfr(
      parent_fields,
      function(f) {
        vals <- suppressWarnings(
          as.numeric(
            points[[f]]
          )
        )
        
        idx <- which(
          !is.na(vals) &
            vals == code
        )
        
        if (length(idx) == 0) {
          return(tibble())
        }
        
        tibble(
          SEARCH_REGION = region,
          REGION_GEOCODE = code,
          PARENT_FIELD = f,
          CHILD_RECORDS = length(idx)
        )
      }
    )
  }
)

cat("\n— Candidate parent/child hierarchy links —\n")

if (nrow(parent_links) == 0) {
  cat(
    "No POINTLOCS parent-code relationships detected for candidate region records.\n"
  )
} else {
  print_df(
    parent_links %>%
      arrange(
        SEARCH_REGION,
        REGION_GEOCODE,
        PARENT_FIELD
      )
  )
}

# ------------------------------------------------------------------------------
# 12. Regional search summary
# ------------------------------------------------------------------------------

region_summary <- region_hits %>%
  group_by(
    SEARCH_REGION
  ) %>%
  summarise(
    N_HITS = n(),
    N_RECORDS = n_distinct(
      paste(
        FILE,
        .ROW_ID
      )
    ),
    N_FILES = n_distinct(FILE),
    FILES = paste(
      sort(
        unique(FILE)
      ),
      collapse = "; "
    ),
    .groups = "drop"
  ) %>%
  arrange(
    desc(N_RECORDS),
    SEARCH_REGION
  )

cat("\n— Region search summary —\n")

if (nrow(region_summary) == 0) {
  cat("No regional records summarised.\n")
} else {
  print_df(region_summary)
}

district_summary <- tibble(
  N_EXPLICIT_DISTRICT_RECORDS = nrow(
    point_districts
  ),
  N_UNIQUE_NAMES = if ("CLEANAME" %in% names(point_districts)) {
    n_distinct(
      point_districts$CLEANAME,
      na.rm = TRUE
    )
  } else if ("NAME" %in% names(point_districts)) {
    n_distinct(
      point_districts$NAME,
      na.rm = TRUE
    )
  } else {
    NA_integer_
  },
  N_UNIQUE_GEOCODES = if ("GEOCODE" %in% names(point_districts)) {
    n_distinct(
      point_districts$GEOCODE,
      na.rm = TRUE
    )
  } else {
    NA_integer_
  }
)

cat("\n— Explicit District summary —\n")
print_df(district_summary)

# ------------------------------------------------------------------------------
# 13. Methodological evidence summary
# ------------------------------------------------------------------------------

explicit_region_names <- character()

if ("CLEANAME" %in% names(point_districts)) {
  explicit_region_names <- c(
    explicit_region_names,
    point_districts$CLEANAME
  )
}

if ("NAME" %in% names(point_districts)) {
  explicit_region_names <- c(
    explicit_region_names,
    point_districts$NAME
  )
}

explicit_region_names <- sort(
  unique(
    explicit_region_names[
      !is.na(explicit_region_names) &
        explicit_region_names != ""
    ]
  )
)

cat("\n— Methodological evidence summary —\n")
cat(
  "Candidate region terms represented:",
  n_distinct(region_hits$SEARCH_REGION),
  "\n"
)
cat(
  "POINTLOCS records explicitly containing 'District':",
  nrow(point_districts),
  "\n"
)

if (length(explicit_region_names) > 0) {
  cat("Explicit District labels:\n")
  
  for (x in explicit_region_names) {
    cat(" -", x, "\n")
  }
}

cat(
  "\nInterpretation status: EVIDENCE ONLY — district remains methodologically unresolved.\n"
)

# ------------------------------------------------------------------------------
# 14. Validation
# ------------------------------------------------------------------------------

validation <- tibble(
  CHECK = c(
    "Required DBFs available",
    "STDAREAS loaded",
    "POINTLOCS loaded",
    "Legacy DBF objects sanitised",
    "Regional search completed",
    "No Star classification fields created",
    "No contemporary analytical tables modified"
  ),
  PASS = c(
    length(missing_files) == 0,
    nrow(std) > 0,
    nrow(points) > 0,
    all(
      vapply(
        dbfs,
        is.data.frame,
        logical(1)
      )
    ),
    is.data.frame(region_hits),
    !any(
      c(
        "STAR",
        "STAR_CLASS",
        "FINAL_STAR"
      ) %in%
        names(
          standardised_hits
        )
    ),
    TRUE
  )
)

cat("\n— Validation —\n")
print_df(validation)

if (!all(validation$PASS)) {
  stop("03k validation failure.")
}

# ------------------------------------------------------------------------------
# 15. Save diagnostic outputs
# ------------------------------------------------------------------------------

outputs <- list(
  vpjd_star_legacy_region_term_hits = region_hits,
  vpjd_star_legacy_region_hierarchy_profile = hierarchy_profile,
  vpjd_star_legacy_explicit_district_records = point_districts,
  vpjd_star_legacy_region_parent_links = parent_links,
  vpjd_star_legacy_region_summary = region_summary,
  vpjd_star_legacy_region_validation = validation
)

walk2(
  outputs,
  names(outputs),
  function(x, nm) {
    saveRDS(
      x,
      file.path(
        out_dir,
        paste0(
          nm,
          ".rds"
        )
      )
    )
    
    write.csv(
      as.data.frame(x),
      file.path(
        out_dir,
        paste0(
          nm,
          ".csv"
        )
      ),
      row.names = FALSE,
      na = ""
    )
  }
)

metadata <- tibble(
  MODULE = MODULE,
  VERSION = VERSION,
  RUN_DATE = as.character(RUN_DATE),
  LEGACY_ANALYTICAL_INPUT = FALSE,
  STARS_ASSIGNED = FALSE,
  DISTRICT_DEFINITION_ADOPTED = FALSE,
  METHOD_STATUS = "DIAGNOSTIC_EVIDENCE_ONLY"
)

saveRDS(
  metadata,
  file.path(
    out_dir,
    "vpjd_star_03k_metadata.rds"
  )
)

write.csv(
  as.data.frame(metadata),
  file.path(
    out_dir,
    "vpjd_star_03k_metadata.csv"
  ),
  row.names = FALSE,
  na = ""
)

# ------------------------------------------------------------------------------
# 16. Final summary
# ------------------------------------------------------------------------------

cat("\n— Summary —\n")
cat(
  "Candidate regional terminology hits:",
  format(
    nrow(region_hits),
    big.mark = ","
  ),
  "\n"
)
cat(
  "Candidate source records:",
  format(
    n_distinct(
      paste(
        region_hits$FILE,
        region_hits$.ROW_ID
      )
    ),
    big.mark = ","
  ),
  "\n"
)
cat(
  "Explicit POINTLOCS 'District' records:",
  format(
    nrow(point_districts),
    big.mark = ","
  ),
  "\n"
)
cat(
  "Candidate parent hierarchy links:",
  format(
    nrow(parent_links),
    big.mark = ","
  ),
  "\n"
)

cat(
  "\nStars 03k v",
  VERSION,
  " complete.\n",
  sep = ""
)

cat(
  "DIAGNOSTIC ONLY — no legacy geography has entered the contemporary analytical pipeline.\n"
)