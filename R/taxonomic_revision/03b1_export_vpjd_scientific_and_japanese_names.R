# ==============================================================================
# VPJD TAXONOMIC REVISION
# 03b1_export_vpjd_scientific_and_japanese_names.R
#
# PURPOSE
# -------
# Export the complete canonical VPJD population with Japanese vernacular names
# from the contemporary Japanese checklist sources ingested in 03a and profiled
# in 03b.
#
# Canonical VPJD authority:
#   vpjd_taxrev_02h_validated_vascular_lineage
#
# Contemporary Japanese sources:
#   GreenList 2.02rc
#   FernGreenList 2.0
#
# OUTPUT PRINCIPLE
# ----------------
# The output remains a VPJD-centred dataset:
#
#   one row per canonical VPJD record
#   11,439 rows expected
#
# Japanese names are supplementary evidence only.
#
# Matching is attempted using:
#
#   1. exact scientific name
#   2. normalised scientific name
#   3. canonicalised name without authorship
#
# Multiple Japanese names are retained and explicitly flagged.
# Unmatched VPJD records remain in the output.
#
# AUDIT STATUS
# ------------
# READ-ONLY database operation.
#
# NO canonical taxonomy is modified.
# NO Star allocations are modified.
# NO taxa are added.
# NO taxa are removed.
#
# VERSION
# -------
# 0.2.0
#
# ==============================================================================


# ==============================================================================
# 01. CONFIGURATION
# ==============================================================================

VERSION <- "0.2.0"

PROJECT_ROOT <-
  "I:/R/OJPCP/VPJD-OJPCP"

DB_PATH <-
  file.path(
    PROJECT_ROOT,
    "data",
    "interim",
    "occurrences",
    "vpjd_occurrences.duckdb"
  )

OUTPUT_DIR <-
  file.path(
    PROJECT_ROOT,
    "outputs",
    "tables",
    "taxonomic_revision",
    "03b1_vpjd_japanese_names"
  )

OUTPUT_MAIN <-
  file.path(
    OUTPUT_DIR,
    "vpjd_scientific_names_with_japanese_names.csv"
  )

OUTPUT_UNMATCHED <-
  file.path(
    OUTPUT_DIR,
    "vpjd_scientific_names_without_japanese_name_match.csv"
  )

OUTPUT_MULTIPLE <-
  file.path(
    OUTPUT_DIR,
    "vpjd_multiple_japanese_names_review.csv"
  )

OUTPUT_COVERAGE <-
  file.path(
    OUTPUT_DIR,
    "vpjd_japanese_name_coverage_by_major_group.csv"
  )

OUTPUT_MATCH_METHOD <-
  file.path(
    OUTPUT_DIR,
    "vpjd_japanese_name_match_method_summary.csv"
  )

EXPECTED_CANONICAL_POPULATION <- 11439L


dir.create(
  OUTPUT_DIR,
  recursive = TRUE,
  showWarnings = FALSE
)


# ==============================================================================
# 02. DATABASE TABLES
# ==============================================================================

CANONICAL_TABLE <-
  "vpjd_taxrev_02h_validated_vascular_lineage"

GREEN_CORE_TABLE <-
  "vpjd_taxrev_03a_greenlist_core"

FERN_CORE_TABLE <-
  "vpjd_taxrev_03a_fern_greenlist_core"

GREEN_VERNACULAR_TABLE <-
  "vpjd_taxrev_03a_greenlist_vernacularname_1"

FERN_VERNACULAR_TABLE <-
  "vpjd_taxrev_03a_fern_greenlist_vernacularname_2"


# ==============================================================================
# 03. PACKAGES
# ==============================================================================

required_packages <-
  c(
    "DBI",
    "duckdb",
    "dplyr",
    "tibble",
    "stringr",
    "readr"
  )

missing_packages <-
  required_packages[
    !vapply(
      required_packages,
      requireNamespace,
      logical(1),
      quietly = TRUE
    )
  ]

if (
  length(missing_packages) > 0L
) {
  
  stop(
    paste0(
      "Required package(s) missing:\n",
      paste(
        missing_packages,
        collapse = "\n"
      )
    )
  )
  
}


suppressPackageStartupMessages({
  
  library(DBI)
  library(duckdb)
  library(dplyr)
  library(tibble)
  library(stringr)
  library(readr)
  
})


# ==============================================================================
# 04. HELPER FUNCTIONS
# ==============================================================================

normalise_text <-
  function(x) {
    
    x <-
      as.character(x)
    
    x[
      is.na(x)
    ] <-
      ""
    
    x <-
      stringr::str_replace_all(
        x,
        "\u00A0",
        " "
      )
    
    x <-
      stringr::str_squish(x)
    
    x[
      x == ""
    ] <-
      NA_character_
    
    x
    
  }


normalise_name <-
  function(x) {
    
    x <-
      normalise_text(x)
    
    x <-
      stringr::str_to_lower(x)
    
    x <-
      stringr::str_replace_all(
        x,
        "[×✕]",
        "x"
      )
    
    x <-
      stringr::str_replace_all(
        x,
        "[[:punct:]]+",
        " "
      )
    
    x <-
      stringr::str_squish(x)
    
    x[
      x == ""
    ] <-
      NA_character_
    
    x
    
  }


first_existing_field <-
  function(
    data,
    candidates
  ) {
    
    fields <-
      names(data)
    
    field_upper <-
      toupper(fields)
    
    candidate_upper <-
      toupper(candidates)
    
    hit <-
      match(
        candidate_upper,
        field_upper
      )
    
    hit <-
      hit[
        !is.na(hit)
      ]
    
    if (
      length(hit) == 0L
    ) {
      
      return(
        NA_character_
      )
      
    }
    
    fields[
      hit[[1]]
    ]
    
  }


derive_genus <-
  function(x) {
    
    x <-
      normalise_text(x)
    
    result <-
      stringr::str_extract(
        x,
        "^(?:×\\s*)?[A-Z][A-Za-zÀ-ÖØ-öø-ÿ-]+"
      )
    
    result <-
      stringr::str_replace(
        result,
        "^×\\s*",
        ""
      )
    
    normalise_text(
      result
    )
    
  }


derive_species_epithet <-
  function(x) {
    
    x <-
      normalise_text(x)
    
    result <-
      stringr::str_match(
        x,
        paste0(
          "^(?:×\\s*)?",
          "[A-Z][A-Za-zÀ-ÖØ-öø-ÿ-]+",
          "\\s+",
          "([a-z][A-Za-zÀ-ÖØ-öø-ÿ-]+)"
        )
      )
    
    normalise_text(
      result[, 2]
    )
    
  }


derive_infraspecific_epithet <-
  function(x) {
    
    x <-
      normalise_text(x)
    
    result <-
      stringr::str_match(
        x,
        paste0(
          "\\s",
          "(?:subsp\\.|ssp\\.|var\\.|f\\.|forma)",
          "\\s+",
          "([a-z][A-Za-zÀ-ÖØ-öø-ÿ-]+)"
        )
      )
    
    normalise_text(
      result[, 2]
    )
    
  }


canonical_name <-
  function(x) {
    
    x <-
      normalise_text(x)
    
    genus <-
      derive_genus(x)
    
    species <-
      derive_species_epithet(x)
    
    infra <-
      derive_infraspecific_epithet(x)
    
    infra_rank <-
      dplyr::case_when(
        
        stringr::str_detect(
          x,
          "\\s(?:subsp\\.|ssp\\.)\\s"
        ) ~
          "subsp.",
        
        stringr::str_detect(
          x,
          "\\svar\\.\\s"
        ) ~
          "var.",
        
        stringr::str_detect(
          x,
          "\\s(?:f\\.|forma)\\s"
        ) ~
          "f.",
        
        TRUE ~
          NA_character_
        
      )
    
    result <-
      dplyr::case_when(
        
        !is.na(genus) &
          !is.na(species) &
          !is.na(infra) &
          !is.na(infra_rank) ~
          paste(
            genus,
            species,
            infra_rank,
            infra
          ),
        
        !is.na(genus) &
          !is.na(species) ~
          paste(
            genus,
            species
          ),
        
        !is.na(genus) ~
          genus,
        
        TRUE ~
          NA_character_
        
      )
    
    normalise_text(
      result
    )
    
  }


collapse_unique <-
  function(x) {
    
    x <-
      normalise_text(x)
    
    x <-
      x[
        !is.na(x)
      ]
    
    x <-
      sort(
        unique(x)
      )
    
    if (
      length(x) == 0L
    ) {
      
      return(
        NA_character_
      )
      
    }
    
    paste(
      x,
      collapse = " | "
    )
    
  }


# ==============================================================================
# 05. DATABASE CHECK
# ==============================================================================

if (
  !file.exists(DB_PATH)
) {
  
  stop(
    paste0(
      "VPJD DuckDB database not found:\n",
      DB_PATH
    )
  )
  
}


# ==============================================================================
# 06. CONNECT READ-ONLY
# ==============================================================================

con <-
  DBI::dbConnect(
    duckdb::duckdb(),
    dbdir = DB_PATH,
    read_only = TRUE
  )


disconnect_safely <-
  function() {
    
    try(
      DBI::dbDisconnect(
        con,
        shutdown = TRUE
      ),
      silent = TRUE
    )
    
  }


# ==============================================================================
# 07. TABLE CHECK
# ==============================================================================

database_tables <-
  DBI::dbListTables(
    con
  )

required_tables <-
  c(
    CANONICAL_TABLE,
    GREEN_CORE_TABLE,
    FERN_CORE_TABLE
  )

missing_tables <-
  setdiff(
    required_tables,
    database_tables
  )

if (
  length(missing_tables) > 0L
) {
  
  disconnect_safely()
  
  stop(
    paste0(
      "Required table(s) missing:\n",
      paste(
        missing_tables,
        collapse = "\n"
      )
    )
  )
  
}


# ==============================================================================
# 08. READ SOURCE TABLES
# ==============================================================================

vpjd <-
  DBI::dbReadTable(
    con,
    CANONICAL_TABLE
  ) |>
  tibble::as_tibble()


green <-
  DBI::dbReadTable(
    con,
    GREEN_CORE_TABLE
  ) |>
  tibble::as_tibble()


fern <-
  DBI::dbReadTable(
    con,
    FERN_CORE_TABLE
  ) |>
  tibble::as_tibble()


green_vernacular <-
  if (
    GREEN_VERNACULAR_TABLE %in%
    database_tables
  ) {
    
    DBI::dbReadTable(
      con,
      GREEN_VERNACULAR_TABLE
    ) |>
      tibble::as_tibble()
    
  } else {
    
    tibble()
    
  }


fern_vernacular <-
  if (
    FERN_VERNACULAR_TABLE %in%
    database_tables
  ) {
    
    DBI::dbReadTable(
      con,
      FERN_VERNACULAR_TABLE
    ) |>
      tibble::as_tibble()
    
  } else {
    
    tibble()
    
  }


# ==============================================================================
# 09. VALIDATE CANONICAL POPULATION
# ==============================================================================

if (
  nrow(vpjd) !=
  EXPECTED_CANONICAL_POPULATION
) {
  
  observed_population <-
    nrow(vpjd)
  
  disconnect_safely()
  
  stop(
    paste0(
      "Unexpected VPJD canonical population.\n\n",
      "Expected: ",
      format(
        EXPECTED_CANONICAL_POPULATION,
        big.mark = ","
      ),
      "\nObserved: ",
      format(
        observed_population,
        big.mark = ","
      )
    )
  )
  
}


# ==============================================================================
# 10. IDENTIFY VPJD FIELDS
# ==============================================================================

vpjd_id_field <-
  first_existing_field(
    vpjd,
    c(
      "FINAL_WCVP_ID",
      "WCVP_ID",
      "TAXONID"
    )
  )


vpjd_name_field <-
  first_existing_field(
    vpjd,
    c(
      "FINAL_WCVP_RECOGNISED_NAME",
      "RECOGNISED_NAME",
      "FINAL_WCVP_NAME",
      "SCIENTIFIC_NAME",
      "SCIENTIFICNAME"
    )
  )


vpjd_family_field <-
  first_existing_field(
    vpjd,
    c(
      "FAMILY"
    )
  )


vpjd_genus_field <-
  first_existing_field(
    vpjd,
    c(
      "GENUS"
    )
  )


vpjd_species_field <-
  first_existing_field(
    vpjd,
    c(
      "SPECIES"
    )
  )


vpjd_rank_field <-
  first_existing_field(
    vpjd,
    c(
      "FINAL_WCVP_RANK",
      "TAXONRANK",
      "TAXON_RANK"
    )
  )


vpjd_group_field <-
  first_existing_field(
    vpjd,
    c(
      "MAJOR_GROUP"
    )
  )


required_vpjd_fields <-
  c(
    vpjd_id_field,
    vpjd_name_field,
    vpjd_family_field,
    vpjd_genus_field,
    vpjd_rank_field,
    vpjd_group_field
  )


if (
  any(
    is.na(
      required_vpjd_fields
    )
  )
) {
  
  missing_labels <-
    c(
      "WCVP ID",
      "recognised scientific name",
      "family",
      "genus",
      "rank",
      "major group"
    )[
      is.na(
        required_vpjd_fields
      )
    ]
  
  disconnect_safely()
  
  stop(
    paste0(
      "Required VPJD field(s) could not be identified:\n",
      paste(
        missing_labels,
        collapse = "\n"
      ),
      "\n\nAvailable fields:\n",
      paste(
        names(vpjd),
        collapse = "\n"
      )
    )
  )
  
}


cat("\n")
cat("------------------------------------------------------------\n")
cat("VPJD CANONICAL FIELD MAPPING\n")
cat("------------------------------------------------------------\n\n")

cat(
  "WCVP ID: ",
  vpjd_id_field,
  "\n",
  sep = ""
)

cat(
  "Scientific name: ",
  vpjd_name_field,
  "\n",
  sep = ""
)

cat(
  "Family: ",
  vpjd_family_field,
  "\n",
  sep = ""
)

cat(
  "Genus: ",
  vpjd_genus_field,
  "\n",
  sep = ""
)

cat(
  "Species: ",
  ifelse(
    is.na(vpjd_species_field),
    "<not identified>",
    vpjd_species_field
  ),
  "\n",
  sep = ""
)

cat(
  "Taxon rank: ",
  vpjd_rank_field,
  "\n",
  sep = ""
)

cat(
  "Major group: ",
  vpjd_group_field,
  "\n",
  sep = ""
)


# ==============================================================================
# 11. BUILD VPJD NAME INVENTORY
# ==============================================================================

vpjd_names <-
  tibble(
    
    VPJD_ROW =
      seq_len(
        nrow(vpjd)
      ),
    
    WCVP_ID =
      normalise_text(
        vpjd[[vpjd_id_field]]
      ),
    
    SCIENTIFIC_NAME =
      normalise_text(
        vpjd[[vpjd_name_field]]
      ),
    
    FAMILY =
      normalise_text(
        vpjd[[vpjd_family_field]]
      ),
    
    GENUS =
      normalise_text(
        vpjd[[vpjd_genus_field]]
      ),
    
    SPECIES =
      if (
        !is.na(vpjd_species_field)
      ) {
        
        normalise_text(
          vpjd[[vpjd_species_field]]
        )
        
      } else {
        
        rep(
          NA_character_,
          nrow(vpjd)
        )
        
      },
    
    TAXON_RANK =
      normalise_text(
        vpjd[[vpjd_rank_field]]
      ),
    
    MAJOR_GROUP =
      normalise_text(
        vpjd[[vpjd_group_field]]
      )
    
  ) |>
  mutate(
    
    EXACT_NAME_KEY =
      SCIENTIFIC_NAME,
    
    NORMALISED_NAME_KEY =
      normalise_name(
        SCIENTIFIC_NAME
      ),
    
    CANONICAL_NAME =
      canonical_name(
        SCIENTIFIC_NAME
      ),
    
    CANONICAL_NAME_KEY =
      normalise_name(
        CANONICAL_NAME
      )
    
  )


# ==============================================================================
# 12. VALIDATE VPJD NAME INVENTORY
# ==============================================================================

if (
  nrow(vpjd_names) !=
  EXPECTED_CANONICAL_POPULATION
) {
  
  disconnect_safely()
  
  stop(
    "VPJD name inventory population validation failed."
  )
  
}


if (
  dplyr::n_distinct(
    vpjd_names$WCVP_ID
  ) !=
  EXPECTED_CANONICAL_POPULATION
) {
  
  disconnect_safely()
  
  stop(
    "VPJD WCVP IDs are not unique across the canonical population."
  )
  
}


cat("\n")
cat("------------------------------------------------------------\n")
cat("VPJD NAME INVENTORY\n")
cat("------------------------------------------------------------\n\n")

cat(
  "Records: ",
  format(
    nrow(vpjd_names),
    big.mark = ","
  ),
  "\n",
  sep = ""
)

cat(
  "Distinct WCVP IDs: ",
  format(
    dplyr::n_distinct(
      vpjd_names$WCVP_ID
    ),
    big.mark = ","
  ),
  "\n",
  sep = ""
)

cat(
  "Scientific names populated: ",
  format(
    sum(
      !is.na(
        vpjd_names$SCIENTIFIC_NAME
      )
    ),
    big.mark = ","
  ),
  "\n",
  sep = ""
)

cat(
  "Families represented: ",
  format(
    dplyr::n_distinct(
      vpjd_names$FAMILY,
      na.rm = TRUE
    ),
    big.mark = ","
  ),
  "\n",
  sep = ""
)

cat(
  "Genera represented: ",
  format(
    dplyr::n_distinct(
      vpjd_names$GENUS,
      na.rm = TRUE
    ),
    big.mark = ","
  ),
  "\n",
  sep = ""
)

cat(
  "Species values represented: ",
  format(
    dplyr::n_distinct(
      vpjd_names$SPECIES,
      na.rm = TRUE
    ),
    big.mark = ","
  ),
  "\n",
  sep = ""
)


# ==============================================================================
# 13. IDENTIFY GREENLIST FIELDS
# ==============================================================================

green_id_field <-
  first_existing_field(
    green,
    c(
      "TAXONID",
      "DWCA_ID"
    )
  )


green_name_field <-
  first_existing_field(
    green,
    c(
      "SCIENTIFICNAME"
    )
  )


green_japanese_field <-
  first_existing_field(
    green,
    c(
      "VERNACULARNAME"
    )
  )


if (
  is.na(green_name_field)
) {
  
  disconnect_safely()
  
  stop(
    "GreenList SCIENTIFICNAME field could not be identified."
  )
  
}


# ==============================================================================
# 14. IDENTIFY FERNGREENLIST FIELDS
# ==============================================================================

fern_id_field <-
  first_existing_field(
    fern,
    c(
      "TAXONID",
      "DWCA_ID"
    )
  )


fern_name_field <-
  first_existing_field(
    fern,
    c(
      "SCIENTIFICNAME"
    )
  )


fern_japanese_field <-
  first_existing_field(
    fern,
    c(
      "VERNACULARNAME"
    )
  )


if (
  is.na(fern_name_field)
) {
  
  disconnect_safely()
  
  stop(
    "FernGreenList SCIENTIFICNAME field could not be identified."
  )
  
}


# ==============================================================================
# 15. GREENLIST CORE JAPANESE-NAME INVENTORY
# ==============================================================================

green_names_core <-
  tibble(
    
    SOURCE =
      rep(
        "GreenList 2.02rc",
        nrow(green)
      ),
    
    SOURCE_TAXON_ID =
      if (
        !is.na(green_id_field)
      ) {
        
        normalise_text(
          green[[green_id_field]]
        )
        
      } else {
        
        as.character(
          seq_len(
            nrow(green)
          )
        )
        
      },
    
    SOURCE_SCIENTIFIC_NAME =
      normalise_text(
        green[[green_name_field]]
      ),
    
    JAPANESE_NAME =
      if (
        !is.na(green_japanese_field)
      ) {
        
        normalise_text(
          green[[green_japanese_field]]
        )
        
      } else {
        
        rep(
          NA_character_,
          nrow(green)
        )
        
      }
    
  )


# ==============================================================================
# 16. FERNGREENLIST CORE JAPANESE-NAME INVENTORY
# ==============================================================================

fern_names_core <-
  tibble(
    
    SOURCE =
      rep(
        "FernGreenList 2.0",
        nrow(fern)
      ),
    
    SOURCE_TAXON_ID =
      if (
        !is.na(fern_id_field)
      ) {
        
        normalise_text(
          fern[[fern_id_field]]
        )
        
      } else {
        
        as.character(
          seq_len(
            nrow(fern)
          )
        )
        
      },
    
    SOURCE_SCIENTIFIC_NAME =
      normalise_text(
        fern[[fern_name_field]]
      ),
    
    JAPANESE_NAME =
      if (
        !is.na(fern_japanese_field)
      ) {
        
        normalise_text(
          fern[[fern_japanese_field]]
        )
        
      } else {
        
        rep(
          NA_character_,
          nrow(fern)
        )
        
      }
    
  )


# ==============================================================================
# 17. PROFILE VERNACULAR EXTENSIONS
# ==============================================================================

green_vernacular_id_field <-
  if (ncol(green_vernacular) > 0L) {
    
    first_existing_field(
      green_vernacular,
      c(
        "COREID",
        "TAXONID",
        "DWCA_ID"
      )
    )
    
  } else {
    
    NA_character_
    
  }


green_vernacular_name_field <-
  if (ncol(green_vernacular) > 0L) {
    
    first_existing_field(
      green_vernacular,
      c(
        "VERNACULARNAME"
      )
    )
    
  } else {
    
    NA_character_
    
  }


fern_vernacular_id_field <-
  if (ncol(fern_vernacular) > 0L) {
    
    first_existing_field(
      fern_vernacular,
      c(
        "COREID",
        "TAXONID",
        "DWCA_ID"
      )
    )
    
  } else {
    
    NA_character_
    
  }


fern_vernacular_name_field <-
  if (ncol(fern_vernacular) > 0L) {
    
    first_existing_field(
      fern_vernacular,
      c(
        "VERNACULARNAME"
      )
    )
    
  } else {
    
    NA_character_
    
  }


# ------------------------------------------------------------------------------
# REPORT DETECTED EXTENSION FIELDS
# ------------------------------------------------------------------------------

cat("\n")
cat("------------------------------------------------------------\n")
cat("VERNACULAR EXTENSION FIELD MAPPING\n")
cat("------------------------------------------------------------\n\n")

cat(
  "GreenList vernacular ID field: ",
  ifelse(
    is.na(green_vernacular_id_field),
    "<not identified>",
    green_vernacular_id_field
  ),
  "\n",
  sep = ""
)

cat(
  "GreenList vernacular-name field: ",
  ifelse(
    is.na(green_vernacular_name_field),
    "<not identified>",
    green_vernacular_name_field
  ),
  "\n",
  sep = ""
)

cat(
  "FernGreenList vernacular ID field: ",
  ifelse(
    is.na(fern_vernacular_id_field),
    "<not identified>",
    fern_vernacular_id_field
  ),
  "\n",
  sep = ""
)

cat(
  "FernGreenList vernacular-name field: ",
  ifelse(
    is.na(fern_vernacular_name_field),
    "<not identified>",
    fern_vernacular_name_field
  ),
  "\n",
  sep = ""
)
# ==============================================================================
# 18. ADD GREENLIST VERNACULAR EXTENSION
# ==============================================================================

if (
  nrow(green_vernacular) > 0L &&
  !is.na(green_vernacular_id_field) &&
  !is.na(green_vernacular_name_field) &&
  !is.na(green_id_field)
) {
  
  green_extension_names <-
    tibble(
      
      SOURCE_TAXON_ID =
        normalise_text(
          green_vernacular[[green_vernacular_id_field]]
        ),
      
      EXTENSION_JAPANESE_NAME =
        normalise_text(
          green_vernacular[[green_vernacular_name_field]]
        )
      
    ) |>
    filter(
      !is.na(
        SOURCE_TAXON_ID
      ),
      !is.na(
        EXTENSION_JAPANESE_NAME
      )
    ) |>
    group_by(
      SOURCE_TAXON_ID
    ) |>
    summarise(
      
      EXTENSION_JAPANESE_NAME =
        collapse_unique(
          EXTENSION_JAPANESE_NAME
        ),
      
      .groups =
        "drop"
      
    )
  
  
  green_names <-
    green_names_core |>
    left_join(
      green_extension_names,
      by =
        "SOURCE_TAXON_ID"
    ) |>
    mutate(
      
      JAPANESE_NAME =
        case_when(
          
          !is.na(EXTENSION_JAPANESE_NAME) ~
            EXTENSION_JAPANESE_NAME,
          
          TRUE ~
            JAPANESE_NAME
          
        )
      
    ) |>
    select(
      -EXTENSION_JAPANESE_NAME
    )
  
} else {
  
  green_names <-
    green_names_core
  
}


# ==============================================================================
# 19. ADD FERNGREENLIST VERNACULAR EXTENSION
# ==============================================================================

if (
  nrow(fern_vernacular) > 0L &&
  !is.na(fern_vernacular_id_field) &&
  !is.na(fern_vernacular_name_field) &&
  !is.na(fern_id_field)
) {
  
  fern_extension_names <-
    tibble(
      
      SOURCE_TAXON_ID =
        normalise_text(
          fern_vernacular[[fern_vernacular_id_field]]
        ),
      
      EXTENSION_JAPANESE_NAME =
        normalise_text(
          fern_vernacular[[fern_vernacular_name_field]]
        )
      
    ) |>
    filter(
      !is.na(
        SOURCE_TAXON_ID
      ),
      !is.na(
        EXTENSION_JAPANESE_NAME
      )
    ) |>
    group_by(
      SOURCE_TAXON_ID
    ) |>
    summarise(
      
      EXTENSION_JAPANESE_NAME =
        collapse_unique(
          EXTENSION_JAPANESE_NAME
        ),
      
      .groups =
        "drop"
      
    )
  
  
  fern_names <-
    fern_names_core |>
    left_join(
      fern_extension_names,
      by =
        "SOURCE_TAXON_ID"
    ) |>
    mutate(
      
      JAPANESE_NAME =
        case_when(
          
          !is.na(EXTENSION_JAPANESE_NAME) ~
            EXTENSION_JAPANESE_NAME,
          
          TRUE ~
            JAPANESE_NAME
          
        )
      
    ) |>
    select(
      -EXTENSION_JAPANESE_NAME
    )
  
} else {
  
  fern_names <-
    fern_names_core
  
}


# ==============================================================================
# 20. BUILD CONTEMPORARY JAPANESE-NAME AUTHORITY
# ==============================================================================

japanese_names <-
  bind_rows(
    green_names,
    fern_names
  ) |>
  mutate(
    
    EXACT_NAME_KEY =
      SOURCE_SCIENTIFIC_NAME,
    
    NORMALISED_NAME_KEY =
      normalise_name(
        SOURCE_SCIENTIFIC_NAME
      ),
    
    CANONICAL_NAME =
      canonical_name(
        SOURCE_SCIENTIFIC_NAME
      ),
    
    CANONICAL_NAME_KEY =
      normalise_name(
        CANONICAL_NAME
      )
    
  ) |>
  filter(
    !is.na(
      SOURCE_SCIENTIFIC_NAME
    )
  )


# ==============================================================================
# 21. EXACT SCIENTIFIC-NAME MATCH
# ==============================================================================

exact_matches <-
  vpjd_names |>
  select(
    VPJD_ROW,
    EXACT_NAME_KEY
  ) |>
  filter(
    !is.na(
      EXACT_NAME_KEY
    )
  ) |>
  inner_join(
    
    japanese_names |>
      select(
        SOURCE,
        SOURCE_TAXON_ID,
        SOURCE_SCIENTIFIC_NAME,
        JAPANESE_NAME,
        EXACT_NAME_KEY
      ) |>
      filter(
        !is.na(
          EXACT_NAME_KEY
        )
      ),
    
    by =
      "EXACT_NAME_KEY",
    
    relationship =
      "many-to-many"
    
  ) |>
  mutate(
    
    MATCH_METHOD =
      "EXACT_SCIENTIFIC_NAME",
    
    MATCH_PRIORITY =
      1L
    
  )


# ==============================================================================
# 22. NORMALISED SCIENTIFIC-NAME MATCH
# ==============================================================================

normalised_matches <-
  vpjd_names |>
  select(
    VPJD_ROW,
    NORMALISED_NAME_KEY
  ) |>
  filter(
    !is.na(
      NORMALISED_NAME_KEY
    )
  ) |>
  inner_join(
    
    japanese_names |>
      select(
        SOURCE,
        SOURCE_TAXON_ID,
        SOURCE_SCIENTIFIC_NAME,
        JAPANESE_NAME,
        NORMALISED_NAME_KEY
      ) |>
      filter(
        !is.na(
          NORMALISED_NAME_KEY
        )
      ),
    
    by =
      "NORMALISED_NAME_KEY",
    
    relationship =
      "many-to-many"
    
  ) |>
  mutate(
    
    MATCH_METHOD =
      "NORMALISED_SCIENTIFIC_NAME",
    
    MATCH_PRIORITY =
      2L
    
  )


# ==============================================================================
# 23. CANONICAL NAME MATCH
# ==============================================================================

canonical_matches <-
  vpjd_names |>
  select(
    VPJD_ROW,
    CANONICAL_NAME_KEY
  ) |>
  filter(
    !is.na(
      CANONICAL_NAME_KEY
    )
  ) |>
  inner_join(
    
    japanese_names |>
      select(
        SOURCE,
        SOURCE_TAXON_ID,
        SOURCE_SCIENTIFIC_NAME,
        JAPANESE_NAME,
        CANONICAL_NAME_KEY
      ) |>
      filter(
        !is.na(
          CANONICAL_NAME_KEY
        )
      ),
    
    by =
      "CANONICAL_NAME_KEY",
    
    relationship =
      "many-to-many"
    
  ) |>
  mutate(
    
    MATCH_METHOD =
      "CANONICAL_NAME_WITHOUT_AUTHORSHIP",
    
    MATCH_PRIORITY =
      3L
    
  )


# ==============================================================================
# 24. COMBINE MATCHES
# ==============================================================================

all_matches <-
  bind_rows(
    exact_matches,
    normalised_matches,
    canonical_matches
  ) |>
  arrange(
    VPJD_ROW,
    MATCH_PRIORITY,
    SOURCE,
    SOURCE_TAXON_ID
  ) |>
  distinct(
    VPJD_ROW,
    SOURCE,
    SOURCE_TAXON_ID,
    JAPANESE_NAME,
    .keep_all = TRUE
  )


# ==============================================================================
# 25. IDENTIFY BEST MATCHING TIER
# ==============================================================================

best_priority <-
  all_matches |>
  group_by(
    VPJD_ROW
  ) |>
  summarise(
    
    BEST_PRIORITY =
      min(
        MATCH_PRIORITY,
        na.rm = TRUE
      ),
    
    .groups =
      "drop"
    
  )


best_matches <-
  all_matches |>
  inner_join(
    best_priority,
    by =
      "VPJD_ROW"
  ) |>
  filter(
    MATCH_PRIORITY ==
      BEST_PRIORITY
  )


# ==============================================================================
# 26. COLLAPSE JAPANESE-NAME EVIDENCE
# ==============================================================================

japanese_name_summary <-
  best_matches |>
  group_by(
    VPJD_ROW
  ) |>
  summarise(
    
    JAPANESE_NAME =
      collapse_unique(
        JAPANESE_NAME
      ),
    
    JAPANESE_NAME_SOURCE =
      collapse_unique(
        SOURCE
      ),
    
    SOURCE_TAXON_ID =
      collapse_unique(
        SOURCE_TAXON_ID
      ),
    
    SOURCE_SCIENTIFIC_NAME =
      collapse_unique(
        SOURCE_SCIENTIFIC_NAME
      ),
    
    JAPANESE_NAME_MATCH_METHOD =
      collapse_unique(
        MATCH_METHOD
      ),
    
    N_JAPANESE_NAMES =
      dplyr::n_distinct(
        JAPANESE_NAME[
          !is.na(
            JAPANESE_NAME
          )
        ]
      ),
    
    N_SOURCE_MATCHES =
      dplyr::n(),
    
    .groups =
      "drop"
    
  )


# ==============================================================================
# 27. COMPILE FINAL VPJD EXPORT
# ==============================================================================

final_export <-
  vpjd_names |>
  left_join(
    japanese_name_summary,
    by =
      "VPJD_ROW"
  ) |>
  mutate(
    
    JAPANESE_NAME_AVAILABLE =
      !is.na(
        JAPANESE_NAME
      ),
    
    JAPANESE_NAME_STATUS =
      case_when(
        
        is.na(
          JAPANESE_NAME
        ) ~
          "NO_DIRECT_MATCH",
        
        N_JAPANESE_NAMES ==
          1L ~
          "ONE_JAPANESE_NAME",
        
        N_JAPANESE_NAMES >
          1L ~
          "MULTIPLE_JAPANESE_NAMES",
        
        TRUE ~
          "REVIEW"
        
      )
    
  ) |>
  select(
    
    WCVP_ID,
    
    SCIENTIFIC_NAME,
    
    JAPANESE_NAME,
    
    FAMILY,
    
    GENUS,
    
    SPECIES,
    
    TAXON_RANK,
    
    MAJOR_GROUP,
    
    JAPANESE_NAME_STATUS,
    
    JAPANESE_NAME_AVAILABLE,
    
    N_JAPANESE_NAMES,
    
    N_SOURCE_MATCHES,
    
    JAPANESE_NAME_SOURCE,
    
    JAPANESE_NAME_MATCH_METHOD,
    
    SOURCE_SCIENTIFIC_NAME,
    
    SOURCE_TAXON_ID
    
  ) |>
  arrange(
    MAJOR_GROUP,
    FAMILY,
    GENUS,
    SCIENTIFIC_NAME
  )


# ==============================================================================
# 28. VALIDATE FINAL EXPORT
# ==============================================================================

validation <-
  tibble(
    
    CHECK =
      c(
        "Canonical VPJD population retained",
        "One output row per canonical VPJD record",
        "WCVP IDs remain unique",
        "Scientific names all populated",
        "Families all populated",
        "Genera all populated",
        "Major groups all populated",
        "Japanese-name evidence does not remove VPJD records",
        "Database modification performed"
      ),
    
    PASS =
      c(
        
        nrow(final_export) ==
          EXPECTED_CANONICAL_POPULATION,
        
        nrow(final_export) ==
          nrow(vpjd_names),
        
        dplyr::n_distinct(
          final_export$WCVP_ID
        ) ==
          EXPECTED_CANONICAL_POPULATION,
        
        all(
          !is.na(
            final_export$SCIENTIFIC_NAME
          )
        ),
        
        all(
          !is.na(
            final_export$FAMILY
          )
        ),
        
        all(
          !is.na(
            final_export$GENUS
          )
        ),
        
        all(
          !is.na(
            final_export$MAJOR_GROUP
          )
        ),
        
        nrow(final_export) ==
          nrow(vpjd),
        
        TRUE
        
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


# ==============================================================================
# 29. STOP IF STRUCTURAL VALIDATION FAILS
# ==============================================================================

structural_validation <-
  validation[
    validation$CHECK !=
      "Database modification performed",
    ,
    drop = FALSE
  ]


if (
  !all(
    structural_validation$PASS
  )
) {
  
  failed_checks <-
    structural_validation[
      !structural_validation$PASS,
      ,
      drop = FALSE
    ]
  
  disconnect_safely()
  
  cat("\n")
  cat("============================================================\n")
  cat("03b1 STRUCTURAL VALIDATION FAILURE\n")
  cat("============================================================\n\n")
  
  for (
    i in seq_len(
      nrow(failed_checks)
    )
  ) {
    
    cat(
      "FAIL: ",
      failed_checks$CHECK[[i]],
      "\n",
      sep = ""
    )
    
  }
  
  stop(
    "03b1 stopped before CSV export."
  )
  
}


# ==============================================================================
# 30. COVERAGE STATISTICS
# ==============================================================================

n_with_japanese <-
  sum(
    final_export$JAPANESE_NAME_AVAILABLE,
    na.rm = TRUE
  )


n_without_japanese <-
  sum(
    !final_export$JAPANESE_NAME_AVAILABLE,
    na.rm = TRUE
  )


n_multiple <-
  sum(
    final_export$JAPANESE_NAME_STATUS ==
      "MULTIPLE_JAPANESE_NAMES",
    na.rm = TRUE
  )


coverage_percent <-
  round(
    100 *
      n_with_japanese /
      EXPECTED_CANONICAL_POPULATION,
    2
  )


# ==============================================================================
# 31. COVERAGE BY MAJOR GROUP
# ==============================================================================

coverage_by_group <-
  final_export |>
  group_by(
    MAJOR_GROUP
  ) |>
  summarise(
    
    VPJD_RECORDS =
      n(),
    
    WITH_JAPANESE_NAME =
      sum(
        JAPANESE_NAME_AVAILABLE,
        na.rm = TRUE
      ),
    
    WITHOUT_JAPANESE_NAME =
      sum(
        !JAPANESE_NAME_AVAILABLE,
        na.rm = TRUE
      ),
    
    COVERAGE_PERCENT =
      round(
        100 *
          WITH_JAPANESE_NAME /
          VPJD_RECORDS,
        2
      ),
    
    .groups =
      "drop"
    
  ) |>
  arrange(
    desc(
      VPJD_RECORDS
    )
  )


# ==============================================================================
# 32. MATCH-METHOD SUMMARY
# ==============================================================================

match_method_summary <-
  final_export |>
  mutate(
    
    JAPANESE_NAME_MATCH_METHOD =
      if_else(
        is.na(
          JAPANESE_NAME_MATCH_METHOD
        ),
        "NO_MATCH",
        JAPANESE_NAME_MATCH_METHOD
      )
    
  ) |>
  count(
    JAPANESE_NAME_MATCH_METHOD,
    name =
      "N_RECORDS"
  ) |>
  mutate(
    
    PERCENT_VPJD =
      round(
        100 *
          N_RECORDS /
          EXPECTED_CANONICAL_POPULATION,
        2
      )
    
  ) |>
  arrange(
    desc(
      N_RECORDS
    )
  )


# ==============================================================================
# 33. UNMATCHED RECORDS
# ==============================================================================

unmatched_names <-
  final_export |>
  filter(
    !JAPANESE_NAME_AVAILABLE
  )


# ==============================================================================
# 34. MULTIPLE JAPANESE-NAME RECORDS
# ==============================================================================

multiple_names <-
  final_export |>
  filter(
    JAPANESE_NAME_STATUS ==
      "MULTIPLE_JAPANESE_NAMES"
  )


# ==============================================================================
# 35. WRITE MAIN CSV
# ==============================================================================

readr::write_excel_csv(
  final_export,
  OUTPUT_MAIN,
  na = ""
)


# ==============================================================================
# 36. WRITE UNMATCHED CSV
# ==============================================================================

readr::write_excel_csv(
  unmatched_names,
  OUTPUT_UNMATCHED,
  na = ""
)


# ==============================================================================
# 37. WRITE MULTIPLE-NAME REVIEW CSV
# ==============================================================================

readr::write_excel_csv(
  multiple_names,
  OUTPUT_MULTIPLE,
  na = ""
)


# ==============================================================================
# 38. WRITE COVERAGE SUMMARY
# ==============================================================================

readr::write_csv(
  coverage_by_group,
  OUTPUT_COVERAGE,
  na = ""
)


# ==============================================================================
# 39. WRITE MATCH-METHOD SUMMARY
# ==============================================================================

readr::write_csv(
  match_method_summary,
  OUTPUT_MATCH_METHOD,
  na = ""
)


# ==============================================================================
# 40. REPORT VALIDATION
# ==============================================================================

cat("\n")
cat("------------------------------------------------------------\n")
cat("VALIDATION\n")
cat("------------------------------------------------------------\n\n")


for (
  i in seq_len(
    nrow(validation)
  )
) {
  
  cat(
    validation$RESULT[[i]],
    ": ",
    validation$CHECK[[i]],
    "\n",
    sep = ""
  )
  
}


# ==============================================================================
# 41. CLEAN DISCONNECT
# ==============================================================================

DBI::dbDisconnect(
  con,
  shutdown = TRUE
)

rm(con)

gc()


# ==============================================================================
# 42. FINAL REPORT
# ==============================================================================

cat("\n")
cat("============================================================\n")
cat("03b1 VPJD SCIENTIFIC + JAPANESE NAME EXPORT\n")
cat("============================================================\n\n")


cat(
  "Version: ",
  VERSION,
  "\n\n",
  sep = ""
)


cat(
  "Canonical VPJD records: ",
  format(
    EXPECTED_CANONICAL_POPULATION,
    big.mark = ","
  ),
  "\n",
  sep = ""
)


cat(
  "Records with Japanese name: ",
  format(
    n_with_japanese,
    big.mark = ","
  ),
  "\n",
  sep = ""
)


cat(
  "Records without Japanese-name match: ",
  format(
    n_without_japanese,
    big.mark = ","
  ),
  "\n",
  sep = ""
)


cat(
  "Records with multiple Japanese names: ",
  format(
    n_multiple,
    big.mark = ","
  ),
  "\n",
  sep = ""
)


cat(
  "Japanese-name coverage: ",
  coverage_percent,
  "%\n\n",
  sep = ""
)


cat("------------------------------------------------------------\n")
cat("COVERAGE BY MAJOR GROUP\n")
cat("------------------------------------------------------------\n\n")


print(
  coverage_by_group,
  n = Inf,
  width = Inf
)


cat("\n")
cat("------------------------------------------------------------\n")
cat("MATCH METHOD SUMMARY\n")
cat("------------------------------------------------------------\n\n")


print(
  match_method_summary,
  n = Inf,
  width = Inf
)


cat("\n")
cat("------------------------------------------------------------\n")
cat("OUTPUT FILES\n")
cat("------------------------------------------------------------\n\n")


cat(
  "Main VPJD + Japanese names:\n",
  OUTPUT_MAIN,
  "\n\n",
  sep = ""
)


cat(
  "Unmatched VPJD names:\n",
  OUTPUT_UNMATCHED,
  "\n\n",
  sep = ""
)


cat(
  "Multiple Japanese names review:\n",
  OUTPUT_MULTIPLE,
  "\n\n",
  sep = ""
)


cat(
  "Coverage by major group:\n",
  OUTPUT_COVERAGE,
  "\n\n",
  sep = ""
)


cat(
  "Match-method summary:\n",
  OUTPUT_MATCH_METHOD,
  "\n\n",
  sep = ""
)


cat("------------------------------------------------------------\n")
cat("AUDIT STATUS\n")
cat("------------------------------------------------------------\n\n")

cat("EXPORT / DIAGNOSTIC ONLY\n")
cat("Canonical taxonomy modified: FALSE\n")
cat("Star allocations modified: FALSE\n")
cat("Taxa removed: 0\n")
cat("Taxa added: 0\n")
cat("DuckDB modified: FALSE\n")
cat("Database connection closed cleanly: TRUE\n")

cat("\n")
cat("============================================================\n")
cat("03b1 COMPLETE\n")
cat("============================================================\n")


# ==============================================================================
# END
# ==============================================================================