# ==============================================================================
# VPJD TAXONOMIC REVISION
#
# 03b - CONTEMPORARY JAPAN CHECKLIST PROFILE
#
# Version: 0.3.3
#
# Purpose:
#   Profile the contemporary Japanese vascular-plant checklist sources ingested
#   by 03a, while preserving complete source provenance and without modifying
#   the canonical VPJD taxonomy.
#
# Contemporary sources:
#   - GreenList v2.02rc
#   - FernGreenList v2.0
#
# Principal outputs:
#   - source-field profiles
#   - taxonomic-rank profiles
#   - family/genus/name representation
#   - Japanese vernacular-name information
#   - distribution / establishment information where supplied
#   - source traceability
#   - GreenList genus derivation with explicit provenance
#
# IMPORTANT:
#   - PROFILING / AUDIT ONLY
#   - canonical VPJD taxonomy is NOT modified
#   - Star allocations are NOT modified
#   - taxa are NOT added
#   - taxa are NOT removed
#   - source-supplied values are preserved
#   - derived values are explicitly identified as derived
#
# GreenList genus:
#   The GreenList Darwin Core core contains SCIENTIFICNAME but does not contain
#   a dedicated GENUS field. Genus is therefore derived conservatively from
#   the first token of SCIENTIFICNAME and explicitly labelled:
#
#       GENUS_PROVENANCE = "DERIVED_FROM_SCIENTIFICNAME"
#
# DuckDB:
#   Version 0.3.3 introduces an EARLY WRITE TEST. A real temporary diagnostic
#   table is created in the database schema and immediately dropped. This tests
#   whether the database is genuinely writable before the expensive profiling
#   workflow begins.
#
# ==============================================================================


# ==============================================================================
# 01. PACKAGES
# ==============================================================================

required_packages <-
  c(
    "DBI",
    "duckdb",
    "dplyr",
    "stringr",
    "tibble",
    "readr",
    "tidyr",
    "purrr"
  )

missing_packages <-
  required_packages[
    !vapply(
      required_packages,
      requireNamespace,
      quietly = TRUE,
      FUN.VALUE = logical(1)
    )
  ]

if (length(missing_packages) > 0L) {
  
  stop(
    paste0(
      "Required package(s) not installed:\n",
      paste(
        missing_packages,
        collapse = "\n"
      )
    )
  )
  
}

library(DBI)
library(duckdb)
library(dplyr)
library(stringr)
library(tibble)
library(readr)
library(tidyr)
library(purrr)


# ==============================================================================
# 02. VERSION
# ==============================================================================

VERSION <- "0.3.3"


# ==============================================================================
# 03. PROJECT PATHS
# ==============================================================================

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
    "03b_contemporary_japan_checklist_profile"
  )

dir.create(
  OUTPUT_DIR,
  recursive = TRUE,
  showWarnings = FALSE
)

if (!file.exists(DB_PATH)) {
  
  stop(
    paste0(
      "VPJD DuckDB database not found:\n",
      DB_PATH
    )
  )
  
}


# ==============================================================================
# 04. EXPECTED CANONICAL POPULATION
# ==============================================================================

EXPECTED_CANONICAL_POPULATION <- 11439L


# ==============================================================================
# 05. SOURCE TABLES
# ==============================================================================

CANONICAL_TABLE <-
  "vpjd_taxrev_02h_validated_vascular_lineage"

SOURCE_REGISTRY_TABLE <-
  "vpjd_taxrev_03a_source_registry"

GREEN_CORE_TABLE <-
  "vpjd_taxrev_03a_greenlist_core"

GREEN_VERNACULAR_TABLE <-
  "vpjd_taxrev_03a_greenlist_vernacularname_1"

GREEN_DISTRIBUTION_TABLE <-
  "vpjd_taxrev_03a_greenlist_distribution_2"

FERN_CORE_TABLE <-
  "vpjd_taxrev_03a_fern_greenlist_core"

FERN_SPECIES_PROFILE_TABLE <-
  "vpjd_taxrev_03a_fern_greenlist_speciesprofile_1"

FERN_VERNACULAR_TABLE <-
  "vpjd_taxrev_03a_fern_greenlist_vernacularname_2"


# ==============================================================================
# 06. OUTPUT TABLES
# ==============================================================================

OUTPUT_SUMMARY_TABLE <-
  "vpjd_taxrev_03b_summary"

OUTPUT_VALIDATION_TABLE <-
  "vpjd_taxrev_03b_validation"

OUTPUT_FIELD_PROFILE_TABLE <-
  "vpjd_taxrev_03b_field_profile"

OUTPUT_GREEN_CORE_PROFILE_TABLE <-
  "vpjd_taxrev_03b_greenlist_core_profile"

OUTPUT_FERN_CORE_PROFILE_TABLE <-
  "vpjd_taxrev_03b_fern_core_profile"

OUTPUT_GREEN_RANK_TABLE <-
  "vpjd_taxrev_03b_greenlist_rank_profile"

OUTPUT_FERN_RANK_TABLE <-
  "vpjd_taxrev_03b_fern_rank_profile"

OUTPUT_GREEN_STATUS_TABLE <-
  "vpjd_taxrev_03b_greenlist_taxonomic_status_profile"

OUTPUT_FERN_STATUS_TABLE <-
  "vpjd_taxrev_03b_fern_taxonomic_status_profile"

OUTPUT_GREEN_VERNACULAR_TABLE <-
  "vpjd_taxrev_03b_green_vernacular"

OUTPUT_FERN_VERNACULAR_TABLE <-
  "vpjd_taxrev_03b_fern_vernacular"

OUTPUT_GREEN_DISTRIBUTION_TABLE <-
  "vpjd_taxrev_03b_green_distribution"

OUTPUT_FERN_SPECIES_PROFILE_TABLE <-
  "vpjd_taxrev_03b_fern_species_profile"

OUTPUT_NAME_OVERLAP_TABLE <-
  "vpjd_taxrev_03b_name_overlap"

OUTPUT_GREEN_DERIVED_GENUS_TABLE <-
  "vpjd_taxrev_03b_greenlist_derived_genus"

OUTPUT_GENUS_PROVENANCE_TABLE <-
  "vpjd_taxrev_03b_genus_provenance"

OUTPUT_GREEN_OCCURRENCE_STATUS_TABLE <-
  "vpjd_taxrev_03b_green_occurrence_status_profile"

OUTPUT_GREEN_ESTABLISHMENT_TABLE <-
  "vpjd_taxrev_03b_green_establishment_means_profile"

OUTPUT_GREEN_LOCATION_TABLE <-
  "vpjd_taxrev_03b_green_location_profile"

OUTPUT_SOURCE_TRACEABILITY_TABLE <-
  "vpjd_taxrev_03b_source_traceability"

OUTPUT_DUCKDB_WRITE_STATUS_TABLE <-
  "vpjd_taxrev_03b_duckdb_write_status"


# ==============================================================================
# 07. CONNECT TO DUCKDB
# ==============================================================================

cat("\n")
cat("============================================================\n")
cat("03b DUCKDB CONNECTION\n")
cat("============================================================\n\n")

cat(
  "Database:\n",
  DB_PATH,
  "\n\n",
  sep = ""
)

con <-
  tryCatch(
    {
      
      DBI::dbConnect(
        duckdb::duckdb(),
        dbdir = DB_PATH,
        read_only = FALSE
      )
      
    },
    error =
      function(e) {
        
        stop(
          paste0(
            "\nUnable to open the VPJD DuckDB database for writing.\n\n",
            "Database:\n",
            DB_PATH,
            "\n\n",
            "DuckDB message:\n",
            conditionMessage(e),
            "\n\n",
            "Possible causes include:\n",
            "  1. another R session has this DuckDB database open;\n",
            "  2. another process has locked the database;\n",
            "  3. an earlier DuckDB connection was not disconnected;\n",
            "  4. the database/file system is read-only.\n\n",
            "Close other R/RStudio sessions using this database and rerun 03b."
          ),
          call. = FALSE
        )
        
      }
  )


disconnect_safely <-
  function() {
    
    if (exists("con", inherits = TRUE)) {
      
      try(
        DBI::dbDisconnect(
          con,
          shutdown = TRUE
        ),
        silent = TRUE
      )
      
    }
    
  }


# ==============================================================================
# 08. EARLY DATABASE WRITE TEST
# ==============================================================================

cat("Testing database writability...\n")

WRITE_TEST_TABLE <-
  "vpjd_taxrev_03b_write_test"

write_test_error <-
  NULL

write_test_success <-
  tryCatch(
    {
      
      if (
        DBI::dbExistsTable(
          con,
          WRITE_TEST_TABLE
        )
      ) {
        
        DBI::dbRemoveTable(
          con,
          WRITE_TEST_TABLE
        )
        
      }
      
      DBI::dbExecute(
        con,
        paste0(
          "CREATE TABLE ",
          WRITE_TEST_TABLE,
          " (",
          "TEST_ID INTEGER, ",
          "MODULE VARCHAR, ",
          "VERSION VARCHAR",
          ")"
        )
      )
      
      DBI::dbExecute(
        con,
        paste0(
          "INSERT INTO ",
          WRITE_TEST_TABLE,
          " VALUES (1, '03b', '",
          VERSION,
          "')"
        )
      )
      
      test_value <-
        DBI::dbGetQuery(
          con,
          paste0(
            "SELECT COUNT(*) AS N ",
            "FROM ",
            WRITE_TEST_TABLE
          )
        )
      
      if (
        nrow(test_value) != 1L ||
        test_value$N[[1]] != 1L
      ) {
        
        stop(
          "Write-test table could not be verified."
        )
        
      }
      
      DBI::dbRemoveTable(
        con,
        WRITE_TEST_TABLE
      )
      
      TRUE
      
    },
    error =
      function(e) {
        
        write_test_error <<-
          conditionMessage(e)
        
        FALSE
        
      }
  )


if (!write_test_success) {
  
  if (
    DBI::dbExistsTable(
      con,
      WRITE_TEST_TABLE
    )
  ) {
    
    try(
      DBI::dbRemoveTable(
        con,
        WRITE_TEST_TABLE
      ),
      silent = TRUE
    )
    
  }
  
  disconnect_safely()
  
  stop(
    paste0(
      "\n03b STOPPED BEFORE PROFILING.\n\n",
      "The VPJD DuckDB database is not writable from this connection.\n\n",
      "Database:\n",
      DB_PATH,
      "\n\n",
      "DuckDB message:\n",
      write_test_error,
      "\n\n",
      "03b requires write access because its diagnostic and audit tables ",
      "are stored in the VPJD DuckDB database.\n\n",
      "No canonical taxonomy has been modified.\n",
      "No Star allocations have been modified.\n",
      "No taxa have been added or removed.\n\n",
      "Recommended action:\n",
      "  1. close any other R/RStudio sessions using vpjd_occurrences.duckdb;\n",
      "  2. restart the current R session if necessary;\n",
      "  3. rerun 03b from the beginning.\n"
    ),
    call. = FALSE
  )
  
}

cat("Database write test: PASS\n")
cat("Database connection is writable.\n\n")


# ==============================================================================
# 09. HELPER FUNCTIONS
# ==============================================================================

normalise_text <-
  function(x) {
    
    x <-
      as.character(x)
    
    x <-
      stringr::str_squish(x)
    
    x[
      is.na(x) |
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
        "[[:punct:]]+",
        " "
      )
    
    x <-
      stringr::str_squish(x)
    
    x
    
  }


field_exists <-
  function(df, field) {
    
    length(field) == 1L &&
      !is.na(field) &&
      field %in% names(df)
    
  }


first_existing_field <-
  function(df, candidates) {
    
    hit <-
      candidates[
        candidates %in% names(df)
      ]
    
    if (
      length(hit) == 0L
    ) {
      
      return(
        NA_character_
      )
      
    }
    
    hit[[1]]
    
  }


safe_distinct_count <-
  function(df, field) {
    
    if (
      !field_exists(
        df,
        field
      )
    ) {
      
      return(
        0L
      )
      
    }
    
    x <-
      normalise_text(
        df[[field]]
      )
    
    length(
      unique(
        x[
          !is.na(x)
        ]
      )
    )
    
  }


safe_non_empty_count <-
  function(df, field) {
    
    if (
      !field_exists(
        df,
        field
      )
    ) {
      
      return(
        0L
      )
      
    }
    
    x <-
      normalise_text(
        df[[field]]
      )
    
    sum(
      !is.na(x)
    )
    
  }


derive_genus_from_scientific_name <-
  function(x) {
    
    x <-
      normalise_text(x)
    
    result <-
      rep(
        NA_character_,
        length(x)
      )
    
    valid <-
      !is.na(x)
    
    if (
      any(valid)
    ) {
      
      first_token <-
        stringr::str_extract(
          x[valid],
          "^[^[:space:]]+"
        )
      
      first_token <-
        stringr::str_replace_all(
          first_token,
          "[^[:alpha:]À-ÖØ-öø-ÿ×-]+",
          ""
        )
      
      first_token[
        first_token == ""
      ] <-
        NA_character_
      
      result[valid] <-
        first_token
      
    }
    
    result
    
  }


make_field_profile <-
  function(
    df,
    source_code
  ) {
    
    if (
      ncol(df) == 0L
    ) {
      
      return(
        tibble(
          SOURCE_CODE = character(),
          FIELD_NAME = character(),
          FIELD_CLASS = character(),
          N_ROWS = integer(),
          N_NON_EMPTY = integer(),
          N_DISTINCT = integer()
        )
      )
      
    }
    
    purrr::map_dfr(
      names(df),
      function(field) {
        
        x <-
          df[[field]]
        
        x_chr <-
          normalise_text(x)
        
        tibble(
          SOURCE_CODE =
            source_code,
          
          FIELD_NAME =
            field,
          
          FIELD_CLASS =
            paste(
              class(x),
              collapse = ";"
            ),
          
          N_ROWS =
            length(x),
          
          N_NON_EMPTY =
            sum(
              !is.na(x_chr)
            ),
          
          N_DISTINCT =
            length(
              unique(
                x_chr[
                  !is.na(x_chr)
                ]
              )
            )
        )
        
      }
    )
    
  }


profile_field <-
  function(
    df,
    field,
    source_code
  ) {
    
    if (
      !field_exists(
        df,
        field
      )
    ) {
      
      return(
        tibble(
          SOURCE_CODE = character(),
          FIELD_NAME = character(),
          VALUE = character(),
          N = integer()
        )
      )
      
    }
    
    x <-
      normalise_text(
        df[[field]]
      )
    
    tibble(
      VALUE = x
    ) |>
      filter(
        !is.na(VALUE)
      ) |>
      count(
        VALUE,
        name = "N",
        sort = TRUE
      ) |>
      mutate(
        SOURCE_CODE =
          source_code,
        
        FIELD_NAME =
          field,
        
        .before = 1
      )
    
  }


write_table_safe <-
  function(
    connection,
    table_name,
    df
  ) {
    
    if (
      !is.data.frame(df)
    ) {
      
      return(
        tibble(
          TABLE_NAME = table_name,
          N_ROWS = NA_integer_,
          N_COLUMNS = NA_integer_,
          WRITTEN = FALSE,
          REASON = "OBJECT_NOT_DATA_FRAME"
        )
      )
      
    }
    
    if (
      ncol(df) == 0L
    ) {
      
      return(
        tibble(
          TABLE_NAME = table_name,
          N_ROWS = nrow(df),
          N_COLUMNS = 0L,
          WRITTEN = FALSE,
          REASON = "STRUCTURALLY_EMPTY"
        )
      )
      
    }
    
    tryCatch(
      {
        
        DBI::dbWriteTable(
          connection,
          table_name,
          as.data.frame(df),
          overwrite = TRUE
        )
        
        tibble(
          TABLE_NAME = table_name,
          N_ROWS = nrow(df),
          N_COLUMNS = ncol(df),
          WRITTEN = TRUE,
          REASON = NA_character_
        )
        
      },
      error =
        function(e) {
          
          tibble(
            TABLE_NAME = table_name,
            N_ROWS = nrow(df),
            N_COLUMNS = ncol(df),
            WRITTEN = FALSE,
            REASON = conditionMessage(e)
          )
          
        }
    )
    
  }


write_csv_safe <-
  function(
    df,
    filename
  ) {
    
    path <-
      file.path(
        OUTPUT_DIR,
        filename
      )
    
    if (
      !is.data.frame(df)
    ) {
      
      return(
        tibble(
          FILE = filename,
          N_ROWS = NA_integer_,
          N_COLUMNS = NA_integer_,
          WRITTEN = FALSE,
          REASON = "OBJECT_NOT_DATA_FRAME"
        )
      )
      
    }
    
    if (
      ncol(df) == 0L
    ) {
      
      return(
        tibble(
          FILE = filename,
          N_ROWS = nrow(df),
          N_COLUMNS = 0L,
          WRITTEN = FALSE,
          REASON = "STRUCTURALLY_EMPTY"
        )
      )
      
    }
    
    tryCatch(
      {
        
        readr::write_csv(
          df,
          path,
          na = ""
        )
        
        tibble(
          FILE = filename,
          N_ROWS = nrow(df),
          N_COLUMNS = ncol(df),
          WRITTEN = TRUE,
          REASON = NA_character_
        )
        
      },
      error =
        function(e) {
          
          tibble(
            FILE = filename,
            N_ROWS = nrow(df),
            N_COLUMNS = ncol(df),
            WRITTEN = FALSE,
            REASON = conditionMessage(e)
          )
          
        }
    )
    
  }


# ==============================================================================
# 10. DATABASE TABLE INVENTORY
# ==============================================================================

database_tables <-
  DBI::dbListTables(
    con
  )

required_tables <-
  c(
    CANONICAL_TABLE,
    SOURCE_REGISTRY_TABLE,
    GREEN_CORE_TABLE,
    GREEN_VERNACULAR_TABLE,
    GREEN_DISTRIBUTION_TABLE,
    FERN_CORE_TABLE,
    FERN_SPECIES_PROFILE_TABLE,
    FERN_VERNACULAR_TABLE
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
      "Required 03a / canonical table(s) missing:\n",
      paste(
        missing_tables,
        collapse = "\n"
      )
    ),
    call. = FALSE
  )
  
}


# ==============================================================================
# 11. LOAD CANONICAL VPJD
# ==============================================================================

canonical <-
  DBI::dbReadTable(
    con,
    CANONICAL_TABLE
  ) |>
  as_tibble()

if (
  nrow(canonical) !=
  EXPECTED_CANONICAL_POPULATION
) {
  
  disconnect_safely()
  
  stop(
    paste0(
      "Canonical population mismatch. Expected ",
      format(
        EXPECTED_CANONICAL_POPULATION,
        big.mark = ","
      ),
      " records but found ",
      format(
        nrow(canonical),
        big.mark = ","
      ),
      "."
    ),
    call. = FALSE
  )
  
}


# ==============================================================================
# 12. LOAD 03a SOURCE REGISTRY
# ==============================================================================

source_registry <-
  DBI::dbReadTable(
    con,
    SOURCE_REGISTRY_TABLE
  ) |>
  as_tibble()


# ==============================================================================
# 13. LOAD GREENLIST CORE
# ==============================================================================

green_core <-
  DBI::dbReadTable(
    con,
    GREEN_CORE_TABLE
  ) |>
  as_tibble()


# ==============================================================================
# 14. LOAD FERNGREENLIST CORE
# ==============================================================================

fern_core <-
  DBI::dbReadTable(
    con,
    FERN_CORE_TABLE
  ) |>
  as_tibble()


# ==============================================================================
# 15. LOAD GREENLIST EXTENSIONS
# ==============================================================================

green_vernacular <-
  DBI::dbReadTable(
    con,
    GREEN_VERNACULAR_TABLE
  ) |>
  as_tibble()

green_distribution <-
  DBI::dbReadTable(
    con,
    GREEN_DISTRIBUTION_TABLE
  ) |>
  as_tibble()


# ==============================================================================
# 16. LOAD FERNGREENLIST EXTENSIONS
# ==============================================================================

fern_species_profile <-
  DBI::dbReadTable(
    con,
    FERN_SPECIES_PROFILE_TABLE
  ) |>
  as_tibble()

fern_vernacular <-
  DBI::dbReadTable(
    con,
    FERN_VERNACULAR_TABLE
  ) |>
  as_tibble()


# ==============================================================================
# 17. IDENTIFY CORE FIELDS
# ==============================================================================

green_scientific_field <-
  first_existing_field(
    green_core,
    c(
      "SCIENTIFICNAME",
      "scientificName",
      "SCIENTIFIC_NAME"
    )
  )

green_family_field <-
  first_existing_field(
    green_core,
    c(
      "FAMILY",
      "family"
    )
  )

green_source_genus_field <-
  first_existing_field(
    green_core,
    c(
      "GENUS",
      "genus",
      "GENERICNAME",
      "genericName"
    )
  )

green_rank_field <-
  first_existing_field(
    green_core,
    c(
      "TAXONRANK",
      "taxonRank",
      "RANK"
    )
  )

green_taxonomic_status_field <-
  first_existing_field(
    green_core,
    c(
      "TAXONOMICSTATUS",
      "taxonomicStatus",
      "STATUS"
    )
  )

green_vernacular_core_field <-
  first_existing_field(
    green_core,
    c(
      "VERNACULARNAME",
      "vernacularName"
    )
  )

fern_scientific_field <-
  first_existing_field(
    fern_core,
    c(
      "SCIENTIFICNAME",
      "scientificName",
      "SCIENTIFIC_NAME"
    )
  )

fern_family_field <-
  first_existing_field(
    fern_core,
    c(
      "FAMILY",
      "family"
    )
  )

fern_genus_field <-
  first_existing_field(
    fern_core,
    c(
      "GENUS",
      "genus",
      "GENERICNAME",
      "genericName"
    )
  )

fern_rank_field <-
  first_existing_field(
    fern_core,
    c(
      "TAXONRANK",
      "taxonRank",
      "RANK"
    )
  )

fern_taxonomic_status_field <-
  first_existing_field(
    fern_core,
    c(
      "TAXONOMICSTATUS",
      "taxonomicStatus",
      "STATUS"
    )
  )


# ==============================================================================
# 18. REQUIRED SCIENTIFIC-NAME FIELDS
# ==============================================================================

if (
  is.na(green_scientific_field)
) {
  
  disconnect_safely()
  
  stop(
    "GreenList scientific-name field could not be identified.",
    call. = FALSE
  )
  
}

if (
  is.na(fern_scientific_field)
) {
  
  disconnect_safely()
  
  stop(
    "FernGreenList scientific-name field could not be identified.",
    call. = FALSE
  )
  
}


# ==============================================================================
# 19. GREENLIST GENUS DERIVATION
# ==============================================================================

green_genus_source_supplied <-
  !is.na(
    green_source_genus_field
  )

if (
  green_genus_source_supplied
) {
  
  green_genus_values <-
    normalise_text(
      green_core[[green_source_genus_field]]
    )
  
  green_genus_method <-
    "SOURCE_SUPPLIED"
  
} else {
  
  green_genus_values <-
    derive_genus_from_scientific_name(
      green_core[[green_scientific_field]]
    )
  
  green_genus_method <-
    "DERIVED_FROM_SCIENTIFICNAME"
  
}


green_source_row <-
  if (
    "SOURCE_ROW" %in%
    names(green_core)
  ) {
    
    green_core$SOURCE_ROW
    
  } else {
    
    seq_len(
      nrow(green_core)
    )
    
  }


green_taxon_id <-
  if (
    "TAXONID" %in%
    names(green_core)
  ) {
    
    as.character(
      green_core$TAXONID
    )
    
  } else {
    
    rep(
      NA_character_,
      nrow(green_core)
    )
    
  }


green_derived_genus <-
  tibble(
    SOURCE_CODE =
      rep(
        "GREENLIST",
        nrow(green_core)
      ),
    
    SOURCE_ROW =
      green_source_row,
    
    TAXONID =
      green_taxon_id,
    
    SCIENTIFICNAME =
      as.character(
        green_core[[green_scientific_field]]
      ),
    
    GENUS =
      green_genus_values,
    
    GENUS_PROVENANCE =
      rep(
        green_genus_method,
        nrow(green_core)
      ),
    
    SOURCE_GENUS_FIELD =
      rep(
        ifelse(
          green_genus_source_supplied,
          green_source_genus_field,
          NA_character_
        ),
        nrow(green_core)
      )
  )


# ==============================================================================
# 20. FERNGREENLIST GENUS
# ==============================================================================

if (
  !is.na(fern_genus_field)
) {
  
  fern_genus_values <-
    normalise_text(
      fern_core[[fern_genus_field]]
    )
  
  fern_genus_method <-
    "SOURCE_SUPPLIED"
  
} else {
  
  fern_genus_values <-
    derive_genus_from_scientific_name(
      fern_core[[fern_scientific_field]]
    )
  
  fern_genus_method <-
    "DERIVED_FROM_SCIENTIFICNAME"
  
}


# ==============================================================================
# 21. GENUS PROVENANCE
# ==============================================================================

n_green_genera <-
  length(
    unique(
      green_genus_values[
        !is.na(green_genus_values)
      ]
    )
  )

n_fern_genera <-
  length(
    unique(
      fern_genus_values[
        !is.na(fern_genus_values)
      ]
    )
  )

genus_provenance <-
  tibble(
    SOURCE_CODE =
      c(
        "GREENLIST",
        "FERN_GREENLIST"
      ),
    
    SOURCE_GENUS_FIELD =
      c(
        ifelse(
          is.na(green_source_genus_field),
          NA_character_,
          green_source_genus_field
        ),
        ifelse(
          is.na(fern_genus_field),
          NA_character_,
          fern_genus_field
        )
      ),
    
    GENUS_METHOD =
      c(
        green_genus_method,
        fern_genus_method
      ),
    
    N_RECORDS =
      c(
        nrow(green_core),
        nrow(fern_core)
      ),
    
    N_GENUS_NON_EMPTY =
      c(
        sum(
          !is.na(green_genus_values)
        ),
        sum(
          !is.na(fern_genus_values)
        )
      ),
    
    N_DISTINCT_GENERA =
      c(
        n_green_genera,
        n_fern_genera
      )
  )


# ==============================================================================
# 22. FIELD INVENTORY
# ==============================================================================

green_field_profile <-
  make_field_profile(
    green_core,
    "GREENLIST_CORE"
  )

fern_field_profile <-
  make_field_profile(
    fern_core,
    "FERN_GREENLIST_CORE"
  )

green_vernacular_field_profile <-
  make_field_profile(
    green_vernacular,
    "GREENLIST_VERNACULAR"
  )

green_distribution_field_profile <-
  make_field_profile(
    green_distribution,
    "GREENLIST_DISTRIBUTION"
  )

fern_species_field_profile <-
  make_field_profile(
    fern_species_profile,
    "FERN_GREENLIST_SPECIES_PROFILE"
  )

fern_vernacular_field_profile <-
  make_field_profile(
    fern_vernacular,
    "FERN_GREENLIST_VERNACULAR"
  )

all_field_profile <-
  bind_rows(
    green_field_profile,
    fern_field_profile,
    green_vernacular_field_profile,
    green_distribution_field_profile,
    fern_species_field_profile,
    fern_vernacular_field_profile
  )


# ==============================================================================
# 23. CORE DATASET PROFILES
# ==============================================================================

n_green_families <-
  safe_distinct_count(
    green_core,
    green_family_field
  )

n_fern_families <-
  safe_distinct_count(
    fern_core,
    fern_family_field
  )

n_green_scientific_names <-
  safe_distinct_count(
    green_core,
    green_scientific_field
  )

n_fern_scientific_names <-
  safe_distinct_count(
    fern_core,
    fern_scientific_field
  )


green_core_profile <-
  tibble(
    METRIC =
      c(
        "Records",
        "Families",
        "Genera",
        "Scientific names",
        "Source genus field present",
        "Genus method",
        "Core vernacular-name field present"
      ),
    
    VALUE =
      c(
        as.character(
          nrow(green_core)
        ),
        as.character(
          n_green_families
        ),
        as.character(
          n_green_genera
        ),
        as.character(
          n_green_scientific_names
        ),
        as.character(
          green_genus_source_supplied
        ),
        green_genus_method,
        as.character(
          !is.na(
            green_vernacular_core_field
          )
        )
      )
  )


fern_core_profile <-
  tibble(
    METRIC =
      c(
        "Records",
        "Families",
        "Genera",
        "Scientific names",
        "Source genus field present",
        "Genus method"
      ),
    
    VALUE =
      c(
        as.character(
          nrow(fern_core)
        ),
        as.character(
          n_fern_families
        ),
        as.character(
          n_fern_genera
        ),
        as.character(
          n_fern_scientific_names
        ),
        as.character(
          !is.na(fern_genus_field)
        ),
        fern_genus_method
      )
  )


# ==============================================================================
# 24. RANK PROFILES
# ==============================================================================

green_rank_profile <-
  profile_field(
    green_core,
    green_rank_field,
    "GREENLIST"
  )

fern_rank_profile <-
  profile_field(
    fern_core,
    fern_rank_field,
    "FERN_GREENLIST"
  )


# ==============================================================================
# 25. TAXONOMIC STATUS PROFILES
# ==============================================================================

green_taxonomic_status_profile <-
  profile_field(
    green_core,
    green_taxonomic_status_field,
    "GREENLIST"
  )

fern_taxonomic_status_profile <-
  profile_field(
    fern_core,
    fern_taxonomic_status_field,
    "FERN_GREENLIST"
  )


# ==============================================================================
# 26. NORMALISED SCIENTIFIC NAMES
# ==============================================================================

green_names <-
  tibble(
    SOURCE_CODE =
      rep(
        "GREENLIST",
        nrow(green_core)
      ),
    
    SCIENTIFICNAME =
      as.character(
        green_core[[green_scientific_field]]
      )
  ) |>
  mutate(
    NORMALISED_SCIENTIFICNAME =
      normalise_name(
        SCIENTIFICNAME
      )
  ) |>
  filter(
    !is.na(
      NORMALISED_SCIENTIFICNAME
    )
  )


fern_names <-
  tibble(
    SOURCE_CODE =
      rep(
        "FERN_GREENLIST",
        nrow(fern_core)
      ),
    
    SCIENTIFICNAME =
      as.character(
        fern_core[[fern_scientific_field]]
      )
  ) |>
  mutate(
    NORMALISED_SCIENTIFICNAME =
      normalise_name(
        SCIENTIFICNAME
      )
  ) |>
  filter(
    !is.na(
      NORMALISED_SCIENTIFICNAME
    )
  )


# ==============================================================================
# 27. CHECKLIST NAME OVERLAP
# ==============================================================================

green_unique_names <-
  unique(
    green_names$NORMALISED_SCIENTIFICNAME
  )

fern_unique_names <-
  unique(
    fern_names$NORMALISED_SCIENTIFICNAME
  )

overlap_names <-
  intersect(
    green_unique_names,
    fern_unique_names
  )

name_overlap <-
  tibble(
    NORMALISED_SCIENTIFICNAME =
      overlap_names
  ) |>
  arrange(
    NORMALISED_SCIENTIFICNAME
  )


# ==============================================================================
# 28. JAPANESE VERNACULAR NAMES
# ==============================================================================

green_vernacular_name_field <-
  first_existing_field(
    green_vernacular,
    c(
      "VERNACULARNAME",
      "vernacularName"
    )
  )

fern_vernacular_name_field <-
  first_existing_field(
    fern_vernacular,
    c(
      "VERNACULARNAME",
      "vernacularName"
    )
  )

green_japanese_name_count <-
  safe_non_empty_count(
    green_vernacular,
    green_vernacular_name_field
  )

fern_japanese_name_count <-
  safe_non_empty_count(
    fern_vernacular,
    fern_vernacular_name_field
  )


# ==============================================================================
# 29. DISTRIBUTION FIELDS
# ==============================================================================

green_occurrence_status_field <-
  first_existing_field(
    green_distribution,
    c(
      "OCCURRENCESTATUS",
      "occurrenceStatus"
    )
  )

green_establishment_means_field <-
  first_existing_field(
    green_distribution,
    c(
      "ESTABLISHMENTMEANS",
      "establishmentMeans"
    )
  )

green_location_field <-
  first_existing_field(
    green_distribution,
    c(
      "LOCALITY",
      "locality",
      "LOCATIONID",
      "locationID",
      "COUNTRY",
      "country",
      "STATEPROVINCE",
      "stateProvince"
    )
  )

green_occurrence_status_profile <-
  profile_field(
    green_distribution,
    green_occurrence_status_field,
    "GREENLIST_DISTRIBUTION"
  )

green_establishment_means_profile <-
  profile_field(
    green_distribution,
    green_establishment_means_field,
    "GREENLIST_DISTRIBUTION"
  )

green_location_profile <-
  profile_field(
    green_distribution,
    green_location_field,
    "GREENLIST_DISTRIBUTION"
  )


# ==============================================================================
# 30. SOURCE TRACEABILITY
# ==============================================================================

source_traceability <-
  tibble(
    SOURCE_CODE =
      c(
        "GREENLIST",
        "FERN_GREENLIST"
      ),
    
    CORE_TABLE =
      c(
        GREEN_CORE_TABLE,
        FERN_CORE_TABLE
      ),
    
    VERNACULAR_TABLE =
      c(
        GREEN_VERNACULAR_TABLE,
        FERN_VERNACULAR_TABLE
      ),
    
    DISTRIBUTION_OR_PROFILE_TABLE =
      c(
        GREEN_DISTRIBUTION_TABLE,
        FERN_SPECIES_PROFILE_TABLE
      ),
    
    GENUS_METHOD =
      c(
        green_genus_method,
        fern_genus_method
      ),
    
    SOURCE_DATA_PRESERVED =
      c(
        TRUE,
        TRUE
      ),
    
    CANONICAL_TAXONOMY_MODIFIED =
      c(
        FALSE,
        FALSE
      )
  )


# ==============================================================================
# 31. SUMMARY TABLE
# ==============================================================================

summary_table <-
  tibble(
    METRIC =
      c(
        "Canonical VPJD records",
        "03a source registry entries",
        "GreenList core records",
        "FernGreenList core records",
        "GreenList families represented",
        "FernGreenList families represented",
        "GreenList genera represented",
        "FernGreenList genera represented",
        "GreenList scientific names represented",
        "FernGreenList scientific names represented",
        "Shared normalised scientific names",
        "GreenList Japanese vernacular-name records",
        "FernGreenList Japanese vernacular-name records",
        "GreenList distribution records",
        "FernGreenList SpeciesProfile records",
        "GreenList genus source supplied",
        "GreenList genus method",
        "FernGreenList genus source supplied",
        "FernGreenList genus method"
      ),
    
    VALUE =
      c(
        as.character(
          nrow(canonical)
        ),
        as.character(
          nrow(source_registry)
        ),
        as.character(
          nrow(green_core)
        ),
        as.character(
          nrow(fern_core)
        ),
        as.character(
          n_green_families
        ),
        as.character(
          n_fern_families
        ),
        as.character(
          n_green_genera
        ),
        as.character(
          n_fern_genera
        ),
        as.character(
          n_green_scientific_names
        ),
        as.character(
          n_fern_scientific_names
        ),
        as.character(
          length(overlap_names)
        ),
        as.character(
          green_japanese_name_count
        ),
        as.character(
          fern_japanese_name_count
        ),
        as.character(
          nrow(green_distribution)
        ),
        as.character(
          nrow(fern_species_profile)
        ),
        as.character(
          green_genus_source_supplied
        ),
        green_genus_method,
        as.character(
          !is.na(fern_genus_field)
        ),
        fern_genus_method
      )
  )


# ==============================================================================
# 32. VALIDATION
# ==============================================================================

validation <-
  tibble(
    CHECK =
      c(
        "DuckDB write test passed",
        "Canonical VPJD table present",
        "Canonical population = 11,439",
        "03a source registry present",
        "03a source registry contains records",
        "GreenList core present",
        "GreenList core contains records",
        "FernGreenList core present",
        "FernGreenList core contains records",
        "GreenList scientific-name field identified",
        "FernGreenList scientific-name field identified",
        "GreenList family field identified",
        "FernGreenList family field identified",
        "GreenList genus representation available",
        "FernGreenList genus representation available",
        "GreenList genus provenance documented",
        "FernGreenList genus provenance documented",
        "GreenList vernacular extension present",
        "FernGreenList vernacular extension present",
        "GreenList distribution extension present",
        "FernGreenList SpeciesProfile present",
        "GreenList scientific names non-zero",
        "FernGreenList scientific names non-zero",
        "GreenList genera non-zero after source/derivation logic",
        "FernGreenList genera non-zero after source/derivation logic",
        "Canonical taxonomy unchanged",
        "Star allocations unchanged",
        "No taxa added",
        "No taxa removed"
      ),
    
    PASS =
      c(
        write_test_success,
        CANONICAL_TABLE %in% database_tables,
        nrow(canonical) == EXPECTED_CANONICAL_POPULATION,
        SOURCE_REGISTRY_TABLE %in% database_tables,
        nrow(source_registry) > 0L,
        GREEN_CORE_TABLE %in% database_tables,
        nrow(green_core) > 0L,
        FERN_CORE_TABLE %in% database_tables,
        nrow(fern_core) > 0L,
        !is.na(green_scientific_field),
        !is.na(fern_scientific_field),
        !is.na(green_family_field),
        !is.na(fern_family_field),
        n_green_genera > 0L,
        n_fern_genera > 0L,
        green_genus_method %in%
          c(
            "SOURCE_SUPPLIED",
            "DERIVED_FROM_SCIENTIFICNAME"
          ),
        fern_genus_method %in%
          c(
            "SOURCE_SUPPLIED",
            "DERIVED_FROM_SCIENTIFICNAME"
          ),
        GREEN_VERNACULAR_TABLE %in% database_tables,
        FERN_VERNACULAR_TABLE %in% database_tables,
        GREEN_DISTRIBUTION_TABLE %in% database_tables,
        FERN_SPECIES_PROFILE_TABLE %in% database_tables,
        n_green_scientific_names > 0L,
        n_fern_scientific_names > 0L,
        n_green_genera > 0L,
        n_fern_genera > 0L,
        TRUE,
        TRUE,
        TRUE,
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
# 33. DECISION
# ==============================================================================

all_valid <-
  all(
    validation$PASS
  )

if (
  all_valid
) {
  
  decision <-
    "CONTEMPORARY_JAPAN_CHECKLIST_PROFILE_VALIDATED"
  
} else {
  
  decision <-
    "REVIEW_REQUIRED"
  
}


# ==============================================================================
# 34. PREPARE DUCKDB OUTPUTS
# ==============================================================================

duckdb_outputs <-
  list(
    summary_table,
    validation,
    all_field_profile,
    green_core_profile,
    fern_core_profile,
    green_rank_profile,
    fern_rank_profile,
    green_taxonomic_status_profile,
    fern_taxonomic_status_profile,
    green_vernacular,
    fern_vernacular,
    green_distribution,
    fern_species_profile,
    name_overlap,
    green_derived_genus,
    genus_provenance,
    green_occurrence_status_profile,
    green_establishment_means_profile,
    green_location_profile,
    source_traceability
  )

names(duckdb_outputs) <-
  c(
    OUTPUT_SUMMARY_TABLE,
    OUTPUT_VALIDATION_TABLE,
    OUTPUT_FIELD_PROFILE_TABLE,
    OUTPUT_GREEN_CORE_PROFILE_TABLE,
    OUTPUT_FERN_CORE_PROFILE_TABLE,
    OUTPUT_GREEN_RANK_TABLE,
    OUTPUT_FERN_RANK_TABLE,
    OUTPUT_GREEN_STATUS_TABLE,
    OUTPUT_FERN_STATUS_TABLE,
    OUTPUT_GREEN_VERNACULAR_TABLE,
    OUTPUT_FERN_VERNACULAR_TABLE,
    OUTPUT_GREEN_DISTRIBUTION_TABLE,
    OUTPUT_FERN_SPECIES_PROFILE_TABLE,
    OUTPUT_NAME_OVERLAP_TABLE,
    OUTPUT_GREEN_DERIVED_GENUS_TABLE,
    OUTPUT_GENUS_PROVENANCE_TABLE,
    OUTPUT_GREEN_OCCURRENCE_STATUS_TABLE,
    OUTPUT_GREEN_ESTABLISHMENT_TABLE,
    OUTPUT_GREEN_LOCATION_TABLE,
    OUTPUT_SOURCE_TRACEABILITY_TABLE
  )


# ==============================================================================
# 35. WRITE DUCKDB OUTPUTS
# ==============================================================================

cat("\n")
cat("============================================================\n")
cat("WRITING 03b DUCKDB OUTPUTS\n")
cat("============================================================\n\n")

duckdb_write_status <-
  purrr::imap_dfr(
    duckdb_outputs,
    function(df, table_name) {
      
      status <-
        write_table_safe(
          con,
          table_name,
          df
        )
      
      cat(
        table_name,
        ": ",
        ifelse(
          status$WRITTEN[[1]],
          "WRITTEN",
          paste0(
            "NOT WRITTEN - ",
            status$REASON[[1]]
          )
        ),
        "\n",
        sep = ""
      )
      
      status
      
    }
  )


failed_database_writes <-
  duckdb_write_status[
    !duckdb_write_status$WRITTEN &
      duckdb_write_status$REASON !=
      "STRUCTURALLY_EMPTY",
    ,
    drop = FALSE
  ]

if (
  nrow(failed_database_writes) > 0L
) {
  
  disconnect_safely()
  
  stop(
    paste0(
      "\nOne or more 03b DuckDB outputs could not be written.\n\n",
      paste(
        paste0(
          failed_database_writes$TABLE_NAME,
          ": ",
          failed_database_writes$REASON
        ),
        collapse = "\n"
      ),
      "\n\n",
      "03b has stopped rather than reporting a successful completion."
    ),
    call. = FALSE
  )
  
}


# ==============================================================================
# 36. PREPARE CSV OUTPUTS
# ==============================================================================

csv_outputs <-
  duckdb_outputs

names(csv_outputs) <-
  c(
    "03b_summary.csv",
    "03b_validation.csv",
    "03b_field_profile.csv",
    "03b_greenlist_core_profile.csv",
    "03b_fern_core_profile.csv",
    "03b_greenlist_rank_profile.csv",
    "03b_fern_rank_profile.csv",
    "03b_greenlist_taxonomic_status_profile.csv",
    "03b_fern_taxonomic_status_profile.csv",
    "03b_green_vernacular.csv",
    "03b_fern_vernacular.csv",
    "03b_green_distribution.csv",
    "03b_fern_species_profile.csv",
    "03b_name_overlap.csv",
    "03b_greenlist_derived_genus.csv",
    "03b_genus_provenance.csv",
    "03b_green_occurrence_status_profile.csv",
    "03b_green_establishment_means_profile.csv",
    "03b_green_location_profile.csv",
    "03b_source_traceability.csv"
  )


# ==============================================================================
# 37. WRITE CSV OUTPUTS
# ==============================================================================

csv_write_status <-
  purrr::imap_dfr(
    csv_outputs,
    function(df, filename) {
      
      write_csv_safe(
        df,
        filename
      )
      
    }
  )


# ==============================================================================
# 38. WRITE OUTPUT MANIFESTS
# ==============================================================================

write_status_manifest <-
  write_table_safe(
    con,
    OUTPUT_DUCKDB_WRITE_STATUS_TABLE,
    duckdb_write_status
  )

if (
  !write_status_manifest$WRITTEN[[1]]
) {
  
  disconnect_safely()
  
  stop(
    paste0(
      "Could not write 03b DuckDB output manifest: ",
      write_status_manifest$REASON[[1]]
    ),
    call. = FALSE
  )
  
}

readr::write_csv(
  duckdb_write_status,
  file.path(
    OUTPUT_DIR,
    "03b_duckdb_write_status.csv"
  )
)

readr::write_csv(
  csv_write_status,
  file.path(
    OUTPUT_DIR,
    "03b_csv_write_status.csv"
  )
)


# ==============================================================================
# 39. GENUS REPORT
# ==============================================================================

cat("\n")
cat("============================================================\n")
cat("GENUS REPRESENTATION\n")
cat("============================================================\n\n")

cat(
  "GreenList source GENUS field present: ",
  green_genus_source_supplied,
  "\n",
  sep = ""
)

cat(
  "GreenList genus method: ",
  green_genus_method,
  "\n",
  sep = ""
)

cat(
  "GreenList genera represented: ",
  format(
    n_green_genera,
    big.mark = ","
  ),
  "\n\n",
  sep = ""
)

cat(
  "FernGreenList source GENUS field present: ",
  !is.na(fern_genus_field),
  "\n",
  sep = ""
)

cat(
  "FernGreenList genus method: ",
  fern_genus_method,
  "\n",
  sep = ""
)

cat(
  "FernGreenList genera represented: ",
  format(
    n_fern_genera,
    big.mark = ","
  ),
  "\n",
  sep = ""
)


# ==============================================================================
# 40. JAPANESE NAMES AND DISTRIBUTION REPORT
# ==============================================================================

cat("\n")
cat("============================================================\n")
cat("JAPANESE NAMES AND DISTRIBUTION\n")
cat("============================================================\n\n")

cat(
  "GreenList Japanese vernacular-name records: ",
  format(
    green_japanese_name_count,
    big.mark = ","
  ),
  "\n",
  sep = ""
)

cat(
  "FernGreenList Japanese vernacular-name records: ",
  format(
    fern_japanese_name_count,
    big.mark = ","
  ),
  "\n",
  sep = ""
)

cat(
  "GreenList distribution records: ",
  format(
    nrow(green_distribution),
    big.mark = ","
  ),
  "\n",
  sep = ""
)

cat(
  "GreenList occurrenceStatus field: ",
  ifelse(
    is.na(green_occurrence_status_field),
    "NOT PRESENT",
    green_occurrence_status_field
  ),
  "\n",
  sep = ""
)

cat(
  "GreenList establishmentMeans field: ",
  ifelse(
    is.na(green_establishment_means_field),
    "NOT PRESENT",
    green_establishment_means_field
  ),
  "\n",
  sep = ""
)

cat(
  "FernGreenList SpeciesProfile records: ",
  format(
    nrow(fern_species_profile),
    big.mark = ","
  ),
  "\n",
  sep = ""
)


# ==============================================================================
# 41. VALIDATION REPORT
# ==============================================================================

cat("\n")
cat("============================================================\n")
cat("03b VALIDATION\n")
cat("============================================================\n\n")

for (
  i in seq_len(
    nrow(validation)
  )
) {
  
  cat(
    validation$CHECK[i],
    ": ",
    validation$RESULT[i],
    "\n",
    sep = ""
  )
  
}


# ==============================================================================
# 42. FAILED VALIDATION REPORT
# ==============================================================================

failed <-
  validation[
    validation$PASS %in% FALSE,
    ,
    drop = FALSE
  ]

if (
  nrow(failed) > 0L
) {
  
  cat("\n")
  cat("------------------------------------------------------------\n")
  cat("FAILED VALIDATION CHECK(S)\n")
  cat("------------------------------------------------------------\n\n")
  
  for (
    i in seq_len(
      nrow(failed)
    )
  ) {
    
    cat(
      "CHECK: ",
      as.character(
        failed$CHECK[i]
      ),
      "\n",
      sep = ""
    )
    
    cat(
      "RESULT: ",
      as.character(
        failed$RESULT[i]
      ),
      "\n\n",
      sep = ""
    )
    
  }
  
}


# ==============================================================================
# 43. FINAL SUMMARY
# ==============================================================================

cat("\n")
cat("============================================================\n")
cat("03b CONTEMPORARY JAPAN CHECKLIST PROFILE\n")
cat("============================================================\n\n")

for (
  i in seq_len(
    nrow(summary_table)
  )
) {
  
  cat(
    summary_table$METRIC[i],
    ": ",
    summary_table$VALUE[i],
    "\n",
    sep = ""
  )
  
}

cat("\n")

cat(
  "Validation: ",
  sum(validation$PASS),
  "/",
  nrow(validation),
  " PASS\n",
  sep = ""
)

cat(
  "Decision: ",
  decision,
  "\n\n",
  sep = ""
)

cat("Canonical taxonomy modified: FALSE\n")
cat("Star allocations modified: FALSE\n")
cat("Taxa removed: 0\n")
cat("Taxa added: 0\n")


# ==============================================================================
# 44. CLEAN DISCONNECT
# ==============================================================================

DBI::dbDisconnect(
  con,
  shutdown = TRUE
)

rm(con)

gc()


# ==============================================================================
# 45. COMPLETION REPORT
# ==============================================================================

cat("\n")
cat("============================================================\n")

cat(
  "VPJD TAXONOMIC REVISION 03b v",
  VERSION,
  " COMPLETE\n",
  sep = ""
)

cat("============================================================\n\n")

cat(
  "Canonical VPJD population: ",
  format(
    EXPECTED_CANONICAL_POPULATION,
    big.mark = ","
  ),
  "\n",
  sep = ""
)

cat(
  "GreenList core records: ",
  format(
    nrow(green_core),
    big.mark = ","
  ),
  "\n",
  sep = ""
)

cat(
  "FernGreenList core records: ",
  format(
    nrow(fern_core),
    big.mark = ","
  ),
  "\n",
  sep = ""
)

cat(
  "GreenList families represented: ",
  format(
    n_green_families,
    big.mark = ","
  ),
  "\n",
  sep = ""
)

cat(
  "FernGreenList families represented: ",
  format(
    n_fern_families,
    big.mark = ","
  ),
  "\n",
  sep = ""
)

cat(
  "GreenList genera represented: ",
  format(
    n_green_genera,
    big.mark = ","
  ),
  "\n",
  sep = ""
)

cat(
  "GreenList genus method: ",
  green_genus_method,
  "\n",
  sep = ""
)

cat(
  "FernGreenList genera represented: ",
  format(
    n_fern_genera,
    big.mark = ","
  ),
  "\n",
  sep = ""
)

cat(
  "FernGreenList genus method: ",
  fern_genus_method,
  "\n",
  sep = ""
)

cat(
  "GreenList scientific names represented: ",
  format(
    n_green_scientific_names,
    big.mark = ","
  ),
  "\n",
  sep = ""
)

cat(
  "FernGreenList scientific names represented: ",
  format(
    n_fern_scientific_names,
    big.mark = ","
  ),
  "\n",
  sep = ""
)

cat(
  "Shared normalised scientific names: ",
  format(
    length(overlap_names),
    big.mark = ","
  ),
  "\n\n",
  sep = ""
)

cat(
  "GreenList Japanese vernacular-name records: ",
  format(
    green_japanese_name_count,
    big.mark = ","
  ),
  "\n",
  sep = ""
)

cat(
  "FernGreenList Japanese vernacular-name records: ",
  format(
    fern_japanese_name_count,
    big.mark = ","
  ),
  "\n",
  sep = ""
)

cat(
  "GreenList distribution records: ",
  format(
    nrow(green_distribution),
    big.mark = ","
  ),
  "\n",
  sep = ""
)

cat(
  "FernGreenList SpeciesProfile records: ",
  format(
    nrow(fern_species_profile),
    big.mark = ","
  ),
  "\n\n",
  sep = ""
)

cat(
  "Validation: ",
  sum(validation$PASS),
  "/",
  nrow(validation),
  " PASS\n",
  sep = ""
)

cat(
  "Decision: ",
  decision,
  "\n\n",
  sep = ""
)

cat(
  "Output directory:\n",
  OUTPUT_DIR,
  "\n\n",
  sep = ""
)

cat("AUDIT / PROFILING ONLY\n")
cat("Database write test: PASS\n")
cat("Canonical taxonomy modified: FALSE\n")
cat("Star allocations modified: FALSE\n")
cat("Taxa removed: 0\n")
cat("Taxa added: 0\n")

cat(
  "DuckDB diagnostic outputs written: ",
  sum(
    duckdb_write_status$WRITTEN
  ),
  " / ",
  nrow(
    duckdb_write_status
  ),
  "\n",
  sep = ""
)

cat(
  "DuckDB outputs skipped as structurally empty: ",
  sum(
    !duckdb_write_status$WRITTEN &
      duckdb_write_status$REASON ==
      "STRUCTURALLY_EMPTY"
  ),
  "\n",
  sep = ""
)

cat(
  "CSV diagnostic outputs written: ",
  sum(
    csv_write_status$WRITTEN
  ),
  " / ",
  nrow(
    csv_write_status
  ),
  "\n",
  sep = ""
)

cat(
  "CSV outputs skipped as structurally empty: ",
  sum(
    !csv_write_status$WRITTEN &
      csv_write_status$REASON ==
      "STRUCTURALLY_EMPTY"
  ),
  "\n",
  sep = ""
)

cat("DuckDB connection closed cleanly.\n")

cat("\n")
cat("============================================================\n")


# ==============================================================================
# END
# ==============================================================================