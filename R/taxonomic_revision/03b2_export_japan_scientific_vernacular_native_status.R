# ==============================================================================
# VPJD TAXONOMIC REVISION
# 03b2_export_japan_scientific_vernacular_native_status.R
#
# PURPOSE
# -------
# Export a simple, auditable inventory of contemporary Japanese checklist taxa
# containing:
#
#   - scientific name
#   - Japanese vernacular name
#   - simplified native / non-native status where supported
#   - original source status
#   - source checklist
#   - source taxon identifier
#
# PRIMARY SOURCES
# ---------------
# GreenList 2.02rc
# FernGreenList 2.0
#
# IMPORTANT
# ---------
# This is an export / diagnostic module only.
#
# It does NOT:
#   - modify canonical VPJD taxonomy;
#   - modify Star allocations;
#   - add or remove VPJD taxa;
#   - reconcile checklist concepts to VPJD;
#   - infer native status where the source does not provide usable evidence.
#
# VERSION
# -------
# 0.2.0
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
    "03b2_japan_scientific_vernacular_native_status"
  )

OUTPUT_MAIN <-
  file.path(
    OUTPUT_DIR,
    "japan_unique_scientific_vernacular_native_status.csv"
  )

OUTPUT_DISTRIBUTION_FIELDS <-
  file.path(
    OUTPUT_DIR,
    "greenlist_distribution_field_profile.csv"
  )

OUTPUT_STATUS_VALUES <-
  file.path(
    OUTPUT_DIR,
    "greenlist_source_status_values.csv"
  )

OUTPUT_SOURCE_RECORDS <-
  file.path(
    OUTPUT_DIR,
    "japan_scientific_vernacular_native_status_source_records.csv"
  )

dir.create(
  OUTPUT_DIR,
  recursive = TRUE,
  showWarnings = FALSE
)


# ==============================================================================
# 02. PACKAGES
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

if (length(missing_packages) > 0L) {
  
  stop(
    paste0(
      "Missing required package(s):\n",
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
# 03. HELPER FUNCTIONS
# ==============================================================================

normalise_text <-
  function(x) {
    
    x <- as.character(x)
    
    x[is.na(x)] <- ""
    
    x <-
      stringr::str_replace_all(
        x,
        "\u00A0",
        " "
      )
    
    x <-
      stringr::str_squish(
        x
      )
    
    x[x == ""] <- NA_character_
    
    x
  }


first_existing_field <-
  function(data, candidates) {
    
    if (ncol(data) == 0L) {
      return(NA_character_)
    }
    
    fields <- names(data)
    
    hit <-
      match(
        toupper(candidates),
        toupper(fields)
      )
    
    hit <-
      hit[
        !is.na(hit)
      ]
    
    if (length(hit) == 0L) {
      return(NA_character_)
    }
    
    fields[[hit[[1]]]]
  }


collapse_unique <-
  function(x) {
    
    x <-
      normalise_text(
        x
      )
    
    x <-
      x[
        !is.na(x)
      ]
    
    x <-
      sort(
        unique(x)
      )
    
    if (length(x) == 0L) {
      return(NA_character_)
    }
    
    paste(
      x,
      collapse = " | "
    )
  }


safe_read_table <-
  function(con, table_name, available_tables) {
    
    if (!(table_name %in% available_tables)) {
      return(tibble())
    }
    
    DBI::dbReadTable(
      con,
      table_name
    ) |>
      tibble::as_tibble()
  }


# ==============================================================================
# 04. DATABASE CHECK
# ==============================================================================

if (!file.exists(DB_PATH)) {
  
  stop(
    paste0(
      "VPJD DuckDB database not found:\n",
      DB_PATH
    )
  )
}


# ==============================================================================
# 05. CONNECT READ-ONLY
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
# 06. TABLE DEFINITIONS
# ==============================================================================

GREEN_CORE_TABLE <-
  "vpjd_taxrev_03a_greenlist_core"

FERN_CORE_TABLE <-
  "vpjd_taxrev_03a_fern_greenlist_core"

GREEN_DISTRIBUTION_TABLE <-
  "vpjd_taxrev_03a_greenlist_distribution_2"

GREEN_VERNACULAR_TABLE <-
  "vpjd_taxrev_03a_greenlist_vernacularname_1"

FERN_VERNACULAR_TABLE <-
  "vpjd_taxrev_03a_fern_greenlist_vernacularname_2"

FERN_SPECIES_PROFILE_TABLE <-
  "vpjd_taxrev_03a_fern_greenlist_speciesprofile_1"


# ==============================================================================
# 07. CHECK REQUIRED TABLES
# ==============================================================================

tables <-
  DBI::dbListTables(
    con
  )

required_tables <-
  c(
    GREEN_CORE_TABLE,
    FERN_CORE_TABLE
  )

missing_tables <-
  setdiff(
    required_tables,
    tables
  )

if (length(missing_tables) > 0L) {
  
  disconnect_safely()
  
  stop(
    paste0(
      "Required 03a table(s) missing:\n",
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

green <-
  safe_read_table(
    con,
    GREEN_CORE_TABLE,
    tables
  )

fern <-
  safe_read_table(
    con,
    FERN_CORE_TABLE,
    tables
  )

green_distribution <-
  safe_read_table(
    con,
    GREEN_DISTRIBUTION_TABLE,
    tables
  )

green_vernacular <-
  safe_read_table(
    con,
    GREEN_VERNACULAR_TABLE,
    tables
  )

fern_vernacular <-
  safe_read_table(
    con,
    FERN_VERNACULAR_TABLE,
    tables
  )

fern_species_profile <-
  safe_read_table(
    con,
    FERN_SPECIES_PROFILE_TABLE,
    tables
  )


# ==============================================================================
# 09. SOURCE TABLE SUMMARY
# ==============================================================================

cat("\n")
cat("============================================================\n")
cat("03b2 SOURCE TABLES\n")
cat("============================================================\n\n")

cat(
  "GreenList core records: ",
  format(nrow(green), big.mark = ","),
  "\n",
  sep = ""
)

cat(
  "FernGreenList core records: ",
  format(nrow(fern), big.mark = ","),
  "\n",
  sep = ""
)

cat(
  "GreenList distribution records: ",
  format(nrow(green_distribution), big.mark = ","),
  "\n",
  sep = ""
)

cat(
  "GreenList vernacular records: ",
  format(nrow(green_vernacular), big.mark = ","),
  "\n",
  sep = ""
)

cat(
  "FernGreenList vernacular records: ",
  format(nrow(fern_vernacular), big.mark = ","),
  "\n",
  sep = ""
)

cat(
  "FernGreenList SpeciesProfile records: ",
  format(nrow(fern_species_profile), big.mark = ","),
  "\n",
  sep = ""
)


# ==============================================================================
# 10. IDENTIFY GREENLIST CORE FIELDS
# ==============================================================================

green_id_field <-
  first_existing_field(
    green,
    c(
      "TAXONID",
      "DWCA_ID",
      "ID"
    )
  )

green_name_field <-
  first_existing_field(
    green,
    c(
      "SCIENTIFICNAME"
    )
  )

green_core_vernacular_field <-
  first_existing_field(
    green,
    c(
      "VERNACULARNAME"
    )
  )

if (is.na(green_name_field)) {
  
  disconnect_safely()
  
  stop(
    "GreenList SCIENTIFICNAME field could not be identified."
  )
}


# ==============================================================================
# 11. IDENTIFY FERNGREENLIST CORE FIELDS
# ==============================================================================

fern_id_field <-
  first_existing_field(
    fern,
    c(
      "TAXONID",
      "DWCA_ID",
      "ID"
    )
  )

fern_name_field <-
  first_existing_field(
    fern,
    c(
      "SCIENTIFICNAME"
    )
  )

fern_core_vernacular_field <-
  first_existing_field(
    fern,
    c(
      "VERNACULARNAME"
    )
  )

if (is.na(fern_name_field)) {
  
  disconnect_safely()
  
  stop(
    "FernGreenList SCIENTIFICNAME field could not be identified."
  )
}


# ==============================================================================
# 12. BUILD GREENLIST CORE INVENTORY
# ==============================================================================

if (!is.na(green_id_field)) {
  
  green_ids <-
    normalise_text(
      green[[green_id_field]]
    )
  
} else {
  
  green_ids <-
    as.character(
      seq_len(
        nrow(green)
      )
    )
}

if (!is.na(green_core_vernacular_field)) {
  
  green_core_japanese <-
    normalise_text(
      green[[green_core_vernacular_field]]
    )
  
} else {
  
  green_core_japanese <-
    rep(
      NA_character_,
      nrow(green)
    )
}

green_inventory <-
  tibble(
    
    SOURCE =
      rep(
        "GreenList 2.02rc",
        nrow(green)
      ),
    
    SOURCE_TAXON_ID =
      green_ids,
    
    SCIENTIFIC_NAME =
      normalise_text(
        green[[green_name_field]]
      ),
    
    JAPANESE_NAME_CORE =
      green_core_japanese
  )


# ==============================================================================
# 13. BUILD FERNGREENLIST CORE INVENTORY
# ==============================================================================

if (!is.na(fern_id_field)) {
  
  fern_ids <-
    normalise_text(
      fern[[fern_id_field]]
    )
  
} else {
  
  fern_ids <-
    as.character(
      seq_len(
        nrow(fern)
      )
    )
}

if (!is.na(fern_core_vernacular_field)) {
  
  fern_core_japanese <-
    normalise_text(
      fern[[fern_core_vernacular_field]]
    )
  
} else {
  
  fern_core_japanese <-
    rep(
      NA_character_,
      nrow(fern)
    )
}

fern_inventory <-
  tibble(
    
    SOURCE =
      rep(
        "FernGreenList 2.0",
        nrow(fern)
      ),
    
    SOURCE_TAXON_ID =
      fern_ids,
    
    SCIENTIFIC_NAME =
      normalise_text(
        fern[[fern_name_field]]
      ),
    
    JAPANESE_NAME_CORE =
      fern_core_japanese
  )


# ==============================================================================
# 14. PROFILE GREENLIST VERNACULAR EXTENSION
# ==============================================================================

green_vernacular_id_field <-
  first_existing_field(
    green_vernacular,
    c(
      "COREID",
      "TAXONID",
      "DWCA_ID",
      "ID"
    )
  )

green_vernacular_name_field <-
  first_existing_field(
    green_vernacular,
    c(
      "VERNACULARNAME"
    )
  )


# ==============================================================================
# 15. PROFILE FERNGREENLIST VERNACULAR EXTENSION
# ==============================================================================

fern_vernacular_id_field <-
  first_existing_field(
    fern_vernacular,
    c(
      "COREID",
      "TAXONID",
      "DWCA_ID",
      "ID"
    )
  )

fern_vernacular_name_field <-
  first_existing_field(
    fern_vernacular,
    c(
      "VERNACULARNAME"
    )
  )


# ==============================================================================
# 16. BUILD GREENLIST VERNACULAR LOOKUP
# ==============================================================================

green_vernacular_lookup <-
  tibble(
    SOURCE_TAXON_ID = character(),
    JAPANESE_NAME_EXTENSION = character()
  )

if (
  nrow(green_vernacular) > 0L &&
  !is.na(green_vernacular_id_field) &&
  !is.na(green_vernacular_name_field)
) {
  
  green_vernacular_lookup <-
    tibble(
      
      SOURCE_TAXON_ID =
        normalise_text(
          green_vernacular[[green_vernacular_id_field]]
        ),
      
      JAPANESE_NAME_EXTENSION =
        normalise_text(
          green_vernacular[[green_vernacular_name_field]]
        )
    ) |>
    filter(
      !is.na(SOURCE_TAXON_ID),
      !is.na(JAPANESE_NAME_EXTENSION)
    ) |>
    group_by(
      SOURCE_TAXON_ID
    ) |>
    summarise(
      JAPANESE_NAME_EXTENSION =
        collapse_unique(
          JAPANESE_NAME_EXTENSION
        ),
      .groups = "drop"
    )
}


# ==============================================================================
# 17. BUILD FERNGREENLIST VERNACULAR LOOKUP
# ==============================================================================

fern_vernacular_lookup <-
  tibble(
    SOURCE_TAXON_ID = character(),
    JAPANESE_NAME_EXTENSION = character()
  )

if (
  nrow(fern_vernacular) > 0L &&
  !is.na(fern_vernacular_id_field) &&
  !is.na(fern_vernacular_name_field)
) {
  
  fern_vernacular_lookup <-
    tibble(
      
      SOURCE_TAXON_ID =
        normalise_text(
          fern_vernacular[[fern_vernacular_id_field]]
        ),
      
      JAPANESE_NAME_EXTENSION =
        normalise_text(
          fern_vernacular[[fern_vernacular_name_field]]
        )
    ) |>
    filter(
      !is.na(SOURCE_TAXON_ID),
      !is.na(JAPANESE_NAME_EXTENSION)
    ) |>
    group_by(
      SOURCE_TAXON_ID
    ) |>
    summarise(
      JAPANESE_NAME_EXTENSION =
        collapse_unique(
          JAPANESE_NAME_EXTENSION
        ),
      .groups = "drop"
    )
}


# ==============================================================================
# 18. ATTACH GREENLIST JAPANESE NAMES
# ==============================================================================

green_inventory <-
  green_inventory |>
  left_join(
    green_vernacular_lookup,
    by = "SOURCE_TAXON_ID"
  ) |>
  mutate(
    JAPANESE_NAME =
      dplyr::coalesce(
        JAPANESE_NAME_EXTENSION,
        JAPANESE_NAME_CORE
      )
  ) |>
  select(
    SOURCE,
    SOURCE_TAXON_ID,
    SCIENTIFIC_NAME,
    JAPANESE_NAME
  )


# ==============================================================================
# 19. ATTACH FERNGREENLIST JAPANESE NAMES
# ==============================================================================

fern_inventory <-
  fern_inventory |>
  left_join(
    fern_vernacular_lookup,
    by = "SOURCE_TAXON_ID"
  ) |>
  mutate(
    JAPANESE_NAME =
      dplyr::coalesce(
        JAPANESE_NAME_EXTENSION,
        JAPANESE_NAME_CORE
      )
  ) |>
  select(
    SOURCE,
    SOURCE_TAXON_ID,
    SCIENTIFIC_NAME,
    JAPANESE_NAME
  )


# ==============================================================================
# 20. PROFILE GREENLIST DISTRIBUTION EXTENSION
# ==============================================================================

distribution_field_profile <-
  tibble(
    
    FIELD =
      names(
        green_distribution
      ),
    
    CLASS =
      vapply(
        green_distribution,
        function(x) {
          class(x)[[1]]
        },
        character(1)
      ),
    
    N_RECORDS =
      vapply(
        green_distribution,
        length,
        integer(1)
      ),
    
    N_NON_MISSING =
      vapply(
        green_distribution,
        function(x) {
          sum(
            !is.na(
              normalise_text(x)
            )
          )
        },
        integer(1)
      ),
    
    N_DISTINCT =
      vapply(
        green_distribution,
        function(x) {
          length(
            unique(
              normalise_text(x)[
                !is.na(
                  normalise_text(x)
                )
              ]
            )
          )
        },
        integer(1)
      )
  )

readr::write_excel_csv(
  distribution_field_profile,
  OUTPUT_DISTRIBUTION_FIELDS,
  na = ""
)


# ==============================================================================
# 21. IDENTIFY DISTRIBUTION FIELDS
# ==============================================================================

distribution_id_field <-
  first_existing_field(
    green_distribution,
    c(
      "COREID",
      "TAXONID",
      "DWCA_ID",
      "ID"
    )
  )

establishment_field <-
  first_existing_field(
    green_distribution,
    c(
      "ESTABLISHMENTMEANS",
      "ESTABLISHMENT_MEANS",
      "DEGREEOFESTABLISHMENT",
      "DEGREE_OF_ESTABLISHMENT",
      "OCCURRENCESTATUS",
      "OCCURRENCE_STATUS"
    )
  )

origin_field <-
  first_existing_field(
    green_distribution,
    c(
      "ORIGIN",
      "ORIGINSTATUS",
      "ORIGIN_STATUS"
    )
  )


# ==============================================================================
# 22. REPORT DISTRIBUTION FIELD DETECTION
# ==============================================================================

cat("\n")
cat("------------------------------------------------------------\n")
cat("GREENLIST DISTRIBUTION FIELD DETECTION\n")
cat("------------------------------------------------------------\n\n")

cat(
  "Distribution ID field: ",
  ifelse(
    is.na(distribution_id_field),
    "<not identified>",
    distribution_id_field
  ),
  "\n",
  sep = ""
)

cat(
  "Establishment/status field: ",
  ifelse(
    is.na(establishment_field),
    "<not identified>",
    establishment_field
  ),
  "\n",
  sep = ""
)

cat(
  "Origin field: ",
  ifelse(
    is.na(origin_field),
    "<not identified>",
    origin_field
  ),
  "\n",
  sep = ""
)


# ==============================================================================
# 23. SELECT SOURCE STATUS FIELD
# ==============================================================================

source_status_field <- NA_character_

if (!is.na(origin_field)) {
  
  source_status_field <-
    origin_field
  
} else if (!is.na(establishment_field)) {
  
  source_status_field <-
    establishment_field
}

cat(
  "Status field selected for export: ",
  ifelse(
    is.na(source_status_field),
    "<none>",
    source_status_field
  ),
  "\n",
  sep = ""
)


# ==============================================================================
# 24. PROFILE SOURCE STATUS VALUES
# ==============================================================================

status_value_profile <-
  tibble(
    SOURCE_NATIVE_STATUS = character(),
    N_RECORDS = integer()
  )

if (
  nrow(green_distribution) > 0L &&
  !is.na(source_status_field)
) {
  
  status_values <-
    normalise_text(
      green_distribution[[source_status_field]]
    )
  
  status_value_profile <-
    tibble(
      SOURCE_NATIVE_STATUS =
        status_values
    ) |>
    filter(
      !is.na(SOURCE_NATIVE_STATUS)
    ) |>
    count(
      SOURCE_NATIVE_STATUS,
      name = "N_RECORDS",
      sort = TRUE
    )
}

readr::write_excel_csv(
  status_value_profile,
  OUTPUT_STATUS_VALUES,
  na = ""
)


# ==============================================================================
# 25. BUILD GREENLIST STATUS LOOKUP
# ==============================================================================

green_status_lookup <-
  tibble(
    SOURCE_TAXON_ID = character(),
    SOURCE_NATIVE_STATUS = character()
  )

if (
  nrow(green_distribution) > 0L &&
  !is.na(distribution_id_field) &&
  !is.na(source_status_field)
) {
  
  green_status_lookup <-
    tibble(
      
      SOURCE_TAXON_ID =
        normalise_text(
          green_distribution[[distribution_id_field]]
        ),
      
      SOURCE_NATIVE_STATUS =
        normalise_text(
          green_distribution[[source_status_field]]
        )
    ) |>
    filter(
      !is.na(SOURCE_TAXON_ID)
    ) |>
    group_by(
      SOURCE_TAXON_ID
    ) |>
    summarise(
      SOURCE_NATIVE_STATUS =
        collapse_unique(
          SOURCE_NATIVE_STATUS
        ),
      .groups = "drop"
    )
}


# ==============================================================================
# 26. ATTACH GREENLIST SOURCE STATUS
# ==============================================================================

green_inventory <-
  green_inventory |>
  left_join(
    green_status_lookup,
    by = "SOURCE_TAXON_ID"
  )


# ==============================================================================
# 27. SEARCH FERNGREENLIST FOR EXPLICIT STATUS FIELD
# ==============================================================================

fern_status_field <-
  first_existing_field(
    fern,
    c(
      "ORIGIN",
      "ORIGINSTATUS",
      "ORIGIN_STATUS",
      "ESTABLISHMENTMEANS",
      "ESTABLISHMENT_MEANS",
      "DEGREEOFESTABLISHMENT",
      "OCCURRENCESTATUS"
    )
  )

if (!is.na(fern_status_field)) {
  
  fern_inventory$SOURCE_NATIVE_STATUS <-
    normalise_text(
      fern[[fern_status_field]]
    )
  
} else {
  
  fern_inventory$SOURCE_NATIVE_STATUS <-
    NA_character_
}


# ==============================================================================
# 28. COMBINE SOURCE INVENTORIES
# ==============================================================================

source_records <-
  bind_rows(
    green_inventory,
    fern_inventory
  ) |>
  filter(
    !is.na(SCIENTIFIC_NAME)
  )


# ==============================================================================
# 29. DERIVE SIMPLIFIED NATIVE STATUS
# ==============================================================================
#
# This classification is deliberately conservative.
#
# SOURCE_NATIVE_STATUS remains the authoritative source value.
#
# The simplified field is intended only as a convenient high-level grouping.
# ==============================================================================

source_records <-
  source_records |>
  mutate(
    
    STATUS_LOWER =
      stringr::str_to_lower(
        dplyr::coalesce(
          SOURCE_NATIVE_STATUS,
          ""
        )
      ),
    
    NATIVE_STATUS =
      case_when(
        
        is.na(SOURCE_NATIVE_STATUS) ~
          "NOT_AVAILABLE",
        
        str_detect(
          STATUS_LOWER,
          "non[- _]?native|introduced|alien|exotic|naturalised|naturalized"
        ) ~
          "NON_NATIVE",
        
        str_detect(
          STATUS_LOWER,
          "native|indigenous"
        ) ~
          "NATIVE",
        
        TRUE ~
          "SOURCE_STATUS_OTHER"
      )
  ) |>
  select(
    -STATUS_LOWER
  )


# ==============================================================================
# 30. EXPORT SOURCE-LEVEL RECORDS
# ==============================================================================

source_records_export <-
  source_records |>
  arrange(
    SOURCE,
    SCIENTIFIC_NAME,
    JAPANESE_NAME
  )

readr::write_excel_csv(
  source_records_export,
  OUTPUT_SOURCE_RECORDS,
  na = ""
)


# ==============================================================================
# 31. COLLAPSE TO UNIQUE SCIENTIFIC NAMES
# ==============================================================================

final_export <-
  source_records |>
  group_by(
    SCIENTIFIC_NAME
  ) |>
  summarise(
    
    JAPANESE_NAME =
      collapse_unique(
        JAPANESE_NAME
      ),
    
    NATIVE_STATUS =
      collapse_unique(
        NATIVE_STATUS
      ),
    
    SOURCE_NATIVE_STATUS =
      collapse_unique(
        SOURCE_NATIVE_STATUS
      ),
    
    SOURCE =
      collapse_unique(
        SOURCE
      ),
    
    SOURCE_TAXON_ID =
      collapse_unique(
        SOURCE_TAXON_ID
      ),
    
    .groups = "drop"
  ) |>
  arrange(
    SCIENTIFIC_NAME
  )


# ==============================================================================
# 32. VALIDATE UNIQUE SCIENTIFIC NAMES
# ==============================================================================

n_duplicate_names <-
  sum(
    duplicated(
      final_export$SCIENTIFIC_NAME
    )
  )

if (n_duplicate_names > 0L) {
  
  disconnect_safely()
  
  stop(
    paste0(
      "Final export contains ",
      n_duplicate_names,
      " duplicated scientific-name rows."
    )
  )
}


# ==============================================================================
# 33. VALIDATE NON-MISSING SCIENTIFIC NAMES
# ==============================================================================

n_missing_scientific_names <-
  sum(
    is.na(
      final_export$SCIENTIFIC_NAME
    )
  )

if (n_missing_scientific_names > 0L) {
  
  disconnect_safely()
  
  stop(
    paste0(
      "Final export contains ",
      n_missing_scientific_names,
      " missing scientific names."
    )
  )
}


# ==============================================================================
# 34. WRITE MAIN CSV
# ==============================================================================

readr::write_excel_csv(
  final_export,
  OUTPUT_MAIN,
  na = ""
)


# ==============================================================================
# 35. SUMMARY STATISTICS
# ==============================================================================

n_total <-
  nrow(
    final_export
  )

n_with_japanese <-
  sum(
    !is.na(
      final_export$JAPANESE_NAME
    )
  )

n_without_japanese <-
  sum(
    is.na(
      final_export$JAPANESE_NAME
    )
  )

n_with_source_status <-
  sum(
    !is.na(
      final_export$SOURCE_NATIVE_STATUS
    )
  )

n_without_source_status <-
  sum(
    is.na(
      final_export$SOURCE_NATIVE_STATUS
    )
  )


# ==============================================================================
# 36. STATUS SUMMARY
# ==============================================================================

status_summary <-
  final_export |>
  count(
    NATIVE_STATUS,
    name = "N_SCIENTIFIC_NAMES",
    sort = TRUE
  )


# ==============================================================================
# 37. SOURCE SUMMARY
# ==============================================================================

source_summary <-
  source_records |>
  group_by(
    SOURCE
  ) |>
  summarise(
    
    N_SOURCE_RECORDS =
      n(),
    
    N_UNIQUE_SCIENTIFIC_NAMES =
      n_distinct(
        SCIENTIFIC_NAME
      ),
    
    N_WITH_JAPANESE_NAME =
      sum(
        !is.na(
          JAPANESE_NAME
        )
      ),
    
    N_WITH_SOURCE_STATUS =
      sum(
        !is.na(
          SOURCE_NATIVE_STATUS
        )
      ),
    
    .groups = "drop"
  )


# ==============================================================================
# 38. FINAL CONSOLE REPORT
# ==============================================================================

cat("\n")
cat("============================================================\n")
cat("03b2 JAPAN SCIENTIFIC / VERNACULAR / STATUS EXPORT\n")
cat("============================================================\n\n")

cat(
  "Version: ",
  VERSION,
  "\n\n",
  sep = ""
)

cat(
  "Unique scientific names: ",
  format(
    n_total,
    big.mark = ","
  ),
  "\n",
  sep = ""
)

cat(
  "With Japanese vernacular name: ",
  format(
    n_with_japanese,
    big.mark = ","
  ),
  "\n",
  sep = ""
)

cat(
  "Without Japanese vernacular name: ",
  format(
    n_without_japanese,
    big.mark = ","
  ),
  "\n",
  sep = ""
)

cat(
  "With source native/status evidence: ",
  format(
    n_with_source_status,
    big.mark = ","
  ),
  "\n",
  sep = ""
)

cat(
  "Without source native/status evidence: ",
  format(
    n_without_source_status,
    big.mark = ","
  ),
  "\n\n",
  sep = ""
)


# ==============================================================================
# 39. PRINT STATUS SUMMARY
# ==============================================================================

cat("------------------------------------------------------------\n")
cat("SIMPLIFIED STATUS SUMMARY\n")
cat("------------------------------------------------------------\n\n")

print(
  status_summary,
  n = Inf,
  width = Inf
)

cat("\n")


# ==============================================================================
# 40. PRINT SOURCE SUMMARY
# ==============================================================================

cat("------------------------------------------------------------\n")
cat("SOURCE SUMMARY\n")
cat("------------------------------------------------------------\n\n")

print(
  source_summary,
  n = Inf,
  width = Inf
)

cat("\n")


# ==============================================================================
# 41. OUTPUT REPORT
# ==============================================================================

cat("------------------------------------------------------------\n")
cat("OUTPUT FILES\n")
cat("------------------------------------------------------------\n\n")

cat(
  "Main unique-name export:\n",
  OUTPUT_MAIN,
  "\n\n",
  sep = ""
)

cat(
  "Source-level records:\n",
  OUTPUT_SOURCE_RECORDS,
  "\n\n",
  sep = ""
)

cat(
  "GreenList distribution field profile:\n",
  OUTPUT_DISTRIBUTION_FIELDS,
  "\n\n",
  sep = ""
)

cat(
  "GreenList source-status values:\n",
  OUTPUT_STATUS_VALUES,
  "\n\n",
  sep = ""
)


# ==============================================================================
# 42. CLEAN DISCONNECT
# ==============================================================================

DBI::dbDisconnect(
  con,
  shutdown = TRUE
)

rm(con)

gc()


# ==============================================================================
# 43. COMPLETION REPORT
# ==============================================================================

cat("\n")
cat("============================================================\n")
cat("03b2 COMPLETE\n")
cat("============================================================\n\n")

cat("EXPORT / DIAGNOSTIC ONLY\n")
cat("Unique scientific-name constraint: PASS\n")
cat("Missing scientific names: 0\n")
cat("Source status retained verbatim: TRUE\n")
cat("Simplified native status derived conservatively: TRUE\n")
cat("Unsupported native-status inference performed: FALSE\n")
cat("Canonical VPJD taxonomy modified: FALSE\n")
cat("Star allocations modified: FALSE\n")
cat("Taxa removed: 0\n")
cat("Taxa added: 0\n")
cat("DuckDB modified: FALSE\n")
cat("Database connection closed cleanly: TRUE\n")

cat("\n")
cat("============================================================\n")


# ==============================================================================
# END
# ==============================================================================