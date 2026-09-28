# =============================================================================
# 02_dbf_import.R
# VPJD-OJPCP
#
# GEOJAPAN DBF source ingestion
#
# Version: 0.1.0
#
# PURPOSE
# -------
# Import the historical GEOJAPAN SPECIES.DBF taxonomic database into the
# reproducible VPJD workflow.
#
# This module:
#   1. Preserves the original DBF as immutable source evidence.
#   2. Imports all source fields without imposing current taxonomy.
#   3. Converts legacy character data to a UTF-8 working representation.
#   4. Preserves original GEOJAPAN identifiers and taxonomic relationships.
#   5. Creates a curated source-level taxon table.
#   6. Creates a source-level synonym/relationship table.
#   7. Creates diagnostic and audit outputs.
#
# This module DOES NOT:
#   - reconcile names against WCVP;
#   - assign VPJD taxon IDs;
#   - transfer Stars between taxonomic concepts;
#   - decide current taxonomic acceptance;
#   - calculate new Stars;
#   - infer native status;
#   - acquire GBIF records.
#
# GEOJAPAN source taxonomy is evidence, not the current VPJD taxonomy.
#
# =============================================================================


# =============================================================================
# 0. PACKAGES
# =============================================================================

library(dplyr)
library(readr)
library(tibble)
library(stringr)
library(here)
library(foreign)


# =============================================================================
# 1. MODULE METADATA
# =============================================================================

DBF_IMPORT_VERSION <- "0.1.0"

GEOJAPAN_SOURCE_NAME <- "GEOJAPAN"

GEOJAPAN_SOURCE_TABLE <- "SPECIES.DBF"

GEOJAPAN_SOURCE_ENCODING <- "CP1252"

VPJD_TARGET_ENCODING <- "UTF-8"


# =============================================================================
# 2. PATHS
# =============================================================================

geojapan_raw_dir <- here::here(
  "data",
  "raw",
  "external",
  "GEOJAPAN"
)

geojapan_source_file <- here::here(
  "data",
  "raw",
  "external",
  "GEOJAPAN",
  "SPECIES.DBF"
)

geojapan_interim_dir <- here::here(
  "data",
  "interim",
  "GEOJAPAN"
)

geojapan_audit_dir <- here::here(
  "outputs",
  "tables",
  "GEOJAPAN"
)

dir.create(
  geojapan_interim_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  geojapan_audit_dir,
  recursive = TRUE,
  showWarnings = FALSE
)


# =============================================================================
# 3. GENERAL HELPERS
# =============================================================================

timestamp_utc <- function() {
  
  format(
    Sys.time(),
    tz = "UTC",
    format = "%Y-%m-%dT%H:%M:%SZ"
  )
}


nonblank <- function(x) {
  
  !is.na(x) &
    nzchar(trimws(as.character(x)))
}


normalise_blank <- function(x) {
  
  if (!is.character(x)) {
    return(x)
  }
  
  x <- trimws(x)
  
  x[x == ""] <- NA_character_
  
  x
}


# =============================================================================
# 4. SOURCE VALIDATION
# =============================================================================

validate_geojapan_source <- function(
    path = geojapan_source_file
) {
  
  if (!file.exists(path)) {
    
    stop(
      "GEOJAPAN source file not found: ",
      path
    )
  }
  
  info <- file.info(path)
  
  if (is.na(info$size) || info$size <= 0) {
    
    stop(
      "GEOJAPAN source file exists but appears empty: ",
      path
    )
  }
  
  invisible(TRUE)
}


# =============================================================================
# 5. IMPORT RAW DBF
# =============================================================================

import_geojapan_species_raw <- function(
    path = geojapan_source_file
) {
  
  validate_geojapan_source(path)
  
  message(
    "Importing GEOJAPAN source: ",
    path
  )
  
  dat <- foreign::read.dbf(
    path,
    as.is = TRUE
  )
  
  dat <- tibble::as_tibble(dat)
  
  message(
    "GEOJAPAN raw import: ",
    format(nrow(dat), big.mark = ","),
    " records × ",
    format(ncol(dat), big.mark = ","),
    " fields."
  )
  
  dat
}


# =============================================================================
# 6. ENCODING DIAGNOSTIC
# =============================================================================

diagnose_character_encoding <- function(dat) {
  
  character_fields <- names(dat)[
    vapply(
      dat,
      is.character,
      logical(1)
    )
  ]
  
  if (length(character_fields) == 0) {
    
    return(
      tibble(
        field = character(),
        nonmissing_values = integer(),
        invalid_utf8 = integer()
      )
    )
  }
  
  result <- lapply(
    character_fields,
    function(nm) {
      
      x <- dat[[nm]]
      
      nonmissing <- !is.na(x)
      
      validated <- iconv(
        x,
        from = "UTF-8",
        to = "UTF-8",
        sub = NA
      )
      
      tibble(
        field = nm,
        
        nonmissing_values =
          sum(
            nonmissing,
            na.rm = TRUE
          ),
        
        invalid_utf8 =
          sum(
            nonmissing &
              is.na(validated),
            na.rm = TRUE
          )
      )
    }
  )
  
  bind_rows(result) %>%
    arrange(
      desc(invalid_utf8),
      field
    )
}


# =============================================================================
# 7. CREATE UTF-8 WORKING REPRESENTATION
# =============================================================================

convert_geojapan_encoding <- function(
    dat,
    source_encoding = GEOJAPAN_SOURCE_ENCODING
) {
  
  dat %>%
    mutate(
      across(
        where(is.character),
        ~ iconv(
          .x,
          from = source_encoding,
          to = VPJD_TARGET_ENCODING,
          sub = NA
        )
      )
    )
}


# =============================================================================
# 8. VALIDATE WORKING ENCODING
# =============================================================================

validate_utf8_working_copy <- function(dat) {
  
  audit <- diagnose_character_encoding(dat)
  
  failures <- sum(
    audit$invalid_utf8,
    na.rm = TRUE
  )
  
  if (failures > 0) {
    
    warning(
      "UTF-8 working copy contains ",
      failures,
      " invalid character value(s)."
    )
  }
  
  list(
    audit = audit,
    failures = failures
  )
}


# =============================================================================
# 9. EXPECTED SOURCE FIELDS
# =============================================================================

GEOJAPAN_KEY_FIELDS <- c(
  "SPNUMBER",
  "GECODE",
  "MERGETO",
  "SYNOF",
  "INV",
  "VALIDLINK",
  "TAXSTAT",
  "ALTTAXSTAT",
  "LEGITIMACY",
  "SYNNOTE",
  "NOMNOTE",
  "TYPENOTE",
  "VAGUE",
  "GENHYBRID",
  "SPHYBRID",
  "HYBRID",
  "CULTIVAR",
  "PEDIGREE",
  "SYNTOT",
  "TYPELINK",
  "SYNCAT",
  "SPQUICK",
  "CF",
  "SP1",
  "HOMONYM",
  "POLY",
  "RANK1",
  "SP2",
  "RANK2",
  "SP3",
  "AUCODE1",
  "AUCODE2",
  "AUCODE3",
  "CITATION",
  "YEAR",
  "DISTRIB",
  "STAR",
  "STARNOTE",
  "SPSCORE",
  "IUCN",
  "CRITERIA",
  "TDWG",
  "TDWGTOTALS",
  "FULLNAME",
  "POLYNOMIAL",
  "SPECTOTAL",
  "COMMON",
  "IPNI",
  "ENTRYDATE",
  "WHO",
  "MOTHERSP",
  "MOTHERFULL",
  "GENUS",
  "FAMILY"
)


audit_source_fields <- function(dat) {
  
  tibble(
    field = GEOJAPAN_KEY_FIELDS,
    present = GEOJAPAN_KEY_FIELDS %in% names(dat)
  )
}


# =============================================================================
# 10. SOURCE-LEVEL TAXON TABLE
# =============================================================================

build_geojapan_taxa <- function(dat) {
  
  required <- c(
    "SPNUMBER",
    "FULLNAME",
    "TAXSTAT",
    "SYNOF"
  )
  
  missing_required <- setdiff(
    required,
    names(dat)
  )
  
  if (length(missing_required) > 0) {
    
    stop(
      "Required GEOJAPAN field(s) missing: ",
      paste(
        missing_required,
        collapse = ", "
      )
    )
  }
  
  acquired_utc <- timestamp_utc()
  
  out <- dat %>%
    
    mutate(
      across(
        where(is.character),
        normalise_blank
      )
    ) %>%
    
    transmute(
      
      # -------------------------------------------------------
      # VPJD provenance
      # -------------------------------------------------------
      
      vpjd_source_repository =
        GEOJAPAN_SOURCE_NAME,
      
      vpjd_source_table =
        GEOJAPAN_SOURCE_TABLE,
      
      vpjd_import_module =
        "02_dbf_import.R",
      
      vpjd_import_version =
        DBF_IMPORT_VERSION,
      
      vpjd_imported_utc =
        acquired_utc,
      
      source_encoding =
        GEOJAPAN_SOURCE_ENCODING,
      
      working_encoding =
        VPJD_TARGET_ENCODING,
      
      
      # -------------------------------------------------------
      # Stable source identifiers
      # -------------------------------------------------------
      
      geojapan_spnumber =
        SPNUMBER,
      
      geojapan_gecode =
        GECODE,
      
      geojapan_merge_to =
        MERGETO,
      
      geojapan_synof =
        SYNOF,
      
      geojapan_validlink =
        VALIDLINK,
      
      
      # -------------------------------------------------------
      # Source taxonomic status
      # -------------------------------------------------------
      
      geojapan_taxstat =
        TAXSTAT,
      
      geojapan_alt_taxstat =
        ALTTAXSTAT,
      
      geojapan_legitimacy =
        LEGITIMACY,
      
      geojapan_synnote =
        SYNNOTE,
      
      geojapan_nomnote =
        NOMNOTE,
      
      geojapan_typenote =
        TYPENOTE,
      
      geojapan_vague =
        VAGUE,
      
      
      # -------------------------------------------------------
      # Hybrid / cultivar evidence
      # -------------------------------------------------------
      
      geojapan_genhybrid =
        GENHYBRID,
      
      geojapan_sphybrid =
        SPHYBRID,
      
      geojapan_hybrid =
        HYBRID,
      
      geojapan_cultivar =
        CULTIVAR,
      
      geojapan_pedigree =
        PEDIGREE,
      
      
      # -------------------------------------------------------
      # Name components
      # -------------------------------------------------------
      
      geojapan_spquick =
        SPQUICK,
      
      geojapan_cf =
        CF,
      
      geojapan_sp1 =
        SP1,
      
      geojapan_homonym =
        HOMONYM,
      
      geojapan_poly =
        POLY,
      
      geojapan_rank1 =
        RANK1,
      
      geojapan_sp2 =
        SP2,
      
      geojapan_rank2 =
        RANK2,
      
      geojapan_sp3 =
        SP3,
      
      
      # -------------------------------------------------------
      # Original source names
      # -------------------------------------------------------
      
      geojapan_fullname =
        FULLNAME,
      
      geojapan_polynomial =
        POLYNOMIAL,
      
      geojapan_genus =
        GENUS,
      
      geojapan_family =
        FAMILY,
      
      geojapan_mother_spnumber =
        MOTHERSP,
      
      geojapan_mother_fullname =
        MOTHERFULL,
      
      
      # -------------------------------------------------------
      # Nomenclatural evidence
      # -------------------------------------------------------
      
      geojapan_aucode1 =
        AUCODE1,
      
      geojapan_aucode2 =
        AUCODE2,
      
      geojapan_aucode3 =
        AUCODE3,
      
      geojapan_citation =
        CITATION,
      
      geojapan_publication_year =
        YEAR,
      
      geojapan_ipni =
        IPNI,
      
      
      # -------------------------------------------------------
      # Historical distribution evidence
      # -------------------------------------------------------
      
      geojapan_distribution =
        DISTRIB,
      
      geojapan_tdwg =
        TDWG,
      
      geojapan_tdwg_totals =
        TDWGTOTALS,
      
      
      # -------------------------------------------------------
      # Historical conservation / Star evidence
      #
      # IMPORTANT:
      # These values are preserved as source evidence only.
      # They are not automatically treated as current VPJD Star.
      # -------------------------------------------------------
      
      geojapan_star =
        STAR,
      
      geojapan_star_note =
        STARNOTE,
      
      geojapan_sp_score =
        SPSCORE,
      
      geojapan_iucn =
        IUCN,
      
      geojapan_iucn_criteria =
        CRITERIA,
      
      
      # -------------------------------------------------------
      # Source database metadata
      # -------------------------------------------------------
      
      geojapan_spectotal =
        SPECTOTAL,
      
      geojapan_common_name =
        COMMON,
      
      geojapan_entry_date =
        ENTRYDATE,
      
      geojapan_entry_by =
        WHO
    )
  
  out
}


# =============================================================================
# 11. BUILD GEOJAPAN TAXONOMIC RELATIONSHIPS
# =============================================================================

build_geojapan_relationships <- function(
    geojapan_taxa
) {
  
  lookup <- geojapan_taxa %>%
    select(
      target_spnumber =
        geojapan_spnumber,
      
      target_fullname =
        geojapan_fullname,
      
      target_taxstat =
        geojapan_taxstat,
      
      target_star =
        geojapan_star
    )
  
  synonym_links <- geojapan_taxa %>%
    filter(
      !is.na(geojapan_synof)
    ) %>%
    transmute(
      
      relationship_type =
        "SYNOF",
      
      source_spnumber =
        geojapan_spnumber,
      
      source_fullname =
        geojapan_fullname,
      
      source_taxstat =
        geojapan_taxstat,
      
      source_star =
        geojapan_star,
      
      target_spnumber =
        geojapan_synof
    ) %>%
    left_join(
      lookup,
      by = "target_spnumber"
    )
  
  merge_links <- geojapan_taxa %>%
    filter(
      !is.na(geojapan_merge_to)
    ) %>%
    transmute(
      
      relationship_type =
        "MERGETO",
      
      source_spnumber =
        geojapan_spnumber,
      
      source_fullname =
        geojapan_fullname,
      
      source_taxstat =
        geojapan_taxstat,
      
      source_star =
        geojapan_star,
      
      target_spnumber =
        geojapan_merge_to
    ) %>%
    left_join(
      lookup,
      by = "target_spnumber"
    )
  
  bind_rows(
    synonym_links,
    merge_links
  ) %>%
    mutate(
      target_found =
        !is.na(target_fullname)
    )
}


# =============================================================================
# 12. TAXONOMIC SUMMARY AUDIT
# =============================================================================

build_geojapan_taxonomic_audit <- function(
    geojapan_taxa
) {
  
  tibble(
    metric = c(
      "source_records",
      "unique_spnumber",
      "named_records",
      "unique_fullnames",
      "accepted_source_records",
      "synonym_links",
      "merge_links",
      "historical_star_records",
      "hybrid_flagged_records",
      "cultivar_flagged_records"
    ),
    
    value = c(
      
      nrow(geojapan_taxa),
      
      n_distinct(
        geojapan_taxa$geojapan_spnumber,
        na.rm = TRUE
      ),
      
      sum(
        nonblank(
          geojapan_taxa$geojapan_fullname
        )
      ),
      
      n_distinct(
        geojapan_taxa$geojapan_fullname[
          nonblank(
            geojapan_taxa$geojapan_fullname
          )
        ]
      ),
      
      sum(
        tolower(
          geojapan_taxa$geojapan_taxstat
        ) == "acc",
        na.rm = TRUE
      ),
      
      sum(
        !is.na(
          geojapan_taxa$geojapan_synof
        )
      ),
      
      sum(
        !is.na(
          geojapan_taxa$geojapan_merge_to
        )
      ),
      
      sum(
        nonblank(
          geojapan_taxa$geojapan_star
        )
      ),
      
      sum(
        nonblank(
          geojapan_taxa$geojapan_genhybrid
        ) |
          nonblank(
            geojapan_taxa$geojapan_sphybrid
          ) |
          nonblank(
            geojapan_taxa$geojapan_hybrid
          )
      ),
      
      sum(
        nonblank(
          geojapan_taxa$geojapan_cultivar
        )
      )
    )
  )
}


# =============================================================================
# 13. STATUS AUDIT
# =============================================================================

build_geojapan_status_audit <- function(
    geojapan_taxa
) {
  
  geojapan_taxa %>%
    count(
      geojapan_taxstat,
      sort = TRUE,
      name = "records"
    )
}


# =============================================================================
# 14. STAR AUDIT
# =============================================================================

build_geojapan_star_audit <- function(
    geojapan_taxa
) {
  
  geojapan_taxa %>%
    count(
      geojapan_taxstat,
      geojapan_star,
      sort = TRUE,
      name = "records"
    )
}


# =============================================================================
# 15. RANK AUDIT
# =============================================================================

build_geojapan_rank_audit <- function(
    geojapan_taxa
) {
  
  geojapan_taxa %>%
    count(
      geojapan_rank1,
      geojapan_rank2,
      sort = TRUE,
      name = "records"
    )
}


# =============================================================================
# 16. RELATIONSHIP AUDIT
# =============================================================================

build_geojapan_relationship_audit <- function(
    relationships
) {
  
  relationships %>%
    count(
      relationship_type,
      target_found,
      sort = TRUE,
      name = "records"
    )
}


# =============================================================================
# 17. DUPLICATE IDENTIFIER AUDIT
# =============================================================================

build_geojapan_identifier_audit <- function(
    geojapan_taxa
) {
  
  geojapan_taxa %>%
    filter(
      !is.na(geojapan_spnumber)
    ) %>%
    count(
      geojapan_spnumber,
      sort = TRUE,
      name = "records"
    ) %>%
    filter(
      records > 1
    )
}


# =============================================================================
# 18. MAIN IMPORT FUNCTION
# =============================================================================

import_geojapan_species <- function(
    write_outputs = TRUE
) {
  
  # ---------------------------------------------------------------------------
  # Import raw source
  # ---------------------------------------------------------------------------
  
  raw <- import_geojapan_species_raw()
  
  
  # ---------------------------------------------------------------------------
  # Diagnose raw encoding
  # ---------------------------------------------------------------------------
  
  raw_encoding_audit <-
    diagnose_character_encoding(raw)
  
  
  # ---------------------------------------------------------------------------
  # Create working UTF-8 representation
  # ---------------------------------------------------------------------------
  
  working <-
    convert_geojapan_encoding(raw)
  
  
  # ---------------------------------------------------------------------------
  # Validate working encoding
  # ---------------------------------------------------------------------------
  
  working_encoding <-
    validate_utf8_working_copy(
      working
    )
  
  
  # ---------------------------------------------------------------------------
  # Source field audit
  # ---------------------------------------------------------------------------
  
  field_audit <-
    audit_source_fields(
      working
    )
  
  
  # ---------------------------------------------------------------------------
  # Curated source-level taxon table
  # ---------------------------------------------------------------------------
  
  taxa <-
    build_geojapan_taxa(
      working
    )
  
  
  # ---------------------------------------------------------------------------
  # Relationships
  # ---------------------------------------------------------------------------
  
  relationships <-
    build_geojapan_relationships(
      taxa
    )
  
  
  # ---------------------------------------------------------------------------
  # Audits
  # ---------------------------------------------------------------------------
  
  taxonomic_audit <-
    build_geojapan_taxonomic_audit(
      taxa
    )
  
  status_audit <-
    build_geojapan_status_audit(
      taxa
    )
  
  star_audit <-
    build_geojapan_star_audit(
      taxa
    )
  
  rank_audit <-
    build_geojapan_rank_audit(
      taxa
    )
  
  relationship_audit <-
    build_geojapan_relationship_audit(
      relationships
    )
  
  identifier_audit <-
    build_geojapan_identifier_audit(
      taxa
    )
  
  
  # ---------------------------------------------------------------------------
  # Write outputs
  # ---------------------------------------------------------------------------
  
  if (isTRUE(write_outputs)) {
    
    # Complete UTF-8 working representation.
    #
    # RDS is authoritative for this interim representation because it
    # preserves R data types and the complete imported source structure.
    
    saveRDS(
      working,
      file.path(
        geojapan_interim_dir,
        "geojapan_species_source.rds"
      )
    )
    
    
    # Curated source-level taxon table
    
    saveRDS(
      taxa,
      file.path(
        geojapan_interim_dir,
        "geojapan_taxa.rds"
      )
    )
    
    readr::write_csv(
      taxa,
      file.path(
        geojapan_interim_dir,
        "geojapan_taxa.csv"
      ),
      na = ""
    )
    
    
    # Relationships
    
    saveRDS(
      relationships,
      file.path(
        geojapan_interim_dir,
        "geojapan_taxonomic_relationships.rds"
      )
    )
    
    readr::write_csv(
      relationships,
      file.path(
        geojapan_interim_dir,
        "geojapan_taxonomic_relationships.csv"
      ),
      na = ""
    )
    
    
    # Audits
    
    readr::write_csv(
      field_audit,
      file.path(
        geojapan_audit_dir,
        "geojapan_source_field_audit.csv"
      )
    )
    
    readr::write_csv(
      raw_encoding_audit,
      file.path(
        geojapan_audit_dir,
        "geojapan_raw_encoding_audit.csv"
      )
    )
    
    readr::write_csv(
      working_encoding$audit,
      file.path(
        geojapan_audit_dir,
        "geojapan_utf8_encoding_audit.csv"
      )
    )
    
    readr::write_csv(
      taxonomic_audit,
      file.path(
        geojapan_audit_dir,
        "geojapan_taxonomic_summary.csv"
      )
    )
    
    readr::write_csv(
      status_audit,
      file.path(
        geojapan_audit_dir,
        "geojapan_taxonomic_status.csv"
      )
    )
    
    readr::write_csv(
      star_audit,
      file.path(
        geojapan_audit_dir,
        "geojapan_star_by_taxonomic_status.csv"
      )
    )
    
    readr::write_csv(
      rank_audit,
      file.path(
        geojapan_audit_dir,
        "geojapan_rank_structure.csv"
      )
    )
    
    readr::write_csv(
      relationship_audit,
      file.path(
        geojapan_audit_dir,
        "geojapan_relationship_audit.csv"
      )
    )
    
    readr::write_csv(
      identifier_audit,
      file.path(
        geojapan_audit_dir,
        "geojapan_duplicate_spnumbers.csv"
      )
    )
  }
  
  
  # ---------------------------------------------------------------------------
  # Console summary
  # ---------------------------------------------------------------------------
  
  message("")
  message("GEOJAPAN import complete.")
  message(
    "Source records: ",
    format(
      nrow(taxa),
      big.mark = ","
    )
  )
  
  message(
    "Unique SPNUMBER values: ",
    format(
      n_distinct(
        taxa$geojapan_spnumber,
        na.rm = TRUE
      ),
      big.mark = ","
    )
  )
  
  message(
    "Accepted source records: ",
    format(
      sum(
        tolower(
          taxa$geojapan_taxstat
        ) == "acc",
        na.rm = TRUE
      ),
      big.mark = ","
    )
  )
  
  message(
    "SYNOF relationships: ",
    format(
      sum(
        relationships$relationship_type ==
          "SYNOF"
      ),
      big.mark = ","
    )
  )
  
  message(
    "MERGETO relationships: ",
    format(
      sum(
        relationships$relationship_type ==
          "MERGETO"
      ),
      big.mark = ","
    )
  )
  
  message(
    "Working UTF-8 failures: ",
    working_encoding$failures
  )
  
  message(
    "Interim directory: ",
    geojapan_interim_dir
  )
  
  message(
    "Audit directory: ",
    geojapan_audit_dir
  )
  
  
  # ---------------------------------------------------------------------------
  # Return all principal objects
  # ---------------------------------------------------------------------------
  
  invisible(
    list(
      raw = raw,
      working = working,
      taxa = taxa,
      relationships = relationships,
      audits = list(
        source_fields =
          field_audit,
        raw_encoding =
          raw_encoding_audit,
        working_encoding =
          working_encoding$audit,
        taxonomic_summary =
          taxonomic_audit,
        status =
          status_audit,
        stars =
          star_audit,
        ranks =
          rank_audit,
        relationships =
          relationship_audit,
        duplicate_spnumbers =
          identifier_audit
      )
    )
  )
}


# =============================================================================
# 19. END
# =============================================================================

message(
  "02_dbf_import.R v",
  DBF_IMPORT_VERSION,
  " loaded."
)

message(
  "Run import_geojapan_species() to import and audit SPECIES.DBF."
)