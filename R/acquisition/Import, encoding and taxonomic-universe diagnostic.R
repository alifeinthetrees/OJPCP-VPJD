# ============================================================
# GEOJAPAN SPECIES.DBF
# Import, encoding and taxonomic-universe diagnostic
#
# Purpose:
#   1. Import SPECIES.DBF without modifying the source file.
#   2. Preserve the raw imported table.
#   3. Diagnose legacy character encoding.
#   4. Create a UTF-8 working copy.
#   5. Examine the taxonomic structure of the database.
#
# No files are written or overwritten by this script.
# ============================================================


# ============================================================
# 0. PACKAGES
# ============================================================

library(here)
library(foreign)
library(dplyr)


# ============================================================
# 1. SOURCE FILE
# ============================================================

species_path <- here::here(
  "data",
  "raw",
  "external",
  "GEOJAPAN",
  "SPECIES.DBF"
)

if (!file.exists(species_path)) {
  stop(
    "SPECIES.DBF not found at: ",
    species_path
  )
}

cat("\n============================================================\n")
cat("GEOJAPAN SPECIES.DBF diagnostic\n")
cat("============================================================\n")

cat("\nSource file:\n")
cat(species_path, "\n")


# ============================================================
# 2. IMPORT RAW DBF
# ============================================================

# IMPORTANT:
# This is the raw imported representation.
# Do not modify species_dbf_raw later in the script.

species_dbf_raw <- foreign::read.dbf(
  species_path,
  as.is = TRUE
)

cat("\n--- RAW IMPORT ---\n")

cat(
  "Rows:    ",
  nrow(species_dbf_raw),
  "\n",
  sep = ""
)

cat(
  "Columns: ",
  ncol(species_dbf_raw),
  "\n",
  sep = ""
)


# ============================================================
# 3. BASIC FIELD STRUCTURE
# ============================================================

cat("\n--- KEY FIELD PRESENCE ---\n")

key_fields <- c(
  "SPNUMBER",
  "GECODE",
  "MERGETO",
  "SYNOF",
  "TAXSTAT",
  "ALTTAXSTAT",
  "GENHYBRID",
  "SPHYBRID",
  "HYBRID",
  "CULTIVAR",
  "SPQUICK",
  "SP1",
  "RANK1",
  "SP2",
  "RANK2",
  "SP3",
  "DISTRIB",
  "STAR",
  "STARNOTE",
  "SPSCORE",
  "IUCN",
  "TDWG",
  "FULLNAME",
  "SPECTOTAL",
  "GENUS",
  "FAMILY"
)

key_field_check <- data.frame(
  field = key_fields,
  present = key_fields %in% names(species_dbf_raw),
  stringsAsFactors = FALSE
)

key_field_check


# ============================================================
# 4. RAW CHARACTER ENCODING DIAGNOSTIC
# ============================================================

character_fields <- names(species_dbf_raw)[
  vapply(
    species_dbf_raw,
    is.character,
    logical(1)
  )
]

encoding_diagnostic_list <- lapply(
  character_fields,
  function(nm) {
    
    x <- species_dbf_raw[[nm]]
    
    nonmissing <- !is.na(x)
    
    utf8_test <- iconv(
      x,
      from = "UTF-8",
      to = "UTF-8",
      sub = NA
    )
    
    invalid <- (
      nonmissing &
        is.na(utf8_test)
    )
    
    data.frame(
      field = nm,
      nonmissing_values = sum(
        nonmissing,
        na.rm = TRUE
      ),
      invalid_utf8 = sum(
        invalid,
        na.rm = TRUE
      ),
      stringsAsFactors = FALSE
    )
  }
)

encoding_diagnostic <- bind_rows(
  encoding_diagnostic_list
) %>%
  arrange(desc(invalid_utf8))

cat("\n--- RAW ENCODING DIAGNOSTIC ---\n")

encoding_problem_fields <- encoding_diagnostic %>%
  filter(invalid_utf8 > 0)

if (nrow(encoding_problem_fields) == 0) {
  
  cat(
    "No invalid UTF-8 values detected in raw character fields.\n"
  )
  
} else {
  
  as.data.frame(
    encoding_problem_fields
  )
}

raw_invalid_utf8 <- sum(
  encoding_diagnostic$invalid_utf8,
  na.rm = TRUE
)

cat(
  "\nTotal invalid UTF-8 values in raw import: ",
  raw_invalid_utf8,
  "\n",
  sep = ""
)


# ============================================================
# 5. CREATE UTF-8 WORKING COPY
# ============================================================

# The raw DBF contains legacy Western-European encoded
# characters. We create a separate working representation.
#
# species_dbf_raw remains unchanged.
#
# CP1252 is currently treated as the candidate source encoding.
# The validation below checks the resulting working strings.

species_dbf <- species_dbf_raw %>%
  mutate(
    across(
      where(is.character),
      ~ iconv(
        .x,
        from = "CP1252",
        to = "UTF-8",
        sub = NA
      )
    )
  )


# ============================================================
# 6. VALIDATE UTF-8 WORKING COPY
# ============================================================

working_character_fields <- names(species_dbf)[
  vapply(
    species_dbf,
    is.character,
    logical(1)
  )
]

working_encoding_list <- lapply(
  working_character_fields,
  function(nm) {
    
    x <- species_dbf[[nm]]
    
    nonmissing <- !is.na(x)
    
    utf8_test <- iconv(
      x,
      from = "UTF-8",
      to = "UTF-8",
      sub = NA
    )
    
    invalid <- (
      nonmissing &
        is.na(utf8_test)
    )
    
    data.frame(
      field = nm,
      nonmissing_values = sum(
        nonmissing,
        na.rm = TRUE
      ),
      invalid_utf8 = sum(
        invalid,
        na.rm = TRUE
      ),
      stringsAsFactors = FALSE
    )
  }
)

working_encoding_diagnostic <- bind_rows(
  working_encoding_list
) %>%
  arrange(desc(invalid_utf8))

encoding_failures <- sum(
  working_encoding_diagnostic$invalid_utf8,
  na.rm = TRUE
)

cat("\n--- UTF-8 WORKING COPY ---\n")

cat(
  "Remaining invalid UTF-8 values: ",
  encoding_failures,
  "\n",
  sep = ""
)

if (encoding_failures > 0) {
  
  cat(
    "\nFields still containing invalid UTF-8:\n"
  )
  
  as.data.frame(
    working_encoding_diagnostic %>%
      filter(invalid_utf8 > 0)
  )
}


# ============================================================
# 7. BASIC TAXONOMIC COUNTS
# ============================================================

cat("\n--- BASIC COUNTS ---\n")

basic_counts <- species_dbf %>%
  summarise(
    
    records = n(),
    
    unique_spnumber =
      n_distinct(
        SPNUMBER,
        na.rm = TRUE
      ),
    
    named_records =
      sum(
        !is.na(FULLNAME) &
          nzchar(trimws(FULLNAME)),
        na.rm = TRUE
      ),
    
    unique_names =
      n_distinct(
        FULLNAME[
          !is.na(FULLNAME) &
            nzchar(trimws(FULLNAME))
        ]
      ),
    
    synonym_links =
      sum(
        !is.na(SYNOF)
      ),
    
    merge_links =
      sum(
        !is.na(MERGETO)
      ),
    
    star_records =
      sum(
        !is.na(STAR) &
          nzchar(trimws(STAR)),
        na.rm = TRUE
      )
  )

as.data.frame(
  basic_counts
)


# ============================================================
# 8. TAXONOMIC STATUS
# ============================================================

cat("\n--- TAXONOMIC STATUS ---\n")

taxonomic_status <- species_dbf %>%
  count(
    TAXSTAT,
    sort = TRUE
  )

as.data.frame(
  taxonomic_status
)


# ============================================================
# 9. ALTERNATIVE TAXONOMIC STATUS
# ============================================================

cat("\n--- ALTERNATIVE TAXONOMIC STATUS ---\n")

alternative_status <- species_dbf %>%
  count(
    ALTTAXSTAT,
    sort = TRUE
  )

as.data.frame(
  alternative_status
)


# ============================================================
# 10. STAR VALUES
# ============================================================

cat("\n--- STAR VALUES ---\n")

star_values <- species_dbf %>%
  count(
    STAR,
    sort = TRUE
  )

as.data.frame(
  star_values
)


# ============================================================
# 11. RANK 1
# ============================================================

cat("\n--- RANK 1 ---\n")

rank1_values <- species_dbf %>%
  count(
    RANK1,
    sort = TRUE
  )

as.data.frame(
  rank1_values
)


# ============================================================
# 12. RANK 2
# ============================================================

cat("\n--- RANK 2 ---\n")

rank2_values <- species_dbf %>%
  count(
    RANK2,
    sort = TRUE
  )

as.data.frame(
  rank2_values
)


# ============================================================
# 13. HYBRID / CULTIVAR FLAGS
# ============================================================

cat("\n--- HYBRID / CULTIVAR FLAGS ---\n")

hybrid_cultivar_counts <- species_dbf %>%
  summarise(
    
    genhybrid =
      sum(
        !is.na(GENHYBRID) &
          nzchar(trimws(GENHYBRID)),
        na.rm = TRUE
      ),
    
    sphybrid =
      sum(
        !is.na(SPHYBRID) &
          nzchar(trimws(SPHYBRID)),
        na.rm = TRUE
      ),
    
    hybrid =
      sum(
        !is.na(HYBRID) &
          nzchar(trimws(HYBRID)),
        na.rm = TRUE
      ),
    
    cultivar =
      sum(
        !is.na(CULTIVAR) &
          nzchar(trimws(CULTIVAR)),
        na.rm = TRUE
      )
  )

as.data.frame(
  hybrid_cultivar_counts
)


# ============================================================
# 14. SPECTOTAL
# ============================================================

cat("\n--- SPECTOTAL ---\n")

spectotal_values <- species_dbf$SPECTOTAL[
  !is.na(species_dbf$SPECTOTAL)
]

if (length(spectotal_values) > 0) {
  
  spectotal_summary <- data.frame(
    nonmissing = length(spectotal_values),
    min = min(spectotal_values),
    max = max(spectotal_values)
  )
  
  spectotal_summary
  
} else {
  
  cat(
    "No non-missing SPECTOTAL values.\n"
  )
}


# ============================================================
# 15. SPNUMBER INTEGRITY
# ============================================================

cat("\n--- SPNUMBER INTEGRITY ---\n")

unique_spnumber_count <- n_distinct(
  species_dbf$SPNUMBER,
  na.rm = TRUE
)

spnumber_duplicates <- species_dbf %>%
  filter(
    !is.na(SPNUMBER)
  ) %>%
  count(
    SPNUMBER,
    sort = TRUE
  ) %>%
  filter(
    n > 1
  )

cat(
  "Unique non-missing SPNUMBER values: ",
  unique_spnumber_count,
  "\n",
  sep = ""
)

cat(
  "Duplicated SPNUMBER identifiers: ",
  nrow(spnumber_duplicates),
  "\n",
  sep = ""
)

if (nrow(spnumber_duplicates) > 0) {
  
  cat(
    "\nFirst 50 duplicated identifiers:\n"
  )
  
  head(
    as.data.frame(spnumber_duplicates),
    50
  )
}


# ============================================================
# 16. SPNUMBER RANGE
# ============================================================

cat("\n--- SPNUMBER RANGE ---\n")

valid_spnumbers <- species_dbf$SPNUMBER[
  !is.na(species_dbf$SPNUMBER)
]

spnumber_range <- data.frame(
  minimum = min(valid_spnumbers),
  maximum = max(valid_spnumbers),
  negative = sum(valid_spnumbers < 0),
  zero = sum(valid_spnumbers == 0),
  positive = sum(valid_spnumbers > 0)
)

spnumber_range


# ============================================================
# 17. SYNONYM RELATIONSHIPS
# ============================================================

cat("\n--- SYNONYM RELATIONSHIPS ---\n")

synonym_records <- species_dbf %>%
  filter(
    !is.na(SYNOF)
  )

cat(
  "Records with SYNOF: ",
  nrow(synonym_records),
  "\n",
  sep = ""
)

cat(
  "Unique SYNOF targets: ",
  n_distinct(
    synonym_records$SYNOF,
    na.rm = TRUE
  ),
  "\n",
  sep = ""
)


# ============================================================
# 18. EXAMPLE SYNONYM LINKS
# ============================================================

cat("\n--- EXAMPLE SYNONYM LINKS ---\n")

example_synonyms <- species_dbf %>%
  filter(
    !is.na(SYNOF)
  ) %>%
  select(
    SPNUMBER,
    FULLNAME,
    TAXSTAT,
    SYNOF,
    STAR
  ) %>%
  head(20)

as.data.frame(
  example_synonyms
)


# ============================================================
# 19. RESOLVE SYNONYM TARGET NAMES
# ============================================================

cat("\n--- EXAMPLE SYNONYM → TARGET RELATIONSHIPS ---\n")

synonym_target_lookup <- species_dbf %>%
  select(
    target_spnumber = SPNUMBER,
    target_fullname = FULLNAME,
    target_taxstat = TAXSTAT,
    target_star = STAR
  )

synonym_relationships <- species_dbf %>%
  filter(
    !is.na(SYNOF)
  ) %>%
  select(
    source_spnumber = SPNUMBER,
    source_fullname = FULLNAME,
    source_taxstat = TAXSTAT,
    source_star = STAR,
    target_spnumber = SYNOF
  ) %>%
  left_join(
    synonym_target_lookup,
    by = "target_spnumber"
  )

head(
  as.data.frame(
    synonym_relationships
  ),
  20
)


# ============================================================
# 20. SYNONYM TARGET INTEGRITY
# ============================================================

cat("\n--- SYNONYM TARGET INTEGRITY ---\n")

synonym_target_integrity <- synonym_relationships %>%
  summarise(
    
    synonym_records = n(),
    
    target_found =
      sum(
        !is.na(target_fullname),
        na.rm = TRUE
      ),
    
    target_not_found =
      sum(
        is.na(target_fullname)
      )
  )

as.data.frame(
  synonym_target_integrity
)


# ============================================================
# 21. TAXONOMIC STATUS × SYNONYM LINK
# ============================================================

cat("\n--- TAXONOMIC STATUS × SYNOF ---\n")

taxstat_synof <- species_dbf %>%
  mutate(
    has_synof = !is.na(SYNOF)
  ) %>%
  count(
    TAXSTAT,
    has_synof,
    sort = TRUE
  )

as.data.frame(
  taxstat_synof
)


# ============================================================
# 22. TAXONOMIC STATUS × STAR
# ============================================================

cat("\n--- TAXONOMIC STATUS × STAR ---\n")

taxstat_star <- species_dbf %>%
  count(
    TAXSTAT,
    STAR,
    sort = TRUE
  )

as.data.frame(
  taxstat_star
)


# ============================================================
# 23. ACCEPTED TAXA
# ============================================================

cat("\n--- ACCEPTED TAXA ---\n")

accepted_taxa <- species_dbf %>%
  filter(
    !is.na(TAXSTAT),
    tolower(trimws(TAXSTAT)) == "acc"
  )

accepted_summary <- accepted_taxa %>%
  summarise(
    
    records = n(),
    
    unique_spnumber =
      n_distinct(
        SPNUMBER,
        na.rm = TRUE
      ),
    
    named_records =
      sum(
        !is.na(FULLNAME) &
          nzchar(trimws(FULLNAME)),
        na.rm = TRUE
      ),
    
    star_records =
      sum(
        !is.na(STAR) &
          nzchar(trimws(STAR)),
        na.rm = TRUE
      )
  )

as.data.frame(
  accepted_summary
)


# ============================================================
# 24. ACCEPTED TAXA BY RANK
# ============================================================

cat("\n--- ACCEPTED TAXA: RANK STRUCTURE ---\n")

accepted_rank_structure <- accepted_taxa %>%
  count(
    RANK1,
    RANK2,
    sort = TRUE
  )

as.data.frame(
  accepted_rank_structure
)


# ============================================================
# 25. FULLNAME UNIQUENESS
# ============================================================

cat("\n--- DUPLICATED FULL NAMES ---\n")

duplicate_fullnames <- species_dbf %>%
  filter(
    !is.na(FULLNAME),
    nzchar(trimws(FULLNAME))
  ) %>%
  count(
    FULLNAME,
    sort = TRUE
  ) %>%
  filter(
    n > 1
  )

cat(
  "Duplicated FULLNAME strings: ",
  nrow(duplicate_fullnames),
  "\n",
  sep = ""
)

if (nrow(duplicate_fullnames) > 0) {
  
  head(
    as.data.frame(
      duplicate_fullnames
    ),
    50
  )
}


# ============================================================
# 26. SOURCE STAR COVERAGE
# ============================================================

cat("\n--- STAR COVERAGE BY TAXONOMIC STATUS ---\n")

star_coverage <- species_dbf %>%
  mutate(
    has_star =
      !is.na(STAR) &
      nzchar(trimws(STAR))
  ) %>%
  count(
    TAXSTAT,
    has_star,
    sort = TRUE
  )

as.data.frame(
  star_coverage
)


# ============================================================
# 27. FINAL SUMMARY
# ============================================================

cat("\n============================================================\n")
cat("GEOJAPAN SPECIES.DBF diagnostic complete\n")
cat("============================================================\n")

cat(
  "Raw source records:        ",
  nrow(species_dbf_raw),
  "\n",
  sep = ""
)

cat(
  "Raw source columns:        ",
  ncol(species_dbf_raw),
  "\n",
  sep = ""
)

cat(
  "Unique SPNUMBER values:    ",
  unique_spnumber_count,
  "\n",
  sep = ""
)

cat(
  "Raw invalid UTF-8 values:  ",
  raw_invalid_utf8,
  "\n",
  sep = ""
)

cat(
  "Working encoding failures: ",
  encoding_failures,
  "\n",
  sep = ""
)

cat(
  "Accepted taxon records:    ",
  nrow(accepted_taxa),
  "\n",
  sep = ""
)

cat(
  "Synonym-linked records:    ",
  nrow(synonym_records),
  "\n",
  sep = ""
)

cat(
  "Duplicated SPNUMBERs:      ",
  nrow(spnumber_duplicates),
  "\n",
  sep = ""
)

cat("\nObjects retained in R:\n")
cat("  species_dbf_raw        = untouched raw DBF import\n")
cat("  species_dbf            = UTF-8 working representation\n")
cat("  accepted_taxa          = TAXSTAT == 'acc'\n")
cat("  synonym_relationships  = resolved SYNOF relationships\n")

cat("\n============================================================\n")