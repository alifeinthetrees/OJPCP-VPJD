# ==============================================================================
# VPJD 03i - TRACE LOST WCVP FIELDS UPSTREAM
# ==============================================================================
# Purpose:
#   Identify exactly where the WCVP concept fields associated with the
#   201 contemporary-Japan checklist candidates were lost.
#
# READ ONLY
# ==============================================================================

library(DBI)
library(duckdb)

DB_PATH <-
  "I:/R/OJPCP/VPJD-OJPCP/data/interim/occurrences/vpjd_occurrences.duckdb"

con <-
  DBI::dbConnect(
    duckdb::duckdb(),
    dbdir = DB_PATH,
    read_only = TRUE
  )


# ==============================================================================
# 1. TABLES TO INSPECT
# ==============================================================================

tables_to_check <-
  c(
    "vpjd_taxrev_03d_wcvp_accepted_not_in_vpjd",
    "vpjd_taxrev_03e_wcvp_not_vpjd_audit",
    "vpjd_taxrev_03f_potential_gap_audit",
    "vpjd_taxrev_03g_audit",
    "vpjd_taxrev_03h_audit",
    "vpjd_taxrev_03i_audit"
  )


database_tables <-
  DBI::dbListTables(
    con
  )


cat("\n")
cat("============================================================\n")
cat("UPSTREAM WCVP FIELD TRACE\n")
cat("============================================================\n\n")


# ==============================================================================
# 2. INSPECT EACH TABLE
# ==============================================================================

for (tbl in tables_to_check) {
  
  cat("\n")
  cat("============================================================\n")
  cat("TABLE: ", tbl, "\n", sep = "")
  cat("============================================================\n\n")
  
  if (!(tbl %in% database_tables)) {
    
    cat("TABLE NOT FOUND\n")
    next
    
  }
  
  dat <-
    DBI::dbReadTable(
      con,
      tbl
    )
  
  cat(
    "Records: ",
    format(
      nrow(dat),
      big.mark = ","
    ),
    "\n\n",
    sep = ""
  )
  
  
  # --------------------------------------------------------------------------
  # Identify fields potentially carrying WCVP information
  # --------------------------------------------------------------------------
  
  wcvp_fields <-
    names(dat)[
      grepl(
        paste0(
          "WCVP|",
          "ACCEPTED.*NAME|",
          "TAXON.*ID|",
          "SCIENTIFIC.*NAME|",
          "RECOGNISED.*NAME"
        ),
        names(dat),
        ignore.case = TRUE
      )
    ]
  
  
  if (length(wcvp_fields) == 0L) {
    
    cat("No candidate WCVP/name fields found.\n")
    next
    
  }
  
  
  cat("Candidate fields:\n\n")
  
  for (field in wcvp_fields) {
    
    values <-
      dat[[field]]
    
    values_character <-
      as.character(
        values
      )
    
    populated <-
      sum(
        !is.na(values_character) &
          trimws(values_character) != ""
      )
    
    unique_populated <-
      length(
        unique(
          values_character[
            !is.na(values_character) &
              trimws(values_character) != ""
          ]
        )
      )
    
    
    cat(
      sprintf(
        "  %-45s %6d populated | %6d unique\n",
        field,
        populated,
        unique_populated
      )
    )
  }
  
  
  # --------------------------------------------------------------------------
  # Show first five records for relevant fields
  # --------------------------------------------------------------------------
  
  cat("\nFirst 5 records:\n\n")
  
  preview <-
    dat[
      seq_len(
        min(
          5L,
          nrow(dat)
        )
      ),
      wcvp_fields,
      drop = FALSE
    ]
  
  print(
    preview,
    row.names = FALSE
  )
  
}


# ==============================================================================
# 3. SPECIAL INSPECTION OF 03d WCVP-NOT-VPJD TABLE
# ==============================================================================

tbl <-
  "vpjd_taxrev_03d_wcvp_accepted_not_in_vpjd"


if (tbl %in% database_tables) {
  
  dat <-
    DBI::dbReadTable(
      con,
      tbl
    )
  
  cat("\n")
  cat("============================================================\n")
  cat("03d WCVP-ACCEPTED-NOT-VPJD FIELD INVENTORY\n")
  cat("============================================================\n\n")
  
  for (field in names(dat)) {
    
    values <-
      as.character(
        dat[[field]]
      )
    
    populated <-
      sum(
        !is.na(values) &
          trimws(values) != ""
      )
    
    cat(
      sprintf(
        "%-45s %6d / %6d populated\n",
        field,
        populated,
        nrow(dat)
      )
    )
  }
}


# ==============================================================================
# 4. CHECK WCVP SUPPORT TABLES
# ==============================================================================

support_tables <-
  c(
    "occurrence_wcvp_accepted_taxa",
    "occurrence_wcvp_recognised_taxa",
    "occurrence_wcvp_reconciliation",
    "occurrence_wcvp_resolved_final",
    "wcvp_occurrence_accepted_lookup",
    "wcvp_occurrence_match_index"
  )


cat("\n")
cat("============================================================\n")
cat("WCVP SUPPORT TABLE FIELD INVENTORY\n")
cat("============================================================\n\n")


for (tbl in support_tables) {
  
  cat("\n")
  cat("------------------------------------------------------------\n")
  cat("TABLE: ", tbl, "\n", sep = "")
  cat("------------------------------------------------------------\n\n")
  
  if (!(tbl %in% database_tables)) {
    
    cat("TABLE NOT FOUND\n")
    next
    
  }
  
  
  dat <-
    DBI::dbReadTable(
      con,
      tbl
    )
  
  
  cat(
    "Records: ",
    format(
      nrow(dat),
      big.mark = ","
    ),
    "\n\n",
    sep = ""
  )
  
  
  for (field in names(dat)) {
    
    if (
      grepl(
        "WCVP|ACCEPT|TAXON|NAME|STATUS|FAMILY|GENUS|RANK",
        field,
        ignore.case = TRUE
      )
    ) {
      
      cat(
        field,
        "\n"
      )
    }
  }
}


# ==============================================================================
# 5. CLEAN DISCONNECT
# ==============================================================================

DBI::dbDisconnect(
  con,
  shutdown = TRUE
)

rm(con)


cat("\n")
cat("============================================================\n")
cat("WCVP FIELD TRACE COMPLETE\n")
cat("============================================================\n")