# ==============================================================================
# 03b - IDENTIFY FAILED VALIDATION CHECK
# ==============================================================================

library(DBI)
library(duckdb)

DB_PATH <- paste0(
  "I:/R/OJPCP/VPJD-OJPCP/",
  "data/interim/occurrences/vpjd_occurrences.duckdb"
)

con <- DBI::dbConnect(
  duckdb::duckdb(),
  dbdir = DB_PATH,
  read_only = TRUE
)

validation <- DBI::dbReadTable(
  con,
  "vpjd_taxrev_03b_validation"
)

cat("\n")
cat("============================================================\n")
cat("03b FAILED VALIDATION CHECK(S)\n")
cat("============================================================\n\n")

# Avoid tibble/dplyr printing entirely
failed <- validation[
  validation$PASS %in% FALSE,
  ,
  drop = FALSE
]

if (nrow(failed) == 0L) {
  
  cat("No failed validation checks found.\n")
  
} else {
  
  for (i in seq_len(nrow(failed))) {
    
    cat(
      "CHECK: ",
      as.character(failed$CHECK[i]),
      "\n",
      sep = ""
    )
    
    cat(
      "PASS: ",
      as.character(failed$PASS[i]),
      "\n",
      sep = ""
    )
    
    if ("RESULT" %in% names(failed)) {
      
      cat(
        "RESULT: ",
        as.character(failed$RESULT[i]),
        "\n",
        sep = ""
      )
    }
    
    cat("\n")
  }
}

cat("------------------------------------------------------------\n")
cat("GREENLIST FIELD NAMES\n")
cat("------------------------------------------------------------\n\n")

green <- DBI::dbReadTable(
  con,
  "vpjd_taxrev_03a_greenlist_core"
)

for (x in names(green)) {
  cat(x, "\n")
}

cat("\n")
cat("------------------------------------------------------------\n")
cat("POTENTIAL GREENLIST TAXONOMIC NAME FIELDS\n")
cat("------------------------------------------------------------\n\n")

candidate_fields <- names(green)[
  grepl(
    "genus|generic|scientific|canonical|name",
    names(green),
    ignore.case = TRUE
  )
]

for (x in candidate_fields) {
  cat(x, "\n")
}

cat("\n")
cat("------------------------------------------------------------\n")
cat("VALUES FROM CANDIDATE FIELDS - FIRST 10 RECORDS\n")
cat("------------------------------------------------------------\n\n")

for (field in candidate_fields) {
  
  cat("\nFIELD: ", field, "\n", sep = "")
  cat("----------------------------------------\n")
  
  values <- as.character(
    green[[field]]
  )
  
  for (i in seq_len(min(10L, length(values)))) {
    
    cat(
      i,
      ": ",
      ifelse(
        is.na(values[i]),
        "<NA>",
        values[i]
      ),
      "\n",
      sep = ""
    )
  }
}

DBI::dbDisconnect(
  con,
  shutdown = TRUE
)

rm(con)

cat("\n============================================================\n")
cat("DIAGNOSTIC COMPLETE\n")
cat("============================================================\n")