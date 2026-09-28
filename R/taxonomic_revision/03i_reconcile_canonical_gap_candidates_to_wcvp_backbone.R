# ==============================================================================
# 03i SINGLE UNRESOLVED WCVP CONCEPT DIAGNOSTIC
# ==============================================================================
#
# PURPOSE
# -------
# Identify the 03i record with a retained WCVP ID but no resolved accepted
# WCVP name, and trace that record through 03f -> 03g -> 03h -> 03i.
#
# The diagnostic also searches WCVP-related DuckDB tables for the target
# WCVP ID so that the authoritative Kew/WCVP concept can be inspected.
#
# READ ONLY
# ---------
# No DuckDB tables are modified.
# No CSV files are written.
# Canonical VPJD taxonomy is not modified.
# Star allocations are not modified.
#
# ==============================================================================


# ==============================================================================
# 1. PACKAGES
# ==============================================================================

library(DBI)
library(duckdb)
library(dplyr)
library(tibble)


# ==============================================================================
# 2. PATHS
# ==============================================================================

PROJECT_ROOT <-
  "I:/R/OJPCP/VPJD-OJPCP"


DB_PATH <-
  file.path(
    PROJECT_ROOT,
    "data/interim/occurrences/vpjd_occurrences.duckdb"
  )


stopifnot(
  file.exists(DB_PATH)
)


# ==============================================================================
# 3. CONNECT READ-ONLY
# ==============================================================================

con <-
  DBI::dbConnect(
    duckdb::duckdb(),
    dbdir = DB_PATH,
    read_only = TRUE
  )


disconnect_safely <-
  function() {
    
    if (
      exists(
        "con",
        inherits = TRUE
      )
    ) {
      
      try(
        DBI::dbDisconnect(
          con,
          shutdown = TRUE
        ),
        silent = TRUE
      )
    }
  }


on.exit(
  disconnect_safely(),
  add = TRUE
)


# ==============================================================================
# 4. DATABASE INVENTORY
# ==============================================================================

tables <-
  DBI::dbListTables(
    con
  )


cat("\n")

cat("============================================================\n")
cat("03i SINGLE UNRESOLVED WCVP CONCEPT DIAGNOSTIC\n")
cat("============================================================\n\n")


cat(
  "Database:\n",
  DB_PATH,
  "\n\n",
  sep = ""
)


# ==============================================================================
# 5. IDENTIFY REQUIRED AUDIT TABLES
# ==============================================================================

find_first_table <-
  function(
    candidates
  ) {
    
    hit <-
      candidates[
        candidates %in%
          tables
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


TABLE_03F <-
  find_first_table(
    c(
      "vpjd_taxrev_03f_potential_gap_audit",
      "vpjd_taxrev_03f_audit"
    )
  )


TABLE_03G <-
  find_first_table(
    c(
      "vpjd_taxrev_03g_audit"
    )
  )


TABLE_03H <-
  find_first_table(
    c(
      "vpjd_taxrev_03h_adjudication_audit",
      "vpjd_taxrev_03h_audit"
    )
  )


TABLE_03I <-
  find_first_table(
    c(
      "vpjd_taxrev_03i_audit"
    )
  )


required_table_map <-
  c(
    "03f" = TABLE_03F,
    "03g" = TABLE_03G,
    "03h" = TABLE_03H,
    "03i" = TABLE_03I
  )


cat("Audit tables identified:\n\n")


for (
  stage in names(
    required_table_map
  )
) {
  
  cat(
    stage,
    ": ",
    required_table_map[[stage]],
    "\n",
    sep = ""
  )
}


if (
  any(
    is.na(
      required_table_map
    )
  )
) {
  
  missing_stages <-
    names(
      required_table_map
    )[
      is.na(
        required_table_map
      )
    ]
  
  disconnect_safely()
  
  stop(
    paste0(
      "\nRequired audit table(s) could not be identified for:\n",
      paste(
        missing_stages,
        collapse = "\n"
      )
    )
  )
}


# ==============================================================================
# 6. LOAD 03f -> 03i
# ==============================================================================

audit_03f <-
  DBI::dbReadTable(
    con,
    TABLE_03F
  ) |>
  tibble::as_tibble()


audit_03g <-
  DBI::dbReadTable(
    con,
    TABLE_03G
  ) |>
  tibble::as_tibble()


audit_03h <-
  DBI::dbReadTable(
    con,
    TABLE_03H
  ) |>
  tibble::as_tibble()


audit_03i <-
  DBI::dbReadTable(
    con,
    TABLE_03I
  ) |>
  tibble::as_tibble()


cat("\n")

cat("------------------------------------------------------------\n")
cat("AUDIT POPULATIONS\n")
cat("------------------------------------------------------------\n\n")


cat(
  "03f records: ",
  format(
    nrow(audit_03f),
    big.mark = ","
  ),
  "\n",
  sep = ""
)


cat(
  "03g records: ",
  format(
    nrow(audit_03g),
    big.mark = ","
  ),
  "\n",
  sep = ""
)


cat(
  "03h records: ",
  format(
    nrow(audit_03h),
    big.mark = ","
  ),
  "\n",
  sep = ""
)


cat(
  "03i records: ",
  format(
    nrow(audit_03i),
    big.mark = ","
  ),
  "\n\n",
  sep = ""
)


# ==============================================================================
# 7. VALIDATE CRITICAL 03i FIELDS
# ==============================================================================

required_03i_fields <-
  c(
    "JAPAN_RECORD_ID",
    "JAPAN_SCIENTIFIC_NAME",
    "WCVP_ID",
    "WCVP_ACCEPTED_NAME"
  )


missing_03i_fields <-
  setdiff(
    required_03i_fields,
    names(audit_03i)
  )


if (
  length(
    missing_03i_fields
  ) > 0L
) {
  
  disconnect_safely()
  
  stop(
    paste0(
      "Required 03i field(s) missing:\n",
      paste(
        missing_03i_fields,
        collapse = "\n"
      )
    )
  )
}


# ==============================================================================
# 8. IDENTIFY UNRESOLVED 03i RECORD
# ==============================================================================

accepted_name_missing <-
  is.na(
    audit_03i$WCVP_ACCEPTED_NAME
  ) |
  trimws(
    as.character(
      audit_03i$WCVP_ACCEPTED_NAME
    )
  ) == ""


status_review <-
  if (
    "WCVP_BACKBONE_CLASS" %in%
    names(audit_03i)
  ) {
    
    as.character(
      audit_03i$WCVP_BACKBONE_CLASS
    ) ==
      "WCVP_STATUS_REVIEW_REQUIRED"
    
  } else {
    
    rep(
      FALSE,
      nrow(audit_03i)
    )
  }


unresolved_concept <-
  if (
    "VPJD_GAP_STATUS" %in%
    names(audit_03i)
  ) {
    
    as.character(
      audit_03i$VPJD_GAP_STATUS
    ) ==
      "UNRESOLVED_WCVP_CONCEPT"
    
  } else {
    
    rep(
      FALSE,
      nrow(audit_03i)
    )
  }


unresolved_03i <-
  audit_03i[
    accepted_name_missing |
      status_review |
      unresolved_concept,
    ,
    drop = FALSE
  ] |>
  tibble::as_tibble()


cat("------------------------------------------------------------\n")
cat("UNRESOLVED 03i RECORD(S)\n")
cat("------------------------------------------------------------\n\n")


cat(
  "Records identified: ",
  nrow(unresolved_03i),
  "\n\n",
  sep = ""
)


if (
  nrow(unresolved_03i) == 0L
) {
  
  disconnect_safely()
  
  stop(
    "No unresolved 03i record was found."
  )
}


print(
  as.data.frame(
    unresolved_03i
  ),
  row.names = FALSE,
  na.print = "NA"
)


# ==============================================================================
# 9. IDENTIFY TARGET JAPAN RECORD ID
# ==============================================================================

target_ids <-
  unique(
    as.character(
      unresolved_03i$JAPAN_RECORD_ID
    )
  )


target_ids <-
  target_ids[
    !is.na(target_ids) &
      target_ids != ""
  ]


if (
  length(target_ids) == 0L
) {
  
  disconnect_safely()
  
  stop(
    "The unresolved record has no usable JAPAN_RECORD_ID."
  )
}


cat("\n")

cat("============================================================\n")
cat("TARGET JAPAN_RECORD ID\n")
cat("============================================================\n\n")


print(
  target_ids
)


# ==============================================================================
# 10. TRACE FUNCTION
# ==============================================================================

trace_by_japan_record <-
  function(
    data,
    ids
  ) {
    
    if (
      !"JAPAN_RECORD_ID" %in%
      names(data)
    ) {
      
      return(
        tibble()
      )
    }
    
    data[
      as.character(
        data$JAPAN_RECORD_ID
      ) %in%
        ids,
      ,
      drop = FALSE
    ] |>
      tibble::as_tibble()
  }


# ==============================================================================
# 11. TRACE THROUGH 03f
# ==============================================================================

trace_03f <-
  trace_by_japan_record(
    audit_03f,
    target_ids
  )


cat("\n")

cat("============================================================\n")
cat("03f RECORD\n")
cat("============================================================\n\n")


print(
  as.data.frame(
    trace_03f
  ),
  row.names = FALSE,
  na.print = "NA"
)


# ==============================================================================
# 12. TRACE THROUGH 03g
# ==============================================================================

trace_03g <-
  trace_by_japan_record(
    audit_03g,
    target_ids
  )


cat("\n")

cat("============================================================\n")
cat("03g RECORD\n")
cat("============================================================\n\n")


print(
  as.data.frame(
    trace_03g
  ),
  row.names = FALSE,
  na.print = "NA"
)


# ==============================================================================
# 13. TRACE THROUGH 03h
# ==============================================================================

trace_03h <-
  trace_by_japan_record(
    audit_03h,
    target_ids
  )


cat("\n")

cat("============================================================\n")
cat("03h RECORD\n")
cat("============================================================\n\n")


print(
  as.data.frame(
    trace_03h
  ),
  row.names = FALSE,
  na.print = "NA"
)


# ==============================================================================
# 14. TRACE THROUGH 03i
# ==============================================================================

trace_03i <-
  trace_by_japan_record(
    audit_03i,
    target_ids
  )


cat("\n")

cat("============================================================\n")
cat("03i RECORD\n")
cat("============================================================\n\n")


print(
  as.data.frame(
    trace_03i
  ),
  row.names = FALSE,
  na.print = "NA"
)


# ==============================================================================
# 15. COMPACT CROSS-STAGE WCVP HAND-OFF
# ==============================================================================

extract_stage <-
  function(
    data,
    stage
  ) {
    
    if (
      !"JAPAN_RECORD_ID" %in%
      names(data)
    ) {
      
      return(
        tibble()
      )
    }
    
    
    stage_data <-
      data[
        as.character(
          data$JAPAN_RECORD_ID
        ) %in%
          target_ids,
        ,
        drop = FALSE
      ] |>
      tibble::as_tibble()
    
    
    fields <-
      c(
        "JAPAN_RECORD_ID",
        "JAPAN_SCIENTIFIC_NAME",
        "JAPANESE_NAME",
        "WCVP_ID",
        "WCVP_ACCEPTED_NAME",
        "WCVP_RANK",
        "WCVP_FAMILY",
        "WCVP_GENUS",
        "WCVP_STATUS"
      )
    
    
    fields <-
      fields[
        fields %in%
          names(stage_data)
      ]
    
    
    stage_data |>
      select(
        all_of(
          fields
        )
      ) |>
      mutate(
        STAGE =
          stage,
        .before = 1
      )
  }


cross_stage <-
  bind_rows(
    extract_stage(
      audit_03f,
      "03f"
    ),
    extract_stage(
      audit_03g,
      "03g"
    ),
    extract_stage(
      audit_03h,
      "03h"
    ),
    extract_stage(
      audit_03i,
      "03i"
    )
  )


cat("\n")

cat("============================================================\n")
cat("CROSS-STAGE WCVP HAND-OFF\n")
cat("============================================================\n\n")


print(
  as.data.frame(
    cross_stage
  ),
  row.names = FALSE,
  na.print = "NA"
)


# ==============================================================================
# 16. INSPECT ALL RELEVANT 03f WCVP RESOLUTION FIELDS
# ==============================================================================

diagnostic_fields_03f <-
  names(trace_03f)[
    grepl(
      "WCVP|SCIENTIFIC|JAPAN|RESOLUTION|MATCH|STATUS",
      names(trace_03f),
      ignore.case = TRUE
    )
  ]


cat("\n")

cat("============================================================\n")
cat("03f WCVP RESOLUTION FIELDS\n")
cat("============================================================\n\n")


if (
  nrow(trace_03f) == 0L
) {
  
  cat(
    "No corresponding 03f record was found.\n"
  )
  
} else {
  
  print(
    as.data.frame(
      trace_03f |>
        select(
          any_of(
            diagnostic_fields_03f
          )
        )
    ),
    row.names = FALSE,
    na.print = "NA"
  )
}


# ==============================================================================
# 17. IDENTIFY TARGET WCVP ID
# ==============================================================================

target_wcvp_ids <-
  unique(
    as.character(
      unresolved_03i$WCVP_ID
    )
  )


target_wcvp_ids <-
  target_wcvp_ids[
    !is.na(target_wcvp_ids) &
      target_wcvp_ids != ""
  ]


cat("\n")

cat("============================================================\n")
cat("TARGET WCVP ID\n")
cat("============================================================\n\n")


print(
  target_wcvp_ids
)


if (
  length(target_wcvp_ids) == 0L
) {
  
  disconnect_safely()
  
  stop(
    paste0(
      "The unresolved 03i record has no WCVP ID. ",
      "The WCVP-table search cannot continue."
    )
  )
}


# ==============================================================================
# 18. IDENTIFY WCVP-RELATED TABLES
# ==============================================================================

candidate_wcvp_tables <-
  tables[
    grepl(
      "wcvp",
      tables,
      ignore.case = TRUE
    )
  ]


cat("\n")

cat("------------------------------------------------------------\n")
cat("WCVP TABLE SEARCH\n")
cat("------------------------------------------------------------\n\n")


cat(
  "WCVP-related tables to inspect: ",
  length(
    candidate_wcvp_tables
  ),
  "\n",
  sep = ""
)


cat(
  "Target WCVP ID(s): ",
  paste(
    target_wcvp_ids,
    collapse = " | "
  ),
  "\n\n",
  sep = ""
)


# ==============================================================================
# 19. SEARCH WCVP TABLES FOR TARGET ID
# ==============================================================================

wcvp_hits <-
  list()


for (
  tbl in candidate_wcvp_tables
) {
  
  fields <-
    DBI::dbListFields(
      con,
      tbl
    )
  
  
  id_fields <-
    fields[
      grepl(
        "WCVP.*ID|PLANT.*ID|TAXON.*ID",
        fields,
        ignore.case = TRUE
      )
    ]
  
  
  if (
    length(id_fields) == 0L
  ) {
    
    next
  }
  
  
  for (
    field in id_fields
  ) {
    
    quoted_table <-
      as.character(
        DBI::dbQuoteIdentifier(
          con,
          tbl
        )
      )
    
    
    quoted_field <-
      as.character(
        DBI::dbQuoteIdentifier(
          con,
          field
        )
      )
    
    
    target_sql <-
      paste(
        DBI::dbQuoteString(
          con,
          target_wcvp_ids
        ),
        collapse = ", "
      )
    
    
    sql <-
      paste0(
        "SELECT * FROM ",
        quoted_table,
        " WHERE CAST(",
        quoted_field,
        " AS VARCHAR) IN (",
        target_sql,
        ")"
      )
    
    
    result <-
      tryCatch(
        DBI::dbGetQuery(
          con,
          sql
        ),
        error =
          function(e) {
            
            NULL
          }
      )
    
    
    if (
      is.null(result) ||
      nrow(result) == 0L
    ) {
      
      next
    }
    
    
    result <-
      result |>
      tibble::as_tibble() |>
      mutate(
        SOURCE_TABLE =
          tbl,
        MATCH_FIELD =
          field,
        .before = 1
      )
    
    
    wcvp_hits[[length(wcvp_hits) + 1L]] <-
      result
  }
}


# ==============================================================================
# 20. REPORT WCVP DATABASE HITS
# ==============================================================================

cat("\n")

cat("============================================================\n")
cat("DATABASE RECORDS MATCHING TARGET WCVP ID\n")
cat("============================================================\n\n")


if (
  length(wcvp_hits) == 0L
) {
  
  cat(
    "No additional database records found for target WCVP ID.\n"
  )
  
} else {
  
  cat(
    "Matching table/field result sets: ",
    length(wcvp_hits),
    "\n\n",
    sep = ""
  )
  
  
  for (
    i in seq_along(
      wcvp_hits
    )
  ) {
    
    hit_data <-
      wcvp_hits[[i]]
    
    
    cat("------------------------------------------------------------\n")
    
    cat(
      "MATCH ",
      i,
      "\n",
      sep = ""
    )
    
    cat("------------------------------------------------------------\n")
    
    
    cat(
      "Source table: ",
      hit_data$SOURCE_TABLE[[1]],
      "\n",
      sep = ""
    )
    
    
    cat(
      "Match field: ",
      hit_data$MATCH_FIELD[[1]],
      "\n",
      sep = ""
    )
    
    
    cat(
      "Rows: ",
      nrow(hit_data),
      "\n\n",
      sep = ""
    )
    
    
    relevant_fields <-
      names(hit_data)[
        grepl(
          paste0(
            "SOURCE_TABLE|MATCH_FIELD|",
            "WCVP|PLANT|TAXON|",
            "SCIENTIFIC|ACCEPT|",
            "STATUS|RANK|FAMILY|GENUS|",
            "SPECIES|NAME"
          ),
          names(hit_data),
          ignore.case = TRUE
        )
      ]
    
    
    if (
      length(relevant_fields) == 0L
    ) {
      
      relevant_fields <-
        names(hit_data)
    }
    
    
    print(
      as.data.frame(
        hit_data |>
          select(
            any_of(
              relevant_fields
            )
          )
      ),
      row.names = FALSE,
      na.print = "NA"
    )
    
    
    cat("\n")
  }
}


# ==============================================================================
# 21. BUILD COMPACT WCVP HIT REGISTRY
# ==============================================================================

if (
  length(wcvp_hits) > 0L
) {
  
  wcvp_hit_registry <-
    bind_rows(
      lapply(
        seq_along(
          wcvp_hits
        ),
        function(i) {
          
          x <-
            wcvp_hits[[i]]
          
          tibble(
            HIT_NUMBER =
              i,
            
            SOURCE_TABLE =
              as.character(
                x$SOURCE_TABLE[[1]]
              ),
            
            MATCH_FIELD =
              as.character(
                x$MATCH_FIELD[[1]]
              ),
            
            N_ROWS =
              nrow(x)
          )
        }
      )
    )
  
} else {
  
  wcvp_hit_registry <-
    tibble(
      HIT_NUMBER =
        integer(),
      
      SOURCE_TABLE =
        character(),
      
      MATCH_FIELD =
        character(),
      
      N_ROWS =
        integer()
    )
}


cat("\n")

cat("============================================================\n")
cat("WCVP HIT REGISTRY\n")
cat("============================================================\n\n")


print(
  as.data.frame(
    wcvp_hit_registry
  ),
  row.names = FALSE,
  na.print = "NA"
)


# ==============================================================================
# 22. CHECK TARGET WCVP ID AGAINST 03f RAW FIELDS
# ==============================================================================

cat("\n")

cat("============================================================\n")
cat("03f TARGET WCVP ID FIELD CHECK\n")
cat("============================================================\n\n")


if (
  nrow(trace_03f) > 0L
) {
  
  wcvp_fields_03f <-
    names(trace_03f)[
      grepl(
        "WCVP",
        names(trace_03f),
        ignore.case = TRUE
      )
    ]
  
  
  if (
    length(wcvp_fields_03f) > 0L
  ) {
    
    print(
      as.data.frame(
        trace_03f |>
          select(
            any_of(
              c(
                "JAPAN_RECORD_ID",
                "JAPAN_SCIENTIFIC_NAME",
                "JAPANESE_NAME",
                wcvp_fields_03f
              )
            )
          )
      ),
      row.names = FALSE,
      na.print = "NA"
    )
    
  } else {
    
    cat(
      "No WCVP-labelled fields were found in the 03f record.\n"
    )
  }
  
} else {
  
  cat(
    "No corresponding 03f record was found.\n"
  )
}


# ==============================================================================
# 23. DETERMINE FIRST STAGE WHERE ACCEPTED NAME IS LOST
# ==============================================================================

stage_name_status <-
  cross_stage |>
  mutate(
    ACCEPTED_NAME_PRESENT =
      if (
        "WCVP_ACCEPTED_NAME" %in%
        names(cross_stage)
      ) {
        
        !is.na(
          WCVP_ACCEPTED_NAME
        ) &
          trimws(
            as.character(
              WCVP_ACCEPTED_NAME
            )
          ) != ""
        
      } else {
        
        FALSE
      }
  ) |>
  select(
    STAGE,
    JAPAN_RECORD_ID,
    any_of(
      c(
        "WCVP_ID",
        "WCVP_ACCEPTED_NAME",
        "WCVP_STATUS"
      )
    ),
    ACCEPTED_NAME_PRESENT
  )


cat("\n")

cat("============================================================\n")
cat("ACCEPTED-NAME HAND-OFF STATUS\n")
cat("============================================================\n\n")


print(
  as.data.frame(
    stage_name_status
  ),
  row.names = FALSE,
  na.print = "NA"
)


# ==============================================================================
# 24. FINAL DIAGNOSTIC SUMMARY
# ==============================================================================

cat("\n")

cat("============================================================\n")
cat("DIAGNOSTIC SUMMARY\n")
cat("============================================================\n\n")


cat(
  "Unresolved 03i records: ",
  nrow(unresolved_03i),
  "\n",
  sep = ""
)


cat(
  "Target JAPAN_RECORD_ID(s): ",
  paste(
    target_ids,
    collapse = " | "
  ),
  "\n",
  sep = ""
)


cat(
  "Target WCVP ID(s): ",
  paste(
    target_wcvp_ids,
    collapse = " | "
  ),
  "\n",
  sep = ""
)


cat(
  "WCVP table/field result sets containing target ID: ",
  length(wcvp_hits),
  "\n",
  sep = ""
)


cat(
  "03f matching records: ",
  nrow(trace_03f),
  "\n",
  sep = ""
)


cat(
  "03g matching records: ",
  nrow(trace_03g),
  "\n",
  sep = ""
)


cat(
  "03h matching records: ",
  nrow(trace_03h),
  "\n",
  sep = ""
)


cat(
  "03i matching records: ",
  nrow(trace_03i),
  "\n\n",
  sep = ""
)


cat("READ-ONLY DIAGNOSTIC\n")
cat("DuckDB modified: FALSE\n")
cat("Canonical taxonomy modified: FALSE\n")
cat("Star allocations modified: FALSE\n")
cat("CSV files written: FALSE\n")


# ==============================================================================
# 25. CLEAN DISCONNECT
# ==============================================================================

DBI::dbDisconnect(
  con,
  shutdown = TRUE
)


rm(con)


gc()


cat("\n")

cat("============================================================\n")
cat("DIAGNOSTIC COMPLETE\n")
cat("============================================================\n")


# ==============================================================================
# END
# ==============================================================================