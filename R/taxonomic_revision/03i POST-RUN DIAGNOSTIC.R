# ==============================================================================
# 03i POST-RUN DIAGNOSTIC
# ==============================================================================

library(readr)
library(dplyr)

audit <-
  read_csv(
    paste0(
      "I:/R/OJPCP/VPJD-OJPCP/",
      "outputs/tables/taxonomic_revision/",
      "03i_reconcile_canonical_gap_candidates_to_wcvp_backbone/",
      "vpjd_taxrev_03i_audit.csv"
    ),
    show_col_types = FALSE
  )


cat("\n============================================================\n")
cat("03i CRITICAL FIELD DIAGNOSTIC\n")
cat("============================================================\n\n")


critical_fields <-
  c(
    "JAPAN_SCIENTIFIC_NAME",
    "JAPANESE_NAME",
    "WCVP_ID",
    "WCVP_ACCEPTED_NAME",
    "WCVP_RANK",
    "WCVP_FAMILY",
    "WCVP_GENUS",
    "WCVP_STATUS"
  )


for (field in critical_fields) {
  
  if (field %in% names(audit)) {
    
    n_present <-
      sum(
        !is.na(audit[[field]]) &
          audit[[field]] != ""
      )
    
    cat(
      sprintf(
        "%-30s %3d / %3d populated\n",
        field,
        n_present,
        nrow(audit)
      )
    )
    
  } else {
    
    cat(
      sprintf(
        "%-30s FIELD MISSING\n",
        field
      )
    )
  }
}


cat("\n------------------------------------------------------------\n")
cat("WCVP BACKBONE CLASS\n")
cat("------------------------------------------------------------\n\n")

print(
  audit |>
    count(
      WCVP_BACKBONE_CLASS,
      sort = TRUE
    ),
  n = Inf
)


cat("\n------------------------------------------------------------\n")
cat("EVIDENCE REQUIREMENT\n")
cat("------------------------------------------------------------\n\n")

print(
  audit |>
    count(
      EVIDENCE_REQUIREMENT,
      sort = TRUE
    ),
  n = Inf
)


cat("\n------------------------------------------------------------\n")
cat("VPJD GAP STATUS\n")
cat("------------------------------------------------------------\n\n")

print(
  audit |>
    count(
      VPJD_GAP_STATUS,
      sort = TRUE
    ),
  n = Inf
)


cat("\n------------------------------------------------------------\n")
cat("FIRST 20 RECORDS\n")
cat("------------------------------------------------------------\n\n")

print(
  audit |>
    select(
      JAPAN_SCIENTIFIC_NAME,
      JAPANESE_NAME,
      WCVP_ID,
      WCVP_ACCEPTED_NAME,
      WCVP_STATUS,
      WCVP_BACKBONE_CLASS,
      EVIDENCE_REQUIREMENT,
      VPJD_GAP_STATUS
    ) |>
    slice_head(
      n = 20
    ),
  n = 20,
  width = Inf
)


cat("\n============================================================\n")
cat("DIAGNOSTIC COMPLETE\n")
cat("============================================================\n")