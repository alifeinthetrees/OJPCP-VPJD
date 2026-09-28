# =============================================================================
# VPJD-OJPCP
# R/stars/05a_profile_provisional_taxonomic_composition.R
# Version 0.1.1
#
# CONTEMPORARY VPJD TAXONOMIC COMPOSITION
#
# PURPOSE
# - Profile the complete 11,439 accepted contemporary VPJD taxa.
# - Attach authoritative WCVP family/genus/species hierarchy.
# - Quantify families, genera, species and infraspecific taxa.
# - Profile taxonomic composition by provisional Star.
# - Produce family-level and rank-level summaries.
#
# IMPORTANT
# - Accepted contemporary VPJD taxa only.
# - WCVP hierarchy is used directly; scientific names are not parsed.
# - WCVP plant_name_id is standardised to character before joining.
# - No Star classifications are changed.
# - Historical flora statistics are not analytical inputs.
# =============================================================================

suppressPackageStartupMessages({
  library(DBI)
  library(duckdb)
  library(dplyr)
  library(readr)
  library(tibble)
  library(here)
})

MODULE <- "stars_05a_profile_provisional_taxonomic_composition"
VERSION <- "0.1.1"
RUN_DATE <- Sys.Date()

DB_PATH <- here(
  "data","interim","occurrences",
  "vpjd_occurrences.duckdb"
)

OUTPUT_DIR <- here(
  "outputs","tables","stars",
  "provisional_taxonomic_composition"
)

TABLE_TAXONOMY <- "vpjd_star_provisional_taxonomic_composition"
TABLE_SUMMARY <- "vpjd_star_provisional_taxonomic_summary"
TABLE_RANK <- "vpjd_star_provisional_rank_profile"
TABLE_FAMILY <- "vpjd_star_provisional_family_profile"
TABLE_STAR_TAXONOMY <- "vpjd_star_provisional_taxonomy_by_star"
TABLE_VALIDATION <- "vpjd_star_05a_validation"
TABLE_METADATA <- "vpjd_star_05a_metadata"

run_stars_05a <- function() {
  
  cat("\n— Contemporary VPJD taxonomic composition —\n\n")
  cat("Run date: ",as.character(RUN_DATE),"\n",sep="")
  cat("Module: ",MODULE,"\n",sep="")
  cat("Version: ",VERSION,"\n\n",sep="")
  
  dir.create(
    OUTPUT_DIR,
    recursive=TRUE,
    showWarnings=FALSE
  )
  
  if (!file.exists(DB_PATH)) {
    stop("VPJD DuckDB not found: ",DB_PATH)
  }
  
  con <- dbConnect(
    duckdb::duckdb(),
    dbdir=DB_PATH,
    read_only=FALSE
  )
  
  on.exit(
    dbDisconnect(con,shutdown=TRUE),
    add=TRUE
  )
  
  # ===========================================================================
  # 1. REQUIRED TABLES
  # ===========================================================================
  
  required_tables <- c(
    "vpjd_star_provisional_wholesale_allocation",
    "occurrence_wcvp_accepted_taxa"
  )
  
  missing_tables <- setdiff(
    required_tables,
    dbListTables(con)
  )
  
  if (length(missing_tables)>0L) {
    stop(
      "Required tables missing: ",
      paste(missing_tables,collapse=", ")
    )
  }
  
  # ===========================================================================
  # 2. STARS 05 POPULATION
  # ===========================================================================
  
  allocation <- as_tibble(
    dbGetQuery(
      con,
      "
      SELECT
        FINAL_WCVP_ID,
        FINAL_WCVP_RECOGNISED_NAME,
        FINAL_WCVP_RANK,
        FINAL_WCVP_STATUS,
        FINAL_WCVP_CONCEPT_CLASS,
        PROVISIONAL_STAR,
        IS_ALLOCATED,
        IS_UNRESOLVED
      FROM vpjd_star_provisional_wholesale_allocation
      "
    )
  ) |>
    mutate(
      FINAL_WCVP_ID=as.character(FINAL_WCVP_ID)
    )
  
  if (nrow(allocation)!=11439L) {
    stop(
      "Expected 11,439 accepted taxa; found ",
      nrow(allocation)
    )
  }
  
  if (n_distinct(allocation$FINAL_WCVP_ID)!=11439L) {
    stop("Stars 05 population is not unique by WCVP ID.")
  }
  
  cat(
    "Accepted contemporary population: ",
    format(nrow(allocation),big.mark=","),
    "\n",
    sep=""
  )
  
  # ===========================================================================
  # 3. AUTHORITATIVE WCVP TAXONOMY
  # ===========================================================================
  
  wcvp_raw <- as_tibble(
    dbGetQuery(
      con,
      "
      SELECT *
      FROM occurrence_wcvp_accepted_taxa
      "
    )
  )
  
  required_wcvp_fields <- c(
    "wcvp_plant_name_id",
    "wcvp_taxon_name",
    "wcvp_taxon_rank",
    "wcvp_taxon_status",
    "wcvp_family",
    "wcvp_genus",
    "wcvp_species"
  )
  
  missing_wcvp_fields <- setdiff(
    required_wcvp_fields,
    names(wcvp_raw)
  )
  
  if (length(missing_wcvp_fields)>0L) {
    stop(
      "Required WCVP fields missing: ",
      paste(missing_wcvp_fields,collapse=", "),
      "\nAvailable fields: ",
      paste(names(wcvp_raw),collapse=", ")
    )
  }
  
  # WCVP IDs were imported as numeric/double.
  # Canonical VPJD FINAL_WCVP_ID is character.
  # Standardise WCVP IDs to character before joining.
  
  wcvp <- wcvp_raw |>
    select(
      wcvp_plant_name_id,
      wcvp_taxon_name,
      wcvp_taxon_rank,
      wcvp_taxon_status,
      wcvp_family,
      wcvp_genus,
      wcvp_species
    ) |>
    mutate(
      wcvp_plant_name_id=as.character(wcvp_plant_name_id)
    ) |>
    distinct(
      wcvp_plant_name_id,
      .keep_all=TRUE
    )
  
  cat(
    "WCVP accepted lookup concepts: ",
    format(nrow(wcvp),big.mark=","),
    "\n",
    sep=""
  )
  
  cat(
    "VPJD ID type: ",
    class(allocation$FINAL_WCVP_ID)[1],
    "\n",
    sep=""
  )
  
  cat(
    "WCVP ID type after standardisation: ",
    class(wcvp$wcvp_plant_name_id)[1],
    "\n\n",
    sep=""
  )
  
  # ===========================================================================
  # 4. JOIN TAXONOMIC HIERARCHY
  # ===========================================================================
  
  taxonomy <- allocation |>
    left_join(
      wcvp,
      by=c(
        "FINAL_WCVP_ID"="wcvp_plant_name_id"
      )
    ) |>
    mutate(
      STAR_DISPLAY=coalesce(
        PROVISIONAL_STAR,
        "UNRESOLVED"
      ),
      RANK_NORMALISED=toupper(
        trimws(
          coalesce(
            wcvp_taxon_rank,
            FINAL_WCVP_RANK
          )
        )
      )
    )
  
  n_wcvp_linked <- sum(
    !is.na(taxonomy$wcvp_taxon_name)
  )
  
  n_missing_wcvp <- sum(
    is.na(taxonomy$wcvp_taxon_name)
  )
  
  cat(
    "VPJD taxa linked to WCVP hierarchy: ",
    format(n_wcvp_linked,big.mark=","),
    " / ",
    format(nrow(taxonomy),big.mark=","),
    "\n",
    sep=""
  )
  
  cat(
    "VPJD taxa without WCVP hierarchy link: ",
    format(n_missing_wcvp,big.mark=","),
    "\n\n",
    sep=""
  )
  
  # ===========================================================================
  # 5. RANK CLASSIFICATION
  # ===========================================================================
  
  taxonomy <- taxonomy |>
    mutate(
      RANK_GROUP=case_when(
        RANK_NORMALISED=="SPECIES" ~
          "Species",
        
        RANK_NORMALISED=="SUBSPECIES" ~
          "Subspecies",
        
        RANK_NORMALISED=="VARIETY" ~
          "Variety",
        
        RANK_NORMALISED=="FORM" ~
          "Form",
        
        RANK_NORMALISED=="GENUS" ~
          "Genus",
        
        RANK_NORMALISED %in% c(
          "SUBVARIETY",
          "SUBFORM",
          "INFRASPECIFIC NAME",
          "INFRASPECIFIC"
        ) ~
          "Other infraspecific",
        
        TRUE ~
          "Other rank"
      )
    )
  
  # ===========================================================================
  # 6. HEADLINE TAXONOMIC COUNTS
  # ===========================================================================
  
  n_families <- taxonomy |>
    filter(
      !is.na(wcvp_family),
      wcvp_family!=""
    ) |>
    summarise(
      N=n_distinct(wcvp_family)
    ) |>
    pull(N)
  
  n_genera <- taxonomy |>
    filter(
      !is.na(wcvp_genus),
      wcvp_genus!=""
    ) |>
    summarise(
      N=n_distinct(wcvp_genus)
    ) |>
    pull(N)
  
  n_species <- sum(
    taxonomy$RANK_GROUP=="Species",
    na.rm=TRUE
  )
  
  n_subspecies <- sum(
    taxonomy$RANK_GROUP=="Subspecies",
    na.rm=TRUE
  )
  
  n_varieties <- sum(
    taxonomy$RANK_GROUP=="Variety",
    na.rm=TRUE
  )
  
  n_forms <- sum(
    taxonomy$RANK_GROUP=="Form",
    na.rm=TRUE
  )
  
  n_other_infraspecific <- sum(
    taxonomy$RANK_GROUP=="Other infraspecific",
    na.rm=TRUE
  )
  
  n_genus_concepts <- sum(
    taxonomy$RANK_GROUP=="Genus",
    na.rm=TRUE
  )
  
  n_other_ranks <- sum(
    taxonomy$RANK_GROUP=="Other rank",
    na.rm=TRUE
  )
  
  n_missing_family <- sum(
    is.na(taxonomy$wcvp_family) |
      taxonomy$wcvp_family==""
  )
  
  n_missing_genus <- sum(
    is.na(taxonomy$wcvp_genus) |
      taxonomy$wcvp_genus==""
  )
  
  # ===========================================================================
  # 7. HEADLINE SUMMARY TABLE
  # ===========================================================================
  
  taxonomic_summary <- tibble(
    METRIC=c(
      "Families",
      "Genera",
      "Species",
      "Subspecies",
      "Varieties",
      "Forms",
      "Other infraspecific taxa",
      "Genus concepts",
      "Other ranks",
      "Accepted taxa"
    ),
    N=c(
      n_families,
      n_genera,
      n_species,
      n_subspecies,
      n_varieties,
      n_forms,
      n_other_infraspecific,
      n_genus_concepts,
      n_other_ranks,
      nrow(taxonomy)
    )
  )
  
  # ===========================================================================
  # 8. EXACT WCVP RANK PROFILE
  # ===========================================================================
  
  rank_profile <- taxonomy |>
    mutate(
      TAXON_RANK=coalesce(
        wcvp_taxon_rank,
        FINAL_WCVP_RANK,
        "MISSING"
      )
    ) |>
    count(
      TAXON_RANK,
      name="N_TAXA",
      sort=TRUE
    ) |>
    mutate(
      PERCENT_ACCEPTED=round(
        100*N_TAXA/nrow(taxonomy),
        3
      )
    )
  
  # ===========================================================================
  # 9. FAMILY PROFILE
  # ===========================================================================
  
  family_profile <- taxonomy |>
    mutate(
      FAMILY=coalesce(
        wcvp_family,
        "MISSING"
      )
    ) |>
    group_by(FAMILY) |>
    summarise(
      N_GENERA=n_distinct(
        wcvp_genus[
          !is.na(wcvp_genus) &
            wcvp_genus!=""
        ]
      ),
      
      N_SPECIES=sum(
        RANK_GROUP=="Species",
        na.rm=TRUE
      ),
      
      N_SUBSPECIES=sum(
        RANK_GROUP=="Subspecies",
        na.rm=TRUE
      ),
      
      N_VARIETIES=sum(
        RANK_GROUP=="Variety",
        na.rm=TRUE
      ),
      
      N_FORMS=sum(
        RANK_GROUP=="Form",
        na.rm=TRUE
      ),
      
      N_OTHER_INFRASPECIFIC=sum(
        RANK_GROUP=="Other infraspecific",
        na.rm=TRUE
      ),
      
      N_GENUS_CONCEPTS=sum(
        RANK_GROUP=="Genus",
        na.rm=TRUE
      ),
      
      N_OTHER_RANKS=sum(
        RANK_GROUP=="Other rank",
        na.rm=TRUE
      ),
      
      N_ACCEPTED_TAXA=n(),
      .groups="drop"
    ) |>
    arrange(
      desc(N_ACCEPTED_TAXA),
      FAMILY
    )
  
  # ===========================================================================
  # 10. TAXONOMIC COMPOSITION BY STAR
  # ===========================================================================
  
  star_taxonomy <- taxonomy |>
    group_by(STAR_DISPLAY) |>
    summarise(
      N_FAMILIES=n_distinct(
        wcvp_family[
          !is.na(wcvp_family) &
            wcvp_family!=""
        ]
      ),
      
      N_GENERA=n_distinct(
        wcvp_genus[
          !is.na(wcvp_genus) &
            wcvp_genus!=""
        ]
      ),
      
      N_SPECIES=sum(
        RANK_GROUP=="Species",
        na.rm=TRUE
      ),
      
      N_SUBSPECIES=sum(
        RANK_GROUP=="Subspecies",
        na.rm=TRUE
      ),
      
      N_VARIETIES=sum(
        RANK_GROUP=="Variety",
        na.rm=TRUE
      ),
      
      N_FORMS=sum(
        RANK_GROUP=="Form",
        na.rm=TRUE
      ),
      
      N_OTHER_INFRASPECIFIC=sum(
        RANK_GROUP=="Other infraspecific",
        na.rm=TRUE
      ),
      
      N_GENUS_CONCEPTS=sum(
        RANK_GROUP=="Genus",
        na.rm=TRUE
      ),
      
      N_OTHER_RANKS=sum(
        RANK_GROUP=="Other rank",
        na.rm=TRUE
      ),
      
      N_TAXA=n(),
      .groups="drop"
    ) |>
    mutate(
      STAR_ORDER=match(
        STAR_DISPLAY,
        c(
          "BK",
          "GD",
          "BU",
          "GN",
          "GX",
          "HYB",
          "UNRESOLVED"
        )
      )
    ) |>
    arrange(STAR_ORDER) |>
    select(-STAR_ORDER)
  
  # ===========================================================================
  # 11. VALIDATION
  # ===========================================================================
  
  rank_total <- sum(
    n_species,
    n_subspecies,
    n_varieties,
    n_forms,
    n_other_infraspecific,
    n_genus_concepts,
    n_other_ranks
  )
  
  validation <- tribble(
    ~CHECK, ~PASS,
    
    "Population = 11,439",
    nrow(taxonomy)==11439L,
    
    "Population unique by WCVP ID",
    n_distinct(taxonomy$FINAL_WCVP_ID)==11439L,
    
    "All population records Accepted",
    all(taxonomy$FINAL_WCVP_STATUS=="Accepted"),
    
    "VPJD WCVP ID stored as character",
    is.character(taxonomy$FINAL_WCVP_ID),
    
    "WCVP lookup ID standardised to character",
    is.character(wcvp$wcvp_plant_name_id),
    
    "WCVP lookup IDs unique after standardisation",
    n_distinct(wcvp$wcvp_plant_name_id)==nrow(wcvp),
    
    "All accepted taxa linked to WCVP lookup",
    n_missing_wcvp==0L,
    
    "All accepted taxa have family",
    n_missing_family==0L,
    
    "All accepted taxa have genus",
    n_missing_genus==0L,
    
    "Rank groups sum to 11,439",
    rank_total==11439L,
    
    "Taxonomic summary accepted total = 11,439",
    taxonomic_summary$N[
      taxonomic_summary$METRIC=="Accepted taxa"
    ]==11439L,
    
    "Exact rank profile sums to 11,439",
    sum(rank_profile$N_TAXA)==11439L,
    
    "Family profile sums to 11,439",
    sum(family_profile$N_ACCEPTED_TAXA)==11439L,
    
    "Star taxonomic profile sums to 11,439",
    sum(star_taxonomy$N_TAXA)==11439L,
    
    "No Star allocations changed",
    all(
      taxonomy$STAR_DISPLAY==
        coalesce(
          taxonomy$PROVISIONAL_STAR,
          "UNRESOLVED"
        )
    )
  ) |>
    mutate(
      PASS=coalesce(PASS,FALSE),
      RESULT=if_else(
        PASS,
        "PASS",
        "FAIL"
      )
    )
  
  # ===========================================================================
  # 12. DISPLAY
  # ===========================================================================
  
  cat(
    "\n============================================================\n"
  )
  
  cat(
    "CONTEMPORARY VPJD TAXONOMIC COMPOSITION\n"
  )
  
  cat(
    "============================================================\n\n"
  )
  
  print(
    taxonomic_summary,
    n=Inf
  )
  
  cat(
    "\n— EXACT WCVP RANK PROFILE —\n"
  )
  
  print(
    rank_profile,
    n=Inf
  )
  
  cat(
    "\n— TAXONOMIC COMPOSITION BY PROVISIONAL STAR —\n"
  )
  
  print(
    star_taxonomy,
    n=Inf
  )
  
  cat(
    "\n— 25 LARGEST FAMILIES —\n"
  )
  
  print(
    family_profile |>
      slice_head(n=25),
    n=25
  )
  
  cat(
    "\n— VALIDATION —\n"
  )
  
  print(
    validation,
    n=Inf
  )
  
  if (!all(validation$PASS)) {
    stop(
      "Stars 05a validation failed. ",
      "Canonical 05a tables have NOT been written."
    )
  }
  
  # ===========================================================================
  # 13. METADATA
  # ===========================================================================
  
  metadata <- tibble(
    MODULE=MODULE,
    VERSION=VERSION,
    RUN_DATE=as.character(RUN_DATE),
    
    ACCEPTED_TAXA=
      nrow(taxonomy),
    
    FAMILIES=
      n_families,
    
    GENERA=
      n_genera,
    
    SPECIES=
      n_species,
    
    SUBSPECIES=
      n_subspecies,
    
    VARIETIES=
      n_varieties,
    
    FORMS=
      n_forms,
    
    OTHER_INFRASPECIFIC=
      n_other_infraspecific,
    
    GENUS_CONCEPTS=
      n_genus_concepts,
    
    OTHER_RANKS=
      n_other_ranks,
    
    WCVP_LINKED=
      n_wcvp_linked,
    
    MISSING_WCVP_LINKS=
      n_missing_wcvp,
    
    MISSING_FAMILY=
      n_missing_family,
    
    MISSING_GENUS=
      n_missing_genus,
    
    HISTORICAL_DATA_USED=
      FALSE,
    
    STAR_ALLOCATIONS_CHANGED=
      FALSE,
    
    STATUS=
      "VALIDATED_TAXONOMIC_PROFILE"
  )
  
  # ===========================================================================
  # 14. WRITE CANONICAL TABLES
  # ===========================================================================
  
  dbWriteTable(
    con,
    TABLE_TAXONOMY,
    taxonomy,
    overwrite=TRUE
  )
  
  dbWriteTable(
    con,
    TABLE_SUMMARY,
    taxonomic_summary,
    overwrite=TRUE
  )
  
  dbWriteTable(
    con,
    TABLE_RANK,
    rank_profile,
    overwrite=TRUE
  )
  
  dbWriteTable(
    con,
    TABLE_FAMILY,
    family_profile,
    overwrite=TRUE
  )
  
  dbWriteTable(
    con,
    TABLE_STAR_TAXONOMY,
    star_taxonomy,
    overwrite=TRUE
  )
  
  dbWriteTable(
    con,
    TABLE_VALIDATION,
    validation,
    overwrite=TRUE
  )
  
  dbWriteTable(
    con,
    TABLE_METADATA,
    metadata,
    overwrite=TRUE
  )
  
  # ===========================================================================
  # 15. WRITE CSV OUTPUTS
  # ===========================================================================
  
  write_csv(
    taxonomy,
    file.path(
      OUTPUT_DIR,
      "stars05a_taxonomic_composition.csv"
    )
  )
  
  write_csv(
    taxonomic_summary,
    file.path(
      OUTPUT_DIR,
      "stars05a_taxonomic_summary.csv"
    )
  )
  
  write_csv(
    rank_profile,
    file.path(
      OUTPUT_DIR,
      "stars05a_rank_profile.csv"
    )
  )
  
  write_csv(
    family_profile,
    file.path(
      OUTPUT_DIR,
      "stars05a_family_profile.csv"
    )
  )
  
  write_csv(
    star_taxonomy,
    file.path(
      OUTPUT_DIR,
      "stars05a_taxonomy_by_star.csv"
    )
  )
  
  write_csv(
    validation,
    file.path(
      OUTPUT_DIR,
      "stars05a_validation.csv"
    )
  )
  
  write_csv(
    metadata,
    file.path(
      OUTPUT_DIR,
      "stars05a_metadata.csv"
    )
  )
  
  # ===========================================================================
  # 16. FINAL SUMMARY
  # ===========================================================================
  
  cat(
    "\n============================================================\n"
  )
  
  cat(
    "Stars 05a v",VERSION," COMPLETE\n",
    sep=""
  )
  
  cat(
    "============================================================\n"
  )
  
  cat(
    "Accepted taxa: ",
    format(nrow(taxonomy),big.mark=","),
    "\n",
    sep=""
  )
  
  cat(
    "Families: ",
    format(n_families,big.mark=","),
    "\n",
    sep=""
  )
  
  cat(
    "Genera: ",
    format(n_genera,big.mark=","),
    "\n",
    sep=""
  )
  
  cat(
    "Species: ",
    format(n_species,big.mark=","),
    "\n",
    sep=""
  )
  
  cat(
    "Subspecies: ",
    format(n_subspecies,big.mark=","),
    "\n",
    sep=""
  )
  
  cat(
    "Varieties: ",
    format(n_varieties,big.mark=","),
    "\n",
    sep=""
  )
  
  cat(
    "Forms: ",
    format(n_forms,big.mark=","),
    "\n",
    sep=""
  )
  
  cat(
    "Other infraspecific: ",
    format(n_other_infraspecific,big.mark=","),
    "\n",
    sep=""
  )
  
  cat(
    "Genus concepts: ",
    format(n_genus_concepts,big.mark=","),
    "\n",
    sep=""
  )
  
  cat(
    "Other ranks: ",
    format(n_other_ranks,big.mark=","),
    "\n",
    sep=""
  )
  
  cat(
    "WCVP hierarchy links: ",
    format(n_wcvp_linked,big.mark=","),
    " / ",
    format(nrow(taxonomy),big.mark=","),
    "\n",
    sep=""
  )
  
  cat(
    "Validation: ",
    sum(validation$PASS),"/",
    nrow(validation),
    " PASS\n",
    sep=""
  )
  
  cat(
    "Canonical taxonomy: ",
    TABLE_TAXONOMY,
    "\n",
    sep=""
  )
  
  cat(
    "Outputs: ",
    OUTPUT_DIR,
    "\n",
    sep=""
  )
  
  cat(
    "============================================================\n"
  )
  
  invisible(
    list(
      taxonomy=taxonomy,
      summary=taxonomic_summary,
      rank_profile=rank_profile,
      family_profile=family_profile,
      star_taxonomy=star_taxonomy,
      validation=validation,
      metadata=metadata
    )
  )
}

stars_05a_result <- run_stars_05a()