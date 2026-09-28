# =============================================================================
# VPJD-OJPCP — 06_build_vpjd_taxa.R
# Build authoritative VPJD taxon concepts — Version 0.1.0
# =============================================================================
library(dplyr); library(readr); library(stringr); library(tibble); library(here)

taxonomy_out_dir <- here("data","processed","taxonomy")
dir.create(taxonomy_out_dir, recursive=TRUE, showWarnings=FALSE)
VPJD_TAXONOMY_VERSION <- "0.1.0"

blank <- function(x) { x <- str_squish(as.character(x)); x[x==""] <- NA_character_; x }
first_ok <- function(x) { x <- blank(x); x <- x[!is.na(x)]; if(length(x)) x[[1]] else NA_character_ }
vpjd_id <- function(n) paste0("VPJD", str_pad(n, 7, pad="0"))
find_col <- function(d, x, required=FALSE) {
  h <- x[x %in% names(d)]
  if(length(h)) return(h[[1]])
  if(required) stop("Required column not found: ", paste(x, collapse=" / "),
                    "\nAvailable columns: ", paste(names(d), collapse=", "))
  NA_character_
}
pull_col <- function(d, x) if(is.na(x)) rep(NA_character_, nrow(d)) else as.character(d[[x]])

norm_status <- function(x) {
  z <- str_to_lower(blank(x))
  case_when(is.na(z)~NA_character_, z=="accepted"~"accepted",
            str_detect(z,"synonym")~"synonym", str_detect(z,"unplaced")~"unplaced",
            str_detect(z,"unresolved")~"unresolved", TRUE~z)
}
norm_rank <- function(x) {
  z <- str_to_lower(blank(x))
  case_when(is.na(z)~NA_character_, z %in% c("species","sp.")~"species",
            z %in% c("subspecies","subsp.","subsp","ssp.","ssp")~"subspecies",
            z %in% c("variety","var.","var")~"variety",
            z %in% c("forma","form","f.","f")~"forma", z=="genus"~"genus", TRUE~z)
}
is_hybrid <- function(x) {
  x <- blank(x); ifelse(is.na(x), FALSE, str_detect(x, "(^|\\s)[×x](\\s|$)"))
}
is_cultivar <- function(x) {
  x <- blank(x); ifelse(is.na(x), FALSE,
                        str_detect(x, regex("\\bcv\\.?\\s|cultivar", ignore_case=TRUE)) | str_detect(x, "'[^']+'"))
}

star_eligibility <- function(status, rank, hybrid=FALSE, cultivar=FALSE, accepted=TRUE) {
  if(!isTRUE(accepted)) return(c("review","No accepted WCVP taxon concept resolved."))
  if(isTRUE(cultivar)) return(c("excluded","Cultivar; not independently eligible for native Star assessment."))
  if(isTRUE(hybrid)) return(c("review","Hybrid taxon; requires explicit hybrid treatment before Star assessment."))
  if(is.na(status)) return(c("review","Taxonomic status unavailable."))
  if(status=="synonym") return(c("historical_only","Synonym; retain historical/source Star evidence only."))
  if(status %in% c("unresolved","unplaced")) return(c("review",paste("Taxonomic concept is",status,"and requires review.")))
  if(status!="accepted") return(c("review",paste("Unsupported taxonomic status:",status)))
  if(rank=="species") return(c("independent","Accepted species eligible for independent geographic Star assessment."))
  if(rank %in% c("subspecies","variety")) return(c("infraspecific","Accepted infraspecific taxon; assess geography then apply infraspecific treatment."))
  if(rank=="forma") return(c("containing_taxon","Forma; Star treatment depends on containing taxon."))
  c("review",paste("Rank not configured for automatic Star assessment:",ifelse(is.na(rank),"missing",rank)))
}

prepare_vpjd_taxonomy_input <- function(data, source_name_col=NULL) {
  if(!is.data.frame(data) || !nrow(data)) stop("Input data must contain records.")
  if(is.null(source_name_col)) source_name_col <- find_col(data,
                                                           c("gbif_binom","scientific_name","scientificName","submitted_name","taxon"), TRUE)
  
  idc <- find_col(data,"wcvp_id",TRUE); nc <- find_col(data,"wcvp_name",TRUE)
  ac <- find_col(data,"wcvp_authors"); rc <- find_col(data,"wcvp_rank",TRUE)
  sc <- find_col(data,"wcvp_status",TRUE); aic <- find_col(data,"wcvp_accepted_id",TRUE)
  anc <- find_col(data,c("accepted_name","wcvp_accepted_name"))
  aac <- find_col(data,c("accepted_authors","wcvp_accepted_authors"))
  arc <- find_col(data,c("accepted_rank","wcvp_accepted_rank"))
  fc <- find_col(data,c("accepted_family","wcvp_accepted_family","family"))
  gc <- find_col(data,c("accepted_genus","wcvp_accepted_genus","genus"))
  mt <- find_col(data,"match_type"); mm <- find_col(data,"multiple_matches")
  ms <- find_col(data,"match_similarity")
  
  wid <- blank(pull_col(data,idc)); wn <- blank(pull_col(data,nc))
  wa <- blank(pull_col(data,ac)); wr <- norm_rank(pull_col(data,rc))
  ws <- norm_status(pull_col(data,sc)); wai <- blank(pull_col(data,aic))
  an <- blank(pull_col(data,anc)); aa <- blank(pull_col(data,aac)); ar <- norm_rank(pull_col(data,arc))
  
  wai <- ifelse(ws=="accepted" & is.na(wai), wid, wai)
  an <- ifelse(!is.na(an), an, ifelse(ws=="accepted",wn,NA_character_))
  aa <- ifelse(!is.na(aa), aa, ifelse(ws=="accepted",wa,NA_character_))
  ar <- ifelse(!is.na(ar), ar, ifelse(ws=="accepted",wr,NA_character_))
  
  tibble(source_name=blank(pull_col(data,source_name_col)), wcvp_id=wid, wcvp_name=wn,
         wcvp_authors=wa, wcvp_rank=wr, wcvp_status=ws, wcvp_accepted_id=wai,
         accepted_name=an, accepted_authors=aa, accepted_rank=norm_rank(ar),
         family=blank(pull_col(data,fc)), genus=blank(pull_col(data,gc)),
         match_type=blank(pull_col(data,mt)), multiple_matches=blank(pull_col(data,mm)),
         match_similarity=suppressWarnings(as.numeric(pull_col(data,ms)))) %>%
    mutate(hybrid_flag=is_hybrid(coalesce(accepted_name,wcvp_name,source_name)),
           cultivar_flag=is_cultivar(coalesce(accepted_name,wcvp_name,source_name)))
}

build_vpjd_taxa <- function(x) {
  z <- x %>% filter(!is.na(wcvp_accepted_id)) %>% group_by(wcvp_accepted_id) %>%
    summarise(accepted_name=first_ok(accepted_name), accepted_authors=first_ok(accepted_authors),
              taxon_rank=first_ok(accepted_rank), family=first_ok(family), genus=first_ok(genus),
              hybrid_flag=any(hybrid_flag,na.rm=TRUE), cultivar_flag=any(cultivar_flag,na.rm=TRUE),
              n_source_names=n_distinct(source_name[!is.na(source_name)]),
              n_wcvp_names=n_distinct(wcvp_id[!is.na(wcvp_id)]), .groups="drop") %>%
    arrange(suppressWarnings(as.numeric(wcvp_accepted_id)),accepted_name) %>%
    mutate(vpjd_taxon_id=vpjd_id(row_number()), taxonomic_status="accepted") %>%
    relocate(vpjd_taxon_id)
  if(!nrow(z)) stop("No accepted WCVP concepts available.")
  e <- t(vapply(seq_len(nrow(z)), function(i)
    star_eligibility(z$taxonomic_status[i],z$taxon_rank[i],z$hybrid_flag[i],z$cultivar_flag[i],TRUE),
    character(2)))
  z$star_eligibility <- e[,1]; z$star_eligibility_reason <- e[,2]
  z %>% mutate(taxonomy_review_required=star_eligibility=="review",
               taxonomy_review_reason=ifelse(taxonomy_review_required,star_eligibility_reason,NA_character_))
}

build_vpjd_name_crosswalk <- function(x,taxa) {
  x %>% left_join(taxa %>% select(vpjd_taxon_id,wcvp_accepted_id),by="wcvp_accepted_id") %>%
    mutate(relationship=case_when(is.na(wcvp_id)~"unmatched",wcvp_status=="accepted"~"accepted_name",
                                  wcvp_status=="synonym"~"synonym",wcvp_status=="unresolved"~"unresolved",
                                  wcvp_status=="unplaced"~"unplaced",TRUE~coalesce(wcvp_status,"unknown")),
           multi=tolower(coalesce(multiple_matches,"false")) %in% c("true","t","1","yes"),
           crosswalk_review_required=is.na(wcvp_id)|is.na(vpjd_taxon_id)|
             relationship %in% c("unresolved","unplaced","unknown")|multi,
           crosswalk_review_reason=case_when(is.na(wcvp_id)~"No WCVP match.",
                                             is.na(vpjd_taxon_id)~"No accepted VPJD taxon concept resolved.",
                                             relationship %in% c("unresolved","unplaced")~paste("WCVP relationship:",relationship),
                                             multi~"WCVP matching returned multiple matches.",TRUE~NA_character_)) %>%
    select(-multi) %>% distinct()
}

build_vpjd_taxonomic_relationships <- function(x) x %>% filter(!is.na(wcvp_id)) %>%
  transmute(vpjd_taxon_id,source_wcvp_id=wcvp_id,source_name=wcvp_name,
            target_wcvp_accepted_id=wcvp_accepted_id,target_accepted_name=accepted_name,
            relationship,relationship_source="WCVP") %>% distinct()

build_vpjd_taxonomy_review <- function(x,taxa,cross) {
  a <- taxa %>% filter(taxonomy_review_required) %>%
    transmute(review_level="taxon_concept",vpjd_taxon_id,source_name=NA_character_,
              wcvp_id=wcvp_accepted_id,accepted_name,review_reason=taxonomy_review_reason)
  b <- cross %>% filter(crosswalk_review_required) %>%
    transmute(review_level="source_name",vpjd_taxon_id,source_name,wcvp_id,accepted_name,
              review_reason=crosswalk_review_reason)
  c <- x %>% filter(is.na(wcvp_id)) %>%
    transmute(review_level="unmatched_name",vpjd_taxon_id=NA_character_,source_name,
              wcvp_id=NA_character_,accepted_name=NA_character_,review_reason="No WCVP taxon match.")
  bind_rows(a,b,c) %>% distinct() %>% arrange(review_level,accepted_name,source_name)
}

build_vpjd_taxonomy <- function(data,label="vpjd",source_name_col=NULL,write_outputs=TRUE) {
  message("Building VPJD taxonomy: ",label)
  x <- prepare_vpjd_taxonomy_input(data,source_name_col)
  taxa <- build_vpjd_taxa(x); cross <- build_vpjd_name_crosswalk(x,taxa)
  rel <- build_vpjd_taxonomic_relationships(cross)
  review <- build_vpjd_taxonomy_review(x,taxa,cross)
  stopifnot(!anyDuplicated(taxa$vpjd_taxon_id),!anyDuplicated(taxa$wcvp_accepted_id))
  meta <- tibble(vpjd_taxonomy_version=VPJD_TAXONOMY_VERSION,label=label,
                 generated=format(Sys.time(),"%Y-%m-%d %H:%M:%S"),taxonomy_backbone="WCVP",
                 n_input_records=nrow(data),n_vpjd_taxa=nrow(taxa),
                 n_name_crosswalk=nrow(cross),n_relationships=nrow(rel),n_review_records=nrow(review))
  out <- list(taxonomy_records=x,vpjd_taxa=taxa,vpjd_name_crosswalk=cross,
              vpjd_taxonomic_relationships=rel,vpjd_taxonomy_review=review,metadata=meta)
  if(write_outputs) {
    write_csv(taxa,file.path(taxonomy_out_dir,"vpjd_taxa.csv"))
    write_csv(cross,file.path(taxonomy_out_dir,"vpjd_name_crosswalk.csv"))
    write_csv(rel,file.path(taxonomy_out_dir,"vpjd_taxonomic_relationships.csv"))
    write_csv(review,file.path(taxonomy_out_dir,"vpjd_taxonomy_review.csv"))
    write_csv(meta,file.path(taxonomy_out_dir,"vpjd_taxonomy_metadata.csv"))
    saveRDS(out,file.path(taxonomy_out_dir,"vpjd_taxonomy.rds"))
  }
  message("VPJD taxonomy build complete. Accepted concepts: ",format(nrow(taxa),big.mark=","),
          "; review rows: ",format(nrow(review),big.mark=","))
  invisible(out)
}

# Self-test
.test <- tibble(
  gbif_binom=c("Betula chichibuensis","Quercus robur","Betula alba","Notareal plantus"),
  wcvp_id=c("21215","174750","TEST_SYN",NA), wcvp_name=c("Betula chichibuensis","Quercus robur","Betula alba",NA),
  wcvp_authors=NA_character_, wcvp_rank=c("species","species","species",NA),
  wcvp_status=c("Accepted","Accepted","Synonym",NA),
  wcvp_accepted_id=c("21215","174750","999999",NA),
  accepted_name=c("Betula chichibuensis","Quercus robur","Betula pubescens",NA),
  accepted_rank=c("species","species","species",NA)
)
.test_result <- build_vpjd_taxonomy(.test,"self_test","gbif_binom",FALSE)
stopifnot(nrow(.test_result$vpjd_taxa)==3,
          any(.test_result$vpjd_name_crosswalk$relationship=="synonym"),
          any(.test_result$vpjd_taxonomy_review$review_level=="unmatched_name"))
rm(.test,.test_result)
message("06_build_vpjd_taxa.R loaded successfully.")
message("Built-in taxonomy tests: passed.")
