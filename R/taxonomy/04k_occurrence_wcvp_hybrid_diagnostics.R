# ==============================================================================
# 04k_occurrence_wcvp_hybrid_diagnostics.R
# VPJD / Oxford-Japan Plant Conservation Partnership
#
# Purpose:
#   Hardened diagnostic assessment of unresolved named hybrid / nothotaxon
#   species concepts after 04j.
#
# Diagnostic routes:
#   K1 - exact WCVP match retaining hybrid marker
#   K2 - exact WCVP match after removing hybrid marker
#   K3 - exact WCVP match to GBIF species field
#
# Classification:
#   - secure_wcvp_hybrid_resolution
#   - review_changed_accepted_concept
#   - wcvp_artificial_hybrid_treatment
#   - multiple_wcvp_hybrid_concepts
#   - no_wcvp_hybrid_evidence
#
# Safeguards:
#   - DIAGNOSTIC ONLY: no WCVP assignments;
#   - no parental inference;
#   - no fuzzy matching;
#   - no edit distance;
#   - no epithet-only matching;
#   - no unrestricted cross-genus search;
#   - Artificial Hybrid is not treated as ordinary Accepted status;
#   - accepted-name changes are explicitly separated for review;
#   - upstream reconciliation tables remain unchanged.
#
# Version: 0.1.1
# ==============================================================================

VPJD_WCVP_HYBRID_DIAG_VERSION <- "0.1.1"
required_packages <- c("here","DBI","duckdb","readr","dplyr","cli","rWCVPdata")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0L) {
  stop("Required package(s) not installed: ", paste(missing_packages, collapse = ", "))
}
suppressPackageStartupMessages({
  library(here)
  library(DBI)
  library(duckdb)
  library(readr)
  library(dplyr)
  library(cli)
})

run_occurrence_wcvp_hybrid_diagnostics <- function() {
  duckdb_path <- here::here(
    "data","interim","occurrences","vpjd_occurrences.duckdb"
  )
  output_dir <- here::here(
    "outputs","tables","taxonomy","occurrence_wcvp_hybrid_diagnostics"
  )
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  if (!file.exists(duckdb_path)) {
    stop("VPJD occurrence DuckDB not found:\n", duckdb_path)
  }
  
  con <- DBI::dbConnect(
    duckdb::duckdb(),
    dbdir = duckdb_path,
    read_only = FALSE
  )
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  
  required_tables <- c(
    "occurrence_wcvp_reconciliation_alternative_combination",
    "occurrence_wcvp_nomenclatural_diagnostics"
  )
  missing_tables <- setdiff(required_tables, DBI::dbListTables(con))
  if (length(missing_tables) > 0L) {
    stop("Required DuckDB table(s) missing: ",
         paste(missing_tables, collapse = ", "))
  }
  
  fmt <- function(x) {
    format(x, big.mark = ",", scientific = FALSE, trim = TRUE)
  }
  
  cli::cli_h1("VPJD WCVP hybrid-name diagnostics v0.1.1")
  
  # ---------------------------------------------------------------------------
  # Secure baseline
  # ---------------------------------------------------------------------------
  
  baseline <- DBI::dbGetQuery(
    con,
    "
    SELECT
      COUNT(*) AS total_concepts,
      SUM(CASE
        WHEN alternative_final_wcvp_accepted_plant_name_id IS NOT NULL
        THEN 1 ELSE 0
      END) AS resolved_before_04k,
      COUNT(
        DISTINCT alternative_final_wcvp_accepted_plant_name_id
      ) AS distinct_accepted_before_04k,
      SUM(CASE
        WHEN alternative_final_wcvp_accepted_plant_name_id IS NULL
        THEN 1 ELSE 0
      END) AS unresolved_before_04k
    FROM occurrence_wcvp_reconciliation_alternative_combination
    "
  )
  
  cli::cli_text(
    "{.strong Secure resolved baseline:} ",
    "{fmt(baseline$resolved_before_04k[[1]])}"
  )
  cli::cli_text(
    "{.strong Secure distinct accepted concepts:} ",
    "{fmt(baseline$distinct_accepted_before_04k[[1]])}"
  )
  cli::cli_text(
    "{.strong Secure unresolved baseline:} ",
    "{fmt(baseline$unresolved_before_04k[[1]])}"
  )
  
  # ---------------------------------------------------------------------------
  # Remove only 04k-owned products
  # ---------------------------------------------------------------------------
  
  for (tbl in c(
    "occurrence_wcvp_hybrid_diagnostic_metadata",
    "occurrence_wcvp_hybrid_diagnostics",
    "occurrence_wcvp_hybrid_candidates"
  )) {
    DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", tbl))
  }
  
  # ---------------------------------------------------------------------------
  # Hybrid input
  # ---------------------------------------------------------------------------
  
  DBI::dbExecute(
    con,
    "
    CREATE OR REPLACE TEMP TABLE hybrid_input_raw AS
    SELECT
      r.vpjd_taxon_universe_id,
      r.taxonKey,
      r.speciesKey,
      r.family,
      r.genus,
      r.species,
      r.taxonRank,
      r.scientificName,
      r.total_records,
      r.present_records,
      LOWER(
        TRIM(
          REGEXP_REPLACE(
            COALESCE(r.scientificName, ''),
            '\\s+',' ','g'
          )
        )
      ) AS norm_scientific_name,
      LOWER(
        TRIM(
          REGEXP_REPLACE(
            COALESCE(r.species, ''),
            '\\s+',' ','g'
          )
        )
      ) AS norm_gbif_species
    FROM occurrence_wcvp_reconciliation_alternative_combination r
    WHERE r.alternative_final_wcvp_accepted_plant_name_id IS NULL
      AND UPPER(COALESCE(r.taxonRank, '')) = 'SPECIES'
      AND REGEXP_MATCHES(
        COALESCE(r.scientificName, ''),
        '^[^ ]+[[:space:]]+[×xX][[:space:]]+[^ ]+'
      )
    "
  )
  
  DBI::dbExecute(
    con,
    "
    CREATE OR REPLACE TEMP TABLE hybrid_input AS
    SELECT
      *,
      LOWER(
        REGEXP_EXTRACT(
          COALESCE(scientificName, ''),
          '^([^ ]+)[[:space:]]+[×xX][[:space:]]+([^ ]+)',
          1
        )
      ) AS hybrid_genus,
      LOWER(
        REGEXP_REPLACE(
          REGEXP_EXTRACT(
            COALESCE(scientificName, ''),
            '^([^ ]+)[[:space:]]+[×xX][[:space:]]+([^ ]+)',
            2
          ),
          '[^[:alpha:]-]','','g'
        )
      ) AS hybrid_epithet
    FROM hybrid_input_raw
    "
  )
  
  input_summary <- DBI::dbGetQuery(
    con,
    "
    SELECT
      COUNT(*) AS concepts,
      SUM(present_records) AS present_records
    FROM hybrid_input
    "
  )
  
  cli::cli_text(
    "{.strong Explicit unresolved hybrid concepts:} ",
    "{fmt(input_summary$concepts[[1]])}"
  )
  cli::cli_text(
    "{.strong PRESENT records represented:} ",
    "{fmt(input_summary$present_records[[1]])}"
  )
  
  # ---------------------------------------------------------------------------
  # Complete WCVP source
  # ---------------------------------------------------------------------------
  
  wcvp_names <- rWCVPdata::wcvp_names
  
  required_wcvp_fields <- c(
    "plant_name_id","taxon_name","taxon_authors","taxon_rank",
    "taxon_status","accepted_plant_name_id","family"
  )
  missing_wcvp_fields <- setdiff(required_wcvp_fields, names(wcvp_names))
  if (length(missing_wcvp_fields) > 0L) {
    stop(
      "rWCVPdata::wcvp_names missing field(s): ",
      paste(missing_wcvp_fields, collapse = ", ")
    )
  }
  
  wcvp_source <- wcvp_names[, required_wcvp_fields, drop = FALSE]
  
  DBI::dbWriteTable(
    con,
    "wcvp_04k_source",
    wcvp_source,
    temporary = TRUE,
    overwrite = TRUE
  )
  
  rm(wcvp_source, wcvp_names)
  gc()
  
  # ---------------------------------------------------------------------------
  # Complete accepted-ID lookup built directly from WCVP.
  #
  # This deliberately does not rely on the earlier occurrence accepted lookup,
  # because Artificial Hybrid rows exposed accepted IDs absent from that table.
  # ---------------------------------------------------------------------------
  
  DBI::dbExecute(
    con,
    "
    CREATE OR REPLACE TEMP TABLE wcvp_04k_id_lookup AS
    SELECT
      TRY_CAST(plant_name_id AS BIGINT) AS plant_name_id,
      taxon_name,
      taxon_authors,
      taxon_rank,
      taxon_status,
      TRY_CAST(accepted_plant_name_id AS BIGINT) AS accepted_plant_name_id,
      family
    FROM wcvp_04k_source
    WHERE TRY_CAST(plant_name_id AS BIGINT) IS NOT NULL
    "
  )
  
  # ---------------------------------------------------------------------------
  # Species nomenclatural index
  # ---------------------------------------------------------------------------
  
  DBI::dbExecute(
    con,
    "
    CREATE OR REPLACE TEMP TABLE wcvp_04k_species_index AS
    SELECT
      TRY_CAST(plant_name_id AS BIGINT) AS wcvp_plant_name_id,
      taxon_name AS wcvp_taxon_name,
      taxon_authors AS wcvp_taxon_authors,
      taxon_status AS wcvp_taxon_status,
      TRY_CAST(accepted_plant_name_id AS BIGINT)
        AS wcvp_accepted_plant_name_id,
      family AS wcvp_family,
      LOWER(
        TRIM(
          REGEXP_REPLACE(
            COALESCE(taxon_name, ''),
            '\\s+',' ','g'
          )
        )
      ) AS norm_wcvp_name,
      LOWER(
        TRIM(
          REGEXP_REPLACE(
            REGEXP_REPLACE(
              COALESCE(taxon_name, ''),
              '[[:space:]]+[×xX][[:space:]]+',
              ' ','g'
            ),
            '\\s+',' ','g'
          )
        )
      ) AS norm_wcvp_name_unmarked
    FROM wcvp_04k_source
    WHERE UPPER(COALESCE(taxon_rank, '')) = 'SPECIES'
      AND TRY_CAST(accepted_plant_name_id AS BIGINT) IS NOT NULL
    "
  )
  
  # ---------------------------------------------------------------------------
  # Query names
  # ---------------------------------------------------------------------------
  
  DBI::dbExecute(
    con,
    "
    CREATE OR REPLACE TEMP TABLE hybrid_query_names AS

    SELECT
      vpjd_taxon_universe_id,
      'K1_MARKED_HYBRID'::VARCHAR AS diagnostic_rule,
      hybrid_genus || ' × ' || hybrid_epithet AS query_name
    FROM hybrid_input
    WHERE hybrid_genus <> ''
      AND hybrid_epithet <> ''

    UNION ALL

    SELECT
      vpjd_taxon_universe_id,
      'K2_UNMARKED_HYBRID',
      hybrid_genus || ' ' || hybrid_epithet
    FROM hybrid_input
    WHERE hybrid_genus <> ''
      AND hybrid_epithet <> ''

    UNION ALL

    SELECT
      vpjd_taxon_universe_id,
      'K3_GBIF_SPECIES_FIELD',
      norm_gbif_species
    FROM hybrid_input
    WHERE norm_gbif_species <> ''
      AND ARRAY_LENGTH(
        STRING_SPLIT(norm_gbif_species, ' ')
      ) = 2
    "
  )
  
  # ---------------------------------------------------------------------------
  # Exact WCVP candidates
  # ---------------------------------------------------------------------------
  
  DBI::dbExecute(
    con,
    "
    CREATE TABLE occurrence_wcvp_hybrid_candidates AS
    SELECT DISTINCT
      q.vpjd_taxon_universe_id,
      q.diagnostic_rule,
      q.query_name,
      w.wcvp_plant_name_id,
      w.wcvp_taxon_name,
      w.wcvp_taxon_authors,
      w.wcvp_taxon_status,
      w.wcvp_accepted_plant_name_id,
      w.wcvp_family
    FROM hybrid_query_names q
    INNER JOIN wcvp_04k_species_index w
      ON (
        q.diagnostic_rule = 'K1_MARKED_HYBRID'
        AND q.query_name = w.norm_wcvp_name
      )
      OR (
        q.diagnostic_rule IN (
          'K2_UNMARKED_HYBRID',
          'K3_GBIF_SPECIES_FIELD'
        )
        AND q.query_name = w.norm_wcvp_name_unmarked
      )
    "
  )
  
  # ---------------------------------------------------------------------------
  # Candidate summary
  # ---------------------------------------------------------------------------
  
  DBI::dbExecute(
    con,
    "
    CREATE OR REPLACE TEMP TABLE hybrid_candidate_summary AS
    SELECT
      vpjd_taxon_universe_id,
      COUNT(*) AS candidate_rows,
      COUNT(DISTINCT wcvp_plant_name_id)
        AS nomenclatural_name_count,
      COUNT(DISTINCT wcvp_accepted_plant_name_id)
        AS accepted_concept_count,
      MIN(wcvp_accepted_plant_name_id)
        AS candidate_accepted_concept_id,
      COUNT(DISTINCT diagnostic_rule)
        AS supporting_rule_count,
      STRING_AGG(
        DISTINCT diagnostic_rule,
        '; ' ORDER BY diagnostic_rule
      ) AS supporting_rules,
      STRING_AGG(
        DISTINCT query_name,
        '; ' ORDER BY query_name
      ) AS matched_query_names,
      SUM(CASE
        WHEN UPPER(COALESCE(wcvp_taxon_status, '')) = 'ARTIFICIAL HYBRID'
        THEN 1 ELSE 0
      END) AS artificial_hybrid_candidate_rows,
      SUM(CASE
        WHEN UPPER(COALESCE(wcvp_taxon_status, '')) = 'ACCEPTED'
        THEN 1 ELSE 0
      END) AS accepted_status_candidate_rows,
      SUM(CASE
        WHEN UPPER(COALESCE(wcvp_taxon_status, '')) = 'SYNONYM'
        THEN 1 ELSE 0
      END) AS synonym_candidate_rows
    FROM occurrence_wcvp_hybrid_candidates
    GROUP BY vpjd_taxon_universe_id
    "
  )
  
  # ---------------------------------------------------------------------------
  # Attach the target WCVP record for a unique accepted ID
  # ---------------------------------------------------------------------------
  
  DBI::dbExecute(
    con,
    "
    CREATE OR REPLACE TEMP TABLE hybrid_candidate_target AS
    SELECT
      c.*,
      t.taxon_name AS target_wcvp_taxon_name,
      t.taxon_authors AS target_wcvp_taxon_authors,
      t.taxon_rank AS target_wcvp_taxon_rank,
      t.taxon_status AS target_wcvp_taxon_status,
      t.accepted_plant_name_id AS target_wcvp_accepted_plant_name_id,
      t.family AS target_wcvp_family,
      LOWER(
        TRIM(
          REGEXP_REPLACE(
            REGEXP_REPLACE(
              COALESCE(t.taxon_name, ''),
              '[[:space:]]+[×xX][[:space:]]+',
              ' ','g'
            ),
            '\\s+',' ','g'
          )
        )
      ) AS target_name_unmarked
    FROM hybrid_candidate_summary c
    LEFT JOIN wcvp_04k_id_lookup t
      ON c.accepted_concept_count = 1
     AND c.candidate_accepted_concept_id = t.plant_name_id
    "
  )
  
  # ---------------------------------------------------------------------------
  # Hardened diagnostic classification
  #
  # secure:
  #   unique target, target record exists, target is Accepted, and target
  #   normalized name agrees with either reconstructed hybrid or GBIF species.
  #
  # artificial:
  #   any unique evidence whose matching nomenclatural evidence contains
  #   Artificial Hybrid, unless it resolves to a normal Accepted target with
  #   direct name agreement.
  #
  # review:
  #   unique target exists but accepted target name changes materially.
  # ---------------------------------------------------------------------------
  
  DBI::dbExecute(
    con,
    "
    CREATE TABLE occurrence_wcvp_hybrid_diagnostics AS
    SELECT
      i.vpjd_taxon_universe_id,
      i.taxonKey,
      i.speciesKey,
      i.family,
      i.genus,
      i.species,
      i.taxonRank,
      i.scientificName,
      i.total_records,
      i.present_records,
      i.hybrid_genus,
      i.hybrid_epithet,
      i.hybrid_genus || ' × ' || i.hybrid_epithet
        AS reconstructed_hybrid_name,
      i.hybrid_genus || ' ' || i.hybrid_epithet
        AS reconstructed_unmarked_name,
      i.norm_gbif_species AS gbif_species_name,
      COALESCE(c.candidate_rows, 0) AS candidate_rows,
      COALESCE(c.nomenclatural_name_count, 0)
        AS nomenclatural_name_count,
      COALESCE(c.accepted_concept_count, 0)
        AS accepted_concept_count,
      c.candidate_accepted_concept_id,
      c.supporting_rule_count,
      c.supporting_rules,
      c.matched_query_names,
      COALESCE(c.artificial_hybrid_candidate_rows, 0)
        AS artificial_hybrid_candidate_rows,
      COALESCE(c.accepted_status_candidate_rows, 0)
        AS accepted_status_candidate_rows,
      COALESCE(c.synonym_candidate_rows, 0)
        AS synonym_candidate_rows,
      c.target_wcvp_taxon_name,
      c.target_wcvp_taxon_authors,
      c.target_wcvp_taxon_rank,
      c.target_wcvp_taxon_status,
      c.target_wcvp_accepted_plant_name_id,
      c.target_wcvp_family,
      c.target_name_unmarked,

      CASE
        WHEN c.accepted_concept_count IS NULL
          OR c.accepted_concept_count = 0
        THEN 'no_wcvp_hybrid_evidence'

        WHEN c.accepted_concept_count > 1
        THEN 'multiple_wcvp_hybrid_concepts'

        WHEN c.artificial_hybrid_candidate_rows > 0
          AND (
            c.target_wcvp_taxon_status IS NULL
            OR UPPER(c.target_wcvp_taxon_status) <> 'ACCEPTED'
          )
        THEN 'wcvp_artificial_hybrid_treatment'

        WHEN c.target_wcvp_taxon_name IS NULL
        THEN 'wcvp_target_record_missing'

        WHEN UPPER(COALESCE(c.target_wcvp_taxon_status, '')) =
             'ARTIFICIAL HYBRID'
        THEN 'wcvp_artificial_hybrid_treatment'

        WHEN UPPER(COALESCE(c.target_wcvp_taxon_status, '')) = 'ACCEPTED'
          AND (
            c.target_name_unmarked =
              i.hybrid_genus || ' ' || i.hybrid_epithet
            OR c.target_name_unmarked = i.norm_gbif_species
          )
        THEN 'secure_wcvp_hybrid_resolution'

        WHEN UPPER(COALESCE(c.target_wcvp_taxon_status, '')) = 'ACCEPTED'
        THEN 'review_changed_accepted_concept'

        ELSE 'review_nonstandard_wcvp_target'
      END AS hybrid_diagnostic_class

    FROM hybrid_input i
    LEFT JOIN hybrid_candidate_target c
      ON i.vpjd_taxon_universe_id = c.vpjd_taxon_universe_id
    "
  )
  
  # ---------------------------------------------------------------------------
  # Baseline safety check
  # ---------------------------------------------------------------------------
  
  baseline_after <- DBI::dbGetQuery(
    con,
    "
    SELECT
      SUM(CASE
        WHEN alternative_final_wcvp_accepted_plant_name_id IS NOT NULL
        THEN 1 ELSE 0
      END) AS resolved_after_04k,
      COUNT(
        DISTINCT alternative_final_wcvp_accepted_plant_name_id
      ) AS distinct_accepted_after_04k
    FROM occurrence_wcvp_reconciliation_alternative_combination
    "
  )
  
  if (
    baseline_after$resolved_after_04k[[1]] !=
    baseline$resolved_before_04k[[1]] ||
    baseline_after$distinct_accepted_after_04k[[1]] !=
    baseline$distinct_accepted_before_04k[[1]]
  ) {
    stop("Safety check failed: 04k changed the secure baseline.")
  }
  
  # ---------------------------------------------------------------------------
  # Summaries
  # ---------------------------------------------------------------------------
  
  diagnostic_summary <- DBI::dbGetQuery(
    con,
    "
    SELECT
      hybrid_diagnostic_class,
      COUNT(*) AS concepts,
      SUM(present_records) AS present_records
    FROM occurrence_wcvp_hybrid_diagnostics
    GROUP BY hybrid_diagnostic_class
    ORDER BY present_records DESC
    "
  )
  
  rule_summary <- DBI::dbGetQuery(
    con,
    "
    SELECT
      diagnostic_rule,
      COUNT(DISTINCT vpjd_taxon_universe_id) AS concepts,
      COUNT(*) AS candidate_rows,
      COUNT(DISTINCT wcvp_accepted_plant_name_id)
        AS accepted_concepts
    FROM occurrence_wcvp_hybrid_candidates
    GROUP BY diagnostic_rule
    ORDER BY concepts DESC, diagnostic_rule
    "
  )
  
  secure_evidence <- DBI::dbGetQuery(
    con,
    "
    SELECT *
    FROM occurrence_wcvp_hybrid_diagnostics
    WHERE hybrid_diagnostic_class =
          'secure_wcvp_hybrid_resolution'
    ORDER BY present_records DESC, scientificName
    "
  )
  
  review_evidence <- DBI::dbGetQuery(
    con,
    "
    SELECT *
    FROM occurrence_wcvp_hybrid_diagnostics
    WHERE hybrid_diagnostic_class IN (
      'review_changed_accepted_concept',
      'review_nonstandard_wcvp_target',
      'wcvp_target_record_missing'
    )
    ORDER BY present_records DESC, scientificName
    "
  )
  
  artificial_hybrids <- DBI::dbGetQuery(
    con,
    "
    SELECT *
    FROM occurrence_wcvp_hybrid_diagnostics
    WHERE hybrid_diagnostic_class =
          'wcvp_artificial_hybrid_treatment'
    ORDER BY present_records DESC, scientificName
    "
  )
  
  ambiguous_evidence <- DBI::dbGetQuery(
    con,
    "
    SELECT *
    FROM occurrence_wcvp_hybrid_diagnostics
    WHERE hybrid_diagnostic_class =
          'multiple_wcvp_hybrid_concepts'
    ORDER BY present_records DESC, scientificName
    "
  )
  
  no_evidence <- DBI::dbGetQuery(
    con,
    "
    SELECT *
    FROM occurrence_wcvp_hybrid_diagnostics
    WHERE hybrid_diagnostic_class =
          'no_wcvp_hybrid_evidence'
    ORDER BY present_records DESC, scientificName
    "
  )
  
  candidate_detail <- DBI::dbGetQuery(
    con,
    "
    SELECT
      d.vpjd_taxon_universe_id,
      d.taxonKey,
      d.scientificName,
      d.family,
      d.species,
      d.present_records,
      d.reconstructed_hybrid_name,
      d.gbif_species_name,
      d.hybrid_diagnostic_class,
      d.candidate_accepted_concept_id,
      d.target_wcvp_taxon_name,
      d.target_wcvp_taxon_authors,
      d.target_wcvp_taxon_rank,
      d.target_wcvp_taxon_status,
      c.diagnostic_rule,
      c.query_name,
      c.wcvp_plant_name_id,
      c.wcvp_taxon_name,
      c.wcvp_taxon_authors,
      c.wcvp_taxon_status,
      c.wcvp_accepted_plant_name_id
    FROM occurrence_wcvp_hybrid_diagnostics d
    INNER JOIN occurrence_wcvp_hybrid_candidates c
      ON d.vpjd_taxon_universe_id = c.vpjd_taxon_universe_id
    ORDER BY
      d.present_records DESC,
      d.scientificName,
      c.diagnostic_rule,
      c.wcvp_taxon_name
    "
  )
  
  # ---------------------------------------------------------------------------
  # Metadata
  # ---------------------------------------------------------------------------
  
  DBI::dbExecute(
    con,
    paste0(
      "
      CREATE TABLE occurrence_wcvp_hybrid_diagnostic_metadata AS
      SELECT
        CURRENT_TIMESTAMP AS created_at,
        '", VPJD_WCVP_HYBRID_DIAG_VERSION, "'::VARCHAR AS script_version,
        COUNT(*) AS hybrid_concepts,
        SUM(present_records) AS hybrid_present_records,
        SUM(CASE
          WHEN hybrid_diagnostic_class =
               'secure_wcvp_hybrid_resolution'
          THEN 1 ELSE 0
        END) AS secure_concepts,
        SUM(CASE
          WHEN hybrid_diagnostic_class =
               'secure_wcvp_hybrid_resolution'
          THEN present_records ELSE 0
        END) AS secure_present_records,
        SUM(CASE
          WHEN hybrid_diagnostic_class =
               'wcvp_artificial_hybrid_treatment'
          THEN 1 ELSE 0
        END) AS artificial_hybrid_concepts,
        SUM(CASE
          WHEN hybrid_diagnostic_class =
               'review_changed_accepted_concept'
          THEN 1 ELSE 0
        END) AS changed_concept_review,
        SUM(CASE
          WHEN hybrid_diagnostic_class =
               'no_wcvp_hybrid_evidence'
          THEN 1 ELSE 0
        END) AS no_evidence_concepts
      FROM occurrence_wcvp_hybrid_diagnostics
      "
    )
  )
  
  # ---------------------------------------------------------------------------
  # Exports
  # ---------------------------------------------------------------------------
  
  full_diagnostics <- DBI::dbGetQuery(
    con,
    "
    SELECT *
    FROM occurrence_wcvp_hybrid_diagnostics
    ORDER BY present_records DESC, scientificName
    "
  )
  
  exports <- list(
    hybrid_diagnostics = full_diagnostics,
    hybrid_diagnostic_summary = diagnostic_summary,
    hybrid_rule_summary = rule_summary,
    hybrid_secure_evidence = secure_evidence,
    hybrid_review_evidence = review_evidence,
    hybrid_artificial_hybrids = artificial_hybrids,
    hybrid_ambiguous_evidence = ambiguous_evidence,
    hybrid_no_evidence = no_evidence,
    hybrid_candidate_detail = candidate_detail
  )
  
  for (nm in names(exports)) {
    readr::write_csv(
      exports[[nm]],
      file.path(output_dir, paste0(nm, ".csv"))
    )
  }
  
  # ---------------------------------------------------------------------------
  # Console
  # ---------------------------------------------------------------------------
  
  secure_n <- nrow(secure_evidence)
  secure_records <- sum(secure_evidence$present_records, na.rm = TRUE)
  review_n <- nrow(review_evidence)
  artificial_n <- nrow(artificial_hybrids)
  ambiguous_n <- nrow(ambiguous_evidence)
  no_evidence_n <- nrow(no_evidence)
  
  cli::cli_h2("Hardened WCVP hybrid diagnostics complete")
  cli::cli_text(
    "{.strong Explicit hybrid concepts:} ",
    "{fmt(input_summary$concepts[[1]])}"
  )
  cli::cli_text(
    "{.strong PRESENT records represented:} ",
    "{fmt(input_summary$present_records[[1]])}"
  )
  cli::cli_text(
    "{.strong Secure hybrid resolutions:} ",
    "{fmt(secure_n)} concepts / {fmt(secure_records)} PRESENT records"
  )
  cli::cli_text(
    "{.strong Changed/nonstandard targets requiring review:} ",
    "{fmt(review_n)}"
  )
  cli::cli_text(
    "{.strong WCVP Artificial Hybrid treatments:} ",
    "{fmt(artificial_n)}"
  )
  cli::cli_text(
    "{.strong Multiple WCVP concepts:} ",
    "{fmt(ambiguous_n)}"
  )
  cli::cli_text(
    "{.strong No WCVP hybrid evidence:} ",
    "{fmt(no_evidence_n)}"
  )
  cli::cli_text(
    "{.strong Secure resolved baseline remains:} ",
    "{fmt(baseline$resolved_before_04k[[1]])}"
  )
  cli::cli_alert_success(
    "04k v0.1.1 is diagnostic only: no WCVP assignments were made."
  )
  cli::cli_alert_success(
    "Artificial Hybrid treatments are separated from ordinary accepted taxa."
  )
  cli::cli_alert_success(
    "Changed accepted concepts are retained for explicit review."
  )
  cli::cli_alert_success(
    "Validated/frozen upstream reconciliation tables were not modified."
  )
  cli::cli_text(
    "04k_occurrence_wcvp_hybrid_diagnostics.R ",
    "v{VPJD_WCVP_HYBRID_DIAG_VERSION} complete."
  )
  
  invisible(list(
    baseline = baseline,
    input_summary = input_summary,
    diagnostic_summary = diagnostic_summary,
    rule_summary = rule_summary,
    secure_evidence = secure_evidence,
    review_evidence = review_evidence,
    artificial_hybrids = artificial_hybrids,
    ambiguous_evidence = ambiguous_evidence,
    no_evidence = no_evidence,
    candidate_detail = candidate_detail
  ))
}

wcvp_hybrid_results <- run_occurrence_wcvp_hybrid_diagnostics()

hybrid_input_summary <- wcvp_hybrid_results$input_summary
hybrid_diagnostic_summary <- wcvp_hybrid_results$diagnostic_summary
hybrid_rule_summary <- wcvp_hybrid_results$rule_summary
hybrid_secure_evidence <- wcvp_hybrid_results$secure_evidence
hybrid_review_evidence <- wcvp_hybrid_results$review_evidence
hybrid_artificial_hybrids <- wcvp_hybrid_results$artificial_hybrids
hybrid_ambiguous_evidence <- wcvp_hybrid_results$ambiguous_evidence
hybrid_no_evidence <- wcvp_hybrid_results$no_evidence
hybrid_candidate_detail <- wcvp_hybrid_results$candidate_detail