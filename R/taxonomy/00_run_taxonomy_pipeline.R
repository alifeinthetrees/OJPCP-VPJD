# ==============================================================================
# VPJD-OJPCP
# 00_run_taxonomy_pipeline.R
#
# Master runner for the active occurrence-derived WCVP taxonomy pipeline.
#
# Each module currently self-executes when sourced. This runner therefore
# controls execution order but does not call module functions separately.
#
# Run from the VPJD-OJPCP project root, or source from an R session opened
# through VPJD-OJPCP.Rproj.
# ==============================================================================

suppressPackageStartupMessages({
  library(here)
})

message("")
message("============================================================")
message("VPJD-OJPCP TAXONOMY PIPELINE")
message("============================================================")
message("Project root: ", here())
message("Started: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
message("")

taxonomy_scripts <- c(
  "03a_build_occurrence_taxon_universe.R",
  "04a_occurrence_wcvp_reconcile.R",
  "04b_occurrence_wcvp_recovery.R",
  "04c_occurrence_wcvp_residual_diagnostics.R",
  "04d_occurrence_wcvp_structured_recovery.R",
  "04e_occurrence_wcvp_infraspecific_recovery.R",
  "04f_occurrence_wcvp_species_recovery.R",
  "04g_occurrence_wcvp_residual_recovery.R",
  "04h_occurrence_wcvp_name_variant_recovery.R",
  "04i_occurrence_wcvp_alternative_combination_recovery.R",
  "04j_occurrence_wcvp_nomenclatural_diagnostics.R",
  "04k_occurrence_wcvp_hybrid_diagnostics.R",
  "04l_occurrence_wcvp_hybrid_recovery.R",
  "04m_occurrence_wcvp_infraspecific_diagnostics.R",
  "04n_occurrence_wcvp_infraspecific_recovery.R",
  "04o_occurrence_wcvp_residual_species_diagnostics.R",
  "04p_occurrence_wcvp_consolidate.R"
)

taxonomy_paths <- here("R", "taxonomy", taxonomy_scripts)

missing_scripts <- taxonomy_scripts[!file.exists(taxonomy_paths)]

if (length(missing_scripts) > 0) {
  stop(
    "Taxonomy pipeline cannot start. Missing script(s):\n",
    paste(" -", missing_scripts, collapse = "\n")
  )
}

for (i in seq_along(taxonomy_scripts)) {

  script <- taxonomy_scripts[[i]]
  path   <- taxonomy_paths[[i]]

  message("")
  message("------------------------------------------------------------")
  message(
    sprintf(
      "[%02d/%02d] %s",
      i,
      length(taxonomy_scripts),
      script
    )
  )
  message("------------------------------------------------------------")

  stage_start <- Sys.time()

  tryCatch(
    {
      source(path, local = .GlobalEnv, echo = FALSE)
    },
    error = function(e) {
      stop(
        "\nTaxonomy pipeline failed in:\n",
        script,
        "\n\nError:\n",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )

  stage_elapsed <- difftime(
    Sys.time(),
    stage_start,
    units = "secs"
  )

  message(
    sprintf(
      "Completed %s in %.1f seconds.",
      script,
      as.numeric(stage_elapsed)
    )
  )
}

message("")
message("============================================================")
message("VPJD-OJPCP TAXONOMY PIPELINE COMPLETE")
message("============================================================")
message("Completed: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
message("")
