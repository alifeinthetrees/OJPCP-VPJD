# VPJD-OJPCP | Master runner
# Open VPJD-OJPCP.Rproj before running. No setwd() is required.

suppressPackageStartupMessages({
  library(here)
})

source(here("R", "functions", "utility_functions.R"))
source(here("R", "acquisition", "01_gbif_acquire.R"))
source(here("R", "acquisition", "02_dbf_import.R"))
source(here("R", "cleaning", "03_clean_occurrences.R"))
source(here("R", "taxonomy", "04_wcvp_standardise.R"))
source(here("R", "stars", "05_star_lookup.R"))
source(here("R", "bioquality", "06_ghi.R"))
source(here("R", "spatial", "07_distribution_maps.R"))
source(here("R", "audit", "08_audit_dataset.R"))

vpjd_dirs(create = TRUE)
message("VPJD-OJPCP project root: ", here())

run_vpjd_gbif <- function(label,
                          dataset_key = NULL,
                          taxon = NULL,
                          taxon_key = NULL,
                          country = NULL,
                          institution_key = NULL,
                          basis_of_record = NULL,
                          coordinate_qc = FALSE,
                          star_path = here("data", "raw", "external", "FOJ_STARS.csv"),
                          run_ghi = TRUE,
                          resolutions = c(1, 0.5, 0.0625),
                          min_species = 1) {
  acq <- acquire_gbif(label, dataset_key, taxon, taxon_key, country, institution_key, basis_of_record)
  clean <- clean_occurrences(acq$data, label, coordinate_qc = coordinate_qc)
  wcvp <- standardise_wcvp(clean, label)
  starred <- add_star_ratings(wcvp, label, star_path = star_path)
  audit_dataset(starred, label)
  p_occ <- map_occurrences(starred, label)

  cells <- NULL; p_ghi <- NULL
  if (run_ghi) {
    cells <- run_ghi_analysis(starred, label, resolutions = resolutions, min_species = min_species)
    p_ghi <- map_ghi(cells, label, resolution = min(resolutions))
  }
  invisible(list(acquisition = acq, clean = clean, wcvp = wcvp, starred = starred, cells = cells, occurrence_map = p_occ, ghi_map = p_ghi))
}

# Single-taxon example:
# result <- run_vpjd_gbif("Betula chichibuensis", taxon = "Betula chichibuensis", run_ghi = FALSE)
#
# Dataset example:
# result <- run_vpjd_gbif("Nagano GBIF", dataset_key = "YOUR-DATASET-UUID")
