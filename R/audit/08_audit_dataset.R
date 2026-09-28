# VPJD-OJPCP | Dataset audit

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(here)
})
source(here("R", "functions", "utility_functions.R"))

audit_dataset <- function(input, label, species_col = "wcvp_accepted_name") {
  dirs <- vpjd_dirs(); slug <- safe_slug(label)
  dat <- if (is.character(input) && length(input) == 1) readRDS(input) else tibble::as_tibble(input)
  has <- function(x) x %in% names(dat)
  n <- nrow(dat)
  audit <- tibble::tibble(
    label = label,
    records = n,
    unique_species = if (has(species_col)) n_distinct(dat[[species_col]], na.rm = TRUE) else NA_integer_,
    unique_gbif_ids = if (has("gbifID")) n_distinct(dat$gbifID, na.rm = TRUE) else NA_integer_,
    records_with_coordinates = if (all(c("decimalLatitude", "decimalLongitude") %in% names(dat))) sum(is.finite(dat$decimalLatitude) & is.finite(dat$decimalLongitude)) else NA_integer_,
    records_wcvp_matched = if (has("wcvp_accepted_name")) sum(!is.na(dat$wcvp_accepted_name)) else NA_integer_,
    records_with_star = if (has("Star")) sum(!is.na(normalise_star(dat$Star))) else NA_integer_,
    unique_star_species = if (has("Star") && has(species_col)) nrow(dat %>% filter(!is.na(normalise_star(Star))) %>% distinct(.data[[species_col]])) else NA_integer_
  )
  readr::write_csv(audit, file.path(dirs$tables, paste0(slug, "_audit.csv")))
  print(audit)
  invisible(audit)
}
