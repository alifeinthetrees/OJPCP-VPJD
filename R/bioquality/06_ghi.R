# VPJD-OJPCP | Canonical GHI / Bioquality functions
# GHI = 100 * (27*BK + 9*GD + 3*BU + 0*GN) / N

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(here)
})
source(here("R", "functions", "utility_functions.R"))

GHI_COEFFICIENTS <- c(BK = 27, GD = 9, BU = 3, GN = 0)

star_frequencies <- function(stars) {
  stars <- normalise_star(stars)
  out <- table(factor(stars, levels = names(GHI_COEFFICIENTS)))
  setNames(as.numeric(out), names(GHI_COEFFICIENTS))
}

ghi_score <- function(stars = NULL, frequencies = NULL) {
  if (is.null(frequencies)) frequencies <- star_frequencies(stars)
  n <- sum(frequencies)
  if (!n) return(NA_real_)
  100 * sum(frequencies[names(GHI_COEFFICIENTS)] * GHI_COEFFICIENTS) / n
}

assign_grid <- function(df, res = 0.0625) {
  df %>% mutate(
    grid_res = res,
    grid_lon = floor(decimalLongitude / res) * res,
    grid_lat = floor(decimalLatitude / res) * res,
    lon_center = grid_lon + res / 2,
    lat_center = grid_lat + res / 2,
    grid_id = paste(grid_lat, grid_lon, sep = "_")
  )
}

summarise_ghi_cells <- function(df, res = 0.0625, species_col = "wcvp_accepted_name", star_col = "Star") {
  d <- assign_grid(df, res) %>%
    mutate(.species = .data[[species_col]], .star = normalise_star(.data[[star_col]])) %>%
    filter(!is.na(.species)) %>% distinct(grid_id, grid_lon, grid_lat, lon_center, lat_center, .species, .star)

  d %>% group_by(grid_id, grid_lon, grid_lat, lon_center, lat_center) %>% summarise(
    n_unique_species = n_distinct(.species),
    nBK = n_distinct(.species[.star == "BK"]),
    nGD = n_distinct(.species[.star == "GD"]),
    nBU = n_distinct(.species[.star == "BU"]),
    nGN = n_distinct(.species[.star == "GN"]),
    n_star_total = nBK + nGD + nBU + nGN,
    ghi = ifelse(n_star_total > 0, 100 * (27*nBK + 9*nGD + 3*nBU) / n_star_total, NA_real_),
    grid_res = res, .groups = "drop"
  )
}

run_ghi_analysis <- function(input, label, resolutions = c(1, 0.5, 0.0625), min_species = 1,
                             species_col = "wcvp_accepted_name", star_col = "Star") {
  dirs <- vpjd_dirs(); slug <- safe_slug(label)
  dat <- if (is.character(input) && length(input) == 1) readRDS(input) else tibble::as_tibble(input)
  cells <- bind_rows(lapply(resolutions, function(r) summarise_ghi_cells(dat, r, species_col, star_col)))
  cells <- cells %>% mutate(retained = n_unique_species >= min_species)
  readr::write_csv(cells, file.path(dirs$tables, paste0(slug, "_ghi_cells_all_resolutions.csv")))
  saveRDS(cells, file.path(dirs$processed, paste0(slug, "_ghi_cells.rds")))
  invisible(cells)
}
