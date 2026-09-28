# VPJD-OJPCP | Distribution and GHI maps

suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(maps)
  library(here)
})
source(here("R", "functions", "utility_functions.R"))

japan_basemap <- function() maps::map_data("world") %>% filter(region == "Japan")

map_occurrences <- function(input, label, species_col = "wcvp_accepted_name",
                            xlim = c(122, 153), ylim = c(24, 46), save = TRUE) {
  dirs <- vpjd_dirs(); slug <- safe_slug(label)
  dat <- if (is.character(input) && length(input) == 1) readRDS(input) else tibble::as_tibble(input)
  p <- ggplot() +
    geom_polygon(data = japan_basemap(), aes(long, lat, group = group), fill = "grey95", colour = "grey65", linewidth = 0.25) +
    geom_point(data = dat %>% filter(!is.na(decimalLongitude), !is.na(decimalLatitude)),
               aes(decimalLongitude, decimalLatitude), alpha = 0.45, size = 0.6) +
    coord_quickmap(xlim = xlim, ylim = ylim) + theme_minimal(base_size = 11) +
    labs(title = paste0(label, " — occurrence distribution"), x = "Longitude", y = "Latitude")
  if (save) ggsave(file.path(dirs$maps, paste0(slug, "_distribution.png")), p, width = 8, height = 7, dpi = 300)
  p
}

map_single_taxon <- function(input, taxon, label = taxon, species_col = "wcvp_accepted_name", save = TRUE) {
  dat <- if (is.character(input) && length(input) == 1) readRDS(input) else tibble::as_tibble(input)
  key <- clean_binomial(taxon)
  dat <- dat %>% filter(clean_binomial(.data[[species_col]]) == key)
  if (!nrow(dat)) stop("No records found for: ", taxon)
  map_occurrences(dat, label = label, species_col = species_col, save = save)
}

map_ghi <- function(cells, label, resolution = 0.0625,
                    xlim = c(122, 153), ylim = c(24, 46), save = TRUE) {
  dirs <- vpjd_dirs(); slug <- safe_slug(label)
  d <- cells %>% filter(abs(grid_res - resolution) < 1e-10, !is.na(ghi))
  p <- ggplot() +
    geom_polygon(data = japan_basemap(), aes(long, lat, group = group), fill = "grey95", colour = "grey70", linewidth = 0.2) +
    geom_tile(data = d, aes(lon_center, lat_center, fill = ghi), width = resolution, height = resolution) +
    coord_quickmap(xlim = xlim, ylim = ylim) + scale_fill_viridis_c() + theme_minimal(base_size = 11) +
    labs(title = paste0(label, " — Genetic Heat Index"), subtitle = paste0("Grid resolution: ", resolution, "°"), fill = "GHI", x = "Longitude", y = "Latitude")
  if (save) ggsave(file.path(dirs$maps, paste0(slug, "_ghi_", gsub("\\.", "_", resolution), "deg.png")), p, width = 8, height = 7, dpi = 300)
  p
}
