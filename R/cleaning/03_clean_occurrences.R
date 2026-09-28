# VPJD-OJPCP | Occurrence cleaning and optional coordinate QC

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(here)
})
source(here("R", "functions", "utility_functions.R"))

clean_occurrences <- function(input,
                              label,
                              coordinate_qc = FALSE,
                              deduplicate_gbif_id = TRUE) {
  dirs <- vpjd_dirs()
  slug <- safe_slug(label)
  dat <- if (is.character(input) && length(input) == 1) {
    readr::read_csv(input, show_col_types = FALSE, progress = FALSE)
  } else {
    tibble::as_tibble(input)
  }

  # Accommodate camelCase GBIF and snake_case imports.
  rename_if <- function(df, old, new) if (old %in% names(df) && !new %in% names(df)) dplyr::rename(df, !!new := all_of(old)) else df
  dat <- rename_if(dat, "decimal_latitude", "decimalLatitude")
  dat <- rename_if(dat, "decimal_longitude", "decimalLongitude")
  dat <- rename_if(dat, "scientific_name", "scientificName")
  dat <- rename_if(dat, "gbif_id", "gbifID")

  if (!all(c("decimalLatitude", "decimalLongitude") %in% names(dat))) stop("Coordinate columns not found.")
  if (!"scientificName" %in% names(dat)) stop("scientificName not found.")

  dat <- dat %>%
    mutate(
      decimalLatitude = suppressWarnings(as.numeric(decimalLatitude)),
      decimalLongitude = suppressWarnings(as.numeric(decimalLongitude)),
      scientificName = stringr::str_squish(as.character(scientificName)),
      gbif_binom = clean_binomial(scientificName)
    ) %>%
    filter(
      is.finite(decimalLatitude), is.finite(decimalLongitude),
      between(decimalLatitude, -90, 90), between(decimalLongitude, -180, 180),
      !is.na(gbif_binom)
    )

  if (deduplicate_gbif_id && "gbifID" %in% names(dat)) dat <- dat %>% distinct(gbifID, .keep_all = TRUE)

  if (coordinate_qc) {
    if (!requireNamespace("CoordinateCleaner", quietly = TRUE)) stop("Install CoordinateCleaner to use coordinate_qc = TRUE.")
    if (!"species" %in% names(dat)) dat$species <- dat$gbif_binom
    flags <- CoordinateCleaner::clean_coordinates(
      x = dat, lon = "decimalLongitude", lat = "decimalLatitude", species = "species",
      tests = c("capitals", "centroids", "equal", "gbif", "institutions", "zeros", "seas"),
      value = "flagged"
    )
    dat$coord_flag <- flags
    dat <- dat %>% filter(is.na(coord_flag) | coord_flag == FALSE)
  }

  out_csv <- file.path(dirs$interim, paste0(slug, "_clean.csv"))
  out_rds <- file.path(dirs$interim, paste0(slug, "_clean.rds"))
  readr::write_csv(dat, out_csv); saveRDS(dat, out_rds)
  message("Cleaned occurrences: ", nrow(dat), " -> ", out_csv)
  invisible(dat)
}
