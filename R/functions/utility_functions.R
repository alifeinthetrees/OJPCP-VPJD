# VPJD-OJPCP | Shared utility functions

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(readr)
  library(fs)
  library(here)
})

vpjd_dirs <- function(create = TRUE) {
  dirs <- list(
    raw_gbif   = here("data", "raw", "GBIF"),
    raw_wcvp   = here("data", "raw", "WCVP"),
    raw_ext    = here("data", "raw", "external"),
    interim    = here("data", "interim"),
    processed  = here("data", "processed"),
    tables     = here("outputs", "tables"),
    maps       = here("outputs", "maps"),
    figures    = here("outputs", "figures"),
    logs       = here("outputs", "logs")
  )
  if (create) invisible(lapply(dirs, fs::dir_create, recurse = TRUE))
  dirs
}

safe_slug <- function(x) {
  x |>
    stringr::str_squish() |>
    stringr::str_to_lower() |>
    stringr::str_replace_all("[^a-z0-9]+", "_") |>
    stringr::str_replace_all("^_|_$", "")
}

clean_binomial <- function(x) {
  x <- stringr::str_squish(as.character(x))
  stringr::str_extract(x, "^[A-Z][A-Za-z.-]+\\s+[a-z][A-Za-z.-]+")
}

normalise_star <- function(x) {
  x <- stringr::str_to_upper(stringr::str_squish(as.character(x)))
  dplyr::case_when(
    x == "BL" ~ "BU",  # legacy Blue code
    x %in% c("BK", "GD", "BU", "GN") ~ x,
    TRUE ~ NA_character_
  )
}

fmt_n <- function(x) format(x, big.mark = ",", scientific = FALSE)
