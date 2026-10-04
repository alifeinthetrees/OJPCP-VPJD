# Reproducibility

## Vascular Plants of Japan Database (VPJD)

This document describes the computational environment, source-data requirements,
workflow structure and validation framework associated with the published
Vascular Plants of Japan Database (VPJD) v1.0.0 data products.

VPJD is developed within the Oxford–Japan Plant Conservation Partnership
(OJPCP) using version-controlled R workflows.

Two versioned VPJD data products are currently published:

- **VPJD Taxonomic Release v1.0.0**  
  DOI: https://doi.org/10.5281/zenodo.23017356

- **VPJD Geography Release v1.0.0**  
  DOI: https://doi.org/10.5281/zenodo.23111498

The Zenodo deposits are the authoritative, frozen research datasets. This
GitHub repository contains the computational workflows and development history
used to construct, validate and extend VPJD.

---

## 1. Scope

VPJD separates its analytical architecture into distinct layers:

1. taxonomic reconciliation and construction of the recognised-taxon backbone;
2. occurrence-data integration and validation;
3. geographic standardisation and taxon-by-area evidence;
4. downstream analyses of geographic range, rarity and conservation
   significance.

The published Taxonomic and Geography v1.0.0 releases represent frozen data
products within this wider analytical framework.

The repository continues to develop beyond these releases. Consequently, the
current state of the `main` branch should not be assumed to represent a
single-command reconstruction of either frozen v1.0.0 release.

In particular, `R/00_run_pipeline.R` is the master runner for the newer
occurrence-first development architecture. It is **not** the reproduction
entry point for the published Taxonomic Release v1.0.0 or Geography Release
v1.0.0.

---

## 2. Software environment

VPJD is developed in R.

The repository uses `renv` to record and restore the R package environment.

The project should be opened using:

`VPJD-OJPCP.Rproj`

The repository contains:

- `.Rprofile`
- `renv/`
- `renv.lock`

The `.Rprofile` activates the project `renv` environment automatically.

From a clean clone, the recorded package environment can be restored in R
using:

```r
install.packages("renv")
renv::restore()
