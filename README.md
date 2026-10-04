# Vascular Plants of Japan Database (VPJD)

The **Vascular Plants of Japan Database (VPJD)** is a data and analytical
infrastructure project of the **Oxford–Japan Plant Conservation Partnership
(OJPCP)**.

VPJD integrates taxonomic, occurrence and geographic evidence for the vascular
flora of Japan within reproducible R workflows. The project provides a
WCVP-aligned taxonomic backbone and a separately versioned geographic evidence
framework to support floristic, biogeographic and conservation research.

## Published releases

### Taxonomic Release v1.0.0

**DOI:** https://doi.org/10.5281/zenodo.23017356

The Taxonomic Release provides the frozen recognised-taxon backbone for VPJD,
aligned to the World Checklist of Vascular Plants (WCVP).

It contains **12,037 recognised vascular plant taxa**, with relationships to
the source taxonomic concepts from which VPJD was constructed.

### Geography Release v1.0.0

**DOI:** https://doi.org/10.5281/zenodo.23111498

The Geography Release provides the geographic evidence framework associated
with the VPJD taxonomic backbone.

It contains:

- 12,037 recognised vascular plant taxa
- 418 canonical geographic concepts
- 51 Japanese botanical areas
- 367 WCVP/TDWG reference concepts
- 128,824 positive taxon × botanical-area relationships
- 11,470 taxa with positive geographic evidence
- 567 taxa currently without positive geographic evidence

The release distinguishes positive geographic evidence from biological
absence. A taxon lacking a positive relationship to an area must therefore
not be interpreted automatically as absent from that area.

## VPJD data architecture

VPJD separates three related analytical layers:

1. **Taxonomy**  
   A reproducible, WCVP-aligned recognised-taxon backbone.

2. **Geographic evidence**  
   Canonical geographic concepts and reproducible positive taxon-by-area
   distribution evidence.

3. **Conservation analysis**  
   Downstream interpretation of geographic range, endemicity, rarity and
   conservation significance.

This separation allows the underlying taxonomic and geographic evidence to be
versioned, validated and cited independently of subsequent conservation
interpretation.

## Current development

With the Taxonomic Release v1.0.0 and Geography Release v1.0.0 established,
development is progressing towards analysis of geographic range and application
of the **Nakamura Key to Stars for the Flora of Japan**.

The published Geography Release does **not** itself assign Star categories or
treat absence of positive evidence as demonstrated biological absence.

Subsequent analyses will use the taxonomic and geographic evidence layers to
support assessment of geographic rarity and conservation significance,
including plant-focused Key Biodiversity Area (KBA) research.

## Repository structure

`R/taxonomy/`  
Taxonomic reconciliation, validation and construction of the VPJD recognised
taxonomic backbone.

`R/Geography/`  
Geographic-source auditing, distribution reconstruction, geographic vocabulary
construction, taxon-by-area evidence, validation and release workflows.

`R/taxonomic_revision/`  
Post-release investigation and development relating to the VPJD taxonomic
backbone.

`Scripts/01_ingest/`  
Source-data ingestion and legacy-data audit workflows.

`renv/` and `renv.lock`  
R dependency management supporting reproducibility.

Generated analytical products, audit outputs and release packages are not
intended to replace the authoritative versioned datasets deposited in Zenodo.

## Reproducibility and data releases

VPJD uses explicit validation and release gates, provenance documentation,
release manifests and SHA-256 checksums to support reproducibility and data
integrity.

### Taxonomic reconciliation

The staged procedure used to reconcile occurrence-derived source concepts
against WCVP and construct the VPJD v1.0.0 recognised taxonomic backbone is
documented in [`docs/TAXONOMIC_RECONCILIATION.md`](docs/TAXONOMIC_RECONCILIATION.md).

The GitHub repository contains the analytical workflows and development
history. Frozen, citable research datasets are published separately through
Zenodo.

### Data and project links

**VPJD Zenodo Community**  
https://zenodo.org/communities/vpjd/

**VPJD Taxonomic Release v1.0.0**  
https://doi.org/10.5281/zenodo.23017356

**VPJD Geography Release v1.0.0**  
https://doi.org/10.5281/zenodo.23111498

## Project

**Oxford–Japan Plant Conservation Partnership (OJPCP)**  
University of Oxford Botanic Garden and Arboretum

**Ben Jones**  
ORCID: https://orcid.org/0000-0003-1430-004X

## Citation

Please cite the relevant version-specific Zenodo release when using VPJD data.
Where both taxonomic and geographic data are used, cite both corresponding
releases.
