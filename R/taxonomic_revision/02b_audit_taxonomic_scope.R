# ==============================================================================
# VPJD / OJPCP
# 02b — Audit complete taxonomic scope
#
# File:
#   R/taxonomic_revision/02b_audit_taxonomic_scope.R
#
# Version: 0.2.0
#
# PURPOSE
# -------
# Positive taxonomic-scope audit of the contemporary VPJD population.
#
# This module:
#   1. reads the canonical 11,439-concept contemporary VPJD taxonomy;
#   2. verifies the completed 02a non-vascular audit;
#   3. classifies all represented families into major vascular-plant lineages;
#   4. propagates family-level classification to every VPJD concept;
#   5. requires complete classification of all 260 represented families;
#   6. produces family-, lineage-, rank- and Star-level profiles;
#   7. writes audit tables and CSV outputs.
#
# IMPORTANT
# ---------
# This is an AUDIT module.
#
# It does NOT:
#   * delete taxa;
#   * alter the canonical 11,439-concept population;
#   * change WCVP reconciliation;
#   * change provisional Star allocations;
#   * automatically remove any taxon.
#
# Major scope groups:
#   ANGIOSPERM
#   GYMNOSPERM
#   FERN
#   LYCOPHYTE
#   NON_VASCULAR
#
# Canonical input:
#   vpjd_star_provisional_taxonomic_composition
#
# Required hand-off:
#   vpjd_taxrev_02a_non_vascular_audit
#
# Expected population:
#   11,439 accepted concepts
#
# Expected families:
#   260
# ==============================================================================


# ==============================================================================
# 00. PACKAGES
# ==============================================================================

required_packages <- c(
  "DBI",
  "duckdb",
  "dplyr",
  "readr",
  "stringr",
  "tibble"
)

missing_packages <- required_packages[
  !vapply(
    required_packages,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )
]

if (length(missing_packages) > 0L) {
  stop(
    "Missing required packages: ",
    paste(missing_packages, collapse = ", ")
  )
}

library(DBI)
library(duckdb)
library(dplyr)
library(readr)
library(stringr)
library(tibble)


# ==============================================================================
# 01. CONFIGURATION
# ==============================================================================

MODULE <- "02b_audit_taxonomic_scope"
VERSION <- "0.2.0"
RUN_DATE <- Sys.Date()

PROJECT_ROOT <- "I:/R/OJPCP/VPJD-OJPCP"

DB_PATH <- file.path(
  PROJECT_ROOT,
  "data",
  "interim",
  "occurrences",
  "vpjd_occurrences.duckdb"
)

OUTPUT_DIR <- file.path(
  PROJECT_ROOT,
  "outputs",
  "tables",
  "taxonomic_revision",
  "02b_taxonomic_scope"
)

dir.create(
  OUTPUT_DIR,
  recursive = TRUE,
  showWarnings = FALSE
)

EXPECTED_POPULATION <- 11439L
EXPECTED_FAMILIES <- 260L

TABLE_INPUT <-
  "vpjd_star_provisional_taxonomic_composition"

TABLE_02A <-
  "vpjd_taxrev_02a_non_vascular_audit"

TABLE_AUDIT <-
  "vpjd_taxrev_02b_taxonomic_scope_audit"

TABLE_FAMILY_CROSSWALK <-
  "vpjd_taxrev_02b_family_lineage_crosswalk"

TABLE_LINEAGE_PROFILE <-
  "vpjd_taxrev_02b_lineage_profile"

TABLE_UNRESOLVED <-
  "vpjd_taxrev_02b_unresolved_taxa"

TABLE_VALIDATION <-
  "vpjd_taxrev_02b_validation"


# ==============================================================================
# 02. CONNECTION SAFETY
# ==============================================================================

if (exists("con", envir = .GlobalEnv)) {
  stop(
    paste0(
      "An object named 'con' already exists in this R session.\n\n",
      "Restart R before running 02b:\n",
      "Session > Restart R\n",
      "or Ctrl + Shift + F10\n\n",
      "Then run 02b from the beginning."
    )
  )
}

if (!file.exists(DB_PATH)) {
  stop(
    "Canonical VPJD DuckDB not found:\n",
    DB_PATH
  )
}


# ==============================================================================
# 03. START REPORT
# ==============================================================================

cat("\n")
cat("============================================================\n")
cat("VPJD TAXONOMIC REVISION — 02b TAXONOMIC SCOPE AUDIT\n")
cat("============================================================\n\n")

cat("Run date: ", RUN_DATE, "\n", sep = "")
cat("Module: ", MODULE, "\n", sep = "")
cat("Version: ", VERSION, "\n", sep = "")
cat("Database: ", DB_PATH, "\n\n", sep = "")


# ==============================================================================
# 04. CONNECT
# ==============================================================================

con <- DBI::dbConnect(
  duckdb::duckdb(),
  dbdir = DB_PATH,
  read_only = FALSE
)

if (!DBI::dbIsValid(con)) {
  stop("DuckDB connection is not valid.")
}

cat(
  "Connection class: ",
  paste(class(con), collapse = ", "),
  "\n",
  sep = ""
)

cat("Connection valid: TRUE\n\n")


# ==============================================================================
# 05. VERIFY INPUT TABLES
# ==============================================================================

db_tables <- DBI::dbListTables(con)

required_tables <- c(
  TABLE_INPUT,
  TABLE_02A
)

missing_tables <- setdiff(
  required_tables,
  db_tables
)

if (length(missing_tables) > 0L) {
  DBI::dbDisconnect(con, shutdown = TRUE)
  rm(con)
  
  stop(
    "Required upstream table(s) missing:\n",
    paste(missing_tables, collapse = "\n")
  )
}

cat("Required upstream tables found:\n")
cat(
  paste0("  ", required_tables),
  sep = "\n"
)
cat("\n\n")


# ==============================================================================
# 06. READ CANONICAL TAXONOMY
# ==============================================================================

taxonomy <- DBI::dbReadTable(
  con,
  TABLE_INPUT
) %>%
  as_tibble()

if (nrow(taxonomy) != EXPECTED_POPULATION) {
  DBI::dbDisconnect(con, shutdown = TRUE)
  rm(con)
  
  stop(
    "Expected ",
    format(EXPECTED_POPULATION, big.mark = ","),
    " canonical concepts; found ",
    format(nrow(taxonomy), big.mark = ","),
    "."
  )
}

required_fields <- c(
  "FINAL_WCVP_ID",
  "FINAL_WCVP_RECOGNISED_NAME",
  "FINAL_WCVP_RANK",
  "FINAL_WCVP_STATUS",
  "wcvp_taxon_name",
  "wcvp_taxon_rank",
  "wcvp_family",
  "wcvp_genus",
  "wcvp_species",
  "PROVISIONAL_STAR"
)

missing_fields <- setdiff(
  required_fields,
  names(taxonomy)
)

if (length(missing_fields) > 0L) {
  DBI::dbDisconnect(con, shutdown = TRUE)
  rm(con)
  
  stop(
    "Required canonical fields missing:\n",
    paste(missing_fields, collapse = ", ")
  )
}


# ==============================================================================
# 07. VERIFY 02a HAND-OFF
# ==============================================================================

audit_02a <- DBI::dbReadTable(
  con,
  TABLE_02A
) %>%
  as_tibble()

if (nrow(audit_02a) != EXPECTED_POPULATION) {
  DBI::dbDisconnect(con, shutdown = TRUE)
  rm(con)
  
  stop("02a hand-off population is not 11,439.")
}

if (
  "IS_KNOWN_NON_VASCULAR_FAMILY" %in%
  names(audit_02a)
) {
  
  n_02a_non_vascular <- sum(
    audit_02a$IS_KNOWN_NON_VASCULAR_FAMILY,
    na.rm = TRUE
  )
  
} else {
  
  DBI::dbDisconnect(con, shutdown = TRUE)
  rm(con)
  
  stop(
    "02a hand-off does not contain ",
    "IS_KNOWN_NON_VASCULAR_FAMILY."
  )
}

cat(
  "02a population: ",
  format(nrow(audit_02a), big.mark = ","),
  "\n",
  sep = ""
)

cat(
  "02a non-vascular candidates: ",
  format(n_02a_non_vascular, big.mark = ","),
  "\n\n",
  sep = ""
)


# ==============================================================================
# 08. STANDARDISE CANONICAL TAXONOMY
# ==============================================================================

taxonomy <- taxonomy %>%
  mutate(
    
    FINAL_WCVP_ID =
      as.character(FINAL_WCVP_ID),
    
    TAXON_NAME =
      coalesce(
        as.character(wcvp_taxon_name),
        as.character(FINAL_WCVP_RECOGNISED_NAME)
      ),
    
    TAXON_RANK =
      toupper(
        trimws(
          coalesce(
            as.character(wcvp_taxon_rank),
            as.character(FINAL_WCVP_RANK),
            ""
          )
        )
      ),
    
    FAMILY =
      toupper(
        trimws(
          coalesce(
            as.character(wcvp_family),
            ""
          )
        )
      ),
    
    GENUS =
      trimws(
        coalesce(
          as.character(wcvp_genus),
          ""
        )
      ),
    
    SPECIES =
      trimws(
        coalesce(
          as.character(wcvp_species),
          ""
        )
      )
  )


# ==============================================================================
# 09. FAMILY INVENTORY
# ==============================================================================

family_inventory <- taxonomy %>%
  group_by(FAMILY) %>%
  summarise(
    
    N_TAXA = n(),
    
    N_GENERA =
      n_distinct(
        GENUS[
          !is.na(GENUS) &
            GENUS != ""
        ]
      ),
    
    .groups = "drop"
  ) %>%
  arrange(FAMILY)

n_families <- nrow(family_inventory)

cat(
  "Canonical families represented: ",
  format(n_families, big.mark = ","),
  "\n\n",
  sep = ""
)

if (n_families != EXPECTED_FAMILIES) {
  DBI::dbDisconnect(con, shutdown = TRUE)
  rm(con)
  
  stop(
    "Expected 260 represented families; found ",
    n_families,
    "."
  )
}


# ==============================================================================
# 10. EXPLICIT MAJOR-LINEAGE FAMILY REGISTERS
# ==============================================================================


# ------------------------------------------------------------------------------
# 10a. GYMNOSPERMS
# ------------------------------------------------------------------------------

gymnosperm_families <- c(
  "ARAUCARIACEAE",
  "CEPHALOTAXACEAE",
  "CUPRESSACEAE",
  "CYCADACEAE",
  "EPHEDRACEAE",
  "GINKGOACEAE",
  "GNETACEAE",
  "PINACEAE",
  "PODOCARPACEAE",
  "SCIADOPITYACEAE",
  "STANGERIACEAE",
  "TAXACEAE",
  "WELWITSCHIACEAE",
  "ZAMIACEAE"
)


# ------------------------------------------------------------------------------
# 10b. LYCOPHYTES
# ------------------------------------------------------------------------------

lycophyte_families <- c(
  "ISOETACEAE",
  "LYCOPODIACEAE",
  "SELAGINELLACEAE"
)


# ------------------------------------------------------------------------------
# 10c. FERNS
# ------------------------------------------------------------------------------

fern_families <- c(
  "ANEMIACEAE",
  "ASPLENIACEAE",
  "ATHYRIACEAE",
  "BLECHNACEAE",
  "CYATHEACEAE",
  "CYSTOPTERIDACEAE",
  "DAVALLIACEAE",
  "DENNSTAEDTIACEAE",
  "DESMOPHLEBIACEAE",
  "DICKSONIACEAE",
  "DIPTERIDACEAE",
  "DRYOPTERIDACEAE",
  "EQUISETACEAE",
  "GLEICHENIACEAE",
  "HYMENOPHYLLACEAE",
  
  # Added v0.2.0
  "HYPODEMATIACEAE",
  
  "LINDSAEACEAE",
  "LOMARIOPSIDACEAE",
  "LYGODIACEAE",
  "MARATTIACEAE",
  "MARSILEACEAE",
  "MATONIACEAE",
  "NEPHROLEPIDACEAE",
  "ONOCLEACEAE",
  "OPHIOGLOSSACEAE",
  "OSMUNDACEAE",
  "PLAGIOGYRIACEAE",
  "POLYPODIACEAE",
  "PSILOTACEAE",
  "PTERIDACEAE",
  "RHACHIDOSORACEAE",
  "SALVINIACEAE",
  "SCHIZAEACEAE",
  "TECTARIACEAE",
  "THELYPTERIDACEAE",
  "WOODSIACEAE"
)


# ------------------------------------------------------------------------------
# 10d. NON-VASCULAR
#
# Retained as a defensive check even though 02a found zero candidates.
# ------------------------------------------------------------------------------

non_vascular_families <- c(
  
  "ANTHOCEROTACEAE",
  "DENDROCEROTACEAE",
  "NOTOTHYLADACEAE",
  
  "ACROBOLBACEAE",
  "ADELANTHACEAE",
  "ANEURACEAE",
  "ANTHELIACEAE",
  "ARNELLIACEAE",
  "AYTONIACEAE",
  "BALANTIOPSIDACEAE",
  "BLASIACEAE",
  "CALYPOGEIACEAE",
  "CEPHALOZIACEAE",
  "CEPHALOZIELLACEAE",
  "CLEVEACEAE",
  "CONOCEPHALACEAE",
  "CORSINIACEAE",
  "FOSSOMBRONIACEAE",
  "FRULLANIACEAE",
  "GEOCALYCACEAE",
  "GYMNOMITRIACEAE",
  "HAPLOMITRIACEAE",
  "HERBERTACEAE",
  "JUBULACEAE",
  "JUNGERMANNIACEAE",
  "LEJEUNEACEAE",
  "LEPICOLEACEAE",
  "LEPIDOZIACEAE",
  "LOPHOCOLEACEAE",
  "LUNULARIACEAE",
  "MARCHANTIACEAE",
  "MARSUPELLACEAE",
  "METZGERIACEAE",
  "PALLAVICINIACEAE",
  "PELLIACEAE",
  "PLAGIOCHILACEAE",
  "PORELLACEAE",
  "RADULACEAE",
  "RICCIACEAE",
  "SCAPANIACEAE",
  "SOLENOSTOMATACEAE",
  "TARGIONIACEAE",
  "TRICHOCOLEACEAE",
  
  "AMBLYSTEGIACEAE",
  "ANDREAEACEAE",
  "ANDREAEOBRYACEAE",
  "ARCHIDIACEAE",
  "BARTRAMIACEAE",
  "BRACHYTHECIACEAE",
  "BRYACEAE",
  "BUXBAUMIACEAE",
  "CALYMPERACEAE",
  "CLIMACIACEAE",
  "CRYPHAEACEAE",
  "DALTONIACEAE",
  "DICRANACEAE",
  "DITRICHACEAE",
  "ENCALYPTACEAE",
  "ENTODONTACEAE",
  "ERPODIACEAE",
  "FABRONIACEAE",
  "FISSIDENTACEAE",
  "FONTINALACEAE",
  "FUNARIACEAE",
  "GRIMMIACEAE",
  "HEDWIGIACEAE",
  "HOOKERIACEAE",
  "HYLOCOMIACEAE",
  "HYPNACEAE",
  "HYPNODENDRACEAE",
  "LEPTODONTACEAE",
  "LEUCOBRYACEAE",
  "LEUCODONTACEAE",
  "MEESIACEAE",
  "METEORIACEAE",
  "MNIACEAE",
  "NECKERACEAE",
  "ORTHOTRICHACEAE",
  "PHYLLOGONIACEAE",
  "PILOTRICHACEAE",
  "PLAGIOTHECIACEAE",
  "POLYTRICHACEAE",
  "POTTIACEAE",
  "PTEROBRYACEAE",
  "PTYCHOMITRIACEAE",
  "RACOPILACEAE",
  "RHIZOGONIACEAE",
  "RHYTIDIACEAE",
  "SEMATOPHYLLACEAE",
  "SPHAGNACEAE",
  "SPLACHNACEAE",
  "THUIDIACEAE"
)


# ==============================================================================
# 11. ANGIOSPERM FAMILY REGISTER
#
# v0.2.0:
# Explicitly includes the 65 angiosperm families left unresolved by v0.1.0.
# No residual "everything else is an angiosperm" inference is permitted.
# ==============================================================================

angiosperm_families <- c(
  
  "ACANTHACEAE",
  "ACORACEAE",
  "ACTINIDIACEAE",
  "ADOXACEAE",
  "AIZOACEAE",
  "ALISMATACEAE",
  "ALLIACEAE",
  "ALSTROEMERIACEAE",
  "ALTINGIACEAE",
  "AMARANTHACEAE",
  "AMARYLLIDACEAE",
  "ANACARDIACEAE",
  "ANCISTROCLADACEAE",
  "ANNONACEAE",
  "APIACEAE",
  "APOCYNACEAE",
  "APONOGETONACEAE",
  "AQUIFOLIACEAE",
  "ARACEAE",
  "ARALIACEAE",
  "ARECACEAE",
  "ARISTOLOCHIACEAE",
  "ASPARAGACEAE",
  "ASPHODELACEAE",
  "ASTERACEAE",
  
  "BALANOPHORACEAE",
  "BALSAMINACEAE",
  "BASELLACEAE",
  "BEGONIACEAE",
  "BERBERIDACEAE",
  "BETULACEAE",
  "BIGNONIACEAE",
  "BIXACEAE",
  "BORAGINACEAE",
  "BRASSICACEAE",
  "BROMELIACEAE",
  "BURMANNIACEAE",
  "BURSERACEAE",
  "BUXACEAE",
  
  "CABOMBACEAE",
  "CACTACEAE",
  "CALOPHYLLACEAE",
  "CALYCANTHACEAE",
  "CAMPANULACEAE",
  "CANNABACEAE",
  "CANNACEAE",
  "CAPPARACEAE",
  "CAPRIFOLIACEAE",
  "CARICACEAE",
  "CARYOPHYLLACEAE",
  "CASUARINACEAE",
  "CELASTRACEAE",
  "CERATOPHYLLACEAE",
  "CERCIDIPHYLLACEAE",
  "CHLORANTHACEAE",
  "CHRYSOBALANACEAE",
  "CISTACEAE",
  "CLEOMACEAE",
  "CLETHRACEAE",
  "CLUSIACEAE",
  "COLCHICACEAE",
  "COMBRETACEAE",
  "COMMELINACEAE",
  "CONVOLVULACEAE",
  "CORIARIACEAE",
  "CORNACEAE",
  "COSTACEAE",
  "CRASSULACEAE",
  "CRYPTERONIACEAE",
  "CUCURBITACEAE",
  "CUNONIACEAE",
  "CYMODOCEACEAE",
  "CYPERACEAE",
  
  "DAPHNIPHYLLACEAE",
  "DIAPENSIACEAE",
  "DILLENIACEAE",
  "DIOSCOREACEAE",
  "DIPENTODONTACEAE",
  "DROSERACEAE",
  
  "EBENACEAE",
  "ELAEAGNACEAE",
  "ELAEOCARPACEAE",
  "ELATINACEAE",
  "ERICACEAE",
  "ERIOCAULACEAE",
  "ERYTHROXYLACEAE",
  "ESCALLONIACEAE",
  "EUCOMMIACEAE",
  "EUPHORBIACEAE",
  "EUPTELEACEAE",
  
  "FABACEAE",
  "FAGACEAE",
  "FLAGELLARIACEAE",
  
  "GARRYACEAE",
  "GELSEMIACEAE",
  "GENTIANACEAE",
  "GERANIACEAE",
  "GESNERIACEAE",
  "GOODENIACEAE",
  "GROSSULARIACEAE",
  
  "HALORAGACEAE",
  "HAMAMELIDACEAE",
  "HELICONIACEAE",
  "HELWINGIACEAE",
  "HEMEROCALLIDACEAE",
  "HERNANDIACEAE",
  "HYDRANGEACEAE",
  "HYDROCHARITACEAE",
  "HYDROLEACEAE",
  "HYDROSTACHYACEAE",
  "HYPERICACEAE",
  "HYPOXIDACEAE",
  
  "ICACINACEAE",
  "IRIDACEAE",
  "ITEACEAE",
  
  "JUGLANDACEAE",
  "JUNCACEAE",
  "JUNCAGINACEAE",
  
  "LAMIACEAE",
  "LARDIZABALACEAE",
  "LAURACEAE",
  "LECYTHIDACEAE",
  "LENTIBULARIACEAE",
  "LILIACEAE",
  "LINACEAE",
  "LINDERNIACEAE",
  "LOGANIACEAE",
  "LORANTHACEAE",
  "LYTHRACEAE",
  
  "MAGNOLIACEAE",
  "MALPIGHIACEAE",
  "MALVACEAE",
  "MARANTACEAE",
  "MARTYNIACEAE",
  "MAZACEAE",
  "MELANTHIACEAE",
  "MELASTOMATACEAE",
  "MELIACEAE",
  "MENISPERMACEAE",
  "MENYANTHACEAE",
  "MITRASTEMONACEAE",
  "MOLLUGINACEAE",
  "MONTIACEAE",
  "MORACEAE",
  "MORINGACEAE",
  "MUSACEAE",
  "MYRICACEAE",
  "MYRISTICACEAE",
  "MYRTACEAE",
  
  "NARTHECIACEAE",
  "NELUMBONACEAE",
  "NEPENTHACEAE",
  "NYCTAGINACEAE",
  "NYMPHAEACEAE",
  "NYSSACEAE",
  
  "OCHNACEAE",
  "OLEACEAE",
  "ONAGRACEAE",
  "ORCHIDACEAE",
  "OROBANCHACEAE",
  "OXALIDACEAE",
  
  "PAEONIACEAE",
  "PANDANACEAE",
  "PAPAVERACEAE",
  "PASSIFLORACEAE",
  "PAULOWNIACEAE",
  "PEDALIACEAE",
  "PENTAPHYLACACEAE",
  "PENTHORACEAE",
  "PETIVERIACEAE",
  "PETROSAVIACEAE",
  "PHILYDRACEAE",
  "PHRYMACEAE",
  "PHYLLANTHACEAE",
  "PHYTOLACCACEAE",
  "PIPERACEAE",
  "PITTOSPORACEAE",
  "PLANTAGINACEAE",
  "PLATANACEAE",
  "PLUMBAGINACEAE",
  "POACEAE",
  "PODOSTEMACEAE",
  "POLEMONIACEAE",
  "POLYGALACEAE",
  "POLYGONACEAE",
  "PONTEDERIACEAE",
  "PORTULACACEAE",
  "POTAMOGETONACEAE",
  "PRIMULACEAE",
  "PROTEACEAE",
  "PUTRANJIVACEAE",
  
  "RANUNCULACEAE",
  "RESEDACEAE",
  "RESTIONACEAE",
  "RHAMNACEAE",
  "RHIZOPHORACEAE",
  "ROSACEAE",
  "RUBIACEAE",
  "RUPPIACEAE",
  "RUTACEAE",
  
  "SABIACEAE",
  "SALICACEAE",
  "SANTALACEAE",
  "SAPINDACEAE",
  "SAPOTACEAE",
  "SAURURACEAE",
  "SAXIFRAGACEAE",
  "SCHEUCHZERIACEAE",
  "SCHISANDRACEAE",
  "SCHOEPFIACEAE",
  "SCROPHULARIACEAE",
  "SIMAROUBACEAE",
  "SMILACACEAE",
  "SOLANACEAE",
  "SPHENOCLEACEAE",
  "STACHYURACEAE",
  "STAPHYLEACEAE",
  "STEMONACEAE",
  "STRELITZIACEAE",
  "STYRACACEAE",
  "SYMPLOCACEAE",
  
  "TALINACEAE",
  "TAMARICACEAE",
  "THEACEAE",
  "THYMELAEACEAE",
  "TOFIELDIACEAE",
  "TRILLIACEAE",
  "TRIURIDACEAE",
  "TROCHODENDRACEAE",
  "TROPAEOLACEAE",
  "TYPHACEAE",
  
  "ULMACEAE",
  "URTICACEAE",
  
  "VERBENACEAE",
  "VIBURNACEAE",
  "VIOLACEAE",
  "VITACEAE",
  "VOCHYSIACEAE",
  
  "XYRIDACEAE",
  
  "ZINGIBERACEAE",
  "ZOSTERACEAE",
  "ZYGOPHYLLACEAE"
)


# ==============================================================================
# 12. BUILD FAMILY CLASSIFICATION REGISTER
# ==============================================================================

raw_register <- bind_rows(
  
  tibble(
    FAMILY = angiosperm_families,
    MAJOR_GROUP = "ANGIOSPERM"
  ),
  
  tibble(
    FAMILY = gymnosperm_families,
    MAJOR_GROUP = "GYMNOSPERM"
  ),
  
  tibble(
    FAMILY = fern_families,
    MAJOR_GROUP = "FERN"
  ),
  
  tibble(
    FAMILY = lycophyte_families,
    MAJOR_GROUP = "LYCOPHYTE"
  ),
  
  tibble(
    FAMILY = non_vascular_families,
    MAJOR_GROUP = "NON_VASCULAR"
  )
)


# ==============================================================================
# 13. CHECK REGISTER FOR CONTRADICTORY ASSIGNMENTS
# ==============================================================================

register_conflicts <- raw_register %>%
  distinct(
    FAMILY,
    MAJOR_GROUP
  ) %>%
  count(
    FAMILY,
    name = "N_GROUPS"
  ) %>%
  filter(
    N_GROUPS > 1L
  )

if (nrow(register_conflicts) > 0L) {
  
  DBI::dbDisconnect(con, shutdown = TRUE)
  rm(con)
  
  stop(
    "Family lineage register contains contradictory assignments:\n",
    paste(
      register_conflicts$FAMILY,
      collapse = ", "
    )
  )
}

family_register <- raw_register %>%
  distinct(
    FAMILY,
    .keep_all = TRUE
  )


# ==============================================================================
# 14. CLASSIFY ALL 260 VPJD FAMILIES
# ==============================================================================

family_crosswalk <- family_inventory %>%
  
  left_join(
    family_register,
    by = "FAMILY"
  ) %>%
  
  mutate(
    
    MAJOR_GROUP =
      coalesce(
        MAJOR_GROUP,
        "UNRESOLVED"
      ),
    
    IS_VASCULAR = case_when(
      
      MAJOR_GROUP %in%
        c(
          "ANGIOSPERM",
          "GYMNOSPERM",
          "FERN",
          "LYCOPHYTE"
        ) ~ TRUE,
      
      MAJOR_GROUP ==
        "NON_VASCULAR" ~ FALSE,
      
      TRUE ~ NA
    ),
    
    SCOPE_STATUS = case_when(
      
      MAJOR_GROUP %in%
        c(
          "ANGIOSPERM",
          "GYMNOSPERM",
          "FERN",
          "LYCOPHYTE"
        ) ~ "IN_SCOPE_VASCULAR",
      
      MAJOR_GROUP ==
        "NON_VASCULAR" ~
        "OUT_OF_SCOPE_NON_VASCULAR",
      
      TRUE ~
        "REQUIRES_CLASSIFICATION"
    ),
    
    CLASSIFICATION_METHOD = case_when(
      
      MAJOR_GROUP ==
        "UNRESOLVED" ~
        "UNRESOLVED_FAMILY",
      
      TRUE ~
        "EXPLICIT_FAMILY_LINEAGE_CROSSWALK"
    )
  ) %>%
  
  arrange(
    MAJOR_GROUP,
    FAMILY
  )


# ==============================================================================
# 15. PROPAGATE FAMILY CLASSIFICATION TO ALL TAXA
# ==============================================================================

audit <- taxonomy %>%
  
  left_join(
    family_crosswalk %>%
      select(
        FAMILY,
        MAJOR_GROUP,
        IS_VASCULAR,
        SCOPE_STATUS,
        CLASSIFICATION_METHOD
      ),
    by = "FAMILY"
  ) %>%
  
  mutate(
    
    MAJOR_GROUP =
      coalesce(
        MAJOR_GROUP,
        "UNRESOLVED"
      ),
    
    SCOPE_STATUS =
      coalesce(
        SCOPE_STATUS,
        "REQUIRES_CLASSIFICATION"
      ),
    
    CLASSIFICATION_METHOD =
      coalesce(
        CLASSIFICATION_METHOD,
        "UNRESOLVED_FAMILY"
      )
  )


# ==============================================================================
# 16. UNRESOLVED TAXA AND FAMILIES
# ==============================================================================

unresolved_families <- family_crosswalk %>%
  filter(
    MAJOR_GROUP == "UNRESOLVED"
  ) %>%
  arrange(
    desc(N_TAXA),
    FAMILY
  )

unresolved_taxa <- audit %>%
  filter(
    MAJOR_GROUP == "UNRESOLVED"
  ) %>%
  select(
    FINAL_WCVP_ID,
    TAXON_NAME,
    TAXON_RANK,
    FAMILY,
    GENUS,
    SPECIES,
    PROVISIONAL_STAR,
    MAJOR_GROUP,
    SCOPE_STATUS,
    CLASSIFICATION_METHOD
  ) %>%
  arrange(
    FAMILY,
    GENUS,
    TAXON_NAME
  )


# ==============================================================================
# 17. MAJOR-LINEAGE PROFILE
# ==============================================================================

lineage_profile <- audit %>%
  
  group_by(
    MAJOR_GROUP
  ) %>%
  
  summarise(
    
    N_FAMILIES =
      n_distinct(
        FAMILY[
          !is.na(FAMILY) &
            FAMILY != ""
        ]
      ),
    
    N_GENERA =
      n_distinct(
        GENUS[
          !is.na(GENUS) &
            GENUS != ""
        ]
      ),
    
    N_SPECIES =
      sum(
        TAXON_RANK == "SPECIES",
        na.rm = TRUE
      ),
    
    N_SUBSPECIES =
      sum(
        TAXON_RANK %in%
          c(
            "SUBSPECIES",
            "SUBSP."
          ),
        na.rm = TRUE
      ),
    
    N_VARIETIES =
      sum(
        TAXON_RANK %in%
          c(
            "VARIETY",
            "VAR."
          ),
        na.rm = TRUE
      ),
    
    N_FORMS =
      sum(
        TAXON_RANK %in%
          c(
            "FORM",
            "FORMA",
            "F."
          ),
        na.rm = TRUE
      ),
    
    N_GENUS_CONCEPTS =
      sum(
        TAXON_RANK == "GENUS",
        na.rm = TRUE
      ),
    
    N_TAXA =
      n(),
    
    .groups = "drop"
  ) %>%
  
  mutate(
    
    PERCENT_ACCEPTED =
      round(
        100 *
          N_TAXA /
          EXPECTED_POPULATION,
        2
      )
  ) %>%
  
  arrange(
    desc(N_TAXA)
  )


# ==============================================================================
# 18. EXACT RANK PROFILE BY LINEAGE
# ==============================================================================

rank_profile <- audit %>%
  
  count(
    MAJOR_GROUP,
    TAXON_RANK,
    name = "N_TAXA"
  ) %>%
  
  arrange(
    MAJOR_GROUP,
    desc(N_TAXA),
    TAXON_RANK
  )


# ==============================================================================
# 19. PROVISIONAL STAR PROFILE BY LINEAGE
# ==============================================================================

star_profile <- audit %>%
  
  mutate(
    STAR_DISPLAY =
      coalesce(
        as.character(PROVISIONAL_STAR),
        "UNRESOLVED"
      )
  ) %>%
  
  count(
    MAJOR_GROUP,
    STAR_DISPLAY,
    name = "N_TAXA"
  ) %>%
  
  arrange(
    MAJOR_GROUP,
    STAR_DISPLAY
  )


# ==============================================================================
# 20. SCOPE PROFILE
# ==============================================================================

scope_profile <- audit %>%
  
  count(
    SCOPE_STATUS,
    name = "N_TAXA"
  ) %>%
  
  mutate(
    
    PERCENT_ACCEPTED =
      round(
        100 *
          N_TAXA /
          EXPECTED_POPULATION,
        2
      )
  ) %>%
  
  arrange(
    desc(N_TAXA)
  )


# ==============================================================================
# 21. COUNTS FOR VALIDATION
# ==============================================================================

n_unresolved_families <-
  nrow(unresolved_families)

n_unresolved_taxa <-
  nrow(unresolved_taxa)

n_non_vascular_taxa <-
  sum(
    audit$MAJOR_GROUP ==
      "NON_VASCULAR",
    na.rm = TRUE
  )

n_vascular_taxa <-
  sum(
    audit$MAJOR_GROUP %in%
      c(
        "ANGIOSPERM",
        "GYMNOSPERM",
        "FERN",
        "LYCOPHYTE"
      ),
    na.rm = TRUE
  )

n_classified_taxa <-
  EXPECTED_POPULATION -
  n_unresolved_taxa


# ==============================================================================
# 22. VALIDATION
#
# v0.2.0:
# Unlike v0.1.0, unresolved families/concepts now constitute validation failure.
# ==============================================================================

valid_groups <- c(
  "ANGIOSPERM",
  "GYMNOSPERM",
  "FERN",
  "LYCOPHYTE",
  "NON_VASCULAR"
)

star_values_unchanged <- all(
  (
    audit$PROVISIONAL_STAR ==
      taxonomy$PROVISIONAL_STAR
  ) |
    (
      is.na(audit$PROVISIONAL_STAR) &
        is.na(taxonomy$PROVISIONAL_STAR)
    ),
  na.rm = TRUE
)

validation <- tibble(
  
  CHECK = c(
    
    "Canonical taxonomy table present",
    "02a audit table present",
    "Canonical population = 11,439",
    "02a population = 11,439",
    "Canonical population unique by WCVP ID",
    "Canonical family population = 260",
    "All taxa have family",
    "All 260 families explicitly classified",
    "No unresolved families remain",
    "No unresolved concepts remain",
    "All taxa assigned recognised major group",
    "Classified population = 11,439",
    "Vascular + non-vascular population = 11,439",
    "02a and 02b non-vascular counts agree",
    "No contradictory family crosswalk assignments",
    "Provisional Star allocations unchanged",
    "No canonical taxa removed",
    "Family crosswalk totals 260 represented families",
    "Lineage profile totals 11,439 concepts"
  ),
  
  PASS = c(
    
    TABLE_INPUT %in% db_tables,
    
    TABLE_02A %in% db_tables,
    
    nrow(audit) ==
      EXPECTED_POPULATION,
    
    nrow(audit_02a) ==
      EXPECTED_POPULATION,
    
    n_distinct(
      audit$FINAL_WCVP_ID
    ) ==
      EXPECTED_POPULATION,
    
    nrow(family_inventory) ==
      EXPECTED_FAMILIES,
    
    all(
      !is.na(audit$FAMILY) &
        audit$FAMILY != ""
    ),
    
    sum(
      family_crosswalk$MAJOR_GROUP !=
        "UNRESOLVED"
    ) ==
      EXPECTED_FAMILIES,
    
    n_unresolved_families ==
      0L,
    
    n_unresolved_taxa ==
      0L,
    
    all(
      audit$MAJOR_GROUP %in%
        valid_groups
    ),
    
    n_classified_taxa ==
      EXPECTED_POPULATION,
    
    n_vascular_taxa +
      n_non_vascular_taxa ==
      EXPECTED_POPULATION,
    
    n_non_vascular_taxa ==
      n_02a_non_vascular,
    
    nrow(register_conflicts) ==
      0L,
    
    star_values_unchanged,
    
    nrow(audit) ==
      nrow(taxonomy),
    
    nrow(family_crosswalk) ==
      EXPECTED_FAMILIES,
    
    sum(lineage_profile$N_TAXA) ==
      EXPECTED_POPULATION
  )
) %>%
  
  mutate(
    
    PASS =
      coalesce(
        PASS,
        FALSE
      ),
    
    RESULT =
      if_else(
        PASS,
        "PASS",
        "FAIL"
      )
  )


# ==============================================================================
# 23. CONSOLE OUTPUT
# ==============================================================================

cat("\n")
cat("============================================================\n")
cat("VPJD MAJOR TAXONOMIC GROUP PROFILE\n")
cat("============================================================\n\n")

print(
  lineage_profile,
  n = Inf
)

cat("\n")
cat("— TAXONOMIC SCOPE PROFILE —\n\n")

print(
  scope_profile,
  n = Inf
)

cat("\n")
cat("— UNRESOLVED FAMILY PROFILE —\n\n")

if (n_unresolved_families == 0L) {
  
  cat(
    "All 260 represented families classified.\n"
  )
  
} else {
  
  print(
    unresolved_families,
    n = Inf
  )
}

cat("\n")
cat("— PROVISIONAL STAR PROFILE BY MAJOR GROUP —\n\n")

print(
  star_profile,
  n = Inf
)

cat("\n")
cat("— VALIDATION —\n\n")

print(
  validation,
  n = Inf
)


# ==============================================================================
# 24. STOP IF VALIDATION FAILS
# ==============================================================================

if (!all(validation$PASS)) {
  
  failed_checks <- validation %>%
    filter(!PASS) %>%
    pull(CHECK)
  
  cat("\n")
  cat("VALIDATION FAILURE\n")
  cat("------------------\n")
  cat(
    paste0(
      " - ",
      failed_checks
    ),
    sep = "\n"
  )
  cat("\n")
  
  if (n_unresolved_families > 0L) {
    
    cat("\nUnresolved families:\n")
    
    print(
      unresolved_families,
      n = Inf
    )
  }
  
  DBI::dbDisconnect(
    con,
    shutdown = TRUE
  )
  
  rm(con)
  
  stop(
    paste0(
      "\nVPJD taxonomic revision 02b validation failed.\n",
      "No v0.2.0 DuckDB outputs have been written."
    )
  )
}


# ==============================================================================
# 25. WRITE DUCKDB OUTPUTS
# ==============================================================================

DBI::dbWriteTable(
  con,
  TABLE_AUDIT,
  audit,
  overwrite = TRUE
)

DBI::dbWriteTable(
  con,
  TABLE_FAMILY_CROSSWALK,
  family_crosswalk,
  overwrite = TRUE
)

DBI::dbWriteTable(
  con,
  TABLE_LINEAGE_PROFILE,
  lineage_profile,
  overwrite = TRUE
)

DBI::dbWriteTable(
  con,
  TABLE_UNRESOLVED,
  unresolved_taxa,
  overwrite = TRUE
)

DBI::dbWriteTable(
  con,
  TABLE_VALIDATION,
  validation,
  overwrite = TRUE
)


# ==============================================================================
# 26. WRITE CSV OUTPUTS
# ==============================================================================

write_csv(
  audit,
  file.path(
    OUTPUT_DIR,
    "VPJD_02b_taxonomic_scope_audit.csv"
  )
)

write_csv(
  family_crosswalk,
  file.path(
    OUTPUT_DIR,
    "VPJD_02b_family_lineage_crosswalk.csv"
  )
)

write_csv(
  lineage_profile,
  file.path(
    OUTPUT_DIR,
    "VPJD_02b_major_lineage_profile.csv"
  )
)

write_csv(
  rank_profile,
  file.path(
    OUTPUT_DIR,
    "VPJD_02b_rank_profile_by_lineage.csv"
  )
)

write_csv(
  star_profile,
  file.path(
    OUTPUT_DIR,
    "VPJD_02b_star_profile_by_lineage.csv"
  )
)

write_csv(
  scope_profile,
  file.path(
    OUTPUT_DIR,
    "VPJD_02b_scope_profile.csv"
  )
)

write_csv(
  unresolved_families,
  file.path(
    OUTPUT_DIR,
    "VPJD_02b_unresolved_families.csv"
  )
)

write_csv(
  unresolved_taxa,
  file.path(
    OUTPUT_DIR,
    "VPJD_02b_unresolved_taxa.csv"
  )
)

write_csv(
  validation,
  file.path(
    OUTPUT_DIR,
    "VPJD_02b_validation.csv"
  )
)


# ==============================================================================
# 27. VERIFY DATABASE OUTPUTS
# ==============================================================================

tables_after_write <-
  DBI::dbListTables(con)

expected_output_tables <- c(
  TABLE_AUDIT,
  TABLE_FAMILY_CROSSWALK,
  TABLE_LINEAGE_PROFILE,
  TABLE_UNRESOLVED,
  TABLE_VALIDATION
)

missing_output_tables <- setdiff(
  expected_output_tables,
  tables_after_write
)

if (length(missing_output_tables) > 0L) {
  
  DBI::dbDisconnect(
    con,
    shutdown = TRUE
  )
  
  rm(con)
  
  stop(
    "02b completed but output tables could not all be verified:\n",
    paste(
      missing_output_tables,
      collapse = ", "
    )
  )
}


# ==============================================================================
# 28. RESULT OBJECT
# ==============================================================================

taxrev_02b_result <- list(
  
  audit =
    audit,
  
  family_crosswalk =
    family_crosswalk,
  
  lineage_profile =
    lineage_profile,
  
  rank_profile =
    rank_profile,
  
  star_profile =
    star_profile,
  
  scope_profile =
    scope_profile,
  
  unresolved_families =
    unresolved_families,
  
  unresolved_taxa =
    unresolved_taxa,
  
  validation =
    validation
)


# ==============================================================================
# 29. COMPLETION REPORT
# ==============================================================================

cat("\n")
cat("============================================================\n")

cat(
  "VPJD TAXONOMIC REVISION 02b v",
  VERSION,
  " COMPLETE\n",
  sep = ""
)

cat("============================================================\n")

cat(
  "Accepted concepts audited: ",
  format(
    EXPECTED_POPULATION,
    big.mark = ","
  ),
  "\n",
  sep = ""
)

cat(
  "Families audited: ",
  format(
    EXPECTED_FAMILIES,
    big.mark = ","
  ),
  "\n",
  sep = ""
)

cat(
  "Classified vascular concepts: ",
  format(
    n_vascular_taxa,
    big.mark = ","
  ),
  "\n",
  sep = ""
)

cat(
  "Non-vascular concepts: ",
  format(
    n_non_vascular_taxa,
    big.mark = ","
  ),
  "\n",
  sep = ""
)

cat(
  "Unresolved families: ",
  format(
    n_unresolved_families,
    big.mark = ","
  ),
  "\n",
  sep = ""
)

cat(
  "Unresolved concepts: ",
  format(
    n_unresolved_taxa,
    big.mark = ","
  ),
  "\n",
  sep = ""
)

cat(
  "Taxonomic coverage: ",
  round(
    100 *
      n_classified_taxa /
      EXPECTED_POPULATION,
    2
  ),
  "%\n",
  sep = ""
)

cat("Canonical taxa removed: 0\n")
cat("Star allocations changed: FALSE\n")

cat(
  "Validation: ",
  sum(validation$PASS),
  "/",
  nrow(validation),
  " PASS\n",
  sep = ""
)

cat(
  "Canonical audit: ",
  TABLE_AUDIT,
  "\n",
  sep = ""
)

cat(
  "Family crosswalk: ",
  TABLE_FAMILY_CROSSWALK,
  "\n",
  sep = ""
)

cat(
  "Outputs: ",
  OUTPUT_DIR,
  "\n",
  sep = ""
)

cat("============================================================\n")


# ==============================================================================
# 30. CLEAN DISCONNECT
# ==============================================================================

DBI::dbDisconnect(
  con,
  shutdown = TRUE
)

rm(con)

cat("\nDuckDB connection closed cleanly.\n")


# ==============================================================================
# END
# ==============================================================================