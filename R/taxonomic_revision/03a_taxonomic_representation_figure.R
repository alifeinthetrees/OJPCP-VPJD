# ==============================================================================
# VPJD — PUBLICATION FIGURES
# 03a — TAXONOMIC REPRESENTATION OF THE VASCULAR PLANTS OF JAPAN DATABASE
#
# Purpose:
#   Produce a clear, scientifically defensible and self-explanatory overview
#   of taxonomic representation in the validated VPJD.
#
# Terminology:
#
#   "Taxonomic records"
#       = individual accepted WCVP-linked records retained in VPJD.
#
#   "Families represented"
#       = number of distinct families occurring in VPJD.
#
#   "Genera represented"
#       = number of distinct genera occurring anywhere in VPJD.
#         This is NOT the same as the number of records at genus rank.
#
#   "Species represented"
#       = number of distinct species occurring anywhere in VPJD.
#         This is NOT necessarily identical to the number of records
#         whose rank is Species.
#
# Major vascular-plant groups:
#   - Angiosperms
#   - Gymnosperms
#   - Ferns
#   - Lycophytes
#
# Outputs:
#
#   TABLES
#     VPJD_03a_headline_statistics.csv
#     VPJD_03a_major_group_summary.csv
#     VPJD_03a_rank_profile.csv
#     VPJD_03a_rank_by_major_group.csv
#     VPJD_03a_validation.csv
#
#   FIGURES
#     VPJD_03a_taxonomic_representation.png
#     VPJD_03a_taxonomic_representation.pdf
#     VPJD_03a_major_group_records.png
#     VPJD_03a_taxonomic_rank_profile.png
#
# IMPORTANT:
#   READ-ONLY ANALYTICAL MODULE.
#   DuckDB is NOT opened.
#   Canonical taxonomy is NOT modified.
#   Star allocations are NOT modified.
#
# ==============================================================================


# ==============================================================================
# 01. VERSION
# ==============================================================================

VERSION <- "0.4.0"


# ==============================================================================
# 02. PACKAGES
# ==============================================================================

suppressPackageStartupMessages({
  
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(stringr)
  library(readr)
  library(ggplot2)
  library(scales)
  
})


# ==============================================================================
# 03. CONSTANTS
# ==============================================================================

EXPECTED_CANONICAL_POPULATION <- 11439L
EXPECTED_CANONICAL_FAMILIES <- 260L


PROJECT_ROOT <-
  "I:/R/OJPCP/VPJD-OJPCP"


INPUT_DIR <- file.path(
  PROJECT_ROOT,
  "outputs",
  "tables",
  "taxonomic_revision",
  "02g_taxonomic_scope"
)


OUTPUT_TABLE_DIR <- file.path(
  PROJECT_ROOT,
  "outputs",
  "tables",
  "taxonomic_revision",
  "03a_taxonomic_representation"
)


OUTPUT_FIGURE_DIR <- file.path(
  PROJECT_ROOT,
  "outputs",
  "figures",
  "taxonomic_revision",
  "03a_taxonomic_representation"
)


dir.create(
  OUTPUT_TABLE_DIR,
  recursive = TRUE,
  showWarnings = FALSE
)


dir.create(
  OUTPUT_FIGURE_DIR,
  recursive = TRUE,
  showWarnings = FALSE
)


# ==============================================================================
# 04. INPUT
# ==============================================================================

INPUT_FILE <- file.path(
  INPUT_DIR,
  "VPJD_02g_retained_population.csv"
)


if (!file.exists(INPUT_FILE)) {
  
  stop(
    "Validated 02g retained population not found:\n",
    INPUT_FILE
  )
  
}


# ==============================================================================
# 05. START REPORT
# ==============================================================================

cat("\n")
cat("============================================================\n")
cat("VPJD TAXONOMIC REPRESENTATION 03a v", VERSION, "\n", sep = "")
cat("============================================================\n\n")

cat(
  "Input:\n",
  INPUT_FILE,
  "\n\n",
  sep = ""
)


# ==============================================================================
# 06. LOAD VALIDATED VPJD
# ==============================================================================

vpjd <- read_csv(
  INPUT_FILE,
  show_col_types = FALSE
)


cat(
  "Taxonomic records loaded: ",
  format(nrow(vpjd), big.mark = ","),
  "\n",
  sep = ""
)


# ==============================================================================
# 07. REQUIRED FIELDS
# ==============================================================================

required_fields <- c(
  
  "FINAL_WCVP_ID",
  "FINAL_WCVP_RECOGNISED_NAME",
  "FINAL_WCVP_RANK",
  "FAMILY",
  "GENUS",
  "SPECIES",
  "MAJOR_GROUP",
  "VPJD_SCOPE"
  
)


missing_fields <- setdiff(
  required_fields,
  names(vpjd)
)


if (length(missing_fields) > 0L) {
  
  stop(
    "Required field(s) missing:\n",
    paste(
      missing_fields,
      collapse = "\n"
    )
  )
  
}


# ==============================================================================
# 08. STANDARDISE FIELDS
# ==============================================================================

vpjd <- vpjd |>
  
  mutate(
    
    FINAL_WCVP_ID =
      as.character(FINAL_WCVP_ID),
    
    FAMILY =
      str_squish(
        as.character(FAMILY)
      ),
    
    GENUS =
      str_squish(
        as.character(GENUS)
      ),
    
    SPECIES =
      str_squish(
        as.character(SPECIES)
      ),
    
    MAJOR_GROUP =
      toupper(
        str_squish(
          as.character(MAJOR_GROUP)
        )
      ),
    
    RANK =
      toupper(
        str_squish(
          as.character(FINAL_WCVP_RANK)
        )
      ),
    
    VPJD_SCOPE =
      toupper(
        str_squish(
          as.character(VPJD_SCOPE)
        )
      )
    
  )


# ==============================================================================
# 09. VALIDATE POPULATION
# ==============================================================================

if (nrow(vpjd) != EXPECTED_CANONICAL_POPULATION) {
  
  stop(
    "VPJD population = ",
    nrow(vpjd),
    "; expected ",
    EXPECTED_CANONICAL_POPULATION,
    "."
  )
  
}


if (
  n_distinct(vpjd$FINAL_WCVP_ID) !=
  EXPECTED_CANONICAL_POPULATION
) {
  
  stop(
    "VPJD population is not unique by FINAL_WCVP_ID."
  )
  
}


if (
  any(
    vpjd$VPJD_SCOPE != "RETAIN"
  )
) {
  
  stop(
    "02g retained population contains records not classified RETAIN."
  )
  
}


# ==============================================================================
# 10. MAJOR-GROUP LABELS
# ==============================================================================

expected_major_groups <- c(
  "ANGIOSPERM",
  "GYMNOSPERM",
  "FERN",
  "LYCOPHYTE"
)


unexpected_major_groups <- setdiff(
  unique(vpjd$MAJOR_GROUP),
  expected_major_groups
)


if (length(unexpected_major_groups) > 0L) {
  
  stop(
    "Unexpected major group(s):\n",
    paste(
      unexpected_major_groups,
      collapse = "\n"
    )
  )
  
}


vpjd <- vpjd |>
  
  mutate(
    
    GROUP_LABEL =
      case_when(
        
        MAJOR_GROUP == "ANGIOSPERM" ~
          "Angiosperms",
        
        MAJOR_GROUP == "GYMNOSPERM" ~
          "Gymnosperms",
        
        MAJOR_GROUP == "FERN" ~
          "Ferns",
        
        MAJOR_GROUP == "LYCOPHYTE" ~
          "Lycophytes",
        
        TRUE ~
          NA_character_
        
      )
    
  )


# ==============================================================================
# 11. NORMALISE TAXONOMIC RANKS
# ==============================================================================

vpjd <- vpjd |>
  
  mutate(
    
    RANK_GROUP =
      case_when(
        
        RANK == "GENUS" ~
          "Genus",
        
        RANK == "SPECIES" ~
          "Species",
        
        RANK == "SUBSPECIES" ~
          "Subspecies",
        
        RANK == "VARIETY" ~
          "Variety",
        
        RANK == "FORM" ~
          "Form",
        
        TRUE ~
          "Other"
        
      )
    
  )


# ==============================================================================
# 12. HEADLINE TAXONOMIC STATISTICS
# ==============================================================================

n_records <-
  nrow(vpjd)


n_families <-
  n_distinct(
    vpjd$FAMILY[
      !is.na(vpjd$FAMILY) &
        vpjd$FAMILY != ""
    ]
  )


n_genera_represented <-
  n_distinct(
    vpjd$GENUS[
      !is.na(vpjd$GENUS) &
        vpjd$GENUS != ""
    ]
  )


n_species_represented <-
  n_distinct(
    vpjd$SPECIES[
      !is.na(vpjd$SPECIES) &
        vpjd$SPECIES != ""
    ]
  )


n_genus_rank <-
  sum(
    vpjd$RANK_GROUP == "Genus"
  )


n_species_rank <-
  sum(
    vpjd$RANK_GROUP == "Species"
  )


n_subspecies <-
  sum(
    vpjd$RANK_GROUP == "Subspecies"
  )


n_varieties <-
  sum(
    vpjd$RANK_GROUP == "Variety"
  )


n_forms <-
  sum(
    vpjd$RANK_GROUP == "Form"
  )


n_other <-
  sum(
    vpjd$RANK_GROUP == "Other"
  )


n_infraspecific <-
  n_subspecies +
  n_varieties +
  n_forms


# ==============================================================================
# 13. HEADLINE STATISTICS TABLE
# ==============================================================================

headline_statistics <- tibble(
  
  METRIC = c(
    
    "Taxonomic records",
    
    "Families represented",
    
    "Genera represented",
    
    "Species represented",
    
    "Genus-rank records",
    
    "Species-rank records",
    
    "Infraspecific records",
    
    "Subspecies",
    
    "Varieties",
    
    "Forms",
    
    "Other ranks"
    
  ),
  
  N = c(
    
    n_records,
    
    n_families,
    
    n_genera_represented,
    
    n_species_represented,
    
    n_genus_rank,
    
    n_species_rank,
    
    n_infraspecific,
    
    n_subspecies,
    
    n_varieties,
    
    n_forms,
    
    n_other
    
  )
  
)


cat("\n")
cat("============================================================\n")
cat("HEADLINE TAXONOMIC STATISTICS\n")
cat("============================================================\n\n")


print(
  headline_statistics,
  n = Inf
)


# ==============================================================================
# 14. EXPLICITLY REPORT GENERA CHECK
# ==============================================================================

cat("\n")
cat("------------------------------------------------------------\n")
cat("GENERA CHECK\n")
cat("------------------------------------------------------------\n\n")


cat(
  "Distinct genera represented anywhere in VPJD: ",
  format(
    n_genera_represented,
    big.mark = ","
  ),
  "\n",
  sep = ""
)


cat(
  "Records whose taxonomic rank is Genus: ",
  format(
    n_genus_rank,
    big.mark = ","
  ),
  "\n\n",
  sep = ""
)


cat(
  "These are deliberately different statistics.\n"
)


# ==============================================================================
# 15. MAJOR-GROUP SUMMARY
# ==============================================================================

major_group_summary <- vpjd |>
  
  group_by(
    MAJOR_GROUP,
    GROUP_LABEL
  ) |>
  
  summarise(
    
    N_RECORDS =
      n(),
    
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
      n_distinct(
        SPECIES[
          !is.na(SPECIES) &
            SPECIES != ""
        ]
      ),
    
    .groups =
      "drop"
    
  ) |>
  
  mutate(
    
    PERCENT_RECORDS =
      100 *
      N_RECORDS /
      n_records
    
  ) |>
  
  arrange(
    desc(N_RECORDS)
  )


cat("\n")
cat("============================================================\n")
cat("TAXONOMIC REPRESENTATION BY MAJOR GROUP\n")
cat("============================================================\n\n")


print(
  major_group_summary,
  n = Inf,
  width = Inf
)


# ==============================================================================
# 16. RANK PROFILE
# ==============================================================================

rank_profile <- vpjd |>
  
  count(
    RANK_GROUP,
    name = "N_RECORDS"
  ) |>
  
  mutate(
    
    PERCENT_RECORDS =
      100 *
      N_RECORDS /
      n_records
    
  ) |>
  
  arrange(
    desc(N_RECORDS)
  )


# ==============================================================================
# 17. RANK PROFILE BY MAJOR GROUP
# ==============================================================================

rank_by_major_group <- vpjd |>
  
  count(
    
    MAJOR_GROUP,
    
    GROUP_LABEL,
    
    RANK_GROUP,
    
    name =
      "N_RECORDS"
    
  )


# ==============================================================================
# 18. WRITE TABLES
# ==============================================================================

write_csv(
  
  headline_statistics,
  
  file.path(
    OUTPUT_TABLE_DIR,
    "VPJD_03a_headline_statistics.csv"
  )
  
)


write_csv(
  
  major_group_summary,
  
  file.path(
    OUTPUT_TABLE_DIR,
    "VPJD_03a_major_group_summary.csv"
  )
  
)


write_csv(
  
  rank_profile,
  
  file.path(
    OUTPUT_TABLE_DIR,
    "VPJD_03a_rank_profile.csv"
  )
  
)


write_csv(
  
  rank_by_major_group,
  
  file.path(
    OUTPUT_TABLE_DIR,
    "VPJD_03a_rank_by_major_group.csv"
  )
  
)


# ==============================================================================
# 19. COLOUR SYSTEM
# ==============================================================================

# Colour is used consistently to encode MAJOR VASCULAR-PLANT GROUP.
#
# Angiosperms  = green
# Gymnosperms  = blue
# Ferns        = orange
# Lycophytes   = purple
#
# The palette provides strong visual separation and remains reasonably
# interpretable under common forms of colour-vision deficiency.

GROUP_COLOURS <- c(
  
  "Angiosperms" =
    "#009E73",
  
  "Gymnosperms" =
    "#0072B2",
  
  "Ferns" =
    "#E69F00",
  
  "Lycophytes" =
    "#CC79A7"
  
)


# Rank is a different variable and therefore uses a separate palette.

RANK_COLOURS <- c(
  
  "Genus" =
    "#0072B2",
  
  "Species" =
    "#009E73",
  
  "Subspecies" =
    "#E69F00",
  
  "Variety" =
    "#CC79A7",
  
  "Form" =
    "#56B4E9",
  
  "Other" =
    "#7F7F7F"
  
)


# ==============================================================================
# 20. FACTOR ORDER
# ==============================================================================

major_group_summary <- major_group_summary |>
  
  mutate(
    
    GROUP_LABEL =
      factor(
        
        GROUP_LABEL,
        
        levels = c(
          
          "Lycophytes",
          
          "Ferns",
          
          "Gymnosperms",
          
          "Angiosperms"
          
        )
        
      )
    
  )


# ==============================================================================
# 21. FIGURE 1 — TAXONOMIC RECORDS BY MAJOR GROUP
# ==============================================================================

major_group_plot_data <- major_group_summary |>
  
  mutate(
    
    LABEL =
      paste0(
        
        comma(N_RECORDS),
        
        "  (",
        
        number(
          PERCENT_RECORDS,
          accuracy = 0.1
        ),
        
        "%)"
        
      )
    
  )


major_group_plot <- ggplot(
  
  major_group_plot_data,
  
  aes(
    
    x =
      N_RECORDS,
    
    y =
      GROUP_LABEL,
    
    fill =
      GROUP_LABEL
    
  )
  
) +
  
  geom_col(
    width = 0.62
  ) +
  
  geom_text(
    
    aes(
      label = LABEL
    ),
    
    hjust = -0.08,
    
    size = 3.8,
    
    fontface = "bold"
    
  ) +
  
  scale_fill_manual(
    values = GROUP_COLOURS
  ) +
  
  scale_x_continuous(
    
    labels = comma,
    
    expand =
      expansion(
        mult = c(
          0,
          0.20
        )
      )
    
  ) +
  
  labs(
    
    title =
      "Taxonomic records by major vascular-plant group",
    
    subtitle =
      paste0(
        comma(n_records),
        " records in the Vascular Plants of Japan Database"
      ),
    
    x =
      "Taxonomic records",
    
    y =
      NULL
    
  ) +
  
  theme_minimal(
    base_size = 11
  ) +
  
  theme(
    
    legend.position =
      "none",
    
    panel.grid.major.y =
      element_blank(),
    
    panel.grid.minor =
      element_blank(),
    
    plot.title =
      element_text(
        face = "bold",
        size = 14
      )
    
  )


# ==============================================================================
# 22. INFOGRAPHIC DATA
# ==============================================================================

infographic_data <- major_group_summary |>
  
  select(
    
    GROUP_LABEL,
    
    N_RECORDS,
    
    N_FAMILIES,
    
    N_GENERA,
    
    N_SPECIES
    
  ) |>
  
  pivot_longer(
    
    cols = c(
      
      N_FAMILIES,
      
      N_GENERA,
      
      N_SPECIES,
      
      N_RECORDS
      
    ),
    
    names_to =
      "METRIC",
    
    values_to =
      "N"
    
  ) |>
  
  mutate(
    
    METRIC =
      case_when(
        
        METRIC == "N_FAMILIES" ~
          paste0(
            "Families (",
            comma(n_families),
            ")"
          ),
        
        METRIC == "N_GENERA" ~
          paste0(
            "Genera (",
            comma(n_genera_represented),
            ")"
          ),
        
        METRIC == "N_SPECIES" ~
          paste0(
            "Species represented (",
            comma(n_species_represented),
            ")"
          ),
        
        METRIC == "N_RECORDS" ~
          paste0(
            "Taxonomic records (",
            comma(n_records),
            ")"
          )
        
      )
    
  )


metric_levels <- c(
  
  paste0(
    "Families (",
    comma(n_families),
    ")"
  ),
  
  paste0(
    "Genera (",
    comma(n_genera_represented),
    ")"
  ),
  
  paste0(
    "Species represented (",
    comma(n_species_represented),
    ")"
  ),
  
  paste0(
    "Taxonomic records (",
    comma(n_records),
    ")"
  )
  
)


infographic_data <- infographic_data |>
  
  mutate(
    
    METRIC =
      factor(
        METRIC,
        levels = metric_levels
      ),
    
    GROUP_LABEL =
      factor(
        
        GROUP_LABEL,
        
        levels = c(
          
          "Lycophytes",
          
          "Ferns",
          
          "Gymnosperms",
          
          "Angiosperms"
          
        )
        
      )
    
  )


# ==============================================================================
# 23. MAIN SCIENTIFIC INFOGRAPHIC
# ==============================================================================

figure_03a <- ggplot(
  
  infographic_data,
  
  aes(
    
    x =
      N,
    
    y =
      GROUP_LABEL,
    
    fill =
      GROUP_LABEL
    
  )
  
) +
  
  geom_col(
    width = 0.64
  ) +
  
  geom_text(
    
    aes(
      label =
        comma(N)
    ),
    
    hjust =
      -0.08,
    
    size =
      3.5,
    
    fontface =
      "bold"
    
  ) +
  
  facet_wrap(
    
    ~ METRIC,
    
    scales =
      "free_x",
    
    ncol =
      2
    
  ) +
  
  scale_fill_manual(
    values =
      GROUP_COLOURS
  ) +
  
  scale_x_continuous(
    
    labels =
      comma,
    
    expand =
      expansion(
        mult = c(
          0,
          0.20
        )
      )
    
  ) +
  
  labs(
    
    title =
      "Taxonomic representation of the Vascular Plants of Japan Database",
    
    subtitle =
      paste0(
        
        comma(n_records),
        
        " taxonomic records representing ",
        
        comma(n_families),
        
        " families and ",
        
        comma(n_genera_represented),
        
        " genera"
        
      ),
    
    x =
      "Number represented",
    
    y =
      NULL,
    
    fill =
      "Major plant group",
    
    caption =
      paste0(
        
        "VPJD includes vascular plants only: angiosperms, gymnosperms, ",
        "ferns and lycophytes. ",
        
        "Numbers within bars show representation within each major group. ",
        "Numbers in panel headings show totals across VPJD."
        
      )
    
  ) +
  
  theme_minimal(
    base_size = 11
  ) +
  
  theme(
    
    legend.position =
      "bottom",
    
    legend.title =
      element_text(
        face = "bold"
      ),
    
    panel.grid.major.y =
      element_blank(),
    
    panel.grid.minor =
      element_blank(),
    
    strip.text =
      element_text(
        face = "bold",
        size = 11
      ),
    
    plot.title =
      element_text(
        face = "bold",
        size = 15
      ),
    
    plot.subtitle =
      element_text(
        size = 10.5,
        colour = "#555555"
      ),
    
    plot.caption =
      element_text(
        size = 8,
        colour = "#666666",
        hjust = 0
      ),
    
    plot.margin =
      margin(
        12,
        30,
        12,
        12
      )
    
  )


# ==============================================================================
# 24. DISPLAY MAIN FIGURE
# ==============================================================================

print(
  figure_03a
)


# ==============================================================================
# 25. RANK PROFILE FIGURE
# ==============================================================================

rank_plot_data <- rank_profile |>
  
  mutate(
    
    RANK_GROUP =
      factor(
        
        RANK_GROUP,
        
        levels = c(
          
          "Other",
          
          "Form",
          
          "Subspecies",
          
          "Genus",
          
          "Variety",
          
          "Species"
          
        )
        
      ),
    
    LABEL =
      paste0(
        
        comma(N_RECORDS),
        
        "  (",
        
        number(
          PERCENT_RECORDS,
          accuracy = 0.1
        ),
        
        "%)"
        
      )
    
  )


rank_plot <- ggplot(
  
  rank_plot_data,
  
  aes(
    
    x =
      N_RECORDS,
    
    y =
      RANK_GROUP,
    
    fill =
      RANK_GROUP
    
  )
  
) +
  
  geom_col(
    width = 0.65
  ) +
  
  geom_text(
    
    aes(
      label = LABEL
    ),
    
    hjust =
      -0.08,
    
    size =
      3.5,
    
    fontface =
      "bold"
    
  ) +
  
  scale_fill_manual(
    values =
      RANK_COLOURS
  ) +
  
  scale_x_continuous(
    
    labels =
      comma,
    
    expand =
      expansion(
        mult = c(
          0,
          0.22
        )
      )
    
  ) +
  
  labs(
    
    title =
      "Taxonomic rank of VPJD records",
    
    subtitle =
      paste0(
        comma(n_records),
        " taxonomic records"
      ),
    
    x =
      "Number of records",
    
    y =
      NULL
    
  ) +
  
  theme_minimal(
    base_size = 11
  ) +
  
  theme(
    
    legend.position =
      "none",
    
    panel.grid.major.y =
      element_blank(),
    
    panel.grid.minor =
      element_blank(),
    
    plot.title =
      element_text(
        face = "bold",
        size = 14
      )
    
  )


# ==============================================================================
# 26. EXPORT MAIN INFOGRAPHIC — PNG
# ==============================================================================

PNG_FILE <- file.path(
  OUTPUT_FIGURE_DIR,
  "VPJD_03a_taxonomic_representation.png"
)


ggsave(
  
  filename =
    PNG_FILE,
  
  plot =
    figure_03a,
  
  width =
    220,
  
  height =
    180,
  
  units =
    "mm",
  
  dpi =
    600,
  
  bg =
    "white"
  
)


# ==============================================================================
# 27. EXPORT MAIN INFOGRAPHIC — PDF
# ==============================================================================

PDF_FILE <- file.path(
  OUTPUT_FIGURE_DIR,
  "VPJD_03a_taxonomic_representation.pdf"
)


ggsave(
  
  filename =
    PDF_FILE,
  
  plot =
    figure_03a,
  
  width =
    220,
  
  height =
    180,
  
  units =
    "mm",
  
  device =
    cairo_pdf,
  
  bg =
    "white"
  
)


# ==============================================================================
# 28. EXPORT MAJOR-GROUP FIGURE
# ==============================================================================

ggsave(
  
  filename =
    file.path(
      OUTPUT_FIGURE_DIR,
      "VPJD_03a_major_group_records.png"
    ),
  
  plot =
    major_group_plot,
  
  width =
    190,
  
  height =
    110,
  
  units =
    "mm",
  
  dpi =
    600,
  
  bg =
    "white"
  
)


# ==============================================================================
# 29. EXPORT TAXONOMIC-RANK FIGURE
# ==============================================================================

ggsave(
  
  filename =
    file.path(
      OUTPUT_FIGURE_DIR,
      "VPJD_03a_taxonomic_rank_profile.png"
    ),
  
  plot =
    rank_plot,
  
  width =
    190,
  
  height =
    120,
  
  units =
    "mm",
  
  dpi =
    600,
  
  bg =
    "white"
  
)


# ==============================================================================
# 30. VALIDATION
# ==============================================================================

validation <- tibble(
  
  CHECK = c(
    
    "Canonical population = 11,439",
    
    "Population unique by WCVP ID",
    
    "Families represented = 260",
    
    "Four vascular-plant groups represented",
    
    "Angiosperm families = 228",
    
    "Gymnosperm families = 11",
    
    "Fern families = 18",
    
    "Lycophyte families = 3",
    
    "Genus-rank records = 823",
    
    "Species-rank records = 8,906",
    
    "Subspecies records = 499",
    
    "Variety records = 1,147",
    
    "Form records = 62",
    
    "Other-rank records = 2",
    
    "Rank totals = 11,439",
    
    "Major-group record totals = 11,439",
    
    "No missing major-group assignments",
    
    "At least one genus represented",
    
    "At least one species represented",
    
    "Main PNG written",
    
    "Main PDF written"
    
  ),
  
  PASS = c(
    
    n_records ==
      EXPECTED_CANONICAL_POPULATION,
    
    n_distinct(
      vpjd$FINAL_WCVP_ID
    ) ==
      EXPECTED_CANONICAL_POPULATION,
    
    n_families ==
      EXPECTED_CANONICAL_FAMILIES,
    
    n_distinct(
      vpjd$MAJOR_GROUP
    ) ==
      4L,
    
    major_group_summary |>
      filter(
        MAJOR_GROUP == "ANGIOSPERM"
      ) |>
      pull(N_FAMILIES) ==
      228L,
    
    major_group_summary |>
      filter(
        MAJOR_GROUP == "GYMNOSPERM"
      ) |>
      pull(N_FAMILIES) ==
      11L,
    
    major_group_summary |>
      filter(
        MAJOR_GROUP == "FERN"
      ) |>
      pull(N_FAMILIES) ==
      18L,
    
    major_group_summary |>
      filter(
        MAJOR_GROUP == "LYCOPHYTE"
      ) |>
      pull(N_FAMILIES) ==
      3L,
    
    n_genus_rank ==
      823L,
    
    n_species_rank ==
      8906L,
    
    n_subspecies ==
      499L,
    
    n_varieties ==
      1147L,
    
    n_forms ==
      62L,
    
    n_other ==
      2L,
    
    sum(
      rank_profile$N_RECORDS
    ) ==
      EXPECTED_CANONICAL_POPULATION,
    
    sum(
      major_group_summary$N_RECORDS
    ) ==
      EXPECTED_CANONICAL_POPULATION,
    
    sum(
      is.na(vpjd$MAJOR_GROUP)
    ) ==
      0L,
    
    n_genera_represented >
      0L,
    
    n_species_represented >
      0L,
    
    file.exists(
      PNG_FILE
    ),
    
    file.exists(
      PDF_FILE
    )
    
  )
  
) |>
  
  mutate(
    
    RESULT =
      if_else(
        PASS,
        "PASS",
        "REVIEW"
      )
    
  )


write_csv(
  
  validation,
  
  file.path(
    OUTPUT_TABLE_DIR,
    "VPJD_03a_validation.csv"
  )
  
)


# ==============================================================================
# 31. FINAL REPORT
# ==============================================================================

cat("\n")
cat("============================================================\n")
cat("03a FINAL TAXONOMIC REPRESENTATION\n")
cat("============================================================\n\n")


cat(
  "Taxonomic records: ",
  comma(n_records),
  "\n",
  sep = ""
)


cat(
  "Families represented: ",
  comma(n_families),
  "\n",
  sep = ""
)


cat(
  "Genera represented: ",
  comma(n_genera_represented),
  "\n",
  sep = ""
)


cat(
  "Species represented: ",
  comma(n_species_represented),
  "\n\n",
  sep = ""
)


cat(
  "Records at genus rank: ",
  comma(n_genus_rank),
  "\n",
  sep = ""
)


cat(
  "Records at species rank: ",
  comma(n_species_rank),
  "\n",
  sep = ""
)


cat(
  "Infraspecific records: ",
  comma(n_infraspecific),
  "\n",
  sep = ""
)


cat(
  "  Subspecies: ",
  comma(n_subspecies),
  "\n",
  sep = ""
)


cat(
  "  Varieties: ",
  comma(n_varieties),
  "\n",
  sep = ""
)


cat(
  "  Forms: ",
  comma(n_forms),
  "\n\n",
  sep = ""
)


cat("------------------------------------------------------------\n")
cat("MAJOR GROUPS\n")
cat("------------------------------------------------------------\n\n")


print(
  major_group_summary,
  n = Inf,
  width = Inf
)


cat("\n")
cat("------------------------------------------------------------\n")
cat("VALIDATION\n")
cat("------------------------------------------------------------\n\n")


print(
  validation,
  n = Inf
)


cat("\n")


cat(
  "Validation: ",
  sum(validation$PASS),
  "/",
  nrow(validation),
  " PASS\n\n",
  sep = ""
)


cat(
  "Main infographic:\n",
  PNG_FILE,
  "\n\n",
  sep = ""
)


cat(
  "PDF:\n",
  PDF_FILE,
  "\n\n",
  sep = ""
)


cat("Database modified: FALSE\n")
cat("Canonical taxonomy modified: FALSE\n")
cat("Star allocations modified: FALSE\n")


cat("\n")
cat("============================================================\n")

cat(
  "VPJD TAXONOMIC REPRESENTATION 03a v",
  VERSION,
  " COMPLETE\n",
  sep = ""
)

cat("============================================================\n")


# ==============================================================================
# END
# ==============================================================================