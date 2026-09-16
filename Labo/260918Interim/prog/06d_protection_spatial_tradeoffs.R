suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(readr)
  library(rnaturalearth)
  library(scales)
  library(sf)
  library(terra)
  library(tidyr)
})

source("prog/common.R")

tradeoff_output_dir <- file.path(
  output_dir, "06_protection_focus", "05_cross_domain_summary"
)
dir.create(tradeoff_output_dir, recursive = TRUE, showWarnings = FALSE)

policy_states <- scenario_states("global2")
grid_years <- c(2005, 2010, seq(2020, 2100, 10))
year_position <- match(target_year, grid_years)
land_classes <- c(
  `Food cropland` = 1L,
  `2G bioenergy` = 2L,
  `Planted forest` = 5L
)

# TEMPORARY WORKAROUND: existing AIM-LUmap files double-count Rbasin and R17.
# Remove this factor after regenerating them with the corrected combine.gms.
grid_export_duplicate_factor <- 2

read_land_classes <- function(state) {
  nc_path <- file.path(
    data_dir, "PLUM", "nc",
    paste0("AIM-LUmap_", policy_states[[state]], ".nc")
  )
  all_layers <- rast(nc_path, subds = "LC_area_share")
  positions <- (unname(land_classes) - 1L) * length(grid_years) + year_position
  selected <- all_layers[[positions]] / grid_export_duplicate_factor
  names(selected) <- names(land_classes)
  selected
}

land_under_effect <- (read_land_classes("111") - read_land_classes("011")) * 100

bii_under_effect <- (
  rast(
    file.path(
      data_dir, "BII", "nc",
      paste0("BIImap_", policy_states[["111"]], "_default.nc")
    ),
    subds = "BII"
  )[[year_position]] -
  rast(
    file.path(
      data_dir, "BII", "nc",
      paste0("BIImap_", policy_states[["011"]], "_default.nc")
    ),
    subds = "BII"
  )[[year_position]]
) * 100
names(bii_under_effect) <- "BII change"

# BII longitudes are shifted by 0.25 degrees relative to AIM-LU. Resample BII
# to the AIM-LU grid before combining cell-level values.
bii_under_effect <- resample(
  bii_under_effect, land_under_effect[[1]], method = "bilinear"
)
names(bii_under_effect) <- "BII change"

# Cell-level association between BII and the three competing land uses.
joint_data <- as.data.frame(
  c(land_under_effect, bii_under_effect), xy = TRUE, na.rm = TRUE
) %>%
  pivot_longer(
    all_of(names(land_classes)),
    names_to = "land_class", values_to = "land_change_pp"
  ) %>%
  rename(bii_change_points = `BII change`) %>%
  filter(is.finite(land_change_pp), is.finite(bii_change_points)) %>%
  mutate(land_class = factor(land_class, levels = names(land_classes)))

# Keep the central 98% in each dimension for a readable exploratory scatter.
# The untrimmed Spearman correlation is retained as a robust full-data check.
joint_display <- joint_data %>%
  group_by(land_class) %>%
  filter(
    abs(land_change_pp) <= quantile(abs(land_change_pp), 0.98, na.rm = TRUE),
    abs(bii_change_points) <=
      quantile(abs(bii_change_points), 0.98, na.rm = TRUE)
  ) %>%
  ungroup()

association_summary <- joint_data %>%
  group_by(land_class) %>%
  summarise(
    cells = n(),
    full_spearman_correlation = cor(
      land_change_pp, bii_change_points, method = "spearman"
    ),
    .groups = "drop"
  ) %>%
  left_join(
    joint_display %>%
      group_by(land_class) %>%
      summarise(
        displayed_cells = n(),
        central_98_pearson_correlation = cor(
          land_change_pp, bii_change_points
        ),
        central_98_linear_slope =
          coef(lm(bii_change_points ~ land_change_pp))[[2]],
        .groups = "drop"
      ),
    by = "land_class"
  )

write_csv(
  association_summary,
  file.path(
    tradeoff_output_dir,
    paste0("grid_bii_land_association_", target_year, ".csv")
  )
)

set.seed(260918)
scatter_data <- joint_display %>%
  group_by(land_class) %>%
  group_modify(~ slice_sample(.x, n = min(50000L, nrow(.x)))) %>%
  ungroup()

scatter_plot <- ggplot(
  scatter_data,
  aes(land_change_pp, bii_change_points)
) +
  geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.25) +
  geom_vline(xintercept = 0, colour = "grey60", linewidth = 0.25) +
  geom_point(colour = "#2c7fb8", alpha = 0.08, size = 0.35) +
  geom_smooth(method = "lm", se = FALSE, colour = "#cb181d", linewidth = 0.7) +
  facet_wrap(vars(land_class), scales = "free_x", ncol = 3) +
  labs(
    x = "Protection-induced land-share change (percentage points)",
    y = "Protection-induced BII change (points)"
  ) +
  theme_paper() +
  theme(panel.grid.minor = element_blank())

ggsave(
  file.path(
    tradeoff_output_dir,
    paste0("grid_bii_land_association_", target_year, ".png")
  ),
  scatter_plot, width = 12, height = 4.8, dpi = 300, bg = "white"
)

# Spatial combinations of food-cropland and BII responses.
land_threshold_pp <- 0.01
bii_threshold_points <- 0.01
cell_area <- cellSize(bii_under_effect, unit = "km")
names(cell_area) <- "cell_area_km2"

quadrant_data <- as.data.frame(
  c(land_under_effect[["Food cropland"]], bii_under_effect, cell_area),
  xy = TRUE, na.rm = TRUE
) %>%
  rename(
    cropland_change_pp = `Food cropland`,
    bii_change_points = `BII change`
  ) %>%
  mutate(
    response = case_when(
      abs(cropland_change_pp) < land_threshold_pp |
        abs(bii_change_points) < bii_threshold_points ~ "Small change",
      bii_change_points > 0 & cropland_change_pp < 0 ~
        "BII gain / cropland loss",
      bii_change_points > 0 & cropland_change_pp > 0 ~
        "BII gain / cropland gain",
      bii_change_points < 0 & cropland_change_pp < 0 ~
        "BII loss / cropland loss",
      bii_change_points < 0 & cropland_change_pp > 0 ~
        "BII loss / cropland gain"
    ),
    response = factor(response, levels = c(
      "BII gain / cropland loss", "BII gain / cropland gain",
      "BII loss / cropland loss", "BII loss / cropland gain",
      "Small change"
    ))
  )

quadrant_summary <- quadrant_data %>%
  group_by(response) %>%
  summarise(
    cells = n(),
    area_million_km2 = sum(cell_area_km2) / 1e6,
    mean_cropland_change_pp = weighted.mean(
      cropland_change_pp, cell_area_km2
    ),
    mean_bii_change_points = weighted.mean(
      bii_change_points, cell_area_km2
    ),
    .groups = "drop"
  ) %>%
  mutate(
    land_threshold_pp = land_threshold_pp,
    bii_threshold_points = bii_threshold_points
  )

write_csv(
  quadrant_summary,
  file.path(
    tradeoff_output_dir,
    paste0("grid_bii_food_cropland_responses_", target_year, ".csv")
  )
)

world <- ne_countries(scale = "small", returnclass = "sf")
quadrant_plot <- ggplot(quadrant_data, aes(x, y, fill = response)) +
  geom_tile(width = 0.5, height = 0.5) +
  geom_sf(
    data = world, fill = NA, colour = "grey30", linewidth = 0.15,
    inherit.aes = FALSE
  ) +
  scale_fill_manual(
    values = c(
      "BII gain / cropland loss" = "#1b9e77",
      "BII gain / cropland gain" = "#66a61e",
      "BII loss / cropland loss" = "#d95f02",
      "BII loss / cropland gain" = "#7570b3",
      "Small change" = "grey90"
    ),
    name = NULL, drop = FALSE
  ) +
  coord_sf(expand = FALSE) +
  theme_void(base_size = 10) +
  theme(legend.position = "bottom")

ggsave(
  file.path(
    tradeoff_output_dir,
    paste0("grid_bii_food_cropland_responses_", target_year, ".png")
  ),
  quadrant_plot, width = 11, height = 5.8, dpi = 300, bg = "white"
)
