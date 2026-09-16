suppressPackageStartupMessages({
  library(gdxrrw)
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
init_gdx()

protection_root <- file.path(output_dir, "06_protection_focus")
land_output_dir <- file.path(protection_root, "01_land_competition")
yield_output_dir <- file.path(protection_root, "02_yield_displacement")
dir.create(land_output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(yield_output_dir, recursive = TRUE, showWarnings = FALSE)

protection_labels <- c(
  protection_without_mitigation = "Without mitigation",
  protection_under_mitigation = "Under mitigation",
  protection_interaction = "Interaction"
)

# Protection-induced spatial land reallocation -------------------------------

grid_scenarios <- scenario_states("global2")
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
    paste0("AIM-LUmap_", grid_scenarios[[state]], ".nc")
  )
  all_layers <- rast(nc_path, subds = "LC_area_share")
  positions <- (unname(land_classes) - 1L) * length(grid_years) + year_position
  selected <- all_layers[[positions]] / grid_export_duplicate_factor
  names(selected) <- names(land_classes)
  selected
}

land_states <- lapply(c("000", "100", "011", "111"), read_land_classes)
names(land_states) <- c("000", "100", "011", "111")

baseline_file <- file.path(
  data_dir, "PLUM", "nc", paste0("AIM-LUmap_", grid_scenarios[["000"]], ".nc")
)
baseline_all <- rast(baseline_file, subds = "LC_area_share")
mask_positions <- (0:11) * length(grid_years) + year_position
land_mask <- app(baseline_all[[mask_positions]], sum, na.rm = TRUE)
land_mask[land_mask <= 0] <- NA
pixel_area <- rast(baseline_file, subds = "pixel_area")
world <- ne_countries(scale = "small", returnclass = "sf")

land_effects <- list(
  protection_without_mitigation = land_states[["100"]] - land_states[["000"]],
  protection_under_mitigation = land_states[["111"]] - land_states[["011"]],
  protection_interaction =
    (land_states[["111"]] - land_states[["011"]]) -
    (land_states[["100"]] - land_states[["000"]])
)

land_area_summary <- list()
for (land_class in names(land_classes)) {
  class_position <- match(land_class, names(land_classes))
  class_effects <- rast(lapply(land_effects, function(x) x[[class_position]]))
  names(class_effects) <- unname(protection_labels)
  class_effects <- mask(class_effects * 100, land_mask)

  map_data <- as.data.frame(
    class_effects, xy = TRUE, na.rm = TRUE, wide = FALSE
  ) %>%
    rename(contrast = layer, change_pp = values) %>%
    mutate(contrast = factor(contrast, levels = unname(protection_labels)))
  fill_limit <- symmetric_limit(map_data$change_pp)

  map_plot <- ggplot(map_data, aes(x, y, fill = change_pp)) +
    geom_tile(width = 0.5, height = 0.5) +
    geom_sf(
      data = world, fill = NA, colour = "grey25", linewidth = 0.15,
      inherit.aes = FALSE
    ) +
    facet_wrap(vars(contrast), ncol = 1) +
    scale_fill_gradient2(
      low = "#8c2d04", mid = "#f7f7f7", high = "#0868ac",
      midpoint = 0, limits = c(-fill_limit, fill_limit), oob = squish,
      name = "Percentage\npoints"
    ) +
    coord_sf(expand = FALSE) +
    theme_void(base_size = 10) +
    theme(legend.position = "bottom", strip.text = element_text(face = "bold"))

  class_stub <- gsub("[^a-z0-9]+", "_", tolower(land_class))
  ggsave(
    file.path(
      land_output_dir,
      paste0(class_stub, "_protection_context_maps_", target_year, ".png")
    ),
    map_plot, width = 10.5, height = 10.5, dpi = 300, bg = "white"
  )

  class_area <- global(
    (class_effects / 100) * pixel_area, "sum", na.rm = TRUE
  ) %>%
    tibble::rownames_to_column("contrast") %>%
    rename(change_million_ha = sum) %>%
    mutate(land_class = land_class, .before = 1)
  land_area_summary[[land_class]] <- class_area
}

land_area_summary <- bind_rows(land_area_summary) %>%
  mutate(
    contrast = factor(contrast, levels = unname(protection_labels)),
    land_class = factor(land_class, levels = names(land_classes))
  )
write_csv(
  land_area_summary,
  file.path(land_output_dir, paste0("world_land_area_effects_", target_year, ".csv"))
)

land_area_plot <- ggplot(
  land_area_summary,
  aes(land_class, change_million_ha, fill = contrast)
) +
  geom_hline(yintercept = 0, colour = "grey45", linewidth = 0.3) +
  geom_col(position = position_dodge(width = 0.78), width = 0.7) +
  scale_fill_manual(
    values = c("#807dba", "#238b45", "#e6550d"), name = NULL
  ) +
  labs(x = NULL, y = "Protection effect (million ha)") +
  theme_paper() +
  theme(panel.grid.major.x = element_blank())

ggsave(
  file.path(land_output_dir, paste0("world_land_area_effects_", target_year, ".png")),
  land_area_plot, width = 8.5, height = 5.2, dpi = 300, bg = "white"
)

rm(land_states, land_effects, baseline_all, land_mask)
gc()

# World land-allocation effects through time ---------------------------------

land_variables <- c(
  Lan_Cov_Cro_Non_Ene_Cro = "Food cropland",
  Lan_Cov_Cro_Ene_Cro_2nd_gen = "2G bioenergy",
  Lan_Cov_Frs_Aff = "Planted forest"
)

land_trajectory <- rgdx.param(
  file.path(data_dir, "PLUM", "Mergedcomparison.gdx"), "IAMCTempwoU"
) %>%
  filter(
    i1 %in% grid_scenarios[c("000", "100", "011", "111")],
    i2 == "WLD",
    i3 %in% names(land_variables),
    as.integer(as.character(i4)) >= 2020
  ) %>%
  transmute(
    state = state_from_scenario(i1, "global2"),
    land_class = unname(land_variables[as.character(i3)]),
    year = as.integer(as.character(i4)),
    value
  ) %>%
  complete(
    nesting(land_class, year),
    state = c("000", "100", "011", "111"),
    fill = list(value = 0)
  ) %>%
  pivot_wider(names_from = state, values_from = value) %>%
  transmute(
    land_class,
    year,
    `Without mitigation` = `100` - `000`,
    `Under mitigation` = `111` - `011`,
    Interaction = (`111` - `011`) - (`100` - `000`)
  ) %>%
  pivot_longer(
    -c(land_class, year), names_to = "contrast", values_to = "change_million_ha"
  ) %>%
  mutate(
    land_class = factor(land_class, levels = unname(land_variables)),
    contrast = factor(contrast, levels = unname(protection_labels))
  )

write_csv(
  land_trajectory,
  file.path(land_output_dir, "world_land_area_protection_trajectories.csv")
)

land_trajectory_plot <- ggplot(
  land_trajectory,
  aes(year, change_million_ha, colour = contrast, linetype = contrast)
) +
  geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.25) +
  geom_line(linewidth = 0.8) +
  facet_wrap(vars(land_class), ncol = 1, scales = "free_y") +
  scale_colour_manual(
    values = c("#807dba", "#238b45", "#e6550d"), name = NULL
  ) +
  scale_linetype_manual(values = c("dashed", "solid", "dotdash"), name = NULL) +
  labs(x = NULL, y = "Protection effect (million ha)") +
  theme_paper()

ggsave(
  file.path(land_output_dir, "world_land_area_protection_trajectories.png"),
  land_trajectory_plot, width = 8.5, height = 8.5, dpi = 300, bg = "white"
)

# Does protection move harvested area across yield levels? -------------------

alpha_file <- file.path(data_dir, "ALPHA", "all_scenario_World.gdx")
alpha_scenarios <- scenario_states("global2")

baseline_yield <- rgdx.param(alpha_file, "yldBas_load") %>%
  filter(
    SCENARIO == alpha_scenarios[["000"]],
    as.integer(as.character(Y)) == target_year,
    c %in% selected_crop_codes
  ) %>%
  transmute(
    crop = as.character(c), Basin = as.character(bas),
    country = as.character(cty), baseline_yield = yldBas_load
  )

basin_area_effects <- rgdx.param(alpha_file, "qlBas_load") %>%
  filter(
    SCENARIO %in% alpha_scenarios[c("000", "100", "011", "111")],
    as.integer(as.character(Y)) == target_year,
    c %in% selected_crop_codes
  ) %>%
  transmute(
    state = state_from_scenario(SCENARIO, "global2"),
    crop = as.character(c), Basin = as.character(bas),
    country = as.character(cty), value = qlBas_load
  ) %>%
  pivot_wider(names_from = state, values_from = value, values_fill = 0) %>%
  inner_join(baseline_yield, by = c("crop", "Basin", "country")) %>%
  filter(is.finite(baseline_yield), baseline_yield > 0, `000` > 0) %>%
  transmute(
    crop, Basin, country, baseline_yield, baseline_area = `000`,
    `Without mitigation` = `100` - `000`,
    `Under mitigation` = `111` - `011`,
    Interaction = (`111` - `011`) - (`100` - `000`)
  ) %>%
  group_by(crop) %>%
  mutate(yield_quintile = ntile(baseline_yield, 5)) %>%
  ungroup() %>%
  mutate(crop_label = unname(commodity_labels[crop]))

write_csv(
  basin_area_effects,
  file.path(yield_output_dir, paste0("basin_area_reallocation_", target_year, ".csv"))
)

quintile_summary <- basin_area_effects %>%
  pivot_longer(
    all_of(unname(protection_labels)),
    names_to = "contrast", values_to = "area_change"
  ) %>%
  group_by(crop, crop_label, yield_quintile, contrast) %>%
  summarise(area_change = sum(area_change, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    yield_quintile = factor(
      yield_quintile, levels = 1:5,
      labels = c("Q1 low", "Q2", "Q3", "Q4", "Q5 high")
    ),
    contrast = factor(contrast, levels = unname(protection_labels))
  )

write_csv(
  quintile_summary,
  file.path(yield_output_dir, paste0("yield_quintile_reallocation_", target_year, ".csv"))
)

quintile_plot <- ggplot(
  quintile_summary,
  aes(yield_quintile, area_change, fill = contrast)
) +
  geom_hline(yintercept = 0, colour = "grey45", linewidth = 0.25) +
  geom_col(position = position_dodge(width = 0.78), width = 0.7) +
  facet_wrap(vars(crop_label), scales = "free_y", ncol = 3) +
  scale_fill_manual(
    values = c("#807dba", "#238b45", "#e6550d"), name = NULL
  ) +
  labs(x = "BaU basin yield quintile", y = "Harvested-area change (1000 ha)") +
  theme_paper() +
  theme(axis.text.x = element_text(angle = 35, hjust = 1))

ggsave(
  file.path(yield_output_dir, paste0("yield_quintile_reallocation_", target_year, ".png")),
  quintile_plot, width = 12, height = 7.2, dpi = 300, bg = "white"
)

source_sink_summary <- basin_area_effects %>%
  pivot_longer(
    all_of(unname(protection_labels)[1:2]),
    names_to = "contrast", values_to = "area_change"
  ) %>%
  filter(abs(area_change) > 1e-10) %>%
  mutate(direction = if_else(area_change < 0, "Area loss", "Area gain")) %>%
  group_by(crop, crop_label, contrast, direction) %>%
  summarise(
    reallocated_area = sum(abs(area_change)),
    weighted_bau_yield = weighted.mean(
      baseline_yield, abs(area_change), na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  mutate(
    contrast = factor(contrast, levels = unname(protection_labels)[1:2]),
    direction = factor(direction, levels = c("Area loss", "Area gain"))
  )

write_csv(
  source_sink_summary,
  file.path(yield_output_dir, paste0("source_sink_yield_levels_", target_year, ".csv"))
)

source_sink_plot <- ggplot(
  source_sink_summary,
  aes(contrast, weighted_bau_yield, fill = direction)
) +
  geom_col(position = position_dodge(width = 0.75), width = 0.68) +
  facet_wrap(vars(crop_label), scales = "free_y", ncol = 3) +
  scale_fill_manual(values = c("#b2182b", "#2166ac"), name = NULL) +
  labs(x = NULL, y = "Area-change-weighted BaU yield (t/ha)") +
  theme_paper() +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))

ggsave(
  file.path(yield_output_dir, paste0("source_sink_yield_levels_", target_year, ".png")),
  source_sink_plot, width = 11, height = 7.2, dpi = 300, bg = "white"
)
