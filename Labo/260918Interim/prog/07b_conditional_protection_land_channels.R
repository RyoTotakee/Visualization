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

analysis_root <- file.path(output_dir, "07_conditional_protection_decomposition")
grid_output_dir <- file.path(analysis_root, "02_grid_land")
channel_output_dir <- file.path(analysis_root, "03_observed_land_channels")
dir.create(grid_output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(channel_output_dir, recursive = TRUE, showWarnings = FALSE)

policy_states <- scenario_states("global2")
needed_states <- names(policy_states)
component_labels <- c(
  protection_main = "Protection main effect",
  protection_x_afforestation = "Protection × afforestation",
  protection_x_beccs = "Protection × BECCS",
  protection_x_afforestation_x_beccs = "Three-way interaction",
  protection_under_mitigation = "Total under mitigation"
)
component_colours <- c(
  "Protection main effect" = "#756bb1",
  "Protection × afforestation" = "#31a354",
  "Protection × BECCS" = "#e6550d",
  "Three-way interaction" = "#3182bd"
)

# Grid-level factorial decomposition of land allocation ----------------------

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

land_states <- lapply(needed_states, read_land_classes)
names(land_states) <- needed_states

baseline_file <- file.path(
  data_dir, "PLUM", "nc", paste0("AIM-LUmap_", policy_states[["000"]], ".nc")
)
baseline_all <- rast(baseline_file, subds = "LC_area_share")
mask_positions <- (0:11) * length(grid_years) + year_position
land_mask <- app(baseline_all[[mask_positions]], sum, na.rm = TRUE)
land_mask[land_mask <= 0] <- NA
pixel_area <- rast(baseline_file, subds = "pixel_area")

grid_components <- list(
  protection_main = land_states[["100"]] - land_states[["000"]],
  protection_x_afforestation =
    land_states[["110"]] - land_states[["100"]] -
    land_states[["010"]] + land_states[["000"]],
  protection_x_beccs =
    land_states[["101"]] - land_states[["100"]] -
    land_states[["001"]] + land_states[["000"]],
  protection_x_afforestation_x_beccs =
    land_states[["111"]] - land_states[["110"]] -
    land_states[["101"]] - land_states[["011"]] +
    land_states[["100"]] + land_states[["010"]] +
    land_states[["001"]] - land_states[["000"]],
  protection_under_mitigation = land_states[["111"]] - land_states[["011"]]
)

world <- ne_countries(scale = "small", returnclass = "sf")
area_summary <- list()

for (land_class in names(land_classes)) {
  component_rasters <- lapply(grid_components, function(x) x[[land_class]])
  component_stack <- rast(component_rasters) * 100
  names(component_stack) <- unname(component_labels[names(component_rasters)])
  component_stack <- mask(component_stack, land_mask)

  closure <- component_stack[["Total under mitigation"]] -
    sum(component_stack[[unname(component_labels[1:4])]])
  closure_max <- global(abs(closure), "max", na.rm = TRUE)[[1]]
  if (is.finite(closure_max) && closure_max > 1e-7) {
    stop("Grid conditional protection decomposition did not close.")
  }

  plot_data <- as.data.frame(
    component_stack, xy = TRUE, na.rm = TRUE, wide = FALSE
  ) %>%
    rename(component = layer, change_pp = values) %>%
    mutate(
      component = factor(component, levels = unname(component_labels))
    )
  fill_limit <- symmetric_limit(plot_data$change_pp)

  map_plot <- ggplot(plot_data, aes(x, y, fill = change_pp)) +
    geom_tile(width = 0.5, height = 0.5) +
    geom_sf(
      data = world, fill = NA, colour = "grey25", linewidth = 0.15,
      inherit.aes = FALSE
    ) +
    facet_wrap(vars(component), ncol = 2) +
    scale_fill_gradient2(
      low = "#8c2d04", mid = "#f7f7f7", high = "#0868ac",
      midpoint = 0, limits = c(-fill_limit, fill_limit), oob = squish,
      name = "Percentage\npoints"
    ) +
    coord_sf(expand = FALSE) +
    theme_void(base_size = 10) +
    theme(legend.position = "bottom", strip.text = element_text(face = "bold"))

  land_stub <- gsub("[^a-z0-9]+", "_", tolower(land_class))
  ggsave(
    file.path(grid_output_dir, paste0(land_stub, "_component_maps_", target_year, ".png")),
    map_plot, width = 11, height = 10.5, dpi = 300, bg = "white"
  )

  area_data <- global(
    (component_stack / 100) * pixel_area, "sum", na.rm = TRUE
  ) %>%
    tibble::rownames_to_column("component") %>%
    rename(change_million_ha = sum) %>%
    mutate(
      land_class = land_class,
      component = factor(component, levels = unname(component_labels))
    )
  area_summary[[land_class]] <- area_data
}

area_summary <- bind_rows(area_summary)
write_csv(
  area_summary,
  file.path(grid_output_dir, paste0("global_land_area_components_", target_year, ".csv"))
)

area_bars <- area_summary %>%
  filter(component != "Total under mitigation") %>%
  ggplot(aes(land_class, change_million_ha, fill = component)) +
  geom_hline(yintercept = 0, colour = "grey45", linewidth = 0.3) +
  geom_col(width = 0.65) +
  geom_point(
    data = area_summary %>% filter(component == "Total under mitigation"),
    aes(land_class, change_million_ha), inherit.aes = FALSE,
    shape = 18, size = 2.6, colour = "black"
  ) +
  coord_flip() +
  scale_fill_manual(values = component_colours, name = NULL) +
  labs(x = NULL, y = "Area contribution (million ha)") +
  theme_paper() +
  theme(panel.grid.major.y = element_blank())

ggsave(
  file.path(grid_output_dir, paste0("global_land_area_stacked_components_", target_year, ".png")),
  area_bars, width = 9, height = 5.2, dpi = 300, bg = "white"
)

rm(land_states, baseline_all, grid_components)
gc()

# Actual change in afforestation and BECCS area after protection -------------

land_channel_variables <- c(
  Lan_Cov_Frs_Aff = "Afforestation area",
  Lan_Cov_Cro_Ene_Cro_2nd_gen = "2G bioenergy area"
)
country_codes <- read_csv(
  file.path(data_dir, "country_mapping.csv"), show_col_types = FALSE
)$country

country_land_states <- rgdx.param(
  file.path(data_dir, "PLUM", "Mergedcomparison.gdx"), "IAMCTempwoU"
) %>%
  filter(
    i1 %in% unname(policy_states),
    i2 %in% country_codes,
    i3 %in% names(land_channel_variables),
    as.integer(as.character(i4)) == target_year
  ) %>%
  transmute(
    state = state_from_scenario(i1, "global2"),
    country = as.character(i2),
    land_channel = unname(land_channel_variables[as.character(i3)]),
    value
  ) %>%
  group_by(country, land_channel, state) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  complete(
    country = country_codes,
    land_channel = unname(land_channel_variables),
    state = needed_states,
    fill = list(value = 0)
  ) %>%
  pivot_wider(names_from = state, values_from = value) %>%
  conditional_protection_factorial() %>%
  mutate(
    protection_change_million_ha = protection_under_mitigation,
    displaced_area_million_ha = -protection_under_mitigation
  )

write_csv(
  country_land_states,
  file.path(channel_output_dir, paste0("country_land_channel_changes_", target_year, ".csv"))
)

world_map <- ne_countries(scale = "small", returnclass = "sf") %>%
  select(country = iso_a3)

for (channel_name in unname(land_channel_variables)) {
  map_values <- country_land_states %>%
    filter(land_channel == channel_name) %>%
    select(country, protection_change_million_ha)
  map_data <- world_map %>% inner_join(map_values, by = "country")
  fill_limit <- symmetric_limit(map_values$protection_change_million_ha)

  channel_map <- ggplot() +
    geom_sf(
      data = world_map, fill = "grey92", colour = "grey65", linewidth = 0.1
    ) +
    geom_sf(
      data = map_data, aes(fill = protection_change_million_ha),
      colour = "grey65", linewidth = 0.1
    ) +
    scale_fill_gradient2(
      low = "#8c2d04", mid = "#f7f7f7", high = "#0868ac",
      midpoint = 0, limits = c(-fill_limit, fill_limit), oob = squish,
      name = "million ha"
    ) +
    coord_sf(expand = FALSE) +
    theme_void(base_size = 10) +
    theme(legend.position = "bottom")

  channel_stub <- gsub("[^a-z0-9]+", "_", tolower(channel_name))
  ggsave(
    file.path(channel_output_dir, paste0(channel_stub, "_protection_change_map_", target_year, ".png")),
    channel_map, width = 10.5, height = 5.8, dpi = 300, bg = "white"
  )
}

# Compare actual land displacement with the corresponding interaction term.
agriculture_file <- file.path(
  analysis_root, "01_agriculture", "country",
  paste0("country_conditional_protection_decomposition_", target_year, ".csv")
)
agriculture_components <- read_csv(
  agriculture_file, show_col_types = FALSE
) %>%
  filter(
    item == "CRP",
    indicator %in% c("Area", "Production", "Producer price")
  )

channel_links <- bind_rows(
  country_land_states %>%
    filter(land_channel == "Afforestation area") %>%
    select(country, land_channel, protection_change_million_ha,
           displaced_area_million_ha) %>%
    inner_join(
      agriculture_components %>%
        select(country, indicator,
               interaction_effect_pct = protection_x_afforestation_pct),
      by = "country"
    ),
  country_land_states %>%
    filter(land_channel == "2G bioenergy area") %>%
    select(country, land_channel, protection_change_million_ha,
           displaced_area_million_ha) %>%
    inner_join(
      agriculture_components %>%
        select(country, indicator,
               interaction_effect_pct = protection_x_beccs_pct),
      by = "country"
    )
) %>%
  filter(is.finite(displaced_area_million_ha), is.finite(interaction_effect_pct))

write_csv(
  channel_links,
  file.path(channel_output_dir, paste0("land_displacement_agriculture_links_", target_year, ".csv"))
)

link_summary <- channel_links %>%
  group_by(land_channel, indicator) %>%
  summarise(
    countries = n(),
    spearman_correlation = cor(
      displaced_area_million_ha, interaction_effect_pct,
      method = "spearman", use = "complete.obs"
    ),
    pearson_correlation = cor(
      displaced_area_million_ha, interaction_effect_pct,
      method = "pearson", use = "complete.obs"
    ),
    linear_slope = coef(
      lm(interaction_effect_pct ~ displaced_area_million_ha)
    )[[2]],
    .groups = "drop"
  )

write_csv(
  link_summary,
  file.path(channel_output_dir, paste0("land_displacement_association_summary_", target_year, ".csv"))
)

display_links <- channel_links %>%
  group_by(land_channel, indicator) %>%
  filter(
    abs(displaced_area_million_ha) <=
      quantile(abs(displaced_area_million_ha), 0.98, na.rm = TRUE),
    abs(interaction_effect_pct) <=
      quantile(abs(interaction_effect_pct), 0.98, na.rm = TRUE)
  ) %>%
  ungroup()

link_plot <- ggplot(
  display_links,
  aes(displaced_area_million_ha, interaction_effect_pct)
) +
  geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.25) +
  geom_vline(xintercept = 0, colour = "grey60", linewidth = 0.25) +
  geom_point(colour = "#2c7fb8", alpha = 0.45, size = 0.9) +
  geom_smooth(method = "lm", se = FALSE, colour = "#cb181d", linewidth = 0.7) +
  facet_grid(indicator ~ land_channel, scales = "free") +
  labs(
    x = "Area displaced after protection (million ha)",
    y = "Corresponding interaction contribution\n(% of mitigation baseline)"
  ) +
  theme_paper() +
  theme(panel.grid.minor = element_blank())

ggsave(
  file.path(channel_output_dir, paste0("land_displacement_agriculture_links_", target_year, ".png")),
  link_plot, width = 11, height = 9, dpi = 300, bg = "white"
)
