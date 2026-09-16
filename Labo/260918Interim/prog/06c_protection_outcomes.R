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
outcome_output_dir <- file.path(protection_root, "04_climate_biodiversity")
synthesis_output_dir <- file.path(protection_root, "05_cross_domain_summary")
dir.create(outcome_output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(synthesis_output_dir, recursive = TRUE, showWarnings = FALSE)

policy_states <- scenario_states("global2")
needed_states <- c("000", "100", "011", "111")
contrast_levels <- c("Without mitigation", "Under mitigation", "Interaction")
world <- ne_countries(scale = "small", returnclass = "sf")

# BII: protection effect without and with land-based mitigation ---------------

bii_years <- c(2005, 2010, seq(2020, 2100, 10))
bii_year_position <- match(target_year, bii_years)

read_bii_stack <- function(state) {
  rast(
    file.path(
      data_dir, "BII", "nc",
      paste0("BIImap_", policy_states[[state]], "_default.nc")
    ),
    subds = "BII"
  )
}

bii_states <- lapply(needed_states, read_bii_stack)
names(bii_states) <- needed_states

bii_without <-
  bii_states[["100"]][[bii_year_position]] -
  bii_states[["000"]][[bii_year_position]]
bii_under <-
  bii_states[["111"]][[bii_year_position]] -
  bii_states[["011"]][[bii_year_position]]
bii_interaction <- bii_under - bii_without
bii_effects <- c(bii_without, bii_under, bii_interaction) * 100
names(bii_effects) <- contrast_levels

bii_map_data <- as.data.frame(
  bii_effects, xy = TRUE, na.rm = TRUE, wide = FALSE
) %>%
  rename(contrast = layer, change_bii_points = values) %>%
  mutate(contrast = factor(contrast, levels = contrast_levels))
bii_limit <- symmetric_limit(bii_map_data$change_bii_points)

bii_map <- ggplot(bii_map_data, aes(x, y, fill = change_bii_points)) +
  geom_tile(width = 0.5, height = 0.5) +
  geom_sf(
    data = world, fill = NA, colour = "grey25", linewidth = 0.15,
    inherit.aes = FALSE
  ) +
  facet_wrap(vars(contrast), ncol = 1) +
  scale_fill_gradient2(
    low = "#8c2d04", mid = "#f7f7f7", high = "#238b45",
    midpoint = 0, limits = c(-bii_limit, bii_limit), oob = squish,
    name = "BII points"
  ) +
  coord_sf(expand = FALSE) +
  theme_void(base_size = 10) +
  theme(legend.position = "bottom", strip.text = element_text(face = "bold"))

ggsave(
  file.path(outcome_output_dir, paste0("bii_protection_context_maps_", target_year, ".png")),
  bii_map, width = 10.5, height = 10.5, dpi = 300, bg = "white"
)

# Area-weighted global BII effect for every available year.
cell_area <- cellSize(bii_states[["000"]][[1]], unit = "km")
area_weighted_mean <- function(raster_value) {
  valid_area <- ifel(is.na(raster_value), NA, cell_area)
  weighted_sum <- global(raster_value * valid_area, "sum", na.rm = TRUE)[[1]]
  area_sum <- global(valid_area, "sum", na.rm = TRUE)[[1]]
  weighted_sum / area_sum
}

bii_trajectory <- lapply(seq_along(bii_years), function(i) {
  effect_without <- bii_states[["100"]][[i]] - bii_states[["000"]][[i]]
  effect_under <- bii_states[["111"]][[i]] - bii_states[["011"]][[i]]
  tibble(
    year = bii_years[[i]],
    `Without mitigation` = 100 * area_weighted_mean(effect_without),
    `Under mitigation` = 100 * area_weighted_mean(effect_under),
    Interaction = 100 * area_weighted_mean(effect_under - effect_without)
  )
}) %>%
  bind_rows() %>%
  pivot_longer(-year, names_to = "contrast", values_to = "change_bii_points") %>%
  mutate(contrast = factor(contrast, levels = contrast_levels))

write_csv(
  bii_trajectory,
  file.path(outcome_output_dir, "world_bii_protection_trajectories.csv")
)

bii_trajectory_plot <- ggplot(
  bii_trajectory,
  aes(year, change_bii_points, colour = contrast, linetype = contrast)
) +
  geom_hline(yintercept = 0, colour = "grey55", linewidth = 0.25) +
  geom_line(linewidth = 0.85) +
  scale_colour_manual(
    values = c("#807dba", "#238b45", "#e6550d"), name = NULL
  ) +
  scale_linetype_manual(values = c("dashed", "solid", "dotdash"), name = NULL) +
  labs(x = NULL, y = "Global mean protection effect (BII points)") +
  theme_paper()

ggsave(
  file.path(outcome_output_dir, "world_bii_protection_trajectories.png"),
  bii_trajectory_plot, width = 8.5, height = 5.2, dpi = 300, bg = "white"
)

rm(bii_states, bii_effects, bii_map_data)
gc()

# Net LUC emissions: direct protection effect --------------------------------

merged_file <- file.path(data_dir, "PLUM", "Mergedcomparison.gdx")
land_emissions <- rgdx.param(merged_file, "GHGL") %>%
  filter(
    i1 %in% policy_states[needed_states],
    i4 == "Net", i5 == "LUC",
    as.integer(as.character(i3)) >= 2020
  ) %>%
  transmute(
    state = state_from_scenario(i1, "global2"),
    region = as.character(i2), year = as.integer(as.character(i3)), value
  )

emissions_world <- land_emissions %>%
  filter(region == "World") %>%
  select(year, state, value) %>%
  pivot_wider(names_from = state, values_from = value) %>%
  transmute(
    year,
    `Without mitigation` = -(`100` - `000`),
    `Under mitigation` = -(`111` - `011`),
    Interaction = -((`111` - `011`) - (`100` - `000`))
  ) %>%
  pivot_longer(
    -year, names_to = "contrast", values_to = "emission_reduction"
  ) %>%
  mutate(contrast = factor(contrast, levels = contrast_levels))

write_csv(
  emissions_world,
  file.path(outcome_output_dir, "world_luc_emission_protection_trajectories.csv")
)

emissions_trajectory_plot <- ggplot(
  emissions_world,
  aes(year, emission_reduction, colour = contrast, linetype = contrast)
) +
  geom_hline(yintercept = 0, colour = "grey55", linewidth = 0.25) +
  geom_line(linewidth = 0.85) +
  scale_colour_manual(
    values = c("#807dba", "#238b45", "#e6550d"), name = NULL
  ) +
  scale_linetype_manual(values = c("dashed", "solid", "dotdash"), name = NULL) +
  labs(
    x = NULL,
    y = expression("Protection-induced LUC emission reduction (Mt CO"[2] * "/yr)")
  ) +
  theme_paper()

ggsave(
  file.path(outcome_output_dir, "world_luc_emission_protection_trajectories.png"),
  emissions_trajectory_plot, width = 8.5, height = 5.2, dpi = 300, bg = "white"
)

country_codes <- read_csv(
  file.path(data_dir, "country_mapping.csv"), show_col_types = FALSE
) %>%
  distinct(country) %>%
  pull(country)

emissions_country <- land_emissions %>%
  filter(region %in% country_codes, year == target_year) %>%
  select(country = region, state, value) %>%
  complete(country, state = needed_states, fill = list(value = 0)) %>%
  pivot_wider(names_from = state, values_from = value) %>%
  transmute(
    country,
    `Without mitigation` = -(`100` - `000`),
    `Under mitigation` = -(`111` - `011`),
    Interaction = -((`111` - `011`) - (`100` - `000`))
  )

write_csv(
  emissions_country,
  file.path(outcome_output_dir, paste0("country_luc_emission_protection_", target_year, ".csv"))
)

emission_map_values <- emissions_country %>%
  pivot_longer(-country, names_to = "contrast", values_to = "emission_reduction") %>%
  mutate(contrast = factor(contrast, levels = contrast_levels))
emission_map_world <- world %>% select(country = iso_a3)
emission_map_data <- emission_map_world %>%
  inner_join(emission_map_values, by = "country")
emission_limit <- symmetric_limit(emission_map_values$emission_reduction)

emission_map <- ggplot() +
  geom_sf(
    data = emission_map_world, fill = "grey92", colour = "grey65", linewidth = 0.1
  ) +
  geom_sf(
    data = emission_map_data, aes(fill = emission_reduction),
    colour = "grey65", linewidth = 0.1
  ) +
  facet_wrap(vars(contrast), ncol = 1) +
  scale_fill_gradient2(
    low = "#8c2d04", mid = "#f7f7f7", high = "#238b45",
    midpoint = 0, limits = c(-emission_limit, emission_limit), oob = squish,
    name = expression("Mt CO"[2] * "/yr")
  ) +
  coord_sf(expand = FALSE) +
  theme_void(base_size = 10) +
  theme(legend.position = "bottom", strip.text = element_text(face = "bold"))

ggsave(
  file.path(
    outcome_output_dir,
    paste0("country_luc_emission_protection_maps_", target_year, ".png")
  ),
  emission_map, width = 10.5, height = 10.5, dpi = 300, bg = "white"
)

# The policy input itself: expansion of integrated protected area ------------

protected_area_trajectory <- rgdx.param(merged_file, "IAMCTempwoU") %>%
  filter(
    i1 %in% policy_states[needed_states],
    i2 == "WLD",
    i3 == "Lan_Cov_Pro_Are_Prm_Sec_Frs_Gra_Int",
    as.integer(as.character(i4)) >= 2020
  ) %>%
  transmute(
    state = state_from_scenario(i1, "global2"),
    year = as.integer(as.character(i4)),
    protected_area = value,
    scenario = factor(
      c(
        `000` = "BaU", `100` = "Protection only",
        `011` = "Mitigation", `111` = "Protection + mitigation"
      )[state],
      levels = c("BaU", "Protection only", "Mitigation", "Protection + mitigation")
    )
  )

write_csv(
  protected_area_trajectory,
  file.path(outcome_output_dir, "world_integrated_protected_area_trajectories.csv")
)

protected_area_plot <- ggplot(
  protected_area_trajectory,
  aes(year, protected_area, colour = scenario, linetype = scenario)
) +
  geom_line(linewidth = 0.85) +
  scale_colour_manual(
    values = c("#636363", "#807dba", "#31a354", "#2171b5"), name = NULL
  ) +
  scale_linetype_manual(
    values = c("solid", "dashed", "dotdash", "solid"), name = NULL
  ) +
  labs(x = NULL, y = "Integrated protected area (million ha)") +
  theme_paper()

ggsave(
  file.path(outcome_output_dir, "world_integrated_protected_area_trajectories.png"),
  protected_area_plot, width = 8.5, height = 5.2, dpi = 300, bg = "white"
)

# Cross-domain physical-effect overview at the selected year -----------------

land_summary <- read_csv(
  file.path(
    protection_root, "01_land_competition",
    paste0("world_land_area_effects_", target_year, ".csv")
  ),
  show_col_types = FALSE
) %>%
  transmute(
    indicator = paste0(land_class, " (million ha)"),
    contrast, effect = change_million_ha
  )

emission_summary <- emissions_world %>%
  filter(year == target_year) %>%
  transmute(
    indicator = "LUC emission reduction (Mt CO2/yr)",
    contrast = as.character(contrast), effect = emission_reduction
  )

bii_summary <- bii_trajectory %>%
  filter(year == target_year) %>%
  transmute(
    indicator = "Global mean BII (points)",
    contrast = as.character(contrast), effect = change_bii_points
  )

protected_area_effects <- protected_area_trajectory %>%
  select(state, year, protected_area) %>%
  pivot_wider(names_from = state, values_from = protected_area) %>%
  filter(year == target_year) %>%
  transmute(
    `Without mitigation` = `100` - `000`,
    `Under mitigation` = `111` - `011`,
    Interaction = (`111` - `011`) - (`100` - `000`)
  ) %>%
  pivot_longer(everything(), names_to = "contrast", values_to = "effect") %>%
  mutate(indicator = "Integrated protected area (million ha)")

cross_domain <- bind_rows(
  protected_area_effects, land_summary, emission_summary, bii_summary
) %>%
  mutate(contrast = factor(contrast, levels = contrast_levels))

write_csv(
  cross_domain,
  file.path(synthesis_output_dir, paste0("world_cross_domain_effects_", target_year, ".csv"))
)

cross_domain_plot <- ggplot(cross_domain, aes(contrast, effect, fill = contrast)) +
  geom_hline(yintercept = 0, colour = "grey45", linewidth = 0.25) +
  geom_col(width = 0.68) +
  facet_wrap(vars(indicator), scales = "free_y", ncol = 3) +
  scale_fill_manual(
    values = c("#807dba", "#238b45", "#e6550d"), guide = "none"
  ) +
  labs(x = NULL, y = "Protection effect") +
  theme_paper() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

ggsave(
  file.path(
    synthesis_output_dir,
    paste0("world_cross_domain_effects_", target_year, ".png")
  ),
  cross_domain_plot, width = 12, height = 7.5, dpi = 300, bg = "white"
)
