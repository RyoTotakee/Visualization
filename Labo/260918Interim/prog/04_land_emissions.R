suppressPackageStartupMessages({
  library(gdxrrw)
  library(dplyr)
  library(ggplot2)
  library(readr)
  library(rnaturalearth)
  library(scales)
  library(sf)
  library(tidyr)
})

source("prog/common.R")
init_gdx()

land_output_dir <- file.path(output_dir, "04_land_emissions")
dir.create(land_output_dir, recursive = TRUE, showWarnings = FALSE)

merged_file <- file.path(data_dir, "PLUM", "Mergedcomparison.gdx")
land_scenarios <- scenario_states("global2")
country_codes <- read_csv(
  file.path(data_dir, "country_mapping.csv"), show_col_types = FALSE
) %>%
  distinct(country) %>%
  pull(country)

# Net land-use-change emissions ----------------------------------------------

land_emissions <- rgdx.param(merged_file, "GHGL") %>%
  filter(
    i1 %in% land_scenarios,
    i4 == "Net",
    i5 == "LUC",
    as.integer(as.character(i3)) >= 2020
  ) %>%
  transmute(
    state = state_from_scenario(i1, "global2"),
    region = as.character(i2),
    year = as.integer(as.character(i3)),
    value
  )

trajectory_labels <- c(
  "000" = "BaU",
  "100" = "Protected areas",
  "011" = "Afforestation + BECCS",
  "111" = "All three policies"
)

emissions_world <- land_emissions %>%
  filter(region == "World", state %in% names(trajectory_labels)) %>%
  mutate(scenario = factor(trajectory_labels[state], levels = trajectory_labels))

emissions_plot <- ggplot(
  emissions_world,
  aes(year, value, colour = scenario, linetype = scenario)
) +
  geom_hline(yintercept = 0, colour = "grey50", linewidth = 0.3) +
  geom_line(linewidth = 0.8) +
  scale_colour_manual(values = c("#636363", "#756bb1", "#31a354", "#2171b5"), name = NULL) +
  scale_linetype_manual(values = c("solid", "dashed", "dotdash", "solid"), name = NULL) +
  labs(x = NULL, y = expression("Net LUC emissions (Mt CO"[2] * "/yr)")) +
  theme_paper()

ggsave(
  file.path(land_output_dir, "world_net_luc_emissions_trajectories.png"),
  emissions_plot, width = 8.5, height = 5.2, dpi = 300, bg = "white"
)

# Country contributions are expressed as emission reductions, so positive
# values mean lower net emissions than BaU.
emissions_country <- land_emissions %>%
  filter(region %in% country_codes, year == target_year) %>%
  select(country = region, state, value) %>%
  complete(country, state = names(land_scenarios), fill = list(value = 0)) %>%
  pivot_wider(names_from = state, values_from = value) %>%
  shapley_three() %>%
  transmute(
    country,
    protection = -protection,
    afforestation = -afforestation,
    beccs = -beccs,
    total_reduction = -total,
    bau_emissions = baseline,
    all_policy_emissions = final
  )

write_csv(
  emissions_country,
  file.path(land_output_dir, paste0("country_luc_emission_shapley_", target_year, ".csv"))
)

policy_labels <- c(
  protection = "Protected areas",
  afforestation = "Afforestation",
  beccs = "BECCS"
)
emission_map_values <- emissions_country %>%
  select(country, all_of(names(policy_labels))) %>%
  pivot_longer(
    all_of(names(policy_labels)),
    names_to = "policy", values_to = "reduction"
  ) %>%
  mutate(policy = factor(policy_labels[policy], levels = policy_labels))

world <- ne_countries(scale = "small", returnclass = "sf") %>%
  select(country = iso_a3)
emission_map_data <- world %>% inner_join(emission_map_values, by = "country")
emission_limit <- symmetric_limit(emission_map_values$reduction)

emission_map <- ggplot() +
  geom_sf(data = world, fill = "grey92", colour = "grey65", linewidth = 0.1) +
  geom_sf(
    data = emission_map_data, aes(fill = reduction),
    colour = "grey65", linewidth = 0.1
  ) +
  facet_wrap(vars(policy), ncol = 1) +
  scale_fill_gradient2(
    low = "#8c2d04", mid = "#f7f7f7", high = "#238b45",
    midpoint = 0, limits = c(-emission_limit, emission_limit), oob = squish,
    na.value = "grey92", name = expression("Mt CO"[2] * "/yr")
  ) +
  coord_sf(expand = FALSE) +
  theme_void(base_size = 10) +
  theme(legend.position = "bottom", strip.text = element_text(face = "bold"))

ggsave(
  file.path(land_output_dir, paste0("country_luc_emission_shapley_maps_", target_year, ".png")),
  emission_map, width = 10.5, height = 10.5, dpi = 300, bg = "white"
)

emission_top <- emissions_country %>%
  slice_max(abs(total_reduction), n = 20, with_ties = FALSE) %>%
  arrange(total_reduction) %>%
  mutate(country = factor(country, levels = country))

emission_bars <- emission_top %>%
  select(country, all_of(names(policy_labels))) %>%
  pivot_longer(
    all_of(names(policy_labels)),
    names_to = "policy", values_to = "reduction"
  ) %>%
  mutate(policy = factor(policy_labels[policy], levels = policy_labels)) %>%
  ggplot(aes(country, reduction, fill = policy)) +
  geom_hline(yintercept = 0, colour = "grey45", linewidth = 0.3) +
  geom_col(width = 0.72) +
  geom_point(
    data = emission_top,
    aes(country, total_reduction), inherit.aes = FALSE,
    shape = 18, size = 2.2, colour = "black"
  ) +
  coord_flip() +
  scale_fill_manual(values = c("#756bb1", "#31a354", "#e6550d"), name = NULL) +
  labs(x = NULL, y = expression("Emission reduction (Mt CO"[2] * "/yr)")) +
  theme_paper() +
  theme(panel.grid.major.y = element_blank())

ggsave(
  file.path(land_output_dir, paste0("country_luc_emission_shapley_bars_", target_year, ".png")),
  emission_bars, width = 8.5, height = 7, dpi = 300, bg = "white"
)

rm(land_emissions, emissions_country, emission_map_data)
gc()

# World land allocation -------------------------------------------------------

land_variables <- c(
  Lan_Cov_Cro_Non_Ene_Cro = "Food cropland",
  Lan_Cov_Cro_Ene_Cro_2nd_gen = "2G bioenergy",
  Lan_Cov_Frs_Aff = "Afforestation"
)

land_cover_world <- rgdx.param(merged_file, "IAMCTempwoU") %>%
  filter(
    i1 %in% land_scenarios[names(trajectory_labels)],
    i2 == "WLD",
    i3 %in% names(land_variables),
    as.integer(as.character(i4)) >= 2020
  ) %>%
  transmute(
    state = state_from_scenario(i1, "global2"),
    land_class = factor(
      land_variables[as.character(i3)],
      levels = unname(land_variables)
    ),
    year = as.integer(as.character(i4)),
    value,
    scenario = factor(trajectory_labels[state], levels = trajectory_labels)
  )

land_cover_plot <- ggplot(
  land_cover_world,
  aes(year, value, colour = scenario, linetype = scenario)
) +
  geom_line(linewidth = 0.75) +
  facet_wrap(vars(land_class), scales = "free_y", ncol = 1) +
  scale_colour_manual(values = c("#636363", "#756bb1", "#31a354", "#2171b5"), name = NULL) +
  scale_linetype_manual(values = c("solid", "dashed", "dotdash", "solid"), name = NULL) +
  labs(x = NULL, y = "Area (million ha)") +
  theme_paper() +
  theme(legend.position = "bottom")

ggsave(
  file.path(land_output_dir, "world_land_allocation_trajectories.png"),
  land_cover_plot, width = 8.5, height = 9, dpi = 300, bg = "white"
)
