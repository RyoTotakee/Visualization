suppressPackageStartupMessages({
  library(dplyr)
  library(gdxrrw)
  library(ggplot2)
  library(readr)
  library(scales)
  library(tidyr)
})

source("prog/common.R")
init_gdx()

analysis_root <- file.path(output_dir, "14_luc_emissions_balance")
unlink(analysis_root, recursive = TRUE)
world_root <- file.path(analysis_root, "01_world")
r10_root <- file.path(analysis_root, "02_r10")
source_root <- file.path(analysis_root, "03_removal_sources")
diagnostic_root <- file.path(analysis_root, "04_protection_diagnostics")
spike_root <- file.path(analysis_root, "05_2070_spike_diagnostics")
for (path in c(world_root, r10_root, source_root, diagnostic_root, spike_root)) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
}

emission_theme <- theme_paper() +
  theme(
    plot.title = element_text(face = "bold", size = 14, margin = margin(b = 6)),
    plot.subtitle = element_text(size = 10.5, colour = "grey30", margin = margin(b = 8)),
    plot.caption = element_text(size = 8.3, colour = "grey35", hjust = 0),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.margin = margin(14, 14, 12, 14)
  )

states <- scenario_states("global2")
scenario_labels <- c(
  `000` = "No policy",
  `100` = "Protection",
  `010` = "Afforestation",
  `001` = "BECCS",
  `110` = "Protection +\nafforestation",
  `101` = "Protection +\nBECCS",
  `011` = "Both CDR",
  `111` = "All three"
)
r10_labels <- c(
  R10AFRICA = "Africa", `R10CHINA+` = "China+", R10EUROPE = "Europe",
  `R10INDIA+` = "India+", R10LATIN_AM = "Latin America",
  R10MIDDLE_EAST = "Middle East", R10NORTH_AM = "North America",
  R10PAC_OECD = "Pacific OECD", R10REF_ECON = "Reforming economies",
  R10REST_ASIA = "Rest of Asia"
)
balance_colours <- c(
  "Gross positive emissions" = "#D55E00",
  "Gross removals" = "#0072B2"
)

integrate_trapezoid <- function(year, value) {
  ordering <- order(year)
  year <- year[ordering]
  value <- value[ordering]
  if (length(year) < 2L) return(NA_real_)
  sum(diff(year) * (head(value, -1) + tail(value, -1)) / 2)
}

# 1. Read gross positive, gross negative, and net LUC flows -----------------

merged_file <- file.path(data_dir, "PLUM", "Mergedcomparison.gdx")
analysis_years <- seq(2020L, target_year, by = 10L)
if (!target_year %in% analysis_years) analysis_years <- sort(c(analysis_years, target_year))
geographies <- c("World", names(r10_labels))

ghgl_all <- rgdx.param(merged_file, "GHGL") %>%
  transmute(
    scenario = as.character(i1), region = as.character(i2),
    year = as.integer(as.character(i3)), emission_category = as.character(i4),
    land_category = as.character(i5), value_mtco2_yr = as.numeric(value)
  ) %>%
  filter(
    scenario %in% unname(states), region %in% geographies,
    year %in% analysis_years
  ) %>%
  mutate(state = state_from_scenario(scenario, "global2"))

luc_reported <- ghgl_all %>%
  filter(
    land_category == "LUC",
    emission_category %in% c("Positive", "Negative", "Net")
  ) %>%
  select(state, region, year, emission_category, value_mtco2_yr) %>%
  complete(
    state = names(states), region = geographies, year = analysis_years,
    emission_category = c("Positive", "Negative", "Net"),
    fill = list(value_mtco2_yr = 0)
  )

managed_forest <- ghgl_all %>%
  filter(emission_category == "Negative", land_category == "MNGFRS") %>%
  select(state, region, year, managed_forest_removal_mtco2_yr = value_mtco2_yr) %>%
  complete(
    state = names(states), region = geographies, year = analysis_years,
    fill = list(managed_forest_removal_mtco2_yr = 0)
  )

luc_balance <- luc_reported %>%
  pivot_wider(names_from = emission_category, values_from = value_mtco2_yr) %>%
  rename(Negative_reported = Negative, Net_reported = Net) %>%
  left_join(managed_forest, by = c("state", "region", "year")) %>%
  mutate(
    Negative = Negative_reported - managed_forest_removal_mtco2_yr,
    Net = Positive + Negative,
    net_calculated_mtco2_yr = Positive + Negative,
    reported_net_residual_mtco2_yr =
      Net_reported - (Positive + Negative_reported),
    scenario = factor(unname(scenario_labels[state]), levels = unname(scenario_labels)),
    region_label = if_else(region == "World", "World", unname(r10_labels[region]))
  )
write_csv(
  luc_balance,
  file.path(analysis_root, paste0("luc_emissions_annual_balance_2020_", target_year, ".csv"))
)

luc_balance_long <- luc_balance %>%
  select(state, scenario, region, region_label, year, Positive, Negative) %>%
  pivot_longer(
    c(Positive, Negative), names_to = "flow", values_to = "value_mtco2_yr"
  ) %>%
  mutate(
    flow = recode(
      flow,
      Positive = "Gross positive emissions",
      Negative = "Gross removals"
    ),
    flow = factor(flow, levels = names(balance_colours))
  )

luc_cumulative <- luc_balance_long %>%
  group_by(state, scenario, region, region_label, flow) %>%
  summarise(
    cumulative_gtco2 = integrate_trapezoid(year, value_mtco2_yr) / 1000,
    .groups = "drop"
  )
luc_cumulative_net <- luc_cumulative %>%
  group_by(state, scenario, region, region_label) %>%
  summarise(net_cumulative_gtco2 = sum(cumulative_gtco2), .groups = "drop")
write_csv(
  luc_cumulative %>%
    left_join(
      luc_cumulative_net,
      by = c("state", "scenario", "region", "region_label")
    ),
  file.path(analysis_root, paste0("luc_emissions_cumulative_2020_", target_year, ".csv"))
)

# Detailed sources used in the main diverging stacks. Managed forest is not
# included. The positive and negative source sums therefore reproduce the
# adjusted balance used for the net marker.
source_definition <- tribble(
  ~emission_category, ~land_category, ~source,
  "Positive", "DEF", "Deforestation",
  "Positive", "DEG", "Grassland degradation",
  "Positive", "CL", "Cropland-related emissions",
  "Positive", "CROP_FLW", "Cropland-related emissions",
  "Positive", "PAS", "Pasture and other emissions",
  "Positive", "SL", "Pasture and other emissions",
  "Positive", "OL", "Pasture and other emissions",
  "Negative", "AFR", "Afforestation",
  "Negative", "NRFABDCUM", "Natural forest regrowth",
  "Negative", "NRGABDCUM", "Natural grassland recovery",
  "Negative", "CL", "Cropland stock gain",
  "Negative", "CROP_FLW", "Cropland soil management",
  "Negative", "PAS", "Pasture and other removals",
  "Negative", "SL", "Pasture and other removals",
  "Negative", "OL", "Pasture and other removals"
)
source_order <- c(
  "Deforestation", "Grassland degradation", "Cropland-related emissions",
  "Pasture and other emissions", "Afforestation", "Natural forest regrowth",
  "Natural grassland recovery", "Cropland stock gain",
  "Cropland soil management", "Pasture and other removals"
)
source_colours <- c(
  "Deforestation" = "#B2182B",
  "Grassland degradation" = "#EF8A62",
  "Cropland-related emissions" = "#FDAE61",
  "Pasture and other emissions" = "#FEE090",
  "Afforestation" = "#1B9E77",
  "Natural forest regrowth" = "#66C2A5",
  "Natural grassland recovery" = "#A6D854",
  "Cropland stock gain" = "#E6AB02",
  "Cropland soil management" = "#5E3C99",
  "Pasture and other removals" = "#3288BD"
)

luc_source_flows <- ghgl_all %>%
  inner_join(source_definition, by = c("emission_category", "land_category")) %>%
  group_by(state, region, year, source) %>%
  summarise(value_mtco2_yr = sum(value_mtco2_yr), .groups = "drop") %>%
  complete(
    state = names(states), region = geographies, year = analysis_years,
    source = source_order, fill = list(value_mtco2_yr = 0)
  ) %>%
  mutate(
    scenario = factor(unname(scenario_labels[state]), levels = unname(scenario_labels)),
    region_label = if_else(region == "World", "World", unname(r10_labels[region])),
    source = factor(source, levels = source_order)
  )
luc_source_cumulative <- luc_source_flows %>%
  group_by(state, scenario, region, region_label, source) %>%
  summarise(
    cumulative_gtco2 = integrate_trapezoid(year, value_mtco2_yr) / 1000,
    .groups = "drop"
  )
source_balance_closure <- luc_source_flows %>%
  group_by(state, region, year) %>%
  summarise(source_net_mtco2_yr = sum(value_mtco2_yr), .groups = "drop") %>%
  left_join(
    luc_balance %>% select(state, region, year, Net),
    by = c("state", "region", "year")
  ) %>%
  mutate(source_balance_residual_mtco2_yr = Net - source_net_mtco2_yr)
write_csv(
  luc_source_flows,
  file.path(analysis_root, paste0("luc_emissions_source_balance_annual_2020_", target_year, ".csv"))
)
write_csv(
  luc_source_cumulative,
  file.path(analysis_root, paste0("luc_emissions_source_balance_cumulative_2020_", target_year, ".csv"))
)

# 2. World cumulative and decadal figures ----------------------------------

world_cumulative <- luc_source_cumulative %>% filter(region == "World")
world_cumulative_net <- luc_cumulative_net %>% filter(region == "World")
p <- ggplot(world_cumulative, aes(scenario, cumulative_gtco2, fill = source)) +
  geom_hline(yintercept = 0, colour = "grey35", linewidth = .4) +
  geom_col(width = .7) +
  geom_point(
    data = world_cumulative_net,
    aes(scenario, net_cumulative_gtco2, shape = "Net"),
    inherit.aes = FALSE, colour = "black", size = 3.1
  ) +
  scale_fill_manual(values = source_colours, name = NULL, drop = TRUE) +
  scale_shape_manual(values = c(Net = 18), name = NULL) +
  scale_y_continuous(labels = label_number(big.mark = ",", accuracy = 1)) +
  labs(
    title = paste0("Cumulative global land-use emissions and removals, 2020–", target_year),
    subtitle = "Sources are stacked above or below zero; managed forest is excluded; diamonds show the recalculated net",
    x = NULL, y = expression("Cumulative LUC CO"[2] * " (Gt CO"[2] * ")"),
    caption = "MergedComparison GHGL, land category LUC, excluding MNGFRS. Decadal MtCO2/yr rates are integrated with the trapezoidal rule. Net is recalculated."
  ) + emission_theme +
  theme(axis.text.x = element_text(angle = 20, hjust = 1), panel.grid.major.x = element_blank())
ggsave(
  file.path(world_root, paste0("world_cumulative_luc_balance_by_source_2020_", target_year, ".png")),
  p, width = 12, height = 7.2, dpi = 300, bg = "white"
)

p <- ggplot(
  luc_cumulative %>% filter(region == "World"),
  aes(scenario, cumulative_gtco2, fill = flow)
) +
  geom_hline(yintercept = 0, colour = "grey35", linewidth = .4) +
  geom_col(width = .7) +
  geom_point(
    data = world_cumulative_net,
    aes(scenario, net_cumulative_gtco2, shape = "Net"),
    inherit.aes = FALSE, colour = "black", size = 3.1
  ) +
  scale_fill_manual(values = balance_colours, name = NULL) +
  scale_shape_manual(values = c(Net = 18), name = NULL) +
  scale_y_continuous(labels = label_number(big.mark = ",", accuracy = 1)) +
  labs(
    title = paste0("Cumulative global land-use emissions and removals, 2020–", target_year),
    subtitle = "Two-colour overview; managed forest is excluded; diamonds show the recalculated net",
    x = NULL, y = expression("Cumulative LUC CO"[2] * " (Gt CO"[2] * ")"),
    caption = "MergedComparison GHGL, land category LUC, excluding MNGFRS. Decadal rates are integrated with the trapezoidal rule."
  ) + emission_theme +
  theme(axis.text.x = element_text(angle = 20, hjust = 1), panel.grid.major.x = element_blank())
ggsave(
  file.path(world_root, paste0("world_cumulative_luc_balance_2020_", target_year, ".png")),
  p, width = 12, height = 7.2, dpi = 300, bg = "white"
)

world_decadal <- luc_source_flows %>% filter(region == "World")
world_decadal_net <- luc_balance %>% filter(region == "World")
p <- ggplot(world_decadal, aes(year, value_mtco2_yr, fill = source)) +
  geom_hline(yintercept = 0, colour = "grey35", linewidth = .35) +
  geom_col(width = 6.7) +
  geom_point(
    data = world_decadal_net,
    aes(year, Net, shape = "Net"), inherit.aes = FALSE,
    colour = "black", size = 2.4
  ) +
  facet_wrap(vars(scenario), ncol = 2) +
  scale_fill_manual(values = source_colours, name = NULL, drop = TRUE) +
  scale_shape_manual(values = c(Net = 18), name = NULL) +
  scale_x_continuous(breaks = analysis_years) +
  scale_y_continuous(labels = label_number(big.mark = ",", accuracy = 1)) +
  labs(
    title = "Global gross LUC emissions, removals, and net flow at each model year",
    subtitle = "All eight policy scenarios; annual rates at 10-year intervals; managed forest is excluded",
    x = NULL, y = expression("LUC CO"[2] * " flow (Mt CO"[2] * "/yr)"),
    caption = "MergedComparison GHGL, land category LUC, excluding MNGFRS. Negative values are removals; the black diamond is the recalculated Net flow."
  ) + emission_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.grid.major.x = element_blank())
ggsave(
  file.path(world_root, paste0("world_decadal_luc_balance_by_source_to_", target_year, ".png")),
  p, width = 13, height = 14, dpi = 300, bg = "white"
)

p <- ggplot(
  luc_balance_long %>% filter(region == "World"),
  aes(year, value_mtco2_yr, fill = flow)
) +
  geom_hline(yintercept = 0, colour = "grey35", linewidth = .35) +
  geom_col(width = 6.7) +
  geom_point(
    data = world_decadal_net,
    aes(year, Net, shape = "Net"), inherit.aes = FALSE,
    colour = "black", size = 2.4
  ) +
  facet_wrap(vars(scenario), ncol = 2) +
  scale_fill_manual(values = balance_colours, name = NULL) +
  scale_shape_manual(values = c(Net = 18), name = NULL) +
  scale_x_continuous(breaks = analysis_years) +
  scale_y_continuous(labels = label_number(big.mark = ",", accuracy = 1)) +
  labs(
    title = "Global gross LUC emissions, removals, and net flow at each model year",
    subtitle = "Two-colour overview; managed forest is excluded",
    x = NULL, y = expression("LUC CO"[2] * " flow (Mt CO"[2] * "/yr)"),
    caption = "MergedComparison GHGL, land category LUC, excluding MNGFRS. The diamond is the recalculated Net."
  ) + emission_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.grid.major.x = element_blank())
ggsave(
  file.path(world_root, paste0("world_decadal_luc_balance_to_", target_year, ".png")),
  p, width = 13, height = 14, dpi = 300, bg = "white"
)

# 3. R10 cumulative and decadal figures ------------------------------------

r10_cumulative <- luc_source_cumulative %>%
  filter(region %in% names(r10_labels)) %>%
  mutate(region_label = factor(region_label, levels = unname(r10_labels)))
r10_cumulative_net <- luc_cumulative_net %>%
  filter(region %in% names(r10_labels)) %>%
  mutate(region_label = factor(region_label, levels = unname(r10_labels)))
p <- ggplot(r10_cumulative, aes(scenario, cumulative_gtco2, fill = source)) +
  geom_hline(yintercept = 0, colour = "grey35", linewidth = .35) +
  geom_col(width = .7) +
  geom_point(
    data = r10_cumulative_net,
    aes(scenario, net_cumulative_gtco2, shape = "Net"),
    inherit.aes = FALSE, colour = "black", size = 2.1
  ) +
  facet_wrap(vars(region_label), scales = "free_y", ncol = 2) +
  scale_fill_manual(values = source_colours, name = NULL, drop = TRUE) +
  scale_shape_manual(values = c(Net = 18), name = NULL) +
  scale_y_continuous(labels = label_number(big.mark = ",", accuracy = 1)) +
  labs(
    title = paste0("Cumulative LUC emissions and removals by R10 region, 2020–", target_year),
    subtitle = "All eight scenarios; managed forest is excluded; vertical scales differ across regions",
    x = NULL, y = expression("Cumulative LUC CO"[2] * " (Gt CO"[2] * ")"),
    caption = "MergedComparison GHGL excluding MNGFRS. Positive and adjusted negative flows are integrated separately; diamonds are their net sum."
  ) + emission_theme +
  theme(
    axis.text.x = element_text(angle = 48, hjust = 1, size = 7.2),
    panel.grid.major.x = element_blank()
  )
ggsave(
  file.path(r10_root, paste0("r10_cumulative_luc_balance_by_source_2020_", target_year, ".png")),
  p, width = 14, height = 18, dpi = 300, bg = "white"
)

p <- ggplot(
  luc_cumulative %>%
    filter(region %in% names(r10_labels)) %>%
    mutate(region_label = factor(region_label, levels = unname(r10_labels))),
  aes(scenario, cumulative_gtco2, fill = flow)
) +
  geom_hline(yintercept = 0, colour = "grey35", linewidth = .35) +
  geom_col(width = .7) +
  geom_point(
    data = r10_cumulative_net,
    aes(scenario, net_cumulative_gtco2, shape = "Net"),
    inherit.aes = FALSE, colour = "black", size = 2.1
  ) +
  facet_wrap(vars(region_label), scales = "free_y", ncol = 2) +
  scale_fill_manual(values = balance_colours, name = NULL) +
  scale_shape_manual(values = c(Net = 18), name = NULL) +
  scale_y_continuous(labels = label_number(big.mark = ",", accuracy = 1)) +
  labs(
    title = paste0("Cumulative LUC emissions and removals by R10 region, 2020–", target_year),
    subtitle = "Two-colour overview; managed forest is excluded; vertical scales differ across regions",
    x = NULL, y = expression("Cumulative LUC CO"[2] * " (Gt CO"[2] * ")"),
    caption = "MergedComparison GHGL excluding MNGFRS. Diamonds are the recalculated net sum."
  ) + emission_theme +
  theme(
    axis.text.x = element_text(angle = 48, hjust = 1, size = 7.2),
    panel.grid.major.x = element_blank()
  )
ggsave(
  file.path(r10_root, paste0("r10_cumulative_luc_balance_2020_", target_year, ".png")),
  p, width = 14, height = 18, dpi = 300, bg = "white"
)

for (scenario_state in names(states)) {
  r10_decadal <- luc_source_flows %>%
    filter(region %in% names(r10_labels), state == scenario_state) %>%
    mutate(region_label = factor(region_label, levels = unname(r10_labels)))
  r10_decadal_net <- luc_balance %>%
    filter(region %in% names(r10_labels), state == scenario_state) %>%
    mutate(region_label = factor(region_label, levels = unname(r10_labels)))
  p <- ggplot(r10_decadal, aes(year, value_mtco2_yr, fill = source)) +
    geom_hline(yintercept = 0, colour = "grey35", linewidth = .35) +
    geom_col(width = 6.7) +
    geom_point(
      data = r10_decadal_net,
      aes(year, Net, shape = "Net"), inherit.aes = FALSE,
      colour = "black", size = 2.1
    ) +
    facet_wrap(vars(region_label), scales = "free_y", ncol = 2) +
    scale_fill_manual(values = source_colours, name = NULL, drop = TRUE) +
    scale_shape_manual(values = c(Net = 18), name = NULL) +
    scale_x_continuous(breaks = analysis_years) +
    scale_y_continuous(labels = label_number(big.mark = ",", accuracy = 1)) +
    labs(
      title = paste0("R10 LUC emissions and removals: ", gsub("\n", " ", scenario_labels[[scenario_state]])),
      subtitle = "Annual rates at each 10-year model point; managed forest is excluded",
      x = NULL, y = expression("LUC CO"[2] * " flow (Mt CO"[2] * "/yr)"),
      caption = "MergedComparison GHGL, land category LUC, excluding MNGFRS. The diamond is the recalculated Net."
    ) + emission_theme +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.grid.major.x = element_blank())
  ggsave(
    file.path(
      r10_root,
      paste0("r10_decadal_luc_balance_by_source_", scenario_state, "_to_", target_year, ".png")
    ),
    p, width = 13, height = 16, dpi = 300, bg = "white"
  )

  r10_decadal_two <- luc_balance_long %>%
    filter(region %in% names(r10_labels), state == scenario_state) %>%
    mutate(region_label = factor(region_label, levels = unname(r10_labels)))
  p <- ggplot(r10_decadal_two, aes(year, value_mtco2_yr, fill = flow)) +
    geom_hline(yintercept = 0, colour = "grey35", linewidth = .35) +
    geom_col(width = 6.7) +
    geom_point(
      data = r10_decadal_net,
      aes(year, Net, shape = "Net"), inherit.aes = FALSE,
      colour = "black", size = 2.1
    ) +
    facet_wrap(vars(region_label), scales = "free_y", ncol = 2) +
    scale_fill_manual(values = balance_colours, name = NULL) +
    scale_shape_manual(values = c(Net = 18), name = NULL) +
    scale_x_continuous(breaks = analysis_years) +
    scale_y_continuous(labels = label_number(big.mark = ",", accuracy = 1)) +
    labs(
      title = paste0("R10 LUC emissions and removals: ", gsub("\n", " ", scenario_labels[[scenario_state]])),
      subtitle = "Two-colour overview; managed forest is excluded",
      x = NULL, y = expression("LUC CO"[2] * " flow (Mt CO"[2] * "/yr)"),
      caption = "MergedComparison GHGL, land category LUC, excluding MNGFRS. The diamond is the recalculated Net."
    ) + emission_theme +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.grid.major.x = element_blank())
  ggsave(
    file.path(
      r10_root,
      paste0("r10_decadal_luc_balance_", scenario_state, "_to_", target_year, ".png")
    ),
    p, width = 13, height = 16, dpi = 300, bg = "white"
  )
}

# 4. What is included in gross removals? -----------------------------------

# These detailed classes reproduce adjusted Negative/LUC without double counting.
# MNGFRS is intentionally excluded because including managed-forest removals
# in the accounting boundary is contestable. Its original values remain in
# luc_emissions_annual_balance for auditing.
removal_source_labels <- c(
  AFR = "Afforestation",
  NRFABDCUM = "Natural forest regrowth",
  NRGABDCUM = "Natural grassland recovery",
  CL = "Cropland stock gain",
  CROP_FLW = "Cropland soil management",
  PAS = "Pasture",
  SL = "Settlement land",
  OL = "Other land"
)
removal_source_colours <- c(
  "Afforestation" = "#1B9E77",
  "Natural forest regrowth" = "#66C2A4",
  "Natural grassland recovery" = "#A6D854",
  "Cropland stock gain" = "#E6AB02",
  "Cropland soil management" = "#D95F02",
  "Pasture" = "#7570B3",
  "Settlement land" = "#BDBDBD",
  "Other land" = "#636363"
)
removal_sources <- ghgl_all %>%
  filter(
    emission_category == "Negative",
    land_category %in% names(removal_source_labels)
  ) %>%
  select(state, region, year, land_category, value_mtco2_yr) %>%
  complete(
    state = names(states), region = geographies, year = analysis_years,
    land_category = names(removal_source_labels),
    fill = list(value_mtco2_yr = 0)
  ) %>%
  mutate(
    scenario = factor(unname(scenario_labels[state]), levels = unname(scenario_labels)),
    region_label = if_else(region == "World", "World", unname(r10_labels[region])),
    removal_source = factor(
      unname(removal_source_labels[land_category]),
      levels = unname(removal_source_labels)
    )
  )

source_closure <- removal_sources %>%
  group_by(state, region, year) %>%
  summarise(source_sum_mtco2_yr = sum(value_mtco2_yr), .groups = "drop") %>%
  left_join(
    luc_balance %>% select(state, region, year, Negative),
    by = c("state", "region", "year")
  ) %>%
  mutate(removal_source_residual_mtco2_yr = Negative - source_sum_mtco2_yr)
write_csv(
  removal_sources,
  file.path(source_root, paste0("luc_removal_sources_annual_2020_", target_year, ".csv"))
)

removal_sources_cumulative <- removal_sources %>%
  group_by(state, scenario, region, region_label, removal_source) %>%
  summarise(
    cumulative_gtco2 = integrate_trapezoid(year, value_mtco2_yr) / 1000,
    .groups = "drop"
  )
write_csv(
  removal_sources_cumulative,
  file.path(source_root, paste0("luc_removal_sources_cumulative_2020_", target_year, ".csv"))
)

world_source_cumulative <- removal_sources_cumulative %>% filter(region == "World")
p <- ggplot(world_source_cumulative, aes(scenario, cumulative_gtco2, fill = removal_source)) +
  geom_hline(yintercept = 0, colour = "grey35", linewidth = .35) +
  geom_col(width = .7) +
  scale_fill_manual(values = removal_source_colours, name = NULL, drop = TRUE) +
  scale_y_continuous(labels = label_number(big.mark = ",", accuracy = 1)) +
  labs(
    title = paste0("Sources of cumulative global LUC removals, 2020–", target_year),
    subtitle = "Managed forest is excluded; the remaining components sum to the adjusted negative flow",
    x = NULL, y = expression("Cumulative removal (Gt CO"[2] * ")"),
    caption = "Included forest sinks are afforestation and natural regrowth. CROP_FLW is the explicit cropland soil-carbon-management sink."
  ) + emission_theme +
  theme(axis.text.x = element_text(angle = 20, hjust = 1), panel.grid.major.x = element_blank())
ggsave(
  file.path(source_root, paste0("world_cumulative_luc_removal_sources_2020_", target_year, ".png")),
  p, width = 13, height = 7.5, dpi = 300, bg = "white"
)

world_source_decadal <- removal_sources %>% filter(region == "World")
p <- ggplot(world_source_decadal, aes(year, value_mtco2_yr, fill = removal_source)) +
  geom_hline(yintercept = 0, colour = "grey35", linewidth = .35) +
  geom_col(width = 6.7) +
  facet_wrap(vars(scenario), ncol = 2) +
  scale_fill_manual(values = removal_source_colours, name = NULL, drop = TRUE) +
  scale_x_continuous(breaks = analysis_years) +
  scale_y_continuous(labels = label_number(big.mark = ",", accuracy = 1)) +
  labs(
    title = "Global LUC removal sources at each model year",
    subtitle = "All eight scenarios; managed forest is excluded from the stacked negative bars",
    x = NULL, y = expression("Removal flow (Mt CO"[2] * "/yr)"),
    caption = "MergedComparison GHGL. These are LUC removals and do not include BECCS capture outside the PLUM LUC account."
  ) + emission_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), panel.grid.major.x = element_blank())
ggsave(
  file.path(source_root, paste0("world_decadal_luc_removal_sources_to_", target_year, ".png")),
  p, width = 14, height = 14.5, dpi = 300, bg = "white"
)

r10_source_cumulative <- removal_sources_cumulative %>%
  filter(region %in% names(r10_labels)) %>%
  mutate(region_label = factor(region_label, levels = unname(r10_labels)))
p <- ggplot(r10_source_cumulative, aes(scenario, cumulative_gtco2, fill = removal_source)) +
  geom_hline(yintercept = 0, colour = "grey35", linewidth = .35) +
  geom_col(width = .7) +
  facet_wrap(vars(region_label), scales = "free_y", ncol = 2) +
  scale_fill_manual(values = removal_source_colours, name = NULL, drop = TRUE) +
  scale_y_continuous(labels = label_number(big.mark = ",", accuracy = 1)) +
  labs(
    title = paste0("Sources of cumulative LUC removals by R10 region, 2020–", target_year),
    subtitle = "Managed forest is excluded; vertical scales differ across regions",
    x = NULL, y = expression("Cumulative removal (Gt CO"[2] * ")"),
    caption = "Detailed negative-flow classes close to adjusted Negative/LUC after removing MNGFRS; aggregate FRS and GL are not added."
  ) + emission_theme +
  theme(
    axis.text.x = element_text(angle = 48, hjust = 1, size = 7.2),
    panel.grid.major.x = element_blank()
  )
ggsave(
  file.path(source_root, paste0("r10_cumulative_luc_removal_sources_2020_", target_year, ".png")),
  p, width = 15, height = 19, dpi = 300, bg = "white"
)

# 5. Why is the protection effect small or counterintuitive? ---------------

protection_comparisons <- tribble(
  ~comparison, ~from, ~to, ~comparison_label,
  "no_cdr", "000", "100", "Protection\nwithout CDR",
  "with_afforestation", "010", "110", "Protection\nwith afforestation",
  "with_beccs", "001", "101", "Protection\nwith BECCS",
  "with_both_cdr", "011", "111", "Protection\nwith both CDR"
)
world_source_wide <- luc_source_cumulative %>%
  filter(region == "World") %>%
  select(state, source, cumulative_gtco2) %>%
  pivot_wider(names_from = state, values_from = cumulative_gtco2)
protection_source_changes <- lapply(seq_len(nrow(protection_comparisons)), function(i) {
  from <- protection_comparisons$from[[i]]
  to <- protection_comparisons$to[[i]]
  world_source_wide %>%
    transmute(
      source,
      comparison = protection_comparisons$comparison[[i]],
      comparison_label = protection_comparisons$comparison_label[[i]],
      protection_effect_gtco2 = .data[[to]] - .data[[from]]
    )
}) %>%
  bind_rows() %>%
  mutate(
    source = factor(source, levels = source_order),
    comparison_label = factor(
      comparison_label, levels = protection_comparisons$comparison_label
    )
  )
protection_net_changes <- protection_source_changes %>%
  group_by(comparison, comparison_label) %>%
  summarise(net_effect_gtco2 = sum(protection_effect_gtco2), .groups = "drop")
write_csv(
  protection_source_changes %>%
    left_join(protection_net_changes, by = c("comparison", "comparison_label")),
  file.path(diagnostic_root, paste0("world_protection_luc_source_effects_2020_", target_year, ".csv"))
)

p <- ggplot(
  protection_source_changes,
  aes(comparison_label, protection_effect_gtco2, fill = source)
) +
  geom_hline(yintercept = 0, colour = "grey35", linewidth = .4) +
  geom_col(width = .68) +
  geom_point(
    data = protection_net_changes,
    aes(comparison_label, net_effect_gtco2, shape = "Net protection effect"),
    inherit.aes = FALSE, colour = "black", size = 3.1
  ) +
  scale_fill_manual(values = source_colours, name = NULL, drop = TRUE) +
  scale_shape_manual(values = c("Net protection effect" = 18), name = NULL) +
  scale_y_continuous(labels = label_number(accuracy = .1)) +
  labs(
    title = "Protection mainly redistributes LUC flows rather than eliminating them",
    subtitle = "Change caused by adding protection in four policy contexts; managed forest is excluded",
    x = NULL, y = expression("Protection effect on cumulative LUC (Gt CO"[2] * ")"),
    caption = paste0(
      "Negative contributions reduce net emissions or increase removals; positive contributions do the reverse. ",
      "Each comparison covers 2020–", target_year, "."
    )
  ) + emission_theme +
  theme(axis.text.x = element_text(angle = 18, hjust = 1), panel.grid.major.x = element_blank())
ggsave(
  file.path(diagnostic_root, paste0("world_protection_luc_source_effects_2020_", target_year, ".png")),
  p, width = 11.5, height = 7.3, dpi = 300, bg = "white"
)

afforestation_area <- rgdx.param(merged_file, "IAMCTempwoU") %>%
  transmute(
    scenario_name = as.character(i1), geography = as.character(i2),
    variable = as.character(i3), year = as.integer(as.character(i4)),
    value = as.numeric(value)
  ) %>%
  filter(
    scenario_name %in% states[c("010", "110")], geography == "WLD",
    variable == "Lan_Cov_Frs_Aff", year %in% analysis_years
  ) %>%
  transmute(
    state = state_from_scenario(scenario_name), year,
    metric = "Afforestation area", value,
    unit = "million ha"
  )
afforestation_sink <- luc_source_flows %>%
  filter(region == "World", state %in% c("010", "110"), source == "Afforestation") %>%
  transmute(
    state, year, metric = "Afforestation removal flow",
    value = value_mtco2_yr, unit = "Mt CO2/yr"
  )
afforestation_area_sink <- bind_rows(afforestation_area, afforestation_sink) %>%
  mutate(
    scenario = factor(
      recode(state, `010` = "Afforestation", `110` = "Protection + afforestation"),
      levels = c("Afforestation", "Protection + afforestation")
    ),
    metric = factor(
      metric, levels = c("Afforestation area", "Afforestation removal flow")
    )
  )
write_csv(
  afforestation_area_sink,
  file.path(diagnostic_root, paste0("world_afforestation_area_and_sink_010_110_to_", target_year, ".csv"))
)

p <- ggplot(
  afforestation_area_sink,
  aes(year, value, colour = scenario, linetype = scenario)
) +
  geom_hline(yintercept = 0, colour = "grey55", linewidth = .3) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.1) +
  facet_wrap(vars(metric, unit), scales = "free_y", ncol = 1,
             labeller = label_both) +
  scale_colour_manual(
    values = c("Afforestation" = "#009E73", "Protection + afforestation" = "#7F3C8D"),
    name = NULL
  ) +
  scale_linetype_manual(
    values = c("Afforestation" = "solid", "Protection + afforestation" = "longdash"),
    name = NULL
  ) +
  scale_x_continuous(breaks = analysis_years) +
  scale_y_continuous(labels = label_number(big.mark = ",", accuracy = 1)) +
  labs(
    title = "Protection shifts afforestation earlier even though its 2100 area is smaller",
    subtitle = "The earlier planted cohorts continue removing CO2, increasing cumulative removal in 110 relative to 010",
    x = NULL, y = NULL,
    caption = "MergedComparison IAMCTempwoU area and GHGL AFR removal. Removal remains negative; managed forest is not included."
  ) + emission_theme +
  theme(panel.grid.minor = element_blank())
ggsave(
  file.path(diagnostic_root, paste0("world_afforestation_area_and_sink_010_110_to_", target_year, ".png")),
  p, width = 11.3, height = 9, dpi = 300, bg = "white"
)

# 6. Why do positive land-conversion emissions peak around 2070? -----------

# GHGL at year t annualises the gross stock/area loss during the preceding
# model interval (normally t-10 to t). The block therefore compares the 2070
# flow with the land reallocation that occurs between 2060 and 2070.
if (target_year >= 2080L) {
  mitigation_states <- c("010", "110", "001", "101", "011", "111")
  mitigation_labels <- unname(scenario_labels[mitigation_states])
  scenario_colours <- c(
    `010` = "#009E73", `110` = "#56B4E9", `001` = "#E69F00",
    `101` = "#D55E00", `011` = "#0072B2", `111` = "#7F3C8D"
  )
  positive_sources <- source_order[1:4]

  world_spike_sources <- luc_source_flows %>%
    filter(
      region == "World", state %in% mitigation_states,
      year %in% c(2060L, 2070L, 2080L),
      as.character(source) %in% positive_sources
    ) %>%
    mutate(
      scenario = factor(unname(scenario_labels[state]), levels = mitigation_labels),
      year = factor(year, levels = c(2060L, 2070L, 2080L))
    )
  write_csv(
    world_spike_sources,
    file.path(spike_root, "world_positive_luc_sources_2060_2070_2080.csv")
  )

  p <- ggplot(world_spike_sources, aes(year, value_mtco2_yr, fill = source)) +
    geom_col(width = .68) +
    facet_wrap(vars(scenario), ncol = 3) +
    scale_fill_manual(values = source_colours, name = NULL, drop = TRUE) +
    scale_y_continuous(labels = label_number(big.mark = ",", accuracy = 1)) +
    labs(
      title = "The 2070 conversion-emission pulse is dominated by grassland loss",
      subtitle = "The 2070 rate represents gross land changes during 2060–2070; mitigation scenarios only",
      x = "Reported model year",
      y = expression("Gross positive LUC emissions (Mt CO"[2] * "/yr)"),
      caption = "MergedComparison GHGL Positive/LUC. Bars show emissions before subtracting afforestation, regrowth and soil removals."
    ) + emission_theme +
    theme(panel.grid.major.x = element_blank())
  ggsave(
    file.path(spike_root, "world_positive_luc_sources_2060_2070_2080.png"),
    p, width = 13.5, height = 8.2, dpi = 300, bg = "white"
  )

  land_variables_spike <- c(
    Lan_Cov_Cro_Non_Ene_Cro = "Food cropland",
    Lan_Cov_Cro_Ene_Cro_2nd_gen = "2G bioenergy cropland",
    Lan_Cov_Frs = "Forest",
    Lan_Cov_Pst = "Pasture",
    Lan_Cov_Oth_Nat_Lan = "Other natural land"
  )
  land_spike_levels <- unname(land_variables_spike)
  world_land_around_2070 <- rgdx.param(merged_file, "IAMCTempwoU") %>%
    filter(
      i1 %in% states[mitigation_states], i2 == "WLD",
      i3 %in% names(land_variables_spike),
      as.integer(as.character(i4)) %in% c(2060L, 2070L, 2080L)
    ) %>%
    transmute(
      state = state_from_scenario(as.character(i1)),
      year = as.integer(as.character(i4)),
      land_class = unname(land_variables_spike[as.character(i3)]),
      area_million_ha = as.numeric(value)
    ) %>%
    group_by(state, year, land_class) %>%
    summarise(area_million_ha = sum(area_million_ha), .groups = "drop") %>%
    pivot_wider(names_from = year, values_from = area_million_ha) %>%
    transmute(
      state, land_class,
      `2060–2070` = `2070` - `2060`,
      `2070–2080` = `2080` - `2070`
    ) %>%
    pivot_longer(
      c(`2060–2070`, `2070–2080`),
      names_to = "interval", values_to = "area_change_million_ha"
    ) %>%
    mutate(
      scenario = factor(unname(scenario_labels[state]), levels = mitigation_labels),
      interval = factor(interval, levels = c("2060–2070", "2070–2080")),
      land_class = factor(land_class, levels = land_spike_levels)
    )
  write_csv(
    world_land_around_2070,
    file.path(spike_root, "world_land_change_around_2070.csv")
  )

  land_change_colours <- c(
    "Food cropland" = "#E69F00", "2G bioenergy cropland" = "#CC79A7",
    "Forest" = "#0072B2", "Pasture" = "#8C6D31",
    "Other natural land" = "#009E73"
  )
  p <- ggplot(
    world_land_around_2070,
    aes(interval, area_change_million_ha, fill = land_class)
  ) +
    geom_hline(yintercept = 0, colour = "grey35", linewidth = .4) +
    geom_col(width = .67) +
    facet_wrap(vars(scenario), ncol = 3) +
    scale_fill_manual(values = land_change_colours, name = NULL) +
    scale_y_continuous(labels = label_number(big.mark = ",", accuracy = 1)) +
    labs(
      title = "CDR land expands much faster during 2060–2070",
      subtitle = "Positive and negative land changes are stacked separately; afforestation is contained within total Forest",
      x = "Land-change interval", y = "Global area change (million ha)",
      caption = "MergedComparison IAMCTempwoU. The five displayed cover classes are not claimed to exhaust Total land."
    ) + emission_theme +
    theme(panel.grid.major.x = element_blank())
  ggsave(
    file.path(spike_root, "world_land_change_around_2070.png"),
    p, width = 13.5, height = 8.4, dpi = 300, bg = "white"
  )

  # QENE_hub has no documented physical unit in the supplied GDX. Index each
  # scenario to 2060=1 and compare its timing with 2G bioenergy cropland.
  hub_bioenergy_trajectory <- rgdx.param(
    file.path(data_dir, "ALPHA", "hub_data.gdx"), "QENE_hub"
  ) %>%
    filter(
      scenario_hub %in% states[c("001", "101", "011", "111")],
      as.integer(as.character(yr)) >= 2030L,
      as.integer(as.character(yr)) <= target_year
    ) %>%
    transmute(
      state = state_from_scenario(as.character(scenario_hub)),
      year = as.integer(as.character(yr)), value = as.numeric(QENE_hub)
    ) %>%
    group_by(state, year) %>%
    summarise(value = sum(value), .groups = "drop") %>%
    group_by(state) %>%
    mutate(value = value / value[year == 2060L]) %>%
    ungroup() %>%
    transmute(state, year, metric = "Hub bioenergy demand (2060 = 1)", value)

  cdr_area_trajectory <- rgdx.param(merged_file, "IAMCTempwoU") %>%
    filter(
      i1 %in% states[mitigation_states], i2 == "WLD",
      i3 %in% c("Lan_Cov_Cro_Ene_Cro_2nd_gen", "Lan_Cov_Frs_Aff"),
      as.integer(as.character(i4)) >= 2030L,
      as.integer(as.character(i4)) <= target_year
    ) %>%
    transmute(
      state = state_from_scenario(as.character(i1)),
      year = as.integer(as.character(i4)),
      metric = recode(
        as.character(i3),
        Lan_Cov_Cro_Ene_Cro_2nd_gen = "2G bioenergy cropland (million ha)",
        Lan_Cov_Frs_Aff = "Afforestation area (million ha)"
      ),
      value = as.numeric(value)
    ) %>%
    group_by(state, year, metric) %>%
    summarise(value = sum(value), .groups = "drop") %>%
    filter(value > 1e-3)

  positive_luc_trajectory <- luc_balance %>%
    filter(region == "World", state %in% mitigation_states, year >= 2030L) %>%
    transmute(
      state, year,
      metric = "Gross positive LUC emissions (Mt CO2/yr)", value = Positive
    )
  cdr_scaleup <- bind_rows(
    hub_bioenergy_trajectory, cdr_area_trajectory, positive_luc_trajectory
  ) %>%
    mutate(
      scenario = factor(unname(scenario_labels[state]), levels = mitigation_labels),
      metric = factor(metric, levels = c(
        "Hub bioenergy demand (2060 = 1)",
        "2G bioenergy cropland (million ha)",
        "Afforestation area (million ha)",
        "Gross positive LUC emissions (Mt CO2/yr)"
      ))
    )
  write_csv(
    cdr_scaleup,
    file.path(spike_root, paste0("world_cdr_scaleup_and_positive_luc_to_", target_year, ".csv"))
  )

  p <- ggplot(cdr_scaleup, aes(year, value, colour = state, linetype = state)) +
    geom_vline(xintercept = 2070, colour = "grey60", linewidth = .35) +
    geom_line(linewidth = .9) +
    geom_point(size = 1.7) +
    facet_wrap(vars(metric), scales = "free_y", ncol = 2) +
    scale_colour_manual(
      values = scenario_colours, breaks = mitigation_states,
      labels = scenario_labels[mitigation_states], name = NULL
    ) +
    scale_linetype_manual(
      values = c(
        `010` = "solid", `110` = "longdash", `001` = "dotted",
        `101` = "dotdash", `011` = "solid", `111` = "longdash"
      ),
      breaks = mitigation_states, labels = scenario_labels[mitigation_states], name = NULL
    ) +
    scale_x_continuous(breaks = analysis_years) +
    scale_y_continuous(labels = label_number(big.mark = ",", accuracy = .1)) +
    labs(
      title = "The 2070 emission pulse coincides with rapid CDR land scale-up",
      subtitle = "Bioenergy demand jumps first; land allocation responds through bioenergy cropland, afforestation and displacement",
      x = NULL, y = NULL,
      caption = "QENE_hub is indexed because its physical unit is undocumented. Land area and LUC emissions use MergedComparison."
    ) + emission_theme
  ggsave(
    file.path(spike_root, paste0("world_cdr_scaleup_and_positive_luc_to_", target_year, ".png")),
    p, width = 13.5, height = 9.2, dpi = 300, bg = "white"
  )

  # The excess is a descriptive contrast, not a causal attribution:
  # 2070 minus the mean of the adjacent 2060 and 2080 rates.
  r10_spike_excess <- luc_source_flows %>%
    filter(
      state == "011", region %in% names(r10_labels),
      year %in% c(2060L, 2070L, 2080L),
      as.character(source) %in% positive_sources
    ) %>%
    select(region, region_label, year, source, value_mtco2_yr) %>%
    pivot_wider(names_from = year, values_from = value_mtco2_yr) %>%
    mutate(excess_mtco2_yr = `2070` - (`2060` + `2080`) / 2) %>%
    group_by(region, region_label) %>%
    mutate(total_excess_mtco2_yr = sum(excess_mtco2_yr)) %>%
    ungroup() %>%
    mutate(
      region_label = factor(
        region_label,
        levels = unique(region_label[order(total_excess_mtco2_yr)])
      )
    )
  write_csv(
    r10_spike_excess,
    file.path(spike_root, "r10_2070_positive_luc_excess_by_source_011.csv")
  )

  p <- ggplot(r10_spike_excess, aes(region_label, excess_mtco2_yr, fill = source)) +
    geom_hline(yintercept = 0, colour = "grey35", linewidth = .4) +
    geom_col(width = .68) +
    coord_flip() +
    scale_fill_manual(values = source_colours, name = NULL, drop = TRUE) +
    scale_y_continuous(labels = label_number(big.mark = ",", accuracy = 1)) +
    labs(
      title = "Europe and Africa account for most of the 2070 excess in Both CDR",
      subtitle = "Excess = 2070 rate minus the mean of 2060 and 2080; source stacks show where the temporary peak occurs",
      x = NULL, y = expression("2070 excess positive LUC emissions (Mt CO"[2] * "/yr)"),
      caption = "This adjacent-year contrast describes the shape of the pulse; it is not a causal policy decomposition."
    ) + emission_theme +
    theme(panel.grid.major.y = element_blank())
  ggsave(
    file.path(spike_root, "r10_2070_positive_luc_excess_by_source_011.png"),
    p, width = 11.5, height = 7.3, dpi = 300, bg = "white"
  )
}

# 7. Validation -------------------------------------------------------------

validation <- tibble(
  check = c(
    "Original reported Net minus original Positive plus Negative",
    "Detailed source sum minus adjusted Negative excluding managed forest",
    "Positive and negative source stacks minus adjusted Net",
    "R10 adjusted Positive/Negative/Net sum minus World"
  ),
  max_abs_residual_mtco2_yr = c(
    max(abs(luc_balance$reported_net_residual_mtco2_yr), na.rm = TRUE),
    max(abs(source_closure$removal_source_residual_mtco2_yr), na.rm = TRUE),
    max(abs(source_balance_closure$source_balance_residual_mtco2_yr), na.rm = TRUE),
    luc_balance %>%
      select(state, region, year, Positive, Negative, Net) %>%
      pivot_longer(c(Positive, Negative, Net), names_to = "emission_category",
                   values_to = "value_mtco2_yr") %>%
      group_by(state, year, emission_category) %>%
      summarise(
        residual = sum(value_mtco2_yr[region %in% names(r10_labels)]) -
          value_mtco2_yr[region == "World"],
        .groups = "drop"
      ) %>%
      summarise(value = max(abs(residual), na.rm = TRUE)) %>%
      pull(value)
  )
)
write_csv(
  validation,
  file.path(analysis_root, paste0("luc_emissions_balance_validation_", target_year, ".csv"))
)

message("Wrote gross/net LUC emission outputs to ", analysis_root)
