suppressPackageStartupMessages({
  library(dplyr)
  library(gdxrrw)
  library(ggplot2)
  library(ggrepel)
  library(readr)
  library(scales)
  library(tidyr)
})

source("prog/common.R")
init_gdx()

analysis_root <- file.path(output_dir, "12_simple_overviews")
unlink(analysis_root, recursive = TRUE)
world_root <- file.path(analysis_root, "01_world_land_trajectories")
r10_root <- file.path(analysis_root, "02_r10_land_trajectories")
country_root <- file.path(analysis_root, "03_country_simple_relationships")
stack_root <- file.path(analysis_root, "04_land_change_stacks")
for (path in c(world_root, r10_root, country_root, stack_root)) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
}

simple_theme <- theme_paper() +
  theme(
    plot.title = element_text(face = "bold", size = 14, margin = margin(b = 6)),
    plot.subtitle = element_text(size = 10.5, colour = "grey30", margin = margin(b = 8)),
    plot.caption = element_text(size = 8.3, colour = "grey35", hjust = 0),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

states <- scenario_states("global2")
scenario_labels <- c(
  `000` = "No policy",
  `100` = "Protection",
  `010` = "Afforestation",
  `001` = "BECCS",
  `110` = "Protection + afforestation",
  `101` = "Protection + BECCS",
  `011` = "Both CDR",
  `111` = "Protection + both CDR"
)
scenario_colours <- c(
  `000` = "#000000", `100` = "#CC79A7", `010` = "#009E73", `001` = "#E69F00",
  `110` = "#56B4E9", `101` = "#D55E00", `011` = "#0072B2", `111` = "#7F3C8D"
)
scenario_linetypes <- c(
  `000` = "solid", `100` = "longdash", `010` = "dotdash", `001` = "dotted",
  `110` = "twodash", `101` = "dashed", `011` = "solid", `111` = "solid"
)
four_states <- c("000", "100", "011", "111")
r10_labels <- c(
  R10AFRICA = "Africa", `R10CHINA+` = "China+", R10EUROPE = "Europe",
  `R10INDIA+` = "India+", R10LATIN_AM = "Latin America",
  R10MIDDLE_EAST = "Middle East", R10NORTH_AM = "North America",
  R10PAC_OECD = "Pacific OECD", R10REF_ECON = "Reforming economies",
  R10REST_ASIA = "Rest of Asia"
)

# 1. Simple land-use trajectories -----------------------------------------

land_variables <- c(
  Lan_Cov_Cro_Non_Ene_Cro = "Food cropland",
  Lan_Cov_Cro_Ene_Cro_2nd_gen = "2G bioenergy cropland",
  Lan_Cov_Frs_Aff = "Afforestation",
  Lan_Cov_Frs = "Forest",
  Lan_Cov_Pst = "Pasture",
  Lan_Cov_Oth_Nat_Lan = "Other natural land",
  Lan_Cov_Pro_Are_Prm_Sec_Frs_Gra_Int = "Integrated protected footprint"
)
merged_file <- file.path(data_dir, "PLUM", "Mergedcomparison.gdx")
land_raw <- rgdx.param(merged_file, "IAMCTempwoU") %>%
  filter(
    i1 %in% unname(states), i2 %in% c("WLD", names(r10_labels)),
    i3 %in% names(land_variables),
    as.integer(as.character(i4)) >= 2020,
    as.integer(as.character(i4)) <= target_year
  ) %>%
  transmute(
    state = state_from_scenario(i1), geography = as.character(i2),
    year = as.integer(as.character(i4)),
    land_class = unname(land_variables[as.character(i3)]),
    area_million_ha = as.numeric(value)
  ) %>%
  group_by(state, geography, year, land_class) %>%
  summarise(area_million_ha = sum(area_million_ha, na.rm = TRUE), .groups = "drop")

analysis_years <- sort(unique(land_raw$year))
land_trajectories <- land_raw %>%
  complete(
    state = names(states), geography = c("WLD", names(r10_labels)),
    year = analysis_years, land_class = unname(land_variables),
    fill = list(area_million_ha = 0)
  ) %>%
  mutate(
    scenario = factor(unname(scenario_labels[state]), levels = unname(scenario_labels)),
    region = if_else(geography == "WLD", "World", unname(r10_labels[geography])),
    land_class = factor(land_class, levels = unname(land_variables))
  )
write_csv(land_trajectories,
          file.path(analysis_root, paste0("land_use_trajectories_world_r10_to_", target_year, ".csv")))

world_four <- land_trajectories %>% filter(geography == "WLD", state %in% four_states)
p <- ggplot(world_four,
            aes(year, area_million_ha, colour = state, linetype = state)) +
  geom_line(linewidth = .9) +
  geom_point(size = 1.35) +
  facet_wrap(vars(land_class), scales = "free_y", ncol = 3) +
  scale_colour_manual(values = scenario_colours[four_states], breaks = four_states,
                      labels = scenario_labels[four_states], name = NULL) +
  scale_linetype_manual(values = scenario_linetypes[four_states], breaks = four_states,
                        labels = scenario_labels[four_states], name = NULL) +
  scale_x_continuous(breaks = pretty_breaks(5)) +
  scale_y_continuous(labels = label_number(big.mark = ",")) +
  labs(
    title = "Global land-use trajectories across the four main policy settings",
    subtitle = "Each panel has its own vertical scale so both large and small land uses remain visible",
    x = NULL, y = "Area (million ha)",
    caption = "MergedComparison, 2020 to the selected end year. Afforestation and protected footprint are diagnostic subsets and are not added to the exclusive land total."
  ) + simple_theme
ggsave(file.path(world_root, paste0("world_land_use_trajectories_four_scenarios_to_", target_year, ".png")),
       p, width = 13, height = 9, dpi = 300, bg = "white")

world_cdr <- land_trajectories %>%
  filter(
    geography == "WLD",
    land_class %in% c("Food cropland", "2G bioenergy cropland", "Afforestation")
  )
p <- ggplot(world_cdr,
            aes(year, area_million_ha, colour = state, linetype = state)) +
  geom_line(linewidth = .8) +
  geom_point(size = 1.15) +
  facet_wrap(vars(land_class), scales = "free_y", ncol = 3) +
  scale_colour_manual(values = scenario_colours, breaks = names(scenario_labels),
                      labels = scenario_labels, name = NULL) +
  scale_linetype_manual(values = scenario_linetypes, breaks = names(scenario_labels),
                        labels = scenario_labels, name = NULL) +
  scale_x_continuous(breaks = pretty_breaks(5)) +
  scale_y_continuous(labels = label_number(big.mark = ",")) +
  labs(
    title = "Afforestation and BECCS scenarios separate the global land responses",
    subtitle = "All eight factorial scenarios are shown for the three land uses central to the research question",
    x = NULL, y = "Area (million ha)",
    caption = "MergedComparison. NoAff and NoBio scenarios identify which CDR option is present; colours and line types encode the same scenario."
  ) + simple_theme + theme(legend.text = element_text(size = 8))
ggsave(file.path(world_root, paste0("world_target_land_trajectories_all_scenarios_to_", target_year, ".png")),
       p, width = 14, height = 6.8, dpi = 300, bg = "white")

r10_four <- land_trajectories %>%
  filter(geography %in% names(r10_labels), state %in% four_states) %>%
  mutate(region = factor(region, levels = unname(r10_labels)))
for (land_name in unname(land_variables)) {
  plot_data <- r10_four %>% filter(land_class == land_name)
  p <- ggplot(plot_data,
              aes(year, area_million_ha, colour = state, linetype = state)) +
    geom_line(linewidth = .72) +
    geom_point(size = .9) +
    facet_wrap(vars(region), scales = "free_y", ncol = 2) +
    scale_colour_manual(values = scenario_colours[four_states], breaks = four_states,
                        labels = scenario_labels[four_states], name = NULL) +
    scale_linetype_manual(values = scenario_linetypes[four_states], breaks = four_states,
                          labels = scenario_labels[four_states], name = NULL) +
    scale_x_continuous(breaks = pretty_breaks(4)) +
    scale_y_continuous(labels = label_number(big.mark = ",")) +
    labs(
      title = paste0(land_name, " trajectories by R10 region"),
      subtitle = "Four policy settings are shown directly; vertical scales differ across regions",
      x = NULL, y = "Area (million ha)",
      caption = "MergedComparison. Compare the blue and purple lines to see the additional protection effect under both CDR options."
    ) + simple_theme
  file_stub <- gsub("[^a-z0-9]+", "_", tolower(land_name))
  ggsave(file.path(r10_root, paste0("r10_", file_stub, "_trajectories_to_", target_year, ".png")),
         p, width = 12, height = 12, dpi = 300, bg = "white")
}

# 2. Diverging land-change stacks -----------------------------------------

# These seven classes are mutually exclusive. Afforestation and protected
# footprint are omitted because they are subsets/overlays and would otherwise
# be counted twice in the land balance.
balance_variables <- c(
  "Lan_Cov_Cro", "Lan_Cov_Cro_Non_Ene_Cro", "Lan_Cov_Cro_Ene_Cro_2nd_gen",
  "Lan_Cov_Frs", "Lan_Cov_Pst", "Lan_Cov_Oth_Nat_Lan", "Lan_Cov_Bui_Are"
)
exclusive_labels <- c(
  "Food cropland", "2G bioenergy cropland", "Fallow cropland",
  "Forest", "Pasture", "Other natural land", "Built-up area"
)
exclusive_colours <- c(
  "Food cropland" = "#E69F00", "2G bioenergy cropland" = "#CC79A7",
  "Fallow cropland" = "#F0E442", "Forest" = "#009E73",
  "Pasture" = "#56B4E9", "Other natural land" = "#0072B2",
  "Built-up area" = "#666666"
)
balance_wide <- rgdx.param(merged_file, "IAMCTempwoU") %>%
  filter(
    i1 %in% unname(states), i2 %in% c("WLD", names(r10_labels)),
    i3 %in% balance_variables,
    as.integer(as.character(i4)) == target_year
  ) %>%
  transmute(
    state = state_from_scenario(i1), geography = as.character(i2),
    variable = as.character(i3), area_million_ha = as.numeric(value)
  ) %>%
  group_by(state, geography, variable) %>%
  summarise(area_million_ha = sum(area_million_ha, na.rm = TRUE), .groups = "drop") %>%
  complete(
    state = names(states), geography = c("WLD", names(r10_labels)),
    variable = balance_variables, fill = list(area_million_ha = 0)
  ) %>%
  pivot_wider(names_from = variable, values_from = area_million_ha) %>%
  transmute(
    state, geography,
    `Food cropland` = Lan_Cov_Cro_Non_Ene_Cro,
    `2G bioenergy cropland` = Lan_Cov_Cro_Ene_Cro_2nd_gen,
    `Fallow cropland` = Lan_Cov_Cro - Lan_Cov_Cro_Non_Ene_Cro -
      Lan_Cov_Cro_Ene_Cro_2nd_gen,
    Forest = Lan_Cov_Frs, Pasture = Lan_Cov_Pst,
    `Other natural land` = Lan_Cov_Oth_Nat_Lan,
    `Built-up area` = Lan_Cov_Bui_Are
  )

scenario_order <- c("100", "010", "001", "011", "110", "101", "111")
scenario_short_labels <- c(
  `100` = "Protection", `010` = "Afforestation", `001` = "BECCS",
  `011` = "Both CDR", `110` = "Protection + aff.",
  `101` = "Protection + BECCS", `111` = "All three"
)
baseline_land_total <- balance_wide %>%
  filter(state == "000") %>%
  rowwise() %>%
  transmute(
    geography,
    no_policy_total_land_mha = sum(c_across(all_of(exclusive_labels)), na.rm = TRUE)
  ) %>%
  ungroup()
balance_change <- balance_wide %>%
  pivot_longer(all_of(exclusive_labels), names_to = "land_class", values_to = "area_million_ha") %>%
  group_by(geography, land_class) %>%
  mutate(change_from_no_policy_mha = area_million_ha - area_million_ha[state == "000"]) %>%
  ungroup() %>%
  filter(state %in% scenario_order) %>%
  mutate(
    scenario = factor(unname(scenario_short_labels[state]), levels = unname(scenario_short_labels)),
    land_class = factor(land_class, levels = exclusive_labels),
    region = if_else(geography == "WLD", "World", unname(r10_labels[geography]))
  ) %>%
  left_join(baseline_land_total, by = "geography") %>%
  mutate(change_from_no_policy_pct_land = 100 * change_from_no_policy_mha / no_policy_total_land_mha)
balance_closure <- balance_change %>%
  group_by(geography, region, state, scenario) %>%
  summarise(net_change_mha = sum(change_from_no_policy_mha), .groups = "drop")
write_csv(
  balance_change %>% left_join(balance_closure, by = c("geography", "region", "state", "scenario")),
  file.path(stack_root, paste0("land_class_changes_from_no_policy_world_r10_", target_year, ".csv"))
)

world_balance <- balance_change %>% filter(geography == "WLD")
p <- ggplot(world_balance, aes(scenario, change_from_no_policy_mha, fill = land_class)) +
  geom_hline(yintercept = 0, colour = "grey30", linewidth = .35) +
  geom_col(width = .72) +
  scale_fill_manual(values = exclusive_colours, name = NULL) +
  scale_y_continuous(labels = label_number(big.mark = ","),
                     expand = expansion(mult = c(.08, .08))) +
  labs(
    title = "Which land classes increase and decrease in each global scenario?",
    subtitle = "Every bar is the 2100 difference from No policy; increases stack upward and decreases downward",
    x = NULL, y = "Change from No policy (million ha)",
    caption = "MergedComparison. Seven mutually exclusive land classes are used, so Afforestation and protected-area overlays are not double-counted."
  ) + simple_theme +
  theme(axis.text.x = element_text(angle = 25, hjust = 1), panel.grid.major.x = element_blank())
ggsave(file.path(stack_root, paste0("world_scenario_land_change_stacks_", target_year, ".png")),
       p, width = 13, height = 7.2, dpi = 300, bg = "white")

r10_balance <- balance_change %>%
  filter(geography %in% names(r10_labels)) %>%
  mutate(region = factor(region, levels = unname(r10_labels)))
p <- ggplot(r10_balance, aes(scenario, change_from_no_policy_mha, fill = land_class)) +
  geom_hline(yintercept = 0, colour = "grey30", linewidth = .3) +
  geom_col(width = .72) +
  facet_wrap(vars(region), scales = "free_y", ncol = 2) +
  scale_fill_manual(values = exclusive_colours, name = NULL) +
  scale_y_continuous(labels = label_number(big.mark = ","),
                     expand = expansion(mult = c(.06, .06))) +
  labs(
    title = "Land gains and losses differ across scenarios and R10 regions",
    subtitle = "All scenario bars use the same No-policy baseline within each region",
    x = NULL, y = "Change from No policy (million ha)",
    caption = "MergedComparison, selected end year. Vertical scales differ across regions; compare absolute values using the axis labels or CSV."
  ) + simple_theme +
  theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 7.5),
        panel.grid.major.x = element_blank())
ggsave(file.path(stack_root, paste0("r10_scenario_land_change_stacks_", target_year, ".png")),
       p, width = 14, height = 15, dpi = 300, bg = "white")

p <- ggplot(r10_balance,
            aes(scenario, change_from_no_policy_pct_land, fill = land_class)) +
  geom_hline(yintercept = 0, colour = "grey30", linewidth = .3) +
  geom_col(width = .72) +
  facet_wrap(vars(region), ncol = 2) +
  scale_fill_manual(values = exclusive_colours, name = NULL) +
  scale_y_continuous(labels = label_number(accuracy = .1, suffix = "%"),
                     expand = expansion(mult = c(.06, .06))) +
  labs(
    title = "Regional land gains and losses as shares of land area",
    subtitle = "Normalization by each region's No-policy land total makes R10 magnitudes comparable",
    x = NULL, y = "Change from No policy (% of regional land)",
    caption = "MergedComparison, selected end year. Positive and negative segments use the same mutually exclusive land classes as the absolute-area figure."
  ) + simple_theme +
  theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 7.5),
        panel.grid.major.x = element_blank())
ggsave(file.path(stack_root, paste0("r10_scenario_land_change_stacks_share_", target_year, ".png")),
       p, width = 14, height = 15, dpi = 300, bg = "white")

r10_protection_balance <- balance_wide %>%
  filter(state %in% c("011", "111"), geography %in% names(r10_labels)) %>%
  pivot_longer(all_of(exclusive_labels), names_to = "land_class", values_to = "area_million_ha") %>%
  pivot_wider(names_from = state, values_from = area_million_ha) %>%
  transmute(
    geography,
    region = factor(unname(r10_labels[geography]), levels = rev(unname(r10_labels))),
    land_class = factor(land_class, levels = exclusive_labels),
    protection_change_mha = `111` - `011`
  ) %>%
  left_join(baseline_land_total, by = "geography") %>%
  mutate(protection_change_pct_land = 100 * protection_change_mha / no_policy_total_land_mha)
write_csv(r10_protection_balance,
          file.path(stack_root, paste0("r10_protection_under_cdr_land_change_", target_year, ".csv")))
p <- ggplot(r10_protection_balance,
            aes(region, protection_change_mha, fill = land_class)) +
  geom_hline(yintercept = 0, colour = "grey30", linewidth = .35) +
  geom_col(width = .7) +
  coord_flip() +
  scale_fill_manual(values = exclusive_colours, name = NULL) +
  scale_y_continuous(labels = label_number(big.mark = ","),
                     expand = expansion(mult = c(.08, .08))) +
  labs(
    title = "How does protection reallocate land under both CDR options?",
    subtitle = "Positive segments gain area and negative segments lose area when moving from 011 to 111",
    x = NULL, y = "Protection-induced change (million ha)",
    caption = "MergedComparison, selected end year. The seven exclusive classes approximately close within each region; segment signs describe area, not welfare."
  ) + simple_theme + theme(panel.grid.major.y = element_blank())
ggsave(file.path(stack_root, paste0("r10_protection_under_cdr_land_change_stacks_", target_year, ".png")),
       p, width = 12, height = 8.2, dpi = 300, bg = "white")

p <- ggplot(r10_protection_balance,
            aes(region, protection_change_pct_land, fill = land_class)) +
  geom_hline(yintercept = 0, colour = "grey30", linewidth = .35) +
  geom_col(width = .7) +
  coord_flip() +
  scale_fill_manual(values = exclusive_colours, name = NULL) +
  scale_y_continuous(labels = label_number(accuracy = .1, suffix = "%"),
                     expand = expansion(mult = c(.08, .08))) +
  labs(
    title = "Protection-induced land reallocation relative to regional land area",
    subtitle = "The same 111−011 changes are normalized by each region's No-policy land total",
    x = NULL, y = "Protection-induced change (% of regional land)",
    caption = "MergedComparison, selected end year. This view compares intensity across regions; use the million-ha version for absolute contribution."
  ) + simple_theme + theme(panel.grid.major.y = element_blank())
ggsave(file.path(stack_root, paste0("r10_protection_under_cdr_land_change_stacks_share_", target_year, ".png")),
       p, width = 12, height = 8.2, dpi = 300, bg = "white")

# The world time series uses the same exclusive classes and compares every
# scenario with No policy in the same year.
balance_time_levels <- rgdx.param(merged_file, "IAMCTempwoU") %>%
  filter(
    i1 %in% unname(states), i2 %in% c("WLD", names(r10_labels)),
    i3 %in% balance_variables,
    as.integer(as.character(i4)) >= 2020,
    as.integer(as.character(i4)) <= target_year
  ) %>%
  transmute(
    state = state_from_scenario(i1), geography = as.character(i2),
    year = as.integer(as.character(i4)),
    variable = as.character(i3), area_million_ha = as.numeric(value)
  ) %>%
  group_by(state, geography, year, variable) %>%
  summarise(area_million_ha = sum(area_million_ha, na.rm = TRUE), .groups = "drop") %>%
  complete(
    state = names(states), geography = c("WLD", names(r10_labels)),
    year = analysis_years, variable = balance_variables,
    fill = list(area_million_ha = 0)
  ) %>%
  pivot_wider(names_from = variable, values_from = area_million_ha) %>%
  transmute(
    state, geography, year,
    `Food cropland` = Lan_Cov_Cro_Non_Ene_Cro,
    `2G bioenergy cropland` = Lan_Cov_Cro_Ene_Cro_2nd_gen,
    `Fallow cropland` = Lan_Cov_Cro - Lan_Cov_Cro_Non_Ene_Cro -
      Lan_Cov_Cro_Ene_Cro_2nd_gen,
    Forest = Lan_Cov_Frs, Pasture = Lan_Cov_Pst,
    `Other natural land` = Lan_Cov_Oth_Nat_Lan,
    `Built-up area` = Lan_Cov_Bui_Are
  ) %>%
  pivot_longer(all_of(exclusive_labels), names_to = "land_class", values_to = "area_million_ha")

balance_time <- balance_time_levels %>%
  filter(geography == "WLD") %>%
  group_by(year, land_class) %>%
  mutate(change_from_no_policy_mha = area_million_ha - area_million_ha[state == "000"]) %>%
  ungroup() %>%
  filter(state %in% scenario_order) %>%
  mutate(
    scenario = factor(unname(scenario_short_labels[state]), levels = unname(scenario_short_labels)),
    land_class = factor(land_class, levels = exclusive_labels)
  )
write_csv(balance_time,
          file.path(stack_root, paste0("world_land_class_changes_over_time_to_", target_year, ".csv")))
p <- ggplot(balance_time,
            aes(year, change_from_no_policy_mha, fill = land_class)) +
  geom_hline(yintercept = 0, colour = "grey30", linewidth = .3) +
  geom_col(width = 7) +
  facet_wrap(vars(scenario), ncol = 2, scales = "free_y") +
  scale_fill_manual(values = exclusive_colours, name = NULL) +
  scale_x_continuous(breaks = pretty_breaks(5)) +
  scale_y_continuous(labels = label_number(big.mark = ","),
                     expand = expansion(mult = c(.05, .05))) +
  labs(
    title = "How do positive and negative land changes develop over time?",
    subtitle = "Each scenario is compared with No policy in the same year",
    x = NULL, y = "Change from No policy (million ha)",
    caption = "MergedComparison. The panels show net changes by exclusive land class; they do not measure gross movement among individual grid cells."
  ) + simple_theme + theme(panel.grid.major.x = element_blank())
ggsave(file.path(stack_root, paste0("world_scenario_land_change_stacks_over_time_to_", target_year, ".png")),
       p, width = 13, height = 15, dpi = 300, bg = "white")

r10_protection_time <- balance_time_levels %>%
  filter(geography %in% names(r10_labels), state %in% c("011", "111")) %>%
  pivot_wider(names_from = state, values_from = area_million_ha) %>%
  transmute(
    region = factor(unname(r10_labels[geography]), levels = unname(r10_labels)),
    year,
    land_class = factor(land_class, levels = exclusive_labels),
    protection_change_mha = `111` - `011`
  )
write_csv(r10_protection_time,
          file.path(stack_root, paste0("r10_protection_under_cdr_land_changes_over_time_to_", target_year, ".csv")))
p <- ggplot(r10_protection_time,
            aes(year, protection_change_mha, fill = land_class)) +
  geom_hline(yintercept = 0, colour = "grey30", linewidth = .3) +
  geom_col(width = 7) +
  facet_wrap(vars(region), scales = "free_y", ncol = 2) +
  scale_fill_manual(values = exclusive_colours, name = NULL) +
  scale_x_continuous(breaks = pretty_breaks(4)) +
  scale_y_continuous(labels = label_number(big.mark = ","),
                     expand = expansion(mult = c(.05, .05))) +
  labs(
    title = "How does protection-induced land reallocation develop across R10 regions?",
    subtitle = "Each year compares Protection + both CDR with Both CDR (111−011)",
    x = NULL, y = "Protection-induced change (million ha)",
    caption = "MergedComparison. Vertical scales differ across regions; positive and negative segments are mutually exclusive land classes."
  ) + simple_theme + theme(panel.grid.major.x = element_blank())
ggsave(file.path(stack_root, paste0("r10_protection_under_cdr_land_change_stacks_over_time_to_", target_year, ".png")),
       p, width = 14, height = 16, dpi = 300, bg = "white")

# 3. Simple country relationships -----------------------------------------

country_evidence <- read_csv(
  file.path(output_dir, "11_results_evidence_chain", "04_country_transmission",
            paste0("country_land_market_evidence_", target_year, ".csv")),
  show_col_types = FALSE
)
country_land <- read_csv(
  file.path(output_dir, "08_land_use_protection", "02_protection_displacement",
            "protection_land_displacement_all_levels.csv"),
  show_col_types = FALSE
) %>%
  filter(
    geography_level == "country",
    land_class %in% c("Afforestation", "2G bioenergy cropland")
  ) %>%
  select(country = geography, land_class, area_011 = `011`, total_land) %>%
  pivot_wider(names_from = land_class, values_from = area_011) %>%
  mutate(
    afforestation_land_share_pct = 100 * Afforestation / total_land,
    bioenergy_land_share_pct = 100 * `2G bioenergy cropland` / total_land,
    total_cdr_land_share_pct = afforestation_land_share_pct + bioenergy_land_share_pct
  )
direct_protection <- read_csv(
  file.path(output_dir, "08_land_use_protection", "02_protection_displacement", "country",
            paste0("country_protection_reallocation_", target_year, ".csv")),
  show_col_types = FALSE
) %>%
  transmute(
    country = geography, protected_share_011_pct, protected_share_111_pct,
    added_protected_share_of_land_pp
  )
country_simple <- country_evidence %>%
  select(
    country, r10, production_weighted_protection_increase_pp,
    production_total_pct, producer_price_total_pct, food_intake_total_pct
  ) %>%
  left_join(country_land, by = "country") %>%
  left_join(direct_protection, by = "country")
write_csv(country_simple,
          file.path(country_root, paste0("country_simple_relationship_data_", target_year, ".csv")))

outcome_labels <- c(
  production_total_pct = "CRP production change (%)",
  producer_price_total_pct = "Producer-price change (%)",
  food_intake_total_pct = "Food-intake change (%)"
)
country_outcomes <- country_simple %>%
  pivot_longer(all_of(names(outcome_labels)), names_to = "outcome_key", values_to = "outcome_value") %>%
  mutate(outcome = factor(unname(outcome_labels[outcome_key]), levels = unname(outcome_labels)))

relationship_plot <- function(data, x_key, x_label, title, subtitle, caption, x_transform = "identity") {
  plot_data <- data %>%
    transmute(
      country, outcome, outcome_value,
      x_value = .data[[x_key]]
    ) %>%
    filter(is.finite(x_value), is.finite(outcome_value)) %>%
    group_by(outcome) %>%
    mutate(label = if_else(
      rank(-abs(outcome_value), ties.method = "first") <= 5 |
        rank(-x_value, ties.method = "first") <= 3,
      country, NA_character_
    )) %>%
    ungroup()
  stats <- plot_data %>%
    group_by(outcome) %>%
    summarise(
      n = n(), rho = cor(x_value, outcome_value, method = "spearman"),
      stat_label = paste0("Spearman ρ = ", number(rho, accuracy = .01), "; n = ", n),
      x_label_pos = min(x_value, na.rm = TRUE),
      .groups = "drop"
    )
  p <- ggplot(plot_data, aes(x_value, outcome_value)) +
    geom_hline(yintercept = 0, colour = "grey65", linewidth = .3) +
    geom_smooth(method = "loess", formula = y ~ x, se = FALSE,
                colour = "#D55E00", linewidth = .8, span = .9) +
    geom_point(colour = "#2C3E50", alpha = .7, size = 1.8) +
    geom_text_repel(aes(label = label), na.rm = TRUE, size = 2.7,
                    max.overlaps = Inf, min.segment.length = 0) +
    geom_text(
      data = stats, aes(x = x_label_pos, y = Inf, label = stat_label),
      inherit.aes = FALSE, hjust = 0, vjust = 1.25, size = 3.1, colour = "grey25"
    ) +
    facet_wrap(vars(outcome), scales = "free_y", ncol = 3) +
    labs(title = title, subtitle = subtitle, x = x_label, y = NULL, caption = caption) +
    simple_theme
  if (x_transform == "pseudo_log") {
    p <- p + scale_x_continuous(
      trans = pseudo_log_trans(sigma = .2, base = 10),
      breaks = c(0, .1, .3, 1, 3, 10, 30, 100),
      expand = expansion(mult = c(.02, .10))
    )
  } else {
    p <- p + scale_x_continuous(expand = expansion(mult = c(.02, .08)))
  }
  p
}

p <- relationship_plot(
  country_outcomes,
  "production_weighted_protection_increase_pp",
  "Protection increase in crop-producing basins (percentage points)",
  "Countries with more protection pressure tend to lose more crop production",
  "Each point is a country; outcomes compare protection + both CDR with both CDR (111−011)",
  "The x-axis is a production-weighted basin protection proxy from protPlum, not direct cropland overlap. Orange curves are descriptive loess trends."
)
ggsave(file.path(country_root, paste0("country_crop_basin_protection_vs_agriculture_", target_year, ".png")),
       p, width = 14, height = 5.8, dpi = 300, bg = "white")

for (spec in list(
  list(
    key = "total_cdr_land_share_pct", label = "Afforestation + 2G bioenergy land in scenario 011 (% of country land)",
    title = "Agricultural protection effects differ with the amount of CDR land",
    file = "country_total_cdr_land_vs_agriculture"
  ),
  list(
    key = "afforestation_land_share_pct", label = "Afforestation land in scenario 011 (% of country land)",
    title = "Countries with more afforestation land show different protection responses",
    file = "country_afforestation_land_vs_agriculture"
  ),
  list(
    key = "bioenergy_land_share_pct", label = "2G bioenergy cropland in scenario 011 (% of country land)",
    title = "Countries with more BECCS land show different protection responses",
    file = "country_beccs_land_vs_agriculture"
  )
)) {
  p <- relationship_plot(
    country_outcomes, spec$key, spec$label, spec$title,
    "The horizontal axis uses a pseudo-log scale so zero and small land shares remain visible",
    "CDR land is measured before protection in the both-CDR scenario (011); vertical outcomes are the subsequent protection effects (111−011). Orange curves are descriptive loess trends.",
    x_transform = "pseudo_log"
  )
  ggsave(file.path(country_root, paste0(spec$file, "_", target_year, ".png")),
         p, width = 14, height = 5.8, dpi = 300, bg = "white")
}

direct_data <- country_outcomes %>%
  filter(
    is.finite(added_protected_share_of_land_pp), added_protected_share_of_land_pp > 0,
    is.finite(outcome_value)
  )
p <- ggplot(direct_data,
            aes(added_protected_share_of_land_pp, outcome_value, label = country)) +
  geom_hline(yintercept = 0, colour = "grey65", linewidth = .3) +
  geom_point(colour = "#0072B2", size = 2.6) +
  geom_text_repel(size = 3, max.overlaps = Inf, min.segment.length = 0) +
  facet_wrap(vars(outcome), scales = "free_y", ncol = 3) +
  labs(
    title = "Directly reported protection expansion is concentrated in eight countries",
    subtitle = "Seven countries have matched AgMIP CRP outcomes; Japan's outcome ratios are missing and remain NA in the CSV",
    x = "Added protected footprint / country land (percentage points)", y = NULL,
    caption = "Both axes compare protection + both CDR with both CDR. This direct protected-area measure is kept separate from the broader protPlum basin-pressure proxy."
  ) + simple_theme
ggsave(file.path(country_root, paste0("country_direct_added_protection_vs_agriculture_", target_year, ".png")),
       p, width = 14, height = 5.8, dpi = 300, bg = "white")

gc()
