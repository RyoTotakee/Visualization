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

analysis_root <- file.path(output_dir, "08_land_use_protection")
unlink(analysis_root, recursive = TRUE)
inventory_root <- file.path(analysis_root, "01_land_inventory")
movement_root <- file.path(analysis_root, "02_protection_displacement")
footprint_root <- file.path(analysis_root, "03_protected_footprint")
grid_root <- file.path(analysis_root, "04_grid_relocation", "world")
for (path in c(inventory_root, movement_root, footprint_root, grid_root)) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
}

states <- scenario_states("global2")
comparison_states <- c("000", "100", "011", "111")
scenario_labels <- c(
  `000` = "BaU", `100` = "Protection only",
  `011` = "Mitigation", `111` = "Mitigation + protection"
)
r10_labels <- c(
  R10AFRICA = "Africa", `R10CHINA+` = "China+", R10EUROPE = "Europe",
  `R10INDIA+` = "India+", R10LATIN_AM = "Latin America",
  R10MIDDLE_EAST = "Middle East", R10NORTH_AM = "North America",
  R10PAC_OECD = "Pacific OECD", R10REF_ECON = "Reforming economies",
  R10REST_ASIA = "Rest of Asia"
)
country_codes <- read_csv(
  file.path(data_dir, "country_mapping.csv"), show_col_types = FALSE
)$country
geographies <- c(country_codes, names(r10_labels), "WLD")

land_variables <- c(
  "Lan_Cov", "Lan_Cov_Bui_Are", "Lan_Cov_Cro",
  "Lan_Cov_Cro_Non_Ene_Cro", "Lan_Cov_Cro_Ene_Cro_2nd_gen",
  "Lan_Cov_Frs", "Lan_Cov_Frs_Aff", "Lan_Cov_Frs_Ref",
  "Lan_Cov_Oth_Lan", "Lan_Cov_Oth_Nat_Lan", "Lan_Cov_Pst",
  "Lan_Cov_Pro_Are_Prm_Sec_Frs_Gra_Int",
  "Lan_Cov_Pro_Are_Prm_Sec_Frs_Gra",
  "Lan_Cov_Pro_Are_Prm_Frs", "Lan_Cov_Pro_Are_Sec_Frs",
  "Lan_Cov_Pro_Are_Prm_Gra", "Lan_Cov_Pro_Are_Sec_Gra",
  "Lan_Cov_Pro_Are_Cro", "Lan_Cov_Pro_Are_Pas"
)

# MergedComparison land data -------------------------------------------------

merged_file <- file.path(data_dir, "PLUM", "Mergedcomparison.gdx")
raw_land <- rgdx.param(merged_file, "IAMCTempwoU") %>%
  filter(
    i1 %in% unname(states[comparison_states]),
    i2 %in% geographies,
    i3 %in% land_variables,
    as.integer(as.character(i4)) >= 2020,
    as.integer(as.character(i4)) <= target_year
  ) %>%
  transmute(
    state = state_from_scenario(i1, "global2"),
    geography = as.character(i2),
    variable = as.character(i3),
    year = as.integer(as.character(i4)),
    value = as.numeric(value)
  ) %>%
  group_by(state, geography, variable, year) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop")

analysis_years <- sort(unique(raw_land$year))
land_wide <- raw_land %>%
  complete(
    state = comparison_states, geography = geographies,
    variable = land_variables, year = analysis_years,
    fill = list(value = 0)
  ) %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  mutate(
    geography_level = case_when(
      geography == "WLD" ~ "world",
      geography %in% names(r10_labels) ~ "r10",
      TRUE ~ "country"
    ),
    `Food cropland` = Lan_Cov_Cro_Non_Ene_Cro,
    `2G bioenergy cropland` = Lan_Cov_Cro_Ene_Cro_2nd_gen,
    `Fallow cropland` = Lan_Cov_Cro - Lan_Cov_Cro_Non_Ene_Cro -
      Lan_Cov_Cro_Ene_Cro_2nd_gen,
    Forest = Lan_Cov_Frs,
    Pasture = Lan_Cov_Pst,
    `Other natural land` = Lan_Cov_Oth_Nat_Lan,
    `Built-up area` = Lan_Cov_Bui_Are,
    total_land = Lan_Cov,
    other_land_outside_total = Lan_Cov_Oth_Lan,
    inventory_sum = `Food cropland` + `2G bioenergy cropland` +
      `Fallow cropland` + Forest + Pasture + `Other natural land` +
      `Built-up area`,
    inventory_residual = total_land - inventory_sum
  )

exclusive_classes <- c(
  "Food cropland", "2G bioenergy cropland", "Fallow cropland",
  "Forest", "Pasture", "Other natural land", "Built-up area"
)
class_colours <- c(
  "Food cropland" = "#e6ab02", "2G bioenergy cropland" = "#d95f02",
  "Fallow cropland" = "#ffd92f", "Forest" = "#1b9e77",
  "Pasture" = "#a6d854", "Other natural land" = "#66c2a5",
  "Built-up area" = "#7570b3"
)

inventory_long <- land_wide %>%
  select(geography, geography_level, state, year, total_land,
         inventory_sum, inventory_residual, all_of(exclusive_classes)) %>%
  pivot_longer(all_of(exclusive_classes), names_to = "land_class",
               values_to = "area_million_ha") %>%
  mutate(
    land_class = factor(land_class, levels = exclusive_classes),
    share_of_total_pct = if_else(
      abs(total_land) > 1e-10, 100 * area_million_ha / total_land, NA_real_
    )
  )

write_csv(inventory_long, file.path(inventory_root, "land_inventory_all_levels.csv"))
write_csv(
  land_wide %>%
    select(geography, geography_level, state, year, total_land,
           inventory_sum, inventory_residual, other_land_outside_total),
  file.path(inventory_root, "land_inventory_closure.csv")
)

# World and R10 composition --------------------------------------------------

world_inventory <- inventory_long %>%
  filter(geography == "WLD", year == target_year, state %in% comparison_states) %>%
  mutate(scenario = factor(scenario_labels[state], levels = scenario_labels))

dir.create(file.path(inventory_root, "world"), recursive = TRUE, showWarnings = FALSE)
for (metric in c("area_million_ha", "share_of_total_pct")) {
  y_label <- if (metric == "area_million_ha") "Area (million ha)" else
    "Share of reported land total (%)"
  p <- ggplot(world_inventory,
              aes(scenario, .data[[metric]], fill = land_class)) +
    geom_col(width = 0.68) +
    scale_fill_manual(values = class_colours, name = NULL) +
    labs(x = NULL, y = y_label) + theme_paper() +
    theme(axis.text.x = element_text(angle = 20, hjust = 1))
  ggsave(
    file.path(inventory_root, "world",
              paste0("world_land_composition_", metric, "_", target_year, ".png")),
    p, width = 9.2, height = 6.2, dpi = 300, bg = "white"
  )
}
write_csv(world_inventory,
          file.path(inventory_root, "world", paste0("world_land_composition_", target_year, ".csv")))

r10_inventory <- inventory_long %>%
  filter(geography %in% names(r10_labels), year == target_year,
         state %in% c("011", "111")) %>%
  mutate(
    region = factor(unname(r10_labels[geography]), levels = unname(r10_labels)),
    scenario = factor(scenario_labels[state], levels = scenario_labels[c("011", "111")])
  )
dir.create(file.path(inventory_root, "r10"), recursive = TRUE, showWarnings = FALSE)
r10_comp <- ggplot(r10_inventory,
                   aes(region, share_of_total_pct, fill = land_class)) +
  geom_col(width = 0.72) + facet_wrap(vars(scenario), ncol = 1) + coord_flip() +
  scale_fill_manual(values = class_colours, name = NULL) +
  labs(x = NULL, y = "Share of reported land total (%)") + theme_paper()
ggsave(file.path(inventory_root, "r10",
                 paste0("r10_land_composition_", target_year, ".png")),
       r10_comp, width = 9.5, height = 10, dpi = 300, bg = "white")
write_csv(r10_inventory,
          file.path(inventory_root, "r10", paste0("r10_land_composition_", target_year, ".csv")))

# Conditional protection changes --------------------------------------------

movement_channels <- c(exclusive_classes, "Afforestation", "Reforestation",
                       "Other land outside reported total")
movement_long <- land_wide %>%
  filter(year == target_year) %>%
  transmute(
    geography, geography_level, state, year,
    across(all_of(exclusive_classes)),
    Afforestation = Lan_Cov_Frs_Aff,
    Reforestation = Lan_Cov_Frs_Ref,
    `Other land outside reported total` = Lan_Cov_Oth_Lan
  ) %>%
  pivot_longer(all_of(movement_channels), names_to = "land_class",
               values_to = "area_million_ha") %>%
  pivot_wider(names_from = state, values_from = area_million_ha) %>%
  left_join(
    land_wide %>%
      filter(year == target_year, state == "011") %>%
      select(geography, total_land),
    by = "geography"
  ) %>%
  mutate(
    is_exclusive = land_class %in% exclusive_classes,
    protection_without_mitigation_mha = `100` - `000`,
    protection_under_mitigation_mha = `111` - `011`,
    protection_interaction_mha = protection_under_mitigation_mha -
      protection_without_mitigation_mha,
    protection_without_mitigation_pct = if_else(
      abs(`000`) > 1e-10, 100 * protection_without_mitigation_mha / `000`, NA_real_
    ),
    protection_under_mitigation_pct = if_else(
      abs(`011`) > 1e-10, 100 * protection_under_mitigation_mha / `011`, NA_real_
    ),
    context_difference_pp = protection_under_mitigation_pct -
      protection_without_mitigation_pct,
    change_pct_of_total_land = if_else(
      abs(total_land) > 1e-10,
      100 * protection_under_mitigation_mha / total_land, NA_real_
    )
  )

# Integrated protected-area footprint and net inter-class reallocation.
protected_footprint <- land_wide %>%
  filter(year == target_year) %>%
  select(geography, geography_level, state, year, total_land,
         Lan_Cov_Pro_Are_Prm_Sec_Frs_Gra_Int) %>%
  pivot_wider(
    names_from = state,
    values_from = c(total_land, Lan_Cov_Pro_Are_Prm_Sec_Frs_Gra_Int),
    names_glue = "{.value}_{state}"
  ) %>%
  mutate(
    total_land = total_land_011,
    protected_000 = Lan_Cov_Pro_Are_Prm_Sec_Frs_Gra_Int_000,
    protected_100 = Lan_Cov_Pro_Are_Prm_Sec_Frs_Gra_Int_100,
    protected_011 = Lan_Cov_Pro_Are_Prm_Sec_Frs_Gra_Int_011,
    protected_111 = Lan_Cov_Pro_Are_Prm_Sec_Frs_Gra_Int_111,
    added_protected_area_mha = protected_111 - protected_011,
    protected_share_011_pct = if_else(
      abs(total_land) > 1e-10, 100 * protected_011 / total_land, NA_real_
    ),
    protected_share_111_pct = if_else(
      abs(total_land) > 1e-10, 100 * protected_111 / total_land, NA_real_
    ),
    added_protected_share_of_land_pp = protected_share_111_pct -
      protected_share_011_pct
  )

reallocation <- movement_long %>%
  filter(is_exclusive) %>%
  group_by(geography, geography_level, year, total_land) %>%
  summarise(
    area_gained_mha = sum(pmax(protection_under_mitigation_mha, 0), na.rm = TRUE),
    area_lost_mha = sum(pmax(-protection_under_mitigation_mha, 0), na.rm = TRUE),
    total_reallocated_mha = 0.5 * (area_gained_mha + area_lost_mha),
    net_closure_mha = sum(protection_under_mitigation_mha, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(
    protected_footprint %>%
      select(geography, added_protected_area_mha,
             protected_share_011_pct, protected_share_111_pct,
             added_protected_share_of_land_pp),
    by = "geography"
  ) %>%
  mutate(
    reallocated_share_of_land_pct = if_else(
      abs(total_land) > 1e-10, 100 * total_reallocated_mha / total_land, NA_real_
    ),
    reallocated_pct_of_added_protection = if_else(
      added_protected_area_mha > 1e-10,
      100 * total_reallocated_mha / added_protected_area_mha, NA_real_
    )
  )

movement_long <- movement_long %>%
  left_join(
    reallocation %>%
      select(geography, area_gained_mha, area_lost_mha,
             total_reallocated_mha, added_protected_area_mha),
    by = "geography"
  ) %>%
  mutate(
    share_within_gain_or_loss_pct = case_when(
      protection_under_mitigation_mha > 0 & area_gained_mha > 0 ~
        100 * protection_under_mitigation_mha / area_gained_mha,
      protection_under_mitigation_mha < 0 & area_lost_mha > 0 ~
        100 * abs(protection_under_mitigation_mha) / area_lost_mha,
      TRUE ~ 0
    ),
    change_pct_of_added_protection = if_else(
      abs(added_protected_area_mha) > 1e-10,
      100 * protection_under_mitigation_mha / added_protected_area_mha, NA_real_
    )
  )

write_csv(movement_long,
          file.path(movement_root, "protection_land_displacement_all_levels.csv"))
write_csv(reallocation,
          file.path(movement_root, "protection_reallocation_summary_all_levels.csv"))

# World land changes and trajectories ---------------------------------------

dir.create(file.path(movement_root, "world"), recursive = TRUE, showWarnings = FALSE)
world_movement <- movement_long %>% filter(geography == "WLD") %>%
  mutate(land_class = factor(land_class, levels = rev(movement_channels)))
world_context <- world_movement %>%
  select(land_class, protection_without_mitigation_mha,
         protection_under_mitigation_mha, protection_interaction_mha) %>%
  pivot_longer(-land_class, names_to = "comparison", values_to = "change_mha") %>%
  mutate(comparison = factor(
    recode(comparison,
           protection_without_mitigation_mha = "Without mitigation",
           protection_under_mitigation_mha = "Under mitigation",
           protection_interaction_mha = "Difference-in-differences"),
    levels = c("Without mitigation", "Under mitigation", "Difference-in-differences")
  ))
world_change_plot <- ggplot(world_context,
                            aes(land_class, change_mha, fill = comparison)) +
  geom_hline(yintercept = 0, colour = "grey45", linewidth = 0.3) +
  geom_col(position = position_dodge(width = 0.75), width = 0.68) + coord_flip() +
  scale_fill_manual(values = c("#807dba", "#238b45", "#e6550d"), name = NULL) +
  labs(x = NULL, y = "Protection-induced area change (million ha)") +
  theme_paper() + theme(panel.grid.major.y = element_blank())
ggsave(file.path(movement_root, "world",
                 paste0("world_land_displacement_context_", target_year, ".png")),
       world_change_plot, width = 9.5, height = 6.6, dpi = 300, bg = "white")

world_share_plot <- world_movement %>%
  filter(is.finite(protection_under_mitigation_pct)) %>%
  ggplot(aes(land_class, protection_under_mitigation_pct,
             fill = protection_under_mitigation_mha > 0)) +
  geom_hline(yintercept = 0, colour = "grey45", linewidth = 0.3) +
  geom_col(width = 0.68) + coord_flip() +
  scale_fill_manual(values = c("#b2182b", "#2166ac"), guide = "none") +
  labs(x = NULL, y = "Change relative to mitigation baseline (%)") +
  theme_paper() + theme(panel.grid.major.y = element_blank())
ggsave(file.path(movement_root, "world",
                 paste0("world_land_displacement_relative_", target_year, ".png")),
       world_share_plot, width = 8.5, height = 6.2, dpi = 300, bg = "white")
write_csv(world_movement,
          file.path(movement_root, "world", paste0("world_land_displacement_", target_year, ".csv")))

world_reallocation <- reallocation %>% filter(geography == "WLD")
world_key_summary <- bind_rows(
  world_reallocation %>%
    select(added_protected_area_mha, total_reallocated_mha) %>%
    pivot_longer(everything(), names_to = "metric", values_to = "value_mha") %>%
    mutate(
      panel = "Protection and reallocation",
      metric = recode(
        metric,
        added_protected_area_mha = "Added protected footprint",
        total_reallocated_mha = "Net inter-class reallocation"
      )
    ),
  world_movement %>%
    filter(land_class %in% c("Food cropland", "2G bioenergy cropland",
                             "Afforestation")) %>%
    transmute(
      panel = "Land-use responses", metric = as.character(land_class),
      value_mha = protection_under_mitigation_mha
    )
) %>%
  mutate(metric = factor(metric, levels = rev(unique(metric))))
world_key_plot <- ggplot(world_key_summary,
                         aes(value_mha, metric, fill = value_mha > 0)) +
  geom_vline(xintercept = 0, colour = "grey45", linewidth = 0.3) +
  geom_col(width = 0.66) +
  facet_wrap(vars(panel), scales = "free", ncol = 1) +
  scale_fill_manual(values = c("#b2182b", "#2166ac"), guide = "none") +
  labs(x = "Area (million ha)", y = NULL) + theme_paper() +
  theme(panel.grid.major.y = element_blank())
ggsave(file.path(movement_root, "world",
                 paste0("world_protection_reallocation_summary_", target_year, ".png")),
       world_key_plot, width = 8.8, height = 6.8, dpi = 300, bg = "white")

world_rate_summary <- bind_rows(
  world_reallocation %>%
    select(added_protected_share_of_land_pp, reallocated_share_of_land_pct) %>%
    pivot_longer(everything(), names_to = "metric", values_to = "percent") %>%
    mutate(
      denominator = "Reported land total",
      metric = recode(
        metric,
        added_protected_share_of_land_pp = "Added protected footprint",
        reallocated_share_of_land_pct = "Net inter-class reallocation"
      )
    ),
  world_reallocation %>%
    transmute(
      denominator = "Added protected footprint",
      metric = "Net inter-class reallocation",
      percent = reallocated_pct_of_added_protection
    ),
  world_movement %>%
    filter(land_class %in% c("Food cropland", "2G bioenergy cropland",
                             "Afforestation")) %>%
    transmute(
      denominator = "Each class mitigation baseline",
      metric = as.character(land_class),
      percent = protection_under_mitigation_pct
    )
)
world_rate_plot <- ggplot(world_rate_summary,
                          aes(percent, reorder(metric, percent),
                              fill = percent > 0)) +
  geom_vline(xintercept = 0, colour = "grey45", linewidth = 0.3) +
  geom_col(width = 0.66) +
  facet_wrap(vars(denominator), scales = "free", ncol = 1) +
  scale_fill_manual(values = c("#b2182b", "#2166ac"), guide = "none") +
  labs(x = "Percent (%)", y = NULL) + theme_paper() +
  theme(panel.grid.major.y = element_blank())
ggsave(file.path(movement_root, "world",
                 paste0("world_protection_reallocation_rates_", target_year, ".png")),
       world_rate_plot, width = 8.8, height = 8.2, dpi = 300, bg = "white")
write_csv(world_key_summary,
          file.path(movement_root, "world",
                    paste0("world_protection_reallocation_summary_", target_year, ".csv")))
write_csv(world_rate_summary,
          file.path(movement_root, "world",
                    paste0("world_protection_reallocation_rates_", target_year, ".csv")))

trajectory <- land_wide %>%
  filter(geography == "WLD") %>%
  transmute(
    state, year, across(all_of(exclusive_classes)),
    Afforestation = Lan_Cov_Frs_Aff,
    `Integrated protected footprint` = Lan_Cov_Pro_Are_Prm_Sec_Frs_Gra_Int
  ) %>%
  pivot_longer(-c(state, year), names_to = "land_class", values_to = "area") %>%
  pivot_wider(names_from = state, values_from = area) %>%
  mutate(
    without_mha = `100` - `000`, under_mha = `111` - `011`,
    interaction_mha = under_mha - without_mha
  ) %>%
  select(year, land_class, without_mha, under_mha, interaction_mha) %>%
  pivot_longer(c(without_mha, under_mha, interaction_mha),
               names_to = "comparison", values_to = "change_mha") %>%
  mutate(comparison = factor(
    recode(comparison, without_mha = "Without mitigation",
           under_mha = "Under mitigation", interaction_mha = "Difference-in-differences"),
    levels = c("Without mitigation", "Under mitigation", "Difference-in-differences")
  ))
trajectory_plot <- ggplot(trajectory,
                          aes(year, change_mha, colour = comparison,
                              linetype = comparison)) +
  geom_hline(yintercept = 0, colour = "grey70", linewidth = 0.25) +
  geom_line(linewidth = 0.75) +
  facet_wrap(vars(land_class), scales = "free_y", ncol = 3) +
  scale_colour_manual(values = c("#807dba", "#238b45", "#e6550d"), name = NULL) +
  scale_linetype_manual(values = c("dashed", "solid", "dotdash"), name = NULL) +
  labs(x = NULL, y = "Protection-induced area change (million ha)") + theme_paper()
ggsave(file.path(movement_root, "world", "world_land_displacement_trajectories.png"),
       trajectory_plot, width = 12, height = 9, dpi = 300, bg = "white")
write_csv(trajectory,
          file.path(movement_root, "world", "world_land_displacement_trajectories.csv"))

# R10 changes ---------------------------------------------------------------

dir.create(file.path(movement_root, "r10"), recursive = TRUE, showWarnings = FALSE)
r10_move <- movement_long %>%
  filter(geography %in% names(r10_labels), is_exclusive) %>%
  mutate(
    region = factor(unname(r10_labels[geography]), levels = rev(unname(r10_labels))),
    land_class = factor(land_class, levels = exclusive_classes)
  )
r10_move_plot <- ggplot(r10_move,
                        aes(region, protection_under_mitigation_mha,
                            fill = protection_under_mitigation_mha > 0)) +
  geom_hline(yintercept = 0, colour = "grey45", linewidth = 0.25) +
  geom_col(width = 0.7) + coord_flip() +
  facet_wrap(vars(land_class), scales = "free_x", ncol = 3) +
  scale_fill_manual(values = c("#b2182b", "#2166ac"), guide = "none") +
  labs(x = NULL, y = "Protection-induced area change (million ha)") + theme_paper()
ggsave(file.path(movement_root, "r10",
                 paste0("r10_land_displacement_", target_year, ".png")),
       r10_move_plot, width = 12, height = 9, dpi = 300, bg = "white")
write_csv(r10_move,
          file.path(movement_root, "r10", paste0("r10_land_displacement_", target_year, ".csv")))

r10_reallocation <- reallocation %>%
  filter(geography %in% names(r10_labels)) %>%
  mutate(region = factor(unname(r10_labels[geography]), levels = rev(unname(r10_labels))))
r10_reallocation_plot <- r10_reallocation %>%
  select(region, added_protected_share_of_land_pp, reallocated_share_of_land_pct) %>%
  pivot_longer(-region, names_to = "metric", values_to = "percent") %>%
  mutate(metric = recode(metric,
                         added_protected_share_of_land_pp = "Added protected footprint",
                         reallocated_share_of_land_pct = "Net inter-class reallocation")) %>%
  ggplot(aes(region, percent, fill = metric)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.68) + coord_flip() +
  scale_fill_manual(values = c("#756bb1", "#e6550d"), name = NULL) +
  labs(x = NULL, y = "Share of reported land total (%)") + theme_paper()
ggsave(file.path(movement_root, "r10",
                 paste0("r10_protection_reallocation_rates_", target_year, ".png")),
       r10_reallocation_plot, width = 9, height = 6.2, dpi = 300, bg = "white")

# Country maps and top-country bars -----------------------------------------

country_dir <- file.path(movement_root, "country")
dir.create(country_dir, recursive = TRUE, showWarnings = FALSE)
world_map <- ne_countries(scale = "small", returnclass = "sf") %>%
  select(geography = iso_a3)
country_move <- movement_long %>% filter(geography_level == "country")
for (class_name in movement_channels) {
  plot_data <- country_move %>% filter(land_class == class_name)
  if (nrow(plot_data) == 0L) next
  class_stub <- tolower(gsub("[^A-Za-z0-9]+", "_", class_name))
  plot_dir <- file.path(country_dir, class_stub)
  dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)

  for (metric in c("protection_under_mitigation_mha",
                   "protection_under_mitigation_pct")) {
    map_values <- plot_data %>% filter(is.finite(.data[[metric]]))
    mapped <- world_map %>% inner_join(map_values, by = "geography")
    limit <- symmetric_limit(map_values[[metric]])
    legend <- if (metric == "protection_under_mitigation_mha")
      "million ha" else "% of baseline"
    p <- ggplot() +
      geom_sf(data = world_map, fill = "grey92", colour = "grey65", linewidth = 0.1) +
      geom_sf(data = mapped, aes(fill = .data[[metric]]),
              colour = "grey65", linewidth = 0.1) +
      scale_fill_gradient2(low = "#8c2d04", mid = "#f7f7f7", high = "#0868ac",
                           midpoint = 0, limits = c(-limit, limit), oob = squish,
                           name = legend) +
      coord_sf(expand = FALSE) + theme_void(base_size = 10) +
      theme(legend.position = "bottom")
    ggsave(file.path(plot_dir, paste0(metric, "_map_", target_year, ".png")),
           p, width = 10.5, height = 5.8, dpi = 300, bg = "white")
  }

  top <- plot_data %>%
    filter(is.finite(protection_under_mitigation_pct)) %>%
    slice_max(abs(protection_under_mitigation_pct), n = 15, with_ties = FALSE) %>%
    arrange(protection_under_mitigation_pct) %>%
    mutate(geography = factor(geography, levels = geography))
  p <- ggplot(top, aes(geography, protection_under_mitigation_pct,
                       fill = protection_under_mitigation_pct > 0)) +
    geom_hline(yintercept = 0, colour = "grey45", linewidth = 0.3) +
    geom_col(width = 0.7) + coord_flip() +
    scale_fill_manual(values = c("#b2182b", "#2166ac"), guide = "none") +
    labs(x = NULL, y = "Change relative to mitigation baseline (%)") +
    theme_paper() + theme(panel.grid.major.y = element_blank())
  ggsave(file.path(plot_dir, paste0("top_country_relative_change_", target_year, ".png")),
         p, width = 8.5, height = 6.4, dpi = 300, bg = "white")
  write_csv(plot_data, file.path(plot_dir, paste0("country_values_", target_year, ".csv")))
}

country_reallocation <- reallocation %>% filter(geography_level == "country")
country_reallocation_map <- country_reallocation %>%
  select(geography, added_protected_share_of_land_pp,
         reallocated_share_of_land_pct, reallocated_pct_of_added_protection) %>%
  pivot_longer(-geography, names_to = "metric", values_to = "percent") %>%
  mutate(metric = factor(recode(
    metric,
    added_protected_share_of_land_pp = "Added protected footprint / land",
    reallocated_share_of_land_pct = "Net inter-class reallocation / land",
    reallocated_pct_of_added_protection = "Reallocation / added protection"
  )))
mapped_reallocation <- world_map %>% inner_join(country_reallocation_map, by = "geography")
limit <- quantile(abs(country_reallocation_map$percent[
  is.finite(country_reallocation_map$percent)
]), 0.98, na.rm = TRUE)
p <- ggplot() +
  geom_sf(data = world_map, fill = "grey92", colour = "grey65", linewidth = 0.1) +
  geom_sf(data = mapped_reallocation, aes(fill = percent),
          colour = "grey65", linewidth = 0.1) +
  facet_wrap(vars(metric), ncol = 1) +
  scale_fill_gradient2(low = "#8c2d04", mid = "#f7f7f7", high = "#0868ac",
                       midpoint = 0, limits = c(-limit, limit),
                       oob = squish, name = "%") +
  coord_sf(expand = FALSE) + theme_void(base_size = 10) +
  theme(legend.position = "bottom", strip.text = element_text(face = "bold"))
ggsave(file.path(country_dir, paste0("country_protection_reallocation_maps_", target_year, ".png")),
       p, width = 10, height = 9, dpi = 300, bg = "white")
write_csv(country_reallocation,
          file.path(country_dir, paste0("country_protection_reallocation_", target_year, ".csv")))

# Protected-area footprint and overlap-like composition ---------------------

protected_components <- c(
  "Primary forest" = "Lan_Cov_Pro_Are_Prm_Frs",
  "Secondary forest" = "Lan_Cov_Pro_Are_Sec_Frs",
  "Primary grassland" = "Lan_Cov_Pro_Are_Prm_Gra",
  "Secondary grassland" = "Lan_Cov_Pro_Are_Sec_Gra",
  "Cropland" = "Lan_Cov_Pro_Are_Cro",
  "Managed pasture" = "Lan_Cov_Pro_Are_Pas"
)
overlap_long <- land_wide %>%
  filter(year == target_year, state %in% c("011", "111")) %>%
  select(geography, geography_level, state, year, total_land,
         all_of(unname(protected_components)),
         Lan_Cov_Pro_Are_Prm_Sec_Frs_Gra,
         Lan_Cov_Pro_Are_Prm_Sec_Frs_Gra_Int,
         Lan_Cov_Frs, Lan_Cov_Oth_Nat_Lan,
         Lan_Cov_Cro, Lan_Cov_Pst) %>%
  pivot_longer(all_of(unname(protected_components)),
               names_to = "variable", values_to = "protected_area_mha") %>%
  mutate(
    component = names(protected_components)[match(variable, protected_components)],
    component = factor(component, levels = names(protected_components)),
    reference_area_mha = case_when(
      component %in% c("Primary forest", "Secondary forest") ~ Lan_Cov_Frs,
      component %in% c("Primary grassland", "Secondary grassland") ~
        Lan_Cov_Oth_Nat_Lan,
      component == "Cropland" ~ Lan_Cov_Cro,
      component == "Managed pasture" ~ Lan_Cov_Pst,
      TRUE ~ NA_real_
    ),
    coverage_pct_of_reference = if_else(
      reference_area_mha > 1e-10,
      100 * protected_area_mha / reference_area_mha, NA_real_
    ),
    component_share_pct = if_else(
      abs(Lan_Cov_Pro_Are_Prm_Sec_Frs_Gra) > 1e-10,
      100 * protected_area_mha / Lan_Cov_Pro_Are_Prm_Sec_Frs_Gra, NA_real_
    ),
    negative_area_flag = protected_area_mha < -1e-8
  )

write_csv(protected_footprint,
          file.path(footprint_root, "integrated_protected_footprint_all_levels.csv"))
write_csv(overlap_long,
          file.path(footprint_root, "protected_land_composition_all_levels.csv"))

dir.create(file.path(footprint_root, "world"), recursive = TRUE, showWarnings = FALSE)
world_overlap <- overlap_long %>%
  filter(geography == "WLD") %>%
  mutate(scenario = factor(scenario_labels[state],
                           levels = scenario_labels[c("011", "111")]))
world_overlap_plot <- ggplot(world_overlap,
                             aes(scenario, protected_area_mha, fill = component)) +
  geom_col(width = 0.65) +
  scale_fill_brewer(palette = "Dark2", name = NULL) +
  labs(x = NULL, y = "Protected-area composition output (million ha)") +
  theme_paper()
ggsave(file.path(footprint_root, "world",
                 paste0("world_protected_land_composition_", target_year, ".png")),
       world_overlap_plot, width = 8.5, height = 5.8, dpi = 300, bg = "white")

world_coverage_plot <- world_overlap %>%
  filter(is.finite(coverage_pct_of_reference)) %>%
  ggplot(aes(component, coverage_pct_of_reference, fill = scenario)) +
  geom_hline(yintercept = 0, colour = "grey45", linewidth = 0.25) +
  geom_col(position = position_dodge(width = 0.74), width = 0.66) + coord_flip() +
  scale_fill_manual(values = c("#31a354", "#756bb1"), name = NULL) +
  labs(x = NULL, y = "Protected output / corresponding land class (%)") +
  theme_paper()
ggsave(file.path(footprint_root, "world",
                 paste0("world_protected_land_coverage_", target_year, ".png")),
       world_coverage_plot, width = 8.5, height = 5.8, dpi = 300, bg = "white")
write_csv(world_overlap,
          file.path(footprint_root, "world", paste0("world_protected_overlap_", target_year, ".csv")))

dir.create(file.path(footprint_root, "r10"), recursive = TRUE, showWarnings = FALSE)
r10_overlap <- overlap_long %>%
  filter(geography %in% names(r10_labels), state == "111") %>%
  mutate(region = factor(unname(r10_labels[geography]), levels = rev(unname(r10_labels))))
r10_overlap_plot <- ggplot(r10_overlap,
                           aes(region, protected_area_mha, fill = component)) +
  geom_col(width = 0.7) + coord_flip() +
  scale_fill_brewer(palette = "Dark2", name = NULL) +
  labs(x = NULL, y = "Protected-area composition output (million ha)") + theme_paper()
ggsave(file.path(footprint_root, "r10",
                 paste0("r10_protected_land_composition_", target_year, ".png")),
       r10_overlap_plot, width = 9.5, height = 6.5, dpi = 300, bg = "white")
write_csv(r10_overlap,
          file.path(footprint_root, "r10", paste0("r10_protected_overlap_", target_year, ".csv")))

country_coverage <- overlap_long %>%
  filter(geography_level == "country", state == "111") %>%
  group_by(geography) %>%
  summarise(
    `Forest protection / forest` = 100 *
      sum(protected_area_mha[component %in% c("Primary forest", "Secondary forest")]) /
      first(Lan_Cov_Frs),
    `Grassland protection / other natural land` = 100 *
      sum(protected_area_mha[component %in% c("Primary grassland", "Secondary grassland")]) /
      first(Lan_Cov_Oth_Nat_Lan),
    `Cropland protection / cropland` = 100 *
      sum(protected_area_mha[component == "Cropland"]) /
      first(Lan_Cov_Cro),
    `Pasture protection / pasture` = 100 *
      sum(protected_area_mha[component == "Managed pasture"]) /
      first(Lan_Cov_Pst),
    .groups = "drop"
  ) %>%
  pivot_longer(-geography, names_to = "metric", values_to = "coverage_pct") %>%
  filter(is.finite(coverage_pct))
mapped_coverage <- world_map %>% inner_join(country_coverage, by = "geography")
limit <- quantile(abs(country_coverage$coverage_pct), 0.98, na.rm = TRUE)
p <- ggplot() +
  geom_sf(data = world_map, fill = "grey92", colour = "grey65", linewidth = 0.1) +
  geom_sf(data = mapped_coverage, aes(fill = coverage_pct),
          colour = "grey65", linewidth = 0.1) +
  facet_wrap(vars(metric), ncol = 2) +
  scale_fill_gradient2(low = "#8c2d04", mid = "#f7f7f7", high = "#0868ac",
                       midpoint = 0, limits = c(-limit, limit), oob = squish,
                       name = "%") +
  coord_sf(expand = FALSE) + theme_void(base_size = 10) +
  theme(legend.position = "bottom", strip.text = element_text(face = "bold"))
dir.create(file.path(footprint_root, "country"), recursive = TRUE, showWarnings = FALSE)
ggsave(file.path(footprint_root, "country",
                 paste0("country_protected_land_coverage_maps_", target_year, ".png")),
       p, width = 11, height = 7.5, dpi = 300, bg = "white")
write_csv(country_coverage,
          file.path(footprint_root, "country",
                    paste0("country_protected_land_coverage_", target_year, ".csv")))

# Grid-level gross gain, loss, net change and within-class relocation --------

grid_years <- c(2005, 2010, seq(2020, 2100, 10))
year_position <- match(target_year, grid_years)
grid_classes <- c(
  `Food cropland` = 1L,
  `2G bioenergy cropland` = 2L,
  `Planted forest` = 5L
)

# TEMPORARY WORKAROUND: existing AIM-LUmap files double-count Rbasin and R17.
# Remove this factor after regenerating them with the corrected combine.gms.
grid_export_duplicate_factor <- 2
read_grid_classes <- function(state) {
  path <- file.path(data_dir, "PLUM", "nc",
                    paste0("AIM-LUmap_", states[[state]], ".nc"))
  all_layers <- rast(path, subds = "LC_area_share")
  positions <- (unname(grid_classes) - 1L) * length(grid_years) + year_position
  selected <- all_layers[[positions]] / grid_export_duplicate_factor
  names(selected) <- names(grid_classes)
  selected
}
grid_011 <- read_grid_classes("011")
grid_111 <- read_grid_classes("111")
pixel_area <- rast(
  file.path(data_dir, "PLUM", "nc", paste0("AIM-LUmap_", states[["011"]], ".nc")),
  subds = "pixel_area"
)
grid_delta <- grid_111 - grid_011
grid_relocation <- lapply(names(grid_classes), function(class_name) {
  delta <- grid_delta[[class_name]]
  gain <- global(ifel(delta > 0, delta, 0) * pixel_area, "sum", na.rm = TRUE)[[1]]
  loss <- global(ifel(delta < 0, -delta, 0) * pixel_area, "sum", na.rm = TRUE)[[1]]
  tibble(
    land_class = class_name,
    area_gained_mha = gain,
    area_lost_mha = loss,
    net_change_mha = gain - loss,
    spatially_relocated_mha = min(gain, loss),
    net_expansion_mha = max(gain - loss, 0),
    net_contraction_mha = max(loss - gain, 0)
  )
}) %>% bind_rows()
write_csv(grid_relocation,
          file.path(grid_root, paste0("world_grid_relocation_", target_year, ".csv")))

# The NetCDF classes and MergedComparison variables are related diagnostics,
# but are not assumed to be numerically interchangeable. Keep the comparison
# explicit so that any discrepancy remains visible.
grid_merged_consistency <- tibble(
  land_class = c("Food cropland", "2G bioenergy cropland", "Planted forest"),
  merged_land_class = c("Food cropland", "2G bioenergy cropland", "Afforestation"),
  exactly_comparable = FALSE,
  interpretation = c(
    "Related cropland measures; NetCDF export and anomalous cell shares can change the aggregate.",
    "Related 2G bioenergy measures; NetCDF export and anomalous cell shares can change the aggregate.",
    "Planted forest in the NetCDF is not defined as identical to the MergedComparison afforestation diagnostic."
  )
) %>%
  left_join(
    grid_relocation %>% select(land_class, grid_net_change_mha = net_change_mha),
    by = "land_class"
  ) %>%
  left_join(
    movement_long %>%
      filter(geography == "WLD") %>%
      transmute(merged_land_class = as.character(land_class),
                merged_net_change_mha = protection_under_mitigation_mha),
    by = "merged_land_class"
  ) %>%
  mutate(grid_minus_merged_mha = grid_net_change_mha - merged_net_change_mha)
write_csv(
  grid_merged_consistency,
  file.path(grid_root, paste0("world_grid_vs_merged_diagnostic_", target_year, ".csv"))
)

grid_plot_data <- grid_relocation %>%
  select(land_class, area_gained_mha, area_lost_mha) %>%
  mutate(area_lost_mha = -area_lost_mha) %>%
  pivot_longer(-land_class, names_to = "direction", values_to = "area_mha") %>%
  mutate(direction = recode(direction,
                            area_gained_mha = "Area gained",
                            area_lost_mha = "Area lost"))
grid_plot <- ggplot(grid_plot_data, aes(land_class, area_mha, fill = direction)) +
  geom_hline(yintercept = 0, colour = "grey45", linewidth = 0.3) +
  geom_col(width = 0.64) +
  geom_point(data = grid_relocation,
             aes(land_class, net_change_mha), inherit.aes = FALSE,
             shape = 18, size = 2.5) +
  coord_flip() +
  scale_fill_manual(values = c("Area gained" = "#2166ac", "Area lost" = "#b2182b"),
                    name = NULL) +
  labs(x = NULL, y = "Grid-level area change (million ha)") +
  theme_paper() + theme(panel.grid.major.y = element_blank())
ggsave(file.path(grid_root, paste0("world_grid_gain_loss_", target_year, ".png")),
       grid_plot, width = 8.5, height = 4.8, dpi = 300, bg = "white")

rm(raw_land, land_wide, grid_011, grid_111, grid_delta)
gc()
