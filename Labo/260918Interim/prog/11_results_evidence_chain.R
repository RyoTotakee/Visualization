suppressPackageStartupMessages({
  library(dplyr)
  library(gdxrrw)
  library(ggplot2)
  library(ggrepel)
  library(patchwork)
  library(readr)
  library(scales)
  library(tidyr)
})

source("prog/common.R")
init_gdx()

analysis_root <- file.path(output_dir, "11_results_evidence_chain")
unlink(analysis_root, recursive = TRUE)
global_root <- file.path(analysis_root, "01_global_land_system")
mechanism_root <- file.path(analysis_root, "02_productive_land_displacement")
interaction_root <- file.path(analysis_root, "03_policy_interaction")
country_root <- file.path(analysis_root, "04_country_transmission")
outcome_root <- file.path(analysis_root, "05_climate_biodiversity_outcomes")
for (path in c(global_root, mechanism_root, interaction_root, country_root, outcome_root)) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
}

story_theme <- theme_paper() +
  theme(
    plot.title = element_text(face = "bold", size = 14, margin = margin(b = 6)),
    plot.subtitle = element_text(size = 10.5, colour = "grey30", margin = margin(b = 9)),
    plot.caption = element_text(size = 8.3, colour = "grey35", hjust = 0),
    strip.text = element_text(face = "bold"), legend.position = "bottom"
  )

states <- scenario_states("global2")
state_labels <- c(
  "000" = "No policy", "100" = "Protection only",
  "010" = "Afforestation only", "001" = "BECCS only",
  "011" = "Both CDR", "111" = "Protection + both CDR"
)
comparison_table <- tribble(
  ~comparison, ~from, ~to, ~comparison_label,
  "protection_no_cdr", "000", "100", "Protection\nwithout CDR",
  "afforestation", "000", "010", "Afforestation",
  "beccs", "000", "001", "BECCS",
  "both_cdr", "000", "011", "Both CDR",
  "protection_with_cdr", "011", "111", "Protection\nwith both CDR",
  "all_policies", "000", "111", "All policies"
)

# 1. Global land system -----------------------------------------------------

inventory_file <- file.path(
  output_dir, "08_land_use_protection", "01_land_inventory",
  "land_inventory_all_levels.csv"
)
world_inventory <- read_csv(inventory_file, show_col_types = FALSE) %>%
  filter(geography_level == "world", year == target_year) %>%
  mutate(
    scenario = factor(state_labels[state], levels = state_labels[c("000", "100", "011", "111")]),
    land_class = factor(land_class, levels = c(
      "Food cropland", "2G bioenergy cropland", "Fallow cropland",
      "Pasture", "Forest", "Other natural land", "Built-up area"
    ))
  )
write_csv(world_inventory,
          file.path(global_root, paste0("world_exclusive_land_composition_", target_year, ".csv")))

p <- ggplot(world_inventory, aes(scenario, area_million_ha, fill = land_class)) +
  geom_col(width = 0.72) +
  scale_fill_manual(values = c(
    "Food cropland" = "#e6ab02", "2G bioenergy cropland" = "#d95f02",
    "Fallow cropland" = "#ffd92f", "Pasture" = "#a6d854",
    "Forest" = "#1b9e77", "Other natural land" = "#66c2a5",
    "Built-up area" = "#7570b3"
  ), name = NULL) +
  scale_y_continuous(labels = label_number(big.mark = ","), expand = expansion(mult = c(0, .04))) +
  labs(
    title = "Protection and land-based CDR alter the global land allocation",
    subtitle = "Exclusive land classes close to the reported land total; afforestation is contained within forest",
    x = NULL, y = "Area (million ha)",
    caption = "MergedComparison, 2100. The four columns isolate no policy, protection, both CDR options, and their joint implementation."
  ) + story_theme +
  theme(axis.text.x = element_text(angle = 18, hjust = 1), panel.grid.major.x = element_blank())
ggsave(file.path(global_root, paste0("world_exclusive_land_composition_", target_year, ".png")),
       p, width = 11, height = 6.7, dpi = 300, bg = "white")

merged_file <- file.path(data_dir, "PLUM", "Mergedcomparison.gdx")
land_variables <- c(
  Lan_Cov_Cro_Non_Ene_Cro = "Food cropland",
  Lan_Cov_Cro_Ene_Cro_2nd_gen = "2G bioenergy cropland",
  Lan_Cov_Frs_Aff = "Afforestation",
  Lan_Cov_Pst = "Pasture"
)
merged_iamc <- rgdx.param(merged_file, "IAMCTempwoU")
world_land_states <- merged_iamc %>%
  filter(
    i1 %in% states[names(state_labels)], i2 %in% c("WLD", "World"),
    i3 %in% names(land_variables), as.integer(as.character(i4)) == target_year
  ) %>%
  transmute(
    state = state_from_scenario(i1, "global2"),
    land_class = unname(land_variables[as.character(i3)]), value
  ) %>%
  group_by(state, land_class) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  complete(state = names(state_labels), land_class = unname(land_variables), fill = list(value = 0)) %>%
  group_by(land_class) %>%
  mutate(
    change_from_no_policy_mha = value - value[state == "000"],
    scenario = factor(state_labels[state], levels = state_labels)
  ) %>% ungroup()
write_csv(world_land_states,
          file.path(global_root, paste0("world_target_land_uses_by_scenario_", target_year, ".csv")))

p <- ggplot(world_land_states, aes(scenario, value, group = land_class)) +
  geom_point(aes(colour = scenario), size = 3) +
  facet_wrap(vars(land_class), scales = "free_y", ncol = 2) +
  scale_colour_manual(values = c(
    "No policy" = "#636363", "Protection only" = "#756bb1",
    "Afforestation only" = "#31a354", "BECCS only" = "#e6550d",
    "Both CDR" = "#3182bd", "Protection + both CDR" = "#111111"
  ), guide = "none") +
  labs(
    title = "Afforestation and BECCS respond differently to the same conservation constraint",
    subtitle = "NoAff and NoBio scenarios separate the two land-based CDR options",
    x = NULL, y = "Area (million ha)",
    caption = "Points are scenario levels, so differences between adjacent categories are not additive unless their starting state is the same."
  ) + story_theme +
  theme(axis.text.x = element_text(angle = 27, hjust = 1))
ggsave(file.path(global_root, paste0("world_target_land_uses_by_scenario_", target_year, ".png")),
       p, width = 12, height = 8.2, dpi = 300, bg = "white")

# 2. Productive-land displacement -----------------------------------------

alpha_file <- file.path(data_dir, "ALPHA", "all_scenario_World.gdx")
crop_area <- rgdx.param(alpha_file, "qlBas_load") %>%
  filter(
    SCENARIO %in% states, as.integer(as.character(Y)) == target_year,
    c %in% selected_crop_codes
  ) %>%
  transmute(
    state = state_from_scenario(SCENARIO), crop = as.character(c),
    basin = as.character(bas), country = as.character(cty), area_1000ha = qlBas_load
  )
crop_yield <- rgdx.param(alpha_file, "yldBas_load") %>%
  filter(
    SCENARIO %in% states, as.integer(as.character(Y)) == target_year,
    c %in% selected_crop_codes
  ) %>%
  transmute(
    state = state_from_scenario(SCENARIO), crop = as.character(c),
    basin = as.character(bas), country = as.character(cty), yield_t_ha = yldBas_load
  )
crop_state_wide <- inner_join(
  crop_area, crop_yield, by = c("state", "crop", "basin", "country")
) %>%
  pivot_wider(
    names_from = state, values_from = c(area_1000ha, yield_t_ha),
    names_glue = "{.value}_{state}"
  )

decomposition_list <- list()
source_sink_list <- list()
basin_comparison_list <- list()
for (i in seq_len(nrow(comparison_table))) {
  cmp <- comparison_table[i, ]
  from <- cmp$from[[1]]
  to <- cmp$to[[1]]
  work <- crop_state_wide %>%
    transmute(
      crop, basin, country,
      area_from = .data[[paste0("area_1000ha_", from)]],
      area_to = .data[[paste0("area_1000ha_", to)]],
      yield_from = .data[[paste0("yield_t_ha_", from)]],
      yield_to = .data[[paste0("yield_t_ha_", to)]],
      area_change = area_to - area_from,
      production_from = area_from * yield_from,
      production_to = area_to * yield_to,
      comparison = cmp$comparison[[1]], comparison_label = cmp$comparison_label[[1]]
    ) %>%
    group_by(crop) %>%
    mutate(
      reference_yield = sum(production_from) / sum(area_from),
      normalized_source_yield = yield_from / reference_yield,
      yield_percentile = percent_rank(yield_from)
    ) %>% ungroup()
  basin_comparison_list[[i]] <- work

  decomposition_list[[i]] <- work %>%
    group_by(crop, comparison, comparison_label) %>%
    summarise(
      baseline_production_1000t = sum(production_from),
      area_quantity_1000t = sum(area_change) * first(reference_yield),
      spatial_allocation_1000t = sum(area_change * (yield_from - first(reference_yield))),
      yield_response_1000t = sum(area_to * (yield_to - yield_from)),
      total_change_1000t = sum(production_to - production_from),
      closure_1000t = total_change_1000t - area_quantity_1000t -
        spatial_allocation_1000t - yield_response_1000t,
      .groups = "drop"
    )

  source_sink_list[[i]] <- work %>%
    group_by(crop, comparison, comparison_label) %>%
    summarise(
      area_lost_1000ha = sum(pmax(-area_change, 0)),
      area_gained_1000ha = sum(pmax(area_change, 0)),
      source_yield_t_ha = weighted.mean(yield_from, pmax(-area_change, 0), na.rm = TRUE),
      sink_yield_t_ha = weighted.mean(yield_from, pmax(area_change, 0), na.rm = TRUE),
      lost_baseline_potential_1000t = sum(pmax(-area_change, 0) * yield_from),
      gained_baseline_potential_1000t = sum(pmax(area_change, 0) * yield_from),
      .groups = "drop"
    ) %>%
    mutate(sink_minus_source_pct = 100 * (sink_yield_t_ha / source_yield_t_ha - 1))
}
basin_comparisons <- bind_rows(basin_comparison_list)
production_decomposition <- bind_rows(decomposition_list) %>%
  mutate(
    crop_label = unname(commodity_labels[crop]),
    across(ends_with("_1000t") & !starts_with("baseline"),
           ~ 100 * .x / baseline_production_1000t, .names = "{sub('_1000t$', '_pct', .col)}")
  )
source_sink <- bind_rows(source_sink_list) %>%
  mutate(crop_label = unname(commodity_labels[crop]))
write_csv(production_decomposition,
          file.path(mechanism_root, paste0("crop_production_area_spatial_yield_decomposition_", target_year, ".csv")))
write_csv(source_sink,
          file.path(mechanism_root, paste0("crop_source_sink_yield_quality_", target_year, ".csv")))

component_labels <- c(
  area_quantity_pct = "Net area quantity",
  spatial_allocation_pct = "Spatial productivity",
  yield_response_pct = "Yield response"
)
decomp_plot <- production_decomposition %>%
  select(crop_label, comparison_label, all_of(names(component_labels)), total_change_pct) %>%
  pivot_longer(all_of(names(component_labels)), names_to = "component", values_to = "contribution_pct") %>%
  mutate(
    component = factor(component_labels[component], levels = component_labels),
    comparison_label = factor(comparison_label, levels = comparison_table$comparison_label)
  )
p <- ggplot(decomp_plot, aes(comparison_label, contribution_pct, fill = component)) +
  geom_hline(yintercept = 0, colour = "grey45", linewidth = .3) +
  geom_col(width = .7) +
  geom_point(
    data = production_decomposition %>%
      mutate(comparison_label = factor(comparison_label, levels = comparison_table$comparison_label)),
    aes(comparison_label, total_change_pct), inherit.aes = FALSE,
    shape = 18, size = 2.5, colour = "black"
  ) +
  facet_wrap(vars(crop_label), scales = "free_y", ncol = 3) +
  scale_fill_manual(values = c(
    "Net area quantity" = "#756bb1", "Spatial productivity" = "#e6550d",
    "Yield response" = "#31a354"
  ), name = NULL) +
  labs(
    title = "Crop production changes reflect area quantity, location and yield response",
    subtitle = "The orange component tests whether cropland moves toward lower- or higher-yield basins",
    x = NULL, y = "Contribution to production change (% of comparison baseline)",
    caption = "Black diamonds are exact production changes from ALPHA area × yield. Components close to the diamond by construction. Crops are not summed across physical tonnes."
  ) + story_theme +
  theme(axis.text.x = element_text(angle = 30, hjust = 1), panel.grid.major.x = element_blank())
ggsave(file.path(mechanism_root, paste0("crop_production_mechanism_decomposition_", target_year, ".png")),
       p, width = 14, height = 8.5, dpi = 300, bg = "white")

quality_plot <- source_sink %>%
  filter(comparison != "all_policies") %>%
  mutate(comparison_label = factor(comparison_label, levels = comparison_table$comparison_label))
p <- ggplot(quality_plot, aes(comparison_label, sink_minus_source_pct, fill = sink_minus_source_pct)) +
  geom_hline(yintercept = 0, colour = "grey35", linewidth = .35) +
  geom_col(width = .7) +
  facet_wrap(vars(crop_label), scales = "free_y", ncol = 3) +
  scale_fill_gradient2(low = "#b2182b", mid = "white", high = "#2166ac", midpoint = 0,
                       name = "Sink − source\nyield (%)") +
  labs(
    title = "Do displaced crops move to lower-productivity basins?",
    subtitle = "Negative values mean that gaining basins have lower baseline yields than losing basins",
    x = NULL, y = "Difference in area-change-weighted baseline yield (%)",
    caption = "Source and sink yields use each comparison's starting-state yield. This diagnoses location quality, not realized production by itself."
  ) + story_theme +
  theme(axis.text.x = element_text(angle = 30, hjust = 1), panel.grid.major.x = element_blank())
ggsave(file.path(mechanism_root, paste0("crop_source_sink_yield_gap_", target_year, ".png")),
       p, width = 14, height = 8.2, dpi = 300, bg = "white")

# 3. Is the protection effect additive? ------------------------------------

world_crp <- read_csv(
  file.path(output_dir, "07_conditional_protection_decomposition", "01_agriculture",
            "world", "aggregate_agmip_items", "crp_crops_non_energy_",
            paste0("world_components_", target_year, ".csv")),
  show_col_types = FALSE
) %>%
  filter(indicator %in% c("Area", "Production", "Producer price", "Food intake", "Imports", "Exports")) %>%
  transmute(
    domain = "AgMIP crops", outcome = as.character(indicator),
    protection_main_pct, protection_x_afforestation_pct,
    protection_x_beccs_pct, protection_x_afforestation_x_beccs_pct,
    total_under_mitigation_pct = protection_under_mitigation_pct
  )

world_land_factorial <- merged_iamc %>%
  filter(
    i1 %in% states, i2 %in% c("WLD", "World"),
    i3 %in% names(land_variables)[1:3], as.integer(as.character(i4)) == target_year
  ) %>%
  transmute(
    state = state_from_scenario(i1),
    land_class = unname(land_variables[as.character(i3)]), value
  ) %>%
  group_by(state, land_class) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  complete(state = names(states), land_class = unname(land_variables)[1:3], fill = list(value = 0)) %>%
  pivot_wider(names_from = state, values_from = value) %>%
  conditional_protection_factorial()
land_interaction <- world_land_factorial %>%
  transmute(
    domain = "AIM-LU land", outcome = land_class,
    protection_main_pct = 100 * protection_main / `011`,
    protection_x_afforestation_pct = 100 * protection_x_afforestation / `011`,
    protection_x_beccs_pct = 100 * protection_x_beccs / `011`,
    protection_x_afforestation_x_beccs_pct = 100 * protection_x_afforestation_x_beccs / `011`,
    total_under_mitigation_pct = 100 * protection_under_mitigation / `011`
  )
interaction_summary <- bind_rows(world_crp, land_interaction) %>%
  mutate(
    combined_interaction_pct = protection_x_afforestation_pct +
      protection_x_beccs_pct + protection_x_afforestation_x_beccs_pct,
    additivity_gap_pct = total_under_mitigation_pct - protection_main_pct,
    interaction_share_of_total_pct = if_else(
      abs(total_under_mitigation_pct) > 1e-8,
      100 * combined_interaction_pct / total_under_mitigation_pct, NA_real_
    )
  )
write_csv(interaction_summary,
          file.path(interaction_root, paste0("world_protection_additivity_diagnostic_", target_year, ".csv")))

p <- ggplot(interaction_summary,
            aes(protection_main_pct, total_under_mitigation_pct, colour = domain)) +
  geom_abline(slope = 1, intercept = 0, linetype = 3, colour = "grey40") +
  geom_hline(yintercept = 0, colour = "grey70", linewidth = .25) +
  geom_vline(xintercept = 0, colour = "grey70", linewidth = .25) +
  geom_point(size = 3) +
  geom_text_repel(aes(label = outcome), size = 3, max.overlaps = Inf,
                  min.segment.length = 0) +
  facet_wrap(vars(domain), scales = "free", ncol = 2) +
  scale_colour_manual(values = c("AgMIP crops" = "#3182bd", "AIM-LU land" = "#e6550d"), name = NULL) +
  labs(
    title = "How far does the protection effect depart from simple additivity?",
    subtitle = "Distance from the diagonal is the combined interaction with afforestation and BECCS",
    x = "Protection main effect, 100−000 (% of the 011 baseline)",
    y = "Protection effect with both CDR, 111−011 (% of the 011 baseline)",
    caption = "CRP excludes energy crops. Near-diagonal outcomes are approximately additive even when large spatial redistribution occurs."
  ) + story_theme
ggsave(file.path(interaction_root, paste0("world_main_vs_context_protection_effect_", target_year, ".png")),
       p, width = 9, height = 7.5, dpi = 300, bg = "white")

interaction_long <- interaction_summary %>%
  select(domain, outcome, starts_with("protection_"), total_under_mitigation_pct) %>%
  select(-protection_main_pct) %>%
  pivot_longer(
    c(protection_x_afforestation_pct, protection_x_beccs_pct,
      protection_x_afforestation_x_beccs_pct),
    names_to = "component", values_to = "interaction_pct"
  ) %>%
  mutate(component = recode(
    component,
    protection_x_afforestation_pct = "Protection × afforestation",
    protection_x_beccs_pct = "Protection × BECCS",
    protection_x_afforestation_x_beccs_pct = "Three-way interaction"
  ))
p <- ggplot(interaction_long, aes(outcome, interaction_pct, fill = component)) +
  geom_hline(yintercept = 0, colour = "grey45", linewidth = .3) +
  geom_col(width = .68) +
  facet_wrap(vars(domain), scales = "free", ncol = 1) +
  coord_flip() +
  scale_fill_manual(values = c(
    "Protection × afforestation" = "#31a354", "Protection × BECCS" = "#e6550d",
    "Three-way interaction" = "#3182bd"
  ), name = NULL) +
  labs(
    title = "Afforestation and BECCS contribute differently to the interaction",
    subtitle = "All components are percentage points of the mitigation baseline, so signs can offset",
    x = NULL, y = "Interaction contribution (% of 011 baseline)",
    caption = "The sum of the three colours equals (111−011)−(100−000). Positive is an increase in the named outcome, not necessarily a benefit."
  ) + story_theme + theme(panel.grid.major.y = element_blank())
ggsave(file.path(interaction_root, paste0("world_afforestation_beccs_interaction_components_", target_year, ".png")),
       p, width = 11, height = 8.5, dpi = 300, bg = "white")

# 4. Country pathway from land pressure to markets -------------------------

# protPlum contains protected shares of primary forest, secondary forest and
# grassland at basin level. Their unweighted mean is a basin pressure proxy;
# it is not a direct protected-area × cropland intersection.
plum_states <- scenario_states("global1")
protection_basin <- rgdx.param(file.path(data_dir, "ALPHA", "plum_data.gdx"), "protPlum") %>%
  filter(
    sce_plum %in% plum_states[c("011", "111")], as.integer(as.character(yr)) == target_year,
    lu_prot %in% c("primf", "secdf", "gra")
  ) %>%
  transmute(
    state = state_from_scenario(sce_plum, "global1"), land_type = as.character(lu_prot),
    basin = as.character(bas), country = as.character(cty), protected_share = protPlum
  ) %>%
  complete(
    nesting(basin, country), state = c("011", "111"),
    land_type = c("primf", "secdf", "gra"), fill = list(protected_share = 0)
  ) %>%
  group_by(state, basin, country) %>%
  summarise(protected_share = mean(protected_share), .groups = "drop") %>%
  pivot_wider(names_from = state, values_from = protected_share, values_fill = 0) %>%
  transmute(country, basin, basin_protection_increase_pp = 100 * (`111` - `011`))

protection_crop_basin <- basin_comparisons %>%
  filter(comparison == "protection_with_cdr") %>%
  left_join(protection_basin, by = c("country", "basin")) %>%
  mutate(basin_protection_increase_pp = coalesce(basin_protection_increase_pp, 0))

country_land_mechanism <- protection_crop_basin %>%
  group_by(crop) %>%
  mutate(global_crop_reference_yield = weighted.mean(yield_from, area_from)) %>%
  ungroup() %>%
  group_by(country) %>%
  summarise(
    production_weighted_protection_increase_pp = weighted.mean(
      basin_protection_increase_pp, production_from, na.rm = TRUE
    ),
    harvested_area_loss_1000ha = sum(pmax(-area_change, 0)),
    harvested_area_gain_1000ha = sum(pmax(area_change, 0)),
    high_yield_loss_share_pct = 100 * sum(pmax(-area_change, 0) * (yield_percentile >= .8)) /
      sum(pmax(-area_change, 0)),
    source_normalized_yield = weighted.mean(
      yield_from / global_crop_reference_yield, pmax(-area_change, 0), na.rm = TRUE
    ),
    sink_normalized_yield = weighted.mean(
      yield_from / global_crop_reference_yield, pmax(area_change, 0), na.rm = TRUE
    ),
    sink_minus_source_normalized_yield_pct = 100 *
      (sink_normalized_yield / source_normalized_yield - 1),
    fixed_yield_production_change_1000t = sum(area_change * yield_from),
    .groups = "drop"
  ) %>%
  mutate(across(where(is.numeric), ~ ifelse(is.finite(.x), .x, NA_real_)))

country_profile <- read_csv(
  file.path(output_dir, "10_country_opportunity_profiles",
            paste0("country_integrated_profile_", target_year, ".csv")),
  show_col_types = FALSE
)
country_evidence <- country_profile %>%
  left_join(country_land_mechanism, by = "country") %>%
  mutate(
    net_import_adjustment_pct_consumption = if_else(
      consumption_baseline > 1e-10,
      100 * ((imports_total_value - exports_total_value) / consumption_baseline), NA_real_
    )
  )
write_csv(country_evidence,
          file.path(country_root, paste0("country_land_market_evidence_", target_year, ".csv")))

country_driver_long <- country_evidence %>%
  select(
    country, population_million, crop_import_dependence_pct, production_total_pct,
    production_weighted_protection_increase_pp, high_yield_loss_share_pct,
    sink_minus_source_normalized_yield_pct
  ) %>%
  pivot_longer(
    c(production_weighted_protection_increase_pp, high_yield_loss_share_pct,
      sink_minus_source_normalized_yield_pct),
    names_to = "driver", values_to = "driver_value"
  ) %>%
  mutate(driver = recode(
    driver,
    production_weighted_protection_increase_pp = "Protection pressure in crop-producing basins (pp)",
    high_yield_loss_share_pct = "Share of crop-area loss in top-yield basins (%)",
    sink_minus_source_normalized_yield_pct = "Yield quality of gaining minus losing basins (%)"
  )) %>%
  filter(is.finite(driver_value), is.finite(production_total_pct)) %>%
  group_by(driver) %>%
  mutate(
    display = abs(driver_value) <= quantile(abs(driver_value), .975, na.rm = TRUE),
    label = if_else(rank(-abs(production_total_pct), ties.method = "first") <= 7,
                    country, NA_character_)
  ) %>%
  ungroup() %>% filter(display)
p <- ggplot(country_driver_long,
            aes(driver_value, production_total_pct, size = population_million,
                colour = crop_import_dependence_pct)) +
  geom_hline(yintercept = 0, colour = "grey55", linewidth = .3) +
  geom_smooth(
    data = country_driver_long,
    aes(driver_value, production_total_pct, group = 1), inherit.aes = FALSE,
    method = "lm", se = FALSE, colour = "grey30", linewidth = .6
  ) +
  geom_point(alpha = .72) +
  geom_text_repel(aes(label = label), na.rm = TRUE, size = 2.8,
                  max.overlaps = Inf, min.segment.length = 0) +
  facet_wrap(vars(driver), scales = "free_x", ncol = 3) +
  scale_colour_viridis_c(option = "C", trans = "sqrt", name = "Baseline import\ndependence (%)") +
  scale_size_area(max_size = 9, name = "Population\n(million)") +
  labs(
    title = "Which land mechanisms are associated with country production impacts?",
    subtitle = "Three continuous diagnostics connect conservation pressure and land quality to AgMIP crop production",
    x = NULL, y = "Protection-induced CRP production change (% of 011 baseline)",
    caption = "The protection-pressure measure averages protected shares of natural land types within basins; it is not direct cropland overlap. Lines are descriptive fits."
  ) + story_theme
ggsave(file.path(country_root, paste0("country_land_drivers_vs_production_", target_year, ".png")),
       p, width = 14, height = 6.6, dpi = 300, bg = "white")

country_cascade <- country_evidence %>%
  filter(
    is.finite(production_total_pct), is.finite(net_import_adjustment_pct_consumption),
    is.finite(food_intake_total_pct), is.finite(producer_price_total_pct)
  ) %>%
  mutate(label = if_else(
    rank(-(abs(production_total_pct) + abs(food_intake_total_pct)), ties.method = "first") <= 12,
    country, NA_character_
  ))
price_limit <- quantile(abs(country_cascade$producer_price_total_pct), .98, na.rm = TRUE)
p1 <- ggplot(country_cascade,
             aes(production_total_pct, net_import_adjustment_pct_consumption,
                 size = population_million, colour = producer_price_total_pct)) +
  geom_hline(yintercept = 0, colour = "grey60", linewidth = .3) +
  geom_vline(xintercept = 0, colour = "grey60", linewidth = .3) +
  geom_point(alpha = .75) +
  geom_text_repel(aes(label = label), na.rm = TRUE, size = 2.7,
                  max.overlaps = Inf, min.segment.length = 0) +
  scale_colour_gradient2(low = "#2166ac", mid = "white", high = "#b2182b", midpoint = 0,
                         limits = c(-price_limit, price_limit), oob = squish,
                         name = "Producer-price\nchange (%)") +
  scale_size_area(max_size = 9, name = "Population\n(million)") +
  labs(
    title = "A. Production and net-import adjustment",
    x = "CRP production change (%)",
    y = "Net-import change / baseline consumption (percentage points)"
  ) + story_theme
p2 <- ggplot(country_cascade,
             aes(net_import_adjustment_pct_consumption, food_intake_total_pct,
                 size = population_million, colour = production_total_pct)) +
  geom_hline(yintercept = 0, colour = "grey60", linewidth = .3) +
  geom_vline(xintercept = 0, colour = "grey60", linewidth = .3) +
  geom_point(alpha = .75) +
  geom_text_repel(aes(label = label), na.rm = TRUE, size = 2.7,
                  max.overlaps = Inf, min.segment.length = 0) +
  scale_colour_gradient2(low = "#b2182b", mid = "white", high = "#2166ac", midpoint = 0,
                         name = "Production\nchange (%)") +
  scale_size_area(max_size = 9, name = "Population\n(million)") +
  labs(
    title = "B. Net-import adjustment and food intake",
    x = "Net-import change / baseline consumption (percentage points)",
    y = "Food-intake change (%)"
  ) + story_theme
p <- (p1 + p2) + plot_annotation(
  title = "Country impacts pass through production, trade, prices and food intake",
  subtitle = "A production decline need not translate one-for-one into lower domestic food intake",
  caption = "AgMIP CRP, protection under both CDR (111−011). Associations reflect the full market equilibrium and do not isolate trade causally.",
  theme = theme(plot.title = element_text(face = "bold", size = 15),
                plot.subtitle = element_text(size = 10.5),
                plot.caption = element_text(size = 8.5, colour = "grey35"))
)
ggsave(file.path(country_root, paste0("country_production_trade_food_cascade_", target_year, ".png")),
       p, width = 15, height = 7, dpi = 300, bg = "white")

# Multi-indicator screen: actual values are printed; colour is a within-metric z score.
heatmap_metrics <- c(
  production_weighted_protection_increase_pp = "Basin protection\npressure (pp)",
  high_yield_loss_share_pct = "High-yield loss\nshare (%)",
  food_grid_relocated_mha = "Food-land\nrelocation (Mha)",
  area_total_pct = "CRP area\nchange (%)",
  production_total_pct = "CRP production\nchange (%)",
  net_import_adjustment_pct_consumption = "Net-import\nadjustment (pp)",
  producer_price_total_pct = "Producer price\nchange (%)",
  food_intake_total_pct = "Food intake\nchange (%)",
  bii_change_points = "BII change\n(points)"
)
heatmap_base <- country_evidence %>%
  filter(if_any(all_of(names(heatmap_metrics)), is.finite)) %>%
  mutate(selection_score = pmax(
    percent_rank(abs(production_total_pct)), percent_rank(abs(producer_price_total_pct)),
    percent_rank(abs(food_intake_total_pct)), percent_rank(abs(bii_change_points)),
    percent_rank(abs(production_weighted_protection_increase_pp)), na.rm = TRUE
  )) %>%
  slice_max(selection_score, n = 32, with_ties = FALSE)
heatmap_long <- heatmap_base %>%
  select(country, all_of(names(heatmap_metrics))) %>%
  pivot_longer(-country, names_to = "metric", values_to = "value") %>%
  group_by(metric) %>%
  mutate(z = as.numeric(scale(value))) %>%
  ungroup() %>%
  mutate(
    metric = factor(unname(heatmap_metrics[metric]), levels = unname(heatmap_metrics)),
    country = factor(country, levels = rev(heatmap_base$country)),
    value_label = if_else(is.finite(value), number(value, accuracy = .1), "NA")
  )
write_csv(heatmap_long,
          file.path(country_root, paste0("country_multi_indicator_screen_", target_year, ".csv")))
p <- ggplot(heatmap_long, aes(metric, country, fill = z)) +
  geom_tile(colour = "white", linewidth = .25) +
  geom_text(aes(label = value_label), size = 2.35) +
  scale_fill_gradient2(low = "#b2182b", mid = "white", high = "#2166ac", midpoint = 0,
                       limits = c(-2.5, 2.5), oob = squish, na.value = "grey90",
                       name = "Within-metric\nz score") +
  labs(
    title = "Countries with large signals differ across the land-to-market chain",
    subtitle = "Cell text gives the actual value; colour standardizes each column only for visual comparison",
    x = NULL, y = NULL,
    caption = "Countries are selected by a continuous maximum-signal rank for display, not assigned to classes. The full country evidence table contains all countries."
  ) + story_theme +
  theme(
    panel.grid = element_blank(), axis.text.x = element_text(angle = 30, hjust = 1, size = 8),
    axis.text.y = element_text(size = 8)
  )
ggsave(file.path(country_root, paste0("country_multi_indicator_screen_", target_year, ".png")),
       p, width = 14, height = 11, dpi = 300, bg = "white")

# Descriptive correlations for the newly constructed mechanism variables.
association_specs <- tribble(
  ~x, ~y,
  "production_weighted_protection_increase_pp", "production_total_pct",
  "high_yield_loss_share_pct", "production_total_pct",
  "sink_minus_source_normalized_yield_pct", "production_total_pct",
  "production_total_pct", "net_import_adjustment_pct_consumption",
  "net_import_adjustment_pct_consumption", "food_intake_total_pct",
  "production_total_pct", "producer_price_total_pct",
  "producer_price_total_pct", "food_intake_total_pct"
)
mechanism_associations <- bind_rows(lapply(seq_len(nrow(association_specs)), function(i) {
  spec <- association_specs[i, ]
  pair <- country_evidence %>%
    transmute(x_value = .data[[spec$x]], y_value = .data[[spec$y]]) %>%
    filter(is.finite(x_value), is.finite(y_value))
  tibble(
    x = spec$x, y = spec$y, n = nrow(pair),
    spearman_rho = cor(pair$x_value, pair$y_value, method = "spearman"),
    pearson_r = cor(pair$x_value, pair$y_value)
  )
}))
write_csv(mechanism_associations,
          file.path(country_root, paste0("country_mechanism_associations_", target_year, ".csv")))

# R10 regions bridge the global totals and heterogeneous country responses.
# Market indicators are read from the already aggregated AgMIP R10 output;
# percentages are therefore calculated from regional levels, not averaged from
# country percentages. Land-mechanism indicators retain ALPHA basin detail and
# are aggregated using starting production or displaced harvested area.
r10_market_dir <- file.path(
  output_dir, "07_conditional_protection_decomposition", "01_agriculture",
  "region_r10", "aggregate_agmip_items", "crp_crops_non_energy_"
)
read_r10_component <- function(metric_file, metric_name) {
  read_csv(
    file.path(r10_market_dir, paste0(metric_file, "_components_", target_year, ".csv")),
    show_col_types = FALSE
  ) %>%
    transmute(
      r10 = as.character(region), region_label = as.character(region_label),
      metric = metric_name,
      baseline = mitigation_baseline,
      change = protection_under_mitigation,
      main_pct = protection_main_pct,
      interaction_pct = protection_x_afforestation_pct + protection_x_beccs_pct +
        protection_x_afforestation_x_beccs_pct,
      total_pct = protection_under_mitigation_pct
    )
}
r10_market_long <- bind_rows(
  read_r10_component("area", "CRP area"),
  read_r10_component("production", "CRP production"),
  read_r10_component("producer_price", "Producer price"),
  read_r10_component("food_intake", "Food intake"),
  read_r10_component("imports", "Imports"),
  read_r10_component("exports", "Exports"),
  read_r10_component("consumption", "Consumption")
)

country_r10 <- country_profile %>% distinct(country, r10)
r10_land_mechanism <- protection_crop_basin %>%
  left_join(country_r10, by = "country") %>%
  filter(!is.na(r10)) %>%
  group_by(crop) %>%
  mutate(global_crop_reference_yield = weighted.mean(yield_from, area_from)) %>%
  ungroup() %>%
  group_by(r10) %>%
  summarise(
    production_weighted_protection_increase_pp = weighted.mean(
      basin_protection_increase_pp, production_from, na.rm = TRUE
    ),
    harvested_area_loss_mha = sum(pmax(-area_change, 0), na.rm = TRUE) / 1000,
    harvested_area_gain_mha = sum(pmax(area_change, 0), na.rm = TRUE) / 1000,
    high_yield_loss_share_pct = 100 *
      sum(pmax(-area_change, 0) * (yield_percentile >= .8), na.rm = TRUE) /
      sum(pmax(-area_change, 0), na.rm = TRUE),
    source_normalized_yield = weighted.mean(
      yield_from / global_crop_reference_yield, pmax(-area_change, 0), na.rm = TRUE
    ),
    sink_normalized_yield = weighted.mean(
      yield_from / global_crop_reference_yield, pmax(area_change, 0), na.rm = TRUE
    ),
    sink_minus_source_normalized_yield_pct = 100 *
      (sink_normalized_yield / source_normalized_yield - 1),
    .groups = "drop"
  ) %>%
  mutate(across(where(is.numeric), ~ ifelse(is.finite(.x), .x, NA_real_)))

r10_market_wide <- r10_market_long %>%
  select(r10, region_label, metric, baseline, change, main_pct, interaction_pct, total_pct) %>%
  pivot_wider(
    names_from = metric, values_from = c(baseline, change, main_pct, interaction_pct, total_pct),
    names_glue = "{metric}_{.value}"
  ) %>%
  mutate(
    net_import_adjustment_pct_consumption = 100 *
      (`Imports_change` - `Exports_change`) / `Consumption_baseline`
  )
r10_evidence <- r10_market_wide %>%
  left_join(r10_land_mechanism, by = "r10") %>%
  transmute(
    r10, region_label,
    production_weighted_protection_increase_pp,
    harvested_area_loss_mha, harvested_area_gain_mha,
    high_yield_loss_share_pct, sink_minus_source_normalized_yield_pct,
    area_total_pct = `CRP area_total_pct`,
    production_total_pct = `CRP production_total_pct`,
    production_interaction_pct = `CRP production_interaction_pct`,
    net_import_adjustment_pct_consumption,
    producer_price_total_pct = `Producer price_total_pct`,
    food_intake_total_pct = `Food intake_total_pct`
  )
write_csv(r10_evidence,
          file.path(country_root, paste0("r10_land_market_evidence_", target_year, ".csv")))

r10_metrics <- c(
  production_weighted_protection_increase_pp = "Protection pressure\n(pp)",
  harvested_area_loss_mha = "Harvested-area\nloss (Mha)",
  harvested_area_gain_mha = "Harvested-area\ngain (Mha)",
  high_yield_loss_share_pct = "High-yield loss\nshare (%)",
  sink_minus_source_normalized_yield_pct = "Sink − source\nyield quality (%)",
  area_total_pct = "CRP area\nchange (%)",
  production_total_pct = "CRP production\nchange (%)",
  production_interaction_pct = "Production\ninteraction (pp)",
  net_import_adjustment_pct_consumption = "Net-import\nadjustment (pp)",
  producer_price_total_pct = "Producer price\nchange (%)",
  food_intake_total_pct = "Food intake\nchange (%)"
)
r10_heatmap <- r10_evidence %>%
  select(region_label, all_of(names(r10_metrics))) %>%
  pivot_longer(-region_label, names_to = "metric", values_to = "value") %>%
  group_by(metric) %>%
  mutate(z = if (sd(value, na.rm = TRUE) > 0) as.numeric(scale(value)) else rep(0, n())) %>%
  ungroup() %>%
  mutate(
    metric = factor(unname(r10_metrics[metric]), levels = unname(r10_metrics)),
    region_label = factor(region_label, levels = rev(unique(r10_evidence$region_label))),
    value_label = if_else(is.finite(value), number(value, accuracy = .1), "NA")
  )
p <- ggplot(r10_heatmap, aes(metric, region_label, fill = z)) +
  geom_tile(colour = "white", linewidth = .35) +
  geom_text(aes(label = value_label), size = 2.65) +
  scale_fill_gradient2(
    low = "#b2182b", mid = "white", high = "#2166ac", midpoint = 0,
    limits = c(-2.3, 2.3), oob = squish, na.value = "grey90",
    name = "Within-metric\nz score"
  ) +
  labs(
    title = "Regional pathways connect land displacement to market adjustment",
    subtitle = "Actual values are printed; colour only standardizes each indicator across the ten R10 regions",
    x = NULL, y = NULL,
    caption = "Protection under both CDR (111−011), 2100. Market values use regional AgMIP CRP aggregates; land diagnostics use ALPHA basin results."
  ) + story_theme +
  theme(
    panel.grid = element_blank(), axis.text.x = element_text(angle = 30, hjust = 1, size = 8),
    axis.text.y = element_text(size = 9)
  )
ggsave(file.path(country_root, paste0("r10_land_market_pathways_", target_year, ".png")),
       p, width = 15, height = 7.4, dpi = 300, bg = "white")

r10_context <- r10_market_long %>%
  filter(metric %in% c("CRP production", "Producer price", "Food intake")) %>%
  mutate(metric = factor(metric, levels = c("CRP production", "Producer price", "Food intake")))
p <- ggplot(r10_context, aes(y = reorder(region_label, total_pct))) +
  geom_vline(xintercept = 0, colour = "grey55", linewidth = .3) +
  geom_segment(aes(x = 0, xend = main_pct, yend = reorder(region_label, total_pct)),
               colour = "#636363", linewidth = 1.8, lineend = "round") +
  geom_segment(aes(x = main_pct, xend = total_pct, yend = reorder(region_label, total_pct),
                   colour = interaction_pct >= 0),
               linewidth = 1.8, lineend = "round") +
  geom_point(aes(x = total_pct), shape = 21, size = 2.7, fill = "white", stroke = .7) +
  facet_wrap(vars(metric), scales = "free_x", ncol = 3) +
  scale_colour_manual(
    values = c(`TRUE` = "#2166ac", `FALSE` = "#b2182b"),
    labels = c(`TRUE` = "Interaction raises outcome", `FALSE` = "Interaction lowers outcome"),
    name = NULL
  ) +
  labs(
    title = "Interaction shifts the protection effect differently across regions",
    subtitle = "Grey shows the protection-only effect; colour extends or reverses it to the total effect under both CDR options",
    x = "Effect (% of each region's 011 baseline)", y = NULL,
    caption = "Open circles are the observed 111−011 effects. The coloured segment is the sum of protection × afforestation, protection × BECCS and the three-way interaction."
  ) + story_theme + theme(panel.grid.major.y = element_blank())
ggsave(file.path(country_root, paste0("r10_protection_context_interaction_", target_year, ".png")),
       p, width = 14, height = 7.7, dpi = 300, bg = "white")

# 5. Are climate and biodiversity objectives retained? ---------------------

afforestation_removal <- merged_iamc %>%
  filter(
    i1 %in% states[c("011", "111")], i2 == "World", i3 == "Car_Rem_Lan_Use_ReA",
    as.integer(as.character(i4)) == target_year
  ) %>%
  transmute(state = state_from_scenario(i1), value) %>%
  group_by(state) %>% summarise(value = sum(value), .groups = "drop")
hub_bioenergy <- rgdx.param(file.path(data_dir, "ALPHA", "hub_data.gdx"), "QENE_hub") %>%
  filter(scenario_hub %in% states[c("011", "111")], as.integer(as.character(yr)) == target_year) %>%
  transmute(state = state_from_scenario(scenario_hub), value = QENE_hub) %>%
  group_by(state) %>% summarise(value = sum(value), .groups = "drop")
land_delivery <- world_land_states %>%
  filter(state %in% c("011", "111"), land_class %in% c(
    "Food cropland", "2G bioenergy cropland", "Afforestation"
  )) %>%
  select(state, metric = land_class, value)
delivery <- bind_rows(
  land_delivery,
  afforestation_removal %>% mutate(metric = "Afforestation carbon removal"),
  hub_bioenergy %>% mutate(metric = "Hub bioenergy demand")
) %>%
  pivot_wider(names_from = state, values_from = value) %>%
  mutate(
    protection_change_pct = 100 * (`111` / `011` - 1),
    protection_change_pct = if_else(abs(protection_change_pct) < 1e-5, 0, protection_change_pct),
    retained_pct = 100 * `111` / `011`
  )
write_csv(delivery,
          file.path(outcome_root, paste0("world_climate_land_delivery_", target_year, ".csv")))
p <- ggplot(delivery, aes(protection_change_pct, reorder(metric, protection_change_pct),
                          fill = protection_change_pct)) +
  geom_vline(xintercept = 0, colour = "grey40", linewidth = .35) +
  geom_col(width = .65) +
  geom_text(aes(label = sprintf("%+.2f%%", protection_change_pct)),
            hjust = -0.1, size = 3.2) +
  scale_fill_gradient2(low = "#b2182b", mid = "white", high = "#2166ac", midpoint = 0,
                       guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(.18, .25))) +
  labs(
    title = "Protection changes land allocation more than exogenous bioenergy demand",
    subtitle = "The chart tests whether mitigation delivery is retained when protection is added",
    x = "Change caused by protection under both CDR (%)", y = NULL,
    caption = "All bars compare 111 with 011. Hub QENE is shown as a demand index because its native unit is not documented in the supplied GDX."
  ) + story_theme + theme(panel.grid.major.y = element_blank())
ggsave(file.path(outcome_root, paste0("world_climate_land_delivery_", target_year, ".png")),
       p, width = 10, height = 5.7, dpi = 300, bg = "white")

bii <- read_csv(
  file.path(output_dir, "06_protection_focus", "04_climate_biodiversity",
            "world_bii_protection_trajectories.csv"), show_col_types = FALSE
) %>%
  filter(year == target_year, contrast == "Under mitigation") %>%
  transmute(metric = "Area-weighted BII change", value = change_bii_points,
            unit = "BII points")
luc <- read_csv(
  file.path(output_dir, "06_protection_focus", "04_climate_biodiversity",
            "world_luc_emission_protection_trajectories.csv"), show_col_types = FALSE
) %>%
  filter(year == target_year, contrast == "Under mitigation") %>%
  transmute(metric = "LUC emission reduction", value = emission_reduction,
            unit = "Mt CO2/yr")
protected <- read_csv(
  file.path(output_dir, "08_land_use_protection", "02_protection_displacement", "world",
            paste0("world_protection_reallocation_summary_", target_year, ".csv")),
  show_col_types = FALSE
) %>%
  filter(metric == "Added protected footprint") %>%
  slice(1) %>%
  transmute(metric = "Additional protected area", value = value_mha,
            unit = "million ha")
environmental_outcomes <- bind_rows(bii, luc, protected) %>%
  mutate(label = paste0(number(value, accuracy = .01), " ", unit))
write_csv(environmental_outcomes,
          file.path(outcome_root, paste0("world_environmental_outcomes_", target_year, ".csv")))
p <- ggplot(environmental_outcomes, aes(value, metric, fill = metric)) +
  geom_col(width = .6) +
  geom_text(aes(label = label), hjust = -0.08, size = 3.3) +
  facet_wrap(vars(unit), scales = "free", ncol = 1) +
  scale_fill_manual(values = c(
    "Area-weighted BII change" = "#238b45", "LUC emission reduction" = "#3182bd",
    "Additional protected area" = "#756bb1"
  ), guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0, .3))) +
  labs(
    title = "Protection produces mixed biodiversity and land-use-emission outcomes",
    subtitle = "The global mean BII response must be checked alongside protected area and LUC emissions",
    x = NULL, y = NULL,
    caption = "Protection under both CDR (111−011), 2100. Facets use different physical units and should not be compared by bar length."
  ) + story_theme + theme(panel.grid.major.y = element_blank())
ggsave(file.path(outcome_root, paste0("world_environmental_outcomes_", target_year, ".png")),
       p, width = 9, height = 7.5, dpi = 300, bg = "white")

rm(merged_iamc)
gc()
