library(tidyverse)
library(gdxrrw)
library(patchwork)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(scales)

sf_use_s2(FALSE)

base_dir <- normalizePath("..", winslash = "/", mustWork = TRUE)
data_dir <- file.path(base_dir, "data", "0311")
out_dir <- file.path(base_dir, "output", "0311")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
if (!dir.exists(out_dir)) {
  stop("Output directory is not available: ", out_dir)
}

scenario_baseline <- "SSP2_BaU_NoCC_No"
scenario_harmsen <- "SSP2_1100C_CACN_DAC_NoCC_No"
scenario_landint <- "SSP2_1100C_CACN_DAC_NoCC_LandInt_global1"

scenario_labels <- c(
  SSP2_BaU_NoCC_No = "Baseline",
  SSP2_1100C_CACN_DAC_NoCC_No = "Harmsen MACC",
  SSP2_1100C_CACN_DAC_NoCC_LandInt_global1 = "LandInt"
)

scenario_palette <- c(
  "Baseline" = "#5B6572",
  "Harmsen MACC" = "#C06C2B",
  "LandInt" = "#0E7490"
)

theme_geer <- theme_bw(base_size = 14) +
  theme(
    panel.grid.major = element_line(color = "grey90", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank(),
    strip.background = element_blank(),
    plot.title.position = "plot",
    plot.caption.position = "plot"
  )

save_plot <- function(plot_obj, filename, width = 10, height = 7) {
  out_file <- file.path(out_dir, filename)
  ggsave(out_file, plot = plot_obj, width = width, height = height, dpi = 250, bg = "white")
  message("Saved: ", out_file)
}

read_iamcplus <- function(variables, regions, years, scenarios) {
  req <- list(
    name = "IAMC_templatePlus",
    form = "full",
    uels = list(scenarios, regions, variables, as.character(years))
  )
  x <- rgdx(file.path(data_dir, "IAMC_templatePlus.gdx"), req, squeeze = FALSE)
  as.data.frame.table(x$val, responseName = "value", stringsAsFactors = FALSE) %>%
    setNames(c("scenario", "region", "variable", "year", "value")) %>%
    mutate(
      year = as.integer(year),
      value = as.numeric(value)
    )
}

read_merged <- function(variables, units, regions, years, scenarios) {
  req <- list(
    name = "IAMCTemp",
    form = "full",
    uels = list(scenarios, regions, variables, units, as.character(years))
  )
  x <- rgdx(file.path(data_dir, "Mergedcomparison.gdx"), req, squeeze = FALSE)
  as.data.frame.table(x$val, responseName = "value", stringsAsFactors = FALSE) %>%
    setNames(c("scenario", "region", "variable", "unit", "year", "value")) %>%
    mutate(
      year = as.integer(year),
      value = as.numeric(value)
    ) %>%
    filter(value != 0)
}

country_sf <- ne_countries(scale = "medium", returnclass = "sf") %>%
  st_make_valid() %>%
  transmute(region = iso_a3, country_name = admin) %>%
  filter(region != "-99")

country_codes <- country_sf$region %>% unique() %>% sort()
analysis_regions <- c("World", country_codes)
analysis_years <- c(2050, 2100)

plus_vars <- c(
  "Agr_Pro_Cro_NonEne",
  "Agr_Pro_Liv",
  "Agr_Pro_Liv_Rum",
  "Agr_Pro_Liv_NonRum",
  "Agr_Pro_Liv_Dai",
  "Prc_Agr_Cor_Ind",
  "Prc_Agr_Whe_Ind",
  "Prc_Agr_Soy_Ind",
  "Red_Cos_AFO_Agr"
)

merged_vars <- c(
  "Lan_Cov_Frs",
  "Lan_Cov_Cro",
  "Lan_Cov_Pst",
  "Emi_CO2_AFO"
)

plus_df <- read_iamcplus(
  variables = plus_vars,
  regions = analysis_regions,
  years = analysis_years,
  scenarios = c(scenario_harmsen, scenario_landint)
)

merged_df <- read_merged(
  variables = merged_vars,
  units = c("million ha", "Mt CO2/yr"),
  regions = analysis_regions,
  years = analysis_years,
  scenarios = c(scenario_baseline, scenario_harmsen, scenario_landint)
)

message("IAMC_templatePlus rows: ", nrow(plus_df))
message("Mergedcomparison rows: ", nrow(merged_df))

plus_wide <- plus_df %>%
  pivot_wider(names_from = scenario, values_from = value) %>%
  mutate(
    change_landint_vs_harmsen = .data[[scenario_landint]] - .data[[scenario_harmsen]],
    pct_landint_vs_harmsen = if_else(abs(.data[[scenario_harmsen]]) > 1e-12,
      100 * (.data[[scenario_landint]] / .data[[scenario_harmsen]] - 1),
      NA_real_
    )
  )

merged_wide <- merged_df %>%
  pivot_wider(names_from = scenario, values_from = value) %>%
  mutate(
    change_harmsen_vs_baseline = .data[[scenario_harmsen]] - .data[[scenario_baseline]],
    change_landint_vs_baseline = .data[[scenario_landint]] - .data[[scenario_baseline]],
    change_landint_vs_harmsen = .data[[scenario_landint]] - .data[[scenario_harmsen]],
    pct_landint_vs_baseline = if_else(abs(.data[[scenario_baseline]]) > 1e-12,
      100 * (.data[[scenario_landint]] / .data[[scenario_baseline]] - 1),
      NA_real_
    )
  )

summary_country <- plus_wide %>%
  filter(region != "World", year %in% analysis_years) %>%
  select(region, year, variable, value_h = all_of(scenario_harmsen), value_l = all_of(scenario_landint), change_landint_vs_harmsen) %>%
  pivot_wider(
    names_from = variable,
    values_from = c(value_h, value_l, change_landint_vs_harmsen),
    names_glue = "{variable}_{.value}"
  ) %>%
  left_join(
    merged_wide %>%
      filter(region != "World", year %in% analysis_years, variable %in% c("Lan_Cov_Frs", "Lan_Cov_Cro", "Lan_Cov_Pst", "Emi_CO2_AFO")) %>%
      select(region, year, variable, baseline = all_of(scenario_baseline), harmsen = all_of(scenario_harmsen), landint = all_of(scenario_landint), change_harmsen_vs_baseline, change_landint_vs_baseline, change_landint_vs_harmsen) %>%
      pivot_wider(
        names_from = variable,
        values_from = c(baseline, harmsen, landint, change_harmsen_vs_baseline, change_landint_vs_baseline, change_landint_vs_harmsen),
        names_glue = "{variable}_{.value}"
      ),
    by = c("region", "year")
  ) %>%
  mutate(
    cost_per_forest_gain = Red_Cos_AFO_Agr_value_l / pmax(Lan_Cov_Frs_change_landint_vs_harmsen, 1e-6),
    crop_to_livestock_ratio = Agr_Pro_Cro_NonEne_value_l / pmax(Agr_Pro_Liv_value_l, 1e-6),
    price_gap_landint_vs_harmsen = Prc_Agr_Cor_Ind_change_landint_vs_harmsen,
    wheat_gap_landint_vs_harmsen = Prc_Agr_Whe_Ind_change_landint_vs_harmsen,
    soy_gap_landint_vs_harmsen = Prc_Agr_Soy_Ind_change_landint_vs_harmsen
  ) %>%
  left_join(st_drop_geometry(country_sf), by = "region")

readr::write_csv(summary_country %>% filter(year == 2050), file.path(out_dir, "country_summary_2050.csv"))
readr::write_csv(summary_country %>% filter(year == 2100), file.path(out_dir, "country_summary_2100.csv"))

top_cost_2050 <- summary_country %>%
  filter(year == 2050) %>%
  slice_max(order_by = Red_Cos_AFO_Agr_value_l, n = 15, with_ties = FALSE) %>%
  mutate(country_name = fct_reorder(coalesce(country_name, region), Red_Cos_AFO_Agr_value_l))

p1 <- ggplot(top_cost_2050, aes(x = Red_Cos_AFO_Agr_value_l, y = country_name)) +
  geom_col(fill = "#0E7490") +
  scale_x_continuous(labels = label_comma()) +
  labs(
    title = "Top 15 AFOLU mitigation cost countries in 2050",
    subtitle = "LandInt scenario",
    x = "Reduction cost",
    y = NULL
  ) +
  theme_geer
save_plot(p1, "fig01_top_cost_countries_2050.png", width = 10, height = 8)

scatter_2050 <- summary_country %>%
  filter(year == 2050) %>%
  drop_na(Red_Cos_AFO_Agr_change_landint_vs_harmsen, Lan_Cov_Frs_change_landint_vs_harmsen, Prc_Agr_Cor_Ind_change_landint_vs_harmsen)

p2 <- ggplot(scatter_2050, aes(
  x = Lan_Cov_Frs_change_landint_vs_harmsen,
  y = Prc_Agr_Cor_Ind_change_landint_vs_harmsen,
  size = Red_Cos_AFO_Agr_value_l
)) +
  geom_hline(yintercept = 0, color = "grey70", linewidth = 0.4) +
  geom_vline(xintercept = 0, color = "grey70", linewidth = 0.4) +
  geom_point(alpha = 0.75, color = "#0E7490") +
  scale_size_continuous(labels = label_comma()) +
  labs(
    title = "Forest gain and crop price response in 2050",
    subtitle = "LandInt minus Harmsen MACC",
    x = "Forest area change [million ha]",
    y = "Crop price index change",
    size = "AFOLU cost"
  ) +
  theme_geer
save_plot(p2, "fig02_forest_vs_price_2050.png")

p3 <- ggplot(scatter_2050, aes(
  x = Agr_Pro_Cro_NonEne_change_landint_vs_harmsen,
  y = Agr_Pro_Liv_change_landint_vs_harmsen,
  size = Red_Cos_AFO_Agr_value_l
)) +
  geom_hline(yintercept = 0, color = "grey70", linewidth = 0.4) +
  geom_vline(xintercept = 0, color = "grey70", linewidth = 0.4) +
  geom_point(alpha = 0.75, color = "#C06C2B") +
  scale_size_continuous(labels = label_comma()) +
  labs(
    title = "Production reallocation under market feedback in 2050",
    subtitle = "LandInt minus Harmsen MACC",
    x = "Crop production change",
    y = "Livestock production change",
    size = "AFOLU cost"
  ) +
  theme_geer
save_plot(p3, "fig03_crop_vs_livestock_2050.png")

portfolio_2050 <- summary_country %>%
  filter(year == 2050) %>%
  slice_max(order_by = Red_Cos_AFO_Agr_value_l, n = 12, with_ties = FALSE) %>%
  transmute(
    country = coalesce(country_name, region),
    Crop = Agr_Pro_Cro_NonEne_change_landint_vs_harmsen,
    Livestock = Agr_Pro_Liv_change_landint_vs_harmsen,
    Forest = Lan_Cov_Frs_change_landint_vs_harmsen,
    Cropland = Lan_Cov_Cro_change_landint_vs_harmsen,
    Pasture = Lan_Cov_Pst_change_landint_vs_harmsen,
    Price = Prc_Agr_Cor_Ind_change_landint_vs_harmsen
  ) %>%
  pivot_longer(-country, names_to = "indicator", values_to = "value")

p4 <- ggplot(portfolio_2050, aes(
  x = indicator,
  y = fct_reorder(country, value, .fun = sum, .desc = TRUE),
  fill = value
)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "#8B1E3F", mid = "white", high = "#0E7490", midpoint = 0) +
  labs(
    title = "Where market feedback changes the result in 2050",
    subtitle = "Top cost countries, LandInt minus Harmsen MACC",
    x = NULL,
    y = NULL
  ) +
  theme_geer +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
save_plot(p4, "fig04_feedback_heatmap_2050.png", width = 11, height = 8)

land_delta_long <- merged_wide %>%
  filter(region != "World", year %in% analysis_years, variable %in% c("Lan_Cov_Frs", "Lan_Cov_Cro", "Lan_Cov_Pst")) %>%
  transmute(
    region,
    year,
    variable = recode(variable,
      Lan_Cov_Frs = "Forest",
      Lan_Cov_Cro = "Cropland",
      Lan_Cov_Pst = "Pasture"
    ),
    `Harmsen MACC` = change_harmsen_vs_baseline,
    LandInt = change_landint_vs_baseline
  ) %>%
  pivot_longer(c(`Harmsen MACC`, LandInt), names_to = "scenario", values_to = "change")

share_plot_2100 <- land_delta_long %>%
  filter(year == 2100) %>%
  group_by(variable, scenario) %>%
  slice_max(order_by = abs(change), n = 12, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(country = fct_reorder(region, change))

p5 <- ggplot(share_plot_2100, aes(x = change, y = country, fill = scenario)) +
  geom_col(position = "dodge") +
  facet_wrap(~ variable, scales = "free_x") +
  labs(
    title = "Largest land-use shifts in 2100",
    subtitle = "Difference from baseline",
    x = "Change [million ha]",
    y = NULL
  ) +
  scale_fill_manual(values = scenario_palette[c("Harmsen MACC", "LandInt")]) +
  theme_geer
save_plot(p5, "fig05_land_shift_rankings_2100.png", width = 12, height = 9)

cost_compare <- plus_df %>%
  filter(variable == "Red_Cos_AFO_Agr") %>%
  mutate(scenario = recode(scenario, !!!scenario_labels)) %>%
  filter(region == "World")

p6 <- ggplot(cost_compare, aes(x = factor(year), y = value, fill = scenario)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = scenario_palette[c("Harmsen MACC", "LandInt")]) +
  scale_y_continuous(labels = label_comma()) +
  labs(
    title = "Global AFOLU mitigation cost",
    subtitle = "Harmsen MACC versus LandInt",
    x = "Year",
    y = "Reduction cost"
  ) +
  theme_geer
save_plot(p6, "fig06_global_cost_comparison.png", width = 8, height = 6)

emission_compare <- merged_wide %>%
  filter(region == "World", variable == "Emi_CO2_AFO") %>%
  transmute(
    year,
    Baseline = .data[[scenario_baseline]],
    `Harmsen MACC` = .data[[scenario_harmsen]],
    LandInt = .data[[scenario_landint]]
  ) %>%
  pivot_longer(-year, names_to = "scenario", values_to = "value")

p7 <- ggplot(emission_compare, aes(x = factor(year), y = value, fill = scenario)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = scenario_palette) +
  labs(
    title = "Global AFOLU CO2 emissions",
    subtitle = "Baseline and mitigation scenarios",
    x = "Year",
    y = "Emissions [Mt CO2/yr]"
  ) +
  theme_geer
save_plot(p7, "fig07_global_afolu_co2.png", width = 8, height = 6)

map_base <- country_sf %>%
  left_join(
    summary_country %>%
      filter(year == 2050) %>%
      select(
        region,
        forest_change = Lan_Cov_Frs_change_landint_vs_baseline,
        cropland_change = Lan_Cov_Cro_change_landint_vs_baseline,
        pasture_change = Lan_Cov_Pst_change_landint_vs_baseline,
        co2_change = Emi_CO2_AFO_change_landint_vs_baseline
      ),
    by = "region"
  )

map_plot <- function(sf_df, var_name, title_txt, filename) {
  p <- ggplot(sf_df) +
    geom_sf(aes(fill = .data[[var_name]]), color = NA) +
    scale_fill_gradient2(low = "#8B1E3F", mid = "white", high = "#0E7490", midpoint = 0, labels = label_number(accuracy = 0.1)) +
    labs(title = title_txt, fill = NULL) +
    theme_geer +
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank()
    )
  save_plot(p, filename, width = 12, height = 6.5)
}

map_plot(map_base, "forest_change", "Forest area change in 2050: LandInt minus baseline", "fig08_map_forest_change_2050.png")
map_plot(map_base, "cropland_change", "Cropland change in 2050: LandInt minus baseline", "fig09_map_cropland_change_2050.png")
map_plot(map_base, "co2_change", "AFOLU CO2 change in 2050: LandInt minus baseline", "fig10_map_co2_change_2050.png")

harmsen_map <- country_sf %>%
  left_join(
    summary_country %>%
      filter(year == 2050) %>%
      select(region, forest_feedback = Lan_Cov_Frs_change_landint_vs_harmsen, price_feedback = Prc_Agr_Cor_Ind_change_landint_vs_harmsen),
    by = "region"
  )

map_plot(harmsen_map, "forest_feedback", "Forest feedback in 2050: LandInt minus Harmsen MACC", "fig11_map_feedback_forest_2050.png")
map_plot(harmsen_map, "price_feedback", "Crop price feedback in 2050: LandInt minus Harmsen MACC", "fig12_map_feedback_price_2050.png")

top_forest_gain <- summary_country %>%
  filter(year == 2050) %>%
  slice_max(order_by = Lan_Cov_Frs_change_landint_vs_baseline, n = 15, with_ties = FALSE) %>%
  mutate(country = fct_reorder(coalesce(country_name, region), Lan_Cov_Frs_change_landint_vs_baseline))

top_forest_loss <- summary_country %>%
  filter(year == 2050) %>%
  slice_min(order_by = Lan_Cov_Frs_change_landint_vs_baseline, n = 15, with_ties = FALSE) %>%
  mutate(country = fct_reorder(coalesce(country_name, region), Lan_Cov_Frs_change_landint_vs_baseline))

p13a <- ggplot(top_forest_gain, aes(x = Lan_Cov_Frs_change_landint_vs_baseline, y = country)) +
  geom_col(fill = "#0E7490") +
  labs(title = "Top forest gain countries in 2050", x = "Forest area change [million ha]", y = NULL) +
  theme_geer

p13b <- ggplot(top_forest_loss, aes(x = Lan_Cov_Frs_change_landint_vs_baseline, y = country)) +
  geom_col(fill = "#8B1E3F") +
  labs(title = "Top forest loss countries in 2050", x = "Forest area change [million ha]", y = NULL) +
  theme_geer

save_plot(p13a + p13b, "fig13_forest_winners_losers_2050.png", width = 13, height = 7)

message("Completed GEER2026 exploratory analysis.")
