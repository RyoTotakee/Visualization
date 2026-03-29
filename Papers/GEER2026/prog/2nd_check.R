library(tidyverse)
library(gdxrrw)
library(patchwork)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(scales)

sf_use_s2(FALSE)

# AFOLU mitigation is concentrated, but priority is not determined by mitigation volume alone.

args_all <- commandArgs(trailingOnly = FALSE)
file_arg <- "--file="
script_arg <- args_all[startsWith(args_all, file_arg)]
script_dir <- if (length(script_arg) > 0) {
  dirname(normalizePath(sub(file_arg, "", script_arg[1]), winslash = "/", mustWork = TRUE))
} else {
  normalizePath("prog", winslash = "/", mustWork = TRUE)
}
base_dir <- normalizePath(file.path(script_dir, ".."), winslash = "/", mustWork = TRUE)
data_dir <- file.path(base_dir, "data", "0311")
out_dir <- file.path(base_dir, "output", "0311", "2nd_check")
country_set_file <- file.path(base_dir, "define", "country.set")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
if (!dir.exists(out_dir)) {
  stop("Output directory is not available: ", out_dir)
}

scenario_baseline <- "SSP2_BaU_NoCC_No"
scenario_landint <- "SSP2_1100C_CACN_DAC_NoCC_LandInt_global1"

scenario_labels <- c(
  SSP2_BaU_NoCC_No = "Baseline",
  SSP2_1100C_CACN_DAC_NoCC_LandInt_global1 = "LandInt"
)

scenario_palette <- c(
  "Baseline" = "#5B6572",
  "LandInt" = "#0B7285"
)

typology_palette <- c(
  "Priority countries" = "#0B7285",
  "High-potential but sensitive" = "#B02A37",
  "Low-potential stable" = "#6C757D",
  "Support-first countries" = "#C77D1D"
)

commodity_palette <- c(
  "Wheat" = "#C96F28",
  "Rice" = "#3A7D44",
  "Other cereals" = "#B39A30",
  "Oilseeds" = "#7A3E9D",
  "Ruminant" = "#8C2D04",
  "Non-ruminant" = "#0B7285",
  "Dairy" = "#457B9D"
)

theme_geer <- theme_bw(base_size = 10) +
  theme(
    panel.grid.major = element_line(color = "grey90", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank(),
    strip.background = element_blank(),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title = element_text(size = 10),
    plot.subtitle = element_text(size = 8),
    plot.caption = element_text(size = 7),
    axis.text = element_text(size = 8),
    legend.text = element_text(size = 8),
    axis.title.x = element_text(margin = margin(t = 8)),
    axis.title.y = element_text(margin = margin(r = 8))
  )

save_plot <- function(plot_obj, filename, width = 10, height = 7) {
  out_file <- file.path(out_dir, filename)
  ggsave(out_file, plot = plot_obj, width = width, height = height, dpi = 260, bg = "white")
  message("Saved: ", out_file)
}

scale01 <- function(x) {
  if (all(is.na(x))) {
    return(rep(NA_real_, length(x)))
  }
  xr <- range(x, na.rm = TRUE)
  if (!is.finite(xr[1]) || !is.finite(xr[2]) || xr[1] == xr[2]) {
    return(rep(0.5, length(x)))
  }
  (x - xr[1]) / (xr[2] - xr[1])
}

safe_pull <- function(df, name) {
  if (name %in% names(df)) {
    return(df[[name]])
  }
  rep(NA_real_, nrow(df))
}

# IAMC_templatePlus = AFOLUB-side mitigation outputs.
# Use this mainly for original mitigation-side variables such as reduction cost,
# CH4/N2O/CO2 reduction, and residual agricultural emissions under LandInt.
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

# IAMCTemp = Mergedcomparison IAMC table with explicit units.
# Use this for baseline-vs-mitigation comparisons of variables that are stable in
# the merged file, especially AFOLU CO2-type indicators.
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

# IAMCTempwoU = Mergedcomparison IAMC table without the unit dimension.
# Use this for land-cover and land-use-flow variables when unit handling in
# IAMCTemp is awkward or inconsistent.
read_merged_wou <- function(variables, regions, years, scenarios) {
  req <- list(
    name = "IAMCTempwoU",
    form = "full",
    uels = list(scenarios, regions, variables, as.character(years))
  )
  x <- rgdx(file.path(data_dir, "Mergedcomparison.gdx"), req, squeeze = FALSE)
  as.data.frame.table(x$val, responseName = "value", stringsAsFactors = FALSE) %>%
    setNames(c("scenario", "region", "variable", "year", "value")) %>%
    mutate(
      year = as.integer(year),
      value = as.numeric(value)
    ) %>%
    filter(value != 0)
}

# GHG = Mergedcomparison land-use emission/uptake summary by component.
# Use this for LULUCF-side emissions, sinks, and net emissions rather than
# constructing new indicators from unrelated sources.
read_ghg <- function(regions, years, scenarios) {
  req <- list(
    name = "GHG",
    form = "full",
    uels = list(scenarios, regions, as.character(years), c("Sink", "Emissions", "Net_emissions"), c("LUM"))
  )
  x <- rgdx(file.path(data_dir, "Mergedcomparison.gdx"), req, squeeze = FALSE)
  as.data.frame.table(x$val, responseName = "value", stringsAsFactors = FALSE) %>%
    setNames(c("scenario", "region", "year", "component", "model", "value")) %>%
    mutate(
      year = as.integer(year),
      value = as.numeric(value)
    ) %>%
    filter(value != 0)
}

# AREA = Mergedcomparison land allocation by land_code.
# Use this for direct land-use change plots by forest, cropland, pasture, etc.
read_area <- function(regions, years, scenarios) {
  req <- list(
    name = "AREA",
    form = "full",
    uels = list(scenarios, regions, as.character(years), c("FRS", "CL", "PAS", "GL", "OL", "PRMFRS", "SECFRS", "MNGPAS", "RAN"), c("LUM"))
  )
  x <- rgdx(file.path(data_dir, "Mergedcomparison.gdx"), req, squeeze = FALSE)
  as.data.frame.table(x$val, responseName = "value", stringsAsFactors = FALSE) %>%
    setNames(c("scenario", "region", "year", "land_code", "model", "value")) %>%
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

country_codes <- if (file.exists(country_set_file)) {
  scan(country_set_file, what = character(), quiet = TRUE)
} else {
  stop("country.set is not available: ", country_set_file)
}
analysis_regions <- c("World", sort(unique(country_codes)))
analysis_years <- c(2030, 2050, 2100)

iamcplus_vars <- c(
  "Red_Cos_AFO_Agr",
  "Red_Cos_AFO_Agr_CH4",
  "Red_Cos_AFO_Agr_N2O",
  "Red_Cos_AFO_Agr_CO2",
  "Emi_Red_CH4_AFO_Agr",
  "Emi_Red_N2O_AFO_Agr",
  "Emi_Red_CO2_AFO_Lan",
  "Agr_Pro_Cro_NonEne",
  "Agr_Pro_Liv",
  "Agr_Pro_Liv_Rum",
  "Agr_Pro_Liv_NonRum",
  "Agr_Pro_Liv_Dai",
  "Prc_Agr_Cor_Ind",
  "Prc_Agr_Whe_Ind",
  "Prc_Agr_Soy_Ind",
  "Lan_Cov_Frs",
  "Lan_Cov_Cro",
  "Lan_Cov_Pst",
  "Emi_CO2_AFO",
  "Emi_equ_CH4_AFO_Agr",
  "Emi_equ_N2O_AFO_Agr"
)

merged_vars <- c(
  "Emi_CO2_AFO"
)

plus_df <- read_iamcplus(
  variables = iamcplus_vars,
  regions = analysis_regions,
  years = analysis_years,
  scenarios = c(scenario_landint)
)

merged_df <- read_merged(
  variables = merged_vars,
  units = c("million ha", "Mt CO2/yr"),
  regions = analysis_regions,
  years = analysis_years,
  scenarios = c(scenario_baseline, scenario_landint)
)

merged_wou_df <- read_merged_wou(
  variables = c(
    "Lan_Cov_Frs",
    "Emi_CO2_AFO_Lan",
    "Emi_CO2_Lan_Use_Flo_Pos_Emi",
    "Emi_CO2_Lan_Use_Flo_Neg_Seq",
    "Emi_CO2_AFO_Def",
    "Emi_CO2_AFO_Aff",
    "Emi_CO2_AFO_For_Man"
  ),
  regions = analysis_regions,
  years = analysis_years,
  scenarios = c(scenario_baseline, scenario_landint)
)

ghg_df <- read_ghg(
  regions = analysis_regions,
  years = analysis_years,
  scenarios = c(scenario_baseline, scenario_landint)
)

area_df <- read_area(
  regions = analysis_regions,
  years = analysis_years,
  scenarios = c(scenario_baseline, scenario_landint)
)

message("IAMCplus rows: ", nrow(plus_df))
message("Merged rows: ", nrow(merged_df))
message("MergedwoU rows: ", nrow(merged_wou_df))
message("GHG rows: ", nrow(ghg_df))
message("AREA rows: ", nrow(area_df))

# AFOLUB variables are kept as original LandInt outputs and joined by country-year.
# No baseline is constructed here because baseline handling is cleaner in
# Mergedcomparison for the current GEER2026 workflow.
plus_wide <- plus_df %>%
  rename(value_landint = value) %>%
  select(region, year, variable, value_landint)

merged_wide <- merged_df %>%
  pivot_wider(names_from = scenario, values_from = value) %>%
  mutate(
    baseline = .data[[scenario_baseline]],
    landint = .data[[scenario_landint]],
    change_landint_vs_baseline = landint - baseline
  ) %>%
  select(region, year, variable, baseline, landint, change_landint_vs_baseline)

merged_wou_wide <- merged_wou_df %>%
  pivot_wider(names_from = scenario, values_from = value) %>%
  mutate(
    baseline = .data[[scenario_baseline]],
    landint = .data[[scenario_landint]],
    change_landint_vs_baseline = landint - baseline
  ) %>%
  select(region, year, variable, baseline, landint, change_landint_vs_baseline)

ghg_wide <- ghg_df %>%
  pivot_wider(names_from = scenario, values_from = value) %>%
  mutate(
    baseline = .data[[scenario_baseline]],
    landint = .data[[scenario_landint]],
    improvement = baseline - landint,
    component = recode(component,
      Sink = "sink",
      Emissions = "emissions",
      Net_emissions = "net_emissions"
    )
  ) %>%
  select(region, year, component, baseline, landint, improvement)

area_wide <- area_df %>%
  pivot_wider(names_from = scenario, values_from = value) %>%
  mutate(
    baseline = .data[[scenario_baseline]],
    landint = .data[[scenario_landint]],
    change_landint_vs_baseline = landint - baseline
  ) %>%
  select(region, year, land_code, baseline, landint, change_landint_vs_baseline)

plus_country <- plus_wide %>%
  filter(region != "World") %>%
  select(region, year, variable, value_landint) %>%
  pivot_wider(
    names_from = variable,
    values_from = c(value_landint),
    names_glue = "{variable}_{.value}"
  )

merged_country <- merged_wide %>%
  filter(region != "World") %>%
  pivot_wider(
    names_from = variable,
    values_from = c(baseline, landint, change_landint_vs_baseline),
    names_glue = "{variable}_{.value}"
  )

merged_wou_country <- merged_wou_wide %>%
  filter(region != "World") %>%
  pivot_wider(
    names_from = variable,
    values_from = c(baseline, landint, change_landint_vs_baseline),
    names_glue = "{variable}_{.value}"
  )

ghg_country <- ghg_wide %>%
  filter(region != "World") %>%
  pivot_wider(
    names_from = component,
    values_from = c(baseline, landint, improvement),
    names_glue = "{component}_{.value}"
  )

area_country <- area_wide %>%
  filter(region != "World") %>%
  mutate(land_code = recode(land_code,
    FRS = "frs",
    CL = "cl",
    PAS = "pas",
    GL = "gl",
    OL = "ol",
    PRMFRS = "prmfrs",
    SECFRS = "secfrs",
    MNGPAS = "mngpas",
    RAN = "ran"
  )) %>%
  pivot_wider(
    names_from = land_code,
    values_from = c(baseline, landint, change_landint_vs_baseline),
    names_glue = "{land_code}_{.value}"
  )

country_joined <- merged_country %>%
  left_join(plus_country, by = c("region", "year")) %>%
  left_join(merged_wou_country, by = c("region", "year")) %>%
  left_join(ghg_country, by = c("region", "year")) %>%
  left_join(area_country, by = c("region", "year")) %>%
  left_join(st_drop_geometry(country_sf), by = "region") %>%
  filter(region %in% country_codes)

# Build a country-year analysis table centered on original variables or simple
# baseline-minus-LandInt / LandInt-minus-baseline differences.
# Avoid introducing custom synthetic indicators beyond light ranking or scaling
# helpers that are only used to organize plots.
country_core <- country_joined %>%
  mutate(
    mitigation_improvement = safe_pull(country_joined, "Emi_CO2_AFO_baseline") - safe_pull(country_joined, "Emi_CO2_AFO_landint"),
    forest_gain = safe_pull(country_joined, "Lan_Cov_Frs_landint") - safe_pull(country_joined, "Lan_Cov_Frs_baseline"),
    cropland_change = safe_pull(country_joined, "Lan_Cov_Cro_landint") - safe_pull(country_joined, "Lan_Cov_Cro_baseline"),
    pasture_change = safe_pull(country_joined, "Lan_Cov_Pst_landint") - safe_pull(country_joined, "Lan_Cov_Pst_baseline"),
    positive_flow_improvement = safe_pull(country_joined, "Emi_CO2_Lan_Use_Flo_Pos_Emi_baseline") - safe_pull(country_joined, "Emi_CO2_Lan_Use_Flo_Pos_Emi_landint"),
    negative_seq_improvement = safe_pull(country_joined, "Emi_CO2_Lan_Use_Flo_Neg_Seq_baseline") - safe_pull(country_joined, "Emi_CO2_Lan_Use_Flo_Neg_Seq_landint"),
    deforestation_improvement = safe_pull(country_joined, "Emi_CO2_AFO_Def_baseline") - safe_pull(country_joined, "Emi_CO2_AFO_Def_landint"),
    afforestation_change = safe_pull(country_joined, "Emi_CO2_AFO_Aff_landint") - safe_pull(country_joined, "Emi_CO2_AFO_Aff_baseline"),
    forest_management_change = safe_pull(country_joined, "Emi_CO2_AFO_For_Man_landint") - safe_pull(country_joined, "Emi_CO2_AFO_For_Man_baseline"),
    cost_landint = safe_pull(country_joined, "Red_Cos_AFO_Agr_value_landint"),
    ag_ch4_reduction = safe_pull(country_joined, "Emi_Red_CH4_AFO_Agr_value_landint"),
    ag_n2o_reduction = safe_pull(country_joined, "Emi_Red_N2O_AFO_Agr_value_landint"),
    land_co2_reduction = safe_pull(country_joined, "Emi_Red_CO2_AFO_Lan_value_landint"),
    residual_ch4 = safe_pull(country_joined, "Emi_equ_CH4_AFO_Agr_value_landint"),
    residual_n2o = safe_pull(country_joined, "Emi_equ_N2O_AFO_Agr_value_landint"),
    land_burden_index_raw = rowMeans(cbind(
      pmax(-cropland_change, 0),
      pmax(-pasture_change, 0),
      pmax(-safe_pull(country_joined, "frs_change_landint_vs_baseline"), 0)
    ), na.rm = TRUE)
  ) %>%
  mutate(
    mitigation_score = scale01(mitigation_improvement),
    forest_score = scale01(forest_gain),
    sink_score = scale01(safe_pull(country_joined, "sink_improvement")),
    net_emissions_score = scale01(safe_pull(country_joined, "net_emissions_improvement")),
    cost_score = scale01(cost_landint),
    land_burden_index = scale01(land_burden_index_raw),
    policy_attractiveness = rowMeans(cbind(mitigation_score, forest_score, sink_score, net_emissions_score, 1 - cost_score, 1 - land_burden_index), na.rm = TRUE),
    structure_shift_index = rowMeans(cbind(
      scale01(abs(safe_pull(country_joined, "cl_change_landint_vs_baseline"))),
      scale01(abs(safe_pull(country_joined, "pas_change_landint_vs_baseline"))),
      scale01(abs(safe_pull(country_joined, "frs_change_landint_vs_baseline"))),
      scale01(abs(positive_flow_improvement)),
      scale01(abs(negative_seq_improvement))
    ), na.rm = TRUE),
    concentration_rank = min_rank(desc(mitigation_improvement)),
    technology_land_share = land_co2_reduction / pmax(ag_ch4_reduction + ag_n2o_reduction + land_co2_reduction, 1e-9)
  )

country_typology <- country_core %>%
  group_by(year) %>%
  mutate(
    mitigation_cut = median(mitigation_improvement, na.rm = TRUE),
    side_cut = median(land_burden_index, na.rm = TRUE),
    typology = case_when(
      mitigation_improvement >= mitigation_cut & land_burden_index <= side_cut ~ "Priority countries",
      mitigation_improvement >= mitigation_cut & land_burden_index > side_cut ~ "High-potential but sensitive",
      mitigation_improvement < mitigation_cut & land_burden_index <= side_cut ~ "Low-potential stable",
      TRUE ~ "Support-first countries"
    )
  ) %>%
  ungroup()

country_share <- country_core %>%
  filter(is.finite(mitigation_improvement), mitigation_improvement > 0) %>%
  group_by(year) %>%
  arrange(desc(mitigation_improvement), .by_group = TRUE) %>%
  mutate(
    world_total = sum(mitigation_improvement, na.rm = TRUE),
    mitigation_share = mitigation_improvement / pmax(world_total, 1e-9),
    cum_share = cumsum(mitigation_share),
    cum_country_share = row_number() / n()
  ) %>%
  ungroup()

concentration_summary <- country_share %>%
  group_by(year) %>%
  summarise(
    share_top5 = sum(mitigation_share[concentration_rank <= 5], na.rm = TRUE),
    share_top10 = sum(mitigation_share[concentration_rank <= 10], na.rm = TRUE),
    share_top20 = sum(mitigation_share[concentration_rank <= 20], na.rm = TRUE),
    .groups = "drop"
  )

sector_share <- country_core %>%
  filter(year %in% c(2050, 2100)) %>%
  mutate(country = coalesce(country_name, region)) %>%
  select(region, country, year, ag_ch4_reduction, ag_n2o_reduction, land_co2_reduction) %>%
  pivot_longer(
    cols = c(ag_ch4_reduction, ag_n2o_reduction, land_co2_reduction),
    names_to = "component",
    values_to = "value"
  ) %>%
  mutate(component = recode(component,
    ag_ch4_reduction = "Agriculture CH4",
    ag_n2o_reduction = "Agriculture N2O",
    land_co2_reduction = "Land CO2"
  ))

readr::write_csv(country_core %>% filter(year == 2050), file.path(out_dir, "country_core_2050.csv"))
readr::write_csv(country_core %>% filter(year == 2100), file.path(out_dir, "country_core_2100.csv"))
readr::write_csv(country_typology %>% filter(year == 2050), file.path(out_dir, "country_typology_2050.csv"))
readr::write_csv(country_typology %>% filter(year == 2100), file.path(out_dir, "country_typology_2100.csv"))
readr::write_csv(concentration_summary, file.path(out_dir, "country_concentration_summary.csv"))

p1_map <- country_sf %>%
  left_join(country_core %>% filter(year == 2050) %>% select(region, mitigation_improvement), by = "region") %>%
  ggplot() +
  geom_sf(aes(fill = mitigation_improvement), color = NA) +
  scale_fill_gradient(low = "grey95", high = "#0B7285", labels = label_number(accuracy = 1)) +
  labs(
    title = "Map: Mergedcomparison.gdx / IAMCTemp / variable = Emi_CO2_AFO / unit = Mt CO2/yr / year = 2050",
    subtitle = "Value plotted = SSP2_BaU_NoCC_No - SSP2_1100C_CACN_DAC_NoCC_LandInt_global1",
    fill = "Baseline - LandInt"
    ,
    caption = "Baseline = SSP2_BaU_NoCC_No; LandInt = SSP2_1100C_CACN_DAC_NoCC_LandInt_global1"
  ) +
  theme_geer +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank())
save_plot(p1_map, "fig01_map_mitigation_potential_2050.png", width = 12, height = 6.5)

lorenz_df <- country_share %>% filter(year %in% c(2050, 2100))
p2 <- ggplot(lorenz_df, aes(x = cum_country_share, y = cum_share, color = factor(year))) +
  geom_line(linewidth = 1.1) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey60") +
  scale_color_manual(values = c("2050" = "#0B7285", "2100" = "#B02A37")) +
  scale_x_continuous(labels = percent_format()) +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "Concentration curve: country ranking by mitigation_improvement",
    subtitle = "Source = Mergedcomparison.gdx / IAMCTemp / variable = Emi_CO2_AFO / unit = Mt CO2/yr / mitigation_improvement = SSP2_BaU_NoCC_No - SSP2_1100C_CACN_DAC_NoCC_LandInt_global1",
    x = "Cumulative share of countries ranked by mitigation_improvement",
    y = "Cumulative share of global mitigation_improvement"
    ,
    caption = "Years shown: 2050 and 2100"
  ) +
  theme_geer
save_plot(p2, "fig02_concentration_curve_2050_2100.png")

topn_df <- concentration_summary %>%
  pivot_longer(cols = starts_with("share_"), names_to = "metric", values_to = "share") %>%
  mutate(metric = recode(metric, share_top5 = "Top 5", share_top10 = "Top 10", share_top20 = "Top 20"))
p3 <- ggplot(topn_df, aes(x = metric, y = share, fill = factor(year))) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("2050" = "#0B7285", "2100" = "#B02A37")) +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "Top-N concentration: share of global mitigation_improvement held by top countries",
    subtitle = "Source = Mergedcomparison.gdx / IAMCTemp / variable = Emi_CO2_AFO / unit = Mt CO2/yr / top-N based on SSP2_BaU_NoCC_No - SSP2_1100C_CACN_DAC_NoCC_LandInt_global1",
    x = "Top-N country group",
    y = "Share of global mitigation_improvement"
    ,
    caption = "Years shown: 2050 and 2100"
  ) +
  theme_geer
save_plot(p3, "fig03_topn_share_comparison.png", width = 8, height = 6)

# fig04: compare mitigation magnitude in Mergedcomparison against AFOLUB cost.
# This is a direct country-level heterogeneity view using original variables:
# Emi_CO2_AFO from IAMCTemp and Red_Cos_AFO_Agr from IAMC_templatePlus.
scatter_2050 <- country_typology %>% filter(year == 2050)
label_2050 <- scatter_2050 %>% slice_max(order_by = mitigation_improvement, n = 12, with_ties = FALSE)

p4 <- ggplot(scatter_2050, aes(x = cost_landint, y = mitigation_improvement, color = typology)) +
  geom_point(alpha = 0.75, size = 2.2) +
  geom_text(data = label_2050, aes(label = region), size = 3, nudge_y = 0.02 * max(scatter_2050$mitigation_improvement, na.rm = TRUE), show.legend = FALSE) +
  scale_color_manual(values = typology_palette) +
  scale_x_continuous(labels = label_comma()) +
  scale_y_continuous(labels = label_comma()) +
  labs(
    title = "Scatter (2050): AFOLUB cost vs baseline-to-LandInt AFOLU CO2 improvement",
    subtitle = "x = IAMC_templatePlus.gdx / IAMC_templatePlus / VEMF = Red_Cos_AFO_Agr / Scenario = SSP2_1100C_CACN_DAC_NoCC_LandInt_global1; y = Mergedcomparison.gdx / IAMCTemp / variable = Emi_CO2_AFO / unit = Mt CO2/yr / SSP2_BaU_NoCC_No - SSP2_1100C_CACN_DAC_NoCC_LandInt_global1",
    x = "Red_Cos_AFO_Agr [IAMC_templatePlus, LandInt]",
    y = "Emi_CO2_AFO [Baseline - LandInt]"
    ,
    caption = "LandInt = SSP2_1100C_CACN_DAC_NoCC_LandInt_global1; Baseline = SSP2_BaU_NoCC_No"
  ) +
  theme_geer
save_plot(p4, "fig04_scatter_mitigation_vs_cost_2050.png")

# fig05: compare land-cover forest change against mitigation improvement.
# Both axes are simple scenario differences using original outputs.
p5 <- ggplot(scatter_2050, aes(x = forest_gain, y = mitigation_improvement, color = typology, size = policy_attractiveness)) +
  geom_point(alpha = 0.8) +
  geom_text(data = label_2050, aes(label = region), size = 2.6, nudge_y = 0.02 * max(scatter_2050$mitigation_improvement, na.rm = TRUE), show.legend = FALSE) +
  scale_color_manual(values = typology_palette) +
  scale_size_continuous(range = c(1.8, 5.5)) +
  scale_x_continuous(labels = label_comma()) +
  scale_y_continuous(labels = label_comma()) +
  labs(
    title = "Scatter (2050): Mergedcomparison.gdx / IAMCTempwoU / Lan_Cov_Frs [LandInt - Baseline] vs Mergedcomparison.gdx / IAMCTemp / Emi_CO2_AFO [Baseline - LandInt]",
    subtitle = "x = Lan_Cov_Frs_landint - Lan_Cov_Frs_baseline / unit = Mha; y = Emi_CO2_AFO_baseline - Emi_CO2_AFO_landint / unit = Mt CO2/yr; scenarios = SSP2_1100C_CACN_DAC_NoCC_LandInt_global1 vs SSP2_BaU_NoCC_No",
    x = "Lan_Cov_Frs_landint - Lan_Cov_Frs_baseline [Mha]",
    y = "Emi_CO2_AFO_baseline - Emi_CO2_AFO_landint [Mt CO2/yr]",
    caption = "Bubble size = policy_attractiveness; color = country typology"
  ) +
  theme_geer
save_plot(p5, "fig05_scatter_forest_vs_mitigation_2050.png")

# fig06: compare LULUCF sink improvement with net-emissions improvement using
# the original GHG symbol from Mergedcomparison.
p6 <- ggplot(scatter_2050, aes(x = sink_improvement, y = net_emissions_improvement, color = typology, size = mitigation_improvement)) +
  geom_point(alpha = 0.8) +
  geom_text(data = label_2050, aes(label = region), size = 2.6, nudge_y = 0.02 * max(scatter_2050$net_emissions_improvement, na.rm = TRUE), show.legend = FALSE) +
  scale_color_manual(values = typology_palette) +
  scale_size_continuous(range = c(1.8, 5.5)) +
  scale_x_continuous(labels = label_comma()) +
  scale_y_continuous(labels = label_comma()) +
  labs(
    title = "Scatter (2050): Mergedcomparison.gdx / GHG / component = Sink vs Net_emissions / improvement = Baseline - LandInt",
    subtitle = "x = sink_baseline - sink_landint; y = net_emissions_baseline - net_emissions_landint; source symbol = GHG; model dimension = LUM; scenarios = SSP2_BaU_NoCC_No and SSP2_1100C_CACN_DAC_NoCC_LandInt_global1",
    x = "GHG Sink improvement = baseline - landint",
    y = "GHG Net_emissions improvement = baseline - landint",
    caption = "Bubble size = mitigation_improvement from IAMCTemp / Emi_CO2_AFO"
  ) +
  theme_geer
save_plot(p6, "fig06_scatter_sink_vs_netemissions_2050.png")

p7_map <- country_sf %>%
  left_join(country_typology %>% filter(year == 2050) %>% select(region, policy_attractiveness), by = "region") %>%
  ggplot() +
  geom_sf(aes(fill = policy_attractiveness), color = NA) +
  scale_fill_gradient(low = "#B02A37", high = "#0B7285", labels = label_number(accuracy = 0.01)) +
  labs(
    title = "Map (2050): policy_attractiveness composite index",
    subtitle = "policy_attractiveness = mean[ mitigation_score(Emi_CO2_AFO baseline-landint), forest_score(Lan_Cov_Frs landint-baseline), sink_score(GHG Sink baseline-landint), net_emissions_score(GHG Net_emissions baseline-landint), 1-cost_score(Red_Cos_AFO_Agr landint), 1-land_burden_index ]",
    fill = "policy_attractiveness"
    ,
    caption = "Sources: Mergedcomparison.gdx / IAMCTemp and IAMC_templatePlus.gdx / IAMC_templatePlus"
  ) +
  theme_geer +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank())
save_plot(p7_map, "fig07_policy_attractiveness_map_2050.png", width = 12, height = 6.5)

p7b_map <- country_sf %>%
  left_join(country_core %>% filter(year == 2050) %>% select(region, net_emissions_improvement), by = "region") %>%
  ggplot() +
  geom_sf(aes(fill = net_emissions_improvement), color = NA) +
  scale_fill_gradient2(low = "#B02A37", mid = "white", high = "#0B7285", midpoint = 0, labels = label_comma()) +
  labs(
    title = "Map (2050): Mergedcomparison.gdx / GHG / component = Net_emissions / improvement = Baseline - LandInt",
    subtitle = "Value plotted = net_emissions_baseline - net_emissions_landint / scenarios = SSP2_BaU_NoCC_No and SSP2_1100C_CACN_DAC_NoCC_LandInt_global1",
    fill = "Net_emissions\nbaseline - landint",
    caption = "Source symbol = GHG; model dimension = LUM"
  ) +
  theme_geer +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank())
save_plot(p7b_map, "fig07b_map_net_emissions_improvement_2050.png", width = 12, height = 6.5)

# fig08-09: typology is only a sorting device for exploration.
# The axes themselves remain original variables or simple changes.
make_typology_plot <- function(df, year_value, filename) {
  d <- df %>% filter(year == year_value)
  lab <- d %>% slice_max(order_by = mitigation_improvement, n = 14, with_ties = FALSE)
  p <- ggplot(d, aes(x = land_burden_index, y = mitigation_improvement, color = typology)) +
    geom_vline(xintercept = median(d$land_burden_index, na.rm = TRUE), linetype = "dashed", color = "grey60") +
    geom_hline(yintercept = median(d$mitigation_improvement, na.rm = TRUE), linetype = "dashed", color = "grey60") +
    geom_point(alpha = 0.8, size = 2.4) +
    geom_text(data = lab, aes(label = region), size = 3, nudge_y = 0.02 * max(d$mitigation_improvement, na.rm = TRUE), show.legend = FALSE) +
    scale_color_manual(values = typology_palette) +
    scale_y_continuous(labels = label_comma()) +
    labs(
      title = paste0("Typology quadrant (", year_value, "): mitigation_improvement vs land_burden_index"),
      subtitle = "x = land_burden_index = scaled mean of pmax(-cropland_change,0), pmax(-pasture_change,0), pmax(-frs_change_landint_vs_baseline,0); y = Mergedcomparison.gdx / IAMCTemp / Emi_CO2_AFO_baseline - Emi_CO2_AFO_landint",
      x = "land_burden_index [0-1 composite]",
      y = "Emi_CO2_AFO_baseline - Emi_CO2_AFO_landint [Mt CO2/yr]",
      caption = "Cropland and pasture use Lan_Cov_Cro, Lan_Cov_Pst from IAMCTempwoU; forest area uses AREA / land_code = FRS"
    ) +
    theme_geer
  save_plot(p, filename)
}

make_typology_plot(country_typology, 2050, "fig08_typology_quadrant_2050.png")
make_typology_plot(country_typology, 2100, "fig09_typology_quadrant_2100.png")

top_portfolio <- sector_share %>%
  left_join(country_core %>% filter(year == 2050) %>% select(region, mitigation_improvement), by = "region") %>%
  filter(year == 2050) %>%
  group_by(region) %>%
  mutate(total_mitigation = first(mitigation_improvement)) %>%
  ungroup() %>%
  slice_max(order_by = total_mitigation, n = 12, with_ties = FALSE) %>%
  mutate(country = fct_reorder(country, total_mitigation))

p10 <- ggplot(top_portfolio, aes(x = country, y = value, fill = component)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c("Agriculture CH4" = "#B86A2D", "Agriculture N2O" = "#6A4C93", "Land CO2" = "#0B7285")) +
  scale_y_continuous(labels = label_comma()) +
  labs(
    title = "Top countries by mitigation_improvement with AFOLUB component breakdown (2050)",
    subtitle = "Bars use IAMC_templatePlus.gdx / IAMC_templatePlus / LandInt scenario / VEMF = Emi_Red_CH4_AFO_Agr, Emi_Red_N2O_AFO_Agr, Emi_Red_CO2_AFO_Lan",
    x = "Country",
    y = "Reduction component value"
    ,
    caption = "Country ranking is based on Mergedcomparison / IAMCTemp / Emi_CO2_AFO(Baseline - LandInt)"
  ) +
  theme_geer
save_plot(p10, "fig10_top_countries_portfolio_2050.png", width = 11, height = 8)

heatmap_2050 <- country_typology %>%
  filter(year == 2050) %>%
  slice_max(order_by = mitigation_improvement, n = 15, with_ties = FALSE) %>%
  transmute(
    country = coalesce(country_name, region),
    mitigation = mitigation_improvement,
    cost = cost_landint,
    forest = forest_gain,
    sink = sink_improvement,
    net_emissions = net_emissions_improvement,
    cropland = cropland_change,
    pasture = pasture_change,
    attractiveness = policy_attractiveness
  ) %>%
  pivot_longer(-country, names_to = "metric", values_to = "value") %>%
  group_by(metric) %>%
  mutate(z = scale01(value)) %>%
  ungroup()

p11 <- ggplot(heatmap_2050, aes(x = metric, y = fct_reorder(country, z, .fun = sum, .desc = TRUE), fill = z)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "#0B7285") +
  labs(
    title = "Heatmap (2050): standardized metrics for top mitigation countries",
    subtitle = "metrics = mitigation[Emi_CO2_AFO Baseline-LandInt], cost[Red_Cos_AFO_Agr LandInt], forest[Lan_Cov_Frs LandInt-Baseline], sink[GHG Sink Baseline-LandInt], net_emissions[GHG Net_emissions Baseline-LandInt], cropland[Lan_Cov_Cro LandInt-Baseline], pasture[Lan_Cov_Pst LandInt-Baseline], attractiveness[composite]",
    x = "Metric",
    y = "Country",
    caption = "Standardization uses min-max scaling within each metric"
  ) +
  theme_geer +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
save_plot(p11, "fig11_top_countries_heatmap_2050.png", width = 11, height = 8)

residual_2100 <- country_core %>%
  filter(year == 2100) %>%
  mutate(country = coalesce(country_name, region)) %>%
  select(country, residual_ch4, residual_n2o) %>%
  pivot_longer(-country, names_to = "component", values_to = "value") %>%
  group_by(country) %>%
  mutate(total = sum(value, na.rm = TRUE)) %>%
  ungroup() %>%
  slice_max(order_by = total, n = 12, with_ties = FALSE) %>%
  mutate(
    country = fct_reorder(country, total),
    component = recode(component, residual_ch4 = "Residual CH4", residual_n2o = "Residual N2O")
  )

p12 <- ggplot(residual_2100, aes(x = country, y = value, fill = component)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c("Residual CH4" = "#B86A2D", "Residual N2O" = "#6A4C93")) +
  scale_y_continuous(labels = label_comma()) +
  labs(
    title = "Residual agricultural emission indicators (2100, LandInt scenario)",
    subtitle = "Source = IAMC_templatePlus.gdx / IAMC_templatePlus / VEMF = Emi_equ_CH4_AFO_Agr and Emi_equ_N2O_AFO_Agr / Scenario = SSP2_1100C_CACN_DAC_NoCC_LandInt_global1",
    x = "Country",
    y = "Residual emission indicator value"
  ) +
  theme_geer
save_plot(p12, "fig12_residual_emissions_2100.png", width = 10, height = 7.5)

# fig13: inspect which countries show the largest land-system shifts across
# original Mergedcomparison variables, not model-derived new metrics.
land_system_df <- country_core %>%
  filter(year == 2050) %>%
  transmute(
    region,
    net_emissions_improvement,
    positive_flow_improvement,
    negative_seq_improvement,
    land_burden_index,
    structure_shift_index
  ) %>%
  pivot_longer(-region, names_to = "metric", values_to = "value") %>%
  group_by(metric) %>%
  slice_max(order_by = abs(value), n = 12, with_ties = FALSE) %>%
  ungroup()

p13 <- ggplot(land_system_df, aes(x = value, y = fct_reorder(region, value), fill = metric)) +
  geom_col() +
  facet_wrap(~ metric, scales = "free_x") +
  labs(
    title = "Largest absolute country-level land-system changes (2050)",
    subtitle = "Panels use baseline vs LandInt only: net_emissions_improvement[GHG], positive_flow_improvement[Emi_CO2_Lan_Use_Flo_Pos_Emi], negative_seq_improvement[Emi_CO2_Lan_Use_Flo_Neg_Seq], land_burden_index, structure_shift_index",
    x = "Metric value",
    y = "Country",
    caption = "Sources = Mergedcomparison.gdx / GHG, IAMCTemp, IAMCTempwoU, AREA"
  ) +
  theme_geer
save_plot(p13, "fig13_land_system_changes_2050.png", width = 12, height = 8)

# fig14: inspect direct AREA changes by land_code for top mitigation countries.
top_area_regions <- country_core %>%
  filter(year == 2050) %>%
  slice_max(order_by = mitigation_improvement, n = 15, with_ties = FALSE) %>%
  select(region, country_name, mitigation_improvement)

area_heatmap <- area_wide %>%
  filter(year == 2050, land_code %in% c("FRS", "CL", "PAS", "PRMFRS", "SECFRS")) %>%
  semi_join(top_area_regions, by = "region") %>%
  left_join(top_area_regions, by = "region") %>%
  mutate(
    land_code = recode(land_code,
      FRS = "AREA land_code = FRS",
      CL = "AREA land_code = CL",
      PAS = "AREA land_code = PAS",
      PRMFRS = "AREA land_code = PRMFRS",
      SECFRS = "AREA land_code = SECFRS"
    ),
    country = coalesce(country_name, region)
  )

p14 <- ggplot(area_heatmap, aes(
  x = land_code,
  y = fct_reorder(country, mitigation_improvement),
  fill = change_landint_vs_baseline
)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "#B02A37", mid = "white", high = "#0B7285", midpoint = 0) +
  labs(
    title = "AREA heatmap (2050): selected land_code area changes for top mitigation countries",
    subtitle = "Source = Mergedcomparison.gdx / symbol = AREA / model = LUM / fill = landint - baseline; countries ranked by IAMCTemp / Emi_CO2_AFO_baseline - Emi_CO2_AFO_landint",
    x = "AREA land_code",
    y = "Country",
    fill = "AREA\nlandint - baseline",
    caption = "Scenarios = SSP2_1100C_CACN_DAC_NoCC_LandInt_global1 and SSP2_BaU_NoCC_No"
  ) +
  theme_geer +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
save_plot(p14, "fig14_area_heatmap_2050.png", width = 11, height = 8)

# fig15: show which GHG components drive the 2100 country-level improvements.
ghg_component_2100 <- ghg_wide %>%
  filter(year == 2100, region != "World") %>%
  left_join(country_core %>% filter(year == 2100) %>% select(region, country_name, mitigation_improvement), by = "region") %>%
  group_by(region) %>%
  mutate(total_mitigation = first(mitigation_improvement)) %>%
  ungroup() %>%
  slice_max(order_by = total_mitigation, n = 12, with_ties = FALSE) %>%
  mutate(
    country = coalesce(country_name, region),
    component = recode(component,
      sink = "GHG component = Sink",
      emissions = "GHG component = Emissions",
      net_emissions = "GHG component = Net_emissions"
    ),
    country = fct_reorder(country, total_mitigation)
  )

p15 <- ggplot(ghg_component_2100, aes(x = country, y = improvement, fill = component)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = label_comma()) +
  labs(
    title = "GHG component improvements for top mitigation countries (2100)",
    subtitle = "Source = Mergedcomparison.gdx / symbol = GHG / improvement = baseline - landint / components = Sink, Emissions, Net_emissions / country ranking by IAMCTemp / Emi_CO2_AFO improvement",
    x = "Country",
    y = "GHG component improvement = baseline - landint",
    caption = "Scenarios = SSP2_BaU_NoCC_No and SSP2_1100C_CACN_DAC_NoCC_LandInt_global1"
  ) +
  theme_geer
save_plot(p15, "fig15_ghg_component_improvement_2100.png", width = 11, height = 8)

# fig16: side-by-side top-country ranking across original variables to help
# narrow the paper direction without relying on a single preferred metric.
top_indicator_df <- bind_rows(
  country_core %>% filter(year == 2050) %>% transmute(region, country = coalesce(country_name, region), indicator = "Mitigation: Emi_CO2_AFO baseline - landint", value = mitigation_improvement),
  country_core %>% filter(year == 2050) %>% transmute(region, country = coalesce(country_name, region), indicator = "Forest gain: Lan_Cov_Frs landint - baseline", value = forest_gain),
  country_core %>% filter(year == 2050) %>% transmute(region, country = coalesce(country_name, region), indicator = "Sink improvement: GHG Sink baseline - landint", value = sink_improvement),
  country_core %>% filter(year == 2050) %>% transmute(region, country = coalesce(country_name, region), indicator = "Net_emissions improvement: GHG Net_emissions baseline - landint", value = net_emissions_improvement),
  country_core %>% filter(year == 2050) %>% transmute(region, country = coalesce(country_name, region), indicator = "Cost: Red_Cos_AFO_Agr landint", value = cost_landint),
  country_core %>% filter(year == 2050) %>% transmute(region, country = coalesce(country_name, region), indicator = "Land burden index", value = land_burden_index)
) %>%
  group_by(indicator) %>%
  slice_max(order_by = abs(value), n = 12, with_ties = FALSE) %>%
  ungroup()

p16 <- ggplot(top_indicator_df, aes(x = value, y = fct_reorder(country, value), fill = indicator)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ indicator, scales = "free_x") +
  labs(
    title = "Top countries by selected indicators (2050)",
    subtitle = "Indicators mix Mergedcomparison.gdx / IAMCTemp, IAMCTempwoU, GHG and IAMC_templatePlus.gdx / IAMC_templatePlus; all values are country-level metrics used in the typology exploration",
    x = "Indicator value",
    y = "Country",
    caption = "Baseline = SSP2_BaU_NoCC_No; LandInt = SSP2_1100C_CACN_DAC_NoCC_LandInt_global1"
  ) +
  theme_geer
save_plot(p16, "fig16_top_countries_by_indicator_2050.png", width = 13, height = 9)

message("Completed 2nd_check exploratory analysis.")
