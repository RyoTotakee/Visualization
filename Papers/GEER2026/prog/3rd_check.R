library(tidyverse)
library(gdxrrw)
library(patchwork)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(scales)

sf_use_s2(FALSE)

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
out_dir <- file.path(base_dir, "output", "0311", "3rd_check")
country_set_file <- file.path(base_dir, "define", "country.set")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
if (!dir.exists(out_dir)) {
  stop("Output directory is not available: ", out_dir)
}

scenario_baseline <- "SSP2_BaU_NoCC_No"
scenario_landint <- "SSP2_1100C_CACN_DAC_NoCC_LandInt_global1"

theme_geer <- theme_bw(base_size = 9) +
  theme(
    panel.grid.major = element_line(color = "grey90", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(size = 6.4, lineheight = 0.92),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title = element_text(size = 9.2),
    plot.subtitle = element_text(size = 7.3),
    plot.caption = element_text(size = 6.8),
    axis.text = element_text(size = 7.5),
    axis.title = element_text(size = 8),
    legend.text = element_text(size = 7.5)
  )

save_plot <- function(plot_obj, filename, width = 10, height = 7) {
  out_file <- file.path(out_dir, filename)
  ggsave(out_file, plot = plot_obj, width = width, height = height, dpi = 260, bg = "white")
  message("Saved: ", out_file)
}

safe_pull <- function(df, name) {
  if (name %in% names(df)) {
    return(df[[name]])
  }
  rep(NA_real_, nrow(df))
}

top_non_missing <- function(df, column, n = 12, desc = TRUE) {
  if (!column %in% names(df)) {
    return(df[0, , drop = FALSE])
  }
  d <- df %>% filter(is.finite(.data[[column]]))
  if (nrow(d) == 0) {
    return(d)
  }
  if (desc) {
    d %>% slice_max(order_by = .data[[column]], n = n, with_ties = FALSE)
  } else {
    d %>% slice_min(order_by = .data[[column]], n = n, with_ties = FALSE)
  }
}

reorder_within_local <- function(x, by, within, fun = mean, sep = "___", ...) {
  stats::reorder(paste(x, within, sep = sep), by, FUN = fun)
}

scale_y_reordered_local <- function(..., sep = "___") {
  scale_y_discrete(labels = function(x) gsub(paste0(sep, ".+$"), "", x), ...)
}

country_sf <- ne_countries(scale = "medium", returnclass = "sf") %>%
  st_make_valid() %>%
  transmute(region = iso_a3, country_name = admin) %>%
  filter(region != "-99")

country_area_tbl <- country_sf %>%
  st_transform(6933) %>%
  mutate(country_area_mha = as.numeric(st_area(geometry)) / 1e10) %>%
  st_drop_geometry() %>%
  select(region, country_area_mha)

country_codes <- if (file.exists(country_set_file)) {
  scan(country_set_file, what = character(), quiet = TRUE)
} else {
  stop("country.set is not available: ", country_set_file)
}

analysis_regions <- c("World", sort(unique(country_codes)))
analysis_years <- c(2030, 2050, 2100)

# Use AGMIP_all_scenario.gdx / AGMIP for market, food, trade, production, land,
# and agricultural-emissions variables. Read only selected variables/items/units.
read_agmip <- function(variables, items, units, regions, years, scenarios) {
  req <- list(
    name = "AGMIP",
    form = "full",
    uels = list(scenarios, regions, variables, items, units, as.character(years))
  )
  x <- rgdx(file.path(data_dir, "AGMIP_all_scenario.gdx"), req, squeeze = FALSE)
  as.data.frame.table(x$val, responseName = "value", stringsAsFactors = FALSE) %>%
    setNames(c("scenario", "region", "variable", "item", "unit", "year", "value")) %>%
    mutate(
      year = as.integer(year),
      value = as.numeric(value)
    )
}

# Use IAMC_templatePlus.gdx / IAMC_templatePlus only for AFOLUB-side original
# variables such as reduction cost, mitigation amount, and residual emissions.
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

# Use Mergedcomparison.gdx / IAMCTemp for IAMC-style variables with unit strings.
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
    )
}

# Use Mergedcomparison.gdx / IAMCTempwoU for land-use variables where the no-unit
# symbol is easier to handle than IAMCTemp.
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
    )
}

# Use Mergedcomparison.gdx / GHG for LULUCF-side emissions, sinks, and net
# emissions. This is the preferred source for LULUCF emission/sink interpretation.
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
    )
}

# Use Mergedcomparison.gdx / AREA for direct land-code area changes.
read_area <- function(regions, years, scenarios) {
  req <- list(
    name = "AREA",
    form = "full",
    uels = list(scenarios, regions, as.character(years), c("FRS", "CL", "PAS", "PRMFRS", "SECFRS"), c("LUM"))
  )
  x <- rgdx(file.path(data_dir, "Mergedcomparison.gdx"), req, squeeze = FALSE)
  as.data.frame.table(x$val, responseName = "value", stringsAsFactors = FALSE) %>%
    setNames(c("scenario", "region", "year", "land_code", "model", "value")) %>%
    mutate(
      year = as.integer(year),
      value = as.numeric(value)
    )
}

agmip_vars <- c("CALI", "XPRP", "PROD", "NETT", "LAND")
agmip_items <- c("TOT", "AGR", "CRP", "LSP", "FOR", "GRS")
agmip_units <- c("kcal/cap/d", "USD/t", "1000 t", "1000 ha")

agmip_emis_vars <- c("EMIS")
agmip_emis_items <- c("tot", "ent", "mmg", "rcc", "sfr", "mas", "mgr", "crs", "bcr")
agmip_emis_units <- c("MtCO2e")

agmip_specs <- tribble(
  ~variable, ~item, ~unit,
  "CALI", "TOT", "kcal/cap/d",
  "XPRP", "AGR", "USD/t",
  "XPRP", "CRP", "USD/t",
  "XPRP", "LSP", "USD/t",
  "PROD", "AGR", "1000 t",
  "PROD", "CRP", "1000 t",
  "PROD", "LSP", "1000 t",
  "NETT", "AGR", "1000 t",
  "NETT", "CRP", "1000 t",
  "NETT", "LSP", "1000 t",
  "LAND", "AGR", "1000 ha",
  "LAND", "CRP", "1000 ha",
  "LAND", "GRS", "1000 ha",
  "LAND", "FOR", "1000 ha",
  "EMIS", "tot", "MtCO2e",
  "EMIS", "ent", "MtCO2e",
  "EMIS", "mmg", "MtCO2e",
  "EMIS", "rcc", "MtCO2e",
  "EMIS", "sfr", "MtCO2e",
  "EMIS", "mas", "MtCO2e",
  "EMIS", "mgr", "MtCO2e",
  "EMIS", "crs", "MtCO2e",
  "EMIS", "bcr", "MtCO2e"
)

iamcplus_vars <- c(
  "Red_Cos_AFO_Agr",
  "Emi_Red_CH4_AFO_Agr",
  "Emi_Red_N2O_AFO_Agr",
  "Emi_Red_CO2_AFO_Lan",
  "Emi_equ_CH4_AFO_Agr",
  "Emi_equ_N2O_AFO_Agr"
)

merged_vars <- c("Emi_CO2_AFO")
merged_units <- c("Mt CO2/yr")

merged_wou_vars <- c(
  "Lan_Cov_Frs",
  "Lan_Cov_Cro",
  "Lan_Cov_Pst",
  "Emi_CO2_AFO_Agr",
  "Emi_CO2_AFO_Lan",
  "Emi_CO2_Lan_Use_Flo_Pos_Emi",
  "Emi_CO2_Lan_Use_Flo_Neg_Seq",
  "Emi_CO2_AFO_Def",
  "Emi_CO2_AFO_Aff",
  "Emi_CO2_AFO_For_Man"
)

agmip_df <- read_agmip(
  variables = agmip_vars,
  items = agmip_items,
  units = agmip_units,
  regions = analysis_regions,
  years = analysis_years,
  scenarios = c(scenario_baseline, scenario_landint)
) %>%
  semi_join(agmip_specs, by = c("variable", "item", "unit"))

agmip_emis_df <- read_agmip(
  variables = agmip_emis_vars,
  items = agmip_emis_items,
  units = agmip_emis_units,
  regions = analysis_regions,
  years = analysis_years,
  scenarios = c(scenario_baseline, scenario_landint)
) %>%
  semi_join(agmip_specs, by = c("variable", "item", "unit"))

agmip_df <- bind_rows(agmip_df, agmip_emis_df)

plus_df <- read_iamcplus(
  variables = iamcplus_vars,
  regions = analysis_regions,
  years = analysis_years,
  scenarios = c(scenario_landint)
)

merged_df <- read_merged(
  variables = merged_vars,
  units = merged_units,
  regions = analysis_regions,
  years = analysis_years,
  scenarios = c(scenario_baseline, scenario_landint)
)

merged_wou_df <- read_merged_wou(
  variables = merged_wou_vars,
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

message("AGMIP rows: ", nrow(agmip_df))
message("IAMC_templatePlus rows: ", nrow(plus_df))
message("IAMCTemp rows: ", nrow(merged_df))
message("IAMCTempwoU rows: ", nrow(merged_wou_df))
message("GHG rows: ", nrow(ghg_df))
message("AREA rows: ", nrow(area_df))

agmip_wide <- agmip_df %>%
  pivot_wider(names_from = scenario, values_from = value) %>%
  mutate(
    baseline = .data[[scenario_baseline]],
    landint = .data[[scenario_landint]],
    change_landint_vs_baseline = landint - baseline,
    improvement_baseline_vs_landint = baseline - landint
  ) %>%
  select(region, year, variable, item, unit, baseline, landint, change_landint_vs_baseline, improvement_baseline_vs_landint)

plus_wide <- plus_df %>%
  rename(value_landint = value) %>%
  select(region, year, variable, value_landint)

merged_wide <- merged_df %>%
  pivot_wider(names_from = scenario, values_from = value) %>%
  mutate(
    baseline = .data[[scenario_baseline]],
    landint = .data[[scenario_landint]],
    change_landint_vs_baseline = landint - baseline,
    improvement_baseline_vs_landint = baseline - landint
  ) %>%
  select(region, year, variable, unit, baseline, landint, change_landint_vs_baseline, improvement_baseline_vs_landint)

merged_wou_wide <- merged_wou_df %>%
  pivot_wider(names_from = scenario, values_from = value) %>%
  mutate(
    baseline = .data[[scenario_baseline]],
    landint = .data[[scenario_landint]],
    change_landint_vs_baseline = landint - baseline,
    improvement_baseline_vs_landint = baseline - landint
  ) %>%
  select(region, year, variable, baseline, landint, change_landint_vs_baseline, improvement_baseline_vs_landint)

ghg_wide <- ghg_df %>%
  pivot_wider(names_from = scenario, values_from = value) %>%
  mutate(
    baseline = .data[[scenario_baseline]],
    landint = .data[[scenario_landint]],
    change_landint_vs_baseline = landint - baseline,
    improvement_baseline_vs_landint = baseline - landint,
    component = recode(component,
      Sink = "sink",
      Emissions = "emissions",
      Net_emissions = "net_emissions"
    )
  ) %>%
  select(region, year, component, baseline, landint, change_landint_vs_baseline, improvement_baseline_vs_landint)

area_wide <- area_df %>%
  pivot_wider(names_from = scenario, values_from = value) %>%
  mutate(
    baseline = .data[[scenario_baseline]],
    landint = .data[[scenario_landint]],
    change_landint_vs_baseline = landint - baseline,
    improvement_baseline_vs_landint = baseline - landint
  ) %>%
  select(region, year, land_code, baseline, landint, change_landint_vs_baseline, improvement_baseline_vs_landint)

agmip_country <- agmip_wide %>%
  filter(region != "World") %>%
  mutate(key = paste(variable, item, unit, sep = "__")) %>%
  select(region, year, key, baseline, landint, change_landint_vs_baseline, improvement_baseline_vs_landint) %>%
  pivot_wider(
    names_from = key,
    values_from = c(baseline, landint, change_landint_vs_baseline, improvement_baseline_vs_landint),
    names_glue = "{key}_{.value}",
    values_fn = dplyr::first
  )

plus_country <- plus_wide %>%
  filter(region != "World") %>%
  pivot_wider(
    names_from = variable,
    values_from = value_landint,
    names_glue = "{variable}_value_landint"
  )

merged_country <- merged_wide %>%
  filter(region != "World") %>%
  mutate(key = paste(variable, unit, sep = "__")) %>%
  select(region, year, key, baseline, landint, change_landint_vs_baseline, improvement_baseline_vs_landint) %>%
  pivot_wider(
    names_from = key,
    values_from = c(baseline, landint, change_landint_vs_baseline, improvement_baseline_vs_landint),
    names_glue = "{key}_{.value}"
  )

merged_wou_country <- merged_wou_wide %>%
  filter(region != "World") %>%
  pivot_wider(
    names_from = variable,
    values_from = c(baseline, landint, change_landint_vs_baseline, improvement_baseline_vs_landint),
    names_glue = "{variable}_{.value}"
  )

ghg_country <- ghg_wide %>%
  filter(region != "World") %>%
  pivot_wider(
    names_from = component,
    values_from = c(baseline, landint, change_landint_vs_baseline, improvement_baseline_vs_landint),
    names_glue = "{component}_{.value}"
  )

area_country <- area_wide %>%
  filter(region != "World") %>%
  mutate(land_code = recode(land_code,
    FRS = "frs",
    CL = "cl",
    PAS = "pas",
    PRMFRS = "prmfrs",
    SECFRS = "secfrs"
  )) %>%
  pivot_wider(
    names_from = land_code,
    values_from = c(baseline, landint, change_landint_vs_baseline, improvement_baseline_vs_landint),
    names_glue = "{land_code}_{.value}"
  )

country_core <- agmip_country %>%
  left_join(plus_country, by = c("region", "year")) %>%
  left_join(merged_country, by = c("region", "year")) %>%
  left_join(merged_wou_country, by = c("region", "year")) %>%
  left_join(ghg_country, by = c("region", "year")) %>%
  left_join(area_country, by = c("region", "year")) %>%
  left_join(st_drop_geometry(country_sf), by = "region") %>%
  left_join(country_area_tbl, by = "region") %>%
  filter(region %in% country_codes) %>%
  mutate(
    mitigation_improvement = safe_pull(., "Emi_CO2_AFO__Mt CO2/yr_improvement_baseline_vs_landint"),
    forest_area_change_mha = safe_pull(., "Lan_Cov_Frs_change_landint_vs_baseline"),
    cropland_area_change_mha = safe_pull(., "Lan_Cov_Cro_change_landint_vs_baseline"),
    pasture_area_change_mha = safe_pull(., "Lan_Cov_Pst_change_landint_vs_baseline"),
    forest_area_change_pct_country = 100 * forest_area_change_mha / pmax(country_area_mha, 1e-12),
    cropland_area_change_pct_country = 100 * cropland_area_change_mha / pmax(country_area_mha, 1e-12),
    pasture_area_change_pct_country = 100 * pasture_area_change_mha / pmax(country_area_mha, 1e-12),
    ag_price_change = safe_pull(., "XPRP__AGR__USD/t_change_landint_vs_baseline"),
    crp_price_change = safe_pull(., "XPRP__CRP__USD/t_change_landint_vs_baseline"),
    lsp_price_change = safe_pull(., "XPRP__LSP__USD/t_change_landint_vs_baseline"),
    calorie_change = safe_pull(., "CALI__TOT__kcal/cap/d_change_landint_vs_baseline"),
    agr_prod_change = safe_pull(., "PROD__AGR__1000 t_change_landint_vs_baseline"),
    crp_prod_change = safe_pull(., "PROD__CRP__1000 t_change_landint_vs_baseline"),
    lsp_prod_change = safe_pull(., "PROD__LSP__1000 t_change_landint_vs_baseline"),
    agr_nett_change = safe_pull(., "NETT__AGR__1000 t_change_landint_vs_baseline"),
    crp_nett_change = safe_pull(., "NETT__CRP__1000 t_change_landint_vs_baseline"),
    lsp_nett_change = safe_pull(., "NETT__LSP__1000 t_change_landint_vs_baseline"),
    cropland_land_change_kha = safe_pull(., "LAND__CRP__1000 ha_change_landint_vs_baseline"),
    grassland_land_change_kha = safe_pull(., "LAND__GRS__1000 ha_change_landint_vs_baseline"),
    forest_land_change_kha = safe_pull(., "LAND__FOR__1000 ha_change_landint_vs_baseline"),
    ag_emis_total_change = rowSums(cbind(
      safe_pull(., "EMIS__ent__MtCO2e_change_landint_vs_baseline"),
      safe_pull(., "EMIS__mmg__MtCO2e_change_landint_vs_baseline"),
      safe_pull(., "EMIS__rcc__MtCO2e_change_landint_vs_baseline"),
      safe_pull(., "EMIS__sfr__MtCO2e_change_landint_vs_baseline"),
      safe_pull(., "EMIS__mas__MtCO2e_change_landint_vs_baseline"),
      safe_pull(., "EMIS__mgr__MtCO2e_change_landint_vs_baseline"),
      safe_pull(., "EMIS__crs__MtCO2e_change_landint_vs_baseline"),
      safe_pull(., "EMIS__bcr__MtCO2e_change_landint_vs_baseline")
    ), na.rm = TRUE),
    ag_emis_ent_change = safe_pull(., "EMIS__ent__MtCO2e_change_landint_vs_baseline"),
    ag_emis_mmg_change = safe_pull(., "EMIS__mmg__MtCO2e_change_landint_vs_baseline"),
    ag_emis_rcc_change = safe_pull(., "EMIS__rcc__MtCO2e_change_landint_vs_baseline"),
    ag_emis_sfr_change = safe_pull(., "EMIS__sfr__MtCO2e_change_landint_vs_baseline"),
    ag_emis_mas_change = safe_pull(., "EMIS__mas__MtCO2e_change_landint_vs_baseline"),
    ag_emis_mgr_change = safe_pull(., "EMIS__mgr__MtCO2e_change_landint_vs_baseline"),
    ag_emis_crs_change = safe_pull(., "EMIS__crs__MtCO2e_change_landint_vs_baseline"),
    ag_emis_bcr_change = safe_pull(., "EMIS__bcr__MtCO2e_change_landint_vs_baseline"),
    crop_emis_change = rowSums(cbind(
      safe_pull(., "EMIS__rcc__MtCO2e_change_landint_vs_baseline"),
      safe_pull(., "EMIS__sfr__MtCO2e_change_landint_vs_baseline"),
      safe_pull(., "EMIS__mas__MtCO2e_change_landint_vs_baseline"),
      safe_pull(., "EMIS__crs__MtCO2e_change_landint_vs_baseline"),
      safe_pull(., "EMIS__bcr__MtCO2e_change_landint_vs_baseline")
    ), na.rm = TRUE),
    livestock_emis_change = rowSums(cbind(
      safe_pull(., "EMIS__ent__MtCO2e_change_landint_vs_baseline"),
      safe_pull(., "EMIS__mmg__MtCO2e_change_landint_vs_baseline"),
      safe_pull(., "EMIS__mgr__MtCO2e_change_landint_vs_baseline")
    ), na.rm = TRUE),
    ag_co2_improvement = safe_pull(., "Emi_CO2_AFO_Agr_improvement_baseline_vs_landint"),
    land_co2_improvement = safe_pull(., "Emi_CO2_AFO_Lan_improvement_baseline_vs_landint"),
    sink_improvement = safe_pull(., "sink_improvement_baseline_vs_landint"),
    emissions_improvement = safe_pull(., "emissions_improvement_baseline_vs_landint"),
    net_emissions_improvement = safe_pull(., "net_emissions_improvement_baseline_vs_landint"),
    positive_flow_improvement = safe_pull(., "Emi_CO2_Lan_Use_Flo_Pos_Emi_improvement_baseline_vs_landint"),
    negative_seq_improvement = safe_pull(., "Emi_CO2_Lan_Use_Flo_Neg_Seq_improvement_baseline_vs_landint"),
    deforestation_improvement = safe_pull(., "Emi_CO2_AFO_Def_improvement_baseline_vs_landint"),
    afforestation_change = safe_pull(., "Emi_CO2_AFO_Aff_change_landint_vs_baseline"),
    forest_management_change = safe_pull(., "Emi_CO2_AFO_For_Man_change_landint_vs_baseline"),
    cost_landint = safe_pull(., "Red_Cos_AFO_Agr_value_landint"),
    ag_ch4_reduction = safe_pull(., "Emi_Red_CH4_AFO_Agr_value_landint"),
    ag_n2o_reduction = safe_pull(., "Emi_Red_N2O_AFO_Agr_value_landint"),
    land_co2_reduction = safe_pull(., "Emi_Red_CO2_AFO_Lan_value_landint"),
    residual_ch4 = safe_pull(., "Emi_equ_CH4_AFO_Agr_value_landint"),
    residual_n2o = safe_pull(., "Emi_equ_N2O_AFO_Agr_value_landint"),
    ag_total_reduction = ag_ch4_reduction + ag_n2o_reduction,
    ag_emis_total_improvement = -ag_emis_total_change,
    crop_emis_improvement = -crop_emis_change,
    livestock_emis_improvement = -livestock_emis_change
  )

world_mitigation <- country_core %>%
  filter(is.finite(mitigation_improvement), mitigation_improvement > 0) %>%
  group_by(year) %>%
  summarise(world_total_mitigation = sum(mitigation_improvement, na.rm = TRUE), .groups = "drop")

country_share <- country_core %>%
  filter(is.finite(mitigation_improvement), mitigation_improvement > 0) %>%
  left_join(world_mitigation, by = "year") %>%
  group_by(year) %>%
  arrange(desc(mitigation_improvement), .by_group = TRUE) %>%
  mutate(
    concentration_rank = row_number(),
    mitigation_share = mitigation_improvement / pmax(world_total_mitigation, 1e-12),
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

rank_metric <- function(df, value_col, metric, direction = "desc", n = 12) {
  base <- df %>%
    transmute(
      region,
      country = coalesce(country_name, region),
      metric = metric,
      value = .data[[value_col]]
    ) %>%
    filter(is.finite(value))
  out <- if (direction == "desc") {
    base %>% slice_max(order_by = value, n = n, with_ties = FALSE)
  } else if (direction == "asc") {
    base %>% slice_min(order_by = value, n = n, with_ties = FALSE)
  } else {
    base %>% slice_max(order_by = abs(value), n = n, with_ties = FALSE)
  }
  if (direction == "asc") {
    out %>% arrange(value) %>% mutate(rank_order = row_number())
  } else {
    out %>% arrange(desc(abs(value))) %>% mutate(rank_order = row_number())
  }
}

rankings_2050 <- bind_rows(
  rank_metric(country_core %>% filter(year == 2050), "land_co2_improvement", "Land-use CO2 improvement: Mergedcomparison / IAMCTempwoU / Emi_CO2_AFO_Lan / baseline - landint", "desc"),
  rank_metric(country_core %>% filter(year == 2050), "ag_emis_total_improvement", "Agricultural EMIS improvement: AGMIP / sum[EMIS(ent,mmg,rcc,sfr,mas,mgr,crs,bcr)] / baseline - landint", "desc"),
  rank_metric(country_core %>% filter(year == 2050), "ag_total_reduction", "Agricultural reduction: IAMC_templatePlus / Emi_Red_CH4_AFO_Agr + Emi_Red_N2O_AFO_Agr / landint", "desc"),
  rank_metric(country_core %>% filter(year == 2050), "cost_landint", "AFOLUB cost: IAMC_templatePlus / Red_Cos_AFO_Agr / landint", "desc"),
  rank_metric(country_core %>% filter(year == 2050), "calorie_change", "Calorie decrease: AGMIP / CALI(TOT) / landint - baseline", "asc"),
  rank_metric(country_core %>% filter(year == 2050), "ag_price_change", "Agricultural price increase: AGMIP / XPRP(AGR) / landint - baseline", "desc"),
  rank_metric(country_core %>% filter(year == 2050), "crp_prod_change", "Crop production decrease: AGMIP / PROD(CRP) / landint - baseline", "asc"),
  rank_metric(country_core %>% filter(year == 2050), "lsp_prod_change", "Livestock production decrease: AGMIP / PROD(LSP) / landint - baseline", "asc"),
  rank_metric(country_core %>% filter(year == 2050), "forest_area_change_pct_country", "Forest area gain share: Mergedcomparison / Lan_Cov_Frs / 100 * (landint - baseline) / country area", "desc"),
  rank_metric(country_core %>% filter(year == 2050), "crop_emis_improvement", "Crop-side EMIS improvement: AGMIP / EMIS(rcc+sfr+mas+crs+bcr) / baseline - landint", "desc"),
  rank_metric(country_core %>% filter(year == 2050), "livestock_emis_improvement", "Livestock-side EMIS improvement: AGMIP / EMIS(ent+mmg+mgr) / baseline - landint", "desc")
)

rankings_2100 <- bind_rows(
  rank_metric(country_core %>% filter(year == 2100), "land_co2_improvement", "Land-use CO2 improvement: Mergedcomparison / IAMCTempwoU / Emi_CO2_AFO_Lan / baseline - landint", "desc"),
  rank_metric(country_core %>% filter(year == 2100), "ag_emis_total_improvement", "Agricultural EMIS improvement: AGMIP / sum[EMIS(ent,mmg,rcc,sfr,mas,mgr,crs,bcr)] / baseline - landint", "desc"),
  rank_metric(country_core %>% filter(year == 2100), "ag_total_reduction", "Agricultural reduction: IAMC_templatePlus / Emi_Red_CH4_AFO_Agr + Emi_Red_N2O_AFO_Agr / landint", "desc"),
  rank_metric(country_core %>% filter(year == 2100), "cost_landint", "AFOLUB cost: IAMC_templatePlus / Red_Cos_AFO_Agr / landint", "desc"),
  rank_metric(country_core %>% filter(year == 2100), "calorie_change", "Calorie decrease: AGMIP / CALI(TOT) / landint - baseline", "asc"),
  rank_metric(country_core %>% filter(year == 2100), "ag_price_change", "Agricultural price increase: AGMIP / XPRP(AGR) / landint - baseline", "desc"),
  rank_metric(country_core %>% filter(year == 2100), "crp_prod_change", "Crop production decrease: AGMIP / PROD(CRP) / landint - baseline", "asc"),
  rank_metric(country_core %>% filter(year == 2100), "lsp_prod_change", "Livestock production decrease: AGMIP / PROD(LSP) / landint - baseline", "asc"),
  rank_metric(country_core %>% filter(year == 2100), "forest_area_change_pct_country", "Forest area gain share: Mergedcomparison / Lan_Cov_Frs / 100 * (landint - baseline) / country area", "desc"),
  rank_metric(country_core %>% filter(year == 2100), "crop_emis_improvement", "Crop-side EMIS improvement: AGMIP / EMIS(rcc+sfr+mas+crs+bcr) / baseline - landint", "desc"),
  rank_metric(country_core %>% filter(year == 2100), "livestock_emis_improvement", "Livestock-side EMIS improvement: AGMIP / EMIS(ent+mmg+mgr) / baseline - landint", "desc")
)

readr::write_csv(country_core %>% filter(year == 2030), file.path(out_dir, "country_core_2030.csv"))
readr::write_csv(country_core %>% filter(year == 2050), file.path(out_dir, "country_core_2050.csv"))
readr::write_csv(country_core %>% filter(year == 2100), file.path(out_dir, "country_core_2100.csv"))
readr::write_csv(concentration_summary, file.path(out_dir, "country_concentration_summary.csv"))
readr::write_csv(rankings_2050, file.path(out_dir, "country_rankings_2050.csv"))
readr::write_csv(rankings_2100, file.path(out_dir, "country_rankings_2100.csv"))

# Use Mergedcomparison.gdx / IAMCTemp / Emi_CO2_AFO to show baseline land-use
# CO2 patterns before looking at scenario differences.
p01 <- country_sf %>%
  left_join(country_core %>% filter(year == 2050) %>% select(region, baseline_emi_co2_afo = `Emi_CO2_AFO__Mt CO2/yr_baseline`), by = "region") %>%
  ggplot() +
  geom_sf(aes(fill = baseline_emi_co2_afo), color = NA) +
  scale_fill_gradient(low = "grey95", high = "#8C2D04", labels = label_comma()) +
  labs(
    title = "Map (2050): Mergedcomparison.gdx / IAMCTemp / variable = Emi_CO2_AFO / unit = Mt CO2/yr / scenario = SSP2_BaU_NoCC_No",
    subtitle = "Value plotted = baseline level of land-use-side CO2 variable Emi_CO2_AFO for country.set countries only",
    fill = "Emi_CO2_AFO\nbaseline",
    caption = "Interpretation note: Emi_CO2_AFO is treated here as land-use-side CO2, not total GHG"
  ) +
  theme_geer +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank())
save_plot(p01, "fig01_map_emi_co2_afo_baseline_2050.png", width = 12, height = 6.5)

# Use Mergedcomparison.gdx / IAMCTemp / Emi_CO2_AFO to map land-use-side CO2
# improvement = baseline - landint.
p02 <- country_sf %>%
  left_join(country_core %>% filter(year == 2050) %>% select(region, mitigation_improvement), by = "region") %>%
  ggplot() +
  geom_sf(aes(fill = mitigation_improvement), color = NA) +
  scale_fill_gradient(low = "grey95", high = "#0B7285", labels = label_comma()) +
  labs(
    title = "Map (2050): Mergedcomparison.gdx / IAMCTemp / variable = Emi_CO2_AFO / unit = Mt CO2/yr",
    subtitle = "Value plotted = land-use-side CO2 improvement = SSP2_BaU_NoCC_No - SSP2_1100C_CACN_DAC_NoCC_LandInt_global1",
    fill = "Baseline -\nLandInt",
    caption = "Interpretation note: Emi_CO2_AFO is used here only as land-use-side CO2 improvement"
  ) +
  theme_geer +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank())
save_plot(p02, "fig02_map_mitigation_improvement_2050.png", width = 12, height = 6.5)

# Use Mergedcomparison.gdx / IAMCTemp / Emi_CO2_AFO to examine concentration of
# land-use-side CO2 improvement across countries.
p03 <- ggplot(country_share %>% filter(year %in% c(2050, 2100)), aes(x = cum_country_share, y = cum_share, color = factor(year))) +
  geom_line(linewidth = 1.05) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey65") +
  scale_color_manual(values = c("2050" = "#0B7285", "2100" = "#B02A37")) +
  scale_x_continuous(labels = percent_format()) +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "Concentration curve: Mergedcomparison.gdx / IAMCTemp / Emi_CO2_AFO / land-use-side CO2 improvement = baseline - landint",
    subtitle = "Countries ranked by land-use-side CO2 improvement; years = 2050, 2100",
    x = "Cumulative share of countries ranked by land-use-side CO2 improvement",
    y = "Cumulative share of global land-use-side CO2 improvement",
    caption = "Source variable = Emi_CO2_AFO [Mt CO2/yr]; this is not total GHG"
  ) +
  theme_geer
save_plot(p03, "fig03_concentration_curve_2050_2100.png")

p04 <- concentration_summary %>%
  pivot_longer(cols = starts_with("share_"), names_to = "metric", values_to = "share") %>%
  mutate(metric = recode(metric, share_top5 = "Top 5", share_top10 = "Top 10", share_top20 = "Top 20")) %>%
  ggplot(aes(x = metric, y = share, fill = factor(year))) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("2050" = "#0B7285", "2100" = "#B02A37")) +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "Top-N shares: Mergedcomparison.gdx / IAMCTemp / Emi_CO2_AFO / land-use-side CO2 improvement = baseline - landint",
    subtitle = "share_top5, share_top10, share_top20 calculated from country-level land-use-side CO2 improvement",
    x = "Top-N country group",
    y = "Share of global land-use-side CO2 improvement",
    caption = "Scenario pair = SSP2_BaU_NoCC_No vs SSP2_1100C_CACN_DAC_NoCC_LandInt_global1"
  ) +
  theme_geer
save_plot(p04, "fig04_topn_share_comparison.png", width = 8, height = 6)

scatter_2050 <- country_core %>% filter(year == 2050)
label_ag_2050 <- top_non_missing(scatter_2050, "ag_total_reduction", n = 12, desc = TRUE)
label_land_2050 <- top_non_missing(scatter_2050, "land_co2_improvement", n = 12, desc = TRUE)

# Compare AFOLUB cost with AFOLUB agricultural reduction amount rather than with
# land-use-side CO2, because Red_Cos_AFO_Agr is an agriculture-side mitigation cost.
p05 <- ggplot(scatter_2050, aes(x = cost_landint, y = ag_total_reduction)) +
  geom_point(alpha = 0.75, color = "#0B7285", size = 2.1) +
  geom_text(data = label_ag_2050, aes(label = region), size = 2.6, nudge_y = 0.02 * max(abs(scatter_2050$ag_total_reduction), na.rm = TRUE), na.rm = TRUE) +
  scale_x_continuous(labels = label_comma()) +
  scale_y_continuous(labels = label_comma()) +
  labs(
    title = "Scatter (2050): IAMC_templatePlus.gdx / IAMC_templatePlus / Red_Cos_AFO_Agr vs [Emi_Red_CH4_AFO_Agr + Emi_Red_N2O_AFO_Agr]",
    subtitle = "x = Red_Cos_AFO_Agr [LandInt scenario]; y = agricultural reduction = Emi_Red_CH4_AFO_Agr + Emi_Red_N2O_AFO_Agr [LandInt scenario]",
    x = "Red_Cos_AFO_Agr [LandInt scenario]",
    y = "Agricultural reduction [LandInt scenario]",
    caption = "Red_Cos_AFO_Agr is compared with agriculture-side reduction, not with land-use-side CO2"
  ) +
  theme_geer
save_plot(p05, "fig05_scatter_mitigation_vs_cost_2050.png")

# Compare calorie change with agricultural emissions improvement. Because
# EMIS(tot) is unstable in the bulk read, the agriculture total here is the
# direct sum of EMIS(ent, mmg, rcc, sfr, mas, mgr, crs, bcr).
p06 <- ggplot(scatter_2050, aes(x = calorie_change, y = ag_emis_total_improvement)) +
  geom_hline(yintercept = 0, color = "grey70", linewidth = 0.4) +
  geom_vline(xintercept = 0, color = "grey70", linewidth = 0.4) +
  geom_point(alpha = 0.75, color = "#7A3E9D", size = 2.1) +
  geom_text(data = label_ag_2050, aes(label = region), size = 2.6, nudge_y = 0.02 * max(abs(scatter_2050$ag_emis_total_improvement), na.rm = TRUE), na.rm = TRUE) +
  scale_x_continuous(labels = label_comma()) +
  scale_y_continuous(labels = label_comma()) +
  labs(
    title = "Scatter (2050): AGMIP_all_scenario.gdx / AGMIP / CALI(TOT) / landint - baseline vs sum[AGMIP / EMIS(ent,mmg,rcc,sfr,mas,mgr,crs,bcr)] / baseline - landint",
    subtitle = "x = calorie change = CALI(TOT)_landint - CALI(TOT)_baseline [kcal/cap/d]; y = agricultural emissions improvement = sum(component baseline - component landint)",
    x = "CALI(TOT) change [landint - baseline, kcal/cap/d]",
    y = "Agricultural EMIS improvement [baseline - landint, MtCO2e]",
    caption = "Agriculture-side emissions are taken from AGMIP component EMIS items, not from Mergedcomparison"
  ) +
  theme_geer
save_plot(p06, "fig06_scatter_mitigation_vs_cali_2050.png")

# Compare crop price change with crop-side agricultural-emissions improvement.
p07 <- ggplot(scatter_2050, aes(x = crp_price_change, y = crop_emis_improvement)) +
  geom_hline(yintercept = 0, color = "grey70", linewidth = 0.4) +
  geom_vline(xintercept = 0, color = "grey70", linewidth = 0.4) +
  geom_point(alpha = 0.75, color = "#C96F28", size = 2.1) +
  geom_text(data = label_ag_2050, aes(label = region), size = 2.6, nudge_y = 0.02 * max(abs(scatter_2050$crop_emis_improvement), na.rm = TRUE), na.rm = TRUE) +
  scale_x_continuous(labels = label_comma()) +
  scale_y_continuous(labels = label_comma()) +
  labs(
    title = "Scatter (2050): AGMIP_all_scenario.gdx / AGMIP / XPRP(CRP) / landint - baseline vs AGMIP_all_scenario.gdx / AGMIP / EMIS(rcc+sfr+mas+crs+bcr) / baseline - landint",
    subtitle = "x = crop price change = XPRP(CRP)_landint - XPRP(CRP)_baseline [USD/t]; y = crop-side EMIS improvement = baseline - landint",
    x = "XPRP(CRP) change [landint - baseline, USD/t]",
    y = "Crop-side EMIS improvement [baseline - landint, MtCO2e]",
    caption = "Crop-side EMIS = rcc + sfr + mas + crs + bcr"
  ) +
  theme_geer
save_plot(p07, "fig07_scatter_mitigation_vs_price_2050.png")

# Compare crop production change with crop-side agricultural-emissions improvement.
p08 <- ggplot(scatter_2050, aes(x = crp_prod_change, y = crop_emis_improvement)) +
  geom_hline(yintercept = 0, color = "grey70", linewidth = 0.4) +
  geom_vline(xintercept = 0, color = "grey70", linewidth = 0.4) +
  geom_point(alpha = 0.75, color = "#457B9D", size = 2.1) +
  geom_text(data = label_ag_2050, aes(label = region), size = 2.6, nudge_y = 0.02 * max(abs(scatter_2050$crop_emis_improvement), na.rm = TRUE), na.rm = TRUE) +
  scale_x_continuous(labels = label_comma()) +
  scale_y_continuous(labels = label_comma()) +
  labs(
    title = "Scatter (2050): AGMIP_all_scenario.gdx / AGMIP / PROD(CRP) / landint - baseline vs AGMIP_all_scenario.gdx / AGMIP / EMIS(rcc+sfr+mas+crs+bcr) / baseline - landint",
    subtitle = "x = crop production change = PROD(CRP)_landint - PROD(CRP)_baseline [1000 t]; y = crop-side EMIS improvement = baseline - landint",
    x = "PROD(CRP) change [landint - baseline, 1000 t]",
    y = "Crop-side EMIS improvement [baseline - landint, MtCO2e]",
    caption = "Crop-side EMIS = rcc + sfr + mas + crs + bcr"
  ) +
  theme_geer
save_plot(p08, "fig08_scatter_mitigation_vs_crop_prod_2050.png")

# Compare livestock production change with livestock-side agricultural-emissions
# improvement.
p09 <- ggplot(scatter_2050, aes(x = lsp_prod_change, y = livestock_emis_improvement)) +
  geom_hline(yintercept = 0, color = "grey70", linewidth = 0.4) +
  geom_vline(xintercept = 0, color = "grey70", linewidth = 0.4) +
  geom_point(alpha = 0.75, color = "#8C2D04", size = 2.1) +
  geom_text(data = label_ag_2050, aes(label = region), size = 2.6, nudge_y = 0.02 * max(abs(scatter_2050$livestock_emis_improvement), na.rm = TRUE), na.rm = TRUE) +
  scale_x_continuous(labels = label_comma()) +
  scale_y_continuous(labels = label_comma()) +
  labs(
    title = "Scatter (2050): AGMIP_all_scenario.gdx / AGMIP / PROD(LSP) / landint - baseline vs AGMIP_all_scenario.gdx / AGMIP / EMIS(ent+mmg+mgr) / baseline - landint",
    subtitle = "x = livestock production change = PROD(LSP)_landint - PROD(LSP)_baseline [1000 t]; y = livestock-side EMIS improvement = baseline - landint",
    x = "PROD(LSP) change [landint - baseline, 1000 t]",
    y = "Livestock-side EMIS improvement [baseline - landint, MtCO2e]",
    caption = "Livestock-side EMIS = ent + mmg + mgr"
  ) +
  theme_geer
save_plot(p09, "fig09_scatter_mitigation_vs_liv_prod_2050.png")

# Compare normalized forest-area change against land-use-side CO2 improvement.
p10 <- ggplot(scatter_2050, aes(x = forest_area_change_pct_country, y = land_co2_improvement, size = abs(land_co2_improvement))) +
  geom_hline(yintercept = 0, color = "grey70", linewidth = 0.4) +
  geom_vline(xintercept = 0, color = "grey70", linewidth = 0.4) +
  geom_point(alpha = 0.75, color = "#0B7285") +
  geom_text(data = label_land_2050, aes(label = region), size = 2.5, nudge_y = 0.02 * max(abs(scatter_2050$land_co2_improvement), na.rm = TRUE), na.rm = TRUE) +
  scale_size_continuous(range = c(1.5, 5)) +
  labs(
    title = "Scatter (2050): Mergedcomparison.gdx / IAMCTempwoU / Lan_Cov_Frs change share vs Mergedcomparison.gdx / IAMCTempwoU / Emi_CO2_AFO_Lan baseline - landint",
    subtitle = "x = 100 * [Lan_Cov_Frs_landint - Lan_Cov_Frs_baseline] / country area; y = land-use-side CO2 improvement = Emi_CO2_AFO_Lan_baseline - Emi_CO2_AFO_Lan_landint; bubble size = |land-use-side CO2 improvement|",
    x = "Lan_Cov_Frs change / country area [%]",
    y = "Land-use-side CO2 improvement [baseline - landint]",
    caption = "Forest change is normalized by country area from the map geometry"
  ) +
  theme_geer
save_plot(p10, "fig10_scatter_forest_vs_price_2050.png")

# Compare normalized cropland change against calorie change.
p11 <- ggplot(scatter_2050, aes(x = cropland_area_change_pct_country, y = calorie_change, size = abs(land_co2_improvement))) +
  geom_hline(yintercept = 0, color = "grey70", linewidth = 0.4) +
  geom_vline(xintercept = 0, color = "grey70", linewidth = 0.4) +
  geom_point(alpha = 0.75, color = "#6A4C93") +
  geom_text(data = label_land_2050, aes(label = region), size = 2.5, nudge_y = 0.02 * max(abs(scatter_2050$calorie_change), na.rm = TRUE), na.rm = TRUE) +
  scale_size_continuous(range = c(1.5, 5)) +
  labs(
    title = "Scatter (2050): Mergedcomparison.gdx / IAMCTempwoU / Lan_Cov_Cro change share vs AGMIP_all_scenario.gdx / AGMIP / CALI(TOT) / landint - baseline",
    subtitle = "x = 100 * [Lan_Cov_Cro_landint - Lan_Cov_Cro_baseline] / country area; y = calorie change = CALI(TOT)_landint - CALI(TOT)_baseline; bubble size = |land-use-side CO2 improvement|",
    x = "Lan_Cov_Cro change / country area [%]",
    y = "CALI(TOT) change [landint - baseline, kcal/cap/d]",
    caption = "Cropland change is normalized by country area from the map geometry"
  ) +
  theme_geer
save_plot(p11, "fig11_scatter_cropland_vs_cali_2050.png")

# Use IAMC_templatePlus.gdx / IAMC_templatePlus for direct mitigation component
# values. Countries are ranked by land-use-side CO2 improvement from
# Mergedcomparison / Emi_CO2_AFO_Lan.
top_mitigation_regions <- top_non_missing(scatter_2050, "land_co2_improvement", n = 12, desc = TRUE) %>%
  arrange(desc(land_co2_improvement)) %>%
  mutate(rank_order = row_number()) %>%
  select(region, country_name, land_co2_improvement, rank_order)

component_df <- country_core %>%
  filter(year == 2050) %>%
  inner_join(top_mitigation_regions, by = c("region", "country_name", "land_co2_improvement")) %>%
  transmute(
    country = coalesce(country_name, region),
    land_co2_improvement,
    rank_order,
    `Emi_Red_CH4_AFO_Agr` = ag_ch4_reduction,
    `Emi_Red_N2O_AFO_Agr` = ag_n2o_reduction,
    `Emi_Red_CO2_AFO_Lan` = land_co2_reduction
  ) %>%
  pivot_longer(cols = starts_with("Emi_Red_"), names_to = "component", values_to = "value") %>%
  mutate(country = forcats::fct_reorder(country, rank_order, .desc = TRUE))

p12 <- ggplot(component_df, aes(x = country, y = value, fill = component)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = label_comma()) +
  labs(
    title = "Top mitigation countries (2050): IAMC_templatePlus.gdx / IAMC_templatePlus / reduction components",
    subtitle = "Bars = Emi_Red_CH4_AFO_Agr, Emi_Red_N2O_AFO_Agr, Emi_Red_CO2_AFO_Lan in LandInt; ranking = Mergedcomparison / IAMCTempwoU / Emi_CO2_AFO_Lan baseline - landint",
    x = "Country",
    y = "Reduction component value",
    caption = "LandInt scenario = SSP2_1100C_CACN_DAC_NoCC_LandInt_global1"
  ) +
  theme_geer
save_plot(p12, "fig12_top_countries_mitigation_components_2050.png", width = 11, height = 8)

# Compare top countries across several original variables and simple metrics.
p13 <- rankings_2050 %>%
  mutate(country_metric = reorder_within_local(country, -rank_order, metric)) %>%
  ggplot(aes(x = value, y = country_metric, fill = metric)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ metric, scales = "free") +
  scale_y_reordered_local() +
  labs(
    title = "Top countries by selected original variables and simple metrics (2050)",
    subtitle = "Each facet shows the top 12 countries ranked within that metric; calorie and production decrease facets use low values, others use high values",
    x = "Metric value",
    y = "Country",
    caption = "Within-facet order is based on that facet's own metric"
  ) +
  theme_geer
save_plot(p13, "fig13_top_countries_rankings_2050.png", width = 13, height = 9)

# Use Mergedcomparison.gdx / GHG for LULUCF net-emissions improvement.
p14 <- country_sf %>%
  left_join(country_core %>% filter(year == 2050) %>% select(region, net_emissions_improvement), by = "region") %>%
  ggplot() +
  geom_sf(aes(fill = net_emissions_improvement), color = NA) +
  scale_fill_gradient2(low = "#B02A37", mid = "white", high = "#0B7285", midpoint = 0, labels = label_comma()) +
  labs(
    title = "Map (2050): Mergedcomparison.gdx / GHG / component = Net_emissions / model = LUM / baseline - landint",
    subtitle = "Value plotted = land-use-side Net_emissions_baseline - Net_emissions_landint within Mergedcomparison only",
    fill = "LUM Net_emissions\nbaseline - landint",
    caption = "Interpretation note: Mergedcomparison / GHG is treated as land-use-side only, not economy-wide GHG"
  ) +
  theme_geer +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank())
save_plot(p14, "fig14_map_lulucf_net_change_2050.png", width = 12, height = 6.5)

# Use Mergedcomparison.gdx / AREA to inspect direct land-code changes for the top
# mitigation countries.
area_heatmap <- area_wide %>%
  filter(year == 2050, land_code %in% c("FRS", "CL", "PAS", "PRMFRS", "SECFRS"), region != "World") %>%
  semi_join(top_mitigation_regions, by = "region") %>%
  left_join(top_mitigation_regions, by = "region") %>%
  left_join(country_area_tbl, by = "region") %>%
  mutate(
    country = coalesce(country_name, region),
    change_pct_country = 100 * change_landint_vs_baseline / pmax(country_area_mha, 1e-12),
    land_code = recode(land_code,
      FRS = "AREA FRS",
      CL = "AREA CL",
      PAS = "AREA PAS",
      PRMFRS = "AREA PRMFRS",
      SECFRS = "AREA SECFRS"
    ),
    country = forcats::fct_reorder(country, rank_order, .desc = TRUE)
  )

p15 <- ggplot(area_heatmap, aes(x = land_code, y = country, fill = change_pct_country)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "#B02A37", mid = "white", high = "#0B7285", midpoint = 0, labels = label_number(accuracy = 0.01)) +
  labs(
    title = "Heatmap (2050): Mergedcomparison.gdx / AREA / 100 * (landint - baseline) / country area for selected land_code",
    subtitle = "Countries ranked by Mergedcomparison / IAMCTempwoU / Emi_CO2_AFO_Lan baseline - landint; land_code = FRS, CL, PAS, PRMFRS, SECFRS",
    x = "AREA land_code",
    y = "Country",
    fill = "AREA change\n/ country area [%]",
    caption = "AREA changes are normalized by country area so large countries do not dominate visually"
  ) +
  theme_geer +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
save_plot(p15, "fig15_area_heatmap_2050.png", width = 11, height = 8)

# Show simple gain/loss rankings for forest area change.
forest_gain <- top_non_missing(scatter_2050, "forest_area_change_pct_country", n = 12, desc = TRUE) %>%
  arrange(desc(forest_area_change_pct_country)) %>%
  mutate(country = factor(coalesce(country_name, region), levels = rev(coalesce(country_name, region))))

forest_loss <- top_non_missing(scatter_2050, "forest_area_change_pct_country", n = 12, desc = FALSE) %>%
  arrange(forest_area_change_pct_country) %>%
  mutate(country = factor(coalesce(country_name, region), levels = rev(coalesce(country_name, region))))

p16a <- ggplot(forest_gain, aes(x = forest_area_change_pct_country, y = country)) +
  geom_col(fill = "#0B7285") +
  labs(
    title = "Top forest gain countries (2050)",
    subtitle = "Mergedcomparison.gdx / IAMCTempwoU / variable = Lan_Cov_Frs / value = 100 * (landint - baseline) / country area",
    x = "Lan_Cov_Frs change / country area [%]",
    y = "Country"
  ) +
  theme_geer

p16b <- ggplot(forest_loss, aes(x = forest_area_change_pct_country, y = country)) +
  geom_col(fill = "#B02A37") +
  labs(
    title = "Top forest loss countries (2050)",
    subtitle = "Mergedcomparison.gdx / IAMCTempwoU / variable = Lan_Cov_Frs / value = 100 * (landint - baseline) / country area",
    x = "Lan_Cov_Frs change / country area [%]",
    y = "Country"
  ) +
  theme_geer

save_plot(p16a + p16b, "fig16_top_forest_gain_loss_2050.png", width = 13, height = 7)

# Use AGMIP_all_scenario.gdx / AGMIP / PROD, NETT, LAND to inspect direct
# structural changes under mitigation rather than only import-export change.
trade_rank <- bind_rows(
  rank_metric(country_core %>% filter(year == 2050), "crp_prod_change", "Structure change: AGMIP / PROD(CRP) / landint - baseline", "abs"),
  rank_metric(country_core %>% filter(year == 2050), "lsp_prod_change", "Structure change: AGMIP / PROD(LSP) / landint - baseline", "abs"),
  rank_metric(country_core %>% filter(year == 2050), "crp_nett_change", "Structure change: AGMIP / NETT(CRP) / landint - baseline", "abs"),
  rank_metric(country_core %>% filter(year == 2050), "lsp_nett_change", "Structure change: AGMIP / NETT(LSP) / landint - baseline", "abs"),
  rank_metric(country_core %>% filter(year == 2050), "cropland_land_change_kha", "Structure change: AGMIP / LAND(CRP) / landint - baseline", "abs"),
  rank_metric(country_core %>% filter(year == 2050), "forest_land_change_kha", "Structure change: AGMIP / LAND(FOR) / landint - baseline", "abs"),
  rank_metric(country_core %>% filter(year == 2050), "grassland_land_change_kha", "Structure change: AGMIP / LAND(GRS) / landint - baseline", "abs")
) %>%
  mutate(country_metric = reorder_within_local(country, -rank_order, metric))

p17 <- ggplot(trade_rank, aes(x = value, y = country_metric, fill = metric)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ metric, scales = "free") +
  scale_y_reordered_local() +
  labs(
    title = "Top countries by structural changes under mitigation (2050)",
    subtitle = "Source = AGMIP_all_scenario.gdx / AGMIP / variables = PROD, NETT, LAND / items = CRP, LSP, FOR, GRS / value = landint - baseline; countries ranked by absolute change within each metric",
    x = "Change value",
    y = "Country",
    caption = "This panel is intended to show structure change rather than only trade change"
  ) +
  theme_geer
save_plot(p17, "fig17_top_trade_change_2050.png", width = 12, height = 8)

# Use Mergedcomparison.gdx / GHG to show which LULUCF-side components change most
# for the top mitigation countries in 2100.
ghg_component_2100 <- ghg_wide %>%
  filter(year == 2100, region != "World") %>%
  left_join(country_core %>% filter(year == 2100) %>% select(region, country_name, land_co2_improvement), by = "region") %>%
  semi_join(top_non_missing(country_core %>% filter(year == 2100), "land_co2_improvement", n = 12, desc = TRUE), by = "region") %>%
  mutate(
    country = coalesce(country_name, region),
    component = recode(component,
      sink = "GHG component = Sink",
      emissions = "GHG component = Emissions",
      net_emissions = "GHG component = Net_emissions"
    ),
    country = fct_reorder(country, land_co2_improvement)
  )

p18 <- ggplot(ghg_component_2100, aes(x = country, y = improvement_baseline_vs_landint, fill = component)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = label_comma()) +
  labs(
    title = "Land-use-side GHG component improvements for top land CO2 improvement countries (2100)",
    subtitle = "Source = Mergedcomparison.gdx / GHG / model = LUM / components = Sink, Emissions, Net_emissions / value = baseline - landint",
    x = "Country",
    y = "GHG component baseline - landint",
    caption = "Interpretation note: this panel is land-use-side only; ranking uses Mergedcomparison / IAMCTempwoU / Emi_CO2_AFO_Lan baseline - landint"
  ) +
  theme_geer
save_plot(p18, "fig18_ghg_component_improvement_2100.png", width = 11, height = 8)

# Use AGMIP_all_scenario.gdx / AGMIP / EMIS source items to inspect baseline
# agricultural emission-source composition for the highest-emission countries.
emis_source_2050 <- agmip_wide %>%
  filter(
    region != "World",
    year == 2050,
    variable == "EMIS",
    item %in% c("ent", "mmg", "rcc", "sfr", "mas", "mgr", "crs", "bcr"),
    unit == "MtCO2e"
  ) %>%
  left_join(
    agmip_wide %>%
      filter(region != "World", year == 2050, variable == "EMIS", item == "tot", unit == "MtCO2e") %>%
      transmute(region, total_baseline_emis = baseline),
    by = "region"
  ) %>%
  left_join(st_drop_geometry(country_sf), by = "region") %>%
  group_by(region) %>%
  mutate(country = coalesce(first(country_name), first(region))) %>%
  ungroup() %>%
  slice_max(order_by = total_baseline_emis, n = 12, with_ties = FALSE) %>%
  arrange(desc(total_baseline_emis)) %>%
  mutate(country = factor(country, levels = rev(unique(country))))

p19 <- ggplot(emis_source_2050, aes(x = country, y = baseline, fill = item)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = label_comma()) +
  labs(
    title = "Baseline agricultural emission-source composition (2050)",
    subtitle = "Source = AGMIP_all_scenario.gdx / AGMIP / variable = EMIS / items = ent, mmg, rcc, sfr, mas, mgr, crs, bcr / unit = MtCO2e / scenario = SSP2_BaU_NoCC_No",
    x = "Country",
    y = "EMIS baseline [MtCO2e]",
    caption = "Countries ranked by EMIS(tot) baseline"
  ) +
  theme_geer
save_plot(p19, "fig19_baseline_ag_emission_sources_2050.png", width = 12, height = 8)

# Compare price and production changes directly across a few core AGMIP items.
price_prod_panel <- bind_rows(
  rank_metric(country_core %>% filter(year == 2050), "ag_price_change", "XPRP(AGR) change: AGMIP / landint - baseline", "abs"),
  rank_metric(country_core %>% filter(year == 2050), "crp_price_change", "XPRP(CRP) change: AGMIP / landint - baseline", "abs"),
  rank_metric(country_core %>% filter(year == 2050), "lsp_price_change", "XPRP(LSP) change: AGMIP / landint - baseline", "abs"),
  rank_metric(country_core %>% filter(year == 2050), "crp_prod_change", "PROD(CRP) change: AGMIP / landint - baseline", "abs"),
  rank_metric(country_core %>% filter(year == 2050), "lsp_prod_change", "PROD(LSP) change: AGMIP / landint - baseline", "abs")
) %>%
  mutate(country_metric = reorder_within_local(country, -rank_order, metric))

p20 <- ggplot(price_prod_panel, aes(x = value, y = country_metric, fill = metric)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ metric, scales = "free") +
  scale_y_reordered_local() +
  labs(
    title = "Top country changes in selected AGMIP prices and production variables (2050)",
    subtitle = "Source = AGMIP_all_scenario.gdx / AGMIP / XPRP and PROD / items = AGR, CRP, LSP / value = landint - baseline; countries ranked by absolute change within each item",
    x = "Change value",
    y = "Country",
    caption = "All panels use original AGMIP variables with no composite index"
  ) +
  theme_geer
save_plot(p20, "fig20_price_production_changes_2050.png", width = 12, height = 8)

# Compare land-use-side CO2 improvement with agriculture-side reduction to see
# whether the same countries dominate both dimensions.
p21 <- ggplot(scatter_2050, aes(x = ag_total_reduction, y = land_co2_improvement)) +
  geom_hline(yintercept = 0, color = "grey70", linewidth = 0.4) +
  geom_vline(xintercept = 0, color = "grey70", linewidth = 0.4) +
  geom_point(alpha = 0.75, color = "#2A6F97", size = 2.1) +
  geom_text(data = label_ag_2050, aes(label = region), size = 2.5, nudge_y = 0.02 * max(abs(scatter_2050$land_co2_improvement), na.rm = TRUE), na.rm = TRUE) +
  scale_x_continuous(labels = label_comma()) +
  scale_y_continuous(labels = label_comma()) +
  labs(
    title = "Scatter (2050): IAMC_templatePlus agricultural reduction vs Mergedcomparison land-use CO2 improvement",
    subtitle = "x = Emi_Red_CH4_AFO_Agr + Emi_Red_N2O_AFO_Agr [LandInt]; y = Emi_CO2_AFO_Lan_baseline - Emi_CO2_AFO_Lan_landint",
    x = "Agricultural reduction [LandInt scenario]",
    y = "Land-use-side CO2 improvement [baseline - landint]",
    caption = "This helps separate countries driven by agricultural non-CO2 reduction from countries driven by land-use CO2"
  ) +
  theme_geer
save_plot(p21, "fig21_scatter_ag_vs_land_mitigation_2050.png", width = 10, height = 7)

# Inspect where agricultural emissions improvement comes from by source item for
# the main agriculture-reduction countries.
emis_improve_heatmap <- country_core %>%
  filter(year == 2050) %>%
  semi_join(top_non_missing(scatter_2050, "ag_total_reduction", n = 12, desc = TRUE), by = "region") %>%
  transmute(
    country = coalesce(country_name, region),
    rank_base = ag_total_reduction,
    `EMIS ent improvement` = -ag_emis_ent_change,
    `EMIS mmg improvement` = -ag_emis_mmg_change,
    `EMIS rcc improvement` = -ag_emis_rcc_change,
    `EMIS sfr improvement` = -ag_emis_sfr_change,
    `EMIS mas improvement` = -ag_emis_mas_change,
    `EMIS mgr improvement` = -ag_emis_mgr_change,
    `EMIS crs improvement` = -ag_emis_crs_change,
    `EMIS bcr improvement` = -ag_emis_bcr_change
  ) %>%
  pivot_longer(cols = starts_with("EMIS "), names_to = "source", values_to = "value") %>%
  mutate(country = forcats::fct_reorder(country, rank_base, .desc = TRUE))

p22 <- ggplot(emis_improve_heatmap, aes(x = source, y = country, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "#B02A37", mid = "white", high = "#0B7285", midpoint = 0, labels = label_comma()) +
  labs(
    title = "Heatmap (2050): agricultural EMIS source improvements for top agriculture-reduction countries",
    subtitle = "Source = AGMIP_all_scenario.gdx / AGMIP / EMIS item-level / value = baseline - landint; countries ranked by IAMC_templatePlus agricultural reduction",
    x = "Agricultural emission source item",
    y = "Country",
    fill = "EMIS\nbaseline - landint",
    caption = "Positive values mean lower emissions in the mitigation scenario"
  ) +
  theme_geer +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
save_plot(p22, "fig22_ag_emission_source_heatmap_2050.png", width = 12, height = 8)

message("Completed 3rd_check exploratory analysis.")
