library(tidyverse)
library(gdxrrw)
library(ggplot2)
library(patchwork)
library(scales)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

sf_use_s2(FALSE)

# settings ----------------------------------------------------------------
args_all <- commandArgs(trailingOnly = FALSE)
file_arg <- "--file="
script_arg <- args_all[startsWith(args_all, file_arg)]
script_dir <- if (length(script_arg) > 0) {
  dirname(normalizePath(sub(file_arg, "", script_arg[1]), winslash = "/", mustWork = TRUE))
} else {
  candidate_dirs <- c(
    getwd(),
    file.path(getwd(), "prog"),
    file.path(getwd(), "Papers", "GEER2026", "prog")
  )
  candidate_dirs <- unique(candidate_dirs[file.exists(candidate_dirs)])
  candidate_dirs <- candidate_dirs[file.info(candidate_dirs)$isdir %in% TRUE]
  candidate_dirs <- candidate_dirs[file.exists(file.path(candidate_dirs, "paperV2.R"))]
  if (length(candidate_dirs) == 0) {
    stop(
      "Could not determine script_dir. Open paperV2.R from Papers/GEER2026/prog/ ",
      "or set the working directory to Papers/GEER2026 before sourcing."
    )
  }
  normalizePath(candidate_dirs[[1]], winslash = "/", mustWork = TRUE)
}

base_dir <- normalizePath(file.path(script_dir, ".."), winslash = "/", mustWork = TRUE)
data_dir <- file.path(base_dir, "data", "0316")
define_dir <- file.path(base_dir, "define")
output_dir <- file.path(base_dir, "output", "paperV2")
country_set_file <- file.path(define_dir, "country.set")
region_map_file <- file.path(define_dir, "region.map")

scenario_baseline <- "SSP2_BaU_NoCC_No"
scenario_landint <- "SSP2_1100C_CACN_DAC_NoCC_LandInt_global1"
scenario_no <- "SSP2_1100C_CACN_DAC_NoCC_No"
plot_years <- c(2050, 2100)
decadal_years <- seq(2020, 2100, 10)
focus_year <- 2100L
top_n <- 20L

theme_main <- theme_bw() +
  theme(
    text = element_text(size = 12),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 9),
    legend.position = "bottom",
    legend.title = element_blank(),
    strip.background = element_blank(),
    strip.clip = "off"
  )

save_plot <- function(filename, plot_obj, width = 12, height = 8, dpi = 300) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  out_path <- file.path(output_dir, filename)
  ggsave(out_path, plot = plot_obj, width = width, height = height, dpi = dpi, bg = "white", limitsize = FALSE)
  message("Saved: ", out_path)
}

safe_pull <- function(df, name) {
  if (name %in% names(df)) {
    return(df[[name]])
  }
  rep(NA_real_, nrow(df))
}

short_country_name <- function(x) {
  dplyr::recode(
    x,
    "United States of America" = "United States",
    "Russian Federation" = "Russia",
    "Democratic Republic of the Congo" = "DR Congo",
    "Republic of the Congo" = "Congo",
    "Korea, Republic of" = "South Korea",
    "Korea, Democratic People's Republic of" = "North Korea",
    "United Republic of Tanzania" = "Tanzania",
    "Lao PDR" = "Laos",
    "Syrian Arab Republic" = "Syria",
    "Viet Nam" = "Vietnam",
    .default = x
  )
}

# common palettes and plotting helpers ------------------------------------

land_color <- c(
  "Cropland" = "#8CA231",
  "Managed and primary forest" = "#014D2F",
  "Grassland" = "#26C3EC",
  "Other natural land" = "#FF9855",
  "Non arable land" = "#AAA9A9",
  "Energy Crop" = "#ED87AD"
)

r5_labels <- c(
  "World" = "World",
  "R5OECD90+EU" = "OECD+EU",
  "R5ASIA" = "Asia",
  "R5MAF" = "Africa/Middle East",
  "R5LAM" = "Latin America",
  "R5REF" = "Former Soviet Union"
)

region_palette <- c(
  "OECD+EU" = "#F28C99",
  "Asia" = "#82B7F8",
  "Africa/Middle East" = "#8DCC7A",
  "Latin America" = "#F1B96B",
  "Former Soviet Union" = "#B79AE8"
)

region_border_palette <- c(
  "OECD+EU" = "#D06A77",
  "Asia" = "#538DD6",
  "Africa/Middle East" = "#5FA24D",
  "Latin America" = "#D39B49",
  "Former Soviet Union" = "#8D6FD0"
)

afolu_component_palette <- c(
  "LULUCF" = "#6BAED6",
  "Agriculture" = "#F6C667"
)

fig1_component_palette <- c(
  "Enteric fermentation" = "#B56576",
  "Manure management" = "#E56B6F",
  "Rice cultivation" = "#E9C46A",
  "Managed soils" = "#F4A261",
  "Land Conversion" = "#4E8F5B",
  "Forest fire" = "#457B9D"
)

fig2_component_palette <- c(
  "Enteric fermentation" = "#B56576",
  "Manure management" = "#E56B6F",
  "Rice cultivation" = "#E9C46A",
  "Managed soils" = "#F4A261",
  "Prevented land conversion" = "#2E7D32",
  "Afforestation" = "#0B84A5",
  "Forest protection" = "#8BC34A",
  "Forest management" = "#4C78A8",
  "Enhanced natural regeneration" = "#F28E2B",
  "Agroforestry" = "#C0CA33",
  "Biochar" = "#8D6E63",
  "Soil carbon sequestration" = "#8E6CBE"
)

crp_lsp_palette <- c(
  "Crops" = "#5B8E7D",
  "Livestock" = "#D98C5F"
)

fixed_afolu_bubble_size <- function(df) {
  labels <- c(
    "<= 20 MtCO2eq/yr",
    "20-100 MtCO2eq/yr",
    "100-250 MtCO2eq/yr",
    ">= 250 MtCO2eq/yr"
  )

  df %>%
    mutate(
      size_bin = cut(
        afolu_reduction,
        breaks = c(-Inf, 20, 100, 250, Inf),
        include.lowest = TRUE,
        right = TRUE,
        labels = labels
      )
    )
}

make_country_map <- function(df, value_col, title_text, fill_label) {
  map_df <- Boundary %>%
    left_join(df, by = c("cty" = "region"))

  ggplot(map_df) +
    geom_sf(aes(fill = .data[[value_col]]), color = "grey70", linewidth = 0.1) +
    scale_fill_gradient2(low = "#4477AA", mid = "white", high = "#CC6677", midpoint = 0, na.value = "grey92") +
    labs(title = title_text, fill = fill_label) +
    theme_void() +
    theme(legend.position = "bottom", plot.title = element_text(size = 12, hjust = 0.5))
}

make_world_ts_panel <- function(df, title_text, palette = afolu_component_palette, limits = c("LULUCF", "Agriculture")) {
  ggplot(df, aes(x = factor(year), y = value, fill = component)) +
    geom_col(width = 0.72) +
    scale_fill_manual(values = palette, limits = limits, drop = FALSE) +
    labs(title = title_text, x = "Year", y = "MtCO2eq/yr") +
    theme_main +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

make_r5_panel <- function(df, title_text, palette = afolu_component_palette, limits = c("LULUCF", "Agriculture")) {
  ggplot(df, aes(x = region_name, y = value, fill = component)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
    geom_col(width = 0.72) +
    scale_fill_manual(values = palette, limits = limits, drop = FALSE) +
    labs(title = title_text, x = NULL, y = "MtCO2eq/yr") +
    theme_main +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

make_top_country_share_panel <- function(df, title_text, full_total_sum, x_label = "MtCO2eq/yr", palette = afolu_component_palette, limits = c("LULUCF", "Agriculture")) {
  share_vec <- if (length(full_total_sum) == 1 && is.finite(full_total_sum) && full_total_sum != 0) {
    100 * (df %>%
      group_by(country) %>%
      summarise(total_value = sum(value, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(total_value)))$total_value / full_total_sum
  } else {
    NULL
  }

  totals <- df %>%
    group_by(country) %>%
    summarise(total_value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(total_value)) %>%
    mutate(
      share = if (!is.null(share_vec)) share_vec else NA_real_,
      cumulative_share = cumsum(share)
    )

  country_levels <- totals$country
  max_total <- max(totals$total_value, na.rm = TRUE)
  share_scale <- if (is.finite(max_total) && max_total > 0) max_total / 100 else 1

  bar_df <- df %>% mutate(country = factor(country, levels = country_levels))
  line_df <- totals %>% mutate(
    country = factor(country, levels = country_levels),
    cumulative_scaled = cumulative_share * share_scale
  )

  ggplot() +
    geom_col(data = bar_df, aes(x = country, y = value, fill = component), width = 0.82) +
    geom_line(data = line_df, aes(x = country, y = cumulative_scaled, group = 1, color = "Cumulative share"), linewidth = 0.8) +
    geom_point(data = line_df, aes(x = country, y = cumulative_scaled, color = "Cumulative share"), size = 1.4) +
    scale_fill_manual(values = palette, limits = limits, drop = FALSE) +
    scale_color_manual(values = c("Cumulative share" = "#C62828")) +
    scale_y_continuous(
      name = x_label,
      sec.axis = sec_axis(~ . / share_scale, name = "Cumulative share (%)")
    ) +
    labs(title = title_text, x = NULL) +
    theme_main +
    theme(axis.text.x = element_text(angle = 60, hjust = 1, vjust = 1))
}

make_horizontal_stack_with_point <- function(stack_df, point_df, title_text, x_label) {
  ggplot(stack_df, aes(x = value, y = country, fill = component)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
    geom_col() +
    geom_point(
      data = point_df,
      aes(x = total_value, y = country),
      inherit.aes = FALSE,
      shape = 21,
      size = 2.2,
      stroke = 0.35,
      fill = "black",
      color = "black"
    ) +
    scale_fill_manual(values = crp_lsp_palette) +
    labs(title = title_text, x = x_label, y = NULL) +
    theme_main
}

make_simple_rank_panel <- function(df, title_text, x_label, fill_color) {
  ggplot(df, aes(x = value, y = country)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
    geom_col(fill = fill_color) +
    labs(title = title_text, x = x_label, y = NULL) +
    theme_main
}

# lookup tables and mappings ----------------------------------------------

country_codes <- scan(country_set_file, what = character(), quiet = TRUE)

region_map <- readLines(region_map_file, warn = FALSE) %>%
  stringr::str_trim() %>%
  .[. != "" & !startsWith(., "*")] %>%
  stringr::str_split("\\s+") %>%
  purrr::map_dfr(function(x) {
    tibble(region = x[[1]], region5_code = x[[length(x)]])
  }) %>%
  filter(region %in% country_codes, region5_code %in% names(r5_labels), region5_code != "World") %>%
  mutate(region5 = recode(region5_code, !!!r5_labels))

c_map <- readLines(file.path(define_dir, "c.map"), warn = FALSE) %>%
  stringr::str_trim() %>%
  .[. != "" & !startsWith(., "*")] %>%
  stringr::str_split("\\s+") %>%
  purrr::map_dfr(function(x) {
    tibble(commodity = x[[1]], group = x[[length(x)]])
  }) %>%
  filter(group %in% c("CRP", "LSP")) %>%
  distinct()

Boundary <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>%
  st_make_valid() %>%
  transmute(cty = dplyr::if_else(adm0_a3 != "-99", adm0_a3, iso_a3), country_name = admin) %>%
  mutate(country_name = short_country_name(country_name)) %>%
  filter(cty %in% country_codes)

# raw data reads -----------------------------------------------------------

read_agmip <- function(variables, items, units, regions, years, scenarios) {
  req <- list(name = "AGMIP", form = "full", uels = list(scenarios, regions, variables, items, units, as.character(years)))
  x <- rgdx(file.path(data_dir, "AGMIP_all_scenario.gdx"), req, squeeze = FALSE)
  as.data.frame.table(x$val, responseName = "value", stringsAsFactors = FALSE) %>%
    setNames(c("scenario", "region", "variable", "item", "unit", "year", "value")) %>%
    mutate(item = toupper(item), year = as.integer(year), value = as.numeric(value)) %>%
    group_by(scenario, region, variable, item, unit, year) %>%
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop")
}

regions_for_read <- unique(c("World", sort(unique(country_codes)), names(r5_labels)))

GEER_AGMIP <- read_agmip(
  variables = c("CALI", "XPRP", "XPRC", "PROD", "CONS", "NETT", "LAND", "POPT", "GDPT", "EMIS", "RedCos"),
  items = c("TOT", "AGR", "CRP", "LSP", "FOR", "GRS", "ECP", "ONV", "NLD", "tot", "ent", "mmg", "rcc", "sfr", "mas", "mgr", "crs", "ors", "bsv", "bcr", "WHT", "RIC", "CGR", "OSD", "SGC", "VFN", "OCR", "RUM", "NRM", "DRY"),
  units = c("kcal/cap/d", "USD/t", "1000 ha", "million", "million kcal", "1000 t", "bn USD 2017 PPP", "MtCO2e", "USD"),
  regions = regions_for_read,
  years = decadal_years,
  scenarios = c(scenario_baseline, scenario_landint)
)

GEER_CTAX <- read_agmip(
  variables = c("CTAX"),
  items = c("TOT"),
  units = c("USD/tCO2e"),
  regions = c("USA"),
  years = decadal_years,
  scenarios = c(scenario_baseline, scenario_landint)
)

GEER_AGMIP_AGG <- rgdx.param(file.path(data_dir, "AGMIP_all_scenario.gdx"), "AGMIP_agg") %>%
  transmute(
    scenario = as.character(SCENARIO),
    region = as.character(region),
    variable = as.character(AgMIP_VAR),
    item = toupper(as.character(ITEM)),
    unit = as.character(AgMIP_UNIT),
    year = as.integer(as.character(Y)),
    value = as.numeric(AGMIP_agg)
  ) %>%
  filter(
    scenario %in% c(scenario_baseline, scenario_landint),
    year %in% decadal_years
  )

GEER_AFOLUB <- rgdx.param(file.path(data_dir, "IAMC_templatePlus.gdx"), "IAMC_templatePlus") %>%
  transmute(
    scenario = as.character(Scenario),
    region = as.character(REMF),
    variable = as.character(VEMF),
    year = as.integer(as.character(YEMF)),
    value = as.numeric(IAMC_templatePlus)
  ) %>%
  filter(
    scenario %in% c(scenario_no, scenario_landint),
    year %in% decadal_years,
    variable %in% c(
      "Emi_equ_GHG_AFO_Lan",
      "Emi_CO2_AFO_Lan_For_Gra_Con",
      "Emi_CO2_AFO_Lan_Aba_Man_Lan",
      "Emi_CO2_AFO_Lan_Emi_Rem_Soi",
      "Emi_CO2_AFO_Lan_For_Bur",
      "Emi_CO2_AFO_Lan_Pea_Fir_For",
      "Emi_CO2_Lan_Use_Flo_Pos_Emi_Lan_Use_Cha",
      "Emi_CO2_Lan_Use_Flo_Pos_Emi_Fir",
      "Emi_CO2_Lan_Use_Flo_RemForest",
      "Emi_CO2_Lan_Use_Flo_Pos_Emi_Woo_Har",
      "Emi_Red_CO2_AFO_Lan_FPr",
      "Emi_Red_CO2_AFO_Lan_Pff",
      "Emi_Red_CO2_AFO_Lan_Ril",
      "Emi_Red_CO2_AFO_Lan_Wmp",
      "Emi_Red_CO2_AFO_Lan_Prr",
      "Emi_Red_CO2_AFO_Lan_Enr",
      "Emi_Red_CO2_AFO_Lan_Agf",
      "Emi_Red_CO2_AFO_Lan_Bch",
      "Emi_Red_CO2_AFO_Lan_Scc",
      "Emi_Red_CO2_AFO_Lan_Scg",
      "Emi_Red_CO2_AFO_Lan_Scs",
      "Red_Cos_AFO_Lan_Pla",
      "Red_Cos_AFO_Lan_Ref",
      "Red_Cos_AFO_Lan_FPr",
      "Red_Cos_AFO_Lan_Pff",
      "Red_Cos_AFO_Lan_Ril",
      "Red_Cos_AFO_Lan_Enr",
      "Red_Cos_AFO_Lan_Agf",
      "Red_Cos_AFO_Lan_Wmp",
      "Red_Cos_AFO_Lan_Prr",
      "Red_Cos_AFO_Lan_Bch",
      "Red_Cos_AFO_Lan_Scc",
      "Red_Cos_AFO_Lan_Scg"
    )
  )

GEER_WORLD_EFBASE <- rgdx.param(file.path(data_dir, "baseyear.gdx"), "EFbase")
GEER_WORLD_QDEM <- rgdx.param(file.path(data_dir, "all_scenario_World.gdx"), "QDEM_load")
GEER_WORLD_QSUP <- rgdx.param(file.path(data_dir, "all_scenario_World.gdx"), "QSUP_load")
GEER_WORLD_RR <- rgdx.param(file.path(data_dir, "all_scenario_World.gdx"), "RR_load")
GEER_WORLD_GHG <- rgdx.param(file.path(data_dir, "all_scenario_World.gdx"), "GHG_load")
GEER_WORLD_PRICE_DECOMP <- rgdx.param(file.path(data_dir, "all_scenario_World.gdx"), "Price_decomp")
GEER_WORLD_GHG_LU <- rgdx.param(file.path(data_dir, "all_scenario_World.gdx"), "GHG_LU_load")
GEER_WORLD_GHG_LU_AGG <- rgdx.param(file.path(data_dir, "all_scenario_global_17.gdx"), "GHG_LU_agg_load")

# shared derived tables ----------------------------------------------------

agmip_diff <- GEER_AGMIP %>%
  filter(region %in% country_codes, year %in% plot_years) %>%
  pivot_wider(names_from = scenario, values_from = value) %>%
  mutate(change = .data[[scenario_landint]] - .data[[scenario_baseline]])

agmip_land_long <- agmip_diff %>%
  filter(variable == "LAND", item %in% c("TOT", "CRP", "ECP", "GRS", "ONV", "NLD", "FOR"), unit == "1000 ha") %>%
  select(region, year, item, change, !!scenario_baseline) %>%
  pivot_wider(names_from = item, values_from = c(change, !!sym(scenario_baseline)), values_fill = 0) %>%
  mutate(
    change_CRP_NE = change_CRP - change_ECP,
    baseline_CRP_NE = .data[[paste0(scenario_baseline, "_CRP")]] - .data[[paste0(scenario_baseline, "_ECP")]]
  ) %>%
  transmute(
    region,
    year,
    total_land = .data[[paste0(scenario_baseline, "_TOT")]],
    `Cropland` = change_CRP_NE,
    `Energy Crop` = change_ECP,
    `Grassland` = change_GRS,
    `Managed and primary forest` = change_FOR,
    `Other natural land` = change_ONV,
    `Non arable land` = change_NLD
  ) %>%
  pivot_longer(cols = c("Cropland", "Energy Crop", "Grassland", "Managed and primary forest", "Other natural land", "Non arable land"), names_to = "land_item", values_to = "land_change") %>%
  mutate(
    land_share = if_else(total_land > 0, 100 * land_change / total_land, NA_real_),
    land_item = factor(land_item, levels = names(land_color))
  )

agmip_emis_actual_reduction <- GEER_AGMIP %>%
  filter(
    region %in% c(country_codes, names(r5_labels)),
    year %in% decadal_years,
    scenario %in% c(scenario_baseline, scenario_landint),
    variable == "EMIS",
    unit == "MtCO2e",
    item %in% c("TOT", "ENT", "MMG", "RCC", "SFR", "MAS", "MGR", "CRS", "ORS", "BSV", "BCR")
  ) %>%
  group_by(region, year, item, scenario) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = scenario, values_from = value) %>%
  mutate(actual_reduction = .data[[scenario_baseline]] - .data[[scenario_landint]])

agmip_ag_reduction_rank <- agmip_emis_actual_reduction %>%
  filter(region %in% country_codes, item != "TOT", year %in% plot_years) %>%
  group_by(region, year) %>%
  summarise(ag_reduction = sum(actual_reduction, na.rm = TRUE), .groups = "drop") %>%
  left_join(st_drop_geometry(Boundary), by = c("region" = "cty")) %>%
  mutate(country = coalesce(country_name, region))

land_total_raw <- GEER_AFOLUB %>%
  filter(variable == "Emi_equ_GHG_AFO_Lan") %>%
  select(region, year, scenario, value) %>%
  pivot_wider(names_from = scenario, values_from = value, values_fill = 0) %>%
  mutate(
    baseline_land_emissions = .data[[scenario_no]],
    post_land_emissions = .data[[scenario_landint]],
    land_actual_change = post_land_emissions - baseline_land_emissions
  )

land_hierarchy_decadal <- GEER_AFOLUB %>%
  filter(variable %in% c(
    "Emi_CO2_AFO_Lan_For_Gra_Con",
    "Emi_CO2_AFO_Lan_Aba_Man_Lan",
    "Emi_CO2_AFO_Lan_Emi_Rem_Soi",
    "Emi_CO2_AFO_Lan_For_Bur",
    "Emi_CO2_AFO_Lan_Pea_Fir_For"
  )) %>%
  mutate(component = recode(
    variable,
    "Emi_CO2_AFO_Lan_For_Gra_Con" = "Forest and grassland conversion",
    "Emi_CO2_AFO_Lan_Aba_Man_Lan" = "Abandonment of managed lands",
    "Emi_CO2_AFO_Lan_Emi_Rem_Soi" = "Soil emissions/removals",
    "Emi_CO2_AFO_Lan_For_Bur" = "Forest burning CO2",
    "Emi_CO2_AFO_Lan_Pea_Fir_For" = "Peat fire CO2"
  )) %>%
  group_by(region, year, component, scenario) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = scenario, values_from = value, values_fill = 0) %>%
  mutate(
    baseline_land_emissions = .data[[scenario_no]],
    post_land_emissions = .data[[scenario_landint]],
    land_actual_change = post_land_emissions - baseline_land_emissions
  )

land_managed_sink_afr_country <- GEER_WORLD_GHG_LU %>%
  transmute(
    scenario = as.character(SCENARIO),
    src = as.character(src),
    gas = as.character(gas),
    region = as.character(cty),
    year = as.integer(as.character(Y)),
    value = as.numeric(GHG_LU_load) / 1000
  ) %>%
  filter(
    scenario == scenario_landint,
    src == "afr",
    gas == "CO2",
    region %in% country_codes,
    year %in% decadal_years
  ) %>%
  group_by(region, year) %>%
  summarise(
    baseline_land_emissions = sum(value, na.rm = TRUE),
    post_land_emissions = sum(value, na.rm = TRUE),
    land_actual_change = sum(value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(component = "Managed forest sink")

land_managed_sink_afr_agg <- GEER_WORLD_GHG_LU_AGG %>%
  transmute(
    scenario = as.character(SCENARIO),
    src = as.character(src),
    gas = as.character(gas),
    region = as.character(region),
    year = as.integer(as.character(Y)),
    value = as.numeric(GHG_LU_agg_load) / 1000
  ) %>%
  filter(
    scenario == scenario_landint,
    src == "afr",
    gas == "CO2",
    region %in% c("World", names(r5_labels)),
    year %in% decadal_years
  ) %>%
  group_by(region, year) %>%
  summarise(
    baseline_land_emissions = sum(value, na.rm = TRUE),
    post_land_emissions = sum(value, na.rm = TRUE),
    land_actual_change = sum(value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(component = "Managed forest sink")

land_hierarchy_afr_decadal <- bind_rows(
  land_hierarchy_decadal,
  land_managed_sink_afr_country,
  land_managed_sink_afr_agg
)

land_mitigation_decadal <- GEER_AFOLUB %>%
  filter(
    scenario == scenario_landint,
    variable %in% c(
      "Emi_Red_CO2_AFO_Lan_FPr",
      "Emi_Red_CO2_AFO_Lan_Pff",
      "Emi_Red_CO2_AFO_Lan_Ril",
      "Emi_Red_CO2_AFO_Lan_Wmp",
      "Emi_Red_CO2_AFO_Lan_Prr",
      "Emi_Red_CO2_AFO_Lan_Enr",
      "Emi_Red_CO2_AFO_Lan_Agf",
      "Emi_Red_CO2_AFO_Lan_Bch",
      "Emi_Red_CO2_AFO_Lan_Scc",
      "Emi_Red_CO2_AFO_Lan_Scg",
      "Emi_Red_CO2_AFO_Lan_Scs"
    )
  ) %>%
  mutate(component = case_when(
    variable %in% c("Emi_Red_CO2_AFO_Lan_FPr", "Emi_Red_CO2_AFO_Lan_Pff") ~ "Forest protection / fire prevention",
    variable %in% c("Emi_Red_CO2_AFO_Lan_Ril", "Emi_Red_CO2_AFO_Lan_Wmp", "Emi_Red_CO2_AFO_Lan_Prr") ~ "Forest management / peatland management",
    variable == "Emi_Red_CO2_AFO_Lan_Enr" ~ "Enhanced natural regeneration",
    variable == "Emi_Red_CO2_AFO_Lan_Agf" ~ "Agroforestry",
    variable == "Emi_Red_CO2_AFO_Lan_Bch" ~ "Biochar",
    variable %in% c("Emi_Red_CO2_AFO_Lan_Scc", "Emi_Red_CO2_AFO_Lan_Scg", "Emi_Red_CO2_AFO_Lan_Scs") ~ "Soil carbon",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(component)) %>%
  group_by(region, year, component) %>%
  summarise(plot_value = -sum(abs(value), na.rm = TRUE), .groups = "drop")

land_change_plus_mitigation_afr_decadal <- bind_rows(
  land_hierarchy_afr_decadal %>%
    filter(component != "Peat fire CO2") %>%
    transmute(region, year, component, plot_value = land_actual_change),
  land_mitigation_decadal
)

afolu_ag_country <- agmip_emis_actual_reduction %>%
  filter(region %in% country_codes, item != "tot") %>%
  group_by(region, year) %>%
  summarise(value = sum(actual_reduction, na.rm = TRUE), .groups = "drop") %>%
  mutate(component = "Agriculture")

afolu_ag_world_r5 <- GEER_AGMIP_AGG %>%
  filter(region %in% names(r5_labels), variable == "EMIS", unit == "MtCO2e", item == "AGR") %>%
  group_by(region, year) %>%
  summarise(
    bau_ag_emissions = sum(value[scenario == scenario_baseline], na.rm = TRUE),
    landint_ag_emissions = sum(value[scenario == scenario_landint], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    value = bau_ag_emissions - landint_ag_emissions,
    region = recode(region, !!!r5_labels),
    component = "Agriculture"
  ) %>%
  select(region, year, component, value)

afolu_land_country <- land_change_plus_mitigation_afr_decadal %>%
  filter(region %in% country_codes) %>%
  group_by(region, year) %>%
  summarise(value = -sum(plot_value, na.rm = TRUE), .groups = "drop") %>%
  mutate(component = "LULUCF")

afolu_land_world_r5 <- land_change_plus_mitigation_afr_decadal %>%
  filter(region %in% c("World", names(r5_labels))) %>%
  group_by(region, year) %>%
  summarise(value = -sum(plot_value, na.rm = TRUE), .groups = "drop") %>%
  mutate(region = recode(region, !!!r5_labels), component = "LULUCF")

afolu_world_r5 <- bind_rows(afolu_ag_world_r5, afolu_land_world_r5)
afolu_country <- bind_rows(afolu_ag_country, afolu_land_country)

afolu_baseline_ag_country <- agmip_emis_actual_reduction %>%
  filter(region %in% country_codes, item != "TOT") %>%
  group_by(region, year) %>%
  summarise(value = sum(.data[[scenario_baseline]], na.rm = TRUE), .groups = "drop") %>%
  mutate(component = "Agriculture")

afolu_baseline_land_country <- GEER_AFOLUB %>%
  filter(
    region %in% country_codes,
    scenario == scenario_no,
    variable %in% c(
      "Emi_CO2_Lan_Use_Flo_Pos_Emi_Lan_Use_Cha",
      "Emi_CO2_AFO_Lan_For_Bur",
      "Emi_CO2_Lan_Use_Flo_Pos_Emi_Fir"
    ),
    year %in% decadal_years
  ) %>%
  group_by(region, year) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  mutate(component = "LULUCF")

afolu_baseline_country <- bind_rows(afolu_baseline_ag_country, afolu_baseline_land_country)

afolu_rate_country <- afolu_country %>%
  group_by(region, year) %>%
  summarise(actual_reduction = sum(value, na.rm = TRUE), .groups = "drop") %>%
  left_join(
    afolu_baseline_country %>%
      group_by(region, year) %>%
      summarise(baseline_emissions = sum(value, na.rm = TRUE), .groups = "drop"),
    by = c("region", "year")
  ) %>%
  mutate(reduction_rate = if_else(is.finite(baseline_emissions) & baseline_emissions != 0, 100 * actual_reduction / baseline_emissions, NA_real_))

afolu_ag_rate_country <- afolu_ag_country %>%
  rename(actual_reduction = value) %>%
  left_join(
    afolu_baseline_ag_country %>%
      rename(baseline_emissions = value),
    by = c("region", "year")
  ) %>%
  mutate(reduction_rate = if_else(is.finite(baseline_emissions) & baseline_emissions != 0, 100 * actual_reduction / baseline_emissions, NA_real_))

world_qsup <- GEER_WORLD_QSUP %>%
  transmute(
    scenario = as.character(SCENARIO),
    commodity = as.character(c),
    region = as.character(cty),
    year = as.integer(as.character(Y)),
    qsup = as.numeric(QSUP_load)
  ) %>%
  filter(
    scenario %in% c(scenario_baseline, scenario_landint),
    region %in% country_codes,
    commodity != "ecr",
    year %in% plot_years
  )

world_qdem <- GEER_WORLD_QDEM %>%
  transmute(
    scenario = as.character(SCENARIO),
    commodity = as.character(c),
    region = as.character(cty),
    year = as.integer(as.character(Y)),
    qdem = as.numeric(QDEM_load)
  ) %>%
  filter(
    scenario %in% c(scenario_baseline, scenario_landint),
    region %in% country_codes,
    commodity != "ecr",
    year %in% plot_years
  )

world_rr <- GEER_WORLD_RR %>%
  transmute(
    scenario = as.character(SCENARIO),
    commodity = as.character(c),
    source = as.character(src),
    gas = as.character(gas),
    region = as.character(cty),
    year = as.integer(as.character(Y)),
    rr = as.numeric(RR_load)
  ) %>%
  filter(
    scenario %in% c(scenario_baseline, scenario_landint),
    region %in% country_codes,
    commodity != "ecr",
    gas %in% c("CH4", "N2O"),
    year %in% plot_years
  )

world_ghg <- GEER_WORLD_GHG %>%
  transmute(
    scenario = as.character(SCENARIO),
    commodity = as.character(c),
    source = as.character(src),
    gas = as.character(gas),
    region = as.character(cty),
    year = as.integer(as.character(Y)),
    ghg = as.numeric(GHG_load)
  ) %>%
  filter(
    scenario %in% c(scenario_baseline, scenario_landint),
    region %in% country_codes,
    commodity != "ecr",
    gas %in% c("CH4", "N2O"),
    year %in% plot_years
  )

world_bau_ef <- GEER_WORLD_EFBASE %>%
  transmute(
    commodity = as.character(a),
    source = as.character(src),
    gas = as.character(gas),
    region = as.character(cty),
    ef_base = as.numeric(EFbase)
  ) %>%
  filter(region %in% country_codes, commodity != "ecr", gas %in% c("CH4", "N2O"))

world_decomp_base <- world_ghg %>%
  select(scenario, region, year, commodity, source, gas) %>%
  distinct() %>%
  left_join(world_qsup, by = c("scenario", "region", "year", "commodity")) %>%
  left_join(world_bau_ef, by = c("region", "commodity", "source", "gas")) %>%
  left_join(world_rr, by = c("scenario", "region", "year", "commodity", "source", "gas")) %>%
  mutate(rr = replace_na(rr, 0), ef = ef_base * (1 - rr))

world_qsup_delta <- world_decomp_base %>%
  select(scenario, region, year, commodity, source, gas, qsup) %>%
  pivot_wider(names_from = scenario, values_from = qsup) %>%
  mutate(
    qsup_ave = (.data[[scenario_baseline]] + .data[[scenario_landint]]) / 2,
    qsup_delta = .data[[scenario_landint]] - .data[[scenario_baseline]]
  ) %>%
  select(region, year, commodity, source, gas, qsup_ave, qsup_delta)

world_ef_delta <- world_decomp_base %>%
  select(scenario, region, year, commodity, source, gas, ef) %>%
  pivot_wider(names_from = scenario, values_from = ef) %>%
  mutate(
    ef_ave = (.data[[scenario_baseline]] + .data[[scenario_landint]]) / 2,
    ef_delta = .data[[scenario_landint]] - .data[[scenario_baseline]]
  ) %>%
  select(region, year, commodity, source, gas, ef_ave, ef_delta)

world_ghg_total <- world_ghg %>%
  pivot_wider(names_from = scenario, values_from = ghg) %>%
  mutate(
    total_change_mt = (.data[[scenario_landint]] - .data[[scenario_baseline]]) / 1000,
    bau_ghg_kt = .data[[scenario_baseline]]
  ) %>%
  select(region, year, commodity, source, gas, total_change_mt, bau_ghg_kt)

world_decomp_country_gas <- world_qsup_delta %>%
  left_join(world_ef_delta, by = c("region", "year", "commodity", "source", "gas")) %>%
  left_join(world_ghg_total, by = c("region", "year", "commodity", "source", "gas")) %>%
  mutate(
    production_change_mt = qsup_delta * ef_ave / 1000,
    reduction_measures_mt = qsup_ave * ef_delta / 1000
  ) %>%
  group_by(region, year, gas) %>%
  summarise(
    bau_ghg_kt = sum(bau_ghg_kt, na.rm = TRUE),
    production_change_mt = sum(production_change_mt, na.rm = TRUE),
    reduction_measures_mt = sum(reduction_measures_mt, na.rm = TRUE),
    total_change_mt = sum(total_change_mt, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(st_drop_geometry(Boundary), by = c("region" = "cty")) %>%
  mutate(country = coalesce(country_name, region))

world_decomp_r5_world_co2eq <- bind_rows(
  world_decomp_country_gas %>%
    left_join(region_map %>% select(region, region5), by = "region") %>%
    filter(!is.na(region5)) %>%
    group_by(region5, year) %>%
    summarise(
      production_change_mt = sum(production_change_mt, na.rm = TRUE),
      reduction_measures_mt = sum(reduction_measures_mt, na.rm = TRUE),
      total_change_mt = sum(total_change_mt, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(region_name = factor(recode(region5, !!!r5_labels), levels = rev(c("World", "OECD+EU", "Asia", "Africa/Middle East", "Latin America", "Former Soviet Union")))),
  world_decomp_country_gas %>%
    group_by(year) %>%
    summarise(
      production_change_mt = sum(production_change_mt, na.rm = TRUE),
      reduction_measures_mt = sum(reduction_measures_mt, na.rm = TRUE),
      total_change_mt = sum(total_change_mt, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(region_name = factor("World", levels = rev(c("World", "OECD+EU", "Asia", "Africa/Middle East", "Latin America", "Former Soviet Union"))))
)

world_qdem_wide <- world_qdem %>%
  pivot_wider(names_from = scenario, values_from = qdem) %>%
  mutate(
    qdem_ratio = if_else(is.finite(.data[[scenario_baseline]]) & .data[[scenario_baseline]] != 0, .data[[scenario_landint]] / .data[[scenario_baseline]], NA_real_)
  ) %>%
  select(region, year, commodity, qdem_ratio)

world_qsup_wide <- world_qsup %>%
  pivot_wider(names_from = scenario, values_from = qsup) %>%
  mutate(
    qsup_ratio = if_else(is.finite(.data[[scenario_baseline]]) & .data[[scenario_baseline]] != 0, .data[[scenario_landint]] / .data[[scenario_baseline]], NA_real_)
  ) %>%
  select(region, year, commodity, qsup_ratio)

world_ef_wide <- world_decomp_base %>%
  select(scenario, region, year, commodity, source, gas, ef) %>%
  pivot_wider(names_from = scenario, values_from = ef) %>%
  mutate(
    ef_ratio = if_else(is.finite(.data[[scenario_baseline]]) & .data[[scenario_baseline]] != 0, .data[[scenario_landint]] / .data[[scenario_baseline]], NA_real_)
  ) %>%
  select(region, year, commodity, source, gas, ef_ratio)

world_ghg_wide <- world_ghg %>%
  pivot_wider(names_from = scenario, values_from = ghg) %>%
  mutate(
    bau_ghg_kt = .data[[scenario_baseline]],
    total_change_mt = (.data[[scenario_landint]] - .data[[scenario_baseline]]) / 1000
  ) %>%
  select(region, year, commodity, source, gas, bau_ghg_kt, total_change_mt)

world_final_wedge_country_co2eq <- world_ghg_wide %>%
  left_join(world_qdem_wide, by = c("region", "year", "commodity")) %>%
  left_join(world_qsup_wide, by = c("region", "year", "commodity")) %>%
  left_join(
    world_rr %>%
      select(scenario, region, year, commodity, source, gas, rr) %>%
      pivot_wider(names_from = scenario, values_from = rr) %>%
      mutate(rr_tech = replace_na(.data[[scenario_landint]], 0)) %>%
      select(region, year, commodity, source, gas, rr_tech),
    by = c("region", "year", "commodity", "source", "gas")
  ) %>%
  left_join(world_ef_wide, by = c("region", "year", "commodity", "source", "gas")) %>%
  mutate(rr_tech = replace_na(rr_tech, 0)) %>%
  mutate(
    demand_mt = bau_ghg_kt * (((qdem_ratio - 1) + ((qdem_ratio - 1) * ef_ratio)) / 2) / 1000,
    supply_mt = bau_ghg_kt * (((ef_ratio - 1) + ((ef_ratio - 1) * qsup_ratio)) / 2) / 1000,
    trade_mt = bau_ghg_kt * ((((qsup_ratio - qdem_ratio)) + ((qsup_ratio - qdem_ratio) * ef_ratio)) / 2) / 1000,
    technical_mt = bau_ghg_kt * (((-rr_tech) + ((-rr_tech) * qsup_ratio)) / 2) / 1000,
    structural_mt = (supply_mt - technical_mt) + trade_mt
  ) %>%
  group_by(region, year) %>%
  summarise(
    demand_mt = sum(demand_mt, na.rm = TRUE),
    technical_mt = sum(technical_mt, na.rm = TRUE),
    structural_mt = sum(structural_mt, na.rm = TRUE),
    total_change_mt = sum(total_change_mt, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(st_drop_geometry(Boundary), by = c("region" = "cty")) %>%
  mutate(country = coalesce(country_name, region))

world_final_wedge_r5_world_co2eq <- bind_rows(
  world_final_wedge_country_co2eq %>%
    left_join(region_map %>% select(region, region5), by = "region") %>%
    filter(!is.na(region5)) %>%
    group_by(region5, year) %>%
    summarise(
      demand_mt = sum(demand_mt, na.rm = TRUE),
      technical_mt = sum(technical_mt, na.rm = TRUE),
      structural_mt = sum(structural_mt, na.rm = TRUE),
      total_change_mt = sum(total_change_mt, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(region_name = factor(recode(region5, !!!r5_labels), levels = rev(c("World", "OECD+EU", "Asia", "Africa/Middle East", "Latin America", "Former Soviet Union")))),
  world_final_wedge_country_co2eq %>%
    group_by(year) %>%
    summarise(
      demand_mt = sum(demand_mt, na.rm = TRUE),
      technical_mt = sum(technical_mt, na.rm = TRUE),
      structural_mt = sum(structural_mt, na.rm = TRUE),
      total_change_mt = sum(total_change_mt, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(region_name = factor("World", levels = rev(c("World", "OECD+EU", "Asia", "Africa/Middle East", "Latin America", "Former Soviet Union"))))
)

afolu_baseline_world_ts <- bind_rows(
  GEER_AGMIP_AGG %>%
    filter(region == "World", scenario == scenario_baseline, variable == "EMIS", item == "AGR", unit == "MtCO2e") %>%
    transmute(year, component = "Agriculture", value),
  land_total_raw %>%
    filter(region == "World") %>%
    transmute(year, component = "LULUCF", value = baseline_land_emissions)
)

afolu_baseline_world_r5_2100 <- bind_rows(
  GEER_AGMIP_AGG %>%
    filter(region %in% names(r5_labels), scenario == scenario_baseline, variable == "EMIS", item == "AGR", unit == "MtCO2e", year == focus_year) %>%
    transmute(region_name = factor(recode(region, !!!r5_labels), levels = rev(c("OECD+EU", "Asia", "Africa/Middle East", "Latin America", "Former Soviet Union"))), component = "Agriculture", value),
  land_total_raw %>%
    filter(region %in% names(r5_labels), year == focus_year) %>%
    transmute(region_name = factor(recode(region, !!!r5_labels), levels = rev(c("OECD+EU", "Asia", "Africa/Middle East", "Latin America", "Former Soviet Union"))), component = "LULUCF", value = baseline_land_emissions)
) %>%
  filter(!is.na(region_name))

afolu_baseline_country_2100 <- bind_rows(
  afolu_baseline_ag_country %>% filter(year == focus_year),
  afolu_baseline_land_country %>% filter(year == focus_year)
) %>%
  left_join(st_drop_geometry(Boundary), by = c("region" = "cty")) %>%
  mutate(country = coalesce(country_name, region))

ag_source_map <- c(
  "ENT" = "Enteric fermentation",
  "MMG" = "Manure management",
  "RCC" = "Rice cultivation",
  "SFR" = "Managed soils",
  "MAS" = "Managed soils",
  "MGR" = "Managed soils",
  "CRS" = "Managed soils",
  "ORS" = "Managed soils",
  "BSV" = "Managed soils",
  "BCR" = "Managed soils"
)

fig1_land_source_map <- c(
  "Emi_CO2_Lan_Use_Flo_Pos_Emi_Lan_Use_Cha" = "Land Conversion",
  "Emi_CO2_AFO_Lan_For_Bur" = "Forest fire",
  "Emi_CO2_Lan_Use_Flo_Pos_Emi_Fir" = "Forest fire"
)

fig1_component_levels <- names(fig1_component_palette)

fig1_ag_world_ts <- GEER_AGMIP_AGG %>%
  filter(region == "World", scenario == scenario_baseline, variable == "EMIS", item %in% names(ag_source_map), unit == "MtCO2e") %>%
  mutate(component = recode(item, !!!ag_source_map)) %>%
  group_by(year, component) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop")

fig1_land_world_ts <- GEER_AFOLUB %>%
  filter(region == "World", scenario == scenario_no, variable %in% names(fig1_land_source_map), year %in% decadal_years) %>%
  mutate(component = recode(variable, !!!fig1_land_source_map)) %>%
  group_by(year, component) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop")

fig1_baseline_world_ts <- bind_rows(fig1_ag_world_ts, fig1_land_world_ts) %>%
  mutate(component = factor(component, levels = fig1_component_levels))

fig1_ag_r5_2100 <- GEER_AGMIP_AGG %>%
  filter(region %in% names(r5_labels), scenario == scenario_baseline, variable == "EMIS", item %in% names(ag_source_map), unit == "MtCO2e", year == focus_year) %>%
  mutate(region_name = recode(region, !!!r5_labels), component = recode(item, !!!ag_source_map)) %>%
  group_by(region_name, component) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop")

fig1_land_r5_2100 <- GEER_AFOLUB %>%
  filter(region %in% names(r5_labels), scenario == scenario_no, variable %in% names(fig1_land_source_map), year == focus_year) %>%
  mutate(region_name = recode(region, !!!r5_labels), component = recode(variable, !!!fig1_land_source_map)) %>%
  group_by(region_name, component) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop")

fig1_baseline_world_r5_2100 <- bind_rows(fig1_ag_r5_2100, fig1_land_r5_2100) %>%
  mutate(
    region_name = factor(region_name, levels = rev(c("OECD+EU", "Asia", "Africa/Middle East", "Latin America", "Former Soviet Union"))),
    component = factor(component, levels = fig1_component_levels)
  ) %>%
  filter(!is.na(region_name))

fig1_ag_country_2100 <- GEER_AGMIP %>%
  filter(region %in% country_codes, scenario == scenario_baseline, variable == "EMIS", item %in% names(ag_source_map), unit == "MtCO2e", year == focus_year) %>%
  mutate(component = recode(item, !!!ag_source_map)) %>%
  group_by(region, component) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop")

fig1_land_country_2100 <- GEER_AFOLUB %>%
  filter(region %in% country_codes, scenario == scenario_no, variable %in% names(fig1_land_source_map), year == focus_year) %>%
  mutate(component = recode(variable, !!!fig1_land_source_map)) %>%
  group_by(region, component) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop")

fig1_baseline_country_2100 <- bind_rows(fig1_ag_country_2100, fig1_land_country_2100) %>%
  left_join(st_drop_geometry(Boundary), by = c("region" = "cty")) %>%
  mutate(country = coalesce(country_name, region), component = factor(component, levels = fig1_component_levels))

fig2_ag_source_map <- ag_source_map
fig2_component_levels <- names(fig2_component_palette)

fig2_ag_world_ts <- agmip_emis_actual_reduction %>%
  filter(region %in% country_codes, item %in% names(fig2_ag_source_map)) %>%
  mutate(component = recode(item, !!!fig2_ag_source_map)) %>%
  group_by(year, component) %>%
  summarise(value = sum(actual_reduction, na.rm = TRUE), .groups = "drop")

fig2_ag_r5_2100 <- agmip_emis_actual_reduction %>%
  filter(region %in% country_codes, year == focus_year, item %in% names(fig2_ag_source_map)) %>%
  left_join(region_map %>% select(region, region5), by = "region") %>%
  filter(!is.na(region5)) %>%
  mutate(region_name = region5, component = recode(item, !!!fig2_ag_source_map)) %>%
  group_by(region_name, component) %>%
  summarise(value = sum(actual_reduction, na.rm = TRUE), .groups = "drop")

fig2_ag_country_2100 <- agmip_emis_actual_reduction %>%
  filter(region %in% country_codes, year == focus_year, item %in% names(fig2_ag_source_map)) %>%
  mutate(component = recode(item, !!!fig2_ag_source_map)) %>%
  group_by(region, component) %>%
  summarise(value = sum(actual_reduction, na.rm = TRUE), .groups = "drop")

fig2_land_change_decadal <- GEER_AFOLUB %>%
  filter(
    variable %in% c(
      "Emi_CO2_Lan_Use_Flo_Pos_Emi_Lan_Use_Cha"
    )
  ) %>%
  mutate(component = case_when(
    variable == "Emi_CO2_Lan_Use_Flo_Pos_Emi_Lan_Use_Cha" ~ "Prevented land conversion",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(component)) %>%
  group_by(region, year, component, scenario) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = scenario, values_from = value, values_fill = 0) %>%
  transmute(region, year, component, value = .data[[scenario_no]] - .data[[scenario_landint]])

fig2_aff_country <- land_managed_sink_afr_country %>%
  transmute(region, year, component = "Afforestation", value = -land_actual_change)

fig2_aff_agg <- land_managed_sink_afr_agg %>%
  transmute(region, year, component = "Afforestation", value = -land_actual_change)

fig2_land_mitigation <- GEER_AFOLUB %>%
  filter(
    scenario == scenario_landint,
    variable %in% c(
      "Emi_Red_CO2_AFO_Lan_FPr",
      "Emi_Red_CO2_AFO_Lan_Pff",
      "Emi_Red_CO2_AFO_Lan_Ril",
      "Emi_Red_CO2_AFO_Lan_Wmp",
      "Emi_Red_CO2_AFO_Lan_Prr",
      "Emi_Red_CO2_AFO_Lan_Enr",
      "Emi_Red_CO2_AFO_Lan_Agf",
      "Emi_Red_CO2_AFO_Lan_Bch",
      "Emi_Red_CO2_AFO_Lan_Scc",
      "Emi_Red_CO2_AFO_Lan_Scg",
      "Emi_Red_CO2_AFO_Lan_Scs"
    )
  ) %>%
  mutate(component = case_when(
    variable %in% c("Emi_Red_CO2_AFO_Lan_FPr", "Emi_Red_CO2_AFO_Lan_Pff") ~ "Forest protection",
    variable %in% c("Emi_Red_CO2_AFO_Lan_Ril", "Emi_Red_CO2_AFO_Lan_Wmp", "Emi_Red_CO2_AFO_Lan_Prr") ~ "Forest management",
    variable == "Emi_Red_CO2_AFO_Lan_Enr" ~ "Enhanced natural regeneration",
    variable == "Emi_Red_CO2_AFO_Lan_Agf" ~ "Agroforestry",
    variable == "Emi_Red_CO2_AFO_Lan_Bch" ~ "Biochar",
    variable %in% c("Emi_Red_CO2_AFO_Lan_Scc", "Emi_Red_CO2_AFO_Lan_Scg", "Emi_Red_CO2_AFO_Lan_Scs") ~ "Soil carbon sequestration",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(component)) %>%
  group_by(region, year, component) %>%
  summarise(value = sum(abs(value), na.rm = TRUE), .groups = "drop")

fig2_land_world_ts <- bind_rows(
  fig2_land_change_decadal %>% filter(region == "World"),
  fig2_aff_agg %>% filter(region == "World"),
  fig2_land_mitigation %>% filter(region == "World")
) %>% group_by(year, component) %>% summarise(value = sum(value, na.rm = TRUE), .groups = "drop")

fig2_land_r5_2100 <- bind_rows(
  fig2_land_change_decadal %>% filter(region %in% names(r5_labels), year == focus_year),
  fig2_aff_agg %>% filter(region %in% names(r5_labels), year == focus_year),
  fig2_land_mitigation %>% filter(region %in% names(r5_labels), year == focus_year)
) %>%
  mutate(region_name = recode(region, !!!r5_labels)) %>%
  group_by(region_name, component) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop")

fig2_land_country_2100 <- bind_rows(
  fig2_land_change_decadal %>% filter(region %in% country_codes, year == focus_year),
  fig2_aff_country %>% filter(region %in% country_codes, year == focus_year),
  fig2_land_mitigation %>% filter(region %in% country_codes, year == focus_year)
) %>%
  group_by(region, component) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop")

fig2_actual_world_ts <- bind_rows(fig2_ag_world_ts, fig2_land_world_ts) %>%
  mutate(component = factor(component, levels = fig2_component_levels))

fig2_actual_r5_2100 <- bind_rows(fig2_ag_r5_2100, fig2_land_r5_2100) %>%
  mutate(
    region_name = factor(region_name, levels = rev(c("OECD+EU", "Asia", "Africa/Middle East", "Latin America", "Former Soviet Union"))),
    component = factor(component, levels = fig2_component_levels)
  ) %>%
  filter(!is.na(region_name))

fig2_actual_country_2100 <- bind_rows(fig2_ag_country_2100, fig2_land_country_2100) %>%
  left_join(st_drop_geometry(Boundary), by = c("region" = "cty")) %>%
  mutate(country = coalesce(country_name, region), component = factor(component, levels = fig2_component_levels))

afolu_rate_country_fig6 <- fig2_actual_country_2100 %>%
  group_by(region, country) %>%
  summarise(actual_reduction = sum(value, na.rm = TRUE), .groups = "drop") %>%
  left_join(
    fig1_baseline_country_2100 %>%
      group_by(region, country) %>%
      summarise(baseline_emissions = sum(value, na.rm = TRUE), .groups = "drop"),
    by = c("region", "country")
  ) %>%
  mutate(
    year = focus_year,
    reduction_rate = if_else(is.finite(baseline_emissions) & baseline_emissions != 0, 100 * actual_reduction / baseline_emissions, NA_real_)
  ) %>%
  select(region, country, year, actual_reduction, baseline_emissions, reduction_rate)

afolu_ag_rate_country_fig6 <- fig2_ag_country_2100 %>%
  left_join(
    fig1_ag_country_2100 %>%
      rename(baseline_emissions = value),
    by = c("region", "component")
  ) %>%
  group_by(region) %>%
  summarise(
    actual_reduction = sum(value, na.rm = TRUE),
    baseline_emissions = sum(baseline_emissions, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(
    fig2_actual_country_2100 %>%
      select(region, country) %>%
      distinct(),
    by = "region"
  ) %>%
  mutate(
    year = focus_year,
    reduction_rate = if_else(is.finite(baseline_emissions) & baseline_emissions != 0, 100 * actual_reduction / baseline_emissions, NA_real_)
  ) %>%
  select(region, country, year, actual_reduction, baseline_emissions, reduction_rate)

afolu_actual_world_ts <- bind_rows(
  GEER_AGMIP_AGG %>%
    filter(region == "World", variable == "EMIS", item == "AGR", unit == "MtCO2e") %>%
    group_by(year) %>%
    summarise(value = sum(value[scenario == scenario_baseline], na.rm = TRUE) - sum(value[scenario == scenario_landint], na.rm = TRUE), .groups = "drop") %>%
    transmute(year, component = "Agriculture", value),
  land_change_plus_mitigation_afr_decadal %>%
    filter(region == "World") %>%
    group_by(year) %>%
    summarise(value = -sum(plot_value, na.rm = TRUE), .groups = "drop") %>%
    transmute(year, component = "LULUCF", value)
)

afolu_actual_r5_2100 <- afolu_world_r5 %>%
  filter(region != "World", year == focus_year) %>%
  transmute(
    region_name = factor(region, levels = rev(c("OECD+EU", "Asia", "Africa/Middle East", "Latin America", "Former Soviet Union"))),
    component,
    value
  ) %>%
  filter(!is.na(region_name))

afolu_actual_country_decadal <- afolu_country %>%
  filter(year %in% decadal_years) %>%
  left_join(st_drop_geometry(Boundary), by = c("region" = "cty")) %>%
  mutate(country = coalesce(country_name, region))

afolu_top20_2100 <- fig2_actual_country_2100 %>%
  group_by(region, country) %>%
  summarise(total_value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total_value)) %>%
  slice_head(n = top_n)

afolu_top20_order <- afolu_top20_2100$country

fig1_top_regions <- fig1_baseline_country_2100 %>%
  group_by(region, country) %>%
  summarise(total_value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total_value)) %>%
  slice_head(n = top_n)

fig1_top_df <- fig1_baseline_country_2100 %>%
  filter(region %in% fig1_top_regions$region) %>%
  mutate(country = factor(country, levels = fig1_top_regions$country))

# figure blocks ------------------------------------------------------------

# ---- Fig 0: Carbon price and energy crop demand
fig0_ctax_df <- GEER_CTAX %>%
  filter(region == "USA", item == "TOT", unit == "USD/tCO2e", scenario == scenario_landint)

fig0_ecp_df <- GEER_AGMIP_AGG %>%
  filter(region == "World", variable == "PROD", item == "ECP", unit == "1000 t", scenario == scenario_landint) %>%
  mutate(value = value / 1000)

p_fig0_ctax <- ggplot(fig0_ctax_df, aes(x = year, y = value)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = decadal_years) +
  labs(x = NULL, y = "Carbon price (USD/tCO2)") +
  theme_bw(base_size = 11) +
  theme(
    panel.grid.minor = element_blank()
  )

p_fig0_ecp <- ggplot(fig0_ecp_df, aes(x = year, y = value)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = decadal_years) +
  labs(x = "Year", y = "Energy crop production (million t)") +
  theme_bw(base_size = 11) +
  theme(
    panel.grid.minor = element_blank()
  )

fig0 <- wrap_plots(
  p_fig0_ctax,
  p_fig0_ecp,
  ncol = 1
) + plot_annotation(tag_levels = list(c("a", "b")))

save_plot("fig0_carbon_price_and_energy_crop.png", fig0, width = 4, height = 7)

# ---- Fig 1: BaU AFOLU emissions
fig1 <- wrap_plots(
  make_world_ts_panel(fig1_baseline_world_ts, "", palette = fig1_component_palette, limits = fig1_component_levels),
  make_top_country_share_panel(
    fig1_top_df %>% group_by(region, country, component) %>% summarise(value = sum(value, na.rm = TRUE), .groups = "drop"),
    "",
    full_total_sum = fig1_baseline_country_2100 %>%
      group_by(region) %>%
      summarise(total_value = sum(value, na.rm = TRUE), .groups = "drop") %>%
      summarise(total_sum = sum(total_value, na.rm = TRUE), .groups = "drop") %>%
      pull(total_sum),
    palette = fig1_component_palette,
    limits = fig1_component_levels
  ),
  make_r5_panel(fig1_baseline_world_r5_2100, "", palette = fig1_component_palette, limits = fig1_component_levels),
  design = "AB\nCB",
  widths = c(1.0, 1.8),
  heights = c(1, 1)
) + plot_layout(guides = "collect") + plot_annotation(tag_levels = list(c("a", "c", "b"))) & theme(legend.position = "bottom")

save_plot("fig1_bau_afolu_emissions.png", fig1, width = 11, height = 6)

readr::write_csv(
  fig1_baseline_world_ts %>%
    filter(year %in% c(2050L, 2100L)) %>%
    group_by(year, component) %>%
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    mutate(region_name = "World") %>%
    select(region_name, year, component, value),
  file.path(output_dir, "fig1_world_component_summary_2050_2100.csv")
)

readr::write_csv(
  fig1_baseline_world_ts %>%
    filter(year %in% c(2050L, 2100L)) %>%
    group_by(year) %>%
    summarise(total_value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    mutate(region_name = "World") %>%
    select(region_name, year, total_value),
  file.path(output_dir, "fig1_world_total_summary_2050_2100.csv")
)

readr::write_csv(
  fig1_baseline_world_r5_2100 %>%
    group_by(region_name, component) %>%
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop"),
  file.path(output_dir, "fig1_r5_component_summary_2100.csv")
)

readr::write_csv(
  fig1_baseline_world_r5_2100 %>%
    group_by(region_name) %>%
    summarise(total_value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    mutate(share_pct_of_world = 100 * total_value / sum(fig1_baseline_world_ts %>% filter(year == focus_year) %>% pull(value), na.rm = TRUE)),
  file.path(output_dir, "fig1_r5_total_summary_2100.csv")
)

fig1_top_country_summary_2100 <- fig1_top_df %>%
  group_by(country, component) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  left_join(
    fig1_top_df %>%
      group_by(country) %>%
      summarise(total_value = sum(value, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(total_value)) %>%
      mutate(
        share_pct = 100 * total_value / sum(fig1_baseline_country_2100$value, na.rm = TRUE),
        cumulative_share_pct = cumsum(share_pct),
        rank = row_number()
      ),
    by = "country"
  ) %>%
  arrange(rank, component)

readr::write_csv(
  fig1_top_country_summary_2100,
  file.path(output_dir, "fig1_top_countries_summary_2100.csv")
)

fig2_top_df <- fig2_actual_country_2100 %>%
  filter(region %in% afolu_top20_2100$region) %>%
  mutate(country = factor(country, levels = afolu_top20_order))

# ---- Fig 2: AFOLU actual reduction
fig2 <- wrap_plots(
  make_world_ts_panel(fig2_actual_world_ts, "", palette = fig2_component_palette, limits = fig2_component_levels),
  make_top_country_share_panel(
    fig2_top_df,
    "",
    full_total_sum = fig2_actual_country_2100 %>%
      group_by(region) %>%
      summarise(total_value = sum(value, na.rm = TRUE), .groups = "drop") %>%
      summarise(total_sum = sum(total_value, na.rm = TRUE), .groups = "drop") %>%
      pull(total_sum),
    palette = fig2_component_palette,
    limits = fig2_component_levels
  ),
  make_r5_panel(fig2_actual_r5_2100, "", palette = fig2_component_palette, limits = fig2_component_levels),
  design = "AB\nCB",
  widths = c(1.0, 1.8),
  heights = c(1, 1)
) + plot_layout(guides = "collect") + plot_annotation(tag_levels = list(c("a", "c", "b"))) & theme(legend.position = "bottom")

save_plot("fig2_afolu_actual_reduction.png", fig2, width = 11, height = 6)

readr::write_csv(
  afolu_actual_world_ts %>%
    filter(year %in% c(2050L, 2100L)) %>%
    group_by(year, component) %>%
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    mutate(region_name = "World") %>%
    select(region_name, year, component, value),
  file.path(output_dir, "fig2_world_component_summary_2050_2100.csv")
)

readr::write_csv(
  afolu_actual_world_ts %>%
    filter(year %in% c(2050L, 2100L)) %>%
    group_by(year) %>%
    summarise(total_value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    mutate(region_name = "World") %>%
    select(region_name, year, total_value),
  file.path(output_dir, "fig2_world_total_summary_2050_2100.csv")
)

readr::write_csv(
  fig2_actual_r5_2100 %>%
    group_by(region_name, component) %>%
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop"),
  file.path(output_dir, "fig2_r5_component_summary_2100.csv")
)

readr::write_csv(
  fig2_actual_r5_2100 %>%
    group_by(region_name) %>%
    summarise(total_value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    mutate(share_pct_of_world = 100 * total_value / sum(fig2_actual_world_ts %>% filter(year == focus_year) %>% pull(value), na.rm = TRUE)),
  file.path(output_dir, "fig2_r5_total_summary_2100.csv")
)

fig2_top_country_summary_2100 <- fig2_actual_country_2100 %>%
  group_by(country, component) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  left_join(
    afolu_top20_2100 %>%
      transmute(
        country,
        total_value,
        share_pct = 100 * total_value / sum(fig2_actual_country_2100$value, na.rm = TRUE),
        cumulative_share_pct = cumsum(100 * total_value / sum(fig2_actual_country_2100$value, na.rm = TRUE)),
        rank = row_number()
      ),
    by = "country"
  ) %>%
  arrange(rank, component)

readr::write_csv(
  fig2_top_country_summary_2100,
  file.path(output_dir, "fig2_top_countries_summary_2100.csv")
)

fig2_cumulative_share_summary <- fig2_actual_country_2100 %>%
  group_by(region, country) %>%
  summarise(total_value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total_value)) %>%
  mutate(
    rank = row_number(),
    share_pct = 100 * total_value / sum(total_value, na.rm = TRUE),
    cumulative_share_pct = cumsum(share_pct)
  ) %>%
  filter(rank %in% c(5, 10, 15, 20)) %>%
  transmute(
    top_n = rank,
    cumulative_share_pct = cumulative_share_pct
  )

readr::write_csv(
  fig2_cumulative_share_summary,
  file.path(output_dir, "fig2_cumulative_share_summary.csv")
)

# ---- Fig 2a: AFOLU reduction rate
fig2_rate_world_ts <- fig2_actual_world_ts %>%
  group_by(year, component) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  left_join(
    fig1_baseline_world_ts %>%
      group_by(year) %>%
      summarise(baseline_total = sum(value, na.rm = TRUE), .groups = "drop"),
    by = "year"
  ) %>%
  mutate(value = if_else(baseline_total > 0, 100 * value / baseline_total, NA_real_)) %>%
  select(year, component, value)

fig2_rate_r5_2100 <- fig2_actual_r5_2100 %>%
  group_by(region_name, component) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  left_join(
    fig1_baseline_world_r5_2100 %>%
      group_by(region_name) %>%
      summarise(baseline_total = sum(value, na.rm = TRUE), .groups = "drop"),
    by = "region_name"
  ) %>%
  mutate(value = if_else(baseline_total > 0, 100 * value / baseline_total, NA_real_))

fig2_rate_country_2100 <- fig2_actual_country_2100 %>%
  group_by(region, country, component) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  left_join(
    fig1_baseline_country_2100 %>%
      group_by(region, country) %>%
      summarise(baseline_total = sum(value, na.rm = TRUE), .groups = "drop"),
    by = c("region", "country")
  ) %>%
  mutate(value = if_else(baseline_total > 0, 100 * value / baseline_total, NA_real_))

fig2_rate_top20 <- fig2_rate_country_2100 %>%
  group_by(region, country) %>%
  summarise(total_rate = sum(value, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total_rate)) %>%
  slice_head(n = top_n)

fig2_rate_top_df <- fig2_rate_country_2100 %>%
  filter(region %in% fig2_rate_top20$region) %>%
  mutate(country = factor(country, levels = fig2_rate_top20$country))

fig2a <- wrap_plots(
  make_world_ts_panel(fig2_rate_world_ts, "", palette = fig2_component_palette, limits = fig2_component_levels) +
    labs(y = "AFOLU mitigation rate (%)"),
  make_horizontal_stack_with_point(
    fig2_rate_top_df %>% select(country, component, value),
    fig2_rate_top20 %>% transmute(country = factor(country, levels = fig2_rate_top20$country), total_value = total_rate),
    "",
    "AFOLU mitigation rate (%)"
  ) +
    scale_fill_manual(values = fig2_component_palette, limits = fig2_component_levels),
  make_r5_panel(fig2_rate_r5_2100, "", palette = fig2_component_palette, limits = fig2_component_levels) +
    labs(y = "AFOLU mitigation rate (%)"),
  design = "AB\nCB",
  widths = c(1.0, 1.8),
  heights = c(1, 1)
) + plot_layout(guides = "collect") + plot_annotation(tag_levels = list(c("a", "c", "b"))) & theme(legend.position = "bottom")

save_plot("fig2a_afolu_reduction_rate.png", fig2a, width = 11, height = 6)

ag_emissions_change_2100 <- agmip_emis_actual_reduction %>%
  filter(region %in% country_codes, year == focus_year, item %in% names(fig2_ag_source_map)) %>%
  mutate(component = recode(item, !!!fig2_ag_source_map)) %>%
  group_by(region, component) %>%
  summarise(value = -sum(actual_reduction, na.rm = TRUE), .groups = "drop") %>%
  left_join(
    agmip_emis_actual_reduction %>%
      filter(region %in% country_codes, year == focus_year, item != "TOT") %>%
      group_by(region) %>%
      summarise(total_value = -sum(actual_reduction, na.rm = TRUE), .groups = "drop"),
    by = "region"
  ) %>%
  filter(total_value > 0) %>%
  left_join(st_drop_geometry(Boundary), by = c("region" = "cty")) %>%
  mutate(country = coalesce(country_name, region))

ag_emissions_increase_order <- ag_emissions_change_2100 %>%
  distinct(region, country, total_value) %>%
  arrange(desc(total_value)) %>%
  pull(country)

# ---- Fig 2b: Countries with increased agricultural emissions under MITI
fig2b <- make_horizontal_stack_with_point(
  ag_emissions_change_2100 %>%
    mutate(country = factor(country, levels = rev(ag_emissions_increase_order))) %>%
    transmute(country, component, value),
  ag_emissions_change_2100 %>%
    distinct(region, country, total_value) %>%
    mutate(country = factor(country, levels = rev(ag_emissions_increase_order))) %>%
    transmute(country, total_value),
  "",
  "EMIS AGR increase under MITI relative to BaU (MtCO2eq/yr)"
) +
  scale_fill_manual(
    values = fig2_component_palette[names(fig2_component_palette) %in% c(
      "Enteric fermentation",
      "Manure management",
      "Rice cultivation",
      "Managed soils"
    )],
    limits = c("Enteric fermentation", "Manure management", "Rice cultivation", "Managed soils"),
    drop = FALSE
  )

save_plot("fig2b_ag_emissions_increase_top_countries_2100.png", fig2b, width = 8, height = 8)

land_conversion_increase_2100 <- GEER_AFOLUB %>%
  filter(
    region %in% country_codes,
    year == focus_year,
    scenario %in% c(scenario_no, scenario_landint),
    variable == "Emi_CO2_Lan_Use_Flo_Pos_Emi_Lan_Use_Cha"
  ) %>%
  select(region, scenario, value) %>%
  pivot_wider(names_from = scenario, values_from = value) %>%
  mutate(change = .data[[scenario_landint]] - .data[[scenario_no]]) %>%
  filter(change > 0) %>%
  left_join(st_drop_geometry(Boundary), by = c("region" = "cty")) %>%
  mutate(country = coalesce(country_name, region))

land_conversion_increase_order <- land_conversion_increase_2100 %>%
  arrange(desc(change)) %>%
  pull(country)

p_land_conv_increase <- ggplot(
  land_conversion_increase_2100 %>%
    mutate(country = factor(country, levels = rev(land_conversion_increase_order))),
  aes(x = change, y = country)
) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
  geom_col(fill = "#2E7D32") +
  geom_point(
    aes(x = change, y = country),
    shape = 21,
    size = 2.2,
    stroke = 0.35,
    fill = "black",
    color = "black"
  ) +
  labs(x = "Land conversion emissions increase from BaU (MtCO2eq/yr)", y = NULL) +
  theme_main

p_land_change_for_increase <- ggplot(
  agmip_land_long %>%
    filter(year == focus_year, region %in% land_conversion_increase_2100$region) %>%
    left_join(land_conversion_increase_2100 %>% select(region, country), by = "region") %>%
    mutate(country = factor(country, levels = rev(land_conversion_increase_order))),
  aes(x = land_share, y = country, fill = land_item)
) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
  geom_col() +
  scale_fill_manual(values = land_color) +
  labs(x = "% of total land area", y = NULL) +
  theme_main

# ---- Fig 2c: Countries with increased land-conversion emissions under MITI
fig2c <- wrap_plots(p_land_conv_increase, p_land_change_for_increase, ncol = 2, widths = c(1.0, 1.35)) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "a") &
  theme(legend.position = "bottom")

save_plot("fig2c_land_conversion_increase_top_countries_2100.png", fig2c, width = 12, height = 8)

prod_kcal_change_2100 <- GEER_AGMIP %>%
  filter(
    region %in% afolu_top20_2100$region,
    year == focus_year,
    scenario %in% c(scenario_baseline, scenario_landint),
    variable == "PROD",
    item %in% c("AGR", "CRP", "LSP"),
    unit == "million kcal"
  ) %>%
  select(region, item, scenario, value) %>%
  pivot_wider(names_from = scenario, values_from = value) %>%
  mutate(change = .data[[scenario_landint]] - .data[[scenario_baseline]]) %>%
  left_join(st_drop_geometry(Boundary), by = c("region" = "cty")) %>%
  mutate(country = factor(coalesce(country_name, region), levels = rev(afolu_top20_order)))

p_prod <- make_horizontal_stack_with_point(
  prod_kcal_change_2100 %>%
    filter(item %in% c("CRP", "LSP")) %>%
    mutate(component = recode(item, CRP = "Crops", LSP = "Livestock")) %>%
    transmute(country, component, value = change),
  prod_kcal_change_2100 %>%
    filter(item == "AGR") %>%
    transmute(country, total_value = change),
  "",
  "Production (million kcal)"
)

cali_change_2100 <- GEER_AGMIP %>%
  filter(
    region %in% afolu_top20_2100$region,
    year == focus_year,
    scenario %in% c(scenario_baseline, scenario_landint),
    variable == "CALI",
    item %in% c("AGR", "CRP", "LSP"),
    unit == "kcal/cap/d"
  ) %>%
  select(region, item, scenario, value) %>%
  pivot_wider(names_from = scenario, values_from = value) %>%
  mutate(change = .data[[scenario_landint]] - .data[[scenario_baseline]]) %>%
  left_join(st_drop_geometry(Boundary), by = c("region" = "cty")) %>%
  mutate(country = factor(coalesce(country_name, region), levels = rev(afolu_top20_order)))

p_cali <- make_horizontal_stack_with_point(
  cali_change_2100 %>%
    filter(item %in% c("CRP", "LSP")) %>%
    mutate(component = recode(item, CRP = "Crops", LSP = "Livestock")) %>%
    transmute(country, component, value = change),
  cali_change_2100 %>%
    filter(item == "AGR") %>%
    transmute(country, total_value = change),
  "",
  "Calorie intake (kcal/cap/d)"
)

p_land_share <- ggplot(
  agmip_land_long %>%
    filter(year == focus_year, region %in% afolu_top20_2100$region) %>%
    left_join(afolu_top20_2100 %>% select(region, country), by = "region") %>%
    mutate(country = factor(country, levels = rev(afolu_top20_order))),
  aes(x = land_share, y = country, fill = land_item)
) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
  geom_col() +
  scale_fill_manual(values = land_color, guide = guide_legend(nrow = 3, byrow = TRUE)) +
  labs(title = NULL, x = "Land-use change (% of total land area)", y = NULL) +
  theme_main

# ---- Fig 3: Top AFOLU reducers and associated changes
fig3 <- wrap_plots(p_prod, p_cali, p_land_share, ncol = 3, widths = c(1.1, 1.1, 1.4)) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "a") &
  theme(legend.position = "bottom")

save_plot("fig3_top_reduction_country_changes_2100.png", fig3, width = 14, height = 7)

ag_cost_2100 <- GEER_AGMIP %>%
  filter(
    region %in% afolu_top20_2100$region,
    year == focus_year,
    scenario == scenario_landint,
    variable == "RedCos",
    item == "AGR",
    unit == "USD"
  ) %>%
  group_by(region) %>%
  summarise(value = sum(value, na.rm = TRUE) / 1e12, .groups = "drop") %>%
  mutate(component = "Agriculture")

land_cost_2100 <- GEER_AFOLUB %>%
  filter(
    region %in% afolu_top20_2100$region,
    scenario == scenario_landint,
    year == focus_year,
    variable %in% c(
      "Red_Cos_AFO_Lan_Pla",
      "Red_Cos_AFO_Lan_Ref",
      "Red_Cos_AFO_Lan_FPr",
      "Red_Cos_AFO_Lan_Pff",
      "Red_Cos_AFO_Lan_Ril",
      "Red_Cos_AFO_Lan_Enr",
      "Red_Cos_AFO_Lan_Agf",
      "Red_Cos_AFO_Lan_Wmp",
      "Red_Cos_AFO_Lan_Prr",
      "Red_Cos_AFO_Lan_Bch",
      "Red_Cos_AFO_Lan_Scc",
      "Red_Cos_AFO_Lan_Scg"
    )
  ) %>%
  group_by(region) %>%
  summarise(value = sum(value, na.rm = TRUE) / 1e9, .groups = "drop") %>%
  mutate(component = "LULUCF")

cost_2100 <- bind_rows(ag_cost_2100, land_cost_2100) %>%
  left_join(st_drop_geometry(Boundary), by = c("region" = "cty")) %>%
  mutate(country = factor(coalesce(country_name, region), levels = rev(afolu_top20_order)))

cost_palette <- c(
  "Agriculture" = "#B56576",
  "LULUCF" = "#2E7D32"
)

p_cost <- ggplot(cost_2100, aes(x = value, y = country, fill = component)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
  geom_col() +
  geom_point(
    data = cost_2100 %>% group_by(country) %>% summarise(total_value = sum(value, na.rm = TRUE), .groups = "drop"),
    aes(x = total_value, y = country),
    inherit.aes = FALSE,
    shape = 21,
    size = 2.2,
    stroke = 0.35,
    fill = "black",
    color = "black"
  ) +
  scale_fill_manual(values = cost_palette) +
  labs(title = NULL, x = "Mitigation cost (billion USD/yr)", y = NULL) +
  theme_main

# ---- Fig 3a: Fig 3 plus mitigation cost
fig3a <- wrap_plots(p_prod, p_cali, p_land_share, p_cost, ncol = 4, widths = c(1.05, 1.05, 1.25, 1.05)) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "a") &
  theme(legend.position = "bottom", legend.text = element_text(size = 8.5))

save_plot("fig3a_top_reduction_country_changes_with_cost_2100.png", fig3a, width = 13, height = 5)

prod_detail_items <- c("WHT", "RIC", "CGR", "OSD", "SGC", "VFN", "OCR", "RUM", "NRM", "DRY")
prod_detail_labels <- c(
  WHT = "Wheat",
  RIC = "Rice",
  CGR = "Coarse grains",
  OSD = "Oilseeds",
  SGC = "Sugar crops",
  VFN = "Vegetables & fruits",
  OCR = "Other crops",
  RUM = "Ruminant",
  NRM = "Non-ruminant",
  DRY = "Dairy"
)
prod_detail_palette <- c(
  "Wheat" = "#4C78A8",
  "Rice" = "#8E6CBE",
  "Coarse grains" = "#2F6690",
  "Oilseeds" = "#5DA5DA",
  "Sugar crops" = "#6C8EBF",
  "Vegetables & fruits" = "#2A9D8F",
  "Other crops" = "#9E9E9E",
  "Ruminant" = "#B2182B",
  "Non-ruminant" = "#F4A261",
  "Dairy" = "#E9C46A"
)

prod_kcal_detail_change_2100 <- GEER_AGMIP %>%
  filter(
    region %in% afolu_top20_2100$region,
    year == focus_year,
    scenario %in% c(scenario_baseline, scenario_landint),
    variable == "PROD",
    item %in% c("AGR", prod_detail_items),
    unit == "million kcal"
  ) %>%
  select(region, item, scenario, value) %>%
  pivot_wider(names_from = scenario, values_from = value) %>%
  mutate(change = .data[[scenario_landint]] - .data[[scenario_baseline]]) %>%
  left_join(st_drop_geometry(Boundary), by = c("region" = "cty")) %>%
  mutate(country = factor(coalesce(country_name, region), levels = rev(afolu_top20_order)))

p_prod_detail <- ggplot(
  prod_kcal_detail_change_2100 %>%
    filter(item %in% prod_detail_items) %>%
    mutate(component = factor(recode(item, !!!prod_detail_labels), levels = unname(prod_detail_labels))),
  aes(x = change, y = country, fill = component)
) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
  geom_col() +
  geom_point(
    data = prod_kcal_detail_change_2100 %>%
      filter(item == "AGR") %>%
      transmute(country, total_value = change),
    aes(x = total_value, y = country),
    inherit.aes = FALSE,
    shape = 21,
    size = 2.2,
    stroke = 0.35,
    fill = "black",
    color = "black"
  ) +
  scale_fill_manual(
    values = prod_detail_palette,
    limits = unname(prod_detail_labels),
    drop = FALSE,
    guide = guide_legend(nrow = 3, byrow = TRUE)
  ) +
  labs(title = NULL, x = "Production (million kcal)", y = NULL) +
  theme_main

# ---- Fig 3a2: Fig 3a with detailed commodity production changes
fig3a2 <- wrap_plots(p_prod_detail, p_cali, p_land_share, p_cost, ncol = 4, widths = c(1.55, 1.05, 1.25, 1.1)) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "a")  &
  theme(legend.position = "bottom", legend.text = element_text(size = 8.5))

save_plot("fig3a2_top_reduction_country_changes_with_detailed_prod_cost_2100.png", fig3a2, width = 13, height = 5)

readr::write_csv(
  prod_kcal_detail_change_2100 %>%
    mutate(
      country = as.character(country),
      item_label = recode(item, !!!prod_detail_labels, AGR = "AGR")
    ) %>%
    select(region, country, item, item_label, change),
  file.path(output_dir, "fig3a2_production_change_summary_2100.csv")
)

readr::write_csv(
  cali_change_2100 %>%
    mutate(
      country = as.character(country),
      item_label = recode(item, AGR = "AGR", CRP = "Crops", LSP = "Livestock")
    ) %>%
    select(region, country, item, item_label, change),
  file.path(output_dir, "fig3a2_calorie_change_summary_2100.csv")
)

readr::write_csv(
  agmip_land_long %>%
    filter(year == focus_year, region %in% afolu_top20_2100$region) %>%
    left_join(afolu_top20_2100 %>% select(region, country), by = "region") %>%
    select(region, country, land_item, land_share),
  file.path(output_dir, "fig3a2_land_change_summary_2100.csv")
)

readr::write_csv(
  cost_2100 %>%
    mutate(country = as.character(country)) %>%
    select(region, country, component, value),
  file.path(output_dir, "fig3a2_cost_summary_2100.csv")
)

xprc_change_2100 <- GEER_AGMIP %>%
  filter(
    region %in% afolu_top20_2100$region,
    year == focus_year,
    scenario %in% c(scenario_baseline, scenario_landint),
    variable == "XPRC",
    item %in% c("AGR", "CRP", "LSP"),
    unit == "USD/t"
  ) %>%
  select(region, item, scenario, value) %>%
  pivot_wider(names_from = scenario, values_from = value) %>%
  mutate(
    value = if_else(
      is.finite(.data[[scenario_baseline]]) & .data[[scenario_baseline]] != 0,
      100 * (.data[[scenario_landint]] / .data[[scenario_baseline]] - 1),
      NA_real_
    )
  ) %>%
  left_join(st_drop_geometry(Boundary), by = c("region" = "cty")) %>%
  mutate(
    country = factor(coalesce(country_name, region), levels = rev(afolu_top20_order)),
    item = recode(item, CRP = "Crops", LSP = "Livestock"),
    item = factor(item, levels = c("Crops", "Livestock"))
  )

# ---- Fig 3b: XPRC change for crops and livestock
fig3b <- ggplot(xprc_change_2100 %>% filter(item %in% c("Crops", "Livestock")), aes(x = value, y = country)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
  geom_col(fill = "#4C78A8") +
  facet_wrap(~ item, nrow = 1, scales = "free_x") +
  labs(x = "Consumer price change under MITI relative to BaU (%)", y = NULL) +
  theme_main

save_plot("fig3b_xprc_change_2100.png", fig3b, width = 5, height = 4)

readr::write_csv(
  xprc_change_2100 %>%
    filter(item %in% c("Crops", "Livestock")) %>%
    mutate(country = as.character(country)) %>%
    select(region, country, item, value),
  file.path(output_dir, "fig3b_xprc_change_summary_2100.csv")
)

make_prod_growth_vs_ef_bubble <- function(item_code, panel_title) {
  emis_items <- if (item_code == "CRP") c("CRS", "RCC", "SFR", "MGR", "MAS") else c("ENT", "MMG")

  ef_df <- GEER_AGMIP %>%
    filter(
      region %in% country_codes,
      year == focus_year,
      scenario == scenario_landint,
      (
        variable == "EMIS" & item %in% emis_items & unit == "MtCO2e"
      ) |
        (
          variable == "PROD" & item == item_code & unit == "1000 t"
        )
    ) %>%
    mutate(measure = case_when(
      variable == "EMIS" ~ "emis",
      variable == "PROD" ~ "prod",
      TRUE ~ NA_character_
    )) %>%
    group_by(region, measure) %>%
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = measure, values_from = value) %>%
    transmute(region, ef_miti = if_else(prod > 0, emis / prod, NA_real_))

  growth_df <- GEER_AGMIP %>%
    filter(
      region %in% country_codes,
      year == focus_year,
      scenario %in% c(scenario_baseline, scenario_landint),
      variable == "PROD",
      item == item_code,
      unit == "1000 t"
    ) %>%
    select(region, scenario, value) %>%
    pivot_wider(names_from = scenario, values_from = value) %>%
    transmute(
      region,
      prod_growth = if_else(
        .data[[scenario_baseline]] > 0,
        100 * (.data[[scenario_landint]] / .data[[scenario_baseline]] - 1),
        NA_real_
      )
    )

  bubble_df <- afolu_country %>%
    group_by(region, year) %>%
    summarise(afolu_reduction = sum(value, na.rm = TRUE), .groups = "drop") %>%
    filter(region %in% country_codes, year == focus_year) %>%
    left_join(ef_df, by = "region") %>%
    left_join(growth_df, by = "region") %>%
    left_join(region_map, by = "region") %>%
    left_join(st_drop_geometry(Boundary), by = c("region" = "cty")) %>%
    mutate(country = coalesce(country_name, region), region5 = factor(region5, levels = names(region_palette)))

  bubble_df <- bubble_df %>%
    fixed_afolu_bubble_size() %>%
    filter(is.finite(ef_miti), is.finite(prod_growth), !is.na(region5))

  size_levels <- levels(droplevels(bubble_df$size_bin))
  label_levels <- tail(size_levels, min(2, length(size_levels)))
  label_df <- bubble_df %>% filter(size_bin %in% label_levels)

  ggplot(bubble_df, aes(x = ef_miti, y = prod_growth)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
    geom_point(aes(size = size_bin, fill = region5, color = region5, alpha = size_bin), shape = 21, stroke = 0.35) +
    geom_text(
      data = label_df,
      aes(label = country, color = region5),
      size = 3.2,
      fontface = "bold",
      nudge_y = 0.03 * max(abs(bubble_df$prod_growth), na.rm = TRUE),
      show.legend = FALSE
    ) +
    scale_fill_manual(values = region_palette) +
    scale_color_manual(values = region_border_palette) +
    scale_size_manual(values = c(3, 4.8, 6.4, 8), drop = FALSE) +
    scale_alpha_manual(values = setNames(c(0.58, 0.66, 0.74, 0.82), size_levels), guide = "none", drop = FALSE) +
    labs(
      title = NULL,
      x = "Emission factor under MITI (MtCO2eq / 1000 t)",
      y = "Production increase from BaU (%)"
    ) +
    theme_main
}

# ---- Fig 3c: Production growth vs emission factor
fig3c <- wrap_plots(
  make_prod_growth_vs_ef_bubble("CRP", "CRP"),
  make_prod_growth_vs_ef_bubble("LSP", "LSP"),
  ncol = 2
) + plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "a") &
  theme(legend.position = "bottom", legend.box = "vertical")

save_plot("fig3c_prod_growth_vs_emission_factor_2100.png", fig3c, width = 12, height = 6)

world_qsup_prices <- GEER_WORLD_QSUP %>%
  transmute(
    scenario = as.character(SCENARIO),
    commodity = as.character(c),
    region = as.character(cty),
    year = as.integer(as.character(Y)),
    qsup = as.numeric(QSUP_load)
  ) %>%
  filter(
    scenario %in% c(scenario_baseline, scenario_landint),
    region %in% country_codes,
    year == focus_year
  ) %>%
  left_join(c_map, by = "commodity")

world_price_decomp <- GEER_WORLD_PRICE_DECOMP %>%
  transmute(
    scenario = as.character(SCENARIO),
    commodity = as.character(c),
    price_var = as.character(price_var),
    region = as.character(cty),
    year = as.integer(as.character(Y)),
    price_value = as.numeric(Price_decomp)
  ) %>%
  filter(
    scenario %in% c(scenario_baseline, scenario_landint),
    region %in% country_codes,
    year == focus_year,
    price_var %in% c("LAND", "other", "reduction_cost", "residual_emission", "intermediate", "FEED")
  ) %>%
  left_join(c_map, by = "commodity")

price_decomp_grouped <- bind_rows(
  world_price_decomp %>%
    filter(!is.na(group)) %>%
    left_join(world_qsup_prices %>% filter(!is.na(group)) %>% select(scenario, commodity, region, year, qsup, group),
              by = c("scenario", "commodity", "region", "year", "group")) %>%
    group_by(region, year, scenario, price_var, item = group) %>%
    summarise(value = if_else(sum(qsup, na.rm = TRUE) > 0, sum(price_value * qsup, na.rm = TRUE) / sum(qsup, na.rm = TRUE), NA_real_), .groups = "drop"),
  world_price_decomp %>%
    left_join(world_qsup_prices %>% select(scenario, commodity, region, year, qsup), by = c("scenario", "commodity", "region", "year")) %>%
    group_by(region, year, scenario, price_var) %>%
    summarise(value = if_else(sum(qsup, na.rm = TRUE) > 0, sum(price_value * qsup, na.rm = TRUE) / sum(qsup, na.rm = TRUE), NA_real_), .groups = "drop") %>%
    mutate(item = "AGR")
) %>%
  mutate(
    price_var = recode(
      price_var,
      LAND = "LAND",
      other = "other",
      reduction_cost = "reduction_cost",
      residual_emission = "residual_emission",
      intermediate = "intermediate",
      FEED = "FEED"
    )
  )

price_decomp_change_2100 <- price_decomp_grouped %>%
  filter(item %in% c("AGR", "CRP", "LSP")) %>%
  select(region, item, price_var, scenario, value) %>%
  pivot_wider(names_from = scenario, values_from = value, values_fill = 0) %>%
  mutate(change = .data[[scenario_landint]] - .data[[scenario_baseline]]) %>%
  left_join(st_drop_geometry(Boundary), by = c("region" = "cty")) %>%
  mutate(country = factor(coalesce(country_name, region), levels = rev(afolu_top20_order)))

xprc_abs_change_2100 <- GEER_AGMIP %>%
  filter(
    region %in% afolu_top20_2100$region,
    year == focus_year,
    scenario %in% c(scenario_baseline, scenario_landint),
    variable == "XPRC",
    item %in% c("AGR", "CRP", "LSP"),
    unit == "USD/t"
  ) %>%
  select(region, item, scenario, value) %>%
  pivot_wider(names_from = scenario, values_from = value) %>%
  mutate(total_value = .data[[scenario_landint]] - .data[[scenario_baseline]]) %>%
  left_join(st_drop_geometry(Boundary), by = c("region" = "cty")) %>%
  mutate(country = factor(coalesce(country_name, region), levels = rev(afolu_top20_order)))

price_var_palette <- c(
  "LAND" = "#2E7D32",
  "FEED" = "#4C78A8",
  "intermediate" = "#F28E2B",
  "residual_emission" = "#B56576",
  "reduction_cost" = "#8E6CBE",
  "other" = "#9D9D9D"
)

# ---- Fig 3d: XPRC decomposition by price drivers
fig3d <- ggplot(
  price_decomp_change_2100 %>% filter(region %in% afolu_top20_2100$region),
  aes(x = change, y = country, fill = price_var)
) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
  geom_col() +
  geom_point(
    data = xprc_abs_change_2100 %>% filter(region %in% afolu_top20_2100$region),
    aes(x = total_value, y = country),
    inherit.aes = FALSE,
    shape = 21,
    size = 2.2,
    stroke = 0.35,
    fill = "black",
    color = "black"
  ) +
  facet_wrap(~ item, nrow = 1, scales = "free_x") +
  scale_fill_manual(values = price_var_palette) +
  labs(x = "XPRC change from BaU (USD/t)", y = NULL) +
  theme_main

save_plot("fig3d_xprc_price_decomp_change_2100.png", fig3d, width = 14, height = 8)

price_decomp_residual_share_2100 <- price_decomp_grouped %>%
  filter(year == focus_year, scenario == scenario_landint, item %in% c("CRP", "LSP")) %>%
  group_by(region, item) %>%
  summarise(
    total_price = sum(value, na.rm = TRUE),
    residual_emission = sum(value[price_var == "residual_emission"], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    residual_share_pct = if_else(
      is.finite(total_price) & total_price != 0,
      100 * residual_emission / total_price,
      NA_real_
    )
  )

price_decomp_land_share_2100 <- price_decomp_grouped %>%
  filter(year == focus_year, scenario == scenario_landint, item %in% c("CRP", "LSP")) %>%
  group_by(region, item) %>%
  summarise(
    total_price = sum(value, na.rm = TRUE),
    land_component = sum(value[price_var == "LAND"], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    land_share_pct = if_else(
      is.finite(total_price) & total_price != 0,
      100 * land_component / total_price,
      NA_real_
    )
  )

price_decomp_reduction_cost_share_2100 <- price_decomp_grouped %>%
  filter(year == focus_year, scenario == scenario_landint, item %in% c("CRP", "LSP")) %>%
  group_by(region, item) %>%
  summarise(
    total_price = sum(value, na.rm = TRUE),
    reduction_cost_component = sum(value[price_var == "reduction_cost"], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    reduction_cost_share_pct = if_else(
      is.finite(total_price) & total_price != 0,
      100 * reduction_cost_component / total_price,
      NA_real_
    )
  )

price_decomp_land_growth_2100 <- price_decomp_grouped %>%
  filter(year == focus_year, item %in% c("CRP", "LSP"), price_var == "LAND") %>%
  select(region, item, scenario, value) %>%
  pivot_wider(names_from = scenario, values_from = value, values_fill = 0) %>%
  mutate(
    land_price_growth = if_else(
      is.finite(.data[[scenario_baseline]]) & .data[[scenario_baseline]] != 0,
      100 * (.data[[scenario_landint]] / .data[[scenario_baseline]] - 1),
      NA_real_
    )
  ) %>%
  select(region, item, land_price_growth)

cali_item_change_2100 <- GEER_AGMIP %>%
  filter(
    region %in% country_codes,
    year == focus_year,
    scenario %in% c(scenario_baseline, scenario_landint),
    variable == "CALI",
    item %in% c("CRP", "LSP"),
    unit == "kcal/cap/d"
  ) %>%
  select(region, item, scenario, value) %>%
  pivot_wider(names_from = scenario, values_from = value) %>%
  mutate(
    cali_change = .data[[scenario_landint]] - .data[[scenario_baseline]]
  ) %>%
  select(region, item, cali_change)

make_residual_share_vs_cali_bubble <- function(item_code) {
  bubble_df <- afolu_country %>%
    group_by(region, year) %>%
    summarise(afolu_reduction = sum(value, na.rm = TRUE), .groups = "drop") %>%
    filter(region %in% country_codes, year == focus_year) %>%
    left_join(price_decomp_residual_share_2100 %>% filter(item == item_code) %>% select(region, residual_share_pct), by = "region") %>%
    left_join(cali_item_change_2100 %>% filter(item == item_code) %>% select(region, cali_change), by = "region") %>%
    left_join(region_map, by = "region") %>%
    left_join(st_drop_geometry(Boundary), by = c("region" = "cty")) %>%
    mutate(country = coalesce(country_name, region), region5 = factor(region5, levels = names(region_palette)))

  bubble_df <- bubble_df %>%
    fixed_afolu_bubble_size() %>%
    filter(is.finite(residual_share_pct), is.finite(cali_change), !is.na(region5))

  size_levels <- levels(droplevels(bubble_df$size_bin))
  label_levels <- tail(size_levels, min(2, length(size_levels)))
  label_df <- bubble_df %>% filter(size_bin %in% label_levels)

  ggplot(bubble_df, aes(x = residual_share_pct, y = cali_change)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
    geom_point(aes(size = size_bin, fill = region5, color = region5, alpha = size_bin), shape = 21, stroke = 0.35) +
    geom_text(
      data = label_df,
      aes(label = country, color = region5),
      size = 3.2,
      fontface = "bold",
      nudge_y = 0.03 * max(abs(bubble_df$cali_change), na.rm = TRUE),
      show.legend = FALSE
    ) +
    scale_fill_manual(values = region_palette) +
    scale_color_manual(values = region_border_palette) +
    scale_size_manual(values = c(3, 4.8, 6.4, 8), drop = FALSE) +
    scale_alpha_manual(values = setNames(c(0.58, 0.66, 0.74, 0.82), size_levels), guide = "none", drop = FALSE) +
    labs(
      title = NULL,
      x = "Residual emission share in MITI price decomposition (%)",
      y = "CALI change from BaU (kcal/cap/d)"
    ) +
    theme_main
}

# ---- Fig 3e: Residual-emission share vs calorie change
fig3e <- wrap_plots(
  make_residual_share_vs_cali_bubble("CRP"),
  make_residual_share_vs_cali_bubble("LSP"),
  ncol = 2
) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "a") &
  theme(legend.position = "bottom")

save_plot("fig3e_residual_share_vs_cali_change_2100.png", fig3e, width = 12, height = 6)

make_residual_share_vs_cali_scatter <- function(item_code) {
  scatter_df <- afolu_country %>%
    group_by(region, year) %>%
    summarise(afolu_reduction = sum(value, na.rm = TRUE), .groups = "drop") %>%
    filter(region %in% country_codes, year == focus_year) %>%
    left_join(price_decomp_residual_share_2100 %>% filter(item == item_code) %>% select(region, residual_share_pct), by = "region") %>%
    left_join(cali_item_change_2100 %>% filter(item == item_code) %>% select(region, cali_change), by = "region") %>%
    left_join(region_map, by = "region") %>%
    left_join(st_drop_geometry(Boundary), by = c("region" = "cty")) %>%
    mutate(country = coalesce(country_name, region), region5 = factor(region5, levels = names(region_palette))) %>%
    fixed_afolu_bubble_size() %>%
    filter(is.finite(residual_share_pct), is.finite(cali_change), !is.na(region5))

  label_levels <- c("100-250 MtCO2eq/yr", ">= 250 MtCO2eq/yr")
  label_df <- scatter_df %>% filter(size_bin %in% label_levels)

  fit <- lm(cali_change ~ residual_share_pct, data = scatter_df)
  fit_sum <- summary(fit)
  r2 <- fit_sum$r.squared
  pval <- coef(fit_sum)[2, 4]
  p_text <- dplyr::case_when(
    !is.finite(pval) ~ "p = NA",
    pval < 0.001 ~ "p < 0.001",
    pval < 0.01 ~ "p < 0.01",
    pval < 0.05 ~ "p < 0.05",
    TRUE ~ "p >= 0.05"
  )
  ann_text <- sprintf("R^2 = %.2f\n%s", r2, p_text)

  ggplot(scatter_df, aes(x = residual_share_pct, y = cali_change, color = region5)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
    geom_point(size = 2.8, alpha = 0.8) +
    geom_smooth(method = "lm", se = TRUE, color = "black", linewidth = 0.6) +
    geom_text(
      data = label_df,
      aes(label = country),
      size = 3.0,
      fontface = "bold",
      nudge_y = 0.03 * max(abs(scatter_df$cali_change), na.rm = TRUE),
      show.legend = FALSE
    ) +
    annotate(
      "text",
      x = Inf, y = -Inf, label = ann_text,
      hjust = 1.02, vjust = -0.2, size = 3.1
    ) +
    scale_color_manual(values = region_border_palette) +
    labs(
      title = NULL,
      x = "Share of residual-emission pricing in domestic food price (%)",
      y = "Change in calorie intake from BaU (kcal/cap/d)"
    ) +
    theme_main
}

# ---- Fig 3e2: Residual-emission share vs calorie change with regression
fig3e2 <- wrap_plots(
  make_residual_share_vs_cali_scatter("CRP"),
  make_residual_share_vs_cali_scatter("LSP"),
  ncol = 1
) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "a") &
  theme(legend.position = "bottom")

save_plot("fig3e2_residual_share_vs_cali_scatter_2100.png", fig3e2, width = 7, height = 10.5)

make_land_share_vs_cali_bubble <- function(item_code) {
  bubble_df <- afolu_country %>%
    group_by(region, year) %>%
    summarise(afolu_reduction = sum(value, na.rm = TRUE), .groups = "drop") %>%
    filter(region %in% country_codes, year == focus_year) %>%
    left_join(price_decomp_land_share_2100 %>% filter(item == item_code) %>% select(region, land_share_pct), by = "region") %>%
    left_join(cali_item_change_2100 %>% filter(item == item_code) %>% select(region, cali_change), by = "region") %>%
    left_join(region_map, by = "region") %>%
    left_join(st_drop_geometry(Boundary), by = c("region" = "cty")) %>%
    mutate(country = coalesce(country_name, region), region5 = factor(region5, levels = names(region_palette)))

  bubble_df <- bubble_df %>% fixed_afolu_bubble_size() %>% filter(is.finite(land_share_pct), is.finite(cali_change), !is.na(region5))
  size_levels <- levels(droplevels(bubble_df$size_bin))
  label_levels <- tail(size_levels, min(2, length(size_levels)))
  label_df <- bubble_df %>% filter(size_bin %in% label_levels)

  ggplot(bubble_df, aes(x = land_share_pct, y = cali_change)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
    geom_point(aes(size = size_bin, fill = region5, color = region5, alpha = size_bin), shape = 21, stroke = 0.35) +
    geom_text(data = label_df, aes(label = country, color = region5), size = 3.2, fontface = "bold", nudge_y = 0.03 * max(abs(bubble_df$cali_change), na.rm = TRUE), show.legend = FALSE) +
    scale_fill_manual(values = region_palette) +
    scale_color_manual(values = region_border_palette) +
    scale_size_manual(values = c(3, 4.8, 6.4, 8), drop = FALSE) +
    scale_alpha_manual(values = setNames(c(0.58, 0.66, 0.74, 0.82), size_levels), guide = "none", drop = FALSE) +
    labs(title = NULL, x = "LAND share in MITI price decomposition (%)", y = "CALI change from BaU (kcal/cap/d)") +
    theme_main
}

# ---- Fig 3f: Land price share vs calorie change
fig3f <- wrap_plots(
  make_land_share_vs_cali_bubble("CRP"),
  make_land_share_vs_cali_bubble("LSP"),
  ncol = 2
) + plot_layout(guides = "collect") + plot_annotation(tag_levels = "a") & theme(legend.position = "bottom")

save_plot("fig3f_land_share_vs_cali_change_2100.png", fig3f, width = 12, height = 6)

make_reduction_cost_share_vs_cali_bubble <- function(item_code) {
  bubble_df <- afolu_country %>%
    group_by(region, year) %>%
    summarise(afolu_reduction = sum(value, na.rm = TRUE), .groups = "drop") %>%
    filter(region %in% country_codes, year == focus_year) %>%
    left_join(price_decomp_reduction_cost_share_2100 %>% filter(item == item_code) %>% select(region, reduction_cost_share_pct), by = "region") %>%
    left_join(cali_item_change_2100 %>% filter(item == item_code) %>% select(region, cali_change), by = "region") %>%
    left_join(region_map, by = "region") %>%
    left_join(st_drop_geometry(Boundary), by = c("region" = "cty")) %>%
    mutate(country = coalesce(country_name, region), region5 = factor(region5, levels = names(region_palette)))

  bubble_df <- bubble_df %>% fixed_afolu_bubble_size() %>% filter(is.finite(reduction_cost_share_pct), is.finite(cali_change), !is.na(region5))
  size_levels <- levels(droplevels(bubble_df$size_bin))
  label_levels <- tail(size_levels, min(2, length(size_levels)))
  label_df <- bubble_df %>% filter(size_bin %in% label_levels)

  ggplot(bubble_df, aes(x = reduction_cost_share_pct, y = cali_change)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
    geom_point(aes(size = size_bin, fill = region5, color = region5, alpha = size_bin), shape = 21, stroke = 0.35) +
    geom_text(data = label_df, aes(label = country, color = region5), size = 3.2, fontface = "bold", nudge_y = 0.03 * max(abs(bubble_df$cali_change), na.rm = TRUE), show.legend = FALSE) +
    scale_fill_manual(values = region_palette) +
    scale_color_manual(values = region_border_palette) +
    scale_size_manual(values = c(3, 4.8, 6.4, 8), drop = FALSE) +
    scale_alpha_manual(values = setNames(c(0.58, 0.66, 0.74, 0.82), size_levels), guide = "none", drop = FALSE) +
    labs(title = NULL, x = "Reduction cost share in MITI price decomposition (%)", y = "CALI change from BaU (kcal/cap/d)") +
    theme_main
}

# ---- Fig 3g: Reduction-cost share vs calorie change
fig3g <- wrap_plots(
  make_reduction_cost_share_vs_cali_bubble("CRP"),
  make_reduction_cost_share_vs_cali_bubble("LSP"),
  ncol = 2
) + plot_layout(guides = "collect") + plot_annotation(tag_levels = "a") & theme(legend.position = "bottom")

save_plot("fig3g_reduction_cost_share_vs_cali_change_2100.png", fig3g, width = 12, height = 6)

make_land_growth_vs_prod_bubble <- function(item_code) {
  prod_growth_df <- GEER_AGMIP %>%
    filter(
      region %in% country_codes,
      year == focus_year,
      scenario %in% c(scenario_baseline, scenario_landint),
      variable == "PROD",
      item == item_code,
      unit == "1000 t"
    ) %>%
    select(region, scenario, value) %>%
    pivot_wider(names_from = scenario, values_from = value) %>%
    transmute(
      region,
      prod_growth = if_else(.data[[scenario_baseline]] > 0, 100 * (.data[[scenario_landint]] / .data[[scenario_baseline]] - 1), NA_real_)
    )

  bubble_df <- afolu_country %>%
    group_by(region, year) %>%
    summarise(afolu_reduction = sum(value, na.rm = TRUE), .groups = "drop") %>%
    filter(region %in% country_codes, year == focus_year) %>%
    left_join(price_decomp_land_growth_2100 %>% filter(item == item_code) %>% select(region, land_price_growth), by = "region") %>%
    left_join(prod_growth_df, by = "region") %>%
    left_join(region_map, by = "region") %>%
    left_join(st_drop_geometry(Boundary), by = c("region" = "cty")) %>%
    mutate(country = coalesce(country_name, region), region5 = factor(region5, levels = names(region_palette)))

  bubble_df <- bubble_df %>% fixed_afolu_bubble_size() %>% filter(is.finite(land_price_growth), is.finite(prod_growth), !is.na(region5))
  size_levels <- levels(droplevels(bubble_df$size_bin))
  label_levels <- tail(size_levels, min(2, length(size_levels)))
  label_df <- bubble_df %>% filter(size_bin %in% label_levels)

  ggplot(bubble_df, aes(x = land_price_growth, y = prod_growth)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
    geom_point(aes(size = size_bin, fill = region5, color = region5, alpha = size_bin), shape = 21, stroke = 0.35) +
    geom_text(data = label_df, aes(label = country, color = region5), size = 3.2, fontface = "bold", nudge_y = 0.03 * max(abs(bubble_df$prod_growth), na.rm = TRUE), show.legend = FALSE) +
    scale_fill_manual(values = region_palette) +
    scale_color_manual(values = region_border_palette) +
    scale_size_manual(values = c(3, 4.8, 6.4, 8), drop = FALSE) +
    scale_alpha_manual(values = setNames(c(0.58, 0.66, 0.74, 0.82), size_levels), guide = "none", drop = FALSE) +
    labs(title = NULL, x = "LAND component increase from BaU (%)", y = "Production increase from BaU (%)") +
    theme_main
}

# ---- Fig 3h: Land price growth vs production growth
fig3h <- wrap_plots(
  make_land_growth_vs_prod_bubble("CRP"),
  make_land_growth_vs_prod_bubble("LSP"),
  ncol = 2
) + plot_layout(guides = "collect") + plot_annotation(tag_levels = "a") & theme(legend.position = "bottom")

save_plot("fig3h_land_growth_vs_prod_change_2100.png", fig3h, width = 12, height = 6)

xprc_agr_change_2100 <- GEER_AGMIP %>%
  filter(
    region %in% country_codes,
    year == focus_year,
    scenario %in% c(scenario_baseline, scenario_landint),
    variable == "XPRC",
    item == "AGR",
    unit == "USD/t"
  ) %>%
  select(region, scenario, value) %>%
  pivot_wider(names_from = scenario, values_from = value) %>%
  transmute(
    region,
    xprc_agr_change = if_else(
      .data[[scenario_baseline]] > 0,
      100 * (.data[[scenario_landint]] / .data[[scenario_baseline]] - 1),
      NA_real_
    )
  )

land_item_share_change_2100 <- GEER_AGMIP %>%
  filter(
    region %in% country_codes,
    year == focus_year,
    scenario %in% c(scenario_baseline, scenario_landint),
    variable == "LAND",
    item %in% c("FOR", "ECP", "TOT"),
    unit == "1000 ha"
  ) %>%
  select(region, item, scenario, value) %>%
  pivot_wider(names_from = scenario, values_from = value) %>%
  select(region, item, baseline = !!sym(scenario_baseline), miti = !!sym(scenario_landint)) %>%
  pivot_wider(names_from = item, values_from = c(baseline, miti)) %>%
  transmute(
    region,
    for_share_change = if_else(baseline_TOT > 0, 100 * (miti_FOR - baseline_FOR) / baseline_TOT, NA_real_),
    ecp_share_change = if_else(baseline_TOT > 0, 100 * (miti_ECP - baseline_ECP) / baseline_TOT, NA_real_),
    for_ecp_share_change = if_else(
      baseline_TOT > 0,
      100 * ((miti_FOR - baseline_FOR) + (miti_ECP - baseline_ECP)) / baseline_TOT,
      NA_real_
    )
  )

make_xprc_vs_land_share_bubble <- function(metric_col, x_label, y_label) {
  bubble_df <- afolu_country %>%
    group_by(region, year) %>%
    summarise(afolu_reduction = sum(value, na.rm = TRUE), .groups = "drop") %>%
    filter(region %in% country_codes, year == focus_year) %>%
    left_join(xprc_agr_change_2100, by = "region") %>%
    left_join(land_item_share_change_2100 %>% select(region, y = all_of(metric_col)), by = "region") %>%
    left_join(region_map, by = "region") %>%
    left_join(st_drop_geometry(Boundary), by = c("region" = "cty")) %>%
    mutate(country = coalesce(country_name, region), region5 = factor(region5, levels = names(region_palette)))

  bubble_df <- bubble_df %>%
    fixed_afolu_bubble_size() %>%
    filter(is.finite(xprc_agr_change), is.finite(y), !is.na(region5))

  size_levels <- levels(droplevels(bubble_df$size_bin))
  label_levels <- tail(size_levels, min(2, length(size_levels)))
  label_df <- bubble_df %>% filter(size_bin %in% label_levels)

  ggplot(bubble_df, aes(x = xprc_agr_change, y = y)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
    geom_point(aes(size = size_bin, fill = region5, color = region5, alpha = size_bin), shape = 21, stroke = 0.35) +
    geom_text(
      data = label_df,
      aes(label = country, color = region5),
      size = 3.2,
      fontface = "bold",
      nudge_y = 0.03 * max(abs(bubble_df$y), na.rm = TRUE),
      show.legend = FALSE
    ) +
    scale_fill_manual(values = region_palette) +
    scale_color_manual(values = region_border_palette) +
    scale_size_manual(values = c(3, 4.8, 6.4, 8), drop = FALSE) +
    scale_alpha_manual(values = setNames(c(0.58, 0.66, 0.74, 0.82), size_levels), guide = "none", drop = FALSE) +
    labs(title = NULL, x = x_label, y = y_label) +
    theme_main
}

# ---- Fig 3i: XPRC change vs land-share change
fig3i <- wrap_plots(
  make_xprc_vs_land_share_bubble(
    "for_share_change",
    "XPRC AGR increase from BaU (%)",
    "Forest area change / total land (%)"
  ),
  make_xprc_vs_land_share_bubble(
    "ecp_share_change",
    "XPRC AGR increase from BaU (%)",
    "Energy crop area change / total land (%)"
  ),
  ncol = 2
) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "a") &
  theme(legend.position = "bottom")

save_plot("fig3i_xprc_vs_land_share_change_2100.png", fig3i, width = 12, height = 6)

prod_agr_growth_2100 <- GEER_AGMIP %>%
  filter(
    region %in% country_codes,
    year == focus_year,
    scenario %in% c(scenario_baseline, scenario_landint),
    variable == "PROD",
    item == "AGR",
    unit == "million kcal"
  ) %>%
  select(region, scenario, value) %>%
  pivot_wider(names_from = scenario, values_from = value) %>%
  transmute(
    region,
    prod_agr_growth = if_else(
      .data[[scenario_baseline]] > 0,
      100 * (.data[[scenario_landint]] / .data[[scenario_baseline]] - 1),
      NA_real_
    )
  )

make_prod_agr_vs_land_share_bubble <- function(metric_col, x_label) {
  bubble_df <- afolu_country %>%
    group_by(region, year) %>%
    summarise(afolu_reduction = sum(value, na.rm = TRUE), .groups = "drop") %>%
    filter(region %in% country_codes, year == focus_year) %>%
    left_join(land_item_share_change_2100 %>% select(region, x = all_of(metric_col)), by = "region") %>%
    left_join(prod_agr_growth_2100, by = "region") %>%
    left_join(region_map, by = "region") %>%
    left_join(st_drop_geometry(Boundary), by = c("region" = "cty")) %>%
    mutate(country = coalesce(country_name, region), region5 = factor(region5, levels = names(region_palette)))

  bubble_df <- bubble_df %>%
    fixed_afolu_bubble_size() %>%
    filter(is.finite(x), is.finite(prod_agr_growth), !is.na(region5))

  size_levels <- levels(droplevels(bubble_df$size_bin))
  label_levels <- tail(size_levels, min(2, length(size_levels)))
  label_df <- bubble_df %>% filter(size_bin %in% label_levels)

  ggplot(bubble_df, aes(x = x, y = prod_agr_growth)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
    geom_point(aes(size = size_bin, fill = region5, color = region5, alpha = size_bin), shape = 21, stroke = 0.35) +
    geom_text(
      data = label_df,
      aes(label = country, color = region5),
      size = 3.2,
      fontface = "bold",
      nudge_y = 0.03 * max(abs(bubble_df$prod_agr_growth), na.rm = TRUE),
      show.legend = FALSE
    ) +
    scale_fill_manual(values = region_palette) +
    scale_color_manual(values = region_border_palette) +
    scale_size_manual(values = c(3, 4.8, 6.4, 8), drop = FALSE) +
    scale_alpha_manual(values = setNames(c(0.58, 0.66, 0.74, 0.82), size_levels), guide = "none", drop = FALSE) +
    labs(title = NULL, x = x_label, y = "PROD AGR increase from BaU (%)") +
    theme_main
}

# ---- Fig 3j: AGR production growth vs land-share change
fig3j <- wrap_plots(
  make_prod_agr_vs_land_share_bubble(
    "for_share_change",
    "Forest area change / total land (%)"
  ),
  make_prod_agr_vs_land_share_bubble(
    "ecp_share_change",
    "Energy crop area change / total land (%)"
  ),
  make_prod_agr_vs_land_share_bubble(
    "for_ecp_share_change",
    "Forest + energy crop area change / total land (%)"
  ),
  ncol = 3
) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "a") &
  theme(legend.position = "bottom")

save_plot("fig3j_prod_agr_vs_land_share_change_2100.png", fig3j, width = 18, height = 6)

top20_strength <- afolu_top20_2100 %>%
  left_join(
    GEER_AGMIP %>%
      filter(year == focus_year, scenario == scenario_baseline, variable == "POPT", item == "TOT", unit == "million", region %in% afolu_top20_2100$region) %>%
      transmute(region, pop_million = value),
    by = "region"
  ) %>%
  left_join(
    GEER_AGMIP %>%
      filter(year == focus_year, scenario == scenario_baseline, variable == "LAND", item == "TOT", unit == "1000 ha", region %in% afolu_top20_2100$region) %>%
      transmute(region, land_kha = value),
    by = "region"
  ) %>%
  left_join(
    afolu_baseline_country_2100 %>%
      group_by(region) %>%
      summarise(baseline_emissions = sum(value, na.rm = TRUE), .groups = "drop"),
    by = "region"
  ) %>%
  mutate(
    country = factor(country, levels = rev(afolu_top20_order)),
    per_capita = if_else(pop_million > 0, total_value / pop_million, NA_real_),
    per_land = if_else(land_kha > 0, total_value / land_kha * 1000, NA_real_),
    reduction_rate = if_else(baseline_emissions != 0, 100 * total_value / baseline_emissions, NA_real_)
  )

# ---- Fig 4: Top reducers, intensity indicators
fig4 <- wrap_plots(
  make_simple_rank_panel(top20_strength %>% transmute(country, value = per_capita), "", "tCO2eq/person/yr", "#4C78A8"),
  make_simple_rank_panel(top20_strength %>% transmute(country, value = per_land), "", "tCO2eq/ha/yr", "#59A14F"),
  make_simple_rank_panel(top20_strength %>% transmute(country, value = reduction_rate), "", "% of AFOLU BaU emissions", "#E15759"),
  ncol = 3
) + plot_annotation(tag_levels = "a")

save_plot("fig4_top_reduction_country_intensity_2100.png", fig4, width = 14, height = 6)

top_per_capita_components <- afolu_actual_country_decadal %>%
  filter(year == focus_year) %>%
  left_join(
    GEER_AGMIP %>%
      filter(year == focus_year, scenario == scenario_baseline, variable == "POPT", item == "TOT", unit == "million", region %in% country_codes) %>%
      transmute(region, denom = value),
    by = "region"
  ) %>%
  mutate(value = if_else(denom > 0, value / denom, NA_real_)) %>%
  group_by(region, country, component) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  group_by(region, country) %>%
  mutate(total_value = sum(value, na.rm = TRUE)) %>%
  ungroup()

top_per_capita_regions <- top_per_capita_components %>%
  distinct(region, country, total_value) %>%
  arrange(desc(total_value)) %>%
  slice_head(n = top_n)

p_fig4a_per_capita <- ggplot(
  top_per_capita_components %>%
    filter(region %in% top_per_capita_regions$region) %>%
    mutate(country = factor(country, levels = rev(top_per_capita_regions$country))),
  aes(x = value, y = country, fill = component)
) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
  geom_col() +
  geom_point(
    data = top_per_capita_regions %>% mutate(country = factor(country, levels = rev(top_per_capita_regions$country))),
    aes(x = total_value, y = country),
    inherit.aes = FALSE,
    shape = 21,
    size = 2.2,
    stroke = 0.35,
    fill = "black",
    color = "black"
  ) +
  scale_fill_manual(values = afolu_component_palette, limits = c("LULUCF", "Agriculture"), drop = FALSE) +
  labs(title = NULL, x = "tCO2eq/person/yr", y = NULL) +
  theme_main

top_per_land_components <- afolu_actual_country_decadal %>%
  filter(year == focus_year) %>%
  left_join(
    GEER_AGMIP %>%
      filter(year == focus_year, scenario == scenario_baseline, variable == "LAND", item == "TOT", unit == "1000 ha", region %in% country_codes) %>%
      transmute(region, denom = value),
    by = "region"
  ) %>%
  mutate(value = if_else(denom > 0, value / denom * 1000, NA_real_)) %>%
  group_by(region, country, component) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  group_by(region, country) %>%
  mutate(total_value = sum(value, na.rm = TRUE)) %>%
  ungroup()

top_per_land_regions <- top_per_land_components %>%
  distinct(region, country, total_value) %>%
  arrange(desc(total_value)) %>%
  slice_head(n = top_n)

p_fig4a_per_land <- ggplot(
  top_per_land_components %>%
    filter(region %in% top_per_land_regions$region) %>%
    mutate(country = factor(country, levels = rev(top_per_land_regions$country))),
  aes(x = value, y = country, fill = component)
) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
  geom_col() +
  geom_point(
    data = top_per_land_regions %>% mutate(country = factor(country, levels = rev(top_per_land_regions$country))),
    aes(x = total_value, y = country),
    inherit.aes = FALSE,
    shape = 21,
    size = 2.2,
    stroke = 0.35,
    fill = "black",
    color = "black"
  ) +
  scale_fill_manual(values = afolu_component_palette, limits = c("LULUCF", "Agriculture"), drop = FALSE) +
  labs(title = NULL, x = "tCO2eq/ha/yr", y = NULL) +
  theme_main

# ---- Fig 4a: Ranked per-capita and per-land reduction intensity
fig4a <- wrap_plots(p_fig4a_per_capita, p_fig4a_per_land, ncol = 2) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "a") &
  theme(legend.position = "bottom")

save_plot("fig4a_top_reduction_country_intensity_ranked_2100.png", fig4a, width = 12, height = 6)

land_efficiency_components <- fig2_land_country_2100 %>%
  filter(component %in% c(
    "Afforestation",
    "Forest protection",
    "Forest management",
    "Enhanced natural regeneration",
    "Agroforestry",
    "Biochar",
    "Soil carbon sequestration"
  )) %>%
  left_join(
    GEER_AGMIP %>%
      filter(
        year == focus_year,
        scenario == scenario_baseline,
        variable == "LAND",
        item == "TOT",
        unit == "1000 ha",
        region %in% country_codes
      ) %>%
      transmute(region, denom = value),
    by = "region"
  ) %>%
  left_join(st_drop_geometry(Boundary), by = c("region" = "cty")) %>%
  mutate(
    country = coalesce(country_name, region),
    value = if_else(denom > 0, value / denom * 1000, NA_real_)
  ) %>%
  group_by(region, country, component) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  group_by(region, country) %>%
  mutate(total_value = sum(value, na.rm = TRUE)) %>%
  ungroup()

top_land_efficiency_regions <- land_efficiency_components %>%
  distinct(region, country, total_value) %>%
  arrange(desc(total_value)) %>%
  slice_head(n = top_n)

p_fig4b <- ggplot(
  land_efficiency_components %>%
    filter(region %in% top_land_efficiency_regions$region) %>%
    mutate(
      country = factor(country, levels = rev(top_land_efficiency_regions$country)),
      component = factor(
        component,
        levels = c(
          "Afforestation",
          "Forest protection",
          "Forest management",
          "Enhanced natural regeneration",
          "Agroforestry",
          "Biochar",
          "Soil carbon sequestration"
        )
      )
    ),
  aes(x = value, y = country, fill = component)
) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
  geom_col() +
  geom_point(
    data = top_land_efficiency_regions %>% mutate(country = factor(country, levels = rev(top_land_efficiency_regions$country))),
    aes(x = total_value, y = country),
    inherit.aes = FALSE,
    shape = 21,
    size = 2.2,
    stroke = 0.35,
    fill = "black",
    color = "black"
  ) +
  scale_fill_manual(
    values = fig2_component_palette,
    breaks = c(
      "Afforestation",
      "Forest protection",
      "Forest management",
      "Enhanced natural regeneration",
      "Agroforestry",
      "Biochar",
      "Soil carbon sequestration"
    ),
    limits = c(
      "Afforestation",
      "Forest protection",
      "Forest management",
      "Enhanced natural regeneration",
      "Agroforestry",
      "Biochar",
      "Soil carbon sequestration"
    ),
    drop = FALSE
  ) +
  labs(title = NULL, x = "tCO2eq/ha/yr", y = NULL) +
  theme_main

# ---- Fig 4b: Ranked land-tech efficiency per unit land
fig4b <- p_fig4b +
  plot_annotation(tag_levels = "a") &
  theme(legend.position = "bottom")

save_plot("fig4b_top_land_efficiency_ranked_2100.png", fig4b, width = 10, height = 8)

make_world_final_wedge_total_plot <- function(plot_year) {
  top_regions <- agmip_ag_reduction_rank %>%
    filter(year == plot_year) %>%
    arrange(desc(ag_reduction)) %>%
    slice_head(n = 10) %>%
    pull(region)

  bottom_regions <- agmip_ag_reduction_rank %>%
    filter(year == plot_year, !region %in% top_regions) %>%
    arrange(ag_reduction) %>%
    slice_head(n = 10) %>%
    pull(region)

  selected_regions <- c(top_regions, bottom_regions)

  country_order <- agmip_ag_reduction_rank %>%
    filter(year == plot_year, region %in% selected_regions) %>%
    mutate(group_order = if_else(region %in% top_regions, 1L, 2L)) %>%
    arrange(group_order, desc(ag_reduction)) %>%
    pull(country)

  decomp_df <- world_final_wedge_country_co2eq %>%
    filter(year == plot_year, region %in% selected_regions) %>%
    transmute(
      country = factor(country, levels = rev(country_order)),
      `Demand side` = demand_mt,
      `Technical` = technical_mt,
      `Structural` = structural_mt
    ) %>%
    pivot_longer(cols = c(`Demand side`, `Technical`, `Structural`), names_to = "change", values_to = "value") %>%
    mutate(change = factor(change, levels = c("Demand side", "Technical", "Structural")))

  point_df <- world_final_wedge_country_co2eq %>%
    filter(year == plot_year, region %in% selected_regions) %>%
    transmute(country = factor(country, levels = rev(country_order)), total_change = total_change_mt)

  ggplot() +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
    geom_col(data = decomp_df, aes(x = value, y = country, fill = change)) +
    geom_point(data = point_df, aes(x = total_change, y = country), size = 2.4, shape = 21, stroke = 0.4, fill = "black", color = "black") +
    scale_fill_manual(values = c("Demand side" = "#9BC7F2", "Technical" = "#E3819C", "Structural" = "#A0EBD0")) +
    labs(title = NULL, x = "Emission change from BaU (MtCO2eq/yr)", y = NULL) +
    theme_main
}

make_world_final_wedge_r5_total_plot <- function(plot_year) {
  decomp_df <- world_final_wedge_r5_world_co2eq %>%
    filter(year == plot_year) %>%
    transmute(
      region_name,
      `Demand side` = demand_mt,
      `Technical` = technical_mt,
      `Structural` = structural_mt
    ) %>%
    pivot_longer(cols = c(`Demand side`, `Technical`, `Structural`), names_to = "change", values_to = "value") %>%
    mutate(change = factor(change, levels = c("Demand side", "Technical", "Structural")))

  point_df <- world_final_wedge_r5_world_co2eq %>%
    filter(year == plot_year) %>%
    transmute(region_name, total_change = total_change_mt)

  ggplot() +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
    geom_col(data = decomp_df, aes(x = value, y = region_name, fill = change)) +
    geom_point(data = point_df, aes(x = total_change, y = region_name), size = 2.4, shape = 21, stroke = 0.4, fill = "black", color = "black") +
    scale_fill_manual(values = c("Demand side" = "#9BC7F2", "Technical" = "#E3819C", "Structural" = "#A0EBD0")) +
    labs(title = NULL, x = "Emission change from BaU (MtCO2eq/yr)", y = NULL) +
    theme_main
}

# ---- Fig 5: Agricultural decomposition in 2100
fig5 <- wrap_plots(
  make_world_final_wedge_r5_total_plot(focus_year),
  make_world_final_wedge_total_plot(focus_year),
  ncol = 2,
  widths = c(0.8, 1)
) + plot_annotation(tag_levels = "a")

save_plot("fig5_agricultural_decomposition_2100.png", fig5, width = 11, height =5.5)

readr::write_csv(
  world_final_wedge_r5_world_co2eq %>%
    filter(year == focus_year) %>%
    mutate(
      demand_share_pct = if_else(abs(total_change_mt) > 0, 100 * demand_mt / total_change_mt, NA_real_),
      technical_share_pct = if_else(abs(total_change_mt) > 0, 100 * technical_mt / total_change_mt, NA_real_),
      structural_share_pct = if_else(abs(total_change_mt) > 0, 100 * structural_mt / total_change_mt, NA_real_)
    ) %>%
    transmute(
      region = as.character(region_name),
      year,
      demand_mt,
      technical_mt,
      structural_mt,
      total_change_mt,
      demand_share_pct,
      technical_share_pct,
      structural_share_pct
    ),
  file.path(output_dir, "fig5_world_r5_decomposition_summary_2100.csv")
)

fig5_top_regions_2100 <- agmip_ag_reduction_rank %>%
  filter(year == focus_year) %>%
  arrange(desc(ag_reduction)) %>%
  slice_head(n = 10) %>%
  pull(region)

fig5_bottom_regions_2100 <- agmip_ag_reduction_rank %>%
  filter(year == focus_year, !region %in% fig5_top_regions_2100) %>%
  arrange(ag_reduction) %>%
  slice_head(n = 10) %>%
  pull(region)

readr::write_csv(
  world_final_wedge_country_co2eq %>%
    filter(year == focus_year, region %in% c(fig5_top_regions_2100, fig5_bottom_regions_2100)) %>%
    left_join(
      agmip_ag_reduction_rank %>%
        filter(year == focus_year) %>%
        select(region, ag_reduction),
      by = "region"
    ) %>%
    transmute(
      region,
      country,
      year,
      ag_reduction,
      demand_mt,
      technical_mt,
      structural_mt,
      total_change_mt
    ),
  file.path(output_dir, "fig5_top_bottom_country_decomposition_summary_2100.csv")
)

# ---- Fig 5 reference version in 2050
fig5_2050 <- wrap_plots(
  make_world_final_wedge_r5_total_plot(2050L),
  make_world_final_wedge_total_plot(2050L),
  ncol = 2,
  widths = c(1.05, 1)
) + plot_annotation(tag_levels = "a")

save_plot("fig5_agricultural_decomposition_2050.png", fig5_2050, width = 12, height = 6)

readr::write_csv(
  world_final_wedge_r5_world_co2eq %>%
    filter(year == 2050L) %>%
    mutate(
      demand_share_pct = if_else(abs(total_change_mt) > 0, 100 * demand_mt / total_change_mt, NA_real_),
      technical_share_pct = if_else(abs(total_change_mt) > 0, 100 * technical_mt / total_change_mt, NA_real_),
      structural_share_pct = if_else(abs(total_change_mt) > 0, 100 * structural_mt / total_change_mt, NA_real_)
    ) %>%
    transmute(
      region = as.character(region_name),
      year,
      demand_mt,
      technical_mt,
      structural_mt,
      total_change_mt,
      demand_share_pct,
      technical_share_pct,
      structural_share_pct
    ),
  file.path(output_dir, "fig5_world_r5_decomposition_summary_2050.csv")
)

afolu_bubble_core <- afolu_country %>%
  group_by(region, year) %>%
  summarise(afolu_reduction = sum(value, na.rm = TRUE), .groups = "drop") %>%
  filter(region %in% country_codes, year %in% plot_years) %>%
  left_join(region_map, by = "region") %>%
  left_join(st_drop_geometry(Boundary), by = c("region" = "cty")) %>%
  mutate(country = coalesce(country_name, region), region5 = factor(region5, levels = names(region_palette)))

fixed_afolu_bubble_size <- function(df) {
  labels <- c(
    "<= 20 MtCO2eq/yr",
    "20-100 MtCO2eq/yr",
    "100-250 MtCO2eq/yr",
    ">= 250 MtCO2eq/yr"
  )

  df %>%
    mutate(
      size_bin = cut(
        afolu_reduction,
        breaks = c(-Inf, 20, 100, 250, Inf),
        include.lowest = TRUE,
        right = TRUE,
        labels = labels
      )
    )
}

nice_up <- function(x) {
  if (!is.finite(x) || x <= 0) return(0)
  base <- 10 ^ floor(log10(x))
  ceiling(x / base) * base
}

afolu_bubble_size <- function(df) {
  fixed_afolu_bubble_size(df)
}

make_bubble_x_df <- function(plot_year, metric_col) {
  switch(
    metric_col,
    land_tot = GEER_AGMIP %>%
      filter(year == plot_year, region %in% country_codes, scenario == scenario_baseline, variable == "LAND", item == "TOT", unit == "1000 ha") %>%
      transmute(region, year, x = value),
    gdppc = GEER_AGMIP %>%
      filter(
        year == plot_year,
        region %in% country_codes,
        scenario == scenario_baseline,
        (variable == "GDPT" & item == "TOT" & unit == "bn USD 2017 PPP") |
          (variable == "POPT" & item == "TOT" & unit == "million")
      ) %>%
      select(region, year, variable, value) %>%
      pivot_wider(names_from = variable, values_from = value) %>%
      transmute(region, year, x = if_else(POPT > 0, GDPT / POPT, NA_real_)),
    self_suff_bau = GEER_AGMIP %>%
      filter(year == plot_year, region %in% country_codes, scenario == scenario_baseline, variable %in% c("PROD", "CONS"), item == "AGR", unit == "million kcal") %>%
      select(region, year, variable, value) %>%
      pivot_wider(names_from = variable, values_from = value) %>%
      transmute(region, year, x = if_else(CONS > 0, 100 * PROD / CONS, NA_real_)),
    cali_agr = GEER_AGMIP %>%
      filter(year == plot_year, region %in% country_codes, scenario %in% c(scenario_baseline, scenario_landint), variable == "CALI", item == "AGR", unit == "kcal/cap/d") %>%
      select(region, year, scenario, value) %>%
      pivot_wider(names_from = scenario, values_from = value) %>%
      transmute(region, year, x = .data[[scenario_landint]] - .data[[scenario_baseline]])
  )
}

make_generic_bubble_plot <- function(plot_year, metric_col, x_label, y_df, y_col, y_label) {
  x_df <- make_bubble_x_df(plot_year, metric_col)

  bubble_df <- y_df %>%
    filter(year == plot_year, region %in% country_codes) %>%
    left_join(
      afolu_country %>% group_by(region, year) %>% summarise(afolu_reduction = sum(value, na.rm = TRUE), .groups = "drop"),
      by = c("region", "year")
    ) %>%
    left_join(region_map, by = "region") %>%
    left_join(st_drop_geometry(Boundary), by = c("region" = "cty")) %>%
    mutate(country = coalesce(country_name, region), region5 = factor(region5, levels = names(region_palette))) %>%
    left_join(x_df, by = c("region", "year")) %>%
    afolu_bubble_size() %>%
    filter(is.finite(.data[[y_col]]), is.finite(x), !is.na(region5))

  size_levels <- levels(droplevels(bubble_df$size_bin))
  label_levels <- tail(size_levels, min(2, length(size_levels)))
  label_df <- bubble_df %>% filter(size_bin %in% label_levels)

  ggplot(bubble_df, aes(x = x, y = .data[[y_col]])) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
    geom_point(aes(size = size_bin, fill = region5, color = region5, alpha = size_bin), shape = 21, stroke = 0.35) +
    geom_text(
      data = label_df,
      aes(label = country, color = region5),
      size = 3.2,
      fontface = "bold",
      nudge_y = 0.03 * max(abs(bubble_df[[y_col]]), na.rm = TRUE),
      show.legend = FALSE
    ) +
    scale_fill_manual(values = region_palette) +
    scale_color_manual(values = region_border_palette) +
    scale_size_manual(values = c(3, 4.8, 6.4, 8), drop = FALSE) +
    scale_alpha_manual(values = setNames(c(0.58, 0.66, 0.74, 0.82), size_levels), guide = "none", drop = FALSE) +
    labs(title = NULL, x = x_label, y = y_label) +
    guides(
      fill = guide_legend(order = 1, nrow = 1, byrow = TRUE, override.aes = list(size = 4, alpha = 1)),
      color = "none",
      size = guide_legend(order = 2, nrow = 1, byrow = TRUE)
    ) +
    theme_main +
    theme(legend.position = "bottom", legend.box = "vertical")
}

make_afolu_rate_bubble_plot <- function(plot_year, metric_col, x_label, title_text) {
  make_generic_bubble_plot(
    plot_year = plot_year,
    metric_col = metric_col,
    x_label = x_label,
    y_df = afolu_rate_country_fig6,
    y_col = "reduction_rate",
    y_label = "AFOLU reduction rate (% of BaU emissions)"
  )
}

# ---- Fig 6: AFOLU reduction-rate bubble plots
fig6 <- wrap_plots(
  make_afolu_rate_bubble_plot(focus_year, "land_tot", "Total land area in BaU (1000 ha)", ""),
  make_afolu_rate_bubble_plot(focus_year, "gdppc", "GDP per capita in BaU (1000 USD 2017 PPP)", ""),
  make_afolu_rate_bubble_plot(focus_year, "self_suff_bau", "Agricultural self-sufficiency in BaU (%)", ""),
  make_afolu_rate_bubble_plot(focus_year, "cali_agr", "Change in calorie intake under MITI relative to BaU (kcal/cap/d)", ""),
  ncol = 2
) + plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "a") &
  theme(legend.position = "bottom", legend.box = "vertical")

save_plot("fig6_afolu_rate_bubbles_2100.png", fig6, width = 14, height = 12)

# ---- Fig 6a: AFOLU reduction-amount bubble plots
fig6a <- wrap_plots(
  make_generic_bubble_plot(focus_year, "land_tot", "Total land area in BaU (1000 ha)", afolu_country %>% group_by(region, year) %>% summarise(actual_reduction = sum(value, na.rm = TRUE), .groups = "drop"), "actual_reduction", "AFOLU reduction (MtCO2eq/yr)"),
  make_generic_bubble_plot(focus_year, "gdppc", "GDP per capita in BaU (1000 USD 2017 PPP)", afolu_country %>% group_by(region, year) %>% summarise(actual_reduction = sum(value, na.rm = TRUE), .groups = "drop"), "actual_reduction", "AFOLU reduction (MtCO2eq/yr)"),
  make_generic_bubble_plot(focus_year, "self_suff_bau", "Agricultural self-sufficiency in BaU (%)", afolu_country %>% group_by(region, year) %>% summarise(actual_reduction = sum(value, na.rm = TRUE), .groups = "drop"), "actual_reduction", "AFOLU reduction (MtCO2eq/yr)"),
  make_generic_bubble_plot(focus_year, "cali_agr", "Change in calorie intake under MITI relative to BaU (kcal/cap/d)", afolu_country %>% group_by(region, year) %>% summarise(actual_reduction = sum(value, na.rm = TRUE), .groups = "drop"), "actual_reduction", "AFOLU reduction (MtCO2eq/yr)"),
  ncol = 2
) + plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "a") &
  theme(legend.position = "bottom", legend.box = "vertical")

save_plot("fig6a_afolu_amount_bubbles_2100.png", fig6a, width = 12, height = 10)

# ---- Fig 6b: LULUCF reduction-amount bubble plots
fig6b <- wrap_plots(
  make_generic_bubble_plot(focus_year, "land_tot", "Total land area in BaU (1000 ha)", afolu_land_country %>% rename(actual_reduction = value), "actual_reduction", "LULUCF reduction (MtCO2eq/yr)"),
  make_generic_bubble_plot(focus_year, "gdppc", "GDP per capita in BaU (1000 USD 2017 PPP)", afolu_land_country %>% rename(actual_reduction = value), "actual_reduction", "LULUCF reduction (MtCO2eq/yr)"),
  make_generic_bubble_plot(focus_year, "self_suff_bau", "Agricultural self-sufficiency in BaU (%)", afolu_land_country %>% rename(actual_reduction = value), "actual_reduction", "LULUCF reduction (MtCO2eq/yr)"),
  make_generic_bubble_plot(focus_year, "cali_agr", "Change in calorie intake under MITI relative to BaU (kcal/cap/d)", afolu_land_country %>% rename(actual_reduction = value), "actual_reduction", "LULUCF reduction (MtCO2eq/yr)"),
  ncol = 2
) + plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "a") &
  theme(legend.position = "bottom", legend.box = "vertical")

save_plot("fig6b_land_amount_bubbles_2100.png", fig6b, width = 12, height = 10)

# ---- Fig 6c: Agriculture reduction-amount bubble plots
fig6c <- wrap_plots(
  make_generic_bubble_plot(focus_year, "land_tot", "Total land area in BaU (1000 ha)", afolu_ag_country %>% rename(actual_reduction = value), "actual_reduction", "Agriculture reduction (MtCO2eq/yr)"),
  make_generic_bubble_plot(focus_year, "gdppc", "GDP per capita in BaU (1000 USD 2017 PPP)", afolu_ag_country %>% rename(actual_reduction = value), "actual_reduction", "Agriculture reduction (MtCO2eq/yr)"),
  make_generic_bubble_plot(focus_year, "self_suff_bau", "Agricultural self-sufficiency in BaU (%)", afolu_ag_country %>% rename(actual_reduction = value), "actual_reduction", "Agriculture reduction (MtCO2eq/yr)"),
  make_generic_bubble_plot(focus_year, "cali_agr", "Change in calorie intake under MITI relative to BaU (kcal/cap/d)", afolu_ag_country %>% rename(actual_reduction = value), "actual_reduction", "Agriculture reduction (MtCO2eq/yr)"),
  ncol = 2
) + plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "a") &
  theme(legend.position = "bottom", legend.box = "vertical")

save_plot("fig6c_ag_amount_bubbles_2100.png", fig6c, width = 12, height = 10)

# ---- Fig 6d: Agriculture reduction-rate bubble plots
fig6d <- wrap_plots(
  make_generic_bubble_plot(focus_year, "land_tot", "Total land area in BaU (1000 ha)", afolu_ag_rate_country_fig6, "reduction_rate", "Agriculture reduction rate (% of BaU emissions)"),
  make_generic_bubble_plot(focus_year, "gdppc", "GDP per capita in BaU (1000 USD 2017 PPP)", afolu_ag_rate_country_fig6, "reduction_rate", "Agriculture reduction rate (% of BaU emissions)"),
  make_generic_bubble_plot(focus_year, "self_suff_bau", "Agricultural self-sufficiency in BaU (%)", afolu_ag_rate_country_fig6, "reduction_rate", "Agriculture reduction rate (% of BaU emissions)"),
  make_generic_bubble_plot(focus_year, "cali_agr", "Change in calorie intake under MITI relative to BaU (kcal/cap/d)", afolu_ag_rate_country_fig6, "reduction_rate", "Agriculture reduction rate (% of BaU emissions)"),
  ncol = 2
) + plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "a") &
  theme(legend.position = "bottom", legend.box = "vertical")

save_plot("fig6d_ag_rate_bubbles_2100.png", fig6d, width = 12, height = 10)

afolu_amount_per_capita_country <- afolu_country %>%
  group_by(region, year) %>%
  summarise(actual_reduction = sum(value, na.rm = TRUE), .groups = "drop") %>%
  left_join(
    GEER_AGMIP %>%
      filter(
        scenario == scenario_baseline,
        variable == "POPT",
        item == "TOT",
        unit == "million",
        region %in% country_codes
      ) %>%
      transmute(region, year, pop_million = value),
    by = c("region", "year")
  ) %>%
  mutate(actual_reduction_pc = if_else(is.finite(pop_million) & pop_million > 0, actual_reduction / pop_million, NA_real_))

afolu_amount_per_land_country <- afolu_country %>%
  group_by(region, year) %>%
  summarise(actual_reduction = sum(value, na.rm = TRUE), .groups = "drop") %>%
  left_join(
    GEER_AGMIP %>%
      filter(
        scenario == scenario_baseline,
        variable == "LAND",
        item == "TOT",
        unit == "1000 ha",
        region %in% country_codes
      ) %>%
      transmute(region, year, land_1000ha = value),
    by = c("region", "year")
  ) %>%
  mutate(actual_reduction_pland = if_else(is.finite(land_1000ha) & land_1000ha > 0, actual_reduction / land_1000ha, NA_real_))

# ---- Fig 6e: AFOLU reduction per capita bubble plots
fig6e <- wrap_plots(
  make_generic_bubble_plot(focus_year, "land_tot", "Total land area in BaU (1000 ha)", afolu_amount_per_capita_country, "actual_reduction_pc", "AFOLU reduction per capita (tCO2eq/person/yr)"),
  make_generic_bubble_plot(focus_year, "gdppc", "GDP per capita in BaU (1000 USD 2017 PPP)", afolu_amount_per_capita_country, "actual_reduction_pc", "AFOLU reduction per capita (tCO2eq/person/yr)"),
  make_generic_bubble_plot(focus_year, "self_suff_bau", "Agricultural self-sufficiency in BaU (%)", afolu_amount_per_capita_country, "actual_reduction_pc", "AFOLU reduction per capita (tCO2eq/person/yr)"),
  make_generic_bubble_plot(focus_year, "cali_agr", "Change in calorie intake under MITI relative to BaU (kcal/cap/d)", afolu_amount_per_capita_country, "actual_reduction_pc", "AFOLU reduction per capita (tCO2eq/person/yr)"),
  ncol = 2
) + plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "a") &
  theme(legend.position = "bottom", legend.box = "vertical")

save_plot("fig6e_afolu_per_capita_bubbles_2100.png", fig6e, width = 12, height = 10)

# ---- Fig 6f: AFOLU reduction per land bubble plots
fig6f <- wrap_plots(
  make_generic_bubble_plot(focus_year, "land_tot", "Total land area in BaU (1000 ha)", afolu_amount_per_land_country, "actual_reduction_pland", "AFOLU reduction per land (ktCO2eq/ha/yr)"),
  make_generic_bubble_plot(focus_year, "gdppc", "GDP per capita in BaU (1000 USD 2017 PPP)", afolu_amount_per_land_country, "actual_reduction_pland", "AFOLU reduction per land (ktCO2eq/ha/yr)"),
  make_generic_bubble_plot(focus_year, "self_suff_bau", "Agricultural self-sufficiency in BaU (%)", afolu_amount_per_land_country, "actual_reduction_pland", "AFOLU reduction per land (ktCO2eq/ha/yr)"),
  make_generic_bubble_plot(focus_year, "cali_agr", "Change in calorie intake under MITI relative to BaU (kcal/cap/d)", afolu_amount_per_land_country, "actual_reduction_pland", "AFOLU reduction per land (ktCO2eq/ha/yr)"),
  ncol = 2
) + plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "a") &
  theme(legend.position = "bottom")

save_plot("fig6f_afolu_per_land_bubbles_2100.png", fig6f, width = 12, height = 10)

afolu_reduction_2100 <- afolu_country %>%
  filter(year == focus_year, region %in% country_codes) %>%
  group_by(region) %>%
  summarise(actual_reduction = sum(value, na.rm = TRUE), .groups = "drop")

xprc_agr_rate_2100 <- GEER_AGMIP %>%
  filter(
    year == focus_year,
    region %in% country_codes,
    scenario %in% c(scenario_baseline, scenario_landint),
    variable == "XPRC",
    item == "AGR",
    unit == "USD/t"
  ) %>%
  select(region, scenario, value) %>%
  pivot_wider(names_from = scenario, values_from = value) %>%
  transmute(
    region,
    metric = if_else(.data[[scenario_baseline]] > 0, 100 * (.data[[scenario_landint]] / .data[[scenario_baseline]] - 1), NA_real_)
  )

cali_decline_2100 <- GEER_AGMIP %>%
  filter(
    year == focus_year,
    region %in% country_codes,
    scenario %in% c(scenario_baseline, scenario_landint),
    variable == "CALI",
    item == "AGR",
    unit == "kcal/cap/d"
  ) %>%
  select(region, scenario, value) %>%
  pivot_wider(names_from = scenario, values_from = value) %>%
  transmute(region, metric = .data[[scenario_baseline]] - .data[[scenario_landint]])

nett_change_2100 <- GEER_AGMIP %>%
  filter(
    year == focus_year,
    region %in% country_codes,
    scenario %in% c(scenario_baseline, scenario_landint),
    variable == "NETT",
    item == "AGR",
    unit == "million kcal"
  ) %>%
  select(region, scenario, value) %>%
  pivot_wider(names_from = scenario, values_from = value) %>%
  transmute(region, metric = .data[[scenario_landint]] - .data[[scenario_baseline]])

make_top_bottom_metric_df <- function(metric_df) {
  ranked <- metric_df %>%
    left_join(afolu_reduction_2100, by = "region") %>%
    filter(is.finite(metric), is.finite(actual_reduction), actual_reduction > 0) %>%
    mutate(value = metric / actual_reduction) %>%
    left_join(st_drop_geometry(Boundary), by = c("region" = "cty")) %>%
    mutate(country = coalesce(country_name, region))

  top_df <- ranked %>% arrange(desc(value)) %>% slice_head(n = 10)
  bottom_df <- ranked %>% filter(!region %in% top_df$region) %>% arrange(value) %>% slice_head(n = 10)
  bind_rows(top_df, bottom_df) %>%
    mutate(country = factor(country, levels = rev(country)))
}

make_fig7_panel <- function(metric_df, x_label, fill_color) {
  plot_df <- make_top_bottom_metric_df(metric_df)
  ggplot(plot_df, aes(x = value, y = country)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
    geom_col(fill = fill_color) +
    labs(title = NULL, x = x_label, y = NULL) +
    theme_main
}

# ---- Fig 7: Impact intensity per unit AFOLU reduction
fig7 <- wrap_plots(
  make_fig7_panel(xprc_agr_rate_2100, "Price increase rate / reduction amount (% per MtCO2eq/yr)", "#4C78A8"),
  make_fig7_panel(cali_decline_2100, "Calorie decline / reduction amount (kcal/cap/d per MtCO2eq/yr)", "#B56576"),
  make_fig7_panel(nett_change_2100, "Net export change / reduction amount (million kcal per MtCO2eq/yr)", "#6BA292"),
  ncol = 3
) +
  plot_annotation(tag_levels = "a")

save_plot("fig7_cost_of_impacts_per_reduction_2100.png", fig7, width = 18, height = 8)

# ---- Appendix Fig A1: Typology of top AFOLU-reducing countries
figA1_typology_df <- world_final_wedge_country_co2eq %>%
  filter(year == focus_year, region %in% afolu_top20_2100$region) %>%
  left_join(
    afolu_top20_2100 %>%
      select(region, country, total_value),
    by = c("region", "country")
  ) %>%
  left_join(region_map, by = "region") %>%
  mutate(
    afolu_reduction = total_value,
    technical_share_pct = if_else(abs(total_change_mt) > 0, 100 * technical_mt / total_change_mt, NA_real_),
    structural_share_pct = if_else(abs(total_change_mt) > 0, 100 * structural_mt / total_change_mt, NA_real_),
    demand_share_pct = if_else(abs(total_change_mt) > 0, 100 * demand_mt / total_change_mt, NA_real_),
    region5 = factor(region5, levels = names(region_palette))
  ) %>%
  afolu_bubble_size() %>%
  filter(is.finite(technical_share_pct), is.finite(structural_share_pct))

figA1_label_df <- figA1_typology_df %>%
  arrange(desc(total_value)) %>%
  slice_head(n = 10)

figA1 <- ggplot(figA1_typology_df, aes(x = structural_share_pct, y = technical_share_pct)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.35) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.35) +
  geom_point(aes(fill = region5, color = region5, size = size_bin, alpha = size_bin), shape = 21, stroke = 0.35) +
  geom_text(
    data = figA1_label_df,
    aes(label = country, color = region5),
    size = 3.1,
    fontface = "bold",
    nudge_y = 2.5,
    show.legend = FALSE
  ) +
  scale_fill_manual(values = region_palette) +
  scale_color_manual(values = region_border_palette) +
  scale_size_manual(values = c(3, 4.8, 6.4, 8), drop = FALSE) +
  scale_alpha_manual(values = c(0.58, 0.66, 0.74, 0.82), guide = "none", drop = FALSE) +
  labs(
    x = "Structural contribution to total mitigation (%)",
    y = "Technical contribution to total mitigation (%)"
  ) +
  guides(
    fill = guide_legend(order = 1, nrow = 1, byrow = TRUE, override.aes = list(size = 4, alpha = 1)),
    color = "none",
    size = guide_legend(order = 2, nrow = 1, byrow = TRUE)
  ) +
  theme_main +
  theme(legend.position = "bottom", legend.box = "vertical")

save_plot("figA1_typology_top_reducers_2100.png", figA1, width = 9, height = 7)

readr::write_csv(
  figA1_typology_df %>%
    transmute(
      region,
      country,
      region5,
      afolu_reduction = total_value,
      technical_mt,
      structural_mt,
      demand_mt,
      total_change_mt,
      technical_share_pct,
      structural_share_pct,
      demand_share_pct
    ) %>%
    arrange(desc(afolu_reduction)),
  file.path(output_dir, "figA1_typology_summary_2100.csv")
)

# ---- Appendix Fig A2: Side-effect intensity for top AFOLU reducers
land_transition_change_2100 <- GEER_AGMIP %>%
  filter(
    year == focus_year,
    region %in% country_codes,
    scenario %in% c(scenario_baseline, scenario_landint),
    variable == "LAND",
    item %in% c("FOR", "ECP"),
    unit == "1000 ha"
  ) %>%
  group_by(region, scenario) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = scenario, values_from = value) %>%
  transmute(region, metric = .data[[scenario_landint]] - .data[[scenario_baseline]])

figA2_metric_df <- afolu_reduction_2100 %>%
  filter(region %in% afolu_top20_2100$region, actual_reduction > 0) %>%
  left_join(xprc_agr_rate_2100, by = "region") %>%
  rename(price_change_rate = metric) %>%
  left_join(cali_decline_2100, by = "region") %>%
  rename(calorie_decline = metric) %>%
  left_join(land_transition_change_2100, by = "region") %>%
  rename(land_transition_change = metric) %>%
  left_join(st_drop_geometry(Boundary), by = c("region" = "cty")) %>%
  mutate(
    country = coalesce(country_name, region),
    price_impact_intensity = price_change_rate / actual_reduction,
    calorie_impact_intensity = calorie_decline / actual_reduction,
    land_transition_intensity = land_transition_change / actual_reduction
  )

make_figA2_panel <- function(metric_col, x_label, fill_color) {
  top_df <- figA2_metric_df %>%
    filter(is.finite(.data[[metric_col]])) %>%
    arrange(desc(.data[[metric_col]])) %>%
    slice_head(n = 10)

  bottom_df <- figA2_metric_df %>%
    filter(is.finite(.data[[metric_col]]), !region %in% top_df$region) %>%
    arrange(.data[[metric_col]]) %>%
    slice_head(n = 10)

  plot_df <- bind_rows(top_df, bottom_df) %>%
    mutate(country = factor(country, levels = rev(country)))

  ggplot(plot_df, aes(x = .data[[metric_col]], y = country)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.35) +
    geom_col(fill = fill_color) +
    labs(x = x_label, y = NULL) +
    theme_main
}

figA2 <- wrap_plots(
  make_figA2_panel("price_impact_intensity", "Price increase rate per unit AFOLU mitigation (% per MtCO2eq/yr)", "#4C78A8"),
  make_figA2_panel("calorie_impact_intensity", "Calorie decline per unit AFOLU mitigation (kcal/cap/d per MtCO2eq/yr)", "#B56576"),
  make_figA2_panel("land_transition_intensity", "Forest + energy-crop area change per unit AFOLU mitigation (1000 ha per MtCO2eq/yr)", "#6BA292"),
  ncol = 3
) + plot_annotation(tag_levels = "a")

save_plot("figA2_side_effect_intensity_2100.png", figA2, width = 18, height = 8)

readr::write_csv(
  figA2_metric_df %>%
    select(region, country, actual_reduction, price_change_rate, calorie_decline, land_transition_change,
           price_impact_intensity, calorie_impact_intensity, land_transition_intensity) %>%
    arrange(desc(actual_reduction)),
  file.path(output_dir, "figA2_side_effect_intensity_summary_2100.csv")
)

# ---- Appendix Fig A3: Role of livestock among top AFOLU reducers
figA3_emis_share <- GEER_AGMIP %>%
  filter(
    year == focus_year,
    region %in% afolu_top20_2100$region,
    scenario == scenario_baseline,
    variable == "EMIS",
    item %in% c("ENT", "MMG", "AGR"),
    unit == "MtCO2e"
  ) %>%
  group_by(region, item) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = item, values_from = value, values_fill = 0) %>%
  transmute(region, livestock_emission_share = if_else(AGR > 0, 100 * (ENT + MMG) / AGR, NA_real_))

figA3_xprc_lsp <- GEER_AGMIP %>%
  filter(
    year == focus_year,
    region %in% afolu_top20_2100$region,
    scenario %in% c(scenario_baseline, scenario_landint),
    variable == "XPRC",
    item == "LSP",
    unit == "USD/t"
  ) %>%
  select(region, scenario, value) %>%
  pivot_wider(names_from = scenario, values_from = value) %>%
  transmute(region, livestock_price_change = if_else(.data[[scenario_baseline]] > 0, 100 * (.data[[scenario_landint]] / .data[[scenario_baseline]] - 1), NA_real_))

figA3_prod_lsp <- GEER_AGMIP %>%
  filter(
    year == focus_year,
    region %in% afolu_top20_2100$region,
    scenario %in% c(scenario_baseline, scenario_landint),
    variable == "PROD",
    item == "LSP",
    unit == "million kcal"
  ) %>%
  select(region, scenario, value) %>%
  pivot_wider(names_from = scenario, values_from = value) %>%
  transmute(region, livestock_prod_change = if_else(.data[[scenario_baseline]] > 0, 100 * (.data[[scenario_landint]] / .data[[scenario_baseline]] - 1), NA_real_))

figA3_df <- afolu_top20_2100 %>%
  select(region, country, total_value) %>%
  left_join(figA3_emis_share, by = "region") %>%
  left_join(figA3_xprc_lsp, by = "region") %>%
  left_join(figA3_prod_lsp, by = "region") %>%
  mutate(country = factor(country, levels = rev(afolu_top20_order)))

make_figA3_panel <- function(value_col, x_label, fill_color) {
  ggplot(figA3_df, aes(x = .data[[value_col]], y = country)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.35) +
    geom_col(fill = fill_color) +
    labs(x = x_label, y = NULL) +
    theme_main
}

figA3 <- wrap_plots(
  make_figA3_panel("livestock_emission_share", "Livestock share in agricultural emissions under BaU (%)", "#4C78A8"),
  make_figA3_panel("livestock_price_change", "Change in livestock producer price under MITI relative to BaU (%)", "#E17C05"),
  make_figA3_panel("livestock_prod_change", "Change in livestock production under MITI relative to BaU (%)", "#B55D60"),
  ncol = 3
) + plot_annotation(tag_levels = "a")

save_plot("figA3_livestock_role_2100.png", figA3, width = 18, height = 8)

readr::write_csv(
  figA3_df %>%
    transmute(
      region,
      country = as.character(country),
      afolu_reduction = total_value,
      livestock_emission_share,
      livestock_price_change,
      livestock_prod_change
    ),
  file.path(output_dir, "figA3_livestock_role_summary_2100.csv")
)

# ---- Appendix Fig A4: Structural contribution and side effects
make_figA4_panel <- function(y_col, y_label) {
  plot_df <- figA1_typology_df %>%
    left_join(
      figA2_metric_df %>% select(region, price_change_rate, calorie_decline),
      by = "region"
    ) %>%
    mutate(y_value = .data[[y_col]]) %>%
    filter(is.finite(structural_share_pct), is.finite(y_value))

  fit <- lm(y_value ~ structural_share_pct, data = plot_df)
  fit_sum <- summary(fit)
  p_val <- coef(fit_sum)[2, 4]
  p_label <- if (is.na(p_val)) {
    "p = NA"
  } else if (p_val < 0.001) {
    "p < 0.001"
  } else if (p_val < 0.01) {
    "p < 0.01"
  } else if (p_val < 0.05) {
    "p < 0.05"
  } else {
    "p >= 0.05"
  }

  label_df <- plot_df %>% arrange(desc(total_value)) %>% slice_head(n = 10)

  ggplot(plot_df, aes(x = structural_share_pct, y = y_value)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.35) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.35) +
    geom_smooth(method = "lm", se = TRUE, color = "black", fill = "grey75", linewidth = 0.7) +
    geom_point(aes(fill = region5, color = region5, size = size_bin, alpha = size_bin), shape = 21, stroke = 0.35) +
    geom_text(
      data = label_df,
      aes(label = country, color = region5),
      size = 3.1,
      fontface = "bold",
      nudge_y = 0.03 * max(abs(plot_df$y_value), na.rm = TRUE),
      show.legend = FALSE
    ) +
    annotate(
      "text",
      x = min(plot_df$structural_share_pct, na.rm = TRUE),
      y = max(plot_df$y_value, na.rm = TRUE),
      hjust = 0,
      vjust = 1,
      label = paste0("R² = ", sprintf("%.2f", fit_sum$r.squared), "\n", p_label),
      size = 3.2
    ) +
    scale_fill_manual(values = region_palette) +
    scale_color_manual(values = region_border_palette) +
    scale_size_manual(values = c(3, 4.8, 6.4, 8), drop = FALSE) +
    scale_alpha_manual(values = c(0.58, 0.66, 0.74, 0.82), guide = "none", drop = FALSE) +
    labs(
      x = "Structural contribution to total mitigation (%)",
      y = y_label
    ) +
    guides(
      fill = guide_legend(order = 1, nrow = 1, byrow = TRUE, override.aes = list(size = 4, alpha = 1)),
      color = "none",
      size = guide_legend(order = 2, nrow = 1, byrow = TRUE)
    ) +
    theme_main +
    theme(legend.position = "bottom", legend.box = "vertical")
}

figA4 <- wrap_plots(
  make_figA4_panel("calorie_decline", "Change in calorie intake from BaU (kcal/cap/d)"),
  make_figA4_panel("price_change_rate", "Change in producer price from BaU (%)"),
  ncol = 1
) + plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "a") &
  theme(legend.position = "bottom", legend.box = "vertical")

save_plot("figA4_structural_side_effects_2100.png", figA4, width = 8.5, height = 11)

readr::write_csv(
  figA1_typology_df %>%
    left_join(
      figA2_metric_df %>% select(region, price_change_rate, calorie_decline),
      by = "region"
    ) %>%
    transmute(
      region,
      country,
      region5,
      afolu_reduction = total_value,
      structural_share_pct,
      technical_share_pct,
      demand_share_pct,
      price_change_rate,
      calorie_decline
    ) %>%
    arrange(desc(afolu_reduction)),
  file.path(output_dir, "figA4_structure_side_effect_summary_2100.csv")
)
