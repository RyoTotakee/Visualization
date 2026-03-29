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
  candidate_dirs <- candidate_dirs[file.exists(file.path(candidate_dirs, "paperV1.R"))]
  if (length(candidate_dirs) == 0) {
    stop(
      "Could not determine script_dir. Open paperV1.R from Papers/GEER2026/prog/ ",
      "or set the working directory to Papers/GEER2026 before sourcing."
    )
  }
  normalizePath(candidate_dirs[[1]], winslash = "/", mustWork = TRUE)
}

base_dir <- normalizePath(file.path(script_dir, ".."), winslash = "/", mustWork = TRUE)
data_dir <- file.path(base_dir, "data", "0316")
define_dir <- file.path(base_dir, "define")
output_dir <- file.path(base_dir, "output", "paperV1")
country_set_file <- file.path(define_dir, "country.set")
region_map_file <- file.path(define_dir, "region.map")
riso_top100_file <- file.path(define_dir, "RISO_top100fao.txt")

scenario_baseline <- "SSP2_BaU_NoCC_No"
scenario_landint <- "SSP2_1100C_CACN_DAC_NoCC_LandInt_global1"
scenario_no <- "SSP2_1100C_CACN_DAC_NoCC_No"
plot_years <- c(2050, 2100)
decadal_years <- seq(2020, 2100, 10)

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

save_plot <- function(filename, plot_obj, width = 8, height = 5, dpi = 300) {
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

component_palette <- c("Land use" = "#A5D6A7", "Agriculture" = "#F6C667")

category_palette <- c(
  "Enteric" = "#FFB74D",
  "Manure" = "#F48FB1",
  "Rice" = "#FFF176",
  "Managed soils" = "#90CAF9",
  "Forest restore" = "#81C784",
  "Forest protect/manage" = "#64B5F6",
  "Biochar/soil carbon" = "#BCAAA4"
)

land_color <- c(
  "Cropland" = "#8CA231",
  "Managed and primary forest" = "#014D2F",
  "Grassland" = "#26C3EC",
  "Other natural land" = "#FF9855",
  "Non arable land" = "#AAA9A9",
  "Energy Crop" = "#ED87AD"
)

emission_palette <- c(
  "Enteric" = "#FFB74D",
  "Manure" = "#F48FB1",
  "Rice" = "#FFF176",
  "Managed soils" = "#90CAF9",
  "Land-use change" = "#FFCC80",
  "Wood harvest CO2" = "#BCAAA4",
  "Fire CO2" = "#EF9A9A",
  "Forest sink" = "#81C784"
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

tech_group <- function(variable) {
  case_when(
    grepl("Liv_Ent_Fer", variable) ~ "Enteric",
    grepl("Liv_Man_Man", variable) ~ "Manure",
    grepl("Agr_Ric", variable) ~ "Rice",
    grepl("Man_Soi", variable) ~ "Managed soils",
    variable %in% c("Emi_Red_CO2_AFO_Lan_Pla", "Emi_Red_CO2_AFO_Lan_Ref", "Emi_Red_CO2_AFO_Lan_Agf", "Emi_Red_CO2_AFO_Lan_Res", "Emi_CO2_AFO_Aff") ~ "Forest restore",
    variable %in% c("Emi_Red_CO2_AFO_Lan_FPr", "Emi_Red_CO2_AFO_Lan_Pff", "Emi_Red_CO2_AFO_Lan_Ril", "Emi_Red_CO2_AFO_Lan_Enr", "Emi_Red_CO2_AFO_Lan_Pro", "Emi_Red_CO2_AFO_Lan_Man") ~ "Forest protect/manage",
    variable %in% c("Emi_Red_CO2_AFO_Lan_Bch", "Emi_Red_CO2_AFO_Lan_Scs", "Emi_Red_CO2_AFO_Lan_Scg", "Emi_Red_CO2_AFO_Lan_Scc", "Emi_Red_CO2_AFO_Agr_Seq", "Car_Seq_Lan_Use_Soi_Car_Man", "Car_Seq_Lan_Use_Soi_Car_Man_Cro", "Car_Seq_Lan_Use_Soi_Car_Man_Gra") ~ "Biochar/soil carbon",
    TRUE ~ NA_character_
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

# data --------------------------------------------------------------------
country_codes <- if (file.exists(country_set_file)) {
  scan(country_set_file, what = character(), quiet = TRUE)
} else {
  stop("country.set is not available: ", country_set_file)
}

region_map <- if (file.exists(region_map_file)) {
  lines <- readLines(region_map_file, warn = FALSE)
  lines <- stringr::str_trim(lines)
  lines <- lines[lines != "" & !startsWith(lines, "*")]
  lines %>%
    str_split("\\s+") %>%
    purrr::map_dfr(function(x) {
      tibble(region = x[[1]], region5_code = x[[length(x)]])
    }) %>%
    filter(region %in% country_codes, region5_code %in% names(r5_labels), region5_code != "World") %>%
    mutate(region5 = recode(region5_code, !!!r5_labels))
} else {
  stop("region.map is not available: ", region_map_file)
}

Boundary <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>%
  st_make_valid() %>%
  transmute(cty = dplyr::if_else(adm0_a3 != "-99", adm0_a3, iso_a3), country_name = admin) %>%
  mutate(country_name = short_country_name(country_name)) %>%
  filter(cty %in% country_codes)

read_agmip <- function(variables, items, units, regions, years, scenarios) {
  req <- list(name = "AGMIP", form = "full", uels = list(scenarios, regions, variables, items, units, as.character(years)))
  x <- rgdx(file.path(data_dir, "AGMIP_all_scenario.gdx"), req, squeeze = FALSE)
  as.data.frame.table(x$val, responseName = "value", stringsAsFactors = FALSE) %>%
    setNames(c("scenario", "region", "variable", "item", "unit", "year", "value")) %>%
    mutate(year = as.integer(year), value = as.numeric(value))
}

regions_for_read <- unique(c("World", sort(unique(country_codes)), names(r5_labels)))

GEER_AGMIP <- read_agmip(
  variables = c("CALI", "XPRP", "PROD", "CONS", "NETT", "LAND", "POPT", "GDPT", "EMIS", "EmisRed", "EmisRedCO2", "EmisRedCH4", "EmisRedN2O"),
  items = c(
    "TOT", "AGR", "CRP", "LSP", "FOR", "GRS", "ECP", "ONV", "NLD",
    "WHT", "RIC", "CGR", "RUM", "NRM", "DRY",
    "tot", "ent", "mmg", "rcc", "sfr", "mas", "mgr", "crs", "ors", "bsv", "bcr"
  ),
  units = c("kcal/cap/d", "USD/t", "Index (2015 = 1)", "Index (2020 = 1)", "1000 t", "1000 ha", "million", "million kcal", "bn USD 2017 PPP", "MtCO2e"),
  regions = regions_for_read,
  years = decadal_years,
  scenarios = c(scenario_baseline, scenario_landint)
)

GEER_AGMIP_AGG <- rgdx.param(file.path(data_dir, "AGMIP_all_scenario.gdx"), "AGMIP_agg") %>%
  transmute(
    scenario = as.character(SCENARIO),
    region = as.character(region),
    variable = as.character(AgMIP_VAR),
    item = as.character(ITEM),
    unit = as.character(AgMIP_UNIT),
    year = as.integer(as.character(Y)),
    value = as.numeric(AGMIP_agg)
  ) %>%
  filter(
    scenario %in% c(scenario_baseline, scenario_landint),
    year %in% plot_years
  )

GEER_AFOLUB_ALL <- rgdx.param(file.path(data_dir, "IAMC_templatePlus.gdx"), "IAMC_templatePlus")
GEER_MERGED_WOU <- rgdx.param(file.path(data_dir, "Mergedcomparison.gdx"), "IAMCTempwoU")
GEER_WORLD_EFBASE <- rgdx.param(file.path(data_dir, "baseyear.gdx"), "EFbase")
GEER_WORLD_QDEM <- rgdx.param(file.path(data_dir, "all_scenario_World.gdx"), "QDEM_load")
GEER_WORLD_QSUP <- rgdx.param(file.path(data_dir, "all_scenario_World.gdx"), "QSUP_load")
GEER_WORLD_RR <- rgdx.param(file.path(data_dir, "all_scenario_World.gdx"), "RR_load")
GEER_WORLD_GHG <- rgdx.param(file.path(data_dir, "all_scenario_World.gdx"), "GHG_load")
GEER_WORLD_GHG_LU <- rgdx.param(file.path(data_dir, "all_scenario_World.gdx"), "GHG_LU_load")
GEER_WORLD_GHG_LU_AGG <- rgdx.param(file.path(data_dir, "all_scenario_global_17.gdx"), "GHG_LU_agg_load")

afolub_ref_vars <- c(
  "Emi_equ_GHG_AFO",
  "Emi_equ_GHG_AFO_Agr",
  "Emi_equ_GHG_AFO_Lan",
  "Emi_Red_equ_GHG_AFO",
  "Emi_Red_equ_GHG_AFO_Agr",
  "Emi_Red_equ_GHG_AFO_Lan",
  "Emi_Red_equ_CH4_AFO_Agr",
  "Emi_Red_equ_N2O_AFO_Agr",
  "Red_Cos_AFO_Agr",
  "Emi_equ_CH4_AFO_Agr",
  "Emi_equ_N2O_AFO_Agr",
  "Emi_equ_CH4_AFO_Agr_Liv_Ent_Fer",
  "Emi_equ_CH4_AFO_Agr_Liv_Man_Man",
  "Emi_equ_CH4_AFO_Agr_Ric",
  "Emi_equ_N2O_AFO_Agr_Liv_Man_Man",
  "Emi_equ_N2O_AFO_Agr_Man_Soi",
  "Emi_CO2_Lan_Use_Flo_Pos_Emi_Lan_Use_Cha",
  "Emi_CO2_Lan_Use_Flo_Pos_Emi_Woo_Har",
  "Emi_CO2_Lan_Use_Flo_Pos_Emi_Fir",
  "Emi_CO2_Lan_Use_Flo_RemForest",
  "Emi_CO2_AFO_Lan_For_Gra_Con",
  "Emi_Red_equ_CH4_AFO_Agr_Liv_Ent_Fer_Bre",
  "Emi_Red_equ_CH4_AFO_Agr_Liv_Ent_Fer_Fee",
  "Emi_Red_equ_CH4_AFO_Agr_Liv_Man_Man_Adc",
  "Emi_Red_equ_CH4_AFO_Agr_Liv_Man_Man_Ams",
  "Emi_Red_equ_CH4_AFO_Agr_Liv_Man_Man_Cfl",
  "Emi_Red_equ_CH4_AFO_Agr_Liv_Man_Man_Cad",
  "Emi_Red_equ_CH4_AFO_Agr_Liv_Man_Man_And",
  "Emi_Red_equ_CH4_AFO_Agr_Ric_Wat",
  "Emi_Red_equ_CH4_AFO_Agr_Ric_Res",
  "Emi_Red_equ_N2O_AFO_Agr_Man_Soi_Til",
  "Emi_Red_equ_N2O_AFO_Agr_Man_Soi_Fer",
  "Emi_Red_equ_N2O_AFO_Agr_Man_Soi_Res",
  "Emi_Red_equ_N2O_AFO_Agr_Man_Soi_Slo",
  "Emi_Red_equ_N2O_AFO_Agr_Man_Soi_Nin",
  "Emi_Red_CO2_AFO_Lan_Pla",
  "Emi_Red_CO2_AFO_Lan_Ref",
  "Emi_Red_CO2_AFO_Lan_FPr",
  "Emi_Red_CO2_AFO_Lan_Pff",
  "Emi_Red_CO2_AFO_Lan_Pro",
  "Emi_Red_CO2_AFO_Lan_Res",
  "Emi_Red_CO2_AFO_Lan_Man",
  "Emi_Red_CO2_AFO_Lan_Ril",
  "Emi_Red_CO2_AFO_Lan_Enr",
  "Emi_Red_CO2_AFO_Lan_Agf",
  "Emi_Red_CO2_AFO_Lan_Bch",
  "Emi_Red_CO2_AFO_Lan_Scs",
  "Emi_Red_CO2_AFO_Lan_Scg",
  "Emi_Red_CO2_AFO_Lan_Scc",
  "Car_Seq_Lan_Use_Soi_Car_Man",
  "Car_Seq_Lan_Use_Soi_Car_Man_Cro",
  "Car_Seq_Lan_Use_Soi_Car_Man_Gra",
  "Emi_CO2_AFO_Aff"
)

GEER_AFOLUB_REF <- GEER_AFOLUB_ALL %>%
  transmute(
    scenario = as.character(Scenario),
    region = as.character(REMF),
    variable = as.character(VEMF),
    year = as.integer(as.character(YEMF)),
    value = as.numeric(IAMC_templatePlus)
  ) %>%
  filter(
    scenario %in% c(scenario_no, scenario_landint, scenario_baseline, "FAO-FAOTier1", "FAO-UNFCCC", "EDGAR"),
    year %in% c(2005L, decadal_years),
    variable %in% afolub_ref_vars
  )

GEER_MERGED <- GEER_MERGED_WOU %>%
  transmute(
    scenario = as.character(i1),
    region = as.character(i2),
    variable = as.character(i3),
    year = as.integer(as.character(i4)),
    value = as.numeric(value)
  ) %>%
  filter(
    scenario %in% c(scenario_baseline, scenario_landint),
    region %in% regions_for_read,
    year %in% decadal_years
  )

riso_top100 <- if (file.exists(riso_top100_file)) {
  readr::read_csv(riso_top100_file, show_col_types = FALSE) %>%
    transmute(region = Region) %>%
    filter(region %in% country_codes)
} else {
  tibble(region = character())
}

message("GEER_AGMIP rows: ", nrow(GEER_AGMIP))
message("GEER_AFOLUB_REF rows: ", nrow(GEER_AFOLUB_REF))
message("GEER_WORLD_EFBASE rows: ", nrow(GEER_WORLD_EFBASE))
message("GEER_WORLD_QDEM rows: ", nrow(GEER_WORLD_QDEM))
message("GEER_WORLD_QSUP rows: ", nrow(GEER_WORLD_QSUP))
message("GEER_WORLD_RR rows: ", nrow(GEER_WORLD_RR))
message("GEER_WORLD_GHG rows: ", nrow(GEER_WORLD_GHG))
message("GEER_WORLD_GHG_LU rows: ", nrow(GEER_WORLD_GHG_LU))
message("GEER_WORLD_GHG_LU_AGG rows: ", nrow(GEER_WORLD_GHG_LU_AGG))

# common tables -----------------------------------------------------------
afolub_potential_country <- GEER_AFOLUB_REF %>%
  filter(
    scenario == scenario_no,
    region %in% c(country_codes, names(r5_labels)),
    year %in% plot_years,
    variable %in% c("Emi_Red_equ_GHG_AFO_Agr", "Emi_Red_equ_GHG_AFO_Lan")
  ) %>%
  mutate(component = recode(
    variable,
    Emi_Red_equ_GHG_AFO_Agr = "Agriculture",
    Emi_Red_equ_GHG_AFO_Lan = "Land use"
  )) %>%
  group_by(region, year, component) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  group_by(region, year) %>%
  mutate(total_potential = sum(value, na.rm = TRUE)) %>%
  ungroup()

afolub_emissions_wide <- GEER_AFOLUB_REF %>%
  filter(
    scenario %in% c(scenario_no, scenario_landint),
    region %in% c(country_codes, names(r5_labels)),
    year %in% plot_years,
    variable %in% c(
      "Emi_equ_GHG_AFO",
      "Emi_equ_GHG_AFO_Lan",
      "Emi_equ_CH4_AFO_Agr_Liv_Ent_Fer",
      "Emi_equ_CH4_AFO_Agr_Liv_Man_Man",
      "Emi_equ_CH4_AFO_Agr_Ric",
      "Emi_equ_N2O_AFO_Agr_Liv_Man_Man",
      "Emi_equ_N2O_AFO_Agr_Man_Soi",
      "Emi_CO2_Lan_Use_Flo_Pos_Emi_Lan_Use_Cha",
      "Emi_CO2_Lan_Use_Flo_Pos_Emi_Woo_Har",
      "Emi_CO2_Lan_Use_Flo_Pos_Emi_Fir",
      "Emi_CO2_Lan_Use_Flo_RemForest",
      "Emi_CO2_AFO_Lan_For_Gra_Con"
    )
  ) %>%
  group_by(region, year, variable, scenario) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = scenario, values_from = value)

afolub_real_reduction_country <- afolub_emissions_wide %>%
  { 
    .$value_no <- safe_pull(., scenario_no)
    .$value_landint <- safe_pull(., scenario_landint)
    .$actual_reduction <- .$value_no - .$value_landint
    .
  } %>%
  select(region, year, variable, value_no, value_landint, actual_reduction)

baseline_country_total <- afolub_real_reduction_country %>%
  filter(variable == "Emi_equ_GHG_AFO", region %in% country_codes) %>%
  transmute(region, year, baseline_emissions = value_no)

post_mitigation_country_total <- afolub_real_reduction_country %>%
  filter(variable == "Emi_equ_GHG_AFO", region %in% country_codes) %>%
  transmute(region, year, post_mitigation_emissions = value_landint)

land_baseline_country_total <- afolub_real_reduction_country %>%
  filter(variable == "Emi_equ_GHG_AFO_Lan", region %in% country_codes) %>%
  transmute(region, year, baseline_land_emissions = value_no)

land_post_country_total <- afolub_real_reduction_country %>%
  filter(variable == "Emi_equ_GHG_AFO_Lan", region %in% country_codes) %>%
  transmute(region, year, post_land_emissions = value_landint)

land_actual_change_country_total <- afolub_real_reduction_country %>%
  filter(variable == "Emi_equ_GHG_AFO_Lan", region %in% country_codes) %>%
  transmute(region, year, land_actual_reduction = actual_reduction)

land_baseline_r5_total <- afolub_real_reduction_country %>%
  filter(variable == "Emi_equ_GHG_AFO_Lan", region %in% names(r5_labels)) %>%
  transmute(region, year, baseline_land_emissions = value_no, region_name = recode(region, !!!r5_labels))

land_post_r5_total <- afolub_real_reduction_country %>%
  filter(variable == "Emi_equ_GHG_AFO_Lan", region %in% names(r5_labels)) %>%
  transmute(region, year, post_land_emissions = value_landint, region_name = recode(region, !!!r5_labels))

land_actual_change_r5_total <- afolub_real_reduction_country %>%
  filter(variable == "Emi_equ_GHG_AFO_Lan", region %in% names(r5_labels)) %>%
  transmute(region, year, land_actual_reduction = actual_reduction, region_name = recode(region, !!!r5_labels))

actual_reduction_total <- afolub_real_reduction_country %>%
  filter(region %in% country_codes) %>%
  mutate(component = case_when(
    variable %in% c(
      "Emi_equ_CH4_AFO_Agr_Liv_Ent_Fer",
      "Emi_equ_CH4_AFO_Agr_Liv_Man_Man",
      "Emi_equ_CH4_AFO_Agr_Ric",
      "Emi_equ_N2O_AFO_Agr_Liv_Man_Man",
      "Emi_equ_N2O_AFO_Agr_Man_Soi"
    ) ~ "Agriculture",
    variable %in% c(
      "Emi_CO2_Lan_Use_Flo_Pos_Emi_Lan_Use_Cha",
      "Emi_CO2_Lan_Use_Flo_Pos_Emi_Woo_Har",
      "Emi_CO2_Lan_Use_Flo_Pos_Emi_Fir",
      "Emi_CO2_Lan_Use_Flo_RemForest",
      "Emi_CO2_AFO_Lan_For_Gra_Con"
    ) ~ "Land use",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(component)) %>%
  group_by(region, year, component) %>%
  summarise(actual_reduction = sum(actual_reduction, na.rm = TRUE), .groups = "drop") %>%
  group_by(region, year) %>%
  mutate(total_actual_reduction = sum(actual_reduction, na.rm = TRUE)) %>%
  ungroup()

land_component_country <- afolub_real_reduction_country %>%
  filter(
    region %in% c(country_codes, names(r5_labels)),
    variable %in% c(
      "Emi_CO2_Lan_Use_Flo_Pos_Emi_Lan_Use_Cha",
      "Emi_CO2_AFO_Lan_For_Gra_Con",
      "Emi_CO2_Lan_Use_Flo_Pos_Emi_Woo_Har",
      "Emi_CO2_Lan_Use_Flo_Pos_Emi_Fir",
      "Emi_CO2_Lan_Use_Flo_RemForest"
    )
  ) %>%
  mutate(component = case_when(
    variable %in% c("Emi_CO2_Lan_Use_Flo_Pos_Emi_Lan_Use_Cha", "Emi_CO2_AFO_Lan_For_Gra_Con") ~ "Land-use change",
    variable %in% c("Emi_CO2_Lan_Use_Flo_Pos_Emi_Woo_Har", "Emi_CO2_Lan_Use_Flo_Pos_Emi_Fir", "Emi_CO2_Lan_Use_Flo_RemForest") ~ "Forest management / harvest / fire",
    TRUE ~ "Other land emissions"
  )) %>%
  group_by(region, year, component) %>%
  summarise(
    baseline_land_emissions = sum(value_no, na.rm = TRUE),
    post_land_emissions = sum(value_landint, na.rm = TRUE),
    land_actual_reduction = sum(actual_reduction, na.rm = TRUE),
    .groups = "drop"
  )

land_country_rank <- land_actual_change_country_total %>%
  left_join(st_drop_geometry(Boundary), by = c("region" = "cty")) %>%
  mutate(country = coalesce(country_name, region))

land_actual_change_rate_country <- land_actual_change_country_total %>%
  left_join(land_baseline_country_total, by = c("region", "year")) %>%
  left_join(st_drop_geometry(Boundary), by = c("region" = "cty")) %>%
  mutate(
    country = coalesce(country_name, region),
    land_reduction_rate = if_else(is.finite(baseline_land_emissions) & baseline_land_emissions > 0, 100 * land_actual_reduction / baseline_land_emissions, NA_real_)
  )

potential_tech_country <- GEER_AFOLUB_REF %>%
  filter(
    scenario == scenario_no,
    region %in% c(country_codes, names(r5_labels)),
    year %in% plot_years,
    grepl("^Emi_Red_equ_|^Emi_Red_CO2_AFO_Lan_", variable) |
      variable %in% c("Car_Seq_Lan_Use_Soi_Car_Man", "Car_Seq_Lan_Use_Soi_Car_Man_Cro", "Car_Seq_Lan_Use_Soi_Car_Man_Gra", "Emi_CO2_AFO_Aff")
  ) %>%
  mutate(tech = tech_group(variable)) %>%
  filter(!is.na(tech)) %>%
  group_by(region, year, tech) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop")

actual_reduction_tech_country <- GEER_AFOLUB_REF %>%
  filter(
    scenario %in% c(scenario_no, scenario_landint),
    region %in% c(country_codes, names(r5_labels)),
    year %in% plot_years,
    grepl("^Emi_Red_equ_|^Emi_Red_CO2_AFO_Lan_", variable) |
      variable %in% c("Car_Seq_Lan_Use_Soi_Car_Man", "Car_Seq_Lan_Use_Soi_Car_Man_Cro", "Car_Seq_Lan_Use_Soi_Car_Man_Gra", "Emi_CO2_AFO_Aff")
  ) %>%
  mutate(tech = tech_group(variable)) %>%
  filter(!is.na(tech)) %>%
  group_by(region, year, tech, scenario) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = scenario, values_from = value) %>%
  mutate(actual_reduction = safe_pull(., scenario_no) - safe_pull(., scenario_landint)) %>%
  select(region, year, tech, actual_reduction)

agmip_diff <- GEER_AGMIP %>%
  filter(region %in% country_codes, year %in% plot_years) %>%
  pivot_wider(names_from = scenario, values_from = value) %>%
  mutate(change = .data[[scenario_landint]] - .data[[scenario_baseline]])

agmip_core_change <- agmip_diff %>%
  filter(
    (variable == "CALI" & item == "TOT" & unit == "kcal/cap/d") |
      (variable == "XPRP" & item %in% c("AGR", "CRP", "LSP") & unit == "USD/t") |
      (variable == "PROD" & item %in% c("AGR", "CRP", "LSP") & unit == "1000 t") |
      (variable == "NETT" & item %in% c("AGR", "CRP", "LSP") & unit == "1000 t")
  ) %>%
  mutate(metric = case_when(
    variable == "CALI" & item == "TOT" ~ "Calorie",
    variable == "XPRP" & item == "AGR" ~ "AGR price",
    variable == "XPRP" & item == "CRP" ~ "CRP price",
    variable == "XPRP" & item == "LSP" ~ "LSP price",
    variable == "PROD" & item == "AGR" ~ "AGR production",
    variable == "PROD" & item == "CRP" ~ "CRP production",
    variable == "PROD" & item == "LSP" ~ "LSP production",
    variable == "NETT" & item == "AGR" ~ "AGR net trade",
    variable == "NETT" & item == "CRP" ~ "CRP net trade",
    variable == "NETT" & item == "LSP" ~ "LSP net trade"
  )) %>%
  select(region, year, metric, change)

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

agmip_sector_reduction <- GEER_AGMIP %>%
  filter(
    region %in% country_codes,
    year %in% plot_years,
    scenario == scenario_landint,
    variable == "EmisRed",
    item %in% c("CRP", "LSP"),
    unit == "MtCO2e"
  ) %>%
  transmute(region, year, sector = item, reduction = value)

agmip_price_index_change <- GEER_AGMIP %>%
  filter(
    region %in% country_codes,
    year %in% plot_years,
    scenario %in% c(scenario_baseline, scenario_landint),
    variable == "XPRP",
    item %in% c("CRP", "LSP"),
    unit %in% c("Index (2015 = 1)", "Index (2020 = 1)")
  ) %>%
  mutate(unit_priority = if_else(unit == "Index (2020 = 1)", 1L, 2L)) %>%
  arrange(region, year, item, scenario, unit_priority) %>%
  group_by(region, year, item, scenario) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  select(region, year, item, scenario, value) %>%
  pivot_wider(names_from = scenario, values_from = value) %>%
  mutate(price_index_change_pct = if_else(.data[[scenario_baseline]] > 0, 100 * (.data[[scenario_landint]] / .data[[scenario_baseline]] - 1), NA_real_)) %>%
  transmute(region, year, sector = item, price_index_change_pct)

agmip_emis_actual_reduction <- GEER_AGMIP %>%
  filter(
    region %in% c(country_codes, names(r5_labels)),
    year %in% plot_years,
    scenario %in% c(scenario_baseline, scenario_landint),
    variable == "EMIS",
    unit == "MtCO2e",
    item %in% c("tot", "ent", "mmg", "rcc", "sfr", "mas", "mgr", "crs", "ors", "bsv", "bcr")
  ) %>%
  group_by(region, year, item, scenario) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = scenario, values_from = value) %>%
  mutate(
    actual_reduction = .data[[scenario_baseline]] - .data[[scenario_landint]],
    source = recode(
      item,
      tot = "Total",
      ent = "Enteric fermentation",
      mmg = "Manure management",
      rcc = "Rice cultivation",
      sfr = "Synthetic fertilizers",
      mas = "Manure applied to soils",
      mgr = "Manure left on pasture",
      crs = "Crop residues",
      ors = "Organic soils",
      bsv = "Burning - Savanna",
      bcr = "Burning - Crop residues"
    )
  )

agmip_ag_reduction_rank <- agmip_emis_actual_reduction %>%
  filter(region %in% country_codes, item != "tot") %>%
  group_by(region, year) %>%
  summarise(ag_reduction = sum(actual_reduction, na.rm = TRUE), .groups = "drop") %>%
  left_join(st_drop_geometry(Boundary), by = c("region" = "cty")) %>%
  mutate(country = coalesce(country_name, region))

agmip_ag_reduction_rate <- agmip_emis_actual_reduction %>%
  filter(region %in% country_codes, item != "tot") %>%
  group_by(region, year) %>%
  summarise(
    ag_reduction = sum(actual_reduction, na.rm = TRUE),
    bau_ag_emissions = sum(.data[[scenario_baseline]], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    ag_reduction_rate = if_else(is.finite(bau_ag_emissions) & bau_ag_emissions > 0, 100 * ag_reduction / bau_ag_emissions, NA_real_)
  ) %>%
  left_join(st_drop_geometry(Boundary), by = c("region" = "cty")) %>%
  mutate(country = coalesce(country_name, region))

agmip_ag_reduction_rate_r5 <- GEER_AGMIP_AGG %>%
  filter(
    region %in% names(r5_labels),
    variable == "EMIS",
    unit == "MtCO2e",
    item == "AGR"
  ) %>%
  group_by(region, year) %>%
  summarise(
    bau_ag_emissions = sum(value[scenario == scenario_baseline], na.rm = TRUE),
    landint_ag_emissions = sum(value[scenario == scenario_landint], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    ag_reduction = bau_ag_emissions - landint_ag_emissions,
    ag_reduction_rate = if_else(is.finite(bau_ag_emissions) & bau_ag_emissions > 0, 100 * ag_reduction / bau_ag_emissions, NA_real_),
    region_name = recode(region, !!!r5_labels)
  )

agmip_land_conversion <- agmip_land_long %>%
  group_by(region, year) %>%
  summarise(
    land_conversion_abs = sum(pmax(land_change, 0), na.rm = TRUE),
    land_conversion_share = sum(pmax(land_share, 0), na.rm = TRUE),
    .groups = "drop"
  )

agmip_population_area <- GEER_AGMIP %>%
  filter(
    region %in% country_codes,
    year %in% plot_years,
    scenario == scenario_baseline,
    (variable == "POPT" & item == "TOT" & unit == "million") |
      (variable == "LAND" & item == "TOT" & unit == "1000 ha")
  ) %>%
  mutate(metric = if_else(variable == "POPT", "population_million", "land_area_kha")) %>%
  select(region, year, metric, value) %>%
  pivot_wider(names_from = metric, values_from = value)

top_potential_country <- afolub_potential_country %>%
  filter(region %in% country_codes) %>%
  distinct(region, year, total_potential) %>%
  left_join(st_drop_geometry(Boundary), by = c("region" = "cty")) %>%
  mutate(country = coalesce(country_name, region))

# 3.1 ---------------------------------------------------------------------
baseline_map_df <- baseline_country_total %>% filter(year %in% plot_years)
save_plot(
  "fig3_1a_baseline_map_2050_2100.png",
  wrap_plots(
    make_country_map(filter(baseline_map_df, year == 2050), "baseline_emissions", "Baseline AFOLU emissions (2050)", "MtCO2eq/yr"),
    make_country_map(filter(baseline_map_df, year == 2100), "baseline_emissions", "Baseline AFOLU emissions (2100)", "MtCO2eq/yr"),
    ncol = 2
  ),
  width = 12,
  height = 5
)

make_baseline_top_plot <- function(plot_year) {
  top_df <- baseline_country_total %>%
    filter(year == plot_year) %>%
    left_join(st_drop_geometry(Boundary), by = c("region" = "cty")) %>%
    mutate(country = coalesce(country_name, region)) %>%
    arrange(desc(baseline_emissions)) %>%
    slice_head(n = 20) %>%
    mutate(country = factor(country, levels = rev(country)))

  ggplot(top_df, aes(x = baseline_emissions, y = country)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
    geom_col(fill = "#6C8EBF") +
    labs(title = paste0("Top baseline emitters (", plot_year, ")"), x = "MtCO2eq/yr", y = NULL) +
    theme_main
}

save_plot(
  "fig3_1b_baseline_top_emitters_2050_2100.png",
  wrap_plots(make_baseline_top_plot(2050), make_baseline_top_plot(2100), ncol = 2),
  width = 12,
  height = 8
)

make_baseline_composition_plot <- function(plot_year) {
  top_regions <- baseline_country_total %>%
    filter(year == plot_year) %>%
    arrange(desc(baseline_emissions)) %>%
    slice_head(n = 15) %>%
    pull(region)

  country_order <- baseline_country_total %>%
    filter(year == plot_year) %>%
    arrange(desc(baseline_emissions)) %>%
    slice_head(n = 15) %>%
    left_join(st_drop_geometry(Boundary), by = c("region" = "cty")) %>%
    mutate(country = coalesce(country_name, region)) %>%
    pull(country)

  df <- afolub_real_reduction_country %>%
    filter(
      year == plot_year,
      region %in% top_regions,
      variable %in% c(
        "Emi_equ_CH4_AFO_Agr_Liv_Ent_Fer",
        "Emi_equ_CH4_AFO_Agr_Liv_Man_Man",
        "Emi_equ_CH4_AFO_Agr_Ric",
        "Emi_equ_N2O_AFO_Agr_Man_Soi",
        "Emi_CO2_Lan_Use_Flo_Pos_Emi_Lan_Use_Cha",
        "Emi_CO2_Lan_Use_Flo_Pos_Emi_Woo_Har",
        "Emi_CO2_Lan_Use_Flo_Pos_Emi_Fir",
        "Emi_CO2_Lan_Use_Flo_RemForest",
        "Emi_CO2_AFO_Lan_For_Gra_Con"
      )
    ) %>%
    transmute(
      region,
      value = value_no,
      component = case_when(
        variable == "Emi_equ_CH4_AFO_Agr_Liv_Ent_Fer" ~ "Enteric",
        variable %in% c("Emi_equ_CH4_AFO_Agr_Liv_Man_Man", "Emi_equ_N2O_AFO_Agr_Liv_Man_Man") ~ "Manure",
        variable == "Emi_equ_CH4_AFO_Agr_Ric" ~ "Rice",
        variable == "Emi_equ_N2O_AFO_Agr_Man_Soi" ~ "Managed soils",
        variable %in% c("Emi_CO2_Lan_Use_Flo_Pos_Emi_Lan_Use_Cha", "Emi_CO2_AFO_Lan_For_Gra_Con") ~ "Land-use change",
        variable == "Emi_CO2_Lan_Use_Flo_Pos_Emi_Woo_Har" ~ "Wood harvest CO2",
        variable == "Emi_CO2_Lan_Use_Flo_Pos_Emi_Fir" ~ "Fire CO2",
        variable == "Emi_CO2_Lan_Use_Flo_RemForest" ~ "Forest sink"
      )
    ) %>%
    group_by(region, component) %>%
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    left_join(st_drop_geometry(Boundary), by = c("region" = "cty")) %>%
    mutate(
      country = coalesce(country_name, region),
      country = factor(country, levels = rev(country_order))
    )

  ggplot(df, aes(x = value, y = country, fill = component)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
    geom_col() +
    scale_fill_manual(values = emission_palette) +
    labs(title = paste0("Baseline emissions composition (", plot_year, ")"), x = "MtCO2eq/yr", y = NULL) +
    theme_main
}

save_plot(
  "fig3_1c_baseline_composition_2050_2100.png",
  wrap_plots(make_baseline_composition_plot(2050), make_baseline_composition_plot(2100), ncol = 2),
  width = 12,
  height = 8
)

post_mitigation_map_df <- post_mitigation_country_total %>% filter(year %in% plot_years)
save_plot(
  "fig3_1d_post_mitigation_map_2050_2100.png",
  wrap_plots(
    make_country_map(filter(post_mitigation_map_df, year == 2050), "post_mitigation_emissions", "Post-mitigation AFOLU emissions (2050)", "MtCO2eq/yr"),
    make_country_map(filter(post_mitigation_map_df, year == 2100), "post_mitigation_emissions", "Post-mitigation AFOLU emissions (2100)", "MtCO2eq/yr"),
    ncol = 2
  ),
  width = 12,
  height = 5
)

make_post_mitigation_top_plot <- function(plot_year) {
  top_df <- post_mitigation_country_total %>%
    filter(year == plot_year) %>%
    left_join(st_drop_geometry(Boundary), by = c("region" = "cty")) %>%
    mutate(country = coalesce(country_name, region)) %>%
    arrange(desc(post_mitigation_emissions)) %>%
    slice_head(n = 20) %>%
    mutate(country = factor(country, levels = rev(country)))

  ggplot(top_df, aes(x = post_mitigation_emissions, y = country)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
    geom_col(fill = "#5B8E7D") +
    labs(title = paste0("Top post-mitigation emitters (", plot_year, ")"), x = "MtCO2eq/yr", y = NULL) +
    theme_main
}

save_plot(
  "fig3_1e_post_mitigation_top_emitters_2050_2100.png",
  wrap_plots(make_post_mitigation_top_plot(2050), make_post_mitigation_top_plot(2100), ncol = 2),
  width = 12,
  height = 8
)

make_post_mitigation_composition_plot <- function(plot_year) {
  top_regions <- post_mitigation_country_total %>%
    filter(year == plot_year) %>%
    arrange(desc(post_mitigation_emissions)) %>%
    slice_head(n = 15) %>%
    pull(region)

  country_order <- post_mitigation_country_total %>%
    filter(year == plot_year) %>%
    arrange(desc(post_mitigation_emissions)) %>%
    slice_head(n = 15) %>%
    left_join(st_drop_geometry(Boundary), by = c("region" = "cty")) %>%
    mutate(country = coalesce(country_name, region)) %>%
    pull(country)

  df <- afolub_real_reduction_country %>%
    filter(
      year == plot_year,
      region %in% top_regions,
      variable %in% c(
        "Emi_equ_CH4_AFO_Agr_Liv_Ent_Fer",
        "Emi_equ_CH4_AFO_Agr_Liv_Man_Man",
        "Emi_equ_CH4_AFO_Agr_Ric",
        "Emi_equ_N2O_AFO_Agr_Man_Soi",
        "Emi_CO2_Lan_Use_Flo_Pos_Emi_Lan_Use_Cha",
        "Emi_CO2_Lan_Use_Flo_Pos_Emi_Woo_Har",
        "Emi_CO2_Lan_Use_Flo_Pos_Emi_Fir",
        "Emi_CO2_Lan_Use_Flo_RemForest",
        "Emi_CO2_AFO_Lan_For_Gra_Con"
      )
    ) %>%
    transmute(
      region,
      value = value_landint,
      component = case_when(
        variable == "Emi_equ_CH4_AFO_Agr_Liv_Ent_Fer" ~ "Enteric",
        variable %in% c("Emi_equ_CH4_AFO_Agr_Liv_Man_Man", "Emi_equ_N2O_AFO_Agr_Liv_Man_Man") ~ "Manure",
        variable == "Emi_equ_CH4_AFO_Agr_Ric" ~ "Rice",
        variable == "Emi_equ_N2O_AFO_Agr_Man_Soi" ~ "Managed soils",
        variable %in% c("Emi_CO2_Lan_Use_Flo_Pos_Emi_Lan_Use_Cha", "Emi_CO2_AFO_Lan_For_Gra_Con") ~ "Land-use change",
        variable == "Emi_CO2_Lan_Use_Flo_Pos_Emi_Woo_Har" ~ "Wood harvest CO2",
        variable == "Emi_CO2_Lan_Use_Flo_Pos_Emi_Fir" ~ "Fire CO2",
        variable == "Emi_CO2_Lan_Use_Flo_RemForest" ~ "Forest sink"
      )
    ) %>%
    group_by(region, component) %>%
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    left_join(st_drop_geometry(Boundary), by = c("region" = "cty")) %>%
    mutate(
      country = coalesce(country_name, region),
      country = factor(country, levels = rev(country_order))
    )

  ggplot(df, aes(x = value, y = country, fill = component)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
    geom_col() +
    scale_fill_manual(values = emission_palette) +
    labs(title = paste0("Post-mitigation emissions composition (", plot_year, ")"), x = "MtCO2eq/yr", y = NULL) +
    theme_main
}

save_plot(
  "fig3_1f_post_mitigation_composition_2050_2100.png",
  wrap_plots(make_post_mitigation_composition_plot(2050), make_post_mitigation_composition_plot(2100), ncol = 2),
  width = 12,
  height = 8
)

# 3.2 ---------------------------------------------------------------------
potential_map_df <- top_potential_country %>% filter(year %in% plot_years) %>% rename(total_value = total_potential)
save_plot(
  "fig3_2a_potential_map_2050_2100.png",
  wrap_plots(
    make_country_map(filter(potential_map_df, year == 2050), "total_value", "Mitigation potential (2050)", "MtCO2eq/yr"),
    make_country_map(filter(potential_map_df, year == 2100), "total_value", "Mitigation potential (2100)", "MtCO2eq/yr"),
    ncol = 2
  ),
  width = 12,
  height = 5
)

make_top_potential_plot <- function(plot_year) {
  df <- afolub_potential_country %>%
    filter(region %in% country_codes, year == plot_year) %>%
    left_join(st_drop_geometry(Boundary), by = c("region" = "cty")) %>%
    mutate(country = coalesce(country_name, region)) %>%
    mutate(component = factor(component, levels = c("Land use", "Agriculture"))) %>%
    group_by(region, country) %>%
    mutate(total_potential = sum(value, na.rm = TRUE)) %>%
    ungroup() %>%
    arrange(desc(total_potential)) %>%
    slice_head(n = 20) %>%
    mutate(country = factor(country, levels = rev(unique(country))))

  ggplot(df, aes(x = value, y = country, fill = component)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
    geom_col() +
    scale_fill_manual(values = component_palette) +
    labs(title = paste0("Top mitigation potential countries (", plot_year, ")"), x = "MtCO2eq/yr", y = NULL) +
    theme_main
}

save_plot(
  "fig3_2b_top_potential_2050_2100.png",
  wrap_plots(make_top_potential_plot(2050), make_top_potential_plot(2100), ncol = 2),
  width = 12,
  height = 8
)

concentration_curve <- top_potential_country %>%
  filter(year %in% plot_years) %>%
  group_by(year) %>%
  arrange(desc(total_potential), .by_group = TRUE) %>%
  mutate(
    share = total_potential / sum(total_potential, na.rm = TRUE),
    cum_share = cumsum(share),
    cum_country = row_number() / n()
  ) %>%
  ungroup()

p_concentration <- ggplot(concentration_curve, aes(x = cum_country, y = cum_share, color = factor(year))) +
  geom_line(linewidth = 1.1) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey60") +
  scale_x_continuous(labels = percent_format()) +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "Concentration of mitigation potential", x = "Cumulative share of countries", y = "Cumulative share of potential") +
  theme_main

save_plot("fig3_2c_concentration_curve.png", p_concentration, width = 8, height = 5)

make_potential_vs_emissions <- function(plot_year) {
  df <- top_potential_country %>%
    filter(year == plot_year) %>%
    left_join(baseline_country_total %>% filter(year == plot_year), by = c("region", "year")) %>%
    left_join(region_map, by = "region") %>%
    mutate(region5 = factor(region5, levels = names(region_palette))) %>%
    filter(is.finite(baseline_emissions), is.finite(total_potential))

  label_df <- df %>% slice_max(order_by = total_potential, n = 12, with_ties = FALSE)

  ggplot(df, aes(x = baseline_emissions, y = total_potential)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
    geom_point(aes(color = region5), size = 2.4, alpha = 0.85) +
    geom_text(data = label_df, aes(label = country, color = region5), size = 3, nudge_y = 0.02 * max(df$total_potential, na.rm = TRUE), show.legend = FALSE) +
    scale_color_manual(values = region_border_palette) +
    labs(title = paste0("Potential vs baseline emissions (", plot_year, ")"), x = "Baseline AFOLU emissions", y = "Mitigation potential") +
    theme_main
}

save_plot(
  "fig3_2d_potential_vs_emissions_2050_2100.png",
  wrap_plots(make_potential_vs_emissions(2050), make_potential_vs_emissions(2100), ncol = 2),
  width = 12,
  height = 6
)

intensity_df <- top_potential_country %>%
  left_join(agmip_population_area, by = c("region", "year")) %>%
  mutate(
    per_capita = if_else(population_million > 0, total_potential / population_million, NA_real_),
    per_area = if_else(land_area_kha > 0, total_potential / land_area_kha, NA_real_)
  )

make_intensity_plot <- function(metric_col, title_text, x_label, plot_year) {
  df <- intensity_df %>%
    filter(year == plot_year) %>%
    arrange(desc(total_potential)) %>%
    slice_head(n = 20) %>%
    mutate(country = factor(country, levels = rev(country)))

  ggplot(df, aes(x = .data[[metric_col]], y = country)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
    geom_col(fill = if (metric_col == "per_capita") "#4C78A8" else "#72B7B2") +
    labs(title = paste0(title_text, " (", plot_year, ")"), x = x_label, y = NULL) +
    theme_main
}

save_plot(
  "fig3_2e_potential_intensity_2100.png",
  wrap_plots(
    make_intensity_plot("per_capita", "Potential per capita", "MtCO2eq / million people", 2100),
    make_intensity_plot("per_area", "Potential per land area", "MtCO2eq / 1000 ha", 2100),
    ncol = 2
  ),
  width = 12,
  height = 8
)

# 3.3 ---------------------------------------------------------------------
make_top_portfolio_plot <- function(plot_year) {
  top_regions <- top_potential_country %>%
    filter(year == plot_year) %>%
    arrange(desc(total_potential)) %>%
    slice_head(n = 15) %>%
    pull(region)

  country_order <- top_potential_country %>%
    filter(year == plot_year) %>%
    arrange(desc(total_potential)) %>%
    slice_head(n = 15) %>%
    pull(country)

  df <- potential_tech_country %>%
    filter(year == plot_year, region %in% top_regions) %>%
    left_join(top_potential_country %>% filter(year == plot_year) %>% select(region, country), by = "region") %>%
    mutate(country = factor(country, levels = rev(country_order)))

  ggplot(df, aes(x = value, y = country, fill = tech)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
    geom_col() +
    scale_fill_manual(values = category_palette) +
    labs(title = paste0("Potential portfolio of top countries (", plot_year, ")"), x = "MtCO2eq/yr", y = NULL) +
    theme_main
}

save_plot(
  "fig3_3a_top_portfolio_2050_2100.png",
  wrap_plots(make_top_portfolio_plot(2050), make_top_portfolio_plot(2100), ncol = 2),
  width = 12,
  height = 8
)

top_heatmap_df <- potential_tech_country %>%
  filter(year == 2100, region %in% (top_potential_country %>% filter(year == 2100) %>% arrange(desc(total_potential)) %>% slice_head(n = 15) %>% pull(region))) %>%
  left_join(top_potential_country %>% filter(year == 2100) %>% select(region, country, total_potential), by = "region") %>%
  mutate(
    country = factor(country, levels = rev((top_potential_country %>% filter(year == 2100) %>% arrange(desc(total_potential)) %>% slice_head(n = 15) %>% pull(country)))),
    tech = factor(tech, levels = names(category_palette))
  )

p_heatmap <- ggplot(top_heatmap_df, aes(x = tech, y = country, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "#C3327A") +
  labs(title = "Technology heatmap in top-potential countries (2100)", x = NULL, y = NULL) +
  theme_main +
  theme(axis.text.x = element_text(angle = 35, hjust = 1))

save_plot("fig3_3b_top_portfolio_heatmap_2100.png", p_heatmap, width = 10, height = 7)

r5_portfolio <- potential_tech_country %>%
  filter(year == 2100, region %in% names(r5_labels), region != "World") %>%
  mutate(region_name = recode(region, !!!r5_labels)) %>%
  group_by(region_name, tech) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  group_by(region_name) %>%
  mutate(total = sum(value, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(region_name = fct_reorder(region_name, total))

p_r5_portfolio <- ggplot(r5_portfolio, aes(x = value, y = region_name, fill = tech)) +
  geom_col() +
  scale_fill_manual(values = category_palette) +
  labs(title = "R5 mitigation portfolio (2100)", x = "MtCO2eq/yr", y = NULL) +
  theme_main

save_plot("fig3_3c_r5_portfolio_2100.png", p_r5_portfolio, width = 9, height = 5)

actual_reduction_country <- actual_reduction_total %>%
  filter(region %in% country_codes) %>%
  distinct(region, year, total_actual_reduction) %>%
  left_join(st_drop_geometry(Boundary), by = c("region" = "cty")) %>%
  mutate(country = coalesce(country_name, region))

actual_reduction_map_df <- actual_reduction_country %>% filter(year %in% plot_years)
save_plot(
  "fig3_3d_actual_reduction_map_2050_2100.png",
  wrap_plots(
    make_country_map(filter(actual_reduction_map_df, year == 2050), "total_actual_reduction", "Actual reduction (2050)", "MtCO2eq/yr"),
    make_country_map(filter(actual_reduction_map_df, year == 2100), "total_actual_reduction", "Actual reduction (2100)", "MtCO2eq/yr"),
    ncol = 2
  ),
  width = 12,
  height = 5
)

make_top_actual_reduction_plot <- function(plot_year) {
  df <- actual_reduction_total %>%
    filter(region %in% country_codes, year == plot_year) %>%
    left_join(st_drop_geometry(Boundary), by = c("region" = "cty")) %>%
    mutate(country = coalesce(country_name, region)) %>%
    mutate(component = factor(component, levels = c("Land use", "Agriculture"))) %>%
    group_by(region, country) %>%
    mutate(total_actual_reduction = sum(actual_reduction, na.rm = TRUE)) %>%
    ungroup() %>%
    arrange(desc(total_actual_reduction)) %>%
    slice_head(n = 20) %>%
    mutate(country = factor(country, levels = rev(unique(country))))

  ggplot(df, aes(x = actual_reduction, y = country, fill = component)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
    geom_col() +
    scale_fill_manual(values = component_palette) +
    labs(title = paste0("Top actual reduction countries (", plot_year, ")"), x = "MtCO2eq/yr", y = NULL) +
    theme_main
}

save_plot(
  "fig3_3e_top_actual_reduction_2050_2100.png",
  wrap_plots(make_top_actual_reduction_plot(2050), make_top_actual_reduction_plot(2100), ncol = 2),
  width = 12,
  height = 8
)

make_top_actual_reduction_rate_plot <- function(plot_year) {
  df <- actual_reduction_total %>%
    filter(region %in% country_codes, year == plot_year) %>%
    left_join(baseline_country_total %>% filter(year == plot_year), by = c("region", "year")) %>%
    left_join(st_drop_geometry(Boundary), by = c("region" = "cty")) %>%
    mutate(
      country = coalesce(country_name, region),
      component = factor(component, levels = c("Land use", "Agriculture")),
      reduction_rate = if_else(is.finite(baseline_emissions) & baseline_emissions > 0, 100 * actual_reduction / baseline_emissions, NA_real_)
    ) %>%
    filter(is.finite(reduction_rate)) %>%
    group_by(region, country) %>%
    mutate(total_reduction_rate = sum(reduction_rate, na.rm = TRUE)) %>%
    ungroup() %>%
    arrange(desc(total_reduction_rate)) %>%
    slice_head(n = 20) %>%
    mutate(country = factor(country, levels = rev(unique(country))))

  ggplot(df, aes(x = reduction_rate, y = country, fill = component)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
    geom_col() +
    scale_fill_manual(values = component_palette) +
    labs(title = paste0("Top actual reduction rate countries (", plot_year, ")"), x = "Reduction rate from BaU emissions (%)", y = NULL) +
    theme_main
}

save_plot(
  "fig3_3e2_top_actual_reduction_rate_2050_2100.png",
  wrap_plots(make_top_actual_reduction_rate_plot(2050), make_top_actual_reduction_rate_plot(2100), ncol = 2),
  width = 12,
  height = 8
)

actual_concentration_curve <- actual_reduction_country %>%
  filter(year %in% plot_years) %>%
  group_by(year) %>%
  arrange(desc(total_actual_reduction), .by_group = TRUE) %>%
  mutate(
    share = total_actual_reduction / sum(total_actual_reduction, na.rm = TRUE),
    cum_share = cumsum(share),
    cum_country = row_number() / n()
  ) %>%
  ungroup()

p_actual_concentration <- ggplot(actual_concentration_curve, aes(x = cum_country, y = cum_share, color = factor(year))) +
  geom_line(linewidth = 1.1) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey60") +
  scale_x_continuous(labels = percent_format()) +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "Concentration of actual reduction", x = "Cumulative share of countries", y = "Cumulative share of actual reduction") +
  theme_main

save_plot("fig3_3f_actual_reduction_concentration_curve.png", p_actual_concentration, width = 8, height = 5)

make_actual_reduction_portfolio_plot <- function(plot_year) {
  top_regions <- actual_reduction_country %>%
    filter(year == plot_year) %>%
    arrange(desc(total_actual_reduction)) %>%
    slice_head(n = 15) %>%
    pull(region)

  country_order <- actual_reduction_country %>%
    filter(year == plot_year) %>%
    arrange(desc(total_actual_reduction)) %>%
    slice_head(n = 15) %>%
    pull(country)

  df <- actual_reduction_tech_country %>%
    filter(year == plot_year, region %in% top_regions) %>%
    left_join(actual_reduction_country %>% filter(year == plot_year) %>% select(region, country), by = "region") %>%
    mutate(country = factor(country, levels = rev(country_order)))

  ggplot(df, aes(x = actual_reduction, y = country, fill = tech)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
    geom_col() +
    scale_fill_manual(values = category_palette) +
    labs(title = paste0("Actual reduction portfolio of top countries (", plot_year, ")"), x = "MtCO2eq/yr", y = NULL) +
    theme_main
}

save_plot(
  "fig3_3g_actual_reduction_portfolio_2050_2100.png",
  wrap_plots(make_actual_reduction_portfolio_plot(2050), make_actual_reduction_portfolio_plot(2100), ncol = 2),
  width = 12,
  height = 8
)

actual_reduction_heatmap_df <- actual_reduction_tech_country %>%
  filter(year == 2100, region %in% (actual_reduction_country %>% filter(year == 2100) %>% arrange(desc(total_actual_reduction)) %>% slice_head(n = 15) %>% pull(region))) %>%
  left_join(actual_reduction_country %>% filter(year == 2100) %>% select(region, country, total_actual_reduction), by = "region") %>%
  mutate(
    country = factor(country, levels = rev((actual_reduction_country %>% filter(year == 2100) %>% arrange(desc(total_actual_reduction)) %>% slice_head(n = 15) %>% pull(country)))),
    tech = factor(tech, levels = names(category_palette))
  )

p_actual_heatmap <- ggplot(actual_reduction_heatmap_df, aes(x = tech, y = country, fill = actual_reduction)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "#4477AA", mid = "white", high = "#C3327A", midpoint = 0) +
  labs(title = "Technology heatmap in top-actual-reduction countries (2100)", x = NULL, y = NULL) +
  theme_main +
  theme(axis.text.x = element_text(angle = 35, hjust = 1))

save_plot("fig3_3h_actual_reduction_heatmap_2100.png", p_actual_heatmap, width = 10, height = 7)

actual_r5_portfolio <- actual_reduction_tech_country %>%
  filter(year == 2100, region %in% names(r5_labels), region != "World") %>%
  mutate(region_name = recode(region, !!!r5_labels)) %>%
  group_by(region_name, tech) %>%
  summarise(actual_reduction = sum(actual_reduction, na.rm = TRUE), .groups = "drop") %>%
  group_by(region_name) %>%
  mutate(total = sum(actual_reduction, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(region_name = fct_reorder(region_name, total))

p_actual_r5_portfolio <- ggplot(actual_r5_portfolio, aes(x = actual_reduction, y = region_name, fill = tech)) +
  geom_col() +
  scale_fill_manual(values = category_palette) +
  labs(title = "R5 actual reduction portfolio (2100)", x = "MtCO2eq/yr", y = NULL) +
  theme_main

print(p_actual_r5_portfolio)
save_plot("fig3_3i_r5_actual_reduction_portfolio_2100.png", p_actual_r5_portfolio, width = 9, height = 5)

# 3.4 ---------------------------------------------------------------------
save_plot(
  "fig3_4a_market_food_maps_2050_2100.png",
  wrap_plots(
    make_country_map(agmip_core_change %>% filter(year == 2050, metric == "AGR price") %>% transmute(region, value = change), "value", "AGR price change from BaU (2050)", "USD/t"),
    make_country_map(agmip_core_change %>% filter(year == 2050, metric == "Calorie") %>% transmute(region, value = change), "value", "Calorie change from BaU (2050)", "kcal/cap/d"),
    make_country_map(agmip_core_change %>% filter(year == 2100, metric == "AGR price") %>% transmute(region, value = change), "value", "AGR price change from BaU (2100)", "USD/t"),
    make_country_map(agmip_core_change %>% filter(year == 2100, metric == "Calorie") %>% transmute(region, value = change), "value", "Calorie change from BaU (2100)", "kcal/cap/d"),
    ncol = 2
  ),
  width = 12,
  height = 10
)

make_distribution_plot <- function(plot_year) {
  df <- bind_rows(
    agmip_core_change %>% filter(year == plot_year, metric %in% c("Calorie", "AGR price", "AGR production")) %>% transmute(metric, value = change),
    agmip_land_long %>% filter(year == plot_year, land_item %in% c("Cropland", "Managed and primary forest", "Grassland")) %>% transmute(metric = as.character(land_item), value = land_share)
  )

  ggplot(df, aes(x = value, y = "")) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
    geom_boxplot(aes(fill = metric), outlier.size = 0.5, width = 0.45) +
    facet_wrap(~ metric, scales = "free_x", ncol = 3) +
    theme_main +
    theme(
      legend.position = "none",
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      strip.text = element_text(size = 10)
    ) +
    labs(title = paste0("Distribution of side effects (", plot_year, ")"), x = "Change", y = NULL)
}

save_plot(
  "fig3_4b_side_effect_distribution_2050_2100.png",
  wrap_plots(make_distribution_plot(2050), make_distribution_plot(2100), ncol = 2),
  width = 12,
  height = 8
)

top_potential_2100 <- top_potential_country %>%
  filter(year == 2100) %>%
  arrange(desc(total_potential)) %>%
  slice_head(n = 20) %>%
  select(region, country, total_potential)

actual_reduction_2100 <- actual_reduction_total %>%
  filter(year == 2100, region %in% top_potential_2100$region) %>%
  left_join(top_potential_2100, by = "region") %>%
  mutate(
    country = factor(country, levels = rev(top_potential_2100$country)),
    component = factor(component, levels = c("Land use", "Agriculture"))
  )

land_change_2100_abs <- agmip_land_long %>%
  filter(year == 2100, region %in% top_potential_2100$region) %>%
  left_join(top_potential_2100, by = "region") %>%
  mutate(country = factor(country, levels = rev(top_potential_2100$country)))

p_reduction_2100 <- ggplot(actual_reduction_2100, aes(x = actual_reduction, y = country, fill = component)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
  geom_col() +
  scale_fill_manual(values = component_palette) +
  labs(title = "Actual reduction in top-potential countries (2100)", x = "MtCO2eq/yr", y = NULL) +
  theme_main

p_land_abs_2100 <- ggplot(land_change_2100_abs, aes(x = land_change, y = country, fill = land_item)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
  geom_col() +
  scale_fill_manual(values = land_color) +
  labs(title = "Land change from BaU (2100)", x = "1000 ha", y = NULL) +
  theme_main

p_land_share_2100 <- ggplot(land_change_2100_abs, aes(x = land_share, y = country, fill = land_item)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
  geom_col() +
  scale_fill_manual(values = land_color) +
  labs(title = "Land change from BaU (2100)", x = "% of total land area", y = NULL) +
  theme_main

save_plot("fig3_4c_top_reduction_and_land_change_2100.png", wrap_plots(p_reduction_2100, p_land_abs_2100, ncol = 2, widths = c(1, 1.4)), width = 14, height = 8)
save_plot("fig3_4d_top_reduction_and_land_change_share_2100.png", wrap_plots(p_reduction_2100, p_land_share_2100, ncol = 2, widths = c(1, 1.4)), width = 14, height = 8)

# 3.5 ---------------------------------------------------------------------
top_potential_regions_2100 <- top_potential_country %>%
  filter(year == 2100) %>%
  arrange(desc(total_potential)) %>%
  slice_head(n = 15) %>%
  select(region, country, total_potential)

heatmap_indicators <- bind_rows(
  actual_reduction_total %>%
    filter(year == 2100, region %in% top_potential_regions_2100$region) %>%
    transmute(region, metric = paste0(component, " reduction"), value = actual_reduction),
  agmip_core_change %>%
    filter(year == 2100, region %in% top_potential_regions_2100$region) %>%
    transmute(region, metric, value = change),
  agmip_land_long %>%
    filter(year == 2100, region %in% top_potential_regions_2100$region, land_item %in% c("Cropland", "Managed and primary forest", "Grassland")) %>%
    transmute(region, metric = paste0(as.character(land_item), " share"), value = land_share)
) %>%
  left_join(top_potential_regions_2100, by = "region") %>%
  mutate(country = factor(country, levels = rev(top_potential_regions_2100$country)))

p_high_heatmap <- ggplot(heatmap_indicators, aes(x = metric, y = country, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "#4477AA", mid = "white", high = "#CC6677", midpoint = 0) +
  labs(title = "Heterogeneity across high-potential countries (2100)", x = NULL, y = NULL) +
  theme_main +
  theme(axis.text.x = element_text(angle = 40, hjust = 1))

save_plot("fig3_5a_high_potential_heatmap_2100.png", p_high_heatmap, width = 13, height = 8)

goods_panel <- agmip_diff %>%
  filter(
    year == 2100,
    region %in% top_potential_regions_2100$region,
    (variable == "XPRP" & item %in% c("WHT", "RIC", "CGR", "RUM", "NRM", "DRY") & unit == "USD/t") |
      (variable == "PROD" & item %in% c("WHT", "RIC", "CGR", "RUM", "NRM", "DRY") & unit == "1000 t")
  ) %>%
  left_join(top_potential_regions_2100, by = "region") %>%
  mutate(country = factor(country, levels = rev(top_potential_regions_2100$country)), metric = paste(variable, item))

p_goods_panel <- ggplot(goods_panel, aes(x = metric, y = country, fill = change)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "#4477AA", mid = "white", high = "#CC6677", midpoint = 0) +
  labs(title = "Commodity-specific market changes in high-potential countries (2100)", x = NULL, y = NULL) +
  theme_main +
  theme(axis.text.x = element_text(angle = 40, hjust = 1))

save_plot("fig3_5b_goods_change_heatmap_2100.png", p_goods_panel, width = 11, height = 8)

structure_panel <- bind_rows(
  agmip_core_change %>% filter(year == 2100, region %in% top_potential_regions_2100$region, metric %in% c("CRP production", "LSP production", "CRP net trade", "LSP net trade")),
  agmip_land_long %>% filter(year == 2100, region %in% top_potential_regions_2100$region, land_item %in% c("Cropland", "Managed and primary forest", "Grassland")) %>% transmute(region, year, metric = as.character(land_item), change = land_share)
) %>%
  left_join(top_potential_regions_2100, by = "region") %>%
  mutate(country = factor(country, levels = rev(top_potential_regions_2100$country)))

p_structure_panel <- ggplot(structure_panel, aes(x = change, y = country)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
  geom_col(fill = "#5B8E7D") +
  facet_wrap(~ metric, scales = "free_x", ncol = 3) +
  labs(title = "Structural changes in high-potential countries (2100)", x = "Change", y = NULL) +
  theme_main

save_plot("fig3_5c_structural_change_panel_2100.png", p_structure_panel, width = 13, height = 8)

# 3.6 ---------------------------------------------------------------------
make_tradeoff_scatter <- function(plot_year, metric_name, title_text) {
  df <- actual_reduction_total %>%
    filter(year == plot_year, component == "Agriculture") %>%
    distinct(region, year, total_actual_reduction) %>%
    left_join(agmip_core_change %>% filter(year == plot_year, metric == metric_name) %>% select(region, year, change), by = c("region", "year")) %>%
    left_join(st_drop_geometry(Boundary), by = c("region" = "cty")) %>%
    left_join(region_map, by = "region") %>%
    mutate(country = coalesce(country_name, region), region5 = factor(region5, levels = names(region_palette))) %>%
    filter(is.finite(total_actual_reduction), is.finite(change))

  label_df <- df %>% slice_max(order_by = total_actual_reduction, n = 12, with_ties = FALSE)

  ggplot(df, aes(x = change, y = total_actual_reduction)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
    geom_point(aes(color = region5), size = 2.4, alpha = 0.85) +
    geom_text(data = label_df, aes(label = country, color = region5), size = 3, nudge_y = 0.02 * max(df$total_actual_reduction, na.rm = TRUE), show.legend = FALSE) +
    scale_color_manual(values = region_border_palette) +
    labs(title = paste0(title_text, " (", plot_year, ")"), x = metric_name, y = "Actual reduction") +
    theme_main
}

save_plot("fig3_6a_tradeoff_calorie_2050_2100.png", wrap_plots(make_tradeoff_scatter(2050, "Calorie", "Actual reduction vs calorie change"), make_tradeoff_scatter(2100, "Calorie", "Actual reduction vs calorie change"), ncol = 2), width = 12, height = 6)
save_plot("fig3_6b_tradeoff_price_2050_2100.png", wrap_plots(make_tradeoff_scatter(2050, "AGR price", "Actual reduction vs AGR price change"), make_tradeoff_scatter(2100, "AGR price", "Actual reduction vs AGR price change"), ncol = 2), width = 12, height = 6)

make_tradeoff_scatter_full_zoom <- function(plot_year, metric_name, title_text, zoom_top_n = 5) {
  df <- actual_reduction_total %>%
    filter(year == plot_year, component == "Agriculture") %>%
    distinct(region, year, total_actual_reduction) %>%
    left_join(agmip_core_change %>% filter(year == plot_year, metric == metric_name) %>% select(region, year, change), by = c("region", "year")) %>%
    left_join(st_drop_geometry(Boundary), by = c("region" = "cty")) %>%
    left_join(region_map, by = "region") %>%
    mutate(country = coalesce(country_name, region), region5 = factor(region5, levels = names(region_palette))) %>%
    filter(is.finite(total_actual_reduction), is.finite(change))

  label_full <- df %>% slice_max(order_by = total_actual_reduction, n = 12, with_ties = FALSE)
  zoom_regions <- df %>% slice_max(order_by = total_actual_reduction, n = zoom_top_n, with_ties = FALSE) %>% pull(region)
  df_zoom <- df %>% filter(!region %in% zoom_regions)
  label_zoom <- df_zoom %>% slice_max(order_by = total_actual_reduction, n = 12, with_ties = FALSE)

  p_full <- ggplot(df, aes(x = change, y = total_actual_reduction)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
    geom_point(aes(color = region5), size = 2.4, alpha = 0.85) +
    geom_text(data = label_full, aes(label = country, color = region5), size = 3, nudge_y = 0.02 * max(df$total_actual_reduction, na.rm = TRUE), show.legend = FALSE) +
    scale_color_manual(values = region_border_palette) +
    labs(title = paste0(title_text, " (", plot_year, ", full)"), x = metric_name, y = "Actual reduction") +
    theme_main

  p_zoom <- ggplot(df_zoom, aes(x = change, y = total_actual_reduction)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
    geom_point(aes(color = region5), size = 2.4, alpha = 0.85) +
    geom_text(data = label_zoom, aes(label = country, color = region5), size = 3, nudge_y = 0.02 * max(df_zoom$total_actual_reduction, na.rm = TRUE), show.legend = FALSE) +
    scale_color_manual(values = region_border_palette) +
    labs(title = paste0(title_text, " (", plot_year, ", zoom)"), x = metric_name, y = "Actual reduction") +
    theme_main

  wrap_plots(p_full, p_zoom, ncol = 2)
}

save_plot("fig3_6a2_tradeoff_calorie_full_zoom_2050_2100.png", wrap_plots(make_tradeoff_scatter_full_zoom(2050, "Calorie", "Actual reduction vs calorie change"), make_tradeoff_scatter_full_zoom(2100, "Calorie", "Actual reduction vs calorie change"), ncol = 1), width = 12, height = 12)
save_plot("fig3_6b2_tradeoff_price_full_zoom_2050_2100.png", wrap_plots(make_tradeoff_scatter_full_zoom(2050, "AGR price", "Actual reduction vs AGR price change"), make_tradeoff_scatter_full_zoom(2100, "AGR price", "Actual reduction vs AGR price change"), ncol = 1), width = 12, height = 12)

make_land_tradeoff <- function(plot_year) {
  df <- actual_reduction_total %>%
    filter(year == plot_year) %>%
    distinct(region, year, total_actual_reduction) %>%
    left_join(agmip_land_long %>% filter(year == plot_year, land_item == "Managed and primary forest") %>% select(region, year, land_share), by = c("region", "year")) %>%
    left_join(st_drop_geometry(Boundary), by = c("region" = "cty")) %>%
    left_join(region_map, by = "region") %>%
    mutate(country = coalesce(country_name, region), region5 = factor(region5, levels = names(region_palette))) %>%
    filter(is.finite(total_actual_reduction), is.finite(land_share))

  label_df <- df %>% slice_max(order_by = total_actual_reduction, n = 12, with_ties = FALSE)

  ggplot(df, aes(x = land_share, y = total_actual_reduction)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
    geom_point(aes(color = region5), size = 2.4, alpha = 0.85) +
    geom_text(data = label_df, aes(label = country, color = region5), size = 3, nudge_y = 0.02 * max(df$total_actual_reduction, na.rm = TRUE), show.legend = FALSE) +
    scale_color_manual(values = region_border_palette) +
    labs(title = paste0("Actual reduction vs forest area change (", plot_year, ")"), x = "Forest area change (% of total land)", y = "Actual reduction") +
    theme_main
}

save_plot("fig3_6c_tradeoff_land_2050_2100.png", wrap_plots(make_land_tradeoff(2050), make_land_tradeoff(2100), ncol = 2), width = 12, height = 6)

make_bubble_plot <- function(plot_year, metric_col, x_label, title_text) {
  df <- actual_reduction_total %>%
    filter(year == plot_year) %>%
    distinct(region, year, total_actual_reduction) %>%
    left_join(region_map, by = "region") %>%
    left_join(st_drop_geometry(Boundary), by = c("region" = "cty")) %>%
    mutate(country = coalesce(country_name, region), region5 = factor(region5, levels = names(region_palette)))

  x_df <- switch(
    metric_col,
    calorie_change = agmip_core_change %>% filter(year == plot_year, metric == "Calorie") %>% transmute(region, year, x = change),
    agr_price_change = agmip_core_change %>% filter(year == plot_year, metric == "AGR price") %>% transmute(region, year, x = change),
    forest_area_change = agmip_land_long %>% filter(year == plot_year, land_item == "Managed and primary forest") %>% transmute(region, year, x = land_change),
    cropland_area_change = agmip_land_long %>% filter(year == plot_year, land_item == "Cropland") %>% transmute(region, year, x = land_change),
    land_conversion_abs = agmip_land_conversion %>% filter(year == plot_year) %>% transmute(region, year, x = land_conversion_abs),
    land_conversion_share = agmip_land_conversion %>% filter(year == plot_year) %>% transmute(region, year, x = land_conversion_share),
    lsp_prod_change = agmip_core_change %>% filter(year == plot_year, metric == "LSP production") %>% transmute(region, year, x = change),
    crp_prod_change = agmip_core_change %>% filter(year == plot_year, metric == "CRP production") %>% transmute(region, year, x = change)
  )

  bubble_df <- df %>%
    left_join(x_df, by = c("region", "year")) %>%
    mutate(
      size_bin = cut(
        total_actual_reduction,
        breaks = c(-Inf, 5, 10, 15, 20, Inf),
        include.lowest = TRUE,
        right = FALSE,
        labels = c("0-5 MtCO2eq/yr", "5-10 MtCO2eq/yr", "10-15 MtCO2eq/yr", "15-20 MtCO2eq/yr", "20+ MtCO2eq/yr")
      )
    ) %>%
    filter(is.finite(total_actual_reduction), is.finite(x), !is.na(region5))

  label_df <- bubble_df %>% filter(size_bin %in% c("15-20 MtCO2eq/yr", "20+ MtCO2eq/yr"))

  ggplot(bubble_df, aes(x = x, y = total_actual_reduction)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
    geom_point(aes(size = size_bin, fill = region5, color = region5, alpha = size_bin), shape = 21, stroke = 0.35) +
    geom_text(data = label_df, aes(label = country, color = region5), size = 3.2, fontface = "bold", nudge_y = 0.03 * max(bubble_df$total_actual_reduction, na.rm = TRUE), show.legend = FALSE) +
    scale_fill_manual(values = region_palette) +
    scale_color_manual(values = region_border_palette) +
    scale_size_manual(values = c(2.5, 4, 5.5, 7, 8.5), drop = FALSE) +
    scale_alpha_manual(values = c("0-5 MtCO2eq/yr" = 0.56, "5-10 MtCO2eq/yr" = 0.62, "10-15 MtCO2eq/yr" = 0.68, "15-20 MtCO2eq/yr" = 0.74, "20+ MtCO2eq/yr" = 0.80), guide = "none", drop = FALSE) +
    labs(title = paste0(title_text, " (", plot_year, ")"), x = x_label, y = "Actual reduction (MtCO2eq/yr)") +
    theme_main
}

save_plot("fig3_6d_bubble_2050_calorie.png", make_bubble_plot(2050, "calorie_change", "Calorie change", "Actual reduction vs calorie change"), width = 8, height = 6)
save_plot("fig3_6e_bubble_2050_agr_price.png", make_bubble_plot(2050, "agr_price_change", "AGR price change", "Actual reduction vs AGR price change"), width = 8, height = 6)
save_plot("fig3_6f_bubble_2050_forest.png", make_bubble_plot(2050, "forest_area_change", "Forest area change", "Actual reduction vs forest area change"), width = 8, height = 6)
save_plot("fig3_6g_bubble_2050_cropland.png", make_bubble_plot(2050, "cropland_area_change", "Cropland change", "Actual reduction vs cropland change"), width = 8, height = 6)
save_plot("fig3_6h_bubble_2050_lsp_prod.png", make_bubble_plot(2050, "lsp_prod_change", "LSP production change", "Actual reduction vs livestock production change"), width = 8, height = 6)
save_plot("fig3_6i_bubble_2050_crp_prod.png", make_bubble_plot(2050, "crp_prod_change", "CRP production change", "Actual reduction vs crop production change"), width = 8, height = 6)
save_plot("fig3_6j_bubble_2100_calorie.png", make_bubble_plot(2100, "calorie_change", "Calorie change", "Actual reduction vs calorie change"), width = 8, height = 6)
save_plot("fig3_6k_bubble_2100_agr_price.png", make_bubble_plot(2100, "agr_price_change", "AGR price change", "Actual reduction vs AGR price change"), width = 8, height = 6)
save_plot("fig3_6l_bubble_2100_forest.png", make_bubble_plot(2100, "forest_area_change", "Forest area change", "Actual reduction vs forest area change"), width = 8, height = 6)
save_plot("fig3_6m_bubble_2100_cropland.png", make_bubble_plot(2100, "cropland_area_change", "Cropland change", "Actual reduction vs cropland change"), width = 8, height = 6)
save_plot("fig3_6n_bubble_2100_lsp_prod.png", make_bubble_plot(2100, "lsp_prod_change", "LSP production change", "Actual reduction vs livestock production change"), width = 8, height = 6)
save_plot("fig3_6o_bubble_2100_crp_prod.png", make_bubble_plot(2100, "crp_prod_change", "CRP production change", "Actual reduction vs crop production change"), width = 8, height = 6)
save_plot("fig3_6ab_bubble_2050_land_conversion_abs.png", make_bubble_plot(2050, "land_conversion_abs", "Land conversion area (1000 ha)", "Actual reduction vs land conversion area"), width = 8, height = 6)
save_plot("fig3_6ac_bubble_2050_land_conversion_share.png", make_bubble_plot(2050, "land_conversion_share", "Land conversion area (% of total land)", "Actual reduction vs land conversion share"), width = 8, height = 6)
save_plot("fig3_6ad_bubble_2100_land_conversion_abs.png", make_bubble_plot(2100, "land_conversion_abs", "Land conversion area (1000 ha)", "Actual reduction vs land conversion area"), width = 8, height = 6)
save_plot("fig3_6ae_bubble_2100_land_conversion_share.png", make_bubble_plot(2100, "land_conversion_share", "Land conversion area (% of total land)", "Actual reduction vs land conversion share"), width = 8, height = 6)

make_normalized_bubble_plot <- function(plot_year, metric_col, norm_mode, x_label, title_text) {
  norm_mode <- match.arg(norm_mode, c("emissions", "population", "land"))

  df <- actual_reduction_total %>%
    filter(year == plot_year) %>%
    distinct(region, year, total_actual_reduction) %>%
    left_join(baseline_country_total %>% filter(year == plot_year), by = c("region", "year")) %>%
    left_join(agmip_population_area %>% filter(year == plot_year), by = c("region", "year")) %>%
    left_join(region_map, by = "region") %>%
    left_join(st_drop_geometry(Boundary), by = c("region" = "cty")) %>%
    mutate(
      country = coalesce(country_name, region),
      region5 = factor(region5, levels = names(region_palette)),
      y = case_when(
        norm_mode == "emissions" & baseline_emissions > 0 ~ total_actual_reduction / baseline_emissions,
        norm_mode == "population" & population_million > 0 ~ total_actual_reduction / population_million,
        norm_mode == "land" & land_area_kha > 0 ~ total_actual_reduction / land_area_kha,
        TRUE ~ NA_real_
      )
    )

  x_df <- switch(
    metric_col,
    calorie_change = agmip_core_change %>% filter(year == plot_year, metric == "Calorie") %>% transmute(region, year, x = change),
    agr_price_change = agmip_core_change %>% filter(year == plot_year, metric == "AGR price") %>% transmute(region, year, x = change),
    forest_area_change = agmip_land_long %>% filter(year == plot_year, land_item == "Managed and primary forest") %>% transmute(region, year, x = land_change),
    cropland_area_change = agmip_land_long %>% filter(year == plot_year, land_item == "Cropland") %>% transmute(region, year, x = land_change),
    lsp_prod_change = agmip_core_change %>% filter(year == plot_year, metric == "LSP production") %>% transmute(region, year, x = change),
    crp_prod_change = agmip_core_change %>% filter(year == plot_year, metric == "CRP production") %>% transmute(region, year, x = change)
  )

  bubble_df <- df %>%
    left_join(x_df, by = c("region", "year")) %>%
    mutate(
      size_bin = cut(
        total_actual_reduction,
        breaks = c(-Inf, 5, 10, 15, 20, Inf),
        include.lowest = TRUE,
        right = FALSE,
        labels = c("0-5 MtCO2eq/yr", "5-10 MtCO2eq/yr", "10-15 MtCO2eq/yr", "15-20 MtCO2eq/yr", "20+ MtCO2eq/yr")
      )
    ) %>%
    filter(is.finite(y), is.finite(x), !is.na(region5))

  label_df <- bubble_df %>% filter(size_bin %in% c("15-20 MtCO2eq/yr", "20+ MtCO2eq/yr"))
  y_label <- case_when(
    norm_mode == "emissions" ~ "Actual reduction / No emissions",
    norm_mode == "population" ~ "Actual reduction / population",
    norm_mode == "land" ~ "Actual reduction / land area"
  )

  ggplot(bubble_df, aes(x = x, y = y)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
    geom_point(aes(size = size_bin, fill = region5, color = region5, alpha = size_bin), shape = 21, stroke = 0.35) +
    geom_text(data = label_df, aes(label = country, color = region5), size = 3.2, fontface = "bold", nudge_y = 0.03 * max(abs(bubble_df$y), na.rm = TRUE), show.legend = FALSE) +
    scale_fill_manual(values = region_palette) +
    scale_color_manual(values = region_border_palette) +
    scale_size_manual(values = c(2.5, 4, 5.5, 7, 8.5), drop = FALSE) +
    scale_alpha_manual(values = c("0-5 MtCO2eq/yr" = 0.56, "5-10 MtCO2eq/yr" = 0.62, "10-15 MtCO2eq/yr" = 0.68, "15-20 MtCO2eq/yr" = 0.74, "20+ MtCO2eq/yr" = 0.80), guide = "none", drop = FALSE) +
    labs(title = paste0(title_text, " (", plot_year, ")"), x = x_label, y = y_label) +
    theme_main
}

save_plot("fig3_6p_bubble_2050_calorie_per_no_emissions.png", make_normalized_bubble_plot(2050, "calorie_change", "emissions", "Calorie change", "Reduction per No emissions vs calorie change"), width = 8, height = 6)
save_plot("fig3_6q_bubble_2050_agr_price_per_no_emissions.png", make_normalized_bubble_plot(2050, "agr_price_change", "emissions", "AGR price change", "Reduction per No emissions vs AGR price change"), width = 8, height = 6)
save_plot("fig3_6r_bubble_2050_calorie_per_population.png", make_normalized_bubble_plot(2050, "calorie_change", "population", "Calorie change", "Reduction per population vs calorie change"), width = 8, height = 6)
save_plot("fig3_6s_bubble_2050_agr_price_per_population.png", make_normalized_bubble_plot(2050, "agr_price_change", "population", "AGR price change", "Reduction per population vs AGR price change"), width = 8, height = 6)
save_plot("fig3_6t_bubble_2050_calorie_per_land.png", make_normalized_bubble_plot(2050, "calorie_change", "land", "Calorie change", "Reduction per land area vs calorie change"), width = 8, height = 6)
save_plot("fig3_6u_bubble_2050_agr_price_per_land.png", make_normalized_bubble_plot(2050, "agr_price_change", "land", "AGR price change", "Reduction per land area vs AGR price change"), width = 8, height = 6)
save_plot("fig3_6v_bubble_2100_calorie_per_no_emissions.png", make_normalized_bubble_plot(2100, "calorie_change", "emissions", "Calorie change", "Reduction per No emissions vs calorie change"), width = 8, height = 6)
save_plot("fig3_6w_bubble_2100_agr_price_per_no_emissions.png", make_normalized_bubble_plot(2100, "agr_price_change", "emissions", "AGR price change", "Reduction per No emissions vs AGR price change"), width = 8, height = 6)
save_plot("fig3_6x_bubble_2100_calorie_per_population.png", make_normalized_bubble_plot(2100, "calorie_change", "population", "Calorie change", "Reduction per population vs calorie change"), width = 8, height = 6)
save_plot("fig3_6y_bubble_2100_agr_price_per_population.png", make_normalized_bubble_plot(2100, "agr_price_change", "population", "AGR price change", "Reduction per population vs AGR price change"), width = 8, height = 6)
save_plot("fig3_6z_bubble_2100_calorie_per_land.png", make_normalized_bubble_plot(2100, "calorie_change", "land", "Calorie change", "Reduction per land area vs calorie change"), width = 8, height = 6)
save_plot("fig3_6aa_bubble_2100_agr_price_per_land.png", make_normalized_bubble_plot(2100, "agr_price_change", "land", "AGR price change", "Reduction per land area vs AGR price change"), width = 8, height = 6)

# 3.7 ---------------------------------------------------------------------
make_quadrant_plot <- function(plot_year, x_mode = c("calorie", "land")) {
  x_mode <- match.arg(x_mode)
  x_df <- switch(
    x_mode,
    calorie = agmip_core_change %>% filter(year == plot_year, metric == "Calorie") %>% transmute(region, year, x = change),
    land = agmip_land_long %>% filter(year == plot_year, land_item == "Managed and primary forest") %>% transmute(region, year, x = land_share)
  )

  x_label <- if (x_mode == "calorie") "Calorie change" else "Forest area change (% total land)"

  df <- actual_reduction_total %>%
    filter(year == plot_year) %>%
    distinct(region, year, total_actual_reduction) %>%
    left_join(x_df, by = c("region", "year")) %>%
    left_join(region_map, by = "region") %>%
    left_join(st_drop_geometry(Boundary), by = c("region" = "cty")) %>%
    mutate(country = coalesce(country_name, region), region5 = factor(region5, levels = names(region_palette))) %>%
    filter(is.finite(total_actual_reduction), is.finite(x))

  label_df <- df %>% slice_max(order_by = total_actual_reduction, n = 12, with_ties = FALSE)

  ggplot(df, aes(x = x, y = total_actual_reduction)) +
    geom_hline(yintercept = median(df$total_actual_reduction, na.rm = TRUE), linetype = "dashed", color = "grey60", linewidth = 0.35) +
    geom_vline(xintercept = median(df$x, na.rm = TRUE), linetype = "dashed", color = "grey60", linewidth = 0.35) +
    geom_point(aes(color = region5), size = 2.4, alpha = 0.85) +
    geom_text(data = label_df, aes(label = country, color = region5), size = 3, nudge_y = 0.02 * max(df$total_actual_reduction, na.rm = TRUE), show.legend = FALSE) +
    scale_color_manual(values = region_border_palette) +
    labs(title = paste0("Quadrant view (", plot_year, ")"), x = x_label, y = "Actual reduction") +
    theme_main
}

save_plot("fig3_7a_quadrant_calorie_2050_2100.png", wrap_plots(make_quadrant_plot(2050, "calorie"), make_quadrant_plot(2100, "calorie"), ncol = 2), width = 12, height = 6)
save_plot("fig3_7b_quadrant_land_2050_2100.png", wrap_plots(make_quadrant_plot(2050, "land"), make_quadrant_plot(2100, "land"), ncol = 2), width = 12, height = 6)

# appendix residual emissions ----------------------------------------------
residual_emissions_country <- GEER_AFOLUB_REF %>%
  filter(
    scenario %in% c(scenario_no, scenario_landint),
    region %in% c(country_codes, names(r5_labels)),
    year %in% plot_years,
    variable %in% c(
      "Emi_equ_GHG_AFO",
      "Emi_equ_GHG_AFO_Agr",
      "Emi_equ_GHG_AFO_Lan",
      "Emi_Red_equ_GHG_AFO",
      "Emi_Red_equ_GHG_AFO_Agr",
      "Emi_Red_equ_GHG_AFO_Lan"
    )
  ) %>%
  select(scenario, region, year, variable, value) %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  mutate(
    residual_total = Emi_equ_GHG_AFO - Emi_Red_equ_GHG_AFO,
    residual_agriculture = Emi_equ_GHG_AFO_Agr - Emi_Red_equ_GHG_AFO_Agr,
    residual_land = Emi_equ_GHG_AFO_Lan - Emi_Red_equ_GHG_AFO_Lan
  )

residual_map_df <- residual_emissions_country %>%
  filter(region %in% country_codes)

save_plot(
  "figA1_residual_emissions_map_no_landint_2050_2100.png",
  wrap_plots(
    make_country_map(filter(residual_map_df, scenario == scenario_no, year == 2050), "residual_total", "Residual AFOLU emissions, No (2050)", "MtCO2eq/yr"),
    make_country_map(filter(residual_map_df, scenario == scenario_no, year == 2100), "residual_total", "Residual AFOLU emissions, No (2100)", "MtCO2eq/yr"),
    make_country_map(filter(residual_map_df, scenario == scenario_landint, year == 2050), "residual_total", "Residual AFOLU emissions, LandInt (2050)", "MtCO2eq/yr"),
    make_country_map(filter(residual_map_df, scenario == scenario_landint, year == 2100), "residual_total", "Residual AFOLU emissions, LandInt (2100)", "MtCO2eq/yr"),
    ncol = 2
  ),
  width = 12,
  height = 10
)

make_top_residual_plot <- function(plot_year, plot_scenario) {
  df <- residual_emissions_country %>%
    filter(region %in% country_codes, year == plot_year, scenario == plot_scenario) %>%
    transmute(region, year, residual_agriculture, residual_land, residual_total) %>%
    pivot_longer(cols = c(residual_land, residual_agriculture), names_to = "component", values_to = "value") %>%
    mutate(component = recode(component, residual_land = "Land use", residual_agriculture = "Agriculture")) %>%
    left_join(st_drop_geometry(Boundary), by = c("region" = "cty")) %>%
    mutate(country = coalesce(country_name, region)) %>%
    group_by(region, country) %>%
    mutate(residual_total = sum(value, na.rm = TRUE)) %>%
    ungroup() %>%
    arrange(desc(residual_total)) %>%
    slice_head(n = 20) %>%
    mutate(country = factor(country, levels = rev(unique(country))), component = factor(component, levels = c("Land use", "Agriculture")))

  ggplot(df, aes(x = value, y = country, fill = component)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
    geom_col() +
    scale_fill_manual(values = component_palette) +
    labs(title = paste0("Top residual emitters, ", if_else(plot_scenario == scenario_no, "No", "LandInt"), " (", plot_year, ")"), x = "MtCO2eq/yr", y = NULL) +
    theme_main
}

save_plot(
  "figA2_top_residual_emitters_no_landint_2050_2100.png",
  wrap_plots(
    make_top_residual_plot(2050, scenario_no),
    make_top_residual_plot(2100, scenario_no),
    make_top_residual_plot(2050, scenario_landint),
    make_top_residual_plot(2100, scenario_landint),
    ncol = 2
  ),
  width = 12,
  height = 14
)

residual_concentration_curve <- residual_emissions_country %>%
  filter(region %in% country_codes, year %in% plot_years, scenario %in% c(scenario_no, scenario_landint)) %>%
  group_by(scenario, year) %>%
  arrange(desc(residual_total), .by_group = TRUE) %>%
  mutate(
    share = residual_total / sum(residual_total, na.rm = TRUE),
    cum_share = cumsum(share),
    cum_country = row_number() / n(),
    scenario_label = if_else(scenario == scenario_no, "No", "LandInt")
  ) %>%
  ungroup()

p_residual_concentration <- ggplot(residual_concentration_curve, aes(x = cum_country, y = cum_share, color = interaction(scenario_label, year, sep = " "))) +
  geom_line(linewidth = 1.1) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey60") +
  scale_x_continuous(labels = percent_format()) +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "Concentration of residual AFOLU emissions", x = "Cumulative share of countries", y = "Cumulative share of residual emissions") +
  theme_main

save_plot("figA3_residual_emissions_concentration_curve.png", p_residual_concentration, width = 8, height = 5)

make_residual_portfolio_plot <- function(plot_year, plot_scenario) {
  top_regions <- residual_emissions_country %>%
    filter(region %in% country_codes, year == plot_year, scenario == plot_scenario) %>%
    arrange(desc(residual_total)) %>%
    slice_head(n = 15) %>%
    pull(region)

  country_order <- residual_emissions_country %>%
    filter(region %in% country_codes, year == plot_year, scenario == plot_scenario) %>%
    arrange(desc(residual_total)) %>%
    slice_head(n = 15) %>%
    left_join(st_drop_geometry(Boundary), by = c("region" = "cty")) %>%
    mutate(country = coalesce(country_name, region)) %>%
    pull(country)

  df <- residual_emissions_country %>%
    filter(region %in% top_regions, year == plot_year, scenario == plot_scenario) %>%
    transmute(region, residual_agriculture, residual_land) %>%
    pivot_longer(cols = c(residual_land, residual_agriculture), names_to = "component", values_to = "value") %>%
    mutate(component = recode(component, residual_land = "Land use", residual_agriculture = "Agriculture")) %>%
    left_join(st_drop_geometry(Boundary), by = c("region" = "cty")) %>%
    mutate(country = coalesce(country_name, region), country = factor(country, levels = rev(country_order)))

  ggplot(df, aes(x = value, y = country, fill = component)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
    geom_col() +
    scale_fill_manual(values = component_palette) +
    labs(title = paste0("Residual emissions portfolio, ", if_else(plot_scenario == scenario_no, "No", "LandInt"), " (", plot_year, ")"), x = "MtCO2eq/yr", y = NULL) +
    theme_main
}

save_plot(
  "figA4_residual_emissions_portfolio_no_landint_2050_2100.png",
  wrap_plots(
    make_residual_portfolio_plot(2050, scenario_no),
    make_residual_portfolio_plot(2100, scenario_no),
    make_residual_portfolio_plot(2050, scenario_landint),
    make_residual_portfolio_plot(2100, scenario_landint),
    ncol = 2
  ),
  width = 12,
  height = 14
)

# appendix AGMIP actual reduction -----------------------------------------
agmip_actual_map_df <- agmip_emis_actual_reduction %>%
  filter(region %in% country_codes, item == "tot")

save_plot(
  "figA5_agmip_actual_reduction_map_2050_2100.png",
  wrap_plots(
    make_country_map(filter(agmip_actual_map_df, year == 2050), "actual_reduction", "AGMIP actual reduction from EMIS (2050)", "MtCO2e"),
    make_country_map(filter(agmip_actual_map_df, year == 2100), "actual_reduction", "AGMIP actual reduction from EMIS (2100)", "MtCO2e"),
    ncol = 2
  ),
  width = 12,
  height = 5
)

make_agmip_top_source_plot <- function(plot_year) {
  top_regions <- agmip_ag_reduction_rank %>%
    filter(year == plot_year) %>%
    arrange(desc(ag_reduction)) %>%
    slice_head(n = 15) %>%
    pull(region)

  country_order <- agmip_ag_reduction_rank %>%
    filter(year == plot_year) %>%
    arrange(desc(ag_reduction)) %>%
    slice_head(n = 15) %>%
    pull(country)

  df <- agmip_emis_actual_reduction %>%
    filter(year == plot_year, region %in% top_regions, item != "tot") %>%
    left_join(agmip_ag_reduction_rank %>% filter(year == plot_year) %>% select(region, country), by = "region") %>%
    mutate(country = factor(country, levels = rev(country_order)))

  ggplot(df, aes(x = actual_reduction, y = country, fill = source)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
    geom_col() +
    labs(title = paste0("AGMIP actual reduction by source in top agricultural reduction countries (", plot_year, ")"), x = "MtCO2e", y = NULL) +
    theme_main
}

save_plot(
  "figA6_agmip_actual_reduction_sources_top_ag_reduction_2050_2100.png",
  wrap_plots(make_agmip_top_source_plot(2050), make_agmip_top_source_plot(2100), ncol = 2),
  width = 12,
  height = 8
)

make_agmip_ag_reduction_rate_plot <- function(plot_year) {
  df <- agmip_ag_reduction_rate %>%
    filter(year == plot_year, is.finite(ag_reduction_rate)) %>%
    arrange(desc(ag_reduction_rate)) %>%
    slice_head(n = 20) %>%
    mutate(country = factor(country, levels = rev(country)))

  ggplot(df, aes(x = ag_reduction_rate, y = country)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
    geom_col(fill = "#5C9BD5") +
    labs(title = paste0("Top agricultural actual reduction rate (", plot_year, ")"), x = "Agricultural reduction / BaU agricultural emissions (%)", y = NULL) +
    theme_main
}

save_plot(
  "figA6b_agmip_actual_reduction_rate_top_ag_2050_2100.png",
  wrap_plots(make_agmip_ag_reduction_rate_plot(2050), make_agmip_ag_reduction_rate_plot(2100), ncol = 2),
  width = 12,
  height = 8
)

make_agmip_ag_reduction_rate_r5_plot <- function(plot_year) {
  df <- agmip_ag_reduction_rate_r5 %>%
    filter(year == plot_year, is.finite(ag_reduction_rate)) %>%
    mutate(region_name = factor(region_name, levels = rev(c("World", "OECD+EU", "Asia", "Africa/Middle East", "Latin America", "Former Soviet Union"))))

  ggplot(df, aes(x = ag_reduction_rate, y = region_name, fill = region_name)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
    geom_col(show.legend = FALSE) +
    scale_fill_manual(values = c("World" = "#4C4C4C", region_palette)) +
    labs(title = paste0("Agricultural actual reduction rate, World and R5 (", plot_year, ")"), x = "Agricultural reduction / BaU agricultural emissions (%)", y = NULL) +
    theme_main
}

save_plot(
  "figA6b2_agmip_actual_reduction_rate_world_r5_2050_2100.png",
  wrap_plots(make_agmip_ag_reduction_rate_r5_plot(2050), make_agmip_ag_reduction_rate_r5_plot(2100), ncol = 2),
  width = 11,
  height = 6
)

make_agmip_ag_rate_bubble <- function(plot_year, metric_col, x_label, title_text) {
  df <- agmip_ag_reduction_rate %>%
    filter(year == plot_year, is.finite(ag_reduction_rate)) %>%
    left_join(region_map, by = "region") %>%
    mutate(
      country = coalesce(country, region),
      region5 = factor(region5, levels = names(region_palette))
    )

  x_df <- switch(
    metric_col,
    calorie_change = agmip_core_change %>% filter(year == plot_year, metric == "Calorie") %>% transmute(region, year, x = change),
    agr_price_change = agmip_core_change %>% filter(year == plot_year, metric == "AGR price") %>% transmute(region, year, x = change),
    lsp_prod_change = agmip_core_change %>% filter(year == plot_year, metric == "LSP production") %>% transmute(region, year, x = change),
    crp_prod_change = agmip_core_change %>% filter(year == plot_year, metric == "CRP production") %>% transmute(region, year, x = change),
    forest_area_change = agmip_land_long %>% filter(year == plot_year, land_item == "Managed and primary forest") %>% transmute(region, year, x = land_change),
    cropland_area_change = agmip_land_long %>% filter(year == plot_year, land_item == "Cropland") %>% transmute(region, year, x = land_change),
    grassland_area_change = agmip_land_long %>% filter(year == plot_year, land_item == "Grassland") %>% transmute(region, year, x = land_change)
  )

  bubble_df <- df %>%
    left_join(x_df, by = c("region", "year")) %>%
    mutate(
      size_bin = factor(ntile(ag_reduction, 5), levels = 1:5, labels = c("Q1", "Q2", "Q3", "Q4", "Q5"))
    ) %>%
    filter(is.finite(x), !is.na(region5))

  label_df <- bubble_df %>% filter(size_bin %in% c("Q4", "Q5"))

  ggplot(bubble_df, aes(x = x, y = ag_reduction_rate)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
    geom_point(aes(size = size_bin, fill = region5, color = region5, alpha = size_bin), shape = 21, stroke = 0.35) +
    geom_text(data = label_df, aes(label = country, color = region5), size = 3.1, fontface = "bold", nudge_y = 0.03 * max(bubble_df$ag_reduction_rate, na.rm = TRUE), show.legend = FALSE) +
    scale_fill_manual(values = region_palette) +
    scale_color_manual(values = region_border_palette) +
    scale_size_manual(values = c(2.5, 4, 5.5, 7, 8.5), drop = FALSE) +
    scale_alpha_manual(values = c("Q1" = 0.56, "Q2" = 0.62, "Q3" = 0.68, "Q4" = 0.74, "Q5" = 0.80), guide = "none", drop = FALSE) +
    labs(title = paste0(title_text, " (", plot_year, ")"), x = x_label, y = "Agricultural reduction / BaU agricultural emissions (%)") +
    theme_main
}

save_plot("figA6c_agmip_ag_rate_bubble_2050_calorie.png", make_agmip_ag_rate_bubble(2050, "calorie_change", "Calorie change", "Agricultural reduction rate vs calorie change"), width = 8, height = 6)
save_plot("figA6d_agmip_ag_rate_bubble_2050_agr_price.png", make_agmip_ag_rate_bubble(2050, "agr_price_change", "AGR price change", "Agricultural reduction rate vs AGR price change"), width = 8, height = 6)
save_plot("figA6e_agmip_ag_rate_bubble_2050_lsp_prod.png", make_agmip_ag_rate_bubble(2050, "lsp_prod_change", "LSP production change", "Agricultural reduction rate vs livestock production change"), width = 8, height = 6)
save_plot("figA6f_agmip_ag_rate_bubble_2050_crp_prod.png", make_agmip_ag_rate_bubble(2050, "crp_prod_change", "CRP production change", "Agricultural reduction rate vs crop production change"), width = 8, height = 6)
save_plot("figA6g_agmip_ag_rate_bubble_2050_forest.png", make_agmip_ag_rate_bubble(2050, "forest_area_change", "Forest area change", "Agricultural reduction rate vs forest area change"), width = 8, height = 6)
save_plot("figA6h_agmip_ag_rate_bubble_2050_cropland.png", make_agmip_ag_rate_bubble(2050, "cropland_area_change", "Cropland change", "Agricultural reduction rate vs cropland change"), width = 8, height = 6)
save_plot("figA6i_agmip_ag_rate_bubble_2050_grassland.png", make_agmip_ag_rate_bubble(2050, "grassland_area_change", "Grassland change", "Agricultural reduction rate vs grassland change"), width = 8, height = 6)
save_plot("figA6j_agmip_ag_rate_bubble_2100_calorie.png", make_agmip_ag_rate_bubble(2100, "calorie_change", "Calorie change", "Agricultural reduction rate vs calorie change"), width = 8, height = 6)
save_plot("figA6k_agmip_ag_rate_bubble_2100_agr_price.png", make_agmip_ag_rate_bubble(2100, "agr_price_change", "AGR price change", "Agricultural reduction rate vs AGR price change"), width = 8, height = 6)
save_plot("figA6l_agmip_ag_rate_bubble_2100_lsp_prod.png", make_agmip_ag_rate_bubble(2100, "lsp_prod_change", "LSP production change", "Agricultural reduction rate vs livestock production change"), width = 8, height = 6)
save_plot("figA6m_agmip_ag_rate_bubble_2100_crp_prod.png", make_agmip_ag_rate_bubble(2100, "crp_prod_change", "CRP production change", "Agricultural reduction rate vs crop production change"), width = 8, height = 6)
save_plot("figA6n_agmip_ag_rate_bubble_2100_forest.png", make_agmip_ag_rate_bubble(2100, "forest_area_change", "Forest area change", "Agricultural reduction rate vs forest area change"), width = 8, height = 6)
save_plot("figA6o_agmip_ag_rate_bubble_2100_cropland.png", make_agmip_ag_rate_bubble(2100, "cropland_area_change", "Cropland change", "Agricultural reduction rate vs cropland change"), width = 8, height = 6)
save_plot("figA6p_agmip_ag_rate_bubble_2100_grassland.png", make_agmip_ag_rate_bubble(2100, "grassland_area_change", "Grassland change", "Agricultural reduction rate vs grassland change"), width = 8, height = 6)

agmip_source_heatmap <- agmip_emis_actual_reduction %>%
  filter(year == 2100, item != "tot", region %in% (top_potential_country %>% filter(year == 2100) %>% arrange(desc(total_potential)) %>% slice_head(n = 15) %>% pull(region))) %>%
  left_join(top_potential_country %>% filter(year == 2100) %>% select(region, country, total_potential), by = "region") %>%
  mutate(country = factor(country, levels = rev((top_potential_country %>% filter(year == 2100) %>% arrange(desc(total_potential)) %>% slice_head(n = 15) %>% pull(country)))))

p_agmip_source_heatmap <- ggplot(agmip_source_heatmap, aes(x = source, y = country, fill = actual_reduction)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "#4477AA", mid = "white", high = "#CC6677", midpoint = 0) +
  labs(title = "AGMIP actual reduction heatmap in top-potential countries (2100)", x = NULL, y = NULL) +
  theme_main +
  theme(axis.text.x = element_text(angle = 35, hjust = 1))

save_plot("figA7_agmip_actual_reduction_heatmap_2100.png", p_agmip_source_heatmap, width = 12, height = 7)

agmip_r5_source <- agmip_emis_actual_reduction %>%
  filter(year == 2100, item != "tot", region %in% names(r5_labels), region != "World") %>%
  mutate(region_name = recode(region, !!!r5_labels)) %>%
  group_by(region_name, source) %>%
  summarise(actual_reduction = sum(actual_reduction, na.rm = TRUE), .groups = "drop") %>%
  group_by(region_name) %>%
  mutate(total = sum(actual_reduction, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(region_name = fct_reorder(region_name, total))

p_agmip_r5_source <- ggplot(agmip_r5_source, aes(x = actual_reduction, y = region_name, fill = source)) +
  geom_col() +
  labs(title = "AGMIP actual reduction by source in R5 (2100)", x = "MtCO2e", y = NULL) +
  theme_main

save_plot("figA8_agmip_actual_reduction_r5_sources_2100.png", p_agmip_r5_source, width = 12, height = 6)

# appendix AGMIP sector bubbles -------------------------------------------
make_agmip_sector_bubble <- function(plot_year, sector_code, x_mode = c("prod", "calorie", "price_index", "land")) {
  x_mode <- match.arg(x_mode)
  sector_label <- if_else(sector_code == "LSP", "LSP", "CRP")

  df <- agmip_sector_reduction %>%
    filter(year == plot_year, sector == sector_code) %>%
    left_join(region_map, by = "region") %>%
    left_join(st_drop_geometry(Boundary), by = c("region" = "cty")) %>%
    mutate(country = coalesce(country_name, region), region5 = factor(region5, levels = names(region_palette)))

  x_df <- switch(
    x_mode,
    prod = agmip_core_change %>% filter(year == plot_year, metric == paste0(sector_label, " production")) %>% transmute(region, year, x = change),
    calorie = agmip_core_change %>% filter(year == plot_year, metric == "Calorie") %>% transmute(region, year, x = change),
    price_index = agmip_price_index_change %>% filter(year == plot_year, sector == sector_code) %>% transmute(region, year, x = price_index_change_pct),
    land = agmip_land_long %>% filter(year == plot_year, land_item == if_else(sector_code == "LSP", "Grassland", "Cropland")) %>% transmute(region, year, x = land_change)
  )

  x_label <- switch(
    x_mode,
    prod = paste0(sector_label, " production change (1000 t)"),
    calorie = "Calorie change (kcal/cap/d)",
    price_index = paste0(sector_label, " producer price change (%)"),
    land = if_else(sector_code == "LSP", "Grassland change (1000 ha)", "Cropland change (1000 ha)")
  )

  title_text <- switch(
    x_mode,
    prod = paste0(sector_label, " reduction vs production change"),
    calorie = paste0(sector_label, " reduction vs calorie change"),
    price_index = paste0(sector_label, " reduction vs producer price change"),
    land = paste0(sector_label, " reduction vs land change")
  )

  bubble_df <- df %>%
    left_join(x_df, by = c("region", "year")) %>%
    mutate(
      size_bin = factor(ntile(reduction, 5), levels = 1:5, labels = c("Q1", "Q2", "Q3", "Q4", "Q5"))
    ) %>%
    filter(is.finite(reduction), is.finite(x), !is.na(region5))

  label_df <- bubble_df %>% filter(size_bin %in% c("Q4", "Q5"))

  ggplot(bubble_df, aes(x = x, y = reduction)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
    geom_point(aes(size = size_bin, fill = region5, color = region5, alpha = size_bin), shape = 21, stroke = 0.35) +
    geom_text(data = label_df, aes(label = country, color = region5), size = 3.1, fontface = "bold", nudge_y = 0.03 * max(bubble_df$reduction, na.rm = TRUE), show.legend = FALSE) +
    scale_fill_manual(values = region_palette) +
    scale_color_manual(values = region_border_palette) +
    scale_size_manual(values = c(2.8, 4.2, 5.6, 7, 8.4), drop = FALSE) +
    scale_alpha_manual(values = c("Q1" = 0.56, "Q2" = 0.62, "Q3" = 0.68, "Q4" = 0.74, "Q5" = 0.80), guide = "none", drop = FALSE) +
    labs(title = paste0(title_text, " (", plot_year, ")"), x = x_label, y = paste0(sector_label, " reduction (MtCO2e)")) +
    theme_main
}

save_plot("figA9_agmip_lsp_reduction_vs_prod_2050_2100.png", wrap_plots(make_agmip_sector_bubble(2050, "LSP", "prod"), make_agmip_sector_bubble(2100, "LSP", "prod"), ncol = 2), width = 12, height = 6)
save_plot("figA10_agmip_crp_reduction_vs_prod_2050_2100.png", wrap_plots(make_agmip_sector_bubble(2050, "CRP", "prod"), make_agmip_sector_bubble(2100, "CRP", "prod"), ncol = 2), width = 12, height = 6)
save_plot("figA11_agmip_lsp_reduction_vs_calorie_2050_2100.png", wrap_plots(make_agmip_sector_bubble(2050, "LSP", "calorie"), make_agmip_sector_bubble(2100, "LSP", "calorie"), ncol = 2), width = 12, height = 6)
save_plot("figA12_agmip_crp_reduction_vs_calorie_2050_2100.png", wrap_plots(make_agmip_sector_bubble(2050, "CRP", "calorie"), make_agmip_sector_bubble(2100, "CRP", "calorie"), ncol = 2), width = 12, height = 6)
save_plot("figA13_agmip_lsp_reduction_vs_price_index_2050_2100.png", wrap_plots(make_agmip_sector_bubble(2050, "LSP", "price_index"), make_agmip_sector_bubble(2100, "LSP", "price_index"), ncol = 2), width = 12, height = 6)
save_plot("figA14_agmip_crp_reduction_vs_price_index_2050_2100.png", wrap_plots(make_agmip_sector_bubble(2050, "CRP", "price_index"), make_agmip_sector_bubble(2100, "CRP", "price_index"), ncol = 2), width = 12, height = 6)
save_plot("figA15_agmip_lsp_reduction_vs_grassland_2050_2100.png", wrap_plots(make_agmip_sector_bubble(2050, "LSP", "land"), make_agmip_sector_bubble(2100, "LSP", "land"), ncol = 2), width = 12, height = 6)
save_plot("figA16_agmip_crp_reduction_vs_cropland_2050_2100.png", wrap_plots(make_agmip_sector_bubble(2050, "CRP", "land"), make_agmip_sector_bubble(2100, "CRP", "land"), ncol = 2), width = 12, height = 6)

# appendix decomposition from all_scenario_World --------------------------
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
  filter(
    region %in% country_codes,
    commodity != "ecr",
    gas %in% c("CH4", "N2O")
  )

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
    total_change_kt = .data[[scenario_landint]] - .data[[scenario_baseline]],
    total_change_mt = total_change_kt / 1000
  ) %>%
  select(region, year, commodity, source, gas, total_change_kt, total_change_mt, bau_ghg_kt = all_of(scenario_baseline))

world_decomp_country_gas <- world_qsup_delta %>%
  full_join(world_ef_delta, by = c("region", "year", "commodity", "source", "gas")) %>%
  mutate(
    ghg_qsup_kt = qsup_delta * ef_ave,
    ghg_ef_kt = qsup_ave * ef_delta
  ) %>%
  full_join(world_ghg_total, by = c("region", "year", "commodity", "source", "gas")) %>%
  group_by(region, year, gas) %>%
  summarise(
    bau_ghg_kt = sum(bau_ghg_kt, na.rm = TRUE),
    ghg_qsup_kt = sum(ghg_qsup_kt, na.rm = TRUE),
    ghg_ef_kt = sum(ghg_ef_kt, na.rm = TRUE),
    total_change_kt = sum(total_change_kt, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    production_change_mt = ghg_qsup_kt / 1000,
    reduction_measures_mt = ghg_ef_kt / 1000,
    total_change_mt = total_change_kt / 1000,
    production_change_pct = if_else(bau_ghg_kt != 0, 100 * ghg_qsup_kt / bau_ghg_kt, NA_real_),
    reduction_measures_pct = if_else(bau_ghg_kt != 0, 100 * ghg_ef_kt / bau_ghg_kt, NA_real_),
    total_change_pct = if_else(bau_ghg_kt != 0, 100 * total_change_kt / bau_ghg_kt, NA_real_)
  ) %>%
  left_join(st_drop_geometry(Boundary), by = c("region" = "cty")) %>%
  mutate(country = coalesce(country_name, region))

world_decomp_country_total <- world_decomp_country_gas %>%
  group_by(region, year, country) %>%
  summarise(total_change_mt = sum(total_change_mt, na.rm = TRUE), .groups = "drop")

world_decomp_country_co2eq <- world_decomp_country_gas %>%
  group_by(region, year, country) %>%
  summarise(
    bau_ghg_kt = sum(bau_ghg_kt, na.rm = TRUE),
    production_change_mt = sum(production_change_mt, na.rm = TRUE),
    reduction_measures_mt = sum(reduction_measures_mt, na.rm = TRUE),
    total_change_mt = sum(total_change_mt, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    production_change_pct = if_else(bau_ghg_kt != 0, 100 * production_change_mt * 1000 / bau_ghg_kt, NA_real_),
    reduction_measures_pct = if_else(bau_ghg_kt != 0, 100 * reduction_measures_mt * 1000 / bau_ghg_kt, NA_real_),
    total_change_pct = if_else(bau_ghg_kt != 0, 100 * total_change_mt * 1000 / bau_ghg_kt, NA_real_)
  )

make_world_decomp_plot <- function(plot_year, value_mode = c("pct", "mt")) {
  value_mode <- match.arg(value_mode)

  top_regions <- agmip_ag_reduction_rank %>%
    filter(year == plot_year) %>%
    arrange(desc(ag_reduction)) %>%
    slice_head(n = 15) %>%
    pull(region)

  country_order <- agmip_ag_reduction_rank %>%
    filter(year == plot_year, region %in% top_regions) %>%
    arrange(desc(ag_reduction)) %>%
    pull(country)

  prod_col <- if (value_mode == "pct") "production_change_pct" else "production_change_mt"
  ef_col <- if (value_mode == "pct") "reduction_measures_pct" else "reduction_measures_mt"
  total_col <- if (value_mode == "pct") "total_change_pct" else "total_change_mt"

  decomp_df <- world_decomp_country_gas %>%
    filter(year == plot_year, region %in% top_regions) %>%
    transmute(
      region,
      country = factor(country, levels = rev(country_order)),
      gas = factor(gas, levels = c("N2O", "CH4")),
      `Production Change` = .data[[prod_col]],
      `Reduction Measures` = .data[[ef_col]],
      total_change = .data[[total_col]]
    ) %>%
    pivot_longer(cols = c(`Reduction Measures`, `Production Change`), names_to = "change", values_to = "value") %>%
    mutate(change = factor(change, levels = c("Reduction Measures", "Production Change")))

  point_df <- world_decomp_country_gas %>%
    filter(year == plot_year, region %in% top_regions) %>%
    transmute(
      country = factor(country, levels = rev(country_order)),
      gas = factor(gas, levels = c("N2O", "CH4")),
      total_change = .data[[total_col]]
    )

  y_label <- if_else(value_mode == "pct", "Emission change from BaU (%)", "Emission change from BaU (MtCO2eq/yr)")
  title_text <- if_else(value_mode == "pct", "Decomposition of emission change", "Decomposition of emission change (absolute)")

  ggplot() +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
    geom_col(data = decomp_df, aes(x = value, y = country, fill = change)) +
    geom_point(data = point_df, aes(x = total_change, y = country), size = 2.4, shape = 21, stroke = 0.4, fill = "black", color = "black") +
    scale_fill_manual(values = c("Reduction Measures" = "#E3819C", "Production Change" = "#A0EBD0")) +
    facet_wrap(~gas, scales = "free_x") +
    labs(title = paste0(title_text, " in top agricultural reduction countries (", plot_year, ")"), x = y_label, y = NULL) +
    theme_main
}

save_plot(
  "figA17_world_decomposition_top_countries_pct_2050_2100.png",
  wrap_plots(make_world_decomp_plot(2050, "pct"), make_world_decomp_plot(2100, "pct"), ncol = 1),
  width = 11,
  height = 11
)

save_plot(
  "figA18_world_decomposition_top_countries_mt_2050_2100.png",
  wrap_plots(make_world_decomp_plot(2050, "mt"), make_world_decomp_plot(2100, "mt"), ncol = 1),
  width = 11,
  height = 11
)

make_world_decomp_total_plot <- function(plot_year, value_mode = c("pct", "mt")) {
  value_mode <- match.arg(value_mode)

  top_regions <- agmip_ag_reduction_rank %>%
    filter(year == plot_year) %>%
    arrange(desc(ag_reduction)) %>%
    slice_head(n = 15) %>%
    pull(region)

  country_order <- agmip_ag_reduction_rank %>%
    filter(year == plot_year, region %in% top_regions) %>%
    arrange(desc(ag_reduction)) %>%
    pull(country)

  prod_col <- if (value_mode == "pct") "production_change_pct" else "production_change_mt"
  ef_col <- if (value_mode == "pct") "reduction_measures_pct" else "reduction_measures_mt"
  total_col <- if (value_mode == "pct") "total_change_pct" else "total_change_mt"
  x_label <- if_else(value_mode == "pct", "Emission change from BaU (%)", "Emission change from BaU (MtCO2eq/yr)")
  title_text <- if_else(value_mode == "pct", "Decomposition of emission change (CH4 + N2O)", "Decomposition of emission change (CH4 + N2O, absolute)")

  decomp_df <- world_decomp_country_co2eq %>%
    filter(year == plot_year, region %in% top_regions) %>%
    transmute(
      country = factor(country, levels = rev(country_order)),
      `Production Change` = .data[[prod_col]],
      `Reduction Measures` = .data[[ef_col]]
    ) %>%
    pivot_longer(cols = c(`Reduction Measures`, `Production Change`), names_to = "change", values_to = "value") %>%
    mutate(change = factor(change, levels = c("Reduction Measures", "Production Change")))

  point_df <- world_decomp_country_co2eq %>%
    filter(year == plot_year, region %in% top_regions) %>%
    transmute(
      country = factor(country, levels = rev(country_order)),
      total_change = .data[[total_col]]
    )

  ggplot() +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
    geom_col(data = decomp_df, aes(x = value, y = country, fill = change)) +
    geom_point(data = point_df, aes(x = total_change, y = country), size = 2.4, shape = 21, stroke = 0.4, fill = "black", color = "black") +
    scale_fill_manual(values = c("Reduction Measures" = "#E3819C", "Production Change" = "#A0EBD0")) +
    labs(title = paste0(title_text, " in top agricultural reduction countries (", plot_year, ")"), x = x_label, y = NULL) +
    theme_main
}

save_plot(
  "figA17c_world_decomposition_top_countries_pct_total_2050_2100.png",
  wrap_plots(make_world_decomp_total_plot(2050, "pct"), make_world_decomp_total_plot(2100, "pct"), ncol = 1),
  width = 9,
  height = 11
)

save_plot(
  "figA18c_world_decomposition_top_countries_mt_total_2050_2100.png",
  wrap_plots(make_world_decomp_total_plot(2050, "mt"), make_world_decomp_total_plot(2100, "mt"), ncol = 1),
  width = 9,
  height = 11
)

world_decomp_r5_gas <- world_decomp_country_gas %>%
  left_join(region_map %>% select(region, region5), by = "region") %>%
  filter(!is.na(region5)) %>%
  group_by(region5, year, gas) %>%
  summarise(
    bau_ghg_kt = sum(bau_ghg_kt, na.rm = TRUE),
    production_change_mt = sum(production_change_mt, na.rm = TRUE),
    reduction_measures_mt = sum(reduction_measures_mt, na.rm = TRUE),
    total_change_mt = sum(total_change_mt, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    production_change_pct = if_else(bau_ghg_kt != 0, 100 * production_change_mt * 1000 / bau_ghg_kt, NA_real_),
    reduction_measures_pct = if_else(bau_ghg_kt != 0, 100 * reduction_measures_mt * 1000 / bau_ghg_kt, NA_real_),
    total_change_pct = if_else(bau_ghg_kt != 0, 100 * total_change_mt * 1000 / bau_ghg_kt, NA_real_),
    region_name = factor(recode(region5, !!!r5_labels), levels = rev(c("World", "OECD+EU", "Asia", "Africa/Middle East", "Latin America", "Former Soviet Union")))
  )

world_decomp_world_gas <- world_decomp_country_gas %>%
  group_by(year, gas) %>%
  summarise(
    bau_ghg_kt = sum(bau_ghg_kt, na.rm = TRUE),
    production_change_mt = sum(production_change_mt, na.rm = TRUE),
    reduction_measures_mt = sum(reduction_measures_mt, na.rm = TRUE),
    total_change_mt = sum(total_change_mt, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    production_change_pct = if_else(bau_ghg_kt != 0, 100 * production_change_mt * 1000 / bau_ghg_kt, NA_real_),
    reduction_measures_pct = if_else(bau_ghg_kt != 0, 100 * reduction_measures_mt * 1000 / bau_ghg_kt, NA_real_),
    total_change_pct = if_else(bau_ghg_kt != 0, 100 * total_change_mt * 1000 / bau_ghg_kt, NA_real_),
    region_name = factor("World", levels = rev(c("World", "OECD+EU", "Asia", "Africa/Middle East", "Latin America", "Former Soviet Union")))
  )

world_decomp_r5_world_gas <- bind_rows(world_decomp_r5_gas, world_decomp_world_gas)

world_decomp_r5_world_co2eq <- world_decomp_r5_world_gas %>%
  group_by(region_name, year) %>%
  summarise(
    bau_ghg_kt = sum(bau_ghg_kt, na.rm = TRUE),
    production_change_mt = sum(production_change_mt, na.rm = TRUE),
    reduction_measures_mt = sum(reduction_measures_mt, na.rm = TRUE),
    total_change_mt = sum(total_change_mt, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    production_change_pct = if_else(bau_ghg_kt != 0, 100 * production_change_mt * 1000 / bau_ghg_kt, NA_real_),
    reduction_measures_pct = if_else(bau_ghg_kt != 0, 100 * reduction_measures_mt * 1000 / bau_ghg_kt, NA_real_),
    total_change_pct = if_else(bau_ghg_kt != 0, 100 * total_change_mt * 1000 / bau_ghg_kt, NA_real_)
  )

make_world_decomp_r5_plot <- function(plot_year, value_mode = c("pct", "mt")) {
  value_mode <- match.arg(value_mode)
  prod_col <- if (value_mode == "pct") "production_change_pct" else "production_change_mt"
  ef_col <- if (value_mode == "pct") "reduction_measures_pct" else "reduction_measures_mt"
  total_col <- if (value_mode == "pct") "total_change_pct" else "total_change_mt"

  decomp_df <- world_decomp_r5_world_gas %>%
    filter(year == plot_year) %>%
    transmute(
      region_name,
      gas = factor(gas, levels = c("N2O", "CH4")),
      `Production Change` = .data[[prod_col]],
      `Reduction Measures` = .data[[ef_col]]
    ) %>%
    pivot_longer(cols = c(`Reduction Measures`, `Production Change`), names_to = "change", values_to = "value") %>%
    mutate(change = factor(change, levels = c("Reduction Measures", "Production Change")))

  point_df <- world_decomp_r5_world_gas %>%
    filter(year == plot_year) %>%
    transmute(
      region_name,
      gas = factor(gas, levels = c("N2O", "CH4")),
      total_change = .data[[total_col]]
    )

  x_label <- if (value_mode == "pct") "Emission change from BaU (%)" else "Emission change from BaU (MtCO2eq/yr)"
  title_text <- if (value_mode == "pct") "Decomposition of emission change in World and R5" else "Decomposition of emission change in World and R5 (absolute)"

  ggplot() +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
    geom_col(data = decomp_df, aes(x = value, y = region_name, fill = change)) +
    geom_point(data = point_df, aes(x = total_change, y = region_name), size = 2.4, shape = 21, stroke = 0.4, fill = "black", color = "black") +
    scale_fill_manual(values = c("Reduction Measures" = "#E3819C", "Production Change" = "#A0EBD0")) +
    facet_wrap(~gas, scales = "free_x") +
    labs(title = paste0(title_text, " (", plot_year, ")"), x = x_label, y = NULL) +
    theme_main
}

save_plot(
  "figA17b_world_decomposition_world_r5_pct_2050_2100.png",
  wrap_plots(make_world_decomp_r5_plot(2050, "pct"), make_world_decomp_r5_plot(2100, "pct"), ncol = 1),
  width = 11,
  height = 9
)

save_plot(
  "figA18b_world_decomposition_world_r5_mt_2050_2100.png",
  wrap_plots(make_world_decomp_r5_plot(2050, "mt"), make_world_decomp_r5_plot(2100, "mt"), ncol = 1),
  width = 11,
  height = 9
)

make_world_decomp_r5_total_plot <- function(plot_year, value_mode = c("pct", "mt")) {
  value_mode <- match.arg(value_mode)
  prod_col <- if (value_mode == "pct") "production_change_pct" else "production_change_mt"
  ef_col <- if (value_mode == "pct") "reduction_measures_pct" else "reduction_measures_mt"
  total_col <- if (value_mode == "pct") "total_change_pct" else "total_change_mt"
  x_label <- if (value_mode == "pct") "Emission change from BaU (%)" else "Emission change from BaU (MtCO2eq/yr)"
  title_text <- if_else(value_mode == "pct", "Decomposition of emission change in World and R5 (CH4 + N2O)", "Decomposition of emission change in World and R5 (CH4 + N2O, absolute)")

  decomp_df <- world_decomp_r5_world_co2eq %>%
    filter(year == plot_year) %>%
    transmute(
      region_name,
      `Production Change` = .data[[prod_col]],
      `Reduction Measures` = .data[[ef_col]]
    ) %>%
    pivot_longer(cols = c(`Reduction Measures`, `Production Change`), names_to = "change", values_to = "value") %>%
    mutate(change = factor(change, levels = c("Reduction Measures", "Production Change")))

  point_df <- world_decomp_r5_world_co2eq %>%
    filter(year == plot_year) %>%
    transmute(region_name, total_change = .data[[total_col]])

  ggplot() +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
    geom_col(data = decomp_df, aes(x = value, y = region_name, fill = change)) +
    geom_point(data = point_df, aes(x = total_change, y = region_name), size = 2.4, shape = 21, stroke = 0.4, fill = "black", color = "black") +
    scale_fill_manual(values = c("Reduction Measures" = "#E3819C", "Production Change" = "#A0EBD0")) +
    labs(title = paste0(title_text, " (", plot_year, ")"), x = x_label, y = NULL) +
    theme_main
}

save_plot(
  "figA17d_world_decomposition_world_r5_pct_total_2050_2100.png",
  wrap_plots(make_world_decomp_r5_total_plot(2050, "pct"), make_world_decomp_r5_total_plot(2100, "pct"), ncol = 1),
  width = 9,
  height = 9
)

save_plot(
  "figA18d_world_decomposition_world_r5_mt_total_2050_2100.png",
  wrap_plots(make_world_decomp_r5_total_plot(2050, "mt"), make_world_decomp_r5_total_plot(2100, "mt"), ncol = 1),
  width = 9,
  height = 9
)

# appendix wedge decomposition from all_scenario_World --------------------
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

world_qdem_wide <- world_qdem %>%
  pivot_wider(names_from = scenario, values_from = qdem) %>%
  mutate(
    qdem_bau = .data[[scenario_baseline]],
    qdem_landint = .data[[scenario_landint]],
    qdem_ratio = if_else(is.finite(qdem_bau) & qdem_bau != 0, qdem_landint / qdem_bau, NA_real_)
  ) %>%
  select(region, year, commodity, qdem_bau, qdem_landint, qdem_ratio)

world_qsup_wide <- world_qsup %>%
  pivot_wider(names_from = scenario, values_from = qsup) %>%
  mutate(
    qsup_bau = .data[[scenario_baseline]],
    qsup_landint = .data[[scenario_landint]],
    qsup_ratio = if_else(is.finite(qsup_bau) & qsup_bau != 0, qsup_landint / qsup_bau, NA_real_)
  ) %>%
  select(region, year, commodity, qsup_bau, qsup_landint, qsup_ratio)

world_ef_wide <- world_decomp_base %>%
  select(scenario, region, year, commodity, source, gas, ef) %>%
  pivot_wider(names_from = scenario, values_from = ef) %>%
  mutate(
    ef_bau = .data[[scenario_baseline]],
    ef_landint = .data[[scenario_landint]],
    ef_ratio = if_else(is.finite(ef_bau) & ef_bau != 0, ef_landint / ef_bau, NA_real_)
  ) %>%
  select(region, year, commodity, source, gas, ef_bau, ef_landint, ef_ratio)

world_ghg_wide <- world_ghg %>%
  pivot_wider(names_from = scenario, values_from = ghg) %>%
  mutate(
    bau_ghg_kt = .data[[scenario_baseline]],
    landint_ghg_kt = .data[[scenario_landint]],
    total_change_kt = landint_ghg_kt - bau_ghg_kt,
    total_change_mt = total_change_kt / 1000
  ) %>%
  select(region, year, commodity, source, gas, bau_ghg_kt, landint_ghg_kt, total_change_kt, total_change_mt)

world_wedge_country_gas <- world_ghg_wide %>%
  left_join(world_qdem_wide, by = c("region", "year", "commodity")) %>%
  left_join(world_qsup_wide, by = c("region", "year", "commodity")) %>%
  left_join(world_ef_wide, by = c("region", "year", "commodity", "source", "gas")) %>%
  mutate(
    demand_kt = bau_ghg_kt * (((qdem_ratio - 1) + ((qdem_ratio - 1) * ef_ratio)) / 2),
    supply_kt = bau_ghg_kt * (((ef_ratio - 1) + ((ef_ratio - 1) * qsup_ratio)) / 2),
    trade_kt = bau_ghg_kt * ((((qsup_ratio - qdem_ratio)) + ((qsup_ratio - qdem_ratio) * ef_ratio)) / 2)
  ) %>%
  group_by(region, year, gas) %>%
  summarise(
    bau_ghg_kt = sum(bau_ghg_kt, na.rm = TRUE),
    demand_mt = sum(demand_kt, na.rm = TRUE) / 1000,
    supply_mt = sum(supply_kt, na.rm = TRUE) / 1000,
    trade_mt = sum(trade_kt, na.rm = TRUE) / 1000,
    total_change_mt = sum(total_change_mt, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    demand_pct = if_else(bau_ghg_kt != 0, 100 * demand_mt * 1000 / bau_ghg_kt, NA_real_),
    supply_pct = if_else(bau_ghg_kt != 0, 100 * supply_mt * 1000 / bau_ghg_kt, NA_real_),
    trade_pct = if_else(bau_ghg_kt != 0, 100 * trade_mt * 1000 / bau_ghg_kt, NA_real_),
    total_change_pct = if_else(bau_ghg_kt != 0, 100 * total_change_mt * 1000 / bau_ghg_kt, NA_real_)
  ) %>%
  left_join(st_drop_geometry(Boundary), by = c("region" = "cty")) %>%
  mutate(country = coalesce(country_name, region))

world_wedge_country_co2eq <- world_wedge_country_gas %>%
  group_by(region, year, country) %>%
  summarise(
    bau_ghg_kt = sum(bau_ghg_kt, na.rm = TRUE),
    demand_mt = sum(demand_mt, na.rm = TRUE),
    supply_mt = sum(supply_mt, na.rm = TRUE),
    trade_mt = sum(trade_mt, na.rm = TRUE),
    total_change_mt = sum(total_change_mt, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    demand_pct = if_else(bau_ghg_kt != 0, 100 * demand_mt * 1000 / bau_ghg_kt, NA_real_),
    supply_pct = if_else(bau_ghg_kt != 0, 100 * supply_mt * 1000 / bau_ghg_kt, NA_real_),
    trade_pct = if_else(bau_ghg_kt != 0, 100 * trade_mt * 1000 / bau_ghg_kt, NA_real_),
    total_change_pct = if_else(bau_ghg_kt != 0, 100 * total_change_mt * 1000 / bau_ghg_kt, NA_real_)
  )

make_world_wedge_plot <- function(plot_year, value_mode = c("pct", "mt")) {
  value_mode <- match.arg(value_mode)
  top_regions <- agmip_ag_reduction_rank %>%
    filter(year == plot_year) %>%
    arrange(desc(ag_reduction)) %>%
    slice_head(n = 15) %>%
    pull(region)

  country_order <- agmip_ag_reduction_rank %>%
    filter(year == plot_year, region %in% top_regions) %>%
    arrange(desc(ag_reduction)) %>%
    pull(country)

  demand_col <- if (value_mode == "pct") "demand_pct" else "demand_mt"
  supply_col <- if (value_mode == "pct") "supply_pct" else "supply_mt"
  trade_col <- if (value_mode == "pct") "trade_pct" else "trade_mt"
  total_col <- if (value_mode == "pct") "total_change_pct" else "total_change_mt"
  x_label <- if_else(value_mode == "pct", "Emission change from BaU (%)", "Emission change from BaU (MtCO2eq/yr)")
  title_text <- if_else(value_mode == "pct", "Wedge decomposition of emission change", "Wedge decomposition of emission change (absolute)")

  decomp_df <- world_wedge_country_gas %>%
    filter(year == plot_year, region %in% top_regions) %>%
    transmute(
      country = factor(country, levels = rev(country_order)),
      gas = factor(gas, levels = c("N2O", "CH4")),
      `Demand side` = .data[[demand_col]],
      `Supply side` = .data[[supply_col]],
      `International trade` = .data[[trade_col]]
    ) %>%
    pivot_longer(cols = c(`Demand side`, `Supply side`, `International trade`), names_to = "change", values_to = "value") %>%
    mutate(change = factor(change, levels = c("Demand side", "Supply side", "International trade")))

  point_df <- world_wedge_country_gas %>%
    filter(year == plot_year, region %in% top_regions) %>%
    transmute(
      country = factor(country, levels = rev(country_order)),
      gas = factor(gas, levels = c("N2O", "CH4")),
      total_change = .data[[total_col]]
    )

  ggplot() +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
    geom_col(data = decomp_df, aes(x = value, y = country, fill = change)) +
    geom_point(data = point_df, aes(x = total_change, y = country), size = 2.4, shape = 21, stroke = 0.4, fill = "black", color = "black") +
    scale_fill_manual(values = c("Demand side" = "#9BC7F2", "Supply side" = "#E3819C", "International trade" = "#A0EBD0")) +
    facet_wrap(~gas, scales = "free_x") +
    labs(title = paste0(title_text, " in top agricultural reduction countries (", plot_year, ")"), x = x_label, y = NULL) +
    theme_main
}

make_world_wedge_total_plot <- function(plot_year, value_mode = c("pct", "mt")) {
  value_mode <- match.arg(value_mode)
  top_regions <- agmip_ag_reduction_rank %>%
    filter(year == plot_year) %>%
    arrange(desc(ag_reduction)) %>%
    slice_head(n = 15) %>%
    pull(region)

  country_order <- agmip_ag_reduction_rank %>%
    filter(year == plot_year, region %in% top_regions) %>%
    arrange(desc(ag_reduction)) %>%
    pull(country)

  demand_col <- if (value_mode == "pct") "demand_pct" else "demand_mt"
  supply_col <- if (value_mode == "pct") "supply_pct" else "supply_mt"
  trade_col <- if (value_mode == "pct") "trade_pct" else "trade_mt"
  total_col <- if (value_mode == "pct") "total_change_pct" else "total_change_mt"
  x_label <- if_else(value_mode == "pct", "Emission change from BaU (%)", "Emission change from BaU (MtCO2eq/yr)")
  title_text <- if_else(value_mode == "pct", "Wedge decomposition of emission change (CH4 + N2O)", "Wedge decomposition of emission change (CH4 + N2O, absolute)")

  decomp_df <- world_wedge_country_co2eq %>%
    filter(year == plot_year, region %in% top_regions) %>%
    transmute(
      country = factor(country, levels = rev(country_order)),
      `Demand side` = .data[[demand_col]],
      `Supply side` = .data[[supply_col]],
      `International trade` = .data[[trade_col]]
    ) %>%
    pivot_longer(cols = c(`Demand side`, `Supply side`, `International trade`), names_to = "change", values_to = "value") %>%
    mutate(change = factor(change, levels = c("Demand side", "Supply side", "International trade")))

  point_df <- world_wedge_country_co2eq %>%
    filter(year == plot_year, region %in% top_regions) %>%
    transmute(
      country = factor(country, levels = rev(country_order)),
      total_change = .data[[total_col]]
    )

  ggplot() +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
    geom_col(data = decomp_df, aes(x = value, y = country, fill = change)) +
    geom_point(data = point_df, aes(x = total_change, y = country), size = 2.4, shape = 21, stroke = 0.4, fill = "black", color = "black") +
    scale_fill_manual(values = c("Demand side" = "#9BC7F2", "Supply side" = "#E3819C", "International trade" = "#A0EBD0")) +
    labs(title = paste0(title_text, " in top agricultural reduction countries (", plot_year, ")"), x = x_label, y = NULL) +
    theme_main
}

save_plot(
  "figA19_world_wedge_top_countries_pct_2050_2100.png",
  wrap_plots(make_world_wedge_plot(2050, "pct"), make_world_wedge_plot(2100, "pct"), ncol = 1),
  width = 12,
  height = 11
)

save_plot(
  "figA20_world_wedge_top_countries_mt_2050_2100.png",
  wrap_plots(make_world_wedge_plot(2050, "mt"), make_world_wedge_plot(2100, "mt"), ncol = 1),
  width = 12,
  height = 11
)

save_plot(
  "figA21_world_wedge_top_countries_pct_total_2050_2100.png",
  wrap_plots(make_world_wedge_total_plot(2050, "pct"), make_world_wedge_total_plot(2100, "pct"), ncol = 1),
  width = 9,
  height = 11
)

save_plot(
  "figA22_world_wedge_top_countries_mt_total_2050_2100.png",
  wrap_plots(make_world_wedge_total_plot(2050, "mt"), make_world_wedge_total_plot(2100, "mt"), ncol = 1),
  width = 9,
  height = 11
)

world_wedge_r5_gas <- world_wedge_country_gas %>%
  left_join(region_map %>% select(region, region5), by = "region") %>%
  filter(!is.na(region5)) %>%
  group_by(region5, year, gas) %>%
  summarise(
    bau_ghg_kt = sum(bau_ghg_kt, na.rm = TRUE),
    demand_mt = sum(demand_mt, na.rm = TRUE),
    supply_mt = sum(supply_mt, na.rm = TRUE),
    trade_mt = sum(trade_mt, na.rm = TRUE),
    total_change_mt = sum(total_change_mt, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    demand_pct = if_else(bau_ghg_kt != 0, 100 * demand_mt * 1000 / bau_ghg_kt, NA_real_),
    supply_pct = if_else(bau_ghg_kt != 0, 100 * supply_mt * 1000 / bau_ghg_kt, NA_real_),
    trade_pct = if_else(bau_ghg_kt != 0, 100 * trade_mt * 1000 / bau_ghg_kt, NA_real_),
    total_change_pct = if_else(bau_ghg_kt != 0, 100 * total_change_mt * 1000 / bau_ghg_kt, NA_real_),
    region_name = factor(recode(region5, !!!r5_labels), levels = rev(c("World", "OECD+EU", "Asia", "Africa/Middle East", "Latin America", "Former Soviet Union")))
  )

world_wedge_world_gas <- world_wedge_country_gas %>%
  group_by(year, gas) %>%
  summarise(
    bau_ghg_kt = sum(bau_ghg_kt, na.rm = TRUE),
    demand_mt = sum(demand_mt, na.rm = TRUE),
    supply_mt = sum(supply_mt, na.rm = TRUE),
    trade_mt = sum(trade_mt, na.rm = TRUE),
    total_change_mt = sum(total_change_mt, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    demand_pct = if_else(bau_ghg_kt != 0, 100 * demand_mt * 1000 / bau_ghg_kt, NA_real_),
    supply_pct = if_else(bau_ghg_kt != 0, 100 * supply_mt * 1000 / bau_ghg_kt, NA_real_),
    trade_pct = if_else(bau_ghg_kt != 0, 100 * trade_mt * 1000 / bau_ghg_kt, NA_real_),
    total_change_pct = if_else(bau_ghg_kt != 0, 100 * total_change_mt * 1000 / bau_ghg_kt, NA_real_),
    region_name = factor("World", levels = rev(c("World", "OECD+EU", "Asia", "Africa/Middle East", "Latin America", "Former Soviet Union")))
  )

world_wedge_r5_world_gas <- bind_rows(world_wedge_r5_gas, world_wedge_world_gas)

world_wedge_r5_world_co2eq <- world_wedge_r5_world_gas %>%
  group_by(region_name, year) %>%
  summarise(
    bau_ghg_kt = sum(bau_ghg_kt, na.rm = TRUE),
    demand_mt = sum(demand_mt, na.rm = TRUE),
    supply_mt = sum(supply_mt, na.rm = TRUE),
    trade_mt = sum(trade_mt, na.rm = TRUE),
    total_change_mt = sum(total_change_mt, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    demand_pct = if_else(bau_ghg_kt != 0, 100 * demand_mt * 1000 / bau_ghg_kt, NA_real_),
    supply_pct = if_else(bau_ghg_kt != 0, 100 * supply_mt * 1000 / bau_ghg_kt, NA_real_),
    trade_pct = if_else(bau_ghg_kt != 0, 100 * trade_mt * 1000 / bau_ghg_kt, NA_real_),
    total_change_pct = if_else(bau_ghg_kt != 0, 100 * total_change_mt * 1000 / bau_ghg_kt, NA_real_)
  )

make_world_wedge_r5_plot <- function(plot_year, value_mode = c("pct", "mt")) {
  value_mode <- match.arg(value_mode)
  demand_col <- if (value_mode == "pct") "demand_pct" else "demand_mt"
  supply_col <- if (value_mode == "pct") "supply_pct" else "supply_mt"
  trade_col <- if (value_mode == "pct") "trade_pct" else "trade_mt"
  total_col <- if (value_mode == "pct") "total_change_pct" else "total_change_mt"
  x_label <- if_else(value_mode == "pct", "Emission change from BaU (%)", "Emission change from BaU (MtCO2eq/yr)")
  title_text <- if_else(value_mode == "pct", "Wedge decomposition in World and R5", "Wedge decomposition in World and R5 (absolute)")

  decomp_df <- world_wedge_r5_world_gas %>%
    filter(year == plot_year) %>%
    transmute(
      region_name,
      gas = factor(gas, levels = c("N2O", "CH4")),
      `Demand side` = .data[[demand_col]],
      `Supply side` = .data[[supply_col]],
      `International trade` = .data[[trade_col]]
    ) %>%
    pivot_longer(cols = c(`Demand side`, `Supply side`, `International trade`), names_to = "change", values_to = "value") %>%
    mutate(change = factor(change, levels = c("Demand side", "Supply side", "International trade")))

  point_df <- world_wedge_r5_world_gas %>%
    filter(year == plot_year) %>%
    transmute(
      region_name,
      gas = factor(gas, levels = c("N2O", "CH4")),
      total_change = .data[[total_col]]
    )

  ggplot() +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
    geom_col(data = decomp_df, aes(x = value, y = region_name, fill = change)) +
    geom_point(data = point_df, aes(x = total_change, y = region_name), size = 2.4, shape = 21, stroke = 0.4, fill = "black", color = "black") +
    scale_fill_manual(values = c("Demand side" = "#9BC7F2", "Supply side" = "#E3819C", "International trade" = "#A0EBD0")) +
    facet_wrap(~gas, scales = "free_x") +
    labs(title = paste0(title_text, " (", plot_year, ")"), x = x_label, y = NULL) +
    theme_main
}

make_world_wedge_r5_total_plot <- function(plot_year, value_mode = c("pct", "mt")) {
  value_mode <- match.arg(value_mode)
  demand_col <- if (value_mode == "pct") "demand_pct" else "demand_mt"
  supply_col <- if (value_mode == "pct") "supply_pct" else "supply_mt"
  trade_col <- if (value_mode == "pct") "trade_pct" else "trade_mt"
  total_col <- if (value_mode == "pct") "total_change_pct" else "total_change_mt"
  x_label <- if_else(value_mode == "pct", "Emission change from BaU (%)", "Emission change from BaU (MtCO2eq/yr)")
  title_text <- if_else(value_mode == "pct", "Wedge decomposition in World and R5 (CH4 + N2O)", "Wedge decomposition in World and R5 (CH4 + N2O, absolute)")

  decomp_df <- world_wedge_r5_world_co2eq %>%
    filter(year == plot_year) %>%
    transmute(
      region_name,
      `Demand side` = .data[[demand_col]],
      `Supply side` = .data[[supply_col]],
      `International trade` = .data[[trade_col]]
    ) %>%
    pivot_longer(cols = c(`Demand side`, `Supply side`, `International trade`), names_to = "change", values_to = "value") %>%
    mutate(change = factor(change, levels = c("Demand side", "Supply side", "International trade")))

  point_df <- world_wedge_r5_world_co2eq %>%
    filter(year == plot_year) %>%
    transmute(region_name, total_change = .data[[total_col]])

  ggplot() +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
    geom_col(data = decomp_df, aes(x = value, y = region_name, fill = change)) +
    geom_point(data = point_df, aes(x = total_change, y = region_name), size = 2.4, shape = 21, stroke = 0.4, fill = "black", color = "black") +
    scale_fill_manual(values = c("Demand side" = "#9BC7F2", "Supply side" = "#E3819C", "International trade" = "#A0EBD0")) +
    labs(title = paste0(title_text, " (", plot_year, ")"), x = x_label, y = NULL) +
    theme_main
}

save_plot(
  "figA23_world_wedge_world_r5_pct_2050_2100.png",
  wrap_plots(make_world_wedge_r5_plot(2050, "pct"), make_world_wedge_r5_plot(2100, "pct"), ncol = 1),
  width = 12,
  height = 9
)

save_plot(
  "figA24_world_wedge_world_r5_mt_2050_2100.png",
  wrap_plots(make_world_wedge_r5_plot(2050, "mt"), make_world_wedge_r5_plot(2100, "mt"), ncol = 1),
  width = 12,
  height = 9
)

save_plot(
  "figA25_world_wedge_world_r5_pct_total_2050_2100.png",
  wrap_plots(make_world_wedge_r5_total_plot(2050, "pct"), make_world_wedge_r5_total_plot(2100, "pct"), ncol = 1),
  width = 9,
  height = 9
)

save_plot(
  "figA26_world_wedge_world_r5_mt_total_2050_2100.png",
  wrap_plots(make_world_wedge_r5_total_plot(2050, "mt"), make_world_wedge_r5_total_plot(2100, "mt"), ncol = 1),
  width = 9,
  height = 9
)

# appendix final wedges using RR as technical effect ----------------------
world_final_wedge_country_gas <- world_ghg_wide %>%
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
    demand_kt = bau_ghg_kt * (((qdem_ratio - 1) + ((qdem_ratio - 1) * ef_ratio)) / 2),
    supply_kt = bau_ghg_kt * (((ef_ratio - 1) + ((ef_ratio - 1) * qsup_ratio)) / 2),
    trade_kt = bau_ghg_kt * ((((qsup_ratio - qdem_ratio)) + ((qsup_ratio - qdem_ratio) * ef_ratio)) / 2),
    technical_kt = bau_ghg_kt * (((-rr_tech) + ((-rr_tech) * qsup_ratio)) / 2),
    structural_kt = (supply_kt - technical_kt) + trade_kt
  ) %>%
  group_by(region, year, gas) %>%
  summarise(
    bau_ghg_kt = sum(bau_ghg_kt, na.rm = TRUE),
    demand_mt = sum(demand_kt, na.rm = TRUE) / 1000,
    technical_mt = sum(technical_kt, na.rm = TRUE) / 1000,
    structural_mt = sum(structural_kt, na.rm = TRUE) / 1000,
    total_change_mt = sum(total_change_mt, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    demand_pct = if_else(bau_ghg_kt != 0, 100 * demand_mt * 1000 / bau_ghg_kt, NA_real_),
    technical_pct = if_else(bau_ghg_kt != 0, 100 * technical_mt * 1000 / bau_ghg_kt, NA_real_),
    structural_pct = if_else(bau_ghg_kt != 0, 100 * structural_mt * 1000 / bau_ghg_kt, NA_real_),
    total_change_pct = if_else(bau_ghg_kt != 0, 100 * total_change_mt * 1000 / bau_ghg_kt, NA_real_)
  ) %>%
  left_join(st_drop_geometry(Boundary), by = c("region" = "cty")) %>%
  mutate(country = coalesce(country_name, region))

world_final_wedge_country_co2eq <- world_final_wedge_country_gas %>%
  group_by(region, year, country) %>%
  summarise(
    bau_ghg_kt = sum(bau_ghg_kt, na.rm = TRUE),
    demand_mt = sum(demand_mt, na.rm = TRUE),
    technical_mt = sum(technical_mt, na.rm = TRUE),
    structural_mt = sum(structural_mt, na.rm = TRUE),
    total_change_mt = sum(total_change_mt, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    demand_pct = if_else(bau_ghg_kt != 0, 100 * demand_mt * 1000 / bau_ghg_kt, NA_real_),
    technical_pct = if_else(bau_ghg_kt != 0, 100 * technical_mt * 1000 / bau_ghg_kt, NA_real_),
    structural_pct = if_else(bau_ghg_kt != 0, 100 * structural_mt * 1000 / bau_ghg_kt, NA_real_),
    total_change_pct = if_else(bau_ghg_kt != 0, 100 * total_change_mt * 1000 / bau_ghg_kt, NA_real_)
  )

make_world_final_wedge_plot <- function(plot_year, value_mode = c("pct", "mt")) {
  value_mode <- match.arg(value_mode)
  top_regions <- agmip_ag_reduction_rank %>%
    filter(year == plot_year) %>%
    arrange(desc(ag_reduction)) %>%
    slice_head(n = 15) %>%
    pull(region)

  country_order <- agmip_ag_reduction_rank %>%
    filter(year == plot_year, region %in% top_regions) %>%
    arrange(desc(ag_reduction)) %>%
    pull(country)

  demand_col <- if (value_mode == "pct") "demand_pct" else "demand_mt"
  technical_col <- if (value_mode == "pct") "technical_pct" else "technical_mt"
  structural_col <- if (value_mode == "pct") "structural_pct" else "structural_mt"
  total_col <- if (value_mode == "pct") "total_change_pct" else "total_change_mt"
  x_label <- if_else(value_mode == "pct", "Emission change from BaU (%)", "Emission change from BaU (MtCO2eq/yr)")
  title_text <- if_else(value_mode == "pct", "Final wedge decomposition", "Final wedge decomposition (absolute)")

  decomp_df <- world_final_wedge_country_gas %>%
    filter(year == plot_year, region %in% top_regions) %>%
    transmute(
      country = factor(country, levels = rev(country_order)),
      gas = factor(gas, levels = c("N2O", "CH4")),
      `Demand side` = .data[[demand_col]],
      `Technical` = .data[[technical_col]],
      `Structural` = .data[[structural_col]]
    ) %>%
    pivot_longer(cols = c(`Demand side`, `Technical`, `Structural`), names_to = "change", values_to = "value") %>%
    mutate(change = factor(change, levels = c("Demand side", "Technical", "Structural")))

  point_df <- world_final_wedge_country_gas %>%
    filter(year == plot_year, region %in% top_regions) %>%
    transmute(
      country = factor(country, levels = rev(country_order)),
      gas = factor(gas, levels = c("N2O", "CH4")),
      total_change = .data[[total_col]]
    )

  ggplot() +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
    geom_col(data = decomp_df, aes(x = value, y = country, fill = change)) +
    geom_point(data = point_df, aes(x = total_change, y = country), size = 2.4, shape = 21, stroke = 0.4, fill = "black", color = "black") +
    scale_fill_manual(values = c("Demand side" = "#9BC7F2", "Technical" = "#E3819C", "Structural" = "#A0EBD0")) +
    facet_wrap(~gas, scales = "free_x") +
    labs(title = paste0(title_text, " in top agricultural reduction countries (", plot_year, ")"), x = x_label, y = NULL) +
    theme_main
}

make_world_final_wedge_total_plot <- function(plot_year, value_mode = c("pct", "mt")) {
  value_mode <- match.arg(value_mode)
  top_regions <- agmip_ag_reduction_rank %>%
    filter(year == plot_year) %>%
    arrange(desc(ag_reduction)) %>%
    slice_head(n = 15) %>%
    pull(region)

  country_order <- agmip_ag_reduction_rank %>%
    filter(year == plot_year, region %in% top_regions) %>%
    arrange(desc(ag_reduction)) %>%
    pull(country)

  demand_col <- if (value_mode == "pct") "demand_pct" else "demand_mt"
  technical_col <- if (value_mode == "pct") "technical_pct" else "technical_mt"
  structural_col <- if (value_mode == "pct") "structural_pct" else "structural_mt"
  total_col <- if (value_mode == "pct") "total_change_pct" else "total_change_mt"
  x_label <- if_else(value_mode == "pct", "Emission change from BaU (%)", "Emission change from BaU (MtCO2eq/yr)")
  title_text <- if_else(value_mode == "pct", "Final wedge decomposition (CH4 + N2O)", "Final wedge decomposition (CH4 + N2O, absolute)")

  decomp_df <- world_final_wedge_country_co2eq %>%
    filter(year == plot_year, region %in% top_regions) %>%
    transmute(
      country = factor(country, levels = rev(country_order)),
      `Demand side` = .data[[demand_col]],
      `Technical` = .data[[technical_col]],
      `Structural` = .data[[structural_col]]
    ) %>%
    pivot_longer(cols = c(`Demand side`, `Technical`, `Structural`), names_to = "change", values_to = "value") %>%
    mutate(change = factor(change, levels = c("Demand side", "Technical", "Structural")))

  point_df <- world_final_wedge_country_co2eq %>%
    filter(year == plot_year, region %in% top_regions) %>%
    transmute(
      country = factor(country, levels = rev(country_order)),
      total_change = .data[[total_col]]
    )

  ggplot() +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
    geom_col(data = decomp_df, aes(x = value, y = country, fill = change)) +
    geom_point(data = point_df, aes(x = total_change, y = country), size = 2.4, shape = 21, stroke = 0.4, fill = "black", color = "black") +
    scale_fill_manual(values = c("Demand side" = "#9BC7F2", "Technical" = "#E3819C", "Structural" = "#A0EBD0")) +
    labs(title = paste0(title_text, " in top agricultural reduction countries (", plot_year, ")"), x = x_label, y = NULL) +
    theme_main
}

save_plot(
  "figA27_world_final_wedge_top_countries_pct_2050_2100.png",
  wrap_plots(make_world_final_wedge_plot(2050, "pct"), make_world_final_wedge_plot(2100, "pct"), ncol = 1),
  width = 12,
  height = 11
)

save_plot(
  "figA28_world_final_wedge_top_countries_mt_2050_2100.png",
  wrap_plots(make_world_final_wedge_plot(2050, "mt"), make_world_final_wedge_plot(2100, "mt"), ncol = 1),
  width = 12,
  height = 11
)

save_plot(
  "figA29_world_final_wedge_top_countries_pct_total_2050_2100.png",
  wrap_plots(make_world_final_wedge_total_plot(2050, "pct"), make_world_final_wedge_total_plot(2100, "pct"), ncol = 1),
  width = 9,
  height = 11
)

save_plot(
  "figA30_world_final_wedge_top_countries_mt_total_2050_2100.png",
  wrap_plots(make_world_final_wedge_total_plot(2050, "mt"), make_world_final_wedge_total_plot(2100, "mt"), ncol = 1),
  width = 9,
  height = 11
)

world_final_wedge_r5_gas <- world_final_wedge_country_gas %>%
  left_join(region_map %>% select(region, region5), by = "region") %>%
  filter(!is.na(region5)) %>%
  group_by(region5, year, gas) %>%
  summarise(
    bau_ghg_kt = sum(bau_ghg_kt, na.rm = TRUE),
    demand_mt = sum(demand_mt, na.rm = TRUE),
    technical_mt = sum(technical_mt, na.rm = TRUE),
    structural_mt = sum(structural_mt, na.rm = TRUE),
    total_change_mt = sum(total_change_mt, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    demand_pct = if_else(bau_ghg_kt != 0, 100 * demand_mt * 1000 / bau_ghg_kt, NA_real_),
    technical_pct = if_else(bau_ghg_kt != 0, 100 * technical_mt * 1000 / bau_ghg_kt, NA_real_),
    structural_pct = if_else(bau_ghg_kt != 0, 100 * structural_mt * 1000 / bau_ghg_kt, NA_real_),
    total_change_pct = if_else(bau_ghg_kt != 0, 100 * total_change_mt * 1000 / bau_ghg_kt, NA_real_),
    region_name = factor(recode(region5, !!!r5_labels), levels = rev(c("World", "OECD+EU", "Asia", "Africa/Middle East", "Latin America", "Former Soviet Union")))
  )

world_final_wedge_world_gas <- world_final_wedge_country_gas %>%
  group_by(year, gas) %>%
  summarise(
    bau_ghg_kt = sum(bau_ghg_kt, na.rm = TRUE),
    demand_mt = sum(demand_mt, na.rm = TRUE),
    technical_mt = sum(technical_mt, na.rm = TRUE),
    structural_mt = sum(structural_mt, na.rm = TRUE),
    total_change_mt = sum(total_change_mt, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    demand_pct = if_else(bau_ghg_kt != 0, 100 * demand_mt * 1000 / bau_ghg_kt, NA_real_),
    technical_pct = if_else(bau_ghg_kt != 0, 100 * technical_mt * 1000 / bau_ghg_kt, NA_real_),
    structural_pct = if_else(bau_ghg_kt != 0, 100 * structural_mt * 1000 / bau_ghg_kt, NA_real_),
    total_change_pct = if_else(bau_ghg_kt != 0, 100 * total_change_mt * 1000 / bau_ghg_kt, NA_real_),
    region_name = factor("World", levels = rev(c("World", "OECD+EU", "Asia", "Africa/Middle East", "Latin America", "Former Soviet Union")))
  )

world_final_wedge_r5_world_gas <- bind_rows(world_final_wedge_r5_gas, world_final_wedge_world_gas)

world_final_wedge_r5_world_co2eq <- world_final_wedge_r5_world_gas %>%
  group_by(region_name, year) %>%
  summarise(
    bau_ghg_kt = sum(bau_ghg_kt, na.rm = TRUE),
    demand_mt = sum(demand_mt, na.rm = TRUE),
    technical_mt = sum(technical_mt, na.rm = TRUE),
    structural_mt = sum(structural_mt, na.rm = TRUE),
    total_change_mt = sum(total_change_mt, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    demand_pct = if_else(bau_ghg_kt != 0, 100 * demand_mt * 1000 / bau_ghg_kt, NA_real_),
    technical_pct = if_else(bau_ghg_kt != 0, 100 * technical_mt * 1000 / bau_ghg_kt, NA_real_),
    structural_pct = if_else(bau_ghg_kt != 0, 100 * structural_mt * 1000 / bau_ghg_kt, NA_real_),
    total_change_pct = if_else(bau_ghg_kt != 0, 100 * total_change_mt * 1000 / bau_ghg_kt, NA_real_)
  )

make_world_final_wedge_r5_plot <- function(plot_year, value_mode = c("pct", "mt")) {
  value_mode <- match.arg(value_mode)
  demand_col <- if (value_mode == "pct") "demand_pct" else "demand_mt"
  technical_col <- if (value_mode == "pct") "technical_pct" else "technical_mt"
  structural_col <- if (value_mode == "pct") "structural_pct" else "structural_mt"
  total_col <- if (value_mode == "pct") "total_change_pct" else "total_change_mt"
  x_label <- if_else(value_mode == "pct", "Emission change from BaU (%)", "Emission change from BaU (MtCO2eq/yr)")
  title_text <- if_else(value_mode == "pct", "Final wedge decomposition in World and R5", "Final wedge decomposition in World and R5 (absolute)")

  decomp_df <- world_final_wedge_r5_world_gas %>%
    filter(year == plot_year) %>%
    transmute(
      region_name,
      gas = factor(gas, levels = c("N2O", "CH4")),
      `Demand side` = .data[[demand_col]],
      `Technical` = .data[[technical_col]],
      `Structural` = .data[[structural_col]]
    ) %>%
    pivot_longer(cols = c(`Demand side`, `Technical`, `Structural`), names_to = "change", values_to = "value") %>%
    mutate(change = factor(change, levels = c("Demand side", "Technical", "Structural")))

  point_df <- world_final_wedge_r5_world_gas %>%
    filter(year == plot_year) %>%
    transmute(
      region_name,
      gas = factor(gas, levels = c("N2O", "CH4")),
      total_change = .data[[total_col]]
    )

  ggplot() +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
    geom_col(data = decomp_df, aes(x = value, y = region_name, fill = change)) +
    geom_point(data = point_df, aes(x = total_change, y = region_name), size = 2.4, shape = 21, stroke = 0.4, fill = "black", color = "black") +
    scale_fill_manual(values = c("Demand side" = "#9BC7F2", "Technical" = "#E3819C", "Structural" = "#A0EBD0")) +
    facet_wrap(~gas, scales = "free_x") +
    labs(title = paste0(title_text, " (", plot_year, ")"), x = x_label, y = NULL) +
    theme_main
}

make_world_final_wedge_r5_total_plot <- function(plot_year, value_mode = c("pct", "mt")) {
  value_mode <- match.arg(value_mode)
  demand_col <- if (value_mode == "pct") "demand_pct" else "demand_mt"
  technical_col <- if (value_mode == "pct") "technical_pct" else "technical_mt"
  structural_col <- if (value_mode == "pct") "structural_pct" else "structural_mt"
  total_col <- if (value_mode == "pct") "total_change_pct" else "total_change_mt"
  x_label <- if_else(value_mode == "pct", "Emission change from BaU (%)", "Emission change from BaU (MtCO2eq/yr)")
  title_text <- if_else(value_mode == "pct", "Final wedge decomposition in World and R5 (CH4 + N2O)", "Final wedge decomposition in World and R5 (CH4 + N2O, absolute)")

  decomp_df <- world_final_wedge_r5_world_co2eq %>%
    filter(year == plot_year) %>%
    transmute(
      region_name,
      `Demand side` = .data[[demand_col]],
      `Technical` = .data[[technical_col]],
      `Structural` = .data[[structural_col]]
    ) %>%
    pivot_longer(cols = c(`Demand side`, `Technical`, `Structural`), names_to = "change", values_to = "value") %>%
    mutate(change = factor(change, levels = c("Demand side", "Technical", "Structural")))

  point_df <- world_final_wedge_r5_world_co2eq %>%
    filter(year == plot_year) %>%
    transmute(region_name, total_change = .data[[total_col]])

  ggplot() +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
    geom_col(data = decomp_df, aes(x = value, y = region_name, fill = change)) +
    geom_point(data = point_df, aes(x = total_change, y = region_name), size = 2.4, shape = 21, stroke = 0.4, fill = "black", color = "black") +
    scale_fill_manual(values = c("Demand side" = "#9BC7F2", "Technical" = "#E3819C", "Structural" = "#A0EBD0")) +
    labs(title = paste0(title_text, " (", plot_year, ")"), x = x_label, y = NULL) +
    theme_main
}

save_plot(
  "figA31_world_final_wedge_world_r5_pct_2050_2100.png",
  wrap_plots(make_world_final_wedge_r5_plot(2050, "pct"), make_world_final_wedge_r5_plot(2100, "pct"), ncol = 1),
  width = 12,
  height = 9
)

save_plot(
  "figA32_world_final_wedge_world_r5_mt_2050_2100.png",
  wrap_plots(make_world_final_wedge_r5_plot(2050, "mt"), make_world_final_wedge_r5_plot(2100, "mt"), ncol = 1),
  width = 12,
  height = 9
)

save_plot(
  "figA33_world_final_wedge_world_r5_pct_total_2050_2100.png",
  wrap_plots(make_world_final_wedge_r5_total_plot(2050, "pct"), make_world_final_wedge_r5_total_plot(2100, "pct"), ncol = 1),
  width = 9,
  height = 9
)

save_plot(
  "figA34_world_final_wedge_world_r5_mt_total_2050_2100.png",
  wrap_plots(make_world_final_wedge_r5_total_plot(2050, "mt"), make_world_final_wedge_r5_total_plot(2100, "mt"), ncol = 1),
  width = 9,
  height = 9
)

# appendix: land-side baseline emissions ----------------------------------
land_component_palette <- c(
  "Land-use change" = "#FFCC80",
  "Forest management / harvest / fire" = "#BCAAA4"
)

land_appendix_raw <- GEER_AFOLUB_ALL %>%
  transmute(
    scenario = as.character(Scenario),
    region = as.character(REMF),
    variable = as.character(VEMF),
    year = as.integer(as.character(YEMF)),
    value = as.numeric(IAMC_templatePlus)
  ) %>%
  filter(
    scenario %in% c(scenario_no, scenario_landint),
    year %in% plot_years,
    variable %in% c(
      "Emi_equ_GHG_AFO_Lan",
      "Emi_CO2_Lan_Use_Flo_Pos_Emi_Lan_Use_Cha",
      "Emi_CO2_AFO_Lan_For_Gra_Con",
      "Emi_CO2_Lan_Use_Flo_Pos_Emi_Woo_Har",
      "Emi_CO2_Lan_Use_Flo_Pos_Emi_Fir",
      "Emi_CO2_Lan_Use_Flo_RemForest"
    )
  )

land_total_raw <- land_appendix_raw %>%
  filter(variable == "Emi_equ_GHG_AFO_Lan") %>%
  select(region, year, scenario, value) %>%
  pivot_wider(names_from = scenario, values_from = value) %>%
  mutate(
    baseline_land_emissions = .data[[scenario_no]],
    post_land_emissions = .data[[scenario_landint]],
    land_actual_reduction = post_land_emissions - baseline_land_emissions
  )

land_world_r5_raw <- land_total_raw %>%
  filter(region %in% c("World", names(r5_labels))) %>%
  mutate(region_name = recode(region, !!!r5_labels))

land_country_raw <- land_total_raw %>%
  filter(region %in% country_codes) %>%
  left_join(st_drop_geometry(Boundary), by = c("region" = "cty")) %>%
  mutate(country = coalesce(country_name, region))

land_component_raw <- land_appendix_raw %>%
  filter(region %in% country_codes) %>%
  mutate(component = case_when(
    variable %in% c("Emi_CO2_Lan_Use_Flo_Pos_Emi_Lan_Use_Cha", "Emi_CO2_AFO_Lan_For_Gra_Con") ~ "Land-use change",
    variable %in% c("Emi_CO2_Lan_Use_Flo_Pos_Emi_Woo_Har", "Emi_CO2_Lan_Use_Flo_Pos_Emi_Fir", "Emi_CO2_Lan_Use_Flo_RemForest") ~ "Forest management / harvest / fire",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(component)) %>%
  group_by(region, year, component, scenario) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = scenario, values_from = value) %>%
  mutate(
    baseline_land_emissions = .data[[scenario_no]],
    post_land_emissions = .data[[scenario_landint]],
    land_actual_reduction = post_land_emissions - baseline_land_emissions
  ) %>%
  left_join(st_drop_geometry(Boundary), by = c("region" = "cty")) %>%
  mutate(country = coalesce(country_name, region))

make_land_world_r5_panel <- function(plot_year, value_col, title_text, fill_color) {
  df <- land_world_r5_raw %>%
    filter(year == plot_year) %>%
    mutate(
      region_name = factor(region_name, levels = rev(c("World", "OECD+EU", "Asia", "Africa/Middle East", "Latin America", "Former Soviet Union"))),
      plot_value = .data[[value_col]]
    )

  ggplot(df, aes(x = plot_value, y = region_name)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
    geom_col(fill = fill_color) +
    labs(title = paste0(title_text, " (", plot_year, ")"), x = "MtCO2eq/yr", y = NULL) +
    theme_main
}

save_plot(
  "figA35_land_baseline_world_r5_2050_2100.png",
  wrap_plots(
    make_land_world_r5_panel(2050, "baseline_land_emissions", "Baseline land emissions in World and R5", "#7AA6C2"),
    make_land_world_r5_panel(2100, "baseline_land_emissions", "Baseline land emissions in World and R5", "#7AA6C2"),
    ncol = 2
  ),
  width = 12,
  height = 6
)

save_plot(
  "figA36_land_post_world_r5_2050_2100.png",
  wrap_plots(
    make_land_world_r5_panel(2050, "post_land_emissions", "Post-mitigation land emissions in World and R5", "#90B4CE"),
    make_land_world_r5_panel(2100, "post_land_emissions", "Post-mitigation land emissions in World and R5", "#90B4CE"),
    ncol = 2
  ),
  width = 12,
  height = 6
)

save_plot(
  "figA37_land_actual_change_world_r5_2050_2100.png",
  wrap_plots(
    make_land_world_r5_panel(2050, "land_actual_reduction", "Actual land-emission change in World and R5", "#5B8E7D"),
    make_land_world_r5_panel(2100, "land_actual_reduction", "Actual land-emission change in World and R5", "#5B8E7D"),
    ncol = 2
  ),
  width = 12,
  height = 6
)

make_land_world_r5_rate_panel <- function(plot_year) {
  df <- land_world_r5_raw %>%
    filter(year == plot_year, is.finite(baseline_land_emissions), baseline_land_emissions > 0) %>%
    mutate(
      region_name = factor(region_name, levels = rev(c("World", "OECD+EU", "Asia", "Africa/Middle East", "Latin America", "Former Soviet Union"))),
      plot_value = 100 * land_actual_reduction / baseline_land_emissions
    )

  ggplot(df, aes(x = plot_value, y = region_name)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
    geom_col(fill = "#6C8EBF") +
    labs(title = paste0("Actual land-emission change rate in World and R5 (", plot_year, ")"), x = "Reduction rate from No land emissions (%)", y = NULL) +
    theme_main
}

save_plot(
  "figA38_land_actual_change_rate_world_r5_2050_2100.png",
  wrap_plots(make_land_world_r5_rate_panel(2050), make_land_world_r5_rate_panel(2100), ncol = 2),
  width = 12,
  height = 6
)

make_top_land_change_panel <- function(plot_year) {
  df <- land_country_raw %>%
    filter(year == plot_year) %>%
    arrange(land_actual_reduction) %>%
    slice_head(n = 20) %>%
    mutate(country = factor(country, levels = rev(country)))

  ggplot(df, aes(x = land_actual_reduction, y = country)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
    geom_col(fill = "#5B8E7D") +
    labs(title = paste0("Top land-emission change countries (", plot_year, ")"), x = "MtCO2eq/yr", y = NULL) +
    theme_main
}

save_plot(
  "figA39_land_actual_change_top_countries_2050_2100.png",
  wrap_plots(make_top_land_change_panel(2050), make_top_land_change_panel(2100), ncol = 2),
  width = 12,
  height = 8
)

make_top_land_change_rate_panel <- function(plot_year) {
  df <- land_country_raw %>%
    filter(year == plot_year, is.finite(baseline_land_emissions), baseline_land_emissions > 0) %>%
    mutate(land_reduction_rate = 100 * land_actual_reduction / baseline_land_emissions) %>%
    arrange(land_actual_reduction) %>%
    slice_head(n = 20) %>%
    mutate(country = factor(country, levels = rev(country)))

  ggplot(df, aes(x = land_reduction_rate, y = country)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
    geom_col(fill = "#6C8EBF") +
    labs(title = paste0("Top land-emission change rate countries (", plot_year, ")"), x = "Reduction rate from No land emissions (%)", y = NULL) +
    theme_main
}

save_plot(
  "figA40_land_actual_change_rate_top_countries_2050_2100.png",
  wrap_plots(make_top_land_change_rate_panel(2050), make_top_land_change_rate_panel(2100), ncol = 2),
  width = 12,
  height = 8
)

make_top_land_component_panel <- function(plot_year, value_col, title_text) {
  top_regions <- land_country_raw %>%
    filter(year == plot_year) %>%
    arrange(land_actual_reduction) %>%
    slice_head(n = 15) %>%
    pull(region)

  country_order <- land_country_raw %>%
    filter(year == plot_year) %>%
    arrange(land_actual_reduction) %>%
    slice_head(n = 15) %>%
    pull(country)

  df <- land_component_raw %>%
    filter(year == plot_year, region %in% top_regions) %>%
    mutate(
      country = factor(country, levels = rev(country_order)),
      component = factor(component, levels = names(land_component_palette)),
      plot_value = .data[[value_col]]
    )

  ggplot(df, aes(x = plot_value, y = country, fill = component)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
    geom_col() +
    scale_fill_manual(values = land_component_palette) +
    labs(title = paste0(title_text, " (", plot_year, ")"), x = "MtCO2eq/yr", y = NULL) +
    theme_main
}

save_plot(
  "figA41_land_baseline_top_components_2050_2100.png",
  wrap_plots(
    make_top_land_component_panel(2050, "baseline_land_emissions", "Baseline land-emissions composition in top-changing countries"),
    make_top_land_component_panel(2100, "baseline_land_emissions", "Baseline land-emissions composition in top-changing countries"),
    ncol = 2
  ),
  width = 12,
  height = 8
)

save_plot(
  "figA42_land_post_top_components_2050_2100.png",
  wrap_plots(
    make_top_land_component_panel(2050, "post_land_emissions", "Post-mitigation land-emissions composition in top-changing countries"),
    make_top_land_component_panel(2100, "post_land_emissions", "Post-mitigation land-emissions composition in top-changing countries"),
    ncol = 2
  ),
  width = 12,
  height = 8
)

save_plot(
  "figA43_land_actual_change_components_2050_2100.png",
  wrap_plots(
    make_top_land_component_panel(2050, "land_actual_reduction", "Actual land-emission change composition in top-changing countries"),
    make_top_land_component_panel(2100, "land_actual_reduction", "Actual land-emission change composition in top-changing countries"),
    ncol = 2
  ),
  width = 12,
  height = 8
)

# land appendix: hierarchy-consistent detailed categories ------------------
land_hierarchy_palette <- c(
  "Forest and grassland conversion" = "#E68613",
  "Abandonment of managed lands" = "#F2C14E",
  "Soil emissions/removals" = "#7A8799",
  "Forest burning CO2" = "#C44536",
  "Peat fire CO2" = "#7B5E57",
  "Managed forest sink" = "#2A6F97"
)

land_hierarchy_raw <- GEER_AFOLUB_ALL %>%
  transmute(
    scenario = as.character(Scenario),
    region = as.character(REMF),
    variable = as.character(VEMF),
    year = as.integer(as.character(YEMF)),
    value = as.numeric(IAMC_templatePlus)
  ) %>%
  filter(
    scenario %in% c(scenario_no, scenario_landint),
    year %in% plot_years,
    variable %in% c(
      "Emi_CO2_AFO_Lan_For_Gra_Con",
      "Emi_CO2_AFO_Lan_Aba_Man_Lan",
      "Emi_CO2_AFO_Lan_Emi_Rem_Soi",
      "Emi_CO2_AFO_Lan_For_Bur",
      "Emi_CO2_AFO_Lan_Pea_Fir_For",
      "Emi_CO2_Lan_Use_Flo_Neg_Seq_Man_For"
    )
  ) %>%
  mutate(component = recode(
    variable,
    "Emi_CO2_AFO_Lan_For_Gra_Con" = "Forest and grassland conversion",
    "Emi_CO2_AFO_Lan_Aba_Man_Lan" = "Abandonment of managed lands",
    "Emi_CO2_AFO_Lan_Emi_Rem_Soi" = "Soil emissions/removals",
    "Emi_CO2_AFO_Lan_For_Bur" = "Forest burning CO2",
    "Emi_CO2_AFO_Lan_Pea_Fir_For" = "Peat fire CO2",
    "Emi_CO2_Lan_Use_Flo_Neg_Seq_Man_For" = "Managed forest sink"
  )) %>%
  group_by(region, year, component, scenario) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = scenario, values_from = value) %>%
  mutate(
    baseline_land_emissions = .data[[scenario_no]],
    post_land_emissions = .data[[scenario_landint]],
    land_actual_change = post_land_emissions - baseline_land_emissions
  )

land_managed_sink_afr <- GEER_WORLD_GHG_LU %>%
  transmute(
    scenario = as.character(SCENARIO),
    src = as.character(src),
    gas = as.character(gas),
    region = as.character(cty),
    year = as.integer(as.character(Y)),
    value = as.numeric(GHG_LU_load) / 1000
  ) %>%
  filter(
    scenario %in% c(scenario_no, scenario_landint),
    src == "afr",
    gas == "CO2",
    region %in% country_codes,
    year %in% plot_years
  ) %>%
  group_by(region, year, scenario) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = scenario, values_from = value, values_fill = 0) %>%
  {
    .$baseline_land_emissions <- safe_pull(., scenario_landint)
    .$post_land_emissions <- safe_pull(., scenario_landint)
    .$land_actual_change <- .$post_land_emissions
    .$component <- "Managed forest sink"
    .
  } %>%
  select(region, year, component, baseline_land_emissions, post_land_emissions, land_actual_change)

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
    scenario %in% c(scenario_no, scenario_landint),
    src == "afr",
    gas == "CO2",
    region %in% c("World", names(r5_labels)),
    year %in% plot_years
  ) %>%
  group_by(region, year, scenario) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = scenario, values_from = value, values_fill = 0) %>%
  {
    .$baseline_land_emissions <- safe_pull(., scenario_landint)
    .$post_land_emissions <- safe_pull(., scenario_landint)
    .$land_actual_change <- .$post_land_emissions
    .$component <- "Managed forest sink"
    .
  } %>%
  select(region, year, component, baseline_land_emissions, post_land_emissions, land_actual_change)

land_hierarchy_raw_afr <- bind_rows(
  land_hierarchy_raw %>% filter(component != "Managed forest sink"),
  land_managed_sink_afr,
  land_managed_sink_afr_agg
)

make_land_hierarchy_world_r5_panel_from <- function(raw_df, plot_year, value_col, title_text) {
  df <- raw_df %>%
    filter(year == plot_year, region %in% c("World", names(r5_labels))) %>%
    filter(!(value_col == "land_actual_change" & component == "Peat fire CO2")) %>%
    mutate(
      region_name = recode(region, !!!r5_labels),
      region_name = factor(region_name, levels = rev(c("World", "OECD+EU", "Asia", "Africa/Middle East", "Latin America", "Former Soviet Union"))),
      component = factor(component, levels = names(land_hierarchy_palette))
    )

  ggplot(df, aes(x = .data[[value_col]], y = region_name, fill = component)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
    geom_col() +
    scale_fill_manual(values = land_hierarchy_palette) +
    labs(title = paste0(title_text, " (", plot_year, ")"), x = "MtCO2eq/yr", y = NULL) +
    guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
    theme_main +
    theme(
      legend.position = "bottom",
      legend.text = element_text(size = 9),
      plot.title = element_text(size = 11, lineheight = 1.0)
    )
}

make_land_hierarchy_world_r5_panel <- function(plot_year, value_col, title_text) {
  make_land_hierarchy_world_r5_panel_from(land_hierarchy_raw, plot_year, value_col, title_text)
}

save_plot(
  "figA44_land_hierarchy_baseline_world_r5_2050_2100.png",
  wrap_plots(
    make_land_hierarchy_world_r5_panel(2050, "baseline_land_emissions", "Baseline land emissions by detailed category in World and R5"),
    make_land_hierarchy_world_r5_panel(2100, "baseline_land_emissions", "Baseline land emissions by detailed category in World and R5"),
    ncol = 2
  ),
  width = 13,
  height = 7
)

save_plot(
  "figA45_land_hierarchy_post_world_r5_2050_2100.png",
  wrap_plots(
    make_land_hierarchy_world_r5_panel(2050, "post_land_emissions", "Post-mitigation land emissions by detailed category in World and R5"),
    make_land_hierarchy_world_r5_panel(2100, "post_land_emissions", "Post-mitigation land emissions by detailed category in World and R5"),
    ncol = 2
  ),
  width = 13,
  height = 7
)

save_plot(
  "figA46_land_hierarchy_change_world_r5_2050_2100.png",
  wrap_plots(
    make_land_hierarchy_world_r5_panel(2050, "land_actual_change", "Land-emission change by detailed category in World and R5"),
    make_land_hierarchy_world_r5_panel(2100, "land_actual_change", "Land-emission change by detailed category in World and R5"),
    ncol = 2
  ),
  width = 13,
  height = 7
)

make_land_hierarchy_top_panel <- function(plot_year, value_col, title_text) {
  df <- land_hierarchy_raw %>%
    filter(year == plot_year, region %in% country_codes) %>%
    filter(!(value_col == "land_actual_change" & component == "Peat fire CO2")) %>%
    group_by(region, year, component) %>%
    summarise(plot_value = sum(.data[[value_col]], na.rm = TRUE), .groups = "drop")

  totals <- df %>%
    group_by(region) %>%
    summarise(total_value = sum(plot_value, na.rm = TRUE), .groups = "drop") %>%
    arrange(total_value) %>%
    slice_head(n = 15)

  country_order <- totals %>%
    left_join(st_drop_geometry(Boundary), by = c("region" = "cty")) %>%
    transmute(country = coalesce(country_name, region), total_value) %>%
    arrange(total_value) %>%
    pull(country)

  df <- df %>%
    filter(region %in% totals$region) %>%
    left_join(st_drop_geometry(Boundary), by = c("region" = "cty")) %>%
    mutate(
      country = factor(coalesce(country_name, region), levels = rev(country_order)),
      component = factor(component, levels = names(land_hierarchy_palette))
    )

  points_df <- totals %>%
    left_join(st_drop_geometry(Boundary), by = c("region" = "cty")) %>%
    transmute(
      country = factor(coalesce(country_name, region), levels = rev(country_order)),
      total_value
    )

  ggplot(df, aes(x = plot_value, y = country, fill = component)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
    geom_col() +
    geom_point(
      data = points_df,
      aes(x = total_value, y = country),
      inherit.aes = FALSE,
      shape = 21,
      size = 2.2,
      stroke = 0.35,
      fill = "black",
      color = "black"
    ) +
    scale_fill_manual(values = land_hierarchy_palette) +
    labs(title = paste0(title_text, " (", plot_year, ")"), x = "MtCO2eq/yr", y = NULL) +
    guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
    theme_main +
    theme(
      legend.position = "bottom",
      legend.text = element_text(size = 9),
      plot.title = element_text(size = 11, lineheight = 1.0)
    )
}

make_land_hierarchy_top_panel_from <- function(raw_df, plot_year, value_col, title_text) {
  df <- raw_df %>%
    filter(year == plot_year, region %in% country_codes) %>%
    filter(!(value_col == "land_actual_change" & component == "Peat fire CO2")) %>%
    group_by(region, year, component) %>%
    summarise(plot_value = sum(.data[[value_col]], na.rm = TRUE), .groups = "drop")

  totals <- df %>%
    group_by(region) %>%
    summarise(total_value = sum(plot_value, na.rm = TRUE), .groups = "drop") %>%
    arrange(total_value) %>%
    slice_head(n = 15)

  country_order <- totals %>%
    left_join(st_drop_geometry(Boundary), by = c("region" = "cty")) %>%
    transmute(country = coalesce(country_name, region), total_value) %>%
    arrange(total_value) %>%
    pull(country)

  df <- df %>%
    filter(region %in% totals$region) %>%
    left_join(st_drop_geometry(Boundary), by = c("region" = "cty")) %>%
    mutate(
      country = factor(coalesce(country_name, region), levels = rev(country_order)),
      component = factor(component, levels = names(land_hierarchy_palette))
    )

  points_df <- totals %>%
    left_join(st_drop_geometry(Boundary), by = c("region" = "cty")) %>%
    transmute(
      country = factor(coalesce(country_name, region), levels = rev(country_order)),
      total_value
    )

  ggplot(df, aes(x = plot_value, y = country, fill = component)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
    geom_col() +
    geom_point(
      data = points_df,
      aes(x = total_value, y = country),
      inherit.aes = FALSE,
      shape = 21,
      size = 2.2,
      stroke = 0.35,
      fill = "black",
      color = "black"
    ) +
    scale_fill_manual(values = land_hierarchy_palette) +
    labs(title = paste0(title_text, " (", plot_year, ")"), x = "MtCO2eq/yr", y = NULL) +
    theme_main
}

save_plot(
  "figA47_land_hierarchy_baseline_top_countries_2050_2100.png",
  wrap_plots(
    make_land_hierarchy_top_panel(2050, "baseline_land_emissions", "Baseline land emissions by detailed category in top-changing countries"),
    make_land_hierarchy_top_panel(2100, "baseline_land_emissions", "Baseline land emissions by detailed category in top-changing countries"),
    ncol = 2
  ),
  width = 13,
  height = 8
)

save_plot(
  "figA48_land_hierarchy_post_top_countries_2050_2100.png",
  wrap_plots(
    make_land_hierarchy_top_panel(2050, "post_land_emissions", "Post-mitigation land emissions by detailed category in top-changing countries"),
    make_land_hierarchy_top_panel(2100, "post_land_emissions", "Post-mitigation land emissions by detailed category in top-changing countries"),
    ncol = 2
  ),
  width = 13,
  height = 8
)

save_plot(
  "figA49_land_hierarchy_change_top_countries_2050_2100.png",
  wrap_plots(
    make_land_hierarchy_top_panel(2050, "land_actual_change", "Land-emission change by detailed category in top-changing countries"),
    make_land_hierarchy_top_panel(2100, "land_actual_change", "Land-emission change by detailed category in top-changing countries"),
    ncol = 2
  ),
  width = 13,
  height = 8
)

save_plot(
  "figA46b_land_hierarchy_change_world_r5_afr_2050_2100.png",
  wrap_plots(
    make_land_hierarchy_world_r5_panel_from(land_hierarchy_raw_afr, 2050, "land_actual_change", "Land-emission change in World and R5, AFR sink version"),
    make_land_hierarchy_world_r5_panel_from(land_hierarchy_raw_afr, 2100, "land_actual_change", "Land-emission change in World and R5, AFR sink version"),
    ncol = 2
  ) + plot_layout(guides = "collect") & theme(legend.position = "bottom"),
  width = 16,
  height = 9.5
)

save_plot(
  "figA49b_land_hierarchy_change_top_countries_afr_2050_2100.png",
  wrap_plots(
    make_land_hierarchy_top_panel_from(land_hierarchy_raw_afr, 2050, "land_actual_change", "Land-emission change in top countries, AFR sink version"),
    make_land_hierarchy_top_panel_from(land_hierarchy_raw_afr, 2100, "land_actual_change", "Land-emission change in top countries, AFR sink version"),
    ncol = 2
  ) + plot_layout(guides = "collect") & theme(legend.position = "bottom"),
  width = 16,
  height = 10.5
)

# land appendix: actual change + land mitigation technologies -------------
land_change_tech_palette <- c(
  "Forest and grassland conversion" = "#E68613",
  "Abandonment of managed lands" = "#F2C14E",
  "Soil emissions/removals" = "#7A8799",
  "Forest burning CO2" = "#C44536",
  "Peat fire CO2" = "#7B5E57",
  "Managed forest sink" = "#2A6F97",
  "Forest protection / fire prevention" = "#2D6A4F",
  "Forest management / peatland management" = "#0B6E4F",
  "Enhanced natural regeneration" = "#6BBF59",
  "Agroforestry" = "#4CC9A6",
  "Biochar" = "#5E548E",
  "Soil carbon" = "#2A9D8F"
)

land_mitigation_raw <- GEER_AFOLUB_ALL %>%
  transmute(
    scenario = as.character(Scenario),
    region = as.character(REMF),
    variable = as.character(VEMF),
    year = as.integer(as.character(YEMF)),
    value = as.numeric(IAMC_templatePlus)
  ) %>%
  filter(
    scenario == scenario_landint,
    year %in% plot_years,
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
    variable %in% c("Emi_Red_CO2_AFO_Lan_Enr") ~ "Enhanced natural regeneration",
    variable %in% c("Emi_Red_CO2_AFO_Lan_Agf") ~ "Agroforestry",
    variable %in% c("Emi_Red_CO2_AFO_Lan_Bch") ~ "Biochar",
    variable %in% c("Emi_Red_CO2_AFO_Lan_Scc", "Emi_Red_CO2_AFO_Lan_Scg", "Emi_Red_CO2_AFO_Lan_Scs") ~ "Soil carbon",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(component)) %>%
  group_by(region, year, component) %>%
  summarise(plot_value = -sum(abs(value), na.rm = TRUE), .groups = "drop")

land_change_plus_mitigation <- bind_rows(
  land_hierarchy_raw %>%
    filter(component != "Peat fire CO2") %>%
    transmute(region, year, component, plot_value = land_actual_change),
  land_mitigation_raw
) %>%
  mutate(component = factor(component, levels = names(land_change_tech_palette)))

land_change_plus_mitigation_afr <- bind_rows(
  land_hierarchy_raw_afr %>%
    filter(component != "Peat fire CO2") %>%
    transmute(region, year, component, plot_value = land_actual_change),
  land_mitigation_raw
) %>%
  mutate(component = factor(component, levels = names(land_change_tech_palette)))

make_land_change_plus_mitigation_world_r5_panel <- function(plot_year) {
  df <- land_change_plus_mitigation %>%
    filter(year == plot_year, region %in% c("World", names(r5_labels))) %>%
    mutate(
      region_name = recode(region, !!!r5_labels),
      region_name = factor(region_name, levels = rev(c("World", "OECD+EU", "Asia", "Africa/Middle East", "Latin America", "Former Soviet Union")))
    )

  ggplot(df, aes(x = plot_value, y = region_name, fill = component)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
    geom_col() +
    scale_fill_manual(values = land_change_tech_palette) +
    labs(title = paste0("Land-emission change plus mitigation technologies in World and R5 (", plot_year, ")"), x = "MtCO2eq/yr", y = NULL) +
    theme_main
}

save_plot(
  "figA50_land_change_plus_mitigation_world_r5_2050_2100.png",
  wrap_plots(
    make_land_change_plus_mitigation_world_r5_panel(2050),
    make_land_change_plus_mitigation_world_r5_panel(2100),
    ncol = 2
  ),
  width = 13,
  height = 7
)

make_land_change_plus_mitigation_top_panel <- function(plot_year) {
  df <- land_change_plus_mitigation %>%
    filter(year == plot_year, region %in% country_codes)

  totals <- df %>%
    group_by(region) %>%
    summarise(total_value = sum(plot_value, na.rm = TRUE), .groups = "drop") %>%
    arrange(total_value) %>%
    slice_head(n = 15)

  country_order <- totals %>%
    left_join(st_drop_geometry(Boundary), by = c("region" = "cty")) %>%
    transmute(country = coalesce(country_name, region), total_value) %>%
    arrange(total_value) %>%
    pull(country)

  df <- df %>%
    filter(region %in% totals$region) %>%
    left_join(st_drop_geometry(Boundary), by = c("region" = "cty")) %>%
    mutate(country = factor(coalesce(country_name, region), levels = rev(country_order)))

  points_df <- totals %>%
    left_join(st_drop_geometry(Boundary), by = c("region" = "cty")) %>%
    transmute(
      country = factor(coalesce(country_name, region), levels = rev(country_order)),
      total_value
    )

  ggplot(df, aes(x = plot_value, y = country, fill = component)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
    geom_col() +
    geom_point(
      data = points_df,
      aes(x = total_value, y = country),
      inherit.aes = FALSE,
      shape = 21,
      size = 2.2,
      stroke = 0.35,
      fill = "black",
      color = "black"
    ) +
    scale_fill_manual(values = land_change_tech_palette) +
    labs(title = paste0("Land-emission change plus mitigation technologies in top-changing countries (", plot_year, ")"), x = "MtCO2eq/yr", y = NULL) +
    theme_main
}

save_plot(
  "figA51_land_change_plus_mitigation_top_countries_2050_2100.png",
  wrap_plots(
    make_land_change_plus_mitigation_top_panel(2050),
    make_land_change_plus_mitigation_top_panel(2100),
    ncol = 2
  ),
  width = 13,
  height = 8
)

make_land_change_plus_mitigation_world_r5_panel_from <- function(raw_df, plot_year, title_text) {
  df <- raw_df %>%
    filter(year == plot_year, region %in% c("World", names(r5_labels))) %>%
    mutate(
      region_name = recode(region, !!!r5_labels),
      region_name = factor(region_name, levels = rev(c("World", "OECD+EU", "Asia", "Africa/Middle East", "Latin America", "Former Soviet Union")))
    )

  ggplot(df, aes(x = plot_value, y = region_name, fill = component)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
    geom_col() +
    scale_fill_manual(values = land_change_tech_palette) +
    labs(title = paste0(title_text, " (", plot_year, ")"), x = "MtCO2eq/yr", y = NULL) +
    guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
    theme_main +
    theme(
      legend.position = "bottom",
      legend.text = element_text(size = 9),
      plot.title = element_text(size = 11, lineheight = 1.0)
    )
}

make_land_change_plus_mitigation_top_panel_from <- function(raw_df, plot_year, title_text) {
  df <- raw_df %>%
    filter(year == plot_year, region %in% country_codes)

  totals <- df %>%
    group_by(region) %>%
    summarise(total_value = sum(plot_value, na.rm = TRUE), .groups = "drop") %>%
    arrange(total_value) %>%
    slice_head(n = 15)

  country_order <- totals %>%
    left_join(st_drop_geometry(Boundary), by = c("region" = "cty")) %>%
    transmute(country = coalesce(country_name, region), total_value) %>%
    arrange(total_value) %>%
    pull(country)

  df <- df %>%
    filter(region %in% totals$region) %>%
    left_join(st_drop_geometry(Boundary), by = c("region" = "cty")) %>%
    mutate(country = factor(coalesce(country_name, region), levels = rev(country_order)))

  points_df <- totals %>%
    left_join(st_drop_geometry(Boundary), by = c("region" = "cty")) %>%
    transmute(
      country = factor(coalesce(country_name, region), levels = rev(country_order)),
      total_value
    )

  ggplot(df, aes(x = plot_value, y = country, fill = component)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
    geom_col() +
    geom_point(
      data = points_df,
      aes(x = total_value, y = country),
      inherit.aes = FALSE,
      shape = 21,
      size = 2.2,
      stroke = 0.35,
      fill = "black",
      color = "black"
    ) +
    scale_fill_manual(values = land_change_tech_palette) +
    labs(title = paste0(title_text, " (", plot_year, ")"), x = "MtCO2eq/yr", y = NULL) +
    guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
    theme_main +
    theme(
      legend.position = "bottom",
      legend.text = element_text(size = 9),
      plot.title = element_text(size = 11, lineheight = 1.0)
    )
}

save_plot(
  "figA50b_land_change_plus_mitigation_world_r5_afr_2050_2100.png",
  wrap_plots(
    make_land_change_plus_mitigation_world_r5_panel_from(land_change_plus_mitigation_afr, 2050, "Land change plus mitigation in World and R5, AFR sink version"),
    make_land_change_plus_mitigation_world_r5_panel_from(land_change_plus_mitigation_afr, 2100, "Land change plus mitigation in World and R5, AFR sink version"),
    ncol = 2
  ) + plot_layout(guides = "collect") & theme(legend.position = "bottom"),
  width = 16,
  height = 9.5
)

save_plot(
  "figA51b_land_change_plus_mitigation_top_countries_afr_2050_2100.png",
  wrap_plots(
    make_land_change_plus_mitigation_top_panel_from(land_change_plus_mitigation_afr, 2050, "Land change plus mitigation in top countries, AFR sink version"),
    make_land_change_plus_mitigation_top_panel_from(land_change_plus_mitigation_afr, 2100, "Land change plus mitigation in top countries, AFR sink version"),
    ncol = 2
  ) + plot_layout(guides = "collect") & theme(legend.position = "bottom"),
  width = 16,
  height = 10.5
)

# appendix: AFOLU actual reduction from agriculture + land ----------------
afolu_component_palette <- c(
  "Agriculture" = "#F6C667",
  "Land use" = "#6BAED6"
)

afolu_ag_country <- agmip_ag_reduction_rank %>%
  transmute(region, year, component = "Agriculture", value = ag_reduction)

afolu_ag_world_r5 <- agmip_ag_reduction_rate_r5 %>%
  transmute(region = region_name, year, component = "Agriculture", value = ag_reduction)

afolu_land_country <- land_change_plus_mitigation_afr %>%
  filter(region %in% country_codes) %>%
  group_by(region, year) %>%
  summarise(value = -sum(plot_value, na.rm = TRUE), .groups = "drop") %>%
  mutate(component = "Land use")

afolu_land_world_r5 <- land_change_plus_mitigation_afr %>%
  filter(region %in% c("World", names(r5_labels))) %>%
  group_by(region, year) %>%
  summarise(value = -sum(plot_value, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    region = recode(region, !!!r5_labels),
    component = "Land use"
  )

afolu_world_r5 <- bind_rows(afolu_ag_world_r5, afolu_land_world_r5)

make_afolu_world_r5_panel <- function(plot_year) {
  df <- afolu_world_r5 %>%
    filter(year == plot_year) %>%
    mutate(region = factor(region, levels = rev(c("World", "OECD+EU", "Asia", "Africa/Middle East", "Latin America", "Former Soviet Union"))))

  ggplot(df, aes(x = value, y = region, fill = component)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
    geom_col() +
    scale_fill_manual(values = afolu_component_palette) +
    labs(title = paste0("AFOLU actual reduction in World and R5 (", plot_year, ")"), x = "MtCO2eq/yr", y = NULL) +
    theme_main
}

save_plot(
  "figA52_afolu_actual_reduction_world_r5_2050_2100.png",
  wrap_plots(
    make_afolu_world_r5_panel(2050),
    make_afolu_world_r5_panel(2100),
    ncol = 2
  ),
  width = 12,
  height = 7
)

afolu_country <- bind_rows(afolu_ag_country, afolu_land_country)

make_afolu_top_country_panel <- function(plot_year) {
  totals <- afolu_country %>%
    filter(year == plot_year) %>%
    group_by(region) %>%
    summarise(total_value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(total_value)) %>%
    slice_head(n = 15)

  country_order <- totals %>%
    left_join(st_drop_geometry(Boundary), by = c("region" = "cty")) %>%
    transmute(country = coalesce(country_name, region), total_value) %>%
    arrange(desc(total_value)) %>%
    pull(country)

  df <- afolu_country %>%
    filter(year == plot_year, region %in% totals$region) %>%
    left_join(st_drop_geometry(Boundary), by = c("region" = "cty")) %>%
    mutate(country = factor(coalesce(country_name, region), levels = rev(country_order)))

  points_df <- totals %>%
    left_join(st_drop_geometry(Boundary), by = c("region" = "cty")) %>%
    transmute(
      country = factor(coalesce(country_name, region), levels = rev(country_order)),
      total_value
    )

  ggplot(df, aes(x = value, y = country, fill = component)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
    geom_col() +
    geom_point(
      data = points_df,
      aes(x = total_value, y = country),
      inherit.aes = FALSE,
      shape = 21,
      size = 2.2,
      stroke = 0.35,
      fill = "black",
      color = "black"
    ) +
    scale_fill_manual(values = afolu_component_palette) +
    labs(title = paste0("Top AFOLU reduction countries (", plot_year, ")"), x = "MtCO2eq/yr", y = NULL) +
    theme_main
}

save_plot(
  "figA53_afolu_actual_reduction_top_countries_2050_2100.png",
  wrap_plots(
    make_afolu_top_country_panel(2050),
    make_afolu_top_country_panel(2100),
    ncol = 2
  ),
  width = 12,
  height = 8
)

# appendix: AFOLU baseline emissions from agriculture + land use ----------
afolu_baseline_ag_country <- agmip_emis_actual_reduction %>%
  filter(region %in% country_codes, item != "tot") %>%
  group_by(region, year) %>%
  summarise(value = sum(.data[[scenario_baseline]], na.rm = TRUE), .groups = "drop") %>%
  mutate(component = "Agriculture")

afolu_baseline_ag_world_r5 <- GEER_AGMIP_AGG %>%
  filter(
    region %in% names(r5_labels),
    variable == "EMIS",
    unit == "MtCO2e",
    item == "AGR",
    scenario == scenario_baseline,
    year %in% plot_years
  ) %>%
  transmute(region = recode(region, !!!r5_labels), year, component = "Agriculture", value)

afolu_baseline_land_country <- land_baseline_country_total %>%
  transmute(region, year, component = "Land use", value = baseline_land_emissions)

afolu_baseline_land_world_r5 <- land_world_r5_raw %>%
  transmute(region = region_name, year, component = "Land use", value = baseline_land_emissions) %>%
  bind_rows(
    land_total_raw %>%
      filter(region == "World") %>%
      transmute(region = "World", year, component = "Land use", value = baseline_land_emissions)
  )

afolu_baseline_world_r5 <- bind_rows(
  afolu_baseline_ag_world_r5 %>% bind_rows(
    GEER_AGMIP_AGG %>%
      filter(
        region == "World",
        variable == "EMIS",
        unit == "MtCO2e",
        item == "AGR",
        scenario == scenario_baseline,
        year %in% plot_years
      ) %>%
      transmute(region = "World", year, component = "Agriculture", value)
  ),
  afolu_baseline_land_world_r5
)

afolu_baseline_country <- bind_rows(afolu_baseline_ag_country, afolu_baseline_land_country)

make_afolu_baseline_world_r5_panel <- function(plot_year) {
  df <- afolu_baseline_world_r5 %>%
    filter(year == plot_year) %>%
    mutate(region = factor(region, levels = rev(c("World", "OECD+EU", "Asia", "Africa/Middle East", "Latin America", "Former Soviet Union"))))

  ggplot(df, aes(x = value, y = region, fill = component)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
    geom_col() +
    scale_fill_manual(
      values = afolu_component_palette,
      limits = c("Land use", "Agriculture"),
      drop = FALSE
    ) +
    labs(title = paste0("AFOLU baseline emissions in World and R5 (", plot_year, ")"), x = "MtCO2eq/yr", y = NULL) +
    theme_main
}

save_plot(
  "figA52b_afolu_baseline_emissions_world_r5_2050_2100.png",
  wrap_plots(
    make_afolu_baseline_world_r5_panel(2050),
    make_afolu_baseline_world_r5_panel(2100),
    ncol = 2
  ),
  width = 12,
  height = 7
)

make_afolu_baseline_top_country_panel <- function(plot_year) {
  totals <- afolu_baseline_country %>%
    filter(year == plot_year) %>%
    group_by(region) %>%
    summarise(total_value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(total_value)) %>%
    slice_head(n = 15)

  country_order <- totals %>%
    left_join(st_drop_geometry(Boundary), by = c("region" = "cty")) %>%
    transmute(country = coalesce(country_name, region), total_value) %>%
    arrange(desc(total_value)) %>%
    pull(country)

  df <- afolu_baseline_country %>%
    filter(year == plot_year, region %in% totals$region) %>%
    left_join(st_drop_geometry(Boundary), by = c("region" = "cty")) %>%
    mutate(
      country = factor(coalesce(country_name, region), levels = rev(country_order)),
      component = factor(component, levels = c("Land use", "Agriculture"))
    )

  points_df <- totals %>%
    left_join(st_drop_geometry(Boundary), by = c("region" = "cty")) %>%
    transmute(
      country = factor(coalesce(country_name, region), levels = rev(country_order)),
      total_value
    )

  ggplot(df, aes(x = value, y = country, fill = component)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
    geom_col() +
    geom_point(
      data = points_df,
      aes(x = total_value, y = country),
      inherit.aes = FALSE,
      shape = 21,
      size = 2.2,
      stroke = 0.35,
      fill = "black",
      color = "black"
    ) +
    scale_fill_manual(
      values = afolu_component_palette,
      limits = c("Land use", "Agriculture"),
      drop = FALSE
    ) +
    labs(title = paste0("Top AFOLU baseline emitters (", plot_year, ")"), x = "MtCO2eq/yr", y = NULL) +
    theme_main
}

save_plot(
  "figA53b_afolu_baseline_emissions_top_countries_2050_2100.png",
  wrap_plots(
    make_afolu_baseline_top_country_panel(2050),
    make_afolu_baseline_top_country_panel(2100),
    ncol = 2
  ),
  width = 14,
  height = 8.5
)

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

make_afolu_rate_top_country_panel <- function(plot_year) {
  totals <- afolu_rate_country %>%
    filter(year == plot_year, region %in% country_codes, is.finite(reduction_rate)) %>%
    arrange(desc(reduction_rate)) %>%
    slice_head(n = 15)

  country_order <- totals %>%
    left_join(st_drop_geometry(Boundary), by = c("region" = "cty")) %>%
    transmute(country = coalesce(country_name, region), reduction_rate) %>%
    arrange(desc(reduction_rate)) %>%
    pull(country)

  df <- afolu_country %>%
    filter(year == plot_year, region %in% totals$region) %>%
    left_join(totals %>% select(region, baseline_emissions), by = "region") %>%
    mutate(value = if_else(is.finite(baseline_emissions) & baseline_emissions != 0, 100 * value / baseline_emissions, NA_real_)) %>%
    left_join(st_drop_geometry(Boundary), by = c("region" = "cty")) %>%
    mutate(country = factor(coalesce(country_name, region), levels = rev(country_order)))

  points_df <- totals %>%
    left_join(st_drop_geometry(Boundary), by = c("region" = "cty")) %>%
    transmute(
      country = factor(coalesce(country_name, region), levels = rev(country_order)),
      reduction_rate
    )

  ggplot(df, aes(x = value, y = country, fill = component)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
    geom_col() +
    geom_point(
      data = points_df,
      aes(x = reduction_rate, y = country),
      inherit.aes = FALSE,
      shape = 21,
      size = 2.2,
      stroke = 0.35,
      fill = "black",
      color = "black"
    ) +
    scale_fill_manual(values = afolu_component_palette) +
    labs(title = paste0("Top AFOLU reduction-rate countries (", plot_year, ")"), x = "% of AFOLU BaU emissions", y = NULL) +
    theme_main
}

save_plot(
  "figA53d_afolu_reduction_rate_top_countries_2050_2100.png",
  wrap_plots(
    make_afolu_rate_top_country_panel(2050),
    make_afolu_rate_top_country_panel(2100),
    ncol = 2
  ),
  width = 12,
  height = 8
)

afolu_baseline_land_convfire_country <- GEER_AFOLUB_REF %>%
  filter(
    scenario == scenario_no,
    region %in% country_codes,
    year %in% plot_years,
    variable %in% c(
      "Emi_CO2_AFO_Lan_For_Gra_Con",
      "Emi_CO2_AFO_Lan_For_Bur",
      "Emi_CO2_AFO_Lan_Pea_Fir_For"
    )
  ) %>%
  group_by(region, year) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  mutate(component = "Land use")

afolu_baseline_land_convfire_world_r5 <- GEER_AFOLUB_REF %>%
  filter(
    scenario == scenario_no,
    region %in% c("World", names(r5_labels)),
    year %in% plot_years,
    variable %in% c(
      "Emi_CO2_AFO_Lan_For_Gra_Con",
      "Emi_CO2_AFO_Lan_For_Bur",
      "Emi_CO2_AFO_Lan_Pea_Fir_For"
    )
  ) %>%
  group_by(region, year) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    region = recode(region, !!!r5_labels),
    component = "Land use"
  )

afolu_baseline_convfire_world_r5 <- bind_rows(
  afolu_baseline_ag_world_r5 %>% bind_rows(
    GEER_AGMIP_AGG %>%
      filter(
        region == "World",
        variable == "EMIS",
        unit == "MtCO2e",
        item == "AGR",
        scenario == scenario_baseline,
        year %in% plot_years
      ) %>%
      transmute(region = "World", year, component = "Agriculture", value)
  ),
  afolu_baseline_land_convfire_world_r5
)

afolu_baseline_convfire_country <- bind_rows(
  afolu_baseline_ag_country,
  afolu_baseline_land_convfire_country
)

make_afolu_baseline_convfire_world_r5_panel <- function(plot_year) {
  df <- afolu_baseline_convfire_world_r5 %>%
    filter(year == plot_year) %>%
    mutate(
      region = factor(region, levels = rev(c("World", "OECD+EU", "Asia", "Africa/Middle East", "Latin America", "Former Soviet Union"))),
      component = factor(component, levels = c("Land use", "Agriculture"))
    )

  ggplot(df, aes(x = value, y = region, fill = component)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
    geom_col() +
    scale_fill_manual(
      values = afolu_component_palette,
      limits = c("Land use", "Agriculture"),
      drop = FALSE
    ) +
    labs(title = paste0("AFOLU baseline emissions (conversion + fire land use) in World and R5 (", plot_year, ")"), x = "MtCO2eq/yr", y = NULL) +
    theme_main
}

save_plot(
  "figA52c_afolu_baseline_emissions_convfire_world_r5_2050_2100.png",
  wrap_plots(
    make_afolu_baseline_convfire_world_r5_panel(2050),
    make_afolu_baseline_convfire_world_r5_panel(2100),
    ncol = 2
  ),
  width = 12,
  height = 7
)

make_afolu_baseline_convfire_top_country_panel <- function(plot_year) {
  totals <- afolu_baseline_convfire_country %>%
    filter(year == plot_year) %>%
    group_by(region) %>%
    summarise(total_value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(total_value)) %>%
    slice_head(n = 15)

  country_order <- totals %>%
    left_join(st_drop_geometry(Boundary), by = c("region" = "cty")) %>%
    transmute(country = coalesce(country_name, region), total_value) %>%
    arrange(desc(total_value)) %>%
    pull(country)

  df <- afolu_baseline_convfire_country %>%
    filter(year == plot_year, region %in% totals$region) %>%
    left_join(st_drop_geometry(Boundary), by = c("region" = "cty")) %>%
    mutate(
      country = factor(coalesce(country_name, region), levels = rev(country_order)),
      component = factor(component, levels = c("Land use", "Agriculture"))
    )

  points_df <- totals %>%
    left_join(st_drop_geometry(Boundary), by = c("region" = "cty")) %>%
    transmute(
      country = factor(coalesce(country_name, region), levels = rev(country_order)),
      total_value
    )

  ggplot(df, aes(x = value, y = country, fill = component)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
    geom_col() +
    geom_point(
      data = points_df,
      aes(x = total_value, y = country),
      inherit.aes = FALSE,
      shape = 21,
      size = 2.2,
      stroke = 0.35,
      fill = "black",
      color = "black"
    ) +
    scale_fill_manual(
      values = afolu_component_palette,
      limits = c("Land use", "Agriculture"),
      drop = FALSE
    ) +
    labs(title = paste0("Top AFOLU baseline emitters (conversion + fire land use, ", plot_year, ")"), x = "MtCO2eq/yr", y = NULL) +
    theme_main
}

save_plot(
  "figA53c_afolu_baseline_emissions_convfire_top_countries_2050_2100.png",
  wrap_plots(
    make_afolu_baseline_convfire_top_country_panel(2050),
    make_afolu_baseline_convfire_top_country_panel(2100),
    ncol = 2
  ),
  width = 14,
  height = 8.5
)

# appendix: AFOLU concentration and heterogeneity -------------------------
afolu_total_country <- afolu_country %>%
  group_by(region, year) %>%
  summarise(total_value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  filter(region %in% country_codes) %>%
  left_join(st_drop_geometry(Boundary), by = c("region" = "cty")) %>%
  mutate(country = coalesce(country_name, region))

make_afolu_concentration_curve <- function(plot_year) {
  df <- afolu_total_country %>%
    filter(year == plot_year) %>%
    arrange(desc(total_value)) %>%
    mutate(
      rank = row_number(),
      cumulative_share = cumsum(total_value) / sum(total_value) * 100
    )

  ggplot(df, aes(x = rank, y = cumulative_share)) +
    geom_line(color = "#2166AC", linewidth = 0.9) +
    geom_point(color = "#2166AC", size = 1.2) +
    geom_hline(yintercept = c(25, 50, 75, 90), linetype = "dashed", color = "grey75", linewidth = 0.3) +
    labs(
      title = paste0("Concentration of AFOLU actual reduction (", plot_year, ")"),
      x = "Country rank",
      y = "Cumulative share of total reduction (%)"
    ) +
    theme_main
}

save_plot(
  "figA54_afolu_actual_reduction_concentration_curve_2050_2100.png",
  wrap_plots(
    make_afolu_concentration_curve(2050),
    make_afolu_concentration_curve(2100),
    ncol = 2
  ),
  width = 12,
  height = 6
)

make_afolu_top_share_panel <- function(plot_year) {
  df <- afolu_total_country %>%
    filter(year == plot_year) %>%
    arrange(desc(total_value))

  summary_df <- tibble(top_n = c(5, 10, 15, 20, 30)) %>%
    mutate(
      share = map_dbl(top_n, ~ sum(head(df$total_value, .x), na.rm = TRUE) / sum(df$total_value, na.rm = TRUE) * 100)
    )

  ggplot(summary_df, aes(x = factor(top_n, levels = top_n), y = share)) +
    geom_col(fill = "#4C78A8", width = 0.72) +
    geom_text(aes(label = sprintf("%.0f%%", share)), vjust = -0.35, size = 3.4) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.08)), limits = c(0, 100)) +
    labs(
      title = paste0("Top-country share of AFOLU actual reduction (", plot_year, ")"),
      x = "Top N countries",
      y = "Share of total reduction (%)"
    ) +
    theme_main
}

save_plot(
  "figA55_afolu_actual_reduction_top_share_2050_2100.png",
  wrap_plots(
    make_afolu_top_share_panel(2050),
    make_afolu_top_share_panel(2100),
    ncol = 2
  ),
  width = 12,
  height = 6
)

save_plot(
  "figA56_afolu_actual_reduction_map_2050_2100.png",
  wrap_plots(
    make_country_map(
      afolu_total_country %>% filter(year == 2050) %>% select(region, total_value),
      "total_value",
      "AFOLU actual reduction in 2050",
      "MtCO2eq/yr"
    ),
    make_country_map(
      afolu_total_country %>% filter(year == 2100) %>% select(region, total_value),
      "total_value",
      "AFOLU actual reduction in 2100",
      "MtCO2eq/yr"
    ),
    ncol = 2
  ),
  width = 14,
  height = 6.5
)

make_afolu_rank_size_panel <- function(plot_year) {
  df <- afolu_total_country %>%
    filter(year == plot_year) %>%
    arrange(desc(total_value)) %>%
    slice_head(n = 30) %>%
    mutate(
      rank = row_number(),
      country = factor(country, levels = rev(country)),
      share = total_value / sum(afolu_total_country$total_value[afolu_total_country$year == plot_year], na.rm = TRUE) * 100,
      cumulative_share = cumsum(share)
    )

  ggplot(df, aes(x = country, y = share)) +
    geom_col(fill = "#76B7B2", width = 0.75) +
    geom_line(aes(x = rank, y = cumulative_share / 2), inherit.aes = FALSE, color = "#E15759", linewidth = 0.8, group = 1) +
    geom_point(aes(x = rank, y = cumulative_share / 2), inherit.aes = FALSE, color = "#E15759", size = 1.1) +
    coord_flip() +
    labs(
      title = paste0("Top 30 country contributions to AFOLU reduction (", plot_year, ")"),
      x = NULL,
      y = "Country share (%)"
    ) +
    theme_main
}

save_plot(
  "figA57_afolu_actual_reduction_rank_size_2050_2100.png",
  wrap_plots(
    make_afolu_rank_size_panel(2050),
    make_afolu_rank_size_panel(2100),
    ncol = 2
  ),
  width = 13,
  height = 9
)

make_afolu_share_cumulative_panel <- function(plot_year) {
  df <- afolu_total_country %>%
    filter(year == plot_year) %>%
    arrange(desc(total_value)) %>%
    slice_head(n = 30) %>%
    mutate(
      rank = row_number(),
      share = total_value / sum(afolu_total_country$total_value[afolu_total_country$year == plot_year], na.rm = TRUE) * 100,
      cumulative_share = cumsum(share),
      country = factor(country, levels = country),
      cumulative_scaled = cumulative_share * (max(share, na.rm = TRUE) / 100)
    )

  share_scale <- max(df$share, na.rm = TRUE) / 100

  ggplot(df, aes(x = country)) +
    geom_col(aes(y = share, fill = "Country share"), width = 0.72) +
    geom_line(aes(y = cumulative_scaled, color = "Cumulative share", group = 1), linewidth = 0.9) +
    geom_point(aes(y = cumulative_scaled, color = "Cumulative share"), size = 1.4) +
    scale_fill_manual(values = c("Country share" = "#76B7B2"), name = NULL) +
    scale_color_manual(values = c("Cumulative share" = "#E15759"), name = NULL) +
    scale_y_continuous(
      name = "Country share of total reduction (%)",
      sec.axis = sec_axis(~ . / share_scale, name = "Cumulative share of total reduction (%)")
    ) +
    labs(
      title = paste0("Top 30 country contributions to AFOLU reduction (", plot_year, ")"),
      x = NULL
    ) +
    theme_main +
    theme(
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 8),
      legend.position = "bottom"
    )
}

save_plot(
  "figA57b_afolu_actual_reduction_share_cumulative_2050_2100.png",
  wrap_plots(
    make_afolu_share_cumulative_panel(2050),
    make_afolu_share_cumulative_panel(2100),
    ncol = 2
  ),
  width = 15,
  height = 7.5
)

make_afolu_top_segments <- function(plot_year, top_n = 10) {
  df <- afolu_total_country %>%
    filter(year == plot_year) %>%
    arrange(desc(total_value)) %>%
    mutate(share = total_value / sum(total_value, na.rm = TRUE) * 100)

  top_df <- df %>%
    slice_head(n = top_n) %>%
    transmute(country, share)

  others_share <- df %>%
    slice(-(1:min(n(), top_n))) %>%
    summarise(share = sum(share, na.rm = TRUE)) %>%
    pull(share)

  bind_rows(
    top_df,
    tibble(country = "Others", share = others_share)
  ) %>%
    mutate(country = forcats::fct_inorder(country))
}

afolu_top_country_palette <- c(
  "#4E79A7", "#F28E2B", "#E15759", "#76B7B2", "#59A14F",
  "#EDC948", "#B07AA1", "#FF9DA7", "#9C755F", "#BAB0AC", "#D9D9D9"
)

make_afolu_pie_panel <- function(plot_year) {
  df <- make_afolu_top_segments(plot_year, top_n = 10) %>%
    mutate(
      ymax = cumsum(share),
      ymin = lag(ymax, default = 0),
      label = if_else(share >= 4, paste0(as.character(country), "\n", sprintf("%.0f%%", share)), NA_character_)
    )

  ggplot(df, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 1.8, fill = country)) +
    geom_rect(color = "white", linewidth = 0.5) +
    geom_text(
      aes(x = 4.35, y = (ymin + ymax) / 2, label = label),
      inherit.aes = FALSE,
      size = 3,
      hjust = 0,
      na.rm = TRUE
    ) +
    coord_polar(theta = "y") +
    xlim(c(0.5, 5.1)) +
    scale_fill_manual(values = afolu_top_country_palette[seq_len(nrow(df))], name = NULL) +
    labs(title = paste0("AFOLU reduction shares: top countries + others (", plot_year, ")")) +
    theme_void() +
    theme(
      legend.position = "right",
      plot.title = element_text(size = 12, hjust = 0.5)
    )
}

save_plot(
  "figA58_afolu_actual_reduction_pie_2050_2100.png",
  wrap_plots(
    make_afolu_pie_panel(2050),
    make_afolu_pie_panel(2100),
    ncol = 2
  ),
  width = 16,
  height = 7.5
)

make_afolu_stacked_share_panel <- function(plot_year) {
  df <- make_afolu_top_segments(plot_year, top_n = 10) %>%
    mutate(group = paste0(plot_year))

  ggplot(df, aes(x = group, y = share, fill = country)) +
    geom_col(width = 0.72) +
    coord_flip() +
    scale_fill_manual(values = afolu_top_country_palette[seq_len(nrow(df))], name = NULL) +
    labs(
      title = paste0("AFOLU reduction shares: top countries + others (", plot_year, ")"),
      x = NULL,
      y = "Share of total reduction (%)"
    ) +
    theme_main +
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      legend.position = "bottom",
      legend.box = "vertical"
    )
}

save_plot(
  "figA59_afolu_actual_reduction_stacked_share_2050_2100.png",
  wrap_plots(
    make_afolu_stacked_share_panel(2050),
    make_afolu_stacked_share_panel(2100),
    ncol = 1,
    guides = "collect"
  ) & theme(legend.position = "bottom"),
  width = 14,
  height = 8
)

prod_kcal_country_2100 <- GEER_AGMIP %>%
  filter(
    region %in% country_codes,
    year == 2100,
    scenario %in% c(scenario_baseline, scenario_landint),
    variable == "PROD",
    item %in% c("CRP", "LSP"),
    unit == "million kcal"
  ) %>%
  left_join(st_drop_geometry(Boundary), by = c("region" = "cty")) %>%
  mutate(country = coalesce(country_name, region))

make_prod_kcal_top_panel <- function(plot_scenario, plot_item) {
  df <- prod_kcal_country_2100 %>%
    filter(scenario == plot_scenario, item == plot_item) %>%
    arrange(desc(value)) %>%
    slice_head(n = 20) %>%
    mutate(country = factor(country, levels = rev(country)))

  ggplot(df, aes(x = value, y = country)) +
    geom_col(fill = if_else(plot_item == "CRP", "#5B8E7D", "#D98C5F"), width = 0.75) +
    geom_point(shape = 21, size = 1.8, stroke = 0.3, fill = "black", color = "black") +
    labs(
      title = paste0(
        if_else(plot_item == "CRP", "CRP", "LSP"),
        " production in 2100: ",
        if_else(plot_scenario == scenario_baseline, "BaU", "LandInt")
      ),
      x = "million kcal",
      y = NULL
    ) +
    theme_main
}

save_plot(
  "figA60_prod_kcal_top_countries_2100.png",
  wrap_plots(
    make_prod_kcal_top_panel(scenario_baseline, "CRP"),
    make_prod_kcal_top_panel(scenario_landint, "CRP"),
    make_prod_kcal_top_panel(scenario_baseline, "LSP"),
    make_prod_kcal_top_panel(scenario_landint, "LSP"),
    ncol = 2
  ),
  width = 14,
  height = 12
)

afolu_top_2100 <- afolu_country %>%
  filter(year == 2100) %>%
  group_by(region) %>%
  summarise(total_value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total_value)) %>%
  slice_head(n = 15) %>%
  left_join(st_drop_geometry(Boundary), by = c("region" = "cty")) %>%
  transmute(region, country = coalesce(country_name, region), total_value)

afolu_top_order_2100 <- rev(afolu_top_2100$country)

prod_kcal_change_2100 <- GEER_AGMIP %>%
  filter(
    region %in% afolu_top_2100$region,
    year == 2100,
    scenario %in% c(scenario_baseline, scenario_landint),
    variable == "PROD",
    item %in% c("AGR", "CRP", "LSP"),
    unit == "million kcal"
  ) %>%
  select(region, item, scenario, value) %>%
  pivot_wider(names_from = scenario, values_from = value) %>%
  mutate(change = .data[[scenario_landint]] - .data[[scenario_baseline]]) %>%
  left_join(st_drop_geometry(Boundary), by = c("region" = "cty")) %>%
  mutate(country = factor(coalesce(country_name, region), levels = afolu_top_order_2100))

prod_kcal_change_stack_2100 <- prod_kcal_change_2100 %>%
  filter(item %in% c("CRP", "LSP")) %>%
  mutate(component = recode(item, CRP = "CRP", LSP = "LSP"))

prod_kcal_change_point_2100 <- prod_kcal_change_2100 %>%
  filter(item == "AGR") %>%
  transmute(country, total_change = change)

p_prod_kcal_change_2100 <- ggplot(prod_kcal_change_stack_2100, aes(x = change, y = country, fill = component)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
  geom_col() +
  geom_point(
    data = prod_kcal_change_point_2100,
    aes(x = total_change, y = country),
    inherit.aes = FALSE,
    shape = 21,
    size = 2.2,
    stroke = 0.35,
    fill = "black",
    color = "black"
  ) +
  scale_fill_manual(values = c("CRP" = "#5B8E7D", "LSP" = "#D98C5F")) +
  labs(
    title = "Production change in top AFOLU reduction countries (2100)",
    x = "million kcal (LandInt - BaU)",
    y = NULL
  ) +
  theme_main

save_plot(
  "figA61_top_afolu_reduction_prod_change_2100.png",
  p_prod_kcal_change_2100,
  width = 11.5,
  height = 8
)

cali_change_2100 <- GEER_AGMIP %>%
  filter(
    region %in% afolu_top_2100$region,
    year == 2100,
    scenario %in% c(scenario_baseline, scenario_landint),
    variable == "CALI",
    item %in% c("AGR", "CRP", "LSP"),
    unit == "kcal/cap/d"
  ) %>%
  select(region, item, scenario, value) %>%
  pivot_wider(names_from = scenario, values_from = value) %>%
  mutate(change = .data[[scenario_landint]] - .data[[scenario_baseline]]) %>%
  left_join(st_drop_geometry(Boundary), by = c("region" = "cty")) %>%
  mutate(country = factor(coalesce(country_name, region), levels = afolu_top_order_2100))

cali_change_stack_2100 <- cali_change_2100 %>%
  filter(item %in% c("CRP", "LSP")) %>%
  mutate(component = recode(item, CRP = "CRP", LSP = "LSP"))

cali_change_point_2100 <- cali_change_2100 %>%
  filter(item == "AGR") %>%
  transmute(country, total_change = change)

p_cali_change_2100 <- ggplot(cali_change_stack_2100, aes(x = change, y = country, fill = component)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
  geom_col() +
  geom_point(
    data = cali_change_point_2100,
    aes(x = total_change, y = country),
    inherit.aes = FALSE,
    shape = 21,
    size = 2.2,
    stroke = 0.35,
    fill = "black",
    color = "black"
  ) +
  scale_fill_manual(values = c("CRP" = "#5B8E7D", "LSP" = "#D98C5F")) +
  labs(
    title = "Calorie intake change in top AFOLU reduction countries (2100)",
    x = "kcal/cap/d (LandInt - BaU)",
    y = NULL
  ) +
  theme_main

save_plot(
  "figA62_top_afolu_reduction_cali_change_2100.png",
  p_cali_change_2100,
  width = 11.5,
  height = 8
)

land_change_afolu_top_2100 <- agmip_land_long %>%
  filter(year == 2100, region %in% afolu_top_2100$region) %>%
  left_join(afolu_top_2100 %>% select(region, country), by = "region") %>%
  mutate(country = factor(country, levels = afolu_top_order_2100))

p_land_change_share_afolu_top_2100 <- ggplot(land_change_afolu_top_2100, aes(x = land_share, y = country, fill = land_item)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
  geom_col() +
  scale_fill_manual(values = land_color) +
  labs(
    title = "Land change from BaU in top AFOLU reduction countries (2100)",
    x = "% of total land area",
    y = NULL
  ) +
  theme_main

save_plot(
  "figA63_top_afolu_reduction_land_change_share_2100.png",
  p_land_change_share_afolu_top_2100,
  width = 11.5,
  height = 8
)

xprp_change_2100 <- GEER_AGMIP %>%
  filter(
    region %in% afolu_top_2100$region,
    year == 2100,
    scenario %in% c(scenario_baseline, scenario_landint),
    variable == "XPRP",
    item %in% c("AGR", "CRP", "LSP"),
    unit == "Index (2015 = 1)"
  ) %>%
  select(region, item, scenario, value) %>%
  pivot_wider(names_from = scenario, values_from = value) %>%
  mutate(change_pct = if_else(.data[[scenario_baseline]] != 0, 100 * (.data[[scenario_landint]] / .data[[scenario_baseline]] - 1), NA_real_)) %>%
  left_join(st_drop_geometry(Boundary), by = c("region" = "cty")) %>%
  mutate(country = factor(coalesce(country_name, region), levels = afolu_top_order_2100))

xprp_change_stack_2100 <- xprp_change_2100 %>%
  filter(item %in% c("CRP", "LSP")) %>%
  mutate(component = recode(item, CRP = "CRP", LSP = "LSP"))

xprp_change_point_2100 <- xprp_change_2100 %>%
  filter(item == "AGR") %>%
  transmute(country, total_change = change_pct)

p_xprp_change_2100 <- ggplot(xprp_change_stack_2100, aes(x = change_pct, y = country, fill = component)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
  geom_col() +
  geom_point(
    data = xprp_change_point_2100,
    aes(x = total_change, y = country),
    inherit.aes = FALSE,
    shape = 21,
    size = 2.2,
    stroke = 0.35,
    fill = "black",
    color = "black"
  ) +
  scale_fill_manual(values = c("CRP" = "#5B8E7D", "LSP" = "#D98C5F")) +
  labs(
    title = "Price index change in top AFOLU reduction countries (2100)",
    x = "% change from BaU",
    y = NULL
  ) +
  theme_main

save_plot(
  "figA64_top_afolu_reduction_xprp_change_2100.png",
  p_xprp_change_2100,
  width = 11.5,
  height = 8
)

self_suff_change_2100 <- GEER_AGMIP %>%
  filter(
    region %in% afolu_top_2100$region,
    year == 2100,
    scenario %in% c(scenario_baseline, scenario_landint),
    variable %in% c("PROD", "CONS"),
    item %in% c("AGR", "CRP", "LSP"),
    unit == "million kcal"
  ) %>%
  transmute(region, item, scenario, variable, value) %>%
  unite("series", variable, scenario, sep = "__") %>%
  pivot_wider(names_from = series, values_from = value) %>%
  mutate(
    self_bau = if_else(.data[[paste0("CONS__", scenario_baseline)]] != 0, 100 * .data[[paste0("PROD__", scenario_baseline)]] / .data[[paste0("CONS__", scenario_baseline)]], NA_real_),
    self_landint = if_else(.data[[paste0("CONS__", scenario_landint)]] != 0, 100 * .data[[paste0("PROD__", scenario_landint)]] / .data[[paste0("CONS__", scenario_landint)]], NA_real_),
    change = self_landint - self_bau
  ) %>%
  left_join(st_drop_geometry(Boundary), by = c("region" = "cty")) %>%
  mutate(country = factor(coalesce(country_name, region), levels = afolu_top_order_2100))

self_suff_stack_2100 <- self_suff_change_2100 %>%
  filter(item %in% c("CRP", "LSP")) %>%
  mutate(component = recode(item, CRP = "CRP", LSP = "LSP"))

self_suff_point_2100 <- self_suff_change_2100 %>%
  filter(item == "AGR") %>%
  transmute(country, total_change = change)

p_self_suff_change_2100 <- ggplot(self_suff_stack_2100, aes(x = change, y = country, fill = component)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
  geom_col() +
  geom_point(
    data = self_suff_point_2100,
    aes(x = total_change, y = country),
    inherit.aes = FALSE,
    shape = 21,
    size = 2.2,
    stroke = 0.35,
    fill = "black",
    color = "black"
  ) +
  scale_fill_manual(values = c("CRP" = "#5B8E7D", "LSP" = "#D98C5F")) +
  labs(
    title = "Food self-sufficiency change in top AFOLU reduction countries (2100)",
    x = "percentage points from BaU",
    y = NULL
  ) +
  theme_main

save_plot(
  "figA65_top_afolu_reduction_self_sufficiency_change_2100.png",
  p_self_suff_change_2100,
  width = 11.5,
  height = 8
)

afolu_bubble_core <- afolu_country %>%
  group_by(region, year) %>%
  summarise(afolu_reduction = sum(value, na.rm = TRUE), .groups = "drop") %>%
  filter(region %in% country_codes, year %in% plot_years) %>%
  left_join(region_map, by = "region") %>%
  left_join(st_drop_geometry(Boundary), by = c("region" = "cty")) %>%
  mutate(
    country = coalesce(country_name, region),
    region5 = factor(region5, levels = names(region_palette))
  )

afolu_bubble_size <- function(df) {
  df %>%
    mutate(
      size_bin = cut(
        afolu_reduction,
        breaks = c(-Inf, 5, 10, 15, 20, Inf),
        include.lowest = TRUE,
        right = FALSE,
        labels = c("0-5 MtCO2eq/yr", "5-10 MtCO2eq/yr", "10-15 MtCO2eq/yr", "15-20 MtCO2eq/yr", "20+ MtCO2eq/yr")
      )
    )
}

make_afolu_bubble_plot <- function(plot_year, metric_col, x_label, title_text) {
  x_df <- switch(
    metric_col,
    land_tot = GEER_AGMIP %>%
      filter(year == plot_year, region %in% country_codes, scenario == scenario_baseline, variable == "LAND", item == "TOT", unit == "1000 ha") %>%
      transmute(region, year, x = value),
    popt_tot = GEER_AGMIP %>%
      filter(year == plot_year, region %in% country_codes, scenario == scenario_baseline, variable == "POPT", item == "TOT", unit == "million") %>%
      transmute(region, year, x = value),
    gdpt_tot = GEER_AGMIP %>%
      filter(year == plot_year, region %in% country_codes, scenario == scenario_baseline, variable == "GDPT", item == "TOT", unit == "bn USD 2017 PPP") %>%
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
    pop_density = GEER_AGMIP %>%
      filter(
        year == plot_year,
        region %in% country_codes,
        scenario == scenario_baseline,
        (variable == "POPT" & item == "TOT" & unit == "million") |
          (variable == "LAND" & item == "TOT" & unit == "1000 ha")
      ) %>%
      select(region, year, variable, value) %>%
      pivot_wider(names_from = variable, values_from = value) %>%
      transmute(region, year, x = if_else(LAND > 0, 1000 * POPT / LAND, NA_real_)),
    self_suff_bau = GEER_AGMIP %>%
      filter(year == plot_year, region %in% country_codes, scenario == scenario_baseline, variable %in% c("PROD", "CONS"), item == "AGR", unit == "million kcal") %>%
      select(region, year, variable, value) %>%
      pivot_wider(names_from = variable, values_from = value) %>%
      transmute(region, year, x = if_else(CONS > 0, 100 * PROD / CONS, NA_real_)),
    cali_agr = GEER_AGMIP %>%
      filter(
        year == plot_year,
        region %in% country_codes,
        scenario %in% c(scenario_baseline, scenario_landint),
        variable == "CALI",
        item == "AGR",
        unit == "kcal/cap/d"
      ) %>%
      select(region, year, scenario, value) %>%
      pivot_wider(names_from = scenario, values_from = value) %>%
      transmute(region, year, x = .data[[scenario_landint]] - .data[[scenario_baseline]]),
    land_for = agmip_land_long %>%
      filter(year == plot_year, region %in% country_codes, land_item == "Managed and primary forest") %>%
      transmute(region, year, x = land_change),
    xprp_agr = GEER_AGMIP %>%
      filter(
        year == plot_year,
        region %in% country_codes,
        scenario %in% c(scenario_baseline, scenario_landint),
        variable == "XPRP",
        item == "AGR",
        unit == "Index (2015 = 1)"
      ) %>%
      select(region, year, scenario, value) %>%
      pivot_wider(names_from = scenario, values_from = value) %>%
      transmute(region, year, x = if_else(.data[[scenario_baseline]] != 0, 100 * (.data[[scenario_landint]] / .data[[scenario_baseline]] - 1), NA_real_)),
    self_suff_agr = GEER_AGMIP %>%
      filter(
        year == plot_year,
        region %in% country_codes,
        scenario %in% c(scenario_baseline, scenario_landint),
        variable %in% c("PROD", "CONS"),
        item == "AGR",
        unit == "million kcal"
      ) %>%
      transmute(region, year, scenario, variable, value) %>%
      unite("series", variable, scenario, sep = "__") %>%
      pivot_wider(names_from = series, values_from = value) %>%
      transmute(
        region,
        year,
        x = if_else(.data[[paste0("CONS__", scenario_landint)]] != 0 & .data[[paste0("CONS__", scenario_baseline)]] != 0,
          100 * .data[[paste0("PROD__", scenario_landint)]] / .data[[paste0("CONS__", scenario_landint)]] -
            100 * .data[[paste0("PROD__", scenario_baseline)]] / .data[[paste0("CONS__", scenario_baseline)]],
          NA_real_
        )
      ),
    prod_crp = GEER_AGMIP %>%
      filter(
        year == plot_year,
        region %in% country_codes,
        scenario %in% c(scenario_baseline, scenario_landint),
        variable == "PROD",
        item == "CRP",
        unit == "million kcal"
      ) %>%
      select(region, year, scenario, value) %>%
      pivot_wider(names_from = scenario, values_from = value) %>%
      transmute(region, year, x = .data[[scenario_landint]] - .data[[scenario_baseline]]),
    prod_lsp = GEER_AGMIP %>%
      filter(
        year == plot_year,
        region %in% country_codes,
        scenario %in% c(scenario_baseline, scenario_landint),
        variable == "PROD",
        item == "LSP",
        unit == "million kcal"
      ) %>%
      select(region, year, scenario, value) %>%
      pivot_wider(names_from = scenario, values_from = value) %>%
      transmute(region, year, x = .data[[scenario_landint]] - .data[[scenario_baseline]])
  )

  bubble_df <- afolu_bubble_core %>%
    filter(year == plot_year) %>%
    left_join(x_df, by = c("region", "year")) %>%
    afolu_bubble_size() %>%
    filter(is.finite(afolu_reduction), is.finite(x), !is.na(region5))

  label_df <- bubble_df %>% filter(size_bin %in% c("15-20 MtCO2eq/yr", "20+ MtCO2eq/yr"))

  ggplot(bubble_df, aes(x = x, y = afolu_reduction)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
    geom_point(aes(size = size_bin, fill = region5, color = region5, alpha = size_bin), shape = 21, stroke = 0.35) +
    geom_text(
      data = label_df,
      aes(label = country, color = region5),
      size = 3.2,
      fontface = "bold",
      nudge_y = 0.03 * max(bubble_df$afolu_reduction, na.rm = TRUE),
      show.legend = FALSE
    ) +
    scale_fill_manual(values = region_palette) +
    scale_color_manual(values = region_border_palette) +
    scale_size_manual(values = c(2.5, 4, 5.5, 7, 8.5), drop = FALSE) +
    scale_alpha_manual(values = c("0-5 MtCO2eq/yr" = 0.56, "5-10 MtCO2eq/yr" = 0.62, "10-15 MtCO2eq/yr" = 0.68, "15-20 MtCO2eq/yr" = 0.74, "20+ MtCO2eq/yr" = 0.80), guide = "none", drop = FALSE) +
    labs(title = paste0(title_text, " (", plot_year, ")"), x = x_label, y = "AFOLU actual reduction (MtCO2eq/yr)") +
    theme_main
}

save_plot("figA66_afolu_bubble_cali_agr_2050_2100.png", wrap_plots(
  make_afolu_bubble_plot(2050, "cali_agr", "CALI AGR change (kcal/cap/d)", "AFOLU reduction vs calorie change"),
  make_afolu_bubble_plot(2100, "cali_agr", "CALI AGR change (kcal/cap/d)", "AFOLU reduction vs calorie change"),
  ncol = 2
), width = 12, height = 6)

save_plot("figA67_afolu_bubble_land_for_2050_2100.png", wrap_plots(
  make_afolu_bubble_plot(2050, "land_for", "LAND FOR change (1000 ha)", "AFOLU reduction vs forest area change"),
  make_afolu_bubble_plot(2100, "land_for", "LAND FOR change (1000 ha)", "AFOLU reduction vs forest area change"),
  ncol = 2
), width = 12, height = 6)

save_plot("figA68_afolu_bubble_xprp_agr_2050_2100.png", wrap_plots(
  make_afolu_bubble_plot(2050, "xprp_agr", "XPRP AGR change (%)", "AFOLU reduction vs AGR price change"),
  make_afolu_bubble_plot(2100, "xprp_agr", "XPRP AGR change (%)", "AFOLU reduction vs AGR price change"),
  ncol = 2
), width = 12, height = 6)

save_plot("figA69_afolu_bubble_self_sufficiency_2050_2100.png", wrap_plots(
  make_afolu_bubble_plot(2050, "self_suff_agr", "Self-sufficiency change (pp)", "AFOLU reduction vs self-sufficiency change"),
  make_afolu_bubble_plot(2100, "self_suff_agr", "Self-sufficiency change (pp)", "AFOLU reduction vs self-sufficiency change"),
  ncol = 2
), width = 12, height = 6)

save_plot("figA70_afolu_bubble_prod_crp_2050_2100.png", wrap_plots(
  make_afolu_bubble_plot(2050, "prod_crp", "PROD CRP change (million kcal)", "AFOLU reduction vs crop production change"),
  make_afolu_bubble_plot(2100, "prod_crp", "PROD CRP change (million kcal)", "AFOLU reduction vs crop production change"),
  ncol = 2
), width = 12, height = 6)

save_plot("figA71_afolu_bubble_prod_lsp_2050_2100.png", wrap_plots(
  make_afolu_bubble_plot(2050, "prod_lsp", "PROD LSP change (million kcal)", "AFOLU reduction vs livestock production change"),
  make_afolu_bubble_plot(2100, "prod_lsp", "PROD LSP change (million kcal)", "AFOLU reduction vs livestock production change"),
  ncol = 2
), width = 12, height = 6)

save_plot("figA78_afolu_bubble_land_tot_2050_2100.png", wrap_plots(
  make_afolu_bubble_plot(2050, "land_tot", "LAND TOT (1000 ha, BaU)", "AFOLU reduction vs land area"),
  make_afolu_bubble_plot(2100, "land_tot", "LAND TOT (1000 ha, BaU)", "AFOLU reduction vs land area"),
  ncol = 2
), width = 12, height = 6)

save_plot("figA79_afolu_bubble_popt_tot_2050_2100.png", wrap_plots(
  make_afolu_bubble_plot(2050, "popt_tot", "POPT TOT (million, BaU)", "AFOLU reduction vs population"),
  make_afolu_bubble_plot(2100, "popt_tot", "POPT TOT (million, BaU)", "AFOLU reduction vs population"),
  ncol = 2
), width = 12, height = 6)

save_plot("figA80_afolu_bubble_gdpt_tot_2050_2100.png", wrap_plots(
  make_afolu_bubble_plot(2050, "gdpt_tot", "GDPT TOT (bn USD 2017 PPP, BaU)", "AFOLU reduction vs GDP"),
  make_afolu_bubble_plot(2100, "gdpt_tot", "GDPT TOT (bn USD 2017 PPP, BaU)", "AFOLU reduction vs GDP"),
  ncol = 2
), width = 12, height = 6)

save_plot("figA81_afolu_bubble_gdppc_2050_2100.png", wrap_plots(
  make_afolu_bubble_plot(2050, "gdppc", "GDP per capita (1000 USD 2017 PPP, BaU)", "AFOLU reduction vs GDP per capita"),
  make_afolu_bubble_plot(2100, "gdppc", "GDP per capita (1000 USD 2017 PPP, BaU)", "AFOLU reduction vs GDP per capita"),
  ncol = 2
), width = 12, height = 6)

save_plot("figA82_afolu_bubble_self_suff_bau_2050_2100.png", wrap_plots(
  make_afolu_bubble_plot(2050, "self_suff_bau", "AGR self-sufficiency (% , BaU)", "AFOLU reduction vs self-sufficiency"),
  make_afolu_bubble_plot(2100, "self_suff_bau", "AGR self-sufficiency (% , BaU)", "AFOLU reduction vs self-sufficiency"),
  ncol = 2
), width = 12, height = 6)

save_plot("figA88_afolu_bubble_pop_density_2050_2100.png", wrap_plots(
  make_afolu_bubble_plot(2050, "pop_density", "Population density (persons/ha, BaU)", "AFOLU reduction vs population density"),
  make_afolu_bubble_plot(2100, "pop_density", "Population density (persons/ha, BaU)", "AFOLU reduction vs population density"),
  ncol = 2
), width = 12, height = 6)

make_afolu_rate_bubble_plot <- function(plot_year, metric_col, x_label, title_text) {
  x_df <- switch(
    metric_col,
    land_tot = GEER_AGMIP %>%
      filter(year == plot_year, region %in% country_codes, scenario == scenario_baseline, variable == "LAND", item == "TOT", unit == "1000 ha") %>%
      transmute(region, year, x = value),
    popt_tot = GEER_AGMIP %>%
      filter(year == plot_year, region %in% country_codes, scenario == scenario_baseline, variable == "POPT", item == "TOT", unit == "million") %>%
      transmute(region, year, x = value),
    gdpt_tot = GEER_AGMIP %>%
      filter(year == plot_year, region %in% country_codes, scenario == scenario_baseline, variable == "GDPT", item == "TOT", unit == "bn USD 2017 PPP") %>%
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
    pop_density = GEER_AGMIP %>%
      filter(
        year == plot_year,
        region %in% country_codes,
        scenario == scenario_baseline,
        (variable == "POPT" & item == "TOT" & unit == "million") |
          (variable == "LAND" & item == "TOT" & unit == "1000 ha")
      ) %>%
      select(region, year, variable, value) %>%
      pivot_wider(names_from = variable, values_from = value) %>%
      transmute(region, year, x = if_else(LAND > 0, 1000 * POPT / LAND, NA_real_)),
    self_suff_bau = GEER_AGMIP %>%
      filter(year == plot_year, region %in% country_codes, scenario == scenario_baseline, variable %in% c("PROD", "CONS"), item == "AGR", unit == "million kcal") %>%
      select(region, year, variable, value) %>%
      pivot_wider(names_from = variable, values_from = value) %>%
      transmute(region, year, x = if_else(CONS > 0, 100 * PROD / CONS, NA_real_)),
    cali_agr = GEER_AGMIP %>%
      filter(year == plot_year, region %in% country_codes, scenario %in% c(scenario_baseline, scenario_landint), variable == "CALI", item == "AGR", unit == "kcal/cap/d") %>%
      select(region, year, scenario, value) %>%
      pivot_wider(names_from = scenario, values_from = value) %>%
      transmute(region, year, x = .data[[scenario_landint]] - .data[[scenario_baseline]]),
    land_for = agmip_land_long %>%
      filter(year == plot_year, region %in% country_codes, land_item == "Managed and primary forest") %>%
      transmute(region, year, x = land_change),
    xprp_agr = GEER_AGMIP %>%
      filter(year == plot_year, region %in% country_codes, scenario %in% c(scenario_baseline, scenario_landint), variable == "XPRP", item == "AGR", unit == "Index (2015 = 1)") %>%
      select(region, year, scenario, value) %>%
      pivot_wider(names_from = scenario, values_from = value) %>%
      transmute(region, year, x = if_else(.data[[scenario_baseline]] != 0, 100 * (.data[[scenario_landint]] / .data[[scenario_baseline]] - 1), NA_real_)),
    self_suff_agr = GEER_AGMIP %>%
      filter(year == plot_year, region %in% country_codes, scenario %in% c(scenario_baseline, scenario_landint), variable %in% c("PROD", "CONS"), item == "AGR", unit == "million kcal") %>%
      transmute(region, year, scenario, variable, value) %>%
      unite("series", variable, scenario, sep = "__") %>%
      pivot_wider(names_from = series, values_from = value) %>%
      transmute(region, year, x = if_else(.data[[paste0("CONS__", scenario_landint)]] != 0 & .data[[paste0("CONS__", scenario_baseline)]] != 0,
        100 * .data[[paste0("PROD__", scenario_landint)]] / .data[[paste0("CONS__", scenario_landint)]] -
          100 * .data[[paste0("PROD__", scenario_baseline)]] / .data[[paste0("CONS__", scenario_baseline)]], NA_real_)),
    prod_crp = GEER_AGMIP %>%
      filter(year == plot_year, region %in% country_codes, scenario %in% c(scenario_baseline, scenario_landint), variable == "PROD", item == "CRP", unit == "million kcal") %>%
      select(region, year, scenario, value) %>%
      pivot_wider(names_from = scenario, values_from = value) %>%
      transmute(region, year, x = .data[[scenario_landint]] - .data[[scenario_baseline]]),
    prod_lsp = GEER_AGMIP %>%
      filter(year == plot_year, region %in% country_codes, scenario %in% c(scenario_baseline, scenario_landint), variable == "PROD", item == "LSP", unit == "million kcal") %>%
      select(region, year, scenario, value) %>%
      pivot_wider(names_from = scenario, values_from = value) %>%
      transmute(region, year, x = .data[[scenario_landint]] - .data[[scenario_baseline]])
  )

  bubble_df <- afolu_rate_country %>%
    filter(year == plot_year, region %in% country_codes) %>%
    left_join(
      afolu_country %>%
        group_by(region, year) %>%
        summarise(afolu_reduction = sum(value, na.rm = TRUE), .groups = "drop"),
      by = c("region", "year")
    ) %>%
    left_join(region_map, by = "region") %>%
    left_join(st_drop_geometry(Boundary), by = c("region" = "cty")) %>%
    mutate(country = coalesce(country_name, region), region5 = factor(region5, levels = names(region_palette))) %>%
    left_join(x_df, by = c("region", "year")) %>%
    afolu_bubble_size() %>%
    filter(is.finite(reduction_rate), is.finite(x), !is.na(region5))

  label_df <- bubble_df %>% filter(size_bin %in% c("15-20 MtCO2eq/yr", "20+ MtCO2eq/yr"))

  ggplot(bubble_df, aes(x = x, y = reduction_rate)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
    geom_point(aes(size = size_bin, fill = region5, color = region5, alpha = size_bin), shape = 21, stroke = 0.35) +
    geom_text(
      data = label_df,
      aes(label = country, color = region5),
      size = 3.2,
      fontface = "bold",
      nudge_y = 0.03 * max(abs(bubble_df$reduction_rate), na.rm = TRUE),
      show.legend = FALSE
    ) +
    scale_fill_manual(values = region_palette) +
    scale_color_manual(values = region_border_palette) +
    scale_size_manual(values = c(2.5, 4, 5.5, 7, 8.5), drop = FALSE) +
    scale_alpha_manual(values = c("0-5 MtCO2eq/yr" = 0.56, "5-10 MtCO2eq/yr" = 0.62, "10-15 MtCO2eq/yr" = 0.68, "15-20 MtCO2eq/yr" = 0.74, "20+ MtCO2eq/yr" = 0.80), guide = "none", drop = FALSE) +
    labs(title = paste0(title_text, " (", plot_year, ")"), x = x_label, y = "AFOLU reduction rate (% of BaU emissions)") +
    theme_main
}

save_plot("figA72_afolu_rate_bubble_cali_agr_2050_2100.png", wrap_plots(
  make_afolu_rate_bubble_plot(2050, "cali_agr", "CALI AGR change (kcal/cap/d)", "AFOLU reduction rate vs calorie change"),
  make_afolu_rate_bubble_plot(2100, "cali_agr", "CALI AGR change (kcal/cap/d)", "AFOLU reduction rate vs calorie change"),
  ncol = 2
), width = 12, height = 6)

save_plot("figA73_afolu_rate_bubble_land_for_2050_2100.png", wrap_plots(
  make_afolu_rate_bubble_plot(2050, "land_for", "LAND FOR change (1000 ha)", "AFOLU reduction rate vs forest area change"),
  make_afolu_rate_bubble_plot(2100, "land_for", "LAND FOR change (1000 ha)", "AFOLU reduction rate vs forest area change"),
  ncol = 2
), width = 12, height = 6)

save_plot("figA74_afolu_rate_bubble_xprp_agr_2050_2100.png", wrap_plots(
  make_afolu_rate_bubble_plot(2050, "xprp_agr", "XPRP AGR change (%)", "AFOLU reduction rate vs AGR price change"),
  make_afolu_rate_bubble_plot(2100, "xprp_agr", "XPRP AGR change (%)", "AFOLU reduction rate vs AGR price change"),
  ncol = 2
), width = 12, height = 6)

save_plot("figA75_afolu_rate_bubble_self_sufficiency_2050_2100.png", wrap_plots(
  make_afolu_rate_bubble_plot(2050, "self_suff_agr", "Self-sufficiency change (pp)", "AFOLU reduction rate vs self-sufficiency change"),
  make_afolu_rate_bubble_plot(2100, "self_suff_agr", "Self-sufficiency change (pp)", "AFOLU reduction rate vs self-sufficiency change"),
  ncol = 2
), width = 12, height = 6)

save_plot("figA76_afolu_rate_bubble_prod_crp_2050_2100.png", wrap_plots(
  make_afolu_rate_bubble_plot(2050, "prod_crp", "PROD CRP change (million kcal)", "AFOLU reduction rate vs crop production change"),
  make_afolu_rate_bubble_plot(2100, "prod_crp", "PROD CRP change (million kcal)", "AFOLU reduction rate vs crop production change"),
  ncol = 2
), width = 12, height = 6)

save_plot("figA77_afolu_rate_bubble_prod_lsp_2050_2100.png", wrap_plots(
  make_afolu_rate_bubble_plot(2050, "prod_lsp", "PROD LSP change (million kcal)", "AFOLU reduction rate vs livestock production change"),
  make_afolu_rate_bubble_plot(2100, "prod_lsp", "PROD LSP change (million kcal)", "AFOLU reduction rate vs livestock production change"),
  ncol = 2
), width = 12, height = 6)

save_plot("figA83_afolu_rate_bubble_land_tot_2050_2100.png", wrap_plots(
  make_afolu_rate_bubble_plot(2050, "land_tot", "LAND TOT (1000 ha, BaU)", "AFOLU reduction rate vs land area"),
  make_afolu_rate_bubble_plot(2100, "land_tot", "LAND TOT (1000 ha, BaU)", "AFOLU reduction rate vs land area"),
  ncol = 2
), width = 12, height = 6)

save_plot("figA84_afolu_rate_bubble_popt_tot_2050_2100.png", wrap_plots(
  make_afolu_rate_bubble_plot(2050, "popt_tot", "POPT TOT (million, BaU)", "AFOLU reduction rate vs population"),
  make_afolu_rate_bubble_plot(2100, "popt_tot", "POPT TOT (million, BaU)", "AFOLU reduction rate vs population"),
  ncol = 2
), width = 12, height = 6)

save_plot("figA85_afolu_rate_bubble_gdpt_tot_2050_2100.png", wrap_plots(
  make_afolu_rate_bubble_plot(2050, "gdpt_tot", "GDPT TOT (bn USD 2017 PPP, BaU)", "AFOLU reduction rate vs GDP"),
  make_afolu_rate_bubble_plot(2100, "gdpt_tot", "GDPT TOT (bn USD 2017 PPP, BaU)", "AFOLU reduction rate vs GDP"),
  ncol = 2
), width = 12, height = 6)

save_plot("figA86_afolu_rate_bubble_gdppc_2050_2100.png", wrap_plots(
  make_afolu_rate_bubble_plot(2050, "gdppc", "GDP per capita (1000 USD 2017 PPP, BaU)", "AFOLU reduction rate vs GDP per capita"),
  make_afolu_rate_bubble_plot(2100, "gdppc", "GDP per capita (1000 USD 2017 PPP, BaU)", "AFOLU reduction rate vs GDP per capita"),
  ncol = 2
), width = 12, height = 6)

save_plot("figA87_afolu_rate_bubble_self_suff_bau_2050_2100.png", wrap_plots(
  make_afolu_rate_bubble_plot(2050, "self_suff_bau", "AGR self-sufficiency (% , BaU)", "AFOLU reduction rate vs self-sufficiency"),
  make_afolu_rate_bubble_plot(2100, "self_suff_bau", "AGR self-sufficiency (% , BaU)", "AFOLU reduction rate vs self-sufficiency"),
  ncol = 2
), width = 12, height = 6)

save_plot("figA89_afolu_rate_bubble_pop_density_2050_2100.png", wrap_plots(
  make_afolu_rate_bubble_plot(2050, "pop_density", "Population density (persons/ha, BaU)", "AFOLU reduction rate vs population density"),
  make_afolu_rate_bubble_plot(2100, "pop_density", "Population density (persons/ha, BaU)", "AFOLU reduction rate vs population density"),
  ncol = 2
), width = 12, height = 6)

make_afolu_prod_price_bubble <- function(plot_year, prod_item, price_item, item_label) {
  price_df <- GEER_AGMIP %>%
    filter(
      year == plot_year,
      region %in% country_codes,
      scenario == scenario_baseline,
      variable == "XPRP",
      item == price_item,
      unit == "USD/t"
    ) %>%
    transmute(region, year, price_usd = value)

  prod_df <- GEER_AGMIP %>%
    filter(
      year == plot_year,
      region %in% country_codes,
      scenario %in% c(scenario_baseline, scenario_landint),
      variable == "PROD",
      item == prod_item,
      unit == "million kcal"
    ) %>%
    select(region, year, scenario, value) %>%
    pivot_wider(names_from = scenario, values_from = value) %>%
    transmute(region, year, prod_change = if_else(.data[[scenario_baseline]] != 0, 100 * (.data[[scenario_landint]] / .data[[scenario_baseline]] - 1), NA_real_))

  bubble_df <- afolu_bubble_core %>%
    filter(year == plot_year) %>%
    left_join(price_df, by = c("region", "year")) %>%
    left_join(prod_df, by = c("region", "year")) %>%
    afolu_bubble_size() %>%
    filter(is.finite(afolu_reduction), is.finite(price_usd), is.finite(prod_change), !is.na(region5))

  label_df <- bubble_df %>% filter(size_bin %in% c("15-20 MtCO2eq/yr", "20+ MtCO2eq/yr"))

  ggplot(bubble_df, aes(x = price_usd, y = prod_change)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
    geom_point(aes(size = size_bin, fill = region5, color = region5, alpha = size_bin), shape = 21, stroke = 0.35) +
    geom_text(
      data = label_df,
      aes(label = country, color = region5),
      size = 3.2,
      fontface = "bold",
      nudge_y = 0.03 * max(abs(bubble_df$prod_change), na.rm = TRUE),
      show.legend = FALSE
    ) +
    scale_fill_manual(values = region_palette) +
    scale_color_manual(values = region_border_palette) +
    scale_size_manual(values = c(2.5, 4, 5.5, 7, 8.5), drop = FALSE) +
    scale_alpha_manual(values = c("0-5 MtCO2eq/yr" = 0.56, "5-10 MtCO2eq/yr" = 0.62, "10-15 MtCO2eq/yr" = 0.68, "15-20 MtCO2eq/yr" = 0.74, "20+ MtCO2eq/yr" = 0.80), guide = "none", drop = FALSE) +
    labs(
      title = paste0("AFOLU reduction vs ", item_label, " price and production (", plot_year, ")"),
      x = paste0("BaU XPRP ", item_label, " (USD/t)"),
      y = paste0("PROD ", item_label, " change (%)")
    ) +
    theme_main
}

save_plot("figA90_afolu_bubble_prod_change_vs_price_agr_2050_2100.png", wrap_plots(
  make_afolu_prod_price_bubble(2050, "AGR", "AGR", "AGR"),
  make_afolu_prod_price_bubble(2100, "AGR", "AGR", "AGR"),
  ncol = 2
), width = 12, height = 6)

save_plot("figA91_afolu_bubble_prod_change_vs_price_crp_2050_2100.png", wrap_plots(
  make_afolu_prod_price_bubble(2050, "CRP", "CRP", "CRP"),
  make_afolu_prod_price_bubble(2100, "CRP", "CRP", "CRP"),
  ncol = 2
), width = 12, height = 6)

save_plot("figA92_afolu_bubble_prod_change_vs_price_lsp_2050_2100.png", wrap_plots(
  make_afolu_prod_price_bubble(2050, "LSP", "LSP", "LSP"),
  make_afolu_prod_price_bubble(2100, "LSP", "LSP", "LSP"),
  ncol = 2
), width = 12, height = 6)

land_top_afr_2100 <- land_change_plus_mitigation_afr %>%
  filter(year == 2100, region %in% country_codes) %>%
  group_by(region) %>%
  summarise(total_value = sum(plot_value, na.rm = TRUE), .groups = "drop") %>%
  arrange(total_value) %>%
  slice_head(n = 15) %>%
  left_join(st_drop_geometry(Boundary), by = c("region" = "cty")) %>%
  transmute(region, country = coalesce(country_name, region), total_value)

land_top_order_2100 <- rev(land_top_afr_2100$country)

p_land_change_plus_mitigation_afr_2100 <- make_land_change_plus_mitigation_top_panel_from(
  land_change_plus_mitigation_afr,
  2100,
  "Land change plus mitigation in top countries, AFR sink version"
)

land_change_afr_2100_abs <- agmip_land_long %>%
  filter(year == 2100, region %in% land_top_afr_2100$region) %>%
  left_join(land_top_afr_2100, by = "region") %>%
  mutate(country = factor(country, levels = land_top_order_2100))

land_total_area_2100 <- GEER_AGMIP %>%
  filter(
    region %in% land_top_afr_2100$region,
    year == 2100,
    scenario == scenario_baseline,
    variable == "LAND",
    item == "TOT",
    unit == "1000 ha"
  ) %>%
  group_by(region, year) %>%
  summarise(total_land_kha = sum(value, na.rm = TRUE), .groups = "drop")

land_change_plus_mitigation_afr_2100_intensity <- land_change_plus_mitigation_afr %>%
  filter(year == 2100, region %in% land_top_afr_2100$region) %>%
  left_join(land_total_area_2100, by = c("region", "year")) %>%
  left_join(land_top_afr_2100, by = "region") %>%
  mutate(
    country = factor(country, levels = land_top_order_2100),
    plot_value_intensity = if_else(is.finite(total_land_kha) & total_land_kha > 0, plot_value / total_land_kha * 1000, NA_real_)
  )

p_land_change_afr_2100_abs <- ggplot(land_change_afr_2100_abs, aes(x = land_change, y = country, fill = land_item)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
  geom_col() +
  scale_fill_manual(values = land_color) +
  labs(title = "Land change from BaU (2100)", x = "1000 ha", y = NULL) +
  theme_main

p_land_change_afr_2100_share <- ggplot(land_change_afr_2100_abs, aes(x = land_share, y = country, fill = land_item)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
  geom_col() +
  scale_fill_manual(values = land_color) +
  labs(title = "Land change from BaU (2100)", x = "% of total land area", y = NULL) +
  theme_main

p_land_change_plus_mitigation_afr_2100_intensity <- ggplot(land_change_plus_mitigation_afr_2100_intensity, aes(x = plot_value_intensity, y = country, fill = component)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
  geom_col() +
  geom_point(
    data = land_change_plus_mitigation_afr_2100_intensity %>%
      group_by(country) %>%
      summarise(total_value = sum(plot_value_intensity, na.rm = TRUE), .groups = "drop"),
    aes(x = total_value, y = country),
    inherit.aes = FALSE,
    shape = 21,
    size = 2.2,
    stroke = 0.35,
    fill = "black",
    color = "black"
  ) +
  scale_fill_manual(values = land_change_tech_palette) +
  labs(title = "Land change plus mitigation in top countries (2100)", x = "tCO2eq/ha/yr", y = NULL) +
  theme_main

save_plot(
  "figA51c_land_change_plus_mitigation_top_countries_afr_with_land_abs_2100.png",
  wrap_plots(p_land_change_plus_mitigation_afr_2100, p_land_change_afr_2100_abs, ncol = 2, widths = c(1, 1.4)),
  width = 16,
  height = 8.5
)

save_plot(
  "figA51d_land_change_plus_mitigation_top_countries_afr_with_land_share_2100.png",
  wrap_plots(p_land_change_plus_mitigation_afr_2100, p_land_change_afr_2100_share, ncol = 2, widths = c(1, 1.4)),
  width = 16,
  height = 8.5
)

save_plot(
  "figA51e_land_change_plus_mitigation_top_countries_afr_per_land_with_land_abs_2100.png",
  wrap_plots(p_land_change_plus_mitigation_afr_2100_intensity, p_land_change_afr_2100_abs, ncol = 2, widths = c(1, 1.4)),
  width = 16,
  height = 8.5
)

save_plot(
  "figA51f_land_change_plus_mitigation_top_countries_afr_per_land_with_land_share_2100.png",
  wrap_plots(p_land_change_plus_mitigation_afr_2100_intensity, p_land_change_afr_2100_share, ncol = 2, widths = c(1, 1.4)),
  width = 16,
  height = 8.5
)
