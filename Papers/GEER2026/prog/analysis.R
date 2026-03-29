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
  candidate_dirs <- candidate_dirs[file.exists(file.path(candidate_dirs, "analysis.R"))]
  if (length(candidate_dirs) == 0) {
    stop(
      "Could not determine script_dir. Open analysis.R from Papers/GEER2026/prog/ ",
      "or set the working directory to Papers/GEER2026 before sourcing."
    )
  }
  normalizePath(candidate_dirs[[1]], winslash = "/", mustWork = TRUE)
}

base_dir <- normalizePath(file.path(script_dir, ".."), winslash = "/", mustWork = TRUE)
data_dir <- file.path(base_dir, "data", "0316")
define_dir <- file.path(base_dir, "define")
output_dir <- file.path(base_dir, "output", "analysis")
country_set_file <- file.path(define_dir, "country.set")
region_map_file <- file.path(define_dir, "region.map")
riso_top100_file <- file.path(define_dir, "RISO_top100fao.txt")

scenario_baseline <- "SSP2_BaU_NoCC_No"
scenario_landint <- "SSP2_1100C_CACN_DAC_NoCC_LandInt_global1"
scenario_no <- "SSP2_1100C_CACN_DAC_NoCC_No"
scenario_afolub_default <- scenario_no
scenario_baseline_landint <- "SSP2_BaU_NoCC_LandInt_global1"
target_year <- 2050
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

safe_pull <- function(df, name) {
  if (name %in% names(df)) {
    return(df[[name]])
  }
  rep(NA_real_, nrow(df))
}

save_plot <- function(filename, plot_obj, width = 8, height = 5, dpi = 300) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  out_path <- file.path(output_dir, filename)
  ggsave(out_path, plot = plot_obj, width = width, height = height, dpi = dpi, bg = "white", limitsize = FALSE)
  message("Saved: ", out_path)
}

scenario_color <- c(
  "SSP2-BaU" = "#3873AD",
  "SSP2-LandInt" = "#C3327A",
  "SSP2-No" = "#8C8C8C"
)

component_palette <- c(
  "Land use" = "#A5D6A7",
  "Agriculture" = "#F6C667"
)

category_palette <- c(
  "Agriculture" = "#F4B400",
  "LULUCF (CDR)" = "#A5D6A7",
  "LULUCF (other)" = "#90CAF9",
  "Enteric" = "#FFB74D",
  "Manure" = "#F48FB1",
  "Rice" = "#FFF176",
  "Managed soils" = "#CE93D8",
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
  "OECD+EU" = "#C85E6C",
  "Asia" = "#4B8DD8",
  "Africa/Middle East" = "#5E9E4C",
  "Latin America" = "#C98D3D",
  "Former Soviet Union" = "#8668C9"
)

short_country_name <- function(x) {
  dplyr::recode(
    x,
    "United States of America" = "United States",
    "Russian Federation" = "Russia",
    "Democratic Republic of the Congo" = "DR Congo",
    "Republic of the Congo" = "Congo",
    "Iran" = "Iran",
    "Viet Nam" = "Vietnam",
    "Lao PDR" = "Laos",
    "Syrian Arab Republic" = "Syria",
    "United Republic of Tanzania" = "Tanzania",
    "Bolivia" = "Bolivia",
    "Venezuela" = "Venezuela",
    "Korea, Republic of" = "South Korea",
    "Korea, Democratic People's Republic of" = "North Korea",
    "Czech Republic" = "Czechia",
    "Macedonia" = "North Macedonia",
    "Myanmar" = "Myanmar",
    "Brunei Darussalam" = "Brunei",
    "Côte d'Ivoire" = "Cote d'Ivoire",
    .default = x
  )
}

afolub_broad_group <- function(variable) {
  case_when(
    grepl("^Emi_Red_equ_", variable) ~ "Agriculture",
    variable %in% c("Emi_Red_CO2_AFO_Lan_Pla", "Emi_Red_CO2_AFO_Lan_Ref", "Emi_Red_CO2_AFO_Lan_Agf", "Emi_Red_CO2_AFO_Lan_Bch", "Emi_Red_CO2_AFO_Lan_Scs", "Emi_Red_CO2_AFO_Lan_Scg", "Emi_Red_CO2_AFO_Lan_Scc", "Car_Seq_Lan_Use_Soi_Car_Man", "Car_Seq_Lan_Use_Soi_Car_Man_Cro", "Car_Seq_Lan_Use_Soi_Car_Man_Gra", "Emi_CO2_AFO_Aff") ~ "LULUCF (CDR)",
    grepl("^Emi_Red_CO2_AFO_Lan_", variable) ~ "LULUCF (other)",
    TRUE ~ NA_character_
  )
}

afolub_sub_group <- function(variable) {
  case_when(
    grepl("Liv_Ent_Fer", variable) ~ "Enteric",
    grepl("Liv_Man_Man", variable) ~ "Manure",
    grepl("Agr_Ric", variable) ~ "Rice",
    grepl("Man_Soi", variable) ~ "Managed soils",
    variable %in% c("Emi_Red_CO2_AFO_Lan_Pla", "Emi_Red_CO2_AFO_Lan_Ref", "Emi_Red_CO2_AFO_Lan_Agf", "Emi_CO2_AFO_Aff") ~ "Forest restore",
    variable %in% c("Emi_Red_CO2_AFO_Lan_FPr", "Emi_Red_CO2_AFO_Lan_Pff", "Emi_Red_CO2_AFO_Lan_Ril", "Emi_Red_CO2_AFO_Lan_Enr", "Emi_Red_CO2_AFO_Lan_Pro", "Emi_Red_CO2_AFO_Lan_Man", "Emi_Red_CO2_AFO_Lan_Res") ~ "Forest protect/manage",
    variable %in% c("Emi_Red_CO2_AFO_Lan_Bch", "Emi_Red_CO2_AFO_Lan_Scs", "Emi_Red_CO2_AFO_Lan_Scg", "Emi_Red_CO2_AFO_Lan_Scc", "Car_Seq_Lan_Use_Soi_Car_Man", "Car_Seq_Lan_Use_Soi_Car_Man_Cro", "Car_Seq_Lan_Use_Soi_Car_Man_Gra") ~ "Biochar/soil carbon",
    TRUE ~ NA_character_
  )
}

afolub_tech_name <- function(variable) {
  recode(
    variable,
    "Emi_Red_equ_CH4_AFO_Agr_Liv_Ent_Fer_Bre" = "High genetic merit",
    "Emi_Red_equ_CH4_AFO_Agr_Liv_Ent_Fer_Fee" = "Feed management",
    "Emi_Red_equ_CH4_AFO_Agr_Liv_Man_Man_Cad" = "Covered digesters",
    "Emi_Red_equ_CH4_AFO_Agr_Liv_Man_Man_And" = "Anaerobic digestion",
    "Emi_Red_equ_CH4_AFO_Agr_Liv_Man_Man_Cfl" = "Dome digester",
    "Emi_Red_equ_CH4_AFO_Agr_Liv_Man_Man_Adc" = "Aerobic decomposition",
    "Emi_Red_equ_CH4_AFO_Agr_Liv_Man_Man_Ams" = "Ammonium sulfate",
    "Emi_Red_equ_CH4_AFO_Agr_Ric_Wat" = "Rice water management",
    "Emi_Red_equ_CH4_AFO_Agr_Ric_Res" = "Rice residue management",
    "Emi_Red_equ_N2O_AFO_Agr_Man_Soi_Til" = "No-tillage",
    "Emi_Red_equ_N2O_AFO_Agr_Man_Soi_Fer" = "Fertilizer efficiency",
    "Emi_Red_equ_N2O_AFO_Agr_Man_Soi_Res" = "Residue replacement",
    "Emi_Red_equ_N2O_AFO_Agr_Man_Soi_Slo" = "Slow-release fertilizer",
    "Emi_Red_equ_N2O_AFO_Agr_Man_Soi_Nin" = "Nitrification inhibitors",
    "Emi_Red_CO2_AFO_Lan_Pla" = "Afforestation",
    "Emi_Red_CO2_AFO_Lan_Ref" = "Reforestation",
    "Emi_Red_CO2_AFO_Lan_FPr" = "Forest protection",
    "Emi_Red_CO2_AFO_Lan_Pff" = "Prevent forest fire",
    "Emi_Red_CO2_AFO_Lan_Ril" = "Reduced impact logging",
    "Emi_Red_CO2_AFO_Lan_Enr" = "Enhanced regeneration",
    "Emi_Red_CO2_AFO_Lan_Agf" = "Agroforestry",
    "Emi_Red_CO2_AFO_Lan_Bch" = "Biochar",
    "Emi_Red_CO2_AFO_Lan_Scs" = "Soil carbon sequestration",
    "Emi_Red_CO2_AFO_Lan_Scg" = "SCS grassland",
    "Emi_Red_CO2_AFO_Lan_Scc" = "SCS cropland",
    .default = variable
  )
}

# data --------------------------------------------------------------------
# country list
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
      tibble(
        region = x[[1]],
        region5_code = x[[length(x)]]
      )
    }) %>%
    filter(region %in% country_codes, region5_code %in% names(r5_labels), region5_code != "World") %>%
    mutate(region5 = recode(region5_code, !!!r5_labels))
} else {
  stop("region.map is not available: ", region_map_file)
}

# boundary
Boundary <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>%
  st_make_valid() %>%
  transmute(cty = dplyr::if_else(adm0_a3 != "-99", adm0_a3, iso_a3), country_name = admin, pop_est = pop_est) %>%
  mutate(country_name = short_country_name(country_name)) %>%
  filter(cty %in% country_codes)

# AGMIP
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

regions_for_read <- c("World", sort(unique(country_codes)))

# main reads
GEER_AGMIP <- read_agmip(
  variables = c("CALI", "CALO", "XPRP", "PROD", "NETT", "LAND", "POPT", "EMIS", "EmisRed", "EmisRedCO2", "EmisRedCH4", "EmisRedN2O"),
  items = c(
    "TOT", "AGR", "CRP", "LSP", "FOR", "GRS", "ECP", "ONV", "NLD",
    "WHT", "RIC", "CGR", "RUM", "NRM", "DRY",
    "tot", "ent", "mmg", "rcc", "sfr", "mas", "mgr", "crs", "bcr"
  ),
  units = c("kcal/cap/d", "USD/t", "1000 t", "1000 ha", "million", "MtCO2e"),
  regions = regions_for_read,
  years = decadal_years,
  scenarios = c(scenario_baseline, scenario_landint)
)

# AFOLUB full
GEER_AFOLUB_ALL <- rgdx.param(file.path(data_dir, "IAMC_templatePlus.gdx"), "IAMC_templatePlus")

# merged full
GEER_MERGED_IAMCTemp_ALL <- rgdx.param(file.path(data_dir, "Mergedcomparison.gdx"), "IAMCTemp")
GEER_MERGED_IAMCTempwoU_ALL <- rgdx.param(file.path(data_dir, "Mergedcomparison.gdx"), "IAMCTempwoU")
GEER_MERGED_AREA_ALL <- rgdx.param(file.path(data_dir, "Mergedcomparison.gdx"), "AREA")

# working subsets
GEER_AFOLUB <- read_iamcplus(
  variables = c(
    "Emi_Red_equ_GHG_AFO_Agr",
    "Emi_Red_equ_GHG_AFO_Lan",
    "Emi_Red_equ_CH4_AFO_Agr",
    "Emi_Red_equ_N2O_AFO_Agr",
    "Red_Cos_AFO_Agr",
    "Emi_equ_CH4_AFO_Agr",
    "Emi_equ_N2O_AFO_Agr"
  ),
  regions = regions_for_read,
  years = decadal_years,
  scenarios = c(scenario_afolub_default)
)

GEER_MERGED <- read_merged_wou(
  variables = c(
    "Emi_CO2_AFO_Lan", "Emi_CO2_AFO_Def", "Emi_CO2_AFO_Aff", "Emi_CO2_AFO_For_Man"
  ),
  regions = regions_for_read,
  years = decadal_years,
  scenarios = c(scenario_baseline, scenario_landint)
)

afolub_ref_vars <- c(
  "Emi_equ_GHG_AFO",
  "Emi_Red_equ_GHG_AFO_Agr",
  "Emi_Red_equ_GHG_AFO_Lan",
  "Emi_Red_equ_CH4_AFO_Agr",
  "Emi_Red_equ_N2O_AFO_Agr",
  "Red_Cos_AFO_Agr",
  "Emi_equ_CH4_AFO_Agr",
  "Emi_equ_N2O_AFO_Agr",
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
  "Emi_CO2_AFO_Aff",
  "Red_Cos_AFO_Agr_Liv_Ent_Fer_Bre",
  "Red_Cos_AFO_Agr_Liv_Ent_Fer_Fee",
  "Red_Cos_AFO_Agr_Liv_Man_Man_Adc",
  "Red_Cos_AFO_Agr_Liv_Man_Man_Ams",
  "Red_Cos_AFO_Agr_Liv_Man_Man_Cfl",
  "Red_Cos_AFO_Agr_Liv_Man_Man_Cad",
  "Red_Cos_AFO_Agr_Liv_Man_Man_And",
  "Red_Cos_AFO_Agr_Ric_Res",
  "Red_Cos_AFO_Agr_Man_Soi_Fer",
  "Red_Cos_AFO_Agr_Man_Soi_Res",
  "Red_Cos_AFO_Agr_Man_Soi_Bio",
  "Red_Cos_AFO_Agr_Man_Soi_Slo",
  "Red_Cos_AFO_Agr_Man_Soi_Nin",
  "Red_Cos_AFO_Lan_Pla",
  "Red_Cos_AFO_Lan_Ref",
  "Red_Cos_AFO_Lan_FPr",
  "Red_Cos_AFO_Lan_Pff",
  "Red_Cos_AFO_Lan_Ril",
  "Red_Cos_AFO_Lan_Enr",
  "Red_Cos_AFO_Lan_Agf",
  "Red_Cos_AFO_Lan_Bch",
  "Red_Cos_AFO_Lan_Scg",
  "Red_Cos_AFO_Lan_Scc",
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

GEER_AFOLUB_REF <- GEER_AFOLUB_ALL %>%
  transmute(
    scenario = as.character(Scenario),
    region = as.character(REMF),
    variable = as.character(VEMF),
    year = as.integer(as.character(YEMF)),
    value = as.numeric(IAMC_templatePlus)
  ) %>%
  filter(
    scenario %in% c(scenario_landint, scenario_no, scenario_baseline, "FAO-FAOTier1", "FAO-UNFCCC", "EDGAR"),
    year %in% c(2005L, decadal_years),
    variable %in% afolub_ref_vars
  )

message("GEER_AGMIP rows: ", nrow(GEER_AGMIP))
message("GEER_AFOLUB rows: ", nrow(GEER_AFOLUB))
message("GEER_AFOLUB_REF rows: ", nrow(GEER_AFOLUB_REF))
message("region.map matched countries: ", nrow(region_map))
message("region.map missing countries: ", sum(!country_codes %in% region_map$region))

riso_top100 <- if (file.exists(riso_top100_file)) {
  readr::read_csv(riso_top100_file, show_col_types = FALSE) %>%
    transmute(region = Region) %>%
    filter(region %in% country_codes)
} else {
  tibble(region = character())
}

# plot 1 ------------------------------------------------------------------
make_top_plot <- function(plot_year) {
  mitigation_country <- GEER_AFOLUB %>%
    filter(
      scenario == scenario_afolub_default,
      year == plot_year,
      variable %in% c("Emi_Red_equ_GHG_AFO_Agr", "Emi_Red_equ_GHG_AFO_Lan"),
      region %in% country_codes
    ) %>%
    mutate(component = recode(
      variable,
      Emi_Red_equ_GHG_AFO_Agr = "Agriculture",
      Emi_Red_equ_GHG_AFO_Lan = "Land use"
    )) %>%
    mutate(component = factor(component, levels = c("Land use", "Agriculture"))) %>%
    left_join(st_drop_geometry(Boundary), by = c("region" = "cty")) %>%
    mutate(country = coalesce(country_name, region))

  top_mitigation_country <- mitigation_country %>%
    group_by(region, country) %>%
    summarise(total_reduction = sum(value, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(total_reduction)) %>%
    mutate(rank = row_number()) %>%
    filter(rank <= 30) %>%
    select(region, country, rank) %>%
    left_join(mitigation_country, by = c("region", "country")) %>%
    arrange(rank, component) %>%
    mutate(country = factor(country, levels = rev(unique(country))))

  ggplot(top_mitigation_country, aes(x = value, y = country, fill = component)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey40", linewidth = 0.35) +
    geom_col() +
    scale_fill_manual(values = component_palette) +
    scale_x_continuous(labels = label_comma()) +
    labs(
      title = paste0("Mitigation potential in ", plot_year),
      subtitle = paste0("Top 30 countries ranked within ", plot_year),
      x = "Mitigation potential",
      y = "Country",
      caption = "Restricted to country.set"
    ) +
    theme_main
}

save_plot("plot01_top_mitigation_2050_2100.png", patchwork::wrap_plots(make_top_plot(2050), make_top_plot(2100), ncol = 2), width = 12, height = 8)

# plot 1b -----------------------------------------------------------------
top_reduction_regions_2100 <- GEER_AFOLUB %>%
  filter(
    scenario == scenario_afolub_default,
    year == 2100,
    variable %in% c("Emi_Red_equ_GHG_AFO_Agr", "Emi_Red_equ_GHG_AFO_Lan"),
    region %in% country_codes
  ) %>%
  group_by(region) %>%
  summarise(total_reduction = sum(value, na.rm = TRUE), .groups = "drop") %>%
  slice_max(order_by = total_reduction, n = 20, with_ties = FALSE)

top_reduction_2100 <- GEER_AFOLUB %>%
  filter(
    scenario == scenario_afolub_default,
    year == 2100,
    variable %in% c("Emi_Red_equ_GHG_AFO_Agr", "Emi_Red_equ_GHG_AFO_Lan"),
    region %in% top_reduction_regions_2100$region
  ) %>%
  mutate(component = recode(
    variable,
    Emi_Red_equ_GHG_AFO_Agr = "Agriculture",
    Emi_Red_equ_GHG_AFO_Lan = "Land use"
  )) %>%
  group_by(region, component) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  left_join(top_reduction_regions_2100, by = "region") %>%
  left_join(st_drop_geometry(Boundary), by = c("region" = "cty")) %>%
  mutate(
    country = coalesce(country_name, region),
    component = factor(component, levels = c("Land use", "Agriculture"))
  ) %>%
  arrange(desc(total_reduction), component)

top_reduction_country_order_2100 <- top_reduction_2100 %>%
  distinct(region, country, total_reduction) %>%
  arrange(desc(total_reduction))

land_change_2100 <- GEER_AGMIP %>%
  filter(
    year == 2100,
    region %in% top_reduction_2100$region,
    variable == "LAND",
    unit == "1000 ha",
    item %in% c("CRP", "ECP", "GRS", "ONV", "FOR", "NLD")
  ) %>%
  select(region, scenario, item, value) %>%
  pivot_wider(names_from = item, values_from = value, values_fill = 0) %>%
  mutate(CRP_NE = CRP - ECP) %>%
  select(region, scenario, NLD, CRP_NE, ECP, GRS, ONV, FOR) %>%
  pivot_longer(cols = c(NLD, CRP_NE, ECP, GRS, ONV, FOR), names_to = "item", values_to = "value") %>%
  pivot_wider(names_from = scenario, values_from = value, values_fill = 0) %>%
  mutate(
    land_change = .data[[scenario_landint]] - .data[[scenario_baseline]],
    item = recode(
      item,
      NLD = "Non arable land",
      CRP_NE = "Cropland",
      ECP = "Energy Crop",
      GRS = "Grassland",
      ONV = "Other natural land",
      FOR = "Managed and primary forest"
    ),
    item = factor(
      item,
      levels = c("Cropland", "Energy Crop", "Grassland", "Managed and primary forest", "Other natural land", "Non arable land")
    )
  ) %>%
  left_join(top_reduction_country_order_2100, by = "region") %>%
  distinct(region, country, total_reduction, item, land_change) %>%
  mutate(country = factor(country, levels = rev(top_reduction_country_order_2100$country)))

land_total_2100 <- GEER_AGMIP %>%
  filter(
    year == 2100,
    scenario == scenario_baseline,
    region %in% top_reduction_country_order_2100$region,
    variable == "LAND",
    item == "TOT",
    unit == "1000 ha"
  ) %>%
  transmute(region, land_total = value) %>%
  distinct(region, land_total)

land_change_share_2100 <- land_change_2100 %>%
  left_join(land_total_2100, by = "region") %>%
  mutate(land_change_share = if_else(land_total > 0, 100 * land_change / land_total, NA_real_))

p_top_reduction_2100 <- ggplot(
  top_reduction_2100 %>% mutate(country = factor(country, levels = rev(unique(country)))),
  aes(x = value, y = country, fill = component)
) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40", linewidth = 0.35) +
  geom_col() +
  scale_fill_manual(values = component_palette) +
  labs(
    title = "Top mitigation countries in 2100",
    x = "Mitigation potential",
    y = NULL
  ) +
  theme_main

p_land_change_2100 <- ggplot(land_change_2100, aes(x = land_change, y = country, fill = item)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40", linewidth = 0.35) +
  geom_col() +
  scale_fill_manual(values = land_color) +
  labs(
    title = "Land change from BaU in 2100",
    x = "1000 ha",
    y = NULL
  ) +
  theme_main

p_land_change_share_2100 <- ggplot(land_change_share_2100, aes(x = land_change_share, y = country, fill = item)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40", linewidth = 0.35) +
  geom_col() +
  scale_fill_manual(values = land_color) +
  labs(
    title = "Land change from BaU in 2100",
    x = "% of total land area",
    y = NULL
  ) +
  theme_main

save_plot("plot01b_top_reduction_and_land_change_2100.png", patchwork::wrap_plots(p_top_reduction_2100, p_land_change_2100, ncol = 2, widths = c(1, 1.4)), width = 14, height = 8)
save_plot("plot01b_top_reduction_and_land_change_share_2100.png", patchwork::wrap_plots(p_top_reduction_2100, p_land_change_share_2100, ncol = 2, widths = c(1, 1.4)), width = 14, height = 8)

# plot 1c -----------------------------------------------------------------
mitigation_intensity_2100 <- top_reduction_country_order_2100 %>%
  left_join(
    GEER_AGMIP %>%
      filter(
        scenario == scenario_landint,
        year == 2100,
        variable == "POPT",
        item == "TOT",
        unit == "million"
      ) %>%
      select(region, population_million = value),
    by = "region"
  ) %>%
  left_join(
    GEER_AGMIP %>%
      filter(
        scenario == scenario_landint,
        year == 2100,
        variable == "LAND",
        item == "TOT",
        unit == "1000 ha"
      ) %>%
      select(region, land_area_kha = value),
    by = "region"
  ) %>%
  mutate(
    reduction_per_capita = if_else(population_million > 0, total_reduction / population_million, NA_real_),
    reduction_per_area = if_else(land_area_kha > 0, total_reduction / land_area_kha, NA_real_)
  )

p_reduction_per_capita_2100 <- ggplot(
  mitigation_intensity_2100 %>% mutate(country = factor(country, levels = rev(country))),
  aes(x = reduction_per_capita, y = country)
) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40", linewidth = 0.35) +
  geom_col(fill = "#4C78A8") +
  labs(
    title = "Per-capita mitigation in 2100",
    x = "MtCO2eq / million people",
    y = NULL
  ) +
  theme_main

p_reduction_per_area_2100 <- ggplot(
  mitigation_intensity_2100 %>% mutate(country = factor(country, levels = rev(country))),
  aes(x = reduction_per_area, y = country)
) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40", linewidth = 0.35) +
  geom_col(fill = "#72B7B2") +
  labs(
    title = "Mitigation per land area in 2100",
    x = "MtCO2eq / 1000 ha",
    y = NULL
  ) +
  theme_main

save_plot("plot01c_reduction_intensity_2100.png", patchwork::wrap_plots(p_reduction_per_capita_2100, p_reduction_per_area_2100, ncol = 2), width = 12, height = 8)

# plot 2 ------------------------------------------------------------------
concentration_curve <- GEER_AFOLUB %>%
  filter(
    scenario == scenario_afolub_default,
    year %in% plot_years,
    variable %in% c("Emi_Red_equ_GHG_AFO_Agr", "Emi_Red_equ_GHG_AFO_Lan"),
    region %in% country_codes
  ) %>%
  left_join(st_drop_geometry(Boundary), by = c("region" = "cty")) %>%
  mutate(country = coalesce(country_name, region)) %>%
  group_by(year, region, country) %>%
  summarise(total_reduction = sum(value, na.rm = TRUE), .groups = "drop") %>%
  group_by(year) %>%
  arrange(desc(total_reduction), .by_group = TRUE) %>%
  mutate(
    share = total_reduction / sum(total_reduction, na.rm = TRUE),
    cum_share = cumsum(share),
    cum_country = row_number() / n()
  ) %>%
  ungroup()

p_concentration <- ggplot(concentration_curve, aes(x = cum_country, y = cum_share, color = factor(year))) +
  geom_line(linewidth = 1.1) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey60") +
  scale_x_continuous(labels = percent_format()) +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "How concentrated is mitigation?",
    subtitle = "Cumulative share of total mitigation across countries",
    x = "Cumulative share of countries",
    y = "Cumulative share of mitigation"
  ) +
  theme_main

save_plot("plot02_concentration_curve.png", p_concentration, width = 8, height = 5)

# plot 3 ------------------------------------------------------------------
agmip_specs <- tribble(
  ~variable, ~item, ~unit,
  "CALI", "TOT", "kcal/cap/d"
)

country_tradeoff <- GEER_AFOLUB %>%
  filter(
    scenario == scenario_afolub_default,
    year %in% plot_years,
    variable %in% c("Emi_Red_equ_GHG_AFO_Agr", "Emi_Red_equ_GHG_AFO_Lan"),
    region %in% country_codes
  ) %>%
  group_by(year, region) %>%
  summarise(total_reduction = sum(value, na.rm = TRUE), .groups = "drop") %>%
  left_join(
    GEER_AGMIP %>%
      semi_join(agmip_specs, by = c("variable", "item", "unit")) %>%
      pivot_wider(names_from = scenario, values_from = value) %>%
      mutate(calorie_change = .data[[scenario_landint]] - .data[[scenario_baseline]]) %>%
      select(region, year, calorie_change),
    by = c("region", "year")
  ) %>%
  left_join(st_drop_geometry(Boundary), by = c("region" = "cty")) %>%
  mutate(country = coalesce(country_name, region))

country_tradeoff_plot <- country_tradeoff %>%
  filter(is.finite(calorie_change), total_reduction > 0)

label_points <- country_tradeoff_plot %>%
  group_by(year) %>%
  slice_max(order_by = total_reduction, n = 10, with_ties = FALSE) %>%
  ungroup()

p_tradeoff <- ggplot(country_tradeoff_plot, aes(x = calorie_change, y = total_reduction)) +
  geom_hline(yintercept = 0, color = "grey70", linewidth = 0.3) +
  geom_vline(xintercept = 0, color = "grey70", linewidth = 0.3) +
  geom_point(alpha = 0.75, color = "#C3327A", size = 2) +
  geom_text(
    data = label_points,
    aes(label = country),
    size = 2.5,
    nudge_y = 0.04 * max(country_tradeoff_plot$total_reduction, na.rm = TRUE),
    check_overlap = TRUE
  ) +
  facet_wrap(~ year, scales = "free") +
  scale_y_continuous(labels = label_number(accuracy = 1, big.mark = ",")) +
  labs(
    title = "Do high-mitigation countries also have calorie losses?",
    subtitle = "Each point is a country",
    x = "Calorie change",
    y = "Total mitigation"
  ) +
  theme_main

save_plot("plot03_tradeoff_calorie.png", p_tradeoff, width = 10, height = 5)

# plot 4 ------------------------------------------------------------------
make_side_effect_plot <- function(plot_year) {
  top_countries <- GEER_AFOLUB %>%
    filter(
      scenario == scenario_afolub_default,
      year == plot_year,
      variable %in% c("Emi_Red_equ_GHG_AFO_Agr", "Emi_Red_equ_GHG_AFO_Lan"),
      region %in% country_codes
    ) %>%
    group_by(region) %>%
    summarise(total_reduction = sum(value, na.rm = TRUE), .groups = "drop") %>%
    slice_max(order_by = total_reduction, n = 12, with_ties = FALSE) %>%
    arrange(desc(total_reduction)) %>%
    mutate(rank = row_number())

  agmip_side <- GEER_AGMIP %>%
    filter(
      year == plot_year,
      region %in% top_countries$region,
      (variable == "CALI" & item == "TOT" & unit == "kcal/cap/d") |
        (variable == "XPRP" & item == "AGR" & unit == "USD/t") |
        (variable == "PROD" & item == "AGR" & unit == "1000 t")
    ) %>%
    pivot_wider(names_from = scenario, values_from = value) %>%
    mutate(value = .data[[scenario_landint]] - .data[[scenario_baseline]]) %>%
    mutate(metric = case_when(
      variable == "CALI" ~ "Calorie change",
      variable == "XPRP" ~ "Price change",
      variable == "PROD" ~ "Production change"
    )) %>%
    select(region, metric, value)

  land_side <- GEER_AGMIP %>%
    filter(
      year == plot_year,
      region %in% top_countries$region,
      variable == "LAND",
      unit == "1000 ha",
      item %in% c("CRP", "ECP", "GRS", "FOR", "ONV", "NLD")
    ) %>%
    select(region, scenario, item, value) %>%
    pivot_wider(names_from = item, values_from = value, values_fill = 0) %>%
    mutate(CRP_NE = CRP - ECP) %>%
    select(region, scenario, CRP_NE, ECP, GRS, ONV, FOR, NLD) %>%
    pivot_longer(cols = c(CRP_NE, ECP, GRS, ONV, FOR, NLD), names_to = "metric", values_to = "value") %>%
    pivot_wider(names_from = scenario, values_from = value, values_fill = 0) %>%
    mutate(
      value = .data[[scenario_landint]] - .data[[scenario_baseline]],
      metric = recode(
        metric,
        CRP_NE = "Cropland change",
        ECP = "Energy crop change",
        GRS = "Grassland change",
        ONV = "Other natural land change",
        FOR = "Forest change",
        NLD = "Non arable land change"
      )
    ) %>%
    select(region, metric, value)

  side_effect_df <- bind_rows(agmip_side, land_side) %>%
    left_join(top_countries, by = "region") %>%
    left_join(st_drop_geometry(Boundary), by = c("region" = "cty")) %>%
    mutate(country = coalesce(country_name, region)) %>%
    arrange(rank) %>%
    mutate(country = factor(country, levels = rev(unique(country))))

  ggplot(side_effect_df, aes(x = value, y = country)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey40", linewidth = 0.35) +
    geom_col(fill = "#3873AD") +
    facet_wrap(~ metric, scales = "free_x", ncol = 2) +
    labs(
      title = paste0("What happens in top mitigation countries? (", plot_year, ")"),
      subtitle = "Top 12 countries ranked by total mitigation",
      x = "Change from BaU",
      y = "Country"
    ) +
    theme_main
}

save_plot("plot04_top_country_side_effects_2050_2100.png", patchwork::wrap_plots(make_side_effect_plot(2050), make_side_effect_plot(2100), ncol = 2), width = 14, height = 10)

# plot 5 ------------------------------------------------------------------
world_reduction_category <- GEER_AFOLUB_REF %>%
  filter(
    scenario == scenario_afolub_default,
    region == "World",
    year >= 2030,
    variable %in% c(
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
      "Emi_Red_CO2_AFO_Lan_Ril",
      "Emi_Red_CO2_AFO_Lan_Enr",
      "Emi_Red_CO2_AFO_Lan_Agf",
      "Emi_Red_CO2_AFO_Lan_Bch",
      "Emi_Red_CO2_AFO_Lan_Scs",
      "Emi_Red_CO2_AFO_Lan_Scg",
      "Emi_Red_CO2_AFO_Lan_Scc"
    )
  ) %>%
  mutate(category = case_when(
    variable %in% c("Emi_Red_equ_CH4_AFO_Agr_Liv_Ent_Fer_Bre", "Emi_Red_equ_CH4_AFO_Agr_Liv_Ent_Fer_Fee") ~ "Enteric",
    variable %in% c("Emi_Red_equ_CH4_AFO_Agr_Liv_Man_Man_Adc", "Emi_Red_equ_CH4_AFO_Agr_Liv_Man_Man_Ams", "Emi_Red_equ_CH4_AFO_Agr_Liv_Man_Man_Cfl", "Emi_Red_equ_CH4_AFO_Agr_Liv_Man_Man_Cad", "Emi_Red_equ_CH4_AFO_Agr_Liv_Man_Man_And") ~ "Manure",
    variable %in% c("Emi_Red_equ_CH4_AFO_Agr_Ric_Wat", "Emi_Red_equ_CH4_AFO_Agr_Ric_Res") ~ "Rice",
    variable %in% c("Emi_Red_equ_N2O_AFO_Agr_Man_Soi_Til", "Emi_Red_equ_N2O_AFO_Agr_Man_Soi_Fer", "Emi_Red_equ_N2O_AFO_Agr_Man_Soi_Res", "Emi_Red_equ_N2O_AFO_Agr_Man_Soi_Slo", "Emi_Red_equ_N2O_AFO_Agr_Man_Soi_Nin") ~ "Managed soils",
    variable %in% c("Emi_Red_CO2_AFO_Lan_Pla", "Emi_Red_CO2_AFO_Lan_Ref", "Emi_Red_CO2_AFO_Lan_Agf") ~ "Forest restore",
    variable %in% c("Emi_Red_CO2_AFO_Lan_FPr", "Emi_Red_CO2_AFO_Lan_Pff", "Emi_Red_CO2_AFO_Lan_Ril", "Emi_Red_CO2_AFO_Lan_Enr") ~ "Forest protect/manage",
    variable %in% c("Emi_Red_CO2_AFO_Lan_Bch", "Emi_Red_CO2_AFO_Lan_Scs", "Emi_Red_CO2_AFO_Lan_Scg", "Emi_Red_CO2_AFO_Lan_Scc") ~ "Biochar/soil carbon",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(category)) %>%
  group_by(year, category) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop")

p_world_reduction_category <- ggplot(world_reduction_category, aes(x = factor(year), y = value, fill = category)) +
  geom_col() +
  scale_fill_manual(values = category_palette) +
  scale_y_continuous(labels = label_comma()) +
  labs(
    title = "World mitigation by technology group",
    x = "Year",
    y = "Reduction potential"
  ) +
  theme_main

save_plot("plot05_world_reduction_category.png", p_world_reduction_category, width = 8, height = 5)

# plot 6 ------------------------------------------------------------------
r5_portfolio_2100 <- GEER_AFOLUB_REF %>%
  filter(
    scenario == scenario_afolub_default,
    region %in% names(r5_labels),
    region != "World",
    year == 2100,
    variable %in% c(
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
      "Emi_Red_CO2_AFO_Lan_Ril",
      "Emi_Red_CO2_AFO_Lan_Enr",
      "Emi_Red_CO2_AFO_Lan_Agf",
      "Emi_Red_CO2_AFO_Lan_Bch",
      "Emi_Red_CO2_AFO_Lan_Scs",
      "Emi_Red_CO2_AFO_Lan_Scg",
      "Emi_Red_CO2_AFO_Lan_Scc"
    )
  ) %>%
  mutate(
    region_name = recode(region, !!!r5_labels),
    category = case_when(
      grepl("Liv_Ent_Fer", variable) ~ "Enteric",
      grepl("Liv_Man_Man", variable) ~ "Manure",
      grepl("Agr_Ric", variable) ~ "Rice",
      grepl("Man_Soi", variable) ~ "Managed soils",
      variable %in% c("Emi_Red_CO2_AFO_Lan_Pla", "Emi_Red_CO2_AFO_Lan_Ref", "Emi_Red_CO2_AFO_Lan_Agf") ~ "Forest restore",
      variable %in% c("Emi_Red_CO2_AFO_Lan_FPr", "Emi_Red_CO2_AFO_Lan_Pff", "Emi_Red_CO2_AFO_Lan_Ril", "Emi_Red_CO2_AFO_Lan_Enr") ~ "Forest protect/manage",
      TRUE ~ "Biochar/soil carbon"
    )
  ) %>%
  group_by(region_name, category) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  group_by(region_name) %>%
  mutate(total = sum(value, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(region_name = fct_reorder(region_name, total))

p_r5_portfolio_2100 <- ggplot(r5_portfolio_2100, aes(x = value, y = region_name, fill = category)) +
  geom_col() +
  scale_fill_manual(values = category_palette) +
  scale_x_continuous(labels = label_comma()) +
  labs(
    title = "R5 mitigation portfolio in 2100",
    x = "Reduction potential",
    y = "Region"
  ) +
  theme_main

save_plot("plot06_r5_portfolio_2100.png", p_r5_portfolio_2100, width = 8, height = 5)

# plot 7 ------------------------------------------------------------------
make_top_portfolio_plot <- function(plot_year) {
  top_countries <- GEER_AFOLUB %>%
    filter(
      scenario == scenario_afolub_default,
      year == plot_year,
      variable %in% c("Emi_Red_equ_GHG_AFO_Agr", "Emi_Red_equ_GHG_AFO_Lan"),
      region %in% country_codes
    ) %>%
    group_by(region) %>%
    summarise(total_reduction = sum(value, na.rm = TRUE), .groups = "drop") %>%
    slice_max(order_by = total_reduction, n = 15, with_ties = FALSE) %>%
    arrange(desc(total_reduction)) %>%
    mutate(rank = row_number())

  top_portfolio <- GEER_AFOLUB_REF %>%
    filter(
      scenario == scenario_afolub_default,
      year == plot_year,
      region %in% top_countries$region,
      variable %in% c(
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
        "Emi_Red_CO2_AFO_Lan_Ril",
        "Emi_Red_CO2_AFO_Lan_Enr",
        "Emi_Red_CO2_AFO_Lan_Agf",
        "Emi_Red_CO2_AFO_Lan_Bch",
        "Emi_Red_CO2_AFO_Lan_Scs",
        "Emi_Red_CO2_AFO_Lan_Scg",
        "Emi_Red_CO2_AFO_Lan_Scc"
      )
    ) %>%
    mutate(category = case_when(
      grepl("Liv_Ent_Fer", variable) ~ "Enteric",
      grepl("Liv_Man_Man", variable) ~ "Manure",
      grepl("Agr_Ric", variable) ~ "Rice",
      grepl("Man_Soi", variable) ~ "Managed soils",
      variable %in% c("Emi_Red_CO2_AFO_Lan_Pla", "Emi_Red_CO2_AFO_Lan_Ref", "Emi_Red_CO2_AFO_Lan_Agf") ~ "Forest restore",
      variable %in% c("Emi_Red_CO2_AFO_Lan_FPr", "Emi_Red_CO2_AFO_Lan_Pff", "Emi_Red_CO2_AFO_Lan_Ril", "Emi_Red_CO2_AFO_Lan_Enr") ~ "Forest protect/manage",
      TRUE ~ "Biochar/soil carbon"
    )) %>%
    group_by(region, category) %>%
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    left_join(top_countries, by = "region") %>%
    left_join(st_drop_geometry(Boundary), by = c("region" = "cty")) %>%
    mutate(country = coalesce(country_name, region)) %>%
    arrange(rank) %>%
    mutate(country = factor(country, levels = rev(unique(country))))

  ggplot(top_portfolio, aes(x = value, y = country, fill = category)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey40", linewidth = 0.35) +
    geom_col() +
    scale_fill_manual(values = category_palette) +
    scale_x_continuous(labels = label_comma()) +
    labs(
      title = paste0("Top-country mitigation portfolio in ", plot_year),
      x = "Reduction potential",
      y = "Country"
    ) +
    theme_main
}

save_plot("plot07_top_portfolio_2050_2100.png", patchwork::wrap_plots(make_top_portfolio_plot(2050), make_top_portfolio_plot(2100), ncol = 2), width = 12, height = 8)

# plot 7b -----------------------------------------------------------------
make_top_emissions_plot <- function(plot_year) {
  top_countries <- GEER_AFOLUB_REF %>%
    filter(
      scenario == scenario_afolub_default,
      year == plot_year,
      region %in% country_codes,
      variable %in% c(
        "Emi_equ_CH4_AFO_Agr_Liv_Ent_Fer",
        "Emi_equ_CH4_AFO_Agr_Liv_Man_Man",
        "Emi_equ_CH4_AFO_Agr_Ric",
        "Emi_equ_N2O_AFO_Agr_Man_Soi",
        "Emi_CO2_Lan_Use_Flo_Pos_Emi_Lan_Use_Cha",
        "Emi_CO2_AFO_Lan_For_Gra_Con",
        "Emi_CO2_Lan_Use_Flo_Pos_Emi_Woo_Har",
        "Emi_CO2_Lan_Use_Flo_Pos_Emi_Fir",
        "Emi_CO2_Lan_Use_Flo_RemForest"
      )
    ) %>%
    group_by(region) %>%
    summarise(total_emissions = sum(value, na.rm = TRUE), .groups = "drop") %>%
    slice_max(order_by = total_emissions, n = 15, with_ties = FALSE) %>%
    arrange(desc(total_emissions)) %>%
    mutate(rank = row_number())

  top_emissions <- GEER_AFOLUB_REF %>%
    filter(
      scenario == scenario_afolub_default,
      year == plot_year,
      region %in% top_countries$region,
      variable %in% c(
        "Emi_equ_CH4_AFO_Agr_Liv_Ent_Fer",
        "Emi_equ_CH4_AFO_Agr_Liv_Man_Man",
        "Emi_equ_CH4_AFO_Agr_Ric",
        "Emi_equ_N2O_AFO_Agr_Man_Soi",
        "Emi_CO2_Lan_Use_Flo_Pos_Emi_Lan_Use_Cha",
        "Emi_CO2_AFO_Lan_For_Gra_Con",
        "Emi_CO2_Lan_Use_Flo_Pos_Emi_Woo_Har",
        "Emi_CO2_Lan_Use_Flo_Pos_Emi_Fir",
        "Emi_CO2_Lan_Use_Flo_RemForest"
      )
    ) %>%
    mutate(component = case_when(
      variable == "Emi_equ_CH4_AFO_Agr_Liv_Ent_Fer" ~ "Enteric",
      variable == "Emi_equ_CH4_AFO_Agr_Liv_Man_Man" ~ "Manure",
      variable == "Emi_equ_CH4_AFO_Agr_Ric" ~ "Rice",
      variable == "Emi_equ_N2O_AFO_Agr_Man_Soi" ~ "Managed soils",
      variable %in% c("Emi_CO2_Lan_Use_Flo_Pos_Emi_Lan_Use_Cha", "Emi_CO2_AFO_Lan_For_Gra_Con") ~ "Land-use change",
      variable == "Emi_CO2_Lan_Use_Flo_Pos_Emi_Woo_Har" ~ "Wood harvest CO2",
      variable == "Emi_CO2_Lan_Use_Flo_Pos_Emi_Fir" ~ "Fire CO2",
      variable == "Emi_CO2_Lan_Use_Flo_RemForest" ~ "Forest sink"
    )) %>%
    group_by(region, component) %>%
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    left_join(top_countries, by = "region") %>%
    left_join(st_drop_geometry(Boundary), by = c("region" = "cty")) %>%
    mutate(country = coalesce(country_name, region)) %>%
    arrange(rank) %>%
    mutate(country = factor(country, levels = rev(unique(country))))

  ggplot(top_emissions, aes(x = value, y = country, fill = component)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey40", linewidth = 0.35) +
    geom_col() +
    scale_fill_manual(values = emission_palette) +
    scale_x_continuous(labels = label_comma()) +
    labs(
      title = paste0("Top-country emissions portfolio in ", plot_year),
      x = "Emissions",
      y = "Country"
    ) +
    theme_main
}

save_plot("plot08_top_emissions_2050_2100.png", patchwork::wrap_plots(make_top_emissions_plot(2050), make_top_emissions_plot(2100), ncol = 2), width = 12, height = 8)

# plot 8 ------------------------------------------------------------------
world_emissions <- GEER_AFOLUB_REF %>%
  filter(
    scenario == scenario_afolub_default,
    region == "World",
    year >= 2030,
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
  mutate(category = case_when(
    variable == "Emi_equ_CH4_AFO_Agr_Liv_Ent_Fer" ~ "Enteric",
    variable == "Emi_equ_CH4_AFO_Agr_Liv_Man_Man" ~ "Manure",
    variable == "Emi_equ_CH4_AFO_Agr_Ric" ~ "Rice",
    variable == "Emi_equ_N2O_AFO_Agr_Man_Soi" ~ "Managed soils",
    variable %in% c("Emi_CO2_Lan_Use_Flo_Pos_Emi_Lan_Use_Cha", "Emi_CO2_AFO_Lan_For_Gra_Con") ~ "Land-use change CO2",
    variable == "Emi_CO2_Lan_Use_Flo_Pos_Emi_Woo_Har" ~ "Wood harvest CO2",
    variable == "Emi_CO2_Lan_Use_Flo_Pos_Emi_Fir" ~ "Fire CO2",
    variable == "Emi_CO2_Lan_Use_Flo_RemForest" ~ "Forest sink CO2"
  )) %>%
  group_by(year, category) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop")

p_world_emissions <- ggplot(world_emissions, aes(x = factor(year), y = value, fill = category)) +
  geom_col() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40", linewidth = 0.35) +
  scale_fill_manual(values = emission_palette) +
  scale_y_continuous(labels = label_comma()) +
  labs(
    title = "World AFOLU emissions by source",
    x = "Year",
    y = "Emissions"
  ) +
  theme_main

save_plot("plot09_world_emissions.png", p_world_emissions, width = 8, height = 5)

# plot 9 ------------------------------------------------------------------
mac_reduction <- GEER_AFOLUB_REF %>%
  filter(
    scenario == scenario_afolub_default,
    region == "World",
    year %in% plot_years,
    variable %in% c(
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
      "Emi_Red_CO2_AFO_Lan_Ril",
      "Emi_Red_CO2_AFO_Lan_Enr",
      "Emi_Red_CO2_AFO_Lan_Agf",
      "Emi_Red_CO2_AFO_Lan_Bch",
      "Emi_Red_CO2_AFO_Lan_Scs",
      "Emi_Red_CO2_AFO_Lan_Scg",
      "Emi_Red_CO2_AFO_Lan_Scc"
    )
  ) %>%
  mutate(category = case_when(
    grepl("Liv_Ent_Fer", variable) ~ "Enteric",
    grepl("Liv_Man_Man", variable) ~ "Manure",
    grepl("Agr_Ric", variable) ~ "Rice",
    grepl("Man_Soi", variable) ~ "Managed soils",
    variable %in% c("Emi_Red_CO2_AFO_Lan_Pla", "Emi_Red_CO2_AFO_Lan_Ref", "Emi_Red_CO2_AFO_Lan_Agf") ~ "Forest restore",
    variable %in% c("Emi_Red_CO2_AFO_Lan_FPr", "Emi_Red_CO2_AFO_Lan_Pff", "Emi_Red_CO2_AFO_Lan_Ril", "Emi_Red_CO2_AFO_Lan_Enr") ~ "Forest protect/manage",
    TRUE ~ "Biochar/soil carbon"
  )) %>%
  group_by(year, category) %>%
  summarise(reduction = sum(value, na.rm = TRUE), .groups = "drop")

mac_cost <- GEER_AFOLUB_REF %>%
  filter(
    scenario == scenario_afolub_default,
    region == "World",
    year %in% plot_years,
    variable %in% c(
      "Red_Cos_AFO_Agr_Liv_Ent_Fer_Bre",
      "Red_Cos_AFO_Agr_Liv_Ent_Fer_Fee",
      "Red_Cos_AFO_Agr_Liv_Man_Man_Adc",
      "Red_Cos_AFO_Agr_Liv_Man_Man_Ams",
      "Red_Cos_AFO_Agr_Liv_Man_Man_Cfl",
      "Red_Cos_AFO_Agr_Liv_Man_Man_Cad",
      "Red_Cos_AFO_Agr_Liv_Man_Man_And",
      "Red_Cos_AFO_Agr_Ric_Res",
      "Red_Cos_AFO_Agr_Man_Soi_Fer",
      "Red_Cos_AFO_Agr_Man_Soi_Res",
      "Red_Cos_AFO_Agr_Man_Soi_Bio",
      "Red_Cos_AFO_Agr_Man_Soi_Slo",
      "Red_Cos_AFO_Agr_Man_Soi_Nin",
      "Red_Cos_AFO_Lan_Pla",
      "Red_Cos_AFO_Lan_Ref",
      "Red_Cos_AFO_Lan_FPr",
      "Red_Cos_AFO_Lan_Pff",
      "Red_Cos_AFO_Lan_Ril",
      "Red_Cos_AFO_Lan_Enr",
      "Red_Cos_AFO_Lan_Agf",
      "Red_Cos_AFO_Lan_Bch",
      "Red_Cos_AFO_Lan_Scg",
      "Red_Cos_AFO_Lan_Scc"
    )
  ) %>%
  mutate(category = case_when(
    grepl("Liv_Ent_Fer", variable) ~ "Enteric",
    grepl("Liv_Man_Man", variable) ~ "Manure",
    grepl("Agr_Ric", variable) ~ "Rice",
    grepl("Man_Soi", variable) ~ "Managed soils",
    variable %in% c("Red_Cos_AFO_Lan_Pla", "Red_Cos_AFO_Lan_Ref", "Red_Cos_AFO_Lan_Agf") ~ "Forest restore",
    variable %in% c("Red_Cos_AFO_Lan_FPr", "Red_Cos_AFO_Lan_Pff", "Red_Cos_AFO_Lan_Ril", "Red_Cos_AFO_Lan_Enr") ~ "Forest protect/manage",
    TRUE ~ "Biochar/soil carbon"
  )) %>%
  group_by(year, category) %>%
  summarise(cost = sum(value, na.rm = TRUE), .groups = "drop")

mac_curve <- mac_reduction %>%
  inner_join(mac_cost, by = c("year", "category")) %>%
  filter(reduction > 0) %>%
  group_by(year) %>%
  arrange(cost / reduction, .by_group = TRUE) %>%
  mutate(
    unit_cost = cost / reduction,
    cum_reduction = cumsum(reduction),
    reduction_share = cum_reduction / sum(reduction, na.rm = TRUE)
  ) %>%
  ungroup()

p_mac <- ggplot(mac_curve, aes(x = reduction_share, y = unit_cost / 1000, color = category)) +
  geom_point(size = 2.5) +
  facet_wrap(~ year, scales = "free_y") +
  scale_color_manual(values = category_palette) +
  scale_x_continuous(labels = percent_format()) +
  labs(
    title = "Category-level MAC",
    x = "Cumulative reduction share",
    y = "Cost / reduction"
  ) +
  theme_main

save_plot("plot10_mac_curve.png", p_mac, width = 10, height = 5)

# plot 10 -----------------------------------------------------------------
emission_compare_2005 <- GEER_AFOLUB_REF %>%
  filter(
    region == "World",
    year == 2005,
    scenario %in% c(scenario_afolub_default, "FAO-FAOTier1", "EDGAR"),
    variable %in% c(
      "Emi_equ_CH4_AFO_Agr_Liv_Ent_Fer",
      "Emi_equ_CH4_AFO_Agr_Liv_Man_Man",
      "Emi_equ_N2O_AFO_Agr_Man_Soi",
      "Emi_equ_CH4_AFO_Agr_Ric",
      "Emi_equ_N2O_AFO_Agr_Liv_Man_Man",
      "Emi_CO2_Lan_Use_Flo_Pos_Emi_Lan_Use_Cha",
      "Emi_CO2_AFO_Lan_For_Gra_Con",
      "Emi_CO2_Lan_Use_Flo_Pos_Emi_Fir",
      "Emi_CO2_Lan_Use_Flo_RemForest"
    )
  ) %>%
  mutate(
    scenario_label = recode(
      scenario,
      !!scenario_afolub_default := "AFOLUB",
      "FAO-FAOTier1" = "FAOSTAT",
      "EDGAR" = "EDGAR"
    ),
    group = case_when(
      variable %in% c("Emi_CO2_Lan_Use_Flo_Pos_Emi_Lan_Use_Cha", "Emi_CO2_AFO_Lan_For_Gra_Con", "Emi_CO2_Lan_Use_Flo_Pos_Emi_Fir", "Emi_CO2_Lan_Use_Flo_RemForest") ~ "Land use change",
      TRUE ~ "Agriculture"
    ),
    component = case_when(
      variable == "Emi_equ_CH4_AFO_Agr_Liv_Ent_Fer" ~ "Enteric",
      variable %in% c("Emi_equ_CH4_AFO_Agr_Liv_Man_Man", "Emi_equ_N2O_AFO_Agr_Liv_Man_Man") ~ "Manure",
      variable == "Emi_equ_CH4_AFO_Agr_Ric" ~ "Rice",
      variable == "Emi_equ_N2O_AFO_Agr_Man_Soi" ~ "Managed soils",
      variable %in% c("Emi_CO2_Lan_Use_Flo_Pos_Emi_Lan_Use_Cha", "Emi_CO2_AFO_Lan_For_Gra_Con") ~ "Land-use change",
      variable == "Emi_CO2_Lan_Use_Flo_Pos_Emi_Fir" ~ "Fire",
      variable == "Emi_CO2_Lan_Use_Flo_RemForest" ~ "Forest sink"
    )
  ) %>%
  group_by(group, scenario_label, component) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop")

p_emission_compare_ag <- emission_compare_2005 %>%
  filter(group == "Agriculture") %>%
  ggplot(aes(x = scenario_label, y = value / 1000, fill = component)) +
  geom_col() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40", linewidth = 0.35) +
  scale_fill_manual(values = emission_palette) +
  labs(
    title = "Agricultural emissions in 2005",
    x = NULL,
    y = "GtCO2eq/yr"
  ) +
  theme_main

p_emission_compare_lu <- emission_compare_2005 %>%
  filter(group == "Land use change") %>%
  ggplot(aes(x = scenario_label, y = value / 1000, fill = component)) +
  geom_col() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40", linewidth = 0.35) +
  scale_fill_manual(values = emission_palette) +
  labs(
    title = "Land-use emissions in 2005",
    x = NULL,
    y = "GtCO2eq/yr"
  ) +
  theme_main

save_plot("plot11_emission_compare_2005.png", patchwork::wrap_plots(p_emission_compare_ag, p_emission_compare_lu, ncol = 2), width = 12, height = 5)

# plot 11 -----------------------------------------------------------------
r5_ref_2100 <- GEER_AFOLUB_REF %>%
  filter(
    scenario == scenario_afolub_default,
    region %in% c("R5REF", "R5LAM", "R5ASIA", "R5OECD90+EU", "R5MAF"),
    year == 2100,
    variable %in% c(
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
      "Emi_Red_CO2_AFO_Lan_Ril",
      "Emi_Red_CO2_AFO_Lan_Enr",
      "Emi_Red_CO2_AFO_Lan_Agf",
      "Emi_Red_CO2_AFO_Lan_Bch",
      "Emi_Red_CO2_AFO_Lan_Scs",
      "Emi_Red_CO2_AFO_Lan_Scg",
      "Emi_Red_CO2_AFO_Lan_Scc"
    )
  ) %>%
  mutate(
    region_name = recode(region, !!!r5_labels),
    category = case_when(
      grepl("^Emi_Red_equ_", variable) ~ "Agriculture",
      variable %in% c("Emi_Red_CO2_AFO_Lan_Pla", "Emi_Red_CO2_AFO_Lan_Ref", "Emi_Red_CO2_AFO_Lan_Agf", "Emi_Red_CO2_AFO_Lan_Bch", "Emi_Red_CO2_AFO_Lan_Scs", "Emi_Red_CO2_AFO_Lan_Scg", "Emi_Red_CO2_AFO_Lan_Scc") ~ "LULUCF (CDR)",
      TRUE ~ "LULUCF (other)"
    )
  ) %>%
  group_by(region_name, category) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop")

p_r5_ref_2100 <- ggplot(r5_ref_2100, aes(x = region_name, y = value / 1000, fill = category)) +
  geom_col() +
  scale_fill_manual(values = category_palette) +
  labs(
    title = "R5 mitigation potential in 2100",
    x = NULL,
    y = "GtCO2eq/yr"
  ) +
  theme_main +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

save_plot("plot12_r5_ref_2100.png", p_r5_ref_2100, width = 8, height = 5)

# plot 12 -----------------------------------------------------------------
top20_ref_2100 <- GEER_AFOLUB_REF %>%
  filter(
    scenario == scenario_afolub_default,
    region %in% country_codes,
    year == 2100,
    variable %in% c(
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
      "Emi_Red_CO2_AFO_Lan_Ril",
      "Emi_Red_CO2_AFO_Lan_Enr",
      "Emi_Red_CO2_AFO_Lan_Agf",
      "Emi_Red_CO2_AFO_Lan_Bch",
      "Emi_Red_CO2_AFO_Lan_Scs",
      "Emi_Red_CO2_AFO_Lan_Scg",
      "Emi_Red_CO2_AFO_Lan_Scc"
    )
  ) %>%
  mutate(category = case_when(
    grepl("^Emi_Red_equ_", variable) ~ "Agriculture",
    variable %in% c("Emi_Red_CO2_AFO_Lan_Pla", "Emi_Red_CO2_AFO_Lan_Ref", "Emi_Red_CO2_AFO_Lan_Agf", "Emi_Red_CO2_AFO_Lan_Bch", "Emi_Red_CO2_AFO_Lan_Scs", "Emi_Red_CO2_AFO_Lan_Scg", "Emi_Red_CO2_AFO_Lan_Scc") ~ "LULUCF (CDR)",
    TRUE ~ "LULUCF (other)"
  )) %>%
  group_by(region, category) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  group_by(region) %>%
  mutate(total = sum(value, na.rm = TRUE)) %>%
  ungroup() %>%
  slice_max(order_by = total, n = 20, with_ties = FALSE) %>%
  left_join(st_drop_geometry(Boundary), by = c("region" = "cty")) %>%
  mutate(country = coalesce(country_name, region)) %>%
  mutate(country = fct_reorder(country, total))

p_top20_ref_2100 <- ggplot(top20_ref_2100, aes(x = value, y = country, fill = category)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40", linewidth = 0.35) +
  geom_col() +
  scale_fill_manual(values = category_palette) +
  labs(
    title = "Top 20 countries in 2100",
    x = "Mitigation potential",
    y = NULL
  ) +
  theme_main

save_plot("plot13_top20_ref_2100.png", p_top20_ref_2100, width = 8, height = 8)

# plot 13 -----------------------------------------------------------------
top20_intensity_2100 <- top20_ref_2100 %>%
  group_by(region, country) %>%
  summarise(total = sum(value, na.rm = TRUE), .groups = "drop") %>%
  left_join(st_drop_geometry(Boundary) %>% select(cty, pop_est), by = c("region" = "cty")) %>%
  mutate(intensity = total * 1e6 / pop_est) %>%
  filter(is.finite(intensity), pop_est > 0) %>%
  mutate(country = fct_reorder(country, intensity))

p_top20_intensity_2100 <- ggplot(top20_intensity_2100, aes(x = intensity, y = country)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40", linewidth = 0.35) +
  geom_col(fill = "grey55") +
  scale_x_continuous(labels = label_comma()) +
  labs(
    title = "Top 20 mitigation intensity in 2100",
    x = "tCO2eq/cap/yr",
    y = NULL
  ) +
  theme_main

save_plot("plot14_top20_and_intensity_2100.png", patchwork::wrap_plots(p_top20_ref_2100, p_top20_intensity_2100, ncol = 2), width = 14, height = 8)

# plot 14 -----------------------------------------------------------------
r5_time_series <- GEER_AFOLUB_REF %>%
  filter(
    scenario == scenario_afolub_default,
    region %in% c("R5REF", "R5LAM", "R5ASIA", "R5OECD90+EU", "R5MAF"),
    year >= 2030,
    variable %in% c(
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
      "Emi_Red_CO2_AFO_Lan_Ril",
      "Emi_Red_CO2_AFO_Lan_Enr",
      "Emi_Red_CO2_AFO_Lan_Agf",
      "Emi_Red_CO2_AFO_Lan_Bch",
      "Emi_Red_CO2_AFO_Lan_Scs",
      "Emi_Red_CO2_AFO_Lan_Scg",
      "Emi_Red_CO2_AFO_Lan_Scc"
    )
  ) %>%
  mutate(
    region_name = recode(region, !!!r5_labels),
    category = afolub_broad_group(variable)
  ) %>%
  filter(!is.na(category)) %>%
  group_by(region_name, year, category) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop")

p_r5_time_series <- ggplot(r5_time_series, aes(x = factor(year), y = value / 1000, fill = category)) +
  geom_col() +
  scale_fill_manual(values = category_palette) +
  facet_wrap(~ region_name, ncol = 3, scales = "free_y") +
  labs(
    title = "R5 mitigation pathways",
    x = "Year",
    y = "GtCO2eq/yr"
  ) +
  theme_main +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

save_plot("plot15_r5_time_series.png", p_r5_time_series, width = 12, height = 8)

# plot 15 -----------------------------------------------------------------
world_time_series <- GEER_AFOLUB_REF %>%
  filter(
    scenario == scenario_afolub_default,
    region == "World",
    year >= 2030,
    variable %in% c(
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
      "Emi_Red_CO2_AFO_Lan_Ril",
      "Emi_Red_CO2_AFO_Lan_Enr",
      "Emi_Red_CO2_AFO_Lan_Agf",
      "Emi_Red_CO2_AFO_Lan_Bch",
      "Emi_Red_CO2_AFO_Lan_Scs",
      "Emi_Red_CO2_AFO_Lan_Scg",
      "Emi_Red_CO2_AFO_Lan_Scc"
    )
  ) %>%
  mutate(category = afolub_broad_group(variable)) %>%
  filter(!is.na(category)) %>%
  group_by(year, category) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop")

p_world_time_series <- ggplot(world_time_series, aes(x = factor(year), y = value / 1000, fill = category)) +
  geom_col() +
  scale_fill_manual(values = category_palette) +
  labs(
    title = "World mitigation pathways",
    x = "Year",
    y = "GtCO2eq/yr"
  ) +
  theme_main

save_plot("plot16_world_time_series.png", p_world_time_series, width = 8, height = 5)

# plot 16 -----------------------------------------------------------------
world_top_tech_2100 <- GEER_AFOLUB_REF %>%
  filter(
    scenario == scenario_afolub_default,
    region == "World",
    year == 2100,
    variable %in% c(
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
      "Emi_Red_CO2_AFO_Lan_Ril",
      "Emi_Red_CO2_AFO_Lan_Enr",
      "Emi_Red_CO2_AFO_Lan_Agf",
      "Emi_Red_CO2_AFO_Lan_Bch",
      "Emi_Red_CO2_AFO_Lan_Scs",
      "Emi_Red_CO2_AFO_Lan_Scg",
      "Emi_Red_CO2_AFO_Lan_Scc"
    )
  ) %>%
  mutate(technology = afolub_tech_name(variable)) %>%
  group_by(technology) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  slice_max(order_by = value, n = 15, with_ties = FALSE) %>%
  mutate(technology = fct_reorder(technology, value))

p_world_top_tech_2100 <- ggplot(world_top_tech_2100, aes(x = value / 1000, y = technology)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40", linewidth = 0.35) +
  geom_col(fill = "#4C78A8") +
  labs(
    title = "Top technologies in 2100",
    x = "GtCO2eq/yr",
    y = NULL
  ) +
  theme_main

save_plot("plot17_world_top_tech_2100.png", p_world_top_tech_2100, width = 8, height = 6)

# hasegawa2025 fig 6a ------------------------------------------------------
fig6a_bar <- GEER_AFOLUB_ALL %>%
  transmute(
    scenario = as.character(Scenario),
    region = as.character(REMF),
    variable = as.character(VEMF),
    year = as.integer(as.character(YEMF)),
    value = as.numeric(IAMC_templatePlus)
  ) %>%
  filter(
    region == "World",
    scenario == scenario_baseline,
    year %in% c(2005L, 2020L, 2030L, 2050L, 2100L),
    variable %in% c(
      "Emi_equ_CH4_AFO_Agr_Liv_Ent_Fer",
      "Emi_equ_CH4_AFO_Agr_Liv_Man_Man",
      "Emi_equ_CH4_AFO_Agr_Ric",
      "Emi_equ_N2O_AFO_Agr_Liv_Man_Man",
      "Emi_equ_N2O_AFO_Agr_Man_Soi",
      "Emi_CO2_Lan_Use_Flo_Pos_Emi_Lan_Use_Cha",
      "Emi_CO2_AFO_Lan_For_Bur",
      "Emi_CO2_Lan_Use_Flo_Pos_Emi_Fir",
      "Emi_CO2_Lan_Use_Flo_RemForest"
    )
  ) %>%
  mutate(
    component = recode(
      variable,
      Emi_equ_CH4_AFO_Agr_Liv_Ent_Fer = "Enteric",
      Emi_equ_CH4_AFO_Agr_Liv_Man_Man = "Manure CH4",
      Emi_equ_CH4_AFO_Agr_Ric = "Rice",
      Emi_equ_N2O_AFO_Agr_Liv_Man_Man = "Manure N2O",
      Emi_equ_N2O_AFO_Agr_Man_Soi = "Managed soils",
      Emi_CO2_Lan_Use_Flo_Pos_Emi_Lan_Use_Cha = "Land-use change",
      Emi_CO2_AFO_Lan_For_Bur = "Forest burning",
      Emi_CO2_Lan_Use_Flo_Pos_Emi_Fir = "Fire",
      Emi_CO2_Lan_Use_Flo_RemForest = "Remaining forest"
    )
  )

fig6a_line <- GEER_AFOLUB_ALL %>%
  transmute(
    scenario = as.character(Scenario),
    region = as.character(REMF),
    variable = as.character(VEMF),
    year = as.integer(as.character(YEMF)),
    value = as.numeric(IAMC_templatePlus)
  ) %>%
  filter(
    region == "World",
    scenario %in% c(scenario_baseline, scenario_no, scenario_landint),
    variable == "Emi_equ_GHG_AFO",
    year %in% c(2005L, 2020L, 2030L, 2050L, 2100L)
  ) %>%
  mutate(
    scenario_label = recode(
      scenario,
      !!scenario_baseline := "BaU",
      !!scenario_no := "No",
      !!scenario_landint := "LandInt"
    )
  )

p_hasegawa_fig6a <- ggplot() +
  geom_col(
    data = fig6a_bar,
    aes(x = factor(year), y = value / 1000, fill = component),
    position = "stack"
  ) +
  geom_line(
    data = fig6a_line,
    aes(x = factor(year), y = value / 1000, group = scenario_label, color = scenario_label),
    linewidth = 0.9
  ) +
  geom_point(
    data = fig6a_line,
    aes(x = factor(year), y = value / 1000, color = scenario_label),
    size = 1.7
  ) +
  geom_hline(yintercept = 0, color = "grey45", linewidth = 0.35) +
  scale_fill_brewer(palette = "Set3") +
  scale_color_manual(values = c("BaU" = "#3873AD", "No" = "#8C8C8C", "LandInt" = "#C3327A")) +
  labs(
    title = "Hasegawa2025 Fig.6a-like",
    x = "Year",
    y = "GtCO2eq/yr"
  ) +
  theme_main

save_plot("plot18_hasegawa_fig6a_like.png", p_hasegawa_fig6a, width = 8, height = 5)

# hasegawa2025 fig 6b ------------------------------------------------------
fig6b_df <- GEER_AFOLUB_ALL %>%
  transmute(
    scenario = as.character(Scenario),
    region = as.character(REMF),
    variable = as.character(VEMF),
    year = as.integer(as.character(YEMF)),
    value = as.numeric(IAMC_templatePlus)
  ) %>%
  filter(
    scenario == scenario_baseline,
    region %in% c("R5OECD90+EU", "R5REF", "R5ASIA", "R5MAF", "R5LAM"),
    variable == "Emi_equ_GHG_AFO",
    year %in% c(2005L, 2020L, 2030L, 2050L, 2100L)
  ) %>%
  mutate(region_label = recode(region, !!!r5_labels))

p_hasegawa_fig6b <- ggplot(fig6b_df, aes(x = factor(year), y = value / 1000, fill = region_label)) +
  geom_col() +
  geom_hline(yintercept = 0, color = "grey45", linewidth = 0.35) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Hasegawa2025 Fig.6b-like",
    x = "Year",
    y = "GtCO2eq/yr"
  ) +
  theme_main

save_plot("plot19_hasegawa_fig6b_like.png", p_hasegawa_fig6b, width = 8, height = 5)

# hasegawa2025 fig 7a ------------------------------------------------------
fig7a_df <- GEER_AFOLUB_ALL %>%
  transmute(
    scenario = as.character(Scenario),
    region = as.character(REMF),
    variable = as.character(VEMF),
    year = as.integer(as.character(YEMF)),
    value = as.numeric(IAMC_templatePlus)
  ) %>%
  filter(
    region == "World",
    scenario %in% c(scenario_no, scenario_landint),
    year %in% c(2030L, 2050L, 2100L),
    variable %in% c(
      "Emi_Red_equ_CH4_AFO_Agr_Liv_Ent_Fer",
      "Emi_Red_equ_CH4_AFO_Agr_Liv_Man_Man",
      "Emi_Red_equ_CH4_AFO_Agr_Ric",
      "Emi_Red_equ_N2O_AFO_Agr_Liv_Man_Man",
      "Emi_Red_equ_N2O_AFO_Agr_Man_Soi",
      "Emi_Red_CO2_AFO_Lan_Pro",
      "Emi_Red_CO2_AFO_Lan_Res",
      "Emi_Red_CO2_AFO_Lan_Man",
      "Emi_Red_CO2_AFO_Lan_Enr",
      "Emi_Red_CO2_AFO_Lan_Agf",
      "Emi_Red_CO2_AFO_Lan_Bch",
      "Car_Seq_Lan_Use_Soi_Car_Man",
      "Car_Seq_Lan_Use_Soi_Car_Man_Cro",
      "Car_Seq_Lan_Use_Soi_Car_Man_Gra",
      "Emi_CO2_AFO_Aff",
      "Emi_Red_GHG_AFO_Die_Cha"
    )
  ) %>%
  mutate(
    scenario_label = recode(
      scenario,
      !!scenario_no := "No",
      !!scenario_landint := "LandInt"
    ),
    component = case_when(
      grepl("Liv_Ent_Fer", variable) ~ "Enteric",
      grepl("Liv_Man_Man", variable) ~ "Manure",
      grepl("Agr_Ric", variable) ~ "Rice",
      grepl("Man_Soi", variable) ~ "Managed soils",
      variable %in% c("Emi_Red_CO2_AFO_Lan_Pro", "Emi_Red_CO2_AFO_Lan_Man", "Emi_Red_CO2_AFO_Lan_Enr") ~ "Forest protect/manage",
      variable %in% c("Emi_Red_CO2_AFO_Lan_Res", "Emi_Red_CO2_AFO_Lan_Agf", "Emi_CO2_AFO_Aff") ~ "Forest restore",
      variable %in% c("Emi_Red_CO2_AFO_Lan_Bch", "Car_Seq_Lan_Use_Soi_Car_Man", "Car_Seq_Lan_Use_Soi_Car_Man_Cro", "Car_Seq_Lan_Use_Soi_Car_Man_Gra") ~ "Biochar/soil carbon",
      variable == "Emi_Red_GHG_AFO_Die_Cha" ~ "Dietary change",
      TRUE ~ variable
    )
  ) %>%
  group_by(scenario_label, year, component) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop")

p_hasegawa_fig7a <- ggplot(fig7a_df, aes(x = factor(year), y = value / 1000, fill = component)) +
  geom_col() +
  geom_hline(yintercept = 0, color = "grey45", linewidth = 0.35) +
  facet_wrap(~ scenario_label) +
  scale_fill_brewer(palette = "Set3") +
  labs(
    title = "Hasegawa2025 Fig.7a-like",
    x = "Year",
    y = "GtCO2eq/yr"
  ) +
  theme_main

save_plot("plot20_hasegawa_fig7a_like.png", p_hasegawa_fig7a, width = 10, height = 5)

# hasegawa2025 fig 7b ------------------------------------------------------
fig7b_df <- GEER_AFOLUB_ALL %>%
  transmute(
    scenario = as.character(Scenario),
    region = as.character(REMF),
    variable = as.character(VEMF),
    year = as.integer(as.character(YEMF)),
    value = as.numeric(IAMC_templatePlus)
  ) %>%
  filter(
    scenario %in% c(scenario_no, scenario_landint),
    region %in% c("R5OECD90+EU", "R5REF", "R5ASIA", "R5MAF", "R5LAM"),
    variable %in% c("Emi_GHG_AFO_Red", "Emi_GHG_AFO_Seq"),
    year %in% c(2030L, 2050L, 2100L)
  ) %>%
  mutate(
    scenario_label = recode(
      scenario,
      !!scenario_no := "No",
      !!scenario_landint := "LandInt"
    ),
    region_label = recode(region, !!!r5_labels)
  )

p_hasegawa_fig7b <- ggplot(fig7b_df, aes(x = factor(year), y = value / 1000, fill = region_label)) +
  geom_col() +
  geom_hline(yintercept = 0, color = "grey45", linewidth = 0.35) +
  facet_wrap(~ scenario_label) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Hasegawa2025 Fig.7b-like",
    x = "Year",
    y = "GtCO2eq/yr"
  ) +
  theme_main

save_plot("plot21_hasegawa_fig7b_like.png", p_hasegawa_fig7b, width = 10, height = 5)

# hasegawa2025 fig 7c ------------------------------------------------------
fig7c_df <- GEER_AFOLUB_ALL %>%
  transmute(
    scenario = as.character(Scenario),
    region = as.character(REMF),
    variable = as.character(VEMF),
    year = as.integer(as.character(YEMF)),
    value = as.numeric(IAMC_templatePlus)
  ) %>%
  filter(
    scenario %in% c(scenario_no, scenario_landint),
    region %in% c("R5OECD90+EU", "R5REF", "R5ASIA", "R5MAF", "R5LAM"),
    year %in% c(2050L, 2100L),
    variable %in% c(
      "Emi_Red_equ_CH4_AFO_Agr_Liv_Ent_Fer",
      "Emi_Red_equ_CH4_AFO_Agr_Liv_Man_Man",
      "Emi_Red_equ_CH4_AFO_Agr_Ric",
      "Emi_Red_equ_N2O_AFO_Agr_Liv_Man_Man",
      "Emi_Red_equ_N2O_AFO_Agr_Man_Soi",
      "Emi_Red_CO2_AFO_Lan_Pro",
      "Emi_Red_CO2_AFO_Lan_Res",
      "Emi_Red_CO2_AFO_Lan_Man",
      "Emi_Red_CO2_AFO_Lan_Enr",
      "Emi_Red_CO2_AFO_Lan_Agf",
      "Emi_Red_CO2_AFO_Lan_Bch",
      "Car_Seq_Lan_Use_Soi_Car_Man",
      "Car_Seq_Lan_Use_Soi_Car_Man_Cro",
      "Car_Seq_Lan_Use_Soi_Car_Man_Gra",
      "Emi_CO2_AFO_Aff",
      "Emi_Red_GHG_AFO_Die_Cha"
    )
  ) %>%
  mutate(
    scenario_label = recode(
      scenario,
      !!scenario_no := "No",
      !!scenario_landint := "LandInt"
    ),
    region_label = recode(region, !!!r5_labels),
    component = case_when(
      grepl("Liv_Ent_Fer", variable) ~ "Enteric",
      grepl("Liv_Man_Man", variable) ~ "Manure",
      grepl("Agr_Ric", variable) ~ "Rice",
      grepl("Man_Soi", variable) ~ "Managed soils",
      variable %in% c("Emi_Red_CO2_AFO_Lan_Pro", "Emi_Red_CO2_AFO_Lan_Man", "Emi_Red_CO2_AFO_Lan_Enr") ~ "Forest protect/manage",
      variable %in% c("Emi_Red_CO2_AFO_Lan_Res", "Emi_Red_CO2_AFO_Lan_Agf", "Emi_CO2_AFO_Aff") ~ "Forest restore",
      variable %in% c("Emi_Red_CO2_AFO_Lan_Bch", "Car_Seq_Lan_Use_Soi_Car_Man", "Car_Seq_Lan_Use_Soi_Car_Man_Cro", "Car_Seq_Lan_Use_Soi_Car_Man_Gra") ~ "Biochar/soil carbon",
      variable == "Emi_Red_GHG_AFO_Die_Cha" ~ "Dietary change",
      TRUE ~ variable
    )
  ) %>%
  group_by(scenario_label, year, region_label, component) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop")

p_hasegawa_fig7c <- ggplot(fig7c_df, aes(x = region_label, y = value / 1000, fill = component)) +
  geom_col() +
  geom_hline(yintercept = 0, color = "grey45", linewidth = 0.35) +
  facet_grid(scenario_label ~ year) +
  scale_fill_brewer(palette = "Set3") +
  labs(
    title = "Hasegawa2025 Fig.7c-like",
    x = NULL,
    y = "GtCO2eq/yr"
  ) +
  theme_main +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

save_plot("plot22_hasegawa_fig7c_like.png", p_hasegawa_fig7c, width = 10, height = 7)

# hasegawa2025 fig 8-like --------------------------------------------------
fig8_top_countries <- if (nrow(riso_top100) > 0) {
  riso_top100 %>%
    slice_head(n = 100)
} else {
  GEER_AFOLUB_ALL %>%
    transmute(
      scenario = as.character(Scenario),
      region = as.character(REMF),
      variable = as.character(VEMF),
      year = as.integer(as.character(YEMF)),
      value = as.numeric(IAMC_templatePlus)
    ) %>%
    filter(
      scenario == scenario_no,
      region %in% country_codes,
      year == 2020L,
      variable == "Emi_equ_GHG_AFO"
    ) %>%
    group_by(region) %>%
    summarise(total = sum(value, na.rm = TRUE), .groups = "drop") %>%
    slice_max(order_by = total, n = 30, with_ties = FALSE)
}

fig8_bar <- GEER_AFOLUB_ALL %>%
  transmute(
    scenario = as.character(Scenario),
    region = as.character(REMF),
    variable = as.character(VEMF),
    year = as.integer(as.character(YEMF)),
    value = as.numeric(IAMC_templatePlus)
  ) %>%
  filter(
    scenario == scenario_no,
    region %in% fig8_top_countries$region,
    year %in% c(2020L, 2050L, 2100L),
    variable %in% c(
      "Emi_equ_CH4_AFO_Agr_Liv_Ent_Fer",
      "Emi_equ_CH4_AFO_Agr_Liv_Man_Man",
      "Emi_equ_CH4_AFO_Agr_Ric",
      "Emi_equ_N2O_AFO_Agr_Liv_Man_Man",
      "Emi_equ_N2O_AFO_Agr_Man_Soi",
      "Emi_CO2_Lan_Use_Flo_Pos_Emi_Lan_Use_Cha",
      "Emi_CO2_AFO_Lan_For_Bur",
      "Emi_CO2_Lan_Use_Flo_Pos_Emi_Fir",
      "Emi_CO2_Lan_Use_Flo_RemForest"
    )
  ) %>%
  mutate(
    component = recode(
      variable,
      Emi_equ_CH4_AFO_Agr_Liv_Ent_Fer = "Enteric",
      Emi_equ_CH4_AFO_Agr_Liv_Man_Man = "Manure CH4",
      Emi_equ_CH4_AFO_Agr_Ric = "Rice",
      Emi_equ_N2O_AFO_Agr_Liv_Man_Man = "Manure N2O",
      Emi_equ_N2O_AFO_Agr_Man_Soi = "Managed soils",
      Emi_CO2_Lan_Use_Flo_Pos_Emi_Lan_Use_Cha = "Land-use change",
      Emi_CO2_AFO_Lan_For_Bur = "Forest burning",
      Emi_CO2_Lan_Use_Flo_Pos_Emi_Fir = "Fire",
      Emi_CO2_Lan_Use_Flo_RemForest = "Remaining forest"
    )
  ) %>%
  left_join(st_drop_geometry(Boundary), by = c("region" = "cty")) %>%
  mutate(country = coalesce(country_name, region))

fig8_line <- GEER_AFOLUB_ALL %>%
  transmute(
    scenario = as.character(Scenario),
    region = as.character(REMF),
    variable = as.character(VEMF),
    year = as.integer(as.character(YEMF)),
    value = as.numeric(IAMC_templatePlus)
  ) %>%
  filter(
    scenario %in% c(scenario_no, scenario_landint),
    region %in% fig8_top_countries$region,
    variable == "Emi_equ_GHG_AFO",
    year %in% c(2020L, 2050L, 2100L)
  ) %>%
  left_join(st_drop_geometry(Boundary), by = c("region" = "cty")) %>%
  mutate(
    country = coalesce(country_name, region),
    scenario_label = recode(
      scenario,
      !!scenario_no := "No",
      !!scenario_landint := "LandInt"
    )
  )

p_hasegawa_fig8 <- ggplot() +
  geom_col(
    data = fig8_bar,
    aes(x = factor(year), y = value / 1000, fill = component),
    position = "stack"
  ) +
  geom_line(
    data = fig8_line,
    aes(x = factor(year), y = value / 1000, group = scenario_label, color = scenario_label),
    linewidth = 0.8
  ) +
  geom_hline(yintercept = 0, color = "grey45", linewidth = 0.35) +
  facet_wrap(~ country, scales = "free_y", ncol = 5) +
  scale_fill_brewer(palette = "Set3") +
  scale_color_manual(values = c("No" = "#8C8C8C", "LandInt" = "#C3327A")) +
  labs(
    title = "Hasegawa2025 Fig.8-like",
    subtitle = if (nrow(riso_top100) > 0) "Using RISO_top100fao.txt" else "Adapted with top 30 No-scenario emitters in 2020",
    x = "Year",
    y = "GtCO2eq/yr"
  ) +
  theme_main +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 0)
  )

save_plot("plot23_hasegawa_fig8_like.png", p_hasegawa_fig8, width = 14, height = 16)

# hasegawa2025 fig 9-like --------------------------------------------------
fig9_df <- GEER_AFOLUB_ALL %>%
  transmute(
    scenario = as.character(Scenario),
    region = as.character(REMF),
    variable = as.character(VEMF),
    year = as.integer(as.character(YEMF)),
    value = as.numeric(IAMC_templatePlus)
  ) %>%
  filter(
    scenario == scenario_afolub_default,
    region %in% fig8_top_countries$region,
    year %in% decadal_years[decadal_years >= 2030],
    variable %in% c(
      "Emi_Red_equ_CH4_AFO_Agr_Liv_Ent_Fer",
      "Emi_Red_equ_CH4_AFO_Agr_Liv_Man_Man",
      "Emi_Red_equ_CH4_AFO_Agr_Ric",
      "Emi_Red_equ_N2O_AFO_Agr_Liv_Man_Man",
      "Emi_Red_equ_N2O_AFO_Agr_Man_Soi",
      "Emi_Red_CO2_AFO_Lan_Pro",
      "Emi_Red_CO2_AFO_Lan_Res",
      "Emi_Red_CO2_AFO_Lan_Man",
      "Emi_Red_CO2_AFO_Lan_Agf",
      "Emi_Red_CO2_AFO_Lan_Bch",
      "Emi_Red_CO2_AFO_Lan_Enr",
      "Car_Seq_Lan_Use_Soi_Car_Man",
      "Car_Seq_Lan_Use_Soi_Car_Man_Cro",
      "Car_Seq_Lan_Use_Soi_Car_Man_Gra",
      "Emi_CO2_AFO_Aff",
      "Emi_Red_CO2_AFO_Agr_Seq"
    )
  ) %>%
  mutate(
    component = case_when(
      grepl("Liv_Ent_Fer", variable) ~ "Enteric",
      grepl("Liv_Man_Man", variable) ~ "Manure",
      grepl("Agr_Ric", variable) ~ "Rice",
      grepl("Man_Soi", variable) ~ "Managed soils",
      variable %in% c("Emi_Red_CO2_AFO_Lan_Pro", "Emi_Red_CO2_AFO_Lan_Man", "Emi_Red_CO2_AFO_Lan_Enr") ~ "Forest protect/manage",
      variable %in% c("Emi_Red_CO2_AFO_Lan_Res", "Emi_Red_CO2_AFO_Lan_Agf", "Emi_CO2_AFO_Aff") ~ "Forest restore",
      variable %in% c("Emi_Red_CO2_AFO_Lan_Bch", "Car_Seq_Lan_Use_Soi_Car_Man", "Car_Seq_Lan_Use_Soi_Car_Man_Cro", "Car_Seq_Lan_Use_Soi_Car_Man_Gra", "Emi_Red_CO2_AFO_Agr_Seq") ~ "Biochar/soil carbon",
      TRUE ~ variable
    )
  ) %>%
  group_by(region, year, component) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  left_join(st_drop_geometry(Boundary), by = c("region" = "cty")) %>%
  mutate(country = coalesce(country_name, region))

p_hasegawa_fig9 <- ggplot(fig9_df, aes(x = factor(year), y = value / 1000, fill = component)) +
  geom_col() +
  geom_hline(yintercept = 0, color = "grey45", linewidth = 0.35) +
  facet_wrap(~ country, scales = "free_y", ncol = 5) +
  scale_fill_brewer(palette = "Set3") +
  labs(
    title = "Hasegawa2025 Fig.9-like",
    subtitle = if (nrow(riso_top100) > 0) "Using RISO_top100fao.txt" else "Adapted with top 30 No-scenario emitters in 2020",
    x = "Year",
    y = "GtCO2eq/yr"
  ) +
  theme_main +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 0)
  )

save_plot("plot24_hasegawa_fig9_like.png", p_hasegawa_fig9, width = 14, height = 16)

# plot 17 -----------------------------------------------------------------
make_bubble_plot <- function(plot_year, metric_name, x_label, title_text) {
  reduction_df <- GEER_AFOLUB_REF %>%
    filter(
      year == plot_year,
      region %in% country_codes,
      scenario %in% c(scenario_baseline, scenario_landint),
      variable %in% c("Emi_equ_GHG_AFO", "Emi_Red_equ_GHG_AFO_Agr", "Emi_Red_equ_GHG_AFO_Lan")
    ) %>%
    group_by(region, scenario, year, variable) %>%
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = variable, values_from = value) %>%
    transmute(
      region = region,
      year = year,
      scenario = scenario,
      total_afolu_emissions = Emi_equ_GHG_AFO,
      total_reduction = coalesce(Emi_Red_equ_GHG_AFO_Agr, 0) + coalesce(Emi_Red_equ_GHG_AFO_Lan, 0)
    ) %>%
    pivot_wider(names_from = scenario, values_from = c(total_afolu_emissions, total_reduction))

  agmip_change <- GEER_AGMIP %>%
    filter(
      year == plot_year,
      region %in% country_codes,
      (variable == "CALI" & item == "TOT" & unit == "kcal/cap/d") |
        (variable == "XPRP" & item == "AGR" & unit == "USD/t") |
        (variable == "PROD" & item == "LSP" & unit == "1000 t") |
        (variable == "PROD" & item == "CRP" & unit == "1000 t")
    ) %>%
    pivot_wider(names_from = scenario, values_from = value) %>%
    mutate(
      metric_name = case_when(
        variable == "CALI" ~ "calorie_change",
        variable == "XPRP" ~ "agr_price_change",
        variable == "PROD" & item == "LSP" ~ "lsp_prod_change",
        variable == "PROD" & item == "CRP" ~ "crp_prod_change"
      ),
      value = .data[[scenario_landint]] - .data[[scenario_baseline]]
    ) %>%
    select(region, year, metric_name, value) %>%
    pivot_wider(names_from = metric_name, values_from = value)

  land_change <- GEER_AGMIP %>%
    filter(
      year == plot_year,
      region %in% country_codes,
      variable == "LAND",
      unit == "1000 ha",
      item %in% c("CRP", "ECP", "FOR")
    ) %>%
    select(region, year, scenario, item, value) %>%
    pivot_wider(names_from = item, values_from = value, values_fill = 0) %>%
    mutate(CRP_NE = CRP - ECP) %>%
    select(region, year, scenario, CRP_NE, FOR) %>%
    pivot_longer(cols = c(CRP_NE, FOR), names_to = "metric_name", values_to = "value") %>%
    pivot_wider(names_from = scenario, values_from = value, values_fill = 0) %>%
    mutate(
      metric_name = recode(
        metric_name,
        CRP_NE = "cropland_area_change",
        FOR = "forest_area_change"
      ),
      value = .data[[scenario_landint]] - .data[[scenario_baseline]]
    ) %>%
    select(region, year, metric_name, value) %>%
    pivot_wider(names_from = metric_name, values_from = value)

  bubble_df <- reduction_df %>%
    transmute(
      region = region,
      year = year,
      total_reduction = .data[[paste0("total_reduction_", scenario_landint)]],
      bau_total_afolu_emissions = coalesce(
        .data[[paste0("total_afolu_emissions_", scenario_baseline)]],
        .data[[paste0("total_afolu_emissions_", scenario_landint)]] +
          .data[[paste0("total_reduction_", scenario_landint)]]
      )
    ) %>%
    left_join(agmip_change, by = c("region", "year")) %>%
    left_join(land_change, by = c("region", "year")) %>%
    inner_join(region_map, by = "region") %>%
    left_join(st_drop_geometry(Boundary), by = c("region" = "cty")) %>%
    mutate(
      country = coalesce(country_name, region),
      region5 = factor(region5, levels = names(region_palette)),
      reduction_rate_pct = 100 * total_reduction / bau_total_afolu_emissions
    ) %>%
    filter(
      is.finite(reduction_rate_pct),
      is.finite(.data[[metric_name]]),
      !is.na(total_reduction),
      !is.na(bau_total_afolu_emissions),
      bau_total_afolu_emissions != 0
    ) %>%
    mutate(
      size_bin = cut(
        total_reduction,
        breaks = c(-Inf, 5, 10, 15, 20, Inf),
        include.lowest = TRUE,
        right = FALSE,
        labels = c(
          "0-5 MtCO2eq/yr",
          "5-10 MtCO2eq/yr",
          "10-15 MtCO2eq/yr",
          "15-20 MtCO2eq/yr",
          "20+ MtCO2eq/yr"
        )
      ),
      size_alpha = case_when(
        size_bin == "0-5 MtCO2eq/yr" ~ 0.50,
        size_bin == "5-10 MtCO2eq/yr" ~ 0.60,
        size_bin == "10-15 MtCO2eq/yr" ~ 0.68,
        size_bin == "15-20 MtCO2eq/yr" ~ 0.76,
        size_bin == "20+ MtCO2eq/yr" ~ 0.84,
        TRUE ~ 0.65
      )
    )

  if (nrow(bubble_df) == 0) {
    return(
      ggplot() +
        annotate("text", x = 0, y = 0, label = paste("No data:", plot_year, metric_name), size = 4) +
        labs(
          title = paste0(title_text, " (", plot_year, ")"),
          x = x_label,
          y = "Reduction rate (%)"
        ) +
        theme_main
    )
  }

  label_df <- bubble_df %>%
    filter(size_bin %in% c("15-20 MtCO2eq/yr", "20+ MtCO2eq/yr"))

  y_nudge <- diff(range(bubble_df$reduction_rate_pct, na.rm = TRUE)) * 0.03
  if (!is.finite(y_nudge) || y_nudge == 0) {
    y_nudge <- 0
  }

  ggplot(bubble_df, aes(x = .data[[metric_name]], y = reduction_rate_pct)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
    geom_point(
      aes(size = size_bin, fill = region5, color = region5, alpha = size_bin),
      shape = 21,
      stroke = 0.35,
      show.legend = TRUE
    ) +
    geom_text(
      data = label_df,
      aes(label = country, color = region5),
      size = 3.25,
      fontface = "bold",
      nudge_y = y_nudge,
      show.legend = FALSE
    ) +
    scale_fill_manual(values = region_palette, drop = FALSE) +
    scale_color_manual(values = region_border_palette, drop = FALSE) +
    scale_size_manual(
      values = c(2.5, 4, 5.5, 7, 8.5),
      drop = FALSE
    ) +
    scale_alpha_manual(
      values = c(
        "0-5 MtCO2eq/yr" = 0.50,
        "5-10 MtCO2eq/yr" = 0.60,
        "10-15 MtCO2eq/yr" = 0.68,
        "15-20 MtCO2eq/yr" = 0.76,
        "20+ MtCO2eq/yr" = 0.84
      ),
      guide = "none",
      drop = FALSE
    ) +
    labs(
      title = paste0(title_text, " (", plot_year, ")"),
      x = x_label,
      y = "Reduction rate (%)"
    ) +
    theme_main
}

save_plot("plot25_bubble_2050_calorie.png", make_bubble_plot(2050, "calorie_change", "Calorie change", "Reduction rate vs calorie change"), width = 8, height = 6)
save_plot("plot26_bubble_2050_agr_price.png", make_bubble_plot(2050, "agr_price_change", "AGR price change", "Reduction rate vs agricultural price change"), width = 8, height = 6)
save_plot("plot27_bubble_2050_forest.png", make_bubble_plot(2050, "forest_area_change", "Forest area change", "Reduction rate vs forest area change"), width = 8, height = 6)
save_plot("plot28_bubble_2050_cropland.png", make_bubble_plot(2050, "cropland_area_change", "Cropland area change", "Reduction rate vs cropland area change"), width = 8, height = 6)
save_plot("plot29_bubble_2050_lsp_prod.png", make_bubble_plot(2050, "lsp_prod_change", "LSP production change", "Reduction rate vs livestock production change"), width = 8, height = 6)
save_plot("plot30_bubble_2050_crp_prod.png", make_bubble_plot(2050, "crp_prod_change", "CRP production change", "Reduction rate vs crop production change"), width = 8, height = 6)

save_plot("plot31_bubble_2100_calorie.png", make_bubble_plot(2100, "calorie_change", "Calorie change", "Reduction rate vs calorie change"), width = 8, height = 6)
save_plot("plot32_bubble_2100_agr_price.png", make_bubble_plot(2100, "agr_price_change", "AGR price change", "Reduction rate vs agricultural price change"), width = 8, height = 6)
save_plot("plot33_bubble_2100_forest.png", make_bubble_plot(2100, "forest_area_change", "Forest area change", "Reduction rate vs forest area change"), width = 8, height = 6)
save_plot("plot34_bubble_2100_cropland.png", make_bubble_plot(2100, "cropland_area_change", "Cropland area change", "Reduction rate vs cropland area change"), width = 8, height = 6)
save_plot("plot35_bubble_2100_lsp_prod.png", make_bubble_plot(2100, "lsp_prod_change", "LSP production change", "Reduction rate vs livestock production change"), width = 8, height = 6)
save_plot("plot36_bubble_2100_crp_prod.png", make_bubble_plot(2100, "crp_prod_change", "CRP production change", "Reduction rate vs crop production change"), width = 8, height = 6)

# plot 18 -----------------------------------------------------------------
make_reduction_emissions_scatter <- function(plot_year) {
  scatter_df <- GEER_AFOLUB_REF %>%
    filter(
      year == plot_year,
      region %in% country_codes,
      scenario == scenario_afolub_default,
      variable %in% c("Emi_equ_GHG_AFO", "Emi_Red_equ_GHG_AFO_Agr", "Emi_Red_equ_GHG_AFO_Lan")
    ) %>%
    group_by(region, scenario, year, variable) %>%
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = variable, values_from = value) %>%
    transmute(
      region,
      year,
      emissions = Emi_equ_GHG_AFO,
      total_reduction = coalesce(Emi_Red_equ_GHG_AFO_Agr, 0) + coalesce(Emi_Red_equ_GHG_AFO_Lan, 0)
    ) %>%
    inner_join(region_map, by = "region") %>%
    left_join(st_drop_geometry(Boundary), by = c("region" = "cty")) %>%
    mutate(
      country = coalesce(country_name, region),
      region5 = factor(region5, levels = names(region_palette))
    ) %>%
    filter(
      is.finite(emissions),
      is.finite(total_reduction),
      !is.na(region5)
    )

  if (nrow(scatter_df) == 0) {
    return(
      ggplot() +
        annotate("text", x = 0, y = 0, label = paste("No data:", plot_year), size = 4) +
        labs(
          title = paste0("Reduction vs emissions (", plot_year, ")"),
          x = "AFOLU emissions",
          y = "Total reduction"
        ) +
        theme_main
    )
  }

  label_df <- scatter_df %>%
    slice_max(order_by = total_reduction, n = 12, with_ties = FALSE)

  ggplot(scatter_df, aes(x = emissions, y = total_reduction)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
    geom_point(aes(color = region5), size = 2.2, alpha = 0.8) +
    geom_text(
      data = label_df,
      aes(label = country, color = region5),
      size = 3.1,
      fontface = "bold",
      nudge_y = 0.02 * max(scatter_df$total_reduction, na.rm = TRUE),
      show.legend = FALSE
    ) +
    scale_color_manual(values = region_border_palette, drop = FALSE) +
    labs(
      title = paste0("Reduction vs emissions (", plot_year, ")"),
      x = "AFOLU emissions",
      y = "Total reduction"
    ) +
    theme_main
}

save_plot(
  "plot37_reduction_vs_emissions_2050_2100.png",
  patchwork::wrap_plots(
    make_reduction_emissions_scatter(2050),
    make_reduction_emissions_scatter(2100),
    ncol = 2
  ),
  width = 12,
  height = 6
)
