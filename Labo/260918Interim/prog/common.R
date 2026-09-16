suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(tidyr)
})

# Shared settings -------------------------------------------------------------

target_year <- as.integer(Sys.getenv("TARGET_YEAR", "2100"))
if (!target_year %in% c(2050L, 2100L)) {
  stop("TARGET_YEAR must be 2050 or 2100.")
}

data_dir <- "data"
output_dir <- "output"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Commodity groups follow define/ALPHA/3_equilibrium.gms. In particular,
# food_crop_codes matches a_cro_ne and therefore excludes energy crop ecr.
food_crop_codes <- c(
  "wht", "rce", "crl", "mze", "str", "sgr", "pls",
  "nut", "ocr", "vgt", "frt", "stm", "spc"
)
livestock_codes <- c("cmt", "omt", "pmt", "rmt", "mlk", "egg")
processed_codes <- c("alc", "vol", "swt", "dai")

selected_crop_codes <- c("wht", "rce", "mze", "crl", "ocr", "sgr")
selected_livestock_codes <- c("cmt", "pmt", "mlk")

commodity_labels <- c(
  wht = "Wheat",
  rce = "Rice",
  crl = "Other cereals",
  mze = "Maize",
  str = "Starchy roots",
  sgr = "Sugar crops",
  pls = "Pulses",
  nut = "Nuts",
  ocr = "Oil crops",
  vgt = "Vegetables",
  frt = "Fruits",
  stm = "Stimulants",
  spc = "Spices",
  cmt = "Cattle meat",
  omt = "Other meat",
  pmt = "Poultry meat",
  rmt = "Small ruminant meat",
  mlk = "Milk",
  egg = "Eggs",
  alc = "Alcoholic beverages",
  vol = "Vegetable oils",
  swt = "Sweeteners",
  dai = "Dairy products",
  ecr = "Energy crops"
)

# AgMIP market-output items. CRP contains the 13 non-energy crops and excludes
# ECP (energy crops). Detailed items intentionally follow AgMIP aggregation.
agmip_item_catalog <- tibble::tribble(
  ~item,      ~agmip_uel,  ~item_label,                              ~item_group, ~is_aggregate,
  "AGR",      "AGR",       "Agriculture",                            "aggregate", TRUE,
  "CRP",      "CRP",       "Crops (non-energy)",                     "aggregate", TRUE,
  "LSP",      "LSP",       "Livestock",                              "aggregate", TRUE,
  "WHT",      "wht",       "Wheat",                                  "crops",     FALSE,
  "RIC",      "RIC",       "Rice",                                   "crops",     FALSE,
  "CGR",      "CGR",       "Coarse grains",                          "crops",     FALSE,
  "OSD",      "OSD",       "Oilseeds",                               "crops",     FALSE,
  "SGC",      "SGC",       "Sugar crops",                            "crops",     FALSE,
  "VFN",      "VFN",       "Vegetables, fruits, nuts and other crops", "crops",   FALSE,
  "VFN|VEG",  "VFN|VEG",   "Vegetables",                             "crops",     FALSE,
  "VFN|FRU",  "VFN|FRU",   "Fruits",                                 "crops",     FALSE,
  "VFN|NUT",  "VFN|NUT",   "Nuts",                                   "crops",     FALSE,
  "OCR",      "ocr",       "Stimulants and spices",                  "crops",     FALSE,
  "RUM",      "RUM",       "Ruminant products",                      "livestock", FALSE,
  "NRM",      "NRM",       "Non-ruminant products",                  "livestock", FALSE,
  "NRM|PRK",  "NRM|PRK",   "Pork and other meat",                    "livestock", FALSE,
  "NRM|PTM",  "NRM|PTM",   "Poultry meat",                           "livestock", FALSE,
  "NRM|EGG",  "NRM|EGG",   "Eggs",                                   "livestock", FALSE,
  "DRY",      "DRY",       "Dairy",                                  "livestock", FALSE
)

agmip_indicator_order <- c(
  "Area", "Production", "Consumption", "Exports", "Imports",
  "Net exports", "Producer price", "Consumer price", "Food intake"
)

read_agmip_market <- function(level = c("country", "aggregate")) {
  level <- match.arg(level)
  agmip_file <- file.path(data_dir, "ALPHA", "AGMIP_all_scenario.gdx")
  symbol <- if (level == "country") "AGMIP" else "AGMIP_agg"
  region_set <- if (level == "country") "cty" else "reg"
  region_name <- if (level == "country") "country" else "region"
  regions <- as.character(gdxrrw::rgdx.set(agmip_file, region_set)[[1]])
  requested_variables <- c(
    "AREA", "PROD", "CONS", "EXPO", "IMPO", "NETT", "CALI"
  )
  requested_units <- c("1000 ha", "million kcal", "USD", "kcal/cap/d")
  requested_items <- unique(agmip_item_catalog$agmip_uel)
  states <- scenario_states("global2")
  request <- list(
    name = symbol,
    form = "full",
    uels = list(
      unname(states), regions, requested_variables, requested_items,
      requested_units, as.character(target_year)
    )
  )
  raw_array <- gdxrrw::rgdx(
    agmip_file, request, squeeze = FALSE, useDomInfo = TRUE
  )
  raw <- as.data.frame.table(
    raw_array$val, responseName = "value", stringsAsFactors = FALSE
  )
  names(raw) <- c(
    "scenario", region_name, "variable", "item", "source_unit", "year", "value"
  )
  raw <- raw %>%
    mutate(
      state = state_from_scenario(scenario, "global2"),
      item = toupper(item),
      year = as.integer(year),
      value = as.numeric(value)
    ) %>%
    select(-scenario, -year) %>%
    left_join(
      agmip_item_catalog %>% select(-agmip_uel), by = "item"
    )

  energy_quantities <- raw %>%
    filter(
      variable %in% c("PROD", "CONS", "EXPO", "IMPO", "NETT"),
      source_unit == "million kcal"
    ) %>%
    mutate(
      indicator = recode(
        variable,
        PROD = "Production", CONS = "Consumption", EXPO = "Exports",
        IMPO = "Imports", NETT = "Net exports"
      ),
      value = value * 4.184e-6,
      unit = "PJ/yr"
    )

  area <- raw %>%
    filter(variable == "AREA", source_unit == "1000 ha") %>%
    mutate(indicator = "Area", unit = "1000 ha")

  food_intake <- raw %>%
    filter(variable == "CALI", source_unit == "kcal/cap/d") %>%
    mutate(
      indicator = "Food intake",
      unit = "kcal/cap/day"
    )

  price_inputs <- raw %>%
    filter(
      variable %in% c("PROD", "CONS"),
      source_unit %in% c("million kcal", "USD")
    ) %>%
    select(
      all_of(region_name), state, item, item_label, item_group,
      is_aggregate, variable, source_unit, value
    ) %>%
    pivot_wider(names_from = source_unit, values_from = value)

  energy_prices <- price_inputs %>%
    filter(`million kcal` > 1e-10) %>%
    transmute(
      across(all_of(region_name)), state, item, item_label, item_group,
      is_aggregate, variable,
      indicator = if_else(variable == "PROD", "Producer price", "Consumer price"),
      source_unit = "AgMIP value / AgMIP caloric energy",
      value = USD / (`million kcal` * 4.184),
      unit = "USD/GJ"
    )

  bind_rows(energy_quantities, area, food_intake, energy_prices) %>%
    select(
      all_of(region_name), state, item, item_label, item_group,
      is_aggregate, indicator, unit, value
    ) %>%
    mutate(
      indicator = factor(indicator, levels = agmip_indicator_order)
    )
}

scenario_states <- function(global_version = "global2") {
  suffix <- paste0("_LandInt_", global_version)
  c(
    "000" = paste0("SSP2_BaU_NoCC", suffix),
    "100" = paste0("SSP2_protect_BaU_NoCC", suffix),
    "010" = paste0("SSP2NoBio_1100C_CACN_DAC_NoCC", suffix),
    "001" = paste0("SSP2NoAff_1100C_CACN_DAC_NoCC", suffix),
    "110" = paste0("SSP2NoBio_protect_1100C_CACN_DAC_NoCC", suffix),
    "101" = paste0("SSP2NoAff_protect_1100C_CACN_DAC_NoCC", suffix),
    "011" = paste0("SSP2_1100C_CACN_DAC_NoCC", suffix),
    "111" = paste0("SSP2_protect_1100C_CACN_DAC_NoCC", suffix)
  )
}

state_from_scenario <- function(x, global_version = "global2") {
  states <- scenario_states(global_version)
  unname(names(states)[match(x, states)])
}

init_gdx <- function() {
  gams_candidates <- c(
    Sys.getenv("GAMS_SYS_DIR", unset = NA_character_),
    if (nzchar(Sys.which("gams"))) dirname(Sys.which("gams")) else NA_character_,
    Sys.glob(path.expand("~/gams*_linux_x64_64_sfx"))
  )
  gams_candidates <- unique(gams_candidates[!is.na(gams_candidates)])
  gams_candidates <- gams_candidates[dir.exists(gams_candidates)]
  if (length(gams_candidates) == 0L) {
    stop("GAMS system directory was not found. Set GAMS_SYS_DIR.")
  }
  gdxrrw::igdx(gams_candidates[[1]], silent = TRUE)
}

shapley_three <- function(wide_data) {
  required_states <- c("000", "100", "010", "001", "110", "101", "011", "111")
  if (!all(required_states %in% names(wide_data))) {
    stop("All eight policy states are required for Shapley decomposition.")
  }

  wide_data %>%
    mutate(
      protection =
        (`100` - `000`) / 3 +
        ((`110` - `010`) + (`101` - `001`)) / 6 +
        (`111` - `011`) / 3,
      afforestation =
        (`010` - `000`) / 3 +
        ((`110` - `100`) + (`011` - `001`)) / 6 +
        (`111` - `101`) / 3,
      beccs =
        (`001` - `000`) / 3 +
        ((`101` - `100`) + (`011` - `010`)) / 6 +
        (`111` - `110`) / 3,
      total = `111` - `000`,
      baseline = `000`,
      final = `111`
    )
}

conditional_protection_factorial <- function(wide_data) {
  required_states <- c("000", "100", "010", "001", "110", "101", "011", "111")
  if (!all(required_states %in% names(wide_data))) {
    stop("All eight policy states are required for conditional protection decomposition.")
  }

  wide_data %>%
    mutate(
      protection_main = `100` - `000`,
      protection_x_afforestation = `110` - `100` - `010` + `000`,
      protection_x_beccs = `101` - `100` - `001` + `000`,
      protection_x_afforestation_x_beccs =
        `111` - `110` - `101` - `011` +
        `100` + `010` + `001` - `000`,
      protection_under_mitigation = `111` - `011`,
      mitigation_baseline = `011`,
      decomposition_residual = protection_under_mitigation -
        protection_main - protection_x_afforestation - protection_x_beccs -
        protection_x_afforestation_x_beccs
    )
}

symmetric_limit <- function(x, probability = 0.98) {
  values <- abs(x[is.finite(x) & x != 0])
  if (length(values) == 0L) return(1)
  max(as.numeric(stats::quantile(values, probability, na.rm = TRUE)), 1e-10)
}

theme_paper <- function(base_size = 10) {
  theme_minimal(base_size = base_size) +
    theme(
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      strip.text = element_text(face = "bold"),
      plot.title = element_blank()
    )
}
