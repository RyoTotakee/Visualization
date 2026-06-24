library(tidyverse)
library(gdxrrw)
library(ggplot2)
library(ggnewscale)

args <- commandArgs(trailingOnly = TRUE)
scenarios <- c(
  "SSP2_BaU_NoCC"
)

gams_sys_dir <- if (length(args) >= 1 && dir.exists(args[1])) args[1] else Sys.getenv("GAMS_SYS_DIR", "")
if (nzchar(gams_sys_dir)) {
  igdx(gams_sys_dir)
}

script_arg <- commandArgs(FALSE)
script_file <- sub("^--file=", "", script_arg[grepl("^--file=", script_arg)])
script_dir <- if (length(script_file) > 0) {
  dirname(normalizePath(script_file[1], winslash = "/", mustWork = TRUE))
} else {
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}
project_dir <- normalizePath(file.path(script_dir, ".."), winslash = "/", mustWork = TRUE)
data_dir <- file.path(project_dir, "data")
output_dir <- file.path(project_dir, "Output")

ssp_csv <- file.path(data_dir, "SSP_IAM_V2_201811.csv")
agmip_gdx <- file.path(data_dir, "AGMIP_all_scenario.gdx")
agmip_historical_gdx <- file.path(data_dir, "AGMIP_historical.gdx")
iamc_gdx <- file.path(data_dir, "IAMC_all_scenario.gdx")
historical_gdx <- file.path(data_dir, "IAMC_historical.gdx")
country_mapping_csv <- file.path(data_dir, "country_mapping.csv")

required_files <- c(ssp_csv, agmip_gdx, agmip_historical_gdx, iamc_gdx, historical_gdx)
missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0) {
  stop("Missing required data files:\n", paste(missing_files, collapse = "\n"))
}

theme_1 <- theme_bw() +
  theme(
    text = element_text(size = 20),
    axis.text.x = element_text(size = 15),
    legend.position = "bottom",
    legend.text = element_text(size = 15),
    legend.key.width = unit(1.2, "cm"),
    legend.box = "horizontal",
    legend.box.just = "center",
    axis.title.x = element_blank(),
    legend.title = element_blank(),
    strip.background = element_blank(),
    strip.clip = "off"
  )

detect_ssp_database_scenario <- function(scn) {
  ssp <- stringr::str_extract(scn, "SSP[1-5]")
  if (is.na(ssp)) {
    return(list(valid = FALSE, ssp = NA_character_, ssp_db = NA_character_))
  }
  if (stringr::str_detect(scn, "500C")) {
    ssp_db <- paste0(ssp, "-19")
  } else if (stringr::str_detect(scn, "BaU")) {
    ssp_db <- paste0(ssp, "-Baseline")
  } else {
    return(list(valid = FALSE, ssp = ssp, ssp_db = NA_character_))
  }
  list(valid = TRUE, ssp = ssp, ssp_db = ssp_db)
}

scenario_candidates <- function(scn) {
  sc_base <- sub("_World$", "", scn)
  unique(c(scn, sc_base))
}

filter_by_scenario <- function(df, scn) {
  for (s in scenario_candidates(scn)) {
    if (any(df$SCENARIO == s)) {
      return(df %>% filter(SCENARIO == s) %>% mutate(SCENARIO = scn))
    }
  }
  df[0, , drop = FALSE]
}

load_country_map <- function() {
  if (!file.exists(country_mapping_csv)) {
    stop(
      "AGMIP_all_scenario.gdx does not contain R5 regions, so aggregation needs this missing file:\n",
      country_mapping_csv
    )
  }
  readr::read_csv(
    country_mapping_csv,
    show_col_types = FALSE,
    col_types = readr::cols(
      country = readr::col_character(),
      region = readr::col_character()
    )
  )
}

aggregate_to_r5 <- function(df, value_col, group_cols) {
  map <- load_country_map()
  world <- df %>% filter(region == "World")
  r5 <- df %>%
    filter(region != "World") %>%
    mutate(region = as.character(region)) %>%
    left_join(map, by = c("region" = "country")) %>%
    filter(!is.na(region.y)) %>%
    mutate(region = region.y) %>%
    select(!region.y) %>%
    group_by(across(all_of(c(group_cols, "region")))) %>%
    summarise(!!value_col := sum(.data[[value_col]], na.rm = TRUE), .groups = "drop")
  bind_rows(r5, world)
}

recode_region <- function(df) {
  region_map <- c(
    "R5ASIA" = "Asia",
    "R5LAM" = "Latin America and the Caribbean",
    "R5MAF" = "Middle East and Africa",
    "R5OECD90+EU" = "OECD+EU",
    "R5REF" = "Eastern Europe and the Former Soviet Union"
  )
  df %>%
    mutate(region = as.character(region)) %>%
    mutate(region = dplyr::case_when(
      region %in% names(region_map) ~ region_map[region],
      TRUE ~ region
    ))
}

target_model <- "AIM-ALPHA"
r5_regions <- c("World", "R5ASIA", "R5LAM", "R5MAF", "R5OECD90+EU", "R5REF")
model_color_cmp <- c(
  "AIM-ALPHA" = "navy",
  "GCAM4" = "#6a6a6a",
  "MESSAGE-GLOBIOM" = "#6a6a6a",
  "AIM/CGE" = "#6a6a6a",
  "IMAGE" = "#6a6a6a",
  "REMIND-MAGPIE" = "#6a6a6a"
)
model_line_cmp <- c(
  "AIM-ALPHA" = 1.3,
  "GCAM4" = 1,
  "MESSAGE-GLOBIOM" = 1,
  "AIM/CGE" = 1,
  "IMAGE" = 1,
  "REMIND-MAGPIE" = 1
)
iam_shape_cmp <- c(
  "GCAM4" = 16,
  "MESSAGE-GLOBIOM" = 17,
  "AIM/CGE" = 15,
  "IMAGE" = 18,
  "REMIND-MAGPIE" = 3
)
hist_fill <- c(
  "FAOSTAT" = "#1F77B4",
  "LUH2" = "#FF7F0E",
  "EDGAR" = "#D62728"
)
aim_color <- c(
  "AIM-ALPHA backcast (<2015)" = "#8B0000",
  "AIM-ALPHA (2015 onward)" = "#001B44"
)
aim_shape <- c(
  "AIM-ALPHA backcast (<2015)" = 17,
  "AIM-ALPHA (2015 onward)" = 16
)

SSP <- readr::read_csv(ssp_csv, show_col_types = FALSE)

hist_faostat <- rgdx.param(historical_gdx, "IAMC_Template") %>%
  mutate(
    VARIABLE = as.character(IAMC_VAR),
    region = as.character(IAMC_REGION),
    Y = as.numeric(as.character(yr)),
    Model2 = "FAOSTAT",
    Historical = IAMC_Template
  ) %>%
  select(!c(IAMC_VAR, IAMC_REGION, yr, IAMC_Template)) %>%
  filter(Y <= 2024)

hist_luh2 <- rgdx.param(historical_gdx, "IAMC_Template_LUH2") %>%
  mutate(
    VARIABLE = as.character(IAMC_VAR),
    region = as.character(IAMC_REGION),
    Y = as.numeric(as.character(yr)),
    Model2 = "LUH2",
    Historical = IAMC_Template_LUH2
  ) %>%
  select(!c(IAMC_VAR, IAMC_REGION, yr, IAMC_Template_LUH2))

hist_edgar <- rgdx.param(historical_gdx, "IAMC_Template_EDGAR") %>%
  mutate(
    VARIABLE = as.character(IAMC_VAR),
    region = as.character(IAMC_REGION),
    Y = as.numeric(as.character(yr)),
    Model2 = "EDGAR",
    Historical = IAMC_Template_EDGAR
  ) %>%
  select(!c(IAMC_VAR, IAMC_REGION, yr, IAMC_Template_EDGAR)) %>%
  mutate(VARIABLE = recode(
    VARIABLE,
    "Emi_CH4_AFO_Agr" = "Emi_CH4_AFO",
    "Emi_N2O_AFO_Agr" = "Emi_N2O_AFO"
  ))

aimalpha_agmip_raw <- rgdx.param(agmip_gdx, "AGMIP_agg")
agmip_hist_raw <- rgdx.param(agmip_historical_gdx, "AGMIP_agg_hist")
iamc_all_raw <- rgdx.param(iamc_gdx, "IAMC_Template")

for (scenario in scenarios) {
  message("Start SSP_compare plots: ", scenario)

ssp_info <- detect_ssp_database_scenario(scenario)
if (!isTRUE(ssp_info$valid)) {
  stop("Scenario must include SSP[1-5] and either 500C or BaU: ", scenario)
}
ssp_db <- ssp_info$ssp_db

compare_dir <- file.path(output_dir, scenario, "SSP_compare")
long_dir <- file.path(compare_dir, "long_term")
near_dir <- file.path(compare_dir, "near_term")

ssp_agr_agmip <- SSP %>%
  filter(VARIABLE %in% c(
    "Agricultural Demand|Crops", "Agricultural Demand|Livestock",
    "Agricultural Production|Crops|Non-Energy", "Agricultural Production|Livestock",
    "Land Cover|Cropland", "Land Cover|Forest", "Land Cover|Pasture",
    "Emissions|CH4|Land Use", "Emissions|N2O|Land Use"
  )) %>%
  filter(MODEL != "AIM/CGE" | VARIABLE != "Agricultural Demand|Crops") %>%
  filter(SCENARIO == ssp_db) %>%
  mutate(region = recode(
    REGION,
    "R5.2ASIA" = "R5ASIA",
    "R5.2LAM" = "R5LAM",
    "R5.2MAF" = "R5MAF",
    "R5.2OECD" = "R5OECD90+EU",
    "R5.2REF" = "R5REF"
  )) %>%
  mutate(VARIABLE = recode(
    VARIABLE,
    "Agricultural Demand|Crops" = "CONS_CRP",
    "Agricultural Demand|Livestock" = "CONS_LSP",
    "Agricultural Production|Crops|Non-Energy" = "PROD_CRP",
    "Agricultural Production|Livestock" = "PROD_LSP",
    "Land Cover|Cropland" = "LAND_CRP",
    "Land Cover|Forest" = "LAND_FOR",
    "Land Cover|Pasture" = "LAND_GRS",
    "Emissions|CH4|Land Use" = "ECH4_TOT",
    "Emissions|N2O|Land Use" = "EN2O_TOT"
  )) %>%
  select(!c(SCENARIO, REGION, UNIT)) %>%
  select(!"2005") %>%
  pivot_longer(cols = -c(MODEL, region, VARIABLE), names_to = "Y", values_to = "AGMIP_agg") %>%
  mutate(Y = as.numeric(Y))

if (nrow(ssp_agr_agmip) == 0) {
  stop("SSP database has no rows for ", ssp_db)
}

ssp_range_agmip <- ssp_agr_agmip %>%
  pivot_wider(names_from = MODEL, values_from = AGMIP_agg)
model_cols <- setdiff(names(ssp_range_agmip), c("VARIABLE", "region", "Y"))
ssp_range_agmip <- ssp_range_agmip %>%
  mutate(
    IAM_max = apply(dplyr::select(., dplyr::all_of(model_cols)), 1, max, na.rm = TRUE),
    IAM_min = apply(dplyr::select(., dplyr::all_of(model_cols)), 1, min, na.rm = TRUE)
  ) %>%
  select(VARIABLE, region, Y, IAM_max, IAM_min) %>%
  na.omit()

aimalpha_agmip <- filter_by_scenario(aimalpha_agmip_raw, scenario) %>%
  filter(!AgMIP_UNIT %in% c("USD", "million kcal", "1000 t")) %>%
  filter(AgMIP_VAR %in% c("LAND", "PROD", "CONS", "ECH4", "EN2O")) %>%
  mutate(AGMIP_agg = AGMIP_agg / 1000) %>%
  unite(VARIABLE, AgMIP_VAR, ITEM, remove = TRUE, sep = "_") %>%
  mutate(VARIABLE = toupper(VARIABLE)) %>%
  mutate(MODEL = target_model) %>%
  mutate(Y = as.numeric(as.character(Y)))

if (nrow(aimalpha_agmip) == 0) {
  stop("AGMIP output has no rows for scenario: ", scenario)
}

aimalpha_agmip$AGMIP_agg[aimalpha_agmip$VARIABLE == "ECH4_TOT"] <-
  aimalpha_agmip$AGMIP_agg[aimalpha_agmip$VARIABLE == "ECH4_TOT"] / 28 * 1000
aimalpha_agmip$AGMIP_agg[aimalpha_agmip$VARIABLE == "EN2O_TOT"] <-
  aimalpha_agmip$AGMIP_agg[aimalpha_agmip$VARIABLE == "EN2O_TOT"] / 265 * 1000 * 1000

if (!any(aimalpha_agmip$region %in% r5_regions)) {
  aimalpha_agmip <- aggregate_to_r5(
    aimalpha_agmip,
    value_col = "AGMIP_agg",
    group_cols = c("SCENARIO", "VARIABLE", "AgMIP_UNIT", "Y", "MODEL")
  )
}
aimalpha_agmip <- aimalpha_agmip %>%
  filter(region %in% r5_regions) %>%
  select(!AgMIP_UNIT)

historical_agmip <- bind_rows(hist_faostat, hist_luh2, hist_edgar) %>%
  filter(Y >= 2000, Y <= 2025, region %in% r5_regions) %>%
  mutate(VARIABLE = recode(
    VARIABLE,
    "Agr_Dem_Cro" = "CONS_CRP",
    "Agr_Dem_Liv" = "CONS_LSP",
    "Agr_Pro_Cro" = "PROD_CRP",
    "Agr_Pro_Liv" = "PROD_LSP",
    "Lan_Cov_Cro" = "LAND_CRP",
    "Lan_Cov_Frs" = "LAND_FOR",
    "Lan_Cov_Pst" = "LAND_GRS",
    "Emi_CH4_AFO" = "ECH4_TOT",
    "Emi_N2O_AFO" = "EN2O_TOT"
  ))

var_map_agmip <- c(
  "CONS_CRP" = "Crop Consumption [million t-DM]",
  "CONS_LSP" = "Livestock Consumption [million t-DM]",
  "PROD_CRP" = "Crop Production [million t-DM]",
  "PROD_LSP" = "Livestock Production [million t-DM]",
  "LAND_CRP" = "Cropland [million ha]",
  "LAND_FOR" = "Forest [million ha]",
  "LAND_GRS" = "Pasture [million ha]",
  "ECH4_TOT" = "CH4 emission [Mt CH4/yr]",
  "EN2O_TOT" = "N2O emission [kt N2O/yr]"
)
var_levels_long <- c(
  "Crop Consumption [million t-DM]",
  "Crop Production [million t-DM]",
  "Cropland [million ha]",
  "Livestock Consumption [million t-DM]",
  "Livestock Production [million t-DM]",
  "Pasture [million ha]",
  "CH4 emission [Mt CH4/yr]",
  "N2O emission [kt N2O/yr]",
  "Forest [million ha]"
)
var_levels_near <- c(
  "Crop \nConsumption \n[million t-DM]",
  "Crop \nProduction \n[million t-DM]",
  "Cropland \n[million ha]",
  "Livestock \nConsumption \n[million t-DM]",
  "Livestock \nProduction \n[million t-DM]",
  "Pasture \n[million ha]",
  "CH4 emission \n[Mt CH4/yr]",
  "N2O emission \n[kt N2O/yr]",
  "Forest \n[million ha]"
)
var_label_map_near <- stats::setNames(var_levels_near, var_levels_long)

mark_aim_period <- function(df) {
  df %>%
    mutate(
      aim_period = if_else(Y < 2015, "AIM-ALPHA backcast (<2015)", "AIM-ALPHA (2015 onward)"),
      aim_period = factor(aim_period, levels = names(aim_color))
    )
}

make_aim_segments <- function(df, value_col) {
  group_cols <- intersect(c("SCENARIO", "region", "VARIABLE", "MODEL"), names(df))
  df %>%
    arrange(across(all_of(group_cols)), Y) %>%
    group_by(across(all_of(group_cols))) %>%
    mutate(
      xend = lead(Y),
      yend = lead(.data[[value_col]])
    ) %>%
    ungroup() %>%
    filter(!is.na(xend), !is.na(yend))
}

recode_agmip_vars <- function(df) {
  df %>%
    mutate(VARIABLE = recode(VARIABLE, !!!var_map_agmip)) %>%
    filter(VARIABLE %in% var_levels_long) %>%
    mutate(VARIABLE = factor(VARIABLE, levels = var_levels_long))
}

ssp_agr_agmip <- recode_region(recode_agmip_vars(ssp_agr_agmip))
ssp_range_agmip <- recode_region(recode_agmip_vars(ssp_range_agmip))
aimalpha_agmip <- recode_region(recode_agmip_vars(aimalpha_agmip))
historical_agmip <- recode_region(recode_agmip_vars(historical_agmip))

region_r5 <- unique(aimalpha_agmip$region)
region_r5 <- region_r5[region_r5 != "World"]
region_r5 <- region_r5[!is.na(region_r5)]

plot_long_term <- function(r) {
  data_line_aim <- aimalpha_agmip %>%
    filter(region == r, MODEL == target_model) %>%
    mark_aim_period()
  data_line_others <- ssp_agr_agmip %>%
    filter(region == r, MODEL != target_model, MODEL %in% setdiff(names(model_color_cmp), target_model))
  data_range <- ssp_range_agmip %>% filter(region == r)
  data_hist <- historical_agmip %>% filter(region == r)

  if (nrow(data_line_aim) == 0 && nrow(data_line_others) == 0 && nrow(data_hist) == 0) {
    message("Skip SSP compare (long-term): no data for ", r)
    return(invisible(NULL))
  }

  model_breaks <- setdiff(names(model_color_cmp), target_model)
  g_plot <- ggplot() +
    geom_line(
      data = data_line_others,
      aes(x = Y, y = AGMIP_agg, color = MODEL, linewidth = MODEL),
      alpha = 0.4,
      show.legend = FALSE
    ) +
    geom_ribbon(
      data = data_range,
      aes(x = Y, ymin = IAM_min, ymax = IAM_max),
      fill = "#BBBBBB",
      alpha = 0.4,
      show.legend = FALSE
    ) +
    geom_point(
      data = data_line_others,
      aes(x = Y, y = AGMIP_agg, color = MODEL, shape = MODEL),
      size = 2.6,
      alpha = 0.4
    ) +
    scale_color_manual(values = model_color_cmp, breaks = model_breaks, name = "Models", guide = "none") +
    scale_shape_manual(
      values = iam_shape_cmp,
      breaks = model_breaks,
      name = "Models",
      drop = TRUE,
      guide = guide_legend(nrow = 2, byrow = TRUE, override.aes = list(color = "#333333", alpha = 0.8, size = 2.8))
    ) +
    ggnewscale::new_scale_color() +
    ggnewscale::new_scale("shape") +
    geom_line(
      data = data_hist,
      aes(x = Y, y = Historical, group = Model2),
      linewidth = 0.9,
      linetype = "dotdash",
      show.legend = FALSE
    ) +
    geom_ribbon(
      data = data_hist,
      aes(x = Y, ymin = Historical * 0.9, ymax = Historical * 1.1, fill = Model2),
      alpha = 0.5
    ) +
    geom_segment(
      data = make_aim_segments(data_line_aim, "AGMIP_agg"),
      aes(x = Y, xend = xend, y = AGMIP_agg, yend = yend, color = aim_period, linewidth = MODEL),
      show.legend = TRUE
    ) +
    geom_point(
      data = data_line_aim %>% filter(Y %% 5 == 0),
      aes(x = Y, y = AGMIP_agg, color = aim_period, shape = aim_period),
      size = 3.2,
      show.legend = TRUE
    ) +
    geom_hline(yintercept = 0, linetype = "dotted") +
    geom_vline(xintercept = 2015, linetype = "dotted") +
    scale_fill_manual(values = hist_fill, breaks = names(hist_fill), name = "Historical") +
    scale_color_manual(values = aim_color, breaks = names(aim_color), name = "AIM-ALPHA") +
    scale_shape_manual(values = aim_shape, breaks = names(aim_shape), name = "AIM-ALPHA") +
    scale_linewidth_manual(values = model_line_cmp, guide = "none") +
    facet_wrap(vars(VARIABLE), scales = "free_y") +
    scale_x_continuous(breaks = seq(2000, 2100, 20)) +
    scale_y_continuous(limits = c(0, NA)) +
    labs(x = "Year", y = "") +
    theme_1 +
    theme(axis.title.y = element_blank(), legend.box = "horizontal") +
    guides(
      color = guide_legend(
        order = 1,
        nrow = 1,
        byrow = TRUE,
        override.aes = list(linewidth = 1.2, alpha = 1)
      ),
      shape = guide_legend(order = 1, nrow = 1, byrow = TRUE),
      fill = guide_legend(order = 3, nrow = 1, byrow = TRUE, override.aes = list(alpha = 0.3, shape = NA, linetype = 0))
    )

  file_name <- paste0("SSP_compare_long_", gsub("[^A-Za-z0-9]+", "_", r), ".png")
  ggsave(file.path(long_dir, file_name), g_plot, width = 15, height = 10, create.dir = TRUE)
}

aimalpha_agmip_nt <- aimalpha_agmip %>%
  filter(Y <= 2030) %>%
  mutate(
    VARIABLE = recode(as.character(VARIABLE), !!!var_label_map_near),
    VARIABLE = factor(VARIABLE, levels = var_levels_near)
  )
ssp_agr_agmip_nt <- ssp_agr_agmip %>%
  filter(Y <= 2030) %>%
  mutate(
    VARIABLE = recode(as.character(VARIABLE), !!!var_label_map_near),
    VARIABLE = factor(VARIABLE, levels = var_levels_near)
  )
ssp_range_agmip_nt <- ssp_range_agmip %>%
  filter(Y <= 2030) %>%
  mutate(
    VARIABLE = recode(as.character(VARIABLE), !!!var_label_map_near),
    VARIABLE = factor(VARIABLE, levels = var_levels_near)
  )
historical_agmip_nt <- historical_agmip %>%
  filter(Y <= 2030) %>%
  mutate(
    VARIABLE = recode(as.character(VARIABLE), !!!var_label_map_near),
    VARIABLE = factor(VARIABLE, levels = var_levels_near)
  )

plot_near_world <- function() {
  model_breaks <- setdiff(names(model_color_cmp), target_model)
  data_line_aim <- aimalpha_agmip_nt %>%
    filter(region == "World", MODEL == target_model) %>%
    mark_aim_period()
  data_line_others <- ssp_agr_agmip_nt %>% filter(region == "World", MODEL != target_model, MODEL %in% model_breaks)
  data_range <- ssp_range_agmip_nt %>% filter(region == "World")
  data_hist <- historical_agmip_nt %>% filter(region == "World")

  if (nrow(data_line_aim) == 0 && nrow(data_line_others) == 0 && nrow(data_hist) == 0) {
    message("Skip SSP compare (near-term) World: no data")
    return(invisible(NULL))
  }

  g_plot <- ggplot() +
    geom_line(data = data_line_others, aes(x = Y, y = AGMIP_agg, color = MODEL, linewidth = MODEL), alpha = 0.4, show.legend = FALSE) +
    geom_ribbon(data = data_range, aes(x = Y, ymin = IAM_min, ymax = IAM_max), fill = "#BBBBBB", alpha = 0.4, show.legend = FALSE) +
    geom_point(data = data_line_others, aes(x = Y, y = AGMIP_agg, color = MODEL, shape = MODEL), size = 2.6, alpha = 0.4) +
    scale_color_manual(values = model_color_cmp, breaks = model_breaks, name = "Models", guide = "none") +
    scale_shape_manual(
      values = iam_shape_cmp,
      breaks = model_breaks,
      name = "Models",
      drop = TRUE,
      guide = guide_legend(nrow = 2, byrow = TRUE, override.aes = list(color = "#333333", alpha = 0.8, size = 2.8))
    ) +
    ggnewscale::new_scale_color() +
    ggnewscale::new_scale("shape") +
    geom_line(data = data_hist, aes(x = Y, y = Historical, group = Model2), linewidth = 0.6, linetype = "dotdash", show.legend = FALSE) +
    geom_ribbon(data = data_hist, aes(x = Y, ymin = Historical * 0.9, ymax = Historical * 1.1, fill = Model2), alpha = 0.4) +
    geom_segment(data = make_aim_segments(data_line_aim, "AGMIP_agg"), aes(x = Y, xend = xend, y = AGMIP_agg, yend = yend, color = aim_period, linewidth = MODEL), show.legend = TRUE) +
    geom_point(data = data_line_aim %>% filter(Y %% 5 == 0), aes(x = Y, y = AGMIP_agg, color = aim_period, shape = aim_period), size = 3.2, show.legend = TRUE) +
    geom_vline(xintercept = 2015, linetype = "dotted") +
    scale_fill_manual(values = hist_fill, breaks = names(hist_fill), name = "Historical") +
    scale_color_manual(values = aim_color, breaks = names(aim_color), name = "AIM-ALPHA") +
    scale_shape_manual(values = aim_shape, breaks = names(aim_shape), name = "AIM-ALPHA") +
    scale_linewidth_manual(values = model_line_cmp, guide = "none") +
    facet_wrap(vars(VARIABLE), scales = "free_y") +
    scale_x_continuous(breaks = seq(2000, 2030, 5), limits = c(2000, 2030)) +
    scale_y_continuous(limits = NULL, expand = expansion(mult = c(0.2, 0.2))) +
    labs(x = "Year", y = "") +
    theme_1 +
    theme(axis.title.y = element_blank(), legend.box = "horizontal") +
    guides(
      color = guide_legend(order = 1, nrow = 2, byrow = TRUE, override.aes = list(linewidth = 1.2, alpha = 1)),
      shape = guide_legend(order = 1, nrow = 2, byrow = TRUE),
      fill = guide_legend(order = 3, nrow = 1, byrow = TRUE, override.aes = list(alpha = 0.3, shape = NA, linetype = 0))
    )

  ggsave(file.path(near_dir, "SSP_compare_near_World.png"), g_plot, width = 15, height = 10, create.dir = TRUE)
}

plot_near_r5 <- function() {
  region_short <- c(
    "Asia" = "Asia",
    "Latin America and the Caribbean" = "Latin America",
    "Middle East and Africa" = "Middle East & Africa",
    "OECD+EU" = "OECD & EU",
    "Eastern Europe and the Former Soviet Union" = "Reforming Economies"
  )
  region_levels <- c("Asia", "Latin America", "Middle East & Africa", "OECD & EU", "Reforming Economies")
  region_labels <- c("Asia", "Latin America", "Middle East &\nAfrica", "OECD & EU", "Reforming\nEconomies")
  model_breaks <- setdiff(names(model_color_cmp), target_model)

  recode_plot_region <- function(df) {
    df %>%
      filter(region != "World") %>%
      mutate(
        region = recode(region, !!!region_short),
        region = factor(region, levels = region_levels, labels = region_labels)
      )
  }

  data_line_aim <- aimalpha_agmip_nt %>%
    filter(MODEL == target_model) %>%
    recode_plot_region() %>%
    mark_aim_period() %>%
    filter(!grepl("^CH4 emission|^N2O emission", as.character(VARIABLE)))
  data_line_others <- ssp_agr_agmip_nt %>%
    filter(MODEL != target_model, MODEL %in% model_breaks) %>%
    recode_plot_region() %>%
    filter(!grepl("^CH4 emission|^N2O emission", as.character(VARIABLE)))
  data_range <- recode_plot_region(ssp_range_agmip_nt) %>%
    filter(!grepl("^CH4 emission|^N2O emission", as.character(VARIABLE)))
  data_hist <- recode_plot_region(historical_agmip_nt) %>%
    filter(!grepl("^CH4 emission|^N2O emission", as.character(VARIABLE)))

  if (nrow(data_line_aim) == 0 && nrow(data_line_others) == 0 && nrow(data_hist) == 0) {
    message("Skip SSP compare (near-term) R5: no data")
    return(invisible(NULL))
  }

  g_plot <- ggplot() +
    geom_line(data = data_line_others, aes(x = Y, y = AGMIP_agg, color = MODEL, linewidth = MODEL), alpha = 0.4, show.legend = FALSE) +
    geom_ribbon(data = data_range, aes(x = Y, ymin = IAM_min, ymax = IAM_max), fill = "#BBBBBB", alpha = 0.4, show.legend = FALSE) +
    geom_point(data = data_line_others, aes(x = Y, y = AGMIP_agg, color = MODEL, shape = MODEL), size = 2.6, alpha = 0.4) +
    scale_color_manual(values = model_color_cmp, breaks = model_breaks, name = "Models", guide = "none") +
    scale_shape_manual(
      values = iam_shape_cmp,
      breaks = model_breaks,
      name = "Models",
      drop = TRUE,
      guide = guide_legend(nrow = 2, byrow = TRUE, override.aes = list(color = "#333333", alpha = 0.8, size = 2.8))
    ) +
    ggnewscale::new_scale_color() +
    ggnewscale::new_scale("shape") +
    geom_line(data = data_hist, aes(x = Y, y = Historical, group = Model2), linewidth = 0.6, linetype = "dotdash", show.legend = FALSE) +
    geom_ribbon(data = data_hist, aes(x = Y, ymin = Historical * 0.9, ymax = Historical * 1.1, fill = Model2), alpha = 0.4) +
    geom_segment(data = make_aim_segments(data_line_aim, "AGMIP_agg"), aes(x = Y, xend = xend, y = AGMIP_agg, yend = yend, color = aim_period, linewidth = MODEL), show.legend = TRUE) +
    geom_point(data = data_line_aim %>% filter(Y %% 5 == 0), aes(x = Y, y = AGMIP_agg, color = aim_period, shape = aim_period), size = 2.8, show.legend = TRUE) +
    geom_vline(xintercept = 2015, linetype = "dotted") +
    scale_fill_manual(values = hist_fill, breaks = names(hist_fill), name = "Historical") +
    scale_color_manual(values = aim_color, breaks = names(aim_color), name = "AIM-ALPHA") +
    scale_shape_manual(values = aim_shape, breaks = names(aim_shape), name = "AIM-ALPHA") +
    scale_linewidth_manual(values = model_line_cmp, guide = "none") +
    ggh4x::facet_grid2(rows = vars(VARIABLE), cols = vars(region), scales = "free", independent = "y") +
    scale_x_continuous(breaks = seq(2000, 2030, 5), limits = c(2000, 2030)) +
    scale_y_continuous(limits = NULL, expand = expansion(mult = c(0.2, 0.2))) +
    labs(x = "Year", y = "") +
    theme_1 +
    theme(
      axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
      axis.text.y = element_text(size = 12),
      strip.text.y = element_text(size = 14, lineheight = 0.95),
      legend.box = "horizontal"
    ) +
    guides(
      color = guide_legend(order = 1, nrow = 2, byrow = TRUE, override.aes = list(linewidth = 1.2, alpha = 1)),
      shape = guide_legend(order = 1, nrow = 2, byrow = TRUE),
      fill = guide_legend(order = 3, nrow = 1, byrow = TRUE, override.aes = list(alpha = 0.3, shape = NA, linetype = 0))
    )

  ggsave(file.path(near_dir, "SSP_compare_near_R5.png"), g_plot, width = 16, height = 11, create.dir = TRUE)
}

plot_extra_indicators <- function() {
  extra_levels <- c(
    "Crop yield\n[fm t/ha]",
    "Crop producer price\n[USD/t]",
    "Livestock producer price\n[USD/t]",
    "Net crop trade\n[million t/yr]",
    "Net livestock trade\n[million t/yr]"
  )
  extra_levels_r5 <- c(
    "Crop yield\n[fm t/ha]",
    "Crop price\n[USD/t]",
    "Livestock price\n[USD/t]",
    "Crop trade\n[million t/yr]",
    "Livestock trade\n[million t/yr]"
  )
  extra_label_r5 <- stats::setNames(extra_levels_r5, extra_levels)
  extra_var_map <- c(
    "Yie_Cro_Cer" = "Crop yield\n[fm t/ha]",
    "Prc_Agr_Foo_Pro_Cro_Ind" = "Crop producer price\n[USD/t]",
    "Prc_Agr_Foo_Pro_Liv_Ind" = "Livestock producer price\n[USD/t]",
    "Trd_Agr_Cro" = "Net crop trade\n[million t/yr]",
    "Trd_Agr_Liv" = "Net livestock trade\n[million t/yr]"
  )
  iamc_extra_vars <- character(0)
  missing_extra_vars <- setdiff(iamc_extra_vars, unique(as.character(filter_by_scenario(iamc_all_raw, scenario)$IAMC_VAR)))
  if (length(missing_extra_vars) > 0) {
    message("Missing extra IAMC variables in IAMC_all_scenario.gdx for ", scenario, ": ", paste(missing_extra_vars, collapse = ", "))
  }

  aimalpha_extra_iamc <- filter_by_scenario(iamc_all_raw, scenario) %>%
    mutate(
      VARIABLE = as.character(IAMC_VAR),
      region = as.character(IAMC_REGION),
      Y = as.numeric(as.character(Y)),
      MODEL = target_model,
      Value = IAMC_Template
    ) %>%
    filter(VARIABLE %in% iamc_extra_vars, Y <= 2030, region %in% r5_regions) %>%
    mutate(VARIABLE = recode(VARIABLE, !!!extra_var_map)) %>%
    select(SCENARIO, region, Y, VARIABLE, MODEL, Value)

  aimalpha_extra_yield <- filter_by_scenario(aimalpha_agmip_raw, scenario) %>%
    filter(AgMIP_VAR == "YILD", ITEM == "CRP", AgMIP_UNIT == "fm t/ha") %>%
    mutate(
      VARIABLE = "Crop yield\n[fm t/ha]",
      Y = as.numeric(as.character(Y)),
      MODEL = target_model,
      Value = AGMIP_agg
    ) %>%
    filter(Y <= 2030, region %in% r5_regions) %>%
    select(SCENARIO, region, Y, VARIABLE, MODEL, Value)

  aimalpha_extra_price <- filter_by_scenario(aimalpha_agmip_raw, scenario) %>%
    filter(AgMIP_VAR == "XPRP", ITEM %in% c("CRP", "LSP"), AgMIP_UNIT == "USD/t") %>%
    mutate(
      VARIABLE = case_when(
        ITEM == "CRP" ~ "Crop producer price\n[USD/t]",
        ITEM == "LSP" ~ "Livestock producer price\n[USD/t]",
        TRUE ~ NA_character_
      ),
      Y = as.numeric(as.character(Y)),
      MODEL = target_model,
      Value = AGMIP_agg
    ) %>%
    filter(!is.na(VARIABLE), Y <= 2030, region %in% r5_regions) %>%
    select(SCENARIO, region, Y, VARIABLE, MODEL, Value)

  aimalpha_extra_trade <- filter_by_scenario(aimalpha_agmip_raw, scenario) %>%
    filter(AgMIP_VAR == "NETT", ITEM %in% c("CRP", "LSP"), AgMIP_UNIT == "1000 t") %>%
    mutate(
      VARIABLE = case_when(
        ITEM == "CRP" ~ "Net crop trade\n[million t/yr]",
        ITEM == "LSP" ~ "Net livestock trade\n[million t/yr]",
        TRUE ~ NA_character_
      ),
      Y = as.numeric(as.character(Y)),
      MODEL = target_model,
      Value = AGMIP_agg / 1000
    ) %>%
    filter(!is.na(VARIABLE), Y <= 2030, region %in% r5_regions) %>%
    select(SCENARIO, region, Y, VARIABLE, MODEL, Value)

  aimalpha_extra <- bind_rows(aimalpha_extra_iamc, aimalpha_extra_yield, aimalpha_extra_price, aimalpha_extra_trade)

  if (nrow(aimalpha_extra) == 0) {
    message("Skip extra indicator plots: no extra indicator rows for ", scenario)
    return(invisible(NULL))
  }

  historical_extra_agmip <- agmip_hist_raw %>%
    mutate(
      region = as.character(region),
      Y = as.numeric(as.character(yr))
    ) %>%
    filter(
      Y >= 2000,
      Y <= 2025,
      region %in% r5_regions,
      (AgMIP_VAR == "YILD" & ITEM == "CRP" & AgMIP_UNIT == "fm t/ha") |
        (AgMIP_VAR == "XPRP" & ITEM %in% c("CRP", "LSP") & AgMIP_UNIT == "USD/t") |
        (AgMIP_VAR == "NETT" & ITEM %in% c("CRP", "LSP") & AgMIP_UNIT == "1000 t")
    ) %>%
    mutate(
      VARIABLE = case_when(
        AgMIP_VAR == "YILD" & ITEM == "CRP" ~ "Crop yield\n[fm t/ha]",
        AgMIP_VAR == "XPRP" & ITEM == "CRP" ~ "Crop producer price\n[USD/t]",
        AgMIP_VAR == "XPRP" & ITEM == "LSP" ~ "Livestock producer price\n[USD/t]",
        AgMIP_VAR == "NETT" & ITEM == "CRP" ~ "Net crop trade\n[million t/yr]",
        AgMIP_VAR == "NETT" & ITEM == "LSP" ~ "Net livestock trade\n[million t/yr]",
        TRUE ~ NA_character_
      ),
      Value = if_else(AgMIP_VAR == "NETT", AGMIP_agg_hist / 1000, AGMIP_agg_hist),
      Model2 = "FAOSTAT"
    ) %>%
    filter(!is.na(VARIABLE)) %>%
    select(region, Y, VARIABLE, Model2, Value)
  historical_extra <- historical_extra_agmip

  aimalpha_extra <- recode_region(aimalpha_extra) %>%
    mutate(
      VARIABLE = factor(VARIABLE, levels = extra_levels),
      aim_period = if_else(Y < 2015, "AIM-ALPHA backcast (<2015)", "AIM-ALPHA (2015 onward)"),
      aim_period = factor(aim_period, levels = names(aim_color))
    )
  historical_extra <- recode_region(historical_extra) %>%
    mutate(VARIABLE = factor(VARIABLE, levels = extra_levels))

  plot_extra_world <- function() {
    world_extra_levels <- extra_levels[!grepl("^Net .* trade", extra_levels)]
    data_aim <- aimalpha_extra %>%
      filter(region == "World", !grepl("^Net .* trade", as.character(VARIABLE))) %>%
      mutate(VARIABLE = factor(as.character(VARIABLE), levels = world_extra_levels))
    data_hist <- historical_extra %>%
      filter(region == "World", !grepl("^Net .* trade", as.character(VARIABLE))) %>%
      mutate(VARIABLE = factor(as.character(VARIABLE), levels = world_extra_levels))

    if (nrow(data_aim) == 0 && nrow(data_hist) == 0) {
      message("Skip extra indicator plot World: no data")
      return(invisible(NULL))
    }

    g_plot <- ggplot() +
      geom_line(
        data = data_hist,
        aes(x = Y, y = Value, group = Model2),
        linewidth = 0.6,
        linetype = "dotdash",
        show.legend = FALSE
      ) +
      geom_ribbon(
        data = data_hist,
        aes(x = Y, ymin = Value * 0.9, ymax = Value * 1.1, fill = Model2),
        alpha = 0.4
      ) +
      geom_segment(
        data = make_aim_segments(data_aim, "Value"),
        aes(x = Y, xend = xend, y = Value, yend = yend, color = aim_period, linewidth = MODEL),
        show.legend = TRUE
      ) +
      geom_point(
        data = data_aim %>% filter(Y %% 5 == 0),
        aes(x = Y, y = Value, color = aim_period, shape = aim_period),
        size = 3.2,
        show.legend = TRUE
      ) +
      geom_vline(xintercept = 2015, linetype = "dotted") +
      scale_fill_manual(values = c("FAOSTAT" = unname(hist_fill["FAOSTAT"])), breaks = "FAOSTAT", name = "Historical") +
      scale_color_manual(values = aim_color, breaks = names(aim_color), name = "AIM-ALPHA") +
      scale_shape_manual(values = aim_shape, breaks = names(aim_shape), name = "AIM-ALPHA") +
      scale_linewidth_manual(values = model_line_cmp[target_model], guide = "none") +
      facet_wrap(vars(VARIABLE), scales = "free_y", nrow = 1) +
      scale_x_continuous(breaks = seq(2000, 2030, 5), limits = c(2000, 2030)) +
      scale_y_continuous(limits = NULL, expand = expansion(mult = c(0.2, 0.2))) +
      labs(x = "Year", y = "") +
      theme_1 +
      theme(
        axis.title.y = element_blank(),
        strip.text = element_text(size = 16, lineheight = 0.95),
        legend.box = "horizontal"
      ) +
      guides(
        fill = guide_legend(order = 2, nrow = 1, byrow = TRUE, override.aes = list(alpha = 0.3, shape = NA, linetype = 0)),
        color = guide_legend(order = 3, nrow = 1, byrow = TRUE, override.aes = list(linewidth = 1.2, alpha = 1)),
        shape = guide_legend(order = 3, nrow = 1, byrow = TRUE)
      )

    ggsave(file.path(near_dir, "SSP_compare_extra_World.png"), g_plot, width = 15, height = 4.8, create.dir = TRUE)
  }

  plot_extra_r5 <- function() {
    region_short <- c(
      "Asia" = "Asia",
      "Latin America and the Caribbean" = "Latin America",
      "Middle East and Africa" = "Middle East & Africa",
      "OECD+EU" = "OECD & EU",
      "Eastern Europe and the Former Soviet Union" = "Reforming Economies"
    )
    region_levels <- c("Asia", "Latin America", "Middle East & Africa", "OECD & EU", "Reforming Economies")
    region_labels <- c("Asia", "Latin America", "Middle East &\nAfrica", "OECD & EU", "Reforming\nEconomies")

    recode_plot_region <- function(df) {
      df %>%
        filter(region != "World") %>%
        mutate(
          region = recode(region, !!!region_short),
          region = factor(region, levels = region_levels, labels = region_labels),
          VARIABLE = recode(as.character(VARIABLE), !!!extra_label_r5),
          VARIABLE = factor(VARIABLE, levels = extra_levels_r5)
        )
    }

    data_aim <- recode_plot_region(aimalpha_extra)
    data_hist <- recode_plot_region(historical_extra)

    if (nrow(data_aim) == 0 && nrow(data_hist) == 0) {
      message("Skip extra indicator plot R5: no data")
      return(invisible(NULL))
    }

    g_plot <- ggplot() +
      geom_line(
        data = data_hist,
        aes(x = Y, y = Value, group = Model2),
        linewidth = 0.6,
        linetype = "dotdash",
        show.legend = FALSE
      ) +
      geom_ribbon(
        data = data_hist,
        aes(x = Y, ymin = Value * 0.9, ymax = Value * 1.1, fill = Model2),
        alpha = 0.4
      ) +
      geom_segment(
        data = make_aim_segments(data_aim, "Value"),
        aes(x = Y, xend = xend, y = Value, yend = yend, color = aim_period, linewidth = MODEL),
        show.legend = TRUE
      ) +
      geom_point(
        data = data_aim %>% filter(Y %% 5 == 0),
        aes(x = Y, y = Value, color = aim_period, shape = aim_period),
        size = 2.8,
        show.legend = TRUE
      ) +
      geom_vline(xintercept = 2015, linetype = "dotted") +
      scale_fill_manual(values = c("FAOSTAT" = unname(hist_fill["FAOSTAT"])), breaks = "FAOSTAT", name = "Historical") +
      scale_color_manual(values = aim_color, breaks = names(aim_color), name = "AIM-ALPHA") +
      scale_shape_manual(values = aim_shape, breaks = names(aim_shape), name = "AIM-ALPHA") +
      scale_linewidth_manual(values = model_line_cmp[target_model], guide = "none") +
      ggh4x::facet_grid2(rows = vars(VARIABLE), cols = vars(region), scales = "free", independent = "y") +
      scale_x_continuous(breaks = seq(2000, 2030, 5), limits = c(2000, 2030)) +
      scale_y_continuous(limits = NULL, expand = expansion(mult = c(0.2, 0.2))) +
      labs(x = "Year", y = "") +
      theme_1 +
      theme(
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 12),
        strip.text.y = element_text(size = 11, lineheight = 0.9),
        legend.box = "horizontal"
      ) +
      guides(
        fill = guide_legend(order = 2, nrow = 1, byrow = TRUE, override.aes = list(alpha = 0.3, shape = NA, linetype = 0)),
        color = guide_legend(order = 3, nrow = 1, byrow = TRUE, override.aes = list(linewidth = 1.2, alpha = 1)),
        shape = guide_legend(order = 3, nrow = 1, byrow = TRUE)
      )

    ggsave(file.path(near_dir, "SSP_compare_extra_R5.png"), g_plot, width = 16, height = 8.5, create.dir = TRUE)
  }

  plot_extra_world()
  plot_extra_r5()
}

plot_long_term("World")
purrr::walk(region_r5, plot_long_term)
plot_near_world()
plot_near_r5()
plot_extra_indicators()

message("SSP_compare plots saved to: ", compare_dir)
}
