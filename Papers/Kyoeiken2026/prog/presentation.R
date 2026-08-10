library(gdxrrw)
library(ggplot2)
library(patchwork)
library(scales)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(tibble)
library(readr)

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
    file.path(getwd(), "Papers", "Kyoeiken2026", "prog")
  )
  candidate_dirs <- unique(candidate_dirs[file.exists(candidate_dirs)])
  candidate_dirs <- candidate_dirs[file.info(candidate_dirs)$isdir %in% TRUE]
  candidate_dirs <- candidate_dirs[file.exists(file.path(candidate_dirs, "presentation.R"))]
  if (length(candidate_dirs) == 0) {
    stop(
      "Could not determine script_dir. Open presentation.R from Papers/Kyoeiken2026/prog/ ",
      "or set the working directory to Papers/Kyoeiken2026 before sourcing."
    )
  }
  normalizePath(candidate_dirs[[1]], winslash = "/", mustWork = TRUE)
}

base_dir <- normalizePath(file.path(script_dir, ".."), winslash = "/", mustWork = TRUE)
data_dir <- file.path(base_dir, "data", "0329")
define_dir <- file.path(base_dir, "define")
output_dir <- file.path(base_dir, "output", "presentation_jp")
country_set_file <- file.path(define_dir, "country.set")
region_map_file <- file.path(define_dir, "region.map")

scenario_baseline <- "SSP2_BaU_NoCC_No"
scenario_landint <- "SSP2_1100C_CACN_DAC_NoCC_LandInt_global1"
scenario_no <- "SSP2_1100C_CACN_DAC_NoCC_No"
plot_years <- c(2050L, 2100L)
decadal_years <- seq(2020, 2100, 10)
analysis_years <- sort(unique(c(2015L, decadal_years)))
focus_year <- 2100L
top_n <- 20L
kcal_million_to_pj <- 4.184e-6

# common palettes and plotting helpers ------------------------------------
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
  out_path <- file.path(output_dir, filename)
  ggsave(
    filename = filename,
    path = output_dir,
    plot = plot_obj,
    width = width,
    height = height,
    dpi = dpi,
    bg = "white",
    limitsize = FALSE,
    create.dir = TRUE
  )
  message("Saved: ", out_path)
}

logmean <- function(x, y) {
  ifelse(abs(x - y) < 1e-12, x, (x - y) / (log(x) - log(y)))
}

safe_positive <- function(x, eps = 1e-12) {
  ifelse(is.na(x) | x <= 0, eps, x)
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

jp_component_labels <- c(
  "Enteric fermentation" = "家畜消化管内発酵",
  "Manure management" = "家畜排せつ物管理",
  "Rice cultivation" = "稲作",
  "Managed soils" = "農用地土壌",
  "Land Conversion" = "土地転換",
  "Forest fire" = "森林火災",
  "Prevented land conversion" = "土地転換の抑制",
  "Afforestation" = "新規植林・再植林",
  "Forest protection" = "森林保護",
  "Forest management" = "森林管理",
  "Enhanced natural regeneration" = "自然再生の促進",
  "Agroforestry" = "アグロフォレストリー",
  "Biochar" = "バイオ炭",
  "Soil carbon sequestration" = "土壌炭素隔離",
  "Cropland" = "農地",
  "Managed and primary forest" = "管理林・原生林",
  "Grassland" = "草地",
  "Other natural land" = "その他自然地",
  "Non arable land" = "非耕作地",
  "Energy Crop" = "エネルギー作物",
  "Crops" = "作物",
  "Livestock" = "畜産物",
  "Agriculture" = "農業",
  "LULUCF" = "LULUCF",
  "Wheat" = "小麦",
  "Rice" = "米",
  "Coarse grains" = "粗粒穀物",
  "Oilseeds" = "油糧種子",
  "Sugar crops" = "砂糖作物",
  "Vegetables & fruits" = "野菜・果実",
  "Other crops" = "その他作物",
  "Ruminant" = "反芻家畜",
  "Non-ruminant" = "非反芻家畜",
  "Dairy" = "乳製品"
)

jp_decomp_labels <- c(
  "Demand side" = "需要側要因",
  "Structural" = "構造要因",
  "Technical" = "技術要因"
)

jp_region_labels <- c(
  "OECD+EU" = "OECD+EU",
  "Asia" = "アジア",
  "Africa/Middle East" = "アフリカ・中東",
  "Latin America" = "ラテンアメリカ",
  "Former Soviet Union" = "旧ソ連圏",
  "World" = "世界"
)

jp_size_labels <- c(
  "<= 20 MtCO2eq/yr" = "20 MtCO2eq/年以下",
  "20-100 MtCO2eq/yr" = "20-100 MtCO2eq/年",
  "100-250 MtCO2eq/yr" = "100-250 MtCO2eq/年",
  ">= 250 MtCO2eq/yr" = "250 MtCO2eq/年以上"
)

jp_labels <- function(label_map) {
  function(x) dplyr::recode(x, !!!label_map, .default = x)
}

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

make_world_ts_panel <- function(df, palette, limits, y_label = "MtCO2eq/年", x_label = "年") {
  ggplot(df, aes(x = factor(year), y = value, fill = component)) +
    geom_col(width = 0.72) +
    scale_fill_manual(values = palette, limits = limits, labels = jp_labels(jp_component_labels), drop = FALSE) +
    labs(x = x_label, y = y_label) +
    theme_main +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

make_world_ts_panel_with_observed <- function(df, palette, limits, y_label = "MtCO2eq/年", x_label = "年") {
  x_axis <- df %>%
    distinct(x_pos, x_label) %>%
    arrange(x_pos)

  ggplot(df, aes(x = x_pos, y = value, fill = component)) +
    geom_col(width = 0.72) +
    scale_x_continuous(
      breaks = x_axis$x_pos,
      labels = x_axis$x_label,
      expand = expansion(mult = c(0.03, 0.03))
    ) +
    scale_fill_manual(values = palette, limits = limits, labels = jp_labels(jp_component_labels), drop = FALSE) +
    labs(x = x_label, y = y_label) +
    theme_main +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.ticks.x = element_blank()
    )
}

make_r5_panel <- function(df, palette, limits, y_label = "MtCO2eq/年", x_expand_add = NULL) {
  p <- ggplot(df, aes(x = region_name, y = value, fill = component)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
    geom_col(width = 0.72) +
    scale_fill_manual(values = palette, limits = limits, labels = jp_labels(jp_component_labels), drop = FALSE) +
    labs(x = NULL, y = y_label) +
    theme_main +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  if (!is.null(x_expand_add)) {
    p <- p + scale_x_discrete(labels = jp_labels(jp_region_labels), expand = expansion(add = x_expand_add))
  } else {
    p <- p + scale_x_discrete(labels = jp_labels(jp_region_labels))
  }

  p
}

make_top_country_share_panel <- function(df, full_total_sum, palette, limits, x_label = "MtCO2eq/年") {
  totals <- df %>%
    group_by(country) %>%
    summarise(total_value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(total_value)) %>%
    mutate(
      share = if (is.finite(full_total_sum) && full_total_sum != 0) 100 * total_value / full_total_sum else NA_real_,
      cumulative_share = cumsum(share)
    )

  country_levels <- totals$country
  max_total <- max(totals$total_value, na.rm = TRUE)
  share_scale <- if (is.finite(max_total) && max_total > 0) max_total / 100 else 1

  bar_df <- df %>% mutate(country = factor(country, levels = country_levels))
  line_df <- totals %>%
    mutate(
      country = factor(country, levels = country_levels),
      cumulative_scaled = cumulative_share * share_scale
    )

  ggplot() +
    geom_col(data = bar_df, aes(x = country, y = value, fill = component), width = 0.82) +
    geom_line(data = line_df, aes(x = country, y = cumulative_scaled, group = 1, color = "Cumulative share"), linewidth = 0.8) +
    geom_point(data = line_df, aes(x = country, y = cumulative_scaled, color = "Cumulative share"), size = 1.4) +
    scale_fill_manual(values = palette, limits = limits, labels = jp_labels(jp_component_labels), drop = FALSE) +
    scale_color_manual(values = c("Cumulative share" = "#C62828"), labels = c("Cumulative share" = "累積シェア")) +
    scale_y_continuous(
      name = x_label,
      sec.axis = sec_axis(~ . / share_scale, name = "累積シェア (%)")
    ) +
    labs(x = NULL) +
    theme_main +
    theme(axis.text.x = element_text(angle = 60, hjust = 1, vjust = 1))
}

make_horizontal_stack_with_point <- function(stack_df, point_df, x_label) {
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
    scale_fill_manual(values = crp_lsp_palette, labels = jp_labels(jp_component_labels), guide = guide_legend(ncol = 1)) +
    labs(x = x_label, y = NULL) +
    theme_main
}

make_bubble_x_df <- function(metric_col, geer_agmip, focus_year, scenario_baseline, scenario_landint, country_codes) {
  switch(
    metric_col,
    land_tot = geer_agmip %>%
      filter(year == focus_year, region %in% country_codes, scenario == scenario_baseline, variable == "LAND", item == "TOT", unit == "1000 ha") %>%
      transmute(region, x = value),
    gdppc = geer_agmip %>%
      filter(
        year == focus_year,
        region %in% country_codes,
        scenario == scenario_baseline,
        (variable == "GDPT" & item == "TOT" & unit == "bn USD 2017 PPP") |
          (variable == "POPT" & item == "TOT" & unit == "million")
      ) %>%
      select(region, variable, value) %>%
      pivot_wider(names_from = variable, values_from = value) %>%
      transmute(region, x = if_else(POPT > 0, GDPT / POPT, NA_real_)),
    self_suff_bau = geer_agmip %>%
      filter(year == focus_year, region %in% country_codes, scenario == scenario_baseline, variable %in% c("PROD", "CONS"), item == "AGR", unit == "million kcal") %>%
      select(region, variable, value) %>%
      pivot_wider(names_from = variable, values_from = value) %>%
      transmute(region, x = if_else(CONS > 0, 100 * PROD / CONS, NA_real_)),
    cali_agr = geer_agmip %>%
      filter(year == focus_year, region %in% country_codes, scenario %in% c(scenario_baseline, scenario_landint), variable == "CALI", item == "AGR", unit == "kcal/cap/d") %>%
      select(region, scenario, value) %>%
      pivot_wider(names_from = scenario, values_from = value) %>%
      transmute(region, x = .data[[scenario_landint]] - .data[[scenario_baseline]])
  )
}

make_generic_bubble_plot <- function(metric_col, x_label, y_df, y_col, y_label, geer_agmip, focus_year, scenario_baseline, scenario_landint, country_codes, region_map, boundary) {
  x_df <- make_bubble_x_df(metric_col, geer_agmip, focus_year, scenario_baseline, scenario_landint, country_codes)

  bubble_df <- y_df %>%
    filter(region %in% country_codes) %>%
    left_join(x_df, by = "region") %>%
    left_join(region_map, by = "region") %>%
    left_join(st_drop_geometry(boundary), by = c("region" = "cty")) %>%
    mutate(country = coalesce(country_name, region), region5 = factor(region5, levels = names(region_palette))) %>%
    fixed_afolu_bubble_size() %>%
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
    scale_fill_manual(values = region_palette, labels = jp_labels(jp_region_labels)) +
    scale_color_manual(values = region_border_palette, labels = jp_labels(jp_region_labels), guide = guide_legend(ncol = 3, byrow = TRUE)) +
    scale_size_manual(values = c(3, 4.8, 6.4, 8), labels = jp_labels(jp_size_labels), drop = FALSE) +
    scale_alpha_manual(values = setNames(c(0.58, 0.66, 0.74, 0.82), size_levels), guide = "none", drop = FALSE) +
    labs(x = x_label, y = y_label) +
    guides(
      fill = guide_legend(order = 1, ncol = 3, byrow = TRUE, override.aes = list(size = 4, alpha = 1)),
      color = "none",
      size = guide_legend(order = 2, ncol = 2, byrow = TRUE)
    ) +
    theme_main +
    theme(legend.position = "bottom", legend.box = "vertical")
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

group_map <- readLines(file.path(define_dir, "group.map"), warn = FALSE) %>%
  stringr::str_trim() %>%
  .[. != "" & !startsWith(., "*")] %>%
  stringr::str_split("\\s+") %>%
  purrr::map_dfr(function(x) {
    tibble(region = x[[1]], development_group = x[[length(x)]])
  }) %>%
  filter(region %in% country_codes) %>%
  distinct(region, development_group)

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
  variables = c("CALI", "XPRC", "PROD", "CONS", "EXPO", "LAND", "POPT", "GDPT", "EMIS", "RedCos"),
  items = c("TOT", "AGR", "CRP", "LSP", "FOR", "GRS", "ECP", "ONV", "NLD", "tot", "ent", "mmg", "rcc", "sfr", "mas", "mgr", "crs", "ors", "bsv", "bcr", "WHT", "RIC", "CGR", "OSD", "SGC", "VFN", "OCR", "RUM", "NRM", "DRY"),
  units = c("kcal/cap/d", "USD/t", "1000 ha", "million", "million kcal", "1000 t", "bn USD 2017 PPP", "MtCO2e", "USD"),
  regions = regions_for_read,
  years = analysis_years,
  scenarios = c(scenario_baseline, scenario_landint)
)

GEER_CTAX <- read_agmip(
  variables = c("CTAX"),
  items = c("TOT"),
  units = c("USD/tCO2e"),
  regions = c("USA"),
  years = analysis_years,
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
      "Emi_CO2_Lan_Use_Flo_Pos_Emi_Lan_Use_Cha",
      "Emi_CO2_AFO_Lan_For_Bur",
      "Emi_CO2_Lan_Use_Flo_Pos_Emi_Fir",
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
  summarise(land_actual_change = sum(value, na.rm = TRUE), .groups = "drop")

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
  summarise(land_actual_change = sum(value, na.rm = TRUE), .groups = "drop")

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

fig1_observed_source_map <- c(
  "Emi_equ_CH4_AFO_Agr_Liv_Ent_Fer" = "Enteric fermentation",
  "Emi_equ_CH4_AFO_Agr_Liv_Man_Man" = "Manure management",
  "Emi_equ_N2O_AFO_Agr_Liv_Man_Man" = "Manure management",
  "Emi_equ_CH4_AFO_Agr_Ric" = "Rice cultivation",
  "Emi_equ_N2O_AFO_Agr_Man_Soi" = "Managed soils",
  "Emi_CO2_Lan_Use_Flo_Pos_Emi_Lan_Use_Cha" = "Land Conversion",
  "Emi_CO2_Lan_Use_Flo_Pos_Emi_Fir" = "Forest fire"
)

fig1_component_levels <- names(fig1_component_palette)

fig1_observed_world_2020 <- {
  req <- list(
    name = "IAMC_templatePlus",
    form = "full",
    uels = list(c("EDGAR", "FAO-FAOTier1"), "World", names(fig1_observed_source_map), "2020")
  )

  rgdx(file.path(data_dir, "IAMC_templatePlus.gdx"), req, squeeze = FALSE)$val %>%
    as.data.frame.table(responseName = "value", stringsAsFactors = FALSE) %>%
    setNames(c("source", "region", "variable", "year", "value")) %>%
    mutate(
      year = as.integer(as.character(year)),
      value = as.numeric(value),
      component = recode(variable, !!!fig1_observed_source_map),
      source = recode(source, "FAO-FAOTier1" = "FAO", .default = source),
      x_label = paste(source, year)
    ) %>%
    group_by(source, x_label, year, component) %>%
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    mutate(component = factor(component, levels = fig1_component_levels))
}

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

fig1_world_ts_with_observed <- bind_rows(
  fig1_observed_world_2020,
  fig1_baseline_world_ts %>%
    filter(year == 2020L) %>%
    mutate(source = "This study", x_label = "This study 2020"),
  fig1_baseline_world_ts %>%
    filter(year %in% c(2030L, 2050L, 2070L, 2100L)) %>%
    mutate(source = "This study", x_label = as.character(year))
) %>%
  mutate(
    x_pos = case_match(
      x_label,
      "EDGAR 2020" ~ 1,
      "FAO 2020" ~ 2,
      "This study 2020" ~ 3,
      "2030" ~ 4.12,
      "2050" ~ 5.12,
      "2070" ~ 6.12,
      "2100" ~ 7.12
    ),
    component = factor(component, levels = fig1_component_levels)
  )

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

fig2_component_levels <- names(fig2_component_palette)

fig2_ag_world_ts <- agmip_emis_actual_reduction %>%
  filter(region %in% country_codes, item %in% names(ag_source_map)) %>%
  mutate(component = recode(item, !!!ag_source_map)) %>%
  group_by(year, component) %>%
  summarise(value = sum(actual_reduction, na.rm = TRUE), .groups = "drop")

fig2_ag_r5_2100 <- agmip_emis_actual_reduction %>%
  filter(region %in% country_codes, year == focus_year, item %in% names(ag_source_map)) %>%
  left_join(region_map %>% select(region, region5), by = "region") %>%
  filter(!is.na(region5)) %>%
  mutate(region_name = region5, component = recode(item, !!!ag_source_map)) %>%
  group_by(region_name, component) %>%
  summarise(value = sum(actual_reduction, na.rm = TRUE), .groups = "drop")

fig2_ag_country_2100 <- agmip_emis_actual_reduction %>%
  filter(region %in% country_codes, year == focus_year, item %in% names(ag_source_map)) %>%
  mutate(component = recode(item, !!!ag_source_map)) %>%
  group_by(region, component) %>%
  summarise(value = sum(actual_reduction, na.rm = TRUE), .groups = "drop")

fig2_land_change_decadal <- GEER_AFOLUB %>%
  filter(variable == "Emi_CO2_Lan_Use_Flo_Pos_Emi_Lan_Use_Cha") %>%
  group_by(region, year, scenario) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = scenario, values_from = value, values_fill = 0) %>%
  transmute(region, year, component = "Prevented land conversion", value = .data[[scenario_no]] - .data[[scenario_landint]])

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
) %>%
  group_by(year, component) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop")

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
  left_join(group_map, by = "region") %>%
  mutate(country = coalesce(country_name, region), component = factor(component, levels = fig2_component_levels))

afolu_top20_2100 <- fig2_actual_country_2100 %>%
  group_by(region, country) %>%
  summarise(total_value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total_value)) %>%
  slice_head(n = top_n)

afolu_top20_order <- afolu_top20_2100$country

afolu_reduction_2100 <- fig2_actual_country_2100 %>%
  group_by(region, country, development_group) %>%
  summarise(afolu_reduction = sum(value, na.rm = TRUE), .groups = "drop")

afolu_rate_country_fig6 <- afolu_reduction_2100 %>%
  left_join(
    fig1_baseline_country_2100 %>%
      group_by(region, country) %>%
      summarise(baseline_emissions = sum(value, na.rm = TRUE), .groups = "drop"),
    by = c("region", "country")
  ) %>%
  mutate(reduction_rate = if_else(is.finite(baseline_emissions) & baseline_emissions != 0, 100 * afolu_reduction / baseline_emissions, NA_real_))

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
  mutate(change = (.data[[scenario_landint]] - .data[[scenario_baseline]]) * kcal_million_to_pj) %>%
  left_join(st_drop_geometry(Boundary), by = c("region" = "cty")) %>%
  mutate(country = factor(coalesce(country_name, region), levels = rev(afolu_top20_order)))

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

fig3b_cali_summary_2100 <- GEER_AGMIP %>%
  filter(
    region %in% afolu_top20_2100$region,
    year %in% c(2015L, focus_year),
    scenario %in% c(scenario_baseline, scenario_landint),
    variable == "CALI",
    item %in% c("AGR", "CRP", "LSP"),
    unit == "kcal/cap/d"
  ) %>%
  transmute(region, year, scenario, item, value) %>%
  pivot_wider(names_from = c(scenario, year), values_from = value) %>%
  left_join(st_drop_geometry(Boundary), by = c("region" = "cty")) %>%
  mutate(
    country = coalesce(country_name, region),
    rank = match(country, afolu_top20_order),
    item_label = recode(item, AGR = "Agriculture", CRP = "Crops", LSP = "Livestock"),
    bau_2100 = .data[[paste0(scenario_baseline, "_", focus_year)]],
    miti_2100 = .data[[paste0(scenario_landint, "_", focus_year)]],
    bau_2015 = .data[[paste0(scenario_baseline, "_2015")]],
    change_from_bau_2100 = miti_2100 - bau_2100,
    change_rate_from_bau_2100_pct = if_else(is.finite(bau_2100) & bau_2100 != 0, 100 * (miti_2100 / bau_2100 - 1), NA_real_),
    change_from_bau_2015 = miti_2100 - bau_2015,
    change_rate_from_bau_2015_pct = if_else(is.finite(bau_2015) & bau_2015 != 0, 100 * (miti_2100 / bau_2015 - 1), NA_real_)
  ) %>%
  transmute(
    rank,
    region,
    country,
    item = item_label,
    bau_2100,
    miti_2100,
    change_from_bau_2100,
    change_rate_from_bau_2100_pct,
    bau_2015,
    change_from_bau_2015,
    change_rate_from_bau_2015_pct
  ) %>%
  arrange(rank, item)

fig3c_land_summary_2100 <- GEER_AGMIP %>%
  filter(
    region %in% afolu_top20_2100$region,
    year %in% c(2015L, focus_year),
    scenario %in% c(scenario_baseline, scenario_landint),
    variable == "LAND",
    item %in% c("TOT", "CRP", "ECP", "GRS", "ONV", "NLD", "FOR"),
    unit == "1000 ha"
  ) %>%
  transmute(region, year, scenario, item, value) %>%
  pivot_wider(names_from = item, values_from = value, values_fill = 0) %>%
  left_join(st_drop_geometry(Boundary), by = c("region" = "cty")) %>%
  mutate(
    country = coalesce(country_name, region),
    rank = match(country, afolu_top20_order),
    total_land = TOT,
    `Cropland` = CRP - ECP,
    `Energy Crop` = ECP,
    `Grassland` = GRS,
    `Managed and primary forest` = FOR,
    `Other natural land` = ONV,
    `Non arable land` = NLD
  ) %>%
  pivot_longer(
    cols = c("Cropland", "Energy Crop", "Grassland", "Managed and primary forest", "Other natural land", "Non arable land"),
    names_to = "land_item",
    values_to = "land_area"
  ) %>%
  select(rank, region, country, scenario, year, total_land, land_item, land_area) %>%
  pivot_wider(names_from = c(scenario, year), values_from = c(total_land, land_area)) %>%
  mutate(
    bau_2100 = .data[[paste0("land_area_", scenario_baseline, "_", focus_year)]],
    miti_2100 = .data[[paste0("land_area_", scenario_landint, "_", focus_year)]],
    bau_2015 = .data[[paste0("land_area_", scenario_baseline, "_2015")]],
    total_land_2100_bau = .data[[paste0("total_land_", scenario_baseline, "_", focus_year)]],
    total_land_2100_miti = .data[[paste0("total_land_", scenario_landint, "_", focus_year)]],
    total_land_2015_bau = .data[[paste0("total_land_", scenario_baseline, "_2015")]],
    change_from_bau_2100 = miti_2100 - bau_2100,
    change_rate_from_bau_2100_pct = if_else(is.finite(total_land_2100_bau) & total_land_2100_bau != 0, 100 * change_from_bau_2100 / total_land_2100_bau, NA_real_),
    change_from_bau_2015 = miti_2100 - bau_2015,
    change_rate_from_bau_2015_pct = if_else(is.finite(total_land_2015_bau) & total_land_2015_bau != 0, 100 * change_from_bau_2015 / total_land_2015_bau, NA_real_),
    bau_2100_share_pct = if_else(is.finite(total_land_2100_bau) & total_land_2100_bau != 0, 100 * bau_2100 / total_land_2100_bau, NA_real_),
    miti_2100_share_pct = if_else(is.finite(total_land_2100_miti) & total_land_2100_miti != 0, 100 * miti_2100 / total_land_2100_miti, NA_real_),
    bau_2015_share_pct = if_else(is.finite(total_land_2015_bau) & total_land_2015_bau != 0, 100 * bau_2015 / total_land_2015_bau, NA_real_)
  ) %>%
  select(
    rank,
    region,
    country,
    land_item,
    bau_2100,
    miti_2100,
    change_from_bau_2100,
    change_rate_from_bau_2100_pct,
    bau_2015,
    change_from_bau_2015,
    change_rate_from_bau_2015_pct,
    bau_2100_share_pct,
    miti_2100_share_pct,
    bau_2015_share_pct
  ) %>%
  arrange(rank, land_item)

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
  summarise(value = sum(value, na.rm = TRUE) / 1e9, .groups = "drop") %>%
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
  summarise(value = sum(value, na.rm = TRUE) / 1e6, .groups = "drop") %>%
  mutate(component = "LULUCF")

cost_2100 <- bind_rows(ag_cost_2100, land_cost_2100) %>%
  left_join(st_drop_geometry(Boundary), by = c("region" = "cty")) %>%
  mutate(country = factor(coalesce(country_name, region), levels = rev(afolu_top20_order)))

xprc_change_2100 <- GEER_AGMIP %>%
  filter(
    region %in% afolu_top20_2100$region,
    year == focus_year,
    scenario %in% c(scenario_baseline, scenario_landint),
    variable == "XPRC",
    item %in% c("CRP", "LSP"),
    unit == "USD/t"
  ) %>%
  select(region, item, scenario, value) %>%
  pivot_wider(names_from = scenario, values_from = value) %>%
  mutate(value = if_else(.data[[scenario_baseline]] > 0, 100 * (.data[[scenario_landint]] / .data[[scenario_baseline]] - 1), NA_real_)) %>%
  left_join(st_drop_geometry(Boundary), by = c("region" = "cty")) %>%
  mutate(
    country = factor(coalesce(country_name, region), levels = rev(afolu_top20_order)),
    item = recode(item, CRP = "Crops", LSP = "Livestock"),
    item = factor(item, levels = c("Crops", "Livestock"))
  )

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

world_qdem_wide <- world_qdem %>%
  pivot_wider(names_from = scenario, values_from = qdem) %>%
  mutate(qdem_ratio = if_else(.data[[scenario_baseline]] > 0, .data[[scenario_landint]] / .data[[scenario_baseline]], NA_real_)) %>%
  select(region, year, commodity, qdem_ratio)

world_qsup_wide <- world_qsup %>%
  pivot_wider(names_from = scenario, values_from = qsup) %>%
  mutate(qsup_ratio = if_else(.data[[scenario_baseline]] > 0, .data[[scenario_landint]] / .data[[scenario_baseline]], NA_real_)) %>%
  select(region, year, commodity, qsup_ratio)

world_ef_wide <- world_decomp_base %>%
  select(scenario, region, year, commodity, source, gas, ef) %>%
  pivot_wider(names_from = scenario, values_from = ef) %>%
  mutate(ef_ratio = if_else(.data[[scenario_baseline]] > 0, .data[[scenario_landint]] / .data[[scenario_baseline]], NA_real_)) %>%
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

world_qdem_levels <- world_qdem %>%
  pivot_wider(names_from = scenario, values_from = qdem) %>%
  transmute(
    region,
    year,
    commodity,
    qdem_bau = .data[[scenario_baseline]],
    qdem_miti = .data[[scenario_landint]]
  )

world_qsup_levels <- world_qsup %>%
  pivot_wider(names_from = scenario, values_from = qsup) %>%
  transmute(
    region,
    year,
    commodity,
    qsup_bau = .data[[scenario_baseline]],
    qsup_miti = .data[[scenario_landint]]
  )

world_ef_levels <- world_decomp_base %>%
  select(scenario, region, year, commodity, source, gas, ef) %>%
  pivot_wider(names_from = scenario, values_from = ef) %>%
  transmute(
    region,
    year,
    commodity,
    source,
    gas,
    ef_bau = .data[[scenario_baseline]],
    ef_miti = .data[[scenario_landint]]
  )

world_lmdi_cell_co2eq <- world_ef_levels %>%
  left_join(world_qdem_levels, by = c("region", "year", "commodity")) %>%
  left_join(world_qsup_levels, by = c("region", "year", "commodity")) %>%
  mutate(
    qdem_bau = safe_positive(qdem_bau),
    qdem_miti = safe_positive(qdem_miti),
    qsup_bau = safe_positive(qsup_bau),
    qsup_miti = safe_positive(qsup_miti),
    ef_bau = safe_positive(ef_bau),
    ef_miti = safe_positive(ef_miti),
    e0_kt = qsup_bau * ef_bau,
    e1_kt = qsup_miti * ef_miti,
    m0 = safe_positive(qsup_bau / qdem_bau),
    m1 = safe_positive(qsup_miti / qdem_miti),
    weight = logmean(e1_kt, e0_kt),
    demand_mt = weight * log(qdem_miti / qdem_bau) / 1000,
    structural_mt = weight * log(m1 / m0) / 1000,
    technical_mt = weight * log(ef_miti / ef_bau) / 1000,
    total_change_mt = (e1_kt - e0_kt) / 1000
  ) %>%
  filter(is.finite(demand_mt), is.finite(structural_mt), is.finite(technical_mt), is.finite(total_change_mt))

world_lmdi_country_co2eq <- world_lmdi_cell_co2eq %>%
  group_by(region, year) %>%
  summarise(
    demand_mt = sum(demand_mt, na.rm = TRUE),
    structural_mt = sum(structural_mt, na.rm = TRUE),
    technical_mt = sum(technical_mt, na.rm = TRUE),
    total_change_mt = sum(total_change_mt, na.rm = TRUE),
    check_mt = total_change_mt - (demand_mt + structural_mt + technical_mt),
    .groups = "drop"
  ) %>%
  left_join(st_drop_geometry(Boundary), by = c("region" = "cty")) %>%
  mutate(country = coalesce(country_name, region))

world_lmdi_r5_world_co2eq <- bind_rows(
  world_lmdi_country_co2eq %>%
    left_join(region_map %>% select(region, region5), by = "region") %>%
    filter(!is.na(region5)) %>%
    group_by(region5, year) %>%
    summarise(
      demand_mt = sum(demand_mt, na.rm = TRUE),
      structural_mt = sum(structural_mt, na.rm = TRUE),
      technical_mt = sum(technical_mt, na.rm = TRUE),
      total_change_mt = sum(total_change_mt, na.rm = TRUE),
      check_mt = sum(check_mt, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(region_name = factor(recode(region5, !!!r5_labels), levels = rev(c("World", "OECD+EU", "Asia", "Africa/Middle East", "Latin America", "Former Soviet Union")))),
  world_lmdi_country_co2eq %>%
    group_by(year) %>%
    summarise(
      demand_mt = sum(demand_mt, na.rm = TRUE),
      structural_mt = sum(structural_mt, na.rm = TRUE),
      technical_mt = sum(technical_mt, na.rm = TRUE),
      total_change_mt = sum(total_change_mt, na.rm = TRUE),
      check_mt = sum(check_mt, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(region_name = factor("World", levels = rev(c("World", "OECD+EU", "Asia", "Africa/Middle East", "Latin America", "Former Soviet Union"))))
)

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
    left_join(
      world_qsup_prices %>% filter(!is.na(group)) %>% select(scenario, commodity, region, year, qsup, group),
      by = c("scenario", "commodity", "region", "year", "group")
    ) %>%
    group_by(region, year, scenario, price_var, item = group) %>%
    summarise(value = if_else(sum(qsup, na.rm = TRUE) > 0, sum(price_value * qsup, na.rm = TRUE) / sum(qsup, na.rm = TRUE), NA_real_), .groups = "drop"),
  world_price_decomp %>%
    left_join(world_qsup_prices %>% select(scenario, commodity, region, year, qsup), by = c("scenario", "commodity", "region", "year")) %>%
    group_by(region, year, scenario, price_var) %>%
    summarise(value = if_else(sum(qsup, na.rm = TRUE) > 0, sum(price_value * qsup, na.rm = TRUE) / sum(qsup, na.rm = TRUE), NA_real_), .groups = "drop") %>%
    mutate(item = "AGR")
)

price_decomp_residual_share_2100 <- price_decomp_grouped %>%
  filter(year == focus_year, scenario == scenario_landint, item %in% c("CRP", "LSP")) %>%
  group_by(region, item) %>%
  summarise(
    total_price = sum(value, na.rm = TRUE),
    residual_emission = sum(value[price_var == "residual_emission"], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(residual_share_pct = if_else(is.finite(total_price) & total_price != 0, 100 * residual_emission / total_price, NA_real_))

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
  mutate(cali_change = .data[[scenario_landint]] - .data[[scenario_baseline]]) %>%
  select(region, item, cali_change)

r5_price_food_summary_2100 <- price_decomp_grouped %>%
  filter(year == focus_year, scenario == scenario_landint, item %in% c("CRP", "LSP")) %>%
  group_by(region, item) %>%
  summarise(
    domestic_price = sum(value, na.rm = TRUE),
    residual_emission_price = sum(value[price_var == "residual_emission"], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(
    world_qsup_prices %>%
      filter(year == focus_year, scenario == scenario_landint, !is.na(group)) %>%
      group_by(region, item = group) %>%
      summarise(production_weight = sum(qsup, na.rm = TRUE), .groups = "drop"),
    by = c("region", "item")
  ) %>%
  left_join(region_map %>% select(region, region5), by = "region") %>%
  filter(!is.na(region5), is.finite(production_weight), production_weight > 0) %>%
  group_by(region5, item) %>%
  summarise(
    country_count = n_distinct(region),
    production_weight_sum = sum(production_weight, na.rm = TRUE),
    domestic_price = sum(domestic_price * production_weight, na.rm = TRUE) / production_weight_sum,
    residual_emission_price = sum(residual_emission_price * production_weight, na.rm = TRUE) / production_weight_sum,
    .groups = "drop"
  ) %>%
  mutate(
    domestic_price_excluding_residual = domestic_price - residual_emission_price,
    residual_share_of_total_price_pct = if_else(domestic_price != 0, 100 * residual_emission_price / domestic_price, NA_real_),
    residual_price_increase_pct = if_else(domestic_price_excluding_residual != 0, 100 * residual_emission_price / domestic_price_excluding_residual, NA_real_)
  ) %>%
  left_join(
    GEER_AGMIP_AGG %>%
      filter(
        region %in% names(r5_labels),
        year == focus_year,
        scenario %in% c(scenario_baseline, scenario_landint),
        variable == "FOOD",
        item %in% c("CRP", "LSP"),
        unit == "million kcal"
      ) %>%
      mutate(region5 = recode(region, !!!r5_labels)) %>%
      transmute(region5, item, scenario, food_pj = value * kcal_million_to_pj) %>%
      pivot_wider(names_from = scenario, values_from = food_pj) %>%
      transmute(
        region5,
        item,
        food_bau_pj = .data[[scenario_baseline]],
        food_miti_pj = .data[[scenario_landint]],
        food_change_pj = .data[[scenario_landint]] - .data[[scenario_baseline]],
        food_change_pct_of_bau = if_else(.data[[scenario_baseline]] != 0, 100 * (.data[[scenario_landint]] - .data[[scenario_baseline]]) / .data[[scenario_baseline]], NA_real_)
      ),
    by = c("region5", "item")
  ) %>%
  left_join(
    GEER_AGMIP_AGG %>%
      filter(
        region %in% names(r5_labels),
        year == focus_year,
        scenario %in% c(scenario_baseline, scenario_landint),
        variable == "CALI",
        item %in% c("CRP", "LSP"),
        unit == "kcal/cap/d"
      ) %>%
      mutate(region5 = recode(region, !!!r5_labels)) %>%
      transmute(region5, item, scenario, calorie_intake_kcal_cap_d = value) %>%
      pivot_wider(names_from = scenario, values_from = calorie_intake_kcal_cap_d) %>%
      transmute(
        region5,
        item,
        calorie_intake_bau_kcal_cap_d = .data[[scenario_baseline]],
        calorie_intake_miti_kcal_cap_d = .data[[scenario_landint]],
        calorie_intake_change_kcal_cap_d = .data[[scenario_landint]] - .data[[scenario_baseline]],
        calorie_intake_change_pct_of_bau = if_else(.data[[scenario_baseline]] != 0, 100 * (.data[[scenario_landint]] - .data[[scenario_baseline]]) / .data[[scenario_baseline]], NA_real_)
      ),
    by = c("region5", "item")
  ) %>%
  mutate(
    region5 = factor(region5, levels = r5_labels),
    item_label = recode(item, CRP = "Crops", LSP = "Livestock")
  ) %>%
  arrange(region5, item)

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
    transmute(country = factor(country, levels = rev(country_order)), total_value = total_change_mt)

  ggplot() +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
    geom_col(data = decomp_df, aes(x = value, y = country, fill = change)) +
    geom_point(data = point_df, aes(x = total_value, y = country), size = 2.4, shape = 21, stroke = 0.4, fill = "black", color = "black") +
    scale_fill_manual(values = c("Demand side" = "#9BC7F2", "Technical" = "#E3819C", "Structural" = "#A0EBD0"), labels = jp_labels(jp_decomp_labels)) +
    labs(x = "BaU比の排出変化 (MtCO2eq/年)", y = NULL) +
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
    transmute(region_name, total_value = total_change_mt)

  ggplot() +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
    geom_col(data = decomp_df, aes(x = value, y = region_name, fill = change)) +
    geom_point(data = point_df, aes(x = total_value, y = region_name), size = 2.4, shape = 21, stroke = 0.4, fill = "black", color = "black") +
    scale_fill_manual(values = c("Demand side" = "#9BC7F2", "Technical" = "#E3819C", "Structural" = "#A0EBD0"), labels = jp_labels(jp_decomp_labels)) +
    scale_y_discrete(labels = jp_labels(jp_region_labels)) +
    labs(x = "BaU比の排出変化 (MtCO2eq/年)", y = NULL) +
    theme_main
}

make_world_lmdi_country_plot <- function(plot_year) {
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

  decomp_df <- world_lmdi_country_co2eq %>%
    filter(year == plot_year, region %in% selected_regions) %>%
    transmute(
      country = factor(country, levels = rev(country_order)),
      `Demand side` = demand_mt,
      `Structural` = structural_mt,
      `Technical` = technical_mt
    ) %>%
    pivot_longer(cols = c(`Demand side`, `Structural`, `Technical`), names_to = "change", values_to = "value") %>%
    mutate(change = factor(change, levels = c("Demand side", "Structural", "Technical")))

  point_df <- world_lmdi_country_co2eq %>%
    filter(year == plot_year, region %in% selected_regions) %>%
    transmute(country = factor(country, levels = rev(country_order)), total_value = total_change_mt)

  ggplot() +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
    geom_col(data = decomp_df, aes(x = value, y = country, fill = change)) +
    geom_point(data = point_df, aes(x = total_value, y = country), size = 2.4, shape = 21, stroke = 0.4, fill = "black", color = "black") +
    scale_fill_manual(values = c("Demand side" = "#9BC7F2", "Structural" = "#A0EBD0", "Technical" = "#E3819C"), labels = jp_labels(jp_decomp_labels)) +
    labs(x = "BaU比の排出変化 (MtCO2eq/年)", y = NULL) +
    theme_main
}

make_world_lmdi_r5_plot <- function(plot_year) {
  decomp_df <- world_lmdi_r5_world_co2eq %>%
    filter(year == plot_year) %>%
    transmute(
      region_name,
      `Demand side` = demand_mt,
      `Structural` = structural_mt,
      `Technical` = technical_mt
    ) %>%
    pivot_longer(cols = c(`Demand side`, `Structural`, `Technical`), names_to = "change", values_to = "value") %>%
    mutate(change = factor(change, levels = c("Demand side", "Structural", "Technical")))

  point_df <- world_lmdi_r5_world_co2eq %>%
    filter(year == plot_year) %>%
    transmute(region_name, total_value = total_change_mt)

  ggplot() +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
    geom_col(data = decomp_df, aes(x = value, y = region_name, fill = change)) +
    geom_point(data = point_df, aes(x = total_value, y = region_name), size = 2.4, shape = 21, stroke = 0.4, fill = "black", color = "black") +
    scale_fill_manual(values = c("Demand side" = "#9BC7F2", "Structural" = "#A0EBD0", "Technical" = "#E3819C"), labels = jp_labels(jp_decomp_labels)) +
    scale_y_discrete(labels = jp_labels(jp_region_labels)) +
    labs(x = "BaU比の排出変化 (MtCO2eq/年)", y = NULL) +
    theme_main
}

make_residual_share_vs_cali_scatter <- function(item_code) {
  scatter_df <- afolu_reduction_2100 %>%
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
    annotate("text", x = Inf, y = -Inf, label = ann_text, hjust = 1.02, vjust = -0.2, size = 3.1) +
    scale_color_manual(values = region_border_palette, labels = jp_labels(jp_region_labels)) +
    labs(
      x = "国内食料価格に占める残存排出価格の割合 (%)",
      y = "BaU比のカロリー摂取量変化\n(kcal/人/日)"
    ) +
    theme_main
}

# figure blocks ------------------------------------------------------------

# ---- Fig 0: Carbon price and energy crop demand
fig0_ctax_df <- GEER_CTAX %>%
  filter(region == "USA", item == "TOT", unit == "USD/tCO2e", scenario == scenario_landint)

fig0_ecp_df <- GEER_AGMIP_AGG %>%
  filter(region == "World", variable == "PROD", item == "ECP", unit == "1000 t", scenario == scenario_landint) %>%
  mutate(value = value / 1000)

p_fig0_ctax <- ggplot(fig0_ctax_df, aes(x = year, y = value)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = decadal_years) +
  labs(x = "年", y = "炭素価格 (USD/tCO2)") +
  theme_bw(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 11),
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.margin = margin(6, 8, 6, 8)
  )

p_fig0_ecp <- ggplot(fig0_ecp_df, aes(x = year, y = value)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = decadal_years) +
  labs(x = "年", y = "エネルギー作物需要\n(百万t)") +
  theme_bw(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 11),
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.margin = margin(6, 8, 6, 8)
  )

fig0 <- wrap_plots(p_fig0_ctax, p_fig0_ecp, ncol = 2) +
  plot_annotation(tag_levels = list(c("a", "b")))

save_plot("fig0_carbon_price_and_energy_crop.png", fig0, width = 6, height = 3)

# ---- Fig 1: BaU AFOLU emissions
fig1_top_regions <- fig1_baseline_country_2100 %>%
  group_by(region, country) %>%
  summarise(total_value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total_value)) %>%
  slice_head(n = top_n)

fig1_top_df <- fig1_baseline_country_2100 %>%
  filter(region %in% fig1_top_regions$region) %>%
  mutate(country = factor(country, levels = fig1_top_regions$country))

fig1 <- wrap_plots(
  make_world_ts_panel_with_observed(fig1_world_ts_with_observed, fig1_component_palette, fig1_component_levels, x_label = NULL) +
    guides(fill = "none") +
    theme(plot.margin = margin(5.5, 5.5, 0, 5.5)),
  free(
    make_r5_panel(fig1_baseline_world_r5_2100, fig1_component_palette, fig1_component_levels) +
      guides(fill = "none") +
      theme(plot.margin = margin(5.5, 5.5, 0, 5.5)),
    type = "space",
    side = "r"
  ),
  make_top_country_share_panel(
    fig1_top_df %>% group_by(region, country, component) %>% summarise(value = sum(value, na.rm = TRUE), .groups = "drop"),
    full_total_sum = fig1_baseline_country_2100 %>% group_by(region) %>% summarise(total_value = sum(value, na.rm = TRUE), .groups = "drop") %>% summarise(total_sum = sum(total_value, na.rm = TRUE), .groups = "drop") %>% pull(total_sum),
    palette = fig1_component_palette,
    limits = fig1_component_levels
  ) +
    guides(
      fill = guide_legend(nrow = 2, byrow = TRUE),
      color = guide_legend(nrow = 1, byrow = TRUE)
    ) +
    theme(plot.margin = margin(0, 5.5, 5.5, 5.5)),
  design = "AB\nCC",
  widths = c(1.1, 1.0),
  heights = c(0.75, 1.25)
) +
  plot_annotation(tag_levels = list(c("a", "b", "c"))) &
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    legend.margin = margin(0, 0, 0, 0)
  )

save_plot("fig1_bau_afolu_emissions.png", fig1, width = 5.5, height = 6.2)

write_csv(
  fig1_world_ts_with_observed %>%
    filter(year == 2020L) %>%
    mutate(source = as.character(source), component = as.character(component)) %>%
    select(source, year, component, value) %>%
    arrange(source, component),
  file.path(output_dir, "fig1_world_observed_model_comparison_2020.csv")
)

write_csv(
  fig1_baseline_world_ts %>%
    filter(year %in% c(2050L, 2100L)) %>%
    group_by(year, component) %>%
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    mutate(region_name = "World") %>%
    select(region_name, year, component, value),
  file.path(output_dir, "fig1_world_component_summary_2050_2100.csv")
)

write_csv(
  fig1_baseline_world_ts %>%
    filter(year %in% c(2050L, 2100L)) %>%
    group_by(year) %>%
    summarise(total_value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    mutate(region_name = "World") %>%
    select(region_name, year, total_value),
  file.path(output_dir, "fig1_world_total_summary_2050_2100.csv")
)

write_csv(
  fig1_baseline_world_r5_2100 %>%
    group_by(region_name, component) %>%
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop"),
  file.path(output_dir, "fig1_r5_component_summary_2100.csv")
)

write_csv(
  fig1_baseline_world_r5_2100 %>%
    group_by(region_name) %>%
    summarise(total_value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    mutate(share_pct_of_world = 100 * total_value / sum(fig1_baseline_world_ts %>% filter(year == focus_year) %>% pull(value), na.rm = TRUE)),
  file.path(output_dir, "fig1_r5_total_summary_2100.csv")
)

write_csv(
  fig1_top_df %>%
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
    arrange(rank, component),
  file.path(output_dir, "fig1_top_countries_summary_2100.csv")
)

# ---- Fig 2: AFOLU actual reduction
fig2_top_df <- fig2_actual_country_2100 %>%
  filter(region %in% afolu_top20_2100$region) %>%
  mutate(country = factor(country, levels = afolu_top20_order))

fig2 <- wrap_plots(
  make_world_ts_panel(fig2_actual_world_ts, fig2_component_palette, fig2_component_levels, x_label = NULL) +
    guides(fill = "none") +
    theme(plot.margin = margin(5.5, 5.5, 0, 5.5)),
  free(
    make_r5_panel(fig2_actual_r5_2100, fig2_component_palette, fig2_component_levels) +
      guides(fill = "none") +
      theme(plot.margin = margin(5.5, 5.5, 0, 5.5)),
    type = "space",
    side = "r"
  ),
  make_top_country_share_panel(
    fig2_top_df,
    full_total_sum = fig2_actual_country_2100 %>% group_by(region) %>% summarise(total_value = sum(value, na.rm = TRUE), .groups = "drop") %>% summarise(total_sum = sum(total_value, na.rm = TRUE), .groups = "drop") %>% pull(total_sum),
    palette = fig2_component_palette,
    limits = fig2_component_levels
  ) +
    guides(
      fill = guide_legend(ncol = 3, byrow = TRUE, order = 1),
      color = guide_legend(nrow = 1, byrow = TRUE, order = 2)
    ) +
    theme(plot.margin = margin(0, 5.5, 5.5, 5.5)),
  design = "AB\nCC",
  widths = c(1.1, 0.9),
  heights = c(0.75, 1.25)
) +
  plot_annotation(tag_levels = list(c("a", "b", "c"))) &
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    legend.margin = margin(0, 0, 0, 0),
    legend.text = element_text(size = 8),
    legend.key.size = grid::unit(0.35, "cm"),
    legend.spacing.y = grid::unit(0.05, "cm")
  )

save_plot("fig2_afolu_actual_reduction.png", fig2, width = 5.5, height = 6.5)

write_csv(
  fig2_actual_world_ts %>%
    filter(year %in% c(2050L, 2100L)) %>%
    group_by(year, component) %>%
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    mutate(region_name = "World") %>%
    select(region_name, year, component, value),
  file.path(output_dir, "fig2_world_component_summary_2050_2100.csv")
)

write_csv(
  fig2_actual_world_ts %>%
    filter(year %in% c(2050L, 2100L)) %>%
    group_by(year) %>%
    summarise(total_value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    mutate(region_name = "World") %>%
    select(region_name, year, total_value),
  file.path(output_dir, "fig2_world_total_summary_2050_2100.csv")
)

write_csv(
  fig2_actual_r5_2100 %>%
    group_by(region_name, component) %>%
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop"),
  file.path(output_dir, "fig2_r5_component_summary_2100.csv")
)

write_csv(
  fig2_actual_r5_2100 %>%
    group_by(region_name) %>%
    summarise(total_value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    mutate(share_pct_of_world = 100 * total_value / sum(fig2_actual_world_ts %>% filter(year == focus_year) %>% pull(value), na.rm = TRUE)),
  file.path(output_dir, "fig2_r5_total_summary_2100.csv")
)

write_csv(
  fig2_actual_country_2100 %>%
    group_by(region, country, development_group, component) %>%
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    left_join(
      afolu_top20_2100 %>%
        transmute(
          region,
          country,
          total_value,
          share_pct = 100 * total_value / sum(fig2_actual_country_2100$value, na.rm = TRUE),
          cumulative_share_pct = cumsum(100 * total_value / sum(fig2_actual_country_2100$value, na.rm = TRUE)),
          rank = row_number()
        ),
      by = c("region", "country")
    ) %>%
    arrange(rank, component),
  file.path(output_dir, "fig2_top_countries_summary_2100.csv")
)

write_csv(
  fig2_actual_country_2100 %>%
    group_by(region, country) %>%
    summarise(total_value = sum(value, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(total_value)) %>%
    mutate(
      rank = row_number(),
      share_pct = 100 * total_value / sum(total_value, na.rm = TRUE),
      cumulative_share_pct = cumsum(share_pct)
    ) %>%
    filter(rank %in% c(5, 10, 15, 20)) %>%
    transmute(top_n = rank, cumulative_share_pct = cumulative_share_pct),
  file.path(output_dir, "fig2_cumulative_share_summary.csv")
)

write_csv(
  afolu_reduction_2100 %>%
    mutate(development_group = replace_na(development_group, "Unknown")) %>%
    group_by(development_group) %>%
    summarise(reduction_mt = sum(afolu_reduction, na.rm = TRUE), .groups = "drop") %>%
    mutate(
      world_reduction_mt = sum(reduction_mt, na.rm = TRUE),
      share_pct_of_world = 100 * reduction_mt / world_reduction_mt
    ) %>%
    arrange(desc(reduction_mt)),
  file.path(output_dir, "fig2_development_group_summary_2100.csv")
)

write_csv(
  afolu_reduction_2100 %>%
    mutate(development_group = replace_na(development_group, "Unknown")) %>%
    filter(development_group == "Developing") %>%
    summarise(
      development_group = "Developing",
      reduction_mt = sum(afolu_reduction, na.rm = TRUE)
    ) %>%
    mutate(
      world_reduction_mt = sum(afolu_reduction_2100$afolu_reduction, na.rm = TRUE),
      share_pct_of_world = 100 * reduction_mt / world_reduction_mt
    ),
  file.path(output_dir, "fig2_developing_share_summary_2100.csv")
)

# ---- Fig 3: Top AFOLU reducers and associated changes with cost
p_prod <- ggplot(
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
    labels = jp_labels(jp_component_labels),
    guide = guide_legend(nrow = 3, byrow = TRUE)
  ) +
  labs(x = "生産量 (PJ)", y = NULL) +
  theme_main

p_cali <- make_horizontal_stack_with_point(
  cali_change_2100 %>%
    filter(item %in% c("CRP", "LSP")) %>%
    mutate(component = recode(item, CRP = "Crops", LSP = "Livestock")) %>%
    transmute(country, component, value = change),
  cali_change_2100 %>% filter(item == "AGR") %>% transmute(country, total_value = change),
  "カロリー摂取量\n(kcal/人/日)"
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
  scale_fill_manual(values = land_color, labels = jp_labels(jp_component_labels), guide = guide_legend(nrow = 3, byrow = TRUE)) +
  labs(x = "土地利用変化\n(総土地面積比, %)", y = NULL) +
  theme_main

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
  scale_fill_manual(values = cost_palette, labels = jp_labels(jp_component_labels), guide = guide_legend(ncol = 1)) +
  labs(x = "緩和費用\n(十億USD/年)", y = NULL) +
  theme_main

fig3 <- wrap_plots(p_prod, p_cali, p_land_share, p_cost, ncol = 4, widths = c(1.55, 1.05, 1.25, 1.1)) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "a") &
  theme(legend.position = "bottom", legend.text = element_text(size = 8.5))

save_plot("fig3_top_reduction_country_changes_with_cost_2100.png", fig3, width = 11, height = 4.5)

write_csv(
  fig3b_cali_summary_2100,
  file.path(output_dir, "fig3b_calorie_change_summary_2100.csv")
)

write_csv(
  fig3c_land_summary_2100,
  file.path(output_dir, "fig3c_land_change_summary_2100.csv")
)

# ---- Fig 4: Producer price changes for crops and livestock
fig4 <- ggplot(xprc_change_2100, aes(x = value, y = country)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
  geom_col(fill = "#4C78A8") +
  facet_wrap(~ item, nrow = 1, scales = "free_x", labeller = as_labeller(jp_component_labels)) +
  labs(x = "MITIのBaU比の消費者価格変化 (%)", y = NULL) +
  theme_main

save_plot("fig4_xprc_change_2100.png", fig4, width = 5, height = 4)

# ---- Fig 5: Residual-emission share vs calorie change
fig5 <- wrap_plots(
  make_residual_share_vs_cali_scatter("CRP") +
    guides(color = "none"),
  make_residual_share_vs_cali_scatter("LSP") +
    guides(color = guide_legend(ncol = 3, byrow = TRUE)),
  ncol = 1
) +
  plot_annotation(tag_levels = "a") &
  theme(
    legend.position = "bottom",
    legend.box = "vertical"
  )

save_plot("fig5_residual_share_vs_cali_scatter_2100.png", fig5, width = 6, height = 9)

write_csv(
  r5_price_food_summary_2100 %>%
    transmute(
      region5 = as.character(region5),
      item = item_label,
      country_count,
      production_weight_sum,
      domestic_price,
      residual_emission_price,
      domestic_price_excluding_residual,
      residual_share_of_total_price_pct,
      residual_price_increase_pct,
      food_bau_pj,
      food_miti_pj,
      food_change_pj,
      food_change_pct_of_bau,
      calorie_intake_bau_kcal_cap_d,
      calorie_intake_miti_kcal_cap_d,
      calorie_intake_change_kcal_cap_d,
      calorie_intake_change_pct_of_bau
    ),
  file.path(output_dir, "fig5_r5_price_residual_tax_food_consumption_summary_2100.csv")
)

# ---- Fig 6: Agricultural decomposition in 2100
fig6 <- wrap_plots(
  make_world_final_wedge_r5_total_plot(focus_year),
  make_world_final_wedge_total_plot(focus_year),
  ncol = 2,
  widths = c(0.8, 1)
) + plot_annotation(tag_levels = "a")

save_plot("fig6_agricultural_decomposition_2100.png", fig6, width = 11, height = 5.5)

write_csv(
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
  file.path(output_dir, "fig6_world_r5_decomposition_summary_2100.csv")
)

fig6_top_regions_2100 <- agmip_ag_reduction_rank %>%
  filter(year == focus_year) %>%
  arrange(desc(ag_reduction)) %>%
  slice_head(n = 10) %>%
  pull(region)

fig6_bottom_regions_2100 <- agmip_ag_reduction_rank %>%
  filter(year == focus_year, !region %in% fig6_top_regions_2100) %>%
  arrange(ag_reduction) %>%
  slice_head(n = 10) %>%
  pull(region)

write_csv(
  world_final_wedge_country_co2eq %>%
    filter(year == focus_year, region %in% c(fig6_top_regions_2100, fig6_bottom_regions_2100)) %>%
    left_join(
      agmip_ag_reduction_rank %>% filter(year == focus_year) %>% select(region, ag_reduction),
      by = "region"
    ) %>%
    transmute(region, country, year, ag_reduction, demand_mt, technical_mt, structural_mt, total_change_mt),
  file.path(output_dir, "fig6_top_bottom_country_decomposition_summary_2100.csv")
)

# ---- Fig 6a: Agricultural decomposition in 2100 (Kaya + LMDI)
fig6a <- wrap_plots(
  make_world_lmdi_r5_plot(focus_year),
  make_world_lmdi_country_plot(focus_year),
  ncol = 1,
  heights = c(0.5, 1)
) + plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "a") &
  theme(legend.position = "bottom")

save_plot("fig6a_agricultural_decomposition_lmdi_2100.png", fig6a, width = 5, height = 6)

write_csv(
  world_lmdi_r5_world_co2eq %>%
    filter(year == focus_year) %>%
    mutate(
      demand_share_pct = if_else(abs(total_change_mt) > 0, 100 * demand_mt / total_change_mt, NA_real_),
      structural_share_pct = if_else(abs(total_change_mt) > 0, 100 * structural_mt / total_change_mt, NA_real_),
      technical_share_pct = if_else(abs(total_change_mt) > 0, 100 * technical_mt / total_change_mt, NA_real_)
    ) %>%
    transmute(
      region = as.character(region_name),
      year,
      demand_mt,
      structural_mt,
      technical_mt,
      total_change_mt,
      check_mt,
      demand_share_pct,
      structural_share_pct,
      technical_share_pct
    ),
  file.path(output_dir, "fig6a_world_r5_decomposition_lmdi_summary_2100.csv")
)

write_csv(
  world_lmdi_country_co2eq %>%
    filter(year == focus_year, region %in% c(fig6_top_regions_2100, fig6_bottom_regions_2100)) %>%
    left_join(
      agmip_ag_reduction_rank %>% filter(year == focus_year) %>% select(region, ag_reduction),
      by = "region"
    ) %>%
    mutate(
      demand_share_pct = if_else(abs(total_change_mt) > 0, 100 * demand_mt / total_change_mt, NA_real_),
      structural_share_pct = if_else(abs(total_change_mt) > 0, 100 * structural_mt / total_change_mt, NA_real_),
      technical_share_pct = if_else(abs(total_change_mt) > 0, 100 * technical_mt / total_change_mt, NA_real_)
    ) %>%
    transmute(
      region,
      country,
      year,
      ag_reduction,
      demand_mt,
      structural_mt,
      technical_mt,
      total_change_mt,
      check_mt,
      demand_share_pct,
      structural_share_pct,
      technical_share_pct
    ),
  file.path(output_dir, "fig6a_top_bottom_country_decomposition_lmdi_summary_2100.csv")
)

miti_bau_structural_emission_increase_2100 <- world_lmdi_country_co2eq %>%
  filter(year == focus_year, region %in% country_codes) %>%
  transmute(
    region,
    country,
    ag_emissions_change_mtco2e = total_change_mt,
    structural_emissions_change_mtco2e = structural_mt
  ) %>%
  left_join(
    world_lmdi_cell_co2eq %>%
      filter(year == focus_year, region %in% country_codes) %>%
      group_by(region) %>%
      summarise(ag_emissions_bau_mtco2e = sum(e0_kt, na.rm = TRUE) / 1000, .groups = "drop"),
    by = "region"
  ) %>%
  left_join(
    GEER_AGMIP %>%
      filter(
        region %in% country_codes,
        year == focus_year,
        scenario %in% c(scenario_baseline, scenario_landint),
        variable == "PROD",
        item %in% c("CRP", "LSP"),
        unit == "million kcal"
      ) %>%
      select(region, item, scenario, value) %>%
      pivot_wider(names_from = scenario, values_from = value) %>%
      mutate(
        production_bau_pj = .data[[scenario_baseline]] * kcal_million_to_pj,
        production_change_pj = (.data[[scenario_landint]] - .data[[scenario_baseline]]) * kcal_million_to_pj
      ) %>%
      select(region, item, production_bau_pj, production_change_pj) %>%
      pivot_wider(names_from = item, values_from = c(production_bau_pj, production_change_pj), values_fill = 0) %>%
      transmute(
        region,
        crp_production_bau_pj = production_bau_pj_CRP,
        lsp_production_bau_pj = production_bau_pj_LSP,
        total_production_bau_pj = production_bau_pj_CRP + production_bau_pj_LSP,
        crp_production_change_pj = production_change_pj_CRP,
        lsp_production_change_pj = production_change_pj_LSP,
        total_production_change_pj = production_change_pj_CRP + production_change_pj_LSP
      ),
    by = "region"
  ) %>%
  left_join(
    GEER_AGMIP %>%
      filter(
        region %in% country_codes,
        year == focus_year,
        scenario %in% c(scenario_baseline, scenario_landint),
        variable == "EXPO",
        item %in% c("CRP", "LSP"),
        unit == "million kcal"
      ) %>%
      select(region, item, scenario, value) %>%
      pivot_wider(names_from = scenario, values_from = value) %>%
      mutate(
        export_bau_pj = .data[[scenario_baseline]] * kcal_million_to_pj,
        export_miti_pj = .data[[scenario_landint]] * kcal_million_to_pj,
        export_change_pj = (.data[[scenario_landint]] - .data[[scenario_baseline]]) * kcal_million_to_pj
      ) %>%
      select(region, item, export_bau_pj, export_miti_pj, export_change_pj) %>%
      pivot_wider(names_from = item, values_from = c(export_bau_pj, export_miti_pj, export_change_pj), values_fill = 0) %>%
      transmute(
        region,
        crp_export_bau_pj = export_bau_pj_CRP,
        lsp_export_bau_pj = export_bau_pj_LSP,
        total_export_bau_pj = export_bau_pj_CRP + export_bau_pj_LSP,
        crp_export_miti_pj = export_miti_pj_CRP,
        lsp_export_miti_pj = export_miti_pj_LSP,
        total_export_miti_pj = export_miti_pj_CRP + export_miti_pj_LSP,
        crp_export_change_pj = export_change_pj_CRP,
        lsp_export_change_pj = export_change_pj_LSP,
        total_export_change_pj = export_change_pj_CRP + export_change_pj_LSP
      ),
    by = "region"
  ) %>%
  left_join(
    GEER_AGMIP %>%
      filter(
        region %in% country_codes,
        year == focus_year,
        scenario %in% c(scenario_baseline, scenario_landint),
        variable == "EMIS",
        item %in% c("CRP", "LSP"),
        unit == "MtCO2e"
      ) %>%
      select(region, item, scenario, value) %>%
      pivot_wider(names_from = scenario, values_from = value) %>%
      mutate(
        emissions_bau_mtco2e = .data[[scenario_baseline]],
        emissions_change_mtco2e = .data[[scenario_landint]] - .data[[scenario_baseline]]
      ) %>%
      select(region, item, emissions_bau_mtco2e, emissions_change_mtco2e) %>%
      pivot_wider(names_from = item, values_from = c(emissions_bau_mtco2e, emissions_change_mtco2e), values_fill = 0) %>%
      transmute(
        region,
        crp_emissions_bau_mtco2e = emissions_bau_mtco2e_CRP,
        lsp_emissions_bau_mtco2e = emissions_bau_mtco2e_LSP,
        total_crp_lsp_emissions_bau_mtco2e = emissions_bau_mtco2e_CRP + emissions_bau_mtco2e_LSP,
        crp_emissions_change_mtco2e = emissions_change_mtco2e_CRP,
        lsp_emissions_change_mtco2e = emissions_change_mtco2e_LSP,
        total_crp_lsp_emissions_change_mtco2e = emissions_change_mtco2e_CRP + emissions_change_mtco2e_LSP
      ),
    by = "region"
  ) %>%
  left_join(
    GEER_AGMIP %>%
      filter(
        region %in% country_codes,
        year == focus_year,
        scenario %in% c(scenario_baseline, scenario_landint),
        variable %in% c("EMIS", "PROD"),
        item %in% c("CRP", "LSP"),
        unit %in% c("MtCO2e", "million kcal")
      ) %>%
      group_by(region, item, scenario, variable) %>%
      summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
      mutate(
        value_for_factor = case_when(
          variable == "PROD" ~ value * kcal_million_to_pj,
          TRUE ~ value
        ),
        scenario_label = if_else(scenario == scenario_baseline, "bau", "miti"),
        item_label = tolower(item)
      ) %>%
      select(region, item_label, scenario_label, variable, value_for_factor) %>%
      pivot_wider(names_from = variable, values_from = value_for_factor) %>%
      mutate(emission_factor_mtco2e_per_pj = if_else(PROD != 0, EMIS / PROD, NA_real_)) %>%
      select(region, item_label, scenario_label, emission_factor_mtco2e_per_pj) %>%
      pivot_wider(names_from = c(item_label, scenario_label), values_from = emission_factor_mtco2e_per_pj, values_fill = 0) %>%
      transmute(
        region,
        crp_bau_emission_factor_mtco2e_per_pj = crp_bau,
        lsp_bau_emission_factor_mtco2e_per_pj = lsp_bau,
        crp_miti_emission_factor_mtco2e_per_pj = crp_miti,
        lsp_miti_emission_factor_mtco2e_per_pj = lsp_miti
      ),
    by = "region"
  ) %>%
  mutate(
    crp_export_emissions_bau_mtco2e = crp_export_bau_pj * crp_bau_emission_factor_mtco2e_per_pj,
    lsp_export_emissions_bau_mtco2e = lsp_export_bau_pj * lsp_bau_emission_factor_mtco2e_per_pj,
    total_export_emissions_bau_mtco2e = crp_export_emissions_bau_mtco2e + lsp_export_emissions_bau_mtco2e,
    crp_export_emissions_miti_mtco2e = crp_export_miti_pj * crp_miti_emission_factor_mtco2e_per_pj,
    lsp_export_emissions_miti_mtco2e = lsp_export_miti_pj * lsp_miti_emission_factor_mtco2e_per_pj,
    total_export_emissions_miti_mtco2e = crp_export_emissions_miti_mtco2e + lsp_export_emissions_miti_mtco2e,
    crp_export_emissions_change_mtco2e = crp_export_emissions_miti_mtco2e - crp_export_emissions_bau_mtco2e,
    lsp_export_emissions_change_mtco2e = lsp_export_emissions_miti_mtco2e - lsp_export_emissions_bau_mtco2e,
    total_export_emissions_change_mtco2e = crp_export_emissions_change_mtco2e + lsp_export_emissions_change_mtco2e
  ) %>%
  filter(structural_emissions_change_mtco2e > 0) %>%
  arrange(desc(structural_emissions_change_mtco2e)) %>%
  mutate(rank = row_number()) %>%
  select(
    rank,
    region,
    country,
    ag_emissions_bau_mtco2e,
    ag_emissions_change_mtco2e,
    structural_emissions_change_mtco2e,
    crp_emissions_bau_mtco2e,
    lsp_emissions_bau_mtco2e,
    total_crp_lsp_emissions_bau_mtco2e,
    crp_emissions_change_mtco2e,
    lsp_emissions_change_mtco2e,
    total_crp_lsp_emissions_change_mtco2e,
    crp_production_bau_pj,
    lsp_production_bau_pj,
    total_production_bau_pj,
    crp_production_change_pj,
    lsp_production_change_pj,
    total_production_change_pj,
    crp_export_bau_pj,
    lsp_export_bau_pj,
    total_export_bau_pj,
    crp_export_miti_pj,
    lsp_export_miti_pj,
    total_export_miti_pj,
    crp_export_change_pj,
    lsp_export_change_pj,
    total_export_change_pj,
    crp_bau_emission_factor_mtco2e_per_pj,
    lsp_bau_emission_factor_mtco2e_per_pj,
    crp_miti_emission_factor_mtco2e_per_pj,
    lsp_miti_emission_factor_mtco2e_per_pj,
    crp_export_emissions_bau_mtco2e,
    lsp_export_emissions_bau_mtco2e,
    total_export_emissions_bau_mtco2e,
    crp_export_emissions_miti_mtco2e,
    lsp_export_emissions_miti_mtco2e,
    total_export_emissions_miti_mtco2e,
    crp_export_emissions_change_mtco2e,
    lsp_export_emissions_change_mtco2e,
    total_export_emissions_change_mtco2e
)

write_csv(
  miti_bau_structural_emission_increase_2100,
  file.path(output_dir, "miti_bau_structural_emission_increase_countries_2100.csv")
)

write_csv(
  miti_bau_structural_emission_increase_2100 %>%
    transmute(
      rank,
      region,
      country,
      ag_emissions_change_pct_of_bau = if_else(ag_emissions_bau_mtco2e != 0, 100 * ag_emissions_change_mtco2e / ag_emissions_bau_mtco2e, NA_real_),
      structural_emissions_change_pct_of_bau_ag_emissions = if_else(ag_emissions_bau_mtco2e != 0, 100 * structural_emissions_change_mtco2e / ag_emissions_bau_mtco2e, NA_real_),
      crp_emissions_change_pct_of_bau = if_else(crp_emissions_bau_mtco2e != 0, 100 * crp_emissions_change_mtco2e / crp_emissions_bau_mtco2e, NA_real_),
      lsp_emissions_change_pct_of_bau = if_else(lsp_emissions_bau_mtco2e != 0, 100 * lsp_emissions_change_mtco2e / lsp_emissions_bau_mtco2e, NA_real_),
      total_crp_lsp_emissions_change_pct_of_bau = if_else(total_crp_lsp_emissions_bau_mtco2e != 0, 100 * total_crp_lsp_emissions_change_mtco2e / total_crp_lsp_emissions_bau_mtco2e, NA_real_),
      crp_production_change_pct_of_bau = if_else(crp_production_bau_pj != 0, 100 * crp_production_change_pj / crp_production_bau_pj, NA_real_),
      lsp_production_change_pct_of_bau = if_else(lsp_production_bau_pj != 0, 100 * lsp_production_change_pj / lsp_production_bau_pj, NA_real_),
      total_production_change_pct_of_bau = if_else(total_production_bau_pj != 0, 100 * total_production_change_pj / total_production_bau_pj, NA_real_),
      crp_export_change_pct_of_bau = if_else(crp_export_bau_pj != 0, 100 * crp_export_change_pj / crp_export_bau_pj, NA_real_),
      lsp_export_change_pct_of_bau = if_else(lsp_export_bau_pj != 0, 100 * lsp_export_change_pj / lsp_export_bau_pj, NA_real_),
      total_export_change_pct_of_bau = if_else(total_export_bau_pj != 0, 100 * total_export_change_pj / total_export_bau_pj, NA_real_),
      crp_export_emissions_change_pct_of_bau_export_emissions = if_else(crp_export_emissions_bau_mtco2e != 0, 100 * crp_export_emissions_change_mtco2e / crp_export_emissions_bau_mtco2e, NA_real_),
      lsp_export_emissions_change_pct_of_bau_export_emissions = if_else(lsp_export_emissions_bau_mtco2e != 0, 100 * lsp_export_emissions_change_mtco2e / lsp_export_emissions_bau_mtco2e, NA_real_),
      total_export_emissions_change_pct_of_bau_export_emissions = if_else(total_export_emissions_bau_mtco2e != 0, 100 * total_export_emissions_change_mtco2e / total_export_emissions_bau_mtco2e, NA_real_)
    ),
  file.path(output_dir, "miti_bau_structural_emission_increase_countries_2100_pct.csv")
)

# ---- Fig 7: AFOLU reduction-rate bubble plots
make_afolu_rate_bubble_plot <- function(metric_col, x_label) {
  make_generic_bubble_plot(
    metric_col = metric_col,
    x_label = x_label,
    y_df = afolu_rate_country_fig6 %>% transmute(region, afolu_reduction = afolu_reduction, reduction_rate),
    y_col = "reduction_rate",
    y_label = "AFOLU削減率\n(BaU排出量比, %)",
    geer_agmip = GEER_AGMIP,
    focus_year = focus_year,
    scenario_baseline = scenario_baseline,
    scenario_landint = scenario_landint,
    country_codes = country_codes,
    region_map = region_map,
    boundary = Boundary
  )
}

fig7 <- wrap_plots(
  make_afolu_rate_bubble_plot("land_tot", "BaUの総土地面積 (1000 ha)"),
  make_afolu_rate_bubble_plot("gdppc", "BaUの1人あたりGDP (1000 USD 2017 PPP)"),
  make_afolu_rate_bubble_plot("self_suff_bau", "BaUの農業自給率 (%)"),
  make_afolu_rate_bubble_plot("cali_agr", "MITIのBaU比のカロリー摂取量変化 (kcal/人/日)"),
  ncol = 1
) + plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "a") &
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    legend.text = element_text(size = 10),
    axis.title = element_text(size = 13)
  )

save_plot("fig7_afolu_rate_bubbles_2100.png", fig7, width = 7, height = 16)

# ---- Exploratory regression: demand-side contribution and price impacts
demand_price_exploratory_2100 <- world_lmdi_country_co2eq %>%
  filter(year == focus_year, region %in% country_codes) %>%
  transmute(
    region,
    country,
    demand_reduction_mt = -demand_mt,
    structural_reduction_mt = -structural_mt,
    technical_reduction_mt = -technical_mt,
    total_reduction_mt = -total_change_mt,
    total_positive_reduction_mt = pmax(-demand_mt, 0) + pmax(-structural_mt, 0) + pmax(-technical_mt, 0),
    demand_reduction_share_pct = if_else(total_positive_reduction_mt > 0, 100 * pmax(-demand_mt, 0) / total_positive_reduction_mt, NA_real_),
    demand_reduction_net_share_pct = if_else(is.finite(total_change_mt) & total_change_mt != 0, 100 * demand_mt / total_change_mt, NA_real_),
    demand_mt,
    structural_mt,
    technical_mt,
    total_change_mt
  ) %>%
  left_join(region_map %>% select(region, region5), by = "region") %>%
  left_join(
    price_decomp_grouped %>%
      filter(year == focus_year, scenario == scenario_landint, item %in% c("CRP", "LSP")) %>%
      group_by(region, item) %>%
      summarise(price_2100 = sum(value, na.rm = TRUE), .groups = "drop") %>%
      mutate(
        price_abs_2100 = abs(price_2100),
        item = recode(item, CRP = "Crops", LSP = "Livestock")
      ),
    by = "region"
  ) %>%
  left_join(
    price_decomp_residual_share_2100 %>%
      mutate(item = recode(item, CRP = "Crops", LSP = "Livestock")) %>%
      select(region, item, total_price, residual_emission, price_transfer_rate_pct = residual_share_pct),
    by = c("region", "item")
  ) %>%
  filter(is.finite(demand_reduction_share_pct), is.finite(price_abs_2100), !is.na(region5), !is.na(item))

demand_price_regression_summary_2100 <- demand_price_exploratory_2100 %>%
  group_by(item) %>%
  group_modify(~ {
    fit_df <- .x %>%
      filter(is.finite(demand_reduction_share_pct), is.finite(price_abs_2100), price_abs_2100 > 0)
    if (nrow(fit_df) < 3) {
      return(tibble(n = nrow(fit_df), intercept = NA_real_, slope_log_price_abs = NA_real_, r_squared = NA_real_, p_value = NA_real_))
    }
    fit <- lm(demand_reduction_share_pct ~ log10(price_abs_2100), data = fit_df)
    fit_sum <- summary(fit)
    tibble(
      n = nrow(fit_df),
      intercept = unname(coef(fit)[1]),
      slope_log_price_abs = unname(coef(fit)[2]),
      r_squared = fit_sum$r.squared,
      p_value = coef(fit_sum)[2, 4]
    )
  }) %>%
  ungroup()

fig_exploratory_demand_price <- ggplot(
  demand_price_exploratory_2100 %>% filter(price_abs_2100 > 0),
  aes(x = price_abs_2100, y = demand_reduction_share_pct, color = region5)
) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.35) +
  geom_point(alpha = 0.8, size = 2.2) +
  geom_smooth(aes(group = 1), method = "lm", se = TRUE, color = "black", linewidth = 0.6) +
  facet_wrap(~ item, scales = "free_x", labeller = as_labeller(jp_component_labels)) +
  scale_x_log10() +
  scale_color_manual(values = region_border_palette, labels = jp_labels(jp_region_labels), guide = guide_legend(ncol = 3, byrow = TRUE)) +
  labs(
    x = "2100年の国内価格影響の絶対値 (対数目盛)",
    y = "削減要因に占める需要側要因の割合 (%)"
  ) +
  theme_main +
  theme(legend.position = "bottom")

save_plot("exploratory_demand_contribution_vs_price_2100.png", fig_exploratory_demand_price, width = 7, height = 4.8)

write_csv(
  demand_price_exploratory_2100 %>%
    arrange(item, desc(demand_reduction_share_pct)) %>%
    select(
      region,
      country,
      region5,
      item,
      demand_reduction_mt,
      structural_reduction_mt,
      technical_reduction_mt,
      demand_reduction_share_pct,
      demand_reduction_net_share_pct,
      total_reduction_mt,
      total_positive_reduction_mt,
      price_abs_2100,
      price_2100,
      price_transfer_rate_pct,
      total_price,
      residual_emission
    ),
  file.path(output_dir, "exploratory_demand_price_table_2100.csv")
)

write_csv(
  demand_price_regression_summary_2100,
  file.path(output_dir, "exploratory_demand_price_regression_summary_2100.csv")
)
