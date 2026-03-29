library(tidyverse)
library(gdxrrw)
library(patchwork)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(scales)
# ============================================================
# 4th_check.R
# 目的:
#   LandInt シナリオと BaU（baseline）を比較し、
#   国別の指標作成・可視化・CSV出力を行う。
#
# 想定する入力:
#   - AGMIP_all_scenario.gdx
#   - IAMC_templatePlus.gdx
#   - Mergedcomparison.gdx
#   - define/country.set
#
# 主な出力:
#   - output/0311/4th_check/*.png
#   - output/0311/4th_check/*.csv
# ============================================================

# ---- パッケージ読み込み ------------------------------------------------------
library(tidyverse)
library(gdxrrw)
library(patchwork)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(scales)

# s2 幾何演算を無効化（既存処理に合わせる）
sf_use_s2(FALSE)

# ---- パス解決（実行場所に依存しないようにする） -----------------------------
# Rscript 実行時の --file=... からスクリプトの場所を推定
args_all <- commandArgs(trailingOnly = FALSE)
file_arg <- "--file="
script_arg <- args_all[startsWith(args_all, file_arg)]

script_dir <- if (length(script_arg) > 0) {
  dirname(normalizePath(sub(file_arg, "", script_arg[1]), winslash = "/", mustWork = TRUE))
} else {
  # 対話実行時のフォールバック
  normalizePath("prog", winslash = "/", mustWork = TRUE)
}

# プロジェクト基準ディレクトリと入出力パス
base_dir <- normalizePath(file.path(script_dir, ".."), winslash = "/", mustWork = TRUE)
data_dir <- file.path(base_dir, "data", "0311")
out_dir <- file.path(base_dir, "output", "0311", "4th_check")
country_set_file <- file.path(base_dir, "define", "country.set")

# 出力先を作成（なければ作る）
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
if (!dir.exists(out_dir)) {
  stop("Output directory is not available: ", out_dir)
}

# ---- シナリオ・対象年の定義 --------------------------------------------------
# baseline（BaU）と比較対象（LandInt）
scenario_baseline <- "SSP2_BaU_NoCC_No"
scenario_landint <- "SSP2_1100C_CACN_DAC_NoCC_LandInt_global1"
scenario_no <- "SSP2_1100C_CACN_DAC_NoCC_No"
analysis_years <- c(2050, 2100)

# ---- 図の共通テーマ ----------------------------------------------------------
theme_geer <- theme_bw(base_size = 9) +
  theme(
    panel.grid.major = element_line(color = "grey90", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(size = 6.1, lineheight = 0.9),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title = element_text(size = 9.1),
    plot.subtitle = element_text(size = 7.2),
    plot.caption = element_text(size = 6.6),
    axis.text = element_text(size = 7.4),
    axis.title = element_text(size = 8),
    legend.text = element_text(size = 7.2)
  )

# ---- ユーティリティ関数 -------------------------------------------------------
# 図保存の共通関数
save_plot <- function(plot_obj, filename, width = 10, height = 7) {
  out_file <- file.path(out_dir, filename)
  ggsave(out_file, plot = plot_obj, width = width, height = height, dpi = 260, bg = "white")
  message("Saved: ", out_file)
}

# 列が存在しない場合でも NA ベクトルを返して落ちないようにする
safe_pull <- function(df, name) {
  if (name %in% names(df)) {
    return(df[[name]])
  }
  rep(NA_real_, nrow(df))
}

# 欠損を除いた上で上位/下位 n 件を取得
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

# facet ごとに y 軸を並び替えるための補助関数
reorder_within_local <- function(x, by, within, fun = mean, sep = "___", ...) {
  stats::reorder(paste(x, within, sep = sep), by, FUN = fun)
}

scale_y_reordered_local <- function(..., sep = "___") {
  scale_y_discrete(labels = function(x) gsub(paste0(sep, ".+$"), "", x), ...)
}

# ヒートマップ用: 外れ値の影響を弱める対称レンジ上限（分位点ベース）
sym_fill_limit <- function(x, prob = 0.98) {
  x <- x[is.finite(x)]
  if (length(x) == 0) {
    return(1)
  }
  q <- stats::quantile(abs(x), probs = prob, na.rm = TRUE, names = FALSE)
  max(q, 1e-9)
}

# ---- 国境ポリゴンと国土面積 ---------------------------------------------------
# Natural Earth から国境データを取得し、分析で使う列だけ残す
country_sf <- ne_countries(scale = "medium", returnclass = "sf") %>%
  st_make_valid() %>%
  transmute(region = iso_a3, country_name = admin) %>%
  filter(region != "-99")

# 面積は等積投影 (EPSG:6933) で計算し、Mha 単位へ換算
country_area_tbl <- country_sf %>%
  st_transform(6933) %>%
  mutate(country_area_mha = as.numeric(st_area(geometry)) / 1e10) %>%
  st_drop_geometry() %>%
  select(region, country_area_mha)

# country.set から分析対象国コードを読み込み
country_codes <- if (file.exists(country_set_file)) {
  scan(country_set_file, what = character(), quiet = TRUE)
} else {
  stop("country.set is not available: ", country_set_file)
}

# GDX 読み込み時は "World" を含めて取得
analysis_regions <- c("World", sort(unique(country_codes)))

# ---- GDX 読み込み関数群 -------------------------------------------------------
# AGMIP_all_scenario.gdx / シンボル AGMIP
# 用途: 市場・食料・生産・貿易・土地・農業排出系
read_agmip <- function(variables, items, units, regions, years, scenarios) {
  req <- list(
    name = "AGMIP",
    form = "full",
    uels = list(scenarios, regions, variables, items, units, as.character(years))
  )
  x <- rgdx(file.path(data_dir, "AGMIP_all_scenario.gdx"), req, squeeze = FALSE)
  as.data.frame.table(x$val, responseName = "value", stringsAsFactors = FALSE) %>%
    setNames(c("scenario", "region", "variable", "item", "unit", "year", "value")) %>%
    mutate(year = as.integer(year), value = as.numeric(value))
}

# IAMC_templatePlus.gdx / シンボル IAMC_templatePlus
# 用途: 削減量・削減コスト・削減ポートフォリオ（LandInt側）
read_iamcplus <- function(variables, regions, years, scenarios) {
  req <- list(
    name = "IAMC_templatePlus",
    form = "full",
    uels = list(scenarios, regions, variables, as.character(years))
  )
  x <- rgdx(file.path(data_dir, "IAMC_templatePlus.gdx"), req, squeeze = FALSE)
  as.data.frame.table(x$val, responseName = "value", stringsAsFactors = FALSE) %>%
    setNames(c("scenario", "region", "variable", "year", "value")) %>%
    mutate(year = as.integer(year), value = as.numeric(value))
}

# Mergedcomparison.gdx / IAMCTemp
# 用途: 単位付きの land-use CO2（本スクリプトでは総GHGとしては使わない）
read_merged <- function(variables, units, regions, years, scenarios) {
  req <- list(
    name = "IAMCTemp",
    form = "full",
    uels = list(scenarios, regions, variables, units, as.character(years))
  )
  x <- rgdx(file.path(data_dir, "Mergedcomparison.gdx"), req, squeeze = FALSE)
  as.data.frame.table(x$val, responseName = "value", stringsAsFactors = FALSE) %>%
    setNames(c("scenario", "region", "variable", "unit", "year", "value")) %>%
    mutate(year = as.integer(year), value = as.numeric(value))
}

# Mergedcomparison.gdx / IAMCTempwoU
# 用途: land-cover 変化と land-use CO2 詳細
read_merged_wou <- function(variables, regions, years, scenarios) {
  req <- list(
    name = "IAMCTempwoU",
    form = "full",
    uels = list(scenarios, regions, variables, as.character(years))
  )
  x <- rgdx(file.path(data_dir, "Mergedcomparison.gdx"), req, squeeze = FALSE)
  as.data.frame.table(x$val, responseName = "value", stringsAsFactors = FALSE) %>%
    setNames(c("scenario", "region", "variable", "year", "value")) %>%
    mutate(year = as.integer(year), value = as.numeric(value))
}

# Mergedcomparison.gdx / AREA
# 用途: land_code 単位の面積変化を直接取得
read_area <- function(regions, years, scenarios) {
  req <- list(
    name = "AREA",
    form = "full",
    uels = list(scenarios, regions, as.character(years), c("FRS", "CL", "PAS", "PRMFRS", "SECFRS"), c("LUM"))
  )
  x <- rgdx(file.path(data_dir, "Mergedcomparison.gdx"), req, squeeze = FALSE)
  as.data.frame.table(x$val, responseName = "value", stringsAsFactors = FALSE) %>%
    setNames(c("scenario", "region", "year", "land_code", "model", "value")) %>%
    mutate(year = as.integer(year), value = as.numeric(value))
}

# ---- 読み込む変数セットの定義 -------------------------------------------------
detail_items <- c("WHT", "RIC", "CGR", "RUM", "NRM", "DRY")

# AGMIPの valid な (variable, item, unit) 組を明示
# 後で semi_join して不要組み合わせを落とす
agmip_specs <- bind_rows(
  # ...（既存の tribble/expand_grid はそのまま）...
)

# ---- データ読み込み -----------------------------------------------------------
# AGMIP（非EMIS）
agmip_non_emis_df <- read_agmip(
  variables = c("CALI", "EmisRed", "EmisRedCH4", "EmisRedN2O", "XPRP", "PROD", "NETT", "LAND"),
  items = c("TOT", "AGR", "CRP", "LSP", "FOR", "GRS", detail_items),
  units = c("kcal/cap/d", "MtCO2e", "USD/t", "1000 t", "1000 ha"),
  regions = analysis_regions,
  years = analysis_years,
  scenarios = c(scenario_baseline, scenario_landint)
) %>%
  semi_join(agmip_specs, by = c("variable", "item", "unit"))

# AGMIP（EMIS）
agmip_emis_df <- read_agmip(
  variables = c("EMIS"),
  items = c("tot", "ent", "mmg", "rcc", "sfr", "mas", "mgr", "crs", "bcr"),
  units = c("MtCO2e"),
  regions = analysis_regions,
  years = analysis_years,
  scenarios = c(scenario_baseline, scenario_landint)
) %>%
  semi_join(agmip_specs, by = c("variable", "item", "unit"))

agmip_df <- bind_rows(agmip_non_emis_df, agmip_emis_df)

# IAMC_templatePlus（LandIntとNo）
plus_df <- read_iamcplus(
  variables = iamcplus_vars,
  regions = analysis_regions,
  years = analysis_years,
  scenarios = c(scenario_landint, scenario_no)
)

# Mergedcomparison（land-use CO2）
merged_df <- read_merged(
  variables = c("Emi_CO2_AFO"),
  units = c("Mt CO2/yr"),
  regions = analysis_regions,
  years = analysis_years,
  scenarios = c(scenario_baseline, scenario_landint)
)

# Mergedcomparison（land-cover/内訳）
merged_wou_df <- read_merged_wou(
  variables = c(
    "Lan_Cov_Frs", "Lan_Cov_Cro", "Lan_Cov_Pst",
    "Emi_CO2_AFO_Lan", "Emi_CO2_AFO_Def", "Emi_CO2_AFO_Aff", "Emi_CO2_AFO_For_Man"
  ),
  regions = analysis_regions,
  years = analysis_years,
  scenarios = c(scenario_baseline, scenario_landint)
)

# Mergedcomparison（AREA）
area_df <- read_area(
  regions = analysis_regions,
  years = analysis_years,
  scenarios = c(scenario_baseline, scenario_landint)
)

# 読み込み件数ログ（データ欠落の早期検知）
message("AGMIP rows: ", nrow(agmip_df))
message("IAMC_templatePlus rows: ", nrow(plus_df))
message("IAMCTemp rows: ", nrow(merged_df))
message("IAMCTempwoU rows: ", nrow(merged_wou_df))
message("AREA rows: ", nrow(area_df))
message("country.set countries: ", length(country_codes))
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
out_dir <- file.path(base_dir, "output", "0311", "4th_check")
country_set_file <- file.path(base_dir, "define", "country.set")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
if (!dir.exists(out_dir)) {
  stop("Output directory is not available: ", out_dir)
}

scenario_baseline <- "SSP2_BaU_NoCC_No"
scenario_landint <- "SSP2_1100C_CACN_DAC_NoCC_LandInt_global1"
scenario_no <- "SSP2_1100C_CACN_DAC_NoCC_No"
analysis_years <- c(2050, 2100)

theme_geer <- theme_bw(base_size = 9) +
  theme(
    panel.grid.major = element_line(color = "grey90", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(size = 6.1, lineheight = 0.9),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title = element_text(size = 9.1),
    plot.subtitle = element_text(size = 7.2),
    plot.caption = element_text(size = 6.6),
    axis.text = element_text(size = 7.4),
    axis.title = element_text(size = 8),
    legend.text = element_text(size = 7.2)
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

sym_fill_limit <- function(x, prob = 0.98) {
  x <- x[is.finite(x)]
  if (length(x) == 0) {
    return(1)
  }
  q <- stats::quantile(abs(x), probs = prob, na.rm = TRUE, names = FALSE)
  max(q, 1e-9)
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

# Use AGMIP_all_scenario.gdx / AGMIP for market, food, production, trade, land,
# and agricultural-emission variables.
read_agmip <- function(variables, items, units, regions, years, scenarios) {
  req <- list(
    name = "AGMIP",
    form = "full",
    uels = list(scenarios, regions, variables, items, units, as.character(years))
  )
  x <- rgdx(file.path(data_dir, "AGMIP_all_scenario.gdx"), req, squeeze = FALSE)
  as.data.frame.table(x$val, responseName = "value", stringsAsFactors = FALSE) %>%
    setNames(c("scenario", "region", "variable", "item", "unit", "year", "value")) %>%
    mutate(year = as.integer(year), value = as.numeric(value))
}

# Use IAMC_templatePlus.gdx / IAMC_templatePlus only for LandInt-side reduction
# amount, reduction cost, and reduction-portfolio variables.
read_iamcplus <- function(variables, regions, years, scenarios) {
  req <- list(
    name = "IAMC_templatePlus",
    form = "full",
    uels = list(scenarios, regions, variables, as.character(years))
  )
  x <- rgdx(file.path(data_dir, "IAMC_templatePlus.gdx"), req, squeeze = FALSE)
  as.data.frame.table(x$val, responseName = "value", stringsAsFactors = FALSE) %>%
    setNames(c("scenario", "region", "variable", "year", "value")) %>%
    mutate(year = as.integer(year), value = as.numeric(value))
}

# Use Mergedcomparison.gdx / IAMCTemp only for land-use-side CO2 variables with
# units. This script does not use Mergedcomparison as economy-wide total GHG.
read_merged <- function(variables, units, regions, years, scenarios) {
  req <- list(
    name = "IAMCTemp",
    form = "full",
    uels = list(scenarios, regions, variables, units, as.character(years))
  )
  x <- rgdx(file.path(data_dir, "Mergedcomparison.gdx"), req, squeeze = FALSE)
  as.data.frame.table(x$val, responseName = "value", stringsAsFactors = FALSE) %>%
    setNames(c("scenario", "region", "variable", "unit", "year", "value")) %>%
    mutate(year = as.integer(year), value = as.numeric(value))
}

# Use Mergedcomparison.gdx / IAMCTempwoU for land-cover and land-use-side CO2
# detail variables.
read_merged_wou <- function(variables, regions, years, scenarios) {
  req <- list(
    name = "IAMCTempwoU",
    form = "full",
    uels = list(scenarios, regions, variables, as.character(years))
  )
  x <- rgdx(file.path(data_dir, "Mergedcomparison.gdx"), req, squeeze = FALSE)
  as.data.frame.table(x$val, responseName = "value", stringsAsFactors = FALSE) %>%
    setNames(c("scenario", "region", "variable", "year", "value")) %>%
    mutate(year = as.integer(year), value = as.numeric(value))
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
    mutate(year = as.integer(year), value = as.numeric(value))
}

detail_items <- c("WHT", "RIC", "CGR", "RUM", "NRM", "DRY")

agmip_specs <- bind_rows(
  tribble(
    ~variable, ~item, ~unit,
    "CALI", "TOT", "kcal/cap/d",
    "EmisRed", "TOT", "MtCO2e",
    "EmisRed", "AGR", "MtCO2e",
    "EmisRed", "LSP", "MtCO2e",
    "EmisRedCH4", "TOT", "MtCO2e",
    "EmisRedCH4", "AGR", "MtCO2e",
    "EmisRedCH4", "LSP", "MtCO2e",
    "EmisRedN2O", "TOT", "MtCO2e",
    "EmisRedN2O", "AGR", "MtCO2e",
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
    "LAND", "FOR", "1000 ha"
  ),
  tidyr::expand_grid(variable = c("XPRP", "PROD", "NETT"), item = detail_items) %>%
    mutate(unit = if_else(variable == "XPRP", "USD/t", "1000 t")),
  tribble(
    ~variable, ~item, ~unit,
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
)

agmip_non_emis_df <- read_agmip(
  variables = c("CALI", "EmisRed", "EmisRedCH4", "EmisRedN2O", "XPRP", "PROD", "NETT", "LAND"),
  items = c("TOT", "AGR", "CRP", "LSP", "FOR", "GRS", detail_items),
  units = c("kcal/cap/d", "MtCO2e", "USD/t", "1000 t", "1000 ha"),
  regions = analysis_regions,
  years = analysis_years,
  scenarios = c(scenario_baseline, scenario_landint)
) %>%
  semi_join(agmip_specs, by = c("variable", "item", "unit"))

agmip_emis_df <- read_agmip(
  variables = c("EMIS"),
  items = c("tot", "ent", "mmg", "rcc", "sfr", "mas", "mgr", "crs", "bcr"),
  units = c("MtCO2e"),
  regions = analysis_regions,
  years = analysis_years,
  scenarios = c(scenario_baseline, scenario_landint)
) %>%
  semi_join(agmip_specs, by = c("variable", "item", "unit"))

agmip_df <- bind_rows(agmip_non_emis_df, agmip_emis_df)

iamcplus_vars <- c(
  "Red_Cos_AFO_Agr",
  "Emi_Red_CH4_AFO_Agr",
  "Emi_Red_N2O_AFO_Agr",
  "Emi_Red_CO2_AFO_Lan",
  "Emi_Red_equ_CH4_AFO_Agr",
  "Emi_Red_equ_N2O_AFO_Agr",
  "Emi_Red_equ_GHG_AFO_Agr",
  "Emi_Red_equ_GHG_AFO_Lan",
  "Emi_Red_CH4_AFO_Agr_Liv",
  "Emi_Red_CH4_AFO_Agr_Liv_Ent_Fer",
  "Emi_Red_CH4_AFO_Agr_Liv_Man_Man",
  "Emi_Red_CH4_AFO_Agr_Ric",
  "Emi_Red_equ_CH4_AFO_Agr_Liv",
  "Emi_Red_equ_CH4_AFO_Agr_Liv_Ent_Fer",
  "Emi_Red_equ_CH4_AFO_Agr_Liv_Man_Man",
  "Emi_Red_equ_CH4_AFO_Agr_Ric",
  "Emi_Red_N2O_AFO_Agr_Man_Soi",
  "Emi_Red_N2O_AFO_Agr_Liv_Man_Man",
  "Emi_Red_N2O_AFO_Agr_Ric",
  "Emi_Red_equ_N2O_AFO_Agr_Man_Soi",
  "Emi_Red_equ_N2O_AFO_Agr_Liv_Man_Man",
  "Emi_Red_equ_N2O_AFO_Agr_Ric",
  "Emi_Red_CO2_AFO_Lan_Pla",
  "Emi_Red_CO2_AFO_Lan_Ref",
  "Emi_Red_CO2_AFO_Lan_FPr",
  "Emi_Red_CO2_AFO_Lan_Enr",
  "Emi_equ_CH4_AFO_Agr",
  "Emi_equ_N2O_AFO_Agr"
)

plus_df <- read_iamcplus(
  variables = iamcplus_vars,
  regions = analysis_regions,
  years = analysis_years,
  scenarios = c(scenario_landint, scenario_no)
)

merged_df <- read_merged(
  variables = c("Emi_CO2_AFO"),
  units = c("Mt CO2/yr"),
  regions = analysis_regions,
  years = analysis_years,
  scenarios = c(scenario_baseline, scenario_landint)
)

merged_wou_df <- read_merged_wou(
  variables = c(
    "Lan_Cov_Frs",
    "Lan_Cov_Cro",
    "Lan_Cov_Pst",
    "Emi_CO2_AFO_Lan",
    "Emi_CO2_AFO_Def",
    "Emi_CO2_AFO_Aff",
    "Emi_CO2_AFO_For_Man"
  ),
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
message("AREA rows: ", nrow(area_df))
message("country.set countries: ", length(country_codes))

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
  mutate(scenario_tag = case_when(
    scenario == scenario_landint ~ "landint",
    scenario == scenario_no ~ "no",
    TRUE ~ scenario
  )) %>%
  select(region, year, variable, scenario_tag, value) %>%
  pivot_wider(
    names_from = c(variable, scenario_tag),
    values_from = value,
    names_glue = "{variable}_value_{scenario_tag}"
  )

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

area_wide <- area_df %>%
  pivot_wider(names_from = scenario, values_from = value) %>%
  mutate(
    baseline = .data[[scenario_baseline]],
    landint = .data[[scenario_landint]],
    change_landint_vs_baseline = landint - baseline,
    improvement_baseline_vs_landint = baseline - landint
  ) %>%  # ============================================================
  # 4th_check.R
  # 目的:
  #   LandInt シナリオと BaU（baseline）を比較し、
  #   国別の指標作成・可視化・CSV出力を行う。
  #
  # 想定する入力:
  #   - AGMIP_all_scenario.gdx
  #   - IAMC_templatePlus.gdx
  #   - Mergedcomparison.gdx
  #   - define/country.set
  #
  # 主な出力:
  #   - output/0311/4th_check/*.png
  #   - output/0311/4th_check/*.csv
  # ============================================================
  
  # ---- パッケージ読み込み ------------------------------------------------------
  library(tidyverse)
  library(gdxrrw)
  library(patchwork)
  library(sf)
  library(rnaturalearth)
  library(rnaturalearthdata)
  library(scales)
  
  # s2 幾何演算を無効化（既存処理に合わせる）
  sf_use_s2(FALSE)
  
  # ---- パス解決（実行場所に依存しないようにする） -----------------------------
  # Rscript 実行時の --file=... からスクリプトの場所を推定
  args_all <- commandArgs(trailingOnly = FALSE)
  file_arg <- "--file="
  script_arg <- args_all[startsWith(args_all, file_arg)]
  
  script_dir <- if (length(script_arg) > 0) {
    dirname(normalizePath(sub(file_arg, "", script_arg[1]), winslash = "/", mustWork = TRUE))
  } else {
    # 対話実行時のフォールバック
    normalizePath("prog", winslash = "/", mustWork = TRUE)
  }
  
  # プロジェクト基準ディレクトリと入出力パス
  base_dir <- normalizePath(file.path(script_dir, ".."), winslash = "/", mustWork = TRUE)
  data_dir <- file.path(base_dir, "data", "0311")
  out_dir <- file.path(base_dir, "output", "0311", "4th_check")
  country_set_file <- file.path(base_dir, "define", "country.set")
  
  # 出力先を作成（なければ作る）
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(out_dir)) {
    stop("Output directory is not available: ", out_dir)
  }
  
  # ---- シナリオ・対象年の定義 --------------------------------------------------
  # baseline（BaU）と比較対象（LandInt）
  scenario_baseline <- "SSP2_BaU_NoCC_No"
  scenario_landint <- "SSP2_1100C_CACN_DAC_NoCC_LandInt_global1"
  scenario_no <- "SSP2_1100C_CACN_DAC_NoCC_No"
  analysis_years <- c(2050, 2100)
  
  # ---- 図の共通テーマ ----------------------------------------------------------
  theme_geer <- theme_bw(base_size = 9) +
    theme(
      panel.grid.major = element_line(color = "grey90", linewidth = 0.3),
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      legend.title = element_blank(),
      strip.background = element_blank(),
      strip.text = element_text(size = 6.1, lineheight = 0.9),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      plot.title = element_text(size = 9.1),
      plot.subtitle = element_text(size = 7.2),
      plot.caption = element_text(size = 6.6),
      axis.text = element_text(size = 7.4),
      axis.title = element_text(size = 8),
      legend.text = element_text(size = 7.2)
    )
  
  # ---- ユーティリティ関数 -------------------------------------------------------
  # 図保存の共通関数
  save_plot <- function(plot_obj, filename, width = 10, height = 7) {
    out_file <- file.path(out_dir, filename)
    ggsave(out_file, plot = plot_obj, width = width, height = height, dpi = 260, bg = "white")
    message("Saved: ", out_file)
  }
  
  # 列が存在しない場合でも NA ベクトルを返して落ちないようにする
  safe_pull <- function(df, name) {
    if (name %in% names(df)) {
      return(df[[name]])
    }
    rep(NA_real_, nrow(df))
  }
  
  # 欠損を除いた上で上位/下位 n 件を取得
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
  
  # facet ごとに y 軸を並び替えるための補助関数
  reorder_within_local <- function(x, by, within, fun = mean, sep = "___", ...) {
    stats::reorder(paste(x, within, sep = sep), by, FUN = fun)
  }
  
  scale_y_reordered_local <- function(..., sep = "___") {
    scale_y_discrete(labels = function(x) gsub(paste0(sep, ".+$"), "", x), ...)
  }
  
  # ヒートマップ用: 外れ値の影響を弱める対称レンジ上限（分位点ベース）
  sym_fill_limit <- function(x, prob = 0.98) {
    x <- x[is.finite(x)]
    if (length(x) == 0) {
      return(1)
    }
    q <- stats::quantile(abs(x), probs = prob, na.rm = TRUE, names = FALSE)
    max(q, 1e-9)
  }
  
  # ---- 国境ポリゴンと国土面積 ---------------------------------------------------
  # Natural Earth から国境データを取得し、分析で使う列だけ残す
  country_sf <- ne_countries(scale = "medium", returnclass = "sf") %>%
    st_make_valid() %>%
    transmute(region = iso_a3, country_name = admin) %>%
    filter(region != "-99")
  
  # 面積は等積投影 (EPSG:6933) で計算し、Mha 単位へ換算
  country_area_tbl <- country_sf %>%
    st_transform(6933) %>%
    mutate(country_area_mha = as.numeric(st_area(geometry)) / 1e10) %>%
    st_drop_geometry() %>%
    select(region, country_area_mha)
  
  # country.set から分析対象国コードを読み込み
  country_codes <- if (file.exists(country_set_file)) {
    scan(country_set_file, what = character(), quiet = TRUE)
  } else {
    stop("country.set is not available: ", country_set_file)
  }
  
  # GDX 読み込み時は "World" を含めて取得
  analysis_regions <- c("World", sort(unique(country_codes)))
  
  # ---- GDX 読み込み関数群 -------------------------------------------------------
  # AGMIP_all_scenario.gdx / シンボル AGMIP
  # 用途: 市場・食料・生産・貿易・土地・農業排出系
  read_agmip <- function(variables, items, units, regions, years, scenarios) {
    req <- list(
      name = "AGMIP",
      form = "full",
      uels = list(scenarios, regions, variables, items, units, as.character(years))
    )
    x <- rgdx(file.path(data_dir, "AGMIP_all_scenario.gdx"), req, squeeze = FALSE)
    as.data.frame.table(x$val, responseName = "value", stringsAsFactors = FALSE) %>%
      setNames(c("scenario", "region", "variable", "item", "unit", "year", "value")) %>%
      mutate(year = as.integer(year), value = as.numeric(value))
  }
  
  # IAMC_templatePlus.gdx / シンボル IAMC_templatePlus
  # 用途: 削減量・削減コスト・削減ポートフォリオ（LandInt側）
  read_iamcplus <- function(variables, regions, years, scenarios) {
    req <- list(
      name = "IAMC_templatePlus",
      form = "full",
      uels = list(scenarios, regions, variables, as.character(years))
    )
    x <- rgdx(file.path(data_dir, "IAMC_templatePlus.gdx"), req, squeeze = FALSE)
    as.data.frame.table(x$val, responseName = "value", stringsAsFactors = FALSE) %>%
      setNames(c("scenario", "region", "variable", "year", "value")) %>%
      mutate(year = as.integer(year), value = as.numeric(value))
  }
  
  # Mergedcomparison.gdx / IAMCTemp
  # 用途: 単位付きの land-use CO2（本スクリプトでは総GHGとしては使わない）
  read_merged <- function(variables, units, regions, years, scenarios) {
    req <- list(
      name = "IAMCTemp",
      form = "full",
      uels = list(scenarios, regions, variables, units, as.character(years))
    )
    x <- rgdx(file.path(data_dir, "Mergedcomparison.gdx"), req, squeeze = FALSE)
    as.data.frame.table(x$val, responseName = "value", stringsAsFactors = FALSE) %>%
      setNames(c("scenario", "region", "variable", "unit", "year", "value")) %>%
      mutate(year = as.integer(year), value = as.numeric(value))
  }
  
  # Mergedcomparison.gdx / IAMCTempwoU
  # 用途: land-cover 変化と land-use CO2 詳細
  read_merged_wou <- function(variables, regions, years, scenarios) {
    req <- list(
      name = "IAMCTempwoU",
      form = "full",
      uels = list(scenarios, regions, variables, as.character(years))
    )
    x <- rgdx(file.path(data_dir, "Mergedcomparison.gdx"), req, squeeze = FALSE)
    as.data.frame.table(x$val, responseName = "value", stringsAsFactors = FALSE) %>%
      setNames(c("scenario", "region", "variable", "year", "value")) %>%
      mutate(year = as.integer(year), value = as.numeric(value))
  }
  
  # Mergedcomparison.gdx / AREA
  # 用途: land_code 単位の面積変化を直接取得
  read_area <- function(regions, years, scenarios) {
    req <- list(
      name = "AREA",
      form = "full",
      uels = list(scenarios, regions, as.character(years), c("FRS", "CL", "PAS", "PRMFRS", "SECFRS"), c("LUM"))
    )
    x <- rgdx(file.path(data_dir, "Mergedcomparison.gdx"), req, squeeze = FALSE)
    as.data.frame.table(x$val, responseName = "value", stringsAsFactors = FALSE) %>%
      setNames(c("scenario", "region", "year", "land_code", "model", "value")) %>%
      mutate(year = as.integer(year), value = as.numeric(value))
  }
  
  # ---- 読み込む変数セットの定義 -------------------------------------------------
  detail_items <- c("WHT", "RIC", "CGR", "RUM", "NRM", "DRY")
  
  # AGMIPの valid な (variable, item, unit) 組を明示
  # 後で semi_join して不要組み合わせを落とす
  agmip_specs <- bind_rows(
    # ...（既存の tribble/expand_grid はそのまま）...
  )
  
  # ---- データ読み込み -----------------------------------------------------------
  # AGMIP（非EMIS）
  agmip_non_emis_df <- read_agmip(
    variables = c("CALI", "EmisRed", "EmisRedCH4", "EmisRedN2O", "XPRP", "PROD", "NETT", "LAND"),
    items = c("TOT", "AGR", "CRP", "LSP", "FOR", "GRS", detail_items),
    units = c("kcal/cap/d", "MtCO2e", "USD/t", "1000 t", "1000 ha"),
    regions = analysis_regions,
    years = analysis_years,
    scenarios = c(scenario_baseline, scenario_landint)
  ) %>%
    semi_join(agmip_specs, by = c("variable", "item", "unit"))
  
  # AGMIP（EMIS）
  agmip_emis_df <- read_agmip(
    variables = c("EMIS"),
    items = c("tot", "ent", "mmg", "rcc", "sfr", "mas", "mgr", "crs", "bcr"),
    units = c("MtCO2e"),
    regions = analysis_regions,
    years = analysis_years,
    scenarios = c(scenario_baseline, scenario_landint)
  ) %>%
    semi_join(agmip_specs, by = c("variable", "item", "unit"))
  
  agmip_df <- bind_rows(agmip_non_emis_df, agmip_emis_df)
  
  # IAMC_templatePlus（LandIntとNo）
  plus_df <- read_iamcplus(
    variables = iamcplus_vars,
    regions = analysis_regions,
    years = analysis_years,
    scenarios = c(scenario_landint, scenario_no)
  )
  
  # Mergedcomparison（land-use CO2）
  merged_df <- read_merged(
    variables = c("Emi_CO2_AFO"),
    units = c("Mt CO2/yr"),
    regions = analysis_regions,
    years = analysis_years,
    scenarios = c(scenario_baseline, scenario_landint)
  )
  
  # Mergedcomparison（land-cover/内訳）
  merged_wou_df <- read_merged_wou(
    variables = c(
      "Lan_Cov_Frs", "Lan_Cov_Cro", "Lan_Cov_Pst",
      "Emi_CO2_AFO_Lan", "Emi_CO2_AFO_Def", "Emi_CO2_AFO_Aff", "Emi_CO2_AFO_For_Man"
    ),
    regions = analysis_regions,
    years = analysis_years,
    scenarios = c(scenario_baseline, scenario_landint)
  )
  
  # Mergedcomparison（AREA）
  area_df <- read_area(
    regions = analysis_regions,
    years = analysis_years,
    scenarios = c(scenario_baseline, scenario_landint)
  )
  
  # 読み込み件数ログ（データ欠落の早期検知）
  message("AGMIP rows: ", nrow(agmip_df))
  message("IAMC_templatePlus rows: ", nrow(plus_df))
  message("IAMCTemp rows: ", nrow(merged_df))
  message("IAMCTempwoU rows: ", nrow(merged_wou_df))
  message("AREA rows: ", nrow(area_df))
  message("country.set countries: ", length(country_codes))
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
  filter(region != "World")

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

area_country <- area_wide %>%
  filter(region != "World") %>%
  mutate(land_code = recode(land_code, FRS = "frs", CL = "cl", PAS = "pas", PRMFRS = "prmfrs", SECFRS = "secfrs")) %>%
  pivot_wider(
    names_from = land_code,
    values_from = c(baseline, landint, change_landint_vs_baseline, improvement_baseline_vs_landint),
    names_glue = "{land_code}_{.value}"
  )

# Build the country-level core table using original variables and simple
# Baseline vs LandInt changes only.
country_core <- agmip_country %>%
  left_join(plus_country, by = c("region", "year")) %>%
  left_join(merged_country, by = c("region", "year")) %>%
  left_join(merged_wou_country, by = c("region", "year")) %>%
  left_join(area_country, by = c("region", "year")) %>%
  left_join(st_drop_geometry(country_sf), by = "region") %>%
  left_join(country_area_tbl, by = "region") %>%
  filter(region %in% country_codes) %>%
  mutate(
    land_co2_total_baseline = safe_pull(., "Emi_CO2_AFO__Mt CO2/yr_baseline"),
    land_co2_total_landint = safe_pull(., "Emi_CO2_AFO__Mt CO2/yr_landint"),
    land_co2_total_change = safe_pull(., "Emi_CO2_AFO__Mt CO2/yr_change_landint_vs_baseline"),
    land_co2_total_improvement = safe_pull(., "Emi_CO2_AFO__Mt CO2/yr_improvement_baseline_vs_landint"),
    land_co2_baseline = safe_pull(., "Emi_CO2_AFO_Lan_baseline"),
    land_co2_landint = safe_pull(., "Emi_CO2_AFO_Lan_landint"),
    land_co2_change = safe_pull(., "Emi_CO2_AFO_Lan_change_landint_vs_baseline"),
    land_co2_improvement = safe_pull(., "Emi_CO2_AFO_Lan_improvement_baseline_vs_landint"),
    deforestation_improvement = safe_pull(., "Emi_CO2_AFO_Def_improvement_baseline_vs_landint"),
    afforestation_change = safe_pull(., "Emi_CO2_AFO_Aff_change_landint_vs_baseline"),
    forest_management_change = safe_pull(., "Emi_CO2_AFO_For_Man_change_landint_vs_baseline"),
    forest_area_change_mha = safe_pull(., "Lan_Cov_Frs_change_landint_vs_baseline"),
    cropland_area_change_mha = safe_pull(., "Lan_Cov_Cro_change_landint_vs_baseline"),
    pasture_area_change_mha = safe_pull(., "Lan_Cov_Pst_change_landint_vs_baseline"),
    forest_area_change_pct_country = 100 * forest_area_change_mha / pmax(country_area_mha, 1e-12),
    cropland_area_change_pct_country = 100 * cropland_area_change_mha / pmax(country_area_mha, 1e-12),
    pasture_area_change_pct_country = 100 * pasture_area_change_mha / pmax(country_area_mha, 1e-12),
    calorie_change = safe_pull(., "CALI__TOT__kcal/cap/d_change_landint_vs_baseline"),
    emisred_tot_landint = safe_pull(., "EmisRed__TOT__MtCO2e_landint"),
    emisred_agr_landint = safe_pull(., "EmisRed__AGR__MtCO2e_landint"),
    emisred_lsp_landint = safe_pull(., "EmisRed__LSP__MtCO2e_landint"),
    emisred_ch4_tot_landint = safe_pull(., "EmisRedCH4__TOT__MtCO2e_landint"),
    emisred_ch4_agr_landint = safe_pull(., "EmisRedCH4__AGR__MtCO2e_landint"),
    emisred_ch4_lsp_landint = safe_pull(., "EmisRedCH4__LSP__MtCO2e_landint"),
    emisred_n2o_tot_landint = safe_pull(., "EmisRedN2O__TOT__MtCO2e_landint"),
    emisred_n2o_agr_landint = safe_pull(., "EmisRedN2O__AGR__MtCO2e_landint"),
    price_change_agr = safe_pull(., "XPRP__AGR__USD/t_change_landint_vs_baseline"),
    price_change_crp = safe_pull(., "XPRP__CRP__USD/t_change_landint_vs_baseline"),
    price_change_lsp = safe_pull(., "XPRP__LSP__USD/t_change_landint_vs_baseline"),
    prod_change_agr = safe_pull(., "PROD__AGR__1000 t_change_landint_vs_baseline"),
    prod_change_crp = safe_pull(., "PROD__CRP__1000 t_change_landint_vs_baseline"),
    prod_change_lsp = safe_pull(., "PROD__LSP__1000 t_change_landint_vs_baseline"),
    nett_change_agr = safe_pull(., "NETT__AGR__1000 t_change_landint_vs_baseline"),
    nett_change_crp = safe_pull(., "NETT__CRP__1000 t_change_landint_vs_baseline"),
    nett_change_lsp = safe_pull(., "NETT__LSP__1000 t_change_landint_vs_baseline"),
    land_change_agr_kha = safe_pull(., "LAND__AGR__1000 ha_change_landint_vs_baseline"),
    land_change_crp_kha = safe_pull(., "LAND__CRP__1000 ha_change_landint_vs_baseline"),
    land_change_grs_kha = safe_pull(., "LAND__GRS__1000 ha_change_landint_vs_baseline"),
    land_change_for_kha = safe_pull(., "LAND__FOR__1000 ha_change_landint_vs_baseline"),
    ag_emis_ent_change = safe_pull(., "EMIS__ent__MtCO2e_change_landint_vs_baseline"),
    ag_emis_mmg_change = safe_pull(., "EMIS__mmg__MtCO2e_change_landint_vs_baseline"),
    ag_emis_rcc_change = safe_pull(., "EMIS__rcc__MtCO2e_change_landint_vs_baseline"),
    ag_emis_sfr_change = safe_pull(., "EMIS__sfr__MtCO2e_change_landint_vs_baseline"),
    ag_emis_mas_change = safe_pull(., "EMIS__mas__MtCO2e_change_landint_vs_baseline"),
    ag_emis_mgr_change = safe_pull(., "EMIS__mgr__MtCO2e_change_landint_vs_baseline"),
    ag_emis_crs_change = safe_pull(., "EMIS__crs__MtCO2e_change_landint_vs_baseline"),
    ag_emis_bcr_change = safe_pull(., "EMIS__bcr__MtCO2e_change_landint_vs_baseline"),
    ag_emis_total_change = rowSums(cbind(
      ag_emis_ent_change, ag_emis_mmg_change, ag_emis_rcc_change, ag_emis_sfr_change,
      ag_emis_mas_change, ag_emis_mgr_change, ag_emis_crs_change, ag_emis_bcr_change
    ), na.rm = TRUE),
    ag_emis_total_improvement = -ag_emis_total_change,
    ag_emis_crop_change = rowSums(cbind(ag_emis_rcc_change, ag_emis_sfr_change, ag_emis_mas_change, ag_emis_crs_change, ag_emis_bcr_change), na.rm = TRUE),
    ag_emis_livestock_change = rowSums(cbind(ag_emis_ent_change, ag_emis_mmg_change, ag_emis_mgr_change), na.rm = TRUE),
    ag_emis_crop_improvement = -ag_emis_crop_change,
    ag_emis_livestock_improvement = -ag_emis_livestock_change,
    ag_cost_landint = safe_pull(., "Red_Cos_AFO_Agr_value_landint"),
    ag_cost_no = safe_pull(., "Red_Cos_AFO_Agr_value_no"),
    ag_ch4_reduction_landint = safe_pull(., "Emi_Red_CH4_AFO_Agr_value_landint"),
    ag_n2o_reduction_landint = safe_pull(., "Emi_Red_N2O_AFO_Agr_value_landint"),
    land_co2_reduction_landint = safe_pull(., "Emi_Red_CO2_AFO_Lan_value_landint"),
    ag_ch4_reduction_equ_landint = safe_pull(., "Emi_Red_equ_CH4_AFO_Agr_value_landint"),
    ag_n2o_reduction_equ_landint = safe_pull(., "Emi_Red_equ_N2O_AFO_Agr_value_landint"),
    ag_total_reduction_landint = safe_pull(., "Emi_Red_equ_GHG_AFO_Agr_value_landint"),
    land_total_reduction_landint = safe_pull(., "Emi_Red_equ_GHG_AFO_Lan_value_landint"),
    ag_ch4_reduction_equ_no = safe_pull(., "Emi_Red_equ_CH4_AFO_Agr_value_no"),
    ag_n2o_reduction_equ_no = safe_pull(., "Emi_Red_equ_N2O_AFO_Agr_value_no"),
    ag_total_reduction_no = safe_pull(., "Emi_Red_equ_GHG_AFO_Agr_value_no"),
    land_total_reduction_no = safe_pull(., "Emi_Red_equ_GHG_AFO_Lan_value_no"),
    red_ch4_liv_landint = safe_pull(., "Emi_Red_CH4_AFO_Agr_Liv_value_landint"),
    red_ch4_ent_landint = safe_pull(., "Emi_Red_CH4_AFO_Agr_Liv_Ent_Fer_value_landint"),
    red_ch4_mman_landint = safe_pull(., "Emi_Red_CH4_AFO_Agr_Liv_Man_Man_value_landint"),
    red_ch4_ric_landint = safe_pull(., "Emi_Red_CH4_AFO_Agr_Ric_value_landint"),
    red_equ_ch4_liv_landint = safe_pull(., "Emi_Red_equ_CH4_AFO_Agr_Liv_value_landint"),
    red_equ_ch4_ent_landint = safe_pull(., "Emi_Red_equ_CH4_AFO_Agr_Liv_Ent_Fer_value_landint"),
    red_equ_ch4_mman_landint = safe_pull(., "Emi_Red_equ_CH4_AFO_Agr_Liv_Man_Man_value_landint"),
    red_equ_ch4_ric_landint = safe_pull(., "Emi_Red_equ_CH4_AFO_Agr_Ric_value_landint"),
    red_n2o_soil_landint = safe_pull(., "Emi_Red_N2O_AFO_Agr_Man_Soi_value_landint"),
    red_n2o_mman_landint = safe_pull(., "Emi_Red_N2O_AFO_Agr_Liv_Man_Man_value_landint"),
    red_n2o_ric_landint = safe_pull(., "Emi_Red_N2O_AFO_Agr_Ric_value_landint"),
    red_equ_n2o_soil_landint = safe_pull(., "Emi_Red_equ_N2O_AFO_Agr_Man_Soi_value_landint"),
    red_equ_n2o_mman_landint = safe_pull(., "Emi_Red_equ_N2O_AFO_Agr_Liv_Man_Man_value_landint"),
    red_equ_n2o_ric_landint = safe_pull(., "Emi_Red_equ_N2O_AFO_Agr_Ric_value_landint"),
    red_land_pla_landint = safe_pull(., "Emi_Red_CO2_AFO_Lan_Pla_value_landint"),
    red_land_ref_landint = safe_pull(., "Emi_Red_CO2_AFO_Lan_Ref_value_landint"),
    red_land_fpr_landint = safe_pull(., "Emi_Red_CO2_AFO_Lan_FPr_value_landint"),
    red_land_enr_landint = safe_pull(., "Emi_Red_CO2_AFO_Lan_Enr_value_landint"),
    red_land_pla_no = safe_pull(., "Emi_Red_CO2_AFO_Lan_Pla_value_no"),
    red_land_ref_no = safe_pull(., "Emi_Red_CO2_AFO_Lan_Ref_value_no"),
    red_land_fpr_no = safe_pull(., "Emi_Red_CO2_AFO_Lan_FPr_value_no"),
    red_land_enr_no = safe_pull(., "Emi_Red_CO2_AFO_Lan_Enr_value_no"),
    residual_ch4_landint = safe_pull(., "Emi_equ_CH4_AFO_Agr_value_landint"),
    residual_n2o_landint = safe_pull(., "Emi_equ_N2O_AFO_Agr_value_landint"),
    residual_ch4_no = safe_pull(., "Emi_equ_CH4_AFO_Agr_value_no"),
    residual_n2o_no = safe_pull(., "Emi_equ_N2O_AFO_Agr_value_no")
  )

goods_detail <- country_core %>%
  transmute(
    region, country_name, year,
    WHT_price_change = safe_pull(., "XPRP__WHT__USD/t_change_landint_vs_baseline"),
    RIC_price_change = safe_pull(., "XPRP__RIC__USD/t_change_landint_vs_baseline"),
    CGR_price_change = safe_pull(., "XPRP__CGR__USD/t_change_landint_vs_baseline"),
    RUM_price_change = safe_pull(., "XPRP__RUM__USD/t_change_landint_vs_baseline"),
    NRM_price_change = safe_pull(., "XPRP__NRM__USD/t_change_landint_vs_baseline"),
    DRY_price_change = safe_pull(., "XPRP__DRY__USD/t_change_landint_vs_baseline"),
    WHT_prod_change = safe_pull(., "PROD__WHT__1000 t_change_landint_vs_baseline"),
    RIC_prod_change = safe_pull(., "PROD__RIC__1000 t_change_landint_vs_baseline"),
    CGR_prod_change = safe_pull(., "PROD__CGR__1000 t_change_landint_vs_baseline"),
    RUM_prod_change = safe_pull(., "PROD__RUM__1000 t_change_landint_vs_baseline"),
    NRM_prod_change = safe_pull(., "PROD__NRM__1000 t_change_landint_vs_baseline"),
    DRY_prod_change = safe_pull(., "PROD__DRY__1000 t_change_landint_vs_baseline"),
    WHT_nett_change = safe_pull(., "NETT__WHT__1000 t_change_landint_vs_baseline"),
    RIC_nett_change = safe_pull(., "NETT__RIC__1000 t_change_landint_vs_baseline"),
    CGR_nett_change = safe_pull(., "NETT__CGR__1000 t_change_landint_vs_baseline"),
    RUM_nett_change = safe_pull(., "NETT__RUM__1000 t_change_landint_vs_baseline"),
    NRM_nett_change = safe_pull(., "NETT__NRM__1000 t_change_landint_vs_baseline"),
    DRY_nett_change = safe_pull(., "NETT__DRY__1000 t_change_landint_vs_baseline")
  )

calc_concentration <- function(df, value_col, label) {
  df %>%
    filter(is.finite(.data[[value_col]]), .data[[value_col]] > 0) %>%
    group_by(year) %>%
    arrange(desc(.data[[value_col]]), .by_group = TRUE) %>%
    mutate(
      indicator = label,
      global_total = sum(.data[[value_col]], na.rm = TRUE),
      rank = row_number(),
      value_share = .data[[value_col]] / pmax(global_total, 1e-12),
      cum_share = cumsum(value_share),
      cum_country_share = row_number() / n()
    ) %>%
    ungroup()
}

concentration_2050 <- bind_rows(
  calc_concentration(country_core, "land_co2_improvement", "Land-use CO2 improvement: Mergedcomparison / IAMCTempwoU / Emi_CO2_AFO_Lan / baseline - landint"),
  calc_concentration(country_core, "ag_emis_total_improvement", "Agricultural EMIS improvement: AGMIP / sum[EMIS(ent,mmg,rcc,sfr,mas,mgr,crs,bcr)] / baseline - landint"),
  calc_concentration(country_core, "land_co2_reduction_landint", "Land CO2 reduction: IAMC_templatePlus / Emi_Red_CO2_AFO_Lan / LandInt")
) %>%
  filter(year == 2050)

topn_summary_2050 <- concentration_2050 %>%
  group_by(indicator) %>%
  summarise(
    `Top 5` = sum(value_share[rank <= 5], na.rm = TRUE),
    `Top 10` = sum(value_share[rank <= 10], na.rm = TRUE),
    `Top 15` = sum(value_share[rank <= 15], na.rm = TRUE),
    `Top 20` = sum(value_share[rank <= 20], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = starts_with("Top"), names_to = "top_group", values_to = "share")

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

country_top_market_2050 <- bind_rows(
  rank_metric(country_core %>% filter(year == 2050), "price_change_agr", "Agricultural producer price change: AGMIP / XPRP(AGR) / landint - baseline", "desc", n = 8),
  rank_metric(country_core %>% filter(year == 2050), "calorie_change", "Calorie change: AGMIP / CALI(TOT) / landint - baseline", "asc", n = 8),
  rank_metric(country_core %>% filter(year == 2050), "prod_change_crp", "Crop production change: AGMIP / PROD(CRP) / landint - baseline", "asc", n = 8),
  rank_metric(country_core %>% filter(year == 2050), "prod_change_lsp", "Livestock production change: AGMIP / PROD(LSP) / landint - baseline", "asc", n = 8)
)

country_top_land_2050 <- bind_rows(
  rank_metric(country_core %>% filter(year == 2050), "forest_area_change_pct_country", "Forest area change share: Mergedcomparison / Lan_Cov_Frs / 100 * (landint - baseline) / country area", "desc", n = 8),
  rank_metric(country_core %>% filter(year == 2050), "cropland_area_change_pct_country", "Cropland area change share: Mergedcomparison / Lan_Cov_Cro / 100 * (landint - baseline) / country area", "asc", n = 8),
  rank_metric(country_core %>% filter(year == 2050), "pasture_area_change_pct_country", "Pasture area change share: Mergedcomparison / Lan_Cov_Pst / 100 * (landint - baseline) / country area", "asc", n = 8),
  rank_metric(country_core %>% filter(year == 2050), "deforestation_improvement", "Deforestation improvement: Mergedcomparison / Emi_CO2_AFO_Def / baseline - landint", "desc", n = 8),
  rank_metric(country_core %>% filter(year == 2050), "afforestation_change", "Afforestation change: Mergedcomparison / Emi_CO2_AFO_Aff / landint - baseline", "desc", n = 8)
)

structure_panel_2050 <- bind_rows(
  rank_metric(country_core %>% filter(year == 2050), "prod_change_crp", "Structure change: AGMIP / PROD(CRP) / landint - baseline", "abs"),
  rank_metric(country_core %>% filter(year == 2050), "prod_change_lsp", "Structure change: AGMIP / PROD(LSP) / landint - baseline", "abs"),
  rank_metric(country_core %>% filter(year == 2050), "nett_change_crp", "Structure change: AGMIP / NETT(CRP) / landint - baseline", "abs"),
  rank_metric(country_core %>% filter(year == 2050), "nett_change_lsp", "Structure change: AGMIP / NETT(LSP) / landint - baseline", "abs"),
  rank_metric(country_core %>% filter(year == 2050), "land_change_crp_kha", "Structure change: AGMIP / LAND(CRP) / landint - baseline", "abs"),
  rank_metric(country_core %>% filter(year == 2050), "land_change_for_kha", "Structure change: AGMIP / LAND(FOR) / landint - baseline", "abs"),
  rank_metric(country_core %>% filter(year == 2050), "land_change_grs_kha", "Structure change: AGMIP / LAND(GRS) / landint - baseline", "abs")
)

readr::write_csv(country_core %>% filter(year == 2050), file.path(out_dir, "country_core_2050.csv"))
readr::write_csv(country_core %>% filter(year == 2100), file.path(out_dir, "country_core_2100.csv"))
readr::write_csv(concentration_2050, file.path(out_dir, "country_concentration_2050.csv"))
readr::write_csv(bind_rows(country_top_market_2050, country_top_land_2050), file.path(out_dir, "country_top_panels_2050.csv"))
readr::write_csv(goods_detail %>% filter(year == 2050), file.path(out_dir, "country_goods_detail_2050.csv"))

scatter_2050 <- country_core %>% filter(year == 2050)
top_ag_2050 <- top_non_missing(scatter_2050, "ag_total_reduction_landint", n = 12, desc = TRUE)
top_ag_emis_2050 <- top_non_missing(scatter_2050, "ag_emis_total_improvement", n = 12, desc = TRUE)
top_land_2050 <- top_non_missing(scatter_2050, "land_co2_improvement", n = 12, desc = TRUE)

# Use Mergedcomparison.gdx / IAMCTemp / Emi_CO2_AFO to map baseline land-use-side
# CO2 before looking at mitigation differences.
p01 <- country_sf %>%
  left_join(country_core %>% filter(year == 2050) %>% select(region, land_co2_total_baseline), by = "region") %>%
  ggplot() +
  geom_sf(aes(fill = land_co2_total_baseline), color = NA) +
  scale_fill_gradient(low = "grey95", high = "#8C2D04", labels = label_comma()) +
  labs(
    title = "Map (2050): Mergedcomparison.gdx / IAMCTemp / variable = Emi_CO2_AFO / unit = Mt CO2/yr / scenario = SSP2_BaU_NoCC_No",
    subtitle = "Value plotted = baseline level of land-use-side CO2 variable Emi_CO2_AFO for country.set countries",
    fill = "Emi_CO2_AFO\nbaseline",
    caption = "Interpretation note: Emi_CO2_AFO is treated as land-use-side CO2, not total GHG"
  ) +
  theme_geer +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank())
save_plot(p01, "map_land_co2_baseline_2050.png", width = 12, height = 6.5)

# Use AGMIP_all_scenario.gdx / AGMIP / EMIS source-item sum to show where
# baseline agricultural emissions are concentrated.
baseline_ag_bar <- scatter_2050 %>%
  mutate(ag_emis_total_baseline = rowSums(cbind(
    safe_pull(scatter_2050, "EMIS__ent__MtCO2e_baseline"),
    safe_pull(scatter_2050, "EMIS__mmg__MtCO2e_baseline"),
    safe_pull(scatter_2050, "EMIS__rcc__MtCO2e_baseline"),
    safe_pull(scatter_2050, "EMIS__sfr__MtCO2e_baseline"),
    safe_pull(scatter_2050, "EMIS__mas__MtCO2e_baseline"),
    safe_pull(scatter_2050, "EMIS__mgr__MtCO2e_baseline"),
    safe_pull(scatter_2050, "EMIS__crs__MtCO2e_baseline"),
    safe_pull(scatter_2050, "EMIS__bcr__MtCO2e_baseline")
  ), na.rm = TRUE)) %>%
  slice_max(order_by = ag_emis_total_baseline, n = 20, with_ties = FALSE) %>%
  arrange(ag_emis_total_baseline) %>%
  mutate(country = factor(coalesce(country_name, region), levels = coalesce(country_name, region)))

p02 <- ggplot(baseline_ag_bar, aes(x = ag_emis_total_baseline, y = country)) +
  geom_col(fill = "#6C757D") +
  scale_x_continuous(labels = label_comma()) +
  labs(
    title = "Top 20 baseline agricultural-emission countries (2050): AGMIP / EMIS(ent+mmg+rcc+sfr+mas+mgr+crs+bcr)",
    subtitle = "Value plotted = source-item EMIS sum under SSP2_BaU_NoCC_No",
    x = "Agricultural EMIS baseline [MtCO2e]",
    y = "Country",
    caption = "This plot excludes land-use CO2 and focuses on agriculture-side EMIS from AGMIP"
  ) +
  theme_geer
save_plot(p02, "bar_top20_ag_emis_baseline_2050.png", width = 10.5, height = 7.5)

# Compare concentration across three different indicators in 2050.
p03 <- ggplot(concentration_2050, aes(x = cum_country_share, y = cum_share, color = indicator)) +
  geom_line(linewidth = 1.05) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey65") +
  scale_x_continuous(labels = percent_format()) +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "Concentration curves (2050): land-use CO2 improvement, agricultural EMIS improvement, AFOLUB agricultural reduction",
    subtitle = "Indicators = Mergedcomparison / Emi_CO2_AFO_Lan baseline - landint; AGMIP / sum[EMIS sources] baseline - landint; IAMC_templatePlus / CH4+N2O reduction in LandInt",
    x = "Cumulative share of countries ranked within each indicator",
    y = "Cumulative share of global indicator total",
    caption = "All country rankings are restricted to define/country.set"
  ) +
  theme_geer
save_plot(p03, "concentration_curves_2050.png", width = 11, height = 7)

# Compare top-N shares across the same three indicators.
p04 <- ggplot(topn_summary_2050, aes(x = top_group, y = share, fill = indicator)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "Top-N shares (2050): land-use CO2 improvement, agricultural EMIS improvement, AFOLUB agricultural reduction",
    subtitle = "Top groups = Top 5, Top 10, Top 15, Top 20; shares are computed separately within each indicator",
    x = "Top-N country group",
    y = "Share of global total",
    caption = "Indicator definitions are the same as in the concentration-curve figure"
  ) +
  theme_geer
save_plot(p04, "topn_shares_2050.png", width = 11, height = 7)

# Use Mergedcomparison.gdx / IAMCTempwoU / Emi_CO2_AFO_Lan to map land-use-side
# CO2 improvement.
p05 <- country_sf %>%
  left_join(country_core %>% filter(year == 2050) %>% select(region, land_co2_improvement), by = "region") %>%
  ggplot() +
  geom_sf(aes(fill = land_co2_improvement), color = NA) +
  scale_fill_gradient(low = "grey95", high = "#0B7285", labels = label_comma()) +
  labs(
    title = "Map (2050): Mergedcomparison.gdx / IAMCTempwoU / variable = Emi_CO2_AFO_Lan / baseline - landint",
    subtitle = "Value plotted = SSP2_BaU_NoCC_No - SSP2_1100C_CACN_DAC_NoCC_LandInt_global1 for land-use-side CO2 only",
    fill = "Baseline -\nLandInt",
    caption = "This figure is land-use-side only"
  ) +
  theme_geer +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank())
save_plot(p05, "map_land_co2_improvement_2050.png", width = 12, height = 6.5)

# Use AGMIP_all_scenario.gdx / AGMIP / EMIS source-item sum to map agriculture-side
# emissions improvement.
p06 <- country_sf %>%
  left_join(country_core %>% filter(year == 2050) %>% select(region, ag_emis_total_improvement), by = "region") %>%
  ggplot() +
  geom_sf(aes(fill = ag_emis_total_improvement), color = NA) +
  scale_fill_gradient(low = "grey95", high = "#2B8A3E", labels = label_comma()) +
  labs(
    title = "Map (2050): AGMIP_all_scenario.gdx / AGMIP / sum[EMIS(ent,mmg,rcc,sfr,mas,mgr,crs,bcr)] / baseline - landint",
    subtitle = "Value plotted = agriculture-side EMIS improvement from source-item sum; land-use emissions are excluded",
    fill = "Agricultural\nbaseline - LandInt",
    caption = "This figure is agriculture-side only and does not use Mergedcomparison / GHG"
  ) +
  theme_geer +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank())
save_plot(p06, "map_ag_emis_improvement_2050.png", width = 12, height = 6.5)

# Compare AFOLUB agricultural reduction cost and AFOLUB agricultural reduction
# in CO2e using Emi_Red_equ_GHG_AFO_Agr.
p07 <- ggplot(scatter_2050, aes(x = ag_cost_landint, y = ag_total_reduction_landint)) +
  geom_point(alpha = 0.75, color = "#0B7285", size = 2.1) +
  geom_text(
    data = top_ag_2050,
    aes(label = region),
    size = 2.5,
    nudge_y = 0.02 * max(abs(scatter_2050$ag_total_reduction_landint), na.rm = TRUE),
    na.rm = TRUE
  ) +
  scale_x_continuous(labels = label_comma()) +
  scale_y_continuous(labels = label_comma()) +
  labs(
    title = "Scatter (2050): IAMC_templatePlus / Red_Cos_AFO_Agr vs IAMC_templatePlus / Emi_Red_equ_GHG_AFO_Agr",
    subtitle = "x = AFOLUB agricultural reduction cost in LandInt; y = AFOLUB agricultural reduction in CO2e",
    x = "Red_Cos_AFO_Agr [LandInt scenario]",
    y = "Emi_Red_equ_GHG_AFO_Agr [LandInt scenario]",
    caption = "Agricultural reduction is taken from the EQU variable, not from raw CH4 and N2O sums"
  ) +
  theme_geer
save_plot(p07, "scatter_ag_reduction_vs_cost_2050.png", width = 10, height = 7)

# Compare calorie change and agriculture-side emissions improvement.
p08 <- ggplot(scatter_2050, aes(x = calorie_change, y = ag_emis_total_improvement)) +
  geom_hline(yintercept = 0, color = "grey70", linewidth = 0.4) +
  geom_vline(xintercept = 0, color = "grey70", linewidth = 0.4) +
  geom_point(alpha = 0.75, color = "#7A3E9D", size = 2.1) +
  geom_text(
    data = top_ag_emis_2050,
    aes(label = region),
    size = 2.5,
    nudge_y = 0.02 * max(abs(scatter_2050$ag_emis_total_improvement), na.rm = TRUE),
    na.rm = TRUE
  ) +
  labs(
    title = "Scatter (2050): AGMIP / CALI(TOT) / landint - baseline vs AGMIP / sum[EMIS source items] / baseline - landint",
    subtitle = "x = calorie intake change; y = agriculture-side EMIS improvement",
    x = "CALI(TOT) change [landint - baseline, kcal/cap/d]",
    y = "Agricultural EMIS improvement [baseline - landint, MtCO2e]",
    caption = "Agricultural EMIS = ent + mmg + rcc + sfr + mas + mgr + crs + bcr"
  ) +
  theme_geer
save_plot(p08, "scatter_ag_emis_vs_cali_2050.png", width = 10, height = 7)

# Compare agricultural price change and agriculture-side emissions improvement.
p09 <- ggplot(scatter_2050, aes(x = price_change_agr, y = ag_emis_total_improvement)) +
  geom_hline(yintercept = 0, color = "grey70", linewidth = 0.4) +
  geom_vline(xintercept = 0, color = "grey70", linewidth = 0.4) +
  geom_point(alpha = 0.75, color = "#C96F28", size = 2.1) +
  geom_text(
    data = top_ag_emis_2050,
    aes(label = region),
    size = 2.5,
    nudge_y = 0.02 * max(abs(scatter_2050$ag_emis_total_improvement), na.rm = TRUE),
    na.rm = TRUE
  ) +
  labs(
    title = "Scatter (2050): AGMIP / XPRP(AGR) / landint - baseline vs AGMIP / sum[EMIS source items] / baseline - landint",
    subtitle = "x = agricultural producer price change; y = agriculture-side EMIS improvement",
    x = "XPRP(AGR) change [landint - baseline, USD/t]",
    y = "Agricultural EMIS improvement [baseline - landint, MtCO2e]",
    caption = "This plot uses original variables and a direct difference only"
  ) +
  theme_geer
save_plot(p09, "scatter_ag_emis_vs_ag_price_2050.png", width = 10, height = 7)

# Compare crop production change and crop-side agricultural emissions improvement.
p10 <- ggplot(scatter_2050, aes(x = prod_change_crp, y = ag_emis_crop_improvement)) +
  geom_hline(yintercept = 0, color = "grey70", linewidth = 0.4) +
  geom_vline(xintercept = 0, color = "grey70", linewidth = 0.4) +
  geom_point(alpha = 0.75, color = "#457B9D", size = 2.1) +
  geom_text(
    data = top_ag_emis_2050,
    aes(label = region),
    size = 2.5,
    nudge_y = 0.02 * max(abs(scatter_2050$ag_emis_crop_improvement), na.rm = TRUE),
    na.rm = TRUE
  ) +
  labs(
    title = "Scatter (2050): AGMIP / PROD(CRP) / landint - baseline vs AGMIP / EMIS(rcc+sfr+mas+crs+bcr) / baseline - landint",
    subtitle = "x = crop production change; y = crop-side agricultural EMIS improvement",
    x = "PROD(CRP) change [landint - baseline, 1000 t]",
    y = "Crop-side EMIS improvement [baseline - landint, MtCO2e]",
    caption = "Crop-side EMIS = rcc + sfr + mas + crs + bcr"
  ) +
  theme_geer
save_plot(p10, "scatter_crop_emis_vs_crop_prod_2050.png", width = 10, height = 7)

# Compare livestock production change and livestock-side agricultural emissions improvement.
p11 <- ggplot(scatter_2050, aes(x = prod_change_lsp, y = ag_emis_livestock_improvement)) +
  geom_hline(yintercept = 0, color = "grey70", linewidth = 0.4) +
  geom_vline(xintercept = 0, color = "grey70", linewidth = 0.4) +
  geom_point(alpha = 0.75, color = "#8C2D04", size = 2.1) +
  geom_text(
    data = top_ag_emis_2050,
    aes(label = region),
    size = 2.5,
    nudge_y = 0.02 * max(abs(scatter_2050$ag_emis_livestock_improvement), na.rm = TRUE),
    na.rm = TRUE
  ) +
  labs(
    title = "Scatter (2050): AGMIP / PROD(LSP) / landint - baseline vs AGMIP / EMIS(ent+mmg+mgr) / baseline - landint",
    subtitle = "x = livestock production change; y = livestock-side agricultural EMIS improvement",
    x = "PROD(LSP) change [landint - baseline, 1000 t]",
    y = "Livestock-side EMIS improvement [baseline - landint, MtCO2e]",
    caption = "Livestock-side EMIS = ent + mmg + mgr"
  ) +
  theme_geer
save_plot(p11, "scatter_liv_emis_vs_liv_prod_2050.png", width = 10, height = 7)

# Compare crop price change and calorie change as a direct food-security diagnostic.
p12 <- ggplot(scatter_2050, aes(x = price_change_crp, y = calorie_change)) +
  geom_hline(yintercept = 0, color = "grey70", linewidth = 0.4) +
  geom_vline(xintercept = 0, color = "grey70", linewidth = 0.4) +
  geom_point(alpha = 0.75, color = "#495057", size = 2.1) +
  geom_text(
    data = top_non_missing(scatter_2050, "price_change_crp", n = 10, desc = TRUE),
    aes(label = region),
    size = 2.4,
    nudge_y = 0.02 * max(abs(scatter_2050$calorie_change), na.rm = TRUE),
    na.rm = TRUE
  ) +
  labs(
    title = "Scatter (2050): AGMIP / XPRP(CRP) / landint - baseline vs AGMIP / CALI(TOT) / landint - baseline",
    subtitle = "x = crop producer price change; y = calorie intake change",
    x = "XPRP(CRP) change [landint - baseline, USD/t]",
    y = "CALI(TOT) change [landint - baseline, kcal/cap/d]",
    caption = "This plot is a direct side-effect diagnostic using original variables only"
  ) +
  theme_geer
save_plot(p12, "scatter_crop_price_vs_cali_2050.png", width = 10, height = 7)

# Compare top countries across market and food side-effect variables only.
p13a <- country_top_market_2050 %>%
  mutate(country_metric = reorder_within_local(country, -rank_order, metric, fun = max)) %>%
  ggplot(aes(x = value, y = country_metric, fill = metric)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ metric, scales = "free_x", ncol = 2) +
  scale_y_reordered_local() +
  labs(
    title = "Top countries by market and food side-effect variables (2050)",
    subtitle = "Top 8 countries within each metric; largest effect is placed at the top of each facet",
    x = "Variable value",
    y = "Country",
    caption = "Facets are split from land-change metrics to avoid over-compression"
  ) +
  theme_geer
save_plot(p13a, "top_countries_side_effects_market_2050.png", width = 14, height = 10)

# Compare top countries across land-change variables in a separate panel.
p13b <- country_top_land_2050 %>%
  mutate(country_metric = reorder_within_local(country, -rank_order, metric, fun = max)) %>%
  ggplot(aes(x = value, y = country_metric, fill = metric)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ metric, scales = "free_x", ncol = 2) +
  scale_y_reordered_local() +
  labs(
    title = "Top countries by land-change variables (2050)",
    subtitle = "Top 8 countries within each metric; largest effect is placed at the top of each facet",
    x = "Variable value",
    y = "Country",
    caption = "Land-change metrics are shown separately so the x scales remain readable"
  ) +
  theme_geer
save_plot(p13b, "top_countries_side_effects_land_2050.png", width = 14, height = 11)

# Compare land-use-side CO2 improvement and normalized forest change.
p14 <- ggplot(scatter_2050, aes(x = forest_area_change_pct_country, y = land_co2_improvement, size = abs(land_co2_improvement))) +
  geom_hline(yintercept = 0, color = "grey70", linewidth = 0.4) +
  geom_vline(xintercept = 0, color = "grey70", linewidth = 0.4) +
  geom_point(alpha = 0.75, color = "#0B7285") +
  geom_text(
    data = top_land_2050,
    aes(label = region),
    size = 2.4,
    nudge_y = 0.02 * max(abs(scatter_2050$land_co2_improvement), na.rm = TRUE),
    na.rm = TRUE
  ) +
  scale_size_continuous(range = c(1.5, 5)) +
  labs(
    title = "Scatter (2050): Mergedcomparison / Lan_Cov_Frs share change vs Mergedcomparison / Emi_CO2_AFO_Lan baseline - landint",
    subtitle = "x = 100 * [Lan_Cov_Frs_landint - Lan_Cov_Frs_baseline] / country area; y = land-use-side CO2 improvement",
    x = "Lan_Cov_Frs change / country area [%]",
    y = "Land-use-side CO2 improvement [baseline - landint, Mt CO2/yr]",
    caption = "Forest change is normalized by country area"
  ) +
  theme_geer
save_plot(p14, "scatter_land_co2_vs_forest_share_2050.png", width = 10, height = 7)

# Compare land-use-side CO2 improvement and normalized cropland change.
p15 <- ggplot(scatter_2050, aes(x = cropland_area_change_pct_country, y = land_co2_improvement, size = abs(land_co2_improvement))) +
  geom_hline(yintercept = 0, color = "grey70", linewidth = 0.4) +
  geom_vline(xintercept = 0, color = "grey70", linewidth = 0.4) +
  geom_point(alpha = 0.75, color = "#6A4C93") +
  geom_text(
    data = top_land_2050,
    aes(label = region),
    size = 2.4,
    nudge_y = 0.02 * max(abs(scatter_2050$land_co2_improvement), na.rm = TRUE),
    na.rm = TRUE
  ) +
  scale_size_continuous(range = c(1.5, 5)) +
  labs(
    title = "Scatter (2050): Mergedcomparison / Lan_Cov_Cro share change vs Mergedcomparison / Emi_CO2_AFO_Lan baseline - landint",
    subtitle = "x = 100 * [Lan_Cov_Cro_landint - Lan_Cov_Cro_baseline] / country area; y = land-use-side CO2 improvement",
    x = "Lan_Cov_Cro change / country area [%]",
    y = "Land-use-side CO2 improvement [baseline - landint, Mt CO2/yr]",
    caption = "Cropland change is normalized by country area"
  ) +
  theme_geer
save_plot(p15, "scatter_land_co2_vs_cropland_share_2050.png", width = 10, height = 7)

# Compare crop price change and normalized forest change.
p16 <- ggplot(scatter_2050, aes(x = forest_area_change_pct_country, y = price_change_crp, size = abs(land_co2_improvement))) +
  geom_hline(yintercept = 0, color = "grey70", linewidth = 0.4) +
  geom_vline(xintercept = 0, color = "grey70", linewidth = 0.4) +
  geom_point(alpha = 0.75, color = "#AD7F00") +
  scale_size_continuous(range = c(1.5, 5)) +
  labs(
    title = "Scatter (2050): Mergedcomparison / Lan_Cov_Frs share change vs AGMIP / XPRP(CRP) / landint - baseline",
    subtitle = "x = forest-area change share; y = crop producer price change",
    x = "Lan_Cov_Frs change / country area [%]",
    y = "XPRP(CRP) change [landint - baseline, USD/t]",
    caption = "This plot checks whether stronger forest expansion aligns with crop-price pressure"
  ) +
  theme_geer
save_plot(p16, "scatter_crop_price_vs_forest_share_2050.png", width = 10, height = 7)

# Show normalized AREA changes for the top land-improvement countries.
top_land_regions <- top_land_2050 %>%
  arrange(desc(land_co2_improvement)) %>%
  mutate(rank_order = row_number()) %>%
  select(region, country_name, rank_order)

area_heatmap <- area_wide %>%
  filter(year == 2050, region != "World") %>%
  semi_join(top_land_regions, by = "region") %>%
  left_join(top_land_regions, by = "region") %>%
  left_join(country_area_tbl, by = "region") %>%
  mutate(
    country = coalesce(country_name, region),
    change_pct_country = 100 * change_landint_vs_baseline / pmax(country_area_mha, 1e-12),
    land_code = recode(land_code, FRS = "AREA(FRS)", CL = "AREA(CL)", PAS = "AREA(PAS)", PRMFRS = "AREA(PRMFRS)", SECFRS = "AREA(SECFRS)"),
    country = forcats::fct_reorder(country, rank_order, .desc = TRUE)
  )

area_fill_lim <- sym_fill_limit(area_heatmap$change_pct_country)

p17 <- ggplot(area_heatmap, aes(x = land_code, y = country, fill = change_pct_country)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "#B02A37",
    mid = "white",
    high = "#0B7285",
    midpoint = 0,
    limits = c(-area_fill_lim, area_fill_lim),
    oob = squish,
    labels = label_number(accuracy = 0.01)
  ) +
  labs(
    title = "Heatmap (2050): Mergedcomparison / AREA / 100 * (landint - baseline) / country area",
    subtitle = "Countries = top land-use CO2 improvement countries; land_code = FRS, CL, PAS, PRMFRS, SECFRS",
    x = "AREA land_code",
    y = "Country",
    fill = "AREA change\n/ country area [%]",
    caption = "Color scale is clipped only for display; plotted values are normalized AREA changes"
  ) +
  theme_geer +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
save_plot(p17, "heatmap_area_change_share_2050.png", width = 11, height = 8)

# Show LandInt reduction portfolio for top agricultural-reduction countries.
top_ag_regions <- top_ag_2050 %>%
  arrange(desc(ag_total_reduction_landint)) %>%
  mutate(rank_order = row_number()) %>%
  select(region, country_name, rank_order)

portfolio_landint_df <- country_core %>%
  filter(year == 2050) %>%
  semi_join(top_ag_regions, by = "region") %>%
  left_join(top_ag_regions, by = "region") %>%
  transmute(
    country = coalesce(country_name.x, country_name.y, region),
    rank_order,
    `Emi_Red_equ_CH4_AFO_Agr` = ag_ch4_reduction_equ_landint,
    `Emi_Red_equ_N2O_AFO_Agr` = ag_n2o_reduction_equ_landint,
    `Emi_Red_equ_GHG_AFO_Lan` = land_total_reduction_landint
  ) %>%
  pivot_longer(cols = starts_with("Emi_Red_"), names_to = "component", values_to = "value") %>%
  mutate(country = forcats::fct_reorder(country, rank_order, .desc = TRUE))

# Show AFOLUB CO2e components as a stacked portfolio.
p18a <- ggplot(portfolio_landint_df, aes(x = country, y = value, fill = component)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = label_comma()) +
  labs(
    title = "Top reduction countries (2050): IAMC_templatePlus / CO2e reduction components in LandInt",
    subtitle = "Bars = Emi_Red_equ_CH4_AFO_Agr, Emi_Red_equ_N2O_AFO_Agr, Emi_Red_equ_GHG_AFO_Lan; ranking = Emi_Red_equ_GHG_AFO_Agr in LandInt",
    x = "Country",
    y = "Reduction component value [CO2e-based IAMC variable]",
    caption = "Stacked components use EQU variables so CH4 and N2O are comparable"
  ) +
  theme_geer
save_plot(p18a, "stacked_reduction_portfolio_landint_2050.png", width = 11, height = 8)

portfolio_no_df <- country_core %>%
  filter(year == 2050) %>%
  semi_join(top_ag_regions, by = "region") %>%
  left_join(top_ag_regions, by = "region") %>%
  transmute(
    country = coalesce(country_name.x, country_name.y, region),
    rank_order,
    `Emi_Red_equ_CH4_AFO_Agr` = ag_ch4_reduction_equ_no,
    `Emi_Red_equ_N2O_AFO_Agr` = ag_n2o_reduction_equ_no,
    `Emi_Red_equ_GHG_AFO_Lan` = land_total_reduction_no
  ) %>%
  pivot_longer(cols = starts_with("Emi_Red_"), names_to = "component", values_to = "value") %>%
  mutate(country = forcats::fct_reorder(country, rank_order, .desc = TRUE))

p18b <- ggplot(portfolio_no_df, aes(x = country, y = value, fill = component)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = label_comma()) +
  labs(
    title = "Top reduction countries (2050): IAMC_templatePlus / CO2e reduction components in No",
    subtitle = "Bars = Emi_Red_equ_CH4_AFO_Agr, Emi_Red_equ_N2O_AFO_Agr, Emi_Red_equ_GHG_AFO_Lan; country order follows LandInt reduction ranking",
    x = "Country",
    y = "Reduction component value [CO2e-based IAMC variable]",
    caption = "Scenario = SSP2_1100C_CACN_DAC_NoCC_No"
  ) +
  theme_geer
save_plot(p18b, "stacked_reduction_portfolio_no_2050.png", width = 11, height = 8)

# Show source-item agricultural-emission improvements from AGMIP.
emis_heatmap <- country_core %>%
  filter(year == 2050) %>%
  semi_join(top_ag_regions, by = "region") %>%
  left_join(top_ag_regions, by = "region") %>%
  transmute(
    country = coalesce(country_name.x, country_name.y, region),
    rank_order,
    ENT = -ag_emis_ent_change,
    MMG = -ag_emis_mmg_change,
    RCC = -ag_emis_rcc_change,
    SFR = -ag_emis_sfr_change,
    MAS = -ag_emis_mas_change,
    MGR = -ag_emis_mgr_change,
    CRS = -ag_emis_crs_change,
    BCR = -ag_emis_bcr_change
  ) %>%
  pivot_longer(cols = c(ENT, MMG, RCC, SFR, MAS, MGR, CRS, BCR), names_to = "source", values_to = "value") %>%
  mutate(country = forcats::fct_reorder(country, rank_order, .desc = TRUE))

emis_fill_lim <- sym_fill_limit(emis_heatmap$value)

p19 <- ggplot(emis_heatmap, aes(x = source, y = country, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "#B02A37",
    mid = "white",
    high = "#0B7285",
    midpoint = 0,
    limits = c(-emis_fill_lim, emis_fill_lim),
    oob = squish,
    labels = label_comma()
  ) +
  labs(
    title = "Heatmap (2050): AGMIP / EMIS source-item improvements for top agricultural-reduction countries",
    subtitle = "Value plotted = source-item baseline - landint for ENT, MMG, RCC, SFR, MAS, MGR, CRS, BCR",
    x = "AGMIP EMIS source item",
    y = "Country",
    fill = "Baseline -\nLandInt",
    caption = "This figure stays on original source items and simple differences only"
  ) +
  theme_geer
save_plot(p19, "heatmap_ag_emission_sources_2050.png", width = 12, height = 8)

# Show structure-change variables with per-facet ranking and free x scales.
p20 <- structure_panel_2050 %>%
  mutate(country_metric = reorder_within_local(country, -rank_order, metric)) %>%
  ggplot(aes(x = value, y = country_metric, fill = metric)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ metric, scales = "free_x", ncol = 2) +
  scale_y_reordered_local() +
  labs(
    title = "Top countries by structural changes under mitigation (2050)",
    subtitle = "Source = AGMIP / PROD, NETT, LAND / items = CRP, LSP, FOR, GRS; countries ranked by absolute change within each metric",
    x = "Change value",
    y = "Country",
    caption = "This panel is intended to inspect structure change rather than only exports/imports"
  ) +
  theme_geer
save_plot(p20, "top_countries_structure_change_2050.png", width = 14, height = 12)

# Show goods-level producer price changes for top agricultural-reduction countries.
goods_price_heatmap <- goods_detail %>%
  filter(year == 2050) %>%
  semi_join(top_ag_regions, by = "region") %>%
  left_join(top_ag_regions, by = "region") %>%
  transmute(
    country = coalesce(country_name.x, country_name.y, region),
    rank_order,
    WHT = WHT_price_change,
    RIC = RIC_price_change,
    CGR = CGR_price_change,
    RUM = RUM_price_change,
    NRM = NRM_price_change,
    DRY = DRY_price_change
  ) %>%
  pivot_longer(cols = c(WHT, RIC, CGR, RUM, NRM, DRY), names_to = "item", values_to = "value") %>%
  mutate(country = forcats::fct_reorder(country, rank_order, .desc = TRUE))

goods_price_lim <- sym_fill_limit(goods_price_heatmap$value)

p21 <- ggplot(goods_price_heatmap, aes(x = item, y = country, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "#2166AC",
    mid = "white",
    high = "#B2182B",
    midpoint = 0,
    limits = c(-goods_price_lim, goods_price_lim),
    oob = squish,
    labels = label_comma()
  ) +
  labs(
    title = "Heatmap (2050): AGMIP / XPRP(goods) / landint - baseline for top agricultural-reduction countries",
    subtitle = "Items = WHT, RIC, CGR, RUM, NRM, DRY; ranking = IAMC_templatePlus / Emi_Red_equ_GHG_AFO_Agr",
    x = "AGMIP item",
    y = "Country",
    fill = "Price change",
    caption = "These are goods-level producer price changes, not aggregated AGR/CRP/LSP"
  ) +
  theme_geer
save_plot(p21, "heatmap_goods_price_change_2050.png", width = 11, height = 8)

# Show goods-level production changes for the same countries.
goods_prod_heatmap <- goods_detail %>%
  filter(year == 2050) %>%
  semi_join(top_ag_regions, by = "region") %>%
  left_join(top_ag_regions, by = "region") %>%
  transmute(
    country = coalesce(country_name.x, country_name.y, region),
    rank_order,
    WHT = WHT_prod_change,
    RIC = RIC_prod_change,
    CGR = CGR_prod_change,
    RUM = RUM_prod_change,
    NRM = NRM_prod_change,
    DRY = DRY_prod_change
  ) %>%
  pivot_longer(cols = c(WHT, RIC, CGR, RUM, NRM, DRY), names_to = "item", values_to = "value") %>%
  mutate(country = forcats::fct_reorder(country, rank_order, .desc = TRUE))

goods_prod_lim <- sym_fill_limit(goods_prod_heatmap$value)

p22 <- ggplot(goods_prod_heatmap, aes(x = item, y = country, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "#2166AC",
    mid = "white",
    high = "#B2182B",
    midpoint = 0,
    limits = c(-goods_prod_lim, goods_prod_lim),
    oob = squish,
    labels = label_comma()
  ) +
  labs(
    title = "Heatmap (2050): AGMIP / PROD(goods) / landint - baseline for top agricultural-reduction countries",
    subtitle = "Items = WHT, RIC, CGR, RUM, NRM, DRY; ranking = IAMC_templatePlus / Emi_Red_equ_GHG_AFO_Agr",
    x = "AGMIP item",
    y = "Country",
    fill = "Production\nchange",
    caption = "These are goods-level production changes, not aggregated AGR/CRP/LSP"
  ) +
  theme_geer
save_plot(p22, "heatmap_goods_production_change_2050.png", width = 11, height = 8)

# Show goods-level net-trade changes as a structural-change diagnostic.
goods_nett_heatmap <- goods_detail %>%
  filter(year == 2050) %>%
  semi_join(top_ag_regions, by = "region") %>%
  left_join(top_ag_regions, by = "region") %>%
  transmute(
    country = coalesce(country_name.x, country_name.y, region),
    rank_order,
    WHT = WHT_nett_change,
    RIC = RIC_nett_change,
    CGR = CGR_nett_change,
    RUM = RUM_nett_change,
    NRM = NRM_nett_change,
    DRY = DRY_nett_change
  ) %>%
  pivot_longer(cols = c(WHT, RIC, CGR, RUM, NRM, DRY), names_to = "item", values_to = "value") %>%
  mutate(country = forcats::fct_reorder(country, rank_order, .desc = TRUE))

goods_nett_lim <- sym_fill_limit(goods_nett_heatmap$value)

p23 <- ggplot(goods_nett_heatmap, aes(x = item, y = country, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "#2166AC",
    mid = "white",
    high = "#B2182B",
    midpoint = 0,
    limits = c(-goods_nett_lim, goods_nett_lim),
    oob = squish,
    labels = label_comma()
  ) +
  labs(
    title = "Heatmap (2050): AGMIP / NETT(goods) / landint - baseline for top agricultural-reduction countries",
    subtitle = "Items = WHT, RIC, CGR, RUM, NRM, DRY; ranking = IAMC_templatePlus / Emi_Red_equ_GHG_AFO_Agr",
    x = "AGMIP item",
    y = "Country",
    fill = "Net trade\nchange",
    caption = "This figure is included as a structural-change diagnostic, not as a standalone trade result"
  ) +
  theme_geer
save_plot(p23, "heatmap_goods_nett_change_2050.png", width = 11, height = 8)

# Use AGMIP_all_scenario.gdx / AGMIP / EmisRed* as CO2e-based mitigation outputs.
# For each EmisRed metric, show the top countries ranked by LandInt value and place
# the corresponding price, production, and calorie changes beside them.
emisred_metrics <- tribble(
  ~metric_col, ~metric_label,
  "emisred_tot_landint", "AGMIP / EmisRed(TOT) / LandInt [MtCO2e]",
  "emisred_agr_landint", "AGMIP / EmisRed(AGR) / LandInt [MtCO2e]",
  "emisred_lsp_landint", "AGMIP / EmisRed(LSP) / LandInt [MtCO2e]",
  "emisred_ch4_tot_landint", "AGMIP / EmisRedCH4(TOT) / LandInt [MtCO2e]",
  "emisred_ch4_agr_landint", "AGMIP / EmisRedCH4(AGR) / LandInt [MtCO2e]",
  "emisred_ch4_lsp_landint", "AGMIP / EmisRedCH4(LSP) / LandInt [MtCO2e]",
  "emisred_n2o_tot_landint", "AGMIP / EmisRedN2O(TOT) / LandInt [MtCO2e]",
  "emisred_n2o_agr_landint", "AGMIP / EmisRedN2O(AGR) / LandInt [MtCO2e]"
)

emisred_top_panel <- purrr::map_dfr(seq_len(nrow(emisred_metrics)), function(i) {
  metric_col <- emisred_metrics$metric_col[[i]]
  metric_label <- emisred_metrics$metric_label[[i]]
  top_df <- top_non_missing(scatter_2050, metric_col, n = 12, desc = TRUE) %>%
    arrange(desc(.data[[metric_col]])) %>%
    mutate(rank_order = row_number())
  if (nrow(top_df) == 0) {
    return(tibble())
  }
  top_df %>%
    transmute(
      metric = metric_label,
      country = coalesce(country_name, region),
      rank_order,
      `Mitigation output` = .data[[metric_col]],
      `Food price change: AGMIP / XPRP(AGR) / landint - baseline [USD/t]` = price_change_agr,
      `Production change: AGMIP / PROD(AGR) / landint - baseline [1000 t]` = prod_change_agr,
      `Calorie change: AGMIP / CALI(TOT) / landint - baseline [kcal/cap/d]` = calorie_change
    ) %>%
    pivot_longer(
      cols = -c(metric, country, rank_order),
      names_to = "panel",
      values_to = "value"
    )
})

p24 <- emisred_top_panel %>%
  mutate(country_metric = reorder_within_local(country, -rank_order, metric, fun = max)) %>%
  ggplot(aes(x = value, y = country_metric, fill = panel)) +
  geom_col(show.legend = FALSE) +
  facet_grid(metric ~ panel, scales = "free_x", space = "free_x") +
  scale_y_reordered_local() +
  labs(
    title = "Top countries by AGMIP EmisRed metrics with side-by-side BaU changes (2050)",
    subtitle = "Rows = EmisRed / EmisRedCH4 / EmisRedN2O metrics from AGMIP LandInt; columns = mitigation output plus XPRP(AGR), PROD(AGR), CALI(TOT) changes relative to SSP2_BaU_NoCC_No",
    x = "Value",
    y = "Country",
    caption = "Countries are ranked within each EmisRed metric by the LandInt value of that metric"
  ) +
  theme_geer +
  theme(strip.text.y = element_text(angle = 0))
save_plot(p24, "top_countries_agmip_emisred_with_side_effects_2050.png", width = 16, height = 16)

# Focus on countries with large agricultural reduction in IAMC_templatePlus and
# inspect whether those same countries show food, price, production, and land-use
# side effects relative to BaU.
top_reduction_countries <- country_core %>%
  filter(year == 2050, is.finite(ag_total_reduction_landint), ag_total_reduction_landint > 0) %>%
  slice_max(order_by = ag_total_reduction_landint, n = 15, with_ties = FALSE) %>%
  arrange(desc(ag_total_reduction_landint)) %>%
  mutate(
    rank_order = row_number(),
    country = factor(coalesce(country_name, region), levels = rev(coalesce(country_name, region)))
  )

p25 <- ggplot(top_reduction_countries, aes(x = ag_total_reduction_landint, y = country)) +
  geom_col(fill = "#0B7285") +
  scale_x_continuous(labels = label_comma()) +
  labs(
    title = "Top agricultural reduction countries (2050): IAMC_templatePlus / Emi_Red_equ_GHG_AFO_Agr / LandInt",
    subtitle = "Countries are ordered by agricultural reduction in CO2e under SSP2_1100C_CACN_DAC_NoCC_LandInt_global1",
    x = "Emi_Red_equ_GHG_AFO_Agr [LandInt scenario]",
    y = "Country",
    caption = "These countries are used as the common country set in the side-effect figures below"
  ) +
  theme_geer
save_plot(p25, "top_reduction_countries_2050.png", width = 9.5, height = 7.5)

top_reduction_side_effects <- top_reduction_countries %>%
  transmute(
    country,
    rank_order,
    `CALI(TOT) change` = calorie_change,
    `XPRP(AGR) change` = price_change_agr,
    `PROD(AGR) change` = prod_change_agr,
    `PROD(CRP) change` = prod_change_crp,
    `PROD(LSP) change` = prod_change_lsp,
    `Lan_Cov_Frs change / country area` = forest_area_change_pct_country,
    `Lan_Cov_Cro change / country area` = cropland_area_change_pct_country,
    `Lan_Cov_Pst change / country area` = pasture_area_change_pct_country
  ) %>%
  pivot_longer(cols = -c(country, rank_order), names_to = "metric", values_to = "value")

p26 <- top_reduction_side_effects %>%
  filter(metric == "CALI(TOT) change") %>%
  ggplot(aes(x = value, y = country)) +
  geom_col(fill = "#7A3E9D") +
  scale_x_continuous(labels = label_comma()) +
  labs(
    title = "Calorie change for top agricultural reduction countries (2050)",
    subtitle = "Countries are ordered by IAMC_templatePlus / Emi_Red_equ_GHG_AFO_Agr in LandInt; bars show AGMIP / CALI(TOT) / landint - baseline",
    x = "CALI(TOT) change [landint - baseline, kcal/cap/d]",
    y = "Country",
    caption = "Negative values indicate lower calorie intake than BaU"
  ) +
  theme_geer
save_plot(p26, "top_reduction_countries_cali_change_2050.png", width = 9.5, height = 7.5)

price_side_effects <- top_reduction_countries %>%
  transmute(
    country,
    rank_order,
    `XPRP(AGR)` = price_change_agr,
    `XPRP(CRP)` = price_change_crp,
    `XPRP(LSP)` = price_change_lsp
  ) %>%
  pivot_longer(cols = -c(country, rank_order), names_to = "metric", values_to = "value")

p27 <- price_side_effects %>%
  ggplot(aes(x = value, y = country)) +
  geom_col(fill = "#C96F28") +
  facet_wrap(~ metric, scales = "free_x", ncol = 1) +
  scale_x_continuous(labels = label_comma()) +
  labs(
    title = "Price changes for top agricultural reduction countries (2050)",
    subtitle = "Countries are ordered by IAMC_templatePlus / Emi_Red_equ_GHG_AFO_Agr in LandInt; bars show AGMIP price changes relative to BaU",
    x = "Price change [landint - baseline, USD/t]",
    y = "Country",
    caption = "AGR, CRP, and LSP are shown separately so the scales stay readable"
  ) +
  theme_geer
save_plot(p27, "top_reduction_countries_price_changes_2050.png", width = 10, height = 10)

prod_side_effects <- top_reduction_countries %>%
  transmute(
    country,
    rank_order,
    `PROD(AGR)` = prod_change_agr,
    `PROD(CRP)` = prod_change_crp,
    `PROD(LSP)` = prod_change_lsp
  ) %>%
  pivot_longer(cols = -c(country, rank_order), names_to = "metric", values_to = "value")

p28 <- prod_side_effects %>%
  ggplot(aes(x = value, y = country)) +
  geom_col(fill = "#457B9D") +
  facet_wrap(~ metric, scales = "free_x", ncol = 1) +
  scale_x_continuous(labels = label_comma()) +
  labs(
    title = "Production changes for top agricultural reduction countries (2050)",
    subtitle = "Countries are ordered by IAMC_templatePlus / Emi_Red_equ_GHG_AFO_Agr in LandInt; bars show AGMIP production changes relative to BaU",
    x = "Production change [landint - baseline]",
    y = "Country",
    caption = "AGR, CRP, and LSP are shown separately"
  ) +
  theme_geer
save_plot(p28, "top_reduction_countries_production_changes_2050.png", width = 10, height = 10)

land_side_effects <- top_reduction_countries %>%
  transmute(
    country,
    rank_order,
    `Lan_Cov_Frs / country area` = forest_area_change_pct_country,
    `Lan_Cov_Cro / country area` = cropland_area_change_pct_country,
    `Lan_Cov_Pst / country area` = pasture_area_change_pct_country
  ) %>%
  pivot_longer(cols = -c(country, rank_order), names_to = "metric", values_to = "value")

p29 <- land_side_effects %>%
  ggplot(aes(x = value, y = country)) +
  geom_col(fill = "#2B8A3E") +
  facet_wrap(~ metric, scales = "free_x", ncol = 1) +
  scale_x_continuous(labels = label_number(accuracy = 0.01)) +
  labs(
    title = "Land-use changes for top agricultural reduction countries (2050)",
    subtitle = "Countries are ordered by IAMC_templatePlus / Emi_Red_equ_GHG_AFO_Agr in LandInt; bars show Mergedcomparison land-cover changes relative to BaU",
    x = "Land-cover change / country area [%]",
    y = "Country",
    caption = "Land-use changes are normalized by country area"
  ) +
  theme_geer
save_plot(p29, "top_reduction_countries_land_changes_2050.png", width = 10, height = 10)
