# ==== Libraries ====
library(tidyverse)
library(gdxrrw)
library(sf)
library(rnaturalearth)
library(ggh4x)

# ==== Settings and paths ====
gams_sys_dir <- Sys.getenv("GAMS_SYS_DIR", dirname(Sys.which("gams")))
igdx(gams_sys_dir)

script_arg <- commandArgs(FALSE)
script_file <- sub("^--file=", "", script_arg[grepl("^--file=", script_arg)])
script_dir <- if (length(script_file) > 0) {
  dirname(normalizePath(script_file[1], winslash = "/", mustWork = TRUE))
} else {
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}
project_dir <- normalizePath(file.path(script_dir, ".."), winslash = "/", mustWork = TRUE)
data_dir <- file.path(project_dir, "data")
output_dir <- file.path(project_dir, "output")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# Historical data
adjusted_gdx <- file.path(data_dir, "adjusted_data.gdx")
agmip_hist_gdx <- file.path(data_dir, "AGMIP_historical.gdx")

# Model output
agmip_gdx <- file.path(data_dir, "AGMIP_all_scenario.gdx")
global17_gdx <- file.path(data_dir, "all_scenario_global_17.gdx")
world_gdx <- file.path(data_dir, "all_scenario_World.gdx")

# Spatial data
basin_shp <- file.path(data_dir, "Basin_Boundary", "Basin_Boundary.shp")

scenario_names <- c(
  "SSP2_TRARM_BaU_NoCC" = "SRM_FIX",
  "SSP2_TRSCALE_BaU_NoCC" = "SRM_EXPO",
  "SSP2_TRSHIFT_BaU_NoCC" = "SRM_BIAS",
  "SSP2_TRPAR_BaU_NoCC" = "IWM"
)
scenario_reference <- "IWM"
scenario_colors <- c(
  "SRM_FIX" = "#E69F00",
  "SRM_EXPO" = "#CC79A7",
  "SRM_BIAS" = "#009E73",
  "IWM" = "#0072B2"
)
source_colors <- c(scenario_colors, "Historical" = "black")

agmip_regions <- c(
  "NAM" = "North America",
  "BRA" = "Brazil",
  "OSA" = "Other Central and South America",
  "EUR" = "Europe",
  "FSU" = "Former Soviet Union",
  "MEN" = "Middle East and North Africa",
  "SSA" = "Sub-Saharan Africa",
  "CHN" = "China",
  "IND" = "India",
  "OAS" = "Other Asia",
  "SEA" = "South-East Asia",
  "ANZ" = "Australia and New Zealand"
)
world_regions <- c("World" = "World", agmip_regions)

periods <- list(
  long = list(years = c(2000, 2100), breaks = seq(2000, 2100, 20), suffix = ""),
  near = list(years = c(2000, 2030), breaks = seq(2000, 2030, 10), suffix = "_near")
)
in_period <- function(df, pr) filter(df, between(Y, pr$years[1], pr$years[2]))

theme_1 <- theme_bw() +
  theme(
    text = element_text(size = 20),
    axis.text.x = element_text(size = 15),
    legend.position = "bottom",
    legend.text = element_text(size = 15),
    legend.title = element_blank(),
    axis.title.x = element_blank(),
    strip.background = element_blank()
  )
theme_x45 <- theme(axis.text.x = element_text(angle = 45, hjust = 1))

theme_map <- theme_void() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    text = element_text(size = 18),
    legend.position = "bottom",
    legend.key.width = unit(2.5, "cm"),
    legend.title = element_text(size = 15),
    strip.text = element_text(size = 18, margin = margin(b = 5))
  )

# Model lines are drawn first. Historical lines and points are drawn on top of them.
ts_layers <- function(data_model, data_hist = NULL, near = FALSE) {
  list(
    geom_line(data = data_model, aes(x = Y, y = value, color = source), linewidth = 1),
    if (near) geom_point(data = data_model, aes(x = Y, y = value, color = source), size = 2),
    if (!is.null(data_hist)) geom_line(data = data_hist, aes(x = Y, y = value, color = source), linewidth = 0.6),
    if (!is.null(data_hist)) geom_point(data = data_hist, aes(x = Y, y = value, color = source), size = 1.5),
    scale_color_manual(values = source_colors, breaks = names(source_colors))
  )
}

fig_dir <- function(name) {
  d <- file.path(output_dir, name)
  dir.create(d, showWarnings = FALSE)
  d
}

# ==== Data ====
# GDX labels are case-insensitive, so the same AgMIP label is spelled "wht" or "WHT" depending
# on the file. VAR and ITEM are upper-cased. If var is given, only those variables are kept.
read_gdx <- function(gdx, sym, cols, var = NULL) {
  df <- rgdx.param(gdx, sym) %>%
    setNames(c(cols, "value"))
  if (!is.null(var)) df <- filter(df, toupper(VAR) %in% var)
  df %>%
    mutate(
      across(all_of(cols), as.character),
      across(any_of(c("VAR", "ITEM")), toupper),
      Y = as.numeric(Y)
    )
}

# Keeps the compared scenarios and replaces SCENARIO by the scenario name in source.
keep_scenarios <- function(df) {
  df %>%
    filter(SCENARIO %in% names(scenario_names)) %>%
    mutate(source = factor(scenario_names[SCENARIO], levels = scenario_names)) %>%
    select(!SCENARIO)
}

agmip_raw <- read_gdx(agmip_gdx, "AGMIP_agg", c("SCENARIO", "region", "VAR", "ITEM", "UNIT", "Y")) %>%
  keep_scenarios()

agmip_hist_raw <- read_gdx(agmip_hist_gdx, "AGMIP_agg_hist", c("region", "VAR", "ITEM", "UNIT", "Y")) %>%
  mutate(source = "Historical")

# The country-level AgMIP tables are large, so only net trade is kept.
agmip_cty_nett_raw <- read_gdx(agmip_gdx, "AGMIP", c("SCENARIO", "cty", "VAR", "ITEM", "UNIT", "Y"), var = "NETT") %>%
  keep_scenarios() %>%
  select(!VAR)

agmip_cty_nett_hist_raw <- read_gdx(agmip_hist_gdx, "AGMIP_hist", c("cty", "VAR", "ITEM", "UNIT", "Y"), var = "NETT") %>%
  mutate(source = "Historical") %>%
  select(!VAR)

agmip_cty_region <- rgdx.set(agmip_gdx, "cty_reg_map") %>%
  setNames(c("cty", "region")) %>%
  mutate(across(everything(), as.character)) %>%
  filter(region %in% names(agmip_regions))

land_basin_raw <- map_dfr(
  c("Cropland" = "qlCro_load", "Pasture" = "qlPas_load", "Forest" = "qlFrs_load",
    "Other natural land" = "qlNat_load", "Total" = "qlTot_load"),
  function(sym) read_gdx(world_gdx, sym, c("SCENARIO", "bas", "cty", "Y")),
  .id = "type"
) %>%
  keep_scenarios()

pw_raw <- read_gdx(global17_gdx, "pwAgg_load", c("SCENARIO", "c", "Y")) %>%
  keep_scenarios()

pw_hist_raw <- read_gdx(adjusted_gdx, "pw", c("c", "Y")) %>%
  mutate(source = "Historical")

sf_use_s2(FALSE)
basin_sf <- st_read(basin_shp, quiet = TRUE) %>%
  select(cty = ISO_A3, bas = Basin)
country_sf <- basin_sf %>%
  group_by(cty) %>%
  summarise(.groups = "drop")
world_bg_sf <- ne_countries(scale = 50, returnclass = "sf") %>%
  filter(continent != "Antarctica") %>%
  select(geometry)

# ==== Figures ====

# ---- Common: AgMIP items ----
agmip_items <- c(
  "RIC" = "Rice",
  "WHT" = "Wheat",
  "CGR" = "Coarse grains",
  "OSD" = "Oilseeds",
  "SGC" = "Sugar crops",
  "HORT" = "Fruits, veg. and nuts",
  "RTP" = "Roots and pulses",
  "RUM" = "Ruminant meat",
  "NRM" = "Non-ruminant meat",
  "DRY" = "Dairy"
)
# CRP and LSP sum goods of different water content, so they are used in dry-matter weight.
total_items <- c(
  "CRP" = "Crops",
  "LSP" = "Livestock"
)
vfn_sub_items <- c("VFN|VEG", "VFN|FRU", "VFN|NUT")

# HORT is the sum of vfn_sub_items. RTP is VFN minus HORT, which equals ALPHA str + pls.
# The input must be an additive quantity. A missing record is zero.
add_hort_rtp <- function(df) {
  keys <- setdiff(names(df), c("ITEM", "value"))
  hort <- df %>%
    filter(ITEM %in% vfn_sub_items) %>%
    group_by(across(all_of(keys))) %>%
    summarise(value = sum(value), .groups = "drop") %>%
    mutate(ITEM = "HORT")
  rtp <- df %>%
    filter(ITEM == "VFN") %>%
    left_join(select(hort, !ITEM), by = keys, suffix = c("", "_hort")) %>%
    mutate(value = value - coalesce(value_hort, 0), ITEM = "RTP") %>%
    select(!value_hort)
  bind_rows(df, hort, rtp)
}

# ---- Common: self-sufficiency rate ----
calc_ssr <- function(df, regions, items, unit) {
  df %>%
    filter(region %in% names(regions), VAR %in% c("PROD", "CONS"), UNIT == unit) %>%
    select(!UNIT) %>%
    add_hort_rtp() %>%
    filter(ITEM %in% names(items)) %>%
    pivot_wider(names_from = VAR, values_from = value) %>%
    filter(CONS > 0, !is.na(PROD)) %>%
    mutate(
      value = PROD / CONS * 100,
      region = factor(regions[region], levels = regions),
      item = factor(items[ITEM], levels = items)
    )
}

# ---- Fig 1: Self-sufficiency rate by item, single scenario ----
fig1_scenario <- "IWM"
fig1_dir <- fig_dir("fig01_ssr")

fig1_model <- agmip_raw %>%
  filter(source == fig1_scenario) %>%
  calc_ssr(agmip_regions, agmip_items, "1000 t")
fig1_hist <- calc_ssr(agmip_hist_raw, agmip_regions, agmip_items, "1000 t")

for (r in names(agmip_regions)) {
  g <- ggplot() +
    ts_layers(filter(fig1_model, region == agmip_regions[[r]]), filter(fig1_hist, region == agmip_regions[[r]])) +
    facet_wrap(~item, nrow = 2, scales = "free_y") +
    scale_x_continuous(breaks = seq(2000, 2100, 20)) +
    labs(y = "Self-sufficiency rate [%]") +
    theme_1 +
    theme_x45
  ggsave(file.path(fig1_dir, paste0("fig01_ssr_", r, ".png")), g, width = 15, height = 7.5, dpi = 300)
}

# ---- Fig 2: Self-sufficiency rate of crops and livestock by region ----
fig2_r5_regions <- c(
  "R5OECD90+EU" = "OECD90 and EU", "R5REF" = "Reforming economies", "R5ASIA" = "Asia",
  "R5MAF" = "Middle East and Africa", "R5LAM" = "Latin America"
)
fig2_dir <- fig_dir("fig02_ssr_total")

fig2_model <- calc_ssr(agmip_raw, agmip_regions, total_items, "1000 t DM")
fig2_hist <- calc_ssr(agmip_hist_raw, agmip_regions, total_items, "1000 t DM")
fig2_r5_model <- calc_ssr(agmip_raw, fig2_r5_regions, total_items, "1000 t DM")
fig2_r5_hist <- calc_ssr(agmip_hist_raw, fig2_r5_regions, total_items, "1000 t DM")

for (p in names(periods)) {
  pr <- periods[[p]]
  for (it in names(total_items)) {
    g <- ggplot() +
      ts_layers(
        in_period(filter(fig2_model, ITEM == it), pr),
        in_period(filter(fig2_hist, ITEM == it), pr),
        near = p == "near"
      ) +
      facet_wrap(~region, ncol = 4, scales = "free_y", labeller = label_wrap_gen(width = 22)) +
      scale_x_continuous(breaks = pr$breaks) +
      labs(y = "Self-sufficiency rate [%]") +
      theme_1 +
      theme_x45
    ggsave(file.path(fig2_dir, paste0("fig02_ssr_", it, pr$suffix, ".png")), g, width = 14, height = 11, dpi = 300)
  }

  # R5 regions in columns, crops and livestock in rows. Each panel has its own y range.
  g <- ggplot() +
    ts_layers(in_period(fig2_r5_model, pr), in_period(fig2_r5_hist, pr), near = p == "near") +
    facet_grid2(item ~ region, scales = "free_y", independent = "y",
      labeller = labeller(region = label_wrap_gen(width = 16))) +
    scale_x_continuous(breaks = pr$breaks) +
    labs(y = "Self-sufficiency rate [%]") +
    theme_1 +
    theme_x45 +
    theme(
      panel.spacing.y = unit(2.5, "lines"),
      strip.text.y = element_text(size = 20, face = "bold", margin = margin(l = 8))
    )
  ggsave(file.path(fig2_dir, paste0("fig02_ssr_R5", pr$suffix, ".png")), g, width = 18, height = 9.5, dpi = 300)
}

# ---- Common: country net trade ----
# AgMIP items include processed goods as primary-product equivalents.
# Items in agmip_items use fresh weight. Items in total_items use dry-matter weight.
# World trade is the sum of positive country NETT. A missing NETT record is zero trade.
calc_cty_net <- function(df) {
  df %>%
    add_hort_rtp() %>%
    filter(
      (UNIT == "1000 t" & ITEM %in% names(agmip_items)) |
        (UNIT == "1000 t DM" & ITEM %in% names(total_items))
    ) %>%
    select(!UNIT)
}

cty_net_model <- calc_cty_net(agmip_cty_nett_raw)
cty_net_hist <- agmip_cty_nett_hist_raw %>%
  filter(Y >= 2000) %>%
  calc_cty_net()

# ---- Fig 3: World trade volume by item ----
fig3_groups <- list(
  item = list(labels = agmip_items, name = "fig03_world_trade", nrow = 2,
    y = "Net trade [Mt]", width = 15, height = 7.5),
  total = list(labels = total_items, name = "fig03_world_trade_total", nrow = 1,
    y = "Net trade, dry matter [Mt DM]", width = 9, height = 6)
)
fig3_dir <- fig_dir("fig03_world_trade")

calc_world_net <- function(df) {
  df %>%
    group_by(source, ITEM, Y) %>%
    summarise(value = sum(pmax(value, 0)) / 1000, .groups = "drop")
}
fig3_model <- calc_world_net(cty_net_model)
fig3_hist <- calc_world_net(cty_net_hist)

for (p in names(periods)) {
  pr <- periods[[p]]
  for (gr in names(fig3_groups)) {
    lab <- fig3_groups[[gr]]$labels
    data_model <- fig3_model %>%
      filter(ITEM %in% names(lab)) %>%
      in_period(pr) %>%
      mutate(item = factor(lab[ITEM], levels = lab))
    data_hist <- fig3_hist %>%
      filter(ITEM %in% names(lab)) %>%
      in_period(pr) %>%
      mutate(item = factor(lab[ITEM], levels = lab))
    g <- ggplot() +
      ts_layers(data_model, data_hist, near = p == "near") +
      facet_wrap(~item, nrow = fig3_groups[[gr]]$nrow, scales = "free_y") +
      scale_x_continuous(breaks = pr$breaks) +
      labs(y = fig3_groups[[gr]]$y) +
      theme_1 +
      theme_x45
    ggsave(
      file.path(fig3_dir, paste0(fig3_groups[[gr]]$name, pr$suffix, ".png")),
      g, width = fig3_groups[[gr]]$width, height = fig3_groups[[gr]]$height, dpi = 300
    )
  }
}

# ---- Fig 4: Share of world exports by region ----
# A region's share is the sum of positive NETT of its countries divided by the world sum of
# positive NETT.
fig4_years <- c(2015, 2100)
fig4_items <- c(agmip_items, total_items)
fig4_dir <- fig_dir("fig04_export_share")

fig4_data <- cty_net_model %>%
  filter(between(Y, fig4_years[1], fig4_years[2])) %>%
  inner_join(agmip_cty_region, by = "cty") %>%
  group_by(source, ITEM, region, Y) %>%
  summarise(value = sum(pmax(value, 0)), .groups = "drop") %>%
  group_by(source, ITEM, Y) %>%
  mutate(share = value / sum(value) * 100) %>%
  ungroup() %>%
  mutate(region = factor(agmip_regions[region], levels = agmip_regions))

for (it in names(fig4_items)) {
  g <- ggplot(filter(fig4_data, ITEM == it), aes(x = Y, y = share, fill = region)) +
    geom_area(color = "white", linewidth = 0.2) +
    facet_wrap(~source, nrow = 1) +
    scale_fill_brewer(palette = "Paired") +
    scale_x_continuous(breaks = seq(2020, 2100, 20), expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    labs(y = paste0(fig4_items[[it]], ", export share [%]")) +
    guides(fill = guide_legend(nrow = 3)) +
    theme_1 +
    theme_x45 +
    theme(panel.spacing = unit(1, "lines"))
  ggsave(file.path(fig4_dir, paste0("fig04_export_share_", it, ".png")), g, width = 15, height = 7.5, dpi = 300)
}

# ---- Fig 5: World price by commodity ----
fig5_items <- c(
  "wht" = "Wheat", "rce" = "Rice", "mze" = "Maize", "crl" = "Other cereals",
  "ocr" = "Oil crops", "vol" = "Vegetable oils", "swt" = "Sweeteners", "pls" = "Pulses",
  "str" = "Starchy roots", "vgt" = "Vegetables", "frt" = "Fruits", "dai" = "Dairy products",
  "cmt" = "Cattle meat", "rmt" = "Small ruminant meat", "pmt" = "Poultry meat", "omt" = "Other meat"
)
fig5_dir <- fig_dir("fig05_world_price")

fig5_model <- pw_raw %>%
  filter(c %in% names(fig5_items)) %>%
  mutate(item = factor(fig5_items[c], levels = fig5_items))
fig5_hist <- pw_hist_raw %>%
  filter(c %in% names(fig5_items)) %>%
  mutate(item = factor(fig5_items[c], levels = fig5_items))

for (p in names(periods)) {
  pr <- periods[[p]]
  g <- ggplot() +
    ts_layers(in_period(fig5_model, pr), in_period(fig5_hist, pr), near = p == "near") +
    facet_wrap(~item, ncol = 4, scales = "free_y") +
    scale_x_continuous(breaks = pr$breaks) +
    labs(y = "World price [USD/t]") +
    theme_1 +
    theme_x45
  ggsave(file.path(fig5_dir, paste0("fig05_world_price", pr$suffix, ".png")), g, width = 15, height = 12, dpi = 300)
}

# ---- Common: producer price and production ----
# The producer price is production value divided by production quantity. This equals AgMIP XPRP
# for single items and gives quantity-weighted prices for HORT, RTP, CRP and LSP.
price_prod <- agmip_raw %>%
  filter(region %in% names(world_regions), VAR == "PROD", UNIT %in% c("1000 t", "1000 t DM", "USD")) %>%
  select(!VAR) %>%
  add_hort_rtp() %>%
  filter(
    (UNIT != "1000 t DM" & ITEM %in% names(agmip_items)) |
      (UNIT != "1000 t" & ITEM %in% names(total_items))
  ) %>%
  mutate(UNIT = if_else(UNIT == "USD", "usd", "qty")) %>%
  pivot_wider(names_from = UNIT, values_from = value) %>%
  transmute(
    source, region, ITEM, Y,
    price = if_else(qty > 0, usd / qty / 1000, NA_real_),
    production = qty / 1000
  ) %>%
  pivot_longer(c(price, production), names_to = "var") %>%
  filter(!is.na(value)) %>%
  mutate(region = factor(world_regions[region], levels = world_regions))

# ---- Fig 6: Producer price and production by region ----
fig6_items <- c(agmip_items, total_items)
fig6_vars <- list(
  price = list(y = "Producer price [USD/t]", y_dm = "Producer price [USD/t DM]"),
  production = list(y = "Production [Mt]", y_dm = "Production [Mt DM]")
)
fig6_dir <- fig_dir("fig06_price_production")

for (p in names(periods)) {
  pr <- periods[[p]]
  for (v in names(fig6_vars)) {
    for (it in names(fig6_items)) {
      y_lab <- if (it %in% names(total_items)) fig6_vars[[v]]$y_dm else fig6_vars[[v]]$y
      g <- ggplot() +
        ts_layers(in_period(filter(price_prod, var == v, ITEM == it), pr), near = p == "near") +
        facet_wrap(~region, ncol = 5, scales = "free_y", labeller = label_wrap_gen(width = 22)) +
        scale_x_continuous(breaks = pr$breaks) +
        labs(y = y_lab) +
        theme_1 +
        theme_x45
      ggsave(
        file.path(fig6_dir, paste0("fig06_", v, "_", it, pr$suffix, ".png")),
        g, width = 16, height = 11, dpi = 300
      )
    }
  }
}

# ---- Fig 7: Producer price change of crops and livestock by region ----
# 2015 is the same in all scenarios.
fig7_year <- 2100
fig7_dir <- fig_dir("fig07_price_change")

fig7_data <- price_prod %>%
  filter(var == "price", ITEM %in% names(total_items), Y %in% c(2015, fig7_year)) %>%
  group_by(source, region, ITEM) %>%
  summarise(change = (value[Y == fig7_year] / value[Y == 2015] - 1) * 100, .groups = "drop") %>%
  mutate(item = factor(total_items[ITEM], levels = total_items))

g <- ggplot(fig7_data, aes(x = item, y = change, fill = source)) +
  geom_hline(yintercept = 0, color = "grey40") +
  geom_col(position = position_dodge(width = 0.8), width = 0.75) +
  facet_wrap(~region, ncol = 5, labeller = label_wrap_gen(width = 22)) +
  scale_fill_manual(values = scenario_colors) +
  labs(y = paste0("Producer price change 2015-", fig7_year, " [%]")) +
  theme_1
ggsave(file.path(fig7_dir, paste0("fig07_price_change_", fig7_year, ".png")), g, width = 16, height = 11, dpi = 300)

# ---- Fig 8: Production change by region and item (stacked bar, dry matter) ----
# Increases are stacked above zero and decreases below zero. The point is the sum over items.
fig8_year <- 2100
fig8_reference <- "SRM_FIX"
fig8_groups <- list(
  crop = c("WHT", "RIC", "CGR", "OSD", "SGC", "HORT", "RTP", "OCR"),
  livestock = c("RUM", "NRM", "DRY")
)
fig8_items <- c(agmip_items, "OCR" = "Other crops")[unlist(fig8_groups)]
fig8_colors <- c(
  "Wheat" = "#E69F00", "Rice" = "#F0E442", "Coarse grains" = "#8C510A", "Oilseeds" = "#BF812D",
  "Sugar crops" = "#DFC27D", "Fruits, veg. and nuts" = "#35978F", "Roots and pulses" = "#80CDC1",
  "Other crops" = "#BDBDBD", "Ruminant meat" = "#B2182B", "Non-ruminant meat" = "#F4A582",
  "Dairy" = "#4393C3"
)
fig8_dir <- fig_dir("fig08_production_change")

fig8_prod <- agmip_raw %>%
  filter(
    Y %in% c(2015, fig8_year), VAR == "PROD", UNIT == "1000 t DM",
    region %in% names(agmip_regions)
  ) %>%
  select(source, region, ITEM, Y, value) %>%
  add_hort_rtp() %>%
  filter(ITEM %in% names(fig8_items)) %>%
  complete(nesting(source, region), ITEM, Y, fill = list(value = 0)) %>%
  mutate(value = value / 1000)

fig8_change <- list(
  base2015 = fig8_prod %>%
    group_by(source, region, ITEM) %>%
    summarise(change = value[Y == fig8_year] - value[Y == 2015], .groups = "drop"),
  reference = fig8_prod %>%
    filter(Y == fig8_year) %>%
    group_by(region, ITEM) %>%
    mutate(change = value - value[source == fig8_reference]) %>%
    ungroup() %>%
    filter(source != fig8_reference)
)
fig8_settings <- list(
  base2015 = list(
    x = paste0("Production change 2015-", fig8_year, " [Mt DM]"),
    name = paste0("fig08_production_change_2015_", fig8_year)
  ),
  reference = list(
    x = paste0("Production in ", fig8_year, ", difference from ", fig8_reference, " [Mt DM]"),
    name = paste0("fig08_production_diff_", fig8_reference, "_", fig8_year)
  )
)

for (k in names(fig8_change)) {
  for (gr in names(fig8_groups)) {
    d <- fig8_change[[k]] %>%
      filter(ITEM %in% fig8_groups[[gr]]) %>%
      mutate(
        region = factor(agmip_regions[region], levels = rev(agmip_regions)),
        item = factor(fig8_items[ITEM], levels = fig8_items)
      )
    d_net <- d %>%
      group_by(source, region) %>%
      summarise(change = sum(change), .groups = "drop")
    g <- ggplot(d, aes(x = change, y = region)) +
      geom_col(aes(fill = item), width = 0.75) +
      geom_vline(xintercept = 0, color = "grey30") +
      geom_point(data = d_net, aes(shape = "Total"), size = 3) +
      facet_wrap(~source, nrow = 1) +
      scale_fill_manual(values = fig8_colors) +
      scale_shape_manual(values = c("Total" = 18)) +
      labs(x = fig8_settings[[k]]$x) +
      guides(fill = guide_legend(nrow = 2, order = 1), shape = guide_legend(order = 2)) +
      theme_1 +
      theme(
        axis.title.x = element_text(), axis.title.y = element_blank(),
        panel.spacing = unit(1.5, "lines"), plot.margin = margin(5.5, 20, 5.5, 5.5)
      )
    ggsave(file.path(fig8_dir, paste0(fig8_settings[[k]]$name, "_", gr, ".png")), g, width = 18, height = 8, dpi = 300)
  }
}

# ---- Common: land use ----
land_items <- c(
  "CRP" = "Cropland",
  "GRS" = "Pasture",
  "FOR" = "Forest",
  "ONV" = "Other natural land"
)

land_agmip <- agmip_raw %>%
  filter(VAR == "LAND", UNIT == "1000 ha", ITEM %in% names(land_items)) %>%
  mutate(
    type = factor(land_items[ITEM], levels = land_items),
    value = value / 1000
  )

# ---- Fig 9: Land-use change from 2015, World and regions (stacked bar) ----
# Positive and negative changes are stacked separately above and below zero.
fig9_years <- seq(2020, 2100, 10)
fig9_colors <- c(
  "Cropland" = "#E6AB02",
  "Pasture" = "#A6D96A",
  "Forest" = "#1B7837",
  "Other natural land" = "#BF9E77"
)
fig9_dir <- fig_dir("fig09_land_stack")

fig9_data <- land_agmip %>%
  filter(region %in% names(world_regions)) %>%
  group_by(source, region, type) %>%
  mutate(change = value - value[Y == 2015]) %>%
  ungroup() %>%
  filter(Y %in% fig9_years)

for (r in names(world_regions)) {
  g <- ggplot(filter(fig9_data, region == r), aes(x = Y, y = change, fill = type)) +
    geom_col(width = 8) +
    geom_hline(yintercept = 0, color = "grey30") +
    facet_wrap(~source, nrow = 1) +
    scale_fill_manual(values = fig9_colors) +
    scale_x_continuous(breaks = seq(2020, 2100, 20)) +
    labs(y = "Land-use change from 2015 [Mha]") +
    theme_1 +
    theme_x45
  ggsave(file.path(fig9_dir, paste0("fig09_land_", r, ".png")), g, width = 15, height = 6.5, dpi = 300)
}

# ---- Fig 10: Land-use change from 2015 by region (bar) ----
fig10_years <- c(2050, 2100)
fig10_dir <- fig_dir("fig10_land_region")

fig10_data <- land_agmip %>%
  filter(region %in% names(world_regions)) %>%
  group_by(source, region, type) %>%
  mutate(change = value - value[Y == 2015]) %>%
  ungroup() %>%
  mutate(region = factor(world_regions[region], levels = world_regions))

for (yr in fig10_years) {
  g <- ggplot(filter(fig10_data, Y == yr), aes(x = type, y = change, fill = source)) +
    geom_hline(yintercept = 0, color = "grey40") +
    geom_col(position = position_dodge(width = 0.8), width = 0.75) +
    facet_wrap(~region, ncol = 5, scales = "free_y", labeller = label_wrap_gen(width = 22)) +
    scale_fill_manual(values = scenario_colors) +
    labs(y = paste0("Land-use change 2015-", yr, " [Mha]")) +
    theme_1 +
    theme_x45
  ggsave(file.path(fig10_dir, paste0("fig10_land_region_", yr, ".png")), g, width = 22, height = 14, dpi = 300)
}

# ---- Fig 11: Land-use change by basin (maps) ----
# Changes are shown as a share of the basin-country land area in 2015 of the reference scenario.
# Color limits are symmetric at the 98th percentile of |x|. Values beyond the limit are drawn at
# the limit.
fig11_years <- c(2050, 2100)
fig11_types <- c("Cropland", "Pasture", "Forest", "Other natural land")
fig11_crs <- "+proj=robin"
fig11_dir <- fig_dir("fig11_land_basin")

sym_limit <- function(x, q = 0.98) {
  lim <- quantile(abs(x), q, na.rm = TRUE)
  if (lim == 0) 1 else signif(lim, 2)
}
scale_fill_div <- function(name, lim) {
  scale_fill_gradient2(
    name = name, low = "#2166AC", mid = "grey95", high = "#B2182B", midpoint = 0,
    limits = c(-lim, lim), oob = scales::squish, na.value = "grey80"
  )
}

# A basin-country cell without a record has zero area of that type.
fig11_land <- land_basin_raw %>%
  filter(Y %in% c(2015, fig11_years)) %>%
  complete(nesting(bas, cty), source, Y, type, fill = list(value = 0))

fig11_total <- fig11_land %>%
  filter(type == "Total", Y == 2015, source == scenario_reference) %>%
  select(bas, cty, total = value)

fig11_change <- fig11_land %>%
  filter(type %in% fig11_types) %>%
  group_by(source, bas, cty, type) %>%
  mutate(change = value - value[Y == 2015]) %>%
  ungroup() %>%
  filter(Y %in% fig11_years) %>%
  left_join(fig11_total, by = c("bas", "cty")) %>%
  mutate(change_pp = if_else(total > 0, change / total * 100, NA_real_))

fig11_diff <- fig11_change %>%
  filter(source != scenario_reference) %>%
  left_join(
    fig11_change %>%
      filter(source == scenario_reference) %>%
      select(bas, cty, type, Y, ref_pp = change_pp),
    by = c("bas", "cty", "type", "Y")
  ) %>%
  mutate(
    diff_pp = change_pp - ref_pp,
    comparison = factor(paste0(source, " - ", scenario_reference),
      levels = paste0(setdiff(scenario_names, scenario_reference), " - ", scenario_reference))
  )

for (tp in fig11_types) {
  for (yr in fig11_years) {
    d_change <- basin_sf %>%
      inner_join(filter(fig11_change, type == tp, Y == yr), by = c("bas", "cty"))
    g <- ggplot(d_change) +
      geom_sf(data = world_bg_sf, fill = "grey80", color = NA) +
      geom_sf(aes(fill = change_pp), color = NA) +
      geom_sf(data = country_sf, fill = NA, color = "grey50", linewidth = 0.1) +
      coord_sf(crs = fig11_crs) +
      facet_wrap(~source, ncol = 2) +
      scale_fill_div(paste0(tp, " change 2015-", yr, "\n[% of land area]"), sym_limit(d_change$change_pp)) +
      theme_map
    ggsave(
      file.path(fig11_dir, paste0("fig11_", gsub(" ", "_", tolower(tp)), "_change_", yr, ".png")),
      g, width = 15, height = 9.5, dpi = 300
    )

    d_diff <- basin_sf %>%
      inner_join(filter(fig11_diff, type == tp, Y == yr), by = c("bas", "cty"))
    g <- ggplot(d_diff) +
      geom_sf(data = world_bg_sf, fill = "grey80", color = NA) +
      geom_sf(aes(fill = diff_pp), color = NA) +
      geom_sf(data = country_sf, fill = NA, color = "grey50", linewidth = 0.1) +
      coord_sf(crs = fig11_crs) +
      facet_wrap(~comparison, ncol = 3) +
      scale_fill_div(
        paste0(tp, " in ", yr, ", difference from ", scenario_reference, "\n[% of land area]"),
        sym_limit(d_diff$diff_pp)
      ) +
      theme_map
    ggsave(
      file.path(fig11_dir, paste0("fig11_", gsub(" ", "_", tolower(tp)), "_diff_", yr, ".png")),
      g, width = 21, height = 5.5, dpi = 300
    )
  }
}

# ---- Fig 12: Calorie intake per capita by region (bar) ----
fig12_years <- c(2050, 2100)
fig12_dir <- fig_dir("fig12_calorie_intake")

fig12_data <- agmip_raw %>%
  filter(
    Y %in% fig12_years, VAR == "CALI", UNIT == "kcal/cap/d", ITEM == "TOT",
    region %in% names(agmip_regions)
  ) %>%
  mutate(
    region = factor(agmip_regions[region], levels = agmip_regions),
    year = factor(Y)
  )

g <- ggplot(fig12_data, aes(x = year, y = value, fill = source)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.75) +
  facet_wrap(~region, ncol = 4, labeller = label_wrap_gen(width = 22)) +
  scale_fill_manual(values = scenario_colors) +
  labs(y = "Calorie intake [kcal/cap/day]") +
  theme_1
ggsave(file.path(fig12_dir, "fig12_calorie_intake.png"), g, width = 14, height = 10, dpi = 300)
