library(dplyr)
library(ggplot2)
library(sf)
library(tidyr)
library(patchwork)
library(gdxrrw)
library(ncdf4)
library(rnaturalearth)
library(rnaturalearthdata)
library(scales)
library(purrr)
library(stringr)

# ---------------- theme ----------------
theme_1 <- theme_bw() +
  theme(
    panel.grid.major = element_line(color = "grey80", linewidth = 0.5),
    panel.grid.minor = element_line(color = "grey90", linewidth = 0.25),
    text = element_text(size = 24),
    axis.text.x = element_text(size = 18, angle = 35, hjust = 1),
    axis.text.y = element_text(size = 20),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.text = element_text(size = 22),
    axis.title.x = element_blank(),
    legend.title = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(size = 22),
    plot.margin = margin(6, 6, 6, 6)
  )

theme_map_min <- function() {
  theme_bw(base_size = 20) +
    theme(
      panel.grid = element_blank(),
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.title = element_blank(),
      strip.background = element_blank(),
      strip.text = element_text(size = 22),
      plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
      panel.border = element_rect(color = "grey60", fill = NA, linewidth = 0.5),
      plot.margin = margin(6, 6, 6, 6)
    )
}

# ---------------- paths ----------------
data_dir <- "../data/0410/"
output_dir <- "../output/0410_100/plot_paper_v2/"
define_dir <- "../define/"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
summary_dir <- file.path(output_dir, "csv_summary")
dir.create(summary_dir, recursive = TRUE, showWarnings = FALSE)

# ---------------- labels and ordering ----------------
scenario_labels <- c(
  "SSP2_BaU_NoCC" = "BASE",
  "SSP2_TBPROD_BaU_NoCC" = "PROD",
  "SSP2_TBDECR_BaU_NoCC" = "DECR",
  "SSP2_TBINCR_BaU_NoCC" = "INCR"
)

scenario_labels <- c(
  "SSP2_BaU_NoCC"                    = "BASE",
  "SSP2_TBPROD100_BaU_NoCC"       = "PROD",
  "SSP2_TBDECR100_BaU_NoCC"  = "DECR",
  "SSP2_TBINCR100_BaU_NoCC"   = "INCR"
)




data_dir   <- "../data/0407/"
output_dir <- "../output/0407/plot_paper_v2/"
define_dir <- "../define/"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
summary_dir <- file.path(output_dir, "csv_summary")
dir.create(summary_dir, recursive = TRUE, showWarnings = FALSE)


scenario_labels <- c(
  "SSP2_BaU_NoCC"                    = "BASE",
  "SSP2_BaU_Reallocation_NoCC"       = "PROD",
  "SSP2_BaU_TradeFacilitation_NoCC"  = "DECR",
  "SSP2_BaU_TradeRestriction_NoCC"   = "INCR"
)



scenario_colors <- c(
  "BASE" = "#000000",
  "PROD" = "#1b9e77",
  "DECR" = "#17becf",
  "INCR" = "#e7298a"
)

scenario_levels_line <- c("BASE", "DECR", "INCR", "PROD")
scenario_levels_bar <- c("DECR", "INCR", "PROD")

region_list <- c("R5REF", "R5OECD90+EU", "R5ASIA", "R5MAF", "R5LAM", "World")
region_labels_agmip <- c(
  "R5REF" = "Reforming Economies",
  "R5OECD90+EU" = "OECD & EU",
  "R5ASIA" = "Asia",
  "R5MAF" = "Middle East & Africa",
  "R5LAM" = "Latin America",
  "World" = "World"
)
region_levels_agmip <- unname(region_labels_agmip[region_list])
agmip_year_breaks <- seq(2020, 2100, by = 10)

land_var_map <- c(
  "Lan_Cov_Cro" = "Cropland",
  "Lan_Cov_Pst" = "Pasture",
  "Lan_Cov_Frs_Nat_Frs_Prm_Frs" = "Primary forest",
  "Lan_Cov_Frs_Nat_Frs_Sec_Frs" = "Secondary forest",
  "Lan_Cov_Frs_Aff_and_Ref" = "Planted forest",
  "Lan_Cov_Oth_Nat_Lan" = "Other Natural Land"
)

agmip_item_labels <- c(
  "CRP" = "Crop",
  "LSP" = "Livestock products",
  "AGR" = "Total"
)
agmip_item_colors <- c(
  "Crop" = "#1b9e77",
  "Livestock products" = "#f7b6d2",
  "Total" = "#377eb8"
)

land_item_labels_agmip <- c(
  "NLD" = "Urban",
  "CRP" = "Cropland",
  "GRS" = "Pasture",
  "ONV" = "Other Natural Land",
  "FOR" = "Forest"
)

make_panel_tag <- function(label_text) {
  ggplot() +
    annotate("text", x = 0, y = 0, label = label_text, hjust = 0, vjust = 0.5, size = 7, fontface = "bold") +
    coord_cartesian(xlim = c(0, 1), ylim = c(-1, 1), expand = FALSE) +
    theme_void()
}

plot_world_series <- function(data, item_code, plot_title, y_label, divide_by = 1) {
  data %>%
    filter(region == "World", ITEM == item_code) %>%
    mutate(
      plot_value = AGMIP_agg / divide_by,
      SCENARIO = factor(as.character(SCENARIO), levels = scenario_levels_line)
    ) %>%
    ggplot(aes(x = Y, y = plot_value, color = SCENARIO, group = SCENARIO)) +
    geom_line(linewidth = 1.2, na.rm = TRUE) +
    labs(title = plot_title, y = y_label) +
    scale_color_manual(values = scenario_colors, drop = FALSE) +
    scale_x_continuous(breaks = agmip_year_breaks, limits = range(agmip_year_breaks)) +
    theme_1 +
    theme(
      plot.title = element_text(size = 20, hjust = 0, face = "bold"),
      axis.title.y = element_text(size = 20),
      axis.text.x = element_text(size = 15, angle = 35, hjust = 1),
      panel.border = element_rect(color = "grey60", fill = NA, linewidth = 0.5)
    )
}

plot_land_series <- function(data, item_name) {
  data %>%
    filter(ITEM == item_name) %>%
    ggplot(aes(x = Year, y = Value, color = Scenario, group = Scenario)) +
    geom_line(linewidth = 1.2, na.rm = TRUE) +
    labs(title = item_name, y = "Area [million ha]") +
    scale_color_manual(values = scenario_colors, drop = FALSE) +
    scale_x_continuous(breaks = agmip_year_breaks, limits = range(agmip_year_breaks)) +
    theme_1 +
    theme(
      plot.title = element_text(size = 18, hjust = 0, face = "bold"),
      axis.title.y = element_text(size = 18),
      axis.text.x = element_text(size = 15, angle = 35, hjust = 1),
      panel.border = element_rect(color = "grey60", fill = NA, linewidth = 0.5)
    )
}

plot_productivity_series <- function(data, plot_title, y_label) {
  data %>%
    ggplot(aes(x = Year, y = Value, color = Scenario, group = Scenario)) +
    geom_line(linewidth = 1.2, na.rm = TRUE) +
    labs(title = plot_title, y = y_label) +
    scale_color_manual(values = scenario_colors, drop = FALSE) +
    scale_x_continuous(breaks = agmip_year_breaks, limits = range(agmip_year_breaks)) +
    theme_1 +
    theme(
      plot.title = element_text(size = 20, hjust = 0, face = "bold"),
      axis.title.y = element_text(size = 20),
      axis.text.x = element_text(size = 15, angle = 35, hjust = 1),
      panel.border = element_rect(color = "grey60", fill = NA, linewidth = 0.5)
    )
}

make_fig4 <- function(land_data, productivity_crop, productivity_livestock) {
  g_land_crop <- plot_land_series(land_data, "Cropland")
  g_land_pasture <- plot_land_series(land_data, "Pasture")
  g_land_forest <- plot_land_series(land_data, "Forest")
  g_land_other <- plot_land_series(land_data, "Other Natural Land")
  productivity_crop <- productivity_crop + labs(title = "Crop")
  productivity_livestock <- productivity_livestock + labs(title = "Livestock products")

  wrap_plots(
    T = make_panel_tag("a)"),
    U = make_panel_tag("b)"),
    A = g_land_crop,
    B = g_land_pasture,
    C = productivity_crop,
    D = g_land_forest,
    E = g_land_other,
    F = productivity_livestock,
    design = "
T##U
AB#C
DE#F
",
    guides = "collect",
    widths = c(1, 1, 0.06, 1),
    heights = c(0.08, 1, 1)
  ) & theme(legend.position = "bottom")
}

save_regional_series_plot <- function(data, y_label, file_name, width = 14, height = 8) {
  p <- data %>%
    ggplot(aes(x = Year, y = Value, color = Scenario, group = Scenario)) +
    geom_line(linewidth = 1.2, na.rm = TRUE) +
    facet_wrap(~ Region, scales = "free_y", ncol = 3) +
    labs(y = y_label) +
    scale_color_manual(values = scenario_colors, drop = FALSE) +
    scale_x_continuous(breaks = agmip_year_breaks, limits = c(min(agmip_year_breaks), max(agmip_year_breaks))) +
    theme_1 +
    theme(
      axis.text.x = element_text(size = 15, angle = 35, hjust = 1),
      axis.text.y = element_text(size = 15),
      axis.title.y = element_text(size = 18),
      strip.text = element_text(size = 17),
      legend.position = "bottom",
      panel.border = element_rect(color = "grey60", fill = NA, linewidth = 0.5)
    )

  out_path <- file.path(output_dir, file_name)
  ggsave(out_path, p, width = width, height = height, dpi = 220, bg = "white")
  message("Saved: ", out_path)
}

build_year_summary <- function(data, id_cols, unit_label) {
  years_of_interest <- c(2020, 2050, 2100)

  value_2020_tbl <- data %>%
    filter(Year == 2020) %>%
    select(all_of(c(id_cols, "Scenario")), value_2020 = Value)

  base_tbl <- data %>%
    filter(Scenario == "BASE", Year %in% years_of_interest) %>%
    select(all_of(c(id_cols, "Year")), base_value = Value)

  data %>%
    filter(Year %in% years_of_interest) %>%
    left_join(value_2020_tbl, by = c(id_cols, "Scenario")) %>%
    left_join(base_tbl, by = c(id_cols, "Year")) %>%
    mutate(
      unit = unit_label,
      diff_from_2020 = Value - value_2020,
      pct_from_2020 = if_else(!is.na(value_2020) & abs(value_2020) > 0, (Value - value_2020) / value_2020 * 100, NA_real_),
      diff_from_BASE = Value - base_value,
      pct_from_BASE = if_else(!is.na(base_value) & abs(base_value) > 0, (Value - base_value) / base_value * 100, NA_real_)
    ) %>%
    arrange(across(all_of(c(id_cols, "Scenario", "Year"))))
}

nc_path_for_map <- function(scen, nc_dir) {
  cand1 <- file.path(nc_dir, paste0("BIImap_", scen, ".nc"))
  cand2 <- file.path(nc_dir, paste0("BIImap_", scen, "_default.nc"))
  if (file.exists(cand1)) {
    return(cand1)
  }
  if (file.exists(cand2)) {
    return(cand2)
  }
  stop("NetCDF not found for scenario: ", scen)
}

get_time_index <- function(time_vals, year, tol = 1e-6) {
  idx <- which(abs(time_vals - year) < tol)
  if (length(idx) > 0) {
    return(idx[1])
  }
  which.min(abs(time_vals - year))
}

guess_dim_index <- function(nc, varname) {
  v <- nc$var[[varname]]
  dn <- tolower(sapply(v$dim, `[[`, "name"))
  lon_idx <- which(dn %in% c("longitude", "lon", "x"))[1]
  lat_idx <- which(dn %in% c("latitude", "lat", "y"))[1]
  time_idx <- which(dn %in% c("time", "year", "t"))[1]

  lon <- ncvar_get(nc, "longitude")
  lat <- ncvar_get(nc, "latitude")
  dims <- dim(ncvar_get(nc, varname))

  if (is.na(lon_idx) || is.na(lat_idx) || is.na(time_idx)) {
    lon_idx <- which(dims == length(lon))[1]
    lat_idx <- which(dims == length(lat))[1]
    time_idx <- setdiff(seq_along(dims), c(lon_idx, lat_idx))[1]
  }

  list(lon = lon_idx, lat = lat_idx, time = time_idx)
}

read_bii_map_at_year <- function(nc_path, year, varname = "BII", clamp01 = TRUE, lat_min = -60) {
  nc <- nc_open(nc_path)
  on.exit(nc_close(nc))

  lon <- ncvar_get(nc, "longitude")
  lat <- ncvar_get(nc, "latitude")
  tim <- ncvar_get(nc, "time")
  raw <- ncvar_get(nc, varname)

  idx <- guess_dim_index(nc, varname)
  arr <- aperm(raw, c(idx$lon, idx$lat, idx$time))
  ti <- get_time_index(tim, year)
  slice <- arr[, , ti]

  if (clamp01) {
    slice <- pmax(0, pmin(slice, 1))
  }

  df <- expand.grid(lon = lon, lat = lat)
  df$value <- as.vector(slice)
  df$value[df$lat < lat_min] <- NA
  df
}

plot_diff_map_one <- function(df_diff, title_txt, world_sf, diff_limits = c(-0.3, 0.3), lat_min = -60) {
  ggplot() +
    geom_raster(data = df_diff, aes(x = lon, y = lat, fill = value), na.rm = TRUE) +
    geom_sf(data = world_sf, fill = NA, color = "grey20", linewidth = 0.25) +
    coord_sf(expand = FALSE, ylim = c(lat_min, 90)) +
    scale_fill_gradient2(
      low = "red",
      mid = "white",
      high = "blue",
      midpoint = 0,
      limits = diff_limits,
      oob = squish,
      na.value = "grey95",
      name = "ΔBII",
      guide = guide_colorbar(
        title.position = "top",
        barheight = grid::unit(60, "mm"),
        barwidth = grid::unit(10, "mm")
      )
    ) +
    labs(title = title_txt) +
    theme_map_min() +
    theme(
      legend.title = element_text(size = 22),
      legend.text = element_text(size = 20)
    )
}

# ======================================================
# Data loading
# ======================================================
IAMCTemp <- rgdx.param(paste0(data_dir, "Mergedcomparison.gdx"), "IAMCTemp") %>%
  rename(Scenario = i1, Region = i2, Variable = i3, Unit = i4, Year = i5, Value = value) %>%
  mutate(
    Year = as.numeric(as.character(Year)),
    Scenario = as.character(Scenario),
    Region = as.character(Region)
  ) %>%
  filter(Region %in% region_list) %>%
  filter(Scenario %in% names(scenario_labels)) %>%
  mutate(
    Scenario = factor(Scenario, levels = names(scenario_labels), labels = unname(scenario_labels)),
    Scenario = factor(as.character(Scenario), levels = scenario_levels_line),
    Region = factor(Region, levels = region_list)
  )

AGMIP_agg <- rgdx.param(paste0(data_dir, "AGMIP_all_scenario.gdx"), "AGMIP_agg") %>%
  rename(VAR = AgMIP_VAR, UNIT = AgMIP_UNIT) %>%
  mutate(
    Y = as.numeric(as.character(Y)),
    SCENARIO = as.character(SCENARIO),
    region = as.character(region)
  ) %>%
  filter(
    SCENARIO %in% names(scenario_labels),
    region %in% names(region_labels_agmip)
  ) %>%
  mutate(
    SCENARIO = factor(SCENARIO, levels = names(scenario_labels), labels = unname(scenario_labels)),
    SCENARIO = factor(as.character(SCENARIO), levels = scenario_levels_line),
    region = factor(region_labels_agmip[region], levels = region_levels_agmip)
  ) %>%
  filter(!is.na(Y), Y >= 2015, Y <= 2100)

Land_world <- IAMCTemp %>%
  filter(
    Region == "World",
    Year %in% seq(2015, 2100, by = 5),
    Variable %in% names(land_var_map)
  ) %>%
  mutate(ITEM = land_var_map[as.character(Variable)]) %>%
  select(Scenario, Year, Unit, ITEM, Value)

world_sf <- ne_countries(scale = "medium", returnclass = "sf") %>%
  dplyr::filter(iso_a3 != "ATA")

c_map <- readLines(file.path(define_dir, "c.map"), warn = FALSE) %>%
  stringr::str_trim() %>%
  .[. != "" & !startsWith(., "*")] %>%
  stringr::str_split("\\s+") %>%
  purrr::map_dfr(function(x) {
    tibble::tibble(commodity = x[[1]], item_group = x[[length(x)]])
  }) %>%
  filter(item_group %in% c("CRP", "LSP")) %>%
  distinct()

# ======================================================
# Fig1: Trade, production, and regional production change
# ======================================================
g_prod_crop <- plot_world_series(
  AGMIP_agg %>% filter(VAR == "PROD", UNIT == "1000 t"),
  item_code = "CRP",
  plot_title = "Crop",
  y_label = "Production [million t]",
  divide_by = 1000
)

g_prod_livestock <- plot_world_series(
  AGMIP_agg %>% filter(VAR == "PROD", UNIT == "1000 t"),
  item_code = "LSP",
  plot_title = "Livestock products",
  y_label = "Production [million t]",
  divide_by = 1000
)

g_trade_crop <- plot_world_series(
  AGMIP_agg %>% filter(VAR == "EXPO", UNIT == "1000 t"),
  item_code = "CRP",
  plot_title = "Crop",
  y_label = "Trade volume [million t]",
  divide_by = 1000
)

g_trade_livestock <- plot_world_series(
  AGMIP_agg %>% filter(VAR == "EXPO", UNIT == "1000 t"),
  item_code = "LSP",
  plot_title = "Livestock products",
  y_label = "Trade volume [million t]",
  divide_by = 1000
)

fig1_left <- wrap_plots(
  T = make_panel_tag("a)"),
  U = make_panel_tag("b)"),
  A = g_trade_crop + labs(title = "Crop"),
  B = g_prod_crop + labs(title = "Crop"),
  C = g_trade_livestock + labs(title = "Livestock products"),
  D = g_prod_livestock + labs(title = "Livestock products"),
  design = "
T#U
A#B
C#D
",
  guides = "collect",
  widths = c(1, 0.02, 1),
  heights = c(0.08, 1, 1)
) & theme(
  legend.position = "bottom",
  plot.title = element_text(size = 20, face = "bold", hjust = 0)
)

prod_change_2050 <- AGMIP_agg %>%
  filter(VAR == "PROD", UNIT == "1000 t", ITEM %in% names(agmip_item_labels), Y == 2050) %>%
  mutate(
    AGMIP_agg = AGMIP_agg / 1000,
    ITEM = recode(ITEM, !!!agmip_item_labels)
  ) %>%
  select(SCENARIO, region, ITEM, AGMIP_agg) %>%
  left_join(
    AGMIP_agg %>%
      filter(VAR == "PROD", UNIT == "1000 t", ITEM %in% names(agmip_item_labels), Y == 2050, SCENARIO == "BASE") %>%
      mutate(
        AGMIP_agg = AGMIP_agg / 1000,
        ITEM = recode(ITEM, !!!agmip_item_labels)
      ) %>%
      select(region, ITEM, base_value = AGMIP_agg),
    by = c("region", "ITEM")
  ) %>%
  mutate(change_from_base = AGMIP_agg - base_value) %>%
  filter(SCENARIO %in% scenario_levels_bar) %>%
  mutate(
    ITEM = factor(ITEM, levels = c("Crop", "Livestock products", "Total")),
    SCENARIO = factor(as.character(SCENARIO), levels = scenario_levels_bar)
  )

fig1_right <- prod_change_2050 %>%
  filter(ITEM %in% c("Crop", "Livestock products")) %>%
  ggplot(aes(x = SCENARIO, y = change_from_base, fill = ITEM)) +
  geom_hline(yintercept = 0, linewidth = 0.6) +
  geom_col(width = 0.75) +
  geom_point(
    data = prod_change_2050 %>% filter(ITEM == "Total"),
    aes(x = SCENARIO, y = change_from_base, color = "Total"),
    size = 2.4,
    inherit.aes = FALSE
  ) +
  facet_wrap(~ region, scales = "free_y", ncol = 3) +
  labs(y = "Difference in production from BASE [million t]") +
  scale_fill_manual(values = agmip_item_colors[c("Crop", "Livestock products")], drop = FALSE) +
  scale_color_manual(values = c("Total" = agmip_item_colors[["Total"]])) +
  theme_1 +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    axis.title.y = element_text(size = 20),
    strip.text = element_text(size = 18),
    panel.border = element_rect(color = "grey60", fill = NA, linewidth = 0.5),
    plot.title = element_text(size = 20, face = "bold", hjust = 0)
  )

fig1_right_tagged <- wrap_plots(
  T = make_panel_tag("c)"),
  A = fig1_right,
  design = "
T
A
",
  heights = c(0.08, 2)
)

fig1 <- wrap_plots(
  A = wrap_elements(full = fig1_left),
  B = wrap_elements(full = fig1_right_tagged),
  ncol = 2,
  widths = c(1.2, 1.0)
)

ggsave(
  file.path(output_dir, "Fig1_trade_production_regional.png"),
  fig1,
  width = 22,
  height = 10,
  dpi = 220,
  bg = "white"
)

fig1_trade_prod_summary <- bind_rows(
  AGMIP_agg %>%
    filter(VAR %in% c("EXPO", "PROD"), UNIT == "1000 t", ITEM %in% c("CRP", "LSP"), region == "World") %>%
    transmute(
      component = case_when(
        VAR == "EXPO" ~ "Trade volume",
        VAR == "PROD" ~ "Production",
        TRUE ~ VAR
      ),
      region = as.character(region),
      item = recode(ITEM, "CRP" = "Crop", "LSP" = "Livestock products"),
      Scenario = factor(as.character(SCENARIO), levels = scenario_levels_line),
      Year = Y,
      Value = AGMIP_agg / 1000
    ),
  AGMIP_agg %>%
    filter(VAR == "PROD", UNIT == "1000 t", ITEM %in% names(agmip_item_labels)) %>%
    transmute(
      component = "Regional production",
      region = as.character(region),
      item = recode(ITEM, !!!agmip_item_labels),
      Scenario = factor(as.character(SCENARIO), levels = scenario_levels_line),
      Year = Y,
      Value = AGMIP_agg / 1000
    )
) %>%
  mutate(region = factor(region, levels = region_levels_agmip))

write.csv(
  build_year_summary(
    fig1_trade_prod_summary,
    id_cols = c("component", "region", "item"),
    unit_label = "million t"
  ),
  file.path(summary_dir, "Fig1_trade_production_regional_summary.csv"),
  row.names = FALSE
)

# # ======================================================
# # Fig2: Regional price change in 2100
# # ======================================================
# xprc_change_2100 <- AGMIP_agg %>%
#   filter(VAR == "XPRC", ITEM == "AGR", UNIT == "USD/t", Y == 2100) %>%
#   select(SCENARIO, region, AGMIP_agg) %>%
#   left_join(
#     AGMIP_agg %>%
#       filter(VAR == "XPRC", ITEM == "AGR", UNIT == "USD/t", Y == 2100, SCENARIO == "BASE") %>%
#       select(region, base_value = AGMIP_agg),
#     by = "region"
#   ) %>%
#   mutate(
#     change_from_base_pct = (AGMIP_agg - base_value) / base_value * 100,
#     SCENARIO = factor(as.character(SCENARIO), levels = scenario_levels_bar),
#     region = factor(as.character(region), levels = region_levels_agmip)
#   ) %>%
#   filter(SCENARIO %in% scenario_levels_bar)

# fig2 <- ggplot(xprc_change_2100, aes(x = SCENARIO, y = change_from_base_pct, fill = SCENARIO)) +
#   geom_hline(yintercept = 0, linewidth = 0.6) +
#   geom_col(width = 0.75) +
#   facet_wrap(~ region, scales = "free_y", ncol = 3) +
#   labs(y = "Change in price from BASE in 2100 [%]") +
#   scale_fill_manual(values = scenario_colors[scenario_levels_bar], drop = FALSE) +
#   theme_1 +
#   theme(
#     axis.text.x = element_text(size = 15, angle = 35, hjust = 1),
#     axis.text.y = element_text(size = 15),
#     axis.title.y = element_text(size = 20),
#     strip.text = element_text(size = 18),
#     legend.position = "none",
#     panel.border = element_rect(color = "grey60", fill = NA, linewidth = 0.5)
#   )

# ggsave(
#   file.path(output_dir, "Fig2_regional_price_change_2100.png"),
#   fig2,
#   width = 14,
#   height = 8.5,
#   dpi = 220,
#   bg = "white"
# )

PC_agg <- rgdx.param(paste0(data_dir, "all_scenario_global_17.gdx"), "PC_agg_load") %>%
  mutate(
    SCENARIO = as.character(SCENARIO),
    c = as.character(c),
    region = as.character(region),
    Y = as.numeric(as.character(Y))
  )

QDEM_agg <- rgdx.param(paste0(data_dir, "all_scenario_global_17.gdx"), "QDEM_agg_load") %>%
  mutate(
    SCENARIO = as.character(SCENARIO),
    c = as.character(c),
    region = as.character(region),
    Y = as.numeric(as.character(Y))
  )

pc_qsup_detail <- PC_agg %>%
  filter(SCENARIO %in% names(scenario_labels), region %in% region_list) %>%
  inner_join(c_map, by = c("c" = "commodity")) %>%
  inner_join(
    QDEM_agg %>%
      filter(SCENARIO %in% names(scenario_labels), region %in% region_list) %>%
      select(SCENARIO, c, region, Y, QDEM_agg_load),
    by = c("SCENARIO", "c", "region", "Y")
  ) %>%
  mutate(total_value = PC_agg_load * QDEM_agg_load)

pc_group_prices <- pc_qsup_detail %>%
  group_by(SCENARIO, region, Y, ITEM = item_group) %>%
  summarise(
    total_value = sum(total_value, na.rm = TRUE),
    total_qdem = sum(QDEM_agg_load, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Value = total_value / total_qdem) %>%
  select(SCENARIO, region, Y, ITEM, Value)

pc_agr_prices <- pc_qsup_detail %>%
  filter(item_group %in% c("CRP", "LSP")) %>%
  group_by(SCENARIO, region, Y) %>%
  summarise(
    total_value = sum(total_value, na.rm = TRUE),
    total_qdem = sum(QDEM_agg_load, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    ITEM = "AGR",
    Value = total_value / total_qdem
  ) %>%
  select(SCENARIO, region, Y, ITEM, Value)

pc_price_agg <- bind_rows(pc_group_prices, pc_agr_prices) %>%
  mutate(
    Scenario = factor(
      SCENARIO,
      levels = names(scenario_labels),
      labels = unname(scenario_labels)
    ),
    Scenario = factor(as.character(Scenario), levels = scenario_levels_line),
    Region = factor(region_labels_agmip[region], levels = region_levels_agmip)
  )

pc_agr_change_2100 <- pc_price_agg %>%
  filter(ITEM == "AGR", Y == 2100) %>%
  select(Scenario, Region, Value) %>%
  left_join(
    pc_price_agg %>%
      filter(ITEM == "AGR", Y == 2100, Scenario == "BASE") %>%
      select(Region, base_value = Value),
    by = "Region"
  ) %>%
  mutate(change_from_base_pct = (Value - base_value) / base_value * 100) %>%
  filter(Scenario %in% scenario_levels_bar)

fig2_pcagg <- ggplot(pc_agr_change_2100, aes(x = Scenario, y = change_from_base_pct, fill = Scenario)) +
  geom_hline(yintercept = 0, linewidth = 0.6) +
  geom_col(width = 0.75) +
  facet_wrap(~ Region, scales = "free_y", ncol = 3) +
  labs(y = "Difference in price from BASE in 2100 [%]") +
  scale_fill_manual(values = scenario_colors[scenario_levels_bar], drop = FALSE) +
  theme_1 +
  theme(
    axis.text.x = element_text(size = 15, angle = 35, hjust = 1),
    axis.text.y = element_text(size = 15),
    axis.title.y = element_text(size = 20),
    strip.text = element_text(size = 18),
    legend.position = "none",
    panel.border = element_rect(color = "grey60", fill = NA, linewidth = 0.5)
  )

ggsave(
  file.path(output_dir, "Fig2_regional_price_change_2100_PCagg_AGR.png"),
  fig2_pcagg,
  width = 14,
  height = 8.5,
  dpi = 220,
  bg = "white"
)

# ======================================================
# Fig3: Mergedcomparison land and productivity
# ======================================================
Land_fig4_merged <- Land_world %>%
  mutate(
    ITEM = case_when(
      ITEM %in% c("Primary forest", "Secondary forest", "Planted forest") ~ "Forest",
      TRUE ~ as.character(ITEM)
    )
  ) %>%
  filter(ITEM %in% c("Cropland", "Pasture", "Forest", "Other Natural Land")) %>%
  group_by(Scenario, Year, ITEM) %>%
  summarise(Value = sum(Value), .groups = "drop") %>%
  mutate(
    ITEM = factor(ITEM, levels = c("Cropland", "Pasture", "Forest", "Other Natural Land")),
    Scenario = factor(as.character(Scenario), levels = scenario_levels_line)
  )

prod_crop_world <- AGMIP_agg %>%
  filter(region == "World", VAR == "PROD", ITEM == "CRP", UNIT == "1000 t") %>%
  transmute(
    Scenario = factor(as.character(SCENARIO), levels = scenario_levels_line),
    Year = Y,
    prod_crop_million_t = AGMIP_agg / 1000
  )

prod_rum_world <- AGMIP_agg %>%
  filter(region == "World", VAR == "PROD", ITEM == "RUM", UNIT == "1000 t") %>%
  transmute(
    Scenario = factor(as.character(SCENARIO), levels = scenario_levels_line),
    Year = Y,
    prod_rum_million_t = AGMIP_agg / 1000
  )

merged_crop_productivity <- Land_fig4_merged %>%
  filter(ITEM == "Cropland") %>%
  left_join(prod_crop_world, by = c("Scenario", "Year")) %>%
  transmute(
    Scenario,
    Year,
    Value = prod_crop_million_t / Value * 1000
  )

merged_pasture_productivity <- Land_fig4_merged %>%
  filter(ITEM == "Pasture") %>%
  left_join(prod_rum_world, by = c("Scenario", "Year")) %>%
  transmute(
    Scenario,
    Year,
    Value = prod_rum_million_t / Value * 1000
  )

fig3 <- make_fig4(
  Land_fig4_merged,
  plot_productivity_series(
    merged_crop_productivity,
    plot_title = "Crop",
    y_label = "Production per cropland area [kg/ha]"
  ),
  plot_productivity_series(
    merged_pasture_productivity,
    plot_title = "Ruminant products",
    y_label = "Production per pasture area [kg/ha]"
  )
)

ggsave(
  file.path(output_dir, "Fig3_land_productivity_mergedcomparison.png"),
  fig3,
  width = 18,
  height = 11,
  dpi = 220,
  bg = "white"
)

fig3_summary <- bind_rows(
  Land_fig4_merged %>%
    transmute(
      component = "Land",
      region = "World",
      item = as.character(ITEM),
      Scenario,
      Year,
      Value,
      unit = "million ha"
    ),
  merged_crop_productivity %>%
    transmute(
      component = "Productivity",
      region = "World",
      item = "Crop",
      Scenario,
      Year,
      Value,
      unit = "kg/ha"
    ),
  merged_pasture_productivity %>%
    transmute(
      component = "Productivity",
      region = "World",
      item = "Ruminant products",
      Scenario,
      Year,
      Value,
      unit = "kg/ha"
    )
)

fig3_summary_out <- bind_rows(
  build_year_summary(
    fig3_summary %>% filter(unit == "million ha") %>% select(-unit),
    id_cols = c("component", "region", "item"),
    unit_label = "million ha"
  ),
  build_year_summary(
    fig3_summary %>% filter(unit == "kg/ha") %>% select(-unit),
    id_cols = c("component", "region", "item"),
    unit_label = "kg/ha"
  )
)

write.csv(
  fig3_summary_out,
  file.path(summary_dir, "Fig3_land_productivity_mergedcomparison_summary.csv"),
  row.names = FALSE
)

# ======================================================
# Fig4: Regional BII time series
# ======================================================
bii_regional <- IAMCTemp %>%
  filter(Variable == "Ter_Bio_Bio_Int_Ind") %>%
  transmute(
    Scenario = factor(as.character(Scenario), levels = scenario_levels_line),
    Region = factor(as.character(Region), levels = region_list, labels = region_levels_agmip),
    Year,
    Value = Value / 100
  )

save_regional_series_plot(
  bii_regional,
  y_label = "BII",
  file_name = "Fig4_BII_regional_facets.png"
)

write.csv(
  build_year_summary(
    bii_regional,
    id_cols = c("Region"),
    unit_label = "BII"
  ),
  file.path(summary_dir, "Fig4_BII_regional_summary.csv"),
  row.names = FALSE
)

# ======================================================
# Fig5: BII difference maps in 2100
# ======================================================
nc_dir <- file.path(data_dir, "BII", "nc")
map_year <- 2100
lat_min <- -60
diff_limits_map <- c(-0.3, 0.3)

base_nc_scen <- names(scenario_labels)[scenario_labels == "BASE"][1]
cmp_nc_scen <- names(scenario_labels)[scenario_labels %in% c("PROD", "DECR", "INCR")]
cmp_labels <- unname(scenario_labels[cmp_nc_scen])
names(cmp_nc_scen) <- cmp_labels

df_base_map <- read_bii_map_at_year(
  nc_path_for_map(base_nc_scen, nc_dir),
  year = map_year,
  varname = "BII",
  clamp01 = TRUE,
  lat_min = lat_min
)

diff_maps <- lapply(names(cmp_nc_scen), function(lbl) {
  df_cmp_map <- read_bii_map_at_year(
    nc_path_for_map(cmp_nc_scen[[lbl]], nc_dir),
    year = map_year,
    varname = "BII",
    clamp01 = TRUE,
    lat_min = lat_min
  )

  dfm <- merge(df_base_map, df_cmp_map, by = c("lon", "lat"), suffixes = c("_base", "_cmp"))
  dfm$value <- dfm$value_cmp - dfm$value_base
  dfm <- dfm[, c("lon", "lat", "value")]

  plot_diff_map_one(dfm, title_txt = lbl, world_sf = world_sf, diff_limits = diff_limits_map, lat_min = lat_min)
})
names(diff_maps) <- names(cmp_nc_scen)

fig5 <- wrap_plots(
  A = diff_maps[["DECR"]],
  B = diff_maps[["INCR"]],
  C = diff_maps[["PROD"]],
  G = patchwork::guide_area(),
  design = "
AABBG
#CC#G
"
) +
  plot_layout(guides = "collect", widths = c(1, 1, 1, 1, 0.45)) &
  theme(
    legend.position = "right",
    legend.direction = "vertical"
  )

ggsave(
  file.path(output_dir, paste0("Fig5_BII_map_diff_from_BASE_", map_year, ".png")),
  fig5,
  width = 22,
  height = 10,
  dpi = 600,
  bg = "white"
)
