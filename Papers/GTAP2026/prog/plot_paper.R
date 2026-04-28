library(dplyr)
library(ggplot2)
library(sf)
library(tidyr)
library(patchwork)
library(gdxrrw)

# ---------------- theme ----------------
theme_1 <- theme_bw() +
  theme(
    panel.grid.major = element_line(color = "grey80", linewidth = 0.5),
    panel.grid.minor = element_line(color = "grey90", linewidth = 0.25),
    text = element_text(size = 24),
    axis.text.x = element_text(size = 18, angle = 45, hjust = 1),
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

# ---------------- paths ----------------
data_dir   <- "../data/0329/"
output_dir <- "../output/0329/"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# ---------------- scenario labels/order ----------------
scenario_labels <- c(
  "SSP2_BaU_NoCC"                    = "BASE",
  "SSP2_TBPROD_BaU_NoCC"       = "PROD",
  "SSP2_TBDECR_BaU_NoCC"  = "DECR",
  "SSP2_TBINCR_BaU_NoCC"   = "INCR"
)


# data_dir   <- "../data/0407/"
# output_dir <- "../output/0407/"
# dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# scenario_labels <- c(
#   "SSP2_BaU_NoCC"                    = "BASE",
#   "SSP2_BaU_Reallocation_NoCC"       = "PROD",
#   "SSP2_BaU_TradeFacilitation_NoCC"  = "DECR",
#   "SSP2_BaU_TradeRestriction_NoCC"   = "INCR"
# )

# scenario_labels <- c(
#   "SSP2_BaU_NoCC"                    = "BASE",
#   "SSP2_TBPROD100_BaU_NoCC"       = "PROD",
#   "SSP2_TBDECR100_BaU_NoCC"  = "DECR",
#   "SSP2_TBINCR100_BaU_NoCC"   = "INCR"
# )

scenario_colors <- c(
  "BASE" = "#000000",
  "PROD" = "#1b9e77",
  "DECR" = "#17becf",
  "INCR" = "#e7298a"
)

region_list <- c("R5REF", "R5OECD90+EU", "R5ASIA", "R5MAF", "R5LAM", "World")
x_breaks_nice <- seq(2020, 2100, by = 20)
land_bar_years <- seq(2030, 2100, by = 10)

# ---------------- Land ITEM order & colors ----------------
land_items_order <- c(
  "Cropland",
  "Pasture",
  "Primary forest",
  "Secondary forest",
  "Planted forest",
  "Other Natural Land"
)

land_item_colors <- c(
  "Cropland"           = "#FF9855",  # yellow/gold
  "Pasture"            = "#26C3EC",  # light green
  "Primary forest"     = "#1B5E20",  # dark green
  "Secondary forest"   = "#4CAF50",  # medium green
  "Planted forest"      = "#9467bd",  # teal
  "Other Natural Land" = "#B08D57"   # brown/olive
)

# ======================================================
# Read IAMCTemp once
# ======================================================
IAMCTemp <- rgdx.param(paste0(data_dir,"Mergedcomparison.gdx"), "IAMCTemp") %>%
  rename(Scenario=i1, Region=i2, Variable=i3, Unit=i4, Year=i5, Value=value) %>%
  mutate(
    Year     = as.numeric(as.character(Year)),
    Scenario = as.character(Scenario),
    Region   = as.character(Region)
  ) %>%
  filter(Region %in% region_list) %>%
  mutate(Region = factor(Region, levels = region_list)) %>%
  filter(Scenario %in% names(scenario_labels)) %>%
  mutate(
    Scenario = factor(
      Scenario,
      levels = names(scenario_labels),
      labels = unname(scenario_labels)
    )
  )

# ======================================================
# (c) BII plot (World only)
# ======================================================
df_BII <- IAMCTemp %>%
  filter(Variable == "Ter_Bio_Bio_Int_Ind", Region == "World") %>%
  mutate(Value = Value / 100) %>%
  mutate(Scenario = factor(as.character(Scenario), levels = c("BASE", "DECR", "INCR", "PROD")))

units_bii <- df_BII %>% distinct(Unit) %>% pull(Unit) %>% as.character()
unit_label_bii <- if (length(units_bii) == 1) units_bii else paste(units_bii, collapse = " / ")
y_lab_bii <- paste0("BII")

p_BII <- ggplot(df_BII, aes(x = Year, y = Value, color = Scenario, group = Scenario)) +
  geom_line(linewidth = 1.2, na.rm = TRUE) +
  labs(y = y_lab_bii) +
  scale_color_manual(values = scenario_colors, drop = FALSE) +
  scale_x_continuous(breaks = x_breaks_nice, limits = c(2020, 2100)) +
  theme_1 +
  theme(
    axis.text.x = element_text(size = 16, angle = 35, hjust = 1),
    panel.border = element_rect(color = "grey60", fill = NA, linewidth = 0.5)
  )

# ======================================================
# Land from IAMCTemp (World, specified variables only)
# ======================================================
land_var_map <- c(
  "Lan_Cov_Cro"                 = "Cropland",
  "Lan_Cov_Pst"                 = "Pasture",
  "Lan_Cov_Frs_Nat_Frs_Prm_Frs" = "Primary forest",
  "Lan_Cov_Frs_Nat_Frs_Sec_Frs" = "Secondary forest",
  "Lan_Cov_Frs_Aff_and_Ref"     = "Planted forest",
  "Lan_Cov_Oth_Nat_Lan"         = "Other Natural Land"
)

Land_world <- IAMCTemp %>%
  filter(
    Region == "World",
    Year %in% seq(2015, 2100, by = 5),
    Variable %in% names(land_var_map)
  ) %>%
  mutate(
    ITEM = land_var_map[as.character(Variable)],
    ITEM = factor(ITEM, levels = land_items_order)
  ) %>%
  select(Scenario, Year, Unit, ITEM, Value) %>%
  group_by(Unit) %>%
  complete(Scenario, Year, ITEM, fill = list(Value = 0)) %>%
  ungroup()

units_land <- Land_world %>% distinct(Unit) %>% pull(Unit) %>% as.character()
unit_label_land <- if (length(units_land) == 1) units_land else paste(units_land, collapse = " / ")

# ======================================================
# (a) BASE land-use stacked BARs (2030-2100, every 10 years)
# ======================================================
Land_base_bar <- Land_world %>%
  filter(Scenario == "BASE", Year %in% land_bar_years) %>%
  mutate(Year_f = factor(Year, levels = land_bar_years))

p_Land_BASE <- ggplot(Land_base_bar, aes(x = Year_f, y = Value, fill = ITEM)) +
  geom_col(width = 0.85) +
  labs(y = paste0("Land area in BASE scenario [Mha]")) +
  scale_fill_manual(values = land_item_colors, breaks = land_items_order, drop = FALSE) +
  theme_1 +
  theme(
    axis.text.x = element_text(size = 16, angle = 35, hjust = 1),
    panel.border = element_rect(color = "grey60", fill = NA, linewidth = 0.5)
  )

# ======================================================
# (b) Differences from BASE at 2050 & 2100 (stacked +/-)
# ======================================================
years_diff <- c(2050, 2100)

base_tbl <- Land_world %>%
  filter(Scenario == "BASE", Year %in% years_diff) %>%
  select(Year, ITEM, base = Value)

diff_tbl <- Land_world %>%
  filter(Scenario %in% c("PROD", "DECR", "INCR"), Year %in% years_diff) %>%
  left_join(base_tbl, by = c("Year", "ITEM")) %>%
  mutate(diff = Value - base) %>%
  mutate(
    Scenario = factor(as.character(Scenario), levels = c("DECR", "INCR", "PROD")),
    Year = factor(Year, levels = years_diff)
  )

p_Land_DIFF <- ggplot(diff_tbl, aes(x = Scenario, y = diff, fill = ITEM)) +
  geom_hline(yintercept = 0, linewidth = 0.6) +
  geom_col(width = 0.75) +
  facet_wrap(~ Year, ncol = 1, scales = "free_y") +
  labs(y = paste0("Differences in land area from BASE [Mha]")) +
  scale_fill_manual(values = land_item_colors, breaks = land_items_order, drop = FALSE) +
  theme_1 +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    panel.border = element_rect(color = "grey60", fill = NA, linewidth = 0.5)
  )

# ======================================================
# Combine: (a) Land BASE left, (b) Land diff middle, (c) BII right
# ======================================================
combined <- (p_Land_BASE + p_Land_DIFF + p_BII) +
  plot_layout(
    guides = "collect",
    widths = c(1.10, 1.00, 1.25)
  ) +
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")") &
  theme(legend.position = "bottom")

out_file <- file.path(output_dir, "Fig1.png")
ggsave(out_file, combined, width = 22, height = 8, dpi = 220, bg = "white")
message("Saved: ", out_file)





# ======================================================
# (d) Map: ΔBII (2100) = (PROD/DECR/INCR) - BASE from NetCDF, 3 panels in one row
# ======================================================
library(ncdf4)
library(rnaturalearth)
library(rnaturalearthdata)
library(viridis)
library(scales)
library(patchwork)

# --- paths for NetCDF ---
nc_dir <- file.path(data_dir, "BII", "nc")  # adjust if your nc folder differs
map_year <- 2100
lat_min <- -60
diff_limits_map <- c(-0.3, 0.3)

# --- coastline (remove Antarctica) ---
world_sf <- ne_countries(scale = "medium", returnclass = "sf") %>%
  dplyr::filter(iso_a3 != "ATA")

# --- scenario names for NetCDF (use your mapping) ---
base_nc_scen <- names(scenario_labels)[scenario_labels == "BASE"][1]
cmp_nc_scen  <- names(scenario_labels)[scenario_labels %in% c("PROD", "DECR", "INCR")]
cmp_labels   <- unname(scenario_labels[cmp_nc_scen])  # "PROD" "DECR" "INCR"
names(cmp_nc_scen) <- cmp_labels                       # name them by label

# --- helpers ---
nc_path_for2 <- function(scen, nc_dir) {
  # Try both: BIImap_<scen>.nc and BIImap_<scen>_default.nc
  cand1 <- file.path(nc_dir, paste0("BIImap_", scen, ".nc"))
  cand2 <- file.path(nc_dir, paste0("BIImap_", scen, "_default.nc"))
  if (file.exists(cand1)) return(cand1)
  if (file.exists(cand2)) return(cand2)
  stop("NetCDF not found for scenario: ", scen, "\nTried:\n", cand1, "\n", cand2)
}

get_time_index <- function(time_vals, year, tol = 1e-6) {
  idx <- which(abs(time_vals - year) < tol)
  if (length(idx) > 0) return(idx[1])
  idx2 <- which.min(abs(time_vals - year))
  warning(sprintf("Year %s not found exactly; using nearest time=%s", year, time_vals[idx2]))
  idx2
}

guess_dim_index <- function(nc, varname) {
  v  <- nc$var[[varname]]
  dn <- tolower(sapply(v$dim, `[[`, "name"))
  
  lon_idx  <- which(dn %in% c("longitude", "lon", "x"))[1]
  lat_idx  <- which(dn %in% c("latitude", "lat", "y"))[1]
  time_idx <- which(dn %in% c("time", "year", "t"))[1]
  
  lon <- ncvar_get(nc, "longitude")
  lat <- ncvar_get(nc, "latitude")
  dims <- dim(ncvar_get(nc, varname))
  
  if (is.na(lon_idx) || is.na(lat_idx) || is.na(time_idx)) {
    lon_idx  <- which(dims == length(lon))[1]
    lat_idx  <- which(dims == length(lat))[1]
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
  arr <- aperm(raw, c(idx$lon, idx$lat, idx$time))  # (lon, lat, time)
  
  ti <- get_time_index(tim, year)
  slice <- arr[, , ti]
  
  if (clamp01) slice <- pmax(0, pmin(slice, 1))
  
  df <- expand.grid(lon = lon, lat = lat)
  df$value <- as.vector(slice)
  
  # mask Antarctica
  df$value[df$lat < lat_min] <- NA
  df
}

theme_map_min <- function() {
  theme_bw(base_size = 20) +
    theme(
      panel.grid = element_blank(),
      axis.title = element_blank(),
      axis.text  = element_blank(),
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

plot_diff_map_one <- function(df_diff, title_txt, diff_limits = c(-0.4, 0.4), lat_min = -60) {
  ggplot() +
    geom_raster(
      data = df_diff,
      aes(x = lon, y = lat, fill = value),
      na.rm = TRUE
    ) +
    geom_sf(
      data = world_sf,
      fill = NA,
      color = "grey20",
      linewidth = 0.25
    ) +
    coord_sf(expand = FALSE, ylim = c(lat_min, 90)) +
    scale_fill_gradient2(
      low = "red", mid = "white", high = "blue",
      midpoint = 0,
      limits = diff_limits,
      oob = squish,
      na.value = "grey95",
      name = "ΔBII",
      guide = guide_colorbar(
        title.position = "top",
        barheight = grid::unit(60, "mm"),
        barwidth  = grid::unit(10, "mm")
      )
    ) +
    labs(title = title_txt) +
    theme_map_min() +
    theme(
      legend.title = element_text(size = 22),
      legend.text  = element_text(size = 20)
    )
  
}

# --- read BASE once ---
nc_base <- nc_path_for2(base_nc_scen, nc_dir)
df_base_map <- read_bii_map_at_year(nc_base, year = map_year, varname = "BII", clamp01 = TRUE, lat_min = lat_min)

# --- build 3 diff maps (compare - base) ---
diff_maps <- lapply(names(cmp_nc_scen), function(lbl) {
  scen <- cmp_nc_scen[[lbl]]
  nc_cmp <- nc_path_for2(scen, nc_dir)
  df_cmp_map <- read_bii_map_at_year(nc_cmp, year = map_year, varname = "BII", clamp01 = TRUE, lat_min = lat_min)
  
  dfm <- merge(df_base_map, df_cmp_map, by = c("lon", "lat"), suffixes = c("_base", "_cmp"))
  dfm$value <- dfm$value_cmp - dfm$value_base
  dfm <- dfm[, c("lon", "lat", "value")]
  
  plot_diff_map_one(dfm, title_txt = lbl, diff_limits = diff_limits_map, lat_min = lat_min)
})
names(diff_maps) <- names(cmp_nc_scen)


design_2row_centerC <- "
AABBG
#CC#G
"

plots <- list(
  A = diff_maps[["DECR"]],
  B = diff_maps[["INCR"]],
  C = diff_maps[["PROD"]],
  G = patchwork::guide_area()
)

p_map_diff_row <- wrap_plots(plots, design = design_2row_centerC) +
  plot_layout(
    guides = "collect",
    widths = c(1, 1, 1, 1, 0.45)  # 最後が凡例列（好みで調整）
  ) &
  theme(
    legend.position  = "right",
    legend.direction = "vertical"
  )


out_file_map_diff <- file.path(
  output_dir,
  paste0("Fig2_BII_map_diff_from_BASE_", map_year, ".png")
)
ggsave(out_file_map_diff, p_map_diff_row, width = 22, height = 10, dpi = 600, bg = "white")
message("Saved: ", out_file_map_diff)


# ======================================================
# AGMIP figures
# ======================================================
region_labels_agmip <- c(
  "R5REF" = "Reforming Economies",
  "R5OECD90+EU" = "OECD & EU",
  "R5ASIA" = "Asia",
  "R5MAF" = "Middle East & Africa",
  "R5LAM" = "Latin America",
  "World" = "World"
)

region_levels_agmip <- unname(region_labels_agmip[region_list])
scenario_levels_line <- c("BASE", "DECR", "INCR", "PROD")
scenario_levels_bar <- c("DECR", "INCR", "PROD")
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
land_item_order_agmip <- c("Cropland", "Pasture", "Forest", "Other Natural Land", "Urban")

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
    SCENARIO = factor(
      SCENARIO,
      levels = names(scenario_labels),
      labels = unname(scenario_labels)
    ),
    SCENARIO = factor(as.character(SCENARIO), levels = scenario_levels_line),
    region = factor(region_labels_agmip[region], levels = region_levels_agmip)
  ) %>%
  filter(!is.na(Y), Y >= 2015, Y <= 2100)

agmip_year_breaks <- seq(2020, 2100, by = 10)

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

# ---------------- Production and trade ----------------
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

g_prod_crop_fig3 <- g_prod_crop + labs(title = "Crop")
g_trade_crop_fig3 <- g_trade_crop + labs(title = "Crop")
g_prod_livestock_fig3 <- g_prod_livestock + labs(title = "Livestock products")
g_trade_livestock_fig3 <- g_trade_livestock + labs(title = "Livestock products")

make_fig3_label <- function(label_text) {
  ggplot() +
    annotate("text", x = 0, y = 0, label = label_text, hjust = 0, vjust = 0.5, size = 7, fontface = "bold") +
    coord_cartesian(xlim = c(0, 1), ylim = c(-1, 1), expand = FALSE) +
    theme_void()
}

fig3_label_prod <- make_fig3_label("a)")
fig3_label_trade <- make_fig3_label("b)")

fig3 <- wrap_plots(
  T = fig3_label_prod,
  U = fig3_label_trade,
  A = g_trade_crop_fig3,
  B = g_prod_crop_fig3,
  C = g_trade_livestock_fig3,
  D = g_prod_livestock_fig3,
  design = "
T#U
A#B
C#D
",
  guides = "collect",
  widths = c(1, 0.02, 1),
  heights = c(0.08, 1, 1)
)
fig3 <- fig3 & theme(
  legend.position = "bottom",
  plot.title = element_text(size = 20, face = "bold", hjust = 0)
)

out_file_fig3 <- file.path(output_dir, "Fig3_AGMIP_prod_trade.png")
ggsave(out_file_fig3, fig3, width = 13, height = 9.5, dpi = 220, bg = "white")
message("Saved: ", out_file_fig3)

# ---------------- Land and productivity ----------------
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

Land_agmip <- AGMIP_agg %>%
  filter(VAR == "LAND") %>%
  pivot_wider(names_from = ITEM, values_from = AGMIP_agg) %>%
  replace_na(list(CRP = 0, NLD = 0, GRS = 0, ONV = 0, FOR = 0, ECP = 0)) %>%
  select(SCENARIO, region, UNIT, Y, NLD, CRP, GRS, ONV, FOR) %>%
  pivot_longer(cols = c(NLD, CRP, GRS, ONV, FOR), names_to = "ITEM", values_to = "AGMIP") %>%
  mutate(
    AGMIP = AGMIP / 1000,
    ITEM = recode(ITEM, !!!land_item_labels_agmip),
    ITEM = factor(ITEM, levels = land_item_order_agmip),
    SCENARIO = factor(as.character(SCENARIO), levels = scenario_levels_line)
  )

Land_fig4_agmip <- Land_agmip %>%
  filter(region == "World", ITEM %in% c("Cropland", "Pasture", "Forest", "Other Natural Land")) %>%
  transmute(
    Scenario = factor(as.character(SCENARIO), levels = scenario_levels_line),
    Year = Y,
    ITEM = factor(as.character(ITEM), levels = c("Cropland", "Pasture", "Forest", "Other Natural Land")),
    Value = AGMIP
  )

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

g_yield_crop <- plot_world_series(
  AGMIP_agg %>% filter(VAR == "YILD", ITEM == "CRP", UNIT == "fm t/ha") %>%
    mutate(AGMIP_agg = AGMIP_agg * 1000),
  item_code = "CRP",
  plot_title = "Crop",
  y_label = "Land productivity [kg/ha]"
)

g_yield_livestock <- plot_world_series(
  AGMIP_agg %>% filter(VAR == "LYLD", ITEM == "LSP", UNIT == "kg prt/ha"),
  item_code = "LSP",
  plot_title = "Livestock products",
  y_label = "Land productivity [kg prt/ha]"
)

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

g_yield_crop_merged <- plot_productivity_series(
  merged_crop_productivity,
  plot_title = "Crop",
  y_label = "Production per cropland area [kg/ha]"
)

g_yield_livestock_merged <- plot_productivity_series(
  merged_pasture_productivity,
  plot_title = "Ruminant products",
  y_label = "Production per pasture area [kg/ha]"
)

make_fig4 <- function(land_data, productivity_crop, productivity_livestock) {
  g_land_crop <- plot_land_series(land_data, "Cropland")
  g_land_pasture <- plot_land_series(land_data, "Pasture")
  g_land_forest <- plot_land_series(land_data, "Forest")
  g_land_other <- plot_land_series(land_data, "Other Natural Land")
  productivity_crop <- productivity_crop + labs(title = "Crop")
  productivity_livestock <- productivity_livestock + labs(title = "Livestock products")

  fig4_label_land <- make_fig3_label("a)")
  fig4_label_prod <- make_fig3_label("b)")

  fig4 <- wrap_plots(
    T = fig4_label_land,
    U = fig4_label_prod,
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
  )
  fig4 <- fig4 & theme(legend.position = "bottom")
  fig4
}

fig4_agmip <- make_fig4(Land_fig4_agmip, g_yield_crop, g_yield_livestock)
out_file_fig4_agmip <- file.path(output_dir, "Fig4_AGMIP_land_productivity.png")
ggsave(out_file_fig4_agmip, fig4_agmip, width = 18, height = 11, dpi = 220, bg = "white")
message("Saved: ", out_file_fig4_agmip)

fig4_merged <- make_fig4(Land_fig4_merged, g_yield_crop_merged, g_yield_livestock_merged)
out_file_fig4_merged <- file.path(output_dir, "Fig4_Mergedcomparison_land_productivity.png")
ggsave(out_file_fig4_merged, fig4_merged, width = 18, height = 11, dpi = 220, bg = "white")
message("Saved: ", out_file_fig4_merged)

# ---------------- Regional production change ----------------
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
  filter(
    SCENARIO %in% scenario_levels_bar
  ) %>%
  mutate(
    ITEM = factor(ITEM, levels = c("Crop", "Livestock products", "Total")),
    SCENARIO = factor(as.character(SCENARIO), levels = scenario_levels_bar)
  )

fig5 <- prod_change_2050 %>%
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
  labs(y = "Change in production from BASE [million t]") +
  scale_fill_manual(values = agmip_item_colors[c("Crop", "Livestock products")], drop = FALSE) +
  scale_color_manual(values = c("Total" = agmip_item_colors[["Total"]])) +
  theme_1 +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    axis.title.y = element_text(size = 20),
    strip.text = element_text(size = 18),
    panel.border = element_rect(color = "grey60", fill = NA, linewidth = 0.5)
  )

out_file_fig5 <- file.path(output_dir, "Fig5_AGMIP_regional_production_change.png")
ggsave(out_file_fig5, fig5, width = 14, height = 8, dpi = 220, bg = "white")
message("Saved: ", out_file_fig5)

fig5_with_tag <- fig5 + plot_annotation(title = "c)")
fig5_with_tag <- fig5_with_tag & theme(
  plot.title = element_text(size = 20, face = "bold", hjust = 0)
)

fig3_fig5_combined <- wrap_plots(
  A = wrap_elements(full = fig3),
  B = wrap_elements(full = fig5_with_tag),
  ncol = 2,
  widths = c(1.2, 1.0)
)

out_file_fig3_fig5 <- file.path(output_dir, "Fig3_Fig5_combined.png")
ggsave(out_file_fig3_fig5, fig3_fig5_combined, width = 22, height = 10, dpi = 220, bg = "white")
message("Saved: ", out_file_fig3_fig5)


# ---------------- Regional price change ----------------
xprc_change_2100 <- AGMIP_agg %>%
  filter(VAR == "XPRC", ITEM == "AGR", UNIT == "USD/t", Y == 2100) %>%
  select(SCENARIO, region, AGMIP_agg) %>%
  left_join(
    AGMIP_agg %>%
      filter(VAR == "XPRC", ITEM == "AGR", UNIT == "USD/t", Y == 2100, SCENARIO == "BASE") %>%
      select(region, base_value = AGMIP_agg),
    by = "region"
  ) %>%
  mutate(
    change_from_base_pct = (AGMIP_agg - base_value) / base_value * 100,
    SCENARIO = factor(as.character(SCENARIO), levels = scenario_levels_bar),
    region = factor(as.character(region), levels = region_levels_agmip)
  ) %>%
  filter(SCENARIO %in% scenario_levels_bar)

fig7 <- ggplot(xprc_change_2100, aes(x = SCENARIO, y = change_from_base_pct, fill = SCENARIO)) +
  geom_hline(yintercept = 0, linewidth = 0.6) +
  geom_col(width = 0.75) +
  facet_wrap(~ region, scales = "free_y", ncol = 3) +
  labs(y = "Change in price from BASE in 2100 [%]") +
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

out_file_fig7 <- file.path(output_dir, "Fig7_AGMIP_regional_price_change_XPRC_AGR_2100.png")
ggsave(out_file_fig7, fig7, width = 14, height = 8.5, dpi = 220, bg = "white")
message("Saved: ", out_file_fig7)

xprc_line <- AGMIP_agg %>%
  filter(VAR == "XPRC", ITEM == "AGR", UNIT == "USD/t") %>%
  transmute(
    Scenario = factor(as.character(SCENARIO), levels = scenario_levels_line),
    Region = factor(as.character(region), levels = region_levels_agmip),
    Year = Y,
    Value = AGMIP_agg
  )

fig8 <- ggplot(xprc_line, aes(x = Year, y = Value, color = Scenario, group = Scenario)) +
  geom_line(linewidth = 1.2, na.rm = TRUE) +
  facet_wrap(~ Region, scales = "free_y", ncol = 3) +
  labs(y = "Price [USD/t]") +
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

out_file_fig8 <- file.path(output_dir, "Fig8_AGMIP_regional_price_timeseries_XPRC_AGR.png")
ggsave(out_file_fig8, fig8, width = 14, height = 8.5, dpi = 220, bg = "white")
message("Saved: ", out_file_fig8)


# ======================================================
# Regional facet figures
# ======================================================
regional_output_dir <- file.path(output_dir, "regional_facets")
dir.create(regional_output_dir, recursive = TRUE, showWarnings = FALSE)

regional_year_breaks <- seq(2020, 2100, by = 10)

save_regional_series_plot <- function(data, y_label, file_name, width = 14, height = 8) {
  p <- data %>%
    ggplot(aes(x = Year, y = Value, color = Scenario, group = Scenario)) +
    geom_line(linewidth = 1.2, na.rm = TRUE) +
    facet_wrap(~ Region, scales = "free_y", ncol = 3) +
    labs(y = y_label) +
    scale_color_manual(values = scenario_colors, drop = FALSE) +
    scale_x_continuous(breaks = regional_year_breaks, limits = c(min(regional_year_breaks), max(regional_year_breaks))) +
    theme_1 +
    theme(
      axis.text.x = element_text(size = 15, angle = 35, hjust = 1),
      axis.text.y = element_text(size = 15),
      axis.title.y = element_text(size = 18),
      strip.text = element_text(size = 17),
      legend.position = "bottom",
      panel.border = element_rect(color = "grey60", fill = NA, linewidth = 0.5)
    )

  out_path <- file.path(regional_output_dir, file_name)
  ggsave(out_path, p, width = width, height = height, dpi = 220, bg = "white")
  message("Saved: ", out_path)
}

agmip_regional_base <- AGMIP_agg %>%
  transmute(
    Scenario = factor(as.character(SCENARIO), levels = scenario_levels_line),
    Region = factor(as.character(region), levels = region_levels_agmip),
    VAR,
    ITEM,
    UNIT,
    Year = Y,
    Value = AGMIP_agg
  )

series_specs <- tibble::tribble(
  ~VAR,    ~ITEM,  ~UNIT,         ~scale_factor, ~y_label,                           ~file_name,
  "PROD",  "CRP",  "1000 t",      1000,          "Production [million t]",           "PROD_CRP_regional_facets.png",
  "PROD",  "LSP",  "1000 t",      1000,          "Production [million t]",           "PROD_LSP_regional_facets.png",
  "EXPO",  "CRP",  "1000 t",      1000,          "Export volume [million t]",        "EXPO_CRP_regional_facets.png",
  "EXPO",  "LSP",  "1000 t",      1000,          "Export volume [million t]",        "EXPO_LSP_regional_facets.png",
  "IMPO",  "CRP",  "1000 t",      1000,          "Import volume [million t]",        "IMPO_CRP_regional_facets.png",
  "IMPO",  "LSP",  "1000 t",      1000,          "Import volume [million t]",        "IMPO_LSP_regional_facets.png",
  "YILD",  "CRP",  "fm t/ha",     0.001,         "Land productivity [kg/ha]",        "YILD_CRP_regional_facets.png",
  "LYLD",  "LSP",  "kg prt/ha",   1,             "Land productivity [kg prt/ha]",    "LYLD_LSP_regional_facets.png"
)

purrr::pwalk(
  series_specs,
  function(VAR, ITEM, UNIT, scale_factor, y_label, file_name) {
    plot_data <- agmip_regional_base %>%
      filter(VAR == !!VAR, ITEM == !!ITEM, UNIT == !!UNIT) %>%
      mutate(Value = Value / scale_factor)

    save_regional_series_plot(plot_data, y_label = y_label, file_name = file_name)
  }
)

land_regional_specs <- tibble::tribble(
  ~item_name,              ~file_name,
  "Cropland",              "LAND_Cropland_regional_facets.png",
  "Pasture",               "LAND_Pasture_regional_facets.png",
  "Forest",                "LAND_Forest_regional_facets.png",
  "Other Natural Land",    "LAND_Other_Natural_Land_regional_facets.png"
)

land_regional_base <- Land_agmip %>%
  transmute(
    Scenario = factor(as.character(SCENARIO), levels = scenario_levels_line),
    Region = factor(as.character(region), levels = region_levels_agmip),
    ITEM = as.character(ITEM),
    Year = Y,
    Value = AGMIP
  )

purrr::pwalk(
  land_regional_specs,
  function(item_name, file_name) {
    plot_data <- land_regional_base %>%
      filter(ITEM == !!item_name)

    save_regional_series_plot(plot_data, y_label = "Area [million ha]", file_name = file_name)
  }
)

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
  file_name = "BII_regional_facets.png"
)


# ======================================================
# Country-level BII vs production scatter
# ======================================================
csv_path_for_bii <- function(scen, data_dir) {
  file.path(data_dir, "BII", "csv", paste0("BII_grid_", scen, "_default.csv"))
}

aggregate_bii_country_2100 <- function(csv_path, scenario_label, country_sf) {
  bii_grid <- read.csv(csv_path)
  bii_points <- st_as_sf(bii_grid, coords = c("lon", "lat"), crs = 4326, remove = FALSE)
  bii_joined <- st_join(
    bii_points,
    country_sf[, c("iso_a3", "name")],
    join = st_intersects,
    left = FALSE
  )

  bii_joined %>%
    st_drop_geometry() %>%
    filter(!is.na(iso_a3), iso_a3 != "-99") %>%
    mutate(weight = cos(lat * pi / 180)) %>%
    group_by(iso_a3) %>%
    summarise(BII_2100 = weighted.mean(v_2100, w = weight, na.rm = TRUE), .groups = "drop") %>%
    mutate(Scenario = scenario_label)
}

extract_agmip_country_prod_2100 <- function(gdx_path, scenarios) {
  agmip_sparse <- rgdx(gdx_path, list(name = "AGMIP", compress = TRUE))
  val <- agmip_sparse$val
  uels <- agmip_sparse$uels

  scenario_idx <- match(scenarios, uels[[1]])
  var_idx <- match("PROD", uels[[3]])
  item_idx <- match("AGR", uels[[4]])
  unit_idx <- match("1000 t", uels[[5]])
  year_idx <- match("2100", uels[[6]])

  keep <- val[, 1] %in% scenario_idx &
    val[, 3] == var_idx &
    val[, 4] == item_idx &
    val[, 5] == unit_idx &
    val[, 6] == year_idx

  filtered <- val[keep, , drop = FALSE]

  data.frame(
    Scenario_raw = uels[[1]][filtered[, 1]],
    Country = uels[[2]][filtered[, 2]],
    Value = filtered[, 7]
  ) %>%
    mutate(
      Scenario = factor(
        Scenario_raw,
        levels = names(scenario_labels),
        labels = unname(scenario_labels)
      )
    ) %>%
    select(Scenario, Country, Value)
}

bii_country_base <- lapply(
  names(scenario_labels),
  function(scen) {
    aggregate_bii_country_2100(
      csv_path_for_bii(scen, data_dir),
      scenario_label = scenario_labels[[scen]],
      country_sf = world_sf
    )
  }
) %>%
  bind_rows()

prod_country_2100 <- extract_agmip_country_prod_2100(
  paste0(data_dir, "AGMIP_all_scenario.gdx"),
  scenarios = names(scenario_labels)
)

country_overlap <- intersect(
  sort(unique(prod_country_2100$Country)),
  sort(unique(bii_country_base$iso_a3))
)

prod_country_change <- prod_country_2100 %>%
  filter(Country %in% country_overlap) %>%
  left_join(
    prod_country_2100 %>%
      filter(Scenario == "BASE") %>%
      rename(base_prod = Value) %>%
      select(Country, base_prod),
    by = "Country"
  ) %>%
  mutate(prod_change_pct = (Value - base_prod) / base_prod * 100) %>%
  filter(Scenario %in% c("DECR", "INCR", "PROD"))

bii_country_change <- bii_country_base %>%
  filter(iso_a3 %in% country_overlap) %>%
  left_join(
    bii_country_base %>%
      filter(Scenario == "BASE") %>%
      rename(base_bii = BII_2100) %>%
      select(iso_a3, base_bii),
    by = "iso_a3"
  ) %>%
  mutate(bii_change = BII_2100 - base_bii) %>%
  filter(Scenario %in% c("DECR", "INCR", "PROD"))

country_scatter <- prod_country_change %>%
  inner_join(
    bii_country_change,
    by = c("Scenario", "Country" = "iso_a3")
  )

p_country_scatter <- ggplot(
  country_scatter,
  aes(x = prod_change_pct, y = bii_change, color = Scenario)
) +
  geom_hline(yintercept = 0, linewidth = 0.5, color = "grey50") +
  geom_vline(xintercept = 0, linewidth = 0.5, color = "grey50") +
  geom_point(size = 2.8) +
  geom_text(aes(label = Country), nudge_y = 0.0015, size = 4, check_overlap = TRUE, show.legend = FALSE) +
  facet_wrap(~ Scenario, ncol = 3) +
  labs(
    x = "Change in AGR production from BASE [%]",
    y = "Change in BII from BASE [absolute index points]"
  ) +
  scale_color_manual(values = scenario_colors, drop = FALSE) +
  theme_1 +
  theme(
    axis.text.x = element_text(size = 14, angle = 0, hjust = 0.5),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    strip.text = element_text(size = 18),
    legend.position = "none",
    panel.border = element_rect(color = "grey60", fill = NA, linewidth = 0.5)
  )

out_file_country_scatter <- file.path(output_dir, "Fig6_country_scatter_AGRprod_vs_BII_2100.png")
ggsave(out_file_country_scatter, p_country_scatter, width = 14, height = 5.5, dpi = 220, bg = "white")
message("Saved: ", out_file_country_scatter)
