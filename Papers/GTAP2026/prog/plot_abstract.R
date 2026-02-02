library(dplyr)
library(ggplot2)
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
data_dir   <- "../data/0106/"
output_dir <- "../output/0106/"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# ---------------- scenario labels/order ----------------
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
  theme(panel.border = element_rect(color = "grey60", fill = NA, linewidth = 0.5))

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
    axis.text.x = element_text(angle = 45, hjust = 1),
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
diff_limits_map <- c(-0.4, 0.4)

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
