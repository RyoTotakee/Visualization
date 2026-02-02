library(ncdf4)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(viridis)
library(scales)
library(dplyr)

# -------------------------
# Config
# -------------------------
data_dir   <- "../data/"
output_dir <- "../output/"
nc_dir     <- file.path(data_dir, "BII", "nc")
plot_dir   <- file.path(output_dir, "plots")
if (!dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE)

varname <- "BII"

base_scenario <- "SSP2_BaU_NoCC_default"
compare_scenarios <- c(
  "SSP2_BaU_Reallocation_NoCC_default",
  "SSP2_BaU_TradeFacilitation_NoCC_default",
  "SSP2_BaU_TradeRestriction_NoCC_default"
)

map_year    <- 2100
delta_years <- c(2005, 2100)   # c(year1, year2)

# Plot scales
bii_limits   <- c(0.3, 1.0)
delta_limits <- c(-0.4, 0.6)
diff_limits  <- c(-0.4, 0.4)

# Antarctica cutoff (adjust if needed)
lat_min <- -60

# Coastlines (remove Antarctica)
world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(iso_a3 != "ATA")

# -------------------------
# Helpers
# -------------------------
nc_path_for <- function(scenario, nc_dir) {
  file.path(nc_dir, paste0("BIImap_", scenario, ".nc"))
}

guess_dim_index <- function(nc, varname) {
  v <- nc$var[[varname]]
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

get_time_index <- function(time_vals, year, tol = 1e-6) {
  idx <- which(abs(time_vals - year) < tol)
  if (length(idx) > 0) return(idx[1])
  idx2 <- which.min(abs(time_vals - year))
  warning(sprintf("Year %s not found exactly; using nearest time=%s", year, time_vals[idx2]))
  idx2
}

read_nc_lonlat_time <- function(nc_path, varname = "BII") {
  nc <- nc_open(nc_path)
  on.exit(nc_close(nc))
  
  lon <- ncvar_get(nc, "longitude")
  lat <- ncvar_get(nc, "latitude")
  time_vals <- ncvar_get(nc, "time")
  raw <- ncvar_get(nc, varname)
  
  idx <- guess_dim_index(nc, varname)
  arr <- aperm(raw, c(idx$lon, idx$lat, idx$time))  # (lon, lat, time)
  
  list(lon = lon, lat = lat, time = time_vals, arr = arr)
}

as_df_lonlat <- function(lon, lat, values, colname = "value") {
  df <- expand.grid(lon = lon, lat = lat)
  df[[colname]] <- as.vector(values)
  df
}

mask_antarctica <- function(df, lat_min = -60) {
  df$value[df$lat < lat_min] <- NA
  df
}

read_bii_at_year <- function(nc_path, year, varname = "BII", clamp01 = TRUE) {
  x <- read_nc_lonlat_time(nc_path, varname)
  ti <- get_time_index(x$time, year)
  slice <- x$arr[, , ti]
  if (clamp01) slice <- pmax(0, pmin(slice, 1))
  df <- as_df_lonlat(x$lon, x$lat, slice, "value")
  mask_antarctica(df, lat_min)
}

read_bii_delta_years <- function(nc_path, year1, year2, varname = "BII") {
  x <- read_nc_lonlat_time(nc_path, varname)
  t1 <- get_time_index(x$time, year1)
  t2 <- get_time_index(x$time, year2)
  delta <- x$arr[, , t2] - x$arr[, , t1]
  df <- as_df_lonlat(x$lon, x$lat, delta, "value")
  mask_antarctica(df, lat_min)
}

base_theme_min <- function() {
  theme_bw(base_size = 12) +
    theme(
      panel.background  = element_rect(fill = "white", colour = NA),
      plot.background   = element_rect(fill = "white", colour = NA),
      panel.grid        = element_blank(),
      panel.border      = element_rect(colour = "black", fill = NA, linewidth = 0.5),
      axis.title        = element_blank(),
      axis.text         = element_blank(),
      axis.ticks        = element_blank(),
      legend.position   = "right",
      legend.background = element_rect(fill = "white", colour = "grey80"),
      legend.key        = element_rect(fill = "white", colour = NA),
      plot.title        = element_text(face = "bold", hjust = 0.5)
    )
}

plot_raster_with_coast <- function(df, title, fill_scale, lat_min = -60) {
  ggplot() +
    geom_raster(data = df, aes(x = lon, y = lat, fill = value), na.rm = TRUE) +
    geom_sf(data = world, fill = NA, color = "grey20", linewidth = 0.3) +
    coord_sf(expand = FALSE, ylim = c(lat_min, 90)) +
    fill_scale +
    labs(title = title) +
    base_theme_min()
}

save_plot <- function(p, out_path, w = 9, h = 4.5, dpi = 300) {
  ggsave(out_path, p, width = w, height = h, dpi = dpi)
}

# -------------------------
# Main runner
# -------------------------
run_bii_outputs <- function(
    base_scenario,
    compare_scenarios,
    nc_dir,
    plot_dir,
    varname = "BII",
    map_year = 2100,
    delta_years = c(2005, 2100),
    bii_limits = c(0.3, 1),
    delta_limits = c(-0.4, 0.6),
    diff_limits = c(-0.4, 0.4),
    lat_min = -60
) {
  all_scenarios <- unique(c(base_scenario, compare_scenarios))
  
  # 1) Maps at map_year
  for (scen in all_scenarios) {
    nc_file <- nc_path_for(scen, nc_dir)
    if (!file.exists(nc_file)) {
      warning("Missing file: ", nc_file)
      next
    }
    
    df <- read_bii_at_year(nc_file, year = map_year, varname = varname, clamp01 = TRUE)
    
    p <- plot_raster_with_coast(
      df,
      title = paste0("BII (", map_year, ") - ", scen),
      fill_scale = scale_fill_viridis(
        option = "C",
        na.value = "grey95",
        name = "BII",
        limits = bii_limits
      ),
      lat_min = lat_min
    )
    
    out_png <- file.path(plot_dir, paste0("BII_map_", map_year, "_", scen, ".png"))
    save_plot(p, out_png)
  }
  
  # 2) Delta maps (year2 - year1)
  y1 <- delta_years[1]
  y2 <- delta_years[2]
  for (scen in all_scenarios) {
    nc_file <- nc_path_for(scen, nc_dir)
    if (!file.exists(nc_file)) {
      warning("Missing file: ", nc_file)
      next
    }
    
    df <- read_bii_delta_years(nc_file, year1 = y1, year2 = y2, varname = varname)
    
    p <- plot_raster_with_coast(
      df,
      title = paste0("ΔBII (", y2, " - ", y1, ") - ", scen),
      fill_scale = scale_fill_gradient2(
        low = "red", mid = "white", high = "blue",
        midpoint = 0,
        name = "ΔBII",
        limits = delta_limits,
        oob = squish,
        na.value = "grey95"   # <- unify background/NA color
      ),
      lat_min = lat_min
    )
    
    out_png <- file.path(plot_dir, paste0("BII_delta_", y2, "_minus_", y1, "_", scen, ".png"))
    save_plot(p, out_png)
  }
  
  # 3) Difference vs base at map_year (compare - base)
  base_file <- nc_path_for(base_scenario, nc_dir)
  if (!file.exists(base_file)) stop("Missing base file: ", base_file)
  
  df_base <- read_bii_at_year(base_file, year = map_year, varname = varname, clamp01 = TRUE)
  names(df_base)[names(df_base) == "value"] <- "base"
  
  for (scen in compare_scenarios) {
    nc_file <- nc_path_for(scen, nc_dir)
    if (!file.exists(nc_file)) {
      warning("Missing file: ", nc_file)
      next
    }
    
    df_cmp <- read_bii_at_year(nc_file, year = map_year, varname = varname, clamp01 = TRUE)
    names(df_cmp)[names(df_cmp) == "value"] <- "cmp"
    
    dfm <- merge(df_base, df_cmp, by = c("lon", "lat"), all = FALSE)
    dfm$value <- dfm$cmp - dfm$base
    dfm <- dfm[, c("lon", "lat", "value")]
    
    p <- plot_raster_with_coast(
      dfm,
      title = paste0("ΔBII (", map_year, ") : ", scen, " - ", base_scenario),
      fill_scale = scale_fill_gradient2(
        low = "red", mid = "white", high = "blue",
        midpoint = 0,
        name = "ΔBII",        # <- no scenario name in legend
        limits = diff_limits,
        oob = squish,
        na.value = "grey95"
      ),
      lat_min = lat_min
    )
    
    out_png <- file.path(plot_dir, paste0("BII_diff_", map_year, "_", scen, "_minus_", base_scenario, ".png"))
    save_plot(p, out_png)
  }
  
  invisible(TRUE)
}

# -------------------------
# Run
# -------------------------
run_bii_outputs(
  base_scenario     = base_scenario,
  compare_scenarios = compare_scenarios,
  nc_dir            = nc_dir,
  plot_dir          = plot_dir,
  varname           = varname,
  map_year          = map_year,
  delta_years       = delta_years,
  bii_limits        = bii_limits,
  delta_limits      = delta_limits,
  diff_limits       = diff_limits,
  lat_min           = lat_min
)
