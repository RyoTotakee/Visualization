# ============================================================
# NetCDF: シナリオ差 (Sce2 - Sce1) のマップ作成
# 指定した年×土地利用区分について、LC_area_share の差分を地図で保存
# 出力: ../output/<Sce2>_minus_<Sce1>/<landclass>/diff_<year>.png
# ============================================================

# ---- libraries ----
# install.packages(c("ncdf4", "ggplot2", "dplyr", "stringr", "sf", "rnaturalearth", "rnaturalearthdata", "scales"))
library(ncdf4)
library(ggplot2)
library(dplyr)
library(stringr)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(scales)

# ---- user settings ----
Sce1 <- "SSP2_BaU_NoCC"   # 引かれる側（基準）
Sce2 <- "SSP2_500C_CACN_DAC_NoCC"     # 引く側（比較）←例。実際の名前に変えてください

data_dir   <- "../data"
output_dir <- "../output"

# 比較したい年
plot_years <- c(2030, 2050, 2070, 2090)

# 土地利用区分（lc_class2 の「番号」or「名前」）
landuse_targets <- c("cropland_other", "grassland", "forest_all")

# 使う変数名
target_var <- "LC_area_share"

# カラーバー範囲（shareの差なので通常は[-1,1]で十分。必要なら変えてOK）
value_limits <- c(-1, 1)

# ---- helper: open nc ----
open_nc <- function(scenario) {
  nc_filename <- paste0("AIM-LUmap_", scenario, ".nc")
  nc_path <- file.path(data_dir, nc_filename)
  if (!file.exists(nc_path)) stop("NetCDF file not found: ", nc_path)
  nc_open(nc_path)
}

# ---- helper: parse lc_class2 labels from units attribute ----
parse_lc_class2_labels <- function(nc) {
  at <- ncatt_get(nc, "lc_class2")
  if (is.null(at$units) || is.na(at$units)) stop("lc_class2 units attribute not found.")
  parts <- str_split(at$units, "/", simplify = FALSE)[[1]]
  kv <- lapply(parts, function(p) {
    sp <- str_split(p, "=", n = 2, simplify = TRUE)
    idx <- as.integer(sp[1])
    lab <- sp[2]
    c(idx = idx, lab = lab)
  })
  idx <- as.integer(sapply(kv, `[[`, "idx"))
  lab <- as.character(sapply(kv, `[[`, "lab"))
  out <- idx
  names(out) <- lab
  out
}

resolve_landuse <- function(targets, label_map) {
  out <- list()
  for (t in targets) {
    if (is.numeric(t)) {
      idx <- as.integer(t)
      lab <- names(label_map)[match(idx, label_map)]
      if (is.na(lab)) lab <- paste0("lc_class2_", idx)
      out[[lab]] <- idx
    } else {
      if (!(t %in% names(label_map))) {
        stop("Unknown landuse label: ", t, "\nAvailable labels include e.g.: ",
             paste(head(names(label_map), 10), collapse = ", "), " ...")
      }
      out[[t]] <- as.integer(label_map[[t]])
    }
  }
  out
}

# ---- helper: time index ----
time_index <- function(time_vals, year) {
  idx <- match(year, time_vals)
  if (is.na(idx)) stop("Year not found in time dimension: ", year)
  idx
}

# ---- helper: read slice (lon,lat for one lc_class2 and one time) ----
read_slice <- function(nc, varname, lon_len, lat_len, lc_idx, t_idx) {
  # dims: lon,lat,lc_class2,time
  ncvar_get(nc, varname,
            start = c(1, 1, lc_idx, t_idx),
            count = c(lon_len, lat_len, 1, 1))
}

# ---- world boundaries as lon/lat paths (works with coord_equal) ----
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
world_ll <- sf::st_transform(world, 4326)
world_lines <- sf::st_cast(sf::st_boundary(world_ll), "MULTILINESTRING")

world_df <- sf::st_coordinates(world_lines) |>
  as.data.frame() |>
  dplyr::rename(lon = X, lat = Y, group = L1, piece = L2) |>
  dplyr::mutate(group = interaction(group, piece, drop = TRUE))

# ---- open both netcdfs ----
nc1 <- open_nc(Sce1)
on.exit(nc_close(nc1), add = TRUE)

nc2 <- open_nc(Sce2)
on.exit(nc_close(nc2), add = TRUE)

# ---- shared coords/time checks ----
lon1 <- nc1$dim$lon$vals; lat1 <- nc1$dim$lat$vals; time1 <- nc1$dim$time$vals
lon2 <- nc2$dim$lon$vals; lat2 <- nc2$dim$lat$vals; time2 <- nc2$dim$time$vals

if (is.null(lon1) || is.null(lat1) || is.null(time1)) stop("Sce1 missing lon/lat/time dim values.")
if (is.null(lon2) || is.null(lat2) || is.null(time2)) stop("Sce2 missing lon/lat/time dim values.")

# 厳密一致チェック（ずれてると差分が意味を失うので止める）
if (!isTRUE(all.equal(lon1, lon2))) stop("lon grids differ between Sce1 and Sce2.")
if (!isTRUE(all.equal(lat1, lat2))) stop("lat grids differ between Sce1 and Sce2.")
if (!isTRUE(all.equal(time1, time2))) stop("time values differ between Sce1 and Sce2.")

lon <- lon1; lat <- lat1; time_vals <- time1

# ---- landuse mapping (Sce1から取得。Sce2も同一想定) ----
lc_map <- parse_lc_class2_labels(nc1)
landuse_list <- resolve_landuse(landuse_targets, lc_map)

# ---- output root ----
diff_tag <- paste0(Sce2, "_minus_", Sce1)
root_out <- file.path(output_dir, diff_tag)
dir.create(root_out, recursive = TRUE, showWarnings = FALSE)

# ---- plot + save ----
save_diff_map <- function(diff_mat, year, lc_label, lc_idx) {
  df <- expand.grid(lon = lon, lat = lat) |>
    mutate(value = as.vector(diff_mat))
  
  p <- ggplot(df, aes(x = lon, y = lat, fill = value)) +
    geom_raster() +
    geom_path(
      data = world_df,
      aes(x = lon, y = lat, group = group),
      inherit.aes = FALSE,
      linewidth = 0.25
    ) +
    coord_equal(expand = FALSE) +
    scale_fill_gradient2(
      limits = value_limits,
      oob = scales::squish
    ) +
    labs(
      title = paste0("Scenario diff: ", Sce2, " - ", Sce1, " | ", target_var, " in ", year),
      subtitle = paste0("lc_class2=", lc_idx, " (", lc_label, ")"),
      x = "Longitude", y = "Latitude", fill = "diff"
    ) +
    theme_classic(base_size = 11) +
    theme(
      plot.background  = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      legend.background = element_rect(fill = "white", color = NA)
    )
  
  class_dir <- file.path(root_out, lc_label)
  dir.create(class_dir, recursive = TRUE, showWarnings = FALSE)
  
  out_file <- file.path(class_dir, sprintf("diff_%d.png", year))
  ggsave(out_file, p, width = 12, height = 6, dpi = 200, bg = "white")
  message("Saved: ", out_file)
}

# ---- main loop ----
lon_len <- length(lon)
lat_len <- length(lat)

for (lc_label in names(landuse_list)) {
  lc_idx <- landuse_list[[lc_label]]
  message("\n== Landuse: ", lc_label, " (", lc_idx, ") ==")
  
  for (yr in plot_years) {
    t_idx <- time_index(time_vals, yr)
    
    m1 <- read_slice(nc1, target_var, lon_len, lat_len, lc_idx, t_idx)
    m2 <- read_slice(nc2, target_var, lon_len, lat_len, lc_idx, t_idx)
    
    diff_mat <- m2 - m1
    save_diff_map(diff_mat, yr, lc_label, lc_idx)
  }
}

message("\nDone.")
