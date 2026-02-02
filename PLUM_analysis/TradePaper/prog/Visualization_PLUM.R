# ============================================================
# NetCDF: LC_area_share の差分マップ作成
# 2005年を基準に、指定した将来年の(将来 - 2005)を地図で保存
# 出力: ../output/<scenario>/<landclass>/delta_<year>_minus_2005.png
# ============================================================

# ---- libraries ----
# install.packages(c("ncdf4", "ggplot2", "dplyr", "stringr"))
library(ncdf4)
library(ggplot2)
library(dplyr)
library(stringr)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(scales)
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
world_ll <- sf::st_transform(world, 4326)  # lon/lat (WGS84)
world_lines <- sf::st_cast(sf::st_boundary(world_ll), "MULTILINESTRING")

# sfをdata.frame化（lon/latの線データに）
world_df <- sf::st_coordinates(world_lines) |>
  as.data.frame() |>
  dplyr::rename(lon = X, lat = Y, group = L1, piece = L2) |>
  dplyr::mutate(group = interaction(group, piece, drop = TRUE))



# ---- user settings ----
scenario <- "SSP2_500C_CACN_DAC_NoCC"

data_dir   <- "../data"
output_dir <- "../output"

# 作りたい年（基準年は base_year に固定）
base_year <- 2005
plot_years <- c(2030, 2050, 2070, 2090)

# 土地利用区分の指定（lc_class2 の「番号」または「名前」どちらでもOK）
# 例: 番号で指定
# landuse_targets <- c(1, 2, 17)
# 例: 名前で指定（units から自動抽出されるラベルと一致させる）
landuse_targets <- c("cropland_other", "grassland", "forest_all")
# "1=cropland_other/2=cropland_2Gbioen/3=grassland/4=forest_unmanaged/5=forest_managed
# /6=restored/7=other/8=built-up/9=abn_cropland_other/10=abn_cropland_2Gbioen/11=abn_grassland
# /12=abn_forest_managed/13=forest_mng_nat/14=forest_mng_planted/15=AR/16=agroforestry
# /17=forest_all/18=Deint-agr/19=deint-cl/20=deint-pas"

# 使う変数名（必要ならここだけ変える）
target_var <- "LC_area_share"

# ---- path + open nc ----
nc_filename <- paste0("AIM-LUmap_", scenario, ".nc")
nc_path <- file.path(data_dir, nc_filename)
if (!file.exists(nc_path)) stop("NetCDF file not found: ", nc_path)

nc <- nc_open(nc_path)
on.exit(nc_close(nc), add = TRUE)

# ---- helper: parse lc_class2 labels from units attribute ----
parse_lc_class2_labels <- function(nc) {
  at <- ncatt_get(nc, "lc_class2")
  if (is.null(at$units) || is.na(at$units)) stop("lc_class2 units attribute not found.")
  # "1=name/2=name/..." をパース
  parts <- str_split(at$units, "/", simplify = FALSE)[[1]]
  kv <- lapply(parts, function(p) {
    sp <- str_split(p, "=", n = 2, simplify = TRUE)
    idx <- as.integer(sp[1])
    lab <- sp[2]
    c(idx = idx, lab = lab)
  })
  idx <- as.integer(sapply(kv, `[[`, "idx"))
  lab <- as.character(sapply(kv, `[[`, "lab"))
  # names付きベクトル: names = label, value = index
  out <- idx
  names(out) <- lab
  out
}

# ---- helper: resolve landuse targets (names or indices) ----
resolve_landuse <- function(targets, label_map) {
  # label_map: named int vector (names = label, value = index)
  out <- list()
  for (t in targets) {
    if (is.numeric(t)) {
      idx <- as.integer(t)
      lab <- names(label_map)[match(idx, label_map)]
      if (is.na(lab)) lab <- paste0("lc_class2_", idx)
      out[[lab]] <- idx
    } else {
      # character label
      if (!(t %in% names(label_map))) {
        stop("Unknown landuse label: ", t, "\nAvailable labels include e.g.: ",
             paste(head(names(label_map), 10), collapse = ", "), " ...")
      }
      out[[t]] <- as.integer(label_map[[t]])
    }
  }
  out
}

# ---- helper: find time index ----
time_vals <- nc$dim$time$vals
if (is.null(time_vals)) stop("time values not found in nc$dim$time$vals")

time_index <- function(year) {
  idx <- match(year, time_vals)
  if (is.na(idx)) stop("Year not found in time dimension: ", year)
  idx
}

# ---- coords ----
lon <- nc$dim$lon$vals
lat <- nc$dim$lat$vals
if (is.null(lon) || is.null(lat)) stop("lon/lat values not found in nc$dim.")

# ---- lc_class2 map ----
lc_map <- parse_lc_class2_labels(nc)
landuse_list <- resolve_landuse(landuse_targets, lc_map)

# ---- output root ----
scenario_out <- file.path(output_dir, scenario)
dir.create(scenario_out, recursive = TRUE, showWarnings = FALSE)

# ---- read slice function (lon,lat for one lc_class2 and one time) ----
read_lc_share_slice <- function(nc, varname, lc_idx, t_idx) {
  # var dims are lon,lat,lc_class2,time
  ncvar_get(
    nc, varname,
    start = c(1, 1, lc_idx, t_idx),
    count = c(length(lon), length(lat), 1, 1)
  )
}

save_delta_map <- function(delta_mat, year, lc_label, lc_idx) {
  df <- expand.grid(lon = lon, lat = lat) |>
    mutate(value = as.vector(delta_mat))
  
  p <- ggplot(df, aes(x = lon, y = lat, fill = value)) +
    geom_raster() +
    # 国境線・海岸線（lineデータなのでcoord_equalでOK）
    geom_path(
      data = world_df,
      aes(x = lon, y = lat, group = group),
      inherit.aes = FALSE,
      linewidth = 0.25
    ) +
    coord_equal(expand = FALSE) +
    scale_fill_gradient2(
      limits = c(-1, 1),
      oob = scales::squish
    ) +
    labs(
      title = paste0("Δ ", target_var, " (", year, " - ", base_year, ")"),
      subtitle = paste0("Scenario: ", scenario, " | lc_class2=", lc_idx, " (", lc_label, ")"),
      x = "Longitude", y = "Latitude", fill = "Δ share"
    ) +
    theme_classic(base_size = 11) +
    theme(
      plot.background  = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      legend.background = element_rect(fill = "white", color = NA)
    )
  
  class_dir <- file.path(scenario_out, lc_label)
  dir.create(class_dir, recursive = TRUE, showWarnings = FALSE)
  
  out_file <- file.path(class_dir, sprintf("delta_%d_minus_%d.png", year, base_year))
  ggsave(out_file, p, width = 12, height = 6, dpi = 200, bg = "white")
  message("Saved: ", out_file)
}



# ---- main loop ----
base_t <- time_index(base_year)

for (lc_label in names(landuse_list)) {
  lc_idx <- landuse_list[[lc_label]]
  
  message("\n== Landuse: ", lc_label, " (", lc_idx, ") ==")
  
  base_mat <- read_lc_share_slice(nc, target_var, lc_idx, base_t)
  
  for (yr in plot_years) {
    t_idx <- time_index(yr)
    fut_mat <- read_lc_share_slice(nc, target_var, lc_idx, t_idx)
    
    delta <- fut_mat - base_mat
    save_delta_map(delta, yr, lc_label, lc_idx)
  }
}

