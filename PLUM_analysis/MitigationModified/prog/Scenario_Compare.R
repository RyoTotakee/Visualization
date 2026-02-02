# ============================================================
# NetCDF: シナリオ差 (Sce - BaseSce) のマップ作成（複数Sce対応）
# 指定した年×土地利用区分について、LC_area_share の差分を地図で保存
# 出力: ../output/<Sce>_minus_<BaseSce>/<landclass>/diff_<year>.png
# ============================================================

# ---- libraries ----
library(ncdf4)
library(ggplot2)
library(dplyr)
library(stringr)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(scales)

# ---- user settings ----
BaseSce <- "SSP2_BaU_NoCC"   # 固定の基準（引かれる側）

# ★比較したいシナリオを複数並べる（引く側）
Scenarios <- c(
  "SSP2_BaU_Reallocation_NoCC",
  "SSP2_BaU_TradeFacilitation_NoCC",
  "SSP2_BaU_TradeRestriction_NoCC"
)

data_dir   <- "../data"
output_dir <- "../output"

plot_years <- c(2030, 2050, 2070, 2090)
landuse_targets <- c("cropland_other", "grassland", "forest_all")
target_var <- "LC_area_share"
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

# ---- helper: read slice ----
read_slice <- function(nc, varname, lon_len, lat_len, lc_idx, t_idx) {
  ncvar_get(nc, varname,
            start = c(1, 1, lc_idx, t_idx),
            count = c(lon_len, lat_len, 1, 1))
}

# ---- world boundaries as lon/lat paths ----
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
world_ll <- sf::st_transform(world, 4326)
world_lines <- sf::st_cast(sf::st_boundary(world_ll), "MULTILINESTRING")

world_df <- sf::st_coordinates(world_lines) |>
  as.data.frame() |>
  dplyr::rename(lon = X, lat = Y, group = L1, piece = L2) |>
  dplyr::mutate(group = interaction(group, piece, drop = TRUE))

# ============================================================
# 1) BaseSce を開く（固定）
# ============================================================
nc_base <- open_nc(BaseSce)
on.exit(nc_close(nc_base), add = TRUE)

lon_b <- nc_base$dim$lon$vals
lat_b <- nc_base$dim$lat$vals
time_b <- nc_base$dim$time$vals

if (is.null(lon_b) || is.null(lat_b) || is.null(time_b)) stop("BaseSce missing lon/lat/time dim values.")

lon <- lon_b
lat <- lat_b
time_vals <- time_b
lon_len <- length(lon)
lat_len <- length(lat)

# landuse mapping は BaseSce から取得（他も同一想定）
lc_map <- parse_lc_class2_labels(nc_base)
landuse_list <- resolve_landuse(landuse_targets, lc_map)

# ============================================================
# 2) Sce を複数回す
# ============================================================
for (Sce in Scenarios) {
  
  message("\n==============================")
  message("Scenario diff: ", Sce, " - ", BaseSce)
  message("==============================")
  
  # ---- open compare nc ----
  # ファイルが無い場合は落ちずにスキップしたいなら warning+next に変えてOK
  nc_cmp <- open_nc(Sce)
  on.exit(nc_close(nc_cmp), add = TRUE)
  
  # ---- shared coords/time checks ----
  lon2 <- nc_cmp$dim$lon$vals; lat2 <- nc_cmp$dim$lat$vals; time2 <- nc_cmp$dim$time$vals
  if (is.null(lon2) || is.null(lat2) || is.null(time2)) stop("Scenario missing lon/lat/time dim values: ", Sce)
  
  if (!isTRUE(all.equal(lon, lon2))) stop("lon grids differ between BaseSce and ", Sce)
  if (!isTRUE(all.equal(lat, lat2))) stop("lat grids differ between BaseSce and ", Sce)
  if (!isTRUE(all.equal(time_vals, time2))) stop("time values differ between BaseSce and ", Sce)
  
  # ---- output root ----
  diff_tag <- paste0(Sce, "_minus_", BaseSce)
  root_out <- file.path(output_dir, diff_tag)
  dir.create(root_out, recursive = TRUE, showWarnings = FALSE)
  
  # ---- plot + save (Sceごとに root_out / Sce を使う) ----
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
        title = paste0("Scenario diff: ", Sce, " - ", BaseSce, " | ", target_var, " in ", year),
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
  for (lc_label in names(landuse_list)) {
    lc_idx <- landuse_list[[lc_label]]
    message("\n== Landuse: ", lc_label, " (", lc_idx, ") ==")
    
    for (yr in plot_years) {
      t_idx <- time_index(time_vals, yr)
      
      m_base <- read_slice(nc_base, target_var, lon_len, lat_len, lc_idx, t_idx)
      m_sce  <- read_slice(nc_cmp,  target_var, lon_len, lat_len, lc_idx, t_idx)
      
      diff_mat <- m_sce - m_base
      save_diff_map(diff_mat, yr, lc_label, lc_idx)
    }
  }
  
  # 明示的に閉じたい場合はこれを使って、上の on.exit(nc_close(nc_cmp), add=TRUE) を消してOK
  # nc_close(nc_cmp)
}

message("\nDone.")
