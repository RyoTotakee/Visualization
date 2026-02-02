library(ncdf4)
library(ggplot2)
library(dplyr)
library(rnaturalearth)
library(rnaturalearthdata)
library(viridis)

# ディレクトリ設定（ユーザー指定）
data_dir   <- "../data/"
output_dir <- "../output/"

# NetCDF があるフォルダ（必要に応じて変更）
nc_dir <- file.path(data_dir, "BII", "nc")

# プロットの出力先
plot_dir <- file.path(output_dir, "plots")
if (!dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE)

# シナリオ一覧
scenarios <- c(
  "SSP2_BaU_NoCC_default",
  "SSP2_BaU_NoCC_afr_as_timber",
  "SSP2_BaU_NoCC_afr_as_primf",
  "SSP2_500C_CACN_DAC_NoCC_default",
  "SSP2_500C_CACN_DAC_NoCC_afr_as_timber",
  "SSP2_500C_CACN_DAC_NoCC_afr_as_primf"
)


# 世界地図（海岸線）
world <- ne_countries(scale = "medium", returnclass = "sf")

read_bii_nc <- function(nc_path, varname = "BII", t_index = NULL) {
  nc <- nc_open(nc_path)
  on.exit(nc_close(nc))
  
  lon <- ncvar_get(nc, "longitude")
  lat <- ncvar_get(nc, "latitude")
  bii_raw <- ncvar_get(nc, varname)
  
  dims <- dim(bii_raw)
  
  # 次元順の推定
  lon_dim <- which(dims == length(lon))[1]
  lat_dim <- which(dims == length(lat))[1]
  time_dim <- setdiff(seq_along(dims), c(lon_dim, lat_dim))[1]
  
  bii_ll_t <- aperm(bii_raw, c(lon_dim, lat_dim, time_dim))
  
  # t_index が指定されていなければ「最後のスライス（おそらく2100年）」を使う
  if (is.null(t_index)) {
    t_index <- dim(bii_ll_t)[3]
  }
  
  bii_slice <- bii_ll_t[, , t_index]
  
  df <- expand.grid(lon = lon, lat = lat)
  df$BII <- as.vector(bii_slice)
  
  df
}


# ---- メインループ：全シナリオを読み込み & プロット ----
for (scen in scenarios) {
  nc_file <- file.path(nc_dir, paste0("BIImap_", scen, ".nc"))
  
  df <- read_bii_nc(nc_file, varname = "BII", t_index = 11) # 2100年
  
  p <- ggplot() +
    # BII ラスタ
    geom_raster(
      data = df,
      aes(x = lon, y = lat, fill = BII),
      na.rm = TRUE
    ) +
    # 海岸線（少し太め & 濃いめ）
    geom_sf(
      data = world,
      fill = NA,
      color = "grey20",
      linewidth = 0.3
    ) +
    coord_sf(expand = FALSE) +
    # BII のカラースケール（0〜1を想定）
    scale_fill_viridis(
      option = "C",
      na.value = "grey95",     # 欠損は薄いグレー
      name = "BII",
      limits = c(0.3, 1)         # 必要に応じて調整
    ) +
    labs(
      title = paste("Biodiversity Intactness Index (BII)", scen),
      x = "Longitude",
      y = "Latitude"
    ) +
    theme_bw(base_size = 12) +
    theme(
      # プロット背景を真っ白に
      panel.background = element_rect(fill = "white", colour = NA),
      plot.background  = element_rect(fill = "white", colour = NA),
      # グリッド線は薄めに
      panel.grid.major = element_line(colour = "grey90", linewidth = 0.3),
      panel.grid.minor = element_line(colour = "grey95", linewidth = 0.2),
      # 枠線をはっきり
      panel.border     = element_rect(colour = "black", fill = NA, linewidth = 0.5),
      # 凡例を見やすく
      legend.position  = "right",
      legend.background = element_rect(fill = "white", colour = "grey80"),
      legend.key       = element_rect(fill = "white", colour = NA),
      # タイトルを少し強調
      plot.title = element_text(face = "bold", hjust = 0.5)
    )
  
  out_png <- file.path(plot_dir, paste0("BII_map_", scen, ".png"))
  ggsave(out_png, p, width = 9, height = 4.5, dpi = 300)
  
}

read_bii_delta_years <- function(nc_path, varname = "BII",
                                 year1 = 2005, year2 = 2100) {
  nc <- nc_open(nc_path)
  on.exit(nc_close(nc))
  
  lon <- ncvar_get(nc, "longitude")
  lat <- ncvar_get(nc, "latitude")
  bii_raw <- ncvar_get(nc, varname)  # 3次元: lon/lat/time のどれかの順
  
  # time からインデックスを特定
  time_vals <- ncvar_get(nc, "time")
  idx1 <- which(time_vals == year1)
  idx2 <- which(time_vals == year2)
  if (length(idx1) == 0 || length(idx2) == 0) {
    stop("指定した年が time にありません: ", year1, ", ", year2)
  }
  
  dims <- dim(bii_raw)
  
  # 次元順の推定（lon / lat / time の位置）
  lon_dim  <- which(dims == length(lon))[1]
  lat_dim  <- which(dims == length(lat))[1]
  time_dim <- setdiff(seq_along(dims), c(lon_dim, lat_dim))[1]
  
  # (lon, lat, time) の順に並べ替え
  bii_ll_t <- aperm(bii_raw, c(lon_dim, lat_dim, time_dim))
  
  # 各年のスライス
  bii1 <- bii_ll_t[, , idx1]
  bii2 <- bii_ll_t[, , idx2]
  
  # 差分（year2 - year1）
  delta <- bii2 - bii1
  
  # df 化
  df <- expand.grid(lon = lon, lat = lat)
  df$delta <- as.vector(delta)
  df
}

for (scen in scenarios) {
  nc_file <- file.path(nc_dir, paste0("BIImap_", scen, ".nc"))
  
  # 2100年 - 2005年 の差分を取得
  df_delta <- read_bii_delta_years(
    nc_path = nc_file,
    varname = "BII",
    year1   = 2005,
    year2   = 2100
  )
  
  p_delta <- ggplot() +
    geom_raster(
      data = df_delta,
      aes(x = lon, y = lat, fill = delta),
      na.rm = TRUE
    ) +
    geom_sf(
      data = world,
      fill = NA,
      color = "grey20",
      linewidth = 0.3
    ) +
    coord_sf(expand = FALSE) +
    # 差分なので ± 方向が分かるように両側カラー
    scale_fill_gradient2(
      low  = "blue",
      mid  = "white",
      high = "red",
      midpoint = 0,
      name = "ΔBII (2100 - 2005)",
      limits = c(-0.4, 0.6),      # ★ ここでスケール固定
      oob = scales::squish       # 範囲外は端に張り付ける（任意だけどおすすめ）
    ) +
    labs(
      title = paste("Change in BII (2100 - 2005)", scen),
      x = "Longitude",
      y = "Latitude"
    ) +
    theme_bw(base_size = 12) +
    theme(
      panel.background = element_rect(fill = "white", colour = NA),
      plot.background  = element_rect(fill = "white", colour = NA),
      panel.grid.major = element_line(colour = "grey90", linewidth = 0.3),
      panel.grid.minor = element_line(colour = "grey95", linewidth = 0.2),
      panel.border     = element_rect(colour = "black", fill = NA, linewidth = 0.5),
      legend.position  = "right",
      legend.background = element_rect(fill = "white", colour = "grey80"),
      legend.key       = element_rect(fill = "white", colour = NA),
      plot.title = element_text(face = "bold", hjust = 0.5)
    )
  
  out_png_delta <- file.path(plot_dir, paste0("BII_delta_map_2100_2005_", scen, ".png"))
  ggsave(out_png_delta, p_delta, width = 9, height = 4.5, dpi = 300)
}


read_bii_at_year <- function(nc_path, varname = "BII", year = 2100) {
  nc <- nc_open(nc_path)
  on.exit(nc_close(nc))
  
  lon <- ncvar_get(nc, "longitude")
  lat <- ncvar_get(nc, "latitude")
  bii_raw   <- ncvar_get(nc, varname)
  time_vals <- ncvar_get(nc, "time")
  
  idx <- which(time_vals == year)
  if (length(idx) == 0) {
    stop("指定した年が time にありません: ", year)
  }
  
  dims <- dim(bii_raw)
  
  # 次元順の推定
  lon_dim  <- which(dims == length(lon))[1]
  lat_dim  <- which(dims == length(lat))[1]
  time_dim <- setdiff(seq_along(dims), c(lon_dim, lat_dim))[1]
  
  # (lon, lat, time) に並べ替え
  bii_ll_t <- aperm(bii_raw, c(lon_dim, lat_dim, time_dim))
  
  bii_slice <- bii_ll_t[, , idx]
  
  # ★ BII を [0, 1] に制限（上限だけでよければ pmin(bii_slice, 1) だけでもOK）
  bii_slice <- pmax(0, pmin(bii_slice, 1))
  
  df <- expand.grid(lon = lon, lat = lat)
  df$BII <- as.vector(bii_slice)
  
  df
}


bau_scen <- c(
  "SSP2_BaU_NoCC_default",
  "SSP2_BaU_NoCC_afr_as_timber",
  "SSP2_BaU_NoCC_afr_as_primf"
)

c500_scen <- c(
  "SSP2_500C_CACN_DAC_NoCC_default",
  "SSP2_500C_CACN_DAC_NoCC_afr_as_timber",
  "SSP2_500C_CACN_DAC_NoCC_afr_as_primf"
)

for (i in seq_along(bau_scen)) {
  scen_bau  <- bau_scen[i]
  scen_500C <- c500_scen[i]
  
  nc_bau  <- file.path(nc_dir, paste0("BIImap_", scen_bau,  ".nc"))
  nc_500C <- file.path(nc_dir, paste0("BIImap_", scen_500C, ".nc"))
  
  # 2100年のBIIを取得
  df_bau  <- read_bii_at_year(nc_bau,  varname = "BII", year = 2100)
  df_500C <- read_bii_at_year(nc_500C, varname = "BII", year = 2100)
  
  # lon, lat で結合（同じグリッド想定だが、安全のため merge）
  df_merged <- merge(
    df_bau, df_500C,
    by = c("lon", "lat"),
    suffixes = c("_BaU", "_500C")
  )
  
  # 差分（500C - BaU）
  df_merged$diff <- df_merged$BII_500C - df_merged$BII_BaU
  
  # プロット（差分）
  p_diff <- ggplot() +
    geom_raster(
      data = df_merged,
      aes(x = lon, y = lat, fill = diff),
      na.rm = TRUE
    ) +
    geom_sf(
      data = world,
      fill = NA,
      color = "grey20",
      linewidth = 0.3
    ) +
    coord_sf(expand = FALSE) +
    scale_fill_gradient2(
      low  = "blue",
      mid  = "white",
      high = "red",
      midpoint = 0,
      limits = c(-0.4, 0.6),      # ★ ここでスケール固定
      oob = scales::squish,       # 範囲外は端に張り付ける（任意だけどおすすめ）
      name = "ΔBII (500C - BaU)",
      na.value = "grey95"
    )+
    labs(
      title = paste("Difference in BII (2100)", "\n500C - BaU:", scen_bau),
      x = "Longitude",
      y = "Latitude"
    ) +
    theme_bw(base_size = 12) +
    theme(
      panel.background = element_rect(fill = "white", colour = NA),
      plot.background  = element_rect(fill = "white", colour = NA),
      panel.grid.major = element_line(colour = "grey90", linewidth = 0.3),
      panel.grid.minor = element_line(colour = "grey95", linewidth = 0.2),
      panel.border     = element_rect(colour = "black", fill = NA, linewidth = 0.5),
      legend.position  = "right",
      legend.background = element_rect(fill = "white", colour = "grey80"),
      legend.key       = element_rect(fill = "white", colour = NA),
      plot.title = element_text(face = "bold", hjust = 0.5)
    )
  
  out_png_diff <- file.path(
    plot_dir,
    paste0("BII_diff_2100_500C_minus_BaU_", scen_bau, ".png")
  )
  ggsave(out_png_diff, p_diff, width = 9, height = 4.5, dpi = 300)
}





