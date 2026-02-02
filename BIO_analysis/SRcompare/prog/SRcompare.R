library(terra)
library(ggplot2)
library(grid)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

coast_sf     <- rnaturalearth::ne_coastline(scale = "medium", returnclass = "sf")
countries_sf <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

# =========================
# ユーザー設定（ここだけ触ればOK）
# =========================
Sce1 <- "Baseline_LU_Fmig"        # ベースライン（あとで自由に変更）
Sce2 <- "Mitigaion_LU_Fmig"  # 比較対象（あとで自由に変更）

sep_sce <- "_"                   # シナリオ名の区切り（例が_なので既定は_。ハイフンなら "-"）
data_dir <- "../data"
out_dir  <- "../output"

years <- c(2030, 2050, 2070, 2090)
taxonlist <- c("Plantae","Amphibia","Reptilia","Aves","Mammalia","all_taxon")


# =========================
# 出力フォルダ作成
# =========================
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

dir_raw   <- file.path(out_dir, "01_raw_SPnumber")
dir_dcur  <- file.path(out_dir, "02_delta_from_Current")
dir_sdiff <- file.path(out_dir, "03_Sce2_minus_Sce1")
dir_sdiff_ratio <- file.path(out_dir, "04_Sce2_minus_Sce1_div_Sce1Current")

dir.create(dir_raw,   showWarnings = FALSE, recursive = TRUE)
dir.create(dir_dcur,  showWarnings = FALSE, recursive = TRUE)
dir.create(dir_sdiff, showWarnings = FALSE, recursive = TRUE)
dir.create(dir_sdiff_ratio, showWarnings = FALSE, recursive = TRUE)



# =========================
# ヘルパー関数
# =========================
parse_sce_tokens <- function(sce, sep = "_") {
  tokens <- strsplit(sce, split = sep, fixed = TRUE)[[1]]
  if (length(tokens) < 2) {
    stop(paste0("シナリオ名 '", sce, "' は少なくとも2要素（例: Baseline_LU_...）が必要です。sep=", sep))
  }
  list(
    tokens = tokens,
    clim = tokens[1],
    lu   = tokens[2]
  )
}

future_path <- function(sce, taxon, year, sep = "_", data_dir = "../data") {
  p <- parse_sce_tokens(sce, sep = sep)
  # 例: data/Baseline_LU_Fmig/SPnumber/SPnumber_Mammalia_Baseline_2050_LU.tif
  file.path(data_dir, sce, "SPnumber",
            paste0("SPnumber_", taxon, "_", p$clim, "_", year, "_", p$lu, ".tif"))
}

current_path <- function(sce, taxon, data_dir = "../data") {
  # 例: data/Baseline_LU_Fmig/SPnumber/SPnumber_Mammalia_Current.tif
  file.path(data_dir, sce, "SPnumber",
            paste0("SPnumber_", taxon, "_Current.tif"))
}

read_rast_safe <- function(path) {
  if (!file.exists(path)) {
    message("[SKIP] not found: ", path)
    return(NULL)
  }
  rast(path)
}

sym_limits <- function(r) {
  # 差分用に 0 を中心とした対称limitsを作る
  mm <- terra::global(r, fun = c("min", "max"), na.rm = TRUE)
  if (is.null(mm) || any(is.na(mm))) return(NULL)
  m <- max(abs(as.numeric(mm[1, 1])), abs(as.numeric(mm[1, 2])))
  if (!is.finite(m) || m == 0) return(NULL)
  c(-m, m)
}

plot_raster_png <- function(r, out_png, title = NULL,
                            fill_name = "Value",
                            is_diff = FALSE,
                            add_coast = TRUE,
                            add_borders = TRUE) {
  df <- as.data.frame(r, xy = TRUE, na.rm = FALSE)
  colnames(df)[3] <- "value"
  
  # rasterのCRSをsfに渡す（ダメならWGS84にフォールバック）
  r_crs_sf <- tryCatch(sf::st_crs(terra::crs(r)), error = function(e) sf::st_crs(4326))
  
  # 境界データをrasterのCRSへ
  coast_r <- tryCatch(sf::st_transform(coast_sf, r_crs_sf), error = function(e) coast_sf)
  countries_r <- tryCatch(sf::st_transform(countries_sf, r_crs_sf), error = function(e) countries_sf)
  
  g <- ggplot(df, aes(x = x, y = y, fill = value)) +
    geom_raster() +
    coord_sf(
      xlim = c(terra::xmin(r), terra::xmax(r)),
      ylim = c(terra::ymin(r), terra::ymax(r)),
      expand = FALSE,
      crs = r_crs_sf
    ) +
    labs(x = NULL, y = NULL, title = title) +
    theme_bw(base_size = 11) +
    theme(
      panel.grid = element_blank(),
      axis.text  = element_blank(),
      axis.ticks = element_blank(),
      legend.position   = "right",
      legend.title      = element_text(size = 10),
      legend.text       = element_text(size = 8),
      legend.key.height = unit(0.4, "cm"),
      legend.key.width  = unit(0.4, "cm"),
      plot.title = element_text(size = 11),
      plot.margin = margin(5, 5, 5, 5)
    )
  
  if (!is_diff) {
    g <- g + scale_fill_viridis_c(name = fill_name, na.value = "grey90")
  } else {
    lim <- sym_limits(r)
    if (is.null(lim)) {
      g <- g + scale_fill_gradient2(name = fill_name, na.value = "grey90", midpoint = 0)
    } else {
      g <- g + scale_fill_gradient2(name = fill_name, na.value = "grey90", midpoint = 0, limits = lim)
    }
  }
  
  # ここで境界線を重ねる（rasterの上）
  if (add_borders) {
    g <- g + geom_sf(data = countries_r, fill = NA, linewidth = 0.15, inherit.aes = FALSE)
  }
  if (add_coast) {
    g <- g + geom_sf(data = coast_r, linewidth = 0.25, inherit.aes = FALSE)
  }
  
  dir.create(dirname(out_png), showWarnings = FALSE, recursive = TRUE)
  ggsave(out_png, plot = g, width = 8, height = 4, dpi = 300)
}



# =========================
# 1) 単純なSPnumber（Sce1/Sce2 + Current含む）と
# 2) Currentからの差（Future - Current）
# =========================
scenarios <- c(Sce1 = Sce1, Sce2 = Sce2)

for (sce_name in names(scenarios)) {
  sce <- scenarios[[sce_name]]
  
  for (taxon in taxonlist) {
    
    # Current
    r_cur <- read_rast_safe(current_path(sce, taxon, data_dir))
    if (!is.null(r_cur)) {
      out_png <- file.path(dir_raw, sce, taxon, paste0("SPnumber_", taxon, "_Current.png"))
      plot_raster_png(r_cur, out_png,
                      title = paste0(sce, " | ", taxon, " | Current"),
                      fill_name = "SPnumber", is_diff = FALSE)
    }
    
    # Future years
    for (year in years) {
      r_fut <- read_rast_safe(future_path(sce, taxon, year, sep_sce, data_dir))
      if (!is.null(r_fut)) {
        # raw
        out_png1 <- file.path(dir_raw, sce, taxon, paste0("SPnumber_", taxon, "_", year, ".png"))
        plot_raster_png(r_fut, out_png1,
                        title = paste0(sce, " | ", taxon, " | ", year),
                        fill_name = "SPnumber", is_diff = FALSE)
        
        # delta from Current
        if (!is.null(r_cur)) {
          r_d <- r_fut - r_cur
          out_png2 <- file.path(dir_dcur, sce, taxon, paste0("Delta_", taxon, "_", year, "_minus_Current.png"))
          plot_raster_png(r_d, out_png2,
                          title = paste0(sce, " | ", taxon, " | ", year, " - Current"),
                          fill_name = "ΔSPnumber", is_diff = TRUE)
        }
      }
    }
  }
}


# =========================
# 3) Sce2 - Sce1（ベースシナリオ差）
# =========================
for (taxon in taxonlist) {
  for (year in years) {
    r1 <- read_rast_safe(future_path(Sce1, taxon, year, sep_sce, data_dir))
    r2 <- read_rast_safe(future_path(Sce2, taxon, year, sep_sce, data_dir))
    
    if (!is.null(r1) && !is.null(r2)) {
      r_diff <- r2 - r1
      out_png <- file.path(dir_sdiff, taxon,
                           paste0("Sce2_minus_Sce1_", taxon, "_", year, ".png"))
      plot_raster_png(r_diff, out_png,
                      title = paste0("Sce2 - Sce1 | ", taxon, " | ", year, "\n", Sce2, " - ", Sce1),
                      fill_name = "ΔSPnumber", is_diff = TRUE)
    }
  }
}

# =========================
# 4) (Sce2 - Sce1) / Sce1_Current（スケール調整用）
# =========================
for (taxon in taxonlist) {
  
  # Sce1 Current を読む（分母）
  r_cur_sce1 <- read_rast_safe(current_path(Sce1, taxon, data_dir))
  if (is.null(r_cur_sce1)) next
  
  for (year in years) {
    r1 <- read_rast_safe(future_path(Sce1, taxon, year, sep_sce, data_dir))
    r2 <- read_rast_safe(future_path(Sce2, taxon, year, sep_sce, data_dir))
    if (is.null(r1) || is.null(r2)) next
    
    # 分母が0やNAの場所は NA にする
    denom <- r_cur_sce1
    denom[denom == 0] <- NA
    
    r_ratio <- (r2 - r1) / denom
    
    out_png <- file.path(dir_sdiff_ratio, taxon,
                         paste0("Sce2_minus_Sce1_div_Sce1Current_", taxon, "_", year, ".png"))
    
    plot_raster_png(
      r_ratio, out_png,
      title = paste0("(Sce2 - Sce1) / Sce1 Current | ", taxon, " | ", year,
                     "\n(", Sce2, " - ", Sce1, ") / ", Sce1, " Current"),
      fill_name = "ΔSPnumber / Sce1(Current)",
      is_diff = TRUE
    )
  }
}


message("Done. Outputs saved under: ", normalizePath(out_dir))
