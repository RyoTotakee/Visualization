library(terra)
library(dplyr)
library(ggplot2)
library(scales)
library(grid)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

coast_sf     <- rnaturalearth::ne_coastline(scale = "medium", returnclass = "sf")
countries_sf <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

# =========================
# ユーザー設定（ここだけ触ればOK）
# =========================
BaseSce <- "Baseline_LU_Fmig"        # ★固定の基準（引かれる側）

# ★比較対象（引く側）を複数並べる
Scenarios <- c(
  "Mitigation_LU_Fmig"
)

sep_sce  <- "_"                   # シナリオ名の区切り
data_dir <- "../data/SPnumber"
out_dir  <- "../output/SPnumber"

years <- c(2030, 2050, 2070, 2090)
taxonlist <- c("Plantae","Amphibia","Reptilia","Aves","Mammalia","all_taxon")


# =========================
# 出力フォルダ作成（共通出力：raw / delta-from-current）
# =========================
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

dir_raw   <- file.path(out_dir, "01_raw_SPnumber")
dir_dcur  <- file.path(out_dir, "02_delta_from_Current")

dir.create(dir_raw,  showWarnings = FALSE, recursive = TRUE)
dir.create(dir_dcur, showWarnings = FALSE, recursive = TRUE)

# ※ 03,04 は「比較シナリオごと」に別 root を作ってその下に出す（後で作成）


# =========================
# ヘルパー関数（そのまま）
# =========================
parse_sce_tokens <- function(sce, sep = "_") {
  tokens <- strsplit(sce, split = sep, fixed = TRUE)[[1]]
  if (length(tokens) < 2) {
    stop(paste0("シナリオ名 '", sce, "' は少なくとも2要素が必要です。sep=", sep))
  }
  list(tokens = tokens, clim = tokens[1], lu = tokens[2])
}

future_path <- function(sce, taxon, year, sep = "_", data_dir = "../data") {
  p <- parse_sce_tokens(sce, sep = sep)
  file.path(data_dir, sce,
            paste0("SPnumber_", taxon, "_", p$clim, "_", year, "_", p$lu, ".tif"))
}

current_path <- function(sce, taxon, data_dir = "../data") {
  file.path(data_dir, sce,
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
  
  r_crs_sf <- tryCatch(sf::st_crs(terra::crs(r)), error = function(e) sf::st_crs(4326))
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
  
  if (add_borders) g <- g + geom_sf(data = countries_r, fill = NA, linewidth = 0.15, inherit.aes = FALSE)
  if (add_coast)   g <- g + geom_sf(data = coast_r, linewidth = 0.25, inherit.aes = FALSE)
  
  dir.create(dirname(out_png), showWarnings = FALSE, recursive = TRUE)
  ggsave(out_png, plot = g, width = 8, height = 4, dpi = 300)
}

# ============================================================
# 1) raw と 2) delta-from-current
#   → BaseSce と Scenarios 全部（単体出力）
# ============================================================
all_single_scenarios <- c(BaseSce, Scenarios)

for (sce in all_single_scenarios) {
  
  message("\n==============================")
  message("Single-scenario outputs: ", sce)
  message("==============================")
  
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

# ============================================================
# 3) Sce - BaseSce と 4) (Sce - BaseSce) / BaseSceCurrent
#   → Sce（複数）ごとに回す
#   出力: ../output/SPnumber/<Sce>_minus_<BaseSce>/03..., 04...
# ============================================================
for (Sce in Scenarios) {
  
  message("\n==============================")
  message("Scenario diff outputs: ", Sce, " - ", BaseSce)
  message("==============================")
  
  # compare root（Sceごと）
  diff_tag <- paste0(Sce, "_minus_", BaseSce)
  diff_root <- file.path(out_dir, diff_tag)
  
  dir_sdiff <- file.path(diff_root, "03_Sce_minus_BaseSce")
  dir_sdiff_ratio <- file.path(diff_root, "04_Sce_minus_BaseSce_div_BaseSceCurrent")
  
  dir.create(dir_sdiff, showWarnings = FALSE, recursive = TRUE)
  dir.create(dir_sdiff_ratio, showWarnings = FALSE, recursive = TRUE)
  
  for (taxon in taxonlist) {
    
    # BaseSce Current（分母）
    r_cur_base <- read_rast_safe(current_path(BaseSce, taxon, data_dir))
    if (is.null(r_cur_base)) {
      message("[SKIP] Base current missing: ", BaseSce, " | ", taxon)
      next
    }
    
    for (year in years) {
      r_base <- read_rast_safe(future_path(BaseSce, taxon, year, sep_sce, data_dir))
      r_sce  <- read_rast_safe(future_path(Sce,     taxon, year, sep_sce, data_dir))
      if (is.null(r_base) || is.null(r_sce)) next
      
      # 3) diff
      r_diff <- r_sce - r_base
      out_png3 <- file.path(dir_sdiff, taxon, paste0("Sce_minus_BaseSce_", taxon, "_", year, ".png"))
      plot_raster_png(r_diff, out_png3,
                      title = paste0("Sce - BaseSce | ", taxon, " | ", year, "\n", Sce, " - ", BaseSce),
                      fill_name = "ΔSPnumber", is_diff = TRUE)
      
      # 4) ratio: (Sce - BaseSce) / BaseSceCurrent
      denom <- r_cur_base
      denom[denom == 0] <- NA
      r_ratio <- (r_sce - r_base) / denom
      
      out_png4 <- file.path(dir_sdiff_ratio, taxon,
                            paste0("Sce_minus_BaseSce_div_BaseSceCurrent_", taxon, "_", year, ".png"))
      
      plot_raster_png(
        r_ratio, out_png4,
        title = paste0("(Sce - BaseSce) / BaseSce Current | ", taxon, " | ", year,
                       "\n(", Sce, " - ", BaseSce, ") / ", BaseSce, " Current"),
        fill_name = "ΔSPnumber / Base(Current)",
        is_diff = TRUE
      )
    }
  }
}

message("Done. Outputs saved under: ", normalizePath(out_dir))



# ============================================================
# 5) 時系列の折れ線（相対変化： (NSfuture - NScurrent)/NScurrent ）
#   - 各グリッドで相対変化を計算
#   - 全グリッドの median を実線
#   - 25/75 percentile を点線
#   - シナリオで色分け
#   - taxonごとに保存
# ============================================================


dir_ts <- file.path(out_dir, "05_timeseries_relative_change")
dir.create(dir_ts, showWarnings = FALSE, recursive = TRUE)

# 「すべてのシナリオ」＝ BaseSce + Scenarios
all_scenarios <- c(BaseSce, Scenarios)

# 1枚のラスタから、相対変化ラスタ→(p25, median, p75) を返す関数
calc_rel_stats <- function(r_future, r_current, min_current = 1) {
  # ★集計対象：Currentが min_current 以上のセルだけ
  mask_ok <- r_current >= min_current
  
  r_current2 <- mask(r_current, mask_ok, maskvalues = FALSE, updatevalue = NA)
  r_future2  <- mask(r_future,  mask_ok, maskvalues = FALSE, updatevalue = NA)
  
  # 分母0は念のためNA
  denom <- r_current2
  denom[denom == 0] <- NA
  
  r_rel <- (r_future2 - r_current2) / (denom * 1.0)
  
  qs <- terra::global(
    r_rel,
    fun = function(x, ...) stats::quantile(
      x, probs = c(0.25, 0.5, 0.75),
      na.rm = TRUE, names = FALSE
    )
  )
  
  v <- as.numeric(qs[1, ])
  if (length(v) != 3) v <- rep(NA_real_, 3)
  
  c(p25 = v[1], median = v[2], p75 = v[3])
}





# taxonごとに計算してプロット
for (taxon in taxonlist) {
  
  message("\n==============================")
  message("Timeseries (relative change): ", taxon)
  message("==============================")


  rows <- list()
  
  for (sce in all_scenarios) {
    
    # BaseSceのCurrentを読む（相対変化の基準）
    r_cur_sce <- read_rast_safe(current_path(sce, taxon, data_dir))
    if (is.null(r_cur_sce)) next
    
    for (year in years) {
      
      r_fut <- read_rast_safe(future_path(sce, taxon, year, sep_sce, data_dir))
      if (is.null(r_fut)) next
      
      st <- calc_rel_stats(r_fut, r_cur_sce, min_current = 1)
      
      
      rows[[length(rows) + 1]] <- data.frame(
        Scenario = sce,
        Taxon = taxon,
        Year = year,
        p25 = st["p25"],
        median = st["median"],
        p75 = st["p75"],
        stringsAsFactors = FALSE
      )
    }
  }
  
  df_ts <- dplyr::bind_rows(rows)
  
  if (nrow(df_ts) == 0) {
    message("[SKIP] No timeseries data: ", taxon)
    next
  }
  
  df_ts <- df_ts %>%
    mutate(
      Scenario = factor(Scenario, levels = all_scenarios),
      Year = as.numeric(Year)
    )
  
  # ---- plot ----
  p <- ggplot(df_ts, aes(x = Year, color = Scenario)) +
    geom_hline(yintercept = 0, linewidth = 0.3, color = "grey60") +
    
    # 25/75 percentile (dashed) + 点
    geom_line(aes(y = p25, group = Scenario), linetype = "dashed", linewidth = 0.6, alpha = 0.9, show.legend = FALSE) +
    geom_point(aes(y = p25), size = 1.2, alpha = 0.9, show.legend = FALSE) +
    geom_line(aes(y = p75, group = Scenario), linetype = "dashed", linewidth = 0.6, alpha = 0.9, show.legend = FALSE) +
    geom_point(aes(y = p75), size = 1.2, alpha = 0.9, show.legend = FALSE) +
    
    # median (solid) + 点
    geom_line(aes(y = median, group = Scenario), linewidth = 1.0) +
    geom_point(aes(y = median), size = 1.6) +
    
    scale_x_continuous(breaks = years) +
    labs(
      title = paste0("Temporal changes in species richness (", taxon, ")"),
      subtitle = paste0(
        "Relative change = (NSfuture - NScurrent) / NScurrent; solid=median, dashed=25/75 percentile\nBase current: ",
        BaseSce
      ),
      x = "Year",
      y = "Relative change in species richness",
      color = "Scenario"
    ) +
    theme_bw(base_size = 11) +
    theme(
      panel.grid.major = element_line(color = "grey85", linewidth = 0.3),
      panel.grid.minor = element_line(color = "grey92", linewidth = 0.2),
      legend.position = "bottom",
      plot.title = element_text(face = "bold")
    )
  
  
  out_png <- file.path(dir_ts, paste0("Timeseries_relchange_", taxon, ".png"))
  ggsave(out_png, p, width = 11, height = 6, dpi = 220, bg = "white")
  message("Saved: ", out_png)
}