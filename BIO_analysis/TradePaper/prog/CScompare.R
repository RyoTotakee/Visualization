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
  "PROD_LU_Fmig",
  "TRFC_LU_Fmig",
  "TRRS_LU_Fmig"
)

sep_sce  <- "_"                   # シナリオ名の区切り
data_dir <- "../data/ComSim"
out_dir  <- "../output/ComSim"

years <- c(2030, 2050, 2070, 2090)


# ============================================================
# ComSim (JaccardSimilarity) の可視化
# - Current無し（すでに Current に対する類似度）
# - 1) raw map（シナリオ×年）
# - 2) シナリオ差 map（Sce - BaseSce）
# - 3) 時系列：median（実線）+ 25/75 percentile（点線）
# 出力: ../output/ComSim/...
# ============================================================

# ---- user settings（あなたの設定を使う） ----
# BaseSce, Scenarios, sep_sce, data_dir, out_dir, years は既に定義済み想定
# data_dir <- "../data/ComSim"
# out_dir  <- "../output/ComSim"

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

dir_raw_cs   <- file.path(out_dir, "01_raw_ComSim")
dir_diff_cs  <- file.path(out_dir, "03_Sce_minus_BaseSce_ComSim")
dir_ts_cs    <- file.path(out_dir, "05_timeseries_ComSim")

dir.create(dir_raw_cs,  showWarnings = FALSE, recursive = TRUE)
dir.create(dir_diff_cs, showWarnings = FALSE, recursive = TRUE)
dir.create(dir_ts_cs,   showWarnings = FALSE, recursive = TRUE)

all_scenarios <- c(BaseSce, Scenarios)

# ---- ComSim path helpers ----
comsim_future_path <- function(sce, year, sep = "_", data_dir = "../data/ComSim") {
  p <- parse_sce_tokens(sce, sep = sep)
  # 例: ../data/ComSim/Baseline_LU_Fmig/JaccardSimilarity_Baseline_2090_LU.tif
  file.path(data_dir, sce,
            paste0("JaccardSimilarity_", p$clim, "_", year, "_", p$lu, ".tif"))
}

# ---- stats helper (p25/median/p75) ----
calc_q_stats <- function(r) {
  qs <- terra::global(
    r,
    fun = function(x, ...) stats::quantile(
      x, probs = c(0.25, 0.5, 0.75),
      na.rm = TRUE, names = FALSE
    )
  )
  v <- as.numeric(qs[1, ])
  if (length(v) != 3) v <- rep(NA_real_, 3)
  c(p25 = v[1], median = v[2], p75 = v[3])
}

# ============================================================
# 1) raw map（シナリオ×年）
# ============================================================
for (sce in all_scenarios) {
  message("\n== ComSim raw maps: ", sce, " ==")
  
  for (year in years) {
    f <- comsim_future_path(sce, year, sep_sce, data_dir)
    r <- read_rast_safe(f)
    if (is.null(r)) next
    
    out_png <- file.path(dir_raw_cs, sce, sprintf("ComSim_%s_%d.png", sce, year))
    plot_raster_png(
      r, out_png,
      title = paste0("ComSim (JaccardSimilarity) | ", sce, " | ", year),
      fill_name = "Jaccard similarity",
      is_diff = FALSE
    )
  }
}

# ============================================================
# 2) シナリオ差 map（Sce - BaseSce）
#   ※ComSim自体が「Currentとの差」なので、これは「類似度の差」
# ============================================================
for (Sce in Scenarios) {
  message("\n== ComSim diff maps: ", Sce, " - ", BaseSce, " ==")
  
  for (year in years) {
    r_base <- read_rast_safe(comsim_future_path(BaseSce, year, sep_sce, data_dir))
    r_sce  <- read_rast_safe(comsim_future_path(Sce,     year, sep_sce, data_dir))
    if (is.null(r_base) || is.null(r_sce)) next
    
    r_diff <- r_sce - r_base
    
    out_png <- file.path(dir_diff_cs, paste0(Sce, "_minus_", BaseSce),
                         sprintf("ComSim_%s_minus_%s_%d.png", Sce, BaseSce, year))
    
    plot_raster_png(
      r_diff, out_png,
      title = paste0("ComSim diff | ", year, "\n", Sce, " - ", BaseSce),
      fill_name = "Δ Jaccard similarity",
      is_diff = TRUE
    )
  }
}

# ============================================================
# 3) 時系列：median（実線）+ 25/75 percentile（点線）
#   ※Taxon区別なし、1枚だけ保存
# ============================================================
rows <- list()

for (sce in all_scenarios) {
  for (year in years) {
    r <- read_rast_safe(comsim_future_path(sce, year, sep_sce, data_dir))
    if (is.null(r)) next
    
    st <- calc_q_stats(r)
    
    rows[[length(rows) + 1]] <- data.frame(
      Scenario = sce,
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
  message("[SKIP] No ComSim timeseries data found.")
} else {
  
  df_ts <- df_ts %>%
    mutate(
      Scenario = factor(Scenario, levels = all_scenarios),
      Year = as.numeric(Year)
    )
  
  p <- ggplot(df_ts, aes(x = Year, color = Scenario)) +
    # Jaccardは [0,1] のはずなので、必要なら0-1線を意識
    geom_hline(yintercept = 1, linewidth = 0.25, color = "grey70") +
    
    # 25/75 percentile (dashed) + 点
    geom_line(aes(y = p25, group = Scenario), linetype = "dashed", linewidth = 0.6, alpha = 0.9, show.legend = FALSE, na.rm = TRUE) +
    geom_point(aes(y = p25), size = 1.2, alpha = 0.9, show.legend = FALSE, na.rm = TRUE) +
    geom_line(aes(y = p75, group = Scenario), linetype = "dashed", linewidth = 0.6, alpha = 0.9, show.legend = FALSE, na.rm = TRUE) +
    geom_point(aes(y = p75), size = 1.2, alpha = 0.9, show.legend = FALSE, na.rm = TRUE) +
    
    # median (solid) + 点
    geom_line(aes(y = median, group = Scenario), linewidth = 1.0, na.rm = TRUE) +
    geom_point(aes(y = median), size = 1.6, na.rm = TRUE) +
    
    scale_x_continuous(breaks = years) +
    labs(
      title = "Temporal changes in similarity index (ComSim / JaccardSimilarity)",
      subtitle = "Solid = median across all grids; dashed = 25th/75th percentiles",
      x = "Year",
      y = "Jaccard similarity to current (ComSim)",
      color = "Scenario"
    ) +
    theme_bw(base_size = 11) +
    theme(
      panel.grid.major = element_line(color = "grey85", linewidth = 0.3),
      panel.grid.minor = element_line(color = "grey92", linewidth = 0.2),
      legend.position = "bottom",
      plot.title = element_text(face = "bold")
    )
  
  out_png <- file.path(dir_ts_cs, "Timeseries_ComSim_JaccardSimilarity.png")
  ggsave(out_png, p, width = 11, height = 6, dpi = 220, bg = "white")
  message("Saved: ", out_png)
}

message("ComSim done. Outputs saved under: ", normalizePath(out_dir))
