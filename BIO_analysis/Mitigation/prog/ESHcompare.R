library(dplyr)
library(ggplot2)
library(tidyr)
library(readr)
library(stringr)

# =========================
# ユーザー設定（ここだけ触ればOK）
# =========================
BaseSce <- "Baseline_LU_Fmig"
Scenarios <- c("Mitigation_LU_Fmig")

data_dir <- "../data/ESH"
out_dir  <- "../output/ESH"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

years <- c(2030, 2050, 2070, 2090)

# facet に並べる地域（R5+Worldの6つ）
region_levels <- c("R5REF","R5OECD90_EU","R5ASIA","R5MAF","R5LAM","World")

# ---- シナリオ→ssp の割当（Baselineだけ違う、という要件をここで吸収）----
ssp_for <- function(sce) {
  if (sce == BaseSce) "ssp370" else "ssp126"
}

# ---- シナリオ名を分解（例: TRRS_LU_Fmig -> clim=TRRS, lu_part=LU_Fmig）----
parse_sce_tokens2 <- function(sce, sep = "_") {
  tokens <- strsplit(sce, split = sep, fixed = TRUE)[[1]]
  if (length(tokens) < 2) stop("Bad scenario name: ", sce)
  clim <- tokens[1]
  lu_part <- paste(tokens[2:length(tokens)], collapse = sep)  # "LU_Fmig"
  list(clim = clim, lu_part = lu_part)
}

# ---- ESH CSV path ----
esh_csv_path <- function(sce, data_dir = "../data/ESH") {
  p <- parse_sce_tokens2(sce, sep = "_")
  ssp <- ssp_for(sce)
  # 例: ESHI_RegionAgg_PROD-MRI-ESM2-0-ssp126-LU_Fmig.csv
  file.path(
    data_dir,
    paste0("ESHI_RegionAgg_", p$clim, "-MRI-ESM2-0-", ssp, "-", p$lu_part, ".csv")
  )
}

read_esh_safe <- function(path, sce) {
  if (!file.exists(path)) {
    message("[SKIP] not found: ", path)
    return(NULL)
  }
  # ファイル形式が厳密にCSVなら readr::read_csv が安全
  df <- suppressMessages(readr::read_csv(path, show_col_types = FALSE))
  # 想定列チェック
  need <- c("region","indicator")
  if (!all(need %in% names(df))) {
    message("[SKIP] missing columns (need region/indicator): ", path)
    return(NULL)
  }
  df$Scenario <- sce
  df
}

# =========================
# 1) 全シナリオCSVを読み込んで結合
# =========================
all_scenarios <- c(BaseSce, Scenarios)

lst <- list()
for (sce in all_scenarios) {
  f <- esh_csv_path(sce, data_dir)
  d <- read_esh_safe(f, sce)
  if (!is.null(d)) lst[[length(lst)+1]] <- d
}
df_raw <- bind_rows(lst)

if (nrow(df_raw) == 0) stop("No ESH CSV loaded. Check data_dir / filenames.")

# =========================
# 2) wide -> long（Current, X2030...）
# =========================
# 取りたい列だけ抽出（余計な列があってもOK）
value_cols <- intersect(names(df_raw), c("Current", paste0("X", years)))

df_long <- df_raw %>%
  select(Scenario, region, indicator, all_of(value_cols)) %>%
  pivot_longer(cols = all_of(value_cols), names_to = "YearLabel", values_to = "Value") %>%
  mutate(
    Year = case_when(
      YearLabel == "Current" ~ 2005,                 # rel2005系なのでCurrentは2005扱い（必要なら変更OK）
      str_starts(YearLabel, "X") ~ as.numeric(str_remove(YearLabel, "^X")),
      TRUE ~ NA_real_
    ),
    region = factor(region, levels = region_levels),
    Scenario = factor(Scenario, levels = all_scenarios)
  ) %>%
  filter(!is.na(Year)) %>%
  filter(region %in% region_levels)

# =========================
# 3) indicator 別に図を作って保存
# =========================
indicators <- sort(unique(df_long$indicator))

# indicator名をファイル名に使える形にする
safe_name <- function(x) {
  x %>%
    str_replace_all("[^A-Za-z0-9_\\-\\.]+", "_") %>%
    str_replace_all("_+", "_")
}

for (ind in indicators) {
  df_i <- df_long %>% filter(indicator == ind)
  
  # 年が揃ってないシナリオがある場合でも描けるようにする
  if (nrow(df_i) == 0) next
  
  p <- ggplot(df_i, aes(x = Year, y = Value, color = Scenario, group = Scenario)) +
    geom_line(linewidth = 0.9, na.rm = TRUE) +
    geom_point(size = 1.4, na.rm = TRUE) +
    facet_wrap(~ region, ncol = 3, scales = "free_y") +
    scale_x_continuous(breaks = c(2005, years)) +
    labs(
      title = "Temporal change of ESH index (Region aggregated)",
      subtitle = paste0("Indicator: ", ind, "  |  Baseline uses ssp370; others use ssp126"),
      x = "Year",
      y = "Value",
      color = "Scenario"
    ) +
    theme_bw(base_size = 11) +
    theme(
      panel.grid.major = element_line(color = "grey85", linewidth = 0.3),
      panel.grid.minor = element_line(color = "grey92", linewidth = 0.2),
      strip.background = element_rect(fill = "grey95", color = "grey70", linewidth = 0.3),
      panel.border = element_rect(color = "grey60", linewidth = 0.4),
      legend.position = "bottom",
      plot.title = element_text(face = "bold")
    )
  
  out_png <- file.path(out_dir, paste0("ESH_timeseries_", safe_name(ind), ".png"))
  ggsave(out_png, p, width = 12, height = 7, dpi = 220, bg = "white")
  message("Saved: ", out_png)
}

message("Done. Outputs saved under: ", normalizePath(out_dir))
