library(dplyr)
library(tidyr)
library(ggplot2)

csv_dir  <- file.path(data_dir, "BII", "csv")
plot_dir <- file.path(output_dir, "plots")
if (!dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE)

scenarios <- c(
  "SSP2_BaU_NoCC_default",
  "SSP2_BaU_NoCC_afr_as_timber",
  "SSP2_BaU_NoCC_afr_as_primf",
  "SSP2_500C_CACN_DAC_NoCC_default",
  "SSP2_500C_CACN_DAC_NoCC_afr_as_timber",
  "SSP2_500C_CACN_DAC_NoCC_afr_as_primf"
)

# ---- region の順序を保持する tidy 関数 ----
read_region_csv <- function(scen) {
  csv_file <- file.path(csv_dir, paste0("BII_regionagg_", scen, "_area.csv"))
  
  df <- read.csv(csv_file, stringsAsFactors = FALSE)
  
  # ★ region を「読み込み順」で factor に
  df$region <- factor(df$region, levels = df$region)
  
  df_long <- df %>%
    pivot_longer(
      cols      = starts_with("v_"),
      names_to  = "year",
      values_to = "BII"
    ) %>%
    mutate(
      year     = as.integer(sub("v_", "", year)),
      scenario = scen
    )
  
  df_long
}

# ---- 全シナリオ読み込み ----
df_all <- do.call(rbind, lapply(scenarios, read_region_csv))
df_all <- df_all %>%
  mutate(
    # BaU か 500C か（線種用）
    scenario_type = ifelse(grepl("BaU", scenario), "BaU", "500C"),
    # ベースのシナリオ・グループ（色用）
    scenario_group = case_when(
      grepl("default$",        scenario) ~ "default",
      grepl("afr_as_timber$",  scenario) ~ "afr_as_timber",
      grepl("afr_as_primf$",   scenario) ~ "afr_as_primf",
      TRUE ~ scenario  # 念のため fallback
    )
  )


p <- ggplot(
  df_all,
  aes(
    x = year,
    y = BII,
    color = scenario_group,
    linetype = scenario_type,
    group = interaction(scenario_group, scenario_type)
  )
) +
  geom_line(linewidth = 0.7) +
  facet_wrap(~ region, scales = "free_y") +
  scale_x_continuous(
    breaks = c(2005, 2020, 2040, 2060, 2080, 2100),   # ★ 目盛りを指定
    minor_breaks = NULL
  ) +
  scale_color_manual(
    values = c(
      "default"        = "#1f78b4",
      "afr_as_timber"  = "#33a02c",
      "afr_as_primf"   = "#e31a1c"
    ),
    name = "Scenario group"
  ) +
  scale_linetype_manual(
    values = c(
      "BaU"   = "dashed",
      "500C"  = "solid"
    ),
    name = "Scenario type"
  ) +
  labs(
    title = "Regional BII trajectories by scenario",
    x = "Year",
    y = "BII"
  ) +
  theme_bw(base_size = 11) +
  theme(
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background  = element_rect(fill = "white", colour = NA),
    panel.grid.major = element_line(colour = "grey90", linewidth = 0.3),
    panel.grid.minor = element_line(colour = "grey95", linewidth = 0.2),
    panel.border     = element_rect(colour = "black", fill = NA, linewidth = 0.4),
    legend.position  = "bottom",
    legend.key       = element_rect(fill = "white", colour = NA),
    strip.background = element_rect(fill = "grey90", colour = NA),
    strip.text       = element_text(face = "bold"),
    # ★ x軸ラベルを斜めに
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
  )



ggsave(
  filename = file.path(plot_dir, "BII_region_timeseries_all_scenarios.png"),
  plot     = p,
  width    = 12,
  height   = 8,
  dpi      = 300
)
