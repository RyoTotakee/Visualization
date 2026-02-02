library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)
library(patchwork)

# ---------------- theme ----------------
theme_1 <- theme_bw() +
  theme(
    panel.grid.major = element_line(color = "grey80", linewidth = 0.5),
    panel.grid.minor = element_line(color = "grey90", linewidth = 0.25),
    text = element_text(size = 24),
    axis.text.x = element_text(size = 18, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 20),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.text = element_text(size = 22),
    axis.title.x = element_blank(),
    legend.title = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(size = 22)
  )

# ---------------- paths ----------------
data_dir   <- "../data/0106/"
output_dir <- "../output/0106/"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# ---------------- scenario labels/order ----------------
scenario_labels <- c(
  "SSP2_BaU_NoCC"                    = "BASE",
  "SSP2_BaU_Reallocation_NoCC"       = "PROD",
  "SSP2_BaU_TradeFacilitation_NoCC"  = "DECR",
  "SSP2_BaU_TradeRestriction_NoCC"   = "INCR"
)

# 色固定（両図で統一）
scenario_colors <- c(
  "BASE" = "#1b9e77",
  "PROD" = "#d95f02",
  "DECR" = "#7570b3",
  "INCR" = "#e7298a"
)

region_list <- c("R5REF", "R5OECD90+EU", "R5ASIA", "R5MAF", "R5LAM", "World")

# ---------------- x-axis breaks（表示をキリよく） ----------------
# データは 2015–2100 (5年刻み)でも、表示は 2020–2100 (10年刻み)などにする
x_breaks_nice <- seq(2020, 2100, by = 10)

# ======================================================
# (b) BII plot
# ======================================================
IAMCTemp <- rgdx.param(paste0(data_dir,"Mergedcomparison.gdx"), "IAMCTemp") %>%
  rename(Scenario=i1, Region=i2, Variable=i3, Unit=i4, Year=i5, Value=value) %>%
  filter(Region %in% region_list) %>%
  mutate(
    Year   = as.numeric(as.character(Year)),
    Region = factor(Region, levels = region_list),
    Scenario = as.character(Scenario)
  ) %>%
  filter(Scenario %in% names(scenario_labels)) %>%
  mutate(
    Scenario = factor(
      Scenario,
      levels = names(scenario_labels),
      labels = unname(scenario_labels)
    )
  )

df_BII <- IAMCTemp %>% filter(Variable == "Ter_Bio_Bio_Int_Ind")

units <- df_BII %>% distinct(Unit) %>% pull(Unit) %>% as.character()
unit_label <- if (length(units) == 1) units else paste(units, collapse = " / ")
y_lab_bii <- paste0("BII [", unit_label, "]")

p_BII <- ggplot(df_BII, aes(x = Year, y = Value, color = Scenario, group = Scenario)) +
  geom_line(linewidth = 1.0, na.rm = TRUE) +
  facet_wrap(~ Region, drop = FALSE, scales = "free_y") +
  labs(y = y_lab_bii) +
  scale_color_manual(values = scenario_colors, drop = FALSE) +
  scale_x_continuous(
    breaks = x_breaks_nice,
    limits = c(2015, 2100)   # 端は保持（見切れ防止）。ラベルは x_breaks_nice だけ出る
  ) +
  theme_1 +
  theme(
    panel.border = element_rect(color = "grey60", fill = NA, linewidth = 0.5),
    strip.text = element_text(size = 18),
    strip.background = element_rect(fill = "grey95", color = NA),
    legend.position = "bottom"
  )

# ======================================================
# (a) Land plot
# ======================================================
AGMIP_agg <- rgdx.param(paste0(data_dir,"AGMIP_all_scenario.gdx"), "AGMIP_agg") %>%
  mutate(Y = as.numeric(as.character(Y))) %>%
  rename(VAR = AgMIP_VAR, UNIT = AgMIP_UNIT) %>%
  filter(region %in% region_list) %>%
  filter(Y %in% seq(2015, 2100, by = 5)) %>%
  mutate(region = factor(region, levels = region_list)) %>%
  filter(SCENARIO %in% names(scenario_labels)) %>%
  mutate(
    SCENARIO = factor(
      SCENARIO,
      levels = names(scenario_labels),
      labels = unname(scenario_labels)
    )
  )

Land <- AGMIP_agg %>%
  filter(VAR == "LAND") %>%
  pivot_wider(names_from = "ITEM", values_from = "AGMIP_agg") %>%
  replace_na(list(CRP = 0, NLD = 0, GRS = 0, ONV = 0, FOR = 0)) %>%
  mutate(CRP_NE = CRP - ECP) %>%
  select(SCENARIO, region, VAR, UNIT, Y, NLD, CRP, GRS, ONV, FOR) %>%
  pivot_longer(cols = c(NLD, CRP, GRS, ONV, FOR), names_to = "ITEM", values_to = "AGMIP") %>%
  mutate(
    AGMIP = AGMIP / 1000,
    ITEM = recode(ITEM,
                  "NLD" = "Urban",
                  "CRP" = "Cropland",
                  "GRS" = "Pasture",
                  "ONV" = "Other Natural Land",
                  "FOR" = "Forest"
    ),
    ITEM = factor(ITEM, levels = c("Cropland", "Pasture", "Forest", "Other Natural Land", "Urban"))
  )

p_Land <- Land %>%
  filter(region == "World", ITEM %in% c("Cropland", "Pasture", "Forest", "Other Natural Land")) %>%
  ggplot(aes(x = Y, y = AGMIP, color = SCENARIO, group = SCENARIO)) +
  geom_line(linewidth = 1.0) +
  facet_wrap(~ ITEM, ncol = 2, scales = "free_y") +
  labs(y = "Area [Mha]") +
  scale_color_manual(values = scenario_colors, drop = FALSE) +
  scale_x_continuous(
    breaks = x_breaks_nice,
    limits = c(2015, 2100)
  ) +
  theme_1 +
  theme(
    panel.border = element_rect(color = "grey60", fill = NA, linewidth = 0.5),
    strip.text = element_text(size = 18),
    strip.background = element_rect(fill = "grey95", color = NA),
    legend.position = "bottom"
  )

# ======================================================
# Combine: (a) Land left, (b) BII right
#   - BIIを少し大きく: widths を調整
# ======================================================
combined <- (p_Land + p_BII) +
  plot_layout(
    guides = "collect",
    widths = c(1, 1.15)   # ←ここで右（BII）を少し大きく
  ) +
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")") &
  theme(legend.position = "bottom")

out_file <- file.path(output_dir, "Land_BII_combined.png")
ggsave(out_file, combined, width = 16, height = 7.5, dpi = 220, bg = "white")
message("Saved: ", out_file)
