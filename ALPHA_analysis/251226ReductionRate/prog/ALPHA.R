library(tidyverse)
library(gdxrrw)
library(ggplot2)
library(ggpmisc)
library(RColorBrewer)
library(purrr)
library(furrr)

theme_1 <- theme_bw() +
  theme(
    # panel.grid.major = element_blank(),
    # panel.grid.minor = element_blank(),
    text = element_text(size = 20), axis.text.x = element_text(size = 15),
    legend.position = "bottom",
    axis.title.x = element_blank(),
    legend.title = element_blank(),
    strip.background = element_blank()
  )

theme_2 <- theme_bw() +
  theme(
    # panel.grid.major = element_blank(),
    # panel.grid.minor = element_blank(),
    text = element_text(size = 20), axis.text.x = element_text(size = 15, angle = 45, hjust = 1),
    legend.position = "bottom",
    axis.title.x = element_blank(),
    legend.title = element_blank(),
    strip.background = element_blank()
  )

land_color <- c(
  "Cropland" = "#8CA231",
  "Managed and primary forest" = "#014D2F",
  "Grassland" = "#26C3EC",
  "Other natural land" = "#FF9855",
  "Non arable land" = "#AAA9A9",
  "Energy Crop" = "#ED87AD"
)

commodity_color <- c(
  "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b",
  "#e377c2", "#7f7f7f", "#bcbd22", "#17becf", "#1b9e77", "#d95f02",
  "#7570b3", "#e7298a", "#66a61e", "#e6ab02", "#a6761d", "#666666",
  "#f7b6d2", "#c5b0d5", "#c49c94", "#9edae5", "#17becf"
)

region_omit <- c("OSA", "FSU", "EUR", "MEN", "SSA", "SEA", "OAS", "ANZ", "NAM", "OAM", "AME", "SAS",
                 "R5ASIA", "R5OECD90+EU", "R5REF", "R5MAF", "R5LAM")



igdx(gams_sys_dir)
threadsnum <- 4
plan(multisession, workers = threadsnum)

# scenario_compare
# country
AGMIP <- rgdx.param("../../output/AGMIP/AGMIP_all_scenario.gdx", "AGMIP") %>%
  mutate(Y = as.character(Y)) %>%
  mutate(Y = as.numeric(Y)) %>%
  filter(Y %in% seq(2015, 2100, 5))

VAR_list <- unique(AGMIP$AgMIP_VAR) %>%
  as.character() %>%
  as.data.frame()
ITEM_list <- unique(AGMIP$ITEM) %>%
  as.character() %>%
  as.data.frame()
UNIT_list <- unique(AGMIP$AgMIP_UNIT) %>%
  as.character() %>%
  as.data.frame()

list_total <- cross_join(VAR_list, ITEM_list)
list_total <- cross_join(list_total, UNIT_list)
names(list_total) <- c("VAR", "ITEM", "UNIT")

num <- c(1:length(list_total$VAR))

AgMIP_plot <- function(n) {
  i <- list_total$VAR[n]
  j <- list_total$ITEM[n]
  k <- list_total$UNIT[n]
  l <- gsub("/", "_", k)
  df <- AGMIP %>% filter(AgMIP_VAR == i, ITEM == j, AgMIP_UNIT == k)
  if (length(df$AGMIP) > 0) {
    if (min(df$AGMIP) < 0) {
      g_df <- df %>% ggplot() +
        geom_line(aes(x = Y, y = AGMIP, color = SCENARIO)) +
        facet_wrap(vars(cty), scales = "free_y") +
        scale_color_brewer(palette = "Set1") +
        scale_x_continuous(breaks = c(2020, 2050, 2100)) +
        labs(y = paste0(k)) +
        theme_1
    } else {
      g_df <- df %>% ggplot() +
        geom_line(aes(x = Y, y = AGMIP, color = SCENARIO)) +
        facet_wrap(vars(cty), scales = "free_y") +
        scale_color_brewer(palette = "Set1") +
        scale_x_continuous(breaks = c(2020, 2050, 2100)) +
        scale_y_continuous(limits = c(0, NA)) +
        labs(y = paste0(k)) +
        theme_1
    }
    ggsave(paste0("../../plot/scenario_compare/country/", i, "/", l, "/", j, ".png"), g_df, width = 30, height = 20, create.dir = T)
  }
}

future_map(num, AgMIP_plot)



# global17
AGMIP_agg <- rgdx.param("../../output/AGMIP/AGMIP_all_scenario.gdx", "AGMIP_agg") %>%
  mutate(Y = as.character(Y)) %>%
  mutate(Y = as.numeric(Y)) %>%
  filter(!region %in% region_omit) %>%
  filter(Y %in% seq(2015, 2100, 5))

VAR_list <- unique(AGMIP_agg$AgMIP_VAR) %>%
  as.character() %>%
  as.data.frame()
ITEM_list <- unique(AGMIP_agg$ITEM) %>%
  as.character() %>%
  as.data.frame()
UNIT_list <- unique(AGMIP_agg$AgMIP_UNIT) %>%
  as.character() %>%
  as.data.frame()

list_total <- cross_join(VAR_list, ITEM_list)
list_total <- cross_join(list_total, UNIT_list)
names(list_total) <- c("VAR", "ITEM", "UNIT")

num <- c(1:length(list_total$VAR))

AgMIP_agg_plot <- function(n) {
  i <- list_total$VAR[n]
  j <- list_total$ITEM[n]
  k <- list_total$UNIT[n]
  l <- gsub("/", "_", k)
  df <- AGMIP_agg %>% filter(AgMIP_VAR == i, ITEM == j, AgMIP_UNIT == k)
  if (length(df$AGMIP_agg) > 0) {
    if (min(df$AGMIP_agg) < 0) {
      g_df <- df %>% ggplot() +
        geom_line(aes(x = Y, y = AGMIP_agg, color = SCENARIO)) +
        facet_wrap(vars(region), scales = "free_y") +
        scale_color_brewer(palette = "Set1") +
        scale_x_continuous(breaks = c(2020, 2050, 2100)) +
        labs(y = paste0(k)) +
        theme_1
    } else {
      g_df <- df %>% ggplot() +
        geom_line(aes(x = Y, y = AGMIP_agg, color = SCENARIO)) +
        facet_wrap(vars(region), scales = "free_y") +
        scale_color_brewer(palette = "Set1") +
        scale_x_continuous(breaks = c(2020, 2050, 2100)) +
        scale_y_continuous(limits = c(0, NA)) +
        labs(y = paste0(k)) +
        theme_1
    }
    ggsave(paste0("../../plot/scenario_compare/global17/", i, "/", l, "/", j, ".png"), g_df, width = 30, height = 20, create.dir = T)
  }
}

future_map(num, AgMIP_agg_plot)





# IAMC
IAMC <- rgdx.param("../data/IAMC_all_scenario.gdx", "IAMC_Template") %>%
  mutate(Y = as.character(Y)) %>%
  mutate(Y = as.numeric(Y)) %>%
  filter(!IAMC_REGION %in% region_omit) %>%
  filter(Y %in% seq(2015, 2100, 5))

VAR_list <- unique(IAMC$IAMC_VAR) %>%
  as.character() %>%
  as.data.frame()

VAR_list <- rgdx.set("../data/IAMC_all_scenario.gdx", "IAMC_VAR", te = T)
names(VAR_list) <- c("IAMC_VAR", "TEXT")
VAR_UNIT <- rgdx.set("../data/IAMC_all_scenario.gdx", "IAMC_UNIT_MAP")
list_total <- full_join(VAR_list, VAR_UNIT)
list_total <- list_total %>% mutate(IAMC_VAR = as.character(IAMC_VAR, TEXT = as.character(TEXT), IAMC_UNIT = as.character(IAMC_UNIT)))
num <- c(1:length(list_total$IAMC_VAR))

IAMC_plot <- function(n) {
  i <- list_total$IAMC_VAR[n]
  j <- list_total$TEXT[n]
  k <- list_total$IAMC_UNIT[n]
  df <- IAMC %>% filter(IAMC_VAR == i)
  if (length(df$IAMC_Template) > 0) {
    if (min(df$IAMC_Template) < 0) {
      g_df <- df %>% ggplot() +
        geom_line(aes(x = Y, y = IAMC_Template, color = SCENARIO)) +
        facet_wrap(vars(IAMC_REGION), scales = "free_y") +
        scale_color_brewer(palette = "Set1") +
        scale_x_continuous(breaks = c(2020, 2050, 2100)) +
        labs(title = j,y = paste0("Emissions [", k,"]")) +
        theme_1
    } else {
      g_df <- df %>% ggplot() +
        geom_line(aes(x = Y, y = IAMC_Template, color = SCENARIO)) +
        facet_wrap(vars(IAMC_REGION), scales = "free_y") +
        scale_color_brewer(palette = "Set1") +
        scale_x_continuous(breaks = c(2020, 2050, 2100)) +
        scale_y_continuous(limits = c(0, NA)) +
        labs(title = j,y = paste0("Emissions [", k,"]")) +
        theme_1
    }
    ggsave(paste0("../output/IAMC/", i, ".png"), g_df, width = 15, height = 10, create.dir = T)
  }
}

# future_map(num, IAMC_plot)

selected_vars <- c(
  "Emi_CH4_AFO_Agr",
  "Emi_CH4_AFO_Agr_Liv_Ent_Fer",
  "Emi_CH4_AFO_Agr_Liv_Man_Man",
  "Emi_CH4_AFO_Agr_Ric",
  "Emi_N2O_AFO_Agr",
  "Emi_N2O_AFO_Agr_Liv_Man_Man",
  "Emi_N2O_AFO_Agr_Man_Soi"
)
idx <- which(list_total$IAMC_VAR %in% selected_vars)
if (length(idx) == 0) stop("selected_vars が list_total$IAMC_VAR に見つかりませんでした。")
future_map(idx, IAMC_plot)





# 
# IAMC_plot <- function(n) {
#   i <- list_total$IAMC_VAR[n]
#   j <- list_total$TEXT[n]
#   k <- list_total$IAMC_UNIT[n]
#   df <- IAMC %>% filter(IAMC_VAR == i)
#   if (length(df$IAMC_Template) > 0) {
#     if (min(df$IAMC_Template) < 0) {
#       g_df <- df %>% ggplot() +
#         geom_line(aes(x = Y, y = IAMC_Template, color = SCENARIO)) +
#         facet_wrap(vars(IAMC_REGION), scales = "free_y") +
#         scale_color_brewer(palette = "Set1") +
#         scale_x_continuous(breaks = c(2020, 2050, 2100)) +
#         labs(title = j,y = paste0("Land [", k,"]")) +
#         theme_1
#     } else {
#       g_df <- df %>% ggplot() +
#         geom_line(aes(x = Y, y = IAMC_Template, color = SCENARIO)) +
#         facet_wrap(vars(IAMC_REGION), scales = "free_y") +
#         scale_color_brewer(palette = "Set1") +
#         scale_x_continuous(breaks = c(2020, 2050, 2100)) +
#         scale_y_continuous(limits = c(0, NA)) +
#         labs(title = j,y = paste0("Land [", k,"]")) +
#         theme_1
#     }
#     ggsave(paste0("../output/IAMC/", i, ".png"), g_df, width = 15, height = 10, create.dir = T)
#   }
# }
# 
# # future_map(num, IAMC_plot)
# 
# selected_vars <- c(
#   "Emi_CH4_AFO_Agr",
#   "Emi_CH4_AFO_Agr_Liv_Ent_Fer",
#   "Emi_CH4_AFO_Agr_Liv_Man_Man",
#   "Emi_CH4_AFO_Agr_Ric",
#   "Emi_N2O_AFO_Agr",
#   "Emi_N2O_AFO_Agr_Liv_Man_Man",
#   "Emi_N2O_AFO_Agr_Man_Soi"
# )
# idx <- which(list_total$IAMC_VAR %in% selected_vars)
# if (length(idx) == 0) stop("selected_vars が list_total$IAMC_VAR に見つかりませんでした。")
# future_map(idx, IAMC_plot)
# 



region_list <- c("BRA","CAN","CHN","IND","JPN","TUR","USA","XE25","XER","XOC","XSE","XSA","XLM","CIS","XME","XNF","XAF","World")

IAMC_AFOLUB <- rgdx.param("../data/IAMC_templatePlus.gdx", "IAMC_templatePlus") %>%
  mutate(Y = as.character(YEMF)) %>%
  mutate(Y = as.numeric(Y)) %>%
  filter(REMF %in% region_list) %>%
  mutate(IAMC_REGION = as.character(REMF))%>%
  mutate(IAMC_Var = as.character(VEMF)) %>%
  mutate(IAMC_Template = as.numeric(IAMC_templatePlus)) %>%
  filter(Y %in% seq(2015, 2100, 5)) %>%
  mutate(SCENARIO = as.character(Scenario)) %>%
  filter(SCENARIO == "SSP2_CM_NoCC") %>%
  select(SCENARIO, IAMC_Var, IAMC_REGION, Y, IAMC_Template)





# ===== 設定 =====
vars_ch4 <- c(
  "Emi_CH4_AFO_Agr_Liv_Ent_Fer",
  "Emi_CH4_AFO_Agr_Liv_Man_Man",
  "Emi_CH4_AFO_Agr_Ric"
)

vars_n2o <- c(
  "Emi_N2O_AFO_Agr_Liv_Man_Man",
  "Emi_N2O_AFO_Agr_Man_Soi"
)

years_sel <- c(2020, 2050, 2085)

# 保存先フォルダ（新しく作る）
out_dir <- "../output/IAMC_stack"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ===== 世界（World）だけに絞って、対象変数&年だけ抽出 =====
# ※ IAMC_REGION の "World" 表記ゆれがあると困るので、よくある候補を許容
world_candidates <- c("World", "WORLD", "Global", "GLB", "WLD")

df_base <- IAMC %>%
  filter(IAMC_REGION %in% world_candidates) %>%
  filter(Y %in% years_sel) %>%
  filter(IAMC_VAR %in% c(vars_ch4, vars_n2o)) %>%
  # 表示用のラベル（あれば TEXT/UNIT を付ける）
  left_join(list_total %>% distinct(IAMC_VAR, TEXT, IAMC_UNIT), by = "IAMC_VAR") %>%
  mutate(
    TEXT = ifelse(is.na(TEXT), IAMC_VAR, TEXT),
    IAMC_UNIT = ifelse(is.na(IAMC_UNIT), "", IAMC_UNIT),
    VAR_LABEL = TEXT
  )

# ===== CH4 plot =====
df_ch4 <- df_base %>%
  filter(IAMC_VAR %in% vars_ch4) %>%
  mutate(
    SCENARIO = as.factor(SCENARIO),
    Y = as.factor(Y),
    VAR_LABEL = factor(VAR_LABEL, levels = unique(VAR_LABEL))
  )

unit_ch4 <- df_ch4 %>% distinct(IAMC_UNIT) %>% pull(IAMC_UNIT) %>% unique()
unit_ch4 <- if (length(unit_ch4) == 1) unit_ch4 else ""

p_ch4 <- ggplot(df_ch4, aes(x = SCENARIO, y = IAMC_Template, fill = VAR_LABEL)) +
  geom_col() +
  facet_wrap(~ Y, nrow = 1) +
  labs(
    x = NULL,  # ← "Scenario" 表示を消す
    y = "Emissions [Mt CH4/yr]",
    fill = NULL
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 40, hjust = 1),
    legend.position = "bottom",          # ← 凡例を下へ
    legend.direction = "horizontal"      # ← 横並び
  ) +
  guides(fill = guide_legend(
    nrow = 3,       # ← ここを 2,3,... と増やすと折り返しが強くなる
    byrow = TRUE    # ← 行単位で埋める
  ))

ggsave(file.path(out_dir, "CH4_stacked_world.png"), p_ch4, width = 5, height = 6)

# ===== N2O plot =====
df_n2o <- df_base %>%
  filter(IAMC_VAR %in% vars_n2o) %>%
  mutate(
    SCENARIO = as.factor(SCENARIO),
    Y = as.factor(Y),
    VAR_LABEL = factor(VAR_LABEL, levels = unique(VAR_LABEL))
  )
p_n2o <- ggplot(df_n2o, aes(x = SCENARIO, y = IAMC_Template, fill = VAR_LABEL)) +
  geom_col() +
  facet_wrap(~ Y, nrow = 1) +
  labs(
    x = NULL,  # ← "Scenario" 表示を消す
    y = "Emissions [kt N2O/yr]",
    fill = NULL
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 40, hjust = 1),
    legend.position = "bottom",
    legend.direction = "horizontal"
  ) +
  guides(fill = guide_legend(
    nrow = 2,       # ← ここを 2,3,... と増やすと折り返しが強くなる
    byrow = TRUE    # ← 行単位で埋める
  ))

ggsave(file.path(out_dir, "N2O_stacked_world.png"), p_n2o, width = 5, height = 6)



df_base_clean <- df_base %>%
  filter(Y %in% years_sel) %>%
  select(SCENARIO, IAMC_VAR, IAMC_REGION, Y, IAMC_Template) %>%
  left_join(list_total %>% distinct(IAMC_VAR, TEXT, IAMC_UNIT), by = "IAMC_VAR") %>%
  mutate(
    TEXT      = as.character(TEXT),
    IAMC_UNIT = as.character(IAMC_UNIT),
    TEXT      = ifelse(is.na(TEXT), IAMC_VAR, TEXT),
    IAMC_UNIT = ifelse(is.na(IAMC_UNIT), "", IAMC_UNIT),
    VAR_LABEL = TEXT
  ) %>%
  select(SCENARIO, IAMC_VAR, IAMC_REGION, Y, IAMC_Template, TEXT, IAMC_UNIT, VAR_LABEL)

# ===== AFOLUB 側も同じ列構造・同じ型に揃える =====
IAMC_AFOLUB2 <- IAMC_AFOLUB %>%
  filter(Y %in% years_sel) %>%
  rename(IAMC_VAR = IAMC_Var) %>%
  mutate(
    SCENARIO = "AFOLUB",
    IAMC_REGION = as.character(IAMC_REGION),
    Y = as.numeric(Y),
    IAMC_Template = as.numeric(IAMC_Template)
  ) %>%
  select(SCENARIO, IAMC_VAR, IAMC_REGION, Y, IAMC_Template) %>%
  left_join(list_total %>% distinct(IAMC_VAR, TEXT, IAMC_UNIT), by = "IAMC_VAR") %>%
  mutate(
    TEXT      = as.character(TEXT),
    IAMC_UNIT = as.character(IAMC_UNIT),
    TEXT      = ifelse(is.na(TEXT), IAMC_VAR, TEXT),
    IAMC_UNIT = ifelse(is.na(IAMC_UNIT), "", IAMC_UNIT),
    VAR_LABEL = TEXT
  ) %>%
  select(SCENARIO, IAMC_VAR, IAMC_REGION, Y, IAMC_Template, TEXT, IAMC_UNIT, VAR_LABEL)

df_base2 <- bind_rows(df_base_clean, IAMC_AFOLUB2)


# ===== CH4/N2O の作り直し（df_base2 を使うだけ）=====
df_ch4 <- df_base2 %>%
  filter(IAMC_VAR %in% vars_ch4) %>%
  mutate(
    SCENARIO = as.factor(SCENARIO),
    Y = as.factor(Y),
    VAR_LABEL = factor(VAR_LABEL, levels = unique(VAR_LABEL))
  )

df_n2o <- df_base2 %>%
  filter(IAMC_VAR %in% vars_n2o) %>%
  mutate(
    SCENARIO = as.factor(SCENARIO),
    Y = as.factor(Y),
    VAR_LABEL = factor(VAR_LABEL, levels = unique(VAR_LABEL))
  )

# ===== プロットは今まで通り（AFOLUB が x 軸に追加される）=====
p_ch4 <- ggplot(df_ch4, aes(x = SCENARIO, y = IAMC_Template, fill = VAR_LABEL)) +
  geom_col() +
  facet_wrap(~ Y, nrow = 1) +
  labs(x = NULL, y = "Emissions [Mt CH4/yr]", fill = NULL) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "horizontal"
  ) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))

ggsave(file.path(out_dir, "CH4_stacked_world_with_AFOLUB.png"), p_ch4, width = 10, height = 4)


p_n2o <- ggplot(df_n2o, aes(x = SCENARIO, y = IAMC_Template, fill = VAR_LABEL)) +
  geom_col() +
  facet_wrap(~ Y, nrow = 1) +
  labs(x = NULL, y = "Emissions [kt N2O/yr]", fill = NULL) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "horizontal"
  ) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))

ggsave(file.path(out_dir, "N2O_stacked_world_with_AFOLUB.png"), p_n2o, width = 10, height = 4)


# 
# # land
# Land <- AGMIP_agg %>%
#   filter(AgMIP_VAR == "LAND") %>%
#   pivot_wider(names_from = "ITEM", values_from = "AGMIP_agg")
# Land[is.na(Land)] <- 0
# Land <- Land %>%
#   mutate(CRP_NE = CRP - ECP) %>%
#   select(SCENARIO, region, AgMIP_VAR, AgMIP_UNIT, Y, NLD, CRP_NE, ECP, GRS, ONV, FOR) %>%
#   pivot_longer(cols = c(NLD, CRP_NE, ECP, GRS, ONV, FOR), names_to = "ITEM", values_to = "AGMIP") %>%
#   mutate(ITEM = recode(ITEM,
#                        "NLD" = "Non arable land",
#                        "CRP_NE" = "Cropland",
#                        "ECP" = "Energy Crop",
#                        "GRS" = "Grassland",
#                        "ONV" = "Other natural land",
#                        "FOR" = "Managed and primary forest"
#   )) %>%
#   mutate(ITEM = factor(ITEM, levels = c("Cropland", "Energy Crop", "Grassland", "Managed and primary forest", "Other natural land", "Non arable land")))
# 
# g_Land <- Land %>%
#   filter(Y == "2100") %>%
#   ggplot() +
#   geom_bar(aes(x = SCENARIO, y = AGMIP, fill = ITEM), stat = "identity") +
#   scale_fill_manual(values = land_color) +
#   labs(y = "Land Area [1000ha]") +
#   facet_wrap(vars(region), scales = "free_y") +
#   theme_2
# ggsave("../../plot/scenario_compare/global17/land_global17.png", g_Land, width = 30, height = 20, create.dir = T)


AC <- rgdx.param("../data/all_scenario_global_17.gdx", "AC_agg_load") %>%
  filter(region %in% region_list)
RR <- rgdx.param("../data/all_scenario_global_17.gdx", "RR_agg_load") %>%
  filter(region %in% region_list)
QSUP <- rgdx.param("../data/all_scenario_global_17.gdx", "QSUP_agg_load") %>%
  filter(region %in% region_list)
GHG <- rgdx.param("../data/all_scenario_global_17.gdx", "GHG_agg_load") %>%
  filter(region %in% region_list)


out_dir <- "../output/AC_RR_line"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

c_sel <- c("rce", "cmt")
gases_to_save <- c("N2O", "CH4")

scenario_levels <- c("SSP2_500C_NoCC", "SSP2_500C_afolub_NoCC")

# =========================
# AC: c ごとに分けて保存
# =========================
AC_f <- AC %>%
  filter(c %in% c_sel) %>%
  mutate(
    Y = as.numeric(Y),
    AC_agg_load = as.numeric(AC_agg_load),
    SCENARIO = factor(as.character(SCENARIO), levels = scenario_levels),
    region = as.factor(region),
    c = as.factor(c)
  )

for (cc in c_sel) {
  df_cc <- AC_f %>% filter(c == cc)
  if (nrow(df_cc) == 0) next
  
  if (min(df_cc$AC_agg_load, na.rm = TRUE) < 0) {
    p_ac <- ggplot(df_cc, aes(x = Y, y = AC_agg_load, color = SCENARIO)) +
      geom_line() +
      facet_wrap(vars(region), scales = "free_y") +
      scale_color_brewer(palette = "Set1") +
      scale_x_continuous(breaks = c(2020, 2050, 2100)) +
      labs(x = NULL, y = paste0("AC_agg_load (c=", cc, ")")) +
      theme_1
  } else {
    p_ac <- ggplot(df_cc, aes(x = Y, y = AC_agg_load, color = SCENARIO)) +
      geom_line() +
      facet_wrap(vars(region), scales = "free_y") +
      scale_color_brewer(palette = "Set1") +
      scale_x_continuous(breaks = c(2020, 2050, 2100)) +
      scale_y_continuous(limits = c(0, NA)) +
      labs(x = NULL, y = paste0("AC_agg_load (c=", cc, ")")) +
      theme_1
  }
  
  ggsave(file.path(out_dir, paste0("AC_agg_load_line_byScenario_c_", cc, ".png")),
         p_ac, width = 15, height = 10)
}

# =========================
# RR: src を GHG 重みで加重平均して集約（cごと）
# =========================

# まず RR と GHG を同じキーで結合
RR_GHG <- RR %>%
  filter(c %in% c_sel) %>%
  mutate(
    Y = as.numeric(Y),
    RR_agg_load = as.numeric(RR_agg_load),
    SCENARIO = factor(as.character(SCENARIO), levels = scenario_levels),
    c = as.character(c),
    src = as.character(src),
    gas = as.character(gas),
    region = as.character(region)
  ) %>%
  inner_join(
    GHG %>%
      filter(c %in% c_sel) %>%
      mutate(
        Y = as.numeric(Y),
        GHG_agg_load = as.numeric(GHG_agg_load),
        SCENARIO = factor(as.character(SCENARIO), levels = scenario_levels),
        c = as.character(c),
        src = as.character(src),
        gas = as.character(gas),
        region = as.character(region)
      ) %>%
      select(SCENARIO, c, src, gas, region, Y, GHG_agg_load),
    by = c("SCENARIO", "c", "src", "gas", "region", "Y")
  ) %>%
  # 重みが欠損/ゼロは除外（ゼロ割・意味なし回避）
  filter(!is.na(GHG_agg_load), GHG_agg_load > 0)

# src を潰して加重平均
RR_sum <- RR_GHG %>%
  group_by(SCENARIO, c, gas, region, Y) %>%
  summarise(
    GHG_sum = sum(GHG_agg_load, na.rm = TRUE),
    RR_agg_load = sum(RR_agg_load * GHG_agg_load, na.rm = TRUE) / GHG_sum,
    .groups = "drop"
  ) %>%
  mutate(
    SCENARIO = factor(as.character(SCENARIO), levels = scenario_levels),
    region = as.factor(region),
    c = as.factor(c),
    gas = as.factor(gas)
  )

# =========================
# RR: c × gas ごとに分けて保存（ここは今までと同じ）
# =========================
for (cc in c_sel) {
  for (g in gases_to_save) {
    df_cg <- RR_sum %>% filter(c == cc, gas == g)
    if (nrow(df_cg) == 0) next
    
    if (min(df_cg$RR_agg_load, na.rm = TRUE) < 0) {
      p_rr <- ggplot(df_cg, aes(x = Y, y = RR_agg_load, color = SCENARIO)) +
        geom_line() +
        facet_wrap(vars(region), scales = "free_y") +
        scale_color_brewer(palette = "Set1", drop = FALSE) +
        scale_x_continuous(breaks = c(2020, 2050, 2100)) +
        labs(x = NULL, y = paste0("RR (GHG-weighted over src) [", g, "] (c=", cc, ")")) +
        theme_1
    } else {
      p_rr <- ggplot(df_cg, aes(x = Y, y = RR_agg_load, color = SCENARIO)) +
        geom_line() +
        facet_wrap(vars(region), scales = "free_y") +
        scale_color_brewer(palette = "Set1", drop = FALSE) +
        scale_x_continuous(breaks = c(2020, 2050, 2100)) +
        scale_y_continuous(limits = c(0, NA)) +
        labs(x = NULL, y = paste0("RR (GHG-weighted over src) [", g, "] (c=", cc, ")")) +
        theme_1
    }
    
    ggsave(
      file.path(out_dir, paste0("RR_agg_load_line_byScenario_GHGweighted_c_", cc, "_", g, ".png")),
      p_rr, width = 15, height = 10
    )
  }
}
