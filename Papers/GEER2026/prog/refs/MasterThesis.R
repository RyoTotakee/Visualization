library(tidyverse)
library(gdxrrw)
library(ggplot2)
library(ggpmisc)
library(purrr)
library(furrr)
library(patchwork)
library(sf)
library(car)
library(MASS)
theme_1 <- theme_bw() +
  theme(
    text = element_text(size = 20), axis.text.x = element_text(size = 12),
    legend.position = "bottom",
    axis.title.x = element_blank(),
    legend.title = element_blank(),
    strip.background = element_blank(),
    strip.clip = "off"
  )
theme_2 <- theme_bw() +
  theme(
    text = element_text(size = 10), axis.text.x = element_text(size = 7, angle = 45, hjust = 1),
    legend.position = "bottom",
    axis.title.x = element_blank(),
    legend.title = element_blank(),
    strip.background = element_blank(),
    strip.clip = "off"
  )
theme_3 <- theme_bw() +
  theme(
    text = element_text(size = 20), axis.text.x = element_text(size = 12),
    legend.position = "bottom",
    legend.title = element_blank(),
    strip.background = element_blank(),
    strip.clip = "off"
  )
theme_4 <- theme_bw() +
  theme(
    text = element_text(size = 10), axis.text.x = element_text(size = 6),
    legend.position = "bottom",
    legend.title = element_blank(),
    strip.background = element_blank(),
    strip.clip = "off"
  )
theme_5 <- theme_bw() +
  theme(
    text = element_text(size = 10), axis.text.x = element_text(size = 7, angle = 45, hjust = 1),
    legend.position = "right",
    axis.title.x = element_blank(),
    legend.title = element_blank(),
    strip.background = element_blank(),
    strip.clip = "off"
  )
Model_color <- c(
  "AgLU" = "#D74322",
  "GCAM4" = "#26c3ec",
  "MESSAGE-GLOBIOM" = "#8CA231",
  "AIM/CGE" = "#007a70",
  "IMAGE" = "#a3d9f2",
  "REMIND-MAGPIE" = "#002923"
)
Model_line <- c(
  "AgLU" = 2,
  "GCAM4" = 1,
  "MESSAGE-GLOBIOM" = 1,
  "AIM/CGE" = 1,
  "IMAGE" = 1,
  "REMIND-MAGPIE" = 1
)
model_shape <- c(
  "FAOSTAT" = 1,
  "FAOSTAT_old" = 0,
  "LUH2" = 2,
  "EDGAR" = 4
)
Scenario_color <- c(
  "SSP1" = "#3cff2e",
  "SSP2" = "#1226ff",
  "SSP3" = "#c40a0a",
  "SSP4" = "#d8a417",
  "SSP5" = "#ab2cdd",
  "FAOSTAT" = "#6a6a6a"
)
AgMIP_color <- c(
  "AIM" = "#007a70",
  "AgLU" = "#D74322",
  "ENVISAGE" = "#a3d9f2",
  "FARM" = "#26c3ec",
  "MAgPIE" = "#002923",
  "GLOBIOM" = "#8CA231",
  "FAOSTAT" = "#888888",
  "LUH2" = "#707070"
)
scenario_color <- c(
  "BaU" = "#3873AD",
  "Mitigation" = "#C3327A"
)
model_shape <- c(
  "FAOSTAT" = 1,
  "LUH2" = 2,
  "EDGAR" = 4
)
land_color <- c(
  "Cropland" = "#FF8C94",
  "Forest" = "#9ECC5C",
  "Grassland" = "#5CBBCC",
  "Other natural land" = "#FFCC77",
  "Non arable land" = "#9e9e9e",
  "Energy Crop" = "#CC5CA9"
)
region_AIM <- c(
  "BRA", "CAN", "CHN", "IND", "JPN", "TUR", "USA", "XE25", "XER", "XOC", "XSE", "XSA", "XLM", "CIS", "XME", "XNF", "XAF", "World"
)
source_color <- c(
  "Enteric fermentation" = "#80E67E",
  "Manure Management" = "#E6B97E",
  "Rice Cultivation" = "#E67ED3",
  "Biomass Burning" = "#7E99E6",
  "Managed soils" = "#E37EE6"
)
var_color <- c(
  "Supply" = "#76DADA",
  "Export" = "#FFCC77",
  "Import" = "#C5A3C5",
  "Demand" = "#FFB2A1"
)
item_color <- c(
  "#7EE6BE", "#9B7EE6", "#7e8ee6", "#E6BE7E", "#DBE67E", "#C87EE6", "#7EBDE6", "#E67EDB", "#7EDCE6", "#E6977E",
  "#D8BFEA", "#e67e8e", "#7EE699", "#2960CC", "#8F29CC", "#CC297B", "#29CC73", "#5729CC", "#CC2971", "#196980", "#806919", "#801A19"
)
# data --------------------------------------------------------------------
AIM_Hub <- rgdx.param("../data/global_17_IAMC.gdx", "IAMC_Template")
UNIT <- rgdx.set("../data/global_17_IAMC.gdx", "VUMAP")
FAOSTAT <- rgdx.param("../data/IAMC_historical.gdx", "IAMC_Template")
LUH2 <- rgdx.param("../data/IAMC_historical.gdx", "IAMC_Template_LUH2")
EDGAR <- rgdx.param("../data/IAMC_historical.gdx", "IAMC_Template_EDGAR")
SSP_database <- read_csv("../data/SSP_IAM_V2_201811.csv")
AGMIP <- read.csv("../data/el2agmip_merged.csv")
AGMIP_FAO <- rgdx.param("../data/AGMIP_historical.gdx", "AgMIP_agg_hist")
AgLU_IAMC <- rgdx.param("../data/IAMC_all_scenario.gdx", "IAMC_Template")
AgLU_AGMIP <- rgdx.param("../data/AGMIP_all_scenario.gdx", "AGMIP")
AGMIP_agg <- rgdx.param("../data/AGMIP_all_scenario.gdx", "AGMIP_agg")
QSUP <- rgdx.param("../data/all_scenario_World.gdx", "QSUP_load")
RR <- rgdx.param("../data/all_scenario_World.gdx", "RR_load")
food <- rgdx.param("../data/all_scenario_World.gdx", "food_cal_load")
PC <- rgdx.param("../data/all_scenario_World.gdx", "PC_load")
Cal <- rgdx.param("../data/all_scenario_World.gdx", "Cal_pc_load")
QH <- rgdx.param("../data/all_scenario_World.gdx", "QH_load")
Yld <- rgdx.param("../data/all_scenario_World.gdx", "Yld_load")
QL <- rgdx.param("../data/all_scenario_World.gdx", "QL_load")
PP <- rgdx.param("../data/all_scenario_World.gdx", "PP_load")
QM <- rgdx.param("../data/all_scenario_World.gdx", "QM_load")
QSUP_agg <- rgdx.param("../data/all_scenario_global_17.gdx", "QSUP_agg_load")
Yld_agg <- rgdx.param("../data/all_scenario_global_17.gdx", "Yld_agg_load")
QL_agg <- rgdx.param("../data/all_scenario_global_17.gdx", "QL_agg_load")
QDEM_agg <- rgdx.param("../data/all_scenario_global_17.gdx", "QDEM_agg_load")
QM_agg <- rgdx.param("../data/all_scenario_global_17.gdx", "QM_agg_load")
QE_agg <- rgdx.param("../data/all_scenario_global_17.gdx", "QE_agg_load")
QH_agg <- rgdx.param("../data/all_scenario_global_17.gdx", "QH_agg_load")
PP_agg <- rgdx.param("../data/all_scenario_global_17.gdx", "PP_agg_load")
GDPPC <- rgdx.param("../data/baseyear.gdx", "GDPpc")
feed_efficiency <- rgdx.param("../data/data.gdx", "feed_efficiency")
feed_share <- rgdx.param("../data/data.gdx", "feed_share")
elhp <- rgdx.param("../data/data.gdx", "elhp")
SYSSHARE <- rgdx.param("../data/data.gdx", "Liv")
Cal_coef <- rgdx.param("../data/data.gdx", "Cal_coef")
QH_hist <- rgdx.param("../data/data.gdx", "QH")
Yld_hist <- rgdx.param("../data/data.gdx", "Yld")
QL_hist <- rgdx.param("../data/data.gdx", "QL")
QSUP_hist <- rgdx.param("../data/data.gdx", "QSUP")
PP_hist <- rgdx.param("../data/data.gdx", "PP")
QM_hist <- rgdx.param("../data/data.gdx", "QM")
QH_agg_hist <- rgdx.param("../data/data_agg.gdx", "QH_agg")
Yld_agg_hist <- rgdx.param("../data/data_agg.gdx", "Yld_agg")
QL_agg_hist <- rgdx.param("../data/data_agg.gdx", "QL_agg")
QSUP_agg_hist <- rgdx.param("../data/data_agg.gdx", "QSUP_agg")
PP_agg_hist <- rgdx.param("../data/data_agg.gdx", "PP_agg")
QM_agg_hist <- rgdx.param("../data/data_agg.gdx", "QM_agg")
EF <- rgdx.param("../data/baseyear.gdx", "EFbase")
region <- rgdx.set("../data/AGMIP_all_scenario.gdx", "cty_region")
region_world <- region %>% mutate(region = "World")
region_world <- full_join(region, region_world)
Boundary <- st_read("../data/Boundary/WB_countries_Admin0_10m.shp")
Boundary$ISO_A3[21] <- "FRA"
Boundary$ISO_A3[51] <- "NOR"
Boundary$ISO_A3[63] <- "XKX"
Boundary <- Boundary %>%
  dplyr::select(WB_NAME, ISO_A3) %>%
  rename("cty" = "ISO_A3")
# 4-1 ---------------------------------------------------------------------
GDPPC_2005 <- GDPPC %>%
  filter(sc == "SSP2") %>%
  filter(yr == "2005") %>%
  dplyr::select(!c(sc, yr))
data4_1 <- full_join(elhp, GDPPC_2005)
data4_1 <- data4_1 %>%
  filter(c %in% c("wht", "str", "cmt", "mlk", "vol", "alc", "sgr")) %>%
  mutate(c = dplyr::recode(c,
    "wht" = "wht,rce,mze,crl",
    "str" = "str,pls,nut,ocr,vgt,frt",
    "cmt" = "cmt,rmt,omt,pmt",
    "mlk" = "mlk",
    "vol" = "vol",
    "alc" = "stm,spc,alc",
    "sgr" = "sgr,swt,egg"
  ))
g4_1 <- data4_1 %>% ggplot() +
  geom_point(aes(x = log(GDPpc), y = elhp)) +
  labs(x = "log(GDPPC)", y = "elhp") +
  facet_wrap(vars(c), scales = "free_y") +
  theme_3
ggsave("../output/figure4_1.png", plot = g4_1, width = 14, height = 8)
c_list <- unique(as.character(data4_1$c))
regresult <- as.data.frame(c_list)
regresult <- regresult %>% mutate(alpha = 0, beta = 0, r2 = 0)
for (i in c_list) {
  data4_1_c <- data4_1 %>%
    filter(c == i) %>%
    na.omit()
  regression <- lm(data = data4_1_c, elhp ~ log(GDPpc))
  regresult_temp <- summary(regression)
  regresult[c_list == i, "alpha"] <- regresult_temp$coefficients[1]
  regresult[c_list == i, "beta"] <- regresult_temp$coefficients[2]
  regresult[c_list == i, "r2"] <- regresult_temp$r.squared
  regresult[c_list == i, "lower"] <- confint(regression, level = 0.95)[2, 1]
  regresult[c_list == i, "upper"] <- confint(regression, level = 0.95)[2, 2]
  regresult[c_list == i, "p"] <- regresult_temp$coefficients[1, 4]
}
write.csv(regresult, file = "../output/table4_1.csv")
# 4-2 ---------------------------------------------------------------------
data4_2 <- SYSSHARE %>%
  filter(sys != "tot") %>%
  mutate(cz = dplyr::recode(cz,
    "A" = "Arid",
    "T" = "Temperate",
    "H" = "Humid",
    "Y" = "HyperArid"
  )) %>%
  pivot_wider(names_from = "sys", values_from = "Liv")
data4_2[is.na(data4_2)] <- 0
data4_2 <- data4_2 %>%
  mutate(TOT = MX + LG) %>%
  mutate(MX = MX / TOT, LG = LG / TOT) %>%
  dplyr::select(!TOT) %>%
  mutate(MX = log(MX / (1 - MX)), LG = log(LG / (1 - LG))) %>%
  dplyr::select(!MX) %>%
  filter(!LG %in% c(Inf, -Inf))
data4_2 <- left_join(data4_2, GDPPC_2005)
g4_2 <- data4_2 %>% ggplot() +
  geom_point(aes(x = log(GDPpc), y = LG, color = cz)) +
  labs(x = "log(GDPPC)", y = "logit(SYSSHARE)") +
  facet_wrap(vars(c, cz), scales = "free_y") +
  theme_3
ggsave("../output/figure4_2.png", plot = g4_2, width = 16, height = 10)
c_list <- unique(data4_2$c)
regresult <- as.data.frame(c_list)
regresult <- regresult %>% mutate(ro = 0, delta_A = 0, delta_T = 0, delta_H = 0, delta_Y = 0, r2 = 0, upper = 0, lower = 0, p = 0)
for (i in c_list) {
  data4_2_c <- data4_2 %>% filter(c == i)
  regression <- lm(data = data4_2_c, LG ~ -1 + log(GDPpc) + cz)
  regresult_temp <- summary(regression)
  regresult[c_list == i, "ro"] <- regresult_temp$coefficients[1]
  regresult[c_list == i, "delta_A"] <- regresult_temp$coefficients[2]
  regresult[c_list == i, "delta_T"] <- regresult_temp$coefficients[3]
  regresult[c_list == i, "delta_H"] <- regresult_temp$coefficients[4]
  regresult[c_list == i, "delta_Y"] <- regresult_temp$coefficients[5]
  regresult[c_list == i, "r2"] <- regresult_temp$adj.r.squared
  regresult[c_list == i, "lower"] <- confint(regression, level = 0.95)[1, 1]
  regresult[c_list == i, "upper"] <- confint(regression, level = 0.95)[1, 2]
  regresult[c_list == i, "p"] <- regresult_temp$coefficients[1, 4]
}
write.csv(regresult, file = "../output/table4_2.csv")
# 4-3 ---------------------------------------------------------------------
data4_3 <- left_join(feed_efficiency, GDPPC_2005) %>%
  mutate(
    cz = dplyr::recode(cz,
      "A" = "Arid",
      "T" = "Temperate",
      "H" = "Humid",
      "Y" = "HyperArid",
      "tot" = "Total"
    ),
    sys = dplyr::recode(sys, "tot" = "Total")
  ) %>%
  mutate(c = factor(c, levels = c("cmt", "rmt", "mlk", "pmt", "omt")))
g4_3 <- data4_3 %>% ggplot() +
  geom_point(aes(x = log(GDPpc), y = log(feed_efficiency), color = cz)) +
  facet_wrap(vars(cz, sys, c), scales = "free_y", ncol = 6) +
  labs(x = "log(GDPPC)", y = "log(FEEDEFFICIENCY)") +
  theme_3
ggsave("../output/figure4_3.png", g4_3, width = 18, height = 14, create.dir = T)
c_list <- c("cmt", "rmt", "mlk")
regresult <- as.data.frame(c_list)
regresult <- regresult %>% mutate(beta = 0, alpha_A = 0, alpha_T = 0, alpha_H = 0, alpha_Y = 0, r2 = 0)
for (j in c("LG", "MX")) {
  data4_3_sys <- data4_3 %>% filter(sys == j)
  for (i in c_list) {
    data4_3_c <- data4_3_sys %>% filter(c == i)
    regression <- lm(data = data4_3_c, log(feed_efficiency) ~ -1 + log(GDPpc) + cz)
    regresult_temp <- summary(regression)
    regresult[c_list == i, "beta"] <- regresult_temp$coefficients[1]
    regresult[c_list == i, "alpha_A"] <- regresult_temp$coefficients[2]
    regresult[c_list == i, "alpha_T"] <- regresult_temp$coefficients[3]
    regresult[c_list == i, "alpha_H"] <- regresult_temp$coefficients[4]
    regresult[c_list == i, "alpha_Y"] <- regresult_temp$coefficients[5]
    regresult[c_list == i, "r2"] <- regresult_temp$adj.r.squared
    regresult[c_list == i, "lower"] <- confint(regression, level = 0.95)[1, 1]
    regresult[c_list == i, "upper"] <- confint(regression, level = 0.95)[1, 2]
    regresult[c_list == i, "p"] <- regresult_temp$coefficients[1, 4]
    regresult <- regresult %>% mutate(sys = j)
  }
  if (j == "LG") {
    regresult_agg <- regresult
  } else {
    regresult_agg <- rbind(regresult_agg, regresult)
  }
}
c_list <- c("pmt", "omt")
regresult <- as.data.frame(c_list)
regresult <- regresult %>% mutate(beta = 0, alpha_tot = 0, r2 = 0, sys = "Total")
data4_3_sys <- data4_3 %>% filter(sys == "Total")
for (i in c_list) {
  data4_3_c <- data4_3_sys %>% filter(c == i)
  regression <- lm(data = data4_3_c, log(feed_efficiency) ~ log(GDPpc))
  regresult_temp <- summary(regression)
  regresult[c_list == i, "beta"] <- regresult_temp$coefficients[2]
  regresult[c_list == i, "alpha_tot"] <- regresult_temp$coefficients[1]
  regresult[c_list == i, "r2"] <- regresult_temp$r.squared
  regresult[c_list == i, "lower"] <- confint(regression, level = 0.95)[2, 1]
  regresult[c_list == i, "upper"] <- confint(regression, level = 0.95)[2, 2]
  regresult[c_list == i, "p"] <- regresult_temp$coefficients[1, 4]
}
regresult <- full_join(regresult_agg, regresult)
write.csv(regresult, file = "../output/table4_3.csv")
# 4-4 ---------------------------------------------------------------------
data4_4 <- left_join(feed_share, GDPPC_2005) %>%
  mutate(cz = dplyr::recode(cz,
    "A" = "Arid",
    "T" = "Temperate",
    "H" = "Humid",
    "Y" = "HyperArid"
  )) %>%
  filter(!c %in% c("omt", "pmt")) %>%
  mutate(logit_feed_share = log(feed_share / (1 - feed_share)))

g4_4 <- data4_4 %>% ggplot() +
  geom_point(aes(x = log(GDPpc), y = logit_feed_share, color = cz)) +
  facet_wrap(vars(cz, sys, c), scales = "free_y", ncol = 6) +
  labs(x = "log(GDPPC)", y = "logit(FEEDSHARE)") +
  theme_3
ggsave("../output/figure4_4.png", g4_4, width = 18, height = 12, create.dir = T)
c_list <- c("cmt", "rmt", "mlk")
regresult <- as.data.frame(c_list)
regresult <- regresult %>% mutate(beta = 0, alpha_A = 0, alpha_T = 0, alpha_H = 0, alpha_Y = 0, r2 = 0)
for (j in c("LG", "MX")) {
  data4_4_sys <- data4_4 %>% filter(sys == j)
  for (i in c_list) {
    data4_4_c <- data4_4_sys %>% filter(c == i)
    regression <- lm(data = data4_4_c, logit_feed_share ~ -1 + log(GDPpc) + cz)
    regresult_temp <- summary(regression)
    regresult[c_list == i, "beta"] <- regresult_temp$coefficients[1]
    regresult[c_list == i, "alpha_A"] <- regresult_temp$coefficients[2]
    regresult[c_list == i, "alpha_T"] <- regresult_temp$coefficients[3]
    regresult[c_list == i, "alpha_H"] <- regresult_temp$coefficients[4]
    regresult[c_list == i, "alpha_Y"] <- regresult_temp$coefficients[5]
    regresult[c_list == i, "r2"] <- regresult_temp$adj.r.squared
    regresult[c_list == i, "lower"] <- confint(regression, level = 0.95)[1, 1]
    regresult[c_list == i, "upper"] <- confint(regression, level = 0.95)[1, 2]
    regresult[c_list == i, "p"] <- regresult_temp$coefficients[1, 4]
    regresult <- regresult %>% mutate(sys = j)
  }
  if (j == "LG") {
    regresult_agg <- regresult
  } else {
    regresult_agg <- rbind(regresult_agg, regresult)
  }
}
write.csv(regresult_agg, file = "../output/table4_4.csv")
# 4-5 ---------------------------------------------------------------------
SSP_agr <- SSP_database %>%
  filter(VARIABLE %in% c("Agricultural Demand|Crops", "Agricultural Demand|Livestock", "Agricultural Production|Crops|Non-Energy", "Agricultural Production|Livestock", "Land Cover|Cropland", "Land Cover|Forest", "Land Cover|Pasture", "Emissions|CH4|Land Use", "Emissions|N2O|Land Use")) %>%
  filter(MODEL != "AIM/CGE" | VARIABLE != "Agricultural Demand|Crops") %>%
  filter(SCENARIO == "SSP2-Baseline") %>%
  mutate(region = dplyr::recode(REGION,
    "R5.2ASIA" = "R5ASIA",
    "R5.2LAM" = "R5LAM",
    "R5.2MAF" = "R5MAF",
    "R5.2OECD" = "R5OECD90+EU",
    "R5.2REF" = "R5REF"
  )) %>%
  mutate(VARIABLE = dplyr::recode(VARIABLE,
    "Agricultural Demand|Crops" = "CONS_CRP",
    "Agricultural Demand|Livestock" = "CONS_LSP",
    "Agricultural Production|Crops|Non-Energy" = "PROD_CRP",
    "Agricultural Production|Livestock" = "PROD_LSP",
    "Land Cover|Cropland" = "LAND_CRP",
    "Land Cover|Forest" = "LAND_FOR",
    "Land Cover|Pasture" = "LAND_GRS",
    "Emissions|CH4|Land Use" = "ECH4_TOT",
    "Emissions|N2O|Land Use" = "EN2O_TOT"
  )) %>%
  dplyr::select(!c(SCENARIO, REGION, UNIT)) %>%
  dplyr::select(!"2005")
SSP_agr <- SSP_agr %>% pivot_longer(cols = -c(MODEL, region, VARIABLE), names_to = "Y", values_to = "AGMIP_agg")
FAOSTAT4_5 <- FAOSTAT %>%
  mutate(
    VARIABLE = as.character(IAMC_VAR),
    region = as.character(IAMC_REGION),
    Y = as.numeric(as.character(yr)),
    Model2 = "FAOSTAT",
    Historical = IAMC_Template
  ) %>%
  dplyr::select(!c(IAMC_VAR, IAMC_REGION, yr, IAMC_Template))
LUH24_5 <- LUH2 %>%
  mutate(
    VARIABLE = as.character(IAMC_VAR),
    region = as.character(IAMC_REGION),
    Y = as.numeric(as.character(yr)),
    Model2 = "LUH2",
    Historical = IAMC_Template_LUH2
  ) %>%
  dplyr::select(!c(IAMC_VAR, IAMC_REGION, yr, IAMC_Template_LUH2))
EDGAR4_5 <- EDGAR %>%
  mutate(
    VARIABLE = as.character(IAMC_VAR),
    region = as.character(IAMC_REGION),
    Y = as.numeric(as.character(yr)),
    Model2 = "EDGAR",
    Historical = IAMC_Template_EDGAR
  ) %>%
  dplyr::select(!c(IAMC_VAR, IAMC_REGION, yr, IAMC_Template_EDGAR))
Historical <- full_join(FAOSTAT4_5, LUH24_5)
Historical <- full_join(Historical, EDGAR4_5) %>%
  filter(Y > 2000, Y < 2021, region %in% c("World", "R5ASIA", "R5LAM", "R5MAF", "R5OECD90+EU", "R5REF")) %>%
  mutate(VARIABLE = dplyr::recode(VARIABLE,
    "Agr_Dem_Cro" = "CONS_CRP",
    "Agr_Dem_Liv" = "CONS_LSP",
    "Agr_Pro_Cro" = "PROD_CRP",
    "Agr_Pro_Liv" = "PROD_LSP",
    "Lan_Cov_Cro" = "LAND_CRP",
    "Lan_Cov_Frs" = "LAND_FOR",
    "Lan_Cov_Pst" = "LAND_GRS",
    "Emi_CH4_AFO" = "ECH4_TOT",
    "Emi_N2O_AFO" = "EN2O_TOT"
  )) %>%
  mutate(Y = as.factor(Y))
data4_5 <- AGMIP_agg %>%
  filter(SCENARIO == "SSP2_BaU_NoCC") %>%
  filter(region %in% c("World", "R5ASIA", "R5LAM", "R5MAF", "R5OECD90+EU", "R5REF")) %>%
  filter(!AgMIP_UNIT %in% c("USD", "million kcal", "1000 t")) %>%
  filter(AgMIP_VAR %in% c("LAND", "PROD", "CONS", "ECH4", "EN2O")) %>%
  mutate(AGMIP_agg = AGMIP_agg / 1000) %>%
  unite(VARIABLE, AgMIP_VAR, ITEM, remove = T, sep = "_") %>%
  mutate(VARIABLE = toupper(VARIABLE)) %>%
  dplyr::select(!c(SCENARIO, AgMIP_UNIT)) %>%
  mutate(MODEL = "AgLU")
data4_5$AGMIP_agg[data4_5$VARIABLE == "ECH4_TOT"] <- data4_5$AGMIP_agg[data4_5$VARIABLE == "ECH4_TOT"] / 28 * 1000
data4_5$AGMIP_agg[data4_5$VARIABLE == "EN2O_TOT"] <- data4_5$AGMIP_agg[data4_5$VARIABLE == "EN2O_TOT"] / 265 * 1000 * 1000
data4_5 <- full_join(data4_5, SSP_agr)
data4_5 <- full_join(data4_5, Historical) %>%
  mutate(Y = as.numeric(Y)) %>%
  mutate(VARIABLE = dplyr::recode(VARIABLE,
    "CONS_CRP" = "Crop Consumption [million t-DM]",
    "CONS_LSP" = "Livestock Consumption [million t-DM]",
    "PROD_CRP" = "Crop Production [million t-DM]",
    "PROD_LSP" = "Livestock Production [million t-DM]",
    "LAND_CRP" = "Crop Land [million ha]",
    "LAND_FOR" = "Forest [million ha]",
    "LAND_GRS" = "Pasture [million ha]",
    "ECH4_TOT" = "CH4 emission [Mt CH4/yr]",
    "EN2O_TOT" = "N2O emission [kt N2O/yr]"
  ))
VAR_list <- c("Crop Consumption [million t-DM]", "Livestock Consumption [million t-DM]", "Crop Production [million t-DM]", "Livestock Production [million t-DM]", "Crop Land [million ha]", "Forest [million ha]", "Pasture [million ha]", "CH4 emission [Mt CH4/yr]", "N2O emission [kt N2O/yr]")
data4_5 <- data4_5 %>%
  filter(VARIABLE %in% VAR_list) %>%
  mutate(VARIABLE = factor(VARIABLE, levels = c("Crop Consumption [million t-DM]", "Crop Production [million t-DM]", "Crop Land [million ha]", "Livestock Consumption [million t-DM]", "Livestock Production [million t-DM]", "Pasture [million ha]", "Forest [million ha]", "CH4 emission [Mt CH4/yr]", "N2O emission [kt N2O/yr]"))) %>%
  mutate(region = dplyr::recode(region,
    "R5ASIA" = "Asia",
    "R5LAM" = "Latin America and the Caribbean",
    "R5MAF" = "Middle East and Africa",
    "R5OECD90+EU" = "OECD+EU",
    "R5REF" = "Eastern Europe and the Former Soviet Union"
  ))
region_list <- unique(data4_5$region)
data4_5_r <- data4_5 %>%
  filter(region == "World") %>%
  dplyr::select(MODEL, VARIABLE, region, Y, AGMIP_agg) %>%
  na.omit()
data4_5_r_hist <- data4_5 %>%
  filter(region == "World") %>%
  dplyr::select(Model2, VARIABLE, region, Y, Historical) %>%
  na.omit()
g4_5 <- ggplot() +
  geom_line(data = data4_5_r, aes(x = Y, y = AGMIP_agg, color = MODEL, linewidth = MODEL)) +
  geom_point(data = data4_5_r_hist, aes(x = Y, y = Historical, shape = Model2), color = "#777777", size = 2.5) +
  scale_color_manual(values = Model_color) +
  scale_linewidth_manual(values = Model_line) +
  scale_shape_manual(values = model_shape) +
  facet_wrap(vars(VARIABLE), scales = "free_y") +
  scale_x_continuous(breaks = seq(2000, 2100, 20)) +
  labs(x = "年", y = "") +
  theme_1 +
  theme(axis.title.y = element_blank())
ggsave("../output/figure4_5.png", g4_5, width = 15, height = 10, create.dir = T)
# 4-6 ---------------------------------------------------------------------
data4_6 <- AGMIP_agg %>%
  filter(SCENARIO == "SSP2_BaU_NoCC") %>%
  mutate(AgMIP_VAR = dplyr::recode(AgMIP_VAR, "LSP" = "PAS")) %>%
  unite(col = variable, ITEM, AgMIP_VAR, sep = "_") %>%
  mutate(variable = paste0("AGMIP_", variable)) %>%
  mutate(variable = toupper(variable)) %>%
  rename("AgLU" = "AGMIP_agg", "UNIT" = "AgMIP_UNIT") %>%
  mutate(Y = as.character(Y)) %>%
  mutate(Y = as.numeric(Y))
AGMIP4_6 <- AGMIP %>%
  filter(Scenario == "BAU_NoCC") %>%
  unite(col = variable, Sector, Indicator, sep = "_") %>%
  mutate(variable = paste0("AGMIP_", variable)) %>%
  rename("UNIT" = "Unit", "Y" = "Year", "region" = "Region") %>%
  dplyr::select(!Scenario) %>%
  pivot_wider(names_from = "Model", values_from = "Value") %>%
  mutate(UNIT = dplyr::recode(UNIT, "t/ha" = "dm t/ha"))
list <- inner_join(data4_6, AGMIP4_6)
var_list <- unique(list$variable)
unit_list <- unique(list$UNIT)
region_list <- c("CAN", "USA", "BRA", "OSA", "FSU", "EUR", "MEN", "SSA", "CHN", "IND", "SEA", "OAS", "ANZ")
data4_6 <- full_join(data4_6, AGMIP4_6)
data4_6 <- data4_6 %>%
  filter(variable %in% var_list, UNIT %in% unit_list, region %in% region_list) %>%
  dplyr::select(!SCENARIO) %>%
  pivot_longer(cols = -c(region, variable, Y, UNIT), names_to = "Model", values_to = "Value") %>%
  mutate(UNIT = as.character(UNIT)) %>%
  na.omit() %>%
  mutate(region = dplyr::recode(region,
    "CAN" = "Canada",
    "USA" = "USA",
    "BRA" = "Brazil",
    "CHN" = "China",
    "IND" = "India",
    "OSA" = "Other South&\nCentral America",
    "FSU" = "Former Soviet Union",
    "EUR" = "Europe",
    "MEN" = "Middle-East&\nNorth Africa",
    "SSA" = "Sub-Saharan \nAfrica",
    "SEA" = "South-East Asia",
    "OAS" = "Other Asia",
    "ANZ" = "Australia/New Zealand"
  ))
AGMIP_FAO4_6 <- AGMIP_FAO %>%
  mutate(AgMIP_VAR = dplyr::recode(AgMIP_VAR, "LSP" = "PAS")) %>%
  unite(col = variable, ITEM, AgMIP_VAR, sep = "_") %>%
  mutate(variable = paste0("AGMIP_", variable)) %>%
  mutate(variable = toupper(variable)) %>%
  rename("Historical" = "AGMIP_agg_hist", "UNIT" = "AgMIP_UNIT", "Y" = "yr") %>%
  mutate(Y = as.character(Y), Model2 = "FAOSTAT") %>%
  mutate(Y = as.numeric(Y)) %>%
  filter(region %in% region_list, UNIT %in% unit_list) %>%
  filter(Y > 2000, Y < 2021) %>%
  mutate(region = dplyr::recode(region,
    "CAN" = "Canada",
    "USA" = "USA",
    "BRA" = "Brazil",
    "CHN" = "China",
    "IND" = "India",
    "OSA" = "Other South&\nCentral America",
    "FSU" = "Former Soviet Union",
    "EUR" = "Europe",
    "MEN" = "Middle-East&\nNorth Africa",
    "SSA" = "Sub-Saharan \nAfrica",
    "SEA" = "South-East Asia",
    "OAS" = "Other Asia",
    "ANZ" = "Australia/New Zealand"
  ))
data4_6_var <- data4_6 %>%
  filter(variable == "AGMIP_WHT_YILD")
AGMIP_FAO4_6_var <- AGMIP_FAO4_6 %>%
  filter(variable == "AGMIP_WHT_YILD")
g4_6_1 <- ggplot() +
  geom_line(data = data4_6_var, aes(x = Y, y = Value, color = Model), linewidth = 0.5) +
  geom_point(data = AGMIP_FAO4_6_var, aes(x = Y, y = Historical, shape = Model2), color = "#777777", size = 1) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 2015, linetype = "dotted") +
  scale_color_manual(values = AgMIP_color) +
  scale_shape_manual(values = model_shape) +
  facet_wrap(vars(region), scales = "free_y", ncol = 5) +
  scale_x_continuous(breaks = seq(2000, 2100, 50)) +
  scale_y_continuous(limits = c(0, NA)) +
  labs(y = "Yield [t-DM/ha]", title = "Wheat") +
  theme_5 +
  theme(legend.position = "none", axis.title.y = element_blank())
data4_6_var <- data4_6 %>%
  filter(variable == "AGMIP_RIC_YILD")
AGMIP_FAO4_6_var <- AGMIP_FAO4_6 %>%
  filter(variable == "AGMIP_RIC_YILD")
g4_6_2 <- ggplot() +
  geom_line(data = data4_6_var, aes(x = Y, y = Value, color = Model), linewidth = 0.5) +
  geom_point(data = AGMIP_FAO4_6_var, aes(x = Y, y = Historical, shape = Model2), color = "#777777", size = 1) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 2015, linetype = "dotted") +
  scale_color_manual(values = AgMIP_color) +
  scale_shape_manual(values = model_shape) +
  facet_wrap(vars(region), scales = "free_y", ncol = 5) +
  scale_x_continuous(breaks = seq(2000, 2100, 50)) +
  scale_y_continuous(limits = c(0, NA)) +
  labs(y = "Yield [t-DM/ha]", title = "Rice") +
  theme_5 +
  theme(legend.position = "none", axis.title.y = element_blank())
data4_6_var <- data4_6 %>%
  filter(variable == "AGMIP_CGR_YILD")
AGMIP_FAO4_6_var <- AGMIP_FAO4_6 %>%
  filter(variable == "AGMIP_CGR_YILD")
g4_6_3 <- ggplot() +
  geom_line(data = data4_6_var, aes(x = Y, y = Value, color = Model), linewidth = 0.5) +
  geom_point(data = AGMIP_FAO4_6_var, aes(x = Y, y = Historical, shape = Model2), color = "#777777", size = 1) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 2015, linetype = "dotted") +
  scale_color_manual(values = AgMIP_color) +
  scale_shape_manual(values = model_shape) +
  facet_wrap(vars(region), scales = "free_y", ncol = 5) +
  scale_x_continuous(breaks = seq(2000, 2100, 50)) +
  scale_y_continuous(limits = c(0, NA)) +
  labs(y = "Yield [t-DM/ha]", title = "Other Grains") +
  theme_5 +
  theme(legend.position = "none")
data4_6_var <- data4_6 %>%
  filter(variable == "AGMIP_OSD_YILD")
AGMIP_FAO4_6_var <- AGMIP_FAO4_6 %>%
  filter(variable == "AGMIP_OSD_YILD")
data4_6_var$Value[data4_6_var$Model == "FARM"] <- data4_6_var$Value[data4_6_var$Model == "FARM"] * 0.545
g4_6_4 <- ggplot() +
  geom_line(data = data4_6_var, aes(x = Y, y = Value, color = Model), linewidth = 0.5) +
  geom_point(data = AGMIP_FAO4_6_var, aes(x = Y, y = Historical, shape = Model2), color = "#777777", size = 1) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 2015, linetype = "dotted") +
  scale_color_manual(values = AgMIP_color) +
  scale_shape_manual(values = model_shape) +
  facet_wrap(vars(region), scales = "free_y", ncol = 5) +
  scale_x_continuous(breaks = seq(2000, 2100, 50)) +
  scale_y_continuous(limits = c(0, NA)) +
  labs(y = "Yield [t-DM/ha]", title = "Oil Crops") +
  theme_5 +
  theme(legend.position = "none", axis.title.y = element_blank())
data4_6_var <- data4_6 %>%
  filter(variable == "AGMIP_VFN_YILD")
AGMIP_FAO4_6_var <- AGMIP_FAO4_6 %>%
  filter(variable == "AGMIP_VFN_YILD")
g4_6_5 <- ggplot() +
  geom_line(data = data4_6_var, aes(x = Y, y = Value, color = Model), linewidth = 0.5) +
  geom_point(data = AGMIP_FAO4_6_var, aes(x = Y, y = Historical, shape = Model2), color = "#777777", size = 1) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 2015, linetype = "dotted") +
  scale_color_manual(values = AgMIP_color) +
  scale_shape_manual(values = model_shape) +
  facet_wrap(vars(region), scales = "free_y", ncol = 5) +
  scale_x_continuous(breaks = seq(2000, 2100, 50)) +
  scale_y_continuous(limits = c(0, NA)) +
  labs(y = "Yield [t-DM/ha]", title = "Vegetables, fruits, nuts") +
  theme_5 +
  theme(legend.position = "none", axis.title.y = element_blank())
data4_6_var <- data4_6 %>%
  filter(variable == "AGMIP_SGC_YILD")
data4_6_var$Value[data4_6_var$Model == "FARM"] <- data4_6_var$Value[data4_6_var$Model == "FARM"] * 0.25
AGMIP_FAO4_6_var <- AGMIP_FAO4_6 %>%
  filter(variable == "AGMIP_SGC_YILD")
g4_6_6 <- ggplot() +
  geom_line(data = data4_6_var, aes(x = Y, y = Value, color = Model), linewidth = 0.5) +
  geom_point(data = AGMIP_FAO4_6_var, aes(x = Y, y = Historical, shape = Model2), color = "#777777", size = 1) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 2015, linetype = "dotted") +
  scale_color_manual(values = AgMIP_color) +
  scale_shape_manual(values = model_shape) +
  facet_wrap(vars(region), scales = "free_y", ncol = 5) +
  scale_x_continuous(breaks = seq(2000, 2100, 50)) +
  scale_y_continuous(limits = c(0, NA)) +
  labs(y = "Yield [t-DM/ha]", title = "Sugar Crops") +
  theme_5 +
  theme(
    legend.position = "bottom",
    axis.title.y = element_blank(),
    legend.text = element_text(size = 10),
    legend.justification = c(6, 0)
  )
g4_6 <- g4_6_1 + g4_6_2 + g4_6_3 + g4_6_4 + g4_6_5 + g4_6_6 + plot_layout(ncol = 2)
ggsave("../output/figure4_6.png", g4_6, width = 9, height = 12.5, create.dir = T)
# 4-7~4-12 ---------------------------------------------------------------------
SSP_agr <- SSP_database %>%
  filter(VARIABLE %in% c("Agricultural Demand|Crops", "Agricultural Demand|Livestock", "Agricultural Production|Crops|Non-Energy", "Agricultural Production|Livestock", "Land Cover|Cropland", "Land Cover|Forest", "Land Cover|Pasture", "Emissions|CH4|Land Use", "Emissions|N2O|Land Use")) %>%
  filter(SCENARIO %in% c("SSP1-Baseline", "SSP2-Baseline", "SSP3-Baseline", "SSP4-Baseline", "SSP5-Baseline")) %>%
  filter(MODEL != "AIM/CGE" | VARIABLE != "Agricultural Demand|Crops") %>%
  mutate(region = dplyr::recode(REGION,
    "R5.2ASIA" = "R5ASIA",
    "R5.2LAM" = "R5LAM",
    "R5.2MAF" = "R5MAF",
    "R5.2OECD" = "R5OECD90+EU",
    "R5.2REF" = "R5REF"
  )) %>%
  mutate(SCENARIO = dplyr::recode(SCENARIO,
    "SSP1-Baseline" = "SSP1",
    "SSP2-Baseline" = "SSP2",
    "SSP3-Baseline" = "SSP3",
    "SSP4-Baseline" = "SSP4",
    "SSP5-Baseline" = "SSP5"
  )) %>%
  mutate(VARIABLE = dplyr::recode(VARIABLE,
    "Agricultural Demand|Crops" = "CONS_CRP",
    "Agricultural Demand|Livestock" = "CONS_LSP",
    "Agricultural Production|Crops|Non-Energy" = "PROD_CRP",
    "Agricultural Production|Livestock" = "PROD_LSP",
    "Land Cover|Cropland" = "LAND_CRP",
    "Land Cover|Forest" = "LAND_FOR",
    "Land Cover|Pasture" = "LAND_GRS",
    "Emissions|CH4|Land Use" = "ECH4_TOT",
    "Emissions|N2O|Land Use" = "EN2O_TOT"
  )) %>%
  dplyr::select(!c(REGION, UNIT)) %>%
  dplyr::select(!"2005")
SSP_agr <- SSP_agr %>% pivot_longer(cols = -c(MODEL, SCENARIO, region, VARIABLE), names_to = "Y", values_to = "AGMIP_agg")
data4_7 <- AGMIP_agg %>%
  filter(SCENARIO %in% c("SSP1_BaU_NoCC", "SSP2_BaU_NoCC", "SSP3_BaU_NoCC", "SSP4_BaU_NoCC", "SSP5_BaU_NoCC")) %>%
  filter(region %in% c("World", "R5ASIA", "R5LAM", "R5MAF", "R5OECD90+EU", "R5REF")) %>%
  filter(!AgMIP_UNIT %in% c("USD", "million kcal", "1000 t")) %>%
  filter(AgMIP_VAR %in% c("LAND", "PROD", "CONS", "ECH4", "EN2O")) %>%
  mutate(SCENARIO = dplyr::recode(SCENARIO,
    "SSP1_BaU_NoCC" = "SSP1",
    "SSP2_BaU_NoCC" = "SSP2",
    "SSP3_BaU_NoCC" = "SSP3",
    "SSP4_BaU_NoCC" = "SSP4",
    "SSP5_BaU_NoCC" = "SSP5"
  )) %>%
  mutate(AGMIP_agg = AGMIP_agg / 1000) %>%
  unite(VARIABLE, AgMIP_VAR, ITEM, remove = T, sep = "_") %>%
  mutate(VARIABLE = toupper(VARIABLE)) %>%
  dplyr::select(!c(AgMIP_UNIT)) %>%
  mutate(MODEL = "AgLU")
data4_7$AGMIP_agg[data4_7$VARIABLE == "ECH4_TOT"] <- data4_7$AGMIP_agg[data4_7$VARIABLE == "ECH4_TOT"] / 28 * 1000
data4_7$AGMIP_agg[data4_7$VARIABLE == "EN2O_TOT"] <- data4_7$AGMIP_agg[data4_7$VARIABLE == "EN2O_TOT"] / 265 * 1000 * 1000

data4_7 <- full_join(SSP_agr, data4_7) %>%
  mutate(Y = as.numeric(Y)) %>%
  mutate(VARIABLE = dplyr::recode(VARIABLE,
    "CONS_CRP" = "Crop Consumption [million t-DM]",
    "CONS_LSP" = "Livestock Consumption [million t-DM]",
    "PROD_CRP" = "Crop Production [million t-DM]",
    "PROD_LSP" = "Livestock Production [million t-DM]",
    "LAND_CRP" = "Crop Land [million ha]",
    "LAND_FOR" = "Forest [million ha]",
    "LAND_GRS" = "Pasture [million ha]",
    "ECH4_TOT" = "CH4 emission [Mt CH4/yr]",
    "EN2O_TOT" = "N2O emission [kt N2O/yr]"
  ))
VAR_list <- c("Crop Consumption [million t-DM]", "Livestock Consumption [million t-DM]", "Crop Production [million t-DM]", "Livestock Production [million t-DM]", "Crop Land [million ha]", "Forest [million ha]", "Pasture [million ha]", "CH4 emission [Mt CH4/yr]", "N2O emission [kt N2O/yr]")
data4_7 <- data4_7 %>%
  filter(VARIABLE %in% VAR_list) %>%
  mutate(VARIABLE = factor(VARIABLE, levels = c("Crop Consumption [million t-DM]", "Crop Production [million t-DM]", "Crop Land [million ha]", "Livestock Consumption [million t-DM]", "Livestock Production [million t-DM]", "Pasture [million ha]", "Forest [million ha]", "CH4 emission [Mt CH4/yr]", "N2O emission [kt N2O/yr]"))) %>%
  mutate(region = dplyr::recode(region,
    "R5ASIA" = "Asia",
    "R5LAM" = "Latin America and the Caribbean",
    "R5MAF" = "Middle East and Africa",
    "R5OECD90+EU" = "OECD+EU",
    "R5REF" = "Eastern Europe and the Former Soviet Union"
  ))
region_list <- unique(data4_7$region)
levels(data4_7$VARIABLE) <- c("Crop \nConsumption \n[million t-DM]", "Crop \nProduction \n[million t-DM]", "Crop Land \n[million ha]", "Livestock \nConsumption \n[million t-DM]", "Livestock \nProduction \n[million t-DM]", "Pasture \n[million ha]", "Forest \n[million ha]", "CH4 emission \n[Mt CH4/yr]", "N2O emission \n[kt N2O/yr]")
data4_7 <- data4_7 %>% pivot_wider(names_from = "MODEL", values_from = "AGMIP_agg")
data4_7$IAM_max <- apply(data4_7[, 5:9], 1, max, na.rm = T)
data4_7$IAM_min <- apply(data4_7[, 5:9], 1, min, na.rm = T)
data4_7_AgLU <- data4_7 %>%
  dplyr::select(SCENARIO, VARIABLE, region, Y, AgLU) %>%
  na.omit()
data4_7_IAM <- data4_7 %>%
  dplyr::select(SCENARIO, VARIABLE, region, Y, IAM_max, IAM_min) %>%
  na.omit() %>%
  filter(Y %in% seq(2010, 2100, 10))
data4_7_FAOSTAT <- FAOSTAT %>%
  mutate(
    VARIABLE = as.character(IAMC_VAR),
    region = as.character(IAMC_REGION),
    Y = as.numeric(as.character(yr)),
    Model2 = "FAOSTAT",
    Historical = IAMC_Template
  ) %>%
  dplyr::select(!c(IAMC_VAR, IAMC_REGION, yr, IAMC_Template))
data4_7_LUH2 <- LUH2 %>%
  mutate(
    VARIABLE = as.character(IAMC_VAR),
    region = as.character(IAMC_REGION),
    Y = as.numeric(as.character(yr)),
    Model2 = "LUH2",
    Historical = IAMC_Template_LUH2
  ) %>%
  dplyr::select(!c(IAMC_VAR, IAMC_REGION, yr, IAMC_Template_LUH2))
data4_7_EDGAR <- EDGAR %>%
  mutate(
    VARIABLE = as.character(IAMC_VAR),
    region = as.character(IAMC_REGION),
    Y = as.numeric(as.character(yr)),
    Model2 = "EDGAR",
    Historical = IAMC_Template_EDGAR
  ) %>%
  dplyr::select(!c(IAMC_VAR, IAMC_REGION, yr, IAMC_Template_EDGAR))
data4_7_Historical <- full_join(data4_7_FAOSTAT, data4_7_LUH2)
data4_7_Historical <- full_join(data4_7_Historical, data4_7_EDGAR) %>%
  filter(Y > 2000, Y < 2021, region %in% c("World", "R5ASIA", "R5LAM", "R5MAF", "R5OECD90+EU", "R5REF")) %>%
  mutate(VARIABLE = dplyr::recode(VARIABLE,
    "Agr_Dem_Cro" = "Crop Consumption [million t-DM]",
    "Agr_Dem_Liv" = "Livestock Consumption [million t-DM]",
    "Agr_Pro_Cro" = "Crop Production [million t-DM]",
    "Agr_Pro_Liv" = "Livestock Production [million t-DM]",
    "Lan_Cov_Cro" = "Crop Land [million ha]",
    "Lan_Cov_Frs" = "Forest [million ha]",
    "Lan_Cov_Pst" = "Pasture [million ha]",
    "Emi_CH4_AFO" = "CH4 emission [Mt CH4/yr]",
    "Emi_N2O_AFO" = "N2O emission [kt N2O/yr]"
  )) %>%
  filter(VARIABLE %in% VAR_list) %>%
  mutate(VARIABLE = factor(VARIABLE, levels = c("Crop Consumption [million t-DM]", "Crop Production [million t-DM]", "Crop Land [million ha]", "Livestock Consumption [million t-DM]", "Livestock Production [million t-DM]", "Pasture [million ha]", "Forest [million ha]", "CH4 emission [Mt CH4/yr]", "N2O emission [kt N2O/yr]"))) %>%
  mutate(region = dplyr::recode(region,
    "R5ASIA" = "Asia",
    "R5LAM" = "Latin America and the Caribbean",
    "R5MAF" = "Middle East and Africa",
    "R5OECD90+EU" = "OECD+EU",
    "R5REF" = "Eastern Europe and the Former Soviet Union"
  ))
levels(data4_7_Historical$VARIABLE) <- c("Crop \nConsumption \n[million t-DM]", "Crop \nProduction \n[million t-DM]", "Crop Land \n[million ha]", "Livestock \nConsumption \n[million t-DM]", "Livestock \nProduction \n[million t-DM]", "Pasture \n[million ha]", "Forest \n[million ha]", "CH4 emission \n[Mt CH4/yr]", "N2O emission \n[kt N2O/yr]")
SSP_plot <- function(i) {
  r <- region_list[i]
  data4_7_AgLU_r <- data4_7_AgLU %>% filter(region == r)
  data4_7_IAM_r <- data4_7_IAM %>% filter(region == r)
  data4_7_Historical_r <- data4_7_Historical %>% filter(region == r)
  g4_7_1 <- data4_7_AgLU_r %>%
    ggplot() +
    geom_line(aes(x = Y, y = AgLU, color = SCENARIO), linewidth = 1) +
    facet_wrap(vars(VARIABLE), scales = "free_y", nrow = 9) +
    scale_color_manual(values = Scenario_color) +
    scale_x_continuous(breaks = seq(2020, 2100, 20)) +
    labs(x = "年", y = "") +
    theme_3 +
    theme(legend.position = "none", axis.title.y = element_blank(), strip.text = element_blank())
  g4_7_2 <- ggplot() +
    geom_line(data = data4_7_AgLU_r, aes(x = Y, y = AgLU, color = SCENARIO), linewidth = 1.2) +
    geom_ribbon(data = data4_7_IAM_r, aes(x = Y, ymax = IAM_max, ymin = IAM_min, fill = SCENARIO), alpha = 0.2) +
    geom_point(data = data4_7_Historical_r, aes(x = Y, y = Historical, shape = Model2), color = "#777777", size = 3) +
    facet_grid(VARIABLE ~ SCENARIO, scales = "free") +
    scale_color_manual(values = Scenario_color) +
    scale_fill_manual(values = Scenario_color) +
    scale_shape_manual(values = model_shape) +
    scale_x_continuous(breaks = seq(2020, 2100, 20)) +
    labs(x = "年", y = "", title = paste0(r)) +
    theme_1 +
    theme(axis.title.y = element_blank())
  g4_7 <- g4_7_1 + g4_7_2 + plot_layout(widths = c(1, 4))
  j <- i + 6
  ggsave(paste0("../output/figure4_", j, ".png"), g4_7, width = 22, height = 15, create.dir = T)
}
future_map(seq(1, 6, 1), SSP_plot)
# 4-13 ------------------------------------------------------------
data4_13_1 <- full_join(QH, Yld)
data4_13_1 <- full_join(data4_13_1, QL)
data4_13_1 <- full_join(data4_13_1, QSUP)
data4_13_1 <- full_join(data4_13_1, PP)
data4_13_1 <- full_join(data4_13_1, QM) %>%
  filter(SCENARIO == "SSP2_BaU_NoCC") %>%
  dplyr::select(!SCENARIO) %>%
  mutate(var = "est")
colnames(data4_13_1) <- c("c", "cty", "yr", "QH", "Yld", "QL", "QSUP", "PP", "QM", "var")
data4_13_2 <- full_join(QH_hist, Yld_hist)
data4_13_2 <- full_join(data4_13_2, QL_hist)
data4_13_2 <- full_join(data4_13_2, QSUP_hist)
data4_13_2 <- full_join(data4_13_2, PP_hist)
data4_13_2 <- full_join(data4_13_2, QM_hist) %>%
  mutate(var = "hist")
data4_13 <- full_join(data4_13_1, data4_13_2) %>%
  filter(yr %in% c("2015", "2020"))
data4_13 <- data4_13 %>%
  pivot_longer(cols = c("QH", "Yld", "QL", "QSUP", "PP", "QM"), names_to = "variable", values_to = "value") %>%
  pivot_wider(names_from = "var", values_from = "value") %>%
  filter(c != "total") %>%
  na.omit() %>%
  mutate(hist = log10(hist), est = log10(est)) %>%
  mutate(upper = hist + 0.3, lower = hist - 0.3) %>%
  mutate(upper2 = hist + 1, lower2 = hist - 1) %>%
  mutate(variable = dplyr::recode(variable,
    "QH" = "Household Consumption Volume",
    "QSUP" = "Supply Volume",
    "QL" = "Harvested Area",
    "Yld" = "Yield",
    "PP" = "Producer Price",
    "QM" = "Import Volume"
  ))
g4_13 <- data4_13 %>%
  ggplot() +
  geom_ribbon(aes(x = hist, ymin = lower2, ymax = upper2), fill = "lightgreen", alpha = 0.3) +
  geom_ribbon(aes(x = hist, ymin = lower, ymax = upper), fill = "lightblue", alpha = 0.9) +
  geom_abline(intercept = 0, slope = 1) +
  geom_abline(intercept = -0.3, slope = 1, linetype = "dotdash") +
  geom_abline(intercept = 0.3, slope = 1, linetype = "dotdash") +
  geom_abline(intercept = -1, slope = 1, linetype = "dotted") +
  geom_abline(intercept = 1, slope = 1, linetype = "dotted") +
  geom_point(aes(x = hist, y = est), size = 1, color = "#C3327A") +
  facet_wrap(vars(yr, variable), scales = "free", nrow = 2) +
  labs(x = "log(Historical)", y = "log(Estimated)") +
  theme_1 +
  theme(axis.title.x = element_text(size = 20))
ggsave("../output/figure4_13.png", plot = g4_13, width = 20, height = 12)

data4_13 <- data4_13 %>%
  filter(variable %in% c("Household Consumption Volume", "Supply Volume", "Harvested Area", "Yield")) %>%
  mutate(variable = dplyr::recode(variable,
    "Household Consumption Volume" = "家計消費量",
    "Supply Volume" = "供給量",
    "Harvested Area" = "収穫面積",
    "Yield" = "収量"
  )) %>%
  mutate("scope" = "0.5~2倍", "scope2" = "0.1~10倍")
g4_13 <- data4_13 %>%
  ggplot() +
  geom_ribbon(aes(x = hist, ymin = lower2, ymax = upper2, fill = scope2), alpha = 0.3) +
  geom_ribbon(aes(x = hist, ymin = lower, ymax = upper, fill = scope), alpha = 0.9) +
  geom_abline(intercept = 0, slope = 1) +
  geom_abline(intercept = -0.3, slope = 1, linetype = "dotdash") +
  geom_abline(intercept = 0.3, slope = 1, linetype = "dotdash") +
  geom_abline(intercept = -1, slope = 1, linetype = "dotted") +
  geom_abline(intercept = 1, slope = 1, linetype = "dotted") +
  geom_point(aes(x = hist, y = est), size = 1, color = "#C3327A") +
  facet_wrap(vars(yr, variable), scales = "free", nrow = 2) +
  labs(x = "log(実績値)", y = "log(推計値)") +
  scale_fill_manual(values = c("0.1~10倍" = "lightgreen", "0.5~2倍" = "lightblue")) +
  theme_1 +
  theme(axis.title.x = element_text(size = 20))
ggsave("../output/presentation6.png", plot = g4_13, width = 15, height = 8)
# 4-14 --------------------------------------------------------------------
data4_14_1 <- full_join(QH_agg, Yld_agg)
data4_14_1 <- full_join(data4_14_1, QL_agg)
data4_14_1 <- full_join(data4_14_1, QSUP_agg)
data4_14_1 <- full_join(data4_14_1, PP_agg)
data4_14_1 <- full_join(data4_14_1, QM_agg) %>%
  filter(SCENARIO == "SSP2_BaU_NoCC") %>%
  dplyr::select(!SCENARIO) %>%
  mutate(var = "est")
colnames(data4_14_1) <- c("c", "region", "yr", "QH_agg", "Yld_agg", "QL_agg", "QSUP_agg", "PP_agg", "QM_agg", "var")
data4_14_2 <- full_join(QH_agg_hist, Yld_agg_hist)
data4_14_2 <- full_join(data4_14_2, QL_agg_hist)
data4_14_2 <- full_join(data4_14_2, QSUP_agg_hist)
data4_14_2 <- full_join(data4_14_2, PP_agg_hist)
data4_14_2 <- full_join(data4_14_2, QM_agg_hist) %>%
  mutate(var = "hist")
data4_14 <- full_join(data4_14_1, data4_14_2) %>%
  filter(yr %in% c("2015", "2020"))
data4_14 <- data4_14 %>%
  pivot_longer(cols = c("QH_agg", "Yld_agg", "QL_agg", "QSUP_agg", "PP_agg", "QM_agg"), names_to = "variable", values_to = "value") %>%
  pivot_wider(names_from = "var", values_from = "value") %>%
  filter(c != "total") %>%
  filter(region %in% region_AIM) %>%
  filter(region != "World") %>%
  na.omit() %>%
  mutate(hist = log10(hist), est = log10(est)) %>%
  mutate(upper = hist + 0.3, lower = hist - 0.3) %>%
  mutate(upper2 = hist + 1, lower2 = hist - 1) %>%
  mutate(variable = dplyr::recode(variable,
    "QH_agg" = "Household Consumption Volume",
    "QSUP_agg" = "Supply Volume",
    "QL_agg" = "Harvested Area",
    "Yld_agg" = "Yield",
    "PP_agg" = "Producer Price",
    "QM_agg" = "Import Volume"
  ))
g4_14 <- data4_14 %>%
  ggplot() +
  geom_ribbon(aes(x = hist, ymin = lower2, ymax = upper2), fill = "lightgreen", alpha = 0.3) +
  geom_ribbon(aes(x = hist, ymin = lower, ymax = upper), fill = "lightblue", alpha = 0.9) +
  geom_abline(intercept = 0, slope = 1) +
  geom_abline(intercept = -0.3, slope = 1, linetype = "dotdash") +
  geom_abline(intercept = 0.3, slope = 1, linetype = "dotdash") +
  geom_abline(intercept = -1, slope = 1, linetype = "dotted") +
  geom_abline(intercept = 1, slope = 1, linetype = "dotted") +
  geom_point(aes(x = hist, y = est), size = 1, color = "#C3327A") +
  facet_wrap(vars(yr, variable), scales = "free", nrow = 2) +
  labs(x = "log(Historical)", y = "log(Estimated)") +
  theme_1 +
  theme(axis.title.x = element_text(size = 20))
ggsave("../output/figure4_14.png", plot = g4_14, width = 20, height = 12)

data4_14 <- data4_14 %>%
  filter(variable %in% c("Household Consumption Volume", "Supply Volume", "Harvested Area", "Yield")) %>%
  mutate(variable = dplyr::recode(variable,
    "Household Consumption Volume" = "家計消費量",
    "Supply Volume" = "供給量",
    "Harvested Area" = "収穫面積",
    "Yield" = "収量"
  )) %>%
  mutate("scope" = "0.5~2倍", "scope2" = "0.1~10倍")
g4_14 <- data4_14 %>%
  ggplot() +
  geom_ribbon(aes(x = hist, ymin = lower2, ymax = upper2, fill = scope2), alpha = 0.3) +
  geom_ribbon(aes(x = hist, ymin = lower, ymax = upper, fill = scope), alpha = 0.9) +
  geom_abline(intercept = 0, slope = 1) +
  geom_abline(intercept = -0.3, slope = 1, linetype = "dotdash") +
  geom_abline(intercept = 0.3, slope = 1, linetype = "dotdash") +
  geom_abline(intercept = -1, slope = 1, linetype = "dotted") +
  geom_abline(intercept = 1, slope = 1, linetype = "dotted") +
  geom_point(aes(x = hist, y = est), size = 1, color = "#C3327A") +
  facet_wrap(vars(yr, variable), scales = "free", nrow = 2) +
  labs(x = "log10(実績値)", y = "log10(推計値)") +
  scale_fill_manual(values = c("0.1~10倍" = "lightgreen", "0.5~2倍" = "lightblue")) +
  theme_1 +
  theme(axis.title.x = element_text(size = 20))
ggsave("../output/presentation7.png", plot = g4_14, width = 15, height = 8)
# 4-15 --------------------------------------------------------------------
data4_15_AIM_Hub <- AIM_Hub %>%
  mutate(SCENARIO = dplyr::recode(SCENARIO,
    "SSP2_500C_CACN_DAC_NoCC_No" = "Mitigation",
    "SSP2_BaU_NoCC_No" = "BaU"
  ))
data4_15_AIM_Hub <- left_join(data4_15_AIM_Hub, UNIT) %>%
  mutate(UEMF = dplyr::recode(UEMF,
    "1000 t" = "1000 t DM",
    "t/ha" = "dm t/ha"
  ))
data4_15_AIM_Hub <- data4_15_AIM_Hub %>%
  rename("region" = "REMF", "variable" = "VEMF", "Y" = "YEMF", "AIM-Hub" = "IAMC_Template", "UNIT" = "UEMF") %>%
  mutate(variable = toupper(variable)) %>%
  mutate(Y = as.character(Y)) %>%
  mutate(Y = as.numeric(Y))

data4_15_IAMC <- AgLU_IAMC %>%
  filter(SCENARIO %in% c("SSP2_BaU_NoCC", "SSP2_500C_NoCC")) %>%
  mutate(Y = as.character(Y)) %>%
  mutate(Y = as.numeric(Y)) %>%
  mutate(SCENARIO = dplyr::recode(SCENARIO,
    "SSP2_500C_NoCC" = "Mitigation",
    "SSP2_BaU_NoCC" = "BaU"
  )) %>%
  rename("AgLU" = "IAMC_Template", "variable" = "IAMC_VAR", "region" = "IAMC_REGION") %>%
  mutate(variable = toupper(variable))
list <- inner_join(data4_15_IAMC, data4_15_AIM_Hub)
var_list <- unique(list$variable)
region_list <- unique(list$region)
data4_15_FAOSTAT <- FAOSTAT %>%
  mutate(
    variable = as.character(IAMC_VAR),
    region = as.character(IAMC_REGION),
    Y = as.numeric(as.character(yr)),
    Model2 = "FAOSTAT",
    Historical = IAMC_Template
  ) %>%
  mutate(variable = toupper(variable)) %>%
  dplyr::select(!c(IAMC_VAR, IAMC_REGION, yr, IAMC_Template))
data4_15_LUH2 <- LUH2 %>%
  mutate(
    variable = as.character(IAMC_VAR),
    region = as.character(IAMC_REGION),
    Y = as.numeric(as.character(yr)),
    Model2 = "LUH2",
    Historical = IAMC_Template_LUH2
  ) %>%
  mutate(variable = toupper(variable)) %>%
  dplyr::select(!c(IAMC_VAR, IAMC_REGION, yr, IAMC_Template_LUH2))
data4_15_EDGAR <- EDGAR %>%
  mutate(
    variable = as.character(IAMC_VAR),
    region = as.character(IAMC_REGION),
    Y = as.numeric(as.character(yr)),
    Model2 = "EDGAR",
    Historical = IAMC_Template_EDGAR
  ) %>%
  mutate(variable = toupper(variable)) %>%
  dplyr::select(!c(IAMC_VAR, IAMC_REGION, yr, IAMC_Template_EDGAR))
data4_15_Historical <- full_join(data4_15_FAOSTAT, data4_15_LUH2)
data4_15_Historical <- full_join(data4_15_Historical, data4_15_EDGAR) %>%
  filter(Y > 2000, Y < 2021, region %in% region_AIM)
data4_15_IAMC <- full_join(data4_15_IAMC, data4_15_AIM_Hub) %>%
  filter(variable %in% var_list, region %in% region_AIM) %>%
  pivot_longer(cols = -c(SCENARIO, region, variable, Y, UNIT), names_to = "Model", values_to = "Value") %>%
  mutate(UNIT = as.character(UNIT)) %>%
  na.omit() %>%
  filter(Y %in% seq(2000, 2100, 5))
data4_15_IAMC <- full_join(data4_15_IAMC, data4_15_Historical) %>%
  mutate(region = dplyr::recode(region,
    "BRA" = "Brazil",
    "CAN" = "Canada",
    "CHN" = "China",
    "IND" = "India",
    "JPN" = "Japan",
    "TUR" = "Turkey",
    "USA" = "United States",
    "XE25" = "EU25",
    "XER" = "Rest of Europe",
    "XOC" = "Oceania",
    "XSE" = "Southeast Asia",
    "XSA" = "Rest of Asia",
    "XLM" = "Rest of South America",
    "CIS" = "Former Soviet Union",
    "XME" = "Middle East",
    "XNF" = "North Africa",
    "XAF" = "Rest of Africa",
    "World" = "World"
  ))
data4_15 <- data4_15_IAMC %>%
  filter(region == "World") %>%
  filter(variable %in% c("AGR_DEM_CRO", "AGR_DEM_LIV", "AGR_PRO_CRO", "AGR_PRO_LIV", "LAN_COV_CRO", "LAN_COV_PST", "LAN_COV_FRS", "EMI_CH4_AFO", "EMI_N2O_AFO")) %>%
  mutate(variable = dplyr::recode(variable,
    "AGR_DEM_CRO" = "Crop Consumption [million t-DM]",
    "AGR_DEM_LIV" = "Livestock Consumption [million t-DM]",
    "AGR_PRO_CRO" = "Crop Production [million t-DM]",
    "AGR_PRO_LIV" = "Livestock Production [million t-DM]",
    "LAN_COV_CRO" = "Crop Land [million ha]",
    "LAN_COV_FRS" = "Forest [million ha]",
    "LAN_COV_PST" = "Pasture [million ha]",
    "EMI_CH4_AFO" = "CH4 emission [Mt CH4/yr]",
    "EMI_N2O_AFO" = "N2O emission [kt N2O/yr]"
  )) %>%
  mutate(variable = factor(variable, levels = c("Crop Consumption [million t-DM]", "Crop Production [million t-DM]", "Crop Land [million ha]", "Livestock Consumption [million t-DM]", "Livestock Production [million t-DM]", "Pasture [million ha]", "Forest [million ha]", "CH4 emission [Mt CH4/yr]", "N2O emission [kt N2O/yr]")))
data4_15_sim <- data4_15 %>%
  dplyr::select(SCENARIO, variable, region, Y, Model, Value) %>%
  na.omit()
data4_15_hist <- data4_15 %>%
  dplyr::select(variable, region, Y, Model2, Historical) %>%
  na.omit()
g4_15 <- ggplot() +
  geom_point(data = data4_15_hist, aes(x = Y, y = Historical, shape = Model2), color = "#01810b", size = 2.5) +
  geom_line(data = data4_15_sim, aes(x = Y, y = Value, linetype = Model, color = SCENARIO), linewidth = 1.3) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 2015, linetype = "dotted") +
  facet_wrap(vars(variable), scales = "free_y") +
  scale_x_continuous(breaks = seq(2000, 2100, 20)) +
  scale_color_manual(values = scenario_color) +
  scale_shape_manual(values = model_shape) +
  labs(x = "年", y = "") +
  theme_1 +
  theme(axis.title.y = element_blank())
ggsave("../output/figure4_15.png", g4_15, width = 15, height = 10, create.dir = T)
data4_15 <- data4_15_IAMC %>%
  filter(region == "World") %>%
  filter(variable %in% c("AGR_DEM_CRO", "AGR_DEM_LIV", "AGR_PRO_CRO", "AGR_PRO_LIV", "LAN_COV_CRO", "LAN_COV_PST", "LAN_COV_FRS", "EMI_CH4_AFO", "EMI_N2O_AFO")) %>%
  mutate(variable = dplyr::recode(variable,
    "AGR_DEM_CRO" = "一次作物消費量 [million t-DM]",
    "AGR_DEM_LIV" = "畜産物消費量 [million t-DM]",
    "AGR_PRO_CRO" = "一次作物生産量 [million t-DM]",
    "AGR_PRO_LIV" = "畜産物生産量 [million t-DM]",
    "LAN_COV_CRO" = "耕作地 [million ha]",
    "LAN_COV_FRS" = "森林 [million ha]",
    "LAN_COV_PST" = "牧草地 [million ha]",
    "EMI_CH4_AFO" = "CH4排出量 [Mt CH4/yr]",
    "EMI_N2O_AFO" = "N2O排出量 [kt N2O/yr]"
  )) %>%
  mutate(variable = factor(variable, levels = c("一次作物消費量 [million t-DM]", "一次作物生産量 [million t-DM]", "耕作地 [million ha]", "畜産物消費量 [million t-DM]", "畜産物生産量 [million t-DM]", "牧草地 [million ha]", "森林 [million ha]", "CH4排出量 [Mt CH4/yr]", "N2O排出量 [kt N2O/yr]")))
data4_15_sim <- data4_15 %>%
  dplyr::select(SCENARIO, variable, region, Y, Model, Value) %>%
  na.omit()
data4_15_hist <- data4_15 %>%
  dplyr::select(variable, region, Y, Model2, Historical) %>%
  na.omit()
g4_15 <- ggplot() +
  geom_point(data = data4_15_hist, aes(x = Y, y = Historical, shape = Model2), color = "#01810b", size = 2.5) +
  geom_line(data = data4_15_sim, aes(x = Y, y = Value, linetype = Model, color = SCENARIO, linewidth = Model), linewidth = 1.3) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 2015, linetype = "dotted") +
  facet_wrap(vars(variable), scales = "free_y") +
  scale_x_continuous(breaks = seq(2000, 2100, 20)) +
  scale_color_manual(values = scenario_color, name = "シナリオ") +
  scale_shape_manual(values = model_shape, name = "実績データ") +
  scale_linetype_manual(values = c(1, 3), name = "モデル") +
  labs(x = "年", y = "") +
  theme_1 +
  theme(axis.title.y = element_blank(), legend.position = "right", legend.title = element_text(size = 15))
ggsave("../output/presentation1.png", g4_15, width = 14, height = 8, create.dir = T)
# 4-16 --------------------------------------------------------------------
data4_16_1 <- data4_15_IAMC %>%
  filter(variable %in% c(
    "EMI_CH4_AFO_AGR_LIV_ENT_FER",
    "EMI_CH4_AFO_AGR_LIV_MAN_MAN",
    "EMI_CH4_AFO_AGR_RIC",
    "EMI_CH4_AFO_BIO_BUR",
    "EMI_CH4_AFO_LAN_GRA_BUR",
    "EMI_N2O_AFO_AGR_LIV_MAN_MAN",
    "EMI_N2O_AFO_AGR_MAN_SOI",
    "EMI_N2O_AFO_BIO_BUR",
    "EMI_N2O_AFO_LAN_GRA_BUR",
    "EMI_N2O_AFO_LAN_GRA_PST"
  )) %>%
  filter(region == "World") %>%
  filter(Y %in% c("2015", "2050", "2100")) %>%
  mutate(gas = dplyr::recode(
    variable,
    "EMI_CH4_AFO_AGR_LIV_ENT_FER" = "CH4",
    "EMI_CH4_AFO_AGR_LIV_MAN_MAN" = "CH4",
    "EMI_CH4_AFO_AGR_RIC" = "CH4",
    "EMI_CH4_AFO_BIO_BUR" = "CH4",
    "EMI_CH4_AFO_LAN_GRA_BUR" = "CH4",
    "EMI_N2O_AFO_AGR_LIV_MAN_MAN" = "N2O",
    "EMI_N2O_AFO_AGR_MAN_SOI" = "N2O",
    "EMI_N2O_AFO_BIO_BUR" = "N2O",
    "EMI_N2O_AFO_LAN_GRA_BUR" = "N2O",
    "EMI_N2O_AFO_LAN_GRA_PST" = "N2O"
  )) %>%
  filter(Model == "AgLU") %>%
  pivot_wider(names_from = "Model2", values_from = "Historical") %>%
  dplyr::select(!c(FAOSTAT, EDGAR, `NA`)) %>%
  na.omit()
data4_16_2 <- data4_15_AIM_Hub %>%
  filter(variable %in% c(
    "EMI_CH4_AFO_AGR_LIV_ENT_FER",
    "EMI_CH4_AFO_AGR_LIV_MAN_MAN",
    "EMI_CH4_AFO_AGR_RIC",
    "EMI_CH4_AFO_BIO_BUR",
    "EMI_CH4_AFO_LAN_GRA_BUR",
    "EMI_N2O_AFO_AGR_LIV_MAN_MAN",
    "EMI_N2O_AFO_AGR_MAN_SOI",
    "EMI_N2O_AFO_BIO_BUR",
    "EMI_N2O_AFO_LAN_GRA_BUR",
    "EMI_N2O_AFO_LAN_GRA_PST"
  )) %>%
  filter(region == "World") %>%
  filter(Y %in% c("2015", "2050", "2100")) %>%
  mutate(gas = dplyr::recode(
    variable,
    "EMI_CH4_AFO_AGR_LIV_ENT_FER" = "CH4",
    "EMI_CH4_AFO_AGR_LIV_MAN_MAN" = "CH4",
    "EMI_CH4_AFO_AGR_RIC" = "CH4",
    "EMI_CH4_AFO_BIO_BUR" = "CH4",
    "EMI_CH4_AFO_LAN_GRA_BUR" = "CH4",
    "EMI_N2O_AFO_AGR_LIV_MAN_MAN" = "N2O",
    "EMI_N2O_AFO_AGR_MAN_SOI" = "N2O",
    "EMI_N2O_AFO_BIO_BUR" = "N2O",
    "EMI_N2O_AFO_LAN_GRA_BUR" = "N2O",
    "EMI_N2O_AFO_LAN_GRA_PST" = "N2O"
  )) %>%
  rename("Value" = "AIM-Hub") %>%
  mutate(Model = "AIM-Hub")
data4_16 <- full_join(data4_16_1, data4_16_2) %>%
  mutate(source = dplyr::recode(
    variable,
    "EMI_CH4_AFO_AGR_LIV_ENT_FER" = "Enteric fermentation",
    "EMI_CH4_AFO_AGR_LIV_MAN_MAN" = "Manure Management",
    "EMI_CH4_AFO_AGR_RIC" = "Rice Cultivation",
    "EMI_CH4_AFO_BIO_BUR" = "Biomass Burning",
    "EMI_CH4_AFO_LAN_GRA_BUR" = "Biomass Burning",
    "EMI_N2O_AFO_AGR_LIV_MAN_MAN" = "Manure Management",
    "EMI_N2O_AFO_AGR_MAN_SOI" = "Managed soils",
    "EMI_N2O_AFO_BIO_BUR" = "Biomass Burning",
    "EMI_N2O_AFO_LAN_GRA_BUR" = "Managed soils",
    "EMI_N2O_AFO_LAN_GRA_PST" = "Managed soils"
  )) %>%
  dplyr::select(!c(region, variable, UNIT)) %>%
  mutate(SCENARIO = factor(SCENARIO, levels = c("BaU", "Mitigation"))) %>%
  mutate(source = factor(source, levels = c(
    "Enteric fermentation",
    "Manure Management",
    "Biomass Burning",
    "Rice Cultivation",
    "Managed soils"
  )))
g4_16_1 <- data4_16 %>%
  filter(gas == "CH4") %>%
  ggplot() +
  geom_bar(aes(x = Model, y = Value, fill = source), stat = "identity") +
  facet_grid(vars(SCENARIO), vars(Y)) +
  scale_fill_manual(values = source_color, guide = guide_legend(ncol = 2)) +
  labs(y = expression(paste(
    {
      CH[4]
    },
    " Emission [Mt ",
    {
      CH[4]
    },
    "/yr]"
  ))) +
  theme_1
g4_16_2 <- data4_16 %>%
  filter(gas == "N2O") %>%
  ggplot() +
  geom_bar(aes(x = Model, y = Value, fill = source), stat = "identity") +
  facet_grid(vars(SCENARIO), vars(Y)) +
  scale_fill_manual(values = source_color) +
  labs(y = expression(paste(
    {
      N[2]
    },
    "O Emission [kt ",
    {
      N[2]
    },
    "O/yr]"
  ))) +
  theme_1
g4_16 <- g4_16_1 + g4_16_2
ggsave("../output/figure4_16.png", g4_16, width = 15, height = 10, create.dir = T)
# 4-17~4-22 --------------------------------------------------------------------
data4_17 <- data4_15_IAMC %>%
  filter(variable == "AGR_PRO_CRO")
data4_17_sim <- data4_17 %>%
  dplyr::select(SCENARIO, variable, region, Y, Model, Value, UNIT) %>%
  na.omit()
data4_17_hist <- data4_17 %>%
  dplyr::select(variable, region, Y, Model2, Historical) %>%
  na.omit()
g4_17 <- ggplot() +
  geom_point(data = data4_17_hist, aes(x = Y, y = Historical, shape = Model2), color = "#01810b", size = 1.5) +
  geom_line(data = data4_17_sim, aes(x = Y, y = Value, color = SCENARIO, linetype = Model), linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 2015, linetype = "dotted") +
  facet_wrap(vars(region), scales = "free_y", ncol = 6) +
  scale_x_continuous(breaks = c(2020, 2050, 2100)) +
  scale_y_continuous(limits = c(0, NA)) +
  scale_color_manual(values = scenario_color) +
  scale_shape_manual(values = model_shape) +
  labs(y = "Crop Production [million t DM]") +
  theme_1
ggsave("../output/figure4_17.png", g4_17, width = 15, height = 7.5, create.dir = T)
data4_18 <- data4_15_IAMC %>%
  filter(variable == "AGR_PRO_LIV")
data4_18_sim <- data4_18 %>%
  dplyr::select(SCENARIO, variable, region, Y, Model, Value, UNIT) %>%
  na.omit()
data4_18_hist <- data4_18 %>%
  dplyr::select(variable, region, Y, Model2, Historical) %>%
  na.omit()
g4_18 <- ggplot() +
  geom_point(data = data4_18_hist, aes(x = Y, y = Historical, shape = Model2), color = "#01810b", size = 1.5) +
  geom_line(data = data4_18_sim, aes(x = Y, y = Value, color = SCENARIO, linetype = Model), linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 2015, linetype = "dotted") +
  facet_wrap(vars(region), scales = "free_y", ncol = 6) +
  scale_x_continuous(breaks = c(2020, 2050, 2100)) +
  scale_y_continuous(limits = c(0, NA)) +
  scale_color_manual(values = scenario_color) +
  scale_shape_manual(values = model_shape) +
  labs(y = "Livestock Production [million t DM]") +
  theme_1
ggsave("../output/figure4_18.png", g4_18, width = 15, height = 7.5, create.dir = T)
data4_19 <- data4_15_IAMC %>%
  filter(variable == "PRC_AGR_FOO_PRO_CRO_IND")
data4_19_sim <- data4_19 %>%
  dplyr::select(SCENARIO, variable, region, Y, Model, Value, UNIT) %>%
  na.omit()
data4_19_hist <- data4_19 %>%
  dplyr::select(variable, region, Y, Model2, Historical) %>%
  na.omit()
g4_19 <- ggplot() +
  geom_point(data = data4_19_hist, aes(x = Y, y = Historical, shape = Model2), color = "#01810b", size = 1.5) +
  geom_line(data = data4_19_sim, aes(x = Y, y = Value, color = SCENARIO, linetype = Model), linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 2015, linetype = "dotted") +
  facet_wrap(vars(region), scales = "free_y", ncol = 6) +
  scale_x_continuous(breaks = c(2020, 2050, 2100)) +
  scale_y_continuous(limits = c(0, NA)) +
  scale_color_manual(values = scenario_color) +
  scale_shape_manual(values = model_shape) +
  labs(y = "Crop Production Price Index [Y2020=1]") +
  theme_1
ggsave("../output/figure4_19.png", g4_19, width = 15, height = 7.5, create.dir = T)
data4_20 <- data4_15_IAMC %>%
  filter(variable == "PRC_AGR_FOO_PRO_LIV_IND")
data4_20_sim <- data4_20 %>%
  dplyr::select(SCENARIO, variable, region, Y, Model, Value, UNIT) %>%
  na.omit()
data4_20_hist <- data4_20 %>%
  dplyr::select(variable, region, Y, Model2, Historical) %>%
  na.omit()
g4_20 <- ggplot() +
  geom_point(data = data4_20_hist, aes(x = Y, y = Historical, shape = Model2), color = "#01810b", size = 1.5) +
  geom_line(data = data4_20_sim, aes(x = Y, y = Value, color = SCENARIO, linetype = Model), linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 2015, linetype = "dotted") +
  facet_wrap(vars(region), scales = "free_y", ncol = 6) +
  scale_x_continuous(breaks = c(2020, 2050, 2100)) +
  scale_y_continuous(limits = c(0, NA)) +
  scale_color_manual(values = scenario_color) +
  scale_shape_manual(values = model_shape) +
  labs(y = "Livestock Production Price Index [Y2020=1]") +
  theme_1
ggsave("../output/figure4_20.png", g4_20, width = 15, height = 7.5, create.dir = T)
data4_21 <- data4_15_IAMC %>%
  filter(variable == "EMI_CH4_AFO")
data4_21_sim <- data4_21 %>%
  dplyr::select(SCENARIO, variable, region, Y, Model, Value, UNIT) %>%
  na.omit()
data4_21_hist <- data4_21 %>%
  dplyr::select(variable, region, Y, Model2, Historical) %>%
  na.omit()
g4_21 <- ggplot() +
  geom_point(data = data4_21_hist, aes(x = Y, y = Historical, shape = Model2), color = "#01810b", size = 1.5) +
  geom_line(data = data4_21_sim, aes(x = Y, y = Value, color = SCENARIO, linetype = Model), linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 2015, linetype = "dotted") +
  facet_wrap(vars(region), scales = "free_y", ncol = 6) +
  scale_x_continuous(breaks = c(2020, 2050, 2100)) +
  scale_y_continuous(limits = c(0, NA)) +
  scale_color_manual(values = scenario_color) +
  scale_shape_manual(values = model_shape) +
  labs(y = expression(paste(
    {
      CH[4]
    },
    " Emission [Mt ",
    {
      CH[4]
    },
    "/yr]"
  ))) +
  theme_1
ggsave("../output/figure4_21.png", g4_21, width = 15, height = 7.5, create.dir = T)
data4_22 <- data4_15_IAMC %>%
  filter(variable == "EMI_N2O_AFO")
data4_22_sim <- data4_22 %>%
  dplyr::select(SCENARIO, variable, region, Y, Model, Value, UNIT) %>%
  na.omit()
data4_22_hist <- data4_22 %>%
  dplyr::select(variable, region, Y, Model2, Historical) %>%
  na.omit()
g4_22 <- ggplot() +
  geom_point(data = data4_22_hist, aes(x = Y, y = Historical, shape = Model2), color = "#01810b", size = 1.5) +
  geom_line(data = data4_22_sim, aes(x = Y, y = Value, color = SCENARIO, linetype = Model), linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 2015, linetype = "dotted") +
  facet_wrap(vars(region), scales = "free_y", ncol = 6) +
  scale_x_continuous(breaks = c(2020, 2050, 2100)) +
  scale_y_continuous(limits = c(0, NA)) +
  scale_color_manual(values = scenario_color) +
  scale_shape_manual(values = model_shape) +
  labs(y = expression(paste(
    {
      N[2]
    },
    "O Emission [kt ",
    {
      N[2]
    },
    "O/yr]"
  ))) +
  theme_1
ggsave("../output/figure4_22.png", g4_22, width = 15, height = 7.5, create.dir = T)
# 4-23 --------------------------------------------------------------------
data4_23_QSUP <- QSUP %>%
  filter(c != "ecr") %>%
  filter(SCENARIO %in% c("SSP2_BaU_NoCC", "SSP2_500C_NoCC")) %>%
  mutate(SCENARIO = dplyr::recode(SCENARIO,
    "SSP2_500C_NoCC" = "Mitigation",
    "SSP2_BaU_NoCC" = "BaU"
  ))
data4_23_EF <- EF %>% rename("c" = "a")
data4_23_RR <- RR %>%
  filter(SCENARIO %in% c("SSP2_BaU_NoCC", "SSP2_500C_NoCC")) %>%
  mutate(SCENARIO = dplyr::recode(SCENARIO,
    "SSP2_500C_NoCC" = "Mitigation",
    "SSP2_BaU_NoCC" = "BaU"
  ))
data4_23_GHG <- left_join(data4_23_QSUP, data4_23_EF, relationship = "many-to-many")
data4_23_GHG <- full_join(data4_23_GHG, data4_23_RR)
data4_23_GHG <- data4_23_GHG %>%
  mutate(Y = as.character(Y)) %>%
  mutate(Y = as.numeric(Y)) %>%
  filter(Y > 2020)
data4_23_GHG$RR_load[is.na(data4_23_GHG$RR_load)] <- 0
data4_23_EF_delta <- data4_23_GHG %>%
  mutate(EF = EFbase * (1 - RR_load)) %>%
  dplyr::select(SCENARIO, c, cty, Y, src, gas, EF) %>%
  pivot_wider(names_from = "SCENARIO", values_from = "EF") %>%
  mutate("EF_ave" = (BaU + Mitigation) / 2, "EF_delta" = (Mitigation - BaU)) %>%
  dplyr::select(!c(BaU, Mitigation))
data4_23_QSUP_delta <- data4_23_GHG %>%
  dplyr::select(SCENARIO, c, cty, Y, src, gas, QSUP_load) %>%
  pivot_wider(names_from = "SCENARIO", values_from = "QSUP_load") %>%
  mutate("QSUP_ave" = (BaU + Mitigation) / 2, "QSUP_delta" = (Mitigation - BaU)) %>%
  dplyr::select(!c(BaU, Mitigation))
data4_23_GHG_delta <- full_join(data4_23_QSUP_delta, data4_23_EF_delta) %>%
  mutate(GHG_QSUP = QSUP_delta * EF_ave, GHG_EF = QSUP_ave * EF_delta, GHG_delta = GHG_QSUP + GHG_EF) %>%
  dplyr::select(c, cty, Y, src, gas, GHG_QSUP, GHG_EF, GHG_delta)
data4_23_GHG <- data4_23_GHG %>%
  mutate(GHG = QSUP_load * EFbase * (1 - RR_load)) %>%
  dplyr::select(SCENARIO, c, cty, Y, src, gas, GHG) %>%
  pivot_wider(names_from = "SCENARIO", values_from = "GHG")
data4_23_GHG <- full_join(data4_23_GHG, data4_23_GHG_delta)
data4_23_GHG_region <- full_join(data4_23_GHG, region_world, relationship = "many-to-many") %>%
  na.omit() %>%
  mutate(c = dplyr::recode(c,
    "wht" = "Crop",
    "rce" = "Crop",
    "crl" = "Crop",
    "mze" = "Crop",
    "str" = "Crop",
    "sgr" = "Crop",
    "swt" = "Crop",
    "pls" = "Crop",
    "nut" = "Crop",
    "ocr" = "Crop",
    "vol" = "Crop",
    "vgt" = "Crop",
    "frt" = "Crop",
    "stm" = "Crop",
    "spc" = "Crop",
    "alc" = "Crop",
    "cmt" = "Livestock",
    "pmt" = "Livestock",
    "omt" = "Livestock",
    "rmt" = "Livestock",
    "mlk" = "Livestock",
    "egg" = "Livestock"
  )) %>%
  group_by(region, Y, gas) %>%
  summarise(Mitigation = sum(Mitigation), BaU = sum(BaU), GHG_QSUP = sum(GHG_QSUP), GHG_EF = sum(GHG_EF), GHG_delta = sum(GHG_delta)) %>%
  mutate(GHG_QSUP_rate = GHG_QSUP / BaU * 100, GHG_EF_rate = GHG_EF / BaU * 100) %>%
  dplyr::select(region, Y, gas, GHG_QSUP_rate, GHG_EF_rate) %>%
  pivot_longer(cols = c(GHG_QSUP_rate, GHG_EF_rate), names_to = "change", values_to = "Value") %>%
  mutate(change = dplyr::recode(change,
    "GHG_QSUP_rate" = "Production Change", "GHG_EF_rate" = "Reduction Measures"
  )) %>%
  mutate(region = dplyr::recode(region,
    "BRA" = "Brazil",
    "CAN" = "Canada",
    "CHN" = "China",
    "IND" = "India",
    "JPN" = "Japan",
    "TUR" = "Turkey",
    "USA" = "United States",
    "XE25" = "EU25",
    "XER" = "Rest of Europe",
    "XOC" = "Oceania",
    "XSE" = "Southeast Asia",
    "XSA" = "Rest of Asia",
    "XLM" = "Rest of South America",
    "CIS" = "Former Soviet Union",
    "XME" = "Middle East",
    "XNF" = "North Africa",
    "XAF" = "Rest of Africa",
    "World" = "World"
  ))
data4_23_GHG_region_total <- data4_23_GHG_region %>%
  filter(Y == "2100") %>%
  group_by(region, Y, gas) %>%
  summarise(Value = sum(Value)) %>%
  mutate(Model = "AgLU")
data4_23_AIM_Hub <- data4_15_AIM_Hub %>%
  filter(variable %in% c("EMI_CH4_AFO", "EMI_N2O_AFO"), region %in% region_AIM, Y == "2100") %>%
  pivot_wider(names_from = "SCENARIO", values_from = "AIM-Hub") %>%
  mutate(Value = (Mitigation - BaU) / BaU * 100) %>%
  mutate(gas = dplyr::recode(variable,
    "EMI_CH4_AFO" = "CH4",
    "EMI_N2O_AFO" = "N2O"
  )) %>%
  dplyr::select(region, Value, gas) %>%
  mutate(Model = "AIM-Hub") %>%
  mutate(region = dplyr::recode(region,
    "BRA" = "Brazil",
    "CAN" = "Canada",
    "CHN" = "China",
    "IND" = "India",
    "JPN" = "Japan",
    "TUR" = "Turkey",
    "USA" = "United States",
    "XE25" = "EU25",
    "XER" = "Rest of Europe",
    "XOC" = "Oceania",
    "XSE" = "Southeast Asia",
    "XSA" = "Rest of Asia",
    "XLM" = "Rest of South America",
    "CIS" = "Former Soviet Union",
    "XME" = "Middle East",
    "XNF" = "North Africa",
    "XAF" = "Rest of Africa",
    "World" = "World"
  )) %>%
  mutate(Model = "AIM-Hub")
data4_23_GHG_region_total <- full_join(data4_23_GHG_region_total, data4_23_AIM_Hub) %>%
  mutate(gas = factor(gas, levels = c("N2O", "CH4")))
data4_23_GHG_region <- data4_23_GHG_region %>%
  filter(Y == "2100") %>%
  mutate(change = factor(change, levels = c("Reduction Measures", "Production Change"))) %>%
  mutate(region = factor(region, levels = c("Brazil", "Canada", "China", "India", "Japan", "Turkey", "United States", "EU25", "Rest of Europe", "Oceania", "Southeast Asia", "Rest of Asia", "Rest of South America", "Former Soviet Union", "Middle East", "North Africa", "Rest of Africa", "World"))) %>%
  mutate(gas = factor(gas, levels = c("N2O", "CH4")))
g4_23 <- ggplot() +
  geom_bar(data = data4_23_GHG_region, aes(x = region, y = Value, fill = change), stat = "identity") +
  geom_point(data = data4_23_GHG_region_total, aes(x = region, y = Value, shape = Model), size = 3) +
  scale_fill_manual(values = c("#E3819C", "#A0EBD0")) +
  scale_shape_manual(values = c(16, 4), ) +
  scale_colour_manual(values = c("black", "black")) +
  facet_wrap(vars(gas), scales = "free_y") +
  labs(y = "Emission Change from BaU [%]") +
  theme_2
ggsave("../output/figure4_23.png", g4_23, width = 10, height = 6, create.dir = T)
data4_23_GHG_region <- data4_23_GHG_region %>%
  mutate(region = dplyr::recode(region,
    "Brazil" = "ブラジル",
    "Canada" = "カナダ",
    "China" = "中国",
    "India" = "インド",
    "Japan" = "日本",
    "Turkey" = "トルコ",
    "United States" = "アメリカ",
    "EU25" = "EU諸国",
    "Rest of Europe" = "その他ヨーロッパ",
    "Oceania" = "オセアニア",
    "Southeast Asia" = "東南アジア",
    "Rest of Asia" = "その他アジア",
    "Rest of South America" = "その他南米",
    "Former Soviet Union" = "旧ソ連諸国",
    "Middle East" = "中東",
    "North Africa" = "北アフリカ",
    "Rest of Africa" = "その他アフリカ",
    "World" = "世界"
  )) %>%
  mutate(change = dplyr::recode(change,
    "Reduction Measures" = "排出削減技術導入",
    "Production Change" = "生産量変化"
  ))
data4_23_GHG_region_total <- data4_23_GHG_region_total %>%
  mutate(region = dplyr::recode(region,
    "Brazil" = "ブラジル",
    "Canada" = "カナダ",
    "China" = "中国",
    "India" = "インド",
    "Japan" = "日本",
    "Turkey" = "トルコ",
    "United States" = "アメリカ",
    "EU25" = "EU諸国",
    "Rest of Europe" = "その他ヨーロッパ",
    "Oceania" = "オセアニア",
    "Southeast Asia" = "東南アジア",
    "Rest of Asia" = "その他アジア",
    "Rest of South America" = "その他南米",
    "Former Soviet Union" = "旧ソ連諸国",
    "Middle East" = "中東",
    "North Africa" = "北アフリカ",
    "Rest of Africa" = "その他アフリカ",
    "World" = "世界"
  ))
g4_23 <- ggplot() +
  geom_bar(data = data4_23_GHG_region, aes(x = region, y = Value, fill = change), stat = "identity") +
  geom_point(data = data4_23_GHG_region_total, aes(x = region, y = Value, shape = Model), size = 3) +
  scale_fill_manual(values = c("#E3819C", "#A0EBD0"), name = "AgLU結果の要因分解") +
  scale_shape_manual(values = c(16, 4), name = "モデル") +
  scale_colour_manual(values = c("black", "black"), name = "モデル") +
  facet_wrap(vars(gas), scales = "free_y") +
  labs(y = "緩和策によるBaUからの排出量変化 [%]") +
  theme_2 +
  theme(strip.text = element_text(size = 10), axis.text.x = element_text(size = 10), axis.title.y = element_text(size = 14), legend.text = element_text(size = 10), legend.title = element_text(size = 10), legend.title.position = "top")
ggsave("../output/presentation2.png", g4_23, width = 10, height = 6, create.dir = T)
# 4-24,4-25 ---------------------------------------------------------------------
data4_24_QSUP <- QSUP_agg %>%
  filter(c != "ecr") %>%
  filter(SCENARIO %in% c("SSP2_BaU_NoCC", "SSP2_500C_NoCC")) %>%
  mutate(SCENARIO = dplyr::recode(SCENARIO,
    "SSP2_500C_NoCC" = "Mitigation",
    "SSP2_BaU_NoCC" = "BaU"
  )) %>%
  rename("Value" = "QSUP_agg_load") %>%
  mutate("var" = "Supply")
data4_24_QDEM <- QDEM_agg %>%
  filter(c != "ecr") %>%
  filter(SCENARIO %in% c("SSP2_BaU_NoCC", "SSP2_500C_NoCC")) %>%
  mutate(SCENARIO = dplyr::recode(SCENARIO,
    "SSP2_500C_NoCC" = "Mitigation",
    "SSP2_BaU_NoCC" = "BaU"
  )) %>%
  rename("Value" = "QDEM_agg_load") %>%
  mutate("var" = "Demand") %>%
  mutate(Value = -Value)
data4_24_QM <- QM_agg %>%
  filter(c != "ecr") %>%
  filter(SCENARIO %in% c("SSP2_BaU_NoCC", "SSP2_500C_NoCC")) %>%
  mutate(SCENARIO = dplyr::recode(SCENARIO,
    "SSP2_500C_NoCC" = "Mitigation",
    "SSP2_BaU_NoCC" = "BaU"
  )) %>%
  rename("Value" = "QM_agg_load") %>%
  mutate("var" = "Import")
data4_24_QE <- QE_agg %>%
  filter(c != "ecr") %>%
  filter(SCENARIO %in% c("SSP2_BaU_NoCC", "SSP2_500C_NoCC")) %>%
  mutate(SCENARIO = dplyr::recode(SCENARIO,
    "SSP2_500C_NoCC" = "Mitigation",
    "SSP2_BaU_NoCC" = "BaU"
  )) %>%
  rename("Value" = "QE_agg_load") %>%
  mutate("var" = "Export") %>%
  mutate(Value = -Value)
data4_24 <- rbind(data4_24_QSUP, data4_24_QDEM, data4_24_QM, data4_24_QE) %>%
  mutate(c = dplyr::recode(c,
    "wht" = "Cereal",
    "rce" = "Cereal",
    "crl" = "Cereal",
    "mze" = "Cereal",
    "str" = "OtherCrop",
    "sgr" = "OtherCrop",
    "pls" = "OtherCrop",
    "nut" = "OtherCrop",
    "ocr" = "OtherCrop",
    "vgt" = "OtherCrop",
    "frt" = "OtherCrop",
    "stm" = "OtherCrop",
    "spc" = "OtherCrop",
    "cmt" = "Ruminant",
    "pmt" = "NonRuminant",
    "omt" = "NonRuminant",
    "rmt" = "Ruminant",
    "mlk" = "Ruminant",
    "egg" = "NonRuminant"
  )) %>%
  group_by(SCENARIO, c, region, Y, var) %>%
  summarise(Value = sum(Value) / 1000) %>%
  ungroup() %>%
  filter(region %in% region_AIM) %>%
  mutate(region = dplyr::recode(region,
    "BRA" = "Brazil",
    "CAN" = "Canada",
    "CHN" = "China",
    "IND" = "India",
    "JPN" = "Japan",
    "TUR" = "Turkey",
    "USA" = "United States",
    "XE25" = "EU25",
    "XER" = "Rest of Europe",
    "XOC" = "Oceania",
    "XSE" = "Southeast Asia",
    "XSA" = "Rest of Asia",
    "XLM" = "Rest of \nSouth America",
    "CIS" = "Former \nSoviet Union",
    "XME" = "Middle East",
    "XNF" = "North Africa",
    "XAF" = "Rest of Africa",
    "World" = "World"
  ))
data4_24_2015 <- data4_24 %>%
  filter(Y == "2015") %>%
  filter(SCENARIO == "BaU") %>%
  mutate(SCENARIO = dplyr::recode(SCENARIO,
    "BaU" = "2015"
  ))
data4_24_2100 <- data4_24 %>%
  filter(Y == "2100")
data4_24 <- full_join(data4_24_2015, data4_24_2100) %>%
  mutate(SCENARIO = factor(SCENARIO, levels = c("2015", "BaU", "Mitigation"))) %>%
  mutate(var = factor(var, levels = c("Export", "Import", "Demand", "Supply")))
g4_24_1 <- data4_24 %>%
  filter(c == "Cereal") %>%
  ggplot() +
  geom_bar(aes(x = SCENARIO, y = Value, fill = var), stat = "identity") +
  facet_wrap(vars(region), scales = "free_y") +
  scale_fill_manual(values = var_color) +
  labs(y = "Volume [million t]", title = "Cereal") +
  theme_2 +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 10)
  )
g4_24_2 <- data4_24 %>%
  filter(c == "OtherCrop") %>%
  ggplot() +
  geom_bar(aes(x = SCENARIO, y = Value, fill = var), stat = "identity") +
  facet_wrap(vars(region), scales = "free_y") +
  scale_fill_manual(values = var_color) +
  labs(y = "Volume [million t]", title = "Other Crop") +
  theme_2 +
  theme(
    legend.justification = c(-1.2, 0),
    plot.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    axis.title.y = element_blank()
  )
g4_24 <- g4_24_1 + g4_24_2
ggsave("../output/figure4_24.png", g4_24, width = 10, height = 6, create.dir = T)
g4_25_1 <- data4_24 %>%
  filter(c == "Ruminant") %>%
  ggplot() +
  geom_bar(aes(x = SCENARIO, y = Value, fill = var), stat = "identity") +
  facet_wrap(vars(region), scales = "free_y") +
  scale_fill_manual(values = var_color) +
  labs(y = "Volume [million t]", title = "Ruminant") +
  theme_2 +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 10)
  )
g4_25_2 <- data4_24 %>%
  filter(c == "NonRuminant") %>%
  ggplot() +
  geom_bar(aes(x = SCENARIO, y = Value, fill = var), stat = "identity") +
  facet_wrap(vars(region), scales = "free_y") +
  scale_fill_manual(values = var_color) +
  labs(y = "Volume [million t]", title = "Non Ruminant") +
  theme_2 +
  theme(
    legend.justification = c(-1.2, 0),
    plot.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    axis.title.y = element_blank()
  )
g4_25 <- g4_25_1 + g4_25_2
ggsave("../output/figure4_25.png", g4_25, width = 10, height = 6, create.dir = T)
# 4-26 --------------------------------------------------------------------
data4_26 <- data4_15_IAMC %>%
  mutate(SCENARIO = dplyr::recode(SCENARIO,
    "Mitigation" = "M",
    "BaU" = "B"
  )) %>%
  pivot_wider(names_from = "Model2", values_from = "Historical") %>%
  dplyr::select(!c(FAOSTAT, LUH2, EDGAR, `NA`)) %>%
  filter(variable %in% c("LAN_COV_BUI_ARE", "LAN_COV_CRO", "LAN_COV_CRO_ENE_CRO", "LAN_COV_FRS", "LAN_COV_PST", "LAN_COV_OTH_LAN", "LAN_COV_OTH_NAT")) %>%
  pivot_wider(names_from = "variable", values_from = "Value") %>%
  filter(SCENARIO != is.na(SCENARIO))
data4_26[is.na(data4_26)] <- 0
data4_26 <- data4_26 %>%
  mutate(LAN_COV = LAN_COV_CRO + LAN_COV_PST + LAN_COV_FRS + LAN_COV_OTH_NAT + LAN_COV_OTH_LAN) %>%
  mutate(
    Cropland = (LAN_COV_CRO - LAN_COV_CRO_ENE_CRO) / LAN_COV * 100,
    `Energy Crop` = LAN_COV_CRO_ENE_CRO / LAN_COV * 100,
    Grassland = LAN_COV_PST / LAN_COV * 100,
    Forest = LAN_COV_FRS / LAN_COV * 100,
    `Other natural land` = LAN_COV_OTH_NAT / LAN_COV * 100,
    `Non arable land` = LAN_COV_OTH_LAN / LAN_COV * 100
  ) %>%
  dplyr::select(!c("LAN_COV", "LAN_COV_CRO", "LAN_COV_CRO_ENE_CRO", "LAN_COV_FRS", "LAN_COV_PST", "LAN_COV_OTH_LAN", "LAN_COV_OTH_NAT")) %>%
  pivot_longer(cols = -c(SCENARIO, region, Y, UNIT, Model), names_to = "variable", values_to = "Value") %>%
  unite(col = Model_SCENARIO, Model, SCENARIO, sep = " | ") %>%
  mutate(variable = factor(variable, levels = c("Cropland", "Energy Crop", "Grassland", "Forest", "Other natural land", "Non arable land")))

data4_26_base <- data4_26 %>%
  filter(Y == "2015") %>%
  dplyr::select(!Y) %>%
  rename("base" = "Value")
data4_26 <- full_join(data4_26, data4_26_base) %>%
  mutate(change = Value - base)
labeli <- as_labeller(c(
  "BRA" = "Brazil",
  "CAN" = "Canada",
  "CHN" = "China",
  "IND" = "India",
  "JPN" = "Japan",
  "TUR" = "Turkey",
  "USA" = "United\nStates",
  "XE25" = "EU25",
  "XER" = "Rest of\nEurope",
  "XOC" = "Oceania",
  "XSE" = "Southeast\nAsia",
  "XSA" = "Rest of\nAsia",
  "XLM" = "Rest of\nSouth America",
  "CIS" = "Former Soviet\nUnion",
  "XME" = "Middle\nEast",
  "XNF" = "North\nAfrica",
  "XAF" = "Rest of\nAfrica",
  "World" = "World",
  "2015" = "2015",
  "2050" = "2050",
  "2100" = "2100"
))
g4_26_1 <- data4_26 %>%
  filter(Y %in% c("2015")) %>%
  mutate(Model_SCENARIO = factor(Model_SCENARIO, levels = c("AgLU | B", "AgLU | M", "AIM-Hub | B", "AIM-Hub | M"))) %>%
  ggplot() +
  geom_bar(aes(x = Model_SCENARIO, y = base, fill = variable), stat = "identity") +
  facet_wrap(vars(region), ncol = 18) +
  scale_fill_manual(values = land_color, guide = guide_legend(ncol = 6)) +
  labs(y = "Land Share [%]", title = "2015") +
  theme_2 +
  theme(legend.position = "none", axis.text.x = element_blank(), axis.ticks.x = element_blank())

g4_26_2 <- data4_26 %>%
  filter(Y %in% c("2050")) %>%
  mutate(Model_SCENARIO = factor(Model_SCENARIO, levels = c("AgLU | B", "AgLU | M", "AIM-Hub | B", "AIM-Hub | M"))) %>%
  ggplot() +
  geom_bar(aes(x = Model_SCENARIO, y = change, fill = variable), stat = "identity") +
  geom_hline(yintercept = 0, linetype = "dotted") +
  facet_wrap(vars(region), ncol = 18) +
  scale_fill_manual(values = land_color, guide = guide_legend(ncol = 6)) +
  labs(y = "Land Share change from 2015 [%]", title = "2050") +
  theme_2 +
  theme(legend.position = "none", axis.text.x = element_blank(), strip.background = element_blank(), strip.text = element_blank(), axis.ticks.x = element_blank())
g4_26_3 <- data4_26 %>%
  filter(Y %in% c("2100")) %>%
  mutate(Model_SCENARIO = factor(Model_SCENARIO, levels = c("AgLU | B", "AgLU | M", "AIM-Hub | B", "AIM-Hub | M"))) %>%
  ggplot() +
  geom_bar(aes(x = Model_SCENARIO, y = change, fill = variable), stat = "identity") +
  geom_hline(yintercept = 0, linetype = "dotted") +
  facet_wrap(vars(region), ncol = 18) +
  scale_fill_manual(values = land_color, guide = guide_legend(ncol = 6)) +
  labs(y = "Land Share change from 2015 [%]", title = "2100", caption = "B = BaU\nM = Mitigation") +
  theme_2 +
  theme(strip.background = element_blank(), strip.text = element_blank(), axis.text.x = element_text(size = 7, angle = 90, hjust = 1), plot.caption = element_text(size = 10, vjust = 17, hjust = -0), legend.text = element_text(size = 10))
g4_26 <- g4_26_1 / g4_26_2 / g4_26_3
ggsave("../output/figure4_26.png", g4_26, width = 11, height = 12, create.dir = T)
# 4-27 --------------------------------------------------------------------
data4_27 <- data4_15_IAMC %>%
  filter(variable == "FOO_AVA_PER_CAP")
data4_27_sim <- data4_27 %>%
  dplyr::select(SCENARIO, variable, region, Y, Model, Value, UNIT) %>%
  na.omit()
data4_27_hist <- data4_27 %>%
  dplyr::select(variable, region, Y, Model2, Historical) %>%
  na.omit()
g4_27 <- ggplot() +
  geom_point(data = data4_27_hist, aes(x = Y, y = Historical, shape = Model2), color = "#01810b", size = 1.5) +
  geom_line(data = data4_27_sim, aes(x = Y, y = Value, color = SCENARIO, linetype = Model), linewidth = 1) +
  geom_vline(xintercept = 2015, linetype = "dotted") +
  facet_wrap(vars(region), ncol = 6) +
  scale_x_continuous(breaks = c(2020, 2050, 2100)) +
  scale_color_manual(values = scenario_color) +
  scale_shape_manual(values = model_shape) +
  labs(y = "Food Intake [kcal/cap/day]") +
  theme_1
ggsave("../output/figure4_27.png", g4_27, width = 15, height = 7.5, create.dir = T)
# 4-28 --------------------------------------------------------------------
data4_28_country <- AgLU_AGMIP %>%
  filter(SCENARIO %in% c("SSP2_BaU_NoCC", "SSP2_500C_NoCC")) %>%
  mutate(Y = as.character(Y)) %>%
  mutate(Y = as.numeric(Y)) %>%
  mutate(SCENARIO = dplyr::recode(SCENARIO,
    "SSP2_500C_NoCC" = "Mitigation",
    "SSP2_BaU_NoCC" = "BaU"
  )) %>%
  filter(AgMIP_VAR == "CALI", ITEM == "tot") %>%
  pivot_wider(names_from = "SCENARIO", values_from = "AGMIP") %>%
  mutate(change = (Mitigation - BaU) / BaU * 100) %>%
  dplyr::select(!c(AgMIP_VAR, ITEM, AgMIP_UNIT, BaU, Mitigation)) %>%
  na.omit() %>%
  rename("country_change" = "change")
data4_28_country <- full_join(data4_28_country, region) %>%
  mutate(Model = "AgLU")
data4_28_region <- AGMIP_agg %>%
  filter(SCENARIO %in% c("SSP2_BaU_NoCC", "SSP2_500C_NoCC")) %>%
  mutate(Y = as.character(Y)) %>%
  mutate(Y = as.numeric(Y)) %>%
  mutate(SCENARIO = dplyr::recode(SCENARIO,
    "SSP2_500C_NoCC" = "Mitigation",
    "SSP2_BaU_NoCC" = "BaU"
  )) %>%
  filter(AgMIP_VAR == "CALI", ITEM == "tot") %>%
  pivot_wider(names_from = "SCENARIO", values_from = "AGMIP_agg") %>%
  mutate(change = (Mitigation - BaU) / BaU * 100) %>%
  dplyr::select(!c(AgMIP_VAR, ITEM, AgMIP_UNIT, BaU, Mitigation)) %>%
  na.omit() %>%
  rename("region_change" = "change") %>%
  filter(region %in% region_AIM) %>%
  filter(Y == "2100") %>%
  mutate(Model = "AgLU")
data4_28 <- full_join(data4_28_country, data4_28_region) %>%
  filter(Y == "2100")
data4_28_AIM_Hub <- data4_15_AIM_Hub %>%
  filter(variable == "FOO_AVA_PER_CAP", region %in% region_AIM, Y == "2100") %>%
  pivot_wider(names_from = "SCENARIO", values_from = "AIM-Hub") %>%
  mutate(region_change = (Mitigation - BaU) / BaU * 100) %>%
  dplyr::select(region, region_change) %>%
  mutate(Model = "AIM-Hub")
data4_28 <- full_join(data4_28, data4_28_AIM_Hub) %>%
  mutate(region = dplyr::recode(region,
    "BRA" = "Brazil",
    "CAN" = "Canada",
    "CHN" = "China",
    "IND" = "India",
    "JPN" = "Japan",
    "TUR" = "Turkey",
    "USA" = "United\nStates",
    "XE25" = "EU25",
    "XER" = "Rest of\nEurope",
    "XOC" = "Oceania",
    "XSE" = "Southeast\nAsia",
    "XSA" = "Rest of\nAsia",
    "XLM" = "Rest of\nSouth America",
    "CIS" = "Former\nSoviet Union",
    "XME" = "Middle\nEast",
    "XNF" = "North\nAfrica",
    "XAF" = "Rest of\nAfrica",
    "World" = "World"
  ))
g4_28 <- ggplot() +
  geom_bar(data = data4_28, aes(x = region, y = region_change, fill = Model), stat = "identity", position = "dodge") +
  geom_point(data = data4_28, aes(x = region, y = country_change), position = position_dodge(width = 0.45, preserve = "single"), size = 1.2) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  scale_fill_manual(values = c("#f68181", "#76a1ea")) +
  labs(y = "Food Intake Change from BaU [%]") +
  theme_2 +
  theme(legend.position = "right")
ggsave("../output/figure4_28.png", plot = g4_28, width = 8, height = 4.5)
data4_28 <- data4_28 %>%
  mutate(region = dplyr::recode(region,
    "Brazil" = "ブラジル",
    "Canada" = "カナダ",
    "China" = "中国",
    "India" = "インド",
    "Japan" = "日本",
    "Turkey" = "トルコ",
    "United\nStates" = "アメリカ",
    "EU25" = "EU諸国",
    "Rest of\nEurope" = "その他ヨーロッパ",
    "Oceania" = "オセアニア",
    "Southeast\nAsia" = "東南アジア",
    "Rest of\nAsia" = "その他アジア",
    "Rest of\nSouth America" = "その他南米",
    "Former\nSoviet Union" = "旧ソ連諸国",
    "Middle\nEast" = "中東",
    "North\nAfrica" = "北アフリカ",
    "Rest of\nAfrica" = "その他アフリカ",
    "World" = "世界"
  ))
g4_28 <- ggplot() +
  geom_bar(data = data4_28, aes(x = region, y = region_change, fill = Model), stat = "identity", position = "dodge") +
  geom_point(data = data4_28, aes(x = region, y = country_change), position = position_dodge(width = 0.45, preserve = "single"), size = 1.2) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  # scale_x_discrete(labels = country) +
  scale_fill_manual(values = c("#f68181", "#76a1ea")) +
  labs(y = "緩和策によるBaUからの一人当たり食料消費変化 [%]") +
  theme_2 +
  theme(legend.position = "right", axis.text.x = element_text(size = 10))
ggsave("../output/presentation4.png", plot = g4_28, width = 8, height = 4.5)
# 4-29 --------------------------------------------------------------------
data4_29_2100 <- data4_28_country %>% filter(Y == "2100")
data4_29 <- full_join(Boundary, data4_29_2100)
g4_29 <- ggplot() +
  geom_sf(data = data4_29, aes(fill = country_change)) +
  theme_bw() +
  scale_fill_gradient2(
    low = "#BA2320",
    mid = "white",
    high = "#253368",
    name = "Food Intake Change from BaU [%]"
  ) +
  coord_sf(xlim = c(-180, 180), ylim = c(-90, 90)) +
  theme(
    panel.border = element_blank(),
    axis.text.x = element_blank(),
    axis.title.y = element_blank(),
    text = element_text(size = 12),
    legend.position = c(0.5, 0.1),
    legend.key.size = unit(1, "cm"),
    legend.direction = "horizontal",
    strip.background = element_blank(),
    plot.margin = margin(0, 0, 0, 0)
  )
ggsave("../output/figure4_29.png", plot = g4_29, width = 12, height = 6)
# 4-30 --------------------------------------------------------------------
data4_30_food <- food %>%
  filter(SCENARIO %in% c("SSP2_BaU_NoCC", "SSP2_500C_NoCC")) %>%
  mutate(SCENARIO = dplyr::recode(SCENARIO,
    "SSP2_500C_NoCC" = "food_Mitigation",
    "SSP2_BaU_NoCC" = "food_BaU"
  )) %>%
  mutate(food_cal_load = food_cal_load / 1000) %>%
  pivot_wider(names_from = "SCENARIO", values_from = "food_cal_load") %>%
  mutate(food_change = (food_Mitigation - food_BaU) / food_BaU * 100)
data4_30_PC <- PC %>%
  filter(SCENARIO %in% c("SSP2_BaU_NoCC", "SSP2_500C_NoCC")) %>%
  mutate(SCENARIO = dplyr::recode(SCENARIO,
    "SSP2_500C_NoCC" = "price_Mitigation",
    "SSP2_BaU_NoCC" = "price_BaU"
  )) %>%
  mutate(PC_load = PC_load)
data4_30_Cal <- Cal %>%
  filter(SCENARIO %in% c("SSP2_BaU_NoCC", "SSP2_500C_NoCC")) %>%
  mutate(SCENARIO = dplyr::recode(SCENARIO,
    "SSP2_500C_NoCC" = "price_Mitigation",
    "SSP2_BaU_NoCC" = "price_BaU"
  ))
data4_30_PC <- full_join(data4_30_PC, data4_30_Cal)
data4_30_PC <- full_join(data4_30_PC, Cal_coef) %>%
  na.omit() %>%
  mutate(Value = PC_load * Cal_pc_load / Cal_coef) %>%
  group_by(SCENARIO, cty, Y) %>%
  summarise(Value = sum(Value), Volume = sum(Cal_pc_load)) %>%
  mutate(Price = Value / Volume) %>%
  dplyr::select(!c(Value, Volume)) %>%
  pivot_wider(names_from = "SCENARIO", values_from = "Price") %>%
  mutate(price_change = (price_Mitigation - price_BaU) / price_BaU * 100)
data4_30_GDPPC <- GDPPC %>%
  filter(sc == "SSP2") %>%
  mutate(GDPpc = GDPpc / 1000 / 1000) %>%
  dplyr::select(!c(sc)) %>%
  rename("Y" = "yr")
data4_30_Suff <- AgLU_AGMIP %>%
  filter(SCENARIO == "SSP2_BaU_NoCC") %>%
  filter(AgMIP_VAR %in% c("CONS", "PROD"), ITEM == "AGR", AgMIP_UNIT == "million kcal") %>%
  dplyr::select(!c(SCENARIO, ITEM, AgMIP_UNIT)) %>%
  pivot_wider(names_from = "AgMIP_VAR", values_from = "AGMIP") %>%
  mutate(Suff = PROD / CONS * 100) %>%
  na.omit() %>%
  dplyr::select(!c(PROD, CONS))
data4_30_EF <- EF %>%
  group_by(a, cty) %>%
  summarise(EF = sum(EFbase)) %>%
  rename("c" = "a")
data4_30_QH <- QH %>%
  filter(SCENARIO == "SSP2_BaU_NoCC") %>%
  dplyr::select(!SCENARIO)
data4_30_QH <- full_join(data4_30_QH, Cal_coef) %>%
  mutate(Cal = QH_load * Cal_coef)
data4_30_EF <- full_join(data4_30_EF, data4_30_QH) %>%
  na.omit() %>%
  mutate(GHG = EF * QH_load) %>%
  group_by(cty, Y) %>%
  summarise(GHG = sum(GHG), Cal = sum(Cal)) %>%
  mutate(EF = GHG / Cal * 1000) %>%
  dplyr::select(cty, Y, EF)
data4_30_Yld <- Yld %>%
  filter(SCENARIO == "SSP2_BaU_NoCC") %>%
  dplyr::select(!SCENARIO)
data4_30_QL <- QL %>%
  filter(SCENARIO == "SSP2_BaU_NoCC") %>%
  dplyr::select(!SCENARIO)
data4_30_Yld <- full_join(Yld, data4_30_QL) %>%
  mutate(QSUP = Yld_load * QL_load)
data4_30_Yld <- full_join(data4_30_Yld, Cal_coef) %>%
  mutate(Cal = QSUP * Cal_coef) %>%
  na.omit() %>%
  group_by(cty, Y) %>%
  summarise(Cal = sum(Cal), QL = sum(QL_load)) %>%
  mutate(Yld = Cal / QL / 1000) %>%
  dplyr::select(!Cal)
data4_30 <- full_join(data4_30_food, data4_30_PC)
data4_30 <- full_join(data4_30, data4_30_GDPPC)
data4_30 <- full_join(data4_30, data4_30_Suff)
data4_30 <- full_join(data4_30, data4_30_EF)
data4_30 <- full_join(data4_30, data4_30_Yld) %>%
  filter(Y == "2100")
g4_30_1 <- data4_30 %>%
  ggplot() +
  geom_point(aes(x = GDPpc, y = food_change), size = 1, color = "#C3327A") +
  geom_hline(yintercept = 0, linetype = "dotted", linewidth = 1) +
  geom_smooth(aes(x = GDPpc, y = food_change), method = "lm", se = TRUE, color = "black", size = 1) +
  stat_poly_eq(aes(x = GDPpc, y = food_change, label = paste(..eq.label.., ..rr.label.., ..p.value.label.., sep = "~~~")),
    parse = TRUE, size = 4, label.y = 0.05, label.x = 0.95
  ) +
  labs(x = "GDP per capita [million US$/cap]", y = "Food Intake Change [%]") +
  theme_3
g4_30_2 <- data4_30 %>%
  ggplot() +
  geom_point(aes(x = price_BaU, y = food_change), size = 1, color = "#C3327A") +
  geom_hline(yintercept = 0, linetype = "dotted", linewidth = 1) +
  geom_smooth(aes(x = price_BaU, y = food_change), method = "lm", se = TRUE, color = "black", size = 1) +
  stat_poly_eq(aes(x = price_BaU, y = food_change, label = paste(..eq.label.., ..rr.label.., ..p.value.label.., sep = "~~~")),
    parse = TRUE, size = 4, label.y = 0.05, label.x = 0.95
  ) +
  labs(x = "Food Price in BaU [US$/1000 kcal]", y = "Food Intake Change [%]") +
  theme_3
g4_30_3 <- data4_30 %>%
  ggplot() +
  geom_point(aes(x = food_BaU, y = food_change), size = 1, color = "#C3327A") +
  geom_hline(yintercept = 0, linetype = "dotted", linewidth = 1) +
  geom_smooth(aes(x = food_BaU, y = food_change), method = "lm", se = TRUE, color = "black", size = 1) +
  stat_poly_eq(aes(x = food_BaU, y = food_change, label = paste(..eq.label.., ..rr.label.., ..p.value.label.., sep = "~~~")),
    parse = TRUE, size = 4, label.y = 0.05, label.x = 0.95
  ) +
  labs(x = "Food Intake in BaU [1000kcal/cap/day]", y = "Food Intake Change [%]") +
  theme_3
g4_30_4 <- data4_30 %>%
  ggplot() +
  geom_point(aes(x = Suff, y = food_change), size = 1, color = "#C3327A") +
  geom_hline(yintercept = 0, linetype = "dotted", linewidth = 1) +
  geom_smooth(aes(x = Suff, y = food_change), method = "lm", se = TRUE, color = "black", size = 1) +
  stat_poly_eq(aes(x = Suff, y = food_change, label = paste(..eq.label.., ..rr.label.., ..p.value.label.., sep = "~~~")),
    parse = TRUE, size = 4, label.y = 0.05, label.x = 0.95
  ) +
  labs(x = "Self Sufficiency in BaU [%]", y = "Food Intake Change [%]") +
  theme_3
g4_30_5 <- data4_30 %>%
  ggplot() +
  geom_point(aes(x = EF, y = food_change), size = 1, color = "#C3327A") +
  geom_hline(yintercept = 0, linetype = "dotted", linewidth = 1) +
  geom_smooth(aes(x = EF, y = food_change), method = "lm", se = TRUE, color = "black", size = 1) +
  stat_poly_eq(aes(x = EF, y = food_change, label = paste(..eq.label.., ..rr.label.., ..p.value.label.., sep = "~~~")),
    parse = TRUE, size = 4, label.y = 0.05, label.x = 0.95
  ) +
  labs(x = "Emission Factor [gCO2eq/kcal]", y = "Food Intake Change [%]") +
  theme_3
g4_30_6 <- data4_30 %>%
  ggplot() +
  geom_point(aes(x = Yld, y = food_change), size = 1, color = "#C3327A") +
  geom_hline(yintercept = 0, linetype = "dotted", linewidth = 1) +
  geom_smooth(aes(x = Yld, y = food_change), method = "lm", se = TRUE, color = "black", size = 1) +
  stat_poly_eq(aes(x = Yld, y = food_change, label = paste(..eq.label.., ..rr.label.., ..p.value.label.., sep = "~~~")),
    parse = TRUE, size = 4, label.y = 0.05, label.x = 0.95
  ) +
  labs(x = "Productivity in BaU [kcal/ha]", y = "Food Intake Change [%]") +
  theme_3
g4_30 <- g4_30_1 + g4_30_2 + g4_30_3 + g4_30_4 + g4_30_5 + g4_30_6
ggsave("../output/figure4_30.png", plot = g4_30, width = 15, height = 8)
g4_30_1 <- data4_30 %>%
  ggplot() +
  geom_point(aes(x = GDPpc, y = food_change), size = 1, color = "#C3327A") +
  geom_hline(yintercept = 0, linetype = "dotted", linewidth = 1) +
  geom_smooth(aes(x = GDPpc, y = food_change), method = "lm", se = TRUE, color = "black", size = 1) +
  stat_poly_eq(aes(x = GDPpc, y = food_change, label = paste(..eq.label.., ..rr.label.., ..p.value.label.., sep = "~~~")),
    parse = TRUE, size = 4, label.y = 0.05, label.x = 0.95
  ) +
  labs(x = "一人当たりGDP [million US$/cap]", y = "食料消費量変化 [%]") +
  theme_3
g4_30_2 <- data4_30 %>%
  ggplot() +
  geom_point(aes(x = price_BaU, y = food_change), size = 1, color = "#C3327A") +
  geom_hline(yintercept = 0, linetype = "dotted", linewidth = 1) +
  geom_smooth(aes(x = price_BaU, y = food_change), method = "lm", se = TRUE, color = "black", size = 1) +
  stat_poly_eq(aes(x = price_BaU, y = food_change, label = paste(..eq.label.., ..rr.label.., ..p.value.label.., sep = "~~~")),
    parse = TRUE, size = 4, label.y = 0.05, label.x = 0.95
  ) +
  labs(x = "BaUの食料価格 [US$/1000kcal]", y = "食料消費量変化 [%]") +
  theme_3
g4_30_3 <- data4_30 %>%
  ggplot() +
  geom_point(aes(x = EF, y = food_change), size = 1, color = "#C3327A") +
  geom_hline(yintercept = 0, linetype = "dotted", linewidth = 1) +
  geom_smooth(aes(x = EF, y = food_change), method = "lm", se = TRUE, color = "black", size = 1) +
  stat_poly_eq(aes(x = EF, y = food_change, label = paste(..eq.label.., ..rr.label.., ..p.value.label.., sep = "~~~")),
    parse = TRUE, size = 4, label.y = 0.05, label.x = 0.95
  ) +
  labs(x = "GHG排出係数 [gCO2eq/kcal]", y = "食料消費量変化 [%]") +
  theme_3
g4_30 <- g4_30_1 + g4_30_2 + g4_30_3
ggsave("../output/presentation5.png", plot = g4_30, width = 15, height = 4)
regression <- lm(data = data4_30, food_change ~ price_BaU + GDPpc + EF)
regresult <- summary(regression)
regresult
confint(regression)
vif(regression)
# 4-31 --------------------------------------------------------------------
data4_31 <- QSUP %>%
  filter(SCENARIO %in% c("SSP2_BaU_NoCC", "SSP2_500C_NoCC")) %>%
  filter(c != "ecr") %>%
  filter(!c %in% c("swt", "alc", "vol")) %>%
  mutate(SCENARIO = dplyr::recode(SCENARIO,
    "SSP2_500C_NoCC" = "Mitigation",
    "SSP2_BaU_NoCC" = "BaU"
  )) %>%
  pivot_wider(names_from = "SCENARIO", values_from = "QSUP_load") %>%
  mutate(QSUP_change = (Mitigation - BaU)) %>%
  dplyr::select(!c(Mitigation, BaU))
data4_31_total <- data4_31 %>%
  group_by(cty, Y) %>%
  mutate(change_total = sum(QSUP_change))
data4_31 <- full_join(data4_31, data4_31_total)
data4_31_select <- data4_31 %>%
  filter(Y == "2100") %>%
  group_by(cty, Y) %>%
  summarise(total = sum(QSUP_change))
QSUPselect1 <- data4_31_select[order(data4_31_select$total), ][1:15, ]
select_cty1 <- unique(as.character(QSUPselect1$cty))
QSUPselect2 <- data4_31_select[order(-data4_31_select$total), ][1:15, ]
select_cty2 <- unique(as.character(QSUPselect2$cty))
select_cty <- union(select_cty1, select_cty2)
data4_31 <- data4_31 %>%
  filter(Y == "2100", cty %in% select_cty)
g4_31 <- data4_31 %>%
  filter(Y == "2100", cty %in% select_cty) %>%
  mutate(QSUP_change = QSUP_change / 1000, change_total = change_total / 1000) %>%
  ggplot() +
  geom_bar(aes(x = reorder(cty, change_total), y = QSUP_change, fill = c), stat = "identity") +
  geom_point(aes(x = reorder(cty, change_total), y = change_total)) +
  geom_hline(yintercept = 0, linetype = "dotted", linewidth = 1) +
  scale_fill_manual(values = item_color, guide = guide_legend(ncol = 11)) +
  labs(y = "Supply change from BaU [million t]") +
  theme_2 +
  theme(legend.key.size = unit(0.8, "lines"))
ggsave("../output/figure4_31.png", plot = g4_31, width = 8, height = 4)
# 4-32 --------------------------------------------------------------------
data4_32_QSUP <- QSUP %>%
  filter(c != "ecr") %>%
  filter(!c %in% c("swt", "alc", "vol")) %>%
  filter(SCENARIO %in% c("SSP2_BaU_NoCC", "SSP2_500C_NoCC")) %>%
  mutate(SCENARIO = dplyr::recode(SCENARIO,
    "SSP2_500C_NoCC" = "Mitigation",
    "SSP2_BaU_NoCC" = "BaU"
  )) %>%
  group_by(SCENARIO, cty, Y) %>%
  summarise(QSUP = sum(QSUP_load))
data4_32_QL <- QL %>%
  filter(c != "ecr") %>%
  filter(SCENARIO %in% c("SSP2_BaU_NoCC", "SSP2_500C_NoCC")) %>%
  mutate(SCENARIO = dplyr::recode(SCENARIO,
    "SSP2_500C_NoCC" = "Mitigation",
    "SSP2_BaU_NoCC" = "BaU"
  )) %>%
  group_by(SCENARIO, cty, Y) %>%
  summarise(QL = sum(QL_load))
data4_32_Yld <- full_join(data4_32_QSUP, data4_32_QL) %>%
  mutate(YLD = QSUP / QL) %>%
  dplyr::select(!c(QSUP, QL)) %>%
  pivot_wider(names_from = "SCENARIO", values_from = "YLD") %>%
  mutate(YLD_change = (Mitigation - BaU), YLD_ave = (Mitigation + BaU) / 2) %>%
  dplyr::select(!c(Mitigation, BaU))
data4_32_QL <- data4_32_QL %>%
  pivot_wider(names_from = "SCENARIO", values_from = "QL") %>%
  mutate(QL_change = (Mitigation - BaU), QL_ave = (Mitigation + BaU) / 2) %>%
  dplyr::select(!c(Mitigation, BaU))
data4_32 <- full_join(data4_32_Yld, data4_32_QL) %>%
  mutate(Productivity = YLD_change * QL_ave, Land = QL_change * YLD_ave) %>%
  dplyr::select(cty, Y, Productivity, Land) %>%
  mutate(Supply = Productivity + Land) %>%
  pivot_longer(cols = c(Productivity, Land), names_to = "var", values_to = "value")
g4_32 <- data4_32 %>%
  filter(Y == "2100", cty %in% select_cty) %>%
  mutate(Supply = Supply / 1000, value = value / 1000) %>%
  ggplot() +
  geom_bar(aes(x = reorder(cty, Supply), y = value, fill = var), stat = "identity") +
  geom_point(aes(x = reorder(cty, Supply), y = Supply)) +
  geom_hline(yintercept = 0, linetype = "dotted", linewidth = 1) +
  scale_fill_manual(values = c("#E3819C", "#A0EBD0")) +
  labs(y = "Supply change from BaU [million t]") +
  theme_2
ggsave("../output/figure4_32.png", plot = g4_32, width = 8, height = 4)
# 5-1~5-3 -----------------------------------------------------------------
data5_1_QSUP <- QSUP %>%
  filter(SCENARIO %in% c("SSP2_BaU_NoCC", "SSP2_500C_NoCC")) %>%
  mutate(SCENARIO = dplyr::recode(SCENARIO,
    "SSP2_500C_NoCC" = "Mitigation",
    "SSP2_BaU_NoCC" = "BaU"
  )) %>%
  group_by(SCENARIO, c, cty, Y) %>%
  summarise(QSUP_load = sum(QSUP_load)) %>%
  pivot_wider(names_from = "SCENARIO", values_from = "QSUP_load") %>%
  mutate(Supply_change = Mitigation / BaU * 100) %>%
  dplyr::select(!c(Mitigation))
data5_1_EF <- EF %>%
  group_by(a, cty) %>%
  summarise(EF = sum(EFbase)) %>%
  ungroup() %>%
  rename("c" = "a")
data5_1_QL <- QL %>%
  filter(SCENARIO == "SSP2_BaU_NoCC") %>%
  dplyr::select(!SCENARIO) %>%
  group_by(c, cty, Y) %>%
  summarise(QL = sum(QL_load))
data5_1_PP <- PP %>%
  filter(SCENARIO == "SSP2_BaU_NoCC") %>%
  dplyr::select(!SCENARIO)
data5_1 <- full_join(data5_1_QSUP, data5_1_EF)
data5_1 <- full_join(data5_1, data5_1_QL)
data5_1 <- full_join(data5_1, data5_1_PP) %>%
  mutate(
    EF = log10(EF),
    Yld = log10(BaU / QL),
    PP = log10(PP_load),
    Supply_change = log10(Supply_change)
  ) %>%
  filter(!c %in% c("ecr", "swt", "alc", "vol", "pmt", "omt", "egg")) %>%
  filter(Y == "2100")
g5_1 <- data5_1 %>%
  ggplot() +
  geom_point(aes(x = EF, y = Supply_change), size = 1, color = "#C3327A") +
  geom_hline(yintercept = 2, linetype = "dotted", linewidth = 1) +
  geom_smooth(aes(x = EF, y = Supply_change), method = "lm", se = TRUE, color = "black", size = 1) +
  stat_poly_eq(aes(x = EF, y = Supply_change, label = paste(..eq.label.., ..rr.label.., ..p.value.label.., sep = "~~~")),
    parse = TRUE, size = 3.3, label.y = 0.95, label.x = 0.95
  ) +
  facet_wrap(vars(c), scale = "free") +
  labs(x = "log(Emission Factor [tCO2-eq/t])", y = "log(Production Change from BaU [%])") +
  theme_3
ggsave("../output/figure5_1.png", plot = g5_1, width = 15, height = 10, create.dir = T)
g5_2 <- data5_1 %>%
  ggplot() +
  geom_point(aes(x = Yld, y = Supply_change), size = 1, color = "#C3327A") +
  geom_hline(yintercept = 2, linetype = "dotted", linewidth = 1) +
  geom_smooth(aes(x = Yld, y = Supply_change), method = "lm", se = TRUE, color = "black", size = 1) +
  stat_poly_eq(aes(x = Yld, y = Supply_change, label = paste(..eq.label.., ..rr.label.., ..p.value.label.., sep = "~~~")),
    parse = TRUE, size = 3.3, label.y = 0.95, label.x = 0.95
  ) +
  facet_wrap(vars(c), scale = "free") +
  labs(x = "log(Land Producivity [t/ha])", y = "log(Production Change from BaU [%])") +
  theme_3
ggsave("../output/figure5_2.png", plot = g5_2, width = 15, height = 10, create.dir = T)
g5_3 <- data5_1 %>%
  ggplot() +
  geom_point(aes(x = PP, y = Supply_change), size = 1, color = "#C3327A") +
  geom_hline(yintercept = 2, linetype = "dotted", linewidth = 1) +
  geom_smooth(aes(x = PP, y = Supply_change), method = "lm", se = TRUE, color = "black", size = 1) +
  stat_poly_eq(aes(x = PP, y = Supply_change, label = paste(..eq.label.., ..rr.label.., ..p.value.label.., sep = "~~~")),
    parse = TRUE, size = 3.3, label.y = 0.95, label.x = 0.95
  ) +
  facet_wrap(vars(c), scale = "free") +
  labs(x = "log(Producer Price [US$/t])", y = "log(Production Change from BaU [%])") +
  theme_3
ggsave("../output/figure5_3.png", plot = g5_3, width = 15, height = 10, create.dir = T)
# presentation ------------------------------------------------------------
presentation_QSUP_total <- QSUP_agg %>%
  filter(c != "ecr") %>%
  filter(!c %in% c("swt", "alc", "vol")) %>%
  filter(SCENARIO == "SSP2_BaU_NoCC") %>%
  dplyr::select(!SCENARIO) %>%
  group_by(region, Y) %>%
  summarise(QSUP = sum(QSUP_agg_load))
presentation_QSUP <- QSUP_agg %>%
  filter(SCENARIO %in% c("SSP2_BaU_NoCC", "SSP2_500C_NoCC")) %>%
  filter(c != "ecr") %>%
  filter(!c %in% c("swt", "alc", "vol")) %>%
  mutate(SCENARIO = dplyr::recode(SCENARIO,
    "SSP2_500C_NoCC" = "Mitigation",
    "SSP2_BaU_NoCC" = "BaU"
  )) %>%
  pivot_wider(names_from = "SCENARIO", values_from = "QSUP_agg_load") %>%
  mutate(QSUP_change = (Mitigation - BaU)) %>%
  dplyr::select(!c(Mitigation, BaU))
presentation_QSUP <- full_join(presentation_QSUP, presentation_QSUP_total) %>%
  mutate(change_rate = QSUP_change / QSUP * 100)
presentation_QSUP_total <- presentation_QSUP %>%
  group_by(region, Y) %>%
  mutate(change_rate_total = sum(change_rate))
presentation_QSUP <- full_join(presentation_QSUP, presentation_QSUP_total) %>%
  filter(Y == "2100", region %in% region_AIM) %>%
  mutate(Model = "AgLU") %>%
  mutate(Y = as.numeric(as.character(Y))) %>%
  mutate(region = dplyr::recode(region,
    "BRA" = "ブラジル",
    "CAN" = "カナダ",
    "CHN" = "中国",
    "IND" = "インド",
    "JPN" = "日本",
    "TUR" = "トルコ",
    "USA" = "アメリカ",
    "XE25" = "EU諸国",
    "XER" = "その他ヨーロッパ",
    "XOC" = "オセアニア",
    "XSE" = "東南アジア",
    "XSA" = "その他アジア",
    "XLM" = "その他南米",
    "CIS" = "旧ソ連諸国",
    "XME" = "中東",
    "XNF" = "北アフリカ",
    "XAF" = "その他アフリカ",
    "World" = "世界"
  )) %>%
  mutate(com = dplyr::recode(c,
    "wht" = "小麦",
    "rce" = "米",
    "crl" = "その他穀物",
    "mze" = "その他穀物",
    "str" = "その他作物",
    "sgr" = "砂糖原料",
    "pls" = "その他作物",
    "ocr" = "油糧作物",
    "nut" = "野菜、フルーツ、ナッツ",
    "vgt" = "野菜、フルーツ、ナッツ",
    "frt" = "野菜、フルーツ、ナッツ",
    "stm" = "その他作物",
    "spc" = "その他作物",
    "cmt" = "反芻家畜",
    "pmt" = "非反芻家畜",
    "omt" = "非反芻家畜",
    "rmt" = "反芻家畜",
    "mlk" = "ミルク",
    "egg" = "卵"
  )) %>%
  dplyr::select(region, change_rate, change_rate_total, Model, com)
g_presentation1 <- presentation_QSUP %>%
  ggplot() +
  geom_bar(aes(x = region, y = change_rate, fill = com), stat = "identity") +
  geom_point(aes(x = region, y = change_rate_total, shape = Model), size = 3) +
  geom_hline(yintercept = 0, linetype = "dotted", linewidth = 1) +
  scale_fill_manual(values = item_color, guide = guide_legend(ncol = 4)) +
  scale_shape_manual(values = c(16, 15), ) +
  labs(y = "緩和策によるBaUからの生産量変化 [%]") +
  theme_2 +
  theme(legend.text = element_text(size = 10), legend.key.size = unit(0.4, "lines"), legend.justification = c(0.5, 0), axis.text.x = element_text(size = 10), axis.title.y = element_text(size = 14)) +
  guides(shape = "none")
presentation_QSUP_total <- QSUP_agg %>%
  filter(c != "ecr") %>%
  filter(!c %in% c("swt", "alc", "vol")) %>%
  filter(SCENARIO %in% c("SSP2_BaU_NoCC", "SSP2_500C_NoCC")) %>%
  mutate(SCENARIO = dplyr::recode(SCENARIO,
    "SSP2_500C_NoCC" = "Mitigation",
    "SSP2_BaU_NoCC" = "BaU"
  )) %>%
  group_by(SCENARIO, region, Y) %>%
  summarise(QSUP = sum(QSUP_agg_load))
presentation_QL_total <- QL_agg %>%
  filter(c != "ecr") %>%
  filter(SCENARIO %in% c("SSP2_BaU_NoCC", "SSP2_500C_NoCC")) %>%
  mutate(SCENARIO = dplyr::recode(SCENARIO,
    "SSP2_500C_NoCC" = "Mitigation",
    "SSP2_BaU_NoCC" = "BaU"
  )) %>%
  group_by(SCENARIO, region, Y) %>%
  summarise(QL = sum(QL_agg_load))
presentation_YLD_total <- full_join(presentation_QSUP_total, presentation_QL_total) %>%
  mutate(YLD = QSUP / QL) %>%
  dplyr::select(!c(QSUP, QL)) %>%
  pivot_wider(names_from = "SCENARIO", values_from = "YLD") %>%
  mutate(YLD_change = (Mitigation - BaU), YLD_ave = (Mitigation + BaU) / 2) %>%
  dplyr::select(!c(Mitigation, BaU))
presentation_QL_total <- presentation_QL_total %>%
  pivot_wider(names_from = "SCENARIO", values_from = "QL") %>%
  mutate(QL_change = (Mitigation - BaU), QL_ave = (Mitigation + BaU) / 2) %>%
  dplyr::select(!c(Mitigation, BaU))
presentation_QSUP_decomp <- full_join(presentation_YLD_total, presentation_QL_total) %>%
  mutate(Productivity = YLD_change * QL_ave, Land = QL_change * YLD_ave) %>%
  dplyr::select(region, Y, Productivity, Land) %>%
  mutate(Supply = Productivity + Land) %>%
  pivot_longer(cols = c(Productivity, Land), names_to = "var", values_to = "value")
presentation_QSUP_BaU <- presentation_QSUP_total %>%
  filter(SCENARIO == "BaU") %>%
  dplyr::select(!SCENARIO)
presentation_QSUP_decomp <- full_join(presentation_QSUP_decomp, presentation_QSUP_BaU) %>%
  mutate(value = value / QSUP * 100, Supply = Supply / QSUP * 100) %>%
  filter(Y == "2100", region %in% region_AIM) %>%
  mutate(Model = "AgLU") %>%
  mutate(Y = as.numeric(as.character(Y))) %>%
  dplyr::select(!c(QSUP, SCENARIO))
presentation_AIM_Hub <- data4_15_AIM_Hub %>%
  filter(variable %in% c("AGR_PRO_CRO", "AGR_PRO_LIV"), Y == "2100", region %in% region_AIM) %>%
  group_by(SCENARIO, region, Y) %>%
  summarise(AIMHub = sum(`AIM-Hub`)) %>%
  ungroup() %>%
  pivot_wider(names_from = "SCENARIO", values_from = "AIMHub") %>%
  mutate(Supply = (Mitigation - BaU) / BaU * 100) %>%
  mutate(Model = "AIM-Hub") %>%
  mutate(var = "Land", value = 0) %>%
  dplyr::select(!c(Mitigation, BaU))
presentation_QSUP_decomp <- full_join(presentation_QSUP_decomp, presentation_AIM_Hub) %>%
  mutate(region = dplyr::recode(region,
    "BRA" = "ブラジル",
    "CAN" = "カナダ",
    "CHN" = "中国",
    "IND" = "インド",
    "JPN" = "日本",
    "TUR" = "トルコ",
    "USA" = "アメリカ",
    "XE25" = "EU諸国",
    "XER" = "その他ヨーロッパ",
    "XOC" = "オセアニア",
    "XSE" = "東南アジア",
    "XSA" = "その他アジア",
    "XLM" = "その他南米",
    "CIS" = "旧ソ連諸国",
    "XME" = "中東",
    "XNF" = "北アフリカ",
    "XAF" = "その他アフリカ",
    "World" = "世界"
  )) %>%
  mutate(var = dplyr::recode(var,
    "Land" = "農地面積変化",
    "Productivity" = "土地生産性変化"
  ))
g_presentation2 <- presentation_QSUP_decomp %>%
  mutate(Supply = Supply, value = value) %>%
  ggplot() +
  geom_bar(aes(x = region, y = value, fill = var), stat = "identity") +
  geom_point(aes(x = region, y = Supply, shape = Model, color = Model), size = 3) +
  geom_hline(yintercept = 0, linetype = "dotted", linewidth = 1) +
  scale_fill_manual(values = c("#E3819C", "#A0EBD0"), name = "AgLU結果の要因分解") +
  scale_shape_manual(values = c(16, 4), name = "モデル") +
  scale_colour_manual(values = c("black", "black"), name = "モデル") +
  labs(y = "緩和策によるBaUからの生産量変化 [%]") +
  theme_2 +
  theme(axis.text.x = element_text(size = 10), legend.text = element_text(size = 10), axis.title.y = element_text(size = 14), legend.title = element_text(size = 10), legend.title.position = "top", legend.justification = c(1, 1))
g_presentation <- g_presentation1 - g_presentation2
ggsave("../output/presentation3.png", plot = g_presentation, width = 10, height = 6)
g_presentation2 <- presentation_QSUP_decomp %>%
  mutate(Supply = Supply, value = value) %>%
  ggplot() +
  geom_bar(aes(x = region, y = value, fill = var), stat = "identity", alpha = 0.8) +
  geom_point(aes(x = region, y = Supply, shape = Model, color = Model), size = 4) +
  geom_hline(yintercept = 0, linetype = "dotted", linewidth = 1) +
  scale_fill_manual(values = c("#E3819C", "#A0EBD0"), name = "AgLU結果の要因分解") +
  scale_shape_manual(values = c(16, 4), name = "モデル") +
  scale_colour_manual(values = c("black", "black"), name = "モデル") +
  labs(y = "緩和策によるBaUからの生産量変化 [%]") +
  theme_2 +
  theme(axis.text.x = element_text(size = 10), legend.text = element_text(size = 10), axis.title.y = element_text(size = 14), legend.title = element_text(size = 10), legend.title.position = "top", legend.justification = c(1, 1))
g_presentation <- g_presentation1 - g_presentation2
ggsave("../output/presentation3_2.png", plot = g_presentation, width = 10, height = 6)
