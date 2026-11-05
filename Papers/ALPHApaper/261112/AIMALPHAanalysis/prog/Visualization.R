library(tidyverse)
library(gdxrrw)
library(ggplot2)
library(ggpmisc)
library(purrr)
library(furrr)
library(patchwork)
library(scales)
library(sf)
library(car)
library(MASS)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggh4x)
sf_use_s2(FALSE)
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
    text = element_text(size = 12), axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
    legend.position = "bottom",
    axis.title.x = element_blank(),
    legend.title = element_blank(),
    strip.background = element_blank(),
    strip.clip = "off"
  )
theme_3 <- theme_bw() +
  theme(
    text = element_text(size = 18), axis.text.x = element_text(size = 12),
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
theme_6 <- theme_bw() +
  theme(
    panel.background = element_rect(fill = "grey90", color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks  = element_blank(),
    axis.title  = element_blank()
  )
Model_color <- c(
  "AIM-ALPHA" = "navy",
  "GCAM4" = "#6a6a6a",
  "MESSAGE-GLOBIOM" = "#6a6a6a",
  "AIM/CGE" = "#6a6a6a",
  "IMAGE" = "#6a6a6a",
  "REMIND-MAGPIE" = "#6a6a6a"
)
Model_color2 <- c(
  "AIM-ALPHA" = "navy",
  "GCAM4" = "#CC5CA9",
  "MESSAGE-GLOBIOM" = "#CC5CA9",
  "AIM/CGE" = "#CC5CA9",
  "IMAGE" = "#CC5CA9",
  "REMIND-MAGPIE" = "#CC5CA9"
)
Model_line <- c(
  "AIM-ALPHA" = 1.3,
  "GCAM4" = 1,
  "MESSAGE-GLOBIOM" = 1,
  "AIM/CGE" = 1,
  "IMAGE" = 1,
  "REMIND-MAGPIE" = 1
)
IAM_shape <-  c(
  "GCAM4" = 16, "MESSAGE-GLOBIOM" = 17, "AIM/CGE" = 15,
  "IMAGE" = 18, "REMIND-MAGPIE" = 3
)
model_shape <- c(
  "FAOSTAT" = 1,
  "FAOSTAT_old" = 0,
  "LUH2" = 2,
  "EDGAR" = 4
)
Scenario_color <- c(
  "SSP1" = "#80E67E",
  "SSP2" = "#1226ff",
  "SSP3" = "#c40a0a",
  "SSP4" = "#d8a417",
  "SSP5" = "#ab2cdd",
  "FAOSTAT" = "#6a6a6a"
)
AgMIP_color <- c(
  "AIM" = "#007a70",
  "AIM-ALPHA" = "#D74322",
  "ENVISAGE" = "#a3d9f2",
  "FARM" = "#26c3ec",
  "MAgPIE" = "#002923",
  "GLOBIOM" = "#8CA231",
  "FAOSTAT" = "#888888",
  "LUH2" = "#707070"
)
scenario_color <- c(
  "SSP2-BaU" = "#3873AD",
  "SSP2-Mitigation" = "#C3327A"
)
scenario_linetype <- c(
  "SSP2-BaU" = "dotdash",
  "SSP2-Mitigation" = "solid"
)
model_shape <- c(
  "FAOSTAT" = 1,
  "LUH2" = 2,
  "EDGAR" = 4
)
land_color <- c(
  "Cropland" = "#FF8C94",
  "Forest" = "#9ECC5C",
  "Pasture" = "#5CBBCC",
  "Other natural land" = "#FFCC77",
  "Non arable land" = "#9e9e9e",
  "Energy Crop" = "#CC5CA9"
)
hist_fill <- c(
  "FAOSTAT"="#1F77B4FF","EDGAR"="#FF7F0EFF","PRIMAPhist"="#2CA02CFF","Gasser et al 2020"="#D62728FF",
  "FAO_EmisLUC"="#9467BDFF","LUH3"="#E377C2FF","LUH2_GCB2025"="#7F7F7FFF","LUH2"="#BCBD22FF"
)
projection_linetype <- c(
  "Backcasting" = "22"
)
aim_linetype <- c(
  projection_linetype,
  "AIM-ALPHA (projection)" = "solid"
)
backcast_color <- "#4D4D4D"
region_AIM <- c(
  "BRA", "CAN", "CHN", "IND", "JPN", "TUR", "USA", "XE25", "XER", "XOC", "XSE", "XSA", "XLM", "CIS", "XME", "XNF", "XAF", "World"
)
region_R5 <- c(
  "R5ASIA", "R5REF", "R5OECD90+EU", "R5MAF", "R5LAM", "World"
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
Boundary <- st_read("../data/Boundary/WB_countries_Admin0_10m.shp")
Boundary$ISO_A3[21] <- "FRA"
Boundary$ISO_A3[51] <- "NOR"
Boundary$ISO_A3[63] <- "XKX"
Boundary <- Boundary %>%
  dplyr::select(WB_NAME, ISO_A3) %>%
  rename("cty" = "ISO_A3")
Basin_Boundary <- st_read(
  dsn = "../data/Basin_Boundary/Basin_Boundary.shp",
  options = "ENCODING=UTF-8",
  quiet = FALSE
)
AgLU_IAMC <- rgdx.param("../data/IAMC_all_scenario.gdx", "IAMC_Template")
FAOSTAT <- rgdx.param("../data/IAMC_historical.gdx", "IAMC_Template")
LUH2 <- rgdx.param("../data/IAMC_historical.gdx", "IAMC_Template_LUH2")
EDGAR <- rgdx.param("../data/IAMC_historical.gdx", "IAMC_Template_EDGAR")
SSP_database <- read_csv("../data/SSP_IAM_V2_201811.csv")
AgLU_AGMIP <- rgdx.param("../data/AGMIP_all_scenario.gdx", "AGMIP")
AGMIP_agg <- rgdx.param("../data/AGMIP_all_scenario.gdx", "AGMIP_agg")
Prc_Car <- rgdx.param("../data/data.gdx", "Prc_Car")
GDP_agg <- rgdx.param("../data/data_agg.gdx", "GDP_agg")
POP_agg <- rgdx.param("../data/data_agg.gdx", "Pop_agg")
Cal <- rgdx.param("../data/all_scenario_World.gdx", "Cal_pc_load")
QH <- rgdx.param("../data/all_scenario_World.gdx", "QH_load")
Yld <- rgdx.param("../data/all_scenario_World.gdx", "Yld_load")
PP <- rgdx.param("../data/all_scenario_World.gdx", "PP_load")
QM <- rgdx.param("../data/all_scenario_World.gdx", "QM_load")
QSUP <- rgdx.param("../data/all_scenario_World.gdx", "QSUP_load")
QLTot <- rgdx.param("../data/all_scenario_World.gdx", "QLTot_load")
QLCro <- rgdx.param("../data/all_scenario_World.gdx", "QLCro_load")
QLPas <- rgdx.param("../data/all_scenario_World.gdx", "QLPas_load")
QLFrs <- rgdx.param("../data/all_scenario_World.gdx", "QLFrsTot_load")
QL <- rgdx.param("../data/all_scenario_World.gdx", "QL_load")
QL2 <- rgdx.param("../data/all_scenario_World.gdx", "QL2_load")
QSUP_agg <- rgdx.param("../data/all_scenario_global_17.gdx", "QSUP_agg_load")
PC_agg <- rgdx.param("../data/all_scenario_global_17.gdx", "PC_agg_load")
QH_hist <- rgdx.param("../data/data.gdx", "QH")
Yld_hist <- rgdx.param("../data/data.gdx", "Yld")
QL_hist <- rgdx.param("../data/data.gdx", "QL")
QSUP_hist <- rgdx.param("../data/data.gdx", "QSUP")
PP_hist <- rgdx.param("../data/data.gdx", "PP")
QM_hist <- rgdx.param("../data/data.gdx", "QM")
QHbase <- rgdx.param("../data/baseyear.gdx", "QHbase")
Yldbase <- rgdx.param("../data/baseyear.gdx", "Yldbase")
QLbase <- rgdx.param("../data/baseyear.gdx", "QLbase")
QSUPbase <- rgdx.param("../data/baseyear.gdx", "QSUPbase")
PPbase <- rgdx.param("../data/baseyear.gdx", "PPbase")
QMbase <- rgdx.param("../data/baseyear.gdx", "QMbase")
c_list <- rgdx.set("../data/all_scenario_World.gdx", "c", te = T) %>%
  mutate(com = as.character(i)) %>%
  mutate(name = as.character(.te)) %>%
  dplyr::select(com, name)
region <- read.csv("../data/country_mapping_R5.csv")
region_world <- region %>% mutate(region = "World")
region_world <- full_join(region, region_world)
# plot ------------------------------------------------------------------

Fig_dir <- "../output/plot/"
if (!dir.exists(Fig_dir)) dir.create(Fig_dir, recursive = TRUE)
Table_dir <- "../output/"
if (!dir.exists(Table_dir)) dir.create(Table_dir, recursive = TRUE)

# Fig.5 Population & GDP ----------------------------------------------
Fig5_pop <- POP_agg %>%
  mutate(yr = as.integer(as.character(yr)),sc = factor(sc, levels = names(Scenario_color))) %>%
  filter(region == "World", yr >= 2015)
Fig5_gdp <- GDP_agg %>%
  mutate(yr = as.integer(as.character(yr)),
         sc = factor(sc, levels = names(Scenario_color)),
         GDP_agg = GDP_agg / 1000) %>%
  filter(region == "World", yr >= 2015)

pop_decade <- dplyr::filter(Fig5_pop, yr %% 10 == 0)
gdp_decade <- dplyr::filter(Fig5_gdp, yr %% 10 == 0)

sc_levels <- union(unique(Fig5_pop$sc), unique(Fig5_gdp$sc)) %>% as.character()
sc_levels <- intersect(sc_levels, names(Scenario_color))
shape_map <- setNames(c(16, 17, 15, 18, 3, 4)[seq_along(sc_levels)], sc_levels)
x_breaks <- seq(2020, 2100, by = 20)
legend_guide <- guides(
  color = guide_legend(title = NULL, ncol = 1, byrow = TRUE,
                       override.aes = list(alpha = 1, linewidth = 0.9, size = 2.2)),
  shape = guide_legend(title = NULL, ncol = 1, byrow = TRUE,
                       override.aes = list(alpha = 1, linewidth = 0.9, size = 2.2))
)

p_fig5_pop <- ggplot(Fig5_pop, aes(x = yr, y = Pop_agg, color = sc, group = sc)) +
  geom_line(linewidth = 0.9, alpha = 0.7, na.rm = TRUE) +
  geom_point(data = pop_decade, aes(shape = sc), size = 2.0, stroke = 0.6,
             alpha = 0.95, na.rm = TRUE) +
  scale_color_manual(values = Scenario_color[sc_levels], breaks = sc_levels, name = NULL) +
  scale_shape_manual(values = shape_map, breaks = sc_levels, name = NULL) +
  scale_x_continuous(breaks = x_breaks, expand = expansion(mult = c(0.01, 0.02))) +
  scale_y_continuous(labels = label_comma(), limits = c(0, NA)) +
  labs(x = "Year", y = "Population [million]") +
  theme_bw(base_size = 12) +
  theme(legend.position = "bottom",
        legend.direction = "vertical",
        plot.title = element_text(face = "bold", hjust = 0)) +
  legend_guide
p_fig5_gdp <- ggplot(Fig5_gdp, aes(x = yr, y = GDP_agg, color = sc, group = sc)) +
  geom_line(linewidth = 0.9, alpha = 0.7, na.rm = TRUE) +
  geom_point(data = gdp_decade, aes(shape = sc), size = 2.0, stroke = 0.6,
             alpha = 0.95, na.rm = TRUE) +
  scale_color_manual(values = Scenario_color[sc_levels], breaks = sc_levels, name = NULL) +
  scale_shape_manual(values = shape_map, breaks = sc_levels, name = NULL) +
  scale_x_continuous(breaks = x_breaks, expand = expansion(mult = c(0.01, 0.02))) +
  scale_y_continuous(labels = label_comma(), limits = c(0, NA)) +
  labs(x = "Year", y = "GDP [trillion US$2017]") +
  theme_bw(base_size = 12) +
  theme(legend.position = "bottom",
        legend.direction = "vertical",
        plot.title = element_text(face = "bold", hjust = 0)) +
  legend_guide
p_fig5 <- (p_fig5_pop | p_fig5_gdp) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")")
ggsave(paste0(Fig_dir, "Fig5.png"), plot = p_fig5, width = 12, height = 5, dpi = 300)


# Fig.6 Carbon price ----------------------------------------------
Fig6_data <- Prc_Car %>%
  mutate(yr = as.integer(as.character(yr))) %>%
  filter(region == "World", cp == "500C")
x_breaks <- seq(2020, 2100, by = 20)
p_fig6 <- ggplot(Fig6_data, aes(x = yr, y = Prc_Car)) +
  geom_line(linewidth = 1.0, na.rm = TRUE, color = "navy") +
  scale_x_continuous(limits = c(2015, 2100), breaks = x_breaks, expand = expansion(mult = c(0, 0.02))) +
  scale_y_continuous(labels = label_comma()) +
  labs(x = "Year", y = expression("Carbon Price [US$/tCO"[2]*"]")) +
  theme_bw(base_size = 12) +
  theme(
    legend.position = "bottom",
    legend.direction = "vertical",
    plot.title = element_text(face = "bold", hjust = 0)
  )
ggsave(paste0(Fig_dir, "Fig6.png"), plot = p_fig6, width = 6, height = 5, dpi = 300)


# Fig.7 ------------------------------------------
SSP_agr <- SSP_database %>%
  filter(VARIABLE %in% c("Agricultural Demand|Crops", "Agricultural Demand|Livestock", "Agricultural Production|Crops|Non-Energy", 
                         "Agricultural Production|Livestock", "Land Cover|Cropland", "Land Cover|Forest", "Land Cover|Pasture", 
                         "Emissions|CH4|Land Use", "Emissions|N2O|Land Use")) %>%
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
SSP_agr <- SSP_agr %>% pivot_longer(cols = -c(MODEL, region, VARIABLE), names_to = "Y", values_to = "AGMIP_agg") %>%
  filter(Y %in% seq(2010,2100,10))
SSP_agr_range <- SSP_agr %>% pivot_wider(names_from = "MODEL", values_from = "AGMIP_agg")
SSP_agr_range$IAM_max <- apply(SSP_agr_range[, 4:9], 1, max, na.rm = T)
SSP_agr_range$IAM_min <- apply(SSP_agr_range[, 4:9], 1, min, na.rm = T)
SSP_agr_range <- SSP_agr_range %>%
  dplyr::select(VARIABLE, region, Y, IAM_max, IAM_min) %>%
  na.omit() 

FAOSTAT_data <- FAOSTAT %>%
  mutate(
    VARIABLE = as.character(IAMC_VAR),
    region = as.character(IAMC_REGION),
    Y = as.numeric(as.character(yr)),
    Model2 = "FAOSTAT",
    Historical = IAMC_Template
  ) %>%
  dplyr::select(!c(IAMC_VAR, IAMC_REGION, yr, IAMC_Template))
LUH2_data <- LUH2 %>%
  mutate(
    VARIABLE = as.character(IAMC_VAR),
    region = as.character(IAMC_REGION),
    Y = as.numeric(as.character(yr)),
    Model2 = "LUH2",
    Historical = IAMC_Template_LUH2
  ) %>%
  dplyr::select(!c(IAMC_VAR, IAMC_REGION, yr, IAMC_Template_LUH2))
EDGAR_data <- EDGAR %>%
  mutate(
    VARIABLE = as.character(IAMC_VAR),
    region = as.character(IAMC_REGION),
    Y = as.numeric(as.character(yr)),
    Model2 = "EDGAR",
    Historical = IAMC_Template_EDGAR
  ) %>%
  dplyr::select(!c(IAMC_VAR, IAMC_REGION, yr, IAMC_Template_EDGAR)) %>%
  mutate(VARIABLE = dplyr::recode(VARIABLE,
                                  "Emi_CH4_AFO_Agr" = "Emi_CH4_AFO",
                                  "Emi_N2O_AFO_Agr" = "Emi_N2O_AFO"
  ))
Historical <- full_join(FAOSTAT_data, LUH2_data)
Historical <- full_join(Historical, EDGAR_data) %>%
  filter(Y > 2000, Y < 2025, region %in% c("World", "R5ASIA", "R5LAM", "R5MAF", "R5OECD90+EU", "R5REF")) %>%
  mutate(VARIABLE = dplyr::recode(VARIABLE,
                                  "Agr_Dem_Cro" = "CONS_CRP",
                                  "Agr_Dem_Liv" = "CONS_LSP",
                                  "Agr_Pro_Cro" = "PROD_CRP",
                                  "Agr_Pro_Liv" = "PROD_LSP",
                                  "Lan_Cov_Cro" = "LAND_CRP",
                                  "Lan_Cov_Frs" = "LAND_FOR",
                                  "Lan_Cov_Pst" = "LAND_GRS",
                                  "Lan_Cov_Oth_Nat" = "LAND_ONV",
                                  "Emi_CH4_AFO" = "ECH4_TOT",
                                  "Emi_N2O_AFO" = "EN2O_TOT"
  )) %>%
  mutate(Y = as.factor(Y))
data_7 <- AGMIP_agg %>%
  filter(SCENARIO == "SSP2_BaU_NoCC") %>%
  filter(region %in% c("World", "R5ASIA", "R5LAM", "R5MAF", "R5OECD90+EU", "R5REF")) %>%
  filter(!AgMIP_UNIT %in% c("USD", "million kcal", "1000 t")) %>%
  filter(AgMIP_VAR %in% c("LAND", "PROD", "CONS", "ECH4", "EN2O")) %>%
  mutate(AGMIP_agg = AGMIP_agg / 1000) %>%
  unite(VARIABLE, AgMIP_VAR, ITEM, remove = T, sep = "_") %>%
  mutate(VARIABLE = toupper(VARIABLE)) %>%
  dplyr::select(!c(SCENARIO, AgMIP_UNIT)) %>%
  mutate(MODEL = "AIM-ALPHA")
data_7$AGMIP_agg[data_7$VARIABLE == "ECH4_TOT"] <- data_7$AGMIP_agg[data_7$VARIABLE == "ECH4_TOT"] / 28 * 1000
data_7$AGMIP_agg[data_7$VARIABLE == "EN2O_TOT"] <- data_7$AGMIP_agg[data_7$VARIABLE == "EN2O_TOT"] / 265 * 1000 * 1000
data_7 <- full_join(data_7, SSP_agr)
data_7 <- full_join(data_7, SSP_agr_range)
data_7 <- full_join(data_7, Historical) %>%
  mutate(Y = as.numeric(Y)) %>%
  mutate(VARIABLE = dplyr::recode(VARIABLE,
                                  "CONS_CRP" = "Crop Consumption [million t-DM]",
                                  "CONS_LSP" = "Livestock Consumption [million t-DM]",
                                  "PROD_CRP" = "Crop Production [million t-DM]",
                                  "PROD_LSP" = "Livestock Production [million t-DM]",
                                  "LAND_CRP" = "Cropland [million ha]",
                                  "LAND_FOR" = "Forest [million ha]",
                                  "LAND_GRS" = "Pasture [million ha]",
                                  "LAND_ONV" = "Other natural land [million ha]",
                                  "ECH4_TOT" = "CH4 emission [Mt CH4/yr]",
                                  "EN2O_TOT" = "N2O emission [kt N2O/yr]"
  ))
VAR_list <- c("Crop Consumption [million t-DM]", "Crop Production [million t-DM]",
              "Livestock Consumption [million t-DM]", "Livestock Production [million t-DM]",
              "Cropland [million ha]", "Pasture [million ha]", "Forest [million ha]", "Other natural land [million ha]",
              "CH4 emission [Mt CH4/yr]", "N2O emission [kt N2O/yr]")
data_7 <- data_7 %>%
  filter(VARIABLE %in% VAR_list) %>%
  mutate(VARIABLE = factor(VARIABLE, levels = VAR_list)) %>%
  mutate(region = dplyr::recode(region,
                                "R5ASIA" = "Asia",
                                "R5LAM" = "Latin America and the Caribbean",
                                "R5MAF" = "Middle East and Africa",
                                "R5OECD90+EU" = "OECD+EU",
                                "R5REF" = "Eastern Europe and the Former Soviet Union"
  ))
data_7_r <- data_7 %>%
  filter(region == "World") %>%
  dplyr::select(MODEL,VARIABLE, region, Y, AGMIP_agg) %>%
  na.omit()
data_7_r_hist <- data_7 %>%
  filter(region == "World") %>%
  dplyr::select(Model2, VARIABLE, region, Y, Historical) %>%
  na.omit()
data_7_r_IAM <- data_7 %>%
  filter(region == "World") %>%
  mutate(Y = as.numeric(as.character(Y))) %>%
  dplyr::select(VARIABLE, region, Y, IAM_max, IAM_min) %>%
  na.omit()
data_line_others <- dplyr::filter(data_7_r, MODEL != "AIM-ALPHA")
data_line_aim    <- dplyr::filter(data_7_r, MODEL == "AIM-ALPHA")
data_line_aim_backcast <- dplyr::filter(data_line_aim, Y <= 2015)
data_line_aim_future <- dplyr::filter(data_line_aim, Y >= 2015)
model_breaks <- setdiff(names(Model_color), "AIM-ALPHA")
g7 <- ggplot() +
  geom_line(
    data = data_line_others,
    aes(x = Y, y = AGMIP_agg, color = MODEL, linewidth = MODEL),
    alpha = 0.4, show.legend = FALSE
  ) +
  geom_ribbon(
    data = data_7_r_IAM,
    aes(x = Y, ymin = IAM_min, ymax = IAM_max),
    fill = "#BBBBBB", alpha = 0.4, show.legend = FALSE
  ) +
  geom_point(
    data = subset(data_7_r, MODEL != "AIM-ALPHA"),
    aes(x = Y, y = AGMIP_agg, color = MODEL, shape = MODEL),
    size = 2.6, alpha = 0.4
  ) +
  geom_line(
    data = data_7_r_hist,
    aes(x = Y, y = Historical, group = Model2),
    size = 0.9, linetype = "dotdash", show.legend = FALSE
  ) +
  geom_ribbon(
    data = data_7_r_hist,
    aes(x = Y, ymin = Historical*0.9, ymax = Historical*1.1, fill = Model2),
    alpha = 0.5
  ) +
  geom_line(
    data = data_line_aim_backcast,
    aes(x = Y, y = AGMIP_agg, linetype = "Backcasting"),
    color = backcast_color,
    linewidth = 1.1,
    show.legend = TRUE
  ) +
  geom_line(
    data = data_line_aim_future,
    aes(x = Y, y = AGMIP_agg, color = MODEL, linewidth = MODEL, linetype = "AIM-ALPHA (projection)"),
    show.legend = TRUE
  ) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 2015, linetype = "dotted") +
  scale_fill_manual(
    values = c(hist_fill),
    breaks = c(names(hist_fill)),
    name = "Historical"
  ) +
  scale_color_manual(values = Model_color, breaks = model_breaks, name = "Models") +
  scale_shape_manual(
    values = IAM_shape,
    breaks = model_breaks, name = "Models", drop = TRUE
  ) +
  scale_linewidth_manual(values = Model_line, guide = "none") +
  scale_linetype_manual(
    values = aim_linetype,
    breaks = names(aim_linetype),
    name = "AIM-ALPHA"
  ) +
  
  facet_wrap(vars(VARIABLE), scales = "free_y", ncol = 4) +
  scale_x_continuous(breaks = seq(2000, 2100, 20)) +
  scale_y_continuous(limits = c(0, NA)) +
  labs(x = "Year", y = "") +
  theme_1 +
  theme(
    axis.title.y = element_blank(),
    # legend.box = "vertical"
    legend.box = "horizontal",
    legend.position = "inside",
    legend.position.inside = c(0.76, 0.16),
    legend.justification.inside = c(0.5, 0.5),
    legend.box.just = "top"
  ) +
  guides(
    color = guide_legend(
      order = 2, ncol = 1,
      override.aes = list(linetype = 0, linewidth = 0, alpha = 1)
    ),
    shape = guide_legend(order = 2, ncol = 1),
    fill  = guide_legend(order = 3, override.aes = list(alpha = 0.3)),
    linetype = guide_legend(
      order = 1,
      override.aes = list(color = c(backcast_color, Model_color["AIM-ALPHA"]), linewidth = 1.2, alpha = 1)
    )
  )
ggsave(paste0(Fig_dir, "Fig7.png"), g7, width = 19, height = 10, dpi = 300)

print("Fig7:")
increase_summary <- data_7_r %>%
  filter(Y %in% c(2015, 2100)) %>%
  group_by(VARIABLE) %>%
  summarise(
    AGMIP_2015 = AGMIP_agg[Y == 2015][1],
    AGMIP_2100 = AGMIP_agg[Y == 2100][1],
    Increase = AGMIP_2100 - AGMIP_2015,
    GrowthRate = (AGMIP_2100 / AGMIP_2015 - 1) * 100
  )
print(increase_summary)


# Fig.8 ----------------------------------------------------
SSP_agr <- SSP_database %>%
  filter(VARIABLE %in% c("Agricultural Demand|Crops", "Agricultural Demand|Livestock", "Agricultural Production|Crops|Non-Energy", 
                         "Agricultural Production|Livestock", "Land Cover|Cropland", "Land Cover|Forest", "Land Cover|Pasture", 
                         "Emissions|CH4|Land Use", "Emissions|N2O|Land Use")) %>%
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
data8 <- AGMIP_agg %>%
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
data8$AGMIP_agg[data8$VARIABLE == "ECH4_TOT"] <- data8$AGMIP_agg[data8$VARIABLE == "ECH4_TOT"] / 28 * 1000
data8$AGMIP_agg[data8$VARIABLE == "EN2O_TOT"] <- data8$AGMIP_agg[data8$VARIABLE == "EN2O_TOT"] / 265 * 1000 * 1000

data8 <- full_join(SSP_agr, data8) %>%
  mutate(Y = as.numeric(Y)) %>%
  mutate(VARIABLE = dplyr::recode(VARIABLE,
                                  "CONS_CRP" = "Crop Consumption [million t-DM]",
                                  "CONS_LSP" = "Livestock Consumption [million t-DM]",
                                  "PROD_CRP" = "Crop Production [million t-DM]",
                                  "PROD_LSP" = "Livestock Production [million t-DM]",
                                  "LAND_CRP" = "Cropland [million ha]",
                                  "LAND_FOR" = "Forest [million ha]",
                                  "LAND_GRS" = "Pasture [million ha]",
                                  "LAND_ONV" = "Other natural land [million ha]",
                                  "ECH4_TOT" = "CH4 emission [Mt CH4/yr]",
                                  "EN2O_TOT" = "N2O emission [kt N2O/yr]"
  ))
VAR_list <- c("Crop Consumption [million t-DM]", "Crop Production [million t-DM]",
              "Livestock Consumption [million t-DM]", "Livestock Production [million t-DM]",
              "Cropland [million ha]", "Pasture [million ha]", "Forest [million ha]", "Other natural land [million ha]",
              "CH4 emission [Mt CH4/yr]", "N2O emission [kt N2O/yr]")
VAR_label <- c("Crop \nConsumption \n[million t-DM]", "Crop \nProduction \n[million t-DM]",
               "Livestock \nConsumption \n[million t-DM]", "Livestock \nProduction \n[million t-DM]",
               "Cropland \n[million ha]", "Pasture \n[million ha]", "Forest \n[million ha]", "Other natural \nland \n[million ha]",
               "CH4 emission \n[Mt CH4/yr]", "N2O emission \n[kt N2O/yr]")
data8 <- data8 %>%
  filter(VARIABLE %in% VAR_list) %>%
  mutate(VARIABLE = factor(VARIABLE, levels = VAR_list)) %>%
  mutate(region = dplyr::recode(region,
                                "R5ASIA" = "Asia",
                                "R5LAM" = "Latin America and the Caribbean",
                                "R5MAF" = "Middle East and Africa",
                                "R5OECD90+EU" = "OECD+EU",
                                "R5REF" = "Eastern Europe and the Former Soviet Union"
  ))
region_list <- unique(data8$region)
levels(data8$VARIABLE) <- VAR_label
data8 <- data8 %>% pivot_wider(names_from = "MODEL", values_from = "AGMIP_agg")
data8$IAM_max <- apply(data8[, 5:9], 1, max, na.rm = T)
data8$IAM_min <- apply(data8[, 5:9], 1, min, na.rm = T)
data8_AgLU <- data8 %>%
  dplyr::select(SCENARIO, VARIABLE, region, Y, AgLU) %>%
  na.omit()
data8_IAM <- data8 %>%
  dplyr::select(SCENARIO, VARIABLE, region, Y, IAM_max, IAM_min) %>%
  na.omit() %>%
  filter(is.finite(IAM_max), is.finite(IAM_min)) %>%
  filter(Y %in% seq(2010, 2100, 10))
data8_FAOSTAT <- FAOSTAT %>%
  mutate(
    VARIABLE = as.character(IAMC_VAR),
    region = as.character(IAMC_REGION),
    Y = as.numeric(as.character(yr)),
    Model2 = "FAOSTAT",
    Historical = IAMC_Template
  ) %>%
  dplyr::select(!c(IAMC_VAR, IAMC_REGION, yr, IAMC_Template))
data8_LUH2 <- LUH2 %>%
  mutate(
    VARIABLE = as.character(IAMC_VAR),
    region = as.character(IAMC_REGION),
    Y = as.numeric(as.character(yr)),
    Model2 = "LUH2",
    Historical = IAMC_Template_LUH2
  ) %>%
  dplyr::select(!c(IAMC_VAR, IAMC_REGION, yr, IAMC_Template_LUH2))
data8_EDGAR <- EDGAR %>%
  mutate(
    VARIABLE = as.character(IAMC_VAR),
    region = as.character(IAMC_REGION),
    Y = as.numeric(as.character(yr)),
    Model2 = "EDGAR",
    Historical = IAMC_Template_EDGAR
  ) %>%
  dplyr::select(!c(IAMC_VAR, IAMC_REGION, yr, IAMC_Template_EDGAR)) %>%
  mutate(VARIABLE = dplyr::recode(VARIABLE,
                                  "Emi_CH4_AFO_Agr" = "Emi_CH4_AFO",
                                  "Emi_N2O_AFO_Agr" = "Emi_N2O_AFO"
  ))
data8_Historical <- full_join(data8_FAOSTAT, data8_LUH2)
data8_Historical <- full_join(data8_Historical, data8_EDGAR) %>%
  filter(Y > 2000, Y < 2025, region %in% c("World", "R5ASIA", "R5LAM", "R5MAF", "R5OECD90+EU", "R5REF")) %>%
  mutate(VARIABLE = dplyr::recode(VARIABLE,
                                  "Agr_Dem_Cro" = "Crop Consumption [million t-DM]",
                                  "Agr_Dem_Liv" = "Livestock Consumption [million t-DM]",
                                  "Agr_Pro_Cro" = "Crop Production [million t-DM]",
                                  "Agr_Pro_Liv" = "Livestock Production [million t-DM]",
                                  "Lan_Cov_Cro" = "Cropland [million ha]",
                                  "Lan_Cov_Frs" = "Forest [million ha]",
                                  "Lan_Cov_Pst" = "Pasture [million ha]",
                                  "Lan_Cov_Oth_Nat" = "Other natural land [million ha]",
                                  "Emi_CH4_AFO" = "CH4 emission [Mt CH4/yr]",
                                  "Emi_N2O_AFO" = "N2O emission [kt N2O/yr]"
  )) %>%
  filter(VARIABLE %in% VAR_list) %>%
  mutate(VARIABLE = factor(VARIABLE, levels = VAR_list)) %>%
  mutate(region = dplyr::recode(region,
                                "R5ASIA" = "Asia",
                                "R5LAM" = "Latin America and the Caribbean",
                                "R5MAF" = "Middle East and Africa",
                                "R5OECD90+EU" = "OECD+EU",
                                "R5REF" = "Eastern Europe and the Former Soviet Union"
  ))
levels(data8_Historical$VARIABLE) <- VAR_label

r <- "World"
data8_AgLU_r <- data8_AgLU %>% filter(region == r)
data8_IAM_r <- data8_IAM %>% filter(region == r)
data8_Historical_r <- data8_Historical %>% filter(region == r)
data8_AgLU_r_backcast <- data8_AgLU_r %>% filter(Y <= 2015)
data8_AgLU_r_future <- data8_AgLU_r %>% filter(Y >= 2015)
g8_1 <- data8_AgLU_r %>%
  ggplot() +
  geom_line(
    data = data8_AgLU_r_backcast,
    aes(x = Y, y = AgLU, linetype = "Backcasting", group = SCENARIO),
    color = backcast_color, linewidth = 0.9, show.legend = FALSE
  ) +
  geom_line(
    data = data8_AgLU_r_future,
    aes(x = Y, y = AgLU, color = SCENARIO, group = SCENARIO),
    linewidth = 1, show.legend = FALSE
  ) +
  facet_wrap(vars(VARIABLE), scales = "free_y", nrow = 10) +
  scale_color_manual(values = Scenario_color) +
  scale_linetype_manual(values = projection_linetype) +
  scale_x_continuous(breaks = seq(2000, 2100, 20), limits = c(2000, 2100)) +
  labs(x = "Year", y = "") +
  theme_3 +
  theme(legend.position = "none", axis.title.y = element_blank(), strip.text = element_blank(), panel.spacing.y = unit(14, "pt"), axis.text.x = element_text(size = 14, angle = 45, hjust = 1))
g8_2 <- ggplot() +
  geom_line(
    data = data8_AgLU_r_backcast,
    aes(x = Y, y = AgLU, linetype = "Backcasting", group = SCENARIO),
    color = backcast_color, linewidth = 1.05, show.legend = c(linetype = TRUE, color = FALSE, shape = FALSE)
  ) +
  geom_line(
    data = data8_AgLU_r_future,
    aes(x = Y, y = AgLU, color = SCENARIO, group = SCENARIO),
    linewidth = 1.2, show.legend = c(color = TRUE, linetype = FALSE, shape = FALSE)
  ) +
  geom_ribbon(data = data8_IAM_r, aes(x = Y, ymax = IAM_max, ymin = IAM_min), fill = "grey50", alpha = 0.2) +
  geom_line(
    data = data8_Historical_r,
    aes(x = Y, y = Historical, group = Model2),
    linewidth = 0.6, linetype = "dotdash", show.legend = FALSE
  ) +
  geom_ribbon(
    data = data8_Historical_r,
    aes(x = Y, ymin = Historical * 0.9, ymax = Historical * 1.1, fill = Model2),
    alpha = 0.4
  ) +
  facet_grid(VARIABLE ~ SCENARIO, scales = "free") +
  scale_color_manual(values = Scenario_color) +
  scale_fill_manual(
    values = c(hist_fill),
    breaks = c(names(hist_fill)),
    name = "Historical"
  ) +
  scale_linetype_manual(
    values = projection_linetype,
    breaks = names(projection_linetype),
    name = "AIM-ALPHA"
  ) +
  scale_x_continuous(breaks = seq(2000, 2100, 20), limits = c(2000, 2100)) +
  labs(x = "Year", y = "") +
  theme_1 +
  theme(axis.title.y = element_blank(), panel.spacing.y = unit(14, "pt"), axis.text.x = element_text(size = 14, angle = 45, hjust = 1)) +
  guides(
    color = guide_legend(
      order = 2,
      override.aes = list(shape = NA, linetype = "solid", linewidth = 1.2)
    ),
    linetype = guide_legend(
      order = 1,
      override.aes = list(color = backcast_color, linewidth = 1.05, shape = NA)
    ),
    fill = guide_legend(
      order = 3,
      override.aes = list(color = NA, linetype = 0, linewidth = 0, alpha = 0.4)
    )
  )
g_fig8 <- g8_1 + g8_2 + plot_layout(widths = c(1, 4))
ggsave(paste0(Fig_dir, "Fig8.png"), g_fig8, width = 24, height = 18.5, dpi=300)

print("Fig8:")
increase_summary <- data8_AgLU %>%
  filter(Y %in% c(2015, 2100)) %>%
  dplyr::select(-region) %>%
  filter(SCENARIO == "SSP3") %>%
  group_by(VARIABLE) %>%
  summarise(
    AGMIP_2015 = AgLU[Y == 2015][1],
    AGMIP_2100 = AgLU[Y == 2100][1],
    Increase = AGMIP_2100 - AGMIP_2015,
    GrowthRate = (AGMIP_2100 / AGMIP_2015 - 1) * 100
  )
print(increase_summary)


# Fig.9 --------------------------------------------------- 
SSP_agr <- SSP_database %>%
  filter(VARIABLE %in% c("Agricultural Demand|Crops", "Agricultural Demand|Livestock", "Agricultural Production|Crops|Non-Energy", 
                         "Agricultural Production|Livestock", "Land Cover|Cropland", "Land Cover|Forest", "Land Cover|Pasture", 
                         "Emissions|CH4|Land Use", "Emissions|N2O|Land Use")) %>%
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
SSP_agr <- SSP_agr %>% pivot_longer(cols = -c(MODEL, region, VARIABLE), names_to = "Y", values_to = "AGMIP_agg") %>%
  filter(Y %in% seq(2010,2100,10))
SSP_agr_range <- SSP_agr %>% pivot_wider(names_from = "MODEL", values_from = "AGMIP_agg")
SSP_agr_range$IAM_max <- apply(SSP_agr_range[, 4:9], 1, max, na.rm = T)
SSP_agr_range$IAM_min <- apply(SSP_agr_range[, 4:9], 1, min, na.rm = T)
SSP_agr_range <- SSP_agr_range %>%
  dplyr::select(VARIABLE, region, Y, IAM_max, IAM_min) %>%
  na.omit() 

FAOSTAT_9 <- FAOSTAT %>%
  mutate(
    VARIABLE = as.character(IAMC_VAR),
    region = as.character(IAMC_REGION),
    Y = as.numeric(as.character(yr)),
    Model2 = "FAOSTAT",
    Historical = IAMC_Template
  ) %>%
  dplyr::select(!c(IAMC_VAR, IAMC_REGION, yr, IAMC_Template))
LUH2_9 <- LUH2 %>%
  mutate(
    VARIABLE = as.character(IAMC_VAR),
    region = as.character(IAMC_REGION),
    Y = as.numeric(as.character(yr)),
    Model2 = "LUH2",
    Historical = IAMC_Template_LUH2
  ) %>%
  dplyr::select(!c(IAMC_VAR, IAMC_REGION, yr, IAMC_Template_LUH2))
EDGAR_9 <- EDGAR %>%
  mutate(
    VARIABLE = as.character(IAMC_VAR),
    region = as.character(IAMC_REGION),
    Y = as.numeric(as.character(yr)),
    Model2 = "EDGAR",
    Historical = IAMC_Template_EDGAR
  ) %>%
  dplyr::select(!c(IAMC_VAR, IAMC_REGION, yr, IAMC_Template_EDGAR)) %>%
  mutate(VARIABLE = dplyr::recode(VARIABLE,
                                  "Emi_CH4_AFO_Agr" = "Emi_CH4_AFO",
                                  "Emi_N2O_AFO_Agr" = "Emi_N2O_AFO"
  ))
# EDGAR_9$Historical[EDGAR_9$VARIABLE == "Emi_CH4_AFO"] <- EDGAR_9$Historical[EDGAR_9$VARIABLE == "Emi_CH4_AFO"] / 28 * 1000
# EDGAR_9$Historical[EDGAR_9$VARIABLE == "Emi_N2O_AFO"] <- EDGAR_9$Historical[EDGAR_9$VARIABLE == "Emi_N2O_AFO"] / 265 * 1000 * 1000
Historical <- full_join(FAOSTAT_9, LUH2_9)
Historical <- full_join(Historical, EDGAR_9) %>%
  filter(Y >= 2000, Y <= 2025, region %in% c("World", "R5ASIA", "R5LAM", "R5MAF", "R5OECD90+EU", "R5REF")) %>%
  # filter(Y %in% c(seq(2000,2020,5),2022,2023), region %in% c("World", "R5ASIA", "R5LAM", "R5MAF", "R5OECD90+EU", "R5REF")) %>%
  mutate(VARIABLE = dplyr::recode(VARIABLE,
                                  "Agr_Dem_Cro" = "CONS_CRP",
                                  "Agr_Dem_Liv" = "CONS_LSP",
                                  "Agr_Pro_Cro" = "PROD_CRP",
                                  "Agr_Pro_Liv" = "PROD_LSP",
                                  "Lan_Cov_Cro" = "LAND_CRP",
                                  "Lan_Cov_Frs" = "LAND_FOR",
                                  "Lan_Cov_Pst" = "LAND_GRS",
                                  "Lan_Cov_Oth_Nat" = "LAND_ONV",
                                  "Emi_CH4_AFO" = "ECH4_TOT",
                                  "Emi_N2O_AFO" = "EN2O_TOT"
  )) %>%
  mutate(Y = as.factor(Y))
data_9 <- AGMIP_agg %>%
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
  mutate(MODEL = "AIM-ALPHA")
data_9$AGMIP_agg[data_9$VARIABLE == "ECH4_TOT"] <- data_9$AGMIP_agg[data_9$VARIABLE == "ECH4_TOT"] / 28 * 1000
data_9$AGMIP_agg[data_9$VARIABLE == "EN2O_TOT"] <- data_9$AGMIP_agg[data_9$VARIABLE == "EN2O_TOT"] / 265 * 1000 * 1000
data_9 <- full_join(data_9, SSP_agr)
data_9 <- full_join(data_9, SSP_agr_range)
data_9 <- full_join(data_9, Historical) %>%
  mutate(Y = as.numeric(Y)) %>%
  mutate(VARIABLE = dplyr::recode(VARIABLE,
                                  "CONS_CRP" = "Crop Consumption [million t-DM]",
                                  "CONS_LSP" = "Livestock Consumption [million t-DM]",
                                  "PROD_CRP" = "Crop Production [million t-DM]",
                                  "PROD_LSP" = "Livestock Production [million t-DM]",
                                  "LAND_CRP" = "Cropland [million ha]",
                                  "LAND_FOR" = "Forest [million ha]",
                                  "LAND_GRS" = "Pasture [million ha]",
                                  "LAND_ONV" = "Other natural land [million ha]",
                                  "ECH4_TOT" = "CH4 emission [Mt CH4/yr]",
                                  "EN2O_TOT" = "N2O emission [kt N2O/yr]"
  ))
VAR_list <- c("Crop Consumption [million t-DM]", "Crop Production [million t-DM]",
              "Livestock Consumption [million t-DM]", "Livestock Production [million t-DM]",
              "Cropland [million ha]", "Pasture [million ha]", "Forest [million ha]", "Other natural land [million ha]",
              "CH4 emission [Mt CH4/yr]", "N2O emission [kt N2O/yr]")
VAR_label <- c("Crop \nConsumption \n[million \nt-DM]", "Crop \nProduction \n[million \nt-DM]",
               "Livestock \nConsumption \n[million \nt-DM]", "Livestock \nProduction \n[million \nt-DM]",
               "Cropland \n[million ha]", "Pasture \n[million ha]", "Forest \n[million ha]", "Other \nnatural land \n[million ha]",
               "CH4 emission \n[Mt CH4/yr]", "N2O emission \n[kt N2O/yr]")

data_9 <- data_9 %>%
  filter(VARIABLE %in% VAR_list) %>%
  mutate(VARIABLE = factor(VARIABLE, levels = VAR_list)) %>%
  mutate(region = dplyr::recode(region,
                                "R5ASIA" = "Asia",
                                "R5LAM" = "Latin America",
                                "R5MAF" = "Middle East & Africa",
                                "R5OECD90+EU" = "OECD & EU",
                                "R5REF" = "Reforming Economies"),
         region = factor(
           region,
           levels = c(
             "Asia",
             "Latin America",
             "Middle East & Africa",
             "OECD & EU",
             "Reforming Economies"
           ),
           labels = c(
             "Asia",
             "Latin America",
             "Middle East &\nAfrica",
             "OECD & EU",
             "Reforming\nEconomies"
           )
         ),
         SCENARIO = factor(SCENARIO, levels = names(Scenario_color))
  ) %>%
  filter(Y <= 2030)%>%
  dplyr::filter(region != "World")

levels(data_9$VARIABLE) <- VAR_label

region_list <- unique(data_9$region)
data_9_r <- data_9  %>%
  dplyr::select(MODEL, VARIABLE, SCENARIO, region, Y, AGMIP_agg) %>%
  tidyr::drop_na()

data_9_r_hist <- data_9 %>%
  dplyr::select(Model2, VARIABLE, region, Y, Historical) %>%
  tidyr::drop_na()

data_line_aim <- dplyr::filter(data_9_r, MODEL == "AIM-ALPHA")
data_line_aim_backcast <- dplyr::filter(data_line_aim, Y <= 2015)
data_line_aim_future <- dplyr::filter(data_line_aim, Y >= 2015)
model_breaks <- setdiff(names(Model_color), "AIM-ALPHA")

g_9 <- ggplot() +
  geom_line(
    data = data_9_r_hist,
    aes(x = Y, y = Historical, group = Model2),
    size = 0.6, linetype = "dotdash", show.legend = FALSE
  ) +
  geom_ribbon(
    data = data_9_r_hist,
    aes(x = Y, ymin = Historical * 0.9, ymax = Historical * 1.1, fill = Model2),
    alpha = 0.4,
    show.legend = c(fill = TRUE, color = FALSE, linetype = FALSE)
  ) +
  geom_line(
    data = data_line_aim_backcast,
    aes(x = Y, y = AGMIP_agg, group = SCENARIO, linetype = "Backcasting"),
    color = backcast_color,
    linewidth = 1.05,
    alpha = 0.7,
    show.legend = c(linetype = TRUE, color = FALSE, fill = FALSE)
  ) +
  geom_line(
    data = data_line_aim_future,
    aes(x = Y, y = AGMIP_agg, color = SCENARIO, group = SCENARIO, linewidth = MODEL),
    alpha = 0.7,
    show.legend = c(color = TRUE, linetype = FALSE, fill = FALSE)
  ) +
  geom_vline(xintercept = 2015, linetype = "dotted") +
  scale_fill_manual(
    values = c(hist_fill),
    breaks = c(names(hist_fill)),
    name = "Historical"
  ) +
  scale_color_manual(
    values = Scenario_color,
    breaks = intersect(levels(data_9$SCENARIO), names(Scenario_color)),
    name = "Scenario"
  ) +
  scale_linewidth_manual(values = Model_line, guide = "none") +
  scale_linetype_manual(
    values = projection_linetype,
    breaks = names(projection_linetype),
    name = "AIM-ALPHA"
  ) +
  ggh4x::facet_grid2(
    rows = vars(VARIABLE),
    cols = vars(region),
    scales = "free", 
    independent = "y"
  ) +
  scale_x_continuous(breaks = seq(2000, 2030, 10), minor_breaks = seq(2000, 2030, 5), limits = c(2000, 2030)) +
  scale_y_continuous(limits = NULL, expand = expansion(mult = c(0.2, 0.2))) +
  labs(x = "Year", y = "") +
  theme_1 +
  theme(
    axis.text.y = element_text(size = 12),
    strip.text.y = element_text(size = 13, lineheight = 0.95)
  ) +
  guides(
    fill = guide_legend(
      order = 3,
      override.aes = list(color = NA, linetype = 0, linewidth = 0, alpha = 0.4)
    ),
    color = guide_legend(
      order = 2,
      override.aes = list(shape = NA, linetype = "solid", linewidth = 1.05, alpha = 1)
    ),
    linetype = guide_legend(
      order = 1,
      override.aes = list(color = backcast_color, linewidth = 1.05, shape = NA, alpha = 1)
    )
  )

ggsave(paste0(Fig_dir, "Fig9.png"), g_9, width = 16, height = 14.5, dpi = 300)


# Fig.10 --------------------------------------------------- 
data_10_1 <- full_join(QH, Yld)
data_10_1 <- full_join(data_10_1, QL)
data_10_1 <- full_join(data_10_1, QSUP)
data_10_1 <- full_join(data_10_1, PP)
data_10_1 <- full_join(data_10_1, QM) %>%
  filter(SCENARIO == "SSP2_BaU_NoCC") %>%
  filter(!c %in% c("fat", "sfd", "mis")) %>% # exclude exogenous commodity
  dplyr::select(!SCENARIO) %>%
  mutate(var = "est")
colnames(data_10_1) <- c("c", "cty", "yr", "QH", "Yld", "QL", "QSUP", "PP", "QM", "var")
data_10_2 <- full_join(QH_hist, Yld_hist)
data_10_2 <- full_join(data_10_2, QL_hist)
data_10_2 <- full_join(data_10_2, QSUP_hist)
data_10_2 <- full_join(data_10_2, PP_hist)
data_10_2 <- full_join(data_10_2, QM_hist) %>%
  filter(!c %in% c("fat", "sfd", "mis")) %>% # exclude exogenous commodity
  mutate(var = "hist")
data_10 <- full_join(data_10_1, data_10_2) %>%
  filter(yr %in% c("2015", "2020"))
data_10 <- data_10 %>%
  pivot_longer(cols = c("QH", "Yld", "QL", "QSUP", "PP", "QM"), names_to = "variable", values_to = "value") %>%
  pivot_wider(names_from = "var", values_from = "value") %>%
  filter(c != "total") %>%
  na.omit() %>%
  # filter(!variable %in% c("QM","PP")) %>%
  mutate(hist = log10(hist), est = log10(est)) %>%
  mutate(upper = hist + 0.3, lower = hist - 0.3) %>%
  mutate(upper2 = hist + 1, lower2 = hist - 1) %>%
  mutate(variable = dplyr::recode(variable,
                                  "QH" = "Food Consumption Volume",
                                  "QSUP" = "Supply Volume",
                                  "QL" = "Harvested Area",
                                  "Yld" = "Yield",
                                  "PP" = "Producer Price",
                                  "QM" = "Import Volume"
  ))  %>%
  mutate(variable = factor(variable, levels = c("Food Consumption Volume", "Supply Volume", "Import Volume","Producer Price","Harvested Area","Yield"))) %>%
  mutate("scope" = "0.5×–2× range", "scope2" = "0.1×–10× range")
g_10 <- data_10 %>%
  ggplot() +
  geom_ribbon(aes(x = hist, ymin = lower2, ymax = upper2, fill = scope2), alpha = 0.3) +
  geom_ribbon(aes(x = hist, ymin = lower, ymax = upper, fill = scope), alpha = 0.9) +
  geom_abline(intercept = 0, slope = 1) +
  geom_abline(intercept = -0.3, slope = 1, linetype = "dotdash") +
  geom_abline(intercept = 0.3, slope = 1, linetype = "dotdash") +
  geom_abline(intercept = -1, slope = 1, linetype = "dotted") +
  geom_abline(intercept = 1, slope = 1, linetype = "dotted") +
  geom_point(aes(x = hist, y = est), size = 1, color = "#C3327A") +
  scale_fill_manual(values = c("0.5×–2× range" = "lightblue", "0.1×–10× range" = "lightgreen")) +
  facet_wrap(vars(yr, variable), scales = "free", nrow = 2) +
  labs(x = "log(Historical)", y = "log(Estimated)") +
  theme_1 +
  theme(axis.title.x = element_text(size = 20))

thr_05_2 <- log10(2) 
thr_01_10 <- 1
data_10 <- data_10 %>%
  mutate(
    diff = est - hist,
    in_05_2  = abs(diff) <= thr_05_2,
    in_01_10 = abs(diff) <= thr_01_10
  )
overall_stat <- data_10 %>%
  summarise(
    N = n(),
    within_05_2  = sum(in_05_2),
    within_01_10 = sum(in_01_10),
    pct_05_2  = 100 * within_05_2  / N,
    pct_01_10 = 100 * within_01_10 / N
  )
byfacet_stat <- data_10 %>%
  group_by(yr, variable) %>%
  summarise(
    N = n(),
    within_05_2  = sum(in_05_2),
    within_01_10 = sum(in_01_10),
    pct_05_2  = 100 * within_05_2  / N,
    pct_01_10 = 100 * within_01_10 / N,
    .groups = "drop"
  )
labels_df <- byfacet_stat %>%
  mutate(label = sprintf("0.5×–2×: %.1f%%\n0.1×–10×: %.1f%%", pct_05_2, pct_01_10))

g_10 <- g_10 +
  geom_text(
    data = labels_df,
    aes(x = -Inf, y = Inf, label = label),
    hjust = -0.2, vjust = 1.3, size = 4, inherit.aes = FALSE
  )
ggsave(paste0(Fig_dir, "Fig10.png"), plot = g_10, width = 18, height = 10)

fig10_var_labels <- c(
  "QH" = "Food Consumption Volume",
  "QSUP" = "Supply Volume",
  "QM" = "Import Volume",
  "PP" = "Producer Price",
  "QL" = "Harvested Area",
  "Yld" = "Yield"
)
fig10_var_levels <- unname(fig10_var_labels)
fig10_scope <- c("0.5-2 range" = "lightblue", "0.1-10 range" = "lightgreen")
make_fig10_data <- function(est_data, ref_data, yr_filter = NULL) {
  data <- full_join(est_data, ref_data)
  if (!is.null(yr_filter)) {
    data <- data %>% filter(yr %in% yr_filter)
  }
  data %>%
    pivot_longer(cols = c("QH", "Yld", "QL", "QSUP", "PP", "QM"), names_to = "variable", values_to = "value") %>%
    pivot_wider(names_from = "var", values_from = "value") %>%
    filter(c != "total") %>%
    filter(is.finite(ref), is.finite(est), ref > 0, est > 0) %>%
    mutate(ref = log10(ref), est = log10(est)) %>%
    mutate(upper = ref + log10(2), lower = ref - log10(2)) %>%
    mutate(upper2 = ref + 1, lower2 = ref - 1) %>%
    mutate(variable = dplyr::recode(variable, !!!fig10_var_labels)) %>%
    mutate(variable = factor(variable, levels = fig10_var_levels)) %>%
    mutate(scope = "0.5-2 range", scope2 = "0.1-10 range")
}
make_fig10_plot <- function(data, facet_vars, nrow, x_label, y_label) {
  labels_df <- data %>%
    mutate(
      diff = est - ref,
      in_05_2 = abs(diff) <= log10(2),
      in_01_10 = abs(diff) <= 1
    ) %>%
    group_by(across(all_of(facet_vars))) %>%
    summarise(
      N = n(),
      pct_05_2 = 100 * sum(in_05_2) / N,
      pct_01_10 = 100 * sum(in_01_10) / N,
      .groups = "drop"
    ) %>%
    mutate(label = sprintf("0.5-2: %.1f%%\n0.1-10: %.1f%%", pct_05_2, pct_01_10))
  
  ggplot(data) +
    geom_ribbon(aes(x = ref, ymin = lower2, ymax = upper2, fill = scope2), alpha = 0.3) +
    geom_ribbon(aes(x = ref, ymin = lower, ymax = upper, fill = scope), alpha = 0.9) +
    geom_abline(intercept = 0, slope = 1) +
    geom_abline(intercept = -log10(2), slope = 1, linetype = "dotdash") +
    geom_abline(intercept = log10(2), slope = 1, linetype = "dotdash") +
    geom_abline(intercept = -1, slope = 1, linetype = "dotted") +
    geom_abline(intercept = 1, slope = 1, linetype = "dotted") +
    geom_point(aes(x = ref, y = est), size = 1, color = "#C3327A") +
    geom_text(
      data = labels_df,
      aes(x = -Inf, y = Inf, label = label),
      hjust = -0.2, vjust = 1.3, size = 3.3, inherit.aes = FALSE
    ) +
    scale_fill_manual(values = fig10_scope) +
    facet_wrap(vars(!!!syms(facet_vars)), scales = "free", nrow = nrow) +
    labs(x = x_label, y = y_label) +
    theme_1 +
    theme(axis.title.x = element_text(size = 20))
}

data_10b_est <- data_10_1 %>%
  mutate(var = "est")
data_10b_ref <- data_10_2 %>%
  rename(ref_yr = yr) %>%
  rename(yr = ref_yr) %>%
  mutate(var = "ref")
data_10b <- make_fig10_data(data_10b_est, data_10b_ref, yr_filter = c("2015", "2020"))

data_10a_est <- data_10b_est %>%
  filter(yr == "2015") %>%
  mutate(yr = "Base year")
QLbase_cty <- QLbase %>%
  group_by(a, cty) %>%
  summarise(QLbase = sum(QLbase, na.rm = TRUE), .groups = "drop")
Yldbase_cty <- QSUPbase %>%
  left_join(QLbase_cty, by = c("a", "cty")) %>%
  mutate(Yldbase = QSUPbase / QLbase) %>%
  dplyr::select(a, cty, Yldbase)
data_10a_ref <- QHbase %>%
  full_join(Yldbase_cty %>% rename(c = a, Yld = Yldbase), by = c("c", "cty")) %>%
  full_join(QLbase_cty %>% rename(c = a, QL = QLbase), by = c("c", "cty")) %>%
  full_join(QSUPbase %>% rename(c = a, QSUP = QSUPbase), by = c("c", "cty")) %>%
  full_join(PPbase %>% rename(c = a, PP = PPbase), by = c("c", "cty")) %>%
  full_join(QMbase %>% rename(QM = QMbase), by = c("c", "cty")) %>%
  filter(!c %in% c("fat", "sfd", "mis")) %>%
  mutate(yr = "Base year", var = "ref") %>%
  dplyr::select(c, cty, yr, QH = QHbase, Yld, QL, QSUP, PP, QM, var)
data_10a <- make_fig10_data(data_10a_est, data_10a_ref)

g_10a <- make_fig10_plot(
  data_10a,
  facet_vars = c("variable"),
  nrow = 1,
  x_label = "Log10(harmonized calibration data)",
  y_label = "Log10(benchmark replication)"
)
g_10b <- make_fig10_plot(
  data_10b,
  facet_vars = c("yr", "variable"),
  nrow = 2,
  x_label = "Log10(raw FAOSTAT)",
  y_label = "Log10(model estimate)"
)
g_10 <- g_10a / g_10b +
  plot_layout(heights = c(0.75, 1.8)) +
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")")
ggsave(paste0(Fig_dir, "Fig10.png"), plot = g_10, width = 18, height = 14)


# Fig.11 --------------------------------------------------- 
SSP_agr <- SSP_database %>%
  filter(VARIABLE %in% c("Agricultural Demand|Crops", "Agricultural Demand|Livestock", "Agricultural Production|Crops|Non-Energy", 
                         "Agricultural Production|Livestock", "Land Cover|Cropland", "Land Cover|Forest", "Land Cover|Pasture", 
                         "Emissions|CH4|Land Use", "Emissions|N2O|Land Use")) %>%
  filter(MODEL != "AIM/CGE" | VARIABLE != "Agricultural Demand|Crops") %>%
  filter(SCENARIO == "SSP2-19") %>%
  mutate(region = dplyr::recode(REGION,
                                "R5.2ASIA" = "R5ASIA",
                                "R5.2LAM" = "R5LAM",
                                "R5.2MAF" = "R5MAF",
                                "R5.2OECD" = "R5OECD90+EU",
                                "R5.2REF" = "R5REF"
  )) %>%
  mutate(VARIABLE = dplyr::recode(VARIABLE,
                                  "Agricultural Demand|Crops" = "AGR_DEM_CRO",
                                  "Agricultural Demand|Livestock" = "AGR_DEM_LIV",
                                  "Agricultural Production|Crops|Non-Energy" = "AGR_PRO_CRO",
                                  "Agricultural Production|Livestock" = "AGR_PRO_LIV",
                                  "Land Cover|Cropland" = "LAN_COV_CRO",
                                  "Land Cover|Forest" = "LAN_COV_FRS",
                                  "Land Cover|Pasture" = "LAN_COV_PST",
                                  "Emissions|CH4|Land Use" = "EMI_CH4_AFO",
                                  "Emissions|N2O|Land Use" = "EMI_N2O_AFO"
  )) %>%
  dplyr::select(!c(SCENARIO, REGION, UNIT)) %>%
  dplyr::select(!"2005")
SSP_agr <- SSP_agr %>% pivot_longer(cols = -c(MODEL, region, VARIABLE), names_to = "Y", values_to = "AGMIP_agg") %>%
  filter(Y %in% seq(2010,2100,10)) %>% mutate(Y = as.numeric(Y)) %>% dplyr::rename(variable=VARIABLE,Model=MODEL,Value=AGMIP_agg)
SSP_agr_range <- SSP_agr %>% pivot_wider(names_from = "Model", values_from = "Value")
SSP_agr_range$IAM_max <- apply(SSP_agr_range[, 4:7], 1, max, na.rm = T)
SSP_agr_range$IAM_min <- apply(SSP_agr_range[, 4:7], 1, min, na.rm = T)
SSP_agr_range <- SSP_agr_range %>%
  dplyr::select(variable, region, Y, IAM_max, IAM_min) %>%
  na.omit() 

data_11_IAMC <- AgLU_IAMC %>%
  filter(SCENARIO %in% c("SSP2_BaU_NoCC", "SSP2_500C_NoCC")) %>%
  mutate(Y = as.character(Y)) %>%
  mutate(Y = as.numeric(Y)) %>%
  mutate(SCENARIO = dplyr::recode(SCENARIO,
                                  "SSP2_500C_NoCC" = "SSP2-Mitigation",
                                  "SSP2_BaU_NoCC" = "SSP2-BaU"
  )) %>%
  rename("AIM-ALPHA" = "IAMC_Template", "variable" = "IAMC_VAR", "region" = "IAMC_REGION") %>%
  mutate(variable = toupper(variable))
data_11_FAOSTAT <- FAOSTAT %>%
  mutate(
    variable = as.character(IAMC_VAR),
    region = as.character(IAMC_REGION),
    Y = as.numeric(as.character(yr)),
    Model2 = "FAOSTAT",
    Historical = IAMC_Template
  ) %>%
  mutate(variable = toupper(variable)) %>%
  dplyr::select(!c(IAMC_VAR, IAMC_REGION, yr, IAMC_Template))
data_11_LUH2 <- LUH2 %>%
  mutate(
    variable = as.character(IAMC_VAR),
    region = as.character(IAMC_REGION),
    Y = as.numeric(as.character(yr)),
    Model2 = "LUH2",
    Historical = IAMC_Template_LUH2
  ) %>%
  mutate(variable = toupper(variable)) %>%
  dplyr::select(!c(IAMC_VAR, IAMC_REGION, yr, IAMC_Template_LUH2))
data_11_EDGAR <- EDGAR %>%
  mutate(
    variable = as.character(IAMC_VAR),
    region = as.character(IAMC_REGION),
    Y = as.numeric(as.character(yr)),
    Model2 = "EDGAR",
    Historical = IAMC_Template_EDGAR
  ) %>%
  mutate(variable = toupper(variable)) %>%
  dplyr::select(!c(IAMC_VAR, IAMC_REGION, yr, IAMC_Template_EDGAR)) %>%
  mutate(variable = dplyr::recode(variable,
                                  "EMI_CH4_AFO_AGR" = "EMI_CH4_AFO",
                                  "EMI_N2O_AFO_AGR" = "EMI_N2O_AFO"
  ))
data_11_Historical <- full_join(data_11_FAOSTAT, data_11_LUH2)
data_11_Historical <- full_join(data_11_Historical, data_11_EDGAR) %>%
  filter(Y > 2000, Y < 2021, region %in% region_AIM)
data_11_IAMC <- data_11_IAMC %>%
  pivot_longer(cols = -c(SCENARIO, region, variable, Y), names_to = "Model", values_to = "Value") %>%
  na.omit() %>%
  filter(Y %in% seq(2000, 2100, 5))  %>% mutate(Y = as.numeric(Y))
data_11_IAMC <- full_join(data_11_IAMC, SSP_agr)
data_11_IAMC <- full_join(data_11_IAMC, SSP_agr_range)
data_11_IAMC <- full_join(data_11_IAMC, data_11_Historical)
data_11 <- data_11_IAMC %>%
  filter(region == "World") %>%
  filter(variable %in% c("AGR_DEM_CRO", "AGR_DEM_LIV", "AGR_PRO_CRO", "AGR_PRO_LIV", "LAN_COV_CRO", "LAN_COV_PST", "LAN_COV_FRS", "LAN_COV_OTH_NAT", "EMI_CH4_AFO", "EMI_N2O_AFO")) %>%
  mutate(variable = dplyr::recode(variable,
                                  "AGR_DEM_CRO" = "Crop Consumption [million t-DM]",
                                  "AGR_DEM_LIV" = "Livestock Consumption [million t-DM]",
                                  "AGR_PRO_CRO" = "Crop Production [million t-DM]",
                                  "AGR_PRO_LIV" = "Livestock Production [million t-DM]",
                                  "LAN_COV_CRO" = "Cropland [million ha]",
                                  "LAN_COV_FRS" = "Forest [million ha]",
                                  "LAN_COV_PST" = "Pasture [million ha]",
                                  "LAN_COV_OTH_NAT" = "Other natural land [million ha]",
                                  "EMI_CH4_AFO" = "CH4 emission [Mt CH4/yr]",
                                  "EMI_N2O_AFO" = "N2O emission [kt N2O/yr]"
  )) %>%
  mutate(variable = factor(variable, levels = c("Crop Consumption [million t-DM]", "Crop Production [million t-DM]",
                                                "Livestock Consumption [million t-DM]", "Livestock Production [million t-DM]",
                                                "Cropland [million ha]", "Pasture [million ha]", "Forest [million ha]", "Other natural land [million ha]",
                                                "CH4 emission [Mt CH4/yr]", "N2O emission [kt N2O/yr]")))
data_line_aim <- data_11 %>%
  filter(Model == "AIM-ALPHA") %>%
  dplyr::select(SCENARIO, variable, region, Y, Value) %>%
  na.omit() %>%
  filter(SCENARIO %in% c("SSP2-BaU", "SSP2-Mitigation")) %>%
  filter(Y >= 2015) %>%
  mutate(SCENARIO = factor(SCENARIO, levels = c("SSP2-BaU", "SSP2-Mitigation")))
data_11_hist <- data_11 %>%
  dplyr::select(variable, region, Y, Model2, Historical) %>%
  na.omit()
data_11_r_IAM <- data_11 %>%
  mutate(Y = as.numeric(as.character(Y))) %>%
  dplyr::select(variable, region, Y, IAM_max, IAM_min) %>%
  na.omit()
model_palette <- tibble::tibble(
  Model = names(Model_color2),
  col   = unname(Model_color2)
)
data_line_others <- data_11 %>%
  filter(Model != "AIM-ALPHA") %>%
  dplyr::select(Model, variable, region, Y, Value) %>%
  na.omit() %>%
  left_join(model_palette, by = "Model")

model_breaks <- setdiff(names(Model_color), "AIM-ALPHA")
scenario_breaks <- names(scenario_color)
g_11 <- ggplot() +
  geom_line(
    data = data_line_others,
    aes(x = Y, y = Value, color = I(col), linewidth = Model),
    alpha = 0.2, show.legend = FALSE
  ) +
  geom_point(
    data = data_line_others,
    aes(x = Y, y = Value, color = I(col), shape = Model),
    size = 2.6, alpha = 0.3, show.legend = TRUE
  ) +
  geom_ribbon(
    data = data_11_r_IAM,
    aes(x = Y, ymin = IAM_min, ymax = IAM_max),
    fill="#CC5CA9",alpha = 0.1, show.legend = FALSE
  ) +
  geom_line(
    data = data_11_hist,
    aes(x = Y, y = Historical, group = Model2),
    size = 0.9, linetype = "dotdash", show.legend = FALSE
  ) +
  geom_ribbon(
    data = data_11_hist,
    aes(x = Y, ymin = Historical*0.9, ymax = Historical*1.1, fill = Model2),
    alpha = 0.5
  ) +
  geom_line(
    data = data_line_aim,
    aes(x = Y, y = Value, color = SCENARIO, linetype = SCENARIO),
    linewidth = 1.3, show.legend = TRUE
  ) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 2015, linetype = "dotted") +
  scale_color_manual(values = scenario_color, name = "Scenario") +
  scale_linetype_manual(values = scenario_linetype, name = "Scenario") +
  scale_shape_manual(values = IAM_shape, breaks = model_breaks, name = "Models", drop = TRUE) +
  scale_fill_manual(
    values = c(hist_fill),
    breaks = c(names(hist_fill)),
    name   = "Historical"
  ) +
  scale_linewidth_manual(values = Model_line, guide = "none") +
  
  facet_wrap(vars(variable), scales = "free_y", ncol = 4) +
  scale_x_continuous(breaks = seq(2000, 2100, 20)) +
  labs(x = "Year", y = "") +
  theme_1 +
  theme(
    axis.title.y  = element_blank(),
    legend.box    = "horizontal",
    legend.position = "inside",
    legend.position.inside = c(0.76, 0.16),
    legend.justification.inside = c(0.5, 0.5),
    legend.box.just = "top"
  ) +
  guides(
    color = guide_legend(
      order = 1, ncol = 1,
      override.aes = list(shape = NA, linewidth = 1.3, alpha = 1)
    ),
    linetype = guide_legend(
      order = 1, ncol = 1,
      override.aes = list(shape = NA, linewidth = 1.3, alpha = 1)
    ),
    shape = guide_legend(
      order = 2, ncol = 1,
      override.aes = list(linetype = 0, linewidth = 0, alpha = 1)
    ),
    fill = guide_legend(
      order = 3,
      override.aes = list(
        shape = NA,        
        colour = NA,       
        linetype = 0,      
        linewidth = 0,
        alpha = 0.3
      )
    )
  )
ggsave(paste0(Fig_dir, "Fig11.png"),  g_11, width = 19, height = 10, dpi=300)

print("Fig11:")
increase_summary <- data_line_aim %>%
  filter(Y %in% c(2015, 2100), SCENARIO == "SSP2-Mitigation") %>%
  dplyr::select(variable,Y,Value) %>%
  group_by(variable) %>%
  summarise(
    AGMIP_2015 = Value[Y == 2015][1],
    AGMIP_2100 = Value[Y == 2100][1],
    Increase = AGMIP_2100 - AGMIP_2015,
    GrowthRate = (AGMIP_2100 / AGMIP_2015 - 1) * 100
  )
print(increase_summary)

increase_summary2 <- data_line_aim %>%
  filter(Y == 2100) %>%
  dplyr::select(SCENARIO, variable,Y,Value) %>%
  group_by(variable) %>%
  summarise(
    AGMIP_BaU = Value[SCENARIO == "SSP2-BaU"][1],
    AGMIP_Miti = Value[SCENARIO == "SSP2-Mitigation"][1],
    Increase = AGMIP_Miti - AGMIP_BaU,
    GrowthRate = (AGMIP_Miti / AGMIP_BaU - 1) * 100
  )
print(increase_summary2)


# Fig.12 ----------------------------
sc_map <- tibble::tibble(
  old = c("SSP2_BaU_NoCC", "SSP2_500C_NoCC"),
  new = c("SSP2-BaU",      "SSP2-Mitigation")
)
c_list_QSUP <- c_list %>%
  filter(!com %in% c("vol","alc","dai","swt","fat","sfd","mis")) %>%
  mutate(name = sub("energr", "energy", name)) %>%
  mutate(name = sub("corn", "maize", name)) %>%
  mutate(name = sub(" \\(crop\\)$", "", name)) %>%
  mutate(name = sub(" \\(livestock\\)$", "", name))

QSUP_agg_data <- QSUP_agg %>%
  mutate(SCENARIO = as.character(SCENARIO)) %>%
  semi_join(sc_map, by = c("SCENARIO" = "old")) %>%
  left_join(sc_map, by = c("SCENARIO" = "old")) %>%
  mutate(SCENARIO = new) %>%
  dplyr::select(-new)
QSUP_agg_data <- QSUP_agg_data %>%
  filter(c %in% c_list_QSUP$com) %>%
  semi_join(c_list_QSUP, by = c("c" = "com")) %>%
  left_join(c_list_QSUP, by = c("c" = "com")) %>%
  mutate(Y = as.numeric(as.character(Y)),
         name = factor(name, levels = c("energy crops","wheat","rice","maize","strachy roots","other cereals","sugar crops","pulses",
                                        "nuts","oil crops","vegetables","fruits","stimulants","spices","cattle meat",
                                        "small ruminant meat","poultry meat","other meat","milk","egg")),
         QSUP_agg_load = QSUP_agg_load/1000) %>%
  filter(!is.na(QSUP_agg_load), !is.na(name))

Land_agg_data <- AGMIP_agg %>%
  filter(AgMIP_VAR == "LAND") %>%
  mutate(SCENARIO = as.character(SCENARIO)) %>%
  semi_join(sc_map, by = c("SCENARIO" = "old")) %>%
  left_join(sc_map, by = c("SCENARIO" = "old")) %>%
  mutate(SCENARIO = new) %>%
  dplyr::select(-new) %>%
  pivot_wider(names_from = "ITEM", values_from = "AGMIP_agg")
Land_agg_data[is.na(Land_agg_data)] <- 0
Land_agg_data <- Land_agg_data %>%
  mutate(CRP_NE = CRP - ECP) %>%
  dplyr::select(SCENARIO, region, AgMIP_VAR, AgMIP_UNIT, Y, NLD, CRP_NE, ECP, GRS, ONV, FOR) %>%
  pivot_longer(cols = c(NLD, CRP_NE, ECP, GRS, ONV, FOR), names_to = "ITEM", values_to = "AGMIP_agg") %>%
  mutate(ITEM = dplyr::recode(ITEM,
                       "NLD" = "Non arable land",
                       "CRP_NE" = "Cropland",
                       "ECP" = "Energy Crop",
                       "GRS" = "Pasture",
                       "ONV" = "Other natural land",
                       "FOR" = "Forest"
  )) %>%
  mutate(ITEM = factor(ITEM, levels = c("Cropland", "Energy Crop", "Pasture", "Forest", "Other natural land", "Non arable land")))  %>%
  mutate(Y = as.numeric(as.character(Y)), AGMIP_agg = AGMIP_agg / 1000)

g_QSUP <- QSUP_agg_data %>%
  filter(region == "World") %>%
  ggplot() +
  geom_area(aes(x = Y, y = QSUP_agg_load, fill = name), stat = "identity") +
  scale_x_continuous(breaks = seq(2020, 2100, 20)) +
  scale_fill_manual(values = item_color) +
  labs(x = "Year", y = "Production [million t]") +
  facet_wrap(vars(SCENARIO)) +
  theme_3 + 
  theme(legend.position = "right")
g_Land <- Land_agg_data %>%
  filter(region == "World") %>%
  ggplot() +
  geom_area(aes(x = Y, y = AGMIP_agg, fill = ITEM), stat = "identity") +
  scale_x_continuous(breaks = seq(2020, 2100, 20)) +
  scale_fill_manual(values = land_color) +
  labs(x = "Year", y = "Land Area [million ha]") +
  facet_wrap(vars(SCENARIO)) +
  theme_3 + 
  theme(legend.position = "right")
g_QSUP_Land <- g_QSUP / g_Land +
  plot_layout(heights = c(1, 1), guides = "keep") +
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")")
ggsave(paste0(Fig_dir, "Fig12.png"), g_QSUP_Land, width = 14, height = 12, dpi=300)

print("Fig12:")
increase_summary <- QSUP_agg_data %>%
  filter(Y %in% c(2015, 2100), SCENARIO == "SSP2-Mitigation", region=="World") %>%
  dplyr::select(c,Y,QSUP_agg_load) %>%
  group_by(c) %>%
  summarise(
    AGMIP_2015 = QSUP_agg_load[Y == 2015][1],
    AGMIP_2100 = QSUP_agg_load[Y == 2100][1],
    Increase = AGMIP_2100 - AGMIP_2015,
    GrowthRate = (AGMIP_2100 / AGMIP_2015 - 1) * 100
  )
print(increase_summary)


extract_unit <- function(x) {
  unit <- stringr::str_match(as.character(x), "\\[(.*)\\]")[, 2]
  dplyr::if_else(is.na(unit), "", unit)
}

clean_indicator <- function(x) {
  stringr::str_squish(stringr::str_remove(as.character(x), "\\s*\\[.*\\]"))
}

make_change_rows <- function(data, figure, comparison, scenario, indicator_col, value_col) {
  data %>%
    filter(Y %in% c(2015, 2100)) %>%
    group_by({{ indicator_col }}) %>%
    summarise(
      value_2015 = {{ value_col }}[Y == 2015][1],
      value_2100 = {{ value_col }}[Y == 2100][1],
      change = value_2100 - value_2015,
      percent_change = (value_2100 / value_2015 - 1) * 100,
      .groups = "drop"
    ) %>%
    rename(indicator_raw = {{ indicator_col }}) %>%
    mutate(
      figure = figure,
      comparison = comparison,
      scenario = scenario,
      indicator = clean_indicator(indicator_raw),
      unit = extract_unit(indicator_raw)
    ) %>%
    dplyr::select(figure, comparison, scenario, indicator, unit, value_2015, value_2100, change, percent_change)
}

fig7_key_numbers <- make_change_rows(
  data_7_r,
  "Fig7",
  "Change from 2015 to 2100",
  "SSP2-BaU",
  VARIABLE,
  AGMIP_agg
)

fig8_key_numbers <- data8_AgLU_r %>%
  filter(Y %in% c(2015, 2100)) %>%
  group_by(SCENARIO, VARIABLE) %>%
  summarise(
    value_2015 = AgLU[Y == 2015][1],
    value_2100 = AgLU[Y == 2100][1],
    change = value_2100 - value_2015,
    percent_change = (value_2100 / value_2015 - 1) * 100,
    .groups = "drop"
  ) %>%
  mutate(
    figure = "Fig8",
    comparison = "Change from 2015 to 2100",
    scenario = as.character(SCENARIO),
    indicator = clean_indicator(VARIABLE),
    unit = extract_unit(VARIABLE)
  ) %>%
  dplyr::select(figure, comparison, scenario, indicator, unit, value_2015, value_2100, change, percent_change)

fig11_mitigation_change <- make_change_rows(
  data_line_aim %>% filter(SCENARIO == "SSP2-Mitigation"),
  "Fig11",
  "Change from 2015 to 2100",
  "SSP2-Mitigation",
  variable,
  Value
)

fig11_mitigation_vs_bau <- data_line_aim %>%
  filter(Y == 2100) %>%
  dplyr::select(SCENARIO, variable, Value) %>%
  tidyr::pivot_wider(names_from = SCENARIO, values_from = Value) %>%
  mutate(
    figure = "Fig11",
    comparison = "Mitigation minus BaU in 2100",
    scenario = "SSP2-Mitigation vs SSP2-BaU",
    indicator = clean_indicator(variable),
    unit = extract_unit(variable),
    value_2015 = NA_real_,
    value_2100 = `SSP2-Mitigation`,
    reference_value_2100 = `SSP2-BaU`,
    change = `SSP2-Mitigation` - `SSP2-BaU`,
    percent_change = (`SSP2-Mitigation` / `SSP2-BaU` - 1) * 100
  ) %>%
  dplyr::select(figure, comparison, scenario, indicator, unit, value_2015, value_2100, reference_value_2100, change, percent_change)

fig12_production_change <- make_change_rows(
  QSUP_agg_data %>% filter(SCENARIO == "SSP2-Mitigation", region == "World"),
  "Fig12",
  "Change from 2015 to 2100",
  "SSP2-Mitigation",
  name,
  QSUP_agg_load
) %>%
  mutate(unit = "million t")

fig12_land_change <- make_change_rows(
  Land_agg_data %>% filter(SCENARIO == "SSP2-Mitigation", region == "World"),
  "Fig12",
  "Change from 2015 to 2100",
  "SSP2-Mitigation",
  ITEM,
  AGMIP_agg
) %>%
  mutate(unit = "million ha")

fig12_land_mitigation_vs_bau <- Land_agg_data %>%
  filter(Y == 2100, region == "World") %>%
  dplyr::select(SCENARIO, ITEM, AGMIP_agg) %>%
  tidyr::pivot_wider(names_from = SCENARIO, values_from = AGMIP_agg) %>%
  mutate(
    figure = "Fig12",
    comparison = "Mitigation minus BaU in 2100",
    scenario = "SSP2-Mitigation vs SSP2-BaU",
    indicator = as.character(ITEM),
    unit = "million ha",
    value_2015 = NA_real_,
    value_2100 = `SSP2-Mitigation`,
    reference_value_2100 = `SSP2-BaU`,
    change = `SSP2-Mitigation` - `SSP2-BaU`,
    percent_change = (`SSP2-Mitigation` / `SSP2-BaU` - 1) * 100
  ) %>%
  dplyr::select(figure, comparison, scenario, indicator, unit, value_2015, value_2100, reference_value_2100, change, percent_change)

key_text_indicators <- c(
  "Crop Consumption", "Crop Production", "Livestock Consumption", "Livestock Production",
  "Cropland", "Pasture", "Forest", "CH4 emission", "N2O emission", "energy crops",
  "Energy Crop"
)

key_figure_numbers <- dplyr::bind_rows(
  fig7_key_numbers,
  fig8_key_numbers,
  fig11_mitigation_change,
  fig11_mitigation_vs_bau,
  fig12_production_change,
  fig12_land_change,
  fig12_land_mitigation_vs_bau
) %>%
  mutate(
    text_priority = indicator %in% key_text_indicators,
    across(where(is.numeric), ~ round(.x, 3))
  ) %>%
  dplyr::select(figure, comparison, scenario, indicator, unit, value_2015, value_2100, reference_value_2100, change, percent_change, text_priority)

readr::write_csv(key_figure_numbers, paste0(Table_dir, "key_figure_numbers.csv"))


# Fig.13 -------------------------------
sc_map <- tibble::tibble(
  old = c("SSP2_BaU_NoCC", "SSP2_500C_NoCC"),
  new = c("SSP2-BaU",      "SSP2-Mitigation")
)
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>%
  dplyr::transmute(
    ISO_A3 = as.character(iso_a3),
    adm0_a3 = as.character(adm0_a3),
    geometry
  ) %>%
  dplyr::mutate(
    ISO_A3 = dplyr::case_when(
      adm0_a3 == "FRA" ~ "FRA",
      adm0_a3 == "NOR" ~ "NOR",
      adm0_a3 == "KOS" ~ "XKX",
      TRUE ~ ISO_A3
    )
  ) %>%
  dplyr::select(-adm0_a3) %>%
  dplyr::filter(ISO_A3 != "ATA") %>% 
  sf::st_transform(sf::st_crs(Basin_Boundary))
add_missing_from_world <- function(bb, world_sf){
  have_iso    <- unique(bb$ISO_A3)
  missing_iso <- setdiff(unique(world_sf$ISO_A3), have_iso)
  if (length(missing_iso) == 0) return(bb)
  bb <- bb %>%
    { if (!"Value" %in% names(.)) dplyr::mutate(., Value = NA_real_) else . }
  missing_sf <- world_sf %>%
    dplyr::filter(ISO_A3 %in% missing_iso) %>%
    dplyr::mutate(Basin = NA_character_,
                  Value = NA_real_) %>%
    dplyr::select(ISO_A3, Basin, geometry, Value)
  bb %>%
    dplyr::select(ISO_A3, Basin, geometry, Value) %>%
    rbind(missing_sf)
}
Basin_Boundary_aug <- add_missing_from_world(Basin_Boundary, world)

QLene <- QL2 %>%
  filter(c == "ecr") %>%
  dplyr::select(-c)
df <- QLTot %>%
  dplyr::left_join(QLene, by = c("SCENARIO","cty","bas","Y")) %>%
  dplyr::mutate(QLEne_share = dplyr::if_else(is.na(QLTot_load) | QLTot_load == 0, NA_real_, 100 * QL2_load / QLTot_load))%>%
  dplyr::left_join(QLFrs, by = c("SCENARIO","cty","bas","Y")) %>%
  dplyr::mutate(QLFrs_share = dplyr::if_else(is.na(QLTot_load) | QLTot_load == 0, NA_real_, 100 * QLFrsTot_load / QLTot_load)) %>%
  dplyr::select(SCENARIO, cty, bas, Y, QLEne_share, QLFrs_share) %>%
  filter(Y %in% c(2015, 2100)) %>%
  mutate(SCENARIO = as.character(SCENARIO)) %>%
  semi_join(sc_map, by = c("SCENARIO" = "old")) %>%
  left_join(sc_map, by = c("SCENARIO" = "old")) %>%
  mutate(SCENARIO = new) %>%
  dplyr::select(-new) %>%
  mutate(
    title = dplyr::case_when(
      Y == 2015 & SCENARIO == "SSP2-BaU" ~ "2015",
      Y == 2100 & SCENARIO == "SSP2-BaU" ~ "BaU in 2100",
      Y == 2100 & SCENARIO == "SSP2-Mitigation" ~ "Mitigation in 2100",
      TRUE ~ NA_character_
    ),
    title = factor(title, levels = c("2015", "BaU in 2100", "Mitigation in 2100"))
  ) 
basin_map_df <- Basin_Boundary_aug %>%
  dplyr::select(-Value) %>%
  dplyr::left_join(df, by = c("ISO_A3" = "cty", "Basin" = "bas")) %>%
  filter(!is.na(title))
country_borders <- world %>%
  dplyr::group_by(ISO_A3) %>%
  dplyr::summarise(.groups = "drop") %>%
  sf::st_make_valid()

g_Ene <- ggplot() +
  geom_sf(data = world, fill = "grey40", color = NA) +
  geom_sf(data = basin_map_df, aes(fill = QLEne_share), color = NA) +
  geom_sf(data = country_borders, fill = NA, color = "white", linewidth = 0.2) +
  geom_sf(data = country_borders, fill = NA, color = "grey15", linewidth = 0.1) +
  scale_fill_viridis_c(option = "plasma", limits = c(0, NA), na.value = "grey40") +
  labs(title = "(a) Energy crop area", fill = "Share[%]") +
  coord_sf(ylim = c(-60, 90), expand = FALSE) +
  facet_wrap(vars(title)) +
  theme_6 +
  theme(legend.position = "none")+
  theme(legend.position = "right") +
  guides(fill = guide_colorbar(
    direction = "vertical",
    title.position = "top",
    barheight = unit(10, "lines"),
    barwidth  = unit(0.7, "lines")
  ))
g_Frs <- ggplot() +
  geom_sf(data = world, fill = "grey40", color = NA) +
  geom_sf(data = basin_map_df, aes(fill = QLFrs_share), color = NA) +
  geom_sf(data = country_borders, fill = NA, color = "white", linewidth = 0.2) +
  geom_sf(data = country_borders, fill = NA, color = "grey15", linewidth = 0.1) +
  scale_fill_viridis_c(option = "plasma", limits = c(0, 100),  na.value = "grey40") +
  labs(title = "(b) Forest area", fill = "Share[%]") +
  coord_sf(ylim = c(-60, 90), expand = FALSE) +
  facet_wrap(vars(title)) +
  theme_6 +
  theme(legend.position = "none")+
  theme(legend.position = "right") +
  guides(fill = guide_colorbar(
    direction = "vertical",
    title.position = "top",
    barheight = unit(10, "lines"),
    barwidth  = unit(0.7, "lines")
  ))
g_all <- (g_Ene / g_Frs) +
  plot_layout(guides = "keep", heights = c(1, 1))
ggsave(paste0(Fig_dir, "Fig13.png"), g_all, width = 15, height = 6, dpi = 300)


# Fig 14 ----------------------------------
data_14_country <- AgLU_AGMIP %>%
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
data_14_country <- full_join(data_14_country, region) %>%
  mutate(Model = "AIM-ALPHA")
data_14_region <- AGMIP_agg %>%
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
  filter(region %in% region_R5) %>%
  filter(Y == "2100") %>%
  mutate(Model = "AIM-ALPHA")
data_14 <- full_join(data_14_country, data_14_region) %>%
  filter(Y == "2100")  %>%
  mutate(region = dplyr::recode(region,
                                "R5ASIA" = "Asia",
                                "R5MAF" = "Middle East & Africa",
                                "R5REF" = "Reforming Economies",
                                "R5LAM" = "Latin America",
                                "R5OECD90+EU" = "OECD90 & EU",
                                "World" = "World"
  ))
pd <- position_dodge(width = 0.8)
g_14 <- ggplot() +
  geom_bar(data = data_14, aes(x = region, y = region_change, fill = Model), stat = "identity", position = pd, width = 0.8) +
  geom_point(data = data_14, aes(x = region, y = country_change), position = pd, size = 1.2) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  scale_fill_manual(values = c("#f68181", "#76a1ea")) +
  labs(y = "Food Intake Change from BaU [%]") +
  theme_2 +
  theme(legend.position = "none")

data_14_2_2100 <- data_14_country %>% filter(Y == "2100")
data_14_2 <- full_join(Boundary, data_14_2_2100)
g_14_2 <- ggplot() +
  geom_sf(data = data_14_2, aes(fill = country_change)) +
  theme_bw() +
  scale_fill_gradient2(
    low = "#BA2320",
    mid = "white",
    high = "#253368",
    name = "Food Intake Change from BaU [%]"
  ) +
  coord_sf(xlim = c(-180, 180), ylim = c(-90, 90),expand = FALSE,clip   = "on") +
  theme(
    panel.border   = element_blank(),
    axis.text.x    = element_blank(),
    axis.text.y    = element_blank(),
    axis.ticks     = element_blank(),
    axis.title.x   = element_blank(),
    axis.title.y   = element_blank(),
    text           = element_text(size = 12),
    legend.position = c(0.5, -0.1),
    legend.key.size = unit(1, "cm"),
    legend.direction = "horizontal",
    strip.background = element_blank(),
    plot.margin     = margin(0, 0, 0, 0)
  )

PC_14 <- full_join(PC_agg, QSUP_agg) %>%
  mutate(Y = as.character(Y)) %>%
  mutate(Y = as.numeric(Y)) %>%
  mutate(total = PC_agg_load * QSUP_agg_load) %>%
  dplyr::select(-PC_agg_load,-QSUP_agg_load) %>%
  pivot_wider(names_from = "c", values_from = "total") %>%
  # mutate(total = (wht+rce+crl+mze+str+sgr+swt+pls+nut+ocr+vol+vgt+frt+stm+spc+alc+cmt+pmt+omt+rmt+mlk+dai+egg)) %>%
  mutate(total = wht+rce+crl+mze) %>%
  dplyr::select(c(SCENARIO, region, Y, total))
QSUP_14 <- QSUP_agg %>%
  mutate(Y = as.character(Y)) %>%
  mutate(Y = as.numeric(Y)) %>%
  pivot_wider(names_from = "c", values_from = "QSUP_agg_load") %>%
  # mutate(total_QSUP = (wht+rce+crl+mze+str+sgr+swt+pls+nut+ocr+vol+vgt+frt+stm+spc+alc+cmt+pmt+omt+rmt+mlk+dai+egg)) %>%
  mutate(total_QSUP = wht+rce+crl+mze) %>%
  dplyr::select(c(SCENARIO, region, Y, total_QSUP))

data_14_region_2 <- full_join(PC_14, QSUP_14) %>%
  mutate(PC_agg_load = total / total_QSUP) %>%
  dplyr::select(-total,-total_QSUP) %>%
  filter(SCENARIO %in% c("SSP2_BaU_NoCC", "SSP2_500C_NoCC")) %>%
  mutate(SCENARIO = dplyr::recode(SCENARIO,
                                  "SSP2_500C_NoCC" = "Mitigation",
                                  "SSP2_BaU_NoCC" = "BaU"
  )) %>%
  pivot_wider(names_from = "SCENARIO", values_from = "PC_agg_load") %>%
  mutate(change = (Mitigation - BaU) / BaU * 100) %>%
  dplyr::select(!c(BaU, Mitigation)) %>%
  na.omit() %>%
  rename("region_change" = "change") %>%
  filter(region %in% region_R5) %>%
  filter(Y == "2100") %>%
  mutate(Model = "AIM-ALPHA")%>%
  mutate(region = dplyr::recode(region,
                                "R5ASIA" = "Asia",
                                "R5MAF" = "Middle East & Africa",
                                "R5REF" = "Reforming Economies",
                                "R5LAM" = "Latin America",
                                "R5OECD90+EU" = "OECD90 & EU",
                                "World" = "World"
  )) %>%
  mutate(region = factor(region, levels = c("Asia", "Latin America","Middle East & Africa","OECD90 & EU","Reforming Economies","World")))
pd <- position_dodge(width = 0.8)
g_14_3 <- ggplot() +
  geom_bar(data = data_14_region_2, aes(x = region, y = region_change, fill = Model), stat = "identity", position = pd, width = 0.8) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  scale_fill_manual(values = c("#f68181", "#76a1ea")) +
  labs(y = "Price Change from BaU [%]") +
  theme_2 +
  theme(legend.position = "none")

left_col <- g_14_3 / g_14 
g_14 <- (plot_spacer() | left_col | plot_spacer() | g_14_2) +
  plot_layout(widths = c(0.02, 0.5, 0.07, 1.5), guides = "keep") +
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")")  &
  theme(
    plot.tag.position = c(-0.05, 1)
  )
ggsave(paste0(Fig_dir, "Fig14.png"),
       plot = g_14, width = 14, height = 7, dpi = 300)
