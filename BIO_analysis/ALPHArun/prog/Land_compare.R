library(tidyverse)
library(gdxrrw)
library(ggplot2)
library(ggpmisc)
library(RColorBrewer)
library(patchwork)
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
  "ALPHA" = 2,
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

AgMIP_region <- c(
  "AIM" = "#007a70",
  "ALPHA" = "#D74322",
  "ENVISAGE" = "#a3d9f2",
  "FARM" = "#26c3ec",
  "MAgPIE" = "#002923",
  "GLOBIOM" = "#8CA231",
  "FAOSTAT" = "#888888",
  "LUH2" = "#707070"
)


region_list <- c("R5REF", "R5OECD90_EU", "R5ASIA", "R5MAF", "R5LAM", "World")


sc <- "SSP2_BaU_NoCC_No"
scenario <- "SSP2_BaU_NoCC"

AIM_Hub <- rgdx.param("../data/global_17_IAMC.gdx", "IAMC_Template") %>% filter(SCENARIO == sc)
UNIT <- rgdx.set("../data/global_17_IAMC.gdx", "VUMAP")
AIM_Hub <- left_join(AIM_Hub, UNIT) %>%
  mutate(UEMF = recode(UEMF,
                       "1000 t" = "1000 t DM",
                       "t/ha" = "dm t/ha"
  ))
AIM_Hub <- AIM_Hub %>%
  rename("region" = "REMF", "variable" = "VEMF", "Y" = "YEMF", "AIMHub" = "IAMC_Template", "UNIT" = "UEMF") %>%
  mutate(SCENARIO = scenario) %>%
  mutate(variable = toupper(variable))
AIM_Hub_AGMIP <- AIM_Hub[grepl(c("AGMIP"), AIM_Hub$variable), ]
AIM_Hub_LAND <- AIM_Hub %>%
  filter(variable %in% c("LAN_COV", "LAN_COV_BUI_ARE", "LAN_COV_CRO", "LAN_COV_FRS", "LAN_COV_OTH_ARA_LAN", "LAN_COV_OTH_LAN", "LAN_COV_PST")) %>%
  pivot_wider(names_from = "variable", values_from = "AIMHub") %>%
  mutate(
    AGMIP_TOT_LAND = LAN_COV * 1000,
    AGMIP_CRP_LAND = LAN_COV_CRO * 1000,
    AGMIP_FOR_LAND = LAN_COV_FRS * 1000,
    AGMIP_ONV_LAND = (LAN_COV_OTH_LAN) * 1000,
    AGMIP_NLD_LAND = (LAN_COV_BUI_ARE) * 1000,
    AGMIP_GRS_LAND = LAN_COV_PST * 1000,
    AGMIP_NAT_LAND = (LAN_COV_FRS + LAN_COV_OTH_LAN) * 1000,
    UNIT = "1000 ha"
  ) %>%
  select(SCENARIO, region, Y, UNIT, AGMIP_TOT_LAND, AGMIP_CRP_LAND, AGMIP_FOR_LAND, AGMIP_GRS_LAND, AGMIP_NLD_LAND, AGMIP_ONV_LAND, AGMIP_NAT_LAND) %>%
  pivot_longer(cols = -c(SCENARIO, region, Y, UNIT), names_to = "variable", values_to = "AIMHub")
AIM_Hub_Price <- AIM_Hub %>%
  filter(UNIT == "Index (2005 = 1)") %>%
  pivot_wider(names_from = "Y", values_from = "AIMHub") %>%
  mutate("base" = `2015`) %>%
  pivot_longer(cols = -c(SCENARIO, region, variable, UNIT, base), names_to = "Y", values_to = "AIMHub") %>%
  mutate(AIMHub = AIMHub / base, UNIT = "Index (2015 = 1)") %>%
  select(!base)
AIM_Hub_AGMIP <- full_join(AIM_Hub_AGMIP, AIM_Hub_LAND)
AIM_Hub_AGMIP <- full_join(AIM_Hub_AGMIP, AIM_Hub_Price) %>%
  mutate(Y = as.character(Y)) %>%
  mutate(Y = as.numeric(Y))

AGMIP <- rgdx.param(paste0("../data/AGMIP_all_scenario.gdx"), "AGMIP_agg") %>%
  filter(SCENARIO == scenario) %>%
  select(-SCENARIO) %>%
  mutate(AgMIP_VAR = recode(AgMIP_VAR, "LSP" = "PAS")) %>%
  unite(col = variable, ITEM, AgMIP_VAR, sep = "_") %>%
  mutate(variable = paste0("AGMIP_", variable)) %>%
  mutate(variable = toupper(variable)) %>%
  rename("ALPHA" = "AGMIP_agg", "UNIT" = "AgMIP_UNIT") %>%
  mutate(Y = as.character(Y)) %>%
  mutate(Y = as.numeric(Y))

AGMIP_full <- inner_join(AIM_Hub_AGMIP, AGMIP)
var_list <- unique(AGMIP_full$variable)
unit_list <- unique(AGMIP_full$UNIT)
region_list <- unique(AGMIP_full$region)
region_list <- c("R5REF", "R5OECD90+EU", "R5ASIA", "R5MAF", "R5LAM", "World")

AGMIP_full <- full_join(AIM_Hub_AGMIP, AGMIP) %>%
  pivot_longer(cols = -c(SCENARIO, region, variable, Y, UNIT), names_to = "Model", values_to = "Value") %>%
  mutate(UNIT = as.character(UNIT)) %>%
  filter(variable %in% var_list, UNIT %in% unit_list, region %in% region_list) %>%
  na.omit() %>%
  mutate(
    region = factor(
      region,
      levels = region_list
    )
  )


AIM_Hub_plot <- function(i) {
  AGMIP_full_var <- AGMIP_full %>%
    filter(variable == i)
  j <- unique(AGMIP_full_var$UNIT)
  k <- substr(i, 11, 14)
  g_plot <- ggplot() +
    geom_line(data = AGMIP_full_var, aes(x = Y, y = Value, color = Model), linewidth = 1) +
    geom_hline(yintercept = 0, linetype = "dotted") +
    geom_vline(xintercept = 2015, linetype = "dotted") +
    facet_wrap(vars(region), scales = "free_y") +
    scale_x_continuous(breaks = seq(2000, 2100, 20)) +
    scale_y_continuous(limits = c(0, NA)) +
    scale_shape_manual(values = model_shape) +
    labs(y = j) +
    theme_1
  ggsave(paste0("../output/land_compare/", k, "/", i, ".png"), g_plot, width = 15, height = 8, create.dir = T)
}

var_list <- c("AGMIP_CRP_LAND", "AGMIP_GRS_LAND", "AGMIP_FOR_LAND", "AGMIP_ONV_LAND")
for (var in var_list) {
  AIM_Hub_plot(var)
}

