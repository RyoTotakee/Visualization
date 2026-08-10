library(sf)
library(ggplot2)
library(dplyr)
library(rnaturalearth)
library(rnaturalearthdata)
library(readxl)
library(readr)
library(gdxrrw)
library(countrycode)
library(patchwork)
library(tidyverse)
library(RColorBrewer)


df_set_emi <- rgdx.param("../../data/thesis/IAMC_templatePlus_CGE_nolimit.gdx", "IAMC_templatePlus")
df_set_emi <- df_set_emi %>%
  filter(Scenario %in% c("SSP2_CM_NoCC","FAO-FAOTier1","EDGAR"))%>%
  mutate(Scenario=recode(Scenario,SSP2_CM_NoCC="AFOLUB","FAO-FAOTier1"="FAOSTAT",EDGAR="EDGAR")) %>% 
  filter(VEMF %in% c("Emi_equ_CH4_AFO_Agr_Liv_Ent_Fer","Emi_equ_CH4_AFO_Agr_Liv_Man_Man","Emi_equ_N2O_AFO_Agr_Liv_Man_Man","Emi_equ_CH4_AFO_Agr_Ric","Emi_equ_N2O_AFO_Agr_Man_Soi","Emi_CO2_Lan_Use_Flo_Pos_Emi_Lan_Use_Cha","Emi_CO2_Lan_Use_Flo_RemForest","Emi_CO2_AFO_Lan_For_Gra_Con","Emi_CO2_Lan_Use_Flo_Pos_Emi_Fir"))%>%
  rename(Value = IAMC_templatePlus)%>%
  filter(REMF %in% c("World")) %>%
  filter(YEMF %in% c("2005")) %>%
  mutate(VEMF = case_when(
    VEMF == "Emi_CO2_AFO_Lan_For_Gra_Con" ~ "Emi_CO2_Lan_Use_Flo_Pos_Emi_Lan_Use_Cha",
    TRUE ~ VEMF
  ))
df_set_emi <- distinct(df_set_emi)

df_LULUCF <- df_set_emi %>%
  filter(VEMF %in% c("Emi_CO2_Lan_Use_Flo_Pos_Emi_Lan_Use_Cha","Emi_CO2_Lan_Use_Flo_RemForest","Emi_CO2_Lan_Use_Flo_Pos_Emi_Fir")) %>%
  mutate(VEMF=recode(VEMF,Emi_CO2_Lan_Use_Flo_Pos_Emi_Lan_Use_Cha="土地利用変化","Emi_CO2_Lan_Use_Flo_Pos_Emi_Fir"="火災","Emi_CO2_Lan_Use_Flo_RemForest"="木質炭素吸収"))

df_AGRICULTURE <- df_set_emi %>%
  filter(VEMF %in% c("Emi_equ_CH4_AFO_Agr_Liv_Ent_Fer","Emi_equ_CH4_AFO_Agr_Liv_Man_Man","Emi_equ_N2O_AFO_Agr_Liv_Man_Man","Emi_equ_CH4_AFO_Agr_Ric","Emi_equ_N2O_AFO_Agr_Man_Soi")) %>%
  mutate(VEMF=recode(VEMF,"Emi_equ_CH4_AFO_Agr_Liv_Ent_Fer"="家畜反芻","Emi_equ_CH4_AFO_Agr_Ric"="稲作","Emi_equ_N2O_AFO_Agr_Man_Soi"="農耕地土壌"))
df_set <- df_AGRICULTURE %>%
  filter(VEMF %in% c("Emi_equ_CH4_AFO_Agr_Liv_Man_Man","Emi_equ_N2O_AFO_Agr_Liv_Man_Man")) %>%
  group_by(Scenario,REMF,YEMF) %>%
  summarize(Value = sum(Value),
            VEMF = "糞尿管理",
            .groups = 'drop')
df_AGRICULTURE2 <- bind_rows(
  df_AGRICULTURE %>% filter(!VEMF %in% c("Emi_equ_CH4_AFO_Agr_Liv_Man_Man","Emi_equ_N2O_AFO_Agr_Liv_Man_Man")),
  df_set)

#LULUCF-----------------------------------------------------------------------------------------------
color_bar1 <- c("木質炭素吸収"="forestgreen","火災"="#d94801","土地利用変化"="#FFE082")

g1 <- df_LULUCF %>%
  mutate(VEMF = factor(VEMF, levels = c("土地利用変化","火災","木質炭素吸収"))) %>%
  ggplot() +
  geom_bar(aes(x=Scenario,y=Value/1000, fill = VEMF),stat = "identity")+
  geom_hline(yintercept = 0, color = "black", linetype = "solid", size = 0.25, alpha = 0.75) +
  labs(tag="(b)",title = "Emissions from Land use change\n土地利用変化由来の排出量",x="Scenario", y = expression(paste('Emissions (Gt',{CO[2]},'eq/yr)',seq="")))+
  #facet_wrap(~REMF, nrow = 5, scales = "free_y")+
  theme_bw()+
  scale_fill_manual(values = color_bar1)+
  theme(plot.title = element_blank(), 
        axis.text.x = element_text(size = 16,color="black"), axis.text.y = element_text(size = 16,color="black"), 
        axis.title.x = element_blank(), axis.title.y = element_text(size=18.5),
        strip.text.x = element_text(size = 10),legend.text = element_text(size =17),
        legend.position = "bottom",legend.title = element_blank(),
        panel.grid.minor = element_line(color = NA), plot.tag = element_text(size = 20),
        axis.line = element_line(colour="black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        strip.background = element_rect(fill="white", colour="white"))
plot(g1)

ggsave(file = "../../output/thesis/4.1 基準年比較/LULUCF.png", plot=g1, width = 8.5, height = 5.5)

#AGRICULTURE------------------------------------------------------------------------------------------
color_bar2 <- c("家畜反芻"="lightsalmon","糞尿管理"="darkolivegreen3","稲作"="peru","農耕地土壌"="lightskyblue3")

g2 <- df_AGRICULTURE2 %>%
  mutate(VEMF = factor(VEMF, levels = c("家畜反芻","糞尿管理","稲作","農耕地土壌"))) %>%
  ggplot() +
  geom_bar(aes(x=Scenario,y=Value/1000, fill = VEMF),stat = "identity")+
  geom_hline(yintercept = 0, color = "black", linetype = "solid", size = 0.25, alpha = 0.75) +
  labs(tag="(a)",title = "Emissions from Land use change\n土地利用変化由来の排出量",x="Scenario", y = expression(paste('Emissions (Gt',{CO[2]},'eq/yr)',seq="")))+
  #facet_wrap(~REMF, nrow = 5, scales = "free_y")+
  theme_bw()+
  scale_fill_manual(values = color_bar2)+
  theme(plot.title = element_blank(), 
        axis.text.x = element_text(size = 16,color="black"), axis.text.y = element_text(size = 16,color="black"), 
        axis.title.x = element_blank(), axis.title.y = element_text(size=18.5),
        strip.text.x = element_text(size = 10),legend.text = element_text(size =17),
        legend.position = "bottom",legend.title = element_blank(),
        panel.grid.minor = element_line(color = NA), plot.tag = element_text(size = 20),
        axis.line = element_line(colour="black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        strip.background = element_rect(fill="white", colour="white"))
plot(g2)

ggsave(file = "../../output/thesis/4.1 基準年比較/AGRICULTURE.png", plot=g2, width = 8.5, height = 5.5)

g3 <-(g2 | g1) + plot_layout(widths = c(1, 1), heights = c(1, 1))
plot(g3)
ggsave(file = "../../output/thesis/4.1 基準年比較/Compare.png", plot=g3, width = 12.5, height = 6.75)

#ERROR check------------------------------------------------------------------------------------------
lu_check <- filter(df_LULUCF,Scenario %in% c("AFOLUB","FAOSTAT")) %>%
  group_by(REMF,YEMF,VEMF) %>%
  summarize(Value = max(Value) - min(Value))
lu_check2 <- filter(df_LULUCF,Scenario %in% c("FAOSTAT"))
lu_check3 <- bind_rows(
  lu_check,
  lu_check2
)
lu_check4 <- lu_check3 %>%
  pivot_wider(names_from = Scenario, values_from = Value) %>%
  rename(minus="NA")
Error_landuse_fao <- lu_check4 %>%
  group_by(REMF,YEMF,VEMF) %>%
  summarize(Error = (sum(minus)/sum(FAOSTAT))*100)

lu_check_fir <- filter(df_LULUCF,Scenario %in% c("AFOLUB","EDGAR")) %>%
  group_by(REMF,YEMF,VEMF) %>%
  summarize(Value = max(Value) - min(Value))
lu_check2_fir <- filter(df_LULUCF,Scenario %in% c("EDGAR"))
lu_check3_fir <- bind_rows(
  lu_check_fir,
  lu_check2_fir
)
lu_check4_fir <- lu_check3_fir %>%
  pivot_wider(names_from = Scenario, values_from = Value) %>%
  rename(minus="NA")
Error_landuse_edgar <- lu_check4_fir %>%
  group_by(REMF,YEMF,VEMF) %>%
  summarize(Error = (sum(minus)/sum(EDGAR))*100)

agr_check <- filter(df_AGRICULTURE2,Scenario %in% c("AFOLUB","FAOSTAT")) %>%
  group_by(REMF,YEMF,VEMF) %>%
  summarize(Value = max(Value) - min(Value))
agr_check2 <- filter(df_AGRICULTURE2,Scenario %in% c("FAOSTAT")) 
#%>%
  rename(Value=IAMC_templatePlus)
agr_check3 <- bind_rows(
  agr_check,
  agr_check2
)
agr_check4 <- agr_check3 %>%
  pivot_wider(names_from = Scenario, values_from = Value) %>%
  rename(minus="NA")
Error_agr_fao <- agr_check4 %>%
  group_by(REMF,YEMF,VEMF) %>%
  summarize(Error = (sum(minus)/sum(FAOSTAT))*100)

agr_check <- filter(df_AGRICULTURE2,Scenario %in% c("AFOLUB","FAOSTAT")) %>%
  group_by(REMF,YEMF,VEMF) %>%
  summarize(Value = max(Value) - min(Value))
agr_check2 <- filter(df_AGRICULTURE2,Scenario %in% c("FAOSTAT"))
agr_check3 <- bind_rows(
  agr_check,
  agr_check2
)
agr_check4 <- agr_check3 %>%
  pivot_wider(names_from = Scenario, values_from = Value) %>%
  rename(minus="NA")
Error_agr_fao <- agr_check4 %>%
  group_by(REMF,YEMF,VEMF) %>%
  summarize(Error = (sum(minus)/sum(FAO))*100)

agr_check_fir <- filter(df_AGRICULTURE2,Scenario %in% c("AFOLUB","EDGAR")) %>%
  group_by(REMF,YEMF,VEMF) %>%
  summarize(Value = max(Value) - min(Value))
agr_check2_fir <- filter(df_AGRICULTURE2,Scenario %in% c("EDGAR"))
agr_check3_fir <- bind_rows(
  agr_check_fir,
  agr_check2_fir
)
agr_check4_fir <- agr_check3_fir %>%
  pivot_wider(names_from = Scenario, values_from = Value) %>%
  rename(minus="NA")
Error_agr_edgar <- agr_check4_fir %>%
  group_by(REMF,YEMF,VEMF) %>%
  summarize(Error = (sum(minus)/sum(EDGAR))*100)
