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

df_set_agr_AgLU_a <- rgdx.param("../../data/thesis/IAMC_templatePlus_AgLU_limit_a.gdx", "IAMC_templatePlus")
df_set_agr_AgLU_b <- rgdx.param("../../data/thesis/IAMC_templatePlus_AgLU_limit_b.gdx", "IAMC_templatePlus")
df_set_agr_AgLU <- full_join(df_set_agr_AgLU_a, df_set_agr_AgLU_b, by = c("REMF", "Scenario", "YEMF","VEMF","IAMC_templatePlus"))
df_set_agr_AgLU <- df_set_agr_AgLU %>%
  group_by(REMF,Scenario,YEMF,VEMF) %>%
  summarise(IAMC_templatePlus = sum(IAMC_templatePlus, na.rm = TRUE))
df_set_red_agr_AgLU <- df_set_agr_AgLU %>%
  filter(Scenario %in% c("SSP2_CM_NoCC")) %>%
  mutate(Scenario=recode(Scenario,"SSP2_CM_NoCC"="AgLU")) %>%
  filter(VEMF %in% c("Emi_Red_equ_CH4_AFO_Agr_Liv_Ent_Fer_Bre","Emi_Red_equ_CH4_AFO_Agr_Liv_Ent_Fer_Fee","Emi_Red_equ_CH4_AFO_Agr_Liv_Man_Man_Cad","Emi_Red_equ_CH4_AFO_Agr_Liv_Man_Man_And","Emi_Red_equ_CH4_AFO_Agr_Liv_Man_Man_Cfl","Emi_Red_equ_CH4_AFO_Agr_Liv_Man_Man_Adc","Emi_Red_equ_CH4_AFO_Agr_Liv_Man_Man_Ams","Emi_Red_equ_CH4_AFO_Agr_Ric_Wat","Emi_Red_equ_CH4_AFO_Agr_Ric_Res","Emi_Red_equ_N2O_AFO_Agr_Man_Soi_Til","Emi_Red_equ_N2O_AFO_Agr_Man_Soi_Fer","Emi_Red_equ_N2O_AFO_Agr_Man_Soi_Res","Emi_Red_equ_N2O_AFO_Agr_Man_Soi_Slo","Emi_Red_equ_N2O_AFO_Agr_Man_Soi_Nin"))

df_set_lu_AgLU_a <- rgdx.param("../../data/thesis/IAMC_templatePlus_AgLU_nolimit_a.gdx", "IAMC_templatePlus")
df_set_lu_AgLU_b <- rgdx.param("../../data/thesis/IAMC_templatePlus_AgLU_nolimit_b.gdx", "IAMC_templatePlus")
df_set_lu_AgLU <- full_join(df_set_lu_AgLU_a, df_set_lu_AgLU_b, by = c("REMF", "Scenario", "YEMF","VEMF","IAMC_templatePlus"))
df_set_lu_AgLU <- df_set_lu_AgLU %>%
  group_by(REMF,Scenario,YEMF,VEMF) %>%
  summarise(IAMC_templatePlus = sum(IAMC_templatePlus, na.rm = TRUE))
df_set_red_lu_AgLU <- df_set_lu_AgLU %>%
  filter(Scenario %in% c("SSP2_CM_NoCC")) %>%
  mutate(Scenario=recode(Scenario,"SSP2_CM_NoCC"="AgLU")) %>%
  filter(VEMF %in% c("Emi_Red_CO2_AFO_Lan_Pla","Emi_Red_CO2_AFO_Lan_Ref","Emi_Red_CO2_AFO_Lan_FPr","Emi_Red_CO2_AFO_Lan_Pff","Emi_Red_CO2_AFO_Lan_Ril","Emi_Red_CO2_AFO_Lan_Enr","Emi_Red_CO2_AFO_Lan_Agf","Emi_Red_CO2_AFO_Lan_Bch","Emi_Red_CO2_AFO_Lan_Scs"))

df_set_red_AgLU <- full_join(df_set_red_agr_AgLU, df_set_red_lu_AgLU, by = c("REMF", "Scenario", "YEMF","VEMF","IAMC_templatePlus"))

df_set_agr_a <- rgdx.param("../../data/thesis/IAMC_templatePlus_CGE_limit_a.gdx", "IAMC_templatePlus")
df_set_agr_b <- rgdx.param("../../data/thesis/IAMC_templatePlus_CGE_limit_b.gdx", "IAMC_templatePlus")
df_set_agr <- full_join(df_set_agr_a, df_set_agr_b, by = c("REMF", "Scenario", "YEMF","VEMF","IAMC_templatePlus"))
df_set_agr <- df_set_agr %>%
  group_by(REMF,Scenario,YEMF,VEMF) %>%
  summarise(IAMC_templatePlus = sum(IAMC_templatePlus, na.rm = TRUE))
df_set_red_agr <- df_set_agr  %>%
  filter(Scenario %in% c("SSP2_CM_NoCC")) %>%
  mutate(Scenario=recode(Scenario,"SSP2_CM_NoCC"="AIM-Hub")) %>%
  filter(VEMF %in% c("Emi_Red_equ_CH4_AFO_Agr_Liv_Ent_Fer_Bre","Emi_Red_equ_CH4_AFO_Agr_Liv_Ent_Fer_Fee","Emi_Red_equ_CH4_AFO_Agr_Liv_Man_Man_Cad","Emi_Red_equ_CH4_AFO_Agr_Liv_Man_Man_And","Emi_Red_equ_CH4_AFO_Agr_Liv_Man_Man_Cfl","Emi_Red_equ_CH4_AFO_Agr_Liv_Man_Man_Adc","Emi_Red_equ_CH4_AFO_Agr_Liv_Man_Man_Ams","Emi_Red_equ_CH4_AFO_Agr_Ric_Wat","Emi_Red_equ_CH4_AFO_Agr_Ric_Res","Emi_Red_equ_N2O_AFO_Agr_Man_Soi_Til","Emi_Red_equ_N2O_AFO_Agr_Man_Soi_Fer","Emi_Red_equ_N2O_AFO_Agr_Man_Soi_Res","Emi_Red_equ_N2O_AFO_Agr_Man_Soi_Slo","Emi_Red_equ_N2O_AFO_Agr_Man_Soi_Nin"))

df_set_lu_a <- rgdx.param("../../data/thesis/IAMC_templatePlus_CGE_nolimit_a.gdx", "IAMC_templatePlus")
df_set_lu_b <- rgdx.param("../../data/thesis/IAMC_templatePlus_CGE_nolimit_b.gdx", "IAMC_templatePlus")
df_set_lu <- full_join(df_set_lu_a, df_set_lu_b, by = c("REMF", "Scenario", "YEMF","VEMF","IAMC_templatePlus"))
df_set_lu <- df_set_lu %>%
  group_by(REMF,Scenario,YEMF,VEMF) %>%
  summarise(IAMC_templatePlus = sum(IAMC_templatePlus, na.rm = TRUE))
df_set_red_lu <- df_set_lu  %>%
  filter(Scenario %in% c("SSP2_CM_NoCC")) %>%
  mutate(Scenario=recode(Scenario,"SSP2_CM_NoCC"="AIM-Hub")) %>%
  filter(VEMF %in% c("Emi_Red_CO2_AFO_Lan_Pla","Emi_Red_CO2_AFO_Lan_Ref","Emi_Red_CO2_AFO_Lan_FPr","Emi_Red_CO2_AFO_Lan_Pff","Emi_Red_CO2_AFO_Lan_Ril","Emi_Red_CO2_AFO_Lan_Enr","Emi_Red_CO2_AFO_Lan_Agf","Emi_Red_CO2_AFO_Lan_Bch","Emi_Red_CO2_AFO_Lan_Scs"))

df_set_red_CGE <- full_join(df_set_red_agr, df_set_red_lu, by = c("REMF", "Scenario", "YEMF","VEMF","IAMC_templatePlus"))


df_set_red <- df_set_red_CGE %>%
  full_join(df_set_red_AgLU, by = c("VEMF","Scenario","YEMF","REMF","IAMC_templatePlus"))

df_set_red <- df_set_red %>%
  #filter(VEMF %in% c("Emi_Red_equ_CH4_AFO_Agr_Liv_Ent_Fer_Bre","Emi_Red_equ_CH4_AFO_Agr_Liv_Ent_Fer_Fee","Emi_Red_equ_CH4_AFO_Agr_Liv_Man_Man_Cad","Emi_Red_equ_CH4_AFO_Agr_Liv_Man_Man_And","Emi_Red_equ_CH4_AFO_Agr_Liv_Man_Man_Cfl","Emi_Red_equ_CH4_AFO_Agr_Liv_Man_Man_Adc","Emi_Red_equ_CH4_AFO_Agr_Liv_Man_Man_Ams","Emi_Red_equ_CH4_AFO_Agr_Ric_Wat","Emi_Red_equ_CH4_AFO_Agr_Ric_Res","Emi_Red_equ_N2O_AFO_Agr_Man_Soi_Til","Emi_Red_equ_N2O_AFO_Agr_Man_Soi_Fer","Emi_Red_equ_N2O_AFO_Agr_Man_Soi_Res","Emi_Red_equ_N2O_AFO_Agr_Man_Soi_Slo","Emi_Red_equ_N2O_AFO_Agr_Man_Soi_Nin",
  #                   "Emi_Red_CO2_AFO_Lan_Pla","Emi_Red_CO2_AFO_Lan_Ref","Emi_Red_CO2_AFO_Lan_FPr","Emi_Red_CO2_AFO_Lan_Pff","Emi_Red_CO2_AFO_Lan_Ril","Emi_Red_CO2_AFO_Lan_Enr","Emi_Red_CO2_AFO_Lan_Agf","Emi_Red_CO2_AFO_Lan_Bch","Emi_Red_CO2_AFO_Lan_Scs"))%>%
  mutate(VEMF=recode(VEMF,Emi_Red_equ_CH4_AFO_Agr_Liv_Ent_Fer_Bre="生産性が高い家畜種への変更",Emi_Red_equ_CH4_AFO_Agr_Liv_Ent_Fer_Fee="濃厚飼料の投与",Emi_Red_equ_CH4_AFO_Agr_Liv_Man_Man_Cad="ラグーンカバー",Emi_Red_equ_CH4_AFO_Agr_Liv_Man_Man_And="嫌気性消化(大規模プラント)",Emi_Red_equ_CH4_AFO_Agr_Liv_Man_Man_Cfl="嫌気性消化(小規模)",Emi_Red_equ_CH4_AFO_Agr_Liv_Man_Man_Adc="糞尿堆肥化",Emi_Red_equ_CH4_AFO_Agr_Liv_Man_Man_Ams="窒素肥料の硫酸アンモニウムへの変更",Emi_Red_equ_CH4_AFO_Agr_Ric_Wat="水田への間断灌漑の導入",Emi_Red_equ_CH4_AFO_Agr_Ric_Res="稲わらのすき込み",
                     Emi_Red_equ_N2O_AFO_Agr_Man_Soi_Til="不耕起移植栽培",Emi_Red_equ_N2O_AFO_Agr_Man_Soi_Fer="肥料の高効率利用",Emi_Red_equ_N2O_AFO_Agr_Man_Soi_Res="化学肥料の有機肥料による置き換え",Emi_Red_equ_N2O_AFO_Agr_Man_Soi_Slo="被覆肥料の利用",Emi_Red_equ_N2O_AFO_Agr_Man_Soi_Nin="硝化抑制剤を含む肥料の利用",
                     Emi_Red_CO2_AFO_Lan_Pla="植林",Emi_Red_CO2_AFO_Lan_Ref="再植林",Emi_Red_CO2_AFO_Lan_FPr="森林保護",Emi_Red_CO2_AFO_Lan_Pff="森林火災の防止","Emi_Red_CO2_AFO_Lan_Ril"="環境負荷の少ない伐採",Emi_Red_CO2_AFO_Lan_Enr="自然再生の強化",Emi_Red_CO2_AFO_Lan_Agf="アグロフォレストリー",Emi_Red_CO2_AFO_Lan_Bch="バイオ炭",Emi_Red_CO2_AFO_Lan_Scs="土壌炭素隔離"))
df_set_red <- df_set_red %>%
  mutate(IAMC_templatePlus = ifelse(IAMC_templatePlus < 0, -IAMC_templatePlus, IAMC_templatePlus))

#set(排出量)---------------------------------------------------------------------
df_emi_aglu <- df_set_agr_AgLU %>%
  filter(VEMF %in% c("Emi_equ_CH4_AFO_Agr_Liv_Ent_Fer","Emi_equ_CH4_AFO_Agr_Liv_Man_Man","Emi_equ_CH4_AFO_Agr_Ric","Emi_equ_N2O_AFO_Agr_Liv_Man_Man","Emi_equ_N2O_AFO_Agr_Man_Soi","Emi_CO2_Lan_Use_Flo_Pos_Emi_Lan_Use_Cha","Emi_CO2_Lan_Use_Flo_RemForest")) %>%
  filter(Scenario %in% c("SSP2_CM_NoCC")) %>%
  mutate(Scenario=recode(Scenario,"SSP2_CM_NoCC"="AgLU"))

df_emi_CGE <- df_set_agr %>%
  filter(VEMF %in% c("Emi_equ_CH4_AFO_Agr_Liv_Ent_Fer","Emi_equ_CH4_AFO_Agr_Liv_Man_Man","Emi_equ_CH4_AFO_Agr_Ric","Emi_equ_N2O_AFO_Agr_Liv_Man_Man","Emi_equ_N2O_AFO_Agr_Man_Soi","Emi_CO2_Lan_Use_Flo_Pos_Emi_Lan_Use_Cha","Emi_CO2_Lan_Use_Flo_RemForest")) %>%
  filter(Scenario %in% c("SSP2_CM_NoCC")) %>%
  mutate(Scenario=recode(Scenario,"SSP2_CM_NoCC"="AIM-Hub"))

agr_cge <- df_emi_CGE %>%
  full_join(df_emi_aglu, by = c("VEMF","Scenario","YEMF","REMF","IAMC_templatePlus"))
agr_cge2 <- agr_cge %>%
  filter(VEMF %in% c("Emi_equ_CH4_AFO_Agr_Liv_Man_Man","Emi_equ_N2O_AFO_Agr_Liv_Man_Man")) %>%
  group_by(Scenario,REMF,YEMF) %>%
  summarize(IAMC_templatePlus = sum(IAMC_templatePlus),
            VEMF = "糞尿管理",
            .groups = 'drop')
agr_cge3 <- bind_rows(
  agr_cge %>% filter(!VEMF %in% c("Emi_equ_CH4_AFO_Agr_Liv_Man_Man","Emi_equ_N2O_AFO_Agr_Liv_Man_Man")),
  agr_cge2) %>%
  mutate(VEMF=recode(VEMF,"Emi_equ_CH4_AFO_Agr_Liv_Ent_Fer"="家畜反芻","Emi_equ_CH4_AFO_Agr_Ric"="稲作","Emi_equ_N2O_AFO_Agr_Man_Soi"="農耕地土壌","Emi_CO2_Lan_Use_Flo_Pos_Emi_Lan_Use_Cha"="土地利用変化","Emi_CO2_Lan_Use_Flo_RemForest"="木質炭素吸収"))

#排出量比較----------------------------------------------------------------------
df_emi_R5 <- agr_cge3 %>%
  filter(REMF %in% c("R5REF","R5LAM","R5ASIA","R5OECD90+EU","R5MAF","World")) %>%
  mutate(REMF=recode(REMF,R5REF="旧ソ連地域",R5LAM="中南米",R5ASIA="アジア",R5MAF="アフリカ・中東","R5OECD90+EU"="OECD+EU"))

df_emi_R5_2100 <- df_emi_R5 %>%
  filter(YEMF %in% c("2100"))

df_emi_R5_EF <- filter(df_emi_R5,VEMF %in% c("家畜反芻"))
df_emi_R5_MM <- filter(df_emi_R5,VEMF %in% c("糞尿管理"))
df_emi_R5_RC <- filter(df_emi_R5,VEMF %in% c("稲作"))
df_emi_R5_MS <- filter(df_emi_R5,VEMF %in% c("農耕地土壌"))
df_emi_R5_RUC <- filter(df_emi_R5,VEMF %in% c("土地利用変化"))
df_emi_R5_WCC <- filter(df_emi_R5,VEMF %in% c("木質炭素吸収"))


Error_R5 <- df_emi_R5_2100 %>%
  group_by(REMF,YEMF,VEMF) %>%
  summarize(Value = max(IAMC_templatePlus) - min(IAMC_templatePlus))
Error_R5_max <- df_emi_R5_2100 %>%
  group_by(REMF,YEMF,VEMF) %>%
  summarize(MAX = max(IAMC_templatePlus))
Error_R5_2 <- Error_R5 %>%
  full_join(Error_R5_max, by = c("VEMF","YEMF","REMF"))
Error_R5_3 <- Error_R5_2 %>%
  group_by(REMF,YEMF,VEMF) %>%
  summarize(Error = (sum(Value)/sum(MAX))*100)

color_bar <- c("AgLU"="lightsalmon","AIM-Hub"="lightgrey")

g1 <- df_emi_R5_EF %>%
  mutate(REMF = factor(REMF, levels = c("OECD+EU","アジア","アフリカ・中東","中南米","旧ソ連地域","World"))) %>%
  ggplot() +
  geom_hline(yintercept = 0, color = "black", linetype = "solid", size = 0.25, alpha = 0.7) +
  geom_line(aes(x=YEMF,y=IAMC_templatePlus/1000,group=Scenario,color=Scenario),linewidth = 1)+
  geom_point(aes(x=YEMF,y=IAMC_templatePlus/1000,group=Scenario,color=Scenario), size = 3)+  
  facet_wrap(~REMF, nrow = 2, scales = "free_y")+
  labs(tag="(a)",title = "家畜反芻", x="", y = expression(paste('Emissions (Gt',{CO[2]},'eq/yr)',seq="")))+
  theme_bw()+
  scale_color_manual(values = color_bar)+
  #scale_linetype_manual(values = linetype_bar) + 
  theme(plot.title =element_text(size = 20), plot.tag=element_text(size = 18),
        axis.text.x = element_text(size = 12,color="black",angle = 45, hjust = 1), axis.text.y = element_text(size = 18,color="black"), 
        axis.title.x = element_blank(), axis.title.y = element_text(size=22),
        strip.text.x = element_text(size = 20),legend.text = element_text(size =22),
        legend.position = "none",legend.title = element_blank(),
        panel.grid.minor = element_line(color = NA), 
        axis.line = element_line(colour="black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        strip.background = element_rect(fill="white", colour="white"))
plot(g1)

g2 <- df_emi_R5_MM %>%
  mutate(REMF = factor(REMF, levels = c("OECD+EU","アジア","アフリカ・中東","中南米","旧ソ連地域","World"))) %>%
  ggplot() +
  geom_hline(yintercept = 0, color = "black", linetype = "solid", size = 0.25, alpha = 0.7) +
  geom_line(aes(x=YEMF,y=IAMC_templatePlus/1000,group=Scenario,color=Scenario),linewidth = 1)+
  geom_point(aes(x=YEMF,y=IAMC_templatePlus/1000,group=Scenario,color=Scenario), size = 3)+  
  facet_wrap(~REMF, nrow = 2, scales = "free_y")+
  labs(tag="(b)",title = "糞尿管理", x="", y = expression(paste('Emissions (Gt',{CO[2]},'eq/yr)',seq="")))+
  theme_bw()+
  scale_color_manual(values = color_bar)+
  #scale_linetype_manual(values = linetype_bar) + 
  theme(plot.title = element_text(size = 20), plot.tag=element_text(size = 18),
        axis.text.x = element_text(size = 12,color="black",angle = 45, hjust = 1), axis.text.y = element_text(size = 18,color="black"), 
        axis.title.x = element_blank(), axis.title.y = element_blank(),
        strip.text.x = element_text(size = 20),legend.text = element_text(size =22),
        legend.position = "none",legend.title = element_blank(),
        panel.grid.minor = element_line(color = NA), 
        axis.line = element_line(colour="black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        strip.background = element_rect(fill="white", colour="white"))
plot(g2)

g3 <- df_emi_R5_RC %>%
  mutate(REMF = factor(REMF, levels = c("OECD+EU","アジア","アフリカ・中東","中南米","旧ソ連地域","World"))) %>%
  ggplot() +
  geom_hline(yintercept = 0, color = "black", linetype = "solid", size = 0.25, alpha = 0.7) +
  geom_line(aes(x=YEMF,y=IAMC_templatePlus/1000,group=Scenario,color=Scenario),linewidth = 1)+
  geom_point(aes(x=YEMF,y=IAMC_templatePlus/1000,group=Scenario,color=Scenario), size = 3)+  
  facet_wrap(~REMF, nrow = 2, scales = "free_y")+
  labs(tag="(c)",title = "稲作", x="", y = expression(paste('Emissions (Gt',{CO[2]},'eq/yr)',seq="")))+
  theme_bw()+
  scale_color_manual(values = color_bar)+
  #scale_linetype_manual(values = linetype_bar) + 
  theme(plot.title = element_text(size = 20), plot.tag=element_text(size = 18),
        axis.text.x = element_text(size = 12,color="black",angle = 45, hjust = 1), axis.text.y = element_text(size = 18,color="black"), 
        axis.title.x = element_blank(), axis.title.y = element_text(size=22),
        strip.text.x = element_text(size = 20),legend.text = element_text(size =22),
        legend.position = "none",legend.title = element_blank(),
        panel.grid.minor = element_line(color = NA), 
        axis.line = element_line(colour="black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        strip.background = element_rect(fill="white", colour="white"))
plot(g3)

g4 <- df_emi_R5_MS %>%
  mutate(REMF = factor(REMF, levels = c("OECD+EU","アジア","アフリカ・中東","中南米","旧ソ連地域","World"))) %>%
  ggplot() +
  geom_hline(yintercept = 0, color = "black", linetype = "solid", size = 0.25, alpha = 0.7) +
  geom_line(aes(x=YEMF,y=IAMC_templatePlus/1000,group=Scenario,color=Scenario),linewidth = 1)+
  geom_point(aes(x=YEMF,y=IAMC_templatePlus/1000,group=Scenario,color=Scenario), size = 3)+  
  facet_wrap(~REMF, nrow = 2, scales = "free_y")+
  labs(tag="(d)",title = "農耕地土壌", x="", y = expression(paste('Emissions (Gt',{CO[2]},'eq/yr)',seq="")))+
  theme_bw()+
  scale_color_manual(values = color_bar)+
  #scale_linetype_manual(values = linetype_bar) + 
  theme(plot.title = element_text(size = 20), plot.tag=element_text(size = 18),
        axis.text.x = element_text(size = 12,color="black",angle = 45, hjust = 1), axis.text.y = element_text(size = 18,color="black"), 
        axis.title.x = element_blank(), axis.title.y = element_blank(),
        strip.text.x = element_text(size = 20),legend.text = element_blank(),
        legend.position = "none",legend.title = element_blank(),
        panel.grid.minor = element_line(color = NA), 
        axis.line = element_line(colour="black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        strip.background = element_rect(fill="white", colour="white"))
plot(g4)

g5 <- df_emi_R5_RUC %>%
  mutate(REMF = factor(REMF, levels = c("OECD+EU","アジア","アフリカ・中東","中南米","旧ソ連地域","World"))) %>%
  ggplot() +
  geom_hline(yintercept = 0, color = "black", linetype = "solid", size = 0.25, alpha = 0.7) +
  geom_line(aes(x=YEMF,y=IAMC_templatePlus/1000,group=Scenario,color=Scenario),linewidth = 1)+
  geom_point(aes(x=YEMF,y=IAMC_templatePlus/1000,group=Scenario,color=Scenario), size = 3)+  
  facet_wrap(~REMF, nrow = 2, scales = "free_y")+
  labs(tag="(e)",title = "土地利用変化", x="", y = expression(paste('Emissions (Gt',{CO[2]},'eq/yr)',seq="")))+
  theme_bw()+
  scale_color_manual(values = color_bar)+
  #scale_linetype_manual(values = linetype_bar) + 
  theme(plot.title = element_text(size = 20), plot.tag=element_text(size = 18),
        axis.text.x = element_text(size = 12,color="black",angle = 45, hjust = 1), axis.text.y = element_text(size = 18,color="black"), 
        axis.title.x = element_blank(), axis.title.y = element_text(size=22),
        strip.text.x = element_text(size = 20),legend.text = element_text(size =22),
        legend.position = "none",legend.title = element_blank(),
        panel.grid.minor = element_line(color = NA), 
        axis.line = element_line(colour="black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        strip.background = element_rect(fill="white", colour="white"))
plot(g5)

g6 <- df_emi_R5_WCC %>%
  mutate(REMF = factor(REMF, levels = c("OECD+EU","アジア","アフリカ・中東","中南米","旧ソ連地域","World"))) %>%
  ggplot() +
  geom_hline(yintercept = 0, color = "black", linetype = "solid", size = 0.25, alpha = 0.7) +
  geom_line(aes(x=YEMF,y=IAMC_templatePlus/1000,group=Scenario,color=Scenario),linewidth = 1)+
  geom_point(aes(x=YEMF,y=IAMC_templatePlus/1000,group=Scenario,color=Scenario), size = 3)+  
  facet_wrap(~REMF, nrow = 2, scales = "free_y")+
  labs(tag="(f)",title = "木質炭素吸収", x="", y = expression(paste('Emissions (Gt',{CO[2]},'eq/yr)',seq="")))+
  theme_bw()+
  scale_color_manual(values = color_bar)+
  #scale_linetype_manual(values = linetype_bar) + 
  theme(plot.title = element_text(size = 20), plot.tag=element_text(size = 18),
        axis.text.x = element_text(size = 12,color="black",angle = 45, hjust = 1), axis.text.y = element_text(size = 18,color="black"), 
        axis.title.x = element_blank(), axis.title.y = element_blank(),
        strip.text.x = element_text(size = 20),legend.text = element_text(size =22),
        legend.position = "none",legend.title = element_blank(),
        panel.grid.minor = element_line(color = NA), 
        axis.line = element_line(colour="black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        strip.background = element_rect(fill="white", colour="white"))
plot(g6)

p <- df_emi_R5_WCC%>%
  mutate(Scenario = factor(Scenario, levels = c("AgLU","AIM-Hub"))) %>%
  ggplot(aes(x=YEMF,y=IAMC_templatePlus/1000,group=Scenario,color=Scenario)) +
  geom_point(size = 3) +
  geom_line(linewidth = 1)+
  theme_bw()+
  #scale_linetype_manual(values = linetype_bar) + 
  scale_color_manual(values = color_bar)+
  theme(plot.title = element_blank(), plot.tag=element_blank(),
        axis.text.x = element_blank(), axis.text.y = element_blank(), 
        axis.title.x = element_blank(), axis.title.y = element_blank(),
        strip.text.x = element_blank(),legend.text = element_text(size =25),
        legend.position = "bottom",legend.title = element_blank(),
        panel.grid.minor = element_blank(), panel.grid = element_blank(),
        axis.line = element_blank(),
        panel.background = element_blank(),axis.ticks.x = element_blank(),axis.ticks.y = element_blank(),
        panel.grid.major = element_blank(),panel.border = element_blank(),
        strip.background = element_blank())


g7 <- (g1| g2) + plot_layout(widths = c(1,  1), heights = c(1, 1))
g8 <- (g3| g4) + plot_layout(widths = c(1,  1), heights = c(1, 1))
g9 <- (g5| g6) + plot_layout(widths = c(1,  1), heights = c(1, 1))
g10 <- (g7/ g8 / g9 / p) + plot_layout(widths = c(1,1), heights = c(1, 1, 1 , 0.00015))

plot(g10)
ggsave(file = "../../output/thesis/4.5 AgLU比較/排出量.png", plot=g10,width = 18.75, height = 17.25)

#削減量比較----------------------------------------------------------------------
df_red_GL <- df_set_red %>%
  filter(REMF %in% c("World"))

df_red_GL_cate <- df_red_GL %>%
  mutate(cate = case_when(
    VEMF == "植林" ~ "森林再生",
    VEMF == "再植林" ~ "森林再生",
    VEMF == "森林保護" ~ "保護",
    VEMF == "環境負荷の少ない伐採" ~ "保護",
    VEMF == "自然再生の強化" ~ "管理",
    VEMF == "森林火災の防止" ~ "管理",
    VEMF == "アグロフォレストリー" ~ "農地再生",
    VEMF == "バイオ炭" ~ "農地再生",
    VEMF == "土壌炭素隔離" ~ "農地再生",
    VEMF == "生産性が高い家畜種への変更" ~ "農業",
    VEMF == "濃厚飼料の投与" ~ "農業",
    VEMF == "ラグーンカバー" ~ "農業",
    VEMF == "嫌気性消化(大規模プラント)" ~ "農業",
    VEMF == "嫌気性消化(小規模)" ~ "農業",
    VEMF == "糞尿堆肥化" ~ "農業",
    VEMF == "窒素肥料の硫酸アンモニウムへの変更" ~ "農業",
    VEMF == "水田への間断灌漑の導入" ~ "農業",
    VEMF == "稲わらのすき込み" ~ "農業",
    VEMF == "不耕起移植栽培" ~ "農業",
    VEMF == "肥料の高効率利用" ~ "農業",
    VEMF == "化学肥料の有機肥料による置き換え" ~ "農業",
    VEMF == "被覆肥料の利用" ~ "農業",
    VEMF == "硝化抑制剤を含む肥料の利用" ~ "農業"
  ))

df_red_GL_cate_sum <-df_red_GL_cate %>%
  group_by(Scenario,REMF,YEMF,cate) %>%
  summarize(
    IAMC_templatePlus = sum(IAMC_templatePlus)
  )

df_red_GL_2050 <- df_red_GL_cate_sum %>%
  filter(YEMF %in% c("2050"))

df_red_GL_2100 <- df_red_GL_cate_sum %>%
  filter(YEMF %in% c("2100"))

Error_red_GL <- df_red_GL_2100 %>%
  group_by(REMF,YEMF,cate) %>%
  summarize(Value = max(IAMC_templatePlus)/min(IAMC_templatePlus))
Error_red_GL_max <- df_red_GL_2050 %>%
  group_by(REMF,YEMF,cate) %>%
  summarize(MAX = max(IAMC_templatePlus))
Error_red_GL2 <- Error_red_GL %>%
  full_join(Error_red_GL_max, by = c("cate","YEMF","REMF"))
Error_red_GL3 <- Error_red_GL2 %>%
  group_by(REMF,YEMF,cate) %>%
  summarize(Error = (sum(Value)/sum(MAX))*100)

color_bar <- c("AgLU"="lightsalmon","AIM-Hub"="lightgrey")

g11 <- df_red_GL_2050 %>%
  #mutate(REMF = factor(REMF, levels = c("OECD+EU","アジア","アフリカ・中東","中南米","旧ソ連地域"))) %>%
  mutate(cate = factor(cate, levels = c("農業","森林再生", "保護", "管理", "農地再生"))) %>%
  mutate(Scenario = factor(Scenario, levels = c("AgLU","AIM-Hub"))) %>%
  ggplot() +
  geom_bar(aes(x=Scenario,y=IAMC_templatePlus/1000,fill=1),stat = "identity",width = 0.8,position = "dodge",size = 0.15,fill="lightsalmon")+
  labs(tag="(a)",title = "2050年", x="", y = expression(paste('Reduction potential (Gt',{CO[2]},'eq/yr)',seq="")))+
  facet_wrap(~cate, nrow = 1, scales = "free_y")+
  theme_bw()+
  #scale_fill_manual(values = color_bar) +
  #scale_pattern_manual(values = c("none","stripe", "circle", "crosshatch", "stripe", "circle", "crosshatch","stripe", "circle", "crosshatch","none", "circle", "crosshatch","stripe", "circle", "none")) + 
  #scale_pattern_density_manual(values = c(0.05, 0.0020, 0.0020, 0.0020, 0.20, 0.40, 0.20, 0.015, 0.025, 0.025, 0.070, 0.070, 0.070, 0.085, 0.270, 0.050)) + 
  #scale_pattern_spacing_manual(values = c(0.05, 0.0141, 0.0171, 0.0141, 0.01, 0.05, 0.02, 0.028, 0.050, 0.025, 0.015, 0.012, 0.015, 0.025, 0.025, 0.025)) +  
  #scale_fill_manual(values = color_bar)+
  #scale_x_discrete(labels = function(x) str_wrap(x, width = 18.5)) +
  theme(plot.title = element_text(size = 20),plot.tag=element_text(size = 18),
        axis.text.x = element_text(size = 21.75,color="black"), axis.text.y = element_text(size = 18.5,color="black"), 
        axis.title.x = element_text(size=17.5), axis.title.y = element_text(size=20.50),
        strip.text.x = element_text(size = 20),legend.text = element_blank(),
        legend.position = "none",legend.title = element_blank(),
        panel.grid.minor = element_line(color = NA), 
        axis.line = element_line(colour="black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        strip.background = element_rect(fill="white", colour="white"))
plot(g11)

g12 <- df_red_GL_2100 %>%
  #mutate(REMF = factor(REMF, levels = c("OECD+EU","アジア","アフリカ・中東","中南米","旧ソ連地域"))) %>%
  mutate(cate = factor(cate, levels = c("農業","森林再生", "保護", "管理", "農地再生"))) %>%
  mutate(Scenario = factor(Scenario, levels = c("AgLU","AIM-Hub"))) %>%
  ggplot() +
  geom_bar(aes(x=Scenario,y=IAMC_templatePlus/1000,fill=1),stat = "identity",width = 0.8,position = "dodge",size = 0.15,fill="lightsalmon")+
  labs(tag="(b)",title = "2100年", x="", y = expression(paste('Reduction potential (Gt',{CO[2]},'eq/yr)',seq="")))+
  facet_wrap(~cate, nrow = 1, scales = "free_y")+
  theme_bw()+
  scale_fill_manual(values = color_bar) +
  #scale_pattern_manual(values = c("none","stripe", "circle", "crosshatch", "stripe", "circle", "crosshatch","stripe", "circle", "crosshatch","none", "circle", "crosshatch","stripe", "circle", "none")) + 
  #scale_pattern_density_manual(values = c(0.05, 0.0020, 0.0020, 0.0020, 0.20, 0.40, 0.20, 0.015, 0.025, 0.025, 0.070, 0.070, 0.070, 0.085, 0.270, 0.050)) + 
  #scale_pattern_spacing_manual(values = c(0.05, 0.0141, 0.0171, 0.0141, 0.01, 0.05, 0.02, 0.028, 0.050, 0.025, 0.015, 0.012, 0.015, 0.025, 0.025, 0.025)) +  
  #scale_fill_manual(values = color_bar)+
  #scale_x_discrete(labels = function(x) str_wrap(x, width = 18.5)) +
  theme(plot.title = element_text(size = 20),plot.tag=element_text(size = 18),
        axis.text.x = element_text(size = 21.75,color="black"), axis.text.y = element_text(size = 18.5,color="black"), 
        axis.title.x = element_text(size=17.5), axis.title.y = element_text(size=20.50),
        strip.text.x = element_text(size = 20),legend.text = element_blank(),
        legend.position = "none",legend.title = element_blank(),
        panel.grid.minor = element_line(color = NA), 
        axis.line = element_line(colour="black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        strip.background = element_rect(fill="white", colour="white"))
plot(g12)

p2 <- df_red_GL_2100%>%
  mutate(Scenario = factor(Scenario, levels = c("AgLU","AIM-Hub"))) %>%
  ggplot(aes(x=cate,y=IAMC_templatePlus/1000,fill=Scenario)) +
  geom_bar(stat = "identity",width = 0.8,position = "dodge",size = 0.15)+
  theme_bw()+
  #scale_linetype_manual(values = linetype_bar) + 
  scale_fill_manual(values = color_bar)+
  theme(plot.title = element_blank(), plot.tag=element_blank(),
        axis.text.x = element_blank(), axis.text.y = element_blank(), 
        axis.title.x = element_blank(), axis.title.y = element_blank(),
        strip.text.x = element_blank(),legend.text = element_text(size =25),
        legend.position = "bottom",legend.title = element_blank(),
        panel.grid.minor = element_blank(), panel.grid = element_blank(),
        axis.line = element_blank(),
        panel.background = element_blank(),axis.ticks.x = element_blank(),axis.ticks.y = element_blank(),
        panel.grid.major = element_blank(),panel.border = element_blank(),
        strip.background = element_blank())

g13 <- (g11 /g12) + plot_layout(widths = c(1, 1), heights = c(1, 1))
plot(g13)
ggsave(file = "../../output/thesis/4.5 AgLU比較/削減量.png", plot=g13,width = 15.85, height = 10.00)
