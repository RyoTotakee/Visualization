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
library(scales)

#------SET-----------------------------------------------------------------------------------------------
df_set_agr_a <- rgdx.param("../../data/thesis/IAMC_templatePlus_CGE_limit_a.gdx", "IAMC_templatePlus")
df_set_agr_b <- rgdx.param("../../data/thesis/IAMC_templatePlus_CGE_limit_b.gdx", "IAMC_templatePlus")
df_set_agr <- full_join(df_set_agr_a, df_set_agr_b, by = c("REMF", "Scenario", "YEMF","VEMF","IAMC_templatePlus"))
df_set_agr <- df_set_agr %>%
  group_by(REMF,Scenario,YEMF,VEMF) %>%
  summarise(IAMC_templatePlus = sum(IAMC_templatePlus, na.rm = TRUE))
df_set_agr <- df_set_agr %>%
  filter(Scenario %in% c("SSP2_CM_NoCC")) %>%
  filter(VEMF %in% c("Emi_Red_equ_CH4_AFO_Agr_Liv_Ent_Fer_Bre","Emi_Red_equ_CH4_AFO_Agr_Liv_Ent_Fer_Fee","Emi_Red_equ_CH4_AFO_Agr_Liv_Man_Man_Cad","Emi_Red_equ_CH4_AFO_Agr_Liv_Man_Man_And","Emi_Red_equ_CH4_AFO_Agr_Liv_Man_Man_Cfl","Emi_Red_equ_CH4_AFO_Agr_Liv_Man_Man_Adc","Emi_Red_equ_CH4_AFO_Agr_Liv_Man_Man_Ams","Emi_Red_equ_CH4_AFO_Agr_Ric_Wat","Emi_Red_equ_CH4_AFO_Agr_Ric_Res","Emi_Red_equ_N2O_AFO_Agr_Man_Soi_Til","Emi_Red_equ_N2O_AFO_Agr_Man_Soi_Fer","Emi_Red_equ_N2O_AFO_Agr_Man_Soi_Res","Emi_Red_equ_N2O_AFO_Agr_Man_Soi_Slo","Emi_Red_equ_N2O_AFO_Agr_Man_Soi_Nin",
                     "Red_Cos_AFO_Agr_Liv_Ent_Fer_Bre","Red_Cos_AFO_Agr_Liv_Ent_Fer_Fee","Red_Cos_AFO_Agr_Liv_Man_Man_Cad","Red_Cos_AFO_Agr_Liv_Man_Man_And",
                     "Red_Cos_AFO_Agr_Liv_Man_Man_Cfl","Red_Cos_AFO_Agr_Liv_Man_Man_Adc","Red_Cos_AFO_Agr_Liv_Man_Man_Ams","Red_Cos_AFO_Agr_Ric_Res",
                     "Red_Cos_AFO_Agr_Man_Soi_Fer","Red_Cos_AFO_Agr_Man_Soi_Res","Red_Cos_AFO_Agr_Man_Soi_Slo","Red_Cos_AFO_Agr_Man_Soi_Nin",
                     "Emi_equ_CH4_AFO_Agr_Liv_Ent_Fer","Emi_equ_CH4_AFO_Agr_Liv_Man_Man","Emi_equ_CH4_AFO_Agr_Ric","Emi_equ_N2O_AFO_Agr_Man_Soi"))

df_set_lu_a <- rgdx.param("../../data/thesis/IAMC_templatePlus_CGE_nolimit_a.gdx", "IAMC_templatePlus")
df_set_lu_b <- rgdx.param("../../data/thesis/IAMC_templatePlus_CGE_nolimit_b.gdx", "IAMC_templatePlus")
df_set_lu <- full_join(df_set_lu_a, df_set_lu_b, by = c("REMF", "Scenario", "YEMF","VEMF","IAMC_templatePlus"))
df_set_lu <- df_set_lu %>%
  group_by(REMF,Scenario,YEMF,VEMF) %>%
  summarise(IAMC_templatePlus = sum(IAMC_templatePlus, na.rm = TRUE))
df_set_lu <- df_set_lu %>%
  filter(Scenario %in% c("SSP2_CM_NoCC")) %>%
  filter(VEMF %in% c("Emi_Red_CO2_AFO_Lan_Pla","Emi_Red_CO2_AFO_Lan_Ref","Emi_Red_CO2_AFO_Lan_FPr","Emi_Red_CO2_AFO_Lan_Pff","Emi_Red_CO2_AFO_Lan_Ril","Emi_Red_CO2_AFO_Lan_Enr","Emi_Red_CO2_AFO_Lan_Agf","Emi_Red_CO2_AFO_Lan_Bch","Emi_Red_CO2_AFO_Lan_Scs",
                     "Red_Cos_AFO_Lan_Pla","Red_Cos_AFO_Lan_Ref","Red_Cos_AFO_Lan_FPr","Red_Cos_AFO_Lan_Pff","Red_Cos_AFO_Lan_Ril","Red_Cos_AFO_Lan_Enr","Red_Cos_AFO_Lan_Agf",
                     "Red_Cos_AFO_Lan_Bch","Red_Cos_AFO_Lan_Scc","Red_Cos_AFO_Lan_Scg",
                     "Emi_CO2_Lan_Use_Flo_Pos_Emi_Lan_Use_Cha","Emi_CO2_Lan_Use_Flo_RemForest","Emi_CO2_AFO_Lan_For_Gra_Con","Emi_CO2_Lan_Use_Flo_Pos_Emi_Fir"))

Load <- full_join(df_set_agr, df_set_lu, by = c("REMF", "Scenario", "YEMF","VEMF","IAMC_templatePlus"))

df_set_red <- Load %>%
  filter(!YEMF %in% c("2005"))%>%
  filter(VEMF %in% c("Emi_Red_equ_CH4_AFO_Agr_Liv_Ent_Fer_Bre","Emi_Red_equ_CH4_AFO_Agr_Liv_Ent_Fer_Fee","Emi_Red_equ_CH4_AFO_Agr_Liv_Man_Man_Cad","Emi_Red_equ_CH4_AFO_Agr_Liv_Man_Man_And","Emi_Red_equ_CH4_AFO_Agr_Liv_Man_Man_Cfl","Emi_Red_equ_CH4_AFO_Agr_Liv_Man_Man_Adc",
                     "Emi_Red_equ_CH4_AFO_Agr_Liv_Man_Man_Ams","Emi_Red_equ_CH4_AFO_Agr_Ric_Wat","Emi_Red_equ_CH4_AFO_Agr_Ric_Res","Emi_Red_equ_N2O_AFO_Agr_Man_Soi_Til","Emi_Red_equ_N2O_AFO_Agr_Man_Soi_Fer","Emi_Red_equ_N2O_AFO_Agr_Man_Soi_Res","Emi_Red_equ_N2O_AFO_Agr_Man_Soi_Slo","Emi_Red_equ_N2O_AFO_Agr_Man_Soi_Nin",
                     "Emi_Red_CO2_AFO_Lan_Pla","Emi_Red_CO2_AFO_Lan_Ref","Emi_Red_CO2_AFO_Lan_FPr","Emi_Red_CO2_AFO_Lan_Pff","Emi_Red_CO2_AFO_Lan_Ril","Emi_Red_CO2_AFO_Lan_Enr","Emi_Red_CO2_AFO_Lan_Agf","Emi_Red_CO2_AFO_Lan_Bch","Emi_Red_CO2_AFO_Lan_Scs")) %>% 
  mutate(VEMF=recode(VEMF,Emi_Red_equ_CH4_AFO_Agr_Liv_Ent_Fer_Bre="生産性が高い家畜種への変更",Emi_Red_equ_CH4_AFO_Agr_Liv_Ent_Fer_Fee="濃厚飼料の投与",Emi_Red_equ_CH4_AFO_Agr_Liv_Man_Man_Cad="ラグーンカバー",Emi_Red_equ_CH4_AFO_Agr_Liv_Man_Man_And="嫌気性消化(大規模プラント)",
                     Emi_Red_equ_CH4_AFO_Agr_Liv_Man_Man_Cfl="嫌気性消化(小規模)",Emi_Red_equ_CH4_AFO_Agr_Liv_Man_Man_Adc="糞尿堆肥化",Emi_Red_equ_CH4_AFO_Agr_Liv_Man_Man_Ams="窒素肥料の硫酸アンモニウムへの変更",Emi_Red_equ_CH4_AFO_Agr_Ric_Wat="水田への間断灌漑の導入",Emi_Red_equ_CH4_AFO_Agr_Ric_Res="稲わらのすき込み",
                     Emi_Red_equ_N2O_AFO_Agr_Man_Soi_Til="不耕起移植栽培",Emi_Red_equ_N2O_AFO_Agr_Man_Soi_Fer="肥料の高効率利用",Emi_Red_equ_N2O_AFO_Agr_Man_Soi_Res="化学肥料の有機肥料による置き換え",Emi_Red_equ_N2O_AFO_Agr_Man_Soi_Slo="被覆肥料の利用",Emi_Red_equ_N2O_AFO_Agr_Man_Soi_Nin="硝化抑制剤を含む肥料の利用",
                     Emi_Red_CO2_AFO_Lan_Pla="植林",Emi_Red_CO2_AFO_Lan_Ref="再植林",Emi_Red_CO2_AFO_Lan_FPr="森林保護",Emi_Red_CO2_AFO_Lan_Pff="森林火災の防止","Emi_Red_CO2_AFO_Lan_Ril"="環境負荷の少ない伐採",Emi_Red_CO2_AFO_Lan_Enr="自然再生の強化",Emi_Red_CO2_AFO_Lan_Agf="アグロフォレストリー",
                     Emi_Red_CO2_AFO_Lan_Bch="バイオ炭",Emi_Red_CO2_AFO_Lan_Scs="土壌炭素隔離"))%>%
  mutate(main = case_when(
    VEMF == "植林" ~ "CO2-LULUCF",
    VEMF == "森林保護" ~ "CO2-LULUCF",
    VEMF == "環境負荷の少ない伐採" ~ "CO2-LULUCF",
    VEMF == "自然再生の強化" ~ "CO2-LULUCF",
    VEMF == "森林火災の防止" ~ "CO2-LULUCF",
    VEMF == "アグロフォレストリー" ~ "CO2-LULUCF",
    VEMF == "バイオ炭" ~ "CO2-LULUCF",
    VEMF == "土壌炭素隔離" ~ "CO2-LULUCF",
    VEMF == "生産性が高い家畜種への変更" ~ "CH4-反芻",
    VEMF == "濃厚飼料の投与" ~ "CH4-反芻",
    VEMF == "ラグーンカバー" ~ "CH4-糞尿",
    VEMF == "嫌気性消化(大規模プラント)" ~ "CH4-糞尿",
    VEMF == "嫌気性消化(小規模)" ~ "CH4-糞尿",
    VEMF == "糞尿堆肥化" ~ "CH4-糞尿",
    VEMF == "窒素肥料の硫酸アンモニウムへの変更" ~ "CH4-稲作",
    VEMF == "水田への間断灌漑の導入" ~ "CH4-稲作",
    VEMF == "稲わらのすき込み" ~ "CH4-稲作",
    VEMF == "不耕起移植栽培" ~ "N2O-土壌",
    VEMF == "肥料の高効率利用" ~ "N2O-土壌",
    VEMF == "化学肥料の有機肥料による置き換え" ~ "N2O-土壌",
    VEMF == "被覆肥料の利用" ~ "N2O-土壌",
    VEMF == "硝化抑制剤を含む肥料の利用" ~ "N2O-土壌"))%>% 
  mutate(IAMC_templatePlus = ifelse(IAMC_templatePlus < 0, -IAMC_templatePlus, IAMC_templatePlus))%>% 
  filter(!is.na(IAMC_templatePlus)) %>% 
  rename(RED=IAMC_templatePlus)

comb <- expand.grid(
  REMF = unique(Load$REMF), 
  Scenario = unique(Load$Scenario),
  YEMF = unique(Load$YEMF)
)

Wat <- data.frame(
  REMF = comb$REMF,
  Scenario = comb$Scenario,
  YEMF = comb$YEMF,
  VEMF = "水田への間断灌漑の導入",
  IAMC_templatePlus = 0
)

Til <- data.frame(
  REMF = comb$REMF,
  Scenario = comb$Scenario,
  YEMF = comb$YEMF,
  VEMF = "不耕起移植栽培",
  IAMC_templatePlus = 0
)

Scs <- Load %>%
  filter(VEMF %in% c("Red_Cos_AFO_Lan_Scc","Red_Cos_AFO_Lan_Scg")) %>%
  group_by(Scenario,REMF,YEMF) %>%
  summarize(IAMC_templatePlus = sum(IAMC_templatePlus),
            VEMF = "土壌炭素隔離",
            .groups = 'drop')

df_set_cos <- Load %>%
  rbind(Til) %>%
  rbind(Scs) %>%
  rbind(Wat) %>% 
  filter(!YEMF %in% c("2005","2010","2020"))%>%
  filter(VEMF %in% c("Red_Cos_AFO_Agr_Liv_Ent_Fer_Bre","Red_Cos_AFO_Agr_Liv_Ent_Fer_Fee","Red_Cos_AFO_Agr_Liv_Man_Man_Cad","Red_Cos_AFO_Agr_Liv_Man_Man_And",
                     "Red_Cos_AFO_Agr_Liv_Man_Man_Cfl","Red_Cos_AFO_Agr_Liv_Man_Man_Adc","Red_Cos_AFO_Agr_Liv_Man_Man_Ams","Red_Cos_AFO_Agr_Ric_Res",
                     "Red_Cos_AFO_Agr_Man_Soi_Fer","Red_Cos_AFO_Agr_Man_Soi_Res","Red_Cos_AFO_Agr_Man_Soi_Slo","Red_Cos_AFO_Agr_Man_Soi_Nin",
                     "Red_Cos_AFO_Lan_Pla","Red_Cos_AFO_Lan_Ref","Red_Cos_AFO_Lan_FPr","Red_Cos_AFO_Lan_Pff","Red_Cos_AFO_Lan_Ril","Red_Cos_AFO_Lan_Enr","Red_Cos_AFO_Lan_Agf",
                     "Red_Cos_AFO_Lan_Bch","土壌炭素隔離","水田への間断灌漑の導入","不耕起移植栽培")) %>% 
  mutate(VEMF=recode(VEMF,Red_Cos_AFO_Agr_Liv_Ent_Fer_Bre="生産性が高い家畜種への変更",Red_Cos_AFO_Agr_Liv_Ent_Fer_Fee="濃厚飼料の投与",Red_Cos_AFO_Agr_Liv_Man_Man_Cad="ラグーンカバー",
                     Red_Cos_AFO_Agr_Liv_Man_Man_And="嫌気性消化(大規模プラント)",Red_Cos_AFO_Agr_Liv_Man_Man_Cfl="嫌気性消化(小規模)",Red_Cos_AFO_Agr_Liv_Man_Man_Adc="糞尿堆肥化",
                     Red_Cos_AFO_Agr_Liv_Man_Man_Ams="窒素肥料の硫酸アンモニウムへの変更",Red_Cos_AFO_Agr_Ric_Res="稲わらのすき込み",Red_Cos_AFO_Agr_Man_Soi_Fer="肥料の高効率利用",
                     Red_Cos_AFO_Agr_Man_Soi_Res="化学肥料の有機肥料による置き換え",Red_Cos_AFO_Agr_Man_Soi_Slo="被覆肥料の利用",Red_Cos_AFO_Agr_Man_Soi_Nin="硝化抑制剤を含む肥料の利用",
                     Red_Cos_AFO_Lan_Pla="植林",Red_Cos_AFO_Lan_Ref="再植林",Red_Cos_AFO_Lan_FPr="森林保護",Red_Cos_AFO_Lan_Pff="森林火災の防止","Red_Cos_AFO_Lan_Ril"="環境負荷の少ない伐採",Red_Cos_AFO_Lan_Enr="自然再生の強化",
                     Red_Cos_AFO_Lan_Agf="アグロフォレストリー",Red_Cos_AFO_Lan_Bch="バイオ炭")) %>% 
  mutate(main = case_when(
    VEMF == "植林" ~ "CO2-LULUCF",
    VEMF == "森林保護" ~ "CO2-LULUCF",
    VEMF == "環境負荷の少ない伐採" ~ "CO2-LULUCF",
    VEMF == "自然再生の強化" ~ "CO2-LULUCF",
    VEMF == "森林火災の防止" ~ "CO2-LULUCF",
    VEMF == "アグロフォレストリー" ~ "CO2-LULUCF",
    VEMF == "バイオ炭" ~ "CO2-LULUCF",
    VEMF == "土壌炭素隔離" ~ "CO2-LULUCF",
    VEMF == "生産性が高い家畜種への変更" ~ "CH4-反芻",
    VEMF == "濃厚飼料の投与" ~ "CH4-反芻",
    VEMF == "ラグーンカバー" ~ "CH4-糞尿",
    VEMF == "嫌気性消化(大規模プラント)" ~ "CH4-糞尿",
    VEMF == "嫌気性消化(小規模)" ~ "CH4-糞尿",
    VEMF == "糞尿堆肥化" ~ "CH4-糞尿",
    VEMF == "窒素肥料の硫酸アンモニウムへの変更" ~ "CH4-稲作",
    VEMF == "水田への間断灌漑の導入" ~ "CH4-稲作",
    VEMF == "稲わらのすき込み" ~ "CH4-稲作",
    VEMF == "不耕起移植栽培" ~ "N2O-土壌",
    VEMF == "肥料の高効率利用" ~ "N2O-土壌",
    VEMF == "化学肥料の有機肥料による置き換え" ~ "N2O-土壌",
    VEMF == "被覆肥料の利用" ~ "N2O-土壌",
    VEMF == "硝化抑制剤を含む肥料の利用" ~ "N2O-土壌"))%>% 
  mutate(IAMC_templatePlus = ifelse(IAMC_templatePlus < 0, -IAMC_templatePlus, IAMC_templatePlus))%>% 
  filter(!is.na(IAMC_templatePlus)) %>% 
  rename(Cost=IAMC_templatePlus)

#Emission
df_set_emi <- Load %>%
  filter(VEMF %in% c("Emi_equ_CH4_AFO_Agr_Liv_Ent_Fer","Emi_equ_CH4_AFO_Agr_Liv_Man_Man","Emi_equ_CH4_AFO_Agr_Ric","Emi_equ_N2O_AFO_Agr_Man_Soi",
                     "Emi_CO2_Lan_Use_Flo_Pos_Emi_Lan_Use_Cha","Emi_CO2_Lan_Use_Flo_RemForest","Emi_CO2_AFO_Lan_For_Gra_Con","Emi_CO2_Lan_Use_Flo_Pos_Emi_Fir"))%>%
  mutate(VEMF = case_when(
    VEMF == "Emi_CO2_AFO_Lan_For_Gra_Con" ~ "Emi_CO2_Lan_Use_Flo_Pos_Emi_Lan_Use_Cha",
    TRUE ~ VEMF
  )) %>% 
  mutate(VEMF=recode(VEMF,Emi_equ_CH4_AFO_Agr_Liv_Ent_Fer="CH4-反芻","Emi_equ_CH4_AFO_Agr_Liv_Man_Man"="CH4-糞尿",Emi_equ_CH4_AFO_Agr_Ric="CH4-稲作",Emi_equ_N2O_AFO_Agr_Man_Soi="N2O-土壌"))
df_set_emi <- distinct(df_set_emi)
df_LU <- df_set_emi %>%
  filter(VEMF %in% c("Emi_CO2_Lan_Use_Flo_Pos_Emi_Lan_Use_Cha","Emi_CO2_Lan_Use_Flo_Pos_Emi_Fir","Emi_CO2_Lan_Use_Flo_RemForest")) %>%
  group_by(Scenario,REMF,YEMF) %>%
  summarize(IAMC_templatePlus = sum(IAMC_templatePlus),
            VEMF = "CO2-LULUCF",
            .groups = 'drop')
df_set_emi <- bind_rows(
  df_set_emi %>% filter(!VEMF %in% c("Emi_CO2_Lan_Use_Flo_Pos_Emi_Lan_Use_Cha","Emi_CO2_Lan_Use_Flo_Pos_Emi_Fir","Emi_CO2_Lan_Use_Flo_RemForest")),
  df_LU)



#-----mac---------------------------------------------------------------------------------------------
df_mac <- full_join(df_set_red, df_set_cos, by = c("REMF","YEMF","VEMF","main","Scenario"))
df_mac_R178 <- df_mac %>%
  filter(!REMF %in% c("XE25","XER","XOC","XSA","XSE","XLM","XNF","XAF","CIS","XME","R5REF","R5LAM","R5ASIA","R5OECD90+EU","R5MAF","World","BHR","BDI","GMB","PSE")) %>%
  filter(!is.na(RED))%>%
  mutate(
    Cost = as.numeric(Cost),
    RED = as.numeric(RED)
  )%>%
  group_by(REMF,VEMF,main) %>%
  mutate(unitcost = ((sum(Cost))/sum(RED)))
df_mac_R178_2100 <- df_mac_R178 %>%
  ungroup() %>% 
  filter(YEMF %in% c("2100","2050")) %>% 
  group_by(YEMF) %>%
  arrange(unitcost) %>% 
  mutate(
    cum_potential = cumsum(RED)
  ) %>% 
  mutate(red_rate=cum_potential/15868.1482955627)

g1 <- df_mac_R178_2100 %>%
  #mutate(VEMF = factor(VEMF, levels = c("家畜反芻","糞尿管理","稲作","農耕地土壌"))) %>%
  mutate(main = factor(main, levels = c("CH4-反芻","CH4-糞尿","CH4-稲作","N2O-土壌","CO2-LULUCF"))) %>%
  ggplot() +
  #geom_line(aes(x = red_rate, y = unitcost/1000),
  #          linewidth = 0.8,
  #          color = "salmon") +
  geom_point(aes(x=red_rate,y=unitcost/1000,color=main), size = 2)+
  facet_wrap(~YEMF, nrow = 1, scales = "free")+
  scale_color_manual(
    values = color_set,
    labels = c(
      expression(paste(CH[4], "-反芻")),
      expression(paste(CH[4], "-糞尿")),
      expression(paste(CH[4], "-稲作")),
      expression(paste(N[2], O, "-土壌")),
      expression(paste(CO[2], "-LULUCF"))
    )
  )+
  scale_x_continuous( limits = c(0, 1),
                      labels = percent_format(accuracy = 1)) +
  #scale_y_continuous(labels = label_number()) +
  labs(tag = "(a)", x="Reduction (%)", y =expression(paste('Cost (1000$/t',{CO[2]},'eq)',seq="")))+
  theme_bw()+
  #geom_text(aes(label = REMF), vjust = -1, hjust = 0.5, size = 4) +
  scale_color_manual(values = color_set)+
  theme(plot.title = element_blank(), plot.tag=element_text(size = 18),
        axis.text.x = element_text(size = 16,color="black"), axis.text.y = element_text(size = 16,color="black"), 
        axis.title.x = element_text(size=21.5), axis.title.y = element_text(size=21.5),
        strip.text.x = element_text(size = 21.5),legend.text = element_text(size =21.5),
        legend.position = "bottom",legend.title = element_blank(),
        panel.grid.minor = element_line(color = NA), 
        axis.line = element_line(colour="black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        strip.background = element_rect(fill="white", colour="white"))
plot(g1)

ggsave(file = "../../output/thesis/mac/mac.png", plot=g1, width = 15.5, height = 7.5)

df_set_emi_R178 <- df_set_emi %>%
  filter(!REMF %in% c("XE25","XER","XOC","XSA","XSE","XLM","XNF","XAF","CIS","XME","R5REF","R5LAM","R5ASIA","R5OECD90+EU","R5MAF","World","BHR","BDI","GMB","PSE")) %>% 
  filter(!YEMF %in% c("2005","2010","2020"))  %>% 
  rename(Emi=IAMC_templatePlus)%>% 
  rename(main=VEMF)%>%
  mutate(
    Emi = as.numeric(Emi)
  ) %>%
  group_by(Scenario,YEMF,main) %>%
  summarise(Emi = sum(Emi, na.rm = TRUE))

df_mac2_R178 <- left_join(df_mac_R178, df_set_emi_R178, by = c("YEMF","main","Scenario"))

df_mac_R178_tech_2100 <- df_mac2_R178 %>%
  filter(!main %in% c("CO2-LULUCF"))  %>% 
  filter(YEMF %in% c("2100","2050")) %>% 
  ungroup() %>% 
  group_by(main,YEMF) %>%
  arrange(unitcost) %>% 
  mutate(
    cum_potential = cumsum(RED)
  ) %>% 
  mutate(red_rate=cum_potential/Emi)

color_set <- c(
  "CH4-反芻" = "#1f77b4",       # 青
  "CH4-糞尿" = "#ff7f0e",       # オレンジ
  "CH4-稲作" = "#2ca02c",       # 緑
  "N2O-土壌" = "#d62728",       # 赤
  "CO2-LULUCF" = "#8c564b"       # 紫
)

g2 <- df_mac_R178_tech_2100 %>%
  mutate(main = factor(main, levels = c("CH4-反芻","CH4-糞尿","CH4-稲作","N2O-土壌"))) %>%
  #mutate(cate = factor(cate, levels = c("反芻対策","糞尿対策","稲作対策","土壌対策","森林再生", "保護", "管理", "農地再生"))) %>%
  ggplot() +
  geom_point(aes(x=red_rate,y=unitcost/1000,color=main), size = 1.5)+
  geom_line(aes(x = red_rate, y = unitcost/1000,color=main),
            linewidth = 0.8) +
  facet_wrap(~YEMF, nrow = 1, scales = "free")+
  scale_x_continuous( limits = c(0, 1),
                      labels = percent_format(accuracy = 1)) +
  labs(tag = "(b)", x=expression(paste('Non-',{CO[2]}, ' Reduction (%)',seq="")), y =expression(paste('Cost (1000$/t',{CO[2]},'eq)',seq="")))+
  theme_bw()+
  #geom_text(aes(label = REMF), vjust = -1, hjust = 0.5, size = 4) +
  scale_color_manual(
    values = color_set,
    labels = c(
      expression(paste(CH[4], "-反芻")),
      expression(paste(CH[4], "-糞尿")),
      expression(paste(CH[4], "-稲作")),
      expression(paste(N[2], O, "-土壌"))
    )
  ) +
  theme(plot.title = element_blank(), plot.tag=element_text(size = 18),
        axis.text.x = element_text(size = 16,color="black"), axis.text.y = element_text(size = 16,color="black"), 
        axis.title.x = element_text(size=21.5), axis.title.y = element_text(size=21.5),
        strip.text.x = element_text(size = 21.5),legend.text = element_text(size =21.5),
        legend.position = "bottom",legend.title = element_blank(),
        panel.grid.minor = element_line(color = NA), 
        axis.line = element_line(colour="black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        strip.background = element_rect(fill="white", colour="white"))
plot(g2)

ggsave(file = "../../output/thesis/mac/mac_tech.png", plot=g2, width = 15.5, height = 7.5)


g3 <- (g1/ g2) + plot_layout(widths = c(1,1), heights = c(1, 1))
plot(g3)

ggsave(file = "../../output/thesis/mac/mac_agg.png", plot=g3, width = 15.5, height = 12.5)

df_mac_R178_tech_LU_2100 <- df_mac2_R178 %>%
  filter(main %in% c("CO2-LULUCF"))  %>% 
  filter(YEMF %in% c("2100","2050")) %>% 
  mutate(main_LU = case_when(
    VEMF == "植林" ~ "森林再生",
    VEMF == "森林保護" ~ "保護",
    VEMF == "環境負荷の少ない伐採" ~ "保護",
    VEMF == "自然再生の強化" ~ "管理",
    VEMF == "森林火災の防止" ~ "管理",
    VEMF == "アグロフォレストリー" ~ "農地再生",
    VEMF == "バイオ炭" ~ "農地再生",
    VEMF == "土壌炭素隔離" ~ "農地再生")) %>% 
  ungroup() %>% 
  group_by(YEMF,main_LU) %>%
  arrange(unitcost) %>% 
  mutate(
    cum_potential = cumsum(RED)
  ) %>% 
  mutate(red_rate=cum_potential/Emi) 

color_bar_c <- c(
  "森林再生" = "#66c2a5",           # ラベンダーブルー
  "保護" = "#a65628",        # 濃緑
  "管理" = "salmon2",      # 明るい緑              # 中間グレー（炭）
  "農地再生" = "#a6d854"          # 黄緑（自然的）
)


g4 <- df_mac_R178_tech_LU_2100 %>%
  mutate(main_LU = factor(main_LU, levels = c("森林再生", "保護", "管理", "農地再生"))) %>%
  #mutate(cate = factor(cate, levels = c("反芻対策","糞尿対策","稲作対策","土壌対策","森林再生", "保護", "管理", "農地再生"))) %>%
  ggplot() +
  geom_point(aes(x=cum_potential/1000,y=unitcost/1000,color=main_LU), size = 1.5)+
  geom_line(aes(x = cum_potential/1000, y = unitcost/1000,color=main_LU),
            linewidth = 0.8) +
  facet_wrap(~YEMF, nrow = 1, scales = "free_y")+
  #scale_x_continuous( limits = c(0, 1),
  #                    labels = percent_format(accuracy = 1)) +
  labs(tag = "(c)", x=expression(paste('',{CO[2]},' Reduction Potential (Gt',{CO[2]},'eq/yr)',seq="")), y =expression(paste('Cost (1000$/t',{CO[2]},'eq)',seq="")))+
  theme_bw()+
  #geom_text(aes(label = REMF), vjust = -1, hjust = 0.5, size = 4) +
  scale_color_manual(
    values = color_bar_c
  ) +
  theme(plot.title = element_blank(), plot.tag=element_text(size = 18),
        axis.text.x = element_text(size = 16,color="black"), axis.text.y = element_text(size = 16,color="black"), 
        axis.title.x = element_text(size=21.5), axis.title.y = element_text(size=21.5),
        strip.text.x = element_text(size = 21.5),legend.text = element_text(size =21.5),
        legend.position = "bottom",legend.title = element_blank(),
        panel.grid.minor = element_line(color = NA), 
        axis.line = element_line(colour="black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        strip.background = element_rect(fill="white", colour="white"))
plot(g4)

ggsave(file = "../../output/thesis/mac/mac_tech_LU.png", plot=g4, width = 15.5, height = 7.5)

g5 <- (g1/ g2 /g4) + plot_layout(widths = c(1,1,1), heights = c(1, 1, 1))
plot(g5)

ggsave(file = "../../output/thesis/mac/mac_agg3.png", plot=g5, width = 15.5, height = 15.5)

#-----mac(地域)---------------------------------------------------------------------------------------------
map_data_agg <- read.table("../../data/Analysis_lu/region17_agg.map", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
map_data_r17 <- read.table("../../data/Analysis_lu/region_iso_17.map", header = TRUE, sep = "\t", stringsAsFactors = FALSE)

df_set_emi2_R178 <- df_set_emi %>%
  filter(REMF %in% c("R5REF","R5LAM","R5ASIA","R5OECD90+EU","R5MAF")) %>% 
  filter(!YEMF %in% c("2005","2010","2020"))  %>% 
  rename(Emi=IAMC_templatePlus)%>% 
  rename(main=VEMF)%>%
  rename(agg=REMF)%>%
  mutate(
    Emi = as.numeric(Emi)
  ) %>%
  group_by(Scenario,YEMF,agg) %>%
  summarise(Emi = sum(Emi, na.rm = TRUE))

df_set_R5 <- left_join(map_data_r17, df_mac_R178, by = c("X.ISO" = "REMF"))
df_set_R5 <- subset(df_set_R5, select = -X)
df_set_R5 <- subset(df_set_R5, select = -X.asterisks.for.map)
df_set_R5 <- df_set_R5 %>%
  mutate(agg = case_when(
    AIM17 == "XE25" ~ "R5OECD90+EU",
    AIM17 == "XER" ~ "R5OECD90+EU",
    AIM17 == "TUR" ~ "R5OECD90+EU",
    AIM17 == "JPN" ~ "R5OECD90+EU",
    AIM17 == "CAN" ~ "R5OECD90+EU",
    AIM17 == "USA" ~ "R5OECD90+EU",
    AIM17 == "CHN" ~ "R5ASIA",
    AIM17 == "IND" ~ "R5ASIA",
    AIM17 == "XSE" ~ "R5ASIA",
    AIM17 == "XSA" ~ "R5ASIA",
    AIM17 == "BRA" ~ "R5LAM",
    AIM17 == "XLM" ~ "R5LAM",
    AIM17 == "CIS" ~ "R5REF",
    AIM17 == "XME" ~ "R5MAF",
    AIM17 == "XNF" ~ "R5MAF",
    AIM17 == "XAF" ~ "R5MAF",
    AIM17 == "XOC" ~ "R5OECD90+EU"
  )) %>% 
  rename("REMF"="X.ISO")

df_mac3_R178 <- left_join(df_set_R5, df_set_emi2_R178, by = c("YEMF","agg","Scenario"))

df_mac_R5_2100 <- df_mac3_R178 %>%
  filter(!main %in% c("CO2-LULUCF"))  %>% 
  filter(YEMF %in% c("2100","2050")) %>% 
  ungroup() %>% 
  group_by(agg,YEMF) %>%
  arrange(unitcost) %>% 
  mutate(
    cum_potential = cumsum(RED)
  ) %>% 
  mutate(red_rate=cum_potential/Emi)%>%
  mutate(agg=recode(agg,R5REF="旧ソ連地域",R5LAM="中南米",R5ASIA="アジア",R5MAF="アフリカ・中東","R5OECD90+EU"="OECD+EU"))


color_bar <- c(
  "OECD+EU"        = "salmon2",        # lightsalmonより少し締めた赤橙
  "アフリカ・中東" = "deepskyblue3",  # lightskyblue3よりコントラスト↑
  "アジア"          = "sandybrown",    # peruより明るく可読性↑
  "中南米"          = "olivedrab3",    # darkolivegreen2より分離良し
  "旧ソ連地域"       = "grey40"        # grey50よりやや濃く
)

g4 <- df_mac_R5_2100 %>%
  mutate(agg = factor(agg, levels = c("OECD+EU","アジア","アフリカ・中東","中南米","旧ソ連地域"))) %>%
  #mutate(cate = factor(cate, levels = c("反芻対策","糞尿対策","稲作対策","土壌対策","森林再生", "保護", "管理", "農地再生"))) %>%
  ggplot() +
  geom_point(aes(x=cum_potential/1000,y=unitcost/1000,color=agg), size = 1.5)+
  geom_line(aes(x = cum_potential/1000, y = unitcost/1000,color=agg),
            linewidth = 0.8) +
  facet_wrap(~YEMF, nrow = 1, scales = "free_y")+
  #scale_x_continuous( limits = c(0, 1),
  #                    labels = percent_format(accuracy = 1)) +
  labs(tag = "(c)", x=expression(paste('Reduction potential (Gt',{CO[2]},'eq/yr)',seq="")), y =expression(paste('Cost (1000$/t',{CO[2]},'eq)',seq="")))+
  theme_bw()+
  #geom_text(aes(label = REMF), vjust = -1, hjust = 0.5, size = 4) +
  scale_color_manual(
    values = color_bar
  ) +
  theme(plot.title = element_blank(), plot.tag=element_text(size = 18),
        axis.text.x = element_text(size = 16,color="black"), axis.text.y = element_text(size = 16,color="black"), 
        axis.title.x = element_text(size=21.5), axis.title.y = element_text(size=21.5),
        strip.text.x = element_text(size = 21.5),legend.text = element_text(size =21.5),
        legend.position = "bottom",legend.title = element_blank(),
        panel.grid.minor = element_line(color = NA), 
        axis.line = element_line(colour="black"),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        strip.background = element_rect(fill="white", colour="white"))
plot(g4)

ggsave(file = "../../output/thesis/mac/mac_R5.png", plot=g4, width = 15.5, height = 7.5)

g5 <- (g1/ g2 /g4) + plot_layout(widths = c(1,1,1), heights = c(1, 1, 1))
plot(g5)

ggsave(file = "../../output/thesis/mac/mac_agg2.png", plot=g5, width = 15.5, height = 15.5)
