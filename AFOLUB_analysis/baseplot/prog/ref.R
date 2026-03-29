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

#------SET-----------------------------------------------------------------------------------------------
Load <- rgdx.param("../../data/Result/IAMC_templatePlus.gdx", "IAMC_templatePlus")
map_data_r17 <- read.table("../../data/Analysis_lu/region_iso_17.map", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
#Reduction
df_set_red <- Load %>%
  filter(Scenario %in% c("SSP2_CM_NoCC")) %>%
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
    VEMF == "植林" ~ "LULUCF(CDR)",
    VEMF == "再植林" ~ "LULUCF(CDR)",
    VEMF == "森林保護" ~ "LULUCF(その他)",
    VEMF == "環境負荷の少ない伐採" ~ "LULUCF(その他)",
    VEMF == "自然再生の強化" ~ "LULUCF(その他)",
    VEMF == "森林火災の防止" ~ "LULUCF(その他)",
    VEMF == "アグロフォレストリー" ~ "LULUCF(CDR)",
    VEMF == "バイオ炭" ~ "LULUCF(CDR)",
    VEMF == "土壌炭素隔離" ~ "LULUCF(CDR)",
    VEMF == "生産性が高い家畜種への変更" ~ "反芻対策",
    VEMF == "濃厚飼料の投与" ~ "反芻対策",
    VEMF == "ラグーンカバー" ~ "糞尿対策",
    VEMF == "嫌気性消化(大規模プラント)" ~ "糞尿対策",
    VEMF == "嫌気性消化(小規模)" ~ "糞尿対策",
    VEMF == "糞尿堆肥化" ~ "糞尿対策",
    VEMF == "窒素肥料の硫酸アンモニウムへの変更" ~ "稲作対策",
    VEMF == "水田への間断灌漑の導入" ~ "稲作対策",
    VEMF == "稲わらのすき込み" ~ "稲作対策",
    VEMF == "不耕起移植栽培" ~ "土壌対策",
    VEMF == "肥料の高効率利用" ~ "土壌対策",
    VEMF == "化学肥料の有機肥料による置き換え" ~ "土壌対策",
    VEMF == "被覆肥料の利用" ~ "土壌対策",
    VEMF == "硝化抑制剤を含む肥料の利用" ~ "土壌対策")) %>% 
  mutate(category = case_when(
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
  )) %>% 
  mutate(sourse = case_when(
    main == "反芻対策" ~ "農業",
    main == "糞尿対策" ~ "農業",
    main == "稲作対策" ~ "農業",
    main == "土壌対策" ~ "農業",
    main == "LULUCF(CDR)" ~ "LULUCF",
    main == "LULUCF(その他)" ~ "LULUCF")) %>% 
  mutate(IAMC_templatePlus = ifelse(IAMC_templatePlus < 0, -IAMC_templatePlus, IAMC_templatePlus))%>% 
  filter(!is.na(IAMC_templatePlus))
df_set_red_re <- left_join(map_data_r17, df_set_red, by = c("X.ISO" = "REMF"))
df_set_red_re <- df_set_red_re %>% 
  rename("REMF" = "X.ISO") %>% 
  subset(select = -X) %>% 
  subset(select = -X.asterisks.for.map)%>%
  mutate(AIM5 = case_when(
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
  ))

#Emission
df_set_emi <- Load %>%
  filter(Scenario %in% c("SSP2_CM_NoCC","FAO-FAOTier1","EDGAR"))%>%
  mutate(Scenario=recode(Scenario,SSP2_CM_NoCC="AFOLUB","FAO-FAOTier1"="FAOSTAT",EDGAR="EDGAR")) %>% 
  filter(VEMF %in% c("Emi_equ_CH4_AFO_Agr_Liv_Ent_Fer","Emi_equ_CH4_AFO_Agr_Liv_Man_Man","Emi_equ_N2O_AFO_Agr_Liv_Man_Man","Emi_equ_CH4_AFO_Agr_Ric","Emi_equ_N2O_AFO_Agr_Man_Soi",
                     "Emi_CO2_Lan_Use_Flo_Pos_Emi_Lan_Use_Cha","Emi_CO2_Lan_Use_Flo_RemForest","Emi_CO2_AFO_Lan_For_Gra_Con","Emi_CO2_Lan_Use_Flo_Pos_Emi_Fir"))%>%
  mutate(VEMF = case_when(
    VEMF == "Emi_CO2_AFO_Lan_For_Gra_Con" ~ "Emi_CO2_Lan_Use_Flo_Pos_Emi_Lan_Use_Cha",
    TRUE ~ VEMF
  )) %>% 
  mutate(VEMF=recode(VEMF,Emi_CO2_Lan_Use_Flo_Pos_Emi_Lan_Use_Cha="土地利用変化","Emi_CO2_Lan_Use_Flo_Pos_Emi_Fir"="火災","Emi_CO2_Lan_Use_Flo_RemForest"="木質炭素吸収",
                     Emi_equ_CH4_AFO_Agr_Liv_Ent_Fer="家畜反芻",Emi_equ_CH4_AFO_Agr_Ric="稲作",Emi_equ_N2O_AFO_Agr_Man_Soi="農耕地土壌"))
df_set_emi <- distinct(df_set_emi)
df_MM <- df_set_emi %>%
  filter(VEMF %in% c("Emi_equ_CH4_AFO_Agr_Liv_Man_Man","Emi_equ_N2O_AFO_Agr_Liv_Man_Man")) %>%
  group_by(Scenario,REMF,YEMF) %>%
  summarize(IAMC_templatePlus = sum(IAMC_templatePlus),
            VEMF = "糞尿管理",
            .groups = 'drop')
df_set_emi <- bind_rows(
  df_set_emi %>% filter(!VEMF %in% c("Emi_equ_CH4_AFO_Agr_Liv_Man_Man","Emi_equ_N2O_AFO_Agr_Liv_Man_Man")),
  df_MM)
df_set_emi2<- df_set_emi %>%
  mutate(sourse = case_when(
    VEMF == "家畜反芻" ~ "農業",
    VEMF == "糞尿管理" ~ "農業",
    VEMF == "稲作" ~ "農業",
    VEMF == "農耕地土壌" ~ "農業",
    VEMF == "土地利用変化" ~ "LULUCF",
    VEMF == "火災" ~ "LULUCF",
    VEMF == "木質炭素吸収" ~ "LULUCF"))

#Cost
df_set_cost <- Load %>%
  filter(Scenario %in% c("SSP2_CM_NoCC")) %>%
  filter(!YEMF %in% c("2005"))%>%
  filter(VEMF %in% c("Red_Cos_AFO_Agr_Liv_Ent_Fer","Red_Cos_AFO_Agr_Liv_Man_Man","Red_Cos_AFO_Agr_Ric","Red_Cos_AFO_Agr_Man_Soi",
                     "Red_Cos_AFO_Lan_Pla","Red_Cos_AFO_Lan_FPr","Red_Cos_AFO_Lan_Pff","Red_Cos_AFO_Lan_Ril",
                     "Red_Cos_AFO_Lan_Enr","Red_Cos_AFO_Lan_Agf","Red_Cos_AFO_Lan_Bch","Red_Cos_AFO_Lan_Scc","Red_Cos_AFO_Lan_Scg")) %>% 
  mutate(VEMF=recode(VEMF,Red_Cos_AFO_Agr_Liv_Ent_Fer="反芻対策",Red_Cos_AFO_Agr_Liv_Man_Man="糞尿対策",Red_Cos_AFO_Agr_Ric="稲作対策",Red_Cos_AFO_Agr_Man_Soi="土壌対策",
                     Red_Cos_AFO_Lan_Pla="植林",Red_Cos_AFO_Lan_FPr="森林保護",Red_Cos_AFO_Lan_Pff="森林火災の防止","Red_Cos_AFO_Lan_Ril"="環境負荷の少ない伐採",
                     Red_Cos_AFO_Lan_Enr="自然再生の強化",Red_Cos_AFO_Lan_Agf="アグロフォレストリー",Red_Cos_AFO_Lan_Bch="バイオ炭"))%>% 
  filter(!is.na(IAMC_templatePlus))
df_Scs <- df_set_cost %>%
  filter(VEMF %in% c("Red_Cos_AFO_Lan_Scc","Red_Cos_AFO_Lan_Scg")) %>%
  group_by(Scenario,REMF,YEMF) %>%
  summarize(IAMC_templatePlus = sum(IAMC_templatePlus),
            VEMF = "土壌炭素隔離",
            .groups = 'drop')
df_set_cost <- bind_rows(
  df_set_cost %>% filter(!VEMF %in% c("Red_Cos_AFO_Lan_Scc","Red_Cos_AFO_Lan_Scg")),
  df_Scs)

#Area
df_set_area <- Load %>%
  filter(Scenario %in% c("SSP2_CM_NoCC")) %>%
  filter(!YEMF %in% c("2005"))%>%
  filter(VEMF %in% c("Emi_Red_CO2_AFO_Lan_Pla","Emi_Red_CO2_AFO_Lan_FPr","Emi_Red_CO2_AFO_Lan_Pff","Emi_Red_CO2_AFO_Lan_Ril","Emi_Red_CO2_AFO_Lan_Enr","Emi_Red_CO2_AFO_Lan_Agf","Emi_Red_CO2_AFO_Lan_Bch","Emi_Red_CO2_AFO_Lan_Scs"))%>%
  mutate(VEMF=recode(VEMF,Emi_Red_CO2_AFO_Lan_Pla="植林",Emi_Red_CO2_AFO_Lan_FPr="森林保護",Emi_Red_CO2_AFO_Lan_Pff="森林火災の防止",Emi_Red_CO2_AFO_Lan_Ril="環境負荷の少ない伐採",
                     Emi_Red_CO2_AFO_Lan_Enr="自然再生の強化",Emi_Red_CO2_AFO_Lan_Agf="アグロフォレストリー",Emi_Red_CO2_AFO_Lan_Bch="バイオ炭",Emi_Red_CO2_AFO_Lan_Scs="土壌炭素隔離"))
#------GraphSET-----------------------------------------------------------------------------------------------
theme <- theme(
  plot.title = element_blank(),
  axis.text.x = element_text(size = 16, color = "black", angle = 45, hjust = 1),
  axis.text.y = element_text(size = 16, color = "black"),
  axis.title.x = element_blank(),axis.title.y = element_text(size = 22),
  strip.text.x = element_text(size = 17.5),strip.text.y = element_text(size = 20),
  legend.text = element_text(size = 20),legend.position = "right",
  legend.title = element_blank(),
  panel.grid.minor = element_line(color = NA),
  axis.line = element_line(colour = "black"),
  panel.background = element_rect(fill = "white"),
  panel.grid.major = element_blank(),
  strip.background = element_rect(fill = "white", colour = "white")
)
theme2 <- theme(
  plot.title = element_blank(), 
  axis.text.x =  element_blank(), axis.text.y = element_blank(), 
  axis.title.x = element_blank(), element_blank(),
  strip.text.x = element_text(size = 10),legend.text = element_text(size =12),
  legend.position = c(0.1, 0.2),legend.title = element_text(size =12.75,margin = margin(b = 10)),
  panel.grid.minor = element_line(color = NA), panel.grid.major = element_blank(),
  strip.background = element_rect(fill="white", colour="white"),axis.ticks = element_blank(),
  panel.border = element_blank(),plot.margin = margin(t = 1, r = 1, b = 1, l = 1)
)
theme3 <- theme(
  plot.title = element_text(size = 20), plot.tag=element_text(size = 18),
  axis.text.x = element_text(size = 16,color="black",angle = 45, hjust = 1), axis.text.y = element_text(size = 18,color="black"), 
  axis.title.x = element_blank(), axis.title.y = element_blank(),
  strip.text.x = element_text(size = 17.5),legend.text = element_text(size =22),
  legend.position = "none",legend.title = element_blank(),
  panel.grid.minor = element_line(color = NA), axis.line = element_line(colour="black"),
  panel.background = element_rect(fill = "white"),panel.grid.major = element_blank(),
  strip.background = element_rect(fill="white", colour="white")
)

color_bar1<- c("反芻対策" = "#FFA040","糞尿対策" = "#F48FB1","稲作対策" = "#FFEE58",
               "土壌対策" = "#CE93D8","LULUCF(CDR)" = "#A5D6A7","LULUCF(その他)" = "#90CAF9")
color_bar2 <- c(
  "植林" = "#66c2a5","再植林" = "#8da0cb","森林保護" = "#1b7837","森林火災の防止" = "#e41a1c",
  "環境負荷の少ない伐採" = "#a65628","自然再生の強化" = "#4daf4a","アグロフォレストリー" = "#bf812d",
  "バイオ炭" = "#b3b3b3","土壌炭素隔離" = "#a6d854","生産性が高い家畜種への変更" = "#fc8d62",
  "濃厚飼料の投与"="#A52A2A","糞尿堆肥化" = "#e78ac3","窒素肥料の硫酸アンモニウムへの変更" = "#80b1d3",
  "ラグーンカバー"="lightgreen","嫌気性消化(大規模プラント)"="#5c6c7a","嫌気性消化(小規模)"="#8daecf",
  "水田への間断灌漑の導入" = "#b3de69","稲わらのすき込み"="#a9a44a","不耕起移植栽培" = "#ffd92f",
  "肥料の高効率利用"="#984ea3","化学肥料の有機肥料による置き換え" = "#e5c494","被覆肥料の利用" = "#4DB6AC","硝化抑制剤を含む肥料の利用" = "#377eb8"
)
color_bar3 <- c("家畜反芻"="lightsalmon","糞尿管理"="darkolivegreen3","稲作"="peru","農耕地土壌"="lightskyblue3",
                "木質炭素吸収"="forestgreen","火災"="#d94801","土地利用変化"="#FFE082")
color_bar4 <- c("AFOLUB"="lightsalmon","FAOSTAT"="grey60")
color_bar5<- c("反芻対策" = "#FFA040","糞尿対策" = "#F48FB1","稲作対策" = "#FFEE58","土壌対策" = "#CE93D8",
               "植林" = "#66c2a5","森林保護" = "#1b7837","森林火災の防止" = "#e41a1c","環境負荷の少ない伐採" = "#a65628",
               "自然再生の強化" = "#4daf4a","アグロフォレストリー" = "#bf812d","バイオ炭" = "#b3b3b3","土壌炭素隔離" = "#a6d854")

#------Reduction-----------------------------------------------------------------------------------------------
#GLOBAL
df_red_GL <- df_set_red %>%
  filter(REMF %in% c("World"))%>%
  group_by(REMF,YEMF,main) %>%
  summarize(
    IAMC_templatePlus = sum(IAMC_templatePlus)
  )

g1 <- df_red_GL %>%
  mutate(main = factor(main, levels = c("LULUCF(CDR)", "LULUCF(その他)","反芻対策", "糞尿対策", "稲作対策","土壌対策"))) %>%
  ggplot() +
  geom_bar(aes(x=YEMF,y=IAMC_templatePlus/1000,fill=main),stat = "identity",color="black",size = 0.15)+
  labs(title = "",x="Year", y = expression(paste('Reduction potential (Gt',{CO[2]},'eq/yr)',seq="")))+
  theme_bw()+
  scale_fill_manual(values = color_bar1)+
  theme+
  xlab(NULL)
plot(g1)

ggsave(file = "../../output/Check/Reduction/change_GLOBAL.png", plot=g1, width = 8, height = 6)

#R5
df_red_R5 <- df_set_red %>%
  filter(REMF %in% c("R5REF","R5LAM","R5ASIA","R5OECD90+EU","R5MAF"))%>%
  mutate(REMF=recode(REMF,R5REF="旧ソ連地域",R5LAM="中南米",R5ASIA="アジア",R5MAF="アフリカ・中東","R5OECD90+EU"="OECD+EU")) %>% 
  group_by(REMF,YEMF,main) %>%
  summarize(
    IAMC_templatePlus = sum(IAMC_templatePlus)
  )

g2 <- df_red_R5 %>%
  mutate(REMF = factor(REMF, levels = c("OECD+EU","アジア","アフリカ・中東","中南米","旧ソ連地域"))) %>%
  mutate(main = factor(main, levels = c("LULUCF(CDR)", "LULUCF(その他)","反芻対策", "糞尿対策", "稲作対策","土壌対策"))) %>%
  ggplot() +
  geom_bar(aes(x=YEMF,y=IAMC_templatePlus/1000,fill=main),stat = "identity",color="black",size = 0.15)+
  facet_wrap(~REMF, nrow = 2)+
  labs(title = "",x="Year", y = expression(paste('Reduction potential (Gt',{CO[2]},'eq/yr)',seq="")))+
  theme_bw()+
  scale_fill_manual(values = color_bar1)+
  theme+
  xlab(NULL)
plot(g2)

ggsave(file = "../../output/Check/Reduction/change_R5.png", plot=g2, width = 12.5, height = 7)

#measure2050
df_red_R5_2050 <- df_set_red %>%
  filter(REMF %in% c("R5REF","R5LAM","R5ASIA","R5OECD90+EU","R5MAF"))%>%
  mutate(REMF=recode(REMF,R5REF="旧ソ連地域",R5LAM="中南米",R5ASIA="アジア",R5MAF="アフリカ・中東","R5OECD90+EU"="OECD+EU")) %>% 
  filter(YEMF %in% c("2050"))

g3 <- df_red_R5_2050 %>%
  mutate(REMF = factor(REMF, levels = c("OECD+EU","アジア","アフリカ・中東","中南米","旧ソ連地域"))) %>%
  mutate(category = factor(category, levels = c("農業","森林再生", "保護", "管理", "農地再生"))) %>%
  ggplot() +
  geom_bar(aes(x=category,y=IAMC_templatePlus/1000,fill=VEMF),stat = "identity",width = 0.8,position = "stack",color="black",size = 0.15)+
  labs(title = "", x="", y = expression(paste('Reduction potential (Gt',{CO[2]},'eq/yr)',seq="")))+
  facet_wrap(~REMF, nrow = 2, scales = "free_y")+
  theme_bw()+
  scale_fill_manual(values = color_bar2) +
  theme+
  guides(fill = guide_legend(ncol = 1)) 
plot(g3)

ggsave(file = "../../output/Check/Reduction/measure_2050R5.png", plot=g3, width = 12.5, height = 7)

#measure2100
df_red_R5_2100 <- df_set_red %>%
  filter(REMF %in% c("R5REF","R5LAM","R5ASIA","R5OECD90+EU","R5MAF"))%>%
  mutate(REMF=recode(REMF,R5REF="旧ソ連地域",R5LAM="中南米",R5ASIA="アジア",R5MAF="アフリカ・中東","R5OECD90+EU"="OECD+EU")) %>% 
  filter(YEMF %in% c("2100"))

g4 <- df_red_R5_2100 %>%
  mutate(REMF = factor(REMF, levels = c("OECD+EU","アジア","アフリカ・中東","中南米","旧ソ連地域"))) %>%
  mutate(category = factor(category, levels = c("農業","森林再生", "保護", "管理", "農地再生"))) %>%
  mutate(VEMF = factor(VEMF, levels = c("生産性が高い家畜種への変更", "濃厚飼料の投与","糞尿堆肥化", "窒素肥料の硫酸アンモニウムへの変更", 
                                        "水田への間断灌漑の導入", "不耕起移植栽培","肥料の高効率利用","化学肥料の有機肥料による置き換え", 
                                        "硝化抑制剤を含む肥料の利用","植林", "再植林", "森林保護","環境負荷の少ない伐採", "自然再生の強化",
                                        "森林火災の防止","アグロフォレストリー", "バイオ炭", "土壌炭素隔離"))) %>%
  ggplot() +
  geom_bar(aes(x=category,y=IAMC_templatePlus/1000,fill=VEMF),stat = "identity",width = 0.8,position = "stack",color="black",size = 0.15)+
  labs(title = "", x="", y = expression(paste('Reduction potential (Gt',{CO[2]},'eq/yr)',seq="")))+
  facet_wrap(~REMF, nrow = 2, scales = "free_y")+
  theme_bw()+
  scale_fill_manual(values = color_bar2) +
  theme+
  guides(fill = guide_legend(ncol = 1)) 
plot(g4)

ggsave(file = "../../output/Check/Reduction/measure_2100R5.png", plot=g4, width = 12.5, height = 7)

#measure_agr
df_red_R5_agr <- df_set_red %>%
  filter(REMF %in% c("R5REF","R5LAM","R5ASIA","R5OECD90+EU","R5MAF"))%>%
  mutate(REMF=recode(REMF,R5REF="旧ソ連地域",R5LAM="中南米",R5ASIA="アジア",R5MAF="アフリカ・中東","R5OECD90+EU"="OECD+EU")) %>% 
  filter(category %in% c("農業"))

g5 <- df_red_R5_agr %>%
  mutate(REMF = factor(REMF, levels = c("OECD+EU","アジア","アフリカ・中東","中南米","旧ソ連地域"))) %>%
  ggplot() +
  geom_bar(aes(x=YEMF,y=IAMC_templatePlus/1000,fill=VEMF),stat = "identity",color="black",size = 0.15)+
  facet_wrap(~REMF, nrow = 2, scales = "free_y")+
  labs(title = "",x="Year", y = expression(paste('Reduction potential (Gt',{CO[2]},'eq/yr)',seq="")))+
  theme_bw()+
  scale_fill_manual(values = color_bar2)+
  theme+
  xlab(NULL)
plot(g5)

ggsave(file = "../../output/Check/Reduction/measure_agrR5.png", plot=g5, width = 13.5, height = 7)

#LULUCF
df_red_R5_lu<- df_set_red %>%
  filter(REMF %in% c("R5REF","R5LAM","R5ASIA","R5OECD90+EU","R5MAF"))%>%
  mutate(REMF=recode(REMF,R5REF="旧ソ連地域",R5LAM="中南米",R5ASIA="アジア",R5MAF="アフリカ・中東","R5OECD90+EU"="OECD+EU")) %>% 
  filter(!category %in% c("農業"))

g6 <- df_red_R5_lu %>%
  mutate(REMF = factor(REMF, levels = c("OECD+EU","アジア","アフリカ・中東","中南米","旧ソ連地域"))) %>%
  ggplot() +
  geom_bar(aes(x=YEMF,y=IAMC_templatePlus/1000,fill=VEMF),stat = "identity",color="black",size = 0.15)+
  facet_wrap(~REMF, nrow = 2, scales = "free_y")+
  labs(title = "",x="Year", y = expression(paste('Reduction potential (Gt',{CO[2]},'eq/yr)',seq="")))+
  theme_bw()+
  scale_fill_manual(values = color_bar2)+
  theme+
  xlab(NULL)
plot(g6)

ggsave(file = "../../output/Check/Reduction/measure_luR5.png", plot=g6, width = 13.5, height = 7)

#countrylevel
df_red_R178_2100 <- df_set_red %>%
  filter(!REMF %in% c("XE25","XER","XOC","XSA","XSE","XLM","XNF","XAF","CIS","XME","R5REF","R5LAM","R5ASIA","R5OECD90+EU","R5MAF","World"))%>%
  group_by(REMF,YEMF) %>%
  summarize(
    Value = sum(IAMC_templatePlus))%>%
  filter(YEMF %in% c("2100"))

world_set <- ne_countries(scale = "medium", returnclass = "sf")
world <- world_set %>%
  filter(!name == "Antarctica")%>%
  filter(name != "Greenland")

world_2100 <- world %>%
  left_join(df_red_R178_2100, by = c("iso_a3_eh" = "REMF"))
world_na <- world_2100 %>%
  filter(is.na(Value))
world_with_data <- world_2100 %>% 
  filter(!is.na(Value))

g7<-
  ggplot() +
  geom_sf(data = world_with_data, aes(fill = Value/1000), color = "black") +
  geom_sf(data = world_na, fill = "grey", color = "black", size = 0.4) +
  scale_fill_gradient(name = expression(paste('Potential (Gt',{CO[2]},'eq/yr)',seq="")),low = "#fff9f3", high = "#d94801") +
  coord_sf(expand = FALSE) +
  theme_bw()+
  guides(fill = guide_colorbar(barwidth = 1.5, barheight = 6))+
  theme2
plot(g7)

ggsave(file = "../../output/Check/Reduction/2100countrylevel.png", plot=g7, width = 10, height = 4.5)

#countrylevel_percapit
world_2100_pop <- world_2100 %>% 
  group_by(iso_a3_eh) %>%
  summarize(Value = sum((Value)*1000000)/sum((pop_est)))

world_na_pop<- world_2100_pop %>% filter(is.na(Value))
world_with_data_pop <- world_2100_pop %>% filter(!is.na(Value))

world_na_pop <- st_wrap_dateline(world_na_pop, options = c("WRAPDATELINE=YES"), quiet = TRUE)
world_with_data_pop <- st_wrap_dateline(world_with_data_pop, options = c("WRAPDATELINE=YES"), quiet = TRUE)

g8<-
  ggplot() +
  geom_sf(data = world_with_data_pop, aes(fill = Value), color = "black") +
  geom_sf(data = world_na_pop, fill = "grey", color = "black") +
  scale_fill_gradient(name = expression(paste('Potential (t',{CO[2]},'eq/cap/yr)',seq="")),low = "#fff9f3", high = "#d94801") +
  coord_sf(expand = FALSE) +
  theme_bw()+
  guides(fill = guide_colorbar(barwidth = 1.5, barheight = 6))+
  theme2
plot(g8)

ggsave(file = "../../output/Check/Reduction/2100countrylevel_percap.png", plot=g8, width = 10.15, height = 4.6)

#------Reduction_region-----------------------------------------------------------------------------------------------
#OECD+EU
df_red_OECD <- df_set_red_re %>% 
  filter(AIM5 %in% c("R5OECD90+EU"))%>% 
  filter(!is.na(IAMC_templatePlus)) %>% 
  group_by(REMF,YEMF,main) %>%
  summarize(
    IAMC_templatePlus = sum(IAMC_templatePlus)
  )

g9 <- df_red_OECD %>%
  mutate(REMF = factor(REMF, levels = c("USA","CAN","AUS","FRA","TUR","DEU","ESP","NZL","GBR","POL","ITA","JPN",
                                        "IRL","NLD","DNK","GRC","SWE","ROU","BEL","HUN","CZE","PRT","AUT","FIN",
                                        "LTU","NOR","SVK","CHE","LVA","BGR","EST","SVN","ISL","MNE","SRB"))) %>%
  mutate(main = factor(main, levels = c("LULUCF(CDR)", "LULUCF(その他)","反芻対策", "糞尿対策", "稲作対策","土壌対策"))) %>%
  ggplot() +
  geom_bar(aes(x=YEMF,y=IAMC_templatePlus,fill=main),stat = "identity",color="black",size = 0.15)+
  labs(title = "",x="Year", y = expression(paste('Reduction potential (Mt',{CO[2]},'eq/yr)',seq="")))+
  facet_wrap(~REMF, ncol = 5, scales = "free_y")+
  theme_bw()+
  scale_fill_manual(values = color_bar1)+
  theme+
  xlab(NULL)
plot(g9)

ggsave(file = "../../output/Check/Reduction/change_count_OECD.png", plot=g9, width = 16.5, height = 16)

#Asia
df_red_asia <- df_set_red_re %>% 
  filter(AIM5 %in% c("R5ASIA"))%>% 
  filter(!is.na(IAMC_templatePlus)) %>% 
  group_by(REMF,YEMF,main) %>%
  summarize(
    IAMC_templatePlus = sum(IAMC_templatePlus)
  )

g10 <- df_red_asia %>%
  mutate(REMF = factor(REMF, levels = c("IND","CHN","PAK","BGD","IDN","THA","MMR","VNM","NPL","AFG",
                                        "PHL","MNG","KHM","KOR","LKA","MYS","LAO","PNG","BTN"))) %>%
  mutate(main = factor(main, levels = c("LULUCF(CDR)", "LULUCF(その他)","反芻対策", "糞尿対策", "稲作対策","土壌対策"))) %>%
  ggplot() +
  geom_bar(aes(x=YEMF,y=IAMC_templatePlus,fill=main),stat = "identity",color="black",size = 0.15)+
  labs(title = "",x="Year", y = expression(paste('Reduction potential (Mt',{CO[2]},'eq/yr)',seq="")))+
  facet_wrap(~REMF, ncol = 5, scales = "free_y")+
  theme_bw()+
  scale_fill_manual(values = color_bar1)+
  theme+
  xlab(NULL)
plot(g10)

ggsave(file = "../../output/Check/Reduction/region/change_ASIA.png", plot=g10, width = 16.5, height = 8.5)

#Africa+MidEAST
df_red_maf <- df_set_red_re %>% 
  filter(AIM5 %in% c("R5MAF"))%>% 
  filter(!is.na(IAMC_templatePlus)) %>% 
  group_by(REMF,YEMF,main) %>%
  summarize(
    IAMC_templatePlus = sum(IAMC_templatePlus)
  )

g11 <- df_red_maf %>%
  mutate(REMF = factor(REMF, levels = c("NGA","ETH","TZA","KEN","ZAF","IRN","TCD","MLI","SOM","UGA","MDG","BFA","NER",
                                        "COD","DZA","CMR","EGY","ZWE","GIN","SEN","CIV","CAF","MAR","AGO","MRT","NAM",
                                        "SAU","MOZ","ERI","GHA","SYR","IRQ","BEN","LBY","ZMB","BWA","MWI","RWA","YEM",
                                        "TUN","SLE","COG","TGO","LSO","SWZ","GNB","JOR","BDI","ISR","DJI","ARE","GMB",
                                        "LBN","KWT","OMN","LBR","PSE","GAB","MUS","COM","CPV","QAT","BHR","GNQ","STP"))) %>%
  mutate(main = factor(main, levels = c("LULUCF(CDR)", "LULUCF(その他)","反芻対策", "糞尿対策", "稲作対策","土壌対策"))) %>%
  ggplot() +
  geom_bar(aes(x=YEMF,y=IAMC_templatePlus,fill=main),stat = "identity",color="black",size = 0.15)+
  labs(title = "",x="Year", y = expression(paste('Reduction potential (Mt',{CO[2]},'eq/yr)',seq="")))+
  facet_wrap(~REMF, ncol = 5, scales = "free_y")+
  theme_bw()+
  scale_fill_manual(values = color_bar1)+
  theme+
  xlab(NULL)
plot(g11)

ggsave(file = "../../output/Check/Reduction/region/change_MAF.png", plot=g11, width = 17, height = 20)

#Latin America
df_red_latin <- df_set_red_re %>% 
  filter(AIM5 %in% c("R5LAM"))%>% 
  filter(!is.na(IAMC_templatePlus)) %>% 
  group_by(REMF,YEMF,main) %>%
  summarize(
    IAMC_templatePlus = sum(IAMC_templatePlus)
  )

g12 <- df_red_latin %>%
  mutate(REMF = factor(REMF, levels = c("BRA","ARG","MEX","COL","VEN","PER","BOL","URY","ECU","PRY",
                                        "CUB","GTM","NIC","DOM","HND","PAN","CRI","GUY","TCA","SUR"))) %>%
  mutate(main = factor(main, levels = c("LULUCF(CDR)", "LULUCF(その他)","反芻対策", "糞尿対策", "稲作対策","土壌対策"))) %>%
  ggplot() +
  geom_bar(aes(x=YEMF,y=IAMC_templatePlus,fill=main),stat = "identity",color="black",size = 0.15)+
  labs(title = "",x="Year", y = expression(paste('Reduction potential (Mt',{CO[2]},'eq/yr)',seq="")))+
  facet_wrap(~REMF, ncol = 5, scales = "free_y")+
  theme_bw()+
  scale_fill_manual(values = color_bar1)+
  theme+
  xlab(NULL)
plot(g12)

ggsave(file = "../../output/Check/Reduction/region/change_LAM.png", plot=g12, width = 16.5, height = 8.5)

#CIS
df_red_cis <- df_set_red_re %>% 
  filter(AIM5 %in% c("R5REF"))%>% 
  filter(!is.na(IAMC_templatePlus)) %>% 
  group_by(REMF,YEMF,main) %>%
  summarize(
    IAMC_templatePlus = sum(IAMC_templatePlus)
  )

g13 <- df_red_cis %>%
  mutate(REMF = factor(REMF, levels = c("RUS","KAZ","UKR","UZB","BLR","TKM","AZE","TJK","KGZ","GEO","MDA","ARM"))) %>%
  mutate(main = factor(main, levels = c("LULUCF(CDR)", "LULUCF(その他)","反芻対策", "糞尿対策", "稲作対策","土壌対策"))) %>%
  ggplot() +
  geom_bar(aes(x=YEMF,y=IAMC_templatePlus,fill=main),stat = "identity",color="black",size = 0.15)+
  labs(title = "",x="Year", y = expression(paste('Reduction potential (Mt',{CO[2]},'eq/yr)',seq="")))+
  facet_wrap(~REMF, ncol = 5, scales = "free_y")+
  theme_bw()+
  scale_fill_manual(values = color_bar1)+
  theme+
  xlab(NULL)
plot(g13)

ggsave(file = "../../output/Check/Reduction/region/change_REF.png", plot=g13, width = 16.5, height = 6.5)

#------Emission_BaU-----------------------------------------------------------------------------------------------
#check
df_emi_Val_GL <- df_set_emi %>%
  filter(REMF %in% c("World"))%>%
  filter(YEMF %in% c("2005")) 

g14 <- df_emi_Val_GL %>%
  mutate(VEMF = factor(VEMF, levels = c("家畜反芻","糞尿管理","稲作","農耕地土壌","土地利用変化","火災","木質炭素吸収"))) %>%
  ggplot() +
  geom_bar(aes(x=Scenario,y=IAMC_templatePlus/1000, fill = VEMF),stat = "identity",color = "black",size = 0.15)+
  geom_hline(yintercept = 0, color = "black", linetype = "solid", size = 0.25, alpha = 0.75) +
  labs(y = expression(paste('Emissions (Gt',{CO[2]},'eq/yr)',seq="")))+
  theme_bw()+
  scale_fill_manual(values = color_bar3)+
  theme
plot(g14)

ggsave(file = "../../output/Check/Emission/Validbase_GLOBAL.png", plot=g14, width = 10, height = 6)

#R5
df_emi_R5 <- df_set_emi %>%
  filter(REMF %in% c("R5REF","R5LAM","R5ASIA","R5OECD90+EU","R5MAF","World")) %>%
  mutate(REMF=recode(REMF,R5REF="旧ソ連地域",R5LAM="中南米",R5ASIA="アジア",R5MAF="アフリカ・中東","R5OECD90+EU"="OECD+EU")) %>% 
  filter(Scenario %in% c("AFOLUB","FAOSTAT")) 

df_emi_R5_EF <- filter(df_emi_R5,VEMF %in% c("家畜反芻"))
df_emi_R5_MM <- filter(df_emi_R5,VEMF %in% c("糞尿管理"))
df_emi_R5_RC <- filter(df_emi_R5,VEMF %in% c("稲作"))
df_emi_R5_MS <- filter(df_emi_R5,VEMF %in% c("農耕地土壌"))
df_emi_R5_RUC <- filter(df_emi_R5,VEMF %in% c("土地利用変化"))
df_emi_R5_WCC <- filter(df_emi_R5,VEMF %in% c("木質炭素吸収"))

g15 <- df_emi_R5_EF %>%
  mutate(REMF = factor(REMF, levels = c("OECD+EU","アジア","アフリカ・中東","中南米","旧ソ連地域","World"))) %>%
  ggplot() +
  geom_hline(yintercept = 0, color = "black", linetype = "solid", size = 0.25, alpha = 0.7) +
  geom_line(aes(x=YEMF,y=IAMC_templatePlus/1000,group=Scenario,color=Scenario),linewidth = 1)+
  geom_point(aes(x=YEMF,y=IAMC_templatePlus/1000,group=Scenario,color=Scenario),size = 3)+  
  facet_wrap(~REMF, nrow = 2, scales = "free_y")+
  labs(tag="(a)",title = "家畜反芻", x="", y = expression(paste('Emissions (Gt',{CO[2]},'eq/yr)',seq="")))+
  theme_bw()+
  scale_color_manual(values = color_bar4)+
  theme3
plot(g15)

g16 <- df_emi_R5_MM %>%
  mutate(REMF = factor(REMF, levels = c("OECD+EU","アジア","アフリカ・中東","中南米","旧ソ連地域","World"))) %>%
  ggplot() +
  geom_hline(yintercept = 0, color = "black", linetype = "solid", size = 0.25, alpha = 0.7) +
  geom_line(aes(x=YEMF,y=IAMC_templatePlus/1000,group=Scenario,color=Scenario),linewidth = 1)+
  geom_point(aes(x=YEMF,y=IAMC_templatePlus/1000,group=Scenario,color=Scenario), size = 3)+  
  facet_wrap(~REMF, nrow = 2, scales = "free_y")+
  labs(tag="(b)",title = "糞尿管理", x="", y = expression(paste('Emissions (Gt',{CO[2]},'eq/yr)',seq="")))+
  theme_bw()+
  scale_color_manual(values = color_bar4)+
  theme3
plot(g16)

g17 <- df_emi_R5_RC %>%
  mutate(REMF = factor(REMF, levels = c("OECD+EU","アジア","アフリカ・中東","中南米","旧ソ連地域","World"))) %>%
  ggplot() +
  geom_hline(yintercept = 0, color = "black", linetype = "solid", size = 0.25, alpha = 0.7) +
  geom_line(aes(x=YEMF,y=IAMC_templatePlus/1000,group=Scenario,color=Scenario),linewidth = 1)+
  geom_point(aes(x=YEMF,y=IAMC_templatePlus/1000,group=Scenario,color=Scenario), size = 3)+  
  facet_wrap(~REMF, nrow = 2, scales = "free_y")+
  labs(tag="(c)",title = "稲作", x="", y = expression(paste('Emissions (Gt',{CO[2]},'eq/yr)',seq="")))+
  theme_bw()+
  scale_color_manual(values = color_bar4)+
  theme3
plot(g17)

g18 <- df_emi_R5_MS %>%
  mutate(REMF = factor(REMF, levels = c("OECD+EU","アジア","アフリカ・中東","中南米","旧ソ連地域","World"))) %>%
  ggplot() +
  geom_hline(yintercept = 0, color = "black", linetype = "solid", size = 0.25, alpha = 0.7) +
  geom_line(aes(x=YEMF,y=IAMC_templatePlus/1000,group=Scenario,color=Scenario),linewidth = 1)+
  geom_point(aes(x=YEMF,y=IAMC_templatePlus/1000,group=Scenario,color=Scenario), size = 3)+  
  facet_wrap(~REMF, nrow = 2, scales = "free_y")+
  labs(tag="(d)",title = "農耕地土壌", x="", y = expression(paste('Emissions (Gt',{CO[2]},'eq/yr)',seq="")))+
  theme_bw()+
  scale_color_manual(values = color_bar4)+
  theme3
plot(g18)

g19 <- df_emi_R5_RUC %>%
  mutate(REMF = factor(REMF, levels = c("OECD+EU","アジア","アフリカ・中東","中南米","旧ソ連地域","World"))) %>%
  ggplot() +
  geom_hline(yintercept = 0, color = "black", linetype = "solid", size = 0.25, alpha = 0.7) +
  geom_line(aes(x=YEMF,y=IAMC_templatePlus/1000,group=Scenario,color=Scenario),linewidth = 1)+
  geom_point(aes(x=YEMF,y=IAMC_templatePlus/1000,group=Scenario,color=Scenario), size = 3)+  
  facet_wrap(~REMF, nrow = 2, scales = "free_y")+
  labs(tag="(e)",title = "土地利用変化", x="", y = expression(paste('Emissions (Gt',{CO[2]},'eq/yr)',seq="")))+
  theme_bw()+
  scale_color_manual(values = color_bar4)+
  theme3
plot(g19)

g20 <- df_emi_R5_WCC %>%
  mutate(REMF = factor(REMF, levels = c("OECD+EU","アジア","アフリカ・中東","中南米","旧ソ連地域","World"))) %>%
  ggplot() +
  geom_hline(yintercept = 0, color = "black", linetype = "solid", size = 0.25, alpha = 0.7) +
  geom_line(aes(x=YEMF,y=IAMC_templatePlus/1000,group=Scenario,color=Scenario),linewidth = 1)+
  geom_point(aes(x=YEMF,y=IAMC_templatePlus/1000,group=Scenario,color=Scenario), size = 3)+  
  facet_wrap(~REMF, nrow = 2, scales = "free_y")+
  labs(tag="(f)",title = "木質炭素吸収", x="", y = expression(paste('Emissions (Gt',{CO[2]},'eq/yr)',seq="")))+
  theme_bw()+
  scale_color_manual(values = color_bar4)+
  theme3
plot(g20)

legend <- df_emi_R5_WCC%>%
  mutate(Scenario = factor(Scenario, levels = c("AFOLUB","FAOSTAT"))) %>%
  ggplot(aes(x=YEMF,y=IAMC_templatePlus/1000,group=Scenario,color=Scenario)) +
  geom_point(size = 3) +
  geom_line(linewidth = 1)+
  theme_bw()+
  scale_color_manual(values = color_bar4)+
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

p1 <- (g15| g16) + plot_layout(widths = c(1, 1), heights = c(1, 1))
p2 <- (g17| g18) + plot_layout(widths = c(1, 1), heights = c(1, 1))
p3 <- (g19| g20) + plot_layout(widths = c(1, 1), heights = c(1, 1))
p4 <- (p1/  plot_spacer() /p2 / plot_spacer() /p3 / legend) + plot_layout(widths = c(1,1), heights = c(1, 0.00001, 1, 0.00001, 1 , 0.00015))
plot(p4)

ggsave(file = "../../output/Check/Emission/change_R5.png", plot=p4,width = 20, height = 17)

#------Emission_CM-----------------------------------------------------------------------------------------------
df_red_so_GL <- df_set_red %>%
  filter(REMF %in% c("World")) %>%
  filter(!YEMF %in% c("2005","2010","2020")) %>%
  group_by(REMF,YEMF,sourse) %>%
  summarize(
    Red = sum(IAMC_templatePlus)
  )
df_emi_so_GL <- df_set_emi2 %>%
  filter(REMF %in% c("World")) %>%
  filter(!YEMF %in% c("2005","2010","2020")) %>%
  group_by(REMF,YEMF,sourse) %>%
  summarize(
    BaU = sum(IAMC_templatePlus)
  )
df_red_emi_GL <- full_join(df_red_so_GL, df_emi_so_GL, by = c("REMF", "YEMF","sourse"))
df_emi_CM_GL <- df_red_emi_GL %>%
  group_by(REMF,YEMF,sourse) %>%
  mutate(CM = sum(BaU)-sum(Red)) %>% 
  pivot_longer(cols=c("BaU","CM"),names_to="Emission",values_to="Value")

g21 <- df_emi_CM_GL %>%
  #mutate(VEMF = factor(VEMF, levels = c("家畜反芻","糞尿管理","稲作","農耕地土壌","土地利用変化","火災","木質炭素吸収"))) %>%
  ggplot() +
  #geom_bar(aes(x=interaction(Emission,YEMF),y=Value/1000, fill = sourse),stat = "identity",color = "black",size = 0.15)+
  geom_bar(aes(x = Emission, y = Value/1000, fill = sourse),stat = "identity", color = "black", size = 0.15) +
  facet_grid(. ~ YEMF, scales = "free_x", space = "free_x")+
  geom_hline(yintercept = 0, color = "black", linetype = "solid", size = 0.25, alpha = 0.75) +
  labs(y = expression(paste('Emissions (Gt',{CO[2]},'eq/yr)',seq="")))+
  theme_bw()+
  theme
plot(g21)

ggsave(file = "../../output/Check/Emission/CM/change_GLOBAL.png", plot=g21,width = 12, height = 7)

#------Cost------------------------------------------------------------------------------------------------------
#GLOBAL
df_cost_GL <- df_set_cost %>%
  filter(REMF %in% c("World"))

g22 <- df_cost_GL %>%
  mutate(VEMF = factor(VEMF, levels = c("植林","森林保護","環境負荷の少ない伐採", "自然再生の強化","森林火災の防止","アグロフォレストリー",
                                        "バイオ炭", "土壌炭素隔離","反芻対策", "糞尿対策", "稲作対策","土壌対策"))) %>%
  ggplot() +
  geom_bar(aes(x=YEMF,y=IAMC_templatePlus/1000,fill=VEMF),stat = "identity",color="black",size = 0.15)+
  labs(title = "",x="Year", y ="Cost(billion USD/yr)")+
  theme_bw()+
  scale_fill_manual(values = color_bar5)+
  theme+
  xlab(NULL)
plot(g22)

ggsave(file = "../../output/Check/Cost/change_GLOBAL.png", plot=g22, width = 8, height = 6)

#R5
df_cost_R5 <- df_set_cost %>%
  filter(REMF %in% c("R5REF","R5LAM","R5ASIA","R5OECD90+EU","R5MAF"))%>%
  mutate(REMF=recode(REMF,R5REF="旧ソ連地域",R5LAM="中南米",R5ASIA="アジア",R5MAF="アフリカ・中東","R5OECD90+EU"="OECD+EU"))

g23 <- df_cost_R5 %>%
  mutate(REMF = factor(REMF, levels = c("OECD+EU","アジア","アフリカ・中東","中南米","旧ソ連地域"))) %>%
  mutate(VEMF = factor(VEMF, levels = c("植林","森林保護","環境負荷の少ない伐採", "自然再生の強化","森林火災の防止","アグロフォレストリー",
                                        "バイオ炭", "土壌炭素隔離","反芻対策", "糞尿対策", "稲作対策","土壌対策"))) %>%
  ggplot() +
  geom_bar(aes(x=YEMF,y=IAMC_templatePlus/1000,fill=VEMF),stat = "identity",color="black",size = 0.15)+
  facet_wrap(~REMF, nrow = 2)+
  labs(title = "",x="Year", y = "Cost(billion USD/yr)")+
  theme_bw()+
  scale_fill_manual(values = color_bar5)+
  theme+
  xlab(NULL)
plot(g23)

ggsave(file = "../../output/Check/Cost/change_R5.png", plot=g23, width = 12.5, height = 7)

#agr
df_cost_R5_agr <- df_set_cost %>%
  filter(REMF %in% c("R5REF","R5LAM","R5ASIA","R5OECD90+EU","R5MAF"))%>%
  mutate(REMF=recode(REMF,R5REF="旧ソ連地域",R5LAM="中南米",R5ASIA="アジア",R5MAF="アフリカ・中東","R5OECD90+EU"="OECD+EU")) %>% 
  filter(VEMF %in% c("反芻対策", "糞尿対策", "稲作対策","土壌対策"))

g24 <- df_cost_R5_agr %>%
  mutate(REMF = factor(REMF, levels = c("OECD+EU","アジア","アフリカ・中東","中南米","旧ソ連地域"))) %>%
  mutate(VEMF = factor(VEMF, levels = c("反芻対策", "糞尿対策", "稲作対策","土壌対策"))) %>%
  ggplot() +
  geom_bar(aes(x=YEMF,y=IAMC_templatePlus/1000,fill=VEMF),stat = "identity",color="black",size = 0.15)+
  facet_wrap(~REMF, nrow = 2, scales = "free_y")+
  labs(title = "",x="Year", y = "Cost(billion USD/yr)")+
  theme_bw()+
  scale_fill_manual(values = color_bar5)+
  theme+
  xlab(NULL)
plot(g24)

ggsave(file = "../../output/Check/Cost/agrR5.png", plot=g24, width = 12, height = 7)

#LULUCF
df_cost_R5_lu <- df_set_cost %>%
  filter(REMF %in% c("R5REF","R5LAM","R5ASIA","R5OECD90+EU","R5MAF"))%>%
  mutate(REMF=recode(REMF,R5REF="旧ソ連地域",R5LAM="中南米",R5ASIA="アジア",R5MAF="アフリカ・中東","R5OECD90+EU"="OECD+EU")) %>% 
  filter(!VEMF %in% c("反芻対策", "糞尿対策", "稲作対策","土壌対策"))

g25 <- df_cost_R5_lu %>%
  mutate(REMF = factor(REMF, levels = c("OECD+EU","アジア","アフリカ・中東","中南米","旧ソ連地域"))) %>%
  mutate(VEMF = factor(VEMF, levels = c("植林","森林保護","環境負荷の少ない伐採", "自然再生の強化","森林火災の防止",
                                        "アグロフォレストリー","バイオ炭", "土壌炭素隔離"))) %>%
  ggplot() +
  geom_bar(aes(x=YEMF,y=IAMC_templatePlus/1000,fill=VEMF),stat = "identity",color="black",size = 0.15)+
  facet_wrap(~REMF, nrow = 2, scales = "free_y")+
  labs(title = "",x="Year", y = "Cost(billion USD/yr)")+
  theme_bw()+
  scale_fill_manual(values = color_bar5)+
  theme+
  xlab(NULL)
plot(g25)

ggsave(file = "../../output/Check/Cost/LULUCFR5.png", plot=g25, width = 12, height = 7)

#------Afforestation------------------------------------------------------------------------------------------------------
df_AR6 <- read_excel("../../data/plantation/AR6_afr.xlsx")
df_AR6_land <- read_excel("../../data/plantation/AR6_LAND.xlsx")

df_AR6 <- df_AR6 %>%
  pivot_longer(cols=c("2000","2005","2010","2015","2020","2025","2030","2035","2040","2045","2050","2055","2060","2065","2070","2075","2080","2085","2090","2095","2100"),names_to="Year",values_to="Value") %>% 
  na.omit(df_AR6)%>%
  filter(Year %in% c("2030","2040","2050","2060","2070","2080","2090","2100"))
df_AR6_land <- df_AR6_land %>%
  pivot_longer(cols=c("2000","2005","2010","2015","2020","2025","2030","2035","2040","2045","2050","2055","2060","2065","2070","2075","2080","2085","2090","2095","2100"),names_to="Year",values_to="Value") %>% 
  na.omit(df_AR6_land)%>%
  filter(Year %in% c("2030","2040","2050","2060","2070","2080","2090","2100"))

summary_custom <- df_AR6 %>% 
  group_by(Year) %>% 
  reframe(p50=median(Value),p90=quantile(Value,.95),p10=quantile(Value,.05),p0=min(Value),p100=max(Value),n=n())
summary_custom_land <- df_AR6_land %>% 
  group_by(Year) %>% 
  reframe(p50=median(Value),p90=quantile(Value,.95),p10=quantile(Value,.05),p0=min(Value),p100=max(Value),n=n())


df_red_Afr_GL <- df_set_red %>%
  filter(REMF %in% c("World"))%>%
  filter(VEMF %in% c("植林"))
df_area_Afr_GL <- df_set_area %>%
  filter(REMF %in% c("World"))%>%
  filter(VEMF %in% c("植林"))

g26<-
  ggplot() +
  geom_crossbar(data=summary_custom %>% mutate(Year=as.character(Year)),
                aes(x=Year,ymin=p10/1000,ymax=p90/1000,y=p50/1000),width=.5,color='white',fill='grey80',linewidth=0.3)+
  geom_crossbar(data=summary_custom %>% mutate(Year=as.character(Year)),
                aes(x=Year,ymin=p0/1000,ymax=p100/1000,y=p0/1000),width=.5,color='grey80',fill='transparent',fatten=0,linewidth=0.3)+
  geom_line(data = df_red_Afr_GL,aes(x = YEMF, y = IAMC_templatePlus/1000, color="本研究", linetype = "本研究", group=1),size = 1) +
  geom_point(data = df_red_Afr_GL, aes(x=YEMF,y=IAMC_templatePlus/1000,color="本研究", group=1),size = 2.5)+
  labs(tag="(a)",title = "",x="Year", y = expression(paste('Reduction potential (Gt',{CO[2]},'eq/yr)',seq="")))+
  theme_bw()+
  scale_color_manual(name = "Line Type",
                     values = c("AR6報告値の中央値" = "darkgrey", "本研究" = "red")) +
  scale_linetype_manual(name = "Line Type",
                        values = c("AR6報告値の中央値" = "dashed", "本研究" = "solid")) +
  theme3+
  guides(fill = guide_legend(nrow = 1))+
  xlab(NULL)
plot(g26)

g27<-
  ggplot() +
  geom_crossbar(data=summary_custom_land %>% mutate(Year=as.character(Year)),
                aes(x=Year,ymin=p10/1000,ymax=p90/1000,y=p50/1000),width=.5,color='white',fill='grey80',linewidth=0.3)+
  geom_crossbar(data=summary_custom_land %>% mutate(Year=as.character(Year)),
                aes(x=Year,ymin=p0/1000,ymax=p100/1000,y=p0/1000),width=.5,color='grey80',fill='transparent',fatten=0,linewidth=0.3)+
  geom_line(data = df_area_Afr_GL,aes(x = YEMF, y = IAMC_templatePlus/1000, color="本研究", linetype = "本研究", group=1),size = 1) +
  geom_point(data = df_area_Afr_GL, aes(x=YEMF,y=IAMC_templatePlus/1000,color="本研究", group=1),size = 2.5)+
  labs(tag="(b)",title = "",x="Year", y = "Area of Afforestation (billion ha/yr)")+
  theme_bw()+
  scale_color_manual(name = "Line Type",
                     values = c("AR6報告値の中央値" = "darkgrey", "本研究" = "red")) +
  scale_linetype_manual(name = "Line Type",
                        values = c("AR6報告値の中央値" = "dashed", "本研究" = "solid")) +
  theme3+
  xlab(NULL)
plot(g27)

l_fig_1de <- tribble(~y,~y2,~label,
                     -0,0,'Min',
                     1.0,1,'Max',
                     .5,.5,'Median',
                     .2,.2,'5th',
                     .8,.8,'95th') %>%
  mutate(x=0) %>%
  ggplot()+
  geom_crossbar(aes(x=x),ymin=.2,ymax=.8,y=.5,width=.15,linewidth=.15,color='white',fill='grey80')+
  geom_crossbar(aes(x=x),ymin=0,ymax=1,y=0,width=.15,linewidth=.15,color='grey80',fill='transparent',fatten=0)+
  geom_point(aes(y=y2),x=.18,shape=95,color='grey70',size=1.5)+
  geom_text(aes(y=y,label=label),x=.35,hjust=0,size=3.15)+
  annotate('text',x=0.15,y=1.2,label='AR6 C3',size=4.15) +
  xlim(-.4,2.0)+ylim(-.4,1.3)+coord_cartesian(ylim = c(-1.75, 1.25)) +
  theme_void()+theme(plot.margin=unit(c(0,0,0,0),'mm'))
plot(l_fig_1de)

g28 <-(g26 | g27 |l_fig_1de) + plot_layout(widths = c(1, 1, 0.35))
plot(g28)
ggsave(file = "../../output/Check/Reduction/Afr_AR6.png", plot=g28, width = 12.75, height = 6.00)